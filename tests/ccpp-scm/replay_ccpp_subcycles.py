#!/usr/bin/env python3
"""Replay the two 22.5-second SCM chemistry boundaries with frozen tolerances."""

from __future__ import annotations

import argparse
from datetime import datetime
import hashlib
import json
import math
import os
from pathlib import Path
import re

from compare_ccpp_native_state import compare as compare_state
from compare_ccpp_photolysis_rates import compare as compare_rates
from export_ccpp_boundary_chemistry import make_profile as chemistry_profile
from export_ccpp_met_ledger import make_profile as met_profile
from run_case import ROOT, fortran_float, run_step

NUMBER = r"[+-]?(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)(?:(?:[EeDd])?[+-][0-9]+)?"


def write_json(path: Path, value: object) -> None:
    """Write finite, inspectable evidence and create its parent directory."""
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, indent=2, allow_nan=False) + "\n")


def capture(log_path: Path) -> tuple[dict, list[dict]]:
    """Read and validate both process-input and host-update boundaries from a real run."""
    text = log_path.read_text()
    if "An error occurred" in text or "ERROR STOP" in text:
        raise ValueError("SCM log records a failure; no native reference may be accepted")
    clocks = re.findall(
        rf"CATCHEM_INTERVAL_CLOCK interval=(\d+) year=(\d+) month=(\d+) day=(\d+) hour=(\d+) minute=(\d+) second=(\d+) day_of_year=(\d+) duration_seconds=\s*({NUMBER})", text)
    if len(clocks) != 1:
        raise ValueError("expected one actual CATChem host-clock record")
    i, y, m, d, h, minute, sec, doy, duration = clocks[0]
    clock = datetime(*map(int, (y, m, d, h, minute, sec)))
    if int(i) != 1 or int(doy) != int(clock.strftime("%j")) or fortran_float(duration) != 45:
        raise ValueError("inconsistent host calendar or duration")
    invocations = [dict(process=p, interval_token=int(i), subcycle=int(s), duration_seconds=fortran_float(d))
                   for p, i, s, d in re.findall(
                       rf"CATCHEM_INVOCATION process=(\S+) interval=(\d+) subcycle=(\d+) duration_seconds=\s*({NUMBER})", text)]
    if [row["process"] for row in invocations] != ["photolysis", "gaschem"] * 2:
        raise ValueError("expected two ordered Photolysis/GasChem pairs")
    updates = [dict(interval_token=int(i), contribution_token=int(t), subcycle=int(s),
                    mapped_species=int(n), duration_seconds=fortran_float(d))
               for i, t, s, n, d in re.findall(
                   rf"CATCHEM_HOST_UPDATE interval=(\d+) contribution_token=(\d+) subcycle=(\d+) mapped_species=(\d+) duration_seconds=\s*({NUMBER})", text)]
    if len(updates) != 2 or [row["subcycle"] for row in updates] != [1, 2]:
        raise ValueError("expected exactly two host updates in loop order")
    if any(row["interval_token"] != 1 or row["duration_seconds"] != 22.5 for row in invocations + updates):
        raise ValueError("unexpected interval identity or subcycle duration")
    if any(row["mapped_species"] != 23 or row["contribution_token"] != 1 for row in updates):
        raise ValueError("unexpected mapped species count or consumed contribution identity")
    rows = [dict(interval_token=int(i), subcycle=int(s), species=p, column=int(c), level=int(k),
                 before=fortran_float(b), after=fortran_float(a), units=u)
            for i, s, p, c, k, b, a, u in re.findall(
                rf"CATCHEM_BOUNDARY_VALUE interval=(\d+) subcycle=(\d+) species=(\S+) column=(\d+) level=(\d+) before=\s*({NUMBER}) after=\s*({NUMBER}) units=([^\n]*)", text)]
    if len(rows) != 2 * 23 * 127:
        raise ValueError(f"expected 5842 constituent boundary rows, got {len(rows)}")
    for name in ("CATCHEM_UNRELATED", "CATCHEM_UNRELATED_TENDENCY"):
        seeded = re.findall(rf"{name}_SEEDED slot=(\d+) VALUE=\s*({NUMBER})", text)
        preserved = re.findall(rf"{name}_PRESERVED slot=(\d+) VALUE=\s*({NUMBER})", text)
        if len(seeded) != 1 or preserved != seeded:
            raise ValueError(f"missing exact preservation evidence for {name}")
    blocks: list[dict] = []
    current = None
    for line in text.splitlines():
        line = line.strip()
        begin = re.fullmatch(r"CATCHEM_MET_BEGIN process=(\S+)", line)
        field = re.fullmatch(r"CATCHEM_MET_FIELD name=(\S+) rank=(\d+) axis=(\d+) columns=(\d+) levels=(\d+) values=(.*)", line)
        end = re.fullmatch(rf"CATCHEM_MET_END process=(\S+) interval=(\d+) subcycle=(\d+) duration_seconds=\s*({NUMBER})", line)
        if begin:
            if current is not None:
                raise ValueError("nested process met blocks")
            current = dict(process=begin[1], fields={})
        elif field:
            if current is None:
                raise ValueError("met field outside its process block")
            values = [fortran_float(v) for v in field[6].split()]
            if len(values) != int(field[4]) * int(field[5]) or any(not math.isfinite(v) or abs(v) >= 1e20 for v in values):
                raise ValueError(f"invalid met field {field[1]}")
            current["fields"][field[1]] = dict(rank=int(field[2]), axis=int(field[3]),
                                              columns=int(field[4]), levels=int(field[5]), values=values)
        elif end:
            if current is None or current["process"] != end[1]:
                raise ValueError("unmatched process met block")
            current.update(interval_token=int(end[2]), subcycle=int(end[3]), duration_seconds=fortran_float(end[4]))
            blocks.append(current)
            current = None
    if current is not None or len(blocks) != 4:
        raise ValueError("expected four complete process met blocks")
    for block, invocation in zip(blocks, invocations):
        if any(block[key] != invocation[key] for key in invocation):
            raise ValueError("met block does not match its successful process invocation")
    diagnostics = [dict(name=n, max_abs=fortran_float(m), sum=fortran_float(s)) for n, m, s in re.findall(
        rf"CATCHEM_DIAGNOSTIC name=(\S+) MAXABS=\s*({NUMBER})\s+SUM=\s*({NUMBER})", text)]
    if len(diagnostics) != 109 or len({row["name"] for row in diagnostics}) != 109:
        raise ValueError("expected the complete 109-reaction final Photolysis rate ledger")
    return dict(schema_version=1, process="gaschem", case="arm_sgp_summer_1997_A_leap",
                status="passed_real_cap_two_subcycles", invocation_ledger=invocations,
                host_update_ledger=updates, boundary_state_ledger=rows, diagnostic_summaries=diagnostics,
                unrelated_state_and_tendency="preserved bitwise", run_log=str(log_path),
                host_clock_utc=clock.isoformat(),
                run_log_sha256=hashlib.sha256(log_path.read_bytes()).hexdigest()), blocks


def main() -> int:
    """Replay both native boundaries and fail unless state and final rate comparisons pass."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--run-log", required=True, type=Path)
    parser.add_argument("--scm-run-dir", required=True, type=Path)
    parser.add_argument("--native-runner", required=True, type=Path)
    parser.add_argument("--output-dir", required=True, type=Path)
    parser.add_argument("--docker-image", required=True)
    parser.add_argument("--start-datetime", required=True, type=datetime.fromisoformat)
    args = parser.parse_args()
    output = args.output_dir.resolve()
    output.mkdir(parents=True, exist_ok=True)
    # A failed rerun must never leave a previous successful gate summary visible.
    output.joinpath("summary.json").unlink(missing_ok=True)
    evidence, blocks = capture(args.run_log.resolve())
    if evidence["host_clock_utc"] != args.start_datetime.isoformat():
        raise ValueError(f"requested replay clock differs from captured host clock {evidence['host_clock_utc']}")
    evidence_path = output / "scm-evidence.json"
    write_json(evidence_path, evidence)
    output.joinpath("met-input-ledger.jsonl").write_text("".join(json.dumps(b) + "\n" for b in blocks))
    # These tolerances are frozen in time-recovery.yml before the native run.
    state_tolerance, rate_relative, rate_absolute = 1e-6, 1e-12, 1e-15
    reports = []
    profiles = []
    previous_rows = None
    for index in range(2):
        photo, gas = blocks[index * 2:index * 2 + 2]
        for name in set(photo["fields"]) & set(gas["fields"]):
            if photo["fields"][name] != gas["fields"][name]:
                raise ValueError(f"{name} differs within paired process calls; joint replay cannot match both")
        profile, manifest = met_profile(gas, output / "met-input-ledger.jsonl", index, args.start_datetime)
        if manifest["status"] != "complete":
            raise ValueError(f"incomplete native meteorology: {manifest}")
        profile_path = output / f"subcycle-{index+1}-met.json"
        write_json(profile_path, profile)
        write_json(profile_path.with_suffix(".manifest.json"), manifest)
        incoming = [r for r in evidence["boundary_state_ledger"] if r["subcycle"] == index + 1]
        if previous_rows is not None:
            before = {(r["species"], r["level"]): r["before"] for r in incoming}
            after = {(r["species"], r["level"]): r["after"] for r in previous_rows}
            if before != after:
                raise ValueError("interleaved host physics changed mapped chemistry; an explicit native host delta is required")
        seed = output / f"subcycle-{index+1}-initial.json"
        write_json(seed, chemistry_profile(evidence_path, 1, index + 1))
        profiles.append(profile)
        previous_rows = incoming
    sequence_path = output / "native-sequence-met.json"
    write_json(sequence_path, {**profiles[0], "replay_steps": profiles})
    final_snapshot = output / "native-sequence.json"
    run_step("native subcycles", [str(args.native_runner.resolve()), "--config",
             str(ROOT / "tests/ccpp-scm/cases/configs/gaschem.yml"), "--met-profile", str(sequence_path),
             "--initial-snapshot", str(output / "subcycle-1-initial.json"), "--snapshot", str(final_snapshot),
             "--steps", "2", "--dt", "22.5"], args.scm_run_dir.resolve(),
             output / "native-sequence.log", os.environ.copy(), args.docker_image)
    for index in range(2):
        snapshot = Path(str(final_snapshot) + f".step-{index+1}.json")
        result = compare_state(evidence_path, snapshot, 1, index + 1, state_tolerance)
        write_json(output / f"subcycle-{index+1}-state-parity.json", result)
        if result["status"] != "passed":
            raise ValueError(f"native subcycle {index+1} state comparison failed: {result}")
        reports.append(result)
    rates = compare_rates(evidence_path, final_snapshot, rate_relative, rate_absolute)
    write_json(output / "final-rate-parity.json", rates)
    summary = dict(status="passed", host_timestep_seconds=45, subcycle_seconds=22.5,
                   clock_start_utc=args.start_datetime.isoformat(), host_updates=2,
                   finite_process_met_blocks=len(blocks), invalid_met_fields=[],
                   constituent_values_compared=sum(r["compared_state_values"] for r in reports),
                   max_state_relative_error=max(r["max_relative_error"] for r in reports),
                   final_rate_comparison=rates, state_comparisons=reports,
                   native_composition="One persistent native Core/MICM instance carries its state through both captured met profiles; SCM mapped state is unchanged by the interleaved host scheme.",
                   clock_policy="Both calls use the shared SCM host interval clock and distinct 22.5-second integration durations.",
                   evidence=str(evidence_path), run_log=str(args.run_log.resolve()), docker_image=args.docker_image)
    summary["source_sha256"] = {
        name: hashlib.sha256((ROOT / name).read_bytes()).hexdigest() for name in (
            "src/core/catchem_core.cpp", "drivers/ccpp/catchem_ccpp_lifecycle.F90",
            "drivers/ccpp/catchem_photolysis.meta",
            "drivers/ccpp/catchem_gaschem.meta",
            "tests/default_parity_candidate_runner.cpp",
            "tests/ccpp-scm/stage_time_recovery_case.py",
            "tests/ccpp-scm/replay_ccpp_subcycles.py",
            "tests/ccpp-scm/cases/time-recovery.yml",
        )
    }
    summary["native_runner_sha256"] = hashlib.sha256(args.native_runner.read_bytes()).hexdigest()
    write_json(output / "summary.json", summary)
    print(json.dumps({k: summary[k] for k in ("status", "host_updates", "constituent_values_compared", "max_state_relative_error")}))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

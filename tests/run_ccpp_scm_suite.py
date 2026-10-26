#!/usr/bin/env python3
"""Build and run one locked GFS v17 CATChem suite in the external CCPP-SCM host."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
from typing import Any


ROOT = Path(__file__).resolve().parents[1]
DEFAULT_WORK = ROOT / "build/ccpp-scm-ci"
LOCK = ROOT / "tests/ccpp-scm/source-lock.json"
CASE = "gabls3_noahmp"
SUITE = "CATChem_EmissionChemistryRemoval"
PROCESSES = ["dust", "seasalt", "wetdep", "settling", "drydep", "so4chem", "carbchem"]
GOCART_INPUTS = [
    *[Path(f"scm/data/physics_input_data/aeroclim.m{month:02d}.nc") for month in range(1, 13)],
    *[Path(f"scm/data/physics_input_data/optics_{name}.dat") for name in ("DU", "SS", "SU", "BC", "OC")],
]
CASE_INPUT = Path(f"scm/data/processed_case_input/{CASE}_SCM_driver.nc")


def digest(path: Path) -> str:
    """Return a streaming SHA-256 for one SCM input file."""
    value = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            value.update(chunk)
    return value.hexdigest()


def run(command: list[str], *, log: Path | None = None, env: dict[str, str] | None = None) -> None:
    """Run a command and include its captured log tail in any failure."""
    result = subprocess.run(command, cwd=ROOT, env=env, text=True,
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT, check=False)
    if log:
        log.parent.mkdir(parents=True, exist_ok=True)
        log.write_text(result.stdout)
    if result.returncode:
        tail = result.stdout[-6000:]
        raise RuntimeError(f"command failed ({result.returncode}): {' '.join(command)}\n{tail}")


def locate_data_root(path: Path) -> Path:
    """Accept either an SCM checkout, its scm/data directory, or a data directory."""
    path = path.expanduser().resolve()
    for candidate in (path / "scm/data", path / "data", path):
        if (candidate / "processed_case_input").is_dir() or (candidate / "physics_input_data").is_dir():
            return candidate
    raise ValueError(f"no SCM processed_case_input or physics_input_data directories under {path}")


def find_scm_checkout(path: Path | None) -> Path | None:
    """Find an SCM source checkout above a supplied input-data path."""
    if path is None:
        return None
    resolved = path.expanduser().resolve()
    for candidate in (resolved, *resolved.parents):
        if (candidate / "scm/data").is_dir() and (candidate / ".git").exists():
            return candidate
    return None


def configure_offline_sources(env: dict[str, str], data_dir: Path | None,
                              lock: dict[str, Any]) -> None:
    """Require every locked source and nested submodule to be available locally."""
    scm_root = Path(env.get("CCPP_SCM_SOURCE", "")).expanduser() if env.get("CCPP_SCM_SOURCE") else None
    scm_root = scm_root or find_scm_checkout(data_dir)
    if scm_root is None:
        raise RuntimeError("offline source preparation requires CCPP_SCM_SOURCE or an SCM checkout in --data-dir")
    scm_root = scm_root.resolve()
    env["CCPP_SCM_SOURCE"] = str(scm_root)
    env.setdefault("CCPP_FRAMEWORK_SOURCE", str(scm_root / "ccpp/framework"))
    env.setdefault("CCPP_PHYSICS_SOURCE", str(scm_root / "ccpp/physics"))
    env.setdefault("CCPP_CMAKEMODULES_SOURCE", str(scm_root / "CMakeModules"))

    catchem_root = Path(env.get("CATCHEM_SOURCE", ROOT)).expanduser().resolve()
    env["CATCHEM_SOURCE"] = str(catchem_root)
    env.setdefault("GOCART_SOURCE", str(catchem_root / "src/external/GOCART"))
    env.setdefault("MUSICA_SOURCE", str(catchem_root / "src/external/musica"))

    expected = {
        "CCPP_SCM_SOURCE": "scm",
        "CCPP_FRAMEWORK_SOURCE": "framework",
        "CCPP_PHYSICS_SOURCE": "physics",
        "CATCHEM_SOURCE": "catchem",
        "GOCART_SOURCE": "gocart",
        "MUSICA_SOURCE": "musica",
    }
    for variable, component in expected.items():
        source = Path(env[variable]).expanduser().resolve()
        if not (source / ".git").exists():
            raise RuntimeError(f"offline source {variable} is not a Git checkout: {source}")
        actual = subprocess.run(["git", "-C", str(source), "rev-parse", "HEAD"],
                                text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                check=False)
        revision = lock["components"][component]["revision"]
        if actual.returncode or actual.stdout.strip() != revision:
            raise RuntimeError(f"offline source {variable} must be at locked {component} revision {revision}: {source}")

    submodule_parent = Path(env["CCPP_SCM_SOURCE"])
    submodule_specs = [(Path(env["CCPP_CMAKEMODULES_SOURCE"]), submodule_parent, "CMakeModules")]
    physics_source = Path(env["CCPP_PHYSICS_SOURCE"])
    for name in ("physics/CONV/C3", "physics/MP/TEMPO/TEMPO",
                 "physics/Radiation/RRTMGP/rte-rrtmgp", "physics/SFC_Layer/MYNN/MYNN"):
        submodule_specs.append((physics_source / name, physics_source, name))
    for source, parent, gitlink_path in submodule_specs:
        if not (source / ".git").exists():
            raise RuntimeError(f"offline source checkouts require initialized submodule: {source}")
        pointer = subprocess.run(["git", "-C", str(parent), "ls-tree", "HEAD", gitlink_path],
                                 text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                 check=False)
        actual = subprocess.run(["git", "-C", str(source), "rev-parse", "HEAD"],
                                text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                check=False)
        expected_revision = pointer.stdout.split()[2] if pointer.returncode == 0 and len(pointer.stdout.split()) >= 3 else ""
        if not expected_revision or actual.returncode or actual.stdout.strip() != expected_revision:
            raise RuntimeError(f"offline submodule {source} does not match locked gitlink {expected_revision or gitlink_path}")


def missing_inputs(data_root: Path) -> list[Path]:
    """List absent or empty case and aerosol climatology input files."""
    required = [CASE_INPUT, *GOCART_INPUTS]
    return [relative for relative in required
            if not (data_root / relative).is_file() or (data_root / relative).stat().st_size == 0]


def verify_locked_data(data_root: Path, lock: dict[str, Any]) -> list[str]:
    """Check extracted static data against the committed checksum lock."""
    failures: list[str] = []
    for relative, expected in lock["files"].items():
        path = data_root / relative
        if not path.is_file() or path.stat().st_size == 0:
            failures.append(f"missing: {relative}")
        elif digest(path) != expected:
            failures.append(f"checksum mismatch: {relative}")
    return failures


def stage_tree(source: Path, destination: Path) -> None:
    """Link workspace inputs without duplicating large climatology files."""
    if not source.is_dir():
        return
    for item in sorted(source.rglob("*")):
        relative = item.relative_to(source)
        target = destination / relative
        if item.is_dir():
            target.mkdir(parents=True, exist_ok=True)
            continue
        target.parent.mkdir(parents=True, exist_ok=True)
        if target.exists() or target.is_symlink():
            if target.is_file() and digest(item) == digest(target):
                continue
            if target.is_dir() and not target.is_symlink():
                shutil.rmtree(target)
            else:
                target.unlink()
        try:
            item.resolve().relative_to(ROOT)
            target.symlink_to(os.path.relpath(item.resolve(), target.parent))
        except ValueError:
            shutil.copy2(item, target)


def mount_data_root(tree: Path, data_root: Path) -> None:
    """Link SCM runtime data directories to a persistent, user-selected location."""
    scm_data = tree / "scm/data"
    scm_data.mkdir(parents=True, exist_ok=True)
    for name in ("processed_case_input", "physics_input_data"):
        target = data_root / name
        target.mkdir(parents=True, exist_ok=True)
        source = scm_data / name
        if source.is_symlink() and source.resolve() == target.resolve():
            continue
        if source.is_symlink():
            if source.is_dir():
                stage_tree(source, target)
            source.unlink()
        elif source.is_dir():
            stage_tree(source, target)
            shutil.rmtree(source)
        elif source.exists() or source.is_symlink():
            source.unlink()
        source.symlink_to(os.path.relpath(target, source.parent), target_is_directory=True)


def checksum_manifest(source: Path) -> dict[str, str]:
    """Hash all files staged beneath SCM's runtime data directory."""
    return {path.relative_to(source).as_posix(): digest(path)
            for path in sorted(source.rglob("*")) if path.is_file()}


def prepare_input_data(work: Path, offline_data_dir: Path | None, offline: bool,
                       evidence_dir: Path, data_lock: dict[str, Any],
                       file_dir: Path | None = None) -> dict[str, Any]:
    """Stage offline data or download the locked SCM static data and GOCART tables."""
    baseline = work / "baseline-source"
    overlay = work / "overlay-source"
    if not baseline.is_dir() or not overlay.is_dir():
        raise RuntimeError("locked SCM sources are missing; prepare must run before data staging")

    cache_root = file_dir.expanduser().resolve() if file_dir else None
    if cache_root is not None:
        cache_root.mkdir(parents=True, exist_ok=True)
    if offline_data_dir is not None:
        source_data = locate_data_root(offline_data_dir)
        if cache_root is not None:
            for name in ("processed_case_input", "physics_input_data"):
                stage_tree(source_data / name, cache_root / name)
    if cache_root is not None:
        for tree in (baseline, overlay):
            mount_data_root(tree, cache_root)
    elif offline_data_dir is not None:
        source_data = locate_data_root(offline_data_dir)
        for tree in (baseline, overlay):
            stage_tree(source_data / "processed_case_input", tree / "scm/data/processed_case_input")
            stage_tree(source_data / "physics_input_data", tree / "scm/data/physics_input_data")

    baseline_data = baseline / "scm/data"
    missing = missing_inputs(baseline)
    locked_failures = verify_locked_data(baseline, data_lock)
    missing.extend(Path(item.removeprefix("missing: ")) for item in locked_failures
                   if item.startswith("missing: "))
    if missing and offline:
        details = "\n  ".join(str(baseline / name) for name in missing)
        raise RuntimeError(f"offline SCM data is incomplete; missing or empty files:\n  {details}")

    downloads: list[dict[str, str]] = []
    if missing:
        scm_data_release = data_lock["scm_release"]
        env = os.environ.copy()
        env.update({"SCM_DATA_RELEASE": scm_data_release})
        for script in ("get_all_static_data.sh", "get_aerosol_climo.sh"):
            script_path = baseline / "contrib" / script
            content = script_path.read_text()
            pinned_url = "https://github.com/NCAR/ccpp-scm/releases/download/v7.0.0/"
            if pinned_url not in content:
                raise RuntimeError(f"expected SCM data-release URL is absent from {script_path}")
            script_path.write_text(content.replace(pinned_url,
                                                   f"https://github.com/NCAR/ccpp-scm/releases/download/{scm_data_release}/"))
            command = ["bash", str(script_path)]
            log = evidence_dir / f"download-{script}.log"
            run(command, log=log, env=env)
            downloads.append({"command": " ".join(command), "log": str(log)})

    remaining = missing_inputs(baseline)
    if remaining:
        details = "\n  ".join(str(baseline / name) for name in remaining)
        raise RuntimeError(f"SCM input-data download completed but required files are missing:\n  {details}")

    locked_failures = verify_locked_data(baseline, data_lock)
    if locked_failures:
        raise RuntimeError("SCM input data does not match the committed checksum lock:\n  " +
                           "\n  ".join(locked_failures))

    for tree in (baseline, overlay):
        stage_tree(baseline_data / "processed_case_input", tree / "scm/data/processed_case_input")
        stage_tree(baseline_data / "physics_input_data", tree / "scm/data/physics_input_data")
        manifest_path = tree / "data-input-checksums.json"
        manifest_path.write_text(json.dumps(checksum_manifest(tree / "scm/data"), indent=2) + "\n")

    return {
        "status": "passed",
        "mode": "offline" if offline else ("local-data-or-download"
                                             if offline_data_dir else
                                             ("file-cache-or-download" if cache_root else "reuse-or-download")),
        "offline_source": str(offline_data_dir.resolve()) if offline_data_dir else None,
        "file_dir": str(cache_root) if cache_root else None,
        "scm_data_release": data_lock["scm_release"],
        "download_commands": downloads,
        "required_case_input": {"path": str(baseline / CASE_INPUT), "sha256": digest(baseline / CASE_INPUT)},
        "gocart_climatology": {str(path): digest(baseline / path) for path in GOCART_INPUTS},
        "checksums": {
            "baseline": str(baseline / "data-input-checksums.json"),
            "overlay": str(overlay / "data-input-checksums.json"),
        },
    }


def command_for(action: str, work: Path, image: str, jobs: int) -> list[str]:
    """Create the common locked runner command for a single suite operation."""
    command = [sys.executable, str(ROOT / "util/ccpp_scm.py"), action,
               "--work-dir", str(work)]
    if action == "prepare":
        command.extend(["--source-lock", str(LOCK)])
    else:
        command.extend(["--cases", "coupled", "--suite", SUITE, "--jobs", str(jobs),
                        "--science-profile", "process"])
    if action != "prepare":
        command.extend(["--docker-image", image])
    return command


def validate_result(work: Path, inputs: dict[str, Any]) -> dict[str, Any]:
    """Fail unless the selected SCM suite ran all seven CATChem processes successfully."""
    case_dir = work / "cases" / f"emission-chemistry-removal-{CASE}-{SUITE}"
    evidence_path = case_dir / "evidence.json"
    evidence = json.loads(evidence_path.read_text())
    expected_calls = PROCESSES
    actual_calls = [row.get("process") for row in evidence.get("invocation_ledger", [])]
    if evidence.get("status") != "passed":
        raise RuntimeError(f"single-column suite evidence status is {evidence.get('status')!r}: {evidence_path}")
    if evidence.get("scientific_acceptance", {}).get("status") != "passed":
        raise RuntimeError("single-column suite scientific acceptance did not pass")
    if actual_calls != expected_calls:
        raise RuntimeError(f"suite process order mismatch: expected {expected_calls}, got {actual_calls}")
    outputs = [Path(path) for path in evidence.get("output_files", [])]
    if not outputs or any(not path.is_file() or path.stat().st_size == 0 for path in outputs):
        raise RuntimeError(f"SCM suite did not produce nonempty output files: {outputs}")
    run_log = Path(evidence["logs"]["run"])
    log_text = run_log.read_text(errors="replace")
    if "completed successfully" not in log_text.lower():
        raise RuntimeError(f"SCM run log lacks its successful completion marker: {run_log}")
    return {
        "schema_version": 1,
        "status": "passed",
        "host": "CCPP-SCM",
        "physics_suite": "SCM_GFS_v17_p8_ugwpv1",
        "catchem_suite": SUITE,
        "case": CASE,
        "processes": actual_calls,
        "input_data": inputs,
        "scm_evidence": str(evidence_path),
        "run_log": str(run_log),
        "outputs": [str(path) for path in outputs],
    }


def main() -> int:
    """Prepare locked sources and inputs, capgen one suite, run it and write evidence."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--work-dir", type=Path, default=DEFAULT_WORK)
    parser.add_argument("--offline", action="store_true",
                        help="run without network access using locked local source checkouts and data")
    parser.add_argument("--data-dir", type=Path,
                        help="reuse SCM data from a checkout, scm/data directory, or data directory")
    parser.add_argument("--file_dir", "--file-dir", dest="file_dir", type=Path,
                        help="persistent output/cache directory containing SCM data subdirectories")
    parser.add_argument("--data-only", action="store_true",
                        help="prepare and validate inputs, then exit without building or running SCM")
    parser.add_argument("--jobs", type=int, default=max(1, min(4, os.cpu_count() or 1)))
    parser.add_argument("--docker-image", default="",
                        help="optional toolchain image; omit when running inside the prepared container")
    args = parser.parse_args()
    work = args.work_dir.expanduser().resolve()
    if work == ROOT or ROOT not in work.parents:
        parser.error("--work-dir must be a generated directory beneath the repository root")
    if args.offline and args.data_dir is None and args.file_dir is None:
        parser.error("--offline requires --data-dir or --file_dir")
    evidence_dir = work / "evidence"
    evidence_dir.mkdir(parents=True, exist_ok=True)

    env = os.environ.copy()
    lock = json.loads(LOCK.read_text())
    data_lock_path = ROOT / lock["data"]["checksums_file"]
    if digest(data_lock_path) != lock["data"]["checksums_file_sha256"]:
        raise RuntimeError(f"SCM data checksum-lock file hash mismatch: {data_lock_path}")
    data_lock = json.loads(data_lock_path.read_text())
    if data_lock.get("scm_release") != lock["data"]["scm_release"]:
        raise RuntimeError("SCM data release in source-lock and data-lock files differ")
    if args.offline:
        configure_offline_sources(env, args.data_dir or args.file_dir, lock)
    else:
        for component in ("catchem", "gocart", "musica"):
            env.setdefault({"catchem": "CATCHEM_SOURCE", "gocart": "GOCART_SOURCE",
                            "musica": "MUSICA_SOURCE"}[component],
                           lock["components"][component]["repository"])
    run(command_for("prepare", work, args.docker_image, args.jobs), env=env,
        log=evidence_dir / "prepare.log")
    inputs = prepare_input_data(work, args.data_dir, args.offline, evidence_dir, data_lock,
                                file_dir=args.file_dir)
    inputs["data_lock"] = {"path": str(data_lock_path),
                            "sha256": lock["data"]["checksums_file_sha256"]}
    if args.data_only:
        inputs["status"] = "passed"
        inputs["data_only"] = True
        inputs["reusable_data_dir"] = str(args.file_dir.expanduser().resolve()
                                           if args.file_dir else work / "baseline-source/scm/data")
        result_path = evidence_dir / "data-preparation.json"
        result_path.write_text(json.dumps(inputs, indent=2) + "\n")
        print(json.dumps(inputs, indent=2))
        return 0
    build_command = command_for("build", work, args.docker_image, args.jobs)
    run(build_command, env=env, log=evidence_dir / "build.log")
    run_command = command_for("run", work, args.docker_image, args.jobs)
    run_command.extend(["--emit-boundary-ledger", "--emit-met-ledger"])
    run(run_command, env=env, log=evidence_dir / "run.log")
    result = validate_result(work, inputs)
    result_path = evidence_dir / "single-suite.json"
    result_path.write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps(result, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

#!/usr/bin/env python3
"""Fail-closed verification of required CCPP-SCM case evidence."""

from __future__ import annotations

import argparse
import json
import math
import hashlib
from pathlib import Path
import re
import yaml

ROOT = Path(__file__).resolve().parents[1]
PROCESSES = ("dust", "seasalt", "drydep", "wetdep", "settling", "so4chem", "carbchem",
             "photolysis", "gaschem")
COUPLED = ("emission-chemistry-removal", "photolysis-gaschem", "time-recovery")
SYSTEM_BUDGETS = {
    "emission-chemistry-removal": ("aerosol_mass_by_species", "sulfur_atoms"),
    "photolysis-gaschem": ("odd_oxygen",),
}
COUPLED_AEROSOL_SPECIES = {"so4", "msa", "bc1", "bc2", "oc1", "oc2",
                           *{f"dust{i}" for i in range(1, 6)}, *{f"seas{i}" for i in range(1, 6)}}


def validate_system_budget_records(mass_budget: object, required: tuple[str, ...],
                                   frozen_tolerances: dict[str, float] | None = None,
                                   expected_aerosols: set[str] | None = None) -> list[str]:
    """Require measured, unit-labelled system budgets with explicit numeric limits."""
    if not isinstance(mass_budget, dict):
        return ["mass_budget must be an object with measured system budget records"]
    records = mass_budget.get("system_budgets", [])
    if not isinstance(records, list):
        return ["system_budgets must be an array of measured budget records"]
    by_name = {item.get("name"): item for item in records if isinstance(item, dict)}
    failures = []
    frozen_tolerances = frozen_tolerances or {}
    for name in required:
        if name not in frozen_tolerances:
            failures.append(f"manifest has no frozen numeric tolerance for system budget: {name}")
            expected_tolerance = None
        else:
            expected_tolerance = frozen_tolerances[name]
        if sum(isinstance(item, dict) and item.get("name") == name for item in records) > 1:
            failures.append(f"duplicate system budget record: {name}")
            continue
        record = by_name.get(name)
        if record is None:
            failures.append(f"missing system budget record: {name}")
            continue
        if not record.get("units"):
            failures.append(f"system budget has no units: {name}")
        if name == "aerosol_mass_by_species":
            species_records = record.get("species_budgets", [])
            if not isinstance(species_records, list) or not species_records:
                failures.append("aerosol mass budget has no per-species closures")
                continue
            species_names = set()
            for species_record in species_records:
                if not isinstance(species_record, dict):
                    failures.append("aerosol mass budget contains a malformed species closure")
                    continue
                species_name = species_record.get("species")
                if not species_name or species_name in species_names:
                    failures.append("aerosol mass budget has a missing or duplicate species closure")
                    continue
                species_names.add(species_name)
                failures.extend(_budget_record_failures(
                    f"{name}/{species_name}", species_record.get("residual"),
                    species_record.get("tolerance"), species_record.get("units"), expected_tolerance))
                if species_record.get("status") != "passed":
                    failures.append(f"system budget is not passed: {name}/{species_name}")
            if expected_aerosols is not None and species_names != expected_aerosols:
                failures.append("aerosol mass budget does not cover the complete required species inventory")
        else:
            failures.extend(_budget_record_failures(
                name, record.get("residual"), record.get("tolerance"), record.get("units"),
                expected_tolerance))
        if record.get("status") != "passed":
            failures.append(f"system budget is not passed: {name}")
    return failures


def manifest_system_budget_tolerances(text: str) -> dict[str, float]:
    """Read numeric residual limits from the manifest's budget declarations."""
    section = re.search(r"(?ms)^budgets:\s*\n(.*?)(?=^[^ \t#][^\n]*:|\Z)", text)
    if not section:
        return {}
    blocks = re.findall(r"(?ms)^  - quantity:\s*([^\s#]+)\s*\n(.*?)(?=^  - quantity:|\Z)", section.group(1))
    limits = {}
    for name, block in blocks:
        match = re.search(r"(?m)^    residual_tolerance:\s*([0-9.eE+-]+)\s*$", block)
        if match:
            try:
                value = float(match.group(1))
                if math.isfinite(value) and value >= 0.0:
                    limits[name] = value
            except ValueError:
                pass
    return limits


def _budget_record_failures(name: str, residual: object, tolerance: object, units: object,
                            expected_tolerance: float | None = None) -> list[str]:
    """Validate one residual/tolerance pair without accepting NaN or infinity."""
    failures = []
    if not units:
        failures.append(f"system budget has no units: {name}")
    try:
        residual_value = float(residual)
        tolerance_value = float(tolerance)
        if not math.isfinite(residual_value) or residual_value < 0.0:
            raise ValueError
        if not math.isfinite(tolerance_value) or tolerance_value < 0.0:
            raise ValueError
        if expected_tolerance is not None and tolerance_value != expected_tolerance:
            failures.append(f"system budget tolerance differs from frozen manifest: {name}")
        if residual_value > tolerance_value:
            failures.append(f"system budget residual exceeds tolerance: {name}")
    except (TypeError, ValueError, OverflowError):
        failures.append(f"system budget residual or tolerance is missing/invalid: {name}")
    return failures


def check_native_reference_evidence(paths: list[str], maximum_absolute: float,
                                    maximum_relative: float, root: Path = ROOT) -> list[str]:
    """Return parity reports that are absent, failed, or exceed the frozen limits."""
    invalid = []
    for relative_path in paths:
        parity_path = root / relative_path
        try:
            parity = json.loads(parity_path.read_text())
            absolute_error = float(parity["max_absolute_error"])
            relative_error = float(parity["max_relative_error"])
            if (parity.get("status") != "passed" or
                    not math.isfinite(absolute_error) or not math.isfinite(relative_error) or
                    min(absolute_error, relative_error) < 0.0 or
                    (absolute_error > maximum_absolute and relative_error > maximum_relative)):
                invalid.append(relative_path)
                continue
            if parity.get('source_evidence'):
                source = Path(parity['source_evidence'])
                snapshot = Path(parity['native_snapshot'])
                if (hashlib.sha256(source.read_bytes()).hexdigest() != parity['source_evidence_sha256'] or
                    hashlib.sha256(snapshot.read_bytes()).hexdigest() != parity['sha256']['native_snapshot']):
                    invalid.append(relative_path)
        except (OSError, json.JSONDecodeError, KeyError, TypeError, ValueError):
            invalid.append(relative_path)
    return invalid


def check_time_recovery_evidence(summary_path: Path) -> str | None:
    """Validate the frozen subcycle replay and its real-cap clock evidence."""
    try:
        summary = json.loads(summary_path.read_text())
        if summary.get("status") != "passed":
            return "time-recovery replay did not pass"
        if summary.get("host_timestep_seconds") != 45 or summary.get("subcycle_seconds") != 22.5:
            return "time-recovery timestep does not match the frozen manifest"
        if summary.get("host_updates") != 2 or summary.get("finite_process_met_blocks") != 4:
            return "time-recovery host updates or finite meteorology coverage is incomplete"
        states = summary.get("state_comparisons", [])
        if len(states) != 2 or any(item.get("status") != "passed" for item in states):
            return "time-recovery lacks two passing native state comparisons"
        if any(item.get("max_relative_error", float("inf")) > 1.0e-6 for item in states):
            return "time-recovery state comparison exceeds the frozen relative tolerance"
        rates = summary.get("final_rate_comparison", {})
        if (rates.get("status") != "passed" or
                rates.get("relative_tolerance") != 1.0e-12 or
                rates.get("absolute_tolerance_s-1") != 1.0e-15):
            return "time-recovery final photolysis rates exceed frozen tolerances"
        log_path = Path(summary.get("run_log", ""))
        if not log_path.is_file() or log_path.stat().st_size == 0:
            return "time-recovery real-cap run log is absent or empty"
        return None
    except (OSError, json.JSONDecodeError, TypeError, ValueError):
        return "time-recovery summary is absent or invalid"


def audit_emission_budget_inputs(evidence_path: Path) -> dict:
    """Report available process closures and whether they establish system budgets."""
    expected = {"dust", "seasalt", "wetdep", "settling", "drydep", "so4chem", "carbchem"}
    failures = []
    closed_process_budgets: list[str] = []
    failed_process_budgets: list[str] = []
    settling_boundary_closure = False
    try:
        evidence = json.loads(evidence_path.read_text())
        state_data = evidence["boundary_state_ledger"]
        met_path = Path(evidence["met_input_ledger"]["path"])
        state_rows = state_data if isinstance(state_data, list) else state_data.get("rows", [])
        # Some versions store state rows inline; otherwise read the referenced JSONL.
        if not state_rows:
            state_path = Path(state_data["path"])
            state_rows = [json.loads(line) for line in state_path.read_text().splitlines() if line.strip()]
        met_rows = [json.loads(line) for line in met_path.read_text().splitlines() if line.strip()]
        processes = {item.get("process", "") for item in evidence.get("invocation_ledger", [])}
        blank_units = sum(not row.get("units") for row in state_rows)
        met_fields = {name for row in met_rows for name in row.get("fields", {})}
        budgets = evidence.get("mass_budget", {}).get("reported_process_budgets", [])
        closed_process_budgets = []
        failed_process_budgets = []
        for budget in budgets:
            residual = budget.get("residual")
            if residual is None:
                failed_process_budgets.append(budget.get("name", "unnamed"))
                continue
            name = budget.get("name", "")
            if name == "settling_column_mass":
                mass = budget.get("mass_loss")
                flux = budget.get("boundary_flux_mass")
                relative_residual = (abs(residual) / max(abs(mass) + abs(flux), 1.0e-30)
                                     if mass is not None and flux is not None else float("inf"))
            else:
                total = budget.get("total")
                bins = budget.get("sum_of_bins")
                relative_residual = (abs(residual) if name in {"drydep_aerosol_removal",
                                                                "so4chem_production",
                                                                "carbchem_carbon_mass"} else
                                     abs(residual) / max(abs(total) + abs(bins), 1.0e-30)
                                     if total is not None and bins is not None else float("inf"))
            budget["relative_residual_audited"] = relative_residual
            if relative_residual <= 1.0e-10:
                closed_process_budgets.append(name + (f"/{budget['species']}" if budget.get("species") else ""))
            else:
                failed_process_budgets.append(name + (f"/{budget['species']}" if budget.get("species") else ""))
        settling_boundary_closure = any(name == "settling_column_mass" for name in closed_process_budgets)
        if evidence.get("status") not in {"passed", "blocked"}:
            failures.append("coupled run evidence is not a completed run")
        if not expected.issubset(processes):
            failures.append("invocation ledger does not cover all seven process calls")
        if not state_rows or blank_units:
            failures.append("boundary state ledger is empty or omits constituent units")
        if not met_rows or not met_fields:
            failures.append("meteorology input ledger is missing or empty")
        if failed_process_budgets:
            failures.append("one or more reported process-local budget closures exceed 1e-10 relative error")
    except (OSError, KeyError, json.JSONDecodeError, TypeError):
        evidence = {}
        state_rows = []
        processes = set()
        blank_units = 0
        met_fields = set()
        closed_process_budgets = []
        settling_boundary_closure = False
        failures.append("coupled evidence or referenced ledgers are missing or malformed")
    mass_budget = evidence.get("mass_budget", {})
    manifest_path = ROOT / "tests/ccpp-scm/cases/emission-chemistry-removal.yml"
    expected_tolerances = manifest_system_budget_tolerances(manifest_path.read_text()) if manifest_path.is_file() else {}
    system_budget_failures = validate_system_budget_records(
        mass_budget, SYSTEM_BUDGETS["emission-chemistry-removal"], expected_tolerances,
        COUPLED_AEROSOL_SPECIES)
    complete_budget = mass_budget.get("status") == "passed" and not system_budget_failures
    if not complete_budget:
        failures.append("system-level per-species aerosol and sulfur-atom budgets remain unmeasured")
        failures.extend(system_budget_failures)
    return {"status": "available" if complete_budget and not failures else "insufficient",
            "evidence": str(evidence_path), "state_rows": len(state_rows),
            "processes": sorted(processes), "blank_unit_rows": blank_units,
            "meteorology_fields": sorted(met_fields),
            "process_budget_closures": closed_process_budgets,
            "failed_process_budget_closures": failed_process_budgets,
            "process_budget_tolerance_relative": 1.0e-10,
            "settling_boundary_closure": settling_boundary_closure,
            "system_budget_status": mass_budget.get("status", "missing"),
            "system_budget_failures": system_budget_failures,
            "failures": failures}


def verify(work_dir: Path, group: str) -> dict:
    """Check frozen requirements against nonempty passing process evidence."""
    required = PROCESSES if group == "individual" else COUPLED
    rows = []
    failures = []
    case_dir = ROOT / "tests/ccpp-scm/cases"
    for case in required:
        manifest = case_dir / f"{case}.yml"
        if not manifest.is_file():
            failures.append(f"missing required case manifest: {case}")
            rows.append({"case": case, "status": "failed", "reason": "missing manifest"})
            continue
        text = manifest.read_text()
        budget_audit = None
        case_budget_failures = []
        if case == "emission-chemistry-removal":
            candidate = work_dir / "cases" / "emission-chemistry-removal-gabls3_noahmp-CATChem_EmissionChemistryRemoval" / "evidence.json"
            if not candidate.is_file():
                candidate = work_dir / "evidence" / "emission-chemistry-removal.json"
            budget_audit = audit_emission_budget_inputs(candidate)
            case_budget_failures.extend(budget_audit["failures"])
        elif case == "photolysis-gaschem":
            candidates = sorted((work_dir / "cases").glob("photolysis-gaschem-*/evidence.json"))
            budget_audit = []
            if not candidates:
                case_budget_failures.append("no day/night evidence is available for the odd-oxygen budget")
            expected_tolerances = manifest_system_budget_tolerances(text)
            for candidate in candidates:
                try:
                    candidate_evidence = json.loads(candidate.read_text())
                    mass_budget = candidate_evidence.get("mass_budget", {})
                    budget_failures = validate_system_budget_records(
                        mass_budget, SYSTEM_BUDGETS[case], expected_tolerances)
                    if mass_budget.get("status") != "passed":
                        budget_failures.append("system mass_budget status is not passed")
                    budget_audit.append({
                        "evidence": str(candidate),
                        "mass_budget_status": mass_budget.get("status", "missing"),
                        "failures": budget_failures,
                    })
                    case_budget_failures.extend(budget_failures)
                except (OSError, json.JSONDecodeError, AttributeError, TypeError):
                    detail = f"system budget evidence is malformed: {candidate}"
                    budget_audit.append({"evidence": str(candidate), "failures": [detail]})
                    case_budget_failures.append(detail)
        if case == "time-recovery":
            frozen = bool(re.search(r"(?m)^  frozen_before_candidate_comparison:\s*true\s*$", text))
            evidence_match = re.search(r"(?m)^  evidence:\s*(\S+)\s*$", text)
            evidence_path = evidence_match.group(1) if evidence_match else ""
            failure = None if frozen and evidence_path else "time-recovery reference tolerances or evidence path are not frozen"
            if failure is None:
                failure = check_time_recovery_evidence(ROOT / evidence_path)
            if failure:
                failures.append(f"invalid time-recovery evidence: {failure}")
                rows.append({"case": case, "status": "failed", "reason": failure})
            else:
                rows.append({"case": case, "status": "passed", "reference_evidence": [evidence_path]})
            continue
        if not re.search(r"(?m)^frozen_before_candidate_comparison:\s*true\s*$", text):
            failures.append(f"acceptance tolerances are not frozen: {case}")
            rows.append({"case": case, "status": "failed", "reason": "unfrozen tolerances"})
            continue
        if re.search(r"(?m)^\s*method:\s*pending", text):
            failures.append(f"reference method is pending: {case}")
            rows.append({"case": case, "status": "failed", "reason": "pending reference"})
            continue
        method_match = re.search(r"(?m)^  method:\s*([^\s#]+)\s*$", text)
        reference_method = method_match.group(1) if method_match else ""
        reference_evidence = []
        if reference_method in {"native_parity", "native_same_boundary_process_replay"}:
            section = re.search(r"(?ms)^reference:\s*\n(.*?)(?=^[^ \t#][^\n]*:|\Z)", text)
            evidence_match = (re.search(r"(?m)^  evidence:\s*\n((?:^    -\s*[^\n]+\n?)+)", section.group(1))
                              if section else None)
            reference_evidence = [line.strip()[2:].strip().strip("'\"")
                                  for line in evidence_match.group(1).splitlines()] if evidence_match else []
            if not reference_evidence:
                failures.append(f"native parity evidence is missing: {case}")
                rows.append({"case": case, "status": "failed", "reason": "missing native parity evidence"})
                continue
            tolerances = re.findall(
                r"(?ms)^  - quantity: [^\n]+\n((?:^    [^\n]*\n?)*)", text)
            rel_limits = [float(value) for block in tolerances
                          for value in re.findall(r"(?m)^    relative:\s*([0-9.eE+-]+)\s*$", block)]
            abs_limits = [float(value) for block in tolerances
                          for value in re.findall(r"(?m)^    absolute:\s*([0-9.eE+-]+)\s*$", block)]
            maximum_relative = max(rel_limits, default=0.0)
            maximum_absolute = max(abs_limits, default=0.0)
            invalid_reference = check_native_reference_evidence(
                reference_evidence, maximum_absolute, maximum_relative)
            if reference_method == "native_same_boundary_process_replay":
                reference = yaml.safe_load(text)['reference']
                limits = reference['tolerances']
                invalid_reference = []
                observed = set()
                for path in reference_evidence:
                    try:
                        report = json.loads((ROOT / path).read_text())
                        is_rate = report['process'] == 'photolysis'
                        name = 'photolysis_rate_profile_metrics' if is_rate else 'mapped_species_after_host_update'
                        bounds = limits[name]
                        abs_bound = bounds['absolute_s-1'] if is_rate else bounds['absolute']
                        invalid_reference.extend(check_native_reference_evidence(
                            [path], float(abs_bound), float(bounds['relative'])))
                        if report.get('compared_metrics' if is_rate else 'compared_state_values', 0) <= 0:
                            invalid_reference.append(path)
                        observed.add((report['case'], 'rates' if is_rate else 'state'))
                    except (OSError, json.JSONDecodeError, KeyError, TypeError, ValueError):
                        invalid_reference.append(path)
                expected = {(case_name, kind) for case_name in
                            ('arm_sgp_summer_1997_A', 'arm_sgp_summer_1997_A_night') for kind in ('rates', 'state')}
                if observed != expected:
                    invalid_reference.append('both SCM day/night cases require state and rate native references')
            if invalid_reference:
                failures.append(f"native parity evidence is missing, failed, or outside frozen limits: {case}")
                rows.append({"case": case, "status": "failed", "reason": "invalid native parity evidence",
                             "reference_evidence": reference_evidence,
                             "invalid_reference_evidence": invalid_reference})
                continue
        if re.search(r"(?m)^\s*residual_tolerance:\s*(to_be_measured|pending|TBD)\s*$", text, re.IGNORECASE):
            failures.append(f"scientific budget tolerance is not frozen: {case}")
            row = {"case": case, "status": "failed", "reason": "unmeasured budget tolerance"}
            if case == "emission-chemistry-removal":
                row["budget_input_audit"] = budget_audit
            elif case == "photolysis-gaschem":
                row["budget_input_audit"] = budget_audit
            rows.append(row)
            continue
        case_match = re.search(r"(?m)^scm_case:\s*([^\s#]+)\s*$", text)
        cases_match = re.search(r"(?m)^\s*scm_cases:\s*\[([^\]]+)\]", text)
        expected_scm_cases = ({case_match.group(1)} if case_match else
                              {item.strip() for item in cases_match.group(1).split(",")} if cases_match else set())
        if not expected_scm_cases:
            failures.append(f"SCM case provenance is missing: {case}")
            rows.append({"case": case, "status": "failed", "reason": "missing SCM case"})
            continue
        candidates = sorted((work_dir / "cases").glob(f"{case}-*/evidence.json"))
        if not candidates and group == "coupled":
            index = work_dir / "evidence" / f"{case}.json"
            candidates = [index] if index.is_file() else []
        passing = []
        passing_scm_cases = set()
        wrong_case = []
        for path in candidates:
            try:
                evidence = json.loads(path.read_text())
            except (OSError, json.JSONDecodeError):
                continue
            if evidence.get('process') != case:
                wrong_case.append(str(path))
                continue
            if evidence.get("case") not in expected_scm_cases:
                wrong_case.append(str(path))
                continue
            outputs = evidence.get("output_files", [])
            if evidence.get("status") != "passed" or not outputs:
                continue
            if any(not Path(item).is_file() or Path(item).stat().st_size == 0 for item in outputs):
                continue
            acceptance = evidence.get("scientific_acceptance")
            # Required science must be explicitly evaluated. Missing acceptance
            # metadata is a skipped gate, even when the host produced a file.
            if not isinstance(acceptance, dict) or acceptance.get("status") != "passed":
                continue
            passing.append(str(path))
            passing_scm_cases.add(evidence['case'])
        missing_scm_cases = expected_scm_cases - passing_scm_cases
        if missing_scm_cases or case_budget_failures:
            if missing_scm_cases:
                failures.append(f"no passing scientific output evidence for {case}")
            if case_budget_failures:
                failures.extend(f"invalid system budget evidence for {case}: {item}"
                                for item in case_budget_failures)
            reason = ("no passing nonempty evidence for the manifest SCM case" if missing_scm_cases else
                      "system budget evidence is incomplete or violates the frozen limit")
            rows.append({"case": case, "status": "failed", "reason": reason,
                         "expected_scm_cases": sorted(expected_scm_cases),
                         "missing_scm_cases": sorted(missing_scm_cases),
                         "wrong_case_evidence": wrong_case,
                         **({"budget_input_audit": budget_audit} if budget_audit is not None else {})})
        else:
            rows.append({"case": case, "status": "passed", "expected_scm_cases": sorted(expected_scm_cases),
                         "evidence": passing,
                         **({"reference_evidence": reference_evidence} if reference_evidence else {})})
    return {"schema_version": 1, "case_group": group,
            "status": "failed" if failures else "passed", "required_cases": rows,
            "failures": failures,
            "policy": "Required coverage, scientific outputs, reference and frozen limits are mandatory."}


def main() -> int:
    """Write machine-readable verification and return nonzero on any missing gate."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--work-dir", required=True, type=Path)
    parser.add_argument("--cases", required=True, choices=("individual", "coupled"))
    args = parser.parse_args()
    report = verify(args.work_dir.resolve(), args.cases)
    path = args.work_dir.resolve() / "evidence" / f"{args.cases}-verification.json"
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(report, indent=2))
    return 0 if report["status"] == "passed" else 1


if __name__ == "__main__":
    raise SystemExit(main())

"""Regression checks for the fail-closed CCPP-SCM acceptance command."""

from pathlib import Path
import argparse
import json
import importlib.util
import sys
import tempfile
import unittest
from unittest.mock import patch

from verify_ccpp_science import (check_native_reference_evidence,
                                audit_emission_budget_inputs,
                                manifest_system_budget_tolerances,
                                validate_system_budget_records,
                                check_time_recovery_evidence, verify)

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "util"))
import ccpp_scm

RUN_CASE_SPEC = importlib.util.spec_from_file_location(
    "ccpp_scm_run_case", ROOT / "tests/ccpp-scm/run_case.py")
run_case = importlib.util.module_from_spec(RUN_CASE_SPEC)
RUN_CASE_SPEC.loader.exec_module(run_case)
NIGHT_CASE_SPEC = importlib.util.spec_from_file_location(
    "ccpp_scm_night_case", ROOT / "tests/ccpp-scm/stage_arm_night_case.py")
night_case = importlib.util.module_from_spec(NIGHT_CASE_SPEC)
NIGHT_CASE_SPEC.loader.exec_module(night_case)
SUITE_RUNNER_SPEC = importlib.util.spec_from_file_location(
    "ccpp_single_suite_runner", ROOT / "tests/run_ccpp_scm_suite.py")
suite_runner = importlib.util.module_from_spec(SUITE_RUNNER_SPEC)
SUITE_RUNNER_SPEC.loader.exec_module(suite_runner)


class TestCCPPSCMAcceptance(unittest.TestCase):
    """Ensure absent, unfrozen or empty required results cannot pass verification."""

    def test_individual_default_runner_selects_manifest_cases(self):
        arguments = argparse.Namespace(cases='individual', suite=None, case=None,
                                       docker_image='locked-test-image', science_profile='all',
                                       work_dir='build/suite-test')
        with patch.object(ccpp_scm, '_run_case_command') as invoke:
            ccpp_scm.run_cases(arguments)
        selected = {call.args[1]: call.args[2] for call in invoke.call_args_list}
        self.assertEqual(selected, {'dust': 'arm_sgp_summer_1997_A', 'seasalt': 'COMBLE',
            'drydep': 'arm_sgp_summer_1997_A', 'wetdep': 'twpice', 'settling': 'bomex',
            'so4chem': 'arm_sgp_summer_1997_A', 'carbchem': 'bomex',
            'photolysis': 'arm_sgp_summer_1997_A', 'gaschem': 'arm_sgp_summer_1997_A'})
        self.assertTrue(all(len(call.kwargs['suite_set']) == 24 for call in invoke.call_args_list))
        self.assertEqual(len({call.kwargs['shared_build_dir'] for call in invoke.call_args_list}), 1)
        self.assertTrue(all(len(call.kwargs['process_set']) == 9 for call in invoke.call_args_list))

    def test_explicit_coupled_suite_builds_only_its_suite_family_and_processes(self):
        arguments = argparse.Namespace(
            cases="coupled", suite="CATChem_EmissionChemistryRemoval", case=None,
            processes="dust,seasalt,drydep,wetdep,settling,so4chem,carbchem,photolysis,gaschem",
            work_dir="build/ccpp-scm-ci", jobs=2, docker_image="",
            science_profile="process", registry_order="manifest",
            emit_boundary_ledger=True, emit_met_ledger=True)
        with patch.object(ccpp_scm, "_run_case_command") as invoke:
            ccpp_scm.build(arguments)
        self.assertEqual(len(invoke.call_args_list), 1)
        call = invoke.call_args_list[0]
        self.assertEqual(call.kwargs["suite_set"], [
            "CATChem_EmissionChemistryRemoval", "CATChem_EmissionChemistryRemoval_ps"])
        self.assertEqual(call.kwargs["process_set"], [
            "dust", "seasalt", "wetdep", "settling", "drydep", "so4chem", "carbchem"])
        self.assertEqual(call.kwargs["science_profile"], "process")

    def test_suite_runner_data_lock_detects_missing_and_modified_inputs(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            (root / "scm/data/processed_case_input").mkdir(parents=True)
            (root / "scm/data/processed_case_input/case.nc").write_bytes(b"case")
            expected = suite_runner.digest(root / "scm/data/processed_case_input/case.nc")
            lock = {"files": {"scm/data/processed_case_input/case.nc": expected,
                              "scm/data/physics_input_data/table.dat": "0" * 64}}
            failures = suite_runner.verify_locked_data(root, lock)
            self.assertEqual(failures, ["missing: scm/data/physics_input_data/table.dat"])
            table = root / "scm/data/physics_input_data/table.dat"
            table.parent.mkdir(parents=True)
            table.write_bytes(b"different")
            self.assertEqual(suite_runner.verify_locked_data(root, lock),
                             ["checksum mismatch: scm/data/physics_input_data/table.dat"])

    def test_offline_suite_runner_requires_local_source_checkouts(self):
        lock = json.loads((ROOT / "tests/ccpp-scm/source-lock.json").read_text())
        with self.assertRaisesRegex(RuntimeError, "offline source preparation requires"):
            suite_runner.configure_offline_sources({}, None, lock)

    def test_file_dir_persists_and_mounts_scm_data_directories(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            scm_tree = root / "scm-work"
            data_root = root / "persistent-inputs"
            source = scm_tree / "scm/data/physics_input_data/table.dat"
            source.parent.mkdir(parents=True)
            source.write_bytes(b"locked table")
            suite_runner.mount_data_root(scm_tree, data_root)
            mounted = scm_tree / "scm/data/physics_input_data"
            self.assertTrue(mounted.is_symlink())
            self.assertEqual((data_root / "physics_input_data/table.dat").read_bytes(), b"locked table")
            self.assertEqual((mounted / "table.dat").read_bytes(), b"locked table")

    def test_mechanism_budget_uses_manifest_species_weights_and_exchange(self):
        trace = '\n'.join((
            'CATCHEM_GASCHEM_STATE column=1 level=1 species=O initial=1 final=1.5 units=mol_m-3',
            'CATCHEM_GASCHEM_STATE column=1 level=1 species=O1D initial=1 final=1.5 units=mol_m-3',
            'CATCHEM_GASCHEM_STATE column=1 level=1 species=O2 initial=4 final=3.5 units=mol_m-3',
            'CATCHEM_GASCHEM_STATE column=1 level=1 species=O3 initial=0 final=0 units=mol_m-3'))
        met = [{"process": "gaschem", "fields": {"Z": {
            "columns": 1, "levels": 2, "values": [0.0, 30.0]}}}]
        budget_config = {
            'quantity': 'odd_oxygen', 'units': 'mol m-2',
            'state_weights': {'O': 1.0, 'O1D': 1.0, 'O3': 1.0},
            'exchange_weights': {'O2': 1.0, 'O3': 1.0}, 'exchange_factor': 2.0,
        }
        budget = run_case.mechanism_species_exchange_budget(trace, met, budget_config, 1.0e-6)
        self.assertEqual(budget['status'], 'passed')
        self.assertEqual(budget['initial'], 60.0)
        self.assertEqual(budget['production'], 30.0)
        self.assertEqual(budget['final'], 90.0)
        corrupt = trace.replace('species=O3 initial=0 final=0', 'species=O3 initial=0 final=0.2')
        self.assertEqual(run_case.mechanism_species_exchange_budget(
            corrupt, met, budget_config, 1.0e-6)['status'], 'failed')
        with self.assertRaises(RuntimeError):
            run_case.mechanism_species_exchange_budget('', met, budget_config, 1.0e-6)

    def test_aerosol_budget_rejects_missing_flux_and_changed_inventory(self):
        aerosols = ('so4', 'msa', 'bc1', 'bc2', 'oc1', 'oc2',
                    *[f'dust{i}' for i in range(1, 6)], *[f'seas{i}' for i in range(1, 6)])
        processes = run_case.PROCESS_GROUPS['emission-chemistry-removal']
        rows = [{'species': species, 'column': 1, 'level': 1, 'before': 1.0, 'after': 1.0, 'units': 'kg kg-1'}
                for process in processes for species in (*aerosols, 'so2', 'dms')]
        met = [{'process': process, 'fields': {'DELP': {'columns': 1, 'values': [9.80665]}}}
               for process in processes]
        terms = [('settling', species) for species in aerosols]
        terms += [('so4chem', species) for species in ('so4', 'msa')]
        terms += [('carbchem', species) for species in ('bc1', 'bc2', 'oc1', 'oc2')]
        terms += [('drydep', 'so2')]
        trace = '\n'.join(f'CATCHEM_COLUMN_TERM process={process} species={species} source=0 sink=0 units=kg_m-2'
                          for process, species in terms)
        diag = [{'name': name, 'sum': 0.0} for name in
                ('dust_emission_total', 'seasalt_mass_emission_total', 'DMS_emission_flux')]
        budgets = run_case.emission_system_budgets(trace, rows, met, diag, 1.0e-6)
        self.assertTrue(all(row['status'] == 'passed' for row in budgets))
        rows[-18]['after'] = 1.1
        budgets = run_case.emission_system_budgets(trace, rows, met, diag, 1.0e-6)
        self.assertEqual(budgets[0]['status'], 'failed')
        with self.assertRaises(RuntimeError):
            run_case.emission_system_budgets('', rows, met, diag, 1.0e-6)

    def test_individual_group_fails_when_bounds_or_evidence_are_incomplete(self):
        with tempfile.TemporaryDirectory() as directory:
            result = verify(Path(directory), "individual")
        self.assertEqual(result["status"], "failed")
        self.assertEqual(len(result["required_cases"]), 9)
        self.assertTrue(result["failures"])

    def test_coupled_group_keeps_unmeasured_budgets_failed_and_checks_time_recovery(self):
        with tempfile.TemporaryDirectory() as directory:
            result = verify(Path(directory), "coupled")
        self.assertEqual(result["status"], "failed")
        self.assertEqual(len(result["required_cases"]), 3)
        rows = {row["case"]: row for row in result["required_cases"]}
        self.assertEqual(rows["emission-chemistry-removal"]["status"], "failed")
        self.assertEqual(rows["photolysis-gaschem"]["status"], "failed")
        self.assertEqual(rows["time-recovery"]["status"], "passed")

    def test_empty_output_cannot_satisfy_a_frozen_process_case(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            evidence_dir = root / "cases" / "drydep-empty"
            evidence_dir.mkdir(parents=True)
            output = root / "empty.nc"
            output.touch()
            (evidence_dir / "evidence.json").write_text(json.dumps({
                "status": "passed", "output_files": [str(output)],
                "scientific_acceptance": {"status": "passed"},
            }))
            report = verify(root, "individual")
        drydep = next(item for item in report["required_cases"] if item["case"] == "drydep")
        self.assertEqual(drydep["status"], "failed")
        self.assertEqual(drydep["reason"], "no passing nonempty evidence for the manifest SCM case")

    def test_absent_scientific_acceptance_is_a_skipped_gate_and_fails(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            evidence_dir = root / "cases" / "drydep-arm_sgp_summer_1997_A"
            evidence_dir.mkdir(parents=True)
            output = root / "drydep.nc"
            output.write_bytes(b"nonempty")
            (evidence_dir / "evidence.json").write_text(json.dumps({
                "status": "passed", "case": "arm_sgp_summer_1997_A",
                "output_files": [str(output)],
            }))
            report = verify(root, "individual")
        drydep = next(item for item in report["required_cases"] if item["case"] == "drydep")
        self.assertEqual(drydep["status"], "failed")

    def test_disabled_scientific_acceptance_cannot_pass(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            evidence_dir = root / "cases" / "drydep-arm_sgp_summer_1997_A"
            evidence_dir.mkdir(parents=True)
            output = root / "drydep.nc"
            output.write_bytes(b"nonempty")
            (evidence_dir / "evidence.json").write_text(json.dumps({
                "status": "passed", "case": "arm_sgp_summer_1997_A",
                "output_files": [str(output)],
                "scientific_acceptance": {"status": "disabled"},
            }))
            report = verify(root, "individual")
        drydep = next(item for item in report["required_cases"] if item["case"] == "drydep")
        self.assertEqual(drydep["status"], "failed")

    def test_missing_or_unregistered_required_tests_are_rejected(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            test_dir = root / "tests"
            test_dir.mkdir()
            cmake = root / "CMakeLists.txt"
            cmake.write_text("")
            with self.assertRaisesRegex(RuntimeError, "Required CCPP tests are absent or unregistered"):
                ccpp_scm.validate_required_tests(cmake, test_dir)

    def test_source_lock_with_malformed_component_revision_is_rejected(self):
        with tempfile.TemporaryDirectory() as directory:
            lock = json.loads((ROOT / "tests/ccpp-scm/source-lock.json").read_text())
            lock["components"]["physics"]["revision"] = "mutable-branch"
            lock_path = Path(directory) / "source-lock.json"
            lock_path.write_text(json.dumps(lock))
            with self.assertRaisesRegex(ValueError, "full 40-character commit hash"):
                ccpp_scm.load_lock(lock_path)

    def test_cap_with_mismatched_compiler_container_is_not_reused(self):
        previous = {"status": "passed", "process": "drydep", "case": "ARM",
                    "suites": ["CATChem_DryDep"], "container_image": "gcc-image-a",
                    "overlay_sha256": "overlay"}
        cache = ("CCPP_SUITES:STRING=CATChem_DryDep\n"
                 "CATCHEM_CCPP_PROCESSES:STRING=drydep\n"
                 "CATCHEM_BUILD_MUSICA:BOOL=OFF\nCATCHEM_ENABLE_KOKKOS:BOOL=OFF\n")
        self.assertFalse(run_case.reusable_cap_matches(
            previous, cache, process="drydep", case="ARM", suites=["CATChem_DryDep"],
            docker_image="gcc-image-b", overlay_sha256="overlay", science_profile="process"))

    def test_case_manifest_runtime_overrides_are_applied_to_scm_namelist(self):
        with tempfile.TemporaryDirectory() as directory:
            source = Path(directory)
            namelist = source / "scm/etc/case_config/gabls3_noahmp.nml"
            namelist.parent.mkdir(parents=True)
            namelist.write_text("&case_config\n do_spinup = .true.\n spinup_timesteps = 12\n/\n")
            result = run_case.apply_case_runtime_overrides(
                source, "gabls3_noahmp",
                "case_runtime_overrides:\n  do_spinup: false\n  spinup_timesteps: 0\n  rationale: omit spinup\nschedule:\n")
            content = namelist.read_text()
        self.assertEqual(result["values"], {"do_spinup": "false", "spinup_timesteps": "0"})
        self.assertIn("do_spinup = .false.", content)
        self.assertIn("spinup_timesteps = 0", content)

    def test_night_case_changes_only_the_locked_arm_clock(self):
        import netCDF4
        import numpy as np

        with tempfile.TemporaryDirectory() as directory:
            source = Path(directory)
            inputs = source / "scm/data/processed_case_input"
            inputs.mkdir(parents=True)
            config_dir = source / "scm/etc/case_config"
            config_dir.mkdir(parents=True)
            original = inputs / "arm_sgp_summer_1997_A_SCM_driver.nc"
            with netCDF4.Dataset(original, "w") as dataset:
                dataset.createDimension("time", 2)
                dataset.createDimension("t0", 1)
                time = dataset.createVariable("time", "f8", ("time",))
                time[:] = [0.0, 10800.0]
                time.units = "seconds since 1997-06-26 23:00:00"
                t0 = dataset.createVariable("t0", "f8", ("t0",))
                t0[:] = [0.0]
                t0.units = time.units
                field = dataset.createVariable("temperature", "f8", ("time",))
                field[:] = [290.0, 291.0]
                longitude = dataset.createVariable("lon", "f8", ("time",))
                longitude[:] = [-97.4833, -97.4833]
                latitude = dataset.createVariable("lat", "f8", ("time",))
                latitude[:] = [36.6, 36.6]
                dataset.start_date = "1997-06-26 23:00:00"
                dataset.end_date = "1997-06-30 23:00:00"
                dataset.case = "arm_sgp_summer_1997_A"
                dataset.title = "locked ARM input"
            (config_dir / "arm_sgp_summer_1997_A.nml").write_text(
                "&case_config\n case_name = 'arm_sgp_summer_1997_A'\n/\n")

            evidence = night_case.stage(source)
            night_path = inputs / "arm_sgp_summer_1997_A_night_SCM_driver.nc"
            with netCDF4.Dataset(original) as day, netCDF4.Dataset(night_path) as night:
                np.testing.assert_array_equal(day.variables["temperature"][:],
                                              night.variables["temperature"][:])
                np.testing.assert_array_equal(day.variables["time"][:], night.variables["time"][:])
                self.assertEqual(night.start_date, "1997-06-27 09:00:00")
                self.assertEqual(night.variables["t0"].units, "seconds since 1997-06-27 09:00:00")
            namelist = (config_dir / "arm_sgp_summer_1997_A_night.nml").read_text()
        self.assertEqual(evidence["clock_start_utc"], "1997-06-27T09:00:00Z")
        self.assertIn("case_name = 'arm_sgp_summer_1997_A_night'", namelist)

    def test_changed_locked_data_checksum_is_rejected(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            baseline = work / "baseline-source"
            for relative in run_case.GOCART_CLIMATOLOGY_FILES:
                path = baseline / relative
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_bytes(b"locked input")
            checksums = {relative: run_case.digest(baseline / relative)
                         for relative in run_case.GOCART_CLIMATOLOGY_FILES}
            (baseline / "data-input-checksums.json").write_text(json.dumps(checksums))
            corrupted = baseline / run_case.GOCART_CLIMATOLOGY_FILES[0]
            corrupted.write_bytes(b"changed input")
            with patch.object(run_case.subprocess, "check_output", return_value="locked-revision\n"):
                with self.assertRaisesRegex(RuntimeError, "GOCART climatology checksum mismatch"):
                    run_case.stage_gocart_climatology(baseline, work, "locked-revision")

    def test_time_recovery_requires_frozen_tolerance_matching_real_cap_evidence(self):
        summary = ROOT / "build/ccpp-scm/native-replay/leap-day-subcycles-final/summary.json"
        if summary.is_file():
            self.assertIsNone(check_time_recovery_evidence(summary))
        with tempfile.TemporaryDirectory() as directory:
            absent = Path(directory) / "missing.json"
            self.assertIsNotNone(check_time_recovery_evidence(absent))

    def test_emission_budget_audit_requires_units_and_system_budget_evidence(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            state = root / "state.jsonl"
            state.write_text("".join(json.dumps({"process": process, "species": "so4",
                                                  "units": "", "before": 1.0, "after": 1.0}) + "\n"
                               for process in ("dust", "seasalt", "wetdep", "settling", "drydep",
                                               "so4chem", "carbchem")))
            met = root / "met.jsonl"
            met.write_text(json.dumps({"fields": {"AIRDEN": {}, "DELP": {}, "PEDGE": {}}}) + "\n")
            evidence = root / "evidence.json"
            evidence.write_text(json.dumps({
                "status": "blocked",
                "boundary_state_ledger": {"path": str(state)},
                "met_input_ledger": {"path": str(met)},
                "invocation_ledger": [{"process": process} for process in
                                       ("dust", "seasalt", "wetdep", "settling", "drydep",
                                        "so4chem", "carbchem")],
                "mass_budget": {"status": "not_demonstrated", "reported_process_budgets": []},
            }))
            audit = audit_emission_budget_inputs(evidence)
        self.assertEqual(audit["status"], "insufficient")
        self.assertEqual(audit["state_rows"], 7)
        self.assertEqual(audit["blank_unit_rows"], 7)
        self.assertTrue(any("omits constituent units" in item for item in audit["failures"]))
        self.assertTrue(any("system-level" in item for item in audit["failures"]))

    def test_emission_budget_audit_separates_local_closures_from_system_acceptance(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            state = root / "state.jsonl"
            state.write_text(json.dumps({"process": "wetdep", "species": "so4", "units": "kg kg-1",
                                         "before": 1.0, "after": 0.9}) + "\n")
            met = root / "met.jsonl"
            met.write_text(json.dumps({"fields": {"DELP": {}, "AIRDEN": {}}}) + "\n")
            evidence = root / "evidence.json"
            processes = ("dust", "seasalt", "wetdep", "settling", "drydep", "so4chem", "carbchem")
            budgets = [
                {"name": "drydep_aerosol_removal", "residual": 1.0e-12, "total": 1.0, "sum_of_bins": 1.0},
                {"name": "so4chem_production", "species": "so4", "residual": 1.0e-12,
                 "total": 1.0, "sum_of_bins": 1.0},
                {"name": "settling_column_mass", "residual": 1.0e-20,
                 "mass_loss": 1.0e-7, "boundary_flux_mass": 1.0e-7},
            ]
            evidence.write_text(json.dumps({
                "status": "blocked",
                "boundary_state_ledger": {"path": str(state)},
                "met_input_ledger": {"path": str(met)},
                "invocation_ledger": [{"process": process} for process in processes],
                "mass_budget": {"status": "not_demonstrated", "reported_process_budgets": budgets},
            }))
            audit = audit_emission_budget_inputs(evidence)
        self.assertEqual(audit["status"], "insufficient")
        self.assertEqual(audit["failed_process_budget_closures"], [])
        self.assertIn("settling_column_mass", audit["process_budget_closures"])
        self.assertEqual(audit["system_budget_status"], "not_demonstrated")
        self.assertTrue(any("system-level" in item for item in audit["failures"]))

    def test_system_budget_status_requires_finite_numeric_species_closures(self):
        required = ("aerosol_mass_by_species", "sulfur_atoms")
        self.assertTrue(any(message.startswith("mass_budget must be an object")
                            for message in validate_system_budget_records(None, required)))
        missing = validate_system_budget_records({"status": "passed"}, required)
        self.assertIn("missing system budget record: aerosol_mass_by_species", missing)
        passed = {
            "system_budgets": [
                {"name": "aerosol_mass_by_species", "status": "passed", "units": "kg m-2",
                 "species_budgets": [{"species": "so4", "status": "passed", "residual": 1.0e-12,
                                      "tolerance": 1.0e-10, "units": "kg m-2"}]},
                {"name": "sulfur_atoms", "status": "passed", "residual": 1.0e-12,
                 "tolerance": 1.0e-10, "units": "mol S m-2"},
            ]
        }
        frozen = {"aerosol_mass_by_species": 1.0e-10, "sulfur_atoms": 1.0e-10}
        self.assertEqual(validate_system_budget_records(passed, required, frozen), [])
        self.assertTrue(any('complete required species inventory' in message for message in
                            validate_system_budget_records(passed, required, frozen, {'so4', 'msa'})))
        passed["system_budgets"][1]["residual"] = float("nan")
        failures = validate_system_budget_records(passed, required, frozen)
        self.assertIn("system budget residual or tolerance is missing/invalid: sulfur_atoms", failures)
        passed["system_budgets"][1]["residual"] = 1.0e-12
        passed["system_budgets"][1]["tolerance"] = 1.0
        failures = validate_system_budget_records(passed, required, frozen)
        self.assertIn("system budget tolerance differs from frozen manifest: sulfur_atoms", failures)

    def test_system_budget_limits_are_read_from_manifest_not_evidence(self):
        manifest = """budgets:\n  - quantity: aerosol_mass_by_species\n    residual_tolerance: 1.0e-8\n  - quantity: sulfur_atoms\n    residual_tolerance: 2.5e-9\nfrozen_before_candidate_comparison: true\n"""
        self.assertEqual(manifest_system_budget_tolerances(manifest), {
            "aerosol_mass_by_species": 1.0e-8,
            "sulfur_atoms": 2.5e-9,
        })

    def test_wrong_scm_case_cannot_satisfy_process_manifest(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            evidence_dir = root / "cases" / "drydep-gabls3_noahmp-CATChem_DryDep"
            evidence_dir.mkdir(parents=True)
            output = root / "wrong-case.nc"
            output.write_bytes(b"nonempty")
            (evidence_dir / "evidence.json").write_text(json.dumps({
                "status": "passed", "case": "gabls3_noahmp",
                "output_files": [str(output)],
                "scientific_acceptance": {"status": "passed"},
            }))
            report = verify(root, "individual")
        drydep = next(item for item in report["required_cases"] if item["case"] == "drydep")
        self.assertEqual(drydep["status"], "failed")
        self.assertEqual(drydep["expected_scm_cases"], ["arm_sgp_summer_1997_A"])
        self.assertEqual(len(drydep["wrong_case_evidence"]), 1)

    def test_unmeasured_coupled_budget_cannot_pass_with_frozen_tolerances(self):
        with tempfile.TemporaryDirectory() as directory:
            report = verify(Path(directory), "coupled")
        emission = next(item for item in report["required_cases"]
                        if item["case"] == "emission-chemistry-removal")
        self.assertEqual(emission["status"], "failed")
        self.assertEqual(emission["budget_input_audit"]["status"], "insufficient")
        self.assertTrue(emission["budget_input_audit"]["system_budget_failures"])

    def test_failed_or_out_of_tolerance_native_reference_cannot_pass(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            bad = root / "bad.json"
            bad.write_text(json.dumps({
                "status": "passed", "max_absolute_error": 1.0,
                "max_relative_error": 1.0,
            }))
            failed = root / "failed.json"
            failed.write_text(json.dumps({"status": "failed"}))
            invalid = check_native_reference_evidence(
                ["missing.json", "bad.json", "failed.json"], 1.0e-12, 1.0e-6, root)
        self.assertEqual(invalid, ["missing.json", "bad.json", "failed.json"])


if __name__ == "__main__":
    unittest.main()

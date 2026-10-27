#!/usr/bin/env python3
"""Validate CATChem suite XML ordering and scheme-to-metadata contracts."""

from __future__ import annotations

import argparse
import json
from pathlib import Path
import re
import xml.etree.ElementTree as ET

ROOT = Path(__file__).resolve().parents[2]
SUITES = ROOT / "tests/ccpp/suites"
METADATA = ROOT / "drivers/ccpp"
REQUIRED = ROOT / "tests/ccpp-scm/cases/required-cases.json"


def suite_schemes(path: Path) -> list[str]:
    """Return scheme names in CCPP suite execution order."""
    root = ET.parse(path).getroot()
    return [item.text.strip() for item in root.iter("scheme") if item.text]


def validate_suite(path: Path) -> list[str]:
    """Check lifecycle pairing, process ordering, update placement, and metadata names."""
    schemes = suite_schemes(path)
    errors: list[str] = []
    process_schemes = [item for item in schemes if re.fullmatch(r"catchem_(dust|seasalt|drydep|wetdep|settling|so4chem|carbchem|photolysis|gaschem)_driver", item)]
    lifecycle_occurrences = schemes.count("catchem_ccpp")
    expected_lifecycle_occurrences = 2
    if process_schemes and lifecycle_occurrences != expected_lifecycle_occurrences:
        errors.append(f"suite must schedule catchem_ccpp {expected_lifecycle_occurrences} time(s) for its host lifecycle boundaries")
    if any(item in schemes for item in ("catchem_ccpp_timestep_init", "catchem_ccpp_timestep_final")):
        errors.append("suite must name the catchem_ccpp scheme table; CCPP generates lifecycle calls from its phase entry points")
    suite_root = ET.parse(path).getroot()
    groups = {group.get("name"): group for group in suite_root.findall("group")}
    if not {"catchem1", "catchem2"}.issubset(groups):
        errors.append("suite must expose the two host-called CATChem schedule groups catchem1 and catchem2")
    if {"catchem1", "catchem2"}.issubset(groups):
        catchem1 = [item.text.strip() for item in groups["catchem1"].iter("scheme") if item.text]
        catchem2 = [item.text.strip() for item in groups["catchem2"].iter("scheme") if item.text]
        early = {"catchem_dust_driver", "catchem_seasalt_driver", "catchem_wetdep_driver",
                 "catchem_settling_driver", "catchem_drydep_driver"}
        late = {"catchem_photolysis_driver", "catchem_gaschem_driver", "catchem_so4chem_driver",
                "catchem_carbchem_driver"}
        if early.intersection(catchem2) or late.intersection(catchem1):
            errors.append("surface emissions/deposition belong in catchem1 and post-microphysics chemistry in catchem2")
        if not catchem1 or catchem1[0] != "catchem_ccpp" or not catchem2 or catchem2[-1] != "catchem_ccpp":
            errors.append("catchem_ccpp must open catchem1 and close catchem2 around the shared interval")
    if groups.get("phys_ps") is not None and any(
        item.text and item.text.strip().startswith("catchem_")
        for item in groups["phys_ps"].iter("scheme")
    ):
        errors.append("CATChem schemes must be isolated from the host phys_ps group")
    if groups.get("phys_ts") is not None and any(
        item.text and item.text.strip().startswith("catchem_")
        for item in groups["phys_ts"].iter("scheme")
    ):
        errors.append("CATChem schemes must be isolated from the host phys_ts group")
    expected_updates = sum(
        int(subcycle.attrib.get("loop", "1")) * sum(
            1 for item in subcycle.findall("scheme")
            if item.text and item.text.strip().startswith("catchem_")
            and "photolysis" not in item.text
            and item.text.strip().endswith("_driver")
        )
        for group in suite_root.findall("group") for subcycle in group.findall("subcycle")
    )
    scheduled_updates = sum(
        int(subcycle.attrib.get("loop", "1")) * sum(
            1 for item in subcycle.findall("scheme")
            if item.text and item.text.strip() == "catchem_apply_tendency"
        )
        for group in suite_root.findall("group") for subcycle in group.findall("subcycle")
    )
    if process_schemes and scheduled_updates != expected_updates:
        errors.append("each concentration-changing process invocation needs one tendency update; photolysis does not")
    selected = [item.removeprefix("catchem_").removesuffix("_driver")
                for item in process_schemes]
    if path.name.startswith("suite_CATChem_EmissionChemistryRemoval"):
        expected = ["dust", "seasalt", "wetdep", "settling", "drydep", "so4chem", "carbchem"]
        if selected != expected:
            errors.append(f"phys_ps removal processes must precede phys_ts chemistry while preserving the requested SO4Chem order; expected {expected}, found {selected}")
        phys_ps = next((group for group in suite_root.findall("group") if group.get("name") == "phys_ps"), None)
        surface_schemes = ([item.text.strip() for item in phys_ps.iter("scheme") if item.text]
                           if phys_ps is not None else [])
        lsm_schemes = {"GFS_surface_composites_pre", "GFS_surface_composites_inter",
                       "GFS_surface_composites_post", "noahmpdrv"}
        if path.name.endswith("_ps.xml"):
            if "scm_sfc_flux_spec" not in surface_schemes or lsm_schemes.intersection(surface_schemes):
                errors.append("EmissionChemistryRemoval_ps must use prescribed surface fluxes and omit the LSM")
        elif "scm_sfc_flux_spec" in surface_schemes or not lsm_schemes.issubset(surface_schemes):
            errors.append("EmissionChemistryRemoval must use the initialized GFS land/ocean surface sequence")
    if path.name.startswith("suite_CATChem_SO4Chem"):
        if selected != ["so4chem"]:
            errors.append(f"SO4Chem suite must contain only the SO4Chem process driver; found {selected}")
        phys_ps = next((group for group in suite_root.findall("group") if group.get("name") == "phys_ps"), None)
        surface_schemes = ([item.text.strip() for item in phys_ps.iter("scheme") if item.text]
                           if phys_ps is not None else [])
        if "scm_sfc_flux_spec" not in surface_schemes:
            errors.append("SO4Chem ARM surface sequence must call scm_sfc_flux_spec")
        if any(item in surface_schemes for item in (
            "noahmpdrv", "sfc_sice", "GFS_surface_composites_pre", "GFS_surface_composites_inter",
            "GFS_surface_composites_post",
        )):
            errors.append("SO4Chem ARM surface sequence must omit uninitialized land/ocean surface models")
    if path.name.startswith("suite_CATChem_PhotolysisGasChem") and selected != ["photolysis", "gaschem"]:
        errors.append(f"photolysis/GasChem process order must be ['photolysis', 'gaschem']; found {selected}")
    if path.name.startswith("suite_CATChem_PhotolysisGasChem"):
        expected_order = ["catchem_ccpp", "catchem_photolysis_driver",
                          "catchem_gaschem_driver", "catchem_apply_tendency", "catchem_ccpp"]
        selected_order = [item for item in schemes if item.startswith("catchem_")]
        if selected_order != expected_order:
            errors.append(f"photolysis/GasChem sequence must be {expected_order}; photolysis has no tendency to apply, found {selected_order}")
        phys_ps = next((group for group in suite_root.findall("group") if group.get("name") == "phys_ps"), None)
        surface_schemes = ([item.text.strip() for item in phys_ps.iter("scheme") if item.text]
                           if phys_ps is not None else [])
        if "scm_sfc_flux_spec" not in surface_schemes:
            errors.append("ARM Photolysis/GasChem surface sequence must use the case's prescribed fluxes")
        if any(item in surface_schemes for item in (
            "noahmpdrv", "sfc_sice", "GFS_surface_composites_pre", "GFS_surface_composites_inter",
            "GFS_surface_composites_post",
        )):
            errors.append("ARM Photolysis/GasChem must omit uninitialized land/ocean surface models")
    if path.name in {"suite_CATChem_Photolysis.xml", "suite_CATChem_Photolysis_ps.xml"}:
        phys_ps = next((group for group in suite_root.findall("group") if group.get("name") == "phys_ps"), None)
        surface_schemes = ([item.text.strip() for item in phys_ps.iter("scheme") if item.text]
                           if phys_ps is not None else [])
        if "scm_sfc_flux_spec" not in surface_schemes:
            errors.append("ARM Photolysis/GasChem process suites must use prescribed surface fluxes")
        if any(item in surface_schemes for item in (
            "noahmpdrv", "sfc_sice", "GFS_surface_composites_pre", "GFS_surface_composites_inter",
            "GFS_surface_composites_post",
        )):
            errors.append("ARM Photolysis/GasChem process suites must omit uninitialized surface models")
    if "photolysis" in selected and "gaschem" in selected:
        if selected.index("photolysis") > selected.index("gaschem"):
            errors.append("photolysis rates must be generated before GasChem")
    for scheme in process_schemes:
        stem = scheme.removesuffix("_driver")
        metadata = METADATA / f"{stem}.meta"
        if not metadata.is_file():
            errors.append(f"missing CCPP metadata for {scheme}: {metadata.relative_to(ROOT)}")
        elif not re.search(rf"(?m)^\s*name\s*=\s*{re.escape(scheme)}\s*$", metadata.read_text()):
            errors.append(f"metadata {metadata.name} does not declare scheme {scheme}")
        if metadata.is_file() and any(name in stem for name in ("photolysis", "gaschem")):
            # Prescribed-flux SCM cases initialize effective momentum roughness
            # (zorl), while the separate land-tile field (zorll) may remain fill.
            roughness = re.search(r"(?ms)^\[z0\]\s*\n(.*?)(?=^\[|\Z)", metadata.read_text())
            if not roughness or not re.search(
                r"(?m)^\s*standard_name\s*=\s*surface_roughness_length\s*$", roughness.group(1)
            ):
                errors.append(f"{metadata.name} must bind Z0 to effective surface_roughness_length, not an uninitialized land-tile roughness")
    if "catchem_ccpp" in schemes and not re.search(
        r"(?m)^\s*name\s*=\s*catchem_ccpp\s*$", (METADATA / "catchem_ccpp_lifecycle.meta").read_text()
    ):
        errors.append("lifecycle metadata table is not named catchem_ccpp")
    if "catchem_apply_tendency" in schemes and not re.search(
        r"(?m)^\s*name\s*=\s*catchem_apply_tendency\s*$",
        (ROOT / "drivers/ccpp/catchem_apply_tendency.meta").read_text(),
    ):
        errors.append("tendency metadata does not declare catchem_apply_tendency_run")
    return errors


def main() -> int:
    """Validate both Phase 5 coupled suites and their required-case registrations."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--suite", action="append", help="suite XML to validate (repeatable)")
    args = parser.parse_args()
    names = args.suite or [
        "suite_CATChem_EmissionChemistryRemoval.xml",
        "suite_CATChem_EmissionChemistryRemoval_ps.xml",
        "suite_CATChem_PhotolysisGasChem.xml",
        "suite_CATChem_PhotolysisGasChem_ps.xml",
        "suite_CATChem_PhotolysisGasChemSubcycle.xml",
        "suite_CATChem_PhotolysisGasChemSubcycle_ps.xml",
        "suite_CATChem_Photolysis.xml",
        "suite_CATChem_Photolysis_ps.xml",
        "suite_CATChem_SO4Chem.xml",
        "suite_CATChem_SO4Chem_ps.xml",
    ]
    expected = json.loads(REQUIRED.read_text())
    registered = {case["id"]: case for case in expected.get("required_cases", [])}
    requirements = {
        "suite_CATChem_EmissionChemistryRemoval.xml": ("emission-chemistry-removal", ["dust", "seasalt", "wetdep", "settling", "drydep", "so4chem", "carbchem"]),
        "suite_CATChem_EmissionChemistryRemoval_ps.xml": ("emission-chemistry-removal", ["dust", "seasalt", "wetdep", "settling", "drydep", "so4chem", "carbchem"]),
        "suite_CATChem_PhotolysisGasChem.xml": ("photolysis-gaschem", ["photolysis", "gaschem"]),
        "suite_CATChem_PhotolysisGasChem_ps.xml": ("photolysis-gaschem", ["photolysis", "gaschem"]),
    }
    failed = False
    for name in names:
        path = SUITES / name
        errors = validate_suite(path)
        if name in requirements:
            case_id, process_list = requirements[name]
            case = registered.get(case_id)
            expected_suite_key = "suite_ps" if name.endswith("_ps.xml") else "suite"
            if not case or case.get("processes") != process_list or case.get(expected_suite_key) != f"tests/ccpp/suites/{name}":
                errors.append(f"required-cases.json does not correctly register {case_id} ({expected_suite_key})")
            if case_id == "photolysis-gaschem" and (
                case.get("scm_cases") != ["arm_sgp_summer_1997_A", "arm_sgp_summer_1997_A_night"]
                or "prescribed surface fluxes" not in case.get("acceptance_surface", "")
            ):
                errors.append("Photolysis/GasChem must register the paired ARM day/night cases with prescribed surface fluxes")
            manifest = ROOT / "tests/ccpp-scm/cases" / f"{case_id}.yml"
            if not manifest.is_file():
                errors.append(f"missing case manifest: {manifest.relative_to(ROOT)}")
        print(json.dumps({"suite": name, "status": "failed" if errors else "passed_structural_checks", "errors": errors}))
        failed = failed or bool(errors)
    return 1 if failed else 0


if __name__ == "__main__":
    raise SystemExit(main())

#!/usr/bin/env python3
"""Configure, build, and run one locked CCPP-SCM case for one CATChem process."""

from __future__ import annotations

import argparse
import hashlib
import json
import math
import os
from pathlib import Path
import re
import shlex
import subprocess
import sys
import xml.etree.ElementTree as ET
from datetime import datetime, timezone
import yaml

ROOT = Path(__file__).resolve().parents[2]
LOCK = ROOT / "tests/ccpp-scm/source-lock.json"
PROCESSES = (
    "dust", "seasalt", "drydep", "wetdep", "settling", "so4chem",
    "carbchem", "photolysis", "gaschem",
)
PROCESS_GROUPS = {
    "emission-chemistry-removal": ("dust", "seasalt", "wetdep", "settling", "drydep", "so4chem", "carbchem"),
    "photolysis-gaschem": ("photolysis", "gaschem"),
}
GOCART_CLIMATOLOGY_FILES = tuple(
    [f"scm/data/physics_input_data/aeroclim.m{month:02d}.nc" for month in range(1, 13)]
    + [f"scm/data/physics_input_data/optics_{species}.dat" for species in ("DU", "SS", "SU", "BC", "OC")]
)


def mechanism_species_exchange_budget(run_text: str, met_rows: list[dict], budget: dict,
                                     tolerance: float) -> dict:
    """Integrate configured weighted solver species and diagnosed exchange over column height."""
    number = r"[-+0-9.eEdD]+"
    records = re.findall(
        rf"CATCHEM_GASCHEM_STATE column=(\d+) level=(\d+) species=([^ ]+) "
        rf"initial=({number}) final=({number}) units=mol_m-3", run_text)
    gas_blocks = [row for row in met_rows if row["process"] == "gaschem"]
    quantity = str(budget["quantity"])
    state_weights = {str(name).upper(): float(weight)
                     for name, weight in budget["state_weights"].items()}
    exchange_weights = {str(name).upper(): float(weight)
                        for name, weight in budget["exchange_weights"].items()}
    exchange_factor = float(budget["exchange_factor"])
    if not state_weights or not exchange_weights or not math.isfinite(exchange_factor):
        raise RuntimeError(f"Configured mechanism budget is incomplete: {quantity}")
    if not gas_blocks or not records:
        raise RuntimeError(f"Mechanism budget has no GasChem state ledger: {quantity}")

    first_cell = (int(records[0][0]), int(records[0][1]))
    species_count = 0
    for column, level, _, _, _ in records:
        if (int(column), int(level)) != first_cell:
            break
        species_count += 1
    if species_count == 0:
        raise RuntimeError(f"Mechanism budget state ledger has no solver species: {quantity}")

    initial, final, production, loss = [], [], [], []
    position = 0
    for block in gas_blocks:
        z = block["fields"]["Z"]
        columns, interfaces = z["columns"], z["levels"]
        levels = interfaces - 1
        cell_count = columns * levels
        block_size = cell_count * species_count
        block_records = records[position:position + block_size]
        if len(block_records) != block_size:
            raise RuntimeError(f"Mechanism budget lacks a complete state block: {quantity}")
        for cell in range(cell_count):
            cell_records = block_records[cell * species_count:(cell + 1) * species_count]
            column, level = int(cell_records[0][0]), int(cell_records[0][1])
            if not (1 <= column <= columns and 1 <= level <= levels):
                raise RuntimeError(f"Mechanism budget has an invalid solver cell: {quantity}")
            values_by_species = {}
            for row_column, row_level, species_name, before, after in cell_records:
                if (int(row_column), int(row_level)) != (column, level):
                    raise RuntimeError(f"Mechanism budget solver-state block is misordered: {quantity}")
                species_name = species_name.upper()
                if species_name in values_by_species:
                    raise RuntimeError(f"Mechanism budget contains duplicate species {species_name}: {quantity}")
                before_value, after_value = fortran_float(before), fortran_float(after)
                if (not math.isfinite(before_value) or not math.isfinite(after_value) or
                        before_value < 0.0 or after_value < 0.0):
                    raise RuntimeError(f"Mechanism budget contains invalid solver concentrations: {quantity}")
                values_by_species[species_name] = (before_value, after_value)
            required_species = set(state_weights) | set(exchange_weights)
            if not required_species.issubset(values_by_species):
                missing = sorted(required_species - set(values_by_species))
                raise RuntimeError(f"Mechanism budget lacks configured species {missing}: {quantity}")
            dz = z["values"][level * columns + column - 1] - z["values"][(level - 1) * columns + column - 1]
            if not math.isfinite(dz) or dz <= 0.0:
                raise RuntimeError(f"Mechanism budget requires positive layer thickness: {quantity}")
            state_before = math.fsum(weight * values_by_species[name][0]
                                      for name, weight in state_weights.items())
            state_after = math.fsum(weight * values_by_species[name][1]
                                     for name, weight in state_weights.items())
            exchange_before = math.fsum(weight * values_by_species[name][0]
                                        for name, weight in exchange_weights.items())
            exchange_after = math.fsum(weight * values_by_species[name][1]
                                       for name, weight in exchange_weights.items())
            initial.append(state_before * dz)
            final.append(state_after * dz)
            net_exchange = exchange_factor * (exchange_before - exchange_after) * dz
            production.append(max(net_exchange, 0.0))
            loss.append(max(-net_exchange, 0.0))
        position += len(block_records)
    if not records or position != len(records):
        raise RuntimeError(f"Mechanism budget solver-state and meteorology block counts do not agree: {quantity}")
    before, after, source, sink = map(math.fsum, (initial, final, production, loss))
    absolute_residual = abs(math.fsum((before, source, -sink, -after)))
    residual = absolute_residual / max(abs(before) + abs(after) + abs(source) + abs(sink), 1.0e-30)
    return {"name": quantity, "units": budget["units"], "initial": before, "final": after,
            "production": source, "loss": sink, "absolute_residual": absolute_residual,
            "residual": residual, "tolerance": tolerance, "cells": position,
            "exchange_definition": budget.get("exchange_definition", "Configured weighted exchange inventory"),
            "status": "passed" if residual <= tolerance else "failed"}


def emission_system_budgets(run_text: str, state_rows: list[dict], met_rows: list[dict],
                            diagnostics: list[dict], tolerance: float) -> list[dict]:
    """Close native GOCART pressure-coordinate column inventories using diagnosed terms."""
    processes = PROCESS_GROUPS["emission-chemistry-removal"]
    aerosols = ("so4", "msa", "bc1", "bc2", "oc1", "oc2", *[f"dust{i}" for i in range(1, 6)],
                *[f"seas{i}" for i in range(1, 6)])
    sulfur_weights = {"so2": 0.06404, "dms": 0.06213, "so4": 0.09606, "msa": 0.09610}
    if not state_rows or len(state_rows) % len(processes):
        raise RuntimeError("Coupled budget requires complete state blocks for all seven processes")
    block_size = len(state_rows) // len(processes)
    inventories = {}
    for sequence, process in enumerate(processes):
        block = next((item for item in met_rows if item['process'] == process), None)
        if block is None:
            raise RuntimeError(f"Coupled budget is missing {process} meteorology")
        fields = block['fields']
        columns = fields['PMID']['columns'] if 'PMID' in fields else fields['DELP']['columns']
        if 'DELP' in fields:
            pressure_mass = [value / 9.80665 for value in fields['DELP']['values']]
        else:
            edge = fields['PEDGE']['values']
            pressure_mass = [(edge[i] - edge[i + columns]) / 9.80665 for i in range(len(edge) - columns)]
        by_species = {}
        for row in state_rows[sequence * block_size:(sequence + 1) * block_size]:
            if row['units'].strip() != 'kg kg-1':
                raise RuntimeError("Coupled budget requires explicit host kg kg-1 state")
            cell = (row['level'] - 1) * columns + row['column'] - 1
            mass = pressure_mass[cell]
            if not math.isfinite(mass) or mass <= 0.0:
                raise RuntimeError("Coupled budget received a nonpositive pressure-coordinate layer mass")
            amounts = by_species.setdefault(row['species'], {'before': [], 'after': []})
            for side in ('before', 'after'):
                amounts[side].append(row[side] * mass)
        inventories[process] = {name: {side: math.fsum(values) for side, values in sides.items()}
                                for name, sides in by_species.items()}
    number = r"[-+0-9.eEdD]+"
    terms = {}
    for process, species, source, sink in re.findall(
            rf"CATCHEM_COLUMN_TERM process=(\S+) species=(\S+) source=({number}) sink=({number}) units=kg_m-2",
            run_text):
        key = (process, species.lower())
        if key in terms:
            raise RuntimeError(f"Duplicate diagnosed column budget term: {key}")
        source, sink = fortran_float(source), fortran_float(sink)
        if any(not math.isfinite(value) or value < 0.0 for value in (source, sink)):
            raise RuntimeError(f"Invalid diagnosed column budget term: {key}")
        terms[key] = (source, sink)
    diag = {row['name']: row['sum'] for row in diagnostics}
    for name in ('dust_emission_total', 'seasalt_mass_emission_total'):
        if diag.get(name) != 0.0:
            raise RuntimeError("This land-case budget requires the independently diagnosed zero emission regime")
    for species in (*aerosols, 'so2'):
        if f'wetdep_flux_{species}' in diag and ('wetdep', species) not in terms:
            raise RuntimeError(f"Coupled budget lacks the surface wet-deposition boundary flux for {species}")
    for species in aerosols:
        if ('settling', species) not in terms:
            raise RuntimeError(f"Coupled budget lacks species-resolved settling boundary flux for {species}")
    for species in ('so4', 'msa'):
        if ('so4chem', species) not in terms:
            raise RuntimeError(f"Coupled budget lacks diagnosed chemical production for {species}")
    for species in ('bc1', 'bc2', 'oc1', 'oc2'):
        if ('carbchem', species) not in terms:
            raise RuntimeError(f"Coupled budget lacks diagnosed carbon transfer for {species}")
    if ('drydep', 'so2') not in terms:
        raise RuntimeError("Coupled sulfur budget lacks diagnosed SO2 dry deposition")
    species_budgets = []
    for species in aerosols:
        before = inventories['dust'][species]['before']
        after = inventories['carbchem'][species]['after']
        source = math.fsum(value[0] for (process, name), value in terms.items() if name == species)
        sink = math.fsum(value[1] for (process, name), value in terms.items() if name == species)
        absolute = abs(math.fsum((before, source, -sink, -after)))
        residual = absolute / max(abs(before) + abs(after) + abs(source) + abs(sink), 1.0e-30)
        species_budgets.append({'species': species, 'units': 'kg m-2', 'initial': before, 'final': after,
                                'production': source, 'loss': sink, 'absolute_residual': absolute,
                                'residual': residual, 'tolerance': tolerance,
                                'status': 'passed' if residual <= tolerance else 'failed'})
    initial_sulfur = math.fsum(inventories['dust'][name]['before'] / mw for name, mw in sulfur_weights.items())
    final_sulfur = math.fsum(inventories['carbchem'][name]['after'] / mw for name, mw in sulfur_weights.items())
    sulfur_sink = math.fsum(value[1] / sulfur_weights[name] for (process, name), value in terms.items()
                           if name in sulfur_weights and process in ('wetdep', 'drydep', 'settling'))
    if diag.get('DMS_emission_flux') != 0.0:
        raise RuntimeError("Land-case sulfur budget requires independently diagnosed zero marine DMS source")
    sulfur_absolute = abs(math.fsum((initial_sulfur, -sulfur_sink, -final_sulfur)))
    sulfur_residual = sulfur_absolute / max(initial_sulfur + final_sulfur + sulfur_sink, 1.0e-30)
    return [
        {'name': 'aerosol_mass_by_species', 'units': 'kg m-2', 'species_budgets': species_budgets,
         'denominator_recipe': 'native GOCART pressure-coordinate column air mass DELP/g',
         'status': 'passed' if all(row['status'] == 'passed' for row in species_budgets) else 'failed'},
        {'name': 'sulfur_atoms', 'units': 'mol S m-2', 'initial': initial_sulfur, 'final': final_sulfur,
         'loss': sulfur_sink, 'absolute_residual': sulfur_absolute, 'residual': sulfur_residual,
         'tolerance': tolerance, 'molecular_weights_kg_mol': sulfur_weights,
         'denominator_recipe': 'native GOCART pressure-coordinate column air mass DELP/g',
         'status': 'passed' if sulfur_residual <= tolerance else 'failed'},
    ]


def run_step(name: str, command: list[str], cwd: Path, log_path: Path, env: dict[str, str],
             docker_image: str | None = None, expected_failure: bool = False) -> None:
    """Run one build or case step and save its complete combined output."""
    log_path.parent.mkdir(parents=True, exist_ok=True)
    if docker_image:
        command = [part.replace(str(ROOT), "/workspace") for part in command]
        if command and command[0] == sys.executable:
            command[0] = "python3"
        command_text = shlex.join(command)
        docker_cwd = str(cwd).replace(str(ROOT), "/workspace")
        docker_env: list[str] = []
        for env_name in ("CATCHEM_CONFIG", "CATCHEM_CCPP_PROCESS",
                     "CATCHEM_CCPP_REGISTRY_ORDER",
                     "CATCHEM_ACCEPTANCE_AEROSOL_MIXING_RATIO",
                     "CATCHEM_ACCEPTANCE_SOIL_MOISTURE",
                     "CATCHEM_ACCEPTANCE_WETDEP_RAIN_M",
                     "CATCHEM_ACCEPTANCE_WETDEP_FLUX_KG_M2_S",
                     "CATCHEM_ACCEPTANCE_SO2_PPMV", "CATCHEM_ACCEPTANCE_DMS_PPMV",
                     "CATCHEM_ACCEPTANCE_H2O2_PPMV", "CATCHEM_ACCEPTANCE_OH_PPMV",
                     "CATCHEM_ACCEPTANCE_NO3_PPMV", "CATCHEM_ACCEPTANCE_O3_PPMV",
                     "CATCHEM_ACCEPTANCE_O3_USE_SCM_PROFILE",
                     "CATCHEM_ACCEPTANCE_FAIL_AFTER_PROCESS", "CATCHEM_BOUNDARY_LEDGER",
                     "CATCHEM_MET_LEDGER"):
            value = env.get(env_name)
            if value:
                if env_name == "CATCHEM_ACCEPTANCE_O3_PPMV" and not (
                        env.get("CATCHEM_CCPP_PROCESS") in {"photolysis", "gaschem"}):
                    continue
                if env_name == "CATCHEM_CONFIG":
                    config_path = Path(value).resolve()
                    try:
                        config_path.relative_to(ROOT)
                    except ValueError as exc:
                        raise RuntimeError(
                            "Docker case configuration must be inside the mounted CATChem workspace"
                        ) from exc
                    value = str(config_path).replace(str(ROOT), "/workspace")
                docker_env.extend(["--env", f"{env_name}={value}"])
        command = ["docker", "run", "--rm", "--platform", "linux/amd64", *docker_env,
                   "--mount", f"type=bind,source={ROOT},target=/workspace",
                   "--workdir", docker_cwd, docker_image, "bash", "-lc", command_text]
        cwd = ROOT
    with log_path.open("w") as log:
        process = subprocess.run(command, cwd=cwd, env=env, text=True,
                                 stdout=log, stderr=subprocess.STDOUT, check=False)
    if expected_failure:
        if not process.returncode:
            raise RuntimeError(f"{name} unexpectedly succeeded; the injected failure did not fire")
        return
    if process.returncode:
        raise RuntimeError(f"{name} failed with exit {process.returncode}; see {log_path}")


def fortran_float(value: str) -> float:
    """Parse a decimal that may use Fortran's omitted-E exponent notation."""
    normalized = value.replace("D", "E").replace("d", "e")
    if not re.search(r"[Ee][+-]?\d+$", normalized):
        normalized = re.sub(r"([+-]\d+)$", r"E\1", normalized)
    return float(normalized)


def digest(path: Path) -> str:
    """Return the SHA-256 digest of a case input file."""
    value = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            value.update(chunk)
    return value.hexdigest()


def catchem_build_source_digest() -> str:
    """Fingerprint CCPP host, core, process, and build-definition sources used by the cap."""
    hasher = hashlib.sha256()
    roots = (ROOT / "CMakeLists.txt", ROOT / "src/api", ROOT / "src/core", ROOT / "src/process",
             ROOT / "drivers/ccpp",
             ROOT / "tests/ccpp-scm/patches")
    files: set[Path] = set()
    for root in roots:
        if root.is_file():
            files.add(root)
        elif root.is_dir():
            files.update(path for path in root.rglob("*") if path.is_file() and not path.is_symlink())
    for path in sorted(files):
        hasher.update(path.relative_to(ROOT).as_posix().encode())
        hasher.update(b"\0")
        with path.open("rb") as stream:
            for chunk in iter(lambda: stream.read(1024 * 1024), b""):
                hasher.update(chunk)
    return hasher.hexdigest()


def host_precision_backend(build: Path, source: Path) -> dict[str, object]:
    """Report the compiled CCPP host real kind and execution backend."""
    machine_source = source / "ccpp/physics/physics/hooks/machine.F"
    kind_source = machine_source.read_text(errors="replace")
    if (not re.search(r"\bkind_dbl_prec\s*=\s*8\b", kind_source) or
            "kind_phys = kind_dbl_prec" not in kind_source or
            "kind_phys = kind_sngl_prec" not in kind_source):
        raise RuntimeError("Pinned SCM machine module no longer declares the expected CCPP kind_phys mapping")
    flags_file = build / "scm/src/CMakeFiles/scm.dir/flags.make"
    flags_text = flags_file.read_text(errors="replace")
    single_precision = bool(re.search(r"(?:^|\s)-DSINGLE_PREC(?:\s|$)", flags_text))
    cache = (build / "CMakeCache.txt").read_text(errors="replace")
    kokkos_match = re.search(r"^CATCHEM_ENABLE_KOKKOS:BOOL=(ON|OFF)$", cache, re.MULTILINE)
    if not kokkos_match:
        raise RuntimeError("SCM build cache does not expose CATCHEM_ENABLE_KOKKOS")
    kokkos_enabled = kokkos_match.group(1) == "ON"
    storage_bytes = 4 if single_precision else 8
    return {
        "status": "passed",
        "kind": "CCPP kind_phys from machine.F",
        "single_precision_macro": single_precision,
        "storage_bytes": storage_bytes,
        "decimal_digits": 6 if single_precision else 15,
        "backend": "Kokkos-enabled host build" if kokkos_enabled else "CPU host build",
        "kokkos_enabled": kokkos_enabled,
        "evidence": [
            str(machine_source),
            str(flags_file),
            str(build / "CMakeCache.txt"),
        ],
    }


def remove_shadowed_catchem_modules(build: Path) -> None:
    """Remove stale Fortran modules that shadow configured module directories."""
    work_module_dir = build / "ccpp"
    if not work_module_dir.is_dir():
        return
    for module in work_module_dir.glob("catchem_*.mod"):
        module.unlink()
    # CMake writes current CCPP modules to build/ccpp/include, but gfortran
    # searches the generated cap source directory first. A stale module beside
    # that source silently wins over the rebuilt host module and can omit newly
    # generated dynamic-constituent fields.
    for module_name in ("catchem_apply_tendency.mod", "ccpp_host_constituents.mod"):
        shadowed_module = work_module_dir / module_name
        if shadowed_module.exists():
            shadowed_module.unlink()


def reusable_cap_matches(previous: dict[str, object], cache: str, *, process: str,
                         case: str, suites: list[str], docker_image: str | None,
                         overlay_sha256: str | None, science_profile: str,
                         processes: list[str] | None = None,
                         source_sha256: str | None = None,
                         failure_injection: bool = False) -> bool:
    """Return whether an existing cap matches its locked source and toolchain inputs."""
    expected_suites = ",".join(suites)
    selected_processes = processes or (["photolysis", "gaschem"] if process == "gaschem" else [process])
    if "gaschem" in selected_processes and "photolysis" not in selected_processes:
        selected_processes = ["photolysis", *selected_processes]
    expected_processes = ";".join(selected_processes)
    expected_musica = "ON" if science_profile == "all" or any(
        item in {"photolysis", "gaschem"} for item in selected_processes) else "OFF"
    return (
        previous.get("status") == "passed"
        and previous.get("processes") == selected_processes
        and previous.get("case") in {case, "coupled"}
        and previous.get("suites") == suites
        and previous.get("container_image") == docker_image
        and previous.get("overlay_sha256") == overlay_sha256
        and previous.get("source_sha256") == source_sha256
        and re.search(rf"(?m)^CCPP_SUITES:[^=]*={re.escape(expected_suites)}$", cache)
        and re.search(rf"(?m)^CATCHEM_CCPP_PROCESSES:[^=]*={re.escape(expected_processes)}$", cache)
        and f"CATCHEM_BUILD_MUSICA:BOOL={expected_musica}" in cache
        and f"CATCHEM_ENABLE_KOKKOS:BOOL={expected_musica}" in cache
        and f"CATCHEM_ENABLE_ACCEPTANCE_FAILURE_INJECTION:BOOL={'ON' if failure_injection else 'OFF'}" in cache
    )


def configure_land_prescribed_flux_case(source: Path, case: str) -> dict[str, object]:
    """Configure a supplied land case to use prescribed fluxes rather than an uninitialized LSM."""
    namelist = source / "scm/etc/case_config" / f"{case}.nml"
    if not namelist.is_file():
        raise RuntimeError(f"SCM case configuration is missing: {namelist}")
    content = namelist.read_text()
    for key, value in (("sfc_type", "1"), ("sfc_flux_spec", ".true.")):
        if re.search(rf"(?im)^\s*{key}\s*=", content):
            content, replacements = re.subn(
                rf"(?im)^(\s*){key}\s*=\s*[^,\n]+", rf"\g<1>{key} = {value}", content, count=1
            )
            if replacements != 1:
                raise RuntimeError(f"Could not set {key} in {namelist}")
        else:
            terminator = re.search(r"(?m)^\s*/\s*$", content)
            if not terminator:
                raise RuntimeError(f"SCM case configuration has no namelist terminator: {namelist}")
            content = content[:terminator.start()] + f"    {key} = {value},\n" + content[terminator.start():]
    namelist.write_text(content)
    return {"path": str(namelist), "sfc_type": 1, "sfc_flux_spec": True,
            "surface": "land_with_prescribed_fluxes", "sha256": digest(namelist)}


def apply_case_runtime_overrides(source: Path, case: str, manifest_text: str) -> dict[str, object]:
    """Apply explicitly declared SCM case runtime overrides from a case manifest."""
    section = re.search(r"(?ms)^case_runtime_overrides:\s*\n(.*?)(?=^[^ \t#][^\n]*:|\Z)", manifest_text)
    if not section:
        return {}
    namelist = source / "scm/etc/case_config" / f"{case}.nml"
    if not namelist.is_file():
        raise RuntimeError(f"SCM case configuration is missing for runtime overrides: {namelist}")
    content = namelist.read_text()
    applied = {}
    for key, raw in re.findall(r"(?m)^\s+(do_spinup|spinup_timesteps):\s*([^\s#]+)", section.group(1)):
        if key == "do_spinup":
            value = ".true." if raw.lower() in {"true", ".true."} else ".false." if raw.lower() in {"false", ".false."} else None
        else:
            value = raw if raw.isdigit() else None
        if value is None:
            raise RuntimeError(f"Invalid {key} runtime override in case manifest: {raw}")
        pattern = rf"(?im)^(\s*{key}\s*=\s*)[^,\n]+"
        content, count = re.subn(pattern, rf"\g<1>{value}", content, count=1)
        if count != 1:
            raise RuntimeError(f"SCM namelist does not define required override key {key}: {namelist}")
        applied[key] = raw
    if applied:
        namelist.write_text(content)
    return {"path": str(namelist), "values": applied, "sha256": digest(namelist)}


def stage_gocart_climatology(source: Path, work: Path, expected_scm_revision: str) -> dict[str, object]:
    """Link and verify the large GFS v17 GOCART climatology from the passed baseline tree."""
    baseline = work / "baseline-source"
    baseline_revision = subprocess.check_output(
        ["git", "-C", str(baseline), "rev-parse", "HEAD"], text=True
    ).strip()
    if baseline_revision != expected_scm_revision:
        raise RuntimeError(
            f"Baseline SCM checkout mismatch: expected {expected_scm_revision}, got {baseline_revision}"
        )
    checksum_file = baseline / "data-input-checksums.json"
    if not checksum_file.is_file():
        raise RuntimeError(f"Baseline SCM data checksum manifest is missing: {checksum_file}")
    recorded = json.loads(checksum_file.read_text())
    hashes: dict[str, str] = {}
    for relative_name in GOCART_CLIMATOLOGY_FILES:
        relative_path = Path(relative_name)
        original = baseline / relative_path
        # data-input-checksums.json is rooted at scm/data, while the lock
        # entries used here are rooted at the SCM checkout.  Translate the
        # checkout-relative path before looking up its staged digest.
        manifest_name = relative_name.removeprefix("scm/data/")
        expected_hash = recorded.get(manifest_name)
        if not original.is_file() or not expected_hash:
            raise RuntimeError(f"Required GOCART climatology input is absent or untracked: {original}")
        actual_hash = digest(original)
        if actual_hash != expected_hash:
            raise RuntimeError(f"GOCART climatology checksum mismatch: {original}")

        destination = source / relative_path
        destination.parent.mkdir(parents=True, exist_ok=True)
        if destination.is_symlink():
            if destination.resolve() != original.resolve():
                raise RuntimeError(f"Conflicting staged GOCART input link: {destination}")
        elif destination.exists():
            if not destination.is_file() or digest(destination) != expected_hash:
                raise RuntimeError(f"Conflicting staged GOCART input: {destination}")
        else:
            destination.symlink_to(os.path.relpath(original, destination.parent))
        hashes[relative_name] = actual_hash
    return {"source": str(baseline), "scm_revision": baseline_revision, "files": hashes}


def run_case(args: argparse.Namespace) -> None:
    """Build and execute an individual or explicitly ordered coupled SCM case."""
    selected_processes = ([item.strip() for item in args.processes.split(",") if item.strip()]
                          if args.processes else list(PROCESS_GROUPS.get(args.case_group, (args.process,))))
    # The individual GasChem suite has a mandatory Photolysis producer. Keep
    # that real suite execution in the expected invocation ledger even when
    # the caller selected only GasChem as its acceptance target.
    if not args.case_group and args.process == "gaschem" and "photolysis" not in selected_processes:
        selected_processes.insert(0, "photolysis")
    if not selected_processes or any(item not in PROCESSES for item in selected_processes):
        raise ValueError("--processes must name supported CATChem processes")
    if args.process not in selected_processes:
        raise ValueError("--process must be one of the selected --processes")
    group_case = args.case_group is not None
    if group_case and tuple(selected_processes) != PROCESS_GROUPS.get(args.case_group):
        raise ValueError(f"--processes do not match the required order for {args.case_group}")
    work = Path(args.work_dir).resolve()
    try:
        work.relative_to(ROOT)
    except ValueError:
        if args.docker_image:
            raise RuntimeError("Docker case work directories must be inside the CATChem repository mount")
    source = work / "overlay-source"
    if not (source / "CMakeLists.txt").is_file():
        raise RuntimeError(f"Prepared overlay is missing at {source}; run util/ccpp_scm.py prepare first")
    lock = json.loads(LOCK.read_text())
    build_source_sha256 = catchem_build_source_digest()
    case_manifest = (ROOT / "tests/ccpp-scm/cases" /
                     (f"{args.case_group}.yml" if group_case else f"{args.process}.yml"))
    process_suite_name = {
        "dust": "Dust", "seasalt": "SeaSalt", "drydep": "DryDep", "wetdep": "WetDep",
        "settling": "Settling", "so4chem": "SO4Chem", "carbchem": "CarbChem",
        "photolysis": "Photolysis", "gaschem": "PhotolysisGasChem",
    }[args.process]
    if args.suite != "SCM_GFS_v17_p8_ugwpv1":
        suite_name = args.suite.removeprefix("CATChem_")
    elif group_case:
        suite_name = {"emission-chemistry-removal": "EmissionChemistryRemoval",
                      "photolysis-gaschem": "PhotolysisGasChem"}[args.case_group]
    else:
        suite_name = process_suite_name
    suite_xml = ROOT / "tests/ccpp/suites" / f"suite_CATChem_{suite_name}.xml"
    if not case_manifest.is_file():
        raise RuntimeError(f"No CATChem acceptance case manifest exists: {case_manifest}")
    if not suite_xml.is_file():
        raise RuntimeError(f"No CATChem CCPP suite exists for {args.process}: {suite_xml}")
    suite_source = suite_xml.read_text()
    missing_drivers = [item for item in selected_processes if f"catchem_{item}" not in suite_source]
    if missing_drivers:
        raise RuntimeError(f"Suite {suite_xml} does not select required CATChem drivers: {missing_drivers}")
    if args.subcycle_count != 1:
        raise RuntimeError(
            "The one-process SCM harness cannot safely repeat a process inside its full physics "
            "subcycle. Use a dedicated coupled scheduling suite with an explicit loop extent."
        )
    patch_root = ROOT / "tests/ccpp-scm/patches"
    overlay_hash = hashlib.sha256()
    for patch_file in sorted(path for path in patch_root.rglob("*") if path.is_file()):
        overlay_hash.update(patch_file.relative_to(patch_root).as_posix().encode())
        overlay_hash.update(b"\0")
        overlay_hash.update(patch_file.read_bytes())
    if lock.get("overlay_sha256") != overlay_hash.hexdigest():
        raise RuntimeError("Tracked CCPP-SCM overlay patch hash does not match source-lock.json")
    revisions: dict[str, str] = {}
    checkout_paths = {
        "catchem": work / "_source-cache" / "catchem",
        "scm": source,
        "framework": source / "ccpp/framework",
        "physics": source / "ccpp/physics",
        "gocart": work / "_source-cache" / "catchem/src/external/GOCART",
        "musica": work / "_source-cache" / "catchem/src/external/musica",
    }
    for component, record in lock["components"].items():
        checkout = checkout_paths[component]
        if not (checkout / ".git").exists():
            raise RuntimeError(f"Prepared {component} checkout is missing: {checkout}")
        actual = subprocess.check_output(["git", "-C", str(checkout), "rev-parse", "HEAD"], text=True).strip()
        if actual != record["revision"]:
            raise RuntimeError(f"{component} checkout mismatch: expected {record['revision']}, got {actual}")
        revisions[component] = actual

    gocart_climatology = stage_gocart_climatology(
        source, work, lock["components"]["scm"]["revision"]
    )

    suite_names = (f"CATChem_{suite_name}", f"CATChem_{suite_name}_ps")
    build_suite_names = tuple(item.strip() for item in args.suite_set.split(",") if item.strip()) \
        if args.suite_set else suite_names
    build_processes = tuple(item.strip() for item in args.process_set.split(",") if item.strip()) \
        if args.process_set else tuple(selected_processes)
    for variant in set((*suite_names, *build_suite_names)):
        source_suite = ROOT / "tests/ccpp/suites" / f"suite_{variant}.xml"
        target_suite = source / "ccpp/suites" / source_suite.name
        target_suite.write_text(source_suite.read_text())
    missing_suites = [name for name in set((*suite_names, *build_suite_names))
                      if not (source / "ccpp/suites" / f"suite_{name}.xml").is_file()]
    if missing_suites:
        raise RuntimeError("Selected CCPP suite variants are absent from the locked SCM source: " +
                           ", ".join(missing_suites))
    staged_case_input: dict[str, object] | None = None
    if args.case == "arm_sgp_summer_1997_A_night":
        night_stage_log = work / "evidence/stage-arm-night-case.log"
        run_step("stage ARM nighttime case", [sys.executable,
                 str(ROOT / "tests/ccpp-scm/stage_arm_night_case.py"), "--source", str(source)],
                 ROOT, night_stage_log, os.environ.copy(), args.docker_image)
        staged_case_input = json.loads(night_stage_log.read_text())
    if args.case == "arm_sgp_summer_1997_A_leap":
        run_step("stage leap-day case", [sys.executable,
                 str(ROOT / "tests/ccpp-scm/stage_time_recovery_case.py"), "--source", str(source)],
                 ROOT, work / "evidence/stage-time-recovery-case.log", os.environ.copy(), args.docker_image)
    case_input = source / "scm/data/processed_case_input" / f"{args.case}_SCM_driver.nc"
    if not case_input.is_file():
        raise RuntimeError(f"Required case input is missing: {case_input}")
    manifest_text = case_manifest.read_text()
    manifest_data = yaml.safe_load(manifest_text)
    case_runtime_overrides = apply_case_runtime_overrides(source, args.case, manifest_text)
    scm_case_override = (configure_land_prescribed_flux_case(source, args.case)
                         if args.case.startswith("arm_sgp_") and any(item in {
                             "dust", "drydep", "so4chem", "photolysis", "gaschem"
                         } for item in selected_processes)
                         else None)

    physics_namelist = "input_GFS_v17_p8_ugwpv1.nml"
    physics_namelist_inputs = [physics_namelist]
    if group_case or any(item in {"wetdep", "so4chem", "carbchem", "drydep", "photolysis", "gaschem"}
                         for item in selected_processes):
        physics_dir = source / "ccpp/physics_namelists"
        namelist_tag = ({"emission-chemistry-removal": "EmissionChemistryRemoval",
                         "photolysis-gaschem": "PhotolysisGasChem"}[args.case_group]
                        if group_case else
                        {"wetdep": "WetDep", "so4chem": "SO4Chem", "carbchem": "CarbChem",
                         "drydep": "DryDep", "photolysis": "PhotolysisGasChem",
                         "gaschem": "PhotolysisGasChem"}[args.process])
        for suffix in ("", "_ps"):
            base_namelist = physics_dir / f"input_GFS_v17_p8_ugwpv1{suffix}.nml"
            process_namelist = physics_dir / f"input_CATChem_{namelist_tag}{suffix}.nml"
            # Each process invocation starts from the locked stock namelist.
            # A previous run leaves its cplchm=true copy in overlay-source;
            # rereading that mutable file made subsequent runs non-idempotent.
            stock_namelist = work / "baseline-source" / "ccpp/physics_namelists" / base_namelist.name
            if not stock_namelist.is_file():
                raise RuntimeError(f"Locked stock physics namelist is missing: {stock_namelist}")
            content = stock_namelist.read_text()
            base_namelist.write_text(content)
            if "cplchm       = .false." not in content.lower():
                raise RuntimeError(f"Locked GFS v17 namelist changed its cplchm default: {base_namelist}")
            content = content.replace("cplchm       = .false.", "cplchm       = .true.", 1)
            if args.process in {"wetdep", "so4chem"}:
                content = content.replace("lheatstrg    = .true.", "lheatstrg    = .false.", 1)
            process_namelist.write_text(content)
            physics_namelist_inputs.append(process_namelist.name)
        physics_namelist = physics_namelist_inputs[1]

    def ensure_patch(
        patch_path: Path,
        name: str,
        markers: dict[str, str],
        *,
        recount: bool = True,
        unidiff_zero: bool = False,
    ) -> None:
        recount_args = ["--recount"] if recount else []
        context_args = ["--unidiff-zero"] if unidiff_zero else []
        reverse = subprocess.run(
            ["git", "-C", str(source), "apply", *recount_args, *context_args, "--reverse", "--check", str(patch_path)],
            stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True, check=False,
        )
        if reverse.returncode == 0:
            return
        apply = subprocess.run(
            ["git", "-C", str(source), "apply", *recount_args, *context_args, "--check", str(patch_path)],
            stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True, check=False,
        )
        if apply.returncode == 0:
            subprocess.run(["git", "-C", str(source), "apply", *recount_args, *context_args, str(patch_path)], check=True)
            return
        missing = [relative for relative, marker in markers.items()
                   if not (source / relative).is_file() or marker not in (source / relative).read_text(errors="replace")]
        if missing:
            raise RuntimeError(f"{name} overlay cannot be applied or verified; missing markers in {missing}: " +
                               (apply.stdout or reverse.stdout))

    ensure_patch(
        ROOT / "tests/ccpp-scm/patches/0002-catchem-target.patch",
        "CATChem CMake",
        {
            "CMakeLists.txt": "CATCHEM_CCPP_PROCESSES",
            "ccpp/CMakeLists.txt": "catchem_ccpp_lifecycle.meta",
            "scm/src/scm_type_defs.F90": "catchem_context_id",
            "scm/src/scm_type_defs.meta": "catchem_interval_token",
            "scm/src/scm.F90": "catchem_context_id",
            "scm/src/scm_time_integration.F90": "catchem_process_subcycle",
        },
        unidiff_zero=True,
    )
    ensure_patch(
        ROOT / "tests/ccpp-scm/patches/0003-catchem-scheduling.patch",
        "CATChem scheduling",
        {"scm/src/scm_time_integration.F90": "catchem_process_subcycle = 2"},
    )
    ensure_patch(
        ROOT / "tests/ccpp-scm/patches/0004-catchem-scm-cases.patch",
        "CATChem SCM suite defaults",
        {"scm/src/suite_info.py": "_catchem_suite_defaults"},
    )
    ensure_patch(
        ROOT / "tests/ccpp-scm/patches/0004-prescribed-surface-lai.patch",
        "GFS prescribed-surface LAI climatology",
        {"ccpp/physics/physics/Interstitials/UFS_SCM_NEPTUNE/GFS_phys_time_vary.scm.F90":
         "same monthly NoahMP LAI climatology"},
        recount=False,
    )

    subcycle_suffix = f"-subcycle{args.subcycle_count}" if args.subcycle_count != 1 else ""
    case_id = f"{args.case_group or args.process}-{args.case}-{suite_names[0]}{subcycle_suffix}"
    case_root = work / "cases" / case_id
    build = Path(args.shared_build_dir).resolve() if args.shared_build_dir else case_root / "build"
    output = case_root / "output"
    logs = case_root / "logs"
    if case_root.exists() and not args.keep_existing:
        import shutil
        shutil.rmtree(case_root)
    case_root.mkdir(parents=True, exist_ok=True)
    output.mkdir(parents=True, exist_ok=True)
    if not args.build_only:
        evidence_path = case_root / 'evidence.json'
        if evidence_path.is_file():
            import shutil
            shutil.copy2(evidence_path, case_root / 'evidence.previous.json')
        evidence_path.write_text(json.dumps({
            'schema_version': 1, 'status': 'running', 'case': args.case,
            'suite': suite_names[0], 'started_utc': datetime.now(timezone.utc).isoformat(),
            'reason': 'A fresh required run must complete before prior evidence can be accepted.',
        }, indent=2) + '\n')
    if any(item in {"photolysis", "gaschem"} for item in selected_processes):
        tuvx_data = ROOT / "src/external/musica/configs/tuvx/data"
        runtime_data = output / "data"
        relative_data = os.path.relpath(tuvx_data, output)
        if runtime_data.is_symlink():
            if runtime_data.resolve() != tuvx_data.resolve():
                raise RuntimeError(f"Conflicting TUV-x data link: {runtime_data}")
        elif runtime_data.exists():
            raise RuntimeError(f"Conflicting TUV-x runtime data path: {runtime_data}")
        else:
            runtime_data.symlink_to(relative_data, target_is_directory=True)
    env = os.environ.copy()
    env["CATCHEM_CCPP_PROCESS"] = args.process
    env["CATCHEM_CCPP_REGISTRY_ORDER"] = args.registry_order
    if args.inject_process_failure:
        if args.process != "gaschem" or args.docker_image is None:
            raise RuntimeError("The real-cap process failure probe requires GasChem and --docker-image")
        if args.runtime != args.timestep or args.subcycle_count != 1:
            raise RuntimeError("The real-cap process failure probe requires exactly one unsplit host interval")
        env["CATCHEM_ACCEPTANCE_FAIL_AFTER_PROCESS"] = "gaschem"
    seed_match = re.search(r"^\s+aerosol_mixing_ratio_kg_kg:\s*([0-9.eE+-]+)\s*$", manifest_text, re.MULTILINE)
    if seed_match:
        seed_value = float(seed_match.group(1))
        if not (seed_value > 0.0):
            raise RuntimeError("Acceptance aerosol mixing ratio must be positive.")
        env["CATCHEM_ACCEPTANCE_AEROSOL_MIXING_RATIO"] = seed_match.group(1)
    soil_seed_match = re.search(r"^\s+soil_moisture_m3_m3:\s*([0-9.eE+-]+)\s*$", manifest_text, re.MULTILINE)
    if "dust" in selected_processes and soil_seed_match:
        soil_seed = float(soil_seed_match.group(1))
        if not (0.0 < soil_seed <= 1.0):
            raise RuntimeError("Acceptance soil moisture must be in (0, 1] m3/m3.")
        env["CATCHEM_ACCEPTANCE_SOIL_MOISTURE"] = soil_seed_match.group(1)
    gas_seeds: dict[str, object] = {}
    if "so4chem" in selected_processes:
        for manifest_key, env_name in (
            ("sulfur_dioxide_mole_fraction_ppmv", "CATCHEM_ACCEPTANCE_SO2_PPMV"),
            ("dimethyl_sulfide_mole_fraction_ppmv", "CATCHEM_ACCEPTANCE_DMS_PPMV"),
            ("hydrogen_peroxide_mole_fraction_ppmv", "CATCHEM_ACCEPTANCE_H2O2_PPMV"),
            ("hydroxyl_radical_mole_fraction_ppmv", "CATCHEM_ACCEPTANCE_OH_PPMV"),
        ):
            match = re.search(rf"^\s+{manifest_key}:\s*([0-9.eE+-]+)\s*$", manifest_text, re.MULTILINE)
            if not match or float(match.group(1)) <= 0.0:
                raise RuntimeError(f"SO4Chem acceptance fixture must declare a positive {manifest_key}.")
            env[env_name] = match.group(1)
            gas_seeds[manifest_key] = match.group(1)
        optional_no3 = re.search(
            r"^\s+nitrate_radical_mole_fraction_ppmv:\s*([0-9.eE+-]+)\s*$", manifest_text, re.MULTILINE
        )
        if optional_no3:
            if float(optional_no3.group(1)) <= 0.0:
                raise RuntimeError("SO4Chem nitrate radical seed must be positive when declared.")
            env["CATCHEM_ACCEPTANCE_NO3_PPMV"] = optional_no3.group(1)
            gas_seeds["nitrate_radical_mole_fraction_ppmv"] = optional_no3.group(1)
    if any(item in {"photolysis", "gaschem"} for item in selected_processes):
        ozone_match = re.search(r"^\s+ozone_mole_fraction_ppmv:\s*([0-9.eE+-]+)\s*$", manifest_text, re.MULTILINE)
        if ozone_match:
            if float(ozone_match.group(1)) <= 0.0:
                raise RuntimeError(f"{args.process} acceptance fixture ozone_mole_fraction_ppmv must be positive.")
            env["CATCHEM_ACCEPTANCE_O3_PPMV"] = ozone_match.group(1)
            gas_seeds["ozone_mole_fraction_ppmv"] = ozone_match.group(1)
        else:
            # The SCM case has already initialized its vertical ozone profile and
            # extended it with the selected mid-latitude summer standard
            # atmosphere. Preserve it in
            # the full CCPP constituent array instead of overriding it uniformly.
            env.pop("CATCHEM_ACCEPTANCE_O3_PPMV", None)
            env["CATCHEM_ACCEPTANCE_O3_USE_SCM_PROFILE"] = "1"
            gas_seeds["ozone_profile_source"] = "SCM case tracer plus mid-latitude summer standard-atmosphere extension"
    wetdep_rain_match = re.search(
        r"^\s+large_scale_precipitation_amount_m:\s*([0-9.eE+-]+)\s*$", manifest_text, re.MULTILINE
    )
    wetdep_flux_match = re.search(
        r"^\s+large_scale_precipitation_flux_kg_m2_s:\s*([0-9.eE+-]+)\s*$", manifest_text, re.MULTILINE
    )
    if "wetdep" in selected_processes:
        if bool(wetdep_rain_match) != bool(wetdep_flux_match):
            raise RuntimeError("WetDep acceptance fixture must declare both rain amount and precipitation flux.")
        if wetdep_rain_match and wetdep_flux_match:
            wetdep_rain = float(wetdep_rain_match.group(1))
            wetdep_flux = float(wetdep_flux_match.group(1))
            if wetdep_rain <= 0.0 or wetdep_flux <= 0.0:
                raise RuntimeError("WetDep acceptance precipitation amount and flux must be positive.")
            env["CATCHEM_ACCEPTANCE_WETDEP_RAIN_M"] = wetdep_rain_match.group(1)
            env["CATCHEM_ACCEPTANCE_WETDEP_FLUX_KG_M2_S"] = wetdep_flux_match.group(1)
    default_config = ("emission-chemistry-removal.yml" if args.case_group == "emission-chemistry-removal"
                      else "gaschem.yml" if args.case_group == "photolysis-gaschem"
                      else f"{args.process}.yml")
    catchem_config = Path(args.catchem_config or env.get("CATCHEM_CONFIG") or
                          ROOT / f"tests/ccpp-scm/cases/configs/{default_config}").resolve()
    if not catchem_config.is_file():
        raise RuntimeError(f"CATChem species configuration is missing: {catchem_config}")
    env["CATCHEM_CONFIG"] = str(catchem_config)
    if args.emit_boundary_ledger:
        env["CATCHEM_BOUNDARY_LEDGER"] = "1"
    else:
        env.pop("CATCHEM_BOUNDARY_LEDGER", None)
    if args.emit_met_ledger:
        env["CATCHEM_MET_LEDGER"] = "1"
    else:
        env.pop("CATCHEM_MET_LEDGER", None)
    catchem_processes = list(build_processes)
    if "gaschem" in catchem_processes and "photolysis" not in catchem_processes:
        catchem_processes.insert(0, "photolysis")
    configure = ["cmake", "-S", str(source), "-B", str(build),
                 f"-DCCPP_SUITES={','.join(build_suite_names)}", f"-DCMAKE_BUILD_TYPE={args.build_type}",
                 f"-DCATCHEM_CHECKOUT_DIR={ROOT}",
                 f"-DCATCHEM_CCPP_PROCESSES={';'.join(catchem_processes)}",
                 "-DCATCHEM_CCPP_TEST_STUBS=OFF",
                 f"-DCATCHEM_ENABLE_ACCEPTANCE_FAILURE_INJECTION={'ON' if args.inject_process_failure else 'OFF'}"]
    if args.science_profile == "all" or any(item in {"photolysis", "gaschem"} for item in catchem_processes):
        configure.extend(["-DCATCHEM_BUILD_MUSICA=ON", "-DCATCHEM_ENABLE_KOKKOS=ON"])
    marker = build / "CATCHEM_CCPP_CAP_READY"
    reusable_build = False
    build_evidence_path = (build / "CATCHEM_CCPP_BUILD_EVIDENCE.json"
                           if args.shared_build_dir else case_root / "build-evidence.json")
    cache_path = build / "CMakeCache.txt"
    if args.keep_existing and marker.is_file() and cache_path.is_file() and build_evidence_path.is_file():
        previous = json.loads(build_evidence_path.read_text())
        cache = cache_path.read_text(errors="replace")
        reusable_build = reusable_cap_matches(
            previous, cache, process=args.process, case=args.case, suites=list(build_suite_names),
            docker_image=args.docker_image, overlay_sha256=lock.get("overlay_sha256"),
            science_profile=args.science_profile, processes=catchem_processes,
            source_sha256=build_source_sha256,
            failure_injection=args.inject_process_failure)
    if not reusable_build:
        try:
            run_step("configure", configure, ROOT, logs / "configure.log", env, args.docker_image)
        except RuntimeError:
            configure_log = logs / "configure.log"
            configure_text = configure_log.read_text(errors="replace") if configure_log.is_file() else ""
            stale_dependency_target = "Objects of target \"tuvx_object\" referenced but no such target exists"
            if stale_dependency_target not in configure_text:
                raise
            # FetchContent can retain the TUV-x source/build tree while a
            # reconfigure loses its generated object target. Clear only CMake's
            # cache/generator state, preserve downloaded sources, and retry once.
            fresh_configure = [configure[0], "--fresh", *configure[1:]]
            run_step("fresh configure", fresh_configure, ROOT, logs / "configure-fresh.log",
                     env, args.docker_image)
        if not marker.is_file():
            raise RuntimeError("SCM configure did not generate the selected CATChem process cap")
        remove_shadowed_catchem_modules(build)
        try:
            run_step("build", ["cmake", "--build", str(build), "--parallel", str(args.jobs)],
                     ROOT, logs / "build.log", env, args.docker_image)
        except RuntimeError:
            build_log = logs / "build.log"
            log_text = build_log.read_text(errors="replace") if build_log.is_file() else ""
            stale_module_marker = "Mismatch in components of derived type"
            if stale_module_marker not in log_text:
                raise
            # GNU Fortran does not always rebuild dependents when an existing .mod
            # file changes layout across process suites. Reconfigure from scratch
            # only for that specific stale-module diagnostic.
            import shutil
            shutil.rmtree(build)
            run_step("configure", configure, ROOT, logs / "configure.log", env, args.docker_image)
            if not marker.is_file():
                raise RuntimeError("Clean SCM configure did not generate the selected CATChem process cap")
            run_step("build", ["cmake", "--build", str(build), "--parallel", str(args.jobs)],
                     ROOT, logs / "build.log", env, args.docker_image)
    generated_caps = list(build.rglob("*cap.F90"))
    generated_routines = [f"catchem_{item}_driver_run" for item in catchem_processes]
    generated_calls = [path for path in generated_caps
                       if any(routine in path.read_text(errors="replace") for routine in generated_routines)]
    missing_routines = [routine for routine in generated_routines
                        if not any(routine in path.read_text(errors="replace") for path in generated_calls)]
    if missing_routines:
        raise RuntimeError(f"Generated caps lack required process run calls: {missing_routines}")
    if args.build_only:
        build_evidence = {
            "schema_version": 1,
            "status": "passed",
            "mode": "real_cap_build_only",
            "process": args.process,
            "processes": catchem_processes,
            "case": "coupled" if args.shared_build_dir else args.case,
            "suites": list(build_suite_names),
            "compiled_suite_variants": list(build_suite_names),
            "runtime_suite_variants": list(suite_names),
            "generated_process_caps": [str(path) for path in generated_calls],
            "build_log": str(logs / "build.log"),
            "container_image": args.docker_image,
            "overlay_sha256": lock.get("overlay_sha256"),
            "source_sha256": build_source_sha256,
        }
        evidence_path = build_evidence_path
        evidence_path.write_text(json.dumps(build_evidence, indent=2) + "\n")
        print(json.dumps(build_evidence, indent=2))
        return
    run_step("run", [sys.executable, str(source / "scm/src/run_scm.py"),
                      "--verbose",
                      "--case", args.case, "--suite", suite_names[0],
                      "--namelist", physics_namelist,
                      "--tracers", "tracers_GFS_v17_p8_ugwpv1.txt",
                      "--bin_dir", str(build), "--run_dir", str(output),
                      "--case_data_dir", "scm/data/processed_case_input",
                      "--runtime", str(args.runtime), "--timestep", str(args.timestep)],
             source, logs / "run.log", env, args.docker_image,
             expected_failure=args.inject_process_failure)
    run_text = (logs / "run.log").read_text(errors="replace")
    if args.inject_process_failure:
        marker = "CATCHEM_ACCEPTANCE_PROCESS_FAILURE_INJECTED process=gaschem stage=after_state_mutation"
        if marker not in run_text:
            raise RuntimeError("The real cap failed without reaching the injected GasChem process-body marker")
        if "CATCHEM_INVOCATION process=photolysis " not in run_text:
            raise RuntimeError("Photolysis did not complete before the GasChem body failure")
        if "CATCHEM_INVOCATION process=gaschem " in run_text or "CATCHEM_HOST_UPDATE " in run_text:
            raise RuntimeError("A process or host update was published after the injected GasChem failure")
        failure_evidence = {
            "schema_version": 1,
            "status": "passed_real_cap_process_body_failure_not_published",
            "process": "gaschem",
            "failure_stage": "after_solver_mutated_CATChem_staging_state_before_CCPP_publication",
            "suite": suite_names[0],
            "case": args.case,
            "failure_marker": marker,
            "photolysis_completed": True,
            "gaschem_success_invocations": 0,
            "host_updates": 0,
            "run_log": str(logs / "run.log"),
            "build_log": str(logs / "build.log"),
            "container_image": args.docker_image,
            "limitation": "The SCM exits after the real-cap error; retry/reimport recovery remains covered by Core tests, not by retrying within this SCM run."
        }
        evidence_path = case_root / "evidence" / "process-body-failure.json"
        evidence_path.parent.mkdir(parents=True, exist_ok=True)
        evidence_path.write_text(json.dumps(failure_evidence, indent=2) + "\n")
        print(json.dumps(failure_evidence, indent=2))
        return
    products = sorted(str(path) for path in output.rglob("*.nc") if path.is_file())
    if not products:
        raise RuntimeError(f"SCM run completed without a NetCDF output under {output}")
    unrelated_seed = re.findall(
        r"CATCHEM_UNRELATED_SEEDED slot=(\d+) VALUE=\s*([0-9.eEdD+-]+)", run_text
    )
    unrelated_preserved = re.findall(
        r"CATCHEM_UNRELATED_PRESERVED slot=(\d+) VALUE=\s*([0-9.eEdD+-]+)", run_text
    )
    if len(unrelated_seed) != 1 or len(unrelated_preserved) != 1 or unrelated_seed != unrelated_preserved:
        raise RuntimeError(
            "SCM run did not prove bitwise preservation of the unrelated nonadvected CCPP host constituent"
        )
    tendency_seeded = re.findall(
        r"CATCHEM_UNRELATED_TENDENCY_SEEDED slot=(\d+) VALUE=\s*([0-9.eEdD+-]+)", run_text
    )
    tendency_preserved = re.findall(
        r"CATCHEM_UNRELATED_TENDENCY_PRESERVED slot=(\d+) VALUE=\s*([0-9.eEdD+-]+)", run_text
    )
    if (not tendency_seeded or len(tendency_seeded) != len(tendency_preserved)
            or tendency_seeded != tendency_preserved):
        raise RuntimeError(
            "SCM run did not prove bitwise preservation of a preexisting unrelated CCPP constituent tendency "
            "at every host interval"
        )
    number_pattern = r"[+-]?(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)(?:(?:[EeDd])?[+-][0-9]+)?"
    process_loop_extents: dict[str, int] = {}
    suite_invocation_order: list[str] = []
    suite_root = ET.parse(suite_xml).getroot()
    for process_name in selected_processes:
        process_loop_extents[process_name] = 0
        for group in suite_root.findall("group"):
            for subcycle_element in group.findall("subcycle"):
                scheme_names = [node.text.strip() for node in subcycle_element.findall("scheme") if node.text]
                if f"catchem_{process_name}_driver" in scheme_names:
                    process_loop_extents[process_name] += int(subcycle_element.attrib.get("loop", "1"))
        if process_loop_extents[process_name] <= 0:
            raise RuntimeError(f"Suite does not execute selected process {process_name}")
    # Preserve CCPP's actual scheme order inside repeated subcycles. Expanding
    # each driver's loop count contiguously misorders suites such as the
    # Photolysis/GasChem recovery case, whose loop is [photo, gas] twice.
    for group in suite_root.findall("group"):
        for subcycle_element in group.findall("subcycle"):
            schemes = [node.text.strip() for node in subcycle_element.findall("scheme") if node.text]
            loop_extent = int(subcycle_element.attrib.get("loop", "1"))
            for _ in range(loop_extent):
                for scheme in schemes:
                    for process_name in selected_processes:
                        if scheme == f"catchem_{process_name}_driver":
                            suite_invocation_order.append(process_name)
    # SCM may include spinup intervals that are additional to runtime/timestep.
    # Count the actual host time steps emitted by the real case instead of
    # undercounting those intervals from the requested integration duration.
    host_interval_count = 1 + len(re.findall(r"(?m)^\s*itt\s*=", run_text))
    execution_counts = {name: host_interval_count * args.subcycle_count * extent
                        for name, extent in process_loop_extents.items()}
    execution_count = max(execution_counts.values())
    host_state_increments = [fortran_float(value) for value in re.findall(
        rf"CATCHEM_HOST_STATE_APPLIED MAXABS_INCREMENT=\s*({number_pattern})", run_text
    )]
    expected_increment_count = sum(count for name, count in execution_counts.items() if name != "photolysis")
    if len(host_state_increments) != expected_increment_count or any(
        not math.isfinite(value) or value < 0.0 for value in host_state_increments
    ) or (not group_case and args.process == "photolysis" and host_state_increments) or (
        (group_case or args.process != "photolysis") and
        not any(value > 0.0 for value in host_state_increments)
    ):
        raise RuntimeError(
            f"SCM run must apply a finite host increment for every state-updating call and at least one "
            f"nonzero increment; got {host_state_increments}"
        )
    expected_marker = f"CATCHEM_SELECTED_PROCESS={args.process} EXECUTIONS=1"
    reported_markers = [line.strip() for line in run_text.splitlines()
                         if line.strip().startswith("CATCHEM_SELECTED_PROCESS=")]
    expected_invocation_processes = [
        process_name
        for _ in range(host_interval_count * args.subcycle_count)
        for process_name in suite_invocation_order
    ]
    expected_markers = [f"CATCHEM_SELECTED_PROCESS={process} EXECUTIONS=1"
                        for process in expected_invocation_processes]
    if reported_markers != expected_markers:
        raise RuntimeError(
            f"SCM run must report the selected process and its required producers in order; "
            f"expected {expected_markers}, found {reported_markers}"
        )
    invocation_ledger = [
        {"process": process, "interval_token": int(interval), "subcycle": int(subcycle),
         "duration_seconds": fortran_float(duration)}
        for process, interval, subcycle, duration in re.findall(
            rf"CATCHEM_INVOCATION process=([^\s]+) interval=(\d+) subcycle=(\d+) duration_seconds=\s*({number_pattern})",
            run_text,
        )
    ]
    if [item["process"] for item in invocation_ledger] != expected_invocation_processes or any(
        not math.isfinite(item["duration_seconds"]) or
        item["duration_seconds"] <= 0.0 for item in invocation_ledger
    ):
        raise RuntimeError(
            f"SCM run must report one valid interval/subcycle/duration row per selected call; got {invocation_ledger}"
        )
    host_update_ledger = [
        {"interval_token": int(interval), "contribution_token": int(contribution),
         "subcycle": int(subcycle), "mapped_species": int(species),
         "duration_seconds": fortran_float(duration)}
        for interval, contribution, subcycle, species, duration in re.findall(
            rf"CATCHEM_HOST_UPDATE interval=(\d+) contribution_token=(\d+) subcycle=(\d+) "
            rf"mapped_species=(\d+) duration_seconds=\s*({number_pattern})",
            run_text,
        )
    ]
    expected_updates = sum(count for name, count in execution_counts.items() if name != "photolysis")
    if len(host_update_ledger) != expected_updates or any(
        item["interval_token"] != item["contribution_token"] or item["subcycle"] <= 0 or
        item["mapped_species"] <= 0 or not math.isfinite(item["duration_seconds"]) or
        item["duration_seconds"] <= 0.0
        for item in host_update_ledger
    ):
        raise RuntimeError(
            f"SCM run must report one successful tendency-consumption row per expected update; got {host_update_ledger}"
        )
    boundary_state_ledger: list[dict[str, object]] = []
    full_constituent_state_ledger: list[dict[str, object]] = []
    if args.emit_boundary_ledger and expected_updates > 0:
        boundary_state_ledger = [
            {"interval_token": int(interval), "subcycle": int(subcycle), "species": species,
             "column": int(column), "level": int(level), "before": fortran_float(before),
             "after": fortran_float(after), "units": units}
            for interval, subcycle, species, column, level, before, after, units in re.findall(
                rf"CATCHEM_BOUNDARY_VALUE interval=(\d+) subcycle=(\d+) species=([^\s]+) "
                rf"column=(\d+) level=(\d+) before=\s*({number_pattern}) after=\s*({number_pattern}) units=([^\n]*)",
                run_text,
            )
        ]
        if not boundary_state_ledger:
            raise RuntimeError("Boundary ledger was requested but the real host interstitial emitted no mapped states")
        if "CATCHEM_FULL_STATE_METADATA_ERROR" in run_text:
            raise RuntimeError("Full CCPP constituent output could not resolve complete framework metadata")
        full_constituent_state_ledger = [
            {"update_sequence": int(update_sequence), "interval_token": int(interval),
             "subcycle": int(subcycle), "standard_name": standard_name,
             "diagnostic_name": diagnostic_name, "column": int(column), "level": int(level),
             "before": fortran_float(before), "after": fortran_float(after), "units": units}
            for update_sequence, interval, subcycle, standard_name, diagnostic_name, column, level, before, after, units in re.findall(
                rf"CATCHEM_FULL_STATE_VALUE update_sequence=(\d+) interval=(\d+) subcycle=(\d+) "
                rf"standard_name=([^\s]+) "
                rf"diagnostic_name=([^\s]+) column=(\d+) level=(\d+) before=\s*({number_pattern}) "
                rf"after=\s*({number_pattern}) units=([^\n]*)",
                run_text,
            )
        ]
        full_state_names = {item["diagnostic_name"] for item in full_constituent_state_ledger}
        full_state_groups = {item["update_sequence"] for item in full_constituent_state_ledger}
        expected_full_state_groups = set(range(1, len(host_update_ledger) + 1))
        full_state_keys = [(item["update_sequence"], item["diagnostic_name"],
                            item["column"], item["level"]) for item in full_constituent_state_ledger]
        rows_by_update_and_name: dict[tuple[int, str], int] = {}
        for item in full_constituent_state_ledger:
            key = (item["update_sequence"], item["diagnostic_name"])
            rows_by_update_and_name[key] = rows_by_update_and_name.get(key, 0) + 1
        snapshot_names = {
            update: {item["diagnostic_name"] for item in full_constituent_state_ledger
                     if item["update_sequence"] == update}
            for update in full_state_groups
        }
        complete_snapshots = bool(snapshot_names) and all(
            names == snapshot_names[min(snapshot_names)] for names in snapshot_names.values())
        complete_snapshots = complete_snapshots and len(set(rows_by_update_and_name.values())) == 1
        if (not full_constituent_state_ledger or len(full_state_keys) != len(set(full_state_keys)) or
                full_state_groups != expected_full_state_groups or
                not complete_snapshots or
                "catchem_acceptance_unrelated_tracer" not in full_state_names):
            raise RuntimeError(
                "Full CCPP output must include unique states for every host update and the nonadvected host constituent"
            )
        unrelated_full_state = [item for item in full_constituent_state_ledger
                                if item["diagnostic_name"] == "catchem_acceptance_unrelated_tracer"]
        if not unrelated_full_state or any(item["before"] != item["after"] for item in unrelated_full_state):
            raise RuntimeError("Full CCPP output did not preserve the nonadvected host constituent")
        if len(boundary_state_ledger) % len(host_update_ledger):
            raise RuntimeError("Mapped state rows cannot be assigned to complete host updates")
        mapped_rows_per_update = len(boundary_state_ledger) // len(host_update_ledger)
        full_state_by_key = {
            (item["update_sequence"], item["diagnostic_name"].casefold(), item["column"], item["level"]): item
            for item in full_constituent_state_ledger
        }
        for row_index, item in enumerate(boundary_state_ledger):
            key = (row_index // mapped_rows_per_update + 1, item["species"].casefold(),
                   item["column"], item["level"])
            full_item = full_state_by_key.get(key)
            if full_item is None or any(full_item[side] != item[side] for side in ("before", "after")):
                raise RuntimeError("Full CCPP snapshot disagrees with mapped state before/after publication")
    met_input_ledger: list[dict[str, object]] = []
    invalid_met_fields: list[dict[str, object]] = []
    met_ledger_path: Path | None = None
    if args.emit_met_ledger:
        current_met: dict[str, object] | None = None
        for line in run_text.splitlines():
            begin = re.fullmatch(r"CATCHEM_MET_BEGIN process=([^\s]+)", line.strip())
            field = re.fullmatch(
                rf"CATCHEM_MET_FIELD name=([^\s]+) rank=(\d+) axis=(\d+) columns=(\d+) levels=(\d+) values=(.*)",
                line.strip(),
            )
            end = re.fullmatch(
                rf"CATCHEM_MET_END process=([^\s]+) interval=(\d+) subcycle=(\d+) duration_seconds=\s*({number_pattern})",
                line.strip(),
            )
            if begin:
                if current_met is not None:
                    raise RuntimeError("A meteorology ledger block began before the previous process block ended")
                current_met = {"process": begin.group(1), "fields": {}}
            elif field:
                if current_met is None:
                    raise RuntimeError("A meteorology field was emitted outside its process block")
                values = [fortran_float(value) for value in field.group(6).split()]
                columns, levels = int(field.group(4)), int(field.group(5))
                if len(values) != columns * levels:
                    raise RuntimeError(f"Invalid meteorology field extent or value for {field.group(1)}")
                nonfinite_indexes = [index for index, value in enumerate(values) if not math.isfinite(value)]
                fill_indexes = [index for index, value in enumerate(values)
                                if math.isfinite(value) and abs(value) >= 1.0e20]
                invalid_indexes = sorted(set(nonfinite_indexes + fill_indexes))
                if invalid_indexes:
                    invalid_met_fields.append({"process": current_met["process"],
                                                 "field": field.group(1),
                                                 "invalid_value_indexes": invalid_indexes,
                                                 "nonfinite_value_indexes": nonfinite_indexes,
                                                 "fill_value_indexes": fill_indexes})
                current_met["fields"][field.group(1)] = {
                    "rank": int(field.group(2)), "axis": int(field.group(3)),
                    "columns": columns, "levels": levels,
                    "values": [value if math.isfinite(value) else None for value in values],
                    "nonfinite_value_indexes": nonfinite_indexes,
                    "fill_value_indexes": fill_indexes,
                    "invalid_value_indexes": invalid_indexes,
                }
            elif end:
                if current_met is None or current_met["process"] != end.group(1):
                    raise RuntimeError("A meteorology ledger end marker did not match its process begin marker")
                current_met.update({"interval_token": int(end.group(2)), "subcycle": int(end.group(3)),
                                    "duration_seconds": fortran_float(end.group(4))})
                if not current_met["fields"]:
                    raise RuntimeError("A successful process emitted no meteorology fields")
                met_input_ledger.append(current_met)
                current_met = None
        if current_met is not None:
            raise RuntimeError(f"Meteorology ledger block for {current_met['process']} has no successful end marker")
        if not met_input_ledger:
            raise RuntimeError("Meteorology ledger was requested but no successful process inputs were captured")
        met_ledger_path = case_root / "evidence" / "met-input-ledger.jsonl"
        met_ledger_path.parent.mkdir(parents=True, exist_ok=True)
        with met_ledger_path.open("w", encoding="utf-8") as ledger_stream:
            for record in met_input_ledger:
                ledger_stream.write(json.dumps(record, allow_nan=False) + "\n")
    diagnostic_summaries = [
        {"name": name, "max_abs": fortran_float(value), "sum": fortran_float(total)}
        for name, value, total in re.findall(
            rf"CATCHEM_DIAGNOSTIC name=([^\s]+) MAXABS=\s*({number_pattern})\s+SUM=\s*({number_pattern})", run_text
        )
    ]
    physical_signals = [
        {"name": name, "value": fortran_float(value)}
        for name, value in re.findall(
            rf"CATCHEM_SIGNAL name=([^\s]+) VALUE=\s*({number_pattern})", run_text
        )
    ]
    signal_match = re.search(r"^\s+signal_name:\s*(\S+)\s*$", manifest_text, re.MULTILINE)
    expected_signal_name = signal_match.group(1) if signal_match else None
    observed_signals: dict[str, float] = {}
    for item in physical_signals:
        observed_signals[item["name"]] = max(observed_signals.get(item["name"], 0.0), item["value"])
    observed_signals.update({item["name"]: item["max_abs"] for item in diagnostic_summaries})
    expected_signal_value = observed_signals.get(expected_signal_name) if expected_signal_name else None
    observed_process_signals = {
        process_name: observed_signals.get(f"{process_name}_constituent_max_abs_delta")
        for process_name in selected_processes
    }
    minimum_signal_match = re.search(
        r"^\s+minimum_abs_value:\s*([0-9.eE+-]+)\s*$", manifest_text, re.MULTILINE
    )
    minimum_signal_value = fortran_float(minimum_signal_match.group(1)) if minimum_signal_match else 0.0
    maximum_signal_match = re.search(
        r"^\s+maximum_abs_value:\s*([0-9.eE+-]+)\s*$", manifest_text, re.MULTILINE
    )
    maximum_signal_value = fortran_float(maximum_signal_match.group(1)) if maximum_signal_match else float("inf")
    signal_is_finite = expected_signal_value is not None and math.isfinite(expected_signal_value)
    if group_case and args.case_group == "emission-chemistry-removal":
        nonzero_match = re.search(r"(?m)^\s+expected_nonzero_processes:\s*\[([^\]]+)\]", manifest_text)
        zero_match = re.search(r"(?m)^\s+expected_zero_processes:\s*\[([^\]]+)\]", manifest_text)
        required_nonzero = [item.strip() for item in nonzero_match.group(1).split(",")] if nonzero_match else []
        expected_zero = [item.strip() for item in zero_match.group(1).split(",")] if zero_match else []
        signals_valid = (bool(required_nonzero) and bool(expected_zero) and
                         all(observed_process_signals.get(name) is not None and
                             math.isfinite(observed_process_signals[name]) and
                             observed_process_signals[name] > 0.0 for name in required_nonzero) and
                         all(observed_process_signals.get(name) == 0.0 for name in expected_zero))
        scientific_acceptance = {
            "required_nonzero_processes": required_nonzero,
            "expected_zero_processes": expected_zero,
            "observed_process_signals": observed_process_signals,
            "status": "passed" if signals_valid else "blocked",
        }
    elif group_case and args.case_group == "photolysis-gaschem":
        scientific_acceptance = {
            "observed_process_signals": observed_process_signals,
            "case_regime": "night" if args.case.endswith("_night") else "day",
            "status": "passed" if all(value is not None and math.isfinite(value)
                                       for value in observed_process_signals.values()) else "blocked",
        }
    else:
        scientific_acceptance = {
            "required_nonzero_signal": expected_signal_name,
            "minimum_abs_value": minimum_signal_value,
            "maximum_abs_value": maximum_signal_value,
            "observed_value": expected_signal_value,
            "status": "passed" if signal_is_finite and minimum_signal_value < expected_signal_value <= maximum_signal_value else "blocked",
        }
    budget_matches = list(re.finditer(
        rf"CATCHEM_BUDGET name=([a-z0-9_]+)(?: species=([a-z0-9_]+))? RESIDUAL=\s*({number_pattern})"
        rf"(?: TOTAL=\s*({number_pattern}) BIN_SUM=\s*({number_pattern}))?"
        rf"(?: MASS_LOSS=\s*({number_pattern})"
        rf" BOUNDARY_FLUX_MASS=\s*({number_pattern}))?"
        rf"(?: ABS_ERROR=\s*({number_pattern}) ABS_TOLERANCE=\s*({number_pattern}))?", run_text
    ))
    mass_budget: dict[str, object] = {"status": "not_demonstrated"}
    if budget_matches:
        residuals = [fortran_float(match.group(3)) for match in budget_matches]
        totals = [fortran_float(match.group(4)) for match in budget_matches if match.group(4)]
        bin_sums = [fortran_float(match.group(5)) for match in budget_matches if match.group(5)]
        aggregate_total = sum(totals)
        aggregate_bin_sum = sum(bin_sums)
        relative_residual = abs(aggregate_total - aggregate_bin_sum) / max(
            abs(aggregate_total) + abs(aggregate_bin_sum), 1.0e-30
        )
        absolute_error = abs(aggregate_total - aggregate_bin_sum)
        absolute_tolerances = [fortran_float(match.group(9)) for match in budget_matches if match.group(9)]
        absolute_tolerance = min(absolute_tolerances) if absolute_tolerances else 0.0
        aggregate_residual = relative_residual
        mass_budget = {
            "name": budget_matches[0].group(1),
            "residual": aggregate_residual,
            "relative_residual": relative_residual,
            "absolute_error": absolute_error,
            "absolute_tolerance": absolute_tolerance,
            "residual_by_execution": residuals,
            "tolerance": 1.0e-10,
            "total": aggregate_total,
            "sum_of_bins": aggregate_bin_sum,
            "mass_loss": sum(fortran_float(match.group(6)) for match in budget_matches if match.group(6)),
            "boundary_flux_mass": sum(fortran_float(match.group(7)) for match in budget_matches if match.group(7)),
            "acceptance_rule": "relative residual <= 1e-10 or absolute error <= the case absolute tolerance",
            "status": "passed" if aggregate_residual <= 1.0e-10 or absolute_error <= absolute_tolerance else "blocked",
        }
    elif args.process == "wetdep":
        diagnostic_values = {item["name"]: item for item in diagnostic_summaries}
        wetdep_mass = sum(item["sum"] for name, item in diagnostic_values.items()
                          if name.startswith("wetdep_mass_"))
        wetdep_flux = sum(item["sum"] for name, item in diagnostic_values.items()
                          if name.startswith("wetdep_flux_"))
        residual = wetdep_mass - wetdep_flux * args.timestep
        if wetdep_mass > 0.0 and wetdep_flux > 0.0:
            mass_budget = {"name": "wetdep_mass_flux_closure", "mass": wetdep_mass,
                           "flux_times_timestep": wetdep_flux * args.timestep, "residual": residual,
                           "tolerance": 1.0e-10,
                           "status": "passed" if abs(residual) <= 1.0e-10 else "blocked"}
    elif args.process == "photolysis":
        mass_budget = {"status": "not_applicable",
                       "reason": "Photolysis publishes reaction rates and does not change constituent mass."}
    elif args.process == "gaschem":
        mass_budget = {"status": "not_applicable",
                       "reason": "This one-step acceptance checks mechanism-driven constituent tendencies and PHOTO dependencies; elemental conservation is outside this process smoke check."}
    if group_case and args.case_group == "emission-chemistry-removal":
        mass_budget = {
            "status": "not_demonstrated",
            "reason": "Per-process mass diagnostics do not establish per-species aerosol column closure or sulfur-atom closure.",
            "reported_process_budgets": [
                {"name": match.group(1), "species": match.group(2),
                 "residual": fortran_float(match.group(3)),
                 "total": fortran_float(match.group(4)) if match.group(4) else None,
                 "sum_of_bins": fortran_float(match.group(5)) if match.group(5) else None,
                 "mass_loss": fortran_float(match.group(6)) if match.group(6) else None,
                 "boundary_flux_mass": fortran_float(match.group(7)) if match.group(7) else None}
                for match in budget_matches
            ],
        }
    if group_case and args.case_group == "photolysis-gaschem":
        budget = next((item for item in manifest_data.get("budgets", [])
                       if item.get("state_weights") and item.get("exchange_weights")), None)
        if budget and args.emit_boundary_ledger and args.emit_met_ledger:
            mechanism_budget = mechanism_species_exchange_budget(
                run_text, met_input_ledger, budget, float(budget["residual_tolerance"]))
            mass_budget = {"status": mechanism_budget["status"], "system_budgets": [mechanism_budget]}
        else:
            mass_budget = {"status": "not_demonstrated",
                           "reason": "Configured mechanism budget requires a frozen limit and both solver/met ledgers"}
    if group_case and args.case_group == "emission-chemistry-removal" and args.emit_boundary_ledger and args.emit_met_ledger:
        tolerance_match = re.search(r"residual_tolerance:\s*([0-9.eE+-]+)", manifest_text)
        if tolerance_match:
            system_budgets = emission_system_budgets(run_text, boundary_state_ledger, met_input_ledger,
                                                     diagnostic_summaries, float(tolerance_match.group(1)))
            mass_budget['system_budgets'] = system_budgets
            mass_budget['status'] = 'passed' if all(row['status'] == 'passed' for row in system_budgets) else 'failed'
            mass_budget['reason'] = 'Independent process diagnostics and boundary fluxes close native pressure-coordinate inventories.'
    completed = (not invalid_met_fields and scientific_acceptance["status"] == "passed" and
                 mass_budget["status"] in ("passed", "not_applicable"))
    precision_backend = host_precision_backend(build, source)
    evidence = {
        "schema_version": 1,
        "created_utc": datetime.now(timezone.utc).isoformat(),
        "status": "passed" if completed else "blocked",
        "mode": "coupled-process-sequence" if group_case or args.process == "gaschem" else "one-process",
        "process": args.case_group or args.process,
        "processes": selected_processes,
        "case": args.case,
        "suite": suite_names[0],
        "source_revisions": revisions,
        "case_input": {"path": str(case_input), "sha256": digest(case_input)},
        "staged_case_input": staged_case_input,
        "external_data": {"gocart_climatology": gocart_climatology},
        "case_manifest": str(case_manifest),
        "ccpp_registry_order": args.registry_order,
        "unrelated_nonadvected_constituent": {
            "slot": int(unrelated_seed[0][0]),
            "seed_value": fortran_float(unrelated_seed[0][1]),
            "preserved_value": fortran_float(unrelated_preserved[0][1]),
            "status": "passed",
        },
        "unrelated_preexisting_tendency": {
            "slot": int(tendency_seeded[0][0]),
            "seed_value": fortran_float(tendency_seeded[0][1]),
            "preserved_value": fortran_float(tendency_preserved[0][1]),
            "status": "passed",
        },
        "host_state_increments": host_state_increments,
        "host_precision_backend": precision_backend,
        "scm_case_override": scm_case_override,
        "scm_case_runtime_overrides": case_runtime_overrides,
        "acceptance_aerosol_seed_kg_kg": env.get("CATCHEM_ACCEPTANCE_AEROSOL_MIXING_RATIO"),
        "acceptance_soil_moisture_m3_m3": env.get("CATCHEM_ACCEPTANCE_SOIL_MOISTURE"),
        "acceptance_wetdep_rain_m": env.get("CATCHEM_ACCEPTANCE_WETDEP_RAIN_M"),
        "acceptance_wetdep_flux_kg_m2_s": env.get("CATCHEM_ACCEPTANCE_WETDEP_FLUX_KG_M2_S"),
        "acceptance_gas_seeds_ppmv": gas_seeds,
        "physics_namelist": [{"name": name, "sha256": digest(source / "ccpp/physics_namelists" / name)}
                             for name in physics_namelist_inputs],
        "generated_process_caps": [str(path) for path in generated_calls],
        "selected_execution_marker": expected_marker,
        "selected_execution_count": sum(execution_counts.values()),
        "invocation_ledger": invocation_ledger,
        "host_update_ledger": host_update_ledger,
        "boundary_ledger_enabled": args.emit_boundary_ledger,
        "met_input_ledger": {
            "enabled": args.emit_met_ledger,
            "process_calls": len(met_input_ledger),
            "field_records": sum(len(item["fields"]) for item in met_input_ledger),
            "invalid_fields": invalid_met_fields,
            "status": "blocked_invalid_inputs" if invalid_met_fields else "captured",
            "path": str(met_ledger_path) if met_ledger_path else None,
            "semantics": "successfully bound CATChem process inputs after driver conversions and derivations",
        },
        "boundary_state_ledger": boundary_state_ledger,
        "full_constituent_state_ledger": full_constituent_state_ledger,
        "full_constituent_state_diagnostics": sorted(
            {item["diagnostic_name"] for item in full_constituent_state_ledger}),
        "diagnostic_summaries": diagnostic_summaries,
        "physical_signals": physical_signals,
        "scientific_acceptance": scientific_acceptance,
        "mass_budget": mass_budget,
        "output_files": products,
        "logs": {name: str(logs / f"{name}.log") for name in ("configure", "build", "run")},
    }
    evidence_path = case_root / "evidence.json"
    evidence_path.write_text(json.dumps(evidence, indent=2) + "\n")
    print(json.dumps(evidence, indent=2))


def main() -> int:
    """Parse a one-process case request and report actionable failures."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--work-dir", required=True, help="prepared directory from util/ccpp_scm.py")
    parser.add_argument("--process", required=True, choices=PROCESSES)
    parser.add_argument("--processes", help="ordered comma-separated process group for a shared host run")
    parser.add_argument("--case-group", choices=tuple(PROCESS_GROUPS),
                        help="run the manifest-defined ordered coupled process group")
    parser.add_argument("--suite-set", help="suite variants compiled into a shared cap")
    parser.add_argument("--process-set", help="processes compiled into a shared cap")
    parser.add_argument("--shared-build-dir", help="shared build directory for coupled suite cap reuse")
    parser.add_argument("--case")
    parser.add_argument("--runtime", type=int, default=600,
                        help="SCM runtime in seconds; one selected-process call requires 600 s")
    parser.add_argument("--timestep", type=int, default=600,
                        help="SCM physics timestep in seconds (GFS v17 maximum is 600 s)")
    parser.add_argument("--subcycle-count", type=int, default=1,
                        help="repeat the selected CATChem driver and its atomic host update within each interval")
    parser.add_argument("--suite", default="SCM_GFS_v17_p8_ugwpv1")
    parser.add_argument("--registry-order", choices=("manifest", "reverse"), default="manifest",
                        help="register CATChem constituents in manifest or reversed order")
    parser.add_argument("--catchem-config", help="CATChem runtime YAML; defaults to the process case config")
    parser.add_argument("--build-type", default="Debug")
    parser.add_argument("--science-profile", choices=("process", "all"), default="process",
                        help="all enables MUSICA and Kokkos for the all-science profile")
    # The locked SCM build contains case-variant Fortran sources that emit
    # duplicate module names; parallel compilation races while writing .mod files.
    parser.add_argument("--jobs", type=int, default=1)
    parser.add_argument("--docker-image", help="run SCM configure/build/case in the supplied Linux image")
    parser.add_argument("--keep-existing", action="store_true")
    parser.add_argument("--inject-process-failure", action="store_true",
                        help="run an acceptance-only GasChem body failure probe with no CCPP host publication")
    parser.add_argument("--emit-boundary-ledger", action="store_true",
                        help="record mapped CCPP constituent values before and after every successful host update")
    parser.add_argument("--emit-met-ledger", action="store_true",
                        help="record each process's successfully bound CATChem meteorology fields")
    parser.add_argument("--build-only", action="store_true",
                        help="configure and compile generated CCPP caps without running SCM")
    args = parser.parse_args()
    if args.case is None:
        args.case = {"seasalt": "COMBLE", "so4chem": "COMBLE", "wetdep": "twpice",
                     "carbchem": "bomex", "dust": "arm_sgp_summer_1997_A",
                     "drydep": "arm_sgp_summer_1997_A", "settling": "bomex"}.get(
            args.process, "arm_sgp_summer_1997_A")
    if args.subcycle_count <= 0:
        parser.error("subcycle count must be positive")
    if args.timestep <= 0 or args.runtime < args.timestep or args.runtime % args.timestep != 0:
        parser.error("selected-process execution evidence requires a positive timestep and runtime as an integer multiple of it")
    try:
        run_case(args)
    except (OSError, ValueError, RuntimeError, TypeError, KeyError,
            subprocess.CalledProcessError, json.JSONDecodeError) as exc:
        case_root = (Path(args.work_dir).resolve() / "cases" /
                     f"{args.process}-{args.case}-{args.suite}")
        case_root.mkdir(parents=True, exist_ok=True)
        failure = {
            "schema_version": 1,
            "created_utc": datetime.now(timezone.utc).isoformat(),
            "status": "failed",
            "mode": "coupled-process-sequence" if args.process == "gaschem" else "one-process",
            "process": args.process,
            "case": args.case,
            "suite": args.suite,
            "failure": str(exc),
        }
        (case_root / "evidence.json").write_text(json.dumps(failure, indent=2) + "\n")
        print(f"run_case: error: {exc}", file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

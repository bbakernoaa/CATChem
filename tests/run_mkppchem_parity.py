#!/usr/bin/env python3
"""Run the mkppchem-vs-GOCART carbon chemistry parity comparison.

This is the process-vs-process parity gate for the MKPP chemistry process.  It reuses the staging and comparison machinery of
``run_default_numerical_parity.py`` (same isolated run directories, same MET
profile builder, same zero-emissions control) and ``compare_parity_snapshots.
py``
(same normalized schema), with the two runners being the SAME executable
(``mkppchem_parity_runner``) launched twice against different staged
configurations:

* baseline  -- ``test1: [carbchem, so4chem]`` on the 22-species Default
  inventory (the existing GOCART chemistry path);
* candidate -- ``test1: [mkppchem]`` on the 26-species generated-mechanism
  inventory.

Only the four carbon species are compared pointwise by default.  The other
mechanism species either have no counterpart tendency in the GOCART path (the
six nitrate-chain tracers) or depend on the cloud-gated aqueous pathway
(``SO2``/``SO4``), so they are excluded from the pointwise assertion by design
and are validated for finiteness, non-negativity, and sulfur conservation
elsewhere.  With ``--cloudy-sulfur`` the aqueous sulfur pathway is exercised by
a candidate-only gate-activation check: two candidate runs share an elevated
sulfur seed and differ only in cloud liquid water, and the cloud-gated aqueous
``SO2 -> SO4`` transfer must switch on with cloud water while conserving sulfur
1:1.  A pointwise sulfur match against ``so4chem`` is deliberately NOT asserted
in any regime: the two schemes parameterize sulfate formation differently (a
full GOCART Henry's-law in-cloud model plus its own gas-phase path, versus a
lumped gated bimolecular term plus a separate gas-phase term), so their SO2/SO4
tendencies diverge by construction both in-cloud and dry; a physically
consistent in-cloud comparison would need the climatological oxidant and cloud
fields only an ExtData-driven run carries.  Per-species
relative tolerances, each justified below against the two solvers' own error
controls, are applied by projecting both snapshots onto one species at a time
and invoking the shared comparator.

Usage (normally from CTest)::

    python3 run_mkppchem_parity.py --runner <mkppchem_parity_runner> \
        [--columns 2] [--levels 57] [--steps 2] [--dt 60]
"""

import argparse
import json
import math
import os
import re
import shutil
import subprocess
import sys
from pathlib import Path

from run_default_numerical_parity import stage

# Physical constants backing the derived-cloud-water Magnus formula, matching
# src/core/catchem_constants.hpp (the same values the in-kernel derivation and
# validate_met use).  Used only to size the stageable profile to the levels
# where that documented derivation stays physically valid.
_H2O_MW = 18.016
_AIR_MW = 28.9644
_EPS = _H2O_MW / _AIR_MW


def _magnus_denominator_positive(temperature_k: float, pmid_pa: float) -> bool:
    """True where the derived cloud-water Magnus denominator PMID-(1-eps)*e_sat
    stays positive, the same guard derive_cloud_liquid_water_device applies
    before trusting the derivation (a non-positive value flags the cell as a
    non-physical column)."""
    if temperature_k <= 29.65:
        return False
    e_sat = 611.2 * math.exp(17.67 * (temperature_k - 273.15) / (temperature_k - 29.65))
    return pmid_pa - (1.0 - _EPS) * e_sat > 0.0


def trim_profile_to_derivation(profile: Path) -> int:
    """Truncate a staged MET profile to the tallest contiguous level prefix on
    which the derived cloud-water source is physically valid for every column.

    The shared NCWCP sounding has a mesospheric temperature rebound (~265 K
    near 0.1 hPa) that pushes the Magnus denominator below zero in the upper
    levels, where mkppchem's per-cell met validation flags a non-physical
    column -- a documented limitation of the derived fallback, not a solver
    defect.  GOCART chemistry is a tropospheric scheme, so dropping the
    invalid top levels keeps the carbon aging parity honest without inventing
    a valid column.  Returns the retained level count.
    """
    document = json.loads(profile.read_text(encoding="utf-8"))
    met_3d = document["met_3d"]
    levels = document["grid"]["levels"]
    columns = document["grid"]["columns"]
    temperature = met_3d["T"]
    pmid = met_3d["PMID"]

    def value_at(rows, lev, col):
        row = rows[lev]
        return row[col] if isinstance(row, list) else row

    valid_levels = levels
    for lev in range(levels):
        if not all(_magnus_denominator_positive(value_at(temperature, lev, col), value_at(pmid, lev, col))
                   for col in range(columns)):
            valid_levels = lev
            break

    if valid_levels < 2:
        raise SystemExit("derived cloud water is invalid across the whole staged profile")
    if valid_levels == levels:
        return levels

    for name in list(met_3d):
        met_3d[name] = met_3d[name][:valid_levels]
    for name in list(document["met_interface"]):
        # Interface fields carry levels + 1 entries; keep valid_levels + 1.
        document["met_interface"][name] = document["met_interface"][name][:valid_levels + 1]
    document["grid"]["levels"] = valid_levels
    profile.write_text(json.dumps(document), encoding="utf-8")
    return valid_levels

# The carbon species compared pointwise: the hydrophobic/hydrophilic aging
# pairs, identical short names in both inventories.
CARBON_SPECIES = ("bc1", "bc2", "oc1", "oc2")

# The mechanism's BC1->BC2 and OC1->OC2 aging constants, exactly 5.0e-06 s^-1
# in the generated rates (src/external/mkpp/mkpp-generated/gocart/rates.cpp:
#   rates[6] = -5e-06*state[6]; rates[7] = +5e-06*state[6];  (BC1 -> BC2)
#   rates[8] = -5e-06*state[8]; rates[9] = +5e-06*state[8];  (OC1 -> OC2))
# and openair-declared in parm/config/openatmos/gocart/mechanism.yaml
# (Arrhenius A = 5.0e-06, no products other than the hydrophilic mode).
AGING_RATE_PER_S = 5.0e-6

# Per-species (rtol, atol) for compare_parity_snapshots.py, each justified
# against the two solvers' own error controls rather than tuned to pass:
#
# * The MKPP side integrates with ROS-3 at rtol = 1.0e-6 and atol = 1.0e-3
#   mol/m3 per species (mkpp-generated/gocart.hpp:539-540).  The carbon rows
#   are fully decoupled from every other species (rates.cpp:6-9 reference only
#   state[6..9]), so each carbon pool obeys the scalar linear ODE
#   dy/dt = -5e-6 y, whose per-step ROS-3 update is the stability function
#   R(z), z = 5e-6*dt.  At the tested dt <= 600 s, z <= 3e-3 and the local
#   truncation error |R(z) - exp(-z)| = O(z^3) < 3e-8 per step; accumulated
#   over the handful of steps this harness runs it stays below ~1e-6 relative.
#   The GOCART side applies exp(-dt/tau) exactly (carbchem's aging is aligned
#   to tau = 1/5e-6 s below), so its own deviation from exp(-z) is only IEEE
#   rounding.
# * bc1/oc1 (the decaying pools): rtol = 1.0e-3 gives ~3 orders of margin over
#   the truncation bound above and absorbs the ROS-3 step-controller noise at
#   rtol = 1e-6 across steps.  atol = 1.0e-18 mol/mol bounds the error where a
#   pool would otherwise vanish; it is ~1e-6 of the 4e-12 seed.
# * bc2/oc2 (the growing pools): same rtol, but the transferred increment per
#   step is (1 - exp(-z)) * y0 ~ 3e-4 * 4e-12 = 1.2e-15 mol/mol, and the two
#   sides' increments agree to the same O(z^3) relative bound, so the same
#   (1.0e-3, 1.0e-18) envelope applies.  A well-behaved carbon aging pair
#   driven by a single first-order 5e-6 rate needs no looser envelope than
#   1e-3.
TOLERANCES = {
    "bc1": (1.0e-3, 1.0e-18),
    "bc2": (1.0e-3, 1.0e-18),
    "oc1": (1.0e-3, 1.0e-18),
    "oc2": (1.0e-3, 1.0e-18),
}

# Gate-activation check (candidate only, --cloudy-sulfur).  An elevated sulfur
# seed makes the bimolecular aqueous term measurable over the run; the cloud
# layer is a short surface slice fed through the process's host-field
# cloud-water source so the gate opens there and nowhere else.  SO2 -> SO4 must
# transfer near 1:1 (the aqueous pathway moves one S atom per reaction) and the
# in-cloud sulfate gain must dwarf the cloud-free gain.  No pointwise match
# against so4chem is attempted -- the two schemes' sulfate chemistry differs by
# construction (see the module docstring).
SULFUR_SEED = "so2=1.0e-6,h2o2=1.0e-6"
SULFUR_CLOUD_LEVELS = 6
SULFUR_CLOUD_CLW = 1.0e-3          # kg/m3, saturates the sigmoid gate
SULFUR_TRANSFER_RTOL = 0.25        # |dSO2 - dSO4| within 25% of dSO4
SULFUR_ACTIVATION_FACTOR = 100.0   # in-cloud gain > 100x cloud-free gain

# Cross-backend tolerance: the SAME generated solver runs on
# both backends, so the only divergence is floating-point reduction order --
# per-cell kernels are independent, but a GPU's warp-level and an OpenMP
# thread's summations round a multi-step stiff solve differently. There is no
# algorithmic difference to absorb, so this is deliberately looser than the
# process-vs-process carbon tolerance (1e-3) only where the two solvers' step
# controllers can land on different intermediate stages: a documented 1e-2
# relative envelope with the same 1e-18 absolute floor. It is a real,
# non-zero tolerance, not a rubber stamp -- a backend that mis-lays out the
# (cell, species) staging or drops a per-timestep refresh drifts far
# beyond it.
CROSS_BACKEND_TOLERANCES = {
    "bc1": (1.0e-2, 1.0e-18),
    "bc2": (1.0e-2, 1.0e-18),
    "oc1": (1.0e-2, 1.0e-18),
    "oc2": (1.0e-2, 1.0e-18),
}


def read_backend(path: Path) -> dict:
    """Return the snapshot's recorded `backend` provenance, or {} if absent.

    The parity runner stamps the execution-space name and whether the default
    memory space is HostSpace; the cross-backend gate uses the name to confirm
    two distinct backends actually ran, and the zero-copy gate uses
    `space_is_host` to decide whether a device transfer is even possible.
    """
    document = json.loads(path.read_text(encoding="utf-8"))
    return document.get("backend", {})


def verify_zero_copy(candidate_snapshot: Path, memcpy_baseline: int | None) -> int:
    """Confirm the process adds no full-state host<->
    device transfer per timestep beyond the StateManager's own sync.

    The gate has a structural half and a measured half. The structural half is
    always checked: the candidate snapshot records whether the default memory
    space is HostSpace. When it is (OpenMP/Serial), device memory does not
    exist, every host<->device copy is a no-op, and the zero-copy condition
    holds by construction -- this is the documented trivial case. When the
    space is a real device (a CUDA/HIP build), the full-state transfer count
    must be measured with an external profiler (nsys, per quickstart §8): the
    profiling wrapper exports CATCHEM_MEMCPY_COUNT, and --memcpy-baseline
    supplies the baseline (so4chem+carbchem) count for the same timestep
    series. Returns 0 on pass or on an honest SKIPPED (device build without
    profiling data), 1 on a measured over-count.
    """
    backend = read_backend(candidate_snapshot)
    name = backend.get("name", "unknown")
    if backend.get("space_is_host", True):
        print(f"[PASS] zero-copy: backend '{name}' is host-resident "
              f"(HostSpace == DeviceSpace), so the process introduces no device transfer")
        return 0
    measured = os.environ.get("CATCHEM_MEMCPY_COUNT")
    if measured is None or memcpy_baseline is None:
        print(f"[SKIPPED] zero-copy: backend '{name}' is device-resident; "
              f"run the candidate under nsys, export CATCHEM_MEMCPY_COUNT=<count> and pass "
              f"--memcpy-baseline <n> to assert the transfer count. "
              f"Structural zero-copy (device views, per-cell kernels, no host staging) "
              f"is guaranteed by the process code.")
        return 0
    if int(measured) > memcpy_baseline:
        print(f"[FAIL] zero-copy: device backend '{name}' performed {measured} full-state "
              f"copies vs baseline {memcpy_baseline}", file=sys.stderr)
        return 1
    print(f"[PASS] zero-copy: device backend '{name}' full-state copies "
          f"{measured} <= baseline {memcpy_baseline}")
    return 0


def cross_backend_compare(runner: Path, config: Path, profile: Path, run_dir: Path,
                          cross_runner: Path, source_root: Path, steps: int, dt: float) -> int:
    """Run the mkppchem candidate on two backends and compare.

    Both sides use the SAME staged candidate configuration and run directory;
    only the executable differs (each binary is built against a different
    Kokkos backend). The baseline for the comparison is the primary `--runner`
    backend, and the harness records which backend produced it, as required.
    Returns 0 when the carbon species agree within CROSS_BACKEND_TOLERANCES
    and the two snapshots came from genuinely different backends.
    """
    primary = run_side("backend_primary", runner, run_dir, config, profile, steps, dt)
    cross = run_side("backend_cross", cross_runner, run_dir, config, profile, steps, dt)
    primary_backend = read_backend(primary).get("name", "unknown")
    cross_backend = read_backend(cross).get("name", "unknown")
    print(f"cross-backend parity: baseline backend = '{primary_backend}', "
          f"comparison backend = '{cross_backend}'")
    if primary_backend == cross_backend:
        print(f"[FAIL] cross-backend parity: both runners report the same backend "
              f"'{primary_backend}'; supply a second runner built for a different backend",
              file=sys.stderr)
        return 1
    comparator = source_root / "tests" / "compare_parity_snapshots.py"
    failures = []
    for species in CARBON_SPECIES:
        rtol, atol = CROSS_BACKEND_TOLERANCES[species]
        a = run_dir / f"cross_{species}_primary.json"
        b = run_dir / f"cross_{species}_cross.json"
        project_snapshot(primary, (species,), a)
        project_snapshot(cross, (species,), b)
        result = subprocess.run(
            [sys.executable, str(comparator), str(a), str(b), "--rtol", str(rtol), "--atol", str(atol)],
            check=False, capture_output=True, text=True,
        )
        status = "PASS" if result.returncode == 0 else "FAIL"
        print(f"[{status}] {species} ({primary_backend} vs {cross_backend}): "
              f"{result.stdout.strip() or result.stderr.strip()}")
        if result.returncode != 0:
            failures.append(species)
    if failures:
        print(f"CROSS-BACKEND FAILURE: carbon species outside tolerance: {', '.join(failures)}",
              file=sys.stderr)
        return 1
    print(f"PASS: cross-backend carbon parity ({primary_backend} vs {cross_backend}) within documented tolerance")
    return 0


def _slice_profile_columns(profile: Path, out_path: Path, keep: int) -> None:
    """Write a copy of a staged MET profile truncated to its first `keep` columns.

    The staged profiles carry one shared vertical column per level (scalar
    per level for the 3-D fields) and per-column entries for the 2-D fields;
    slicing the 2-D arrays and the grid's column count yields a valid smaller
    grid whose retained columns are identical to the original's.
    """
    document = json.loads(profile.read_text(encoding="utf-8"))
    columns = document["grid"]["columns"]
    if keep > columns:
        raise RuntimeError(f"cannot slice a {columns}-column profile to {keep} columns")
    for name in list(document["met_2d"]):
        document["met_2d"][name] = document["met_2d"][name][:keep]
    # met_3d, met_interface and met_soil entries are broadcast profiles (one
    # value per level, not per column), so only the 2-D fields and the grid
    # extent change.
    document["grid"]["columns"] = keep
    out_path.write_text(json.dumps(document), encoding="utf-8")


def verify_repeat_determinism(runner: Path, config: Path, profile: Path, run_dir: Path,
                              steps: int, dt: float) -> int:
    """Byte-identical repeat: the same runner, configuration, profile, step
    count and dt, executed twice, must produce snapshots that are identical
    byte for byte. A single differing byte is a nondeterminism defect -- the
    snapshot values are written at round-trip precision, so equality of the
    text is equality of the doubles.
    """
    first = run_side("repeat_a", runner, run_dir, config, profile, steps, dt)
    second = run_side("repeat_b", runner, run_dir, config, profile, steps, dt)
    if first.read_bytes() == second.read_bytes():
        print("[PASS] repeat determinism: two identical runs produced byte-identical snapshots")
        return 0
    print("[FAIL] repeat determinism: snapshots differ between two identical runs "
          f"({first} vs {second})", file=sys.stderr)
    return 1


def verify_column_decomposition(runner: Path, config: Path, profile: Path, run_dir: Path,
                                columns: int, levels: int, steps: int, dt: float) -> int:
    """Rank-count agreement, decomposition-independence form: the process
    solves every cell independently from per-cell inputs, so a run on a grid
    with a different column count must reproduce the original run's values in
    the retained columns exactly (byte-for-byte at round-trip precision).

    This is the honest serial proxy for the distributed gate in quickstart §7:
    the parity runner is a serial executable, so true multi-rank agreement is
    verified by the distributed smoke path (and by the error-reduction
    stability unit test), while this gate proves the numerics do not depend on
    the grid extent a column is embedded in.
    """
    full = run_side("decomp_full", runner, run_dir, config, profile, steps, dt)
    split = run_dir / "profile_split.json"
    _slice_profile_columns(profile, split, columns - 1)
    smaller = run_side("decomp_small", runner, run_dir, config, split, steps, dt)
    full_doc = json.loads(full.read_text(encoding="utf-8"))
    small_doc = json.loads(smaller.read_text(encoding="utf-8"))
    full_field = full_doc["snapshots"][0]["fields"]["concentration"]
    small_field = small_doc["snapshots"][0]["fields"]["concentration"]
    fc, fl, fs = full_field["shape"]
    sc, sl, ss = small_field["shape"]
    if (fl, fs) != (sl, ss):
        print(f"[FAIL] column decomposition: level/species extents disagree ({full_field['shape']} vs "
              f"{small_field['shape']})", file=sys.stderr)
        return 1

    def at(field, columns_, levels_, species_, col, lev, sp):
        # Serialised order: column outermost, species innermost (the runner's
        # write_json loop order), matching project_snapshot.at().
        return field["values"][col * levels_ * species_ + lev * species_ + sp]

    mismatch = 0
    worst = 0.0
    for lev in range(fl):
        for sp in range(fs):
            a = at(full_field, fc, fl, fs, 0, lev, sp)
            b = at(small_field, sc, sl, ss, 0, lev, sp)
            if a != b:
                mismatch += 1
                worst = max(worst, abs(a - b))
    if mismatch == 0:
        print(f"[PASS] column decomposition: column 0 agrees bit-for-bit between the "
              f"{fc}- and {sc}-column grids ({fl * fs} values compared)")
        return 0
    print(f"[FAIL] column decomposition: {mismatch} of {fl * fs} retained-column values differ "
          f"between the {fc}- and {sc}-column grids (max abs {worst:g})", file=sys.stderr)
    return 1


def verify_failure_message_stability(runner: Path, config: Path, profile: Path, run_dir: Path,
                                     steps: int, dt: float) -> int:
    """Diagnostic reproducibility: a poisoned input cell must produce the same
    fatal message on every repeat AND name the same cell on a grid with a
    different column count -- the reported failing cell is the lowest failing
    index, so the diagnostic is deterministic, not just the happy path.

    The staged profiles carry one scalar per level (broadcast across columns),
    so poisoning a level flags every column of it and the lowest failing cell
    is column 1 of that level in either grid.
    """
    poisoned = json.loads(profile.read_text(encoding="utf-8"))
    levels = poisoned["grid"]["levels"]
    columns = poisoned["grid"]["columns"]
    if columns < 2:
        print("[SKIPPED] failure-message stability: profile has fewer than two columns")
        return 0
    # RH outside [0, 1] is non-physical met; validate_met flags the cell and
    # the reduction must name exactly it.
    mid = levels // 2
    poisoned["met_3d"]["RH"][mid] = 1.0e9
    poison_path = run_dir / "profile_poisoned.json"
    poison_path.write_text(json.dumps(poisoned), encoding="utf-8")

    def run_poisoned(label: str, profile_path: Path) -> str:
        proc = subprocess.run(
            [str(runner), "--config", str(config), "--met-profile", str(profile_path),
             "--snapshot", str(run_dir / f"{label}.json"), "--steps", str(steps), "--dt", str(dt)],
            cwd=run_dir, check=False, capture_output=True, text=True,
        )
        if proc.returncode == 0:
            raise RuntimeError("poisoned-cell run unexpectedly succeeded; the gate is not armed")
        return proc.stderr

    expected_cell = f"column 1, level {mid + 1}"
    messages = {run_poisoned("poison_1", poison_path), run_poisoned("poison_2", poison_path)}
    if len(messages) != 1:
        print("[FAIL] failure-message stability: repeated poisoned runs produced different messages",
              file=sys.stderr)
        return 1
    message = messages.pop()
    if expected_cell not in message:
        print(f"[FAIL] failure-message stability: message does not name the poisoned cell "
              f"({expected_cell}): {message.strip()}", file=sys.stderr)
        return 1

    # The same poison on a one-column-fewer grid must name the same lowest cell.
    sliced = run_dir / "profile_poisoned_split.json"
    _slice_profile_columns(poison_path, sliced, columns - 1)
    shifted = run_poisoned("poison_split", sliced)
    if expected_cell not in shifted:
        print(f"[FAIL] failure-message stability: split-grid message does not name the lowest cell "
              f"({expected_cell}): {shifted.strip()}", file=sys.stderr)
        return 1
    print("[PASS] failure-message stability: identical message on repeat, lowest-cell identity "
          "unchanged under a different column count")
    return 0


def verify_determinism_suite(runner: Path, config: Path, profile: Path, run_dir: Path,
                             columns: int, levels: int, steps: int, dt: float) -> int:
    """The full determinism/reproducibility gate:
    byte-identical repeat, column-decomposition independence, and fatal-
    diagnostic stability. Returns 0 only when every sub-gate passes or is
    honestly skipped."""
    for gate in (verify_repeat_determinism(runner, config, profile, run_dir, steps, dt),
                 verify_column_decomposition(runner, config, profile, run_dir, columns, levels, steps, dt),
                 verify_failure_message_stability(runner, config, profile, run_dir, steps, dt)):
        if gate != 0:
            return gate
    return 0



def project_snapshot(path: Path, keep: tuple[str, ...], out_path: Path) -> None:
    """Rewrite a full-inventory snapshot keeping only the ``keep`` species.

    The comparator requires the species metadata to match exactly, which the
    22-species baseline and 26-species candidate inventories cannot do.
    Projecting both onto the same named subset restores a well-defined
    pointwise comparison without weakening the comparator itself.
    """
    document = json.loads(path.read_text(encoding="utf-8"))
    snapshot = document["snapshots"][0]
    field = snapshot["fields"]["concentration"]
    names = field["species"]
    columns, levels, species = field["shape"]
    values = field["values"]
    if sorted(keep) != sorted(set(keep)) or any(name not in names for name in keep):
        raise RuntimeError(f"snapshot {path} lacks one of the compared species {keep}")
    indices = [names.index(name) for name in keep]

    def at(column: int, level: int, species_index: int) -> float:
        # The runner's write_json emits the flat array in loop order: column
        # outermost, species innermost (index = column*levels*species +
        # level*species + species_index). The buffer itself is native
        # LayoutLeft; only the serialized order differs.
        return values[column * levels * species + level * species + species_index]

    projected = [at(c, lev, s) for c in range(columns) for lev in range(levels) for s in indices]
    out_path.write_text(
        json.dumps(
            {
                "schema_version": 1,
                "snapshots": [
                    {
                        "checkpoint": snapshot["checkpoint"],
                        "fields": {
                            "concentration": {
                                "units": field["units"],
                                "shape": [columns, levels, len(keep)],
                                "species": list(keep),
                                "values": projected,
                            }
                        },
                    }
                ],
            }
        ),
        encoding="utf-8",
    )


def stage_baseline(run_dir: Path, source_root: Path, columns: int, levels: int) -> tuple[Path, Path]:
    """Stage the existing GOCART chemistry path (carbchem + so4chem)."""
    config, profile = stage(run_dir, source_root, columns, levels, ("carbchem", "so4chem"))
    text = config.read_text(encoding="utf-8")
    # Align carbchem's hydrophobic->hydrophilic aging to the mechanism's
    # 5e-6 s^-1 constant.  The Default fixture's 2.5-day e-folding encodes
    # 4.63e-6 s^-1, a 7.5% rate difference that would dominate any solver
    # tolerance; with tau = 1/(5e-6*86400) days both sides integrate the same
    # first-order transfer and the residual is pure integration error.
    tau_days = 1.0 / (AGING_RATE_PER_S * 86400.0)
    text, replaced = re.subn(
        r"(time_days_hydrophobic_to_hydrophilic: )2\.5",
        rf"\g<1>{tau_days!r}",
        text,
        count=1,
    )
    if replaced != 1:
        raise RuntimeError("could not align the carbchem aging rate in the staged baseline config")
    config.write_text(text, encoding="utf-8")
    return config, profile


def stage_candidate(run_dir: Path, source_root: Path, columns: int, levels: int,
                    solver_seed: float | None = None) -> tuple[Path, Path]:
    """Stage the MKPP process (mkppchem only) on the mechanism inventory."""
    config, profile = stage(run_dir, source_root, columns, levels, ("mkppchem",))
    # Every mechanism species must resolve to a tracer, and the
    # Default inventory lacks the six nitrate-chain species, so the candidate
    # runs on the generated 26-species file, staged under the name the
    # Default config already points at.
    shutil.copy2(source_root / "tests" / "Configs" / "Mkpp" / "CATChem_mkppchem_species.yml",
                 run_dir / "CATChem_species.yml")
    text = config.read_text(encoding="utf-8")
    # Inject the mkppchem settings block at the top-level `processes:` map
    # (column-0 anchor; the test1 phase list is indented and untouched).
    # Absolute definition_file: staged run directories are outside the repo,
    # and the path is provenance-logged (the runtime facts come from the
    # compiled metadata header, not from parsing this file).
    block = (
        "processes:\n"
        "  mkppchem:\n"
        "    activate: true\n"
        '    mechanism: "gocart"\n'
        f"    definition_file: \"{(source_root / 'parm' / 'config' / 'openatmos' / 'gocart' / 'mechanism.yaml')}\"\n"
        "    cloud_liquid_water:\n"
        "      mode: derived\n"
        # The parity phase runs mkppchem alone (no photolysis process), and the
        # mechanism declares a photolysis reaction, so the coupling contract
        # requires smoke mode here: J-values come from the definition's
        # host-forcing constants. This does not perturb the compared carbon
        # species (BC/OC aging is OH-independent); sulfur parity is gated
        # separately on the MKPP regeneration.
        "    photolysis_smoke_mode: true\n"
    )
    if solver_seed is not None:
        # Runtime ramp-seed override: the same solve a compile-time seed would
        # bake, configured per run instead of per artifact. Used to show the
        # runtime path reproduces a compile-time-seeded snapshot bit-for-bit.
        block += f"    initial_step_seed: {solver_seed!r}\n"
    text, replaced = re.subn(r"^processes:\n", block.replace("\\", "\\\\"), text, count=1, flags=re.M)
    if replaced != 1:
        raise RuntimeError("could not inject the mkppchem settings block into the staged candidate config")
    config.write_text(text, encoding="utf-8")
    return config, profile


def run_side(label: str, runner: Path, run_dir: Path, config: Path, profile: Path, steps: int,
             dt: float, seed: str | None = None) -> Path:
    snapshot = run_dir / f"{label}.json"
    command = [str(runner), "--config", str(config), "--met-profile", str(profile),
               "--snapshot", str(snapshot), "--steps", str(steps), "--dt", str(dt)]
    if seed:
        command += ["--seed", seed]
    subprocess.run(command, cwd=run_dir, check=True)
    if not snapshot.is_file():
        raise RuntimeError(f"{label} runner did not write {snapshot}")
    return snapshot


def _species_value(snapshot_path: Path, label: str, column: int = 0, level: int = 0) -> float:
    """Read one species at (column, level) from a snapshot's final checkpoint.

    The serialized order is column outermost, species innermost (index =
    column*levels*species + level*species + species_index), matching the
    runner's write loop.
    """
    document = json.loads(Path(snapshot_path).read_text(encoding="utf-8"))
    field = document["snapshots"][-1]["fields"]["concentration"]
    columns, levels, species = field["shape"]
    index = field["species"].index(label)
    return field["values"][column * levels * species + level * species + index]


def _inject_cloudy_host_field(profile: Path, out_path: Path, clw: float, cloud_levels: int) -> None:
    """Write a copy of a staged profile carrying a host cloud-liquid-water field.

    The staged sounding is cloud-free (CLDF = 0 everywhere), so the derived
    cloud-water source yields zero and the aqueous gate stays shut.  Adding a
    CLDLIQ field with a nonzero surface slice lets the process's host-field
    cloud-water source open the gate in exactly those levels; the rest of the
    profile is untouched so the comparison isolates the cloud effect.
    """
    document = json.loads(profile.read_text(encoding="utf-8"))
    levels = document["grid"]["levels"]
    document["met_3d"]["CLDLIQ"] = [clw if lev < cloud_levels else 0.0 for lev in range(levels)]
    out_path.write_text(json.dumps(document), encoding="utf-8")


def _candidate_cloudy_config(config: Path, out_path: Path) -> None:
    """Rewrite the staged candidate config to read cloud water from CLDLIQ.

    The staged candidate uses the derived cloud-water source (grid-mean
    condensate from CLDF/QV), which is zero in the cloud-free sounding.  Point
    it at the injected host field so the gate opens where CLDLIQ is set.
    """
    text = config.read_text(encoding="utf-8")
    text, replaced = re.subn(
        r"( *)mode: derived\n",
        r"\g<1>mode: host_field\n\g<1>host_field: CLDLIQ\n",
        text,
        count=1,
    )
    if replaced != 1:
        raise RuntimeError("could not switch the staged candidate to a host-field cloud-water source")
    out_path.write_text(text, encoding="utf-8")


def verify_sulfur_gate(runner: Path, candidate_dir: Path, candidate_config: Path,
                       candidate_profile: Path, steps: int, dt: float) -> int:
    """Candidate-only proof that the cloud-liquid-water gate drives aqueous SO2.

    Two candidate runs share the elevated sulfur seed and differ only in cloud
    water: the staged (cloud-free, derived) profile keeps the gate shut, the
    injected host-field profile opens it in the surface slice.  The aqueous
    pathway transfers one S atom per reaction, so the in-cloud SO2 loss must
    match the SO4 gain near 1:1 and the in-cloud gain must dwarf the cloud-free
    gain.  This asserts the gate's behavior through the full process; it does
    NOT compare against so4chem, whose in-cloud chemistry is a different
    parameterization.  Returns 0 on pass, 1 on failure.
    """
    dry = run_side("sulfur_dry", runner, candidate_dir, candidate_config, candidate_profile,
                   steps, dt, seed=SULFUR_SEED)
    cloudy_profile = candidate_dir / "sulfur_cloudy_met.json"
    _inject_cloudy_host_field(candidate_profile, cloudy_profile, SULFUR_CLOUD_CLW, SULFUR_CLOUD_LEVELS)
    cloudy_config = candidate_dir / "sulfur_cloudy_config.yml"
    _candidate_cloudy_config(candidate_config, cloudy_config)
    cloudy = run_side("sulfur_cloudy", runner, candidate_dir, cloudy_config, cloudy_profile,
                      steps, dt, seed=SULFUR_SEED)

    so2_dry, so2_cloudy = _species_value(dry, "so2"), _species_value(cloudy, "so2")
    so4_dry, so4_cloudy = _species_value(dry, "so4"), _species_value(cloudy, "so4")
    d_so2 = so2_dry - so2_cloudy
    d_so4 = so4_cloudy - so4_dry
    print(f"sulfur gate: cloud-free dSO4 {so4_dry - 4.0e-12:.3e}, "
          f"in-cloud dSO4 {d_so4:.3e}, in-cloud dSO2 {d_so2:.3e}")
    if d_so4 <= 0.0:
        print("[FAIL] sulfur gate: no in-cloud sulfate production (gate did not open)", file=sys.stderr)
        return 1
    if d_so4 < SULFUR_ACTIVATION_FACTOR * max(so4_dry - 4.0e-12, 1e-30):
        print(f"[FAIL] sulfur gate: in-cloud sulfate gain {d_so4:.3e} not > "
              f"{SULFUR_ACTIVATION_FACTOR:.0f}x the cloud-free gain", file=sys.stderr)
        return 1
    if abs(d_so2 - d_so4) > SULFUR_TRANSFER_RTOL * d_so4:
        print(f"[FAIL] sulfur gate: SO2 loss {d_so2:.3e} does not track SO4 gain {d_so4:.3e} "
              f"(aqueous pathway should transfer sulfur 1:1)", file=sys.stderr)
        return 1
    print("[PASS] sulfur gate: cloud water switches aqueous SO2 -> SO4 on with 1:1 sulfur transfer "
          "(in-cloud gain > cloud-free gain)")
    print("[SKIP] in-cloud pointwise parity vs so4chem: different aqueous parameterization, needs "
          "a climatological oxidant/cloud sounding (ExtData-driven HPC run)")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--runner", required=True, type=Path,
                        help="mkppchem_parity_runner executable (used for both sides)")
    parser.add_argument("--workdir", type=Path, default=None,
                        help="run directories staging root (default: a temp dir under the runner's cwd)")
    parser.add_argument("--source-root", type=Path, default=Path(__file__).resolve().parents[1])
    parser.add_argument("--columns", type=int, default=2)
    parser.add_argument("--levels", type=int, default=64,
                        help="staged profile levels; automatically trimmed to the tallest "
                             "prefix where the derived cloud-water source stays physically valid")
    parser.add_argument("--steps", type=int, default=2)
    parser.add_argument("--dt", type=float, default=60.0)
    parser.add_argument("--cross-runner", type=Path, default=None,
                        help="a second mkppchem_parity_runner executable built for a DIFFERENT "
                             "Kokkos backend; when given, cross-backend parity is asserted")
    parser.add_argument("--memcpy-baseline", type=int, default=None,
                        help="baseline full-state host<->device transfer count over the same "
                             "timestep series (from the so4chem+carbchem run under nsys); used "
                             "only on a device backend to assert zero-copy")
    parser.add_argument("--skip-determinism", action="store_true",
                        help="skip the determinism/reproducibility gate (repeat, column-"
                             "decomposition, and failure-message stability); the gate adds "
                             "five runner invocations")
    parser.add_argument("--cloudy-sulfur", action="store_true",
                        help="exercise the cloud-gated aqueous sulfur pathway with a "
                             "candidate-only gate-activation check: an elevated sulfur seed, "
                             "dry vs host-field cloud water, asserting the SO2 -> SO4 transfer "
                             "switches on with cloud liquid water and conserves sulfur (adds "
                             "two runner invocations)")
    parser.add_argument("--sulfur-steps", type=int, default=24,
                        help="timestep count for the gate-activation runs (long enough for the "
                             "aqueous transfer to be measurable at the elevated seed)")
    parser.add_argument("--sulfur-dt", type=float, default=3600.0,
                        help="timestep length (s) for the gate-activation runs")
    parser.add_argument("--solver-seed", type=float, default=None,
                        help="inject initial_step_seed into the candidate mkppchem settings "
                             "(runtime override of the generated artifact's baked ramp seed)")
    args = parser.parse_args()
    if args.solver_seed is not None and not args.solver_seed > 0.0:
        raise SystemExit("--solver-seed must be a positive fraction of the chemistry interval")
    if args.columns < 1 or args.levels < 2 or args.steps < 1 or args.dt <= 0.0:
        raise SystemExit("columns and steps must be positive; levels must be at least two; dt must be positive")

    workdir = args.workdir if args.workdir else Path(__file__).resolve().parent / "mkppchem_parity_work"
    baseline_dir, candidate_dir = workdir / "baseline", workdir / "candidate"
    baseline_config, baseline_profile = stage_baseline(baseline_dir, args.source_root, args.columns, args.levels)
    candidate_config, candidate_profile = stage_candidate(candidate_dir, args.source_root, args.columns,
                                                          args.levels, args.solver_seed)
    # Both profiles are built from the same sounding, so trimming each to its
    # valid prefix yields the same level count and keeps the two snapshots on
    # one grid (the comparator's shape check).  The derived-cloud-water guard
    # only binds the candidate side, but the baseline must match its extent.
    candidate_levels = trim_profile_to_derivation(candidate_profile)
    baseline_levels = trim_profile_to_derivation(baseline_profile)
    if candidate_levels != baseline_levels:
        raise SystemExit(f"profile trim disagreed: candidate={candidate_levels} baseline={baseline_levels}")
    print(f"staged {candidate_levels} levels (of {args.levels}) for the derived cloud-water source")
    baseline_snapshot = run_side("baseline", args.runner, baseline_dir, baseline_config, baseline_profile,
                                 args.steps, args.dt)
    candidate_snapshot = run_side("candidate", args.runner, candidate_dir, candidate_config, candidate_profile,
                                  args.steps, args.dt)

    comparator = args.source_root / "tests" / "compare_parity_snapshots.py"
    failures = []
    for species in CARBON_SPECIES:
        rtol, atol = TOLERANCES[species]
        baseline_projected = workdir / f"baseline_{species}.json"
        candidate_projected = workdir / f"candidate_{species}.json"
        project_snapshot(baseline_snapshot, (species,), baseline_projected)
        project_snapshot(candidate_snapshot, (species,), candidate_projected)
        result = subprocess.run(
            [sys.executable, str(comparator), str(baseline_projected), str(candidate_projected),
             "--rtol", str(rtol), "--atol", str(atol)],
            check=False, capture_output=True, text=True,
        )
        status = "PASS" if result.returncode == 0 else "FAIL"
        print(f"[{status}] {species}: {result.stdout.strip() or result.stderr.strip()}")
        if result.returncode != 0:
            failures.append(species)
    if failures:
        print(f"PARITY FAILURE: compared species outside tolerance: {', '.join(failures)}", file=sys.stderr)
        return 1
    print("PASS: mkppchem carbon-species parity (bc1, bc2, oc1, oc2) against carbchem+so4chem")

    # Cloud-gated aqueous sulfur: the gate-activation check runs the candidate
    # only (dry vs host-field cloud water at an elevated seed) and confirms the
    # SO2 -> SO4 transfer switches on with cloud liquid water, conserving
    # sulfur.  Deliberately not a pointwise match against so4chem, whose sulfate
    # chemistry is a different parameterization.
    if args.cloudy_sulfur:
        rc = verify_sulfur_gate(args.runner, candidate_dir, candidate_config, candidate_profile,
                                args.sulfur_steps, args.sulfur_dt)
        if rc != 0:
            return rc

    # The zero-copy gate. On a host-resident backend it
    # is structurally satisfied; on a device backend it needs profiler data
    # (honest SKIPPED without it, never a silent pass).
    rc = verify_zero_copy(candidate_snapshot, args.memcpy_baseline)
    if rc != 0:
        return rc

    # The determinism and reproducibility gate. The
    # candidate side (the mkppchem process itself) is what must be repeatable,
    # decomposition-independent, and stable in its failure diagnostics.
    if args.skip_determinism:
        print("[SKIPPED] determinism suite (--skip-determinism)")
    else:
        rc = verify_determinism_suite(args.runner, candidate_config, candidate_profile, candidate_dir,
                                      args.columns, candidate_levels, args.steps, args.dt)
        if rc != 0:
            return rc

    # Cross-backend parity, only when a second backend's runner
    # is supplied. The candidate configuration is reused unchanged so the two
    # backends solve the identical problem; the primary --runner backend is
    # recorded as the baseline producer.
    if args.cross_runner is not None:
        rc = cross_backend_compare(args.runner, candidate_config, candidate_profile, candidate_dir,
                                   args.cross_runner, args.source_root, args.steps, args.dt)
        if rc != 0:
            return rc
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

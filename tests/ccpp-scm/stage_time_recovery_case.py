#!/usr/bin/env python3
"""Stage the leap-day ARM SCM fixture without changing any meteorological data."""

from __future__ import annotations

import argparse
from datetime import datetime
import hashlib
import json
from pathlib import Path
import re
import shutil

import netCDF4
import numpy as np


def stage(source: Path) -> dict:
    """Clone locked ARM input, shift its calendar origin, and verify every variable."""
    base_name = "arm_sgp_summer_1997_A"
    case_name = base_name + "_leap"
    inputs = source / "scm/data/processed_case_input"
    original = inputs / f"{base_name}_SCM_driver.nc"
    destination = inputs / f"{case_name}_SCM_driver.nc"
    temporary = destination.with_suffix(".tmp.nc")
    shutil.copyfile(original, temporary)
    try:
        with netCDF4.Dataset(temporary, "r+") as target:
            if float(target.variables["t0"][0]) != 0.0:
                raise ValueError("clock-shift fixture requires a zero initial-time offset")
            for name in ("t0", "time"):
                target.variables[name].units = "seconds since 2024-02-29 18:00:00"
            # The locked SCM reads these DEPHY global attributes to initialize
            # its calendar; changing variable units alone leaves the old date.
            new_start = datetime(2024, 2, 29, 18)
            old_start = datetime.fromisoformat(target.start_date)
            old_end = datetime.fromisoformat(target.end_date)
            target.start_date = new_start.strftime("%Y-%m-%d %H:%M:%S")
            target.end_date = (new_start + (old_end - old_start)).strftime("%Y-%m-%d %H:%M:%S")
            target.case = case_name
            target.title = "Locked ARM SGP column with leap-day host clock; meteorology unchanged"
        with netCDF4.Dataset(original) as reference, netCDF4.Dataset(temporary) as target:
            if set(reference.variables) != set(target.variables):
                raise ValueError("clock shift changed the case variable inventory")
            for name in reference.variables:
                np.testing.assert_array_equal(reference.variables[name][:], target.variables[name][:],
                                              err_msg=f"clock shift changed {name}")
        temporary.replace(destination)
    finally:
        temporary.unlink(missing_ok=True)
    configs = source / "scm/etc/case_config"
    content = (configs / f"{base_name}.nml").read_text()
    for name, value in (("case_name", f"'{case_name}'"), ("sfc_type", "1"),
                        ("sfc_flux_spec", ".true."), ("sfc_roughness_length_cm", "10.0"),
                        ("n_itt_out", "1"), ("n_itt_diag", "1")):
        pattern = rf"(?im)^(\s*){name}\s*=\s*[^,\n]+"
        if re.search(pattern, content):
            content = re.sub(pattern, rf"\g<1>{name} = {value}", content, count=1)
        else:
            content = re.sub(r"(?m)^\s*/\s*$", f"    {name} = {value},\n/", content, count=1)
    (configs / f"{case_name}.nml").write_text(content)
    return dict(case=case_name, clock_start_utc="2024-02-29T18:00:00Z",
                meteorological_variables="all identical to locked ARM case",
                original_sha256=hashlib.sha256(original.read_bytes()).hexdigest(),
                staged_sha256=hashlib.sha256(destination.read_bytes()).hexdigest())


def main() -> int:
    """Stage the time-recovery input within an already prepared SCM source tree."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source", required=True, type=Path)
    args = parser.parse_args()
    print(json.dumps(stage(args.source.resolve()), indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

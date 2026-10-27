#!/usr/bin/env python3
"""Stage an ARM SGP nighttime clock from the locked SCM meteorology input."""

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


def stage(source: Path) -> dict[str, object]:
    """Copy the ARM SGP forcing, move its initial clock to night, and verify data identity."""
    base_name = "arm_sgp_summer_1997_A"
    case_name = f"{base_name}_night"
    night_start = datetime(1997, 6, 27, 9)
    inputs = source / "scm/data/processed_case_input"
    original = inputs / f"{base_name}_SCM_driver.nc"
    destination = inputs / f"{case_name}_SCM_driver.nc"
    temporary = destination.with_suffix(".tmp.nc")
    if not original.is_file():
        raise FileNotFoundError(f"Locked SCM ARM input is missing: {original}")
    shutil.copyfile(original, temporary)
    try:
        with netCDF4.Dataset(temporary, "r+") as target:
            if float(target.variables["t0"][0]) != 0.0:
                raise ValueError("nighttime fixture requires a zero initial-time offset")
            old_start = datetime.fromisoformat(target.start_date)
            old_end = datetime.fromisoformat(target.end_date)
            target.start_date = night_start.strftime("%Y-%m-%d %H:%M:%S")
            target.end_date = (night_start + (old_end - old_start)).strftime("%Y-%m-%d %H:%M:%S")
            for name in ("t0", "time"):
                target.variables[name].units = f"seconds since {target.start_date}"
            target.case = f"{case_name}_{target.start_date}_{target.variables['lon'][0]:g}E{target.variables['lat'][0]:g}N"
            target.title = "Locked ARM SGP case with nighttime clock; meteorology unchanged"

        with netCDF4.Dataset(original) as reference, netCDF4.Dataset(temporary) as target:
            if set(reference.variables) != set(target.variables):
                raise ValueError("nighttime clock staging changed the case variable inventory")
            for name in reference.variables:
                np.testing.assert_array_equal(reference.variables[name][:], target.variables[name][:],
                                              err_msg=f"nighttime staging changed SCM meteorology variable {name}")
            if reference.start_date == target.start_date:
                raise ValueError("nighttime fixture retained the daytime calendar origin")
        temporary.replace(destination)
    finally:
        temporary.unlink(missing_ok=True)

    configs = source / "scm/etc/case_config"
    content = (configs / f"{base_name}.nml").read_text()
    content, count = re.subn(r"(?im)^(\s*case_name\s*=\s*)'[^']+'",
                             rf"\g<1>'{case_name}'", content, count=1)
    if count != 1:
        raise ValueError("ARM case namelist does not contain exactly one case_name entry")
    (configs / f"{case_name}.nml").write_text(content)

    return {
        "case": case_name,
        "clock_start_utc": night_start.isoformat() + "Z",
        "meteorology": "all SCM variables retained byte-value-equivalent to the locked ARM input",
        "source_sha256": hashlib.sha256(original.read_bytes()).hexdigest(),
        "staged_sha256": hashlib.sha256(destination.read_bytes()).hexdigest(),
    }


def main() -> int:
    """Stage a nighttime SCM input within a prepared, disposable SCM checkout."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source", required=True, type=Path)
    args = parser.parse_args()
    print(json.dumps(stage(args.source.resolve()), indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

#!/usr/bin/env python3
"""Export one GFS v17 CCPP-SCM meteorology record for native process replay.

The profile uses SCM output fields at an exact requested model time and the
corresponding DEPHY forcing record for site coordinates and roughness. It never
falls back to the synthetic Profile_NCWCP sounding. Missing process inputs are
reported in the sidecar manifest so an incomplete profile cannot be mistaken
for a complete same-boundary reference.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import math
from pathlib import Path
from typing import Any


RD = 287.05
EPSILON = 0.622
GRAVITY = 9.80665


def digest(path: Path) -> str:
    """Return a SHA-256 digest for a meteorology input file."""
    value = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            value.update(chunk)
    return value.hexdigest()


def values_1d(variable: Any, time_index: int, time_dim: str, column_dim: str,
              vertical_dim: str) -> list[float]:
    """Read one time/column vector using named NetCDF dimensions."""
    index = tuple(time_index if dim == time_dim else 0 if dim == column_dim else slice(None)
                  for dim in variable.dimensions)
    data = variable[index]
    if getattr(data, "mask", False) is not False:
        import numpy as np
        if np.ma.getmaskarray(data).any():
            raise ValueError(f"SCM field {variable.name} contains masked values at the selected time")
    return [float(item) for item in data.reshape(-1)]


def scalar_at(variable: Any, time_index: int, time_dim: str, column_dim: str) -> float:
    """Read a scalar or one value per time and column from an SCM output."""
    index = tuple(time_index if dim == time_dim else 0 if dim == column_dim else 0
                  for dim in variable.dimensions)
    return float(variable[index])


def case_at(variable: Any, time_index: int, time_dim: str, column_dim: str,
            t0_dim: str) -> float:
    """Read a surface value at a forcing record or from the initial state."""
    index = tuple(time_index if dim == time_dim else 0 if dim in (column_dim, t0_dim) else 0
                  for dim in variable.dimensions)
    return float(variable[index])


def saturation_vapor_pressure_pa(temperature_k: float) -> float:
    """Compute liquid-water saturation vapor pressure with the Bolton formula."""
    temperature_c = temperature_k - 273.15
    return 611.2 * math.exp(17.67 * temperature_c / (temperature_c + 243.5))


def get_field(dataset: Any, names: tuple[str, ...], **kwargs: Any) -> list[float] | None:
    """Read the first available vertical field alias."""
    for name in names:
        if name in dataset.variables:
            variable = dataset.variables[name]
            try:
                field = values_1d(variable, **kwargs)
            except ValueError:
                return None
            if all(math.isfinite(value) for value in field):
                return field
            return None
    return None


def export(output_path: Path, case_path: Path, output_time: float, profile_path: Path,
           process: str) -> dict[str, Any]:
    """Export a time-aligned profile and provenance manifest."""
    try:
        from netCDF4 import Dataset
    except ImportError as exc:
        raise RuntimeError("export requires Python netCDF4") from exc

    with Dataset(output_path) as output, Dataset(case_path) as case:
        out_time = output.variables["time_inst"][:].reshape(-1).filled(float("nan"))
        matching = [i for i, value in enumerate(out_time)
                    if math.isfinite(float(value)) and abs(float(value) - output_time) <= 1.0e-6]
        if len(matching) != 1:
            raise ValueError(f"SCM output must contain exactly one record at {output_time}s; found {matching}")
        case_times = [float(value) for value in case.variables["time"][:].reshape(-1)]
        case_matches = [i for i, value in enumerate(case_times) if abs(value - output_time) <= 1.0e-6]
        if len(case_matches) != 1:
            raise ValueError(f"DEPHY forcing must contain exactly one record at {output_time}s; found {case_matches}")
        out_i, case_i = matching[0], case_matches[0]
        time_dim, col_dim = "time_inst_dim", "hor_dim_layer"
        vertical_dim = "vert_dim_layer"

        def out_profile(name: str) -> list[float]:
            return values_1d(output.variables[name], out_i, time_dim, col_dim, vertical_dim)

        pressure = out_profile("pres")
        pressure_edges = out_profile("pres_i")
        temperature = out_profile("T")
        humidity = out_profile("qv")
        if not (len(pressure) == len(temperature) == len(humidity) and
                len(pressure_edges) == len(pressure) + 1):
            raise ValueError("GFS v17 SCM pressure, temperature, humidity and interface profiles disagree")
        if any(not math.isfinite(x) or x <= 0 for x in pressure + pressure_edges + temperature):
            raise ValueError("GFS v17 SCM profile has a nonphysical pressure or temperature")
        if any((pressure_edges[i] - pressure_edges[i + 1]) <= 0 for i in range(len(pressure_edges) - 1)):
            raise ValueError("GFS v17 SCM interfaces must be ordered from surface to model top")

        height_edges = [0.0]
        for k in range(len(pressure)):
            q = max(0.0, humidity[k])
            virtual_temperature = temperature[k] * (1.0 + 0.61 * q)
            height_edges.append(height_edges[-1] + RD * virtual_temperature / GRAVITY *
                                math.log(pressure_edges[k] / pressure_edges[k + 1]))
        layer_height = [height_edges[k + 1] - height_edges[k] for k in range(len(pressure))]
        density = [p / (RD * t * (1.0 + 0.61 * max(0.0, q)))
                   for p, t, q in zip(pressure, temperature, humidity)]
        dry_density = [p / (RD * t) for p, t in zip(pressure, temperature)]
        relative_humidity = []
        for p, t, q in zip(pressure, temperature, humidity):
            vapor_pressure = p * q / (EPSILON + (1.0 - EPSILON) * q)
            relative_humidity.append(min(1.0, max(0.0, vapor_pressure / saturation_vapor_pressure_pa(t))))

        met_3d: dict[str, list[float]] = {
            "T": temperature,
            "PMID": pressure,
            "DELP": [abs(pressure_edges[i] - pressure_edges[i + 1]) for i in range(len(pressure))],
            "AIRDEN": density,
            "AIRDEN_DRY": dry_density,
            "MAIRDEN": density[:],
            "RH": relative_humidity,
            "QV": humidity,
            "SPHU": humidity[:],
            "BXHEIGHT": layer_height,
        }
        aliases = {"U": ("u",), "V": ("v",), "QL": ("ql",), "QI": ("qi",), "CLDF": ("rad_cloud_fraction",)}
        for target, source_names in aliases.items():
            field = get_field(output, source_names, time_index=out_i, time_dim=time_dim,
                              column_dim=col_dim, vertical_dim=vertical_dim)
            if field is not None and len(field) == len(pressure):
                met_3d[target] = field

        case_time_dim = "time"
        lat = case_at(case.variables["lat"], case_i, case_time_dim, "", "t0")
        lon = case_at(case.variables["lon"], case_i, case_time_dim, "", "t0")
        met_2d: dict[str, list[float]] = {"LAT": [lat], "LON": [lon]}
        output_aliases = {"PS": "pres_s", "TS": "tsfc", "TSKIN": "tsfc", "USTAR": "ustar",
                          "HFLUX": "shf", "PBLH": "hpbl", "SWGDN": "sfc_dwn_sw",
                          "U10M": "u10m", "V10M": "v10m"}
        for target, source in output_aliases.items():
            if source in output.variables:
                met_2d[target] = [scalar_at(output.variables[source], out_i, time_dim, col_dim)]
        if "z0" in case.variables:
            met_2d["Z0"] = [case_at(case.variables["z0"], case_i, case_time_dim, "", "t0")]
        if "ts_forc" in case.variables:
            met_2d.setdefault("TS", [case_at(case.variables["ts_forc"], case_i, case_time_dim, "", "t0")])
        clouds = get_field(output, ("rad_cloud_fraction",), time_index=out_i, time_dim=time_dim,
                           column_dim=col_dim, vertical_dim=vertical_dim)
        if clouds:
            met_2d["CLDFRC"] = [sum(clouds) / len(clouds)]

        required = {
            "settling": ["met_3d/T", "met_3d/TSTEP", "met_3d/AIRDEN", "met_3d/RH", "met_interface/Z",
                         "met_3d/PMID", "met_3d/DELP"],
            "drydep": ["met_3d/BXHEIGHT", "met_2d/CLDFRC", "met_soil/FRLAI", "met_soil/FRLANDUSE",
                       "met_soil/ILAND", "met_2d/LAT", "met_2d/LON", "met_2d/PS", "met_2d/USTAR",
                       "met_2d/SWGDN", "met_2d/TS", "met_2d/TSKIN", "met_2d/Z0"],
        }
        missing = []
        for item in required.get(process, []):
            section, key = item.split("/", 1)
            if section == "met_interface":
                available = key == "Z"
            elif section == "met_3d":
                available = key == "TSTEP" or key in met_3d
            elif section == "met_2d":
                available = key in met_2d
            else:
                available = False
            if not available:
                missing.append(item)

        profile = {
            "schema_version": 1,
            "source": "CCPP-SCM GFS v17 output and DEPHY forcing",
            "process": process,
            "time_seconds": output_time,
            "grid": {"columns": 1, "levels": len(pressure)},
            "met_3d": met_3d,
            "met_interface": {"PEDGE": pressure_edges, "Z": height_edges},
            "met_2d": met_2d,
            "met_soil": {},
            "derived_fields": {
                "AIRDEN": "ideal-gas moist density from SCM pres, T, and qv",
                "AIRDEN_DRY": "ideal-gas dry density from SCM pres and T",
                "RH": "Bolton saturation vapor pressure from SCM pres, T, and qv",
                "Z": "hydrostatic height integrated from SCM pressure interfaces and virtual temperature; not a bitwise copy of host geopotential",
                "DELP": "absolute differences of SCM pressure interfaces",
            },
            "meteorology_completeness": {"status": "complete" if not missing else "incomplete",
                                          "missing_required_fields": missing,
                                          "rule": "No synthetic sounding or unstated surface defaults are inserted.",
                                          "host_fields_reconstructed": ["RH", "AIRDEN", "AIRDEN_DRY", "DELP", "Z"]},
        }
        profile_path.parent.mkdir(parents=True, exist_ok=True)
        profile_path.write_text(json.dumps(profile, indent=2, allow_nan=False) + "\n", encoding="utf-8")
        return {
            "schema_version": 1,
            "status": profile["meteorology_completeness"]["status"],
            "process": process,
            "time_seconds": output_time,
            "levels": len(pressure),
            "scm_output": {"path": str(output_path), "sha256": digest(output_path), "record_index": out_i},
            "dephy_case": {"path": str(case_path), "sha256": digest(case_path), "record_index": case_i},
            "profile": str(profile_path),
            "missing_required_fields": missing,
            "limitations": ["Profile contains meteorology only; incoming constituent state is captured separately.",
                            "The requested time must exist in both the SCM output and DEPHY forcing."],
        }


def main() -> int:
    """Export profile JSON and a machine-readable provenance report."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--scm-output", type=Path, required=True)
    parser.add_argument("--case-input", type=Path, required=True)
    parser.add_argument("--time-seconds", type=float, required=True)
    parser.add_argument("--process", choices=("settling", "drydep"), required=True)
    parser.add_argument("--output", type=Path, required=True, help="native runner met-profile JSON")
    parser.add_argument("--manifest", type=Path, help="defaults to OUTPUT.manifest.json")
    args = parser.parse_args()
    manifest_path = args.manifest or args.output.with_suffix(".manifest.json")
    report = export(args.scm_output, args.case_input, args.time_seconds, args.output, args.process)
    manifest_path.parent.mkdir(parents=True, exist_ok=True)
    manifest_path.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    print(json.dumps(report, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

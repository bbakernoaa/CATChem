#!/usr/bin/env python3
"""Export one exact CATChem process meteorology block as a legacy runner profile."""

from __future__ import annotations

import argparse
from datetime import datetime
import json
from pathlib import Path
from typing import Any


LEGACY_REQUIRED = {
    "settling": {"met_3d": ("T", "AIRDEN", "RH", "PMID", "DELP"),
                 "met_interface": ("Z",)},
    "drydep": {"met_3d": ("PMID", "T", "QV", "AIRDEN", "CLDF"),
                "met_interface": ("PEDGE", "Z"),
                "met_2d": ("LAT", "LON", "PS", "TS", "USTAR", "HFLUX", "PBLH", "DLUSE",
                           "LAI", "FRSNO", "SWGDN", "Z0", "FRLAKE", "GWETTOP", "LWI",
                           "U10M", "V10M"),
                "met_soil": ()},
    "wetdep": {"met_3d": ("AIRDEN", "DELP", "T", "PMID", "QV", "AIRDEN_DRY"),
                "met_interface": ("PEDGE", "PFILSAN", "PFLLSAN"),
                "met_2d": ("PRECCON", "PRECLSC"),
                "met_soil": ()},
    "so4chem": {"met_3d": ("AIRDEN", "DELP", "PMID", "T", "CLDF"),
                 "met_interface": ("PEDGE", "Z"),
                 "met_2d": ("HFLUX", "LAT", "LON", "PBLH", "USTAR", "U10M", "V10M", "LWI", "Z0")},
    "photolysis": {"met_3d": ("AIRDEN", "BXHEIGHT", "DELP", "PMID", "T", "CLDF"),
                   "met_interface": ("PEDGE", "Z"),
                   "met_2d": ("LAT", "LON", "PBLH", "USTAR", "U10M", "V10M", "LWI", "Z0")},
    "gaschem": {"met_3d": ("AIRDEN", "BXHEIGHT", "DELP", "PMID", "T", "CLDF"),
                "met_interface": ("PEDGE", "Z"),
                "met_2d": ("HFLUX", "LAT", "LON", "PBLH", "USTAR", "U10M", "V10M", "LWI", "Z0")},
}


def load_blocks(ledger_path: Path) -> list[dict[str, Any]]:
    """Read process blocks from the run_case JSON Lines meteorology ledger."""
    return [json.loads(line) for line in ledger_path.read_text(encoding="utf-8").splitlines() if line.strip()]


def make_profile(block: dict[str, Any], ledger_path: Path, sequence: int,
                 start_datetime: datetime | None) -> tuple[dict[str, Any], dict[str, Any]]:
    """Convert captured adapter fields into the native runner's profile structure."""
    categories: dict[str, dict[str, list[float]]] = {
        "met_2d": {}, "met_3d": {}, "met_interface": {}, "met_soil": {},
    }
    column_counts = {field["columns"] for field in block["fields"].values()}
    if column_counts != {1}:
        raise ValueError(f"native SCM profile export currently requires one column; received {column_counts}")
    vertical_sizes: list[int] = []
    missing: list[str] = []
    for name, field in block["fields"].items():
        values = field["values"]
        axis = field["axis"]
        invalid_indexes = list(field.get("invalid_value_indexes", field.get("nonfinite_value_indexes", [])))
        if invalid_indexes or any(value is None for value in values):
            missing.append(f"{name} contains invalid (non-finite or fill) values at flattened indexes {invalid_indexes}")
        if field["rank"] == 1 or axis == 0:
            category = "met_2d"
        elif axis == 2:
            category = "met_soil"
        elif name in {"Z", "PEDGE", "PFILSAN", "PFLLSAN"}:
            category = "met_interface"
        else:
            category = "met_3d"
            vertical_sizes.append(field["levels"])
        if category in categories:
            categories[category][name] = values
    levels = max(vertical_sizes, default=0)
    process = block["process"]
    required = LEGACY_REQUIRED.get(process)
    if required:
        for category, fields in required.items():
            missing.extend(f"{category}/{name}" for name in fields if name not in categories[category])
    else:
        missing.append(f"native runner has no registration for {process}")
    profile = {
        "schema_version": 1,
        "source": "CCPP CATChem adapter meteorology ledger",
        "process": process,
        "interval_token": block["interval_token"],
        "subcycle": block["subcycle"],
        "duration_seconds": block["duration_seconds"],
        "grid": {"columns": 1, "levels": levels},
        **categories,
        "meteorology_completeness": {
            "status": "complete" if not missing else "incomplete",
            "missing_required_fields": missing,
            "rule": "Only successfully bound process inputs are included; CATChem derives the same secondary fields during replay.",
        },
    }
    if start_datetime is not None:
        profile["clock"] = {
            "year": start_datetime.year,
            "month": start_datetime.month,
            "day": start_datetime.day,
            "hour": start_datetime.hour,
            "minute": start_datetime.minute,
            "second": start_datetime.second,
            "day_of_year": int(start_datetime.strftime("%j")),
        }
    manifest = {
        "schema_version": 1,
        "process": process,
        "sequence": sequence,
        "interval_token": block["interval_token"],
        "subcycle": block["subcycle"],
        "duration_seconds": block["duration_seconds"],
        "ledger": str(ledger_path),
        "captured_fields": sorted(block["fields"]),
        "missing_required_fields": missing,
        "status": profile["meteorology_completeness"]["status"],
        "clock": profile.get("clock"),
        "limitation": "This file contains process meteorology only; matching constituent-state seeding remains required.",
    }
    return profile, manifest


def main() -> int:
    """Select a process call and write its meteorology profile and provenance manifest."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--ledger", type=Path, required=True)
    parser.add_argument("--process", required=True)
    parser.add_argument("--sequence", type=int, default=0,
                        help="zero-based occurrence of PROCESS in the ledger")
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--start-datetime", type=datetime.fromisoformat,
                        help="case start time as YYYY-MM-DDTHH:MM:SS for solar-geometry replay")
    args = parser.parse_args()
    matches = [row for row in load_blocks(args.ledger) if row["process"] == args.process]
    if args.sequence < 0 or args.sequence >= len(matches):
        parser.error(f"{args.process} sequence {args.sequence} is unavailable; found {len(matches)} calls")
    profile, manifest = make_profile(matches[args.sequence], args.ledger, args.sequence, args.start_datetime)
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(profile, indent=2, allow_nan=False) + "\n", encoding="utf-8")
    args.output.with_suffix(".manifest.json").write_text(json.dumps(manifest, indent=2) + "\n", encoding="utf-8")
    print(json.dumps(manifest, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

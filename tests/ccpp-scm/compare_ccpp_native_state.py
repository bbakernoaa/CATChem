#!/usr/bin/env python3
"""Compare a native process snapshot with the matching SCM host-update boundary."""

from __future__ import annotations

import argparse
import hashlib
import json
import math
from pathlib import Path
from typing import Any


def compare(evidence_path: Path, snapshot_path: Path, interval: int, subcycle: int,
            relative_tolerance: float) -> dict[str, Any]:
    """Compare all matching species and levels against one SCM after-state."""
    evidence = json.loads(evidence_path.read_text(encoding="utf-8"))
    snapshot = json.loads(snapshot_path.read_text(encoding="utf-8"))
    fields = snapshot["snapshots"][0]["fields"]
    concentration = fields["concentration"]
    shape = concentration["shape"]
    if len(shape) != 3 or shape[0] != 1:
        raise ValueError(f"expected native concentration shape [1, levels, species], got {shape}")
    levels = int(shape[1])
    native_species = [str(name) for name in concentration["species"]]
    native_values = concentration["values"]
    if len(native_values) != levels * len(native_species):
        raise ValueError("native concentration values do not match snapshot shape")
    rows = [row for row in evidence.get("boundary_state_ledger", [])
            if row["interval_token"] == interval and row["subcycle"] == subcycle]
    reference = {(str(row["species"]), int(row["level"])): float(row["after"]) for row in rows}
    if len(reference) != len(rows):
        raise ValueError("duplicate SCM boundary values found")
    compared = 0
    max_absolute = 0.0
    max_relative = 0.0
    max_absolute_at = None
    max_relative_at = None
    for level in range(1, levels + 1):
        for species_index, species in enumerate(native_species):
            key = (species, level)
            if key not in reference:
                raise ValueError(f"SCM boundary has no reference for native species/level {key}")
            actual = float(native_values[(level - 1) * len(native_species) + species_index])
            expected = reference[key]
            if not math.isfinite(actual) or not math.isfinite(expected):
                raise ValueError(f"non-finite state value for {key}")
            absolute = abs(actual - expected)
            relative = absolute / max(abs(actual), abs(expected), 1e-30)
            compared += 1
            if absolute > max_absolute:
                max_absolute = absolute
                max_absolute_at = [species, level, actual, expected]
            if relative > max_relative:
                max_relative = relative
                max_relative_at = [species, level, actual, expected]
    expected_count = len(reference)
    if compared != expected_count:
        raise ValueError(f"compared {compared} native values, but SCM boundary has {expected_count}")
    return {
        "schema_version": 1,
        "process": evidence["process"],
        "case": evidence["case"],
        "interval_token": interval,
        "subcycle": subcycle,
        "duration_seconds": evidence.get("host_update_ledger", [{}])[subcycle - 1].get("duration_seconds"),
        "native_snapshot_checkpoint": snapshot["snapshots"][0].get("checkpoint"),
        "compared_state_values": compared,
        "max_absolute_error": max_absolute,
        "max_absolute_error_at": max_absolute_at,
        "max_relative_error": max_relative,
        "max_relative_error_at": max_relative_at,
        "relative_tolerance": relative_tolerance,
        "status": "passed" if max_relative <= relative_tolerance else "failed",
        "sha256": {
            "scm_evidence": hashlib.sha256(evidence_path.read_bytes()).hexdigest(),
            "native_snapshot": hashlib.sha256(snapshot_path.read_bytes()).hexdigest(),
        },
    }


def main() -> int:
    """Parse evidence paths, run the state comparison, and write a JSON report."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--evidence", type=Path, required=True)
    parser.add_argument("--snapshot", type=Path, required=True)
    parser.add_argument("--interval", type=int, default=1)
    parser.add_argument("--subcycle", type=int, default=1)
    parser.add_argument("--relative-tolerance", type=float, default=1e-6)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    result = compare(args.evidence, args.snapshot, args.interval, args.subcycle,
                     args.relative_tolerance)
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(result, indent=2) + "\n", encoding="utf-8")
    print(json.dumps(result, indent=2))
    return 0 if result["status"] == "passed" else 1


if __name__ == "__main__":
    raise SystemExit(main())

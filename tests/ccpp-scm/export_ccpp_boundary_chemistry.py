#!/usr/bin/env python3
"""Export a single SCM host-boundary constituent state for native process replay."""

from __future__ import annotations

import argparse
import hashlib
import json
import math
from pathlib import Path
from typing import Any


def make_profile(evidence_path: Path, interval: int, subcycle: int) -> dict[str, Any]:
    """Build a level-major chemistry seed from the pre-update mapped CCPP state."""
    evidence = json.loads(evidence_path.read_text(encoding="utf-8"))
    rows = [row for row in evidence.get("boundary_state_ledger", [])
            if row["interval_token"] == interval and row["subcycle"] == subcycle]
    if not rows:
        raise ValueError(f"no boundary-state rows for interval {interval}, subcycle {subcycle}")
    if any(int(row["column"]) != 1 for row in rows):
        raise ValueError("native replay currently supports a single SCM column")
    species = sorted({str(row["species"]) for row in rows})
    levels = sorted({int(row["level"]) for row in rows})
    if levels != list(range(1, len(levels) + 1)):
        raise ValueError("boundary state levels are not contiguous from 1")
    values_by_cell: dict[tuple[int, str], float] = {}
    for row in rows:
        key = (int(row["level"]), str(row["species"]))
        if key in values_by_cell:
            raise ValueError(f"duplicate boundary-state value for {key}")
        value = float(row["before"])
        if not math.isfinite(value):
            raise ValueError(f"non-finite CCPP state for {key}")
        values_by_cell[key] = value
    expected = len(levels) * len(species)
    if len(values_by_cell) != expected:
        raise ValueError(f"incomplete CCPP state: expected {expected} values, got {len(values_by_cell)}")
    return {
        "schema_version": 1,
        "source": "pre-update CCPP host constituent state from CCPP-SCM",
        "process": evidence["process"],
        "case": evidence["case"],
        "interval_token": interval,
        "subcycle": subcycle,
        "grid": {"columns": 1, "levels": len(levels)},
        "chemistry": {
            "units": "CCPP host constituent units from the registered species manifest",
            "species": species,
            "values": [values_by_cell[(level, name)] for level in levels for name in species],
        },
    }


def main() -> int:
    """Select a captured SCM boundary and write the native runner chemistry profile."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--evidence", type=Path, required=True,
                        help="run_case evidence.json produced with --emit-boundary-ledger")
    parser.add_argument("--interval", type=int, default=1)
    parser.add_argument("--subcycle", type=int, default=1)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    profile = make_profile(args.evidence, args.interval, args.subcycle)
    args.output.parent.mkdir(parents=True, exist_ok=True)
    encoded = (json.dumps(profile, indent=2, allow_nan=False) + "\n").encode()
    args.output.write_bytes(encoded)
    manifest = {
        "schema_version": 1,
        "source_evidence": str(args.evidence),
        "source_evidence_sha256": hashlib.sha256(args.evidence.read_bytes()).hexdigest(),
        "output_sha256": hashlib.sha256(encoded).hexdigest(),
        "process": profile["process"],
        "case": profile["case"],
        "interval_token": profile["interval_token"],
        "subcycle": profile["subcycle"],
        "columns": profile["grid"]["columns"],
        "levels": profile["grid"]["levels"],
        "species_count": len(profile["chemistry"]["species"]),
        "state_boundary": "before successful CATChem host tendency application",
    }
    args.output.with_suffix(".manifest.json").write_text(json.dumps(manifest, indent=2) + "\n",
                                                         encoding="utf-8")
    print(json.dumps(manifest, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

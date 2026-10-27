#!/usr/bin/env python3
"""Compare native Photolysis reaction-rate metrics with a CCPP-SCM boundary."""

from __future__ import annotations

import argparse
import hashlib
import json
import math
from pathlib import Path
from typing import Any


def compare(evidence_path: Path, snapshot_path: Path, relative_tolerance: float,
            absolute_tolerance: float) -> dict[str, Any]:
    """Compare every reaction's full-profile max and sum metrics."""
    evidence = json.loads(evidence_path.read_text(encoding="utf-8"))
    snapshot = json.loads(snapshot_path.read_text(encoding="utf-8"))
    native = snapshot["snapshots"][0]["fields"].get("diagnostics", {})
    reference = {
        item["name"]: (float(item["max_abs"]), float(item["sum"]))
        for item in evidence.get("diagnostic_summaries", [])
        if item["name"].startswith("photolysis_rate_")
    }
    native_names = set(native)
    reference_names = set(reference)
    if native_names != reference_names:
        raise ValueError(
            f"photolysis diagnostic sets differ: native-only={sorted(native_names-reference_names)}, "
            f"CCPP-only={sorted(reference_names-native_names)}"
        )
    compared = 0
    max_relative_error = 0.0
    max_absolute_error = 0.0
    worst: dict[str, Any] | None = None
    for name, (expected_max, expected_sum) in reference.items():
        field = native[name]
        if field.get("units") != "s-1":
            raise ValueError(f"unexpected units for {name}: {field.get('units')}")
        values = [float(value) for value in field["values"]]
        if not values or not all(math.isfinite(value) for value in values):
            raise ValueError(f"empty or non-finite native rate profile for {name}")
        actuals = (("max_abs", max(abs(value) for value in values), expected_max),
                   ("sum", sum(values), expected_sum))
        for metric, actual, expected in actuals:
            absolute_error = abs(actual - expected)
            relative_error = absolute_error / max(abs(actual), abs(expected), 1.0e-300)
            if absolute_error > max_absolute_error:
                max_absolute_error = absolute_error
            if relative_error > max_relative_error:
                max_relative_error = relative_error
                worst = {"reaction": name, "metric": metric, "native": actual,
                         "ccpp": expected, "absolute_error": absolute_error,
                         "relative_error": relative_error}
            if absolute_error > absolute_tolerance + relative_tolerance * abs(expected):
                raise ValueError(
                    f"{name} {metric} exceeds tolerance: native={actual}, CCPP={expected}, "
                    f"absolute_error={absolute_error}"
                )
            compared += 1
    return {
        "schema_version": 1,
        "process": "photolysis",
        "case": evidence["case"],
        "interval_token": 1,
        "rate_profiles": len(reference),
        "compared_metrics": compared,
        "metrics": ["max_abs", "sum"],
        "relative_tolerance": relative_tolerance,
        "absolute_tolerance_s-1": absolute_tolerance,
        "max_absolute_error": max_absolute_error,
        "max_relative_error": max_relative_error,
        "worst": worst,
        "status": "passed",
        "sha256": {
            "scm_evidence": hashlib.sha256(evidence_path.read_bytes()).hexdigest(),
            "native_snapshot": hashlib.sha256(snapshot_path.read_bytes()).hexdigest(),
        },
    }


def main() -> int:
    """Compare one CCPP-SCM evidence file with a native rate snapshot."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--evidence", type=Path, required=True)
    parser.add_argument("--snapshot", type=Path, required=True)
    parser.add_argument("--relative-tolerance", type=float, required=True)
    parser.add_argument("--absolute-tolerance", type=float, required=True,
                        help="absolute rate tolerance in s-1")
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    result = compare(args.evidence, args.snapshot, args.relative_tolerance,
                     args.absolute_tolerance)
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(result, indent=2) + "\n", encoding="utf-8")
    print(json.dumps(result, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

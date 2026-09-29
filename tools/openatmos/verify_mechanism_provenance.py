#!/usr/bin/env python3
"""CI provenance verifier for the mkppchem single-source-of-truth contract.

The OpenAtmos definition is the authority; two derived artifacts must agree with
it, and each other:

  * the compiled MKPP artifact (``mkpp-generated/<mech>.hpp``), which the solver
    is generated from, and
  * the build-time metadata header (``catchem_mkpp_mechanism_meta.hpp``, one
    aggregated ``Facts`` struct per mechanism), which
    the process ``#include``s instead of parsing YAML at runtime.

This tool re-parses the pinned definition and cross-checks the structural
facts:

  - species count: definition == artifact ``NUM_SPECIES`` == header ``num_species``
  - species name set (case-insensitive): definition == artifact ``Species`` enum
    == header ``species_order``
  - species slot ORDER: definition declaration order == artifact enum order ==
    header ``species_order`` order (the solver indexes state positionally)
  - photolysis count: definition == artifact ``NUM_PHOTOLYSIS`` == header
    ``num_photolysis``
  - photolysis label ORDER: definition == header ``photolysis_labels``
  - reaction count: definition == header ``num_reactions``
  - mechanism id: definition metadata == header ``mechanism_id``
  - provenance digest: definition metadata == header ``legacy_source_sha256``
  - cloud-gate flag: definition trigger == header ``has_cloud_gated_reaction``
  - equilibrium channel indices: definition-derived (the generator's own rule)
    == header ``env_*`` == artifact ``EquilibriumInput::COUNT``
  - R1 guard: a cloud-gated definition requires the artifact to expose a third
    ``EquilibriumInput`` (``COUNT >= 3``)

The metadata header is a single aggregated file holding one ``Facts`` struct
per mechanism, so a member is read only from within the named struct's body
(``struct <Mechanism>Facts { ... }``) -- never from the whole file, which would
let one mechanism's counts satisfy another's check.

Every mismatch is fatal and names BOTH sides (the definition path and the
artifact/header path) so a maintainer can tell which one is stale. A non-zero
exit means the checked-in artifacts have drifted from the definition.

Usage:
    python3 tools/openatmos/verify_mechanism_provenance.py \
        --definition parm/config/openatmos/gocart/mechanism.yaml \
        --artifact src/external/mkpp/mkpp-generated/gocart.hpp \
        --meta-header src/process/mkppchem/catchem_mkpp_mechanism_meta.hpp

The struct to read is named from the definition's ``mkpp_mechanism_id`` (e.g.
'gocart' -> 'GocartFacts'), so the same aggregated header serves every
mechanism. Pass it once per mechanism you want checked.
"""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

try:
    import yaml
except ImportError as exc:  # pragma: no cover
    sys.exit(f"FATAL ERROR: verify_mechanism_provenance requires PyYAML: {exc}")

# Reuse the generator's definition-derived equilibrium-channel rule verbatim
# so the verifier's expectation and the header's baked value cannot drift
# apart: both read the same function, and any disagreement with the compiled
# artifact is reported as drift, not as a difference of interpretation.
sys.path.insert(0, str(Path(__file__).resolve().parent))
from generate_catchem_species import env_indices  # noqa: E402


class Drift(Exception):
    """A provenance mismatch; the message names both offending sides."""


def _fail(msg: str) -> "NoReturn":  # type: ignore[name-defined]
    raise SystemExit(f"FATAL ERROR: verify_mechanism_provenance: {msg}")


# --------------------------------------------------------------------------- #
# Definition (YAML, source of truth)
# --------------------------------------------------------------------------- #
def parse_definition(path: Path) -> dict:
    try:
        with path.open() as handle:
            data = yaml.safe_load(handle)
    except (OSError, yaml.YAMLError) as exc:
        _fail(f"cannot read/parse definition '{path}': {exc}")
    if not isinstance(data, dict):
        _fail(f"definition '{path}' did not parse to a mapping")

    species = [str(s["name"]) for s in (data.get("species") or []) if isinstance(s, dict) and s.get("name")]
    labels = [str(r["name"]) for r in (data.get("reactions") or [])
              if isinstance(r, dict) and r.get("type") == "PHOTOLYSIS" and r.get("name")]
    num_reactions = len(data.get("reactions") or [])
    cloud_gated = any(
        isinstance(r, dict)
        and isinstance(r.get("activation_trigger"), str)
        and "cloud_liquid_water" in r["activation_trigger"].lower()
        for r in (data.get("reactions") or [])
    )
    metadata = data.get("metadata") or {}
    forcing_map = metadata.get("photolysis_host_forcing_expressions") or {}
    forcing = [float(forcing_map.get(label, 0.0)) for label in labels]
    env_t, env_rh, env_cw, env_count = env_indices(data, cloud_gated)
    return {
        "path": str(path),
        "species": species,
        "photolysis_labels": labels,
        "photolysis_host_forcing": forcing,
        "num_reactions": num_reactions,
        "cloud_gated": cloud_gated,
        "mechanism_id": str(metadata.get("mkpp_mechanism_id", "")),
        "sha256": str(metadata.get("legacy_source_sha256", "")),
        "env_temperature": env_t,
        "env_humidity": env_rh,
        "env_cloud_water": env_cw,
        "env_count": env_count,
    }


# --------------------------------------------------------------------------- #
# Compiled artifact (generated C++ header)
# --------------------------------------------------------------------------- #
def parse_artifact(path: Path) -> dict:
    try:
        text = path.read_text()
    except OSError as exc:
        _fail(f"cannot read artifact '{path}': {exc}")

    species_block = re.search(r"enum\s+Species\s*:\s*int\s*\{(.*?)\}", text, re.S)
    if not species_block:
        _fail(f"artifact '{path}' has no 'enum Species' block to read slot names from")
    species = re.findall(r"([A-Za-z_]\w*)\s*=\s*\d+", species_block.group(1))

    num_species = _const_int(text, "NUM_SPECIES", path)
    num_photolysis = _const_int(text, "NUM_PHOTOLYSIS", path)

    eq_block = re.search(r"enum\s+class\s+EquilibriumInput\s*:\s*int\s*\{(.*?)\}", text, re.S)
    eq_count = 0
    if eq_block:
        entries = re.findall(r"([A-Za-z_]\w*)\s*=\s*(\d+)", eq_block.group(1))
        count_decl = next((int(v) for k, v in entries if k == "COUNT"), None)
        eq_count = count_decl if count_decl is not None else len(entries)

    return {
        "path": str(path),
        "species": species,
        "num_species": num_species,
        "num_photolysis": num_photolysis,
        "equilibrium_input_count": eq_count,
    }


def _const_int(text: str, name: str, path: Path) -> int:
    m = re.search(rf"\b{name}\b\s*=\s*(\d+)", text)
    if not m:
        _fail(f"artifact '{path}' does not define {name}")
    return int(m.group(1))


# --------------------------------------------------------------------------- #
# Build-time metadata header (generated by generate_catchem_species.py)
# --------------------------------------------------------------------------- #
def parse_meta_header(path: Path, struct_name: str) -> dict:
    """Read one mechanism's Facts struct out of the aggregated header.

    The header holds every mechanism's struct, so the member regexes run only
    inside the named struct's body -- reading the whole file would let a
    different mechanism's counts satisfy this one's checks.
    """
    try:
        text = path.read_text()
    except OSError as exc:
        _fail(f"cannot read meta header '{path}': {exc}")

    body = _struct_body(text, struct_name, path)

    def str_view(name: str) -> str:
        m = re.search(rf"std::string_view\s+{name}\s*=\s*\"((?:[^\"\\]|\\.)*)\"", body)
        return m.group(1) if m else ""

    species = _string_array(body, "species_order", path)
    labels = _string_array(body, "photolysis_labels", path, allow_empty=True)
    forcing = _double_array(body, "photolysis_host_forcing", path, allow_empty=not labels)
    num_species = _header_int(body, "num_species", path)
    num_photolysis = _header_int(body, "num_photolysis", path)
    num_reactions = _header_int(body, "num_reactions", path)
    cloud = re.search(r"bool\s+has_cloud_gated_reaction\s*=\s*(true|false)", body)
    env = {
        "env_temperature": _header_signed_int(body, "env_temperature", path),
        "env_humidity": _header_signed_int(body, "env_humidity", path),
        "env_cloud_water": _header_signed_int(body, "env_cloud_water", path),
        "env_count": _header_signed_int(body, "env_count", path),
    }
    return {
        "path": str(path),
        "struct_name": struct_name,
        "mechanism_id": str_view("mechanism_id"),
        "sha256": str_view("legacy_source_sha256"),
        "species": species,
        "photolysis_labels": labels,
        "photolysis_host_forcing": forcing,
        "num_species": num_species,
        "num_photolysis": num_photolysis,
        "num_reactions": num_reactions,
        "cloud_gated": (cloud.group(1) == "true") if cloud else None,
        **env,
    }


def _struct_body(text: str, struct_name: str, path: Path) -> str:
    """Return the body of ``struct <struct_name> { ... };`` (brace-matched).

    Brace matching is required because the struct contains nested initializer
    braces (the std::array members); a non-greedy ``.*?};`` would stop at the
    first array's closing brace and silently truncate the struct.
    """
    header = re.search(rf"struct\s+{re.escape(struct_name)}\s*\{{", text)
    if not header:
        _fail(f"meta header '{path}' has no 'struct {struct_name}' block")
    i = header.end() - 1  # at the opening brace
    depth = 0
    for j in range(i, len(text)):
        if text[j] == "{":
            depth += 1
        elif text[j] == "}":
            depth -= 1
            if depth == 0:
                return text[i + 1 : j]
    _fail(f"meta header '{path}': 'struct {struct_name}' is not brace-balanced")


def _header_int(text: str, name: str, path: Path) -> int:
    m = re.search(rf"int\s+{name}\s*=\s*(\d+)", text)
    if not m:
        _fail(f"meta header '{path}' does not define {name}")
    return int(m.group(1))


def _header_signed_int(text: str, name: str, path: Path) -> int:
    """Read an ``int`` member that may legitimately be negative (-1 sentinel).

    The equilibrium channel indices use -1 for a channel the solver does not
    accept, so unlike ``_header_int`` this must capture a leading minus.
    """
    m = re.search(rf"int\s+{name}\s*=\s*(-?\d+)", text)
    if not m:
        _fail(f"meta header '{path}' does not define {name}")
    return int(m.group(1))


def _string_array(text: str, name: str, path: Path, allow_empty: bool = False) -> list[str]:
    m = re.search(rf"std::array<std::string_view,\s*\d+>\s+{name}\s*=\s*\{{(.*?)\}};", text, re.S)
    if not m:
        if allow_empty and re.search(rf"int\s+num_photolysis\s*=\s*0", text):
            return []
        _fail(f"meta header '{path}' does not define {name}")
    body = re.sub(r"//[^\n]*", "", m.group(1))  # drop the "(none)" placeholder comment
    return re.findall(r'"((?:[^"\\]|\\.)*)"', body)


def _double_array(text: str, name: str, path: Path, allow_empty: bool = False) -> list[float]:
    m = re.search(rf"std::array<double,\s*\d+>\s+{name}\s*=\s*\{{(.*?)\}};", text, re.S)
    if not m:
        if allow_empty:
            return []
        _fail(f"meta header '{path}' does not define {name}")
    body = re.sub(r"//[^\n]*", "", m.group(1))
    return [float(v) for v in re.findall(r"[-+0-9.eE]+", body)]


# --------------------------------------------------------------------------- #
# Cross-checks
# --------------------------------------------------------------------------- #
def _upper(names: list[str]) -> set[str]:
    return {n.upper() for n in names}


def verify(defn: dict, art: dict, hdr: dict) -> list[str]:
    problems: list[str] = []

    # Species count across all three.
    counts = {"definition": len(defn["species"]), "artifact NUM_SPECIES": art["num_species"],
              "meta header num_species": hdr["num_species"], "artifact Species enum": len(art["species"]),
              "meta header species_order": len(hdr["species"])}
    if len(set(counts.values())) != 1:
        problems.append(
            f"species count mismatch: {_fmt(counts)} "
            f"(definition '{defn['path']}', artifact '{art['path']}', header '{hdr['path']}')"
        )

    # Species name set (case-insensitive) across all three.
    name_sets = {"definition": _upper(defn["species"]), "artifact": _upper(art["species"]),
                 "meta header": _upper(hdr["species"])}
    if len({frozenset(s) for s in name_sets.values()}) != 1:
        problems.append(
            "species name set mismatch (case-insensitive): "
            + "; ".join(f"{k}={sorted(v)}" for k, v in name_sets.items())
            + f" (definition '{defn['path']}', artifact '{art['path']}')"
        )

    # Species slot ORDER: definition vs artifact enum vs header array.
    if defn["species"] != art["species"]:
        problems.append(
            f"species ORDER differs between definition and artifact enum: "
            f"definition={defn['species']} artifact={art['species']} "
            f"(definition '{defn['path']}', artifact '{art['path']}')"
        )
    if defn["species"] != hdr["species"]:
        problems.append(
            f"species ORDER differs between definition and meta header: "
            f"definition={defn['species']} header={hdr['species']} "
            f"(definition '{defn['path']}', header '{hdr['path']}')"
        )

    # Photolysis count.
    pcounts = {"definition": len(defn["photolysis_labels"]), "artifact NUM_PHOTOLYSIS": art["num_photolysis"],
               "meta header num_photolysis": hdr["num_photolysis"]}
    if len(set(pcounts.values())) != 1:
        problems.append(
            f"photolysis count mismatch: {_fmt(pcounts)} "
            f"(definition '{defn['path']}', artifact '{art['path']}')"
        )

    # Photolysis label ORDER: definition vs header (the artifact exposes no
    # label data yet -- a follow-up -- so the header is the checked copy).
    if defn["photolysis_labels"] != hdr["photolysis_labels"]:
        problems.append(
            f"photolysis label ORDER differs: definition={defn['photolysis_labels']} "
            f"header={hdr['photolysis_labels']} (definition '{defn['path']}', header '{hdr['path']}')"
        )

    # Host-forcing constants (smoke mode only): definition expressions must
    # match the header array, in label order. Guards the single-source contract
    # for the fallback path.
    if defn["photolysis_host_forcing"] != hdr["photolysis_host_forcing"]:
        problems.append(
            f"photolysis host forcing differs: definition={defn['photolysis_host_forcing']} "
            f"header={hdr['photolysis_host_forcing']} (definition '{defn['path']}', header '{hdr['path']}')"
        )

    # Mechanism id.
    if defn["mechanism_id"] and defn["mechanism_id"] != hdr["mechanism_id"]:
        problems.append(
            f"mechanism id mismatch: definition='{defn['mechanism_id']}' "
            f"header='{hdr['mechanism_id']}' (definition '{defn['path']}', header '{hdr['path']}')"
        )

    # Provenance digest (only when the definition carries one).
    if defn["sha256"] and defn["sha256"] != hdr["sha256"]:
        problems.append(
            f"provenance digest mismatch: definition='{defn['sha256']}' "
            f"header='{hdr['sha256']}' (definition '{defn['path']}', header '{hdr['path']}')"
        )

    # Reaction count: definition vs header (the compiled artifact exposes no
    # reaction-count constant, so the header is the checked copy -- same as the
    # photolysis labels).
    if defn["num_reactions"] != hdr["num_reactions"]:
        problems.append(
            f"reaction count mismatch: definition={defn['num_reactions']} "
            f"header={hdr['num_reactions']} (definition '{defn['path']}', header '{hdr['path']}')"
        )

    # Cloud-gate flag.
    if defn["cloud_gated"] != hdr["cloud_gated"]:
        problems.append(
            f"cloud-gate flag mismatch: definition={defn['cloud_gated']} "
            f"header={hdr['cloud_gated']} (definition '{defn['path']}', header '{hdr['path']}')"
        )

    # Equilibrium channel indices: the definition-derived values (the same
    # rule the generator baked) must equal the header's baked env_* members,
    # and the header's env_count must equal the artifact's EquilibriumInput::
    # COUNT. This locks the invariant the generic MechanismTraits relies on:
    # a solver's env-channel indices are a build fact, not a hand-written
    # guess, so a stale header or a re-emitted artifact fails CI here.
    env_def = (defn["env_temperature"], defn["env_humidity"], defn["env_cloud_water"], defn["env_count"])
    env_hdr = (hdr["env_temperature"], hdr["env_humidity"], hdr["env_cloud_water"], hdr["env_count"])
    if env_def != env_hdr:
        problems.append(
            f"equilibrium channel indices differ: definition-derived={env_def} "
            f"header={env_hdr} (definition '{defn['path']}', header '{hdr['path']}')"
        )
    if hdr["env_count"] != art["equilibrium_input_count"]:
        problems.append(
            f"equilibrium input count differs: header env_count={hdr['env_count']} "
            f"artifact EquilibriumInput::COUNT={art['equilibrium_input_count']} "
            f"(header '{hdr['path']} [{hdr['struct_name']}]', artifact '{art['path']}')"
        )
    # An artifact with no enum (COUNT 0) must carry -1 sentinels, not indices
    # into a parameter channel the solver never accepts.
    if art["equilibrium_input_count"] == 0 and any(
        hdr[k] != -1 for k in ("env_temperature", "env_humidity", "env_cloud_water")
    ):
        problems.append(
            f"artifact '{art['path']}' exposes no EquilibriumInput enum but header "
            f"'{hdr['path']}' [{hdr['struct_name']}] gives non-sentinel channel indices "
            f"({hdr['env_temperature']}, {hdr['env_humidity']}, {hdr['env_cloud_water']})"
        )

    # R1 guard: a cloud-gated definition needs a third EquilibriumInput.
    if defn["cloud_gated"] and art["equilibrium_input_count"] < 3:
        problems.append(
            f"definition '{defn['path']}' declares a cloud_liquid_water-gated reaction but "
            f"artifact '{art['path']}' exposes only {art['equilibrium_input_count']} equilibrium "
            f"inputs (no CloudLiquidWater slot); regenerate the mechanism with the cloud gate."
        )

    return problems


def _fmt(mapping: dict) -> str:
    return ", ".join(f"{k}={v}" for k, v in mapping.items())


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--definition", required=True, type=Path)
    parser.add_argument("--artifact", required=True, type=Path)
    parser.add_argument("--meta-header", required=True, type=Path)
    args = parser.parse_args(argv)

    defn = parse_definition(args.definition)
    art = parse_artifact(args.artifact)
    # The struct to read is named from the definition's mechanism id, so one
    # aggregated header serves every mechanism.
    if not defn["mechanism_id"]:
        _fail(f"definition '{defn['path']}' has no metadata.mkpp_mechanism_id "
              f"to name the header struct to verify")
    struct_name = defn["mechanism_id"][:1].upper() + defn["mechanism_id"][1:] + "Facts"
    hdr = parse_meta_header(args.meta_header, struct_name)

    problems = verify(defn, art, hdr)
    if problems:
        for p in problems:
            print(f"FATAL ERROR: provenance drift: {p}", file=sys.stderr)
        print(f"\nverify_mechanism_provenance: {len(problems)} mismatch(es) -- "
              f"the checked-in artifact/header is stale relative to the definition.",
              file=sys.stderr)
        return 1

    print(
        f"OK: provenance consistent -- {len(defn['species'])} species, "
        f"{len(defn['photolysis_labels'])} photolysis label(s), "
        f"cloud_gated={defn['cloud_gated']}, equilibrium_inputs={art['equilibrium_input_count']} "
        f"(definition '{defn['path']}' == artifact '{art['path']}' == header '{hdr['path']}')"
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())

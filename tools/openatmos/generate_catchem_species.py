#!/usr/bin/env python3
"""Offline generator for the mkppchem CATChem species configuration and the
build-time mechanism metadata header.

This is an AUTHORING tool. The MKPP compiler must never be invoked during the
CATChem build or at runtime, and the same offline-only rule applies here: this
script is run by a mechanism author (or a maintainer re-pinning a mechanism),
never by CMake and never by the process. Its two outputs are checked in:

  1. A CATChem species YAML (``--species-out``) listing every species the
     mechanism advances, with the full per-species metadata CATChem needs to
     allocate and classify the tracer. There is no hand-maintained species list
     anywhere in the feature: the YAML is derived from the definition.

  2. ONE aggregated C++ metadata header (``--meta-out``) and ONE aggregated
     solver-registry header (``--registry-out``). The metadata header carries,
     for every definition given, the species slot names, photolysis labels,
     mechanism id, provenance digest, cloud-gate flag and solver
     equilibrium-channel indices as ``constexpr`` data, plus the definition
     table the runtime reads. The registry header carries the solver-adapter
     factories the process selects from. The process ``#include``s these
     headers so the runtime never parses YAML (the single-parse design
     decision); the CI provenance verifier re-parses the pinned definitions
     and asserts the headers still match.

     Aggregating is deliberate: adding a mechanism means adding its definition
     to this command -- never a new header file per mechanism, and never a
     hand-edited ``MechanismTraits`` specialization or registry entry.
     Everything the tables need is derivable from the definition (MKPP's
     EquilibriumInput enum order is fixed, so the equilibrium-channel indices
     are too), which is what lets the traits live in one generic template.

The definitions are the OpenAtmos YAMLs at ``--definition`` (repeatable; the
species output is per-definition, the two C++ headers are aggregated across
all definitions). Species metadata lives in each file's namespaced top-level
``catchem:`` block, which the MKPP compiler ignores (its parser reads only the
OpenAtmos keys). Every species a mechanism declares MUST have a matching
``catchem:`` entry or generation fails -- a missing entry is exactly the
silent-metadata-gap this single-source contract forbids.

Usage:
    # Regenerate the aggregated C++ headers for every pinned mechanism:
    python3 tools/openatmos/generate_catchem_species.py \
        --definition parm/config/openatmos/gocart/mechanism.yaml \
        --definition parm/config/openatmos/chapman/mechanism.yaml \
        --meta-out src/process/mkppchem/catchem_mkpp_mechanism_meta.hpp \
        --registry-out src/process/mkppchem/catchem_mkpp_mechanism_registry.hpp

    # Regenerate one mechanism's species configuration:
    python3 tools/openatmos/generate_catchem_species.py \
        --definition parm/config/openatmos/gocart/mechanism.yaml \
        --species-out tests/Configs/Mkpp/CATChem_mkppchem_species.yml
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

try:
    import yaml
except ImportError as exc:  # pragma: no cover - authoring-time dependency
    sys.exit(f"FATAL ERROR: generate_catchem_species requires PyYAML: {exc}")


# Keys emitted verbatim (no ``__`` prefix) because CATChem's reader looks them up
# by their literal name; everything else in a species' metadata is namespaced
# with the ``__`` convention the existing species files use.
_VERBATIM_KEYS = {"molecular weight [kg mol-1]"}

# Stable output order so regeneration is deterministic and reviewable.
_METADATA_ORDER = [
    "long_name",
    "description",
    "molecular weight [kg mol-1]",
    "is_gas",
    "is_aerosol",
    "is_dust",
    "is_seasalt",
    "is_advected",
    "is_drydep",
    "is_wetdep",
    "is_photolysis",
    "is_hydrophilic",
    "radius",
    "density",
    "lower_radius",
    "upper_radius",
    "viscosity",
    "dd_f0",
    "dd_hstar",
    "dd_DvzAerSnow",
    "dd_DvzMinVal_snow",
    "dd_DvzMinVal_land",
    "henry_k0",
    "henry_cr",
    "henry_pKa",
    "wd_retfactor",
    "wd_LiqAndGas",
    "wd_convfacI2G",
    "wd_rainouteff",
    "wd_reevap_frac",
    "t_chem_loss",
    "mie_name",
]


def _fail(msg: str) -> "NoReturn":  # type: ignore[name-defined]
    raise SystemExit(f"FATAL ERROR: generate_catchem_species: {msg}")


def load_definition(path: Path) -> dict:
    if path.suffix.lower() not in (".yaml", ".yml"):
        _fail(f"definition must be YAML (.yaml/.yml), got '{path}'")
    try:
        with path.open() as handle:
            data = yaml.safe_load(handle)
    except (OSError, yaml.YAMLError) as exc:
        _fail(f"cannot read/parse definition '{path}': {exc}")
    if not isinstance(data, dict):
        _fail(f"definition '{path}' did not parse to a mapping")
    return data


def _species_names(data: dict) -> list[str]:
    species = data.get("species")
    if not isinstance(species, list) or not species:
        _fail("definition has no 'species' sequence")
    names = []
    for entry in species:
        if not isinstance(entry, dict) or not entry.get("name"):
            _fail("a species entry is missing a scalar 'name'")
        names.append(str(entry["name"]))
    return names


def _photolysis_labels(data: dict) -> list[str]:
    labels = []
    for rxn in data.get("reactions", []) or []:
        if isinstance(rxn, dict) and rxn.get("type") == "PHOTOLYSIS":
            name = rxn.get("name")
            if not name:
                _fail("a PHOTOLYSIS reaction is missing its 'name' label")
            labels.append(str(name))
    return labels


def _photolysis_host_forcing(data: dict, labels: list[str]) -> list[float]:
    """Host-forcing constant per photolysis label, in declaration order.

    The definition's `metadata.photolysis_host_forcing_expressions` is the only
    machine-readable J-value the author supplies. It is NOT a runtime rate
    (a constant would not respond to solar zenith angle), only the isolated-
    mechanism smoke-test fallback, so it is baked into the meta header parallel
    to the label order. A label with no expression defaults to 0.0.
    """
    expressions = (data.get("metadata") or {}).get("photolysis_host_forcing_expressions") or {}
    forcing = []
    for label in labels:
        if label not in expressions:
            forcing.append(0.0)
            continue
        raw = expressions[label]
        try:
            forcing.append(float(raw))
        except (TypeError, ValueError) as exc:
            _fail(f"photolysis host forcing for '{label}' is not a number: {raw!r}")
    return forcing


def _has_cloud_gate(data: dict) -> bool:
    for rxn in data.get("reactions", []) or []:
        trigger = rxn.get("activation_trigger") if isinstance(rxn, dict) else None
        if isinstance(trigger, str) and "cloud_liquid_water" in trigger.lower():
            return True
    return False


def _has_equilibrium(data: dict) -> bool:
    """Mirror MKPP's codegen rule for the EquilibriumInput enum.

    MKPP emits the enum -- and threads the per-cell equilibrium parameter
    channel into the solver -- only when the mechanism has an EQUILIBRIUM
    reaction (template_context.py: ``has_equilibrium =
    bool(mech.equilibrium_reactions)``). A cloud-gated kinetic reaction
    forces the same channel on, because its gate consumes the third
    equilibrium input.
    """
    return any(
        isinstance(rxn, dict) and str(rxn.get("type", "")).upper() == "EQUILIBRIUM"
        for rxn in (data.get("reactions") or [])
    )


def env_indices(data: dict, cloud_gated: bool) -> tuple[int, int, int, int]:
    """Return (temperature, humidity, cloud_water, count) channel indices.

    Follows MKPP's emission rule exactly: the enum order is fixed
    (Temperature=0, RelativeHumidity=1, CloudLiquidWater=2 when a reaction is
    cloud-gated, COUNT 2 or 3), and there is no enum at all -- and no
    parameter channel -- for a mechanism with neither an equilibrium reaction
    nor a cloud gate. ``-1`` marks a channel the solver does not accept. The
    CI provenance verifier re-derives these from the definition and
    cross-checks them against the compiled artifact's enum.
    """
    if not (_has_equilibrium(data) or cloud_gated):
        return -1, -1, -1, 0
    if cloud_gated:
        return 0, 1, 2, 3
    return 0, 1, -1, 2


def _catchem_metadata(data: dict) -> dict:
    block = data.get("catchem")
    if not isinstance(block, dict):
        _fail("definition is missing the top-level 'catchem:' metadata block")
    return block


def _yaml_scalar(value) -> str:
    """Render a scalar exactly the way the hand-authored species files do.

    Molecular weights keep the ``<n>ee-3`` form (e.g. ``64.04e-3``) that the
    existing CATChem species files use, so the generated file reads like its
    neighbours; booleans use YAML truth values; floats render plainly.
    """
    if isinstance(value, bool):
        return "true" if value else "false"
    if isinstance(value, (list, tuple)):
        return "[" + ", ".join(str(v) for v in value) + "]"
    if isinstance(value, float):
        return repr(value)
    return str(value)


def emit_species_yaml(data: dict, definition_path: Path) -> str:
    names = _species_names(data)
    metadata = _catchem_metadata(data)
    missing = [n for n in names if n not in metadata]
    if missing:
        _fail(
            "definition species have no 'catchem:' metadata entry: "
            + ", ".join(missing)
        )

    lines = [
        "### CATChem_mkppchem_species.yml: MKPP mechanism species configuration.",
        "###",
        "### GENERATED by tools/openatmos/generate_catchem_species.py from",
        f"###   {definition_path}",
        "### Do not edit by hand: the OpenAtmos definition is the single source",
        "### of truth. Regenerate instead of editing.",
        "###",
        "### Species names are lowercased to match CATChem's tracer convention;",
        "### the runtime species mapping matches case-insensitively, so slot",
        "### order (declaration order here) is what the solver depends on.",
        "",
    ]
    for name in names:
        meta = metadata[name]
        # CATChem's tracer name is the lowercased mechanism name.
        lines.append(f"- name: {name.lower()}")
        # Always carry a description equal to the mechanism name so the
        # generated file is self-documenting and stable across regeneration.
        merged = {"description": name, **meta}
        emitted = set()
        for key in _METADATA_ORDER:
            if key not in merged:
                continue
            emitted.add(key)
            value = merged[key]
            out_key = key if key in _VERBATIM_KEYS else f"__{key}"
            lines.append(f"  {out_key}: {_yaml_scalar(value)}")
        # Any metadata key not in the canonical order list is still emitted
        # (alphabetically) so nothing authored in the definition is silently
        # dropped from the runtime configuration.
        for key in sorted(set(merged) - emitted):
            out_key = key if key in _VERBATIM_KEYS else f"__{key}"
            lines.append(f"  {out_key}: {_yaml_scalar(merged[key])}")
        lines.append("")
    return "\n".join(lines) + ""


def _cstr(value: str) -> str:
    return '"' + value.replace("\\", "\\\\").replace('"', '\\"') + '"'


def _mech_id(data: dict, definition_path: Path) -> str:
    mech_id = str((data.get("metadata") or {}).get("mkpp_mechanism_id", ""))
    if not mech_id:
        _fail(f"definition {definition_path} metadata.mkpp_mechanism_id is required")
    return mech_id


def _struct_name(mech_id: str) -> str:
    return mech_id[:1].upper() + mech_id[1:] + "Facts"


def emit_meta_struct(data: dict, definition_path: Path) -> str:
    """Emit one mechanism's ``Facts`` struct for the aggregated header."""
    names = _species_names(data)
    labels = _photolysis_labels(data)
    metadata = _catchem_metadata(data)
    missing = [n for n in names if n not in metadata]
    if missing:
        _fail("definition species have no 'catchem:' metadata entry: " + ", ".join(missing))

    mech_id = _mech_id(data, definition_path)
    sha = str((data.get("metadata") or {}).get("legacy_source_sha256", ""))
    def_name = str(data.get("name", ""))
    cloud_gated = _has_cloud_gate(data)
    num_reactions = len(data.get("reactions") or [])
    env_t, env_rh, env_cw, env_count = env_indices(data, cloud_gated)

    species_members = ",\n            ".join(_cstr(n) for n in names)
    label_members = (
        ",\n            ".join(_cstr(lbl) for lbl in labels) if labels else "// (none)"
    )
    forcing = _photolysis_host_forcing(data, labels)
    forcing_members = (
        ",\n            ".join(repr(v) for v in forcing) if forcing else "// (none)"
    )

    struct_name = _struct_name(mech_id)
    return f"""    // Source definition: {definition_path}
    struct {struct_name} {{
        /// Definition identity, for the init log and the provenance comparison.
        static constexpr std::string_view mechanism_id = {_cstr(mech_id)};
        static constexpr std::string_view definition_name = {_cstr(def_name)};
        static constexpr std::string_view legacy_source_sha256 = {_cstr(sha)};
        static constexpr std::string_view definition_source = {_cstr(str(definition_path))};

        /// The definition declares a cloud_liquid_water-gated reaction. Drives
        /// the R1 interim guard and the cloud-water source resolution at
        /// init().
        static constexpr bool has_cloud_gated_reaction = {"true" if cloud_gated else "false"};

        /// Solver equilibrium-parameter channel indices, derived from the
        /// definition's reaction kinds. MKPP emits the EquilibriumInput enum
        /// (fixed order Temperature=0, RelativeHumidity=1, CloudLiquidWater=2
        /// when a reaction is cloud-gated) only for mechanisms with an
        /// equilibrium reaction or a cloud gate, and -1 marks a channel the
        /// generated solver does not accept. The generic MechanismTraits reads
        /// these in place of a per-mechanism specialization; the verifier
        /// re-derives them from the definition and cross-checks the artifact's
        /// enum, so a stale value fails CI, never runtime.
        static constexpr int env_temperature = {env_t};
        static constexpr int env_humidity = {env_rh};
        static constexpr int env_cloud_water = {env_cw};
        static constexpr int env_count = {env_count};

        /// Species slot order: declaration order in the definition is the
        /// solver's per-cell state index. Iterating this in any other order is
        /// a correctness bug, not a style choice.
        static constexpr int num_species = {len(names)};
        static constexpr std::array<std::string_view, {len(names)}> species_order = {{
            {species_members}
        }};

        /// Photolysis labels in declaration order: this order IS the jvals
        /// index (the generated solver exposes photolysis rates positionally,
        /// with no rate-parameter map to scan).
        static constexpr int num_photolysis = {len(labels)};
        static constexpr std::array<std::string_view, {max(len(labels), 1)}> photolysis_labels = {{
            {label_members}
        }};

        /// Host-forcing J-value constants, parallel to photolysis_labels
        /// (same declaration order). Runtime NEVER uses these as a rate --
        /// they are the isolated-mechanism smoke-test fallback only; the
        /// coupled path always reads photolysis_rate_<label> diagnostics.
        static constexpr std::array<double, {max(len(labels), 1)}> photolysis_host_forcing = {{
            {forcing_members}
        }};

        /// Total reaction count in the definition, for the init log. The
        /// verifier re-parses the definition and asserts this still matches.
        static constexpr int num_reactions = {num_reactions};
    }};"""


def _definition_table_entry(mech_id: str, struct_name: str) -> str:
    """One gated entry of the generated definition table."""
    return (
        f"#ifdef MKPP_HOST_MECH_{mech_id.upper()}\n"
        f'            {{"{mech_id}", +[] {{ return definition_from_facts<meta::{struct_name}>(); }}}},\n'
        "#endif"
    )


def _registry_entry(mech_id: str, struct_name: str) -> str:
    """One gated entry of the generated solver registry."""
    return (
        f"#ifdef MKPP_HOST_MECH_{mech_id.upper()}\n"
        f'            {{"{mech_id}",\n'
        "             +[]() -> std::unique_ptr<SolverAdapter> {\n"
        "                 return std::make_unique<MechanismAdapter<\n"
        f"                     ::mkpp::generated::{mech_id}::SolverKernels<mkpp::StagingBuffers::ExecSpace>,\n"
        f"                     meta::{struct_name}>>();\n"
        "             }},\n"
        "#endif"
    )


def _collect_mechs(entries: list[tuple[dict, Path]]) -> list[tuple[str, str]]:
    """Validate and return (mech_id, struct_name) pairs in command order.

    Duplicate ids are fatal: two definitions claiming one mechanism id would
    silently register the same name twice, and the runtime selection would
    depend on argument order -- exactly the ambiguity this contract forbids.
    """
    seen: dict[str, Path] = {}
    out: list[tuple[str, str]] = []
    for data, path in entries:
        mech_id = _mech_id(data, path)
        if mech_id in seen:
            _fail(
                f"duplicate mkpp_mechanism_id '{mech_id}' from '{path}' "
                f"(already claimed by '{seen[mech_id]}')"
            )
        seen[mech_id] = path
        out.append((mech_id, _struct_name(mech_id)))
    return out


def emit_aggregated_meta(entries: list[tuple[dict, Path]]) -> str:
    """Emit the single aggregated metadata header for all given definitions.

    ``entries`` is a list of (parsed definition, definition path) pairs in
    command order. The output carries one Facts struct per mechanism plus the
    build-gated definition table the runtime reads -- no per-mechanism file.
    """
    mechs = _collect_mechs(entries)
    structs = "\n\n".join(emit_meta_struct(data, path) for (data, path), (_, _) in zip(entries, mechs))
    table = "\n".join(_definition_table_entry(m, s) for m, s in mechs)
    sources = "\n".join(f"//   {path}" for _, path in entries)
    return f"""// Aggregated build-time metadata for the mkppchem mechanisms: one Facts
// struct per pinned OpenAtmos definition plus the definition table the
// runtime reads.
//
// GENERATED by tools/openatmos/generate_catchem_species.py. Do not edit by
// hand and do not parse at runtime -- these structs ARE the runtime's copy of
// the OpenAtmos definitions' structural facts, baked once at authoring time
// so the process never loads a YAML parser (the single-parse design). The CI
// provenance verifier (tools/openatmos/verify_mechanism_provenance.py)
// re-parses each pinned definition and asserts this header still matches;
// any drift fails the build, not a silent mismatch.
//
// One aggregated header instead of one file per mechanism: adding a mechanism
// means adding its definition to the generator command -- never a new source
// file, and never a hand-edited MechanismTraits specialization.
//
// The definition table is gated by the MKPP_HOST_MECH_<UPPER> macros CMake
// sets from CATCHEM_MKPP_MECHANISMS, so a build that compiles a subset of the
// generated mechanisms sees exactly that subset.
//
// Source definitions:
{sources}
#pragma once

#include "catchem_mkpp_mechanism.hpp"

#include <array>
#include <cstddef>
#include <string>
#include <string_view>
#include <vector>

namespace catchem::mkpp::meta {{

{structs}

}} // namespace catchem::mkpp::meta

namespace catchem::mkpp {{

    /// Convert one generated metadata struct into a MechanismDefinition.
    /// Templated on the Facts type so the same code serves every compiled
    /// mechanism; this -- not a YAML parse -- is where the runtime's
    /// definition facts come from (the single-parse design).
    template <typename Facts>
    MechanismDefinition definition_from_facts() {{
        MechanismDefinition def;
        def.name = std::string(Facts::definition_name);
        def.mkpp_mechanism_id = std::string(Facts::mechanism_id);
        def.legacy_source_sha256 = std::string(Facts::legacy_source_sha256);
        def.definition_path = std::string(Facts::definition_source);
        def.has_cloud_gated_reaction = Facts::has_cloud_gated_reaction;
        def.num_reactions = Facts::num_reactions;
        for (const auto& slot : Facts::species_order)
            def.species_order.emplace_back(slot);
        for (const auto& label : Facts::photolysis_labels)
            def.photolysis_labels.emplace_back(label);
        // Host-forcing constants parallel to the labels (smoke mode only).
        for (size_t i = 0; i < def.photolysis_labels.size(); ++i)
            def.photolysis_host_forcing[def.photolysis_labels[i]] = Facts::photolysis_host_forcing[i];

        // Equilibrium input names mirror the parser: temperature and humidity
        // are always consumed by the solver's per-cell parameters; the cloud
        // gate adds a third.
        def.equilibrium_inputs = {{"Temperature", "RelativeHumidity"}};
        if (def.has_cloud_gated_reaction)
            def.equilibrium_inputs.push_back("CloudLiquidWater");
        return def;
    }}

    /// One compiled mechanism's baked definition facts, keyed by id.
    struct DefinitionTableEntry {{
        std::string_view id;
        MechanismDefinition (*build)();
    }};

    /// The mechanisms whose baked definition facts are present in this build.
    /// The CMake CATCHEM_MKPP_MECHANISMS list is what makes a gated entry
    /// exist here; adding a mechanism needs no hand edit of this file.
    inline const std::vector<DefinitionTableEntry>& definition_table() {{
        static const std::vector<DefinitionTableEntry> table = {{
{table}
        }};
        return table;
    }}

}} // namespace catchem::mkpp
"""


def emit_aggregated_registry(entries: list[tuple[dict, Path]]) -> str:
    """Emit the single aggregated solver-registry header.

    The registry maps mechanism id -> solver-adapter factory, gated by the
    same MKPP_HOST_MECH_<UPPER> macros as the definition table, so the
    registry and the compiled set never diverge. It lives apart from the
    metadata header because instantiating a solver needs the generated
    artifact headers (pulled in via MKPP's mechanism manager) and the
    hand-written adapter machinery -- dependencies the definition table
    deliberately does not carry.
    """
    mechs = _collect_mechs(entries)
    registry = "\n".join(_registry_entry(m, s) for m, s in mechs)
    sources = "\n".join(f"//   {path}" for _, path in entries)
    return f"""// Aggregated solver registry for the mkppchem mechanisms: mechanism id ->
// solver-adapter factory, one gated entry per pinned OpenAtmos definition.
//
// GENERATED by tools/openatmos/generate_catchem_species.py. Do not edit by
// hand. Adding a mechanism means adding its definition to the generator
// command -- never a new file, never a hand-written registry entry or
// MechanismTraits specialization (the generic template in
// catchem_mkpp_mechanism_traits.hpp reads every mechanism's Facts struct).
//
// Entries are gated by the MKPP_HOST_MECH_<UPPER> macros CMake sets from
// CATCHEM_MKPP_MECHANISMS -- the same gating as the definition table, so a
// build that compiles a subset registers exactly that subset.
//
// Source definitions:
{sources}
#pragma once

#include "catchem_mkpp_mechanism_meta.hpp"
#include "catchem_mkpp_mechanism_traits.hpp"

// MKPP's compiled-mechanism manager pulls the selected generated artifact
// headers (gated by MKPP_HOST_SELECTED_MECHANISMS / MKPP_HOST_MECH_<UPPER>),
// which is what makes the ::mkpp::generated::<mechanism> types named below
// visible in this header's translation units.
#include <mkpp_host/mechanism_manager.hpp>

#include <memory>
#include <string>
#include <utility>
#include <vector>

namespace catchem::mkpp {{

    /// Factory signature for one compiled mechanism's solver adapter.
    using SolverFactory = std::unique_ptr<SolverAdapter> (*)();

    /// The mechanisms compiled into this binary, paired with their solver
    /// adapters. This is the process's authority for what can run; an id not
    /// listed here is not solvable in this build.
    inline const std::vector<std::pair<std::string, SolverFactory>>& mechanism_registry() {{
        static const std::vector<std::pair<std::string, SolverFactory>> registry = {{
{registry}
        }};
        return registry;
    }}

}} // namespace catchem::mkpp
"""


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--definition", required=True, nargs="+", action="extend", type=Path,
                        help="OpenAtmos mechanism definition YAML (source of truth); "
                             "repeatable, and every input is aggregated into the C++ headers")
    parser.add_argument("--species-out", type=Path,
                        help="write the generated CATChem species YAML here (exactly one --definition)")
    parser.add_argument("--meta-out", type=Path,
                        help="write the aggregated C++ metadata header here")
    parser.add_argument("--registry-out", type=Path,
                        help="write the aggregated C++ solver-registry header here")
    args = parser.parse_args(argv)

    if not any((args.species_out, args.meta_out, args.registry_out)):
        _fail("at least one of --species-out, --meta-out or --registry-out is required")
    if args.species_out and len(args.definition) != 1:
        _fail("--species-out takes exactly one --definition (it is per-mechanism); run it once per mechanism")
    if bool(args.meta_out) != bool(args.registry_out):
        _fail("--meta-out and --registry-out are the two halves of one aggregated output; pass both or neither")

    # Normalise the recorded source paths to the given form for stable output.
    entries = [(load_definition(path), Path(str(path))) for path in args.definition]

    if args.species_out:
        data, def_path = entries[0]
        text = emit_species_yaml(data, def_path)
        args.species_out.parent.mkdir(parents=True, exist_ok=True)
        args.species_out.write_text(text)
        print(f"wrote {args.species_out}")
    if args.meta_out:
        text = emit_aggregated_meta(entries)
        args.meta_out.parent.mkdir(parents=True, exist_ok=True)
        args.meta_out.write_text(text)
        print(f"wrote {args.meta_out}")
    if args.registry_out:
        text = emit_aggregated_registry(entries)
        args.registry_out.parent.mkdir(parents=True, exist_ok=True)
        args.registry_out.write_text(text)
        print(f"wrote {args.registry_out}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

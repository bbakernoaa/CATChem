// src/process/mkppchem/catchem_mkpp_mechanism.hpp
//
// OpenAtmos mechanism definition parsing, compiled-mechanism provenance
// validation, and the mechanism-slot <-> CATChem-index species mapping.
//
// This unit is deliberately Kokkos- and MKPP-free: it consumes the definition
// facts (from the generated metadata header at runtime, from YAML in the CI
// tests) and the *counts* the compiled artifact exposes, so it can be unit
// tested on synthetic inputs without a GPU toolchain. The compiled artifact's
// constants (NUM_SPECIES, NUM_PHOTOLYSIS) are passed in by the caller, which
// is the only translation unit that includes the generated solver header.
//
// Split for the single-parse design: catchem_mkpp_mechanism.cpp (this unit's
// yaml-free half: definition_from_meta, validate_provenance,
// build_species_mapping) is compiled into the process library; the YAML parser
// (parse_openatmos_definition, in catchem_mkpp_mechanism_parse.cpp) is
// compiled only into the CI/unit-test target, so yaml-cpp never reaches the
// runtime link.
#pragma once

#include <cstddef>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

namespace catchem::mkpp {

    /**
     * @brief Reaction type discriminator from the OpenAtmos definition.
     */
    enum class ReactionType { Arrhenius, Heterogeneous, Photolysis, Equilibrium, Unknown };

    /**
     * @brief One parsed OpenAtmos definition, in declaration order.
     *
     * Declaration order is load-bearing: the species vector is the mechanism
     * slot order, and the photolysis label vector is the positional `jvals`
     * index (the generated solver exposes photolysis rates positionally with no
     * rate-parameter map). Iterating these in any other order is a correctness
     * bug, not a style choice.
     */
    struct MechanismDefinition {
        std::string name;                                 ///< definition `name`
        std::string mkpp_mechanism_id;                    ///< metadata `mkpp_mechanism_id`
        std::string legacy_source_sha256;                 ///< metadata provenance digest
        std::vector<std::string> species_order;           ///< species names, declaration order == slot order
        std::vector<std::string> photolysis_labels;       ///< PHOTOLYSIS reaction names, declaration order == jvals index
        std::vector<std::string> equilibrium_inputs;      ///< met quantity names referenced by activation triggers
        std::vector<ReactionType> reaction_types;         ///< per-reaction type, for logging/validation
        int num_reactions = 0;                            ///< total reactions in the definition (init log)
        bool has_cloud_gated_reaction = false;            ///< any reaction whose trigger names cloud_liquid_water
        std::string definition_path;                      ///< file the definition was parsed from (for diagnostics)
        std::unordered_map<std::string, double> photolysis_host_forcing; ///< label -> constant (smoke mode only)

        /// Canonical identity used for provenance comparison.
        std::string identity() const { return mkpp_mechanism_id + ":" + legacy_source_sha256; }
    };

    /**
     * @brief What the compiled mechanism artifact declares, passed in by the
     *        caller so this header stays free of the generated solver include.
     */
    struct CompiledMechanismDescriptor {
        std::string identifier;   ///< e.g. "gocart"
        int num_species;          ///< artifact NUM_SPECIES
        int num_photolysis;       ///< artifact NUM_PHOTOLYSIS
        int num_equilibrium_inputs; ///< artifact EquilibriumInput::COUNT
        std::string artifact_path;  ///< generated header path (for diagnostics)
        /// The artifact's Species enum names, when the caller can supply them.
        /// The generated header exposes the enum only as C++ identifiers (no
        /// string table), so the runtime leaves this empty and the name-set
        /// authority is the CI provenance verifier, which reads the enum
        /// textually. Tests and tooling that DO have the names fill it in and
        /// get the case-insensitive set check for free.
        std::vector<std::string> species_names;
    };

    /**
     * @brief One resolved mechanism-slot to CATChem-index correspondence.
     */
    struct SpeciesMappingEntry {
        int mechanism_slot;   ///< index into the solver's per-cell state vector
        int catchem_index;    ///< index into ChemState species axis
        std::string canonical_name; ///< uppercased join key
    };

    /**
     * @brief Result of building the species mapping across the whole mechanism.
     */
    struct SpeciesMapping {
        std::vector<SpeciesMappingEntry> mapped;         ///< resolved slots, in slot order
        std::vector<std::string> unmatched_mechanism;    ///< mechanism species with no CATChem counterpart (must be empty)
        std::vector<std::string> unadvanced_catchem;     ///< CATChem species the mechanism does not advance (info only)
    };

    /**
     * @brief Build the runtime's MechanismDefinition from the generated
     *        metadata header (catchem_mkpp_mechanism_meta.hpp, this mechanism's
     *        Facts struct).
     *
     * This is what `init()` uses. The runtime does not parse the OpenAtmos
     * definition: the build-time generator (tools/openatmos/
     * generate_catchem_species.py) parses it once and bakes its structural
     * facts into the header, and the CI provenance verifier
     * (tools/openatmos/verify_mechanism_provenance.py) re-parses the pinned
     * definition and asserts the header still matches. A stale header is
     * therefore a CI failure, not a silent runtime divergence.
     *
     * `mechanism_id` selects which compiled mechanism's baked facts to return
     * (the shipped default when empty). An id with no compiled metadata in
     * this build is fatal naming the request -- the compiled set is a build
     * fact, and the caller's registry lookup normally rejects an uncompiled id
     * before this is ever reached.
     *
     * @return The definition facts as recorded at generation time.
     */
    MechanismDefinition definition_from_meta(const std::string& mechanism_id = "");

    /**
     * @brief The mechanism ids whose compiled definition metadata is present
     *        in this build -- the same list the metadata table was generated
     *        for, gated by the MKPP_HOST_MECH_<UPPER> macros CMake sets from
     *        CATCHEM_MKPP_MECHANISMS. The configuration-time process-order
     *        validator uses it to resolve the selected mechanism's species
     *        before any solver object exists.
     */
    std::vector<std::string> compiled_mechanism_ids();

    /**
     * @brief Parse the optional runtime `initial_step_seed` process option.
     *
     * The empty string means "unset": the generated artifact's baked
     * compile-time seed stays in force and the result has no value. Any other
     * input must parse -- fully, no trailing garbage -- to a finite positive
     * number; anything else is fatal naming the offending value, because a
     * silently-misread seed would change solver behavior without any visible
     * trace in the run log.
     */
    std::optional<double> parse_initial_step_seed(const std::string& value);

    /**
     * @brief Resolve a configured mechanism name against the mechanisms
     *        compiled into this build.
     *
     * An empty request selects `default_id` and sets `used_default` so the
     * caller can log that the shipped default was chosen. A request naming a
     * mechanism that was not compiled is fatal naming the request and every
     * available mechanism -- the same diagnostic the MKPP host registry raises
     * for an unknown mechanism, but produced without a bare std::runtime_error
     * escaping into the run log (the process re-raises MKPP's own throws with
     * this context, and this helper is the pure-logic path the unit tests
     * exercise directly).
     *
     * @param requested      configured `mechanism` value ("" = not configured)
     * @param available      mechanism ids compiled into the build
     * @param default_id     shipped default when nothing is configured
     * @param used_default   out: true when the default was selected
     * @return index into `available` for the selected mechanism
     */
    size_t select_mechanism(const std::string& requested, const std::vector<std::string>& available,
                            const std::string& default_id, bool& used_default);

    /**
     * @brief Verify a configured definition file against the selected
     *        mechanism's compiled provenance.
     *
     * The runtime never parses YAML, so this is a targeted textual scan of the
     * definition's `metadata:` block: `mkpp_mechanism_id` must equal the
     * selected mechanism id and `legacy_source_sha256` must equal the compiled
     * digest. Either disagreement -- or a file that carries no metadata block
     * at all -- is fatal naming BOTH definitions (the configured path and the
     * compiled mechanism's own shipped definition path), before any timestep
     * runs. An unreadable file is fatal naming the path.
     */
    void verify_definition_provenance(const std::string& definition_file, const std::string& mechanism_id,
                                      const std::string& expected_sha256, const std::string& compiled_definition_path);

    /**
     * @brief Parse an OpenAtmos definition file (YAML, read via yaml-cpp) into
     *        a MechanismDefinition.
     *
     * NOT used by the runtime (see definition_from_meta): this parser is
     * compiled only into the CI/unit-test target (catchem_mkpp_mechanism_parse.cpp),
     * which exercises its failure-path diagnostics directly. The path must
     * carry a `.yaml`/`.yml` extension; a `.json` path is rejected even though
     * JSON would load as a YAML subset, because the JSON files shipped in the
     * MKPP tree are derived migration artifacts, not the source of truth (the
     * definition's own metadata records `legacy_source: *.yaml`).
     *
     * @param path Filesystem path to the YAML definition.
     * @return The parsed definition.
     * @throws std::runtime_error with a `FATAL ERROR:` prefix naming the path
     *         and reason when the extension is not YAML, the file is missing,
     *         unparseable, or malformed (empty/duplicate species name,
     *         unresolved reactant/product).
     */
    MechanismDefinition parse_openatmos_definition(const std::string& path);

    /**
     * @brief Validate the parsed definition against the compiled artifact.
     *
     * Enforces the structural provenance checks (mechanism id, species count,
     * photolysis count) and the interim guard: when the definition declares
     * a cloud-gated reaction but the artifact exposes no cloud-water equilibrium
     * input, this is fatal rather than silently running ungated chemistry.
     *
     * @param def          Parsed definition.
     * @param artifact     Compiled-mechanism descriptor.
     * @throws std::runtime_error with a `FATAL ERROR:` prefix naming both sides
     *         on any mismatch.
     */
    void validate_provenance(const MechanismDefinition& def, const CompiledMechanismDescriptor& artifact);

    /**
     * @brief Build the mechanism-slot to CATChem-index mapping.
     *
     * Matching is case-insensitive against CATChem's canonical uppercase names.
     * Never zero-fills or aliases a mechanism species that has no counterpart.
     *
     * @param def                  Parsed definition (slot names, in order).
     * @param catchem_name_to_index CATChem's canonical uppercase name -> index map.
     * @return The mapping; `unmatched_mechanism` must be empty for a valid run.
     */
    SpeciesMapping build_species_mapping(const MechanismDefinition& def,
                                         const std::unordered_map<std::string, int>& catchem_name_to_index);

    /**
     * @brief One process-order conflict: an existing GOCART chemistry process
     *        scheduled alongside `mkppchem` that advances some of the same
     *        species, so both would apply the same tendency.
     */
    struct ProcessOrderConflict {
        std::string process;             ///< the conflicting process name (so4chem/carbchem)
        std::vector<std::string> shared; ///< canonical species both advance
    };

    /**
     * @brief Detect a process-order conflict (mkppchem + so4chem/carbchem).
     *
     * A run whose resolved process order activates `mkppchem` together with an
     * existing GOCART chemistry process that advances the same species would
     * silently double-apply the chemistry, so it is fatal. The check is a set
     * intersection of the active process list against the known GOCART
     * chemistry processes, restricted to the species each shares with the
     * mechanism. Returns every conflicting process (there can be two), in the
     * order they appear in `active_processes`, so the reported diagnostic is
     * deterministic across ranks.
     *
     * @param active_processes     Resolved process order (the run's process list).
     * @param mechanism_species    Canonical (uppercased) species the mechanism advances.
     * @return Conflicts; empty when the process order is safe.
     */
    std::vector<ProcessOrderConflict> find_process_order_conflict(const std::vector<std::string>& active_processes,
                                                                  const std::vector<std::string>& mechanism_species);

    /**
     * @brief Human-readable summary of detected conflicts, for the fatal
     *        message: "mkppchem shares SO4, SO2, DMS with so4chem" per entry.
     */
    std::string describe_process_order_conflicts(const std::vector<ProcessOrderConflict>& conflicts);

    /**
     * @brief Result of planning the photolysis coupling at init().
     *
     * The plan is the ordered label -> jvals-slot binding (index i binds
     * jvals slot i, so the order is the definition's declaration order and
     * never DiagnosticManager's map order -- the positional hazard this avoids.
     * `smoke_mode` selects the definition's host-forcing constants instead of
     * diagnostic binding; the process logs a prominent warning in that case.
     */
    struct PhotolysisBinding {
        bool smoke_mode = false;                    ///< host-forcing constants, no diagnostics bound
        std::vector<std::string> diag_names;        ///< jvals slot i <- diag_names[i] (empty in smoke mode)
        std::vector<double> smoke_forcing;          ///< smoke mode: forcing per jvals slot, label order
    };

    /**
     * @brief Plan the photolysis coupling for one init().
     *
     * Enforces the coupling contract against the configuration and the diagnostics
     * the host has registered so far:
     *  - smoke mode + a photolysis source also scheduled/bound is a fatal
     *    contradiction (the constants would silently override computed rates);
     *  - a mechanism that declares photolysis, run without smoke mode and
     *    with no photolysis diagnostics registered at all, is fatal:
     *    zero J-rates would silently disable the OH production the whole
     *    sulfur cycle depends on;
     *  - a specific label with no registered `photolysis_rate_<label>` field
     *    is fatal, naming the label and the definition;
     *  - otherwise every label binds to its diagnostic in declaration order.
     *
     * The checks are pure string logic over the caller-supplied facts, so
     * they are testable without a solver run or a live DiagnosticManager.
     *
     * @param def                Definition (photolysis labels + host forcing).
     * @param smoke_mode         Configured `photolysis_smoke_mode`.
     * @param active_processes   Resolved process order (for the contradiction check).
     * @param registered_diags   DiagnosticManager::get_registered_names().
     * @return The binding plan.
     * @throws std::runtime_error with a `FATAL ERROR:` prefix on contradiction,
     *         missing photolysis source, or
     *         an unresolved label.
     */
    PhotolysisBinding plan_photolysis_binding(const MechanismDefinition& def, bool smoke_mode,
                                              const std::vector<std::string>& active_processes,
                                              const std::vector<std::string>& registered_diags);

    /// Uppercase a string in place (ASCII); used for canonical species matching.
    std::string canonical_upper(std::string_view name);

} // namespace catchem::mkpp

// src/process/mkppchem/catchem_mkpp_mechanism.cpp
//
// The yaml-free half of the mkppchem mechanism unit: the runtime's
// definition-from-meta-header, the compiled-mechanism selection helper, the
// configured-definition provenance scan, the compiled-artifact provenance check
// (with the R1 interim guard), and the species-mapping builder. Compiled into
// the process library. The definition facts it serves come from the single
// aggregated generated metadata header, whose table is gated by the
// MKPP_HOST_MECH_<UPPER> macros CMake sets from CATCHEM_MKPP_MECHANISMS. The
// YAML parser lives in catchem_mkpp_mechanism_parse.cpp, which is compiled only
// into the CI/unit-test target -- the runtime never parses YAML (the
// single-parse design), so yaml-cpp stays off the process link.
#include "catchem_mkpp_mechanism.hpp"

#include "catchem_mkpp_mechanism_meta.hpp"
#include <algorithm>
#include <cctype>
#include <cerrno>
#include <cmath>
#include <cstdlib>
#include <fstream>
#include <functional>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>

namespace catchem::mkpp {

    namespace {

        /// Wrap a reason in the project's fatal-diagnostic convention so the
        /// message is unambiguous in a run log and names the offending artifact.
        [[noreturn]] void fatal(const std::string& context, const std::string& reason) {
            throw std::runtime_error("FATAL ERROR: mkppchem " + context + ": " + reason);
        }

        /// Locate one compiled mechanism's baked definition facts. The table is
        /// generated (catchem_mkpp_mechanism_meta.hpp), gated by the same
        /// MKPP_HOST_MECH_<UPPER> macros CMake sets from
        /// CATCHEM_MKPP_MECHANISMS, so adding a mechanism is a definition file
        /// plus a generator run -- never an edit to this file.
        const DefinitionTableEntry* find_meta(const std::string& id) {
            const auto& table = definition_table();
            auto it = std::find_if(table.begin(), table.end(),
                                   [&](const auto& e) { return e.id == id; });
            return it == table.end() ? nullptr : &*it;
        }

        /// Extract a flat scalar from a YAML `metadata:` block by key,
        /// textually. The runtime never links a YAML parser, and the provenance
        /// scan needs exactly two scalars; a targeted line scan is enough and
        /// fails closed (an absent key reads as empty, which the caller treats
        /// as a mismatch).
        std::string yaml_scalar(const std::string& text, const std::string& key) {
            const auto pos = text.find(key + ":");
            if (pos == std::string::npos)
                return {};
            const auto line_end = text.find('\n', pos);
            std::string value = text.substr(pos + key.size() + 1,
                                            line_end == std::string::npos ? std::string::npos
                                                                          : line_end - (pos + key.size() + 1));
            const auto begin = value.find_first_not_of(" \t\r\"'");
            const auto end = value.find_last_not_of(" \t\r\"'");
            return (begin == std::string::npos) ? std::string{} : value.substr(begin, end - begin + 1);
        }

    } // namespace

    std::string canonical_upper(std::string_view name) {
        std::string out(name);
        std::transform(out.begin(), out.end(), out.begin(),
                       [](unsigned char c) { return static_cast<char>(std::toupper(c)); });
        return out;
    }

    MechanismDefinition definition_from_meta(const std::string& mechanism_id) {
        // The runtime's copy of the definition's structural facts, taken from
        // the generated metadata header rather than a YAML parse. The CI
        // provenance verifier (tools/openatmos/verify_mechanism_provenance.py)
        // re-parses the pinned definition and asserts this header still matches,
        // so a stale header fails the build, not a silent runtime mismatch.
        // The id always comes from the caller's compiled-mechanism registry, so
        // a mechanism whose facts were never generated cannot reach here.
        const std::string id = mechanism_id.empty() ? std::string("gocart") : mechanism_id;
        const auto* entry = find_meta(id);
        if (!entry)
            fatal("mechanism", "no compiled definition metadata for mechanism '" + id +
                                   "' in this build (the metadata table is fixed at build time)");
        return entry->build();
    }

    std::vector<std::string> compiled_mechanism_ids() {
        // The ids whose compiled definition metadata is present in this build.
        // Used by the configuration-time process-order validator, which needs
        // the selected mechanism's species before any solver exists.
        const auto& table = definition_table();
        std::vector<std::string> ids;
        ids.reserve(table.size());
        for (const auto& e : table)
            ids.emplace_back(e.id);
        return ids;
    }

    std::optional<double> parse_initial_step_seed(const std::string& value) {
        // Unset (empty) defers to the artifact's baked compile-time seed.
        if (value.empty())
            return std::nullopt;
        // Fully-parse or fatal: a trailing "x" or a locale-dependent partial
        // read would silently substitute a different seed than configured.
        // strod reports where parsing stopped via the end pointer.
        const char* begin = value.c_str();
        char* end = nullptr;
        errno = 0;
        const double parsed = std::strtod(begin, &end);
        const bool trailing_junk = end != begin + value.size();
        if (end == begin || trailing_junk || errno == ERANGE || !std::isfinite(parsed) || parsed <= 0.0) {
            fatal("configuration",
                  "initial_step_seed must be a finite positive fraction of the chemistry interval, got '" +
                      value + "'");
        }
        return parsed;
    }

    size_t select_mechanism(const std::string& requested, const std::vector<std::string>& available,
                            const std::string& default_id, bool& used_default) {
        const std::string& chosen = requested.empty() ? default_id : requested;
        used_default = requested.empty();
        auto it = std::find(available.begin(), available.end(), chosen);
        if (it == available.end()) {
            std::ostringstream list;
            for (size_t i = 0; i < available.size(); ++i)
                list << (i ? ", " : "") << available[i];
            fatal("mechanism", "requested mechanism '" + chosen + "' is not compiled into this build; available: " +
                                    (list.str().empty() ? std::string("<none>") : list.str()));
        }
        return static_cast<size_t>(it - available.begin());
    }

    void verify_definition_provenance(const std::string& definition_file, const std::string& mechanism_id,
                                      const std::string& expected_sha256, const std::string& compiled_definition_path) {
        std::ifstream input(definition_file);
        if (!input)
            fatal("provenance", "configured definition_file '" + definition_file +
                                    "' cannot be opened for the provenance check against mechanism '" +
                                    mechanism_id + "'");
        std::ostringstream buffer;
        buffer << input.rdbuf();
        const std::string text = buffer.str();

        const std::string file_id = yaml_scalar(text, "mkpp_mechanism_id");
        if (file_id != mechanism_id)
            fatal("provenance", "configured definition '" + definition_file + "' declares mechanism id '" +
                                    (file_id.empty() ? std::string("<none>") : file_id) +
                                    "' but the selected compiled mechanism is '" + mechanism_id +
                                    "' (its own definition: '" + compiled_definition_path +
                                    "'); the runtime species state and the solver must come from the same source");

        const std::string file_sha = yaml_scalar(text, "legacy_source_sha256");
        if (!expected_sha256.empty() && file_sha != expected_sha256)
            fatal("provenance", "configured definition '" + definition_file + "' carries provenance digest '" +
                                    (file_sha.empty() ? std::string("<none>") : file_sha) + "' but the compiled mechanism '" +
                                    mechanism_id + "' was generated from digest '" + expected_sha256 +
                                    "' (its own definition: '" + compiled_definition_path +
                                    "') -- a stale or substituted definition, not the one the solver was built from");
    }

    void validate_provenance(const MechanismDefinition& def, const CompiledMechanismDescriptor& artifact) {
        if (def.mkpp_mechanism_id != artifact.identifier)
            fatal("provenance", "definition '" + def.definition_path + "' declares mechanism id '" +
                                    def.mkpp_mechanism_id + "' but the selected compiled artifact is '" +
                                    artifact.identifier + "' (" + artifact.artifact_path + ")");
        if (static_cast<int>(def.species_order.size()) != artifact.num_species)
            fatal("provenance", "definition '" + def.definition_path + "' declares " +
                                    std::to_string(def.species_order.size()) + " species but the compiled artifact '" +
                                    artifact.identifier + "' (" + artifact.artifact_path + ") declares " +
                                    std::to_string(artifact.num_species) + " (stale artifact?)");
        if (static_cast<int>(def.photolysis_labels.size()) != artifact.num_photolysis)
            fatal("provenance", "definition '" + def.definition_path + "' declares " +
                                    std::to_string(def.photolysis_labels.size()) +
                                    " photolysis reactions but the compiled artifact '" + artifact.identifier + "' (" +
                                    artifact.artifact_path + ") declares NUM_PHOTOLYSIS=" +
                                    std::to_string(artifact.num_photolysis) + " (stale artifact?)");

        // Name-set equality (case-insensitive), checked only when the caller
        // supplied the artifact's species names. The generated header exposes
        // the Species enum as C++ identifiers with no string table, so the
        // runtime descriptor leaves species_names empty and the name-set
        // authority is the CI provenance verifier (which reads the enum
        // textually). When names ARE supplied (tests, tooling), a species
        // renamed on one side and not the other is caught here, not by a
        // downstream zero-fill.
        if (!artifact.species_names.empty()) {
            std::set<std::string> def_upper;
            for (const auto& s : def.species_order)
                def_upper.insert(canonical_upper(s));
            std::set<std::string> art_upper;
            for (const auto& s : artifact.species_names)
                art_upper.insert(canonical_upper(s));
            if (def_upper != art_upper) {
                std::string missing, extra;
                for (const auto& s : def_upper)
                    if (!art_upper.count(s))
                        missing += (missing.empty() ? "" : ", ") + s;
                for (const auto& s : art_upper)
                    if (!def_upper.count(s))
                        extra += (extra.empty() ? "" : ", ") + s;
                fatal("provenance", "definition '" + def.definition_path + "' and compiled artifact '" +
                                        artifact.identifier + "' (" + artifact.artifact_path +
                                        ") disagree on the species name set: definition-only [" + missing +
                                        "], artifact-only [" + extra + "] (stale artifact or renamed species?)");
            }
        }

        // R1 interim guard: the definition declares a cloud-gated reaction, but
        // the shipped artifact exposes no cloud-water equilibrium input. Running
        // it would silently over-produce sulfate, so fail loud rather than degrade.
        if (def.has_cloud_gated_reaction && artifact.num_equilibrium_inputs < 3)
            fatal("provenance",
                  "definition '" + def.definition_path +
                      "' declares a cloud_liquid_water-gated reaction, but the compiled artifact '" +
                      artifact.identifier + "' (" + artifact.artifact_path +
                      ") exposes only " + std::to_string(artifact.num_equilibrium_inputs) +
                      " equilibrium inputs and does not implement the gate its own definition declares. "
                      "Regenerate the mechanism with cloud_liquid_water as a third EquilibriumInput before "
                      "running this configuration.");
    }

    SpeciesMapping build_species_mapping(const MechanismDefinition& def,
                                         const std::unordered_map<std::string, int>& catchem_name_to_index) {
        SpeciesMapping mapping;
        std::set<int> claimed_catchem;
        for (int slot = 0; slot < static_cast<int>(def.species_order.size()); ++slot) {
            const std::string canonical = canonical_upper(def.species_order[static_cast<std::size_t>(slot)]);
            auto it = catchem_name_to_index.find(canonical);
            if (it == catchem_name_to_index.end()) {
                mapping.unmatched_mechanism.push_back(canonical);
                continue;
            }
            // A slot must not collide onto an already-claimed CATChem index;
            // that would double-apply a tendency to one tracer.
            if (!claimed_catchem.insert(it->second).second) {
                mapping.unmatched_mechanism.push_back(canonical + " (collides with an earlier slot)");
                continue;
            }
            mapping.mapped.push_back({slot, it->second, canonical});
        }

        // CATChem species the mechanism does not advance: informational only.
        std::set<int> mapped_indices;
        for (const auto& entry : mapping.mapped)
            mapped_indices.insert(entry.catchem_index);
        for (const auto& [name, index] : catchem_name_to_index) {
            if (mapped_indices.find(index) == mapped_indices.end())
                mapping.unadvanced_catchem.push_back(name);
        }
        std::sort(mapping.unadvanced_catchem.begin(), mapping.unadvanced_catchem.end());
        return mapping;
    }

    std::vector<ProcessOrderConflict> find_process_order_conflict(const std::vector<std::string>& active_processes,
                                                                  const std::vector<std::string>& mechanism_species) {
        // The existing GOCART chemistry processes that overlap the mechanism,
        // each paired with the canonical species whose tendency it writes. The
        // sulfur chain belongs to so4chem (DMS -> SO2 -> SO4); the carbon
        // aging pair to carbchem (BC1 <-> BC2, OC1 <-> OC2). A species the
        // mechanism does not advance cannot double-apply, so the reported
        // overlap is the intersection with `mechanism_species`.
        static const std::pair<const char*, std::vector<std::string>> gocart_chemistry[] = {
            {"so4chem", {"DMS", "SO2", "SO4"}},
            {"carbchem", {"BC1", "BC2", "OC1", "OC2"}},
        };
        std::set<std::string> mechanism_set;
        for (const auto& s : mechanism_species)
            mechanism_set.insert(canonical_upper(s));

        std::vector<ProcessOrderConflict> conflicts;
        for (const auto& process : active_processes) {
            const std::string process_upper = canonical_upper(process);
            for (const auto& [name, advanced] : gocart_chemistry) {
                if (process_upper != canonical_upper(name))
                    continue;
                std::vector<std::string> shared;
                for (const auto& s : advanced)
                    if (mechanism_set.count(s))
                        shared.push_back(s);
                if (!shared.empty())
                    conflicts.push_back({name, std::move(shared)});
            }
        }
        return conflicts;
    }

    std::string describe_process_order_conflicts(const std::vector<ProcessOrderConflict>& conflicts) {
        std::string out;
        for (const auto& conflict : conflicts) {
            if (!out.empty())
                out += "; ";
            out += "mkppchem shares ";
            for (size_t i = 0; i < conflict.shared.size(); ++i)
                out += (i ? ", " : "") + conflict.shared[i];
            out += " with " + conflict.process;
        }
        return out;
    }
    namespace {

        /// The diagnostic name the photolysis process registers for one J-rate
        /// label. The prefix is the binding contract between the two processes;
        /// it appears nowhere else.
        constexpr std::string_view kPhotolysisDiagPrefix = "photolysis_rate_";

        bool has_photolysis_prefix(const std::string& name) {
            return name.size() > kPhotolysisDiagPrefix.size() &&
                   name.compare(0, kPhotolysisDiagPrefix.size(), kPhotolysisDiagPrefix) == 0;
        }

    } // namespace

    PhotolysisBinding plan_photolysis_binding(const MechanismDefinition& def, bool smoke_mode,
                                              const std::vector<std::string>& active_processes,
                                              const std::vector<std::string>& registered_diags) {
        PhotolysisBinding plan;
        const bool source_scheduled =
            std::any_of(active_processes.begin(), active_processes.end(), [](const std::string& p) {
                std::string lower(p);
                std::transform(lower.begin(), lower.end(), lower.begin(),
                               [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
                return lower == "photolysis";
            });
        const bool source_bound = std::any_of(registered_diags.begin(), registered_diags.end(), has_photolysis_prefix);
        const bool photolysis_source = source_scheduled || source_bound;

        if (smoke_mode) {
            // Smoke mode replaces the coupling with the definition's constants,
            // so a real photolysis source alongside it is a contradiction: the
            // computed rates would be silently discarded.
            if (photolysis_source)
                fatal("photolysis",
                      "configuration contradiction: photolysis_smoke_mode is true while a photolysis source is "
                      "also " +
                          std::string(source_scheduled ? "scheduled in the run phases" : "bound in diagnostics") +
                          "; smoke mode applies the definition's host-forcing constants and must never run "
                          "beside computed J-rates (definition '" + def.definition_path + "')");
            if (def.photolysis_labels.empty())
                return plan; // Nothing to force: the mechanism declares no photolysis.
            plan.smoke_mode = true;
            for (const auto& label : def.photolysis_labels) {
                const auto it = def.photolysis_host_forcing.find(label);
                plan.smoke_forcing.push_back(it == def.photolysis_host_forcing.end() ? 0.0 : it->second);
            }
            return plan;
        }

        if (def.photolysis_labels.empty())
            return plan; // Ungated mechanism: no coupling required.

        // The mechanism consumes J-rates but no photolysis source is
        // present. Zero-filled jvals would silently disable the OH production
        // the sulfur cycle depends on -- fail loud instead.
        if (!source_bound)
            fatal("photolysis", "mechanism '" + def.mkpp_mechanism_id + "' (definition '" + def.definition_path +
                                    "') declares " + std::to_string(def.photolysis_labels.size()) +
                                    " photolysis reaction(s) that require photolysis rates, but no active process "
                                    "provides them: no photolysis_rate_* diagnostics are registered. Activate the "
                                    "photolysis process before mkppchem, or set photolysis_smoke_mode for isolated "
                                    "mechanism testing.");

        // Bind every label to its diagnostic, in definition
        // declaration order -- index i of the returned list is jvals slot i.
        // A label whose field is absent is fatal naming the label and the
        // definition; map iteration order is never consulted.
        plan.diag_names.reserve(def.photolysis_labels.size());
        for (const auto& label : def.photolysis_labels) {
            std::string diag_name(kPhotolysisDiagPrefix);
            diag_name += label;
            const bool present =
                std::find(registered_diags.begin(), registered_diags.end(), diag_name) != registered_diags.end();
            if (!present)
                fatal("photolysis", "photolysis label '" + label + "' declared by definition '" +
                                        def.definition_path + "' has no bound diagnostic '" + diag_name +
                                        "'; the configured photolysis mechanism does not compute every label the "
                                        "chemistry consumes (check the TUV-x J-value configuration)");
            plan.diag_names.push_back(std::move(diag_name));
        }
        return plan;
    }
} // namespace catchem::mkpp

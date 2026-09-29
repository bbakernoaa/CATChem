// src/process/mkppchem/catchem_mkpp_mechanism_parse.cpp
//
// YAML parsing of an OpenAtmos mechanism definition. This is the ONLY mkppchem
// translation unit that uses yaml-cpp, and it is deliberately NOT compiled into
// the runtime process library: the process builds its MechanismDefinition from
// the generated metadata header (catchem_mkpp_mechanism_meta.hpp) instead of
// re-parsing the definition at runtime (the single-parse design). This file is
// compiled into the CI/unit-test target, which exercises the parser's
// failure-path diagnostics (malformed/missing/duplicate definitions) directly.
//
// The yaml-free provenance and mapping logic (validate_provenance,
// build_species_mapping, canonical_upper, definition_from_meta) lives in
// catchem_mkpp_mechanism.cpp.
#include "catchem_mkpp_mechanism.hpp"

#include <yaml-cpp/yaml.h>

#include <algorithm>
#include <cctype>
#include <set>
#include <stdexcept>
#include <string>

namespace catchem::mkpp {

    namespace {

        /// Wrap a reason in the project's fatal-diagnostic convention so the
        /// message is unambiguous in a run log and names the offending artifact.
        [[noreturn]] void fatal(const std::string& context, const std::string& reason) {
            throw std::runtime_error("FATAL ERROR: mkppchem " + context + ": " + reason);
        }

        bool contains_cloud_water(const std::string& trigger) {
            // The OpenAtmos trigger names the met quantity, e.g.
            // "meteo.cloud_liquid_water > 1.0e-6". Case-insensitive substring
            // match is sufficient: the quantity name is a fixed schema token.
            std::string lower = trigger;
            std::transform(lower.begin(), lower.end(), lower.begin(),
                           [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
            return lower.find("cloud_liquid_water") != std::string::npos;
        }

        ReactionType reaction_type_from_string(const std::string& t) {
            if (t == "ARRHENIUS")
                return ReactionType::Arrhenius;
            if (t == "HETEROGENEOUS")
                return ReactionType::Heterogeneous;
            if (t == "PHOTOLYSIS")
                return ReactionType::Photolysis;
            if (t == "EQUILIBRIUM")
                return ReactionType::Equilibrium;
            return ReactionType::Unknown;
        }

        std::string species_name_of(const YAML::Node& entry, const std::string& path) {
            if (!entry.IsMap() || !entry["species name"])
                fatal("definition parse (" + path + ")", "reaction participant is missing a 'species name'");
            return entry["species name"].as<std::string>();
        }

        /// The OpenAtmos definition is YAML by contract. JSON is a YAML subset
        /// and would load silently, but accepting it would let a migrated
        /// `mechanism.json` (a derived artifact) masquerade as the source of
        /// truth; reject the extension so the provenance chain stays explicit.
        void require_yaml_extension(const std::string& path) {
            const auto dot = path.find_last_of('.');
            const std::string ext = (dot == std::string::npos) ? std::string() : path.substr(dot + 1);
            std::string lower = ext;
            std::transform(lower.begin(), lower.end(), lower.begin(),
                           [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
            if (lower != "yaml" && lower != "yml")
                fatal("definition parse (" + path + ")",
                      "OpenAtmos definitions must be YAML (.yaml/.yml); got '." + ext +
                          "'. JSON is a derived migration format, not the source of truth.");
        }

    } // namespace

    MechanismDefinition parse_openatmos_definition(const std::string& path) {
        require_yaml_extension(path);
        YAML::Node root;
        try {
            root = YAML::LoadFile(path);
        } catch (const YAML::Exception& e) {
            fatal("definition parse", "cannot read/parse '" + path + "': " + e.what());
        }

        MechanismDefinition def;
        def.definition_path = path;

        if (!root["name"] || !root["name"].IsScalar())
            fatal("definition parse (" + path + ")", "missing or non-scalar top-level 'name'");
        def.name = root["name"].as<std::string>();

        // Metadata: mechanism id is required for provenance; the provenance
        // digest and host-forcing map are optional.
        if (root["metadata"] && root["metadata"]["mkpp_mechanism_id"])
            def.mkpp_mechanism_id = root["metadata"]["mkpp_mechanism_id"].as<std::string>();
        else
            fatal("definition parse (" + path + ")", "metadata.mkpp_mechanism_id is required");
        if (root["metadata"] && root["metadata"]["legacy_source_sha256"])
            def.legacy_source_sha256 = root["metadata"]["legacy_source_sha256"].as<std::string>();
        if (root["metadata"] && root["metadata"]["photolysis_host_forcing_expressions"]) {
            for (const auto& kv : root["metadata"]["photolysis_host_forcing_expressions"]) {
                const std::string label = kv.first.as<std::string>();
                const std::string value = kv.second.as<std::string>();
                try {
                    def.photolysis_host_forcing[label] = std::stod(value);
                } catch (const std::exception&) {
                    fatal("definition parse (" + path + ")",
                          "photolysis host forcing for '" + label + "' is not a finite number: '" + value + "'");
                }
            }
        }

        // Species: declaration order is the mechanism slot order.
        if (!root["species"] || !root["species"].IsSequence())
            fatal("definition parse (" + path + ")", "'species' must be a non-empty sequence");
        std::set<std::string> seen_upper;
        for (const auto& sp : root["species"]) {
            if (!sp["name"] || !sp["name"].IsScalar())
                fatal("definition parse (" + path + ")", "a species entry is missing a scalar 'name'");
            const std::string raw = sp["name"].as<std::string>();
            const std::string upper = canonical_upper(raw);
            if (upper.empty())
                fatal("definition parse (" + path + ")", "species name is empty");
            if (!seen_upper.insert(upper).second)
                fatal("definition parse (" + path + ")", "duplicate species name (case-insensitive): '" + raw + "'");
            def.species_order.push_back(raw);
        }
        if (def.species_order.empty())
            fatal("definition parse (" + path + ")", "'species' sequence is empty");

        // Reactions: classify, collect photolysis labels in declaration order,
        // and detect the cloud-water gate and equilibrium inputs.
        if (!root["reactions"] || !root["reactions"].IsSequence())
            fatal("definition parse (" + path + ")", "'reactions' must be a sequence");
        for (const auto& rxn : root["reactions"]) {
            if (!rxn["type"] || !rxn["type"].IsScalar())
                fatal("definition parse (" + path + ")", "a reaction is missing a scalar 'type'");
            const std::string t = rxn["type"].as<std::string>();
            const ReactionType type = reaction_type_from_string(t);
            if (type == ReactionType::Unknown)
                fatal("definition parse (" + path + ")", "unknown reaction type '" + t + "'");
            def.reaction_types.push_back(type);

            // Every reactant/product must resolve to a declared species.
            for (const char* key : {"reactants", "products"}) {
                if (!rxn[key])
                    continue;
                for (const auto& part : rxn[key]) {
                    const std::string pname = species_name_of(part, path);
                    const std::string pkey = canonical_upper(pname);
                    const bool found =
                        std::any_of(def.species_order.begin(), def.species_order.end(),
                                    [&pkey](const std::string& s) { return canonical_upper(s) == pkey; });
                    if (!found)
                        fatal("definition parse (" + path + ")",
                              "reaction " + std::string(key) + " references undeclared species '" + pname + "'");
                }
            }

            if (type == ReactionType::Photolysis) {
                if (!rxn["name"] || !rxn["name"].IsScalar())
                    fatal("definition parse (" + path + ")", "a PHOTOLYSIS reaction is missing a scalar 'name' label");
                def.photolysis_labels.push_back(rxn["name"].as<std::string>());
            }

            if (rxn["activation_trigger"] && rxn["activation_trigger"].IsScalar()) {
                const std::string trigger = rxn["activation_trigger"].as<std::string>();
                if (contains_cloud_water(trigger))
                    def.has_cloud_gated_reaction = true;
            }
        }

        // Equilibrium input names: the definition's cloud gate adds
        // cloud_liquid_water; temperature and humidity are always consumed by
        // the solver's per-cell parameters.
        def.equilibrium_inputs = {"Temperature", "RelativeHumidity"};
        if (def.has_cloud_gated_reaction)
            def.equilibrium_inputs.push_back("CloudLiquidWater");

        // Total reaction count, mirroring the meta header's num_reactions so the
        // init log reads the same number from either source.
        def.num_reactions = static_cast<int>(def.reaction_types.size());

        return def;
    }

} // namespace catchem::mkpp

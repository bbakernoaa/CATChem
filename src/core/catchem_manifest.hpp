/**
 * @file catchem_manifest.hpp
 * @brief Metadata-only, immutable summary of a configured mechanism.
 *
 * @details MechanismManifest parses a configuration (plus its species file)
 * and validates it, then keeps only the descriptive result: species names,
 * CCPP standard names, units, representations, and the process activation
 * list. Building it creates no Core, StateManager, solver, Kokkos lease, or
 * process instance, so drivers and tooling can inspect a mechanism cheaply
 * before committing resources. The fingerprint is a stable FNV-1a hash over
 * the configuration file contents (or the in-memory species metadata when no
 * file backs the configuration), intended to detect ordinary edits — not for
 * adversarial collision resistance.
 */
#pragma once

#include "catchem_config_manager.hpp"
#include "catchem_species_metadata.hpp"
#include <cstdint>
#include <string>
#include <unordered_map>
#include <vector>

namespace catchem {

    /**
     * @brief Stable fingerprint identifying one parsed configuration.
     *
     * @param config ConfigManager whose config_file_path (and, when set, the
     *               file's bytes plus the resolved species file) are hashed.
     * @return 16 lowercase hex characters. When config_file_path is empty the
     *         mechanism identity and every species attribute are hashed from
     *         memory instead, so in-memory mechanisms still differ.
     *
     * @note Throws std::invalid_argument when a named dependency file cannot
     * be opened. The hash is FNV-1a over bytes; format changes to the files
     * (whitespace, comments) change the fingerprint.
     */
    std::string configuration_fingerprint(const ConfigManager& config);

    /// One species as described by a manifest (no concentration storage).
    struct ManifestSpecies {
        std::string name;                            ///< Canonical short name.
        std::string standard_name;                   ///< CCPP/CF standard name.
        std::string units;                           ///< Configured concentration units.
        std::vector<std::string> aliases;            ///< Alternate accepted names.
        std::vector<std::string> roles;              ///< Functional role labels.
        SpeciesRepresentation host_representation;   ///< Representation of the host field.
        std::map<std::string, SpeciesRepresentation> process_representations; ///< Per-process representations.
        bool gas = false;      ///< Gas-phase species.
        bool aerosol = false;  ///< Aerosol-phase species.
        bool advected = true;  ///< Participates in transport.
        double molecular_weight_kg_mol = 0.0; ///< Molecular weight [kg mol-1].
    };

    /// One process block as described by a manifest.
    struct ManifestProcess {
        std::string name;      ///< Process key as written under the processes block.
        bool available = false;///< True when the process block sets activate.
        std::string scheme;    ///< Selected scheme name (empty when unset).
    };

    /**
     * @brief Immutable, metadata-only view of a configured mechanism. Constructing this
     * object parses configuration and species metadata but never creates Core,
     * StateManager, a solver, Kokkos lease, or process instance.
     */
    class MechanismManifest {
    public:
        /**
         * @brief Parse, load, validate, and summarise one configuration file.
         *
         * @param config_path Path to the top-level CATChem YAML configuration;
         *                    must be non-empty. The species file named under
         *                    simulation/species_filename is resolved relative
         *                    to this path and loaded too.
         *
         * @note Throws std::invalid_argument on an empty path, an empty
         * species/standard name, units outside the supported set (kg kg-1,
         * mol mol-1, ppmv, ug kg-1, kg m-3, mol m-3, or empty), a name or
         * alias collision, or an empty mechanism; std::out_of_range and the
         * ConfigManager validation errors propagate from validate_or_throw().
         * A fully constructed manifest is read-only afterwards.
         */
        explicit MechanismManifest(const std::string& config_path);

        const std::string& fingerprint() const noexcept { return fingerprint_; } ///< 16-hex configuration fingerprint.
        const std::string& identity() const noexcept { return identity_; }      ///< Mechanism identity, "configured" when unset.
        const std::vector<ManifestSpecies>& species() const noexcept { return species_; }   ///< Species in index order.
        const std::vector<ManifestProcess>& processes() const noexcept { return processes_; } ///< Process blocks from configuration.

        /**
         * @brief Resolve a species name or alias to its manifest index.
         * @param name_or_alias Species short name or alias; case-insensitive.
         * @return 0-based position in species().
         * @throws std::out_of_range when the name is not in the mechanism.
         */
        std::size_t species_index(const std::string& name_or_alias) const;

    private:
        std::string identity_;                                ///< Mechanism identity string.
        std::string fingerprint_;                             ///< Configuration fingerprint.
        std::vector<ManifestSpecies> species_;                ///< Manifest species list.
        std::vector<ManifestProcess> processes_;              ///< Manifest process list.
        std::unordered_map<std::string, std::size_t> name_index_; ///< Canonical names/aliases -> species position.
    };

} // namespace catchem

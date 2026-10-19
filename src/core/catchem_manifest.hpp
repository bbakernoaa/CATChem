#pragma once

#include "catchem_config_manager.hpp"
#include "catchem_species_metadata.hpp"
#include <cstdint>
#include <string>
#include <unordered_map>
#include <vector>

namespace catchem {

    std::string configuration_fingerprint(const ConfigManager& config);

    struct ManifestSpecies {
        std::string name;
        std::string standard_name;
        std::string units;
        std::vector<std::string> aliases;
        std::vector<std::string> roles;
        SpeciesRepresentation host_representation;
        std::map<std::string, SpeciesRepresentation> process_representations;
        bool gas = false;
        bool aerosol = false;
        bool advected = true;
        double molecular_weight_kg_mol = 0.0;
    };

    struct ManifestProcess {
        std::string name;
        bool available = false;
        std::string scheme;
    };

    /// Immutable, metadata-only view of a configured mechanism. Constructing this
    /// object parses configuration and species metadata but never creates Core,
    /// StateManager, a solver, Kokkos lease, or process instance.
    class MechanismManifest {
    public:
        explicit MechanismManifest(const std::string& config_path);

        const std::string& fingerprint() const noexcept { return fingerprint_; }
        const std::string& identity() const noexcept { return identity_; }
        const std::vector<ManifestSpecies>& species() const noexcept { return species_; }
        const std::vector<ManifestProcess>& processes() const noexcept { return processes_; }
        std::size_t species_index(const std::string& name_or_alias) const;

    private:
        std::string identity_;
        std::string fingerprint_;
        std::vector<ManifestSpecies> species_;
        std::vector<ManifestProcess> processes_;
        std::unordered_map<std::string, std::size_t> name_index_;
    };

} // namespace catchem

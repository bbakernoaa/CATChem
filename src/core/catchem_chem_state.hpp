#pragma once
#include "catchem_config_manager.hpp"
#include "catchem_manifest.hpp"
#include "catchem_interop_field.hpp"
#include "catchem_species_metadata.hpp"
#include <algorithm>
#include <cctype>
#include <memory>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

namespace catchem {

    struct ChemState {
        // Single unified 3D View (cols, levels, species)
        std::shared_ptr<InteropField<double, 3>> conc;
        std::shared_ptr<const MechanismDefinition> mechanism;

        // Species metadata database
        std::vector<SpeciesMetadata> species_list;
        std::unordered_map<std::string, int> species_name_to_index; // 0-based indexing

        // Pre-filtered category lists (0-based)
        std::vector<int> gas_indices;
        std::vector<int> aerosol_indices;
        std::vector<int> settling_indices;
        std::vector<int> tracer_indices;
        std::vector<int> advected_indices;
        std::vector<int> drydep_indices;
        std::vector<int> wetdep_indices;
        std::vector<int> photolysis_indices;
        std::vector<int> dust_indices;
        std::vector<int> seasalt_indices;

        // Cached flat C-character array of short names
        std::vector<char> species_names_c_arr;

        void load_from_config_manager(const ConfigManager& config_mgr) {
            species_list.clear();
            species_name_to_index.clear();

            gas_indices.clear();
            aerosol_indices.clear();
            settling_indices.clear();
            tracer_indices.clear();
            advected_indices.clear();
            drydep_indices.clear();
            wetdep_indices.clear();
            photolysis_indices.clear();
            dust_indices.clear();
            seasalt_indices.clear();

            int index = 0;
            auto descriptor = std::make_shared<MechanismDefinition>();
            descriptor->identity =
                config_mgr.data.mechanism_identity.empty() ? "configured" : config_mgr.data.mechanism_identity;
            descriptor->source = config_mgr.config_file_path;
            descriptor->fingerprint = configuration_fingerprint(config_mgr);
            for (const auto& capability : config_mgr.data.mechanism_capabilities)
                descriptor->capabilities.insert(canonical_species_name(capability));
            for (const auto& sp : config_mgr.data.species) {
                SpeciesMetadata meta = make_species_metadata(sp);

                species_list.push_back(meta);
                descriptor->species.push_back(meta);
                std::string canonical_name = meta.short_name;
                std::transform(canonical_name.begin(), canonical_name.end(), canonical_name.begin(),
                               [](unsigned char c) { return std::toupper(c); });
                if (canonical_name.empty())
                    throw std::invalid_argument("Chemical mechanism contains an empty species name");
                if (!species_name_to_index.emplace(canonical_name, index).second)
                    throw std::invalid_argument("Chemical mechanism contains duplicate species name: " +
                                                meta.short_name);

                // Classify species
                if (meta.is_gas)
                    gas_indices.push_back(index);
                if (meta.is_aerosol)
                    aerosol_indices.push_back(index);
                if (meta.is_aerosol && meta.do_settling)
                    settling_indices.push_back(index);
                if (meta.is_tracer)
                    tracer_indices.push_back(index);
                if (meta.is_advected)
                    advected_indices.push_back(index);
                if (meta.is_drydep)
                    drydep_indices.push_back(index);
                if (meta.is_wetdep)
                    wetdep_indices.push_back(index);
                if (meta.is_photolysis)
                    photolysis_indices.push_back(index);
                if (meta.is_dust)
                    dust_indices.push_back(index);
                if (meta.is_seasalt)
                    seasalt_indices.push_back(index);

                index++;
            }
            descriptor->rebuild_index();
            mechanism = descriptor;

            // Pre-compute and cache flat C-linkable species name character array
            species_names_c_arr.assign(species_list.size() * 32, ' ');
            for (size_t i = 0; i < species_list.size(); ++i) {
                std::string name = species_list[i].short_name;
                for (auto& c : name)
                    c = std::toupper(c);
                for (size_t j = 0; j < name.size() && j < 32; ++j) {
                    species_names_c_arr[i * 32 + j] = name[j];
                }
            }
        }

        void load_species_config(const std::string& filename, ConfigManager* cfg = nullptr) {
            if (cfg) {
                cfg->load_species_file(filename);
                load_from_config_manager(*cfg);
            } else {
                ConfigManager temp_cfg;
                temp_cfg.load_species_file(filename);
                load_from_config_manager(temp_cfg);
            }
        }
    };

} // namespace catchem

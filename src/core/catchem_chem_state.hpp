/**
 * @file catchem_chem_state.hpp
 * @brief Chemical mechanism state: species metadata, categories, and the
 *        unified concentration field.
 *
 * @details ChemState owns the runtime view of one configured mechanism. The
 * concentration array is a single 3D InteropField (column, level, species)
 * whose units are species-native as declared in the mechanism file;
 * StateManager binds it from host memory and this struct never allocates it.
 * Species classification (gas, aerosol, deposition, photolysis, ...) is
 * pre-filtered into 0-based index vectors at load time so process loops can
 * iterate a category without re-testing flags. The MechanismDefinition and the
 * flat Fortran-name buffer are rebuilt from scratch on every load; loading is
 * not thread-safe with respect to concurrent readers.
 *
 * @see catchem_species_metadata.hpp for SpeciesMetadata and MechanismDefinition,
 * catchem_config_manager.hpp for the YAML source of the data.
 */
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

    /**
     * @brief Species metadata and concentration storage for the active mechanism.
     *
     * @details Plain aggregate owned by StateManager. conc is a shared view
     * over host memory (never owned here); mechanism and the index vectors
     * are rebuilt wholesale by load_from_config_manager().
     */
    struct ChemState {
        /// Single unified 3D View (cols, levels, species)
        std::shared_ptr<InteropField<double, 3>> conc;                ///< Host-bound concentration array, null until bind_unified_chemistry().
        std::shared_ptr<const MechanismDefinition> mechanism;         ///< Immutable indexed mechanism, rebuilt on every load.

        /// Species metadata database
        std::vector<SpeciesMetadata> species_list;                        ///< Metadata in concentration-array index order.
        std::unordered_map<std::string, int> species_name_to_index; //!< Uppercased short names to 0-based index.

        /// Pre-filtered category lists (0-based species indices)
        std::vector<int> gas_indices;         ///< is_gas species.
        std::vector<int> aerosol_indices;     ///< is_aerosol species.
        std::vector<int> settling_indices;    ///< Aerosols with do_settling.
        std::vector<int> tracer_indices;      ///< is_tracer species.
        std::vector<int> advected_indices;    ///< is_advected species.
        std::vector<int> drydep_indices;      ///< is_drydep species.
        std::vector<int> wetdep_indices;      ///< is_wetdep species.
        std::vector<int> photolysis_indices;  ///< is_photolysis species.
        std::vector<int> dust_indices;        ///< is_dust species.
        std::vector<int> seasalt_indices;     ///< is_seasalt species.

        /// Pre-compute and cache flat C-linkable species name character array
        std::vector<char> species_names_c_arr; //!< 32 blank-padded uppercase chars per species, for Fortran interop.

        /**
         * @brief Rebuild the whole state from a parsed configuration.
         *
         * @param config_mgr Fully populated ConfigManager; only borrowed for
         *        the duration of the call (its data is copied into this state).
         *
         * @details Clears every list and map, then walks
         * config_mgr.data.species in order, converting each entry through
         * make_species_metadata() and appending it to species_list. The
         * uppercase short name becomes the key of species_name_to_index (its
         * 0-based position) and selects the category index vectors. A
         * MechanismDefinition is assembled in parallel with identity
         * "configured" when the mechanism block carries none, source = the
         * config file path, a configuration_fingerprint() value, and the
         * declared capabilities canonicalised to uppercase.
         *
         * @note Throws std::invalid_argument on an empty species name or a
         * name that duplicates an earlier one after case folding; the state is
         * left partially rebuilt in that case, so callers must treat a throw
         * as fatal. Finally caches a 32-character-per-species blank-padded,
         * uppercase name array for the Fortran interop layer
         * (species_names_c_arr[i * 32 + j]).
         */
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

        /**
         * @brief Load a species mechanism YAML file and rebuild the state.
         *
         * @param filename  Path to a CATChem species file (sequence of species
         *                  entries); opened with yaml-cpp, must exist.
         * @param cfg       Optional existing ConfigManager to reuse. When
         *                  provided, the species are merged into it and the
         *                  state is loaded from it; when null a temporary
         *                  ConfigManager is used, so the mechanism identity,
         *                  source path, and fingerprint fall back to
         *                  "configured"/empty defaults.
         *
         * @note Any species already held by cfg remain; load_species_file()
         * clears and replaces only the species list.
         */
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

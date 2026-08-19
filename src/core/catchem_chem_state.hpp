#pragma once
#include "catchem_config_manager.hpp"
#include "catchem_interop_field.hpp"
#include "catchem_species_metadata.hpp"
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

namespace catchem {

    struct ChemState {
        // Single unified 3D View (cols, levels, species)
        std::shared_ptr<InteropField<double, 3>> conc;

        // Species metadata database
        std::vector<SpeciesMetadata> species_list;
        std::unordered_map<std::string, int> species_name_to_index; // 0-based indexing

        // Pre-filtered category lists (0-based)
        std::vector<int> gas_indices;
        std::vector<int> aerosol_indices;
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
            tracer_indices.clear();
            advected_indices.clear();
            drydep_indices.clear();
            wetdep_indices.clear();
            photolysis_indices.clear();
            dust_indices.clear();
            seasalt_indices.clear();

            int index = 0;
            for (const auto& sp : config_mgr.data.species) {
                SpeciesMetadata meta;
                meta.short_name = sp.name;
                meta.long_name = sp.long_name.empty() ? sp.name : sp.long_name;
                meta.description = sp.description;

                meta.is_gas = sp.is_gas;
                meta.is_aerosol = sp.is_aerosol;
                meta.is_tracer = sp.is_tracer;
                meta.is_advected = sp.is_advected;
                meta.is_drydep = sp.is_drydep;
                meta.is_wetdep = sp.is_wetdep;
                meta.is_photolysis = sp.is_photolysis;
                meta.is_gocart_aero = sp.is_gocart_aero;
                meta.is_dust = sp.is_dust;
                meta.is_seasalt = sp.is_seasalt;

                meta.mw_g = sp.mw_g > 0.0 ? sp.mw_g : sp.molecular_weight_kg_mol * 1000.0;
                meta.density = sp.density;
                meta.radius = sp.radius;
                meta.lower_radius = sp.lower_radius;
                meta.upper_radius = sp.upper_radius;
                meta.viscosity = sp.viscosity;

                meta.dd_f0 = sp.dd_f0;
                meta.dd_hstar = sp.dd_hstar;
                meta.dd_DvzAerSnow = sp.dd_DvzAerSnow;
                meta.dd_DvzMinVal_snow = sp.dd_DvzMinVal_snow;
                meta.dd_DvzMinVal_land = sp.dd_DvzMinVal_land;

                meta.henry_k0 = sp.henry_k0;
                meta.henry_cr = sp.henry_cr;
                meta.henry_pKa = sp.henry_pKa;
                meta.wd_retfactor = sp.wd_retfactor;
                meta.wd_LiqAndGas = sp.wd_LiqAndGas;
                meta.wd_convfacI2G = sp.wd_convfacI2G;
                meta.wd_rainouteff = sp.wd_rainouteff;
                meta.wd_reevap_frac = sp.wd_reevap_frac;

                meta.t_chem_loss = sp.t_chem_loss;
                meta.BackgroundVV = sp.BackgroundVV;
                meta.mie_name = sp.mie_name;

                species_list.push_back(meta);
                species_name_to_index[meta.short_name] = index;

                // Classify species
                if (meta.is_gas)
                    gas_indices.push_back(index);
                if (meta.is_aerosol)
                    aerosol_indices.push_back(index);
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

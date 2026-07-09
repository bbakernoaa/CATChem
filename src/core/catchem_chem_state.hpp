#pragma once
#include <string>
#include <vector>
#include <unordered_map>
#include <memory>
#include <yaml-cpp/yaml.h>
#include "catchem_interop_field.hpp"
#include "catchem_species_metadata.hpp"

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

    void load_species_config(const std::string& filename) {
        YAML::Node config = YAML::LoadFile(filename);
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
        for (auto const& item : config) {
            std::string key = item.first.as<std::string>();
            YAML::Node val = item.second;

            SpeciesMetadata meta;
            meta.short_name = key;
            meta.long_name = val["name"] ? val["name"].as<std::string>() : key;
            meta.description = val["description"] ? val["description"].as<std::string>() : "";

            meta.is_gas = val["is_gas"] ? val["is_gas"].as<bool>() : false;
            meta.is_aerosol = val["is_aerosol"] ? val["is_aerosol"].as<bool>() : false;
            meta.is_tracer = val["is_tracer"] ? val["is_tracer"].as<bool>() : false;
            meta.is_advected = val["is_advected"] ? val["is_advected"].as<bool>() : true;
            meta.is_drydep = val["is_drydep"] ? val["is_drydep"].as<bool>() : false;
            meta.is_wetdep = val["is_wetdep"] ? val["is_wetdep"].as<bool>() : false;
            meta.is_photolysis = val["is_photolysis"] ? val["is_photolysis"].as<bool>() : false;
            meta.is_dust = val["is_dust"] ? val["is_dust"].as<bool>() : false;
            meta.is_seasalt = val["is_seasalt"] ? val["is_seasalt"].as<bool>() : false;

            meta.mw_g = val["mw_g"] ? val["mw_g"].as<double>() : 0.0;
            meta.density = val["density"] ? val["density"].as<double>() : 0.0;
            meta.radius = val["radius"] ? val["radius"].as<double>() : 0.0;
            meta.lower_radius = val["lower_radius"] ? val["lower_radius"].as<double>() : 0.0;
            meta.upper_radius = val["upper_radius"] ? val["upper_radius"].as<double>() : 0.0;
            meta.viscosity = val["viscosity"] ? val["viscosity"].as<double>() : 0.0;

            meta.dd_f0 = val["dd_f0"] ? val["dd_f0"].as<double>() : 0.0;
            meta.dd_hstar = val["dd_hstar"] ? val["dd_hstar"].as<double>() : 0.0;
            meta.dd_DvzAerSnow = val["dd_DvzAerSnow"] ? val["dd_DvzAerSnow"].as<double>() : 0.0;
            meta.dd_DvzMinVal_snow = val["dd_DvzMinVal_snow"] ? val["dd_DvzMinVal_snow"].as<double>() : 0.0;
            meta.dd_DvzMinVal_land = val["dd_DvzMinVal_land"] ? val["dd_DvzMinVal_land"].as<double>() : 0.0;

            meta.wd_retfactor = val["wd_retfactor"] ? val["wd_retfactor"].as<double>() : 0.0;
            meta.wd_LiqAndGas = val["wd_LiqAndGas"] ? val["wd_LiqAndGas"].as<bool>() : false;
            meta.wd_convfacI2G = val["wd_convfacI2G"] ? val["wd_convfacI2G"].as<double>() : 0.0;
            
            if (val["wd_rainouteff"]) {
                meta.wd_rainouteff = val["wd_rainouteff"].as<std::vector<double>>();
            }
            meta.mie_name = val["mie_name"] ? val["mie_name"].as<std::string>() : "";

            species_list.push_back(meta);
            species_name_to_index[key] = index;

            // Classify species
            if (meta.is_gas) gas_indices.push_back(index);
            if (meta.is_aerosol) aerosol_indices.push_back(index);
            if (meta.is_tracer) tracer_indices.push_back(index);
            if (meta.is_advected) advected_indices.push_back(index);
            if (meta.is_drydep) drydep_indices.push_back(index);
            if (meta.is_wetdep) wetdep_indices.push_back(index);
            if (meta.is_photolysis) photolysis_indices.push_back(index);
            if (meta.is_dust) dust_indices.push_back(index);
            if (meta.is_seasalt) seasalt_indices.push_back(index);

            index++;
        }

        // Pre-compute and cache flat C-linkable species name character array
        species_names_c_arr.assign(species_list.size() * 32, ' ');
        for (size_t i = 0; i < species_list.size(); ++i) {
            std::string name = species_list[i].short_name;
            for (size_t j = 0; j < name.size() && j < 32; ++j) {
                species_names_c_arr[i * 32 + j] = name[j];
            }
        }
    }
};

} // namespace catchem

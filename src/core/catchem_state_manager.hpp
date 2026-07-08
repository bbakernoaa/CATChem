#pragma once
#include <unordered_map>
#include <string>
#include <memory>
#include <vector>
#include <yaml-cpp/yaml.h>
#include "catchem_interop_field.hpp"
#include "catchem_species_metadata.hpp"

namespace catchem {

class StateManager {
public:
    int n_cols;
    int n_levels;
    int n_species;

    std::unordered_map<std::string, std::shared_ptr<InteropField<double, 1>>> fields_1d;
    std::unordered_map<std::string, std::shared_ptr<InteropField<double, 2>>> fields_2d;
    std::unordered_map<std::string, std::shared_ptr<InteropField<double, 3>>> fields_3d;

    // Species metadata structures
    std::vector<SpeciesMetadata> species_list;
    std::unordered_map<std::string, int> species_name_to_index; // 0-based index

    // Category lists (0-based offsets)
    std::vector<int> gas_indices;
    std::vector<int> aerosol_indices;
    std::vector<int> tracer_indices;
    std::vector<int> advected_indices;
    std::vector<int> drydep_indices;
    std::vector<int> wetdep_indices;
    std::vector<int> photolysis_indices;
    std::vector<int> dust_indices;
    std::vector<int> seasalt_indices;

    StateManager(int nc, int nl, int ns) : n_cols(nc), n_levels(nl), n_species(ns) {}

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
    }

    void bind_field_1d(const std::string& name, double* ptr) {
        fields_1d[name] = std::make_shared<InteropField<double, 1>>(ptr, std::vector<int>{n_cols});
    }

    void bind_field_2d(const std::string& name, double* ptr) {
        fields_2d[name] = std::make_shared<InteropField<double, 2>>(ptr, std::vector<int>{n_cols, n_levels});
    }

    void bind_field_3d(const std::string& name, double* ptr) {
        fields_3d[name] = std::make_shared<InteropField<double, 3>>(ptr, std::vector<int>{n_cols, n_levels, n_species});
    }

    void sync_to_device() {
        for (auto& [k, v] : fields_1d) v->sync_to_device();
        for (auto& [k, v] : fields_2d) v->sync_to_device();
        for (auto& [k, v] : fields_3d) v->sync_to_device();
    }

    void sync_to_host() {
        for (auto& [k, v] : fields_1d) v->sync_to_host();
        for (auto& [k, v] : fields_2d) v->sync_to_host();
        for (auto& [k, v] : fields_3d) v->sync_to_host();
    }

    double* get_host_pointer_1d(const std::string& name) {
        if (fields_1d.find(name) == fields_1d.end()) return nullptr;
        return fields_1d.at(name)->host_view.data();
    }

    double* get_host_pointer_2d(const std::string& name) {
        if (fields_2d.find(name) == fields_2d.end()) return nullptr;
        return fields_2d.at(name)->host_view.data();
    }

    double* get_host_pointer_3d(const std::string& name) {
        if (fields_3d.find(name) == fields_3d.end()) return nullptr;
        return fields_3d.at(name)->host_view.data();
    }
};

} // namespace catchem

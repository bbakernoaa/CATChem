#include "catchem_config_manager.hpp"
#include <filesystem>
#include <iostream>

namespace catchem {

    void ConfigManager::load_from_file(const std::string& filename) {
        config_file_path = filename;
        std::filesystem::path config_dir = std::filesystem::path(filename).parent_path();

        try {
            YAML::Node config = YAML::LoadFile(filename);
            root_node = config;
            config_file_path = filename;
            if (config["simulation"]) {
                auto sim = config["simulation"];
                if (sim["species_filename"]) {
                    data.species_filename = sim["species_filename"].as<std::string>();
                }
                if (sim["emission_filename"]) {
                    data.emission_filename = sim["emission_filename"].as<std::string>();
                    std::filesystem::path resolved_emis = config_dir / data.emission_filename;
                    std::string target_emis = std::filesystem::exists(resolved_emis) ? resolved_emis.string() : data.emission_filename;
                    load_emission_mapping(target_emis);
                }
                if (sim["nx"]) {
                    data.runtime.nx = sim["nx"].as<int>();
                }
                if (sim["ny"]) {
                    data.runtime.ny = sim["ny"].as<int>();
                }
                if (sim["nz"]) {
                    data.runtime.nz = sim["nz"].as<int>();
                }
                if (sim["timestep"]) {
                    data.runtime.dt = sim["timestep"].as<double>();
                } else if (sim["dt"]) {
                    data.runtime.dt = sim["dt"].as<double>();
                }
                if (sim["nsteps"]) {
                    data.runtime.nsteps = sim["nsteps"].as<int>();
                }
            }
            is_loaded = true;
        } catch (const std::exception& e) {
            std::cerr << "Error loading configuration file " << filename << ": " << e.what() << std::endl;
            is_loaded = false;
            throw;
        }
    }

    void ConfigManager::load_emission_mapping(const std::string& filename) {
        try {
            YAML::Node emis_yaml = YAML::LoadFile(filename);
            data.emission_mapping.categories.clear();

            for (auto cat_it = emis_yaml.begin(); cat_it != emis_yaml.end(); ++cat_it) {
                EmissionCategoryMapping category;
                category.category_name = cat_it->first.as<std::string>();
                category.is_active = true;

                YAML::Node cat_node = cat_it->second;
                for (auto field_it = cat_node.begin(); field_it != cat_node.end(); ++field_it) {
                    EmisSpeciesMappingEntry entry;
                    entry.emission_field = field_it->first.as<std::string>();

                    YAML::Node field_node = field_it->second;
                    if (field_node["long_name"]) {
                        entry.long_name = field_node["long_name"].as<std::string>();
                    }
                    if (field_node["units"]) {
                        entry.units = field_node["units"].as<std::string>();
                    }

                    if (field_node["map"] && field_node["map"].IsSequence()) {
                        for (const auto& item : field_node["map"]) {
                            entry.map.push_back(item.as<std::string>());
                        }
                    }
                    if (field_node["scale"] && field_node["scale"].IsSequence()) {
                        for (const auto& item : field_node["scale"]) {
                            entry.scale.push_back(item.as<double>());
                        }
                    }
                    category.species_mappings.push_back(entry);
                }
                data.emission_mapping.categories.push_back(category);
            }
            data.emission_mapping.is_loaded = true;
        } catch (const std::exception& e) {
            std::cerr << "Error loading emission mapping file " << filename << ": " << e.what() << std::endl;
            data.emission_mapping.is_loaded = false;
        }
    }

} // namespace catchem

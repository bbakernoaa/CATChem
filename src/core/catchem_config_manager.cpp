#include "catchem_config_manager.hpp"
#include <filesystem>
#include <iostream>
#include <sstream>

namespace {

    YAML::Node get_node_by_path(const YAML::Node& root, const std::string& path) {
        if (!root || !root.IsMap()) return YAML::Node();
        YAML::Node current = root;
        std::stringstream ss(path);
        std::string token;
        while (std::getline(ss, token, '/')) {
            if (token.empty()) continue;
            if (current && current.IsMap() && current[token]) {
                current = current[token];
            } else {
                return YAML::Node();
            }
        }
        return current;
    }

} // namespace

namespace catchem {

    bool ConfigManager::get_bool_path(const std::string& path, bool default_val) const {
        if (!is_loaded) return default_val;
        YAML::Node node = get_node_by_path(root_node, path);
        if (node && node.IsScalar()) {
            try {
                return node.as<bool>();
            } catch (...) {
            }
        }
        return default_val;
    }

    std::string ConfigManager::get_string_path(const std::string& path, const std::string& default_val) const {
        if (!is_loaded) return default_val;
        YAML::Node node = get_node_by_path(root_node, path);
        if (node && node.IsScalar()) {
            try {
                return node.as<std::string>();
            } catch (...) {
            }
        }
        return default_val;
    }

    double ConfigManager::get_double_path(const std::string& path, double default_val) const {
        if (!is_loaded) return default_val;
        YAML::Node node = get_node_by_path(root_node, path);
        if (node && node.IsScalar()) {
            try {
                return node.as<double>();
            } catch (...) {
            }
        }
        return default_val;
    }

    int ConfigManager::get_int_path(const std::string& path, int default_val) const {
        if (!is_loaded) return default_val;
        YAML::Node node = get_node_by_path(root_node, path);
        if (node && node.IsScalar()) {
            try {
                return node.as<int>();
            } catch (...) {
            }
        }
        return default_val;
    }

    std::vector<std::string> ConfigManager::get_string_list_path(const std::string& path) const {
        std::vector<std::string> result;
        if (!is_loaded) return result;
        YAML::Node node = get_node_by_path(root_node, path);
        if (node && node.IsSequence()) {
            for (const auto& item : node) {
                try {
                    result.push_back(item.as<std::string>());
                } catch (...) {
                }
            }
        }
        return result;
    }

    void ConfigManager::load_from_file(const std::string& filename) {
        config_file_path = filename;
        std::filesystem::path config_dir = std::filesystem::path(filename).parent_path();

        try {
            YAML::Node config = YAML::LoadFile(filename);
            root_node = config;
            config_file_path = filename;
            is_loaded = true;

            if (config["simulation"]) {
                auto sim = config["simulation"];
                if (sim["species_filename"]) {
                    data.species_filename = sim["species_filename"].as<std::string>();
                }
                if (sim["emission_filename"]) {
                    data.emission_filename = sim["emission_filename"].as<std::string>();
                    std::filesystem::path resolved_emis = config_dir / data.emission_filename;
                    std::string target_emis =
                        std::filesystem::exists(resolved_emis) ? resolved_emis.string() : data.emission_filename;
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

            // Diagnostics output settings
            if (config["diagnostics"] && config["diagnostics"]["output"]) {
                auto diag_out = config["diagnostics"]["output"];
                if (diag_out["enabled"]) {
                    data.runtime.DiagEnabled = diag_out["enabled"].as<bool>();
                }
                if (diag_out["frequency"]) {
                    data.runtime.Output_Frequency = diag_out["frequency"].as<double>();
                }
                if (diag_out["directory"]) {
                    data.file_paths.Output_Directory = diag_out["directory"].as<std::string>();
                }
                if (diag_out["prefix"]) {
                    data.file_paths.Output_Prefix = diag_out["prefix"].as<std::string>();
                }
                if (diag_out["compress_lev"]) {
                    data.runtime.CompressLev = diag_out["compress_lev"].as<int>();
                }
                if (diag_out["latlon_output"]) {
                    data.runtime.latlon_output = diag_out["latlon_output"].as<bool>();
                }
                if (diag_out["diag_list"] && diag_out["diag_list"].IsSequence()) {
                    data.runtime.diag_species.clear();
                    for (const auto& item : diag_out["diag_list"]) {
                        data.runtime.diag_species.push_back(item.as<std::string>());
                    }
                }
            }

            // Mie optics settings
            if (config["mie"]) {
                auto mie_node = config["mie"];
                data.mie.enabled = true;
                if (mie_node["directory"]) {
                    data.mie.directory = mie_node["directory"].as<std::string>();
                }
                if (mie_node["files"] && mie_node["files"].IsMap()) {
                    data.mie.files.clear();
                    for (auto it = mie_node["files"].begin(); it != mie_node["files"].end(); ++it) {
                        data.mie.files[it->first.as<std::string>()] = it->second.as<std::string>();
                    }
                }
            }

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

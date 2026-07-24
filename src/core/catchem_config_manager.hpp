// src/core/catchem_config_manager.hpp
#pragma once
#include <string>
#include <yaml-cpp/yaml.h>

namespace catchem {

    struct RuntimeConfig {
        int nx = 1;
        int ny = 1;
        int nz = 1;
        double dt = 3600.0;
        int nsteps = 1;
    };

    struct ConfigData {
        RuntimeConfig runtime;
        // We can add FilePathConfig, etc. here later
    };

    class ConfigManager {
    public:
        ConfigData data;
        YAML::Node root_node;
        bool is_loaded = false;

        ConfigManager() = default;
        void load_from_file(const std::string& filename);

        YAML::Node get_process_config(const std::string& process_name) const {
            if (is_loaded && root_node["process"] && root_node["process"][process_name]) {
                return root_node["process"][process_name];
            }
            return YAML::Node();
        }
    };

} // namespace catchem

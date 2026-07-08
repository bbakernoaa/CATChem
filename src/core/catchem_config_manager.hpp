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
    bool is_loaded = false;

    ConfigManager() = default;
    void load_from_file(const std::string& filename);
};

} // namespace catchem

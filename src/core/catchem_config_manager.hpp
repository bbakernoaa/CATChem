#pragma once
#include <string>
#include <string_view>
#include <vector>
#include <yaml-cpp/yaml.h>

namespace catchem {

    /// @brief Single emission species mapping entry.
    struct EmisSpeciesMappingEntry {
        std::string emission_field;
        std::string long_name;
        std::string units = "kg/m2/s";
        std::vector<std::string> map;
        std::vector<double> scale;
    };

    /// @brief Category mapping containing species mapping entries.
    struct EmissionCategoryMapping {
        std::string category_name;
        bool is_active = true;
        std::vector<EmisSpeciesMappingEntry> species_mappings;
    };

    /// @brief Container for loaded emission mapping configuration.
    struct EmissionMappingConfig {
        bool is_loaded = false;
        std::vector<EmissionCategoryMapping> categories;
    };

    /// @brief Runtime simulation configuration parameters.
    struct RuntimeConfig {
        int nx = 1;
        int ny = 1;
        int nz = 1;
        double dt = 3600.0;
        int nsteps = 1;
    };

    /// @brief Aggregated configuration data parsed from YAML files.
    struct ConfigData {
        RuntimeConfig runtime;
        std::string species_filename;  ///< simulation:species_filename, as written in the YAML
        std::string emission_filename; ///< simulation:emission_filename, as written in the YAML
        EmissionMappingConfig emission_mapping;
    };

    /// @brief Manages loading and accessing application YAML configuration settings.
    class ConfigManager {
    public:
        ConfigData data;
        YAML::Node root_node;
        bool is_loaded = false;
        std::string config_file_path;

        ConfigManager() = default;

        /// @brief Load top-level simulation configuration from a YAML file.
        /// @param filename Path to the configuration YAML file.
        void load_from_file(const std::string& filename);

        /// @brief Load emission mapping definitions from a YAML file.
        /// @param filename Path to the emission mapping YAML file.
        void load_emission_mapping(const std::string& filename);

        /// @brief Get process configuration sub-node.
        /// @param process_name Name of the target process.
        /// @return YAML::Node containing process options or empty node.
        YAML::Node get_process_config(std::string_view process_name) const {
            if (is_loaded && root_node["process"]) {
                std::string key(process_name);
                if (root_node["process"][key]) {
                    return root_node["process"][key];
                }
            }
            return YAML::Node();
        }
    };

} // namespace catchem

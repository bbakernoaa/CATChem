#pragma once
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>
#include <yaml-cpp/yaml.h>

namespace catchem {

    /// @brief Mie aerosol optics configuration parameters.
    struct MieConfig {
        bool enabled = false;
        std::string directory = "./ExtData/monochromatic/";
        std::unordered_map<std::string, std::string> files;
    };

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
        double Output_Frequency = 3600.0;
        int CompressLev = 0;
        bool latlon_output = true;
        bool DiagEnabled = false;
        std::vector<std::string> diag_species;
    };

    /// @brief Output file paths configuration.
    struct FilePathConfig {
        std::string Output_Directory = "./output";
        std::string Output_Prefix = "catchem_diag";
    };

    /// @brief Aggregated configuration data parsed from YAML files.
    struct ConfigData {
        RuntimeConfig runtime;
        FilePathConfig file_paths;
        std::string species_filename;  ///< simulation:species_filename, as written in the YAML
        std::string emission_filename; ///< simulation:emission_filename, as written in the YAML
        EmissionMappingConfig emission_mapping;
        MieConfig mie;
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

        /// @brief Evaluate boolean value at a slash-separated YAML path.
        bool get_bool_path(const std::string& path, bool default_val = false) const;

        /// @brief Evaluate string value at a slash-separated YAML path.
        std::string get_string_path(const std::string& path, const std::string& default_val = "") const;

        /// @brief Evaluate double floating-point value at a slash-separated YAML path.
        double get_double_path(const std::string& path, double default_val = 0.0) const;

        /// @brief Evaluate integer value at a slash-separated YAML path.
        int get_int_path(const std::string& path, int default_val = 0) const;

        /// @brief Evaluate string sequence list at a slash-separated YAML path.
        std::vector<std::string> get_string_list_path(const std::string& path) const;

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

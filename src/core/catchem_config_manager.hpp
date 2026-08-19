#pragma once
#include <map>
#include <string>
#include <string_view>
#include <vector>
#include <yaml-cpp/yaml.h>

namespace catchem {

    /// @brief Basic simulation file and verbosity settings from YAML.
    struct SimulationConfig {
        std::string name;
        std::string start_date;
        std::string end_date;
        std::string species_filename;
        std::string emission_filename;
        bool verbose_enabled = false;
    };

    struct RuntimeConfig {
        int nx = 1;
        int ny = 1;
        int nz = 1;
        double dt = 3600.0;
        int nsteps = 1;
    };

    /// @brief Grid dimensions and static grid settings from YAML.
    struct GridConfig {
        int number_of_levels = 1;
        int number_of_soil_layers = 0;
    };

    /// @brief Runtime timestep settings from YAML.
    struct TimestepConfig {
        int transport_timestep_in_s = 0;
        int chemistry_timestep_in_s = 0;
    };

    /// @brief Diagnostic output settings from YAML.
    struct DiagnosticOutputConfig {
        bool enabled = false;
        std::string directory;
        std::string prefix;
        int frequency = 0;
        std::string format;
        int compress_lev = 0;
        std::vector<std::string> diag_list;
    };

    /// @brief Diagnostic collection settings from YAML.
    struct DiagnosticCollectionConfig {
        bool enabled = false;
        int buffer_size = 0;
    };

    /// @brief Top-level diagnostic settings from YAML.
    struct DiagnosticsConfig {
        DiagnosticOutputConfig output;
        DiagnosticCollectionConfig collection;
    };

    /// @brief Process activation and nested process settings from YAML.
    struct ProcessConfig {
    private:
        YAML::Node settings_node;

    public:
        bool activate = false;
        bool diagnostics = false;
        std::string scheme;
        std::vector<std::string> diag_species;

        void set_settings_node(const YAML::Node& node) { settings_node = node; }
        const YAML::Node& get_settings_node() const { return settings_node; }

        bool get_bool(std::string_view key, bool default_val = false) const;
        double get_double(std::string_view key, double default_val = 0.0) const;
        int get_int(std::string_view key, int default_val = 0) const;
        std::string get_string(std::string_view key, std::string_view default_val = "") const;
    };

    /// @brief Species metadata loaded from a CATChem species YAML file.
    struct SpeciesConfig {
        std::string name;
        std::string long_name;
        std::string description;
        double molecular_weight_kg_mol = 0.0;
        double density = 0.0;
        double radius = 0.0;
        double lower_radius = 0.0;
        double upper_radius = 0.0;
        double viscosity = 0.0;
        bool is_gas = false;
        bool is_aerosol = false;
        bool is_dust = false;
        bool is_drydep = false;
        bool is_wetdep = false;
        bool is_advected = true;
        bool is_photolysis = false;
        std::string mie_name;
    };

    /// @brief Mapping for one external emission source field.
    struct EmissionFieldMapping {
        std::string long_name;
        std::string units;
        std::vector<double> scale;
        std::vector<std::string> map;
    };

    /// @brief Emission mapping category from a CATChem emission YAML file.
    struct EmissionCategoryMapping {
        std::map<std::string, EmissionFieldMapping> fields;
    };

    struct ConfigData {
        SimulationConfig simulation;
        RuntimeConfig runtime;
        GridConfig grid;
        TimestepConfig timesteps;
        DiagnosticsConfig diagnostics;
        std::string species_filename; ///< simulation:species_filename, as written in the YAML
        std::vector<std::string> active_processes;
        std::map<std::string, ProcessConfig> processes;
        std::vector<SpeciesConfig> species;
        std::map<std::string, EmissionCategoryMapping> emission_mappings;
    };

    class ConfigManager {
    private:
        YAML::Node root_node;

    public:
        ConfigData data;
        bool is_loaded = false;
        std::string config_file_path;

        ConfigManager() = default;
        void load_from_file(const std::string& filename);
        void load_species_file(const std::string& filename);
        void load_emission_mapping_file(const std::string& filename);

        const YAML::Node& get_root_node() const { return root_node; }

        // Safe path-based queries
        bool get_bool(std::string_view path, bool default_val = false) const;
        double get_double(std::string_view path, double default_val = 0.0) const;
        int get_int(std::string_view path, int default_val = 0) const;
        std::string get_string(std::string_view path, std::string_view default_val = "") const;
        std::vector<std::string> get_string_list(std::string_view path) const;

        // Structured process / emission queries
        bool is_process_active(std::string_view process_name) const;
        bool is_category_active(std::string_view category_name) const;
        std::string find_process_file_setting(std::string_view process_name) const;

        YAML::Node get_process_config(std::string_view process_name) const {
            if (!is_loaded) {
                return YAML::Node();
            }
            if (root_node["processes"]) {
                std::string key(process_name);
                if (root_node["processes"][key]) {
                    return root_node["processes"][key];
                }
            }
            if (root_node["process"]) {
                std::string key(process_name);
                if (root_node["process"][key]) {
                    return root_node["process"][key];
                }
            }
            return YAML::Node();
        }
    };

} // namespace catchem

#include "catchem_config_manager.hpp"
#include <iostream>

namespace catchem {

    namespace {

        template <typename T> T value_or(const YAML::Node& node, const T& default_value) {
            return node ? node.as<T>() : default_value;
        }

        std::vector<std::string> string_vector_or_empty(const YAML::Node& node) {
            std::vector<std::string> values;
            if (!node || !node.IsSequence()) {
                return values;
            }
            for (const auto& item : node) {
                values.push_back(item.as<std::string>());
            }
            return values;
        }

        std::vector<double> double_vector_or_empty(const YAML::Node& node) {
            std::vector<double> values;
            if (!node || !node.IsSequence()) {
                return values;
            }
            for (const auto& item : node) {
                values.push_back(item.as<double>());
            }
            return values;
        }

        void parse_processes(const YAML::Node& process_node, ConfigData& data) {
            if (!process_node || !process_node.IsMap()) {
                return;
            }
            for (const auto& process : process_node) {
                const std::string name = process.first.as<std::string>();
                const YAML::Node node = process.second;
                ProcessConfig config;
                config.activate = value_or<bool>(node["activate"], false);
                config.diagnostics = value_or<bool>(node["diagnostics"], false);
                if (node["scheme"]) {
                    config.scheme = node["scheme"].as<std::string>();
                }
                config.diag_species = string_vector_or_empty(node["diag_species"]);
                config.settings = node;
                data.processes[name] = config;
            }
        }

    } // namespace

    void ConfigManager::load_from_file(const std::string& filename) {
        config_file_path = filename;
        try {
            YAML::Node config = YAML::LoadFile(filename);
            root_node = config;
            config_file_path = filename;
            data.active_processes.clear();
            data.processes.clear();
            if (config["simulation"]) {
                auto sim = config["simulation"];
                data.simulation.name = value_or<std::string>(sim["name"], "");
                data.simulation.start_date = value_or<std::string>(sim["start_date"], "");
                data.simulation.end_date = value_or<std::string>(sim["end_date"], "");
                if (sim["species_filename"]) {
                    data.species_filename = sim["species_filename"].as<std::string>();
                    data.simulation.species_filename = data.species_filename;
                }
                if (sim["emission_filename"]) {
                    data.simulation.emission_filename = sim["emission_filename"].as<std::string>();
                }
                if (sim["verbose"]) {
                    data.simulation.verbose_enabled = value_or<bool>(sim["verbose"]["activate"], false);
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
            if (config["grid"]) {
                const YAML::Node grid = config["grid"];
                data.grid.number_of_levels = value_or<int>(grid["number_of_levels"], data.grid.number_of_levels);
                data.grid.number_of_soil_layers =
                    value_or<int>(grid["number_of_soil_layers"], data.grid.number_of_soil_layers);
                data.runtime.nz = data.grid.number_of_levels;
            }
            if (config["timesteps"]) {
                const YAML::Node timesteps = config["timesteps"];
                data.timesteps.transport_timestep_in_s =
                    value_or<int>(timesteps["transport_timestep_in_s"], data.timesteps.transport_timestep_in_s);
                data.timesteps.chemistry_timestep_in_s =
                    value_or<int>(timesteps["chemistry_timestep_in_s"], data.timesteps.chemistry_timestep_in_s);
            }
            if (config["diagnostics"]) {
                const YAML::Node diagnostics = config["diagnostics"];
                if (diagnostics["output"]) {
                    const YAML::Node output = diagnostics["output"];
                    data.diagnostics.output.enabled = value_or<bool>(output["enabled"], false);
                    data.diagnostics.output.directory = value_or<std::string>(output["directory"], "");
                    data.diagnostics.output.prefix = value_or<std::string>(output["prefix"], "");
                    data.diagnostics.output.frequency = value_or<int>(output["frequency"], 0);
                    data.diagnostics.output.format = value_or<std::string>(output["format"], "");
                    data.diagnostics.output.compress_lev = value_or<int>(output["compress_lev"], 0);
                    data.diagnostics.output.diag_list = string_vector_or_empty(output["diag_list"]);
                }
                if (diagnostics["collection"]) {
                    const YAML::Node collection = diagnostics["collection"];
                    data.diagnostics.collection.enabled = value_or<bool>(collection["enabled"], false);
                    data.diagnostics.collection.buffer_size = value_or<int>(collection["buffer_size"], 0);
                }
            }
            parse_processes(config["processes"], data);
            parse_processes(config["process"], data);
            if (config["run_phases"]) {
                for (const auto& phase : config["run_phases"]) {
                    const YAML::Node processes = phase.second["processes"];
                    if (!processes || !processes.IsSequence()) {
                        continue;
                    }
                    for (const auto& process : processes) {
                        data.active_processes.push_back(process.as<std::string>());
                    }
                }
            }
            is_loaded = true;
        } catch (const std::exception& e) {
            std::cerr << "Error loading configuration file " << filename << ": " << e.what() << std::endl;
            is_loaded = false;
            throw;
        }
    }

    void ConfigManager::load_species_file(const std::string& filename) {
        const YAML::Node species_root = YAML::LoadFile(filename);
        data.species.clear();
        if (!species_root || !species_root.IsSequence()) {
            return;
        }
        for (const auto& species_node : species_root) {
            SpeciesConfig species;
            species.name = value_or<std::string>(species_node["name"], "");
            species.long_name = value_or<std::string>(species_node["__long_name"], "");
            species.description = value_or<std::string>(species_node["__description"], "");
            species.molecular_weight_kg_mol = value_or<double>(species_node["molecular weight [kg mol-1]"], 0.0);
            species.density = value_or<double>(species_node["__density"], 0.0);
            species.radius = value_or<double>(species_node["__radius"], 0.0);
            species.lower_radius = value_or<double>(species_node["__lower_radius"], 0.0);
            species.upper_radius = value_or<double>(species_node["__upper_radius"], 0.0);
            species.viscosity = value_or<double>(species_node["__viscosity"], 0.0);
            species.is_gas = value_or<bool>(species_node["__is_gas"], false);
            species.is_aerosol = value_or<bool>(species_node["__is_aerosol"], false);
            species.is_dust = value_or<bool>(species_node["__is_dust"], false);
            species.is_drydep = value_or<bool>(species_node["__is_drydep"], false);
            species.is_wetdep = value_or<bool>(species_node["__is_wetdep"], false);
            species.is_advected = value_or<bool>(species_node["__is_advected"], true);
            species.is_photolysis = value_or<bool>(species_node["__is_photolysis"], false);
            species.mie_name = value_or<std::string>(species_node["__mie_name"], "");
            data.species.push_back(species);
        }
    }

    void ConfigManager::load_emission_mapping_file(const std::string& filename) {
        const YAML::Node emission_root = YAML::LoadFile(filename);
        data.emission_mappings.clear();
        if (!emission_root || !emission_root.IsMap()) {
            return;
        }
        for (const auto& category_node : emission_root) {
            const std::string category_name = category_node.first.as<std::string>();
            EmissionCategoryMapping category;
            for (const auto& field_node : category_node.second) {
                const std::string field_name = field_node.first.as<std::string>();
                const YAML::Node node = field_node.second;
                EmissionFieldMapping field;
                field.long_name = value_or<std::string>(node["long_name"], "");
                field.units = value_or<std::string>(node["units"], "");
                field.scale = double_vector_or_empty(node["scale"]);
                field.map = string_vector_or_empty(node["map"]);
                category.fields[field_name] = field;
            }
            data.emission_mappings[category_name] = category;
        }
    }

} // namespace catchem

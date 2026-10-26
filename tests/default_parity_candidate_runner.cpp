#include "catchem_api.hpp"
#include <yaml-cpp/yaml.h>

#include <filesystem>
#include <cmath>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <map>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

extern "C" {
void catchem_register_seasalt_cpp();
void catchem_register_dust_cpp();
void catchem_register_carbchem_cpp();
void catchem_register_settling_cpp();
void catchem_register_drydep_cpp();
void catchem_register_so4chem_cpp();
void catchem_register_wetdep_cpp();
#ifdef CATCHEM_PARITY_HAS_MUSICA
void catchem_register_photolysis_cpp();
void catchem_register_gaschem_cpp();
#endif
}

void register_candidate_processes() {
    // Static process libraries are not pulled into every linker/toolchain
    // solely for their registration symbols.  Register them explicitly.
    catchem_register_seasalt_cpp();
    catchem_register_dust_cpp();
    catchem_register_carbchem_cpp();
    catchem_register_settling_cpp();
    catchem_register_drydep_cpp();
    catchem_register_so4chem_cpp();
    catchem_register_wetdep_cpp();
#ifdef CATCHEM_PARITY_HAS_MUSICA
    catchem_register_photolysis_cpp();
    catchem_register_gaschem_cpp();
#endif
}

namespace {
    struct SpeciesRepresentation {
        bool is_gas = false;
        bool is_aerosol = false;
        bool is_advected = false;
        uint32_t amount_type = 0;
        uint32_t air_basis = 0;
        double molecular_weight_kg_mol = 0.0;
    };

    constexpr double dry_air_molecular_weight_kg_mol = 0.0289647;

    struct Arguments {
        std::string config;
        std::string profile;
        std::string snapshot;
        std::string initial_snapshot;
        int steps = 1;
        double dt = 10.0;
        bool zero_initial_chemistry = false;
    };

    Arguments parse_arguments(int argc, char** argv) {
        Arguments args;
        for (int i = 1; i < argc; i += 2) {
            if (i + 1 >= argc)
                throw std::runtime_error("expected a value after " + std::string(argv[i]));
            const std::string key = argv[i];
            if (key == "--config")
                args.config = argv[i + 1];
            else if (key == "--met-profile")
                args.profile = argv[i + 1];
            else if (key == "--snapshot")
                args.snapshot = argv[i + 1];
            else if (key == "--initial-snapshot")
                args.initial_snapshot = argv[i + 1];
            else if (key == "--steps")
                args.steps = std::stoi(argv[i + 1]);
            else if (key == "--dt")
                args.dt = std::stod(argv[i + 1]);
            else if (key == "--initial-chemistry")
                args.zero_initial_chemistry = std::string(argv[i + 1]) == "zero";
            else
                throw std::runtime_error("unknown argument: " + key);
        }
        if (args.config.empty() || args.profile.empty() || args.snapshot.empty() || args.steps < 1 || args.dt <= 0.0)
            throw std::runtime_error("--config, --met-profile, --snapshot, and positive --steps are required");
        return args;
    }

    std::vector<double> columnize(const YAML::Node& values, int columns, int vertical) {
        if (!values.IsSequence())
            throw std::runtime_error("profile field has an unexpected vertical extent");
        std::vector<double> result(static_cast<std::size_t>(columns) * vertical);
        // Surface fields are supplied one value per column.  Vertical profiles
        // remain a single shared profile unless a runner is later extended with
        // per-column nested profiles.
        if (vertical == 1 && static_cast<int>(values.size()) == columns) {
            for (int column = 0; column < columns; ++column)
                result[column] = values[column].as<double>();
            return result;
        }
        if (static_cast<int>(values.size()) != vertical)
            throw std::runtime_error("profile field has an unexpected vertical extent");
        for (int column = 0; column < columns; ++column)
            for (int level = 0; level < vertical; ++level)
                // InteropField uses layout_left: column is contiguous, then the
                // vertical axis.  This is the same packing used by the Fortran
                // core and its science bridges.
                result[static_cast<std::size_t>(column) + static_cast<std::size_t>(columns) * level] =
                    values[level].as<double>();
        return result;
    }

    void require_status(int status, const std::string& operation) {
        if (status != CATCHEM_SUCCESS)
            throw std::runtime_error(operation + " failed with status " + std::to_string(status));
    }

    std::size_t native_index(int column, int level, int species_index, int columns, int levels) {
        // CATChem's bound state follows the legacy Fortran layout: column is the
        // contiguous axis, then level, then species.
        return static_cast<std::size_t>(column) +
               static_cast<std::size_t>(columns) *
                   (static_cast<std::size_t>(level) + static_cast<std::size_t>(levels) * species_index);
    }

    /**
     * @brief Convert a registered CCPP host amount into CATChem's native units.
     * @param value Host value in the constituent array.
     * @param name Species short name for error messages.
     * @param meta Runtime representation from the species manifest.
     * @return Gas concentration in dry ppmv or aerosol concentration in dry ug/kg.
     */
    double host_to_native(double value, const std::string& name, const SpeciesRepresentation& meta) {
        if (!std::isfinite(value))
            throw std::runtime_error("non-finite host concentration for " + name);
        if (!meta.is_gas && !meta.is_aerosol && !meta.is_advected)
            return value;
        if (meta.air_basis != 1 || meta.amount_type != 2)
            throw std::runtime_error("native replay supports dry mass-ratio host species only: " + name);
        if (meta.is_gas) {
            if (!(meta.molecular_weight_kg_mol > 0.0))
                throw std::runtime_error("gas species has invalid molecular weight: " + name);
            return value * dry_air_molecular_weight_kg_mol / meta.molecular_weight_kg_mol * 1.0e6;
        }
        if (meta.is_aerosol)
            return value * 1.0e9;
        throw std::runtime_error("native replay cannot convert unclassified advected species " + name);
    }

    /**
     * @brief Convert CATChem-native output units back to the registered host units.
     * @param value Native gas ppmv or aerosol ug/kg value.
     * @param name Species short name for error messages.
     * @param meta Runtime representation from the species manifest.
     * @return Value in the dry mass-ratio CCPP host representation.
     */
    double native_to_host(double value, const std::string& name, const SpeciesRepresentation& meta) {
        if (!std::isfinite(value))
            throw std::runtime_error("non-finite native concentration for " + name);
        if (!meta.is_gas && !meta.is_aerosol && !meta.is_advected)
            return value;
        if (meta.air_basis != 1 || meta.amount_type != 2)
            throw std::runtime_error("native replay supports dry mass-ratio host species only: " + name);
        if (meta.is_gas) {
            if (!(meta.molecular_weight_kg_mol > 0.0))
                throw std::runtime_error("gas species has invalid molecular weight: " + name);
            return value * 1.0e-6 * meta.molecular_weight_kg_mol / dry_air_molecular_weight_kg_mol;
        }
        if (meta.is_aerosol)
            return value * 1.0e-9;
        throw std::runtime_error("native replay cannot convert unclassified advected species " + name);
    }

    /**
     * @brief Write a native snapshot in CCPP host units for direct boundary comparison.
     * @param path Destination JSON file.
     * @param concentration CATChem-native concentration layout.
     * @param columns Horizontal column count.
     * @param levels Vertical level count.
     * @param species_names Runtime mechanism order.
     * @param representations Runtime species conversion metadata.
     * @param steps Number of timesteps completed.
     */
    void write_json(const std::string& path, void* core, const std::vector<double>& concentration, int columns, int levels,
                    const std::vector<std::string>& species_names,
                    const std::unordered_map<std::string, SpeciesRepresentation>& representations, int steps) {
        std::ofstream output(path);
        if (!output)
            throw std::runtime_error("unable to create snapshot " + path);
        output << std::setprecision(17);
        output << "{\n  \"schema_version\": 1,\n  \"snapshots\": [{\n";
        output << "    \"checkpoint\": \"after_timestep_" << steps << "\",\n    \"fields\": {\n";
        output << "      \"concentration\": {\"units\": \"CCPP host representation\", \"shape\": [" << columns << ", " << levels
               << ", " << species_names.size() << "], \"species\": [";
        const int species = static_cast<int>(species_names.size());
        for (int i = 0; i < species; ++i)
            output << (i ? ", " : "") << "\"" << species_names[i] << "\"";
        output << "], \"values\": [";
        bool first = true;
        for (int column = 0; column < columns; ++column)
            for (int level = 0; level < levels; ++level)
                for (int species_index = 0; species_index < species; ++species_index) {
                    const auto& name = species_names[species_index];
                    const auto representation = representations.find(name);
                    if (representation == representations.end())
                        throw std::runtime_error("missing host representation for native species " + name);
                    const double value = native_to_host(
                        concentration[native_index(column, level, species_index, columns, levels)], name,
                        representation->second);
                    output << (first ? "" : ", ")
                           << value;
                    first = false;
                }
        output << "]},\n    \"diagnostics\": {";
        catchem_diag_sync_to_host(core);
        int diagnostic_count = 0;
        require_status(catchem_diag_get_count_checked(core, &diagnostic_count), "query diagnostic count");
        bool first_diagnostic = true;
        for (int index = 0; index < diagnostic_count; ++index) {
            char name_buffer[512] = {};
            require_status(catchem_diag_get_name_at_checked(core, index, name_buffer, sizeof(name_buffer)),
                           "query diagnostic name");
            const std::string name(name_buffer);
            if (name.rfind("photolysis_rate_", 0) != 0)
                continue;
            int rank = 0;
            require_status(catchem_diag_get_rank_checked(core, name.c_str(), &rank), "query diagnostic rank");
            std::vector<int> dimensions(static_cast<std::size_t>(rank));
            require_status(catchem_diag_get_dims_checked(core, name.c_str(), dimensions.data(), rank),
                           "query diagnostic dimensions");
            void* raw = nullptr;
            require_status(catchem_diag_get_pointer_checked(core, name.c_str(), rank, dimensions.data(), &raw),
                           "read photolysis-rate diagnostic");
            if (!raw)
                throw std::runtime_error("photolysis-rate diagnostic has no host data: " + name);
            char units[128] = {};
            require_status(catchem_diag_get_units_checked(core, name.c_str(), units, sizeof(units)),
                           "query diagnostic units");
            std::size_t count = 1;
            for (int extent : dimensions)
                count *= static_cast<std::size_t>(extent);
            const auto* values = static_cast<const double*>(raw);
            output << (first_diagnostic ? "" : ", ") << "\n      \"" << name << "\": {\"units\": \""
                   << units << "\", \"shape\": [";
            for (int dim = 0; dim < rank; ++dim)
                output << (dim ? ", " : "") << dimensions[dim];
            output << "], \"values\": [";
            for (std::size_t value = 0; value < count; ++value)
                output << (value ? ", " : "") << values[value];
            output << "]}";
            first_diagnostic = false;
        }
        output << "\n    }\n    }\n  }]\n}\n";
    }

/**
 * @brief Import an initial chemistry profile into the candidate species order.
 *
 * @details Accepts either the runner's normalized snapshot schema or the
 * species-named chemistry schema emitted by export_ccpp_boundary_chemistry.py.
 * The latter may contain host constituents beyond the native CATChem species;
 * values are copied by species name and converted from CCPP host mass ratios
 * into CATChem-native ppmv (gas) or ug/kg (aerosol) using the runtime manifest.
 *
 * @param initial YAML node containing a normalized snapshot or chemistry profile.
 * @param columns Candidate column count.
 * @param levels Candidate vertical level count.
 * @param native_species Candidate species order from the loaded mechanism.
 * @param representations Runtime species metadata keyed by short name.
 * @param concentration Destination in CATChem's column-level-species layout.
 */
void import_initial_chemistry(const YAML::Node& initial, int columns, int levels,
                              const std::vector<std::string>& native_species,
                              const std::unordered_map<std::string, SpeciesRepresentation>& representations,
                              std::vector<double>& concentration) {
    const int species = static_cast<int>(native_species.size());

    const YAML::Node chemistry = initial["chemistry"];
    if (chemistry && chemistry.IsMap()) {
        const auto input_columns = initial["grid"]["columns"].as<int>();
        const auto input_levels = initial["grid"]["levels"].as<int>();
        const auto input_species_node = chemistry["species"];
        const auto values = chemistry["values"];
        if (input_columns != columns || input_levels != levels || !input_species_node.IsSequence() || !values.IsSequence())
            throw std::runtime_error("initial chemistry profile grid or species list is malformed");
        std::map<std::string, int> source_index;
        for (std::size_t i = 0; i < input_species_node.size(); ++i) {
            const auto name = input_species_node[i].as<std::string>();
            if (!source_index.emplace(name, static_cast<int>(i)).second)
                throw std::runtime_error("initial chemistry profile contains duplicate species " + name);
        }
        if (static_cast<int>(values.size()) != input_levels * static_cast<int>(input_species_node.size()))
            throw std::runtime_error("initial chemistry profile value count does not match its grid and species");
        for (const auto& name : native_species)
            if (source_index.find(name) == source_index.end())
                throw std::runtime_error("initial chemistry profile is missing native species " + name);
        for (int level = 0; level < levels; ++level)
            for (int native = 0; native < species; ++native) {
                const int source = source_index.at(native_species[native]);
                const auto input_index = static_cast<std::size_t>(level) * input_species_node.size() + source;
                const double value = values[input_index].as<double>();
                if (!std::isfinite(value))
                    throw std::runtime_error("initial chemistry profile contains a non-finite value");
                const auto representation = representations.find(native_species[native]);
                if (representation == representations.end())
                    throw std::runtime_error("missing host representation for native species " + native_species[native]);
                const auto& meta = representation->second;
                const double native_value = host_to_native(value, native_species[native], meta);
                concentration[native_index(0, level, native, columns, levels)] = native_value;
            }
        return;
    }

    const YAML::Node field = initial["snapshots"][0]["fields"]["concentration"];
    const auto shape = field["shape"];
    const auto values = field["values"];
    if (shape[0].as<int>() != columns || shape[1].as<int>() != levels || shape[2].as<int>() != species ||
        static_cast<int>(values.size()) != columns * levels * species)
        throw std::runtime_error("initial snapshot concentration extent does not match the candidate grid");
    const bool legacy_native_units = field["units"] && field["units"].as<std::string>() == "model-native";
    for (int col = 0; col < columns; ++col)
        for (int lev = 0; lev < levels; ++lev)
            for (int sp = 0; sp < species; ++sp) {
                const std::size_t canonical = static_cast<std::size_t>(sp) +
                    static_cast<std::size_t>(species) * (static_cast<std::size_t>(lev) +
                    static_cast<std::size_t>(levels) * col);
                const double value = values[canonical].as<double>();
                const auto representation = representations.find(native_species[sp]);
                if (representation == representations.end())
                    throw std::runtime_error("missing host representation for native species " + native_species[sp]);
                concentration[native_index(col, lev, sp, columns, levels)] =
                    legacy_native_units ? value : host_to_native(value, native_species[sp], representation->second);
            }
}
    /**
     * @brief Bind one captured meteorology profile without reconstructing process instances.
     * @param state Checked CATChem state handle.
     * @param profile Exact process-boundary meteorology and vertical axes.
     * @param columns Horizontal extent.
     * @param levels Layer extent.
     * @param owned_fields Storage retained until Core destruction for all bound host buffers.
     */
    void bind_profile_meteorology(void* state, const YAML::Node& profile, int columns, int levels,
                                 std::vector<std::vector<double>>& owned_fields) {
        for (const auto& item : profile["met_2d"]) {
            owned_fields.push_back(columnize(item.second, columns, 1));
            require_status(catchem_state_bind_met_2d_checked(state, item.first.as<std::string>().c_str(),
                                                             owned_fields.back().data(), columns, 1),
                           "bind 2-D met");
        }
        for (const auto& item : profile["met_3d"]) {
            owned_fields.push_back(columnize(item.second, columns, levels));
            require_status(catchem_state_bind_met_3d_checked(state, item.first.as<std::string>().c_str(),
                                                             owned_fields.back().data(), columns, levels, 1),
                           "bind 3-D met");
        }
        for (const auto& item : profile["met_interface"]) {
            owned_fields.push_back(columnize(item.second, columns, levels + 1));
            require_status(catchem_state_bind_met_3d_axis_checked(state, item.first.as<std::string>().c_str(),
                                                                  owned_fields.back().data(), columns, levels + 1, 1,
                                                                  1),
                           "bind interface met");
        }
        for (const auto& item : profile["met_soil"]) {
            const int soil_levels = static_cast<int>(item.second.size());
            owned_fields.push_back(columnize(item.second, columns, soil_levels));
            require_status(catchem_state_bind_met_3d_axis_checked(state, item.first.as<std::string>().c_str(),
                                                                  owned_fields.back().data(), columns, soil_levels, 1,
                                                                  2),
                           "bind soil met");
        }
    }

} // namespace

int main(int argc, char** argv) {
    try {
        const auto args = parse_arguments(argc, argv);
        register_candidate_processes();
        const YAML::Node profile = YAML::LoadFile(args.profile);
        const int columns = profile["grid"]["columns"].as<int>();
        const int levels = profile["grid"]["levels"].as<int>();
        void* core = catchem_core_create_from_config_with_grid(args.config.c_str(), columns, levels);
        if (!core) {
            char detail[512] = {};
            catchem_get_last_error(detail, sizeof(detail));
            throw std::runtime_error(std::string("candidate core construction failed: ") + detail);
        }
        void* state = catchem_core_get_state_manager(core);
        if (!state)
            throw std::runtime_error("candidate state manager unavailable");

        int species_count = 0;
        require_status(catchem_state_get_species_count_checked(state, &species_count), "query species count");
        std::vector<std::string> species_names;
        std::unordered_map<std::string, SpeciesRepresentation> representations;
        species_names.reserve(static_cast<std::size_t>(species_count));
        for (int index = 1; index <= species_count; ++index) {
            char name[128] = {};
            require_status(catchem_state_get_species_name_at_checked(state, index, name, sizeof(name)),
                           "query species name");
            species_names.emplace_back(name);
        }
        void* manifest = nullptr;
        require_status(catchem_manifest_open_checked(args.config.c_str(), &manifest), "open species manifest");
        int manifest_species_count = 0;
        require_status(catchem_manifest_get_species_count_checked(manifest, &manifest_species_count),
                       "query manifest species count");
        for (int index = 0; index < manifest_species_count; ++index) {
            catchem_manifest_species_v1 descriptor{};
            descriptor.schema_version = 1;
            descriptor.struct_size = sizeof(descriptor);
            char name[256] = {}, standard_name[512] = {}, units[128] = {};
            require_status(catchem_manifest_get_species_v1_checked(manifest, index, &descriptor, name, sizeof(name),
                                                                   standard_name, sizeof(standard_name), units,
                                                                   sizeof(units)),
                           "query manifest species metadata");
            representations.emplace(name, SpeciesRepresentation{(descriptor.flags & 1U) != 0,
                                                                  (descriptor.flags & 2U) != 0,
                                                                  (descriptor.flags & 4U) != 0,
                                                                  descriptor.amount_type, descriptor.air_basis,
                                                                  descriptor.molecular_weight_kg_mol});
        }
        require_status(catchem_manifest_destroy_checked(manifest), "close species manifest");
        if (manifest_species_count != species_count)
            throw std::runtime_error("runtime state and species manifest counts do not match");

        std::vector<std::vector<double>> owned_fields;
        bind_profile_meteorology(state, profile, columns, levels, owned_fields);

        const int species = species_count;
        std::vector<double> concentration(static_cast<std::size_t>(columns) * levels * species, 0.0);
        if (!args.initial_snapshot.empty()) {
            const YAML::Node initial = YAML::LoadFile(args.initial_snapshot);
            import_initial_chemistry(initial, columns, levels, species_names, representations, concentration);
        } else if (!args.zero_initial_chemistry) {
            // Deterministic, positive tracer state makes the parity comparison
            // sensitive to process tendencies instead of passing trivially on
            // an all-zero chemistry field.
            for (int col = 0; col < columns; ++col)
                for (int lev = 0; lev < levels; ++lev)
                    for (int sp = 0; sp < species; ++sp)
                        concentration[native_index(col, lev, sp, columns, levels)] =
                            1.0e-12 * (1.0 + 0.01 * lev + 0.001 * sp);
        }
        require_status(
            catchem_state_bind_unified_chemistry_checked(state, concentration.data(), columns, levels, species),
            "bind concentration");
        const YAML::Node sequence = profile["replay_steps"];
        if (sequence && (!sequence.IsSequence() || static_cast<int>(sequence.size()) != args.steps))
            throw std::runtime_error("replay_steps must contain one meteorology profile per requested step");
        for (int step = 0; step < args.steps; ++step) {
            const YAML::Node active = sequence ? sequence[step] : profile;
            if (sequence && step > 0) {
                if (active["grid"]["columns"].as<int>() != columns || active["grid"]["levels"].as<int>() != levels)
                    throw std::runtime_error("replay grid changed within the native process lifetime");
                bind_profile_meteorology(state, active, columns, levels, owned_fields);
            }
            const YAML::Node clock = active["clock"];
            const int year = clock ? clock["year"].as<int>() : 2026;
            const int month = clock ? clock["month"].as<int>() : 1;
            const int day = clock ? clock["day"].as<int>() : 1;
            const int hour = clock ? clock["hour"].as<int>() : 0;
            const int minute = clock ? clock["minute"].as<int>() : 0;
            const int second = clock ? clock["second"].as<int>() : 0;
            const int day_of_year = clock ? clock["day_of_year"].as<int>() : 1;
            require_status(catchem_state_set_time_checked(state, year, month, day, hour, minute, second,
                                                           day_of_year, args.dt), "set parity clock");
            require_status(catchem_core_run_timestep(core, args.dt), "run timestep");
            if (sequence)
                write_json(args.snapshot + ".step-" + std::to_string(step + 1) + ".json", core, concentration,
                           columns, levels, species_names, representations, step + 1);
        }
        write_json(args.snapshot, core, concentration, columns, levels, species_names, representations, args.steps);
        catchem_core_destroy(core);
        return 0;
    } catch (const std::exception& error) {
        std::cerr << "candidate parity runner: " << error.what() << '\n';
        return 1;
    }
}

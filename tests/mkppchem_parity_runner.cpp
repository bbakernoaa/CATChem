// Snapshot runner for the mkppchem process-vs-process parity test
// (tests/run_mkppchem_parity.py).  It writes the same normalized schema as
// default_parity_candidate_runner -- schema_version 1 JSON consumed by
// compare_parity_snapshots.py -- with three deliberate differences:
//
//   1. The species count and names are read from the bound state instead of
//      being hardcoded to the 22-species Default fixture, because the two
//      parity sides use different species inventories (the 26-species MKPP
//      mechanism file vs the 22-species Default file).  The comparator's
//      metadata check is satisfied downstream by projecting both snapshots
//      onto the shared carbon subset.
//   2. It drives the C++ core directly (load config -> bind -> derive ->
//      init -> run), the same manual sequence as the mkppchem column test,
//      instead of the config constructor.  That constructor eagerly
//      initializes every scheduled process before a standalone runner can
//      bind host MET pointers, which suits processes that only check fields
//      at run() but is fatal for mkppchem, whose cloud-water source
//      resolution is an init-time contract.
//   3. The initial chemistry is the uniform 4.0e-12 mol/mol seed proven
//      well-posed for the generated gocart solver (the nitrate equilibrium
//      block is only consistent near the Kc product), applied identically on
//      both sides so the carbon columns start from the same value.
//
// Both sides run the SAME executable: the staged config's run_phases decide
// whether carbchem+so4chem or mkppchem runs.  Every process is registered.
#include "catchem_config_manager.hpp"
#include "catchem_core.hpp"
#include "catchem_process_registry.hpp"
#include "catchem_state_manager.hpp"
#include <Kokkos_Core.hpp>

#include <type_traits>
#include <yaml-cpp/yaml.h>

#include <cctype>
#include <algorithm>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <memory>
#include <stdexcept>
#include <string>
#include <vector>

extern "C" {
void catchem_register_carbchem_cpp();
void catchem_register_so4chem_cpp();
void catchem_register_mkppchem_cpp();
}

namespace {

void register_parity_processes() {
    // Static process libraries are not pulled into every linker/toolchain
    // solely for their registration symbols.  Register them explicitly.
    catchem_register_carbchem_cpp();
    catchem_register_so4chem_cpp();
    catchem_register_mkppchem_cpp();
}

struct Arguments {
    std::string config;
    std::string profile;
    std::string snapshot;
    int steps = 1;
    double dt = 10.0;
    /// Per-species initial-concentration overrides, "name=value" pairs.
    /// Applied on top of the uniform seed, identically on both parity sides,
    /// so a comparison can start from elevated tracers where a pathway's
    /// tendency is measurable within the run's few steps.
    std::vector<std::pair<std::string, double>> seeds;
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
        else if (key == "--steps")
            args.steps = std::stoi(argv[i + 1]);
        else if (key == "--dt")
            args.dt = std::stod(argv[i + 1]);
        else if (key == "--seed") {
            // Comma-separated "name=value" pairs, e.g. --seed so2=1e-6,h2o2=1e-7.
            std::string list = argv[i + 1];
            std::size_t start = 0;
            while (start < list.size()) {
                const std::size_t comma = list.find(',', start);
                const std::string item = list.substr(start, comma == std::string::npos ? std::string::npos
                                                                                       : comma - start);
                const std::size_t eq = item.find('=');
                if (eq == std::string::npos || eq + 1 >= item.size())
                    throw std::runtime_error("--seed expects name=value pairs, got '" + item + "'");
                args.seeds.emplace_back(item.substr(0, eq), std::stod(item.substr(eq + 1)));
                if (comma == std::string::npos)
                    break;
                start = comma + 1;
            }
        } else
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

std::size_t native_index(int column, int level, int species_index, int columns, int levels) {
    // CATChem's bound state follows the legacy Fortran layout: column is the
    // contiguous axis, then level, then species.
    return static_cast<std::size_t>(column) + static_cast<std::size_t>(columns) *
                                                  (static_cast<std::size_t>(level) + static_cast<std::size_t>(levels) * species_index);
}

// The execution-space identity the snapshot records so the parity harness can
// tell which backend produced a result (cross-backend parity) and
// whether the zero-copy condition holds structurally.
// `space_is_host` is true exactly when HostSpace and the default execution
// space's memory space are the same type -- the OpenMP/Serial case, where no
// device memory exists and every host<->device transfer is a no-op.
struct BackendInfo {
    std::string name;
    bool space_is_host = true;
};

BackendInfo current_backend() {
    BackendInfo info;
    info.name = Kokkos::DefaultExecutionSpace::name();
    info.space_is_host =
        std::is_same_v<Kokkos::DefaultExecutionSpace::memory_space, Kokkos::HostSpace>;
    return info;
}

void write_json(const std::string& path, const std::vector<double>& concentration, int columns, int levels,
                const std::vector<std::string>& species_names, int steps, const BackendInfo& backend) {
    std::ofstream output(path);
    if (!output)
        throw std::runtime_error("unable to create snapshot " + path);
    const int species = static_cast<int>(species_names.size());
    output << std::setprecision(17);
    output << "{\n  \"schema_version\": 1,\n";
    // Backend provenance lives at the top level, beside schema_version: the
    // comparator reads only `snapshots`, so it ignores this, and the parity
    // harness can pull it without descending into the field tree.
    output << "  \"backend\": {\"name\": \"" << backend.name << "\", \"space_is_host\": "
           << (backend.space_is_host ? "true" : "false") << "},\n";
    output << "  \"snapshots\": [{\n";
    output << "    \"checkpoint\": \"after_timestep_" << steps << "\",\n    \"fields\": {\n";
    output << "      \"concentration\": {\"units\": \"model-native\", \"shape\": [" << columns << ", " << levels
           << ", " << species << "], \"species\": [";
    for (int i = 0; i < species; ++i)
        output << (i ? ", " : "") << "\"" << species_names[i] << "\"";
    output << "], \"values\": [";
    bool first = true;
    for (int column = 0; column < columns; ++column)
        for (int level = 0; level < levels; ++level)
            for (int species_index = 0; species_index < species; ++species_index) {
                output << (first ? "" : ", ")
                       << concentration[native_index(column, level, species_index, columns, levels)];
                first = false;
            }
    output << "]}\n    }\n  }]\n}\n";
}

// Resolve a (possibly relative) path against the config file's directory,
// mirroring Core's internal load_and_validate_configuration so staged
// "./CATChem_species.yml" references work from any CWD.
std::string resolve_against_config(const std::string& config_file, const std::string& path) {
    if (path.empty() || path.front() == '/')
        return path;
    const auto slash = config_file.find_last_of('/');
    if (slash == std::string::npos)
        return path;
    return config_file.substr(0, slash + 1) + path;
}

} // namespace

int main(int argc, char** argv) {
    // Own the Kokkos lifetime explicitly (the column test's pattern): the
    // core's runtime lease then attaches to the already-initialized backend
    // instead of initializing/finalizing behind the runner's back.
    Kokkos::initialize(argc, argv);
    int exit_code = 0;
    try {
        const auto args = parse_arguments(argc, argv);
        register_parity_processes();
        const YAML::Node profile = YAML::LoadFile(args.profile);
        const int columns = profile["grid"]["columns"].as<int>();
        const int levels = profile["grid"]["levels"].as<int>();

        // Load the staged configuration up front: the species inventory is
        // config-driven (22 tracers on the Default side, 26 on the mechanism
        // side), so the Core grid must be sized from it.
        auto config = std::make_shared<catchem::ConfigManager>();
        config->load_from_file(args.config);
        config->load_species_file(resolve_against_config(args.config, config->data.species_filename));
        if (!config->data.simulation.emission_filename.empty())
            config->load_emission_mapping_file(
                resolve_against_config(args.config, config->data.simulation.emission_filename));
        config->validate_or_throw();
        const int species = static_cast<int>(config->data.species.size());
        if (species <= 0)
            throw std::runtime_error("staged configuration carries no species inventory");

        auto core = std::make_shared<catchem::Core>(columns, levels, species);
        auto state = core->get_state_manager();
        state->set_validation_policy(config->data.physical_validation_policy);
        state->attach_config_manager(config);
        state->set_configuration_path(args.config);
        state->chemistry().load_from_config_manager(*config);

        std::vector<std::vector<double>> owned_fields;
        for (const auto& item : profile["met_3d"]) {
            owned_fields.push_back(columnize(item.second, columns, levels));
            state->bind_met_field_3d(item.first.as<std::string>(), owned_fields.back().data());
        }
        for (const auto& item : profile["met_interface"]) {
            owned_fields.push_back(columnize(item.second, columns, levels + 1));
            state->bind_met_field_3d_contract(item.first.as<std::string>(), owned_fields.back().data(), levels + 1,
                                              catchem::SemanticAxis::Interface);
        }
        for (const auto& item : profile["met_2d"]) {
            owned_fields.push_back(columnize(item.second, columns, 1));
            state->bind_met_field_2d(item.first.as<std::string>(), owned_fields.back().data());
        }
        for (const auto& item : profile["met_soil"]) {
            const int soil_levels = static_cast<int>(item.second.size());
            owned_fields.push_back(columnize(item.second, columns, soil_levels));
            state->bind_met_field_3d_contract(item.first.as<std::string>(), owned_fields.back().data(), soil_levels,
                                              catchem::SemanticAxis::SoilLayer);
        }

        // The execution plan validates required fields before run(), and the
        // mkppchem contract requires AIRDEN_DRY, so derive it from the bound
        // T/PMID/QV exactly as the NUOPC driver does per import step.
        state->derive_airden_dry();

        std::vector<std::string> species_names;
        species_names.reserve(species);
        for (const auto& meta : state->chemistry().species_list)
            species_names.push_back(meta.short_name);

        // Uniform 4.0e-12 mol/mol on every species: the seed the generated
        // gocart solver is well-posed around, applied identically on both
        // parity sides so the compared carbon columns start equal.
        std::vector<double> concentration(static_cast<std::size_t>(columns) * levels * species, 0.0);
        for (int col = 0; col < columns; ++col)
            for (int lev = 0; lev < levels; ++lev)
                for (int sp = 0; sp < species; ++sp)
                    concentration[native_index(col, lev, sp, columns, levels)] = 4.0e-12;
        // Per-species overrides (same values on both parity sides): a named
        // tracer is set to the requested mol/mol everywhere, so a pathway's
        // tendency -- e.g. cloud-gated aqueous SO2 oxidation -- is measurable
        // over the run's few steps instead of being swamped by the background.
        for (const auto& [name, value] : args.seeds) {
            const auto it = std::find_if(species_names.begin(), species_names.end(),
                                         [&name](const std::string& s) {
                                             return s.size() == name.size() &&
                                                    std::equal(s.begin(), s.end(), name.begin(),
                                                               [](char a, char b) {
                                                                   return std::tolower(a) == std::tolower(b);
                                                               });
                                         });
            if (it == species_names.end())
                throw std::runtime_error("--seed names unknown species '" + name + "'");
            const int sp = static_cast<int>(std::distance(species_names.begin(), it));
            for (int col = 0; col < columns; ++col)
                for (int lev = 0; lev < levels; ++lev)
                    concentration[native_index(col, lev, sp, columns, levels)] = value;
        }
        state->bind_unified_chemistry(concentration.data());
        // Both process bridges consume the state clock's timestep; the same
        // value drives carbchem's exp(-dt/tau) and mkppchem's ROS-3 interval.
        state->clock().year = 2026;
        state->clock().month = 1;
        state->clock().day = 1;
        state->clock().hour = 0;
        state->clock().minute = 0;
        state->clock().second = 0;
        state->clock().doy = 1;
        state->clock().timestep = args.dt;

        // Initialize and schedule exactly the processes the staged phase
        // activates -- carbchem+so4chem on the baseline side, mkppchem on the
        // candidate side -- mirroring Core::add_configured_processes but after
        // the MET bindings above (see the header note on init-time ordering).
        auto& registry = catchem::ProcessRegistry::get_instance();
        for (const auto& process_name : config->data.active_processes) {
            const auto settings = config->data.processes.find(process_name);
            if (settings == config->data.processes.end() || !settings->second.activate)
                continue;
            registry.validate_settings(process_name, settings->second);
            auto process = registry.create(process_name);
            process->init(state);
            core->add_process(process);
        }
        if (core->get_num_processes() == 0)
            throw std::runtime_error("staged configuration activates no processes");

        for (int step = 0; step < args.steps; ++step)
            core->run_timestep(args.dt);
        // The candidate side's process writes tendencies on device; read back
        // through the state's host views so the snapshot reflects the solved
        // values (a no-op under HostSpace builds).
        state->sync_to_host();
        write_json(args.snapshot, concentration, columns, levels, species_names, args.steps, current_backend());
    } catch (const std::exception& error) {
        std::cerr << "mkppchem parity runner: " << error.what() << '\n';
        exit_code = 1;
    }
    Kokkos::finalize();
    return exit_code;
}

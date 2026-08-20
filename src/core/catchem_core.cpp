#include "catchem_core.hpp"
#include "catchem_process_registry.hpp"
#include <iostream>

namespace {

    // The core owns the Kokkos runtime for hosts that drive it through the
    // C API (e.g. the NUOPC cap): managed Views (diagnostics, device
    // mirrors) require an initialized runtime. No-op when Kokkos is off or
    // when the host already initialized it. Finalization is intentionally
    // left to the host/test harness.
    void ensure_kokkos_initialized() {
#ifdef CATCHEM_ENABLE_KOKKOS
        if (!Kokkos::is_initialized()) {
            Kokkos::initialize();
        }
#endif
    }

    // Resolve a (possibly relative) species path against the config file's
    // directory, so YAML-relative paths work regardless of the host's CWD.
    std::string resolve_against_config(const std::string& config_file, const std::string& path) {
        if (path.empty() || path.front() == '/')
            return path;
        auto slash = config_file.find_last_of('/');
        if (slash == std::string::npos)
            return path;
        return config_file.substr(0, slash + 1) + path;
    }

    // Load the species list declared by the config (if any) and size the
    // state's species dimension from it.
    void load_configured_species(catchem::StateManager& state, const std::string& config_file,
                                 const std::string& species_filename) {
        if (species_filename.empty())
            return;
        state.chem.load_species_config(resolve_against_config(config_file, species_filename));
        if (!state.chem.species_list.empty()) {
            state.n_species = static_cast<int>(state.chem.species_list.size());
        }
    }

} // namespace

namespace catchem {

    Core::Core(int nc, int nl, int ns) {
        ensure_kokkos_initialized();
        config_mgr = std::make_shared<ConfigManager>();
        config_mgr->data.runtime.nx = nc;
        config_mgr->data.runtime.ny = 1;
        config_mgr->data.runtime.nz = nl;

        grid_mgr = std::make_shared<GridManager>(nc, 1, nl);
        state_mgr = std::make_shared<StateManager>(nc, nl, ns);
        state_mgr->config_mgr = config_mgr;
        diag_mgr = std::make_shared<DiagnosticManager>();
        state_mgr->diag_mgr = diag_mgr;
    }

    Core::Core(const std::string& config_file) {
        ensure_kokkos_initialized();
        config_mgr = std::make_shared<ConfigManager>();
        config_mgr->load_from_file(config_file);

        int nx = config_mgr->data.runtime.nx;
        int ny = config_mgr->data.runtime.ny;
        int nz = config_mgr->data.runtime.nz;

        grid_mgr = std::make_shared<GridManager>(nx, ny, nz);
        state_mgr = std::make_shared<StateManager>(nx * ny, nz, 50);
        state_mgr->config_mgr = config_mgr;
        state_mgr->config_file_path = config_mgr->config_file_path;
        diag_mgr = std::make_shared<DiagnosticManager>();
        state_mgr->diag_mgr = diag_mgr;
        load_configured_species(*state_mgr, config_file, config_mgr->data.species_filename);
        if (!config_mgr->data.simulation.emission_filename.empty()) {
            config_mgr->load_emission_mapping_file(resolve_against_config(config_file, config_mgr->data.simulation.emission_filename));
        }
        add_configured_processes();
    }

    Core::Core(const std::string& config_file, int nc, int nl) {
        ensure_kokkos_initialized();
        config_mgr = std::make_shared<ConfigManager>();
        config_mgr->load_from_file(config_file);

        // The host (e.g. UFS per-rank domain decomposition) dictates the grid
        // dimensions; the YAML grid section applies to standalone runs only.
        config_mgr->data.runtime.nx = nc;
        config_mgr->data.runtime.ny = 1;
        config_mgr->data.runtime.nz = nl;

        grid_mgr = std::make_shared<GridManager>(nc, 1, nl);
        state_mgr = std::make_shared<StateManager>(nc, nl, 50); // 50 = fallback when no species file
        state_mgr->config_mgr = config_mgr;
        state_mgr->config_file_path = config_mgr->config_file_path;
        diag_mgr = std::make_shared<DiagnosticManager>();
        state_mgr->diag_mgr = diag_mgr;
        load_configured_species(*state_mgr, config_file, config_mgr->data.species_filename);
        if (!config_mgr->data.simulation.emission_filename.empty()) {
            config_mgr->load_emission_mapping_file(resolve_against_config(config_file, config_mgr->data.simulation.emission_filename));
        }
        add_configured_processes();
    }

    std::shared_ptr<ConfigManager> Core::get_config_manager() {
        return config_mgr;
    }

    std::shared_ptr<GridManager> Core::get_grid_manager() {
        return grid_mgr;
    }

    std::shared_ptr<StateManager> Core::get_state_manager() {
        return state_mgr;
    }

    std::shared_ptr<DiagnosticManager> Core::get_diagnostic_manager() {
        return diag_mgr;
    }

    std::size_t Core::get_num_processes() const {
        return processes.size();
    }

    void Core::add_configured_processes() {
        auto& registry = ProcessRegistry::get_instance();
        for (const auto& process_name : config_mgr->data.active_processes) {
            auto process = registry.create(process_name);
            process->init(state_mgr);
            add_process(process);
        }
    }

    void Core::add_process(std::shared_ptr<ProcessInterface> process) {
        processes.push_back(process);
    }

    void Core::run_timestep(double dt) {
        if (dt <= 0.0 || dt > 86400.0) {
            throw std::out_of_range(
                "Timestep dt must be positive and within a plausible physical daily limit (0 < dt <= 86400).");
        }

        // Sync shared boundary arrays to active execution spaces
        state_mgr->sync_to_device();

        for (auto& process : processes) {
            process->run(state_mgr);
        }

        // Sync execution outputs back to Fortran-accessible memory
        state_mgr->sync_to_host();

        // Sync diagnostics
        diag_mgr->sync_to_host();
    }

    void Core::run_timestep() {
        double dt = config_mgr->data.runtime.dt;
        run_timestep(dt);
    }

} // namespace catchem

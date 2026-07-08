#include "catchem_core.hpp"

namespace catchem {

Core::Core(int nc, int nl, int ns) {
    state_mgr = std::make_shared<StateManager>(nc, nl, ns);
    diag_mgr = std::make_shared<DiagnosticManager>();
}

std::shared_ptr<StateManager> Core::get_state_manager() {
    return state_mgr;
}

std::shared_ptr<DiagnosticManager> Core::get_diagnostic_manager() {
    return diag_mgr;
}

void Core::add_process(std::shared_ptr<ProcessInterface> process) {
    processes.push_back(process);
}

void Core::run_timestep(double dt) {
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

} // namespace catchem

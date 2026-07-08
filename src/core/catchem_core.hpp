#pragma once
#include <memory>
#include <vector>
#include <string>
#include "catchem_state_manager.hpp"
#include "catchem_process_interface.hpp"
#include "catchem_diagnostic_manager.hpp"
#include "catchem_config_manager.hpp"
#include "catchem_grid_manager.hpp"

namespace catchem {

class Core {
private:
    std::shared_ptr<ConfigManager> config_mgr;
    std::shared_ptr<GridManager> grid_mgr;
    std::shared_ptr<StateManager> state_mgr;
    std::shared_ptr<DiagnosticManager> diag_mgr;
    std::vector<std::shared_ptr<ProcessInterface>> processes;
public:
    Core(int nc, int nl, int ns);
    Core(const std::string& config_file);
    std::shared_ptr<ConfigManager> get_config_manager();
    std::shared_ptr<GridManager> get_grid_manager();
    std::shared_ptr<StateManager> get_state_manager();
    std::shared_ptr<DiagnosticManager> get_diagnostic_manager();
    void add_process(std::shared_ptr<ProcessInterface> process);
    void run_timestep(double dt);
    void run_timestep();
};

} // namespace catchem

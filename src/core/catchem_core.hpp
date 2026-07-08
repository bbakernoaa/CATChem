#pragma once
#include <memory>
#include <vector>
#include "catchem_state_manager.hpp"
#include "catchem_process_interface.hpp"

namespace catchem {

class Core {
private:
    std::shared_ptr<StateManager> state_mgr;
    std::vector<std::shared_ptr<ProcessInterface>> processes;
public:
    Core(int nc, int nl, int ns);
    std::shared_ptr<StateManager> get_state_manager();
    void add_process(std::shared_ptr<ProcessInterface> process);
    void run_timestep(double dt);
};

} // namespace catchem

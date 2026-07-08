#include "catchem_process_so4chem.hpp"

namespace catchem {

SO4chemProcess::SO4chemProcess() : active_scheme("legacy_fortran"), fortran_callback(nullptr) {}

void SO4chemProcess::init(std::shared_ptr<StateManager> state) {}

void SO4chemProcess::set_fortran_bridge_callback(std::function<void(void*)> cb) {
    fortran_callback = cb;
}

void SO4chemProcess::run(std::shared_ptr<StateManager> state) {
    if (fortran_callback) {
        state->sync_to_host();
        fortran_callback(static_cast<void*>(state.get()));
        state->sync_to_device();
    }
}

} // namespace catchem

#include "catchem_process_wetdep.hpp"

namespace catchem {

WetDepProcess::WetDepProcess() : active_scheme("legacy_fortran"), fortran_callback(nullptr) {}

void WetDepProcess::init(std::shared_ptr<StateManager> state) {}

void WetDepProcess::set_fortran_bridge_callback(std::function<void(void*)> cb) {
    fortran_callback = cb;
}

void WetDepProcess::run(std::shared_ptr<StateManager> state) {
    if (fortran_callback) {
        state->sync_to_host();
        fortran_callback(static_cast<void*>(state.get()));
        state->sync_to_device();
    }
}

} // namespace catchem

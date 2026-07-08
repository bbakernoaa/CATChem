#include "catchem_process_drydep.hpp"

namespace catchem {

DryDepProcess::DryDepProcess() : active_scheme("legacy_fortran"), fortran_callback(nullptr) {}

void DryDepProcess::init(std::shared_ptr<StateManager> state) {}

void DryDepProcess::set_fortran_bridge_callback(std::function<void(void*)> cb) {
    fortran_callback = cb;
}

void DryDepProcess::run(std::shared_ptr<StateManager> state) {
    if (fortran_callback) {
        state->sync_to_host();
        fortran_callback(static_cast<void*>(state.get()));
        state->sync_to_device();
    }
}

} // namespace catchem

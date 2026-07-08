#include "catchem_process_settling.hpp"

namespace catchem {

SettlingProcess::SettlingProcess() : active_scheme("legacy_fortran"), fortran_callback(nullptr) {}

void SettlingProcess::init(std::shared_ptr<StateManager> state) {
    // Register self in global registry on initialization
}

void SettlingProcess::set_fortran_bridge_callback(std::function<void(void*)> cb) {
    fortran_callback = cb;
}

void SettlingProcess::run(std::shared_ptr<StateManager> state) {
    if (fortran_callback) {
        state->sync_to_host();
        fortran_callback(static_cast<void*>(state.get()));
        state->sync_to_device();
    }
}

} // namespace catchem

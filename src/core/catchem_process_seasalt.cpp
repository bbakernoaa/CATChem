#include "catchem_process_seasalt.hpp"

namespace catchem {

SeaSaltProcess::SeaSaltProcess() : active_scheme("legacy_fortran"), fortran_callback(nullptr) {}

void SeaSaltProcess::init(std::shared_ptr<StateManager> state) {}

void SeaSaltProcess::set_fortran_bridge_callback(std::function<void(void*)> cb) {
    fortran_callback = cb;
}

void SeaSaltProcess::run(std::shared_ptr<StateManager> state) {
    if (fortran_callback) {
        state->sync_to_host();
        fortran_callback(static_cast<void*>(state.get()));
        state->sync_to_device();
    }
}

} // namespace catchem

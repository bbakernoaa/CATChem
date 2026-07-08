#include "catchem_api.hpp"
#include "catchem_core.hpp"
#include "catchem_state_manager.hpp"
#include "catchem_diagnostic_manager.hpp"

extern "C" {

void* catchem_core_create(int nc, int nl, int ns) {
    return static_cast<void*>(new catchem::Core(nc, nl, ns));
}

void catchem_core_destroy(void* core_ptr) {
    delete static_cast<catchem::Core*>(core_ptr);
}

void* catchem_core_get_state_manager(void* core_ptr) {
    auto* core = static_cast<catchem::Core*>(core_ptr);
    return static_cast<void*>(core->get_state_manager().get());
}

void catchem_state_bind_1d(void* state_ptr, const char* name, double* ptr) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    state->bind_field_1d(name, ptr);
}

void catchem_state_bind_2d(void* state_ptr, const char* name, double* ptr) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    state->bind_field_2d(name, ptr);
}

void catchem_state_bind_3d(void* state_ptr, const char* name, double* ptr) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    state->bind_field_3d(name, ptr);
}

void catchem_state_sync_to_device(void* state_ptr) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    state->sync_to_device();
}

void catchem_state_sync_to_host(void* state_ptr) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    state->sync_to_host();
}

double* catchem_state_get_pointer_1d(void* state_ptr, const char* name) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    return state->get_host_pointer_1d(name);
}

double* catchem_state_get_pointer_2d(void* state_ptr, const char* name) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    return state->get_host_pointer_2d(name);
}

double* catchem_state_get_pointer_3d(void* state_ptr, const char* name) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    return state->get_host_pointer_3d(name);
}

void catchem_core_run_timestep(void* core_ptr, double dt) {
    auto* core = static_cast<catchem::Core*>(core_ptr);
    core->run_timestep(dt);
}

void catchem_diag_register(void* core_ptr, const char* name, const char* desc, const char* units, int rank, int dim1, int dim2, int dim3) {
    auto* core = static_cast<catchem::Core*>(core_ptr);
    catchem::DiagType type;
    std::vector<int> dims;
    if (rank == 2) {
        type = catchem::DiagType::FIELD_2D;
        dims = {dim1, dim2};
    } else if (rank == 3) {
        type = catchem::DiagType::FIELD_3D;
        dims = {dim1, dim2, dim3};
    } else {
        type = catchem::DiagType::SCALAR; // Simplified for now
    }
    core->get_diagnostic_manager()->register_field(name, desc, units, type, dims);
}

void* catchem_diag_get_pointer(void* core_ptr, const char* name) {
    auto* core = static_cast<catchem::Core*>(core_ptr);
    return core->get_diagnostic_manager()->get_host_pointer(name);
}

void catchem_diag_sync_to_host(void* core_ptr) {
    auto* core = static_cast<catchem::Core*>(core_ptr);
    core->get_diagnostic_manager()->sync_to_host();
}

void catchem_diag_reset(void* core_ptr) {
    auto* core = static_cast<catchem::Core*>(core_ptr);
    core->get_diagnostic_manager()->reset_all();
}

}

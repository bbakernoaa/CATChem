#include "catchem_api.hpp"
#include "catchem_core.hpp"
#include "catchem_state_manager.hpp"
#include "catchem_diagnostic_manager.hpp"
#include "catchem_process_registry.hpp"

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

void catchem_state_bind_met_2d(void* state_ptr, const char* name, double* ptr) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    state->bind_met_field_2d(name, ptr);
}

void catchem_state_bind_met_3d(void* state_ptr, const char* name, double* ptr) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    state->bind_met_field_3d(name, ptr);
}

void catchem_state_bind_unified_chemistry(void* state_ptr, double* ptr) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    state->bind_unified_chemistry(ptr);
}

void catchem_state_set_time(void* state_ptr, int yr, int mo, int dy, int hr, int mn, int sc, int doy, double tstep) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    state->time.year = yr;
    state->time.month = mo;
    state->time.day = dy;
    state->time.hour = hr;
    state->time.minute = mn;
    state->time.second = sc;
    state->time.doy = doy;
    state->time.timestep = tstep;
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

void catchem_core_add_process_by_name(void* core_ptr, const char* name) {
    auto* core = static_cast<catchem::Core*>(core_ptr);
    core->add_process(catchem::ProcessRegistry::get_instance().create(name));
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

void catchem_state_load_species_config(void* state_ptr, const char* filename) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    state->load_species_config(filename);
}

int catchem_state_get_species_count(void* state_ptr) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    return static_cast<int>(state->chem.species_list.size());
}

int catchem_state_get_species_index(void* state_ptr, const char* name) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    auto it = state->chem.species_name_to_index.find(name);
    if (it != state->chem.species_name_to_index.end()) {
        return it->second + 1; // Translate 0-based C++ index to 1-based Fortran index
    }
    return -1;
}

int catchem_state_get_gas_species_count(void* state_ptr) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    return static_cast<int>(state->chem.gas_indices.size());
}

void catchem_state_get_gas_indices(void* state_ptr, int* indices_out) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    for (size_t i = 0; i < state->chem.gas_indices.size(); ++i) {
        indices_out[i] = state->chem.gas_indices[i] + 1; // 1-based
    }
}

int catchem_state_get_aerosol_species_count(void* state_ptr) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    return static_cast<int>(state->chem.aerosol_indices.size());
}

void catchem_state_get_aerosol_indices(void* state_ptr, int* indices_out) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    for (size_t i = 0; i < state->chem.aerosol_indices.size(); ++i) {
        indices_out[i] = state->chem.aerosol_indices[i] + 1; // 1-based
    }
}

double catchem_state_get_species_mw(void* state_ptr, int index) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    int idx_0 = index - 1; // 1-based to 0-based
    if (idx_0 >= 0 && idx_0 < static_cast<int>(state->chem.species_list.size())) {
        return state->chem.species_list[idx_0].mw_g;
    }
    return 0.0;
}

int catchem_state_is_species_gas(void* state_ptr, int index) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    int idx_0 = index - 1;
    if (idx_0 >= 0 && idx_0 < static_cast<int>(state->chem.species_list.size())) {
        return state->chem.species_list[idx_0].is_gas ? 1 : 0;
    }
    return 0;
}

int catchem_state_is_species_aerosol(void* state_ptr, int index) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    int idx_0 = index - 1;
    if (idx_0 >= 0 && idx_0 < static_cast<int>(state->chem.species_list.size())) {
        return state->chem.species_list[idx_0].is_aerosol ? 1 : 0;
    }
    return 0;
}

void catchem_state_derive_bxheight(void* state_ptr) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    state->derive_bxheight();
}

void catchem_state_derive_airden_dry(void* state_ptr) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    state->derive_airden_dry();
}

}

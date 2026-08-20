#include "catchem_api.hpp"
#include "catchem_core.hpp"
#include "catchem_diagnostic_manager.hpp"
#include "catchem_process_registry.hpp"
#include "catchem_state_manager.hpp"
#include "catchem_unit_conversion.hpp"
#include <cstring>
#include <fstream>
#include <iostream>
#include <sstream>

#if defined(__GNUC__) || defined(__clang__)
#define CATCHEM_WEAK_SYMBOL __attribute__((weak))
#else
#define CATCHEM_WEAK_SYMBOL
#endif

extern "C" {
void catchem_register_carbchem_cpp() CATCHEM_WEAK_SYMBOL;
void catchem_register_drydep_cpp() CATCHEM_WEAK_SYMBOL;
void catchem_register_dust_cpp() CATCHEM_WEAK_SYMBOL;
void catchem_register_seasalt_cpp() CATCHEM_WEAK_SYMBOL;
void catchem_register_settling_cpp() CATCHEM_WEAK_SYMBOL;
void catchem_register_so4chem_cpp() CATCHEM_WEAK_SYMBOL;
void catchem_register_wetdep_cpp() CATCHEM_WEAK_SYMBOL;
}

namespace {

    static void copy_string_to_buffer(const std::string& src, char* buffer, int max_len) {
        if (buffer == nullptr || max_len <= 0)
            return;
        std::strncpy(buffer, src.c_str(), max_len - 1);
        buffer[max_len - 1] = '\0';
    }

    void register_builtin_processes() {
        static bool registered = false;
        if (registered) {
            return;
        }
        if (catchem_register_carbchem_cpp)
            catchem_register_carbchem_cpp();
        if (catchem_register_drydep_cpp)
            catchem_register_drydep_cpp();
        if (catchem_register_dust_cpp)
            catchem_register_dust_cpp();
        if (catchem_register_seasalt_cpp)
            catchem_register_seasalt_cpp();
        if (catchem_register_settling_cpp)
            catchem_register_settling_cpp();
        if (catchem_register_so4chem_cpp)
            catchem_register_so4chem_cpp();
        if (catchem_register_wetdep_cpp)
            catchem_register_wetdep_cpp();
        registered = true;
    }

} // namespace

extern "C" {

void* catchem_core_create(int nc, int nl, int ns) {
    return static_cast<void*>(new catchem::Core(nc, nl, ns));
}

void* catchem_core_create_from_config(const char* config_file) {
    try {
        register_builtin_processes();
        return static_cast<void*>(new catchem::Core(config_file));
    } catch (const std::exception& e) {
        std::cerr << "CATChem API Error: Failed to create Core from config '" << config_file
                  << "'. Details: " << e.what() << std::endl;
        return nullptr;
    }
}

void* catchem_core_create_from_config_with_grid(const char* config_file, int ncols, int nlevels) {
    try {
        register_builtin_processes();
        return static_cast<void*>(new catchem::Core(config_file, ncols, nlevels));
    } catch (const std::exception& e) {
        std::cerr << "CATChem API Error: Failed to create Core from config '" << config_file << "' with grid (" << ncols
                  << ", " << nlevels << "). Details: " << e.what() << std::endl;
        return nullptr;
    }
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

double* catchem_state_get_species_conc_pointer(void* state_ptr, int species_index) {
    if (state_ptr == nullptr)
        return nullptr;
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    int idx_0 = species_index - 1;
    if (state->chem.conc && state->chem.conc->host_data() && idx_0 >= 0 && idx_0 < state->n_species) {
        return state->chem.conc->host_data() + static_cast<size_t>(idx_0) * state->n_cols * state->n_levels;
    }
    return nullptr;
}

int catchem_core_run_timestep(void* core_ptr, double dt) {
    try {
        auto* core = static_cast<catchem::Core*>(core_ptr);
        core->run_timestep(dt);
        return 0;
    } catch (const std::exception& e) {
        std::cerr << "CATChem API Error: Exception caught during core run_timestep: " << e.what() << std::endl;
        return -1;
    }
}

void catchem_core_add_process_by_name(void* core_ptr, const char* name) {
    register_builtin_processes();
    auto* core = static_cast<catchem::Core*>(core_ptr);
    auto process = catchem::ProcessRegistry::get_instance().create(name);
    process->init(core->get_state_manager());
    core->add_process(process);
}

int catchem_core_get_num_processes(void* core_ptr) {
    if (core_ptr == nullptr) {
        return 0;
    }
    auto* core = static_cast<catchem::Core*>(core_ptr);
    return static_cast<int>(core->get_num_processes());
}

void catchem_diag_register(void* core_ptr, const char* name, const char* desc, const char* units, int rank, int dim1,
                           int dim2, int dim3) {
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

int catchem_diag_get_rank(void* core_ptr, const char* name) {
    if (!core_ptr || !name) return 0;
    auto* core = static_cast<catchem::Core*>(core_ptr);
    auto diag_mgr = core->get_diagnostic_manager();
    if (!diag_mgr || !diag_mgr->has_field(name)) return 0;
    auto field = diag_mgr->get_field(name);
    return field ? static_cast<int>(field->dimensions.size()) : 0;
}

void catchem_diag_get_dims(void* core_ptr, const char* name, int* dims_out) {
    if (!core_ptr || !name || !dims_out) return;
    auto* core = static_cast<catchem::Core*>(core_ptr);
    auto diag_mgr = core->get_diagnostic_manager();
    if (!diag_mgr || !diag_mgr->has_field(name)) return;
    auto field = diag_mgr->get_field(name);
    if (!field) return;
    for (size_t i = 0; i < field->dimensions.size(); ++i) {
        dims_out[i] = field->dimensions[i];
    }
}

void catchem_diag_sync_to_host(void* core_ptr) {
    auto* core = static_cast<catchem::Core*>(core_ptr);
    core->get_diagnostic_manager()->sync_to_host();
}

void catchem_diag_reset(void* core_ptr) {
    auto* core = static_cast<catchem::Core*>(core_ptr);
    core->get_diagnostic_manager()->reset_all();
}

int catchem_diag_get_count(void* core_ptr) {
    auto* core = static_cast<catchem::Core*>(core_ptr);
    return static_cast<int>(core->get_diagnostic_manager()->get_registered_names().size());
}

void catchem_diag_get_name_at(void* core_ptr, int index, char* name_out) {
    auto* core = static_cast<catchem::Core*>(core_ptr);
    auto names = core->get_diagnostic_manager()->get_registered_names();
    if (index >= 0 && index < static_cast<int>(names.size())) {
        std::strcpy(name_out, names[index].c_str());
    } else {
        name_out[0] = '\0';
    }
}

void catchem_state_load_species_config(void* state_ptr, const char* filename) {
    try {
        auto* state = static_cast<catchem::StateManager*>(state_ptr);
        state->load_species_config(filename);
    } catch (const std::exception& e) {
        std::cerr << "CATChem API Error: Failed to load species configuration '" << filename
                  << "'. Details: " << e.what() << std::endl;
    }
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

void catchem_state_get_species_name_at(void* state_ptr, int index, char* name_out) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    int idx_0 = index - 1;
    if (idx_0 >= 0 && idx_0 < static_cast<int>(state->chem.species_list.size())) {
        copy_string_to_buffer(state->chem.species_list[idx_0].short_name, name_out, 64);
    } else {
        copy_string_to_buffer("", name_out, 64);
    }
}

void catchem_state_get_species_long_name_at(void* state_ptr, int index, char* name_out) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    int idx_0 = index - 1;
    if (idx_0 >= 0 && idx_0 < static_cast<int>(state->chem.species_list.size())) {
        copy_string_to_buffer(state->chem.species_list[idx_0].long_name, name_out, 128);
    } else {
        copy_string_to_buffer("", name_out, 128);
    }
}

void catchem_state_get_species_desc_at(void* state_ptr, int index, char* desc_out) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    int idx_0 = index - 1;
    if (idx_0 >= 0 && idx_0 < static_cast<int>(state->chem.species_list.size())) {
        copy_string_to_buffer(state->chem.species_list[idx_0].description, desc_out, 256);
    } else {
        copy_string_to_buffer("", desc_out, 256);
    }
}

double catchem_state_get_species_density(void* state_ptr, int index) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    int idx_0 = index - 1;
    if (idx_0 >= 0 && idx_0 < static_cast<int>(state->chem.species_list.size())) {
        return state->chem.species_list[idx_0].density;
    }
    return 0.0;
}

double catchem_state_get_species_radius(void* state_ptr, int index) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    int idx_0 = index - 1;
    if (idx_0 >= 0 && idx_0 < static_cast<int>(state->chem.species_list.size())) {
        return state->chem.species_list[idx_0].radius;
    }
    return 0.0;
}

double catchem_state_get_species_lower_radius(void* state_ptr, int index) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    int idx_0 = index - 1;
    if (idx_0 >= 0 && idx_0 < static_cast<int>(state->chem.species_list.size())) {
        return state->chem.species_list[idx_0].lower_radius;
    }
    return 0.0;
}

double catchem_state_get_species_upper_radius(void* state_ptr, int index) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    int idx_0 = index - 1;
    if (idx_0 >= 0 && idx_0 < static_cast<int>(state->chem.species_list.size())) {
        return state->chem.species_list[idx_0].upper_radius;
    }
    return 0.0;
}

int catchem_state_is_species_dust(void* state_ptr, int index) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    int idx_0 = index - 1;
    if (idx_0 >= 0 && idx_0 < static_cast<int>(state->chem.species_list.size())) {
        return state->chem.species_list[idx_0].is_dust ? 1 : 0;
    }
    return 0;
}

int catchem_state_is_species_seasalt(void* state_ptr, int index) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    int idx_0 = index - 1;
    if (idx_0 >= 0 && idx_0 < static_cast<int>(state->chem.species_list.size())) {
        return state->chem.species_list[idx_0].is_seasalt ? 1 : 0;
    }
    return 0;
}

int catchem_state_is_species_drydep(void* state_ptr, int index) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    int idx_0 = index - 1;
    if (idx_0 >= 0 && idx_0 < static_cast<int>(state->chem.species_list.size())) {
        return state->chem.species_list[idx_0].is_drydep ? 1 : 0;
    }
    return 0;
}

int catchem_state_is_species_wetdep(void* state_ptr, int index) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    int idx_0 = index - 1;
    if (idx_0 >= 0 && idx_0 < static_cast<int>(state->chem.species_list.size())) {
        return state->chem.species_list[idx_0].is_wetdep ? 1 : 0;
    }
    return 0;
}

void catchem_state_get_species_mie_name(void* state_ptr, int index, char* mie_out) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    int idx_0 = index - 1;
    if (idx_0 >= 0 && idx_0 < static_cast<int>(state->chem.species_list.size())) {
        copy_string_to_buffer(state->chem.species_list[idx_0].mie_name, mie_out, 64);
    } else {
        copy_string_to_buffer("", mie_out, 64);
    }
}

void catchem_state_derive_bxheight(void* state_ptr) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    state->derive_bxheight();
}

void catchem_state_derive_airden_dry(void* state_ptr) {
    auto* state = static_cast<catchem::StateManager*>(state_ptr);
    state->derive_airden_dry();
}

void catchem_get_grid_dimensions(void* core_ptr, int* nx, int* ny, int* nz) {
    if (core_ptr == nullptr)
        return;
    auto* core = static_cast<catchem::Core*>(core_ptr);
    auto grid = core->get_grid_manager();
    if (nx)
        *nx = grid->geometry.nx;
    if (ny)
        *ny = grid->geometry.ny;
    if (nz)
        *nz = grid->geometry.nz;
}

double catchem_get_config_timestep(void* core_ptr) {
    if (core_ptr == nullptr)
        return 0.0;
    auto* core = static_cast<catchem::Core*>(core_ptr);
    return core->get_config_manager()->data.runtime.dt;
}

int catchem_config_get_output_frequency(void* core_ptr) {
    if (core_ptr == nullptr)
        return 0;
    auto* core = static_cast<catchem::Core*>(core_ptr);
    return core->get_config_manager()->data.diagnostics.output.frequency;
}

int catchem_config_get_compress_level(void* core_ptr) {
    if (core_ptr == nullptr)
        return 0;
    auto* core = static_cast<catchem::Core*>(core_ptr);
    return core->get_config_manager()->data.diagnostics.output.compress_lev;
}

void catchem_config_get_output_directory(void* core_ptr, char* buffer, int max_len) {
    if (core_ptr == nullptr) {
        copy_string_to_buffer("./", buffer, max_len);
        return;
    }
    auto* core = static_cast<catchem::Core*>(core_ptr);
    copy_string_to_buffer(core->get_config_manager()->data.diagnostics.output.directory, buffer, max_len);
}

void catchem_config_get_output_prefix(void* core_ptr, char* buffer, int max_len) {
    if (core_ptr == nullptr) {
        copy_string_to_buffer("catchem_diag", buffer, max_len);
        return;
    }
    auto* core = static_cast<catchem::Core*>(core_ptr);
    copy_string_to_buffer(core->get_config_manager()->data.diagnostics.output.prefix, buffer, max_len);
}

int catchem_config_get_latlon_output(void* core_ptr) {
    if (core_ptr == nullptr)
        return 0;
    auto* core = static_cast<catchem::Core*>(core_ptr);
    return core->get_config_manager()->data.diagnostics.output.format == "latlon" ? 1 : 0;
}

int catchem_config_get_diag_enabled(void* core_ptr) {
    if (core_ptr == nullptr)
        return 0;
    auto* core = static_cast<catchem::Core*>(core_ptr);
    return core->get_config_manager()->data.diagnostics.output.enabled ? 1 : 0;
}

int catchem_config_get_diag_species_count(void* core_ptr) {
    if (core_ptr == nullptr)
        return 0;
    auto* core = static_cast<catchem::Core*>(core_ptr);
    return static_cast<int>(core->get_config_manager()->data.diagnostics.output.diag_list.size());
}

void catchem_config_get_diag_species_at(void* core_ptr, int index, char* buffer, int max_len) {
    if (core_ptr == nullptr)
        return;
    auto* core = static_cast<catchem::Core*>(core_ptr);
    const auto& diag_list = core->get_config_manager()->data.diagnostics.output.diag_list;
    if (index >= 0 && index < static_cast<int>(diag_list.size())) {
        copy_string_to_buffer(diag_list[index], buffer, max_len);
    } else {
        copy_string_to_buffer("", buffer, max_len);
    }
}

int catchem_config_get_process_active(void* core_ptr, const char* process_name) {
    if (core_ptr == nullptr || process_name == nullptr)
        return 0;
    auto* core = static_cast<catchem::Core*>(core_ptr);
    const auto& processes = core->get_config_manager()->data.processes;
    auto it = processes.find(std::string(process_name));
    if (it != processes.end()) {
        return it->second.activate ? 1 : 0;
    }
    return 0;
}

int catchem_config_has_emission_mapping(void* core_ptr) {
    if (core_ptr == nullptr)
        return 0;
    auto* core = static_cast<catchem::Core*>(core_ptr);
    return !core->get_config_manager()->data.emission_mappings.empty() ? 1 : 0;
}

int catchem_config_get_emission_category_count(void* core_ptr) {
    if (core_ptr == nullptr)
        return 0;
    auto* core = static_cast<catchem::Core*>(core_ptr);
    return static_cast<int>(core->get_config_manager()->data.emission_mappings.size());
}

void catchem_config_get_emission_category_name_at(void* core_ptr, int index, char* name_out, int max_len) {
    if (core_ptr == nullptr) {
        copy_string_to_buffer("", name_out, max_len);
        return;
    }
    auto* core = static_cast<catchem::Core*>(core_ptr);
    const auto& mappings = core->get_config_manager()->data.emission_mappings;
    if (index >= 0 && index < static_cast<int>(mappings.size())) {
        auto it = mappings.begin();
        std::advance(it, index);
        copy_string_to_buffer(it->first, name_out, max_len);
    } else {
        copy_string_to_buffer("", name_out, max_len);
    }
}

int catchem_config_is_emission_category_active(void* core_ptr, const char* category_name) {
    if (core_ptr == nullptr || category_name == nullptr)
        return 0;
    auto* core = static_cast<catchem::Core*>(core_ptr);
    return core->get_config_manager()->is_category_active(category_name) ? 1 : 0;
}

int catchem_config_get_emission_field_count(void* core_ptr, const char* category_name) {
    if (core_ptr == nullptr || category_name == nullptr)
        return 0;
    auto* core = static_cast<catchem::Core*>(core_ptr);
    const auto& mappings = core->get_config_manager()->data.emission_mappings;
    auto it = mappings.find(std::string(category_name));
    if (it != mappings.end()) {
        return static_cast<int>(it->second.fields.size());
    }
    return 0;
}

void catchem_config_get_emission_field_name_at(void* core_ptr, const char* category_name, int field_idx, char* name_out,
                                               int max_len) {
    if (core_ptr == nullptr || category_name == nullptr) {
        copy_string_to_buffer("", name_out, max_len);
        return;
    }
    auto* core = static_cast<catchem::Core*>(core_ptr);
    const auto& mappings = core->get_config_manager()->data.emission_mappings;
    auto it = mappings.find(std::string(category_name));
    if (it != mappings.end() && field_idx >= 0 && field_idx < static_cast<int>(it->second.fields.size())) {
        auto fit = it->second.fields.begin();
        std::advance(fit, field_idx);
        copy_string_to_buffer(fit->first, name_out, max_len);
    } else {
        copy_string_to_buffer("", name_out, max_len);
    }
}

int catchem_config_get_emission_species_map_count(void* core_ptr, const char* category_name, const char* field_name) {
    if (core_ptr == nullptr || category_name == nullptr || field_name == nullptr)
        return 0;
    auto* core = static_cast<catchem::Core*>(core_ptr);
    const auto& mappings = core->get_config_manager()->data.emission_mappings;
    auto it = mappings.find(std::string(category_name));
    if (it != mappings.end()) {
        auto fit = it->second.fields.find(std::string(field_name));
        if (fit != it->second.fields.end()) {
            return static_cast<int>(fit->second.map.size());
        }
    }
    return 0;
}

void catchem_config_get_emission_species_map_at(void* core_ptr, const char* category_name, const char* field_name,
                                                int map_idx, char* target_species_out, int max_len, double* scale_out,
                                                int* species_idx_out) {
    if (core_ptr == nullptr || category_name == nullptr || field_name == nullptr) {
        copy_string_to_buffer("", target_species_out, max_len);
        if (scale_out)
            *scale_out = 1.0;
        if (species_idx_out)
            *species_idx_out = -1;
        return;
    }
    auto* core = static_cast<catchem::Core*>(core_ptr);
    const auto& mappings = core->get_config_manager()->data.emission_mappings;
    auto it = mappings.find(std::string(category_name));
    if (it != mappings.end()) {
        auto fit = it->second.fields.find(std::string(field_name));
        if (fit != it->second.fields.end() && map_idx >= 0 && map_idx < static_cast<int>(fit->second.map.size())) {
            copy_string_to_buffer(fit->second.map[map_idx], target_species_out, max_len);
            if (scale_out) {
                *scale_out = (map_idx < static_cast<int>(fit->second.scale.size())) ? fit->second.scale[map_idx] : 1.0;
            }
            if (species_idx_out) {
                *species_idx_out =
                    catchem_state_get_species_index(core->get_state_manager().get(), fit->second.map[map_idx].c_str());
            }
            return;
        }
    }
    copy_string_to_buffer("", target_species_out, max_len);
    if (scale_out)
        *scale_out = 1.0;
    if (species_idx_out)
        *species_idx_out = -1;
}

int catchem_config_get_yaml_bool(void* core_ptr, const char* yaml_path, int default_val) {
    if (core_ptr == nullptr || yaml_path == nullptr)
        return default_val;
    auto* core = static_cast<catchem::Core*>(core_ptr);
    return core->get_config_manager()->get_bool(yaml_path, default_val != 0) ? 1 : 0;
}

double catchem_config_get_yaml_double(void* core_ptr, const char* yaml_path, double default_val) {
    if (core_ptr == nullptr || yaml_path == nullptr)
        return default_val;
    auto* core = static_cast<catchem::Core*>(core_ptr);
    return core->get_config_manager()->get_double(yaml_path, default_val);
}

int catchem_config_get_yaml_int(void* core_ptr, const char* yaml_path, int default_val) {
    if (core_ptr == nullptr || yaml_path == nullptr)
        return default_val;
    auto* core = static_cast<catchem::Core*>(core_ptr);
    return core->get_config_manager()->get_int(yaml_path, default_val);
}

void catchem_config_get_yaml_string(void* core_ptr, const char* yaml_path, char* val_out, int max_len,
                                    const char* default_val) {
    if (core_ptr == nullptr || yaml_path == nullptr) {
        copy_string_to_buffer(default_val ? default_val : "", val_out, max_len);
        return;
    }
    auto* core = static_cast<catchem::Core*>(core_ptr);
    std::string result = core->get_config_manager()->get_string(yaml_path, default_val ? default_val : "");
    copy_string_to_buffer(result, val_out, max_len);
}

void catchem_config_find_fengsha_static_file(void* core_ptr, char* val_out, int max_len) {
    if (core_ptr == nullptr) {
        copy_string_to_buffer("", val_out, max_len);
        return;
    }
    auto* core = static_cast<catchem::Core*>(core_ptr);
    std::string result = core->get_config_manager()->find_process_file_setting("fengsha");
    copy_string_to_buffer(result, val_out, max_len);
}

int catchem_config_get_yaml_list_count(void* core_ptr, const char* yaml_path) {
    if (core_ptr == nullptr || yaml_path == nullptr)
        return 0;
    auto* core = static_cast<catchem::Core*>(core_ptr);
    return static_cast<int>(core->get_config_manager()->get_string_list(yaml_path).size());
}

void catchem_config_get_yaml_list_at(void* core_ptr, const char* yaml_path, int index, char* val_out, int max_len) {
    if (core_ptr == nullptr || yaml_path == nullptr || index < 0) {
        copy_string_to_buffer("", val_out, max_len);
        return;
    }
    auto* core = static_cast<catchem::Core*>(core_ptr);
    auto list = core->get_config_manager()->get_string_list(yaml_path);
    if (index < static_cast<int>(list.size())) {
        copy_string_to_buffer(list[index], val_out, max_len);
    } else {
        copy_string_to_buffer("", val_out, max_len);
    }
}

// =========================================================================
// TimeState C-Linkable API Implementation
// =========================================================================

void* catchem_time_state_create() {
    try {
        return static_cast<void*>(new catchem::TimeState());
    } catch (...) {
        return nullptr;
    }
}

void catchem_time_state_destroy(void* ptr) {
    try {
        delete static_cast<catchem::TimeState*>(ptr);
    } catch (...) {
    }
}

int catchem_time_state_init(void* ptr, int year, int month, int day, int hour, int minute, int second,
                            double timestep) {
    try {
        auto* ts = static_cast<catchem::TimeState*>(ptr);
        ts->year = year;
        ts->month = month;
        ts->day = day;
        ts->hour = hour;
        ts->minute = minute;
        ts->second = second;
        ts->timestep = timestep;
        ts->calculate_derived_fields();
        return 0;
    } catch (...) {
        return -1;
    }
}

int catchem_time_state_advance(void* ptr, double dt) {
    try {
        static_cast<catchem::TimeState*>(ptr)->advance(dt);
        return 0;
    } catch (...) {
        return -1;
    }
}

int catchem_time_state_reset(void* ptr) {
    try {
        auto* ts = static_cast<catchem::TimeState*>(ptr);
        ts->year = 2000;
        ts->month = 1;
        ts->day = 1;
        ts->hour = 0;
        ts->minute = 0;
        ts->second = 0;
        ts->timestep = 3600.0;
        ts->calculate_derived_fields();
        return 0;
    } catch (...) {
        return -1;
    }
}

int catchem_time_state_get_year(void* ptr) {
    try {
        return static_cast<catchem::TimeState*>(ptr)->year;
    } catch (...) {
        return 0;
    }
}

int catchem_time_state_get_month(void* ptr) {
    try {
        return static_cast<catchem::TimeState*>(ptr)->month;
    } catch (...) {
        return 0;
    }
}

int catchem_time_state_get_day(void* ptr) {
    try {
        return static_cast<catchem::TimeState*>(ptr)->day;
    } catch (...) {
        return 0;
    }
}

int catchem_time_state_get_hour(void* ptr) {
    try {
        return static_cast<catchem::TimeState*>(ptr)->hour;
    } catch (...) {
        return 0;
    }
}

int catchem_time_state_get_minute(void* ptr) {
    try {
        return static_cast<catchem::TimeState*>(ptr)->minute;
    } catch (...) {
        return 0;
    }
}

int catchem_time_state_get_second(void* ptr) {
    try {
        return static_cast<catchem::TimeState*>(ptr)->second;
    } catch (...) {
        return 0;
    }
}

double catchem_time_state_get_timestep(void* ptr) {
    try {
        return static_cast<catchem::TimeState*>(ptr)->timestep;
    } catch (...) {
        return 0.0;
    }
}

double catchem_time_state_get_julian_date(void* ptr) {
    try {
        return static_cast<catchem::TimeState*>(ptr)->julian_date;
    } catch (...) {
        return 0.0;
    }
}

int catchem_time_state_get_doy(void* ptr) {
    try {
        return static_cast<catchem::TimeState*>(ptr)->doy;
    } catch (...) {
        return 0;
    }
}

double catchem_time_state_get_cos_sza(void* ptr, double lat, double lon, bool mid_timestep) {
    try {
        return static_cast<catchem::TimeState*>(ptr)->get_cos_sza(lat, lon, mid_timestep);
    } catch (...) {
        return 0.0;
    }
}

int catchem_time_state_get_timezone_offset(void* ptr, double lon) {
    try {
        int offset = static_cast<int>(lon / 15.0);
        return std::max(-12, std::min(14, offset));
    } catch (...) {
        return 0;
    }
}

bool catchem_time_state_is_leap_year(int year) {
    return catchem::TimeState::is_leap_year(year);
}

int catchem_time_state_get_days_in_month(int month, int year) {
    return catchem::TimeState::get_days_in_month(month, year);
}

bool catchem_time_state_is_global_holiday(int month, int day) {
    return (month == 1 && day == 1) || (month == 12 && day == 25);
}

bool catchem_time_state_is_us_holiday(int month, int day) {
    return (month == 7 && day == 4) || (month == 11 && day >= 22 && day <= 28);
}

// =========================================================================
// UnitConversion C-Linkable API Implementation
// =========================================================================

double catchem_convert_concentration(double val, const char* from_units, const char* to_units, double mw, double temp,
                                     double press, int* rc) {
    try {
        *rc = 0;
        std::string from = catchem::unit_conversion::to_upper(from_units);
        std::string to = catchem::unit_conversion::to_upper(to_units);
        if (from == to) {
            return val;
        }

        // Identify and extract VMR scale factors relative to PPBV
        auto get_vmr_factor = [](const std::string& unit, bool& is_vmr) -> double {
            is_vmr = true;
            if (unit == "PPMV" || unit == "PPM")
                return 1e3;
            if (unit == "PPBV" || unit == "PPB")
                return 1.0;
            if (unit == "PPTV" || unit == "PPT")
                return 1e-3;
            is_vmr = false;
            return 1.0;
        };

        bool from_is_vmr = false;
        double from_factor = get_vmr_factor(from, from_is_vmr);
        bool to_is_vmr = false;
        double to_factor = get_vmr_factor(to, to_is_vmr);

        // Direct VMR-to-VMR conversion
        if (from_is_vmr && to_is_vmr) {
            return val * (from_factor / to_factor);
        }

        // Normalize VMR conversion from/to mass/volume units
        std::string from_normalized = from_is_vmr ? "PPBV" : from;
        std::string to_normalized = to_is_vmr ? "PPBV" : to;
        double input_val = from_is_vmr ? (val * from_factor) : val;

        double result = 0.0;
        std::string key = from_normalized + " -> " + to_normalized;

        if (key == "PPBV -> UG/M3" || key == "PPBV -> UG M-3" || key == "PPBV -> UG/M^3") {
            result = catchem::unit_conversion::ppbv_to_ugm3(input_val, mw, temp, press);
        } else if (key == "UG/M3 -> PPBV" || key == "UG M-3 -> PPBV" || key == "UG/M^3 -> PPBV") {
            result = catchem::unit_conversion::ugm3_to_ppbv(input_val, mw, temp, press);
        } else if (key == "PPBV -> MG/M3" || key == "PPBV -> MG M-3" || key == "PPBV -> MG/M^3") {
            result = catchem::unit_conversion::ppbv_to_ugm3(input_val, mw, temp, press) * 1e-3;
        } else if (key == "MG/M3 -> PPBV" || key == "MG M-3 -> PPBV" || key == "MG/M^3 -> PPBV") {
            result = catchem::unit_conversion::ugm3_to_ppbv(input_val * 1e3, mw, temp, press);
        } else if (key == "MOLEC/CM3 -> PPBV" || key == "MOLEC CM-3 -> PPBV" || key == "MOLEC/CM^3 -> PPBV") {
            result = catchem::unit_conversion::molcm3_to_ppbv(input_val, temp, press);
        } else if (key == "PPBV -> MOLEC/CM3" || key == "PPBV -> MOLEC CM-3" || key == "PPBV -> MOLEC/CM^3") {
            result = catchem::unit_conversion::ppbv_to_molcm3(input_val, temp, press);
        } else if (key == "MOLEC/CM3 -> UG/M3" || key == "MOLEC CM-3 -> UG/M3" || key == "MOLEC/CM^3 -> UG/M3" ||
                   key == "MOLEC/CM3 -> UG M-3" || key == "MOLEC/CM3 -> UG/M^3") {
            double ppbv = catchem::unit_conversion::molcm3_to_ppbv(input_val, temp, press);
            result = catchem::unit_conversion::ppbv_to_ugm3(ppbv, mw, temp, press);
        } else {
            *rc = -1;
            return val;
        }

        // If target is a VMR, scale output from PPBV to target
        if (to_is_vmr) {
            result /= to_factor;
        }

        return result;
    } catch (...) {
        *rc = -1;
        return val;
    }
}

double catchem_convert_pressure(double val, const char* from_units, const char* to_units, int* rc) {
    try {
        *rc = 0;
        if (catchem::unit_conversion::to_upper(from_units) == catchem::unit_conversion::to_upper(to_units)) {
            return val;
        }
        return catchem::unit_conversion::convert_pressure(val, from_units, to_units, *rc);
    } catch (...) {
        *rc = -1;
        return val;
    }
}

double catchem_convert_temperature(double val, const char* from_units, const char* to_units, int* rc) {
    try {
        *rc = 0;
        if (catchem::unit_conversion::to_upper(from_units) == catchem::unit_conversion::to_upper(to_units)) {
            return val;
        }
        return catchem::unit_conversion::convert_temperature(val, from_units, to_units, *rc);
    } catch (...) {
        *rc = -1;
        return val;
    }
}

double catchem_convert_flux(double val, const char* from_units, const char* to_units, double mw, int* rc) {
    try {
        *rc = 0;
        if (catchem::unit_conversion::to_upper(from_units) == catchem::unit_conversion::to_upper(to_units)) {
            return val;
        }
        return catchem::unit_conversion::convert_flux(val, from_units, to_units, mw, *rc);
    } catch (...) {
        *rc = -1;
        return val;
    }
}

double catchem_convert_rate_constant(double val, const char* from_units, const char* to_units, int* rc) {
    try {
        *rc = 0;
        if (catchem::unit_conversion::to_upper(from_units) == catchem::unit_conversion::to_upper(to_units)) {
            return val;
        }
        return catchem::unit_conversion::convert_rate_constant(val, from_units, to_units, *rc);
    } catch (...) {
        *rc = -1;
        return val;
    }
}

double catchem_convert_mass_units(double val, const char* from_units, const char* to_units, int* rc) {
    try {
        *rc = 0;
        if (catchem::unit_conversion::to_upper(from_units) == catchem::unit_conversion::to_upper(to_units)) {
            return val;
        }
        return catchem::unit_conversion::convert_mass_units(val, from_units, to_units, *rc);
    } catch (...) {
        *rc = -1;
        return val;
    }
}

double catchem_calculate_air_density(double temp, double press, double humidity, bool use_humidity) {
    try {
        return catchem::unit_conversion::calculate_air_density(temp, press, humidity, use_humidity);
    } catch (...) {
        return 0.0;
    }
}

double catchem_calculate_molecular_weight(const char* formula) {
    try {
        return catchem::unit_conversion::calculate_molecular_weight(formula);
    } catch (...) {
        return 0.0;
    }
}

double catchem_convert_imperial(double val, const char* from_units, const char* to_units, const char* category,
                                int* rc) {
    try {
        return catchem::unit_conversion::convert_imperial(val, from_units, to_units, category, *rc);
    } catch (...) {
        *rc = -1;
        return val;
    }
}

int catchem_convert_process_concentration_units(catchem::fp* values, int size, const char* from_units,
                                                const char* to_units, catchem::fp mw, catchem::fp temp,
                                                catchem::fp press) {
    try {
        int rc = 0;
        for (int i = 0; i < size; ++i) {
            values[i] = static_cast<catchem::fp>(catchem_convert_concentration(
                static_cast<double>(values[i]), from_units, to_units, static_cast<double>(mw),
                static_cast<double>(temp), static_cast<double>(press), &rc));
            if (rc != 0)
                return rc;
        }
        return 0;
    } catch (...) {
        return -1;
    }
}

int catchem_convert_process_flux_units(catchem::fp* values, int size, const char* from_units, const char* to_units,
                                       catchem::fp mw) {
    try {
        int rc = 0;
        for (int i = 0; i < size; ++i) {
            values[i] = static_cast<catchem::fp>(catchem_convert_flux(static_cast<double>(values[i]), from_units,
                                                                      to_units, static_cast<double>(mw), &rc));
            if (rc != 0)
                return rc;
        }
        return 0;
    } catch (...) {
        return -1;
    }
}

// =========================================================================
// Species Metadata and Property Query C-API
// =========================================================================
double catchem_state_get_species_viscosity(void* state_ptr, int index) {
    try {
        auto* state = static_cast<catchem::StateManager*>(state_ptr);
        int idx_0 = index - 1;
        if (idx_0 >= 0 && idx_0 < static_cast<int>(state->chem.species_list.size())) {
            return state->chem.species_list[idx_0].viscosity;
        }
        return 0.0;
    } catch (...) {
        return 0.0;
    }
}

int catchem_state_get_species_is_tracer(void* state_ptr, int index) {
    try {
        auto* state = static_cast<catchem::StateManager*>(state_ptr);
        int idx_0 = index - 1;
        if (idx_0 >= 0 && idx_0 < static_cast<int>(state->chem.species_list.size())) {
            return state->chem.species_list[idx_0].is_tracer ? 1 : 0;
        }
        return 0;
    } catch (...) {
        return 0;
    }
}

int catchem_state_get_species_is_advected(void* state_ptr, int index) {
    try {
        auto* state = static_cast<catchem::StateManager*>(state_ptr);
        int idx_0 = index - 1;
        if (idx_0 >= 0 && idx_0 < static_cast<int>(state->chem.species_list.size())) {
            return state->chem.species_list[idx_0].is_advected ? 1 : 0;
        }
        return 0;
    } catch (...) {
        return 0;
    }
}

int catchem_state_get_species_is_photolysis(void* state_ptr, int index) {
    try {
        auto* state = static_cast<catchem::StateManager*>(state_ptr);
        int idx_0 = index - 1;
        if (idx_0 >= 0 && idx_0 < static_cast<int>(state->chem.species_list.size())) {
            return state->chem.species_list[idx_0].is_photolysis ? 1 : 0;
        }
        return 0;
    } catch (...) {
        return 0;
    }
}

double catchem_state_get_species_dd_f0(void* state_ptr, int index) {
    try {
        auto* state = static_cast<catchem::StateManager*>(state_ptr);
        int idx_0 = index - 1;
        if (idx_0 >= 0 && idx_0 < static_cast<int>(state->chem.species_list.size())) {
            return state->chem.species_list[idx_0].dd_f0;
        }
        return 0.0;
    } catch (...) {
        return 0.0;
    }
}

double catchem_state_get_species_dd_hstar(void* state_ptr, int index) {
    try {
        auto* state = static_cast<catchem::StateManager*>(state_ptr);
        int idx_0 = index - 1;
        if (idx_0 >= 0 && idx_0 < static_cast<int>(state->chem.species_list.size())) {
            return state->chem.species_list[idx_0].dd_hstar;
        }
        return 0.0;
    } catch (...) {
        return 0.0;
    }
}

double catchem_state_get_species_dd_DvzAerSnow(void* state_ptr, int index) {
    try {
        auto* state = static_cast<catchem::StateManager*>(state_ptr);
        int idx_0 = index - 1;
        if (idx_0 >= 0 && idx_0 < static_cast<int>(state->chem.species_list.size())) {
            return state->chem.species_list[idx_0].dd_DvzAerSnow;
        }
        return 0.0;
    } catch (...) {
        return 0.0;
    }
}

double catchem_state_get_species_dd_DvzMinVal_snow(void* state_ptr, int index) {
    try {
        auto* state = static_cast<catchem::StateManager*>(state_ptr);
        int idx_0 = index - 1;
        if (idx_0 >= 0 && idx_0 < static_cast<int>(state->chem.species_list.size())) {
            return state->chem.species_list[idx_0].dd_DvzMinVal_snow;
        }
        return 0.0;
    } catch (...) {
        return 0.0;
    }
}

double catchem_state_get_species_dd_DvzMinVal_land(void* state_ptr, int index) {
    try {
        auto* state = static_cast<catchem::StateManager*>(state_ptr);
        int idx_0 = index - 1;
        if (idx_0 >= 0 && idx_0 < static_cast<int>(state->chem.species_list.size())) {
            return state->chem.species_list[idx_0].dd_DvzMinVal_land;
        }
        return 0.0;
    } catch (...) {
        return 0.0;
    }
}

double catchem_state_get_species_henry_k0(void* state_ptr, int index) {
    try {
        auto* state = static_cast<catchem::StateManager*>(state_ptr);
        int idx_0 = index - 1;
        if (idx_0 >= 0 && idx_0 < static_cast<int>(state->chem.species_list.size())) {
            return state->chem.species_list[idx_0].henry_k0;
        }
        return 0.0;
    } catch (...) {
        return 0.0;
    }
}

double catchem_state_get_species_henry_cr(void* state_ptr, int index) {
    try {
        auto* state = static_cast<catchem::StateManager*>(state_ptr);
        int idx_0 = index - 1;
        if (idx_0 >= 0 && idx_0 < static_cast<int>(state->chem.species_list.size())) {
            return state->chem.species_list[idx_0].henry_cr;
        }
        return 0.0;
    } catch (...) {
        return 0.0;
    }
}

double catchem_state_get_species_henry_pKa(void* state_ptr, int index) {
    try {
        auto* state = static_cast<catchem::StateManager*>(state_ptr);
        int idx_0 = index - 1;
        if (idx_0 >= 0 && idx_0 < static_cast<int>(state->chem.species_list.size())) {
            return state->chem.species_list[idx_0].henry_pKa;
        }
        return 0.0;
    } catch (...) {
        return 0.0;
    }
}

double catchem_state_get_species_wd_retfactor(void* state_ptr, int index) {
    try {
        auto* state = static_cast<catchem::StateManager*>(state_ptr);
        int idx_0 = index - 1;
        if (idx_0 >= 0 && idx_0 < static_cast<int>(state->chem.species_list.size())) {
            return state->chem.species_list[idx_0].wd_retfactor;
        }
        return 0.0;
    } catch (...) {
        return 0.0;
    }
}

int catchem_state_get_species_wd_LiqAndGas(void* state_ptr, int index) {
    try {
        auto* state = static_cast<catchem::StateManager*>(state_ptr);
        int idx_0 = index - 1;
        if (idx_0 >= 0 && idx_0 < static_cast<int>(state->chem.species_list.size())) {
            return state->chem.species_list[idx_0].wd_LiqAndGas ? 1 : 0;
        }
        return 0;
    } catch (...) {
        return 0;
    }
}

double catchem_state_get_species_wd_convfacI2G(void* state_ptr, int index) {
    try {
        auto* state = static_cast<catchem::StateManager*>(state_ptr);
        int idx_0 = index - 1;
        if (idx_0 >= 0 && idx_0 < static_cast<int>(state->chem.species_list.size())) {
            return state->chem.species_list[idx_0].wd_convfacI2G;
        }
        return 0.0;
    } catch (...) {
        return 0.0;
    }
}

void catchem_state_get_species_wd_rainouteff(void* state_ptr, int index, double* eff_out) {
    try {
        auto* state = static_cast<catchem::StateManager*>(state_ptr);
        int idx_0 = index - 1;
        if (idx_0 >= 0 && idx_0 < static_cast<int>(state->chem.species_list.size())) {
            auto& eff = state->chem.species_list[idx_0].wd_rainouteff;
            for (size_t i = 0; i < 3; ++i) {
                eff_out[i] = i < eff.size() ? eff[i] : 0.0;
            }
        } else {
            eff_out[0] = eff_out[1] = eff_out[2] = 0.0;
        }
    } catch (...) {
        eff_out[0] = eff_out[1] = eff_out[2] = 0.0;
    }
}

double catchem_state_get_species_wd_reevap_frac(void* state_ptr, int index) {
    try {
        auto* state = static_cast<catchem::StateManager*>(state_ptr);
        int idx_0 = index - 1;
        if (idx_0 >= 0 && idx_0 < static_cast<int>(state->chem.species_list.size())) {
            return state->chem.species_list[idx_0].wd_reevap_frac;
        }
        return 0.5;
    } catch (...) {
        return 0.5;
    }
}

double catchem_state_get_species_t_chem_loss(void* state_ptr, int index) {
    try {
        auto* state = static_cast<catchem::StateManager*>(state_ptr);
        int idx_0 = index - 1;
        if (idx_0 >= 0 && idx_0 < static_cast<int>(state->chem.species_list.size())) {
            return state->chem.species_list[idx_0].t_chem_loss;
        }
        return -1.0;
    } catch (...) {
        return -1.0;
    }
}

double catchem_state_get_species_BackgroundVV(void* state_ptr, int index) {
    try {
        auto* state = static_cast<catchem::StateManager*>(state_ptr);
        int idx_0 = index - 1;
        if (idx_0 >= 0 && idx_0 < static_cast<int>(state->chem.species_list.size())) {
            return state->chem.species_list[idx_0].BackgroundVV;
        }
        return 1.0e-20;
    } catch (...) {
        return 1.0e-20;
    }
}

// =========================================================================
// Meteorological Core Calculation C-API Definitions
// =========================================================================
double catchem_met_potential_temperature(double temp, double press, double sfc_press) {
    return catchem::met_utilities::potential_temperature(temp, press, sfc_press);
}

double catchem_met_virtual_temperature(double temp, double qv) {
    return catchem::met_utilities::virtual_temperature(temp, qv);
}

double catchem_met_dew_point(double temp, double rh) {
    return catchem::met_utilities::dew_point(temp, rh);
}

double catchem_met_relative_humidity(double temp, double qv, double press) {
    return catchem::met_utilities::relative_humidity(temp, qv, press);
}

double catchem_met_saturation_vapor_pressure(double temp) {
    return catchem::met_utilities::saturation_vapor_pressure(temp);
}

double catchem_met_monin_obukhov_length(double ustar, double t0, double hflux, double rho) {
    return catchem::met_utilities::monin_obukhov_length(ustar, t0, hflux, rho);
}

double catchem_met_friction_velocity(double tau, double rho) {
    return catchem::met_utilities::friction_velocity(tau, rho);
}

double catchem_met_cunningham_correction_factor(double dp, double lambda) {
    return catchem::met_utilities::cunningham_correction_factor(dp, lambda);
}

double catchem_met_mean_free_path_air(double temp, double press) {
    return catchem::met_utilities::mean_free_path_air(temp, press);
}

void catchem_met_solar_zenith_angle(int doy, double hour, double lat_rad, double lon_rad, double* sza_deg,
                                    double* cossza) {
    try {
        catchem::fp sza_tmp = 0.0;
        catchem::fp cos_tmp = 0.0;
        catchem::met_utilities::solar_zenith_angle(doy, static_cast<catchem::fp>(hour),
                                                   static_cast<catchem::fp>(lat_rad), static_cast<catchem::fp>(lon_rad),
                                                   sza_tmp, cos_tmp);
        *sza_deg = static_cast<double>(sza_tmp);
        *cossza = static_cast<double>(cos_tmp);
    } catch (...) {
        *sza_deg = 0.0;
        *cossza = 0.0;
    }
}

double catchem_met_mixing_ratio(double q) {
    return catchem::met_utilities::mixing_ratio(q);
}

double catchem_met_specific_humidity(double r) {
    return catchem::met_utilities::specific_humidity(r);
}

double catchem_met_dry_adiabatic_lapse_rate() {
    return catchem::met_utilities::dry_adiabatic_lapse_rate();
}

double catchem_met_bulk_richardson_number(double t0, double tz, double u, double z) {
    return catchem::met_utilities::bulk_richardson_number(t0, tz, u, z);
}

int catchem_met_stability_classification(double l) {
    return catchem::met_utilities::stability_classification(l);
}

double catchem_met_saturation_mixing_ratio(double p, double t) {
    return catchem::met_utilities::saturation_mixing_ratio(p, t);
}

double catchem_met_latent_heat_vaporization(double t) {
    return catchem::met_utilities::latent_heat_vaporization(t);
}

double catchem_met_psychrometric_constant(double p, double lv) {
    return catchem::met_utilities::psychrometric_constant(p, lv);
}

double catchem_met_wind_profile_loglaw(double ustar, double z, double z0) {
    return catchem::met_utilities::wind_profile_loglaw(ustar, z, z0);
}

double catchem_met_brunt_vaisala_frequency(double t0, double dtdz) {
    return catchem::met_utilities::brunt_vaisala_frequency(t0, dtdz);
}

double catchem_met_psi_m_businger(double zeta) {
    return catchem::met_utilities::psi_m_businger(zeta);
}

double catchem_met_psi_h_businger(double zeta) {
    return catchem::met_utilities::psi_h_businger(zeta);
}

double catchem_met_arrhenius_rate(double a, double ea, double t) {
    return catchem::met_utilities::arrhenius_rate(a, ea, t);
}

double catchem_met_henrys_law_constant(double h0, double dh, double t, double t0) {
    return catchem::met_utilities::henrys_law_constant(h0, dh, t, t0);
}

double catchem_met_photolysis_rate_scaling(double j0, double sza) {
    return catchem::met_utilities::photolysis_rate_scaling(j0, sza);
}

double catchem_met_ppm_to_ugm3(double ppm, double m, double t, double p) {
    return catchem::met_utilities::ppm_to_ugm3(ppm, m, t, p);
}

double catchem_met_ugm3_to_ppm(double ugm3, double m, double t, double p) {
    return catchem::met_utilities::ugm3_to_ppm(ugm3, m, t, p);
}

double catchem_met_stokes_settling_velocity(double dp, double rho_p, double rho_a, double mu, double cc) {
    return catchem::met_utilities::stokes_settling_velocity(dp, rho_p, rho_a, mu, cc);
}

double catchem_met_stokes_number(double rho_p, double d_p, double u, double mu, double l) {
    return catchem::met_utilities::stokes_number(rho_p, d_p, u, mu, l);
}

double catchem_met_nuclear_decay(double n0, double lambda, double t) {
    return catchem::met_utilities::nuclear_decay(n0, lambda, t);
}
}

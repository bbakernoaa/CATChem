/**
 * @file catchem_api.hpp
 * @brief Flat, BIND(C) linkable API endpoints for CATChem host model integration.
 */

#pragma once

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Creates the C++ Core orchestrator instance.
 * @param nc Number of horizontal columns.
 * @param nl Number of vertical levels.
 * @param ns Number of chemical species.
 * @return Opacity-wrapped void* handle pointing to catchem::Core.
 */
void* catchem_core_create(int nc, int nl, int ns);

/**
 * @brief Creates the C++ Core orchestrator from a YAML config file.
 * @param config_file Null-terminated filesystem path.
 * @return Opacity-wrapped void* handle pointing to catchem::Core.
 */
void* catchem_core_create_from_config(const char* config_file);

/**
 * @brief Destroys the Core orchestrator instance and releases heap memory.
 * @param core_ptr Pointer to the active catchem::Core instance.
 */
void catchem_core_destroy(void* core_ptr);

/**
 * @brief Extracts the underlying StateManager handle from Core.
 * @param core_ptr Pointer to the active catchem::Core instance.
 * @return Pointer to catchem::StateManager.
 */
void* catchem_core_get_state_manager(void* core_ptr);

/** @brief Bind a 1D double field to the StateManager registry. */
void catchem_state_bind_1d(void* state_ptr, const char* name, double* ptr);

/** @brief Bind a 2D double field to the StateManager registry. */
void catchem_state_bind_2d(void* state_ptr, const char* name, double* ptr);

/** @brief Bind a 3D double field to the StateManager registry. */
void catchem_state_bind_3d(void* state_ptr, const char* name, double* ptr);

/** @brief Bind a 2D meteorological field by name. */
void catchem_state_bind_met_2d(void* state_ptr, const char* name, double* ptr);

/** @brief Bind a 3D meteorological field by name. */
void catchem_state_bind_met_3d(void* state_ptr, const char* name, double* ptr);

/** @brief Binds the contiguous, multi-species unified chemistry concentrations array. */
void catchem_state_bind_unified_chemistry(void* state_ptr, double* ptr);

/** @brief Sets current simulation time within the state. */
void catchem_state_set_time(void* state_ptr, int yr, int mo, int dy, int hr, int mn, int sc, int doy, double tstep);

/** @brief Synchronizes registered host pointers to Kokkos device memory space. */
void catchem_state_sync_to_device(void* state_ptr);

/** @brief Synchronizes Kokkos device calculations back to host buffers. */
void catchem_state_sync_to_host(void* state_ptr);

/** @brief Retrieves direct host pointers from the 1D, 2D, or 3D fields. */
double* catchem_state_get_pointer_1d(void* state_ptr, const char* name);
double* catchem_state_get_pointer_2d(void* state_ptr, const char* name);
double* catchem_state_get_pointer_3d(void* state_ptr, const char* name);

/**
 * @brief Executes a single timestepped execution over scheduled processes.
 * @param core_ptr Core pointer.
 * @param dt Step size in seconds.
 */
void catchem_core_run_timestep(void* core_ptr, double dt);

/** @brief Registers and attaches an active physics process handler. */
void catchem_core_add_process_by_name(void* core_ptr, const char* name);

// Grid and Configuration API
void catchem_get_grid_dimensions(void* core_ptr, int* nx, int* ny, int* nz);
double catchem_get_config_timestep(void* core_ptr);

// Diagnostic API
void catchem_diag_register(void* core_ptr, const char* name, const char* desc, const char* units, int rank, int dim1,
                           int dim2, int dim3);
void* catchem_diag_get_pointer(void* core_ptr, const char* name);
void catchem_diag_sync_to_host(void* core_ptr);
void catchem_diag_reset(void* core_ptr);
int catchem_diag_get_count(void* core_ptr);
void catchem_diag_get_name_at(void* core_ptr, int index, char* name_out);

// YAML Species Metadata
void catchem_state_load_species_config(void* state_ptr, const char* filename);
int catchem_state_get_species_count(void* state_ptr);
int catchem_state_get_species_index(void* state_ptr,
                                    const char* name); // returns 1-based index matching Fortran, or -1 if not found

// Categorized counts and list getters
int catchem_state_get_gas_species_count(void* state_ptr);
void catchem_state_get_gas_indices(void* state_ptr, int* indices_out); // populates 1-based indices
int catchem_state_get_aerosol_species_count(void* state_ptr);
void catchem_state_get_aerosol_indices(void* state_ptr, int* indices_out);

// Individual property getters (by 1-based index)
double catchem_state_get_species_mw(void* state_ptr, int index);
int catchem_state_is_species_gas(void* state_ptr, int index);
int catchem_state_is_species_aerosol(void* state_ptr, int index);

// Physics derivations
void catchem_state_derive_bxheight(void* state_ptr);
void catchem_state_derive_airden_dry(void* state_ptr);

#ifdef __cplusplus
}
#endif

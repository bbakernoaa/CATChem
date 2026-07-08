#pragma once

#ifdef __cplusplus
extern "C" {
#endif

void* catchem_core_create(int nc, int nl, int ns);
void catchem_core_destroy(void* core_ptr);
void* catchem_core_get_state_manager(void* core_ptr);
void catchem_state_bind_1d(void* state_ptr, const char* name, double* ptr);
void catchem_state_bind_2d(void* state_ptr, const char* name, double* ptr);
void catchem_state_bind_3d(void* state_ptr, const char* name, double* ptr);
void catchem_state_sync_to_device(void* state_ptr);
void catchem_state_sync_to_host(void* state_ptr);
double* catchem_state_get_pointer_1d(void* state_ptr, const char* name);
double* catchem_state_get_pointer_2d(void* state_ptr, const char* name);
double* catchem_state_get_pointer_3d(void* state_ptr, const char* name);
void catchem_core_run_timestep(void* core_ptr, double dt);

// Diagnostic API
void catchem_diag_register(void* core_ptr, const char* name, const char* desc, const char* units, int rank, int dim1, int dim2, int dim3);
void* catchem_diag_get_pointer(void* core_ptr, const char* name);
void catchem_diag_sync_to_host(void* core_ptr);
void catchem_diag_reset(void* core_ptr);

#ifdef __cplusplus
}
#endif

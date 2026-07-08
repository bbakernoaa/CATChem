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
void catchem_core_run_timestep(void* core_ptr, double dt);

#ifdef __cplusplus
}
#endif

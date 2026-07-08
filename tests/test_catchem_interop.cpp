#include "catchem_api.hpp"
#include <Kokkos_Core.hpp>
#include <cassert>
#include <iostream>
#include <vector>

// Mock Fortran physics scheme working directly on host array
void run_mock_fortran_physics(double* ptr, int n_cols, int n_levels) {
    // Simulate Fortran LayoutLeft (column-major) indexing: (i, j) -> i + j * n_cols
    for (int j = 0; j < n_levels; ++j) {
        for (int i = 0; i < n_cols; ++i) {
            ptr[i + j * n_cols] += 10.0; // Add tendency
        }
    }
}

int main(int argc, char* argv[]) {
    Kokkos::initialize(argc, argv);
    {
        int n_cols = 4;
        int n_levels = 5;
        int n_species = 2;

        // Allocate mock Fortran memory (column-major contiguous)
        std::vector<double> fortran_array(n_cols * n_levels, 1.0);

        // 1. Create Core & bind arrays
        void* core = catchem_core_create(n_cols, n_levels, n_species);
        void* state = catchem_core_get_state_manager(core);

        catchem_state_bind_2d(state, "temperature", fortran_array.data());

        // 2. Sync to active space
        catchem_state_sync_to_device(state);

        // 3. Execute Fortran process sequentially modifying the raw array on host
        run_mock_fortran_physics(fortran_array.data(), n_cols, n_levels);

        // Verify direct zero-copy modification
        assert(fortran_array[0] == 11.0);

        // 4. Sync up and clean up
        catchem_state_sync_to_host(state);
        catchem_core_destroy(core);

        std::cout << "SUCCESS: Interop Shared State Validation Passed!\n";
    }
    Kokkos::finalize();
    return 0;
}

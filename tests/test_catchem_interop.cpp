#include "catchem_api.hpp"
#include "catchem_core.hpp"
#include "catchem_diagnostic_manager.hpp"
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

// A dummy process that writes to a diagnostic field
class DummyDiagProcess : public catchem::ProcessInterface {
private:
    std::shared_ptr<catchem::DiagnosticManager> diag_mgr;
    int n_cols;
public:
    DummyDiagProcess(std::shared_ptr<catchem::DiagnosticManager> dm, int nc) : diag_mgr(dm), n_cols(nc) {}
    
    std::string get_name() const override { return "DummyDiagProcess"; }
    
    void init(std::shared_ptr<catchem::StateManager> state) override {}
    
    void run(std::shared_ptr<catchem::StateManager> state) override {
        // Retrieve the underlying diagnostic device View
        auto dust_flux = diag_mgr->get_device_view_2d("dust_emission_flux");
        
        // Capture View by value in the parallel kernel
        Kokkos::parallel_for("calculate_dust_emissions", 
            Kokkos::RangePolicy<Kokkos::DefaultExecutionSpace>(0, n_cols),
            KOKKOS_LAMBDA(int icol) {
                // Write directly to the diagnostic view
                dust_flux(icol, 0) = 42.0 + icol;
            }
        );
    }
    
    void finalize() override {}
};

int main(int argc, char* argv[]) {
    Kokkos::initialize(argc, argv);
    {
        // ==========================================
        // TEST 1: Phase 1 Shared Memory / Interop Test
        // ==========================================
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

        // ==========================================
        // TEST 2: Phase 2 Diagnostic Collection Test
        // ==========================================
        {
            int nx = 4;
            int ny = 1;
            int nz = 5;
            int n_cols = nx * ny;
            
            // 1. Create Core (creates StateManager & DiagnosticManager)
            void* core_ptr = catchem_core_create(n_cols, nz, 1);
            auto* core = static_cast<catchem::Core*>(core_ptr);
            auto diag_mgr = core->get_diagnostic_manager();
            
            // 2. Register diagnostic through C-API
            catchem_diag_register(core_ptr, "dust_emission_flux", "Dust flux", "kg/m2/s", 2, n_cols, 1, 0);
            
            // 3. Attach dummy diagnostic process
            core->add_process(std::make_shared<DummyDiagProcess>(diag_mgr, n_cols));
            
            // 4. Run timestep (runs dummy process and syncs diagnostics to host)
            catchem_core_run_timestep(core_ptr, 3600.0);
            
            // 5. Get host pointer and verify results
            void* host_ptr = catchem_diag_get_pointer(core_ptr, "dust_emission_flux");
            double* dust_flux_host = static_cast<double*>(host_ptr);
            
            bool passed = true;
            for (int i = 0; i < n_cols; ++i) {
                if (dust_flux_host[i] != 42.0 + i) { // Note LayoutLeft means col_i is inner dimension
                    std::cerr << "Diagnostic mismatch at col " << i << ": expected " << 42.0 + i 
                              << ", got " << dust_flux_host[i] << std::endl;
                    passed = false;
                }
            }
            
            if (passed) {
                std::cout << "SUCCESS: C++ Diagnostic Validation Passed!\n";
            } else {
                std::cout << "FAILURE: C++ Diagnostic Validation Failed!\n";
                catchem_core_destroy(core_ptr);
                Kokkos::finalize();
                return 1;
            }
            
            catchem_core_destroy(core_ptr);
        }
    }
    Kokkos::finalize();
    return 0;
}

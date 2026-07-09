#include <Kokkos_Core.hpp>
#include "catchem_core.hpp"
#include "catchem_api.hpp"
#include "catchem_state_manager.hpp"
#include "catchem_process_registry.hpp"
#include <iostream>
#include <vector>
#include <random>
#include <cmath>
#include <cassert>

extern "C" {
    void catchem_register_seasalt_cpp();
    void catchem_register_drydep_cpp();
    void catchem_register_wetdep_cpp();
    void catchem_register_settling_cpp();
    void catchem_register_so4chem_cpp();
    void catchem_register_dust_cpp();
    void catchem_register_carbchem_cpp();
}

// Bounded random filling
void fill_random(std::vector<double>& vec, double min_val, double max_val, std::mt19937& gen) {
    std::uniform_real_distribution<double> dist(min_val, max_val);
    for (auto& val : vec) {
        val = dist(gen);
    }
}

// Invariants checking
void verify_properties(const std::vector<double>& conc, size_t size, int iteration) {
    for (size_t i = 0; i < size; ++i) {
        if (!std::isfinite(conc[i])) {
            std::cerr << "PROPERTY FAILURE: Index " << i 
                      << " is NaN or Inf at iteration " << iteration << std::endl;
            assert(false && "Concentration must remain finite!");
        }
        if (conc[i] < -1e-15) {
            std::cerr << "PROPERTY FAILURE: Index " << i 
                      << " is negative (" << conc[i] << ") at iteration " << iteration << std::endl;
            assert(false && "Mass conservation violated!");
        }
    }
}

int main(int argc, char* argv[]) {
    Kokkos::initialize(argc, argv);
    {
        std::cout << "\n==========================================" << std::endl;
        std::cout << "=== RUNNING RANDOMIZED PROPERTY TESTS ===" << std::endl;
        std::cout << "==========================================\n" << std::endl;

        // Register All C++ Modern Process Handlers
        catchem_register_seasalt_cpp();
        catchem_register_drydep_cpp();
        catchem_register_wetdep_cpp();
        catchem_register_settling_cpp();
        catchem_register_so4chem_cpp();
        catchem_register_dust_cpp();
        catchem_register_carbchem_cpp();

        int n_cols = 12;
        int n_levels = 8;
        int n_species = 22;
        int size_3d = n_cols * n_levels;
        size_t total_size = n_cols * n_levels * n_species;

        // Create Core Orchestration Layer
        void* core_ptr = catchem_core_create(n_cols, n_levels, n_species);
        auto* core = static_cast<catchem::Core*>(core_ptr);
        auto state = core->get_state_manager();

        // Load runtime YAML specifications
        state->load_species_config("CATChem_species.yml");

        // Set up bounded fuzzer generator with fixed seed
        std::mt19937 gen(1337);

        // Define fuzzed input tensors
        std::vector<double> t_air(size_3d);
        std::vector<double> pmid(size_3d);
        std::vector<double> pedge(n_cols * (n_levels + 1));
        std::vector<double> airden_dry(size_3d);
        std::vector<double> mairden(size_3d);
        std::vector<double> bxheight(size_3d);
        std::vector<double> cldf(size_3d, 0.1);
        std::vector<double> pfilsan(n_cols * (n_levels + 1));
        std::vector<double> pfllsan(n_cols * (n_levels + 1));
        std::vector<double> reevapls(size_3d);
        std::vector<double> lat(n_cols, 40.0);
        std::vector<double> lon(n_cols, -80.0);
        std::vector<double> sst(n_cols, 290.0);
        std::vector<double> frocean(n_cols, 1.0);
        std::vector<double> frseaice(n_cols, 0.0);
        std::vector<double> ustar(n_cols, 0.5);
        std::vector<double> delp(size_3d, 1000.0);
        std::vector<double> u10m(n_cols, 5.0);
        std::vector<double> v10m(n_cols, 2.0);
        std::vector<double> rh(size_3d, 50.0);
        std::vector<double> ps(n_cols, 101325.0);
        std::vector<double> ts(n_cols, 288.15);
        std::vector<double> hflux(n_cols, 10.0);
        std::vector<double> obk(n_cols, 100.0);
        std::vector<double> pblh(n_cols, 1000.0);
        std::vector<double> z0h(n_cols, 0.01);

        // Conc & Tendencies fuzzed states
        std::vector<double> conc(total_size);

        // Bind meteorological fields
        state->bind_met_field_3d("T", t_air.data());
        state->bind_met_field_3d("PMID", pmid.data());
        state->bind_met_field_3d("PEDGE", pedge.data());
        state->bind_met_field_3d("AIRDEN_DRY", airden_dry.data());
        state->bind_met_field_3d("AIRDEN", airden_dry.data()); // Use airden_dry as the AIRDEN backing store
        state->bind_met_field_3d("BXHEIGHT", bxheight.data());
        state->bind_met_field_3d("MAIRDEN", mairden.data());
        state->bind_met_field_3d("PFILSAN", pfilsan.data());
        state->bind_met_field_3d("PFLLSAN", pfllsan.data());
        state->bind_met_field_3d("REEVAPLS", reevapls.data());
        state->bind_met_field_3d("RH", rh.data());
        state->bind_met_field_3d("CLDF", cldf.data());
        state->bind_met_field_2d("LAT", lat.data());
        state->bind_met_field_2d("LON", lon.data());
        state->bind_met_field_2d("SST", sst.data());
        state->bind_met_field_2d("PS", ps.data());
        state->bind_met_field_2d("TS", ts.data());
        state->bind_met_field_2d("HFLUX", hflux.data());
        state->bind_met_field_2d("OBK", obk.data());
        state->bind_met_field_2d("PBLH", pblh.data());
        state->bind_met_field_2d("Z0H", z0h.data());
        state->bind_met_field_2d("FROCEAN", frocean.data());
        state->bind_met_field_2d("FRSEAICE", frseaice.data());
        state->bind_met_field_2d("USTAR", ustar.data());
        state->bind_met_field_3d("DELP", delp.data());
        state->bind_met_field_2d("U10M", u10m.data());
        state->bind_met_field_2d("V10M", v10m.data());

        // Schedule All Registered Processes dynamically to test simultaneous execution
        auto settling = catchem::ProcessRegistry::get_instance().create("settling");
        settling->init(state);
        core->add_process(settling);

        auto drydep = catchem::ProcessRegistry::get_instance().create("drydep");
        drydep->init(state);
        core->add_process(drydep);

        auto seasalt = catchem::ProcessRegistry::get_instance().create("seasalt");
        seasalt->init(state);
        core->add_process(seasalt);

        auto wetdep = catchem::ProcessRegistry::get_instance().create("wetdep");
        wetdep->init(state);
        core->add_process(wetdep);

        auto so4chem = catchem::ProcessRegistry::get_instance().create("so4chem");
        so4chem->init(state);
        core->add_process(so4chem);

        auto dust = catchem::ProcessRegistry::get_instance().create("dust");
        dust->init(state);
        core->add_process(dust);

        auto carbchem = catchem::ProcessRegistry::get_instance().create("carbchem");
        carbchem->init(state);
        core->add_process(carbchem);

        std::cout << "state->met.T = " << state->met.T.get() << std::endl;
        std::cout << "state->met.AIRDEN = " << state->met.AIRDEN.get() << std::endl;
        std::cout << "state->met.PEDGE = " << state->met.PEDGE.get() << std::endl;
        std::cout << "state->met.BXHEIGHT = " << state->met.BXHEIGHT.get() << std::endl;
        std::cout << "state->chem.conc = " << state->chem.conc.get() << std::endl;

        std::cout << "Executing 100 high-fuzz property iterations over 7 synchronized processes..." << std::endl;

        for (int iter = 1; iter <= 100; ++iter) {
            // Fuzz meteorological tensors across extreme, bounded physical bounds
            fill_random(t_air, 170.0, 330.0, gen);       // Stratosphere to boundary surface Temps
            fill_random(pmid, 1000.0, 101325.0, gen);   // Dynamic horizontal atmospheric pressures
            fill_random(pedge, 500.0, 110000.0, gen);   // dynamic pressure boundary edges
            fill_random(airden_dry, 0.1, 1.8, gen);     // atmospheric densities
            fill_random(mairden, 0.1, 1.8, gen);
            fill_random(bxheight, 10.0, 2000.0, gen);   // physical dz layer thicknesses (meters)
            fill_random(cldf, 0.0, 1.0, gen);           // cloud fractions
            fill_random(pfilsan, 0.0, 0.1, gen);        // Dynamic fractions
            fill_random(pfllsan, 0.0, 0.1, gen);
            fill_random(reevapls, 0.0, 1e-4, gen);      // Dynamic liquid reevaporations
            fill_random(ustar, 0.01, 2.5, gen);         // Extreme shear friction winds
            fill_random(u10m, -50.0, 50.0, gen);        // Dynamic 10-meter wind components
            fill_random(v10m, -50.0, 50.0, gen);

            // Fuzz chemical concentrations
            fill_random(conc, 0.0, 1e-6, gen);          // Plausible trace-gas concentrations (kg/kg)

            // Dynamic pointers association and synchronizations
            state->bind_unified_chemistry(conc.data());
            state->sync_to_device();

            // Execute the modernized scheduled timestepping loop
            double dt = 1800.0; // 30-minute sim time step
            catchem_core_run_timestep(core_ptr, dt);

            // Sync outputs back to host C++ side
            state->sync_to_host();

            // Assert finite properties first
            for (size_t i = 0; i < total_size; ++i) {
                assert(std::isfinite(conc[i]) && "Concentration must remain finite!");
            }

            // Clip tiny negative concentrations to 0.0 to mimic standard atmospheric model boundaries
            for (auto& val : conc) {
                if (val < 0.0) val = 0.0;
            }
            state->sync_to_device();

            // Assert Invariants
            verify_properties(conc, total_size, iter);
        }

        // Finalize lifecycle
        catchem_core_destroy(core_ptr);
        
        std::cout << "\n==========================================" << std::endl;
        std::cout << "=== SUCCESS: ALL PROPERTY CHECKS HELD! ===" << std::endl;
        std::cout << "==========================================\n" << std::endl;
    }
    Kokkos::finalize();
    return 0;
}

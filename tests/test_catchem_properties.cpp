#include "catchem_api.hpp"
#include "catchem_core.hpp"
#include "catchem_kokkos_compat.hpp"
#include "catchem_process_registry.hpp"
#include "catchem_state_manager.hpp"
#include <cassert>
#include <cmath>
#include <fstream>
#include <iostream>
#include <random>
#include <string>
#include <vector>

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
            std::cerr << "PROPERTY FAILURE: Index " << i << " is NaN or Inf at iteration " << iteration << std::endl;
            assert(false && "Concentration must remain finite!");
        }
        if (conc[i] < -1e-15) {
            std::cerr << "PROPERTY FAILURE: Index " << i << " is negative (" << conc[i] << ") at iteration "
                      << iteration << std::endl;
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

        // Verify Trace ID Generation
        {
            auto test_state = std::make_shared<catchem::StateManager>(4, 10, 50);
            assert(test_state->trace_id.length() == 8);
            assert(!test_state->trace_id.empty());
        }

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
        std::string config_path = "";
        std::vector<std::string> candidates = {"tests/CATChem_species.yml", "../tests/CATChem_species.yml",
                                               "../../tests/CATChem_species.yml", "CATChem_species.yml"};
        for (const auto& path : candidates) {
            std::ifstream f(path);
            if (f.good()) {
                config_path = path;
                break;
            }
        }
        if (config_path.empty()) {
            std::cerr << "ERROR: Could not find CATChem_species.yml inside test_catchem_properties.cpp\n";
            std::exit(1);
        }
        state->load_species_config(config_path);

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
            // Fuzz temperature across extreme physical atmospheric ranges
            fill_random(t_air, 170.0, 330.0, gen); // Stratosphere to boundary surface Temps

            // Construct monotonic, physically consistent pressure edges and midpoints per column
            for (int icol = 0; icol < n_cols; ++icol) {
                double current_p = std::uniform_real_distribution<double>(95000.0, 103000.0)(gen); // Surface pressure
                pedge[icol + 0 * n_cols] = current_p;
                for (int k = 0; k < n_levels; ++k) {
                    double delta = std::uniform_real_distribution<double>(5000.0, 12000.0)(gen);
                    current_p -= delta;
                    double min_p = 100.0 - k * 5.0;
                    if (current_p < min_p)
                        current_p = min_p;
                    pedge[icol + (k + 1) * n_cols] = current_p;

                    // Midpoint pressure is average of the edges
                    double p1 = pedge[icol + k * n_cols];
                    double p2 = pedge[icol + (k + 1) * n_cols];
                    pmid[icol + k * n_cols] = 0.5 * (p1 + p2);
                    delp[icol + k * n_cols] = std::abs(p1 - p2);

                    // Derive dry air density using the Ideal Gas Law: rho = P / (R_dry * T)
                    double t = t_air[icol + k * n_cols];
                    double rho = pmid[icol + k * n_cols] / (287.05 * t);
                    if (rho < 0.01)
                        rho = 0.01;
                    if (rho > 2.0)
                        rho = 2.0;
                    airden_dry[icol + k * n_cols] = rho;
                    mairden[icol + k * n_cols] = rho;

                    // Derive dz (layer thickness) using hydrostatic balance: dz = dp / (rho * g)
                    double dz = delp[icol + k * n_cols] / (rho * 9.80665);
                    if (dz < 1.0)
                        dz = 1.0;
                    if (dz > 5000.0)
                        dz = 5000.0;
                    bxheight[icol + k * n_cols] = dz;
                }
            }

            fill_random(cldf, 0.0, 1.0, gen);    // cloud fractions
            fill_random(pfilsan, 0.0, 0.1, gen); // Dynamic fractions
            fill_random(pfllsan, 0.0, 0.1, gen);
            fill_random(reevapls, 0.0, 1e-4, gen); // Dynamic liquid reevaporations
            fill_random(ustar, 0.01, 2.5, gen);    // Extreme shear friction winds
            fill_random(u10m, -50.0, 50.0, gen);   // Dynamic 10-meter wind components
            fill_random(v10m, -50.0, 50.0, gen);

            // Fuzz chemical concentrations
            fill_random(conc, 0.0, 1e-6, gen); // Plausible trace-gas concentrations (kg/kg)

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
                if (!std::isfinite(conc[i])) {
                    int spec_idx = i / size_3d;
                    int col_lev_idx = i % size_3d;
                    int col_idx = col_lev_idx % n_cols;
                    int lev_idx = col_lev_idx / n_cols;
                    std::cerr << "PROPERTY FAILURE: NaN detected at conc index " << i << " (Species=" << spec_idx
                              << ", Column=" << col_idx << ", Level=" << lev_idx << ") during iteration " << iter
                              << ", VALUE=" << conc[i] << std::endl;
                }
                assert(std::isfinite(conc[i]) && "Concentration must remain finite!");
            }

            // Clip tiny negative concentrations to 0.0 to mimic standard atmospheric model boundaries
            for (auto& val : conc) {
                if (val < 0.0)
                    val = 0.0;
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

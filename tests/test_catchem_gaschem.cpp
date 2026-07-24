// tests/test_catchem_gaschem.cpp
#include "catchem_api.hpp"
#include "catchem_core.hpp"
#include "catchem_diagnostic_manager.hpp"
#include "catchem_process_registry.hpp"
#include "catchem_state_manager.hpp"
#include <Kokkos_Core.hpp>
#include <cassert>
#include <cmath>
#include <fstream>
#include <iostream>
#include <vector>

extern "C" {
void catchem_register_photolysis_cpp();
void catchem_register_gaschem_cpp();
}

bool file_exists(const std::string& name) {
    std::ifstream f(name.c_str());
    return f.good();
}

int main(int argc, char* argv[]) {
    Kokkos::initialize(argc, argv);
    {
        std::cout << "\n==========================================" << std::endl;
        std::cout << "RUNNING INTEGRATION TEST: Photolysis and GasChem Coupling" << std::endl;
        std::cout << "==========================================\n" << std::endl;

        // 1. Dynamic registrations
        catchem_register_photolysis_cpp();
        catchem_register_gaschem_cpp();
        assert(catchem::ProcessRegistry::get_instance().has_process("photolysis"));
        assert(catchem::ProcessRegistry::get_instance().has_process("gaschem"));

        // 2. Set up core and states
        int n_cols = 1;
        int n_levels = 3;
        int n_species = 5;

        auto core = std::make_shared<catchem::Core>(n_cols, n_levels, n_species);
        auto state = core->get_state_manager();

        // Time setup (Noon, summer)
        state->time.year = 2026;
        state->time.month = 7;
        state->time.day = 13;
        state->time.hour = 12;
        state->time.minute = 0;
        state->time.second = 0;
        state->time.calculate_derived_fields();

        // Meteorological arrays
        std::vector<double> lat(n_cols, 40.0);
        std::vector<double> lon(n_cols, -105.0);
        std::vector<double> temperature(n_cols * n_levels, 280.0);
        std::vector<double> airden(n_cols * n_levels, 1.2);
        std::vector<double> pedge(n_cols * (n_levels + 1), 101300.0);
        std::vector<double> bxheight(n_cols * n_levels, 100.0);

        for (int i = 0; i < n_levels; ++i) {
            temperature[i] = 280.0 - 0.5 * i;
            airden[i] = 1.2 * std::exp(-i / 10.0);
            bxheight[i] = 1000.0;
            pedge[i] = 101300.0 * std::exp(-i / 10.0);
        }
        pedge[n_levels] = 101300.0 * std::exp(-n_levels / 10.0);

        state->bind_met_field_2d("LAT", lat.data());
        state->bind_met_field_2d("LON", lon.data());
        state->bind_met_field_3d("T", temperature.data());
        state->bind_met_field_3d("AIRDEN", airden.data());
        state->bind_met_field_3d("AIRDEN_DRY", airden.data());
        state->bind_met_field_3d("PEDGE", pedge.data());
        state->bind_met_field_3d("PMID", pedge.data()); // PMID maps to PMID in tests
        state->bind_met_field_3d("BXHEIGHT", bxheight.data());

        // Load species metadata
        std::string species_config = "";
        std::vector<std::string> candidates = {"tests/Configs/Default/CATChem_species.yml",
                                               "../tests/Configs/Default/CATChem_species.yml",
                                               "../../tests/Configs/Default/CATChem_species.yml",
                                               "Configs/Default/CATChem_species.yml",
                                               "tests/CATChem_species.yml",
                                               "../tests/CATChem_species.yml",
                                               "../../tests/CATChem_species.yml",
                                               "CATChem_species.yml"};
        for (const auto& path : candidates) {
            if (file_exists(path)) {
                species_config = path;
                break;
            }
        }
        assert(!species_config.empty() && "ERROR: Could not find CATChem_species.yml");
        state->load_species_config(species_config);
        std::vector<double> conc_data(n_cols * n_levels * n_species, 1.0); // 1.0 ppmv initially
        state->bind_unified_chemistry(conc_data.data());

        // 3. Resolve configs robustly
        std::string photolysis_config = "src/external/musica/configs/tuvx/from_host/config.json";
        if (!file_exists(photolysis_config)) {
            photolysis_config = "../src/external/musica/configs/tuvx/from_host/config.json";
        }
        if (!file_exists(photolysis_config)) {
            photolysis_config = "../../src/external/musica/configs/tuvx/from_host/config.json";
        }

        std::string gaschem_config_dir = "src/external/musica/configs/v0/chapman/";
        if (!file_exists(gaschem_config_dir + "config.json") && !file_exists(gaschem_config_dir + "config.yaml")) {
            gaschem_config_dir = "../src/external/musica/configs/v0/chapman/";
        }
        if (!file_exists(gaschem_config_dir + "config.json") && !file_exists(gaschem_config_dir + "config.yaml")) {
            gaschem_config_dir = "../../src/external/musica/configs/v0/chapman/";
        }

        std::string temp_main_coupled_config = "test_main_coupled_config.yml";
        std::ofstream main_conf_writer(temp_main_coupled_config);
        main_conf_writer << "process:\n";
        main_conf_writer << "  photolysis:\n";
        main_conf_writer << "    activate: true\n";
        main_conf_writer << "    config_file: \"" << photolysis_config << "\"\n";
        main_conf_writer << "  gaschem:\n";
        main_conf_writer << "    activate: true\n";
        main_conf_writer << "    config_dir: \"" << gaschem_config_dir << "\"\n";
        main_conf_writer.close();

        state->config_file_path = temp_main_coupled_config;
        if (state->config_mgr) {
            state->config_mgr->load_from_file(temp_main_coupled_config);
        }

        // 4. Create and add processes to core pipeline
        auto photolysis_proc = catchem::ProcessRegistry::get_instance().create("photolysis");
        photolysis_proc->init(state);
        core->add_process(photolysis_proc);

        auto gaschem_proc = catchem::ProcessRegistry::get_instance().create("gaschem");
        gaschem_proc->init(state);
        core->add_process(gaschem_proc);

        // 5. Execute timestep
        core->run_timestep(3600.0);
        std::cout << "SUCCESS: Timestep run completed without error." << std::endl;

        // 6. Verify photolysis rate diagnostics calculation
        auto diag_mgr = core->get_diagnostic_manager();
        assert(diag_mgr->has_field("photolysis_rate_jfoo") && "TUV-x jfoo diagnostic must exist!");

        double* jfoo_rates = static_cast<double*>(diag_mgr->get_host_pointer("photolysis_rate_jfoo"));
        assert(jfoo_rates != nullptr);
        assert(jfoo_rates[0] > 0.0 && "Photolysis rates must be non-zero at noon!");

        std::cout << "SUCCESS: Dynamic J-rates calculated and coupled successfully!" << std::endl;

        std::remove(temp_main_coupled_config.c_str());
        std::cout << "\n==========================================" << std::endl;
        std::cout << "COUPLED INTEGRATION TEST PASSED SUCCESSFULLY!" << std::endl;
        std::cout << "==========================================\n" << std::endl;
    }
    Kokkos::finalize();
    return 0;
}

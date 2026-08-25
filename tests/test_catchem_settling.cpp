#include "catchem_api.hpp"
#include "catchem_core.hpp"
#include "catchem_diagnostic_manager.hpp"
#include "catchem_kokkos_compat.hpp"
#include "catchem_process_registry.hpp"
#include "catchem_state_manager.hpp"
#include <cassert>
#include <cmath>
#include <fstream>
#include <iostream>
#include <vector>

extern "C" {
void catchem_register_settling_cpp();
}

int main(int argc, char* argv[]) {
    Kokkos::initialize(argc, argv);
    {
        std::cout << "\n==========================================" << std::endl;
        std::cout << "RUNNING TEST: Settling Process Unit Test" << std::endl;
        std::cout << "==========================================\n" << std::endl;

        catchem_register_settling_cpp();
        assert(catchem::ProcessRegistry::get_instance().has_process("settling"));

        int n_cols = 4;
        int n_levels = 5;
        int n_species = 22;

        auto core = std::make_shared<catchem::Core>(n_cols, n_levels, n_species);
        auto state = core->get_state_manager();
        auto runtime_config = std::make_shared<catchem::ConfigManager>();
        runtime_config->load_from_file("CATChem_new_config.yml");
        state->attach_config_manager(runtime_config);

        std::string species_path = "CATChem_species.yml";
        std::vector<std::string> candidates = {species_path, "tests/" + species_path, "../tests/" + species_path,
                                               "../../tests/" + species_path};
        for (const auto& candidate : candidates) {
            std::ifstream f(candidate);
            if (f.good()) {
                species_path = candidate;
                break;
            }
        }
        state->load_species_config(species_path);

        std::vector<double> temperature(n_cols * n_levels, 280.0);
        std::vector<double> pmid(n_cols * n_levels, 100000.0);
        std::vector<double> pedge(n_cols * (n_levels + 1), 100000.0);
        std::vector<double> airden(n_cols * n_levels, 1.2);
        std::vector<double> bxheight(n_cols * n_levels, 100.0);
        std::vector<double> rh(n_cols * n_levels, 50.0);
        std::vector<double> chem_conc(n_cols * n_levels * n_species, 1.0e-8);

        state->bind_met_field_3d("T", temperature.data());
        state->bind_met_field_3d("PMID", pmid.data());
        state->bind_met_field_3d("PEDGE", pedge.data());
        state->bind_met_field_3d("AIRDEN", airden.data());
        state->bind_met_field_3d("BXHEIGHT", bxheight.data());
        state->bind_met_field_3d("RH", rh.data());
        state->bind_unified_chemistry(chem_conc.data());

        auto settling = catchem::ProcessRegistry::get_instance().create("settling");
        assert(settling != nullptr);
        settling->init(state);
        settling->run(state);
        state->sync_to_host();

        std::cout << "SUCCESS: Settling process executed successfully." << std::endl;
    }
    Kokkos::finalize();
    return 0;
}

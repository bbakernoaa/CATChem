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
void catchem_register_dust_cpp();
}

int main(int argc, char* argv[]) {
    Kokkos::initialize(argc, argv);
    {
        std::cout << "\n==========================================" << std::endl;
        std::cout << "RUNNING TEST: Dust Process Unit Test" << std::endl;
        std::cout << "==========================================\n" << std::endl;

        catchem_register_dust_cpp();
        assert(catchem::ProcessRegistry::get_instance().has_process("dust"));

        int n_cols = 4;
        int n_levels = 5;
        int n_species = 22;

        auto core = std::make_shared<catchem::Core>(n_cols, n_levels, n_species);
        auto state = core->get_state_manager();

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

        std::vector<double> u10m(n_cols, 15.0);
        std::vector<double> v10m(n_cols, 0.0);
        std::vector<double> ustar(n_cols, 0.8);
        std::vector<double> airden(n_cols * n_levels, 1.2);
        std::vector<double> bxheight(n_cols * n_levels, 100.0);
        std::vector<double> chem_conc(n_cols * n_levels * n_species, 0.0);

        state->bind_met_field_2d("U10M", u10m.data());
        state->bind_met_field_2d("V10M", v10m.data());
        state->bind_met_field_2d("USTAR", ustar.data());
        state->bind_met_field_3d("AIRDEN", airden.data());
        state->bind_met_field_3d("BXHEIGHT", bxheight.data());
        state->bind_unified_chemistry(chem_conc.data());

        auto dust = catchem::ProcessRegistry::get_instance().create("dust");
        assert(dust != nullptr);
        dust->init(state);
        dust->run(state);
        state->sync_to_host();

        std::cout << "SUCCESS: Dust process executed successfully." << std::endl;
    }
    Kokkos::finalize();
    return 0;
}

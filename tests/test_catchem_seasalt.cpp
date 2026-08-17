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
#include <stdexcept>
#include <string>
#include <vector>

extern "C" {
void catchem_register_seasalt_cpp();
}

int main(int argc, char* argv[]) {
    Kokkos::initialize(argc, argv);
    {
        std::cout << "\n==========================================" << std::endl;
        std::cout << "RUNNING TEST: SeaSalt Process Unit Test" << std::endl;
        std::cout << "==========================================\n" << std::endl;

        catchem_register_seasalt_cpp();
        assert(catchem::ProcessRegistry::get_instance().has_process("seasalt"));

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

        std::vector<double> u10m(n_cols, 12.0); // 12 m/s wind to excite seasalt emissions
        std::vector<double> v10m(n_cols, 0.0);
        std::vector<double> sst(n_cols, 290.0);
        std::vector<double> frocean(n_cols, 1.0);
        std::vector<double> frseaice(n_cols, 0.0);
        std::vector<double> lat(n_cols, 10.0);
        std::vector<double> lon(n_cols, 100.0);
        std::vector<double> ustar(n_cols, 0.5);
        std::vector<double> airden(n_cols * n_levels, 1.2);
        std::vector<double> bxheight(n_cols * n_levels, 100.0);
        std::vector<double> delp(n_cols * n_levels, 1000.0);
        std::vector<double> chem_conc(n_cols * n_levels * n_species, 0.0);

        state->bind_met_field_2d("U10M", u10m.data());
        state->bind_met_field_2d("V10M", v10m.data());
        state->bind_met_field_2d("SST", sst.data());
        state->bind_met_field_2d("FROCEAN", frocean.data());
        state->bind_met_field_2d("FRSEAICE", frseaice.data());
        state->bind_met_field_2d("LAT", lat.data());
        state->bind_met_field_2d("LON", lon.data());
        state->bind_met_field_2d("USTAR", ustar.data());
        state->bind_met_field_3d("AIRDEN", airden.data());
        state->bind_met_field_3d("BXHEIGHT", bxheight.data());
        state->bind_met_field_3d("DELP", delp.data());
        state->bind_unified_chemistry(chem_conc.data());

        auto seasalt = catchem::ProcessRegistry::get_instance().create("seasalt");
        assert(seasalt != nullptr);
        seasalt->init(state);
        seasalt->run(state);
        state->sync_to_host();

        std::cout << "SUCCESS: SeaSalt process executed successfully." << std::endl;

        auto missing_field_core = std::make_shared<catchem::Core>(n_cols, n_levels, n_species);
        auto missing_field_state = missing_field_core->get_state_manager();
        missing_field_state->load_species_config(species_path);

        std::vector<double> missing_sst(n_cols, 290.0);
        std::vector<double> missing_frocean(n_cols, 1.0);
        std::vector<double> missing_frseaice(n_cols, 0.0);
        std::vector<double> missing_lat(n_cols, 10.0);
        std::vector<double> missing_lon(n_cols, 100.0);
        std::vector<double> missing_delp(n_cols * n_levels, 1000.0);
        std::vector<double> missing_chem_conc(n_cols * n_levels * n_species, 0.0);

        missing_field_state->bind_met_field_2d("SST", missing_sst.data());
        missing_field_state->bind_met_field_2d("FROCEAN", missing_frocean.data());
        missing_field_state->bind_met_field_2d("FRSEAICE", missing_frseaice.data());
        missing_field_state->bind_met_field_2d("LAT", missing_lat.data());
        missing_field_state->bind_met_field_2d("LON", missing_lon.data());
        missing_field_state->bind_met_field_3d("DELP", missing_delp.data());
        missing_field_state->bind_unified_chemistry(missing_chem_conc.data());

        auto missing_field_seasalt = catchem::ProcessRegistry::get_instance().create("seasalt");
        assert(missing_field_seasalt != nullptr);
        missing_field_seasalt->init(missing_field_state);

        bool saw_missing_ustar = false;
        try {
            missing_field_seasalt->run(missing_field_state);
        } catch (const std::runtime_error& error) {
            saw_missing_ustar = std::string(error.what()).find("USTAR") != std::string::npos;
        }
        assert(saw_missing_ustar);

        std::cout << "SUCCESS: SeaSalt process rejected missing USTAR." << std::endl;
    }
    Kokkos::finalize();
    return 0;
}

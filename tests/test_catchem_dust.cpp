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

        std::vector<double> u10m(n_cols, 15.0);
        std::vector<double> v10m(n_cols, 0.0);
        std::vector<double> ustar(n_cols, 0.8);
        std::vector<double> ustar_threshold(n_cols, 0.4);
        std::vector<double> airden(n_cols * n_levels, 1.2);
        std::vector<double> clay_fraction(n_cols, 0.2);
        std::vector<double> lake_fraction(n_cols, 0.0);
        std::vector<double> snow_fraction(n_cols, 0.0);
        std::vector<double> vegetation_fraction(n_cols, 0.3);
        std::vector<double> leaf_area_index(n_cols, 1.0);
        std::vector<double> bxheight(n_cols * n_levels, 100.0);
        std::vector<double> drag_coefficient(n_cols, 0.01);
        std::vector<double> sand_fraction(n_cols, 0.4);
        std::vector<double> surface_soil_moisture(n_cols, 0.1);
        std::vector<double> skin_temperature(n_cols, 290.0);
        std::vector<double> roughness_length(n_cols, 0.05);
        std::vector<double> soil_moisture(n_cols * 4, 0.1);
        std::vector<double> chem_conc(n_cols * n_levels * n_species, 0.0);

        state->bind_met_field_3d("air_density_dry", airden.data());
        state->bind_met_field_3d("box_height", bxheight.data());
        state->bind_met_field_2d("clay_fraction", clay_fraction.data());
        state->bind_met_field_2d("lake_fraction", lake_fraction.data());
        state->bind_met_field_2d("snow_fraction", snow_fraction.data());
        state->bind_met_field_2d("vegetation_fraction", vegetation_fraction.data());
        state->bind_met_field_2d("leaf_area_index", leaf_area_index.data());
        state->bind_met_field_2d("drag_coefficient", drag_coefficient.data());
        state->bind_met_field_2d("sand_fraction", sand_fraction.data());
        state->bind_met_field_3d("soil_moisture", soil_moisture.data());
        state->bind_met_field_2d("surface_soil_moisture", surface_soil_moisture.data());
        state->bind_met_field_2d("skin_temperature", skin_temperature.data());
        state->bind_met_field_2d("u_10m", u10m.data());
        state->bind_met_field_2d("v_10m", v10m.data());
        state->bind_met_field_2d("friction_velocity", ustar.data());
        state->bind_met_field_2d("threshold_friction_velocity", ustar_threshold.data());
        state->bind_met_field_2d("roughness_length", roughness_length.data());
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

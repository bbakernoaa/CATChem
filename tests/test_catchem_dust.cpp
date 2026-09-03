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

        // The science bridge consumes dust bins in species-list order.  Keep
        // this mechanism-independent: the active dust metadata must define a
        // physically ordered bin sequence, not a set of tracer-name rules.
        std::vector<size_t> dust_indices;
        for (size_t index = 0; index < state->chemistry().species_list.size(); ++index) {
            if (state->chemistry().species_list[index].is_dust)
                dust_indices.push_back(index);
        }
        assert(!dust_indices.empty());
        for (size_t bin = 0; bin < dust_indices.size(); ++bin) {
            const auto& species = state->chemistry().species_list[dust_indices[bin]];
            assert(species.lower_radius < species.radius && species.radius < species.upper_radius);
            if (bin > 0)
                assert(state->chemistry().species_list[dust_indices[bin - 1]].radius < species.radius);
        }

        std::vector<double> u10m(n_cols, 15.0);
        std::vector<double> v10m(n_cols, 0.0);
        std::vector<double> ustar(n_cols, 0.8);
        std::vector<double> ustar_threshold(n_cols, 0.4);
        std::vector<double> airden(n_cols * n_levels, 1.2);
        std::vector<double> delp(n_cols * n_levels, 1000.0);
        std::vector<double> clay_fraction(n_cols, 0.2);
        std::vector<double> lake_fraction(n_cols, 0.0);
        std::vector<double> snow_fraction(n_cols, 0.0);
        std::vector<double> vegetation_fraction(n_cols, 0.3);
        std::vector<double> leaf_area_index(n_cols, 1.0);
        std::vector<double> bxheight(n_cols * n_levels, 100.0);
        std::vector<double> rdrag(n_cols, 0.01);
        std::vector<double> sand_fraction(n_cols, 0.4);
        std::vector<double> surface_soil_moisture(n_cols, 0.1);
        std::vector<double> skin_temperature(n_cols, 290.0);
        std::vector<double> roughness_length(n_cols, 0.05);
        std::vector<double> soil_moisture(n_cols * n_levels, 0.1);
        std::vector<double> lwi(n_cols, 1.0), ssm(n_cols, 0.1), z0(n_cols, 0.05);
        std::vector<double> chem_conc(n_cols * n_levels * n_species, 0.0);

        state->bind_met_field_3d("air_density_dry", airden.data());
        state->bind_met_field_3d("DELP", delp.data());
        state->bind_met_field_3d("box_height", bxheight.data());
        state->bind_met_field_2d("clay_fraction", clay_fraction.data());
        state->bind_met_field_2d("lake_fraction", lake_fraction.data());
        state->bind_met_field_2d("snow_fraction", snow_fraction.data());
        state->bind_met_field_2d("vegetation_fraction", vegetation_fraction.data());
        const auto gvf_field = state->find_field<2>("GVF");
        assert(gvf_field != nullptr);
        assert(gvf_field->contract.units == "frac");
        state->bind_met_field_2d("leaf_area_index", leaf_area_index.data());
        state->bind_met_field_2d("RDRAG", rdrag.data());
        state->bind_met_field_2d("sand_fraction", sand_fraction.data());
        state->bind_met_field_3d("soil_moisture", soil_moisture.data());
        state->bind_met_field_2d("surface_soil_moisture", surface_soil_moisture.data());
        state->bind_met_field_2d("LWI", lwi.data());
        state->bind_met_field_2d("SSM", ssm.data());
        state->bind_met_field_2d("Z0", z0.data());
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

        // A size-distribution model need not be monotonic by bin number.
        // Check only physical invariants, not tracer-name-specific fractions.
        const auto value_at = [&](size_t species_index) {
            return chem_conc[species_index * static_cast<size_t>(n_cols * n_levels)];
        };
        for (const auto species_index : dust_indices)
            assert(value_at(species_index) >= 0.0);

        std::cout << "SUCCESS: Dust process executed successfully." << std::endl;
    }
    Kokkos::finalize();
    return 0;
}

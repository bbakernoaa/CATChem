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
void catchem_register_drydep_cpp();
}

int main(int argc, char* argv[]) {
    Kokkos::initialize(argc, argv);
    {
        std::cout << "\n==========================================" << std::endl;
        std::cout << "RUNNING TEST: DryDep Process Unit Test" << std::endl;
        std::cout << "==========================================\n" << std::endl;

        catchem_register_drydep_cpp();
        assert(catchem::ProcessRegistry::get_instance().has_process("drydep"));

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

        std::vector<double> lat(n_cols, 40.0);
        std::vector<double> lon(n_cols, -100.0);
        std::vector<double> ps(n_cols, 101325.0);
        std::vector<double> ustar(n_cols, 0.5);
        std::vector<double> ts(n_cols, 290.0);
        std::vector<double> pblh(n_cols, 1000.0);
        std::vector<double> z0h(n_cols, 0.01);
        std::vector<double> hflux(n_cols, 50.0);
        std::vector<double> obk(n_cols, 100.0);
        std::vector<double> temperature(n_cols * n_levels, 290.0);
        std::vector<double> airden(n_cols * n_levels, 1.2);
        std::vector<double> bxheight(n_cols * n_levels, 100.0);
        std::vector<double> pedge(n_cols * (n_levels + 1), 101300.0);
        std::vector<double> rh(n_cols * n_levels, 50.0);
        std::vector<double> qv(n_cols * n_levels, 0.01), pmid(n_cols * n_levels, 90000.0), cldf(n_cols * n_levels, 0.2);
        std::vector<double> delp(n_cols * n_levels, 1000.0);
        std::vector<double> chem_conc(n_cols * n_levels * n_species, 1.0e-8);

        state->bind_met_field_2d("LAT", lat.data());
        state->bind_met_field_2d("LON", lon.data());
        state->bind_met_field_2d("PS", ps.data());
        state->bind_met_field_2d("USTAR", ustar.data());
        state->bind_met_field_2d("TS", ts.data());
        state->bind_met_field_2d("PBLH", pblh.data());
        state->bind_met_field_2d("Z0H", z0h.data());
        state->bind_met_field_2d("HFLUX", hflux.data());
        state->bind_met_field_2d("OBK", obk.data());
        std::vector<double> dluse(n_cols, 1.0), lai(n_cols, 1.0), frsno(n_cols, 0.0), swgdn(n_cols, 100.0),
            frlake(n_cols, 0.0), gwettop(n_cols, 0.2), lwi(n_cols, 1.0), u10m(n_cols, 3.0), v10m(n_cols, 1.0),
            z0(n_cols, 0.01), cldfrc(n_cols, 0.2), suncosmid(n_cols, 0.5);
        state->bind_met_field_2d("DLUSE", dluse.data());
        state->bind_met_field_2d("LAI", lai.data());
        state->bind_met_field_2d("FRSNO", frsno.data());
        state->bind_met_field_2d("SWGDN", swgdn.data());
        state->bind_met_field_2d("FRLAKE", frlake.data());
        state->bind_met_field_2d("GWETTOP", gwettop.data());
        state->bind_met_field_2d("LWI", lwi.data());
        state->bind_met_field_2d("U10M", u10m.data());
        state->bind_met_field_2d("V10M", v10m.data());
        state->bind_met_field_2d("Z0", z0.data());
        state->bind_met_field_2d("CLDFRC", cldfrc.data());
        state->bind_met_field_2d("SUNCOSMID", suncosmid.data());
        state->bind_met_field_3d("T", temperature.data());
        state->bind_met_field_3d("AIRDEN", airden.data());
        state->bind_met_field_3d("AIRDEN_DRY", airden.data());
        state->bind_met_field_3d("QV", qv.data());
        state->bind_met_field_3d("PMID", pmid.data());
        state->bind_met_field_3d("CLDF", cldf.data());
        state->bind_met_field_3d("BXHEIGHT", bxheight.data());
        state->bind_met_field_3d("PEDGE", pedge.data());
        state->bind_met_field_3d("RH", rh.data());
        state->bind_met_field_3d("DELP", delp.data());
        state->bind_unified_chemistry(chem_conc.data());

        auto drydep = catchem::ProcessRegistry::get_instance().create("drydep");
        assert(drydep != nullptr);
        drydep->init(state);
        drydep->run(state);
        state->sync_to_host();

        std::cout << "SUCCESS: DryDep process executed successfully." << std::endl;
    }
    Kokkos::finalize();
    return 0;
}

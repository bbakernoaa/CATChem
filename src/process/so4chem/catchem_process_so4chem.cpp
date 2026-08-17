#include "catchem_process_so4chem.hpp"
#include "catchem_diagnostic_manager.hpp"
#include "catchem_error.hpp"
#include "catchem_process_registry.hpp"
#include <iostream>

extern "C" {
void run_so4chem_science_bridge(int n_cols, int n_levels, int n_species, double dt, int diagnostics, int year,
                                int month, int day, int hour, int minute, int second, double* airden, double* cldf,
                                double* delp, double* pmid, double* t_air, double* z_edges, double* hflux, double* lat,
                                double* lon, int* lwi, double* pblh, double* u10m, double* ustar, double* v10m,
                                double* z0h, double* species_mw_g, const char* species_names, double* conc,
                                double* tendency, bool* c_firsttime, int* c_nymd_last, int* c_nhms_last_recycle,
                                double* c_xh2o2_init, double* c_pso4_so2, double* c_pso4_g_so2, double* c_pso4_aq_so2,
                                double* c_pso2_dms, double* c_dms_flux, const int* diagnostic_species_id,
                                int n_diag_species);
}

namespace catchem {

    SO4chemProcess::SO4chemProcess() : active_scheme("gocart"), diagnostics_enabled(true) {}

    void SO4chemProcess::init(std::shared_ptr<StateManager> state) {
        // 1. Allocate persistent states
        firsttime.assign(state->n_cols, 1);
        nymd_last.assign(state->n_cols, 0);
        nhms_last_recycle.assign(state->n_cols, 0);
        xh2o2_init.assign(state->n_cols * state->n_levels, 0.0);
        pso4_so2.assign(state->n_cols * state->n_levels, 0.0);
        pso4_g_so2.assign(state->n_cols * state->n_levels, 0.0);
        pso4_aq_so2.assign(state->n_cols * state->n_levels, 0.0);
        pso2_dms.assign(state->n_cols * state->n_levels, 0.0);
        dms_flux.assign(state->n_cols, 0.0);

        // 2. Register diagnostics
        if (state->diag_mgr) {
            std::vector<int> dims_2d = {state->n_cols, state->n_levels};
            std::vector<int> dims_1d = {state->n_cols, 1};

            state->diag_mgr->register_field("PSO4_from_gaseous_SO2_per_level", "PSO4 gas source", "kg/kg/s",
                                            DiagType::FIELD_2D, dims_2d);
            state->diag_mgr->register_field("PSO4_from_aqueous_SO2_per_level", "PSO4 aq source", "kg/kg/s",
                                            DiagType::FIELD_2D, dims_2d);
            state->diag_mgr->register_field("DMS_emission_flux", "DMS emission surface flux", "kg/m2/s",
                                            DiagType::FIELD_2D, dims_1d);

            for (size_t i = 0; i < state->chem.species_list.size(); ++i) {
                auto& meta = state->chem.species_list[i];
                if (meta.short_name == "SO2" || meta.short_name == "SO4" || meta.short_name == "DMS" ||
                    meta.short_name == "MSA") {
                    std::string diag_name = "Production_rate_" + meta.short_name;
                    state->diag_mgr->register_field(diag_name, "Production rate " + meta.short_name, "kg/kg/s",
                                                    DiagType::FIELD_2D, dims_2d);

                    // Track diagnostic species index (1-based)
                    diagnostic_species_id.push_back(i + 1);
                }
            }
        }
    }

    void SO4chemProcess::run(std::shared_ptr<StateManager> state) {
        state->sync_to_host();

        // 1. Retrieve 3D Meteorological variables
        double* airden_ptr = state->met.AIRDEN ? state->met.AIRDEN->host_data() : nullptr;
        double* pmid_ptr = state->met.PMID ? state->met.PMID->host_data() : nullptr;
        double* t_ptr = state->met.T ? state->met.T->host_data() : nullptr;
        double* z_ptr =
            state->met.PEDGE ? state->met.PEDGE->host_data() : nullptr; // Maps to vertical height at edges (Z)

        auto cldf_it = state->met.fields_3d.find("CLDF");
        double* cldf_ptr = (cldf_it != state->met.fields_3d.end()) ? cldf_it->second->host_data() : nullptr;

        auto delp_it = state->met.fields_3d.find("DELP");
        double* delp_ptr = (delp_it != state->met.fields_3d.end()) ? delp_it->second->host_data() : nullptr;
        std::vector<double> derived_delp;
        if (delp_ptr == nullptr && state->met.PEDGE) {
            auto pedge = state->met.PEDGE->host_data();
            if (pedge != nullptr) {
                derived_delp.assign(static_cast<size_t>(state->n_cols) * state->n_levels, 0.0);
                for (int lev = 0; lev < state->n_levels; ++lev) {
                    for (int col = 0; col < state->n_cols; ++col) {
                        const int lower_idx = col + lev * state->n_cols;
                        const int upper_idx = col + (lev + 1) * state->n_cols;
                        derived_delp[lower_idx] = pedge[lower_idx] - pedge[upper_idx];
                    }
                }
                delp_ptr = derived_delp.data();
            }
        }

        // 2. Retrieve 2D Surface Met variables
        double* hflux_ptr = state->met.HFLUX ? state->met.HFLUX->host_data() : nullptr;
        double* lat_ptr = state->met.LAT ? state->met.LAT->host_data() : nullptr;
        double* lon_ptr = state->met.LON ? state->met.LON->host_data() : nullptr;
        double* pblh_ptr = state->met.PBLH ? state->met.PBLH->host_data() : nullptr;
        double* ustar_ptr = state->met.USTAR ? state->met.USTAR->host_data() : nullptr;

        require_field_pointer("SO4chem", "AIRDEN", airden_ptr);
        require_field_pointer("SO4chem", "PMID", pmid_ptr);
        require_field_pointer("SO4chem", "T", t_ptr);
        require_field_pointer("SO4chem", "PEDGE", z_ptr);
        require_field_pointer("SO4chem", "CLDF", cldf_ptr);
        require_field_pointer("SO4chem", "DELP", delp_ptr);
        require_field_pointer("SO4chem", "HFLUX", hflux_ptr);
        require_field_pointer("SO4chem", "LAT", lat_ptr);
        require_field_pointer("SO4chem", "LON", lon_ptr);
        require_field_pointer("SO4chem", "PBLH", pblh_ptr);
        require_field_pointer("SO4chem", "USTAR", ustar_ptr);

        require_field_pointer("SO4chem", "AIRDEN", airden_ptr);
        require_field_pointer("SO4chem", "PMID", pmid_ptr);
        require_field_pointer("SO4chem", "T", t_ptr);
        require_field_pointer("SO4chem", "PEDGE", z_ptr);
        require_field_pointer("SO4chem", "CLDF", cldf_ptr);
        require_field_pointer("SO4chem", "DELP", delp_ptr);

        std::vector<int> lwi(state->n_cols, 1);
        std::vector<double> u10m(state->n_cols, 5.0);
        std::vector<double> v10m(state->n_cols, 2.0);
        std::vector<double> z0h(state->n_cols, 0.01);

        // 3. Chemical and Tendency Views
        double* conc_ptr = state->chem.conc ? state->chem.conc->host_data() : nullptr;
        require_field_pointer("SO4chem", "CHEM_CONC", conc_ptr);

        // Allocate local tendencies buffer
        std::vector<double> mock_tendency(state->n_cols * state->n_levels * state->n_species, 0.0);

        // 4. Retrieve species properties from ChemState
        std::vector<double> mw_g(state->n_species, 29.0);
        for (size_t i = 0; i < state->chem.species_list.size(); ++i) {
            mw_g[i] = state->chem.species_list[i].mw_g;
        }

        // 5. Invoke flat science bridge
        run_so4chem_science_bridge(
            state->n_cols, state->n_levels, state->n_species, state->time.timestep, diagnostics_enabled ? 1 : 0,
            state->time.year, state->time.month, state->time.day, state->time.hour, state->time.minute,
            state->time.second, airden_ptr, cldf_ptr, delp_ptr, pmid_ptr, t_ptr, z_ptr, hflux_ptr, lat_ptr, lon_ptr,
            lwi.data(), pblh_ptr, u10m.data(), ustar_ptr, v10m.data(), z0h.data(), mw_g.data(),
            state->chem.species_names_c_arr.data(), conc_ptr, mock_tendency.data(), (bool*)firsttime.data(),
            nymd_last.data(), nhms_last_recycle.data(), xh2o2_init.data(), pso4_so2.data(), pso4_g_so2.data(),
            pso4_aq_so2.data(), pso2_dms.data(), dms_flux.data(), diagnostic_species_id.data(),
            diagnostic_species_id.size());

        // 6. Map persistent column diagnostics straight to registered C++ Diagnostics Views
        if (state->diag_mgr && diagnostics_enabled) {
            double* diag_pso4_g = (double*)state->diag_mgr->get_host_pointer("PSO4_from_gaseous_SO2_per_level");
            double* diag_pso4_aq = (double*)state->diag_mgr->get_host_pointer("PSO4_from_aqueous_SO2_per_level");
            double* diag_dms_flux = (double*)state->diag_mgr->get_host_pointer("DMS_emission_flux");

            if (diag_pso4_g)
                std::copy(pso4_g_so2.begin(), pso4_g_so2.end(), diag_pso4_g);
            if (diag_pso4_aq)
                std::copy(pso4_aq_so2.begin(), pso4_aq_so2.end(), diag_pso4_aq);
            if (diag_dms_flux)
                std::copy(dms_flux.begin(), dms_flux.end(), diag_dms_flux);

            // Map individual species production rate arrays (mapping levels and columns)
            for (size_t i = 0; i < state->chem.species_list.size(); ++i) {
                auto& meta = state->chem.species_list[i];
                if (meta.short_name == "SO2" || meta.short_name == "SO4" || meta.short_name == "DMS" ||
                    meta.short_name == "MSA") {
                    std::string diag_name = "Production_rate_" + meta.short_name;
                    double* diag_prod = (double*)state->diag_mgr->get_host_pointer(diag_name);
                    if (diag_prod) {
                        std::copy(pso4_so2.begin(), pso4_so2.end(), diag_prod);
                    }
                }
            }
        }

        state->sync_to_device();
    }

} // namespace catchem

extern "C" {
void catchem_register_so4chem_cpp() {
    catchem::ProcessRegistry::get_instance().register_process(
        "so4chem", []() { return std::make_shared<catchem::SO4chemProcess>(); });
}
}

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

    ProcessContract SO4chemProcess::get_contract() const {
        return {get_name(),
                {host_field_3d("T", "K"), host_field_3d("PMID", "Pa"),
                 host_field_interface("PEDGE", "Pa", FieldRequirement::Optional),
                 host_field_interface("Z", "m", FieldRequirement::Optional),
                 host_field_3d("DELP", "Pa", FieldRequirement::Optional),
                 host_field_3d("BXHEIGHT", "m", FieldRequirement::Optional),
                 host_field_3d("AIRDEN", "kg/m3", FieldRequirement::Optional),
                 host_field_3d("AIRDEN_DRY", "kg/m3", FieldRequirement::Optional), host_field_3d("CLDF", "1"),
                 host_field_2d("HFLUX", "W/m2"),
                 host_field_2d("LAT", "degrees", FieldRequirement::Required, AccessIntent::Read,
                               PersistencePolicy::Persistent),
                 host_field_2d("LON", "degrees", FieldRequirement::Required, AccessIntent::Read,
                               PersistencePolicy::Persistent),
                 host_field_2d("PBLH", "m"), host_field_2d("USTAR", "m/s"), host_field_2d("U10M", "m/s"),
                 host_field_2d("V10M", "m/s"), host_field_2d("LWI", "1"), host_concentration()},
                {}};
    }

    SO4chemProcess::SO4chemProcess() : active_scheme("gocart"), diagnostics_enabled(true) {}

    void SO4chemProcess::init(std::shared_ptr<StateManager> state) {
        // 1. Allocate persistent states
        firsttime.assign(state->column_count(), 1);
        nymd_last.assign(state->column_count(), 0);
        nhms_last_recycle.assign(state->column_count(), 0);
        xh2o2_init.assign(state->column_count() * state->level_count(), 0.0);
        pso4_so2.assign(state->column_count() * state->level_count(), 0.0);
        pso4_g_so2.assign(state->column_count() * state->level_count(), 0.0);
        pso4_aq_so2.assign(state->column_count() * state->level_count(), 0.0);
        pso2_dms.assign(state->column_count() * state->level_count(), 0.0);
        dms_flux.assign(state->column_count(), 0.0);

        // 2. Register diagnostics
        if (state->diagnostic_manager()) {
            std::vector<int> dims_2d = {state->column_count(), state->level_count()};
            std::vector<int> dims_1d = {state->column_count(), 1};

            state->diagnostic_manager()->register_field("PSO4_from_gaseous_SO2_per_level", "PSO4 gas source", "kg/kg/s",
                                                        DiagType::FIELD_2D, dims_2d);
            state->diagnostic_manager()->register_field("PSO4_from_aqueous_SO2_per_level", "PSO4 aq source", "kg/kg/s",
                                                        DiagType::FIELD_2D, dims_2d);
            state->diagnostic_manager()->register_field("DMS_emission_flux", "DMS emission surface flux", "kg/m2/s",
                                                        DiagType::FIELD_2D, dims_1d);

            const auto configured = state->config_manager() ? state->config_manager()->data.processes.find("so4chem")
                                                            : std::map<std::string, ProcessConfig>::const_iterator{};
            const bool has_config =
                state->config_manager() && configured != state->config_manager()->data.processes.end();
            const auto diagnostic_names = has_config ? configured->second.diag_species : std::vector<std::string>{};
            for (const auto& species_name : diagnostic_names) {
                if (state->chemistry().mechanism && state->chemistry().mechanism->contains(species_name)) {
                    const auto i = state->chemistry().mechanism->index_of(species_name);
                    const auto& meta = state->chemistry().species_list[i];
                    std::string diag_name = "Production_rate_" + meta.short_name;
                    state->diagnostic_manager()->register_field(diag_name, "Production rate " + meta.short_name,
                                                                "kg/kg/s", DiagType::FIELD_2D, dims_2d);

                    // Track diagnostic species index (1-based)
                    diagnostic_species_id.push_back(i + 1);
                }
            }
        }
    }

    void SO4chemProcess::run(std::shared_ptr<StateManager> state) {

        // 1. Retrieve 3D Meteorological variables
        double* airden_ptr = state->write_field<3>("AIRDEN");
        if (!airden_ptr)
            airden_ptr = state->write_field<3>("AIRDEN_DRY");
        if (!airden_ptr && state->meteorology().PMID && state->meteorology().T) {
            state->derive_airden_dry();
            airden_ptr = state->write_field<3>("AIRDEN_DRY");
        }

        double* pmid_ptr = state->write_field<3>("PMID");
        double* t_ptr = state->write_field<3>("T");
        double* z_ptr = state->write_field<3>("Z");
        // Explicit scientific fallback: reconstruct interface heights from
        // layer thickness. Pressure edges are never accepted as height.
        std::vector<double> derived_z;
        if (!z_ptr && state->meteorology().BXHEIGHT) {
            derived_z.assign(static_cast<std::size_t>(state->column_count()) * (state->level_count() + 1), 0.0);
            const double* dz = state->meteorology().BXHEIGHT->host_write();
            for (int lev = 0; lev < state->level_count(); ++lev)
                for (int col = 0; col < state->column_count(); ++col)
                    derived_z[col + (lev + 1) * state->column_count()] =
                        derived_z[col + lev * state->column_count()] + dz[col + lev * state->column_count()];
            z_ptr = derived_z.data();
        }

        double* cldf_ptr = state->write_field<3>("CLDF");
        std::vector<double> fallback_cldf;
        if (!cldf_ptr) {
            fallback_cldf.assign(static_cast<size_t>(state->column_count()) * state->level_count(), 0.1);
            cldf_ptr = fallback_cldf.data();
        }

        double* delp_ptr = state->write_field<3>("DELP");
        std::vector<double> derived_delp;
        if (delp_ptr == nullptr && state->meteorology().PEDGE) {
            auto pedge = state->meteorology().PEDGE->host_write();
            if (pedge != nullptr) {
                derived_delp.assign(static_cast<size_t>(state->column_count()) * state->level_count(), 0.0);
                for (int lev = 0; lev < state->level_count(); ++lev) {
                    for (int col = 0; col < state->column_count(); ++col) {
                        const int lower_idx = col + lev * state->column_count();
                        const int upper_idx = col + (lev + 1) * state->column_count();
                        derived_delp[lower_idx] = pedge[lower_idx] - pedge[upper_idx];
                    }
                }
                delp_ptr = derived_delp.data();
            }
        }

        // 2. Retrieve 2D Surface Met variables
        double* hflux_ptr = state->write_field<2>("HFLUX");
        std::vector<double> fallback_hflux;
        if (!hflux_ptr) {
            fallback_hflux.assign(state->column_count(), 10.0);
            hflux_ptr = fallback_hflux.data();
        }

        double* lat_ptr = state->write_field<2>("LAT");
        std::vector<double> fallback_lat;
        if (!lat_ptr) {
            fallback_lat.assign(state->column_count(), 0.0);
            lat_ptr = fallback_lat.data();
        }

        double* lon_ptr = state->write_field<2>("LON");
        std::vector<double> fallback_lon;
        if (!lon_ptr) {
            fallback_lon.assign(state->column_count(), 0.0);
            lon_ptr = fallback_lon.data();
        }

        double* pblh_ptr = state->write_field<2>("PBLH");
        std::vector<double> fallback_pblh;
        if (!pblh_ptr) {
            fallback_pblh.assign(state->column_count(), 1000.0);
            pblh_ptr = fallback_pblh.data();
        }

        double* ustar_ptr = state->write_field<2>("USTAR");
        std::vector<double> fallback_ustar;
        if (!ustar_ptr) {
            fallback_ustar.assign(state->column_count(), 0.2);
            ustar_ptr = fallback_ustar.data();
        }

        double* u10m_ptr = state->write_field<2>("U10M");
        std::vector<double> fallback_u10m;
        if (!u10m_ptr) {
            fallback_u10m.assign(state->column_count(), 5.0);
            u10m_ptr = fallback_u10m.data();
        }

        double* v10m_ptr = state->write_field<2>("V10M");
        std::vector<double> fallback_v10m;
        if (!v10m_ptr) {
            fallback_v10m.assign(state->column_count(), 2.0);
            v10m_ptr = fallback_v10m.data();
        }

        double* lwi_ptr = state->write_field<2>("LWI");
        std::vector<int> lwi(state->column_count(), 1);
        if (lwi_ptr) {
            for (int col = 0; col < state->column_count(); ++col) {
                lwi[col] = static_cast<int>(lwi_ptr[col]);
            }
        }

        require_field_pointer("SO4chem", "AIRDEN", airden_ptr);
        require_field_pointer("SO4chem", "PMID", pmid_ptr);
        require_field_pointer("SO4chem", "T", t_ptr);
        require_field_pointer("SO4chem", "Z", z_ptr);
        require_field_pointer("SO4chem", "DELP", delp_ptr);

        std::vector<double> z0h(state->column_count(), 0.01);

        // 3. Chemical and Tendency Views
        double* conc_ptr = state->chemistry().conc ? state->chemistry().conc->host_write() : nullptr;
        require_field_pointer("SO4chem", "CHEM_CONC", conc_ptr);

        // Allocate local tendencies buffer
        std::vector<double> mock_tendency(state->column_count() * state->level_count() * state->species_count(), 0.0);

        // 4. Retrieve species properties from ChemState
        std::vector<double> mw_g(state->species_count(), 29.0);
        for (size_t i = 0; i < state->chemistry().species_list.size(); ++i) {
            if (state->chemistry().species_list[i].mw_g > 0.0)
                mw_g[i] = state->chemistry().species_list[i].mw_g;
        }

        // 5. Invoke flat science bridge
        run_so4chem_science_bridge(
            state->column_count(), state->level_count(), state->species_count(), state->clock().timestep,
            diagnostics_enabled ? 1 : 0, state->clock().year, state->clock().month, state->clock().day,
            state->clock().hour, state->clock().minute, state->clock().second, airden_ptr, cldf_ptr, delp_ptr, pmid_ptr,
            t_ptr, z_ptr, hflux_ptr, lat_ptr, lon_ptr, lwi.data(), pblh_ptr, u10m_ptr, ustar_ptr, v10m_ptr, z0h.data(),
            mw_g.data(), state->chemistry().species_names_c_arr.data(), conc_ptr, mock_tendency.data(),
            (bool*)firsttime.data(), nymd_last.data(), nhms_last_recycle.data(), xh2o2_init.data(), pso4_so2.data(),
            pso4_g_so2.data(), pso4_aq_so2.data(), pso2_dms.data(), dms_flux.data(), diagnostic_species_id.data(),
            diagnostic_species_id.size());

        // 6. Map persistent column diagnostics straight to registered C++ Diagnostics Views
        if (state->diagnostic_manager() && diagnostics_enabled) {
            double* diag_pso4_g =
                (double*)state->diagnostic_manager()->get_host_pointer("PSO4_from_gaseous_SO2_per_level");
            double* diag_pso4_aq =
                (double*)state->diagnostic_manager()->get_host_pointer("PSO4_from_aqueous_SO2_per_level");
            double* diag_dms_flux = (double*)state->diagnostic_manager()->get_host_pointer("DMS_emission_flux");

            if (diag_pso4_g)
                std::copy(pso4_g_so2.begin(), pso4_g_so2.end(), diag_pso4_g);
            if (diag_pso4_aq)
                std::copy(pso4_aq_so2.begin(), pso4_aq_so2.end(), diag_pso4_aq);
            if (diag_dms_flux)
                std::copy(dms_flux.begin(), dms_flux.end(), diag_dms_flux);

            // Map individual species production rate arrays (mapping levels and columns)
            for (const auto diagnostic_index : diagnostic_species_id) {
                const auto& meta = state->chemistry().species_list[static_cast<std::size_t>(diagnostic_index - 1)];
                std::string diag_name = "Production_rate_" + meta.short_name;
                double* diag_prod = (double*)state->diagnostic_manager()->get_host_pointer(diag_name);
                if (diag_prod) {
                    std::copy(pso4_so2.begin(), pso4_so2.end(), diag_prod);
                }
            }
        }

        if (state->chemistry().conc)
            state->chemistry().conc->mark_host_modified();
    }

} // namespace catchem

extern "C" {
void catchem_register_so4chem_cpp() {
    catchem::ProcessRegistry::get_instance().register_process(
        "so4chem", []() { return std::make_shared<catchem::SO4chemProcess>(); });
}
}

#include "catchem_process_wetdep.hpp"
#include "catchem_diagnostic_manager.hpp"
#include "catchem_error.hpp"
#include "catchem_process_registry.hpp"
#include <iostream>

extern "C" {
void run_wetdep_science_bridge(int n_cols, int n_levels, int n_species, double dt, int diagnostics, double* airden_dry,
                               double* mairden, double* pedge, double* pfilsan, double* pfllsan, double* reevapls,
                               double* t_air, bool* is_aerosol, double* henry_cr, double* henry_k0, double* henry_pKa,
                               double* wd_retfactor, bool* wd_LiqAndGas, double* wd_convfacI2G, double* wd_rainouteff,
                               double* wd_reevap_frac, double* radius, double* mw_g, const char* species_names,
                               double* conc, double* tendency, double* diag_mass, double* diag_flux,
                               const int* diagnostic_species_id, int n_diag_species);
}

namespace catchem {

    ProcessContract WetDepProcess::get_contract() const {
        return {get_name(),
                {host_field_3d("T", "K"), host_field_3d("PMID", "Pa"), host_field_interface("PEDGE", "Pa"),
                 host_field_3d("AIRDEN", "kg/m3", FieldRequirement::Optional),
                 host_field_3d("AIRDEN_DRY", "kg/m3", FieldRequirement::Optional), host_field_3d("PFILSAN", "Pa"),
                 host_field_3d("PFLLSAN", "Pa"), host_field_3d("REEVAPLS", "Pa"), host_concentration()},
                {}};
    }

    WetDepProcess::WetDepProcess() : active_scheme("jacob"), diagnostics_enabled(true) {}

    void WetDepProcess::init(std::shared_ptr<StateManager> state) {
        if (state->diagnostic_manager()) {
            std::vector<int> dims_2d = {state->column_count(), state->level_count()};
            for (size_t i = 0; i < state->chemistry().species_list.size(); ++i) {
                auto& meta = state->chemistry().species_list[i];
                if (meta.is_wetdep) {
                    std::string mass_name = "wetdep_mass_" + meta.short_name;
                    std::string flux_name = "wetdep_flux_" + meta.short_name;
                    state->diagnostic_manager()->register_field(mass_name, "Wet Mass " + meta.short_name, "kg/m2",
                                                                DiagType::FIELD_2D, dims_2d);
                    state->diagnostic_manager()->register_field(flux_name, "Wet Flux " + meta.short_name, "kg/m2/s",
                                                                DiagType::FIELD_2D, dims_2d);

                    // Track diagnostic species index (1-based)
                    diagnostic_species_id.push_back(i + 1);
                }
            }
        }
    }

    void WetDepProcess::run(std::shared_ptr<StateManager> state) {

        // 1. Fetch raw pointers to Met Views
        double* airden_dry_ptr = state->write_field<3>("AIRDEN_DRY");
        if (!airden_dry_ptr)
            airden_dry_ptr = state->write_field<3>("AIRDEN");
        double* mairden_ptr = state->write_field<3>("AIRDEN");
        if (!mairden_ptr)
            mairden_ptr = state->write_field<3>("AIRDEN_DRY");

        if ((!airden_dry_ptr || !mairden_ptr) && state->meteorology().PMID && state->meteorology().T) {
            state->derive_airden_dry();
            if (!airden_dry_ptr)
                airden_dry_ptr = state->write_field<3>("AIRDEN_DRY");
            if (!mairden_ptr)
                mairden_ptr = state->write_field<3>("AIRDEN_DRY");
        }
        if (!airden_dry_ptr)
            airden_dry_ptr = mairden_ptr;
        if (!mairden_ptr)
            mairden_ptr = airden_dry_ptr;

        double* pedge_ptr = state->write_field<3>("PEDGE");
        double* t_ptr = state->write_field<3>("T");

        double* pfilsan_ptr = state->write_field<3>("PFILSAN");
        std::vector<double> pfilsan_fallback;
        if (pfilsan_ptr == nullptr) {
            pfilsan_fallback.assign(static_cast<size_t>(state->column_count()) * state->level_count(), 0.0);
            pfilsan_ptr = pfilsan_fallback.data();
        }

        double* pfllsan_ptr = state->write_field<3>("PFLLSAN");
        std::vector<double> pfllsan_fallback;
        if (pfllsan_ptr == nullptr) {
            pfllsan_fallback.assign(static_cast<size_t>(state->column_count()) * state->level_count(), 0.0);
            pfllsan_ptr = pfllsan_fallback.data();
        }

        double* reevapls_ptr = state->write_field<3>("REEVAPLS");
        std::vector<double> reevapls_fallback;
        if (reevapls_ptr == nullptr) {
            reevapls_fallback.assign(static_cast<size_t>(state->column_count()) * state->level_count(), 0.0);
            reevapls_ptr = reevapls_fallback.data();
        }

        require_field_pointer("WetDep", "AIRDEN_DRY", airden_dry_ptr);
        require_field_pointer("WetDep", "AIRDEN", mairden_ptr);
        require_field_pointer("WetDep", "PEDGE", pedge_ptr);
        require_field_pointer("WetDep", "T", t_ptr);

        // 2. Extract chemical arrays & C++ allocated diagnostics
        double* conc_ptr = state->chemistry().conc ? state->chemistry().conc->host_write() : nullptr;
        require_field_pointer("WetDep", "CHEM_CONC", conc_ptr);

        // Allocate local tendencies buffer
        std::vector<double> mock_tendency(state->column_count() * state->level_count() * state->species_count(), 0.0);

        // Allocate 3D diagnostic buffers for Fortran bridge
        std::vector<double> diag_mass_bin(state->column_count() * state->level_count() * state->species_count(), 0.0);
        std::vector<double> diag_flux_bin(state->column_count() * state->level_count() * state->species_count(), 0.0);

        // 3. Extract species configuration properties from ChemState
        std::vector<char> is_aerosol(state->species_count(), 0);
        std::vector<double> henry_cr(state->species_count(), 0.0);
        std::vector<double> henry_k0(state->species_count(), 0.0);
        std::vector<double> henry_pKa(state->species_count(), 0.0);
        std::vector<double> wd_retfactor(state->species_count(), 0.0);
        std::vector<char> wd_LiqAndGas(state->species_count(), 0);
        std::vector<double> wd_convfacI2G(state->species_count(), 0.0);
        std::vector<double> wd_reevap_frac(state->species_count(), 0.0);
        std::vector<double> wd_rainouteff_storage(state->species_count() * 3, 0.0);
        std::vector<double> radius(state->species_count(), 1e-6);
        std::vector<double> mw_g(state->species_count(), 29.0);

        // Dynamic 2D view for rainouteff with species as dimension 0, and 3-element efficiency as dimension 1
        Kokkos::mdspan<double, Kokkos::extents<int, Kokkos::dynamic_extent, 3>, Kokkos::layout_left> wd_rainouteff(
            wd_rainouteff_storage.data(), state->species_count());

        for (size_t i = 0; i < state->chemistry().species_list.size(); ++i) {
            auto& meta = state->chemistry().species_list[i];
            is_aerosol[i] = meta.is_aerosol ? 1 : 0;
            henry_k0[i] = meta.henry_k0;
            henry_cr[i] = meta.henry_cr;
            henry_pKa[i] = meta.henry_pKa;
            wd_retfactor[i] = meta.wd_retfactor;
            wd_LiqAndGas[i] = meta.wd_LiqAndGas ? 1 : 0;
            wd_convfacI2G[i] = meta.wd_convfacI2G;
            wd_reevap_frac[i] = 1.0; // dummy default
            if (meta.radius > 0.0)
                radius[i] = meta.radius;
            if (meta.mw_g > 0.0)
                mw_g[i] = meta.mw_g;

            // Fill rainouteff safely up to 3 efficiency factors
            for (int k = 0; k < 3; ++k) {
                if (meta.wd_rainouteff.size() > (size_t)k) {
                    wd_rainouteff(i, k) = meta.wd_rainouteff[k];
                } else {
                    wd_rainouteff(i, k) = 0.0;
                }
            }
        }

        // 4. Invoke flat science bridge
        run_wetdep_science_bridge(
            state->column_count(), state->level_count(), state->species_count(), state->clock().timestep,
            diagnostics_enabled ? 1 : 0, airden_dry_ptr, mairden_ptr, pedge_ptr, pfilsan_ptr, pfllsan_ptr, reevapls_ptr,
            t_ptr, (bool*)is_aerosol.data(), henry_cr.data(), henry_k0.data(), henry_pKa.data(), wd_retfactor.data(),
            (bool*)wd_LiqAndGas.data(), wd_convfacI2G.data(), wd_rainouteff.data_handle(), wd_reevap_frac.data(),
            radius.data(), mw_g.data(), state->chemistry().species_names_c_arr.data(), conc_ptr, mock_tendency.data(),
            diag_mass_bin.data(), diag_flux_bin.data(), diagnostic_species_id.data(), diagnostic_species_id.size());

        // 5. Map 3D bin diagnostics back to dynamically registered individual C++ diagnostics
        if (state->diagnostic_manager() && diagnostics_enabled) {
            for (size_t i = 0; i < state->chemistry().species_list.size(); ++i) {
                auto& meta = state->chemistry().species_list[i];
                if (meta.is_wetdep) {
                    std::string mass_name = "wetdep_mass_" + meta.short_name;
                    std::string flux_name = "wetdep_flux_" + meta.short_name;
                    double* mass_ptr = (double*)state->diagnostic_manager()->get_host_pointer(mass_name);
                    double* num_ptr = (double*)state->diagnostic_manager()->get_host_pointer(flux_name);
                    for (int col = 0; col < state->column_count(); ++col) {
                        for (int lvl = 0; lvl < state->level_count(); ++lvl) {
                            int idx =
                                col + lvl * state->column_count() + i * state->column_count() * state->level_count();
                            if (mass_ptr)
                                mass_ptr[col + lvl * state->column_count()] = diag_mass_bin[idx];
                            if (num_ptr)
                                num_ptr[col + lvl * state->column_count()] = diag_flux_bin[idx];
                        }
                    }
                }
            }
        }

        if (state->chemistry().conc)
            state->chemistry().conc->mark_host_modified();
    }

} // namespace catchem

extern "C" {
void catchem_register_wetdep_cpp() {
    catchem::ProcessRegistry::get_instance().register_process(
        "wetdep", []() { return std::make_shared<catchem::WetDepProcess>(); });
}
}

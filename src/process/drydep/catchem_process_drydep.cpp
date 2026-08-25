#include "catchem_process_drydep.hpp"
#include "catchem_diagnostic_manager.hpp"
#include "catchem_error.hpp"
#include "catchem_process_registry.hpp"
#include <iostream>

extern "C" {
void run_drydep_science_bridge(int n_cols, int n_levels, int n_species, double dt, const char* gas_scheme,
                               const char* aero_scheme, int diagnostics, double* bxheight, double* airden,
                               double* t_air, double* pedge, double* rh, double* cldfrc, double* frlai,
                               double* frlanduse, int* iland, bool* is_ice, bool* is_land, bool* is_snow, double* lat,
                               double* lon, double* obk, double* ps, double* salinity, double* suncosmid, double* swgdn,
                               double* ts, double* tskin, double* ustar, double* z0, double* frlake, double* gwettop,
                               double* hflux, int* lwi, double* pblh, double* u10m, double* v10m, double* z0h,
                               double* mw_g, double* dd_f0, double* dd_hstar, double* dd_DvzAerSnow,
                               double* dd_DvzMinVal_snow, double* dd_DvzMinVal_land, double* density, double* radius,
                               bool* is_seasalt, bool* is_dust, double* lower_radius, double* upper_radius,
                               bool* is_gas, double* conc, double* tendency, double* diag_con, double* diag_vel,
                               const int* diagnostic_species_id, int n_diag_species);
}

namespace catchem {

    ProcessContract DryDepProcess::get_contract() const {
        return {get_name(),
                {host_field_3d("T", "K"), host_field_3d("PMID", "Pa", FieldRequirement::Optional),
                 host_field_interface("PEDGE", "Pa"), host_field_3d("BXHEIGHT", "m"),
                 host_field_3d("AIRDEN", "kg/m3", FieldRequirement::Optional),
                 host_field_3d("AIRDEN_DRY", "kg/m3", FieldRequirement::Optional),
                 host_field_3d("RH", "1", FieldRequirement::Optional), host_field_2d("PS", "Pa"),
                 host_field_2d("TS", "K"),
                 host_field_2d("LAT", "degrees", FieldRequirement::Optional, AccessIntent::Read,
                               PersistencePolicy::Persistent),
                 host_field_2d("LON", "degrees", FieldRequirement::Optional, AccessIntent::Read,
                               PersistencePolicy::Persistent),
                 host_field_2d("USTAR", "m/s", FieldRequirement::Optional),
                 host_field_2d("HFLUX", "W/m2", FieldRequirement::Optional),
                 host_field_2d("OBK", "m", FieldRequirement::Optional),
                 host_field_2d("PBLH", "m", FieldRequirement::Optional), host_concentration()},
                {}};
    }

    DryDepProcess::DryDepProcess() : gas_scheme("wesely"), aero_scheme("gocart"), diagnostics_enabled(true) {}

    void DryDepProcess::init(std::shared_ptr<StateManager> state) {
        // 1. Setup diagnostic species ID dynamically based on the is_drydep metadata switch
        for (size_t i = 0; i < state->chemistry().species_list.size(); ++i) {
            if (state->chemistry().species_list[i].is_drydep) {
                diagnostic_species_id.push_back(i + 1); // 1-based for Fortran bridge
            }
        }

        // 2. Register C++ Diagnostic fields
        std::vector<int> dims_2d = {state->column_count(), state->species_count()};
        state->diagnostic_manager()->register_field("drydep_con_per_species", "Deposition Concentration", "ug/kg",
                                                    DiagType::FIELD_2D, dims_2d);
        state->diagnostic_manager()->register_field("drydep_velocity_per_species", "Deposition Velocity", "m/s",
                                                    DiagType::FIELD_2D, dims_2d);
    }

    void DryDepProcess::run(std::shared_ptr<StateManager> state) {

        // 1. Fetch 3D Met Views with fallbacks and derivations
        if (!state->meteorology().BXHEIGHT && state->meteorology().PEDGE && state->meteorology().T) {
            state->derive_bxheight();
        }
        double* bxheight_ptr = state->write_field<3>("BXHEIGHT");

        double* airden_ptr = state->write_field<3>("AIRDEN");
        if (!airden_ptr)
            airden_ptr = state->write_field<3>("AIRDEN_DRY");
        if (!airden_ptr && state->meteorology().PMID && state->meteorology().T) {
            state->derive_airden_dry();
            airden_ptr = state->write_field<3>("AIRDEN_DRY");
        }

        double* t_ptr = state->write_field<3>("T");
        double* pedge_ptr = state->write_field<3>("PEDGE");

        double* rh_ptr = state->write_field<3>("RH");
        std::vector<double> fallback_rh;
        if (!rh_ptr) {
            fallback_rh.assign(static_cast<size_t>(state->column_count()) * state->level_count(), 0.5);
            rh_ptr = fallback_rh.data();
        }

        // 2. Retrieve surface met and grid positions
        double* ps_ptr = state->write_field<2>("PS");
        double* ts_ptr = state->write_field<2>("TS");

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

        double* ustar_ptr = state->write_field<2>("USTAR");
        std::vector<double> fallback_ustar;
        if (!ustar_ptr) {
            fallback_ustar.assign(state->column_count(), 0.2);
            ustar_ptr = fallback_ustar.data();
        }

        double* hflux_ptr = state->write_field<2>("HFLUX");
        std::vector<double> fallback_hflux;
        if (!hflux_ptr) {
            fallback_hflux.assign(state->column_count(), 10.0);
            hflux_ptr = fallback_hflux.data();
        }

        double* obk_ptr = state->write_field<2>("OBK");
        std::vector<double> fallback_obk;
        if (!obk_ptr) {
            fallback_obk.assign(state->column_count(), 100.0);
            obk_ptr = fallback_obk.data();
        }

        double* pblh_ptr = state->write_field<2>("PBLH");
        std::vector<double> fallback_pblh;
        if (!pblh_ptr) {
            fallback_pblh.assign(state->column_count(), 1000.0);
            pblh_ptr = fallback_pblh.data();
        }

        require_field_pointer("DryDep", "BXHEIGHT", bxheight_ptr);
        require_field_pointer("DryDep", "AIRDEN", airden_ptr);
        require_field_pointer("DryDep", "T", t_ptr);
        require_field_pointer("DryDep", "PEDGE", pedge_ptr);
        require_field_pointer("DryDep", "PS", ps_ptr);
        require_field_pointer("DryDep", "TS", ts_ptr);

        // Mock/Fallbacks for remaining metadata arrays - Using char for bool to support standard .data()
        std::vector<double> cldfrc(state->column_count(), 0.1);

        // Multi-dimensional standard C++20 views using standard layout_left to match Fortran column-major
        std::vector<double> frlai_storage(state->column_count() * 1 * 20, 1.5);
        std::vector<double> frlanduse_storage(state->column_count() * 1 * 20, 0.05);
        std::vector<int> iland_storage(state->column_count() * 1 * 20, 1);

        Kokkos::mdspan<double, Kokkos::extents<int, Kokkos::dynamic_extent, 1, 20>, Kokkos::layout_left> frlai(
            frlai_storage.data(), state->column_count());
        Kokkos::mdspan<double, Kokkos::extents<int, Kokkos::dynamic_extent, 1, 20>, Kokkos::layout_left> frlanduse(
            frlanduse_storage.data(), state->column_count());
        Kokkos::mdspan<int, Kokkos::extents<int, Kokkos::dynamic_extent, 1, 20>, Kokkos::layout_left> iland(
            iland_storage.data(), state->column_count());

        std::vector<char> is_ice(state->column_count(), 0);
        std::vector<char> is_land(state->column_count(), 1);
        std::vector<char> is_snow(state->column_count(), 0);
        std::vector<double> salinity(state->column_count(), 35.0);
        std::vector<double> suncosmid(state->column_count(), 0.8);
        std::vector<double> swgdn(state->column_count(), 400.0);
        std::vector<double> tskin(state->column_count(), 288.15);
        std::vector<double> z0(state->column_count(), 0.1);
        std::vector<double> frlake(state->column_count(), 0.0);
        std::vector<double> gwettop(state->column_count(), 0.5);
        std::vector<int> lwi(state->column_count(), 1);
        std::vector<double> u10m(state->column_count(), 5.0);
        std::vector<double> v10m(state->column_count(), 2.0);
        std::vector<double> z0h(state->column_count(), 0.01);

        // 3. Extract chemical arrays & C++ allocated diagnostics
        double* conc_ptr = state->chemistry().conc ? state->chemistry().conc->host_write() : nullptr;
        require_field_pointer("DryDep", "CHEM_CONC", conc_ptr);

        // Allocate local tendencies buffer
        std::vector<double> mock_tendency(state->column_count() * state->level_count() * state->species_count(), 0.0);

        double* diag_con = (double*)state->diagnostic_manager()->get_host_pointer("drydep_con_per_species");
        double* diag_vel = (double*)state->diagnostic_manager()->get_host_pointer("drydep_velocity_per_species");

        // 4. Retrieve species configuration properties from ChemState
        std::vector<double> mw_g(state->species_count(), 29.0);
        std::vector<double> dd_f0(state->species_count(), 0.0);
        std::vector<double> dd_hstar(state->species_count(), 0.0);
        std::vector<double> dd_DvzAerSnow(state->species_count(), 0.0);
        std::vector<double> dd_DvzMinVal_snow(state->species_count(), 0.0);
        std::vector<double> dd_DvzMinVal_land(state->species_count(), 0.0);
        std::vector<double> density(state->species_count(), 1000.0);
        std::vector<double> radius(state->species_count(), 1e-6);
        std::vector<char> is_seasalt(state->species_count(), 0);
        std::vector<char> is_dust(state->species_count(), 0);
        std::vector<double> lower_radius(state->species_count(), 0.0);
        std::vector<double> upper_radius(state->species_count(), 0.0);
        std::vector<char> is_gas(state->species_count(), 1);

        for (size_t i = 0; i < state->chemistry().species_list.size(); ++i) {
            auto& meta = state->chemistry().species_list[i];
            if (meta.mw_g > 0.0)
                mw_g[i] = meta.mw_g;
            dd_f0[i] = meta.dd_f0;
            dd_hstar[i] = meta.dd_hstar;
            dd_DvzAerSnow[i] = meta.dd_DvzAerSnow;
            dd_DvzMinVal_snow[i] = meta.dd_DvzMinVal_snow;
            dd_DvzMinVal_land[i] = meta.dd_DvzMinVal_land;
            if (meta.density > 0.0)
                density[i] = meta.density;
            if (meta.radius > 0.0)
                radius[i] = meta.radius;
            is_seasalt[i] = meta.is_seasalt ? 1 : 0;
            is_dust[i] = meta.is_dust ? 1 : 0;
            if (meta.lower_radius > 0.0)
                lower_radius[i] = meta.lower_radius;
            if (meta.upper_radius > 0.0)
                upper_radius[i] = meta.upper_radius;
            if (lower_radius[i] <= 0.0)
                lower_radius[i] = radius[i] * 0.1;
            if (upper_radius[i] <= lower_radius[i])
                upper_radius[i] = radius[i] * 2.0;
            is_gas[i] = meta.is_gas ? 1 : 0;
        }

        // 5. Invoke flat science bridge (casting char* vectors to bool* pointers)
        run_drydep_science_bridge(
            state->column_count(), state->level_count(), state->species_count(), state->clock().timestep,
            gas_scheme.c_str(), aero_scheme.c_str(), diagnostics_enabled ? 1 : 0, bxheight_ptr, airden_ptr, t_ptr,
            pedge_ptr, rh_ptr, cldfrc.data(), frlai.data_handle(), frlanduse.data_handle(), iland.data_handle(),
            (bool*)is_ice.data(), (bool*)is_land.data(), (bool*)is_snow.data(), lat_ptr, lon_ptr, obk_ptr, ps_ptr,
            salinity.data(), suncosmid.data(), swgdn.data(), ts_ptr, tskin.data(), ustar_ptr, z0.data(), frlake.data(),
            gwettop.data(), hflux_ptr, lwi.data(), pblh_ptr, u10m.data(), v10m.data(), z0h.data(), mw_g.data(),
            dd_f0.data(), dd_hstar.data(), dd_DvzAerSnow.data(), dd_DvzMinVal_snow.data(), dd_DvzMinVal_land.data(),
            density.data(), radius.data(), (bool*)is_seasalt.data(), (bool*)is_dust.data(), lower_radius.data(),
            upper_radius.data(), (bool*)is_gas.data(), conc_ptr, mock_tendency.data(), diag_con, diag_vel,
            diagnostic_species_id.data(), diagnostic_species_id.size());

        if (state->chemistry().conc)
            state->chemistry().conc->mark_host_modified();
    }

} // namespace catchem

extern "C" {
void catchem_register_drydep_cpp() {
    catchem::ProcessRegistry::get_instance().register_process(
        "drydep", []() { return std::make_shared<catchem::DryDepProcess>(); });
}
}

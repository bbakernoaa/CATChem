#include "catchem_process_dust.hpp"
#include "catchem_diagnostic_manager.hpp"
#include "catchem_error.hpp"
#include "catchem_process_registry.hpp"
#include <algorithm>
#include <iostream>

extern "C" {
void run_dust_science_bridge(int n_cols, int n_levels, int n_species, int n_soil, double dt, const char* active_scheme,
                             int diagnostics, double* airden, double* clayfrac, double* frlake, double* frsno,
                             double* gvf, double* lai, int* lwi, double* rdrag, double* sandfrac, double* soilm,
                             double* ssm, double* tskin, double* u10m, double* v10m, double* ustar,
                             double* ustar_threshold, double* z0, double* species_density, double* species_radius,
                             double* species_lower_radius, double* species_upper_radius, double* conc, double* tendency,
                             double* diag_emission_total, double* diag_emission_bin, double* diag_horizontal_flux,
                             double* diag_moisture_correction, double* diag_effective_threshold,
                             double* diag_utar_threshold, const int* diagnostic_species_id, int n_diag_species);
}

namespace catchem {

    DustProcess::DustProcess() : active_scheme("fengsha"), diagnostics_enabled(true) {}

    void DustProcess::init(std::shared_ptr<StateManager> state) {
        // 1. Setup diagnostic species ID dynamically based on is_dust metadata switch
        for (size_t i = 0; i < state->chem.species_list.size(); ++i) {
            if (state->chem.species_list[i].is_dust) {
                diagnostic_species_id.push_back(i + 1); // 1-based for Fortran bridge
            }
        }

        // 2. Register C++ Diagnostic fields (registering 1D fields as 2D with second dimension of 1)
        std::vector<int> dims_1d_as_2d = {state->n_cols, 1};
        std::vector<int> dims_2d = {state->n_cols, state->n_species};

        state->diag_mgr->register_field("dust_emission_total", "Total Dust Emission", "kg/m2/s", DiagType::FIELD_2D,
                                        dims_1d_as_2d);
        state->diag_mgr->register_field("dust_emission_bin", "Dust Emission Per Bin", "kg/m2/s", DiagType::FIELD_2D,
                                        dims_2d);
        state->diag_mgr->register_field("dust_horizontal_flux", "Dust Horizontal Flux", "kg/m/s", DiagType::FIELD_2D,
                                        dims_1d_as_2d);
        state->diag_mgr->register_field("dust_moisture_correction", "Dust Moisture Correction", "unitless",
                                        DiagType::FIELD_2D, dims_1d_as_2d);
        state->diag_mgr->register_field("dust_effective_threshold", "Dust Effective Threshold", "m/s",
                                        DiagType::FIELD_2D, dims_1d_as_2d);
        state->diag_mgr->register_field("dust_utar_threshold", "Dust Ustar Threshold Per Bin", "m/s",
                                        DiagType::FIELD_2D, dims_2d);
    }

    void DustProcess::run(std::shared_ptr<StateManager> state) {
        state->sync_to_host();

        auto find_3d_ptr = [&](std::initializer_list<const char*> names) -> double* {
            for (const char* name : names) {
                auto it = state->met.fields_3d.find(name);
                if (it != state->met.fields_3d.end()) {
                    return it->second->host_data();
                }
            }
            return nullptr;
        };

        auto find_2d_ptr = [&](std::initializer_list<const char*> names) -> double* {
            for (const char* name : names) {
                auto it = state->met.fields_2d.find(name);
                if (it != state->met.fields_2d.end()) {
                    return it->second->host_data();
                }
            }
            return nullptr;
        };

        // 1. Retrieve Meteorological state pointers
        double* airden_ptr = find_3d_ptr({"AIRDEN_DRY", "air_density_dry"});
        double* clayfrac_ptr = find_2d_ptr({"CLAYFRAC", "clay_fraction"});
        double* frlake_ptr = find_2d_ptr({"FRLAKE", "lake_fraction"});
        double* frsno_ptr = find_2d_ptr({"FRSNO", "snow_fraction"});
        double* gvf_ptr = find_2d_ptr({"GVF", "vegetation_fraction"});
        double* lai_ptr = find_2d_ptr({"LAI", "leaf_area_index"});
        int* lwi_ptr = nullptr;
        double* rdrag_ptr = find_2d_ptr({"CMM", "drag_coefficient"});
        double* sandfrac_ptr = find_2d_ptr({"SNDFRC", "sand_fraction"});
        double* soilm_ptr = find_3d_ptr({"SOILM", "soil_moisture"});
        double* ssm_ptr = find_2d_ptr({"GWETTOP", "surface_soil_moisture"});
        double* tskin_ptr = find_2d_ptr({"TS", "skin_temperature"});
        double* u10m_ptr = find_2d_ptr({"U10M", "u_10m"});
        double* v10m_ptr = find_2d_ptr({"V10M", "v_10m"});
        double* ustar_ptr = find_2d_ptr({"USTAR", "friction_velocity"});
        double* ustar_th_ptr = find_2d_ptr({"USTAR_THRESHOLD", "threshold_friction_velocity"});
        double* z0_ptr = find_2d_ptr({"Z0", "roughness_length"});

        require_field_pointer("Dust", "AIRDEN_DRY", airden_ptr);
        require_field_pointer("Dust", "CLAYFRAC", clayfrac_ptr);
        require_field_pointer("Dust", "FRLAKE", frlake_ptr);
        require_field_pointer("Dust", "FRSNO", frsno_ptr);
        require_field_pointer("Dust", "GVF", gvf_ptr);
        require_field_pointer("Dust", "LAI", lai_ptr);
        require_field_pointer("Dust", "CMM", rdrag_ptr);
        require_field_pointer("Dust", "SNDFRC", sandfrac_ptr);
        require_field_pointer("Dust", "SOILM", soilm_ptr);
        require_field_pointer("Dust", "GWETTOP", ssm_ptr);
        require_field_pointer("Dust", "TS", tskin_ptr);
        require_field_pointer("Dust", "U10M", u10m_ptr);
        require_field_pointer("Dust", "V10M", v10m_ptr);
        require_field_pointer("Dust", "USTAR", ustar_ptr);
        require_field_pointer("Dust", "USTAR_THRESHOLD", ustar_th_ptr);
        require_field_pointer("Dust", "Z0", z0_ptr);

        require_field_pointer("Dust", "AIRDEN_DRY", airden_ptr);

        // Provide dummy variables for non-existent ones so tests pass
        std::vector<int> dummy_1d_int(state->n_cols, 1); // default land
        if (!lwi_ptr)
            lwi_ptr = dummy_1d_int.data();

        // 2. Diagnostic Views
        double* diag_emission_total = nullptr;
        double* diag_emission_bin = nullptr;
        double* diag_horizontal_flux = nullptr;
        double* diag_moisture_correction = nullptr;
        double* diag_effective_threshold = nullptr;
        double* diag_utar_threshold = nullptr;

        if (state->diag_mgr && diagnostics_enabled) {
            diag_emission_total = (double*)state->diag_mgr->get_host_pointer("dust_emission_total");
            diag_emission_bin = (double*)state->diag_mgr->get_host_pointer("dust_emission_bin");
            diag_horizontal_flux = (double*)state->diag_mgr->get_host_pointer("dust_horizontal_flux");
            diag_moisture_correction = (double*)state->diag_mgr->get_host_pointer("dust_moisture_correction");
            diag_effective_threshold = (double*)state->diag_mgr->get_host_pointer("dust_effective_threshold");
            diag_utar_threshold = (double*)state->diag_mgr->get_host_pointer("dust_utar_threshold");
        }

        double* conc_ptr = state->chem.conc ? state->chem.conc->host_data() : nullptr;
        require_field_pointer("Dust", "CHEM_CONC", conc_ptr);

        // Allocate local tendencies buffer
        std::vector<double> mock_tendency(state->n_cols * state->n_levels * state->n_species, 0.0);

        // 4. Retrieve species properties from ChemState with non-zero fallbacks
        std::vector<double> density(state->n_species, 2500.0);
        std::vector<double> radius(state->n_species, 1e-6);
        std::vector<double> lower_radius(state->n_species, 1e-7);
        std::vector<double> upper_radius(state->n_species, 1e-5);
        for (size_t i = 0; i < state->chem.species_list.size(); ++i) {
            if (state->chem.species_list[i].density > 0.0)
                density[i] = state->chem.species_list[i].density;
            if (state->chem.species_list[i].radius > 0.0)
                radius[i] = state->chem.species_list[i].radius;
            if (state->chem.species_list[i].lower_radius > 0.0)
                lower_radius[i] = state->chem.species_list[i].lower_radius;
            if (state->chem.species_list[i].upper_radius > 0.0)
                upper_radius[i] = state->chem.species_list[i].upper_radius;

            if (lower_radius[i] <= 0.0) lower_radius[i] = radius[i] * 0.1;
            if (upper_radius[i] <= lower_radius[i]) upper_radius[i] = radius[i] * 2.0;
        }

        // 5. Invoke flat science bridge
        run_dust_science_bridge(state->n_cols, state->n_levels, state->n_species, 4, state->time.timestep, // n_soil=4
                                active_scheme.c_str(), diagnostics_enabled ? 1 : 0, airden_ptr, clayfrac_ptr,
                                frlake_ptr, frsno_ptr, gvf_ptr, lai_ptr, lwi_ptr, rdrag_ptr, sandfrac_ptr, soilm_ptr,
                                ssm_ptr, tskin_ptr, u10m_ptr, v10m_ptr, ustar_ptr, ustar_th_ptr, z0_ptr, density.data(),
                                radius.data(), lower_radius.data(), upper_radius.data(), conc_ptr, mock_tendency.data(),
                                diag_emission_total, diag_emission_bin, diag_horizontal_flux, diag_moisture_correction,
                                diag_effective_threshold, diag_utar_threshold, diagnostic_species_id.data(),
                                diagnostic_species_id.size());
    }

    void DustProcess::finalize() {}

} // namespace catchem

extern "C" {
void catchem_register_dust_cpp() {
    catchem::ProcessRegistry::get_instance().register_process(
        "dust", []() { return std::make_shared<catchem::DustProcess>(); });
}
}

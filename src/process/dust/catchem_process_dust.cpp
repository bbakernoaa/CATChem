#include "catchem_process_dust.hpp"
#include "catchem_diagnostic_manager.hpp"
#include "catchem_error.hpp"
#include "catchem_logger.hpp"
#include "catchem_process_registry.hpp"
#include <algorithm>
#include <cmath>
#include <iostream>
#include <limits>
#include <stdexcept>

extern "C" {
void run_dust_science_bridge(int n_cols, int n_levels, int n_species, int n_soil, double dt, const char* active_scheme,
                             int diagnostics, double fengsha_alpha, double fengsha_gamma,
                             double fengsha_drylimit_factor, double fengsha_moisture_factor, double fengsha_kvhmax,
                             int fengsha_drag_option, int fengsha_horizflux_option, int fengsha_moist_option,
                             int fengsha_distribution_option, const double* ginoux_ch_du, int n_ginoux_ch_du,
                             const double* airden, const double* delp, const double* clayfrac,
                             const double* frlake, const double* frsno, const double* gvf, const double* lai, int* lwi,
                             const double* rdrag, const double* sandfrac, const double* soilm, const double* gwettop,
                             const double* ssm, const double* tskin, const double* u10m, const double* v10m,
                             const double* ustar, const double* ustar_threshold, const double* z0,
                             const double* species_density, const double* species_radius,
                             const double* species_lower_radius, const double* species_upper_radius, double* conc,
                             double* tendency, double* diag_emission_total, double* diag_emission_bin,
                             double* diag_horizontal_flux, double* diag_moisture_correction,
                             double* diag_effective_threshold, double* diag_utar_threshold,
                             const int* diagnostic_species_id, int n_diag_species);
}

namespace catchem {

    ProcessContract DustProcess::get_contract() const {
        std::vector<FieldAccessContract> fields{host_field_interface("PEDGE", "Pa"), host_field_3d("DELP", "Pa"),
                                                host_field_3d("AIRDEN_DRY", "kg/m3"), host_concentration()};
        if (active_scheme == "fengsha") {
            fields.insert(fields.end(),
                          {host_field_soil_layer("SOILM", "m3/m3"), host_field_2d("CLAYFRAC", "1"),
                           host_field_2d("FRLAKE", "1"), host_field_2d("FRSNO", "frac"), host_field_2d("GVF", "frac"),
                           host_field_2d("LAI", "m2/m2", FieldRequirement::Optional), host_field_2d("LWI", "1"),
                           host_field_2d("RDRAG", "1"), host_field_2d("SNDFRC", "1"), host_field_2d("SSM", "1"),
                           host_field_2d("TS", "K"), host_field_2d("USTAR", "m/s"),
                           host_field_2d("USTAR_THRESHOLD", "m/s"), host_field_2d("Z0", "m")});
        } else if (active_scheme == "ginoux") {
            fields.insert(fields.end(),
                          {host_field_2d("FRLAKE", "1"), host_field_2d("FRSNO", "frac"), host_field_2d("GWETTOP", "1"),
                           host_field_2d("LWI", "1"), host_field_2d("SSM", "1"), host_field_2d("TS", "K"),
                           host_field_2d("U10M", "m/s"), host_field_2d("V10M", "m/s")});
        }
        return {get_name(), std::move(fields), {}};
    }

    DustProcess::DustProcess() : active_scheme("fengsha"), diagnostics_enabled(true) {}

    void DustProcess::prepare_inputs(std::shared_ptr<StateManager> state) {
        state->derive_delp();
        if (!state->read_field<3>("AIRDEN_DRY"))
            state->derive_airden_dry();
    }

    void DustProcess::init(std::shared_ptr<StateManager> state) {
        const auto config = state->config_manager();
        if (!config)
            throw std::invalid_argument("Dust requires a runtime YAML configuration");
        const auto configured = config->data.processes.find("dust");
        if (configured == config->data.processes.end() || configured->second.scheme.empty())
            throw std::invalid_argument("Dust requires processes.dust.scheme in the runtime YAML");
        active_scheme = configured->second.scheme;
        diagnostics_enabled = configured->second.diagnostics;
        if (active_scheme != "fengsha" && active_scheme != "ginoux")
            throw std::invalid_argument("Dust runtime YAML selected unsupported scheme: " + active_scheme);

        // 2. Read scheme tuning options from the runtime YAML.  Each lookup
        // falls back to the compiled default declared in DustCommon_Mod.F90,
        // so a configuration that omits the option keeps current behavior.
        // Core validates the option names against the registered schema, so a
        // misspelled key fails at initialization rather than being dropped.
        const auto& settings = configured->second;
        fengsha_alpha = settings.get_double("fengsha/alpha", fengsha_alpha);
        fengsha_gamma = settings.get_double("fengsha/gamma", fengsha_gamma);
        fengsha_drylimit_factor = settings.get_double("fengsha/drylimit_factor", fengsha_drylimit_factor);
        fengsha_moist_correction_factor =
            settings.get_double("fengsha/moist_correction_factor", fengsha_moist_correction_factor);
        fengsha_kvhmax = settings.get_double("fengsha/kvhmax", fengsha_kvhmax);
        fengsha_drag_option = settings.get_int("fengsha/drag_option", fengsha_drag_option);
        fengsha_horizflux_option = settings.get_int("fengsha/horizflux_option", fengsha_horizflux_option);
        fengsha_moist_option = settings.get_int("fengsha/moist_option", fengsha_moist_option);
        fengsha_distribution_option = settings.get_int("fengsha/distribution_option", fengsha_distribution_option);

        // Ch_DU carries one multiplier per dust size bin; the scheme type
        // declares exactly five bins, so a provided sequence must match.
        auto ch_du = settings.get_vector("ginoux/Ch_DU");
        if (!ch_du.empty()) {
            if (ch_du.size() != ginoux_ch_du.size())
                throw std::invalid_argument("Dust ginoux Ch_DU must declare " + std::to_string(ginoux_ch_du.size()) +
                                            " values, one per dust size bin");
            ginoux_ch_du = std::move(ch_du);
        }

        // 3. Setup diagnostic species ID dynamically based on is_dust metadata switch
        for (size_t i = 0; i < state->chemistry().species_list.size(); ++i) {
            if (state->chemistry().species_list[i].is_dust) {
                diagnostic_species_id.push_back(i + 1); // 1-based for Fortran bridge
            }
        }

        if (!diagnostics_enabled)
            return;

        // 4. Register C++ Diagnostic fields (registering 1D fields as 2D with second dimension of 1)
        std::vector<int> dims_1d_as_2d = {state->column_count(), 1};
        std::vector<int> dims_2d = {state->column_count(), state->species_count()};

        state->diagnostic_manager()->register_field("dust_emission_total", "Total Dust Emission", "kg/m2/s",
                                                    DiagType::FIELD_2D, dims_1d_as_2d);
        state->diagnostic_manager()->register_field("dust_emission_bin", "Dust Emission Per Bin", "kg/m2/s",
                                                    DiagType::FIELD_2D, dims_2d);
        state->diagnostic_manager()->register_field("dust_horizontal_flux", "Dust Horizontal Flux", "kg/m/s",
                                                    DiagType::FIELD_2D, dims_1d_as_2d);
        state->diagnostic_manager()->register_field("dust_moisture_correction", "Dust Moisture Correction", "unitless",
                                                    DiagType::FIELD_2D, dims_1d_as_2d);
        state->diagnostic_manager()->register_field("dust_effective_threshold", "Dust Effective Threshold", "m/s",
                                                    DiagType::FIELD_2D, dims_1d_as_2d);
        state->diagnostic_manager()->register_field("dust_utar_threshold", "Dust Ustar Threshold Per Bin", "m/s",
                                                    DiagType::FIELD_2D, dims_2d);
    }

    void DustProcess::run(std::shared_ptr<StateManager> state) {

        // 1. Retrieve Meteorological state pointers
        const double* airden_ptr = state->read_field<3>("AIRDEN_DRY");
        const double* delp_ptr = state->read_field<3>("DELP");
        const double* clayfrac_ptr = state->read_field<2>("CLAYFRAC");
        const double* frlake_ptr = state->read_field<2>("FRLAKE");
        const double* frsno_ptr = state->read_field<2>("FRSNO");
        const double* gvf_ptr = state->read_field<2>("GVF");
        const double* lai_ptr = state->read_field<2>("LAI");
        const double* lwi_double_ptr = state->read_field<2>("LWI");
        std::vector<int> lwi(state->column_count());
        require_field_pointer("Dust", "LWI", lwi_double_ptr);
        for (int col = 0; col < state->column_count(); ++col)
            lwi[col] = static_cast<int>(lwi_double_ptr[col]);
        const double* rdrag_ptr = state->read_field<2>("RDRAG");
        const double* sandfrac_ptr = state->read_field<2>("SNDFRC");
        const double* soilm_ptr = state->read_field<3>("SOILM");
        const auto soilm_field = state->find_field<3>("SOILM");
        const int n_soil = soilm_field ? static_cast<int>(soilm_field->extent(1)) : 0;
        const double* gwettop_ptr = state->read_field<2>("GWETTOP");
        const double* ssm_ptr = state->read_field<2>("SSM");
        const double* tskin_ptr = state->read_field<2>("TS");
        const double* u10m_ptr = state->read_field<2>("U10M");
        const double* v10m_ptr = state->read_field<2>("V10M");
        const double* ustar_ptr = state->read_field<2>("USTAR");
        const double* ustar_th_ptr = state->read_field<2>("USTAR_THRESHOLD");
        const double* z0_ptr = state->read_field<2>("Z0");

        require_field_pointer("Dust", "AIRDEN_DRY", airden_ptr);
        require_field_pointer("Dust", "DELP", delp_ptr);
        if (active_scheme == "fengsha") {
            require_field_pointer("Dust", "CLAYFRAC", clayfrac_ptr);
            require_field_pointer("Dust", "FRLAKE", frlake_ptr);
            require_field_pointer("Dust", "FRSNO", frsno_ptr);
            require_field_pointer("Dust", "GVF", gvf_ptr);
            require_field_pointer("Dust", "LAI", lai_ptr);
            require_field_pointer("Dust", "RDRAG", rdrag_ptr);
            require_field_pointer("Dust", "SNDFRC", sandfrac_ptr);
            require_field_pointer("Dust", "SOILM", soilm_ptr);
            if (n_soil <= 0)
                throw std::runtime_error("Dust: SOILM has no soil-layer extent");
            require_field_pointer("Dust", "SSM", ssm_ptr);
            require_field_pointer("Dust", "TS", tskin_ptr);
            require_field_pointer("Dust", "USTAR", ustar_ptr);
            require_field_pointer("Dust", "USTAR_THRESHOLD", ustar_th_ptr);
            require_field_pointer("Dust", "Z0", z0_ptr);
        } else {
            require_field_pointer("Dust", "FRLAKE", frlake_ptr);
            require_field_pointer("Dust", "FRSNO", frsno_ptr);
            require_field_pointer("Dust", "GWETTOP", gwettop_ptr);
            require_field_pointer("Dust", "SSM", ssm_ptr);
            require_field_pointer("Dust", "TS", tskin_ptr);
            require_field_pointer("Dust", "U10M", u10m_ptr);
            require_field_pointer("Dust", "V10M", v10m_ptr);
        }

        const auto config = state->config_manager();
        if (config && config->data.simulation.verbose_enabled) {
            double min_delp = std::numeric_limits<double>::infinity();
            double max_delp = 0.0;
            for (int level = 0; level < state->level_count(); ++level) {
                for (int column = 0; column < state->column_count(); ++column) {
                    const std::size_t index =
                        static_cast<std::size_t>(column) + static_cast<std::size_t>(level) * state->column_count();
                    if (std::isfinite(delp_ptr[index]) && delp_ptr[index] > 0.0) {
                        min_delp = std::min(min_delp, delp_ptr[index]);
                        max_delp = std::max(max_delp, delp_ptr[index]);
                    }
                }
            }
            Logger::debug(state.get(), "Dust layer-mass conversion inputs",
                          {{"delp_pa_min", std::to_string(min_delp)},
                           {"delp_pa_max", std::to_string(max_delp)},
                           {"conversion", "flux*g/DELP*1e9 kg/kg-to-ug/kg"}});
        }

        // 2. Diagnostic Views
        double* diag_emission_total = nullptr;
        double* diag_emission_bin = nullptr;
        double* diag_horizontal_flux = nullptr;
        double* diag_moisture_correction = nullptr;
        double* diag_effective_threshold = nullptr;
        double* diag_utar_threshold = nullptr;

        if (state->diagnostic_manager() && diagnostics_enabled) {
            diag_emission_total = (double*)state->diagnostic_manager()->get_host_pointer("dust_emission_total");
            diag_emission_bin = (double*)state->diagnostic_manager()->get_host_pointer("dust_emission_bin");
            diag_horizontal_flux = (double*)state->diagnostic_manager()->get_host_pointer("dust_horizontal_flux");
            diag_moisture_correction =
                (double*)state->diagnostic_manager()->get_host_pointer("dust_moisture_correction");
            diag_effective_threshold =
                (double*)state->diagnostic_manager()->get_host_pointer("dust_effective_threshold");
            diag_utar_threshold = (double*)state->diagnostic_manager()->get_host_pointer("dust_utar_threshold");
        }

        double* conc_ptr = state->chemistry().conc ? state->chemistry().conc->host_write() : nullptr;
        require_field_pointer("Dust", "CHEM_CONC", conc_ptr);

        // 4. Slice dust species so the scheme size distribution matches the legacy Fortran interface.
        std::vector<int> dust_global_indices;
        std::vector<double> density;
        std::vector<double> radius;
        std::vector<double> lower_radius;
        std::vector<double> upper_radius;
        for (size_t i = 0; i < state->chemistry().species_list.size(); ++i) {
            const auto& meta = state->chemistry().species_list[i];
            if (meta.is_dust) {
                if (!(meta.density > 0.0 && meta.radius > 0.0 && meta.lower_radius > 0.0 &&
                      meta.upper_radius > meta.lower_radius))
                    throw std::runtime_error(
                        "Dust species '" + meta.short_name +
                        "' requires explicit positive density, radius, lower_radius, and upper_radius");

                dust_global_indices.push_back(static_cast<int>(i));
                density.push_back(meta.density);
                radius.push_back(meta.radius);
                lower_radius.push_back(meta.lower_radius);
                upper_radius.push_back(meta.upper_radius);
            }
        }

        const int n_dust = static_cast<int>(dust_global_indices.size());
        if (n_dust == 0) {
            return;
        }

        std::vector<double> sliced_conc(static_cast<size_t>(state->column_count()) * state->level_count() * n_dust,
                                        0.0);
        for (int local_idx = 0; local_idx < n_dust; ++local_idx) {
            const int global_idx = dust_global_indices[local_idx];
            for (int col = 0; col < state->column_count(); ++col) {
                for (int lev = 0; lev < state->level_count(); ++lev) {
                    const int src_idx =
                        col + lev * state->column_count() + global_idx * state->column_count() * state->level_count();
                    const int dest_idx =
                        col + lev * state->column_count() + local_idx * state->column_count() * state->level_count();
                    sliced_conc[dest_idx] = conc_ptr[src_idx];
                }
            }
        }

        std::vector<double> mock_tendency(static_cast<size_t>(state->column_count()) * state->level_count() * n_dust,
                                          0.0);
        std::vector<int> local_diagnostic_species_id(n_dust);
        for (int local_idx = 0; local_idx < n_dust; ++local_idx) {
            local_diagnostic_species_id[local_idx] = local_idx + 1;
        }

        std::vector<double> local_diag_emission_bin(static_cast<size_t>(state->column_count()) * n_dust, 0.0);
        std::vector<double> local_diag_utar_threshold(static_cast<size_t>(state->column_count()) * n_dust, 0.0);

        // 5. Invoke flat science bridge
        run_dust_science_bridge(
            state->column_count(), state->level_count(), n_dust, n_soil, state->clock().timestep, active_scheme.c_str(),
            diagnostics_enabled ? 1 : 0, fengsha_alpha, fengsha_gamma, fengsha_drylimit_factor,
            fengsha_moist_correction_factor, fengsha_kvhmax, fengsha_drag_option, fengsha_horizflux_option,
            fengsha_moist_option, fengsha_distribution_option, ginoux_ch_du.data(), static_cast<int>(ginoux_ch_du.size()),
            airden_ptr, delp_ptr, clayfrac_ptr, frlake_ptr, frsno_ptr, gvf_ptr, lai_ptr,
            lwi.data(), rdrag_ptr, sandfrac_ptr, soilm_ptr, gwettop_ptr, ssm_ptr, tskin_ptr, u10m_ptr, v10m_ptr,
            ustar_ptr, ustar_th_ptr, z0_ptr, density.data(), radius.data(), lower_radius.data(), upper_radius.data(),
            sliced_conc.data(), mock_tendency.data(), diag_emission_total, local_diag_emission_bin.data(),
            diag_horizontal_flux, diag_moisture_correction, diag_effective_threshold, local_diag_utar_threshold.data(),
            local_diagnostic_species_id.data(), local_diagnostic_species_id.size());

        for (int local_idx = 0; local_idx < n_dust; ++local_idx) {
            const int global_idx = dust_global_indices[local_idx];
            for (int col = 0; col < state->column_count(); ++col) {
                for (int lev = 0; lev < state->level_count(); ++lev) {
                    const int src_idx =
                        col + lev * state->column_count() + local_idx * state->column_count() * state->level_count();
                    const int dest_idx =
                        col + lev * state->column_count() + global_idx * state->column_count() * state->level_count();
                    conc_ptr[dest_idx] = sliced_conc[src_idx];
                }

                if (diag_emission_bin) {
                    diag_emission_bin[col + global_idx * state->column_count()] =
                        local_diag_emission_bin[col + local_idx * state->column_count()];
                }
                if (diag_utar_threshold) {
                    diag_utar_threshold[col + global_idx * state->column_count()] =
                        local_diag_utar_threshold[col + local_idx * state->column_count()];
                }
            }
        }

        if (state->chemistry().conc)
            state->chemistry().conc->mark_host_modified();
    }

    void DustProcess::finalize() {}

} // namespace catchem

extern "C" {
void catchem_register_dust_cpp() {
    catchem::ProcessRegistry::get_instance().register_process(
        "dust", []() { return std::make_shared<catchem::DustProcess>(); }, {},
        catchem::make_settings_validator("dust", {"fengsha/alpha", "fengsha/gamma", "fengsha/drylimit_factor",
                                                  "fengsha/moist_correction_factor", "fengsha/kvhmax",
                                                  "fengsha/drag_option", "fengsha/horizflux_option",
                                                  "fengsha/moist_option", "fengsha/distribution_option",
                                                  "ginoux/Ch_DU"}));
}
}

#include "catchem_process_seasalt.hpp"
#include "catchem_diagnostic_manager.hpp"
#include "catchem_error.hpp"
#include "catchem_process_registry.hpp"
#include <iostream>

extern "C" {
void run_seasalt_science_bridge(int n_cols, int n_levels, int n_species, double dt, const char* active_scheme,
                                int diagnostics, double* frocean, double* frseaice, double* lat, double* lon,
                                double* sst, double* u10m, double* v10m, double* ustar, double* delp, double* density,
                                double* radius, double* lower_radius, double* upper_radius, bool* is_gas, double* mw_g,
                                double* conc, double* tendency, double* diag_mass_total, double* diag_num_total,
                                double* diag_mass_bin, double* diag_num_bin, const int* diagnostic_species_id,
                                int n_diag_species);
}

namespace catchem {

    ProcessContract SeaSaltProcess::get_contract() const {
        return {get_name(), {host_field_interface("PEDGE", "Pa", FieldRequirement::Optional),
                            host_field_3d("DELP", "Pa", FieldRequirement::Optional),
                            host_field_2d("FROCEAN", "1"), host_field_2d("FRSEAICE", "1"),
                            host_field_2d("TS", "K"),
                            host_field_2d("LAT", "degrees", FieldRequirement::Required, AccessIntent::Read,
                                                          PersistencePolicy::Persistent),
                            host_field_2d("LON", "degrees", FieldRequirement::Required, AccessIntent::Read,
                                                          PersistencePolicy::Persistent),
                            host_field_2d("USTAR", "m/s"), host_field_2d("U10M", "m/s"),
                            host_field_2d("V10M", "m/s"),
                            host_concentration()}, {}};
    }

    SeaSaltProcess::SeaSaltProcess() : active_scheme("geos12"), diagnostics_enabled(true) {}

    void SeaSaltProcess::init(std::shared_ptr<StateManager> state) {
        if (state->diagnostic_manager()) {
            std::vector<int> dims_1d = {state->column_count(), 1};
            state->diagnostic_manager()->register_field("seasalt_mass_emission_total", "Total Mass Emission", "kg/m2/s",
                                            DiagType::FIELD_2D, dims_1d);
            state->diagnostic_manager()->register_field("seasalt_number_emission_total", "Total Number Emission", "#/m2/s",
                                            DiagType::FIELD_2D, dims_1d);

            for (size_t i = 0; i < state->chemistry().species_list.size(); ++i) {
                auto& meta = state->chemistry().species_list[i];
                if (meta.is_seasalt) {
                    std::string mass_name = "seasalt_mass_emission_" + meta.short_name;
                    std::string num_name = "seasalt_number_emission_" + meta.short_name;
                    state->diagnostic_manager()->register_field(mass_name, "Mass Emission " + meta.short_name, "kg/m2/s",
                                                    DiagType::FIELD_2D, dims_1d);
                    state->diagnostic_manager()->register_field(num_name, "Number Emission " + meta.short_name, "#/m2/s",
                                                    DiagType::FIELD_2D, dims_1d);
                }
            }
        }
    }

    void SeaSaltProcess::run(std::shared_ptr<StateManager> state) {

        double* frocean_ptr = state->write_field<2>("FROCEAN");
        double* frseaice_ptr = state->write_field<2>("FRSEAICE");
        double* sst_ptr = state->write_field<2>("TS");
        double* lat_ptr = state->write_field<2>("LAT");
        double* lon_ptr = state->write_field<2>("LON");
        double* ustar_ptr = state->write_field<2>("USTAR");
        double* u10m_ptr = state->write_field<2>("U10M");
        double* v10m_ptr = state->write_field<2>("V10M");

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

        require_field_pointer("SeaSalt", "FROCEAN", frocean_ptr);
        require_field_pointer("SeaSalt", "FRSEAICE", frseaice_ptr);
        require_field_pointer("SeaSalt", "LAT", lat_ptr);
        require_field_pointer("SeaSalt", "LON", lon_ptr);
        require_field_pointer("SeaSalt", "SST", sst_ptr);
        require_field_pointer("SeaSalt", "USTAR", ustar_ptr);
        require_field_pointer("SeaSalt", "U10M", u10m_ptr);
        require_field_pointer("SeaSalt", "V10M", v10m_ptr);
        require_field_pointer("SeaSalt", "DELP", delp_ptr);

        // 2. Identify and slice SeaSalt-only chemical species to comply with science solver limits
        std::vector<int> ss_global_indices;
        std::vector<double> density;
        std::vector<double> radius;
        std::vector<double> lower_radius;
        std::vector<double> upper_radius;
        std::vector<char> is_gas;
        std::vector<double> mw_g;

        for (size_t i = 0; i < state->chemistry().species_list.size(); ++i) {
            auto& meta = state->chemistry().species_list[i];
            if (meta.is_seasalt) {
                ss_global_indices.push_back(i);
                double d_val = meta.density > 0.0 ? meta.density : 2200.0;
                double r_val = meta.radius > 0.0 ? meta.radius : 1.0e-6;
                double lr_val = meta.lower_radius > 0.0 ? meta.lower_radius : r_val * 0.1;
                double ur_val = meta.upper_radius > lr_val ? meta.upper_radius : r_val * 2.0;

                density.push_back(d_val);
                radius.push_back(r_val);
                lower_radius.push_back(lr_val);
                upper_radius.push_back(ur_val);
                is_gas.push_back(meta.is_gas ? 1 : 0);
                mw_g.push_back(meta.mw_g > 0.0 ? meta.mw_g : 58.44);
            }
        }

        int n_seasalt = ss_global_indices.size();
        if (n_seasalt == 0)
            return; // No sea salt species configured

        // Extract chemical concentration raw host pointer
        double* conc_ptr = state->chemistry().conc ? state->chemistry().conc->host_write() : nullptr;
        require_field_pointer("SeaSalt", "CHEM_CONC", conc_ptr);

        // Allocate contiguous temporary slice for concentrations
        std::vector<double> sliced_conc(state->column_count() * state->level_count() * n_seasalt, 0.0);
        for (int i = 0; i < n_seasalt; ++i) {
            int g_idx = ss_global_indices[i];
            for (int col = 0; col < state->column_count(); ++col) {
                for (int lvl = 0; lvl < state->level_count(); ++lvl) {
                    int src_idx = col + lvl * state->column_count() + g_idx * state->column_count() * state->level_count();
                    int dest_idx = col + lvl * state->column_count() + i * state->column_count() * state->level_count();
                    sliced_conc[dest_idx] = conc_ptr[src_idx];
                }
            }
        }

        // Allocate local tendencies buffer for sliced subset
        std::vector<double> mock_tendency(state->column_count() * state->level_count() * n_seasalt, 0.0);

        // 3. Extract diagnostics
        double* diag_mass_total_ptr =
            state->diagnostic_manager() ? (double*)state->diagnostic_manager()->get_host_pointer("seasalt_mass_emission_total") : nullptr;
        double* diag_num_total_ptr =
            state->diagnostic_manager() ? (double*)state->diagnostic_manager()->get_host_pointer("seasalt_number_emission_total") : nullptr;

        std::vector<double> diag_mass_bin(state->column_count() * n_seasalt, 0.0);
        std::vector<double> diag_num_bin(state->column_count() * n_seasalt, 0.0);

        // Dynamic ID array mapping diagnostic species (1-based index in the sliced sea salt subset!)
        std::vector<int> diagnostic_species_id(n_seasalt);
        for (int i = 0; i < n_seasalt; ++i) {
            diagnostic_species_id[i] = i + 1;
        }

        // 5. Invoke flat science bridge
        run_seasalt_science_bridge(state->column_count(), state->level_count(), n_seasalt, state->clock().timestep,
                                   active_scheme.c_str(), diagnostics_enabled ? 1 : 0, frocean_ptr, frseaice_ptr,
                                   lat_ptr, lon_ptr, sst_ptr, u10m_ptr, v10m_ptr, ustar_ptr, delp_ptr, density.data(),
                                   radius.data(), lower_radius.data(), upper_radius.data(), (bool*)is_gas.data(),
                                   mw_g.data(), sliced_conc.data(), mock_tendency.data(), diag_mass_total_ptr,
                                   diag_num_total_ptr, diag_mass_bin.data(), diag_num_bin.data(),
                                   diagnostic_species_id.data(), diagnostic_species_id.size());

        // 6. Copy sliced concentrations back to main unified chemistry state
        for (int i = 0; i < n_seasalt; ++i) {
            int g_idx = ss_global_indices[i];
            for (int col = 0; col < state->column_count(); ++col) {
                for (int lvl = 0; lvl < state->level_count(); ++lvl) {
                    int src_idx = col + lvl * state->column_count() + i * state->column_count() * state->level_count();
                    int dest_idx = col + lvl * state->column_count() + g_idx * state->column_count() * state->level_count();
                    conc_ptr[dest_idx] = sliced_conc[src_idx];
                }
            }
        }

        // 7. Map bin diagnostics back to dynamically registered individual C++ diagnostics
        if (state->diagnostic_manager() && diagnostics_enabled) {
            for (int i = 0; i < n_seasalt; ++i) {
                auto& meta = state->chemistry().species_list[ss_global_indices[i]];
                std::string mass_name = "seasalt_mass_emission_" + meta.short_name;
                std::string num_name = "seasalt_number_emission_" + meta.short_name;
                double* mass_ptr = (double*)state->diagnostic_manager()->get_host_pointer(mass_name);
                double* num_ptr = (double*)state->diagnostic_manager()->get_host_pointer(num_name);
                for (int col = 0; col < state->column_count(); ++col) {
                    if (mass_ptr)
                        mass_ptr[col] = diag_mass_bin[col + i * state->column_count()];
                    if (num_ptr)
                        num_ptr[col] = diag_num_bin[col + i * state->column_count()];
                }
            }
        }

        if (state->chemistry().conc) state->chemistry().conc->mark_host_modified();
    }

} // namespace catchem

extern "C" {
void catchem_register_seasalt_cpp() {
    catchem::ProcessRegistry::get_instance().register_process(
        "seasalt", []() { return std::make_shared<catchem::SeaSaltProcess>(); });
}
}

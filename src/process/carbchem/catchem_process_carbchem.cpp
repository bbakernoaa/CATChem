#include "catchem_process_carbchem.hpp"
#include "catchem_diagnostic_manager.hpp"
#include "catchem_error.hpp"
#include "catchem_process_registry.hpp"
#include <algorithm>
#include <iostream>

extern "C" {
void run_carbchem_science_bridge(int n_cols, int n_levels, int n_species, double dt, const char* active_scheme,
                                 int diagnostics, int year, int month, int day, int hour, int minute, int second,
                                 double* airden, double* delp, double* pmid, double* species_t_chem_loss,
                                 const char* species_names_char, double* conc, double* tendency, double* diag_prod_mass,
                                 double* diag_loss_flux, double* diag_phobic_mass, double* diag_phobic_flux,
                                 const int* diagnostic_species_id, int n_diag_species);
}

namespace catchem {

    ProcessContract CarbChemProcess::get_contract() const {
        return {get_name(), {host_field_3d("PMID", "Pa"), host_field_interface("PEDGE", "Pa", FieldRequirement::Optional),
                            host_field_3d("T", "K", FieldRequirement::Optional),
                            host_field_3d("DELP", "Pa", FieldRequirement::Optional),
                            host_field_3d("AIRDEN_DRY", "kg/m3", FieldRequirement::Optional),
                            host_field_3d("AIRDEN", "kg/m3", FieldRequirement::Optional),
                            host_concentration()}, {}};
    }

    CarbChemProcess::CarbChemProcess() : active_scheme("gocart"), diagnostics_enabled(true) {}

    void CarbChemProcess::init(std::shared_ptr<StateManager> state) {
        // 1. Setup diagnostic species ID dynamically (using a dummy is_carbchem flag if we had one, but we map all
        // indices here for simplicity since CarbChem filters internally)
        for (size_t i = 0; i < state->chemistry().species_list.size(); ++i) {
            diagnostic_species_id.push_back(i + 1); // 1-based for Fortran bridge
        }

        // 2. Register C++ Diagnostic fields
        std::vector<int> dims_3d = {state->column_count(), state->level_count(), state->species_count()};
        std::vector<int> dims_2d = {state->column_count(), state->species_count()};

        state->diagnostic_manager()->register_field("carbchem_prod_mass", "Carbon Chemistry Production Mass", "kg/kg",
                                        DiagType::FIELD_3D, dims_3d);
        state->diagnostic_manager()->register_field("carbchem_loss_flux", "Carbon Chemistry Loss Flux", "kg/m2/s",
                                        DiagType::FIELD_2D, dims_2d);
        state->diagnostic_manager()->register_field("carbchem_phobic_mass", "Carbon Chemistry Phobic to Philic Mass", "kg/kg",
                                        DiagType::FIELD_3D, dims_3d);
        state->diagnostic_manager()->register_field("carbchem_phobic_flux", "Carbon Chemistry Phobic to Philic Flux", "kg/m2/s",
                                        DiagType::FIELD_2D, dims_2d);
    }

    void CarbChemProcess::run(std::shared_ptr<StateManager> state) {

        // 1. Retrieve 3D Meteorological state pointers
        double* airden_ptr = state->write_field<3>("AIRDEN_DRY");
        if (!airden_ptr) airden_ptr = state->write_field<3>("AIRDEN");
        if (!airden_ptr && state->meteorology().PMID && state->meteorology().T) {
            state->derive_airden_dry();
            airden_ptr = state->write_field<3>("AIRDEN_DRY");
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

        double* pmid_ptr = state->write_field<3>("PMID");

        require_field_pointer("CarbChem", "AIRDEN_DRY", airden_ptr);
        require_field_pointer("CarbChem", "DELP", delp_ptr);
        require_field_pointer("CarbChem", "PMID", pmid_ptr);

        // 2. Diagnostic Views
        double* diag_prod_mass = nullptr;
        double* diag_loss_flux = nullptr;
        double* diag_phobic_mass = nullptr;
        double* diag_phobic_flux = nullptr;

        if (state->diagnostic_manager() && diagnostics_enabled) {
            diag_prod_mass = (double*)state->diagnostic_manager()->get_host_pointer("carbchem_prod_mass");
            diag_loss_flux = (double*)state->diagnostic_manager()->get_host_pointer("carbchem_loss_flux");
            diag_phobic_mass = (double*)state->diagnostic_manager()->get_host_pointer("carbchem_phobic_mass");
            diag_phobic_flux = (double*)state->diagnostic_manager()->get_host_pointer("carbchem_phobic_flux");
        }

        double* conc_ptr = state->chemistry().conc ? state->chemistry().conc->host_write() : nullptr;
        require_field_pointer("CarbChem", "CHEM_CONC", conc_ptr);

        // Allocate local tendencies buffer
        std::vector<double> mock_tendency(state->column_count() * state->level_count() * state->species_count(), 0.0);

        // 4. Retrieve species properties from ChemState
        std::vector<double> t_chem_loss(state->species_count(), 1.0);
        for (size_t i = 0; i < state->chemistry().species_list.size(); ++i) {
            if (state->chemistry().species_list[i].t_chem_loss > 0.0) {
                t_chem_loss[i] = state->chemistry().species_list[i].t_chem_loss;
            }
        }

        // 5. Invoke flat science bridge
        run_carbchem_science_bridge(
            state->column_count(), state->level_count(), state->species_count(), state->clock().timestep, active_scheme.c_str(),
            diagnostics_enabled ? 1 : 0, state->clock().year, state->clock().month, state->clock().day, state->clock().hour,
            state->clock().minute, state->clock().second, airden_ptr, delp_ptr, pmid_ptr, t_chem_loss.data(),
            state->chemistry().species_names_c_arr.data(), conc_ptr, mock_tendency.data(), diag_prod_mass, diag_loss_flux,
            diag_phobic_mass, diag_phobic_flux, diagnostic_species_id.data(), diagnostic_species_id.size());

        if (state->chemistry().conc) state->chemistry().conc->mark_host_modified();
    }

    void CarbChemProcess::finalize() {}

} // namespace catchem

extern "C" {
void catchem_register_carbchem_cpp() {
    catchem::ProcessRegistry::get_instance().register_process(
        "carbchem", []() { return std::make_shared<catchem::CarbChemProcess>(); });
}
}

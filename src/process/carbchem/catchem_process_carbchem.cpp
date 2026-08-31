#include "catchem_process_carbchem.hpp"
#include "catchem_diagnostic_manager.hpp"
#include "catchem_error.hpp"
#include "catchem_process_registry.hpp"
#include <algorithm>
#include <iostream>

extern "C" {
void run_carbchem_science_bridge(int n_cols, int n_levels, int n_species, double dt, const char* active_scheme,
                                 int diagnostics, double gocart_time_days_hydrophobic_to_hydrophilic,
                                 int year, int month, int day, int hour, int minute, int second,
                                 double* airden, double* delp, double* pmid, double* species_t_chem_loss,
                                 const char* species_names_char, double* conc, double* tendency, double* diag_prod_mass,
                                 double* diag_loss_flux, double* diag_phobic_mass, double* diag_phobic_flux,
                                 const int* diagnostic_species_id, int n_diag_species);
}

namespace catchem {

    ProcessContract CarbChemProcess::get_contract() const {
        return {get_name(),
                {host_field_3d("PMID", "Pa"), host_field_interface("PEDGE", "Pa"), host_field_3d("T", "K"),
                 host_field_3d("DELP", "Pa"), host_field_3d("AIRDEN_DRY", "kg/m3"), host_concentration()},
                {}};
    }

    CarbChemProcess::CarbChemProcess() : active_scheme("gocart"), diagnostics_enabled(true) {}

    void CarbChemProcess::prepare_inputs(std::shared_ptr<StateManager> state) {
        state->derive_delp();
        state->derive_airden_dry();
    }

    void CarbChemProcess::init(std::shared_ptr<StateManager> state) {
        const auto config = state->config_manager();
        if (!config)
            throw std::invalid_argument("CarbChem requires a runtime YAML configuration");
        const auto configured = config->data.processes.find("carbchem");
        if (configured == config->data.processes.end() || configured->second.scheme != "gocart")
            throw std::invalid_argument("CarbChem requires processes.carbchem.scheme: gocart");
        diagnostics_enabled = configured->second.diagnostics;

        // Read scheme tuning options from the runtime YAML.  The lookup falls
        // back to the compiled default declared in CarbChemCommon_Mod.F90, so
        // a configuration that omits the option keeps current behavior.
        gocart_time_days = configured->second.get_double("gocart/time_days_hydrophobic_to_hydrophilic",
                                                         gocart_time_days);
        if (!(gocart_time_days > 0.0))
            throw std::invalid_argument("CarbChem gocart time_days_hydrophobic_to_hydrophilic must be positive");
        // 1. Setup diagnostic species ID dynamically (using a dummy is_carbchem flag if we had one, but we map all
        // indices here for simplicity since CarbChem filters internally)
        for (size_t i = 0; i < state->chemistry().species_list.size(); ++i) {
            diagnostic_species_id.push_back(i + 1); // 1-based for Fortran bridge
        }

        if (!diagnostics_enabled)
            return;
        // 2. Register C++ Diagnostic fields
        std::vector<int> dims_3d = {state->column_count(), state->level_count(), state->species_count()};
        std::vector<int> dims_2d = {state->column_count(), state->species_count()};

        state->diagnostic_manager()->register_field("carbchem_prod_mass", "Carbon Chemistry Production Mass", "kg/kg",
                                                    DiagType::FIELD_3D, dims_3d);
        state->diagnostic_manager()->register_field("carbchem_loss_flux", "Carbon Chemistry Loss Flux", "kg/m2/s",
                                                    DiagType::FIELD_2D, dims_2d);
        state->diagnostic_manager()->register_field("carbchem_phobic_mass", "Carbon Chemistry Phobic to Philic Mass",
                                                    "kg/kg", DiagType::FIELD_3D, dims_3d);
        state->diagnostic_manager()->register_field("carbchem_phobic_flux", "Carbon Chemistry Phobic to Philic Flux",
                                                    "kg/m2/s", DiagType::FIELD_2D, dims_2d);
    }

    void CarbChemProcess::run(std::shared_ptr<StateManager> state) {

        // 1. Retrieve 3D Meteorological state pointers
        double* airden_ptr = state->write_field<3>("AIRDEN_DRY");

        double* delp_ptr = state->write_field<3>("DELP");

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
        // Preserve negative values: GOCART's carbonChemLoss uses tChemLoss<0 as
        // its own "loss disabled" sentinel and early-returns on it; clamping to
        // 0 here instead makes it compute exp(-cdt/0), annihilating the species.
        std::vector<double> t_chem_loss(state->species_count(), -1.0);
        for (size_t i = 0; i < state->chemistry().species_list.size(); ++i) {
            t_chem_loss[i] = state->chemistry().species_list[i].t_chem_loss;
        }

        // 5. Invoke flat science bridge
        run_carbchem_science_bridge(state->column_count(), state->level_count(), state->species_count(),
                                    state->clock().timestep, active_scheme.c_str(), diagnostics_enabled ? 1 : 0,
                                    gocart_time_days,
                                    state->clock().year, state->clock().month, state->clock().day, state->clock().hour,
                                    state->clock().minute, state->clock().second, airden_ptr, delp_ptr, pmid_ptr,
                                    t_chem_loss.data(), state->chemistry().species_names_c_arr.data(), conc_ptr,
                                    mock_tendency.data(), diag_prod_mass, diag_loss_flux, diag_phobic_mass,
                                    diag_phobic_flux, diagnostic_species_id.data(), diagnostic_species_id.size());

        if (state->chemistry().conc)
            state->chemistry().conc->mark_host_modified();
    }

    void CarbChemProcess::finalize() {}

} // namespace catchem

extern "C" {
void catchem_register_carbchem_cpp() {
    catchem::ProcessRegistry::get_instance().register_process(
        "carbchem", []() { return std::make_shared<catchem::CarbChemProcess>(); }, {},
        catchem::make_settings_validator("carbchem", {"gocart/time_days_hydrophobic_to_hydrophilic"}));
}
}

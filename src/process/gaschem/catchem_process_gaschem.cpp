// src/process/gaschem/catchem_process_gaschem.cpp
#include "catchem_process_gaschem.hpp"
#include "catchem_diagnostic_manager.hpp"
#include "catchem_process_registry.hpp"
#include <algorithm>
#include <cctype>
#include <iostream>
#include <yaml-cpp/yaml.h>

namespace catchem {

    GasChemProcess::GasChemProcess() = default;
    GasChemProcess::~GasChemProcess() = default;

    void GasChemProcess::init(std::shared_ptr<StateManager> state) {
        std::cout << "DEBUG: GasChemProcess::init started" << std::endl;

        // 1. Resolve configuration directory path dynamically
        if (state->config_mgr) {
            try {
                YAML::Node gas_node = state->config_mgr->get_process_config("gaschem");
                if (gas_node.IsDefined() && gas_node["config_dir"]) {
                    this->config_dir = gas_node["config_dir"].as<std::string>();
                }
            } catch (const std::exception& e) {
                std::cerr << "GasChemProcess: Warning: failed to parse config from ConfigManager: " << e.what() << std::endl;
            }
        } else if (!state->config_file_path.empty()) {
            try {
                YAML::Node main_config = YAML::LoadFile(state->config_file_path);
                if (main_config["process"] && main_config["process"]["gaschem"]) {
                    auto gas_node = main_config["process"]["gaschem"];
                    if (gas_node["config_dir"]) {
                        this->config_dir = gas_node["config_dir"].as<std::string>();
                    }
                }
            } catch (const std::exception& e) {
                std::cerr << "GasChemProcess: Warning: failed to parse main config for gaschem: " << e.what()
                          << std::endl;
            }
        }

        if (this->config_dir.empty()) {
            if (!state->config_file_path.empty()) {
                std::string path = state->config_file_path;
                size_t last_slash = path.find_last_of("/\\");
                if (last_slash != std::string::npos) {
                    this->config_dir = path.substr(0, last_slash + 1);
                } else {
                    this->config_dir = "./";
                }
            } else {
                this->config_dir = "tests/Configs/Default/";
            }
        }

        std::cout << "DEBUG: GasChemProcess config directory resolved: " << config_dir << std::endl;

        // 2. Initialize MICM and State using musica library
        try {
            micm_instance = std::make_unique<musica::MICM>(config_dir, musica::RosenbrockStandardOrder);
            micm_state = std::make_unique<musica::State>(*micm_instance, state->n_cols * state->n_levels);
            initialized = true;
            std::cout << "DEBUG: GasChemProcess initialized MICM successfully!" << std::endl;
        } catch (const std::exception& e) {
            std::cerr << "GasChemProcess: Error: failed to initialize MICM: " << e.what() << std::endl;
            initialized = false;
        }
    }

    void GasChemProcess::run(std::shared_ptr<StateManager> state) {
        if (!initialized) {
            std::cerr << "GasChemProcess: Warning: skipped run because solver is not initialized." << std::endl;
            return;
        }

        // 1. Sync device to host
        state->sync_to_host();

        auto temp = state->met.T->host_view;
        auto pmid = state->met.PMID->host_view;
        auto airden_dry = state->met.AIRDEN_DRY->host_view;
        auto conc = state->chem.conc->host_view;

        auto& micm_conditions = micm_state->GetConditions();
        auto& micm_concs = micm_state->GetOrderedConcentrations();
        auto& micm_rate_params = micm_state->GetOrderedRateParameters();

        int nc = state->n_cols;
        int nl = state->n_levels;
        int ns = state->n_species;

        size_t vector_size_ = micm_instance->GetVectorSize();
        auto variable_map = micm_state->GetVariableMap();
        size_t n_micm_species = variable_map.size();

        auto rate_param_map = micm_state->GetRateParameterMap();
        size_t n_rate_params = rate_param_map.size();

        // Dry air molecular weight in kg/mol
        const double air_mw_kg = 0.0289644;

        // 2. Map environmental variables and input concentrations to state
        for (int ilev = 0; ilev < nl; ++ilev) {
            for (int icol = 0; icol < nc; ++icol) {
                int i_cell = ilev * nc + icol;

                double t_val = temp(icol, ilev, 0);
                double p_val = pmid(icol, ilev, 0);
                double density_dry_kg = airden_dry(icol, ilev, 0);

                // Standard boundary assertions
                if (t_val <= 0.0)
                    t_val = 298.15;
                if (p_val <= 0.0)
                    p_val = 101325.0;
                if (density_dry_kg <= 0.0)
                    density_dry_kg = 1.2;

                // Convert dry air density: kg/m3 to mol/m3
                double air_density_mol = density_dry_kg / air_mw_kg;

                micm_conditions[i_cell].temperature_ = t_val;
                micm_conditions[i_cell].pressure_ = p_val;
                micm_conditions[i_cell].air_density_ = air_density_mol;

                // Copy concentrations: ppmv -> mol/m3
                for (int ispec = 0; ispec < ns; ++ispec) {
                    std::string name = state->chem.species_list[ispec].short_name;
                    for (auto& c : name)
                        c = std::toupper(c);

                    auto it = variable_map.find(name);
                    if (it != variable_map.end()) {
                        size_t i_micm_spec = it->second;
                        double ppmv_val = conc(icol, ilev, ispec);
                        if (ppmv_val < 0.0)
                            ppmv_val = 1.0e-20; // Safe bounding to prevent NaN

                        double conc_molar = ppmv_val * 1.0e-6 * air_density_mol;

                        size_t group_index = i_cell / vector_size_;
                        size_t row_in_group = i_cell % vector_size_;
                        size_t idx = (group_index * n_micm_species + i_micm_spec) * vector_size_ + row_in_group;
                        micm_concs[idx] = conc_molar;
                    }
                }

                // 3. Dynamic Photolysis Mapping (PHOTO.<label> to photolysis_rate_<label>)
                for (const auto& [param_name, i_param] : rate_param_map) {
                    size_t group_index = i_cell / vector_size_;
                    size_t row_in_group = i_cell % vector_size_;
                    size_t idx = (group_index * n_rate_params + i_param) * vector_size_ + row_in_group;

                    if (param_name.rfind("PHOTO.", 0) == 0) {
                        std::string label = param_name.substr(6);
                        std::string diag_name = "photolysis_rate_" + label;

                        double rate_val = 0.0;
                        if (state->diag_mgr && state->diag_mgr->has_field(diag_name)) {
                            double* diag_ptr = static_cast<double*>(state->diag_mgr->get_host_pointer(diag_name));
                            if (diag_ptr) {
                                int diag_idx = ilev * nc + icol;
                                rate_val = diag_ptr[diag_idx];
                            }
                        }
                        micm_rate_params[idx] = rate_val;
                    } else if (param_name.rfind("LOSS.", 0) == 0) {
                        micm_rate_params[idx] = 1.0;
                    }
                }
            }
        }

        // 4. Run standard CPU solver
        double tstep = state->time.timestep;
        if (tstep <= 0.0)
            tstep = 3600.0; // fallback default
        auto solver_result = micm_instance->Solve(micm_state.get(), tstep);
        if (solver_result.state_ != micm::SolverState::Converged &&
            solver_result.state_ != micm::SolverState::AcceptingUnconvergedIntegration) {
            std::cerr << "GasChemProcess: Warning: MICM Solver did not reach convergence! Final state: "
                      << micm::SolverStateToString(solver_result.state_) << std::endl;
        }

        // 5. Convert output concentrations back: mol/m3 -> ppmv
        for (int ilev = 0; ilev < nl; ++ilev) {
            for (int icol = 0; icol < nc; ++icol) {
                int i_cell = ilev * nc + icol;
                double air_density_mol = micm_conditions[i_cell].air_density_;

                for (int ispec = 0; ispec < ns; ++ispec) {
                    std::string name = state->chem.species_list[ispec].short_name;
                    for (auto& c : name)
                        c = std::toupper(c);

                    auto it = variable_map.find(name);
                    if (it != variable_map.end()) {
                        size_t i_micm_spec = it->second;

                        size_t group_index = i_cell / vector_size_;
                        size_t row_in_group = i_cell % vector_size_;
                        size_t idx = (group_index * n_micm_species + i_micm_spec) * vector_size_ + row_in_group;
                        double conc_molar = micm_concs[idx];

                        double ppmv_val = (conc_molar / air_density_mol) * 1.0e6;
                        if (ppmv_val < 0.0)
                            ppmv_val = 1.0e-20;
                        conc(icol, ilev, ispec) = ppmv_val;
                    }
                }
            }
        }

        // 6. Sync back to device
        state->sync_to_device();
    }

    void GasChemProcess::finalize() {}

} // namespace catchem

void catchem_register_gaschem_cpp() {
    catchem::ProcessRegistry::get_instance().register_process(
        "gaschem", []() { return std::make_shared<catchem::GasChemProcess>(); });
}

// src/process/photolysis/catchem_process_photolysis.cpp
#include "catchem_process_photolysis.hpp"
#include "catchem_process_registry.hpp"
#include "catchem_diagnostic_manager.hpp"
#include <yaml-cpp/yaml.h>
#include <cmath>
#include <algorithm>
#include <iostream>

namespace catchem {

    PhotolysisProcess::PhotolysisProcess() : config_path("") {}
    PhotolysisProcess::~PhotolysisProcess() = default;

    void PhotolysisProcess::init(std::shared_ptr<StateManager> state) {
        if (!state->config_file_path.empty()) {
            try {
                YAML::Node main_config = YAML::LoadFile(state->config_file_path);
                if (main_config["process"] && main_config["process"]["photolysis"]) {
                    auto photo_node = main_config["process"]["photolysis"];
                    if (photo_node["config_file"]) {
                        this->config_path = photo_node["config_file"].as<std::string>();
                    }
                }
            } catch (const std::exception& e) {
                std::cerr << "PhotolysisProcess: Warning: failed to parse main config: " << e.what() << std::endl;
            }
        }

        if (this->config_path.empty()) {
            this->config_path = "src/external/musica/configs/tuvx/tuv_5_4.yml";
        }

        musica::Error err;
        std::unique_ptr<musica::GridMap> grids(musica::CreateGridMap(&err));
        std::unique_ptr<musica::ProfileMap> profiles(musica::CreateProfileMap(&err));
        std::unique_ptr<musica::RadiatorMap> radiators(musica::CreateRadiatorMap(&err));

        tuvx_instance = std::make_unique<musica::TUVX>();
        tuvx_instance->Create(config_path.c_str(), grids.get(), profiles.get(), radiators.get(), &err);

        if (err.status_ != 0) {
            std::cerr << "PhotolysisProcess: Error: Failed to initialize TUV-x! " << err.message_ << std::endl;
            return;
        }

        tuvx_instance->GetPhotolysisRateConstantsOrdering(&photo_mappings, &err);

        if (state->diag_mgr) {
            std::vector<int> dims_2d = {state->n_cols, state->n_levels};
            for (size_t i = 0; i < photo_mappings.size_; ++i) {
                std::string rx_name = photo_mappings.mappings_[i].name_;
                state->diag_mgr->register_field("photolysis_rate_" + rx_name, 
                                                "Photolysis rate for " + rx_name, 
                                                "s-1", DiagType::FIELD_2D, dims_2d);
            }
        }
    }

    void PhotolysisProcess::run(std::shared_ptr<StateManager> state) {
        if (!tuvx_instance) return;

        state->sync_to_host();

        int i_o3 = -1;
        for (size_t i = 0; i < state->chem.species_list.size(); ++i) {
            if (state->chem.species_list[i].short_name == "O3") {
                i_o3 = i;
                break;
            }
        }

        musica::Error err;
        int num_reactions = tuvx_instance->GetPhotolysisRateConstantCount();
        
        std::unique_ptr<musica::ProfileMap> profiles(tuvx_instance->GetProfileMap(&err));
        if (err.status_ != 0) {
            std::cerr << "PhotolysisProcess: Error getting ProfileMap: " << err.message_ << std::endl;
            return;
        }
        
        musica::Profile* profile_air = profiles->GetProfile("air", "molecule cm-3", &err);
        musica::Profile* profile_o2  = profiles->GetProfile("O2", "molecule cm-3", &err);
        musica::Profile* profile_o3  = profiles->GetProfile("O3", "molecule cm-3", &err);

        std::vector<double> air_profile(state->n_levels, 0.0);
        std::vector<double> o2_profile(state->n_levels, 0.0);
        std::vector<double> o3_profile(state->n_levels, 0.0);

        for (int i_col = 0; i_col < state->n_cols; ++i_col) {
            double lat_deg = state->met.LAT ? state->met.LAT->host_view(i_col, 0) : 40.0;
            double lon_deg = state->met.LON ? state->met.LON->host_view(i_col, 0) : -105.0;
            double cos_sza = state->time.get_cos_sza(lat_deg, lon_deg, true);
            double sza_rad = std::acos(std::max(-1.0, std::min(1.0, cos_sza)));

            for (int i_lvl = 0; i_lvl < state->n_levels; ++i_lvl) {
                double airden_kg_m3 = state->met.AIRDEN ? state->met.AIRDEN->host_view(i_col, i_lvl, 0) : 1.2;
                // 2.079153e19 is the conversion factor: (1000.0 / 28.9644) * 6.02214e23 / 1e6
                air_profile[i_lvl] = airden_kg_m3 * 2.079153e19;
                o2_profile[i_lvl] = air_profile[i_lvl] * 0.2095;

                if (i_o3 >= 0 && state->chem.conc) {
                    o3_profile[i_lvl] = state->chem.conc->host_view(i_col, i_lvl, i_o3);
                } else {
                    o3_profile[i_lvl] = air_profile[i_lvl] * 3e-7; // default climatology O3 mixing ratio
                }
            }

            profile_air->SetMidpointValues(air_profile.data(), state->n_levels, &err);
            profile_o2->SetMidpointValues(o2_profile.data(), state->n_levels, &err);
            profile_o3->SetMidpointValues(o3_profile.data(), state->n_levels, &err);

            std::vector<double> edge_photolysis_rates((state->n_levels + 1) * num_reactions, 0.0);
            std::vector<double> edge_heating_rates((state->n_levels + 1) * tuvx_instance->GetHeatingRateCount(), 0.0);

            tuvx_instance->Run(
                sza_rad, 
                1.0, // Earth-sun distance
                edge_photolysis_rates.data(),
                edge_heating_rates.data(),
                nullptr, 
                nullptr, 
                nullptr, 
                &err);

            if (err.status_ != 0) {
                std::cerr << "PhotolysisProcess: Solver error in column " << i_col << ": " << err.message_ << std::endl;
                continue;
            }

            if (state->diag_mgr) {
                for (size_t rx_idx = 0; rx_idx < photo_mappings.size_; ++rx_idx) {
                    std::string rx_name = photo_mappings.mappings_[rx_idx].name_;
                    std::string diag_name = "photolysis_rate_" + rx_name;
                    double* diag_ptr = static_cast<double*>(state->diag_mgr->get_host_pointer(diag_name));

                    if (diag_ptr) {
                        for (int i_lvl = 0; i_lvl < state->n_levels; ++i_lvl) {
                            int idx_edge1 = rx_idx * (state->n_levels + 1) + i_lvl;
                            int idx_edge2 = rx_idx * (state->n_levels + 1) + (i_lvl + 1);

                            double rate_midpoint = 0.5 * (edge_photolysis_rates[idx_edge1] + edge_photolysis_rates[idx_edge2]);

                            int diag_idx = i_lvl * state->n_cols + i_col;
                            diag_ptr[diag_idx] = rate_midpoint;
                        }
                    }
                }
            }
        }

        state->sync_to_device();
        if (state->diag_mgr) {
            state->diag_mgr->sync_to_device();
        }
    }

    void PhotolysisProcess::finalize() {
        // Cleanup resources
    }

} // namespace catchem

extern "C" {
void catchem_register_photolysis_cpp() {
    catchem::ProcessRegistry::get_instance().register_process(
        "photolysis", []() { return std::make_shared<catchem::PhotolysisProcess>(); });
}
}

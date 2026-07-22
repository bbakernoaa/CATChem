// src/process/photolysis/catchem_process_photolysis.cpp
#include "catchem_process_photolysis.hpp"
#include "catchem_diagnostic_manager.hpp"
#include "catchem_process_registry.hpp"
#include <algorithm>
#include <cmath>
#include <iostream>
#include <musica/tuvx/grid.hpp>
#include <musica/tuvx/grid_map.hpp>
#include <musica/tuvx/profile.hpp>
#include <musica/tuvx/profile_map.hpp>
#include <musica/tuvx/radiator_map.hpp>
#include <musica/tuvx/tuvx_c_interface.hpp>
#include <unordered_set>
#include <yaml-cpp/yaml.h>

namespace catchem {

    PhotolysisProcess::PhotolysisProcess() : config_path("") {}
    PhotolysisProcess::~PhotolysisProcess() = default;

    void PhotolysisProcess::init(std::shared_ptr<StateManager> state) {
        std::cout << "DEBUG: PhotolysisProcess::init started" << std::endl;
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

        std::cout << "DEBUG: Parsing TUV-x config file to check pre-defined profiles: " << config_path << std::endl;
        std::unordered_set<std::string> config_defined_profiles;
        try {
            YAML::Node tuvx_config = YAML::LoadFile(config_path);
            if (tuvx_config["profiles"]) {
                for (const auto& p : tuvx_config["profiles"]) {
                    if (p["name"]) {
                        config_defined_profiles.insert(p["name"].as<std::string>());
                    }
                }
            }
        } catch (const std::exception& e) {
            std::cout << "DEBUG: YAML parse warning: " << e.what() << std::endl;
        }

        std::cout << "DEBUG: Creating GridMap, ProfileMap, and RadiatorMap" << std::endl;
        musica::Error err;
        grids = musica::CreateGridMap(&err);
        profiles = musica::CreateProfileMap(&err);
        radiators = musica::CreateRadiatorMap(&err);

        std::cout << "DEBUG: Creating height grid" << std::endl;
        musica::Grid* height_grid = musica::CreateGrid("height", "km", state->n_levels, &err);
        std::vector<double> dummy_edges(state->n_levels + 1, 0.0);
        std::vector<double> dummy_mids(state->n_levels, 0.0);
        for (int i = 0; i <= state->n_levels; ++i) {
            dummy_edges[i] = i * 1.0;
            if (i < state->n_levels) {
                dummy_mids[i] = i * 1.0 + 0.5;
            }
        }
        musica::SetGridEdges(height_grid, dummy_edges.data(), dummy_edges.size(), &err);
        musica::SetGridMidpoints(height_grid, dummy_mids.data(), dummy_mids.size(), &err);

        std::cout << "DEBUG: Adding height grid to GridMap" << std::endl;
        musica::AddGrid(grids, height_grid, &err);

        std::cout << "DEBUG: Selecting and configuring wavelength grid" << std::endl;
        int wl_sections = 156;
        std::vector<double> wl_edges;

        if (config_path.find("from_host") != std::string::npos ||
            config_path.find("config.json") != std::string::npos) {
            wl_sections = 5;
            wl_edges = {300.0, 400.0, 500.0, 600.0, 700.0, 800.0};
        } else {
            wl_sections = 156;
            wl_edges = {
                120.0000, 121.4000, 121.9000, 122.3000, 123.1000, 123.8000, 124.6000, 125.4000, 126.2000, 127.0000,
                128.6000, 129.4000, 130.3000, 132.0000, 135.0000, 137.0000, 145.0000, 155.0000, 165.0000, 170.0000,
                175.4000, 177.0000, 178.6000, 180.2000, 181.8000, 183.5000, 185.2000, 186.9000, 188.7000, 190.5000,
                192.3000, 194.2000, 196.1000, 198.0000, 200.0000, 202.0000, 204.1000, 206.2000, 208.3330, 210.5260,
                212.7660, 215.0540, 217.3910, 219.7800, 222.2220, 224.7190, 227.2730, 229.8850, 232.5580, 235.2940,
                238.0950, 240.9640, 243.9020, 246.9140, 250.0000, 253.1650, 256.4100, 259.7400, 263.1580, 266.6670,
                270.2700, 273.9730, 277.7780, 281.6900, 285.7140, 289.8550, 294.1180, 298.5000, 302.5000, 303.5000,
                304.5000, 305.5000, 306.5000, 307.5000, 308.5000, 309.5000, 310.5000, 311.5000, 312.5000, 313.5000,
                314.5000, 317.5000, 322.5000, 327.5000, 332.5000, 337.5000, 342.5000, 347.5000, 352.5000, 357.5000,
                362.5000, 367.5000, 372.5000, 377.5000, 382.5000, 387.5000, 392.5000, 397.5000, 402.5000, 407.5000,
                412.5000, 417.5000, 422.5000, 427.5000, 432.5000, 437.5000, 442.5000, 447.5000, 452.5000, 457.5000,
                462.5000, 467.5000, 472.5000, 477.5000, 482.5000, 487.5000, 492.5000, 497.5000, 502.5000, 507.5000,
                512.5000, 517.5000, 522.5000, 527.5000, 532.5000, 537.5000, 542.5000, 547.5000, 552.5000, 557.5000,
                562.5000, 567.5000, 572.5000, 577.5000, 582.5000, 587.5000, 592.5000, 597.5000, 602.5000, 607.5000,
                612.5000, 617.5000, 622.5000, 627.5000, 632.5000, 637.5000, 642.5000, 647.1000, 655.0000, 665.0000,
                675.0000, 685.0000, 695.0000, 705.0000, 715.0000, 725.0000, 735.0000};
        }

        std::cout << "DEBUG: Creating wavelength grid with " << wl_sections << " sections" << std::endl;
        musica::Grid* wl_grid = musica::CreateGrid("wavelength", "nm", wl_sections, &err);
        std::vector<double> wl_mids(wl_sections, 0.0);
        for (int i = 0; i < wl_sections; ++i) {
            wl_mids[i] = 0.5 * (wl_edges[i] + wl_edges[i + 1]);
        }
        musica::SetGridEdges(wl_grid, wl_edges.data(), wl_edges.size(), &err);
        musica::SetGridMidpoints(wl_grid, wl_mids.data(), wl_mids.size(), &err);

        std::cout << "DEBUG: Adding wavelength grid to GridMap" << std::endl;
        musica::AddGrid(grids, wl_grid, &err);

        // 3. Helper to register profiles safely only if missing from the config file definition
        auto register_profile_if_missing = [&](const char* name, const char* units, musica::Grid* grid,
                                               double default_val, std::size_t num_vals) {
            if (config_defined_profiles.find(name) == config_defined_profiles.end()) {
                std::cout << "DEBUG: Pre-registering missing profile: " << name << " (" << units << ")" << std::endl;
                musica::Profile* new_prof = musica::CreateProfile(name, units, grid, &err);
                std::vector<double> dummy(num_vals, default_val);
                musica::SetProfileMidpointValues(new_prof, dummy.data(), num_vals, &err);
                musica::AddProfile(profiles, new_prof, &err);
                musica::DeleteProfile(new_prof, &err);
            }
        };

        register_profile_if_missing("temperature", "K", height_grid, 280.0, state->n_levels);
        register_profile_if_missing("air", "molecule cm-3", height_grid, 1e12, state->n_levels);
        register_profile_if_missing("O2", "molecule cm-3", height_grid, 1e12, state->n_levels);
        register_profile_if_missing("O3", "molecule cm-3", height_grid, 1e12, state->n_levels);
        register_profile_if_missing("surface albedo", "none", wl_grid, 0.1, wl_sections);
        register_profile_if_missing("extraterrestrial flux", "photon cm-2 s-1", wl_grid, 1.5e14, wl_sections);

        // 4. Safely delete local grids as they are cloned/owned inside the GridMap
        std::cout << "DEBUG: Deleting local height and wavelength grid pointers" << std::endl;
        musica::DeleteGrid(height_grid, &err);
        musica::DeleteGrid(wl_grid, &err);

        // 5. Create TUVX instance using C API
        std::cout << "DEBUG: Calling musica::CreateTuvx with config_path: " << config_path << std::endl;
        tuvx_instance = musica::CreateTuvx(config_path.c_str(), grids, profiles, radiators, &err);

        if (err.code_ != 0) {
            std::cerr << "PhotolysisProcess: Error: Failed to initialize TUV-x! "
                      << (err.message_.value_ ? err.message_.value_ : "Unknown Error") << std::endl;
            return;
        }

        std::cout << "DEBUG: Getting Photolysis rate constants ordering" << std::endl;
        musica::GetPhotolysisRateConstantsOrdering(tuvx_instance, &photo_mappings, &err);

        std::cout << "DEBUG: Dynamic diagnostic field registration" << std::endl;
        if (state->diag_mgr) {
            std::vector<int> dims_2d = {state->n_cols, state->n_levels};
            for (size_t i = 0; i < photo_mappings.size_; ++i) {
                std::string rx_name =
                    photo_mappings.mappings_[i].name_.value_ ? photo_mappings.mappings_[i].name_.value_ : "";
                state->diag_mgr->register_field("photolysis_rate_" + rx_name, "Photolysis rate for " + rx_name, "s-1",
                                                DiagType::FIELD_2D, dims_2d);
            }
        }
        std::cout << "DEBUG: PhotolysisProcess::init complete" << std::endl;
    }

    void PhotolysisProcess::run(std::shared_ptr<StateManager> state) {
        std::cout << "DEBUG: PhotolysisProcess::run started" << std::endl;
        if (!tuvx_instance) {
            std::cout << "DEBUG: tuvx_instance is NULL, returning" << std::endl;
            return;
        }

        std::cout << "DEBUG: Syncing state to host" << std::endl;
        state->sync_to_host();

        std::cout << "DEBUG: Locating Ozone index" << std::endl;
        int i_o3 = -1;
        for (size_t i = 0; i < state->chem.species_list.size(); ++i) {
            if (state->chem.species_list[i].short_name == "O3") {
                i_o3 = i;
                break;
            }
        }
        std::cout << "DEBUG: Ozone index = " << i_o3 << std::endl;

        musica::Error err;
        int num_reactions = photo_mappings.size_;
        std::cout << "DEBUG: Number of photolysis reactions mapped = " << num_reactions << std::endl;

        std::cout << "DEBUG: Fetching ProfileMap from TUVX instance" << std::endl;
        musica::ProfileMap* loaded_profiles = musica::GetProfileMap(tuvx_instance, &err);
        if (err.code_ != 0) {
            std::cerr << "PhotolysisProcess: Error getting ProfileMap: "
                      << (err.message_.value_ ? err.message_.value_ : "Unknown Error") << std::endl;
            return;
        }

        std::cout << "DEBUG: Retrieving individual Profile pointers" << std::endl;
        musica::Profile* profile_air = musica::GetProfile(loaded_profiles, "air", "molecule cm-3", &err);
        musica::Profile* profile_o2 = musica::GetProfile(loaded_profiles, "O2", "molecule cm-3", &err);
        musica::Profile* profile_o3 = musica::GetProfile(loaded_profiles, "O3", "molecule cm-3", &err);
        musica::Profile* profile_temp = musica::GetProfile(loaded_profiles, "temperature", "K", &err);

        std::cout << "DEBUG: profile_air = " << profile_air << ", profile_o2 = " << profile_o2
                  << ", profile_o3 = " << profile_o3 << ", profile_temp = " << profile_temp << std::endl;

        if (profile_air) {
            std::cout << "DEBUG: profile_air name = " << profile_air->GetName(&err) << std::endl;
        }
        if (profile_o2) {
            std::cout << "DEBUG: profile_o2 name = " << profile_o2->GetName(&err) << std::endl;
        }

        std::cout << "DEBUG: Retrieving GridMap from TUVX instance" << std::endl;
        musica::GridMap* loaded_grids = musica::GetGridMap(tuvx_instance, &err);
        musica::Grid* height_grid = musica::GetGrid(loaded_grids, "height", "km", &err);
        std::cout << "DEBUG: height_grid = " << height_grid << std::endl;

        std::vector<double> height_edges(state->n_levels + 1, 0.0);
        std::vector<double> air_profile(state->n_levels, 0.0);
        std::vector<double> o2_profile(state->n_levels, 0.0);
        std::vector<double> o3_profile(state->n_levels, 0.0);
        std::vector<double> temp_profile(state->n_levels, 0.0);

        std::cout << "DEBUG: Starting column-wise calculation loop" << std::endl;
        for (int i_col = 0; i_col < state->n_cols; ++i_col) {
            std::cout << "DEBUG: Column " << i_col << ": calculating SZA" << std::endl;
            double lat_deg = state->met.LAT ? state->met.LAT->host_view(i_col, 0) : 40.0;
            double lon_deg = state->met.LON ? state->met.LON->host_view(i_col, 0) : -105.0;
            double cos_sza = state->time.get_cos_sza(lat_deg, lon_deg, true);
            double sza_rad = std::acos(std::max(-1.0, std::min(1.0, cos_sza)));

            std::cout << "DEBUG: Column " << i_col << ": updating grid height edges" << std::endl;
            height_edges[0] = 0.0;
            for (int i_lvl = 0; i_lvl < state->n_levels; ++i_lvl) {
                double dz_m = state->met.BXHEIGHT ? state->met.BXHEIGHT->host_view(i_col, i_lvl, 0) : 100.0;
                height_edges[i_lvl + 1] = height_edges[i_lvl] + dz_m / 1000.0;
            }
            if (height_grid) {
                musica::SetGridEdges(height_grid, height_edges.data(), height_edges.size(), &err);
            }

            std::cout << "DEBUG: Column " << i_col << ": populating profile midpoint vectors" << std::endl;
            for (int i_lvl = 0; i_lvl < state->n_levels; ++i_lvl) {
                double airden_kg_m3 = state->met.AIRDEN ? state->met.AIRDEN->host_view(i_col, i_lvl, 0) : 1.2;
                air_profile[i_lvl] = airden_kg_m3 * 2.079153e19;
                o2_profile[i_lvl] = air_profile[i_lvl] * 0.2095;
                temp_profile[i_lvl] = state->met.T ? state->met.T->host_view(i_col, i_lvl, 0) : 280.0;

                if (i_o3 >= 0 && state->chem.conc) {
                    o3_profile[i_lvl] = state->chem.conc->host_view(i_col, i_lvl, i_o3);
                } else {
                    o3_profile[i_lvl] = air_profile[i_lvl] * 3e-7;
                }
            }

            std::cout << "DEBUG: Column " << i_col << ": updating profiles in TUVX" << std::endl;
            if (profile_air) {
                std::cout << "DEBUG: SetProfileMidpointValues for air" << std::endl;
                musica::SetProfileMidpointValues(profile_air, air_profile.data(), state->n_levels, &err);
            }
            if (profile_o2) {
                std::cout << "DEBUG: SetProfileMidpointValues for O2" << std::endl;
                musica::SetProfileMidpointValues(profile_o2, o2_profile.data(), state->n_levels, &err);
            }
            if (profile_o3) {
                std::cout << "DEBUG: SetProfileMidpointValues for O3" << std::endl;
                musica::SetProfileMidpointValues(profile_o3, o3_profile.data(), state->n_levels, &err);
            }
            if (profile_temp) {
                std::cout << "DEBUG: SetProfileMidpointValues for temperature" << std::endl;
                musica::SetProfileMidpointValues(profile_temp, temp_profile.data(), state->n_levels, &err);
            }

            std::vector<double> edge_photolysis_rates((state->n_levels + 1) * num_reactions, 0.0);
            std::vector<double> edge_heating_rates((state->n_levels + 1) * tuvx_instance->GetHeatingRateCount(), 0.0);

            std::cout << "DEBUG: Column " << i_col << ": calling musica::RunTuvx" << std::endl;
            musica::RunTuvx(tuvx_instance, sza_rad, 1.0, edge_photolysis_rates.data(), edge_heating_rates.data(),
                            nullptr, nullptr, nullptr, &err);

            if (err.code_ != 0) {
                std::cerr << "PhotolysisProcess: Solver error in column " << i_col << ": "
                          << (err.message_.value_ ? err.message_.value_ : "Unknown Error") << std::endl;
                continue;
            }

            std::cout << "DEBUG: Column " << i_col << ": copying midpoint-interpolated J-rates to diagnostics"
                      << std::endl;
            if (state->diag_mgr) {
                for (size_t rx_idx = 0; rx_idx < photo_mappings.size_; ++rx_idx) {
                    std::string rx_name = photo_mappings.mappings_[rx_idx].name_.value_
                                              ? photo_mappings.mappings_[rx_idx].name_.value_
                                              : "";
                    std::string diag_name = "photolysis_rate_" + rx_name;
                    double* diag_ptr = static_cast<double*>(state->diag_mgr->get_host_pointer(diag_name));

                    if (diag_ptr) {
                        for (int i_lvl = 0; i_lvl < state->n_levels; ++i_lvl) {
                            int idx_edge1 = rx_idx * (state->n_levels + 1) + i_lvl;
                            int idx_edge2 = rx_idx * (state->n_levels + 1) + (i_lvl + 1);

                            double rate_midpoint =
                                0.5 * (edge_photolysis_rates[idx_edge1] + edge_photolysis_rates[idx_edge2]);

                            int diag_idx = i_lvl * state->n_cols + i_col;
                            diag_ptr[diag_idx] = rate_midpoint;
                        }
                    }
                }
            }
        }

        std::cout << "DEBUG: Syncing diagnostics and state to device" << std::endl;
        state->sync_to_device();
        if (state->diag_mgr) {
            state->diag_mgr->sync_to_device();
        }
        std::cout << "DEBUG: PhotolysisProcess::run complete" << std::endl;
    }

    void PhotolysisProcess::finalize() {
        musica::Error err;
        if (tuvx_instance) {
            musica::DeleteTuvx(tuvx_instance, &err);
            tuvx_instance = nullptr;
        }
        if (grids) {
            musica::DeleteGridMap(grids, &err);
            grids = nullptr;
        }
        if (profiles) {
            musica::DeleteProfileMap(profiles, &err);
            profiles = nullptr;
        }
        if (radiators) {
            musica::DeleteRadiatorMap(radiators, &err);
            radiators = nullptr;
        }
    }

} // namespace catchem

extern "C" {
void catchem_register_photolysis_cpp() {
    catchem::ProcessRegistry::get_instance().register_process(
        "photolysis", []() { return std::make_shared<catchem::PhotolysisProcess>(); });
}
}

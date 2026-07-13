// src/process/photolysis/catchem_process_photolysis.cpp
#include "catchem_process_photolysis.hpp"
#include "catchem_process_registry.hpp"
#include "catchem_diagnostic_manager.hpp"
#include <yaml-cpp/yaml.h>
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
        std::cout << "DEBUG: PhotolysisProcess run" << std::endl;
    }

    void PhotolysisProcess::finalize() {
        std::cout << "DEBUG: PhotolysisProcess finalize" << std::endl;
    }

} // namespace catchem

extern "C" {
void catchem_register_photolysis_cpp() {
    catchem::ProcessRegistry::get_instance().register_process(
        "photolysis", []() { return std::make_shared<catchem::PhotolysisProcess>(); });
}
}

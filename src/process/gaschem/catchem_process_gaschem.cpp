// src/process/gaschem/catchem_process_gaschem.cpp
#include "catchem_process_gaschem.hpp"
#include "catchem_process_registry.hpp"
#include "catchem_diagnostic_manager.hpp"
#include <iostream>
#include <algorithm>
#include <yaml-cpp/yaml.h>

namespace catchem {

    GasChemProcess::GasChemProcess() = default;
    GasChemProcess::~GasChemProcess() = default;

    void GasChemProcess::init(std::shared_ptr<StateManager> state) {
        std::cout << "DEBUG: GasChemProcess::init started" << std::endl;
        
        // 1. Resolve configuration directory path dynamically
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
        // Implementation will be handled in subsequent tasks
    }

    void GasChemProcess::finalize() {}

} // namespace catchem

void catchem_register_gaschem_cpp() {
    catchem::ProcessRegistry::get_instance().register_process("gaschem", []() {
        return std::make_shared<catchem::GasChemProcess>();
    });
}

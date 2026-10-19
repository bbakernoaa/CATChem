// src/process/gaschem/catchem_process_gaschem.hpp
#pragma once
#include "catchem_process_interface.hpp"
#include <memory>
#include <musica/micm/micm.hpp>
#include <musica/micm/state.hpp>
#include <string>
#include <vector>

namespace catchem {

    /** @brief Executes one selected MUSICA mechanism over the imported CATChem state. */
    class GasChemProcess : public ProcessInterface {
    private:
        std::string config_dir;
        // Optional explicit single-file MUSICA mechanism (v1 format). When set, it
        // takes precedence over config_dir and is passed directly to MICM.
        std::string config_file;
        std::unique_ptr<musica::MICM> micm_instance;
        std::unique_ptr<musica::State> micm_state;
        std::vector<std::string> photolysis_products;
        bool initialized = false;

    public:
        /** @brief Construct an uninitialized GasChem process adapter. */
        GasChemProcess();
        /** @brief Release the owned MUSICA instance and solver state. */
        ~GasChemProcess() override;

        /** @brief Return the stable process registry name. */
        std::string get_name() const override { return std::string(ProcessNames::GasChem); }
        /** @brief Declare required meteorology and mechanism-derived products. */
        ProcessContract get_contract() const override;

        /** @brief Load and validate the configured MUSICA mechanism. */
        void init(std::shared_ptr<StateManager> state) override;
        /** @brief Advance the selected chemistry mechanism for the host interval. */
        void run(std::shared_ptr<StateManager> state) override;
        /** @brief Release process-owned solver resources. */
        void finalize() override;
    };

} // namespace catchem

extern "C" {
void catchem_register_gaschem_cpp();
}

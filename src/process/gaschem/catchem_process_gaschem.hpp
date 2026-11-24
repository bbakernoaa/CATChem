// src/process/gaschem/catchem_process_gaschem.hpp
/**
 * @file catchem_process_gaschem.hpp
 * @brief Declares the gas-phase chemistry process adapter around the MUSICA MICM solver.
 *
 * @details Unlike the Fortran-bridged processes, gaschem runs its science in
 * C++ through musica::MICM. The adapter owns the solver instance and its
 * State object for the lifetime between init() and finalize(); concentration
 * exchange happens through borrowed host views of the shared CONCENTRATION
 * field, converted ppmv ↔ mol m-3 at the solver boundary. Photolysis rate
 * parameters required by the mechanism (PHOTO.<species>) are consumed as
 * diagnostic products published by the photolysis process, so gaschem depends
 * on a fresh successful photolysis generation for every step.
 */
#pragma once
#include "catchem_process_interface.hpp"
#include <memory>
#include <musica/micm/micm.hpp>
#include <musica/micm/state.hpp>
#include <string>
#include <vector>

namespace catchem {

    /**
     * @brief Executes one selected MUSICA mechanism over the imported CATChem state.
     *
     * @details The mechanism is chosen by configuration: an explicit
     * `processes/gaschem/config_file` (single-file MUSICA v1 mechanism) takes
     * precedence over `processes/gaschem/config_dir` (v0 CAMP directory
     * layout); init() also auto-detects a config.yaml/config.yml entry point
     * inside the directory. init() builds the musica::MICM Rosenbrock solver
     * and a musica::State sized for column_count × level_count cells, and
     * derives the photolysis prerequisite list (products_required) from every
     * mechanism rate parameter named PHOTO.*.
     *
     * @see musica::MICM, PhotolysisProcess (producer of the
     * photolysis_rate_<label> diagnostics consumed in run()).
     */
    class GasChemProcess : public ProcessInterface {
    private:
        /// Resolved directory holding the CAMP (v0) mechanism configuration.
        std::string config_dir;
        // Optional explicit single-file MUSICA mechanism (v1 format). When set, it
        // takes precedence over config_dir and is passed directly to MICM.
        std::string config_file;
        /// Owned MICM solver built from the parsed mechanism; null until init() succeeds.
        std::unique_ptr<musica::MICM> micm_instance;
        /// Owned solver state (conditions, concentrations, rate parameters) sized nc*nl cells.
        std::unique_ptr<musica::State> micm_state;
        /// Diagnostic names 'photolysis_rate_<label>' for each mechanism PHOTO.<label>
        /// rate parameter; declared as required products in get_contract().
        std::vector<std::string> photolysis_products;
        /// True once init() has built the solver; run() skips (with a warning) when false.
        bool initialized = false;

    public:
        /** @brief Construct an uninitialized GasChem process adapter. */
        GasChemProcess();
        /** @brief Release the owned MUSICA instance and solver state. */
        ~GasChemProcess() override;

        /** @brief Return the stable process registry name. */
        std::string get_name() const override { return std::string(ProcessNames::GasChem); }

        /**
         * @brief Declare the met inputs and the mechanism-driven photolysis products.
         *
         * @return A ProcessContract for "gaschem" with host 3-D reads of T (K),
         *         PMID (Pa), an Optional AIRDEN_DRY (kg/m3), species-native
         *         CONCENTRATION (read/write), plus one Required product
         *         'photolysis_rate_<label>' per PHOTO.<label> rate parameter in
         *         the parsed mechanism (producer: "photolysis").
         *
         * @note The product list is empty until init() has parsed the
         *       mechanism; the execution plan validates the contract after
         *       init(), so ordering is guaranteed by the driver.
         */
        ProcessContract get_contract() const override;

        /**
         * @brief Load and validate the configured MUSICA mechanism.
         *
         * @param state Shared simulation state; supplies the configuration
         *              manager, the runtime configuration path (directory
         *              fallback), and grid dimensions for the solver State.
         *
         * @note Throws std::runtime_error when MICM fails to parse or build the
         *       mechanism (initialized stays false). Active CATChem species
         *       missing from the solver's variable map are only warned about —
         *       they are skipped silently during exchange in run().
         */
        void init(std::shared_ptr<StateManager> state) override;

        /**
         * @brief Advance the selected chemistry mechanism for the host interval.
         *
         * @param state Shared simulation state. run() refreshes AIRDEN_DRY, then
         *              requires current T, PMID, AIRDEN_DRY and CONCENTRATION
         *              views for the active import generation, converts
         *              concentrations ppmv → mol m-3, copies photolysis rates
         *              from the diagnostic manager, solves with the MICM
         *              Rosenbrock integrator over clock().timestep [s], and
         *              converts results back mol m-3 → ppmv in place.
         *
         * @note Returns early (warning only) when init() never succeeded.
         *       Throws std::runtime_error on stale/missing prerequisites or a
         *       non-converged solver, and std::domain_error on non-physical
         *       inputs (T/PMID/AIRDEN_DRY ≤ 0 or non-finite, negative
         *       concentrations, stale or non-finite photolysis rates) or a
         *       non-positive timestep. Marks CONCENTRATION host-modified.
         */
        void run(std::shared_ptr<StateManager> state) override;

        /** @brief Release process-owned solver resources. */
        void finalize() override;
    };

} // namespace catchem

extern "C" {
/// Register the "gaschem" process with the ProcessRegistry (no settings
/// validator: gaschem options are read through ConfigManager paths, not the
/// nested processes.<scheme>.<key> schema).
void catchem_register_gaschem_cpp();
}

/**
 * @file catchem_process_carbchem.hpp
 * @brief Declares the carbon-processing (hydrophobic/hydrophilic organic and
 *        black carbon) process adapter.
 *
 * @details The adapter owns no arrays: every met and concentration buffer it
 * forwards to the Fortran science bridge is borrowed from StateManager and
 * must remain alive for the duration of the call. The bridge is invoked
 * host-only, so the process advertises ExecutionSpaceIntent::Host accesses
 * for all of its contract fields.
 */
#pragma once

#include "catchem_process_interface.hpp"
#include "catchem_state_manager.hpp"
#include <memory>
#include <string>
#include <vector>

namespace catchem {

    /**
     * @brief GOCART carbon-chemistry process: hydrophobic/hydrophilic BC and OC
     *        ageing plus carbon loss, executed by the Fortran scheme layer.
     *
     * @details Supported scheme: `gocart` only — init() rejects any other
     * value of `processes.carbchem.scheme`, and the instance is constructed
     * with active_scheme already set to "gocart".
     *
     * The science itself lives in Fortran; this adapter stages configuration,
     * resolves the diagnostic species set, and forwards flat host pointers on
     * every step. @see run_carbchem_science_bridge (declared extern "C" in the
     * .cpp), CarbChemScienceBridge.F90, and the GOCART scheme implementation in
     * src/process/carbchem/schemes/CarbChemScheme_GOCART_Mod.F90.
     *
     * Lifecycle: prepare_inputs() materialises the process-owned DELP/AIRDEN
     * prerequisites, init() reads the YAML and registers the four
     * `carbchem_*` diagnostic fields, run() calls the bridge and marks the
     * concentration view host-modified, finalize() releases nothing.
     * Instance state is per-column-block configuration only, so the object is
     * not safe for concurrent run() calls on shared state.
     */
    class CarbChemProcess : public ProcessInterface {
    public:
        /** @brief Construct the adapter with the fixed `gocart` scheme and diagnostics on. */
        CarbChemProcess();
        /// @brief Destructor; the adapter owns no resources beyond its member strings/vectors.
        ~CarbChemProcess() override = default;

        /**
         * @brief Returns the canonical process name ("carbchem").
         *
         * @copydoc ProcessInterface::get_name
         */
        std::string get_name() const override { return "carbchem"; }

        /**
         * @brief Declare the host 3-D PMID/DELP/AIRDEN and species-native
         *        CONCENTRATION fields the GOCART carbon scheme consumes.
         *
         * @return A ProcessContract for "carbchem" whose fields are
         *         PMID (Pa), DELP (Pa), AIRDEN (kg/m3), and CONCENTRATION
         *         (Column, Level, Species) — all ExecutionSpaceIntent::Host.
         *
         * @note The contract names no diagnostics; the four `carbchem_*`
         *       products are registered with the DiagnosticManager by init()
         *       instead, because their packed species axis depends on the
         *       resolved diag_species set.
         */
        ProcessContract get_contract() const override;

        /**
         * @brief Derive the DELP layer thickness and AIRDEN air density the
         *        bridge consumes.
         *
         * @param state Shared simulation state; retained only for the call.
         *                Must already carry current PMID/T (and QV when the
         *                host wants moist AIRDEN) — the derive_* helpers throw
         *                std::runtime_error when their own prerequisites are
         *                absent.
         */
        void prepare_inputs(std::shared_ptr<StateManager> state) override;

        /**
         * @brief Read processes.carbchem (scheme, diagnostics, diag_species,
         *        gocart/time_days_hydrophobic_to_hydrophilic) from the runtime
         *        YAML and register the carbon-diagnostic field contracts.
         *
         * @param state Shared simulation state; must expose a non-null
         *              config_manager() and a loaded species catalog.
         *
         * @note Throws std::invalid_argument when no configuration is
         *       attached, when the YAML scheme is not "gocart", when
         *       gocart/time_days_hydrophobic_to_hydrophilic is not strictly
         *       positive, or when diag_species names an unknown species.
         *       Diagnostic registration is skipped when
         *       `processes.carbchem.diagnostics` is false or the mechanism
         *       selects no species.
         */
        void init(std::shared_ptr<StateManager> state) override;

        /**
         * @brief Run one GOCART carbon-chemistry step over the current column block.
         *
         * @param state Shared simulation state. run() re-invokes
         *              prepare_inputs() so that direct API callers and focused
         *              process tests obtain the same prerequisites the
         *              execution plan guarantees.
         *
         * @note Throws std::runtime_error via require_field_pointer() when any
         *       required field pointer is null and std::runtime_error when a
         *       BC1/BC2/OC1/OC2 carbon species is missing from the catalog.
         *       The CONCENTRATION view is written in place by the bridge and
         *       then marked host-modified; the bridge's tendency array is local
         *       scratch and is not published. Emits one `CATCHEM_BUDGET
         *       name=carbchem_carbon_mass` closure line to stdout per call.
         */
        void run(std::shared_ptr<StateManager> state) override;

        /**
         * @brief Release process-owned resources.
         *
         * @copydoc ProcessInterface::finalize
         * @note The GOCART path holds no persistent resources, so this is a no-op.
         */
        void finalize() override;

    private:
        /// Active scheme name; always "gocart" — the only scheme init() accepts.
        std::string active_scheme;
        /// Mirrors `processes.carbchem/diagnostics`; gates diagnostic-pointer handoff to the bridge.
        bool diagnostics_enabled;
        /// Diagnostic species slots in GLOBAL 1-based catalog space — the index
        /// space the GOCART scheme matches against its species_idx argument.
        std::vector<int> diagnostic_species_id;

        // GOCART scheme options staged from the runtime configuration
        // (processes/carbchem/gocart/*) and forwarded to the science bridge.
        /// Hydrophobic-to-hydrophilic BC/OC ageing timescale [days]; compiled
        /// default 2.5 matches CarbChemCommon_Mod.F90, init() requires > 0.
        double gocart_time_days = 2.5;
    };

} // namespace catchem

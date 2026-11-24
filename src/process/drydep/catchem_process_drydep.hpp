/**
 * @file catchem_process_drydep.hpp
 * @brief Declares the dry-deposition process adapter (Wesely gases + GOCART/Zhang aerosols).
 *
 * @details The adapter owns no arrays: every met, surface, and concentration
 * buffer it forwards to the Fortran science bridge is borrowed from
 * StateManager and must remain alive for the duration of the call. Species
 * metadata (molecular weights, deposition parameters, size-binning flags) is
 * gathered into per-run local scratch vectors that mirror the full catalog.
 */
#pragma once
#include "catchem_process_interface.hpp"
#include <string>
#include <vector>

namespace catchem {

    /**
     * @brief Surface dry deposition of gases and aerosols over the current column block.
     *
     * @details Supported schemes: gas deposition is fixed to `wesely`
     * (Wesely 1989) and aerosol deposition selects `gocart` (GOCART-2G) or
     * `zhang` — init() reads processes.drydep.gas_scheme and
     * processes.drydep.aero_scheme and rejects every other combination.
     *
     * The science lives in Fortran; this adapter stages configuration,
     * resolves the is_drydep diagnostic subset, gathers species metadata, and
     * forwards flat host pointers on every step. @see run_drydep_science_bridge
     * (declared extern "C" in the .cpp) and DryDepScienceBridge.F90.
     *
     * Lifecycle: prepare_inputs() derives the process-owned products
     * (BXHEIGHT, AIRDEN, RH, SALINITY, land-use categories, OBK, column cloud
     * fraction, SUNCOSMID); init() reads the YAML, loads per-scheme tuning
     * options, and registers the two packed drydep diagnostic fields; run()
     * calls the bridge, closes an aerosol-removal mass budget, and marks the
     * concentration view host-modified; finalize() is an inline no-op.
     */
    class DryDepProcess : public ProcessInterface {
    private:
        /// Gas-phase scheme name; init() only accepts "wesely".
        std::string gas_scheme;
        /// Aerosol-phase scheme name; init() only accepts "gocart" or "zhang".
        std::string aero_scheme;
        /// Mirrors `processes.drydep/diagnostics`; gates diagnostic-pointer handoff to the bridge.
        bool diagnostics_enabled;
        /// Diagnostic species slots in GLOBAL 1-based catalog space, built from
        /// each species' is_drydep metadata flag in init().
        std::vector<int> diagnostic_species_id;

        // Per-scheme tuning options read from processes.drydep.<scheme>.*
        // during init() and forwarded to the science bridge on every run().
        // Defaults mirror the DryDepScheme*Config types in DryDepCommon_Mod.F90.
        /// Wesely: multiplier applied to computed deposition velocities [1], default 1.0.
        double wesely_scale_factor = 1.0;
        /// Wesely: apply the CO2 stomatal-conductance adjustment when true.
        bool wesely_co2_effect = true;
        /// Wesely: ambient CO2 level [ppmv] used in the stomatal conductance scaling, default 600.
        double wesely_co2_level = 600.0;
        /// Wesely: reference CO2 level [ppmv] the conductance formula is calibrated against, default 380.
        double wesely_co2_reference = 380.0;
        /// GOCART aerosol: multiplier applied to computed deposition velocities [1], default 1.0.
        double gocart_scale_factor = 1.0;
        /// GOCART aerosol: enable surface resuspension of deposited particles.
        bool gocart_resuspension = false;
        /// GOCART aerosol: restrict resuspension to dust species when true (requires gocart_resuspension).
        bool gocart_dust_resuspension_only = true;
        /// Wesely: skip SO2 so its uptake is handled by the so4chem GOCART sulfate driver instead.
        bool wesely_skip_so2 = false;
        /// GOCART aerosol: skip SO4/MSA so their uptake is handled by the so4chem GOCART sulfate driver instead.
        bool gocart_skip_sulfate_aero = false;
        /// Zhang aerosol: multiplier applied to computed deposition velocities [1], default 1.0.
        double zhang_scale_factor = 1.0;

    public:
        /** @brief Construct the adapter with wesely/gocart schemes and diagnostics on. */
        DryDepProcess();

        /**
         * @brief Returns the canonical process name ("drydep").
         *
         * @copydoc ProcessInterface::get_name
         */
        std::string get_name() const override { return "drydep"; }

        /**
         * @brief Declare the full host-field surface the dry-deposition schemes read.
         *
         * @return A ProcessContract for "drydep" covering 3-D T (K), QV (kg/kg),
         *         PMID (Pa), BXHEIGHT (m), AIRDEN (kg/m3), RH (1), CLDF (1);
         *         interface-axis PEDGE (Pa) and Z (m); 2-D surface inputs PS (Pa),
         *         TS (K), USTAR (m/s), HFLUX (W/m2), OBK (m), PBLH (m), DLUSE (1),
         *         LAI (m2/m2), FRSNO (frac), SWGDN (W/m2), Z0 (m), FRLAKE (1),
         *         GWETTOP (1), LWI (1), U10M/V10M (m/s); persistent LAT/LON
         *         (degrees); an Optional SALINITY (1); and species-native
         *         CONCENTRATION (read/write). All accesses are
         *         ExecutionSpaceIntent::Host.
         *
         * @note The multi-category FRLAI, FRLANDUSE, and ILAND inputs are read
         *       directly from the state in run() rather than declared here,
         *       because reconstructing them from scalar LAI/DLUSE would change
         *       the Wesely calculation relative to the legacy MetState wiring.
         */
        ProcessContract get_contract() const override;

        /**
         * @brief Derive the process-owned meteorological and surface products.
         *
         * @param state Shared simulation state; retained only for the call.
         *              Must already carry the current host imports each
         *              derive_* helper needs (PEDGE, T/QV/PMID, USTAR/HFLUX,
         *              LAI/DLUSE, …); the helpers throw std::runtime_error when
         *              their own prerequisites are absent.
         */
        void prepare_inputs(std::shared_ptr<StateManager> state) override;

        /**
         * @brief Read processes.drydep (gas_scheme, aero_scheme, diagnostics, and the
         *        wesely, gocart, and zhang tuning options) from the runtime YAML and
         *        register the packed diagnostic fields.
         *
         * @param state Shared simulation state; must expose a non-null
         *              config_manager() and a loaded species catalog.
         *
         * @note Throws std::invalid_argument when no configuration is attached,
         *       the processes.drydep block is missing, or the gas/aerosol scheme
         *       pair is unsupported. Diagnostic species are selected by the
         *       per-species is_drydep flag (not by diag_species); the two
         *       `drydep_*_per_species` fields are registered only when
         *       diagnostics are enabled and at least one species participates.
         */
        void init(std::shared_ptr<StateManager> state) override;

        /**
         * @brief Run one dry-deposition step over the current column block.
         *
         * @param state Shared simulation state. All met reads are host-only and
         *              generation-current; run() does not re-derive them (the
         *              execution plan calls prepare_inputs() first), and reads
         *              never mark host-owned NUOPC imports modified.
         *
         * @note Throws std::runtime_error via require_field_pointer() when any
         *       required field pointer is null, and for species missing an
         *       explicit molecular weight (mw_g) or for aerosols missing valid
         *       density/radius bounds. The bridge removes mass from
         *       CONCENTRATION in place; the local tendency array is scratch and
         *       is not published. Emits one `CATCHEM_BUDGET
         *       name=drydep_aerosol_removal` closure line to stdout when
         *       diagnostics are enabled.
         */
        void run(std::shared_ptr<StateManager> state) override;

        /**
         * @brief Release process-owned resources.
         *
         * @copydoc ProcessInterface::finalize
         * @note DryDep holds no persistent resources, so this is an inline no-op.
         */
        void finalize() override {}
    };

} // namespace catchem

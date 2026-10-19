/**
 * @file catchem_process_so4chem.hpp
 * @brief Declares the GOCART sulfur-chemistry (DMS/SO2/SO4/MSA) process adapter.
 *
 * @details Besides borrowing host met and concentration buffers from
 * StateManager for each bridge call, this adapter owns persistent per-column
 * and per-layer Fortran state (the H2O2 oxidant memory and sulfate production
 * accumulators) that the scheme carries across timesteps. Those vectors are
 * sized in init() and must not be resized between run() calls; the bridge
 * updates them in place.
 */
#pragma once
#include "catchem_process_interface.hpp"
#include <string>
#include <vector>

namespace catchem {

    /**
     * @brief GOCART sulfate chemistry: DMS oxidation, SO2 aqueous/gaseous conversion, and H2O2 oxidant memory.
     *
     * @details Supported scheme: `gocart` only — init() rejects any other
     * value of processes.so4chem.scheme. The adapter enforces the unit
     * contract of the Fortran ProcessSO4chemInterface_Mod/SO4chemScheme_GOCART_Mod
     * pair: dms and so2 must be gases (ppmv) and so4 and msa must be aerosols
     * (ug/kg), because the science uses fixed internal conversions for these
     * four species.
     *
     * The science runs in Fortran; this adapter stages configuration, allocates
     * the cross-timestep column state, and forwards flat host pointers on every
     * step. @see run_so4chem_science_bridge (declared extern "C" in the .cpp)
     * and SO4chemScienceBridge.F90.
     *
     * Lifecycle: prepare_inputs() derives DELP/AIRDEN and requires a current
     * Z0H or fallback Z0 roughness; init() reads the YAML, validates the sulfur
     * species phases, allocates persistent state, and registers the PSO4/DMS
     * production plus per-species Production_rate_<sp> diagnostics; run()
     * calls the bridge, closes an SO4/MSA production budget, copies the
     * persistent accumulators into registered diagnostics, and marks
     * CONCENTRATION host-modified; finalize() is an inline no-op (the
     * persistent vectors die with the instance).
     */
    class SO4chemProcess : public ProcessInterface {
    private:
        /// Active scheme name; always "gocart" — the only scheme init() accepts.
        std::string active_scheme;
        /// Mirrors `processes.so4chem/diagnostics`; gates diagnostic writes and per-species id forwarding.
        bool diagnostics_enabled;
        /// Diagnostic species slots in GLOBAL 1-based catalog space (default sulfur
        /// chain plus oxidants, or the diag_species override resolved in init()).
        std::vector<int> diagnostic_species_id;

        // GOCART scheme options staged from the runtime configuration
        // (processes/so4chem/gocart/*) and forwarded to the science bridge.
        /// GOCART: update the SO2 concentration from sulfate chemistry when true (default true).
        bool gocart_update_so2 = true;
        /// GOCART: let the sulfate driver apply its own dry deposition of SO2 (default false; drydep handles it otherwise).
        bool gocart_do_drydep = false;

        // Persistent column states
        // Stored as C int rather than char/bool so the Fortran bridge has an
        // exact, alias-safe integer(c_int) ABI.  std::vector<bool> is packed
        // and vector<char> cannot legally be reinterpreted as bool*.
        /// Per-column first-step flag (1 until the scheme has initialised the
        /// H2O2 oxidant climatology for that column); [column_count], in/out.
        std::vector<int> firsttime;
        /// Per-column last day (YYYYMMDD numeric) of H2O2 oxidant update; [column_count], in/out.
        std::vector<int> nymd_last;
        /// Per-column last time-of-day (HHMMSS numeric) of H2O2 recycle; [column_count], in/out.
        std::vector<int> nhms_last_recycle;
        /// Per-layer H2O2 column initialization carried between steps (mol/mol volume
        /// mixing ratio — the GOCART scheme seeds it from the climatology and recycles
        /// it across steps); [column_count, level_count], in/out.
        std::vector<double> xh2o2_init;
        /// Per-layer gaseous SO2 → SO4 production rate [kg/kg/s]; [column_count, level_count], out — also copied to its diagnostic field.
        std::vector<double> pso4_g_so2;
        /// Per-layer aqueous SO2 → SO4 production rate [kg/kg/s]; [column_count, level_count], out — also copied to its diagnostic field.
        std::vector<double> pso4_aq_so2;
        /// Per-layer DMS → SO2 production rate; [column_count, level_count], zero-initialised.
        /// Reserved in the bridge ABI alongside the other sulfur accumulators, but the
        /// current Fortran bridge binds this pointer without reading or writing it (the
        /// DMS → SO2 conversion is folded into the in-place `conc`/`tendency` update), so
        /// the buffer stays all-zero across steps.
        std::vector<double> pso2_dms;
        /// Per-column DMS surface emission flux [kg/m2/s]; [column_count], out — also copied to its diagnostic field.
        std::vector<double> dms_flux;

    public:
        /** @brief Construct the adapter with the fixed `gocart` scheme and diagnostics on. */
        SO4chemProcess();

        /**
         * @brief Returns the canonical process name ("so4chem").
         *
         * @copydoc ProcessInterface::get_name
         */
        std::string get_name() const override { return "so4chem"; }

        /**
         * @brief Declare the host met, surface, and concentration inputs the sulfate driver reads.
         *
         * @return A ProcessContract for "so4chem" with host 3-D reads of T (K),
         *         PMID (Pa), DELP (Pa), AIRDEN (kg/m3), CLDF (1); interface-axis
         *         PEDGE (Pa) and Z (m); 2-D HFLUX (W/m2), persistent LAT/LON
         *         (degrees), PBLH (m), USTAR (m/s), U10M/V10M (m/s), LWI (1);
         *         Optional Z0H and Z0 (m) — at least one must be current, which
         *         prepare_inputs() enforces; and species-native CONCENTRATION.
         */
        ProcessContract get_contract() const override;

        /**
         * @brief Derive shared meteorology and validate the SO4Chem roughness alternative.
         *
         * @param state Shared simulation state; retained only for the call.
         *              Must already carry current PMID/T (for AIRDEN) and PEDGE
         *              (for DELP).
         *
         * @note Throws std::runtime_error when neither Z0H nor the Z0 fallback is
         *       current: GOCART's surface resistance needs thermal roughness, and
         *       run() converts Z0 → Z0H only from an already-imported value.
         */
        void prepare_inputs(std::shared_ptr<StateManager> state) override;

        /**
         * @brief Read processes.so4chem (scheme, diagnostics, diag_species, and the
         *        gocart/update_so2 and gocart/do_drydep options), validate the sulfur
         *        species unit contract, and allocate persistent state.
         *
         * @param state Shared simulation state; must expose a non-null
         *              config_manager() and a loaded mechanism containing dms,
         *              so2, so4, and msa with the required gas/aerosol phases.
         *
         * @note Throws std::invalid_argument when no configuration is attached,
         *       the scheme is not "gocart", the mechanism is missing, a required
         *       sulfur species is absent or mis-phased, or diag_species names an
         *       unknown species. Default diagnostic set (dms, so2, so4, msa,
         *       h2o2, oh, no3, dms_in) tolerates species absent from the
         *       mechanism. Persistent vectors are sized here to
         *       column_count × level_count and must stay fixed for the run.
         */
        void init(std::shared_ptr<StateManager> state) override;

        /**
         * @brief Run one sulfur-chemistry step over the current column block.
         *
         * @param state Shared simulation state; supplies host field pointers,
         *              the species catalog, clock calendar/timestep, and the
         *              diagnostic manager. All buffers are borrowed for the call;
         *              the persistent member vectors are updated in place.
         *
         * @note Throws std::runtime_error via require_field_pointer() on missing
         *       fields and for species missing an explicit molecular weight; the
         *       budget check additionally requires so4 and msa to be present as
         *       aerosols with diagnostic slots. When Z0H is absent, run()
         *       materialises it from Z0 using the UFS/GOCART convention
         *       (Z0H = 0.01 × raw Z0, i.e. the already unit-converted state).
         *       Emits `CATCHEM_BUDGET name=so4chem_production` to stdout per call.
         */
        void run(std::shared_ptr<StateManager> state) override;

        /**
         * @brief Release process-owned resources.
         *
         * @copydoc ProcessInterface::finalize
         * @note SO4Chem holds no Fortran-side resources; the persistent column
         *       vectors are released with the instance, so this is an inline no-op.
         */
        void finalize() override {}
    };

} // namespace catchem

/**
 * @file catchem_process_dust.hpp
 * @brief Declares the mineral-dust emission process adapter (Fengsha and Ginoux schemes).
 *
 * @details The adapter owns no arrays: the soil, surface, and concentration
 * buffers it forwards to the Fortran science bridge are borrowed from
 * StateManager and must remain alive for the duration of the call. Dust size
 * bins (DUST1..DUSTn, count derived from is_dust metadata) are resolved by
 * canonical name in both init() and run(), so the diagnostic bin ids and the
 * physics bin arrays always share one ordering.
 */
#pragma once

#include "catchem_process_interface.hpp"
#include "catchem_state_manager.hpp"
#include <memory>
#include <string>
#include <vector>

namespace catchem {

    /**
     * @brief Mineral-dust emission from dry, unvegetated soils into the lowest model layers.
     *
     * @details Supported schemes: `fengsha` (NOAA ARL Fengsha, default) and
     * `ginoux` — init() reads processes.dust.scheme and rejects anything else.
     * The science runs in Fortran; this adapter stages configuration, resolves
     * the DUST bins, and forwards flat host pointers on every step.
     * @see run_dust_science_bridge (declared extern "C" in the .cpp) and
     * DustScienceBridge.F90.
     *
     * Lifecycle: prepare_inputs() derives DELP and AIRDEN; init() reads the
     * YAML, validates the Fengsha/Ginoux tuning options, resolves the bins, and
     * registers the total/per-bin emission plus threshold diagnostics; run()
     * calls the bridge (which emits mass into CONCENTRATION in place), closes
     * the bin-closure budget, and marks the concentration view host-modified;
     * finalize() is a no-op.
     *
     * @note The two schemes consume different surface fields: get_contract()
     * adds the Fengsha soil-texture set (CLAYFRAC, GVF, RDRAG, SNDFRC, SOILM,
     * USTAR_THRESHOLD, Z0, …) or the Ginoux set (GWETTOP, U10M/V10M, …)
     * depending on active_scheme, and run() enforces the matching pointers.
     */
    class DustProcess : public ProcessInterface {
    public:
        /** @brief Construct the adapter with the fengsha scheme and diagnostics on. */
        DustProcess();
        /// @brief Destructor; the adapter owns no resources beyond its member strings/vectors.
        ~DustProcess() override = default;

        /**
         * @brief Returns the canonical process name ("dust").
         *
         * @copydoc ProcessInterface::get_name
         */
        std::string get_name() const override { return "dust"; }

        /**
         * @brief Declare the scheme-conditional host inputs the dust kernel reads.
         *
         * @return A ProcessContract for "dust" with interface PEDGE (Pa), 3-D
         *         DELP (Pa) and AIRDEN (kg/m3), and species-native
         *         CONCENTRATION (read/write), plus — for fengsha — soil-layer
         *         SOILM (m3/m3) and 2-D CLAYFRAC (1), FRLAKE (1), FRSNO (frac),
         *         GVF (frac), LAI (m2/m2), LWI (1), RDRAG (1), SNDFRC (1), SSM
         *         (1), TS (K), USTAR (m/s), USTAR_THRESHOLD (m/s), Z0 (m); for
         *         ginoux — 2-D FRLAKE (1), FRSNO (frac), GWETTOP (1), LWI (1),
         *         SSM (1), TS (K), U10M/V10M (m/s). All met accesses are host
         *         reads — emissions enter the model through CONCENTRATION.
         */
        ProcessContract get_contract() const override;

        /**
         * @brief Derive the DELP layer thickness and AIRDEN air density the bridge consumes.
         *
         * @param state Shared simulation state; retained only for the call.
         *              Must already carry the current PMID/T (and PEDGE when
         *              DELP is not imported) — the derive_* helpers throw
         *              std::runtime_error when their prerequisites are absent.
         */
        void prepare_inputs(std::shared_ptr<StateManager> state) override;

        /**
         * @brief Read processes.dust (scheme, diagnostics, diag_species, and the
         *        fengsha and ginoux/Ch_DU tuning options) from the runtime YAML
         *        and register emission diagnostics.
         *
         * @param state Shared simulation state; must expose a non-null
         *              config_manager() and a mechanism whose is_dust species
         *              cover the canonical DUST1..DUSTn sequence with valid
         *              density and radius bounds.
         *
         * @note Throws std::invalid_argument when no configuration is attached,
         *       the scheme is missing/unsupported, ginoux/Ch_DU does not declare
         *       exactly five values, or diag_species names a non-dust species;
         *       std::runtime_error (from the shared bin resolver) when a
         *       canonical bin is absent or physically incomplete. Registers six
         *       dust_* diagnostic fields only when diagnostics are enabled.
         */
        void init(std::shared_ptr<StateManager> state) override;

        /**
         * @brief Run one dust-emission step over the current column block.
         *
         * @param state Shared simulation state. run() re-invokes
         *              prepare_inputs() so direct API users and focused process
         *              tests obtain the same prerequisites the execution plan
         *              guarantees, then reads the scheme-conditional surface and
         *              soil fields as host pointers.
         *
         * @note Throws std::runtime_error via require_field_pointer() on missing
         *       fields, and for a SOILM field without a soil-layer extent on the
         *       fengsha path; returns early when no dust bins exist. The bridge
         *       writes DUST slots of CONCENTRATION in place (resolved by name);
         *       the local tendency array is scratch. Emits `CATCHEM_BUDGET
         *       name=dust_bin_closure` to the log when totals and per-bin
         *       diagnostics are both present.
         */
        void run(std::shared_ptr<StateManager> state) override;

        /**
         * @brief Release process-owned resources.
         *
         * @copydoc ProcessInterface::finalize
         * @note Dust holds no persistent resources, so this is a no-op.
         */
        void finalize() override;

    private:
        /// Active emission scheme; constructor default "fengsha", init() validates against fengsha/ginoux.
        std::string active_scheme;
        /// Mirrors `processes.dust/diagnostics`; gates diagnostic-pointer handoff to the bridge.
        bool diagnostics_enabled;
        /// Diagnostic bin slots in 1-based LOCAL bin space (positions within the
        /// canonical resolved_dust_bins order), optionally subset by diag_species.
        std::vector<int> diagnostic_species_id;

        // Fengsha scheme tuning options, read from processes.dust.fengsha.*
        // during init() and forwarded to the science bridge on every run().
        // Defaults mirror DustSchemeFENGSHAConfig in DustCommon_Mod.F90.
        /// Fengsha: linear scaling factor applied to the emission source term [1], default 0.2.
        double fengsha_alpha = 0.2;
        /// Fengsha: exponent on the soil-moisture source factor [1], default 1.0.
        double fengsha_gamma = 1.0;
        /// Fengsha: multiplier on the Fecan dry-limit threshold [1], default 1.0.
        double fengsha_drylimit_factor = 1.0;
        /// Fengsha: multiplier on the moisture correction term [1], default 1.0.
        double fengsha_moist_correction_factor = 1.0;
        /// Fengsha: maximum vertical-to-horizontal mass-flux ratio [1], default 2e-4.
        double fengsha_kvhmax = 0.0002;
        /// Fengsha drag partition: 1 input drag, 2 Darmenova, 3 Leung 2022, 4 MB95 (default 1).
        int fengsha_drag_option = 1;
        /// Fengsha horizontal flux scheme: 1 White 1979, 2 Draxler 2001, 3 Kawamura 1964 (default 1).
        int fengsha_horizflux_option = 1;
        /// Fengsha moisture parameterization: 1 Fecan, 2 Zhao (default 1).
        int fengsha_moist_option = 1;
        /// Fengsha size-distribution option: 1 Kok 2011, 2 Meng 2022 (not implemented) (default 1).
        int fengsha_distribution_option = 1;

        // Ginoux scheme tuning options, read from processes.dust.ginoux.*
        // during init().  Ch_DU holds one multiplier per dust size bin and
        // must stay the same length as DustSchemeGINOUXConfig%Ch_DU.
        /// Ginoux: per-bin dust emission tuning coefficients [kg s-2 m-5], five entries, default 1.0 each.
        std::vector<double> ginoux_ch_du = std::vector<double>(5, 1.0);
    };

} // namespace catchem

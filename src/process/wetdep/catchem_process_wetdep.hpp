/**
 * @file catchem_process_wetdep.hpp
 * @brief Declares the wet-deposition process adapter (Jacob et al. 2000 and GOCART2G wet removal).
 *
 * @details The adapter owns no arrays: every met, cloud, and concentration
 * buffer it forwards to one of two Fortran science bridges is borrowed from
 * StateManager and must remain alive for the duration of the call. The two
 * schemes have separate bridge entry points — the Jacob FFI surface is kept
 * byte-identical to legacy, and the GOCART2G entry adds the PRECCON/PRECLSC
 * surface rain inputs — with the active scheme selecting which one run() uses.
 */
#pragma once
#include "catchem_process_interface.hpp"
#include <string>
#include <vector>

namespace catchem {

    /**
     * @brief Below-cloud washout and in-cloud scavenging of gases and aerosols.
     *
     * @details Supported schemes: `jacob` (Jacob et al. [2000], default) and
     * `gocart` (GOCART2G wet removal: SU_Wet_Removal for the sulfate chain,
     * WetRemovalUFS for other aerosols) — init() reads
     * processes.wetdep.scheme and rejects anything else.
     *
     * The science runs in Fortran; this adapter stages configuration, resolves
     * the diagnostic species set (diag_species override, else the is_wetdep
     * metadata flag), gathers per-species Henry-law and scheme parameters, and
     * forwards flat host pointers on every step. @see run_wetdep_science_bridge
     * and run_wetdep_gocart_science_bridge (declared extern "C" in the .cpp)
     * and WetDepScienceBridge.F90.
     *
     * Lifecycle: prepare_inputs() derives scheme-specific products (REEVAPLS
     * and AIRDEN_DRY for Jacob, plus AIRDEN for both); init() reads the YAML,
     * validates gocart tuning ranges, and registers one mass/flux diagnostic
     * pair per selected species; run() calls the selected bridge, scatters the
     * packed per-species diagnostics back into the registered fields, and
     * marks CONCENTRATION host-modified; finalize() is an inline no-op.
     */
    class WetDepProcess : public ProcessInterface {
    private:
        /// Active scheme name; constructor default "jacob", init() validates against jacob/gocart.
        std::string active_scheme;
        /// Mirrors `processes.wetdep/diagnostics`; gates diagnostic registration and pointer handoff.
        bool diagnostics_enabled;
        /// Diagnostic species slots in GLOBAL 1-based catalog space — the positions
        /// the Jacob scheme uses as its packed diag_mass/diag_flux slab order.
        std::vector<int> diagnostic_species_id;

        // Jacob scheme tuning options read from processes.wetdep.jacob.*
        // during init() and forwarded to the science bridge on every run().
        // Defaults mirror WetDepSchemeJACOBConfig in WetDepCommon_Mod.F90.
        /// Jacob: overall washout tuning factor [1], default 1.0.
        double jacob_scale_factor = 1.0;
        /// Jacob: radius threshold for aerosol wet deposition [µm], default 1.0.
        double jacob_radius_threshold = 1.0;
        /// Jacob: use the GOCART SU_Wet_Removal resuspension alpha for sulfate (SO4/SO2) species.
        bool jacob_so4_gocart_resusp = true;
        /// Jacob: sulfate-only below-cloud washout efficiency multiplier [1] (1.0 = unchanged), default 1.0.
        double jacob_so4_washout_eff = 1.0;

        // GOCART2G wet-removal scheme tuning options read from
        // processes.wetdep.gocart.* during init() and forwarded to the GOCART
        // bridge entry on every run().  Defaults mirror
        // WetDepSchemeGOCARTConfig in WetDepCommon_Mod.F90 (GOCART2G values).
        /// GOCART: overall washout tuning factor [1]; init() requires > 0, default 1.0.
        double gocart_scale_factor = 1.0;
        /// GOCART: WetRemovalUFS below-cloud washout tuning (wtune) [1]; init() requires >= 0, default 1.0.
        double gocart_washout_tuning = 1.0;
        /// GOCART: aerosol washout radius threshold [µm]; init() requires > 0, default 0.05.
        double gocart_radius_threshold = 0.05;

    public:
        /** @brief Construct the adapter with the Jacob scheme and diagnostics on. */
        WetDepProcess();

        /**
         * @brief Returns the canonical process name ("wetdep").
         *
         * @copydoc ProcessInterface::get_name
         */
        std::string get_name() const override { return "wetdep"; }

        /**
         * @brief Declare required host inputs, including GOCART-only rain amounts.
         *
         * @return A ProcessContract for "wetdep" with host 3-D T (K), AIRDEN
         *         (kg/m3); interface-axis PEDGE (Pa), PFILSAN/PFLLSAN
         *         (kg/m2/s); 2-D PRECCON/PRECLSC (m); and species-native
         *         CONCENTRATION. PRECCON/PRECLSC are Required only for the
         *         gocart scheme (Optional otherwise so a jacob-only
         *         configuration validates). For jacob the contract additionally
         *         requires PMID (Pa), AIRDEN_DRY (kg/m3), QV (kg/kg), and
         *         REEVAPLS (kg/kg/s) as 3-D reads.
         */
        ProcessContract get_contract() const override;

        /**
         * @brief Derive only the meteorological products consumed by the active wet-deposition scheme.
         *
         * @param state Shared simulation state; retained only for the call.
         *              AIRDEN is always derived (both schemes); REEVAPLS and
         *              AIRDEN_DRY are Jacob-only and derived only then. The
         *              derive_* helpers throw std::runtime_error when their own
         *              prerequisites (PEDGE, PMID/T/QV, PFILSAN/PFLLSAN) are
         *              absent.
         */
        void prepare_inputs(std::shared_ptr<StateManager> state) override;

        /**
         * @brief Read processes.wetdep (scheme, diagnostics, diag_species, and the
         *        jacob and gocart tuning options) from the runtime YAML and
         *        register per-species diagnostics.
         *
         * @param state Shared simulation state; must expose a non-null
         *              config_manager() and a loaded species catalog.
         *
         * @note Throws std::invalid_argument when no configuration is attached,
         *       the scheme is missing/unsupported, diag_species is used without a
         *       loaded mechanism or names an unknown species, or a gocart tuning
         *       option is out of range (scale_factor > 0, washout_tuning >= 0,
         *       radius_threshold > 0). Registers one wetdep_mass_/wetdep_flux_
         *       field per selected species only when diagnostics are enabled.
         */
        void init(std::shared_ptr<StateManager> state) override;

        /**
         * @brief Run one wet-deposition step over the current column block.
         *
         * @param state Shared simulation state; supplies host field pointers,
         *              the species catalog, clock().timestep [s], and the
         *              diagnostic manager. All buffers are borrowed for the call.
         *
         * @note Throws std::runtime_error via require_field_pointer() on missing
         *       fields (AIRDEN_DRY/REEVAPLS additionally for jacob, PRECCON/
         *       PRECLSC for gocart), for a non-positive gocart timestep (the
         *       kernel divides rain by it), and for wetdep-participating aerosols
         *       missing radius/mw_g or gases missing mw_g. The bridge removes mass
         *       from CONCENTRATION in place; the local tendency array is scratch.
         *       Packed diagnostics are scattered back into the registered
         *       per-species fields by selected-set position, not global index.
         */
        void run(std::shared_ptr<StateManager> state) override;

        /**
         * @brief Release process-owned resources.
         *
         * @copydoc ProcessInterface::finalize
         * @note WetDep holds no persistent resources, so this is an inline no-op.
         */
        void finalize() override {}
    };

} // namespace catchem

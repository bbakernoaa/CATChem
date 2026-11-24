/**
 * @file catchem_process_seasalt.hpp
 * @brief Declares the sea-salt emission process adapter (Gong 1997/2003, GEOS-Chem 2012).
 *
 * @details The adapter owns no arrays: the surface/ocean buffers it forwards
 * to the Fortran science bridge are borrowed from StateManager and must
 * remain alive for the duration of the call. The five canonical SEAS1..SEAS5
 * size bins are resolved by name (never by declaration order) in both init()
 * and run(), so the diagnostic bin ids and the physics bin arrays always share
 * one ordering.
 */
#pragma once
#include "catchem_process_interface.hpp"
#include <string>
#include <vector>

namespace catchem {

    /**
     * @brief Sea-salt emission from ocean surfaces into the lowest model layers.
     *
     * @details Supported schemes: `gong97`, `gong03`, and `geos12` —
     * init() reads processes.seasalt.scheme and rejects anything else
     * (default geos12). The science runs in Fortran; this adapter stages the
     * per-scheme tuning options, resolves the SEAS1..SEAS5 bins, and forwards
     * flat host pointers on every step. @see run_seasalt_science_bridge
     * (declared extern "C" in the .cpp) and SeaSaltScienceBridge.F90.
     *
     * Lifecycle: prepare_inputs() derives DELP; init() reads the YAML and
     * registers the total and per-bin emission diagnostics; run() gathers bin
     * metadata, calls the bridge (which emits mass/number into CONCENTRATION
     * in place), closes the bin-closure budget, and marks the concentration
     * view host-modified; finalize() is an inline no-op.
     *
     * @note Only the geos12 scheme requires USTAR; get_contract() adds it
     * conditionally, and run() enforces it for that scheme only.
     */
    class SeaSaltProcess : public ProcessInterface {
    private:
        /// Active emission scheme; constructor default "geos12", init() validates against gong97/gong03/geos12.
        std::string active_scheme;
        /// Mirrors `processes.seasalt/diagnostics`; gates diagnostic-pointer handoff to the bridge.
        bool diagnostics_enabled;
        /// Diagnostic bin slots in 1-based LOCAL bin space (positions within the
        /// canonical SEAS1..SEAS5 order), optionally subset by diag_species.
        std::vector<int> diagnostic_species_id;

        // Per-scheme tuning options read from processes.seasalt.<scheme>.*
        // during init() and forwarded to the science bridge on every run().
        // Defaults mirror the SeaSaltScheme*Config types in SeaSaltCommon_Mod.F90.
        /// Gong 1997: emission scale factor [1], default 1.0.
        double gong97_scale_factor = 1.0;
        /// Gong 1997: apply the Fan–Toon Weibull wind-speed distribution to emissions.
        bool gong97_weibull_flag = false;
        /// Gong 2003: emission scale factor [1], default 1.0.
        double gong03_scale_factor = 1.0;
        /// Gong 2003: apply the Fan–Toon Weibull wind-speed distribution to emissions.
        bool gong03_weibull_flag = false;
        /// GEOS-Chem 2012: emission scale factor [1], default 1.0.
        double geos12_scale_factor = 1.0;
        /// GEOS-Chem 2012: apply the Fan–Toon Weibull wind-speed distribution to emissions.
        bool geos12_weibull_flag = false;

    public:
        /** @brief Construct the adapter with the geos12 scheme and diagnostics on. */
        SeaSaltProcess();

        /**
         * @brief Returns the canonical process name ("seasalt").
         *
         * @copydoc ProcessInterface::get_name
         */
        std::string get_name() const override { return "seasalt"; }

        /**
         * @brief Declare the ocean/surface host inputs the emission schemes read.
         *
         * @return A ProcessContract for "seasalt" with interface PEDGE (Pa),
         *         3-D DELP (Pa), 2-D FROCEAN/FRSEAICE (frac), TS (K),
         *         persistent LAT/LON (degrees), U10M/V10M (m/s), and
         *         species-native CONCENTRATION (read/write); USTAR (m/s) is
         *         added only when the active scheme is geos12. All met accesses
         *         are host reads — emissions enter the model through
         *         CONCENTRATION.
         */
        ProcessContract get_contract() const override;

        /**
         * @brief Derive the DELP layer thickness the emission kernel converts mass to mixing ratios.
         *
         * @param state Shared simulation state; retained only for the call.
         *              Must already carry a current PEDGE — derive_delp() throws
         *              std::runtime_error otherwise.
         */
        void prepare_inputs(std::shared_ptr<StateManager> state) override;

        /**
         * @brief Read processes.seasalt (scheme, diagnostics, diag_species, and the
         *        gong97/gong03/geos12 scale_factor and weibull_flag options) from the
         *        runtime YAML and register emission diagnostics.
         *
         * @param state Shared simulation state; must expose a non-null
         *              config_manager() and a mechanism containing the five
         *              canonical SEAS1..SEAS5 bins with valid density, radius
         *              bounds, and molecular weight.
         *
         * @note Throws std::invalid_argument when no configuration is attached,
         *       the scheme is missing/unsupported, or diag_species names a
         *       non-sea-salt species; std::runtime_error (from the shared bin
         *       resolver) when a canonical bin is absent or physically
         *       incomplete. Registers two total and two per-bin emission fields
         *       only when diagnostics are enabled.
         */
        void init(std::shared_ptr<StateManager> state) override;

        /**
         * @brief Run one sea-salt emission step over the current column block.
         *
         * @param state Shared simulation state; supplies host field pointers,
         *              the species catalog, the clock timestep [s], and the
         *              diagnostic manager. Buffers are borrowed for the call.
         *
         * @note Returns early when no sea-salt bins exist (cannot happen after
         *       a successful init()). Throws std::runtime_error via
         *       require_field_pointer() on missing fields and when the
         *       registered per-bin diagnostic capacity is smaller than the
         *       selected bin count. The bridge writes SEAS1..SEAS5 slots of
         *       CONCENTRATION in place (resolved by name); the local tendency
         *       array is scratch. Emits `CATCHEM_BUDGET name=seasalt_bin_closure`
         *       to the log when totals and per-bin diagnostics are both present.
         */
        void run(std::shared_ptr<StateManager> state) override;

        /**
         * @brief Release process-owned resources.
         *
         * @copydoc ProcessInterface::finalize
         * @note SeaSalt holds no persistent resources, so this is an inline no-op.
         */
        void finalize() override {}
    };

} // namespace catchem

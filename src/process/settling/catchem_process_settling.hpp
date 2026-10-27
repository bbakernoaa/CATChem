/**
 * @file catchem_process_settling.hpp
 * @brief Declares the gravitational-settling process adapter for GOCART aerosols.
 *
 * @details The adapter owns one resource that crosses the language boundary:
 * an immutable Fortran-side Mie optics-table handle (when the simple_scheme
 * path is selected). Everything else — met fields and the CONCENTRATION view —
 * is borrowed from StateManager and must remain alive for the duration of each
 * bridge call. Bridge entry points are serialized by a process-local mutex
 * because the Fortran store behind the handle is module-stateful.
 */
#pragma once
#include "catchem_process_interface.hpp"
#include <functional>
#include <memory>
#include <string>
#include <vector>

namespace catchem {

    /**
     * @brief Runs the GOCART aerosol settling process through its Fortran bridge.
     *
     * @details Supported scheme: `gocart` only — init() rejects any other
     * value of processes.settling.scheme. Within GOCART, two optics paths are
     * selectable by gocart/simple_scheme: the species-metadata path (default,
     * reproducing upstream GOCART2G swelling/density metadata) or the legacy
     * Mie optics-table path driven by the top-level `mie:` YAML section.
     *
     * The instance owns its immutable Mie-table handle and all species mappings
     * used by its bridge calls. @see run_settling_science_bridge and
     * run_settling_mie_init/run_settling_mie_release (declared extern "C" in
     * the .cpp) and SettlingScienceBridge.F90.
     *
     * Lifecycle: prepare_inputs() derives DELP/AIRDEN/RH; init() reads the
     * YAML, optionally loads Mie tables, freezes per-species radius/dust/opts
     * mappings, and registers the velocity/flux diagnostics; run() forwards
     * flat host pointers to the kernel under the bridge mutex and marks
     * CONCENTRATION host-modified; finalize() releases the Mie handle.
     */
    class SettlingProcess : public ProcessInterface {
    private:
        /// Active scheme name; init() only accepts "gocart" (constructor default is a placeholder).
        std::string active_scheme;
        /// Optional legacy test hook: when set, run() invokes it instead of the real bridge.
        std::function<void(void*)> fortran_callback;

        // GOCART scheme options staged from the runtime configuration
        // (processes/settling/gocart/*).  They are forwarded verbatim to the
        // Fortran science bridge, which reproduces the upstream GOCART2G
        // settling path.  scale_factor is retained for configuration parity;
        // the upstream metadata path does not consume it.  simple_scheme selects
        // the optics-table (Mie) path; when true the tables named by the
        // top-level "mie:" section are loaded during init and every settling
        // species must resolve one through its __mie_name.
        double gocart_scale_factor =
            1.0; //!< Settling velocity multiplier [1]; parity option, unused on the metadata path.
        bool gocart_simple_scheme = false;     //!< Select the Mie optics-table path instead of species metadata.
        int gocart_swelling_method = 1;        //!< Swelling scheme: 1 Fitzgerald 1975, 2 Gerber 1985.
        bool gocart_correction_maring = false; //!< Apply the Maring et al. (2003) settling-velocity correction.
        bool gocart_maring_dust_only = true;   //!< Restrict the Maring correction to dust species.

        // The Fortran science bridge resolves these canonical names against
        // the full chemistry species list.  Do not pass C++ indices across
        // this language boundary: C++ is zero-based and Fortran is one-based.
        std::vector<char> aerosol_species_names; //!< Settling-subset species names, 32-byte blank-padded fixed width.
        std::vector<double> host_radius_dry; //!< Dry particle radius [µm], as configured (µm → m inside the scheme).
        std::vector<double> host_rhop_dry;   //!< Dry particle density [kg/m3], as configured.
        std::vector<int> host_is_dust;       //!< 0/1 dust flag per settling species (integer ABI, not logical).
        /// Per settling species' __mie_name (32-byte fixed width, same packing as
        /// aerosol_species_names).  The Fortran bridge maps these to loaded table
        /// indices; empty means unresolved and aborts initialization on the optics path.
        std::vector<char> aerosol_mie_names;
        /// Context-owned immutable Fortran table set; zero when simple_scheme is off.
        int mie_handle = 0;

        // Per-process scheme diagnostics (specs: process-diagnostics-parity).
        // diagnostics_enabled mirrors processes/settling/diagnostics;
        // diagnostic_species_id holds 1-based LOCAL positions within the
        // aerosol subset the bridge gathers (species_mie_map order), matching
        // the species_idx the scheme loops over.
        bool diagnostics_enabled = false; //!< Mirrors processes/settling/diagnostics; gates diagnostic pointer handoff.
        std::vector<int> diagnostic_species_id; //!< Selected diagnostic slots in 1-based LOCAL aerosol-subset space.
        std::vector<std::string>
            diagnostic_species_names; //!< Short names parallel to ids, used for run() field lookups.

        // Native C++ GOCART settling engine (scheme "gocart_cpp").  When this
        // scheme is selected the process runs the ported settling science in
        // C++ and takes the wet particle radius/density from the aerosol
        // library, so the Fortran science bridge is never invoked.  The
        // library-facing state lives behind a pimpl defined in the
        // translation unit, keeping the installed headers free of the
        // dependency; selecting the scheme in a build without that support is
        // a fatal configuration error caught during initialization.
        bool use_cpp_engine = false;
        struct CppEngine;
        std::unique_ptr<CppEngine> cpp_engine;
#ifdef CATCHEM_HAVE_EXAERO
        // Bring up the aerosol package for the C++ engine: initialize the
        // Kokkos runtime, load the configured species YAML, and resolve every
        // settling species to a package index (fail-fast on any unresolved
        // species).  Called from init() only when the C++ engine is selected.
        void init_cpp_engine(std::shared_ptr<StateManager> state, const std::string& config_path);

        // Run the ported settling science for one step over the whole grid
        // block, reading the environment/state fields and writing tendencies
        // plus the per-species diagnostics.  Called from run() when the C++
        // engine is selected.
        void run_cpp_engine(std::shared_ptr<StateManager> state);
#endif

    public:
        /** @brief Constructs a settling process with the default scheme settings. */
        SettlingProcess();
        /**
         * @brief Releases the Mie-table context and the native engine owned by this process.
         *
         * Out-of-line so the pimpl type is complete at the destruction point.
         */
        ~SettlingProcess() override;

        /**
         * @brief Returns the canonical process name ("settling").
         *
         * @copydoc ProcessInterface::get_name
         */
        std::string get_name() const override { return "settling"; }

        /**
         * @brief Describes the host fields read and written by settling.
         *
         * @return A ProcessContract for "settling" with host 3-D reads of T (K),
         *         AIRDEN (kg/m3), DELP (Pa), PMID (Pa), RH (1), interface-axis Z
         *         (m), and species-native CONCENTRATION (read/write).
         *
         * @note Fields are deliberately advertised as Host accesses: the Fortran
         *       bridge mutates host concentration memory directly, and device
         *       intent would let the execution plan mark the device copy as the
         *       latest writer, letting a later host sync clobber settling results.
         */
        ProcessContract get_contract() const override;

        /**
         * @brief Derives required host inputs before contract validation.
         *
         * @param state Shared simulation state; retained only for the call.
         *              GOCART settling requires DELP, AIRDEN, and RH, all three
         *              of which are optional host products, so this makes them
         *              process-owned and current. Throws std::runtime_error from
         *              the derive_* helpers when their PMID/T/QV prerequisites
         *              are absent.
         */
        void prepare_inputs(std::shared_ptr<StateManager> state) override;

        /**
         * @brief Reads processes.settling (scheme, diagnostics, diag_species, and the
         *        gocart tuning options) and prepares immutable species and table mappings.
         *
         * @param state Shared simulation state; must expose a non-null
         *              config_manager(), a loaded catalog with settling-classified
         *              aerosols (is_aerosol && do_settling), and — on the
         *              simple_scheme path — a readable `mie:` table section.
         *
         * @note Throws std::invalid_argument for a missing/unsupported scheme,
         *       non-positive gocart/scale_factor, swelling_method outside {1,2},
         *       unreadable optics directories/tables, or a simple_scheme species
         *       whose __mie_name matches no configured table; std::runtime_error
         *       when a Mie table fails to open (Fortran rc) or a settling
         *       species lacks explicit radius/density. Radii cross the bridge in
         *       micrometres as configured; the legacy scheme converts to metres.
         */
        void init(std::shared_ptr<StateManager> state) override;

        /**
         * @brief Executes settling for the current host interval.
         *
         * @param state Shared simulation state. run() re-invokes
         *              prepare_inputs() for direct API users, gathers host field
         *              pointers, and calls the Fortran kernel under the
         *              process-wide bridge mutex.
         *
         * @note No-op (logged) when the mechanism has no settling aerosols. When
         *       the legacy fortran_callback is installed it replaces the real
         *       bridge entirely. Throws std::runtime_error via
         *       require_field_pointer() on missing fields, on bridge_rc == 2
         *       (unresolved Mie mapping), or any other nonzero bridge_rc. Writes
         *       CONCENTRATION in place, marks it host-modified, and emits
         *       `CATCHEM_BUDGET name=settling_column_mass` to the log when the
         *       flux diagnostic is active.
         */
        void run(std::shared_ptr<StateManager> state) override;

        /**
         * @brief Releases resources owned by this initialized process instance.
         *
         * @note Frees the Mie-table handle (if any) under the bridge mutex and
         *       zeroes it; safe to call twice — the destructor repeats the guard.
         */
        void finalize() override;

        // For legacy tests
        /**
         * @brief Installs the compatibility callback used by legacy tests.
         *
         * @param cb Receives the raw StateManager pointer when run() executes;
         *           the process does not copy any state into it. Passing an
         *           empty std::function restores the real Fortran bridge path.
         */
        void set_fortran_bridge_callback(std::function<void(void*)> cb);
    };

} // namespace catchem

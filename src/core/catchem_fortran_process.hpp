/**
 * @file catchem_fortran_process.hpp
 * @brief Minimal ProcessInterface adapter for Fortran bridge callbacks.
 *
 * @details Schemes implemented in Fortran (GOCART-era dust, wetdep, ...) are
 * scheduled through this wrapper: Core calls run(), the wrapper syncs the
 * bound Kokkos views down to host memory, invokes the BIND(C) callback with
 * the raw StateManager pointer, marks the unified concentration array
 * host-modified when the callback may have written it, and syncs back up.
 * The callback therefore sees ordinary host pointers through the C ABI and
 * must not retain the state pointer past the call.
 * @see ProcessInterface for the lifecycle contract this class implements.
 */
// src/core/catchem_fortran_process.hpp
#pragma once
#include "catchem_process_interface.hpp"
#include <memory>
#include <string>

namespace catchem {

    // C-linkage declarations matching Fortran bridge callbacks
    extern "C" {
    /// Fortran bridge entry point; receives the raw StateManager pointer and
    /// reads/writes fields through the C-callable state accessors.
    typedef void (*FortranBridgeCallback)(void* state_mgr);
    }

    /**
     * @brief Process wrapper that delegates run() to a Fortran bridge callback.
     *
     * @details Non-copyable by construction (holds a plain function pointer);
     * owned by Core's schedule. init()/finalize() are intentionally empty: the
     * Fortran side holds no per-process resources beyond the state fields.
     */
    class FortranProcess : public ProcessInterface {
    private:
        std::string name;                    ///< Schedule name returned by get_name().
        FortranBridgeCallback bridge_callback; ///< Fortran entry point; null means a no-op run.

    public:
        /**
         * @brief Wrap a Fortran bridge callback as a schedulable process.
         * @param process_name Schedule name, e.g. "wetdep".
         * @param callback     Fortran entry point; may be null, which makes
         *                     run() a pure sync round-trip.
         */
        FortranProcess(const std::string& process_name, FortranBridgeCallback callback)
            : name(process_name), bridge_callback(callback) {}

        /// @copydoc ProcessInterface::get_name
        std::string get_name() const override { return name; }

        /// @copydoc ProcessInterface::init
        void init([[maybe_unused]] std::shared_ptr<StateManager> state) override {
            // Initial setup if required
        }

        /**
         * @brief Run the Fortran bridge with host-consistent views.
         * @param state Shared StateManager whose bound fields are synced to
         *              host before, and back to device after, the callback.
         * @note The callback runs on host memory only. When a unified
         *       concentration array is bound it is marked host-modified
         *       afterwards, so the next sync_to_device() pushes Fortran's
         *       writes back to any device mirror.
         * @see ProcessInterface::run for the lifecycle position.
         */
        void run(std::shared_ptr<StateManager> state) override {
            // 1. Sync device Views to host unified memory
            state->sync_to_host();

            // 2. Invoke the Fortran bridging callback
            if (bridge_callback) {
                bridge_callback(static_cast<void*>(state.get()));
                if (state->chemistry().conc)
                    state->chemistry().conc->mark_host_modified();
            }

            // 3. Sync modified host buffers back to device Views
            state->sync_to_device();
        }

        /// @copydoc ProcessInterface::finalize
        void finalize() override {
            // Cleanup if required
        }
    };

} // namespace catchem

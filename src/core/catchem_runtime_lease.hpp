/**
 * @file catchem_runtime_lease.hpp
 * @brief Reference-counted lease over the process-global execution runtime.
 *
 * @details CATChem may either use a Kokkos runtime the host model already
 * initialized (HostOwned), initialize and finalize one itself (CATChemOwned),
 * or run without any backend (Disabled, the host-only build). RuntimeLease
 * tracks that decision with one process-wide snapshot guarded by a mutex:
 * the first lease to arrive decides the mode, later leases must agree with
 * it, and the runtime is only finalized on request once every lease has
 * been released. In host-only builds every lease is Disabled and finalize
 * is a no-op.
 */
#pragma once

#include "catchem_error.hpp"
#include <cstddef>
#include <memory>

namespace catchem {

    /// Who owns the execution runtime, decided by the first active lease.
    enum class RuntimeMode { Disabled, HostOwned, CATChemOwned };
    /// Lifecycle of the process-global runtime snapshot.
    enum class RuntimeState { Unavailable, Initialized, FinalizePending, Finalized };

    /**
     * @brief Point-in-time view of the shared runtime state (copy, not a handle).
     */
    struct RuntimeSnapshot {
        RuntimeMode mode = RuntimeMode::Disabled;       ///< Mode chosen by the first lease.
        RuntimeState state = RuntimeState::Unavailable; ///< Whether the backend is usable or already finalized.
        std::size_t lease_count = 0;                    ///< Live leases pinning the runtime.
        bool initialized_by_catchem = false;            ///< True when CATChemOwned called Kokkos::initialize().
    };

    /**
     * @brief Move-only RAII lease on the process-global execution runtime.
     *
     * @details Constructed by Core::initialize() before any Kokkos view is
     * created and released by Core::shutdown(). Copying is deleted so the
     * lease count stays honest; moving transfers the single count. All state
     * uses process-wide static variables behind a mutex, so leases from different threads
     * contend safely but the mode decision is first-writer-wins.
     * @see catchem_kokkos_compat.hpp for the host-only build that skips Kokkos.
     */
    class RuntimeLease {
    public:
        RuntimeLease() noexcept = default;
        /**
         * @brief Acquire a lease, deciding or joining the runtime mode.
         * @param mode Requested ownership (ignored in host-only builds, which
         *             are always Disabled).
         * @throws std::runtime_error when the runtime was already finalized,
         *         when HostOwned is requested before the host initialized
         *         Kokkos, or when Disabled conflicts with active leases.
         */
        explicit RuntimeLease(RuntimeMode mode);
        RuntimeLease(const RuntimeLease&) = delete;
        RuntimeLease& operator=(const RuntimeLease&) = delete;
        /// Steal @p other's lease slot; @p other becomes inactive.
        RuntimeLease(RuntimeLease&& other) noexcept;
        /// Releases any held lease, then steals @p other's slot.
        RuntimeLease& operator=(RuntimeLease&& other) noexcept;
        /// Releases the held lease, if any.
        ~RuntimeLease();

        /// True when this object currently holds a lease count.
        bool active() const noexcept { return active_; }
        /// The mode the lease was taken under; Disabled when inactive.
        RuntimeMode mode() const noexcept { return mode_; }
        /**
         * @brief Drop this lease's count exactly once.
         * @note When the last lease of a HostOwned runtime is released the
         *       snapshot resets to defaults so a later request can re-decide
         *       the mode; CATChemOwned keeps its Initialized state so
         *       request_finalize() can still run.
         */
        void release() noexcept;

        /// Thread-safe copy of the current process-wide runtime snapshot.
        static RuntimeSnapshot snapshot() noexcept;
        /**
         * @brief Finalize the runtime, if CATChem owns it and nothing is leased.
         * @return Success after the backend is finalized and the state is
         *         Finalized, or InvalidState when the mode is not CATChemOwned
         *         or any lease is still active.
         * @note Finalized is sticky: constructing a further lease afterwards
         *       throws rather than silently re-initializing Kokkos.
         */
        static BoundaryStatus request_finalize() noexcept;

    private:
        RuntimeMode mode_ = RuntimeMode::Disabled; ///< Mode this lease holds (or last held).
        bool active_ = false;                      ///< Whether this object contributes to lease_count.
    };

} // namespace catchem

/**
 * @file catchem_handle_registry.hpp
 * @brief Generation-aware registry that validates opaque C-API handles.
 *
 * @details Host models pass `void*` handles to the flat C API; this registry
 * turns that unchecked pointer into a validated, type-tagged admission. Every
 * checked entry point acquires an AdmissionLease before dereferencing a handle
 * and holds it for the duration of the call, which gives the registry a
 * use-count it can drain: close_and_wait() marks a handle closing, removes it
 * from the map, and blocks until every outstanding lease has released, so a
 * destroy racing a concurrent call cannot free memory another thread is using.
 *
 * Ownership is expressed through the record's owner pointer: a child handle
 * (e.g. the StateManager obtained from a Core) names its parent, and acquire()
 * leases parent and child together in a deterministic lock order (parent
 * first). invalidate_children() drops dependent handles when the parent dies,
 * so a stale child pointer is later reported as InvalidHandle rather than
 * dereferencing freed memory.
 */
#pragma once
#include "catchem_error.hpp"
#include <condition_variable>
#include <cstdint>
#include <memory>
#include <mutex>
#include <unordered_map>
#include <utility>
#include <vector>

namespace catchem {
    /// The C++ object a registry handle wraps. Each checked entry point names
    /// the type it expects, so a mismatch is reported instead of mis-cast.
    enum class HandleType : std::uint32_t { Core = 1, State = 2, Diagnostic = 3, Configuration = 4, Time = 5, Manifest = 6 };
    /// Whether destroying the handle also deletes the object it points at.
    /// Owned: created by a catchem_*_create/open entry point. Borrowed: an
    /// accessor result such as a Core's StateManager, deleted by its parent.
    enum class HandleOwnership : std::uint8_t { Owned, Borrowed };
    /// Static identity of one registered handle, fixed at registration.
    struct HandleRecord {
        HandleType type = HandleType::Core;             ///< Object kind the pointer wraps.
        std::uint32_t contract_version = 1;             ///< API struct version the handle was created with.
        std::uint64_t generation = 0;                   ///< Mutation counter checked by acquire().
        const void* owner = nullptr;                    ///< Parent handle borrowed from; null for standalone handles.
        HandleOwnership ownership = HandleOwnership::Borrowed; ///< Whether the close path deletes the pointee.
    };

    /**
     * @brief Move-only RAII proof that a handle was admitted.
     *
     * @details Obtained only from HandleRegistry::acquire(); holding it means
     * the registry counts this call as active use of every leased control
     * (parent included). Destruction decrements the counts and notifies any
     * close_and_wait() blocked on a closing control, so the lease must outlive
     * every dereference of the handle. Copying is deleted to keep exactly one
     * owner per admission; move transfers that ownership.
     */
    class AdmissionLease {
        struct Control {
            explicit Control(HandleRecord value) : record(std::move(value)) {}
            HandleRecord record;               ///< Immutable identity of the handle.
            std::mutex mutex;                  ///< Guards active/closing.
            std::condition_variable drained;   ///< Signalled when a closing control reaches zero active leases.
            std::size_t active = 0;            ///< Number of leases currently held.
            bool closing = false;              ///< Set by close_and_wait() to refuse new admissions.
        };
        friend class HandleRegistry;
        std::vector<std::shared_ptr<Control>> controls_; ///< Leased controls, parent first.
        explicit AdmissionLease(std::vector<std::shared_ptr<Control>> value) : controls_(std::move(value)) {}

    public:
        AdmissionLease() = default;
        AdmissionLease(const AdmissionLease&) = delete;
        AdmissionLease& operator=(const AdmissionLease&) = delete;
        AdmissionLease(AdmissionLease&&) noexcept = default;
        /// Releases any lease this lease holds, then takes over @p other's.
        AdmissionLease& operator=(AdmissionLease&& other) noexcept {
            if (this != &other) {
                release();
                controls_ = std::move(other.controls_);
            }
            return *this;
        }
        ~AdmissionLease() { release(); }
        /// True when this lease holds at least one admitted control.
        explicit operator bool() const noexcept { return !controls_.empty(); }
        /**
         * @brief Drop every held admission, in reverse (child-first) order.
         *
         * @details Each control's active count is decremented under its own
         * lock; a closing control with no remaining leases wakes its
         * close_and_wait(). Idempotent: releasing an empty lease does nothing,
         * which is what makes move-assignment safe.
         */
        void release() noexcept {
            for (auto iterator = controls_.rbegin(); iterator != controls_.rend(); ++iterator) {
                const auto& control = *iterator;
                std::lock_guard<std::mutex> lock(control->mutex);
                if (control->active > 0)
                    --control->active;
                if (control->closing && control->active == 0)
                    control->drained.notify_all();
            }
            controls_.clear();
        }
    };

    /**
     * @brief Process-wide map from opaque pointer to handle identity.
     *
     * @details Meyers singleton: the C API has no host-visible place to carry
     * registry state, so the pointer itself is the key. Every method locks the
     * registry mutex, and acquire() additionally takes per-control locks in
     * parent-then-child order (released before returning) so concurrent
     * admissions of related handles cannot deadlock.
     */
    class HandleRegistry {
        using Control = AdmissionLease::Control;

    public:
        /// The one registry instance for the process.
        static HandleRegistry& instance() {
            static HandleRegistry registry;
            return registry;
        }
        /**
         * @brief Register a new handle.
         *
         * @param handle  Pointer that hosts will pass back; must be non-null.
         * @param record  Identity (type, generation, owner, ownership) fixed here.
         * @return true when inserted; false for a null pointer or a pointer
         *         already registered (the first registration wins).
         */
        bool add(const void* handle, HandleRecord record) {
            if (!handle)
                return false;
            std::lock_guard<std::mutex> lock(mutex_);
            return records_.emplace(handle, std::make_shared<Control>(std::move(record))).second;
        }
        /**
         * @brief Admit a handle for use, taking a lease on it and its parent.
         *
         * @param handle     Pointer supplied by the host.
         * @param expected   HandleType the caller requires.
         * @param owner      When non-null, the record's owner must equal it;
         *                   otherwise the handle is rejected as InvalidHandle.
         * @param generation When non-zero, the record's generation must equal
         *                   it; a mismatch is StaleGeneration. Zero skips the
         *                   check, which is what the C boundary uses.
         * @return Success plus a live AdmissionLease, or a failure status with
         *         an empty lease: NullArgument for a null pointer,
         *         InvalidHandle for an unregistered pointer, a mismatched
         *         owner, a missing parent record, or a handle already closing,
         *         WrongHandleType for a type mismatch, and StaleGeneration for
         *         a generation mismatch.
         *
         * @note The lease must be held for as long as the pointee is used;
         *       destruction of the lease is what unblocks a concurrent
         *       close_and_wait().
         */
        std::pair<BoundaryStatus, AdmissionLease> acquire(const void* handle, HandleType expected,
                                                          const void* owner = nullptr,
                                                          std::uint64_t generation = 0) noexcept {
            if (!handle)
                return {BoundaryStatus::NullArgument, AdmissionLease{}};
            std::vector<std::shared_ptr<Control>> controls;
            {
                std::lock_guard<std::mutex> registry_lock(mutex_);
                const auto found = records_.find(handle);
                if (found == records_.end())
                    return {BoundaryStatus::InvalidHandle, AdmissionLease{}};
                const auto& record = found->second->record;
                if (record.type != expected)
                    return {BoundaryStatus::WrongHandleType, AdmissionLease{}};
                if (owner && record.owner != owner)
                    return {BoundaryStatus::InvalidHandle, AdmissionLease{}};
                if (generation && record.generation != generation)
                    return {BoundaryStatus::StaleGeneration, AdmissionLease{}};
                if (record.owner) {
                    const auto parent = records_.find(record.owner);
                    if (parent == records_.end())
                        return {BoundaryStatus::InvalidHandle, AdmissionLease{}};
                    controls.push_back(parent->second);
                }
                controls.push_back(found->second);
                for (const auto& control : controls)
                    control->mutex.lock();
                bool closing = false;
                for (const auto& control : controls)
                    closing = closing || control->closing;
                if (!closing)
                    for (const auto& control : controls)
                        ++control->active;
                for (auto iterator = controls.rbegin(); iterator != controls.rend(); ++iterator)
                    (*iterator)->mutex.unlock();
                if (closing)
                    return {BoundaryStatus::InvalidHandle, AdmissionLease{}};
            }
            return {BoundaryStatus::Success, AdmissionLease(std::move(controls))};
        }
        /**
         * @brief Check a handle's identity without keeping it alive.
         *
         * @param handle     Pointer to validate.
         * @param expected   Required HandleType.
         * @param owner      Required owner, or null to skip the check.
         * @param generation Required generation, or 0 to skip the check.
         * @return The status acquire() would have produced; any lease taken
         *         during the check is released immediately, so this does not
         *         protect a later dereference.
         */
        BoundaryStatus validate(const void* handle, HandleType expected, const void* owner = nullptr,
                                std::uint64_t generation = 0) const noexcept {
            auto acquired = const_cast<HandleRegistry*>(this)->acquire(handle, expected, owner, generation);
            return acquired.first;
        }
        /**
         * @brief Retire a handle and wait for every in-flight user to finish.
         *
         * @details The record is marked closing (so no new lease is admitted)
         * and erased from the map while the registry lock is held, then this
         * blocks on the control's drained condition until active reaches zero.
         * The caller owns the pointee and must delete it after this returns.
         * @param handle   Pointer to retire.
         * @param expected HandleType it must have been registered as.
         * @return Success; NullArgument for a null pointer; InvalidHandle for
         *         an unregistered pointer; WrongHandleType for a type mismatch.
         */
        BoundaryStatus close_and_wait(const void* handle, HandleType expected) noexcept {
            std::shared_ptr<Control> control;
            {
                std::lock_guard<std::mutex> registry_lock(mutex_);
                const auto found = records_.find(handle);
                if (found == records_.end())
                    return handle ? BoundaryStatus::InvalidHandle : BoundaryStatus::NullArgument;
                if (found->second->record.type != expected)
                    return BoundaryStatus::WrongHandleType;
                control = found->second;
                {
                    std::lock_guard<std::mutex> lock(control->mutex);
                    control->closing = true;
                }
                records_.erase(found);
            }
            std::unique_lock<std::mutex> lock(control->mutex);
            control->drained.wait(lock, [&] { return control->active == 0; });
            return BoundaryStatus::Success;
        }
        /// Erase a handle's record without waiting for outstanding leases.
        /// Used for rollback paths where the object was never published.
        void remove(const void* handle) noexcept {
            std::lock_guard<std::mutex> lock(mutex_);
            records_.erase(handle);
        }
        /**
         * @brief Drop every child handle borrowed from @p owner.
         *
         * @param owner Pointer of the parent being destroyed.
         *
         * @note Erases records only; it does not wait for leases on children
         * that are in flight, so call it from the destroy path after the
         * parent's own close_and_wait() has drained the parent's users.
         */
        void invalidate_children(const void* owner) noexcept {
            std::lock_guard<std::mutex> lock(mutex_);
            for (auto iterator = records_.begin(); iterator != records_.end();) {
                if (iterator->second->record.owner == owner)
                    iterator = records_.erase(iterator);
                else
                    ++iterator;
            }
        }
        /// Number of currently registered handles; intended for diagnostics
        /// and tests, not for making lifecycle decisions.
        std::size_t size() const noexcept {
            std::lock_guard<std::mutex> lock(mutex_);
            return records_.size();
        }

    private:
        mutable std::mutex mutex_;                                        ///< Guards the records_ map.
        std::unordered_map<const void*, std::shared_ptr<Control>> records_; ///< Handle pointer to its control block.
    };
} // namespace catchem

/**
 * @file catchem_diagnostic_manager.hpp
 * @brief Registry, generation clock and view accessor for diagnostic fields.
 *
 * @details Owns every DiagnosticField by shared_ptr under its exact registered
 * name and advances a single generation counter at each timestep
 * or host interval, which is how stale/failed diagnostic output is detected:
 * consumers compare a field's last_write_generation against generation(), and
 * mark_generation_failed() flags the current generation when a process threw.
 * Registration is contract-checked (see register_field_contract) and repeated
 * registration of an identical field is idempotent; a conflicting re-register
 * throws. get_registered_names() returns insertion order, not map order, so
 * NetCDF output is deterministic.
 *
 * Not internally locked: Core's lifecycle mutex serialises registration,
 * generation advance and view access against execution.
 * @see catchem_diagnostic.hpp for the per-field storage and sync semantics.
 */
// src/core/catchem_diagnostic_manager.hpp
#pragma once
#include "catchem_diagnostic.hpp"
#include <memory>
#include <string>
#include <cstdint>
#include <unordered_map>

namespace catchem {

    /**
     * @brief The set of registered diagnostic fields for one simulation.
     *
     * @details Owned by Core through std::shared_ptr and handed to processes
     * and the C API; the manager must outlive every view or pointer it returns.
     */
    class DiagnosticManager {
    private:
        std::unordered_map<std::string, std::shared_ptr<DiagnosticField>> fields; ///< Canonical name to field.

    public:
        DiagnosticManager() = default;

        /**
         * @brief Register a field with inferred axes and instantaneous reset.
         * @param name  Field name; the exact string is the map key.
         * @param desc  Human-readable description.
         * @param units CF-style units string.
         * @param type  FIELD_2D or FIELD_3D.
         * @param dims  Extent per dimension; length must equal the rank.
         * @note Legacy shim: it infers Column/Level-or-Singleton/Species and
         *       cannot supply packed labels, so it registers with the strict
         *       label checks disabled. Prefer register_field_contract().
         * @throws std::invalid_argument from the contract path on a bad shape.
         */
        void register_field(const std::string& name, const std::string& desc, const std::string& units, DiagType type,
                            const std::vector<int>& dims);
        /**
         * @brief Register a field with an explicit, validated contract.
         *
         * @param name          Field name; the exact string is the map key.
         * @param desc          Human-readable description; must be non-empty.
         * @param units         CF-style units; must be non-empty.
         * @param type          FIELD_2D or FIELD_3D.
         * @param dims          Extent per dimension; all positive, size == rank.
         * @param policy        Instantaneous, TimestepAccumulated or Persistent.
         * @param reset_value   Fill value applied by reset() [same units as the field].
         * @param axes          Semantic axis per dimension; axis 0 must be Column.
         * @param unpack_labels One label per slot of the packed Species/Category
         *                      axis; empty when no axis is packed.
         * @param strict_labels When false (the legacy shim path) the packed-axis
         *                      and label checks are skipped; the writer still
         *                      fails loud at output time if unpacking is needed.
         *
         * @throws std::invalid_argument on an incomplete contract, a violated
         *         unpacking invariant, an unsafe/duplicate label, or a
         *         re-registration whose shape, units, axes, policy or labels
         *         conflict with the existing field.
         * @note Registering an identical contract again is a silent no-op, which
         *       lets processes re-declare diagnostics after a schedule rebuild.
         */
        void register_field_contract(const std::string& name, const std::string& desc, const std::string& units,
                                     DiagType type, const std::vector<int>& dims, DiagnosticPolicy policy,
                                     double reset_value, const std::vector<SemanticAxis>& axes,
                                     const std::vector<std::string>& unpack_labels = {}, bool strict_labels = true);

        /// True when a field is registered under exactly this name string.
        bool has_field(const std::string& name) const;
        /**
         * @brief Shared ownership of one registered field.
         * @param name Field name; exact-match lookup (no case folding).
         * @return The field's shared_ptr; keep it only while the manager lives.
         * @throws std::invalid_argument when the name is unknown.
         */
        std::shared_ptr<DiagnosticField> get_field(const std::string& name);
        /// Semantic axis per dimension (length == rank). Throws on unknown name.
        const std::vector<SemanticAxis>& get_axes(const std::string& name) const;
        /// Labels for the single packed (Species/Category) dimension; empty when the
        /// field has none. Throws on unknown name.
        const std::vector<std::string>& get_unpack_labels(const std::string& name) const;

#ifdef CATCHEM_ENABLE_KOKKOS
        /**
         * @brief Device-ready LayoutLeft view of a 2D diagnostic.
         * @param name Field name (exact match).
         * @return Shallow Kokkos::View copy referencing the field's device
         *         storage; valid while the manager and field live.
         * @note Host contents are copied to device first when the host wrote
         *       last, and the field is then marked device-current.
         * @throws std::invalid_argument when unknown or not 2D.
         */
        Kokkos::View<double**, Kokkos::LayoutLeft, Kokkos::DefaultExecutionSpace::memory_space>
        get_device_view_2d(const std::string& name);

        /// @copydoc get_device_view_2d (3D rank)
        Kokkos::View<double***, Kokkos::LayoutLeft, Kokkos::DefaultExecutionSpace::memory_space>
        get_device_view_3d(const std::string& name);
#else
        /**
         * @brief LayoutLeft mdspan over a 2D diagnostic's storage.
         * @param name Field name (exact match).
         * @return Non-owning view over the field's vector; valid while the
         *         field keeps its dimensions. Marks the field device-current
         *         (a no-op sync in host-only builds).
         * @throws std::invalid_argument when unknown or not 2D.
         */
        DiagnosticField::Mdspan2D get_device_view_2d(const std::string& name);

        /// @copydoc get_device_view_2d (3D rank)
        DiagnosticField::Mdspan3D get_device_view_3d(const std::string& name);
#endif

        /**
         * @brief Host pointer for reading a diagnostic's values.
         * @param name Field name (exact match).
         * @return Borrowed, N-dimensional LayoutLeft storage pointer, synced
         *         from device when needed; null for unsupported types. Valid
         *         until the field is destroyed or resized.
         * @throws std::invalid_argument when the name is unknown.
         */
        const void* get_host_read_pointer(const std::string& name);
        /**
         * @brief Host pointer for writing a diagnostic's values.
         * @param name Field name (exact match).
         * @return Same pointer as get_host_read_pointer(), but the field is
         *         marked host-current afterwards, so the next generation
         *         advance or device view request propagates the writes.
         * @throws std::invalid_argument when the name is unknown.
         */
        void* get_host_write_pointer(const std::string& name);
        /// Read/write host pointer alias; prefer the explicit read/write variants.
        void* get_host_pointer(const std::string& name) { return const_cast<void*>(get_host_read_pointer(name)); }
        /// Mark one field host-written without handing out a pointer.
        void mark_host_modified(const std::string& name);
        /// Mark one field device-written without handing out a view.
        void mark_device_modified(const std::string& name);
        /// Sync every field device -> host (no-ops where already consistent).
        void sync_to_host();
        /// Sync every field host -> device (no-ops where already consistent).
        void sync_to_device();
        /// Reset every field to its reset_value, regardless of policy.
        void reset_all();
        /**
         * @brief Start a new timestep: advance the generation for all fields.
         * @note Zeroes the interval memory so begin_interval() treats the next
         *       id as new again; use this for Core-driven steps.
         */
        void begin_timestep();
        /**
         * @brief Start a host-owned interval, advancing the generation once per id.
         * @param interval_id Host interval identity; must be non-zero. Repeating
         *                    the same id is idempotent (no double advance).
         * @throws std::invalid_argument when @p interval_id is zero, which is
         *         reserved for the timestep path.
         */
        void begin_interval(std::uint64_t interval_id);
        /// Flag the current generation as failed on every field at that generation.
        void mark_generation_failed();
        /// The generation stamp applied to fields at the last advance.
        std::size_t generation() const noexcept { return generation_; }
        /// Registered names in registration order, for deterministic output.
        std::vector<std::string> get_registered_names() const;

    private:
        std::size_t generation_ = 0;               ///< Monotonic generation counter over timesteps/intervals.
        std::uint64_t last_interval_id_ = 0;       ///< Most recent interval id, for idempotent begin_interval().
        /// Insertion order of field names. `fields` is an unordered_map, so this
        /// side-list is what makes get_registered_names() (and therefore the
        /// axes-driven diagnostic writer's variable order) deterministic.
        std::vector<std::string> registration_order_;
    };

} // namespace catchem

/**
 * @file catchem_diagnostic.hpp
 * @brief One registered diagnostic output field with host/device mirrors.
 *
 * @details A DiagnosticField owns managed storage for one named diagnostic
 * (2D column or 3D column x level/species). Under Kokkos it keeps a device
 * view and a host view (both LayoutLeft, Fortran-compatible); in host-only
 * builds a single std::vector backs both roles and the mdspan accessors serve
 * as "views". Freshness is tracked three ways: a generation counter advanced
 * by DiagnosticManager each timestep/interval, a LatestWriter tag saying
 * which mirror was written last (syncs are no-ops when already consistent),
 * and an AvailabilityState for consumers. The reset policy decides whether
 * values are re-initialised when the generation advances.
 */
// src/core/catchem_diagnostic.hpp
#pragma once
#include "catchem_field_contract.hpp"
#include "catchem_interop_field.hpp"
#include "catchem_kokkos_compat.hpp"
#include <string>
#include <vector>

namespace catchem {

    /// Shape of a registered diagnostic; SCALAR and FIELD_1D are declared but
    /// only 2D and 3D are constructible today (the constructor rejects others).
    enum class DiagType { SCALAR, FIELD_1D, FIELD_2D, FIELD_3D };
    /// How a diagnostic's values behave when the generation advances.
    enum class DiagnosticPolicy { Instantaneous, TimestepAccumulated, Persistent };

    /**
     * @brief Managed storage, metadata and freshness state for one diagnostic.
     *
     * @details Owned through std::shared_ptr by DiagnosticManager; hosts get
     * raw pointers or views that stay valid while the manager lives and the
     * field keeps its name. Not thread-safe by itself: mutation happens under
     * Core's lifecycle mutex during scheduled execution.
     */
    class DiagnosticField {
    public:
        /// LayoutLeft mdspan aliases usable in both build flavors.
        using Mdspan2D = typename MdspanTypeHelper<double, 2>::type;
        using Mdspan3D = typename MdspanTypeHelper<double, 3>::type;

        std::string name;                    ///< Registered field name; the manager's lookup key.
        std::string description;             ///< Human-readable description for output metadata.
        std::string units;                   ///< CF-style units string, e.g. "kg m-2 s-1".
        DiagType type;                       ///< 2D or 3D shape; fixed at construction.
        std::vector<int> dimensions;         ///< Extent per dimension, outermost-first as registered.
        std::vector<SemanticAxis> axes;      ///< Semantic axis per dimension (length == rank).
        /// Human-readable label for each position of the field's single packed
        /// (Species or Category) dimension; empty when no dimension is packed.
        /// Index i is the 0-based slot the scheme writes, NOT a global species index.
        std::vector<std::string> unpack_labels;
        std::size_t generation = 0;                    ///< Current generation copied from the manager at advance time.
        std::size_t registration_generation = 0;       ///< Generation in effect when the field was registered.
        /// Generation in which a producer last wrote this diagnostic.
        std::size_t last_write_generation = 0;
        AvailabilityState availability = AvailabilityState::Unavailable; ///< Whether consumers may read current values.
        bool generation_failed = false;                ///< Set when a process failed during this generation.
        LatestWriter latest_writer = LatestWriter::Uninitialized; ///< Mirror written last; drives sync no-ops.
        DiagnosticPolicy reset_policy = DiagnosticPolicy::Instantaneous; ///< Whether reset() runs on advance.
        double reset_value = 0.0;                      ///< Fill value used by reset().

#ifdef CATCHEM_ENABLE_KOKKOS
        /// Device/host memory spaces; equal when Kokkos runs on host only.
        using HostSpace = Kokkos::HostSpace;
        using DeviceSpace = Kokkos::DefaultExecutionSpace::memory_space;

        /// LayoutLeft device view over the 2D storage (Fortran order).
        using View2D = Kokkos::View<double**, Kokkos::LayoutLeft, DeviceSpace>;
        /// LayoutLeft host mirror of the 2D storage.
        using HostView2D = Kokkos::View<double**, Kokkos::LayoutLeft, HostSpace>;

        /// LayoutLeft device view over the 3D storage.
        using View3D = Kokkos::View<double***, Kokkos::LayoutLeft, DeviceSpace>;
        /// LayoutLeft host mirror of the 3D storage.
        using HostView3D = Kokkos::View<double***, Kokkos::LayoutLeft, HostSpace>;

        View2D device_view_2d; ///< Device-resident 2D view (aliases host view when no GPU).
        HostView2D host_view_2d; ///< Host mirror of the 2D storage.

        View3D device_view_3d; ///< Device-resident 3D view (aliases host view when no GPU).
        HostView3D host_view_3d; ///< Host mirror of the 3D storage.
#else
        // Host-only builds own their storage directly; mdspans over `storage`
        // serve as both the "host" and "device" views.
        std::vector<double> storage; ///< Zero-initialised flat storage, LayoutLeft order.
#endif

        /// True when host and device memory spaces differ, i.e. real copies happen.
        bool is_gpu_target;

        /**
         * @brief Allocate and initialise the field's storage.
         *
         * @param name_val     Registered field name.
         * @param desc_val     Human-readable description.
         * @param units_val    CF-style units string.
         * @param type_val     FIELD_2D or FIELD_3D; anything else throws.
         * @param dims         Extent per dimension; size must equal the rank.
         * @param policy       Reset behaviour on generation advance (default
         *                     Instantaneous).
         * @param reset        Fill value used by reset() (default 0.0).
         * @param semantic_axes Axis per dimension; stored as given (the manager
         *                     validates the contract before constructing).
         * @param labels       Per-slot labels for a packed Species/Category axis.
         * @throws std::invalid_argument on a rank/dims mismatch or unsupported
         *         DiagType.
         */
        DiagnosticField(const std::string& name_val, const std::string& desc_val, const std::string& units_val,
                        DiagType type_val, const std::vector<int>& dims,
                        DiagnosticPolicy policy = DiagnosticPolicy::Instantaneous, double reset = 0.0,
                        std::vector<SemanticAxis> semantic_axes = {}, std::vector<std::string> labels = {});

        /// Copy device to host when the device wrote last; no-op otherwise.
        void sync_to_host();
        /// Copy host to device when the host wrote last; no-op otherwise.
        void sync_to_device();
        /// Fill both mirrors with reset_value and clear the write generation.
        void reset();
        /**
         * @brief Advance to a new generation and apply the reset policy.
         * @param value New generation counter from the manager.
         * @note Non-persistent fields are reset(); Persistent ones keep their
         *       values. Availability becomes Current in both cases.
         */
        void advance_generation(std::size_t value);
        /** @brief Record that the current diagnostic generation was written on host memory. */
        void mark_host_modified() {
            latest_writer = LatestWriter::HostCurrent;
            last_write_generation = generation;
            availability = AvailabilityState::Current;
        }
        /** @brief Record that the current diagnostic generation was written on device memory. */
        void mark_device_modified() {
            latest_writer = LatestWriter::DeviceCurrent;
            last_write_generation = generation;
            availability = AvailabilityState::Current;
        }
    };

} // namespace catchem

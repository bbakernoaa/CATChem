#pragma once
#include "catchem_field_contract.hpp"
#include "catchem_kokkos_compat.hpp"
#include <array>
#include <memory>
#include <stdexcept>
#include <type_traits>
#include <vector>
#ifdef CATCHEM_ENABLE_KOKKOS
#include <mdspan/mdspan.hpp>
#endif

namespace catchem {

    // --- SFINAE template helpers for standard Kokkos::mdspan mapping defined at namespace scope ---

    /**
     * @brief Maps (DataType, Rank) to the matching LayoutLeft mdspan type.
     * @tparam DataType Element type of the view.
     * @tparam Rank     Number of dimensions, 1 to 3; other ranks have no specialization.
     */
    template <typename DataType, int Rank> struct MdspanTypeHelper;

    template <typename DataType> struct MdspanTypeHelper<DataType, 1> {
        using type = Kokkos::mdspan<DataType, Kokkos::extents<int, Kokkos::dynamic_extent>, Kokkos::layout_left>;
    };

    template <typename DataType> struct MdspanTypeHelper<DataType, 2> {
        using type = Kokkos::mdspan<DataType, Kokkos::extents<int, Kokkos::dynamic_extent, Kokkos::dynamic_extent>,
                                    Kokkos::layout_left>;
    };

    template <typename DataType> struct MdspanTypeHelper<DataType, 3> {
        using type =
            Kokkos::mdspan<DataType,
                           Kokkos::extents<int, Kokkos::dynamic_extent, Kokkos::dynamic_extent, Kokkos::dynamic_extent>,
                           Kokkos::layout_left>;
    };

    /**
     * @class InteropField
     * @brief High-performance wrapper mapping host-allocated contiguous pointers to device-capable views.
     *
     * This class translates standard raw pointer buffers across language boundaries (e.g. Fortran to C++).
     * With Kokkos enabled it provides unmanaged LayoutLeft Views plus a managed device mirror; in host-only
     * builds the same interface is served directly by Kokkos::mdspan (standalone library) over the host
     * buffer, and the device-sync operations are no-ops.
     *
     * @tparam DataType Numeric type of the grid elements (typically double).
     * @tparam Rank Dimensional dimensionality of the field (1, 2, or 3).
     */
    template <typename DataType, int Rank> class InteropField {
    public:
        using MdspanType = typename MdspanTypeHelper<DataType, Rank>::type; ///< LayoutLeft mdspan view over the field's memory.
        std::array<std::size_t, Rank> immutable_extents{}; ///< Per-axis sizes fixed at construction; never resized.
        FieldContract contract;                            ///< Semantic description (name, units, extents, axes, persistence).
        std::size_t generation = 0;                        ///< Import generation stamped by set_generation().
        AvailabilityState availability = AvailabilityState::Current; ///< Data validity state for the current import.
        LatestWriter latest_writer = LatestWriter::HostCurrent;      ///< Side holding the newest values; drives sync decisions.
        std::size_t host_to_device_sync_count = 0;         ///< Cumulative host-to-device transfers (statistics).
        std::size_t device_to_host_sync_count = 0;         ///< Cumulative device-to-host transfers (statistics).

        /// Record that the host buffer now holds the newest values (no data movement).
        void mark_host_modified() { latest_writer = LatestWriter::HostCurrent; }
        /// Record that the device mirror now holds the newest values (no data movement).
        void mark_device_modified() { latest_writer = LatestWriter::DeviceCurrent; }

        /**
         * @brief Stamp the field with an import generation and mark it current.
         * @param value Generation counter from StateManager::begin_import_generation().
         */
        void set_generation(std::size_t value) {
            generation = value;
            availability = AvailabilityState::Current;
        }
        /// Mark the data unavailable (timestep-persistent fields age out here).
        void invalidate() { availability = AvailabilityState::Unavailable; }

        /**
         * @brief Test whether the data is current for a given import generation.
         * @param value Generation the caller requires.
         * @return true when availability is Current and generation equals value.
         */
        bool is_current(std::size_t value) const {
            return availability == AvailabilityState::Current && generation == value;
        }

        /**
         * @brief Size of one axis as fixed at construction.
         * @param axis Zero-based axis index; must be in [0, Rank).
         * @return The extent of that axis.
         * @throws std::out_of_range when axis is negative or >= Rank.
         */
        std::size_t extent(int axis) const { return immutable_extents.at(static_cast<std::size_t>(axis)); }

        /**
         * @brief Return a host pointer with the data guaranteed on the host.
         * @return Pointer to the host buffer (referenced, not copied); the
         *         caller must not outlive the field's backing memory.
         *
         * @note sync_to_host() runs first, so a device-written mirror is
         * fetched before the pointer is handed out.
         */
        const DataType* host_read() {
            sync_to_host();
            return host_data();
        }

        /**
         * @brief Return a writable host pointer and mark the host as newest.
         * @return Pointer to the host buffer (referenced, not copied).
         *
         * @note Calls sync_to_host() then mark_host_modified(); writers on
         * the device side must go through device_write() instead.
         */
        DataType* host_write() {
            sync_to_host();
            mark_host_modified();
            return host_data();
        }

        /**
         * @brief Return the execution-space view with the data on the device.
         * @return Kokkos view (or mdspan in host-only builds) referencing the
         *         active memory; shallow, no copy.
         */
        auto device_read() {
            sync_to_device();
            return view();
        }

        /**
         * @brief Return the device view and mark the device as newest.
         * @return Kokkos view (or mdspan in host-only builds) referencing the
         *         active memory; shallow, no copy.
         */
        auto device_write() {
            sync_to_device();
            mark_device_modified();
            return view();
        }

    private:
        /// Validate dims (exactly Rank positive entries), cache them as immutable_extents, and mirror them into contract.extents.
        void validate_dimensions(const std::vector<int>& dims) {
            if (dims.size() != static_cast<std::size_t>(Rank))
                throw std::invalid_argument("InteropField dimension rank mismatch");
            for (int r = 0; r < Rank; ++r) {
                if (dims[r] <= 0)
                    throw std::invalid_argument("InteropField dimensions must be positive");
                immutable_extents[r] = static_cast<std::size_t>(dims[r]);
            }
            contract.extents.assign(immutable_extents.begin(), immutable_extents.end());
        }

    public:
#ifdef CATCHEM_ENABLE_KOKKOS
        using HostSpace = Kokkos::HostSpace;                                  ///< Space of the caller-owned host buffer.
        using DeviceSpace = Kokkos::DefaultExecutionSpace::memory_space;     ///< Space of the managed device mirror.

        /// Selects the Kokkos View type for element type T, rank R (1-3), memory Space, and Unmanaged ownership.
        template <typename T, int R, typename Space, bool Unmanaged> struct ViewType;

        /// Rank-1 LayoutLeft View; unmanaged variant wraps a raw pointer without owning it.
        template <typename T, typename Space, bool Unmanaged> struct ViewType<T, 1, Space, Unmanaged> {
            using type = typename std::conditional_t<
                Unmanaged, Kokkos::View<T*, Kokkos::LayoutLeft, Space, Kokkos::MemoryTraits<Kokkos::Unmanaged>>,
                Kokkos::View<T*, Kokkos::LayoutLeft, Space>>;
        };

        /// Rank-2 LayoutLeft View; unmanaged variant wraps a raw pointer without owning it.
        template <typename T, typename Space, bool Unmanaged> struct ViewType<T, 2, Space, Unmanaged> {
            using type = typename std::conditional_t<
                Unmanaged, Kokkos::View<T**, Kokkos::LayoutLeft, Space, Kokkos::MemoryTraits<Kokkos::Unmanaged>>,
                Kokkos::View<T**, Kokkos::LayoutLeft, Space>>;
        };

        /// Rank-3 LayoutLeft View; unmanaged variant wraps a raw pointer without owning it.
        template <typename T, typename Space, bool Unmanaged> struct ViewType<T, 3, Space, Unmanaged> {
            using type = std::conditional_t<
                Unmanaged, Kokkos::View<T***, Kokkos::LayoutLeft, Space, Kokkos::MemoryTraits<Kokkos::Unmanaged>>,
                Kokkos::View<T***, Kokkos::LayoutLeft, Space>>;
        };

        using HostViewType = typename ViewType<DataType, Rank, HostSpace, true>::type;   ///< Unmanaged LayoutLeft view over host memory.
        using DeviceViewType = typename ViewType<DataType, Rank, DeviceSpace, false>::type; ///< Kokkos-owned device mirror.

        HostViewType host_view;     ///< Contiguous host layout left view mapping raw pointer.
        DeviceViewType device_view; ///< Dedicated device view managed by Kokkos execution space.
        bool is_gpu_target;         ///< Flag indicating if device-host data sync is required.

        /**
         * @brief Constructs InteropField and maps raw host memory.
         * @param ptr Raw host pointer (must be non-null). The caller retains
         *            ownership and it must stay valid for the field's
         *            lifetime; the field only references, never copies it.
         * @param dims Vector indicating grid bounds.
         * @throws std::invalid_argument if ptr is null or dimensions are invalid.
         *
         * @note When HostSpace and DeviceSpace differ, a managed device
         * mirror of the same extents is allocated; in unified-memory builds
         * no device allocation happens and the syncs collapse.
         */
        InteropField(DataType* ptr, const std::vector<int>& dims) {
            if (ptr == nullptr) {
                throw std::invalid_argument("InteropField constructor failed: input pointer is null.");
            }
            validate_dimensions(dims);

            is_gpu_target = !std::is_same_v<HostSpace, DeviceSpace>;

            if constexpr (Rank == 1) {
                host_view = HostViewType(ptr, dims[0]);
                if (is_gpu_target)
                    device_view = DeviceViewType("dev_field_1d", dims[0]);
            } else if constexpr (Rank == 2) {
                host_view = HostViewType(ptr, dims[0], dims[1]);
                if (is_gpu_target)
                    device_view = DeviceViewType("dev_field_2d", dims[0], dims[1]);
            } else if constexpr (Rank == 3) {
                host_view = HostViewType(ptr, dims[0], dims[1], dims[2]);
                if (is_gpu_target)
                    device_view = DeviceViewType("dev_field_3d", dims[0], dims[1], dims[2]);
            }
        }

        /**
         * @brief Copy host data buffer to the selected Kokkos device view.
         *
         * @note No-op unless latest_writer is HostCurrent; on a real transfer
         * the host-to-device counter increments and the field becomes
         * Synchronized. Safe to call repeatedly.
         */
        void sync_to_device() {
            if (latest_writer != LatestWriter::HostCurrent)
                return;
            if constexpr (!std::is_same_v<HostSpace, DeviceSpace>) {
                Kokkos::deep_copy(device_view, host_view);
            }
            ++host_to_device_sync_count;
            latest_writer = LatestWriter::Synchronized;
        }

        /**
         * @brief Updates the host raw pointer and updates the unmanaged host view.
         * @param ptr New caller-owned host buffer with the same extents as
         *            before; must be non-null. The previous pointer is simply
         *            dropped (never freed here).
         *
         * @note Marks the host as the newest writer. The old memory must not
         * still be referenced by outstanding views.
         * @throws std::invalid_argument when ptr is null.
         */
        void update_host_pointer(DataType* ptr) {
            if (ptr == nullptr) {
                throw std::invalid_argument("InteropField::update_host_pointer failed: input pointer is null.");
            }
            mark_host_modified();
            if constexpr (Rank == 1) {
                host_view = HostViewType(ptr, host_view.extent(0));
            } else if constexpr (Rank == 2) {
                host_view = HostViewType(ptr, host_view.extent(0), host_view.extent(1));
            } else if constexpr (Rank == 3) {
                host_view = HostViewType(ptr, host_view.extent(0), host_view.extent(1), host_view.extent(2));
            }
        }

        /**
         * @brief Sync calculated outputs from Kokkos device back to host buffer.
         *
         * @note No-op unless latest_writer is DeviceCurrent; on a real
         * transfer the device-to-host counter increments and the field
         * becomes Synchronized.
         */
        void sync_to_host() {
            if (latest_writer != LatestWriter::DeviceCurrent)
                return;
            if constexpr (!std::is_same_v<HostSpace, DeviceSpace>) {
                Kokkos::deep_copy(host_view, device_view);
            }
            ++device_to_host_sync_count;
            latest_writer = LatestWriter::Synchronized;
        }

        /**
         * @brief Retrieves the active Kokkos view mapped to current execution space.
         * @return The device mirror when host and device spaces differ, else
         *         the unmanaged host view. Shallow reference, no copy.
         *
         * @note Does not synchronise; callers must sync_to_device() first
         * when the host side holds the newest values.
         */
        auto view() const {
            if constexpr (!std::is_same_v<HostSpace, DeviceSpace>) {
                return device_view;
            } else {
                return host_view;
            }
        }

        /**
         * @brief Raw pointer to the host-side buffer.
         * @return Pointer into the caller-owned host memory (no sync, no
         *         ownership transfer).
         */
        DataType* host_data() const { return host_view.data(); }

        /**
         * @brief Constructs a modern, standard-conforming mdspan referencing active data memory.
         * @return LayoutLeft mdspan over the active (device or host) view's
         *         memory; references, does not copy.
         */
        MdspanType mdspan() const {
            auto v = view();
            if constexpr (Rank == 1) {
                return MdspanType(v.data(), v.extent(0));
            } else if constexpr (Rank == 2) {
                return MdspanType(v.data(), v.extent(0), v.extent(1));
            } else if constexpr (Rank == 3) {
                return MdspanType(v.data(), v.extent(0), v.extent(1), v.extent(2));
            }
        }
#else
        DataType* data_ptr;         ///< Raw host buffer (not owned).
        std::array<int, Rank> dims; ///< Grid bounds per rank.
        bool is_gpu_target = false; ///< Host-only builds never target a device.

        /**
         * @brief Wrap a caller-owned host buffer with fixed extents.
         * @param ptr Non-null host pointer; referenced, never copied or freed.
         * @param dim_vec Exactly Rank positive extents, in field order.
         * @throws std::invalid_argument when ptr is null or dims are invalid.
         */
        InteropField(DataType* ptr, const std::vector<int>& dim_vec) : data_ptr(ptr) {
            if (ptr == nullptr) {
                throw std::invalid_argument("InteropField constructor failed: input pointer is null.");
            }
            validate_dimensions(dim_vec);
            for (int r = 0; r < Rank; ++r)
                dims[r] = dim_vec[r];
        }

        /// Bookkeeping-only sync: flips HostCurrent to Synchronized and counts the (virtual) transfer; no data moves.
        void sync_to_device() {
            if (latest_writer == LatestWriter::HostCurrent) {
                ++host_to_device_sync_count;
                latest_writer = LatestWriter::Synchronized;
            }
        }
        /// Bookkeeping-only sync: flips DeviceCurrent to Synchronized and counts the (virtual) transfer; no data moves.
        void sync_to_host() {
            if (latest_writer == LatestWriter::DeviceCurrent) {
                ++device_to_host_sync_count;
                latest_writer = LatestWriter::Synchronized;
            }
        }

        /**
         * @brief Updates the host raw pointer.
         * @param ptr New caller-owned host buffer; must be non-null. The
         *            previous pointer is dropped, never freed here.
         * @throws std::invalid_argument when ptr is null.
         */
        void update_host_pointer(DataType* ptr) {
            if (ptr == nullptr) {
                throw std::invalid_argument("InteropField::update_host_pointer failed: input pointer is null.");
            }
            data_ptr = ptr;
            mark_host_modified();
        }

        /**
         * @brief Raw pointer to the host-side buffer.
         * @return The caller-owned host pointer (no ownership transfer).
         */
        DataType* host_data() const { return data_ptr; }

        /**
         * @brief Host mdspan over the buffer (the only "view" in host-only builds).
         * @return LayoutLeft mdspan referencing data_ptr; no copy.
         */
        MdspanType mdspan() const {
            if constexpr (Rank == 1) {
                return MdspanType(data_ptr, dims[0]);
            } else if constexpr (Rank == 2) {
                return MdspanType(data_ptr, dims[0], dims[1]);
            } else if constexpr (Rank == 3) {
                return MdspanType(data_ptr, dims[0], dims[1], dims[2]);
            }
        }

        /**
         * @brief Same as mdspan(); kernels index views and mdspans identically via operator().
         * @return LayoutLeft mdspan over the host buffer.
         */
        MdspanType view() const { return mdspan(); }
#endif
    };

} // namespace catchem

#pragma once
#include <Kokkos_Core.hpp>
#include <vector>
#include <memory>
#include <type_traits>

namespace catchem {

template <typename DataType, int Rank>
class InteropField {
public:
    using HostSpace = Kokkos::HostSpace;
    using DeviceSpace = Kokkos::DefaultExecutionSpace::memory_space;

    template <typename T, int R, typename Space, bool Unmanaged>
    struct ViewType;

    template <typename T, typename Space, bool Unmanaged>
    struct ViewType<T, 1, Space, Unmanaged> {
        using type = typename std::conditional_t<Unmanaged,
            Kokkos::View<T*, Kokkos::LayoutLeft, Space, Kokkos::MemoryTraits<Kokkos::Unmanaged>>,
            Kokkos::View<T*, Kokkos::LayoutLeft, Space>>;
    };

    template <typename T, typename Space, bool Unmanaged>
    struct ViewType<T, 2, Space, Unmanaged> {
        using type = typename std::conditional_t<Unmanaged,
            Kokkos::View<T**, Kokkos::LayoutLeft, Space, Kokkos::MemoryTraits<Kokkos::Unmanaged>>,
            Kokkos::View<T**, Kokkos::LayoutLeft, Space>>;
    };

    template <typename T, typename Space, bool Unmanaged>
    struct ViewType<T, 3, Space, Unmanaged> {
        using type = std::conditional_t<Unmanaged,
            Kokkos::View<T***, Kokkos::LayoutLeft, Space, Kokkos::MemoryTraits<Kokkos::Unmanaged>>,
            Kokkos::View<T***, Kokkos::LayoutLeft, Space>>;
    };

    using HostViewType = typename ViewType<DataType, Rank, HostSpace, true>::type;
    using DeviceViewType = typename ViewType<DataType, Rank, DeviceSpace, false>::type;

    HostViewType host_view;
    DeviceViewType device_view;
    bool is_gpu_target;

    InteropField(DataType* ptr, const std::vector<int>& dims) {
        is_gpu_target = !std::is_same_v<HostSpace, DeviceSpace>;
        
        if constexpr (Rank == 1) {
            host_view = HostViewType(ptr, dims[0]);
            if (is_gpu_target) device_view = DeviceViewType("dev_field_1d", dims[0]);
        } else if constexpr (Rank == 2) {
            host_view = HostViewType(ptr, dims[0], dims[1]);
            if (is_gpu_target) device_view = DeviceViewType("dev_field_2d", dims[0], dims[1]);
        } else if constexpr (Rank == 3) {
            host_view = HostViewType(ptr, dims[0], dims[1], dims[2]);
            if (is_gpu_target) device_view = DeviceViewType("dev_field_3d", dims[0], dims[1], dims[2]);
        }
    }

    void sync_to_device() {
        if constexpr (!std::is_same_v<HostSpace, DeviceSpace>) {
            Kokkos::deep_copy(device_view, host_view);
        }
    }

    void sync_to_host() {
        if constexpr (!std::is_same_v<HostSpace, DeviceSpace>) {
            Kokkos::deep_copy(host_view, device_view);
        }
    }

    auto view() const {
        if constexpr (!std::is_same_v<HostSpace, DeviceSpace>) {
            return device_view;
        } else {
            return host_view;
        }
    }
};

} // namespace catchem

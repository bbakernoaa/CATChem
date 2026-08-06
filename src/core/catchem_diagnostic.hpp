// src/core/catchem_diagnostic.hpp
#pragma once
#include "catchem_interop_field.hpp"
#include "catchem_kokkos_compat.hpp"
#include <string>
#include <vector>

namespace catchem {

    enum class DiagType { SCALAR, FIELD_1D, FIELD_2D, FIELD_3D };

    class DiagnosticField {
    public:
        using Mdspan2D = typename MdspanTypeHelper<double, 2>::type;
        using Mdspan3D = typename MdspanTypeHelper<double, 3>::type;

        std::string name;
        std::string description;
        std::string units;
        DiagType type;
        std::vector<int> dimensions;

#ifdef CATCHEM_ENABLE_KOKKOS
        using HostSpace = Kokkos::HostSpace;
        using DeviceSpace = Kokkos::DefaultExecutionSpace::memory_space;

        using View2D = Kokkos::View<double**, Kokkos::LayoutLeft, DeviceSpace>;
        using HostView2D = Kokkos::View<double**, Kokkos::LayoutLeft, HostSpace>;

        using View3D = Kokkos::View<double***, Kokkos::LayoutLeft, DeviceSpace>;
        using HostView3D = Kokkos::View<double***, Kokkos::LayoutLeft, HostSpace>;

        View2D device_view_2d;
        HostView2D host_view_2d;

        View3D device_view_3d;
        HostView3D host_view_3d;
#else
        // Host-only builds own their storage directly; mdspans over `storage`
        // serve as both the "host" and "device" views.
        std::vector<double> storage;
#endif

        bool is_gpu_target;

        DiagnosticField(const std::string& name_val, const std::string& desc_val, const std::string& units_val,
                        DiagType type_val, const std::vector<int>& dims);

        void sync_to_host();
        void sync_to_device();
        void reset();
    };

} // namespace catchem

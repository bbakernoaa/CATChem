/**
 * @file kokkos_common.hpp
 * @brief Common Kokkos types and utilities for CATChem interop layer.
 *
 * Defines type aliases, view types, and utility macros used by all
 * Kokkos dispatch functions. Uses LayoutLeft (column-major) to match
 * Fortran memory ordering.
 */

#ifndef CATCHEM_KOKKOS_COMMON_HPP
#define CATCHEM_KOKKOS_COMMON_HPP

#include <Kokkos_Core.hpp>

namespace catchem {

// ---------------------------------------------------------------------------
// Execution and memory spaces (default from Kokkos configuration)
// ---------------------------------------------------------------------------
using ExecSpace  = Kokkos::DefaultExecutionSpace;
using MemSpace   = ExecSpace::memory_space;
using DeviceType = Kokkos::Device<ExecSpace, MemSpace>;

using HostExecSpace = Kokkos::DefaultHostExecutionSpace;
using HostMemSpace  = HostExecSpace::memory_space;

// ---------------------------------------------------------------------------
// Layout — column-major to match Fortran array ordering
// ---------------------------------------------------------------------------
using Layout = Kokkos::LayoutLeft;

// ---------------------------------------------------------------------------
// View type aliases for batch arrays
// ---------------------------------------------------------------------------

/// 1-D view (n_cols) or (n_levels) — scalars per column or per level
using View1D = Kokkos::View<double*, Layout, MemSpace>;

/// 2-D view (n_cols, n_levels) — one field across a batch of columns
using View2D = Kokkos::View<double**, Layout, MemSpace>;

/// 3-D view (n_cols, n_levels, n_species) — species data across batch
using View3D = Kokkos::View<double***, Layout, MemSpace>;

/// Unmanaged views wrapping Fortran pointers (no ownership, no copy)
using UnmanagedView1D = Kokkos::View<double*, Layout, MemSpace,
                                      Kokkos::MemoryTraits<Kokkos::Unmanaged>>;
using UnmanagedView2D = Kokkos::View<double**, Layout, MemSpace,
                                      Kokkos::MemoryTraits<Kokkos::Unmanaged>>;
using UnmanagedView3D = Kokkos::View<double***, Layout, MemSpace,
                                      Kokkos::MemoryTraits<Kokkos::Unmanaged>>;

/// Const unmanaged views for read-only input data
using ConstUnmanagedView1D = Kokkos::View<const double*, Layout, MemSpace,
                                           Kokkos::MemoryTraits<Kokkos::Unmanaged>>;
using ConstUnmanagedView2D = Kokkos::View<const double**, Layout, MemSpace,
                                           Kokkos::MemoryTraits<Kokkos::Unmanaged>>;

// ---------------------------------------------------------------------------
// Physical constants (matching Fortran SettlingPhysics_Mod)
// ---------------------------------------------------------------------------
namespace constants {
   constexpr double RHOW            = 1000.0;       // Density of water [kg/m3]
   constexpr double V_UPWARD_MARING = 0.33e-2;      // Maring correction [m/s]
   constexpr double KB              = 1.3807e-23;    // Boltzmann constant
   constexpr double M_AIR           = 4.8096e-26;    // Mass of avg air molecule [kg]
   constexpr double PI              = 3.141529265;
   constexpr double F_VT            = 8.0 * KB / PI / M_AIR;
   constexpr double TWO_OVER_NINE   = 2.0 / 9.0;

   // Drag correction coefficients (Pruppacher and Klett)
   constexpr double A0 = -3.18657;
   constexpr double A1 =  0.992696;
   constexpr double A2 = -1.53193e-3;
   constexpr double A3 = -9.870593e-4;
   constexpr double A4 = -5.78878e-4;
   constexpr double A5 =  8.55176e-5;
   constexpr double A6 = -3.27815e-6;
} // namespace constants

// ---------------------------------------------------------------------------
// Utility: wrap a raw Fortran pointer as an unmanaged Kokkos View
// ---------------------------------------------------------------------------

/// Wrap a 1-D Fortran pointer as an unmanaged View
inline UnmanagedView1D wrap_1d(double* ptr, int n) {
   return UnmanagedView1D(ptr, n);
}

/// Wrap a 2-D Fortran pointer as an unmanaged View (column-major)
inline UnmanagedView2D wrap_2d(double* ptr, int n1, int n2) {
   return UnmanagedView2D(ptr, n1, n2);
}

/// Wrap a 3-D Fortran pointer as an unmanaged View (column-major)
inline UnmanagedView3D wrap_3d(double* ptr, int n1, int n2, int n3) {
   return UnmanagedView3D(ptr, n1, n2, n3);
}

/// Wrap a const 1-D Fortran pointer
inline ConstUnmanagedView1D wrap_const_1d(const double* ptr, int n) {
   return ConstUnmanagedView1D(ptr, n);
}

/// Wrap a const 2-D Fortran pointer
inline ConstUnmanagedView2D wrap_const_2d(const double* ptr, int n1, int n2) {
   return ConstUnmanagedView2D(ptr, n1, n2);
}

} // namespace catchem

#endif // CATCHEM_KOKKOS_COMMON_HPP

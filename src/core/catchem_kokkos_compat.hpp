/**
 * @file catchem_kokkos_compat.hpp
 * @brief Single include point for the Kokkos / host-only build split.
 *
 * With CATCHEM_ENABLE_KOKKOS defined this pulls in Kokkos proper. Without
 * it, the standalone kokkos/mdspan library provides the Kokkos::mdspan data
 * types (same header, same namespace), and the Kokkos function-annotation
 * macros are masked with plain `inline` so shared kernel code compiles
 * host-only unchanged.
 */
#pragma once

/// @brief Enables the reference mdspan implementation's operator() so kernels
/// can index views and mdspans identically (Kokkos::View uses operator()
/// natively). Must be set before ANY <mdspan/mdspan.hpp> include, in both the
/// Kokkos and host-only flavors: under -std=gnu++23 the standalone header sees
/// std::mdspan and drops its operator(), breaking `view(i, j)` indexing across
/// the core.
#ifndef MDSPAN_USE_PAREN_OPERATOR
#define MDSPAN_USE_PAREN_OPERATOR 1
#endif

#ifdef CATCHEM_ENABLE_KOKKOS

#include <Kokkos_Core.hpp>

#else

/// @brief Pins the vendored single-header mdspan (src/external/mdspan) to the
/// Kokkos namespace: it defaults to std, and both build flavors must agree on
/// the same Kokkos::mdspan spellings.
#ifndef MDSPAN_IMPL_STANDARD_NAMESPACE
#define MDSPAN_IMPL_STANDARD_NAMESPACE Kokkos
#endif
#include <mdspan/mdspan.hpp>

/// @brief Host-only stand-in for KOKKOS_INLINE_FUNCTION: plain inline, so
/// shared kernel sources compile without the Kokkos backend.
#ifndef KOKKOS_INLINE_FUNCTION
#define KOKKOS_INLINE_FUNCTION inline
#endif
/// @brief Host-only stand-in for KOKKOS_FUNCTION (device-annotated free
/// functions); plain inline.
#ifndef KOKKOS_FUNCTION
#define KOKKOS_FUNCTION inline
#endif
/// @brief Host-only stand-in for KOKKOS_LAMBDA: a by-value capture, matching
/// the device launch semantics closely enough for serial fallbacks.
#ifndef KOKKOS_LAMBDA
#define KOKKOS_LAMBDA [=]
#endif

/// @brief Lifecycle no-ops so hosts and tests can call the Kokkos runtime
/// entry points unconditionally in a host-only build (nothing to initialise
/// or tear down when the backend is absent).
namespace Kokkos {
    inline void initialize(int, char*[]) {}
    inline void initialize() {}
    inline void finalize() {}
} // namespace Kokkos

#endif

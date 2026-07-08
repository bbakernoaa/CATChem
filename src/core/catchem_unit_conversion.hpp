#pragma once

#ifdef ENABLE_KOKKOS
#include <Kokkos_Core.hpp>
#else
#define KOKKOS_INLINE_FUNCTION inline
#endif

#include "catchem_precision.hpp"
#include "catchem_constants.hpp"

namespace catchem {
namespace unit_conversion {

KOKKOS_INLINE_FUNCTION
inline fp ppbv_to_ugm3(fp ppbv, fp mw, fp temp, fp press) {
    return ppbv * mw * press / (constants::RSTARG * temp) * 1.0e-3;
}

KOKKOS_INLINE_FUNCTION
inline fp ugm3_to_ppbv(fp ugm3, fp mw, fp temp, fp press) {
    return ugm3 * constants::RSTARG * temp / (mw * press) * 1.0e3;
}

} // namespace unit_conversion
} // namespace catchem

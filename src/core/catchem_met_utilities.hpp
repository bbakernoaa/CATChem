#pragma once

#ifdef ENABLE_KOKKOS
#include <Kokkos_Core.hpp>
#else
#define KOKKOS_INLINE_FUNCTION inline
#endif

#include <cmath>
#include "catchem_precision.hpp"
#include "catchem_constants.hpp"

namespace catchem {
namespace met_utilities {

KOKKOS_INLINE_FUNCTION
inline fp potential_temperature(fp temp, fp press, fp sfc_press) {
    return temp * std::pow(sfc_press / press, constants::RD / constants::CP);
}

KOKKOS_INLINE_FUNCTION
inline fp virtual_temperature(fp temp, fp qv) {
    return temp * (1.0 + 0.61 * qv);
}

KOKKOS_INLINE_FUNCTION
inline fp cunningham_correction_factor(fp dp, fp lambda) {
    if (dp > 0.0 && lambda > 0.0) {
        return 1.0 + 2.0 * lambda / dp * (1.257 + 0.4 * std::exp(-1.1 * dp / lambda));
    }
    return 1.0;
}

} // namespace met_utilities
} // namespace catchem

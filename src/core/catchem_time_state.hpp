#pragma once
#include <algorithm>
#include <cmath>

#ifdef ENABLE_KOKKOS
#include <Kokkos_Core.hpp>
#else
#ifndef KOKKOS_INLINE_FUNCTION
#define KOKKOS_INLINE_FUNCTION inline
#endif
#ifndef KOKKOS_FUNCTION
#define KOKKOS_FUNCTION inline
#endif
#endif

#ifndef KOKKOS_FUNCTION
#define KOKKOS_FUNCTION KOKKOS_INLINE_FUNCTION
#endif

#include "catchem_constants.hpp"
#include "catchem_precision.hpp"

namespace catchem {

    struct TimeState {
        int year = 2000;
        int month = 1;
        int day = 1;
        int hour = 0;
        int minute = 0;
        int second = 0;
        double timestep = 3600.0;
        double julian_date = 0.0;
        int doy = 1;

        KOKKOS_FUNCTION
        double get_cos_sza(double lat_deg, double lon_deg, bool mid_timestep = false) const {
            double lat_rad = lat_deg * constants::PI_180;

            double frac_hour = hour + minute / 60.0 + second / 3600.0;
            if (mid_timestep) {
                frac_hour += (timestep / 2.0) / 3600.0;
            }

            // Day angle [radians]
            double gamma = 2.0 * constants::PI * (doy - 1.0) / 365.0;

            // Solar declination (high-precision Fourier calculation matches GOCART2G)
            double dec = 0.006918 - 0.399912 * std::cos(gamma) + 0.070257 * std::sin(gamma) -
                         0.006758 * std::cos(2.0 * gamma) + 0.000907 * std::sin(2.0 * gamma) -
                         0.002697 * std::cos(3.0 * gamma) + 0.001480 * std::sin(3.0 * gamma);

            // Equation of time
            double eqtime = 229.18 * (0.000075 + 0.001868 * std::cos(gamma) - 0.032077 * std::sin(gamma) -
                                      0.014615 * std::cos(2.0 * gamma) - 0.040849 * std::sin(2.0 * gamma));

            double time_offset = eqtime + 4.0 * lon_deg;
            double true_solar_time = frac_hour * 60.0 + time_offset;

            double hour_angle = (true_solar_time / 4.0) - 180.0;
            double ha_rad = hour_angle * constants::PI_180;

            double cos_sza = std::sin(lat_rad) * std::sin(dec) + std::cos(lat_rad) * std::cos(dec) * std::cos(ha_rad);

            // Clamp output safely to [-1.0, 1.0]
            return std::max(-1.0, std::min(1.0, cos_sza));
        }
    };

} // namespace catchem

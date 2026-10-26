/**
 * @file catchem_time_state.hpp
 * @brief Calendar/clock state and solar-geometry helpers for a CATChem run.
 *
 * @details Holds the wall-clock date components, the timestep length, and the
 * derived Julian date and day-of-year. Every member is plain value data with
 * no dynamic allocation, so the struct is safe to capture by value inside
 * Kokkos lambdas; the static helpers are marked KOKKOS_FUNCTION to stay
 * callable from device code. Derived fields are only recomputed by
 * calculate_derived_fields() or advance() — assigning the date components
 * directly leaves julian_date and doy stale until one of those runs.
 */
#pragma once
#include "catchem_kokkos_compat.hpp"
#include <algorithm>
#include <cmath>

#include "catchem_constants.hpp"
#include "catchem_precision.hpp"

namespace catchem {

    /**
     * @brief Simulation clock with proleptic-Gregorian calendar arithmetic.
     *
     * @details Owns nothing; copyable and device-copyable. valid() enforces
     * the accepted ranges (see valid_date_time) plus 0 < timestep <= 86400 s.
     * advance() rolls the clock forward by whole seconds only — a fractional
     * dt is truncated by the static_cast<int>, so sub-second steps do not
     * accumulate.
     */
    struct TimeState {
        int year = 2000;          ///< Calendar year (CE), minimum 1.
        int month = 1;            ///< Month of year, 1-12.
        int day = 1;              ///< Day of month, 1 through get_days_in_month().
        int hour = 0;             ///< Hour of day, 0-23 UTC.
        int minute = 0;           ///< Minute of hour, 0-59.
        int second = 0;           ///< Whole second of minute, 0-59.
        double timestep = 3600.0; ///< Current step length [s]; must satisfy 0 < dt <= 86400.
        double julian_date = 0.0; ///< Julian day number at the current clock (derived, -0.5 at midnight).
        int doy = 1;              ///< Day of year, 1-366 (derived).

        /**
         * @brief Gregorian leap-year test.
         * @param y Year to test.
         * @return true when y is a leap year (divisible by 4 but not 100, or by 400).
         */
        KOKKOS_FUNCTION
        static bool is_leap_year(int y) { return (y % 4 == 0 && y % 100 != 0) || (y % 400 == 0); }

        /**
         * @brief Days in a month, honouring February in leap years.
         * @param m Month, 1-12.
         * @param y Year used for the leap-year test.
         * @return Days in the month, or 0 when m is outside 1-12.
         */
        KOKKOS_FUNCTION
        static int get_days_in_month(int m, int y) {
            const int days_per_month[] = {0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
            if (m < 1 || m > 12)
                return 0;
            if (m == 2 && is_leap_year(y))
                return 29;
            return days_per_month[m];
        }

        /**
         * @brief Validate calendar components and their day-of-year consistency.
         * @param y Year, must be >= 1.
         * @param m Month, must be 1-12.
         * @param d Day of month, must be 1 through get_days_in_month(m, y).
         * @param h Hour, must be 0-23.
         * @param minute_value Minute, must be 0-59.
         * @param second_value Whole second, must be 0-59.
         * @param day_of_year Day of year; must equal d plus the preceding months' lengths.
         * @return true when every component is in range and day_of_year agrees
         *         with the date.
         */
        KOKKOS_FUNCTION
        static bool valid_date_time(int y, int m, int d, int h, int minute_value, int second_value, int day_of_year) {
            if (y < 1 || m < 1 || m > 12 || d < 1 || d > get_days_in_month(m, y) || h < 0 || h > 23 ||
                minute_value < 0 || minute_value > 59 || second_value < 0 || second_value > 59)
                return false;
            int expected = d;
            for (int month_value = 1; month_value < m; ++month_value) {
                expected += get_days_in_month(month_value, y);
            }
            return day_of_year == expected;
        }

        /**
         * @brief Check this clock's components and timestep.
         * @return true when valid_date_time() accepts the date and timestep is
         *         finite with 0 < timestep <= 86400 s.
         */
        KOKKOS_FUNCTION
        bool valid() const {
            return valid_date_time(year, month, day, hour, minute, second, doy) && std::isfinite(timestep) &&
                   timestep > 0.0 && timestep <= 86400.0;
        }

        /**
         * @brief Recompute julian_date and doy from the date components.
         *
         * @details Julian day number uses the standard integer Gregorian
         * formula with the fractional day measured from noon (00:00 UTC gives
         * JD - 0.5). The day-of-year loop adds the leap day after February in
         * leap years.
         *
         * @note Call after assigning the components directly; advance() calls
         * this automatically.
         */
        KOKKOS_FUNCTION
        void calculate_derived_fields() {
            // Julian Day arithmetic
            int a = (14 - month) / 12;
            int y = year + 4800 - a;
            int m = month + 12 * a - 3;
            int jdn = day + (153 * m + 2) / 5 + 365 * y + y / 4 - y / 100 + y / 400 - 32045;

            julian_date = static_cast<double>(jdn) - 0.5 + (hour + minute / 60.0 + second / 3600.0) / 24.0;

            // Calculate Day of Year (DOY)
            const int days_per_month[] = {0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
            doy = day;
            for (int i = 1; i < month; ++i) {
                doy += days_per_month[i];
                if (i == 2 && is_leap_year(year)) {
                    doy += 1;
                }
            }
        }

        /**
         * @brief Advance the clock by one timestep and refresh derived fields.
         * @param dt Step length [s]; also stored as timestep. Only the integer
         *           part is added to the seconds — fractional dt is truncated.
         *
         * @note Carries seconds into minutes, minutes into hours, and hours
         * into days/months/years, including leap Februaries. A negative dt is
         * not handled: the modulo arithmetic can produce out-of-range
         * components, so callers must pass a positive step.
         */
        KOKKOS_FUNCTION
        void advance(double dt) {
            timestep = dt;
            int total_sec = second + static_cast<int>(dt);
            second = total_sec % 60;
            int total_min = minute + total_sec / 60;
            minute = total_min % 60;
            int total_hr = hour + total_min / 60;
            hour = total_hr % 24;
            int extra_days = total_hr / 24;

            while (extra_days > 0) {
                int dim = get_days_in_month(month, year);
                if (day + extra_days <= dim) {
                    day += extra_days;
                    extra_days = 0;
                } else {
                    extra_days -= (dim - day + 1);
                    day = 1;
                    if (month == 12) {
                        month = 1;
                        year += 1;
                    } else {
                        month += 1;
                    }
                }
            }
            calculate_derived_fields();
        }

        /**
         * @brief Cosine of the solar zenith angle for a column.
         *
         * @param lat_deg  Latitude of the column [deg, north positive].
         * @param lon_deg  Longitude of the column [deg, east positive].
         * @param mid_timestep When true, evaluate at the middle of the current
         *                     step (frac_hour + timestep/2) instead of the step
         *                     start.
         * @return Cosine of the solar zenith angle clamped to [-1, 1];
         *         values <= 0 mean the sun is at or below the horizon.
         *
         * @note Solar declination and the equation of time use the
         * high-precision Fourier series that matches GOCART2G, driven by the
         * day angle 2*pi*(doy-1)/365 and the stored clock components. The
         * result depends on doy/hour being current, so call after
         * calculate_derived_fields() when the date was set directly.
         */
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

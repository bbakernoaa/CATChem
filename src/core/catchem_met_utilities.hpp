/**
 * @file catchem_met_utilities.hpp
 * @brief Pointwise derived-meteorology kernels shared by StateManager and the C API.
 *
 * @details Header-only functions in catchem::met_utilities, each marked
 * KOKKOS_INLINE_FUNCTION so the same source compiles into Kokkos device
 * kernels and host fallback loops. All parameters and return values use
 * catchem::fp (double under USE_REAL8, float otherwise); units are identical
 * in both precisions. The catchem::math alias namespace selects the Kokkos or
 * standard math overloads depending on CATCHEM_ENABLE_KOKKOS.
 */
#pragma once
#include "catchem_kokkos_compat.hpp"

// catchem::math aliases device-safe Kokkos math functions when the Kokkos
// backend is enabled, and the standard library functions otherwise, so the
// kernels below compile unchanged for either build.
#ifdef CATCHEM_ENABLE_KOKKOS
namespace catchem {
    namespace math {
        using Kokkos::acos;
        using Kokkos::cos;
        using Kokkos::exp;
        using Kokkos::log;
        using Kokkos::max;
        using Kokkos::min;
        using Kokkos::pow;
        using Kokkos::sin;
        using Kokkos::sqrt;
    } // namespace math
} // namespace catchem
#else
#include <algorithm>
#include <cmath>
namespace catchem {
    namespace math {
        using std::acos;
        using std::cos;
        using std::exp;
        using std::log;
        using std::max;
        using std::min;
        using std::pow;
        using std::sin;
        using std::sqrt;
    } // namespace math
} // namespace catchem
#endif

#include "catchem_constants.hpp"
#include "catchem_precision.hpp"

namespace catchem {
    /**
     * @brief Derived-meteorology pointwise kernels.
     *
     * @details Every function is stateless and operates only on its arguments;
     * field lifetime, synchronisation, and validation are the caller's
     * (StateManager or the C API wrappers in catchem_api_utilities.cpp).
     */
    namespace met_utilities {

        /**
         * @brief Potential temperature of a parcel brought to the reference pressure.
         * @param temp Air temperature [K].
         * @param press Pressure at the parcel level [Pa].
         * @param sfc_press Surface (reference) pressure [Pa].
         * @return Potential temperature [K]: temp * (sfc_press / press)^(RD/CP).
         * @note Unguarded division; press <= 0 or NaN propagates through the power.
         */
        KOKKOS_INLINE_FUNCTION
        fp potential_temperature(fp temp, fp press, fp sfc_press) {
            return temp * math::pow(sfc_press / press, constants::RD / constants::CP);
        }

        /**
         * @brief Density-equivalent temperature of moist air.
         * @param temp Air temperature [K].
         * @param qv Water vapour mixing ratio [kg kg-1].
         * @return Virtual temperature [K]: temp * (1 + 0.61 * qv).
         */
        KOKKOS_INLINE_FUNCTION
        fp virtual_temperature(fp temp, fp qv) {
            return temp * (1.0 + 0.61 * qv);
        }

        /**
         * @brief Saturation vapour pressure over water (Magnus form).
         * @param temp Air temperature [K].
         * @return Saturation vapour pressure [Pa]: 611.2 * exp(17.67 (T-273.15)/(T-29.65)).
         * @note Singular at T = 29.65 K; no guard.
         */
        KOKKOS_INLINE_FUNCTION
        fp saturation_vapor_pressure(fp temp) {
            return 611.2 * math::exp(17.67 * (temp - 273.15) / (temp - 29.65));
        }

        /**
         * @brief Dew point from temperature and relative humidity (inverse Magnus).
         * @param temp Air temperature [K].
         * @param rh Relative humidity as a fraction (0-1), not percent.
         * @return Dew-point temperature [K].
         * @note rh <= 0 yields the Magnus-form limit near 29.65 K rather than -inf.
         */
        KOKKOS_INLINE_FUNCTION
        fp dew_point(fp temp, fp rh) {
            fp es = saturation_vapor_pressure(temp);
            fp ed = rh * es;
            return 243.5 / (17.67 / math::log(ed / 611.2) - 1.0) + 273.15;
        }

        /**
         * @brief Relative humidity from vapour mixing ratio, GOCART-aligned.
         * @param T Air temperature [K].
         * @param qv Water vapour mixing ratio [kg kg-1].
         * @param p Air pressure [Pa].
         * @return Relative humidity as a fraction, clamped to [0.005, 0.99].
         */
        // GOCART-aligned relative humidity in the Alduchov-Eskridge Magnus form:
        //   es [Pa] = 610.94 * exp(17.625 * tc / (tc + 243.04)),  tc = T - 273.15
        //   e  [Pa] = p * qv / (0.622 + qv)
        //   RH      = clip(e / es, 0.005, 0.99)
        // This replaces the former Bolton form and the [0, 1] clamp. The change
        // is O(1 %) in the mid-troposphere and is applied unconditionally (not
        // config-gated). Negative qv or non-positive p are validation errors at
        // import, not clamped here; the floor keeps qv = 0 finite at 0.005.
        KOKKOS_INLINE_FUNCTION
        fp relative_humidity(fp T, fp qv, fp p) {
            const fp tc = T - 273.15;
            const fp es = 610.94 * math::exp(17.625 * tc / (tc + 243.04));
            const fp e = qv * p / (0.622 + qv);
            const fp rh = e / es;
            return math::max(static_cast<fp>(0.005), math::min(static_cast<fp>(0.99), rh));
        }

        // Pointwise derived-meteorology kernels. StateManager owns field
        // lifetime and invokes these only after validating host inputs.
        /**
         * @brief Hydrostatic thickness of a layer between two pressure surfaces.
         * @param pressure_bottom Layer bottom pressure [Pa].
         * @param pressure_top Layer top pressure [Pa].
         * @param temperature Layer-mean temperature [K].
         * @param qv Water vapour mixing ratio [kg kg-1], clamped to [0, 0.9999].
         * @return Layer thickness [m]: (RD/G0) * Tv * ln(pb/pt); 0 unless
         *         pb > pt > 0 and T > 0.
         */
        KOKKOS_INLINE_FUNCTION
        fp hydrostatic_layer_thickness(fp pressure_bottom, fp pressure_top, fp temperature, fp qv) {
            if (!(pressure_bottom > pressure_top && pressure_top > 0.0 && temperature > 0.0))
                return 0.0;
            const fp bounded_qv = math::max(static_cast<fp>(0.0), math::min(static_cast<fp>(0.9999), qv));
            return (constants::RD / constants::G0) * virtual_temperature(temperature, bounded_qv) *
                   math::log(pressure_bottom / pressure_top);
        }

        /**
         * @brief Pressure depth of a layer.
         * @param pressure_bottom Layer bottom pressure [Pa].
         * @param pressure_top Layer top pressure [Pa].
         * @return pb - pt [Pa], or 0 when the ordering is not bottom > top.
         */
        KOKKOS_INLINE_FUNCTION
        fp pressure_thickness(fp pressure_bottom, fp pressure_top) {
            return pressure_bottom > pressure_top ? pressure_bottom - pressure_top : 0.0;
        }

        /**
         * @brief Density of the dry-air component of a moist parcel.
         * @param pressure Air pressure [Pa].
         * @param temperature Air temperature [K].
         * @param qv Water vapour mixing ratio [kg kg-1], clamped to [0, 0.9999].
         * @return Dry-air density [kg m-3]: p (1 - xw) / (RD T), where xw is the
         *         water mole fraction derived from qv; 0 unless p > 0 and T > 0.
         */
        KOKKOS_INLINE_FUNCTION
        fp dry_air_density(fp pressure, fp temperature, fp qv) {
            if (!(pressure > 0.0 && temperature > 0.0))
                return 0.0;
            const fp bounded_qv = math::max(static_cast<fp>(0.0), math::min(static_cast<fp>(0.9999), qv));
            const fp mixing =
                (constants::AIR_MW / constants::H2O_MW) * bounded_qv / (static_cast<fp>(1.0) - bounded_qv);
            const fp water_mole_fraction = mixing / (static_cast<fp>(1.0) + mixing);
            return pressure * (static_cast<fp>(1.0) - water_mole_fraction) / (constants::RD * temperature);
        }

        // Legacy metstate_mod large-scale/anvil precipitation re-evaporation
        // tendency [kg kg-1 s-1]. This is a diagnostic, never a fallback.
        /**
         * @brief Re-evaporation tendency for large-scale precipitation.
         * @param temperature Layer temperature [K].
         * @param qv Water vapour mixing ratio [kg kg-1].
         * @param pressure_mid Layer mid-level pressure [Pa].
         * @param pressure_bottom Layer bottom pressure [Pa].
         * @param pressure_top Layer top pressure [Pa].
         * @param ice_flux Grid-box ice precipitation flux [kg m-2 s-1], negative parts ignored.
         * @param liquid_flux Grid-box liquid precipitation flux [kg m-2 s-1], negative parts ignored.
         * @return Re-evaporation mixing-ratio tendency [kg kg-1 s-1], clamped to
         *         [0, (ice+liquid)/air_mass]; 0 unless the layer has positive air
         *         mass, RH below 0.9, and a positive precipitation flux.
         * @note The liquid/ice coefficient interpolates linearly between 258.15 K
         *       (ice only, 0.5e-5) and 273.15 K (liquid only, 2.0e-5).
         */
        KOKKOS_INLINE_FUNCTION
        fp large_scale_reevaporation(fp temperature, fp qv, fp pressure_mid, fp pressure_bottom, fp pressure_top,
                                     fp ice_flux, fp liquid_flux) {
            constexpr fp rh_threshold = 0.9;
            constexpr fp liquid_coefficient = 2.0e-5;
            constexpr fp ice_coefficient = 0.5e-5;
            constexpr fp liquid_temperature = 273.15;
            constexpr fp ice_temperature = 258.15;
            const fp liquid = math::max(static_cast<fp>(0.0), liquid_flux);
            const fp ice = math::max(static_cast<fp>(0.0), ice_flux);
            const fp air_mass = (pressure_bottom - pressure_top) / constants::G0;
            const fp rh = relative_humidity(temperature, qv, pressure_mid);
            if (!(air_mass > 0.0) || !(rh < rh_threshold) || !(liquid + ice > 0.0))
                return 0.0;
            const fp coefficient =
                temperature > liquid_temperature ? liquid_coefficient
                : temperature > ice_temperature
                    ? ((temperature - ice_temperature) / (liquid_temperature - ice_temperature)) * liquid_coefficient +
                          ((liquid_temperature - temperature) / (liquid_temperature - ice_temperature)) *
                              ice_coefficient
                    : ice_coefficient;
            const fp rh_term = math::max(static_cast<fp>(0.0), static_cast<fp>(1.0) - rh / rh_threshold);
            const fp liquid_loss =
                liquid > 0.0 ? math::min(coefficient * rh_term * math::sqrt(liquid), liquid / air_mass) : 0.0;
            const fp ice_loss = temperature > ice_temperature
                                    ? math::min(coefficient * rh_term * math::sqrt(ice), ice / air_mass)
                                    : 0.0;
            return math::max(static_cast<fp>(0.0), math::min(liquid_loss + ice_loss, (liquid + ice) / air_mass));
        }

        /**
         * @brief Convert specific humidity to a mixing ratio.
         * @param q Specific humidity [kg kg-1].
         * @return Mixing ratio [kg kg-1]: q / (1 - q). Singular at q = 1; no guard.
         */
        KOKKOS_INLINE_FUNCTION
        fp mixing_ratio(fp q) {
            return q / (1.0 - q);
        }

        /**
         * @brief Convert a mixing ratio to specific humidity.
         * @param r Mixing ratio [kg kg-1].
         * @return Specific humidity [kg kg-1]: r / (1 + r).
         */
        KOKKOS_INLINE_FUNCTION
        fp specific_humidity(fp r) {
            return r / (1.0 + r);
        }

        /**
         * @brief Dry adiabatic lapse rate.
         * @return G0 / CP [K m-1] (~0.00976 K/m), the environmental lapse rate of
         *         a dry adiabat.
         */
        KOKKOS_INLINE_FUNCTION
        fp dry_adiabatic_lapse_rate() {
            return constants::G0 / constants::CP;
        }

        /**
         * @brief Bulk Richardson number between the surface and height z.
         * @param T0 Surface temperature [K].
         * @param Tz Temperature at height z [K].
         * @param u Wind speed at height z [m s-1].
         * @param z Height above surface [m].
         * @return Dimensionless bulk Richardson number (G0/T0)(Tz-T0)z/u^2,
         *         positive for a stable (warmer-above) profile; 0 unless u > 0 and z > 0.
         */
        KOKKOS_INLINE_FUNCTION
        fp bulk_richardson_number(fp T0, fp Tz, fp u, fp z) {
            if (u > 0.0 && z > 0.0) {
                return (constants::G0 / T0) * (Tz - T0) * z / (u * u);
            }
            return 0.0;
        }

        /**
         * @brief Monin-Obukhov length from surface-layer fluxes.
         * @param ustar Friction velocity [m s-1].
         * @param T0 Surface temperature [K].
         * @param H Sensible heat flux [W m-2]; sign carries stability.
         * @param rho Air density [kg m-3].
         * @return Obukhov length [m]: -(u*^3 rho CP T0)/(0.41 G0 H). Returns a
         *         +1e5 m neutral/stable default when u* <= 0 or |H| <= 0.
         */
        KOKKOS_INLINE_FUNCTION
        fp monin_obukhov_length(fp ustar, fp T0, fp H, fp rho) {
            if (ustar > 0.0 && (H < 0.0 ? -H : H) > 0.0) {
                return -(ustar * ustar * ustar * rho * constants::CP * T0) / (0.41 * constants::G0 * H);
            }
            return 1.0e5; // Neutral/stable default
        }

        /**
         * @brief Friction velocity from surface shear stress.
         * @param tau Surface shear stress [Pa]; magnitude used.
         * @param rho Air density [kg m-3].
         * @return Friction velocity u* = sqrt(|tau|/rho) [m s-1]; 0 unless rho > 0.
         */
        KOKKOS_INLINE_FUNCTION
        fp friction_velocity(fp tau, fp rho) {
            if (rho > 0.0) {
                fp abs_tau = tau < 0.0 ? -tau : tau;
                return math::sqrt(abs_tau / rho);
            }
            return 0.0;
        }

        /**
         * @brief Classify stability from the Obukhov length.
         * @param L Monin-Obukhov length [m].
         * @return -1 unstable (L < -200 m), +1 stable (L > 200 m), 0 otherwise
         *         (near-neutral band and the 1e5 sentinel collapse into 0).
         */
        KOKKOS_INLINE_FUNCTION
        int stability_classification(fp L) {
            if (L < -200.0)
                return -1;
            if (L > 200.0)
                return 1;
            return 0;
        }

        /**
         * @brief Saturation mixing ratio over water.
         * @param p Air pressure [Pa].
         * @param T Air temperature [K].
         * @return Saturation mixing ratio [kg kg-1]: 0.622 es / (p - es). Singular
         *         when p <= es; no guard.
         */
        KOKKOS_INLINE_FUNCTION
        fp saturation_mixing_ratio(fp p, fp T) {
            fp es = saturation_vapor_pressure(T);
            return 0.622 * es / (p - es);
        }

        /**
         * @brief Latent heat of vaporization, linear in temperature.
         * @param T Air temperature [K].
         * @return Lv [J kg-1]: 2.501e6 - 2.361e3 (T - 273.15).
         */
        KOKKOS_INLINE_FUNCTION
        fp latent_heat_vaporization(fp T) {
            return 2.501e6 - 2.361e3 * (T - 273.15);
        }

        /**
         * @brief Psychrometric constant.
         * @param p Air pressure [Pa].
         * @param Lv Latent heat of vaporization [J kg-1] (see latent_heat_vaporization).
         * @return gamma = CP p / (0.622 Lv) [Pa K-1].
         */
        KOKKOS_INLINE_FUNCTION
        fp psychrometric_constant(fp p, fp Lv) {
            return constants::CP * p / (0.622 * Lv);
        }

        /**
         * @brief Neutral logarithmic wind profile.
         * @param ustar Friction velocity [m s-1].
         * @param z Measurement height [m].
         * @param z0 Surface roughness length [m].
         * @return Wind speed [m s-1]: (ustar divided by 0.41) ln(z/z0); 0 unless z > z0 > 0.
         */
        KOKKOS_INLINE_FUNCTION
        fp wind_profile_loglaw(fp ustar, fp z, fp z0) {
            if (z > z0 && z0 > 0.0) {
                return ustar / 0.41 * math::log(z / z0);
            }
            return 0.0;
        }

        /**
         * @brief Squared Brunt-Vaisala frequency for a dry stratified column.
         * @param T0 Reference temperature [K].
         * @param dTdz Environmental temperature lapse dT/dz [K m-1].
         * @return N^2 [s-2]: (G0/T0)(dTdz + G0/CP); positive means stable.
         */
        KOKKOS_INLINE_FUNCTION
        fp brunt_vaisala_frequency(fp T0, fp dTdz) {
            return (constants::G0 / T0) * (dTdz + constants::G0 / constants::CP);
        }

        /**
         * @brief Businger-Dyer profile function for momentum.
         * @param zeta Dimensionless stability z/L (negative unstable, positive stable).
         * @return psi_m: 2 ln((1+sqrt(1-16 zeta))/2) when zeta < 0, else -5 zeta.
         */
        KOKKOS_INLINE_FUNCTION
        fp psi_m_businger(fp zeta) {
            if (zeta < 0.0) {
                return 2.0 * math::log((1.0 + math::sqrt(1.0 - 16.0 * zeta)) / 2.0);
            } else {
                return -5.0 * zeta;
            }
        }

        /**
         * @brief Businger-Dyer profile function for heat (same form as psi_m here).
         * @param zeta Dimensionless stability z/L.
         * @return psi_h: 2 ln((1+sqrt(1-16 zeta))/2) when zeta < 0, else -5 zeta.
         */
        KOKKOS_INLINE_FUNCTION
        fp psi_h_businger(fp zeta) {
            if (zeta < 0.0) {
                return 2.0 * math::log((1.0 + math::sqrt(1.0 - 16.0 * zeta)) / 2.0);
            } else {
                return -5.0 * zeta;
            }
        }

        /**
         * @brief Arrhenius-type temperature dependence of a rate coefficient.
         * @param A Pre-exponential factor [same units as the returned rate].
         * @param Ea Activation energy [J mol-1].
         * @param T Temperature [K].
         * @return A * exp(-Ea / (RSTARG T)); T = 0 yields exp(-inf) = 0.
         */
        KOKKOS_INLINE_FUNCTION
        fp arrhenius_rate(fp A, fp Ea, fp T) {
            return A * math::exp(-Ea / (constants::RSTARG * T));
        }

        /**
         * @brief van' t Hoff temperature scaling of a Henry's law constant.
         * @param H0 Henry's law constant at the reference temperature [H0's units].
         * @param dH Enthalpy of dissolution [J mol-1].
         * @param T Temperature [K].
         * @param T0 Reference temperature [K].
         * @return H0 * exp(-dH/RSTARG (1/T - 1/T0)); singular at T = 0.
         */
        KOKKOS_INLINE_FUNCTION
        fp henrys_law_constant(fp H0, fp dH, fp T, fp T0) {
            return H0 * math::exp(-dH / constants::RSTARG * (1.0 / T - 1.0 / T0));
        }

        /**
         * @brief Scale a photolysis frequency by illumination geometry.
         * @param J0 Clear-sky photolysis frequency at overhead sun [s-1].
         * @param sza Solar zenith angle [degrees].
         * @return J0 * max(0, cos(sza)) [s-1]; zero on the night side.
         */
        KOKKOS_INLINE_FUNCTION
        fp photolysis_rate_scaling(fp J0, fp sza) {
            return J0 * math::max(static_cast<fp>(0.0), math::cos(sza * constants::PI_180));
        }

        /**
         * @brief Convert a volume mixing ratio to mass concentration (ideal gas).
         * @param ppm Volume mixing ratio [parts per million by volume].
         * @param M Species molecular weight [g mol-1].
         * @param T Air temperature [K].
         * @param p Air pressure [Pa].
         * @return Mass concentration [ug m-3]: ppm 1e-6 p M / (RSTARG T) 1e3.
         */
        KOKKOS_INLINE_FUNCTION
        fp ppm_to_ugm3(fp ppm, fp M, fp T, fp p) {
            return ppm * 1.0e-6 * p * M / (constants::RSTARG * T) * 1.0e3;
        }

        /**
         * @brief Inverse of ppm_to_ugm3.
         * @param ugm3 Mass concentration [ug m-3].
         * @param M Species molecular weight [g mol-1]; must be non-zero.
         * @param T Air temperature [K].
         * @param p Air pressure [Pa]; must be non-zero.
         * @return Volume mixing ratio [ppm].
         */
        KOKKOS_INLINE_FUNCTION
        fp ugm3_to_ppm(fp ugm3, fp M, fp T, fp p) {
            return ugm3 * (constants::RSTARG * T) / (p * M * 1.0e3) * 1.0e6;
        }

        /**
         * @brief Stokes settling velocity of a small sphere.
         * @param dp Particle diameter [m].
         * @param rho_p Particle density [kg m-3].
         * @param rho_a Air density [kg m-3]; negative buoyancy falls slowly upward.
         * @param mu Dynamic (absolute) viscosity of air [kg m-1 s-1].
         * @param Cc Cunningham slip correction factor [dimensionless].
         * @return Settling velocity [m s-1]: dp^2 (rho_p-rho_a) G0 Cc / (18 mu).
         */
        KOKKOS_INLINE_FUNCTION
        fp stokes_settling_velocity(fp dp, fp rho_p, fp rho_a, fp mu, fp Cc) {
            return (dp * dp) * (rho_p - rho_a) * constants::G0 * Cc / (18.0 * mu);
        }

        /**
         * @brief Cunningham slip correction for submicron particles.
         * @param dp Particle diameter [m].
         * @param lambda Gas mean free path [m] (see mean_free_path_air).
         * @return 1 + 2 (lambda/dp) (1.257 + 0.4 exp(-1.1 dp/lambda)); exactly 1.0
         *         when either argument is non-positive.
         */
        KOKKOS_INLINE_FUNCTION
        fp cunningham_correction_factor(fp dp, fp lambda) {
            if (dp > 0.0 && lambda > 0.0) {
                return 1.0 + 2.0 * lambda / dp * (1.257 + 0.4 * math::exp(-1.1 * dp / lambda));
            }
            return 1.0;
        }

        /**
         * @brief Stokes number of a particle in a flow over an obstacle scale L.
         * @param rho_p Particle density [kg m-3].
         * @param d_p Particle diameter [m].
         * @param U Flow speed [m s-1].
         * @param mu Dynamic viscosity of air [kg m-1 s-1].
         * @param L Characteristic flow length scale [m].
         * @return Dimensionless St = rho_p d_p^2 U / (18 mu L); 0 unless mu > 0 and L > 0.
         */
        KOKKOS_INLINE_FUNCTION
        fp stokes_number(fp rho_p, fp d_p, fp U, fp mu, fp L) {
            if (mu > 0.0 && L > 0.0) {
                return (rho_p * d_p * d_p * U) / (18.0 * mu * L);
            }
            return 0.0;
        }

        /**
         * @brief Kinetic-theory mean free path of air molecules.
         * @param T Air temperature [K].
         * @param p Air pressure [Pa].
         * @return Mean free path [m]: BOLTZ T / (sqrt(2) pi d_air^2 p) with a
         *         3.7e-10 m effective molecular diameter; singular at p = 0.
         */
        KOKKOS_INLINE_FUNCTION
        fp mean_free_path_air(fp T, fp p) {
            constexpr fp d_air = 3.7e-10; // Effective air molecule diameter [m]
            return constants::BOLTZ * T / (1.414213562373095 * constants::PI * d_air * d_air * p);
        }

        /**
         * @brief Radioactive decay to a remaining quantity.
         * @param N0 Initial quantity [any count].
         * @param lambda Decay constant [s-1] (consistent with t's unit).
         * @param t Elapsed time [same time unit as 1/lambda].
         * @return N0 * exp(-lambda t) [same unit as N0].
         */
        KOKKOS_INLINE_FUNCTION
        fp nuclear_decay(fp N0, fp lambda, fp t) {
            return N0 * math::exp(-lambda * t);
        }

        /**
         * @brief Solar zenith angle from day-of-year, hour, and site geometry.
         * @param jday Day of year (1-365/366).
         * @param xhour Universal time hour (0-24); local time is derived from lon_rad.
         * @param lat_rad Site latitude [radians].
         * @param lon_rad Site longitude [radians, east positive].
         * @param sza_deg Output: solar zenith angle [degrees] (always 0-180).
         * @param cossza Output: cosine of the zenith angle, clamped to [-1, 1] and
         *        then set to 0 on the night side (cossza < 0 before the reset).
         * @note Declination uses the Fourier coefficient series; local solar time
         *       wraps into [0, 24] hours. Both outputs are written on every call.
         */
        KOKKOS_INLINE_FUNCTION
        void solar_zenith_angle(int jday, fp xhour, fp lat_rad, fp lon_rad, fp& sza_deg, fp& cossza) {
            // Solar declination Fourier coefficients
            constexpr fp a0 = 0.006918;
            constexpr fp a1 = 0.399912;
            constexpr fp a2 = 0.006758;
            constexpr fp a3 = 0.002697;
            constexpr fp b1 = 0.070257;
            constexpr fp b2 = 0.000907;
            constexpr fp b3 = 0.000148;

            fp rad2deg = 180.0 / constants::PI;
            fp r = 2.0 * constants::PI * static_cast<fp>(jday - 1) / 365.0;

            fp dec = a0 - a1 * math::cos(r) + b1 * math::sin(r) - a2 * math::cos(2.0 * r) + b2 * math::sin(2.0 * r) -
                     a3 * math::cos(3.0 * r) + b3 * math::sin(3.0 * r);

            fp xlon = lon_rad * rad2deg;
            fp timloc = xhour + xlon / 15.0;
            while (timloc < 0.0)
                timloc += 24.0;
            while (timloc > 24.0)
                timloc -= 24.0;

            fp ahr = (timloc - 12.0) * 15.0;
            if (ahr < 0.0)
                ahr = -ahr;
            fp ahr_rad = ahr * constants::PI / 180.0;

            cossza = math::sin(lat_rad) * math::sin(dec) + math::cos(lat_rad) * math::cos(dec) * math::cos(ahr_rad);
            cossza = math::max(static_cast<fp>(-1.0), math::min(static_cast<fp>(1.0), cossza));

            sza_deg = math::acos(cossza) * rad2deg;

            if (cossza < 0.0)
                cossza = 0.0;
        }

    } // namespace met_utilities
} // namespace catchem

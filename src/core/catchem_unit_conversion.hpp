/**
 * @file catchem_unit_conversion.hpp
 * @brief Chemistry unit conversion helpers for concentrations, fluxes, and rates.
 *
 * @details Two families of helpers live here. The KOKKOS_INLINE_FUNCTION
 * converters (air density, ppbv/ugm3, molcm3/ppbv, ppmv/mgm3, molecule
 * fluxes) are device-callable and use double literals matching the legacy
 * Fortran constants.F90 so GPU and CPU paths agree. The inline host-only
 * convert_* family parses unit strings case-insensitively, routes through a
 * base unit (Pa, K, kg m-2 s-1, m3 molecule-1 s-1, kg, m), and reports
 * failure through an int& rc instead of exceptions: rc stays 0 on success and
 * is set to -1 when either unit string is unrecognised, in which case the
 * input value is returned unchanged. The double literals here intentionally
 * duplicate catchem::constants (which is fp-typed) because these helpers must
 * stay double-precision regardless of the USE_REAL8 build.
 */
#pragma once
#include "catchem_kokkos_compat.hpp"

#include "catchem_constants.hpp"
#include "catchem_precision.hpp"
#include <algorithm>
#include <string>
#include <vector>

namespace catchem {
    /**
     * @brief Chemistry unit conversion helpers.
     *
     * @details Stateless inline functions; no translation-unit state and safe
     * for concurrent use. See the file block for the two calling conventions.
     */
    namespace unit_conversion {

        // Numerical constants matching legacy constants.F90
        constexpr double BOLTZ = 1.380649e-23;     ///< Boltzmann constant [J/K].
        constexpr double AVOGADRO = 6.02214076e23; ///< Avogadro number [molecules/mol].
        constexpr double RSTARG = 8.314462618;     ///< Universal gas constant [J/(mol*K)].
        constexpr double AIRMW = 28.9644;          ///< Dry air molecular weight [g/mol].

        /**
         * @brief Air density from temperature and pressure.
         *
         * @param temp         Temperature [K]; must be positive for a physical result.
         * @param press        Pressure [Pa].
         * @param humidity     Water vapour mixing ratio [kg kg-1]; used only when
         *                     use_humidity is true.
         * @param use_humidity When true, form the virtual temperature with
         *                     rv = 461.5 and rd = 287.05 and return the moist
         *                     density; when false, use the dry ideal-gas law
         *                     with AIRMW/RSTARG.
         * @return Air density [kg m-3].
         *
         * @note Device-callable. Neither branch guards temp == 0, so callers
         * must keep temperatures physical.
         */
        KOKKOS_INLINE_FUNCTION
        double calculate_air_density(double temp, double press, double humidity = 0.0, bool use_humidity = false) {
            if (use_humidity) {
                double rv = 461.5;
                double rd = 287.05;
                double eps = rd / rv;
                double vtemp = temp * (1.0 + humidity * (1.0 - eps) / eps);
                return press / (rd * vtemp);
            } else {
                return press * AIRMW / (RSTARG * temp) * 1.0e-3;
            }
        }

        /**
         * @brief Convert a volume mixing ratio from ppbv to mass concentration.
         * @param ppbv  Mixing ratio [ppbv].
         * @param mw    Species molecular weight [g mol-1].
         * @param temp  Air temperature [K]; must be positive.
         * @param press Air pressure [Pa].
         * @return Mass concentration [ug m-3] from the ideal gas law.
         */
        KOKKOS_INLINE_FUNCTION
        double ppbv_to_ugm3(double ppbv, double mw, double temp, double press) {
            return ppbv * mw * press / (RSTARG * temp) * 1.0e-3;
        }

        /**
         * @brief Convert a mass concentration to a volume mixing ratio.
         * @param ugm3  Mass concentration [ug m-3].
         * @param mw    Species molecular weight [g mol-1]; must be non-zero.
         * @param temp  Air temperature [K]; must be positive.
         * @param press Air pressure [Pa]; must be non-zero.
         * @return Mixing ratio [ppbv]; the exact inverse of ppbv_to_ugm3().
         */
        KOKKOS_INLINE_FUNCTION
        double ugm3_to_ppbv(double ugm3, double mw, double temp, double press) {
            return ugm3 * RSTARG * temp / (mw * press) * 1.0e3;
        }

        /**
         * @brief Convert a molar concentration to a volume mixing ratio.
         * @param molcm3 Molar concentration [mol cm-3].
         * @param temp   Air temperature [K]; must be positive.
         * @param press  Air pressure [Pa]; sets the ideal-gas number density
         *               press/(BOLTZ*temp) in molecules cm-3.
         * @return Mixing ratio [ppbv].
         */
        KOKKOS_INLINE_FUNCTION
        double molcm3_to_ppbv(double molcm3, double temp, double press) {
            double number_density = press / (BOLTZ * temp) * 1.0e-6;
            return molcm3 / number_density * 1.0e9;
        }

        /**
         * @brief Convert a volume mixing ratio to a molar concentration.
         * @param ppbv  Mixing ratio [ppbv].
         * @param temp  Air temperature [K]; must be positive.
         * @param press Air pressure [Pa].
         * @return Molar concentration [mol cm-3]; inverse of molcm3_to_ppbv().
         */
        KOKKOS_INLINE_FUNCTION
        double ppbv_to_molcm3(double ppbv, double temp, double press) {
            double number_density = press / (BOLTZ * temp) * 1.0e-6;
            return ppbv * number_density * 1.0e-9;
        }

        /**
         * @brief Convert a volume mixing ratio from ppmv to mass concentration.
         * @param ppmv  Mixing ratio [ppmv].
         * @param mw    Species molecular weight [g mol-1].
         * @param temp  Air temperature [K]; must be positive.
         * @param press Air pressure [Pa].
         * @return Mass concentration [mg m-3].
         */
        KOKKOS_INLINE_FUNCTION
        double ppmv_to_mgm3(double ppmv, double mw, double temp, double press) {
            return ppmv * mw * press / (RSTARG * temp);
        }

        /**
         * @brief Convert a mass concentration to a volume mixing ratio.
         * @param mgm3  Mass concentration [mg m-3].
         * @param mw    Species molecular weight [g mol-1]; must be non-zero.
         * @param temp  Air temperature [K]; must be positive.
         * @param press Air pressure [Pa]; must be non-zero.
         * @return Mixing ratio [ppmv]; inverse of ppmv_to_mgm3().
         */
        KOKKOS_INLINE_FUNCTION
        double mgm3_to_ppmv(double mgm3, double mw, double temp, double press) {
            return mgm3 * RSTARG * temp / (mw * press);
        }

        /**
         * @brief Convert a molecular surface exchange flux to mass units.
         * @param molcm2s Molecules flux [molecules cm-2 s-1]; the legacy name
         *                says "mol" but the value is divided by AVOGADRO, so
         *                the input is a molecule count.
         * @param mw      Species molecular weight [g mol-1].
         * @return Flux [kg m-2 s-1].
         */
        KOKKOS_INLINE_FUNCTION
        double molcm2s_to_kgm2s(double molcm2s, double mw) {
            return (molcm2s / AVOGADRO) * mw * 1.0e-3 * 1.0e4;
        }

        /**
         * @brief Convert a mass exchange flux to molecular units.
         * @param kgm2s Flux [kg m-2 s-1].
         * @param mw    Species molecular weight [g mol-1]; must be non-zero.
         * @return Flux [molecules cm-2 s-1]; inverse of molcm2s_to_kgm2s().
         */
        KOKKOS_INLINE_FUNCTION
        double kgm2s_to_molcm2s(double kgm2s, double mw) {
            return (kgm2s / mw) * AVOGADRO * 1.0e3 * 1.0e-4;
        }

        /**
         * @brief Molecular-weight lookup for common gas-phase species.
         * @param formula Chemical formula, matched case-insensitively against
         *                O3, NO2, CO, SO2, HNO3, NH3, HCHO, CH4, and CO2.
         * @return Molecular weight [g mol-1]. Unrecognised formulas return the
         *         dry-air value AIRMW (28.9644), never zero.
         */
        inline double calculate_molecular_weight(const std::string& formula) {
            std::string f = formula;
            std::transform(f.begin(), f.end(), f.begin(), ::toupper);
            if (f == "O3")
                return 48.0;
            if (f == "NO2")
                return 46.0055;
            if (f == "CO")
                return 28.01;
            if (f == "SO2")
                return 64.066;
            if (f == "HNO3")
                return 63.012;
            if (f == "NH3")
                return 17.031;
            if (f == "HCHO")
                return 30.026;
            if (f == "CH4")
                return 16.04;
            if (f == "CO2")
                return 44.01;
            return AIRMW;
        }

        /**
         * @brief Uppercase a unit string for case-insensitive matching.
         * @param str Unit text as written in configuration or file metadata.
         * @return The uppercase copy used by every convert_* comparison.
         */
        inline std::string to_upper(const std::string& str) {
            std::string s = str;
            std::transform(s.begin(), s.end(), s.begin(), ::toupper);
            return s;
        }

        /**
         * @brief Convert a pressure between supported units via pascals.
         * @param val    Pressure value expressed in from_u.
         * @param from_u Source unit, case-insensitive: PA, HPA/MB/MBAR, ATM,
         *               TORR/MMHG, or PSI.
         * @param to_u   Target unit from the same set.
         * @param rc     Output status: 0 on success; -1 when either unit is
         *               unrecognised.
         * @return The converted pressure in to_u, or val unchanged when rc is -1.
         */
        inline double convert_pressure(double val, const std::string& from_u, const std::string& to_u, int& rc) {
            rc = 0;
            double val_pa = val;
            std::string from = to_upper(from_u);
            std::string to = to_upper(to_u);

            // Convert to Pa
            if (from == "PA")
                val_pa = val;
            else if (from == "HPA" || from == "MB" || from == "MBAR")
                val_pa = val * 100.0;
            else if (from == "ATM")
                val_pa = val * 101325.0;
            else if (from == "TORR" || from == "MMHG")
                val_pa = val * 133.322387415;
            else if (from == "PSI")
                val_pa = val * 6894.75729317;
            else {
                rc = -1;
                return val;
            }

            // Convert from Pa to Target
            if (to == "PA")
                return val_pa;
            if (to == "HPA" || to == "MB" || to == "MBAR")
                return val_pa / 100.0;
            if (to == "ATM")
                return val_pa / 101325.0;
            if (to == "TORR" || to == "MMHG")
                return val_pa / 133.322387415;
            if (to == "PSI")
                return val_pa / 6894.75729317;

            rc = -1;
            return val;
        }

        /**
         * @brief Convert a temperature between kelvin, Celsius, and Fahrenheit.
         * @param val    Temperature value expressed in from_u.
         * @param from_u Source unit, case-insensitive: K/KELVIN, C/CELSIUS/
         *               DEGC/DEGREE_CELSIUS/DEGREE_C, or F/FAHRENHEIT/DEGF/
         *               DEGREE_FAHRENHEIT/DEGREE_F.
         * @param to_u   Target unit from the same set.
         * @param rc     Output status: 0 on success; -1 when either unit is
         *               unrecognised.
         * @return The converted temperature in to_u, or val unchanged when rc is -1.
         *
         * @note The Celsius path uses a 271.15 K offset (not 273.15),
         * deliberately matching the legacy Fortran constant; the Fahrenheit
         * path uses the standard 273.15 offset.
         */
        inline double convert_temperature(double val, const std::string& from_u, const std::string& to_u, int& rc) {
            rc = 0;
            double val_k = val;
            std::string from = to_upper(from_u);
            std::string to = to_upper(to_u);

            // Convert to K
            if (from == "K" || from == "KELVIN")
                val_k = val;
            else if (from == "C" || from == "CELSIUS" || from == "DEGC" || from == "DEGREE_CELSIUS" ||
                     from == "DEGREE_C")
                val_k = val + 271.15; // match legacy constant
            else if (from == "F" || from == "FAHRENHEIT" || from == "DEGF" || from == "DEGREE_FAHRENHEIT" ||
                     from == "DEGREE_F")
                val_k = (val - 32.0) * 5.0 / 9.0 + 273.15;
            else {
                rc = -1;
                return val;
            }

            // Convert from K to Target
            if (to == "K" || to == "KELVIN")
                return val_k;
            if (to == "C" || to == "CELSIUS" || to == "DEGC" || to == "DEGREE_CELSIUS" || to == "DEGREE_C")
                return val_k - 271.15;
            if (to == "F" || to == "FAHRENHEIT" || to == "DEGF" || to == "DEGREE_FAHRENHEIT" || to == "DEGREE_F")
                return (val_k - 273.15) * 9.0 / 5.0 + 32.0;

            rc = -1;
            return val;
        }

        /**
         * @brief Convert a surface exchange flux between mass and molecular units.
         * @param val    Flux value expressed in from_u.
         * @param from_u Source unit, case-insensitive: KG/M2/S or KG M-2 S-1,
         *               MOLEC/CM2/S variants, or MOLEC/M2/S variants.
         * @param to_u   Target unit from the same set.
         * @param mw     Species molecular weight [g mol-1] used for the
         *               molecule-to-mass crossover.
         * @param rc     Output status: 0 on success; -1 when either unit is
         *               unrecognised.
         * @return The converted flux in to_u, or val unchanged when rc is -1.
         */
        inline double convert_flux(double val, const std::string& from_u, const std::string& to_u, double mw, int& rc) {
            rc = 0;
            double val_kgm2s = val;
            std::string from = to_upper(from_u);
            std::string to = to_upper(to_u);

            // Convert to kg/m2/s
            if (from == "KG/M2/S" || from == "KG M-2 S-1")
                val_kgm2s = val;
            else if (from == "MOLEC/CM2/S" || from == "MOLEC CM-2 S-1" || from == "MOLECULES/CM2/S")
                val_kgm2s = molcm2s_to_kgm2s(val, mw);
            else if (from == "MOLEC/M2/S" || from == "MOLEC M-2 S-1" || from == "MOLECULES/M2/S")
                val_kgm2s = molcm2s_to_kgm2s(val * 1e-4, mw);
            else {
                rc = -1;
                return val;
            }

            // Convert from kg/m2/s to Target
            if (to == "KG/M2/S" || to == "KG M-2 S-1")
                return val_kgm2s;
            if (to == "MOLEC/CM2/S" || to == "MOLEC CM-2 S-1" || to == "MOLECULES/CM2/S")
                return kgm2s_to_molcm2s(val_kgm2s, mw);
            if (to == "MOLEC/M2/S" || to == "MOLEC M-2 S-1" || to == "MOLECULES/M2/S")
                return kgm2s_to_molcm2s(val_kgm2s, mw) * 1e4;

            rc = -1;
            return val;
        }

        /**
         * @brief Convert a chemical rate constant between cm3/molecule/s, m3/molecule/s, and 1/s.
         * @param val    Rate value expressed in from_u.
         * @param from_u Source unit, case-insensitive: CM3/MOLECULE/S or
         *               CM3 MOLEC-1 S-1, M3/MOLECULE/S or M3 MOLEC-1 S-1,
         *               or 1/S / S-1 (photolysis and first-order loss).
         * @param to_u   Target unit from the same set.
         * @param rc     Output status: 0 on success; -1 when either unit is
         *               unrecognised.
         * @return The converted rate in to_u, or val unchanged when rc is -1.
         *
         * @note The code treats S-1 as numerically identical to the
         * m3/molecule-1/s-1 base, so converting 1/S to CM3/MOLECULE/S
         * (or back) applies the same 1e6 factor as between the two
         * volume-based units rather than passing the value through.
         */
        inline double convert_rate_constant(double val, const std::string& from_u, const std::string& to_u, int& rc) {
            rc = 0;
            double val_mks = val;
            std::string from = to_upper(from_u);
            std::string to = to_upper(to_u);

            if (from == "CM3/MOLECULE/S" || from == "CM3 MOLEC-1 S-1")
                val_mks = val * 1e-6;
            else if (from == "M3/MOLECULE/S" || from == "M3 MOLEC-1 S-1")
                val_mks = val;
            else if (from == "1/S" || from == "S-1")
                val_mks = val;
            else {
                rc = -1;
                return val;
            }

            if (to == "CM3/MOLECULE/S" || to == "CM3 MOLEC-1 S-1")
                return val_mks * 1e6;
            if (to == "M3/MOLECULE/S" || to == "M3 MOLEC-1 S-1")
                return val_mks;
            if (to == "1/S" || to == "S-1")
                return val_mks;

            rc = -1;
            return val;
        }

        /**
         * @brief Convert a mass between SI and common imperial mass units.
         * @param val    Mass value expressed in from_u.
         * @param from_u Source unit, case-insensitive: KG/KILOGRAM, G/GRAM,
         *               MG/MILLIGRAM, UG/MICROGRAM, LB/POUND/LBS, or OZ/OUNCE.
         * @param to_u   Target unit from the same set.
         * @param rc     Output status: 0 on success; -1 when either unit is
         *               unrecognised.
         * @return The converted mass in to_u, or val unchanged when rc is -1.
         */
        inline double convert_mass_units(double val, const std::string& from_u, const std::string& to_u, int& rc) {
            rc = 0;
            double val_kg = val;
            std::string from = to_upper(from_u);
            std::string to = to_upper(to_u);

            if (from == "KG" || from == "KILOGRAM")
                val_kg = val;
            else if (from == "G" || from == "GRAM")
                val_kg = val * 1e-3;
            else if (from == "MG" || from == "MILLIGRAM")
                val_kg = val * 1e-6;
            else if (from == "UG" || from == "MICROGRAM")
                val_kg = val * 1e-9;
            else if (from == "LB" || from == "POUND" || from == "LBS")
                val_kg = val * 0.45359237;
            else if (from == "OZ" || from == "OUNCE")
                val_kg = val * 0.028349523125;
            else {
                rc = -1;
                return val;
            }

            if (to == "KG" || to == "KILOGRAM")
                return val_kg;
            if (to == "G" || to == "GRAM")
                return val_kg * 1e3;
            if (to == "MG" || to == "MILLIGRAM")
                return val_kg * 1e6;
            if (to == "UG" || to == "MICROGRAM")
                return val_kg * 1e9;
            if (to == "LB" || to == "POUND" || to == "LBS")
                return val_kg / 0.45359237;
            if (to == "OZ" || to == "OUNCE")
                return val_kg / 0.028349523125;

            rc = -1;
            return val;
        }

        /**
         * @brief Convert a quantity between metric and imperial units in one category.
         * @param val    Value expressed in from_u.
         * @param from_u Source unit within the category, case-insensitive.
         * @param to_u   Target unit within the category, case-insensitive.
         * @param cat    Quantity category, case-insensitive: LENGTH (M, CM, MM,
         *               IN, FT, YD, MI), AREA (M2, CM2, IN2, FT2, ACRE), VOLUME
         *               (M3, L, CM3/CC, GAL, FT3), SPEED or VELOCITY (M/S, KM/H,
         *               MPH, KTS), FORCE (N, DYNE, LBF), or ENERGY (J, CAL,
         *               KCAL, BTU, WH, KWH).
         * @param rc     Output status: 0 on success; -1 when the category or
         *               either unit is unrecognised.
         * @return The converted value in to_u, or val unchanged when rc is -1.
         */
        inline double convert_imperial(double val, const std::string& from_u, const std::string& to_u,
                                       const std::string& cat, int& rc) {
            rc = 0;
            std::string c = to_upper(cat);
            std::string from = to_upper(from_u);
            std::string to = to_upper(to_u);

            if (c == "LENGTH") {
                double val_m = val;
                if (from == "M" || from == "METER")
                    val_m = val;
                else if (from == "CM" || from == "CENTIMETER")
                    val_m = val * 1e-2;
                else if (from == "MM" || from == "MILLIMETER")
                    val_m = val * 1e-3;
                else if (from == "IN" || from == "INCH")
                    val_m = val * 0.0254;
                else if (from == "FT" || from == "FOOT" || from == "FEET")
                    val_m = val * 0.3048;
                else if (from == "YD" || from == "YARD")
                    val_m = val * 0.9144;
                else if (from == "MI" || from == "MILE")
                    val_m = val * 1609.344;
                else {
                    rc = -1;
                    return val;
                }

                if (to == "M" || to == "METER")
                    return val_m;
                if (to == "CM" || to == "CENTIMETER")
                    return val_m * 1e2;
                if (to == "MM" || to == "MILLIMETER")
                    return val_m * 1e3;
                if (to == "IN" || to == "INCH")
                    return val_m / 0.0254;
                if (to == "FT" || to == "FOOT" || to == "FEET")
                    return val_m / 0.3048;
                if (to == "YD" || to == "YARD")
                    return val_m / 0.9144;
                if (to == "MI" || to == "MILE")
                    return val_m / 1609.344;
            } else if (c == "AREA") {
                double val_m2 = val;
                if (from == "M2")
                    val_m2 = val;
                else if (from == "CM2")
                    val_m2 = val * 1e-4;
                else if (from == "IN2")
                    val_m2 = val * 0.00064516;
                else if (from == "FT2")
                    val_m2 = val * 0.09290304;
                else if (from == "ACRE" || from == "ACRES")
                    val_m2 = val * 4046.8564224;
                else {
                    rc = -1;
                    return val;
                }

                if (to == "M2")
                    return val_m2;
                if (to == "CM2")
                    return val_m2 * 1e4;
                if (to == "IN2")
                    return val_m2 / 0.00064516;
                if (to == "FT2")
                    return val_m2 / 0.09290304;
                if (to == "ACRE" || to == "ACRES")
                    return val_m2 / 4046.8564224;
            } else if (c == "VOLUME") {
                double val_m3 = val;
                if (from == "M3")
                    val_m3 = val;
                else if (from == "L" || from == "LITER" || from == "LITERS")
                    val_m3 = val * 1e-3;
                else if (from == "CM3" || from == "CC")
                    val_m3 = val * 1e-6;
                else if (from == "GAL" || from == "GALLON" || from == "GALLONS")
                    val_m3 = val * 0.003785411784;
                else if (from == "FT3")
                    val_m3 = val * 0.028316846592;
                else {
                    rc = -1;
                    return val;
                }

                if (to == "M3")
                    return val_m3;
                if (to == "L" || to == "LITER" || to == "LITERS")
                    return val_m3 * 1e3;
                if (to == "CM3" || to == "CC")
                    return val_m3 * 1e6;
                if (to == "GAL" || to == "GALLON" || to == "GALLONS")
                    return val_m3 / 0.003785411784;
                if (to == "FT3")
                    return val_m3 / 0.028316846592;
            } else if (c == "SPEED" || c == "VELOCITY") {
                double val_ms = val;
                if (from == "M/S" || from == "M S-1")
                    val_ms = val;
                else if (from == "KM/H" || from == "KMH")
                    val_ms = val / 3.6;
                else if (from == "MPH" || from == "MILE/H")
                    val_ms = val * 0.44704;
                else if (from == "KTS" || from == "KNOT" || from == "KNOTS")
                    val_ms = val * 0.5144444444;
                else {
                    rc = -1;
                    return val;
                }

                if (to == "M/S" || to == "M S-1")
                    return val_ms;
                if (to == "KM/H" || to == "KMH")
                    return val_ms * 3.6;
                if (to == "MPH" || to == "MILE/H")
                    return val_ms / 0.44704;
                if (to == "KTS" || to == "KNOT" || to == "KNOTS")
                    return val_ms / 0.5144444444;
            } else if (c == "FORCE") {
                double val_n = val;
                if (from == "N" || from == "NEWTON")
                    val_n = val;
                else if (from == "DYNE")
                    val_n = val * 1e-5;
                else if (from == "LBF" || from == "POUND-FORCE")
                    val_n = val * 4.4482216152605;
                else {
                    rc = -1;
                    return val;
                }

                if (to == "N" || to == "NEWTON")
                    return val_n;
                if (to == "DYNE")
                    return val_n * 1e5;
                if (to == "LBF" || to == "POUND-FORCE")
                    return val_n / 4.4482216152605;
            } else if (c == "ENERGY") {
                double val_j = val;
                if (from == "J" || from == "JOULE")
                    val_j = val;
                else if (from == "CAL" || from == "CALORIE")
                    val_j = val * 4.184;
                else if (from == "KCAL")
                    val_j = val * 4184.0;
                else if (from == "BTU")
                    val_j = val * 1055.05585262;
                else if (from == "WH")
                    val_j = val * 3600.0;
                else if (from == "KWH")
                    val_j = val * 3600000.0;
                else {
                    rc = -1;
                    return val;
                }

                if (to == "J" || to == "JOULE")
                    return val_j;
                if (to == "CAL" || to == "CALORIE")
                    return val_j / 4.184;
                if (to == "KCAL")
                    return val_j / 4184.0;
                if (to == "BTU")
                    return val_j / 1055.05585262;
                if (to == "WH")
                    return val_j / 3600.0;
                if (to == "KWH")
                    return val_j / 3600000.0;
            }

            rc = -1;
            return val;
        }

    } // namespace unit_conversion
} // namespace catchem

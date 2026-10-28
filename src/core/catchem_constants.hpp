/**
 * @file catchem_constants.hpp
 * @brief Physical and mathematical constants shared by the CATChem core.
 *
 * @details Header-only compile-time constants in the catchem::constants
 * namespace, carried over from the legacy Fortran constants.F90 so derived
 * quantities (hydrostatic thickness, air density, solar geometry) reproduce
 * upstream values bit-for-bit at a given precision. Each constant has the
 * working real type catchem::fp, so a single-precision build (USE_REAL8
 * undefined) narrows the literals below; the values are not hard-coded to
 * double.
 *
 * @see catchem_precision.hpp for the fp selection, and
 * catchem_unit_conversion.hpp, which repeats several of these literals as
 * explicit double values for the unit-conversion helpers.
 */
#pragma once

#include "catchem_precision.hpp"

namespace catchem {
    /**
     * @brief Legacy-compatible physical constants.
     *
     * @details All members are compile-time constexpr in catchem::fp; there is
     * no runtime state and nothing to initialise.
     */
    namespace constants {

        constexpr fp AVO = 6.022140857e23;        ///< Avogadro constant [molecules mol-1].
        constexpr fp G0 = 9.80665;                ///< Standard gravitational acceleration [m s-2].
        constexpr fp RSTARG = 8.3144598;          ///< Universal gas constant [J mol-1 K-1].
        constexpr fp BOLTZ = 1.38064852e-23;      ///< Boltzmann constant [J K-1].
        constexpr fp ATM = 1.01325e5;             ///< Standard atmospheric pressure [Pa].
        constexpr fp AIR_MW = 28.9644;            ///< Dry-air molecular weight [g mol-1].
        constexpr fp H2O_MW = 18.016;             ///< Water molecular weight [g mol-1].
        constexpr fp RD = 287.0;                  ///< Dry-air gas constant [J kg-1 K-1].
        constexpr fp CP = 1004.6;                 ///< Specific heat of dry air at constant pressure [J kg-1 K-1].
        constexpr fp PI = 3.14159265358979323846; ///< Circle constant pi.
        constexpr fp PI_180 = PI / 180.0;         ///< Degrees-to-radians conversion factor [rad deg-1].

    } // namespace constants
} // namespace catchem

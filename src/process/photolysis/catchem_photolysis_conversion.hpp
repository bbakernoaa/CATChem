/**
 * @file catchem_photolysis_conversion.hpp
 * @brief Unit conversions used to prepare MUSICA TUV-x profiles.
 *
 * @details Header-only inline conversions shared between the photolysis
 * process and its tests. CATChem stores gas constituents as dry-air mole
 * fractions (ppmv) while MUSICA TUV-x ozone profiles require number density
 * (molecules cm-3); keeping the factor and its domain checks in one place
 * guarantees both sides of the FFI agree on the conversion.
 */

#pragma once

#include <cmath>
#include <stdexcept>

namespace catchem {

    /**
     * @brief Convert ozone dry-air mole fraction in ppmv to number density.
     * @param ozone_ppmv Ozone mole fraction in parts per million by volume.
     * @param dry_air_number_density_cm3 Dry-air number density in molecules cm-3.
     * @return Ozone number density in molecules cm-3.
     * @throws std::domain_error If either input is non-finite or physically invalid.
     */
    inline double photolysis_ozone_ppmv_to_number_density(double ozone_ppmv, double dry_air_number_density_cm3) {
        if (!std::isfinite(ozone_ppmv) || ozone_ppmv < 0.0 || !std::isfinite(dry_air_number_density_cm3) ||
            dry_air_number_density_cm3 <= 0.0)
            throw std::domain_error(
                "Photolysis ozone conversion requires nonnegative ppmv and positive dry-air density");
        const double number_density = ozone_ppmv * 1.0e-6 * dry_air_number_density_cm3;
        if (!std::isfinite(number_density))
            throw std::domain_error("Photolysis ozone number density is non-finite");
        return number_density;
    }

} // namespace catchem

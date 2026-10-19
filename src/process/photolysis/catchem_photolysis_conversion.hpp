/**
 * @file catchem_photolysis_conversion.hpp
 * @brief Unit conversions used to prepare MUSICA TUV-x profiles.
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
    inline double photolysis_ozone_ppmv_to_number_density(double ozone_ppmv,
                                                          double dry_air_number_density_cm3) {
        if (!std::isfinite(ozone_ppmv) || ozone_ppmv < 0.0 ||
            !std::isfinite(dry_air_number_density_cm3) || dry_air_number_density_cm3 <= 0.0)
            throw std::domain_error("Photolysis ozone conversion requires nonnegative ppmv and positive dry-air density");
        const double number_density = ozone_ppmv * 1.0e-6 * dry_air_number_density_cm3;
        if (!std::isfinite(number_density))
            throw std::domain_error("Photolysis ozone number density is non-finite");
        return number_density;
    }

} // namespace catchem

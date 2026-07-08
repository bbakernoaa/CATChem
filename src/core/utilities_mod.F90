!> \file utilities_mod.F90
!! \brief General utility functions for CATChem
!! \ingroup core_modules
!!
!! \author CATChem Development Team
!! \date 2025
!! \version 2.0
!!
!! This module provides general utility functions for unit conversions,
!! calculations, and validation that are used throughout CATChem.
!!
!! \details
!! The utilities module includes:
!! - Unit conversion functions (pressure, temperature)
!! - Atmospheric calculation utilities
!! - Validation functions for physical consistency
!! - Common mathematical operations
!! - Error checking and validation helpers
!!
!! \section utilities_usage Usage Example
!! \code{.f90}
!! use utilities_mod
!! real(fp) :: pressure_hpa, pressure_pa
!! integer :: rc
!!
!! pressure_pa = convert_pressure_units(pressure_hpa, 'hPa', 'Pa', rc)
!! call validate_atmospheric_constants(rc)
!! \endcode
!!
module utilities_mod
   use precision_mod
   use constants
   use error_mod, only: CC_SUCCESS, CC_FAILURE, ERROR_NUMERICAL_INSTABILITY, ERROR_INVALID_INPUT, &
      ErrorManagerType
   use UnitConversion_Mod, only: convert_pressure, convert_temperature, &
      uc_calculate_air_density => calculate_air_density

   implicit none
   private

   ! Public utility functions
   public :: validate_atmospheric_constants
   public :: convert_pressure_units
   public :: convert_temperature_units
   public :: calculate_air_density
   public :: calculate_scale_height
   public :: is_valid_temperature
   public :: is_valid_pressure
   public :: check_array_bounds
   public :: safe_divide
   public :: calculate_geopotential_height

contains

   !> \brief Validate atmospheric constants for physical consistency
   !!
   !! This subroutine performs runtime validation of atmospheric constants
   !! to ensure physical consistency and catch any compilation issues.
   !!
   !! \param[out] rc Return code (CC_SUCCESS if all constants are valid)
   !! \param[in] error_mgr Optional error manager for enhanced reporting
   subroutine validate_atmospheric_constants(rc, error_mgr)
      implicit none
      integer, intent(out) :: rc
      type(ErrorManagerType), intent(inout), optional :: error_mgr

      rc = CC_SUCCESS

      if (present(error_mgr)) then
         call error_mgr%push_context('validate_atmospheric_constants', 'checking physical constants')
      endif

      ! Check fundamental constants
      if (g0 <= 0.0_fp) then
         if (present(error_mgr)) then
            call error_mgr%report_error(ERROR_NUMERICAL_INSTABILITY, &
               'Invalid gravitational acceleration', rc)
         else
            rc = CC_FAILURE
         endif
         return
      endif

      if (Rd <= 0.0_fp) then
         if (present(error_mgr)) then
            call error_mgr%report_error(ERROR_NUMERICAL_INSTABILITY, &
               'Invalid dry air gas constant', rc)
         else
            rc = CC_FAILURE
         endif
         return
      endif

      ! Check derived relationships
      if (abs(Cp - Cv - Rd) > 1.0e-6_fp) then
         if (present(error_mgr)) then
            call error_mgr%report_error(ERROR_NUMERICAL_INSTABILITY, &
               'Inconsistent thermodynamic constants: Cp - Cv ≠ Rd', rc)
         else
            rc = CC_FAILURE
         endif
         return
      endif

      if (present(error_mgr)) then
         call error_mgr%pop_context()
      endif

   end subroutine validate_atmospheric_constants

   !> \brief Convert pressure between different units
   !!
   !! Delegates to UnitConversion_Mod for the actual conversion.
   !! Accepted units: 'Pa', 'pa', 'hPa', 'hpa', 'mb', 'mbar', 'atm',
   !!                 'Torr', 'torr', 'mmHg', 'mmhg', 'psi'
   !!
   !! \param[in] pressure_in Input pressure value
   !! \param[in] unit_in Input unit
   !! \param[in] unit_out Output unit
   !! \param[out] rc Return code
   function convert_pressure_units(pressure_in, unit_in, unit_out, rc) result(pressure_out)
      implicit none
      real(fp), intent(in) :: pressure_in
      character(len=*), intent(in) :: unit_in, unit_out
      integer, intent(out) :: rc
      real(fp) :: pressure_out

      pressure_out = convert_pressure(pressure_in, unit_in, unit_out, rc)
      if (rc == CC_FAILURE) then
         rc = ERROR_INVALID_INPUT
      endif

   end function convert_pressure_units

   !> \brief Convert temperature between different units
   !!
   !! Delegates to UnitConversion_Mod for the actual conversion.
   !! Accepted units: 'K', 'Kelvin', 'kelvin', 'C', 'Celsius', 'celsius',
   !!                 'F', 'Fahrenheit', 'fahrenheit'
   !!
   !! \param[in] temp_in Input temperature value
   !! \param[in] unit_in Input unit
   !! \param[in] unit_out Output unit
   !! \param[out] rc Return code
   function convert_temperature_units(temp_in, unit_in, unit_out, rc) result(temp_out)
      implicit none
      real(fp), intent(in) :: temp_in
      character(len=*), intent(in) :: unit_in, unit_out
      integer, intent(out) :: rc
      real(fp) :: temp_out

      temp_out = convert_temperature(temp_in, unit_in, unit_out, rc)
      if (rc == CC_FAILURE) then
         rc = ERROR_INVALID_INPUT
      endif

   end function convert_temperature_units

   !> \brief Calculate air density using ideal gas law
   !!
   !! Delegates to UnitConversion_Mod for the actual calculation.
   !!
   !! \param[in] pressure Pressure [Pa]
   !! \param[in] temperature Temperature [K]
   !! \param[out] rc Return code
   function calculate_air_density(pressure, temperature, rc) result(density)
      implicit none
      real(fp), intent(in) :: pressure, temperature
      integer, intent(out) :: rc
      real(fp) :: density

      rc = CC_SUCCESS

      if (temperature <= 0.0_fp) then
         rc = ERROR_INVALID_INPUT
         density = 0.0_fp
         return
      endif

      if (pressure < 0.0_fp) then
         rc = ERROR_INVALID_INPUT
         density = 0.0_fp
         return
      endif

      density = uc_calculate_air_density(temperature, pressure)

   end function calculate_air_density

   !> \brief Calculate atmospheric scale height using geopotential height
   !!
   !! \param[in] temperature Temperature [K]
   !! \param[out] rc Return code
   !! \param[in] z (optional) Geometric height above surface [m]
   function calculate_scale_height(temperature, rc, z) result(scale_height)
      use constants, only: g0, Rd, Re
      implicit none
      real(fp), intent(in) :: temperature
      integer, intent(out) :: rc
      real(fp), intent(in), optional :: z
      real(fp) :: scale_height
      real(fp) :: g_local

      rc = CC_SUCCESS

      if (temperature <= 0.0_fp) then
         rc = ERROR_INVALID_INPUT
         scale_height = 0.0_fp
         return
      endif

      if (present(z)) then
         g_local = g0 * (Re / (Re + z))
      else
         g_local = g0
      endif

      scale_height = Rd * temperature / g_local

   end function calculate_scale_height

   !> \brief Check if temperature is in valid range
   !!
   !! \param[in] temperature Temperature [K]
   function is_valid_temperature(temperature) result(is_valid)
      implicit none
      real(fp), intent(in) :: temperature
      logical :: is_valid

      ! Valid range: 100K to 400K (typical atmospheric range)
      is_valid = (temperature >= 100.0_fp .and. temperature <= 400.0_fp)

   end function is_valid_temperature

   !> \brief Check if pressure is in valid range
   !!
   !! \param[in] pressure Pressure [Pa]
   function is_valid_pressure(pressure) result(is_valid)
      implicit none
      real(fp), intent(in) :: pressure
      logical :: is_valid

      ! Valid range: 1 Pa to 110000 Pa (typical atmospheric range)
      is_valid = (pressure >= 1.0_fp .and. pressure <= 110000.0_fp)

   end function is_valid_pressure

   !> \brief Check array bounds safely
   !!
   !! \param[in] index Index to check
   !! \param[in] array_size Size of array
   !! \param[in] array_name Name of array for error reporting
   !! \param[out] rc Return code
   subroutine check_array_bounds(index, array_size, array_name, rc)
      implicit none
      integer, intent(in) :: index, array_size
      character(len=*), intent(in) :: array_name
      integer, intent(out) :: rc

      rc = CC_SUCCESS

      if (index < 1 .or. index > array_size) then
         rc = ERROR_INVALID_INPUT
      endif

   end subroutine check_array_bounds

   !> \brief Safe division with zero check
   !!
   !! \param[in] numerator Numerator
   !! \param[in] denominator Denominator
   !! \param[out] rc Return code
   function safe_divide(numerator, denominator, rc) result(quotient)
      implicit none
      real(fp), intent(in) :: numerator, denominator
      integer, intent(out) :: rc
      real(fp) :: quotient

      rc = CC_SUCCESS

      if (abs(denominator) < epsilon(denominator) * 1000.0_fp) then
         rc = ERROR_NUMERICAL_INSTABILITY
         quotient = 0.0_fp
      else
         quotient = numerator / denominator
      endif

   end function safe_divide

   !> \brief Calculate geopotential height difference between two pressure levels
   !!
   !! \param[in] p1 Pressure at lower level [Pa]
   !! \param[in] p2 Pressure at upper level [Pa]
   !! \param[in] Tv_mean Mean virtual temperature between levels [K]
   !! \param[out] rc Return code
   function calculate_geopotential_height(p1, p2, Tv_mean, rc) result(z)
      use constants, only: g0, Rd
      implicit none
      real(fp), intent(in) :: p1, p2, Tv_mean
      integer, intent(out) :: rc
      real(fp) :: z

      rc = CC_SUCCESS
      z = 0.0_fp

      if (p1 <= 0.0_fp .or. p2 <= 0.0_fp .or. Tv_mean <= 0.0_fp) then
         rc = ERROR_INVALID_INPUT
         return
      endif

      if (p1 <= p2) then
         rc = ERROR_INVALID_INPUT
         return
      endif

      z = (Rd * Tv_mean / g0) * log(p1 / p2)

   end function calculate_geopotential_height

end module utilities_mod

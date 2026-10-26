!> \brief Exercises supported CCPP constituent conversions and validation.
program test_ccpp_conversion
   use iso_c_binding, only: c_double, c_int
   use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_positive_inf
   use catchem_ccpp_conversion
   implicit none
   integer(c_int) :: status
   integer :: layer
   real(c_double) :: value, reverse, density, ozone, dry_ratio, dry_layer_mass, pressure_layer_mass
   real(c_double) :: host_mass, solver_mass, ozone_mass_ratio, ozone_column_number, ozone_column_mass
   real(c_double), parameter :: humid_density(2) = [1.2_c_double, 0.9_c_double]
   real(c_double), parameter :: dry_density_profile(2) = [0.8_c_double, 0.6_c_double]
   real(c_double), parameter :: layer_depth(2) = [100.0_c_double, 250.0_c_double]
   real(c_double), parameter :: conversion_rtol = 64.0_c_double*epsilon(1.0_c_double)

   ! Independently stated dry-air mass/mole conversion for a 64 g/mol gas.
   call catchem_gas_mass_ratio_to_dry_ppmv(1.0e-6_c_double, CATCHEM_AMOUNT_MASS_RATIO, CATCHEM_AIR_DRY, &
      0.064_c_double, 0.0_c_double, 0.0_c_double, value, status)
   call require(status == CATCHEM_CONVERSION_OK, 'dry gas conversion accepted')
   call require_close(value, 1.0e-6_c_double*0.0289647_c_double/0.064_c_double*1.0e6_c_double, &
      'gas mass ratio converts to dry ppmv')
   call catchem_dry_ppmv_to_gas_mass_ratio(1.0_c_double, CATCHEM_AMOUNT_MASS_RATIO, 0.064_c_double, reverse, status)
   call require(status == CATCHEM_CONVERSION_OK, 'inverse gas conversion accepted')
   call require_close(reverse, 1.0e-6_c_double*0.064_c_double/0.0289647_c_double, &
      'dry ppmv converts to dry gas mass ratio')

   call catchem_aerosol_mass_ratio_to_dry_ug_kg(1.0e-9_c_double, CATCHEM_AMOUNT_MASS_RATIO, &
      CATCHEM_AIR_DRY, 0.0_c_double, 0.0_c_double, value, status)
   call require(status == CATCHEM_CONVERSION_OK .and. abs(value - 1.0_c_double) <= conversion_rtol, &
      'aerosol mass ratio maps to micrograms per kilogram')
   call catchem_aerosol_mass_ratio_to_dry_ug_kg(1.0e-9_c_double, CATCHEM_AMOUNT_MOLE_FRACTION, &
      CATCHEM_AIR_DRY, 0.0_c_double, 0.0_c_double, value, status)
   call require(status == CATCHEM_CONVERSION_INVALID, 'aerosol rejects gas mole-fraction amount type')

   call catchem_host_mass_ratio_to_dry(2.0e-6_c_double, CATCHEM_AIR_MOIST, 1.0_c_double, 0.8_c_double, value, status)
   call require(status == CATCHEM_CONVERSION_OK .and. abs(value - 2.5e-6_c_double) <= &
      conversion_rtol*2.5e-6_c_double, &
      'moist mass basis explicitly converts using matching densities')
   call catchem_host_mass_ratio_to_dry(2.0e-6_c_double, CATCHEM_AIR_TOTAL_CONDENSATE, &
      1.2_c_double, 0.8_c_double, value, status)
   call require(status == CATCHEM_CONVERSION_OK .and. abs(value - 3.0e-6_c_double) <= &
      conversion_rtol*3.0e-6_c_double, 'condensate-inclusive basis uses its independently declared density')
   call catchem_host_mass_ratio_to_dry(1.0e-6_c_double, CATCHEM_AIR_TOTAL_CONDENSATE, &
      0.0_c_double, 0.8_c_double, value, status)
   call require(status == CATCHEM_CONVERSION_INVALID, 'required air density must be positive')
   call catchem_host_ppmv_to_dry(100.0_c_double, CATCHEM_AMOUNT_MOLE_FRACTION, CATCHEM_AIR_MOIST, &
      2.5e19_c_double, 2.0e19_c_double, value, status)
   call require(status == CATCHEM_CONVERSION_OK .and. abs(value - 125.0_c_double) <= conversion_rtol*125.0_c_double, &
      'moist volume fraction uses explicit host and dry number densities')

   ! Independent humid WetDep and DryDep mass closures.  The native solver
   ! amount is per dry-air mass; multiply by dry density and layer depth to
   ! recover the same species mass represented by the moist host ratio.
   call catchem_host_mass_ratio_to_dry(2.0e-6_c_double, CATCHEM_AIR_MOIST, &
      humid_density(1), dry_density_profile(1), dry_ratio, status)
   call require(status == CATCHEM_CONVERSION_OK, 'humid WetDep host amount converts to dry-air solver basis')
   host_mass = 2.0e-6_c_double*humid_density(1)*layer_depth(1)
   solver_mass = dry_ratio*dry_density_profile(1)*layer_depth(1)
   call require_close(solver_mass, host_mass, 'humid WetDep preserves layer species mass')
   write (*, '(a,es24.16)') 'PASS humid_wetdep_mass_budget_kg_m2=', abs(solver_mass - host_mass)

   call catchem_host_mass_ratio_to_dry(8.25e-9_c_double, CATCHEM_AIR_MOIST, &
      humid_density(2), dry_density_profile(2), dry_ratio, status)
   call require(status == CATCHEM_CONVERSION_OK, 'humid DryDep host amount converts to dry-air solver basis')
   host_mass = 8.25e-9_c_double*humid_density(2)*layer_depth(2)
   solver_mass = dry_ratio*dry_density_profile(2)*layer_depth(2)
   call require_close(solver_mass, host_mass, 'humid DryDep preserves layer species mass')
   dry_layer_mass = dry_density_profile(2)*layer_depth(2)
   pressure_layer_mass = 1800.0_c_double/9.80665_c_double
   call require(dry_layer_mass <= pressure_layer_mass, 'humid DryDep dry mass remains within hydrostatic layer mass')
   write (*, '(a,es24.16)') 'PASS humid_drydep_mass_budget_kg_m2=', abs(solver_mass - host_mass)

   call catchem_dry_air_number_density(1.0_c_double, density, status)
   call require(status == CATCHEM_CONVERSION_OK .and. density > 2.0e19_c_double, &
      'dry-air number density uses SI density and kg/mol')
   call catchem_dry_ppmv_to_number_density(40.0_c_double, CATCHEM_AMOUNT_MOLE_FRACTION, density, ozone, status)
   call require(status == CATCHEM_CONVERSION_OK .and. abs(ozone - 40.0e-6_c_double*density) <= &
      conversion_rtol*40.0e-6_c_double*density, &
      'dry ozone ppmv converts to molecules per cubic centimetre')

   ! Independent non-round-trip amounts: the results are computed from the
   ! physical definitions and the locked SI constants, not by inverse helpers.
   call catchem_gas_mass_ratio_to_dry_ppmv(2.5e-7_c_double, CATCHEM_AMOUNT_MASS_RATIO, CATCHEM_AIR_DRY, &
      0.044_c_double, 0.0_c_double, 0.0_c_double, value, status)
   call require(status == CATCHEM_CONVERSION_OK, 'analytic gas amount conversion accepted')
   call require_close(value, 2.5e-7_c_double*0.0289647_c_double/0.044_c_double*1.0e6_c_double, &
      'known nonzero gas amount matches independent mole ratio')
   call catchem_aerosol_mass_ratio_to_dry_ug_kg(8.25e-9_c_double, CATCHEM_AMOUNT_MASS_RATIO, &
      CATCHEM_AIR_DRY, 0.0_c_double, 0.0_c_double, value, status)
   call require(status == CATCHEM_CONVERSION_OK .and. abs(value - 8.25_c_double) <= &
      conversion_rtol*8.25_c_double, 'known aerosol mass maps to micrograms per dry kilogram')
   call catchem_dry_air_number_density(0.8_c_double, density, status)
   call require(status == CATCHEM_CONVERSION_OK, 'analytic dry-air number density accepted')
   call catchem_dry_ppmv_to_number_density(45.0_c_double, CATCHEM_AMOUNT_MOLE_FRACTION, density, ozone, status)
   call require(status == CATCHEM_CONVERSION_OK, 'analytic ozone amount conversion accepted')
   call require_close(ozone, 45.0e-6_c_double*(0.8_c_double/0.0289647_c_double*6.02214076e23_c_double*1.0e-6_c_double), &
      'ozone number density agrees with independent SI calculation')
   call catchem_dry_ppmv_to_gas_mass_ratio(45.0_c_double, CATCHEM_AMOUNT_MASS_RATIO, &
      0.048_c_double, ozone_mass_ratio, status)
   call require(status == CATCHEM_CONVERSION_OK, 'ozone ppmv converts to dry mass ratio')
   ozone_column_number = 0.0_c_double
   ozone_column_mass = 0.0_c_double
   do layer = 1, size(dry_density_profile)
      call catchem_dry_air_number_density(dry_density_profile(layer), density, status)
      call require(status == CATCHEM_CONVERSION_OK, 'ozone column layer dry number density is valid')
      call catchem_dry_ppmv_to_number_density(45.0_c_double, CATCHEM_AMOUNT_MOLE_FRACTION, &
         density, ozone, status)
      call require(status == CATCHEM_CONVERSION_OK, 'ozone column ppmv conversion is valid')
      ozone_column_number = ozone_column_number + ozone*layer_depth(layer)*100.0_c_double
      ozone_column_mass = ozone_column_mass + ozone_mass_ratio*dry_density_profile(layer)/0.048_c_double* &
         6.02214076e23_c_double/1.0e6_c_double*layer_depth(layer)*100.0_c_double
   end do
   call require_close(ozone_column_mass, ozone_column_number, &
      'integrated ozone mass and molecular columns agree over humid host layers')
   write (*, '(a,es24.16)') 'PASS ozone_column_equivalence_molecule_cm2=', &
      abs(ozone_column_mass - ozone_column_number)/max(abs(ozone_column_mass) + abs(ozone_column_number), 1.0_c_double)

   ! Equal solver and physical dry layer masses produce an identity conversion.
   call catchem_dry_to_solver_mass_ratio(1.0e-6_c_double, 980.665_c_double, 9.80665_c_double, &
      1.0_c_double, 100.0_c_double, value, status)
   call require(status == CATCHEM_CONVERSION_OK .and. abs(value - 1.0e-6_c_double) <= conversion_rtol*1.0e-6_c_double, &
      'dry ratio converts to solver ratio when layer masses match')
   call catchem_solver_to_dry_mass_ratio(value, 980.665_c_double, 9.80665_c_double, &
      1.0_c_double, 100.0_c_double, reverse, status)
   call require(status == CATCHEM_CONVERSION_OK .and. abs(reverse - 1.0e-6_c_double) <= conversion_rtol*1.0e-6_c_double, &
      'solver ratio converts back to dry ratio')
   call catchem_dry_to_solver_mass_ratio(1.0e-6_c_double, 980.665_c_double, 9.80665_c_double, &
      1.1_c_double, 100.0_c_double, value, status)
   call require(status == CATCHEM_CONVERSION_INVALID, 'dry layer mass cannot exceed hydrostatic pressure-layer mass')

   call catchem_dry_ppmv_to_gas_mass_ratio(ieee_value(value, ieee_positive_inf), &
      CATCHEM_AMOUNT_MASS_RATIO, 0.064_c_double, reverse, status)
   call require(status == CATCHEM_CONVERSION_INVALID, 'nonfinite amount is rejected')
   call catchem_dry_ppmv_to_gas_mass_ratio(1.0_c_double, CATCHEM_AMOUNT_NUMBER_CONCENTRATION, &
      0.064_c_double, reverse, status)
   call require(status == CATCHEM_CONVERSION_INVALID, 'unsupported number concentration is rejected')
   print *, 'PASS test_ccpp_conversions'
contains
   !> \brief Compare a nonzero analytic result under the frozen conversion tolerance.
   !> \param actual Value returned by a conversion.
   !> \param expected Independently derived physical value.
   !> \param message Assertion description.
   subroutine require_close(actual, expected, message)
      real(c_double), intent(in) :: actual, expected
      character(len=*), intent(in) :: message
      call require(abs(actual - expected) <= conversion_rtol*abs(expected), message)
   end subroutine require_close

   !> \brief Fails the test program with the supplied assertion message.
   !> \\param condition True when the assertion passes.
   !> \\param message Description printed when the assertion fails.
   subroutine require(condition, message)
      logical, intent(in) :: condition
      character(len=*), intent(in) :: message
      if (.not. condition) then
         print *, 'FAIL: ', trim(message)
         stop 1
      end if
   end subroutine
end program test_ccpp_conversion

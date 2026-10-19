!> \\brief Exercises supported CCPP constituent conversions and validation.
program test_ccpp_conversion
  use iso_c_binding, only: c_double, c_int
  use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_positive_inf
  use catchem_ccpp_conversion
  implicit none
  integer(c_int) :: status
  real(c_double) :: value, reverse, density, ozone
  real(c_double), parameter :: eps = 1.0e-11_c_double

  ! Independently stated dry-air mass/mole conversion for a 64 g/mol gas.
  call catchem_gas_mass_ratio_to_dry_ppmv(1.0e-6_c_double,CATCHEM_AMOUNT_MASS_RATIO,CATCHEM_AIR_DRY, &
       0.064_c_double,0.0_c_double,0.0_c_double,value,status)
  call require(status == CATCHEM_CONVERSION_OK, 'dry gas conversion accepted')
  call require(abs(value-(1.0e-6_c_double*0.0289647_c_double/0.064_c_double*1.0e6_c_double)) < eps, &
       'gas mass ratio converts to dry ppmv')
  call catchem_dry_ppmv_to_gas_mass_ratio(1.0_c_double,CATCHEM_AMOUNT_MASS_RATIO,0.064_c_double,reverse,status)
  call require(status == CATCHEM_CONVERSION_OK, 'inverse gas conversion accepted')
  call require(abs(reverse-1.0e-6_c_double*0.064_c_double/0.0289647_c_double) < eps, &
       'dry ppmv converts to dry gas mass ratio')

  call catchem_aerosol_mass_ratio_to_dry_ug_kg(1.0e-9_c_double,CATCHEM_AMOUNT_MASS_RATIO, &
       CATCHEM_AIR_DRY,0.0_c_double,0.0_c_double,value,status)
  call require(status == CATCHEM_CONVERSION_OK .and. abs(value-1.0_c_double) < eps, &
       'aerosol mass ratio maps to micrograms per kilogram')
  call catchem_aerosol_mass_ratio_to_dry_ug_kg(1.0e-9_c_double,CATCHEM_AMOUNT_MOLE_FRACTION, &
       CATCHEM_AIR_DRY,0.0_c_double,0.0_c_double,value,status)
  call require(status == CATCHEM_CONVERSION_INVALID, 'aerosol rejects gas mole-fraction amount type')

  call catchem_host_mass_ratio_to_dry(2.0e-6_c_double,CATCHEM_AIR_MOIST,1.0_c_double,0.8_c_double,value,status)
  call require(status == CATCHEM_CONVERSION_OK .and. abs(value-2.5e-6_c_double) < eps, &
       'moist mass basis explicitly converts using matching densities')
  call catchem_host_mass_ratio_to_dry(1.0e-6_c_double,CATCHEM_AIR_TOTAL_CONDENSATE, &
       0.0_c_double,0.8_c_double,value,status)
  call require(status == CATCHEM_CONVERSION_INVALID, 'required air density must be positive')
  call catchem_host_ppmv_to_dry(100.0_c_double,CATCHEM_AMOUNT_MOLE_FRACTION,CATCHEM_AIR_MOIST, &
       2.5e19_c_double,2.0e19_c_double,value,status)
  call require(status == CATCHEM_CONVERSION_OK .and. abs(value-125.0_c_double) < eps, &
       'moist volume fraction uses explicit host and dry number densities')

  call catchem_dry_air_number_density(1.0_c_double,density,status)
  call require(status == CATCHEM_CONVERSION_OK .and. density > 2.0e19_c_double, &
       'dry-air number density uses SI density and kg/mol')
  call catchem_dry_ppmv_to_number_density(40.0_c_double,CATCHEM_AMOUNT_MOLE_FRACTION,density,ozone,status)
  call require(status == CATCHEM_CONVERSION_OK .and. abs(ozone-40.0e-6_c_double*density) < eps*density, &
       'dry ozone ppmv converts to molecules per cubic centimetre')

  ! Equal solver and physical dry layer masses produce an identity conversion.
  call catchem_dry_to_solver_mass_ratio(1.0e-6_c_double,980.665_c_double,9.80665_c_double, &
       1.0_c_double,100.0_c_double,value,status)
  call require(status == CATCHEM_CONVERSION_OK .and. abs(value-1.0e-6_c_double) < eps, &
       'dry ratio converts to solver ratio when layer masses match')
  call catchem_solver_to_dry_mass_ratio(value,980.665_c_double,9.80665_c_double, &
       1.0_c_double,100.0_c_double,reverse,status)
  call require(status == CATCHEM_CONVERSION_OK .and. abs(reverse-1.0e-6_c_double) < eps, &
       'solver ratio converts back to dry ratio')

  call catchem_dry_ppmv_to_gas_mass_ratio(ieee_value(value,ieee_positive_inf), &
       CATCHEM_AMOUNT_MASS_RATIO,0.064_c_double,reverse,status)
  call require(status == CATCHEM_CONVERSION_INVALID, 'nonfinite amount is rejected')
  call catchem_dry_ppmv_to_gas_mass_ratio(1.0_c_double,CATCHEM_AMOUNT_NUMBER_CONCENTRATION, &
       0.064_c_double,reverse,status)
  call require(status == CATCHEM_CONVERSION_INVALID, 'unsupported number concentration is rejected')
  print *, 'PASS test_ccpp_conversion'
contains
  !> \\brief Fails the test program with the supplied assertion message.
  !> \\param condition True when the assertion passes.
  !> \\param message Description printed when the assertion fails.
  subroutine require(condition,message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message
    if (.not. condition) then
      print *, 'FAIL: ', trim(message)
      stop 1
    end if
  end subroutine
end program test_ccpp_conversion

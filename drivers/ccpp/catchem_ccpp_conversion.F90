! Checked physical conversions between the canonical host representation and
! CATChem's dry-air process representations.
module catchem_ccpp_conversion
   use iso_c_binding, only: c_double, c_int
   use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
   implicit none
   private
   integer(c_int), parameter, public :: CATCHEM_CONVERSION_OK = 0_c_int
   integer(c_int), parameter, public :: CATCHEM_CONVERSION_INVALID = 1_c_int
   integer(c_int), parameter, public :: CATCHEM_AMOUNT_MOLE_FRACTION = 1_c_int
   integer(c_int), parameter, public :: CATCHEM_AMOUNT_MASS_RATIO = 2_c_int
   integer(c_int), parameter, public :: CATCHEM_AMOUNT_NUMBER_CONCENTRATION = 3_c_int
   integer(c_int), parameter, public :: CATCHEM_AIR_DRY = 1_c_int
   integer(c_int), parameter, public :: CATCHEM_AIR_MOIST = 2_c_int
   integer(c_int), parameter, public :: CATCHEM_AIR_TOTAL_CONDENSATE = 3_c_int
   integer(c_int), parameter, public :: CATCHEM_CLASS_GAS = 1_c_int
   integer(c_int), parameter, public :: CATCHEM_CLASS_AEROSOL = 2_c_int
   real(c_double), parameter :: M_DRY_AIR_KG_MOL = 0.0289647_c_double
   real(c_double), parameter :: AVOGADRO_MOL_INV = 6.02214076e23_c_double
   real(c_double), parameter :: R_DRY_AIR_J_KG_K = 287.05_c_double
   real(c_double), parameter :: R_WATER_VAPOR_J_KG_K = 461.5_c_double
   real(c_double), parameter :: EPSILON_DRY_AIR = R_DRY_AIR_J_KG_K/R_WATER_VAPOR_J_KG_K

   public :: catchem_gas_mass_ratio_to_dry_ppmv, catchem_dry_ppmv_to_gas_mass_ratio
   public :: catchem_aerosol_mass_ratio_to_dry_ug_kg, catchem_host_mass_ratio_to_dry
   public :: catchem_host_ppmv_to_dry, catchem_dry_ppmv_to_number_density
   public :: catchem_dry_air_number_density, catchem_dry_to_solver_mass_ratio
   public :: catchem_solver_to_dry_mass_ratio
   public :: catchem_gfs_density_state

contains

   !> Compute moist and dry air densities and number densities from GFS layer state.
   !> param pressure Pa layer pressure.
   !> param temperature K layer temperature.
   !> param specific_humidity Water-vapor mass fraction of moist air.
   !> param moist_density Total moist-air density in kg m-3.
   !> param dry_density Dry-air density in kg m-3.
   !> param moist_number_density Total-air number density in molecules cm-3.
   !> param dry_number_density Dry-air number density in molecules cm-3.
   !> param status Conversion status, zero only when every input is physical.
   pure subroutine catchem_gfs_density_state(pressure, temperature, specific_humidity, moist_density, &
                                             dry_density, moist_number_density, dry_number_density, status)
      real(c_double), intent(in) :: pressure(:, :), temperature(:, :), specific_humidity(:, :)
      real(c_double), intent(out) :: moist_density(:, :), dry_density(:, :)
      real(c_double), intent(out) :: moist_number_density(:, :), dry_number_density(:, :)
      integer(c_int), intent(out) :: status
      real(c_double) :: vapor_pressure, dry_pressure, vapor_density, dry_number
      integer :: col, lev

      status = CATCHEM_CONVERSION_INVALID
      if (any(shape(temperature) /= shape(pressure)) .or. &
          any(shape(specific_humidity) /= shape(pressure)) .or. &
          any(shape(moist_density) /= shape(pressure)) .or. any(shape(dry_density) /= shape(pressure)) .or. &
          any(shape(moist_number_density) /= shape(pressure)) .or. &
          any(shape(dry_number_density) /= shape(pressure))) return
      do lev = 1, size(pressure, 2)
         do col = 1, size(pressure, 1)
            if (.not. positive_finite(pressure(col, lev)) .or. .not. positive_finite(temperature(col, lev))) return
            if (.not. ieee_is_finite(specific_humidity(col, lev))) return
            if (specific_humidity(col, lev) < 0.0_c_double .or. specific_humidity(col, lev) >= 1.0_c_double) return
            vapor_pressure = specific_humidity(col, lev)*pressure(col, lev)/ &
                             (EPSILON_DRY_AIR + (1.0_c_double - EPSILON_DRY_AIR)*specific_humidity(col, lev))
            dry_pressure = pressure(col, lev) - vapor_pressure
            if (.not. positive_finite(dry_pressure)) return
            dry_density(col, lev) = dry_pressure/(R_DRY_AIR_J_KG_K*temperature(col, lev))
            vapor_density = vapor_pressure/(R_WATER_VAPOR_J_KG_K*temperature(col, lev))
            moist_density(col, lev) = dry_density(col, lev) + vapor_density
            dry_number = dry_density(col, lev)/M_DRY_AIR_KG_MOL*AVOGADRO_MOL_INV/1.0e6_c_double
            dry_number_density(col, lev) = dry_number
            moist_number_density(col, lev) = dry_number*pressure(col, lev)/dry_pressure
            if (.not. positive_finite(moist_density(col, lev)) .or. .not. positive_finite(dry_density(col, lev)) .or. &
                .not. positive_finite(moist_number_density(col, lev)) .or. &
                .not. positive_finite(dry_number_density(col, lev))) return
         end do
      end do
      status = CATCHEM_CONVERSION_OK
   end subroutine catchem_gfs_density_state

   !> \brief Implements positive finite for the CATChem CCPP boundary.
   !> \param value Argument used by positive_finite.
   pure logical function positive_finite(value)
      real(c_double), intent(in) :: value
      positive_finite = .false.
      if (.not. ieee_is_finite(value)) return
      positive_finite = value > 0.0_c_double
   end function

   !> \brief Implements catchem host mass ratio to dry for the CATChem CCPP boundary.
   !> \param host_ratio Argument used by catchem_host_mass_ratio_to_dry.
   !> \param air_basis Argument used by catchem_host_mass_ratio_to_dry.
   !> \param host_density Argument used by catchem_host_mass_ratio_to_dry.
   !> \param dry_density Argument used by catchem_host_mass_ratio_to_dry.
   !> \param dry_ratio Argument used by catchem_host_mass_ratio_to_dry.
   !> \param status Argument used by catchem_host_mass_ratio_to_dry.
   pure subroutine catchem_host_mass_ratio_to_dry(host_ratio, air_basis, host_density, dry_density, dry_ratio, status)
      real(c_double), intent(in) :: host_ratio, host_density, dry_density
      integer(c_int), intent(in) :: air_basis
      real(c_double), intent(out) :: dry_ratio
      integer(c_int), intent(out) :: status
      dry_ratio = 0.0_c_double
      status = CATCHEM_CONVERSION_INVALID
      if (.not. ieee_is_finite(host_ratio)) return
      if (host_ratio < 0.0_c_double) return
      select case (air_basis)
      case (CATCHEM_AIR_DRY)
         dry_ratio = host_ratio
      case (CATCHEM_AIR_MOIST, CATCHEM_AIR_TOTAL_CONDENSATE)
         if (.not. positive_finite(host_density) .or. .not. positive_finite(dry_density)) return
         dry_ratio = host_ratio*host_density/dry_density
      case default
         return
      end select
      if (.not. ieee_is_finite(dry_ratio)) then
         dry_ratio = 0.0_c_double
         return
      end if
      status = CATCHEM_CONVERSION_OK
   end subroutine

   !> \brief Implements catchem gas mass ratio to dry ppmv for the CATChem CCPP boundary.
   !> \param host_ratio Argument used by catchem_gas_mass_ratio_to_dry_ppmv.
   !> \param amount_type Argument used by catchem_gas_mass_ratio_to_dry_ppmv.
   !> \param air_basis Argument used by catchem_gas_mass_ratio_to_dry_ppmv.
   !> \param molecular_weight_kg_mol Argument used by catchem_gas_mass_ratio_to_dry_ppmv.
   pure subroutine catchem_gas_mass_ratio_to_dry_ppmv(host_ratio, amount_type, air_basis, molecular_weight_kg_mol, &
                                                      host_density, dry_density, dry_ppmv, status)
      real(c_double), intent(in) :: host_ratio, molecular_weight_kg_mol, host_density, dry_density
      integer(c_int), intent(in) :: amount_type, air_basis
      real(c_double), intent(out) :: dry_ppmv
      integer(c_int), intent(out) :: status
      real(c_double) :: dry_ratio
      dry_ppmv = 0.0_c_double
      status = CATCHEM_CONVERSION_INVALID
      if (amount_type /= CATCHEM_AMOUNT_MASS_RATIO .or. .not. positive_finite(molecular_weight_kg_mol)) return
      call catchem_host_mass_ratio_to_dry(host_ratio, air_basis, host_density, dry_density, dry_ratio, status)
      if (status /= CATCHEM_CONVERSION_OK) return
      dry_ppmv = dry_ratio*M_DRY_AIR_KG_MOL/molecular_weight_kg_mol*1.0e6_c_double
      if (.not. ieee_is_finite(dry_ppmv)) then
         dry_ppmv = 0.0_c_double
         status = CATCHEM_CONVERSION_INVALID
      end if
   end subroutine

   !> \brief Implements catchem dry ppmv to gas mass ratio for the CATChem CCPP boundary.
   !> \param dry_ppmv Argument used by catchem_dry_ppmv_to_gas_mass_ratio.
   !> \param amount_type Argument used by catchem_dry_ppmv_to_gas_mass_ratio.
   !> \param molecular_weight_kg_mol Argument used by catchem_dry_ppmv_to_gas_mass_ratio.
   pure subroutine catchem_dry_ppmv_to_gas_mass_ratio(dry_ppmv, amount_type, molecular_weight_kg_mol, &
                                                      dry_ratio, status)
      real(c_double), intent(in) :: dry_ppmv, molecular_weight_kg_mol
      integer(c_int), intent(in) :: amount_type
      real(c_double), intent(out) :: dry_ratio
      integer(c_int), intent(out) :: status
      dry_ratio = 0.0_c_double
      status = CATCHEM_CONVERSION_INVALID
      if (amount_type /= CATCHEM_AMOUNT_MASS_RATIO) return
      if (.not. ieee_is_finite(dry_ppmv)) return
      if (dry_ppmv < 0.0_c_double) return
      if (.not. positive_finite(molecular_weight_kg_mol)) return
      dry_ratio = dry_ppmv*1.0e-6_c_double*molecular_weight_kg_mol/M_DRY_AIR_KG_MOL
      if (.not. ieee_is_finite(dry_ratio)) then
         dry_ratio = 0.0_c_double
         return
      end if
      status = CATCHEM_CONVERSION_OK
   end subroutine

   !> \brief Implements catchem aerosol mass ratio to dry ug kg for the CATChem CCPP boundary.
   !> \param host_ratio Argument used by catchem_aerosol_mass_ratio_to_dry_ug_kg.
   !> \param amount_type Argument used by catchem_aerosol_mass_ratio_to_dry_ug_kg.
   !> \param air_basis Argument used by catchem_aerosol_mass_ratio_to_dry_ug_kg.
   pure subroutine catchem_aerosol_mass_ratio_to_dry_ug_kg(host_ratio, amount_type, air_basis, &
                                                           host_density, dry_density, dry_ug_kg, status)
      real(c_double), intent(in) :: host_ratio, host_density, dry_density
      integer(c_int), intent(in) :: amount_type, air_basis
      real(c_double), intent(out) :: dry_ug_kg
      integer(c_int), intent(out) :: status
      real(c_double) :: dry_ratio
      dry_ug_kg = 0.0_c_double
      status = CATCHEM_CONVERSION_INVALID
      if (amount_type /= CATCHEM_AMOUNT_MASS_RATIO) return
      call catchem_host_mass_ratio_to_dry(host_ratio, air_basis, host_density, dry_density, dry_ratio, status)
      if (status /= CATCHEM_CONVERSION_OK) return
      dry_ug_kg = dry_ratio*1.0e9_c_double
      if (.not. ieee_is_finite(dry_ug_kg)) then
         dry_ug_kg = 0.0_c_double
         status = CATCHEM_CONVERSION_INVALID
      end if
   end subroutine

   !> \brief Implements catchem host ppmv to dry for the CATChem CCPP boundary.
   !> \param host_ppmv Argument used by catchem_host_ppmv_to_dry.
   !> \param amount_type Argument used by catchem_host_ppmv_to_dry.
   !> \param air_basis Argument used by catchem_host_ppmv_to_dry.
   !> \param host_number_density Argument used by catchem_host_ppmv_to_dry.
   pure subroutine catchem_host_ppmv_to_dry(host_ppmv, amount_type, air_basis, host_number_density, &
                                            dry_number_density, dry_ppmv, status)
      real(c_double), intent(in) :: host_ppmv, host_number_density, dry_number_density
      integer(c_int), intent(in) :: amount_type, air_basis
      real(c_double), intent(out) :: dry_ppmv
      integer(c_int), intent(out) :: status
      dry_ppmv = 0.0_c_double
      status = CATCHEM_CONVERSION_INVALID
      if (amount_type /= CATCHEM_AMOUNT_MOLE_FRACTION) return
      if (.not. ieee_is_finite(host_ppmv)) return
      if (host_ppmv < 0.0_c_double) return
      select case (air_basis)
      case (CATCHEM_AIR_DRY)
         dry_ppmv = host_ppmv
      case (CATCHEM_AIR_MOIST, CATCHEM_AIR_TOTAL_CONDENSATE)
         if (.not. positive_finite(host_number_density) .or. .not. positive_finite(dry_number_density)) return
         dry_ppmv = host_ppmv*host_number_density/dry_number_density
      case default
         return
      end select
      if (.not. ieee_is_finite(dry_ppmv)) then
         dry_ppmv = 0.0_c_double
         return
      end if
      status = CATCHEM_CONVERSION_OK
   end subroutine

   !> \brief Implements catchem dry air number density for the CATChem CCPP boundary.
   !> \param dry_density_kg_m3 Argument used by catchem_dry_air_number_density.
   !> \param number_density_cm3 Argument used by catchem_dry_air_number_density.
   !> \param status Argument used by catchem_dry_air_number_density.
   pure subroutine catchem_dry_air_number_density(dry_density_kg_m3, number_density_cm3, status)
      real(c_double), intent(in) :: dry_density_kg_m3
      real(c_double), intent(out) :: number_density_cm3
      integer(c_int), intent(out) :: status
      number_density_cm3 = 0.0_c_double
      status = CATCHEM_CONVERSION_INVALID
      if (.not. positive_finite(dry_density_kg_m3)) return
      number_density_cm3 = dry_density_kg_m3/M_DRY_AIR_KG_MOL*AVOGADRO_MOL_INV*1.0e-6_c_double
      if (.not. positive_finite(number_density_cm3)) then
         number_density_cm3 = 0.0_c_double
         return
      end if
      status = CATCHEM_CONVERSION_OK
   end subroutine

   !> \brief Implements catchem dry ppmv to number density for the CATChem CCPP boundary.
   !> \param dry_ppmv Argument used by catchem_dry_ppmv_to_number_density.
   !> \param amount_type Argument used by catchem_dry_ppmv_to_number_density.
   !> \param dry_air_number_density_cm3 Argument used by catchem_dry_ppmv_to_number_density.
   pure subroutine catchem_dry_ppmv_to_number_density(dry_ppmv, amount_type, dry_air_number_density_cm3, &
                                                      number_density_cm3, status)
      real(c_double), intent(in) :: dry_ppmv, dry_air_number_density_cm3
      integer(c_int), intent(in) :: amount_type
      real(c_double), intent(out) :: number_density_cm3
      integer(c_int), intent(out) :: status
      number_density_cm3 = 0.0_c_double
      status = CATCHEM_CONVERSION_INVALID
      if (amount_type /= CATCHEM_AMOUNT_MOLE_FRACTION) return
      if (.not. ieee_is_finite(dry_ppmv)) return
      if (dry_ppmv < 0.0_c_double) return
      if (.not. positive_finite(dry_air_number_density_cm3)) return
      number_density_cm3 = dry_ppmv*1.0e-6_c_double*dry_air_number_density_cm3
      if (.not. ieee_is_finite(number_density_cm3)) then
         number_density_cm3 = 0.0_c_double
         return
      end if
      status = CATCHEM_CONVERSION_OK
   end subroutine

   ! Convert a canonical dry-air mass ratio to the private solver denominator
   ! m_solver = DELP/g, using the corresponding dry-air mass in the layer.
   !> \brief Implements catchem dry to solver mass ratio for the CATChem CCPP boundary.
   !> \param dry_ratio Argument used by catchem_dry_to_solver_mass_ratio.
   !> \param delta_pressure_pa Argument used by catchem_dry_to_solver_mass_ratio.
   !> \param gravity_m_s2 Argument used by catchem_dry_to_solver_mass_ratio.
   pure subroutine catchem_dry_to_solver_mass_ratio(dry_ratio, delta_pressure_pa, gravity_m_s2, &
                                                    dry_density_kg_m3, layer_thickness_m, solver_ratio, status)
      real(c_double), intent(in) :: dry_ratio, delta_pressure_pa, gravity_m_s2
      real(c_double), intent(in) :: dry_density_kg_m3, layer_thickness_m
      real(c_double), intent(out) :: solver_ratio
      integer(c_int), intent(out) :: status
      real(c_double) :: solver_mass, dry_mass
      solver_ratio = 0.0_c_double
      status = CATCHEM_CONVERSION_INVALID
      if (.not. ieee_is_finite(dry_ratio)) return
      if (dry_ratio < 0.0_c_double) return
      if (.not. positive_finite(delta_pressure_pa) .or. .not. positive_finite(gravity_m_s2) .or. &
          .not. positive_finite(dry_density_kg_m3) .or. .not. positive_finite(layer_thickness_m)) return
      solver_mass = delta_pressure_pa/gravity_m_s2
      dry_mass = dry_density_kg_m3*layer_thickness_m
      if (.not. positive_finite(solver_mass) .or. .not. positive_finite(dry_mass)) return
      if (dry_mass > solver_mass*(1.0_c_double + 32.0_c_double*epsilon(1.0_c_double))) return
      solver_ratio = dry_ratio*dry_mass/solver_mass
      if (.not. ieee_is_finite(solver_ratio)) then
         solver_ratio = 0.0_c_double
         return
      end if
      status = CATCHEM_CONVERSION_OK
   end subroutine

   !> \brief Implements catchem solver to dry mass ratio for the CATChem CCPP boundary.
   !> \param solver_ratio Argument used by catchem_solver_to_dry_mass_ratio.
   !> \param delta_pressure_pa Argument used by catchem_solver_to_dry_mass_ratio.
   !> \param gravity_m_s2 Argument used by catchem_solver_to_dry_mass_ratio.
   pure subroutine catchem_solver_to_dry_mass_ratio(solver_ratio, delta_pressure_pa, gravity_m_s2, &
                                                    dry_density_kg_m3, layer_thickness_m, dry_ratio, status)
      real(c_double), intent(in) :: solver_ratio, delta_pressure_pa, gravity_m_s2
      real(c_double), intent(in) :: dry_density_kg_m3, layer_thickness_m
      real(c_double), intent(out) :: dry_ratio
      integer(c_int), intent(out) :: status
      real(c_double) :: solver_mass, dry_mass
      dry_ratio = 0.0_c_double
      status = CATCHEM_CONVERSION_INVALID
      if (.not. ieee_is_finite(solver_ratio)) return
      if (solver_ratio < 0.0_c_double) return
      if (.not. positive_finite(delta_pressure_pa) .or. .not. positive_finite(gravity_m_s2) .or. &
          .not. positive_finite(dry_density_kg_m3) .or. .not. positive_finite(layer_thickness_m)) return
      solver_mass = delta_pressure_pa/gravity_m_s2
      dry_mass = dry_density_kg_m3*layer_thickness_m
      if (.not. positive_finite(solver_mass) .or. .not. positive_finite(dry_mass)) return
      if (dry_mass > solver_mass*(1.0_c_double + 32.0_c_double*epsilon(1.0_c_double))) return
      dry_ratio = solver_ratio*solver_mass/dry_mass
      if (.not. ieee_is_finite(dry_ratio)) then
         dry_ratio = 0.0_c_double
         return
      end if
      status = CATCHEM_CONVERSION_OK
   end subroutine

end module catchem_ccpp_conversion

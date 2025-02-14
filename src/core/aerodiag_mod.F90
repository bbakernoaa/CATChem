!>
!! \file aerodiag_mod.F90
!! \brief This file contains the module for catchem aerosol diagnostics
!!
!! \ingroup core_modules
!!
!! This file contains the module for catchem aerosol diagnostics
!!
!!!>

module aerodiag_mod

   use precision_mod
   implicit none

   !> \brief Module for catchem aerosol diagnostics
   !!
   !! This module contains subroutines and functions related to the catchem aerosol diagnostics
   !!
   !! \ingroup core_modules
   !!
   !! \param Config The input config object.
   !! \param Species The Species object to be initialized.
   !! \param RC The return code.
   !!
   !!!>
   type, public :: AeroDiagsStateType

      ! Create aerosol diagnostics 
      real(kind=fp) :: aerosol_diagnostics ! turn on aerosol diagnostics?
      real(kind=fp) :: mass_diagnostics    ! turn on mass diagnostics?
      real(kind=fp) :: optical_diagnostics ! turn on optical diagnostics?
      real(kind=fp) :: sfc_mass    ! sfc mass concentration (kg/m3)
      real(kind=fp) :: col_mass    ! col mass density (kg/m2)
      real(kind=fp) :: mass       ! 3d mass mixing ratio (kg/kg)
      real(kind=fp) :: conc       ! 3d mass concentration (kg/m3)
      real(kind=fp) :: extinction_aod     ! extinction AOD at 550 nm     (m)
      real(kind=fp) :: strat_extinction_aod   ! stratospheric ext. AOD at 550 nm (m)
      real(kind=fp) :: scattering_aod     ! scattering AOD at 550 nm  (m)
      real(kind=fp) :: strat_scattering_aod   ! stratospheric sct. AOD at 550 nm (m)
      real(kind=fp) :: sfc_mass_pm25  ! surface mass concentration of PM2.5 (kg/m3)
      real(kind=fp) :: column_mass_pm25  ! column mass density of PM2.5 (kg/m2)
      real(kind=fp) :: mass_pm25     ! 3d mass mixing ratio of PM2.5 (kg/kg)
      real(kind=fp) :: extinction_aod_pm25 ! ext. AOD at 550 nm (m)
      real(kind=fp) :: scattering_aod_pm25 ! sct. AOD at 550 nm (m)
      real(kind=fp) :: aerosol_index       ! TOMS UV Aerosol Index 
      real(kind=fp) :: column_flux_u       ! column mass flux in E-W direction (units?)
      real(kind=fp) :: column_flux_v       ! column mass flux in N-S direction (units?)
      real(kind=fp) :: extinction_coef    ! extinction coefficient (1/m)
      real(kind=fp) :: scattering_coef    ! scattering coefficient (1/m)
      real(kind=fp) :: backscatter_coef    ! backscatter coefficient (m-1 sr-1)
      real(kind=fp) :: extinction_aod_finemode   ! fine mode (sub micron) extinction AOD at 550 nm
      real(kind=fp) :: scattering_aod_finemode   ! fine mode (sub micron) scattering AOD at 550 nm
      real(kind=fp) :: angstrom_parameter   ! 470-870 nm Angstrom parameter


   end type AeroDiagsStateType


contains

   subroutine init(AeroDiagsState)


      type(AeroDiagsStateType), intent(inout) :: AeroDiagsState
      AeroDiagsState%aerosol_diagnostics = 
      AeroDiagsState%mass_diagnostics = 
      AeroDiagsState%optical_diagnostics = 
      AeroDiagsState%sfc_mass = 
      AeroDiagsState%col_mass = 
      AeroDiagsState%mass = 
      AeroDiagsState%conc = 
      AeroDiagsState%extinction_aod = 
      AeroDiagsState%strat_extinction_aod = 


   end subroutine init



end module aerodiag_mod




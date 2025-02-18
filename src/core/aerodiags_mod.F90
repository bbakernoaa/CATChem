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
      real(kind=fp), allocatable :: aerosol_diagnostics ! turn on aerosol diagnostics?
      real(kind=fp), allocatable :: mass_diagnostics    ! turn on mass diagnostics?
      real(kind=fp), allocatable :: optical_diagnostics ! turn on optical diagnostics?
      real(kind=fp), allocatable :: sfc_mass    ! sfc mass concentration (kg/m3)
      real(kind=fp), allocatable :: col_mass    ! col mass density (kg/m2)
      real(kind=fp), allocatable :: mass       ! 3d mass mixing ratio (kg/kg)
      real(kind=fp), allocatable :: conc       ! 3d mass concentration (kg/m3)
      real(kind=fp), allocatable :: extinction_aod     ! extinction AOD at 550 nm     (m)
      real(kind=fp), allocatable :: strat_extinction_aod   ! stratospheric ext. AOD at 550 nm (m)
      real(kind=fp), allocatable :: scattering_aod     ! scattering AOD at 550 nm  (m)
      real(kind=fp), allocatable :: strat_scattering_aod   ! stratospheric sct. AOD at 550 nm (m)
      real(kind=fp), allocatable :: sfc_mass_pm25  ! surface mass concentration of PM2.5 (kg/m3)
      real(kind=fp), allocatable :: column_mass_pm25  ! column mass density of PM2.5 (kg/m2)
      real(kind=fp), allocatable :: mass_pm25     ! 3d mass mixing ratio of PM2.5 (kg/kg)
      real(kind=fp), allocatable :: extinction_aod_pm25 ! ext. AOD at 550 nm (m)
      real(kind=fp), allocatable :: scattering_aod_pm25 ! sct. AOD at 550 nm (m)
      real(kind=fp), allocatable :: aerosol_index       ! TOMS UV Aerosol Index 
      real(kind=fp), allocatable :: column_flux_u       ! column mass flux in E-W direction (units?)
      real(kind=fp), allocatable :: column_flux_v       ! column mass flux in N-S direction (units?)
      real(kind=fp), allocatable :: extinction_coef    ! extinction coefficient (1/m)
      real(kind=fp), allocatable :: scattering_coef    ! scattering coefficient (1/m)
      real(kind=fp), allocatable :: backscatter_coef    ! backscatter coefficient (m-1 sr-1)
      real(kind=fp), allocatable :: extinction_aod_finemode   ! fine mode (sub micron) extinction AOD at 550 nm
      real(kind=fp), allocatable :: scattering_aod_finemode   ! fine mode (sub micron) scattering AOD at 550 nm
      real(kind=fp), allocatable :: angstrom_parameter   ! 470-870 nm Angstrom parameter


   end type AeroDiagsStateType


contains

   subroutine init(AeroDiagsState)


      type(AeroDiagsStateType), intent(inout) :: AeroDiagsState

      AeroDiagsState%aerosol_diagnostics
      AeroDiagsState%mass_diagnostics
      AeroDiagsState%optical_diagnostics
      AeroDiagsState%sfc_mass
      AeroDiagsState%col_mass
      AeroDiagsState%mass
      AeroDiagsState%conc
      AeroDiagsState%extinction_aod
      AeroDiagsState%strat_extinction_aod
      AeroDiagsState%scattering_aod
      AeroDiagsState%strat_scattering_aod
      AeroDiagsState%sfc_mass_pm25
      AeroDiagsState%column_mass_pm25
      AeroDiagsState%mass_pm25
      AeroDiagsState%extinction_aod_pm25
      AeroDiagsState%scattering_aod_pm25
      AeroDiagsState%aerosol_index
      AeroDiagsState%column_flux_u
      AeroDiagsState%column_flux_v
      AeroDiagsState%extinction_coef
      AeroDiagsState%scattering_coef
      AeroDiagsState%backscatter_coef
      AeroDiagsState%extinction_aod_finemode
      AeroDiagsState%scattering_aod_finemode
      AeroDiagsState%angstrom_parameter


   end subroutine init



end module aerodiag_mod




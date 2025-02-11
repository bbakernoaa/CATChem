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
   type, public :: AeroDiagsType

      ! Names
      character(len=30) :: long_name  !< long name for aerosol diagnostics used for netcdf attribute "long_name"
      character(len=30) :: short_name !< short name for aerosol diagnostics
      character(len=50) :: description !< description of aerosol diagnostics

      ! Logcial switches
      logical :: is_gocart_aero       !< if true, species is a GOCART aerosol species
      logical :: is_dust              !< if true, species is a dust
      logical :: is_dms               !< if true, species is DMS

      ! Numerical properties
      real(kind=fp) :: mw_g                 !< gaseous molecular weight
      real(kind=fp) :: density              !< particle density (kg/m3)
      real(kind=fp) :: radius               !< mean molecular diameter in meters
      real(kind=fp) :: lower_radius         !< lower radius in meters
      real(kind=fp) :: upper_radius         !< upper radius in meters
      real(kind=fp) :: viscosity            !< kinematic viscosity (m2/s)


      ! Default background concentration
      real(kind=fp) :: BackgroundVV        !< Background conc [v/v]

      ! Indices
      integer :: gocart_aero_index    !< gocart_aero index in gocart_aero array

      ! Concentration
      real(kind=fp), ALLOCATABLE :: conc(:)             !< species concentration [v/v] or kg/kg

   end type AeroDiagsType

   !
   ! !DEFINED PARAMETERS:
   !
   !=========================================================================
   ! Missing species concentration value if not in restart file and special
   ! background value not defined
   !=========================================================================
   REAL(fp), PARAMETER, PUBLIC :: MISSING_VV  = 1.0e-20_fp ! Missing spc conc

contains

   subroutine init(AeroDiags_State)
      type(SpeciesType), intent(inout) :: AeroDiags_State
      character(len=*), intent(in) :: species_name
      integer, intent(in) :: atomic_num

      AeroDiags_State%short_name = species_name
      AeroDiags_State%mw_g = atomic_num
   end subroutine init



end module aerodiag_mod




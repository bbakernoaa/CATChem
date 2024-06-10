!>
!! \file species_mod.F90
!! \brief This file contains the module for catchem species
!!
!! This file contains the module for catchem species
!!
!!!>

module species_mod

   use precision_mod
   implicit none

   type :: SpeciesType
      private

      ! Names
      character(len=30) :: long_name  !< long name for species used for netcdf attribute "long_name"
      character(len=30) :: short_name !< short name for species

      ! Logcial switches
      logical :: is_gas               !< if true, species is a gas and not an aerosol
      logical :: is_aero              !< if true, species is aerosol and not a gas
      logical :: is_tracer            !< if true, species is a tracer and not an aerosol or gas that undergoes chemistry or photolysis
      logical :: is_advected          !< if true, species is advected
      logical :: is_drydep            !< if true, species undergoes dry depotiion
      logical :: is_photolysis        !< if true, species undergoes photolysis
      logical :: is_gocart_aero       !< if true, species is a GOCART aerosol species

      ! Numerical properties
      integer :: mw_g                 !< gaseos molecular weight
      integer :: density              !< particle density (kg/m3)
      integer :: radius               !< mean molecular diameter in meters
      integer :: lower_radius         !< lower radius in meters
      integer :: upper_radius         !< upper radius in meters
      integer :: viscosity            !< kinematic viscosity (m2/s)


      ! Default background concentration
      real(kind=fp) :: BackgroundVV        !< Background conc [v/v]

      ! Indices
      integer :: species_index        !< species index in species array
      integer :: drydep_index         !< drydep index in drydep array
      integer :: photolysis_index     !< photolysis index in photolysis array
      integer :: gocart_aero_index    !< gocart_aero index in gocart_aero array
   end type SpeciesType



  !=========================================================================
  ! Type for the Species Database object (vector of type Species)
  !=========================================================================
!  TYPE, PUBLIC :: SpcPtr
!     TYPE(Species), POINTER :: Info         ! Single entry of Species Database
!  END TYPE SpcPtr



   !
   ! !DEFINED PARAMETERS:
   !
   !=========================================================================
   ! Missing species concentration value if not in restart file and special
   ! background value not defined
   !=========================================================================
   REAL(fp), PARAMETER, PUBLIC :: MISSING_VV  = 1.0e-20_fp ! Missing spc conc

contains

   subroutine init(Species_State, species_name, atomic_num)
      type(SpeciesType), intent(inout) :: Species_State
      character(len=*), intent(in) :: species_name
      integer, intent(in) :: atomic_num

      Species_State%short_name = species_name
      Species_State%mw_g = atomic_num
   end subroutine init

   ! function get_name(this) result(species_name)
   !    character(len=30) :: species_name

   !    species_name = this%short_name
   ! end function get_name

   ! function get_atomic_number(this) result(atomic_num)
   !    class(Species), intent(in) :: this
   !    integer :: atomic_num

   !    atomic_num = this%atomic_number
   ! end function get_atomic_number

end module species_mod

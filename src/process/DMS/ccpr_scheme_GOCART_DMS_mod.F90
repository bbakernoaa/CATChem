!>
!! \file
!! \brief CCPr Scheme for DMS
!!
!!
!! Reference: Benchmarking GOCART-2G in the Goddard Earth Observing System (GEOS)
!! Allison B. Collow, Peter R. Colarco, Arlindo M. da Silva, Virginie Buchard,
!! Huisheng Bian, M Chin, Sampa Das, Ravi Govindaraju, Dongchul Kim, and Valentina Aquila,
!! Geosci. Model Development, 17, 14431468, 2024
!! https://doi.org/10.5194/gmd-17-1443-2024
!!
!! \author Lacey Holland
!! \date 01/2025
!!!>
module CCPr_Scheme_GOCART_DMS_Mod

   implicit none

   private

   public :: CCPr_Scheme_GOCART_DMS

contains

   !> \brief Brief description of the subroutine
   !!
   !! \param MetState     Meteorological Variables
   !! \param DiagState    Diagnostic Variables
   !! \param DMSState   DMS Variables
   !! \param RC           Success or Failure
   !!
   !! Note that other state types may be required, e.g. one specific to the process group.
   !!!>
   subroutine CCPr_Scheme_GOCART_DMS(km, cdt, g0, tmpu, u10m, v10m, oro, delp, &
                                             dmso_conc, dms, SU_emis, ndms, RC)

      ! Uses
      USE GOCART2G_process, only: DMSemission

      IMPLICIT NONE

      ! Arguments
      INTEGER, intent(in)                     :: km            ! number of vertical levels
      integer, intent(in) :: ndms      ! index of DMS relative to other sulfate tracers
      real, intent(in)    :: fMassDMS  ! gram molecular weight of DMS

      REAL, intent(in)                      :: g0
      REAL, intent(in)                      :: cdt               ! model timestep [sec]
      REAL,  intent(in)               :: u10m                   ! 10-m u-wind component [m/sec]
      REAL,  intent(in)               :: v10m                   ! 10-m v-wind component [m/sec]

      REAL, intent(in),dimension(:,:),pointer  :: DMSO_CONC      ! DMS source concentration [units??]
      REAL, allocatable, intent(in), DIMENSION(:) :: tmpu   ! Temperature [K]
      REAL, allocatable, DIMENSION(:) :: delp   ! Pressure Thickness for layer [Pa]

      INTEGER, intent(in)       :: lwi                   ! orography flag; Land, ocean, ice mask

      REAL, intent(inout),dimension(:,:,:),pointer  :: DMS      ! DMS [kg kg-1]
      REAL, intent(inout),dimension(:,:,:),pointer  :: SU_emis   ! SU emissions, kg/m2/s
      REAL, parameter :: fMassDMS=62.   ! g mol-1  -  should this go somewhere else in the future??

      integer, intent(out) :: RC                      ! Success or Failure

      ! Local Variables
      character(len=256) :: errMsg
      character(len=256) :: thisLoc

      real, pointer :: GOCART_TMPU(:,:,:)
      real, pointer :: GOCART_DELP(:,:,:)
      real, pointer :: GOCART_LWI(:,:)
      real, pointer :: GOCART_U10(:,:)
      real, pointer :: GOCART_V10(:,:)


!!! BELOW IS FROM GOCART DMSemission
!! !INPUT PARAMETERS:
!   integer, intent(in) :: km  ! number model layers, and number of species respectively
!   real, intent(in)    :: cdt ! model time step [seconds]
!   real, intent(in)    :: grav ! gravity [m sec-1]
!   real, pointer, dimension(:,:,:), intent(in)  :: tmpu  ! temperature [K]
!   real, pointer, dimension(:,:), intent(in)    :: u10m  ! 10-m u-wind component [m s-1]
!   real, pointer, dimension(:,:), intent(in)    :: v10m  ! 10-m v-wind component [m s-1]
!   real, pointer, dimension(:,:), intent(in)    :: oro   ! orography flag
!   real, pointer, dimension(:,:,:), intent(in)  :: delp  ! pressure thickness [Pa]
!   real, dimension(:,:), intent(in) :: dmso_conc ! DMS source [1]
!   integer, intent(in) :: ndms      ! index of DMS relative to other sulfate tracers
!   real, intent(in)    :: fMassDMS  ! gram molecular weight of DMS
!
!! !INOUT PARAMETERS:
!   real, dimension(:,:,:), intent(inout)  :: dms ! dms [kg kg-1]
!   real, pointer, dimension(:,:,:), intent(inout)  :: SU_emis   ! SU emissions, kg/m2/s


      ! Initialize
      errMsg = ''
      thisLoc = ' -> at CCPr_Scheme_GOCART_DMS (in CCPr_Scheme_GOCART_DMS_mod.F90)'
      RC = CC_SUCCESS

      !------------------
      ! Begin Scheme Code
      !------------------

      ! Begin GOCART Code
      ! GOCART Options comes in from DMSState
      ! Diagnostic Variables are added through DiagState below

      ! Run the DMS Scheme
      !-------------------------
      if (DMSState%Activate) then
         ! Run the DMS Scheme
         !-------------------------
         if (DMSState%SchemeOpt == 1) then
            ! Run the DMS Scheme
            !-------------------------

            call DMSemission (km, cdt, g0, tmpu, u10m, v10m, oro, delp, &
                           fMassDMS, dmso_conc, dms, SU_emis, ndms, rc)

         endif

      endif

      if (associated(GOCART_TMPU)) nullify(GOCART_TMPU)
      if (associated(GOCART_DELP)) nullify(GOCART_DELP)
      if (associated(GOCART_U10)) nullify(GOCART_U10)
      if (associated(GOCART_V10)) nullify(GOCART_V10)
      if (associated(GOCART_LWI)) nullify(GOCART_LWI)

   end subroutine CCPr_Scheme_GOCART_DMS


end module CCPr_Scheme_GOCART_DryDep_Mod









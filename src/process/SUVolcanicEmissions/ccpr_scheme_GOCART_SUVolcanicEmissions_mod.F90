!>
!! \file
!! \brief CCPr Scheme for Volcanic Emissions
!!
!!
!! Reference: Benchmarking GOCART-2G in the Goddard Earth Observing System (GEOS)
!! Allison B. Collow, Peter R. Colarco, Arlindo M. da Silva, Virginie Buchard,
!! Huisheng Bian, M Chin, Sampa Das, Ravi Govindaraju, Dongchul Kim, and Valentina Aquila,
!! Geosci. Model Development, 17, 14431468, 2024
!! https://doi.org/10.5194/gmd-17-1443-2024
!!
!! \author Lacey Holland
!! \date 07/2024
!!!>
module CCPr_Scheme_GOCART_SUVolcanicEmissions_Mod

   implicit none

   private

   public :: CCPr_Scheme_GOCART_SUVolcanicEmissions

contains

   !> \brief Brief description of the subroutine
   !!
   !! \param MetState     Meteorological Variables
   !! \param DiagState    Diagnostic Variables
   !! \param EmisState    Emission State variables
   !! \param SUVolcanicEmissions  SUVolcanicEmissions Variables
   !! \param RC           Success or Failure
   !!
   !! Note that other state types may be required, e.g. one specific to the process group.
   !!!>

   ! Need to change this so that it includes only what is necessary
   !  May need to do something to connect iPoint, jPoint to vlat, vlon
   ! should only run this where we know there is a volcano????
   !subroutine CCPr_Scheme_GOCART_SUVolcanicEmissions( MetState, DiagState, EmisState, &
   !   SUVolcanicEmissionsState, &
   !   RC)
   subroutine CCPr_Scheme_GOCART_SUVolcanicEmissions(km, &
      cdt, &
      VStart, &
      VEnd, &
      nVolc, &
      iPoint, &
      jPoint, &
   !YMD, &
      HMS, &
      g0, &
      zbox, &
      delp, &
      area, &
      vSO2, &
      nSO2, &
      SO2, &
      SU_emis, &
      vCloud, &
      vElev, &
      vLat, &
      VLon, &
      RC )

      USE GOCART2G_Process, only: SUVolcanicEmissions
      USE ReadEmissions, only:  ReadASCIIPointEmissions

      IMPLICIT NONE

      ! Arguments
      INTEGER, intent(in)              :: km                ! number of vertical levels

      INTEGER, intent(inout),dimension(1)   :: vStart      ! Emissions Start time [sec]
      INTEGER, intent(inout),dimension(1)   :: vEnd        ! Emissions end time [sec]
      INTEGER, intent(inout)                :: nVolc       ! number of volcanic sources
      INTEGER, intent(inout)                :: rc          ! error code - is this inout or out???
      INTEGER, intent(inout),dimension(1)   :: iPoint, jPoint ! sub-domain - we only run this at the place/time of eruption??
      !INTEGER, intent(in)                  :: YMD
      INTEGER, intent(in)                   :: HMS    ! current model time [sec]
      INTEGER, intent(inout)                :: nSO2     ! index of SO2 relative to other sulfate tracers


      REAL, intent(in)                      :: g0
      REAL, intent(in)                      :: cdt               ! model timestep [sec]

      REAL, intent(inout),dimension(:,:)    :: area     ! area of grid cell [m^2]
      REAL, intent(inout),dimension(:)      :: vSO2   ! volcanic emissions  [kg]
      !!!!  Below can be figured out from ChemSpeciesState%nSpeciesSUVolcanicIndex???
      REAL, intent(inout),dimension(:,:,:),pointer  :: SO2       ! SO2 [kg kg-1]
      REAL, intent(inout),dimension(:,:,:),pointer  :: SU_emis   ! SU emissions, kg/m2/s
      REAL, intent(inout),dimension(:)        :: vCloud    ! top elevation of emissions [m]
      REAL, intent(inout),dimension(:)        :: vElev     ! bottom elevation of emissions [m]
      REAL, intent(inout),dimension(:)        :: vLat     ! latitude specified in file [degree]
      REAL, intent(inout),dimension(:)        :: VLon     ! longitude specified in file [degree]

      !CHARACTER                :: fname
      REAL, DIMENSION(:,:),pointer    :: SO2EMVN   ! non-explosive volcanic emissions [kg m-2 s-1]
      REAL, DIMENSION(:,:),pointer    :: SO2EMVE   ! explosive volcanic emissions [kg m-2 s-1]
      REAL, allocatable, DIMENSION(:) :: delp   ! Pressure Thickness for layer [Pa]
      REAL, allocatable, DIMENSION(:) :: zbox  ! geopotential Height difference [m] for layer

      !TYPE(MetStateType),  INTENT(IN) :: MetState       ! MetState Instance
      !TYPE(DiagStateType), INTENT(IN) :: DiagState       ! DiagState Instance
      !TYPE(SUVolcanicEmissionsStateType), INTENT(IN) :: SUVolcanicEmissionsState       ! SUVolcanicEmissionsState Instance

      ! should these be pulled from ChemSpeciesState and the species YAML?
      REAL, parameter :: fMassSulfur = 32.  !  gram molecular weights of species
      REAL, parameter :: fMassSO2 = 64.     !  gram molecular weights of species
      !REAL, parameter :: fMassSO4 = 96.     !  gram molecular weights of species
      !CHARACTER(len=7), parameter :: label='volcano'


      ! Local Variables
      real, pointer :: GOCART_ZBOX(:,:,:)
      real, pointer :: GOCART_DELP(:,:,:)

      character(len=256) :: errMsg
      character(len=256) :: thisLoc

      ! Initialize
      errMsg = ''
      thisLoc = ' -> at CCPr_Scheme_GOCART_SUVolcanicEmissions &
      & (in CCPr_Scheme_GOCART_SUVolcanicEmissions_mod.F90)'
      RC = 0

      ! transform data for GOCART SUVolcanicEmissions call

      !  Need to re-write for individual variables
      ! put into the src/core directory as a module???
      call PrepMetVarsForGOCARTSUV(km,  &
         delp,            &
         zbox,           &
         GOCART_DELP,     &
         GOCART_ZBOX)

      !------------------
      ! Begin Scheme Code
      !------------------

      !!! I don't know if we need the line below, if we are just reading in??
      vSO2 = vSO2 * fMassSO2 / fMassSulfur

!!!!!!!!!!!!!!!!!!!!! NEED TO EDIT ABOVE !!!!!!!!!!!!!!!!!!!!!
!!!! Most of the volcanic stuff, is going directly into the call below

!! Need to replace below.  Not sure if ipoint, jpoint coordinates are necessary
      if (nVolc > 0) then

         iPoint(1) = 0
         jPoint(1) = 0

         call SUvolcanicEmissions (nVolc, vStart, vEnd, vSO2, &
            vElev, vCloud, &
            iPoint, jPoint, &
            hms, SO2EMVN, SO2EMVE, SO2, nSO2, &
            SU_emis, km, cdt, g0, gocart_ZBOX, gocart_DELP, area, &
            vLat, vLon, rc)

      end if

      if (associated(GOCART_DELP)) nullify(GOCART_DELP)
      if (associated(GOCART_zbox)) nullify(GOCART_zbox)


   end subroutine CCPr_Scheme_GOCART_SUVolcanicEmissions

end module CCPr_Scheme_GOCART_SUVolcanicEmissions_Mod

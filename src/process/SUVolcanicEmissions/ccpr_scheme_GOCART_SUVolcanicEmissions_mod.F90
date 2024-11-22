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
   subroutine CCPr_Scheme_GOCART_SUVolcanicEmissions( MetState, DiagState, EmisState, &
      SUVolcanicEmissions, &
      RC)

      USE GOCART2G_Process, only: SUVolcanicEmissions
      USE ReadEmissions, only:  ReadASCIIPointEmissions

      IMPLICIT NONE

      ! Arguments
      INTEGER, intent(in)              :: km                ! number of vertical levels
      INTEGER, intent(in)              :: cdt               ! model timestep [sec]

      INTEGER, intent(inout)           :: vStart      ! Emissions Start time
      INTEGER, intent(inout)           :: vEnd        ! Emissions end time
      INTEGER, intent(inout)           :: nVolc       ! number of volcanic sources
      INTEGER, intent(inout)           :: rc          ! error code - is this inout or out???
      INTEGER, intent(in)              :: iPoint, jPoint ! sub-domain - we only run this at the place/time of eruption??
      INTEGER, intent(in)              :: YMD
      INTEGER, intent(in)              :: HMS

      REAL, allocatable, intent(in), DIMENSION(:) :: delp   ! Pressure Thickness [Pa]
      REAL, allocatable, intent(in), DIMENSION(:) :: hghte  ! top of layer geopotential Height [m]
      REAL, intent(in)                        :: g0

      REAL, intent(inout)                      :: area     ! area of grid cell [m^2]
      REAL, intent(inout)                     :: vSO2   ! volcanic emissions from file [kg]
 !!!!  Below can be figured out from ChemSpeciesState%nSpeciesSUVolcanicIndex???
      REAL, intent(inout)                     :: nSO2     ! index of SO2 relative to other sulfate tracers
      REAL, intent(inout)                     :: SO2       ! SO2 [kg kg-1]
      REAL, intent(inout)                     :: SU_emis   ! SU emissions, kg/m2/s
      REAL, intent(inout)                     :: vCloud    ! top elevation of emissions [m]
      REAL, intent(inout)                     :: vElev     ! bottom elevation of emissions [m]
      REAL, intent(inout)                     :: SO2EMVN   ! non-explosive volcanic emissions [kg m-2 s-1]
      REAL, intent(inout)                     :: SO2EMVE   ! explosive volcanic emissions [kg m-2 s-1]
      REAL, intent(inout)                      :: vLat     ! latitude specified in file [degree]
      REAL, intent(inout)                      :: VLon     ! longitude specified in file [degree]

      ! should these be pulled from ChemSpeciesState?
      REAL, parameter :: fMassSulfur = 32.     !  gram molecular weights of species 
      REAL, parameter :: fMassSO2 = 64.     !  gram molecular weights of species
      REAL, parameter :: fMassSO4 = 96.     !  gram molecular weights of species

      ! Output
      integer, intent(out) :: RC                      ! Success or Failure, success is 0

      ! Local Variables
      real, pointer :: GOCART_HGHTE(:,:,:)
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
      call PrepMetVarsForGOCARTSUV(km,     &
         tmpu,            &
         hghte,           &
         GOCART_tmpu,     &
         GOCART_HGHTE)

      !------------------
      ! Begin Scheme Code
      !------------------

!      Get pointwise SO2 and altitude of volcanoes from a daily file data base
       if(index(self%volcano_srcfilen,'volcanic_') /= 0) then
          call ReadASCIIPointEmissions (MetState%YMD, fname, nVolc, vLat, vLon, &
                                   vElev, vCloud, vSO2, vStart, &
                                   vEnd, label='volcano', __RC__)
!!! I don't know if we need the line below, if we are just reading in??
          vSO2 = vSO2 * fMassSO2 / fMassSulfur
!         Special possible case
!!! I don't know why this is here, so I commented it out for now
          if(self%volcano_srcfilen(1:9) == '/dev/null') nVolc = 0  ! option for no volcano??
       end if

!!!!!!!!!!!!!!!!!!!!! NEED TO EDIT ABOVE !!!!!!!!!!!!!!!!!!!!!
!!!! Most of the volcanic stuff, is going directly into the call below

!! Need to replace below.  Not sure if ipoint, jpoint coordinates will be necessary
    if (workspace%nVolc > 0) then
       if (associated(SO2EMVE)) SO2EMVE=0.0
       if (associated(SO2EMVN)) SO2EMVN=0.0

      iPoint = 0
      jPoint = 0

      call SUvolcanicEmissions (nVolc, vStart, vEnd, vSO2, vElev, vCloud, iPoint, &
           jPoint, nhms, SO2EMVN, SO2EMVE, SO2, SUVolcanicEmissionsState%nSpeciesSUVolcanic, &
           SU_emis, km, cdt, g0, gocart_hghte, gocart_delp, area, vLat, vLon, rc)

    end if

      if (associated(GOCART_DELP)) nullify(GOCART_DELP)
      if (associated(GOCART_TMPU)) nullify(GOCART_TMPU)
     

   end subroutine CCPr_Scheme_GOCART_SUVolcanicEmissions

end module CCPr_Scheme_GOCART_SUVolcanicEmissions


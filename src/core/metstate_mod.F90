!> \file metstate_mod.F90
!! \brief Module for meteorology state variables
!!
!! This module contains subroutines and functions related to the MetStateType instance of CATChem.
!! It includes subroutines for initializing of the MetStateType.
!!
!! \ingroup core_modules
!!!>
MODULE MetState_Mod
   !
   ! USES:
   !
   USE Cmn_Size_Mod, ONLY : NSURFTYPE
   ! USE Dictionary_M, ONLY : dictionary_t
   USE Error_Mod
   USE Precision_Mod
   ! USE Registry_Mod


   IMPLICIT NONE
   PRIVATE
   !
   ! !PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: Zero_MetState
   PUBLIC :: Met_Allocate
   !
   ! !PUBLIC DATA MEMBERS:
   !
   !=========================================================================
   ! Derived type for Meteorology State
   !=========================================================================

   !> \brief Derived type for Meteorology State
   !!
   !! \ingroup core_modules
   !!!>
   TYPE, PUBLIC :: MetStateType

      CHARACTER(LEN=3)             :: State     = 'MET'    ! Name of this state

      ! NLEVS
      !------
      INTEGER               :: nLEVS             !< Number of vertical levels
      INTEGER               :: nHORZ             !< Number of horizontal levels
      INTEGER               :: nSOIL             !< # number of soil layers
      INTEGER               :: nLNDTYPE          !< # of landtypes in box (I,J)

      ! TIMESTEP
      !---------
      REAL(fp), ALLOCATABLE :: TSTEP             !< Time step [s]

      ! Logicals
      !---------
      LOGICAL, ALLOCATABLE :: IsLand(:)           !< Is this a land grid box?
      LOGICAL, ALLOCATABLE :: IsWater(:)          !< Is this a water grid box?
      LOGICAL, ALLOCATABLE :: IsIce(:)            !< Is this a ice grid box?
      LOGICAL, ALLOCATABLE :: IsSnow(:)           !< Is this a snow grid box?
      LOGICAL, ALLOCATABLE :: InStratMeso(:,:)    !< Are we in the stratosphere or mesosphere?
      LOGICAL, ALLOCATABLE :: InStratosphere(:,:) !< Are we in the stratosphere?
      LOGICAL, ALLOCATABLE :: InTroposphere(:,:)  !< Are we in the troposphere?
      LOGICAL, ALLOCATABLE :: InPbl(:,:)          !< Are we in the PBL?
      LOGICAL, ALLOCATABLE :: IsLocalNoon(:)      !< Is it local noon (between 11 and 13 local solar time?

      ! Land Specific Fields
      !---------------------
      REAL(fp), ALLOCATABLE :: AREA_M2(:)         !< Grid box surface area [m2]
      INTEGER,  ALLOCATABLE :: LWI(:)             !< Land water ice mask (0-sea, 1-land, 2-ice)
      REAL(fp), ALLOCATABLE :: CLAYFRAC(:)        !< Fraction of clay [1]
      REAL(fp), ALLOCATABLE :: SANDFRAC(:)        !< Fraction of sand [1]
      INTEGER,  ALLOCATABLE :: DSOILTYPE(:)       !< Dominant soil type
      INTEGER,  ALLOCATABLE :: DLUSE(:)           !< Dominant land-use type
      REAL(fp), ALLOCATABLE :: FRVEG(:)           !< Fraction of veg [1]
      REAL(fp), ALLOCATABLE :: FRLAKE(:)          !< Fraction of lake [1]
      REAL(fp), ALLOCATABLE :: FRLAND(:)          !< Fraction of land [1]
      REAL(fp), ALLOCATABLE :: FRLANDICE(:)        !< Fraction of land ice [1]
      REAL(fp), ALLOCATABLE :: FROCEAN(:)         !< Fraction of ocean [1]
      REAL(fp), ALLOCATABLE :: FRSEAICE(:)        !< Sfc sea ice fraction
      REAL(fp), ALLOCATABLE :: FRSNO(:)           !< Sfc snow fraction
      REAL(fp), ALLOCATABLE :: LAI(:)             !< Leaf area index [m2/m2] (online) Dominant
      REAL(fp), ALLOCATABLE :: GVF(:)             !< Green Vegetative Fraction
      REAL(fp), ALLOCATABLE :: RDRAG(:)           !< Drag Partition [1]
      REAL(fp), ALLOCATABLE :: SEAICE00(:)        !< Sea ice coverage 00-10%
      REAL(fp), ALLOCATABLE :: SEAICE10(:)        !< Sea ice coverage 10-20%
      REAL(fp), ALLOCATABLE :: SEAICE20(:)        !< Sea ice coverage 20-30%
      REAL(fp), ALLOCATABLE :: SEAICE30(:)        !< Sea ice coverage 30-40%
      REAL(fp), ALLOCATABLE :: SEAICE40(:)        !< Sea ice coverage 40-50%
      REAL(fp), ALLOCATABLE :: SEAICE50(:)        !< Sea ice coverage 50-60%
      REAL(fp), ALLOCATABLE :: SEAICE60(:)        !< Sea ice coverage 60-70%
      REAL(fp), ALLOCATABLE :: SEAICE70(:)        !< Sea ice coverage 70-80%
      REAL(fp), ALLOCATABLE :: SEAICE80(:)        !< Sea ice coverage 80-90%
      REAL(fp), ALLOCATABLE :: SEAICE90(:)        !< Sea ice coverage 90-100%
      REAL(fp), ALLOCATABLE :: SNODP(:)           !< Snow depth [m]
      REAL(fp), ALLOCATABLE :: SNOMAS(:)          !< Snow mass [kg/m2]
      REAL(fp), ALLOCATABLE :: SSM(:)             !< Sediment Supply Map [1]
      REAL(fp), ALLOCATABLE :: USTAR_THRESHOLD(:) !< Threshold friction velocity [m/s]
      REAL(fp), ALLOCATABLE :: GWETTOP(:)         !< Top soil moisture [1]
      REAL(fp), ALLOCATABLE :: GWETROOT(:)        !< Root Zone soil moisture [1]
      REAL(fp), ALLOCATABLE :: WILT(:)            !< Wilt point [1]
      REAL(fp), ALLOCATABLE :: SOILM(:,:)         !< Volumetric Soil moisture [m3/m3]
      REAL(fp), ALLOCATABLE :: FRLANDUSE(:,:)     !< Fractional Land Use
      REAL(fp), ALLOCATABLE :: FRLAI(:,:)         !< LAI in each Fractional Land use type [m2/m2]

      ! Radiation Related Surface Fields
      !---------------------------------
      REAL(fp), ALLOCATABLE :: ALBD_VIS(:)       !< Visible surface albedo [1]
      REAL(fp), ALLOCATABLE :: ALBD_NIR(:)       !< Near-IR surface albedo [1]
      REAL(fp), ALLOCATABLE :: ALBD_UV(:)        !< UV surface albedo [1]
      REAL(fp), ALLOCATABLE :: PARDR(:)          !< Direct photsynthetically active radiation [W/m2]
      REAL(fp), ALLOCATABLE :: PARDF(:)          !< Diffuse photsynthetically active radiation [W/m2]
      REAL(fp), ALLOCATABLE :: SUNCOS(:)         !< COS(solar zenith angle) at current time
      REAL(fp), ALLOCATABLE :: SUNCOSmid(:)      !< COS(solar zenith angle) at midpoint of chem timestep
      REAL(fp), ALLOCATABLE :: SUNCOSsum(:)      !< Sum of COS(SZA) for HEMCO OH diurnal variability
      REAL(fp), ALLOCATABLE :: SZAFACT(:)        !< Diurnal scale factor for HEMCO OH diurnal variability (computed) [1]
      REAL(fp), ALLOCATABLE :: SWGDN(:)          !< Incident radiation @ ground [W/m2]



      ! Flux Related Fields
      !--------------------
      REAL(fp), ALLOCATABLE :: EFLUX(:)             !< Latent heat flux [W/m2]
      REAL(fp), ALLOCATABLE :: HFLUX(:)             !< Sensible heat flux [W/m2]
      REAL(fp), ALLOCATABLE :: U10M(:)              !< E/W wind speed @ 10m ht [m/s]
      REAL(fp), ALLOCATABLE :: USTAR(:)             !< Friction velocity [m/s]
      REAL(fp), ALLOCATABLE :: V10M(:)              !< N/S wind speed @ 10m ht [m/s]
      REAL(fp), ALLOCATABLE :: Z0(:)                !< Surface roughness height [m]
      REAL(fp), ALLOCATABLE :: z0h(:)               !< Thremal Surface roughness height [m]
      REAL(fp), ALLOCATABLE :: FRZ0(:,:)            !< Aerodynamic Roughness Length per FRLANDUSE
      REAL(fp), ALLOCATABLE :: FRZ0H(:)             !< Thermal Surface roughness length [m]
      REAL(fp), ALLOCATABLE :: PBLH(:)              !< PBL height [m]
      REAL(fp), ALLOCATABLE :: F_OF_PBL(:,:)        !< Fraction of box within PBL [1]
      REAL(fp), ALLOCATABLE :: F_UNDER_PBLTOP(:,:)  !< Fraction of box under PBL top

      ! Cloud & Precipitation Related Fields
      !-------------------------------------
      REAL(fp), ALLOCATABLE :: CLDFRC(:)         !< Column cloud fraction [1]
      REAL(fp), ALLOCATABLE :: CONV_DEPTH(:)     !< Convective cloud depth [m]
      REAL(fp), ALLOCATABLE :: FLASH_DEN(:)     !< Lightning flash density [#/km2/s]
      REAL(fp), ALLOCATABLE :: CNV_FRC(:)        !< Convective fraction [1]
      REAL(fp), ALLOCATABLE :: CLDF(:,:)        !< 3-D cloud fraction [1]
      REAL(fp), ALLOCATABLE :: CMFMC(:,:)       !< Cloud mass flux [kg/m2/s]
      REAL(fp), ALLOCATABLE :: DQRCU(:,:)       !< Conv precip production rate [kg/kg/s] (assume per dry air)
      REAL(fp), ALLOCATABLE :: DQRLSAN(:,:)     !< LS precip prod rate [kg/kg/s] (assume per dry air)
      REAL(fp), ALLOCATABLE :: DTRAIN(:,:)      !< Detrainment flux [kg/m2/s]
      REAL(fp), ALLOCATABLE :: PRECANV(:)        !< Anvil previp @ ground [kg/m2/s] -> [mm/day]
      REAL(fp), ALLOCATABLE :: PRECCON(:)        !< Conv  precip @ ground [kg/m2/s] -> [mm/day]
      REAL(fp), ALLOCATABLE :: PRECLSC(:)        !< Large-scale precip @ ground kg/m2/s] -> [mm/day]
      REAL(fp), ALLOCATABLE :: PRECTOT(:)        !< Total precip @ ground [kg/m2/s] -> [mm/day]
      REAL(fp), ALLOCATABLE :: QI(:,:)          !< Mass fraction of cloud ice water [kg/kg dry air]
      REAL(fp), ALLOCATABLE :: QL(:,:)          !< Mass fraction of cloud liquid water [kg/kg dry air]
      REAL(fp), ALLOCATABLE :: PFICU(:,:)       !< Dwn flux ice prec:conv [kg/m2/s]
      REAL(fp), ALLOCATABLE :: PFILSAN(:,:)     !< Dwn flux ice prec:LS+anv [kg/m2/s]
      REAL(fp), ALLOCATABLE :: PFLCU(:,:)       !< Dwn flux liq prec:conv [kg/m2/s]
      REAL(fp), ALLOCATABLE :: PFLLSAN(:,:)     !< Dwn flux ice prec:LS+anv [kg/m2/s]
      REAL(fp), ALLOCATABLE :: TAUCLI(:,:)      !< Opt depth of ice clouds [1]
      REAL(fp), ALLOCATABLE :: TAUCLW(:,:)      !< Opt depth of H2O clouds [1]

      ! State Related Fields
      !---------------------
      REAL(fp), ALLOCATABLE :: TS(:)             !< Surface temperature [K]
      REAL(fp), ALLOCATABLE :: T2M(:)            !< Temperature 2m [K]
      REAL(fp), ALLOCATABLE :: TSKIN(:)          !< Surface skin temperature [K]
      REAL(fp), ALLOCATABLE :: PHIS(:)           !< Surface geopotential height [m2/s2]
      REAL(fp), ALLOCATABLE :: PS(:)             !< Surface Pressure [hPa]
      REAL(fp), ALLOCATABLE :: SST(:)            !< Sea surface temperature [K]
      REAL(fp), ALLOCATABLE :: SLP(:)            !< Sea level pressure [hPa]
      REAL(fp), ALLOCATABLE :: TO3(:)            !< Total overhead O3 column [DU]
      REAL(fp), ALLOCATABLE :: PS_WET(:)         !< Wet surface pressure at start of timestep [hPa]
      REAL(fp), ALLOCATABLE :: PS_DRY(:)         !< Dry surface pressure at start of timestep [hPa]
      REAL(fp), ALLOCATABLE :: QV2M(:)           !< Specific Humidity at 2m [kg/kg]
      REAL(fp), ALLOCATABLE :: TROPP(:)          !< Tropopause pressure [hPa]
      INTEGER,  ALLOCATABLE :: TropLev(:)        !< Tropopause level [1]
      REAL(fp), ALLOCATABLE :: TropHt(:)         !< Tropopause height [km]

      REAL(fp), ALLOCATABLE :: Z(:,:)           !< Full Layer Geopotential Height
      REAL(fp), ALLOCATABLE :: ZMID(:,:)        !< Mid Layer Geopotential Height
      REAL(fp), ALLOCATABLE :: BXHEIGHT(:,:)    !< Grid box height [m] (dry air)
      REAL(fp), ALLOCATABLE :: QV(:,:)          !< Specific Humidity [kg/kg]
      REAL(fp), ALLOCATABLE :: T(:,:)           !< Temperature [K]
      REAL(fp), ALLOCATABLE :: THETA(:,:)       !< Potential temperature [K]
      REAL(fp), ALLOCATABLE :: TV(:,:)          !< Virtual temperature [K]
      REAL(fp), ALLOCATABLE :: V(:,:)           !< N/S component of wind [m s-1]
      REAL(fp), ALLOCATABLE :: U(:,:)           !< E/W component of wind [m s-1]
      REAL(fp), ALLOCATABLE :: OMEGA(:,:)       !< Updraft velocity [Pa/s]
      REAL(fp), ALLOCATABLE :: RH(:,:)          !< Relative humidity [%]
      REAL(fp), ALLOCATABLE :: SPHU(:,:)        !< Specific humidity [g H2O/kg tot air]
      REAL(fp), ALLOCATABLE :: AIRDEN(:,:)      !< Dry air density [kg/m3]
      REAL(fp), ALLOCATABLE :: AIRNUMDEN(:,:)   !< Dry air density [molec/cm3]
      REAL(fp), ALLOCATABLE :: MAIRDEN(:,:)     !< Moist air density [kg/m3]
      REAL(fp), ALLOCATABLE :: AVGW(:,:)        !< Water vapor volume mixing ratio [vol H2O/vol dry air]
      REAL(fp), ALLOCATABLE :: DELP(:,:)        !< Delta-P (wet) across box [hPa]
      REAL(fp), ALLOCATABLE :: DELP_DRY(:,:)    !< Delta-P (dry) across box [hPa]
      REAL(fp), ALLOCATABLE :: DAIRMASS(:,:)    !< Dry air mass [kg] in grid box
      REAL(fp), ALLOCATABLE :: AIRVOL(:,:)      !< Grid box volume [m3] (dry air)
      REAL(fp), ALLOCATABLE :: PEDGE_DRY(:,:)   !< Dry air partial pressure @ level edges [hPa]
      REAL(fp), ALLOCATABLE :: PMID(:,:)        !< Average wet air pressure [hPa] defined as arithmetic average of edge pressures
      REAL(fp), ALLOCATABLE :: PMID_DRY(:,:)    !< Dry air partial pressure [hPa] defined as arithmetic avg of edge pressures

   END TYPE MetStateType

CONTAINS

   !---------------------------------------------------------------------------
   ! PUBLIC MEMBER FUNCTIONS
   !---------------------------------------------------------------------------
   !
   SUBROUTINE Zero_MetState( MetState, RC )
      !
      ! !INPUT/OUTPUT PARAMETERS:
      !
      TYPE(MetStateType), INTENT(INOUT) :: MetState
      !
      ! !OUTPUT PARAMETERS:
      !
      INTEGER,        INTENT(OUT)   :: RC
      !
      ! !REVISION HISTORY:
      !  21 Sep 2020 - R. Yantosca - Initial version
      !  See the subsequent Git history with the gitk browser!
      !EOP
      !------------------------------------------------------------------------------
      !BOC
      !
      ! Initialize
      RC = CC_SUCCESS

      MetState%USTAR = ZERO

   END SUBROUTINE Zero_MetState

   !>
   !! \brief Allocate the MetState object
   !!
   !! \ingroup core_modules
   !!
   !! \param GridState   CATCHem grid state
   !! \param MetState    CATCHem met state
   !! \param RC          Error return code
   !!!>
   SUBROUTINE Met_Allocate( MetState, RC)
      ! USES

      IMPLICIT NONE

      ! Arguments
      TYPE(MetStateType), INTENT(INOUT) :: MetState !< Meteorological state
      INTEGER,            INTENT(OUT)   :: RC       !< Return code

      ! Local variables
      CHARACTER(LEN=255) :: ErrMsg, thisLoc
      integer :: nHORZ
      integer :: nLEVS
      integer :: nSOIL
      integer :: nLNDTYPE

      ! Initialize
      RC = CC_SUCCESS
      ErrMsg = ''
      thisLoc = ' -> at Met_Allocate (in core/metstate_mod.F90)'



      ! Nullify all fields for safety's sake before allocating them
      ! This can prevent compilation errors caused by uninitialized values


      !--------------------------------------------------
      ! Initialize fields
      !--------------------------------------------------
      nHORZ = MetState%nHORZ
      nLEVS = MetState%nLEVS
      nSOIL = MetState%nSOIL
      nLNDTYPE = MetState%nLNDTYPE

      ! Logicals
      !-----------------------
      if (.not. allocated(MetState%InStratosphere)) then
         allocate(MetState%InStratosphere(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%InStratosphere'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      if (.not. allocated(MetState%IsLand)) then
         allocate(MetState%IsLand(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%IsLand'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      if (.not. allocated(MetState%IsWater)) then
         allocate(MetState%IsWater(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%IsWater'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      if (.not. allocated(MetState%IsIce)) then
         allocate(MetState%IsIce(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%IsIce'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      if (.not. allocated(MetState%IsSnow)) then
         allocate(MetState%IsSnow(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%IsSnow'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      if (.not. allocated(MetState%InTroposphere)) then
         allocate(MetState%InTroposphere(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%InTroposphere'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      if (.not. allocated(MetState%InStratMeso)) then
         allocate(MetState%InStratMeso(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%InStratMeso'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      if (.not. allocated(MetState%InPbl)) then
         allocate(MetState%InPbl(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%InPbl'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! Land Specific Fields
      !---------------------
      if (.not. allocated(MetState%AREA_M2)) then
         allocate(MetState%AREA_M2(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%AREA_M2'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      if (.not. allocated(MetState%LWI)) then
         allocate(MetState%LWI(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%LWI'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      if (.not. allocated(MetState%CLAYFRAC)) then
         allocate(MetState%CLAYFRAC(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%CLAYFRAC'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      if (.not. allocated(MetState%SANDFRAC)) then
         allocate(MetState%SANDFRAC(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%SANDFRAC'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      if (.not. allocated(MetState%DSOILTYPE)) then
         allocate(MetState%DSOILTYPE(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%DSOILTYPE'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      if (.not. allocated(MetState%DLUSE)) then
         allocate(MetState%DLUSE(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%DLUSE'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      if (.not. allocated(MetState%FRVEG)) then
         allocate(MetState%FRVEG(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%FRVEG'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      if (.not. allocated(MetState%FRLAKE)) then
         allocate(MetState%FRLAKE(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%FRLAKE'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      if (.not. allocated(MetState%FRLANDICE)) then
         allocate(MetState%FRLANDICE(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%FRLANDICE'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      if (.not. allocated(MetState%FROCEAN)) then
         allocate(MetState%FROCEAN(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%FROCEAN'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      if (.not. allocated(MetState%FRSEAICE)) then
         allocate(MetState%FRSEAICE(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%FRSEAICE'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      if (.not. allocated(MetState%FRSNO)) then
         allocate(MetState%FRSNO(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%FRSNO'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      if (.not. allocated(MetState%LAI)) then
         allocate(MetState%LAI(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%LAI'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      if (.not. allocated(MetState%GVF)) then
         allocate(MetState%GVF(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%FRAC_SNOW'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      if (.not. allocated(MetState%RDRAG)) then
         allocate(MetState%RDRAG(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%RDRAG'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if


      if (.not. allocated(MetState%SEAICE00)) then
         allocate(MetState%SEAICE00(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%SEAICE00'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      if (.not. allocated(MetState%SEAICE10)) then
         allocate(MetState%SEAICE10(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%SEAICE10'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      if (.not. allocated(MetState%SEAICE20)) then
         allocate(MetState%SEAICE20(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%SEAICE20'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      if (.not. allocated(MetState%SEAICE30)) then
         allocate(MetState%SEAICE30(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%SEAICE30'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      if (.not. allocated(MetState%SEAICE40)) then
         allocate(MetState%SEAICE40(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%SEAICE40'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      if (.not. allocated(MetState%SEAICE50)) then
         allocate(MetState%SEAICE50(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%SEAICE50'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      if (.not. allocated(MetState%SEAICE60)) then
         allocate(MetState%SEAICE60(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%SEAICE60'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      if (.not. allocated(MetState%SEAICE70)) then
         allocate(MetState%SEAICE70(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%SEAICE70'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      if (.not. allocated(MetState%SEAICE80)) then
         allocate(MetState%SEAICE80(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%SEAICE80'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      if (.not. allocated(MetState%SEAICE90)) then
         allocate(MetState%SEAICE90(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%SEAICE90'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! if (.not. allocated(MetState%SEAICE100)) then
      !    allocate(MetState%SEAICE100(nHORZ), stat=RC)
      !    if (RC /= CC_SUCCESS) then
      !       errMsg = 'Error allocating MetState%SEAICE100'
      !       call CC_Error(errMsg, RC, thisLoc)
      !       return
      !    endif
      ! end if

      if (.not. allocated(MetState%SNODP)) then
         allocate(MetState%SNODP(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%SNODP'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      if (.not. allocated(MetState%SNOMAS)) then
         allocate(MetState%SNOMAS(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%SNOMAS'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      if (.not. allocated(MetState%SSM)) then
         allocate(MetState%SSM(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%SSM'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      if (.not. allocated(MetState%USTAR_THRESHOLD)) then
         allocate(MetState%USTAR_THRESHOLD(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%USTAR_THRESHOLD'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      if (.not. allocated(MetState%PBLH)) then
         allocate(MetState%PBLH(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%PBLH'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! GWETTOP
      if (.not. allocated(MetState%GWETTOP)) then
         allocate(MetState%GWETTOP(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%GWETTOP'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! GWETROOT
      if (.not. allocated(MetState%GWETROOT)) then
         allocate(MetState%GWETROOT(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%GWETROOT'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! WILT
      if (.not. allocated(MetState%WILT)) then
         allocate(MetState%WILT(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%WILT'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! SOILM
      if (.not. allocated(MetState%SOILM)) then
         allocate(MetState%SOILM(nHORZ,nSOIL), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%SOILM'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! FRLANDUSE
      if (.not. allocated(MetState%FRLANDUSE)) then
         allocate(MetState%FRLANDUSE(nHORZ, nLNDTYPE), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%FRLANDUSE'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! FRLAI
      if (.not. allocated(MetState%FRLAI)) then
         allocate(MetState%FRLAI(nHORZ, nLNDTYPE), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%FRLAI'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! Albedo
      if (.not. allocated(MetState%ALBD_VIS)) then
         allocate(MetState%ALBD_VIS(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%ALBD_VIS'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! Near-IR Surface Albedo
      if (.not. allocated(MetState%ALBD_NIR)) then
         allocate(MetState%ALBD_NIR(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%ALBD_NIR'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! UV Surface Albedo
      if (.not. allocated(MetState%ALBD_UV)) then
         allocate(MetState%ALBD_UV(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%ALBD_UV'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! PARDR
      if (.not. allocated(MetState%PARDR)) then
         allocate(MetState%PARDR(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%PARDR'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! PARDF
      if (.not. allocated(MetState%PARDF)) then
         allocate(MetState%PARDF(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%PARDF'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! SUNCOS
      if (.not. allocated(MetState%SUNCOS)) then
         allocate(MetState%SUNCOS(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%SUNCOS'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! SUNCOSmid
      if (.not. allocated(MetState%SUNCOSmid)) then
         allocate(MetState%SUNCOSmid(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%SUNCOSmid'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! SUNCOSsum
      if (.not. allocated(MetState%SUNCOSsum)) then
         allocate(MetState%SUNCOSsum(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%SUNCOSsum'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! SWGRND
      if (.not. allocated(MetState%SWGDN)) then
         allocate(MetState%SWGDN(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%SWGRND'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! Flux related Fields
      !-----------------------
      ! EFLUX
      if (.not. allocated(MetState%EFLUX)) then
         allocate(MetState%EFLUX(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%EFLUX'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! HFLUX
      if (.not. allocated(MetState%HFLUX)) then
         allocate(MetState%HFLUX(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%HFLUX'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! U10M
      if (.not. allocated(MetState%U10M)) then
         allocate(MetState%U10M(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%U10M'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! V10M
      if (.not. allocated(MetState%V10M)) then
         allocate(MetState%V10M(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%V10M'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! USTAR
      if (.not. allocated(MetState%USTAR)) then
         allocate(MetState%USTAR(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%USTAR'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! Z0
      if (.not. allocated(MetState%Z0)) then
         allocate(MetState%Z0(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%Z0'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! FRZ0
      if (.not. allocated(MetState%FRZ0)) then
         allocate(MetState%FRZ0(nHORZ, nLNDTYPE), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%FRZ0'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! Z0H
      if (.not. allocated(MetState%Z0H)) then
         allocate(MetState%Z0H(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%Z0H'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! FRZ0H
      if (.not. allocated(MetState%FRZ0H)) then
         allocate(MetState%FRZ0H(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%FRZ0H'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! Cloud / Precipitation
      !-----------------------
      ! CLDFRC
      if (.not. allocated(MetState%CLDFRC)) then
         allocate(MetState%CLDFRC(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%CLDFRC'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! CLDF
      if (.not. allocated(MetState%CLDF)) then
         allocate(MetState%CLDF(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%CLDF'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! CONV_DEPTH
      if (.not. allocated(MetState%CONV_DEPTH)) then
         allocate(MetState%CONV_DEPTH(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%CMFMC'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! FLASH_DEN
      if (.not. allocated(MetState%FLASH_DEN)) then
         allocate(MetState%FLASH_DEN(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%DQRCU'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! CMFMC
      if (.not. allocated(MetState%CMFMC)) then
         allocate(MetState%CMFMC(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%CMFMC'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! DQRCU
      if (.not. allocated(MetState%DQRCU)) then
         allocate(MetState%DQRCU(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%DQRCU'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! DQRLSAN
      if (.not. allocated(MetState%DQRLSAN)) then
         allocate(MetState%DQRLSAN(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%DQRLSAN'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! DTRAIN
      if (.not. allocated(MetState%DTRAIN)) then
         allocate(MetState%DTRAIN(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%DTRAIN'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! PRECANV
      if (.not. allocated(MetState%PRECANV)) then
         allocate(MetState%PRECANV(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%PRECANV'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! PRECCONV
      if (.not. allocated(MetState%PRECCON)) then
         allocate(MetState%PRECCON(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%PRECCON'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! PRECLSC
      if (.not. allocated(MetState%PRECLSC)) then
         allocate(MetState%PRECLSC(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%PRECLSC'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! PRECTOT
      if (.not. allocated(MetState%PRECTOT)) then
         allocate(MetState%PRECTOT(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%PRECTOT'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! QI
      if (.not. allocated(MetState%QI)) then
         allocate(MetState%QI(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%QI'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! QL
      if (.not. allocated(MetState%QL)) then
         allocate(MetState%QL(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%QL'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! PFICU
      if (.not. allocated(MetState%PFICU)) then
         allocate(MetState%PFICU(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%PFICU'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! PFILSAN
      if (.not. allocated(MetState%PFILSAN)) then
         allocate(MetState%PFILSAN(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%PFILSAN'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! PFLLSAN
      if (.not. allocated(MetState%PFLLSAN)) then
         allocate(MetState%PFLLSAN(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%PFLLSAN'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! TAUCLI
      if (.not. allocated(MetState%TAUCLI)) then
         allocate(MetState%TAUCLI(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%TAUCLI'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! TAUCLW
      if (.not. allocated(MetState%TAUCLW)) then
         allocate(MetState%TAUCLW(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%TAUCLW'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! State Variables
      ! ---------------

      ! Z
      if (.not. allocated(MetState%Z)) then
         allocate(MetState%Z(nHORZ, nLEVS + 1), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%Z'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! ZMID
      if (.not. allocated(MetState%ZMID)) then
         allocate(MetState%ZMID(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%ZMID'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! BXHEIGHT
      if (.not. allocated(MetState%BXHEIGHT)) then
         allocate(MetState%BXHEIGHT(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%BXHEIGHT'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! QV
      if (.not. allocated(MetState%QV)) then
         allocate(MetState%QV(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%InStratosphere'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! T
      if (.not. allocated(MetState%T)) then
         allocate(MetState%T(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%T'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! THETA
      if (.not. allocated(MetState%THETA)) then
         allocate(MetState%THETA(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%THETA'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! TV
      if (.not. allocated(MetState%TV)) then
         allocate(MetState%TV(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%TV'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! U
      if (.not. allocated(MetState%U)) then
         allocate(MetState%U(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%InStratosphere'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! V
      if (.not. allocated(MetState%V)) then
         allocate(MetState%V(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%InStratosphere'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! OMEGA
      if (.not. allocated(MetState%OMEGA)) then
         allocate(MetState%OMEGA(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%OMEGA'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! RH
      if (.not. allocated(MetState%RH)) then
         allocate(MetState%RH(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%InStratosphere'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! SPHU
      if (.not. allocated(MetState%SPHU)) then
         allocate(MetState%SPHU(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%InStratosphere'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! AIRDEN
      if (.not. allocated(MetState%AIRDEN)) then
         allocate(MetState%AIRDEN(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%InStratosphere'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! AIRNUMDEN
      if (.not. allocated(MetState%AIRNUMDEN)) then
         allocate(MetState%AIRNUMDEN(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%InStratosphere'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! MAIRDEN
      if (.not. allocated(MetState%MAIRDEN)) then
         allocate(MetState%MAIRDEN(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%InStratosphere'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! AVGW
      if (.not. allocated(MetState%AVGW)) then
         allocate(MetState%AVGW(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%InStratosphere'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! DELP
      if (.not. allocated(MetState%DELP)) then
         allocate(MetState%DELP(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%InStratosphere'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! DELP_DRY
      if (.not. allocated(MetState%DELP_DRY)) then
         allocate(MetState%DELP_DRY(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%InStratosphere'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! DAIRMASS
      if (.not. allocated(MetState%DAIRMASS)) then
         allocate(MetState%DAIRMASS(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%InStratosphere'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! AIRVOL
      if (.not. allocated(MetState%AIRVOL)) then
         allocate(MetState%AIRVOL(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%InStratosphere'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! PMID
      if (.not. allocated(MetState%PMID)) then
         allocate(MetState%PMID(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%InStratosphere'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! PMID_DRY
      if (.not. allocated(MetState%PMID_DRY)) then
         allocate(MetState%PMID_DRY(nHORZ, nLEVS), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%InStratosphere'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! PEDGE
      if (.not. allocated(MetState%PEDGE_DRY)) then
         allocate(MetState%PEDGE_DRY(nHORZ, nLEVS+1), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%InStratosphere'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! TS
      if (.not. allocated(MetState%TS)) then
         allocate(MetState%TS(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%TS'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! T2M
      if (.not. allocated(MetState%T2M)) then
         allocate(MetState%T2M(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%T2M'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! TSKIN
      if (.not. allocated(MetState%TSKIN)) then
         allocate(MetState%TSKIN(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%TSKIN'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! PHIS
      if (.not. allocated(MetState%PHIS)) then
         allocate(MetState%PHIS(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%PHIS'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! PS
      if (.not. allocated(MetState%PS)) then
         allocate(MetState%PS(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%PS'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! SST
      if (.not. allocated(MetState%SST)) then
         allocate(MetState%SST(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%SST'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! SLP
      if (.not. allocated(MetState%SLP)) then
         allocate(MetState%SLP(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%SLP'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! TO3
      if (.not. allocated(MetState%TO3)) then
         allocate(MetState%TO3(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%TO3'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! PS_WET
      if (.not. allocated(MetState%PS_WET)) then
         allocate(MetState%PS_WET(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%PS_WET'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! PS_DRY
      if (.not. allocated(MetState%PS_DRY)) then
         allocate(MetState%PS_DRY(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%PS_DRY'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! QV2M
      if (.not. allocated(MetState%QV2M)) then
         allocate(MetState%QV2M(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%QV2M'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! TROPP
      if (.not. allocated(MetState%TROPP)) then
         allocate(MetState%TROPP(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%TROPP'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! TropLev
      if (.not. allocated(MetState%TropLev)) then
         allocate(MetState%TropLev(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%TropLev'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

      ! TropHt
      if (.not. allocated(MetState%TropHt)) then
         allocate(MetState%TropHt(nHORZ), stat=RC)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Error allocating MetState%TropHt'
            call CC_Error(errMsg, RC, thisLoc)
            return
         endif
      end if

   end subroutine Met_Allocate

END MODULE MetState_Mod

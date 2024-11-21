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
   USE Error_Mod
   USE Precision_Mod


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

      ! Integer Fields for MetState Array Dimensions
      !---------------------------------------------
      INTEGER               :: nLEVS             !< Number of vertical levels
      INTEGER               :: nSOIL             !< # number of soil layers
      INTEGER               :: nLNDTYPE          !< # of landtypes in box (I,J)

      ! Location Specific Fields
      !-------------------------
      real(fp) :: LAT               !< Latitude [degrees]
      real(fp) :: LON               !< Longitude [degrees]

      ! TIMESTEP
      !---------
      REAL(fp) :: TSTEP             !< Time step [s]
      REAL(fp) :: JDAY              !< Julian Day of year
      REAL(fp) :: JDAY_FRAC         !< Fractional Julian Day of year
      real(fp) :: HR                !< Hour of Day in UTC

      ! Logicals
      !---------
      LOGICAL           :: IsLand                !< Is this a land grid box?
      LOGICAL           :: IsWater               !< Is this a water grid box?
      LOGICAL           :: IsIce                 !< Is this a ice grid box?
      LOGICAL           :: IsSnow                !< Is this a snow grid box?
      LOGICAL           :: IsLocalNoon           ! Is it local noon (between 11 and 13 local solar time?
      LOGICAL,  ALLOCATABLE :: InStratMeso(:)    !< Are we in the stratosphere or mesosphere?
      LOGICAL,  ALLOCATABLE :: InStratosphere(:) !< Are we in the stratosphere?
      LOGICAL,  ALLOCATABLE :: InTroposphere(:)  !< Are we in the troposphere?
      LOGICAL,  ALLOCATABLE :: InPbl(:)          !< Are we in the PBL?


      ! Land Specific Fields
      !---------------------
      REAL(fp)              :: AREA_M2         !< Grid box surface area [m2]
      INTEGER               :: LWI             !< Land water ice mask (0-sea, 1-land, 2-ice)
      REAL(fp)              :: CLAYFRAC        !< Fraction of clay [1]
      INTEGER               :: DSOILTYPE       !< Dominant soil type
      INTEGER               :: DLUSE           !< Dominant land-use type
      REAL(fp)              :: FRVEG           !< Fraction of veg [1]
      REAL(fp)              :: FRLAKE          !< Fraction of lake [1]
      REAL(fp)              :: FRLAND          !< Fraction of land [1]
      REAL(fp)              :: FRLANDIC        !< Fraction of land ice [1]
      REAL(fp)              :: FROCEAN         !< Fraction of ocean [1]
      REAL(fp)              :: FRSEAICE        !< Sfc sea ice fraction [1]
      REAL(fp)              :: FRSNO           !< Sfc snow fraction [1]
      REAL(fp)              :: FRURBAN         !< Fraction of urban [1]
      REAL(fp)              :: LAI             !< Leaf area index [m2/m2] (online) Dominant
      REAL(fp)              :: GVF             !< Green Vegetative Fraction
      REAL(fp)              :: RDRAG           !< Drag Partition [1]
      REAL(fp)              :: SANDFRAC        !< Fraction of sand [1]
      REAL(fp)              :: SEAICE00        !< Sea ice coverage 00-10%
      REAL(fp)              :: SEAICE10        !< Sea ice coverage 10-20%
      REAL(fp)              :: SEAICE20        !< Sea ice coverage 20-30%
      REAL(fp)              :: SEAICE30        !< Sea ice coverage 30-40%
      REAL(fp)              :: SEAICE40        !< Sea ice coverage 40-50%
      REAL(fp)              :: SEAICE50        !< Sea ice coverage 50-60%
      REAL(fp)              :: SEAICE60        !< Sea ice coverage 60-70%
      REAL(fp)              :: SEAICE70        !< Sea ice coverage 70-80%
      REAL(fp)              :: SEAICE80        !< Sea ice coverage 80-90%
      REAL(fp)              :: SEAICE90        !< Sea ice coverage 90-100%
      REAL(fp)              :: SNODP           !< Snow depth [m]
      REAL(fp)              :: SNOMAS          !< Snow mass [kg/m2]
      REAL(fp)              :: SSM             !< Sediment Supply Map [1]
      REAL(fp)              :: USTAR_THRESHOLD !< Threshold friction velocity [m/s]
      REAL(fp)              :: GWETTOP         !< Top soil moisture [1]
      REAL(fp)              :: GWETROOT        !< Root Zone soil moisture [1]
      REAL(fp)              :: WILT            !< Wilt point [1]
      REAL(fp), ALLOCATABLE :: SOILM(:)        !< Volumetric Soil moisture [m3/m3]
      REAL(fp), ALLOCATABLE :: SOILT(:)        !< Volumetric Soil T [K]
      REAL(fp), ALLOCATABLE :: FRLANDUSE(:)    !< Fractional Land Use
      REAL(fp), ALLOCATABLE :: FRLAI(:)        !< LAI in each Fractional Land use type [m2/m2]

      ! Radiation Related Surface Fields
      !---------------------------------
      REAL(fp)              :: ALBD_VIS       !< Visible surface albedo [1]
      REAL(fp)              :: ALBD_NIR       !< Near-IR surface albedo [1]
      REAL(fp)              :: ALBD_UV        !< UV surface albedo [1]
      REAL(fp)              :: PARDR          !< Direct photsynthetically active radiation [W/m2]
      REAL(fp)              :: PARDF          !< Diffuse photsynthetically active radiation [W/m2]
      REAL(fp)              :: SUNCOS         !< COS(solar zenith angle) at current time
      REAL(fp)              :: SUNCOSmid      !< COS(solar zenith angle) at midpoint of chem timestep
      REAL(fp)              :: SUNCOSsum      !< Sum of COS(SZA) for HEMCO OH diurnal variability
      REAL(fp)              :: SZAFACT        !< Diurnal scale factor for HEMCO OH diurnal variability (computed) [1]
      REAL(fp)              :: SWGDN          !< Incident radiation @ ground [W/m2]



      ! Flux Related Fields
      !--------------------
      REAL(fp)              :: EFLUX             !< Latent heat flux [W/m2]
      REAL(fp)              :: HFLUX             !< Sensible heat flux [W/m2]
      REAL(fp)              :: U10M              !< E/W wind speed @ 10m ht [m/s]
      REAL(fp)              :: USTAR             !< Friction velocity [m/s]
      REAL(fp)              :: V10M              !< N/S wind speed @ 10m ht [m/s]
      REAL(fp)              :: Z0                !< Surface roughness height [m]
      REAL(fp)              :: Z0H               !< Surface roughness height, for heat (thermal roughness) [m]
      REAL(fp), ALLOCATABLE :: FRZ0(:)           !< Aerodynamic Roughness Length per FRLANDUSE
      REAL(fp)              :: PBLH              !< PBL height [m]
      REAL(fp), ALLOCATABLE :: F_OF_PBL(:)       !< Fraction of box within PBL [1]
      REAL(fp), ALLOCATABLE :: F_UNDER_PBLTOP(:) !< Fraction of box under PBL top

      ! Cloud & Precipitation Related Fields
      !-------------------------------------
      REAL(fp)              :: CLDFRC         !< Column cloud fraction [1]
      REAL(fp)              :: CONV_DEPTH     !< Convective cloud depth [m]
      REAL(fp)              :: FLASH_DENS     !< Lightning flash density [#/km2/s]
      REAL(fp)              :: CNV_FRC        !< Convective fraction [1]
      REAL(fp), ALLOCATABLE :: CLDF(:)        !< 3-D cloud fraction [1]
      REAL(fp), ALLOCATABLE :: CMFMC(:)       !< Cloud mass flux [kg/m2/s]
      REAL(fp), ALLOCATABLE :: DQRCU(:)       !< Conv precip production rate [kg/kg/s] (assume per dry air)
      REAL(fp), ALLOCATABLE :: DQRLSAN(:)     !< LS precip prod rate [kg/kg/s] (assume per dry air)
      REAL(fp), ALLOCATABLE :: DTRAIN(:)      !< Detrainment flux [kg/m2/s]
      REAL(fp)              :: PRECANV        !< Anvil previp @ ground [kg/m2/s] -> [mm/day]
      REAL(fp)              :: PRECCON        !< Conv  precip @ ground [kg/m2/s] -> [mm/day]
      REAL(fp)              :: PRECLSC        !< Large-scale precip @ ground kg/m2/s] -> [mm/day]
      REAL(fp)              :: PRECTOT        !< Total precip @ ground [kg/m2/s] -> [mm/day]
      REAL(fp), ALLOCATABLE :: QI(:)          !< Mass fraction of cloud ice water [kg/kg dry air]
      REAL(fp), ALLOCATABLE :: QL(:)          !< Mass fraction of cloud liquid water [kg/kg dry air]
      REAL(fp), ALLOCATABLE :: PFICU(:)       !< Dwn flux ice prec:conv [kg/m2/s]
      REAL(fp), ALLOCATABLE :: PFILSAN(:)     !< Dwn flux ice prec:LS+anv [kg/m2/s]
      REAL(fp), ALLOCATABLE :: PFLCU(:)       !< Dwn flux liq prec:conv [kg/m2/s]
      REAL(fp), ALLOCATABLE :: PFLLSAN(:)     !< Dwn flux ice prec:LS+anv [kg/m2/s]
      REAL(fp), ALLOCATABLE :: TAUCLI(:)      !< Opt depth of ice clouds [1]
      REAL(fp), ALLOCATABLE :: TAUCLW(:)      !< Opt depth of H2O clouds [1]

      ! State Related Fields
      !---------------------
      REAL(fp)              :: PHIS           !< Surface geopotential height [m2/s2]
      REAL(fp), ALLOCATABLE :: Z(:)           !< Full Layer Geopotential Height
      REAL(fp), ALLOCATABLE :: ZMID(:)        !< Mid Layer Geopotential Height
      REAL(fp), ALLOCATABLE :: BXHEIGHT(:)    !< Grid box height [m] (dry air)
      REAL(fp)              :: PS_WET         !< Wet surface pressure at start of timestep [hPa]
      REAL(fp)              :: PS_DRY         !< Dry surface pressure at start of timestep [hPa]
      REAL(fp)              :: QV2M           !< Specific Humidity at 2m [kg/kg]
      REAL(fp), ALLOCATABLE :: QV(:)          !< Specific Humidity [kg/kg]
      REAL(fp)              :: T2M            !< Temperature 2m [K]
      REAL(fp)              :: TS             !< Surface temperature [K]
      REAL(fp)              :: TSKIN          !< Surface skin temperature [K]
      REAL(fp), ALLOCATABLE :: T(:)           !< Temperature [K]
      REAL(fp), ALLOCATABLE :: THETA(:)       !< Potential temperature [K]
      REAL(fp), ALLOCATABLE :: TV(:)          !< Virtual temperature [K]
      REAL(fp), ALLOCATABLE :: V(:)           !< N/S component of wind [m s-1]
      REAL(fp), ALLOCATABLE :: U(:)           !< E/W component of wind [m s-1]
      REAL(fp)              :: SST            !< Sea surface temperature [K]
      REAL(fp)              :: SLP            !< Sea level pressure [hPa]
      REAL(fp)              :: PS             !< Surface Pressure [hPa]
      REAL(fp), ALLOCATABLE :: OMEGA(:)       !< Updraft velocity [Pa/s]
      REAL(fp), ALLOCATABLE :: RH(:)          !< Relative humidity [%]
      REAL(fp)              :: TO3            !< Total overhead O3 column [DU]
      REAL(fp)              :: TROPP          !< Tropopause pressure [hPa]
      INTEGER               :: TropLev        !< Tropopause level [1]
      REAL(fp)              :: TropHt         !< Tropopause height [km]
      REAL(fp), ALLOCATABLE :: SPHU(:)        !< Specific humidity [g H2O/kg tot air]
      REAL(fp), ALLOCATABLE :: AIRDEN(:)      !< Dry air density [kg/m3]
      REAL(fp), ALLOCATABLE :: AIRNUMDEN(:)   !< Dry air density [molec/cm3]
      REAL(fp), ALLOCATABLE :: MAIRDEN(:)     !< Moist air density [kg/m3]
      REAL(fp), ALLOCATABLE :: AVGW(:)        !< Water vapor volume mixing ratio [vol H2O/vol dry air]
      REAL(fp), ALLOCATABLE :: DELP(:)        !< Delta-P (wet) across box [hPa]
      REAL(fp), ALLOCATABLE :: DELP_DRY(:)    !< Delta-P (dry) across box [hPa]
      REAL(fp), ALLOCATABLE :: DAIRMASS(:)    !< Dry air mass [kg] in grid box
      REAL(fp), ALLOCATABLE :: AIRVOL(:)      !< Grid box volume [m3] (dry air)
      REAL(fp), ALLOCATABLE :: PEDGE_DRY(:)   !< Dry air partial pressure @ level edges [hPa]
      REAL(fp), ALLOCATABLE :: PMID(:)        !< Average wet air pressure [hPa] defined as arithmetic average of edge pressures
      REAL(fp), ALLOCATABLE :: PMID_DRY(:)    !< Dry air partial pressure [hPa] defined as arithmetic avg of edge pressures

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

   SUBROUTINE Met_Allocate(MetState, RC)
      USE error_mod,     ONLY : CC_CheckAllocate, CC_SUCCESS, CC_Error

      IMPLICIT NONE

      ! Arguments
      TYPE(MetStateType),  INTENT(INOUT) :: MetState !< Meteorological state
      INTEGER,            INTENT(OUT)   :: RC       !< Return code

      ! Local variables
      CHARACTER(LEN=255) :: ErrMsg, thisLoc

      ! Initialize
      RC = CC_SUCCESS
      ErrMsg = ''
      thisLoc = ' -> at Met_Allocate (in core/metstate_mod.F90)'

      ! Visible Surface Albedo
      !-----------------------
      MetState%ALBD_VIS = ZERO
      MetState%ALBD_NIR = ZERO
      MetState%ALBD_UV = ZERO
      MetState%AREA_M2 = ZERO
      MetState%CLDFRC = ZERO
      MetState%CONV_DEPTH = ZERO
      MetState%EFLUX = ZERO
      MetState%FRLAKE = ZERO
      MetState%FRLAND = ZERO
      MetState%FRLANDIC = ZERO
      MetState%FROCEAN = ZERO
      MetState%FRSEAICE = ZERO
      MetState%FRSNO = ZERO
      MetState%GWETROOT = ZERO
      MetState%GWETTOP = ZERO
      MetState%HFLUX = ZERO
      MetState%IsLand = .false.
      MetState%IsWater = .false.
      MetState%IsIce = .false.
      MetState%IsSnow = .false.
      MetState%LAI = ZERO
      MetState%PARDR = ZERO
      MetState%PARDF = ZERO
      MetState%PBLH = ZERO
      MetState%PS = ZERO
      MetState%QV2M = ZERO
      MetState%T2M = ZERO
      MetState%TSKIN = ZERO
      MetState%U10M = ZERO
      MetState%V10M = ZERO
      MetState%z0 = ZERO
      MetState%z0h = ZERO
      MetState%USTAR_THRESHOLD = ZERO
      MetState%RDRAG = ZERO
      MetState%SSM = ZERO
      MetState%CLAYFRAC = ZERO
      MetSTate%SANDFRAC = ZERO
      MetState%SST = ZERO

      ! Allocate Column Fields using CC_CheckAllocate
      !-----------------------
      ! Logicals
      RC = CC_CheckAllocate(MetState%InStratosphere, MetState%nLEVS, 'MetState%InStratosphere')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%InPbl, MetState%nLEVS, 'MetState%InPbl')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%InStratMeso, MetState%nLEVS, 'MetState%InStratMeso')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%InTroposphere, MetState%nLEVS, 'MetState%InTroposphere')
      IF (RC /= CC_SUCCESS) RETURN

      ! Flux Related
      RC = CC_CheckAllocate(MetState%F_OF_PBL, MetState%nLEVS, 'MetState%F_OF_PBL')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%F_UNDER_PBLTOP, MetState%nLEVS, 'MetState%F_UNDER_PBLTOP')
      IF (RC /= CC_SUCCESS) RETURN

      ! Cloud / Precipitation
      RC = CC_CheckAllocate(MetState%CLDF, MetState%nLEVS, 'MetState%CLDF')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%CMFMC, MetState%nLEVS, 'MetState%CMFMC')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%DQRCU, MetState%nLEVS, 'MetState%DQRCU')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%DQRLSAN, MetState%nLEVS, 'MetState%DQRLSAN')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%DTRAIN, MetState%nLEVS, 'MetState%DTRAIN')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%QI, MetState%nLEVS, 'MetState%QI')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%QL, MetState%nLEVS, 'MetState%QL')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%PFICU, MetState%nLEVS, 'MetState%PFICU')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%PFILSAN, MetState%nLEVS, 'MetState%PFILSAN')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%PFLCU, MetState%nLEVS, 'MetState%PFLCU')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%PFLLSAN, MetState%nLEVS, 'MetState%PFLLSAN')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%TAUCLI, MetState%nLEVS, 'MetState%TAUCLI')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%TAUCLW, MetState%nLEVS, 'MetState%TAUCLW')
      IF (RC /= CC_SUCCESS) RETURN

      ! State Related
      RC = CC_CheckAllocate(MetState%Z, MetState%nLEVS + 1, 'MetState%Z')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%ZMID, MetState%nLEVS, 'MetState%ZMID')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%BXHEIGHT, MetState%nLEVS, 'MetState%BXHEIGHT')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%QV, MetState%nLEVS, 'MetState%QV')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%T, MetState%nLEVS, 'MetState%T')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%THETA, MetState%nLEVS, 'MetState%THETA')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%TV, MetState%nLEVS, 'MetState%TV')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%U, MetState%nLEVS, 'MetState%U')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%V, MetState%nLEVS, 'MetState%V')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%OMEGA, MetState%nLEVS, 'MetState%OMEGA')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%RH, MetState%nLEVS, 'MetState%RH')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%SPHU, MetState%nLEVS, 'MetState%SPHU')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%AIRDEN, MetState%nLEVS, 'MetState%AIRDEN')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%AIRNUMDEN, MetState%nLEVS, 'MetState%AIRNUMDEN')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%MAIRDEN, MetState%nLEVS, 'MetState%MAIRDEN')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%AVGW, MetState%nLEVS, 'MetState%AVGW')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%DELP, MetState%nLEVS, 'MetState%DELP')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%DELP_DRY, MetState%nLEVS, 'MetState%DELP_DRY')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%DAIRMASS, MetState%nLEVS, 'MetState%DAIRMASS')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%AIRVOL, MetState%nLEVS, 'MetState%AIRVOL')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%PMID, MetState%nLEVS, 'MetState%PMID')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%PMID_DRY, MetState%nLEVS, 'MetState%PMID_DRY')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%PEDGE_DRY, MetState%nLEVS + 1, 'MetState%PEDGE_DRY')
      IF (RC /= CC_SUCCESS) RETURN

      ! Surface and Soil Properties
      RC = CC_CheckAllocate(MetState%SOILM, MetState%nSOIL, 'MetState%SOILM')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%SOILT, MetState%nSOIL, 'MetState%SOILT')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%FRLANDUSE, MetState%nLNDTYPE, 'MetState%FRLANDUSE')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%FRLAI, MetState%nLNDTYPE, 'MetState%FRLAI')
      IF (RC /= CC_SUCCESS) RETURN

      RC = CC_CheckAllocate(MetState%FRZ0, MetState%nLNDTYPE, 'MetState%FRZ0')
      IF (RC /= CC_SUCCESS) RETURN

   END SUBROUTINE Met_Allocate

   !>
   !! \brief Deallocate the MetState object
   !!
   !! \ingroup core_modules
   !!
   !! \param MetState    CATCHem met state
   !! \param RC          Error return code
   !!!>
   SUBROUTINE Met_Finalize( MetState, RC )
      ! Arguments
      TYPE(MetStateType), INTENT(INOUT) :: MetState !< Meteorological state
      INTEGER,            INTENT(OUT)   :: RC       !< Return code

      ! Local variables
      CHARACTER(LEN=255) :: ErrMsg, thisLoc

      ! Initialize
      RC = CC_SUCCESS
      ErrMsg = ''
      thisLoc = ' -> at Met_Finalize (in core/metstate_mod.F90)'

      ! Deallocate all allocated arrays
      RC = CC_CheckDeallocate(MetState%InStratosphere, 'MetState%InStratosphere')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%InPbl,          'MetState%InPbl')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%InStratMeso,    'MetState%InStratMeso')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%InTroposphere,  'MetState%InTroposphere')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%F_OF_PBL,       'MetState%F_OF_PBL')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%F_UNDER_PBLTOP, 'MetState%F_UNDER_PBLTOP')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%CLDF,           'MetState%CLDF')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%CMFMC,          'MetState%CMFMC')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%DQRCU,          'MetState%DQRCU')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%DQRLSAN,        'MetState%DQRLSAN')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%DTRAIN,         'MetState%DTRAIN')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%QI,             'MetState%QI')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%QL,             'MetState%QL')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%PFICU,          'MetState%PFICU')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%PFILSAN,        'MetState%PFILSAN')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%PFLCU,          'MetState%PFLCU')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%PFLLSAN,        'MetState%PFLLSAN')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%TAUCLI,         'MetState%TAUCLI')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%TAUCLW,         'MetState%TAUCLW')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%Z,              'MetState%Z')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%ZMID,           'MetState%ZMID')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%BXHEIGHT,       'MetState%BXHEIGHT')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%QV,             'MetState%QV')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%T,              'MetState%T')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%THETA,          'MetState%THETA')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%TV,             'MetState%TV')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%U,              'MetState%U')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%V,              'MetState%V')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%OMEGA,          'MetState%OMEGA')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%RH,             'MetState%RH')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%SPHU,           'MetState%SPHU')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%AIRDEN,         'MetState%AIRDEN')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%AIRNUMDEN,      'MetState%AIRNUMDEN')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%MAIRDEN,        'MetState%MAIRDEN')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%AVGW,           'MetState%AVGW')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%DELP,           'MetState%DELP')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%DELP_DRY,       'MetState%DELP_DRY')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%DAIRMASS,       'MetState%DAIRMASS')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%AIRVOL,         'MetState%AIRVOL')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%PMID,           'MetState%PMID')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%PMID_DRY,       'MetState%PMID_DRY')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%PEDGE_DRY,      'MetState%PEDGE_DRY')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%SOILM,          'MetState%SOILM')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%SOILT,          'MetState%SOILT')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%FRLANDUSE,      'MetState%FRLANDUSE')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%FRLAI,          'MetState%FRLAI')
      IF (RC /= CC_SUCCESS) RETURN
      RC = CC_CheckDeallocate(MetState%FRZ0,           'MetState%FRZ0')
      IF (RC /= CC_SUCCESS) RETURN

   END SUBROUTINE Met_Finalize



END MODULE MetState_Mod

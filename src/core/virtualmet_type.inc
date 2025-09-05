! Generated VirtualMetType definition based on MetState field definitions
! This macro should be included in the VirtualColumn_Mod.F90 type definition
! Auto-generated from MetState field definitions

   !> \brief Virtual meteorological data container with direct pointers
   !! \details Contains pointers to meteorological fields for a single column.
   !! Pointers are set directly from MetState accessor functions, eliminating
   !! data copying and providing efficient field access.
   type :: VirtualMetType
      ! 3D atmospheric fields (vertical profiles: nlev) - pointers to MetState data
      real(fp), pointer :: AIRDEN(:) => null()  !< Air density [kg/m3]
      real(fp), pointer :: AIRNUMDEN(:) => null()  !< Air number density [molec/cm3]
      real(fp), pointer :: AIRVOL(:) => null()  !< Air volume [m3]
      real(fp), pointer :: AVGW(:) => null()  !< Average molecular weight [g/mol]
      real(fp), pointer :: BXHEIGHT(:) => null()  !< Box height [m]
      real(fp), pointer :: CLDF(:) => null()  !< Cloud fraction [1]
      real(fp), pointer :: CMFMC(:) => null()  !< Cloud mass flux [kg/m2/s]
      real(fp), pointer :: DAIRMASS(:) => null()  !< Dry air mass [kg]
      real(fp), pointer :: DELP(:) => null()  !< Pressure thickness [Pa]
      real(fp), pointer :: DELP_DRY(:) => null()  !< Dry pressure thickness [Pa]
      real(fp), pointer :: DQRCU(:) => null()  !< Conv precip rate [kg/kg/s]
      real(fp), pointer :: DQRLSAN(:) => null()  !< LS precip rate [kg/kg/s]
      real(fp), pointer :: DTRAIN(:) => null()  !< Detrainment flux [kg/m2/s]
      real(fp), pointer :: F_OF_PBL(:) => null()  !< Fraction in PBL [1]
      real(fp), pointer :: F_UNDER_PBLTOP(:) => null()  !< Fraction under PBL top [1]
      real(fp), pointer :: MAIRDEN(:) => null()  !< Moist air density [kg/m3]
      real(fp), pointer :: OMEGA(:) => null()  !< Vertical velocity [Pa/s]
      real(fp), pointer :: PEDGE(:) => null()  !< Edge pressure [Pa]
      real(fp), pointer :: PEDGE_DRY(:) => null()  !< Dry edge pressure [Pa]
      real(fp), pointer :: PFICU(:) => null()  !< Conv ice precip flux [kg/m2/s]
      real(fp), pointer :: PFILSAN(:) => null()  !< LS ice precip flux [kg/m2/s]
      real(fp), pointer :: PFLCU(:) => null()  !< Conv liq precip flux [kg/m2/s]
      real(fp), pointer :: PFLLSAN(:) => null()  !< LS liq precip flux [kg/m2/s]
      real(fp), pointer :: PMID(:) => null()  !< Mid-level pressure [Pa]
      real(fp), pointer :: PMID_DRY(:) => null()  !< Dry mid pressure [Pa]
      real(fp), pointer :: QI(:) => null()  !< Ice water [kg/kg]
      real(fp), pointer :: QL(:) => null()  !< Liquid water [kg/kg]
      real(fp), pointer :: QV(:) => null()  !< Water vapor [kg/kg]
      real(fp), pointer :: RH(:) => null()  !< Relative humidity [%]
      real(fp), pointer :: SPHU(:) => null()  !< Specific humidity [kg/kg]
      real(fp), pointer :: T(:) => null()  !< Temperature [K]
      real(fp), pointer :: TAUCLI(:) => null()  !< Ice cloud optical depth [1]
      real(fp), pointer :: TAUCLW(:) => null()  !< Water cloud optical depth [1]
      real(fp), pointer :: THETA(:) => null()  !< Potential temperature [K]
      real(fp), pointer :: TV(:) => null()  !< Virtual temperature [K]
      real(fp), pointer :: U(:) => null()  !< U-wind [m/s]
      real(fp), pointer :: V(:) => null()  !< V-wind [m/s]
      real(fp), pointer :: Z(:) => null()  !< Geopotential height [m]
      real(fp), pointer :: ZMID(:) => null()  !< Mid-level height [m]

      ! 3D categorical fields (NOT vertical levels) - pointers to MetState data
      ! WARNING: These have different third dimensions (nsoil, nlanduse, etc.)
      ! Do NOT iterate with nlev - use appropriate category counts!
      real(fp), pointer :: FRLAI(:) => null()  !< LAI per land use type [m2/m2] (nlanduse categories)
      real(fp), pointer :: FRLANDUSE(:) => null()  !< Fractional land use [1] (nlanduse categories)
      real(fp), pointer :: FRSOIL(:) => null()  !< Fractional soil [1] (nsoil categories)
      real(fp), pointer :: FRZ0(:) => null()  !< Roughness per land use [m] (nlanduse categories)
      real(fp), pointer :: SOILM(:) => null()  !< Volumetric soil moisture [m3/m3] (nsoil layers)

      ! 2D surface fields (scalars for the column) - values copied from MetState
      real(fp) :: ALBD_NIR = 0.0_fp  !< ALBD_NIR field
      real(fp) :: ALBD_UV = 0.0_fp  !< ALBD_UV field
      real(fp) :: ALBD_VIS = 0.0_fp  !< ALBD_VIS field
      real(fp) :: AREA_M2 = 0.0_fp  !< Grid area [m2]
      real(fp) :: CLAYFRAC = 0.0_fp  !< CLAYFRAC field
      real(fp) :: CLDFRC = 0.0_fp  !< CLDFRC field
      real(fp) :: CNV_FRC = 0.0_fp  !< CNV_FRC field
      real(fp) :: CONV_DEPTH = 0.0_fp  !< CONV_DEPTH field
      real(fp) :: EFLUX = 0.0_fp  !< Latent heat flux [W/m2]
      real(fp) :: FLASH_DENS = 0.0_fp  !< FLASH_DENS field
      real(fp) :: FRLAKE = 0.0_fp  !< FRLAKE field
      real(fp) :: FRLAND = 0.0_fp  !< Land fraction [1]
      real(fp) :: FRLANDIC = 0.0_fp  !< FRLANDIC field
      real(fp) :: FROCEAN = 0.0_fp  !< Ocean fraction [1]
      real(fp) :: FRSEAICE = 0.0_fp  !< Sea ice fraction [1]
      real(fp) :: FRSNO = 0.0_fp  !< FRSNO field
      real(fp) :: FRVEG = 0.0_fp  !< FRVEG field
      real(fp) :: GVF = 0.0_fp  !< GVF field
      real(fp) :: GWETROOT = 0.0_fp  !< GWETROOT field
      real(fp) :: GWETTOP = 0.0_fp  !< GWETTOP field
      real(fp) :: HFLUX = 0.0_fp  !< Sensible heat flux [W/m2]
      real(fp) :: LAI = 0.0_fp  !< Leaf area index [m2/m2]
      real(fp) :: LAT = 0.0_fp  !< LAT field
      real(fp) :: LON = 0.0_fp  !< LON field
      real(fp) :: OBK = 0.0_fp  !< OBK field
      real(fp) :: PARDF = 0.0_fp  !< PARDF field
      real(fp) :: PARDR = 0.0_fp  !< PARDR field
      real(fp) :: PBLH = 0.0_fp  !< PBL height [m]
      real(fp) :: PHIS = 0.0_fp  !< Surface geopotential [m2/s2]
      real(fp) :: PRECANV = 0.0_fp  !< Anvil precipitation [kg/m2/s]
      real(fp) :: PRECCON = 0.0_fp  !< Convective precipitation [kg/m2/s]
      real(fp) :: PRECLSC = 0.0_fp  !< Large-scale precipitation [kg/m2/s]
      real(fp) :: PS = 0.0_fp  !< Surface pressure [Pa]
      real(fp) :: PS_DRY = 0.0_fp  !< PS_DRY field
      real(fp) :: PS_WET = 0.0_fp  !< PS_WET field
      real(fp) :: QV2M = 0.0_fp  !< 2m water vapor [kg/kg]
      real(fp) :: RDRAG = 0.0_fp  !< RDRAG field
      real(fp) :: SANDFRAC = 0.0_fp  !< SANDFRAC field
      real(fp) :: SEAICE00 = 0.0_fp  !< SEAICE00 field
      real(fp) :: SEAICE10 = 0.0_fp  !< SEAICE10 field
      real(fp) :: SEAICE20 = 0.0_fp  !< SEAICE20 field
      real(fp) :: SEAICE30 = 0.0_fp  !< SEAICE30 field
      real(fp) :: SEAICE40 = 0.0_fp  !< SEAICE40 field
      real(fp) :: SEAICE50 = 0.0_fp  !< SEAICE50 field
      real(fp) :: SEAICE60 = 0.0_fp  !< SEAICE60 field
      real(fp) :: SEAICE70 = 0.0_fp  !< SEAICE70 field
      real(fp) :: SEAICE80 = 0.0_fp  !< SEAICE80 field
      real(fp) :: SEAICE90 = 0.0_fp  !< SEAICE90 field
      real(fp) :: SLP = 0.0_fp  !< Sea level pressure [Pa]
      real(fp) :: SNODP = 0.0_fp  !< SNODP field
      real(fp) :: SNOMAS = 0.0_fp  !< SNOMAS field
      real(fp) :: SSM = 0.0_fp  !< SSM field
      real(fp) :: SST = 0.0_fp  !< Sea surface temperature [K]
      real(fp) :: SUNCOS = 0.0_fp  !< Cosine solar zenith angle [1]
      real(fp) :: SUNCOSmid = 0.0_fp  !< SUNCOSmid field
      real(fp) :: SUNCOSsum = 0.0_fp  !< SUNCOSsum field
      real(fp) :: SWGDN = 0.0_fp  !< Downward SW radiation [W/m2]
      real(fp) :: SZAFACT = 0.0_fp  !< SZAFACT field
      real(fp) :: T2M = 0.0_fp  !< 2m temperature [K]
      real(fp) :: TO3 = 0.0_fp  !< Total ozone [DU]
      real(fp) :: TROPP = 0.0_fp  !< Tropopause pressure [Pa]
      real(fp) :: TS = 0.0_fp  !< Surface temperature [K]
      real(fp) :: TSKIN = 0.0_fp  !< Skin temperature [K]
      real(fp) :: TropHt = 0.0_fp  !< TropHt field
      real(fp) :: U10M = 0.0_fp  !< 10m U-wind [m/s]
      real(fp) :: USTAR = 0.0_fp  !< Friction velocity [m/s]
      real(fp) :: USTAR_THRESHOLD = 0.0_fp  !< USTAR_THRESHOLD field
      real(fp) :: V10M = 0.0_fp  !< 10m V-wind [m/s]
      real(fp) :: WILT = 0.0_fp  !< WILT field
      real(fp) :: Z0 = 0.0_fp  !< Roughness length [m]
      real(fp) :: Z0H = 0.0_fp  !< Z0H field

   contains
      procedure :: cleanup => virtual_met_cleanup
   end type VirtualMetType


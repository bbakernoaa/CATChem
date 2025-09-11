! \file metstate_mod.F90
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
   USE GridGeometry_Mod
   ! USE TimeState_Mod, only: TimeStateType



   IMPLICIT NONE
   PRIVATE

   PUBLIC :: MetStateType           ! Main data type

   !=========================================================================
   ! Derived type for Meteorology State
   !=========================================================================

   ! \brief Derived type for Meteorology State
   !!
   !! Contains all meteorological state variables for CATChem including
   !! land, radiation, flux, cloud, and state-related fields. Use type-bound
   !! procedures for initialization, cleanup, validation, and memory usage.
   !!
   !! \ingroup core_modules
   !!!>
   TYPE, PUBLIC :: MetStateType
      CHARACTER(LEN=3)             :: State     = 'MET'    !< Name of this state
      INTEGER                      :: NLEVS             !< Number of vertical levels
      TYPE(GridGeometryType) :: geometry
      INTEGER                      :: NSURFTYPE         !< Number of surface types
      ! Grid flags (2D: nx, ny)
      LOGICAL, ALLOCATABLE         :: IsLand(:,:)       !< Is this a land grid box?
      LOGICAL, ALLOCATABLE         :: IsWater(:,:)      !< Is this a water grid box?
      LOGICAL, ALLOCATABLE         :: IsIce(:,:)        !< Is this an ice grid box?
      LOGICAL, ALLOCATABLE         :: IsSnow(:,:)       !< Is this a snow grid box?
      ! Vertical flags and arrays (3D: nx, ny, nz)
      LOGICAL,  ALLOCATABLE        :: InStratMeso(:,:,:)    !< Are we in the stratosphere or mesosphere?
      LOGICAL,  ALLOCATABLE        :: InStratosphere(:,:,:) !< Are we in the stratosphere?
      LOGICAL,  ALLOCATABLE        :: InTroposphere(:,:,:)  !< Are we in the troposphere?
      LOGICAL,  ALLOCATABLE        :: InPbl(:,:,:)          !< Are we in the PBL?
      LOGICAL,  ALLOCATABLE        :: IsLocalNoon(:,:)      !< Is it local noon (between 11 and 13 local solar time)?
      ! Surface properties (2D: nx, ny)
      REAL(fp), ALLOCATABLE        :: AREA_M2(:,:)      !< Grid box surface area [m2]
      INTEGER,  ALLOCATABLE        :: LWI(:,:)          !< Land water ice mask (0-sea, 1-land, 2-ice)
      INTEGER,  ALLOCATABLE        :: DLUSE(:,:)        !< Dominant land-use type
      REAL(fp), ALLOCATABLE        :: FRVEG(:,:)        !< Fraction of veg [1]
      REAL(fp), ALLOCATABLE        :: FRLAKE(:,:)       !< Fraction of lake [1]
      REAL(fp), ALLOCATABLE        :: FRLAND(:,:)       !< Fraction of land [1]
      REAL(fp), ALLOCATABLE        :: FRLANDIC(:,:)     !< Fraction of land ice [1]
      REAL(fp), ALLOCATABLE        :: FROCEAN(:,:)      !< Fraction of ocean [1]
      REAL(fp), ALLOCATABLE        :: FRSEAICE(:,:)     !< Sfc sea ice fraction
      REAL(fp), ALLOCATABLE        :: FRSNO(:,:)        !< Sfc snow fraction
      REAL(fp), ALLOCATABLE        :: LAI(:,:)          !< Leaf area index [m2/m2] (online) Dominant
      REAL(fp), ALLOCATABLE        :: GVF(:,:)          !< Green Vegetative Fraction
      ! Dust Only Variables
      REAL(fp), ALLOCATABLE        :: RDRAG(:,:)        !< Drag Partition [1]
      REAL(fp), ALLOCATABLE        :: USTAR_THRESHOLD(:,:) !< Threshold friction velocity [m/s]
      REAL(fp), ALLOCATABLE        :: SSM(:,:)          !< Sediment Supply Map [1]
      ! Surface and ice properties (2D: nx, ny)
      REAL(fp), ALLOCATABLE        :: SEAICE00(:,:)     !< Sea ice coverage 00-10%
      REAL(fp), ALLOCATABLE        :: SEAICE10(:,:)     !< Sea ice coverage 10-20%
      REAL(fp), ALLOCATABLE        :: SEAICE20(:,:)     !< Sea ice coverage 20-30%
      REAL(fp), ALLOCATABLE        :: SEAICE30(:,:)     !< Sea ice coverage 30-40%
      REAL(fp), ALLOCATABLE        :: SEAICE40(:,:)     !< Sea ice coverage 40-50%
      REAL(fp), ALLOCATABLE        :: SEAICE50(:,:)     !< Sea ice coverage 50-60%
      REAL(fp), ALLOCATABLE        :: SEAICE60(:,:)     !< Sea ice coverage 60-70%
      REAL(fp), ALLOCATABLE        :: SEAICE70(:,:)     !< Sea ice coverage 70-80%
      REAL(fp), ALLOCATABLE        :: SEAICE80(:,:)     !< Sea ice coverage 80-90%
      REAL(fp), ALLOCATABLE        :: SEAICE90(:,:)     !< Sea ice coverage 90-100%
      REAL(fp), ALLOCATABLE        :: SNODP(:,:)        !< Snow depth [m]
      REAL(fp), ALLOCATABLE        :: SNOMAS(:,:)       !< Snow mass [kg/m2]

      ! Soil and land use arrays (2D for counts, 3D for fractions)
      INTEGER,  ALLOCATABLE        :: DSOILTYPE(:,:)    !< Dominant soil type
      REAL(fp), ALLOCATABLE        :: CLAYFRAC(:,:)     !< Fraction of clay [1]
      REAL(fp), ALLOCATABLE        :: SANDFRAC(:,:)     !< Fraction of sand [1]
      INTEGER,  ALLOCATABLE        :: nLNDTYPE(:,:)     !< # of landtypes in box (I,J)
      REAL(fp), ALLOCATABLE        :: GWETTOP(:,:)      !< Top soil moisture [1]
      REAL(fp), ALLOCATABLE        :: GWETROOT(:,:)     !< Root Zone soil moisture [1]
      REAL(fp), ALLOCATABLE        :: WILT(:,:)         !< Wilt point [1]
      INTEGER                      :: nSOIL             !< # number of soil layers
      INTEGER                      :: nSOILTYPE         !< # number of soil types
      REAL(fp), ALLOCATABLE        :: SOILM(:,:,:)      !< Volumetric Soil moisture [m3/m3] (nx,ny,nsoil)
      REAL(fp), ALLOCATABLE        :: FRLANDUSE(:,:,:)  !< Fractional Land Use (nx,ny,nlanduse)
      REAL(fp), ALLOCATABLE        :: FRSOIL(:,:,:)     !< Fractional Soil (nx,ny,nsoil)
      REAL(fp), ALLOCATABLE        :: FRLAI(:,:,:)      !< LAI in each Fractional Land use type [m2/m2] (nx,ny,nlanduse)
      ! Location arrays (1D for single point, 2D for grid)
      real(fp), ALLOCATABLE        :: LAT(:,:)         !< Latitude
      real(fp), ALLOCATABLE        :: LON(:,:)         !< Longitude
      character(len=20), ALLOCATABLE :: LUCNAME(:,:)   !< name of land use category
      ! Surface meteorological properties (2D: nx, ny)
      REAL(fp), ALLOCATABLE        :: ALBD_VIS(:,:)     !< Visible surface albedo [1]
      REAL(fp), ALLOCATABLE        :: ALBD_NIR(:,:)     !< Near-IR surface albedo [1]
      REAL(fp), ALLOCATABLE        :: ALBD_UV(:,:)      !< UV surface albedo [1]
      REAL(fp), ALLOCATABLE        :: PARDR(:,:)        !< Direct photsynthetically active radiation [W/m2]
      REAL(fp), ALLOCATABLE        :: PARDF(:,:)        !< Diffuse photsynthetically active radiation [W/m2]
      REAL(fp), ALLOCATABLE        :: SUNCOS(:,:)       !< COS(solar zenith angle) at current time
      REAL(fp), ALLOCATABLE        :: SUNCOSmid(:,:)    !< COS(solar zenith angle) at midpoint of chem timestep
      REAL(fp), ALLOCATABLE        :: SUNCOSsum(:,:)    !< Sum of COS(SZA) for HEMCO OH diurnal variability
      REAL(fp), ALLOCATABLE        :: SZAFACT(:,:)      !< Diurnal scale factor for HEMCO OH diurnal variability (computed) [1]
      REAL(fp), ALLOCATABLE        :: SWGDN(:,:)        !< Incident radiation @ ground [W/m2]
      REAL(fp), ALLOCATABLE        :: EFLUX(:,:)        !< Latent heat flux [W/m2]
      REAL(fp), ALLOCATABLE        :: HFLUX(:,:)        !< Sensible heat flux [W/m2]
      REAL(fp), ALLOCATABLE        :: U10M(:,:)         !< E/W wind speed @ 10m ht [m/s]
      REAL(fp), ALLOCATABLE        :: USTAR(:,:)        !< Friction velocity [m/s]
      REAL(fp), ALLOCATABLE        :: V10M(:,:)         !< N/S wind speed @ 10m ht [m/s]
      REAL(fp), ALLOCATABLE        :: Z0(:,:)           !< Surface roughness height [m]
      REAL(fp), ALLOCATABLE        :: Z0H(:,:)          !< Surface roughness height, for heat (thermal roughness) [m]
      REAL(fp), ALLOCATABLE        :: FRZ0(:,:,:)       !< Aerodynamic Roughness Length per FRLANDUSE (nx,ny,nlanduse)
      REAL(fp), ALLOCATABLE        :: PBLH(:,:)         !< PBL height [m]
      ! 3D volumetric fields (3D: nx, ny, nz)
      REAL(fp), ALLOCATABLE        :: F_OF_PBL(:,:,:)       !< Fraction of box within PBL [1]
      REAL(fp), ALLOCATABLE        :: F_UNDER_PBLTOP(:,:,:) !< Fraction of box under PBL top
      real(fp), ALLOCATABLE        :: OBK(:,:)          !< Monin-Obhukov length [m]
      ! Cloud and precipitation properties (2D for surface, 3D for volumetric)
      REAL(fp), ALLOCATABLE        :: CLDFRC(:,:)       !< Column cloud fraction [1]
      REAL(fp), ALLOCATABLE        :: CONV_DEPTH(:,:)   !< Convective cloud depth [m]
      REAL(fp), ALLOCATABLE        :: FLASH_DENS(:,:)   !< Lightning flash density [#/km2/s]
      REAL(fp), ALLOCATABLE        :: CNV_FRC(:,:)      !< Convective fraction [1]
      REAL(fp), ALLOCATABLE        :: CLDF(:,:,:)       !< 3-D cloud fraction [1]
      REAL(fp), ALLOCATABLE        :: CMFMC(:,:,:)      !< Cloud mass flux [kg/m2/s]
      REAL(fp), ALLOCATABLE        :: DQRCU(:,:,:)      !< Conv precip production rate [kg/kg/s] (assume per dry air)
      REAL(fp), ALLOCATABLE        :: DQRLSAN(:,:,:)    !< LS precip prod rate [kg/kg/s] (assume per dry air)
      REAL(fp), ALLOCATABLE        :: DTRAIN(:,:,:)     !< Detrainment flux [kg/m2/s]
      REAL(fp), ALLOCATABLE        :: PRECANV(:,:)      !< Anvil previp @ ground [kg/m2/s] -> [mm/day]
      REAL(fp), ALLOCATABLE        :: PRECCON(:,:)      !< Conv  precip @ ground [kg/m2/s] -> [mm/day]
      REAL(fp), ALLOCATABLE        :: PRECLSC(:,:)      !< Large-scale precip @ ground kg/m2/s] -> [mm/day]
      ! 3D cloud and precipitation arrays
      REAL(fp), ALLOCATABLE        :: QI(:,:,:)         !< Mass fraction of cloud ice water [kg/kg dry air]
      REAL(fp), ALLOCATABLE        :: QL(:,:,:)         !< Mass fraction of cloud liquid water [kg/kg dry air]
      REAL(fp), ALLOCATABLE        :: PFICU(:,:,:)      !< Dwn flux ice prec:conv [kg/m2/s]
      REAL(fp), ALLOCATABLE        :: PFILSAN(:,:,:)    !< Dwn flux ice prec:LS+anv [kg/m2/s]
      REAL(fp), ALLOCATABLE        :: PFLCU(:,:,:)      !< Dwn flux liq prec:conv [kg/m2/s]
      REAL(fp), ALLOCATABLE        :: PFLLSAN(:,:,:)    !< Dwn flux ice prec:LS+anv [kg/m2/s]
      REAL(fp), ALLOCATABLE        :: TAUCLI(:,:,:)     !< Opt depth of ice clouds [1]
      REAL(fp), ALLOCATABLE        :: TAUCLW(:,:,:)     !< Opt depth of H2O clouds [1]
      ! Surface scalars (now 2D: nx, ny)
      REAL(fp), ALLOCATABLE        :: PHIS(:,:)         !< Surface geopotential height [m2/s2]
      REAL(fp), ALLOCATABLE        :: PS_WET(:,:)       !< Wet surface pressure at start of timestep [hPa]
      REAL(fp), ALLOCATABLE        :: PS_DRY(:,:)       !< Dry surface pressure at start of timestep [hPa]
      REAL(fp), ALLOCATABLE        :: QV2M(:,:)         !< Specific Humidity at 2m [kg/kg]
      REAL(fp), ALLOCATABLE        :: T2M(:,:)          !< Temperature 2m [K]
      REAL(fp), ALLOCATABLE        :: TS(:,:)           !< Surface temperature [K]
      REAL(fp), ALLOCATABLE        :: TSKIN(:,:)        !< Surface skin temperature [K]
      REAL(fp), ALLOCATABLE        :: SST(:,:)          !< Sea surface temperature [K]
      REAL(fp), ALLOCATABLE        :: SLP(:,:)          !< Sea level pressure [hPa]
      REAL(fp), ALLOCATABLE        :: PS(:,:)           !< Surface Pressure [hPa]
      REAL(fp), ALLOCATABLE        :: TO3(:,:)          !< Total overhead O3 column [DU]
      REAL(fp), ALLOCATABLE        :: TROPP(:,:)        !< Tropopause pressure [hPa]
      INTEGER,  ALLOCATABLE        :: TropLev(:,:)      !< Tropopause level [1]
      REAL(fp), ALLOCATABLE        :: TropHt(:,:)       !< Tropopause height [km]
      ! 3D atmospheric variables (3D: nx, ny, nz)
      REAL(fp), ALLOCATABLE        :: Z(:,:,:)          !< Full Layer Geopotential Height
      REAL(fp), ALLOCATABLE        :: ZMID(:,:,:)       !< Mid Layer Geopotential Height
      REAL(fp), ALLOCATABLE        :: BXHEIGHT(:,:,:)   !< Grid box height [m] (dry air)
      REAL(fp), ALLOCATABLE        :: QV(:,:,:)         !< Specific Humidity [kg/kg]
      REAL(fp), ALLOCATABLE        :: T(:,:,:)          !< Temperature [K]
      REAL(fp), ALLOCATABLE        :: THETA(:,:,:)      !< Potential temperature [K]
      REAL(fp), ALLOCATABLE        :: TV(:,:,:)         !< Virtual temperature [K]
      REAL(fp), ALLOCATABLE        :: V(:,:,:)          !< N/S component of wind [m s-1]
      REAL(fp), ALLOCATABLE        :: U(:,:,:)          !< E/W component of wind [m s-1]
      REAL(fp), ALLOCATABLE        :: OMEGA(:,:,:)      !< Updraft velocity [Pa/s]
      REAL(fp), ALLOCATABLE        :: RH(:,:,:)         !< Relative humidity [%]
      REAL(fp), ALLOCATABLE        :: SPHU(:,:,:)       !< Specific humidity [g H2O/kg tot air]
      REAL(fp), ALLOCATABLE        :: AIRDEN(:,:,:)     !< Dry air density [kg/m3]
      REAL(fp), ALLOCATABLE        :: AIRNUMDEN(:,:,:)  !< Dry air density [molec/cm3]
      REAL(fp), ALLOCATABLE        :: MAIRDEN(:,:,:)    !< Moist air density [kg/m3]
      REAL(fp), ALLOCATABLE        :: AVGW(:,:,:)       !< Water vapor volume mixing ratio [vol H2O/vol dry air]
      REAL(fp), ALLOCATABLE        :: DELP(:,:,:)       !< Delta-P (wet) across box [hPa]
      REAL(fp), ALLOCATABLE        :: DELP_DRY(:,:,:)   !< Delta-P (dry) across box [hPa]
      REAL(fp), ALLOCATABLE        :: DAIRMASS(:,:,:)   !< Dry air mass [kg] in grid box
      REAL(fp), ALLOCATABLE        :: AIRVOL(:,:,:)     !< Grid box volume [m3] (dry air)
      REAL(fp), ALLOCATABLE        :: PEDGE_DRY(:,:,:)  !< Dry air partial pressure @ level edges [hPa] (nx,ny,nz+1)
      REAL(fp), ALLOCATABLE        :: PEDGE(:,:,:)      !< Air partial pressure @ level edges [hPa] (nx,ny,nz+1)
      REAL(fp), ALLOCATABLE        :: PMID(:,:,:)       !< Average wet air pressure [hPa] defined as arithmetic average of edge pressures
      REAL(fp), ALLOCATABLE        :: PMID_DRY(:,:,:)   !< Dry air partial pressure [hPa] defined as arithmetic avg of edge pressures
      contains
      procedure :: init => metstate_init
      procedure :: cleanup => metstate_cleanup
      procedure :: validate => metstate_validate
      procedure :: reset => metstate_reset
      procedure :: is_allocated => metstate_is_allocated
      procedure :: get_memory_usage => metstate_get_memory_usage
      procedure :: print_summary => metstate_print_summary
      procedure :: get_dimensions => metstate_get_dimensions
      procedure :: get_field_ptr => metstate_get_field_ptr
      procedure, public :: get_column_ptr_func => metstate_get_column_ptr_func
      procedure, public :: get_column_ptr => metstate_get_column_ptr_subroutine
      procedure, public :: get_2Dto0D_value => metstate_get_2Dto0D_value
      procedure, public :: get_scalar_value => metstate_get_scalar_value
      procedure :: allocate_field => metstate_allocate_field
      procedure :: deallocate_field => metstate_deallocate_field
      procedure, private :: allocate_arrays => allocate_metstate_arrays
   end type MetStateType

CONTAINS

   !> \brief Initialize a MetStateType object
   !!
   !! Initializes the meteorological state object, sets default values, and allocates required arrays.
   !!
   !! \param[inout] this      MetStateType object to initialize
   !! \param[in]    nx        Number of grid points in x direction
   !! \param[in]    ny        Number of grid points in y direction
   !! \param[in]    nlevs     Number of vertical levels
   !! \param[in]    nsoil     Number of soil layers
   !! \param[in]    nsoiltype Number of soil types
   !! \param[in]    nsurftype Number of surface types
   !! \param[inout] error_mgr Error manager for context and error reporting
   !! \param[out]   rc        Return code (CC_SUCCESS or error code)
   subroutine metstate_init(this, nx, ny, nlevs, nsoil, nsoiltype, nsurftype, error_mgr, rc)
      use error_mod, only: ErrorManagerType, CC_SUCCESS, ERROR_MEMORY_ALLOCATION

      implicit none
      class(MetStateType), intent(inout) :: this
      integer, intent(in) :: nx, ny, nlevs
      integer, intent(in) :: nsoil, nsoiltype, nsurftype
      type(ErrorManagerType), pointer, intent(inout) :: error_mgr
      integer, intent(out) :: rc
      character(len=256) :: thisLoc

      thisLoc = 'metstate_init (in core/metstate_mod.F90)'
      call error_mgr%push_context('metstate_init', 'initializing meteorological state')

      rc = CC_SUCCESS

      call this%geometry%set(nx, ny, nlevs) ! Add a set() method to GridGeometryType
      
      this%State = 'MET'

      ! Set soil and surface parameters
      this%NSURFTYPE = nsurftype
      
      ! Set soil parameters directly
      this%nSOIL = nsoil
      this%nSOILTYPE = nsoiltype

      ! Call helper procedure to allocate arrays
      call this%allocate_arrays('ALL', error_mgr, rc)

      call error_mgr%pop_context()
   end subroutine metstate_init

   !> \brief Allocate all arrays for MetStateType (optionally, only a specific field)
   !!
   !! Helper procedure to allocate and initialize arrays in the meteorological state.
   !!
   !! \param[inout] this      MetStateType object
   !! \param[in]    field_name Name of the field to allocate (or 'ALL' for all fields)
   !! \param[inout] error_mgr Error manager for context and error reporting
   !! \param[out]   rc        Return code
   subroutine allocate_metstate_arrays(this, field_name, error_mgr, rc)
      use error_mod, only: ErrorManagerType, CC_SUCCESS, ERROR_MEMORY_ALLOCATION

      implicit none
      class(MetStateType), intent(inout) :: this
      character(len=*), intent(in) :: field_name
      type(ErrorManagerType), pointer, intent(inout) :: error_mgr
      integer, intent(out) :: rc
      character(len=256) :: thisLoc
      integer :: nx, ny, nz, nsoil, nsoiltype, nSURFTYPE

      thisLoc = 'allocate_metstate_arrays (in core/metstate_mod.F90)'
      rc = CC_SUCCESS

      call this%geometry%get_dimensions(nx, ny, nz)
      
      ! Use the properly initialized values (no more defaults needed)
      nsoil = this%nSOIL
      nsoiltype = this%nSOILTYPE
      nSURFTYPE = this%NSURFTYPE

      ! Auto-allocate all REAL(fp), ALLOCATABLE fields (generated, now field-by-field)
      select case (trim(field_name))
#include "metstate_allocate_fields.inc"
      end select

      ! Initialize to safe defaults
      this%T = 288.15_fp
      this%U = 0.0_fp
      this%V = 0.0_fp
      this%QV = 0.001_fp
      this%RH = 50.0_fp
      this%AIRDEN = 1.2_fp
      this%BXHEIGHT = 100.0_fp
      this%PS = 1013.25_fp
      this%SLP = 1013.25_fp
      this%T2M = 288.15_fp
      this%TS = 288.15_fp

   end subroutine allocate_metstate_arrays

   !> \brief Deallocate and clean up all arrays in MetStateType (field-by-field)
   !!
   !! Deallocates the specified allocatable array and resets scalar values in the meteorological state.
   !!
   !! \param[inout] this MetStateType object
   !! \param[in]    field_name Name of the field to deallocate (or 'ALL' for all fields)
   !! \param[out]   rc   Return code (CC_SUCCESS)
   subroutine metstate_cleanup(this, field_name, rc)
      implicit none
      class(MetStateType), intent(inout) :: this
      character(len=*), intent(in) :: field_name
      integer, intent(out) :: rc

      rc = CC_SUCCESS

      ! Deallocate only the requested field (or all if 'ALL')
      select case (trim(field_name))
#include "metstate_deallocate_fields.inc"
      end select

      this%State = ''

   end subroutine metstate_cleanup

   !> \brief Validate the MetStateType object for consistency and physical reasonableness
   !!
   !! Checks that the number of levels is positive, temperatures and pressures are within physical ranges,
   !! and that required arrays are allocated.
   !!
   !! \param[in]    this      MetStateType object
   !! \param[inout] error_mgr Error manager for context and error reporting
   !! \param[out]   rc        Return code (CC_SUCCESS or error code)
   subroutine metstate_validate(this, error_mgr, rc)
      use error_mod, only: ErrorManagerType, CC_SUCCESS, ERROR_INVALID_INPUT
      use utilities_mod, only: is_valid_temperature, is_valid_pressure

      implicit none
      class(MetStateType), intent(in) :: this
      type(ErrorManagerType), pointer, intent(inout) :: error_mgr
      integer, intent(out) :: rc

      character(len=256) :: thisLoc

      thisLoc = 'metstate_validate (in core/metstate_mod.F90)'
      call error_mgr%push_context('metstate_validate', 'validating meteorological state')

      rc = CC_SUCCESS

      ! Check basic state
      if (this%NLEVS <= 0) then
         call error_mgr%report_error(ERROR_INVALID_INPUT, &
                                     'Number of levels must be positive', rc, &
                                     thisLoc, 'Set NLEVS to a positive integer')
         call error_mgr%pop_context()
         return
      endif

      ! Validate temperatures (use maxval/minval for array validation)
      if (allocated(this%T2M)) then
         if (maxval(this%T2M) > 400.0_fp .or. minval(this%T2M) < 100.0_fp) then
            call error_mgr%report_error(ERROR_INVALID_INPUT, &
                                        '2m temperature out of physical range', rc, &
                                        thisLoc, 'Check temperature units and values')
            call error_mgr%pop_context()
            return
         endif
      endif

      if (allocated(this%TS)) then
         if (maxval(this%TS) > 400.0_fp .or. minval(this%TS) < 100.0_fp) then
            call error_mgr%report_error(ERROR_INVALID_INPUT, &
                                        'Surface temperature out of physical range', rc, &
                                        thisLoc, 'Check temperature units and values')
            call error_mgr%pop_context()
            return
         endif
      endif

      ! Validate pressures (use maxval/minval for array validation)
      if (allocated(this%PS)) then
         if (maxval(this%PS) > 1200.0_fp .or. minval(this%PS) < 10.0_fp) then
            call error_mgr%report_error(ERROR_INVALID_INPUT, &
                                        'Surface pressure out of physical range', rc, &
                                        thisLoc, 'Check pressure units and values')
            call error_mgr%pop_context()
            return
         endif
      endif

      if (allocated(this%SLP)) then
         if (maxval(this%SLP) > 1200.0_fp .or. minval(this%SLP) < 500.0_fp) then
            call error_mgr%report_error(ERROR_INVALID_INPUT, &
                                        'Sea level pressure out of physical range', rc, &
                                        thisLoc, 'Check pressure units and values')
            call error_mgr%pop_context()
            return
         endif
      endif

      ! Check array allocation
      if (.not. this%is_allocated()) then
         call error_mgr%report_error(ERROR_INVALID_INPUT, &
                                     'Required arrays not allocated', rc, &
                                     thisLoc, 'Call init() before using MetState')
         call error_mgr%pop_context()
         return
      endif

      call error_mgr%pop_context()
   end subroutine metstate_validate

   !> \brief Reset MetStateType to initial values
   !!
   !! Resets time, surface fields, and arrays to standard atmosphere values.
   !!
   !! \param[inout] this MetStateType object
   !! \param[out]   rc   Return code (CC_SUCCESS)
   subroutine metstate_reset(this, rc)
      implicit none
      class(MetStateType), intent(inout) :: this
      integer, intent(out) :: rc

      rc = CC_SUCCESS

      ! Reset to standard atmosphere values
      if (allocated(this%T2M)) this%T2M = 288.15_fp
      if (allocated(this%TS)) this%TS = 288.15_fp
      if (allocated(this%TSKIN)) this%TSKIN = 288.15_fp
      if (allocated(this%PS)) this%PS = 1013.25_fp
      if (allocated(this%SLP)) this%SLP = 1013.25_fp
      if (allocated(this%SST)) this%SST = 288.15_fp

      ! Reset arrays if allocated
      if (allocated(this%T)) this%T = 288.15_fp
      if (allocated(this%U)) this%U = 0.0_fp
      if (allocated(this%V)) this%V = 0.0_fp
      if (allocated(this%QV)) this%QV = 0.01_fp
      if (allocated(this%RH)) this%RH = 50.0_fp

   end subroutine metstate_reset

   !> \brief Check if required arrays are allocated in MetStateType
   !!
   !! Returns .true. if all required arrays are allocated, .false. otherwise.
   !!
   !! \param[in] this MetStateType object
   !! \return    Logical flag indicating allocation status
   function metstate_is_allocated(this) result(is_alloc)
      implicit none
      class(MetStateType), intent(in) :: this
      logical :: is_alloc

      is_alloc = allocated(this%T) .and. allocated(this%U) .and. allocated(this%V) .and. &
                 allocated(this%QV) .and. allocated(this%PMID) .and. allocated(this%DELP)
   end function metstate_is_allocated

   !> \brief Get approximate memory usage of MetStateType in bytes
   !!
   !! Estimates the memory usage of all arrays in the meteorological state object.
   !!
   !! \param[in] this MetStateType object
   !! \return    Estimated memory usage in bytes (integer(kind=8))
   function metstate_get_memory_usage(this) result(memory_bytes)
      implicit none
      class(MetStateType), intent(in) :: this
      integer(kind=8) :: memory_bytes

      integer :: nlevs

      memory_bytes = 0
      nlevs = this%NLEVS

      if (nlevs > 0) then
         ! Estimate based on number of allocated arrays and precision
         ! Each real(fp) array: nlevs * 8 bytes (assuming fp = real64)
         ! Each logical array: nlevs * 1 byte
         memory_bytes = nlevs * 8 * 26  ! 26 real arrays
         memory_bytes = memory_bytes + nlevs * 1 * 4  ! 4 logical arrays
         memory_bytes = memory_bytes + (nlevs+1) * 8 * 2  ! 2 edge arrays
      endif
   end function metstate_get_memory_usage

   !> \brief Print a summary of the MetStateType object to standard output
   !!
   !! Prints key fields, allocation status, and memory usage for diagnostics.
   !!
   !! \param[in] this MetStateType object
   subroutine metstate_print_summary(this)
      implicit none
      class(MetStateType), intent(in) :: this

      write(*,'(A)') '=== MetState Summary ==='
      write(*,'(A,A)') 'State: ', trim(this%State)
      write(*,'(A,I0)') 'Number of levels: ', this%NLEVS
      if (allocated(this%TS)) then
         write(*,'(A,F8.2,A)') 'Surface temperature: ', this%TS(1,1), ' K'
      endif
      if (allocated(this%PS)) then
         write(*,'(A,F8.2,A)') 'Surface pressure: ', this%PS(1,1), ' hPa'
      endif
      if (allocated(this%SLP)) then
         write(*,'(A,F8.2,A)') 'Sea level pressure: ', this%SLP(1,1), ' hPa'
      endif
      write(*,'(A,L1)') 'Arrays allocated: ', this%is_allocated()
      write(*,'(A,I0,A)') 'Memory usage: ', this%get_memory_usage(), ' bytes'
      write(*,'(A)') '======================='
   end subroutine metstate_print_summary

   !========================================================================
   !! Get grid dimensions for column interface support
   !========================================================================
   subroutine metstate_get_dimensions(this, nx, ny, nlev)
      class(MetStateType), intent(in) :: this
      integer, intent(out) :: nx, ny, nlev

      ! Get actual dimensions from geometry
      call this%geometry%get_dimensions(nx, ny, nlev)

   end subroutine metstate_get_dimensions

!    !========================================================================
!    !! Get pointer to a meteorological field array by name
!    !!
!    !! Returns a pointer to the requested 1D field array (e.g., temperature, pressure) for the column model.
!    !! Returns null() if the field is not found or not allocated.
!    !!
!    !! \param[in]  this       MetStateType object
!    !! \param[in]  field_name Name of the field (e.g., 'T', 'temperature')
!    !! \return     Pointer to the requested field array, or null()
!    function metstate_get_field_ptr(this, field_name) result(field_ptr)
!       implicit none
!       class(MetStateType), intent(in), target :: this
!       character(len=*), intent(in) :: field_name
!       real(fp), pointer :: field_ptr(:)

!       ! Return pointer to 1D column data
!       field_ptr => null()

!       select case (trim(field_name))
! #include "metstate_accessor.inc"
!       end select

!    end function metstate_get_field_ptr

   !> \brief Allocate a specific field in MetStateType by name
   !!
   !! Calls the generated select-case macro to allocate only the requested field.
   !!
   !! \param[inout] this      MetStateType object
   !! \param[in]    field_name Name of the field to allocate
   !! \param[out]   rc         Return code (CC_SUCCESS or error code)
   subroutine metstate_allocate_field(this, field_name, rc)
      class(MetStateType), intent(inout) :: this
      character(len=*), intent(in) :: field_name
      integer, intent(out) :: rc
      integer :: nx, ny, nz, nsoil, nsoiltype, nSURFTYPE
      rc = CC_SUCCESS
      call this%geometry%get_dimensions(nx, ny, nz)
      ! Use the properly initialized values (no more defaults needed) 
      nsoil = this%nSOIL
      nsoiltype = this%nSOILTYPE
      nSURFTYPE = this%NSURFTYPE
      ! Only allocate the requested field
      select case (trim(field_name))
#include "metstate_allocate_fields.inc"
      end select
   end subroutine metstate_allocate_field

   !> \brief Deallocate a specific field in MetStateType by name
   !!
   !! Calls the generated select-case macro to deallocate only the requested field.
   !!
   !! \param[inout] this      MetStateType object
   !! \param[in]    field_name Name of the field to deallocate
   !! \param[out]   rc         Return code (CC_SUCCESS or error code)
   subroutine metstate_deallocate_field(this, field_name, rc)
      class(MetStateType), intent(inout) :: this
      character(len=*), intent(in) :: field_name
      integer, intent(out) :: rc
      rc = CC_SUCCESS
      ! Only deallocate the requested field
      select case (trim(field_name))
#include "metstate_deallocate_fields.inc"
      end select
   end subroutine metstate_deallocate_field

   !> \brief Get a pointer to a vertical column for a given field name and (i,j) indices (type-safe)
   !!
   !! Calls the type-safe function version to obtain a real(fp) pointer, then assigns it to a polymorphic pointer for generic access.
   !! If the field is not found or not allocated, col_ptr is set to null and rc is set to CC_FAILURE.
   !!
   !! \param[inout] this      MetStateType object
   !! \param[in]    field_name Name of the field (e.g., 'T', 'temperature')
   !! \param[in]    i          Grid column index (1-based)
   !! \param[in]    j          Grid row index (1-based)
   !! \param[out]   col_ptr    Pointer to the vertical column data (polymorphic pointer)
   !! \param[out]   rc         Return code (CC_SUCCESS if found, CC_FAILURE otherwise)
   subroutine metstate_get_3Dto1D_ptr(this, field_name, i, j, col_ptr, rc)
      class(MetStateType), intent(inout) :: this
      character(len=*), intent(in) :: field_name
      integer, intent(in) :: i, j
      real(fp), pointer :: col_ptr(:)
      integer, intent(out) :: rc
      col_ptr => this%get_column_ptr_func(field_name, i, j)
      if (associated(col_ptr)) then
         rc = 0
      else
         rc = 1
      endif
   end subroutine metstate_get_3Dto1D_ptr

   !========================================================================
   !! Get pointer to a vertical column for a given field at (i,j)
   !! Returns a pointer to the vertical profile for a given field at grid location (i,j).
   !! For column models, returns the full 1D array.
   !! \param[in]  this       MetStateType object
   !! \param[in]  field_name Name of the field (e.g., 'T', 'temperature')
   !! \param[in]  i          Grid column index (optional, default 1)
   !! \param[in]  j          Grid row index (optional, default 1)
   !! \return     Pointer to vertical profile (1D)
   function metstate_get_column_ptr_func(this, field_name, i, j) result(column_ptr)
      implicit none
      class(MetStateType), intent(in), target :: this
      character(len=*), intent(in) :: field_name
      integer, intent(in), optional :: i, j
      real(fp), pointer :: column_ptr(:)
      integer :: nx, ny, nlev, col_i, col_j
      column_ptr => null()
      call this%get_dimensions(nx, ny, nlev)
      col_i = 1; col_j = 1
      if (present(i)) col_i = max(1, min(i, nx))
      if (present(j)) col_j = max(1, min(j, ny))
      select case (trim(field_name))
#include "metstate_column_accessor.inc"
      end select
   end function metstate_get_column_ptr_func

   !> Get a scalar value from a 2D field at (i,j)
   function metstate_get_2Dto0D_value(this, field_name, i, j) result(scalar_val)
      class(MetStateType), intent(in) :: this
      character(len=*), intent(in) :: field_name
      integer, intent(in) :: i, j
      real(fp) :: scalar_val
      integer :: col_i, col_j
      col_i = i
      col_j = j
      select case (trim(field_name))
#include "metstate_2d_scalar_accessor.inc"
      end select
   end function metstate_get_2Dto0D_value

   !> Get a scalar value from a scalar field
   function metstate_get_scalar_value(this, field_name) result(scalar_val)
      class(MetStateType), intent(in) :: this
      character(len=*), intent(in) :: field_name
      real(fp) :: scalar_val
      select case (trim(field_name))
#include "metstate_scalar_accessor.inc"
      end select
   end function metstate_get_scalar_value

   !> High-level interface: get any field (column, 2D, or scalar)
   subroutine metstate_get_field_ptr(this, field_name, i, j, col_ptr, scalar_val, rc)
      class(MetStateType), intent(in) :: this
      character(len=*), intent(in) :: field_name
      integer, intent(in), optional :: i, j
      real(fp), pointer, optional :: col_ptr(:)
      real(fp), optional :: scalar_val
      integer, intent(out) :: rc
      ! Try 3D column first
      if (present(col_ptr) .and. present(i) .and. present(j)) then
         col_ptr => this%get_column_ptr_func(field_name, i, j)
         if (associated(col_ptr)) then
               rc = 0
               return
         end if
      end if
      ! Try 2D scalar
      if (present(scalar_val) .and. present(i) .and. present(j)) then
         scalar_val = this%get_2Dto0D_value(field_name, i, j)
         rc = 0
         return
      end if
      ! Try scalar field
      if (present(scalar_val)) then
         scalar_val = this%get_scalar_value(field_name)
         rc = 0
         return
      end if
      rc = 1 ! Not found
   end subroutine metstate_get_field_ptr

   !> \brief Get a pointer to a vertical column for a given field name and (i,j) indices (subroutine version)
   !!
   !! This subroutine version provides the interface expected by StateManager_Mod.
   !! It handles different variable types (2D fields, 3D fields, scalar values) and returns
   !! a 1D pointer to the appropriate data.
   !!
   !! \param[inout] this      MetStateType object
   !! \param[in]    field_name Name of the field (e.g., 'T', 'temperature', 'PS')
   !! \param[in]    i          Grid column index (1-based)
   !! \param[in]    j          Grid row index (1-based)
   !! \param[out]   col_ptr    Pointer to the vertical column data (1D array)
   !! \param[out]   rc         Return code (CC_SUCCESS if found, CC_FAILURE otherwise)
   subroutine metstate_get_column_ptr_subroutine(this, field_name, i, j, col_ptr, rc)
      use error_mod, only: CC_SUCCESS, CC_FAILURE

      implicit none
      class(MetStateType), intent(inout), target :: this
      character(len=*), intent(in) :: field_name
      integer, intent(in) :: i, j
      real(fp), pointer :: col_ptr(:)
      integer, intent(out) :: rc

      ! Local variables for handling different field types
      real(fp), pointer :: temp_col_ptr(:)
      real(fp) :: scalar_val
      integer :: nx, ny, nlev

      rc = CC_FAILURE
      nullify(col_ptr)

      call this%get_dimensions(nx, ny, nlev)

      ! First try to get as a 3D field (vertical column)
      temp_col_ptr => this%get_column_ptr_func(field_name, i, j)
      if (associated(temp_col_ptr)) then
         col_ptr => temp_col_ptr
         rc = CC_SUCCESS
         return
      endif

      ! If not found as 3D field, try as 2D field and create a single-element array
      ! For 2D fields, we return a pointer to a single-element array containing the scalar value
      select case (trim(field_name))
      case ('PS', 'SLP', 'TS', 'T2M', 'TSKIN', 'SST', 'PHIS', 'PS_WET', 'PS_DRY', &
            'QV2M', 'AREA_M2', 'ALBD_VIS', 'ALBD_NIR', 'ALBD_UV', 'PARDR', 'PARDF', &
            'SUNCOS', 'SUNCOSmid', 'SWGDN', 'EFLUX', 'HFLUX', 'U10M', 'V10M', &
            'USTAR', 'Z0', 'Z0H', 'PBLH', 'OBK', 'CLDFRC', 'CONV_DEPTH', &
            'FLASH_DENS', 'CNV_FRC', 'PRECANV', 'PRECCON', 'PRECLSC', &
            'LAI', 'GVF', 'RDRAG', 'CLAYFRAC', 'SANDFRAC', 'FRVEG', 'FRLAKE', &
            'FRLAND', 'FRLANDIC', 'FROCEAN', 'FRSEAICE', 'FRSNO', 'SNODP', &
            'SNOMAS', 'SSM', 'USTAR_THRESHOLD', 'GWETTOP', 'GWETROOT', 'WILT', &
            'TO3', 'TROPP', 'TropHt', 'LAT', 'LON')

         scalar_val = this%get_2Dto0D_value(field_name, i, j)

         ! For 2D fields, we need to return a pointer to a single element
         ! This is a limitation of the current interface - we can't easily return a scalar as a 1D pointer
         ! For now, we'll return null and indicate failure
         rc = CC_FAILURE
         return

      case default
         ! Try as scalar field - similar limitation
         scalar_val = this%get_scalar_value(field_name)
         rc = CC_FAILURE
         return
      end select

   end subroutine metstate_get_column_ptr_subroutine

END MODULE MetState_Mod

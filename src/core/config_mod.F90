!> \file config_mod.F90
!!\brief Contains the ConfigType type and Read_Input_File subroutine
!!
!! This module contains subroutines and functions related to the ConfigType DataType of CATChem.
!! It includes subroutines for reading the configuration file.
!! \ingroup core_modules
!!!>
MODULE Config_Mod
!
! !USES:
!
   USE CharPak_Mod, ONLY : MaxDim  => MaxStrLen
   USE QfYaml_Mod
   USE Precision_Mod

   IMPLICIT NONE
   PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
   PUBLIC  :: Read_Input_File

!
! !DEFINED PARAMETERS:
!
   ! YAML configuration file name to be read
   CHARACTER(LEN=21), PARAMETER, PRIVATE :: configFile ='./CATChem_config.yml'

CONTAINS

   !> \brief Read the configuration file
   !!
   !! This subroutine reads the configuration file and populates the ConfigType object.
   !!
   !! \param Config The configuration options
   !! \param GridState The grid state containing information about the grid
   !! \param RC The return code
   !!
   !! \ingroup core_modules
   !!!>
   SUBROUTINE Read_Input_File( Config , GridState, RC )
!
! !USES:
!
      USE Error_Mod
      USE Config_Opt_Mod,  ONLY : ConfigType
      USE GridState_Mod, ONLY : GridStateType

!
! !INPUT/OUTPUT PARAMETERS:
!
      TYPE(ConfigType),    INTENT(INOUT) :: Config    ! Input options
      TYPE(GridStateType), INTENT(INOUT) :: GridState  ! Grid State object
!
! !OUTPUT PARAMETERS:
!
      INTEGER,        INTENT(OUT)   :: RC          ! Success or failure
!
! !LOCAL VARIABLES:
!
      ! Strings
      CHARACTER(LEN=18), PARAMETER :: configFile ='CATChem_config.yml' ! base configuration file
      ! Objects
      TYPE(QFYAML_t)     :: ConfigInput, ConfigAnchored

      ! Error handling
      CHARACTER(LEN=255) :: thisLoc ! where am i
      CHARACTER(LEN=512) :: errMsg  ! error message

      !========================================================================
      ! Read_Input_File begins here!
      !========================================================================

      ! Echo output
      IF ( Config %amIRoot ) THEN
         WRITE( 6, '(a  )' ) REPEAT( '=', 79 )
         WRITE( 6, '(a,/)' ) 'CATChem Initialization'
         WRITE( 6, 100   ) TRIM( configFile )
100      FORMAT( 'READ_INPUT_FILE: Opening ', a )
      ENDIF

      ! Assume success
      RC      = CC_SUCCESS
      errMsg  = ''
      thisLoc = ' -> at Read_Input_File (in module CATChem/src/core/input_mod.F90)'

      !========================================================================
      ! Read the YAML file into the Config object
      !========================================================================
      CALL QFYAML_Init( configFile, ConfigInput, ConfigAnchored, RC )
      IF ( RC /= CC_SUCCESS ) THEN
         errMsg = 'Error reading configuration file: ' // TRIM( configFile )
         CALL CC_Error( errMsg, RC, thisLoc )
         RETURN
      ENDIF

      !========================================================================
      ! Get basic simulation settings from the YAML Config object
      !========================================================================

      ! Simulation config settings
      CALL Config_Simulation( ConfigInput, Config, RC )
      IF ( RC /= CC_SUCCESS ) THEN
         errMsg = 'Error in "Config_Simulation"!'
         CALL CC_Error( errMsg, RC, thisLoc  )
         CALL QFYAML_CleanUp( ConfigInput )
         CALL QFYAML_CleanUp( ConfigAnchored )
         RETURN
      ENDIF

      !========================================================================
      ! Get grid settings from the YAML Config object
      !========================================================================

      ! Grid config settings
      CALL Config_Grid( ConfigInput, Config, GridState, RC )
      IF ( RC /= CC_SUCCESS ) THEN
         errMsg = 'Error in "Config_Grid"!'
         CALL CC_Error( errMsg, RC, thisLoc  )
         CALL QFYAML_CleanUp( ConfigInput         )
         CALL QFYAML_CleanUp( ConfigAnchored )
         RETURN
      ENDIF

      ! !========================================================================
      ! ! Config processes
      ! !========================================================================
      call Config_Process_SeaSalt(ConfigInput, Config, RC)
      IF ( RC /= CC_SUCCESS ) THEN
         errMsg = 'Error in "Config_Process_SeaSalt"!'
         CALL CC_Error( errMsg, RC, thisLoc  )
         CALL QFYAML_CleanUp( ConfigInput         )
         CALL QFYAML_CleanUp( ConfigAnchored )
         RETURN
      ENDIF

      call Config_Process_Dust(ConfigInput, Config, RC)
      IF ( RC /= CC_SUCCESS ) THEN
         errMsg = 'Error in "Config_Process_Dust"!'
         CALL CC_Error( errMsg, RC, thisLoc  )
         CALL QFYAML_CleanUp( ConfigInput         )
         CALL QFYAML_CleanUp( ConfigAnchored )
         RETURN
      ENDIF

      !========================================================================
      ! Further error-checking and initialization
      !========================================================================
      CALL QFYAML_CleanUp( ConfigInput )
      CALL QFYAML_CleanUp( ConfigAnchored )

   END SUBROUTINE Read_Input_File

   !> \brief Process simulation configuration
   !!
   !! This function processes the simulation configuration and performs the necessary actions based on the configuration.
   !!
   !! \param[in] ConfigInput The YAML configuration object
   !! \param[inout] Config The configuration object
   !! \param[out] RC The return code
   !!
   !! \ingroup core_modules
   !!!>
   SUBROUTINE Config_Simulation( ConfigInput, Config, RC )
!
! !USES:
!
      USE Charpak_Mod,   ONLY : To_UpperCase
      USE Error_Mod
      USE Config_Opt_Mod, ONLY : ConfigType
      ! USE Time_Mod
!
! !INPUT/OUTPUT PARAMETERS:
!
      TYPE(QFYAML_t), INTENT(INOUT) :: ConfigInput      ! YAML Config object
      TYPE(ConfigType), INTENT(INOUT) :: Config   ! Input Options object
!
! !OUTPUT PARAMETERS:
!
      INTEGER,        INTENT(OUT)   :: RC          ! Success or failure
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      ! Scalars
      LOGICAL                      :: v_bool
      INTEGER                      :: N,                C
      REAL(fp)                     :: JulianDateStart,  JulianDateEnd

      ! Arrays
      INTEGER                      :: a_int(2)

      ! Strings
      CHARACTER(LEN=6)             :: timeStr
      CHARACTER(LEN=8)             :: dateStr
      CHARACTER(LEN=12)            :: met
      CHARACTER(LEN=15)            :: verboseMsg
      CHARACTER(LEN=24)            :: sim
      CHARACTER(LEN=255)           :: thisLoc
      CHARACTER(LEN=512)           :: errMsg
      CHARACTER(LEN=QFYAML_NamLen) :: key
      CHARACTER(LEN=QFYAML_StrLen) :: v_str

      !========================================================================
      ! Config_Simulation begins here!
      !========================================================================

      ! Initialize
      RC      = CC_SUCCESS
      errMsg  = ''
      thisLoc = &
         ' -> at Config_Simulation (in module CATChem/src/core/input_mod.F90)'

      !------------------------------------------------------------------------
      ! Simulation type
      !------------------------------------------------------------------------
      key   = "simulation%name"
      v_str = MISSING_STR
      CALL QFYAML_Add_Get( ConfigInput, TRIM( key ), v_str, "", RC )
      IF ( RC /= CC_SUCCESS ) THEN
         errMsg = 'Error parsing ' // TRIM( key ) // '!'
         CALL CC_Error( errMsg, RC, thisLoc )
         RETURN
      ENDIF
      Config%SimulationName = TRIM( v_str )


      ! Return success
      RC = CC_SUCCESS

      !========================================================================
      ! Print to screen
      !========================================================================
!       IF ( Config%amIRoot ) THEN
!          WRITE( 6, 90  ) 'SIMULATION SETTINGS'
!          WRITE( 6, 95  ) '-------------------'
!          WRITE( 6, 110 ) 'Simulation name             : ',                     &
!             TRIM( Config%SimulationName )
!          WRITE( 6, 120 ) 'Turn on verbose output      : ',                     &
!             Config%Verbose
!          WRITE( 6, 110 ) 'Verbose output printed on   : ',                     &
!             TRIM( verboseMsg )
!       ENDIF

!       ! Format statements
! 90    FORMAT( /, A              )
! 95    FORMAT( A                 )
! 100   FORMAT( A, I8.8, 1X, I6.6 )
! 110   FORMAT( A, A              )
! 120   FORMAT( A, L5             )

   END SUBROUTINE Config_Simulation

   !> \brief Process grid configuration
   !!
   !! This function processes the grid configuration and performs the necessary actions based on the configuration.
   !!
   !! \param[in] ConfigInput The YAML configuration object
   !! \param[inout] Config The configuration object
   !! \param[out] RC The return code
   !!
   !! \ingroup core_modules
   !!!>
   SUBROUTINE Config_Grid( ConfigInput, Config, GridState, RC )
!
! !USES:
!
      USE CharPak_Mod,    ONLY : StrSplit
      USE Error_Mod
      USE Config_Opt_Mod,  ONLY : ConfigType
      USE GridState_Mod, ONLY : GridStateType
!
! !INPUT/OUTPUT PARAMETERS:
!
      TYPE(QFYAML_t),      INTENT(INOUT) :: ConfigInput      ! YAML Config object
      TYPE(ConfigType),     INTENT(INOUT) :: Config   ! Input options
      TYPE(GridStateType), INTENT(INOUT) :: GridState  ! Grid State
!
! !OUTPUT PARAMETERS:
!
      INTEGER,        INTENT(OUT)   :: RC          ! Success or failure
!
! !LOCAL VARIABLES:
!
      ! Scalars
      LOGICAL                      :: v_bool
      INTEGER                      :: v_int
      INTEGER                      :: nSubStrs
      INTEGER                      :: N
      INTEGER                      :: C

      ! Arrays
      INTEGER                      :: a_int(4)

      ! Strings
      CHARACTER(LEN=10)            :: xMin_Str, xMax_Str
      CHARACTER(LEN=10)            :: yMin_Str, yMax_Str
      CHARACTER(LEN=255)           :: thisLoc,  nLev
      CHARACTER(LEN=512)           :: errMsg
      CHARACTER(LEN=QFYAML_StrLen) :: key
      CHARACTER(LEN=QFYAML_StrLen) :: v_str

      ! String arrays
      CHARACTER(LEN=255)           :: subStrs(MAXDIM)
      CHARACTER(LEN=QFYAML_StrLen) :: a_str(2)

      !========================================================================
      ! Config_Grid begins here!
      !========================================================================

      ! Initialize
      RC      = CC_SUCCESS
      errMsg  = ''
      thisLoc = ' -> at Config_Grid (in CATChem/src/core/input_mod.F90)'

      !------------------------------------------------------------------------
      ! Level range
      !------------------------------------------------------------------------
      key   = "grid%number_of_levels"
      v_int = MISSING_INT
      CALL QFYAML_Add_Get( ConfigInput, TRIM( key ), v_int, "", RC )
      IF ( RC /= CC_SUCCESS ) THEN
         errMsg = 'Error parsing ' // TRIM( key ) // '!'
         CALL CC_Error( errMsg, RC, thisLoc )
         RETURN
      ENDIF
      GridState%number_of_levels = v_int

      !------------------------------------------------------------------------
      ! number of x and y dimensions (nx and ny)
      !------------------------------------------------------------------------
      key   = "grid%nx"
      v_int = MISSING_INT
      CALL QFYAML_Add_Get( ConfigInput, TRIM( key ), v_int, "", RC )
      IF ( RC /= CC_SUCCESS ) THEN
         errMsg = 'Error parsing ' // TRIM( key ) // '!'
         CALL CC_Error( errMsg, RC, thisLoc )
         RETURN
      ENDIF
      GridState%NX = v_int

      key   = "grid%ny"
      v_int = MISSING_INT
      CALL QFYAML_Add_Get( ConfigInput, TRIM( key ), v_int, "", RC )
      IF ( RC /= CC_SUCCESS ) THEN
         errMsg = 'Error parsing ' // TRIM( key ) // '!'
         CALL CC_Error( errMsg, RC, thisLoc )
         RETURN
      ENDIF
      GridState%NY = v_int

      ! Return success
      RC = CC_SUCCESS

   END SUBROUTINE Config_Grid

   !> \brief Process dust configuration
   !!
   !! This function processes the dust configuration and performs the necessary actions based on the configuration.
   !!
   !! \param[in] ConfigInput The YAML configuration object
   !! \param[inout] Config The configuration object
   !! \param[out] RC The return code
   !!
   !! \ingroup core_modules
   !!!>
   SUBROUTINE Config_Process_Dust( ConfigInput, Config, RC )
      USE CharPak_Mod,    ONLY : StrSplit
      USE Error_Mod
      USE Config_Opt_Mod,  ONLY : ConfigType

      TYPE(QFYAML_t),      INTENT(INOUT) ::ConfigInput      ! YAML Config object
      TYPE(ConfigType),     INTENT(INOUT) :: Config   ! Input options

      !
      ! !OUTPUT PARAMETERS:
      !
      INTEGER,        INTENT(OUT)   :: RC          ! Success or failure
      ! !LOCAL VARIABLES:
      !
      ! Scalars
      LOGICAL                      :: v_bool
      INTEGER                      :: v_int
      INTEGER                      :: nSubStrs
      INTEGER                      :: N
      INTEGER                      :: C

      ! Reals
      REAL(fp)                     :: v_real

      ! Arrays
      INTEGER                      :: a_int(4)

      ! Strings
      CHARACTER(LEN=10)            :: xMin_Str, xMax_Str
      CHARACTER(LEN=10)            :: yMin_Str, yMax_Str
      CHARACTER(LEN=255)           :: thisLoc,  nLev
      CHARACTER(LEN=512)           :: errMsg
      CHARACTER(LEN=QFYAML_StrLen) :: key
      CHARACTER(LEN=QFYAML_StrLen) :: v_str

      ! String arrays
      CHARACTER(LEN=255)           :: subStrs(MAXDIM)
      CHARACTER(LEN=QFYAML_StrLen) :: a_str(2)

      !========================================================================
      ! Config_Process_Dust begins here!
      !========================================================================

      ! Initialize
      RC      = CC_SUCCESS
      thisLoc = ' -> at Config_Process_Dust (in CATChem/src/core/config_mod.F90)'
      errMsg = ''

      key   = "process%dust%activate"
      v_bool = MISSING_BOOL
      CALL QFYAML_Add_Get( ConfigInput, TRIM( key ), v_bool, "", RC )
      IF ( RC /= CC_SUCCESS ) THEN
         errMsg = 'Error parsing ' // TRIM( key ) // '!'
         CALL CC_Error( errMsg, RC, thisLoc )
         RETURN
      ENDIF
      Config%Dust_Activate = v_bool

      key   = "process%dust%scheme_opt"
      v_int = MISSING_INT
      CALL QFYAML_Add_Get( ConfigInput, TRIM( key ), v_int, "", RC )
      IF ( RC /= CC_SUCCESS ) THEN
         errMsg = TRIM( key ) // 'Not Found, Setting Default to 1'
         RETURN
      ENDIF
      Config%dust_scheme = v_int

      key = 'process%dust%dust_drag_opt'
      v_int = MISSING_INT
      CALL QFYAML_Add_Get( ConfigInput, TRIM( key ), v_int, "", RC )
      IF ( RC /= CC_SUCCESS ) THEN
         errMsg = TRIM( key ) // 'Not Found, Setting Default to 1'
      ENDIF
      Config%dust_drag_opt = v_int

      key = 'process%dust%dust_moist_opt'
      v_int = MISSING_INT
      CALL QFYAML_Add_Get( ConfigInput, TRIM( key ), v_int, "", RC )
      IF ( RC /= CC_SUCCESS ) THEN
         errMsg = TRIM( key ) // 'Not Found, Setting Default to 1'
      ENDIF
      Config%dust_moist_opt = v_int

      key = 'process%dust%dust_horizflux_opt'
      v_int = MISSING_INT
      CALL QFYAML_Add_Get( ConfigInput, TRIM( key ), v_int, "", RC )
      IF ( RC /= CC_SUCCESS ) THEN
         errMsg = TRIM( key ) // 'Not Found, Setting Default to 1'
      ENDIF
      Config%dust_horizflux_opt = v_int

      key = 'process%dust%dust_alpha'
      v_real = MISSING_REAL
      CALL QFYAML_Add_Get( ConfigInput, TRIM( key ), v_real, "", RC )
      IF ( RC /= CC_SUCCESS ) THEN
         errMsg = TRIM( key ) // 'Not Found, Setting Default to 1'
      ENDIF
      ! write(*,*) v_real
      Config%dust_alpha = v_real

      key = 'process%dust%dust_beta'
      v_real = MISSING_REAL
      CALL QFYAML_Add_Get( ConfigInput, TRIM( key ), v_real, "", RC )
      IF ( RC /= CC_SUCCESS ) THEN
         errMsg = TRIM( key ) // 'Not Found, Setting Default to 1'
      ENDIF
      Config%dust_beta = v_real
      write(*,*) "Dust Configuration"
      write(*,*) '------------------------------------'
      write(*,*) 'Config%dust%activate = ', Config%dust_activate
      write(*,*) 'Config%dust%scheme_opt = ', Config%dust_scheme
      write(*,*) 'Config%dust%dust_drag_opt = ', Config%dust_drag_opt
      write(*,*) 'Config%dust%dust_moist_opt = ', Config%dust_moist_opt
      write(*,*) 'Config%dust%dust_horizflux_opt = ', Config%dust_horizflux_opt
      write(*,*) 'Config%dust%dust_alpha = ', Config%dust_alpha
      write(*,*) 'Config%dust%dust_beta = ', Config%dust_beta
      write(*,*) '------------------------------------'

   END SUBROUTINE Config_Process_Dust

   !> \brief Process seasalt configuration
   !!
   !! This function processes the seasalt configuration and performs the necessary actions based on the configuration.
   !!
   !! \param[in] ConfigInput The YAML configuration object
   !! \param[inout] Config The configuration object
   !! \param[out] RC The return code
   !!
   !! \ingroup core_modules
   !!!>
   SUBROUTINE Config_Process_SeaSalt( ConfigInput, Config, RC )
      USE CharPak_Mod,    ONLY : StrSplit
      USE Error_Mod
      USE Config_Opt_Mod,  ONLY : ConfigType

      TYPE(QFYAML_t),      INTENT(INOUT) ::ConfigInput      ! YAML Config object
      TYPE(ConfigType),     INTENT(INOUT) :: Config   ! Input options

      !
      ! !OUTPUT PARAMETERS:
      !
      INTEGER,        INTENT(OUT)   :: RC          ! Success or failure
      ! !LOCAL VARIABLES:
      !
      ! Scalars
      LOGICAL                      :: v_bool
      INTEGER                      :: v_int
      INTEGER                      :: nSubStrs
      INTEGER                      :: N
      INTEGER                      :: C

      ! Reals
      REAL(fp)                     :: v_real

      ! Arrays
      INTEGER                      :: a_int(4)

      ! Strings
      CHARACTER(LEN=10)            :: xMin_Str, xMax_Str
      CHARACTER(LEN=10)            :: yMin_Str, yMax_Str
      CHARACTER(LEN=255)           :: thisLoc,  nLev
      CHARACTER(LEN=512)           :: errMsg
      CHARACTER(LEN=QFYAML_StrLen) :: key
      CHARACTER(LEN=QFYAML_StrLen) :: v_str

      ! String arrays
      CHARACTER(LEN=255)           :: subStrs(MAXDIM)
      CHARACTER(LEN=QFYAML_StrLen) :: a_str(2)

      !========================================================================
      ! Config_Process_SeaSalt begins here!
      !========================================================================

      ! Initialize
      RC      = CC_SUCCESS
      thisLoc = ' -> at Config_Process_SeaSalt (in CATChem/src/core/config_mod.F90)'
      errMsg = ''
      ! TODO #105 Fix reading of config file
      key   = "process%seasalt%activate"
      v_bool = MISSING_BOOL
      CALL QFYAML_Add_Get( ConfigInput, TRIM( key ), v_bool, "", RC )
      IF ( RC /= CC_SUCCESS ) THEN
         errMsg = 'Error parsing ' // TRIM( key ) // '!'
         CALL CC_Error( errMsg, RC, thisLoc )
         RETURN
      ENDIF
      Config%seasalt_activate = v_bool

      key   = "process%seasalt%weibull"
      v_bool = MISSING_BOOL
      CALL QFYAML_Add_Get( ConfigInput, TRIM( key ), v_bool, "", RC )
      IF ( RC /= CC_SUCCESS ) THEN
         errMsg = 'Error parsing ' // TRIM( key ) // '!'
         CALL CC_Error( errMsg, RC, thisLoc )
         RETURN
      ENDIF
      Config%seasalt_weibull = v_bool


      key   = "process%seasalt%scheme_opt"
      v_int = MISSING_INT
      CALL QFYAML_Add_Get( ConfigInput, TRIM( key ), v_int, "", RC )
      IF ( RC /= CC_SUCCESS ) THEN
         errMsg = TRIM( key ) // 'Not Found, Setting Default to 1'
         RETURN
      ENDIF
      Config%seasalt_scheme = v_int

      key = 'process%seasalt%scale_factor'
      v_real = MISSING_REAL
      CALL QFYAML_Add_Get( ConfigInput, TRIM( key ), v_real, "", RC )
      IF ( RC /= CC_SUCCESS ) THEN
         errMsg = TRIM( key ) // 'Not Found, Setting Default to 1'
      ENDIF
      ! write(*,*) v_real
      Config%seasalt_scalefactor = v_real

      write(*,*) "SeaSalt Configuration"
      write(*,*) '------------------------------------'
      write(*,*) 'Config%seasalt_activate = ', Config%seasalt_activate
      write(*,*) 'Config%seasalt_scheme = ', Config%seasalt_scheme
      write(*,*) 'Config%seasalt_weibull = ', Config%seasalt_weibull
      write(*,*) 'Config%seasalt_scalefactor = ', Config%seasalt_scalefactor
      write(*,*) '------------------------------------'

   END SUBROUTINE Config_Process_SeaSalt

END MODULE config_mod

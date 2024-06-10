!------------------------------------------------------------------------------
!BOP
!
! !MODULE: input_mod.F90
!
! !DESCRIPTION: Contains routines that read the CATChem configuration file at
!  the start of the run and pass the information to Input_Opt.
!\\
!\\
! !INTERFACE:
!
MODULE Input_Mod
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
  PUBLIC  :: Do_Error_Checks
  PUBLIC  :: Validate_Directories
  PUBLIC  :: Config_Simulation
  PUBLIC  :: Config_Grid
  PUBLIC  :: Config_Aerosol
  
!
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !DEFINED PARAMETERS:
!
  ! YAML configuration file name to be read
  CHARACTER(LEN=21), PARAMETER, PRIVATE :: configFile ='./CATChem_config.yml'

CONTAINS
!EOC
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: read_input_file
!
! !DESCRIPTION: Driver program for reading the CATChem input file.
!\\
!\\
! In an ESMF environment, all time steps (chemistry, convection, emissions,
! dynamics) must be specified externally before calling this routine.
! The time steps specified in the CATChem configuration file are ignored.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Read_Input_File( Input_Opt, State_Grid, RC )
!
! !USES:
!
    USE Error_Mod
    USE Input_Opt_Mod,  ONLY : OptInput
    USE State_Grid_Mod, ONLY : GrdState
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(OptInput), INTENT(INOUT) :: Input_Opt   ! Input options
    TYPE(GrdState), INTENT(INOUT) :: State_Grid  ! Grid State object
!
! !OUTPUT PARAMETERS:
!
    INTEGER,        INTENT(OUT)   :: RC          ! Success or failure
!
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    ! Strings
    CHARACTER(LEN=255) :: thisLoc
    CHARACTER(LEN=512) :: errMsg

    ! Objects
    TYPE(QFYAML_t)     :: Config, ConfigAnchored

    !========================================================================
    ! Read_Input_File begins here!
    !========================================================================

    ! Echo output
! LDH: Comment out this section for now
!    IF ( Input_Opt%amIRoot ) THEN
!       WRITE( 6, '(a  )' ) REPEAT( '=', 79 )
!       WRITE( 6, '(a,/)' ) 'C A T C H E M   U S E R   I N P U T'
!       WRITE( 6, 100   ) TRIM( configFile )
!100    FORMAT( 'READ_INPUT_FILE: Opening ', a )
!    ENDIF

    ! Assume success
!    RC      = CC_SUCCESS
!    errMsg  = ''
!    thisLoc = ' -> at Read_Input_File (in module input_mod.F90)'

    !========================================================================
    ! Read the YAML file into the Config object
    !========================================================================
    CALL QFYAML_Init( configFile, Config, ConfigAnchored, RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error reading configuration file: ' // TRIM( configFile )
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF

    !========================================================================
    ! Get basic simulation settings from the YAML Config object
    !========================================================================

    ! Simulation config settings
    CALL Config_Simulation( Config, Input_Opt, RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error in "Config_Simulation"!'
       CALL CC_Error( errMsg, RC, thisLoc  )
       CALL QFYAML_CleanUp( Config         )
       CALL QFYAML_CleanUp( ConfigAnchored )
       RETURN
    ENDIF

#if !(defined( EXTERNAL_GRID ) || defined( EXTERNAL_FORCING ))
    ! Grid config settings
    ! Skip if we are gettomg the grid from an external model
    CALL Config_Grid( Config, Input_Opt, State_Grid, RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error in "Config_Grid"!'
       CALL CC_Error( errMsg, RC, thisLoc  )
       CALL QFYAML_CleanUp( Config         )
       CALL QFYAML_CleanUp( ConfigAnchored )
       RETURN
    ENDIF
#endif


    !========================================================================
    ! Get settings for CATChem operations from the YAML Config object
    !========================================================================


    ! Aerosol settings
    CALL Config_Aerosol( Config, Input_Opt, RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error in "Config_Aerosol"!'
       CALL CC_Error( errMsg, RC, thisLoc  )
       CALL QFYAML_CleanUp( Config         )
       CALL QFYAML_CleanUp( ConfigAnchored )
       RETURN
    ENDIF




    !========================================================================
    ! Check CATCHEM timesteps
    !========================================================================
!    CALL Check_Time_Steps( Input_Opt, State_Grid, RC )
!    IF ( RC /= CC_SUCCESS ) THEN
!       errMsg = 'Error in "Check_Time_Steps"!'
!       CALL CC_Error( errMsg, RC, thisLoc )
!       RETURN
!    ENDIF

    !========================================================================
    ! Further error-checking and initialization
    !========================================================================
    CALL QFYAML_CleanUp( Config         )
    CALL QFYAML_CleanUp( ConfigAnchored )

  END SUBROUTINE Read_Input_File

  !EOC
!------------------------------------------------------------------------------
!                  CATChem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: config_simulation
!
! !DESCRIPTION: Copies simulation information from the Config object
!  to Input_Opt, and does necessary checks.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Config_Simulation( Config, Input_Opt, RC )
!
! !USES:
!
    USE Charpak_Mod,   ONLY : To_UpperCase
    USE Error_Mod
    USE Input_Opt_Mod, ONLY : OptInput
    USE Time_Mod
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(QFYAML_t), INTENT(INOUT) :: Config      ! YAML Config object
    TYPE(OptInput), INTENT(INOUT) :: Input_Opt   ! Input Options object
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
#if defined( ESMF_ ) || defined( MODEL_ )
    INTEGER                      :: H,       M,       S
    REAL(f4)                     :: init_UTC
#endif

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
     ' -> at Config_Simulation (in module input_mod.F90)'

    !------------------------------------------------------------------------
    ! Simulation type
    !------------------------------------------------------------------------
    key   = "simulation%name"
    v_str = MISSING_STR
    CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    Input_Opt%SimulationName = TRIM( v_str )

    ! Error check simulation name
    Sim = To_UpperCase( TRIM( Input_Opt%SimulationName ) )
    IF ( TRIM(Sim) /= 'AEROSOL'                                        .and. &
         TRIM(Sim) /= 'CARBON'                                         .and. &
         TRIM(Sim) /= 'CH4'                                            .and. &
         TRIM(Sim) /= 'CO2'                                            .and. &
         TRIM(Sim) /= 'FULLCHEM'                                       .and. &
         TRIM(Sim) /= 'HG'                                             .and. &
         TRIM(Sim) /= 'METALS'                                         .and. &
         TRIM(Sim) /= 'POPS'                                           .and. &
         TRIM(Sim) /= 'TAGCH4'                                         .and. &
         TRIM(Sim) /= 'TAGCO'                                          .and. &
         TRIM(Sim) /= 'TAGO3'                                          .and. &
         TRIM(Sim) /= 'TRANSPORTTRACERS' ) THEN
         
       errMsg = Trim( Input_Opt%SimulationName) // ' is not a'            // &
                ' valid simulation. Supported simulations are:'           // &
                ' aerosol, carbon, CH4, CO2, fullchem, Hg, Metals, POPs,' // &
                ' TransportTracers, TagCH4, TagCO, or TagO3.'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF

    ! Set simulation type flags in Input_Opt
    Input_Opt%ITS_AN_AEROSOL_SIM   = ( TRIM(Sim) == 'AEROSOL'               )
    Input_Opt%ITS_A_CARBON_SIM     = ( TRIM(Sim) == 'CARBON'                )
    Input_Opt%ITS_A_CH4_SIM        = ( TRIM(Sim) == 'CH4'                   )
    Input_Opt%ITS_A_CO2_SIM        = ( TRIM(Sim) == 'CO2'                   )
    Input_Opt%ITS_A_FULLCHEM_SIM   = ( TRIM(Sim) == 'FULLCHEM'              )
    Input_Opt%ITS_A_MERCURY_SIM    = ( TRIM(Sim) == 'HG'                    )
    Input_Opt%ITS_A_TRACEMETAL_SIM = ( TRIM(Sim) == 'METALS'                )
    Input_Opt%ITS_A_POPS_SIM       = ( TRIM(Sim) == 'POPS'                  )
    Input_Opt%ITS_A_TAGCH4_SIM     = ( TRIM(Sim) == 'TAGCH4'                )
    Input_Opt%ITS_A_TAGCO_SIM      = ( TRIM(Sim) == 'TAGCO'                 )
    Input_Opt%ITS_A_TAGO3_SIM      = ( TRIM(Sim) == 'TAGO3'                 )
    Input_Opt%ITS_A_TRACER_SIM     = ( TRIM(Sim) == 'TRANSPORTTRACERS'      )

    !------------------------------------------------------------------------
    ! Species database file
    !------------------------------------------------------------------------
    key   = "simulation%species_database_file"
    v_str = MISSING_STR
    CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    Input_Opt%SpcDataBaseFile = TRIM( v_str )

    !------------------------------------------------------------------------
    ! Species metadata output file
    !------------------------------------------------------------------------
    key   = "simulation%species_metadata_output_file"
    v_str = MISSING_STR
    CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    Input_Opt%SpcMetaDataOutFile = TRIM( v_str )

    !------------------------------------------------------------------------
    ! Turn on debug output
    !------------------------------------------------------------------------
    key    = "simulation%verbose%activate"
    v_bool = MISSING_BOOL
    CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    Input_Opt%VerboseRequested = v_bool

    !------------------------------------------------------------------------
    ! Which cores for verbose output: root or all?
    !------------------------------------------------------------------------
    key  = "simulation%verbose%on_cores"
    v_str = MISSING_STR
    CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    Input_Opt%VerboseOnCores = To_UpperCase( v_str )

    ! Should verbose output be printed only on root or on all cores?
    SELECT CASE ( TRIM( Input_Opt%VerboseOnCores ) )
       CASE( 'ROOT' )
          verboseMsg = 'root core only'
          Input_Opt%Verbose =                                                &
             ( Input_Opt%VerboseRequested .and. Input_Opt%amIRoot )    
       CASE( 'ALL' )
          verboseMsg = 'all cores'
          Input_Opt%Verbose = Input_Opt%VerboseRequested
       CASE DEFAULT
          errMsg = 'Invalid selection!' // NEW_LINE( 'a' ) //                &
               'simulation:verbose:on_cores must be either "root" or "all"'
          CALL CC_Error( errMsg, RC, thisLoc )
          RETURN
    END SELECT

#if defined( MODEL_GCHP ) || defined( MODEL_GEOS )
    !========================================================================
    !          %%%%%%% GCHP and NASA/GEOS (with ESMF & MPI) %%%%%%%
    !
    ! Because GCHP and NASA/GEOS use ESMF, we need to take the start & end
    ! dates from the ESMF resource file (GEOSCHEMchem_GridComp_mod.rc)
    ! instead of those in CATChem_config.yml.  Therefore, the following
    ! fields have already been defined in the GCHP_Chunk_Init routine
    ! (located in module Interfaces/GCHP/Chem_GridCompMod.F90):
    !
    ! (1) Input_Opt%NYMDb
    ! (2) Input_Opt%NHMSb
    ! (3) Input_Opt%NYMDe
    ! (4) Input_Opt%NYMDe
    !
    ! Pass these fields of Input_Opt to routine Accept_External_Date_Time,
    ! as well as the starting UTC value in hours.
    !========================================================================

    ! Make sure the starting date NYMDb is valid
    IF ( .not. Valid_Date( Input_Opt%NYMDb ) ) THEN
       WRITE( DateStr, '(i8.8)' ) Input_Opt%NYMDb
       errMsg = 'Input%Opt%NYMDb = ' // DateStr // ' is not a valid '     // &
                'calendar date!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF

    ! Make sure the starting time NHMSb is valid
    IF ( .not. Valid_Time( Input_Opt%NHMSb ) ) THEN
       WRITE( TimeStr, '(i6.6)' ) Input_Opt%NHMSb
       errMsg = 'Input%Opt%NHMSb = ' // TimeStr // ' is not a valid '     // &
                'clock time!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF

    ! Make sure the ending date NYMDe is valid
    IF ( .not. Valid_Date( Input_Opt%NYMDe ) ) THEN
       WRITE( DateStr, '(i8.8)' ) Input_Opt%NYMDe
       errMsg = 'Input%Opt%NYMDe = ' // DateStr // ' is not a valid '     // &
                'calendar date!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF

    ! Make sure the ending time NHMSe is valid
    IF ( .not. Valid_Time( Input_Opt%NHMSe ) ) THEN
       WRITE( TimeStr, '(i6.6)' ) Input_Opt%NHMSe
       errMsg = 'Input%Opt%NHMSe = ' // TimeStr // ' is not a valid '     // &
                'clock time!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF

    ! Get the starting UTC time from Input_Opt%NHMSb for use below
    CALL YMD_Extract( Input_Opt%NHMSb, H, M, S )
    init_UTC = ( H + ( M / 60 ) + ( S / 3600 ) )

    ! Pass the values for the start & end times of the simulation directly
    ! to time_mod.F90 via subroutine ACCEPT_EXTERNAL_DATE_TIME.
    ! (bmy, 12/6/12)
    CALL Accept_External_Date_Time( value_NYMDb = Input_Opt%NYMDb,           &
                                    value_NHMSb = Input_Opt%NHMSb,           &
                                    value_NYMDe = Input_Opt%NYMDe,           &
                                    value_NHMSe = Input_Opt%NHMSe,           &
                                    value_NYMD  = Input_Opt%NYMDb,           &
                                    value_NHMS  = Input_Opt%NHMSb,           &
                                    value_UTC   = init_UTC,                  &
                                    RC          = RC                        )

    !------------------------------------------------------------------------
    ! Chemistry inputs folder (GCHP/NASA-GEOS only)
    !------------------------------------------------------------------------

    key   = "simulation%chem_inputs_dir"
    v_str = MISSING_STR
    CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    Input_Opt%CHEM_INPUTS_DIR = TRIM( v_str )

    !------------------------------------------------------------------------
    ! Set other fields of Input_Opt accordingly
    !------------------------------------------------------------------------
    Input_Opt%MetField        = 'See ExtData.rc'
    Input_Opt%DATA_DIR        = 'N/A'
    Input_Opt%RUN_DIR         = 'N/A'

#else
    !========================================================================
    !             %%%%%%% CATChem CLASSIC (with OpenMP) %%%%%%%
    !
    ! If we aren't using ESMF, read extra settings from CATChem_config.yml
    !========================================================================

    !------------------------------------------------------------------------
    ! Simulation start date
    !------------------------------------------------------------------------
    key   = "simulation%start_date"
    a_int = MISSING_INT
    CALL QFYAML_Add_Get( Config, TRIM( key ), a_int, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    Input_Opt%NYMDb = a_int(1)
    Input_Opt%NHMSb = a_int(2)

    ! Make sure the starting date NYMDb is valid
    IF ( .not. Valid_Date( Input_Opt%NYMDb ) ) THEN
       WRITE( DateStr, '(i8.8)' ) Input_Opt%NYMDb
       errMsg = 'Input%Opt%NYMDb = ' // DateStr        // &
                ' is not a valid calendar date!'       // &
                ' Please check your "CATChem_config.yml" file.'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF

    ! Make sure the starting time NHMSb is valid
    IF ( .not. Valid_Time( Input_Opt%NHMSb ) ) THEN
       WRITE( TimeStr, '(i6.6)' ) Input_Opt%NHMSb
       errMsg = 'Input%Opt%NHMSb = ' // TimeStr        // &
                ' is not a valid clock time!'          // &
                ' Please check your "CATChem_config.yml" file.'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF

    !------------------------------------------------------------------------
    ! Simulation end date
    !------------------------------------------------------------------------
    key   = "simulation%end_date"
    a_int = MISSING_INT
    CALL QFYAML_Add_Get( Config, TRIM( key ), a_int, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    Input_Opt%NYMDe = a_int(1)
    Input_Opt%NHMSe = a_int(2)

    ! Make sure the starting date NYMDb is valid
    IF ( .not. Valid_Date( Input_Opt%NYMDe ) ) THEN
       WRITE( DateStr, '(i8.8)' ) Input_Opt%NYMDe
       errMsg = 'Input%Opt%NYMDe = ' // DateStr        // &
                ' is not a valid calendar date!'       // &
                ' Please check your "CATChem_config.yml" file.'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF

    ! Make sure the ending time NHMSe is valid
    IF ( .not. Valid_Time( Input_Opt%NHMSe ) ) THEN
       WRITE( TimeStr, '(i6.6)' ) Input_Opt%NHMSe
       errMsg = 'Input%Opt%NHMSe = ' // TimeStr        // &
                ' is not a valid clock time!'          // &
                ' Please check your "CATChem_config.yml" file.'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF

    !------------------------------------------------------------------------
    ! Root data directory
    !------------------------------------------------------------------------
    key   = "simulation%root_data_dir"
    v_str = MISSING_STR
    CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    Input_Opt%Data_Dir = TRIM( v_str )

    ! Make sure DATA-DIR ends with a "/" character
    C = LEN_TRIM( Input_Opt%DATA_DIR )
    IF ( Input_Opt%DATA_DIR(C:C) /= '/' ) THEN
       Input_Opt%DATA_DIR = TRIM( Input_Opt%DATA_DIR ) // '/'
    ENDIF

    ! Create CHEM_INPUTS directory
    Input_Opt%CHEM_INPUTS_DIR = TRIM( Input_Opt%DATA_DIR ) // &
                                'CHEM_INPUTS/'

    !------------------------------------------------------------------------
    ! Meteorology field
    !------------------------------------------------------------------------
    key   = "simulation%met_field"
    v_str = MISSING_STR
    CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    Input_Opt%MetField = TRIM( v_str )

#if !defined( MODEL_CESM )
    ! Make sure a valid met field is specified
    Met = To_UpperCase( TRIM( Input_Opt%MetField ) )
! LDH:  A More comprehensive list of CATChem models need to be added
    SELECT CASE( TRIM( Met ) )
       CASE( 'UFS', 'UFS-CHEM' )
          Input_Opt%MetField = 'UFS'
       CASE( 'WRF', 'WRF-CHEM' )
          Input_Opt%MetField = 'WRF'
       CASE( 'AQM')
          Input_Opt%MetField = 'AQM'
       CASE( 'MODEL-X' )
          Input_Opt%MetField = 'MODEL-X'
       CASE( 'MODEL-Y' )
          Input_Opt%MetField = 'MODEL-Y'
       CASE DEFAULT
          errMsg = Trim( Input_Opt%MetField ) // ' is not a valid '       // &
                ' met field. Supported met fields are UFS, '          // &
                ' WRF,  AQM, or MODEL-X. Please check your '              // &
                '"CATChem_config.ymls" file.'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    END SELECT
#endif

    !------------------------------------------------------------------------
    ! Turn on timers
    !------------------------------------------------------------------------
    key    = "simulation%use_gcclassic_timers"
    v_bool = MISSING_BOOL
    CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    Input_Opt%UseTimers = v_bool

    !------------------------------------------------------------------------
    ! Set start time of run in "time_mod.F90"
    !------------------------------------------------------------------------
    CALL Set_Begin_Time( Input_Opt%NYMDb, Input_Opt%NHMSb, RC  )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error encountered in "Set_Begin_Time"!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF

    !------------------------------------------------------------------------
    ! Set end time of run in "time_mod.F90"
    !------------------------------------------------------------------------
    errMsg = 'Error encountered in "Set_Begin_Time"!'
    CALL Set_End_Time( Input_Opt%NYMDe, Input_Opt%NHMSe, RC )
    IF ( RC /= CC_SUCCESS ) THEN
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF

    ! Set the current time
    CALL Set_Current_Time()

#ifdef BPCH_DIAG
    !------------------------------------------------------------------------
    ! Set the start of the 1st diagnostic interval
    ! Only applies when CATChem is configured with -DBPCH_DIAG=y
    !------------------------------------------------------------------------
    CALL Set_DiagB( GET_TAU() )
#endif

#endif

    !========================================================================
    ! Compute the length of the simulation, in elapsed seconds
    !========================================================================
    JulianDateStart        = GET_JD( Input_Opt%NymdB, Input_Opt%NhmsB )
    JulianDateEnd          = GET_JD( Input_Opt%NymdE, Input_Opt%NhmsE )
    Input_Opt%SimLengthSec = NINT( ( JulianDateEnd - JulianDateStart  )      &
                           * 86400_f8)

    ! Return success
    RC = CC_SUCCESS

    !========================================================================
    ! Print to screen
    !========================================================================
    IF ( Input_Opt%amIRoot ) THEN
       WRITE( 6, 90  ) 'SIMULATION SETTINGS'
       WRITE( 6, 95  ) '-------------------'
       WRITE( 6, 110 ) 'Simulation name             : ',                     &
                        TRIM( Input_Opt%SimulationName )
       WRITE( 6, 110 ) 'CHEM_INPUTS directory       : ',                     &
                        TRIM( Input_Opt%CHEM_INPUTS_DIR )
       WRITE( 6, 110 ) 'Species database file       : ',                     &
                        TRIM( Input_Opt%SpcDatabaseFile )
       WRITE( 6, 120 ) 'Turn on verbose output      : ',                     &
                        Input_Opt%Verbose
       WRITE( 6, 110 ) 'Verbose output printed on   : ',                     &
                        TRIM( verboseMsg )
#ifdef MODEL_CLASSIC
       WRITE( 6, 100 ) 'Start time of run           : ',                     &
                        Input_Opt%NYMDb, Input_Opt%NHMSb
       WRITE( 6, 100 ) 'End time of run             : ',                     &
                        Input_Opt%NYMDe, Input_Opt%NHMSe
       WRITE( 6, 110 ) 'Data Directory              : ',                     &
                        TRIM( Input_Opt%DATA_DIR )
       WRITE( 6, 110 ) 'Meteorology field           : ',                     &
                        TRIM( Input_Opt%MetField )
       WRITE( 6, 120 ) 'Turn on CATChem timers    : ',                     &
                        Input_Opt%useTimers
#endif
    ENDIF

    ! Format statements
90  FORMAT( /, A              )
95  FORMAT( A                 )
100 FORMAT( A, I8.8, 1X, I6.6 )
110 FORMAT( A, A              )
120 FORMAT( A, L5             )

  END SUBROUTINE Config_Simulation
!EOC
!------------------------------------------------------------------------------
!                  CATChem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: config_grid
!
! !DESCRIPTION: Copies grid information from the Config object
!  to Input_Opt, and does necessary checks.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Config_Grid( Config, Input_Opt, State_Grid, RC )
!
! !USES:
!
    USE CharPak_Mod,    ONLY : StrSplit
    USE Error_Mod
    USE Input_Opt_Mod,  ONLY : OptInput
    USE RoundOff_Mod,   ONLY : Cast_and_RoundOff
    USE State_Grid_Mod, ONLY : GrdState
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(QFYAML_t), INTENT(INOUT) :: Config      ! YAML Config object
    TYPE(OptInput), INTENT(INOUT) :: Input_Opt   ! Input options
    TYPE(GrdState), INTENT(INOUT) :: State_Grid  ! Grid State
!
! !OUTPUT PARAMETERS:
!
    INTEGER,        INTENT(OUT)   :: RC          ! Success or failure
!
!EOP
!------------------------------------------------------------------------------
!BOC
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
    thisLoc = ' -> at Config_Grid (in input_mod.F90)'

    !------------------------------------------------------------------------
    ! Grid resolution
    !------------------------------------------------------------------------
    key   = "grid%resolution"
    v_str = MISSING_STR
    CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    State_Grid%GridRes = TRIM( v_str )

    ! Split into two values, separated by 'x'
    CALL StrSplit( TRIM( State_Grid%GridRes ) , 'x', SubStrs, nSubStrs )

    ! Stop with error if there are more than two substrings
    IF ( nSubStrs /= 2 ) THEN
       errMsg = 'Error in extracting delta X and Y values from'    // &
                ' State_Grid%GridRes. Values must be separated by' // &
                ' an x. Please check your "CATChem_config.yml" file.'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF

    ! Save the delta X and Y values
    State_Grid%DY = Cast_and_RoundOff( subStrs(1), places=4 )
    State_Grid%DX = Cast_and_RoundOff( subStrs(2), places=4 )

    !------------------------------------------------------------------------
    ! Level range
    !------------------------------------------------------------------------
    key   = "grid%number_of_levels"
    v_int = MISSING_INT
    CALL QFYAML_Add_Get( Config, TRIM( key ), v_int, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    State_Grid%NZ = v_int

    !------------------------------------------------------------------------
    ! Longitude range
    !------------------------------------------------------------------------
    key   = "grid%longitude%range"
    a_str = MISSING_STR
    CALL QFYAML_Add_Get( Config, TRIM( key ), a_str, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    State_Grid%XMin = Cast_and_RoundOff( a_str(1), places=4 )
    State_Grid%XMax = Cast_and_RoundOff( a_str(2), places=4 )

    ! Make sure values are in valid rangre
    IF ( State_Grid%XMin >= State_Grid%XMax ) THEN
       WRITE( XMin_Str, '(i10)' ) State_Grid%XMin
       WRITE( XMax_Str, '(i10)' ) State_Grid%XMax
       errMsg = 'Lower lon must be smaller than upper lon: ' // &
                TRIM( XMin_Str ) // ' ' // TRIM( XMax_Str )
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF

    !------------------------------------------------------------------------
    ! Center longitude on International Date Line?Longitude range
    !------------------------------------------------------------------------
    key    = "grid%longitude%center_at_180"
    v_bool = MISSING_BOOL
    CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
      errMsg = 'Error parsing ' // TRIM( key ) // '!'
      CALL CC_Error( errMsg, RC, thisLoc )
      RETURN
    ENDIF
    State_Grid%Center180 = v_bool

    !------------------------------------------------------------------------
    ! Latitude range
    !------------------------------------------------------------------------
    key   = "grid%latitude%range"
    a_str = MISSING_STR
    CALL QFYAML_Add_Get( Config, TRIM( key ), a_str, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    State_Grid%YMin = Cast_and_RoundOff( a_str(1), places=4 )
    State_Grid%YMax = Cast_and_RoundOff( a_str(2), places=4 )

    ! Make sure values are in valid range
    IF ( State_Grid%YMin >= State_Grid%YMax ) THEN
       WRITE( YMin_Str, '(i10)' ) State_Grid%YMin
       WRITE( YMax_Str, '(i10)' ) State_Grid%YMax
       errMsg = 'Lower lat must be smaller than upper lat: ' // &
                TRIM( YMin_Str ) // ' ' // TRIM( YMax_Str )
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF

    ! Restrict latitude values to -90.0 and 90.0
    IF ( State_Grid%YMin < -90.0_fp ) THEN
      WRITE( YMin_Str, '(i10)' ) State_Grid%YMin
      errMsg = 'Lower latitude must be between -90 and 90 degN: ' // &
                TRIM( YMin_Str )
      CALL CC_Error( errMsg, RC, thisLoc )
      RETURN
    ENDIF
    IF ( State_Grid%YMax > 90.0_fp ) THEN
       WRITE( YMax_Str, '(i10)' ) State_Grid%YMax
       errMsg = 'Upper latitude must be between -90 and 90 degN: ' // &
                TRIM( YMax_Str )
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF

    !------------------------------------------------------------------------
    ! Use half-sized polar boxes in latitude?
    !------------------------------------------------------------------------
    key    = "grid%latitude%half_size_polar_boxes"
    v_bool = MISSING_BOOL
    CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    State_Grid%HalfPolar = v_bool

    !------------------------------------------------------------------------
    ! Nested grid settings
    !------------------------------------------------------------------------
    key    = "grid%nested_grid_simulation%activate"
    v_bool = MISSING_BOOL
    CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    State_Grid%NestedGrid = v_bool

    IF ( State_Grid%NestedGrid ) THEN
       ! Increase NX by 1
       State_Grid%NX        = State_Grid%NX + 1

       ! For now hardcode HalfPolar to false when using a nested grid
       State_Grid%HalfPolar = .FALSE.
    ENDIF

    !------------------------------------------------------------------------
    ! Nested grid transport offsets
    !------------------------------------------------------------------------
    key   = "grid%nested_grid_simulation%buffer_zone_NSEW"
    a_int = MISSING_INT
    CALL QFYAML_Add_Get( Config, TRIM( key ), a_int, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    State_Grid%NorthBuffer = a_int(1)
    State_Grid%SouthBuffer = a_int(2)
    State_Grid%EastBuffer  = a_int(3)
    State_Grid%WestBuffer  = a_int(4)

    ! Set buffers to zero for global grids
    IF ( .not. State_Grid%NestedGrid ) THEN
       State_Grid%NorthBuffer = 0
       State_Grid%SouthBuffer = 0
       State_Grid%EastBuffer  = 0
       State_Grid%WestBuffer  = 0
    ENDIF

    ! Compute grid horizontal dimensions
    State_Grid%NX =                                                          &
       FLOOR( ( State_Grid%XMax - State_Grid%XMin ) / State_Grid%DX )
    IF ( State_Grid%HalfPolar .and. .not. State_Grid%NestedGrid ) THEN
       State_Grid%NY =                                                       &
          FLOOR( ( State_Grid%YMax - State_Grid%YMin ) / State_Grid%DY ) + 1
    ELSE
       State_Grid%NY =                                                       &
          FLOOR( ( State_Grid%YMax - State_Grid%YMin ) / State_Grid%DY )
    ENDIF

    ! Return success
    RC = CC_SUCCESS

    !========================================================================
    ! Print to screen
    !========================================================================
    IF ( Input_Opt%amIRoot ) THEN
       WRITE( 6, 90  ) 'GRID SETTINGS'
       WRITE( 6, 95  )  '------------'
       WRITE( 6, 100 ) 'Grid resolution             : ',                     &
                        TRIM( State_Grid%GridRes )
       WRITE( 6, 110 ) 'Min/max longitude           : ',                     &
                        State_Grid%XMin, State_Grid%XMax
       WRITE( 6, 110 ) 'Min/max latitude            : ',                     &
                        State_Grid%YMin, State_Grid%YMax
       WRITE( 6, 120 ) 'X grid dimension            : ',                     &
                        State_Grid%NX
       WRITE( 6, 120 ) 'Y grid dimension            : ',                     &
                        State_Grid%NY
       WRITE( 6, 120 ) 'Z grid dimension            : ',                     &
                        State_Grid%NZ
       WRITE( 6, 130 ) 'Use half-sized polar boxes? : ',                     &
                        State_Grid%HalfPolar
       WRITE( 6, 130 ) 'Center on Intl Date Line?   : ',                     &
                        State_Grid%Center180
       WRITE( 6, 130 ) 'Is this a nested-grid sim?  : ',                     &
                        State_Grid%NestedGrid
       WRITE( 6, 140 ) ' --> Buffer zone (N S E W ) : ',                     &
                        State_Grid%NorthBuffer,                              &
                        State_Grid%SouthBuffer,                              &
                        State_Grid%EastBuffer,                               &
                        State_Grid%WestBuffer
    ENDIF

    ! Format statements
90  FORMAT( /, A                )
95  FORMAT( A                   )
100 FORMAT( A, A                )
110 FORMAT( A, F10.4, 1X, F10.4 )
120 FORMAT( A, I5               )
130 FORMAT( A, L5               )
140 FORMAT( A, 4I5              )

  END SUBROUTINE Config_Grid
!EOC
!------------------------------------------------------------------------------
!                  CATChem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: config_aerosol
!
! !DESCRIPTION: Copies aerosol information from the Config object
!  to Input_Opt, and does necessary checks.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Config_Aerosol( Config, Input_Opt, RC )
!
! !USES:
!
    USE Error_Mod
    USE Input_Opt_Mod, ONLY : OptInput
    USE RoundOff_Mod,  ONLY : Cast_and_RoundOff
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(QFYAML_t), INTENT(INOUT) :: Config      ! YAML Config object
    TYPE(OptInput), INTENT(INOUT) :: Input_Opt   ! Input Options object
!
! !OUTPUT PARAMETERS:
!
    INTEGER,        INTENT(OUT)   :: RC          ! Success or failure
!
! !REMARKS:
!  Move error checks that depend on species indices to the subroutine
!  DO_ERROR_CHECKS.  This is now called from GC_INIT_EXTRA, after the
!  initialization of the species database.
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    ! Scalars
    INTEGER                      :: N, T
    INTEGER                      :: v_int
    LOGICAL                      :: v_bool
    REAL(yp)                     :: v_real

    ! Strings
    CHARACTER(LEN=255)           :: thisLoc
    CHARACTER(LEN=255)           :: errMsg
    CHARACTER(LEN=QFYAML_NamLen) :: key
    CHARACTER(LEN=QFYAML_StrLen) :: v_str
    CHARACTER(LEN=QFYAML_StrLen) :: a_str(2)

    !========================================================================
    ! Config_Aerosol begins here!
    !========================================================================

    ! Initialize
    RC      = CC_SUCCESS
    errMsg  = ''
    thisLoc = ' -> at Config_Aerosol (in module input_mod.F90)'

    !------------------------------------------------------------------------
    ! Use online carbon aerosols?
    !------------------------------------------------------------------------
    key    = "aerosols%carbon%activate"
    v_bool = MISSING_BOOL
    CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    Input_Opt%LCARB = v_bool

    !------------------------------------------------------------------------
    ! Use brown carbon aerosols?
    !------------------------------------------------------------------------
    key    = "aerosols%carbon%use_brown_carbon"
    v_bool = MISSING_BOOL
    CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    Input_Opt%LBRC = v_bool

    !------------------------------------------------------------------------
    ! Include BC absorption enhancement due to coating?
    !------------------------------------------------------------------------
    key    = "aerosols%carbon%enhance_black_carbon_absorption%activate"
    v_bool = MISSING_BOOL
    CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF

    !------------------------------------------------------------------------
    ! Define BC absorption enhancement
    !------------------------------------------------------------------------
    key   = "aerosols%carbon%enhance_black_carbon_absorption%hydrophilic"
    v_str = MISSING_STR
    CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF

    !------------------------------------------------------------------------
    ! Define BC absorption enhancement (xnw, 8/24/15)
    !------------------------------------------------------------------------
    key   = "aerosols%carbon%enhance_black_carbon_absorption%hydrophobic"
    v_str = MISSING_STR
    CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF

    !------------------------------------------------------------------------
    ! Use secondary organic aerosols?
    !------------------------------------------------------------------------
    key    = "aerosols%complex_SOA%activate"
    v_bool = MISSING_BOOL
    CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    Input_Opt%LSOA = v_bool

    !------------------------------------------------------------------------
    ! Use semi-volatile POA?
    !------------------------------------------------------------------------
    key    = "aerosols%complex_SOA%semivolatile_POA"
    v_bool = MISSING_BOOL
    CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    Input_Opt%LSVPOA = v_bool

    !------------------------------------------------------------------------
    ! Use online dust aerosols ?
    !------------------------------------------------------------------------
    key    = "aerosols%dust%activate"
    v_bool = MISSING_BOOL
    CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    Input_Opt%LDUST = v_bool

    !------------------------------------------------------------------------
    ! Use SO2 and HNO3 uptake on dust aerosols
    !------------------------------------------------------------------------
    key    = "aerosols%dust%acid_uptake_on_dust"
    v_bool = MISSING_BOOL
    CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    Input_Opt%LDSTUP = v_bool

    !------------------------------------------------------------------------
    ! Use online sea-salt aerosols?
    !------------------------------------------------------------------------
    key    = "aerosols%sea_salt%activate"
    v_bool = MISSING_BOOL
    CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    Input_Opt%LSSALT = v_bool

    !------------------------------------------------------------------------
    ! Accum mode seasalt radii bin edges [um]
    !------------------------------------------------------------------------
    key   = "aerosols%sea_salt%SALA_radius_bin_in_um"
    a_str = MISSING_STR
    CALL QFYAML_Add_Get( Config, TRIM( key ), a_str, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
!  LDH: Comment out for now, these are not yet defined
!    Input_Opt%SALA_Redge_um(1) = Cast_and_RoundOff( a_str(1), places=2 )
!    Input_Opt%SALA_Redge_um(2) = Cast_and_RoundOff( a_str(2), places=2 )

    !------------------------------------------------------------------------
    ! Coarse mode seasalt radii bin edges [um]
    !------------------------------------------------------------------------
    key   = "aerosols%sea_salt%SALC_radius_bin_in_um"
    a_str = MISSING_STR
    CALL QFYAML_Add_Get( Config, TRIM( key ), a_str, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
!  LDH: Comment out for now, these are not yet defined
!    Input_Opt%SALC_Redge_um(1) = Cast_and_RoundOff( a_str(1), places=2 )
!    Input_Opt%SALC_Redge_um(2) = Cast_and_RoundOff( a_str(2), places=2 )

    !------------------------------------------------------------------------
    ! Use marine organic aerosols?
    !------------------------------------------------------------------------
    key    = "aerosols%sea_salt%marine_organic_aerosols"
    v_bool = MISSING_BOOL
    CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    Input_Opt%LMPOA = v_bool

    !------------------------------------------------------------------------
    ! Apply gravitational settling in stratosphere?
    !------------------------------------------------------------------------
    key    = "aerosols%stratosphere%settle_strat_aerosol"
    v_bool = MISSING_BOOL
    CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    Input_Opt%LGRAVSTRAT = v_bool

    !------------------------------------------------------------------------
    ! Use solid polar stratospheric clouds (PSCs)?
    !------------------------------------------------------------------------
    key    = "aerosols%stratosphere%polar_strat_clouds%activate"
    v_bool = MISSING_BOOL
    CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    Input_Opt%LSOLIDPSC = v_bool

    !------------------------------------------------------------------------
    ! Perform heterogeneous chemistry on PSCs?
    !------------------------------------------------------------------------
    key    = "aerosols%stratosphere%polar_strat_clouds%het_chem"
    v_bool = MISSING_BOOL
    CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    Input_Opt%LPSCCHEM = v_bool

    !------------------------------------------------------------------------
    ! Allow homogeneous NAT?
    !------------------------------------------------------------------------
    key    = "aerosols%stratosphere%allow_homogeneous_NAT"
    v_bool = MISSING_BOOL
    CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    Input_Opt%LHOMNUCNAT = v_bool

    !------------------------------------------------------------------------
    ! NAT supercooling requirement (K)
    !------------------------------------------------------------------------
    key   = "aerosols%stratosphere%NAT_supercooling_req_in_K"
    v_str = MISSING_STR
    CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    Input_Opt%T_NAT_SUPERCOOL = Cast_and_RoundOff( v_str, places=2 )

    !------------------------------------------------------------------------
    ! Ice supersaturation ratio requirement
    !------------------------------------------------------------------------
    key   = "aerosols%stratosphere%supersat_factor_req_for_ice_nucl"
    v_str = MISSING_STR
    CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    Input_Opt%P_ICE_SUPERSAT = Cast_and_RoundOff( v_str, places=2 )

    !------------------------------------------------------------------------
    ! Include stratospheric aerosols optical depths?
    !------------------------------------------------------------------------
    key    = "aerosols%stratosphere%calc_strat_aod"
    v_bool = MISSING_BOOL
    CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    Input_Opt%LSTRATOD = v_bool

    !------------------------------------------------------------------------
    ! Use online sulfate aerosols?
    !------------------------------------------------------------------------
    key    = "aerosols%sulfate%activate"
    v_bool = MISSING_BOOL
    CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    Input_Opt%LSULF = v_bool

    !------------------------------------------------------------------------
    ! Use metal catalyzed oxidation of SO2?
    !------------------------------------------------------------------------
    key    = "aerosols%sulfate%metal_cat_SO2_oxidation"
    v_bool = MISSING_BOOL
    CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error parsing ' // TRIM( key ) // '!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    Input_Opt%LMETALCATSO2 = v_bool

    !=================================================================
    ! Error checks
    !=================================================================

    ! Make sure that SALA, SALC bins are contiguous
!  LDH: Comment out for now, these are not yet defined
!    IF ( Input_Opt%SALA_REDGE_um(2) /= &
!         Input_Opt%SALC_REDGE_um(1)     ) THEN
!       errMsg = 'SALA and SALC bin edges are not contiguous!'
!       CALL CC_Error( errMsg, RC, thisLoc )
!       RETURN
!    ENDIF

    ! Turn off switches for simulations that don't use aerosols
    IF ( ( .not. Input_Opt%ITS_A_FULLCHEM_SIM )  .and. &
         ( .not. Input_Opt%ITS_AN_AEROSOL_SIM ) ) THEN
       Input_Opt%LSULF        = .FALSE.
       Input_Opt%LMETALCATSO2 = .FALSE.
       Input_Opt%LCARB        = .FALSE.
       Input_Opt%LBRC         = .FALSE.
       Input_Opt%LSOA         = .FALSE.
       Input_Opt%LDUST        = .FALSE.
       Input_Opt%LSSALT       = .FALSE.
       Input_Opt%LMPOA        = .FALSE.
       Input_Opt%LSVPOA       = .FALSE.
    ENDIF

    ! Return success
    RC = CC_SUCCESS

    !========================================================================
    ! Print to screen
    !========================================================================
    IF ( Input_Opt%amIRoot ) THEN
       WRITE( 6, 90  ) 'AEROSOL SETTINGS'
       WRITE( 6, 95  ) '----------------'
       WRITE( 6, 100 ) 'Online SULFATE AEROSOLS?    : ', Input_Opt%LSULF
       WRITE( 6, 100 ) 'Metal catalyzed SO2 ox.?    : ', Input_Opt%LMETALCATSO2
       WRITE( 6, 100 ) 'Online CARBON AEROSOLS?     : ', Input_Opt%LCARB
       WRITE( 6, 100 ) 'Brown Carbon Aerosol?       : ', Input_Opt%LBRC
       WRITE( 6, 100 ) 'Online COMPLEX SOA?         : ', Input_Opt%LSOA
       WRITE( 6, 100 ) 'Semivolatile POA?           : ', Input_Opt%LSVPOA
       WRITE( 6, 100 ) 'Online DUST AEROSOLS?       : ', Input_Opt%LDUST
       WRITE( 6, 100 ) 'Acid uptake on dust?        : ', Input_Opt%LDSTUP
       WRITE( 6, 100 ) 'Online SEA SALT AEROSOLS?   : ', Input_Opt%LSSALT
       WRITE( 6, 110 ) 'Accum  SEA SALT radii [um]  : ',                     &
                                                 Input_Opt%SALA_REDGE_um(1), &
                                                 Input_Opt%SALA_REDGE_um(2)
       WRITE( 6, 110 ) 'Coarse SEA SALT radii [um]  : ',                     &
                                                 Input_Opt%SALC_REDGE_um(1), &
                                                 Input_Opt%SALC_REDGE_um(2)
       WRITE( 6, 100 ) 'MARINE ORGANIC AEROSOLS?    : ', Input_Opt%LMPOA
       WRITE( 6, 100 ) 'Settle strat. aerosols?     : ', Input_Opt%LGRAVSTRAT
       WRITE( 6, 100 ) 'Online SOLID PSC aerosols?  : ', Input_Opt%LSOLIDPSC
       WRITE( 6, 100 ) 'Allow hom. NAT nucleation?  : ', Input_Opt%LHOMNUCNAT
       WRITE( 6, 120 ) 'NAT supercooling requirement: ',                     &
                                                   Input_Opt%T_NAT_SUPERCOOL
       WRITE( 6, 120 ) 'Ice supersaturation req.    : ',                     &
                                     ((Input_Opt%P_ICE_SUPERSAT-1)*1.e+2_fp)
       WRITE( 6, 100 ) 'Perform PSC het. chemistry? : ', Input_Opt%LPSCCHEM
       WRITE( 6, 100 ) 'Use strat. aerosol OD?      : ', Input_Opt%LSTRATOD
    ENDIF

90  FORMAT( /, A                 )
95  FORMAT( A                    )
100 FORMAT( A, L5                )
105 FORMAT( A, f8.2              )
110 FORMAT( A, f8.2, ' - ', f8.2 )
120 FORMAT( A, f8.2, 'K'         )

  END SUBROUTINE Config_Aerosol
!EOC


!------------------------------------------------------------------------------
!                  CATChem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: validate_directories
!
! !DESCRIPTION: Makes sure that each of the directories that we have read from
!  the CATChem input file are valid. Also, trailing separator characters will
!  be added.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE VALIDATE_DIRECTORIES( Input_Opt, RC )
!
! !USES:
!
    USE Error_Mod
    USE Input_Opt_Mod, ONLY : OptInput
    USE Time_Mod,      ONLY : Expand_Date
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(OptInput), INTENT(INOUT) :: Input_Opt   ! Input Options object
!
! !OUTPUT PARAMETERS:
!
    INTEGER,        INTENT(OUT)   :: RC          ! Success or failure?
!
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    ! Strings
    CHARACTER(LEN=255) :: errMsg, thisLoc, Dir

    !=================================================================
    ! Validate_Directories begins here!
    !=================================================================

    ! Initialize
    RC      = CC_SUCCESS
    errMsg  = 'Invalid directory encountered!'
    thisLoc = ' -> at Validate_Directories (in module input_mod.F90)'

    ! Skip for dry-runs
    IF ( Input_Opt%DryRun ) RETURN

#if !defined( MODEL_CESM )
    ! Check directories
    CALL Check_Directory( Input_Opt, Input_Opt%DATA_DIR, RC )
    IF ( RC /= CC_SUCCESS ) THEN
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
#endif

    CALL Check_Directory( Input_Opt, Input_Opt%CHEM_INPUTS_DIR, RC )
    IF ( RC /= CC_SUCCESS ) THEN
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF

#if !defined( MODEL_CESM )
    CALL Check_Directory( Input_Opt, Input_Opt%RUN_DIR, RC )
    IF ( RC /= CC_SUCCESS ) THEN
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
#endif

  END SUBROUTINE Validate_Directories
!EOC
!------------------------------------------------------------------------------
!                  CATChem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: check_directory
!
! !DESCRIPTION: Makes sure that the given directory is valid.  Also a trailing
!  slash character will be added if necessary.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Check_Directory( Input_Opt, dir, RC )
!
! !USES:
!
    USE Error_Mod
    USE FILE_MOD,      ONLY : File_Exists
    USE Input_Opt_Mod, ONLY : OptInput
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(OptInput),   INTENT(INOUT) :: Input_Opt   ! Input Options object
    CHARACTER(LEN=*), INTENT(INOUT) :: dir         ! Dir to be checked
!
! !OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(OUT)   :: RC          ! Success or failure
!
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    ! Scalars
    INTEGER            :: C

    ! Strings
    CHARACTER(LEN=255) :: errMsg, thisLoc

    !=================================================================
    ! Check_Directory begins here!
    !=================================================================

    ! Initialize
    RC      = CC_SUCCESS
    errMsg  = ''
    thisLoc = ' -> at Check_Directory (in module input_mod.F90)'

    ! Locate the last non-white-space character of NEWDIR
    C = LEN_TRIM( dir )

    ! Add the trailing directory separator if it is not present
    IF ( dir(C:C) /= '/' ) THEN
       dir(C+1:C+1) = TRIM( '/' )
    ENDIF

    !=================================================================
    ! Test if the directory actually exists
    !=================================================================

    ! If the directory does not exist then stop w/ an error message
    IF ( .not. File_Exists( dir ) ) THEN
       errMsg = 'Invalid directory: ' // TRIM( dir )
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF

  END SUBROUTINE Check_Directory

  
#ifdef BPCH_DIAG
!------------------------------------------------------------------------------
!                  CATChem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: is_last_day_good
!
! !DESCRIPTION: Tests to see if there is output scheduled on the last day of
!  the run.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE IS_LAST_DAY_GOOD( Input_Opt, RC )
!
! !USES:
!
    USE Error_Mod
    USE Input_Opt_Mod, ONLY : OptInput
    USE JULDAY_MOD,    ONLY : JULDAY
    USE TIME_MOD,      ONLY : GET_NYMDe, ITS_A_LEAPYEAR, YMD_EXTRACT
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(OptInput), INTENT(INOUT) :: Input_Opt  ! Input options
    INTEGER,        INTENT(OUT)   :: RC         ! Success or failure?
!
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    ! Scalars
    LOGICAL            :: IS_LEAPYEAR
    INTEGER            :: NYMDe, Y, M, D, LASTDAY
    REAL(fp)           :: JD, JD0

    ! Strings
    CHARACTER(LEN=255) :: errMsg, thisLoc

    !=================================================================
    ! Is_Last_Day_Good begins here!
    !=================================================================

    ! Initialize
    RC      = CC_SUCCESS
    errMsg  = ''
    thisLoc = ' -> at Is_Last_Day_Good (in module input_mod.F90)'

    ! Astronomical Julian Day corresponding to NYMDe
    NYMDe = GET_NYMDe()
    CALL YMD_EXTRACT( NYMDe, Y, M, D )
    JD = JULDAY( Y, M, DBLE( D ) )

    ! Astronomical Julian Day corresponding to the 1st of the year
    JD0 = JULDAY( Y, 1, 0d0 )

    ! LASTDAY is the day of year corresponding to NYMDe
    LASTDAY = JD - JD0

    ! Skip past the element of NJDAY for Feb 29, if necessary
    IF ( .not. ITS_A_LEAPYEAR( Y, .TRUE. ) .and. LASTDAY > 59 ) THEN
       LASTDAY = LASTDAY + 1
    ENDIF

    ! Exit w/ error if THIS_NJDAY = 0
    IF ( Input_Opt%NJDAY(LASTDAY) == 0 ) THEN
       errMsg = 'No output scheduled on last day of run!'
       CALL CC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF

  END SUBROUTINE IS_LAST_DAY_GOOD
!EOC
#endif
!------------------------------------------------------------------------------
!                  CATChem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: do_error_checks
!
! !DESCRIPTION: Makes sure that certain species are defined in order to
!  proceed with a certain option.  Halts the simulation with an error message
!  if incorrect inputs  would have caused  a simulation to crash.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Do_Error_Checks( Input_Opt, RC )
!
! !USES:
!
    USE Error_Mod
    USE Input_Opt_Mod, ONLY : OptInput

! LDH: Comment this out for now
!    USE State_Chm_Mod, ONLY : Ind_  
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(OptInput), INTENT(INOUT) :: Input_Opt
!
! !OUTPUT PARAMETERS:
!
    INTEGER,        INTENT(OUT)   :: RC
!
! !REMARKS:
!  The Ind_() function now defines all species ID's.  It returns -1 if
!  a species cannot be found.  The prior behavior was to return 0 if a
!  species wasn't found.  Therefore, in order to preserve the logic of the
!  error checks, we must force any -1's returned by Ind_() to 0's in
!  this subroutine.
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    ! Scalars
    INTEGER            :: I

    ! Strings
    CHARACTER(LEN=255) :: MSG, LOCATION

    !=================================================================
    ! Initialization
    !=================================================================

    ! Assume success
    RC       = CC_SUCCESS

    ! Define location string
    LOCATION = '-> at Do_Error_Checks (in input_mod.F90)'

    !=================================================================
    ! Error check SEASALT AEROSOLS
    !=================================================================
!    I = MAX( Ind_('SALA','A'), 0 ) + MAX( Ind_('SALC','A'), 0 )
!
!    IF ( Input_Opt%LSSALT ) THEN
!       IF ( I == 0 ) THEN
!          MSG = 'LSSALT=T but ONLINE SEASALT AEROSOLS are undefined!'
!          CALL CC_Error( Msg, RC, Location )
!          RETURN
!       ENDIF
!    ELSE
!       IF ( I > 0 ) THEN
!          MSG = 'Cannot use ONLINE SEASALT AEROSOLS if LSSALT=F!'
!          CALL CC_Error( Msg, RC, Location )
!          RETURN
!       ENDIF
!    ENDIF



    !=================================================================
    ! Error check DUST AEROSOLS
    !=================================================================

!#ifdef TOMAS
!    ! For TOMAS only: If DUST01 is present, the other dust species are too
!    I = MAX( Ind_('DUST01','A'), 0 )
!#else
!    ! Non-TOMAS simulations: Need all DST1-DST4 species
!    I = MAX( Ind_('DST1','A'), 0 ) + &
!        MAX( Ind_('DST2','A'), 0 ) + &
!        MAX( Ind_('DST3','A'), 0 ) + &
!        MAX( Ind_('DST4','A'), 0 )
!#endif

!    IF ( Input_Opt%LDUST ) THEN
!       IF ( I == 0 ) THEN
!          MSG = 'LDUST=T but ONLINE DUST AEROSOLS are undefined!'
!          CALL CC_Error( Msg, RC, Location )
!          RETURN
!       ENDIF
!    ELSE
!       IF ( I > 0 ) THEN
!          MSG = 'Cannot use ONLINE DUST AEROSOLS if LDUST=F!'
!          CALL CC_Error( Msg, RC, Location )
!          RETURN
!       ENDIF
!    ENDIF
!
!    !=================================================================
!    ! Error check DUST NITRATE    AEROSOLS
!    !             DUST SULFATE    AEROSOLS
!    !             DUST ALKALINITY AEROSOLS
!    !=================================================================
!    I = MAX( Ind_('NITd1'  ,'A'), 0 ) + &
!        MAX( Ind_('NITd2'  ,'A'), 0 ) + &
!        MAX( Ind_('NITd3'  ,'A'), 0 ) + &
!        MAX( Ind_('NITd4'  ,'A'), 0 ) + &
!        MAX( Ind_('SO4d1'  ,'A'), 0 ) + &
!        MAX( Ind_('SO4d2'  ,'A'), 0 ) + &
!        MAX( Ind_('SO4d3'  ,'A'), 0 ) + &
!        MAX( Ind_('SO4d4'  ,'A'), 0 ) + &
!        MAX( Ind_('DSTAL1' ,'A'), 0 ) + &
!        MAX( Ind_('DSTAL2' ,'A'), 0 ) + &
!        MAX( Ind_('DSTAL3' ,'A'), 0 ) + &
!        MAX( Ind_('DSTAL4' ,'A'), 0 )
!
!    IF ( Input_Opt%LDSTUP ) THEN
!       IF ( I < 12 ) THEN
!          MSG = 'LDSTUP=T but COATED DUST AEROSOLS are undefined!'
!          CALL CC_Error( Msg, RC, Location )
!          RETURN
!       ENDIF
!    ELSE
!       IF ( I > 0 ) THEN
!          MSG = 'Cannot use COATED DUST AEROSOLS if LDSTUP=F!'
!          CALL CC_Error( Msg, RC, Location )
!          RETURN
!       ENDIF
!    ENDIF
!
!    !=================================================================
!    ! Error check SEASALT AEROSOLS
!    !=================================================================
!    I = MAX( Ind_('SALA','A'), 0 ) + MAX( Ind_('SALC','A'), 0 )
!
!    IF ( Input_Opt%LSSALT ) THEN
!       IF ( I == 0 ) THEN
!          MSG = 'LSSALT=T but ONLINE SEASALT AEROSOLS are undefined!'
!          CALL CC_Error( Msg, RC, Location )
!          RETURN
!       ENDIF
!    ELSE
!       IF ( I > 0 ) THEN
!          MSG = 'Cannot use ONLINE SEASALT AEROSOLS if LSSALT=F!'
!          CALL CC_Error( Msg, RC, Location )
!          RETURN
!       ENDIF
!    ENDIF
!

  END SUBROUTINE Do_Error_Checks
!EOC
!------------------------------------------------------------------------------
!                  CATChem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Find_Number_of_Species
!
! !DESCRIPTION: Searches a string array containing species names and returns
!  the number of valid species (i.e. species that do not match MISSING_STR).
!  Assumes all the valid species will be listed contiguously at the front
!  of the array
!\\
!\\
! !INTERFACE:
!
   FUNCTION Get_Number_of_Species( a_str ) RESULT( n_valid )
!
! !INPUT PARAMETERS:
!
    CHARACTER(LEN=*), INTENT(IN) :: a_str(:)
!
! !RETURN VALUE:
!
    INTEGER                      :: n_valid
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER :: N

    ! Return the number of valid species
    n_valid = 0
    DO N = 1, SIZE( a_str )
       IF ( TRIM( a_str(N) ) == MISSING_STR ) EXIT
       n_valid = n_valid + 1
    ENDDO

  END FUNCTION Get_Number_of_Species
!EOC
END MODULE Input_Mod

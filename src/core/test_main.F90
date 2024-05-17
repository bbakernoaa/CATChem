!> \\file read_species_database.F90


program read_species_database

  use QfYaml_Mod
!  use ErrCode_Mod
  implicit none


  CHARACTER(LEN=20), PARAMETER :: SpecFileName ='species_database.yml'
  CHARACTER(LEN=18), PARAMETER :: configFile ='CATChem_config.yml'
  CHARACTER(LEN=19), PARAMETER :: configFile2 ='CATChem_config2.yml'
  TYPE(QFYAML_t)     :: Config, ConfigAnchored
  TYPE(QFYAML_t)     :: SpecConfig
  TYPE(OptInput), INTENT(INOUT) :: Input_Opt   ! Input options
  TYPE(GrdState), INTENT(INOUT) :: State_Grid  ! Grid State object


!  CHARACTER(LEN=255) :: thisLoc
  CHARACTER(LEN=512) :: errMsg

!  INTEGER :: GC_SUCCESS=1


!  INTEGER, PARAMETER :: GC_SUCCESS =  0   ! Routine returns success
!  INTEGER, PUBLIC, PARAMETER :: GC_FAILURE = -1   ! Routine returns failure

  INTEGER         :: RC=1          ! Success or failure


         WRITE( 6, '(a  )' ) REPEAT( '=', 79 )
         WRITE( 6, 100   ) TRIM( configFile )

100    FORMAT( 'READ_INPUT_FILE: Opening ', a )



! Assume success
!    RC      = GC_SUCCESS
!    errMsg  = ''
!    thisLoc = ' -> at Read_Input_File (in module GeosCore/input_mod.F90)'
!========================================================================
! Read the YAML file into the Config object
!========================================================================

!    print *, 'QFYAML_Init(', configFile, 'Config, ConfigAnchored, RC  )'
    CALL QFYAML_Init( configFile, Config, ConfigAnchored, RC )

    CALL QFYAML_Init( SpecFileName, SpecConfig, ConfigAnchored, RC )
    CALL QFYAML_Print( SpecConfig, RC, SpecFileName)


!    CALL QFYAML_Init( configFile, Config, ConfigAnchored, RC )
!    CALL QFYAML_Print( Config, RC, configFile2)


!    print *, "configFile = ", configFile
!    print *, "Config = ", config
!    print *, "configAnchored = ", ConfigAnchored
!    print *, 'RC = ',  RC


    IF ( RC /= 0 ) THEN
        errMsg = 'Error reading configuration file: ' // TRIM( configFile )
!        CALL GC_Error( errMsg, RC, thisLoc )
!        RETURN
!    print *, errMsg
    ENDIF





end program read_species_database







!> \\file test_main.F90


program test_main


  use QfYaml_Mod
  use GridState_Mod
  use Error_Mod
  use Input_Mod
  use Input_Opt_Mod, ONLY : OptInput
  use State_Grid_Mod
  implicit none


  CHARACTER(LEN=20), PARAMETER :: SpecFileName ='species_database.yml'
  CHARACTER(LEN=18), PARAMETER :: configFile ='CATChem_config.yml'
  TYPE(QFYAML_t)     :: Config, ConfigAnchored
  TYPE(QFYAML_t)     :: SpecConfig
  CHARACTER(LEN=255)          :: thisLoc
  TYPE(OptInput)    ::  Input_Opt
  TYPE(GrdState)    ::  State_Grid
  CHARACTER(LEN=512) :: errMsg

  INTEGER         :: RC=1          ! Success or failure

       WRITE( 6, '(a  )' ) REPEAT( '=', 79 )
       WRITE( 6, '(a,x,a)'   ) "CONFIGFILE: ", TRIM( configFile )

100    FORMAT( 'READ_INPUT_FILE: Opening ', a )


!========================================================================

! LDH:  This pulls from the configFile, refer to qfyaml.F90
    CALL QFYAML_Init( configFile, Config, ConfigAnchored, RC )

         WRITE( 6, '(a  )' ) REPEAT( '=', 79 )
         WRITE( 6, 100   ) TRIM( configFile )
         WRITE( 6, '(a  )' )  ' '


    CALL Config_Simulation( Config, Input_Opt, RC )    
! LDH: for possible settings, refer to input_mod.F90
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error in "Config_Simulation"!'
       CALL CC_Error( errMsg, RC, thisLoc  )
       CALL QFYAML_CleanUp( Config         )
       CALL QFYAML_CleanUp( ConfigAnchored )
       RETURN
    ENDIF

       WRITE( 6, '(a  )' ) " -- Simulation Settings --"
       WRITE( 6, '(a,x,a)' ) "Simulation Type:  ", Input_Opt%SimulationName
       WRITE( 6, '(a,x,a)' ) "Data dir:  ", Input_Opt%data_dir
       WRITE( 6, '(a,x,a)' ) "Met Field:  ", Input_Opt%metfield
       WRITE( 6, '(a,x,a)' ) "Species Database file:  ", Input_Opt%spcdatabasefile
       WRITE( 6, '(a  )' )  ' '

! LDH: for possible settings, refer to input_mod.F90
    CALL Config_Grid( Config, Input_Opt, State_Grid, RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error in "Config_Grid"!'
       CALL CC_Error( errMsg, RC, thisLoc  )
       CALL QFYAML_CleanUp( Config         )
       RETURN
    ENDIF
! LDH:  The grid specifications require recasting in input_mod.F90
         WRITE( 6, '(a  )' ) " -- Resolution Settings --"
         WRITE( 6, '(a,x,a)' ) "Resolution:  ", State_Grid%GridRes
         WRITE( 6, '(a,x,f7.3)' ) "DX:  ", State_Grid%DX
         WRITE( 6, '(a,x,f7.3)' ) "DY:  ", State_Grid%DY
         WRITE( 6, '(a,x,i4)' ) "NUMLEVELS:  ", State_Grid%NZ
         WRITE( 6, '(a,x,f9.4,x,f9.4)' ) "Lon Range:  ", State_Grid%Xmin, State_Grid%Xmax
         WRITE( 6, '(a,x,f9.4,x,f9.4)' ) "Lat Range:  ", State_Grid%Ymin, State_Grid%Ymax
         WRITE( 6, '(a  )' )  ' '

! LDH:  This pulls from the config_aerosol subroutine in input_mod.F90
    CALL Config_Aerosol( Config, Input_Opt, RC )
    IF ( RC /= CC_SUCCESS ) THEN
       errMsg = 'Error in "Config_Aerosol"!'
       CALL CC_Error( errMsg, RC, thisLoc  )
       CALL QFYAML_CleanUp( Config         )
       RETURN
    ENDIF
         WRITE( 6, '(a  )' ) " -- Aerosol Settings --"
         WRITE( 6, '(a,t30,l)' ) "Run Dust Aerosols?:  ", Input_Opt%ldust
         WRITE( 6, '(a,t30,l)' ) "Run Sea Salt Aerosols?:  ", Input_Opt%lssalt
         WRITE( 6, '(a  )' )  ' '
         WRITE( 6, '(a  )' ) REPEAT( '=', 79 )



! LDH:  Next - Define additional types and Allocate Arrays???
! LDH:       - Add additional relevant modules and checks as needed



end program test_main








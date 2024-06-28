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
   SUBROUTINE Read_Input_File( Config , GridState, EmisState, ChemState, RC )
!
! !USES:
!
      USE Error_Mod
      USE Config_Opt_Mod,  ONLY : ConfigType
      USE GridState_Mod, ONLY : GridStateType
      use ChemState_Mod, only : ChemStateType
      use EmisState_Mod, only : EmisStateType
!
! !INPUT/OUTPUT PARAMETERS:
!
      TYPE(ConfigType),    INTENT(INOUT) :: Config    ! Input options
      TYPE(GridStateType), INTENT(INOUT) :: GridState  ! Grid State object
      TYPE(ChemStateType), INTENT(inout) :: ChemState ! Chemical State
      TYPE(EmisStateType), INTENT(inout) :: EmisState ! Emission State
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
      character(LEN=512) :: filename

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

      call Config_Chem_State(config%SpcDatabaseFile, GridState,ChemState, RC)
      if (RC /= CC_SUCCESS) then
         errMsg = 'Error in "Config_Chem_State"!'
         CALL CC_Error( errMsg, RC, thisLoc  )
         CALL QFYAML_CleanUp( ConfigInput )
         CALL QFYAML_CleanUp( ConfigAnchored )
         RETURN
      endif

      call Config_Emis_State(config%EmissionDatabaseFile, EmisState, ChemState, RC)

      !========================================================================
      ! Further error-checking and initialization
      !========================================================================
      CALL QFYAML_CleanUp( ConfigInput )
      CALL QFYAML_CleanUp( ConfigAnchored )

   END SUBROUTINE Read_Input_File

   !> Reads the species database
   !!
   !! \param   filename The name of the species database
   !! \param   GridState The grid state object
   !! \param   ChemState The chemical state object
   !! \param   RC Return code
   !!
   !!!>
   SUBROUTINE Config_Chem_State( filename, GridState, ChemState, RC )
      USE ChemState_Mod, ONLY : ChemStateType, Find_Number_of_Species, Find_Indices_of_Species
      use Config_Opt_Mod, ONLY : ConfigType
      USE Error_Mod
      USE GridState_Mod, ONLY : GridStateType

      CHARACTER(LEN=*), INTENT(IN) :: filename
      TYPE(ChemStateType), INTENT(INOUT) :: ChemState
      TYPE(GridStateType), INTENT(IN) :: GridState
      INTEGER, INTENT(INOUT) :: RC

      TYPE(QFYAML_t)     :: ConfigInput, ConfigAnchored

      CHARACTER(LEN=255) :: thisLoc ! where am i
      CHARACTER(LEN=512) :: errMsg  ! error message
      character(len=QFYAML_NamLen), allocatable :: speciesName(:)
      integer :: n
      CHARACTER(LEN=QFYAML_NamLen) :: key
      CHARACTER(LEN=QFYAML_StrLen) :: v_str
      integer :: v_int
      real    :: v_real
      logical :: v_logical

      Character(len=17) :: tags(17)

      RC = CC_SUCCESS

      thisLoc = ' -> at Config_Chem_State (in module core/config_mod.F90)'

      tags = (/'name             ', &
         'long_name        ', &
         'description      ', &
         'lower_radius     ', &
         'upper_radius     ', &
         'radius           ', &
         'is_dust          ', &
         'is_sea_salt      ', &
         'is_tracer        ', &
         'is_aerosol       ', &
         'is_gas           ', &
         'is_advected      ', &
         'is_photolysis    ', &
         'mw_g             ', &
         'viscosity        ', &
         'density          ', &
         'BackgroundVV     '/)


      !========================================================================
      ! Initialize the QFYAML Species YAML object
      !========================================================================
      CALL QFYAML_Species_Init(filename, ConfigInput, ConfigAnchored, speciesName, RC )
      IF (RC /= 0) THEN
         errMsg = 'Error in "Config_Chem_State"!'
         call CC_Error(errMsg, RC, thisLoc)
         call QFYAML_CleanUp(ConfigInput)
         RETURN
      ENDIF

      ! Allocate the number of species for the ChemState
      if (size(speciesName) > 0) then
         ALLOCATE(ChemState%SpeciesNames(size(speciesName)), STAT=RC)
         IF (RC /= CC_SUCCESS) then
            errMsg = 'Error Allocating ChemState%SpeciesNames in "Config_Chem_State"!'
            call CC_Error(errMsg, RC, thisLoc)
            call QFYAML_CleanUp(ConfigInput)
            RETURN
         ENDIF

         ! Assign the The number of species
         ChemState%nSpecies = size(speciesName)

         ! Assign species names
         do n = 1, ChemState%nSpecies
            ChemState%SpeciesNames(n) = TRIM(speciesName(n))
         enddo

         ! Allocate the species
         ALLOCATE(ChemState%ChemSpecies(ChemState%nSpecies), STAT=RC)
         IF (RC /= CC_SUCCESS) then
            errMsg = 'Error Allocating ChemState%ChemSpecies in "Config_Chem_State"!'
            call CC_Error(errMsg, RC, thisLoc)
            call QFYAML_CleanUp(ConfigInput)
            RETURN
         ENDIF

      ENDIF

      ! Print Species

      write(*,*) '==============================='
      write(*,*) 'Chemical Species Settings:'
      write(*,*) '==============================='

      ! Loop over the species
      do n = 1, ChemState%nSpecies

         write(*,*) '| Species: ', TRIM(ChemState%SpeciesNames(n))
         !-----------------------------
         !  Initialize species Strings
         !-----------------------------

         key = TRIM(ChemState%SpeciesNames(n)) // '%' // 'name'
         v_str = MISSING_STR
         CALL QFYAML_Add_Get( ConfigInput, TRIM(key), v_str, "", RC )
         IF (RC /= CC_SUCCESS) then
            errMsg = 'Error Getting Species Name in "Config_Chem_State"!'
            call CC_Error(errMsg, RC, thisLoc)
            call QFYAML_CleanUp(ConfigInput)
            RETURN
         ENDIF
         ChemState%ChemSpecies(n)%short_name = TRIM(v_str)
         write(*,*) '|  short_name: ', TRIM(ChemState%ChemSpecies(n)%short_name)

         key = TRIM(ChemState%SpeciesNames(n)) // '%' // 'long_name'
         v_str = MISSING_STR
         CALL QFYAML_Add_Get( ConfigInput, TRIM(key), v_str, "", RC )
         IF (RC /= CC_SUCCESS) then
            ! assume that if the long name isn't in the species.yaml file that long_name = short_name
            errMsg = 'Error Getting Species Long Name in "Config_Chem_State"!'
            ChemState%ChemSpecies(n)%long_name = TRIM(ChemState%ChemSpecies(n)%short_name)
         else
            ChemState%ChemSpecies(n)%long_name = TRIM(v_str)
         ENDIF
         write(*,*) '|  long_name: ', TRIM(ChemState%ChemSpecies(n)%long_name)

         key = TRIM(ChemState%SpeciesNames(n)) // '%' // 'description'
         v_str = MISSING_STR
         CALL QFYAML_Add_Get( ConfigInput, TRIM(key), v_str, "", RC )
         IF (RC /= CC_SUCCESS) then
            ! assume description is None if it isn't in the species.yaml file
            ChemState%ChemSpecies(n)%description = 'None'
         ENDIF
         ChemState%ChemSpecies(n)%description = TRIM(v_str)
         write(*,*) '|  description: ', TRIM(ChemState%ChemSpecies(n)%description)

         !-----------------------------
         !  Initialize species booleans
         !-----------------------------

         key = TRIM(ChemState%SpeciesNames(n)) // '%' // 'is_gas'
         v_logical = MISSING_BOOL
         CALL QFYAML_Add_Get( ConfigInput, TRIM(key), v_logical, "", RC )
         IF (RC /= CC_SUCCESS) then
            ! assume that if is_gas isn't in the species.yaml file assume false
            ChemState%ChemSpecies(n)%is_gas = MISSING_BOOL
         ENDIF
         ChemState%ChemSpecies(n)%is_gas = v_logical
         write(*,*) '|  is_gas: ', ChemState%ChemSpecies(n)%is_gas

         key = TRIM(ChemState%SpeciesNames(n)) // '%' // 'is_aerosol'
         v_logical = MISSING_BOOL
         CALL QFYAML_Add_Get( ConfigInput, TRIM(key), v_logical, "", RC )
         IF (RC /= CC_SUCCESS) then
            ! assume that if is_aerosol isn't in the species.yaml file assume false
            ChemState%ChemSpecies(n)%is_aerosol = MISSING_BOOL
         ENDIF
         ChemState%ChemSpecies(n)%is_aerosol = v_logical
         write(*,*) '|  is_aerosol: ', ChemState%ChemSpecies(n)%is_aerosol

         key = TRIM(ChemState%SpeciesNames(n)) // '%' // 'is_tracer'
         v_logical = MISSING_BOOL
         CALL QFYAML_Add_Get( ConfigInput, TRIM(key), v_logical, "", RC )
         IF (RC /= CC_SUCCESS) then
            ! assume that if is_tracer isn't in the species.yaml file assume false
            ChemState%ChemSpecies(n)%is_tracer = MISSING_BOOL
         ENDIF
         ChemState%ChemSpecies(n)%is_tracer = v_logical
         write(*,*) '|  is_tracer: ', ChemState%ChemSpecies(n)%is_tracer

         key = TRIM(ChemState%SpeciesNames(n)) // '%' // 'is_advected'
         v_logical = MISSING_BOOL
         CALL QFYAML_Add_Get( ConfigInput, TRIM(key), v_logical, "", RC )
         IF (RC /= CC_SUCCESS) then
            ! assume that if is_advected isn't in the species.yaml file assume false
            ChemState%ChemSpecies(n)%is_advected = MISSING_BOOL
         ENDIF
         ChemState%ChemSpecies(n)%is_advected = v_logical
         write(*,*) '|  is_advected: ', ChemState%ChemSpecies(n)%is_advected

         key = TRIM(ChemState%SpeciesNames(n)) // '%' // 'is_drydep'
         v_logical = MISSING_BOOL
         CALL QFYAML_Add_Get( ConfigInput, TRIM(key), v_logical, "", RC )
         IF (RC /= CC_SUCCESS) then
            ! assume that if is_drydep isn't in the species.yaml file assume false
            ChemState%ChemSpecies(n)%is_drydep = MISSING_BOOL
         ENDIF
         ChemState%ChemSpecies(n)%is_drydep = v_logical
         write(*,*) '|  is_drydep: ', ChemState%ChemSpecies(n)%is_drydep

         key = TRIM(ChemState%SpeciesNames(n)) // '%' // 'is_photolysis'
         v_logical = MISSING_BOOL
         CALL QFYAML_Add_Get( ConfigInput, TRIM(key), v_logical, "", RC )
         IF (RC /= CC_SUCCESS) then
            ! assume that if is_photolysis isn't in the species.yaml file assume false
            ChemState%ChemSpecies(n)%is_photolysis = MISSING_BOOL
         ENDIF
         ChemState%ChemSpecies(n)%is_photolysis = v_logical
         write(*,*) '|  is_photolysis: ', ChemState%ChemSpecies(n)%is_photolysis

         key = TRIM(ChemState%SpeciesNames(n)) // '%' // 'is_dust'
         v_logical = MISSING_BOOL
         CALL QFYAML_Add_Get( ConfigInput, TRIM(key), v_logical, "", RC )
         IF (RC /= CC_SUCCESS) then
            ! assume that if is_dust isn't in the species.yaml file assume false
            ChemState%ChemSpecies(n)%is_dust = MISSING_BOOL
         ENDIF
         ChemState%ChemSpecies(n)%is_dust = v_logical
         write(*,*) '|  is_dust: ', ChemState%ChemSpecies(n)%is_dust

         key = TRIM(ChemState%SpeciesNames(n)) // '%' // 'is_seasalt'
         v_logical = MISSING_BOOL
         CALL QFYAML_Add_Get( ConfigInput, TRIM(key), v_logical, "", RC )
         IF (RC /= CC_SUCCESS) then
            ! assume that if is_seasalt isn't in the species.yaml file assume false
            ChemState%ChemSpecies(n)%is_seasalt = MISSING_BOOL
         ENDIF
         ChemState%ChemSpecies(n)%is_seasalt = v_logical
         write(*,*) '|  is_seasalt: ', ChemState%ChemSpecies(n)%is_seasalt

         !-----------------------------
         !  Initialize species reals
         !-----------------------------

         key = TRIM(ChemState%SpeciesNames(n)) // '%' // 'mw_g'
         v_real = MISSING_REAL
         CALL QFYAML_Add_Get( ConfigInput, TRIM(key), v_real, "", RC )
         IF (RC /= CC_SUCCESS) then
            if (ChemState%ChemSpecies(n)%is_gas .eqv. .false.) then
               ! assume that if mw_g isn't in the species.yaml file assume 0.0
               ChemState%ChemSpecies(n)%mw_g = MISSING_REAL
            else
               ! if is_gas mw_g must be present
               errMsg = 'MW_g required for gas species ' // TRIM(ChemState%SpeciesNames(n))
               CALL CC_Error( errMsg, RC, thisLoc )
               RETURN
            endif
         ENDIF
         ChemState%ChemSpecies(n)%mw_g = v_real
         write(*,*) '|  mw_g: ', ChemState%ChemSpecies(n)%mw_g

         key = TRIM(ChemState%SpeciesNames(n)) // '%' // 'density'
         v_real = MISSING_REAL
         CALL QFYAML_Add_Get( ConfigInput, TRIM(key), v_real, "", RC )
         IF (RC /= CC_SUCCESS) then
            if (ChemState%ChemSpecies(n)%is_aerosol .eqv. .false.) then
               ! assume that if density isn't in the species.yaml file assume 0.0
               ChemState%ChemSpecies(n)%density = MISSING_REAL
            else
               ! if is_aerosol density must be present
               errMsg = 'Density required for aerosol species ' // TRIM(ChemState%SpeciesNames(n))
               CALL CC_Error( errMsg, RC, thisLoc )
               RETURN
            endif
         ENDIF
         ChemState%ChemSpecies(n)%density = v_real
         write(*,*) '|  density: ', ChemState%ChemSpecies(n)%density

         key = TRIM(ChemState%SpeciesNames(n)) // '%' // 'radius'
         v_real = MISSING_REAL
         CALL QFYAML_Add_Get( ConfigInput, TRIM(key), v_real, "", RC )
         IF (RC /= CC_SUCCESS) then
            if (ChemState%ChemSpecies(n)%is_aerosol .eqv. .false.) then
               ! assume that if radius isn't in the species.yaml file assume 0.0
               ChemState%ChemSpecies(n)%radius = MISSING_REAL
            else
               ! if is_aerosol radius must be present
               errMsg = 'Radius required for aerosol species ' // TRIM(ChemState%SpeciesNames(n))
               CALL CC_Error( errMsg, RC, thisLoc )
               RETURN
            endif
         ENDIF
         ChemState%ChemSpecies(n)%radius = v_real
         write(*,*) '|  radius: ', ChemState%ChemSpecies(n)%radius

         key = TRIM(ChemState%SpeciesNames(n)) // '%' // 'lower_radius'
         v_real = MISSING_REAL
         CALL QFYAML_Add_Get( ConfigInput, TRIM(key), v_real, "", RC )
         IF (RC /= CC_SUCCESS) then
            if (ChemState%ChemSpecies(n)%is_aerosol .eqv. .false.) then
               ! assume that if lower_radius isn't in the species.yaml file assume 0.0
               ChemState%ChemSpecies(n)%lower_radius = MISSING_REAL
            else
               ! if is_aerosol lower_radius must be present
               errMsg = 'Lower_radius required for aerosol species ' // TRIM(ChemState%SpeciesNames(n))
               CALL CC_Error( errMsg, RC, thisLoc )
               RETURN
            endif
         ENDIF
         ChemState%ChemSpecies(n)%lower_radius = v_real
         write(*,*) '|  lower_radius: ', ChemState%ChemSpecies(n)%lower_radius

         key = TRIM(ChemState%SpeciesNames(n)) // '%' // 'upper_radius'
         v_real = MISSING_REAL
         CALL QFYAML_Add_Get( ConfigInput, TRIM(key), v_real, "", RC )
         IF (RC /= CC_SUCCESS) then
            if (ChemState%ChemSpecies(n)%is_aerosol .eqv. .false.) then
               ! assume that if upper_radius isn't in the species.yaml file assume 0.0
               ChemState%ChemSpecies(n)%upper_radius = MISSING_REAL
            else
               ! if is_aerosol upper_radius must be present
               errMsg = 'upper_radius required for aerosol species ' // TRIM(ChemState%SpeciesNames(n))
               CALL CC_Error( errMsg, RC, thisLoc )
               RETURN
            endif
         ENDIF
         ChemState%ChemSpecies(n)%upper_radius = v_real
         write(*,*) '|  upper_radius: ', ChemState%ChemSpecies(n)%upper_radius

         key = TRIM(ChemState%SpeciesNames(n)) // '%' // 'viscosity'
         v_real = MISSING_REAL
         CALL QFYAML_Add_Get( ConfigInput, TRIM(key), v_real, "", RC )
         IF (RC /= CC_SUCCESS) then
            if (ChemState%ChemSpecies(n)%is_gas .eqv. .false.) then
               ! assume that if viscosity isn't in the species.yaml file assume 0.0
               ChemState%ChemSpecies(n)%viscosity = MISSING_REAL
            else
               ! if is_gas viscosity must be present
               errMsg = 'viscosity required for aerosol species ' // TRIM(ChemState%SpeciesNames(n))
               CALL CC_Error( errMsg, RC, thisLoc )
               RETURN
            endif
         ENDIF
         ChemState%ChemSpecies(n)%viscosity = v_real
         write(*,*) '|  viscosity: ', ChemState%ChemSpecies(n)%viscosity

         !---------------------------------------
         ! Allocate initial Species Concentration
         !---------------------------------------
         ALLOCATE(ChemState%ChemSpecies(n)%conc(GridState%number_of_levels), STAT=RC)

      enddo ! n

      CALL Find_Number_of_Species(ChemState, RC)
      IF (RC /= CC_SUCCESS) THEN
         errMsg = 'Error in Find_Number_of_Species'
         CALL CC_Error( errMsg, RC, thisLoc )
         RETURN
      ENDIF

      CALL Find_Indices_of_Species(ChemState, RC)
      IF (RC /= CC_SUCCESS) THEN
         errMsg = 'Error in Find_Number_of_Species'
         CALL CC_Error( errMsg, RC, thisLoc )
         RETURN
      ENDIF

      write(*,*) '========================================================='
      write(*,*) '| Chemstate SUMMARY'
      write(*,*) '|  number_of_species:  ', ChemState%nSpecies
      write(*,*) '|  number_of_aerosols: ', ChemState%nSpeciesAero
      write(*,*) '|  number_of_gases:    ', ChemState%nSpeciesGas
      write(*,*) '|  number of tracers:  ', ChemState%nSpeciesTracer
      write(*,*) '|  number of dust:     ', ChemState%nSpeciesDust
      write(*,*) '|  number of seasalt:  ', ChemState%nSpeciesSeaSalt
      write(*,*) '========================================================='

   END SUBROUTINE Config_Chem_State

   SUBROUTINE Config_Emis_State( filename, EmisState, ChemState, RC )
      USE ChemState_Mod, ONLY : ChemStateType
      USE EmisState_Mod, ONLY : EmisStateType
      use Config_Opt_Mod, ONLY : ConfigType
      USE Error_Mod
      USE GridState_Mod, ONLY : GridStateType

      CHARACTER(LEN=*), INTENT(IN) :: filename
      TYPE(ChemStateType), INTENT(INOUT) :: ChemState
      TYPE(EmisStateType), INTENT(INOUT) :: EmisState
      INTEGER, INTENT(INOUT) :: RC

      TYPE(QFYAML_t)     :: ConfigInput, ConfigAnchored

      CHARACTER(LEN=255) :: thisLoc ! where am i
      CHARACTER(LEN=512) :: errMsg  ! error message
      character(len=QFYAML_NamLen), allocatable :: EmisCats(:)
      integer :: n, s, j
      CHARACTER(LEN=QFYAML_NamLen) :: key
      CHARACTER(LEN=QFYAML_NamLen) :: base
      CHARACTER(LEN=QFYAML_StrLen) :: v_str
      integer :: v_int
      real    :: v_real
      logical :: v_logical
      real, allocatable :: v_real_arr(:)
      integer :: arr_size
      CHARACTER(LEN=QFYAML_NamLen), pointer :: v_str_arr(:)

      Character(len=17) :: tags(5)

      RC = CC_SUCCESS

      thisLoc = ' -> at Config_Emis_State (in module core/config_mod.F90)'

      tags = (/'name             ', &
         'long_name        ', &
         'scale            ', &
         'units            ', &
         'EmisFlux         ' /)

      !========================================================================
      ! Initialize the QFYAML Species YAML object
      !========================================================================
      CALL QFYAML_Emis_Init(filename, ConfigInput, ConfigAnchored, EmisState, RC )
      IF (RC /= 0) THEN
         errMsg = 'Error in "Config_Chem_State"!'
         call CC_Error(errMsg, RC, thisLoc)
         call QFYAML_CleanUp(ConfigInput)
         RETURN
      ENDIF

      do n = 1, EmisState%nEmisCategories
         do s = 1, EmisState%EmisCats(n)%nEmisSpecies
            base = TRIM(EmisState%EmisCats(n)%name) // '%' // TRIM(EmisState%EmisCats(n)%EmisSpecies(s)%name)

            ! get long_name of emission species in the category
            key =  TRIM(base) // '%long_name'
            v_str = MISSING_STR
            CALL QFYAML_Add_Get( ConfigInput, TRIM( key ), v_str, "", RC )
            IF ( RC /= CC_SUCCESS ) THEN
               v_str = EmisState%EmisCats(n)%EmisSpecies(s)%name
            ENDIF
            EmisState%EmisCats(n)%EmisSpecies(s)%long_name = TRIM( v_str )

            ! get units of emission species in the category
            key =  TRIM(base) // '%units'
            v_str = MISSING_STR
            CALL QFYAML_Add_Get( ConfigInput, TRIM( key ), v_str, "", RC )
            IF ( RC /= CC_SUCCESS ) THEN
               v_str = "kg m-2 s-1" ! assume these units for now
            else if (v_str == MISSING_STR) then
               v_str = "kg m-2 s-1"
            endif
            EmisState%EmisCats(n)%EmisSpecies(s)%units = TRIM( v_str )

            ! Get the mapping of the emission species to the mechanism species
            key = TRIM(base) // '%map'
            v_str = MISSING_STR
            call QFYAML_Add_Get( ConfigInput, TRIM( key ), v_str, "", RC )
            if (RC /= CC_SUCCESS) then
               errMsg = 'Error in QFYAML_Add_Get ' // TRIM(key)
               call CC_Error(errMsg, RC, thisLoc)
               call QFYAML_CleanUp(ConfigInput)
               call QFYAML_CleanUp(ConfigAnchored)
               RETURN
            endif
            call QFYAML_String_to_String_Arr(v_str,                 &
               EmisState%EmisCats(n)%EmisSpecies(s)%EmisMapName,    &
               EmisState%EmisCats(n)%EmisSpecies(s)%nEmisMap,     &
               RC)
            if (RC /= CC_SUCCESS) then
               errMsg = 'Error in QFYAML_String_to_Real_Arr'
               call CC_Error(errMsg, RC, thisLoc)
               call QFYAML_CleanUp(ConfigInput)
               call QFYAML_CleanUp(ConfigAnchored)
               RETURN
            endif
            j = EmisState%EmisCats(n)%EmisSpecies(s)%nEmisMap ! temporary

            ! Get the scaling of the emissions to mapped species
            key = TRIM(base) // '%scale'
            v_str = MISSING_STR
            call QFYAML_Add_Get( ConfigInput, TRIM( key ), v_str, "", RC )
            if (RC /= CC_SUCCESS) then
               errMsg = 'Error in QFYAML_Add_Get ' // TRIM(key)
               call CC_Error(errMsg, RC, thisLoc)
               call QFYAML_CleanUp(ConfigInput)
               call QFYAML_CleanUp(ConfigAnchored)
               RETURN
            endif
            if (v_str == MISSING_STR) then
               Allocate(EmisState%EmisCats(n)%EmisSpecies(s)%EmisScale(j), STAT=RC)
               if (RC /= CC_SUCCESS) then
                  errMsg = 'Error allocating EmisState%EmisCats(n)%EmisSpecies(s)%EmisScale'
                  call CC_Error(errMsg, RC, thisLoc)
                  call QFYAML_CleanUp(ConfigInput)
                  call QFYAML_CleanUp(ConfigAnchored)
                  RETURN
               endif
               EmisState%EmisCats(n)%EmisSpecies(s)%EmisScale = 1.0_fp
            else
               call QFYAML_String_to_Real_Arr(v_str,                 &
                  EmisState%EmisCats(n)%EmisSpecies(s)%EmisScale,    &
                  EmisState%EmisCats(n)%EmisSpecies(s)%nEmisMap,     &
                  RC)
               if (RC /= CC_SUCCESS) then
                  errMsg = 'Error in QFYAML_String_to_Real_Arr'
                  call CC_Error(errMsg, RC, thisLoc)
                  call QFYAML_CleanUp(ConfigInput)
                  call QFYAML_CleanUp(ConfigAnchored)
                  RETURN
               endif
            endif

         enddo
      enddo

      !========================================================================
      ! Print EmisState
      !========================================================================
      write(*,*) '==================================================='
      write(*,*) 'Emission Settings:'
      write(*,*) '==================================================='

      write(*,*) '| nEmisCategories: ', EmisState%nEmisCategories
      do n = 1, EmisState%nEmisCategories
         write(*,*) '| Category: ', TRIM(EmisState%EmisCats(n)%name)
         do s = 1, EmisState%EmisCats(n)%nEmisSpecies
            write(*,*) '|   Species:     ' // TRIM(EmisState%EmisCats(n)%EmisSpecies(s)%name)
            write(*,*) '|     long_name: ' // TRIM(EmisState%EmisCats(n)%EmisSpecies(s)%long_name)
            write(*,*) '|     units:     ' // TRIM(EmisState%EmisCats(n)%EmisSpecies(s)%units)
            write(*,*) '|     nEmisMap:  ', EmisState%EmisCats(n)%EmisSpecies(s)%nEmisMap
            do j = 1, EmisState%EmisCats(n)%EmisSpecies(s)%nEmisMap
               write(*,*) '|       Emission Mapping:  ' // TRIM(EmisState%EmisCats(n)%EmisSpecies(s)%EmisMapName(j)) &
                  // ' -> ', EmisState%EmisCats(n)%EmisSpecies(s)%EmisScale(j)
            enddo
         enddo
      enddo
      write(*,*) '==================================================='

      ! Cleanup the QFYAML objects
      CALL QFYAML_CleanUp(ConfigInput)
      CALL QFYAML_CleanUp(ConfigAnchored)

   END SUBROUTINE Config_Emis_State


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

      key   = "simulation%species_filename"
      v_str = MISSING_STR
      CALL QFYAML_Add_Get( ConfigInput, TRIM( key ), v_str, "", RC )
      IF ( RC /= CC_SUCCESS ) THEN
         errMsg = 'Error parsing ' // TRIM( key ) // '!'
         CALL CC_Error( errMsg, RC, thisLoc )
         RETURN
      ENDIF
      Config%SpcDatabaseFile = TRIM( v_str )

      key   = "simulation%emission_filename"
      v_str = MISSING_STR
      CALL QFYAML_Add_Get( ConfigInput, TRIM( key ), v_str, "", RC )
      IF ( RC /= CC_SUCCESS ) THEN
         errMsg = 'Error parsing ' // TRIM( key ) // '!'
         CALL CC_Error( errMsg, RC, thisLoc )
         RETURN
      ENDIF
      Config%EmissionDatabaseFile = TRIM( v_str )


      ! Return success
      RC = CC_SUCCESS

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

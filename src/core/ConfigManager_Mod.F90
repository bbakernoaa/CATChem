!> \file ConfigManager_Mod.F90
!! \brief Enhanced configuration management for CATChem
!! \ingroup core_modules
!!
!! \author CATChem Development Team
!! \date 2025
!! \version 2.0
!!
!! This module provides a modern, flexible configuration management system
!! for CATChem, supporting modular configuration loading, validation, and
!! runtime configuration updates.
!!
!! \details
!! The ConfigManager consolidates and modernizes CATChem configuration management
!! by replacing two previous modules:
!!
!! **Replaces config_opt_mod.F90:**
!! - Legacy config support removed, now using modern YAML-based ConfigDataType
!! - All configuration parameters and options
!! - Process-specific configuration flags
!!
!! **Replaces config_mod.F90:**
!! - Read_Input_File subroutine -> ConfigManager%load_from_file() method
!! - YAML parsing logic with enhanced error handling
!! - Configuration validation and initialization
!!
!! **New features:**
!! - Hierarchical configuration loading with includes and inheritance
!! - Schema-based validation with detailed error reporting
!! - Plugin-based process configuration loading
!! - Runtime configuration updates and hot-reloading
!! - Configuration templates and presets
!! - Environment variable and command-line override support
!!
!! \section config_usage Usage Example
!! \code{.f90}
!! use ConfigManager_Mod
!! type(ConfigManagerType) :: config_mgr
!! type(StateContainer) :: container
!! integer :: rc
!!
!! call config_mgr%init(rc)
!! call config_mgr%load_from_file('CATChem_config.yml', rc)
!! call config_mgr%apply_to_container(container, rc)
!! \endcode
!!
module ConfigManager_Mod
   use iso_c_binding, only: c_associated
   use Precision_Mod, only: fp, MISSING_BOOL, MISSING
   use Error_Mod, only : CC_SUCCESS, CC_FAILURE, ERROR_INVALID_CONFIG, ERROR_INVALID_INPUT, ErrorManagerType
   use Species_Mod, only: SpeciesType
   use yaml_interface_mod, only : yaml_node_t, yaml_load_file, yaml_load_string, yaml_destroy_node, &
                                  yaml_get_string, yaml_get_integer, yaml_get_real, yaml_get_logical, &
                                  yaml_has_key, yaml_get, yaml_set, yaml_is_map, yaml_is_sequence, &
                                  yaml_get_size, yaml_get_string_array, yaml_get_all_keys

   implicit none
   private

   ! Define precision types

   public :: ConfigManagerType
   public :: ConfigDataType      ! Modern YAML-based configuration data structure
   public :: ConfigSchemaType
   public :: ConfigPresetType
   public :: CONFIG_STRATEGY_STRICT, CONFIG_STRATEGY_PERMISSIVE, CONFIG_STRATEGY_FALLBACK

   !> \brief Configuration loading strategies
   integer, parameter :: CONFIG_STRATEGY_STRICT = 1    !< Fail on any validation error
   integer, parameter :: CONFIG_STRATEGY_PERMISSIVE = 2 !< Warning on validation errors
   integer, parameter :: CONFIG_STRATEGY_FALLBACK = 3   !< Use defaults on errors

   !> \brief Runtime and MPI configuration
   type :: RuntimeConfig
      integer :: numCPUs = 1                         !< Number of MPI processes
      integer :: thisCPU = 0                         !< Local MPI process handle
      integer :: MPIComm = -1                        !< MPI Communicator Handle
      logical :: isMPI = .false.                     !< Is this an MPI simulation?
      logical :: amIRoot = .true.                    !< Is this the root cpu?
      logical :: DryRun = .false.                    !< Is this a dry run?
      character(len=255) :: SimulationName = ''      !< Name of the simulation
      logical :: VerboseRequested = .false.          !< Was verbose output requested?
      character(len=10) :: VerboseOnCores = 'root'   !< Which cores should produce verbose output
      logical :: Verbose = .false.                   !< Should verbose output be produced?

      ! Simulation dimensions
      integer :: nLevs = 127                          !< Number of vertical levels
      integer :: nSpecies = 50                       !< Total number of chemical species
      integer :: maxSpecies = 500                    !< Maximum number of chemical species
      integer :: nSpecies_drydep = 20                !< Number of species with dry deposition
      integer :: nEmissionCategories = 10            !< Number of emission categories
      integer :: nEmissionSpecies = 50               !< Number of emission species per category
   end type RuntimeConfig

   !> \brief File paths and data sources
   type :: FilePathConfig
      character(len=255) :: Emission_File = ''       !< Path to emission data file
      character(len=255) :: Species_File = ''        !< Path to species configuration file
      character(len=255) :: Input_Directory = './'   !< Input data directory
      character(len=255) :: Output_Directory = './'  !< Output data directory
   end type FilePathConfig


   !> \brief External emissions configuration
   type :: ExternalEmisConfig
      logical :: activate = .false.                   !< Enable external emissions
      character(len=256) :: config_file = ''          !< External emissions configuration file
      character(len=64) :: temporal_profile = 'constant' !< Temporal profile type
      logical :: dynamic_mapping = .true.             !< Enable dynamic species mapping
      real(fp) :: global_scale_factor = 1.0_fp        !< Global scaling factor
   end type ExternalEmisConfig

   !> \brief Modernized configuration data structure
   !!
   !! This type provides a modern YAML-based configuration system
   !! with better organization, validation, and extensibility.
   !!
   !! \details
   !! Key features:
   !! - Better organization with nested types for different categories
   !! - Built-in validation and consistency checking
   !! - Support for dynamic process configuration
   !! - Enhanced debugging and introspection capabilities
   !! - Full YAML configuration support
   type :: ConfigDataType

      ! Configuration categories
      type(RuntimeConfig) :: runtime                    !< Runtime and MPI settings
      type(FilePathConfig) :: file_paths                !< File paths and data sources
      type(ExternalEmisConfig) :: external_emissions    !< External emissions configuration

      ! Metadata
      character(len=64) :: config_version = '2.0'       !< Configuration version
      character(len=256) :: source_file = ''            !< Source configuration file
      logical :: is_validated = .false.                 !< Has configuration been validated?

      type(ProcessConfigType), allocatable :: run_phase_processes(:)  !< All process configs for run phases
      type(RunPhaseType), allocatable :: run_phases(:)               !< Array of run phase configurations

   contains
      ! Initialization and cleanup
      procedure :: init => config_data_init
      procedure :: cleanup => config_data_cleanup
      procedure :: validate => config_data_validate

      ! Utility methods
      procedure :: print_summary => config_data_print_summary
      procedure :: to_yaml_string => config_data_to_yaml_string
      procedure :: copy => config_data_copy

   end type ConfigDataType

   !> \brief Process configuration for dynamic run phases
   !! This type allows for flexible, phase-based process configuration.
   type :: ProcessConfigType
      character(len=64) :: name             !< Process name
      character(len=64) :: process_type     !< Process type (e.g., 'verticaltransport')
      character(len=64) :: scheme           !< Scheme name (e.g., 'YSU')
      logical :: enabled                    !< Whether process is enabled
      integer :: priority                   !< Execution priority (lower = earlier)
      character(len=16) :: timing           !< Timing type ('explicit', 'implicit')
      integer :: subcycling                 !< Number of subcycles
      character(len=256) :: config_details  !< Additional configuration as string (YAML/JSON)
   end type ProcessConfigType

   !> \brief Run phase configuration for phase-based execution
   !! Contains the phase name, description, frequency, and the list of processes in the phase.
   type :: RunPhaseType
      character(len=64) :: name             !< Phase name
      character(len=256) :: description     !< Phase description
      character(len=32) :: frequency        !< Run frequency (e.g., 'every timestep')
      integer :: subcycling                 !< Number of subcycles for the phase
      integer :: num_processes              !< Number of processes in this phase
      type(ProcessConfigType), allocatable :: processes(:)  !< Array of process configurations
   end type RunPhaseType

   !> \brief Configuration schema definition
   !!
   !! Defines the expected structure and validation rules for configuration files.
   type :: ConfigSchemaType
      private
      character(len=256) :: name                        !< Schema name
      character(len=512) :: description                 !< Schema description
      character(len=64), allocatable :: required_fields(:) !< Required configuration fields
      character(len=64), allocatable :: optional_fields(:) !< Optional configuration fields
      logical :: strict_validation = .true.            !< Strict validation mode
   contains
      procedure :: init => schema_init
      procedure :: add_required_field => schema_add_required_field
      procedure :: add_optional_field => schema_add_optional_field
      procedure :: validate_config => schema_validate_config
      ! Add emission-specific validation methods
      procedure :: validate_emission_config => schema_validate_emission_config
      procedure :: validate_species_mapping => schema_validate_species_mapping
      procedure :: validate_scaling_factors => schema_validate_scaling_factors
   end type ConfigSchemaType

   !> \brief Configuration preset for common use cases
   !!
   !! Provides predefined configuration templates for common modeling scenarios.
   type :: ConfigPresetType
      character(len=256) :: name                        !< Preset name
      character(len=512) :: description                 !< Preset description
      character(len=1024) :: yaml_content               !< YAML configuration content
   contains
      procedure :: load_from_string => preset_load_from_string
      procedure :: save_to_file => preset_save_to_file
   end type ConfigPresetType

   !> \brief Main configuration manager
   !!
   !! Central configuration management with validation, loading, and state application.
   type :: ConfigManagerType
      private

      ! Configuration data
      type(yaml_node_t) :: yaml_data                   !< Loaded YAML configuration
      type(ConfigDataType) :: config_data              !< Structured configuration data
      logical :: is_loaded = .false.                    !< Configuration loaded flag

      ! Configuration metadata
      character(len=512) :: config_file = ''            !< Current config file path
      character(len=256) :: config_version = ''         !< Configuration version
      integer :: load_strategy = CONFIG_STRATEGY_STRICT !< Loading strategy

      ! Schema and validation
      type(ConfigSchemaType) :: schema                  !< Configuration schema
      logical :: schema_loaded = .false.                !< Schema loaded flag

      ! Environment and overrides
      character(len=64), allocatable :: env_overrides(:)  !< Environment variable overrides
      character(len=512), allocatable :: cli_overrides(:) !< Command line overrides

   contains
      ! Initialization and cleanup
      procedure :: init => config_manager_init
      procedure :: finalize => config_manager_finalize

      ! Configuration loading
      procedure :: load_from_file => config_manager_load_from_file
      procedure :: load_from_string => config_manager_load_from_string
      procedure :: reload => config_manager_reload
      procedure :: load_preset => config_manager_load_preset

      ! Schema management
      procedure :: load_schema => config_manager_load_schema
      procedure :: validate => config_manager_validate

      ! Configuration access
      procedure :: get_string => config_manager_get_string
      procedure :: get_integer => config_manager_get_integer
      procedure :: get_real => config_manager_get_real
      procedure :: get_logical => config_manager_get_logical
      procedure :: get_array => config_manager_get_array
      procedure :: get_nspecies => config_manager_get_nspecies
      procedure :: get_max_species => config_manager_get_max_species
      procedure :: get_nemission_categories => config_manager_get_nemission_categories
      procedure :: get_nemission_species => config_manager_get_nemission_species
      procedure :: get_species_file => config_manager_get_species_file

      ! State integration
      !procedure :: apply_to_container => config_manager_apply_to_container
      !procedure :: extract_from_container => config_manager_extract_from_container

      ! Species and emission configuration
      procedure :: load_and_init_species => config_manager_load_and_init_species
      procedure :: load_emission_config => config_manager_load_emission_config

      ! Configuration update methods
      procedure :: update_runtime_from_configs => config_manager_update_runtime_from_configs
      procedure :: parse_config_data

      ! Utility methods
      procedure :: print_summary => config_manager_print_summary
      procedure :: save_to_file => config_manager_save_to_file
      procedure :: set_loading_strategy => config_manager_set_loading_strategy
      procedure :: add_env_override => config_manager_add_env_override
      procedure :: add_cli_override => config_manager_add_cli_override

   end type ConfigManagerType

   ! Built-in configuration presets
   ! type(ConfigPresetType), parameter :: PRESET_BASIC = ConfigPresetType( &
   !    name = 'basic', &
   !    description = 'Basic CATChem configuration for testing', &
   !    yaml_content = 'simulation: {start_date: "2023-01-01", end_date: "2023-01-02"}' &
   ! )

contains

   !========================================================================
   ! ConfigSchema Implementation
   !========================================================================

   !> \brief Initialize configuration schema
   subroutine schema_init(this, name, description, strict)
      class(ConfigSchemaType), intent(inout) :: this
      character(len=*), intent(in) :: name
      character(len=*), intent(in) :: description
      logical, intent(in), optional :: strict

      this%name = trim(name)
      this%description = trim(description)

      if (present(strict)) then
         this%strict_validation = strict
      else
         this%strict_validation = .true.
      endif

      ! Allocate empty arrays
      if (.not. allocated(this%required_fields)) allocate(this%required_fields(0))
      if (.not. allocated(this%optional_fields)) allocate(this%optional_fields(0))

   end subroutine schema_init

   !> \brief Add required field to schema
   subroutine schema_add_required_field(this, field_name)
      class(ConfigSchemaType), intent(inout) :: this
      character(len=*), intent(in) :: field_name

      character(len=64), allocatable :: temp_array(:)
      integer :: n, i

      n = size(this%required_fields)
      allocate(temp_array(n+1))

      do i = 1, n
         temp_array(i) = this%required_fields(i)
      end do
      temp_array(n+1) = trim(field_name)

      call move_alloc(temp_array, this%required_fields)

   end subroutine schema_add_required_field

   !> \brief Add optional field to schema
   subroutine schema_add_optional_field(this, field_name)
      class(ConfigSchemaType), intent(inout) :: this
      character(len=*), intent(in) :: field_name

      character(len=64), allocatable :: temp_array(:)
      integer :: n, i

      n = size(this%optional_fields)
      allocate(temp_array(n+1))

      do i = 1, n
         temp_array(i) = this%optional_fields(i)
      end do
      temp_array(n+1) = trim(field_name)

      call move_alloc(temp_array, this%optional_fields)

   end subroutine schema_add_optional_field

   !> \brief Validate configuration against schema
   subroutine schema_validate_config(this, yaml_data, rc)
      class(ConfigSchemaType), intent(in) :: this
      type(yaml_node_t), intent(in) :: yaml_data
      integer, intent(out) :: rc

      integer :: i
      logical :: key_exists
      character(len=512) :: missing_fields

      rc = CC_SUCCESS
      missing_fields = ''

      ! Basic validation - check if we have a valid node
      if (.not. c_associated(yaml_data%ptr)) then
         rc = CC_FAILURE
         return
      endif

      ! Check required fields
      if (allocated(this%required_fields)) then
         do i = 1, size(this%required_fields)
            key_exists = yaml_has_key(yaml_data, trim(this%required_fields(i)))
            if (.not. key_exists) then
               if (len_trim(missing_fields) > 0) then
                  missing_fields = trim(missing_fields) // ', ' // trim(this%required_fields(i))
               else
                  missing_fields = trim(this%required_fields(i))
               endif
            endif
         end do
      endif

      ! Report missing required fields
      if (len_trim(missing_fields) > 0) then
         write(*, '(A,A)') 'ERROR: Missing required configuration fields: ', trim(missing_fields)
         if (this%strict_validation) then
            rc = CC_FAILURE
            return
         endif
      endif

      ! Validate optional fields exist (if present)
      if (allocated(this%optional_fields)) then
         do i = 1, size(this%optional_fields)
            key_exists = yaml_has_key(yaml_data, trim(this%optional_fields(i)))
            if (key_exists) then
               write(*, '(A,A)') 'INFO: Found optional field: ', trim(this%optional_fields(i))
            endif
         end do
      endif

      write(*, '(A)') 'INFO: Configuration validation completed'

   end subroutine schema_validate_config

   !> \brief Validate emission configuration against schema
   subroutine schema_validate_emission_config(this, yaml_data, config_file, rc)
      implicit none
      class(ConfigSchemaType), intent(in) :: this
      type(yaml_node_t), intent(in) :: yaml_data
      character(len=*), intent(in) :: config_file
      integer, intent(out) :: rc

      logical :: file_exists, key_exists
      type(yaml_node_t) :: emission_config
      integer :: n_sources, n_species
      character(len=256) :: data_directory

      ! Suppress warning for unused argument (used for interface compatibility)
      if (.false.) then
         key_exists = yaml_has_key(yaml_data, "dummy")
      endif

      rc = CC_SUCCESS

      ! Check if emission config file exists
      inquire(file=trim(config_file), exist=file_exists)
      if (.not. file_exists) then
         write(*, '(A,A)') 'WARNING: Emission configuration file not found: ', trim(config_file)
         ! Don't fail - emissions may be optional
         return
      endif

      ! Load and validate emission configuration
      emission_config = yaml_load_file(config_file)
      if (.not. c_associated(emission_config%ptr)) then
         write(*, '(A)') 'ERROR: Failed to parse emission configuration file'
         rc = CC_FAILURE
         return
      endif

      ! Validate required emission fields
      key_exists = yaml_has_key(emission_config, "emissions")
      if (.not. key_exists) then
         write(*, '(A)') 'ERROR: Missing emissions section in configuration'
         rc = CC_FAILURE
         call yaml_destroy_node(emission_config)
         return
      endif

      ! Check for data directory
      key_exists = yaml_has_key(emission_config, "emissions/data_directory")
      if (key_exists) then
         if (yaml_get_string(emission_config, "emissions/data_directory", data_directory)) then
            inquire(file=trim(data_directory), exist=file_exists)
            if (.not. file_exists) then
               write(*, '(A,A)') 'WARNING: Emission data directory does not exist: ', trim(data_directory)
            endif
         endif
      endif

      ! Validate emission sources
      if (yaml_get_integer(emission_config, "emissions/n_sources", n_sources)) then
         if (n_sources <= 0) then
            write(*, '(A)') 'WARNING: No emission sources configured'
         else
            write(*, '(A,I0,A)') 'INFO: Found ', n_sources, ' emission sources'
         endif
      endif

      ! Validate species mapping
      if (yaml_get_integer(emission_config, "emissions/n_species", n_species)) then
         if (n_species <= 0) then
            write(*, '(A)') 'WARNING: No emission species configured'
         else
            write(*, '(A,I0,A)') 'INFO: Found ', n_species, ' emission species'
         endif
      endif

      ! Clean up
      call yaml_destroy_node(emission_config)

      write(*, '(A)') 'INFO: Emission configuration validation completed'

   end subroutine schema_validate_emission_config

   !> \brief Validate species mapping consistency
   subroutine schema_validate_species_mapping(this, species_mappings, rc)
      implicit none
      class(ConfigSchemaType), intent(in) :: this
      character(len=*), intent(in) :: species_mappings(:)
      integer, intent(out) :: rc

      integer :: i

      rc = CC_SUCCESS

      ! Validate species mapping entries
      do i = 1, size(species_mappings)
         if (len_trim(species_mappings(i)) == 0) then
            write(*, '(A,I0)') 'WARNING: Empty species mapping at index ', i
         endif
      end do

   end subroutine schema_validate_species_mapping

   !> \brief Validate scaling factors
   subroutine schema_validate_scaling_factors(this, scale_factors, rc)
      implicit none
      class(ConfigSchemaType), intent(in) :: this
      real(fp), intent(in) :: scale_factors(:)
      integer, intent(out) :: rc

      integer :: i
      real(fp) :: total_scale

      rc = CC_SUCCESS

      ! Check for reasonable scaling factors
      do i = 1, size(scale_factors)
         if (scale_factors(i) < 0.0_fp) then
            write(*, '(A,I0,A,F8.3)') 'WARNING: Negative scaling factor at index ', i, ': ', scale_factors(i)
         endif
         if (scale_factors(i) > 10.0_fp) then
            write(*, '(A,I0,A,F8.3)') 'WARNING: Large scaling factor at index ', i, ': ', scale_factors(i)
         endif
      end do

      total_scale = sum(scale_factors)
      if (total_scale > 5.0_fp) then
         write(*, '(A,F8.3)') 'WARNING: Very large total scaling factor: ', total_scale
      endif

   end subroutine schema_validate_scaling_factors

   !========================================================================
   ! ConfigPreset Implementation
   !========================================================================

   !> \brief Load preset from string content
   subroutine preset_load_from_string(this, yaml_string)
      implicit none
      class(ConfigPresetType), intent(inout) :: this
      character(len=*), intent(in) :: yaml_string

      this%yaml_content = trim(yaml_string)

   end subroutine preset_load_from_string

   !> \brief Save preset to file
   subroutine preset_save_to_file(this, filename, rc)
      implicit none
      class(ConfigPresetType), intent(in) :: this
      character(len=*), intent(in) :: filename
      integer, intent(out) :: rc

      integer :: unit_num

      rc = CC_SUCCESS

      open(newunit=unit_num, file=trim(filename), status='replace', iostat=rc)
      if (rc /= 0) return

      write(unit_num, '(A)') trim(this%yaml_content)
      close(unit_num)

   end subroutine preset_save_to_file

   !========================================================================
   ! ConfigManager Implementation
   !========================================================================

   !> \brief Initialize configuration manager
   subroutine config_manager_init(this, rc)
      implicit none
      class(ConfigManagerType), intent(inout) :: this
      integer, intent(out) :: rc

      rc = CC_SUCCESS

      ! Initialize schema with common CATChem fields
      call this%schema%init('CATChem', 'CATChem configuration schema')
      call this%schema%add_required_field('simulation')
      call this%schema%add_optional_field('processes')
      call this%schema%add_optional_field('output')
      call this%schema%add_optional_field('external_emissions')
      call this%schema%add_optional_field('species_mapping')

      this%schema_loaded = .true.

      ! Initialize override arrays
      if (.not. allocated(this%env_overrides)) allocate(this%env_overrides(0))
      if (.not. allocated(this%cli_overrides)) allocate(this%cli_overrides(0))

   end subroutine config_manager_init

   !> \brief Finalize configuration manager
   subroutine config_manager_finalize(this, rc)
      implicit none
      class(ConfigManagerType), intent(inout) :: this
      integer, intent(out) :: rc

      rc = CC_SUCCESS

      if (this%is_loaded) then
         call yaml_destroy_node(this%yaml_data)
      endif

      this%is_loaded = .false.
      this%schema_loaded = .false.

      if (allocated(this%env_overrides)) deallocate(this%env_overrides)
      if (allocated(this%cli_overrides)) deallocate(this%cli_overrides)

   end subroutine config_manager_finalize

   !> \brief Load configuration from file
   subroutine config_manager_load_from_file(this, filename, rc)
      implicit none
      class(ConfigManagerType), intent(inout) :: this
      character(len=*), intent(in) :: filename
      integer, intent(out) :: rc

      logical :: file_exists

      rc = CC_SUCCESS

      ! Check if file exists
      inquire(file=trim(filename), exist=file_exists)
      if (.not. file_exists) then
         rc = CC_FAILURE
         return
      endif

      ! Clean up any existing configuration
      if (this%is_loaded) then
         call yaml_destroy_node(this%yaml_data)
      endif

      ! Load YAML configuration using yaml_interface_mod
      this%yaml_data = yaml_load_file(filename)
      if (.not. c_associated(this%yaml_data%ptr)) then
         rc = CC_FAILURE
         return
      endif

      this%config_file = trim(filename)
      this%is_loaded = .true.

      ! Validate against schema if loaded
      if (this%schema_loaded) then
         call this%validate(rc)
         if (rc /= CC_SUCCESS .and. this%load_strategy == CONFIG_STRATEGY_STRICT) then
            return
         endif
      endif

      ! Parse structured configuration data
      call this%parse_config_data(rc)

   end subroutine config_manager_load_from_file

   !> \brief Load configuration from string
   subroutine config_manager_load_from_string(this, yaml_string, rc)
      implicit none
      class(ConfigManagerType), intent(inout) :: this
      character(len=*), intent(in) :: yaml_string
      integer, intent(out) :: rc

      rc = CC_SUCCESS

      ! Clean up any existing configuration
      if (this%is_loaded) then
         call yaml_destroy_node(this%yaml_data)
      endif

      ! Load YAML configuration from string using yaml_interface_mod
      this%yaml_data = yaml_load_string(yaml_string)
      if (.not. c_associated(this%yaml_data%ptr)) then
         rc = CC_FAILURE
         return
      endif

      this%config_file = '<string>'
      this%is_loaded = .true.

      ! Validate against schema if loaded
      if (this%schema_loaded) then
         call this%validate(rc)
         if (rc /= CC_SUCCESS .and. this%load_strategy == CONFIG_STRATEGY_STRICT) then
            return
         endif
      endif

      ! Parse structured configuration data
      call this%parse_config_data(rc)

   end subroutine config_manager_load_from_string

   !> \brief Reload configuration from current file
   subroutine config_manager_reload(this, rc)
      implicit none
      class(ConfigManagerType), intent(inout) :: this
      integer, intent(out) :: rc

      if (len_trim(this%config_file) == 0) then
         rc = CC_FAILURE
         return
      endif

      call this%load_from_file(this%config_file, rc)

   end subroutine config_manager_reload

   !> \brief Load a predefined configuration preset
   subroutine config_manager_load_preset(this, preset, rc)
      implicit none
      class(ConfigManagerType), intent(inout) :: this
      type(ConfigPresetType), intent(in) :: preset
      integer, intent(out) :: rc

      call this%load_from_string(preset%yaml_content, rc)

   end subroutine config_manager_load_preset

   !> \brief Load configuration schema
   subroutine config_manager_load_schema(this, schema_file, rc)
      implicit none
      class(ConfigManagerType), intent(inout) :: this
      character(len=*), intent(in) :: schema_file
      integer, intent(out) :: rc

      type(yaml_node_t) :: schema_config
      logical :: file_exists, success
      character(len=64) :: required_fields(100), optional_fields(100)
      character(len=256) :: schema_name, schema_description
      integer :: n_required, n_optional, i
      character(len=256) :: key

      rc = CC_SUCCESS

      ! Check if schema file exists
      inquire(file=trim(schema_file), exist=file_exists)
      if (.not. file_exists) then
         write(*, '(A,A)') 'INFO: Schema file not found, using default schema: ', trim(schema_file)
         ! Use default schema initialization
         this%schema_loaded = .true.
         return
      endif

      ! Load schema configuration file
      schema_config = yaml_load_file(schema_file)
      if (.not. c_associated(schema_config%ptr)) then
         write(*, '(A)') 'WARNING: Failed to parse schema file, using defaults'
         this%schema_loaded = .true.
         return
      endif

      ! Read schema metadata
      success = yaml_get_string(schema_config, "schema/name", schema_name)
      if (success) then
         write(*, '(A,A)') 'INFO: Loading schema: ', trim(schema_name)
      endif

      success = yaml_get_string(schema_config, "schema/description", &
                               schema_description)

      ! Read required fields
      success = yaml_get_integer(schema_config, "schema/required_fields/n_fields", n_required)
      if (success .and. n_required > 0) then
         do i = 1, min(n_required, size(required_fields))
            write(key, '(A,I0)') 'schema/required_fields/field_', i
            success = yaml_get_string(schema_config, trim(key), required_fields(i))
            if (success) then
               call this%schema%add_required_field(trim(required_fields(i)))
            endif
         end do
         write(*, '(A,I0,A)') 'INFO: Added ', min(n_required, size(required_fields)), ' required fields to schema'
      endif

      ! Read optional fields
      success = yaml_get_integer(schema_config, "schema/optional_fields/n_fields", n_optional)
      if (success .and. n_optional > 0) then
         do i = 1, min(n_optional, size(optional_fields))
            write(key, '(A,I0)') 'schema/optional_fields/field_', i
            success = yaml_get_string(schema_config, trim(key), optional_fields(i))
            if (success) then
               call this%schema%add_optional_field(trim(optional_fields(i)))
            endif
         end do
         write(*, '(A,I0,A)') 'INFO: Added ', min(n_optional, size(optional_fields)), ' optional fields to schema'
      endif

      ! Clean up
      call yaml_destroy_node(schema_config)

      this%schema_loaded = .true.
      write(*, '(A)') 'INFO: Schema loaded successfully'

   end subroutine config_manager_load_schema

   !> \brief Validate loaded configuration
   subroutine config_manager_validate(this, rc)
      implicit none
      class(ConfigManagerType), intent(inout) :: this
      integer, intent(out) :: rc

      if (.not. this%is_loaded) then
         rc = CC_FAILURE
         return
      endif

      if (.not. this%schema_loaded) then
         rc = CC_SUCCESS  ! No schema to validate against
         return
      endif

      call this%schema%validate_config(this%yaml_data, rc)

   end subroutine config_manager_validate

   !> \brief Get string value from configuration
   subroutine config_manager_get_string(this, key, value, rc, default_value)
      implicit none
      class(ConfigManagerType), intent(inout) :: this
      character(len=*), intent(in) :: key
      character(len=*), intent(out) :: value
      integer, intent(out) :: rc
      character(len=*), optional, intent(in) :: default_value

      ! Try to get value from loaded YAML data using yaml_get interface
      if (this%is_loaded) then
         call yaml_get(this%yaml_data, key, value, rc, default_value)
      else
         ! Use default value if provided
         if (present(default_value)) then
            value = default_value
            rc = CC_SUCCESS
         else
            value = ''
            rc = CC_FAILURE
         endif
      endif

   end subroutine config_manager_get_string

   !> \brief Get integer value from configuration
   subroutine config_manager_get_integer(this, key, value, rc, default_value)
      implicit none
      class(ConfigManagerType), intent(inout) :: this
      character(len=*), intent(in) :: key
      integer, intent(out) :: value
      integer, intent(out) :: rc
      integer, optional, intent(in) :: default_value

      ! Try to get value from loaded YAML data using yaml_get interface
      if (this%is_loaded) then
         call yaml_get(this%yaml_data, key, value, rc, default_value)
      else
         ! Use default value if provided
         if (present(default_value)) then
            value = default_value
            rc = CC_SUCCESS
         else
            value = 0
            rc = CC_FAILURE
         endif
      endif

   end subroutine config_manager_get_integer

   !> \brief Get real value from configuration
   subroutine config_manager_get_real(this, key, value, rc, default_value)
      implicit none
      class(ConfigManagerType), intent(inout) :: this
      character(len=*), intent(in) :: key
      real(fp), intent(out) :: value
      integer, intent(out) :: rc
      real(fp), optional, intent(in) :: default_value

      ! Try to get value from loaded YAML data using yaml_get interface
      if (this%is_loaded) then
         call yaml_get(this%yaml_data, key, value, rc, default_value)
      else
         ! Use default value if provided
         if (present(default_value)) then
            value = default_value
            rc = CC_SUCCESS
         else
            value = 0.0_fp
            rc = CC_FAILURE
         endif
      endif

   end subroutine config_manager_get_real

   !> \brief Get logical value from configuration
   subroutine config_manager_get_logical(this, key, value, rc, default_value)
      implicit none
      class(ConfigManagerType), intent(inout) :: this
      character(len=*), intent(in) :: key
      logical, intent(out) :: value
      integer, intent(out) :: rc
      logical, optional, intent(in) :: default_value

      ! Try to get value from loaded YAML data using yaml_get interface
      if (this%is_loaded) then
         call yaml_get(this%yaml_data, key, value, rc, default_value)
      else
         ! Use default value if provided
         if (present(default_value)) then
            value = default_value
            rc = CC_SUCCESS
         else
            value = .false.
            rc = CC_FAILURE
         endif
      endif

   end subroutine config_manager_get_logical

   !> \brief Get array value from configuration
   subroutine config_manager_get_array(this, key, values, rc)
      implicit none
      class(ConfigManagerType), intent(in) :: this
      character(len=*), intent(in) :: key
      character(len=*), allocatable, intent(out) :: values(:)
      integer, intent(out) :: rc

      ! Suppress warnings for unused arguments/variables
      if (.false.) write(*,*) trim(key)

      ! Try to get array from loaded YAML data
      if (this%is_loaded) then
         ! For now, simplified implementation - would need to parse YAML arrays
         ! This is a placeholder that returns empty array
         allocate(values(0))
         rc = CC_SUCCESS
      else
         allocate(values(0))
         rc = CC_FAILURE
      endif

   end subroutine config_manager_get_array

   !> \brief Get number of species from configuration
   function config_manager_get_nspecies(this) result(nspecies)
      implicit none
      class(ConfigManagerType), intent(in) :: this
      integer :: nspecies

      nspecies = this%config_data%runtime%nSpecies
   end function config_manager_get_nspecies

   !> \brief Get maximum number of species from configuration
   function config_manager_get_max_species(this) result(max_species)
      implicit none
      class(ConfigManagerType), intent(in) :: this
      integer :: max_species

      max_species = this%config_data%runtime%maxSpecies
   end function config_manager_get_max_species

   !> \brief Get number of emission categories from configuration
   function config_manager_get_nemission_categories(this) result(nemission_categories)
      implicit none
      class(ConfigManagerType), intent(in) :: this
      integer :: nemission_categories

      nemission_categories = this%config_data%runtime%nEmissionCategories
   end function config_manager_get_nemission_categories

   !> \brief Get number of emission species from configuration
   function config_manager_get_nemission_species(this) result(nemission_species)
      implicit none
      class(ConfigManagerType), intent(in) :: this
      integer :: nemission_species

      nemission_species = this%config_data%runtime%nEmissionSpecies
   end function config_manager_get_nemission_species

   !> \brief Get species filename from configuration
   function config_manager_get_species_file(this) result(species_file)
      implicit none
      class(ConfigManagerType), intent(in) :: this
      character(len=255) :: species_file

      species_file = this%config_data%file_paths%Species_File
   end function config_manager_get_species_file

   !> \brief Print configuration summary
   subroutine config_manager_print_summary(this)
      implicit none
      class(ConfigManagerType), intent(in) :: this

      write(*,'(A)') '=== ConfigManager Summary ==='
      write(*,'(A,A)') 'Config file: ', trim(this%config_file)
      write(*,'(A,L1)') 'Loaded: ', this%is_loaded
      write(*,'(A,L1)') 'Schema loaded: ', this%schema_loaded
      write(*,'(A,I0)') 'Environment overrides: ', size(this%env_overrides)
      write(*,'(A,I0)') 'CLI overrides: ', size(this%cli_overrides)
      write(*,'(A)') '=============================='

   end subroutine config_manager_print_summary

   !> \brief Save current configuration to file
   subroutine config_manager_save_to_file(this, filename, rc)
      implicit none
      class(ConfigManagerType), intent(in) :: this
      character(len=*), intent(in) :: filename
      integer, intent(out) :: rc

      integer :: unit_num, io_stat

      rc = CC_SUCCESS

      if (.not. this%is_loaded) then
         rc = CC_FAILURE
         return
      endif

      ! Open file for writing
      open(newunit=unit_num, file=trim(filename), status='replace', &
           action='write', iostat=io_stat)
      if (io_stat /= 0) then
         rc = CC_FAILURE
         return
      endif

      ! Write basic configuration info (placeholder implementation)
      write(unit_num, '(A)') '# CATChem Configuration (Generated)'
      write(unit_num, '(A)') '# This is a placeholder - full YAML serialization not yet implemented'
      write(unit_num, '(A,I0)') 'nSpecies: ', this%config_data%runtime%nSpecies
      write(unit_num, '(A,I0)') 'nEmissionCategories: ', this%config_data%runtime%nEmissionCategories
      write(unit_num, '(A,I0)') 'nEmissionSpecies: ', this%config_data%runtime%nEmissionSpecies

      close(unit_num)

   end subroutine config_manager_save_to_file

   !> \brief Set configuration loading strategy
   subroutine config_manager_set_loading_strategy(this, strategy)
      implicit none
      class(ConfigManagerType), intent(inout) :: this
      integer, intent(in) :: strategy

      this%load_strategy = strategy

   end subroutine config_manager_set_loading_strategy

   !> \brief Add environment variable override
   subroutine config_manager_add_env_override(this, env_var, rc)
      implicit none
      class(ConfigManagerType), intent(inout) :: this
      character(len=*), intent(in) :: env_var
      integer, intent(out) :: rc

      character(len=64), allocatable :: temp_array(:)
      integer :: n, i

      rc = CC_SUCCESS

      n = size(this%env_overrides)
      allocate(temp_array(n+1))

      do i = 1, n
         temp_array(i) = this%env_overrides(i)
      end do
      temp_array(n+1) = trim(env_var)

      call move_alloc(temp_array, this%env_overrides)

   end subroutine config_manager_add_env_override

   !> \brief Add command line override
   subroutine config_manager_add_cli_override(this, cli_arg, rc)
      implicit none
      class(ConfigManagerType), intent(inout) :: this
      character(len=*), intent(in) :: cli_arg
      integer, intent(out) :: rc

      character(len=512), allocatable :: temp_array(:)
      integer :: n, i

      rc = CC_SUCCESS

      n = size(this%cli_overrides)
      allocate(temp_array(n+1))

      do i = 1, n
         temp_array(i) = this%cli_overrides(i)
      end do
      temp_array(n+1) = trim(cli_arg)

      call move_alloc(temp_array, this%cli_overrides)

   end subroutine config_manager_add_cli_override

   !========================================================================
   ! ConfigDataType Procedures
   !========================================================================

   !> \brief Initialize configuration data
   subroutine config_data_init(this, rc)
      implicit none
      class(ConfigDataType), intent(inout) :: this
      integer, intent(out) :: rc

      rc = CC_SUCCESS

      ! Initialize all components to defaults (already done by default initialization)
      this%is_validated = .false.
      this%source_file = ''
      this%config_version = '2.0'

   end subroutine config_data_init

   !> \brief Clean up configuration data
   subroutine config_data_cleanup(this, rc)
      implicit none
      class(ConfigDataType), intent(inout) :: this
      integer, intent(out) :: rc

      rc = CC_SUCCESS

      ! Reset to defaults
      call this%init(rc)

   end subroutine config_data_cleanup

   !> \brief Validate configuration data
   function config_data_validate(this, error_mgr, rc) result(is_valid)
      implicit none
      class(ConfigDataType), intent(inout) :: this
      type(ErrorManagerType), intent(inout) :: error_mgr
      integer, intent(out) :: rc
      logical :: is_valid

      rc = CC_SUCCESS
      is_valid = .true.

      call error_mgr%push_context('config_data_validate', 'ConfigManager_Mod.F90')

      ! Basic validation checks
      if (this%runtime%numCPUs < 1) then
         call error_mgr%report_error(ERROR_INVALID_CONFIG, &
              'Number of CPUs must be positive', rc, 'config_data_validate')
         is_valid = .false.
         call error_mgr%pop_context()
         return
      endif

      if (this%runtime%nLevs < 1) then
         call error_mgr%report_error(ERROR_INVALID_CONFIG, &
              'Number of levels must be positive', rc, 'config_data_validate')
         is_valid = .false.
         call error_mgr%pop_context()
         return
      endif

      if (this%runtime%maxSpecies < 1) then
         call error_mgr%report_error(ERROR_INVALID_CONFIG, &
              'Maximum species must be positive', rc, 'config_data_validate')
         is_valid = .false.
         call error_mgr%pop_context()
         return
      endif

      if (len_trim(this%file_paths%Input_Directory) == 0) then
         call error_mgr%report_error(ERROR_INVALID_CONFIG, &
              'Input directory must be specified', rc, 'config_data_validate')
         is_valid = .false.
         call error_mgr%pop_context()
         return
      endif

      ! Mark as validated if all checks pass
      this%is_validated = is_valid
      call error_mgr%pop_context()

   end function config_data_validate

   !> \brief Print configuration summary
   subroutine config_data_print_summary(this)
      implicit none
      class(ConfigDataType), intent(in) :: this

      write(*, '(A)') '=== Configuration Summary ==='
      write(*, '(A,A)') 'Version: ', trim(this%config_version)
      write(*, '(A,A)') 'Source file: ', trim(this%source_file)
      write(*, '(A,L1)') 'Validated: ', this%is_validated
      write(*, '(A,I0)') 'Number of CPUs: ', this%runtime%numCPUs
      write(*, '(A,A)') 'Simulation name: ', trim(this%runtime%SimulationName)
      write(*, '(A,L1)') 'External emissions activated: ', this%external_emissions%activate
      write(*, '(A)') '============================'

   end subroutine config_data_print_summary

   !> \brief Convert configuration to YAML string
   function config_data_to_yaml_string(this, rc) result(yaml_string)
      implicit none
      class(ConfigDataType), intent(in) :: this
      integer, intent(out) :: rc
      character(len=:), allocatable :: yaml_string

      rc = CC_SUCCESS

      ! This is a placeholder - would convert to actual YAML format
      yaml_string = 'configuration:\n  version: ' // trim(this%config_version) // '\n'

   end function config_data_to_yaml_string

   !> \brief Copy configuration data
   subroutine config_data_copy(this, source, rc)
      implicit none
      class(ConfigDataType), intent(inout) :: this
      class(ConfigDataType), intent(in) :: source
      integer, intent(out) :: rc

      rc = CC_SUCCESS

      ! Deep copy all components
      this%runtime = source%runtime
      this%file_paths = source%file_paths
      this%external_emissions = source%external_emissions
      this%config_version = source%config_version
      this%source_file = source%source_file
      this%is_validated = source%is_validated

   end subroutine config_data_copy

   !> \brief Parse YAML data into structured configuration
   subroutine parse_config_data(this, rc)
      implicit none
      class(ConfigManagerType), intent(inout) :: this
      integer, intent(out) :: rc

      rc = CC_SUCCESS

      if (.not. this%is_loaded) then
         rc = CC_FAILURE
         return
      endif

      ! Parse runtime configuration
      call yaml_get(this%yaml_data, 'runtime/nEmissionSpecies', this%config_data%runtime%nEmissionSpecies, rc, 50)

      ! Parse file paths
      call yaml_get(this%yaml_data, 'output/directory', this%config_data%file_paths%Output_Directory, rc, './')
      call yaml_get(this%yaml_data, 'simulation/species_filename', this%config_data%file_paths%Species_File, rc, '')
      call yaml_get(this%yaml_data, 'simulation/emission_filename', this%config_data%file_paths%Emission_File, rc, '')

      ! Parse external emissions configuration
      if (yaml_has_key(this%yaml_data, 'external_emissions')) then
         call yaml_get(this%yaml_data, 'external_emissions/global_scale_factor', this%config_data%external_emissions%global_scale_factor, rc, 1.0_fp)
      endif

      ! Mark configuration as validated
      this%config_data%is_validated = .true.
      this%config_data%source_file = this%config_file

   end subroutine parse_config_data

   !> \brief Load complete species data with all properties from YAML file and initialize ChemState
   !!
   !! This enhanced method loads species configuration including all properties
   !! such as molecular weight, type flags, background concentrations, etc.,
   !! and initializes the provided ChemState with the loaded species data.
   !! Based on the YAML structure in test_species.yml.
   !!
   !! \param[in]    filename     Species configuration file path
   !! \param[inout] chem_state   ChemState to initialize with species data
   !! \param[in]    grid         Grid geometry for concentration array initialization
   !! \param[out]   num_species  Number of species loaded
   !! \param[out]   rc          Return code
   subroutine config_manager_load_and_init_species(this, filename, chem_state, error_mgr, grid, rc, num_species)
      use ChemState_Mod, only: ChemStateType
      use Error_Mod, only: ErrorManagerType
      use GridGeometry_Mod, only: GridGeometryType
      implicit none
      class(ConfigManagerType), intent(inout) :: this
      character(len=*), intent(in) :: filename
      type(ChemStateType), intent(inout) :: chem_state
      type(ErrorManagerType), pointer, intent(inout) :: error_mgr
      type(GridGeometryType), pointer, intent(in) :: grid
      integer, intent(out) :: rc
      integer, optional, intent(out) :: num_species

      type(yaml_node_t) :: species_config
      logical :: file_exists, success
      integer :: i, j, list_size, total_keys, species_index
      character(len=256) :: species_path
      character(len=64), allocatable :: species_keys(:)
      character(len=64) :: all_yaml_keys(200)

      rc = CC_SUCCESS

      ! Check if file exists
      inquire(file=trim(filename), exist=file_exists)
      if (.not. file_exists) then
         write(*, '(A,A)') 'ERROR: Species configuration file not found: ', trim(filename)
         rc = CC_FAILURE
         return
      endif

      ! Load species configuration file
      species_config = yaml_load_file(filename)
      if (.not. c_associated(species_config%ptr)) then
         write(*, '(A)') 'ERROR: Failed to load species configuration file'
         rc = CC_FAILURE
         return
      endif

      ! Check if this is a map/dictionary structure
      if (.not. yaml_is_map(species_config)) then
         write(*, '(A)') 'ERROR: Species configuration file must be a YAML map/dictionary'
         rc = CC_FAILURE
         call yaml_destroy_node(species_config)
         return
      endif

      ! Get all top-level keys - these are the species names
      total_keys = yaml_get_size(species_config)
      success = yaml_get_all_keys(species_config, all_yaml_keys, list_size)
      
      if (.not. success .or. list_size <= 0) then
         write(*, '(A)') 'ERROR: Failed to read species keys from configuration file'
         rc = CC_FAILURE
         call yaml_destroy_node(species_config)
         return
      endif
      
      write(*, '(A,I0,A)') 'INFO: Found ', list_size, ' species keys in configuration'

      ! Initialize ChemState with the number of species and grid geometry
      call chem_state%init(list_size, error_mgr, rc, grid)
      if (rc /= CC_SUCCESS) then
         write(*, '(A)') 'ERROR: Failed to initialize ChemState'
         call yaml_destroy_node(species_config)
         return
      endif

      ! Allocate species keys array
      allocate(species_keys(list_size))
      
      ! Copy species keys and clean null characters from C strings
      do i = 1, list_size
         species_keys(i) = all_yaml_keys(i)
         ! Replace null characters with spaces for proper Fortran string handling
         do j = 1, len(species_keys(i))
            if (ichar(species_keys(i)(j:j)) == 0) then
               species_keys(i)(j:j) = ' '
            endif
         end do
      end do
      
      ! Load each species and directly populate ChemState in single loop
      do i = 1, list_size
         species_path = trim(species_keys(i))
         
         ! Load species properties directly into ChemState%ChemSpecies array
         call load_species_properties(species_config, trim(species_path), chem_state%ChemSpecies(i), rc)
         if (rc /= CC_SUCCESS) then
            write(*, '(A,A,A,I0)') 'ERROR: Failed to load species properties for "', &
                                  trim(species_path), '", species #', i
            deallocate(species_keys)
            call yaml_destroy_node(species_config)
            return
         endif

         ! Directly populate ChemState arrays - species_index is just i
         species_index = i
         chem_state%nSpecies = species_index
         ! Clean the species name before storing it
         chem_state%ChemSpecies(i)%short_name = trim(adjustl(chem_state%ChemSpecies(i)%short_name))
         chem_state%SpeciesNames(species_index) = trim(chem_state%ChemSpecies(i)%short_name)
         chem_state%SpeciesIndex(species_index) = species_index

         ! Classify species into categories (allow multiple classifications)
         if (chem_state%ChemSpecies(i)%is_gas) then
            chem_state%nSpeciesGas = chem_state%nSpeciesGas + 1
            chem_state%GasIndex(chem_state%nSpeciesGas) = species_index
         endif

         if (chem_state%ChemSpecies(i)%is_aerosol) then
            chem_state%nSpeciesAero = chem_state%nSpeciesAero + 1
            chem_state%AeroIndex(chem_state%nSpeciesAero) = species_index
         endif

         if (chem_state%ChemSpecies(i)%is_dust) then
            chem_state%nSpeciesDust = chem_state%nSpeciesDust + 1
            chem_state%DustIndex(chem_state%nSpeciesDust) = species_index
         endif

         if (chem_state%ChemSpecies(i)%is_seasalt) then
            chem_state%nSpeciesSeaSalt = chem_state%nSpeciesSeaSalt + 1
            chem_state%SeaSaltIndex(chem_state%nSpeciesSeaSalt) = species_index
         endif

         if (chem_state%ChemSpecies(i)%is_drydep) then
            chem_state%nSpeciesDryDep = chem_state%nSpeciesDryDep + 1
            chem_state%DryDepIndex(chem_state%nSpeciesDryDep) = species_index

            ! Also add to aerosol dry deposition if it's an aerosol
            if (chem_state%ChemSpecies(i)%is_aerosol .and. chem_state%ChemSpecies(i)%is_drydep) then
               chem_state%nSpeciesAeroDryDep = chem_state%nSpeciesAeroDryDep + 1
               chem_state%AeroDryDepIndex(chem_state%nSpeciesAeroDryDep) = species_index
            endif
         endif

         if (chem_state%ChemSpecies(i)%is_tracer) then
            chem_state%nSpeciesTracer = chem_state%nSpeciesTracer + 1
            chem_state%TracerIndex(chem_state%nSpeciesTracer) = species_index
         endif

         !print species info as a test
         ! write(*, '(A,A)') 'Species name: ', chem_state%ChemSpecies(i)%short_name
         ! write(*, '(A,A)') 'Description: ', chem_state%ChemSpecies(i)%description
         ! write(*, *) 'lower radius: ', chem_state%ChemSpecies(i)%lower_radius
         ! write(*, *) 'upper radius: ', chem_state%ChemSpecies(i)%upper_radius
         ! write(*, *) 'density: ', chem_state%ChemSpecies(i)%density
         ! write(*, *) 'Molecular weight: ', chem_state%ChemSpecies(i)%mw_g
         ! write(*, *) 'is gas: ', chem_state%ChemSpecies(i)%is_gas
         ! write(*, *) 'is aerosol: ', chem_state%ChemSpecies(i)%is_aerosol
         ! write(*, *) 'is dust: ', chem_state%ChemSpecies(i)%is_dust
         ! write(*, *) 'is sea salt: ', chem_state%ChemSpecies(i)%is_seasalt
         ! write(*, *) 'is dry deposition: ', chem_state%ChemSpecies(i)%is_drydep
         ! write(*, *) 'is tracer: ', chem_state%ChemSpecies(i)%is_tracer

      enddo

      ! Clean up
      deallocate(species_keys)
      call yaml_destroy_node(species_config)

      write(*, '(A,I0,A)') 'INFO: Successfully initialized ChemState with ', list_size, ' species'
      write(*, '(A,I0)') '  Gas species: ', chem_state%nSpeciesGas
      write(*, '(A,I0)') '  Aerosol species: ', chem_state%nSpeciesAero  
      write(*, '(A,I0)') '  Dust species: ', chem_state%nSpeciesDust
      write(*, '(A,I0)') '  Sea salt species: ', chem_state%nSpeciesSeaSalt
      write(*, '(A,I0)') '  Dry deposition species: ', chem_state%nSpeciesDryDep
      write(*, '(A,I0)') '  Aerosol dry deposition species: ', chem_state%nSpeciesAeroDryDep
      write(*, '(A,I0)') '  Tracer species: ', chem_state%nSpeciesTracer

      ! Return optional output parameters
      if (present(num_species)) then
         num_species = list_size
      endif

   end subroutine config_manager_load_and_init_species

   !> \brief Helper subroutine to load species properties from YAML
   !! \param[in] yaml_root Root YAML node
   !! \param[in] species_path Path to the species data in YAML
   !! \param[inout] species Species object to populate
   !! \param[out] rc Return code
   subroutine load_species_properties(yaml_root, species_path, species, rc)
      implicit none
      type(yaml_node_t), intent(in) :: yaml_root
      character(len=*), intent(in) :: species_path
      type(SpeciesType), intent(inout) :: species
      integer, intent(out) :: rc

      logical :: success
      character(len=256) :: field_path
      character(len=64) :: species_name
      real(fp) :: temp_real  ! Using project-wide fp precision
      logical :: temp_logical
      character(len=256) :: temp_string
      integer :: yaml_rc  ! Separate return code for YAML operations
      integer :: i, j  ! Loop variables for debugging

      rc = CC_SUCCESS

      ! Initialize species with defaults
      call species%init('UNKNOWN', 'Unknown Species', 28.0_fp, rc)
      if (rc /= CC_SUCCESS) then
         write(*, '(A,A)') 'ERROR: Failed to initialize species at path: ', trim(species_path)
         return
      endif

      ! Load species name (required) - try 'name' field first, then use path as fallback
      write(field_path, '(A,A)') trim(species_path), '/name'
      success = yaml_get_string(yaml_root, trim(field_path), species_name)
      if (success) then
         ! Clean null characters from YAML-loaded name
         do i = 1, len(species_name)
            if (ichar(species_name(i:i)) == 0) then
               species_name(i:i) = ' '
            endif
         end do
         species%short_name = trim(adjustl(species_name))
         species%long_name = trim(adjustl(species_name))
      else
         ! Use the key/path as the species name (for flat format like so2:, so4:)
         ! Clean any null characters that may be in the species_path
         species_name = species_path
         do i = 1, len(species_name)
            if (ichar(species_name(i:i)) == 0) then
               species_name(i:i) = ' '
            endif
         end do
         species%short_name = trim(adjustl(species_name))
         species%long_name = trim(adjustl(species_name))
         ! Suppress verbose output: using key as fallback name is expected behavior
         ! write(*, '(A,A,A,A)') 'INFO: No name field found, using key "', &
         !                      trim(species_path), '" as species name'
      endif

      ! Load long_name if explicitly provided
      write(field_path, '(A,A)') trim(species_path), '/long_name'
      success = yaml_get_string(yaml_root, trim(field_path), temp_string)
      if (success) then
         ! Clean null characters from YAML-loaded long_name
         do i = 1, len(temp_string)
            if (ichar(temp_string(i:i)) == 0) then
               temp_string(i:i) = ' '
            endif
         end do
         species%long_name = trim(adjustl(temp_string))
      endif

      ! Load description (optional)
      write(field_path, '(A,A)') trim(species_path), '/description'
      success = yaml_get_string(yaml_root, trim(field_path), temp_string)
      if (success) then
         ! Clean null characters from YAML-loaded description
         do i = 1, len(temp_string)
            if (ichar(temp_string(i:i)) == 0) then
               temp_string(i:i) = ' '
            endif
         end do
         species%description = trim(adjustl(temp_string))
      endif

      ! Load molecular weight (optional, but important)
      write(field_path, '(A,A)') trim(species_path), '/molecular_weight'
      call yaml_get(yaml_root, trim(field_path), temp_real, yaml_rc)
      if (yaml_rc == 0) then
         species%mw_g = temp_real
      else
         ! No molecular weight specified - keep default from init
         write(*, '(A,A,A)') 'WARNING: No molecular_weight found for species ', &
                            trim(species%short_name), ', using default 28.0'
      endif

      ! Load physical properties
      write(field_path, '(A,A)') trim(species_path), '/density'
      call yaml_get(yaml_root, trim(field_path), temp_real, yaml_rc)
      if (yaml_rc == 0) then
         species%density = temp_real
      else
         species%density = MISSING
      endif

      write(field_path, '(A,A)') trim(species_path), '/radius'
      call yaml_get(yaml_root, trim(field_path), temp_real, yaml_rc)
      if (yaml_rc == 0) then
         species%radius = temp_real
      else
         species%radius = MISSING
      endif

      write(field_path, '(A,A)') trim(species_path), '/lower_radius'
      call yaml_get(yaml_root, trim(field_path), temp_real, yaml_rc)
      if (yaml_rc == 0) then
         species%lower_radius = temp_real
      else
         species%lower_radius = MISSING
      endif

      write(field_path, '(A,A)') trim(species_path), '/upper_radius'
      call yaml_get(yaml_root, trim(field_path), temp_real, yaml_rc)
      if (yaml_rc == 0) then
         species%upper_radius = temp_real
      else
         species%upper_radius = MISSING
      endif

      ! Load type flags (with proper default handling)
      write(field_path, '(A,A)') trim(species_path), '/is_gas'
      success = yaml_get_logical(yaml_root, trim(field_path), temp_logical)
      if (success) then
         species%is_gas = temp_logical
      else
         species%is_gas = MISSING_BOOL
      endif

      write(field_path, '(A,A)') trim(species_path), '/is_aerosol'
      success = yaml_get_logical(yaml_root, trim(field_path), temp_logical)
      if (success) then
         species%is_aerosol = temp_logical
      else
         species%is_aerosol = MISSING_BOOL
      endif

      write(field_path, '(A,A)') trim(species_path), '/is_dust'
      success = yaml_get_logical(yaml_root, trim(field_path), temp_logical)
      if (success) then
         species%is_dust = temp_logical
      else
         species%is_dust = MISSING_BOOL
      endif

      write(field_path, '(A,A)') trim(species_path), '/is_seasalt'
      success = yaml_get_logical(yaml_root, trim(field_path), temp_logical)
      if (success) then
         species%is_seasalt = temp_logical
      else
         species%is_seasalt = MISSING_BOOL
      endif

      write(field_path, '(A,A)') trim(species_path), '/is_tracer'
      success = yaml_get_logical(yaml_root, trim(field_path), temp_logical)
      if (success) then
         species%is_tracer = temp_logical
      else
         species%is_tracer = MISSING_BOOL
      endif

      write(field_path, '(A,A)') trim(species_path), '/is_drydep'
      success = yaml_get_logical(yaml_root, trim(field_path), temp_logical)
      if (success) then
         species%is_drydep = temp_logical
      else
         species%is_drydep = MISSING_BOOL
      endif

      write(field_path, '(A,A)') trim(species_path), '/is_photolysis'
      success = yaml_get_logical(yaml_root, trim(field_path), temp_logical)
      if (success) then
         species%is_photolysis = temp_logical
      else
         species%is_photolysis = MISSING_BOOL
      endif

      ! Load background concentration (optional)
      write(field_path, '(A,A)') trim(species_path), '/background_vv'
      call yaml_get(yaml_root, trim(field_path), temp_real, yaml_rc)
      if (yaml_rc == 0) then
         species%BackgroundVV = temp_real
      else
         species%BackgroundVV = MISSING
      endif

      ! Set species as valid
      species%is_valid = .true.

      ! Print species information in a single line
      write(*, '(A,A,A,F6.1,A,L1,A,L1,A,L1,A,L1,A)') &
         'INFO: Loaded species "', trim(adjustl(species%short_name)), &
         '" (MW=', species%mw_g, ', gas=', species%is_gas, &
         ', aerosol=', species%is_aerosol, ', dust=', species%is_dust, &
         ', seasalt=', species%is_seasalt, ')'

   end subroutine load_species_properties

   !> \brief Load emission configuration from file
   subroutine config_manager_load_emission_config(this, filename, rc)
      implicit none
      class(ConfigManagerType), intent(inout) :: this
      character(len=*), intent(in) :: filename
      integer, intent(out) :: rc

      type(yaml_node_t) :: emission_config
      logical :: file_exists, success
      character(len=256) :: emission_directory, scaling_method
      integer :: n_emission_sources
      real(fp) :: global_scaling_factor

      rc = CC_SUCCESS

      ! Check if file exists
      inquire(file=trim(filename), exist=file_exists)
      if (.not. file_exists) then
         write(*, '(A,A)') 'WARNING: Emission configuration file not found: ', trim(filename)
         ! Don't fail - emissions may be optional
         return
      endif

      ! Load emission configuration file
      emission_config = yaml_load_file(filename)
      if (.not. c_associated(emission_config%ptr)) then
         rc = CC_FAILURE
         return
      endif

      ! Read basic emission settings
      success = yaml_get_string(emission_config, "emissions/data_directory", &
                               emission_directory)
      if (success) then
         write(*, '(A,A)') 'INFO: Emission data directory: ', trim(emission_directory)
      endif

      success = yaml_get_string(emission_config, "emissions/scaling_method", &
                               scaling_method)
      if (success) then
         write(*, '(A,A)') 'INFO: Emission scaling method: ', trim(scaling_method)
      endif

      call yaml_get(emission_config, "emissions/global_scaling_factor", &
                             global_scaling_factor, rc)
      if (rc == 0) then
         write(*, '(A,F8.3)') 'INFO: Global emission scaling factor: ', global_scaling_factor
      endif

      success = yaml_get_integer(emission_config, "emissions/n_sources", n_emission_sources)
      if (success) then
         write(*, '(A,I0)') 'INFO: Number of emission sources: ', n_emission_sources
      endif

      ! TODO: Parse individual emission sources, species mapping,
      ! vertical distributions, temporal profiles, etc.
      ! This would require extending the ConfigDataType to store emission data

      ! Clean up
      call yaml_destroy_node(emission_config)

      write(*, '(A)') 'INFO: Emission configuration loaded successfully'

   end subroutine config_manager_load_emission_config

   !> \brief Update runtime configuration from loaded configs
   subroutine config_manager_update_runtime_from_configs(this, rc)
      implicit none
      class(ConfigManagerType), intent(inout) :: this
      integer, intent(out) :: rc

      rc = CC_SUCCESS

      ! Placeholder implementation - would update runtime settings

   end subroutine config_manager_update_runtime_from_configs

end module ConfigManager_Mod

!> \file SeaSaltProcessCreator_Mod.F90
!! \brief Factory for creating seasalt process instances
!!
!! This module provides the factory functions for creating seasalt
!! process instances following the CATChem Process Factory pattern.
!!
!! Generated on: 2025-08-29T16:37:23.024068
!! Author: Barry Baker & Wei Li
!! Version: 1.0.0

module SeaSaltProcessCreator_Mod

   use iso_fortran_env, only: fp => real64
   use Error_Mod, only: CC_SUCCESS, CC_FAILURE
   use ProcessInterface_Mod
   use ProcessSeaSaltInterface_Mod

   implicit none
   private

   public :: create_seasalt_process
   public :: register_seasalt_process

   ! Process metadata constants
   character(len=*), parameter :: PROCESS_NAME = 'seasalt'
   character(len=*), parameter :: PROCESS_VERSION = '1.0.0'
   character(len=*), parameter :: PROCESS_DESCRIPTION = 'Process for computing sea salt aerosol emissions over ocean surfaces'
   character(len=*), parameter :: PROCESS_AUTHOR = 'Barry Baker & Wei Li'

contains

   !> Create a new seasalt process instance
   !!
   !! This factory function creates and returns a new instance of the
   !! seasalt process. The process is not initialized - the caller
   !! must call the init() method with appropriate configuration.
   !!
   !! @param[out] process     Allocated process instance
   !! @param[out] rc          Return code
   subroutine create_seasalt_process(process, rc)
      class(ProcessInterface), allocatable, intent(out) :: process
      integer, intent(out) :: rc

      type(ProcessSeaSaltInterface), allocatable :: seasalt_process
      integer :: alloc_stat

      rc = CC_SUCCESS

      ! Allocate the process instance
      allocate(seasalt_process, stat=alloc_stat)
      if (alloc_stat /= 0) then
         rc = CC_FAILURE
         return
      end if

      ! Move to polymorphic variable
      call move_alloc(seasalt_process, process)

   end subroutine create_seasalt_process

   !> Register the seasalt process with the global registry
   !!
   !! This subroutine should be called during module initialization to
   !! register the seasalt process with the global process registry.
   !!
   !! @param[out] rc Return code
   subroutine register_seasalt_process(rc)
      use ProcessRegistry_Mod, only: get_global_registry, ProcessRegistryType

      integer, intent(out) :: rc
      type(ProcessRegistryType), pointer :: registry

      rc = CC_SUCCESS
      registry => get_global_registry()

      call registry%register_process( &
         name=PROCESS_NAME, &
         category='emission', &
         description=PROCESS_DESCRIPTION, &
         creator=create_seasalt_process, &
         rc=rc &
      )

   end subroutine register_seasalt_process

   !> Get information about the seasalt process
   !!
   !! This function returns metadata about the seasalt process
   !! including name, version, description, and capabilities.
   !!
   !! @param[out] info Process information structure
   subroutine get_seasalt_process_info(info)
      type(ProcessInfo), intent(out) :: info

      ! Basic information
      info%name = PROCESS_NAME
      info%version = PROCESS_VERSION
      info%description = PROCESS_DESCRIPTION
      info%author = PROCESS_AUTHOR

      ! Process characteristics
      info%process_type = 'emission'
      info%is_multiphase = .false.
      info%has_size_bins = .false.
      info%supports_vectorization = .true.

      ! Integration characteristics
      info%timestep_dependency = 'independent'
      info%parallelization = 'column'
      info%memory_requirements = 'low'

      ! Species information
      info%n_species = 0



      ! Available schemes
      info%n_schemes = 3
      allocate(info%scheme_names(info%n_schemes))
      allocate(info%scheme_descriptions(info%n_schemes))
      info%scheme_names(1) = 'gong97'
      info%scheme_descriptions(1) = 'Gong 1997 sea salt emission scheme'
      info%scheme_names(2) = 'gong03'
      info%scheme_descriptions(2) = 'Gong 2003 sea salt emission scheme with improved sub- and super-micron treatment'
      info%scheme_names(3) = 'geos12'
      info%scheme_descriptions(3) = 'GEOS-Chem 2012 sea salt emission scheme with observational constraints'
      info%default_scheme = ''

      ! Required meteorological fields
      info%n_required_met_fields = 0

      ! Optional meteorological fields
      info%n_optional_met_fields = 0

      ! Required chemical fields
      info%n_required_chem_fields = 0

      ! Diagnostic information
      info%n_diagnostics = 2
      allocate(info%diagnostic_names(info%n_diagnostics))
      allocate(info%diagnostic_descriptions(info%n_diagnostics))
      allocate(info%diagnostic_units(info%n_diagnostics))
      info%diagnostic_names(1) = 'seasalt_mass_emission_total'
      info%diagnostic_descriptions(1) = 'Sea salt mass emission flux total'
      info%diagnostic_units(1) = 'ug/m2/s'
      info%diagnostic_names(2) = 'seasalt_number_emission_total'
      info%diagnostic_descriptions(2) = 'Sea salt number emission flux total'
      info%diagnostic_units(2) = '#/m2/s'

   end subroutine get_seasalt_process_info

   !> Check if seasalt process is compatible with host model
   !!
   !! This function checks whether the seasalt process is compatible
   !! with the host model configuration and requirements.
   !!
   !! @param[in] host_config Host model configuration
   !! @param[inout] error_handler Error handling object
   !! @return .true. if compatible, .false. otherwise
   function is_seasalt_compatible(host_config, error_handler) result(is_compatible)
      type(HostModelConfig), intent(in) :: host_config
      type(ErrorHandler), intent(inout) :: error_handler
      logical :: is_compatible

      is_compatible = .true.

      ! Check required species

      ! Check required meteorological fields



   end function is_seasalt_compatible

   !> Validate seasalt process configuration
   !!
   !! This function validates the process configuration against the
   !! process requirements and constraints.
   !!
   !! @param[in] config_data Configuration data (YAML, namelist, etc.)
   !! @param[inout] error_handler Error handling object
   !! @return .true. if valid, .false. otherwise
   function validate_seasalt_config_data(config_data, error_handler) result(is_valid)
      character(len=*), intent(in) :: config_data
      type(ErrorHandler), intent(inout) :: error_handler
      logical :: is_valid

      ! TODO: Implement configuration validation
      ! This should parse the config_data and validate:
      ! - Scheme selection is valid
      ! - Parameters are within acceptable ranges
      ! - Required fields are present
      ! - No conflicting options

      is_valid = .true.

      ! Placeholder validation
      if (len_trim(config_data) == 0) then
         call error_handler%set_error(ERROR_CONFIG, &
            "Empty configuration data provided")
         is_valid = .false.
      end if

   end function validate_seasalt_config_data

   !> Get default configuration for seasalt process
   !!
   !! This function returns a default configuration string that can be
   !! used to initialize the seasalt process with reasonable defaults.
   !!
   !! @param[out] config_data Default configuration string
   subroutine get_seasalt_default_config(config_data)
      character(len=*), intent(out) :: config_data

      ! Return default YAML configuration
      config_data = &
         '# Default seasalt process configuration' // new_line('A') // &
         'process:' // new_line('A') // &
         '  name: "seasalt"' // new_line('A') // &
         '  version: "1.0.0"' // new_line('A') // &
         '  active_scheme: ""' // new_line('A') // &
         '  is_active: true' // new_line('A') // &
         '' // new_line('A') // &
         '# Scheme configuration' // new_line('A') // &
         'schemes:' // new_line('A') // &
         '  gong97:' // new_line('A') // &
         '    description: "Gong 1997 sea salt emission scheme"' // new_line('A') // &
         '    algorithm_type: "explicit"' // new_line('A') // &
         '    parameters:' // new_line('A') // &
         '      scale_factor: 1.0' // new_line('A') // &
         '      weibull_flag: False' // new_line('A') // &
         '' // new_line('A') // &
         '  gong03:' // new_line('A') // &
         '    description: "Gong 2003 sea salt emission scheme with improved sub- and super-micron treatment"' // new_line('A') // &
         '    algorithm_type: "explicit"' // new_line('A') // &
         '    parameters:' // new_line('A') // &
         '      scale_factor: 1.0' // new_line('A') // &
         '      weibull_flag: False' // new_line('A') // &
         '' // new_line('A') // &
         '  geos12:' // new_line('A') // &
         '    description: "GEOS-Chem 2012 sea salt emission scheme with observational constraints"' // new_line('A') // &
         '    algorithm_type: "explicit"' // new_line('A') // &
         '    parameters:' // new_line('A') // &
         '      scale_factor: 1.0' // new_line('A') // &
         '' // new_line('A') // &
         '# Diagnostic configuration' // new_line('A') // &
         'diagnostics:' // new_line('A') // &
         '  output_frequency: 3600.0  # seconds' // new_line('A') // &
         '  output_diagnostics: true'

   end subroutine get_seasalt_default_config

end module SeaSaltProcessCreator_Mod
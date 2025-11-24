!> \file DustCommon_Mod.F90
!! \brief Common types and utilities for dust process
!!
!! This module defines the configuration types used by the
!! dust process and its schemes.
!!
!! Generated on: 2025-09-09T14:29:24.630006
!! Author: Barry Baker
!! Version: 1.0.0

module DustCommon_Mod

   use precision_mod, only: fp
   ! use precision_mod, only: fp
   use error_mod, only: CC_SUCCESS, CC_FAILURE, CC_Error, CC_Warning, ErrorManagerType, &
      ERROR_INVALID_CONFIG, ERROR_INVALID_STATE, ERROR_NOT_FOUND
   use ConfigManager_Mod, only: ConfigManagerType  ! ConfigManager integration
   use StateManager_Mod, only: StateManagerType  ! Add StateManager integration

   implicit none
   private

   ! Export types
   public :: DustProcessConfig  ! New unified process config
   public :: DustConfig
   public :: DustSchemeFENGSHAConfig
   public :: DustSchemeGINOUXConfig

   ! Export utility functions
   public :: int_to_string

   !> Main configuration type for dust process
   type :: DustConfig

      ! Process settings
      character(len=32) :: scheme = 'fengsha'
      logical :: is_active = .true.
      logical :: diagnostics = .false.  ! Diagnostic switch
      real(fp) :: dt_min = 1.0_fp     ! Minimum time step (seconds)
      real(fp) :: dt_max = 3600.0_fp  ! Maximum time step (seconds)

      ! Species configuration
      integer :: n_species = 0
      character(len=32), allocatable :: species_names(:)
      integer, allocatable :: species_indices(:)  ! Indices of dust species in ChemState




      ! Diagnostic configuration
      logical :: output_diagnostics = .true.
      real(fp) :: diagnostic_frequency = 3600.0_fp  ! Output frequency (seconds)

   contains
      procedure, public :: validate => validate_dust_config
      procedure, public :: finalize => finalize_dust_config
      procedure, public :: print_summary => print_dust_config_summary
   end type DustConfig

   !> Configuration type for fengsha scheme
   type :: DustSchemeFENGSHAConfig

      ! Scheme metadata
      character(len=64) :: scheme_name = 'fengsha'
      character(len=256) :: description = 'Fengsha Dust emission scheme developed at NOAA ARL for use at NOAA NWS'
      character(len=64) :: author = 'Barry Baker'
      character(len=16) :: algorithm_type = 'explicit'

      ! Process configuration
      logical :: affects_full_column = .false.  ! Surface-only processing

      ! Scheme parameters
      real(fp) :: alpha = 0.16  ! linear scaling factor
      real(fp) :: beta = 1.0  ! Exponential scaling factor on source parameter
      real(fp) :: drylimit_factor = 1.0  ! Dry Limit factor modifying the Fecan dry limit following Zender 2003
      real(fp) :: drag_option = 1  ! Drag Partition Option: 1 - use input drag, 2 - Darmenova, 3 - Leung 2022, 4 - MB95
      real(fp) :: moist_option = 1 - fecan  ! Moisture parameterization: 1 - Fecan, 2 - shao, 3 - modified shao
      real(fp) :: distribution_option = 1  ! Dust Distribution option: 1 - Kok 2011, 2 - Meng 2022

      ! Required meteorological fields
      integer :: n_required_met_fields = 12
      character(len=32) :: required_met_fields(12)

   contains
      procedure, public :: validate => validate_fengsha_config
      procedure, public :: finalize => finalize_fengsha_config
   end type DustSchemeFENGSHAConfig

   ! fengsha scheme uses local variables only - no persistent state type needed

   !> Configuration type for ginoux scheme
   type :: DustSchemeGINOUXConfig

      ! Scheme metadata
      character(len=64) :: scheme_name = 'ginoux'
      character(len=256) :: description = 'Ginoux dust emission scheme'
      character(len=64) :: author = 'Barry Baker'
      character(len=16) :: algorithm_type = 'explicit'

      ! Process configuration
      logical :: affects_full_column = .false.  ! Surface-only processing

      ! Scheme parameters
      real(fp) :: Ch_DU = [0.1, 0.1, 0.1, 0.1, 0.1]  ! Dust tuning coefficient per species

      ! Required meteorological fields
      integer :: n_required_met_fields = 5
      character(len=32) :: required_met_fields(5)

   contains
      procedure, public :: validate => validate_ginoux_config
      procedure, public :: finalize => finalize_ginoux_config
   end type DustSchemeGINOUXConfig

   ! ginoux scheme uses local variables only - no persistent state type needed


   !> Unified process configuration type that bridges ConfigManager and process-specific configs
   !! This is the main configuration type that ProcessInterface should use
   type :: DustProcessConfig

      ! Process metadata
      character(len=64) :: process_name = 'dust'
      character(len=16) :: process_version = '1.0.0'
      logical :: is_active = .true.

      ! Process-specific configuration (delegate to DustConfig)
      type(DustConfig) :: dust_config

      ! Scheme configurations
      type(DustSchemeFENGSHAConfig) :: fengsha_config
      type(DustSchemeGINOUXConfig) :: ginoux_config

   contains
      procedure, public :: load_from_config => dust_process_load_config
      procedure, public :: load_species_from_chem_state => load_species_from_chem_state
      procedure, public :: validate => dust_process_validate
      procedure, public :: finalize => dust_process_finalize
      procedure, public :: get_active_scheme_config => get_active_scheme_config
      procedure, private :: load_fengsha_config
      procedure, private :: load_ginoux_config
   end type DustProcessConfig

contains

   !> Validate dust configuration
   subroutine validate_dust_config(this, error_handler)
      class(DustConfig), intent(inout) :: this
      type(ErrorManagerType), intent(inout) :: error_handler

      character(len=256) :: error_msg
      integer :: rc

      ! Validate time step bounds
      if (this%dt_min <= 0.0_fp) then
         call error_handler%report_error(ERROR_INVALID_CONFIG, &
            "Minimum time step must be positive", rc)
         return
      end if

      if (this%dt_max < this%dt_min) then
         call error_handler%report_error(ERROR_INVALID_CONFIG, &
            "Maximum time step must be >= minimum time step", rc)
         return
      end if

      ! Validate active scheme
      if (trim(this%scheme) /= 'fengsha' .and. &
         trim(this%scheme) /= 'ginoux' .and. &
         .true.) then
         write(error_msg, '(A)') "Invalid scheme: " // trim(this%scheme)
         call error_handler%report_error(ERROR_INVALID_CONFIG, error_msg, rc)
         return
      end if

   end subroutine validate_dust_config

   !> Print configuration summary
   subroutine print_dust_config_summary(this)
      class(DustConfig), intent(in) :: this

      write(*, '(A)') "=== Dust Process Configuration ==="
      write(*, '(A,A)') "  Active scheme: ", trim(this%scheme)
      write(*, '(A,I0)') "  Number of species: ", this%n_species
      write(*, '(A,F0.1,A)') "  Minimum time step: ", this%dt_min, " s"
      write(*, '(A,F0.1,A)') "  Maximum time step: ", this%dt_max, " s"
      write(*, '(A,L1)') "  Output diagnostics: ", this%output_diagnostics
      write(*, '(A)') "============================================="

   end subroutine print_dust_config_summary

   !> Finalize dust configuration
   subroutine finalize_dust_config(this)
      class(DustConfig), intent(inout) :: this

      ! Deallocate species names array
      if (allocated(this%species_names)) then
         deallocate(this%species_names)
      end if

      ! Deallocate species indices array
      if (allocated(this%species_indices)) then
         deallocate(this%species_indices)
      end if


   end subroutine finalize_dust_config

   !> Validate fengsha scheme configuration
   subroutine validate_fengsha_config(this, error_handler)
      class(DustSchemeFENGSHAConfig), intent(inout) :: this
      type(ErrorManagerType), intent(inout) :: error_handler

      ! TODO: Add scheme-specific validation

   end subroutine validate_fengsha_config

   !> Finalize fengsha scheme configuration
   subroutine finalize_fengsha_config(this)
      class(DustSchemeFENGSHAConfig), intent(inout) :: this

      ! Nothing to deallocate for basic configuration

   end subroutine finalize_fengsha_config


   !> Validate ginoux scheme configuration
   subroutine validate_ginoux_config(this, error_handler)
      class(DustSchemeGINOUXConfig), intent(inout) :: this
      type(ErrorManagerType), intent(inout) :: error_handler

      ! TODO: Add scheme-specific validation

   end subroutine validate_ginoux_config

   !> Finalize ginoux scheme configuration
   subroutine finalize_ginoux_config(this)
      class(DustSchemeGINOUXConfig), intent(inout) :: this

      ! Nothing to deallocate for basic configuration

   end subroutine finalize_ginoux_config



   !> Convert integer to string (utility function)
   function int_to_string(int_val) result(str_val)
      integer, intent(in) :: int_val
      character(len=32) :: str_val

      write(str_val, '(I0)') int_val
      str_val = adjustl(str_val)

   end function int_to_string

   !> Load unified process configuration from ConfigManager
   !! This is the main function that ProcessInterface.parse_process_config should call
   !! Process reads its configuration directly from the master YAML via ConfigManager
   subroutine dust_process_load_config(this, config_manager, error_handler)
      class(DustProcessConfig), intent(inout) :: this
      type(ConfigManagerType), intent(inout) :: config_manager
      type(ErrorManagerType), intent(inout) :: error_handler

      character(len=256) :: scheme_name
      integer :: ierr, rc

      ! Process reads directly from master YAML structure: processes.dust
      ! ConfigManager provides generic YAML access, process handles its own configuration

      ! Load process metadata
      call config_manager%get_string("processes/dust/name", this%process_name, rc, "dust")
      if (rc /= CC_SUCCESS) this%process_name = "dust"  ! default

      call config_manager%get_string("processes/dust/version", this%process_version, rc, "1.0.0")
      if (rc /= CC_SUCCESS) this%process_version = "1.0.0"  ! default

      call config_manager%get_logical("processes/dust/activate", this%is_active, rc, .true.)
      if (rc /= CC_SUCCESS) this%is_active = .true.  ! default

      ! Load process-specific configuration directly from master YAML
      call config_manager%get_string("processes/dust/scheme", this%dust_config%scheme, rc, "fengsha")
      if (rc /= CC_SUCCESS) then
         call error_handler%report_error(ERROR_INVALID_CONFIG, &
            "Missing required 'scheme' in processes/dust configuration", rc)
         return
      end if

      ! Load diagnostic switch
      call config_manager%get_logical("processes/dust/diagnostics", this%dust_config%diagnostics, rc, .false.)
      if (rc /= CC_SUCCESS) this%dust_config%diagnostics = .false.  ! Default

      ! Species configuration is loaded from ChemState in load_species_from_chem_state
      ! The species come from the master species YAML file (CATChem_species.yml)
      ! and are filtered by is_dust property


      ! Load scheme-specific configuration from master YAML
      scheme_name = trim(this%dust_config%scheme)
      select case (scheme_name)
       case ('fengsha')
         call this%load_fengsha_config(config_manager, error_handler)
       case ('ginoux')
         call this%load_ginoux_config(config_manager, error_handler)
       case default
         call error_handler%report_error(ERROR_INVALID_STATE, &
            "Unknown dust scheme: " // trim(scheme_name), rc)
         return
      end select

   end subroutine dust_process_load_config


   !> Load species from ChemState
   !! This function is used for dynamic species discovery (by_metadata or all_species)
   !! For 'all_species' mode: loads all species using nSpecies and SpeciesIndex
   !! For 'by_metadata' mode: loads species by type using nSpeciesDust and DustIndex
   subroutine load_species_from_chem_state(this, chem_state, error_handler)
      use ChemState_Mod, only: ChemStateType

      class(DustProcessConfig), intent(inout) :: this
      type(ChemStateType), pointer, intent(in) :: chem_state
      type(ErrorManagerType), intent(inout) :: error_handler

      integer :: i, rc

      if (.not. associated(chem_state)) then
         call error_handler%report_error(ERROR_INVALID_STATE, &
            "ChemState not associated in load_species_from_chem_state", rc)
         return
      end if

      ! by_metadata mode: Load species by type from ChemState using dynamic metadata flag mapping
      ! No metadata flags specified, use class-based indexing
      this%dust_config%n_species = chem_state%nSpeciesDust

      if (this%dust_config%n_species <= 0) then
         call error_handler%report_error(ERROR_INVALID_STATE, &
            "No dust species found in ChemState", rc)
         return
      end if

      ! Check if DustIndex is allocated and has correct size
      if (.not. allocated(chem_state%DustIndex)) then
         call error_handler%report_error(ERROR_INVALID_STATE, &
            "DustIndex not allocated in ChemState", rc)
         return
      end if

      if (size(chem_state%DustIndex) < this%dust_config%n_species) then
         call error_handler%report_error(ERROR_INVALID_STATE, &
            "DustIndex size inconsistent with nSpeciesDust", rc)
         return
      end if

      ! Deallocate existing arrays if allocated
      if (allocated(this%dust_config%species_names)) then
         deallocate(this%dust_config%species_names)
      end if
      if (allocated(this%dust_config%species_indices)) then
         deallocate(this%dust_config%species_indices)
      end if

      ! Allocate arrays
      allocate(this%dust_config%species_names(this%dust_config%n_species))
      allocate(this%dust_config%species_indices(this%dust_config%n_species))

      ! by_metadata mode: Copy indices from metadata-specific index array using dynamic mapping
      ! No metadata flags specified, use class-based indexing
      this%dust_config%species_indices(1:this%dust_config%n_species) = &
         chem_state%DustIndex(1:this%dust_config%n_species)

      ! Get species names using the indices
      do i = 1, this%dust_config%n_species
         if (this%dust_config%species_indices(i) > 0 .and. &
            this%dust_config%species_indices(i) <= size(chem_state%SpeciesNames)) then
            this%dust_config%species_names(i) = &
               trim(chem_state%SpeciesNames(this%dust_config%species_indices(i)))
         else
            call error_handler%report_error(ERROR_INVALID_STATE, &
               "Invalid species index in species index array", rc)
            return
         end if
      end do


   end subroutine load_species_from_chem_state

   !> Load fengsha scheme configuration from master YAML
   subroutine load_fengsha_config(this, config_manager, error_handler)
      class(DustProcessConfig), intent(inout) :: this
      type(ConfigManagerType), intent(inout) :: config_manager
      type(ErrorManagerType), intent(inout) :: error_handler

      integer :: ierr, rc

      ! Load scheme parameters directly from processes/dust/fengsha/ in master YAML
      call config_manager%get_real("processes/dust/fengsha/alpha", &
         this%fengsha_config%alpha, rc, 0.16_fp)
      if (rc /= CC_SUCCESS) this%fengsha_config%alpha = 0.16_fp
      call config_manager%get_real("processes/dust/fengsha/beta", &
         this%fengsha_config%beta, rc, 1.0_fp)
      if (rc /= CC_SUCCESS) this%fengsha_config%beta = 1.0_fp
      call config_manager%get_real("processes/dust/fengsha/drylimit_factor", &
         this%fengsha_config%drylimit_factor, rc, 1.0_fp)
      if (rc /= CC_SUCCESS) this%fengsha_config%drylimit_factor = 1.0_fp
      call config_manager%get_real("processes/dust/fengsha/drag_option", &
         this%fengsha_config%drag_option, rc, 1_fp)
      if (rc /= CC_SUCCESS) this%fengsha_config%drag_option = 1_fp
      call config_manager%get_real("processes/dust/fengsha/moist_option", &
         this%fengsha_config%moist_option, rc, 1 - fecan_fp)
      if (rc /= CC_SUCCESS) this%fengsha_config%moist_option = 1 - fecan_fp
      call config_manager%get_real("processes/dust/fengsha/distribution_option", &
         this%fengsha_config%distribution_option, rc, 1_fp)
      if (rc /= CC_SUCCESS) this%fengsha_config%distribution_option = 1_fp


   end subroutine load_fengsha_config

   !> Load ginoux scheme configuration from master YAML
   subroutine load_ginoux_config(this, config_manager, error_handler)
      class(DustProcessConfig), intent(inout) :: this
      type(ConfigManagerType), intent(inout) :: config_manager
      type(ErrorManagerType), intent(inout) :: error_handler

      integer :: ierr, rc

      ! Load scheme parameters directly from processes/dust/ginoux/ in master YAML
      call config_manager%get_real("processes/dust/ginoux/Ch_DU", &
         this%ginoux_config%Ch_DU, rc, [0.1, 0.1, 0.1, 0.1, 0.1]_fp)
      if (rc /= CC_SUCCESS) this%ginoux_config%Ch_DU = [0.1, 0.1, 0.1, 0.1, 0.1]_fp


   end subroutine load_ginoux_config


   !> Validate unified process configuration
   subroutine dust_process_validate(this, state_manager, error_handler)
      class(DustProcessConfig), intent(inout) :: this
      type(StateManagerType), intent(in) :: state_manager
      type(ErrorManagerType), intent(inout) :: error_handler

      ! Validate main config
      call this%dust_config%validate(error_handler)

      ! Validate scheme-specific config
      select case (trim(this%dust_config%scheme))
       case ('fengsha')
         call this%fengsha_config%validate(error_handler)
       case ('ginoux')
         call this%ginoux_config%validate(error_handler)
      end select

   end subroutine dust_process_validate

   !> Finalize unified process configuration
   subroutine dust_process_finalize(this)
      class(DustProcessConfig), intent(inout) :: this

      call this%dust_config%finalize()
      call this%fengsha_config%finalize()
      call this%ginoux_config%finalize()

   end subroutine dust_process_finalize

   !> Get active scheme configuration (polymorphic return)
   function get_active_scheme_config(this) result(scheme_config)
      class(DustProcessConfig), intent(in) :: this
      class(*), allocatable :: scheme_config

      select case (trim(this%dust_config%scheme))
       case ('fengsha')
         allocate(scheme_config, source=this%fengsha_config)
       case ('ginoux')
         allocate(scheme_config, source=this%ginoux_config)
       case default
         ! Return null
      end select

   end function get_active_scheme_config

end module DustCommon_Mod

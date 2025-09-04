!> \file CATChemCore_Mod.F90
!! \brief Central CATChem core framework with unified component management
!! \ingroup core_modules
!!
!! \author CATChem Development Team
!! \date 2025
!! \version 2.0
!!
!! This module provides the central CATChem core that owns and manages all
!! major components (StateManager, GridManager, DiagnosticManager, etc.)
!! and provides controlled access between them. This eliminates circular
!! dependencies and provides a clean, centralized architecture.
!!
!! \details
!! Key Features:
!! - Centralized component lifecycle management
!! - Controlled inter-component communication
!! - Simplified initialization and error handling
!! - Better testability and modularity
!!
!! \section core_usage Usage Example
!! \code{.f90}
!! use CATChemCore_Mod
!! type(CATChemCoreType) :: core
!! integer :: rc
!!
!! ! Initialize the core with configuration
!! call core%init('config.yaml', rc)
!!
!! ! Run simulation timesteps
!! do timestep = 1, n_timesteps
!!    call core%run_timestep(timestep, dt, rc)
!! enddo
!!
!! ! Clean shutdown
!! call core%finalize(rc)
!! \endcode
!!
module CATChemCore_Mod
   use precision_mod, only: fp
   use Error_Mod, only: CC_SUCCESS, CC_FAILURE, ErrorManagerType, ERROR_PROCESS_INITIALIZATION
   use ConfigManager_Mod, only: ConfigManagerType, ConfigDataType
   use StateManager_Mod, only: StateManagerType
   use GridManager_Mod, only: GridManagerType, GridGeometryType
   use DiagnosticManager_Mod, only: DiagnosticManagerType
   use ProcessManager_Mod, only: ProcessManagerType
   use MetState_Mod, only: MetStateType
   use ChemState_Mod, only: ChemStateType

   implicit none
   private

   public :: CATChemCoreType
   public :: CATChemBuilderType

   !> \brief Central CATChem core that owns all major components
   !!
   !! This type serves as the single entry point and owner of all major
   !! CATChem components. It manages their lifecycles and provides controlled
   !! access between components to avoid circular dependencies.
   type :: CATChemCoreType
      private

      ! Core components (owned by the core)
      type(ErrorManagerType)      :: error_mgr      !< Central error manager
      type(ConfigManagerType)     :: config_mgr     !< Configuration manager
      type(ConfigDataType)        :: config         !< Configuration data
      type(StateManagerType)      :: state_mgr      !< State manager
      type(GridManagerType)       :: grid_mgr       !< Grid manager
      type(DiagnosticManagerType) :: diag_mgr       !< Diagnostic manager
      type(ProcessManagerType)    :: process_mgr    !< Process manager

      ! Core state
      logical :: is_initialized = .false.           !< Initialization status
      logical :: is_configured = .false.            !< Configuration status
      character(len=512) :: config_file = ''        !< Configuration file path
      character(len=256) :: name = 'CATChem_Core'   !< Core instance name

      ! Grid configuration
      integer :: nx = 64, ny = 64, nz = 72          !< Default grid dimensions

   contains
      ! Core lifecycle
      procedure :: init => core_init
      procedure :: configure => core_configure
      procedure :: finalize => core_finalize
      procedure :: validate => core_validate

      ! Component access methods (controlled)
      procedure :: get_state_manager => core_get_state_manager
      procedure :: get_grid_manager => core_get_grid_manager
      procedure :: get_diagnostic_manager => core_get_diagnostic_manager
      procedure :: get_process_manager => core_get_process_manager
      procedure :: get_config => core_get_config
      procedure :: get_error_manager => core_get_error_manager

      ! High-level operations
      procedure :: run_timestep => core_run_timestep
      procedure :: setup_grid => core_setup_grid
      procedure :: setup_state => core_setup_state
      procedure :: setup_diagnostics => core_setup_diagnostics
      procedure :: setup_processes => core_setup_processes

      ! Process management convenience methods
      procedure :: add_process => core_add_process
      procedure :: run_processes => core_run_processes

      ! Utilities
      procedure :: print_info => core_print_info
      procedure :: is_ready => core_is_ready
      procedure :: set_name => core_set_name
      procedure :: get_memory_usage => core_get_memory_usage

   end type CATChemCoreType

   !> \brief Builder pattern for constructing CATChem core
   !!
   !! Provides a flexible way to configure and build the CATChem core
   !! with various options and settings.
   type :: CATChemBuilderType
      private

      character(len=512) :: config_file = ''
      character(len=256) :: name = 'CATChem_Core'
      integer :: nx = 64, ny = 64, nz = 72
      logical :: verbose = .false.
      logical :: validate_config = .true.

   contains
      procedure :: init => builder_init
      procedure :: with_config => builder_with_config
      procedure :: with_name => builder_with_name
      procedure :: with_grid => builder_with_grid
      procedure :: with_verbose => builder_with_verbose
      procedure :: skip_validation => builder_skip_validation
      procedure :: build => builder_build

   end type CATChemBuilderType

contains

   !========================================================================
   ! CATChemCoreType Implementation
   !========================================================================

   !> \brief Initialize the CATChem core
   !!
   !! Sets up the core framework and initializes the error manager.
   !! Components are initialized later during configuration.
   subroutine core_init(this, name, rc)
      class(CATChemCoreType), intent(inout) :: this
      character(len=*), intent(in), optional :: name
      integer, intent(out) :: rc

      rc = CC_SUCCESS

      ! Set core name
      if (present(name)) then
         this%name = trim(name)
      endif

      ! Initialize error manager first (it's needed by everything else)
      call this%error_mgr%init(verbose=.true.)
      call this%error_mgr%push_context('core_init', 'Initializing CATChem core')

      this%is_initialized = .true.
 
      ! Initialize configuration data to ensure it's in a valid state
      call this%config%init(rc)
      if (rc /= CC_SUCCESS) then
         call this%error_mgr%report_error(rc, 'Failed to initialize config data', rc)
         call this%error_mgr%pop_context()
         return
      endif

      call this%error_mgr%pop_context()

   end subroutine core_init

   !> \brief Configure the CATChem core with a configuration file
   !!
   !! Loads configuration and initializes all components in the correct order.
   subroutine core_configure(this, config_file, nx, ny, nz, rc)
      class(CATChemCoreType), intent(inout) :: this
      character(len=*), intent(in), optional :: config_file
      integer, intent(in), optional :: nx, ny, nz
      integer, intent(out) :: rc

      integer :: local_rc
      logical :: is_valid

      rc = CC_SUCCESS

      if (.not. this%is_initialized) then
         rc = CC_FAILURE
         return
      endif

      call this%error_mgr%push_context('core_configure', 'Configuring CATChem core')

      ! Set grid dimensions
      if (present(nx)) this%nx = nx
      if (present(ny)) this%ny = ny
      if (present(nz)) this%nz = nz

      ! Initialize configuration manager
      call this%config_mgr%init(local_rc)
      if (local_rc /= CC_SUCCESS) then
         call this%error_mgr%report_error(local_rc, 'Failed to initialize config manager', rc)
         call this%error_mgr%pop_context()
         return
      endif

      ! Load configuration file if provided
      if (present(config_file)) then
         this%config_file = trim(config_file)
         call this%config_mgr%load_from_file(this%config_file, local_rc)
         if (local_rc /= CC_SUCCESS) then
            call this%error_mgr%report_error(local_rc, 'Failed to load config file', rc)
            call this%error_mgr%pop_context()
            return
         endif
      endif

      ! Validate configuration
      is_valid = this%config%validate(this%error_mgr, local_rc)
      if (local_rc /= CC_SUCCESS .or. .not. is_valid) then
         call this%error_mgr%report_error(local_rc, 'Configuration validation failed', rc)
         call this%error_mgr%pop_context()
         return
      endif

      ! Initialize components in dependency order
      write(*,*) 'Entering core_configure'
      
      write(*,*) 'Calling setup_grid'
      call this%setup_grid(local_rc)
      write(*,*) 'Exited setup_grid'
      if (local_rc /= CC_SUCCESS) then
         rc = local_rc
         call this%error_mgr%pop_context()
         return
      endif

      write(*,*) 'Calling setup_state'
      call this%setup_state(local_rc)
      write(*,*) 'Exited setup_state'
      if (local_rc /= CC_SUCCESS) then
         rc = local_rc
         call this%error_mgr%pop_context()
         return
      endif

      write(*,*) 'Calling setup_diagnostics'
      call this%setup_diagnostics(local_rc)
      write(*,*) 'Exited setup_diagnostics'
      if (local_rc /= CC_SUCCESS) then
         rc = local_rc
         call this%error_mgr%pop_context()
         return
      endif

      write(*,*) 'Calling setup_processes'
      call this%setup_processes(local_rc)
      write(*,*) 'Exited setup_processes'
      if (local_rc /= CC_SUCCESS) then
         rc = local_rc
         call this%error_mgr%pop_context()
         return
      endif
      
      write(*,*) 'Exiting core_configure'

      this%is_configured = .true.

      call this%error_mgr%pop_context()

   end subroutine core_configure

   !> \brief Set up grid manager
   subroutine core_setup_grid(this, rc)
      class(CATChemCoreType), intent(inout) :: this
      integer, intent(out) :: rc

      write(*,*) 'Entering core_setup_grid'

      rc = CC_SUCCESS

      call this%error_mgr%push_context('core_setup_grid', 'Setting up grid manager')

      ! Initialize grid manager
      call this%grid_mgr%init(this%nx, this%ny, this%nz, this%error_mgr, rc=rc)
      if (rc /= CC_SUCCESS) then
         call this%error_mgr%report_error(rc, 'Failed to initialize grid manager', rc)
         call this%error_mgr%pop_context()
         write(*,*) 'Exiting core_setup_grid'
         return
      endif

      call this%error_mgr%pop_context()
      
      write(*,*) 'Exiting core_setup_grid'

   end subroutine core_setup_grid

   !> \brief Set up state manager
   subroutine core_setup_state(this, rc)
      class(CATChemCoreType), intent(inout) :: this
      integer, intent(out) :: rc

      integer :: local_rc
      type(MetStateType), pointer :: met_ptr
      type(ChemStateType), pointer :: chem_ptr
      type(ErrorManagerType), pointer :: error_mgr_ref

      write(*,*) 'Entering core_setup_state'

      rc = CC_SUCCESS

      call this%error_mgr%push_context('core_setup_state', 'Setting up state manager')

      ! Initialize state manager
      call this%state_mgr%init(this%name // '_StateManager', local_rc)
      if (local_rc /= CC_SUCCESS) then
         call this%error_mgr%report_error(local_rc, 'Failed to initialize state manager', rc)
         call this%error_mgr%pop_context()
         write(*,*) 'Exiting core_setup_state'
         return
      endif

      ! Initialize meteorological state
      met_ptr => this%state_mgr%get_met_state_ptr()
      if (associated(met_ptr)) then
          error_mgr_ref = this%state_mgr%get_error_manager()
          call met_ptr%init(this%nx, this%ny, this%nz, error_mgr_ref, local_rc)
          if (local_rc /= CC_SUCCESS) then
             call this%error_mgr%report_error(local_rc, 'Failed to initialize met state', rc)
             call this%error_mgr%pop_context()
             write(*,*) 'Exiting core_setup_state'
             return
          endif
      else
          ! Log the fact that met_ptr is null, but don't fail the whole process
          ! This allows the core to continue functioning even if met state is not available
          write(*,*) 'WARNING: MetState pointer is null, skipping met state initialization'
          ! Continue with the rest of the setup
      endif

      ! Initialize chemistry state
      chem_ptr => this%state_mgr%get_chem_state_ptr()
      if (associated(chem_ptr)) then
         error_mgr_ref = this%state_mgr%get_error_manager()
         ! Initialize with a reasonable default number of species
         ! In production, this would come from configuration
         call chem_ptr%init(50, error_mgr_ref, local_rc)
         if (local_rc /= CC_SUCCESS) then
            call this%error_mgr%report_error(local_rc, 'Failed to initialize chem state', rc)
            call this%error_mgr%pop_context()
            write(*,*) 'Exiting core_setup_state'
            return
         endif
      endif

      ! Mark state manager as configured
      call this%state_mgr%set_configured()

      call this%error_mgr%pop_context()
      
      write(*,*) 'Exiting core_setup_state'

   end subroutine core_setup_state

   !> \brief Set up diagnostic manager
   subroutine core_setup_diagnostics(this, rc)
      class(CATChemCoreType), intent(inout) :: this
      integer, intent(out) :: rc

      write(*,*) 'Entering core_setup_diagnostics'

      rc = CC_SUCCESS

      call this%error_mgr%push_context('core_setup_diagnostics', 'Setting up diagnostic manager')

      ! Initialize diagnostic manager with error manager reference
      call this%diag_mgr%init(this%error_mgr, rc)
      if (rc /= CC_SUCCESS) then
         call this%error_mgr%report_error(rc, 'Failed to initialize diagnostic manager', rc)
         call this%error_mgr%pop_context()
         write(*,*) 'Exiting core_setup_diagnostics'
         return
      endif

      ! Connect diagnostic manager to state manager for process access
      call this%state_mgr%set_diagnostic_manager(this%diag_mgr)

      call this%error_mgr%pop_context()
      
      write(*,*) 'Exiting core_setup_diagnostics'

   end subroutine core_setup_diagnostics

   !> \brief Setup process manager
   subroutine core_setup_processes(this, rc)
      class(CATChemCoreType), intent(inout) :: this
      integer, intent(out) :: rc

      integer :: local_rc

      write(*,*) 'Entering core_setup_processes'

      rc = CC_SUCCESS

      call this%error_mgr%push_context('core_setup_processes', 'Setting up process manager')

      ! Initialize process manager
      call this%process_mgr%init(local_rc)
      if (local_rc /= CC_SUCCESS) then
         call this%error_mgr%report_error(local_rc, 'Failed to initialize process manager', rc)
         call this%error_mgr%pop_context()
         write(*,*) 'Exiting core_setup_processes'
         return
      endif

      call this%error_mgr%pop_context()
      
      write(*,*) 'Exiting core_setup_processes'

   end subroutine core_setup_processes

   !> \brief Add a process to the process manager
   subroutine core_add_process(this, process_name, scheme_name, rc)
      class(CATChemCoreType), intent(inout) :: this
      character(len=*), intent(in) :: process_name
      character(len=*), intent(in) :: scheme_name
      integer, intent(out) :: rc

      call this%error_mgr%push_context("core_add_process")
      rc = CC_SUCCESS

      call this%process_mgr%add_process(process_name, scheme_name, this%state_mgr, rc)
      if (rc /= CC_SUCCESS) then
         call this%error_mgr%report_error(rc, "Failed to add process: " // trim(process_name), rc)
         call this%error_mgr%pop_context()
         return
      endif

      call this%error_mgr%pop_context()

   end subroutine core_add_process

   !> \brief Run all processes
   subroutine core_run_processes(this, rc)
      class(CATChemCoreType), intent(inout) :: this
      integer, intent(out) :: rc

      call this%error_mgr%push_context("core_run_processes")
      rc = CC_SUCCESS

      call this%process_mgr%run_all(this%state_mgr, rc)
      if (rc /= CC_SUCCESS) then
         call this%error_mgr%report_error(rc, "Failed to run processes", rc)
         call this%error_mgr%pop_context()
         return
      endif

      call this%error_mgr%pop_context()

   end subroutine core_run_processes

   !> \brief Run a single timestep
   subroutine core_run_timestep(this, timestep, dt, rc)
      class(CATChemCoreType), intent(inout) :: this
      integer, intent(in) :: timestep
      real(fp), intent(in) :: dt
      integer, intent(out) :: rc

      rc = CC_SUCCESS

      if (.not. this%is_ready()) then
         rc = CC_FAILURE
         return
      endif

      call this%error_mgr%push_context('core_run_timestep', 'Running timestep')

      ! Basic timestep orchestration
      ! 1. Run all processes
      call this%run_processes(rc)
      if (rc /= CC_SUCCESS) then
         call this%error_mgr%report_error(rc, 'Process execution failed', rc)
         call this%error_mgr%pop_context()
         return
      endif

      ! 2. Collect diagnostics
      call this%diag_mgr%collect_all_diagnostics(rc)
      if (rc /= CC_SUCCESS) then
         ! Don't fail the timestep for diagnostic issues, just warn
         call this%error_mgr%report_error(ERROR_PROCESS_INITIALIZATION, &
                                         'Diagnostic collection failed', rc)
         rc = CC_SUCCESS
      endif

      ! 3. Update state timestep counter (if needed)
      ! This would be where state evolution time tracking occurs

      call this%error_mgr%pop_context()

   end subroutine core_run_timestep

   !> \brief Finalize and clean up the core
   subroutine core_finalize(this, rc)
      class(CATChemCoreType), intent(inout) :: this
      integer, intent(out) :: rc

      integer :: local_rc

      rc = CC_SUCCESS

      call this%error_mgr%push_context('core_finalize', 'Finalizing CATChem core')

      ! Clean up components in reverse order
      if (this%is_configured) then
         ! Clean up diagnostic manager
         call this%diag_mgr%finalize(local_rc)
         if (local_rc /= CC_SUCCESS) then
            call this%error_mgr%report_error(ERROR_PROCESS_INITIALIZATION, &
                                            'Failed to finalize diagnostic manager', local_rc)
         endif

         ! Clean up state manager
         call this%state_mgr%finalize(local_rc)

         ! Clean up grid manager
         call this%grid_mgr%cleanup()

         ! Clean up configuration
         ! Note: ConfigManager uses automatic finalization for YAML data
      endif

      this%is_configured = .false.
      this%is_initialized = .false.

      call this%error_mgr%pop_context()

   end subroutine core_finalize

   !> \brief Validate core state
   function core_validate(this) result(is_valid)
      class(CATChemCoreType), intent(in) :: this
      logical :: is_valid

      is_valid = this%is_initialized .and. &
                 this%is_configured .and. &
                 this%state_mgr%is_ready() .and. &
                 this%grid_mgr%is_ready()

   end function core_validate

   !> \brief Get state manager (controlled access)
   function core_get_state_manager(this) result(state_mgr_ptr)
      class(CATChemCoreType), intent(inout), target :: this
      type(StateManagerType), pointer :: state_mgr_ptr

      state_mgr_ptr => this%state_mgr

   end function core_get_state_manager

   !> \brief Get grid manager (controlled access)
   function core_get_grid_manager(this) result(grid_mgr_ptr)
      class(CATChemCoreType), intent(inout), target :: this
      type(GridManagerType), pointer :: grid_mgr_ptr

      grid_mgr_ptr => this%grid_mgr

   end function core_get_grid_manager

   !> \brief Get diagnostic manager (controlled access)
   function core_get_diagnostic_manager(this) result(diag_mgr_ptr)
      class(CATChemCoreType), intent(inout), target :: this
      type(DiagnosticManagerType), pointer :: diag_mgr_ptr

      diag_mgr_ptr => this%diag_mgr

   end function core_get_diagnostic_manager

   !> \brief Get process manager (controlled access)
   function core_get_process_manager(this) result(process_mgr_ptr)
      class(CATChemCoreType), intent(inout), target :: this
      type(ProcessManagerType), pointer :: process_mgr_ptr

      process_mgr_ptr => this%process_mgr

   end function core_get_process_manager

   !> \brief Get configuration (controlled access)
   function core_get_config(this) result(config_ptr)
      class(CATChemCoreType), intent(inout), target :: this
      type(ConfigDataType), pointer :: config_ptr

      config_ptr => this%config

   end function core_get_config

   !> \brief Get error manager (controlled access)
   function core_get_error_manager(this) result(error_mgr_ptr)
      class(CATChemCoreType), intent(inout), target :: this
      type(ErrorManagerType), pointer :: error_mgr_ptr

      error_mgr_ptr => this%error_mgr

   end function core_get_error_manager

   !> \brief Check if core is ready for operations
   function core_is_ready(this) result(is_ready)
      class(CATChemCoreType), intent(in) :: this
      logical :: is_ready

      is_ready = this%validate()

   end function core_is_ready

   !> \brief Set core name
   subroutine core_set_name(this, name)
      class(CATChemCoreType), intent(inout) :: this
      character(len=*), intent(in) :: name

      this%name = trim(name)

   end subroutine core_set_name

   !> \brief Get approximate memory usage
   function core_get_memory_usage(this) result(memory_bytes)
      class(CATChemCoreType), intent(in) :: this
      integer(kind=8) :: memory_bytes

      memory_bytes = 0

      if (this%is_configured) then
         memory_bytes = memory_bytes + this%state_mgr%get_memory_usage()
         ! Add other component memory usage when available
      endif

   end function core_get_memory_usage

   !> \brief Print core information
   subroutine core_print_info(this)
      class(CATChemCoreType), intent(in) :: this

      write(*,'(A)') '================================================'
      write(*,'(A)') 'CATChem Core Information'
      write(*,'(A)') '================================================'
      write(*,'(A,A)') 'Name: ', trim(this%name)
      write(*,'(A,L1)') 'Initialized: ', this%is_initialized
      write(*,'(A,L1)') 'Configured: ', this%is_configured
      write(*,'(A,L1)') 'Ready: ', this%is_ready()
      write(*,'(A,A)') 'Config file: ', trim(this%config_file)
      write(*,'(A,3I6)') 'Grid dimensions (nx,ny,nz): ', this%nx, this%ny, this%nz
      write(*,'(A,I0)') 'Memory usage (bytes): ', this%get_memory_usage()
      write(*,'(A)') '================================================'

      if (this%is_configured) then
         write(*,'(A)') 'Component Status:'
         call this%state_mgr%print_info()
         call this%grid_mgr%print_info()
      endif

   end subroutine core_print_info

   !========================================================================
   ! CATChemBuilderType Implementation
   !========================================================================

   !> \brief Initialize builder
   subroutine builder_init(this)
      class(CATChemBuilderType), intent(inout) :: this

      ! Reset to defaults
      this%config_file = ''
      this%name = 'CATChem_Core'
      this%nx = 64
      this%ny = 64
      this%nz = 72
      this%verbose = .false.
      this%validate_config = .true.

   end subroutine builder_init

   !> \brief Set configuration file
   function builder_with_config(this, config_file) result(builder_ref)
      class(CATChemBuilderType), intent(inout) :: this
      character(len=*), intent(in) :: config_file
      type(CATChemBuilderType) :: builder_ref

      this%config_file = trim(config_file)
      builder_ref = this

   end function builder_with_config

   !> \brief Set core name
   function builder_with_name(this, name) result(builder_ref)
      class(CATChemBuilderType), intent(inout) :: this
      character(len=*), intent(in) :: name
      type(CATChemBuilderType) :: builder_ref

      this%name = trim(name)
      builder_ref = this

   end function builder_with_name

   !> \brief Set grid dimensions
   function builder_with_grid(this, nx, ny, nz) result(builder_ref)
      class(CATChemBuilderType), intent(inout) :: this
      integer, intent(in) :: nx, ny, nz
      type(CATChemBuilderType) :: builder_ref

      this%nx = nx
      this%ny = ny
      this%nz = nz
      builder_ref = this

   end function builder_with_grid

   !> \brief Enable verbose output
   function builder_with_verbose(this) result(builder_ref)
      class(CATChemBuilderType), intent(inout) :: this
      type(CATChemBuilderType) :: builder_ref

      this%verbose = .true.
      builder_ref = this

   end function builder_with_verbose

   !> \brief Skip configuration validation
   function builder_skip_validation(this) result(builder_ref)
      class(CATChemBuilderType), intent(inout) :: this
      type(CATChemBuilderType) :: builder_ref

      this%validate_config = .false.
      builder_ref = this

   end function builder_skip_validation

   !> \brief Build the CATChem core
   subroutine builder_build(this, core, rc)
      class(CATChemBuilderType), intent(in) :: this
      type(CATChemCoreType), intent(out) :: core
      integer, intent(out) :: rc

      integer :: local_rc

      rc = CC_SUCCESS

      if (this%verbose) then
         write(*,'(A)') 'Building CATChem core...'
         write(*,'(A,A)') '  Name: ', trim(this%name)
         write(*,'(A,3I6)') '  Grid: ', this%nx, this%ny, this%nz
         if (len_trim(this%config_file) > 0) then
            write(*,'(A,A)') '  Config: ', trim(this%config_file)
         endif
      endif

      ! Initialize core
      call core%init(this%name, local_rc)
      if (local_rc /= CC_SUCCESS) then
         rc = local_rc
         return
      endif

      ! Configure core
      if (len_trim(this%config_file) > 0) then
         call core%configure(this%config_file, this%nx, this%ny, this%nz, local_rc)
      else
         call core%configure(nx=this%nx, ny=this%ny, nz=this%nz, rc=local_rc)
      endif

      if (local_rc /= CC_SUCCESS) then
         rc = local_rc
         return
      endif

      if (this%verbose) then
         write(*,'(A)') 'CATChem core built successfully!'
         call core%print_info()
      endif

   end subroutine builder_build

end module CATChemCore_Mod

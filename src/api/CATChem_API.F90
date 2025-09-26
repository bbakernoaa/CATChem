!> \file CATChem_API.F90
!! \brief Streamlined CATChem API for host model integration
!! \ingroup catchem_api
!!
!! \author CATChem Development Team
!! \date 2025
!! \version 2.1
!!
!! This module provides a streamlined, lightweight API for integrating CATChem
!! into different modeling architectures. It leverages the existing core 
!! architecture without duplicating functionality, providing clean interfaces
!! for the most common integration patterns.
!!
!! Key design principles:
!! - Lightweight wrapper around existing core components
!! - Support for multiple processes and run phases  
!! - Streamlined data exchange with host models
!! - Clear error handling and status reporting
!! - No duplication of existing types (ConfigManager, StateManager, etc.)
!!
!! Usage pattern:
!! 1. Initialize with configuration file
!! 2. Setup grid geometry
!! 3. Add processes as needed
!! 4. Configure run phases (optional)
!! 5. Execute timesteps or phases
!! 6. Exchange data with host model
!! 7. Retrieve diagnostics
!! 8. Finalize
!!
module CATChem_API
   use Precision_Mod, only: fp
   use Error_Mod, only: CC_SUCCESS, CC_FAILURE
   use CATChemCore_Mod, only: CATChemCoreType, CATChemBuilderType
   use StateManager_Mod, only: StateManagerType
   use ProcessManager_Mod, only: ProcessManagerType
   use GridManager_Mod, only: GridManagerType
   use DiagnosticManager_Mod, only: DiagnosticManagerType
   use MetState_Mod, only: MetStateType
   use ChemState_Mod, only: ChemStateType
   use ConfigManager_Mod, only: ConfigDataType
   use DiagnosticInterface_Mod, only: DiagnosticRegistryType, DiagnosticFieldType, &
                                     DIAG_REAL_SCALAR, DIAG_REAL_1D, DIAG_REAL_2D, DIAG_REAL_3D, &
                                     DIAG_INTEGER_SCALAR, DIAG_INTEGER_1D, DIAG_INTEGER_2D, DIAG_INTEGER_3D
   use ProcessInterface_Mod, only: ProcessInterface
   ! Import process registration functions
   use SeaSaltProcessCreator_Mod, only: register_seasalt_process
   
   implicit none
   private
   
   ! Public interface types and constants
   public :: CATChem_Model
   public :: CATChem_SUCCESS, CATChem_FAILURE
   
   ! Return codes
   integer, parameter :: CATChem_SUCCESS = 0
   integer, parameter :: CATChem_FAILURE = -1
   
   !> Main CATChem API interface type
   !! This type provides a simplified interface to the CATChem core functionality
   !! while maintaining access to all necessary components for host model integration.
   type :: CATChem_Model
      private
      ! Core engine - uses existing CATChemCore infrastructure
      type(CATChemCoreType) :: core
      
      ! Configuration and status tracking
      logical :: initialized = .false.
      logical :: grid_setup = .false.
      logical :: enable_run_phase = .false.
      character(len=512) :: config_file = ''
      character(len=512) :: last_error_msg = ''
      
      ! Grid information
      integer :: nx = 0, ny = 0, nz = 0
      integer :: nsoil = 4, nsoiltype = 19, nsurftype = 13
      
   contains
      ! Basic lifecycle methods
      procedure :: initialize => model_initialize
      procedure :: finalize => model_finalize
      
      ! Grid access (no separate setup needed now)
      procedure :: get_grid_dimensions => model_get_grid_dimensions
      
      ! Process management
      procedure :: add_process => model_add_process
      procedure :: get_process_names => model_get_process_names
      procedure :: get_num_processes => model_get_num_processes
      procedure, private :: model_register_process
      
      ! Run execution
      procedure :: run_timestep => model_run_timestep
      procedure :: run_phase => model_run_phase
      procedure :: run_all_phases => model_run_all_phases
      procedure :: get_phase_names => model_get_phase_names
      
      ! Data exchange methods
      procedure :: set_meteorology => model_set_meteorology
      procedure :: get_meteorology => model_get_meteorology
      procedure :: set_chemistry => model_set_chemistry
      procedure :: get_chemistry => model_get_chemistry
      procedure :: set_emissions => model_set_emissions
      
      ! Diagnostic methods
      procedure :: get_diagnostic_names => model_get_diagnostic_names
      procedure :: get_diagnostic => model_get_diagnostic
      procedure :: get_all_diagnostics => model_get_all_diagnostics
      
      ! Utility methods
      procedure :: is_ready => model_is_ready
      procedure :: is_initialized => model_is_initialized
      procedure :: get_error_message => model_get_error_message
      procedure :: reset_error => model_reset_error
      
      ! Core access methods (for advanced users)
      procedure :: get_state_manager => model_get_state_manager
      procedure :: get_process_manager => model_get_process_manager
      procedure :: get_grid_manager => model_get_grid_manager
      procedure :: get_diagnostic_manager => model_get_diagnostic_manager
   end type CATChem_Model

contains

   !> Initialize the CATChem model with configuration file and grid dimensions
   !! This method sets up the core CATChem infrastructure using the builder pattern,
   !! loads configuration from the specified file, and sets up the grid geometry.
   subroutine model_initialize(this, config_file, nx, ny, nz, nsoil, nsoiltype, nsurftype, rc)
      class(CATChem_Model), intent(inout) :: this
      character(len=*), intent(in) :: config_file
      integer, intent(in) :: nx, ny, nz
      integer, intent(in), optional :: nsoil, nsoiltype, nsurftype
      integer, intent(out) :: rc
      
      type(CATChemBuilderType) :: builder
      type(ConfigDataType), pointer :: config_data => null()
      
      rc = CATChem_SUCCESS
      call this%reset_error()
      
      ! Validate inputs
      if (len_trim(config_file) == 0) then
         this%last_error_msg = 'Configuration file path is empty'
         rc = CATChem_FAILURE
         return
      endif
      
      if (nx <= 0 .or. ny <= 0 .or. nz <= 0) then
         this%last_error_msg = 'Grid dimensions must be positive'
         rc = CATChem_FAILURE
         return
      endif
      
      ! Store config file path and grid dimensions
      this%config_file = trim(config_file)
      this%nx = nx
      this%ny = ny
      this%nz = nz
      
      ! Store soil and surface parameters (use provided values or defaults)
      if (present(nsoil)) this%nsoil = nsoil
      if (present(nsoiltype)) this%nsoiltype = nsoiltype
      if (present(nsurftype)) this%nsurftype = nsurftype
      
      ! Initialize core using builder pattern with grid information
      call builder%init()
      builder = builder%with_name('CATChem_API_Instance')
      builder = builder%with_config(config_file)
      builder = builder%with_grid(nx, ny, nz, this%nsoil, this%nsoiltype, this%nsurftype)
      call builder%build(this%core, rc)
      
      if (rc /= CC_SUCCESS) then
         this%last_error_msg = 'Failed to initialize CATChem core with config: ' // trim(config_file)
         rc = CATChem_FAILURE
         return
      endif
      
      this%initialized = .true.
      this%grid_setup = .true.  ! Grid is now set up during initialization

      ! Get configuration data from core
      config_data => this%core%get_config()
      if ( .not. associated(config_data)) then
         this%last_error_msg = 'Required managers or config data not available'
         rc = CATChem_FAILURE
         return
      endif
      this%enable_run_phase = config_data%run_phases_enabled

   end subroutine model_initialize

   !> Finalize the CATChem model and clean up resources
   subroutine model_finalize(this, rc)
      class(CATChem_Model), intent(inout) :: this
      integer, intent(out) :: rc
      
      rc = CATChem_SUCCESS
      call this%reset_error()
      
      if (.not. this%initialized) then
         rc = CATChem_SUCCESS  ! Already finalized
         return
      endif
      
      ! Finalize core
      call this%core%finalize(rc)
      if (rc /= CC_SUCCESS) then
         this%last_error_msg = 'Warning: Core finalization had issues'
         ! Don't return failure for finalization warnings
         rc = CATChem_SUCCESS
      endif
      
      ! Reset state
      this%initialized = .false.
      this%grid_setup = .false.
      this%enable_run_phase = .false.
      this%nx = 0
      this%ny = 0
      this%nz = 0
      this%config_file = ''
   end subroutine model_finalize

   !> Get current grid dimensions
   subroutine model_get_grid_dimensions(this, nx, ny, nz, nsoil, nsoiltype, nsurftype)
      class(CATChem_Model), intent(in) :: this
      integer, intent(out) :: nx, ny, nz
      integer, intent(out), optional :: nsoil, nsoiltype, nsurftype
      
      nx = this%nx
      ny = this%ny
      nz = this%nz
      if (present(nsoil)) nsoil = this%nsoil
      if (present(nsoiltype)) nsoiltype = this%nsoiltype
      if (present(nsurftype)) nsurftype = this%nsurftype
   end subroutine model_get_grid_dimensions

   !> Add all enabled processes from configuration
   !! This method reads the ConfigManager data and adds all processes where enabled = true
   !! It automatically registers and adds each enabled process to the core.
   subroutine model_add_process(this, rc)
      use ConfigManager_Mod, only: ConfigDataType
      class(CATChem_Model), intent(inout) :: this
      integer, intent(out) :: rc
      
      type(ConfigDataType), pointer :: config_data => null()
      type(ProcessManagerType), pointer :: process_mgr => null()
      integer :: i, reg_rc, add_rc
      
      rc = CATChem_SUCCESS
      call this%reset_error()
      
      if (.not. this%initialized) then
         this%last_error_msg = 'Model must be initialized first'
         rc = CATChem_FAILURE
         return
      endif
      
      ! Get configuration data from core
      config_data => this%core%get_config()
      if (.not. associated(config_data)) then
         this%last_error_msg = 'Configuration data not available'
         rc = CATChem_FAILURE
         return
      endif
      
      ! Get process manager
      process_mgr => this%core%get_process_manager()
      if (.not. associated(process_mgr)) then
         this%last_error_msg = 'ProcessManager not available from core'
         rc = CATChem_FAILURE
         return
      endif
      
      ! Check if processes are available (either run phase or direct processes)
      if (.not. allocated(config_data%run_phase_processes)) then
         this%last_error_msg = 'No processes configured'
         rc = CATChem_FAILURE
         return
      endif
      
      ! Loop through all processes in configuration and add enabled ones
      do i = 1, size(config_data%run_phase_processes)
         if (config_data%run_phase_processes(i)%enabled) then
            
            ! Automatically register the process based on its name
            call this%model_register_process(config_data%run_phase_processes(i)%name, process_mgr, reg_rc)
            if (reg_rc /= CC_SUCCESS) then
               write(*,'(A,A,A)') 'Warning: Failed to register enabled process: ', &
                  trim(config_data%run_phase_processes(i)%name), '. Skipping this process.'
               cycle  ! Skip this process and continue with others
            endif
            
            ! Add process to core
            call this%core%add_process(config_data%run_phase_processes(i)%name, add_rc)
            if (add_rc /= CC_SUCCESS) then
               write(*,'(A,A,A)') 'Warning: Failed to add enabled process to core: ', &
                  trim(config_data%run_phase_processes(i)%name), '. Skipping this process.'
               cycle  ! Skip this process and continue with others
            endif
            
            write(*,'(A,A)') 'Successfully added enabled process: ', &
               trim(config_data%run_phase_processes(i)%name)
         else
            write(*,'(A,A)') 'Skipping disabled process: ', &
               trim(config_data%run_phase_processes(i)%name)
         endif
      end do
   end subroutine model_add_process

   !> Automatically register a process based on its name
   !! This is a private helper method that calls the appropriate registration function
   !! based on the process name.
   subroutine model_register_process(this, process_name, process_mgr, rc)
      class(CATChem_Model), intent(inout) :: this
      character(len=*), intent(in) :: process_name
      type(ProcessManagerType), intent(inout) :: process_mgr
      integer, intent(out) :: rc
      
      rc = CC_SUCCESS
      
      ! Dispatch to the appropriate registration function based on process name
      select case (trim(process_name))
      case ('seasalt')
         call register_seasalt_process(process_mgr, rc)
         if (rc /= CC_SUCCESS) then
            this%last_error_msg = 'Failed to register seasalt process'
         endif
      
      ! Add more processes here as they become available
      ! case ('dust')
      !    call register_dust_process(process_mgr, rc)
      ! case ('chemistry')  
      !    call register_chemistry_process(process_mgr, rc)
      
      case default
         this%last_error_msg = 'Unknown process type: ' // trim(process_name) // &
                               '. Supported processes: seasalt'
         rc = CC_FAILURE
      end select
      
   end subroutine model_register_process

   !> Get names of all configured processes
   subroutine model_get_process_names(this, process_names, rc)
      class(CATChem_Model), intent(inout) :: this
      character(len=*), allocatable, intent(out) :: process_names(:)
      integer, intent(out) :: rc
      
      type(ProcessManagerType), pointer :: process_mgr => null()
      character(len=64) :: temp_names(50)  ! Temporary array with max size
      integer :: count, i
      
      rc = CATChem_SUCCESS
      
      if (.not. this%initialized) then
         allocate(process_names(0))
         rc = CATChem_FAILURE
         return
      endif
      
      ! Get process manager from core
      process_mgr => this%core%get_process_manager()
      if (.not. associated(process_mgr)) then
         allocate(process_names(0))
         rc = CATChem_FAILURE
         return
      endif
      
      ! Get process list from ProcessManager
      call process_mgr%list_processes(temp_names, count)
      
      ! Allocate output array with actual count
      allocate(process_names(count))
      
      ! Copy the actual process names
      do i = 1, count
         process_names(i) = temp_names(i)
      end do
      
   end subroutine model_get_process_names

   !> Get number of configured processes
   function model_get_num_processes(this) result(num_processes)
      class(CATChem_Model), intent(inout) :: this
      integer :: num_processes
      
      type(ProcessManagerType), pointer :: process_mgr => null()
      character(len=64) :: temp_names(50)  ! Temporary array with max size
      
      num_processes = 0
      
      if (.not. this%initialized) return
      
      ! Get process manager from core
      process_mgr => this%core%get_process_manager()
      if (.not. associated(process_mgr)) return
      
      ! Get process count from ProcessManager
      call process_mgr%list_processes(temp_names, num_processes)
      
   end function model_get_num_processes


   !> Run a single timestep
   !! This method executes one timestep of the CATChem simulation
   subroutine model_run_timestep(this, timestep, dt, rc)
      class(CATChem_Model), intent(inout) :: this
      integer, intent(in) :: timestep         ! Current timestep number
      real(fp), intent(in) :: dt              ! Timestep size [s]
      integer, intent(out) :: rc
      
      rc = CATChem_SUCCESS
      call this%reset_error()
      
      if (.not. this%is_ready()) then
         this%last_error_msg = 'Model is not ready to run timestep'
         rc = CATChem_FAILURE
         return
      endif
      
      if (dt <= 0.0_fp) then
         this%last_error_msg = 'Timestep size must be positive'
         rc = CATChem_FAILURE
         return
      endif
      
      if (this%enable_run_phase) then
        call this%run_all_phases(rc)
        if (rc /= CC_SUCCESS) then
           this%last_error_msg = 'Failed to run all phases during timestep'
           rc = CATChem_FAILURE
           return
        endif
      else
        ! Run the core timestep
        call this%core%run_timestep(timestep, dt, rc)      
        if (rc /= CC_SUCCESS) then
            this%last_error_msg = 'Failed to run all processes during timestep'
            rc = CATChem_FAILURE
            return
        endif
      endif
   end subroutine model_run_timestep

   !> Run a specific phase
   !! This method executes a named phase of the simulation
   subroutine model_run_phase(this, phase_name, rc)
      class(CATChem_Model), intent(inout) :: this
      character(len=*), intent(in) :: phase_name
      integer, intent(out) :: rc
      
      type(ProcessManagerType), pointer :: process_mgr => null()
      type(StateManagerType), pointer :: state_mgr => null()
      type(ConfigDataType), pointer :: config_data => null()
      
      rc = CATChem_SUCCESS
      call this%reset_error()
      
      if (.not. this%is_ready()) then
         this%last_error_msg = 'Model is not ready for phase execution'
         rc = CATChem_FAILURE
         return
      endif
      
      ! Get managers and config data
      process_mgr => this%core%get_process_manager()
      state_mgr => this%core%get_state_manager()
      config_data => this%core%get_config()
      
      if (.not. associated(process_mgr) .or. .not. associated(state_mgr) .or. .not. associated(config_data)) then
         this%last_error_msg = 'Required managers or config data not available'
         rc = CATChem_FAILURE
         return
      endif
      
      ! Run the specific phase via ProcessManager
      call process_mgr%run_phase(phase_name, config_data, state_mgr, rc)
      if (rc /= CC_SUCCESS) then
         this%last_error_msg = 'Failed to run phase: ' // trim(phase_name)
         rc = CATChem_FAILURE
      endif
      
   end subroutine model_run_phase

   !> Run all configured phases in sequence
   !! This method executes all phases in the order they were configured using ConfigManager data
   subroutine model_run_all_phases(this, rc)
      class(CATChem_Model), intent(inout) :: this
      integer, intent(out) :: rc
      
      type(ProcessManagerType), pointer :: process_mgr => null()
      type(StateManagerType), pointer :: state_mgr => null()
      type(ConfigDataType), pointer :: config_data => null()
      
      rc = CATChem_SUCCESS
      call this%reset_error()
      
      if (.not. this%is_ready()) then
         this%last_error_msg = 'Model is not ready for phase execution'
         rc = CATChem_FAILURE
         return
      endif
      
      ! Get managers and config data
      process_mgr => this%core%get_process_manager()
      state_mgr => this%core%get_state_manager()
      config_data => this%core%get_config()
      
      if (.not. associated(process_mgr) .or. .not. associated(state_mgr) .or. .not. associated(config_data)) then
         this%last_error_msg = 'Required managers or config data not available'
         rc = CATChem_FAILURE
         return
      endif
      
      ! Run all phases in sequence using ConfigManager data
      call process_mgr%run_all_phases(config_data, state_mgr, rc)
      if (rc /= CC_SUCCESS) then
         this%last_error_msg = 'Failed to run all phases'
         rc = CATChem_FAILURE
      endif
      
   end subroutine model_run_all_phases

   !> Get names of configured run phases
   !! Note: This now gets phase information from ConfigManager
   subroutine model_get_phase_names(this, phase_names, rc)
      use ConfigManager_Mod, only: ConfigDataType
      class(CATChem_Model), intent(inout) :: this
      character(len=*), allocatable, intent(out) :: phase_names(:)
      integer, intent(out) :: rc
      
      type(ConfigDataType), pointer :: config_data => null()
      integer :: i
      
      rc = CATChem_SUCCESS
      
      if (.not. this%initialized) then
         allocate(phase_names(0))
         rc = CATChem_FAILURE
         return
      endif
      
      ! Get configuration data from core
      config_data => this%core%get_config()
      if (.not. associated(config_data)) then
         allocate(phase_names(0))
         rc = CATChem_FAILURE
         return
      endif
      
      ! Check if run phases are available
      if (.not. allocated(config_data%run_phases)) then
         allocate(phase_names(0))
         return
      endif
      
      allocate(phase_names(size(config_data%run_phases)))
      do i = 1, size(config_data%run_phases)
         phase_names(i) = config_data%run_phases(i)%name
      end do
      
   end subroutine model_get_phase_names


   !> Set meteorological data from host model
   !! This method transfers meteorological data from the host model to CATChem
   subroutine model_set_meteorology(this, temp, pressure, humidity, wind_u, wind_v, delp, frocean, frseaice, sst, u10m, v10m, ustar,rc)
      class(CATChem_Model), intent(inout) :: this
      real(fp), intent(in) :: temp(:,:,:)      ! Temperature [K]
      real(fp), intent(in) :: pressure(:,:,:)  ! Pressure [Pa]
      real(fp), intent(in) :: humidity(:,:,:)  ! Specific humidity [kg/kg]
      real(fp), intent(in) :: wind_u(:,:,:)    ! U-wind [m/s]
      real(fp), intent(in) :: wind_v(:,:,:)    ! V-wind [m/s]
      real(fp), intent(in), optional :: delp(:,:,:)    ! Pressure thickness [Pa]
      real(fp), intent(in), optional :: frocean(:,:)   ! Fraction ocean
      real(fp), intent(in), optional :: frseaice(:,:)  ! Fraction sea ice
      real(fp), intent(in), optional :: sst(:,:)       ! Sea surface temperature [K]
      real(fp), intent(in), optional :: u10m(:,:)      ! 10m U-wind [m/s]
      real(fp), intent(in), optional :: v10m(:,:)      ! 10m V-wind [m/s]
      real(fp), intent(in), optional :: ustar(:,:)     ! Friction velocity [m/s]
      integer, intent(out) :: rc
      
      type(StateManagerType), pointer :: state_mgr => null()
      type(MetStateType), pointer :: met_state => null()
      integer, dimension(3) :: expected_shape, actual_shape
      
      rc = CATChem_SUCCESS
      call this%reset_error()
      
      if (.not. this%is_ready()) then
         this%last_error_msg = 'Model is not ready for meteorology data'
         rc = CATChem_FAILURE
         return
      endif
      
      ! Validate array dimensions
      expected_shape = [this%nx, this%ny, this%nz]
      actual_shape = shape(temp)
      
      if (any(actual_shape /= expected_shape)) then
         this%last_error_msg = 'Temperature array dimensions do not match grid'
         rc = CATChem_FAILURE
         return
      endif
      
      ! Get state manager and meteorology state
      state_mgr => this%core%get_state_manager()
      if (.not. associated(state_mgr)) then
         this%last_error_msg = 'State manager not available'
         rc = CATChem_FAILURE
         return
      endif
      
      met_state => state_mgr%get_met_state_ptr()
      if (.not. associated(met_state)) then
         this%last_error_msg = 'Meteorology state not available'
         rc = CATChem_FAILURE
         return
      endif
      
      ! Copy data to meteorology state
      ! Note: These assignments use the actual MetState field names
      if (allocated(met_state%T)) then
         met_state%T = temp
      endif
      if (allocated(met_state%PMID)) then
         met_state%PMID = pressure
      endif
      if (allocated(met_state%QV)) then
         met_state%QV = humidity
      endif
      if (allocated(met_state%U)) then
         met_state%U = wind_u
      endif
      if (allocated(met_state%V)) then
         met_state%V = wind_v
      endif
      if (present(delp)) then
         if (allocated(met_state%DELP)) then
            met_state%DELP = delp
         endif
      endif
      if (present(frocean)) then
        if (allocated(met_state%FROCEAN)) then
            met_state%FROCEAN = frocean
        endif
      endif
      if (present(frseaice)) then
        if (allocated(met_state%FRSEAICE)) then
            met_state%FRSEAICE = frseaice
        endif
      endif
      if (present(sst)) then
        if (allocated(met_state%SST)) then
            met_state%SST = sst
        endif
      endif
      if (present(u10m)) then
        if (allocated(met_state%U10M)) then
            met_state%U10M = u10m
        endif
      endif
      if (present(v10m)) then
        if (allocated(met_state%V10M)) then
            met_state%V10M = v10m   
        endif
      endif
      if (present(ustar)) then
        if (allocated(met_state%USTAR)) then
            met_state%USTAR = ustar
        endif
      endif
   end subroutine model_set_meteorology

   !> Get meteorological data to host model
   !! This method transfers meteorological data from CATChem to the host model
   subroutine model_get_meteorology(this, temp, pressure, humidity, wind_u, wind_v, rc)
      class(CATChem_Model), intent(inout) :: this
      real(fp), allocatable, intent(out) :: temp(:,:,:)      ! Temperature [K]
      real(fp), allocatable, intent(out) :: pressure(:,:,:)  ! Pressure [Pa]
      real(fp), allocatable, intent(out) :: humidity(:,:,:)  ! Specific humidity [kg/kg]
      real(fp), allocatable, intent(out) :: wind_u(:,:,:)    ! U-wind [m/s]
      real(fp), allocatable, intent(out) :: wind_v(:,:,:)    ! V-wind [m/s]
      integer, intent(out) :: rc
      
      type(StateManagerType), pointer :: state_mgr => null()
      type(MetStateType), pointer :: met_state => null()
      
      rc = CATChem_SUCCESS
      
      if (.not. this%is_ready()) then
         rc = CATChem_FAILURE
         return
      endif
      
      ! Get state manager and meteorology state
      state_mgr => this%core%get_state_manager()
      if (.not. associated(state_mgr)) then
         rc = CATChem_FAILURE
         return
      endif
      
      met_state => state_mgr%get_met_state_ptr()
      if (.not. associated(met_state)) then
         rc = CATChem_FAILURE
         return
      endif
      
      ! Allocate and copy data from meteorology state
      allocate(temp(this%nx, this%ny, this%nz))
      allocate(pressure(this%nx, this%ny, this%nz))
      allocate(humidity(this%nx, this%ny, this%nz))
      allocate(wind_u(this%nx, this%ny, this%nz))
      allocate(wind_v(this%nx, this%ny, this%nz))
      
      if (allocated(met_state%T)) then
         temp = met_state%T
      endif
      if (allocated(met_state%PMID)) then
         pressure = met_state%PMID
      endif
      if (allocated(met_state%QV)) then
         humidity = met_state%QV
      endif
      if (allocated(met_state%U)) then
         wind_u = met_state%U
      endif
      if (allocated(met_state%V)) then
         wind_v = met_state%V
      endif
      
   end subroutine model_get_meteorology

   !> Set chemical concentrations from host model
   !! This method transfers chemical species data from the host model to CATChem
   subroutine model_set_chemistry(this, species_names, concentrations, rc)
      class(CATChem_Model), intent(inout) :: this
      character(len=*), intent(in) :: species_names(:)
      real(fp), intent(in) :: concentrations(:,:,:,:)  ! [species, nx, ny, nz]
      integer, intent(out) :: rc
      
      type(StateManagerType), pointer :: state_mgr => null()
      type(ChemStateType), pointer :: chem_state => null()
      integer :: num_species
      
      rc = CATChem_SUCCESS
      call this%reset_error()
      
      if (.not. this%is_ready()) then
         this%last_error_msg = 'Model is not ready for chemistry data'
         rc = CATChem_FAILURE
         return
      endif
      
      num_species = size(species_names)
      if (num_species /= size(concentrations, 1)) then
         this%last_error_msg = 'Number of species names does not match concentration dimensions'
         rc = CATChem_FAILURE
         return
      endif
      
      ! Get state manager and chemistry state
      state_mgr => this%core%get_state_manager()
      if (.not. associated(state_mgr)) then
         this%last_error_msg = 'State manager not available'
         rc = CATChem_FAILURE
         return
      endif
      
      chem_state => state_mgr%get_chem_state_ptr()
      if (.not. associated(chem_state)) then
         this%last_error_msg = 'Chemistry state not available'
         rc = CATChem_FAILURE
         return
      endif
      
      ! TODO: Implement chemistry data transfer
      ! This would involve mapping species names to indices and copying data
      this%last_error_msg = 'Chemistry data transfer not yet implemented'
      rc = CATChem_FAILURE
      
   end subroutine model_set_chemistry

   !> Get chemical concentrations to host model
   !! This method transfers chemical species data from CATChem to the host model
   subroutine model_get_chemistry(this, species_names, concentrations, rc)
      class(CATChem_Model), intent(inout) :: this
      character(len=*), allocatable, intent(out) :: species_names(:)
      real(fp), allocatable, intent(out) :: concentrations(:,:,:,:)  ! [species, nx, ny, nz]
      integer, intent(out) :: rc
      
      type(StateManagerType), pointer :: state_mgr => null()
      type(ChemStateType), pointer :: chem_state => null()
      
      rc = CATChem_SUCCESS
      
      if (.not. this%is_ready()) then
         rc = CATChem_FAILURE
         return
      endif
      
      ! Get state manager and chemistry state
      state_mgr => this%core%get_state_manager()
      if (.not. associated(state_mgr)) then
         rc = CATChem_FAILURE
         return
      endif
      
      chem_state => state_mgr%get_chem_state_ptr()
      if (.not. associated(chem_state)) then
         rc = CATChem_FAILURE
         return
      endif
      
      ! TODO: Implement chemistry data retrieval
      ! This would involve getting species names and copying concentration data
      allocate(species_names(0))
      allocate(concentrations(0, this%nx, this%ny, this%nz))
      
   end subroutine model_get_chemistry

   !> Set emission data from host model
   !! This method transfers emission data from the host model to CATChem
   subroutine model_set_emissions(this, species_names, emissions, rc)
      class(CATChem_Model), intent(inout) :: this
      character(len=*), intent(in) :: species_names(:)
      real(fp), intent(in) :: emissions(:,:,:,:)  ! [species, nx, ny, nz]
      integer, intent(out) :: rc
      
      rc = CATChem_SUCCESS
      call this%reset_error()
      
      if (.not. this%is_ready()) then
         this%last_error_msg = 'Model is not ready for emission data'
         rc = CATChem_FAILURE
         return
      endif
      
      ! TODO: Implement emission data transfer when EmisState is available
      this%last_error_msg = 'Emission data transfer not yet implemented'
      rc = CATChem_FAILURE
      
   end subroutine model_set_emissions

   !> Get names of available diagnostics
   !! This method retrieves the names of all available diagnostic fields
   subroutine model_get_diagnostic_names(this, diagnostic_names, rc)
      class(CATChem_Model), intent(inout) :: this
      character(len=*), allocatable, intent(out) :: diagnostic_names(:)
      integer, intent(out) :: rc
      
      type(DiagnosticManagerType), pointer :: diag_mgr => null()
      
      rc = CATChem_SUCCESS
      
      if (.not. this%initialized) then
         allocate(diagnostic_names(0))
         rc = CATChem_FAILURE
         return
      endif
      
      diag_mgr => this%core%get_diagnostic_manager()
      if (.not. associated(diag_mgr)) then
         allocate(diagnostic_names(0))
         rc = CATChem_FAILURE
         return
      endif
      
      ! TODO: Implement diagnostic name retrieval
      allocate(diagnostic_names(0))
      
   end subroutine model_get_diagnostic_names

   !> Get a specific diagnostic field
   !! This method retrieves data for a named diagnostic field
   subroutine model_get_diagnostic(this, diagnostic_name, diagnostic_data, rc)
      class(CATChem_Model), intent(inout) :: this
      character(len=*), intent(in) :: diagnostic_name
      real(fp), allocatable, intent(out) :: diagnostic_data(:,:,:)
      integer, intent(out) :: rc
      
      type(DiagnosticManagerType), pointer :: diag_mgr => null()
      
      rc = CATChem_SUCCESS
      
      if (.not. this%initialized) then
         rc = CATChem_FAILURE
         return
      endif
      
      diag_mgr => this%core%get_diagnostic_manager()
      if (.not. associated(diag_mgr)) then
         rc = CATChem_FAILURE
         return
      endif
      
      ! Allocate output array
      allocate(diagnostic_data(this%nx, this%ny, this%nz))
      diagnostic_data = 0.0_fp
      
      ! TODO: Implement specific diagnostic retrieval
      rc = CATChem_FAILURE
      
   end subroutine model_get_diagnostic

   !> Get all diagnostic data
   !! This method retrieves all available diagnostic data
   subroutine model_get_all_diagnostics(this, diagnostic_names, diagnostic_data, rc)
      class(CATChem_Model), intent(inout) :: this
      character(len=*), allocatable, intent(out) :: diagnostic_names(:)
      real(fp), allocatable, intent(out) :: diagnostic_data(:,:,:,:)  ! [diag, nx, ny, nz]
      integer, intent(out) :: rc
      
      rc = CATChem_SUCCESS
      
      if (.not. this%initialized) then
         rc = CATChem_FAILURE
         return
      endif
      
      ! TODO: Implement all diagnostics retrieval
      allocate(diagnostic_names(0))
      allocate(diagnostic_data(0, this%nx, this%ny, this%nz))
      
   end subroutine model_get_all_diagnostics

   !> Check if model is ready to run
   !! This method checks if all necessary components are initialized and configured
   function model_is_ready(this) result(is_ready)
      class(CATChem_Model), intent(inout) :: this
      logical :: is_ready
      
      is_ready = this%initialized .and. this%grid_setup .and. (this%get_num_processes() > 0)
   end function model_is_ready

   !> Check if model is initialized
   function model_is_initialized(this) result(is_initialized)
      class(CATChem_Model), intent(in) :: this
      logical :: is_initialized
      
      is_initialized = this%initialized
   end function model_is_initialized

   !> Get the last error message
   subroutine model_get_error_message(this, error_msg)
      class(CATChem_Model), intent(in) :: this
      character(len=*), intent(out) :: error_msg
      
      error_msg = trim(this%last_error_msg)
   end subroutine model_get_error_message

   !> Reset the error message
   subroutine model_reset_error(this)
      class(CATChem_Model), intent(inout) :: this
      
      this%last_error_msg = ''
   end subroutine model_reset_error

   ! Core access methods for advanced users
   
   !> Get direct access to the state manager (advanced usage)
   function model_get_state_manager(this) result(state_mgr_ptr)
      class(CATChem_Model), intent(inout) :: this
      type(StateManagerType), pointer :: state_mgr_ptr
      
      state_mgr_ptr => this%core%get_state_manager()
   end function model_get_state_manager

   !> Get direct access to the process manager (advanced usage)
   function model_get_process_manager(this) result(process_mgr_ptr)
      class(CATChem_Model), intent(inout) :: this
      type(ProcessManagerType), pointer :: process_mgr_ptr
      
      process_mgr_ptr => this%core%get_process_manager()
   end function model_get_process_manager

   !> Get direct access to the grid manager (advanced usage)
   function model_get_grid_manager(this) result(grid_mgr_ptr)
      class(CATChem_Model), intent(inout) :: this
      type(GridManagerType), pointer :: grid_mgr_ptr
      
      grid_mgr_ptr => this%core%get_grid_manager()
   end function model_get_grid_manager

   !> Get direct access to the diagnostic manager (advanced usage)
   function model_get_diagnostic_manager(this) result(diag_mgr_ptr)
      class(CATChem_Model), intent(inout) :: this
      type(DiagnosticManagerType), pointer :: diag_mgr_ptr
      
      diag_mgr_ptr => this%core%get_diagnostic_manager()
   end function model_get_diagnostic_manager

end module CATChem_API
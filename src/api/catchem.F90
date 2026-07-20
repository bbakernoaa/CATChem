!> \file catchem.F90
!! \brief CATChem core data types and routines.
!!
!! \defgroup catchem CATChem Main Group
!! \brief Main CATChem atmospheric chemistry modeling system
!!
!! This is the top-level group for the CATChem (Configurable ATmospheric CHEMistry)
!! modeling system, encompassing all processes, drivers, and core functionality.
!!
!! \defgroup catchem_api CATChem API
!! \brief Core CATChem API functions and data types
!! \ingroup catchem
!!
!! This group contains the main API functions and data types for the
!! CATChem atmospheric chemistry modeling system. This is the primary
!! interface for users integrating CATChem into their applications.
!!
!! \defgroup core_modules CATChem Core Modules
!! \brief Core modules and data types for CATChem
!! \ingroup catchem
!!
!! This group contains all core modules, state definitions, and fundamental
!! data types used throughout the CATChem system.
!!
!! \defgroup process_modules CATChem Process Modules
!! \brief All atmospheric chemistry process modules
!! \ingroup catchem
!!
!! This group contains all process modules in the CATChem system,
!! including dust, sea salt, dry deposition, and plume rise processes.
!!
!! \defgroup catchem_seasalt_process CATChem Sea Salt Process
!! \brief Sea salt emission and transport processes
!! \ingroup process_modules
!!
!! This group contains all modules, functions, and data types related to
!! sea salt emission calculations in the CATChem atmospheric chemistry model.
!!
!! \defgroup catchem_plumerise_process CATChem Plume Rise Process
!! \brief Point source emission vertical distribution
!! \ingroup process_modules
!!
!! This group contains modules and functions for calculating the vertical
!! distribution of point source emissions through plume rise calculations.
!!
!! \defgroup catchem_drydep_process CATChem Dry Deposition Process
!! \brief Atmospheric dry deposition calculations
!! \ingroup process_modules
!!
!! This group contains modules and functions for calculating dry deposition
!! of atmospheric species in the CATChem system.
!!
!! CATChem core data types and routines.
!!!>
module CATChem

   !---------------
   ! CATChem States
   !---------------
   use ChemState_Mod,  only: ChemStateType    ! Chemical State
   ! use GridState_Mod,  only: GridStateType    ! Grid State (DEPRECATED - removed)
   use DiagState_Mod,  only: DiagStateType    ! Diagnostic State
   use EmisState_Mod,  only: EmisStateType    ! Emission State
   use MetState_Mod,   only: MetStateType     ! Meteorology State
   use species_mod,    only: SpeciesType      ! Species State
   use Config_Opt_Mod, only: ConfigType
   use state_mod,      only: StateContainerType, StateBuilderType  ! Modern state management

   !------------------
   ! Grid Management
   !------------------
   use GridManager_Mod, only: GridManagerType, GridGeometryType, ColumnIteratorType
   use ColumnInterface_Mod, only: VirtualColumnType, ColumnProcessorType

   !--------------------
   ! Process Management
   !--------------------
   use ProcessInterface_Mod, only: ProcessInterface, ColumnProcessInterface
   use ProcessManager_Mod, only: ProcessManagerType
   use ProcessFactory_Mod, only: ProcessFactoryType

   !----------------
   ! Core routines
   !----------------
   ! chemstate
   use ChemState_Mod, only: cc_find_species_by_name => FindSpecByName
   use ChemState_Mod, only: cc_get_species_conc => GetSpecConc
   use ChemState_Mod, only: cc_get_species_conc_by_name => GetSpecConcByName
   use ChemState_Mod, only: cc_get_species_conc_by_index => GetSpecConcByIndex
   ! use ChemState_Mod, only: cc_allocate_chemstate => Chem_Allocate  ! DEPRECATED
   ! metstate
   ! use MetState_Mod, only: cc_allocate_metstate => Met_Allocate  ! DEPRECATED
   ! diagstate
   ! diagstate - Already modernized, use type-bound procedures:
   ! - DiagState%init() for initialization
   ! - DiagState%cleanup() for cleanup
   ! - DiagState%validate() for validation
   ! emisstate - All legacy functions removed. Use modern type-bound procedures:
   ! - EmisState%init() for initialization
   ! - EmisState%cleanup() for cleanup
   ! - EmisState%find_species() for species mapping
   ! - EmisState%apply_emissions() for applying emissions

   !-------------------
   ! Configuration Read
   !-------------------
   ! Legacy config_mod disabled - use modern ConfigManager instead:
   ! - ConfigManager%load_config() for configuration loading
   ! - ConfigManager%get_value() for accessing values

   !---------------
   ! Error Handling
   !---------------
   use Error_Mod, only: cc_check_var => CC_CheckVar     ! Method for checking variables
   use Error_Mod, only: cc_emit_error => CC_Error       ! Method for emitting errors
   use Error_Mod, only: CC_FAILURE                      ! CATCHem Failure return code
   use Error_Mod, only: CC_SUCCESS                      ! CATChem Successful return code
   use Error_Mod, only: cc_emit_warning => CC_Warning   ! Method for emitting warnings
   !
   use init_mod, only: cc_init_diag => Init_Diag        ! Method for initializing the diag state
   use init_mod, only: cc_init_met => Init_Met          ! Method for initializing the met state
   use init_mod, only: cc_init_emis => Init_Emis        ! Method for initializing the emission state
   use run_mod, only: cc_init_process => Init_Process  ! Method for initializing the process state
   use run_mod,  only: cc_run_process => Run_Process    ! Method for running the processes
   use run_mod,  only: cc_finalize_process => Finalize_Process    ! Method for finalizing the processes

   !-----------------------------
   ! Column Virtualization API
   !-----------------------------
   public :: cc_init_grid_manager       ! Initialize grid manager with column virtualization
   public :: cc_create_virtual_column   ! Create a virtual column for processing
   public :: cc_run_column_processes    ! Run all column-based processes
   public :: cc_process_all_columns     ! Process all columns with specified process

   !------------------
   ! CATChem Precision
   !------------------
   use precision_mod, only: cc_rk => fp                 ! Real Precision

   !------------------
   ! CATChem Processes
   !------------------
   ! Dust
   use CCPr_Dust_Common_Mod, only: DustStateType                                ! Dust State
   use CCPr_Dust_mod, only: cc_dust_init => CCPr_Dust_Init                      ! Dust Process Initialization Routine
   use CCPr_Dust_mod, only: cc_dust_run => CCPr_Dust_Run                        ! Dust Process Run Routine
   use CCPr_Dust_mod, only: cc_dust_finalize => CCPr_Dust_Finalize              ! Dust Process Finalization Routine
   ! Seasalt
   use CCPr_SeaSalt_Common_Mod, only: SeaSaltStateType                          ! SeaSalt State
   use CCPr_SeaSalt_mod, only: cc_seasalt_init => CCPr_SeaSalt_Init             ! SeaSalt Process Initialization Routine
   use CCPr_SeaSalt_mod, only: cc_seasalt_run => CCPr_SeaSalt_Run               ! SeaSalt Process Run Routine
   use CCPr_SeaSalt_mod, only: cc_seasalt_finalize => CCPr_SeaSalt_Finalize     ! SeaSalt Process Finalization Routine
   ! Plumerise
   use CCPr_Plumerise_mod, only: PlumeRiseStateType                               ! Plumerise State
   use CCPr_Plumerise_mod, only: cc_plumerise_init => CCPr_Plumerise_Init         ! Plumerise Process Initialization Routine
   use CCPr_Plumerise_mod, only: cc_plumerise_run => CCPr_Plumerise_Run           ! Plumerise Process Run Routine
   use CCPr_Plumerise_mod, only: cc_plumerise_finalize => CCPr_Plumerise_Finalize ! Plumerise Process Finalization Routine
   ! Dry Dep
   use CCPr_DryDep_mod, only: DryDepStateType                            ! DryDep State
   use CCPr_DryDep_mod, only: cc_drydep_init => CCPr_DryDep_Init         ! DryDep Process Initialization Routine
   use CCPr_DryDep_mod, only: cc_drydep_run => CCPr_DryDep_Run           ! DryDep Process Run Routine
   use CCPr_DryDep_mod, only: cc_drydep_finalize => CCPr_DryDep_Finalize ! DryDep Process Finalization Routine
   ! Chemical mechanism solver (TODO: turn off now)
   !use CCPr_Chem_mod, only: cc_get_micm_version => get_micm_version

   implicit none

   public

contains

   !> \brief Initialize grid manager with column virtualization
   !! \param[inout] container State container
   !! \param[in] nx Number of grid points in x direction
   !! \param[in] ny Number of grid points in y direction
   !! \param[in] nz Number of grid points in z direction
   !! \param[out] rc Return code
   subroutine cc_init_grid_manager(container, nx, ny, nz, rc)
      type(StateContainerType), intent(inout) :: container
      integer, intent(in) :: nx, ny, nz
      integer, intent(out) :: rc

      type(GridManagerType), pointer :: grid_mgr
      type(GridGeometryType) :: geometry

      ! Get grid manager from container
      grid_mgr => container%get_grid_manager()
      if (.not. associated(grid_mgr)) then
         rc = CC_FAILURE
         return
      endif

      ! Initialize geometry
      call geometry%init(nx, ny, nz, rc)
      if (rc /= CC_SUCCESS) return

      ! Initialize grid manager
      call grid_mgr%init(geometry, rc)
      if (rc /= CC_SUCCESS) return

      rc = CC_SUCCESS
   end subroutine cc_init_grid_manager

   !> \brief Create a virtual column for processing
   !! \param[inout] container State container
   !! \param[in] col_idx Column index
   !! \param[out] virtual_col Virtual column object
   !! \param[out] rc Return code
   subroutine cc_create_virtual_column(container, col_idx, virtual_col, rc)
      type(StateContainerType), intent(inout) :: container
      integer, intent(in) :: col_idx
      type(VirtualColumnType), intent(out) :: virtual_col
      integer, intent(out) :: rc

      type(GridManagerType), pointer :: grid_mgr

      ! Get grid manager from container
      grid_mgr => container%get_grid_manager()
      if (.not. associated(grid_mgr)) then
         rc = CC_FAILURE
         return
      endif

      ! Create virtual column
      call grid_mgr%create_virtual_column(col_idx, virtual_col, rc)
      if (rc /= CC_SUCCESS) return

      ! Extract data from container
      call virtual_col%extract_from_container(container, rc)
   end subroutine cc_create_virtual_column

   !> \brief Run all column-based processes using process manager
   !! \param[inout] proc_mgr Process manager
   !! \param[inout] container State container
   !! \param[out] rc Return code
   subroutine cc_run_column_processes(proc_mgr, container, rc)
      type(ProcessManagerType), intent(inout) :: proc_mgr
      type(StateContainerType), intent(inout) :: container
      integer, intent(out) :: rc

      call proc_mgr%run_column_processes(container, rc)
   end subroutine cc_run_column_processes

   !> \brief Process all columns with a specific process
   !! \param[inout] proc_mgr Process manager
   !! \param[in] process_name Name of process to run
   !! \param[inout] container State container
   !! \param[out] rc Return code
   subroutine cc_process_all_columns(proc_mgr, process_name, container, rc)
      type(ProcessManagerType), intent(inout) :: proc_mgr
      character(len=*), intent(in) :: process_name
      type(StateContainerType), intent(inout) :: container
      integer, intent(out) :: rc

      call proc_mgr%run_process(process_name, container, rc)
   end subroutine cc_process_all_columns

end module CATChem

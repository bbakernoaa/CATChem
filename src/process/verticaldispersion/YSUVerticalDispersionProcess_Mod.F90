!> \file ysuverticaldispersionProcess_Mod.F90
!! \brief ysuverticaldispersion process implementation
!! \ingroup process_modules
!!
!! \author CATChem Development Team
!! \date 2025
!! \version 1.0
!!
!! Yonsei University boundary layer scheme for vertical mixing of tracers
!!
module ysuverticaldispersionProcess_Mod
   use precision_mod
   use state_mod, only : StateContainerType
   use error_mod
   use ProcessInterface_Mod
   use DiagnosticInterface_Mod, only: DiagnosticFieldType, DiagnosticRegistryType, &
      DIAG_REAL_2D, DIAG_REAL_3D, DIAG_REAL_SCALAR
   use ColumnInterface_Mod, only: VirtualColumnType, ColumnProcessorType
   use scaleAwareYSUScheme_Mod

   implicit none
   private

   public :: ysuverticaldispersionProcessType

   !> ysuverticaldispersion process type extending ColumnProcessInterface for optimal performance
   type, extends(ColumnProcessInterface) :: ysuverticaldispersionProcessType
      private

      ! Process-specific configuration
      character(len=32) :: selected_scheme = 'scaleAwareYSU'

      ! Column processing support - ALWAYS use column virtualization for performance
      logical :: column_processing_enabled = .true.
      logical :: supports_3d_processing = .false.  ! Prefer column processing

      ! ============================================================================
      ! USER CUSTOMIZATION SECTION: Process-specific data members
      ! ============================================================================
      ! TODO: Add your process-specific data members here
      ! Example for transformation processes:
      ! real(fp), allocatable :: reaction_rates(:)        ! [1/s] reaction rate constants
      ! real(fp), allocatable :: photolysis_rates(:,:,:)  ! [1/s] J-values (nx,ny,nz)
      !
      ! Example for loss processes:
      ! real(fp), allocatable :: deposition_velocity(:,:) ! [m/s] dry deposition velocities
      ! real(fp), allocatable :: washout_rates(:)         ! [1/s] wet removal rates
      ! ============================================================================

   contains
      ! Required ProcessInterface methods
      procedure :: init => ysuverticaldispersion_process_init
      procedure :: run => ysuverticaldispersion_process_run
      procedure :: finalize => ysuverticaldispersion_process_finalize

      ! Enhanced diagnostic methods
      procedure :: register_diagnostics => ysuverticaldispersion_register_diagnostics
      procedure :: update_diagnostics => ysuverticaldispersion_update_diagnostics

      ! Column processing methods
      procedure :: run_column => ysuverticaldispersion_run_column
      procedure :: supports_column_processing => ysuverticaldispersion_supports_column_processing

      ! ============================================================================
      ! USER CUSTOMIZATION SECTION: Additional methods
      ! ============================================================================
      ! TODO: Add your process-specific methods here
      ! Example:
      ! procedure :: calculate_rates => ysuverticaldispersion_calc_rates
      ! procedure :: validate_inputs => ysuverticaldispersion_validate_inputs
      ! procedure :: apply_temporal_scaling => ysuverticaldispersion_apply_temporal_scaling
      ! ============================================================================
   end type ysuverticaldispersionProcessType

contains

   !> Initialize ysuverticaldispersion process
   subroutine ysuverticaldispersion_process_init(this, container, rc)
      class(ysuverticaldispersionProcessType), intent(inout) :: this
      type(StateContainerType), intent(inout) :: container
      integer, intent(out) :: rc

      type(ErrorManagerType), pointer :: error_mgr
      type(ConfigDataType), pointer :: config
      character(len=256) :: message

      rc = CC_SUCCESS

      error_mgr => container%get_error_manager()
      call error_mgr%push_context('ysuverticaldispersion_process_init', &
         'Initializing ysuverticaldispersion process')

      ! Set process metadata
      this%name = 'ysuverticaldispersion'
      this%version = '1.0'
      this%description = 'Yonsei University boundary layer scheme for vertical mixing of tracers'

      ! Get configuration
      config => container%get_config_ptr()
      if (.not. associated(config)) then
         call error_mgr%report_error(ERROR_NOT_INITIALIZED, &
            'Configuration not available', rc, &
            'ysuverticaldispersion_process_init', &
            'Ensure StateContainer is properly initialized')
         call error_mgr%pop_context()
         return
      endif

      ! ============================================================================
      ! USER CUSTOMIZATION SECTION: Process-specific initialization
      ! ============================================================================
      ! TODO: Add your process-specific initialization here
      ! Example for chemistry processes:
      ! call this%load_reaction_mechanism(config%mechanism_file, rc)
      ! if (rc /= CC_SUCCESS) then
      !    call error_mgr%report_error(ERROR_FILE_IO, &
      !                               'Failed to load reaction mechanism', rc)
      !    call error_mgr%pop_context()
      !    return
      ! endif
      ! ============================================================================


      ! Validate inputs
      ! Basic validation
      if (.not. container%is_ready()) then
         call error_mgr%report_error(ERROR_NOT_INITIALIZED, &
            'StateContainer not ready', rc)
         call error_mgr%pop_context()
         return
      end if

      ! Register diagnostics with the new diagnostic system
      call this%register_diagnostics(container, rc)
      if (rc /= CC_SUCCESS) then
         call error_mgr%report_error(ERROR_DIAGNOSTIC_REGISTRATION, &
            'Failed to register ysuverticaldispersion diagnostics', rc)
         call error_mgr%pop_context()
         return
      endif

      this%is_initialized = .true.
      this%is_active = .true.

      ! Register diagnostics after successful initialization
      call this%register_diagnostics(container, rc)
      if (rc /= CC_SUCCESS) then
         call error_mgr%report_error(ERROR_DIAGNOSTIC_REGISTRATION, &
            'Failed to register diagnostics', rc)
         call error_mgr%pop_context()
         return
      endif

      call error_mgr%pop_context()

      write(message, '(A,A,A)') 'ysuverticaldispersion process initialized with scheme: ', &
         trim(this%selected_scheme)
      call error_mgr%report_info(message)

      call error_mgr%pop_context()

   end subroutine ysuverticaldispersion_process_init

   !> Run ysuverticaldispersion process
   subroutine ysuverticaldispersion_process_run(this, container, rc)
      class(ysuverticaldispersionProcessType), intent(inout) :: this
      type(StateContainerType), intent(inout) :: container
      integer, intent(out) :: rc

      type(GridManagerType), pointer :: grid_mgr
      type(ErrorManagerType), pointer :: error_mgr

      rc = CC_SUCCESS

      if (.not. this%is_ready()) then
         rc = CC_FAILURE
         return
      end if

      error_mgr => container%get_error_manager()
      call error_mgr%push_context('ysuverticaldispersion_process_run', &
         'ysuverticaldispersion process execution')

      ! Use column virtualization if supported and enabled
      if (this%supports_column_processing() .and. this%column_processing_enabled) then
         grid_mgr => container%get_grid_manager_ptr()
         if (associated(grid_mgr) .and. grid_mgr%virtualization_enabled) then
            call this%run_column_batch(container, rc)
         else
            call this%run_3d_processing(container, rc)
         endif
      else
         call this%run_3d_processing(container, rc)
      endif

      if (rc /= CC_SUCCESS) then
         call error_mgr%report_error(ERROR_PROCESS_EXECUTION, &
            'ysuverticaldispersion process execution failed', rc)
         call error_mgr%pop_context()
         return
      endif

      ! Update diagnostics after successful execution
      call this%update_diagnostics(container, rc)
      if (rc /= CC_SUCCESS) then
         call error_mgr%report_error(ERROR_DIAGNOSTIC_UPDATE, &
            'Failed to update diagnostics', rc)
         call error_mgr%pop_context()
         return
      endif

      call error_mgr%pop_context()

   end subroutine ysuverticaldispersion_process_run

   !> Finalize ysuverticaldispersion process
   subroutine ysuverticaldispersion_process_finalize(this, rc)
      class(ysuverticaldispersionProcessType), intent(inout) :: this
      integer, intent(out) :: rc

      rc = CC_SUCCESS

      this%is_initialized = .false.
      this%is_active = .false.

   end subroutine ysuverticaldispersion_process_finalize

   !> Run column batch processing (optimized for performance)
   subroutine run_column_batch(this, container, rc)
      class(ysuverticaldispersionProcessType), intent(inout) :: this
      type(StateContainerType), intent(inout) :: container
      integer, intent(out) :: rc

      type(GridManagerType), pointer :: grid_mgr
      type(ColumnIteratorType) :: iterator
      type(VirtualColumnType) :: column
      type(ErrorManagerType), pointer :: error_mgr
      integer :: local_rc

      rc = CC_SUCCESS
      error_mgr => container%get_error_manager()

      grid_mgr => container%get_grid_manager_ptr()
      call grid_mgr%get_column_iterator(iterator, rc)
      if (rc /= CC_SUCCESS) return

      ! Process all columns using virtualization
      do while (iterator%has_next())
         call iterator%next(column)

         call this%run_column(column, container, local_rc)
         if (local_rc /= CC_SUCCESS) then
            rc = local_rc
            call error_mgr%report_error(ERROR_PROCESS_EXECUTION, &
               'Column processing failed', rc)
            return
         endif
      end do

   end subroutine run_column_batch

   !> Run 3D processing (fallback when column virtualization not available)
   subroutine run_3d_processing(this, container, rc)
      class(ysuverticaldispersionProcessType), intent(inout) :: this
      type(StateContainerType), intent(inout) :: container
      integer, intent(out) :: rc

      type(MetStateType), pointer :: met_state
      type(ChemStateType), pointer :: chem_state
      type(ErrorManagerType), pointer :: error_mgr

      rc = CC_SUCCESS
      error_mgr => container%get_error_manager()

      ! Get state pointers (no allocation, just access)
      met_state => container%get_met_state_ptr()
      chem_state => container%get_chem_state_ptr()

      ! Execute scheme-specific calculations on 3D data
      select case (trim(this%selected_scheme))
       case ('scaleAwareYSU')
         call this%run_scaleawareysu_scheme_3d(met_state, chem_state, rc)
       case default
         call error_mgr%report_error(ERROR_INVALID_CONFIG, &
            'Unknown scheme: ' // trim(this%selected_scheme), rc)
         return
      end select

   end subroutine run_3d_processing

   !> Column processing implementation (required by ColumnProcessInterface)
   subroutine ysuverticaldispersion_run_column(this, column, container, rc)
      class(ysuverticaldispersionProcessType), intent(inout) :: this
      type(VirtualColumnType), intent(inout) :: column
      type(StateContainerType), intent(inout) :: container
      integer, intent(out) :: rc

      ! ============================================================================
      ! USER CUSTOMIZATION SECTION: Column processing implementation
      ! ============================================================================
      ! TODO: Implement your column-specific processing here
      ! Use column%met_data, column%chem_data, column%emis_data pointers
      ! to access data without additional allocations

      ! Example for emission process:
      ! ============================================================================

      rc = CC_SUCCESS

      ! For now, use scheme-specific processing
      select case (trim(this%selected_scheme))
       case ('scaleAwareYSU')
         call this%run_scaleawareysu_column_scheme(column, container, rc)
       case default
         rc = CC_FAILURE
         return
      end select

   end subroutine ysuverticaldispersion_run_column

   !> Check if this process supports column processing
   function ysuverticaldispersion_supports_column_processing(this) result(supports)
      class(ysuverticaldispersionProcessType), intent(in) :: this
      logical :: supports

      supports = this%column_processing_enabled

   end function ysuverticaldispersion_supports_column_processing

   !> Register diagnostics for this process
   subroutine ysuverticaldispersion_register_diagnostics(this, container, rc)
      class(ysuverticaldispersionProcessType), intent(inout) :: this
      type(StateContainerType), intent(inout) :: container
      integer, intent(out) :: rc

      type(DiagnosticManagerType), pointer :: diag_mgr
      type(DiagnosticRegistryType), pointer :: registry
      type(ErrorManagerType), pointer :: error_mgr

      rc = CC_SUCCESS
      error_mgr => container%get_error_manager()
      call error_mgr%push_context('ysuverticaldispersion_register_diagnostics', &
         'Registering ysuverticaldispersion diagnostics')

      ! Call parent to register process
      call this%ProcessInterface%register_diagnostics(container, rc)
      if (rc /= CC_SUCCESS) then
         call error_mgr%pop_context()
         return
      endif

      ! Get diagnostic registry for this process
      diag_mgr => container%get_diagnostic_manager_ptr()
      call diag_mgr%get_process_registry(this%name, registry, rc)
      if (rc /= CC_SUCCESS) then
         call error_mgr%pop_context()
         return
      endif

      ! ============================================================================
      ! USER CUSTOMIZATION SECTION: Register process-specific diagnostics
      ! ============================================================================
      ! TODO: Register your process-specific diagnostic fields here

      ! Example diagnostic registrations:
      ! ============================================================================

      call error_mgr%pop_context()

   end subroutine ysuverticaldispersion_register_diagnostics

   !> Update diagnostics for this process
   subroutine ysuverticaldispersion_update_diagnostics(this, container, rc)
      class(ysuverticaldispersionProcessType), intent(inout) :: this
      type(StateContainerType), intent(inout) :: container
      integer, intent(out) :: rc

      type(DiagnosticManagerType), pointer :: diag_mgr
      type(DiagnosticRegistryType), pointer :: registry
      type(DiagnosticFieldType), pointer :: diag_field
      type(ErrorManagerType), pointer :: error_mgr

      rc = CC_SUCCESS
      error_mgr => container%get_error_manager()

      ! Get diagnostic registry for this process
      diag_mgr => container%get_diagnostic_manager_ptr()
      call diag_mgr%get_process_registry(this%name, registry, rc)
      if (rc /= CC_SUCCESS) return

      ! ============================================================================
      ! USER CUSTOMIZATION SECTION: Update process-specific diagnostics
      ! ============================================================================
      ! TODO: Update your diagnostic fields with current process data

      ! Example diagnostic updates:
      ! ============================================================================

   end subroutine ysuverticaldispersion_update_diagnostics

   !> Run standardYSU scheme on 3D data (fallback method)
   subroutine run_standardysu_scheme_3d(this, met_state, chem_state, rc)
      class(ysuverticaldispersionProcessType), intent(inout) :: this
      type(MetStateType), intent(in) :: met_state
      type(ChemStateType), intent(inout) :: chem_state
      integer, intent(out) :: rc

      ! ============================================================================
      ! USER CUSTOMIZATION SECTION: 3D processing implementation
      ! ============================================================================
      ! TODO: Implement your 3D processing here
      ! Use pointers to access data arrays without additional allocation

      ! ============================================================================

      rc = CC_SUCCESS

      ! Placeholder implementation
      write(*,*) 'Running standardYSU scheme on 3D data - implement me!'

   end subroutine run_standardysu_scheme_3d

   !> Run standardYSU scheme on column data (optimized method)
   subroutine run_standardysu_column_scheme(this, column, container, rc)
      class(ysuverticaldispersionProcessType), intent(inout) :: this
      type(VirtualColumnType), intent(inout) :: column
      type(StateContainerType), intent(inout) :: container
      integer, intent(out) :: rc

      ! ============================================================================
      ! USER CUSTOMIZATION SECTION: Column processing implementation
      ! ============================================================================
      ! TODO: Implement your column processing here
      ! Use column data pointers for efficient access (no allocation needed)

      ! ============================================================================

      rc = CC_SUCCESS

      ! Placeholder implementation
      write(*,*) 'Running standardYSU scheme on column data at (', &
         column%global_i, ',', column%global_j, ') - implement me!'

   end subroutine run_standardysu_column_scheme

   !> Run enhancedYSU scheme on 3D data (fallback method)
   subroutine run_enhancedysu_scheme_3d(this, met_state, chem_state, rc)
      class(ysuverticaldispersionProcessType), intent(inout) :: this
      type(MetStateType), intent(in) :: met_state
      type(ChemStateType), intent(inout) :: chem_state
      integer, intent(out) :: rc

      ! ============================================================================
      ! USER CUSTOMIZATION SECTION: 3D processing implementation
      ! ============================================================================
      ! TODO: Implement your 3D processing here
      ! Use pointers to access data arrays without additional allocation

      ! ============================================================================

      rc = CC_SUCCESS

      ! Placeholder implementation
      write(*,*) 'Running enhancedYSU scheme on 3D data - implement me!'

   end subroutine run_enhancedysu_scheme_3d

   !> Run enhancedYSU scheme on column data (optimized method)
   subroutine run_enhancedysu_column_scheme(this, column, container, rc)
      class(ysuverticaldispersionProcessType), intent(inout) :: this
      type(VirtualColumnType), intent(inout) :: column
      type(StateContainerType), intent(inout) :: container
      integer, intent(out) :: rc

      ! ============================================================================
      ! USER CUSTOMIZATION SECTION: Column processing implementation
      ! ============================================================================
      ! TODO: Implement your column processing here
      ! Use column data pointers for efficient access (no allocation needed)

      ! ============================================================================

      rc = CC_SUCCESS

      ! Placeholder implementation
      write(*,*) 'Running enhancedYSU scheme on column data at (', &
         column%global_i, ',', column%global_j, ') - implement me!'

   end subroutine run_enhancedysu_column_scheme

   !> Run scaleAwareYSU scheme on 3D data (fallback method)
   subroutine run_scaleawareysu_scheme_3d(this, met_state, chem_state, rc)
      class(ysuverticaldispersionProcessType), intent(inout) :: this
      type(MetStateType), intent(in) :: met_state
      type(ChemStateType), intent(inout) :: chem_state
      integer, intent(out) :: rc

      ! ============================================================================
      ! USER CUSTOMIZATION SECTION: 3D processing implementation
      ! ============================================================================
      ! TODO: Implement your 3D processing here
      ! Use pointers to access data arrays without additional allocation

      ! ============================================================================

      rc = CC_SUCCESS

      ! Placeholder implementation
      write(*,*) 'Running scaleAwareYSU scheme on 3D data - implement me!'

   end subroutine run_scaleawareysu_scheme_3d

   !> Run scaleAwareYSU scheme on column data (optimized method)
   subroutine run_scaleawareysu_column_scheme(this, column, container, rc)
      class(ysuverticaldispersionProcessType), intent(inout) :: this
      type(VirtualColumnType), intent(inout) :: column
      type(StateContainerType), intent(inout) :: container
      integer, intent(out) :: rc

      ! ============================================================================
      ! USER CUSTOMIZATION SECTION: Column processing implementation
      ! ============================================================================
      ! TODO: Implement your column processing here
      ! Use column data pointers for efficient access (no allocation needed)

      ! ============================================================================

      rc = CC_SUCCESS

      ! Placeholder implementation
      write(*,*) 'Running scaleAwareYSU scheme on column data at (', &
         column%global_i, ',', column%global_j, ') - implement me!'

   end subroutine run_scaleawareysu_column_scheme

end module ysuverticaldispersionProcess_Mod

!> \file DiagnosticManager_Mod.F90
!! \brief Central diagnostic manager integrating with CATChem framework
!! \ingroup core_modules
!!
!! \author CATChem Development Team
!! \date 2025
!! \version 2.0
!!
!! This module provides a central diagnostic manager that integrates the dynamic
!! diagnostic system with the existing CATChem framework (StateContainer,
!! ProcessManager, ConfigManager, ErrorManager).
!!
!! \details
!! The DiagnosticManager:
!! - Manages diagnostic registries for all processes
!! - Integrates with StateContainer for state management
!! - Uses ErrorManager for consistent error handling
!! - Supports ConfigManager for output configuration
!! - Provides centralized diagnostic collection and output
!! - Replaces static diagstate_mod with dynamic system
!!
!! \section diag_manager_usage Usage Example
!! \code{.f90}
!! use DiagnosticManager_Mod
!! type(DiagnosticManagerType) :: diag_mgr
!! type(StateContainerType) :: container
!! integer :: rc
!!
!! ! Initialize diagnostic manager
!! call diag_mgr%init(container, rc)
!!
!! ! Processes register their diagnostics via process manager
!! call process_mgr%run_all(container, rc)
!!
!! ! Collect and write diagnostics
!! call diag_mgr%collect_all_diagnostics(container, rc)
!! call diag_mgr%write_output(container, rc)
!! \endcode
!!
module DiagnosticManager_Mod
   use precision_mod, only: fp
   use error_mod, only: ErrorManagerType, CC_SUCCESS, CC_FAILURE, &
                        ERROR_INVALID_INPUT, ERROR_NOT_FOUND, ERROR_MEMORY_ALLOCATION
   use DiagnosticInterface_Mod, only: DiagnosticRegistryType, DiagnosticFieldType, &
                                      DiagnosticDataType
   ! Removed StateManager_Mod import to break circular dependency

   implicit none
   private

   public :: DiagnosticManagerType

   !> \brief Central diagnostic manager for CATChem
   !!
   !! This type manages all process-specific diagnostic registries and provides
   !! centralized diagnostic collection, output, and management integrated with
   !! the CATChem framework.
   type :: DiagnosticManagerType
      private

      ! Error management
      type(ErrorManagerType), pointer :: error_mgr => null()

      ! Process registries management
      type(DiagnosticRegistryType), allocatable :: process_registries(:)
      character(len=64), allocatable :: process_names(:)
      integer :: num_processes = 0
      integer :: max_processes = 50

      ! Configuration and output management
      logical :: output_enabled = .true.
      character(len=256) :: output_prefix = 'catchem_diag'
      character(len=256) :: output_directory = './'
      integer :: output_frequency = 1  ! timesteps

      ! Collection state
      logical :: collection_enabled = .true.
      integer :: current_timestep = 0

   contains
      ! Initialization and management
      procedure :: init => diagnostic_manager_init
      procedure :: finalize => diagnostic_manager_finalize
      procedure :: reset => diagnostic_manager_reset

      ! Process registry management
      procedure :: register_process => diagnostic_manager_register_process
      procedure :: get_process_registry => diagnostic_manager_get_process_registry
      procedure :: remove_process => diagnostic_manager_remove_process
      procedure :: list_processes => diagnostic_manager_list_processes

      ! Configuration management
      procedure :: configure_output => diagnostic_manager_configure_output
      procedure :: set_output_frequency => diagnostic_manager_set_output_frequency
      procedure :: enable_collection => diagnostic_manager_enable_collection
      procedure :: disable_collection => diagnostic_manager_disable_collection

      ! Diagnostic collection and output
      procedure :: collect_all_diagnostics => diagnostic_manager_collect_all
      procedure :: collect_process_diagnostics => diagnostic_manager_collect_process
      procedure :: write_output => diagnostic_manager_write_output
      procedure :: advance_timestep => diagnostic_manager_advance_timestep

      ! Utility methods
      procedure :: get_total_diagnostics => diagnostic_manager_get_total_diagnostics
      procedure :: print_summary => diagnostic_manager_print_summary
      procedure :: validate_state => diagnostic_manager_validate_state

   end type DiagnosticManagerType

contains

   !> \brief Initialize diagnostic manager
   !!
   !! \param[inout] this DiagnosticManagerType instance
   !! \param[inout] error_mgr ErrorManager for error handling
   !! \param[out] rc Return code
   subroutine diagnostic_manager_init(this, error_mgr, rc)
      class(DiagnosticManagerType), intent(inout) :: this
      type(ErrorManagerType), intent(inout), target :: error_mgr
      integer, intent(out) :: rc

      rc = CC_SUCCESS

      ! Store ErrorManager reference for internal use
      this%error_mgr => error_mgr

      ! Initialize diagnostic manager
      call this%error_mgr%push_context('diagnostic_manager_init', 'Initializing diagnostic manager')

      ! Allocate process arrays
      if (.not. allocated(this%process_registries)) then
         allocate(this%process_registries(this%max_processes), stat=rc)
         if (rc /= 0) then
            call this%error_mgr%report_error(ERROR_MEMORY_ALLOCATION, &
                                        'Failed to allocate process registries', rc)
            call this%error_mgr%pop_context()
            return
         endif
      endif

      if (.not. allocated(this%process_names)) then
         allocate(this%process_names(this%max_processes), stat=rc)
         if (rc /= 0) then
            call this%error_mgr%report_error(ERROR_MEMORY_ALLOCATION, &
                                        'Failed to allocate process names', rc)
            call this%error_mgr%pop_context()
            return
         endif
      endif

      ! Initialize counters
      this%num_processes = 0
      this%current_timestep = 0

      ! TODO: Read configuration from container's ConfigManager
      ! For now, use defaults

      call this%error_mgr%pop_context()

   end subroutine diagnostic_manager_init

   !> \brief Finalize diagnostic manager
   !!
   !! \param[inout] this DiagnosticManagerType instance
   !! \param[out] rc Return code
   subroutine diagnostic_manager_finalize(this, rc)
      class(DiagnosticManagerType), intent(inout) :: this
      integer, intent(out) :: rc

      integer :: i, local_rc

      rc = CC_SUCCESS

      ! Finalize all process registries
      do i = 1, this%num_processes
         call this%process_registries(i)%finalize(local_rc)
         if (local_rc /= CC_SUCCESS) then
            rc = local_rc
         endif
      enddo

      ! Deallocate arrays
      if (allocated(this%process_registries)) then
         deallocate(this%process_registries)
      endif

      if (allocated(this%process_names)) then
         deallocate(this%process_names)
      endif

      this%num_processes = 0

   end subroutine diagnostic_manager_finalize

   !> \brief Reset diagnostic manager state
   !!
   !! \param[inout] this DiagnosticManagerType instance
   !! \param[out] rc Return code
   subroutine diagnostic_manager_reset(this, rc)
      class(DiagnosticManagerType), intent(inout) :: this
      integer, intent(out) :: rc

      integer :: i, local_rc

      rc = CC_SUCCESS

      ! Reset all process registries
      do i = 1, this%num_processes
         call this%process_registries(i)%reset(local_rc)
         if (local_rc /= CC_SUCCESS) then
            rc = local_rc
         endif
      enddo

      this%current_timestep = 0

   end subroutine diagnostic_manager_reset

   !> \brief Register a process with the diagnostic manager
   !!
   !! \param[inout] this DiagnosticManagerType instance
   !! \param[in] process_name Name of the process to register
   !! \param[out] rc Return code
   subroutine diagnostic_manager_register_process(this, process_name, rc)
      class(DiagnosticManagerType), intent(inout) :: this
      character(len=*), intent(in) :: process_name
      integer, intent(out) :: rc

      integer :: i

      rc = CC_SUCCESS

      ! Check if process already registered
      do i = 1, this%num_processes
         if (trim(this%process_names(i)) == trim(process_name)) then
            call this%error_mgr%report_error(ERROR_INVALID_INPUT, &
                                        'Process already registered: ' // trim(process_name), rc)
            return
         endif
      enddo

      ! Check capacity
      if (this%num_processes >= this%max_processes) then
         call this%error_mgr%report_error(ERROR_MEMORY_ALLOCATION, &
                                     'Maximum number of processes reached', rc)
         return
      endif

      ! Add new process
      this%num_processes = this%num_processes + 1
      this%process_names(this%num_processes) = trim(process_name)

      ! Initialize process registry
      call this%process_registries(this%num_processes)%init(process_name, this%error_mgr, rc)

   end subroutine diagnostic_manager_register_process

   !> \brief Get diagnostic registry for a specific process (returns pointer to internal registry)
   !!
   !! \param[inout] this DiagnosticManagerType instance
   !! \param[in] process_name Name of the process
   !! \param[out] registry Pointer to the diagnostic registry
   !! \param[out] rc Return code
   subroutine diagnostic_manager_get_process_registry(this, process_name, registry, rc)
      class(DiagnosticManagerType), intent(inout), target :: this
      character(len=*), intent(in) :: process_name
      type(DiagnosticRegistryType), pointer, intent(out) :: registry
      integer, intent(out) :: rc

      integer :: i

      rc = CC_FAILURE
      nullify(registry)

      ! Find process
      do i = 1, this%num_processes
         if (trim(this%process_names(i)) == trim(process_name)) then
            registry => this%process_registries(i)
            rc = CC_SUCCESS
            return
         endif
      enddo

   end subroutine diagnostic_manager_get_process_registry

   !> \brief Remove a process from the diagnostic manager
   !!
   !! \param[inout] this DiagnosticManagerType instance
   !! \param[in] process_name Name of the process to remove
   !! \param[out] rc Return code
   subroutine diagnostic_manager_remove_process(this, process_name, rc)
      class(DiagnosticManagerType), intent(inout) :: this
      character(len=*), intent(in) :: process_name
      integer, intent(out) :: rc

      integer :: i, j, local_rc

      rc = CC_FAILURE

      ! Find and remove process
      do i = 1, this%num_processes
         if (trim(this%process_names(i)) == trim(process_name)) then
            ! Finalize registry
            call this%process_registries(i)%finalize(local_rc)

            ! Shift remaining processes
            do j = i, this%num_processes - 1
               this%process_names(j) = this%process_names(j + 1)
               this%process_registries(j) = this%process_registries(j + 1)
            enddo

            this%num_processes = this%num_processes - 1
            rc = CC_SUCCESS
            return
         endif
      enddo

   end subroutine diagnostic_manager_remove_process

   !> \brief List all registered processes
   !!
   !! \param[in] this DiagnosticManagerType instance
   !! \param[out] process_list Array of process names
   !! \param[out] num_processes Number of processes
   !! \param[out] rc Return code
   subroutine diagnostic_manager_list_processes(this, process_list, num_processes, rc)
      class(DiagnosticManagerType), intent(in) :: this
      character(len=64), allocatable, intent(out) :: process_list(:)
      integer, intent(out) :: num_processes
      integer, intent(out) :: rc

      rc = CC_SUCCESS
      num_processes = this%num_processes

      if (num_processes > 0) then
         allocate(process_list(num_processes), stat=rc)
         if (rc /= 0) then
            rc = ERROR_MEMORY_ALLOCATION
            return
         endif

         process_list(1:num_processes) = this%process_names(1:num_processes)
      endif

   end subroutine diagnostic_manager_list_processes

   !> \brief Configure diagnostic output settings
   !!
   !! \param[inout] this DiagnosticManagerType instance
   !! \param[in] output_prefix Optional output file prefix
   !! \param[in] output_directory Optional output directory
   !! \param[in] output_frequency Optional output frequency in timesteps
   !! \param[out] rc Return code
   subroutine diagnostic_manager_configure_output(this, rc, output_prefix, &
                                                  output_directory, output_frequency)
      class(DiagnosticManagerType), intent(inout) :: this
      integer, intent(out) :: rc
      character(len=*), optional, intent(in) :: output_prefix
      character(len=*), optional, intent(in) :: output_directory
      integer, optional, intent(in) :: output_frequency

      rc = CC_SUCCESS

      if (present(output_prefix)) then
         this%output_prefix = trim(output_prefix)
      endif

      if (present(output_directory)) then
         this%output_directory = trim(output_directory)
      endif

      if (present(output_frequency)) then
         this%output_frequency = output_frequency
      endif

   end subroutine diagnostic_manager_configure_output

   !> \brief Set output frequency
   !!
   !! \param[inout] this DiagnosticManagerType instance
   !! \param[in] frequency Output frequency in timesteps
   !! \param[out] rc Return code
   subroutine diagnostic_manager_set_output_frequency(this, frequency, rc)
      class(DiagnosticManagerType), intent(inout) :: this
      integer, intent(in) :: frequency
      integer, intent(out) :: rc

      rc = CC_SUCCESS

      if (frequency > 0) then
         this%output_frequency = frequency
      else
         rc = ERROR_INVALID_INPUT
      endif

   end subroutine diagnostic_manager_set_output_frequency

   !> \brief Enable diagnostic collection
   !!
   !! \param[inout] this DiagnosticManagerType instance
   !! \param[out] rc Return code
   subroutine diagnostic_manager_enable_collection(this, rc)
      class(DiagnosticManagerType), intent(inout) :: this
      integer, intent(out) :: rc

      rc = CC_SUCCESS
      this%collection_enabled = .true.

   end subroutine diagnostic_manager_enable_collection

   !> \brief Disable diagnostic collection
   !!
   !! \param[inout] this DiagnosticManagerType instance
   !! \param[out] rc Return code
   subroutine diagnostic_manager_disable_collection(this, rc)
      class(DiagnosticManagerType), intent(inout) :: this
      integer, intent(out) :: rc

      rc = CC_SUCCESS
      this%collection_enabled = .false.

   end subroutine diagnostic_manager_disable_collection

   !> \brief Collect diagnostics from all processes
   !!
   !! \param[inout] this DiagnosticManagerType instance
   !! \param[out] rc Return code
   subroutine diagnostic_manager_collect_all(this, rc)
      class(DiagnosticManagerType), intent(inout) :: this
      integer, intent(out) :: rc

      integer :: i, local_rc

      rc = CC_SUCCESS

      if (.not. this%collection_enabled) return

      call this%error_mgr%push_context('diagnostic_manager_collect_all', &
                                  'Collecting diagnostics from all processes')

      ! Collect from each process
      do i = 1, this%num_processes
         call this%collect_process_diagnostics(this%process_names(i), local_rc)
         if (local_rc /= CC_SUCCESS) then
            call this%error_mgr%report_error(local_rc, &
                                        'Failed to collect diagnostics from: ' // &
                                        trim(this%process_names(i)), rc)
            ! Continue with other processes
         endif
      enddo

      call this%error_mgr%pop_context()

   end subroutine diagnostic_manager_collect_all

   !> \brief Collect diagnostics from a specific process
   !!
   !! \param[inout] this DiagnosticManagerType instance
   !! \param[in] process_name Name of the process
   !! \param[out] rc Return code
   subroutine diagnostic_manager_collect_process(this, process_name, rc)
      class(DiagnosticManagerType), intent(inout) :: this
      character(len=*), intent(in) :: process_name
      integer, intent(out) :: rc

      type(DiagnosticRegistryType), pointer :: registry

      ! Get process registry
      call this%get_process_registry(process_name, registry, rc)
      if (rc /= CC_SUCCESS) return

      ! Collect diagnostics (would access state data to update diagnostic values)
      ! This is a placeholder - actual implementation would extract data from
      ! StateContainer components and update diagnostic field values
      rc = CC_SUCCESS

   end subroutine diagnostic_manager_collect_process

   !> \brief Write diagnostic output
   !!
   !! \param[inout] this DiagnosticManagerType instance
   !! \param[out] rc Return code
   subroutine diagnostic_manager_write_output(this, rc)
      class(DiagnosticManagerType), intent(inout) :: this
      integer, intent(out) :: rc

      rc = CC_SUCCESS

      if (.not. this%output_enabled) return

      ! Check if it's time to output
      if (mod(this%current_timestep, this%output_frequency) /= 0) return

      call this%error_mgr%push_context('diagnostic_manager_write_output', &
                                  'Writing diagnostic output')

      ! TODO: Implement actual file output
      ! This would write diagnostic data to NetCDF files or other formats
      ! For now, placeholder

      call this%error_mgr%pop_context()

   end subroutine diagnostic_manager_write_output

   !> \brief Advance timestep counter
   !!
   !! \param[inout] this DiagnosticManagerType instance
   !! \param[out] rc Return code
   subroutine diagnostic_manager_advance_timestep(this, rc)
      class(DiagnosticManagerType), intent(inout) :: this
      integer, intent(out) :: rc

      rc = CC_SUCCESS
      this%current_timestep = this%current_timestep + 1

   end subroutine diagnostic_manager_advance_timestep

   !> \brief Get total number of diagnostics across all processes
   !!
   !! \param[in] this DiagnosticManagerType instance
   !! \result total_diagnostics Total number of registered diagnostics
   function diagnostic_manager_get_total_diagnostics(this) result(total_diagnostics)
      class(DiagnosticManagerType), intent(in) :: this
      integer :: total_diagnostics

      integer :: i

      total_diagnostics = 0

      do i = 1, this%num_processes
         total_diagnostics = total_diagnostics + this%process_registries(i)%get_num_diagnostics()
      enddo

   end function diagnostic_manager_get_total_diagnostics

   !> \brief Print diagnostic manager summary
   !!
   !! \param[in] this DiagnosticManagerType instance
   subroutine diagnostic_manager_print_summary(this)
      class(DiagnosticManagerType), intent(in) :: this

      integer :: i

      write(*,'(A)') '=== DiagnosticManager Summary ==='
      write(*,'(A,I0)') 'Number of processes: ', this%num_processes
      write(*,'(A,I0)') 'Total diagnostics: ', this%get_total_diagnostics()
      write(*,'(A,L1)') 'Collection enabled: ', this%collection_enabled
      write(*,'(A,L1)') 'Output enabled: ', this%output_enabled
      write(*,'(A,I0)') 'Output frequency: ', this%output_frequency
      write(*,'(A,I0)') 'Current timestep: ', this%current_timestep

      write(*,'(A)') 'Registered processes:'
      do i = 1, this%num_processes
         write(*,'(A,I0,A,A,A,I0,A)') '  ', i, ': ', trim(this%process_names(i)), &
                                      ' (', this%process_registries(i)%get_num_diagnostics(), ' diagnostics)'
      enddo

      write(*,'(A)') '================================='

   end subroutine diagnostic_manager_print_summary

   !> \brief Validate diagnostic manager state
   !!
   !! \param[inout] this DiagnosticManagerType instance
   !! \param[out] rc Return code
   subroutine diagnostic_manager_validate_state(this, rc)
      class(DiagnosticManagerType), intent(inout) :: this
      integer, intent(out) :: rc

      integer :: i, local_rc

      rc = CC_SUCCESS

      ! Validate each process registry
      do i = 1, this%num_processes
         call this%process_registries(i)%validate(this%error_mgr, local_rc)
         if (local_rc /= CC_SUCCESS) then
            call this%error_mgr%report_error(local_rc, &
                                        'Validation failed for process: ' // &
                                        trim(this%process_names(i)), rc)
            return
         endif
      enddo

   end subroutine diagnostic_manager_validate_state

end module DiagnosticManager_Mod

!> \file test_ProcessInterface.f90
!! \brief Test program for ProcessInterface module
!!
!! Tests the flattened type hierarchy where column processing methods
!! are directly accessible on ProcessInterface.
!!
!! **Validates: Requirements 1.1, 1.2, 1.7**
!!
module test_ProcessInterface_mod
   use testing_mod, only: assert, assert_close
   use ProcessInterface_Mod
   use StateManager_Mod, only: StateManagerType
   use VirtualColumn_Mod, only: VirtualColumnType
   use Error_Mod, only: CC_SUCCESS, CC_FAILURE

   implicit none

   !> Concrete dummy process extending ProcessInterface directly.
   !! Implements all deferred methods including column processing,
   !! proving the flattened hierarchy works.
   type, extends(ProcessInterface) :: DummyProcessType
      integer :: init_column_call_count = 0
      integer :: run_column_call_count = 0
      integer :: finalize_column_call_count = 0
   contains
      procedure, public :: init => dummy_init
      procedure, public :: run => dummy_run
      procedure, public :: finalize => dummy_finalize
      procedure, public :: init_column_processing => dummy_init_column_processing
      procedure, public :: run_column => dummy_run_column
      procedure, public :: finalize_column_processing => dummy_finalize_column_processing
   end type DummyProcessType

contains

   subroutine dummy_init(this, container, rc)
      class(DummyProcessType), intent(inout) :: this
      type(StateManagerType), intent(inout) :: container
      integer, intent(out) :: rc

      this%name = 'DummyProcess'
      this%version = '1.0'
      this%description = 'A dummy process for testing'
      call this%activate()
      rc = CC_SUCCESS
   end subroutine dummy_init

   subroutine dummy_run(this, container, rc)
      class(DummyProcessType), intent(inout) :: this
      type(StateManagerType), intent(inout) :: container
      integer, intent(out) :: rc

      rc = CC_SUCCESS
   end subroutine dummy_run

   subroutine dummy_finalize(this, rc)
      class(DummyProcessType), intent(inout) :: this
      integer, intent(out) :: rc

      call this%deactivate()
      rc = CC_SUCCESS
   end subroutine dummy_finalize

   subroutine dummy_init_column_processing(this, container, rc)
      class(DummyProcessType), intent(inout) :: this
      type(StateManagerType), intent(inout) :: container
      integer, intent(out) :: rc

      this%init_column_call_count = this%init_column_call_count + 1
      rc = CC_SUCCESS
   end subroutine dummy_init_column_processing

   subroutine dummy_run_column(this, column, container, rc)
      class(DummyProcessType), intent(inout) :: this
      type(VirtualColumnType), intent(inout) :: column
      type(StateManagerType), intent(inout) :: container
      integer, intent(out) :: rc

      this%run_column_call_count = this%run_column_call_count + 1
      rc = CC_SUCCESS
   end subroutine dummy_run_column

   subroutine dummy_finalize_column_processing(this, rc)
      class(DummyProcessType), intent(inout) :: this
      integer, intent(out) :: rc

      this%finalize_column_call_count = this%finalize_column_call_count + 1
      rc = CC_SUCCESS
   end subroutine dummy_finalize_column_processing

end module test_ProcessInterface_mod

program test_ProcessInterface
   use test_ProcessInterface_mod
   use precision_mod, only: fp
   implicit none

   type(DummyProcessType) :: proc
   type(StateManagerType) :: state_mgr
   integer :: rc
   integer :: batch_size
   logical :: enabled

   write(*,*) 'Testing ProcessInterface module...'
   write(*,*) ''

   ! ================================================================
   ! Test 1: State manager initialization
   ! ================================================================
   write(*,*) 'Test 1: State manager initialization'
   call state_mgr%init('TestStateManager', rc)
   call assert(rc == CC_SUCCESS, "StateManager initialization should succeed")
   write(*,*) 'Test 1 passed!'
   write(*,*) ''

   ! ================================================================
   ! Test 2: Dummy process initialization
   ! ================================================================
   write(*,*) 'Test 2: Dummy process initialization'
   call proc%init(state_mgr, rc)
   call assert(rc == CC_SUCCESS, "Dummy process initialization should succeed")
   write(*,*) 'Test 2 passed!'
   write(*,*) ''

   ! ================================================================
   ! Test 3: Process properties
   ! ================================================================
   write(*,*) 'Test 3: Process properties'
   call assert(proc%is_ready(), "Process should be ready after initialization")
   write(*,*) 'Test 3 passed!'
   write(*,*) ''

   ! ================================================================
   ! Test 4: Process activation/deactivation
   ! ================================================================
   write(*,*) 'Test 4: Process activation/deactivation'
   call proc%deactivate()
   call assert(.not. proc%is_ready(), "Process should not be ready when deactivated")
   call proc%activate()
   call assert(proc%is_ready(), "Process should be ready after activation")
   write(*,*) 'Test 4 passed!'
   write(*,*) ''

   ! ================================================================
   ! Test 5: Running the process
   ! ================================================================
   write(*,*) 'Test 5: Running the process'
   call proc%run(state_mgr, rc)
   call assert(rc == CC_SUCCESS, "Process run should succeed")
   write(*,*) 'Test 5 passed!'
   write(*,*) ''

   ! ================================================================
   ! Test 6: Process finalization
   ! ================================================================
   write(*,*) 'Test 6: Process finalization'
   call proc%finalize(rc)
   call assert(rc == CC_SUCCESS, "Process finalization should succeed")
   write(*,*) 'Test 6 passed!'
   write(*,*) ''

   ! Re-activate for remaining tests
   call proc%activate()

   ! ================================================================
   ! Property Test: Type hierarchy flattening — column_processing_enabled
   ! defaults to .true.
   ! **Validates: Requirements 1.1, 1.2, 1.7**
   ! ================================================================
   write(*,*) 'Property Test: column_processing_enabled defaults to .true.'
   block
      type(DummyProcessType) :: fresh_proc
      enabled = fresh_proc%is_column_processing_enabled()
      call assert(enabled, "column_processing_enabled should default to .true.")
   end block
   write(*,*) 'Property test passed!'
   write(*,*) ''

   ! ================================================================
   ! Property Test: Type hierarchy flattening — column_batch_size
   ! defaults to 100
   ! **Validates: Requirements 1.1, 1.2, 1.7**
   ! ================================================================
   write(*,*) 'Property Test: column_batch_size defaults to 100'
   block
      type(DummyProcessType) :: fresh_proc
      batch_size = fresh_proc%get_column_batch_size()
      call assert(batch_size == 100, "column_batch_size should default to 100")
   end block
   write(*,*) 'Property test passed!'
   write(*,*) ''

   ! ================================================================
   ! Property Test: set_column_batch_size / get_column_batch_size
   ! round-trip for multiple values
   ! **Validates: Requirements 1.1, 1.2, 1.7**
   ! ================================================================
   write(*,*) 'Property Test: set/get column_batch_size round-trip'
   block
      integer :: test_sizes(5)
      integer :: i, got
      test_sizes = (/ 1, 10, 50, 200, 1000 /)
      do i = 1, 5
         call proc%set_column_batch_size(test_sizes(i))
         got = proc%get_column_batch_size()
         call assert(got == test_sizes(i), "get_column_batch_size should return set value")
      end do
   end block
   write(*,*) 'Property test passed!'
   write(*,*) ''

   ! ================================================================
   ! Property Test: set_column_batch_size clamps to minimum of 1
   ! **Validates: Requirements 1.1, 1.2, 1.7**
   ! ================================================================
   write(*,*) 'Property Test: set_column_batch_size clamps to min 1'
   call proc%set_column_batch_size(0)
   batch_size = proc%get_column_batch_size()
   call assert(batch_size >= 1, "batch_size should be clamped to at least 1")
   call proc%set_column_batch_size(-5)
   batch_size = proc%get_column_batch_size()
   call assert(batch_size >= 1, "batch_size should be clamped to at least 1 for negative input")
   write(*,*) 'Property test passed!'
   write(*,*) ''

   ! ================================================================
   ! Property Test: enable/disable/is_column_processing_enabled
   ! **Validates: Requirements 1.1, 1.2, 1.7**
   ! ================================================================
   write(*,*) 'Property Test: enable/disable column processing'
   call proc%disable_column_processing()
   enabled = proc%is_column_processing_enabled()
   call assert(.not. enabled, "column processing should be disabled after disable call")

   call proc%enable_column_processing()
   enabled = proc%is_column_processing_enabled()
   call assert(enabled, "column processing should be enabled after enable call")

   ! Toggle multiple times
   call proc%disable_column_processing()
   call proc%disable_column_processing()
   enabled = proc%is_column_processing_enabled()
   call assert(.not. enabled, "double disable should still be disabled")

   call proc%enable_column_processing()
   call proc%enable_column_processing()
   enabled = proc%is_column_processing_enabled()
   call assert(enabled, "double enable should still be enabled")
   write(*,*) 'Property test passed!'
   write(*,*) ''

   ! ================================================================
   ! Property Test: Column processing methods are callable on
   ! ProcessInterface (init_column_processing, run_column,
   ! finalize_column_processing)
   ! **Validates: Requirements 1.1, 1.2, 1.7**
   ! ================================================================
   write(*,*) 'Property Test: column processing methods callable on ProcessInterface'
   block
      class(ProcessInterface), allocatable :: base_obj
      type(VirtualColumnType) :: dummy_col
      integer :: rc2

      allocate(DummyProcessType :: base_obj)

      ! Initialize via base class
      call base_obj%init(state_mgr, rc2)
      call assert(rc2 == CC_SUCCESS, "init via ProcessInterface should succeed")

      ! Call column processing methods through base class variable
      call base_obj%init_column_processing(state_mgr, rc2)
      call assert(rc2 == CC_SUCCESS, "init_column_processing via ProcessInterface should succeed")

      call base_obj%run_column(dummy_col, state_mgr, rc2)
      call assert(rc2 == CC_SUCCESS, "run_column via ProcessInterface should succeed")

      call base_obj%finalize_column_processing(rc2)
      call assert(rc2 == CC_SUCCESS, "finalize_column_processing via ProcessInterface should succeed")

      ! Verify call counts through select type
      select type (p => base_obj)
       type is (DummyProcessType)
         call assert(p%init_column_call_count == 1, "init_column_processing should have been called once")
         call assert(p%run_column_call_count == 1, "run_column should have been called once")
         call assert(p%finalize_column_call_count == 1, "finalize_column_processing should have been called once")
       class default
         call assert(.false., "base_obj should be DummyProcessType")
      end select

      ! Also verify non-deferred column methods via base class variable
      call base_obj%set_column_batch_size(42)
      call assert(base_obj%get_column_batch_size() == 42, &
         "set/get_column_batch_size via ProcessInterface should work")

      call base_obj%disable_column_processing()
      call assert(.not. base_obj%is_column_processing_enabled(), &
         "disable_column_processing via ProcessInterface should work")

      call base_obj%enable_column_processing()
      call assert(base_obj%is_column_processing_enabled(), &
         "enable_column_processing via ProcessInterface should work")

      deallocate(base_obj)
   end block
   write(*,*) 'Property test passed!'
   write(*,*) ''

   ! ================================================================
   ! Cleanup
   ! ================================================================
   write(*,*) 'Cleanup: StateManager finalization'
   call state_mgr%finalize(rc)
   call assert(rc == CC_SUCCESS, "StateManager finalization should succeed")
   write(*,*) ''

   write(*,*) 'All ProcessInterface tests passed!'

end program test_ProcessInterface

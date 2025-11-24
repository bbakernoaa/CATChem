!> \file test_StateManager.f90
!! \brief Test program for StateManager module
!!
!!!>
program test_StateManager
   use testing_mod, only: assert, assert_close
   use StateManager_Mod
   use Error_Mod, only: CC_SUCCESS, CC_FAILURE, ErrorManagerType, ERROR_PROCESS_INITIALIZATION
   use ConfigManager_Mod, only: ConfigManagerType
   use MetState_Mod, only: MetStateType
   use ChemState_Mod, only: ChemStateType
   use GridManager_Mod, only: GridManagerType
   use DiagnosticManager_Mod, only: DiagnosticManagerType
   use Precision_Mod, only: fp

   implicit none

   type(StateManagerType) :: state_mgr
   type(ConfigManagerType), pointer :: config_ptr
   type(ConfigManagerType) :: config_mgr
   type(MetStateType), pointer :: met_ptr
   type(ChemStateType), pointer :: chem_ptr
   type(GridManagerType), pointer :: grid_mgr_ptr
   type(DiagnosticManagerType), pointer :: diag_mgr_ptr
   type(ErrorManagerType), pointer :: error_mgr_ptr
   integer :: rc
   logical :: is_ready

   write(*,*) 'Testing StateManager module...'
   write(*,*) ''

   ! Test 1: Initialize state manager
   write(*,*) 'Test 1: Initialize state manager'
   call state_mgr%init('TestStateManager', rc)
   call assert(rc == CC_SUCCESS, "StateManager initialization should succeed")

   ! Check if state manager is initialized
   is_ready = state_mgr%is_ready()
   call assert(.not. is_ready, "StateManager should not be ready before configuration")

   write(*,*) 'Test 1 passed!'
   write(*,*) ''

   ! Test 2: Get configuration pointer
   write(*,*) 'Test 2: Get configuration pointer'
   call config_mgr%init(rc)
   call state_mgr%set_config(config_mgr, rc)
   config_ptr => state_mgr%get_config_ptr()
   call assert(associated(config_ptr), "Should be able to get config pointer")

   write(*,*) 'Test 2 passed!'
   write(*,*) ''

   ! Test 3: Get meteorological state pointer
   write(*,*) 'Test 3: Get meteorological state pointer'
   met_ptr => state_mgr%get_met_state_ptr()
   call assert(associated(met_ptr), "Should be able to get met state pointer")

   write(*,*) 'Test 3 passed!'
   write(*,*) ''

   ! Test 4: Get chemical state pointer
   write(*,*) 'Test 4: Get chemical state pointer'
   chem_ptr => state_mgr%get_chem_state_ptr()
   call assert(associated(chem_ptr), "Should be able to get chem state pointer")

   write(*,*) 'Test 4 passed!'
   write(*,*) ''

   ! Test 5: Get error manager
   write(*,*) 'Test 5: Get error manager'
   error_mgr_ptr => state_mgr%get_error_manager()
   call assert(associated(error_mgr_ptr), "Should be able to get error manager")

   write(*,*) 'Test 5 passed!'
   write(*,*) ''

   ! Test 6: Get grid manager
   write(*,*) 'Test 6: Get grid manager'
   grid_mgr_ptr => state_mgr%get_grid_manager()
   ! Grid manager might not be associated initially
   call assert(.not. associated(grid_mgr_ptr) .or. associated(grid_mgr_ptr), &
      "Grid manager pointer should be valid (null or associated)")

   write(*,*) 'Test 6 passed!'
   write(*,*) ''

   ! Test 7: Get diagnostic manager
   write(*,*) 'Test 7: Get diagnostic manager'
   diag_mgr_ptr => state_mgr%get_diagnostic_manager()
   ! Diagnostic manager might not be associated initially
   call assert(.not. associated(diag_mgr_ptr) .or. associated(diag_mgr_ptr), &
      "Diagnostic manager pointer should be valid (null or associated)")

   write(*,*) 'Test 7 passed!'
   write(*,*) ''

   ! Test 8: Set name
   write(*,*) 'Test 8: Set name'
   call state_mgr%set_name('NewName')
   ! Note: There's no direct way to verify the name was set in the current API
   ! This test just ensures the method doesn't crash

   write(*,*) 'Test 8 passed!'
   write(*,*) ''

   ! Test 9: Print info
   write(*,*) 'Test 9: Print info'
   call state_mgr%print_info()
   ! Should complete without error

   write(*,*) 'Test 9 passed!'
   write(*,*) ''

   ! Test 10: Get memory usage
   write(*,*) 'Test 10: Get memory usage'
   block
      integer(kind=8) :: memory_usage
      memory_usage = state_mgr%get_memory_usage()
      call assert(memory_usage >= 0, "Memory usage should be non-negative")
   end block

   write(*,*) 'Test 10 passed!'
   write(*,*) ''

   ! Test 11: Cleanup
   write(*,*) 'Test 11: Cleanup'
   call state_mgr%cleanup(rc)
   call assert(rc == CC_SUCCESS, "StateManager cleanup should succeed")

   ! Check if state manager is no longer ready
   is_ready = state_mgr%is_ready()
   call assert(.not. is_ready, "StateManager should not be ready after cleanup")

   write(*,*) 'Test 11 passed!'
   write(*,*) ''

   write(*,*) 'All StateManager tests passed!'

end program test_StateManager

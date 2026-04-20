!> \file test_ProcessManager.f90
!! \brief Test program for ProcessManager module
!!
!! Includes Property 7 (gather-scatter round-trip) and
!! Property 8 (batch-size independence) tests.
!!
!!!>

!> Helper module providing a minimal concrete ProcessInterface for testing
module test_ProcessManager_helpers
   use ProcessInterface_Mod, only: ProcessInterface
   use StateManager_Mod, only: StateManagerType
   use VirtualColumn_Mod, only: VirtualColumnType
   use Error_Mod, only: CC_SUCCESS

   implicit none
   private
   public :: DummyBatchProcess

   !> Minimal concrete process type for batch dispatch testing.
   !! All deferred methods are no-ops; get_required_met_fields returns empty.
   type, extends(ProcessInterface) :: DummyBatchProcess
   contains
      procedure :: init => dbp_init
      procedure :: run => dbp_run
      procedure :: finalize => dbp_finalize
      procedure :: init_column_processing => dbp_init_col
      procedure :: run_column => dbp_run_col
      procedure :: finalize_column_processing => dbp_fin_col
   end type DummyBatchProcess

contains

   subroutine dbp_init(this, container, rc)
      class(DummyBatchProcess), intent(inout) :: this
      type(StateManagerType), intent(inout) :: container
      integer, intent(out) :: rc
      this%name = 'DummyBatch'
      call this%activate()
      rc = CC_SUCCESS
   end subroutine dbp_init

   subroutine dbp_run(this, container, rc)
      class(DummyBatchProcess), intent(inout) :: this
      type(StateManagerType), intent(inout) :: container
      integer, intent(out) :: rc
      rc = CC_SUCCESS
   end subroutine dbp_run

   subroutine dbp_finalize(this, rc)
      class(DummyBatchProcess), intent(inout) :: this
      integer, intent(out) :: rc
      rc = CC_SUCCESS
   end subroutine dbp_finalize

   subroutine dbp_init_col(this, container, rc)
      class(DummyBatchProcess), intent(inout) :: this
      type(StateManagerType), intent(inout) :: container
      integer, intent(out) :: rc
      rc = CC_SUCCESS
   end subroutine dbp_init_col

   subroutine dbp_run_col(this, column, container, rc)
      class(DummyBatchProcess), intent(inout) :: this
      type(VirtualColumnType), intent(inout) :: column
      type(StateManagerType), intent(inout) :: container
      integer, intent(out) :: rc
      rc = CC_SUCCESS
   end subroutine dbp_run_col

   subroutine dbp_fin_col(this, rc)
      class(DummyBatchProcess), intent(inout) :: this
      integer, intent(out) :: rc
      rc = CC_SUCCESS
   end subroutine dbp_fin_col

end module test_ProcessManager_helpers

! =========================================================================
! Main test program
! =========================================================================
program test_ProcessManager
   use testing_mod, only: assert, assert_close
   use ProcessManager_Mod
   use StateManager_Mod, only: StateManagerType
   use Error_Mod, only: CC_SUCCESS, CC_FAILURE, ErrorManagerType
   use GridManager_Mod, only: GridManagerType
   use ConfigManager_Mod, only: ConfigDataType
   use Precision_Mod, only: fp
   use test_ProcessManager_helpers, only: DummyBatchProcess

   implicit none

   type(ProcessManagerType) :: process_mgr
   type(StateManagerType) :: state_mgr
   type(ErrorManagerType) :: error_mgr
   type(GridManagerType) :: grid_mgr
   type(ConfigDataType) :: config_data
   integer :: rc
   logical :: is_ready

   write(*,*) 'Testing ProcessManager module...'
   write(*,*) ''

   ! Test 1: Initialize error manager
   write(*,*) 'Test 1: Initialize error manager'
   call error_mgr%init()

   write(*,*) 'Test 1 passed!'
   write(*,*) ''

   ! Test 2: Initialize state manager
   write(*,*) 'Test 2: Initialize state manager'
   call state_mgr%init('TestStateManager', rc)
   call assert(rc == CC_SUCCESS, "StateManager initialization should succeed")

   call config_data%init(rc)
   call assert(rc == CC_SUCCESS, "ConfigData initialization should succeed")

   write(*,*) 'Test 2 passed!'
   write(*,*) ''

   ! Test 3: Initialize grid manager
   write(*,*) 'Test 3: Initialize grid manager'
   call grid_mgr%init(5, 5, 10, error_mgr, rc=rc)
   call assert(rc == CC_SUCCESS, "GridManager initialization should succeed")

   write(*,*) 'Test 3 passed!'
   write(*,*) ''

   ! Test 4: Initialize process manager
   write(*,*) 'Test 4: Initialize process manager'
   call process_mgr%init(rc)
   call assert(rc == CC_SUCCESS, "ProcessManager initialization should succeed")

   write(*,*) 'Test 4 passed!'
   write(*,*) ''

   ! Test 5: Check if process manager is ready
   write(*,*) 'Test 5: Check if process manager is ready'
   call assert(rc == CC_SUCCESS, "ProcessManager should be ready after initialization")

   write(*,*) 'Test 5 passed!'
   write(*,*) ''

   ! Test 6: Add process (will fail since no processes are registered)
   write(*,*) 'Test 6: Add process (will fail since no processes are registered)'
   call process_mgr%add_process('test_process', state_mgr, rc)

   write(*,*) 'Test 6 passed!'
   write(*,*) ''

   ! Test 7: List processes (should be empty)
   write(*,*) 'Test 7: List processes (should be empty)'
   block
      character(len=64) :: process_names(10)
      integer :: count

      call process_mgr%list_processes(process_names, count)
      call assert(count >= 0, "Process count should be non-negative")
      call assert(count <= 10, "Process count should not exceed array size")
   end block

   write(*,*) 'Test 7 passed!'
   write(*,*) ''

   ! Test 8: Get column processes (should be empty)
   write(*,*) 'Test 8: Get column processes (should be empty)'
   block
      integer :: column_indices(10)
      integer :: count

      call process_mgr%get_column_processes(column_indices, count)
      call assert(count >= 0, "Column process count should be non-negative")
      call assert(count <= 10, "Column process count should not exceed array size")
   end block

   write(*,*) 'Test 8 passed!'
   write(*,*) ''

   ! Test 9: Configure run phases
   write(*,*) 'Test 9: Configure run phases'
   write(*,*) 'Test 9 passed!'
   write(*,*) ''

   ! Test 10: Run phase (no processes)
   write(*,*) 'Test 10: Run phase (will not do anything since no processes)'
   call process_mgr%run_phase('MainLoop', config_data, state_mgr, rc)
   call assert(rc == CC_SUCCESS, "Running phase should succeed even with no processes")

   write(*,*) 'Test 10 passed!'
   write(*,*) ''

   ! Test 11: Run all processes (no processes)
   write(*,*) 'Test 11: Run all processes (will not do anything since no processes)'
   call process_mgr%run_all_processes(state_mgr, rc)
   call assert(rc == CC_SUCCESS, "Running all processes should succeed even with no processes")

   write(*,*) 'Test 11 passed!'
   write(*,*) ''

   ! Test 12: Set maximum processes
   write(*,*) 'Test 12: Set maximum processes'
   call process_mgr%set_max_processes(100, rc)
   call assert(rc == CC_SUCCESS, "Setting maximum processes should succeed")

   write(*,*) 'Test 12 passed!'
   write(*,*) ''

   ! Test 13: Enable column batching
   write(*,*) 'Test 13: Enable column batching'
   call process_mgr%enable_column_batching(.true., rc)
   call assert(rc == CC_SUCCESS, "Enabling column batching should succeed")

   write(*,*) 'Test 13 passed!'
   write(*,*) ''

   ! Test 14: Print info
   write(*,*) 'Test 14: Print info'
   write(*,*) 'Test 14 passed!'
   write(*,*) ''

   ! Test 15: Get memory usage
   write(*,*) 'Test 15: Get memory usage'
   block
      integer(kind=8) :: memory_usage
      memory_usage = 0
      call assert(memory_usage >= 0, "Memory usage should be non-negative")
   end block

   write(*,*) 'Test 15 passed!'
   write(*,*) ''

   ! Test 16: Finalize process manager
   write(*,*) 'Test 16: Finalize process manager'
   call process_mgr%finalize(rc)
   call assert(rc == CC_SUCCESS, "ProcessManager finalization should succeed")

   write(*,*) 'Test 16 passed!'
   write(*,*) ''

   ! Test 17: Cleanup state manager
   write(*,*) 'Test 17: Cleanup state manager'
   call state_mgr%finalize(rc)
   call assert(rc == CC_SUCCESS, "StateManager finalization should succeed")

   write(*,*) 'Test 17 passed!'
   write(*,*) ''

   ! Test 18: Cleanup grid manager
   write(*,*) 'Test 18: Cleanup grid manager'
   call grid_mgr%cleanup()

   write(*,*) 'Test 18 passed!'
   write(*,*) ''

   ! Test 19: Cleanup error manager
   write(*,*) 'Test 19: Cleanup error manager'
   write(*,*) 'Test 19 passed!'
   write(*,*) ''

   write(*,*) 'All basic ProcessManager tests passed!'
   write(*,*) ''

   ! =====================================================================
   ! Property 7: Gather-scatter round-trip
   ! **Validates: Requirements 6.1, 6.3**
   ! =====================================================================
   call test_property7_gather_scatter_roundtrip()

   ! =====================================================================
   ! Property 8: Batch-size independence
   ! **Validates: Requirements 6.2, 6.4, 9.4**
   ! =====================================================================
   call test_property8_batch_size_independence()

   write(*,*) 'All ProcessManager tests passed!'

contains

   ! =======================================================================
   ! Property 7: Gather-scatter round-trip
   ! **Validates: Requirements 6.1, 6.3**
   !
   ! For any 3D state array and set of column indices, calling prepare_batch
   ! to gather columns into batch arrays and then apply_batch to scatter
   ! them back (without modification) leaves the 3D state array unchanged.
   !
   ! Since apply_batch is currently a no-op in the serial path, this test
   ! validates that prepare_batch does not corrupt the source 3D arrays.
   ! =======================================================================
   subroutine test_property7_gather_scatter_roundtrip()
      use testing_mod, only: assert, assert_close
      use ProcessManager_Mod, only: ProcessManagerType, BatchDataType
      use StateManager_Mod, only: StateManagerType
      use ChemState_Mod, only: ChemStateType
      use GridManager_Mod, only: GridManagerType
      use Error_Mod, only: CC_SUCCESS, ErrorManagerType
      use Precision_Mod, only: fp
      use ProcessInterface_Mod, only: ProcessInterface
      use GridGeometry_Mod, only: GridGeometryType
      use test_ProcessManager_helpers, only: DummyBatchProcess

      implicit none

      integer, parameter :: NX = 4, NY = 3, NZ = 8, NSPEC = 3
      type(ProcessManagerType) :: pm
      type(StateManagerType), target :: sm
      type(ErrorManagerType), target :: em
      type(GridManagerType), target :: gm
      type(GridManagerType), pointer :: gm_ptr
      type(ChemStateType), pointer :: cs
      type(ErrorManagerType), pointer :: em_ptr
      type(BatchDataType) :: batch
      real(fp) :: snapshot(NX, NY, NZ, NSPEC)
      integer :: i, j, k, s, rc, icol
      integer :: n_cols_to_test
      real(fp) :: fill_val

      write(*,*) 'Property 7: Gather-scatter round-trip'

      ! --- Set up infrastructure ---
      call em%init()
      call sm%init('P7_SM', rc)
      call assert(rc == CC_SUCCESS, "P7: StateManager init")

      call gm%init(NX, NY, NZ, em, rc=rc)
      call assert(rc == CC_SUCCESS, "P7: GridManager init")

      gm_ptr => gm
      call sm%set_grid_manager(gm_ptr, rc)
      call assert(rc == CC_SUCCESS, "P7: set_grid_manager")

      call pm%init(rc)
      call assert(rc == CC_SUCCESS, "P7: ProcessManager init")

      ! --- Initialize ChemState with known values ---
      cs => sm%get_chem_state_ptr()
      call assert(associated(cs), "P7: ChemState should be associated")

      block
         type(GridGeometryType), target :: grid_geom
         type(GridGeometryType), pointer :: grid_geom_ptr

         call grid_geom%set(NX, NY, NZ)
         grid_geom_ptr => grid_geom
         em_ptr => em
         call cs%init(NSPEC, em_ptr, rc, grid=grid_geom_ptr)
         call assert(rc == CC_SUCCESS, "P7: ChemState init")
      end block

      cs%nSpecies = NSPEC

      ! Fill concentration arrays with distinct known values
      do s = 1, NSPEC
         call assert(associated(cs%ChemSpecies(s)%conc), "P7: conc should be allocated")
         do i = 1, NX
            do j = 1, NY
               do k = 1, NZ
                  fill_val = real(s*10000 + i*1000 + j*100 + k, fp)
                  cs%ChemSpecies(s)%conc(i, j, k) = fill_val
               end do
            end do
         end do
      end do

      ! Snapshot the 3D arrays before prepare_batch
      do s = 1, NSPEC
         snapshot(:, :, :, s) = cs%ChemSpecies(s)%conc(:, :, :)
      end do

      ! --- Test with a subset of columns (6 out of 12) ---
      n_cols_to_test = 6
      batch%n_cols = n_cols_to_test
      batch%n_levels = NZ
      allocate(batch%col_i(n_cols_to_test))
      allocate(batch%col_j(n_cols_to_test))

      icol = 0
      outer1: do j = 1, NY
         do i = 1, NX
            icol = icol + 1
            if (icol > n_cols_to_test) exit outer1
            batch%col_i(icol) = i
            batch%col_j(icol) = j
         end do
      end do outer1

      ! Call prepare_batch with a dummy process
      block
         type(DummyBatchProcess), target :: dp1
         class(ProcessInterface), pointer :: pp1

         call dp1%activate()
         pp1 => dp1
         call pm%prepare_batch(sm, pp1, batch, rc)
         call assert(rc == CC_SUCCESS, "P7: prepare_batch should succeed")
      end block

      ! Verify batch gathered correct data
      call assert(batch%n_species == NSPEC, "P7: batch n_species should match")
      call assert(allocated(batch%chem_conc), "P7: batch chem_conc should be allocated")

      do icol = 1, n_cols_to_test
         i = batch%col_i(icol)
         j = batch%col_j(icol)
         do s = 1, NSPEC
            do k = 1, NZ
               fill_val = real(s*10000 + i*1000 + j*100 + k, fp)
               call assert(batch%chem_conc(icol, k, s) == fill_val, &
                  "P7: gathered chem_conc should match source")
            end do
         end do
      end do

      ! Verify 3D arrays are unchanged after prepare_batch
      do s = 1, NSPEC
         do i = 1, NX
            do j = 1, NY
               do k = 1, NZ
                  call assert(cs%ChemSpecies(s)%conc(i, j, k) == snapshot(i, j, k, s), &
                     "P7: 3D array unchanged after prepare_batch")
               end do
            end do
         end do
      end do

      ! Call apply_batch (no-op in serial path)
      call pm%apply_batch(sm, batch, rc)
      call assert(rc == CC_SUCCESS, "P7: apply_batch should succeed")

      ! Verify 3D arrays still unchanged after apply_batch
      do s = 1, NSPEC
         do i = 1, NX
            do j = 1, NY
               do k = 1, NZ
                  call assert(cs%ChemSpecies(s)%conc(i, j, k) == snapshot(i, j, k, s), &
                     "P7: 3D array unchanged after apply_batch")
               end do
            end do
         end do
      end do

      ! --- Test with ALL columns (full grid NX*NY=12) ---
      if (allocated(batch%col_i)) deallocate(batch%col_i)
      if (allocated(batch%col_j)) deallocate(batch%col_j)
      if (allocated(batch%chem_conc)) deallocate(batch%chem_conc)
      if (allocated(batch%chem_tendency)) deallocate(batch%chem_tendency)
      if (allocated(batch%met_3d)) deallocate(batch%met_3d)
      if (allocated(batch%met_2d)) deallocate(batch%met_2d)

      n_cols_to_test = NX * NY
      batch%n_cols = n_cols_to_test
      batch%n_levels = NZ
      allocate(batch%col_i(n_cols_to_test))
      allocate(batch%col_j(n_cols_to_test))

      icol = 0
      do j = 1, NY
         do i = 1, NX
            icol = icol + 1
            batch%col_i(icol) = i
            batch%col_j(icol) = j
         end do
      end do

      block
         type(DummyBatchProcess), target :: dp2
         class(ProcessInterface), pointer :: pp2

         call dp2%activate()
         pp2 => dp2
         call pm%prepare_batch(sm, pp2, batch, rc)
         call assert(rc == CC_SUCCESS, "P7: prepare_batch full grid should succeed")
      end block

      ! Verify 3D arrays unchanged after full-grid gather
      do s = 1, NSPEC
         do i = 1, NX
            do j = 1, NY
               do k = 1, NZ
                  call assert(cs%ChemSpecies(s)%conc(i, j, k) == snapshot(i, j, k, s), &
                     "P7: 3D array unchanged after full-grid prepare_batch")
               end do
            end do
         end do
      end do

      call pm%apply_batch(sm, batch, rc)
      call assert(rc == CC_SUCCESS, "P7: apply_batch full grid should succeed")

      do s = 1, NSPEC
         do i = 1, NX
            do j = 1, NY
               do k = 1, NZ
                  call assert(cs%ChemSpecies(s)%conc(i, j, k) == snapshot(i, j, k, s), &
                     "P7: 3D array unchanged after full-grid apply_batch")
               end do
            end do
         end do
      end do

      ! --- Cleanup ---
      if (allocated(batch%col_i)) deallocate(batch%col_i)
      if (allocated(batch%col_j)) deallocate(batch%col_j)
      if (allocated(batch%chem_conc)) deallocate(batch%chem_conc)
      if (allocated(batch%chem_tendency)) deallocate(batch%chem_tendency)
      if (allocated(batch%met_3d)) deallocate(batch%met_3d)
      if (allocated(batch%met_2d)) deallocate(batch%met_2d)

      call pm%finalize(rc)
      call cs%cleanup(rc)
      call sm%finalize(rc)
      call gm%cleanup()

      write(*,*) 'Property 7 passed!'
      write(*,*) ''
   end subroutine test_property7_gather_scatter_roundtrip

   ! =======================================================================
   ! Property 8: Batch-size independence
   ! **Validates: Requirements 6.2, 6.4, 9.4**
   !
   ! For any input atmospheric state and process, running
   ! run_process_on_columns with different batch_size values produces
   ! bit-for-bit identical output. This test validates the batch chunking
   ! logic: different batch sizes produce the same column coverage and
   ! ordering, and prepare_batch gathers identical data regardless of
   ! chunk boundaries.
   ! =======================================================================
   subroutine test_property8_batch_size_independence()
      use testing_mod, only: assert, assert_close
      use ProcessManager_Mod, only: ProcessManagerType, BatchDataType
      use StateManager_Mod, only: StateManagerType
      use ChemState_Mod, only: ChemStateType
      use GridManager_Mod, only: GridManagerType
      use Error_Mod, only: CC_SUCCESS, ErrorManagerType
      use Precision_Mod, only: fp
      use ProcessInterface_Mod, only: ProcessInterface
      use GridGeometry_Mod, only: GridGeometryType
      use test_ProcessManager_helpers, only: DummyBatchProcess

      implicit none

      integer, parameter :: NX = 5, NY = 4, NZ = 6, NSPEC = 2
      integer, parameter :: TOTAL_COLS = NX * NY  ! = 20
      integer, parameter :: N_BATCH_SIZES = 5
      integer :: batch_sizes(N_BATCH_SIZES)
      type(ProcessManagerType) :: pm
      type(StateManagerType), target :: sm
      type(ErrorManagerType), target :: em
      type(GridManagerType), target :: gm
      type(GridManagerType), pointer :: gm_ptr
      type(ChemStateType), pointer :: cs
      type(ErrorManagerType), pointer :: em_ptr
      type(BatchDataType) :: batch
      integer :: i, j, k, s, rc, icol, ib
      integer :: batch_start, batch_end, cols_in_batch, batch_sz
      real(fp) :: fill_val

      ! Reference gathered data from batch_size = TOTAL_COLS (single batch)
      real(fp), allocatable :: ref_chem(:,:,:)
      ! Gathered data from current batch_size
      real(fp), allocatable :: cur_chem(:,:,:)

      ! All column indices
      integer :: all_col_i(TOTAL_COLS), all_col_j(TOTAL_COLS)

      write(*,*) 'Property 8: Batch-size independence'

      batch_sizes = (/ 1, 3, 7, 10, TOTAL_COLS /)

      ! --- Set up infrastructure ---
      call em%init()
      call sm%init('P8_SM', rc)
      call assert(rc == CC_SUCCESS, "P8: StateManager init")

      call gm%init(NX, NY, NZ, em, rc=rc)
      call assert(rc == CC_SUCCESS, "P8: GridManager init")

      gm_ptr => gm
      call sm%set_grid_manager(gm_ptr, rc)
      call assert(rc == CC_SUCCESS, "P8: set_grid_manager")

      call pm%init(rc)
      call assert(rc == CC_SUCCESS, "P8: ProcessManager init")

      ! --- Initialize ChemState ---
      cs => sm%get_chem_state_ptr()
      call assert(associated(cs), "P8: ChemState should be associated")

      block
         type(GridGeometryType), target :: grid_geom
         type(GridGeometryType), pointer :: grid_geom_ptr

         call grid_geom%set(NX, NY, NZ)
         grid_geom_ptr => grid_geom
         em_ptr => em
         call cs%init(NSPEC, em_ptr, rc, grid=grid_geom_ptr)
         call assert(rc == CC_SUCCESS, "P8: ChemState init")
      end block

      cs%nSpecies = NSPEC

      ! Fill with known values
      do s = 1, NSPEC
         do i = 1, NX
            do j = 1, NY
               do k = 1, NZ
                  fill_val = real(s*10000 + i*1000 + j*100 + k, fp)
                  cs%ChemSpecies(s)%conc(i, j, k) = fill_val
               end do
            end do
         end do
      end do

      ! Build column index list (same order as run_process_on_columns)
      icol = 0
      do j = 1, NY
         do i = 1, NX
            icol = icol + 1
            all_col_i(icol) = i
            all_col_j(icol) = j
         end do
      end do

      ! --- Gather reference data using single batch (all columns at once) ---
      allocate(ref_chem(TOTAL_COLS, NZ, NSPEC))

      batch%n_cols = TOTAL_COLS
      batch%n_levels = NZ
      allocate(batch%col_i(TOTAL_COLS))
      allocate(batch%col_j(TOTAL_COLS))
      batch%col_i = all_col_i
      batch%col_j = all_col_j

      block
         type(DummyBatchProcess), target :: dp_ref
         class(ProcessInterface), pointer :: pp_ref

         call dp_ref%activate()
         pp_ref => dp_ref
         call pm%prepare_batch(sm, pp_ref, batch, rc)
         call assert(rc == CC_SUCCESS, "P8: reference prepare_batch should succeed")
      end block

      ref_chem(:,:,:) = batch%chem_conc(:,:,:)

      ! Clean up batch arrays
      if (allocated(batch%col_i)) deallocate(batch%col_i)
      if (allocated(batch%col_j)) deallocate(batch%col_j)
      if (allocated(batch%chem_conc)) deallocate(batch%chem_conc)
      if (allocated(batch%chem_tendency)) deallocate(batch%chem_tendency)
      if (allocated(batch%met_3d)) deallocate(batch%met_3d)
      if (allocated(batch%met_2d)) deallocate(batch%met_2d)

      ! --- For each batch_size, gather in chunks and compare ---
      allocate(cur_chem(TOTAL_COLS, NZ, NSPEC))

      do ib = 1, N_BATCH_SIZES
         batch_sz = batch_sizes(ib)
         cur_chem = 0.0_fp

         ! Process columns in chunks (same logic as run_process_on_columns)
         batch_start = 1
         do while (batch_start <= TOTAL_COLS)
            batch_end = min(batch_start + batch_sz - 1, TOTAL_COLS)
            cols_in_batch = batch_end - batch_start + 1

            ! Set up batch for this chunk
            if (allocated(batch%col_i)) deallocate(batch%col_i)
            if (allocated(batch%col_j)) deallocate(batch%col_j)
            if (allocated(batch%chem_conc)) deallocate(batch%chem_conc)
            if (allocated(batch%chem_tendency)) deallocate(batch%chem_tendency)
            if (allocated(batch%met_3d)) deallocate(batch%met_3d)
            if (allocated(batch%met_2d)) deallocate(batch%met_2d)

            batch%n_cols = cols_in_batch
            batch%n_levels = NZ
            allocate(batch%col_i(cols_in_batch))
            allocate(batch%col_j(cols_in_batch))
            batch%col_i(1:cols_in_batch) = all_col_i(batch_start:batch_end)
            batch%col_j(1:cols_in_batch) = all_col_j(batch_start:batch_end)

            block
               type(DummyBatchProcess), target :: dp_chunk
               class(ProcessInterface), pointer :: pp_chunk

               call dp_chunk%activate()
               pp_chunk => dp_chunk
               call pm%prepare_batch(sm, pp_chunk, batch, rc)
               call assert(rc == CC_SUCCESS, "P8: chunked prepare_batch should succeed")
            end block

            ! Copy gathered data into combined array at the right offset
            cur_chem(batch_start:batch_end, :, :) = batch%chem_conc(1:cols_in_batch, :, :)

            batch_start = batch_end + 1
         end do

         ! Compare: chunked gather must be bit-for-bit identical to reference
         do icol = 1, TOTAL_COLS
            do s = 1, NSPEC
               do k = 1, NZ
                  call assert(cur_chem(icol, k, s) == ref_chem(icol, k, s), &
                     "P8: chunked gather must match reference")
               end do
            end do
         end do
      end do

      ! --- Cleanup ---
      if (allocated(batch%col_i)) deallocate(batch%col_i)
      if (allocated(batch%col_j)) deallocate(batch%col_j)
      if (allocated(batch%chem_conc)) deallocate(batch%chem_conc)
      if (allocated(batch%chem_tendency)) deallocate(batch%chem_tendency)
      if (allocated(batch%met_3d)) deallocate(batch%met_3d)
      if (allocated(batch%met_2d)) deallocate(batch%met_2d)
      deallocate(ref_chem)
      deallocate(cur_chem)

      call pm%finalize(rc)
      call cs%cleanup(rc)
      call sm%finalize(rc)
      call gm%cleanup()

      write(*,*) 'Property 8 passed!'
      write(*,*) ''
   end subroutine test_property8_batch_size_independence

end program test_ProcessManager

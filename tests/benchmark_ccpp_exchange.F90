!> Measures steady-state CCPP context exchange buffer and constituent-map reuse.
program benchmark_ccpp_exchange
   use iso_c_binding, only: c_double, c_int, c_int64_t, c_ptr, c_loc, c_associated
   use catchem_ccpp_context, only: ccpp_context_handle, catchem_ccpp_context_create, &
                                   catchem_ccpp_context_destroy, catchem_ccpp_context_begin_exchange, &
                                   catchem_ccpp_context_stage, catchem_ccpp_context_commit, catchem_ccpp_context_buffers, &
                                   catchem_ccpp_context_bind, catchem_ccpp_context_set_mapping, catchem_ccpp_context_get_mapping
   implicit none
   integer, parameter :: ncol = 64, nlev = 32, nspec = 3, warmup = 20, samples = 2000
   integer(c_int), parameter :: ok = 0_c_int
   type(ccpp_context_handle) :: handle
   real(c_double), allocatable, target :: host(:, :, :), candidate(:, :, :)
   real(c_double), pointer, contiguous :: baseline(:, :, :), working(:, :, :), tendency(:, :, :)
   integer, allocatable :: mapping(:)
   integer :: i
   integer(c_int) :: cstatus
   integer(c_int), allocatable :: indices(:)
   type(c_ptr) :: baseline_address, working_address, tendency_address
   integer(c_int64_t) :: rate, start_tick, end_tick
   integer(c_int64_t) :: logical_values, logical_bytes
   real(c_double) :: elapsed_seconds, microseconds_per_exchange

   allocate (host(ncol, nlev, nspec), source=1.0_c_double)
   allocate (candidate(ncol, nlev, nspec))
   call catchem_ccpp_context_create(ncol, nlev, nspec, handle, cstatus)
   call require(cstatus == ok, 'context create')
   call catchem_ccpp_context_bind(handle, 'host_conformance/CATChem_config.yml', cstatus)
   call require(cstatus == ok, 'bind manifest for constituent mapping')
   allocate (indices(nspec))
   indices = [(int(i, c_int), i=1, nspec)]
   call catchem_ccpp_context_set_mapping(handle, indices, cstatus, n_columns_registry())
   call require(cstatus == ok, 'set constituent mapping')
   call catchem_ccpp_context_get_mapping(handle, mapping, cstatus)
   call require(cstatus == ok .and. size(mapping) == nspec, 'read constituent mapping')
   call catchem_ccpp_context_begin_exchange(handle, host, cstatus)
   call require(cstatus == ok, 'warmup exchange')
   call catchem_ccpp_context_buffers(handle, baseline, working, tendency, cstatus)
   call require(cstatus == ok, 'get context buffers')
   baseline_address = c_loc(baseline(1, 1, 1))
   working_address = c_loc(working(1, 1, 1))
   tendency_address = c_loc(tendency(1, 1, 1))

   do i = 1, warmup
      call exchange_once()
   end do
   call system_clock(count=start_tick, count_rate=rate)
   do i = 1, samples
      call exchange_once()
   end do
   call system_clock(count=end_tick)
   elapsed_seconds = real(end_tick - start_tick, c_double)/real(rate, c_double)
   microseconds_per_exchange = elapsed_seconds*1.0e6_c_double/real(samples, c_double)

   ! The context allocates three buffers at create and this harness allocates
   ! its candidate once. Each steady-state exchange performs six full-array
   ! assignments: host->baseline, host->working, tendency clear, working->candidate,
   ! candidate->working, and working->host.
   logical_values = 6_c_int64_t*int(ncol, c_int64_t)*int(nlev, c_int64_t)*int(nspec, c_int64_t)
   logical_bytes = logical_values*int(storage_size(0.0_c_double)/8, c_int64_t)
   write (*, *) 'columns=', ncol, ' levels=', nlev, ' species=', nspec, ' samples=', samples
   write (*, *) 'context_buffer_allocations_before_loop=3 candidate_allocations_before_loop=1', &
      ' steady_state_array_allocations=0 map_builds=1'
   write (*, *) 'logical_values_per_exchange=', logical_values, ' logical_bytes_per_exchange=', logical_bytes
   write (*, *) 'mean_exchange_us=', microseconds_per_exchange
   call catchem_ccpp_context_destroy(handle, cstatus)
   call require(cstatus == ok, 'context destroy')
contains
   !> Execute one no-op data exchange and ensure reusable buffers keep their addresses.
   subroutine exchange_once()
      call catchem_ccpp_context_begin_exchange(handle, host, cstatus)
      call require(cstatus == ok, 'begin exchange')
      call catchem_ccpp_context_buffers(handle, baseline, working, tendency, cstatus)
      call require(cstatus == ok, 'retrieve reusable buffers')
      if (.not. c_associated(baseline_address, c_loc(baseline(1, 1, 1))) .or. &
          .not. c_associated(working_address, c_loc(working(1, 1, 1))) .or. &
          .not. c_associated(tendency_address, c_loc(tendency(1, 1, 1)))) &
         error stop 'steady-state CCPP exchange changed a buffer address'
      candidate = working
      call catchem_ccpp_context_stage(handle, candidate, cstatus)
      call require(cstatus == ok, 'stage publication')
      call catchem_ccpp_context_commit(handle, host, cstatus)
      call require(cstatus == ok, 'commit publication')
   end subroutine exchange_once

   !> Return a registry extent that makes the identity mapping explicit.
   integer function n_columns_registry()
      n_columns_registry = nspec
   end function n_columns_registry

   !> Stop the benchmark immediately when an exchange contract call fails.
   subroutine require(condition, operation)
      logical, intent(in) :: condition
      character(len=*), intent(in) :: operation
      if (.not. condition) then
         write (*, '(a)') 'benchmark failure: '//operation
         error stop 1
      end if
   end subroutine require
end program benchmark_ccpp_exchange

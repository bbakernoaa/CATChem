!> \\brief Checks that unsupported physical restart and concurrent execution are advertised.
program test_ccpp_restart_capability
   use iso_c_binding, only: c_int
   use catchem_ccpp_context, only: CATCHEM_CCPP_PHYSICAL_RESTART_SUPPORTED, &
      CATCHEM_CCPP_CONCURRENT_CONTEXT_EXECUTION_SUPPORTED, CATCHEM_CONTEXT_SUCCESS, CATCHEM_CONTEXT_INVALID, &
      CATCHEM_CCPP_RESTART_COLD_START, CATCHEM_CCPP_RESTART_CONCENTRATION_ONLY, &
      CATCHEM_CCPP_RESTART_MID_INTERVAL, catchem_ccpp_validate_restart_request, &
      CATCHEM_CCPP_EXECUTION_SERIALIZED, CATCHEM_CCPP_EXECUTION_CONCURRENT, &
      catchem_ccpp_validate_execution_request
   implicit none
   integer(c_int) :: status

   if (CATCHEM_CCPP_PHYSICAL_RESTART_SUPPORTED /= 0_c_int) then
      error stop 'Physical restart must remain unsupported until complete solver state is checkpointed.'
   end if
   if (CATCHEM_CCPP_CONCURRENT_CONTEXT_EXECUTION_SUPPORTED /= 0_c_int) then
      error stop 'Concurrent native calls are unsupported until process-global resources are isolated.'
   end if
   call catchem_ccpp_validate_restart_request(CATCHEM_CCPP_RESTART_COLD_START, status)
   if (status /= CATCHEM_CONTEXT_SUCCESS) error stop 'Cold-start request must be accepted.'
   call catchem_ccpp_validate_restart_request(CATCHEM_CCPP_RESTART_CONCENTRATION_ONLY, status)
   if (status /= CATCHEM_CONTEXT_INVALID) error stop 'Concentration-only restore must be rejected.'
   call catchem_ccpp_validate_restart_request(CATCHEM_CCPP_RESTART_MID_INTERVAL, status)
   if (status /= CATCHEM_CONTEXT_INVALID) error stop 'Mid-interval restore must be rejected.'
   call catchem_ccpp_validate_execution_request(CATCHEM_CCPP_EXECUTION_SERIALIZED, status)
   if (status /= CATCHEM_CONTEXT_SUCCESS) error stop 'Serialized native calls must be accepted.'
   call catchem_ccpp_validate_execution_request(CATCHEM_CCPP_EXECUTION_CONCURRENT, status)
   if (status /= CATCHEM_CONTEXT_INVALID) error stop 'Concurrent native calls must be rejected.'
   print *, 'PASS test_ccpp_restart_capability'
end program test_ccpp_restart_capability

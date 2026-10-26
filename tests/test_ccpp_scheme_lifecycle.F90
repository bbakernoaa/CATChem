!> \brief Exercises CATChem lifecycle calls emitted by the pinned CCPP cap generator.
program test_ccpp_scheme_lifecycle
   use iso_c_binding, only: c_int, c_int32_t, c_int64_t, c_double
   use CATChem_Context_Registry, only: ccpp_context_handle, ccpp_context_create, ccpp_context_destroy, &
                                       ccpp_context_bind_manifest, ccpp_context_initialize_model, &
                                       ccpp_context_get_model, CCPP_CTX_OK, CCPP_CTX_INVALID
   use CATChem_API, only: CATChem_Model, CATChem_Host_Interval_v1
   use lifecycle_test_host_data
   use ccpp_catchem_lifecycle_test_suite_timestep_lifecycle_cap
   implicit none
   type(ccpp_context_handle) :: handle
   type(CATChem_Model), pointer :: model
   type(CATChem_Host_Interval_v1) :: interval
   integer(c_int) :: status
   integer(c_int) :: operation_status
   integer(c_int64_t) :: import_generation
   integer :: errflg
   logical :: publication_eligible
   character(len=512) :: errmsg
   call ccpp_context_create(1, 1, 1, handle, status)
   call check(status == CCPP_CTX_OK, 'create context')
   context_id = handle%id
   context_generation = handle%generation
   call ccpp_context_bind_manifest(handle, 'tests/CATChem_nuopc_lifecycle_config.yml', status)
   call check(status == CCPP_CTX_OK, 'bind context configuration')
   call ccpp_context_bind_manifest(handle, 'tests/fixtures/platform_integrity_valid.yml', status)
   call check(status == CCPP_CTX_INVALID, 'reject configuration disagreement')
   call ccpp_context_initialize_model(handle, 1, 1, status)
   call check(status == CCPP_CTX_OK, 'initialize configured process schedule')
   call ccpp_context_get_model(handle, model, status)
   call check(status == CCPP_CTX_OK .and. associated(model), 'get initialized context model')
   interval%interval_id = 501_c_int64_t
   interval%year = 2024
   interval%month = 2
   interval%day = 29
   interval%hour = 12
   interval%minute = 0
   interval%second = 0
   interval%day_of_year = 60
   interval%duration_seconds = 60.0_c_double
   call model%begin_interval(interval, errflg)
   call check(errflg == 0, 'begin checked selected-dispatch interval')
   call model%run_selected_process(501_c_int64_t, 'site', 0_c_int32_t, 'dust', 30.0_c_double, &
                                   operation_status, publication_eligible, import_generation, errflg)
   call check(errflg /= 0 .and. operation_status == 1_c_int .and. .not. publication_eligible, &
              'unavailable selected process is rejected without full-schedule fallback')
   call model%end_interval(501_c_int64_t, errflg)
   call check(errflg == 0, 'end checked selected-dispatch interval')
   call timestep_lifecycle_state_alloc(1, errmsg, errflg)
   call check(errflg == 0, 'generated group state allocation')
   call timestep_lifecycle_init(1, 1, 1, errflg, errmsg)
   call check(errflg == 0, 'generated group init')
   context_id = 0
   context_generation = 0
   call timestep_lifecycle_timestep_init(1, 1, 1, errflg, errmsg)
   call check(errflg /= 0, 'generated cap propagates unavailable context')
   context_id = handle%id
   context_generation = handle%generation
   call timestep_lifecycle_timestep_init(1, 1, 1, errflg, errmsg)
   call check(errflg == 0, 'generated scheme interval open')
   call timestep_lifecycle_run(1, 1, 1, errflg, errmsg)
   call check(errflg == 0, 'generated group run')
   call timestep_lifecycle_timestep_final(1, 1, 1, errflg, errmsg)
   call check(errflg == 0, 'generated scheme interval close')
   call timestep_lifecycle_final(1, 1, 1, errflg, errmsg)
   call check(errflg == 0, 'generated group final')
   call timestep_lifecycle_state_dealloc(errmsg, errflg)
   call check(errflg == 0, 'generated group state deallocation')
   call ccpp_context_destroy(handle, status)
   call check(status == CCPP_CTX_OK, 'destroy context')
contains
   !> \brief Stops the test when a generated lifecycle assertion fails.
   subroutine check(condition, label)
      logical, intent(in) :: condition
      character(len=*), intent(in) :: label
      if (.not. condition) then
         write (*, '(a)') 'FAIL: '//label
         stop 1
      end if
   end subroutine check
end program test_ccpp_scheme_lifecycle

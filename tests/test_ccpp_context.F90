!> \\brief Exercises CATChem context isolation, lifecycle, and tendency tokens.
program test_ccpp_context
   use iso_c_binding, only: c_char, c_double, c_int, c_int64_t, c_null_char, c_ptr
   use CATChem_Context_Registry
   use CATChem_API, only: CATChem_Manifest_Species_v1, CATChem_Model
   use test_ccpp_constituent_mapping, only: run_registry_mapping_cases
   implicit none
   type(ccpp_context_handle) :: a, b, c, stale
   integer(c_int) :: status
   real(c_double), allocatable, target :: state_a(:, :, :), state_b(:, :, :), candidate(:, :, :)
   real(c_double), allocatable :: contribution(:, :, :), shared_tendency(:, :, :)
   integer, allocatable :: reverse_indices(:)
   integer :: species_count, species_index
   real(c_double), pointer, contiguous :: baseline(:, :, :), working(:, :, :), tendency(:, :, :)
   real(c_double), pointer, contiguous :: baseline_b(:, :, :), working_b(:, :, :), tendency_b(:, :, :)
   type(CATChem_Manifest_Species_v1) :: species
   type(CATChem_Model), pointer :: model
   type(CATChem_Model), pointer :: model_b
   character(len=512) :: name, standard_name, units
   integer :: errflg, config_unit, io_status, leases_before_finalization
   integer(c_int) :: mie_a, mie_b, mie_rc, limiter_rc
   character(kind=c_char) :: mie_type(32, 1), mie_path_a(512, 1), mie_path_b(512, 1), mie_name(64)
   real(c_double) :: mie_flux(1, 2), mie_rh(1, 2), mie_factor_a(1), mie_factor_b(1), mie_factor_check(1)
   logical :: mapping_passed
   interface
      integer(c_int) function attach_finalize_failure(core) bind(C, name='catchem_test_attach_finalize_failure')
         import :: c_int, c_ptr
         type(c_ptr), value :: core
      end function
      integer(c_int) function finalized_count() bind(C, name='catchem_test_finalized_count')
         import :: c_int
      end function
      integer(c_int) function runtime_lease_count() bind(C, name='catchem_test_runtime_lease_count')
         import :: c_int
      end function
      subroutine run_settling_mie_init(n_files, type_names, file_paths, mie_handle, init_rc) bind(C)
         import :: c_char, c_int
         integer(c_int), value :: n_files
         character(kind=c_char), intent(in) :: type_names(32, n_files), file_paths(512, n_files)
         integer(c_int), intent(out) :: mie_handle, init_rc
      end subroutine
      subroutine run_settling_mie_release(mie_handle) bind(C)
         import :: c_int
         integer(c_int), value :: mie_handle
      end subroutine
      subroutine run_emission_mie_factor(mie_handle, n_columns, n_levels, mie_name_c, emission_flux, rh, &
         scale_factor, dt, factor, limiter_rc) bind(C)
         import :: c_char, c_double, c_int
         integer(c_int), value :: mie_handle, n_columns, n_levels
         character(kind=c_char), intent(in) :: mie_name_c(64)
         real(c_double), intent(in) :: emission_flux(n_columns, n_levels), rh(n_columns, n_levels)
         real(c_double), value :: scale_factor, dt
         real(c_double), intent(out) :: factor(n_columns)
         integer(c_int), intent(out) :: limiter_rc
      end subroutine
   end interface

   allocate (state_a(2, 3, 2), state_b(2, 3, 3), candidate(2, 3, 2), contribution(2, 3, 2), &
      shared_tendency(2, 3, 2))
   state_a = 7.0_c_double
   state_b = 19.0_c_double
   call ccpp_context_create(2, 3, 2, a, status)
   call require(status == CCPP_CTX_OK .and. ccpp_context_valid(a), 'first context creation')
   call ccpp_context_create(2, 3, 3, b, status)
   call require(status == CCPP_CTX_OK .and. ccpp_context_valid(b), 'second context creation')
   call require(a%id /= b%id, 'contexts have distinct registry slots')

   call ccpp_context_scheme_init(a, 1, status)
   call require(status == CCPP_CTX_OK, 'Dust participant init')
   call ccpp_context_scheme_init(a, 2, status)
   call require(status == CCPP_CTX_OK, 'independent SeaSalt participant init')
   call ccpp_context_scheme_init(a, 1, status)
   call require(status == CCPP_CTX_PENDING, 'duplicate process init is rejected')
   call ccpp_context_scheme_final(a, 1, status)
   call require(status == CCPP_CTX_OK, 'Dust participant final')
   call ccpp_context_scheme_final(a, 1, status)
   call require(status == CCPP_CTX_PENDING, 'unmatched process final is rejected')
   call ccpp_context_scheme_final(a, 2, status)
   call require(status == CCPP_CTX_OK, 'SeaSalt participant remains independently active')

   call ccpp_context_begin_exchange(a, state_a, status)
   call require(status == CCPP_CTX_OK, 'begin exchange for first context')
   call ccpp_context_begin_exchange(b, state_b, status)
   call require(status == CCPP_CTX_OK, 'begin exchange for second context')
   call ccpp_context_get_buffers(a, baseline, working, tendency, status)
   call require(status == CCPP_CTX_OK .and. maxval(abs(baseline - 7.0_c_double)) < 1.0e-12_c_double, &
      'first context owns baseline buffer')
   call ccpp_context_get_buffers(b, baseline_b, working_b, tendency_b, status)
   call require(status == CCPP_CTX_OK .and. maxval(abs(baseline_b - 19.0_c_double)) < 1.0e-12_c_double, &
      'second context owns independent buffers')

   ! A no-op exchange stages the exact input array and leaves publication pending
   ! until the host explicitly commits the complete candidate.
   call ccpp_context_stage_publication(a, working, status)
   call require(status == CCPP_CTX_OK, 'stage no-op candidate')
   call ccpp_context_commit_publication(a, state_a, status)
   call require(status == CCPP_CTX_OK .and. maxval(abs(state_a - 7.0_c_double)) < 1.0e-12_c_double, 'commit no-op exchange')
   call ccpp_context_commit_publication(a, state_a, status)
   call require(status == CCPP_CTX_PENDING, 'second commit has no pending publication')
   candidate = 3.0_c_double
   call ccpp_context_stage_publication(a, candidate, status)
   call require(status == CCPP_CTX_OK, 'stage candidate for cancellation')
   call ccpp_context_cancel_publication(a, status)
   call require(status == CCPP_CTX_OK, 'cancel pending publication')
   call require(maxval(abs(state_b - 19.0_c_double)) < 1.0e-12_c_double, &
      'uncommitted context does not publish into another host array')

   contribution = 2.0_c_double
   shared_tendency = 5.0_c_double
   call ccpp_context_open_interval(a, 11_c_int64_t, status)
   errflg = int(status)
   call require(errflg == 0, 'open host-owned interval through lifecycle helper')
   call ccpp_context_open_interval(a, 11_c_int64_t, status)
   errflg = int(status)
   call require(errflg == 0, 'repeated interval init is idempotent')
   call ccpp_context_stage_tendency(a, 11_c_int64_t, contribution, status)
   call require(status == CCPP_CTX_OK, 'stage token-bound host tendency')
   call ccpp_context_begin_exchange(a, state_a, status)
   call require(status == CCPP_CTX_PENDING, 'new exchange cannot discard an unapplied contribution')
#ifdef CATCHEM_CONTEXT_WITH_CCPP
   call ccpp_context_close_interval(a, 11_c_int64_t, status)
   errflg = int(status)
#else
   call ccpp_context_close_interval(a, 11_c_int64_t, status)
   errflg = int(status)
#endif
   call require(errflg /= 0, 'interval cannot close while its contribution is pending')
   call ccpp_context_apply_tendency(a, 11_c_int64_t, 2.0_c_double, shared_tendency, status)
   call require(status == CCPP_CTX_OK .and. maxval(abs(shared_tendency - 9.0_c_double)) < 1.0e-12_c_double, &
      'matching token applies duration times the tendency exactly once')
   call ccpp_context_apply_tendency(a, 11_c_int64_t, 2.0_c_double, shared_tendency, status)
   call require(status == CCPP_CTX_PENDING .and. maxval(abs(shared_tendency - 9.0_c_double)) < 1.0e-12_c_double, &
      'a contribution token is consumed once')
   call ccpp_context_stage_tendency(a, 11_c_int64_t, contribution, status)
   call require(status == CCPP_CTX_OK, 'a later invocation can stage a contribution in the same host interval')
   call ccpp_context_apply_tendency(a, 11_c_int64_t, 2.0_c_double, shared_tendency, status)
   call require(status == CCPP_CTX_OK .and. maxval(abs(shared_tendency - 13.0_c_double)) < 1.0e-12_c_double, &
      'a later invocation in the same interval accumulates without clearing prior contributors')
   call ccpp_context_apply_tendency(a, 11_c_int64_t, 2.0_c_double, shared_tendency, status)
   call require(status == CCPP_CTX_PENDING .and. maxval(abs(shared_tendency - 13.0_c_double)) < 1.0e-12_c_double, &
      'each staged invocation contribution is consumed only once')
   call ccpp_context_close_interval(a, 11_c_int64_t, status)
   errflg = int(status)
   call require(errflg == 0, 'close applied host-owned interval through lifecycle helper')

   contribution = huge(0.0_c_double)
   shared_tendency = 3.0_c_double
   shared_tendency(1, 1, 1) = huge(0.0_c_double)
   call ccpp_context_open_interval(a, 12_c_int64_t, status)
   errflg = int(status)
   call require(errflg == 0, 'open next host-owned interval')
   call ccpp_context_stage_tendency(a, 12_c_int64_t, contribution, status)
   call require(status == CCPP_CTX_OK, 'stage a contribution before overflow check')
   call ccpp_context_apply_tendency(a, 12_c_int64_t, 1.0_c_double, shared_tendency, status)
   call require(status == CCPP_CTX_INVALID .and. &
      shared_tendency(1, 1, 1) > 0.99_c_double*huge(0.0_c_double) .and. &
      abs(shared_tendency(2, 3, 2) - 3.0_c_double) < 1.0e-12_c_double, &
      'overflow rejects the complete tendency addition')
   call ccpp_context_cancel_tendency(a, 12_c_int64_t, status)
   call require(status == CCPP_CTX_OK, 'failed overflow contribution can be canceled')
   call ccpp_context_close_interval(a, 12_c_int64_t, status)
   errflg = int(status)
   call require(errflg == 0, 'close canceled host-owned interval')

   call ccpp_context_bind_manifest(a, 'CATChem_nuopc_lifecycle_config.yml', status)
   call require(status == CCPP_CTX_OK, 'immutable mechanism manifest binding')
   call ccpp_context_bind_manifest(a, 'CATChem_nuopc_lifecycle_config.yml', status)
   call require(status == CCPP_CTX_OK, 'repeat bind to same manifest is idempotent')
   call ccpp_context_bind_manifest(a, 'fixtures/platform_integrity_valid.yml', status)
   call require(status == CCPP_CTX_INVALID, 'context cannot be rebound to another manifest')
   call ccpp_context_get_manifest_species(a, 1, species, name, standard_name, units, status)
   call require(status == CCPP_CTX_OK .and. len_trim(name) > 0 .and. len_trim(standard_name) > 0, &
      'immutable manifest species lookup')
   call ccpp_context_initialize_model(a, 2, 3, status)
   call require(status == CCPP_CTX_OK, 'one Core is initialized for the context')
   call run_registry_mapping_cases(a, mapping_passed)
   call require(mapping_passed, 'permuted registry map is accepted and every changed map invalidates the binding')
   call ccpp_context_initialize_model(a, 2, 3, status)
   call require(status == CCPP_CTX_OK, 'repeated Core initialization is idempotent')
   call ccpp_context_initialize_model(a, 3, 3, status)
   call require(status == CCPP_CTX_INVALID, 'initialized context grid is immutable')
   call ccpp_context_get_model(a, model, status)
   call require(status == CCPP_CTX_OK .and. model%is_initialized(), 'context owns its Core model')

   ! Bind a second context to a different valid mechanism configuration.
   call ccpp_context_bind_manifest(b, 'fixtures/platform_integrity_valid.yml', status)
   call require(status == CCPP_CTX_OK, 'second context binds its independent mechanism')
   call ccpp_context_initialize_model(b, 2, 3, status)
   call require(status == CCPP_CTX_OK, 'second context initializes an independent Core')
   call ccpp_context_manifest_species_count(b, species_count, status)
   call require(status == CCPP_CTX_OK .and. species_count > 0, 'second context has its own species registry')
   allocate (reverse_indices(species_count))
   do species_index = 1, species_count
      reverse_indices(species_index) = species_count - species_index + 1
   end do
   call ccpp_context_set_species_map(b, reverse_indices, status, species_count)
   call require(status == CCPP_CTX_OK, 'second context accepts its reversed registry mapping')
   call ccpp_context_get_model(b, model_b, status)
   call require(status == CCPP_CTX_OK .and. model_b%is_initialized(), 'second context Core is live')

   ! The bridge owns loaded Mie tables by opaque handle. Distinct table values
   ! must produce distinct results, and releasing one handle cannot invalidate
   ! another context's table.
   mie_type = ' '
   mie_type(1:2, 1) = [character(kind=c_char) :: 'D', 'U']
   mie_path_a = ' '
   mie_path_b = ' '
   mie_path_a(1:24, 1) = [character(kind=c_char) :: &
      'f', 'i', 'x', 't', 'u', 'r', 'e', 's', '/', 'g', 'o', 'c', 'a', 'r', 't', '_', 'm', 'i', 'e', '_', 'a', '.', 'n', 'c']
   mie_path_b(1:24, 1) = [character(kind=c_char) :: &
      'f', 'i', 'x', 't', 'u', 'r', 'e', 's', '/', 'g', 'o', 'c', 'a', 'r', 't', '_', 'm', 'i', 'e', '_', 'b', '.', 'n', 'c']
   call run_settling_mie_init(1_c_int, mie_type, mie_path_a, mie_a, mie_rc)
   call require(mie_rc == 0_c_int .and. mie_a > 0_c_int, 'load first independent Mie table context')
   call run_settling_mie_init(1_c_int, mie_type, mie_path_b, mie_b, mie_rc)
   call require(mie_rc == 0_c_int .and. mie_b > 0_c_int .and. mie_a /= mie_b, &
      'load second independent Mie table context')
   mie_name = c_null_char
   mie_name(1:2) = [character(kind=c_char) :: 'D', 'U']
   mie_flux = 1.0_c_double
   mie_rh = 0.5_c_double
   call run_emission_mie_factor(mie_a, 1_c_int, 2_c_int, mie_name, mie_flux, mie_rh, 1.0_c_double, 600.0_c_double, &
      mie_factor_a, limiter_rc)
   call require(limiter_rc == 0_c_int, 'query first Mie table handle')
   call run_emission_mie_factor(mie_b, 1_c_int, 2_c_int, mie_name, mie_flux, mie_rh, 1.0_c_double, 600.0_c_double, &
      mie_factor_b, limiter_rc)
   call require(limiter_rc == 0_c_int .and. abs(mie_factor_a(1) - mie_factor_b(1)) > 1.0e-12_c_double, &
      'each Mie handle retains its own table values')
   call run_settling_mie_release(mie_a)
   call run_emission_mie_factor(mie_b, 1_c_int, 2_c_int, mie_name, mie_flux, mie_rh, 1.0_c_double, 600.0_c_double, &
      mie_factor_check, limiter_rc)
   call require(limiter_rc == 0_c_int .and. abs(mie_factor_check(1) - mie_factor_b(1)) < 1.0e-15_c_double, &
      'releasing first Mie context preserves second context table')
   call run_emission_mie_factor(mie_a, 1_c_int, 2_c_int, mie_name, mie_flux, mie_rh, 1.0_c_double, 600.0_c_double, &
      mie_factor_check, limiter_rc)
   call require(limiter_rc == 2_c_int, 'released Mie handle is rejected')
   call run_settling_mie_release(mie_b)

   ! Force a real Core construction followed by manifest-validation failure.
   ! The context must finalize that partial initialization and remain reusable,
   ! while the independently initialized context b stays alive.
   call write_context_config('fixtures/platform_integrity_species.yml')
   call ccpp_context_create(2, 3, 3, c, status)
   call require(status == CCPP_CTX_OK, 'partial-init context creation')
   call ccpp_context_bind_manifest(c, 'context_init_failure.yml', status)
   call require(status == CCPP_CTX_OK, 'partial-init context binds its original manifest')
   call write_context_config('fixtures/host_conformance/species.yml')
   call ccpp_context_initialize_model(c, 2, 3, status)
   call require(status == CCPP_CTX_INVALID, 'changed mechanism fails manifest validation after Core creation')
   call write_context_config('fixtures/platform_integrity_species.yml')
   call ccpp_context_initialize_model(c, 2, 3, status)
   call require(status == CCPP_CTX_OK, 'context recovers after manifest-validation cleanup')
   call ccpp_context_get_model(c, model, status)
   call require(status == CCPP_CTX_OK .and. model%is_initialized(), 'recovered context owns a live Core')
   call require(model_b%is_initialized(), 'partial-init cleanup preserves the other context Core')
   status = attach_finalize_failure(model%cpp_core_ptr)
   call require(status == 0_c_int, 'attach test finalizers to initialized context Core')
   leases_before_finalization = runtime_lease_count()
   call ccpp_context_destroy(c, status)
   call require(status == CCPP_CTX_INVALID .and. c%id == 0, 'failed finalization reports error and clears context')
   call require(finalized_count() == 2_c_int, 'all finalizers run despite throwing errors')
   call require(index(model%last_error, 'injected finalizer 2') > 0, 'checked teardown preserves first cleanup error')
   call require(runtime_lease_count() == leases_before_finalization - 1, 'failed teardown releases exactly one runtime owner')
   call require(model_b%is_initialized(), 'finalizer failure preserves independent peer Core')
   open (newunit=config_unit, file='context_init_failure.yml', status='old', iostat=io_status)
   call require(io_status == 0, 'temporary context config exists before cleanup')
   close (config_unit, status='delete', iostat=io_status)
   call require(io_status == 0, 'remove temporary context config')

   stale = a
   call ccpp_context_destroy(a, status)
   call require(status == CCPP_CTX_OK .and. a%id == 0, 'destroy clears host handle')
   call require(.not. ccpp_context_valid(stale), 'destroyed generation is stale')
   call require(model_b%is_initialized() .and. maxval(abs(state_b - 19.0_c_double)) < 1.0e-12_c_double, &
      'destroying the first context preserves the second model and host state')
   call ccpp_context_scheme_init(stale, 1, status)
   call require(status == CCPP_CTX_STALE, 'stale ID/generation pair is rejected')
   call ccpp_context_destroy(b, status)
   call require(status == CCPP_CTX_OK, 'destroy second context')
   call ccpp_context_destroy(b, status)
   call require(status == CCPP_CTX_ABSENT, 'zero ID denotes absent context')

   ! A second complete context/model lifetime must work after teardown, with a
   ! fresh handle generation and no state retained from the first cycle.
   call ccpp_context_create(2, 3, 2, a, status)
   call require(status == CCPP_CTX_OK .and. ccpp_context_valid(a), 'second lifecycle cycle creates context')
   call ccpp_context_bind_manifest(a, 'CATChem_nuopc_lifecycle_config.yml', status)
   call require(status == CCPP_CTX_OK, 'second lifecycle cycle binds manifest')
   call ccpp_context_initialize_model(a, 2, 3, status)
   call require(status == CCPP_CTX_OK, 'second lifecycle cycle initializes Core')
   call ccpp_context_get_model(a, model, status)
   call require(status == CCPP_CTX_OK .and. model%is_initialized(), 'second lifecycle Core is live')
   call ccpp_context_destroy(a, status)
   call require(status == CCPP_CTX_OK .and. a%id == 0, 'second lifecycle cycle tears down cleanly')

   call ccpp_context_exhaust_generations_for_test()
   call ccpp_context_create(1, 1, 1, a, status)
   call require(status == CCPP_CTX_EXHAUSTED .and. a%id == 0, 'generation exhaustion is rejected without wrap')

   print *, 'PASS test_ccpp_context'
contains
   !> \brief Writes a small valid CATChem config used to induce manifest drift.
   !> \param species_file Relative path to the species catalog for this config.
   subroutine write_context_config(species_file)
      character(len=*), intent(in) :: species_file
      integer :: unit, local_status
      open (newunit=unit, file='context_init_failure.yml', status='replace', action='write', iostat=local_status)
      call require(local_status == 0, 'open temporary context config')
      write (unit, '(a)') 'simulation:'
      write (unit, '(a)') '  name: context-init-failure'
      write (unit, '(a)') '  species_filename: '//trim(species_file)
      write (unit, '(a)') '  nx: 2'
      write (unit, '(a)') '  ny: 1'
      write (unit, '(a)') '  nz: 3'
      write (unit, '(a)') '  timestep: 60'
      write (unit, '(a)') '  nsteps: 2'
      write (unit, '(a)') 'grid:'
      write (unit, '(a)') '  number_of_levels: 3'
      write (unit, '(a)') '  number_of_soil_layers: 2'
      write (unit, '(a)') 'diagnostics:'
      write (unit, '(a)') '  output:'
      write (unit, '(a)') '    enabled: false'
      write (unit, '(a)') '    directory: output'
      write (unit, '(a)') '    prefix: context'
      write (unit, '(a)') '    frequency: 60'
      write (unit, '(a)') '    format: netcdf'
      write (unit, '(a)') '    compress_lev: 0'
      write (unit, '(a)') '  collection:'
      write (unit, '(a)') '    enabled: false'
      write (unit, '(a)') '    buffer_size: 1'
      write (unit, '(a)') 'run_phases: {}'
      write (unit, '(a)') 'processes: {}'
      close (unit)
   end subroutine

   !> \\brief Fails the test program with the supplied assertion message.
   !> \\param condition True when the assertion passes.
   !> \\param message Description printed when the assertion fails.
   subroutine require(condition, message)
      logical, intent(in) :: condition
      character(len=*), intent(in) :: message
      if (.not. condition) then
         print *, 'FAIL: ', trim(message)
         stop 1
      end if
   end subroutine
end program test_ccpp_context

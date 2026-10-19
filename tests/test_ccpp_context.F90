!> \\brief Exercises CATChem context isolation, lifecycle, and tendency tokens.
program test_ccpp_context
  use iso_c_binding, only: c_double, c_int, c_int64_t
  use CATChem_Context_Registry
  use CATChem_API, only: CATChem_Manifest_Species_v1, CATChem_Model
  use catchem_ccpp_lifecycle, only: catchem_ccpp_timestep_init, catchem_ccpp_timestep_final
  implicit none
  type(ccpp_context_handle) :: a, b, stale
  integer(c_int) :: status
  real(c_double), allocatable, target :: state_a(:,:,:), state_b(:,:,:), candidate(:,:,:)
  real(c_double), allocatable :: contribution(:,:,:), shared_tendency(:,:,:)
  real(c_double), pointer, contiguous :: baseline(:,:,:), working(:,:,:), tendency(:,:,:)
  real(c_double), pointer, contiguous :: baseline_b(:,:,:), working_b(:,:,:), tendency_b(:,:,:)
  type(CATChem_Manifest_Species_v1) :: species
  type(CATChem_Model), pointer :: model
  character(len=512) :: name, standard_name, units
  character(len=512) :: errmsg
  integer :: errflg

  allocate(state_a(2,3,2), state_b(2,3,2), candidate(2,3,2), contribution(2,3,2), shared_tendency(2,3,2))
  state_a = 7.0_c_double
  state_b = 19.0_c_double
  call ccpp_context_create(2,3,2,a,status)
  call require(status == CCPP_CTX_OK .and. ccpp_context_valid(a), 'first context creation')
  call ccpp_context_create(2,3,2,b,status)
  call require(status == CCPP_CTX_OK .and. ccpp_context_valid(b), 'second context creation')
  call require(a%id /= b%id, 'contexts have distinct registry slots')

  call ccpp_context_scheme_init(a,1,status)
  call require(status == CCPP_CTX_OK, 'Dust participant init')
  call ccpp_context_scheme_init(a,2,status)
  call require(status == CCPP_CTX_OK, 'independent SeaSalt participant init')
  call ccpp_context_scheme_init(a,1,status)
  call require(status == CCPP_CTX_PENDING, 'duplicate process init is rejected')
  call ccpp_context_scheme_final(a,1,status)
  call require(status == CCPP_CTX_OK, 'Dust participant final')
  call ccpp_context_scheme_final(a,1,status)
  call require(status == CCPP_CTX_PENDING, 'unmatched process final is rejected')
  call ccpp_context_scheme_final(a,2,status)
  call require(status == CCPP_CTX_OK, 'SeaSalt participant remains independently active')

  call ccpp_context_begin_exchange(a,state_a,status)
  call require(status == CCPP_CTX_OK, 'begin exchange for first context')
  call ccpp_context_begin_exchange(b,state_b,status)
  call require(status == CCPP_CTX_OK, 'begin exchange for second context')
  call ccpp_context_get_buffers(a,baseline,working,tendency,status)
  call require(status == CCPP_CTX_OK .and. maxval(abs(baseline-7.0_c_double)) < 1.0e-12_c_double, &
               'first context owns baseline buffer')
  call ccpp_context_get_buffers(b,baseline_b,working_b,tendency_b,status)
  call require(status == CCPP_CTX_OK .and. maxval(abs(baseline_b-19.0_c_double)) < 1.0e-12_c_double, &
               'second context owns independent buffers')

  ! A no-op exchange stages the exact input array and leaves publication pending
  ! until the host explicitly commits the complete candidate.
  call ccpp_context_stage_publication(a,working,status)
  call require(status == CCPP_CTX_OK, 'stage no-op candidate')
  call ccpp_context_commit_publication(a,state_a,status)
  call require(status == CCPP_CTX_OK .and. maxval(abs(state_a-7.0_c_double)) < 1.0e-12_c_double, 'commit no-op exchange')
  call ccpp_context_commit_publication(a,state_a,status)
  call require(status == CCPP_CTX_PENDING, 'second commit has no pending publication')
  candidate = 3.0_c_double
  call ccpp_context_stage_publication(a,candidate,status)
  call require(status == CCPP_CTX_OK, 'stage candidate for cancellation')
  call ccpp_context_cancel_publication(a,status)
  call require(status == CCPP_CTX_OK, 'cancel pending publication')
  call require(maxval(abs(state_b-19.0_c_double)) < 1.0e-12_c_double, &
               'uncommitted context does not publish into another host array')

  contribution = 2.0_c_double
  shared_tendency = 5.0_c_double
  call catchem_ccpp_timestep_init(a%id,a%generation,11_c_int64_t,errmsg,errflg)
  call require(errflg == 0, 'open host-owned interval through lifecycle helper')
  call catchem_ccpp_timestep_init(a%id,a%generation,11_c_int64_t,errmsg,errflg)
  call require(errflg == 0, 'repeated interval init is idempotent')
  call ccpp_context_stage_tendency(a,11_c_int64_t,contribution,status)
  call require(status == CCPP_CTX_OK, 'stage token-bound host tendency')
  call ccpp_context_begin_exchange(a,state_a,status)
  call require(status == CCPP_CTX_PENDING, 'new exchange cannot discard an unapplied contribution')
  call catchem_ccpp_timestep_final(a%id,a%generation,11_c_int64_t,errmsg,errflg)
  call require(errflg /= 0, 'interval cannot close while its contribution is pending')
  call ccpp_context_apply_tendency(a,11_c_int64_t,2.0_c_double,shared_tendency,status)
  call require(status == CCPP_CTX_OK .and. maxval(abs(shared_tendency-9.0_c_double)) < 1.0e-12_c_double, &
               'matching token applies duration times the tendency exactly once')
  call ccpp_context_apply_tendency(a,11_c_int64_t,2.0_c_double,shared_tendency,status)
  call require(status == CCPP_CTX_PENDING .and. maxval(abs(shared_tendency-9.0_c_double)) < 1.0e-12_c_double, &
               'a contribution token is consumed once')
  call ccpp_context_stage_tendency(a,11_c_int64_t,contribution,status)
  call require(status == CCPP_CTX_PENDING, 'an applied token cannot be staged again')
  call catchem_ccpp_timestep_final(a%id,a%generation,11_c_int64_t,errmsg,errflg)
  call require(errflg == 0, 'close applied host-owned interval through lifecycle helper')

  contribution = huge(0.0_c_double)
  shared_tendency = 3.0_c_double
  shared_tendency(1,1,1) = huge(0.0_c_double)
  call catchem_ccpp_timestep_init(a%id,a%generation,12_c_int64_t,errmsg,errflg)
  call require(errflg == 0, 'open next host-owned interval')
  call ccpp_context_stage_tendency(a,12_c_int64_t,contribution,status)
  call require(status == CCPP_CTX_OK, 'stage a contribution before overflow check')
  call ccpp_context_apply_tendency(a,12_c_int64_t,1.0_c_double,shared_tendency,status)
  call require(status == CCPP_CTX_INVALID .and. &
               shared_tendency(1,1,1) > 0.99_c_double*huge(0.0_c_double) .and. &
               abs(shared_tendency(2,3,2)-3.0_c_double) < 1.0e-12_c_double, &
               'overflow rejects the complete tendency addition')
  call ccpp_context_cancel_tendency(a,12_c_int64_t,status)
  call require(status == CCPP_CTX_OK, 'failed pending contribution can be canceled')
  call catchem_ccpp_timestep_final(a%id,a%generation,12_c_int64_t,errmsg,errflg)
  call require(errflg == 0, 'close canceled host-owned interval')

  call ccpp_context_bind_manifest(a,'CATChem_nuopc_lifecycle_config.yml',status)
  call require(status == CCPP_CTX_OK, 'immutable mechanism manifest binding')
  call ccpp_context_bind_manifest(a,'CATChem_nuopc_lifecycle_config.yml',status)
  call require(status == CCPP_CTX_OK, 'repeat bind to same manifest is idempotent')
  call ccpp_context_bind_manifest(a,'fixtures/platform_integrity_valid.yml',status)
  call require(status == CCPP_CTX_INVALID, 'context cannot be rebound to another manifest')
  call ccpp_context_get_manifest_species(a,1,species,name,standard_name,units,status)
  call require(status == CCPP_CTX_OK .and. len_trim(name) > 0 .and. len_trim(standard_name) > 0, &
               'immutable manifest species lookup')
  call ccpp_context_initialize_model(a,2,3,status)
  call require(status == CCPP_CTX_OK, 'one Core is initialized for the context')
  call ccpp_context_initialize_model(a,2,3,status)
  call require(status == CCPP_CTX_OK, 'repeated Core initialization is idempotent')
  call ccpp_context_initialize_model(a,3,3,status)
  call require(status == CCPP_CTX_INVALID, 'initialized context grid is immutable')
  call ccpp_context_get_model(a,model,status)
  call require(status == CCPP_CTX_OK .and. model%is_initialized(), 'context owns its Core model')

  stale = a
  call ccpp_context_destroy(a,status)
  call require(status == CCPP_CTX_OK .and. a%id == 0, 'destroy clears host handle')
  call require(.not. ccpp_context_valid(stale), 'destroyed generation is stale')
  call ccpp_context_scheme_init(stale,1,status)
  call require(status == CCPP_CTX_STALE, 'stale ID/generation pair is rejected')
  call ccpp_context_destroy(b,status)
  call require(status == CCPP_CTX_OK, 'destroy second context')
  call ccpp_context_destroy(b,status)
  call require(status == CCPP_CTX_ABSENT, 'zero ID denotes absent context')

  call ccpp_context_exhaust_generations_for_test()
  call ccpp_context_create(1,1,1,a,status)
  call require(status == CCPP_CTX_EXHAUSTED .and. a%id == 0, 'generation exhaustion is rejected without wrap')

  print *, 'PASS test_ccpp_context'
contains
  !> \\brief Fails the test program with the supplied assertion message.
  !> \\param condition True when the assertion passes.
  !> \\param message Description printed when the assertion fails.
  subroutine require(condition,message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message
    if (.not. condition) then
      print *, 'FAIL: ', trim(message)
      stop 1
    end if
  end subroutine
end program test_ccpp_context

! Host-owned, generation-checked CATChem contexts shared by CCPP scheme instances.
module CATChem_Context_Registry
  use iso_c_binding, only: c_double, c_int64_t, c_int
  use iso_fortran_env, only: error_unit
  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
  use CATChem_API, only: CATChem_Model, CATChem_Manifest, CATChem_Manifest_Species_v1
  implicit none
  private
  integer, parameter :: max_contexts = 128, max_process_participants = 9
  integer(c_int), parameter, public :: CCPP_CTX_OK = 0, CCPP_CTX_ABSENT = 1, CCPP_CTX_STALE = 2
  integer(c_int), parameter, public :: CCPP_CTX_EXHAUSTED = 3, CCPP_CTX_INVALID = 4
  integer(c_int), parameter, public :: CCPP_CTX_PENDING = 5
  integer(c_int64_t), parameter :: max_generation = huge(0_c_int64_t)

   !> \brief Stores instance-owned state for ccpp context handle.
  type, public :: ccpp_context_handle
    integer(c_int) :: id = 0
    integer(c_int64_t) :: generation = 0
  end type

   !> \brief Stores instance-owned state for context slot.
  type :: context_slot
    logical :: occupied = .false.
    integer(c_int64_t) :: generation = 0
    logical :: scheme_initialized(max_process_participants) = .false.
    logical :: publication_pending = .false.
    logical :: contribution_pending = .false.
    logical :: interval_open = .false.
    integer(c_int64_t) :: interval_token = 0_c_int64_t
    integer(c_int64_t) :: contribution_token = 0_c_int64_t
    integer(c_int64_t) :: last_applied_token = 0_c_int64_t
    type(CATChem_Model) :: model
    type(CATChem_Manifest) :: manifest
    logical :: manifest_bound = .false.
    character(len=1024) :: config_path = ''
    character(len=:), allocatable :: manifest_fingerprint
    integer :: ncol = 0, nlev = 0
    real(c_double), allocatable :: baseline(:,:,:), working(:,:,:), tendency(:,:,:)
    integer, allocatable :: host_species_index(:)
  end type
  type(context_slot), target, save :: slots(max_contexts)

   !> \brief Stores instance-owned state for registry guard.
  type :: registry_guard
  contains
    procedure :: acquire => acquire_registry_guard
    final :: release_registry_guard
  end type

  interface
  !> \brief Implements catchem context registry lock for the CATChem CCPP boundary.
    subroutine catchem_context_registry_lock() bind(C, name="catchem_context_registry_lock")
    end subroutine
  !> \brief Implements catchem context registry unlock for the CATChem CCPP boundary.
    subroutine catchem_context_registry_unlock() bind(C, name="catchem_context_registry_unlock")
    end subroutine
  end interface

  public :: ccpp_context_create, ccpp_context_destroy, ccpp_context_valid
  public :: ccpp_context_scheme_init, ccpp_context_scheme_final
  public :: ccpp_context_open_interval, ccpp_context_close_interval
  public :: ccpp_context_begin_exchange, ccpp_context_stage_publication
  public :: ccpp_context_commit_publication, ccpp_context_cancel_publication
  public :: ccpp_context_get_buffers
  public :: ccpp_context_bind_manifest, ccpp_context_initialize_model
  public :: ccpp_context_get_manifest_species, ccpp_context_manifest_species_count, ccpp_context_get_model
  public :: ccpp_context_set_species_map, ccpp_context_get_species_map
  public :: ccpp_context_stage_tendency, ccpp_context_apply_tendency, ccpp_context_cancel_tendency
#ifdef CATCHEM_CONTEXT_TESTING
  public :: ccpp_context_exhaust_generations_for_test
#endif

contains

  !> \brief Implements ccpp context manifest species count for the CATChem CCPP boundary.
  !> \param handle Argument used by ccpp_context_manifest_species_count.
  !> \param count Argument used by ccpp_context_manifest_species_count.
  !> \param status Argument used by ccpp_context_manifest_species_count.
  subroutine ccpp_context_manifest_species_count(handle, count, status)
    type(ccpp_context_handle), intent(in) :: handle
    integer, intent(out) :: count
    integer(c_int), intent(out) :: status
    type(registry_guard) :: guard
    integer :: i
    call guard%acquire()
    count = 0
    i = checked_slot(handle)
    status = CCPP_CTX_STALE
    if (i == 0) return
    if (.not. slots(i)%manifest_bound) then
      status = CCPP_CTX_INVALID
      return
    end if
    call slots(i)%manifest%species_count(count,status)
    if (status /= 0_c_int) status = CCPP_CTX_INVALID
  end subroutine

  !> \brief Implements acquire registry guard for the CATChem CCPP boundary.
  !> \param this Argument used by acquire_registry_guard.
  subroutine acquire_registry_guard(this)
    class(registry_guard), intent(inout) :: this
    call catchem_context_registry_lock()
  end subroutine

  !> \brief Implements release registry guard for the CATChem CCPP boundary.
  !> \param this Argument used by release_registry_guard.
  subroutine release_registry_guard(this)
    type(registry_guard), intent(inout) :: this
    call catchem_context_registry_unlock()
  end subroutine

#ifdef CATCHEM_CONTEXT_TESTING
  !> \brief Implements ccpp context exhaust generations for test for the CATChem CCPP boundary.
  subroutine ccpp_context_exhaust_generations_for_test()
    type(registry_guard) :: guard
    integer :: i
    call guard%acquire()
    do i = 1, max_contexts
      if (.not. slots(i)%occupied) slots(i)%generation = max_generation
    end do
  end subroutine
#endif

  !> \brief Implements ccpp context create for the CATChem CCPP boundary.
  !> \param ncol Argument used by ccpp_context_create.
  !> \param nlev Argument used by ccpp_context_create.
  !> \param nspec Argument used by ccpp_context_create.
  !> \param handle Argument used by ccpp_context_create.
  !> \param status Argument used by ccpp_context_create.
  subroutine ccpp_context_create(ncol, nlev, nspec, handle, status)
    type(registry_guard) :: guard
    integer, intent(in) :: ncol, nlev, nspec
    type(ccpp_context_handle), intent(out) :: handle
    integer(c_int), intent(out) :: status
    integer :: i, ierr
    call guard%acquire()
    handle = ccpp_context_handle()
    status = CCPP_CTX_EXHAUSTED
    if (ncol <= 0 .or. nlev <= 0 .or. nspec <= 0) then
      status = CCPP_CTX_INVALID
      return
    end if
    do i = 1, max_contexts
      if (slots(i)%occupied) cycle
      if (slots(i)%generation == max_generation) cycle
      slots(i)%generation = slots(i)%generation + 1_c_int64_t
      slots(i)%occupied = .true.
      slots(i)%scheme_initialized = .false.
      slots(i)%publication_pending = .false.
      slots(i)%contribution_pending = .false.
      slots(i)%interval_open = .false.
      slots(i)%interval_token = 0_c_int64_t
      slots(i)%contribution_token = 0_c_int64_t
      slots(i)%last_applied_token = 0_c_int64_t
      slots(i)%manifest_bound = .false.
      slots(i)%config_path = ''
      if (allocated(slots(i)%manifest_fingerprint)) deallocate(slots(i)%manifest_fingerprint)
      slots(i)%ncol = ncol
      slots(i)%nlev = nlev
      allocate(slots(i)%baseline(ncol,nlev,nspec), slots(i)%working(ncol,nlev,nspec), &
               slots(i)%tendency(ncol,nlev,nspec), stat=ierr)
      if (ierr /= 0) then
        slots(i)%occupied = .false.
        deallocate(slots(i)%baseline, slots(i)%working, slots(i)%tendency, stat=ierr)
        status = CCPP_CTX_EXHAUSTED
        return
      end if
      slots(i)%baseline = 0.0_c_double
      slots(i)%working = 0.0_c_double
      slots(i)%tendency = 0.0_c_double
      handle%id = int(i,c_int)
      handle%generation = slots(i)%generation
      status = CCPP_CTX_OK
      return
    end do
  end subroutine

  !> \brief Implements checked slot for the CATChem CCPP boundary.
  !> \param handle Argument used by checked_slot.
  integer function checked_slot(handle) result(index)
    type(ccpp_context_handle), intent(in) :: handle
    index = 0
    if (handle%id < 1 .or. handle%id > max_contexts .or. handle%generation <= 0) return
    if (.not. slots(handle%id)%occupied) return
    if (slots(handle%id)%generation /= handle%generation) return
    index = handle%id
  end function

  !> \brief Implements ccpp context valid for the CATChem CCPP boundary.
  !> \param handle Argument used by ccpp_context_valid.
  logical function ccpp_context_valid(handle)
    type(registry_guard) :: guard
    type(ccpp_context_handle), intent(in) :: handle
    call guard%acquire()
    ccpp_context_valid = checked_slot(handle) /= 0
  end function

  !> \brief Implements ccpp context destroy for the CATChem CCPP boundary.
  !> \param handle Argument used by ccpp_context_destroy.
  !> \param status Argument used by ccpp_context_destroy.
  subroutine ccpp_context_destroy(handle, status)
    type(registry_guard) :: guard
    type(ccpp_context_handle), intent(inout) :: handle
    integer(c_int), intent(out) :: status
    integer :: i, rc, manifest_rc
    call guard%acquire()
    i = checked_slot(handle)
    if (i == 0) then
      status = merge(CCPP_CTX_ABSENT, CCPP_CTX_STALE, handle%id == 0 .or. handle%generation == 0)
      return
    end if
    status = CCPP_CTX_OK
    if (slots(i)%model%is_initialized()) then
      call slots(i)%model%finalize(rc)
      if (rc /= 0) status = CCPP_CTX_INVALID
    end if
    if (slots(i)%manifest_bound) then
      call slots(i)%manifest%close(manifest_rc)
      if (manifest_rc /= 0) status = CCPP_CTX_INVALID
    end if
    slots(i)%manifest_bound = .false.
    slots(i)%config_path = ''
    if (allocated(slots(i)%manifest_fingerprint)) deallocate(slots(i)%manifest_fingerprint)
    if (allocated(slots(i)%baseline)) deallocate(slots(i)%baseline)
    if (allocated(slots(i)%working)) deallocate(slots(i)%working)
    if (allocated(slots(i)%tendency)) deallocate(slots(i)%tendency)
    if (allocated(slots(i)%host_species_index)) deallocate(slots(i)%host_species_index)
    slots(i)%publication_pending = .false.
    slots(i)%contribution_pending = .false.
    slots(i)%interval_open = .false.
    slots(i)%interval_token = 0_c_int64_t
    slots(i)%contribution_token = 0_c_int64_t
    slots(i)%last_applied_token = 0_c_int64_t
    slots(i)%scheme_initialized = .false.
    slots(i)%ncol = 0
    slots(i)%nlev = 0
    slots(i)%occupied = .false.
    handle = ccpp_context_handle()
  end subroutine

  !> \brief Implements ccpp context bind manifest for the CATChem CCPP boundary.
  !> \param handle Argument used by ccpp_context_bind_manifest.
  !> \param config_path Argument used by ccpp_context_bind_manifest.
  !> \param status Argument used by ccpp_context_bind_manifest.
  subroutine ccpp_context_bind_manifest(handle, config_path, status)
    type(ccpp_context_handle), intent(in) :: handle
    character(len=*), intent(in) :: config_path
    integer(c_int), intent(out) :: status
    type(registry_guard) :: guard
    integer :: i, rc
    character(len=:), allocatable :: fingerprint
    call guard%acquire()
    i = checked_slot(handle)
    status = CCPP_CTX_STALE
    if (i == 0) return
    if (len_trim(config_path) == 0 .or. len_trim(config_path) > len(slots(i)%config_path)) then
      status = CCPP_CTX_INVALID
      return
    end if
    if (slots(i)%manifest_bound) then
      if (trim(slots(i)%config_path) == trim(config_path)) then
        status = CCPP_CTX_OK
      else
        status = CCPP_CTX_INVALID
      end if
      return
    end if
    call slots(i)%manifest%open(trim(config_path),rc)
    if (rc /= 0) then
      status = CCPP_CTX_INVALID
      return
    end if
    call slots(i)%manifest%get_fingerprint(fingerprint,rc)
    if (rc /= 0 .or. len(fingerprint) == 0) then
      call slots(i)%manifest%close(rc)
      status = CCPP_CTX_INVALID
      return
    end if
    slots(i)%manifest_fingerprint = fingerprint
    slots(i)%config_path = trim(config_path)
    slots(i)%manifest_bound = .true.
    status = CCPP_CTX_OK
  end subroutine

  !> \brief Implements ccpp context initialize model for the CATChem CCPP boundary.
  !> \param handle Argument used by ccpp_context_initialize_model.
  !> \param ncol Argument used by ccpp_context_initialize_model.
  !> \param nlev Argument used by ccpp_context_initialize_model.
  !> \param status Argument used by ccpp_context_initialize_model.
  subroutine ccpp_context_initialize_model(handle, ncol, nlev, status)
    type(ccpp_context_handle), intent(in) :: handle
    integer, intent(in) :: ncol, nlev
    integer(c_int), intent(out) :: status
    type(registry_guard) :: guard
    integer :: i, rc
    call guard%acquire()
    i = checked_slot(handle)
    status = CCPP_CTX_STALE
    if (i == 0) return
    if (.not. slots(i)%manifest_bound .or. ncol <= 0 .or. nlev <= 0) then
      status = CCPP_CTX_INVALID
      return
    end if
    if (slots(i)%ncol /= ncol .or. slots(i)%nlev /= nlev) then
      status = CCPP_CTX_INVALID
      return
    end if
    if (slots(i)%model%is_initialized()) then
      if (slots(i)%ncol == ncol .and. slots(i)%nlev == nlev) then
        status = CCPP_CTX_OK
      else
        status = CCPP_CTX_INVALID
      end if
      return
    end if
    call slots(i)%model%initialize(trim(slots(i)%config_path),ncol,1,nlev,rc=rc)
    if (rc /= 0) then
      write(error_unit,'(a)') 'CATChem context model initialization failed: ' // trim(slots(i)%model%last_error)
      status = CCPP_CTX_INVALID
      return
    end if
    call slots(i)%model%validate_manifest(slots(i)%manifest,rc)
    if (rc /= 0) then
      write(error_unit,'(a)') 'CATChem context manifest validation failed: ' // trim(slots(i)%model%last_error)
      call slots(i)%model%finalize(rc)
      status = CCPP_CTX_INVALID
      return
    end if
    slots(i)%ncol = ncol
    slots(i)%nlev = nlev
    status = CCPP_CTX_OK
  end subroutine

  !> \brief Implements ccpp context get manifest species for the CATChem CCPP boundary.
  !> \param handle Argument used by ccpp_context_get_manifest_species.
  !> \param index Argument used by ccpp_context_get_manifest_species.
  !> \param descriptor Argument used by ccpp_context_get_manifest_species.
  !> \param name Argument used by ccpp_context_get_manifest_species.
  !> \param standard_name Argument used by ccpp_context_get_manifest_species.
  !> \param units Argument used by ccpp_context_get_manifest_species.
  !> \param status Argument used by ccpp_context_get_manifest_species.
  subroutine ccpp_context_get_manifest_species(handle, index, descriptor, name, standard_name, units, status)
    type(ccpp_context_handle), intent(in) :: handle
    integer, intent(in) :: index
    type(CATChem_Manifest_Species_v1), intent(inout) :: descriptor
    character(len=*), intent(out) :: name, standard_name, units
    integer(c_int), intent(out) :: status
    type(registry_guard) :: guard
    integer :: i, rc, count
    call guard%acquire()
    i = checked_slot(handle)
    status = CCPP_CTX_STALE
    if (i == 0) return
    if (.not. slots(i)%manifest_bound) then
      status = CCPP_CTX_INVALID
      return
    end if
    call slots(i)%manifest%species_count(count,rc)
    if (rc /= 0 .or. index < 1 .or. index > count) then
      status = CCPP_CTX_INVALID
      return
    end if
    call slots(i)%manifest%get_species(index,descriptor,name,standard_name,units,rc)
    status = merge(CCPP_CTX_OK,CCPP_CTX_INVALID,rc == 0)
  end subroutine

  !> \brief Implements ccpp context get model for the CATChem CCPP boundary.
  !> \param handle Argument used by ccpp_context_get_model.
  !> \param model Argument used by ccpp_context_get_model.
  !> \param status Argument used by ccpp_context_get_model.
  subroutine ccpp_context_get_model(handle, model, status)
    type(ccpp_context_handle), intent(in) :: handle
    type(CATChem_Model), pointer, intent(out) :: model
    integer(c_int), intent(out) :: status
    type(registry_guard) :: guard
    integer :: i
    nullify(model)
    call guard%acquire()
    i = checked_slot(handle)
    status = CCPP_CTX_STALE
    if (i == 0) return
    if (.not. slots(i)%model%is_initialized()) then
      status = CCPP_CTX_INVALID
      return
    end if
    model => slots(i)%model
    status = CCPP_CTX_OK
  end subroutine

  !> \brief Implements ccpp context scheme init for the CATChem CCPP boundary.
  !> \param handle Argument used by ccpp_context_scheme_init.
  !> \param process_id One-based CATChem process participant ID (1 through 9).
  !> \param status Argument used by ccpp_context_scheme_init.
  subroutine ccpp_context_scheme_init(handle, process_id, status)
    type(registry_guard) :: guard
    type(ccpp_context_handle), intent(in) :: handle
    integer, intent(in) :: process_id
    integer(c_int), intent(out) :: status
    integer :: i
    call guard%acquire()
    i = checked_slot(handle)
    status = CCPP_CTX_STALE
    if (i == 0) return
    if (process_id < 1 .or. process_id > max_process_participants) then
      status = CCPP_CTX_INVALID
      return
    end if
    if (slots(i)%scheme_initialized(process_id)) then
      status = CCPP_CTX_PENDING
      return
    end if
    slots(i)%scheme_initialized(process_id) = .true.
    status = CCPP_CTX_OK
  end subroutine

  !> \brief Implements ccpp context scheme final for the CATChem CCPP boundary.
  !> \param handle Argument used by ccpp_context_scheme_final.
  !> \param process_id One-based CATChem process participant ID (1 through 9).
  !> \param status Argument used by ccpp_context_scheme_final.
  subroutine ccpp_context_scheme_final(handle, process_id, status)
    type(registry_guard) :: guard
    type(ccpp_context_handle), intent(in) :: handle
    integer, intent(in) :: process_id
    integer(c_int), intent(out) :: status
    integer :: i
    call guard%acquire()
    i = checked_slot(handle)
    status = CCPP_CTX_STALE
    if (i == 0) return
    if (process_id < 1 .or. process_id > max_process_participants) then
      status = CCPP_CTX_INVALID
      return
    end if
    if (.not. slots(i)%scheme_initialized(process_id)) then
      status = CCPP_CTX_PENDING
      return
    end if
    slots(i)%scheme_initialized(process_id) = .false.
    status = CCPP_CTX_OK
  end subroutine

  !> \brief Implements ccpp context open interval for the CATChem CCPP boundary.
  !> \param handle Argument used by ccpp_context_open_interval.
  !> \param token Argument used by ccpp_context_open_interval.
  !> \param status Argument used by ccpp_context_open_interval.
  subroutine ccpp_context_open_interval(handle, token, status)
    type(ccpp_context_handle), intent(in) :: handle
    integer(c_int64_t), intent(in) :: token
    integer(c_int), intent(out) :: status
    type(registry_guard) :: guard
    integer :: i
    call guard%acquire()
    i = checked_slot(handle)
    status = CCPP_CTX_STALE
    if (i == 0) return
    if (token <= 0_c_int64_t .or. slots(i)%contribution_pending) then
      status = CCPP_CTX_PENDING
      return
    end if
    if (slots(i)%interval_open) then
      status = merge(CCPP_CTX_OK, CCPP_CTX_PENDING, slots(i)%interval_token == token)
      return
    end if
    slots(i)%interval_open = .true.
    slots(i)%interval_token = token
    status = CCPP_CTX_OK
  end subroutine

  !> \brief Implements ccpp context close interval for the CATChem CCPP boundary.
  !> \param handle Argument used by ccpp_context_close_interval.
  !> \param token Argument used by ccpp_context_close_interval.
  !> \param status Argument used by ccpp_context_close_interval.
  subroutine ccpp_context_close_interval(handle, token, status)
    type(ccpp_context_handle), intent(in) :: handle
    integer(c_int64_t), intent(in) :: token
    integer(c_int), intent(out) :: status
    type(registry_guard) :: guard
    integer :: i
    call guard%acquire()
    i = checked_slot(handle)
    status = CCPP_CTX_STALE
    if (i == 0) return
    if (.not. slots(i)%interval_open) then
      status = CCPP_CTX_OK
      return
    end if
    if (token /= slots(i)%interval_token .or. slots(i)%contribution_pending) then
      status = CCPP_CTX_PENDING
      return
    end if
    slots(i)%interval_open = .false.
    slots(i)%interval_token = 0_c_int64_t
    status = CCPP_CTX_OK
  end subroutine

  !> \brief Implements ccpp context begin exchange for the CATChem CCPP boundary.
  !> \param handle Argument used by ccpp_context_begin_exchange.
  !> \param constituent_state Argument used by ccpp_context_begin_exchange.
  !> \param status Argument used by ccpp_context_begin_exchange.
  subroutine ccpp_context_begin_exchange(handle, constituent_state, status)
    type(registry_guard) :: guard
    type(ccpp_context_handle), intent(in) :: handle
    real(c_double), intent(in) :: constituent_state(:,:,:)
    integer(c_int), intent(out) :: status
    integer :: i
    call guard%acquire()
    i = checked_slot(handle)
    status = CCPP_CTX_STALE
    if (i == 0) return
    if (slots(i)%contribution_pending) then
      status = CCPP_CTX_PENDING
      return
    end if
    if (any(shape(constituent_state) /= shape(slots(i)%baseline))) then
      status = CCPP_CTX_INVALID
      return
    end if
    slots(i)%baseline = constituent_state
    slots(i)%working = constituent_state
    slots(i)%tendency = 0.0_c_double
    slots(i)%publication_pending = .false.
    status = CCPP_CTX_OK
  end subroutine

  !> \brief Implements ccpp context stage publication for the CATChem CCPP boundary.
  !> \param handle Argument used by ccpp_context_stage_publication.
  !> \param candidate Argument used by ccpp_context_stage_publication.
  !> \param status Argument used by ccpp_context_stage_publication.
  subroutine ccpp_context_stage_publication(handle, candidate, status)
    type(registry_guard) :: guard
    type(ccpp_context_handle), intent(in) :: handle
    real(c_double), intent(in) :: candidate(:,:,:)
    integer(c_int), intent(out) :: status
    integer :: i
    call guard%acquire()
    i = checked_slot(handle)
    status = CCPP_CTX_STALE
    if (i == 0) return
    if (any(shape(candidate) /= shape(slots(i)%working)) .or. any(.not. ieee_is_finite(candidate))) then
      status = CCPP_CTX_INVALID
      return
    end if
    slots(i)%working = candidate
    slots(i)%publication_pending = .true.
    status = CCPP_CTX_OK
  end subroutine

  !> \brief Implements ccpp context commit publication for the CATChem CCPP boundary.
  !> \param handle Argument used by ccpp_context_commit_publication.
  !> \param constituent_state Argument used by ccpp_context_commit_publication.
  !> \param status Argument used by ccpp_context_commit_publication.
  subroutine ccpp_context_commit_publication(handle, constituent_state, status)
    type(registry_guard) :: guard
    type(ccpp_context_handle), intent(in) :: handle
    real(c_double), intent(inout) :: constituent_state(:,:,:)
    integer(c_int), intent(out) :: status
    integer :: i
    call guard%acquire()
    i = checked_slot(handle)
    status = CCPP_CTX_STALE
    if (i == 0) return
    if (.not. slots(i)%publication_pending) then
      status = CCPP_CTX_PENDING
      return
    end if
    if (any(shape(constituent_state) /= shape(slots(i)%working))) then
      status = CCPP_CTX_INVALID
      return
    end if
    constituent_state = slots(i)%working
    slots(i)%publication_pending = .false.
    status = CCPP_CTX_OK
  end subroutine

  !> \brief Implements ccpp context cancel publication for the CATChem CCPP boundary.
  !> \param handle Argument used by ccpp_context_cancel_publication.
  !> \param status Argument used by ccpp_context_cancel_publication.
  subroutine ccpp_context_cancel_publication(handle, status)
    type(registry_guard) :: guard
    type(ccpp_context_handle), intent(in) :: handle
    integer(c_int), intent(out) :: status
    integer :: i
    call guard%acquire()
    i = checked_slot(handle)
    status = CCPP_CTX_STALE
    if (i == 0) return
    slots(i)%working = slots(i)%baseline
    slots(i)%publication_pending = .false.
    status = CCPP_CTX_OK
  end subroutine

  !> \brief Implements ccpp context get buffers for the CATChem CCPP boundary.
  !> \param handle Argument used by ccpp_context_get_buffers.
  !> \param baseline Argument used by ccpp_context_get_buffers.
  !> \param working Argument used by ccpp_context_get_buffers.
  !> \param tendency Argument used by ccpp_context_get_buffers.
  !> \param status Argument used by ccpp_context_get_buffers.
  subroutine ccpp_context_get_buffers(handle, baseline, working, tendency, status)
    type(registry_guard) :: guard
    type(ccpp_context_handle), intent(in) :: handle
    real(c_double), pointer, contiguous, intent(out) :: baseline(:,:,:), working(:,:,:), tendency(:,:,:)
    integer(c_int), intent(out) :: status
    integer :: i
    call guard%acquire()
    nullify(baseline, working, tendency)
    i = checked_slot(handle)
    status = CCPP_CTX_STALE
    if (i == 0) return
    baseline => slots(i)%baseline
    working => slots(i)%working
    tendency => slots(i)%tendency
    status = CCPP_CTX_OK
  end subroutine

  !> \brief Implements ccpp context set species map for the CATChem CCPP boundary.
  !> \param handle Argument used by ccpp_context_set_species_map.
  !> \param host_indices Argument used by ccpp_context_set_species_map.
  !> \param status Argument used by ccpp_context_set_species_map.
  subroutine ccpp_context_set_species_map(handle, host_indices, status)
    type(ccpp_context_handle), intent(in) :: handle
    integer, intent(in) :: host_indices(:)
    integer(c_int), intent(out) :: status
    type(registry_guard) :: guard
    integer :: i, j
    call guard%acquire()
    i = checked_slot(handle)
    status = CCPP_CTX_STALE
    if (i == 0) return
    if (.not. slots(i)%manifest_bound) then
      status = CCPP_CTX_INVALID
      return
    end if
    if (size(host_indices) /= size(slots(i)%baseline,3) .or. any(host_indices <= 0)) then
      status = CCPP_CTX_INVALID
      return
    end if
    do j = 1, size(host_indices)
      if (any(host_indices(1:j-1) == host_indices(j))) then
        status = CCPP_CTX_INVALID
        return
      end if
    end do
    if (allocated(slots(i)%host_species_index)) then
      if (all(slots(i)%host_species_index == host_indices)) then
        status = CCPP_CTX_OK
      else
        status = CCPP_CTX_INVALID
      end if
      return
    end if
    allocate(slots(i)%host_species_index(size(host_indices)))
    slots(i)%host_species_index = host_indices
    status = CCPP_CTX_OK
  end subroutine

  !> \brief Implements ccpp context get species map for the CATChem CCPP boundary.
  !> \param handle Argument used by ccpp_context_get_species_map.
  !> \param host_indices Argument used by ccpp_context_get_species_map.
  !> \param status Argument used by ccpp_context_get_species_map.
  subroutine ccpp_context_get_species_map(handle, host_indices, status)
    type(ccpp_context_handle), intent(in) :: handle
    integer, allocatable, intent(out) :: host_indices(:)
    integer(c_int), intent(out) :: status
    type(registry_guard) :: guard
    integer :: i
    call guard%acquire()
    i = checked_slot(handle)
    status = CCPP_CTX_STALE
    if (i == 0) return
    status = CCPP_CTX_INVALID
    if (.not. allocated(slots(i)%host_species_index)) return
    host_indices = slots(i)%host_species_index
    status = CCPP_CTX_OK
  end subroutine

  !> \brief Implements ccpp context stage tendency for the CATChem CCPP boundary.
  !> \param handle Argument used by ccpp_context_stage_tendency.
  !> \param token Argument used by ccpp_context_stage_tendency.
  !> \param contribution Argument used by ccpp_context_stage_tendency.
  !> \param status Argument used by ccpp_context_stage_tendency.
  subroutine ccpp_context_stage_tendency(handle, token, contribution, status)
    type(ccpp_context_handle), intent(in) :: handle
    integer(c_int64_t), intent(in) :: token
    real(c_double), intent(in) :: contribution(:,:,:)
    integer(c_int), intent(out) :: status
    type(registry_guard) :: guard
    integer :: i
    call guard%acquire()
    i = checked_slot(handle)
    status = CCPP_CTX_STALE
    if (i == 0) return
    if (token <= 0_c_int64_t .or. .not. slots(i)%interval_open .or. token /= slots(i)%interval_token .or. &
        slots(i)%contribution_pending .or. &
        token == slots(i)%last_applied_token) then
      status = CCPP_CTX_PENDING
      return
    end if
    if (any(shape(contribution) /= shape(slots(i)%tendency)) .or. &
        any(.not. ieee_is_finite(contribution))) then
      status = CCPP_CTX_INVALID
      return
    end if
    slots(i)%tendency = contribution
    slots(i)%contribution_token = token
    slots(i)%contribution_pending = .true.
    status = CCPP_CTX_OK
  end subroutine

  !> \brief Implements ccpp context apply tendency for the CATChem CCPP boundary.
  !> \param handle Argument used by ccpp_context_apply_tendency.
  !> \param token Argument used by ccpp_context_apply_tendency.
  !> \param duration_seconds Argument used by ccpp_context_apply_tendency.
  !> \param host_tendency Argument used by ccpp_context_apply_tendency.
  !> \param status Argument used by ccpp_context_apply_tendency.
  subroutine ccpp_context_apply_tendency(handle, token, duration_seconds, host_tendency, status)
    type(ccpp_context_handle), intent(in) :: handle
    integer(c_int64_t), intent(in) :: token
    real(c_double), intent(in) :: duration_seconds
    real(c_double), intent(inout) :: host_tendency(:,:,:)
    integer(c_int), intent(out) :: status
    type(registry_guard) :: guard
    real(c_double), allocatable :: candidate(:,:,:)
    integer :: i, ierr
    call guard%acquire()
    i = checked_slot(handle)
    status = CCPP_CTX_STALE
    if (i == 0) return
    if (.not. slots(i)%interval_open .or. token /= slots(i)%interval_token .or. &
        .not. slots(i)%contribution_pending .or. token /= slots(i)%contribution_token .or. &
        token == slots(i)%last_applied_token) then
      status = CCPP_CTX_PENDING
      return
    end if
    if (.not. ieee_is_finite(duration_seconds) .or. duration_seconds <= 0.0_c_double) then
      status = CCPP_CTX_INVALID
      return
    end if
    if (any(shape(host_tendency) /= shape(slots(i)%tendency)) .or. &
        any(.not. ieee_is_finite(host_tendency))) then
      status = CCPP_CTX_INVALID
      return
    end if
    allocate(candidate,source=host_tendency,stat=ierr)
    if (ierr /= 0) then
      status = CCPP_CTX_INVALID
      return
    end if
    if (any(abs(slots(i)%tendency) > huge(0.0_c_double)/duration_seconds)) then
      status = CCPP_CTX_INVALID
      return
    end if
    if (any(abs(host_tendency) > huge(0.0_c_double) - abs(duration_seconds*slots(i)%tendency))) then
      status = CCPP_CTX_INVALID
      return
    end if
    candidate = host_tendency + duration_seconds*slots(i)%tendency
    if (any(.not. ieee_is_finite(candidate))) then
      status = CCPP_CTX_INVALID
      return
    end if
    host_tendency = candidate
    slots(i)%last_applied_token = token
    slots(i)%contribution_token = 0_c_int64_t
    slots(i)%contribution_pending = .false.
    status = CCPP_CTX_OK
  end subroutine

  !> \brief Implements ccpp context cancel tendency for the CATChem CCPP boundary.
  !> \param handle Argument used by ccpp_context_cancel_tendency.
  !> \param token Argument used by ccpp_context_cancel_tendency.
  !> \param status Argument used by ccpp_context_cancel_tendency.
  subroutine ccpp_context_cancel_tendency(handle, token, status)
    type(ccpp_context_handle), intent(in) :: handle
    integer(c_int64_t), intent(in) :: token
    integer(c_int), intent(out) :: status
    type(registry_guard) :: guard
    integer :: i
    call guard%acquire()
    i = checked_slot(handle)
    status = CCPP_CTX_STALE
    if (i == 0) return
    if (.not. slots(i)%contribution_pending .or. token /= slots(i)%contribution_token) then
      status = CCPP_CTX_PENDING
      return
    end if
    slots(i)%tendency = 0.0_c_double
    slots(i)%contribution_pending = .false.
    slots(i)%contribution_token = 0_c_int64_t
    status = CCPP_CTX_OK
  end subroutine

end module CATChem_Context_Registry

! CCPP-facing facade for the synchronized, host-owned CATChem context registry.
module catchem_ccpp_context
   use iso_c_binding, only: c_double, c_int, c_int64_t
   use CATChem_API, only: CATChem_Model, CATChem_Manifest_Species_v1
   use CATChem_Context_Registry, only: ccpp_context_handle, ccpp_context_create, ccpp_context_destroy, &
                                       ccpp_context_valid, ccpp_context_scheme_init, ccpp_context_scheme_final, &
                                       ccpp_context_open_interval, ccpp_context_close_interval, &
                                     ccpp_context_bind_manifest, ccpp_context_initialize_model, ccpp_context_get_manifest_species, &
                                       ccpp_context_manifest_species_count, &
                                       ccpp_context_get_model, ccpp_context_begin_exchange, ccpp_context_stage_publication, &
                                       ccpp_context_commit_publication, ccpp_context_cancel_publication, ccpp_context_get_buffers, &
                                       ccpp_context_set_species_map, ccpp_context_get_species_map, ccpp_context_clear_species_map, &
                                       ccpp_context_stage_tendency_registry => ccpp_context_stage_tendency, &
                                       ccpp_context_apply_tendency_registry => ccpp_context_apply_tendency, &
                                       ccpp_context_cancel_tendency_registry => ccpp_context_cancel_tendency, &
                                CCPP_CTX_OK, CCPP_CTX_ABSENT, CCPP_CTX_STALE, CCPP_CTX_EXHAUSTED, CCPP_CTX_INVALID, CCPP_CTX_PENDING
   implicit none
   private
   public :: ccpp_context_handle, CATChem_Manifest_Species_v1, CATChem_Model
   public :: CATCHEM_CONTEXT_SUCCESS, CATCHEM_CONTEXT_ABSENT, CATCHEM_CONTEXT_STALE
   public :: CATCHEM_CONTEXT_EXHAUSTED, CATCHEM_CONTEXT_INVALID, CATCHEM_CONTEXT_PENDING
   public :: CATCHEM_CCPP_PHYSICAL_RESTART_SUPPORTED
   public :: CATCHEM_CCPP_CONCURRENT_CONTEXT_EXECUTION_SUPPORTED
   public :: CATCHEM_CCPP_RESTART_COLD_START, CATCHEM_CCPP_RESTART_CONCENTRATION_ONLY
   public :: CATCHEM_CCPP_RESTART_MID_INTERVAL
   public :: catchem_ccpp_validate_restart_request
   public :: CATCHEM_CCPP_EXECUTION_SERIALIZED, CATCHEM_CCPP_EXECUTION_CONCURRENT
   public :: catchem_ccpp_validate_execution_request
   public :: catchem_ccpp_context_create, catchem_ccpp_context_destroy, catchem_ccpp_context_valid
   public :: catchem_ccpp_context_bind, catchem_ccpp_context_initialize
   public :: catchem_ccpp_context_scheme_init, catchem_ccpp_context_scheme_final
   public :: catchem_ccpp_context_open_interval, catchem_ccpp_context_close_interval
   public :: catchem_ccpp_context_species, catchem_ccpp_context_model
   public :: catchem_ccpp_context_species_count
   public :: catchem_ccpp_context_begin_exchange, catchem_ccpp_context_stage
   public :: catchem_ccpp_context_commit, catchem_ccpp_context_cancel, catchem_ccpp_context_buffers
   public :: catchem_ccpp_context_set_mapping, catchem_ccpp_context_get_mapping, catchem_ccpp_context_clear_mapping
   public :: catchem_ccpp_context_stage_tendency, catchem_ccpp_context_apply_tendency
   public :: catchem_ccpp_context_cancel_tendency

   integer(c_int), parameter :: CATCHEM_CONTEXT_SUCCESS = CCPP_CTX_OK
   integer(c_int), parameter :: CATCHEM_CONTEXT_ABSENT = CCPP_CTX_ABSENT
   integer(c_int), parameter :: CATCHEM_CONTEXT_STALE = CCPP_CTX_STALE
   integer(c_int), parameter :: CATCHEM_CONTEXT_EXHAUSTED = CCPP_CTX_EXHAUSTED
   integer(c_int), parameter :: CATCHEM_CONTEXT_INVALID = CCPP_CTX_INVALID
   integer(c_int), parameter :: CATCHEM_CONTEXT_PENDING = CCPP_CTX_PENDING
   !> Physical restart is unavailable until a complete solver and interval checkpoint exists.
   integer(c_int), parameter :: CATCHEM_CCPP_PHYSICAL_RESTART_SUPPORTED = 0_c_int
   !> Separate Core locks do not serialize process-global science libraries across contexts.
   integer(c_int), parameter :: CATCHEM_CCPP_CONCURRENT_CONTEXT_EXECUTION_SUPPORTED = 0_c_int
   !> Request kind accepted by catchem_ccpp_validate_restart_request for cold starts.
   integer(c_int), parameter :: CATCHEM_CCPP_RESTART_COLD_START = 0_c_int
   !> Unsupported concentration-only restore request.
   integer(c_int), parameter :: CATCHEM_CCPP_RESTART_CONCENTRATION_ONLY = 1_c_int
   !> Unsupported physical restore during an open host interval.
   integer(c_int), parameter :: CATCHEM_CCPP_RESTART_MID_INTERVAL = 2_c_int
   !> Request mode for host-serialized CATChem execution.
   integer(c_int), parameter :: CATCHEM_CCPP_EXECUTION_SERIALIZED = 0_c_int
   !> Unsupported request mode for concurrent native CATChem execution.
   integer(c_int), parameter :: CATCHEM_CCPP_EXECUTION_CONCURRENT = 1_c_int

contains
   !> Validate a restart request before the host enters CATChem science.
   !> @param request_kind Cold start, concentration-only restore, or mid-interval restore.
   !> @param status CATCHEM_CONTEXT_SUCCESS for cold start, CATCHEM_CONTEXT_INVALID otherwise.
   subroutine catchem_ccpp_validate_restart_request(request_kind, status) &
      bind(C, name='catchem_ccpp_validate_restart_request')
      integer(c_int), value, intent(in) :: request_kind
      integer(c_int), intent(out) :: status
      if (request_kind == CATCHEM_CCPP_RESTART_COLD_START) then
         status = CATCHEM_CONTEXT_SUCCESS
      else
         status = CATCHEM_CONTEXT_INVALID
      end if
   end subroutine catchem_ccpp_validate_restart_request

   !> Validate the host's native-call concurrency policy before science executes.
   !> @param request_kind Serialized or concurrent execution request.
   !> @param status CATCHEM_CONTEXT_SUCCESS for serialized execution, CATCHEM_CONTEXT_INVALID otherwise.
   subroutine catchem_ccpp_validate_execution_request(request_kind, status) &
      bind(C, name='catchem_ccpp_validate_execution_request')
      integer(c_int), value, intent(in) :: request_kind
      integer(c_int), intent(out) :: status
      if (request_kind == CATCHEM_CCPP_EXECUTION_SERIALIZED) then
         status = CATCHEM_CONTEXT_SUCCESS
      else
         status = CATCHEM_CONTEXT_INVALID
      end if
   end subroutine catchem_ccpp_validate_execution_request

   !> \brief Implements catchem ccpp context species count for the CATChem CCPP boundary.
   !> \param handle Argument used by catchem_ccpp_context_species_count.
   !> \param count Argument used by catchem_ccpp_context_species_count.
   !> \param status Argument used by catchem_ccpp_context_species_count.
   subroutine catchem_ccpp_context_species_count(handle, count, status)
      type(ccpp_context_handle), intent(in) :: handle
      integer, intent(out) :: count
      integer(c_int), intent(out) :: status
      call ccpp_context_manifest_species_count(handle, count, status)
   end subroutine

   !> \brief Clear the host registry map so failed remapping cannot use stale indices.
   !> \param handle Context to clear.
   !> \param status CCPP context status.
   subroutine catchem_ccpp_context_clear_mapping(handle, status)
      type(ccpp_context_handle), intent(in) :: handle
      integer(c_int), intent(out) :: status
      call ccpp_context_clear_species_map(handle, status)
   end subroutine

   !> \brief Implements catchem ccpp context create for the CATChem CCPP boundary.
   !> \param ncol Argument used by catchem_ccpp_context_create.
   !> \param nlev Argument used by catchem_ccpp_context_create.
   !> \param nspec Argument used by catchem_ccpp_context_create.
   !> \param handle Argument used by catchem_ccpp_context_create.
   !> \param status Argument used by catchem_ccpp_context_create.
   subroutine catchem_ccpp_context_create(ncol, nlev, nspec, handle, status)
      integer, intent(in) :: ncol, nlev, nspec
      type(ccpp_context_handle), intent(out) :: handle
      integer(c_int), intent(out) :: status
      call ccpp_context_create(ncol, nlev, nspec, handle, status)
   end subroutine

   !> \brief Implements catchem ccpp context destroy for the CATChem CCPP boundary.
   !> \param handle Argument used by catchem_ccpp_context_destroy.
   !> \param status Argument used by catchem_ccpp_context_destroy.
   subroutine catchem_ccpp_context_destroy(handle, status)
      type(ccpp_context_handle), intent(inout) :: handle
      integer(c_int), intent(out) :: status
      call ccpp_context_destroy(handle, status)
   end subroutine

   !> \brief Implements catchem ccpp context valid for the CATChem CCPP boundary.
   !> \param handle Argument used by catchem_ccpp_context_valid.
   logical function catchem_ccpp_context_valid(handle)
      type(ccpp_context_handle), intent(in) :: handle
      catchem_ccpp_context_valid = ccpp_context_valid(handle)
   end function

   !> \brief Implements catchem ccpp context bind for the CATChem CCPP boundary.
   !> \param handle Argument used by catchem_ccpp_context_bind.
   !> \param config_path Argument used by catchem_ccpp_context_bind.
   !> \param status Argument used by catchem_ccpp_context_bind.
   subroutine catchem_ccpp_context_bind(handle, config_path, status)
      type(ccpp_context_handle), intent(in) :: handle
      character(len=*), intent(in) :: config_path
      integer(c_int), intent(out) :: status
      call ccpp_context_bind_manifest(handle, config_path, status)
   end subroutine

   !> \brief Implements catchem ccpp context initialize for the CATChem CCPP boundary.
   !> \param handle Argument used by catchem_ccpp_context_initialize.
   !> \param ncol Argument used by catchem_ccpp_context_initialize.
   !> \param nlev Argument used by catchem_ccpp_context_initialize.
   !> \param status Argument used by catchem_ccpp_context_initialize.
   subroutine catchem_ccpp_context_initialize(handle, ncol, nlev, status)
      type(ccpp_context_handle), intent(in) :: handle
      integer, intent(in) :: ncol, nlev
      integer(c_int), intent(out) :: status
      call ccpp_context_initialize_model(handle, ncol, nlev, status)
   end subroutine

   !> \brief Implements catchem ccpp context scheme init for the CATChem CCPP boundary.
   !> \param handle Argument used by catchem_ccpp_context_scheme_init.
   !> \param process_id One-based CATChem process participant ID (1 through 9).
   !> \param status Argument used by catchem_ccpp_context_scheme_init.
   subroutine catchem_ccpp_context_scheme_init(handle, process_id, status)
      type(ccpp_context_handle), intent(in) :: handle
      integer, intent(in) :: process_id
      integer(c_int), intent(out) :: status
      call ccpp_context_scheme_init(handle, process_id, status)
   end subroutine

   !> \brief Implements catchem ccpp context scheme final for the CATChem CCPP boundary.
   !> \param handle Argument used by catchem_ccpp_context_scheme_final.
   !> \param process_id One-based CATChem process participant ID (1 through 9).
   !> \param status Argument used by catchem_ccpp_context_scheme_final.
   subroutine catchem_ccpp_context_scheme_final(handle, process_id, status)
      type(ccpp_context_handle), intent(in) :: handle
      integer, intent(in) :: process_id
      integer(c_int), intent(out) :: status
      call ccpp_context_scheme_final(handle, process_id, status)
   end subroutine

   !> \brief Implements catchem ccpp context open interval for the CATChem CCPP boundary.
   !> \param handle Argument used by catchem_ccpp_context_open_interval.
   !> \param token Argument used by catchem_ccpp_context_open_interval.
   !> \param status Argument used by catchem_ccpp_context_open_interval.
   subroutine catchem_ccpp_context_open_interval(handle, token, status)
      type(ccpp_context_handle), intent(in) :: handle
      integer(c_int64_t), intent(in) :: token
      integer(c_int), intent(out) :: status
      call ccpp_context_open_interval(handle, token, status)
   end subroutine

   !> \brief Implements catchem ccpp context close interval for the CATChem CCPP boundary.
   !> \param handle Argument used by catchem_ccpp_context_close_interval.
   !> \param token Argument used by catchem_ccpp_context_close_interval.
   !> \param status Argument used by catchem_ccpp_context_close_interval.
   subroutine catchem_ccpp_context_close_interval(handle, token, status)
      type(ccpp_context_handle), intent(in) :: handle
      integer(c_int64_t), intent(in) :: token
      integer(c_int), intent(out) :: status
      call ccpp_context_close_interval(handle, token, status)
   end subroutine

   !> \brief Implements catchem ccpp context species for the CATChem CCPP boundary.
   !> \param handle Argument used by catchem_ccpp_context_species.
   !> \param index Argument used by catchem_ccpp_context_species.
   !> \param descriptor Argument used by catchem_ccpp_context_species.
   !> \param name Argument used by catchem_ccpp_context_species.
   !> \param standard_name Argument used by catchem_ccpp_context_species.
   !> \param units Argument used by catchem_ccpp_context_species.
   !> \param status Argument used by catchem_ccpp_context_species.
   subroutine catchem_ccpp_context_species(handle, index, descriptor, name, standard_name, units, status)
      type(ccpp_context_handle), intent(in) :: handle
      integer, intent(in) :: index
      type(CATChem_Manifest_Species_v1), intent(inout) :: descriptor
      character(len=*), intent(out) :: name, standard_name, units
      integer(c_int), intent(out) :: status
      call ccpp_context_get_manifest_species(handle, index, descriptor, name, standard_name, units, status)
   end subroutine

   !> \brief Implements catchem ccpp context model for the CATChem CCPP boundary.
   !> \param handle Argument used by catchem_ccpp_context_model.
   !> \param model Argument used by catchem_ccpp_context_model.
   !> \param status Argument used by catchem_ccpp_context_model.
   subroutine catchem_ccpp_context_model(handle, model, status)
      type(ccpp_context_handle), intent(in) :: handle
      type(CATChem_Model), pointer, intent(out) :: model
      integer(c_int), intent(out) :: status
      call ccpp_context_get_model(handle, model, status)
   end subroutine

   !> \brief Implements catchem ccpp context begin exchange for the CATChem CCPP boundary.
   !> \param handle Argument used by catchem_ccpp_context_begin_exchange.
   !> \param state Argument used by catchem_ccpp_context_begin_exchange.
   !> \param status Argument used by catchem_ccpp_context_begin_exchange.
   subroutine catchem_ccpp_context_begin_exchange(handle, state, status)
      type(ccpp_context_handle), intent(in) :: handle
      real(c_double), intent(in) :: state(:, :, :)
      integer(c_int), intent(out) :: status
      call ccpp_context_begin_exchange(handle, state, status)
   end subroutine

   !> \brief Implements catchem ccpp context stage for the CATChem CCPP boundary.
   !> \param handle Argument used by catchem_ccpp_context_stage.
   !> \param candidate Argument used by catchem_ccpp_context_stage.
   !> \param status Argument used by catchem_ccpp_context_stage.
   subroutine catchem_ccpp_context_stage(handle, candidate, status)
      type(ccpp_context_handle), intent(in) :: handle
      real(c_double), intent(in) :: candidate(:, :, :)
      integer(c_int), intent(out) :: status
      call ccpp_context_stage_publication(handle, candidate, status)
   end subroutine

   !> \brief Implements catchem ccpp context commit for the CATChem CCPP boundary.
   !> \param handle Argument used by catchem_ccpp_context_commit.
   !> \param state Argument used by catchem_ccpp_context_commit.
   !> \param status Argument used by catchem_ccpp_context_commit.
   subroutine catchem_ccpp_context_commit(handle, state, status)
      type(ccpp_context_handle), intent(in) :: handle
      real(c_double), intent(inout) :: state(:, :, :)
      integer(c_int), intent(out) :: status
      call ccpp_context_commit_publication(handle, state, status)
   end subroutine

   !> \brief Implements catchem ccpp context cancel for the CATChem CCPP boundary.
   !> \param handle Argument used by catchem_ccpp_context_cancel.
   !> \param status Argument used by catchem_ccpp_context_cancel.
   subroutine catchem_ccpp_context_cancel(handle, status)
      type(ccpp_context_handle), intent(in) :: handle
      integer(c_int), intent(out) :: status
      call ccpp_context_cancel_publication(handle, status)
   end subroutine

   !> \brief Implements catchem ccpp context buffers for the CATChem CCPP boundary.
   !> \param handle Argument used by catchem_ccpp_context_buffers.
   !> \param baseline Argument used by catchem_ccpp_context_buffers.
   !> \param working Argument used by catchem_ccpp_context_buffers.
   !> \param tendency Argument used by catchem_ccpp_context_buffers.
   !> \param status Argument used by catchem_ccpp_context_buffers.
   subroutine catchem_ccpp_context_buffers(handle, baseline, working, tendency, status)
      type(ccpp_context_handle), intent(in) :: handle
      real(c_double), pointer, contiguous, intent(out) :: baseline(:, :, :), working(:, :, :), tendency(:, :, :)
      integer(c_int), intent(out) :: status
      call ccpp_context_get_buffers(handle, baseline, working, tendency, status)
   end subroutine

   !> \brief Implements catchem ccpp context set mapping for the CATChem CCPP boundary.
   !> \param handle Argument used by catchem_ccpp_context_set_mapping.
   !> \param host_indices Argument used by catchem_ccpp_context_set_mapping.
   !> \param status Argument used by catchem_ccpp_context_set_mapping.
   !> \param host_registry_extent Number of slots in the full CCPP host registry.
   subroutine catchem_ccpp_context_set_mapping(handle, host_indices, status, host_registry_extent)
      type(ccpp_context_handle), intent(in) :: handle
      integer, intent(in) :: host_indices(:)
      integer(c_int), intent(out) :: status
      integer, optional, intent(in) :: host_registry_extent
      call ccpp_context_set_species_map(handle, host_indices, status, host_registry_extent)
   end subroutine

   !> \brief Implements catchem ccpp context get mapping for the CATChem CCPP boundary.
   !> \param handle Argument used by catchem_ccpp_context_get_mapping.
   !> \param host_indices Argument used by catchem_ccpp_context_get_mapping.
   !> \param status Argument used by catchem_ccpp_context_get_mapping.
   subroutine catchem_ccpp_context_get_mapping(handle, host_indices, status)
      type(ccpp_context_handle), intent(in) :: handle
      integer, allocatable, intent(out) :: host_indices(:)
      integer(c_int), intent(out) :: status
      call ccpp_context_get_species_map(handle, host_indices, status)
   end subroutine

   !> \brief Implements catchem ccpp context stage tendency for the CATChem CCPP boundary.
   !> \param handle Argument used by catchem_ccpp_context_stage_tendency.
   !> \param token Argument used by catchem_ccpp_context_stage_tendency.
   !> \param contribution Argument used by catchem_ccpp_context_stage_tendency.
   !> \param status Argument used by catchem_ccpp_context_stage_tendency.
   subroutine catchem_ccpp_context_stage_tendency(handle, token, contribution, status)
      type(ccpp_context_handle), intent(in) :: handle
      integer(c_int64_t), intent(in) :: token
      real(c_double), intent(in) :: contribution(:, :, :)
      integer(c_int), intent(out) :: status
      call ccpp_context_stage_tendency_registry(handle, token, contribution, status)
   end subroutine

   !> \brief Implements catchem ccpp context apply tendency for the CATChem CCPP boundary.
   !> \param handle Argument used by catchem_ccpp_context_apply_tendency.
   !> \param token Argument used by catchem_ccpp_context_apply_tendency.
   !> \param duration_seconds Argument used by catchem_ccpp_context_apply_tendency.
   !> \param host_tendency Argument used by catchem_ccpp_context_apply_tendency.
   !> \param status Argument used by catchem_ccpp_context_apply_tendency.
   subroutine catchem_ccpp_context_apply_tendency(handle, token, duration_seconds, host_tendency, status)
      type(ccpp_context_handle), intent(in) :: handle
      integer(c_int64_t), intent(in) :: token
      real(c_double), intent(in) :: duration_seconds
      real(c_double), intent(inout) :: host_tendency(:, :, :)
      integer(c_int), intent(out) :: status
      call ccpp_context_apply_tendency_registry(handle, token, duration_seconds, host_tendency, status)
   end subroutine

   !> \brief Implements catchem ccpp context cancel tendency for the CATChem CCPP boundary.
   !> \param handle Argument used by catchem_ccpp_context_cancel_tendency.
   !> \param token Argument used by catchem_ccpp_context_cancel_tendency.
   !> \param status Argument used by catchem_ccpp_context_cancel_tendency.
   subroutine catchem_ccpp_context_cancel_tendency(handle, token, status)
      type(ccpp_context_handle), intent(in) :: handle
      integer(c_int64_t), intent(in) :: token
      integer(c_int), intent(out) :: status
      call ccpp_context_cancel_tendency_registry(handle, token, status)
   end subroutine
end module catchem_ccpp_context

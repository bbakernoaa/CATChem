!> Exercises immutable registry-order mappings and whole-binding invalidation.
module test_ccpp_constituent_mapping
   use iso_c_binding, only: c_int
   use CATChem_Context_Registry, only: ccpp_context_handle, ccpp_context_set_species_map, &
                                       ccpp_context_get_species_map, CCPP_CTX_OK, CCPP_CTX_INVALID
   implicit none
   private
   public :: run_registry_mapping_cases
contains
   !> \brief Verify arbitrary ordering, duplicate rejection, and stale-map invalidation.
   !> \param handle Initialized context used by the mapping contract test.
   !> \param passed Set true only if every mapping assertion succeeds.
   subroutine run_registry_mapping_cases(handle, passed)
      type(ccpp_context_handle), intent(in) :: handle
      logical, intent(out) :: passed
      integer, allocatable :: slots(:)
      integer(c_int) :: status

      passed = .false.
      call ccpp_context_set_species_map(handle, [2, 1], status)
      if (status /= CCPP_CTX_OK) return
      call ccpp_context_get_species_map(handle, slots, status)
      if (status /= CCPP_CTX_OK .or. size(slots) /= 2 .or. any(slots /= [2, 1])) return

      ! A duplicate slot is a malformed replacement registry and retires the
      ! previous map instead of leaving an earlier binding usable.
      call ccpp_context_set_species_map(handle, [1, 1], status)
      if (status /= CCPP_CTX_INVALID) return
      call ccpp_context_get_species_map(handle, slots, status)
      if (status /= CCPP_CTX_INVALID) return

      call ccpp_context_set_species_map(handle, [3, 1], status)
      if (status /= CCPP_CTX_INVALID) return
      call ccpp_context_get_species_map(handle, slots, status)
      if (status /= CCPP_CTX_INVALID) return

      ! CCPP may append unrelated or nonadvected host tracers beyond CATChem's
      ! private species extent. The adapter supplies the actual host registry
      ! extent, so a valid later host slot remains mappable.
      call ccpp_context_set_species_map(handle, [3, 1], status, 3)
      if (status /= CCPP_CTX_OK) return
      call ccpp_context_get_species_map(handle, slots, status)
      if (status /= CCPP_CTX_OK .or. any(slots /= [3, 1])) return
      call ccpp_context_set_species_map(handle, [4, 1], status, 3)
      if (status /= CCPP_CTX_INVALID) return
      call ccpp_context_get_species_map(handle, slots, status)
      if (status /= CCPP_CTX_INVALID) return

      call ccpp_context_set_species_map(handle, [1, 1], status)
      if (status /= CCPP_CTX_INVALID) return
      call ccpp_context_get_species_map(handle, slots, status)
      if (status /= CCPP_CTX_INVALID) return

      call ccpp_context_set_species_map(handle, [2, 1], status)
      if (status /= CCPP_CTX_OK) return
      call ccpp_context_set_species_map(handle, [1, 2], status)
      if (status /= CCPP_CTX_INVALID) return
      call ccpp_context_get_species_map(handle, slots, status)
      if (status /= CCPP_CTX_INVALID) return
      passed = .true.
   end subroutine run_registry_mapping_cases
end module test_ccpp_constituent_mapping

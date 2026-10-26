!> Adapter property-level CCPP registry conformance cases.
module test_ccpp_property_mapping
   use iso_c_binding, only: c_double, c_int, c_int64_t
   use machine, only: kind_phys
   use ccpp_constituent_prop_mod, only: ccpp_constituent_properties_t, ccpp_constituent_prop_ptr_t
   use catchem_ccpp_context, only: ccpp_context_handle, catchem_ccpp_context_species_count
   use catchem_ccpp_adapter, only: catchem_ccpp_register_constituents, catchem_ccpp_build_mapping, &
                                   catchem_ccpp_gather, catchem_ccpp_convert_to_host
   use CATChem_Context_Registry, only: ccpp_context_get_species_map, CCPP_CTX_OK, CCPP_CTX_INVALID
   implicit none
   private
   public :: run_ccpp_property_mapping_cases
contains
   !> Exercise the adapter against real CCPP constituent property objects.
  !! @param handle Initialized manifest context.
  !! @param passed True only when permutation and conflict cases pass.
   subroutine run_ccpp_property_mapping_cases(handle, passed)
      type(ccpp_context_handle), intent(in) :: handle
      logical, intent(out) :: passed
      type(ccpp_constituent_properties_t), allocatable, target :: properties(:)
      type(ccpp_constituent_prop_ptr_t), allocatable :: property_views(:)
      type(ccpp_constituent_properties_t), pointer :: property_pointer
      integer, allocatable :: mapped_slots(:), current_slots(:)
      type(ccpp_constituent_prop_ptr_t), allocatable :: missing_property_views(:)
      real(kind_phys), allocatable, target :: host_backing(:, :, :)
      real(kind_phys), pointer :: host_view(:, :, :)
      real(kind_phys), allocatable :: host_density(:, :), dry_density(:, :), host_candidate(:, :, :), host_candidate_before(:, :, :)
      real(kind_phys), allocatable :: host_number_density(:, :), dry_number_density(:, :)
      real(c_double), allocatable :: canonical_forward(:, :, :), canonical_reverse(:, :, :)
      integer(c_int) :: status
      integer :: species_count, errflg, i, expected_slot, property_status, slot
      character(len=1024) :: errmsg, registered_name, registered_units

      passed = .false.
      call catchem_ccpp_context_species_count(handle, species_count, status)
      if (status /= CCPP_CTX_OK .or. species_count <= 0) then
         print *, 'property mapping: invalid manifest count', status, species_count
         return
      end if
      allocate (properties(species_count + 1), property_views(species_count + 1))
      call catchem_ccpp_register_constituents(handle, properties(1:species_count), errmsg, errflg)
      if (errflg /= 0) then
         print *, 'property mapping: registration failed', errflg, trim(errmsg)
         return
      end if
      do i = 1, species_count
         call properties(i)%set_const_index(species_count - i + 1, errflg, errmsg)
         if (errflg /= 0) then
            print *, 'property mapping: setting index failed', i, errflg, trim(errmsg)
            return
         end if
         property_pointer => properties(i)
         call property_views(i)%set(property_pointer, errflg, errmsg)
         if (errflg /= 0) then
            print *, 'property mapping: pointer setup failed', i, errflg, trim(errmsg)
            return
         end if
      end do
      call properties(species_count + 1)%instantiate(std_name='unrelated_host_tracer', &
                                            long_name='unrelated host tracer', diag_name='unrelated_host_tracer', units='kg kg-1', &
                                           vertical_dim='vertical_layer_dimension', advected=.false., default_value=0.0_kind_phys, &
                                                    min_value=0.0_kind_phys, mixing_ratio_type='dry', errcode=errflg, errmsg=errmsg)
      if (errflg /= 0) return
      call properties(species_count + 1)%set_const_index(species_count + 1, errflg, errmsg)
      if (errflg /= 0) return
      property_pointer => properties(species_count + 1)
      call property_views(species_count + 1)%set(property_pointer, errflg, errmsg)
      if (errflg /= 0) return

      call catchem_ccpp_build_mapping(handle, property_views, mapped_slots, errmsg, errflg)
      if (errflg /= 0) then
         print *, 'property mapping: adapter rejected permutation', errflg, trim(errmsg)
         return
      end if
      if (.not. allocated(mapped_slots)) then
         print *, 'property mapping: result was not allocated'
         return
      end if
      if (size(mapped_slots) /= species_count) then
         print *, 'property mapping: wrong result extent', size(mapped_slots), species_count
         return
      end if
      do i = 1, species_count
         expected_slot = species_count - i + 1
         if (mapped_slots(i) /= expected_slot) then
            print *, 'property mapping: slot mismatch', i, mapped_slots(i), expected_slot
            return
         end if
      end do

      ! Exercise the real adapter gather with a noncontiguous constituent view,
      ! keeping species selected through their registered CCPP physical indices.
      allocate (host_backing(1, 4, 2*(species_count + 1)), host_density(1, 4), dry_density(1, 4), &
                host_number_density(1, 4), dry_number_density(1, 4), &
                canonical_forward(1, 4, species_count), canonical_reverse(1, 4, species_count), &
                host_candidate(1, 4, species_count + 1), host_candidate_before(1, 4, species_count + 1))
      host_view => host_backing(:, :, 1:2*(species_count + 1):2)
      host_view = 0.0_kind_phys
      do i = 1, species_count
         slot = mapped_slots(i)
         host_view(:, 1, slot) = real(i, kind_phys)*1.0e-9_kind_phys
         host_view(:, 2, slot) = real(2*i, kind_phys)*1.0e-9_kind_phys
         host_view(:, 3, slot) = real(3*i, kind_phys)*1.0e-9_kind_phys
         host_view(:, 4, slot) = real(4*i, kind_phys)*1.0e-9_kind_phys
      end do
      host_density = 1.0_kind_phys
      dry_density = 1.0_kind_phys
      host_number_density = 1.0_kind_phys
      dry_number_density = 1.0_kind_phys
      call catchem_ccpp_gather(handle, mapped_slots, host_view, host_density, dry_density, &
                               host_number_density, dry_number_density, canonical_forward, errmsg, errflg)
      if (errflg /= 0) then
         print *, 'property mapping: adapter gather rejected noncontiguous view', errflg, trim(errmsg)
         return
      end if
      call ccpp_context_get_species_map(handle, current_slots, status)
      if (status /= CCPP_CTX_OK .or. any(current_slots /= mapped_slots)) then
         print *, 'property mapping: context did not retain permutation', status
         return
      end if

      ! Force the final mapped constituent's physical output conversion to fail.
      ! The adapter must not expose earlier successfully converted species.
      canonical_forward = 0.0_c_double
      canonical_forward(:, :, species_count) = -1.0_c_double
      host_candidate = 17.0_kind_phys
      host_candidate_before = host_candidate
      call catchem_ccpp_convert_to_host(handle, mapped_slots, canonical_forward, host_density, dry_density, &
                                        host_number_density, dry_number_density, host_candidate, errmsg, errflg)
      if (errflg == 0 .or. .not. same_bits_3d(host_candidate, host_candidate_before)) then
         print *, 'property mapping: final species conversion failure published a partial host candidate'
         return
      end if

      ! Omitting a required species fails closed and invalidates the whole map.
      allocate (missing_property_views(species_count - 1))
      missing_property_views = property_views(1:species_count - 1)
      call catchem_ccpp_build_mapping(handle, missing_property_views, mapped_slots, errmsg, errflg)
      if (errflg == 0) then
         print *, 'property mapping: missing required species was accepted'
         return
      end if
      call ccpp_context_get_species_map(handle, current_slots, status)
      if (status /= CCPP_CTX_INVALID .or. allocated(current_slots)) then
         print *, 'property mapping: missing species retained a stale map', status
         return
      end if
      call catchem_ccpp_build_mapping(handle, property_views, mapped_slots, errmsg, errflg)
      if (errflg /= 0) then
         print *, 'property mapping: valid registry did not recover after failure', errflg, trim(errmsg)
         return
      end if

      ! A conflicting full property signature must retire the just-installed map.
      call properties(1)%standard_name(registered_name, property_status, errmsg)
      if (property_status /= 0) return
      call properties(1)%units(registered_units, property_status, errmsg)
      if (property_status /= 0) return
      call properties(1)%instantiate(std_name=trim(registered_name), long_name='conflicting long name', &
                     diag_name='conflict', units=trim(registered_units), vertical_dim='vertical_layer_dimension', advected=.true., &
                                     default_value=0.0_kind_phys, min_value=0.0_kind_phys, mixing_ratio_type='dry', &
                                     errcode=errflg, errmsg=errmsg)
      if (errflg /= 0) return
      call catchem_ccpp_build_mapping(handle, property_views, mapped_slots, errmsg, errflg)
      if (errflg == 0) return
      call ccpp_context_get_species_map(handle, current_slots, status)
      if (status /= CCPP_CTX_INVALID .or. allocated(current_slots)) return
      passed = .true.
   end subroutine run_ccpp_property_mapping_cases

   !> Compare mapped canonical values bit for bit.
  !! @param left First mapped array.
  !! @param right Second mapped array.
  !! @return True when their binary64 values are identical.
   pure logical function same_bits(left, right)
      real(c_double), intent(in) :: left(:, :), right(:, :)
      same_bits = all(transfer(left, [0_c_int64_t], size(left)) == &
                      transfer(right, [0_c_int64_t], size(right)))
   end function same_bits

   !> Compare full host candidate arrays bit for bit.
  !! @param left First candidate array.
  !! @param right Candidate array expected to remain untouched.
  !! @return True when every binary64 element matches.
   pure logical function same_bits_3d(left, right)
      real(kind_phys), intent(in) :: left(:, :, :), right(:, :, :)
      same_bits_3d = all(transfer(left, [0_c_int64_t], size(left)) == &
                         transfer(right, [0_c_int64_t], size(right)))
   end function same_bits_3d

end module test_ccpp_property_mapping

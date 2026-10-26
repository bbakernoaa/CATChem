! CCPP update interstitial: apply this context's successful tendency once.
module catchem_apply_tendency
   use iso_c_binding, only: c_int, c_int64_t, c_double
   use iso_fortran_env, only: output_unit
   use machine, only: kind_phys
   use catchem_ccpp_context, only: ccpp_context_handle, CATChem_Manifest_Species_v1
   use catchem_ccpp_context, only: catchem_ccpp_context_get_mapping, catchem_ccpp_context_species
   use catchem_ccpp_adapter, only: catchem_ccpp_apply_tendency
   use ccpp_host_constituents, only: ccpp_model_constituents_obj
   implicit none
   private
   public :: catchem_apply_tendency_run
   !> Monotonic identifier for full-array snapshots emitted during one SCM run.
   integer, save :: full_state_update_sequence = 0
contains
   !> \brief Implements catchem apply tendency for the CATChem CCPP boundary.
   !> \param context_id Argument used by catchem_apply_tendency.
   !> \param context_generation Argument used by catchem_apply_tendency.
   !> \param interval_token Argument used by catchem_apply_tendency.
   !> \param duration_seconds Argument used by catchem_apply_tendency.
   !> \param constituents Full CCPP constituent state supplied by the host.
   !> \param constituent_tendencies Host tendency array modified by the successful CATChem contribution.
   !> \param errmsg Error detail on failure.
   !> \param errflg CCPP error code, zero on success.
   subroutine catchem_apply_tendency_run(context_id, context_generation, interval_token, &
                                         loop_counter, loop_extent, duration_seconds, &
                                         constituents, constituent_tendencies, errmsg, errflg)
      integer(c_int), intent(in) :: context_id
      integer(c_int64_t), intent(in) :: context_generation, interval_token
      integer, intent(in) :: loop_counter, loop_extent
      real(kind_phys), intent(in) :: duration_seconds
      real(kind_phys), intent(inout) :: constituents(:, :, :), constituent_tendencies(:, :, :)
      character(len=*), intent(out) :: errmsg
      integer, intent(out) :: errflg
      type(ccpp_context_handle) :: handle
      integer, allocatable :: host_species_index(:)
      real(kind_phys), allocatable :: state_before(:, :, :)
      real(kind_phys), allocatable :: full_state_before(:, :, :)
      real(kind_phys) :: max_increment, invocation_duration
      integer(c_int) :: status
      integer :: trace_status
      character(len=8) :: trace_switch
      if (loop_counter < 1 .or. loop_extent < 1 .or. loop_counter > loop_extent) then
         errmsg = 'CCPP subcycle counter and extent are invalid.'
         errflg = 1
         return
      end if
      invocation_duration = duration_seconds/real(loop_extent, kind_phys)
      handle%id = context_id
      handle%generation = context_generation
      call catchem_ccpp_context_get_mapping(handle, host_species_index, status)
      if (status /= 0_c_int) then
         errmsg = 'CATChem update scheme has no context-local constituent map.'
         errflg = int(status)
         return
      end if
      allocate (state_before(size(constituents, 1), size(constituents, 2), size(host_species_index)))
      state_before = constituents(:, :, host_species_index)
      call get_environment_variable('CATCHEM_BOUNDARY_LEDGER', trace_switch, status=trace_status)
      if (trace_status == 0 .and. trim(trace_switch) == '1') then
         allocate (full_state_before(size(constituents, 1), size(constituents, 2), size(constituents, 3)))
         full_state_before = constituents
      end if
      call catchem_ccpp_apply_tendency(handle, interval_token, real(invocation_duration, c_double), &
                                       host_species_index, constituents, constituent_tendencies, errmsg, errflg)
      if (errflg == 0) then
         max_increment = maxval(abs(real(constituents(:, :, host_species_index) - state_before, c_double)))
         write (output_unit, '(a,i0,a,i0,a,i0,a,i0,a,es24.16)') &
            'CATCHEM_HOST_UPDATE interval=', interval_token, ' contribution_token=', interval_token, &
            ' subcycle=', loop_counter, ' mapped_species=', size(host_species_index), &
            ' duration_seconds=', real(invocation_duration, c_double)
         write (output_unit, '(a,es24.16)') 'CATCHEM_HOST_STATE_APPLIED MAXABS_INCREMENT=', max_increment
         if (allocated(full_state_before)) then
            call write_boundary_ledger(handle, interval_token, loop_counter, invocation_duration, &
                                       host_species_index, state_before, constituents)
            call write_full_constituent_ledger(interval_token, loop_counter, full_state_before, constituents)
         end if
      end if
   end subroutine catchem_apply_tendency_run

   !> \brief Write mapped CCPP state immediately before and after a successful CATChem update.
   !> \param handle The context whose manifest defines the mapped CATChem species.
   !> \param token The host interval identity for this contribution.
   !> \param subcycle The one-based subcycle index within the host interval.
   !> \param duration The duration in seconds consumed by this update.
   !> \param host_species_index One-based CCPP constituent slots in manifest order.
   !> \param before The mapped CCPP state captured before CATChem publication.
   !> \param constituents The full CCPP state after CATChem publication.
   subroutine write_boundary_ledger(handle, token, subcycle, duration, host_species_index, before, constituents)
      type(ccpp_context_handle), intent(in) :: handle
      integer(c_int64_t), intent(in) :: token
      integer, intent(in) :: subcycle, host_species_index(:)
      real(kind_phys), intent(in) :: duration, before(:, :, :), constituents(:, :, :)
      type(CATChem_Manifest_Species_v1) :: descriptor
      character(len=256) :: species_name, standard_name, units, host_units
      integer(c_int) :: status
      integer :: species, column, level, host_slot

      write (output_unit, '(a,i0,a,i0,a,es24.16)') 'CATCHEM_BOUNDARY_BEGIN interval=', token, &
         ' subcycle=', subcycle, ' duration_seconds=', real(duration, c_double)
      do species = 1, size(host_species_index)
         call catchem_ccpp_context_species(handle, species, descriptor, species_name, standard_name, units, status)
         if (status /= 0_c_int) then
            write (output_unit, '(a,i0,a,i0)') 'CATCHEM_BOUNDARY_METADATA_ERROR species_index=', species, &
               ' status=', status
            return
         end if
         host_units = trim(units)
         ! Empty legacy YAML units inherit the context's default dry-air mass
         ! mixing-ratio representation at this CCPP host boundary.
         if (len_trim(host_units) == 0 .and. descriptor%amount_type == 2) host_units = 'kg kg-1'
         host_slot = host_species_index(species)
         do level = 1, size(before, 2)
            do column = 1, size(before, 1)
               write (output_unit, '(a,i0,a,i0,a,a,a,i0,a,i0,a,es24.16,a,es24.16,a,a)') &
                  'CATCHEM_BOUNDARY_VALUE interval=', token, ' subcycle=', subcycle, ' species=', trim(species_name), &
                  ' column=', column, ' level=', level, ' before=', real(before(column, level, species), c_double), &
                  ' after=', real(constituents(column, level, host_slot), c_double), ' units=', trim(host_units)
            end do
         end do
      end do
      write (output_unit, '(a,i0,a,i0)') 'CATCHEM_BOUNDARY_END interval=', token, ' subcycle=', subcycle
   end subroutine write_boundary_ledger

   !> Write the complete framework-owned CCPP constituent array at each host update.
  !! This includes constituents outside CATChem's manifest, including nonadvected host fields.
   !> @param token Host interval identity.
   !> @param subcycle One-based subcycle index within the interval.
   !> @param before Full constituent state before CATChem publication.
   !> @param after Full constituent state after CATChem publication.
   subroutine write_full_constituent_ledger(token, subcycle, before, after)
      integer(c_int64_t), intent(in) :: token
      integer, intent(in) :: subcycle
      real(kind_phys), intent(in) :: before(:, :, :), after(:, :, :)
      character(len=256) :: standard_name, diagnostic_name, units, message
      integer :: metadata_index, constituent_index, column, level, local_status, update_sequence

      if (full_state_update_sequence == huge(full_state_update_sequence)) then
         write (output_unit, '(a)') 'CATCHEM_FULL_STATE_METADATA_ERROR snapshot sequence is exhausted'
         return
      end if
      full_state_update_sequence = full_state_update_sequence + 1
      update_sequence = full_state_update_sequence

      if (.not. allocated(ccpp_model_constituents_obj)) then
         write (output_unit, '(a)') 'CATCHEM_FULL_STATE_METADATA_ERROR host constituent registry is unavailable'
         return
      end if
      if (size(ccpp_model_constituents_obj) /= 1) then
         write (output_unit, '(a)') 'CATCHEM_FULL_STATE_METADATA_ERROR host constituent registry is unavailable'
         return
      end if
      if (.not. allocated(ccpp_model_constituents_obj(1)%const_metadata)) then
         write (output_unit, '(a)') 'CATCHEM_FULL_STATE_METADATA_ERROR constituent properties are unavailable'
         return
      end if
      if (size(before, 3) /= size(after, 3)) then
         write (output_unit, '(a)') 'CATCHEM_FULL_STATE_METADATA_ERROR before/after state extents differ'
         return
      end if
      do metadata_index = 1, size(ccpp_model_constituents_obj(1)%const_metadata)
         call ccpp_model_constituents_obj(1)%const_metadata(metadata_index)%standard_name( &
            standard_name, local_status, message)
         if (local_status /= 0) then
            write (output_unit, '(a,i0,a,a)') 'CATCHEM_FULL_STATE_METADATA_ERROR index=', metadata_index, ' ', trim(message)
            return
         end if
         call ccpp_model_constituents_obj(1)%const_metadata(metadata_index)%diagnostic_name( &
            diagnostic_name, local_status, message)
         if (local_status /= 0) then
            write (output_unit, '(a,i0,a,a)') 'CATCHEM_FULL_STATE_METADATA_ERROR index=', metadata_index, ' ', trim(message)
            return
         end if
         call ccpp_model_constituents_obj(1)%const_metadata(metadata_index)%units(units, local_status, message)
         if (local_status /= 0) then
            write (output_unit, '(a,i0,a,a)') 'CATCHEM_FULL_STATE_METADATA_ERROR index=', metadata_index, ' ', trim(message)
            return
         end if
         call ccpp_model_constituents_obj(1)%const_metadata(metadata_index)%const_index( &
            constituent_index, local_status, message)
         if (local_status /= 0 .or. constituent_index < 1 .or. constituent_index > size(before, 3)) then
            write (output_unit, '(a,i0,a,a)') 'CATCHEM_FULL_STATE_METADATA_ERROR index=', metadata_index, ' ', trim(message)
            return
         end if
         do level = 1, size(before, 2)
            do column = 1, size(before, 1)
               write (output_unit, '(a,i0,a,i0,a,i0,a,a,a,a,a,i0,a,i0,a,es24.16,a,es24.16,a,a)') &
                  'CATCHEM_FULL_STATE_VALUE update_sequence=', update_sequence, ' interval=', token, &
                  ' subcycle=', subcycle, &
                  ' standard_name=', trim(standard_name), ' diagnostic_name=', trim(diagnostic_name), &
                  ' column=', column, ' level=', level, ' before=', real(before(column, level, constituent_index), c_double), &
                  ' after=', real(after(column, level, constituent_index), c_double), ' units=', trim(units)
            end do
         end do
      end do
   end subroutine write_full_constituent_ledger
end module catchem_apply_tendency

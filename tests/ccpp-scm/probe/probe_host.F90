program probe_host
   use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
   use ccpp_kinds, only: kind_phys
   use ccpp_constituent_prop_mod, only: ccpp_constituent_properties_t
   use ccpp_constituent_prop_mod, only: ccpp_constituent_prop_ptr_t
   use probe_host_data, only: ncols, pver, ninstances
   use probe_host_ccpp_cap, only: ccpp_register, ccpp_register_constituents
   use probe_host_ccpp_cap, only: ccpp_initialize_constituents, ccpp_number_constituents
   use probe_host_ccpp_cap, only: ccpp_const_get_index, ccpp_constituents_array
   use probe_host_ccpp_cap, only: ccpp_model_const_properties
   use probe_host_ccpp_cap, only: ccpp_init, ccpp_physics_init, ccpp_physics_run
   use probe_host_ccpp_cap, only: ccpp_physics_timestep_init, ccpp_physics_timestep_final
   use probe_host_ccpp_cap, only: ccpp_physics_final, ccpp_final
   use probe_host_ccpp_cap, only: ccpp_deallocate_dynamic_constituents
   implicit none

   character(len=*), parameter :: suite = 'constituent_probe_suite'
   integer, parameter :: host_property_count(ninstances) = [2, 2, 2]
   character(len=512) :: errmsg
   integer :: errcode, errflg, instance, count, idx_shared, idx_host, idx_scheme
   type(ccpp_constituent_properties_t), allocatable, target :: host_props(:, :)
   type(ccpp_constituent_prop_ptr_t), pointer :: prop_view(:)
   real(kind=kind_phys), pointer :: state(:, :, :)
   character(len=128) :: prop_name

   ! Two successful registries deliberately reverse host slot order.  A third
   ! registry submits an incompatible duplicate and must fail during merge.
   do instance = 1, ninstances
      call ccpp_register(suite_name=suite, instance=instance, &
         ninstances=ninstances, errmsg=errmsg, errflg=errflg)
      call check_lifecycle('ccpp_register')
   end do
   do instance = 1, 2
      call make_host_properties(instance, conflict=.false.)
      call ccpp_register_constituents(host_constituents=host_props(1:host_property_count(instance), instance), &
         instance=instance, ninstances=ninstances, errmsg=errmsg, errcode=errcode)
      call check('ccpp_register_constituents valid instance')
   end do
   call make_host_properties(3, conflict=.true.)
   call ccpp_register_constituents(host_constituents=host_props(1:host_property_count(3), 3), &
      instance=3, ninstances=ninstances, errmsg=errmsg, errcode=errcode)
   if (errcode == 0 .or. len_trim(errmsg) == 0) then
      write(error_unit, '(a)') 'FAIL: conflicting duplicate properties were accepted'
      stop 1
   end if

   do instance = 1, 2
      call ccpp_initialize_constituents(ncols=ncols, num_layers=pver, &
         instance=instance, errmsg=errmsg, errcode=errcode)
      call check('ccpp_initialize_constituents')
      call ccpp_number_constituents(num_flds=count, instance=instance, &
         errmsg=errmsg, errcode=errcode)
      call check('ccpp_number_constituents')
      if (count /= 3) error stop 'expected host tracer plus two scheme constituents'
      call ccpp_const_get_index(stdname='probe_shared_tracer', &
         const_index=idx_shared, instance=instance, errmsg=errmsg, errcode=errcode)
      call check('lookup shared tracer')
      call ccpp_const_get_index(stdname='probe_host_tracer', &
         const_index=idx_host, instance=instance, errmsg=errmsg, errcode=errcode)
      call check('lookup host tracer')
      call ccpp_const_get_index(stdname='probe_scheme_tracer', &
         const_index=idx_scheme, instance=instance, errmsg=errmsg, errcode=errcode)
      call check('lookup scheme tracer')
      if (idx_shared == idx_scheme .or. idx_host == idx_scheme) error stop 'duplicate index'
      if (instance == 1) write(output_unit, '(a)') 'HOST_INPUT_ORDER 1 shared,host'
      if (instance == 2) write(output_unit, '(a)') 'HOST_INPUT_ORDER 2 host,shared'
      write(output_unit, '(a,i0,a,4(i0,1x))') 'INSTANCE_RESULT ', instance, &
         ' count/shared/host/scheme=', count, idx_shared, idx_host, idx_scheme

      ! Framework hash traversal defines final indices; resolve by each
      ! instance's property view rather than assuming host declaration order.
      prop_view => ccpp_model_const_properties(instance)
      call prop_view(idx_shared)%standard_name(prop_name, errcode=errcode, errmsg=errmsg)
      call check('shared property lookup')
      if (trim(prop_name) /= 'probe_shared_tracer') error stop 'shared index/property mismatch'
      call prop_view(idx_host)%standard_name(prop_name, errcode=errcode, errmsg=errmsg)
      call check('host property lookup')
      if (trim(prop_name) /= 'probe_host_tracer') error stop 'host index/property mismatch'
      call prop_view(idx_scheme)%standard_name(prop_name, errcode=errcode, errmsg=errmsg)
      call check('scheme property lookup')
      if (trim(prop_name) /= 'probe_scheme_tracer') error stop 'scheme index/property mismatch'

      state => ccpp_constituents_array(instance)
      if (size(state, 1) /= ncols .or. size(state, 2) /= pver .or. &
         size(state, 3) /= count) error stop 'full constituent state extent mismatch'
      state = real(instance, kind_phys)
      call run_lifecycle(instance)
   end do

   do instance = 1, 2
      call ccpp_final(suite_name=suite, instance=instance, ninstances=ninstances, &
         errmsg=errmsg, errflg=errflg)
      call check_lifecycle('ccpp_final')
      call ccpp_deallocate_dynamic_constituents(instance=instance)
   end do
   call ccpp_deallocate_dynamic_constituents(instance=3)
   deallocate(host_props)
   write(output_unit, '(a)') 'PASS: merge, conflict rejection, slot order, full arrays, two instances'

contains
   subroutine make_host_properties(which, conflict)
      integer, intent(in) :: which
      logical, intent(in) :: conflict
      if (.not. allocated(host_props)) allocate(host_props(2, ninstances))
      if (which == 1) then
         call add_shared(host_props(1, which), conflict)
         call add_host(host_props(2, which))
      else
         call add_host(host_props(1, which))
         call add_shared(host_props(2, which), conflict)
      end if
   end subroutine make_host_properties

   subroutine add_shared(prop, conflict)
      type(ccpp_constituent_properties_t), intent(inout) :: prop
      logical, intent(in) :: conflict
      integer :: status
      character(len=512) :: message
      if (conflict) then
         call prop%instantiate(std_name='probe_shared_tracer', &
            long_name='Shared host and scheme probe tracer', diag_name='P_SHARED', &
            units='mol mol-1', vertical_dim='vertical_layer_dimension', &
            advected=.true., default_value=0.0_kind_phys, errcode=status, errmsg=message)
      else
         call prop%instantiate(std_name='probe_shared_tracer', &
            long_name='Shared host and scheme probe tracer', diag_name='P_SHARED', &
            units='kg kg-1', vertical_dim='vertical_layer_dimension', &
            advected=.true., default_value=0.0_kind_phys, errcode=status, errmsg=message)
      end if
      if (status /= 0) error stop 'failed to instantiate shared tracer'
   end subroutine add_shared

   subroutine add_host(prop)
      type(ccpp_constituent_properties_t), intent(inout) :: prop
      integer :: status
      character(len=512) :: message
      call prop%instantiate(std_name='probe_host_tracer', &
         long_name='Host-owned probe tracer', diag_name='P_HOST', &
         units='kg kg-1', vertical_dim='vertical_layer_dimension', &
         advected=.true., default_value=0.0_kind_phys, errcode=status, errmsg=message)
      if (status /= 0) error stop 'failed to instantiate host tracer'
   end subroutine add_host

   subroutine add_named_host(prop, std_name, diagnostic_name)
      type(ccpp_constituent_properties_t), intent(inout) :: prop
      character(len=*), intent(in) :: std_name, diagnostic_name
      integer :: status
      character(len=512) :: message
      call prop%instantiate(std_name=std_name, long_name='Additional host probe tracer', &
         diag_name=diagnostic_name, units='kg kg-1', &
         vertical_dim='vertical_layer_dimension', advected=.true., &
         default_value=0.0_kind_phys, errcode=status, errmsg=message)
      if (status /= 0) error stop 'failed to instantiate additional host tracer'
   end subroutine add_named_host

   subroutine run_lifecycle(which)
      integer, intent(in) :: which
      call ccpp_init(suite_name=suite, instance=which, ninstances=ninstances, &
         errmsg=errmsg, errflg=errflg)
      call check_lifecycle('ccpp_init')
      call ccpp_physics_init(suite_name=suite, group_name='physics', lb=1, ub=ncols, &
         thread_num=1, nthreads=1, nphys_threads=1, instance=which, &
         ninstances=ninstances, errmsg=errmsg, errflg=errflg)
      call check_lifecycle('ccpp_physics_init')
      call ccpp_physics_timestep_init(suite_name=suite, group_name='physics', &
         lb=1, ub=ncols, thread_num=1, nthreads=1, nphys_threads=1, &
         instance=which, ninstances=ninstances, errmsg=errmsg, errflg=errflg)
      call check_lifecycle('ccpp_physics_timestep_init')
      call ccpp_physics_run(suite_name=suite, group_name='physics', lb=1, ub=ncols, &
         thread_num=1, nthreads=1, nphys_threads=1, instance=which, &
         ninstances=ninstances, errmsg=errmsg, errflg=errflg)
      call check_lifecycle('ccpp_physics_run')
      call ccpp_physics_timestep_final(suite_name=suite, group_name='physics', &
         lb=1, ub=ncols, thread_num=1, nthreads=1, nphys_threads=1, &
         instance=which, ninstances=ninstances, errmsg=errmsg, errflg=errflg)
      call check_lifecycle('ccpp_physics_timestep_final')
      call ccpp_physics_final(suite_name=suite, group_name='physics', lb=1, ub=ncols, &
         thread_num=1, nthreads=1, nphys_threads=1, instance=which, &
         ninstances=ninstances, errmsg=errmsg, errflg=errflg)
      call check_lifecycle('ccpp_physics_final')
   end subroutine run_lifecycle

   subroutine check(phase)
      character(len=*), intent(in) :: phase
      if (errcode /= 0) then
         write(error_unit, '(3a)') trim(phase), ': ', trim(errmsg)
         stop 1
      end if
   end subroutine check
   subroutine check_lifecycle(phase)
      character(len=*), intent(in) :: phase
      if (errflg /= 0) then
         write(error_unit, '(3a)') trim(phase), ': ', trim(errmsg)
         stop 1
      end if
   end subroutine check_lifecycle
end program probe_host

! Small real-framework scheme used by the G0 constituent probe.
module constituent_probe
   use ccpp_kinds, only: kind_phys
   use ccpp_constituent_prop_mod, only: ccpp_constituent_properties_t
   use ccpp_constituent_prop_mod, only: ccpp_constituent_prop_ptr_t
   implicit none
   private
   public :: constituent_probe_register, constituent_probe_run
contains
   subroutine constituent_probe_register(dyn_const, errmsg, errcode)
      type(ccpp_constituent_properties_t), allocatable, intent(out) :: dyn_const(:)
      character(len=*), intent(out) :: errmsg
      integer, intent(out) :: errcode

      allocate(dyn_const(2), stat=errcode)
      errmsg = ''
      if (errcode /= 0) then
         errmsg = 'constituent_probe_register: allocation failed'
         return
      end if
      call dyn_const(1)%instantiate(std_name='probe_shared_tracer', &
         long_name='Shared host and scheme probe tracer', diag_name='P_SHARED', &
         units='kg kg-1', vertical_dim='vertical_layer_dimension', &
         advected=.true., default_value=0.0_kind_phys, &
         errcode=errcode, errmsg=errmsg)
      if (errcode /= 0) return
      call dyn_const(2)%instantiate(std_name='probe_scheme_tracer', &
         long_name='Scheme registered probe tracer', diag_name='P_SCHEME', &
         units='kg kg-1', vertical_dim='vertical_layer_dimension', &
         advected=.true., default_value=0.0_kind_phys, &
         errcode=errcode, errmsg=errmsg)
   end subroutine constituent_probe_register

   subroutine constituent_probe_run(ccpp_constituents, ccpp_constituent_tendencies, &
      ccpp_constituent_properties, probe_count, errmsg, errcode)
      real(kind=kind_phys), intent(inout) :: ccpp_constituents(:, :, :)
      real(kind=kind_phys), intent(inout) :: ccpp_constituent_tendencies(:, :, :)
      type(ccpp_constituent_prop_ptr_t), intent(in) :: ccpp_constituent_properties(:)
      integer, intent(in) :: probe_count
      character(len=*), intent(out) :: errmsg
      integer, intent(out) :: errcode
      integer :: index, property_errcode
      character(len=128) :: property_name
      character(len=512) :: property_errmsg
      logical :: found_host, found_shared, found_scheme

      errmsg = ''
      errcode = 0
      if (size(ccpp_constituents, 3) /= probe_count .or. &
         size(ccpp_constituent_tendencies, 3) /= probe_count .or. &
         size(ccpp_constituent_properties) /= probe_count) then
         errcode = 1
         errmsg = 'full constituent state, tendency, and property extents disagree'
         return
      end if
      found_host = .false.
      found_shared = .false.
      found_scheme = .false.
      do index = 1, probe_count
         call ccpp_constituent_properties(index)%standard_name(property_name, &
            errcode=property_errcode, errmsg=property_errmsg)
         if (property_errcode /= 0 .or. len_trim(property_name) == 0) then
            errcode = 1
            errmsg = 'invalid entry in full constituent property array: ' // &
               trim(property_errmsg)
            return
         end if
         select case (trim(property_name))
          case ('probe_host_tracer')
            found_host = .true.
          case ('probe_shared_tracer')
            found_shared = .true.
          case ('probe_scheme_tracer')
            found_scheme = .true.
         end select
      end do
      if (.not. (found_host .and. found_shared .and. found_scheme)) then
         errcode = 1
         errmsg = 'full property array is missing a registered probe tracer'
         return
      end if
      ! Touch every constituent column to prove the arrays are full-axis views.
      ccpp_constituent_tendencies = ccpp_constituent_tendencies + &
         0.0_kind_phys * ccpp_constituents
   end subroutine constituent_probe_run
end module constituent_probe

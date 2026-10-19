! Shared host interval lifecycle for all CATChem CCPP scheme instances.
module catchem_ccpp
  use iso_c_binding, only: c_double, c_int, c_int64_t
  use iso_fortran_env, only: output_unit
  use machine, only: kind_phys
  use catchem_bridge_precision, only: fp
  use CATChem_API, only: CATChem_Host_Interval_v1, CATChem_Model
  use catchem_ccpp_context, only: ccpp_context_handle, catchem_ccpp_context_open_interval, &
                                  catchem_ccpp_context_close_interval, catchem_ccpp_context_model, &
                                  CATCHEM_CONTEXT_SUCCESS
  implicit none
  private
  public :: catchem_ccpp_timestep_init, catchem_ccpp_timestep_final
  public :: catchem_ccpp_timestep_init_impl, catchem_ccpp_timestep_final_impl
contains
  !> \brief Implements catchem ccpp timestep init for the CATChem CCPP boundary.
  !> \param context_id Argument used by catchem_ccpp_timestep_init.
  !> \param context_generation Argument used by catchem_ccpp_timestep_init.
  !> \param interval_token Argument used by catchem_ccpp_timestep_init.
  !> \param year Host interval year.
  !> \param month Host interval month.
  !> \param day Host interval day of month.
  !> \param hour Host interval hour.
  !> \param minute Host interval minute.
  !> \param second Host interval second.
  !> \param day_of_year Host interval ordinal day.
  !> \param duration_seconds Host interval duration in seconds.
  !> \param errmsg Argument used by catchem_ccpp_timestep_init.
  !> \param errflg Argument used by catchem_ccpp_timestep_init.
  subroutine catchem_ccpp_timestep_init_impl(context_id, context_generation, interval_token, year, month, day, &
      hour, minute, second, day_of_year, duration_seconds, errmsg, errflg)
    integer(c_int), intent(in) :: context_id
    integer(c_int64_t), intent(in) :: context_generation, interval_token
    integer, intent(in) :: year, month, day, hour, minute, second, day_of_year
    real(kind_phys), intent(in) :: duration_seconds
    character(len=*), intent(out) :: errmsg
    integer, intent(out) :: errflg
    type(ccpp_context_handle) :: handle
    type(CATChem_Host_Interval_v1) :: interval
    type(CATChem_Model), pointer :: model
    integer(c_int) :: status
    handle%id = context_id
    handle%generation = context_generation
    errmsg = ''
    errflg = 0
    call catchem_ccpp_context_open_interval(handle, interval_token, status)
    if (status /= CATCHEM_CONTEXT_SUCCESS) then
      errmsg = 'CATChem could not open the host supplied physics interval token.'
      errflg = int(status)
      return
    end if
    call catchem_ccpp_context_model(handle, model, status)
    if (status /= CATCHEM_CONTEXT_SUCCESS .or. .not. associated(model)) then
      call catchem_ccpp_context_close_interval(handle, interval_token, status)
      errmsg = 'CATChem could not retrieve the model for the host physics interval.'
      errflg = max(1, int(status))
      return
    end if
    interval%interval_id = interval_token
    interval%year = year
    interval%month = month
    interval%day = day
    interval%hour = hour
    interval%minute = minute
    interval%second = second
    interval%day_of_year = day_of_year
    interval%duration_seconds = real(duration_seconds,c_double)
    call model%begin_interval(interval, errflg)
    if (errflg /= 0) then
      errmsg = 'CATChem could not begin the host clock interval: '//trim(model%last_error)
      call catchem_ccpp_context_close_interval(handle, interval_token, status)
    end if
  end subroutine catchem_ccpp_timestep_init_impl

  !> \brief Implements catchem ccpp timestep final for the CATChem CCPP boundary.
  !> \param context_id Argument used by catchem_ccpp_timestep_final.
  !> \param context_generation Argument used by catchem_ccpp_timestep_final.
  !> \param interval_token Argument used by catchem_ccpp_timestep_final.
  !> \param errmsg Argument used by catchem_ccpp_timestep_final.
  !> \param errflg Argument used by catchem_ccpp_timestep_final.
  subroutine catchem_ccpp_timestep_final_impl(context_id, context_generation, interval_token, errmsg, errflg)
    integer(c_int), intent(in) :: context_id
    integer(c_int64_t), intent(in) :: context_generation, interval_token
    character(len=*), intent(out) :: errmsg
    integer, intent(out) :: errflg
    type(ccpp_context_handle) :: handle
    type(CATChem_Model), pointer :: model
    character(len=64), allocatable :: diagnostic_names(:), diagnostic_fields(:)
    real(fp), allocatable :: diagnostic_data(:,:,:)
    integer(c_int) :: status
    integer :: diagnostic_index, diagnostic_status
    real(c_double) :: diagnostic_maximum
    real(c_double) :: diagnostic_sum
    handle%id = context_id
    handle%generation = context_generation
    errmsg = ''
    errflg = 0
    call catchem_ccpp_context_close_interval(handle, interval_token, status)
    if (status /= CATCHEM_CONTEXT_SUCCESS) then
      errmsg = 'CATChem could not close the host interval; a tendency contribution may remain unapplied.'
      errflg = int(status)
      return
    end if
    call catchem_ccpp_context_model(handle, model, status)
    if (status /= CATCHEM_CONTEXT_SUCCESS .or. .not. associated(model)) then
      errmsg = 'CATChem could not retrieve the model to close its core interval.'
      errflg = max(1, int(status))
      return
    end if
    call model%end_interval(interval_token, errflg)
    if (errflg /= 0) then
      errmsg = 'CATChem could not end the host clock interval: '//trim(model%last_error)
      return
    end if
    call model%get_diagnostic_names(diagnostic_names,diagnostic_fields,diagnostic_status)
    if (diagnostic_status == 0) then
      do diagnostic_index = 1, size(diagnostic_names)
        call model%get_diagnostic(trim(diagnostic_names(diagnostic_index)),diagnostic_data,diagnostic_status)
        if (diagnostic_status /= 0) cycle
        if (.not. allocated(diagnostic_data)) cycle
        if (size(diagnostic_data) == 0) cycle
        diagnostic_maximum = real(maxval(abs(diagnostic_data)),c_double)
        diagnostic_sum = real(sum(diagnostic_data),c_double)
        write(output_unit,'(a,a,a,es24.16,a,es24.16)') 'CATCHEM_DIAGNOSTIC name=', &
          trim(diagnostic_names(diagnostic_index)), ' MAXABS=', diagnostic_maximum, ' SUM=', diagnostic_sum
      end do
    end if
  end subroutine catchem_ccpp_timestep_final_impl
  !> \brief Open the host-owned shared interval once for all CATChem processes.
  !> \param context_id Host context slot.
  !> \param context_generation Host context generation.
  !> \param interval_token Token for this physics interval.
  !> \param year Host interval year.
  !> \param month Host interval month.
  !> \param day Host interval day of month.
  !> \param hour Host interval hour.
  !> \param minute Host interval minute.
  !> \param second Host interval second.
  !> \param day_of_year Host interval ordinal day.
  !> \param duration_seconds Host interval duration in seconds.
  !> \param errmsg Error detail on failure.
  !> \param errflg CCPP error code.
  subroutine catchem_ccpp_timestep_init(context_id, context_generation, interval_token, year, month, day, &
      hour, minute, second, day_of_year, duration_seconds, errmsg, errflg)
    integer(c_int), intent(in) :: context_id
    integer(c_int64_t), intent(in) :: context_generation, interval_token
    integer, intent(in) :: year, month, day, hour, minute, second, day_of_year
    real(kind_phys), intent(in) :: duration_seconds
    character(len=*), intent(out) :: errmsg
    integer, intent(out) :: errflg
    call catchem_ccpp_timestep_init_impl(context_id, context_generation, interval_token, year, month, day, &
      hour, minute, second, day_of_year, duration_seconds, errmsg, errflg)
  end subroutine catchem_ccpp_timestep_init

  !> \brief Close the shared interval after every participating process has finished.
  !> \param context_id Host context slot.
  !> \param context_generation Host context generation.
  !> \param interval_token Token for this physics interval.
  !> \param errmsg Error detail on failure.
  !> \param errflg CCPP error code.
  subroutine catchem_ccpp_timestep_final(context_id, context_generation, interval_token, errmsg, errflg)
    integer(c_int), intent(in) :: context_id
    integer(c_int64_t), intent(in) :: context_generation, interval_token
    character(len=*), intent(out) :: errmsg
    integer, intent(out) :: errflg
    call catchem_ccpp_timestep_final_impl(context_id, context_generation, interval_token, errmsg, errflg)
  end subroutine catchem_ccpp_timestep_final
end module catchem_ccpp

!> \brief Opens the host-owned CATChem interval as a separately selectable CCPP scheme.

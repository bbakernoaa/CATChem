!> \file TimeState_Mod.F90
!! \brief Time state and common time/solar functions for atmospheric chemistry
!!
!! Provides timekeeping, solar zenith angle, and calendar utilities for CATChem.
!!
module TimeState_Mod
   use StateManager_Mod, only: STATE_STATUS_UNINITIALIZED, STATE_STATUS_INITIALIZED
   use error_mod, only: ErrorManagerType, CC_SUCCESS, CC_FAILURE
   use constants, only: PI, PI_180
   implicit none
   private
   public :: TimeStateType, is_global_holiday, is_us_holiday

   !> \brief Time state for model
   type :: TimeStateType
      integer :: year = 2000
      integer :: month = 1
      integer :: day = 1
      integer :: hour = 0
      integer :: minute = 0
      integer :: second = 0
      real    :: timestep = 3600.0 !< seconds
      real    :: julian_date = 0.0
      integer :: doy = 1
   contains
      procedure :: get_sza
      procedure :: get_cos_sza
      procedure :: get_timestep
      procedure :: get_current_date
      procedure :: get_julian_date
      procedure :: get_doy
      procedure :: init => timestate_init
      procedure :: validate => timestate_validate
      procedure :: cleanup => timestate_cleanup
      procedure :: reset => timestate_reset
      procedure :: get_status => timestate_get_status
      procedure :: get_memory_usage => timestate_get_memory_usage
      procedure :: print_info => timestate_print_info
      procedure :: is_ready => timestate_is_ready
      procedure :: get_time_iso8601
      procedure :: get_time_human
      procedure :: get_time_compact
      procedure :: get_timezone_offset
   end type TimeStateType

contains

   !> \brief Compute solar zenith angle (degrees) using latitude, longitude, and time of day
   real function get_sza(this, lat, lon) result(sza)
      class(TimeStateType), intent(in) :: this
      real, intent(in) :: lat, lon
      ! Accurate solar zenith angle calculation
      ! Inputs: lat, lon in degrees; time from this%hour, this%minute, this%second; day of year from this%doy
      real :: lat_rad, lon_rad, decl_rad, ha_rad
      real :: decl, eqtime, time_offset, tst, ha
      real :: cos_sza_val
      real :: fractional_hour, gamma

      ! Convert latitude and longitude to radians
      lat_rad = lat * PI_180
      lon_rad = lon * PI_180

      ! Calculate fractional hour of the day (UTC)
      fractional_hour = real(this%hour) + real(this%minute)/60.0 + real(this%second)/3600.0

      ! Calculate day angle (in radians)
      gamma = 2.0 * PI * (real(this%doy) - 1.0) / 365.0

      ! Solar declination (in degrees, then radians)
      decl = 23.44 * sin(2.0 * PI * (real(this%doy) - 81.0) / 365.0)
      decl_rad = decl * PI_180

      ! Equation of time (in minutes)
      eqtime = 229.18 * (0.000075 + 0.001868 * cos(gamma) - 0.032077 * sin(gamma) \
      - 0.014615 * cos(2.0*gamma) - 0.040849 * sin(2.0*gamma))

      ! Time offset (in minutes)
      time_offset = eqtime + 4.0 * lon

      ! True solar time (in minutes)
      tst = fractional_hour * 60.0 + time_offset

      ! Hour angle (in degrees, then radians)
      ha = (tst / 4.0) - 180.0
      ha_rad = ha * PI_180

      ! Solar zenith angle calculation
      cos_sza_val = sin(lat_rad) * sin(decl_rad) + cos(lat_rad) * cos(decl_rad) * cos(ha_rad)
      cos_sza_val = max(-1.0, min(1.0, cos_sza_val)) ! Clamp for safety
      sza = acos(cos_sza_val) / PI_180
      sza = min(max(sza, 0.0), 90.0) ! Clamp to [0, 90] degrees
   end function get_sza

   !> \brief Compute cosine of solar zenith angle
   real function get_cos_sza(this, lat, lon) result(cos_sza)
      class(TimeStateType), intent(in) :: this
      real, intent(in) :: lat, lon
      cos_sza = cos(this%get_sza(lat, lon) * PI_180)
   end function get_cos_sza

   !> \brief Get model timestep (seconds)
   real function get_timestep(this) result(dt)
      class(TimeStateType), intent(in) :: this
      dt = this%timestep
   end function get_timestep

   !> \brief Get current date as (year, month, day)
   subroutine get_current_date(this, year, month, day)
      class(TimeStateType), intent(in) :: this
      integer, intent(out) :: year, month, day
      year = this%year
      month = this%month
      day = this%day
   end subroutine get_current_date

   !> \brief Get Julian date
   real function get_julian_date(this) result(jd)
      class(TimeStateType), intent(in) :: this
      jd = this%julian_date
   end function get_julian_date

   !> \brief Get day of year
   integer function get_doy(this) result(doy)
      class(TimeStateType), intent(in) :: this
      doy = this%doy
   end function get_doy

   !> \brief Initialize TimeStateType
   subroutine timestate_init(this, error_mgr, rc)
      class(TimeStateType), intent(inout) :: this
      type(ErrorManagerType), pointer, intent(inout) :: error_mgr
      integer, intent(out) :: rc

      ! Suppress unused parameter warning
      if (associated(error_mgr)) continue
      rc = CC_SUCCESS
   end subroutine timestate_init

   !> \brief Validate TimeStateType
   subroutine timestate_validate(this, error_mgr, rc)
      class(TimeStateType), intent(in) :: this
      type(ErrorManagerType), pointer, intent(inout) :: error_mgr
      integer, intent(out) :: rc

      ! Suppress unused parameter warning
      if (associated(error_mgr)) continue
      rc = CC_SUCCESS
   end subroutine timestate_validate

   !> \brief Cleanup TimeStateType
   subroutine timestate_cleanup(this, error_mgr, rc)
      class(TimeStateType), intent(inout) :: this
      type(ErrorManagerType), pointer, intent(inout) :: error_mgr
      integer, intent(out) :: rc

      ! Suppress unused parameter warning
      if (associated(error_mgr)) continue
      rc = CC_SUCCESS
   end subroutine timestate_cleanup

   !> \brief Reset TimeStateType
   subroutine timestate_reset(this, error_mgr, rc)
      class(TimeStateType), intent(inout) :: this
      type(ErrorManagerType), pointer, intent(inout) :: error_mgr
      integer, intent(out) :: rc

      ! Suppress unused parameter warning
      if (associated(error_mgr)) continue
      rc = CC_SUCCESS
   end subroutine timestate_reset

   !> \brief Get status of TimeStateType
   function timestate_get_status(this) result(status)
      class(TimeStateType), intent(in) :: this
      integer :: status
      status = STATE_STATUS_INITIALIZED
   end function timestate_get_status

   !> \brief Get memory usage of TimeStateType
   function timestate_get_memory_usage(this) result(memory_bytes)
      class(TimeStateType), intent(in) :: this
      integer(8) :: memory_bytes
      memory_bytes = 0_8
   end function timestate_get_memory_usage

   !> \brief Print info for TimeStateType
   subroutine timestate_print_info(this, unit)
      class(TimeStateType), intent(in) :: this
      integer, optional, intent(in) :: unit
      if (present(unit)) then
         write(unit,*) 'TimeStateType: ', this%year, this%month, this%day, this%hour, this%minute, this%second
      else
         print *, 'TimeStateType: ', this%year, this%month, this%day, this%hour, this%minute, this%second
      end if
   end subroutine timestate_print_info

   !> \brief Is TimeStateType ready?
   function timestate_is_ready(this) result(ready)
      class(TimeStateType), intent(in) :: this
      logical :: ready
      ready = .true.
   end function timestate_is_ready

   !> \brief Check if a date is a global holiday
   logical function is_global_holiday(month, day)
      integer, intent(in) :: month, day
      is_global_holiday = ( (month==1 .and. day==1) .or. (month==12 .and. day==25) )
   end function is_global_holiday

   !> \brief Check if a date is a U.S. holiday
   logical function is_us_holiday(month, day)
      integer, intent(in) :: month, day
      is_us_holiday = ( (month==7 .and. day==4) .or. (month==11 .and. day>=22 .and. day<=28) )
   end function is_us_holiday

   !> \brief Get current simulation time as ISO 8601 string (YYYY-MM-DDTHH:MM:SS)
   pure function get_time_iso8601(this) result(timestr)
      class(TimeStateType), intent(in) :: this
      character(len=25) :: timestr
      write(timestr, '(I4.4,"-",I2.2,"-",I2.2,"T",I2.2,":",I2.2,":",I2.2)') &
         this%year, this%month, this%day, this%hour, this%minute, this%second
   end function get_time_iso8601

   !> \brief Get current simulation time as a human-readable string (e.g., 2025-06-25 14:30:00)
   pure function get_time_human(this) result(timestr)
      class(TimeStateType), intent(in) :: this
      character(len=25) :: timestr
      write(timestr, '(I4.4,"-",I2.2,"-",I2.2," ",I2.2,":",I2.2,":",I2.2)') &
         this%year, this%month, this%day, this%hour, this%minute, this%second
   end function get_time_human

   !> \brief Get current simulation time as a compact string (YYYYMMDD_HHMMSS)
   pure function get_time_compact(this) result(timestr)
      class(TimeStateType), intent(in) :: this
      character(len=16) :: timestr
      write(timestr, '(I4.4,I2.2,I2.2,"_",I2.2,I2.2,I2.2)') &
         this%year, this%month, this%day, this%hour, this%minute, this%second
   end function get_time_compact

   !> \brief Calculate local timezone offset (hours) from longitude (robust, clamped)
   pure integer function get_timezone_offset(this, lon) result(tz_offset)
      class(TimeStateType), intent(in) :: this
      real, intent(in) :: lon
      ! Truncate toward zero, clamp to [-12, 14] (real-world timezones)
      tz_offset = int(lon / 15.0)
      tz_offset = max(-12, min(14, tz_offset))
   end function get_timezone_offset

end module TimeState_Mod

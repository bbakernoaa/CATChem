!> \file test_TimeState.f90
!! \brief Test program for TimeState module
!!
!!!>
program test_TimeState
   use testing_mod, only: assert, assert_close
   use TimeState_Mod
   use Error_Mod, only: ErrorManagerType, CC_SUCCESS

   implicit none

   type(TimeStateType) :: time_state
   type(ErrorManagerType), pointer :: error_mgr
   integer :: year, month, day
   real :: jd
   integer :: doy
   real :: sza
   integer :: rc

   write(*,*) 'Testing TimeState module...'
   write(*,*) ''

   ! Test 1: Error manager initialization
   write(*,*) 'Test 1: Error manager initialization'
   allocate(error_mgr)
   call error_mgr%init()

   ! Test 2: TimeState initialization
   write(*,*) 'Test 2: TimeState initialization'
   call time_state%init(error_mgr, rc)
   call assert(rc == CC_SUCCESS, "TimeState initialization should succeed")

   write(*,*) 'Test 2 passed!'
   write(*,*) ''

   ! Test 3: Default values
   write(*,*) 'Test 3: Default values'
   call time_state%get_current_date(year, month, day)
   jd = time_state%get_julian_date()
   doy = time_state%get_doy()

   call assert(year == 2000, "Default year should be 2000")
   call assert(month == 1, "Default month should be 1")
   call assert(day == 1, "Default day should be 1")
   call assert(doy == 1, "Default day of year should be 1")

   write(*,*) 'Test 3 passed!'
   write(*,*) ''

   ! Test 4: Solar zenith angle calculation
   write(*,*) 'Test 4: Solar zenith angle calculation'
   ! Calculate SZA for noon at equator on day 1
   sza = time_state%get_sza(0.0, 0.0)  ! lat=0, lon=0
   call assert(sza >= 0.0 .and. sza <= 90.0, "SZA should be between 0 and 90 degrees")

   write(*,*) 'Test 4 passed!'
   write(*,*) ''

   ! Test 5: Timestep
   write(*,*) 'Test 5: Timestep'
   block
      real :: dt
      dt = time_state%get_timestep()
      call assert(dt == 3600.0, "Default timestep should be 3600 seconds")
   end block

   write(*,*) 'Test 5 passed!'
   write(*,*) ''

   ! Test 6: Time string functions
   write(*,*) 'Test 6: Time string functions'
   block
      character(len=25) :: iso_time
      character(len=25) :: human_time
      character(len=16) :: compact_time

      iso_time = time_state%get_time_iso8601()
      human_time = time_state%get_time_human()
      compact_time = time_state%get_time_compact()

      ! Just check that they return non-empty strings
      call assert(len_trim(iso_time) > 0, "ISO time should not be empty")
      call assert(len_trim(human_time) > 0, "Human time should not be empty")
      call assert(len_trim(compact_time) > 0, "Compact time should not be empty")
   end block

   write(*,*) 'Test 6 passed!'
   write(*,*) ''

   ! Test 7: Cleanup
   write(*,*) 'Test 7: Cleanup'
   call time_state%cleanup(error_mgr, rc)
   call assert(rc == CC_SUCCESS, "TimeState cleanup should succeed")

   write(*,*) 'Test 7 passed!'
   write(*,*) ''

   write(*,*) 'All TimeState tests passed!'

end program test_TimeState

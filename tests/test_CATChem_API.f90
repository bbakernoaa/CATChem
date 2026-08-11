!> \file test_CATChem_API.f90
!! \brief Init-sequence test for the high-level CATChem_Model API.
!!
!! Mirrors the NUOPC cap's usage so the Fortran state facade over the
!! C++-owned state is covered without ESMF:
!!   initialize -> get_state_manager -> get_met_state_ptr -> write lat/lon
!!   -> run_timestep -> finalize
!! Also asserts that grid dimensions are host-supplied (rank-local), not
!! taken from the YAML grid section.
program test_CATChem_API
   use CATChem_API, only: CATChem_Model
   use StateManager_Mod, only: StateManagerType
   use MetState_Mod, only: MetStateType
   use Error_Mod, only: ErrorManagerType, CC_SUCCESS
   use precision_mod, only: fp
   implicit none

   type(CATChem_Model) :: model
   type(StateManagerType), pointer :: sm
   type(MetStateType), pointer :: met
   type(ErrorManagerType), pointer :: em
   real(fp), allocatable :: z0_cm(:, :)
   integer :: rc
   integer, parameter :: nx = 4, ny = 2, nz = 5
   character(len=*), parameter :: config_file = 'CATChem_new_config.yml'
   logical :: exists

   inquire(file=config_file, exist=exists)
   if (.not. exists) then
      print *, 'FAIL: config fixture not found: ', config_file
      error stop 1
   end if

   ! 1. Initialize with host-local grid dimensions (the YAML grid section
   !    says 64 levels; the host dims below must win).
   call model%initialize(config_file, nx, ny, nz, rc=rc)
   if (rc /= 0) then
      print *, 'FAIL: model initialize rc=', rc
      error stop 1
   end if
   if (model%nx /= nx .or. model%ny /= ny .or. model%nz /= nz) then
      print *, 'FAIL: model dims not host-supplied:', model%nx, model%ny, model%nz
      error stop 1
   end if
   print *, 'PASS: initialize with host-local grid dimensions'

   ! 2. The facade must be constructed and fully wired.
   sm => model%get_state_manager()
   met => sm%get_met_state_ptr()
   if (.not. associated(met)) then
      print *, 'FAIL: met state facade not associated'
      error stop 1
   end if
   if (.not. associated(met%LAT) .or. .not. associated(met%LON)) then
      print *, 'FAIL: LAT/LON not bound through the C++ state'
      error stop 1
   end if
   if (.not. associated(met%AREA_M2)) then
      print *, 'FAIL: AREA_M2 not allocated/bound'
      error stop 1
   end if
   print *, 'PASS: facade constructed, met arrays bound'

   ! 3. Write lat/lon the way the cap does, then re-fetch the facade to
   !    prove the values live in the shared (C++-registered) buffers.
   met%LAT = 40.0_fp
   met%LON = 250.0_fp
   where (met%LON > 180.0_fp)
      met%LON = met%LON - 360.0_fp
   end where

   met => sm%get_met_state_ptr()
   if (abs(met%LAT(1, 1) - 40.0_fp) > 1.0e-12_fp) then
      print *, 'FAIL: LAT not persisted through shared buffer:', met%LAT(1, 1)
      error stop 1
   end if
   if (abs(met%LON(nx, ny) + 110.0_fp) > 1.0e-12_fp) then
      print *, 'FAIL: LON not persisted/converted:', met%LON(nx, ny)
      error stop 1
   end if
   print *, 'PASS: lat/lon written and persisted through shared buffers'

   ! 3b. Replay the NUOPC transform's Z0 path (cm -> m conversion via
   !     set_field) — the 2026-08-11 run-phase abort regression.
   if (.not. associated(met%Z0)) then
      print *, 'FAIL: Z0 not allocated/bound by the facade'
      error stop 1
   end if
   em => sm%get_error_manager()
   allocate(z0_cm(nx, ny))
   z0_cm = 150.0_fp
   call met%set_field('Z0', z0_cm*0.01_fp, em, rc)
   if (rc /= CC_SUCCESS) then
      print *, 'FAIL: set_field(Z0) rc=', rc, ' (missing case)'
      error stop 1
   end if
   met => sm%get_met_state_ptr()
   if (abs(met%Z0(1, 1) - 1.5_fp) > 1.0e-12_fp) then
      print *, 'FAIL: Z0 not persisted through shared buffer:', met%Z0(1, 1)
      error stop 1
   end if
   deallocate(z0_cm)
   print *, 'PASS: Z0 transform path (set_field + shared buffer)'

   ! 4. Error manager and time state facades exist (run phase uses them).
   if (.not. associated(sm%get_error_manager())) then
      print *, 'FAIL: error manager facade missing'
      error stop 1
   end if
   if (.not. associated(sm%get_time_state_ptr())) then
      print *, 'FAIL: time state facade missing'
      error stop 1
   end if
   print *, 'PASS: error/time facades present'

   ! 5. A timestep runs (no processes registered; exercises sync paths).
   call model%run_timestep(1, 300.0_fp, rc)
   if (rc /= 0) then
      print *, 'FAIL: run_timestep rc=', rc
      error stop 1
   end if
   print *, 'PASS: run_timestep'

   call model%finalize(rc)
   if (rc /= 0) then
      print *, 'FAIL: finalize rc=', rc
      error stop 1
   end if
   print *, 'PASS: finalize'

   print *, 'All CATChem_API init-sequence tests passed!'
end program test_CATChem_API

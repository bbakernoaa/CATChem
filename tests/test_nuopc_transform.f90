!> \file test_nuopc_transform.f90
!! \brief ESMF-backed test of the NUOPC import transform.
!!
!! Builds only when CATCHEM_BUILD_NUOPC=ON. Reproduces the ursa
!! run-phase mechanism in-process: load the real field mapping
!! (CATChem_field_mapping.yml), create an ESMF field for every
!! required import entry, and drive transform_nuopc_to_catchem —
!! asserting success and the Z0 cm->m conversion landing in the met
!! state (the 2026-08-11 first-timestep abort regression).
program test_nuopc_transform
   use iso_c_binding, only: c_ptr, c_char, c_double, c_associated, c_f_pointer, c_null_char
   use ESMF
   use catchem_nuopc_interface, only: load_field_config, transform_nuopc_to_catchem, &
      transform_catchem_to_nuopc, field_config, cc_wrap_type, update_pm_diagnostics
   use catchem_bridge_error, only: CC_SUCCESS
   use catchem_bridge_precision, only: fp

   implicit none

   interface
      type(c_ptr) function catchem_state_get_pointer_2d(state_ptr, name) bind(C, name="catchem_state_get_pointer_2d")
         import :: c_ptr, c_char
         type(c_ptr), value :: state_ptr
         character(kind=c_char), intent(in) :: name(*)
      end function
   end interface

   integer, parameter :: nx = 4, ny = 2, nz = 5, ntr = 2
   type(cc_wrap_type) :: cc_wrap
   type(ESMF_Grid) :: grid
   type(ESMF_State) :: importState, exportState
   type(ESMF_Field) :: field
   type(ESMF_Time) :: currTime
   real(ESMF_KIND_R8), pointer :: fptr2(:, :), fptr3(:, :, :), fptr4(:, :, :, :)
   real(c_double), pointer :: z0_ptr(:,:) => null()
   type(c_ptr) :: raw_z0_ptr
   character(len=256) :: errmsg
   integer :: rc, i, nzf

   call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN, &
      defaultlogfilename="test_nuopc_transform.log", rc=rc)
   call check(rc, "ESMF_Initialize")

   ! 1. Load the real production field mapping
   call load_field_config('CATChem_field_mapping.yml', rc, errmsg)
   if (rc /= 0) then
      print *, 'FAIL: load_field_config: ', trim(errmsg)
      error stop 1
   end if

   ! 2. Initialize the model (facade + host-local grid dims)
   call cc_wrap%catchem_model%initialize('CATChem_new_config.yml', nx, ny, nz, rc=rc)
   if (rc /= 0) then
      print *, 'FAIL: model initialize rc=', rc
      error stop 1
   end if
   cc_wrap%field_config = field_config
   call ESMF_TimeIntervalSet(cc_wrap%timeStep, s=600, rc=rc)
   call check(rc, "TimeIntervalSet")

   ! 3. Build an import state holding every REQUIRED import field from
   !    the mapping, sized per its declared dimensionality. Z0
   !    (inst_surface_roughness) carries 150 cm; everything else 1.0.
   grid = ESMF_GridCreateNoPeriDim(maxIndex=(/nx, ny/), rc=rc)
   call check(rc, "GridCreate")
   importState = ESMF_StateCreate(name="import", rc=rc)
   call check(rc, "StateCreate")

   do i = 1, cc_wrap%field_config%n_import_fields
      select case (cc_wrap%field_config%import_fields(i)%dimensions)
       case (2)
         field = ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8, &
            name=trim(cc_wrap%field_config%import_fields(i)%standard_name), rc=rc)
         call check(rc, "FieldCreate 2d")
         call ESMF_FieldGet(field, farrayPtr=fptr2, rc=rc)
         call check(rc, "FieldGet 2d")
         if (trim(cc_wrap%field_config%import_fields(i)%catchem_var) == 'Z0') then
            fptr2 = 150.0_ESMF_KIND_R8 ! cm; the transform converts to 1.5 m
         else
            fptr2 = 1.0_ESMF_KIND_R8
         end if
       case (3)
         nzf = nz
         if (trim(cc_wrap%field_config%import_fields(i)%catchem_var) == 'PEDGE') nzf = nz + 1
         field = ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8, &
            ungriddedLBound=(/1/), ungriddedUBound=(/nzf/), &
            name=trim(cc_wrap%field_config%import_fields(i)%standard_name), rc=rc)
         call check(rc, "FieldCreate 3d")
         call ESMF_FieldGet(field, farrayPtr=fptr3, rc=rc)
         call check(rc, "FieldGet 3d")
         fptr3 = 1.0_ESMF_KIND_R8
       case (4)
         field = ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8, &
            ungriddedLBound=(/1, 1/), ungriddedUBound=(/nz, ntr/), &
            name=trim(cc_wrap%field_config%import_fields(i)%standard_name), rc=rc)
         call check(rc, "FieldCreate 4d")
         call ESMF_FieldGet(field, farrayPtr=fptr4, rc=rc)
         call check(rc, "FieldGet 4d")
         fptr4 = 1.0_ESMF_KIND_R8
       case default
         cycle
      end select

      call ESMF_StateAdd(importState, (/field/), rc=rc)
      call check(rc, "StateAdd")
   end do

   ! 4. Drive the production transform — the exact ursa run-phase path
   call ESMF_TimeSet(currTime, yy=2021, mm=3, dd=22, h=6, rc=rc)
   call check(rc, "TimeSet")
   call transform_nuopc_to_catchem(cc_wrap, importState, currTime, rc)
   if (rc /= ESMF_SUCCESS) then
      print *, 'FAIL: transform_nuopc_to_catchem rc=', rc, &
         ' (see test_nuopc_transform.log)'
      error stop 1
   end if
   print *, 'PASS: transform_nuopc_to_catchem over the full required mapping'

   ! 5. Z0 must have landed in the C++ met state
   raw_z0_ptr = catchem_state_get_pointer_2d(cc_wrap%catchem_model%state_mgr_ptr, "Z0" // c_null_char)
   if (.not. c_associated(raw_z0_ptr)) then
      print *, 'FAIL: Z0 not bound in C++ met state after transform'
      error stop 1
   end if
   call c_f_pointer(raw_z0_ptr, z0_ptr, [nx, ny])
   if (abs(z0_ptr(1, 1) - 1.5_fp) > 1.0e-12_fp) then
      print *, 'FAIL: Z0 cm->m conversion not applied:', z0_ptr(1, 1)
      error stop 1
   end if
   print *, 'PASS: Z0 converted (150 cm -> 1.5 m) and stored'

   ! 6. PM2.5/PM10 diagnostics update — the 2026-08-11 17:07 run-phase
   !    abort regression. Must succeed and register the fields in the
   !    C++ DiagnosticManager (where NUOPC export reads from).
   call update_pm_diagnostics(cc_wrap, rc)
   if (rc /= CC_SUCCESS) then
      print *, 'FAIL: update_pm_diagnostics rc=', rc
      error stop 1
   end if
   block
      character(len=64), allocatable :: diag_names(:)
      call cc_wrap%catchem_model%get_diagnostic_names(diag_names, rc=rc)
      if (rc /= 0 .or. .not. allocated(diag_names)) then
         print *, 'FAIL: get_diagnostic_names rc=', rc
         error stop 1
      end if
      if (.not. any(diag_names == 'pm25') .or. .not. any(diag_names == 'pm10')) then
         print *, 'FAIL: pm25/pm10 not registered in the C++ diagnostic manager'
         error stop 1
      end if
   end block
   print *, 'PASS: PM2.5/PM10 diagnostics updated and registered'

   ! 7. Test transform_catchem_to_nuopc (full C++ Core -> C API -> Fortran export state)
   exportState = ESMF_StateCreate(name="export", rc=rc)
   call check(rc, "StateCreate export")

   do i = 1, cc_wrap%field_config%n_export_fields
      select case (cc_wrap%field_config%export_fields(i)%dimensions)
       case (2)
         field = ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8, &
            name=trim(cc_wrap%field_config%export_fields(i)%standard_name), rc=rc)
         call check(rc, "Export FieldCreate 2d")
       case (3)
         field = ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8, &
            ungriddedLBound=(/1/), ungriddedUBound=(/nz/), &
            name=trim(cc_wrap%field_config%export_fields(i)%standard_name), rc=rc)
         call check(rc, "Export FieldCreate 3d")
       case (4)
         field = ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8, &
            ungriddedLBound=(/1, 1/), ungriddedUBound=(/nz, ntr/), &
            name=trim(cc_wrap%field_config%export_fields(i)%standard_name), rc=rc)
         call check(rc, "Export FieldCreate 4d")
       case default
         cycle
      end select
      call ESMF_StateAdd(exportState, (/field/), rc=rc)
      call check(rc, "Export StateAdd")
   end do

   call transform_catchem_to_nuopc(cc_wrap, exportState, rc)
   if (rc /= ESMF_SUCCESS) then
      print *, 'FAIL: transform_catchem_to_nuopc rc=', rc
      error stop 1
   end if
   print *, 'PASS: transform_catchem_to_nuopc over export state'

   call ESMF_Finalize(rc=rc)
   print *, 'All NUOPC transform tests passed!'

contains

   subroutine check(rc_in, what)
      integer, intent(in) :: rc_in
      character(len=*), intent(in) :: what
      if (rc_in /= ESMF_SUCCESS) then
         print *, 'FAIL (setup): ', what, ' rc=', rc_in
         error stop 1
      end if
   end subroutine check

end program test_nuopc_transform

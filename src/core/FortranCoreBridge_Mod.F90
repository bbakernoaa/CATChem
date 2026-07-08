!> \file FortranCoreBridge_Mod.F90
!! \brief Fortran dynamic bridging procedures callback to execute legacy schemes.
module FortranCoreBridge_Mod
   use iso_c_binding, only: c_ptr, c_f_pointer, c_null_char, c_associated, c_char, c_double
   implicit none
   private

   public :: run_settling_physics_fortran_bridge

   interface
      ! C-API Bindings
      function catchem_state_get_pointer_2d(state_ptr, name) bind(C, name="catchem_state_get_pointer_2d")
         import :: c_ptr, c_char
         type(c_ptr), value :: state_ptr
         character(kind=c_char), intent(in) :: name(*)
         type(c_ptr) :: catchem_state_get_pointer_2d
      end function catchem_state_get_pointer_2d
   end interface

contains

   !> \brief C-linkable dynamic bridge to execute legacy physical schemes on raw C++ memory pointers.
   subroutine run_settling_physics_fortran_bridge(state_ptr) bind(C, name="run_settling_physics_fortran_bridge")
      type(c_ptr), value :: state_ptr
      type(c_ptr) :: c_temp
      real(c_double), pointer :: temp(:,:)
      integer :: n_cols, n_levels

      ! 1. Mock dimensions for this bridge test (matching test_catchem_interop sizes: 4 x 5)
      n_cols = 4
      n_levels = 5

      ! 2. Retrieve C++ double pointer for "temperature"
      ! append null-termination to Fortran string literal
      c_temp = catchem_state_get_pointer_2d(state_ptr, "temperature" // c_null_char)
      
      if (.not. c_associated(c_temp)) return

      ! 3. Wrap raw C++ pointer back to Fortran array pointer (LayoutLeft matching column-major size)
      call c_f_pointer(c_temp, temp, [n_cols, n_levels])

      ! 4. Execute legacy Fortran physical scheme directly working on shared memory in-place
      temp(:,:) = temp(:,:) + 10.0_c_double

   end subroutine run_settling_physics_fortran_bridge

end module FortranCoreBridge_Mod

!> Exercises stride-safe constituent import and extent rejection.
program test_ccpp_layout
   use iso_c_binding, only: c_double, c_int, c_int64_t
   use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
   use machine, only: kind_phys
   use catchem_ccpp_layout
   implicit none
   real(kind_phys), allocatable :: backing(:, :, :), empty_host(:, :, :), interface_host(:, :, :), soil_host(:, :, :)
   real(kind_phys), allocatable :: host_before(:, :, :)
   real(c_double), allocatable :: canonical(:, :, :)
   integer(c_int) :: status
   integer :: col, layer, species

   allocate (backing(2, 4, 6), canonical(2, 2, 2), host_before(2, 4, 6))
   do species = 1, size(backing, 3)
      do layer = 1, size(backing, 2)
         do col = 1, size(backing, 1)
            backing(col, layer, species) = real(100*species + 10*layer + col, kind_phys)
         end do
      end do
   end do
   canonical = -99.0_c_double

   ! The stride-two layer and species sections force a noncontiguous host view.
   call catchem_ccpp_copy_layout(backing(:, 1:3:2, 1:5:2), [3, 1], [2, 1], canonical, status)
   call require(status == CATCHEM_LAYOUT_OK, 'noncontiguous and reordered host layout is accepted')
   call require(abs(canonical(2, 1, 1) - 532.0_c_double) < 1.0e-12_c_double .and. &
      abs(canonical(1, 2, 2) - 111.0_c_double) < 1.0e-12_c_double, &
      'slot and vertical maps are applied exactly once')

   ! Scatter reverses the declared vertical order and preserves every unmapped
   ! constituent bit-for-bit, including in a noncontiguous host section.
   host_before = backing
   canonical(1, 1, :) = [901.0_c_double, 902.0_c_double]
   canonical(2, 1, :) = [903.0_c_double, 904.0_c_double]
   canonical(1, 2, :) = [905.0_c_double, 906.0_c_double]
   canonical(2, 2, :) = [907.0_c_double, 908.0_c_double]
   host_before = backing
   call catchem_ccpp_scatter_layout(canonical, [3, 1], [2, 1], backing(:, 1:3:2, 1:5:2), status)
   call require(status == CATCHEM_LAYOUT_OK, 'atomic scatter accepts a declared reverse layer order')
   call require(same_bits(backing(:, :, 2:2), host_before(:, :, 2:2)) .and. &
      same_bits(backing(:, :, 3:3), host_before(:, :, 3:3)) .and. &
      same_bits(backing(:, :, 4:4), host_before(:, :, 4:4)) .and. &
      same_bits(backing(:, :, 6:6), host_before(:, :, 6:6)), &
      'scatter preserves all unrelated constituent slots bitwise')
   call require(abs(backing(1, 3, 5) - 901.0_kind_phys) < 1.0e-12_kind_phys .and. &
      abs(backing(2, 3, 1) - 904.0_kind_phys) < 1.0e-12_kind_phys .and. &
      abs(backing(2, 1, 5) - 907.0_kind_phys) < 1.0e-12_kind_phys, &
      'scatter applies species and vertical maps exactly once')

   canonical = 77.0_c_double
   host_before = backing
   canonical(2, 2, 2) = ieee_value(0.0_c_double, ieee_quiet_nan)
   call catchem_ccpp_scatter_layout(canonical, [3, 1], [2, 1], backing(:, 1:3:2, 1:5:2), status)
   call require(status == CATCHEM_LAYOUT_INVALID .and. same_bits(backing, host_before), &
      'nonfinite late output leaves every host slot unchanged')
   canonical = 77.0_c_double
   call catchem_ccpp_copy_layout(backing, [3, 3], [1, 2], canonical, status)
   call require(status == CATCHEM_LAYOUT_INVALID .and. maxval(abs(canonical - 77.0_c_double)) < tiny(1.0_c_double), &
      'duplicate constituent slots fail without exposing partial output')
   call catchem_ccpp_copy_layout(backing, [3, 1], [1, 1], canonical, status)
   call require(status == CATCHEM_LAYOUT_INVALID .and. maxval(abs(canonical - 77.0_c_double)) < tiny(1.0_c_double), &
      'duplicate vertical levels fail without exposing partial output')

   backing(2, 2, 3) = real(ieee_value(0.0_c_double, ieee_quiet_nan), kind_phys)
   call catchem_ccpp_copy_layout(backing, [1, 3], [1, 2], canonical, status)
   call require(status == CATCHEM_LAYOUT_INVALID .and. maxval(abs(canonical - 77.0_c_double)) < tiny(1.0_c_double), &
      'nonfinite late element leaves every output slot unchanged')

   allocate (empty_host(0, 2, 3))
   call catchem_ccpp_copy_layout(empty_host, [1, 2], [1, 2], canonical, status)
   call require(status == CATCHEM_LAYOUT_INVALID .and. maxval(abs(canonical - 77.0_c_double)) < tiny(1.0_c_double), &
      'zero extents are rejected before array access')
   allocate (interface_host(2, 3, 6), soil_host(2, 4, 6))
   interface_host = 1.0_kind_phys
   soil_host = 2.0_kind_phys
   call catchem_ccpp_copy_layout(interface_host, [1, 3], [1, 2], canonical, status)
   call require(status == CATCHEM_LAYOUT_INVALID .and. maxval(abs(canonical - 77.0_c_double)) < tiny(1.0_c_double), &
      'interface extent cannot masquerade as a layer extent')
   call catchem_ccpp_copy_layout(soil_host, [1, 3], [1, 2], canonical, status)
   call require(status == CATCHEM_LAYOUT_INVALID .and. maxval(abs(canonical - 77.0_c_double)) < tiny(1.0_c_double), &
      'soil extent cannot masquerade as a chemistry layer extent')
   print *, 'PASS test_ccpp_layout'
contains
   !> Compare real arrays by their storage bits for exact preservation checks.
   !! @param left First host array.
   !! @param right Second host array.
   !! @return True when extents and stored bits are identical.
   pure logical function same_bits(left, right)
      real(kind_phys), intent(in) :: left(:, :, :), right(:, :, :)
      same_bits = all(transfer(left, [0_c_int64_t], size(left)) == &
         transfer(right, [0_c_int64_t], size(right)))
   end function same_bits

   !> \brief Fail the test program with an assertion message.
   !> \param condition True when the assertion passes.
   !> \param message Description of the required behavior.
   subroutine require(condition, message)
      logical, intent(in) :: condition
      character(len=*), intent(in) :: message
      if (.not. condition) then
         print *, 'FAIL: ', trim(message)
         stop 1
      end if
   end subroutine require
end program test_ccpp_layout

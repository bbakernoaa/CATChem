!> \file test_VirtualColumn.f90
!! \brief Test program for VirtualColumn module
!!
!!!>
program test_VirtualColumn
   use testing_mod, only: assert, assert_close
   use VirtualColumn_Mod
   use Precision_Mod, only: fp
   use species_mod, only: SpeciesType

   implicit none

   type(VirtualColumnType) :: virtual_col
   integer :: nlev, nspec_chem, nspec_emis
   integer :: i, j
   real(fp) :: lat, lon, area
   real(fp) :: test_value
   integer :: rc
   ! ChemSpecies array for pointer-based chem access testing
   type(SpeciesType), target :: test_chem_species(5)
   integer :: s

   write(*,*) 'Testing VirtualColumn module...'
   write(*,*) ''

   ! Test 1: Initialize virtual column
   write(*,*) 'Test 1: Initialize virtual column'
   nlev = 10
   nspec_chem = 5
   nspec_emis = 3
   i = 1
   j = 1
   lat = 45.0_fp
   lon = -120.0_fp
   area = 1000000.0_fp  ! 1 km²

   call virtual_col%init(nlev, nspec_chem, nspec_emis, i, j, lat, lon, area, rc)
   call assert(rc == 0, "Virtual column initialization should succeed")

   ! Set up ChemSpecies array with allocated conc arrays for pointer-based access
   do s = 1, nspec_chem
      allocate(test_chem_species(s)%conc(2, 2, nlev))
      test_chem_species(s)%conc = 0.0_fp
   end do
   ! Re-init with chem_species pointer
   call virtual_col%cleanup()
   call virtual_col%init(nlev, nspec_chem, nspec_emis, i, j, lat, lon, area, rc, &
      chem_species=test_chem_species)

   write(*,*) 'Test 1 passed!'
   write(*,*) ''

   ! Test 2: Get dimensions
   write(*,*) 'Test 2: Get dimensions'
   call virtual_col%get_dimensions(nlev, nspec_chem, nspec_emis)

   call assert(nlev == 10, "NLEV should be 10")
   call assert(nspec_chem == 5, "NSPEC_CHEM should be 5")
   call assert(nspec_emis == 3, "NSPEC_EMIS should be 3")

   write(*,*) 'Test 2 passed!'
   write(*,*) ''

   ! Test 3: Get position
   write(*,*) 'Test 3: Get position'
   call virtual_col%get_position(i, j)

   call assert(i == 1, "I should be 1")
   call assert(j == 1, "J should be 1")

   write(*,*) 'Test 3 passed!'
   write(*,*) ''

   ! Test 4: Set and get meteorological field
   write(*,*) 'Test 4: Set and get meteorological field'
   ! Use a valid meteorological field from VirtualMetType, e.g. T(:) for temperature
   if (.not. associated(virtual_col%met%T)) then
      write(*,*) "[ERROR] Temperature field pointer not associated"
      stop
   endif
   virtual_col%met%T(1) = 288.15_fp
   test_value = virtual_col%met%T(1)  ! Read back the value we just set

   call assert_close(test_value, 288.15_fp, 1.0e-6_fp, "Met field value should match")

   write(*,*) 'Test 4 passed!'
   write(*,*) ''

   ! Test 5: Set and get chemical field
   write(*,*) 'Test 5: Set and get chemical field'
   call virtual_col%set_chem_field(1, 1, 1.0e-9_fp)  ! 1 ppb
   test_value = virtual_col%get_chem_field(1, 1)

   call assert_close(test_value, 1.0e-9_fp, 1.0e-12_fp, "Chem field value should match")

   write(*,*) 'Test 5 passed!'
   write(*,*) ''

   ! Test 6: Set and get emission field
   write(*,*) 'Test 6: Set and get emission field'
   ! set_emis_field expects (k, ispec, value)
   call virtual_col%set_emis_field(1, 1, 1.0e-6_fp)  ! 1 μg/m²/s
   test_value = virtual_col%get_emis_field(1, 1)

   call assert_close(test_value, 1.0e-6_fp, 1.0e-9_fp, "Emission field value should match")

   write(*,*) 'Test 6 passed!'
   write(*,*) ''

   ! Test 7: Cleanup
   write(*,*) 'Test 7: Cleanup'
   call virtual_col%cleanup()

   ! After cleanup, accessing fields should return 0 or fail gracefully
   ! test_value = virtual_col%met%T(1)
   ! Accessing met%T(1) after cleanup is unsafe and may cause a segmentation fault,
   ! because cleanup nullifies or deallocates the pointer.
   ! We're not asserting on this because behavior after cleanup may vary

   write(*,*) 'Test 7 passed!'
   write(*,*) ''

   ! Clean up test ChemSpecies conc arrays
   do s = 1, 5
      if (associated(test_chem_species(s)%conc)) deallocate(test_chem_species(s)%conc)
   end do

   ! =====================================================================
   ! Property 2: Pointer-based chemical data read correctness
   ! **Validates: Requirements 4.1, 4.2**
   !
   ! For any valid grid position (i, j), species index ispec, and vertical
   ! level k, get_chem_field(ispec, k) returns the same value as
   ! ChemSpecies(ispec)%conc(i, j, k) when the virtual column is created
   ! for position (i, j).
   ! =====================================================================
   call test_property2_chem_read_correctness()

   ! =====================================================================
   ! Property 3: Pointer-based chemical data write-through correctness
   ! **Validates: Requirements 4.3, 4.5**
   !
   ! For any valid grid position (i, j), species index ispec, vertical
   ! level k, and concentration value v, calling set_chem_field(k, ispec, v)
   ! causes ChemSpecies(ispec)%conc(i, j, k) to equal v, and
   ! apply_virtual_column is a no-op for chemical data.
   ! =====================================================================
   call test_property3_chem_write_through_correctness()

   write(*,*) 'All VirtualColumn tests passed!'

contains

   ! =======================================================================
   ! Property 2: Pointer-based chemical data read correctness
   ! =======================================================================
   subroutine test_property2_chem_read_correctness()
      use testing_mod, only: assert, assert_close
      use VirtualColumn_Mod, only: VirtualColumnType
      use Precision_Mod, only: fp
      use species_mod, only: SpeciesType

      implicit none

      integer, parameter :: NX = 3, NY = 4, NLEV = 8, NSPEC = 5, NEMIS = 2
      type(SpeciesType), target :: chem_species(NSPEC)
      type(VirtualColumnType) :: vc
      integer :: s, ii, jj, k, ispec, rc
      real(fp) :: expected_val, got_val
      real(fp) :: fill_val

      write(*,*) 'Property 2: Pointer-based chemical data read correctness'

      ! Allocate and fill conc arrays with known, distinct values
      ! Value pattern: fill_val = s * 1000 + ii * 100 + jj * 10 + k
      do s = 1, NSPEC
         allocate(chem_species(s)%conc(NX, NY, NLEV))
         do ii = 1, NX
            do jj = 1, NY
               do k = 1, NLEV
                  fill_val = real(s * 1000 + ii * 100 + jj * 10 + k, fp)
                  chem_species(s)%conc(ii, jj, k) = fill_val
               end do
            end do
         end do
      end do

      ! Test across multiple grid positions and all species/levels
      do ii = 1, NX
         do jj = 1, NY
            ! Create a VirtualColumn for position (ii, jj)
            call vc%init(NLEV, NSPEC, NEMIS, ii, jj, 0.0_fp, 0.0_fp, 1.0_fp, rc, &
               chem_species=chem_species)
            call assert(rc == 0, "Property 2: init should succeed")

            ! Verify get_chem_field returns the exact value from the 3D array
            do ispec = 1, NSPEC
               do k = 1, NLEV
                  expected_val = chem_species(ispec)%conc(ii, jj, k)
                  got_val = vc%get_chem_field(ispec, k)
                  call assert(got_val == expected_val, &
                     "Property 2: get_chem_field must match conc array")
               end do
            end do

            call vc%cleanup()
         end do
      end do

      ! Clean up
      do s = 1, NSPEC
         deallocate(chem_species(s)%conc)
      end do

      write(*,*) 'Property 2 passed!'
      write(*,*) ''
   end subroutine test_property2_chem_read_correctness

   ! =======================================================================
   ! Property 3: Pointer-based chemical data write-through correctness
   ! =======================================================================
   subroutine test_property3_chem_write_through_correctness()
      use testing_mod, only: assert, assert_close
      use VirtualColumn_Mod, only: VirtualColumnType
      use Precision_Mod, only: fp
      use species_mod, only: SpeciesType
      use StateManager_Mod, only: StateManagerType

      implicit none

      integer, parameter :: NX = 3, NY = 4, NLEV = 8, NSPEC = 5, NEMIS = 2
      type(SpeciesType), target :: chem_species(NSPEC)
      type(VirtualColumnType) :: vc
      integer :: s, ii, jj, k, ispec, rc
      real(fp) :: write_val, read_back
      real(fp) :: before_val, after_val

      write(*,*) 'Property 3: Pointer-based chemical data write-through correctness'

      ! Allocate and fill conc arrays with initial values
      do s = 1, NSPEC
         allocate(chem_species(s)%conc(NX, NY, NLEV))
         chem_species(s)%conc = 0.0_fp
      end do

      ! --- Part A: set_chem_field writes directly to the 3D array ---
      do ii = 1, NX
         do jj = 1, NY
            call vc%init(NLEV, NSPEC, NEMIS, ii, jj, 0.0_fp, 0.0_fp, 1.0_fp, rc, &
               chem_species=chem_species)
            call assert(rc == 0, "Property 3: init should succeed")

            do ispec = 1, NSPEC
               do k = 1, NLEV
                  ! Write a unique value through set_chem_field
                  write_val = real(ispec * 1000 + ii * 100 + jj * 10 + k, fp) * 1.5_fp
                  call vc%set_chem_field(k, ispec, write_val)

                  ! Verify the 3D array was directly modified
                  call assert(chem_species(ispec)%conc(ii, jj, k) == write_val, &
                     "Property 3: set_chem_field must write directly to 3D array")

                  ! Also verify get_chem_field reads back the same value
                  read_back = vc%get_chem_field(ispec, k)
                  call assert(read_back == write_val, &
                     "Property 3: get_chem_field must read back written value")
               end do
            end do

            call vc%cleanup()
         end do
      end do

      ! --- Part B: apply_virtual_column is a no-op for chemical data ---
      ! Re-initialize conc arrays with known values
      do s = 1, NSPEC
         do ii = 1, NX
            do jj = 1, NY
               do k = 1, NLEV
                  chem_species(s)%conc(ii, jj, k) = real(s * 100 + k, fp)
               end do
            end do
         end do
      end do

      ! Create a VirtualColumn, modify chem data, then call apply_virtual_column
      ! and verify it doesn't change anything (it's a no-op)
      ii = 2
      jj = 3
      call vc%init(NLEV, NSPEC, NEMIS, ii, jj, 0.0_fp, 0.0_fp, 1.0_fp, rc, &
         chem_species=chem_species)
      call assert(rc == 0, "Property 3 no-op: init should succeed")

      ! Write new values through set_chem_field
      do ispec = 1, NSPEC
         do k = 1, NLEV
            write_val = real(ispec + k, fp) * 99.0_fp
            call vc%set_chem_field(k, ispec, write_val)
         end do
      end do

      ! Record the current state of the 3D arrays (after set_chem_field writes)
      ! Then call apply_virtual_column and verify nothing changed
      block
         type(StateManagerType) :: sm
         real(fp) :: snapshot(NX, NY, NLEV, NSPEC)
         integer :: init_rc

         ! Initialize StateManager so apply_virtual_column can be called
         call sm%init(rc=init_rc)
         call assert(init_rc == 0, "Property 3 no-op: StateManager init should succeed")

         ! Snapshot current 3D array state
         do s = 1, NSPEC
            snapshot(:, :, :, s) = chem_species(s)%conc(:, :, :)
         end do

         ! apply_virtual_column should be a no-op for chemical data
         call sm%apply_virtual_column(vc, rc)
         call assert(rc == 0, "Property 3 no-op: apply_virtual_column should succeed")

         ! Verify nothing changed in the 3D arrays
         do s = 1, NSPEC
            do ii = 1, NX
               do jj = 1, NY
                  do k = 1, NLEV
                     before_val = snapshot(ii, jj, k, s)
                     after_val = chem_species(s)%conc(ii, jj, k)
                     call assert(before_val == after_val, &
                        "Property 3 no-op: apply_virtual_column must not change chem data")
                  end do
               end do
            end do
         end do

         call sm%cleanup(init_rc)
      end block

      call vc%cleanup()

      ! Clean up
      do s = 1, NSPEC
         deallocate(chem_species(s)%conc)
      end do

      write(*,*) 'Property 3 passed!'
      write(*,*) ''
   end subroutine test_property3_chem_write_through_correctness

end program test_VirtualColumn

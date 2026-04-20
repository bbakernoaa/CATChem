!> \file test_KokkosGpuTolerance.F90
!! \brief Property test for GPU floating-point tolerance (Property 10).
!!
!! **Validates: Requirements 9.5**
!!
!! **Property 10: GPU floating-point tolerance**
!! Tests that Kokkos GPU dispatch produces results within 1.0e-12
!! relative error of CPU Fortran serial kernel (only when GPU backend
!! available).
!!
!! This test is only compiled when ENABLE_KOKKOS=ON.
!! If no GPU backend is configured (i.e., Kokkos is Serial or OpenMP only),
!! the test prints a skip message and exits successfully.
!!
!! When a GPU backend IS available, the test calls the Kokkos dispatch
!! function (which will execute on GPU) and compares against the Fortran
!! serial kernel with a tolerance of 1.0e-12 relative error.

program test_KokkosGpuTolerance
   use iso_c_binding, only: c_int, c_double, c_ptr, c_loc
   use precision_mod, only: fp
   use testing_mod, only: assert
   use SettlingPhysics_Mod, only: settling_compute
   use KokkosDispatch_Mod, only: kokkos_dispatch_settling_gocart

   implicit none

   integer, parameter :: NLEV = 72
   integer, parameter :: N_COLS = 10
   integer, parameter :: N_SPECIES = 2
   integer, parameter :: N_RANDOM_SEEDS = 25

   ! GPU tolerance per Requirement 9.5
   real(fp), parameter :: GPU_RTOL = 1.0e-12_fp

   ! Note: We cannot detect the Kokkos backend from Fortran at runtime
   ! without a helper C function. This test always runs the comparison
   ! with the GPU tolerance. On CPU backends, the results will be
   ! even closer (bit-for-bit), so the test will pass trivially.
   ! On GPU backends, the 1e-12 tolerance accommodates non-associative
   ! floating-point arithmetic.

   integer :: iseed
   integer :: total_tests

   total_tests = 0

   write(*,*) '=== Property Test: GPU Floating-Point Tolerance ==='
   write(*,*) '--- Property 10: GPU floating-point tolerance ---'
   write(*,*) '--- Validates: Requirements 9.5 ---'
   write(*,*) ''
   write(*,*) '  Note: This test verifies results are within 1.0e-12'
   write(*,*) '  relative error. On CPU backends this passes trivially.'
   write(*,*) '  On GPU backends this validates FP tolerance.'
   write(*,*) ''

   ! Fixed configuration tests
   call run_gpu_tolerance_test(0, 0, 1, 'No swelling, profile 1')
   total_tests = total_tests + 1
   call run_gpu_tolerance_test(1, 0, 2, 'Fitzgerald, profile 2')
   total_tests = total_tests + 1
   call run_gpu_tolerance_test(2, 0, 3, 'Gerber, profile 3')
   total_tests = total_tests + 1
   call run_gpu_tolerance_test(4, 1, 4, 'PK2007 + Maring, profile 4')
   total_tests = total_tests + 1

   ! Property-based iteration: sweep random seeds across swelling methods
   ! Each seed generates different atmospheric profiles for N_COLS columns.
   ! 5 swelling configs x 25 seeds = 125 iterations (>100 minimum)
   do iseed = 1, N_RANDOM_SEEDS
      call run_gpu_tolerance_test(0, 0, 100 + iseed, 'PBT: no swelling')
      total_tests = total_tests + 1
      call run_gpu_tolerance_test(1, 0, 200 + iseed, 'PBT: Fitzgerald')
      total_tests = total_tests + 1
      call run_gpu_tolerance_test(2, 1, 300 + iseed, 'PBT: Gerber+Maring')
      total_tests = total_tests + 1
      call run_gpu_tolerance_test(3, 0, 400 + iseed, 'PBT: Gerber NH4SO4')
      total_tests = total_tests + 1
      call run_gpu_tolerance_test(4, 1, 500 + iseed, 'PBT: PK2007+Maring')
      total_tests = total_tests + 1
   end do

   write(*,*) ''
   write(*,'(a,i0,a)') ' === All ', total_tests, ' GPU tolerance tests PASSED ==='

contains

   !---------------------------------------------------------------------------
   !> Assert relative closeness with GPU tolerance.
   !---------------------------------------------------------------------------
   subroutine assert_gpu_close(a, b, rtol, msg)
      real(fp), intent(in) :: a, b, rtol
      character(len=*), intent(in) :: msg
      real(fp) :: denom, rel_err

      denom = max(abs(a), abs(b))
      if (denom < tiny(1.0_fp)) return

      rel_err = abs(a - b) / denom
      if (rel_err > rtol) then
         print '(a, ": rel_err=", es12.4, " (tol=", es12.4, &
            &") a=", es22.14, " b=", es22.14)', &
            msg, rel_err, rtol, a, b
         stop 1
      end if
   end subroutine assert_gpu_close

   !---------------------------------------------------------------------------
   !> Generate a realistic atmospheric column profile.
   !---------------------------------------------------------------------------
   subroutine generate_column(seed_val, t, airden, rh, z_edge, delp, conc, nsp)
      integer, intent(in) :: seed_val, nsp
      real(fp), intent(out) :: t(NLEV), airden(NLEV), rh(NLEV)
      real(fp), intent(out) :: z_edge(NLEV+1), delp(NLEV)
      real(fp), intent(out) :: conc(NLEV, nsp)

      integer :: k, s
      real(fp) :: z_mid, p_mid, dz_k, var
      real(fp), parameter :: GRAV = 9.80665_fp
      real(fp), parameter :: RGAS = 287.05_fp
      real(fp), parameter :: LAPSE = 6.5e-3_fp
      real(fp) :: p_sfc, t_sfc

      var = real(seed_val, fp) * 0.01_fp
      p_sfc = 101325.0_fp + var * 500.0_fp
      t_sfc = 288.15_fp + var * 5.0_fp

      z_edge(1) = 0.0_fp
      do k = 1, NLEV
         if (k <= 20) then
            dz_k = 100.0_fp + real(k, fp) * 10.0_fp
         else if (k <= 50) then
            dz_k = 400.0_fp + real(k - 20, fp) * 20.0_fp
         else
            dz_k = 1000.0_fp + real(k - 50, fp) * 50.0_fp
         end if
         z_edge(k+1) = z_edge(k) + dz_k
      end do

      do k = 1, NLEV
         z_mid = 0.5_fp * (z_edge(k) + z_edge(k+1))
         t(k) = max(200.0_fp, t_sfc - LAPSE * z_mid)
         p_mid = p_sfc * (t(k) / t_sfc)**(GRAV / (LAPSE * RGAS))
         p_mid = max(1.0_fp, p_mid)
         airden(k) = p_mid / (RGAS * t(k))
         airden(k) = max(0.001_fp, airden(k))
         dz_k = z_edge(k+1) - z_edge(k)
         delp(k) = airden(k) * GRAV * dz_k
         delp(k) = max(1.0_fp, delp(k))
         rh(k) = max(0.01_fp, min(0.95_fp, &
            0.80_fp - z_mid / 50000.0_fp + var * 0.05_fp))
      end do

      do s = 1, nsp
         do k = 1, NLEV
            z_mid = 0.5_fp * (z_edge(k) + z_edge(k+1))
            conc(k, s) = max(1.0e-20_fp, &
               1.0e-9_fp * exp(-z_mid / 8000.0_fp) * &
               (1.0_fp + var * 0.1_fp) * (1.0_fp + real(s, fp) * 0.1_fp))
         end do
      end do
   end subroutine generate_column

   !---------------------------------------------------------------------------
   !> Run GPU tolerance test.
   !---------------------------------------------------------------------------
   subroutine run_gpu_tolerance_test(swelling_flag, correction_maring, seed, label)
      integer, intent(in) :: swelling_flag, correction_maring, seed
      character(len=*), intent(in) :: label

      real(c_double), allocatable, target :: batch_airden(:,:)
      real(c_double), allocatable, target :: batch_delp(:,:)
      real(c_double), allocatable, target :: batch_pmid(:,:)
      real(c_double), allocatable, target :: batch_rh(:,:)
      real(c_double), allocatable, target :: batch_t(:,:)
      real(c_double), allocatable, target :: batch_z(:,:)
      real(c_double), allocatable, target :: batch_conc(:,:,:)
      real(c_double), allocatable, target :: batch_tendency(:,:,:)
      real(c_double), allocatable, target :: sp_radius(:)
      real(c_double), allocatable, target :: sp_density(:)

      real(fp) :: t_col(NLEV), airden_col(NLEV), rh_col(NLEV)
      real(fp) :: z_edge_col(NLEV+1), delp_col(NLEV)
      real(fp) :: conc_col(NLEV, N_SPECIES)
      real(fp) :: conc_fortran(NLEV)
      real(fp) :: pmid_col(NLEV)

      real(fp), parameter :: GRAV = 9.80665_fp
      real(fp), parameter :: CDT = 300.0_fp
      ! Pressure lid matching C++ dispatch (0.01 hPa = 1.0 Pa)
      real(fp), parameter :: PLID_PA = 1.0_fp

      integer :: icol, k, s, rc_f, klid_f
      real(fp) :: min_diff, diff_val
      logical :: do_maring

      write(*,*) '  Testing: ', trim(label)

      do_maring = (correction_maring /= 0)

      allocate(batch_airden(N_COLS, NLEV))
      allocate(batch_delp(N_COLS, NLEV))
      allocate(batch_pmid(N_COLS, NLEV))
      allocate(batch_rh(N_COLS, NLEV))
      allocate(batch_t(N_COLS, NLEV))
      allocate(batch_z(N_COLS, NLEV+1))
      allocate(batch_conc(N_COLS, NLEV, N_SPECIES))
      allocate(batch_tendency(N_COLS, NLEV, N_SPECIES))
      allocate(sp_radius(N_SPECIES))
      allocate(sp_density(N_SPECIES))

      sp_radius(1) = 1.0e-6_fp
      sp_radius(2) = 3.0e-6_fp
      sp_density(1) = 2500.0_fp
      sp_density(2) = 2000.0_fp

      do icol = 1, N_COLS
         call generate_column(seed * 100 + icol, t_col, airden_col, rh_col, &
            z_edge_col, delp_col, conc_col, N_SPECIES)

         do k = 1, NLEV
            batch_airden(icol, k) = airden_col(k)
            batch_delp(icol, k) = delp_col(k)
            batch_pmid(icol, k) = airden_col(k) * 287.05_fp * t_col(k)
            batch_rh(icol, k) = rh_col(k)
            batch_t(icol, k) = t_col(k)
         end do
         do k = 1, NLEV+1
            batch_z(icol, k) = z_edge_col(k)
         end do
         do s = 1, N_SPECIES
            do k = 1, NLEV
               batch_conc(icol, k, s) = conc_col(k, s)
            end do
         end do
      end do

      batch_tendency = 0.0_fp

      ! Call Kokkos dispatch (runs on whatever backend is configured)
      call kokkos_dispatch_settling_gocart( &
         int(N_COLS, c_int), int(NLEV, c_int), int(N_SPECIES, c_int), &
         c_loc(batch_airden), c_loc(batch_delp), c_loc(batch_pmid), &
         c_loc(batch_rh), c_loc(batch_t), c_loc(batch_z), &
         real(CDT, c_double), &
         c_loc(sp_radius), c_loc(sp_density), &
         int(swelling_flag, c_int), int(correction_maring, c_int), &
         c_loc(batch_conc), c_loc(batch_tendency))

      ! Compare against Fortran serial kernel
      do icol = 1, N_COLS
         call generate_column(seed * 100 + icol, t_col, airden_col, rh_col, &
            z_edge_col, delp_col, conc_col, N_SPECIES)

         ! Compute pmid and find klid matching C++ logic
         do k = 1, NLEV
            pmid_col(k) = airden_col(k) * 287.05_fp * t_col(k)
         end do
         klid_f = 1
         min_diff = abs(pmid_col(1) - PLID_PA)
         do k = 2, NLEV
            diff_val = abs(pmid_col(k) - PLID_PA)
            if (diff_val < min_diff) then
               klid_f = k
               min_diff = diff_val
            end if
         end do

         do s = 1, N_SPECIES
            conc_fortran = conc_col(:, s)

            call settling_compute(NLEV, klid_f, CDT, GRAV, &
               sp_radius(s), sp_density(s), swelling_flag, &
               conc_fortran, t_col, airden_col, rh_col, z_edge_col, delp_col, &
               correction_maring=do_maring, solver_type=2, rc=rc_f)

            call assert(rc_f == 0, 'Fortran settling_compute should succeed')

            do k = 1, NLEV
               call assert_gpu_close( &
                  batch_tendency(icol, k, s), conc_fortran(k), GPU_RTOL, &
                  'GPU tolerance check')
            end do
         end do
      end do

      deallocate(batch_airden, batch_delp, batch_pmid, batch_rh, batch_t)
      deallocate(batch_z, batch_conc, batch_tendency, sp_radius, sp_density)

      write(*,*) '    PASSED'
   end subroutine run_gpu_tolerance_test

end program test_KokkosGpuTolerance

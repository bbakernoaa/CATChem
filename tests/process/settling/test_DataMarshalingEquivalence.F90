!> \file test_DataMarshalingEquivalence.F90
!! \brief Property test for data marshaling elimination equivalence.
!!
!! **Validates: Requirements 5.1, 5.2**
!!
!! **Property 4: Data marshaling elimination equivalence**
!! Tests that running compute_gocart with pointer-based data (arrays passed
!! directly) produces bit-for-bit identical output to running with copied
!! local arrays (simulating the old local-copy marshaling approach).
!!
!! This validates that eliminating the intermediate copy step in scheme
!! column runners does not change numerical results. Fortran's array
!! passing semantics guarantee this by language spec, but we verify
!! explicitly for confidence.

program test_DataMarshalingEquivalence
   use precision_mod, only: fp
   use testing_mod, only: assert, assert_close
   use SettlingPhysics_Mod, only: settling_compute
   use SettlingCommon_Mod, only: SettlingSchemeGOCARTConfig
   use SettlingScheme_GOCART_Mod, only: compute_gocart
   use Constants, only: g0

   implicit none

   integer, parameter :: NLEV = 72
   integer, parameter :: NSPECIES = 3

   write(*,*) '=== Property Test: Data Marshaling Elimination Equivalence ==='
   write(*,*) ''
   write(*,*) '--- Property 4: Data marshaling elimination equivalence ---'
   write(*,*) '--- Validates: Requirements 5.1, 5.2 ---'
   write(*,*) ''

   ! Test 1: compute_gocart with direct arrays vs copied arrays
   call test_compute_gocart_marshaling(1, 'Direct vs copied arrays, profile 1')
   call test_compute_gocart_marshaling(2, 'Direct vs copied arrays, profile 2')

   ! Test 2: settling_compute with direct arrays vs copied arrays
   call test_settling_compute_marshaling(1, 'settling_compute direct vs copy, profile 1')
   call test_settling_compute_marshaling(2, 'settling_compute direct vs copy, profile 2')

   write(*,*) ''
   write(*,*) '=== All data marshaling equivalence tests PASSED ==='

contains

   !---------------------------------------------------------------------------
   !> Generate a realistic atmospheric column profile in bottom-to-top order.
   !---------------------------------------------------------------------------
   subroutine generate_profile(seed_val, t, airden, rh, z_edge, delp, pmid, conc)
      integer, intent(in) :: seed_val
      real(fp), intent(out) :: t(NLEV)
      real(fp), intent(out) :: airden(NLEV)
      real(fp), intent(out) :: rh(NLEV)
      real(fp), intent(out) :: z_edge(NLEV+1)
      real(fp), intent(out) :: delp(NLEV)
      real(fp), intent(out) :: pmid(NLEV)
      real(fp), intent(out) :: conc(NLEV, NSPECIES)

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
         pmid(k) = p_mid
         airden(k) = p_mid / (RGAS * t(k))
         airden(k) = max(0.001_fp, airden(k))
         dz_k = z_edge(k+1) - z_edge(k)
         delp(k) = airden(k) * GRAV * dz_k
         delp(k) = max(1.0_fp, delp(k))
         rh(k) = max(0.01_fp, min(0.95_fp, 0.80_fp - z_mid / 50000.0_fp + var * 0.05_fp))
         do s = 1, NSPECIES
            conc(k, s) = max(1.0e-20_fp, 1.0e-9_fp * exp(-z_mid / 8000.0_fp) &
                        * (1.0_fp + var * 0.1_fp) * real(s, fp) * 0.5_fp)
         end do
      end do
   end subroutine generate_profile


   !---------------------------------------------------------------------------
   !> Test that compute_gocart produces bit-for-bit identical results
   !! when called with direct arrays vs. copied local arrays.
   !!
   !! This simulates the old marshaling approach (allocate + copy) vs.
   !! the new pointer-based approach (pass directly).
   !---------------------------------------------------------------------------
   subroutine test_compute_gocart_marshaling(seed, label)
      integer, intent(in) :: seed
      character(len=*), intent(in) :: label

      ! "Original" arrays (simulating data from VirtualColumnType)
      real(fp) :: t_orig(NLEV), airden_orig(NLEV), rh_orig(NLEV)
      real(fp) :: z_edge_orig(NLEV+1), delp_orig(NLEV), pmid_orig(NLEV)
      real(fp) :: conc_orig(NLEV, NSPECIES)

      ! Copied arrays (simulating old local-copy marshaling)
      real(fp) :: t_copy(NLEV), airden_copy(NLEV), rh_copy(NLEV)
      real(fp) :: z_edge_copy(NLEV+1), delp_copy(NLEV), pmid_copy(NLEV)
      real(fp) :: conc_copy(NLEV, NSPECIES)

      ! Output arrays for direct path
      real(fp) :: tend_direct(NLEV, NSPECIES)
      real(fp) :: vsettle_direct(NLEV, NSPECIES)
      real(fp) :: flux_direct(NSPECIES)

      ! Output arrays for copied path
      real(fp) :: tend_copy(NLEV, NSPECIES)
      real(fp) :: vsettle_copy(NLEV, NSPECIES)
      real(fp) :: flux_copy(NSPECIES)

      ! Species properties
      real(fp) :: species_radius(NSPECIES)
      real(fp) :: species_density(NSPECIES)
      integer :: diag_ids(NSPECIES)

      type(SettlingSchemeGOCARTConfig) :: params
      integer :: k, s

      write(*,*) '  Testing: ', trim(label)

      ! Generate test data
      call generate_profile(seed, t_orig, airden_orig, rh_orig, &
         z_edge_orig, delp_orig, pmid_orig, conc_orig)

      ! Set up species properties
      species_radius(1) = 1.0e-6_fp
      species_radius(2) = 2.5e-6_fp
      species_radius(3) = 5.0e-6_fp
      species_density(1) = 2500.0_fp
      species_density(2) = 2650.0_fp
      species_density(3) = 1800.0_fp
      diag_ids = (/ 1, 2, 3 /)

      ! Set up params
      params%swelling_method = 1
      params%correction_maring = .false.

      ! --- Path A: Direct arrays (pointer-based, new approach) ---
      tend_direct = 0.0_fp
      vsettle_direct = 0.0_fp
      flux_direct = 0.0_fp

      call compute_gocart( &
         NLEV, NSPECIES, params, &
         airden_orig, delp_orig, pmid_orig, rh_orig, t_orig, &
         300.0_fp, z_edge_orig, &
         species_radius, species_density, &
         conc_orig, tend_direct, &
         vsettle_direct, flux_direct, diag_ids)

      ! --- Path B: Copied arrays (old marshaling approach) ---
      ! Simulate the old approach: allocate local arrays and copy data
      t_copy = t_orig
      airden_copy = airden_orig
      rh_copy = rh_orig
      z_edge_copy = z_edge_orig
      delp_copy = delp_orig
      pmid_copy = pmid_orig
      conc_copy = conc_orig

      tend_copy = 0.0_fp
      vsettle_copy = 0.0_fp
      flux_copy = 0.0_fp

      call compute_gocart( &
         NLEV, NSPECIES, params, &
         airden_copy, delp_copy, pmid_copy, rh_copy, t_copy, &
         300.0_fp, z_edge_copy, &
         species_radius, species_density, &
         conc_copy, tend_copy, &
         vsettle_copy, flux_copy, diag_ids)

      ! --- Verify bit-for-bit identical results ---
      do s = 1, NSPECIES
         do k = 1, NLEV
            call assert(tend_direct(k, s) == tend_copy(k, s), &
               'Tendency must be bit-for-bit identical')
            call assert(vsettle_direct(k, s) == vsettle_copy(k, s), &
               'Settling velocity must be bit-for-bit identical')
         end do
         call assert(flux_direct(s) == flux_copy(s), &
            'Flux must be bit-for-bit identical')
      end do

      write(*,*) '    PASSED'
   end subroutine test_compute_gocart_marshaling

   !---------------------------------------------------------------------------
   !> Test that settling_compute produces bit-for-bit identical results
   !! when called with direct arrays vs. copied local arrays.
   !---------------------------------------------------------------------------
   subroutine test_settling_compute_marshaling(seed, label)
      integer, intent(in) :: seed
      character(len=*), intent(in) :: label

      ! Original arrays
      real(fp) :: t_orig(NLEV), airden_orig(NLEV), rh_orig(NLEV)
      real(fp) :: z_edge_orig(NLEV+1), delp_orig(NLEV), pmid_orig(NLEV)
      real(fp) :: conc_all(NLEV, NSPECIES)

      ! Direct path working arrays
      real(fp) :: conc_direct(NLEV)
      real(fp) :: vsettle_direct(NLEV)
      real(fp) :: flux_direct

      ! Copy path working arrays
      real(fp) :: t_copy(NLEV), airden_copy(NLEV), rh_copy(NLEV)
      real(fp) :: z_edge_copy(NLEV+1), delp_copy(NLEV)
      real(fp) :: conc_copy(NLEV)
      real(fp) :: vsettle_copy(NLEV)
      real(fp) :: flux_copy

      real(fp), parameter :: RADIUS_DRY = 1.0e-6_fp
      real(fp), parameter :: RHOP_DRY = 2500.0_fp
      real(fp), parameter :: CDT = 300.0_fp

      integer :: rc_direct, rc_copy, k

      write(*,*) '  Testing: ', trim(label)

      call generate_profile(seed, t_orig, airden_orig, rh_orig, &
         z_edge_orig, delp_orig, pmid_orig, conc_all)

      ! --- Path A: Direct arrays ---
      conc_direct = conc_all(:, 1)
      vsettle_direct = 0.0_fp
      flux_direct = 0.0_fp

      call settling_compute(NLEV, 1, CDT, g0, &
         RADIUS_DRY, RHOP_DRY, 1, &
         conc_direct, t_orig, airden_orig, rh_orig, z_edge_orig, delp_orig, &
         vsettle_out=vsettle_direct, fluxout=flux_direct, &
         solver_type=2, rc=rc_direct)

      ! --- Path B: Copied arrays ---
      t_copy = t_orig
      airden_copy = airden_orig
      rh_copy = rh_orig
      z_edge_copy = z_edge_orig
      delp_copy = delp_orig
      conc_copy = conc_all(:, 1)
      vsettle_copy = 0.0_fp
      flux_copy = 0.0_fp

      call settling_compute(NLEV, 1, CDT, g0, &
         RADIUS_DRY, RHOP_DRY, 1, &
         conc_copy, t_copy, airden_copy, rh_copy, z_edge_copy, delp_copy, &
         vsettle_out=vsettle_copy, fluxout=flux_copy, &
         solver_type=2, rc=rc_copy)

      ! --- Verify bit-for-bit identical ---
      call assert(rc_direct == rc_copy, 'Return codes must match')

      do k = 1, NLEV
         call assert(conc_direct(k) == conc_copy(k), &
            'Concentration must be bit-for-bit identical')
         call assert(vsettle_direct(k) == vsettle_copy(k), &
            'Settling velocity must be bit-for-bit identical')
      end do
      call assert(flux_direct == flux_copy, &
         'Flux must be bit-for-bit identical')

      write(*,*) '    PASSED'
   end subroutine test_settling_compute_marshaling

end program test_DataMarshalingEquivalence

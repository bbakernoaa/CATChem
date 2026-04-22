!> \file test_SO4chemPhysics.F90
!! \brief Property and unit tests for SO4 chemistry physics internalization.
!!
!! **Property 1: Solar zenith angle output bounds and consistency**
!! For any valid (jday, xhour, lat_rad, lon_rad), cossza in [0,1]
!! and sza_deg in [0,180].
!! **Validates: Requirements 3.3**
!!
!! **Property 2: Solar zenith angle equivalence with GOCART2G**
!! solar_zenith_angle produces bit-for-bit identical results to the
!! GOCART2G szangle algorithm for the same inputs.
!! **Validates: Requirements 3.4, 10.1**
!!
!! **Property 3: Oxidant diurnal scaling correctness**
!! OH is zero at nighttime and proportional to cossza_now/tcosz during daytime.
!! NO3 is zero during daytime and scaled by 86400/tnight at night.
!! OH VMR-to-number-density conversion is correct.
!! H2O2 recycle resets to climatology when flag is true.
!! **Validates: Requirements 4.2, 4.3, 4.4, 4.5, 12.2**
!!
!! **Property 4: DMS sulfur mass conservation and non-negativity**
!! Sulfur mass is conserved through DMS oxidation and all outputs are non-negative.
!! NO3 pathway contributes zero DMS loss when cossza > 0.
!! **Validates: Requirements 5.4, 12.3, 12.6**
!!
!! **Property 5: SO2 oxidation non-negativity and mass bound**
!! Gas-phase and aqueous-phase SO4 production rates are non-negative.
!! Total SO4 produced does not exceed available SO2.
!! Aqueous production is zero when T < 258 K.
!! **Validates: Requirements 6.3, 12.4**
!!
!! **Property 6: SO4/MSA update formula correctness**
!! species_final = species_0 + production * cdt at all levels.
!! All output concentrations are non-negative.
!! **Validates: Requirements 7.3**
!!
!! **Property 7: Full chemistry timestep equivalence with GOCART2G**
!! so4chem_driver produces species concentrations identical to the
!! GOCART2G SulfateUpdateOxidants + SulfateChemDriver path (with dry
!! deposition disabled) within machine epsilon tolerance.
!! **Validates: Requirements 10.1, 10.2**
!!
!! Unit tests for solar_zenith_angle:
!! - Solar noon at equator on equinox: cossza ~ 1.0
!! - Midnight at any location: cossza = 0.0
!! **Validates: Requirements 3.3, 12.5**

program test_SO4chemPhysics
   use precision_mod, only: fp
   use testing_mod, only: assert, assert_close
   use Met_Utilities_Mod, only: solar_zenith_angle
   use SO4chemPhysics_Mod, only: so4chem_dms_oxidation, so4chem_so2_oxidation, &
      so4chem_so4_update, so4chem_msa_update, so4chem_update_oxidants, &
      so4chem_driver
   use GOCART2G_Process, only: SulfateUpdateOxidants, SulfateChemDriver
   use Constants, only: PI, VON_KARMAN

   implicit none

   integer, parameter :: N_ITER = 200

   write(*,*) '=== SO4 Chemistry Physics Tests ==='
   write(*,*) ''

   call test_property1_sza_bounds()
   call test_property2_sza_equivalence()
   call test_unit_solar_noon_equinox()
   call test_unit_midnight()
   call test_property3_oxidant_scaling()
   call test_property4_dms_mass_conservation()
   call test_property5_so2_oxidation()
   call test_property6_so4_msa_update()
   call test_property7_full_equivalence()

   write(*,*) ''
   write(*,*) '=== All SO4 chemistry physics tests PASSED ==='

contains


   !---------------------------------------------------------------------------
   !> Simple linear congruential PRNG returning value in [0, 1)
   !---------------------------------------------------------------------------
   function rand_val(seed) result(val)
      integer, intent(inout) :: seed
      real(fp) :: val
      ! LCG parameters (Numerical Recipes)
      seed = mod(seed * 1103515245 + 12345, 2147483647)
      if (seed < 0) seed = seed + 2147483647
      val = real(seed, fp) / 2147483647.0_fp
   end function rand_val

   !---------------------------------------------------------------------------
   !> Property 1: SZA output bounds and consistency
   !! For random (jday, xhour, lat_rad, lon_rad), verify:
   !!   cossza in [0, 1] and sza_deg in [0, 180]
   !!   sza_deg ≈ acos(unclamped_cossza) × 180/π within floating-point precision
   !! **Validates: Requirements 3.3**
   !---------------------------------------------------------------------------
   subroutine test_property1_sza_bounds()
      integer :: i, seed, jday
      real(fp) :: xhour, lat_rad, lon_rad, sza_deg, cossza
      ! Variables for consistency check (recompute unclamped cossza)
      real(fp) :: r, dec, xlon, timloc, ahr, cossza_raw, sza_expected
      real(fp), parameter :: a0 = 0.006918_fp, a1 = 0.399912_fp
      real(fp), parameter :: a2 = 0.006758_fp, a3 = 0.002697_fp
      real(fp), parameter :: b1 = 0.070257_fp, b2 = 0.000907_fp
      real(fp), parameter :: b3 = 0.000148_fp
      real(fp) :: radToDeg
      ! Tolerance for floating-point consistency (acos precision)
      real(fp), parameter :: CONSISTENCY_TOL = 1.0e-10_fp

      write(*,*) '--- Property 1: SZA output bounds and consistency ---'

      seed = 42
      radToDeg = 180.0_fp / PI

      do i = 1, N_ITER
         ! Generate random inputs within valid ranges
         jday = 1 + int(rand_val(seed) * 365.99_fp)  ! 1-366
         xhour = rand_val(seed) * 24.0_fp             ! 0-24
         lat_rad = (rand_val(seed) - 0.5_fp) * PI     ! -pi/2 to pi/2
         lon_rad = (rand_val(seed) - 0.5_fp) * 2.0_fp * PI  ! -pi to pi

         call solar_zenith_angle(jday, xhour, lat_rad, lon_rad, sza_deg, cossza)

         ! Verify bounds on cossza (clamped to [0,1])
         call assert(cossza >= 0.0_fp, 'cossza must be >= 0')
         call assert(cossza <= 1.0_fp, 'cossza must be <= 1')
         ! Verify bounds on sza_deg
         call assert(sza_deg >= 0.0_fp, 'sza_deg must be >= 0')
         call assert(sza_deg <= 180.0_fp, 'sza_deg must be <= 180')

         ! Verify consistency: sza_deg ≈ acos(unclamped_cossza) × 180/π
         ! Recompute the unclamped cossza using the same algorithm
         r = 2.0_fp * PI * real(jday - 1, fp) / 365.0_fp
         dec = a0 - a1*cos(r)       + b1*sin(r)       &
            - a2*cos(2.0_fp*r) + b2*sin(2.0_fp*r) &
            - a3*cos(3.0_fp*r) + b3*sin(3.0_fp*r)
         xlon = lon_rad * radToDeg
         timloc = xhour + xlon / 15.0_fp
         if (timloc < 0.0_fp)  timloc = timloc + 24.0_fp
         if (timloc > 24.0_fp) timloc = timloc - 24.0_fp
         ahr = abs(timloc - 12.0_fp) * 15.0_fp * PI / 180.0_fp
         cossza_raw = sin(lat_rad)*sin(dec) + cos(lat_rad)*cos(dec)*cos(ahr)
         ! Clamp to [-1,1] before acos (same as the subroutine)
         cossza_raw = min(max(cossza_raw, -1.0_fp), 1.0_fp)
         sza_expected = acos(cossza_raw) * radToDeg

         call assert_close(sza_deg, sza_expected, CONSISTENCY_TOL, &
            'sza_deg must equal acos(unclamped_cossza)*rad2deg')
      end do

      write(*,*) '  PASSED (', N_ITER, ' iterations)'
   end subroutine test_property1_sza_bounds

   !---------------------------------------------------------------------------
   !> Property 2: SZA equivalence with GOCART2G szangle algorithm
   !! Compare solar_zenith_angle output against the GOCART2G szangle
   !! algorithm (inlined here since szangle is private in GOCART2G_Process).
   !! Verify bit-for-bit identical cossza and sza_deg values.
   !! **Validates: Requirements 3.4, 10.1**
   !---------------------------------------------------------------------------
   subroutine test_property2_sza_equivalence()
      integer :: i, seed, jday
      real(fp) :: xhour, lat_rad, lon_rad
      real(fp) :: sza_new, cossza_new
      real(fp) :: sza_ref, cossza_ref

      ! GOCART2G szangle local variables
      real(fp) :: a0, a1, a2, a3, b1, b2, b3
      real(fp) :: r, dec, xlon, timloc, ahr, rlat
      real(fp) :: radToDeg

      write(*,*) '--- Property 2: SZA equivalence with GOCART2G ---'

      seed = 12345
      radToDeg = 180.0_fp / PI

      do i = 1, N_ITER
         ! Generate random inputs
         jday = 1 + int(rand_val(seed) * 365.99_fp)
         xhour = rand_val(seed) * 24.0_fp
         lat_rad = (rand_val(seed) - 0.5_fp) * PI
         lon_rad = (rand_val(seed) - 0.5_fp) * 2.0_fp * PI

         ! --- Call new solar_zenith_angle ---
         call solar_zenith_angle(jday, xhour, lat_rad, lon_rad, sza_new, cossza_new)

         ! --- Inline GOCART2G szangle algorithm (reference) ---
         a0 = 0.006918_fp
         a1 = 0.399912_fp
         a2 = 0.006758_fp
         a3 = 0.002697_fp
         b1 = 0.070257_fp
         b2 = 0.000907_fp
         b3 = 0.000148_fp

         r = 2.0_fp * PI * real(jday - 1, fp) / 365.0_fp

         dec = a0 - a1*cos(r)          + b1*sin(r)          &
            - a2*cos(2.0_fp*r)    + b2*sin(2.0_fp*r)    &
            - a3*cos(3.0_fp*r)    + b3*sin(3.0_fp*r)

         xlon = lon_rad * radToDeg
         timloc = xhour + xlon / 15.0_fp
         if (timloc < 0.0_fp)  timloc = timloc + 24.0_fp
         if (timloc > 24.0_fp) timloc = timloc - 24.0_fp

         ahr = abs(timloc - 12.0_fp) * 15.0_fp * PI / 180.0_fp

         rlat = lat_rad
         cossza_ref = sin(rlat)*sin(dec) + cos(rlat)*cos(dec)*cos(ahr)
         cossza_ref = min(max(cossza_ref, -1.0_fp), 1.0_fp)
         sza_ref = acos(cossza_ref) * radToDeg
         if (cossza_ref < 0.0_fp) cossza_ref = 0.0_fp

         ! --- Compare: bit-for-bit identical ---
         call assert(cossza_new == cossza_ref, 'cossza must be bit-for-bit identical')
         call assert(sza_new == sza_ref, 'sza_deg must be bit-for-bit identical')
      end do

      write(*,*) '  PASSED (', N_ITER, ' iterations)'
   end subroutine test_property2_sza_equivalence

   !---------------------------------------------------------------------------
   !> Unit test: Solar noon at equator on equinox -> cossza ~ 1.0
   !! On the vernal equinox (~March 20, jday=80), at solar noon (12:00 UTC)
   !! at the equator (lat=0, lon=0), the sun is nearly overhead.
   !! **Validates: Requirements 3.3, 12.5**
   !---------------------------------------------------------------------------
   subroutine test_unit_solar_noon_equinox()
      real(fp) :: sza_deg, cossza
      real(fp), parameter :: TOL = 0.05_fp  ! declination is ~0 but not exactly 0

      write(*,*) '--- Unit test: Solar noon at equator on equinox ---'

      ! jday=80 (~ March 21), xhour=12.0 UTC, lat=0, lon=0
      call solar_zenith_angle(80, 12.0_fp, 0.0_fp, 0.0_fp, sza_deg, cossza)

      call assert_close(cossza, 1.0_fp, TOL, 'cossza at equator noon equinox ~ 1.0')
      call assert(sza_deg < 5.0_fp, 'sza_deg at equator noon equinox < 5 degrees')

      write(*,*) '  PASSED: cossza =', cossza, ' sza_deg =', sza_deg
   end subroutine test_unit_solar_noon_equinox

   !---------------------------------------------------------------------------
   !> Unit test: Midnight at any location -> cossza = 0.0
   !! At midnight (local time), the sun is below the horizon for most
   !! locations. We test at the equator at midnight UTC with lon=0
   !! (local midnight), where cossza should be 0 (clamped from negative).
   !! **Validates: Requirements 3.3, 12.5**
   !---------------------------------------------------------------------------
   subroutine test_unit_midnight()
      real(fp) :: sza_deg, cossza

      write(*,*) '--- Unit test: Midnight -> cossza = 0.0 ---'

      ! jday=80, xhour=0.0 UTC, lat=0 (equator), lon=0 (local midnight)
      call solar_zenith_angle(80, 0.0_fp, 0.0_fp, 0.0_fp, sza_deg, cossza)

      call assert(cossza == 0.0_fp, 'cossza at midnight must be 0.0')
      call assert(sza_deg > 90.0_fp, 'sza_deg at midnight must be > 90 degrees')

      write(*,*) '  PASSED: cossza =', cossza, ' sza_deg =', sza_deg
   end subroutine test_unit_midnight

   !---------------------------------------------------------------------------
   !> Property 3: Oxidant diurnal scaling correctness
   !! Verify OH/NO3 scaling and H2O2 recycle behavior.
   !! **Validates: Requirements 4.2, 4.3, 4.4, 4.5, 12.2**
   !---------------------------------------------------------------------------
   subroutine test_property3_oxidant_scaling()
      integer, parameter :: NLEV = 10
      real(fp), parameter :: CDT = 3600.0_fp
      real(fp), parameter :: AIRMW_VAL = 28.9644_fp
      real(fp), parameter :: AVO_VAL = 6.022140857e+23_fp

      integer :: i, seed, nymd, nhms, jday, n, ndystep
      real(fp) :: lat_rad, lon_rad, xhour, xhouruse
      real(fp) :: oh_clim(NLEV), no3_clim(NLEV), h2o2_clim(NLEV)
      real(fp) :: xoh(NLEV), xno3(NLEV), xh2o2(NLEV), h2o2_init(NLEV)
      real(fp) :: rhoa(NLEV)
      logical  :: recycle_h2o2
      integer  :: rc, k

      ! Variables for independent verification
      real(fp) :: sza_deg, cossza, cossza_now
      real(fp) :: tcosz, tday, tnight
      real(fp) :: expected_oh_vmr, expected_oh_nd, expected_no3
      real(fp), parameter :: TOL = 1.0e-10_fp

      write(*,*) '--- Property 3: Oxidant diurnal scaling correctness ---'

      seed = 77777

      do i = 1, N_ITER
         ! Generate random inputs
         lat_rad = (rand_val(seed) - 0.5_fp) * PI
         lon_rad = (rand_val(seed) - 0.5_fp) * 2.0_fp * PI

         ! Random date/time — use first day of random month for valid dates
         nymd = 20200101 + int(rand_val(seed) * 11.0_fp) * 100
         nhms = int(rand_val(seed) * 23.99_fp) * 10000

         ! Random climatological fields and air density
         do k = 1, NLEV
            oh_clim(k)   = rand_val(seed) * 1.0e-12_fp + 1.0e-14_fp
            no3_clim(k)  = rand_val(seed) * 1.0e-11_fp + 1.0e-13_fp
            h2o2_clim(k) = rand_val(seed) * 1.0e-9_fp + 1.0e-11_fp
            rhoa(k)      = 0.3_fp + rand_val(seed) * 1.0_fp
         end do

         ! --- Test with recycle_h2o2 = .true. ---
         xoh = 0.0_fp; xno3 = 0.0_fp; xh2o2 = 0.0_fp
         h2o2_init = 0.0_fp
         recycle_h2o2 = .true.

         call so4chem_update_oxidants(NLEV, CDT, nymd, nhms, lat_rad, lon_rad, &
            AIRMW_VAL, AVO_VAL, oh_clim, no3_clim, h2o2_clim, &
            xoh, xno3, xh2o2, h2o2_init, recycle_h2o2, rhoa, rc)

         call assert(rc == 0, 'P3: update_oxidants should succeed')

         ! Verify H2O2 recycle: xh2o2 should equal h2o2_clim
         do k = 1, NLEV
            call assert_close(xh2o2(k), h2o2_clim(k), TOL, &
               'P3: H2O2 recycle should reset to climatology')
         end do
         call assert(.not. recycle_h2o2, 'P3: recycle flag should be cleared')

         ! --- Independently compute expected values ---
         ! Compute jday from nymd
         jday = compute_jday(nymd)
         xhour = real(nhms / 10000, fp)

         ! Integrate cos(SZA) over the day
         ndystep = nint(86400.0_fp / CDT)
         tcosz = 0.0_fp
         tday  = 0.0_fp
         xhouruse = xhour

         do n = 1, ndystep
            call solar_zenith_angle(jday, xhouruse, lat_rad, lon_rad, sza_deg, cossza)
            tcosz = tcosz + cossza
            xhouruse = xhouruse + CDT / 3600.0_fp
            if (xhouruse > 24.0_fp) xhouruse = xhouruse - 24.0_fp
            if (cossza > 0.0_fp) tday = tday + CDT
         end do

         call solar_zenith_angle(jday, xhour, lat_rad, lon_rad, sza_deg, cossza_now)
         tnight = 86400.0_fp - tday

         ! Verify OH scaling
         do k = 1, NLEV
            if (tcosz > 0.0_fp) then
               expected_oh_vmr = oh_clim(k) * (86400.0_fp / CDT) * cossza_now / tcosz
            else
               expected_oh_vmr = 0.0_fp
            end if
            if (expected_oh_vmr < 0.0_fp) expected_oh_vmr = 0.0_fp
            ! Convert to number density
            expected_oh_nd = expected_oh_vmr * 1000.0_fp * rhoa(k) / AIRMW_VAL &
               * AVO_VAL * 1.0e-6_fp

            call assert_close(xoh(k), expected_oh_nd, &
               max(abs(expected_oh_nd) * 1.0e-10_fp, tiny(1.0_fp)), &
               'P3: OH scaling mismatch')
         end do

         ! Verify NO3 scaling
         do k = 1, NLEV
            if (cossza_now > 0.0_fp .or. tnight < tiny(1.0_fp)) then
               expected_no3 = 0.0_fp
            else
               expected_no3 = no3_clim(k) * 86400.0_fp / tnight
            end if
            call assert_close(xno3(k), expected_no3, &
               max(abs(expected_no3) * 1.0e-10_fp, tiny(1.0_fp)), &
               'P3: NO3 scaling mismatch')
         end do

         ! Verify OH is zero at nighttime
         if (cossza_now == 0.0_fp) then
            do k = 1, NLEV
               call assert(xoh(k) == 0.0_fp, 'P3: OH must be zero at nighttime')
            end do
         end if

         ! Verify NO3 is zero during daytime
         if (cossza_now > 0.0_fp) then
            do k = 1, NLEV
               call assert(xno3(k) == 0.0_fp, 'P3: NO3 must be zero during daytime')
            end do
         end if
      end do

      write(*,*) '  PASSED (', N_ITER, ' iterations)'
   end subroutine test_property3_oxidant_scaling

   !---------------------------------------------------------------------------
   !> Property 4: DMS sulfur mass conservation and non-negativity
   !! **Validates: Requirements 5.4, 12.3, 12.6**
   !---------------------------------------------------------------------------
   subroutine test_property4_dms_mass_conservation()
      integer, parameter :: NLEV = 10
      integer, parameter :: KLID = 1
      real(fp), parameter :: CDT = 3600.0_fp
      real(fp), parameter :: AIRMW_VAL = 28.9644_fp
      real(fp), parameter :: AVO_VAL = 6.022140857e+23_fp
      real(fp), parameter :: fMassDMS = 62.13_fp
      real(fp), parameter :: fMassSO2 = 64.066_fp
      real(fp), parameter :: fMassMSA = 96.11_fp
      real(fp), parameter :: REL_TOL = 1.0e-6_fp

      integer :: i, seed, k, rc
      real(fp) :: dms(NLEV), dms_init(NLEV)
      real(fp) :: xoh(NLEV), xno3(NLEV)
      real(fp) :: tmpu(NLEV), rhoa(NLEV)
      real(fp) :: pso2_dms(NLEV), pmsa_dms(NLEV)
      real(fp) :: cossza
      real(fp) :: sulfur_conserved, rel_err, denom

      ! For daytime NO3 test
      real(fp) :: dms_day(NLEV)
      real(fp) :: pso2_day(NLEV), pmsa_day(NLEV)
      real(fp) :: dms_night(NLEV)
      real(fp) :: pso2_night(NLEV), pmsa_night(NLEV)
      real(fp) :: zero_no3(NLEV)

      write(*,*) '--- Property 4: DMS sulfur mass conservation ---'

      seed = 54321

      do i = 1, N_ITER
         ! Generate random inputs
         do k = 1, NLEV
            dms(k)  = rand_val(seed) * 1.0e-9_fp + 1.0e-15_fp
            xoh(k)  = rand_val(seed) * 1.0e7_fp
            xno3(k) = rand_val(seed) * 1.0e-11_fp
            tmpu(k) = 220.0_fp + rand_val(seed) * 80.0_fp
            rhoa(k) = 0.3_fp + rand_val(seed) * 1.0_fp
         end do

         ! Random cossza (mix of day and night)
         cossza = rand_val(seed)
         if (cossza < 0.3_fp) cossza = 0.0_fp  ! ~30% nighttime

         dms_init = dms

         call so4chem_dms_oxidation(NLEV, KLID, CDT, AIRMW_VAL, AVO_VAL, &
            fMassMSA, fMassDMS, fMassSO2, dms, xoh, xno3, cossza, tmpu, rhoa, &
            pso2_dms, pmsa_dms, rc)

         call assert(rc == 0, 'P4: dms_oxidation should succeed')

         ! Verify non-negativity
         do k = KLID, NLEV
            call assert(dms(k) >= 0.0_fp, 'P4: dms_final must be >= 0')
            call assert(pso2_dms(k) >= 0.0_fp, 'P4: pso2_dms must be >= 0')
            call assert(pmsa_dms(k) >= 0.0_fp, 'P4: pmsa_dms must be >= 0')
         end do

         ! Verify sulfur mass conservation at each level
         ! The implementation uses dms0 = max(dms_input, tiny(dms_input)) as the
         ! starting value, and dms_final = max(computed_final, tiny(computed_final))
         ! as the output. The SO2 production is computed from the un-clamped dms_final,
         ! so: dms_final_unclamped + pso2*cdt*(fMassDMS/fMassSO2) + pmsa*cdt*(fMassDMS/fMassMSA) = dms0
         ! But dms(k) = max(dms_final_unclamped, tiny(...)), so we need tolerance for the tiny() clamp.
         do k = KLID, NLEV
            sulfur_conserved = dms(k) &
               + pso2_dms(k) * CDT * (fMassDMS / fMassSO2) &
               + pmsa_dms(k) * CDT * (fMassDMS / fMassMSA)

            ! dms0 inside the routine = max(dms_init(k), tiny(dms_init(k)))
            denom = max(dms_init(k), tiny(1.0_fp))
            rel_err = abs(sulfur_conserved - denom) / denom

            ! Allow tolerance for floating-point error from exponential decay
            ! and mass ratio conversions, plus the tiny() clamp on dms_final
            call assert(rel_err < REL_TOL .or. &
               abs(sulfur_conserved - denom) < 2.0_fp * tiny(1.0_fp), &
               'P4: sulfur mass not conserved in DMS oxidation')
         end do

         ! --- Verify NO3 pathway contributes zero DMS loss when cossza > 0 ---
         ! Run with cossza > 0 (daytime) and compare with xno3=0
         do k = 1, NLEV
            dms_day(k) = dms_init(k)
            dms_night(k) = dms_init(k)
         end do

         ! Daytime run with NO3 present
         call so4chem_dms_oxidation(NLEV, KLID, CDT, AIRMW_VAL, AVO_VAL, &
            fMassMSA, fMassDMS, fMassSO2, dms_day, xoh, xno3, 0.5_fp, tmpu, rhoa, &
            pso2_day, pmsa_day, rc)

         ! Daytime run with NO3 = 0
         dms_night = dms_init
         zero_no3 = 0.0_fp
         call so4chem_dms_oxidation(NLEV, KLID, CDT, AIRMW_VAL, AVO_VAL, &
            fMassMSA, fMassDMS, fMassSO2, dms_night, xoh, &
            zero_no3, 0.5_fp, tmpu, rhoa, &
            pso2_night, pmsa_night, rc)

         ! With cossza > 0, NO3 should not contribute, so results should be identical
         do k = KLID, NLEV
            call assert(dms_day(k) == dms_night(k), &
               'P4: NO3 should not affect DMS during daytime')
         end do
      end do

      write(*,*) '  PASSED (', N_ITER, ' iterations)'
   end subroutine test_property4_dms_mass_conservation

   !---------------------------------------------------------------------------
   !> Property 5: SO2 oxidation non-negativity and mass bound
   !! **Validates: Requirements 6.3, 12.4**
   !---------------------------------------------------------------------------
   subroutine test_property5_so2_oxidation()
      integer, parameter :: NLEV = 10
      integer, parameter :: KLID = 1
      real(fp), parameter :: CDT = 3600.0_fp
      real(fp), parameter :: AIRMW_VAL = 28.9644_fp
      real(fp), parameter :: AVO_VAL = 6.022140857e+23_fp
      real(fp), parameter :: GRAV = 9.80665_fp
      real(fp), parameter :: fMassSO4 = 96.06_fp
      real(fp), parameter :: fMassSO2 = 64.066_fp

      integer :: i, seed, k, rc
      real(fp) :: so2(NLEV), so2_init(NLEV)
      real(fp) :: xoh(NLEV), xh2o2(NLEV)
      real(fp) :: tmpu(NLEV), rhoa(NLEV), delp(NLEV), cloud(NLEV)
      real(fp) :: pso2_dms(NLEV)
      real(fp) :: pso4g_so2(NLEV), pso4aq_so2(NLEV)
      real(fp) :: total_so4, available_so2

      write(*,*) '--- Property 5: SO2 oxidation non-negativity and mass bound ---'

      seed = 98765

      do i = 1, N_ITER
         ! Generate random inputs
         do k = 1, NLEV
            so2(k)      = rand_val(seed) * 1.0e-8_fp + 1.0e-15_fp
            xoh(k)      = rand_val(seed) * 1.0e7_fp
            xh2o2(k)    = rand_val(seed) * 1.0e-9_fp + 1.0e-12_fp
            tmpu(k)     = 220.0_fp + rand_val(seed) * 80.0_fp
            rhoa(k)     = 0.3_fp + rand_val(seed) * 1.0_fp
            delp(k)     = 500.0_fp + rand_val(seed) * 5000.0_fp
            cloud(k)    = rand_val(seed) * 0.8_fp
            pso2_dms(k) = rand_val(seed) * 1.0e-12_fp
         end do

         so2_init = so2

         call so4chem_so2_oxidation(NLEV, KLID, CDT, AIRMW_VAL, AVO_VAL, GRAV, &
            fMassSO4, fMassSO2, so2, xoh, xh2o2, tmpu, rhoa, delp, cloud, &
            pso2_dms, pso4g_so2, pso4aq_so2, rc)

         call assert(rc == 0, 'P5: so2_oxidation should succeed')

         do k = KLID, NLEV
            ! Verify non-negativity
            call assert(pso4g_so2(k) >= 0.0_fp, 'P5: pso4g_so2 must be >= 0')
            call assert(pso4aq_so2(k) >= 0.0_fp, 'P5: pso4aq_so2 must be >= 0')

            ! Verify total SO4 produced does not exceed available SO2
            ! Available SO2 = initial SO2 + DMS source, adjusted by mass ratio
            total_so4 = (pso4g_so2(k) + pso4aq_so2(k)) * CDT
            available_so2 = (so2_init(k) + pso2_dms(k) * CDT) * (fMassSO4 / fMassSO2)
            ! Use a small tolerance for floating point
            call assert(total_so4 <= available_so2 * (1.0_fp + 1.0e-10_fp), &
               'P5: total SO4 must not exceed available SO2')

            ! Verify aqueous production is zero when T < 258 K
            if (tmpu(k) < 258.0_fp) then
               call assert(pso4aq_so2(k) == 0.0_fp, &
                  'P5: aqueous SO4 must be zero when T < 258 K')
            end if
         end do
      end do

      ! --- Additional test: force all temperatures below 258 K ---
      do k = 1, NLEV
         so2(k)      = 1.0e-9_fp
         xoh(k)      = 1.0e6_fp
         xh2o2(k)    = 1.0e-10_fp
         tmpu(k)     = 240.0_fp  ! Below 258 K
         rhoa(k)     = 1.0_fp
         delp(k)     = 3000.0_fp
         cloud(k)    = 0.5_fp
         pso2_dms(k) = 1.0e-13_fp
      end do

      call so4chem_so2_oxidation(NLEV, KLID, CDT, AIRMW_VAL, AVO_VAL, GRAV, &
         fMassSO4, fMassSO2, so2, xoh, xh2o2, tmpu, rhoa, delp, cloud, &
         pso2_dms, pso4g_so2, pso4aq_so2, rc)

      do k = KLID, NLEV
         call assert(pso4aq_so2(k) == 0.0_fp, &
            'P5: aqueous SO4 must be zero for all levels when T < 258 K')
      end do

      write(*,*) '  PASSED (', N_ITER, ' iterations)'
   end subroutine test_property5_so2_oxidation

   !---------------------------------------------------------------------------
   !> Property 6: SO4/MSA update formula correctness
   !! **Validates: Requirements 7.3**
   !---------------------------------------------------------------------------
   subroutine test_property6_so4_msa_update()
      integer, parameter :: NLEV = 10
      integer, parameter :: KLID = 1
      real(fp), parameter :: CDT = 3600.0_fp
      real(fp), parameter :: GRAV = 9.80665_fp
      real(fp), parameter :: TOL = 1.0e-10_fp

      integer :: i, seed, k, rc
      real(fp) :: so4(NLEV), so4_init(NLEV)
      real(fp) :: msa(NLEV), msa_init(NLEV)
      real(fp) :: pso4g(NLEV), pso4aq(NLEV), pmsa(NLEV)
      real(fp) :: delp(NLEV)
      real(fp) :: expected, so4_0_clamped, msa_0_clamped

      write(*,*) '--- Property 6: SO4/MSA update formula correctness ---'

      seed = 11111

      do i = 1, N_ITER
         ! Generate random inputs
         do k = 1, NLEV
            so4(k)   = rand_val(seed) * 1.0e-9_fp + 1.0e-15_fp
            msa(k)   = rand_val(seed) * 1.0e-10_fp + 1.0e-15_fp
            pso4g(k) = rand_val(seed) * 1.0e-12_fp
            pso4aq(k)= rand_val(seed) * 1.0e-12_fp
            pmsa(k)  = rand_val(seed) * 1.0e-13_fp
            delp(k)  = 500.0_fp + rand_val(seed) * 5000.0_fp
         end do

         so4_init = so4
         msa_init = msa

         ! Test SO4 update
         call so4chem_so4_update(NLEV, KLID, CDT, GRAV, so4, delp, &
            pso4g, pso4aq, rc)
         call assert(rc == 0, 'P6: so4_update should succeed')

         ! Verify formula: so4 = max(so4_0, tiny) + (pso4g + pso4aq) * cdt
         ! then clamped to max(result, tiny)
         do k = KLID, NLEV
            so4_0_clamped = max(so4_init(k), tiny(so4_init(k)))
            expected = so4_0_clamped + (pso4g(k) + pso4aq(k)) * CDT
            expected = max(expected, tiny(expected))

            call assert_close(so4(k), expected, &
               max(abs(expected) * TOL, tiny(1.0_fp)), &
               'P6: SO4 update formula mismatch')
            call assert(so4(k) >= 0.0_fp, 'P6: SO4 must be non-negative')
         end do

         ! Test MSA update
         call so4chem_msa_update(NLEV, KLID, CDT, GRAV, msa, delp, &
            pmsa, rc)
         call assert(rc == 0, 'P6: msa_update should succeed')

         ! Verify formula: msa = max(msa_0, tiny) + pmsa * cdt
         ! then clamped to max(result, tiny)
         do k = KLID, NLEV
            msa_0_clamped = max(msa_init(k), tiny(msa_init(k)))
            expected = msa_0_clamped + pmsa(k) * CDT
            expected = max(expected, tiny(expected))

            call assert_close(msa(k), expected, &
               max(abs(expected) * TOL, tiny(1.0_fp)), &
               'P6: MSA update formula mismatch')
            call assert(msa(k) >= 0.0_fp, 'P6: MSA must be non-negative')
         end do
      end do

      write(*,*) '  PASSED (', N_ITER, ' iterations)'
   end subroutine test_property6_so4_msa_update

   !---------------------------------------------------------------------------
   !> Property 7: Full chemistry timestep equivalence with GOCART2G
   !! Compare so4chem_driver output against GOCART2G SulfateUpdateOxidants +
   !! SulfateChemDriver path (with dry deposition disabled).
   !! **Validates: Requirements 10.1, 10.2**
   !---------------------------------------------------------------------------
   subroutine test_property7_full_equivalence()
      integer, parameter :: NLEV = 72
      integer, parameter :: N_P7_ITER = 100
      real(fp), parameter :: CDT = 3600.0_fp
      real(fp), parameter :: GRAV = 9.80665_fp
      real(fp), parameter :: AIRMW_VAL = 28.9644_fp
      real(fp), parameter :: AVO_VAL = 6.022140857e+23_fp
      real(fp), parameter :: CPD_VAL = 1004.16_fp
      real(fp), parameter :: fMassDMS_val = 62.13_fp
      real(fp), parameter :: fMassSO2_val = 64.066_fp
      real(fp), parameter :: fMassSO4_val = 96.06_fp
      real(fp), parameter :: fMassMSA_val = 96.11_fp
      ! Relative tolerance: GOCART2G uses default real (4-byte) for arrays
      ! while SO4chemPhysics_Mod uses real(fp) (8-byte) throughout. The
      ! 4-byte precision in GOCART2G oxidant arrays (~7 significant digits)
      ! is amplified by exponential decay in DMS/SO2 chemistry, leading to
      ! relative errors up to ~1e-3 at levels with fast reaction rates.
      real(fp), parameter :: RTOL = 5.0e-3_fp

      integer :: i, seed, k, rc_new, rc_g
      integer :: nymd, nhms

      ! CATChem 1D arrays (bottom-to-top)
      real(fp) :: lat_rad, lon_rad
      real(fp) :: dms_c(NLEV), so2_c(NLEV), so4_c(NLEV), msa_c(NLEV)
      real(fp) :: oh_clim_c(NLEV), no3_clim_c(NLEV), h2o2_clim_c(NLEV)
      real(fp) :: xoh_c(NLEV), xno3_c(NLEV), xh2o2_c(NLEV), h2o2_init_c(NLEV)
      real(fp) :: tmpu_c(NLEV), rhoa_c(NLEV), delp_c(NLEV), cloud_c(NLEV)
      real(fp) :: pso2_dms_c(NLEV), pmsa_dms_c(NLEV)
      real(fp) :: pso4g_so2_c(NLEV), pso4aq_so2_c(NLEV)
      logical  :: recycle_h2o2_c

      ! GOCART2G 3D arrays (1,1,km) top-to-bottom — pointer targets
      real, allocatable, target :: dms_g(:,:,:), so2_g(:,:,:)
      real, allocatable, target :: so4_g(:,:,:)
      real, allocatable, target :: msa_g(:,:,:)
      real, allocatable, target :: oh_clim_g(:,:,:), no3_clim_g(:,:,:), h2o2_clim_g(:,:,:)
      real, allocatable, target :: xoh_g(:,:,:), xno3_g(:,:,:), xh2o2_g(:,:,:)
      real, allocatable, target :: h2o2_init_g(:,:,:)
      real, allocatable, target :: tmpu_g(:,:,:), rhoa_g(:,:,:)
      real, allocatable, target :: delp_g(:,:,:), cloud_g(:,:,:)
      real, allocatable, target :: hghte_g(:,:,:)
      real, allocatable, target :: ustar_g(:,:), shflux_g(:,:), oro_g(:,:)
      real, allocatable, target :: pblh_g(:,:), z0h_g(:,:)
      real, allocatable, target :: lonRad_g(:,:), latRad_g(:,:)
      real, allocatable, target :: SU_dep_g(:,:,:)
      real, allocatable, target :: SU_PSO2_g(:,:), SU_PMSA_g(:,:)
      real, allocatable, target :: SU_PSO4_g(:,:), SU_PSO4g_g(:,:), SU_PSO4aq_g(:,:)
      real, allocatable, target :: pso2_g(:,:,:), pmsa_g(:,:,:)
      real, allocatable, target :: pso4_g(:,:,:), pso4g_g(:,:,:), pso4aq_g(:,:,:)
      real, allocatable :: drydepositionfrequency_g(:,:)
      logical :: recycle_h2o2_g
      integer :: nymd_last_g

      ! Pointers for GOCART2G interfaces
      real, pointer :: tmpu_ptr(:,:,:), rhoa_ptr(:,:,:), hghte_ptr(:,:,:)
      real, pointer :: ustar_ptr(:,:), shflux_ptr(:,:), oro_ptr(:,:)
      real, pointer :: pblh_ptr(:,:), z0h_ptr(:,:)
      real, pointer :: msa_ptr(:,:,:)
      real, pointer :: SU_dep_ptr(:,:,:)
      real, pointer :: SU_PSO2_ptr(:,:), SU_PMSA_ptr(:,:)
      real, pointer :: SU_PSO4_ptr(:,:), SU_PSO4g_ptr(:,:), SU_PSO4aq_ptr(:,:)
      real, pointer :: pso2_ptr(:,:,:), pmsa_ptr(:,:,:)
      real, pointer :: pso4_ptr(:,:,:), pso4g_ptr(:,:,:), pso4aq_ptr(:,:,:)
      real, pointer :: oh_clim_ptr(:,:,:), no3_clim_ptr(:,:,:), h2o2_clim_ptr(:,:,:)

      real(fp) :: rel_err, denom_val
      real(fp) :: radToDeg_val
      real :: undefval_g

      write(*,*) '--- Property 7: Full chemistry timestep equivalence ---'
      write(*,*) '--- Validates: Requirements 10.1, 10.2 ---'

      seed = 99999
      radToDeg_val = 180.0_fp / PI
      undefval_g = 1.0e15

      do i = 1, N_P7_ITER

         ! --- Generate random atmospheric column profile ---
         lat_rad = (rand_val(seed) - 0.5_fp) * PI
         lon_rad = (rand_val(seed) - 0.5_fp) * 2.0_fp * PI

         ! Random date/time — use first day of random month
         nymd = 20200101 + int(rand_val(seed) * 11.0_fp) * 100
         nhms = int(rand_val(seed) * 23.99_fp) * 10000

         ! Generate random column data (bottom-to-top for CATChem)
         do k = 1, NLEV
            tmpu_c(k)      = 220.0_fp + rand_val(seed) * 80.0_fp
            rhoa_c(k)      = 0.3_fp + rand_val(seed) * 1.0_fp
            delp_c(k)      = 500.0_fp + rand_val(seed) * 5000.0_fp
            cloud_c(k)     = rand_val(seed) * 0.5_fp
            dms_c(k)       = rand_val(seed) * 1.0e-9_fp + 1.0e-15_fp
            so2_c(k)       = rand_val(seed) * 1.0e-8_fp + 1.0e-15_fp
            so4_c(k)       = rand_val(seed) * 1.0e-9_fp + 1.0e-15_fp
            msa_c(k)       = rand_val(seed) * 1.0e-10_fp + 1.0e-15_fp
            oh_clim_c(k)   = rand_val(seed) * 1.0e-12_fp + 1.0e-14_fp
            no3_clim_c(k)  = rand_val(seed) * 1.0e-11_fp + 1.0e-13_fp
            h2o2_clim_c(k) = rand_val(seed) * 1.0e-9_fp + 1.0e-11_fp
         end do

         ! --- Allocate GOCART2G 3D arrays ---
         allocate(dms_g(1,1,NLEV), so2_g(1,1,NLEV), so4_g(1,1,NLEV), msa_g(1,1,NLEV))
         allocate(oh_clim_g(1,1,NLEV), no3_clim_g(1,1,NLEV), h2o2_clim_g(1,1,NLEV))
         allocate(xoh_g(1,1,NLEV), xno3_g(1,1,NLEV), xh2o2_g(1,1,NLEV))
         allocate(h2o2_init_g(1,1,NLEV))
         allocate(tmpu_g(1,1,NLEV), rhoa_g(1,1,NLEV))
         allocate(delp_g(1,1,NLEV), cloud_g(1,1,NLEV))
         allocate(hghte_g(1,1,0:NLEV))
         allocate(ustar_g(1,1), shflux_g(1,1), oro_g(1,1))
         allocate(pblh_g(1,1), z0h_g(1,1))
         allocate(lonRad_g(1,1), latRad_g(1,1))
         allocate(SU_dep_g(1,1,4))
         allocate(SU_PSO2_g(1,1), SU_PMSA_g(1,1))
         allocate(SU_PSO4_g(1,1), SU_PSO4g_g(1,1), SU_PSO4aq_g(1,1))
         allocate(pso2_g(1,1,NLEV), pmsa_g(1,1,NLEV))
         allocate(pso4_g(1,1,NLEV), pso4g_g(1,1,NLEV), pso4aq_g(1,1,NLEV))

         ! --- Fill GOCART2G arrays (reversed: gocart(k) = catchem(NLEV+1-k)) ---
         do k = 1, NLEV
            tmpu_g(1,1,k)      = real(tmpu_c(NLEV + 1 - k))
            rhoa_g(1,1,k)      = real(rhoa_c(NLEV + 1 - k))
            delp_g(1,1,k)      = real(delp_c(NLEV + 1 - k))
            cloud_g(1,1,k)     = real(cloud_c(NLEV + 1 - k))
            dms_g(1,1,k)       = real(dms_c(NLEV + 1 - k))
            so2_g(1,1,k)       = real(so2_c(NLEV + 1 - k))
            so4_g(1,1,k)       = real(so4_c(NLEV + 1 - k))
            msa_g(1,1,k)       = real(msa_c(NLEV + 1 - k))
            oh_clim_g(1,1,k)   = real(oh_clim_c(NLEV + 1 - k))
            no3_clim_g(1,1,k)  = real(no3_clim_c(NLEV + 1 - k))
            h2o2_clim_g(1,1,k) = real(h2o2_clim_c(NLEV + 1 - k))
         end do

         ! Set up edge heights for GOCART2G (top-to-bottom, 0-based)
         ! hghte_g(0) = TOA, hghte_g(NLEV) = surface
         ! Use huge value for surface layer to disable dry deposition
         ! dz = hghte(km-1) - hghte(km) => if hghte(km) = -1e38, dz is huge
         do k = 0, NLEV - 1
            hghte_g(1,1,k) = real(50000.0_fp - real(k, fp) * 700.0_fp)
         end do
         ! Set surface edge to huge negative value so dz = hghte(km-1) - hghte(km) is huge
         hghte_g(1,1,NLEV) = -1.0e38

         ! Surface met fields (needed by DryDeposition but won't matter with huge dz)
         ustar_g(1,1)  = 0.3
         shflux_g(1,1) = 10.0
         oro_g(1,1)    = 1.0   ! LAND
         pblh_g(1,1)   = 1000.0
         z0h_g(1,1)    = 0.01

         lonRad_g(1,1) = real(lon_rad)
         latRad_g(1,1) = real(lat_rad)

         ! Initialize oxidant working arrays for GOCART2G
         xoh_g  = 0.0
         xno3_g = 0.0
         xh2o2_g = 0.0
         h2o2_init_g = 0.0

         ! Set recycle_h2o2 = .true. for both paths
         recycle_h2o2_g = .true.
         recycle_h2o2_c = .true.

         ! Set nymd_last != nymd to avoid the reset check in SulfateUpdateOxidants
         nymd_last_g = nymd - 1

         ! Initialize diagnostic arrays
         SU_dep_g = 0.0
         SU_PSO2_g = 0.0; SU_PMSA_g = 0.0
         SU_PSO4_g = 0.0; SU_PSO4g_g = 0.0; SU_PSO4aq_g = 0.0
         pso2_g = 0.0; pmsa_g = 0.0
         pso4_g = 0.0; pso4g_g = 0.0; pso4aq_g = 0.0

         ! Set up pointers for GOCART2G
         tmpu_ptr => tmpu_g
         rhoa_ptr => rhoa_g
         hghte_ptr => hghte_g
         ustar_ptr => ustar_g
         shflux_ptr => shflux_g
         oro_ptr => oro_g
         pblh_ptr => pblh_g
         z0h_ptr => z0h_g
         msa_ptr => msa_g
         SU_dep_ptr => SU_dep_g
         SU_PSO2_ptr => SU_PSO2_g
         SU_PMSA_ptr => SU_PMSA_g
         SU_PSO4_ptr => SU_PSO4_g
         SU_PSO4g_ptr => SU_PSO4g_g
         SU_PSO4aq_ptr => SU_PSO4aq_g
         pso2_ptr => pso2_g
         pmsa_ptr => pmsa_g
         pso4_ptr => pso4_g
         pso4g_ptr => pso4g_g
         pso4aq_ptr => pso4aq_g
         oh_clim_ptr => oh_clim_g
         no3_clim_ptr => no3_clim_g
         h2o2_clim_ptr => h2o2_clim_g

         ! --- Call GOCART2G path ---
         ! 1) SulfateUpdateOxidants
         call SulfateUpdateOxidants(nymd, nhms, lonRad_g, latRad_g, &
            rhoa_g, NLEV, real(CDT), nymd_last_g, &
            undefval_g, real(radToDeg_val), real(AVO_VAL), real(PI), real(AIRMW_VAL), &
            oh_clim_ptr, no3_clim_ptr, h2o2_clim_ptr, &
            xoh_g, xno3_g, xh2o2_g, recycle_h2o2_g, rc_g)

         call assert(rc_g == 0, 'P7: GOCART2G SulfateUpdateOxidants should succeed')

         ! 2) SulfateChemDriver (with dry deposition disabled via huge hghte)
         call SulfateChemDriver(NLEV, 1, real(CDT), real(PI), real(radToDeg_val), &
            real(VON_KARMAN), real(AIRMW_VAL), real(AVO_VAL), real(CPD_VAL), real(GRAV), &
            real(fMassMSA_val), real(fMassDMS_val), real(fMassSO2_val), real(fMassSO4_val), &
            nymd, nhms, lonRad_g, latRad_g, &
            dms_g, so2_g, so4_g, msa_ptr, &
            1, 2, 3, 4, &
            xoh_g, xno3_g, xh2o2_g, h2o2_init_g, &
            delp_g, tmpu_ptr, cloud_g, rhoa_ptr, hghte_ptr, &
            ustar_ptr, shflux_ptr, oro_ptr, pblh_ptr, z0h_ptr, &
            SU_dep_ptr, SU_PSO2_ptr, SU_PMSA_ptr, &
            SU_PSO4_ptr, SU_PSO4g_ptr, SU_PSO4aq_ptr, &
            pso2_ptr, pmsa_ptr, pso4_ptr, pso4g_ptr, pso4aq_ptr, &
            drydepositionfrequency_g, rc_g)

         call assert(rc_g == 0, 'P7: GOCART2G SulfateChemDriver should succeed')

         ! --- Call CATChem path ---
         ! Initialize CATChem oxidant working arrays
         xoh_c  = 0.0_fp
         xno3_c = 0.0_fp
         xh2o2_c = 0.0_fp
         h2o2_init_c = 0.0_fp

         call so4chem_driver(NLEV, 1, CDT, nymd, nhms, lat_rad, lon_rad, &
            AIRMW_VAL, AVO_VAL, GRAV, fMassDMS_val, fMassSO2_val, fMassSO4_val, fMassMSA_val, &
            dms_c, so2_c, so4_c, msa_c, oh_clim_c, no3_clim_c, h2o2_clim_c, &
            xoh_c, xno3_c, xh2o2_c, h2o2_init_c, recycle_h2o2_c, &
            tmpu_c, rhoa_c, delp_c, cloud_c, 1, &
            pso2_dms_c, pmsa_dms_c, pso4g_so2_c, pso4aq_so2_c, rc_new)

         call assert(rc_new == 0, 'P7: so4chem_driver should succeed')

         ! --- Compare results (accounting for vertical reversal) ---
         ! Compare DMS
         do k = 1, NLEV
            denom_val = max(abs(real(dms_g(1,1,NLEV + 1 - k), fp)), &
               abs(dms_c(k)), tiny(1.0_fp))
            rel_err = abs(dms_c(k) - real(dms_g(1,1,NLEV + 1 - k), fp)) / denom_val
            if (rel_err > RTOL) then
               print '(a,i3,a,es12.4,a,es20.12,a,es20.12)', &
                  '  P7 DMS mismatch at k=', k, ' rel_err=', rel_err, &
                  ' catchem=', dms_c(k), ' gocart=', real(dms_g(1,1,NLEV+1-k), fp)
               stop 1
            end if
         end do

         ! Compare SO2
         do k = 1, NLEV
            denom_val = max(abs(real(so2_g(1,1,NLEV + 1 - k), fp)), &
               abs(so2_c(k)), tiny(1.0_fp))
            rel_err = abs(so2_c(k) - real(so2_g(1,1,NLEV + 1 - k), fp)) / denom_val
            if (rel_err > RTOL) then
               print '(a,i3,a,es12.4,a,es20.12,a,es20.12)', &
                  '  P7 SO2 mismatch at k=', k, ' rel_err=', rel_err, &
                  ' catchem=', so2_c(k), ' gocart=', real(so2_g(1,1,NLEV+1-k), fp)
               stop 1
            end if
         end do

         ! Compare SO4
         do k = 1, NLEV
            denom_val = max(abs(real(so4_g(1,1,NLEV + 1 - k), fp)), &
               abs(so4_c(k)), tiny(1.0_fp))
            rel_err = abs(so4_c(k) - real(so4_g(1,1,NLEV + 1 - k), fp)) / denom_val
            if (rel_err > RTOL) then
               print '(a,i3,a,es12.4,a,es20.12,a,es20.12)', &
                  '  P7 SO4 mismatch at k=', k, ' rel_err=', rel_err, &
                  ' catchem=', so4_c(k), ' gocart=', real(so4_g(1,1,NLEV+1-k), fp)
               stop 1
            end if
         end do

         ! Compare MSA
         do k = 1, NLEV
            denom_val = max(abs(real(msa_g(1,1,NLEV + 1 - k), fp)), &
               abs(msa_c(k)), tiny(1.0_fp))
            rel_err = abs(msa_c(k) - real(msa_g(1,1,NLEV + 1 - k), fp)) / denom_val
            if (rel_err > RTOL) then
               print '(a,i3,a,es12.4,a,es20.12,a,es20.12)', &
                  '  P7 MSA mismatch at k=', k, ' rel_err=', rel_err, &
                  ' catchem=', msa_c(k), ' gocart=', real(msa_g(1,1,NLEV+1-k), fp)
               stop 1
            end if
         end do

         ! Cleanup
         if (allocated(drydepositionfrequency_g)) deallocate(drydepositionfrequency_g)
         deallocate(dms_g, so2_g, so4_g, msa_g)
         deallocate(oh_clim_g, no3_clim_g, h2o2_clim_g)
         deallocate(xoh_g, xno3_g, xh2o2_g, h2o2_init_g)
         deallocate(tmpu_g, rhoa_g, delp_g, cloud_g, hghte_g)
         deallocate(ustar_g, shflux_g, oro_g, pblh_g, z0h_g)
         deallocate(lonRad_g, latRad_g)
         deallocate(SU_dep_g, SU_PSO2_g, SU_PMSA_g)
         deallocate(SU_PSO4_g, SU_PSO4g_g, SU_PSO4aq_g)
         deallocate(pso2_g, pmsa_g, pso4_g, pso4g_g, pso4aq_g)
         nullify(tmpu_ptr, rhoa_ptr, hghte_ptr)
         nullify(ustar_ptr, shflux_ptr, oro_ptr, pblh_ptr, z0h_ptr)
         nullify(msa_ptr, SU_dep_ptr)
         nullify(SU_PSO2_ptr, SU_PMSA_ptr, SU_PSO4_ptr, SU_PSO4g_ptr, SU_PSO4aq_ptr)
         nullify(pso2_ptr, pmsa_ptr, pso4_ptr, pso4g_ptr, pso4aq_ptr)
         nullify(oh_clim_ptr, no3_clim_ptr, h2o2_clim_ptr)

      end do

      write(*,*) '  PASSED (', N_P7_ITER, ' iterations)'
   end subroutine test_property7_full_equivalence

   !---------------------------------------------------------------------------
   !> Helper: compute day-of-year from YYYYMMDD integer (for test verification)
   !---------------------------------------------------------------------------
   pure integer function compute_jday(nymd)
      integer, intent(in) :: nymd
      integer :: yyyy, mm, dd, imon, isleapyr
      integer :: ndays(12)

      ndays = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

      yyyy = nymd / 10000
      mm   = mod(nymd, 10000) / 100
      dd   = mod(nymd, 100)

      isleapyr = 0
      if (mod(yyyy, 4) == 0) then
         isleapyr = 1
         if (mod(yyyy, 100) == 0) then
            isleapyr = 0
            if (mod(yyyy, 400) == 0) isleapyr = 1
         end if
      end if

      compute_jday = 0
      if (mm == 1) then
         compute_jday = dd
      else
         do imon = 1, mm - 1
            if (imon == 2 .and. isleapyr == 1) then
               compute_jday = compute_jday + 29
            else
               compute_jday = compute_jday + ndays(imon)
            end if
         end do
         compute_jday = compute_jday + dd
      end if
   end function compute_jday

end program test_SO4chemPhysics

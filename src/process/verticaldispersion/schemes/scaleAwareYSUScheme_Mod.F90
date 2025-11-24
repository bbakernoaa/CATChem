!> \file scaleAwareYSUScheme_Mod.F90
!! \brief Scale-aware YSU boundary layer scheme for vertical mixing
!! \ingroup ysuverticaldispersion_schemes
!!
!! \author CATChem Development Team
!! \date 2025
!! \version 1.0
!!
!! This module implements the scale-aware Yonsei University (YSU) boundary layer scheme
!! for vertical mixing of atmospheric tracers. The scheme includes:
!! - Eddy diffusivity calculation based on boundary layer height and stability
!! - Counter-gradient transport for convective conditions
!! - Scale-aware mixing to handle grid-scale effects
!! - Implicit time integration for numerical stability
!!
!! \references
!! [1] Hong, S.-Y., Y. Noh, and J. Dudhia (2006), A new vertical diffusion package
!!     with an explicit treatment of entrainment processes, Mon. Wea. Rev., 134, 2318-2341.
!! [2] Hong, S.-Y. (2010), A new stable boundary-layer mixing scheme and its impact
!!     on the simulated East Asian summer monsoon, Q. J. R. Meteorol. Soc., 136, 1481-1496.
!!
module scaleAwareYSUScheme_Mod
   use precision_mod
   use constants, only: g0, VON_KARMAN, Cp, Rd

   implicit none
   private

   public :: ysu_calculate_tendencies

   ! YSU scheme parameters
   real(fp), parameter :: BETA_M = 0.2_fp        ! Profile shape parameter for momentum
   real(fp), parameter :: BETA_H = 1.0_fp        ! Profile shape parameter for heat/mass
   real(fp), parameter :: PRANDTL = 1.0_fp       ! Turbulent Prandtl number
   real(fp), parameter :: MIN_KH = 1.0e-6_fp     ! Minimum eddy diffusivity [m2/s]
   real(fp), parameter :: MAX_KH = 1000.0_fp     ! Maximum eddy diffusivity [m2/s]
   real(fp), parameter :: GAMMA_M = 0.0_fp       ! Counter-gradient parameter (momentum)
   real(fp), parameter :: GAMMA_H = 7.8_fp       ! Counter-gradient parameter (heat/mass)
   real(fp), parameter :: GRID_SCALE_FACTOR = 0.1_fp  ! Scale-aware mixing factor

contains

   !> YSU vertical mixing for tracers
   !!
   !! Implements the scale-aware YSU boundary layer scheme for vertical mixing
   !! of atmospheric tracers using implicit time integration.
   !!
   !! \param[in]     nz         Number of vertical levels
   !! \param[in]     ntracers   Number of tracers
   !! \param[in]     dt         Time step [s]
   !! \param[in]     dz         Layer thickness [m]
   !! \param[in]     zh         Height at layer interfaces [m]
   !! \param[in]     zc         Height at layer centers [m]
   !! \param[in]     pblh       Boundary layer height [m]
   !! \param[in]     ustar      Friction velocity [m/s]
   !! \param[in]     wstar      Convective velocity scale [m/s]
   !! \param[in]     theta      Potential temperature [K]
   !! \param[in]     dx         Grid spacing [m] (for scale-aware mixing)
   !! \param[in]     tracers    Tracer concentrations [various units]
   !! \param[out]    tendencies Tracer tendencies [units/s]
   !! \param[out]    kh         Eddy diffusivity for heat/mass [m2/s]
   !! \param[out]    rc         Return code
   !!
   subroutine ysu_calculate_tendencies(nz, ntracers, dt, dz, zh, zc, pblh, ustar, wstar, &
      theta, dx, tracers, tendencies, kh, rc)

      integer, intent(in) :: nz, ntracers
      real(fp), intent(in) :: dt
      real(fp), intent(in) :: dz(nz)
      real(fp), intent(in) :: zh(nz+1)        ! Interface heights
      real(fp), intent(in) :: zc(nz)          ! Layer center heights
      real(fp), intent(in) :: pblh            ! PBL height
      real(fp), intent(in) :: ustar           ! Friction velocity
      real(fp), intent(in) :: wstar           ! Convective velocity scale
      real(fp), intent(in) :: theta(nz)       ! Potential temperature
      real(fp), intent(in) :: dx              ! Grid spacing
      real(fp), intent(in) :: tracers(nz, ntracers)
      real(fp), intent(out) :: tendencies(nz, ntracers)
      real(fp), intent(out) :: kh(nz+1)       ! Eddy diffusivity at interfaces
      integer, intent(out) :: rc

      ! Local variables
      real(fp) :: ws                           ! Mixed velocity scale
      real(fp) :: gamma_term                   ! Counter-gradient term
      real(fp) :: scale_factor                 ! Grid-scale factor
      real(fp) :: a(nz), b(nz), c(nz), d(nz) ! Tridiagonal matrix elements
      real(fp) :: mixed_tracers(nz)           ! Mixed tracer concentrations
      integer :: k, n

      rc = 0  ! Success
      tendencies = 0.0_fp  ! Initialize tendencies

      ! Calculate mixed velocity scale
      ws = sqrt(ustar**2 + (BETA_H * wstar)**2)

      ! Calculate scale-aware factor based on grid spacing
      scale_factor = 1.0_fp + GRID_SCALE_FACTOR * (dx / 1000.0_fp)**0.33_fp

      ! Calculate eddy diffusivity profile
      do k = 1, nz+1
         if (zh(k) <= pblh) then
            ! Within PBL: YSU profile
            kh(k) = VON_KARMAN * ws * zh(k) * (1.0_fp - zh(k) / pblh)**BETA_H * scale_factor
         else
            ! Above PBL: reduced mixing
            kh(k) = MIN_KH * exp(-(zh(k) - pblh) / 1000.0_fp)
         endif

         ! Apply bounds
         kh(k) = max(MIN_KH, min(MAX_KH, kh(k)))
      enddo

      ! Apply vertical mixing to each tracer using implicit scheme
      do n = 1, ntracers

         ! Copy current tracer values
         mixed_tracers(:) = tracers(:, n)

         ! Set up tridiagonal system for implicit mixing
         ! d(C)/dt = d/dz[Kh * d(C)/dz]
         ! Discretized as: (C^(n+1) - C^n)/dt = [Kh_(k+1/2) * (C^(n+1)_(k+1) - C^(n+1)_k) / dz_(k+1/2) -
         !                                       Kh_(k-1/2) * (C^(n+1)_k - C^(n+1)_(k-1)) / dz_(k-1/2)] / dz_k

         do k = 1, nz
            if (k == 1) then
               ! Bottom boundary (no flux)
               a(k) = 0.0_fp
               b(k) = 1.0_fp + dt * kh(k+1) / (dz(k) * 0.5_fp * (dz(k) + dz(k+1)))
               c(k) = -dt * kh(k+1) / (dz(k) * 0.5_fp * (dz(k) + dz(k+1)))
               d(k) = mixed_tracers(k)

            elseif (k == nz) then
               ! Top boundary (no flux)
               a(k) = -dt * kh(k) / (dz(k) * 0.5_fp * (dz(k-1) + dz(k)))
               b(k) = 1.0_fp + dt * kh(k) / (dz(k) * 0.5_fp * (dz(k-1) + dz(k)))
               c(k) = 0.0_fp
               d(k) = mixed_tracers(k)

            else
               ! Interior points
               a(k) = -dt * kh(k) / (dz(k) * 0.5_fp * (dz(k-1) + dz(k)))
               b(k) = 1.0_fp + dt * (kh(k) / (dz(k) * 0.5_fp * (dz(k-1) + dz(k))) + &
                  kh(k+1) / (dz(k) * 0.5_fp * (dz(k) + dz(k+1))))
               c(k) = -dt * kh(k+1) / (dz(k) * 0.5_fp * (dz(k) + dz(k+1)))
               d(k) = mixed_tracers(k)
            endif

            ! Add counter-gradient term for scalars in convective conditions
            if (wstar > 0.1_fp .and. zh(k) <= pblh .and. k > 1 .and. k < nz) then
               gamma_term = GAMMA_H * wstar * (theta(k+1) - theta(k-1)) / &
                  (2.0_fp * dz(k) * theta(k))
               d(k) = d(k) + dt * gamma_term
            endif
         enddo

         ! Solve tridiagonal system to get mixed concentrations
         call solve_tridiagonal(nz, a, b, c, d, mixed_tracers)

         ! Calculate tendencies as (C_new - C_old) / dt
         tendencies(:, n) = (mixed_tracers(:) - tracers(:, n)) / dt

      enddo

   end subroutine ysu_calculate_tendencies

   !> Solve tridiagonal system using Thomas algorithm
   !!
   !! Solves the system Ax = d where A is tridiagonal with:
   !! - a(i): sub-diagonal elements
   !! - b(i): diagonal elements
   !! - c(i): super-diagonal elements
   !!
   subroutine solve_tridiagonal(n, a, b, c, d, x)
      integer, intent(in) :: n
      real(fp), intent(in) :: a(n), b(n), c(n), d(n)
      real(fp), intent(out) :: x(n)

      real(fp) :: work_b(n), work_d(n)
      real(fp) :: factor
      integer :: i

      ! Forward elimination
      work_b(1) = b(1)
      work_d(1) = d(1)

      do i = 2, n
         factor = a(i) / work_b(i-1)
         work_b(i) = b(i) - factor * c(i-1)
         work_d(i) = d(i) - factor * work_d(i-1)
      enddo

      ! Back substitution
      x(n) = work_d(n) / work_b(n)

      do i = n-1, 1, -1
         x(i) = (work_d(i) - c(i) * x(i+1)) / work_b(i)
      enddo

   end subroutine solve_tridiagonal

end module scaleAwareYSUScheme_Mod

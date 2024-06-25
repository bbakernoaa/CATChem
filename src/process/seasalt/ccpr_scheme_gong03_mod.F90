!>
!! \file
!! \brief Contains the Gong 2003
!!
!! Gong, S. L. (2003), A parameterization of sea-salt aerosol source function for sub- and super-micron particles,
!! Global Biogeochem. Cycles, 17, 1097, doi:10.1029/2003GB002079, 4.
!!
!! \author Barry baker
!! \date 05/2024
!!
!! \ingroup catchem_seasalt_process
!!!>
module CCPr_Scheme_Gong03_Mod

   implicit none

   private

   public :: CCPr_Scheme_Gong03

contains

   !> \brief Run the Gong Sea Salt Emission scheme
   !!
   !! \param [IN] FROCEAN
   !! \param [IN] FRSEAICE
   !! \param [IN] U10M
   !! \param [IN] V10M
   !! \param [IN] SST
   !! \param [IN] WeibullFlag
   !! \param [IN] SeaSaltScaleFactor
   !! \param [IN] UpperBinRadius
   !! \param [IN] LowerBinRadius
   !! \param [IN] EffectiveRadius
   !! \param [IN] SeaSaltDensity
   !! \param [IN] EmissionBin
   !! \param [IN] NumberEmissionBin
   !! \param [IN] TotalEmission
   !! \param [IN] TotalNumberEmission
   !! \param [OUT] RC
   !!
   !! \ingroup catchem_seasalt_process
   !!!>
   subroutine CCPr_Scheme_Gong03(FROCEAN,              &
      FRSEAICE,             &
      U10M,                 &
      V10M,                 &
      SST,                  &
      WeibullFlag,          &
      SeaSaltScaleFactor,   &
      UpperBinRadius,       &
      LowerBinRadius,       &
      EffectiveRadius,      &
      SeaSaltDensity,       &
      EmissionBin,          &
      NumberEmissionBin,    &
      TotalEmission,        &
      TotalNumberEmission,  &
      RC)

      ! Uses
      Use CCPr_SeaSalt_Common_Mod
      use precision_mod, only : fp, ZERO, ONE, f8
      use constants,     only : PI

      implicit none

      ! Arguments
      !----------
      ! Inputs
      real(fp), intent(in) :: FROCEAN                                  !< Ocean Fraction
      real(fp), intent(in) :: FRSEAICE                                 !< SeaIce Fraction
      real(fp), intent(in) :: U10M                                     !< U-Component of 10m wind speed
      real(fp), intent(in) :: V10M                                     !< V-Component of 10m wind speed
      real(fp), intent(in) :: SST                                      !< Sea surface temperature
      logical,  intent(in) :: WeibullFlag                              !< Weibull Flag
      real(fp), intent(in) :: SeaSaltScaleFactor                       !< SeaSalt Tuning Parameter [-]
      real(fp), dimension(:), intent(in) :: UpperBinRadius             !< SeaSalt Upper Bin Radius [m]
      real(fp), dimension(:), intent(in) :: LowerBinRadius             !< SeaSalt Lower Bin Radius [m]
      real(fp), dimension(:), intent(in) :: EffectiveRadius            !< SeaSalt Effective Radius [m]
      real(fp), dimension(:), intent(in) :: SeaSaltDensity             !< SeaSalt Density [kg/m^3]

      ! Inputs/Outputs
      real(fp), dimension(:), intent(inout) :: EmissionBin             !< Emission Rate per Bin [ug/m2/s]
      real(fp), dimension(:), intent(inout) :: NumberEmissionBin       !< Number of particles emitted per bin [#/m2/s]
      real(fp), intent(inout)               :: TotalEmission           !< Total SeaSalt Emission [ug m-2 s-1]
      real(fp), intent(inout)               :: TotalNumberEmission     !< Total Number Emitted    [# m-2 s-1]

      ! Outputs
      integer, intent(out) :: RC                                 !< return code

      ! Local Variables
      logical :: do_seasalt                            !< Enable Dust Calculation Flag
      integer :: n, ir                                 !< Loop counter
      integer :: nbins                                 !< number of SeaSalt bins
      real(f8) :: w10m                                 !< 10m wind speed [m/s]
      ! real(fp), allocatable :: EmissionBin(:)          !< Emission Rate per Bin [kg/m2/s]
      ! real(fp), allocatable :: NumberEmissionBin(:)    !< Number of particles emitted per bin [#/m2/s]
      integer, parameter :: nr = 10                    !< Number of (linear) sub-size bins
      real, parameter    :: r80fac = 1.65              !< ratio of radius(RH=0.8)/radius(RH=0.) [Gerber]
      real(fp) :: DryRadius                            !< sub-bin radius         (dry, um)
      real(fp) :: DeltaDryRadius                       !< sub-bin radius spacing (dry, um)
      real(fp) :: rwet, drwet                          !< sub-bin radius spacing (rh=80%, um)
      real(fp) :: NumberEmissions                      !< sub-bin number emission rate [#/m2/s]
      real(fp) :: MassEmissions                        !< sub-bin number emission rate [kg/m2/s]
      real(fp) :: aFac
      real(fp) :: bFac
      real(fp) :: scalefac
      real(fp) :: rpow
      real(fp) :: exppow
      real(fp) :: wpow
      real(fp) :: MassScaleFac
      real(fp) :: gweibull
      real(fp) :: fsstemis
      real(fp) :: fhoppel
      real(fp) :: scale

      ! Initialize
      RC = -1
      EmissionBin = ZERO
      MassEmissions = ZERO
      NumberEmissions = ZERO
      gweibull = ONE
      fsstemis = ONE
      fhoppel = ONE

      nbins = size(EffectiveRadius)

      !--------------------------------------------------------------------
      ! Don't do Sea Salt over certain criteria
      !--------------------------------------------------------------------
      do_seasalt = .true. ! Default value for all cases

      ! Don't do Sea Salt over land
      !----------------------------------------------------------------
      scale = FROCEAN - FRSEAICE
      if (scale .eq. 0) then
         do_seasalt = .False.
      endif

      if (do_seasalt) then

         ! get 10m mean wind speed
         !------------------------
         w10m = sqrt(U10M ** 2 + V10M ** 2)

         ! Gong 03 Params
         !---------------
         scalefac = 1._fp
         rpow     = 3.45_fp
         exppow   = 1.607_fp
         wpow     = 3.41_fp

         ! Weibull Distribution following Fan and Toon 2011 if WeibullFlag
         !----------------------------------------------------------------------------
         call weibullDistribution(gweibull, WeibullFlag, w10m, RC)
         if (RC /= 0) then
            RC = -1
            print *, 'Error in weibullDistribution'
            return
         endif

         ! Get Jeagle SST Correction
         call jeagleSSTcorrection(fsstemis, SST,1, RC)
         if (RC /= 0) then
            RC = -1
            print *, 'Error in jeagleSSTcorrection'
            return
         endif

         scale = scale * gweibull * fsstemis * SeaSaltScaleFactor

         do n = 1, nbins

            ! delta dry radius
            !-----------------
            DeltaDryRadius = (UpperBinRadius(n) - LowerBinRadius(n) )/ nr

            ! Dry Radius Substep
            !-------------------
            DryRadius = LowerBinRadius(n) + 0.5 * DeltaDryRadius

            do ir = 1, nr ! SubSteps

               ! Mass scale fcator
               MassScaleFac = scalefac * 4._fp/3._fp*PI*SeaSaltDensity(n)*(DryRadius**3._fp) * 1.e-18_fp

               ! Effective Wet Radius in Sub Step
               rwet  = r80fac * DryRadius

               ! Effective Delta Wet Radius
               drwet = r80fac * DeltaDryRadius

               aFac = 4.7_fp*(1._fp + 30._fp*rwet)**(-0.017_fp*rwet**(-1.44_fp))
               bFac = (0.433_fp-log10(rwet))/0.433_fp

               ! Number emissions flux (# m-2 s-1)
               NumberEmissions = NumberEmissions + SeasaltEmissionGong( rwet, drwet, w10m, scalefac, &
                  aFac, bFac, rpow, exppow, wpow )

               ! Mass emissions flux (kg m-2 s-1)
               MassEmissions = MassEmissions + SeasaltEmissionGong( rwet, drwet, w10m, MassScaleFac, &
                  aFac, bFac, rpow, exppow, wpow )

               DryRadius = DryRadius + DeltaDryRadius

            enddo

            EmissionBin(n) = MassEmissions * scale * 1.0e9_fp ! Convert to kg/m2/s from ug/m2/s
            NumberEmissionBin(n) = NumberEmissions * scale

            MassEmissions = ZERO
            NumberEmissions = ZERO

         enddo ! nbins

         TotalEmission = sum(EmissionBin)
         TotalNumberEmission = sum(NumberEmissionBin)

      endif ! do_Sea Salt

      RC = 0
   end subroutine CCPr_Scheme_Gong03

end module CCPr_Scheme_Gong03_Mod

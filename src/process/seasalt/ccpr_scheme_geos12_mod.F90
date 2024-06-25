!>
!! \file
!! \brief Contains the GEOS2012 Sea Salt Emission algorithm
!!
!! Jaeglé, L., Quinn, P. K., Bates, T. S., Alexander, B., and Lin, J.-T.: Global distribution of sea salt aerosols:
!! new constraints from in situ and remote sensing observations, Atmos. Chem. Phys., 11, 3137–3157,
!! https://doi.org/10.5194/acp-11-3137-2011, 2011.
!!
!! \author Barry baker
!! \date 05/2024
!!
!! \ingroup catchem_seasalt_process
!!!>
module CCPr_Scheme_GEOS12_Mod

   implicit none

   private

   public :: CCPr_Scheme_GEOS12

contains

   !> \brief Scheme GEOS2012 Sea Salt Emission scheme
   !!
   !! \param   [IN] FROCEAN
   !! \param   [IN] FRSEAICE
   !! \param   [IN] USTAR
   !! \param   [IN] SST
   !! \param   [IN] SeaSaltScaleFactor
   !! \param   [IN] UpperBinRadius
   !! \param   [IN] LowerBinRadius
   !! \param   [IN] EffectiveRadius
   !! \param   [IN] SeaSaltDensity
   !! \param   [IN] EmissionBin
   !! \param   [IN] NumberEmissionBin
   !! \param   [IN] TotalEmission
   !! \param   [IN] TotalNumberEmission
   !! \param   [OUT] RC
   !!
   !! \ingroup catchem_seasalt_process
   !!!>
   subroutine CCPr_Scheme_GEOS12(FROCEAN,             &
      FRSEAICE,            &
      USTAR,               &
      SST,                 &
      SeaSaltScaleFactor,  &
      UpperBinRadius,      &
      LowerBinRadius,      &
      EffectiveRadius,     &
      SeaSaltDensity,      &
      EmissionBin,         &
      NumberEmissionBin,   &
      TotalEmission,       &
      TotalNumberEmission, &
      RC)

      ! Uses
      Use CCPr_SeaSalt_Common_Mod
      use precision_mod, only : fp, ZERO, ONE, f8
      use constants,     only : PI

      implicit none

      ! Arguments
      !----------
      ! Inputs
      real(fp), intent(in) :: FROCEAN                                !< Ocean Fraction
      real(fp), intent(in) :: FRSEAICE                               !< SeaIce Fraction
      real(fp), intent(in) :: USTAR                                  !< Friction velocity
      real(fp), intent(in) :: SST                                    !< Sea surface temperature
      real(fp), intent(in) :: SeaSaltScaleFactor                     !< SeaSalt Tuning Parameter [-]
      real(fp), dimension(:), intent(in) :: UpperBinRadius           !< SeaSalt Upper Bin Radius [m]
      real(fp), dimension(:), intent(in) :: LowerBinRadius           !< SeaSalt Lower Bin Radius [m]
      real(fp), dimension(:), intent(in) :: EffectiveRadius          !< SeaSalt Effective Radius [m]
      real(fp), dimension(:), intent(in) :: SeaSaltDensity           !< SeaSalt Density [kg/m^3]

      ! Inputs/Outputs
      real(fp), intent(inout)               :: TotalEmission         !< Total SeaSalt Emission [ug m-2 s-1]
      real(fp), intent(inout)               :: TotalNumberEmission   !< Total SeaSalt Emission [# m-2 s-1]
      real(fp), dimension(:), intent(inout) :: EmissionBin           !< SeaSalt Emission [ug m-2 s-1]
      real(fp), dimension(:), intent(inout) :: NumberEmissionBin     !< SeaSalt Emission [# m-2 s-1]

      ! Outputs
      integer, intent(out) :: RC !< Return code

      ! Local Variables
      logical :: do_seasalt                            !< Enable Dust Calculation Flag
      integer :: n, ir                                 !< Loop counter
      integer :: nbins                                 !< number of SeaSalt bins
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
      real(fp) :: fsstemis
      real(fp) :: fhoppel
      real(fp) :: scale

      ! Initialize
      RC = -1
      EmissionBin = ZERO
      MassEmissions = ZERO
      NumberEmissions = ZERO
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

         ! GEOS 12 Params
         !---------------
         scalefac = 33.0e3_fp
         rpow     = 3.45_fp
         exppow   = 1.607_fp
         wpow     = 3.41_fp - 1._fp

         ! Get Jeagle SST Correction
         call jeagleSSTcorrection(fsstemis, SST,1, RC)
         if (RC /= 0) then
            RC = -1
            print *, 'Error in jeagleSSTcorrection'
            return
         endif

         ! Total emission scale
         scale = scale * fsstemis * SeaSaltScaleFactor

         do n = 1, nbins

            ! delta dry radius
            !-----------------
            DeltaDryRadius = (UpperBinRadius(n) - LowerBinRadius(n) ) / nr

            ! Dry Radius Substep
            !-------------------
            DryRadius = LowerBinRadius(n) + 0.5 * DeltaDryRadius

            ! Mass scale fcator
            MassScaleFac = scalefac * 4._fp/3._fp*PI*SeaSaltDensity(n)*(DryRadius**3._fp) * 1.e-18_fp

            do ir = 1, nr ! SubSteps

               ! Effective Wet Radius in Sub Step
               rwet  = r80fac * DryRadius

               ! Effective Delta Wet Radius
               drwet = r80fac * DeltaDryRadius

               aFac     = 4.7_fp*(1._fp + 30._fp*rwet)**(-0.017_fp*rwet**(-1.44_fp))
               bFac     = (0.380_fp-log10(rwet))/0.65_fp

               ! Number emissions flux (# m-2 s-1)
               NumberEmissions = NumberEmissions + SeasaltEmissionGong( rwet, drwet, real(ustar, f8), scalefac, aFac, &
                  bFac, rpow, exppow, wpow )

               ! Mass emissions flux (kg m-2 s-1)
               MassEmissions = MassEmissions + SeasaltEmissionGong( rwet, drwet, real(ustar, f8), MassScaleFac, &
                  aFac, bFac, rpow, exppow, wpow )

               DryRadius = DryRadius + DeltaDryRadius

            enddo

            EmissionBin(n) = MassEmissions * scale * 1.0e9_fp ! convert to kg m-2 s-1 from ug m-2 s-1
            NumberEmissionBin(n) = NumberEmissions * scale

            MassEmissions = ZERO
            NumberEmissions = ZERO

         enddo ! nbins

         TotalEmission = sum(EmissionBin)
         TotalNumberEmission = sum(NumberEmissionBin)

      endif ! do_Sea Salt

      RC = 0

   end subroutine CCPr_Scheme_GEOS12

end module CCPr_Scheme_GEOS12_Mod

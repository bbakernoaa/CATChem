!>
!! \file ccpr_SeaSalt_common_mod.F90
!! \brief Contains module ccpr_SeaSalt_common_mod
!!
!! \author Barry Baker
!! \date 05/2024
!!
!! \ingroup catchem_seasalt_process
!!!>
module CCPr_SeaSalt_Common_Mod
   use precision_mod, Only : fp, ZERO, f8
   implicit none

   private

   PUBLIC :: SeasaltEmissionGong
   PUBLIC :: weibullDistribution
   PUBLIC :: jeagleSSTcorrection

   !> \brief Type for CATCHem SeaSalt Process
   !!
   !! \details Contains all the information needed to run the CATChem SeaSalt Process
   !!
   !! This type contains the following variables:
   !! - Activate : Activate Process (True/False)
   !! - nSeaSaltSpecies : Number of SeaSalt processes
   !! - SchemeOpt : Scheme Option
   !! - SeaSaltSpeciesIndex : Index of SeaSalt species
   !! - SpcIDs : CATChem species IDs
   !! - LowerBinRadius : Lower bin radius [m]
   !! - UpperBinRadius : Upper bin radius [m]
   !! - EffectiveRadius : Effective radius [m]
   !! - SeaSaltDensity : SeaSalt density [kg m-3]
   !! - BetaScaleFactor : Beta Scaling Parameter  [-]
   !! - AlphaScaleFactor : Scaling Parameter [-]
   !! - TotalEmission : Total emission [ug m-2 s-1]
   !! - TotalNumberEmission : Total number emission [# m-2 s-1]
   !! - EmissionPerSpecies : Emission rate   [ug m-2 s-1]
   !! - SeaSaltScaleFactor : SeaSalt Tuning Parameter [-]
   !! - WeibullFlag : Apply Weibull Distribution to 10m wind speeds following Fan and Toon 2011
   !! - HoppelFlag : Apply Hoppel Flag following Fan and Toon 2011
   !! - NumberEmissionBin : Number of Emission per Bin [# m-2 s-1]
   !!
   !! \ingroup catchem_seasalt_process
   !!!>
   TYPE, PUBLIC :: SeaSaltStateType
      ! Generic Variables for Every Process
      Logical                         :: Activate               !< Activate Process (True/False)
      INTEGER                         :: nSeaSaltSpecies        !< Number of SeaSalt processes
      INTEGER                         :: SchemeOpt              !< Scheme Option
      INTEGER, ALLOCATABLE            :: SeaSaltSpeciesIndex(:) !< Index of SeaSalt species
      INTEGER, ALLOCATABLE            :: SpcIDs(:)              !< CATChem species IDs
      Logical                         :: WeibullFlag            !< Apply Weibull Distribution to 10m wind speeds following Fan and Toon 2011
      Logical                         :: HoppelFlag             !< Apply Hoppel Flag following Fan and Toon 2011
      ! Process Specific Parameters
      REAL(fp), ALLOCATABLE           :: LowerBinRadius(:)      !< Lower bin radius        [m]
      REAL(fp), ALLOCATABLE           :: UpperBinRadius(:)      !< Upper bin radius        [m]
      REAL(fp), ALLOCATABLE           :: EffectiveRadius(:)     !< Effective radius        [m]
      REAL(fp), ALLOCATABLE           :: SeaSaltDensity(:)      !< SeaSalt density         [kg m-3]

      REAL(fp), ALLOCATABLE           :: EmissionPerSpecies(:)  !< Emission per species    [ug m-2 s-1]
      REAL(fp), ALLOCATABLE           :: NumberEmissionBin(:)   !< Particle Number emission per species [# m-2 s-1]
      REAL(fp)                        :: SeaSaltScaleFactor     !< SeaSalt Tuning Parameter [-]
      REAL(fp)                        :: TotalEmission          !< Total emission          [ug m-2 s-1]
      REAL(fp)                        :: TotalNumberEmission    !< Total Number Emitted    [# m-2 s-1]

      !=================================================================
      ! Module specific variables/arrays/data pointers come below
      !=================================================================

   END TYPE SeaSaltStateType


contains

   !>
   !! \brief Function to compute sea salt emissions following the Gong style parameterization.
   !!
   !! Functional form is from Gong 2003:
   !!  dN/dr = scalefac * 1.373 * (w^wpow) * (r^-aFac) * (1+0.057*r^rpow) * 10^(exppow*exp(-bFac^2))
   !! where r is the particle radius at 80% RH, dr is the size bin width at 80% RH, and w is the wind speed
   !!
   !! \ingroup catchem_seasalt_process
   !!!>
   function SeasaltEmissionGong ( r, dr, w, scalefac, aFac, bFac, rpow, exppow, wpow )

      real(fp), intent(in) :: r         !< Wet particle radius [um]
      real(fp), intent(in) :: dr        !< Wet particle bin width [um]
      real(f8), intent(in) :: w         !< Grid box mean wind speed [m s-1] (10-m or ustar wind)
      real(fp), intent(in) :: scalefac  !< scale factor
      real(fp), intent(in) :: aFac
      real(fp), intent(in) :: bFac
      real(fp), intent(in) :: rpow
      real(fp), intent(in) :: exppow
      real(fp), intent(in) :: wpow
      real(fp)             :: SeasaltEmissionGong

      !  Initialize
      SeasaltEmissionGong = 0.

      !  Particle size distribution function
      SeasaltEmissionGong = scalefac * 1.373_fp*r**(-aFac)*(1._fp+0.057_fp*r**rpow) &
         *10._fp**(exppow*exp(-bFac**2._fp))*dr
      !  Apply wind speed function
      SeasaltEmissionGong = w**wpow * SeasaltEmissionGong

   end function SeasaltEmissionGong

   !>
   !! \brief Jeagle et al. 2012 SST correction
   !!
   !! Jaeglé, L., Quinn, P. K., Bates, T. S., Alexander, B., and Lin, J.-T.:
   !! Global distribution of sea salt aerosols: new constraints from in situ and remote
   !! sensing observations, Atmos. Chem. Phys., 11, 3137–3157,
   !! https://doi.org/10.5194/acp-11-3137-2011, 2011.
   !!
   !! \ingroup catchem_seasalt_process
   !!!>
   subroutine jeagleSSTcorrection(fsstemis, sst, sstFlag, rc)

      ! !USES:
      implicit NONE

      ! !INPUT/OUTPUT PARAMETERS:
      real(fp), intent(inout) :: fsstemis     !
      real(fp), intent(in)  :: sst  ! surface temperature (K)
      integer, intent(in) :: sstFlag

      ! !OUTPUT PARAMETERS:
      integer, optional, intent(out) :: rc
      !EOP

      ! !Local Variables
      real(fp) :: tskin_c
      !EOP
      !-------------------------------------------------------------------------
      !  Begin...
      RC = -1 ! Error code
      fsstemis = 1.0_fp

      fsstemis = ZERO
      tskin_c  = sst - 273.15
      if (sstFlag .eq. 1) then
         fsstemis = max(0.,(0.3 + 0.1*tskin_c - 0.0076*tskin_c**2 + 0.00021*tskin_c**3))
      else
         ! temperature range (0, 36) C
         tskin_c = max(-0.1_fp, Tskin_c)
         tskin_c = min(36.0, tskin_c)

         fsstemis = (-1.107211_fp -0.010681_fp * tskin_c -0.002276_fp * tskin_c**2.0_fp &
            + 60.288927_fp*1.0_fp/(40.0_fp - tskin_c))
         fsstemis = max(0.0_fp, fsstemis)
         fsstemis = min(7.0_fp, fsstemis)
      endif

      RC = 0
   end subroutine jeagleSSTcorrection

   !>
   !! \brief Calculate the weibull distribution for 10m wind speed (u10, v10)
   !!
   !! The Weibull distribution correction ends up being a multiplicative constant
   !! (g) times our present source function (see Eq. 12 in Fan & Toon, 2011 and notes for
   !! (9/22/11). This constant is derived from the incomplete and complete forms of the gamma
   !! function, hence the utilities pasted below.  The Weibull function and shape
   !! parameters (k, c) assumed are from Justus 1978.
   !!
   !! \param[inout] gweibull Multiplicative constant
   !! \param[in]    weibullFlag  Flag for weibull correction
   !! \param[in]    wm 10m wind speed
   !! \param[out]   RC Return Code
   !!
   !! \ingroup catchem_seasalt_process
   !!!>
   subroutine weibullDistribution(gweibull, weibullFlag, wm, RC)

      implicit none

      ! Input/Output
      !-------------
      real(fp), intent(inout) :: gweibull

      ! Input
      !------
      logical,  intent(in)    :: weibullFlag
      real(f8), intent(in)    :: wm

      ! Output
      !-------
      integer,  intent(out)   :: RC

      ! Local Variables
      real(f8) :: a, c, k, wt, x
      character(len=256) :: errMsg, thisLoc !  needed for error handling                      thisLoc
      ! Initialize
      errMsg = ''
      thisLoc = ' -> at weibullDistribution (in util/metutils_mod.F90)'
      RC = 0
      gweibull = 1.0_fp

      wt = 4.d0

      if (weibullFlag) then
         gweibull = 0.0_fp

         if (wm > 0.01) then
            k = 0.94d0 * sqrt(wm)
            c = wm / gamma(1.d0 + 1.d0 / k)
            x = (wt / c) ** k
            a = 3.41d0 / k + 1.d0
            gweibull = (c / wm) ** 3.41d0 * igamma(a,x, RC)
         endif
      endif


   end subroutine weibullDistribution

   !>
   !! \brief Calculate the incomplete Gamma function
   !!
   !! The incomplete Gamma function is defined as
   !! \int_x^\infty t^{A-1}\exp(-t) dt
   !!
   !! \param[in]    A
   !! \param[in]    X
   !! \param[out]   RC
   !!
   !! \ingroup catchem_seasalt_process
   !!!>
   DOUBLE PRECISION function igamma(A, X, rc)

      IMPLICIT NONE
      double precision, intent(in) ::        A
      DOUBLE PRECISION, INTENT(IN) ::      X

      integer, intent(out) :: rc
      ! LOCAL VARIABLE
      DOUBLE PRECISION :: XAM, GIN,  S, R, T0
      INTEGER K
      rc = 0

      XAM=-X+A*LOG(X)
      IF (XAM.GT.700.0.OR.A.GT.170.0) THEN
         WRITE(*,*)'IGAMMA: a and/or x too large, X = ', X
         WRITE(*,*) 'A = ', A
         rc = -1
         return
      ENDIF

      IF (X.EQ.0.0) THEN
         IGAMMA=GAMMA(A)

      ELSE IF (X.LE.1.0+A) THEN
         S=1.0/A
         R=S
         DO  K=1,60
            R=R*X/(A+K)
            S=S+R
            IF (ABS(R/S).LT.1.0e-15) EXIT
         END DO
         GIN=EXP(XAM)*S
         IGAMMA=GAMMA(A)-GIN
      ELSE IF (X.GT.1.0+A) THEN
         T0=0.0
         DO K=60,1,-1
            T0=(K-A)/(1.0+K/(X+T0))
         end do

         IGAMMA=EXP(XAM)/(X+T0)

      ENDIF

   end function igamma

end module CCPr_SeaSalt_Common_Mod

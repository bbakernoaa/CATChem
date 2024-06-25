!>
!! \file ccpr_dust_common_mod.F90
!! \brief Contains module ccpr_dust_common_mod
!!
!! \ingroup catchem_dust_process
!!
!! \author Barry Baker
!! \date 05/2024
!!!>
module CCPr_Dust_Common_Mod
   use precision_mod, only: fp, ZERO
   implicit none
   private

   public :: Fecan_SoilMoisture
   public :: Shao_SoilMoisture
   public :: KokDistribution
   public :: Soil_Erosion_Potential
   public :: Draxler_HorizFlux
   public :: Kawamura_HorizFlux
   public :: MB95_DragPartition
   public :: MB97_threshold_velocity
   public :: DustStateType

   !> \brief Type for CATCHem Dust Process
   !!
   !! \details Contains all the information needed to run the CATChem Dust Process
   !!
   !! This type contains the following variables:
   !! - Activate : Activate Process (True/False)
   !! - nDustSpecies : Number of dust processes
   !! - SchemeOpt : Scheme Option (1=Fengsha, 2=Ginoux)
   !! - DustSpeciesIndex : Index of dust species
   !! - SpcIDs : CATChem species IDs
   !! - LowerBinRadius : Lower bin Radius        [m]
   !! - UpperBinRadius : Upper bin Radius        [m]
   !! - EffectiveRadius : Effective Radius        [m]
   !! - DustDensity : Dust Particle Density            [kg/m^3]
   !! - BetaScaleFactor : Beta Scaling Parameter  [1]
   !! - AlphaScaleFactor : Alpha Scaling Parameter [1]
   !! - TotalEmission : Total Emission          [ug/m^2/s]
   !! - EmissionPerSpecies : Emission Rate per dust species  [ug/m^2/s]
   !! - MoistOpt : Moisture Calculation Option
   !! - DragOpt : Drag Calculation Option
   !! - HorizFluxOpt : Horizontal Flux Calculation Option
   !!
   !! \ingroup catchem_dust_process
   !!!>
   TYPE :: DustStateType
      ! Generic Variables for Every Process
      Logical                         :: Activate                !< Activate Process (True/False)
      INTEGER                         :: nDustSpecies            !< Number of dust processes
      INTEGER                         :: SchemeOpt               !< Scheme Option
      INTEGER, ALLOCATABLE            :: DustSpeciesIndex(:)     !< Index of dust species
      INTEGER, ALLOCATABLE            :: SpcIDs(:)               !< CATChem species IDs

      ! Process Specific Parameters
      REAL(fp), ALLOCATABLE           :: LowerBinRadius(:)       !< Lower bin radius        [m]
      REAL(fp), ALLOCATABLE           :: UpperBinRadius(:)       !< Upper bin radius        [m]
      REAL(fp), ALLOCATABLE           :: EffectiveRadius(:)      !< Effective radius        [m]
      REAL(fp), ALLOCATABLE           :: DustDensity(:)          !< Dust density            [kg/m^3]
      REAL(fp)                        :: BetaScaleFactor         !< Gamma Scaling Parameter  [1]
      REAL(fp)                        :: AlphaScaleFactor        !< Alpha Scaling Parameter [1]
      REAL(fp)                        :: TotalEmission           !< Total emission          [kg/m^2/s]
      REAL(fp), ALLOCATABLE           :: EmissionPerSpecies(:)   !< Emission per species    [kg/m^2/s]

      ! Scheme Options
      INTEGER                         :: MoistOpt                !< Fengsha-Moisture Calculation Option
      INTEGER                         :: DragOpt                 !< Fengsha-Drag Calculation Option
      INTEGER                         :: HorizFluxOpt            !< Horizontal Flux Calculation Option


      !=================================================================
      ! Module specific variables/arrays/data pointers come below
      !=================================================================

   END TYPE DustStateType

contains
   !>
   !! \brief Computes the soil moisture attenuation factor for dust emission
   !!
   !! Fecan, F., Marticorena, B., and Bergametti, G.: Parametrization of the increase of the aeolian
   !! erosion threshold wind friction velocity due to soil moisture for arid and semi-arid areas,
   !! Ann. Geophys., 17, 149–157, https://doi.org/10.1007/s00585-999-0149-7, 1999.
   !!
   !! \param clay Fractional clay content
   !! \param sand Fractional sand content
   !! \param volumetric_soil_moisture Volumetric soil moisture
   !! \param H Soil moisture attenuation factor for dust emission
   !!
   !! \ingroup catchem_dust_process
   !!!>
   subroutine Fecan_SoilMoisture( clay, sand, volumetric_soil_moisture, H)
      IMPLICIT NONE
      ! Parameters
      !-----------
      real(fp), intent(in)  :: clay                      !< Fractional Clay Content
      real(fp), intent(in)  :: sand                      !< Fractional Sand Content
      real(fp), intent(in)  :: volumetric_soil_moisture  !< Volumetric soil moisture [m3 m-3]
      real(fp), intent(out) :: H                         !< Soil Moisture attenuation factor for dust emission [1]

      ! Local Variables
      !----------------
      real(fp) :: vsat                      !< Saturated volumetric water content (sand-dependent) [m3 m-3]

      real(fp) :: gravimetric_soil_moisture !< Gravimetric soil moisture [kg/kg]
      real(fp) :: DryLimit                  !< Dry limit of the soil moisture [kg/kg]

      ! Initialize
      !-----------
      H = ZERO

      !--------------------------------------------
      ! Compute Saturated Volumetric Water Content
      !--------------------------------------------
      vsat = 0.489_fp - 0.00126_fp * (100._fp * sand)

      !--------------------------------------------
      ! Compute Gravimetric Soil moisture
      !--------------------------------------------
      gravimetric_soil_moisture = volumetric_soil_moisture * 1000.0_fp / (1.0_fp - vsat)

      !--------------------------------------------
      ! Compute Dry Limit
      !--------------------------------------------
      DryLimit = clay * (14.0 * clay + 17.0)

      !--------------------------------------------
      ! Compute attenuation factor
      !--------------------------------------------
      H = sqrt(1.0_fp + 1.21_fp * max(0._fp, gravimetric_soil_moisture - DryLimit)**0.68_fp)
      return

   end subroutine Fecan_SoilMoisture

   !>
   !! \brief Computes the soil moisture attenuation factor for dust emission
   !!
   !! Zhao, T. L., S. L. Gong, X. Y. Zhang, A. Abdel-Mawgoud, and Y. P. Shao (2006),
   !! An assessment of dust emission schemes in modeling east Asian dust storms,
   !! J. Geophys. Res., 111, D05S90, doi:10.1029/2004JD005746.
   !!
   !! \param volumetric_soil_moisture Volumetric soil moisture
   !! \param H Soil moisture attenuation factor for dust emission
   !!
   !! \ingroup catchem_dust_process
   !!!>
   subroutine Shao_SoilMoisture( volumetric_soil_moisture, H)
      IMPLICIT NONE
      ! Parameters
      real(fp), intent(in)  :: volumetric_soil_moisture  !< Volumetric soil moisture [m3 m-3]
      real(fp), intent(out) :: H                         !< Soil Moisture attenuation factor for dust emission [1]

      ! Initialize
      H = ZERO

      !--------------------------------------------
      ! Compute attenuation factor
      !--------------------------------------------
      if (volumetric_soil_moisture <= 0.03_fp) THEN
         H = exp(22.7_fp * volumetric_soil_moisture)
      else
         H = exp(93.5_fp * volumetric_soil_moisture - 2.029_fp)
      endif

      return

   end subroutine Shao_SoilMoisture

   !>
   !! \brief KokDistribution
   !!
   !! Kok, J. F. (2011a), A scaling theory for the size distribution of emitted
   !! dust aerosols suggests climate models underestimate the size of the global
   !! dust cycle, Proc. Natl. Acad. Sci. U. S. A., 108(3), 1016–1021,
   !! doi:10.1073/pnas.1014798108.
   !!
   !! \param radius Radius
   !! \param rLow Lower radius
   !! \param rUp Upper radius
   !! \param dist Distribution
   !!
   !! \ingroup catchem_dust_process
   !!!>
   subroutine KokDistribution(radius, rLow, rUp, dist)
      use constants, only: pi
      IMPLICIT NONE
      ! Parameters
      real(fp), dimension(:), intent(in)  :: radius
      real(fp), dimension(:), intent(in)  :: rLow
      real(fp), dimension(:), intent(in)  :: rUp
      real(fp), dimension(:), intent(out) :: dist

      ! Local Variables
      integer :: n          !< looping variable
      integer :: nbins      !< number of bins
      real(fp) :: diameter  !< effective diameter of particle
      real(fp) :: dlam      !< diameter / lambda
      real(fp) :: dvol      !< volume of particle

      ! Constants
      real(fp), parameter :: mmd = 3.4                               !< median mass diameter [microns]
      real(fp), parameter :: stddev = 3.0                            !< standard deviation [microns]
      real(fp), parameter :: lambda = 12.0                           !< crack propagation length [um]
      real(fp), parameter :: factor = 1. / ( sqrt(2.) * log(stddev)) !< auxiliary constant for the distribution

      ! Initialize
      dvol = ZERO
      dist = ZERO
      nbins = size(radius)

      do n = 1, nbins
         diameter = radius(n) * 2.0_fp
         dlam = diameter / lambda
         dvol = 4._fp / 3._fp * pi * diameter**3.0_fp
         diameter = (1._fp + erf(factor * log(diameter/mmd))) * exp(-dlam * dlam * dlam) * log(rUp(n)/rLow(n))
         dvol = dvol + dist(n)
      end do

      ! Normalize Distribution
      do n = 1, nbins
         dist(n) = dist(n) / dvol
      end do
      return
   end subroutine KokDistribution

   !>
   !! \brief Computes the soil erosion potential
   !!
   !! TODO: Find reference for this
   !!
   !! \param clayfrac clay fraction
   !! \param sandfrac sand fraction
   !! \param SEP soil erosion potential
   !!
   !! \ingroup catchem_dust_process
   !!!>
   subroutine Soil_Erosion_Potential(clayfrac, sandfrac, SEP)

      IMPLICIT NONE
      ! Parameters
      real(fp), intent(in)    :: clayfrac !< Fractional Clay Content (0-1)
      real(fp), intent(in)    :: sandfrac !< Fractional Sand Content (0-1)
      real(fp), intent(inout) :: SEP      !< Soil Erosion Potential (0-1)


      ! Initialize
      SEP = ZERO

      !--------------------------------------------
      ! Compute SEP
      !--------------------------------------------
      if (clayfrac > 0.0 .and. sandfrac > 0.0) then
         SEP = (0.08_fp * clayfrac + 0.12_fp * sandfrac + (1 - sandfrac - clayfrac))
      endif

      return

   end subroutine Soil_Erosion_Potential

   !>
   !! \brief Computes Draxler Hoirizontal Flux
   !!
   !! Draxler, R.R, D.A. Gillette, J.S. Kirkpatrick, and J. Heller (2001),
   !! Estimating PM10 air concentrations from dust storms in Iraq, Kuwait,
   !! and Saudi Arabia, Atm. Environ, 35: 4315-4330.
   !! https://doi.org/10.1016/S1352-2310(01)00159-5
   !!
   !! \param ustar friction velocity
   !! \param ustar_threshold dry threshold friction velocity
   !! \param R Drag partition
   !! \param H Soil Moisture Attenuation Factor
   !! \param HorizFlux Horizontal Mass Flux
   !!
   !!
   !! \ingroup catchem_dust_process
   !!!>
   subroutine Draxler_HorizFlux(ustar, ustar_threshold, R, H, HorizFlux)
      IMPLICIT NONE
      ! Parameters
      real(fp), intent(in)  :: ustar            !< friction velocity [m/s]
      real(fp), intent(in)  :: ustar_threshold  !< dry threshold friction velocity [m/s]
      real(fp), intent(in)  :: R                !< Drag partition (0-1)
      real(fp), intent(in)  :: H                !< Soil Moisture Attenuation Factor
      real(fp), intent(inout) :: HorizFlux      !< Horizontal Mass Flux [kg/m2/s]

      ! Local Variables
      !----------------
      real(fp) :: u_ts    !< Modified threshold friction velocity

      ! Initialize
      !-----------
      HorizFlux = ZERO

      !--------------------------------------------
      ! Compute Draxler Horizontal Flux
      !--------------------------------------------
      u_ts = ustar_threshold * H / R

      if (ustar >= ustar_threshold) then
         HorizFlux = max(0._fp ,(ustar * R) ** 3.0_fp * (1.0_fp - ( u_ts / ustar ) ** 2.0_fp))
      endif


      return
   end subroutine Draxler_HorizFlux

   !>
   !! \brief Computes Kawamura Hoirizontal Flux
   !!
   !! Kawamura, R., 1951. Study on sand movement by wind. Report, 5(3), pp.95-112.
   !!
   !! Webb, N., Chappell, A., LeGrand, S., Ziegler, N., Edwards, B. 2020.
   !! A note on the use of drag partition in aeolian transport models.
   !! Aeolian Research. 42:100560. https://doi.org/10.1016/j.aeolia.2019.100560.
   !!
   !! \param ustar friction velocity
   !! \param ustar_threshold dry threshold friction velocity
   !! \param R Drag partition
   !! \param H Soil Moisture Attenuation Factor
   !! \param HorizFlux Horizontal Mass Flux
   !!
   !! \ingroup catchem_dust_process
   !!!>
   subroutine Kawamura_HorizFlux(ustar, ustar_threshold, R, H, HorizFlux)
      IMPLICIT NONE
      ! Parameters
      real(fp), intent(in)  :: ustar           !< friction velocity [m/s]
      real(fp), intent(in)  :: ustar_threshold !< dry threshold friction velocity [m/s]
      real(fp), intent(in)  :: R               !< Drag partition (0-1)
      real(fp), intent(in)  :: H               !< Soil Moisture Attenuation Factor
      real(fp), intent(inout) :: HorizFlux     !<

      ! Local Variables
      real(fp) :: u_ts !< Modified threshold friction velocity

      ! Initialize
      HorizFlux = ZERO

      !--------------------------------------------
      ! Compute Kawamura Horizontal Flux
      !--------------------------------------------
      u_ts = ustar_threshold * H / R

      HorizFlux = MAX(0._fp, (ustar ** 3.0_fp * (1.0_fp - (u_ts / ustar) ** 2.0_fp) * (1.0_fp + (u_ts / ustar) ** 2.0_fp ) ) )

      return
   end subroutine Kawamura_HorizFlux

   !>
   !! \brief Computes the Drag Partition from MB95
   !!
   !! Marticorena, B. and Bergametti, G.: Modeling the atmospheric dust cycle:
   !! 1. Design of a soil-derived dust emission scheme,
   !! J. Geophys. Res.-Atmos., 100, 16415–16430, https://doi.org/10.1029/95JD00690, 1995
   !!
   !! \param z0 roughness length
   !! \param R Drag partition
   !!
   !! \ingroup catchem_dust_process
   !!!>
   subroutine MB95_DragPartition(z0, R)
      IMPLICIT NONE
      ! Parameters
      real(fp), intent(in)  :: z0   !< roughness length [m]
      real(fp), intent(out) :: R    !< Drag partition (0-1)

      ! Local Variables
      real(fp), parameter :: z0s = 0.0008467 !< ideal roughness length of soil

      ! Initialize
      R = ZERO

      !--------------------------------------------
      ! MB95 Drag Partition
      !--------------------------------------------
      R = 1.0_fp - (log(z0 / z0s ) / log(0.7_fp * (0.1_fp / z0s) ** 0.8_fp))
      return

   end subroutine MB95_DragPartition

   !>
   !! \brief Computes the Threshold Friction Velocity from MB97
   !!
   !! Marticorena, B. and Bergametti, G.: Modeling the atmospheric dust cycle:
   !! 1. Design of a soil-derived dust emission scheme,
   !! J. Geophys. Res.-Atmos., 100, 16415–16430, https://doi.org/10.1029/95JD00690, 1995 | TODO fix with correct reference
   !!
   !! \param soil_density soil density
   !! \param air_density air density
   !! \param radius particle radius
   !! \param ustar_threshold threshold friction velocity
   !!
   !! \ingroup catchem_dust_process
   !!!>
   subroutine MB97_threshold_velocity(soil_density, air_density, radius, ustar_threshold)
      ! USES
      USE constants, ONLY: g0
      IMPLICIT NONE

      ! Input Parameters
      !-----------------
      real(fp), intent(in) :: radius       !< particle radius
      real(fp), intent(in) :: soil_density !< soil density
      real(fp), intent(in) :: air_density  !< air density

      ! Output Parameters
      !------------------
      real(fp), intent(out) :: ustar_threshold !< threshold friction velocity


      ! Local Variables
      !-----------------
      real(fp) :: diameter !< diameter of particle [m]

      diameter = 2.0_fp * radius
      ustar_threshold = 0.13_fp * sqrt(soil_density*g0*diameter/air_density) &
         * sqrt(1.0_fp + 6.e-7_fp/(soil_density*g0*diameter**2.5_fp)) &
         / sqrt(1.928_fp*(1331.0_fp*(100._fp*diameter)**1.56_fp+0.38_fp)**0.092_fp - 1.0_fp)

      return

   end subroutine MB97_threshold_velocity


end module CCPr_Dust_Common_Mod

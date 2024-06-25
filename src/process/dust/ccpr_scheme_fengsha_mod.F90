!>
!! \file
!! \brief Contains the FENGSHA windblown dust emission scheme
!!
!!
!! Zhang, L., Montuoro, R., McKeen, S. A., Baker, B., Bhattacharjee, P. S., Grell, G. A.,
!! Henderson, J., Pan, L., Frost, G. J., McQueen, J., Saylor, R., Li, H., Ahmadov, R.,
!! Wang, J., Stajner, I., Kondragunta, S., Zhang, X., and Li, F.:
!! Development and evaluation of the Aerosol Forecast Member in the National Center for Environment
!! Prediction (NCEP)'s Global Ensemble Forecast System (GEFS-Aerosols v1),
!! Geosci. Model Dev., 15, 5337–5369, https://doi.org/10.5194/gmd-15-5337-2022, 2022.
!!
!! \author Barry baker
!! \date 05/2024
!! \ingroup catchem_dust_process
!!!>
module CCPr_Scheme_Fengsha_Mod

   implicit none

   private

   public :: CCPr_Scheme_Fengsha

contains


   !> \brief This is the FENGSHA dust emission scheme developed at NOAA Air Resources Laboratory
   !!
   !! This is the FENGSHA dust emission scheme developed at NOAA Air Resources Laboratory.  Originally developed
   !! by Daniel Tong and revised by Barry Baker.  FENGSHA is implemented operationally at the NOAA National
   !! Weather Service in the Global Ensamble Forecast System (GEFS) version 12 and has been implemented in
   !! some fashion in the National Air Quality Forecast System Capability (NAQFC) since 2012.
   !!
   !! outputs emission flux in [ug m-2 s-1]
   !!
   !! \param nDustSpecies Number of dust species
   !! \param DSOILTYPE Soil type (1-19)
   !! \param SSM Surface soil moisture [fraction]
   !! \param RDRAG Roughness length for momentum transfer [m]
   !! \param TSKIN Skin temperature [K]
   !! \param USTAR Friction velocity [m/s]
   !! \param USTAR_THRESHOLD Friction velocity threshold [m/s]
   !! \param GWETTOP Top level volumetric soil moisture [m3/m3]
   !! \param z0 Roughness length [m]
   !! \param CLAYFRAC Fraction of clay
   !! \param SANDFRAC Fraction of sand
   !! \param AIRDEN Air density [kg/m3]
   !! \param FRLAND Land fraction
   !! \param FRLANDICE Land ice fraction
   !! \param FRSNOW Snow fraction
   !! \param alpha Alpha scaling parameter
   !! \param gamma Gamma scaling parameter
   !! \param reff Effective radius
   !! \param rlower Lower radius
   !! \param rupper Upper radius
   !! \param MoistOpt Moisture option
   !! \param DragOpt Drag option
   !! \param HorizFluxOpt Horizontal flux option
   !! \param TotalEmission Total Emission Rate [ug/m2/s]
   !! \param EmissionBin Emission Rate per Species [ug/m2/s]
   !! \param RC Return code
   !!
   !! \ingroup catchem_dust_process
   !!!>
   subroutine CCPr_Scheme_Fengsha(nDustSpecies,    &
      DSOILTYPE,        &
      SSM,              &
      RDRAG,            &
      TSKIN,            &
      USTAR,            &
      USTAR_THRESHOLD,  &
      GWETTOP,          &
      z0,               &
      CLAYFRAC,         &
      SANDFRAC,         &
      AIRDEN,           &
      FROCEAN,          &
      FRLANDICE,        &
      FRSNOW,           &
      alpha,            &
      gamma,            &
      reff,             &
      rlower,           &
      rupper,           &
      TotalEmission,    &
      EmissionBin,      &
      RC,               &
      MoistOpt,         &
      DragOpt,          &
      HorizFluxOpt)

      ! Uses
      USE Constants,     Only : g0
      Use CCPr_Dust_Common_Mod
      use precision_mod, only : fp, ZERO

      IMPLICIT NONE

      ! Arguments
      integer,  intent(in) :: nDustSpecies    !< Number of Dust Species
      integer,  intent(in) :: DSOILTYPE       !< Dominant Soil Type
      real(fp), intent(in) :: SSM             !< Sediment Supply Map (0-1)
      real(fp), intent(in) :: RDRAG           !< Drag Partition (0-1)
      real(fp), intent(in) :: TSKIN           !< Skin Temperature [K]
      real(fp), intent(in) :: USTAR           !< Friction Velocity [m/s]
      real(fp), intent(in) :: USTAR_THRESHOLD !< Friction Velocity Threshold [m/s]
      real(fp), intent(in) :: CLAYFRAC        !< Fraction of clay (0-1)
      real(fp), intent(in) :: SANDFRAC        !< Fraction of sand (0-1)
      real(fp), intent(in) :: AIRDEN          !< Air Density [kg/m3]
      real(fp), intent(in) :: alpha           !< Alpha Parameter
      real(fp), intent(in) :: gamma           !< Gamma Parameter
      real(fp), intent(in) :: FROCEAN          !< Fraction of Land
      real(fp), intent(in) :: FRLANDICE       !< Fraction of Land Ice
      real(fp), intent(in) :: FRSNOW          !< Fraction of Snow
      real(fp), intent(in) :: GWETTOP         !< Top soil wetness [m3/m3]
      real(fp), intent(in) :: z0              !< Surface roughness [m]

      real(fp), dimension(:), intent(in) :: reff            !< Effective Radius
      real(fp), dimension(:), intent(in) :: rlower          !< Lower Radius
      real(fp), dimension(:), intent(in) :: rupper         !< Upper Radius

      integer,  intent(in), optional :: MoistOpt        !< Option for moisture
      integer,  intent(in), optional :: DragOpt         !< Option for drag
      integer,  intent(in), optional :: HorizFluxOpt    !< Option for horizontal flux

      ! Outputs
      real(fp), intent(inout) :: TotalEmission  !< Total Emission
      real(fp), intent(inout) :: EmissionBin(:) !< Emission per Bin
      integer, intent(out)  :: RC             !< Return Code

      ! Local Variables
      logical :: do_dust                               !< Enable Dust Calculation Flag
      integer :: n                                     !< Bin index
      integer :: nbins                                 !< number of dust bins
      real(fp) :: hflux                                !< Horizontal Flux
      real(fp) :: R                                    !< Drag Partition [1]
      real(fp) :: h_to_v_ratio                         !< Horizontal to Vertical Mass Flux Ratio
      real(fp) :: airmass                              !< Air Mass at lowest model level
      real(fp) :: H                                    !< Soil Moisture Attenuation Factor
      real(fp) :: distribution(nDustSpecies)           !< Distribution Weights
      real(fp) :: EmissBins(nDustSpecies)              !< Emission Rate per Bin
      real(fp) :: SEP                                  !< Soil Erosion Potential
      real(fp) :: alpha_grav                           !< Alpha Parameter over Gravity
      real(fp) :: HorizFlux                            !< Horizontal Mass Flux
      real(fp) :: FengshaScaling                       !< Total Scaling Factor
      real(fp), parameter :: clay_thresh = 0.2
      real(fp), parameter :: kvhmax = 2.0e-4 !< Max. Vertical to Horizontal Mass Flux Ratio
      integer :: MoistOpt_
      integer :: DragOpt_
      integer :: HorizFluxOpt_

      ! Initialize
      RC = 0

      hflux = ZERO
      h_to_v_ratio = ZERO
      airmass = ZERO
      distribution = ZERO
      HorizFlux = ZERO
      FengshaScaling = ZERO
      SEP = ZERO
      H = ZERO
      EmissBins = ZERO
      TotalEmission = ZERO

      nbins = size(reff)

      if (present(MoistOpt) .eqv. .false.) then
         MoistOpt_ = 1
      else
         MoistOpt_ = MoistOpt
      endif
      if (present(DragOpt) .eqv. .false.) then
         DragOpt_ = 1
      else
         DragOpt_ = DragOpt
      endif
      if (present(HorizFluxOpt) .eqv. .false.) then
         HorizFluxOpt_ = 1
      else
         HorizFluxOpt_ = HorizFluxOpt
      endif

      alpha_grav = alpha / g0

      !--------------------------------------------------------------------
      ! Don't do dust over certain criteria
      !--------------------------------------------------------------------
      do_dust = .true. ! Default value for all cases

      ! Don't do dust over bedrock, lava, or Permanent Ice (15, 16, 18)
      !----------------------------------------------------------------
      if (DSOILTYPE == 15 .or. DSOILTYPE == 16 .or. DSOILTYPE == 18) then
         do_dust = .false.
      endif

      ! Check for valid inputs from SSM and RDRAG
      !------------------------------------------
      if (SSM < 0.15_fp .or. SSM > 1.0_fp) then
         do_dust = .false.
      endif

      if (RDRAG < 0.15_fp .or. RDRAG > 1.0_fp) then
         do_dust = .false.
      endif

      ! Don't do dust over frozen soil
      !--------------------------------
      if (TSKIN <= 273.15_fp) then
         do_dust = .false.
      endif

      if (do_dust) then
         !----------------------------------------------------------------
         ! Calculate Dust Flux
         !----------------------------------------------------------------

         ! Calculate soil moisture
         !--------------------------------
         if (MoistOpt_ == 1) then
            call Fecan_SoilMoisture(CLAYFRAC, SANDFRAC, GWETTOP, H)
         elseif (MoistOpt_ == 2) then
            call Shao_SoilMoisture(GWETTOP, H)
         endif

         ! get distribution of dust
         !--------------------------------
         if (nDustSpecies == 1) then
            call KokDistribution(reff, rlower, rupper, distribution)
         endif

         ! Compute vertical-to-horizontal mass flux ratio
         ! ----------------------------------------------
         if (CLAYFRAC > clay_thresh) then
            h_to_v_ratio = kvhmax
         else
            h_to_v_ratio = 10.0_fp**(13.4_fp*CLAYFRAC-6.0_fp)
         end if

         ! Compute the soil erosion potential
         !-----------------------------------
         call Soil_Erosion_Potential(CLAYFRAC, SANDFRAC, SEP)

         ! Compute the Drag Partition
         ! 1: Input Drag Partition
         ! 2: MB95 Drag Partition
         ! 3: Darmenova 2009
         !----------------------------
         if (DragOpt_ == 1) then
            R = RDRAG
         elseif (DragOpt_ == 2) then
            call MB95_DragPartition(z0, R)
         elseif (DragOpt_ == 3) then
            ! call Darmenova_DragPartition(z0, R) -> TODO: Darmenova 2009
            write(*,*) 'Place Holder'
         else
            RC = -1
            return
         endif

         ! Compute the Horizontal Mass Flux
         ! 1: Kawamura 1951 / Webb 2020
         ! 2: Draxler 2001
         !----------------------------------
         if (HorizFluxOpt_ == 1) then
            call Kawamura_HorizFlux(USTAR, USTAR_THRESHOLD, R, H, HorizFlux)
         elseif (HorizFluxOpt_ == 2) then
            call Draxler_HorizFlux(USTAR, USTAR_THRESHOLD, R, H, HorizFlux)
         else
            RC = -1
            return
         endif

         ! Compute the Total Dust Flux (ug/m2/s)
         !--------------------------------------
         FengshaScaling = (1. - FRLANDICE) * (1. - FRSNOW) * (1 - FROCEAN) * alpha_grav * (SSM ** gamma) * AIRDEN*1.0e9_fp

         TotalEmission = FengshaScaling * HorizFlux * h_to_v_ratio

         ! Compute the Dust Concentration
         !-------------------------------
         if (nDustSpecies > 0) then
            do n = 1, nDustSpecies
               EmissionBin(n) = distribution(n) * TotalEmission
            enddo
         endif

         ! Degugging print statements for testing purposes
         ! write(*,*) 'USTAR: ', USTAR
         ! write(*,*) 'USTAR_T', USTAR_THRESHOLD * H / R
         ! write(*,*) 'Total Emission Raw', FengshaScaling * HorizFlux * h_to_v_ratio
         ! write(*,*) 'Total Emission', TotalEmission
         ! write(*,*) 'FengshaScaling', FengshaScaling
         ! write(*,*) 'HorizFlux', HorizFlux
         ! write(*,*) 'h_to_v_ratio', h_to_v_ratio

         ! TODO: Add microplastic flux option https://doi.org/10.1021/acs.est.4c01252
         ! will require population density
         ! Compute Microplastic Flux due to windblown dust
         !------------------------------------------------

      endif

   end subroutine CCPr_Scheme_Fengsha

end module CCPr_Scheme_Fengsha_Mod

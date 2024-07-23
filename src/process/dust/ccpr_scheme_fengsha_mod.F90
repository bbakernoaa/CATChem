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
   !! \param MetState     Meteorological Variables
   !! \param DiagState    Diagnostic Variables
   !! \param DustState    Dust Variables
   !! \param RC           Success or Failure
   !!
   !! \ingroup catchem_dust_process
   !!!>
   subroutine CCPr_Scheme_Fengsha(MetState, DiagState, DustState, RC)

      ! Uses
      USE Constants,     Only : g0
      Use CCPr_Dust_Common_Mod
      use precision_mod, only : fp, ZERO
      Use MetState_Mod,  Only : MetStateType
      Use DiagState_Mod, Only : DiagStateType
      Use Error_Mod,     Only : CC_SUCCESS, CC_FAILURE, CC_Error
      Use CCPr_Dust_Common_Mod, Only : DustStateType

      IMPLICIT NONE

      ! Arguments
      type(MetStateType),  intent(in)    :: MetState     !< Meteorological Variables
      type(DiagStateType), intent(inout) :: DiagState    !< Diagnostic Variables
      type(DustStateType), intent(inout) :: DustState    !< Dust Variables

      integer, intent(out) :: RC                      !< Success or Failure

      ! Local Variables
      character(len=256) :: errMsg
      character(len=256) :: thisLoc
      logical :: do_dust                               !< Enable Dust Calculation Flag
      integer :: n                                     !< Bin index
      integer :: nbins                                 !< number of dust bins
      real(fp) :: hflux                                !< Horizontal Flux
      real(fp) :: R                                    !< Drag Partition [1]
      real(fp) :: h_to_v_ratio                         !< Horizontal to Vertical Mass Flux Ratio
      real(fp) :: airmass                              !< Air Mass at lowest model level
      real(fp) :: H                                    !< Soil Moisture Attenuation Factor
      real(fp) :: distribution(DustState%nDustSpecies) !< Distribution Weights
      real(fp) :: EmissBins(DustState%nDustSpecies)    !< Emission Rate per Bin
      real(fp) :: SEP                                  !< Soil Erosion Potential
      real(fp) :: alpha_grav                           !< Alpha Parameter over Gravity
      real(fp) :: HorizFlux                            !< Horizontal Mass Flux
      real(fp) :: FengshaScaling                       !< Total Scaling Factor


      real(fp), parameter :: clay_thresh = 0.2
      real(fp), parameter :: kvhmax = 2.0e-4 !< Max. Vertical to Horizontal Mass Flux Ratio

      ! Initialize
      errMsg = ''
      thisLoc = ' -> at CCPr_Scheme_Fengsha (in CCPr_Scheme_Fengsha_mod.F90)'
      RC = CC_SUCCESS

      hflux = ZERO
      h_to_v_ratio = ZERO
      airmass = ZERO
      distribution = ZERO
      HorizFlux = ZERO
      FengshaScaling = ZERO
      SEP = ZERO
      H = ZERO
      EmissBins = ZERO
      DustState%TotalEmission = ZERO

      nbins = size(DustState%EffectiveRadius)

      alpha_grav = DustState%AlphaScaleFactor / g0

      !--------------------------------------------------------------------
      ! Don't do dust over certain criteria
      !--------------------------------------------------------------------
      do_dust = .true. ! Default value for all cases

      ! Don't do dust over bedrock, lava, or Permanent Ice (15, 16, 18)
      !----------------------------------------------------------------
      if (MetState%DSOILTYPE == 15 .or. MetState%DSOILTYPE == 16 .or. MetState%DSOILTYPE == 18) then
         do_dust = .false.
      endif

      ! Check for valid inputs from SSM and RDRAG
      !------------------------------------------
      if (MetState%SSM < 0.15_fp .or. MetState%SSM > 1.0_fp) then
         do_dust = .false.
      endif

      if (MetState%RDRAG < 0.15_fp .or. MetState%RDRAG > 1.0_fp) then
         do_dust = .false.
      endif

      ! Don't do dust over frozen soil
      !--------------------------------
      if (MetState%TSKIN <= 273.15_fp) then
         do_dust = .false.
      endif

      if (do_dust) then
         !----------------------------------------------------------------
         ! Calculate Dust Flux
         !----------------------------------------------------------------

         ! Calculate soil moisture
         !--------------------------------
         if (DustState%MoistOpt == 1) then
            call Fecan_SoilMoisture(MetState%CLAYFRAC, MetState%SANDFRAC, MetState%GWETTOP, H)
         elseif (DustState%MoistOpt == 2) then
            call Shao_SoilMoisture(MetState%GWETTOP, H)
         endif

         ! get distribution of dust
         !--------------------------------
         if (DustState%nDustSpecies == 1) then
            call KokDistribution(DustState%EffectiveRadius, DustState%LowerBinRadius, DustState%UpperBinRadius, distribution)
         endif

         ! Compute vertical-to-horizontal mass flux ratio
         ! ----------------------------------------------
         if (MetState%CLAYFRAC > clay_thresh) then
            h_to_v_ratio = kvhmax
         else
            h_to_v_ratio = 10.0_fp**(13.4_fp*MetState%CLAYFRAC-6.0_fp)
         end if

         ! Compute the soil erosion potential
         !-----------------------------------
         call Soil_Erosion_Potential(MetState%CLAYFRAC, MetState%SANDFRAC, SEP)

         ! Compute the Drag Partition
         ! 1: Input Drag Partition
         ! 2: MB95 Drag Partition
         ! 3: Darmenova 2009
         !----------------------------
         if (DustState%DragOpt == 1) then
            R = MetState%RDRAG
         elseif (DustState%DragOpt == 2) then
            call MB95_DragPartition(MetState%z0, R)
         elseif (DustState%DragOpt == 3) then
            ! call Darmenova_DragPartition(MetState%z0, R) -> TODO: Darmenova 2009
            write(*,*) 'Place Holder'
         else
            RC = CC_FAILURE
            errMsg = 'Invalid Fengsha Drag Option'
            call CC_Error( errMsg, RC , thisLoc)
            return
         endif

         ! Compute the Horizontal Mass Flux
         ! 1: Kawamura 1951 / Webb 2020
         ! 2: Draxler 2001
         !----------------------------------
         if (DustState%HorizFluxOpt == 1) then
            call Kawamura_HorizFlux(MetState%USTAR, MetState%USTAR_THRESHOLD, R, H, HorizFlux)
         elseif (DustState%HorizFluxOpt == 2) then
            call Draxler_HorizFlux(MetState%USTAR, MetState%USTAR_THRESHOLD, R, H, HorizFlux)
         else
            RC = CC_FAILURE
            errMsg = 'Invalid Fengsha Horizontal Flux Option'
            call CC_Error( errMsg, RC , thisLoc)
            return
         endif

         ! Compute the Total Dust Flux (ug/m2/s)
         !--------------------------------------
         FengshaScaling = DustState%AlphaScaleFactor * (MetState%SSM ** DustState%BetaScaleFactor) * MetState%AIRDEN(1) &
            / g0 * 1.0e9_fp

         DustState%TotalEmission = FengshaScaling * HorizFlux * h_to_v_ratio

         ! Fill Diagnostic TotalFlux
         !--------------------------
         DiagState%dust_total_flux = DustState%TotalEmission

         ! Compute the Dust Concentration
         !-------------------------------
         if (DustState%nDustSpecies > 0) then
            do n = 1, DustState%nDustSpecies
               EmissBins(n) = distribution(n) * DustState%TotalEmission
            enddo
         endif

         ! TODO: Add microplastic flux option https://doi.org/10.1021/acs.est.4c01252
         ! will require population density
         ! Compute Microplastic Flux due to windblown dust
         !------------------------------------------------

      endif

   end subroutine CCPr_Scheme_Fengsha

end module CCPr_Scheme_Fengsha_Mod

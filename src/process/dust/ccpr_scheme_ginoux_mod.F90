!>
!! \file
!! \brief Contains the Ginoux windblown dust emission scheme
!!
!! Ginoux, P., “Sources and distributions of dust aerosols simulated with the GOCART model”,
!! <Journal of Geophysical Research, vol. 106, no. D17, pp. 20, 255–20, 273, 2001.
!! doi:10.1029/2000JD000053.
!!
!! \author Barry baker
!! \date 05/2024
!! \ingroup catchem_dust_process
!!!>

module CCPr_Scheme_Ginoux_Mod

   implicit none

   private

   public :: CCPr_Scheme_Ginoux

contains

   !>
   !! \brief Calculates the dust emission flux in ug m-2 s-1
   !!
   !! \param [IN] MetState The MetState object
   !! \param [INOUT] DiagState The DiagState object
   !! \param [INOUT] ChemState The ChemState object
   !! \param [INOUT] DustState The DustState object
   !! \param [OUT] RC Return code
   !!
   !! \ingroup catchem_dust_process
   !!!>
   subroutine CCPr_Scheme_Ginoux(MetState, DiagState, DustState, RC)

      ! Uses
      Use CCPr_Dust_Common_Mod
      use precision_mod, only : fp
      Use MetState_Mod,  Only : MetStateType
      Use DiagState_Mod, Only : DiagStateType
      Use Error_Mod,     Only : CC_SUCCESS, CC_FAILURE, CC_Error
      Use CCPr_Dust_Common_Mod, Only : DustStateType

      implicit none

      ! Arguments
      type(MetStateType),  intent(in)    :: MetState   !< Meteorological Variables
      type(DiagStateType), intent(inout) :: DiagState  !< Diagnostic Variables
      type(DustStateType), intent(inout) :: DustState  !< Dust Variables
      integer, intent(out) :: RC

      ! Local Variables
      character(len=256) :: errMsg
      character(len=256) :: thisLoc
      logical :: do_dust                               !< Enable Dust Calculation Flag
      integer :: n                                     !< Bin index
      integer :: nbins                                 !< number of dust bins
      real(fp) :: ginoux_scaling                       !< Ginoux scaling
      real(fp) :: u_thresh0                            !< Dry threshold wind speed [m/s]
      real(fp) :: u_thresh                             !< Moisture Corrected threshold wind speed [m/s]
      real(fp) :: w10m                                 !< 10m wind speed [m/s]
      real(fp), allocatable :: EmissionBin(:)          !< Emission Rate per Bin [ug/m2/s]

      ! Initialize
      errMsg = ''
      thisLoc = ' -> at CCPr_Scheme_Ginoux (in ccpr_scheme_ginoux_mod.F90)'
      RC = CC_FAILURE

      nbins = size(DustState%EffectiveRadius)
      ALLOCATE(EmissionBin(nbins), STAT=RC)
      if (RC /= CC_SUCCESS) then
         errMsg = 'Error Allocating EmissionBin'
         call CC_Error(errMsg, RC, thisLoc)
         return
      endif

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

      ! Don't do dust over frozen soil
      !--------------------------------
      if (MetState%TSKIN <= 273.15_fp) then
         do_dust = .false.
      endif

      if (do_dust) then

         ! get the scaling factor following Ginoux et al. (2001)
         ginoux_scaling = (1 - MetState%FROCEAN) * (1 - MetState%FRSNO) * MetState%SSM * DustState%AlphaScaleFactor

         do n = 1, nbins

            ! get threshold friction velocity following MB97
            call MB97_threshold_velocity(DustState%DustDensity(n), MetState%AIRDEN(1), DustState%EffectiveRadius(n), u_thresh0)

            ! get 10m mean wind speed
            w10m = sqrt(MetState%U10M ** 2 + MetState%V10M ** 2)

            if (MetState%GWETTOP .lt. 0.5_fp) then

               ! add the moisture correction following Ginoux et al. (2001)
               u_thresh = amax1(0., u_thresh0 * (1.2_fp + 0.2_fp*alog10(max(1.e-3_fp, MetState%GWETTOP))) )

               if (w10m .gt. u_thresh) then
                  EmissionBin(n) = ginoux_scaling * w10m ** 2 * max(0.,(w10m - u_thresh) )
                  DustState%EmissionPerSpecies(n) = EmissionBin(n)
               endif

            endif ! GWETTOP < 0.5
         enddo ! nbins

         DustState%TotalEmission = sum(DustState%EmissionPerSpecies)
         DiagState%dust_total_flux = DustState%TotalEmission

      endif ! do_dust

      RC = CC_SUCCESS
   end subroutine CCPr_Scheme_Ginoux

end module CCPr_Scheme_Ginoux_Mod

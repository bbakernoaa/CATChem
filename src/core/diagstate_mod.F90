!> \file diagstate_mod.F90
!! \brief Contains the DiagStateType and Diag_Allocate subroutine
!!
!! \ingroup core_modules
!!
!! \details This module contains subroutines and functions related to the DiagState instance of CATChem.
!! It includes subroutines for initializing of the DiagState.
!!!>
module DiagState_Mod
   ! Uses
   USE Precision_Mod
   USE Error_Mod
   USE ChemState_Mod,  only : ChemStateType

   IMPLICIT NONE
   private

   ! PUBLIC :: Zero_DiagState
   PUBLIC :: Diag_Allocate

   !> \brief Data type for storing diagnostic state variables
   !!
   !! \ingroup core_modules
   !!
   !!!>
   type, public :: DiagStateType

      ! Surface or single-level variables

      ! Dust Specific Variables
      ! -----------------------
      real(fp), allocatable :: dust_total_flux(:)             !< Total flux of dust particles [kg m-2 s-1]
      real(fp), allocatable :: dust_emission_per_bin(:,:)     !< Emission per bin [kg m-2 s-1]
      real(fp), allocatable :: dust_effective_threshold(:)    !< Effective dust threshold [m/s]
      real(fp), allocatable :: dust_soil_moisture_adj(:)      !< Adjusted soil moisture [1]
      real(fp), allocatable :: dust_horiz_flux(:)             !< Horizontal flux of dust particles [kg m-2 s-1]
      real(fp), allocatable :: dust_drag_partition(:)         !< Drag partitioning factor [1]
      real(fp), allocatable :: dust_soil_erosion_potential(:) !< Erosion potential [m]

      ! Sea Salt Specific Variables
      ! ---------------------------
      real(fp), allocatable :: ss_total_flux(:)            !< Total flux of sea salt particles [kg m-2 s-1]
      real(fp), allocatable :: ss_total_nflux(:)           !< Total number flux [n/m2/s]
      real(fp), allocatable :: ss_emission_per_bin(:,:)    !< Emission [kg m-2 s-1]
      real(fp), allocatable :: ss_nemission_per_bin(:,:)   !< number of particle emitted per bin [n/m2/s]


      ! Aerosol properties
      ! TODO: Add support for multiple aerosol types / wavelengths and more aerosol optical properties
      real(fp), allocatable :: AOD550(:)  !< Total AOD at 550nm [1]
      real(fp), allocatable :: AOD380(:)  !< Total AOD at 380nm [1]
      real(fp), allocatable :: TOMSAI(:)  !< TOMS Aerosol Index [1]

      real(fp) :: briggs_plumerise_height !< Effective plume rise height from Briggs algorithm [m]
      real(fp) :: sofiev_plumerise_height !< Effective plume rise height from Sofiev algorithm [m]

      ! Species Specific Variables


   end type DiagStateType

CONTAINS

   !> \brief Allocate memory for the diagnostic state variables
   !!
   !! This subroutine allocates memory for the diagnostic state variables.
   !!
   !! \param Config The configuration options
   !! \param GridState The grid state containing information about the grid
   !! \param DiagState The diagnostic state to be allocated
   !! \param RC The return code
   !! \ingroup core_modules
   !!!>
   subroutine Diag_Allocate(Config, MetState, ChemState, DiagState, RC)
      ! USES
      use MetState_Mod,  only : MetStateType
      USE Config_Opt_Mod, ONLY : ConfigType

      ! Arguments
      type(ConfigType),    INTENT(IN)    :: Config
      type(MetStateType),  INTENT(IN)    :: MetState
      type(DiagStateType), INTENT(INOUT) :: DiagState ! Diag State object
      type(ChemStateType), INTENT(IN)    :: ChemState

      ! OUTPUT Params
      INTEGER,             INTENT(OUT)   :: RC        ! Success or failure

      ! Local variables
      integer :: nDust
      integer :: nSS
      integer :: nHORZ

      ! Error handling
      CHARACTER(LEN=255) :: ErrMsg
      CHARACTER(LEN=255) :: thisLoc

      ! Initialize
      RC = 0
      ErrMsg = ''
      thisLoc = ' -> at Diag_Allocate (in core/diagstate_mod.F90)'

      ! Nullify all fields for safety's sake before allocating them
      ! This can prevent compilation errors caused by uninitialized values
      ! DiagState%x => NULL()

      ! If dust process is activated then allocate dust related diagnostics
      nHORZ = MetState%nHORZ
      nDust = ChemState%nDust
      nSS = ChemState%nSeaSalt

      if (Config%dust_activate) then
         if (nDust .eq. 0) nDust = 5
         print*, 'Number of dust bins: ', nDust, ChemState%nDust
         if (.not. allocated(DiagState%dust_total_flux)) then
            allocate(DiagState%dust_total_flux(nHORZ), stat=RC)
            if (RC /= CC_SUCCESS) then
               ErrMsg = 'Error allocating DiagState%dust_total_flux'
               CALL CC_Error( ErrMsg, RC, thisLoc )
            endif
            DiagState%dust_total_flux = ZERO
         endif

         if (.not. allocated(DiagState%dust_emission_per_bin)) then
            allocate(DiagState%dust_emission_per_bin(nHORZ, nDust), stat=RC)
            if (RC /= CC_SUCCESS) then
               ErrMsg = 'Error allocating DiagState%dust_emission_per_bin'
               CALL CC_Error( ErrMsg, RC, thisLoc )
            endif
            DiagState%dust_emission_per_bin(:,:) = ZERO
         endif

         if (.not. allocated(DiagState%dust_effective_threshold)) then
            allocate(DiagState%dust_effective_threshold(nHORZ), stat=RC)
            if (RC /= CC_SUCCESS) then
               ErrMsg = 'Error allocating DiagState%dust_effective_threshold'
               CALL CC_Error( ErrMsg, RC, thisLoc )
            endif
            DiagState%dust_effective_threshold = ZERO
         endif

         if (.not. allocated(DiagState%dust_soil_moisture_adj)) then
            allocate(DiagState%dust_soil_moisture_adj(nHORZ), stat=RC)
            if (RC /= CC_SUCCESS) then
               ErrMsg = 'Error allocating DiagState%dust_soil_moisture_adj'
               CALL CC_Error( ErrMsg, RC, thisLoc )
            endif
            DiagState%dust_soil_moisture_adj = ZERO
         endif

         if (.not. allocated(DiagState%dust_horiz_flux)) then
            allocate(DiagState%dust_horiz_flux(nHORZ), stat=RC)
            if (RC /= CC_SUCCESS) then
               ErrMsg = 'Error allocating DiagState%dust_horiz_flux'
               CALL CC_Error( ErrMsg, RC, thisLoc )
            endif
            DiagState%dust_horiz_flux = ZERO
         endif

         if (.not. allocated(DiagState%dust_drag_partition)) then
            allocate(DiagState%dust_drag_partition(nHORZ), stat=RC)
            if (RC /= CC_SUCCESS) then
               ErrMsg = 'Error allocating DiagState%dust_drag_partition'
               CALL CC_Error( ErrMsg, RC, thisLoc )
            endif
            DiagState%dust_drag_partition = ZERO
         endif

         if (.not. allocated(DiagState%dust_soil_erosion_potential)) then
            allocate(DiagState%dust_soil_erosion_potential(nHORZ), stat=RC)
            if (RC /= CC_SUCCESS) then
               ErrMsg = 'Error allocating DiagState%dust_soil_erosion_potential'
               CALL CC_Error( ErrMsg, RC, thisLoc )
            endif
            DiagState%dust_soil_erosion_potential = ZERO
         endif

      endif

      ! If sea salt process is activated then allocate sea salt related diagnostics
      if (Config%seasalt_activate) then

         if (nSS .eq. 0) nSS = 5
         print*, 'Number of sea salt bins: ', nSS, ChemState%nSeaSalt
         if (.not. allocated(DiagState%ss_total_flux)) then
            allocate(DiagState%ss_total_flux(nHORZ), stat=RC)
            if (RC /= CC_SUCCESS) then
               ErrMsg = 'Error allocating DiagState%ss_total_flux'
               CALL CC_Error( ErrMsg, RC, thisLoc )
            endif
            DiagState%ss_total_flux = ZERO
         endif

         if (.not. allocated(DiagState%ss_nemission_per_bin)) then
            allocate(DiagState%ss_nemission_per_bin(nHORZ, nSS), stat=RC)
            if (RC /= CC_SUCCESS) then
               ErrMsg = 'Error allocating DiagState%ss_nemission_per_bin'
               CALL CC_Error( ErrMsg, RC, thisLoc )
            endif
            DiagState%ss_nemission_per_bin = ZERO
         endif

         if (.not. allocated(DiagState%ss_emission_per_bin)) then
            allocate(DiagState%ss_emission_per_bin(nHORZ, nSS), stat=RC)
            if (RC /= CC_SUCCESS) then
               ErrMsg = 'Error allocating DiagState%ss_emission_per_bin'
               CALL CC_Error( ErrMsg, RC, thisLoc )
            endif
            DiagState%ss_emission_per_bin = ZERO
         endif

         if (.not. allocated(DiagState%ss_total_nflux)) then
            allocate(DiagState%ss_total_nflux(nHORZ), stat=RC)
            if (RC /= CC_SUCCESS) then
               ErrMsg = 'Error allocating DiagState%ss_total_nflux'
               CALL CC_Error( ErrMsg, RC, thisLoc )
            endif
            DiagState%ss_total_nflux = ZERO
         endif
      endif

   end subroutine Diag_Allocate

end module DiagState_Mod

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
      REAL(fp) :: dust_total_flux      !< Total flux of dust particles [kg m-2 s-1]
      REAL(fp) :: sea_salt_total_flux  !< Total flux of sea salt particles [kg m-2 s-1]

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
   subroutine Diag_Allocate(Config, DiagState, RC)
      ! USES
      USE GridState_Mod, ONLY : GridStateType
      USE Config_Opt_Mod, ONLY : ConfigType

      ! Arguments
      type(ConfigType),    INTENT(IN)    :: Config
      ! type(GridStateType), INTENT(IN)    :: GridState ! Grid State object
      type(DiagStateType), INTENT(INOUT) :: DiagState ! Diag State object
      ! OUTPUT Params
      INTEGER,             INTENT(OUT)   :: RC        ! Success or failure

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
      if (Config%dust_activate) then
         DiagState%dust_total_flux = ZERO
      endif

      ! If sea salt process is activated then allocate sea salt related diagnostics
      if (Config%seasalt_activate) then
         DiagState%sea_salt_total_flux = ZERO
      endif

   end subroutine Diag_Allocate

end module DiagState_Mod

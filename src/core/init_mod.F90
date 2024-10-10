!> \file init_mod.F90
!! \brief Initialization module for the program.
!!
!! This module contains subroutines and functions related to the initialization of the program.
!! It includes subroutines for initializing the grid, the time step, and the solution.
!!
!! \ingroup core_modules
!!!>
module init_mod

   implicit none

   PUBLIC :: Init_Met
   PUBLIC :: Init_Diag
   PUBLIC :: Init_Chem
   PUBLIC :: Init_Emis

contains

   !> \brief Initialize the met state
   !!
   !! This subroutine allocates the met state.
   !!
   !! \param GridState The grid state containing information about the grid.
   !! \param MetState The met state to be initialized.
   !! \param RC The return code.
   !!
   !! \ingroup core_modules
   !!!>
   subroutine Init_Met(MetState, RC)
      !Uses
      use MetState_Mod
      USE Error_Mod
      implicit none

      ! Arguments
      TYPE(MetStateType), INTENT(INOUT) :: MetState
      INTEGER,        INTENT(OUT) :: RC

      ! Local variables
      CHARACTER(LEN=255) :: ErrMsg, thisLoc

      ! Initialize
      RC = 0
      ErrMsg = ''
      thisLoc = ' -> at Init_Met (in core/state_mod.F90)'

      call Met_Allocate(MetState, RC)
      if (RC /= CC_SUCCESS) then
         errMsg = 'Error allocating met state'
         call CC_Error(errMsg, RC , thisLoc)
      endif

   end subroutine Init_Met

   !> \brief Initialize the diag state
   !!
   !! This subroutine allocates the diag state.
   !!
   !! \param Config_Opt The config.
   !! \param GridState The grid state containing information about the grid.
   !! \param DiagState The diag state to be initialized.
   !! \param RC The return code.
   !!
   !! \ingroup core_modules
   !!!>
   subroutine Init_Diag(Config, MetState, DiagState, ChemState, RC)
      use DiagState_Mod
      use Config_Opt_Mod, Only : ConfigType
      use MetState_Mod, Only : MetStateType
      use ChemState_Mod, Only : ChemStateType
      use Error_Mod

      implicit none

      ! Arguments
      TYPE(ConfigType),    INTENT(IN)    :: Config
      TYPE(MetStateType),  INTENT(IN)    :: MetState
      TYPE(DiagStateType), INTENT(INOUT) :: DiagState
      TYPE(ChemStateType), INTENT(IN)    :: ChemState
      INTEGER,             INTENT(OUT)   :: RC

      ! Local variables
      CHARACTER(LEN=255) :: ErrMsg, thisLoc

      ! Initialize
      RC = CC_SUCCESS
      ErrMsg = ''
      thisLoc = ' -> at Init_Diag (in core/init_mod.F90)'

      call Diag_Allocate(Config, MetState, ChemState, DiagState, RC)
      if (RC /= CC_SUCCESS) then
         errMsg = 'Error allocating diag state'
         call CC_Error(errMsg, RC , thisLoc)
      endif

   end subroutine Init_Diag

   !> \brief Initialize the Chem state
   !!
   !! This subroutine allocates the Chem state.
   !!
   !! \param Config_Opt The config.
   !! \param MetState The grid state containing information about the grid.
   !! \param ChemState The Chem state to be initialized.
   !! \param RC The return code.
   !!
   !! \ingroup core_modules
   !!!>
   subroutine Init_Chem(Config, MetState, ChemState, RC)
      use ChemState_Mod
      use Config_Opt_Mod, Only : ConfigType
      use MetState_Mod, Only : MetStateType
      use Error_Mod

      implicit none

      ! Arguments
      TYPE(ConfigType),    INTENT(IN)    :: Config
      TYPE(MetStateType),  INTENT(IN)    :: MetState
      TYPE(ChemStateType), INTENT(INOUT) :: ChemState

      INTEGER, INTENT(OUT) :: RC

      ! Local variables
      CHARACTER(LEN=255) :: ErrMsg, thisLoc

      ! Initialize
      RC = CC_SUCCESS
      ErrMsg = ''
      thisLoc = ' -> at Init_Diag (in core/init_mod.F90)'

      ! call Diag_Allocate(Config, MetState, DiagState, RC)
      ! if (RC /= CC_SUCCESS) then
      !    errMsg = 'Error allocating diag state'
      !    call CC_Error(errMsg, RC , thisLoc)
      ! endif

   end subroutine Init_Chem

   !> \brief Initialize the Chem state
   !!
   !! This subroutine allocates the Chem state.
   !!
   !! \param Config_Opt The config.
   !! \param MetState The grid state containing information about the grid.
   !! \param ChemState The Chem state to be initialized.
   !! \param RC The return code.
   !!
   !! \ingroup core_modules
   !!!>
   subroutine Init_Emis(Config, MetState, EmisState, RC)
      use EmisState_Mod
      use Config_Opt_Mod, Only : ConfigType
      use MetState_Mod, Only : MetStateType
      use Error_Mod

      implicit none

      ! Arguments
      TYPE(ConfigType),    INTENT(IN)    :: Config
      TYPE(MetStateType),  INTENT(IN)    :: MetState
      TYPE(EmisStateType), INTENT(INOUT) :: EmisState

      INTEGER, INTENT(OUT) :: RC

      ! Local variables
      CHARACTER(LEN=255) :: ErrMsg, thisLoc

      ! Initialize
      RC = CC_SUCCESS
      ErrMsg = ''
      thisLoc = ' -> at Init_Diag (in core/init_mod.F90)'

      ! call Diag_Allocate(Config, MetState, DiagState, RC)
      ! if (RC /= CC_SUCCESS) then
      !    errMsg = 'Error allocating diag state'
      !    call CC_Error(errMsg, RC , thisLoc)
      ! endif

   end subroutine Init_Emis

end module init_mod

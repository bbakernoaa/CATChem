!> \file gridstate_mod.F90
!!
!! \brief Module for grid state variables
!!
!! This module contains subroutines and functions related to the grid state.
!!
!!!>
module GridState_Mod

   USE Error_Mod
   USE precision_mod

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: Grid_Init_State
   PUBLIC :: Grid_State_Allocate
   PUBLIC :: Grid_Cleanup
   type, public :: GridStateType

      ! Integers
      integer :: nx
      integer :: ny
      integer :: number_of_levels

      ! Reals
      real(fp), POINTER :: area(:,:)

      !-------------------------------------------------------------------------
      ! Name of variables containing grid information
      !-------------------------------------------------------------------------
      CHARACTER(LEN=4)             :: State     = 'Grid'    ! Name of this state
   end type GridStateType


contains


   !> \brief Initialize a GridState object
   !!
   !! This subroutine initializes a GridState object.
   !!
   !! \param Config The input config object.
   !! \param GridState The GridState object to be initialized.
   !! \param RC The return code
   !!
   !!!>
   subroutine Grid_Init_State(Config, GridState, RC)
      use Error_Mod, only : CC_SUCCESS

      use Config_Opt_Mod, Only : ConfigType

      implicit none

      ! Input Params
      type(ConfigType),    intent(in)    :: Config ! Input Options object

      ! INOUT Params
      type(GridStateType), intent(inout) :: GridState ! Grid State object


      ! OUTPUT Params
      INTEGER,         INTENT(OUT)   :: RC          ! Success or failure

      CHARACTER(LEN=512) :: errMsg
      CHARACTER(LEN=255) :: thisLoc

      ! set error handling defaults
      RC = 0
      errMsg = ''
      thisLoc = 'Grid_Init_State() -> at initializing GridState'

      ! initialize GridState
      GridState%nx=0
      GridState%ny=0
      GridState%number_of_levels=0
      GridState%area => NULL()

   end subroutine Grid_Init_State


   !> @brief Initialize a GridState
   !!
   !! This subroutine initializes a GridState object.
   !!
   !! @param Config The input config object.
   !! @param GridState The GridState object to be initialized.
   !! @param RC The return code.
   !!
   !!!>
   subroutine Grid_Init(Config, GridState, RC)

      USE Config_Opt_Mod, ONLY : ConfigType

      implicit none

      type(ConfigType),    intent(in)    :: Config ! Input Options object
      type(GridStateType), intent(inout) :: GridState ! Grid State object


      INTEGER,         INTENT(OUT)   :: RC          ! Success or failure

      CHARACTER(LEN=512) :: errMsg
      CHARACTER(LEN=512) :: thisLoc

      RC=0

      ! allocate GridState
      ! ------------------

      ! start with integers
      GridState%nx = 0
      GridState%ny = 0
      GridState%number_of_levels = 0

      ! nullify arrays
      GridState%area   => NULL()

   end subroutine Grid_Init


   !> \brief Allocate a GridState object
   !!
   !! This subroutine allocates memory for GridState.
   !!
   !! \param [inout] GridState The GridState instance to be allocated.
   !! \param [out] RC The return code.
   !!
   !!!>
   subroutine Grid_State_Allocate(GridState, RC) !Config, GridState, RC)

      use Error_Mod
      ! use Config_Mod, only : ConfigType

      implicit none

      ! input Params
      ! type(ConfigType),  intent(in)    :: Config ! Input Options object

      ! INOUT Params
      type(GridStateType), intent(inout) :: GridState ! Grid State object

      ! OUTPUT Params
      INTEGER,         INTENT(OUT)   :: RC          !Success or failure

      CHARACTER(LEN=512) :: errMsg
      CHARACTER(LEN=512) :: thisLoc

      ! set error handling defaults
      thisLoc = 'Grid_Allocate() -> at allocating GridState'
      errMsg = ''
      RC = 0

      ALLOCATE( GridState%Area( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'GridState%Area', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      GridState%Area = 0e+0_fp

   end subroutine Grid_State_Allocate

   !> \brief Deallocate a GridState object
   !!
   !! This subroutine deallocates memory for GridState.
   !!
   !! \param [inout] GridState The GridState instance to be deallocated.
   !! \param [out] RC The return code.
   !!
   !!!>
   subroutine Grid_Cleanup(GridState, RC)

      ! INOUT Params
      type(GridStateType), intent(inout) :: GridState ! Grid State object

      ! OUTPUT Params
      INTEGER,         INTENT(OUT)   :: RC          !Success or failure

      CHARACTER(LEN=512) :: errMsg
      CHARACTER(LEN=512) :: thisLoc

      ! set error handling defaults
      thisLoc = 'Grid_Cleanup() -> at deallocating GridState'
      errMsg = ''
      RC = 0

      DEALLOCATE( GridState%Area, STAT=RC )
      CALL CC_CheckVar( 'GridState%Area', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      GridState%Area => NULL()

   end subroutine Grid_Cleanup

end module GridState_Mod

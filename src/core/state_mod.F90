!> \file state_mod.F90
!!
!! \brief This file contains the module for CATChem state
!!
!! \defgroup core_modules
!! \ingroup core_modules
!!
!!!>
module state_mod
   use precision_mod
   use Config_Opt_Mod, only : ConfigType
   use GridState_Mod,  only : GridStateType
   use MetState_Mod,   only : MetStateType
   use ChemState_Mod,  only : ChemStateType
   use EmisState_Mod,  only : EmisStateType
   use DiagState_Mod,  only : DiagStateType

   IMPLICIT NONE

   ! PUBLIC
   type(GridStateType), PUBLIC :: GridState
   type(MetStateType),  PUBLIC :: MetState
   type(ChemStateType), PUBLIC :: ChemState
   type(ConfigType),    PUBLIC :: Config
   type(EmisStateType), PUBLIC :: EmisState
   type(DiagStateType), PUBLIC :: DiagState

end module state_mod

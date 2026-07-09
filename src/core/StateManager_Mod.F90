!> \file StateManager_Mod.F90
!! \brief Lightweight backward-compatible Fortran wrapper for StateManager pointer associations
!!
module StateManager_Mod
   use Precision_Mod, only: fp
   use Error_Mod, only: CC_SUCCESS, CC_FAILURE, ErrorManagerType
   use MetState_Mod, only: MetStateType
   use ChemState_Mod, only: ChemStateType
   use ConfigManager_Mod, only: ConfigManagerType
   use TimeState_Mod, only: TimeStateType
   use iso_c_binding, only: c_ptr, c_null_ptr

   implicit none
   private

   public :: StateManagerType

   type :: StateManagerType
      type(c_ptr) :: cpp_ptr = c_null_ptr
      real(fp) :: tstep = 0.0_fp
      type(MetStateType), pointer :: met_state => null()
      type(ChemStateType), pointer :: chem_state => null()
      type(ConfigManagerType), pointer :: config_mgr => null()
      type(ErrorManagerType), pointer :: error_mgr => null()
      type(TimeStateType), pointer :: time_state => null()
   contains
      procedure :: get_met_state_ptr => state_mgr_get_met_state_ptr
      procedure :: get_chem_state_ptr => state_mgr_get_chem_state_ptr
      procedure :: get_config_ptr => state_mgr_get_config_ptr
      procedure :: get_error_manager => state_mgr_get_error_mgr
      procedure :: get_time_state_ptr => state_mgr_get_time_state_ptr
   end type StateManagerType

contains

   function state_mgr_get_met_state_ptr(this) result(ptr)
      class(StateManagerType), intent(in) :: this
      type(MetStateType), pointer :: ptr
      ptr => this%met_state
   end function state_mgr_get_met_state_ptr

   function state_mgr_get_chem_state_ptr(this) result(ptr)
      class(StateManagerType), intent(in) :: this
      type(ChemStateType), pointer :: ptr
      ptr => this%chem_state
   end function state_mgr_get_chem_state_ptr

   function state_mgr_get_config_ptr(this) result(ptr)
      class(StateManagerType), intent(in) :: this
      type(ConfigManagerType), pointer :: ptr
      ptr => this%config_mgr
   end function state_mgr_get_config_ptr

   function state_mgr_get_error_mgr(this) result(ptr)
      class(StateManagerType), intent(in) :: this
      type(ErrorManagerType), pointer :: ptr
      ptr => this%error_mgr
   end function state_mgr_get_error_mgr

   function state_mgr_get_time_state_ptr(this) result(ptr)
      class(StateManagerType), intent(in) :: this
      type(TimeStateType), pointer :: ptr
      ptr => this%time_state
   end function state_mgr_get_time_state_ptr

end module StateManager_Mod

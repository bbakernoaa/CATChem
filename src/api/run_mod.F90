!> \file run_mod.F90
!! \brief Run module for CATChem atmospheric chemistry processes
!!
!! This module contains the main process execution routines for CATChem,
!! including initialization, run, and finalization of all atmospheric
!! chemistry processes such as emissions, dry deposition, and other
!! chemical transformations.
!!
!! \ingroup core_modules
module run_mod
   USE MetState_Mod, ONLY : MetStateType
   USE ChemState_Mod, ONLY : ChemStateType
   USE DiagState_Mod, ONLY : DiagStateType
   USE EmisState_Mod, ONLY : EmisStateType
   use Config_Opt_Mod, only: ConfigType
   USE CCPr_Dust_Common_Mod, ONLY : DustStateType
   USE CCPr_SeaSalt_Common_Mod, ONLY : SeaSaltStateType
   USE CCPR_DryDep_Mod, ONLY : DryDepStateType
   USE Error_Mod

   implicit none

   PRIVATE

   PUBLIC :: Run_Process
   PUBLIC :: Finalize_Process
   PUBLIC :: Init_Process

contains


   !> Run all CATChem atmospheric chemistry processes
   !!
   !! This subroutine executes all enabled atmospheric chemistry processes
   !! including emissions, dry deposition, and other chemical processes
   !! for a single time step.
   !!
   !! @param MetState The meteorological state containing atmospheric conditions
   !! @param DiagState The diagnostic state for storing process outputs
   !! @param ChemState The chemical state containing species concentrations
   !! @param EmisState The emission state containing emission data
   !! @param DustState The dust process state
   !! @param SeaSaltState The sea salt process state
   !! @param DryDepState The dry deposition process state
   !! @param RC The return code indicating success (CC_SUCCESS) or failure
   !!
   !! The processes are executed in the following order:
   !! 1. Emission processes (dust, sea salt, etc.)
   !! 2. Dry deposition calculations
   !! 3. Wet deposition calculations (planned)
   !!
   !! @note All processes update the ChemState and DiagState objects
   !! @warning RC should be checked after calling this routine
   subroutine Run_Process(MetState, DiagState, ChemState, EmisState, DustState, SeaSaltState, DryDepState, RC)
      !
      use CCPR_DryDep_Mod, Only : CCPR_DryDep_Run

      implicit none

      ! Arguments
      type(MetStateType), INTENT(IN) :: MetState
      type(DiagStateType), INTENT(INOUT) :: DiagState
      type(ChemStateType), INTENT(INOUT) :: ChemState
      TYPE(EmisStateType), INTENT(INOUT) :: EmisState
      type(DustStateType), INTENT(INOUT) :: DustState
      type(SeaSaltStateType), INTENT(INOUT) :: SeaSaltState
      type(DryDepStateType), INTENT(INOUT) :: DryDepState
      INTEGER,        INTENT(OUT) :: RC

      ! Local variables
      CHARACTER(LEN=255) :: ErrMsg, thisLoc

      ! Initialize
      RC = 0
      ErrMsg = ''
      thisLoc = ' -> at Run_Process (in core/run_mod.F90)'

      !run all the emission processes
      call Run_Emis(MetState, DiagState, ChemState, EmisState, DustState, SeaSaltState, RC)
      if (RC /= CC_SUCCESS) then
         errMsg = 'Error running emission processes.'
         call CC_Error(errMsg, RC , thisLoc)
      endif

      !run dry deposition
      call CCPr_DryDep_Run( MetState, DiagState, DryDepState, ChemState, RC )
      if (RC /= CC_SUCCESS) then
         errMsg = 'Error running dry deposition.'
         call CC_Error(errMsg, RC , thisLoc)
      endif

      !run wet deposition

   end subroutine Run_Process


   !> Run atmospheric emission processes
   !!
   !! This subroutine executes emission processes including dust, sea salt,
   !! and other aerosol emissions, then applies them to the chemical state.
   !!
   !! @param MetState The meteorological state containing atmospheric conditions
   !! @param DiagState The diagnostic state for storing process outputs
   !! @param ChemState The chemical state containing species concentrations
   !! @param EmisState The emission state containing emission data
   !! @param DustState The dust process state
   !! @param SeaSaltState The sea salt process state
   !! @param RC The return code indicating success (CC_SUCCESS) or failure
   !!
   !! The emission processes include:
   !! - Dust emission calculations using Ginux scheme with 5 size bins
   !! - Sea salt emission calculations based on wind speed
   !! - Application of all emissions to the chemistry state
   !!
   !! @note Dust emissions currently work with default 5 bins regardless of namelist
   !! @warning RC should be checked after calling this routine
   subroutine Run_Emis(MetState, DiagState, ChemState, EmisState, DustState, SeaSaltState, RC)
      use CCPr_Dust_mod, ONLY : CCPR_Dust_Run
      use CCPr_SeaSalt_mod, ONLY : CCPR_SeaSalt_Run
      use EmisState_Mod, only: Apply_Emis_to_Chem
      implicit none
      ! Arguments
      type(ChemStateType), intent(inout) :: ChemState
      type(DiagStateType), intent(inout) :: DiagState
      type(MetStateType), intent(in)  :: MetState
      type(EmisStateType), intent(inout) :: EmisState
      type(DustStateType), intent(inout) :: DustState
      type(SeaSaltStateType), intent(inout) :: SeaSaltState
      integer, intent(inout) :: RC

      !local variables
      !integer :: i, k

      ! Error handling
      CHARACTER(LEN=255)    :: ErrMsg
      CHARACTER(LEN=255)    :: ThisLoc
      ThisLoc = ' -> at Run_Emis (in core/run_mo.F90)'


      !TODO: make sure each emission process has filled the EmisState flux since cc_apply_emis_to_chem is based on the EmisState
      ! Call dust emission
      !TODO: dust per bin emission only works on the default 5 biins of the Ginux scheme despite what defined in the emission namelist
      call CCPR_Dust_Run(MetState, DiagState, DustState, EmisState, RC)
      if (RC /= CC_SUCCESS) then
         errMsg = 'Error running dust emissions'
         call CC_Error(errMsg, RC , thisLoc)
         return
      end if

      ! Call sea salt emission
      call CCPR_SeaSalt_Run(MetState, SeaSaltState, EmisState, RC)
      if (RC /= CC_SUCCESS) then
         errMsg = 'Error running seal salt emissions'
         call CC_Error(errMsg, RC , thisLoc)
         return
      end if

      !apply emissions to chemistry state
      call Apply_Emis_to_Chem(EmisState, MetState, ChemState, RC)
      if (RC /= CC_SUCCESS) then
         errMsg = 'Error applying emissions to chemistry state'
         call CC_Error(errMsg, RC , thisLoc)
         return
      end if


   end subroutine Run_Emis


   !> Finalize all CATChem atmospheric chemistry processes
   !!
   !! This subroutine performs cleanup and finalization for all
   !! atmospheric chemistry processes, deallocating memory and
   !! closing any open resources.
   !!
   !! @param DustState The dust process state to finalize
   !! @param SeaSaltState The sea salt process state to finalize
   !! @param DryDepState The dry deposition process state to finalize
   !! @param RC The return code indicating success (CC_SUCCESS) or failure
   !!
   !! The finalization includes:
   !! - Cleanup of dust emission process state
   !! - Cleanup of sea salt emission process state
   !! - Cleanup of dry deposition process state
   !! - Cleanup of wet deposition process state (planned)
   !!
   !! @note This routine should be called at the end of simulation
   !! @warning RC should be checked after calling this routine
   subroutine Finalize_Process(DustState, SeaSaltState, DryDepState, RC)
      !
      use CCPR_Dust_Mod, Only : CCPr_Dust_Finalize
      use CCPR_SeaSalt_Mod, Only : CCPr_SeaSalt_Finalize
      use CCPR_DryDep_Mod, Only : CCPr_DryDep_Finalize

      implicit none

      ! Arguments
      type(DustStateType), INTENT(INOUT) :: DustState
      type(SeaSaltStateType), INTENT(INOUT) :: SeaSaltState
      type(DryDepStateType), INTENT(INOUT) :: DryDepState
      INTEGER,        INTENT(OUT) :: RC

      ! Local variables
      CHARACTER(LEN=255) :: ErrMsg, thisLoc

      ! Initialize
      RC = 0
      ErrMsg = ''
      thisLoc = ' -> at Finalize_Process (in core/run_mod.F90)'

      !finalize all the emission processes
      call CCPr_Dust_Finalize(DustState, RC)
      if (RC /= CC_SUCCESS) then
         errMsg = 'Error finalizing dust emissions.'
         call CC_Error(errMsg, RC , thisLoc)
      endif
      call CCPr_SeaSalt_Finalize(SeaSaltState, RC)
      if (RC /= CC_SUCCESS) then
         errMsg = 'Error finalizing sea salt emissions.'
         call CC_Error(errMsg, RC , thisLoc)
      endif

      !finalize dry deposition
      call CCPr_DryDep_Finalize(DryDepState, RC)
      if (RC /= CC_SUCCESS) then
         errMsg = 'Error finalizing dry deposition.'
         call CC_Error(errMsg, RC , thisLoc)
      endif

      !finalize wet deposition

   end subroutine Finalize_Process

   !> Initialize all CATChem atmospheric chemistry processes
   !!
   !! This subroutine performs initialization of all atmospheric chemistry
   !! processes including dust emission, sea salt emission, and dry deposition.
   !! It must be called before any process execution routines.
   !!
   !! @param config The CATChem configuration object containing setup parameters
   !! @param ChemState The chemical state to be initialized with species data
   !! @param EmisState The emission state to be initialized for emission processes
   !! @param DustState The dust process state to be initialized
   !! @param SeaSaltState The sea salt process state to be initialized
   !! @param DryDepState The dry deposition process state to be initialized
   !! @param RC The return code indicating success (CC_SUCCESS) or failure
   !!
   !! The initialization includes:
   !! - Setting up dust emission process parameters and lookup tables
   !! - Configuring sea salt emission parameterizations
   !! - Preparing dry deposition velocity calculations
   !! - Validating process configuration consistency
   !!
   !! @note This routine must be called after state allocation but before process execution
   !! @warning RC should be checked after calling this routine
   subroutine Init_Process (config, ChemState, EmisState, DustState, SeaSaltState, DryDepState, RC)
      use CCPr_Dust_mod, only: CCPr_Dust_Init
      use CCPr_SeaSalt_mod, only: CCPr_SeaSalt_Init
      use CCPr_DryDep_mod, only: CCPr_DryDep_Init
      implicit none

      type(ConfigType), intent(in) :: config
      type(ChemStateType), intent(inout) :: ChemState
      type(EmisStateType), intent(inout) :: EmisState
      type(DustStateType), intent(inout) :: DustState
      type(SeaSaltStateType), intent(inout) :: SeaSaltState
      type(DryDepStateType), intent(inout) :: DryDepState
      integer, intent(inout) :: RC

      ! Error handling
      !---------------
      CHARACTER(LEN=255)    :: ErrMsg
      CHARACTER(LEN=255)    :: ThisLoc
      ThisLoc = ' -> at init_process (in core/init_mod.F90)'

      ! Initialize DustState
      call CCPR_Dust_Init(config, DustState, ChemState, EmisState, RC)
      if (RC /= CC_SUCCESS) then
         ErrMsg = 'Error in CCPR_Dust_Init'
         call CC_Error(ErrMsg, RC, ThisLoc )
         return
      end if

      ! Initialize SealSaltState
      call CCPr_SeaSalt_Init(config, SeaSaltState, ChemState, EmisState, RC)
      if (RC /= CC_SUCCESS) then
         ErrMsg = 'Error in CCPr_SeaSalt_Init'
         call CC_Error(ErrMsg, RC, ThisLoc )
         return
      end if

      ! Initialize DryDepState
      call CCPr_DryDep_Init(config, DryDepState, ChemState, RC)
      if (RC /= CC_SUCCESS) then
         ErrMsg = 'Error in CCPr_DryDep_Init'
         call CC_Error(ErrMsg, RC, ThisLoc )
         return
      end if

   end subroutine Init_Process

end module run_mod

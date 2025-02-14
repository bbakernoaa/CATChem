!> \brief CCPR Aerosol Diagnostics state types
!!
!!
!!
!! \author Lacey Holland
!! \date 01/2025
!!!>

MODULE CCPR_Aero_Diags_mod

   USE Precision_mod
   USE Error_Mod
   USE DiagState_Mod, Only : DiagStateType
   USE MetState_Mod,  Only : MetStateType
   USE ChemState_Mod, Only : ChemStateType
   USE Config_Opt_Mod, Only : ConfigType
   USE Constants

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: CCPR_Aerosol_Diags_Init
   PUBLIC :: CCPR_Aerosol_Diags_Run
   PUBLIC :: CCPR_Aerosol_Diags_Finalize
   PUBLIC :: AeroDiagsStateType

   !!
   !!
   !! \param Activate Activate Aerosol Diagnostics (True/False)
   !!!>

   TYPE :: AeroDiagsStateType
      LOGICAL                         :: Activate              ! Activate Process (True/False)
      INTEGER                         :: SchemeOpt             ! Diagnostics Scheme (True/False)

      ! Process Specific Parameters

      ! Namelist parameters for specific Aerosol Diagnostics goes here as well
      !=================================================================
      ! Module specific variables/arrays/data pointers come below
      !=================================================================

   END TYPE AeroDiagsStateType

CONTAINS

   !>
   !! \brief Initialize the CATChem Aerosol Diagnostics module
   !!
   !! \param Config       CATCHem configuration options
   !! \param AeroDiagState   CATCHem PROCESS state
   !! \param ChemState         CATCHem chemical state
   !! \param RC               Error return code
   !!
   !!!>


   SUBROUTINE CCPR_Aerosol_Diags_Init( Config, AeroDiagsState, RC )
      ! USE


      IMPLICIT NONE
      ! INPUT PARAMETERS
      !-----------------
      TYPE(ConfigType), POINTER       :: Config    ! Module options
      TYPE(AeroDiagConfigType), POINTER       :: AeroDiagConfig    ! Module options

      ! INPUT/OUTPUT PARAMETERS
      !------------------------
      TYPE(AeroDiagStateType), POINTER  :: AeroDiagsState ! Aerosol Diagnostics state
      INTEGER,         INTENT(INOUT)    :: RC       ! Success or failure

      ! Error handling
      !---------------
      CHARACTER(LEN=255)    :: ErrMsg
      CHARACTER(LEN=255)    :: ThisLoc

      ! LOCAL VARIABLES
      !----------------


      ! Put any local variables here

      !=================================================================
      ! CCPR_Aerosol_Diags_Init begins here!
      !=================================================================
      ErrMsg = ''
      ThisLoc = ' -> at CCPR_Aero_Diags_INIT (in process/diagnostics/ccpr_aero_diags_mod.F90)'

      ! First check if process is activated in config | if not don't allocate arrays or pointers
      if (Config%AeroDiags_activate) then

         ! Activate Process
         !------------------
         AeroDiagsState%Activate = .true.

         ! Set scheme option
         !------------------
         AeroDiagsState%SchemeOpt = Config%AeroDiags_Scheme

         ! Add read DiagState config file
         if (AeroDiagConfig%aerosol_diagnostics) then
            AeroDiagsState%aerosol_diagnostics    = 
         end if

         AeroDiagsState%mass_diagnostics
         = AeroDiagConfig%mass_diagnostics
         AeroDiagsState%optical_diagnostics
         = AeroDiagConfig%optical_diagnostics
         AeroDiagsState%sfcmass
         = AeroDiagConfig%SfcMass
         AeroDiagsState%colmass
         = AeroDiagConfig%ColMass
         AeroDiagsState%mass
         = AeroDiagConfig%Mass
         AeroDiagsState%conc
         = AeroDiagConfig%Conc
         AeroDiagsState%extinction_aod
         = AeroDiagConfig%extinction_aod
         AeroDiagsState%strat_extinction_aod
         = AeroDiagConfig%strat_extinction_aod

         AeroDiagsState%scattering_aod
         = AeroDiagConfig%scattering_aod

         AeroDiagsState%strat_scattering_aod
         = AeroDiagConfig%strat_scattering_aod

         AeroDiagsState%sfcmasspm25
         = AeroDiagConfig%SfcMasspm25
         AeroDiagsState%colmasspm25
         = AeroDiagConfig%ColMasspm25
         AeroDiagsState%aerindx
         = AeroDiagConfig%AerosolIndex
         AeroDiagsState%fluxu
         = AeroDiagConfig%FluxU

         if (AeroDiagsState%fluxv) then
            AeroDiagConfig%FluxV
         end if

         if ( AeroDiagConfig%extinction_coef) then
            AeroDiagsState%extinction_coef
         end if

         if ( AeroDiagConfig%scattering_coef) then
            AeroDiagsState%scattering_coef
         end if

         if (AeroDiagConfig%backscatter_coef) then
            AeroDiagsState%backscatter_coef
         end if

         if (AeroDiagConfig%extinction_aod_finemode) then
            AeroDiagsState%extinction_aod_finemode
         end if

         if (AeroDiagConfig%scattering_aod_finemode) then
            AeroDiagsState%scattering_aod_finemode
         end if

         if (AeroDiagConfig%Angstrom) then
            AeroDiagsState%angstrom
         end if

         if ( AeroDiagConfig%NO3nFlag ) then
           AeroDiagsState%NO3nFlag
         end if

      else

         AeroDiagsState%Activate = .false.

      endif

   end subroutine CCPR_Aero_Diags_Init


   !>
   !! \brief Run the Aerosol Diagnostics scheme
   !!
   !! \param [IN] MetState - The MetState object
   !! \param [INOUT] DiagState - The DiagState object
   !! \param [INOUT] AeroDiagsState - The AeroDiagsState object
   !! \param [INOUT] ChemState - The ChemState object
   !! \param [OUT] RC Return code
   !!!>
   SUBROUTINE CCPr_Aero_Diags_Run( MetState, DiagState, AeroDiagsState, ChemState, RC )

      ! USE
      USE constants, only : g0
      USE CCPr_Scheme_GOCART_Aero_Diags_Mod, only : CCPR_Scheme_GOCART_Aero_Diags

      IMPLICIT NONE
      ! INPUT PARAMETERS
      TYPE(MetStateType),  INTENT(IN) :: MetState       ! MetState Instance

      ! INPUT/OUTPUT PARAMETERS
      TYPE(DiagStateType), INTENT(INOUT)   :: DiagState       ! DiagState Instance
      TYPE(AeroDiagsStateType), INTENT(INOUT)    :: AeroDiagsState     ! AeroDiagsState Instance
      TYPE(ChemStateType),  INTENT(INOUT)  :: ChemState       ! ChemState Instance

      ! OUTPUT PARAMETERS
      INTEGER, INTENT(OUT) :: RC                                 ! Return Code

      ! LOCAL VARIABLES
      CHARACTER(LEN=255) :: ErrMsg, thisLoc

      ! Initialize
      RC = CC_SUCCESS
      errMsg = ''
      thisLoc = ' -> at CCPr_Aero_Diags_Run (in process/diagnostics/ccpr_aero_diags_mod.F90)'

      ! Run the Aerosol Diagnostics Scheme
      !-------------------------
      if (AeroDiagsState%Activate) then
         ! Run the Aerosol Diagnostics Scheme
         !-------------------------

         if (AeroDiagsState%SchemeOpt == 1) then
            ! Run the Aerosol Diagnostics Scheme
            !-------------------------

            call CCPr_Scheme_GOCART_Aero_Diags(MetState%NLEVS, &
                    klid, nbegin, nbins, rlow, rup, &
                    wavelengths_profile, wavelengths_vertint, &
                    aerosol, g0, &
                    MetState%T,&
                    MetState%AIRDEN, &
                    MetState%RH, &
                    MetState%UWND, &
                    MetState%VWND, &
                    MetState%DELP, &
                    MetState%PEDGE_DRY, &
                    MetState%TROPP, &
                    RC)

         endif

      endif

   end subroutine CCPr_Aero_Diags_Run


   !>
   !! \brief Finalize the Aerosol Diagnostics
   !!
   !! \param [INOUT] AeroDiagsState
   !! \param [OUT] RC Return code
   !!!>
   SUBROUTINE CCPr_Aero_Diags_Finalize( AeroDiagsState, RC )

      ! USE
      !----

      IMPLICIT NONE

      ! INPUT/OUTPUT PARAMETERS
      TYPE(AeroDiagsStateType), INTENT(INOUT) :: AeroDiagsState  ! Aero Diagnostics State Instance

      ! OUTPUT PARAMETERS
      INTEGER, INTENT(OUT) :: RC                                  ! Return Code

      ! LOCAL VARIABLES
      CHARACTER(LEN=255) :: ErrMsg, thisLoc

      ! Initialize
      RC = CC_SUCCESS
      errMsg = ''
      thisLoc = ' -> at CCPr_Aero_Diags_Finalize (in process/diagnostics/CCPr_Aero_Diags_mod.F90)'


   end subroutine CCPr_Aero_Diags_Finalize


END MODULE CCPR_Aero_Diags_Mod





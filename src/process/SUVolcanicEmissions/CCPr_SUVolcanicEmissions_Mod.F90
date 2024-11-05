!> \brief CCPR suvolcanicemissions state types
!!
!! \defgroup catchem_suvolcanicemissions_process
!!
!! \author Lacey Holland
!! \date 10/2024
!!!>
MODULE CCPR_SUVolcanicEmissions_mod
   USE Precision_mod
   USE Error_Mod
   USE DiagState_Mod, Only : DiagStateType
   USE MetState_Mod,  Only : MetStateType
   USE ChemState_Mod, Only : ChemStateType
   USE Config_Opt_Mod, Only : ConfigType

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: CCPR_SUVolcanicEmissions_Init
   PUBLIC :: CCPR_SUVolcanicEmissions_Run
   PUBLIC :: CCPR_SUVolcanicEmissions_Finalize
   PUBLIC :: SUVolcanicEmissionsStateType


   !> \brief SUVolcanicEmissionsStateType
   !!
   !! SUVolcanicEmissionsStateType is the process-specific derived type.
   !!
   !! \param Activate Activate Process (True/False)
   !! \param Scheme Scheme Option
   !! \param SUVolcanicSpeciesIndex Effected Chemical Species from Volcanic
   !! \param nSpc # of species
   !! \param SpcIDs CATChem species IDs
   !! \param ScaleFactor Scale Factor
   !! \param Resuspension Activate resuspension  (True/False)
   !!
   !! \ingroup core_modules
   !!!>
   TYPE :: SUVolcanicEmissionsStateType

      ! Process Specific Parameters

      ! Namelist parameters for specific Volcanic goes here as well
      !=================================================================
      ! Module specific variables/arrays/data pointers come below
      !=================================================================

      LOGICAL                         :: Activate              ! Activate Process (True/False)
      INTEGER                         :: SchemeOpt             ! Scheme Option (if there is only one SchemeOpt always = 1)


   END TYPE SUVolcanicEmissionsStateType


CONTAINS

   !>
   !! \brief Initialize the CATChem Volcanic module
   !!
   !! \param Config       CATCHem configuration options
   !! \param SUVolcanicEmissionsState   CATCHem PROCESS state
   !! \param ChemState         CATCHem chemical state
   !! \param RC               Error return code
   !!
   !! \ingroup catchem_suvolcanicemissions_process
   !!
   !!!>
   SUBROUTINE CCPR_SUVolcanicEmissions_Init( Config, SUVolcanicEmisionsState, ChemState, RC )
      ! USE


      IMPLICIT NONE
      ! INPUT PARAMETERS
      !-----------------
      TYPE(ConfigType)    :: Config    ! Module options
      TYPE(ChemStateType) :: ChemState ! Chemical state

      ! INPUT/OUTPUT PARAMETERS
      !------------------------
      TYPE(SUVolcanicEmissionsStateType)          :: SUVolcanicEmissionsState ! Volcanic state
      INTEGER,         INTENT(INOUT) :: RC       ! Success or failure

      ! Error handling
      !---------------
      CHARACTER(LEN=255)    :: ErrMsg
      CHARACTER(LEN=255)    :: ThisLoc

      ! LOCAL VARIABLES
      !----------------


      ! Put any local variables here

      !=================================================================
      ! CCPR_DryDep_Init begins here!
      !=================================================================
      ErrMsg = ''
      ThisLoc = ' -> at CCPR_SUVolcanicEmissions_INIT (in process/SUVolcanicEmissions/CCPr_SUVolcanicEmissions_mod.F90)'

      ! First check if process is activated in config | if not don't allocate arrays or pointers
      if (Config%suvolcanicemissions_activate) then

         ! Activate Process
         !------------------
         SUVolcanicEmissionsState%Activate = .true.
         allocate(SUVolcanicEmissionsState%drydep_frequency(ChemState%nSpeciesSUVolcanic), STAT=RC)
         IF ( RC /= CC_SUCCESS ) THEN
            ErrMsg = 'Could not Allocate SUVolcanicEmissionsState%drydep_frequency(ChemState%nSpeciesSUVolcanic)'
            CALL CC_Error( ErrMsg, RC, ThisLoc )
         ENDIF
         SUVolcanicEmissionsState%drydep_frequency(1:ChemState%nSpeciesSUVolcanic)=ZERO

         ! Set scheme option
         !------------------
         ! For now, the only option is SchemeOpt = 1
         SUVolcanicEmissionsState%SchemeOpt = 1

      else
         SUVolcanicEmissionsState%Activate = .false.
      end if

   end subroutine CCPR_SUVolcanicEmissions_Init

   !>
   !! \brief Run the SUVolcanicEmisions
   !!
   !! \param [IN] MetState - The MetState object
   !! \param [INOUT] DiagState - The DiagState object
   !! \param [INOUT] SUVolcanicEmissionsState - The SUVolcanicEmissionsState object
   !! \param [INOUT] ChemState - The ChemState object
   !! \param [OUT] RC Return code
   !!
   !! \ingroup catchem_suvolcanicemissions_process
   !!!>
   SUBROUTINE CCPr_SUVolcanicEmissions_Run( MetState, DiagState, SUVolcanicEmissionsState, ChemState, RC )

      ! USE
      USE constants, only : g0
      use CCPr_Scheme_GOCART_SUVolcanicEmissions_Mod, only : CCPr_Scheme_GOCART_SUVolcanicEmissions

      IMPLICIT NONE
      ! INPUT PARAMETERS
      TYPE(MetStateType),  INTENT(IN) :: MetState       !< MetState Instance

      ! INPUT/OUTPUT PARAMETERS
      TYPE(DiagStateType), INTENT(INOUT)      :: DiagState       !< DiagState Instance
      TYPE(SUVolcanicEmissionsStateType), INTENT(INOUT)  :: SUVolcanicEmissionsState  !< SUVolcanicEmissionsState Instance
      TYPE(ChemStateType),  INTENT(INOUT)     :: ChemState       !< ChemState Instance

      ! OUTPUT PARAMETERS
      INTEGER, INTENT(OUT) :: RC                                 ! Return Code

      ! LOCAL VARIABLES
      CHARACTER(LEN=255) :: ErrMsg, thisLoc
      INTEGER :: km
      REAL(fp) :: dqa                       ! Change in Species due to drydep
      REAL(fp) :: SpecConc                  ! Temporary Species concentration

      ! Initialize
      RC = CC_SUCCESS
      errMsg = ''
      thisLoc = ' -> at CCPr_SUVolcanicEmissions_Run &
             & (in process/SUVolcanicEmissions/ccpr_SUVolcanicEmissions_mod.F90)'

      km = MetState%NLEVS

      ! Run the DryDep Scheme
      !-------------------------
      if (SUVolcanicEmissionsState%Activate) then
         ! Run the DryDep Scheme
         !-------------------------
         if (SUVolcanicEmissionsState%SchemeOpt == 1) then
            ! Run the SU Volcanic Scheme - Only Applicable to AEROSOL species
            !-------------------------

            ! Below is not needed if only SO2 is reported
            if (ChemState%nSpeciesSUVolcanic > 0) then

               ! loop through aerosol species
               do i = 1, ChemState%nSpeciesSUVolcanic

                  call CCPr_Scheme_GOCART_SUVolcanicEmissions( MetState%NLEVS,   &
                     MetState%ZMID,    &
                     MetState%DELP,    &
                     g0,               &
                     RC)

               end do ! do i = 1, ChemState%nSpeciesSUVolcanic

            endif  ! if (ChemState%nSpeciesSUVolcanic > 0)

         endif  ! if (SUVolcanicEmissionsState%SchemeOpt == 1)

         ! TO DO:  apply dry dep velocities/freq to chem species
         write(*,*) 'TODO: Need to figure out how to add back to the chemical species state '

      endif   !  if (SUVolcanicEmissionsState%Activate)


   end subroutine CCPr_SUVolcanicEmissions_Run

   !>
   !! \brief Finalize the DryDep
   !!
   !! \param [INOUT] SUVolcanicEmissionsState
   !! \param [OUT] RC Return code
   !!!>
   SUBROUTINE CCPr_SUVolcanicEmissions_Finalize( SUVolcanicEmissionsState, RC )

      ! USE
      !----

      IMPLICIT NONE

      ! INPUT/OUTPUT PARAMETERS
      TYPE(SUVolcanicEmissionsStateType), INTENT(INOUT) :: SUVolcanicEmissionsState  ! SUVolcanicEmissionsState Instance

      ! OUTPUT PARAMETERS
      INTEGER, INTENT(OUT) :: RC                                  ! Return Code

      ! LOCAL VARIABLES
      CHARACTER(LEN=255) :: ErrMsg, thisLoc

      ! Initialize
      RC = CC_SUCCESS
      errMsg = ''
      thisLoc = ' -> at CCPr_SUVolcanicEmissions_Finalize &
      &(in process/SUVolcanicEmissions/ccpr_SUVolcanicEmissions_mod.F90)'

! This will not need to be deallocated, leaving in as placeholders; need to change

      DEALLOCATE( SUVolcanicEmissionsState%drydep_vel, STAT=RC )
      IF ( RC /= CC_SUCCESS ) THEN
         ErrMsg = 'Could not Deallocate SUVolcanicEmissionsState%drydep_vel'
         CALL CC_Error( ErrMsg, RC, ThisLoc )
      ENDIF


   end subroutine CCPr_SUVolcanicEmissions_Finalize


END MODULE CCPR_SUVolcanicEmissions_Mod




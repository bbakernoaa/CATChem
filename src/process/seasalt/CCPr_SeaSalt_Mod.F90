!> \file CCPr_SeaSalt_mod.F90
!! \brief Driver for the CATCHem Process: SeaSalt
!!
!! Driver for the seasalt process.
!!
!! \defgroup catchem_seasalt_process
!! \ingroup catchem
!!
!!
!! The CATChem SeaSalt Process group holds all the CATCHem SeaSalt processes.
!!!>
MODULE CCPr_SeaSalt_mod

   ! USES:
   USE Precision_Mod
   USE Error_MOD
   USE DiagState_Mod, Only : DiagStateType
   USE MetState_Mod, Only : MetStateType
   USE Config_Opt_Mod, Only : ConfigType
   USE ChemState_Mod, Only : ChemStateType
   USE CCPr_SeaSalt_Common_Mod, Only : SeaSaltStateType

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: CCPR_SeaSalt_Run
   PUBLIC :: CCPR_SeaSalt_Init
   PUBLIC :: CCPR_SeaSalt_Finalize

CONTAINS

   !>
   !! \brief Initialize the CATCHem SeaSalt Process
   !!
   !! \param Config_Opt  CATCHem configuration options
   !! \param SeaSaltState   CATCHem SeaSalt state
   !! \param ChmState    CATCHem chemical state
   !! \param RC          Error return code
   !!
   !! \ingroup catchem_seasalt_process
   !!!>
   SUBROUTINE CCPR_SeaSalt_Init( Config, SeaSaltState, ChemState, RC)
      ! USES

      IMPLICIT NONE

      ! INPUT PARAMETERS
      !-----------------
      TYPE(ConfigType),    intent(in)    :: Config     ! Config options
      TYPE(SeaSaltStateType), intent(inout) :: SeaSaltState  ! Nullify SeaSalt State During INIT
      TYPE(ChemStateType), intent(in)    :: ChemState  ! Chemical State

      ! INPUT/OUTPUT PARAMETERS
      !------------------------
      INTEGER,          INTENT(INOUT) :: RC

      ! LOCAL VARIABLES
      !----------------
      Integer, parameter :: nSeaSaltBinsDefault = 5
      REAL(fp), DIMENSION(nSeaSaltBinsDefault), Parameter :: DefaultSeaSaltDensity  = (/ 2200., &
         2200., &
         2200., &
         2200., &
         2200. /)
      REAL(fp), DIMENSION(nSeaSaltBinsDefault), Parameter :: DefaultEffectiveRadius = (/ 0.079, &
         0.316, &
         1.119, &
         2.818, &
         7.772 /)
      REAL(fp), DIMENSION(nSeaSaltBinsDefault), Parameter :: DefaultLowerBinRadius  = (/ 0.03, &
         0.1,  &
         0.5,  &
         1.5,  &
         5.0  /)
      REAL(fp), DIMENSION(nSeaSaltBinsDefault), Parameter :: DefaultUpperBinRadius  = (/ 0.1, &
         0.5, &
         1.5, &
         5.0, &
         10. /)

      INTEGER :: k ! Loop Counter

      ! Error handling
      !---------------
      CHARACTER(LEN=255)    :: ErrMsg
      CHARACTER(LEN=255)    :: ThisLoc

      ! Initialize Error handling
      !--------------------------
      ErrMsg = ''
      ThisLoc = ' -> at CCPR_SeaSalt_INIT (in process/SeaSalt/ccpr_SeaSalt_mod.F90)'

      ! Initialize
      !-----------
      if (Config%seasalt_activate) then

         ! Activate SeaSalt Process
         !-------------------------
         SeaSaltState%Activate = .true.

         ! Set number of seasalt species
         !------------------------------
         SeaSaltState%nSeaSaltSpecies = ChemState%nSpeciesSeaSalt

         ! Set Scheme Options | Default GEOS 2012 scheme
         !----------------------------------------------
         if (Config%seasalt_scheme < 0) then
            SeaSaltState%SeaSaltScaleFactor = 3
         else
            SeaSaltState%SeaSaltScaleFactor = Config%seasalt_scheme
         endif
         SeaSaltState%SchemeOpt = Config%seasalt_scheme

         ! Set Tuning Scale Factor | Default = 1 if not set
         !-------------------------------------------------
         if (Config%seasalt_scalefactor < 0) then
            SeaSaltState%SeaSaltScaleFactor = 1
         else
            SeaSaltState%SeaSaltScaleFactor = Config%seasalt_scalefactor
         endif

         ! Set Weibull Distribution flag following Fan and Toon 2011 | Default = .true.
         SeaSaltState%WeibullFlag = Config%seasalt_weibull

         ! Set Hoppel Correction flag following Fan and Toon 2011 | Default = .true.
         SeaSaltState%HoppelFlag = Config%seasalt_hoppel

         if (SeaSaltState%nSeaSaltSpecies == 0) then

            ! Set default bin properties for schemes that need them
            !------------------------------------------------------
            ALLOCATE(SeaSaltState%LowerBinRadius(nSeaSaltBinsDefault), STAT=RC)
            CALL CC_CheckVar('SeaSaltState%LowerBinRadius', 0, RC)
            IF (RC /= CC_SUCCESS) RETURN
            do k = 1, nSeaSaltBinsDefault
               SeaSaltState%LowerBinRadius(k) = DefaultLowerBinRadius(k)
            end do

            ALLOCATE(SeaSaltState%UpperBinRadius(nSeaSaltBinsDefault), STAT=RC)
            CALL CC_CheckVar('SeaSaltState%UpperBinRadius', 0, RC)
            IF (RC /= CC_SUCCESS) RETURN
            do k = 1, nSeaSaltBinsDefault
               SeaSaltState%UpperBinRadius(k) = DefaultUpperBinRadius(k)
            enddo

            ALLOCATE(SeaSaltState%EffectiveRadius(nSeaSaltBinsDefault), STAT=RC)
            CALL CC_CheckVar('SeaSaltState%EffectiveRadius', 0, RC)
            IF (RC /= CC_SUCCESS) RETURN
            do k = 1, nSeaSaltBinsDefault
               SeaSaltState%EffectiveRadius(k) = DefaultEffectiveRadius(k)
            end do

            ALLOCATE(SeaSaltState%SeaSaltDensity(nSeaSaltBinsDefault), STAT=RC)
            CALL CC_CheckVar('SeaSaltState%SeaSaltDensity', 0, RC)
            IF (RC /= CC_SUCCESS) RETURN
            do k = 1, nSeaSaltBinsDefault
               SeaSaltState%SeaSaltDensity(k) = DefaultSeaSaltDensity(k)
            end do

            ALLOCATE(SeaSaltState%EmissionPerSpecies(nSeaSaltBinsDefault), STAT=RC)
            CALL CC_CheckVar('EmissionPerSpecies', 0, RC)
            IF (RC /= CC_SUCCESS) RETURN
            do k = 1, nSeaSaltBinsDefault
               SeaSaltState%EmissionPerSpecies(k) = ZERO
            end do

            ALLOCATE(SeaSaltState%NumberEmissionBin(nSeaSaltBinsDefault), STAT=RC)
            CALL CC_CheckVar('SeaSaltState%NumberEmissionBin', 0, RC)
            IF (RC /= CC_SUCCESS) RETURN
            do k = 1, nSeaSaltBinsDefault
               SeaSaltState%NumberEmissionBin(k) = ZERO
            end do

         else

            ! seasalt Aerosols are present in ChmState
            !--------------------------------------

            !TODO: Need to figure out how exactly to do this at the moment
            write(*,*) 'TODO: Need to figure out how to add back to the chemical species state '

         endif

      else
         SeaSaltState%TotalEmission = ZERO
         SeaSaltState%TotalNumberEmission = ZERO
         SeaSaltState%SeaSaltScaleFactor = ZERO
         SeaSaltState%SchemeOpt = 3
         SeaSaltState%Activate = .false.

      endif


   END SUBROUTINE CCPR_SeaSalt_INIT

   !>
   !! \brief Run the seasalt scheme
   !!
   !! \param [IN] MetState The MetState object
   !! \param [INOUT] DiagState The DiagState object
   !! \param [INOUT] SeaSaltState The SeaSaltState object
   !! \param [INOUT] ChemState The ChemState object
   !! \param [OUT] RC Return code
   !!
   !! \ingroup CATChem_SeaSalt_Processes
   !!!>
   SUBROUTINE CCPr_SeaSalt_Run( MetState, DiagState, SeaSaltState, ChemState, RC )

      ! USE
      USE CCPr_Scheme_Gong03_mod,  ONLY: CCPr_Scheme_Gong03   !< Gong2003 SeaSalt Scheme
      USE CCPr_Scheme_Gong97_mod,  ONLY: CCPr_Scheme_Gong97   !< Gong1997 SeaSalt Scheme
      USE CCPr_Scheme_GEOS12_mod,  ONLY: CCPr_Scheme_GEOS12   !< Gong1997 SeaSalt Scheme

      IMPLICIT NONE

      ! INPUT PARAMETERS
      !-----------------
      TYPE(MetStateType),  INTENT(IN) :: MetState       ! MetState Instance

      ! INPUT/OUTPUT PARAMETERS
      !------------------------
      TYPE(DiagStateType), INTENT(INOUT)    :: DiagState   ! DiagState Instance
      TYPE(SeaSaltStateType), INTENT(INOUT) :: SeaSaltState   ! SeaSaltState Instance
      TYPE(ChemStateType), INTENT(INOUT)    :: ChemState  ! ChemState Instance

      ! OUTPUT PARAMETERS
      !------------------
      INTEGER, INTENT(OUT) :: RC                         ! Return Code


      ! LOCAL VARIABLES
      !----------------
      CHARACTER(LEN=255) :: ErrMsg, thisLoc

      ! Initialize
      !-----------
      RC = CC_SUCCESS
      errMsg = ''
      thisLoc = ' -> at CCPr_SeaSalt_Run (in process/seasalt/ccpr_seasalt_mod.F90)'

      if (SeaSaltState%Activate) then

         ! Run the SeaSalt Scheme
         !--------------------
         if (SeaSaltState%SchemeOpt == 1) then ! Gong2003
            call CCPr_Scheme_Gong03(MetState%FROCEAN,                 &
               MetState%FRSEAICE,                &
               MetState%U10M,                    &
               MetState%V10M,                    &
               MetState%SST,                     &
               SeaSaltState%WeibullFlag,         &
               SeaSaltState%SeaSaltScaleFactor,  &
               SeaSaltState%UpperBinRadius,      &
               SeaSaltState%LowerBinRadius,      &
               SeaSaltState%EffectiveRadius,     &
               SeaSaltState%SeaSaltDensity,      &
               SeaSaltState%EmissionPerSpecies,  &
               SeaSaltState%NumberEmissionBin,   &
               SeaSaltState%TotalEmission,       &
               SeaSaltState%TotalNumberEmission, &
               RC)
            if (RC /= CC_SUCCESS) then
               errMsg = 'Error in CCPr_Scheme_Gong03'
               CALL CC_Error( errMsg, RC, thisLoc )
            endif
         else if (SeaSaltState%SchemeOpt == 2) then ! Gong1997
            ! call CCPr_Scheme_Gong97( MetState, DiagState, SeaSaltState, RC )
            call CCPr_Scheme_Gong97(MetState%FROCEAN,                 &
               MetState%FRSEAICE,                &
               MetState%U10M,                    &
               MetState%V10M,                    &
               MetState%SST,                     &
               SeaSaltState%WeibullFlag,         &
               SeaSaltState%SeaSaltScaleFactor,  &
               SeaSaltState%UpperBinRadius,      &
               SeaSaltState%LowerBinRadius,      &
               SeaSaltState%EffectiveRadius,     &
               SeaSaltState%SeaSaltDensity,      &
               SeaSaltState%EmissionPerSpecies,  &
               SeaSaltState%NumberEmissionBin,   &
               SeaSaltState%TotalEmission,       &
               SeaSaltState%TotalNumberEmission, &
               RC)
            if (RC /= CC_SUCCESS) then
               errMsg = 'Error in CCPr_Scheme_Gong97'
               CALL CC_Error( errMsg, RC, thisLoc )
            endif
         else if (SeaSaltState%SchemeOpt == 3) then ! GEOS2012
            call CCPr_Scheme_GEOS12(MetState%FROCEAN,                 &
               MetState%FRSEAICE,                &
               MetState%USTAR,                   &
               MetState%SST,                     &
               SeaSaltState%SeaSaltScaleFactor,  &
               SeaSaltState%UpperBinRadius,      &
               SeaSaltState%LowerBinRadius,      &
               SeaSaltState%EffectiveRadius,     &
               SeaSaltState%SeaSaltDensity,      &
               SeaSaltState%EmissionPerSpecies,  &
               SeaSaltState%NumberEmissionBin,   &
               SeaSaltState%TotalEmission,       &
               SeaSaltState%TotalNumberEmission, &
               RC)
            if (RC /= CC_SUCCESS) then
               errMsg = 'Error in CCPr_Scheme_GEOS12'
               CALL CC_Error( errMsg, RC, thisLoc )
            endif
         else
            errMsg =  'ERROR: Unknown seasalt scheme option'
            RC = CC_FAILURE
            CALL CC_Error( errMsg, RC, thisLoc )
            return
         endif
      endif



   END SUBROUTINE CCPr_SeaSalt_Run

   !>
   !! \brief Finalize the seasalt scheme
   !!
   !! \param [INOUT] SeaSaltState The SeaSaltState object
   !! \param [OUT] RC Return code
   !!
   !! \ingroup CATChem_SeaSalt_Processes
   !!!>
   SUBROUTINE CCPr_SeaSalt_Finalize( SeaSaltState, RC )

      ! USE
      !----

      IMPLICIT NONE

      ! INPUT/OUTPUT PARAMETERS
      !------------------------
      TYPE(SeaSaltStateType), INTENT(INOUT) :: SeaSaltState ! SeaSaltState Instance
      INTEGER, INTENT(OUT) :: RC                       ! Return Code

      ! LOCAL VARIABLES
      !----------------
      CHARACTER(LEN=255) :: ErrMsg, thisLoc

      ! Initialize
      !-----------
      RC = CC_SUCCESS
      errMsg = ''
      thisLoc = ' -> at CCPr_SeaSalt_Finalize (in process/seasalt/ccpr_SeaSalt.F90)'

      DEALLOCATE( SeaSaltState%LowerBinRadius, STAT=RC )
      CALL CC_CheckVar('SeaSaltState%LowerBinRadius', 0, RC)
      IF (RC /= CC_SUCCESS) RETURN

      DEALLOCATE( SeaSaltState%UpperBinRadius, STAT=RC )
      CALL CC_CheckVar('SeaSaltState%UpperBinRadius', 0, RC)
      IF (RC /= CC_SUCCESS) RETURN

      DEALLOCATE( SeaSaltState%EffectiveRadius, STAT=RC )
      CALL CC_CheckVar('SeaSaltState%EffectiveRadius', 0, RC)
      IF (RC /= CC_SUCCESS) RETURN

      DEALLOCATE( SeaSaltState%SeaSaltDensity, STAT=RC )
      CALL CC_CheckVar('SeaSaltState%SeaSaltDensity', 0, RC)
      IF (RC /= CC_SUCCESS) RETURN

      DEALLOCATE( SeaSaltState%EmissionPerSpecies, STAT=RC )
      CALL CC_CheckVar('SeaSaltState%EmissionPerSpecies', 0, RC)
      IF (RC /= CC_SUCCESS) RETURN

   END SUBROUTINE CCPr_SeaSalt_Finalize

END MODULE CCPr_SeaSalt_Mod

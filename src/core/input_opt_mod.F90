!------------------------------------------------------------------------------
!                  GEOS-Chem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: input_opt_mod.F90
!
! !DESCRIPTION: Module INPUT\_OPT\_MOD contains the derived type for GEOS-Chem
!  options and logical switches.
!\\
!\\
! !INTERFACE:
!
MODULE Input_Opt_Mod
!
! !USES:
!
  USE PRECISION_MOD    ! For GEOS-Chem Precision (fp)

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC :: Set_Input_Opt
  PUBLIC :: Cleanup_Input_Opt
!
! !PUBLIC DATA MEMBERS:
!
  !=========================================================================
  ! Derived type for Input Options
  !=========================================================================
  TYPE, PUBLIC :: OptInput

     !----------------------------------------
     ! General Runtime & Distributed Comp Info
     !----------------------------------------
     INTEGER                     :: numCPUs    ! Number of MPI procs
     INTEGER                     :: thisCPU    ! Local MPI process handle
     INTEGER                     :: MPIComm    ! MPI Communicator Handle
     LOGICAL                     :: isMPI      ! Is this an MPI sim?
     LOGICAL                     :: amIRoot    ! Is this the root cpu?

     !----------------------------------------
     ! Dry run info (print out file names)
     !----------------------------------------
     LOGICAL                     :: DryRun     ! Is this a dry run?

     !----------------------------------------
     ! SIZE PARAMETER fields
     !----------------------------------------
     INTEGER                     :: Max_BPCH_Diag
     INTEGER                     :: Max_Families
     INTEGER                     :: Max_AdvectSpc

     !----------------------------------------
     ! SIMULATION MENU fields
     !----------------------------------------
     INTEGER                     :: NYMDb
     INTEGER                     :: NHMSb
     INTEGER                     :: NYMDe
     INTEGER                     :: NHMSe
     INTEGER                     :: SimLengthSec
     CHARACTER(LEN=255)          :: RUN_DIR
     CHARACTER(LEN=255)          :: DATA_DIR
     CHARACTER(LEN=255)          :: CHEM_INPUTS_DIR
     CHARACTER(LEN=255)          :: MetField
     CHARACTER(LEN=255)          :: SimulationName
     CHARACTER(LEN=255)          :: SpcDatabaseFile
     CHARACTER(LEN=255)          :: SpcMetaDataOutFile
     LOGICAL                     :: ITS_AN_AEROSOL_SIM
     LOGICAL                     :: ITS_A_CARBON_SIM
     LOGICAL                     :: ITS_A_CH4_SIM
     LOGICAL                     :: ITS_A_CO2_SIM
     LOGICAL                     :: ITS_A_FULLCHEM_SIM
     LOGICAL                     :: ITS_A_MERCURY_SIM
     LOGICAL                     :: ITS_A_POPS_SIM
     LOGICAL                     :: ITS_A_TAGCH4_SIM
     LOGICAL                     :: ITS_A_TAGCO_SIM
     LOGICAL                     :: ITS_A_TAGO3_SIM
     LOGICAL                     :: ITS_A_TRACEMETAL_SIM
     LOGICAL                     :: ITS_A_TRACER_SIM
     LOGICAL                     :: VerboseRequested
     CHARACTER(LEN=10)           :: VerboseOnCores
     LOGICAL                     :: Verbose
     LOGICAL                     :: useTimers

     !----------------------------------------
     ! ADVECTED SPECIES MENU fields
     !----------------------------------------
     INTEGER                     :: N_ADVECT
     CHARACTER(LEN=255), POINTER :: AdvectSpc_Name(:)
     LOGICAL                     :: LSPLIT

     !----------------------------------------
     ! AEROSOL MENU fields
     !----------------------------------------
     LOGICAL                     :: LSULF
     LOGICAL                     :: LMETALCATSO2
     LOGICAL                     :: LCARB
     LOGICAL                     :: LBRC
     LOGICAL                     :: LSOA
     LOGICAL                     :: LMPOA
     LOGICAL                     :: LSVPOA
     LOGICAL                     :: LDUST
     LOGICAL                     :: LDEAD
     LOGICAL                     :: LSSALT
     LOGICAL                     :: LDSTUP
     REAL(fp),           POINTER :: SALA_REDGE_um(:)
     REAL(fp),           POINTER :: SALC_REDGE_um(:)
     LOGICAL                     :: LGRAVSTRAT
     LOGICAL                     :: LSOLIDPSC
     LOGICAL                     :: LHOMNUCNAT
     REAL(fp)                    :: T_NAT_SUPERCOOL
     REAL(fp)                    :: P_ICE_SUPERSAT
     LOGICAL                     :: LPSCCHEM
     LOGICAL                     :: LSTRATOD



     !----------------------------------------
     ! CHEMISTRY MENU fields
     !----------------------------------------
     LOGICAL                     :: LCHEM
     LOGICAL                     :: LINEAR_CHEM
     LOGICAL                     :: LLINOZ
     LOGICAL                     :: LSYNOZ
     INTEGER                     :: TS_CHEM
     REAL(fp)                    :: GAMMA_HO2
     LOGICAL                     :: LACTIVEH2O
     LOGICAL                     :: LINITSPEC
     LOGICAL                     :: USE_ONLINE_O3
     LOGICAL                     :: USE_O3_FROM_MET
     LOGICAL                     :: USE_TOMS_O3
     LOGICAL                     :: USE_AUTOREDUCE
     LOGICAL                     :: AUTOREDUCE_IS_KEEPACTIVE
     LOGICAL                     :: AUTOREDUCE_IS_KEY_THRESHOLD
     LOGICAL                     :: AUTOREDUCE_IS_PRS_THRESHOLD
     LOGICAL                     :: AUTOREDUCE_IS_APPEND
     REAL(f8)                    :: AUTOREDUCE_THRESHOLD
     REAL(f8)                    :: AUTOREDUCE_TUNING_OH
     REAL(f8)                    :: AUTOREDUCE_TUNING_NO2






     !----------------------------------------
     ! OUTPUT MENU fields
     !----------------------------------------
     INTEGER,            POINTER :: NJDAY(:)

     !----------------------------------------
     ! DIAGNOSTIC MENU fields
     !----------------------------------------
     CHARACTER(LEN=255)          :: HistoryInputFile
     INTEGER                     :: ND03   ! Hg
     INTEGER                     :: ND06   ! TOMAS
     INTEGER                     :: ND44   ! TOMAS
     INTEGER                     :: ND53   ! POPs
     INTEGER                     :: ND59   ! TOMAS
     INTEGER                     :: ND60   ! TOMAS
     INTEGER                     :: ND61   ! TOMAS

     INTEGER                     :: TS_DIAG
     INTEGER,            POINTER :: TINDEX(:,:)
     INTEGER,            POINTER :: TCOUNT(:)
     INTEGER,            POINTER :: TMAX(:)
     LOGICAL                     :: DO_DIAG_WRITE

     ! Collection ids
     INTEGER                     :: DIAG_COLLECTION
     INTEGER                     :: GC_RST_COLLECTION ! Used only for NetCDF




  END TYPE OptInput
!
! !REMARKS:
!
! !REVISION HISTORY:
!  See https://github.com/geoschem/geos-chem for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
CONTAINS
!EOC
!------------------------------------------------------------------------------
!                  GEOS-Chem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Set_Input_Opt
!
! !DESCRIPTION: Subroutine SET\_INPUT\_OPT intializes all GEOS-Chem
!  options carried in Input Options derived type object.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Set_Input_Opt( am_I_Root, Input_Opt, RC )
!
! !USES:
!
    USE Error_Mod
!
! !INPUT PARAMETERS:
!
    LOGICAL,        INTENT(IN)    :: am_I_Root   ! Are we on the root CPU?
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(OptInput), INTENT(INOUT) :: Input_Opt   ! Input Options object
!
! !OUTPUT PARAMETERS:
!
    INTEGER,        INTENT(OUT)   :: RC          ! Success or failure?
!
! !REMARKS:
!
! !REVISION HISTORY:
!  01 Nov 2012 - R. Yantosca - Initial version
!  See https://github.com/geoschem/geos-chem for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    ! Strings
    CHARACTER(LEN=30) :: arrayId

    !----------------------------------------
    ! Initialize
    ! Set pointers to NULL for safety's sake
    !----------------------------------------
    RC                               =  CC_SUCCESS
    Input_Opt%AdvectSpc_Name         => NULL()
    Input_Opt%SALA_REDGE_um          => NULL()
    Input_Opt%SALC_REDGE_um          => NULL()
    Input_Opt%NJDAY                  => NULL()
    Input_Opt%TINDEX                 => NULL()
    Input_Opt%TCOUNT                 => NULL()
    Input_Opt%TMAX                   => NULL()

    !----------------------------------------
    ! General Runtime & Distributed Comp Info
    !----------------------------------------
    Input_Opt%amIRoot                = am_I_Root
    Input_Opt%isMPI                  = .FALSE.
    Input_Opt%numCPUs                = 1
    Input_Opt%thisCPU                = -1
    Input_Opt%MPIComm                = -1

    !----------------------------------------
    ! Dry run info (print out file names)
    !----------------------------------------
    Input_Opt%DryRun                 = .FALSE.

    !----------------------------------------
    ! SIZE PARAMETER fields
    !
    ! Set to large placeholder values
    !----------------------------------------
#ifdef RRTMG
    Input_Opt%Max_BPCH_Diag          = 187 ! Mirror MAX_DIAG in CMN_DIAG_mod.F90
#else
    Input_Opt%Max_BPCH_Diag          = 80  ! Mirror MAX_DIAG in CMN_DIAG_mod.F90
#endif
    Input_Opt%Max_Families           = 250
    Input_Opt%Max_AdvectSpc          = 600

    !----------------------------------------
    ! SIMULATION MENU fields
    !----------------------------------------
    Input_Opt%NYMDb                  = 0
    Input_Opt%NHMSb                  = 0
    Input_Opt%NYMDe                  = 0
    Input_Opt%NHMSe                  = 0
    Input_Opt%SimLengthSec           = 0
    Input_Opt%RUN_DIR                = './'
    Input_Opt%DATA_DIR               = './'
    Input_Opt%CHEM_INPUTS_DIR        = './'
    Input_Opt%MetField               = ''
    Input_Opt%SimulationName         = ''
    Input_Opt%SpcDatabaseFile        = ''
    Input_Opt%SpcMetaDataOutFile     = ''
    Input_Opt%ITS_AN_AEROSOL_SIM     = .FALSE.
    Input_Opt%ITS_A_CARBON_SIM       = .FALSE.
    Input_Opt%ITS_A_CH4_SIM          = .FALSE.
    Input_Opt%ITS_A_CO2_SIM          = .FALSE.
    Input_Opt%ITS_A_FULLCHEM_SIM     = .FALSE.
    Input_Opt%ITS_A_MERCURY_SIM      = .FALSE.
    Input_Opt%ITS_A_POPS_SIM         = .FALSE.
    Input_Opt%ITS_A_TAGCH4_SIM       = .FALSE.
    Input_Opt%ITS_A_TAGCO_SIM        = .FALSE.
    Input_Opt%ITS_A_TAGO3_SIM        = .FALSE.
    Input_Opt%ITS_A_TRACEMETAL_SIM   = .FALSE.
    Input_Opt%ITS_A_TRACER_SIM       = .FALSE.
    Input_Opt%VerboseRequested       = .FALSE.
    Input_Opt%VerboseOnCores         = ''
    Input_Opt%Verbose                = .FALSE.
    Input_Opt%useTimers              = .FALSE.

    !----------------------------------------
    ! ADVECTED SPECIES MENU fields
    !----------------------------------------
    arrayId = 'Input_Opt%AdvectSpc_Name'
    ALLOCATE( Input_Opt%AdvectSpc_Name( Input_Opt%Max_AdvectSpc ), STAT=RC )
    CALL CC_CheckVar( arrayId, 0, RC )
    IF ( RC /= CC_SUCCESS ) RETURN

    Input_Opt%N_ADVECT               = 0
    Input_Opt%AdvectSpc_Name         = ''
    Input_Opt%LSPLIT                 = .FALSE.

    !----------------------------------------
    ! AEROSOL MENU fields
    !----------------------------------------
    arrayId = 'Input_Opt%SALA_REDGE_um'
    ALLOCATE( Input_Opt%SALA_REDGE_um( 2 ), STAT=RC )
    CALL CC_CheckVar( arrayId, 0, RC )
    IF ( RC /= CC_SUCCESS ) RETURN

    arrayId = 'Input_Opt%SALC_REDGE_um'
    ALLOCATE( Input_Opt%SALC_REDGE_um( 2 ), STAT=RC )
    CALL CC_CheckVar( arrayId, 0, RC )
    IF ( RC /= CC_SUCCESS ) RETURN

    Input_Opt%LSULF                  = .FALSE.
    Input_Opt%LMETALCATSO2           = .FALSE.
    Input_Opt%LCARB                  = .FALSE.
    Input_Opt%LBRC                   = .FALSE.
    Input_Opt%LSOA                   = .FALSE.
    Input_Opt%LMPOA                  = .FALSE.
    Input_Opt%LSVPOA                 = .FALSE.
    Input_Opt%LDUST                  = .FALSE.
    Input_Opt%LDEAD                  = .FALSE.
    Input_Opt%LDSTUP                 = .FALSE.
    Input_Opt%LSSALT                 = .FALSE.
    Input_Opt%SALA_REDGE_um          = 0.0_fp
    Input_Opt%SALC_REDGE_um          = 0.0_fp
    Input_Opt%LGRAVSTRAT             = .FALSE.
    Input_Opt%LSOLIDPSC              = .FALSE.
    Input_Opt%LHOMNUCNAT             = .FALSE.
    Input_Opt%T_NAT_SUPERCOOL        = 0.0_fp
    Input_Opt%P_ICE_SUPERSAT         = 0.0_fp
    Input_Opt%LPSCCHEM               = .FALSE.
    Input_Opt%LSTRATOD               = .FALSE.


    !----------------------------------------
    ! CHEMISTRY MENU fields
    !----------------------------------------
    Input_Opt%LCHEM                  = .FALSE.
    Input_Opt%LINEAR_CHEM            = .FALSE.
    Input_Opt%LLINOZ                 = .FALSE.
    Input_Opt%LSYNOZ                 = .FALSE.
#ifdef MODEL_GEOS
    Input_Opt%LGMIOZ                 = .FALSE.
#endif
    Input_Opt%TS_CHEM                = 0
    Input_Opt%GAMMA_HO2              = 0.0_fp
    Input_Opt%LACTIVEH2O             = .FALSE.
    Input_Opt%LINITSPEC              = .FALSE.
    Input_Opt%USE_ONLINE_O3          = .FALSE.
    Input_Opt%USE_O3_FROM_MET        = .FALSE.
    Input_Opt%USE_TOMS_O3            = .FALSE.

    Input_Opt%USE_AUTOREDUCE                = .FALSE.
    Input_Opt%AUTOREDUCE_IS_KEY_THRESHOLD   = .TRUE.
    Input_Opt%AUTOREDUCE_TUNING_OH          = 5e-5_fp
    Input_Opt%AUTOREDUCE_TUNING_NO2         = 1e-4_fp
    Input_Opt%AUTOREDUCE_IS_PRS_THRESHOLD   = .TRUE.
    Input_Opt%AUTOREDUCE_IS_KEEPACTIVE      = .FALSE.
    Input_Opt%AUTOREDUCE_IS_APPEND          = .FALSE.


    !----------------------------------------
    ! DIAGNOSTIC MENU fields
    !----------------------------------------
    Input_Opt%HistoryInputFile       = ''
    Input_Opt%DIAG_COLLECTION        = -999
    Input_Opt%TS_DIAG                = 0
    ALLOCATE( Input_Opt%TCOUNT( Input_Opt%Max_BPCH_Diag ), STAT=RC )
    ALLOCATE( Input_Opt%TMAX  ( Input_Opt%Max_BPCH_Diag ), STAT=RC )

    Input_Opt%ND03                   = 0
    Input_Opt%ND06                   = 0
    Input_Opt%ND44                   = 0
    Input_Opt%ND53                   = 0
    Input_Opt%ND59                   = 0
    Input_Opt%ND60                   = 0
    Input_Opt%ND61                   = 0
    Input_Opt%TCOUNT(:)              = 0
    Input_Opt%TMAX(:)	             = 0
#if defined( ESMF_ ) || defined( EXTERNAL_GRID ) || defined( EXTERNAL_FORCING )
    ! Need to shut off G-C diagnostics when
    ! connecting to an external GCM (bmy, 3/29/13)
    Input_Opt%DO_DIAG_WRITE          = .FALSE.
#else
    ! For traditional G-C runs, always write diags (bmy, 3/29/13)
    Input_Opt%DO_DIAG_WRITE          = .TRUE.
#endif



  END SUBROUTINE Set_Input_Opt

  !EOC
!------------------------------------------------------------------------------
!                  GEOS-Chem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Cleanup_Input_Opt
!
! !DESCRIPTION: Subroutine CLEANUP\_INPUT\_OPT deallocates all
!  allocatable fields of the Input Options object.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Cleanup_Input_Opt( Input_Opt, RC )
!
! !USES:
!
    USE Error_Mod
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(OptInput), INTENT(INOUT) :: Input_Opt   ! Input Options object
!
! !OUTPUT PARAMETERS:
!
    INTEGER,        INTENT(OUT)   :: RC          ! Success or failure
!
! !REVISION HISTORY:
!  02 Nov 2012 - R. Yantosca - Initial version
!  See https://github.com/geoschem/geos-chem for complete history
!EOP
!------------------------------------------------------------------------------
!BOC

    ! Assume success
    RC = CC_SUCCESS

    !======================================================================
    ! Deallocate fields of the Input Options object
    !======================================================================
    IF ( ASSOCIATED( Input_Opt%AdvectSpc_Name ) ) THEN
       DEALLOCATE( Input_Opt%AdvectSpc_Name, STAT=RC )
       CALL CC_CheckVar( 'Input_Opt%AdvectSpcName', 2, RC )
       IF ( RC /= CC_SUCCESS ) RETURN
       Input_Opt%AdvectSpc_Name => NULL()
    ENDIF

    IF ( ASSOCIATED( Input_Opt%SALA_REDGE_um ) ) THEN
       DEALLOCATE( Input_Opt%SALA_REDGE_um, STAT=RC )
       CALL CC_CheckVar( 'Input_Opt%SALA_REDGE_um', 2, RC )
       IF ( RC /= CC_SUCCESS ) RETURN
       Input_Opt%SALA_REDGE_um => NULL()
    ENDIF

    IF ( ASSOCIATED( Input_Opt%SALC_REDGE_um ) ) THEN
       DEALLOCATE( Input_Opt%SALC_REDGE_um, STAT=RC )
       CALL CC_CheckVar( 'Input_Opt%SALC_REDGE_um', 2, RC )
       IF ( RC /= CC_SUCCESS ) RETURN
       Input_Opt%SALC_REDGE_um => NULL()
    ENDIF

    IF ( ASSOCIATED( Input_Opt%NJDAY ) ) THEN
       DEALLOCATE( Input_Opt%NJDAY, STAT=RC )
       CALL CC_CheckVar( 'Input_Opt%NJDAY', 2, RC )
       IF ( RC /= CC_SUCCESS ) RETURN
       Input_Opt%NJDAY => NULL()
    ENDIF

    IF ( ASSOCIATED( Input_Opt%TINDEX ) ) THEN
       DEALLOCATE( Input_Opt%TINDEX, STAT=RC )
       CALL CC_CheckVar( 'Input_Opt%TINDEX', 2, RC )
       IF ( RC /= CC_SUCCESS ) RETURN
       Input_Opt%TINDEX => NULL()
    ENDIF

    IF ( ASSOCIATED( Input_Opt%TCOUNT ) ) THEN
       DEALLOCATE( Input_Opt%TCOUNT, STAT=RC )
       CALL CC_CheckVar( 'Input_Opt%TCOUNT', 2, RC )
       IF ( RC /= CC_SUCCESS ) RETURN
       Input_Opt%TCOUNT => NULL()
    ENDIF

    IF ( ASSOCIATED( Input_Opt%TMAX ) ) THEN
       DEALLOCATE( Input_Opt%TMAX, STAT=RC )
       CALL CC_CheckVar( 'Input_Opt%TMAX', 2, RC )
       IF ( RC /= CC_SUCCESS ) RETURN
       Input_Opt%TMAX => NULL()
    ENDIF





#ifdef MODEL_GEOS
    !=======================================================================
    ! These fields of Input_Opt are only finalized when
    ! GEOS-Chem is coupled to the online NASA/GEOS ESM
    !=======================================================================
    IF ( ASSOCIATED( Input_Opt%RxnRconst_IDs ) ) THEN
       DEALLOCATE( Input_Opt%RxnRconst_IDs, STAT=RC )
       CALL CC_CheckVar( 'Input_Opt%RxnRconst_IDs', 2, RC )
       IF ( RC /= CC_SUCCESS ) RETURN
       Input_Opt%RxnRconst_IDs => NULL()
    ENDIF

    IF ( ASSOCIATED( Input_Opt%RxnRates_IDs ) ) THEN
       DEALLOCATE( Input_Opt%RxnRates_IDs, STAT=RC )
       CALL CC_CheckVar( 'Input_Opt%RxnRates_IDs', 2, RC )
       IF ( RC /= CC_SUCCESS ) RETURN
       Input_Opt%RxnRates_IDs => NULL()
    ENDIF

    IF ( ASSOCIATED( Input_Opt%Jval_IDs ) ) THEN
       DEALLOCATE( Input_Opt%Jval_IDs, STAT=RC )
       CALL CC_CheckVar( 'Input_Opt%Jval_Ids', 2, RC )
       IF ( RC /= CC_SUCCESS ) RETURN
       Input_Opt%Jval_Ids => NULL()
    ENDIF
#endif

#if defined( ESMF_ )
    If (Associated(Input_Opt%lgr)) Input_Opt%lgr => NULL()
#endif

  END SUBROUTINE Cleanup_Input_Opt
!EOC
END MODULE Input_Opt_Mod

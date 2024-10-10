!> \file CCPr_Dust_mod.F90
!! \brief Driver for the CATCHem Process: Dust
!!
!! \defgroup catchem_dust_process
!!
!! The CATChem Dust Process group holds all the CATCHem dust processes.
!!!>
MODULE CCPr_Dust_mod

   ! USES:
   USE Precision_Mod
   USE Error_MOD
   USE State_mod
   USE CCPr_Dust_Common_Mod, Only : DustStateType

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: CCPR_Dust_Run
   PUBLIC :: CCPR_Dust_Init
   PUBLIC :: CCPR_Dust_Finalize

CONTAINS

   !>
   !! \brief Initialize the CATCHem Dust Process
   !!
   !! \param Config_Opt  CATCHem configuration options
   !! \param DustState   CATCHem dust state
   !! \param ChmState    CATCHem chemical state
   !! \param RC          Error return code
   !!
   !! \ingroup catchem_dust_process
   !!!>
   SUBROUTINE CCPR_Dust_Init( Config, DustState, ChemState, RC)
      ! USES

      IMPLICIT NONE

      ! INPUT PARAMETERS
      !-----------------
      TYPE(ConfigType),    intent(in)    :: Config     ! Config options
      TYPE(DustStateType), intent(inout) :: DustState  ! Nullify Dust State During INIT
      TYPE(ChemStateType), intent(in)    :: ChemState  ! Chemical State

      ! INPUT/OUTPUT PARAMETERS
      !------------------------
      INTEGER,          INTENT(INOUT) :: RC

      ! LOCAL VARIABLES
      !----------------
      Integer, parameter :: nDustBinsDefault = 5
      REAL(fp), DIMENSION(nDustBinsDefault), Parameter :: DefaultDustDensity     = (/ 2500.0, 2650.0, 2650.0, 2650.0, 2650.0 /)
      REAL(fp), DIMENSION(nDustBinsDefault), Parameter :: DefaultEffectiveRadius = (/ 0.73e-6, 1.4e-6, 2.4e-6, 4.5e-6, 8.0e-6 /)
      REAL(fp), DIMENSION(nDustBinsDefault), Parameter :: DefaultLowerBinRadius  = (/ 0.1e-6, 1.0e-6, 1.8e-6, 3.0e-6, 6.0e-6  /)
      REAL(fp), DIMENSION(nDustBinsDefault), Parameter :: DefaultUpperBinRadius  = (/ 1.0e-6, 1.8e-6, 3.0e-6, 6.0e-6, 10.0e-6  /)

      INTEGER :: k ! Loop Counter
      integer :: index ! index of dust species

      ! Error handling
      !---------------
      CHARACTER(LEN=255)    :: ErrMsg
      CHARACTER(LEN=255)    :: ThisLoc

      ! Initialize Error handling
      !--------------------------
      ErrMsg = ''
      ThisLoc = ' -> at CCPR_DUST_INIT (in process/dust/ccpr_dust_mod.F90)'

      ! Initialize
      !-----------
      if (Config%dust_activate) then

         ! Activate Dust Process
         !----------------------
         duststate%Activate = .true.

         ! Set number of dust species
         !---------------------------
         DustState%nDustSpecies = ChemState%nDust

         ! Set Scheme Options
         !-------------------
         if (Config%dust_scheme < 0) then ! not listed in config
            DustState%SchemeOpt = 1
         else
            duststate%SchemeOpt = Config%dust_scheme
         endif

         ! Set Drag Calculation Option
         !----------------------------
         if (Config%dust_drag_opt < 0) then ! not listed in config
            DustState%DragOpt = 1
         else
            duststate%DragOpt = Config%dust_drag_opt
         endif

         ! Set Moisture Calculation Option
         !--------------------------------
         if (Config%dust_moist_opt < 0) then ! not listed in config
            DustState%MoistOpt = 1
         else
            duststate%MoistOpt = Config%dust_moist_opt
         endif

         ! Set the Alpha Parameter
         !------------------------
         if (Config%dust_alpha < 0) then ! not listed in config
            DustState%AlphaScaleFactor = 1.0_fp
         else
            duststate%AlphaScaleFactor = Config%dust_alpha
         endif

         ! Set the beta scalefactor
         !-------------------------
         if (Config%dust_beta < 0) then ! not listed in config
            DustState%BetaScaleFactor = 1.0_fp
         else
            duststate%BetaScaleFactor = Config%dust_beta
         endif

         ! Set Horizontal Flux Calculation Option
         !---------------------------------------
         if (Config%dust_horizflux_opt < 0) then ! not listed i n config
            DustState%HorizFluxOpt = 1
         else
            duststate%HorizFluxOpt = Config%dust_horizflux_opt
         endif

         if (DustState%nDustSpecies == 0) then

            ! Set default bin properties for schemes that need them
            !------------------------------------------------------
            ALLOCATE(DustState%LowerBinRadius(nDustBinsDefault), STAT=RC)
            CALL CC_CheckVar('DustState%LowerBinRadius', 0, RC)
            IF (RC /= CC_SUCCESS) RETURN
            do k = 1, nDustBinsDefault
               DustState%LowerBinRadius(k) = DefaultLowerBinRadius(k)
            end do

            ALLOCATE(DustState%UpperBinRadius(nDustBinsDefault), STAT=RC)
            CALL CC_CheckVar('DustState%UpperBinRadius', 0, RC)
            IF (RC /= CC_SUCCESS) RETURN
            do k = 1, nDustBinsDefault
               DustState%UpperBinRadius(k) = DefaultUpperBinRadius(k)
            enddo

            ALLOCATE(DustState%EffectiveRadius(nDustBinsDefault), STAT=RC)
            CALL CC_CheckVar('DustState%EffectiveRadius', 0, RC)
            IF (RC /= CC_SUCCESS) RETURN
            do k = 1, nDustBinsDefault
               DustState%EffectiveRadius(k) = DefaultEffectiveRadius(k)
            end do

            ALLOCATE(DustState%DustDensity(nDustBinsDefault), STAT=RC)
            CALL CC_CheckVar('DustState%DustDensity', 0, RC)
            IF (RC /= CC_SUCCESS) RETURN
            do k = 1, nDustBinsDefault
               DustState%DustDensity(k) = DefaultDustDensity(k)
            end do

         else

            ! Dust Aerosols are present in ChmState
            !--------------------------------------
            print*, 'Using Dust bins from ChmState'
            ALLOCATE(DustState%LowerBinRadius(ChemState%nDust), STAT=RC)
            CALL CC_CheckVar('DustState%LowerBinRadius', 0, RC)
            IF (RC /= CC_SUCCESS) RETURN
            do k = 1, ChemState%nDust
               index = ChemState%DustIndex(k)
               DustState%LowerBinRadius(k) = ChemState%Species(index)%lower_radius
            end do

            ALLOCATE(DustState%UpperBinRadius(ChemState%nDust), STAT=RC)
            CALL CC_CheckVar('DustState%UpperBinRadius', 0, RC)
            IF (RC /= CC_SUCCESS) RETURN
            do k = 1, ChemState%nDust
               index = ChemState%DustIndex(k)
               DustState%UpperBinRadius(k) = ChemState%Species(index)%upper_radius
            enddo

            ALLOCATE(DustState%EffectiveRadius(ChemState%nDust), STAT=RC)
            CALL CC_CheckVar('DustState%EffectiveRadius', 0, RC)
            IF (RC /= CC_SUCCESS) RETURN
            do k = 1, ChemState%nDust
               index = ChemState%DustIndex(k)
               DustState%EffectiveRadius(k) = ChemState%Species(index)%radius
            end do

            ALLOCATE(DustState%DustDensity(ChemState%nDust), STAT=RC)
            CALL CC_CheckVar('DustState%DustDensity', 0, RC)
            IF (RC /= CC_SUCCESS) RETURN
            do k = 1, ChemState%nDust
               index = ChemState%DustIndex(k)
               DustState%DustDensity(k) = ChemState%Species(index)%density
            end do


         endif

      else

         DustState%Activate = .false.

      endif


   END SUBROUTINE CCPR_DUST_INIT

   !>
   !! \brief Run the dust scheme
   !!
   !! \param [IN] MetState The MetState object
   !! \param [INOUT] DiagState The DiagState object
   !! \param [INOUT] DustState The DustState object
   !! \param [INOUT] ChemState The ChemState object
   !! \param [OUT] RC Return code
   !!
   !! \ingroup catchem_dust_process
   !!!>
   SUBROUTINE CCPr_Dust_Run( MetState, DiagState, DustState, RC )

      ! USE
      USE CCPr_Scheme_Fengsha_Mod, ONLY: CCPr_Scheme_Fengsha  ! Fengsha Dust Scheme
      USE CCPr_Scheme_Ginoux_Mod,  ONLY: CCPr_Scheme_Ginoux   ! Ginoux Dust Scheme

      IMPLICIT NONE

      ! INPUT PARAMETERS
      !-----------------
      TYPE(MetStateType),  INTENT(IN) :: MetState       ! MetState Instance

      ! INPUT/OUTPUT PARAMETERS
      !------------------------
      TYPE(DiagStateType), INTENT(INOUT) :: DiagState   ! DiagState Instance
      TYPE(DustStateType), INTENT(INOUT) :: DustState   ! DustState Instance
      ! TYPE(ChemStateType), INTENT(INOUT) :: ChemState  ! ChemState Instance

      ! OUTPUT PARAMETERS
      !------------------
      INTEGER, INTENT(OUT) :: RC                         ! Return Code


      ! LOCAL VARIABLES
      !----------------
      CHARACTER(LEN=255) :: ErrMsg, thisLoc
      integer :: i ! horizontal loop counter
      integer :: b ! bin loop counter
      real :: airden ! air density
      real :: airden_arr(MetState%nLEVS)
      real :: total_flux
      real :: emis_per_bin(DustState%nDustSpecies)
      real :: soil_moisture_adj
      real :: soil_erosion_potential
      real :: USTAR_T
      real :: horiz_flux
      real :: drag_partition

      ! Initialize
      !-----------
      RC = CC_SUCCESS
      errMsg = ''
      thisLoc = ' -> at CCPr_Dust_Run (in process/dust/ccpr_dust_mod.F90)'

      total_flux = 0.0
      emis_per_bin(:) = 0.0
      USTAR_T = 0.0
      horiz_flux = 0.0
      soil_moisture_adj = 0.0
      soil_erosion_potential = 0.0
      drag_partition = 0.0

      if (DustState%Activate) then

         horizon_loop: do i = 1, MetState%nHORZ

            airden = MetState%AIRDEN(i,1)
            airden_arr = MetState%AIRDEN(i,1:MetState%nLEVS)

            ! Run the Dust Scheme
            !--------------------
            if (DustState%SchemeOpt == 1) then ! FENGSHA
               call CCPr_Scheme_Fengsha(DustState%nDustSpecies, &
                  MetState%DSOILTYPE(i),                        &
                  MetState%SSM(i),                              &
                  MetState%RDRAG(i),                            &
                  MetState%TSKIN(i),                            &
                  MetState%USTAR(i),                            &
                  MetState%USTAR_THRESHOLD(i),                  &
                  MetState%GWETTOP(i),                          &
                  MetState%z0(i),                               &
                  MetState%CLAYFRAC(i),                         &
                  MetSTate%SANDFRAC(i),                         &
                  airden,                                       &
                  MetState%FROCEAN(i),                          &
                  MetState%FRLANDICE(i),                        &
                  MetState%FRSNO(i),                            &
                  DustState%AlphaScaleFactor,                   &
                  DustState%BetaScaleFactor,                    &
                  DustState%EffectiveRadius,                    &
                  DustState%LowerBinRadius,                     &
                  DustState%UpperBinRadius,                     &
                  total_flux,                                   &
                  emis_per_bin,                                 &
                  USTAR_T,                                      &
                  soil_moisture_adj,                            &
                  horiz_flux,                                   &
                  drag_partition,                               &
                  soil_erosion_potential,                       &
                  RC,                                           &
                  MoistOpt=DustState%MoistOpt,                  &
                  DragOpt=DustState%DragOpt,                    &
                  HorizFluxOpt=DustState%HorizFluxOpt)

               if (RC /= CC_SUCCESS) then
                  errMsg = 'Error in CCPr_Scheme_Fengsha'
                  CALL CC_Error( errMsg, RC, thisLoc )
               endif

               DiagState%dust_total_flux(i) = total_flux
               DiagState%dust_emission_per_bin(i,:) = emis_per_bin
               DiagState%dust_effective_threshold(i) = USTAR_T
               DiagState%dust_horiz_flux(i) = horiz_flux
               DiagState%dust_soil_moisture_adj(i) = soil_moisture_adj
               DiagState%dust_soil_erosion_potential(i) = soil_erosion_potential
               DiagState%dust_drag_partition(i) = drag_partition

            else if (DustState%SchemeOpt == 2) then ! GINOUX
               call CCPr_Scheme_Ginoux(MetState%DSOILTYPE(i), &
                  MetState%SSM(i),                            &
                  MetState%TSKIN(i),                          &
                  MetState%FROCEAN(i),                        &
                  MetState%FRSNO(i),                          &
                  airden_arr,                       &
                  MetState%U10M(i),                           &
                  MetState%V10M(i),                           &
                  MetState%GWETTOP(i),                        &
                  DustState%AlphaScaleFactor,                 &
                  DustState%EffectiveRadius,                  &
                  DustState%DustDensity,                      &
                  total_flux,                                 &
                  emis_per_bin,                               &
                  RC)
               if (RC /= CC_SUCCESS) then
                  errMsg = 'Error in CCPr_Scheme_Ginoux'
                  CALL CC_Error( errMsg, RC, thisLoc )
               endif

               DiagState%dust_total_flux(i) = total_flux
               DiagState%dust_emission_per_bin(i,:) = emis_per_bin
            else
               errMsg =  'ERROR: Unknown dust scheme option'
               RC = CC_FAILURE
               CALL CC_Error( errMsg, RC, thisLoc )
               return
            endif

            total_flux = ZERO
            emis_per_bin = ZERO
            USTAR_T = ZERO
            horiz_flux = ZERO
            soil_moisture_adj = ZERO
            soil_erosion_potential = ZERO
            drag_partition = ZERO

         enddo horizon_loop
      endif

   END SUBROUTINE CCPr_Dust_Run

   !>
   !! \brief Finalize the dust scheme
   !!
   !! \param [INOUT] DustState The DustState object
   !! \param [OUT] RC Return code
   !!
   !! \ingroup catchem_dust_process
   !!!>
   SUBROUTINE CCPr_Dust_Finalize( DustState, RC )

      ! USE
      !----

      IMPLICIT NONE

      ! INPUT/OUTPUT PARAMETERS
      !------------------------
      TYPE(DustStateType), INTENT(INOUT) :: DustState ! DustState Instance
      INTEGER, INTENT(OUT) :: RC                       ! Return Code

      ! LOCAL VARIABLES
      !----------------
      CHARACTER(LEN=255) :: ErrMsg, thisLoc

      ! Initialize
      !-----------
      RC = CC_SUCCESS
      errMsg = ''
      thisLoc = ' -> at CCPr_Dust_Finalize (in process/dust/ccpr_dust.F90)'

      DEALLOCATE( DustState%LowerBinRadius, STAT=RC )
      CALL CC_CheckVar('DustState%LowerBinRadius', 0, RC)
      IF (RC /= CC_SUCCESS) RETURN

      DEALLOCATE( DustState%UpperBinRadius, STAT=RC )
      CALL CC_CheckVar('DustState%UpperBinRadius', 0, RC)
      IF (RC /= CC_SUCCESS) RETURN

      DEALLOCATE( DustState%EffectiveRadius, STAT=RC )
      CALL CC_CheckVar('DustState%EffectiveRadius', 0, RC)
      IF (RC /= CC_SUCCESS) RETURN

      DEALLOCATE( DustState%DustDensity, STAT=RC )
      CALL CC_CheckVar('DustState%DustDensity', 0, RC)
      IF (RC /= CC_SUCCESS) RETURN

      ! DEALLOCATE( DustState%EmissionPerSpecies, STAT=RC )
      ! CALL CC_CheckVar('DustState%EmissionPerSpecies', 0, RC)
      ! IF (RC /= CC_SUCCESS) RETURN

   END SUBROUTINE CCPr_Dust_Finalize

END MODULE CCPr_Dust_Mod

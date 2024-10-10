!>
!! \file chemstate_mod.F90
!! \brief Contains the `ChemStateType` data type and related subroutines and functions.
!!
!!
!! The `ChemState_Mod` module contains the chemstate_mod::chemstatetype data type
!! and related subroutines and functions for managing the state of the chemical model.
!!
!! \ingroup core_modules
!!!>
module ChemState_Mod
   !
   ! USES:
   !
   USE Error_Mod
   USE Precision_Mod
   ! use species_mod, only: SpeciesType

   IMPLICIT NONE
   PRIVATE
   !
   ! !PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: Chem_Allocate
   PUBLIC :: Find_Number_of_Species
   PUBLIC :: Find_Index_of_Species
   PUBLIC :: FindSpecByName
   PUBLIC :: GetSpecConc
   PUBLIC :: GetSpecConcByName
   PUBLIC :: GetSpecConcByIndex
   !
   ! !Private DATA MEMBERS:
   !
   !=========================================================================
   !

   !> \brief Data type for managing the state of the chemical model.
   !!
   !! \details chemStateType contains the following data members:
   !! \param State: A character string containing the name of this state.
   !! \param nSpecies: The total number of species.
   !! \param nSpeciesGas: The number of gas species.
   !! \param nSpeciesAero: The number of aerosol species.
   !! \param nSpeciesDust: The number of dust species.
   !! \param nSpeicesSeaSalt: The number of sea salt species.
   !! \param SpeciesIndex: An array containing the total species index.
   !! \param AeroIndex: An array containing the aerosol species index.
   !! \param GasIndex: An array containing the gas species index.
   !! \param DustIndex: An array containing the dust species index.
   !! \param SeaSaltIndex: An array containing the sea salt species index.
   !! \param Species: A 2-D array containing the concentration of each species.
   !!
   !! \ingroup core_modules
   !!!>
   type, public :: ChemStateType
      !---------------------------------------------------------------------
      ! Name of variables containing chemistry information
      !---------------------------------------------------------------------
      CHARACTER(LEN=4)  :: State     = 'Chem'    ! Name of this state

      !---------------------------------------------------------------------
      ! Integers
      !---------------------------------------------------------------------
      INTEGER              :: nSpecies          !< Total Number of Species
      INTEGER              :: nGas              !< Number of Gas Species
      INTEGER              :: nAero             !< Number of Aerosol Species
      INTEGER              :: nTracer           !< Number of Tracer Species
      INTEGER              :: nDust             !< Number of Dust Species
      INTEGER              :: nSeaSalt          !< Number of SeaSalt Species
      INTEGER, ALLOCATABLE :: SpeciesIndex(:)   !< Total Species Index
      INTEGER, ALLOCATABLE :: TracerIndex(:)    !< Tracer Species Index
      INTEGER, ALLOCATABLE :: AeroIndex(:)      !< Aerosol Species Index
      INTEGER, ALLOCATABLE :: GasIndex(:)       !< Gas Species Index
      INTEGER, ALLOCATABLE :: DustIndex(:)      !< Dust Species Index
      INTEGER, ALLOCATABLE :: SeaSaltIndex(:)   !< SeaSalt Species Index
      CHARACTER(len=50), ALLOCATABLE :: SpeciesNames(:)  !< Species Names

      !---------------------------------------------------------------------
      ! Reals
      !---------------------------------------------------------------------
      type(SpeciesType), allocatable :: Species(:)

   end type ChemStateType

   type, public :: SpeciesType

      ! Names
      character(len=30) :: long_name  !< long name for species used for netcdf attribute "long_name"
      character(len=30) :: short_name !< short name for species
      character(len=50) :: description !< description of species

      ! Logcial switches
      logical :: is_gas               !< if true, species is a gas and not an aerosol
      logical :: is_aerosol           !< if true, species is aerosol and not a gas
      logical :: is_tracer            !< if true, species is a tracer and not an aerosol or gas that undergoes chemistry or photolysis
      logical :: is_advected          !< if true, species is advected
      logical :: is_drydep            !< if true, species undergoes dry depotiion
      logical :: is_photolysis        !< if true, species undergoes photolysis
      logical :: is_gocart_aero       !< if true, species is a GOCART aerosol species
      logical :: is_dust              !< if true, species is a dust
      logical :: is_seasalt           !< if true, species is a seasalt

      ! Numerical properties
      real(kind=fp) :: mw_g                 !< gaseous molecular weight
      real(kind=fp) :: density              !< particle density (kg/m3)
      real(kind=fp) :: radius               !< mean molecular diameter in meters
      real(kind=fp) :: lower_radius         !< lower radius in meters
      real(kind=fp) :: upper_radius         !< upper radius in meters
      real(kind=fp) :: viscosity            !< kinematic viscosity (m2/s)

      ! Default background concentration
      real(kind=fp) :: BackgroundVV        !< Background conc [v/v]

      ! Indices
      integer :: species_index        !< species index in species array
      integer :: drydep_index         !< drydep index in drydep array
      integer :: photolysis_index     !< photolysis index in photolysis array
      integer :: gocart_aero_index    !< gocart_aero index in gocart_aero array

      ! Concentration
      real(kind=fp), ALLOCATABLE :: conc(:,:)             !< species concentration [v/v] or kg/kg

   end type SpeciesType

CONTAINS


   !> \brief Allocate the chem state
   !!
   !! \details Allocate the chem state.
   !!
   !! \ingroup core_modules
   !!
   !! \param MetState Grid State
   !! \param ChemState Chem State
   !! \param RC Return code
   !!
   !!!>
   subroutine Chem_Allocate(MetState, ChemState, RC)

      ! USES
      USE MetState_Mod,  ONLY : MetStateType
      USE Species_Mod,    Only : SpeciesType

      IMPLICIT NONE

      ! INOUT Params
      type(MetStateType), INTENT(in)    :: MetState ! Grid State object
      type(ChemStateType), INTENT(inout) :: ChemState ! chem State object
      ! type(SpeciesType),   POINTER       :: Species   ! Species object
      ! OUTPUT Params
      INTEGER,             INTENT(OUT)   :: RC            ! Success or failure

      ! Local
      integer :: i    ! Looping Variable

      ! Error handling
      CHARACTER(LEN=255) :: ErrMsg
      CHARACTER(LEN=255) :: thisLoc

      ! Initialize
      RC = CC_SUCCESS
      ErrMsg = ''
      thisLoc = ' -> at chem_Allocate (in core/chemstate_mod.F90)'

      ! Allocate
      ALLOCATE( ChemState%Species( ChemState%nSpecies ), STAT=RC )
      IF ( RC /= CC_SUCCESS ) THEN
         ErrMsg = 'Could not allocate ChemState%Species'
         CALL CC_Error( ErrMsg, RC, thisLoc )
      ENDIF
      CALL CC_CheckVar( 'ChemState%Species', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN

      do i=0, ChemState%nSpecies
         ALLOCATE(ChemState%Species(i)%conc(MetState%nHORZ, MetState%NLEVS), STAT=RC)
         IF ( RC /= CC_SUCCESS ) THEN
            ErrMsg = 'Could not Allocate ChemState%Species(i)%conc'
            CALL CC_Error( ErrMsg, RC, thisLoc )
         ENDIF
         ChemState%Species(i)%conc = TINY_
      end do

   end subroutine Chem_Allocate

   !> \brief Find the number of species
   !!
   !! This subroutine finds the number of species
   !!
   !! \param ChemState The ChemState object
   !! \param RC The return code
   !!
   !! \ingroup core_modules
   !!!>
   subroutine Find_Number_of_Species(ChemState, RC)
      ! USES
      USE Species_Mod,  ONLY :SpeciesType

      IMPLICIT NONE

      ! INOUT Params
      type(ChemStateType), INTENT(inout) :: ChemState     ! chem State object
      ! OUTPUT Params
      INTEGER,             INTENT(OUT)   :: RC            ! Success or failure

      ! Error handling
      CHARACTER(LEN=255) :: ErrMsg
      CHARACTER(LEN=255) :: thisLoc

      ! Local variables
      INTEGER :: i

      ! Initialize
      RC = CC_SUCCESS
      ErrMsg = ''
      thisLoc = ' -> at Find_Number_of_Species (in core/chemstate_mod.F90)'

      ! Initialize to zero before counting species
      ChemState%nAero = 0
      ChemState%nDust = 0
      ChemState%nGas = 0
      ChemState%nSeaSalt = 0
      ChemState%nTracer = 0

      ! Count number of species
      do i = 1, ChemState%nSpecies
         if (ChemState%Species(i)%is_gas .eqv. .true.) then
            ChemState%nGas = ChemState%nGas + 1
         endif
         if (ChemState%Species(i)%is_aerosol .eqv. .true.) then
            ChemState%nAero = ChemState%nAero + 1
         endif
         if (ChemState%Species(i)%is_dust .eqv. .true.) then
            ChemState%nDust = ChemState%nDust + 1
         endif
         if (ChemState%Species(i)%is_seasalt .eqv. .true.) then
            ChemState%nSeaSalt = ChemState%nSeaSalt + 1
         endif
         if (ChemState%Species(i)%is_tracer .eqv. .true.) then
            ChemState%nTracer = ChemState%nTracer + 1
         endif
      enddo

   end subroutine Find_Number_of_Species

   !> \brief Find the indices of species
   !!
   !! \param ChemState The ChemState object
   !! \param RC The return code
   !!
   !! \ingroup core_modules
   !!!>
   subroutine Find_Index_of_Species(ChemState, RC)
      ! USES
      USE Species_Mod,  ONLY : SpeciesType

      IMPLICIT NONE

      ! INOUT Params
      type(ChemStateType),  INTENT(INOUT) :: ChemState     ! chem State object
      ! OUTPUT Params
      INTEGER,             INTENT(OUT)   :: RC            ! Success or failure

      ! Error handling
      CHARACTER(LEN=255) :: ErrMsg
      CHARACTER(LEN=255) :: thisLoc

      ! Local variables
      integer :: n ! looping variable
      integer :: aero_index      ! Current Aerosol Index
      integer :: gas_index       ! Current Gas Index
      integer :: dust_index      ! Current Dust Index
      integer :: seasalt_index   ! Current Seas Salt Index
      integer :: tracer_index    ! Current Tracer Index


      ! Initialize
      RC = CC_SUCCESS
      ErrMsg = ''
      thisLoc = ' -> at Find_indices_of_Species (in core/chemstate_mod.F90)'


      ! Initialize to zero before counting species
      aero_index = 1
      gas_index = 1
      dust_index = 1
      seasalt_index = 1
      tracer_index = 1

      ! Allocate index arrays
      ALLOCATE(Chemstate%AeroIndex(ChemState%nAero), STAT=RC)
      IF ( RC /= CC_SUCCESS ) THEN
         errMsg = 'Error allocating Chemstate%AeroIndex'
         call CC_Error(errMsg, RC, thisLoc)
         RETURN
      ENDIF

      ALLOCATE(Chemstate%TracerIndex(ChemState%nTracer), STAT=RC)
      IF ( RC /= CC_SUCCESS ) THEN
         errMsg = 'Error allocating Chemstate%TracerIndex'
         call CC_Error(errMsg, RC, thisLoc)
         RETURN
      ENDIF

      ALLOCATE(Chemstate%GasIndex(ChemState%nGas), STAT=RC)
      IF ( RC /= CC_SUCCESS ) THEN
         errMsg = 'Error allocating Chemstate%GasIndex'
         call CC_Error(errMsg, RC, thisLoc)
         RETURN
      ENDIF

      ALLOCATE(Chemstate%DustIndex(ChemState%nDust), STAT=RC)
      IF ( RC /= CC_SUCCESS ) THEN
         errMsg = 'Error allocating Chemstate%DustIndex'
         call CC_Error(errMsg, RC, thisLoc)
         RETURN
      ENDIF

      ALLOCATE(Chemstate%SeaSaltIndex(ChemState%nSeaSalt), STAT=RC)
      IF ( RC /= CC_SUCCESS ) THEN
         errMsg = 'Error allocating Chemstate%SeaSaltIndex'
         call CC_Error(errMsg, RC, thisLoc)
         RETURN
      ENDIF

      ! Find indices for species groups
      do n = 1, ChemState%nSpecies
         if (ChemState%Species(n)%is_aerosol .eqv. .true.) then
            Chemstate%AeroIndex(aero_index) = n
            aero_index = aero_index + 1
         endif
         if (ChemState%Species(n)%is_gas .eqv. .true.) then
            Chemstate%GasIndex(gas_index) = n
            gas_index = gas_index + 1
         endif
         if (ChemState%Species(n)%is_dust .eqv. .true.) then
            Chemstate%DustIndex(dust_index) = n
            dust_index = dust_index + 1
         endif
         if (ChemState%Species(n)%is_seasalt .eqv. .true.) then
            Chemstate%SeaSaltIndex(seasalt_index) = n
            seasalt_index = seasalt_index + 1
         endif
         if (ChemState%Species(n)%is_tracer .eqv. .true.) then
            Chemstate%TracerIndex(tracer_index) = n
            tracer_index = tracer_index + 1
         endif
      enddo

   end subroutine Find_index_of_Species

   !> \brief Find the species by name
   !!
   !! \param ChemState The ChemState object
   !! \param name The name of the species
   !! \param index The index of the species
   !! \param RC The return code
   !!
   !! \ingroup core_modules
   !!!>
   subroutine FindSpecByName(ChemState, name, index, RC)

      type(ChemStateType),  INTENT(INOUT) :: ChemState     ! chem State object
      character(len=50),    INTENT(in)    :: name
      integer,              INTENT(out)   :: index
      integer,              INTENT(out)   :: RC

      ! Error handling
      CHARACTER(LEN=255) :: ErrMsg
      CHARACTER(LEN=255) :: thisLoc

      ! local variables
      integer :: n

      ! Initialize
      RC = CC_SUCCESS
      ErrMsg = ''
      thisLoc = ' -> at FindSpecByName (in core/chemstate_mod.F90)'

      index = 0
      do n = 1, ChemState%nSpecies
         if (TRIM(name) == TRIM(ChemState%SpeciesNames(n))) then
            index = n
            exit
         endif
      enddo

   end subroutine FindSpecByName


   !> \brief Get the concentration of a species
   !!
   !! get the concentration of a species given either the index or the name of the species
   !!
   !! \param ChemState The ChemState object
   !! \param concentration The concentration of the species
   !! \param RC The return code
   !! \param index The index of the species - Optional
   !! \param name The name of the species - Optional
   !!
   !! \ingroup core_modules
   !!!>
   subroutine GetSpecConc(ChemState, concentration, RC, index, name)

      type(ChemStateType),  INTENT(INOUT) :: ChemState     ! chem State object
      real(kind=fp), pointer, dimension(:,:), INTENT(out)   :: concentration
      integer,              INTENT(out)   :: RC
      integer, optional,    INTENT(inout)    :: index
      character(len=50), optional, INTENT(inout)    :: name

      ! Error handling
      CHARACTER(LEN=255) :: ErrMsg
      CHARACTER(LEN=255) :: thisLoc

      ! Initialize
      RC = CC_SUCCESS
      ErrMsg = ''
      thisLoc = ' -> at GetSpecConc (in core/chemstate_mod.F90)'

      if (present(index)) then
         call GetSpecConcByIndex(ChemState, concentration, index, RC)
      elseif (present(name)) then
         call GetSpecConcByName(ChemState, concentration, name, RC)
      else
         RC = CC_FAILURE
      endif

      if (RC /= CC_SUCCESS) then
         errMsg = 'Error in GetSpecConc'
         call CC_Error(errMsg, RC, thisLoc)
         RETURN
      endif

   end subroutine GetSpecConc

   !> \brief Get the concentration of a species by index
   !!
   !! \param ChemState The ChemState object
   !! \param concentration The concentration of the species
   !! \param RC The return code
   !! \param index The index of the species
   !!
   !! \ingroup core_modules
   !!!>
   subroutine GetSpecConcByIndex(ChemState, concentration, index, RC)

      type(ChemStateType),  INTENT(INOUT) :: ChemState     ! chem State object
      real(kind=fp), pointer, dimension(:,:), INTENT(out)   :: concentration
      integer,              INTENT(in)    :: index
      integer,              INTENT(out)   :: RC

      ! Error handling
      CHARACTER(LEN=255) :: ErrMsg
      CHARACTER(LEN=255) :: thisLoc

      ! Initialize
      RC = CC_SUCCESS
      ErrMsg = ''
      thisLoc = ' -> at GetSpecConcByIndex (in core/chemstate_mod.F90)'

      if (index < 1 .or. index > ChemState%nSpecies) then
         RC = CC_FAILURE
         errMsg = 'index out of bounds'
         call CC_Error(errMsg, RC, thisLoc)
         RETURN
      endif

      concentration = ChemState%Species(index)%conc

   end subroutine GetSpecConcByIndex

   !> \brief Get the concentration of a species by name
   !!
   !! \param ChemState The ChemState object
   !! \param concentration The concentration of the species
   !! \param RC The return code
   !! \param name The name of the species
   !!
   !! \ingroup core_modules
   !!!>
   subroutine GetSpecConcByName(ChemState, concentration, name, RC)

      type(ChemStateType),  INTENT(INOUT) :: ChemState     ! chem State object
      real(kind=fp), pointer, dimension(:,:), INTENT(out)   :: concentration
      character(len=50),    INTENT(in)    :: name
      integer,              INTENT(out)   :: RC

      ! Locals
      integer :: index

      ! Error handling
      CHARACTER(LEN=255) :: ErrMsg
      CHARACTER(LEN=255) :: thisLoc

      ! Initialize
      RC = CC_SUCCESS
      ErrMsg = ''
      thisLoc = ' -> at GetSpecConcByName (in core/chemstate_mod.F90)'

      call FindSpecByName(ChemState, name, index, RC)

      if (RC /= CC_SUCCESS) then
         errMsg = 'Error in GetSpecConcByName'
         call CC_Error(errMsg, RC, thisLoc)
         RETURN
      endif

      concentration = ChemState%Species(index)%conc

   end subroutine GetSpecConcByName

end module ChemState_Mod

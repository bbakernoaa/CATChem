!> \file
!! \brief Contains the `ChemStateType` data type and related subroutines and functions.
!!
!!
!! The `ChemState_Mod` module contains the chemstate_mod::chemstatetype data type
!! and related subroutines and functions for managing the state of the chemical model.
!!
!! \ingroup Core_Modules
!!!>
module ChemState_Mod
   !
   ! USES:
   !
   USE Error_Mod
   USE Precision_Mod

   IMPLICIT NONE
   PRIVATE
   !
   ! !PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: Chem_Allocate
   !
   ! !Private DATA MEMBERS:
   !
   !=========================================================================
   !

   !> \brief Data type for managing the state of the chemical model.
   !!
   !> \details chemStateType contains the following data members:
   !> - State: A character string containing the name of this state.
   !> - nSpecies: The total number of species.
   !> - nSpeciesGas: The number of gas species.
   !> - nSpeciesAero: The number of aerosol species.
   !> - nSpeciesDust: The number of dust species.
   !> - NSpeicesSeaSalt: The number of sea salt species.
   !> - SpeciesIndex: An array containing the total species index.
   !> - AeroIndex: An array containing the aerosol species index.
   !> - GasIndex: An array containing the gas species index.
   !> - DustIndex: An array containing the dust species index.
   !> - SeaSaltIndex: An array containing the sea salt species index.
   !> - chemSpecies: A 2-D array containing the concentration of each species.
   type, public :: ChemStateType
      !---------------------------------------------------------------------
      ! Name of variables containing chemistry information
      !---------------------------------------------------------------------
      CHARACTER(LEN=4)  :: State     = 'Chem'    ! Name of this state

      !---------------------------------------------------------------------
      ! Integers
      !---------------------------------------------------------------------
      INTEGER              :: nSpecies          !< Total Number of Species
      INTEGER              :: nSpeciesGas       !< Number of Gas Species
      INTEGER              :: nSpeciesAero      !< Number of Aerosol Species
      INTEGER              :: nSpeciesDust      !< Number of Dust Species
      INTEGER              :: nSpeciesSeaSalt   !< Number of SeaSalt Species
      INTEGER, ALLOCATABLE :: SpeciesIndex(:)   !< Total Species Index
      INTEGER, ALLOCATABLE :: AeroIndex(:)      !< Aerosol Species Index
      INTEGER, ALLOCATABLE :: GasIndex(:)       !< Gas Species Index
      INTEGER, ALLOCATABLE :: DustIndex(:)      !< Dust Species Index
      INTEGER, ALLOCATABLE :: SeaSaltIndex(:)   !< SeaSalt Species Index

      !---------------------------------------------------------------------
      ! Reals
      !---------------------------------------------------------------------
      REAL(fp), POINTER :: chemSpecies(:,:)

      ! TODO: Add properties for species
   end type ChemStateType

CONTAINS


   subroutine Chem_Allocate(Config, GridState, Species, ChemState, RC)

      ! USES
      USE Config_Opt_Mod, ONLY : ConfigType
      USE GridState_Mod,  ONLY : GridStateType
      USE Species_Mod,    Only : SpeciesType

      IMPLICIT NONE

      ! INOUT Params
      type(ConfigType),    INTENT(in)    :: Config    ! Input Options object
      type(GridStateType), INTENT(in)    :: GridState ! Grid State object
      type(ChemStateType), INTENT(inout) :: ChemState ! chem State object
      type(SpeciesType),   POINTER       :: Species   !Species object
      ! OUTPUT Params
      INTEGER,             INTENT(OUT)   :: RC            ! Success or failure

      ! Error handling
      CHARACTER(LEN=255) :: ErrMsg
      CHARACTER(LEN=255) :: thisLoc

      ! Initialize
      RC = CC_SUCCESS
      ErrMsg = ''
      thisLoc = ' -> at chem_Allocate (in core/chemstate_mod.F90)'

      ! Nullify all fields for safety's sake before allocating them
      ! This can prevent compilation errors caused by uninitialized values
      ! Nullify all fields for safety's sake before allocating them
      ! This can prevent compilation errors caused by uninitialized values
      ChemState%chemSpecies => NULL()

      ! Allocate
      ALLOCATE( ChemState%chemSpecies( GridState%number_of_levels, ChemState%nSpecies ), STAT=RC )
      CALL CC_CheckVar( 'ChemState%chemSpecies', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      ChemState%chemSpecies = TINY

      ! do other checks etc

   end subroutine Chem_Allocate

   !!
   !=========================================================================
   !
   !<b>Find Number of Species</b>
   !
   !<p>This subroutine loops through the Species object and counts the number of species</p>
   !
   subroutine Find_Number_of_Species(ChemState, Species, RC)
      ! USES
      USE Species_Mod,  ONLY :SpeciesType

      IMPLICIT NONE

      ! INOUT Params
      type(ChemStateType), INTENT(inout) :: ChemState     ! chem State object
      type(SpeciesType),   INTENT(INOUT) :: Species !Species object
      ! OUTPUT Params
      INTEGER,             INTENT(OUT)   :: RC            ! Success or failure

      ! Error handling
      CHARACTER(LEN=255) :: ErrMsg
      CHARACTER(LEN=255) :: thisLoc

      ! Initialize
      RC = CC_SUCCESS
      ErrMsg = ''
      thisLoc = ' -> at Find_Number_of_Species (in core/chemstate_mod.F90)'

      ! do nothing yet
      ! loop through Species etc and find the number of species


   end subroutine Find_Number_of_Species

   !
   !=========================================================================
   !
   subroutine Find_Index_of_Species(ChemState, Species, RC)
      ! USES
      USE Species_Mod,  ONLY : SpeciesType

      IMPLICIT NONE

      ! INOUT Params
      type(ChemStateType),  INTENT(INOUT) :: ChemState     ! chem State object
      type(SpeciesType),   INTENT(INOUT) :: Species ! Species object
      ! OUTPUT Params
      INTEGER,             INTENT(OUT)   :: RC            ! Success or failure

      ! Error handling
      CHARACTER(LEN=255) :: ErrMsg
      CHARACTER(LEN=255) :: thisLoc

      ! Initialize
      RC = CC_SUCCESS
      ErrMsg = ''
      thisLoc = ' -> at Find_Index_of_Species (in core/chemstate_mod.F90)'

      ! do nothing yet
      ! loop through Species etc and find the number of species


   end subroutine Find_Index_of_Species

end module ChemState_Mod

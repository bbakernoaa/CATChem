
module EmisState_Mod

   ! Uses

   USE Error_Mod
   USE Precision_Mod

   IMPLICIT NONE
   PRIVATE

   !> \brief Emission Species State
   !!
   !! \details The Emission Species States contain information about the emitted species and
   !!          the emitted fluxes, scale factors, and speciation to concentration mappings.
   !!
   !! \ingroup core_modules
   !!
   !! \param name Name of the species
   !! \param long_name Long name of the species
   !! \param units Units of the species
   !! \param nEmisMap Number of Emission mappings per emitted species
   !! \param EmisMapIndex Emission mapping to concentration index
   !! \param EmisScale Scale factor
   !! \param EmisFlux Emission flux
   !!
   !!!>
   TYPE :: EmisSpeciesType
      ! Character
      character(len=10)               :: name             !< Name of the species
      character(len=50)               :: long_name        !< Long name of the species
      character(len=10)               :: units            !< Units of the species
      character(len=100), allocatable :: EmisMapName(:)   !< Emission mapping to concentration species

      ! Integers
      integer              :: nEmisMap         !< Number of Emission mappings per emitted species
      integer, ALLOCATABLE :: EmisMapIndex(:)  !< Emission mapping to concentration index

      ! Real
      real(fp), ALLOCATABLE :: EmisScale(:)    !< Scale factor
      real(fp), ALLOCATABLE :: EmisFlux(:)     !< Emission flux

   END TYPE EmisSpeciesType

   !> \brief Emission Category State
   !!
   !! \details The Emission Category State contains all the EmisSpecies types for a given emission
   !!          category. It includes the name of the category, the number of emitted species, and
   !!          the emitted species containers.
   !!
   !! \param name Name of the category
   !! \param nEmisSpecies Number of emitted species
   !! \param EmisSpecies Emitted Species container
   !!
   !! \ingroup core_modules
   !!!>
   TYPE :: EmisCategoryType

      ! Character
      character(len=20)    :: name                         !< Name of the species

      ! Integers
      integer              :: nEmisSpecies                 !< Number of emitted species per process
      type(EmisSpeciesType), ALLOCATABLE :: EmisSpecies(:) !< Emitted species container

   END TYPE EmisCategoryType

   !>
   !! \brief Emission State
   !!
   !! \details The Emission State contains all the emission category containers for the simulation and
   !!          the number of emission categories.
   !!
   !! \ingroup core_modules
   !!
   !! \param State Name of the state
   !! \param nEmisCategories Number of emission categories
   !! \param EmisCats Emission category containers
   !!
   !!!>
   TYPE, PUBLIC :: EmisStateType

      ! Character
      CHARACTER(LEN=4) :: State = 'Emis' !< Name of this state

      ! Integers
      integer :: nEmisCategories         !< Number of emission categories

      ! Types
      type(EmisCategoryType), ALLOCATABLE :: EmisCats(:) !< Emission categories container

   END TYPE EmisStateType

END MODULE EmisState_Mod

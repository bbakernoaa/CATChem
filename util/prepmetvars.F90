module PrepMetVars

   implicit none

   private


CONTAINS


   !>
   !! \brief PrepMetVarsForGOCARTSUV - Prep the meteorological variables for GOCART SUVolcanicEmissions scheme
   !!
   !! \param [INOUT] metstate
   !! \param [INOUT] tmpu
   !! \param [INOUT] rhoa
   !! \param [INOUT] hghte
   !! \param [INOUT] oro
   !! \param [INOUT] ustar
   !! \param [INOUT] pblh
   !! \param [INOUT] shflux
   !! \param [INOUT] z0h
   !! \param [INOUT] u10m
   !! \param [INOUT] v10m
   !! \param [INOUT] fraclake
   !! \param [INOUT] gwettop
   !! \param [OUT] rc
   !!
   !! \ingroup core_modules
   !!!>

   ! Need to fix below subroutine to convert one variable at a time.

  subroutine PrepMetVarsForGOCARTSUV(km,        &
      delp,            &
      hghte,           &
      GOCART_delp,     &
      GOCART_HGHTE)


      IMPLICIT NONE

      ! INPUTS
      INTEGER, intent(in)                     :: km     ! number of vertical levels
      REAL,  intent(in), DIMENSION(:), target :: delp   ! Temperature [K]
      REAL,  intent(in), DIMENSION(:), target :: hghte  ! Height [m]

      ! INPUT/OUTPUTS
      REAL, intent(inout), pointer :: GOCART_DELP(:,:,:)   !< temperature [K]
      REAL, intent(inout), pointer, DIMENSION(:,:,:) :: GOCART_HGHTE  !< geometric height [m]

      ! OUTPUTS - Add error handling back in late
      !INTEGER :: rc !< Return code

      ! Error handling
      !character(len=255) :: thisloc

      allocate(GOCART_DELP(1, 1, km))
      allocate(GOCART_HGHTE(1, 1, km))

      GOCART_DELP(1,1,:) = delp ! temperature [K]
      GOCART_HGHTE(1,1,:) = hghte    ! top of layer geopotential height [m]


   end subroutine PrepMetVarsForGOCARTSUV


end module PrepMetVars
module PrepMetVars_Mod

   implicit none

   private

   public :: PrepMetVarsForGOCARTSUV
   public :: PrepAnyMetVarForGOCART

CONTAINS


   !>
   !! \brief PrepMetVarsForGOCARTSUV - Prep the meteorological variables for GOCART SUVolcanicEmissions scheme
   !!
   !! \param [INOUT] zmid
   !! \param [INOUT] pmid
   !!
   !! \ingroup core_modules
   !!!>

   ! Need to fix below subroutine to convert one variable at a time.

   subroutine PrepMetVarsForGOCARTSUV(km,        &
      delp,            &
      zbox,           &
      GOCART_DELP,     &
      GOCART_ZBOX)


      IMPLICIT NONE

      ! INPUTS
      INTEGER, intent(in)                     :: km     ! number of vertical levels
      REAL,  intent(in), DIMENSION(:), target :: delp   ! Temperature [K]
      REAL,  intent(in), DIMENSION(:), target :: zbox  ! Height [m]

      ! INPUT/OUTPUTS
      REAL, intent(inout), pointer :: GOCART_DELP(:,:,:)   !< temperature [K]
      REAL, intent(inout), pointer, DIMENSION(:,:,:) :: GOCART_ZBOX  !< geometric height [m]

      ! OUTPUTS - Add error handling back in late
      !INTEGER :: rc !< Return code

      ! Error handling
      !character(len=255) :: thisloc

      allocate(GOCART_DELP(1, 1, km))
      allocate(GOCART_ZBOX(1, 1, km))

      GOCART_DELP(1,1,:) = delp !  pressure  in middle of layer
      GOCART_ZBOX(1,1,:) = zbox    ! mid layer geopotential height [m]


   end subroutine PrepMetVarsForGOCARTSUV


   subroutine PrepAnyMetVarForGOCART(km,        &
      var,            &
      GOCART_VAR)

      IMPLICIT NONE

      ! INPUTS
      INTEGER, intent(in)                     :: km     ! number of vertical levels
      REAL, intent(in), DIMENSION(:), target :: var   ! any variable
      REAL, pointer, DIMENSION(:,:,:) :: GOCART_3D_VAR
      REAL, pointer, DIMENSION(:,:) :: GOCART_2D_VAR
      REAL, pointer, DIMENSION(:,:,:) :: GOCART_VAR


      if (km == 0 ) then
         ! point
         allocate(GOCART_2D_VAR(1, 1))
         Allocate(GOCART_VAR(1,1,1))
         GOCART_2D_VAR(1,1) = var(1)
         GOCART_2D_VAR => GOCART_VAR(:,:,1)
      else
         ! column
         allocate(GOCART_3D_VAR(1, 1, km))
         Allocate(GOCART_VAR(1,1,km))
         GOCART_3D_VAR(1,1,:) = var
         GOCART_3D_VAR => GOCART_VAR
      end if


   end subroutine PrepAnyMetVarForGOCART


end module PrepMetVars_Mod

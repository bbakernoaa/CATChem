MODULE CCPR_GOCART_AEROSOL_DIAGS_MOD

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: CCPR_GOCART_AERO_DIAGS


CONTAINS


   subroutine CCPR_GOCART_Aero_Diags( )

      ! Uses
      USE GOCART2G_Process, ONLY : Aero_Compute_Diags
      USE PrepMetVars_Mod

      IMPLICIT NONE

      TYPE(GOCART2G_Mie), INTENT(IN) :: mie        ! mie table
      INTEGER, INTENT(IN) :: km, nbegin, nbins
      INTEGER, INTENT(IN) :: klid
      REAL, INTENT(IN) :: g0
      REAL, DIMENSION(:), INTENT(IN) :: wavelengths_profile
      REAL, DIMENSION(:), INTENT(IN) :: wavelengths_vertint
      REAL, DIMENSION(:,:,:,:), INTENT(IN) :: aerosol

      REAL, ALLOCATABLE, DIMENSION(:) :: TMPU
      REAL, ALLOCATABLE, DIMENSION(:) :: RHOA
      REAL, ALLOCATABLE, DIMENSION(:) :: DELP
      REAL, ALLOCATABLE, DIMENSION(:) :: RH
      REAL, ALLOCATABLE, DIMENSION(:) :: UWND, VWND
      REAL, ALLOCATABLE, DIMENSION(:) :: PLE
      REAL :: TROPP

      REAL, POINTER, DIMENSION(:,:,:) :: GOCART_TMPU, GOCART_RHOA
      REAL, POINTER, DIMENSION(:,:,:) :: GOCART_DELP, GOCART_RH
      REAL, POINTER, DIMENSION(:,:,:) :: GOCART_U, GOCART_V
      REAL, POINTER, DIMENSION(:,:,:) :: GOCART_PLE
      REAL, POINTER, DIMENSION(:,:) :: GOCART_TROPP
 
      REAL :: sfcmass, colmass, mass, conc, sfcmass25, 
      REAL :: colmass25, aerindx
      REAL :: fluxu     ! Column mass flux in x direction
      REAL :: fluxv     ! Column mass flux in y direction
      REAL :: angstrom  ! 470-870 nm Angstrom parameter
      REAL, DIMENSION(:) :: exttau
      REAL, DIMENSION(:) :: stexttau
      REAL, DIMENSION(:) :: scatau
      REAL, DIMENSION(:) :: stscatau
      REAL, DIMENSION(:) :: mass25
      REAL, DIMENSION(:) :: exttau25
      REAL, DIMENSION(:) :: scatau25
      REAL, DIMENSION(:,:) :: extcoef   ! 3d ext. coefficient, 1/m
      REAL, DIMENSION(:,:) :: scacoef   ! 3d scat.coefficient, 1/m
      REAL, DIMENSION(:,:) :: bckcoef   ! 3d backscatter coefficient, m-1 sr-1
      REAL, DIMENSION(:) :: exttaufm  ! fine mode (sub-micron) ext. AOT at 550 nm
      REAL, DIMENSION(:) :: scataufm  ! fine mode (sub-micron) sct. AOT at 550 nm


      TYPE, PRIVATE :: ArgsType
        REAL, DIMENSION(:,:) :: sfcmass
        REAL, DIMENSION(:,:) :: colmass
        REAL, DIMENSION(:,:) :: mass
        REAL, DIMENSION(:,:) :: conc
        REAL, DIMENSION(:,:,:) :: exttau
        REAL, DIMENSION(:,:,:) :: stexttau
        REAL, DIMENSION(:,:,:) :: scatau
        REAL, DIMENSION(:,:,:) :: stscatau
        REAL, DIMENSION(:,:) :: sfcmass25
        REAL, DIMENSION(:,:) :: colmass25
        REAL, DIMENSION(:,:,:) :: mass25
        REAL, DIMENSION(:,:,:) :: exttau25
        REAL, DIMENSION(:,:,:) :: scatau25
        REAL, DIMENSION(:,:) :: aerindx
        REAL, DIMENSION(:,:) :: fluxu     ! Column mass flux in x direction
        REAL, DIMENSION(:,:) :: fluxv     ! Column mass flux in y direction
        REAL, DIMENSION(:,:,:,:) :: extcoef   ! 3d ext. coefficient, 1/m
        REAL, DIMENSION(:,:,:,:) :: scacoef   ! 3d scat.coefficient, 1/m
        REAL, DIMENSION(:,:,:,:) :: bckcoef   ! 3d backscatter coefficient, m-1 sr-1
        REAL, DIMENSION(:,:,:) :: exttaufm  ! fine mode (sub-micron) ext. AOT at 550 nm
        REAL, DIMENSION(:,:,:) :: scataufm  ! fine mode (sub-micron) sct. AOT at 550 nm
        REAL, DIMENSION(:,:)   :: angstrom  ! 470-870 nm Angstrom parameter
      END TYPE ArgsType



!   subroutine Aero_Compute_Diags( mie, km, klid, nbegin, nbins, rlow, rup, &
!                                  wavelengths_profile, wavelengths_vertint, aerosol, &
!                                  grav, tmpu, rhoa, rh, u, v, delp, ple, tropp, &
!                                  sfcmass, colmass, mass, exttau, stexttau, scatau, stscatau,&
!                                  sfcmass25, colmass25, mass25, exttau25, scatau25, &
!                                  fluxu, fluxv, conc, extcoef, scacoef, bckcoef,&
!                                  exttaufm, scataufm, angstrom, aerindx, NO3nFlag, rc )
!
!! !INPUT PARAMETERS:
!   type(GOCART2G_Mie),  intent(in) :: mie        ! mie table
!   integer, intent(in) :: km, nbegin, nbins
!   integer,    intent(in)    :: klid   ! index for pressure lid
!   real, optional, dimension(:), intent(in)    :: rlow   ! bin radii - low bounds
!   real, optional, dimension(:), intent(in)    :: rup    ! bin radii - upper bounds
!   real, dimension(:), intent(in)    :: wavelengths_profile
!   real, dimension(:), intent(in)    :: wavelengths_vertint
!   real, dimension(:,:,:,:), intent(in) :: aerosol     !
!   real, intent(in) :: grav
!   real, pointer, dimension(:,:,:), intent(in) :: tmpu  ! temperature [K]
!   real, pointer, dimension(:,:,:), intent(in) :: rhoa  ! air density [kg/m^3]
!   real, pointer, dimension(:,:,:), intent(in) :: delp  ! pressure thickness [Pa]
!   real, pointer, dimension(:,:,:), intent(in) :: rh    ! relative humidity [1]
!   real, pointer, dimension(:,:,:), intent(in) :: u     ! east-west wind [m/s]
!   real, pointer, dimension(:,:,:), intent(in) :: v     ! north-south wind [m/s]
!   real, pointer, dimension(:,:,:), intent(in) :: ple   ! level edge air pressure [Pa]
!   real, pointer, dimension(:,:), intent(in)   :: tropp ! tropopause pressure [Pa]
!   logical, optional, intent(in)               :: NO3nFlag
!! !OUTPUT PARAMETERS:
!!  Total mass
!   real, optional, dimension(:,:), intent(inout)   :: sfcmass   ! sfc mass concentration kg/m3
!   real, optional, dimension(:,:), intent(inout)   :: colmass   ! col mass density kg/m2
!   real, optional, dimension(:,:,:), intent(inout) :: mass      ! 3d mass mixing ratio kg/kg
!   real, optional, dimension(:,:,:), intent(inout) :: conc      ! 3d mass concentration, kg/m3
!!  Total optical properties
!   real, optional, dimension(:,:,:), intent(inout)   :: exttau    ! ext. AOT at 550 nm
!   real, optional, dimension(:,:,:), intent(inout)   :: stexttau  ! stratospheric ext. AOT at 550 nm
!   real, optional, dimension(:,:,:), intent(inout)   :: scatau    ! sct. AOT at 550 nm
!   real, optional, dimension(:,:,:), intent(inout)   :: stscatau  ! stratospheric sct. AOT at 550 nm
!   real, optional, dimension(:,:), intent(inout)   :: sfcmass25 ! sfc mass concentration kg/m3 (pm2.5)
!   real, optional, dimension(:,:), intent(inout)   :: colmass25 ! col mass density kg/m2 (pm2.5)
!   real, optional, dimension(:,:,:), intent(inout) :: mass25    ! 3d mass mixing ratio kg/kg (pm2.5)
!   real, optional, dimension(:,:,:), intent(inout)   :: exttau25  ! ext. AOT at 550 nm (pm2.5)
!   real, optional, dimension(:,:,:), intent(inout)   :: scatau25  ! sct. AOT at 550 nm (pm2.5)
!   real, optional, dimension(:,:),  intent(inout)  :: aerindx   ! TOMS UV AI
!   real, optional, dimension(:,:), intent(inout)   :: fluxu     ! Column mass flux in x direction
!   real, optional, dimension(:,:), intent(inout)   :: fluxv     ! Column mass flux in y direction
!   real, optional, dimension(:,:,:,:), intent(inout) :: extcoef   ! 3d ext. coefficient, 1/m
!   real, optional, dimension(:,:,:,:), intent(inout) :: scacoef   ! 3d scat.coefficient, 1/m
!   real, optional, dimension(:,:,:,:), intent(inout) :: bckcoef   ! 3d backscatter coefficient, m-1 sr-1
!   real, optional, dimension(:,:,:), intent(inout)   :: exttaufm  ! fine mode (sub-micron) ext. AOT at 550 nm
!   real, optional, dimension(:,:,:), intent(inout)   :: scataufm  ! fine mode (sub-micron) sct. AOT at 550 nm
!   real, optional, dimension(:,:), intent(inout)   :: angstrom  ! 470-870 nm Angstrom parameter
!   integer, optional, intent(out)   :: rc        ! Error return code:
!                                                 !  0 - all is well
!                                                 !  1 -



      CALL INCR_REAL_RANK3(tmpu, GOCART_TMPU)
      CALL INCR_REAL_RANK3(rhoa, GOCART_RHOA)
      CALL INCR_REAL_RANK3(delp, GOCART_DELP)
      CALL INCR_REAL_RANK3(rh, GOCART_RH)
      CALL INCR_REAL_RANK3(uwind, GOCART_U)
      CALL INCR_REAL_RANK3(vwind, GOCART_V)
      CALL INCR_REAL_RANK3(ple, GOCART_PLE)

      CALL INCR_REAL_RANK2(tropp, GOCART_TROPP)



!   subroutine Aero_Compute_Diags( mie, km, klid, nbegin, nbins, rlow, rup, &
!                                  wavelengths_profile, wavelengths_vertint, aerosol, &
!                                  grav, tmpu, rhoa, rh, u, v, delp, ple, tropp, &
!                                  sfcmass, colmass, mass, exttau, stexttau, scatau, stscatau,&
!                                  sfcmass25, colmass25, mass25, exttau25, scatau25, &
!                                  fluxu, fluxv, conc, extcoef, scacoef, bckcoef,&
!                                  exttaufm, scataufm, angstrom, aerindx, NO3nFlag, rc )



   end subroutine CCPR_GOCART_Aero_Diags


END MODULE CCPR_GOCART_AEROSOL_DIAGS_MOD





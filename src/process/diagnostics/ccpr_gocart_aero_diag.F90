

   subroutine GOCART_Aerosol_Diagnostics( )



   ! Uses
   USE GOCART2G_MIEMOD, only : GOCART2G_Mie

   IMPLICIT NONE


!   subroutine Aero_Compute_Diags( mie, km, klid, begin, nbins, rlow, rup, &
!                                  wavelengths_profile, wavelengths_vertint, aerosol, &
!                                  grav, tmpu, rhoa, rh, u, v, delp, ple,tropp, &
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
!
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

!



   end subroutine GOCART_Aerosol_Diagnostics




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
 
      REAL :: cc_sfcmass, cc_colmass, cc_mass, cc_conc, cc_sfcmass25, 
      REAL :: cc_colmass25, cc_aerindx
      REAL :: cc_fluxu     ! Column mass flux in x direction
      REAL :: cc_fluxv     ! Column mass flux in y direction
      REAL :: cc_angstrom  ! 470-870 nm Angstrom parameter
      REAL, DIMENSION(:) :: cc_exttau
      REAL, DIMENSION(:) :: cc_stexttau
      REAL, DIMENSION(:) :: cc_scatau
      REAL, DIMENSION(:) :: cc_stscatau
      REAL, DIMENSION(:) :: cc_mass25
      REAL, DIMENSION(:) :: cc_exttau25
      REAL, DIMENSION(:) :: cc_scatau25
      REAL, DIMENSION(:,:) :: cc_extcoef   ! 3d ext. coefficient, 1/m
      REAL, DIMENSION(:,:) :: cc_scacoef   ! 3d scat.coefficient, 1/m
      REAL, DIMENSION(:,:) :: cc_bckcoef   ! 3d backscatter coefficient, m-1 sr-1
      REAL, DIMENSION(:) :: cc_exttaufm  ! fine mode (sub-micron) ext. AOT at 550 nm
      REAL, DIMENSION(:) :: cc_scataufm  ! fine mode (sub-micron) sct. AOT at 550 nm
      LOGICAL :: cc_NO3nFlag

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
        LOGICAL :: NO3nFlag
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

      if (present(cc_sfcmass)) then
        CALL INCR_REAL_RANK2(cc_sfcmass, ArgsType%sfcmass)
      end if

      if (present(cc_colmass)) then
        CALL INCR_REAL_RANK2(cc_colmass, ArgsType%colmass)
      end if

      if (present(cc_mass)) then
        CALL INCR_REAL_RANK2(cc_mass, ArgsType%mass)
      end if

      if (present(cc_conc) then
        CALL INCR_REAL_RANK2(cc_conc, ArgsType%conc)
      end if

      if (present(cc_exttau)) then
        CALL INCR_REAL_RANK3(cc_exttau, ArgsType%exttau)
      end if

      if (present(cc_stextau)) then
        CALL INCR_REAL_RANK3(cc_stextau, ArgsType%stextau)
      end if

      if (present(cc_scatau)) then
        CALL INCR_REAL_RANK3(cc_scatau, ArgsType%scatau)
      end if

      if (present(cc_stscatau)) then
        CALL INCR_REAL_RANK3(cc_stscatau, ArgsType%stscatau)
      end if

      if (present(cc_aerindx)) then
        CALL INCR_REAL_RANK2(cc_aerindx, ArgsType%aerindx)
      end if

      if (present(cc_fluxu)) then
        CALL INCR_REAL_RANK2(cc_fluxu, ArgsType%fluxu)
      end if

      if (present(cc_fluxv)) then
        CALL INCR_REAL_RANK2(cc_fluxv, ArgsType%fluxv)
      end if

      if (present(cc_extcoef)) then
        CALL INCR_REAL_RANK4(cc_extcoef, ArgsType%extcoef)
      end if

      if (present(cc_scacoef)) then
        CALL INCR_REAL_RANK4(cc_scacoef, ArgsType%scacoef)
      end if

      if (present(cc_bckcoef)) then
        CALL INCR_REAL_RANK4(cc_bckcoef, ArgsType%bckcoef)
      end if

      if (present(cc_exttaufm)) then
        CALL INCR_REAL_RANK3(cc_exttaufm, ArgsType%exttaufm)
      end if

      if (present(cc_scataufm)) then
        CALL INCR_REAL_RANK3(cc_scataufm, ArgsType%scataufm)
      end if

      if (present(cc_angstrom)) then
        CALL INCR_REAL_RANK2(cc_angstrom, ArgsType%angstrom)
      end if
 
      if (present(CC_NO3nflag)) then
        ArgsType%NO3nFlag = cc_no3nflag
      end if

   
   CALL Aero_Compute_Diags( mie, km, klid, nbegin, nbins, rlow, rup, &
                                  wavelengths_profile, wavelengths_vertint, aerosol, &
                                  grav, tmpu, rhoa, rh, u, v, delp, ple, tropp, 
                                  sfcmass, colmass, mass, exttau, stexttau, scatau, stscatau,&
                                  sfcmass25, colmass25, mass25, exttau25, scatau25, &
                                  fluxu, fluxv, conc, extcoef, scacoef, bckcoef,&
                                  exttaufm, scataufm, angstrom, aerindx, NO3nFlag, rc )



   end subroutine CCPR_GOCART_Aero_Diags


END MODULE CCPR_GOCART_AEROSOL_DIAGS_MOD




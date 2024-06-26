program test_dust
   use CATChem, fp => cc_rk
   use testing_mod, only: assert
   implicit none

   type(ConfigType) :: Config
   type(ChemStateType) :: ChemState
   type(MetStateType) :: MetState
   type(DiagStateType) :: DiagState
   type(SeaSaltStateType) :: SeaSaltState
   type(GridStateType) :: GridState

   ! Integers
   INTEGER:: rc          ! Success or failure

   character(len=:), allocatable :: title

   ! Error handling
   CHARACTER(LEN=512) :: errMsg
   CHARACTER(LEN=255) :: thisLoc
   CHARACTER(LEN=18), PARAMETER :: configFile ='CATChem_config.yml'

   thisLoc = 'test_dust -> at read CATChem_Config.yml'
   errMsg = ''
   rc = CC_SUCCESS

   write(*,*) '   CCCCC      A     TTTTTTT   CCCCC  H'
   write(*,*) '  C          A A       T     C       H       CCCC   EEEE   M       M'
   write(*,*) '  C         AAAAA      T     C       HHHHH  C      E    E  M M   M M'
   write(*,*) '  C        A     A     T     C       H   H  C      E EE    M   M   M'
   write(*,*) '   CCCCC  A       A    T      CCCCC  H   H   CCCC   EEEEE  M       M'
   write(*,*) ''
   write(*,*) ''

   !----------------------------
   ! Test 1
   !----------------------------

   ! Read input file and initialize grid
   call cc_read_config(Config, GridState, ChemState, rc)
   if (rc /= CC_success) then
      errMsg = 'Error reading configuration file: ' // TRIM( configFile )
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   endif
   title = 'SeaSalt Test 1 | Read Config'
   write(*,*) 'title = ', title
   write(*,*) 'Config%seasalt_activate = ', Config%seasalt_activate
   write(*,*) 'Config%seasalt_scheme = ', Config%seasalt_scheme
   write(*,*) 'Config%seasalt_scalefactor = ', Config%seasalt_scalefactor
   write(*,*) 'Config%seasalt_weibull = ', Config%seasalt_weibull

   !----------------------------
   ! Test 2
   !----------------------------
   ! Set number of dust species to zero for now
   ChemState%nSpeciesSeaSalt = 0

   ! Meteorological State
   MetState%SST=300.0_fp
   MetState%FROCEAN = 1.0_fp
   MetState%FRSEAICE = 0.0_fp
   MetState%U10M = 20.0_fp
   MetState%V10M = 20.0_fp
   MetState%USTAR = 2.0_fp
   allocate(MetState%AIRDEN(1))
   MetState%AIRDEN = 1.2_fp  ! kg/m3

   title = "SeaSalt Test 2 | Test GEOS12 defaults"
   Config%seasalt_activate = .TRUE.

   SeaSaltState%SchemeOpt = 3

   call cc_seasalt_init(Config, SeaSaltState, ChemState, rc)
   if (rc /= CC_SUCCESS) then
      errMsg = 'Error in cc_seasalt_init'
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   end if

   call cc_seasalt_run(MetState, DiagState, SeaSaltState, ChemState, rc)
   if (rc /= CC_SUCCESS) then
      errMsg = 'Error in cc_seasalt_run'
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   end if

   call print_info(Config, SeaSaltState, MetState, title)
   call assert(SeaSaltState%TotalEmission > 0.0_fp, "Test GEOS12 SeaSalt Scheme")
   SeaSaltState%TotalEmission = 0.0_fp
   !-------------------------
   ! Test Gong03 Scheme
   !-------------------------
   title = "SeaSalt Test 2 | Test Gong03"
   SeaSaltState%SchemeOpt = 1

   call cc_seasalt_run(MetState, DiagState, SeaSaltState, ChemState, rc)
   if (rc /= CC_SUCCESS) then
      errMsg = 'Error in cc_seasalt_run'
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   end if

   call print_info(Config, SeaSaltState, MetState, title)
   call assert(SeaSaltState%TotalEmission > 0.0_fp, "Test GEOS12 SeaSalt Scheme")
   SeaSaltState%TotalEmission = 0.0_fp
   !-------------------------
   ! Test Gong03 Scheme
   !-------------------------
   title = "SeaSalt Test 3 | Test Gong97"
   SeaSaltState%SchemeOpt = 2

   call cc_seasalt_run(MetState, DiagState, SeaSaltState, ChemState, rc)
   if (rc /= CC_SUCCESS) then
      errMsg = 'Error in cc_seasalt_run'
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   end if

   call print_info(Config, SeaSaltState, MetState, title)
   call assert(SeaSaltState%TotalEmission > 0.0_fp, "Test Gong97 SeaSalt Scheme")
   SeaSaltState%TotalEmission = 0.0_fp

contains

   subroutine print_info(Config_, SeaSaltState_, MetState_, title_)
      type(ConfigType), intent(in) :: Config_
      type(MetStateType), intent(in) :: MetState_
      type(SeaSaltStateType), intent(in) :: SeaSaltState_
      character(len=*), intent(in) :: title_

      write(*,*) '======================================='
      write(*,*) title_
      write(*,*) '======================================='
      write(*,*) '*************'
      write(*,*) 'Configuration '
      write(*,*) '*************'
      write(*,*) 'SeaSaltState%activate = ', SeaSaltState_%activate
      write(*,*) 'SeaSaltState%SchemeOpt = ', SeaSaltState_%SchemeOpt
      write(*,*) 'SeaSaltState%SeaSaltScaleFactor = ', SeaSaltState_%SeaSaltScaleFactor
      write(*,*) 'MetState%FROCEAN = ', MetState_%FROCEAN
      write(*,*) 'MetState%FRSEAICE = ', MetState_%FRSEAICE
      write(*,*) 'MetState%U10M =', MetState_%U10M
      write(*,*) 'MetState%V10M =', MetState_%V10M
      write(*,*) 'MetState%USTAR =', MetState_%USTAR
      write(*,*) 'MetState%AIRDEN =', MetState_%AIRDEN
      write(*,*) 'SeaSaltState%TotalEmission = ', SeaSaltState_%TotalEmission

   end subroutine print_info

end program test_dust

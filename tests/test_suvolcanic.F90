program test_suvolcanic
   use CATChem, fp => cc_rk
   use testing_mod, only: assert
   use precision_mod, only: rae
   implicit none

   type(ConfigType) :: Config
   type(ChemStateType) :: ChemState
   type(MetStateType) :: MetState
   type(DiagStateType) :: DiagState
   type(SUVolcanicEmissionsStateType) :: SUVolcanicEmissionsState
   type(GridStateType) :: GridState
   type(EmisStateType) :: EmisState

   ! Integers
   INTEGER:: rc          ! Success or failure

   character(len=:), allocatable :: title
   integer :: i ! loop counter

   ! Error handling
   CHARACTER(LEN=512) :: errMsg
   CHARACTER(LEN=255) :: thisLoc
   CHARACTER(LEN=255), PARAMETER :: configFile ='Configs/Default/CATChem_config.yml'


   thisLoc = 'test_suvolcanic -> at read CATChem_Config.yml'
   errMsg = ''
   rc = CC_SUCCESS

   write(*,*) '   CCCCC      A     TTTTTTT   CCCCC  H'
   write(*,*) '  C          A A       T     C       H        EEEE   M       M'
   write(*,*) '  C         AAAAA      T     C       HHHHH   E    E  M M   M M'
   write(*,*) '  C        A     A     T     C       H   H   E EE    M   M   M'
   write(*,*) '   CCCCC  A       A    T      CCCCC  H   H    EEEEE  M       M'
   write(*,*) ''
   write(*,*) ''

   !----------------------------
   ! Test 1
   !----------------------------

   ! Read input file and initialize grid
   call cc_read_config(Config, GridState, EmisState, ChemState, rc, configFile)
   if (rc /= CC_success) then
      errMsg = 'Error reading configuration file: ' // TRIM( configFile )
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   endif


   title = 'Volcanic Test 1 | Read Config'
   SUVolcanicEmissions%Activate = .false.
   call print_info(Config, SUVolcanicEmissionsState, MetState, title)
   write (*,*) '-- '
   write (*,*) 'Completed ', title
   write (*,*) '--'

   !----------------------------
   ! Test 2
   !----------------------------
   ! Set number of Volcanic species

   ChemState%nSpeciesSUVolcanic = 2
   SUVolcanicEmissionsState%Activate = .true.

   ! Meteorological State
   allocate(MetState%DELP(MetState%NLEVS))
   allocate(MetState%ZMID(MetState%NLEVS))


   do i = 1, MetState%NLEVS
      MetState%DELP(i)=101300 - I*1000       ! Need to change to something more reasonable and check units.
      MetState%ZMID(i) = (MetState%NLEVS*100 - I*100)   ! m
   end do

   SUVolcanicEmissionsState%SchemeOpt = 1
  

   ! Allocate DiagState
   call cc_allocate_diagstate(Config, DiagState, ChemState, RC)
   if (rc /= CC_SUCCESS) then
      errMsg = 'Error in cc_allocate_diagstate'
      stop 1
   endif

   title = "SUVolcanic Test 2 | Test GOCART SUVolcanic defaults"

   call cc_drydep_init(Config, DryDepState, ChemState, rc)
   if (rc /= CC_SUCCESS) then
      errMsg = 'Error in cc_drydep_init'
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   end if

   ! commenting out for now
   call cc_suvolcanicemissions_run(MetState, DiagState, DryDepState, ChemState, rc)
   if (rc /= CC_SUCCESS) then
      errMsg = 'Error in _suvolcanicemissions_run'
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   end if

   call print_info(Config, SUVolcanicEmissionsState, MetState, title)
   ! Need to update below 10/29/2024
 !  call assert(DiagState%drydep_frequency(1) > 0.0_fp, "Test GOCART DryDep Scheme (no resuspension)")


contains

   subroutine print_info(Config_, SUVolcanicEmissionsState_, MetState_, title_)

      type(ConfigType), intent(in) :: Config_
      type(MetStateType), intent(in) :: MetState_
      type(DryDepStateType), intent(in) :: DryDepState_
      character(len=*), intent(in) :: title_

      write(*,*) '======================================='
      write(*,*) title_
      write(*,*) '======================================='
      write(*,*) '*************'
      write(*,*) 'Configuration '
      write(*,*) '*************'
      write(*,*) 'Config%suvolcanicemissions_activate = ', Config_%suvolcanicemissions_activate
      write(*,*) 'Config%suvolcanicemissions_scheme = ', Config_%suvolcanicemissions_scheme
    

      if (SUVolcanicEmissionsState_%Activate) then

         write(*,*) 'SUVolcanicEmissionsState%Activate = ', SUVolcanicEmissionsState_%Activate
         write(*,*) 'SUVolcanicEmissionsState%SchemeOpt = ', SUVolcanicEmissionsState_%SchemeOpt

!!! Change below
         !write(*,*) 'MetState%AIRDEN =', MetState_%AIRDEN
         !write(*,*) 'DryDepState%drydepf = ', DryDepState_%drydep_frequency

      end if

   end subroutine print_info


end program test_suvolcanic
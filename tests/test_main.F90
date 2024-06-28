!> \file test_main.F90
!! \brief Main program for testing CATChem
!!
!!!>
program test_main
   use CATChem
   use state_mod  ! FIXME: declare states here or move to a driver

   IMPLICIT NONE

   ! Integers
   INTEGER:: rc          ! Success or failure

   ! Error handling
   CHARACTER(LEN=512) :: errMsg
   CHARACTER(LEN=255) :: thisLoc
   CHARACTER(LEN=18), PARAMETER :: configFile = 'CATChem_config.yml'
   ! set thisLoc
   thisLoc = 'test_main::test_main() -> at read CATChem_Conifg.yml'
   errMsg = ''
   rc = CC_SUCCESS

   write(*,*) '   CCCCC      A     TTTTTTT   CCCCC  H'
   write(*,*) '  C          A A       T     C       H       CCCC   EEEEE  M       M'
   write(*,*) '  C         AAAAA      T     C       HHHHH  C      E    E  M M   M M'
   write(*,*) '  C        A     A     T     C       H   H  C      E EE    M   M   M'
   write(*,*) '   CCCCC  A       A    T      CCCCC  H   H   CCCC   EEEEE  M       M'
   write(*,*) ''
   write(*,*) ''

   ! Read input file and initialize grid
   call cc_read_config(Config, GridState, EmisState, ChemState, rc)
   if (rc /= CC_SUCCESS) then
      errMsg = 'Error reading configuration file: ' // TRIM( configFile )
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   endif

   ! call cc_read_species(Config, rc)

   ! write grid info
   write(*,*) 'Grid info:'
   write(*,*) 'Number of grid nx = ', GridState%NX
   write(*,*) 'Number of grid ny = ', GridState%NY
   write(*,*) 'Number of grid levels = ', GridState%number_of_levels

   ! initialize met
   call cc_init_met(GridState, MetState, rc)
   if (rc /= CC_SUCCESS) then
      errMsg = 'Error initializing meteorology'
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   endif

   write(*,*) 'Finished CATChem'

end program test_main

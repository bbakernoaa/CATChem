!> \file test_main.F90
!! \brief Main program for testing CATChem
!!
!!!>
program test_main
   use CATChem
   use state_mod  ! FIXME: declare states here or move to a driver
   use testing_mod, only: assert_close

   IMPLICIT NONE

   ! Integers
   INTEGER:: rc          ! Success or failure

   ! Local variables
   integer :: index
   CHARACTER(len=50), PARAMETER :: DUST1 = 'dust1'
   CHARACTER(len=50), PARAMETER :: DUST2 = 'dust2'

   ! Error handling
   CHARACTER(LEN=512) :: errMsg
   CHARACTER(LEN=255) :: thisLoc
   CHARACTER(LEN=255), PARAMETER :: configFile = 'Configs/Default/CATChem_config.yml'
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
   call cc_read_config(Config, GridState, EmisState, ChemState, rc, configFile)
   if (rc /= CC_SUCCESS) then
      errMsg = 'Error reading configuration file: ' // TRIM( configFile )
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   endif

   ! Check Species names and idnex numbers for consistency
   print *, 'Checking Species names and idnex numbers for consistency'
   call cc_find_species_by_name(ChemState, DUST1, index, RC)
   if (rc /= CC_SUCCESS) then
      errMsg = 'Error finding species index: ' // TRIM( DUST1 )
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   endif
   if (index /= 1) then
      errMsg = 'Error: index for ' // TRIM( DUST1 ) // ' is not 1'
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   endif

   ! Check Species names and idnex numbers for consistency
   print *, 'Checking Species cc_find_species_by_name'
   call cc_find_species_by_name(ChemState, DUST2, index, RC)
   if (rc /= CC_SUCCESS) then
      errMsg = 'Error finding species index: ' // TRIM( DUST1 )
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   endif
   if (index /= 2) then
      errMsg = 'Error: index for ' // TRIM( DUST2 ) // ' is not 2'
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   endif

   ! Ensure dust1 is an aerosol and is_dust
   if (.not. ChemState%ChemSpecies(1)%is_aerosol) then
      errMsg = 'Error: dust1 is not an aerosol'
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   endif
   if (.not. ChemState%ChemSpecies(1)%is_dust) then
      errMsg = 'Error: dust1 is not a dust'
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   endif
   if (ChemState%ChemSpecies(1)%is_seasalt) then
      errMsg = 'Error: dust2 is categorized as seasalt'
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   endif
   if (ChemState%ChemSpecies(1)%is_gas) then
      errMsg = 'Error: dust2 is categorized as gas'
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   endif

   ! Check numerical values of dust1
   call assert_close(ChemState%ChemSpecies(1)%lower_radius, 0.1_fp, msg="dust1 lower radius")
   call assert_close(ChemState%ChemSpecies(1)%upper_radius, 1.0_fp, msg="dust1 upper radius")
   call assert_close(ChemState%ChemSpecies(1)%radius, 0.8_fp, msg="dust1 radius")
   call assert_close(ChemState%ChemSpecies(1)%density, 2500.0_fp, msg="dust1 density")

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

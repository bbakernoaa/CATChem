program test_plumerise
   use CATChem, fp => cc_rk
   use testing_mod, only: assert_close, load_column_data
   use state_mod

   implicit none

   type(PlumeriseStateType) :: PlumeriseState

   integer :: rc          ! Success or failure
   integer :: c, s, p
   real, parameter,dimension(4) :: CO_PLMRISE = (/ 808.429, 1261.015, 2576.692, 4026.496  /)
   real, parameter,dimension(1) :: ISOP_PLMRISE = (/ 723.005 /)
   integer :: index

   ! test_frp = (/10.0e6, 100.0e6, 500.0e6, 1000.0e6/)
   ! test_sofieve_emis = (/10.0e6, 100.0e6, 500.0e6, 1000.0e6/)

   CHARACTER(LEN=255), PARAMETER :: configFile ='Configs/Plumerise/CATChem_config.yml'

   ! Error handling
   CHARACTER(LEN=255) :: errMsg
   CHARACTER(LEN=255) :: thisLoc

   thisLoc = 'test_plumerise -> at read CATChem_Config.yml'
   errMsg = ''
   rc = CC_SUCCESS

   write(*,*) '   CCCCC      A     TTTTTTT   CCCCC  H'
   write(*,*) '  C          A A       T     C       H       CCCC   EEEE   M       M'
   write(*,*) '  C         AAAAA      T     C       HHHHH  C      E    E  M M   M M'
   write(*,*) '  C        A     A     T     C       H   H  C      E EE    M   M   M'
   write(*,*) '   CCCCC  A       A    T      CCCCC  H   H   CCCC   EEEEE  M       M'
   write(*,*) ''
   write(*,*) '  PLUMERISE TEST'

   MetState%nHORZ = 1
   MetState%nLEVS = 127
   call cc_init_met(MetState, rc)
   if (rc /= CC_SUCCESS) then
      errMsg = 'Error initializing meteorology'
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   endif

   ! Read input file and initialize grid
   call cc_read_config(Config, MetState, EmisState, ChemState, rc, configFile)
   if (rc /= CC_success) then
      errMsg = 'Error reading configuration file: ' // TRIM( configFile )
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   endif

   ! Allocate MetState
   MetState%nSOIL = 4
   print*, 'Allocating MetState'
   call cc_allocate_metstate(MetState, rc)
   if (rc /= CC_success) then
      errMsg = 'Error in "cc_allocate_metstate"'
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   endif
   print*, 'Allocated MetState'

   ! Meteorological State
   call load_column_data("MetProfiles/Profile_NCWCP_Aug_18z.csv", MetState, rc, verbose=.True.)
   if (rc /= CC_success) then
      errMsg = 'Error in "load_column_data"'
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   endif

   ! Allocate EmisState
   call cc_allocate_emisstate(MetState,EmisState, rc)
   if (rc /= CC_success) then
      errMsg = 'Error in "cc_allocate_emisstate"'
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   endif

   ! Allocate EmisState with FRP
   do c = 1, EmisState%nCats
      if (EmisState%Cats(c)%nPlumerise /= 0) then
         do s = 1, EmisState%Cats(c)%nSpecies
            print*, '  Species: ', EmisState%Cats(c)%Species(s)%name
            print*, '    Plumerise opt: ', EmisState%Cats(c)%Species(s)%plumerise
            if (EmisState%Cats(c)%Species(s)%plumerise == 1) then

               EmisState%Cats(c)%Species(s)%nPlmSrc = 4
               allocate(EmisState%Cats(c)%Species(s)%PlmSrcFlx(1,EmisState%Cats(c)%Species(s)%nPlmSrc), stat=rc)
               if (rc /= CC_success) then
                  errMsg = 'Error in allocate for EmisState%Cats(c)%Species(s)%PlmSrcFlx'
                  call cc_emit_error(errMsg, rc, thisLoc)
                  stop 1
               endif
               allocate(EmisState%Cats(c)%Species(s)%FRP(EmisState%Cats(c)%Species(s)%nPlmSrc), stat=rc)
               if (rc /= CC_success) then
                  errMsg = 'Error in allocate for EmisState%Cats(c)%Species(s)%FRP'
                  call cc_emit_error(errMsg, rc, thisLoc)
                  stop 1
               endif
               allocate(EmisState%Cats(c)%Species(s)%PlmRiseHgt(1,EmisState%Cats(c)%Species(s)%nPlmSrc), stat=rc)
               if (rc /= CC_success) then
                  errMsg = 'Error in allocate for EmisState%Cats(c)%Species(s)%FRP'
                  call cc_emit_error(errMsg, rc, thisLoc)
                  stop 1
               endif
               ! do n = 1, EmisState%Cats(c)%Species(s)%nPlmSrc

               EmisState%Cats(c)%Species(s)%FRP(:) = (/10.0e6, 100.0e6, 500.0e7, 1000.0e10/)
               EmisState%Cats(c)%Species(s)%PlmSrcFlx(1,:) = (/10., 100., 500., 1000./)

            else if (EmisState%Cats(c)%Species(s)%plumerise == 2) then

               EmisState%Cats(c)%Species(s)%nPlmSrc = 1
               allocate(EmisState%Cats(c)%Species(s)%PlmSrcFlx(1,EmisState%Cats(c)%Species(s)%nPlmSrc), stat=rc)
               if (rc /= CC_success) then
                  errMsg = 'Error in allocate for EmisState%Cats(c)%Species(s)%PlmSrcFlx'
                  call cc_emit_error(errMsg, rc, thisLoc)
                  stop 1
               endif
               allocate(EmisState%Cats(c)%Species(s)%STKDM(EmisState%Cats(c)%Species(s)%nPlmSrc), stat=rc)
               if (rc /= CC_success) then
                  errMsg = 'Error in allocate for EmisState%Cats(c)%Species(s)%STKDM'
                  call cc_emit_error(errMsg, rc, thisLoc)
                  stop 1
               endif
               allocate(EmisState%Cats(c)%Species(s)%STKHT(EmisState%Cats(c)%Species(s)%nPlmSrc), stat=rc)
               if (rc /= CC_success) then
                  errMsg = 'Error in allocate for EmisState%Cats(c)%Species(s)%STKHT'
                  call cc_emit_error(errMsg, rc, thisLoc)
                  stop 1
               endif
               allocate(EmisState%Cats(c)%Species(s)%STKTK(EmisState%Cats(c)%Species(s)%nPlmSrc), stat=rc)
               if (rc /= CC_success) then
                  errMsg = 'Error in allocate for EmisState%Cats(c)%Species(s)%STKTK'
                  call cc_emit_error(errMsg, rc, thisLoc)
                  stop 1
               endif
               allocate(EmisState%Cats(c)%Species(s)%STKVE(EmisState%Cats(c)%Species(s)%nPlmSrc), stat=rc)
               if (rc /= CC_success) then
                  errMsg = 'Error in allocate for EmisState%Cats(c)%Species(s)%STKVE'
                  call cc_emit_error(errMsg, rc, thisLoc)
                  stop 1
               endif
               allocate(EmisState%Cats(c)%Species(s)%PlmRiseHgt(1,EmisState%Cats(c)%Species(s)%nPlmSrc), stat=rc)
               if (rc /= CC_success) then
                  errMsg = 'Error in allocate for EmisState%Cats(c)%Species(s)%FRP'
                  call cc_emit_error(errMsg, rc, thisLoc)
                  stop 1
               endif
               EmisState%Cats(c)%Species(s)%STKDM = (/30.0/)
               EmisState%Cats(c)%Species(s)%STKHT = (/10.0/)
               EmisState%Cats(c)%Species(s)%STKTK = (/273.0/)
               EmisState%Cats(c)%Species(s)%STKVE = (/10.0/)
               EmisState%Cats(c)%Species(s)%PlmSrcFlx(1,:) = (/10./)

            endif
         enddo
      endif
   enddo

   ! Plumerise init
   call cc_plumerise_init(PlumeriseState, rc)
   if (rc /= CC_success) then
      errMsg = 'Error in "cc_plumerise_init"'
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   endif

   ! Plumerise run
   call cc_plumerise_run(PlumeriseState, rc, verbose=.true.)
   if (rc /= CC_success) then
      errMsg = 'Error in "cc_plumerise_run"'
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   endif

   ! check plumerise output
   index = 1
   cats2: do c = 1, EmisState%nCats
      species2: do s = 1, EmisState%Cats(c)%nSpecies

         plume2: do p = 1, EmisState%Cats(c)%Species(s)%nPlmSrc
            if (EmisState%Cats(c)%Species(s)%plumerise > 0) then
               if (TRIM(EmisState%Cats(c)%Species(s)%name) == 'CO') then
                  call assert_close(EmisState%Cats(c)%Species(s)%PlmRiseHgt(1,p), CO_PLMRISE(index), 1.e-3)
                  index = index + 1
               else if (TRIM(EmisState%Cats(c)%Species(s)%name) == 'ISOP') then
                  call assert_close(EmisState%Cats(c)%Species(s)%PlmRiseHgt(1,p), ISOP_PLMRISE(1), 1.e-3)
               endif
            endif

         end do plume2
      end do species2
   end do cats2

end program test_plumerise

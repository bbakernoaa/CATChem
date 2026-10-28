!> Runs adapter mapping cases with a context sized for the manifest catalog.
program test_ccpp_property_mapping_main
   use iso_c_binding, only: c_int
   use CATChem_Context_Registry, only: ccpp_context_handle, ccpp_context_create, &
      ccpp_context_bind_manifest, ccpp_context_manifest_species_count, ccpp_context_destroy, CCPP_CTX_OK
   use test_ccpp_property_mapping, only: run_ccpp_property_mapping_cases
   implicit none
   type(ccpp_context_handle) :: probe, context
   integer(c_int) :: status
   integer :: species_count
   logical :: passed

   call ccpp_context_create(1, 1, 1, probe, status)
   call require(status == CCPP_CTX_OK, 'create manifest probe context')
   call ccpp_context_bind_manifest(probe, 'CATChem_nuopc_lifecycle_config.yml', status)
   call require(status == CCPP_CTX_OK, 'bind manifest probe context')
   call ccpp_context_manifest_species_count(probe, species_count, status)
   call require(status == CCPP_CTX_OK .and. species_count > 0, 'read manifest species extent')
   call ccpp_context_destroy(probe, status)
   call require(status == CCPP_CTX_OK, 'destroy manifest probe context')

   call ccpp_context_create(1, 1, species_count, context, status)
   call require(status == CCPP_CTX_OK, 'create manifest-sized property context')
   call ccpp_context_bind_manifest(context, 'CATChem_nuopc_lifecycle_config.yml', status)
   call require(status == CCPP_CTX_OK, 'bind property context')
   call run_ccpp_property_mapping_cases(context, passed)
   call require(passed, 'CCPP property mapping conformance cases')
   call ccpp_context_destroy(context, status)
   call require(status == CCPP_CTX_OK, 'destroy property context')
   print *, 'PASS test_ccpp_property_mapping'
contains
   !> Fail the test with a concise assertion message.
   !! @param condition True when the assertion passes.
   !! @param message Description of the required behavior.
   subroutine require(condition, message)
      logical, intent(in) :: condition
      character(len=*), intent(in) :: message
      if (.not. condition) then
         print *, 'FAIL: ', trim(message)
         stop 1
      end if
   end subroutine require
end program test_ccpp_property_mapping_main

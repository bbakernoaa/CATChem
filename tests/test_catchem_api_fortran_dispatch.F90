program test_catchem_api_fortran_dispatch
   use iso_c_binding, only: c_int, c_int32_t, c_int64_t, c_long_long, c_double
   use CATChem_API
   implicit none
   type(CATChem_Model) :: model
   type(CATChem_Manifest) :: manifest
   type(CATChem_Manifest_Species_v1) :: species
   type(CATChem_Host_Interval_v1) :: interval
   type(CATChem_Process_Contract_v1) :: contract
   type(CATChem_Process_Field_v1) :: field
   integer :: rc
   integer(c_int) :: operation_status, outcome_status, process_index, state_classification
   integer(c_int64_t) :: generation
   integer(c_long_long) :: timestep, import_generation
   logical :: publication_eligible
   character(len=512) :: process_name, field_name, units
   character(len=512) :: config
   integer :: species_count

   config = 'CATChem_nuopc_lifecycle_config.yml'
   call manifest%open(trim(config), rc)
   call require(rc == 0, 'Fortran manifest open')
   call manifest%species_count(species_count, rc)
   call require(rc == 0 .and. species_count > 0, 'Fortran manifest species discovery')
   call manifest%get_species(1, species, process_name, field_name, units, rc)
   call require(rc == 0 .and. len_trim(process_name) > 0 .and. species%name_required_len > 1, &
      'Fortran manifest species descriptor')
   call manifest%close(rc)
   call require(rc == 0, 'Fortran manifest close')
   call model%initialize(trim(config), 2, 1, 5, rc=rc)
   call require(rc == 0, 'model initialization')
   call model%get_process_contract(1, contract, process_name, rc)
   call require(rc == 0 .and. trim(process_name) == 'seasalt', 'Fortran process contract discovery')
   call require(contract%field_count > 0, 'contract field count')
   call model%get_process_field(1, 1, field, field_name, units, rc)
   call require(rc == 0 .and. field%rank > 0 .and. len_trim(field_name) > 0 .and. len_trim(units) > 0, &
      'Fortran field contract discovery')

   interval%interval_id = 700_c_int64_t
   interval%year = 2024; interval%month = 2; interval%day = 29
   interval%hour = 12; interval%minute = 0; interval%second = 0; interval%day_of_year = 60
   interval%duration_seconds = 60.0_c_double
   call model%begin_interval(interval, rc)
   call require(rc == 0, 'Fortran checked interval begin')
   call model%run_selected_process(interval%interval_id, 'site', 0_c_int32_t, 'unknown', 30.0_c_double, &
      operation_status, publication_eligible, generation, rc)
   call require(rc /= 0 .and. operation_status == 1_c_int .and. .not. publication_eligible, &
      'rejected selected call is ineligible for publication')
   call model%run_selected_process(interval%interval_id, 'site', 1_c_int32_t, 'drydep', 30.0_c_double, &
      operation_status, publication_eligible, generation, rc)
   call require(rc /= 0 .and. operation_status == 1_c_int .and. .not. publication_eligible, &
      'configured-suite disagreement rejects a process absent from the configured schedule')
   call model%end_interval(interval%interval_id, rc)
   call require(rc == 0, 'Fortran checked interval end')
   call model%get_timestep_outcome(outcome_status, timestep, interval%duration_seconds, import_generation, &
      process_index, state_classification, process_name, units, rc)
   call require(rc == 0 .and. timestep == 1_c_long_long .and. outcome_status == 2_c_int, &
      'Fortran checked timestep outcome')
   call model%finalize(rc)
   call require(rc == 0, 'model finalization')
   print *, 'PASS test_catchem_api_fortran_dispatch'
contains
   !> Assert a test condition and stop with its identifying message on failure.
   !! @param condition Boolean condition expected to hold.
   !! @param message Short description of the failed assertion.
   subroutine require(condition, message)
      logical, intent(in) :: condition
      character(len=*), intent(in) :: message
      if (.not. condition) then
         print *, 'FAIL: ', trim(message)
         stop 1
      end if
   end subroutine
end program test_catchem_api_fortran_dispatch

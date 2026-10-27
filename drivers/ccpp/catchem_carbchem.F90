!> \file catchem_carbchem.F90
!! \brief CCPP selected-process driver for GOCART carbonaceous aerosol chemistry.
module catchem_carbchem_driver
   use iso_c_binding, only: c_double, c_int, c_int64_t
   use iso_fortran_env, only: output_unit
   use machine, only: kind_phys
   use ccpp_constituent_prop_mod, only: ccpp_constituent_properties_t, ccpp_constituent_prop_ptr_t
   use catchem_ccpp_context, only: ccpp_context_handle, catchem_ccpp_context_species_count, &
                                   catchem_ccpp_context_set_mapping, catchem_ccpp_context_get_mapping
   use catchem_ccpp_adapter, only: catchem_ccpp_register_constituents, catchem_ccpp_build_mapping, &
                                   catchem_ccpp_begin_import, catchem_ccpp_bind_met_3d, catchem_ccpp_bind_met_vertical, &
                                   catchem_ccpp_gather, catchem_ccpp_run_selected, catchem_ccpp_stage_tendency
   use catchem_ccpp_conversion, only: catchem_gfs_density_state
   implicit none
   private
   public :: catchem_carbchem_driver_register, catchem_carbchem_driver_init, &
             catchem_carbchem_driver_run, catchem_carbchem_driver_final
contains
   !> Register every mechanism constituent used by the configured CarbChem process.
  !! @param context_id Host-owned context slot.
  !! @param context_generation Context generation protecting against stale reuse.
  !! @param constituent_props Unique registration output for this scheme.
  !! @param errmsg Error detail.
  !! @param errflg CCPP error code; zero on success.
   subroutine catchem_carbchem_driver_register(context_id, context_generation, constituent_props, errmsg, errflg)
      integer(c_int), intent(in) :: context_id
      integer(c_int64_t), intent(in) :: context_generation
      type(ccpp_constituent_properties_t), allocatable, intent(out) :: constituent_props(:)
      character(len=*), intent(out) :: errmsg
      integer, intent(out) :: errflg
      type(ccpp_context_handle) :: handle
      integer(c_int) :: status
      integer :: species_count
      handle%id = context_id
      handle%generation = context_generation
      errmsg = ''
      errflg = 0
      call catchem_ccpp_context_species_count(handle, species_count, status)
      if (status /= 0_c_int .or. species_count <= 0) then
         errmsg = 'CarbChem registration requires a bound nonempty CATChem mechanism manifest.'
         errflg = max(1, int(status))
         return
      end if
      allocate (constituent_props(species_count), stat=errflg)
      if (errflg /= 0) then
         errmsg = 'CarbChem could not allocate its constituent registration output.'
         return
      end if
      call catchem_ccpp_register_constituents(handle, constituent_props, errmsg, errflg)
   end subroutine catchem_carbchem_driver_register

   !> Build and retain CarbChem's context-local constituent-index map.
  !! @param context_id Host-owned context slot.
  !! @param context_generation Context generation.
  !! @param constituent_properties Full merged CCPP property pointers.
  !! @param errmsg Error detail.
  !! @param errflg CCPP error code; zero on success.
   subroutine catchem_carbchem_driver_init(context_id, context_generation, constituent_properties, errmsg, errflg)
      integer(c_int), intent(in) :: context_id
      integer(c_int64_t), intent(in) :: context_generation
      type(ccpp_constituent_prop_ptr_t), intent(in) :: constituent_properties(:)
      character(len=*), intent(out) :: errmsg
      integer, intent(out) :: errflg
      type(ccpp_context_handle) :: handle
      integer, allocatable :: host_to_catchem(:)
      integer(c_int) :: status
      handle%id = context_id
      handle%generation = context_generation
      call catchem_ccpp_build_mapping(handle, constituent_properties, host_to_catchem, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_context_set_mapping(handle, host_to_catchem, status)
      if (status /= 0_c_int) then
         errmsg = 'CarbChem could not install the verified context-local CCPP constituent map.'
         errflg = int(status)
      end if
   end subroutine catchem_carbchem_driver_init

   !> Import the full constituent state, run only CarbChem, and stage its host-representation tendency.
  !! @param context_id Host-owned context slot.
  !! @param context_generation Context generation.
  !! @param interval_id Physics interval identity.
  !! @param interval_token Shared host interval token.
  !! @param site Host coupling site.
  !! @param subcycle Process subcycle.
  !! @param duration_seconds Invocation duration.
  !! @param constituents Complete CCPP constituent array.
  !! @param air_pressure Layer pressure in Pa.
  !! @param air_temperature Layer temperature in K.
  !! @param specific_humidity Water-vapor mass fraction.
  !! @param pedge Interface pressure in Pa.
  !! @param errmsg Error detail.
  !! @param errflg CCPP error code; zero on success.
   subroutine catchem_carbchem_driver_run(context_id, context_generation, interval_id, interval_token, site, subcycle, &
                            duration_seconds, constituents, air_pressure, air_temperature, specific_humidity, pedge, errmsg, errflg)
      integer(c_int), intent(in) :: context_id
      integer(c_int64_t), intent(in) :: context_generation, interval_id, interval_token
      character(len=*), intent(in) :: site
      integer(c_int), intent(in) :: subcycle
      real(kind_phys), intent(in) :: duration_seconds
      real(kind_phys), intent(in) :: constituents(:, :, :), air_pressure(:, :), air_temperature(:, :), &
                                     specific_humidity(:, :)
      real(kind_phys), intent(in), target, contiguous :: pedge(:, :)
      character(len=*), intent(out) :: errmsg
      integer, intent(out) :: errflg
      type(ccpp_context_handle) :: handle
      integer, allocatable :: host_to_catchem(:)
      real(c_double), allocatable :: staging(:, :, :), initial_staging(:, :, :)
      real(kind_phys), allocatable :: host_density(:, :), dry_density(:, :), host_number_density(:, :), &
                                      dry_number_density(:, :)
      real(kind_phys), allocatable :: delp(:, :)
      real(c_double), allocatable :: pressure_c(:, :), temperature_c(:, :), humidity_c(:, :)
      real(c_double), allocatable :: host_density_c(:, :), dry_density_c(:, :), host_number_density_c(:, :), &
                                     dry_number_density_c(:, :)
      integer(c_int) :: conversion_status, operation_status
      integer(c_int64_t) :: import_generation
      logical :: publication_eligible

      handle%id = context_id
      handle%generation = context_generation
      errmsg = ''
      errflg = 0
      call catchem_ccpp_context_get_mapping(handle, host_to_catchem, operation_status)
      if (operation_status /= 0_c_int) then
         errmsg = 'CarbChem run requires the shared CCPP constituent map from scheme initialization.'
         errflg = int(operation_status)
         return
      end if
      if (any(shape(air_temperature) /= shape(air_pressure)) .or. &
          any(shape(specific_humidity) /= shape(air_pressure)) .or. &
          size(pedge, 1) /= size(air_pressure, 1) .or. size(pedge, 2) /= size(air_pressure, 2) + 1) then
         errmsg = 'CarbChem thermodynamic and pressure-interface extents must match.'
         errflg = 1
         return
      end if
      allocate (pressure_c(size(air_pressure, 1), size(air_pressure, 2)), &
            temperature_c(size(air_pressure, 1), size(air_pressure, 2)), humidity_c(size(air_pressure, 1), size(air_pressure, 2)), &
        host_density_c(size(air_pressure, 1), size(air_pressure, 2)), dry_density_c(size(air_pressure, 1), size(air_pressure, 2)), &
                host_number_density_c(size(air_pressure, 1), size(air_pressure, 2)), &
                dry_number_density_c(size(air_pressure, 1), size(air_pressure, 2)), &
            host_density(size(air_pressure, 1), size(air_pressure, 2)), dry_density(size(air_pressure, 1), size(air_pressure, 2)), &
                host_number_density(size(air_pressure, 1), size(air_pressure, 2)), &
                dry_number_density(size(air_pressure, 1), size(air_pressure, 2)), delp(size(air_pressure, 1), &
                                                                                       size(air_pressure, 2)))
      pressure_c = real(air_pressure, c_double)
      temperature_c = real(air_temperature, c_double)
      humidity_c = real(specific_humidity, c_double)
      call catchem_gfs_density_state(pressure_c, temperature_c, humidity_c, host_density_c, dry_density_c, &
                                     host_number_density_c, dry_number_density_c, conversion_status)
      if (conversion_status /= 0_c_int) then
         errmsg = 'CarbChem requires physical pressure, temperature and specific humidity.'
         errflg = int(conversion_status)
         return
      end if
      host_density = real(host_density_c, kind_phys)
      dry_density = real(dry_density_c, kind_phys)
      host_number_density = real(host_number_density_c, kind_phys)
      dry_number_density = real(dry_number_density_c, kind_phys)
      delp = abs(pedge(:, 2:) - pedge(:, :size(pedge, 2) - 1))
      ! Bind this call's host fields in a fresh import generation.
      call catchem_ccpp_begin_import(handle, errmsg, errflg, 'carbchem')
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_3d(handle, 'AIRDEN', host_density, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_3d(handle, 'DELP', delp, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_3d(handle, 'PMID', air_pressure, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_3d(handle, 'T', air_temperature, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_vertical(handle, 'PEDGE', pedge, 1, errmsg, errflg)
      if (errflg /= 0) return
      allocate (staging(size(constituents, 1), size(constituents, 2), size(host_to_catchem)))
      ! Gather the complete mapped constituent state into CATChem staging arrays.
      call catchem_ccpp_gather(handle, host_to_catchem, constituents, host_density, dry_density, &
                               host_number_density, dry_number_density, staging, errmsg, errflg)
      if (errflg /= 0) return
      initial_staging = staging
      call catchem_ccpp_run_selected(handle, host_to_catchem, staging, interval_id, site, int(subcycle), 'carbchem', &
                        real(duration_seconds, c_double), operation_status, publication_eligible, import_generation, errmsg, errflg)
      if (errflg /= 0) return
      write (*, '(a,es24.16)') 'CATCHEM_SIGNAL name=carbchem_constituent_max_abs_delta VALUE=', &
         maxval(abs(staging - initial_staging))
      ! Save the process result as a tendency for the host update scheme to apply.
      call catchem_ccpp_stage_tendency(handle, interval_token, 'carbchem', real(duration_seconds, c_double), &
                       host_to_catchem, constituents, staging, host_density, dry_density, host_number_density, dry_number_density, &
                                       errmsg, errflg)
      if (errflg == 0) write (*, '(a)') 'CATCHEM_SELECTED_PROCESS=carbchem EXECUTIONS=1'
      if (errflg == 0) write (output_unit, '(a,a,a,i0,a,i0,a,es24.16)') &
         'CATCHEM_INVOCATION process=', 'carbchem', ' interval=', interval_id, ' subcycle=', subcycle, &
         ' duration_seconds=', real(duration_seconds, c_double)
   end subroutine catchem_carbchem_driver_run

   !> Finalize the CarbChem participant for its context generation.
  !! @param context_id Host-owned context slot.
  !! @param context_generation Context generation.
  !! @param errmsg Error detail.
  !! @param errflg CCPP error code; zero on success.
   subroutine catchem_carbchem_driver_final(context_id, context_generation, errmsg, errflg)
      integer(c_int), intent(in) :: context_id
      integer(c_int64_t), intent(in) :: context_generation
      character(len=*), intent(out) :: errmsg
      integer, intent(out) :: errflg
      type(ccpp_context_handle) :: handle
      handle%id = context_id
      handle%generation = context_generation
      errmsg = ''
      errflg = 0
      if (context_generation <= 0_c_int64_t) then
         errmsg = 'CarbChem finalization received an invalid context generation.'
         errflg = 1
      end if
   end subroutine catchem_carbchem_driver_final
end module catchem_carbchem_driver

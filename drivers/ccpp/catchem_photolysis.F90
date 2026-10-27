!> \file catchem_photolysis.F90
!! \brief CCPP selected-process driver for CATChem sulfate chemistry.
module catchem_photolysis_driver
   use iso_c_binding, only: c_double, c_int, c_int64_t
   use iso_fortran_env, only: output_unit
   use catchem_bridge_precision, only: fp
   use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
   use machine, only: kind_phys
   use ccpp_constituent_prop_mod, only: ccpp_constituent_properties_t, ccpp_constituent_prop_ptr_t
   use catchem_ccpp_context, only: ccpp_context_handle, catchem_ccpp_context_species_count, &
                       catchem_ccpp_context_set_mapping, catchem_ccpp_context_get_mapping, catchem_ccpp_context_model, CATChem_Model
   use catchem_ccpp_adapter, only: catchem_ccpp_process_is_active, &
                                    catchem_ccpp_register_constituents, catchem_ccpp_build_mapping, &
                                   catchem_ccpp_begin_import, catchem_ccpp_bind_met_2d, catchem_ccpp_bind_met_3d, &
                         catchem_ccpp_bind_met_vertical, catchem_ccpp_gather, catchem_ccpp_run_selected, catchem_ccpp_stage_tendency
   use catchem_ccpp_conversion, only: catchem_gfs_density_state
   implicit none
   private
   public :: catchem_photolysis_driver_register, catchem_photolysis_driver_init, &
             catchem_photolysis_driver_run, catchem_photolysis_driver_final
contains
   !> Register every configured constituent for Photolysis through CCPP.
  !! @param context_id Host-owned context slot.
  !! @param context_generation Context generation protecting against stale reuse.
  !! @param constituent_props Unique registration output for this scheme.
  !! @param errmsg Error detail.
  !! @param errflg CCPP error code; zero on success.
   subroutine catchem_photolysis_driver_register(context_id, context_generation, constituent_props, errmsg, errflg)
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
         errmsg = 'Photolysis registration requires a bound nonempty CATChem mechanism manifest.'
         errflg = max(1, int(status))
         return
      end if
      allocate (constituent_props(species_count), stat=errflg)
      if (errflg /= 0) then
         errmsg = 'Photolysis could not allocate its constituent registration output.'
         return
      end if
      call catchem_ccpp_register_constituents(handle, constituent_props, errmsg, errflg)
   end subroutine catchem_photolysis_driver_register

   !> Build and retain Photolysis's context-local constituent-index map.
  !! @param context_id Host-owned context slot.
  !! @param context_generation Context generation.
  !! @param constituent_properties Full merged CCPP property pointers.
  !! @param errmsg Error detail.
  !! @param errflg CCPP error code; zero on success.
   subroutine catchem_photolysis_driver_init(context_id, context_generation, constituent_properties, errmsg, errflg)
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
         errmsg = 'Photolysis could not install the verified context-local CCPP constituent map.'
         errflg = int(status)
      end if
   end subroutine catchem_photolysis_driver_init

   !> Import the full constituent state, call only Photolysis, and stage its tendency.
  !! @param context_id Host-owned context slot.
  !! @param context_generation Context generation.
  !! @param interval_id Physics interval identity.
  !! @param interval_token Shared host interval token.
  !! @param site Declared host coupling site.
  !! @param subcycle Declared process subcycle.
  !! @param duration_seconds Interval duration in seconds.
  !! @param constituents Complete CCPP constituent array in host order.
  !! @param air_pressure Layer pressure in Pa.
  !! @param air_temperature Layer temperature in K.
  !! @param specific_humidity Water-vapor mass fraction.
  !! @param pedge Interface pressure in Pa.
  !! @param geopotential_interface Interface geopotential in m2 s-2.
  !! @param bxheight Layer thickness derived from host interface geopotential in m.
  !! @param cloud_fraction Layer cloud fraction.
  !! @param latitude Column latitude in degrees.
  !! @param longitude Column longitude in degrees.
  !! @param pbl_height Boundary-layer height in m.
  !! @param ustar Surface friction velocity in m s-1.
  !! @param u10m Eastward 10-m wind in m s-1.
  !! @param v10m Northward 10-m wind in m s-1.
  !! @param lwi GFS sea-land-ice category.
  !! @param z0 Momentum roughness length in cm, converted to the CATChem state unit.
  !! @param errmsg Error detail.
  !! @param loop_counter CCPP suite subcycle iteration.
  !! @param loop_extent CCPP suite subcycle count used to scale invocation duration.
  !! @param errflg CCPP error code; zero on success.
   subroutine catchem_photolysis_driver_run(context_id, context_generation, interval_id, interval_token, site, subcycle, &
                                         loop_counter, loop_extent, duration_seconds, constituents, air_pressure, air_temperature, &
                                            specific_humidity, pedge, geopotential_interface, cloud_fraction, latitude, longitude, &
                                            pbl_height, ustar, u10m, v10m, lwi, z0, errmsg, errflg)
      integer(c_int), intent(in) :: context_id
      integer(c_int64_t), intent(in) :: context_generation, interval_id, interval_token
      character(len=*), intent(in) :: site
      integer(c_int), intent(in) :: subcycle, loop_counter, loop_extent
      real(kind_phys), intent(in) :: duration_seconds
      real(kind_phys), intent(in) :: constituents(:, :, :), air_pressure(:, :), air_temperature(:, :), &
                                     specific_humidity(:, :)
      real(kind_phys), intent(in), target, contiguous :: pedge(:, :), geopotential_interface(:, :), cloud_fraction(:, :)
      real(kind_phys), intent(in), target, contiguous :: latitude(:), longitude(:), pbl_height(:)
      real(kind_phys), intent(in), target, contiguous :: ustar(:), u10m(:), v10m(:)
      integer, intent(in) :: lwi(:)
      real(kind_phys), intent(in), target, contiguous :: z0(:)
      character(len=*), intent(out) :: errmsg
      integer, intent(out) :: errflg
      type(ccpp_context_handle) :: handle
      integer, allocatable :: host_to_catchem(:)
      real(c_double), allocatable :: staging(:, :, :), initial_staging(:, :, :)
      real(kind_phys), allocatable :: host_density(:, :), dry_density(:, :), host_number_density(:, :), &
                                      dry_number_density(:, :)
      real(kind_phys), allocatable :: cloud_fraction_layer(:, :)
      real(kind_phys), allocatable :: delp(:, :), lwi_real(:), height_interface(:, :), bxheight(:, :), z0_meters(:)
      real(c_double), allocatable :: pressure_c(:, :), temperature_c(:, :), humidity_c(:, :)
      real(c_double), allocatable :: host_density_c(:, :), dry_density_c(:, :), host_number_density_c(:, :), &
                                     dry_number_density_c(:, :)
      type(CATChem_Model), pointer :: model
      character(len=64), allocatable :: diagnostic_names(:)
      real(fp), allocatable :: diagnostic_data(:, :, :)
      integer(c_int) :: conversion_status, operation_status
      integer(c_int64_t) :: import_generation
      integer(c_int) :: effective_subcycle
      integer(c_int64_t) :: effective_subcycle_wide
      real(kind_phys) :: invocation_duration
      logical :: publication_eligible
      integer :: diagnostic_status, diagnostic_index, rate_count
      real(c_double) :: rate_maximum
      logical :: process_active
      handle%id = context_id
      handle%generation = context_generation
      call catchem_ccpp_process_is_active(handle, 'photolysis', process_active, errmsg, errflg)
      if (errflg /= 0 .or. .not. process_active) return
      errmsg = ''
      errflg = 0
      if (subcycle < 1_c_int .or. loop_counter < 1_c_int .or. loop_extent < 1_c_int .or. &
          loop_counter > loop_extent) then
         errmsg = 'CCPP subcycle counter and extent are invalid.'
         errflg = 1
         return
      end if
      effective_subcycle_wide = int(subcycle - 1_c_int, c_int64_t)*int(loop_extent, c_int64_t) + &
                                int(loop_counter, c_int64_t)
      if (effective_subcycle_wide > int(huge(effective_subcycle), c_int64_t)) then
         errmsg = 'Combined CATChem subcycle identity exceeds its supported range.'
         errflg = 1
         return
      end if
      effective_subcycle = int(effective_subcycle_wide, c_int)
      invocation_duration = duration_seconds/real(loop_extent, kind_phys)
      if (interval_token /= interval_id) then
         errmsg = 'Photolysis interval identity must match the shared host interval token.'
         errflg = 1
         return
      end if
      call catchem_ccpp_context_get_mapping(handle, host_to_catchem, operation_status)
      if (operation_status /= 0_c_int) then
         errmsg = 'Photolysis run requires the shared CCPP constituent map from scheme initialization.'
         errflg = int(operation_status)
         return
      end if
      if (any(shape(air_temperature) /= shape(air_pressure)) .or. &
          any(shape(specific_humidity) /= shape(air_pressure)) .or. &
          size(pedge, 1) /= size(air_pressure, 1) .or. size(pedge, 2) /= size(air_pressure, 2) + 1 .or. &
          size(geopotential_interface, 1) /= size(air_pressure, 1) .or. &
          size(geopotential_interface, 2) /= size(air_pressure, 2) + 1 .or. &
          size(cloud_fraction, 1) /= size(air_pressure, 1) .or. size(cloud_fraction, 2) < size(air_pressure, 2) .or. &
          size(lwi) /= size(air_pressure, 1) .or. size(z0) /= size(air_pressure, 1)) then
         errmsg = 'Photolysis meteorology extents must match layer state and its interface fields.'
         errflg = 1
         return
      end if
      allocate (cloud_fraction_layer(size(air_pressure, 1), size(air_pressure, 2)))
      cloud_fraction_layer = cloud_fraction(:, 1:size(air_pressure, 2))
      write (output_unit, '(a,2(es24.16,1x))') 'CATCHEM_SO4_CLOUD_RANGE=', &
         minval(cloud_fraction_layer), maxval(cloud_fraction_layer)
      if (any(.not. ieee_is_finite(cloud_fraction_layer)) .or. any(cloud_fraction_layer < 0.0_kind_phys) .or. &
          any(cloud_fraction_layer > 1.0_kind_phys)) then
         errmsg = 'Photolysis instantaneous cloud fraction must contain finite values in [0, 1].'
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
                dry_number_density(size(air_pressure, 1), size(air_pressure, 2)), &
                delp(size(air_pressure, 1), size(air_pressure, 2)), lwi_real(size(lwi)), &
                height_interface(size(air_pressure, 1), size(air_pressure, 2) + 1), &
                bxheight(size(air_pressure, 1), size(air_pressure, 2)), z0_meters(size(z0)))
      pressure_c = real(air_pressure, c_double)
      temperature_c = real(air_temperature, c_double)
      humidity_c = real(specific_humidity, c_double)
      call catchem_gfs_density_state(pressure_c, temperature_c, humidity_c, host_density_c, dry_density_c, &
                                     host_number_density_c, dry_number_density_c, conversion_status)
      if (conversion_status /= 0_c_int) then
         errmsg = 'Photolysis requires physical pressure, temperature and specific humidity.'
         errflg = int(conversion_status)
         return
      end if
      host_density = real(host_density_c, kind_phys)
      dry_density = real(dry_density_c, kind_phys)
      host_number_density = real(host_number_density_c, kind_phys)
      dry_number_density = real(dry_number_density_c, kind_phys)
      delp = abs(pedge(:, 2:) - pedge(:, :size(pedge, 2) - 1))
      height_interface = geopotential_interface/9.80665_kind_phys
      bxheight = abs(height_interface(:, 2:) - height_interface(:, :size(height_interface, 2) - 1))
      z0_meters = z0*0.01_kind_phys
      lwi_real = real(lwi, kind_phys)
      ! Bind this call's host fields in a fresh import generation.
      call catchem_ccpp_begin_import(handle, errmsg, errflg, 'photolysis')
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_3d(handle, 'AIRDEN', host_density, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_3d(handle, 'BXHEIGHT', bxheight, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_3d(handle, 'DELP', delp, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_3d(handle, 'PMID', air_pressure, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_3d(handle, 'T', air_temperature, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_3d(handle, 'CLDF', cloud_fraction_layer, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_vertical(handle, 'PEDGE', pedge, 1, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_vertical(handle, 'Z', height_interface, 1, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_2d(handle, 'LAT', latitude, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_2d(handle, 'LON', longitude, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_2d(handle, 'PBLH', pbl_height, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_2d(handle, 'USTAR', ustar, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_2d(handle, 'U10M', u10m, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_2d(handle, 'V10M', v10m, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_2d(handle, 'LWI', lwi_real, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_2d(handle, 'Z0', z0_meters, errmsg, errflg)
      if (errflg /= 0) return
      allocate (staging(size(constituents, 1), size(constituents, 2), size(host_to_catchem)))
      ! Gather the complete mapped constituent state into CATChem staging arrays.
      call catchem_ccpp_gather(handle, host_to_catchem, constituents, host_density, dry_density, &
                               host_number_density, dry_number_density, staging, errmsg, errflg)
      if (errflg /= 0) return
      initial_staging = staging
      call catchem_ccpp_run_selected(handle, host_to_catchem, staging, interval_id, site, int(effective_subcycle), 'photolysis', &
                     real(invocation_duration, c_double), operation_status, publication_eligible, import_generation, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_context_model(handle, model, operation_status)
      if (operation_status /= 0_c_int .or. .not. associated(model)) then
         errmsg = 'Photolysis completed but its mechanism-associated product diagnostics could not be retrieved.'
         errflg = max(1, int(operation_status))
         return
      end if
      call model%get_diagnostic_names(diagnostic_names, rc=diagnostic_status)
      if (diagnostic_status /= 0) then
         errmsg = 'Photolysis completed but its product name registry could not be read.'
         errflg = max(1, diagnostic_status)
         return
      end if
      rate_maximum = 0.0_c_double
      rate_count = 0
      do diagnostic_index = 1, size(diagnostic_names)
         if (index(trim(diagnostic_names(diagnostic_index)), 'photolysis_rate_') /= 1) cycle
         call model%get_diagnostic(trim(diagnostic_names(diagnostic_index)), diagnostic_data, diagnostic_status)
         if (diagnostic_status /= 0 .or. .not. allocated(diagnostic_data)) cycle
         rate_maximum = max(rate_maximum, real(maxval(abs(diagnostic_data)), c_double))
         rate_count = rate_count + 1
      end do
      if (rate_count <= 0) then
         errmsg = 'Photolysis published no named reaction-rate products for the configured mechanism.'
         errflg = 1
         return
      end if
      write (output_unit, '(a,i0)') 'CATCHEM_PHOTOLYSIS_PRODUCTS COUNT=', rate_count
      write (output_unit, '(a,es24.16)') 'CATCHEM_SIGNAL name=photolysis_rate_max_abs VALUE=', rate_maximum
      ! Photolysis publishes named mechanism products inside ExecutionPlan and has no chemical contribution.
      if (errflg == 0) write (output_unit, '(a)') 'CATCHEM_SELECTED_PROCESS=photolysis EXECUTIONS=1'
      if (errflg == 0) write (output_unit, '(a,a,a,i0,a,i0,a,es24.16)') &
         'CATCHEM_INVOCATION process=', 'photolysis', ' interval=', interval_id, ' subcycle=', effective_subcycle, &
         ' duration_seconds=', real(invocation_duration, c_double)
   end subroutine catchem_photolysis_driver_run

   !> Finalize the Photolysis participant for its context generation.
  !! @param context_id Host-owned context slot.
  !! @param context_generation Context generation.
  !! @param errmsg Error detail.
  !! @param errflg CCPP error code; zero on success.
   subroutine catchem_photolysis_driver_final(context_id, context_generation, errmsg, errflg)
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
         errmsg = 'Photolysis finalization received an invalid context generation.'
         errflg = 1
      end if
   end subroutine catchem_photolysis_driver_final
end module catchem_photolysis_driver

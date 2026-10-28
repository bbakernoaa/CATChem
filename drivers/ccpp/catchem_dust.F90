!> \file catchem_dust_driver.F90
!! \brief CCPP selected-process driver for CATChem dust emission schemes.
module catchem_dust_driver
   use iso_c_binding, only: c_double, c_int, c_int64_t
   use iso_fortran_env, only: output_unit
   use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
   use machine, only: kind_phys
   use ccpp_constituent_prop_mod, only: ccpp_constituent_properties_t, ccpp_constituent_prop_ptr_t
   use catchem_ccpp_context, only: ccpp_context_handle, catchem_ccpp_context_species_count, &
                                   catchem_ccpp_context_set_mapping, catchem_ccpp_context_get_mapping
   use catchem_ccpp_adapter, only: catchem_ccpp_process_is_active, &
                                    catchem_ccpp_register_constituents, catchem_ccpp_build_mapping, &
                                   catchem_ccpp_begin_import, catchem_ccpp_bind_met_2d, catchem_ccpp_bind_met_3d, &
                                   catchem_ccpp_bind_met_vertical, catchem_ccpp_gather, catchem_ccpp_run_selected, &
                                   catchem_ccpp_stage_tendency
   use catchem_ccpp_conversion, only: catchem_gfs_density_state
   implicit none
   private
   public :: catchem_dust_driver_register, catchem_dust_driver_init, catchem_dust_driver_run, catchem_dust_driver_final
contains

   !> \brief Register the configured mechanism's complete constituent set with CCPP.
   !> \param context_id Host-owned context slot.
   !> \param context_generation Generation that protects against stale context reuse.
   !> \param constituent_props Unique Dust registration output.
   !> \param errmsg Error detail on failure.
   !> \param errflg CCPP error code, zero on success.
   !> \section arg_table_catchem_dust_driver_register Argument Table
   !! \htmlinclude arg_table_catchem_dust_driver_register.hml
   !!
   subroutine catchem_dust_driver_register(context_id, context_generation, constituent_props, errmsg, errflg)
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
         errmsg = 'Dust registration requires a bound nonempty CATChem mechanism manifest.'
         errflg = max(1, int(status))
         return
      end if
      allocate (constituent_props(species_count), stat=errflg)
      if (errflg /= 0) then
         errmsg = 'Dust could not allocate its constituent registration output.'
         return
      end if
      call catchem_ccpp_register_constituents(handle, constituent_props, errmsg, errflg)
   end subroutine catchem_dust_driver_register

   !> \brief Build and retain this host registry's constituent index mapping.
   !> \param context_id Host-owned context slot.
   !> \param context_generation Context generation.
   !> \param constituent_properties Full merged host property pointers.
   !> \param errmsg Error detail on failure.
   !> \param errflg CCPP error code, zero on success.
   !> \section arg_table_catchem_dust_driver_init Argument Table
   !! \htmlinclude arg_table_catchem_dust_driver_init.hml
   !!
   subroutine catchem_dust_driver_init(context_id, context_generation, constituent_properties, errmsg, errflg)
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
         errmsg = 'Dust could not install the verified context-local CCPP constituent map.'
         errflg = int(status)
      end if
   end subroutine catchem_dust_driver_init

   !> \brief Import this call's host fields, execute only Dust, and stage its host tendency.
   !> \param context_id Host-owned context slot.
   !> \param context_generation Context generation.
   !> \param interval_id Host interval identity.
   !> \param interval_token Shared interval token.
   !> \param site Declared host coupling site.
   !> \param subcycle Declared process subcycle.
   !> \param duration_seconds Duration represented by this invocation.
   !> \param constituents Full CCPP constituent array in host order.
   !> \param air_pressure Host layer pressure in Pa.
   !> \param air_temperature Host layer temperature in K.
   !> \param specific_humidity Host water-vapor mass fraction.
   !> \param landfrac Fractional host land mask, converted to the scheme's integer land indicator.
   !> \param soilm Soil moisture by soil layer; the Ginoux surface-moisture fallback uses layer one.
   !> \param gwetTop Normalized soil wetness supplied by the host when available.
   !> \param ssm Surface soil moisture supplied directly when available.
   !> \param errmsg Error detail on failure.
   !> \param errflg CCPP error code, zero on success.
   !> \section arg_table_catchem_dust_driver_run Argument Table
   !! \htmlinclude arg_table_catchem_dust_driver_run.hml
   !!
   subroutine catchem_dust_driver_run(context_id, context_generation, interval_id, interval_token, site, subcycle, &
                                      duration_seconds, constituents, air_pressure, air_temperature, specific_humidity, &
                                pedge, soilm, clayfrac, frlake, frsno, gvf, lai, landfrac, lwi, rdrag, sandfrac, gwetTop, ssm, ts, &
                                      u10m, v10m, ustar, ustar_threshold, z0, errmsg, errflg)
      integer(c_int), intent(in) :: context_id
      integer(c_int64_t), intent(in) :: context_generation, interval_id, interval_token
      character(len=*), intent(in) :: site
      integer(c_int), intent(in) :: subcycle
      real(kind_phys), intent(in) :: duration_seconds
      real(kind_phys), intent(in) :: constituents(:, :, :)
      real(kind_phys), intent(in) :: air_pressure(:, :), air_temperature(:, :), specific_humidity(:, :)
      real(kind_phys), intent(in), target, contiguous :: pedge(:, :)
      real(kind_phys), optional, intent(in), target, contiguous :: soilm(:, :)
      real(kind_phys), optional, intent(in), target, contiguous :: clayfrac(:), frlake(:), frsno(:), gvf(:), lai(:)
      real(kind_phys), optional, intent(in) :: landfrac(:)
      integer, optional, intent(in) :: lwi(:)
      real(kind_phys), optional, intent(in), target, contiguous :: rdrag(:), sandfrac(:), gwetTop(:), ssm(:), ts(:)
      real(kind_phys), optional, intent(in), target, contiguous :: u10m(:), v10m(:), ustar(:), ustar_threshold(:), z0(:)
      character(len=*), intent(out) :: errmsg
      integer, intent(out) :: errflg
      type(ccpp_context_handle) :: handle
      integer, allocatable :: host_to_catchem(:)
      real(c_double), allocatable :: staging(:, :, :)
      real(c_double), allocatable :: initial_staging(:, :, :)
      real(kind_phys), allocatable :: integer_mask_as_real(:)
      real(kind_phys), allocatable :: ginoux_surface_moisture(:)
      real(kind_phys), allocatable :: host_density(:, :), dry_density(:, :), host_number_density(:, :), &
                                      dry_number_density(:, :)
      real(kind_phys), allocatable :: delp(:, :)
      real(c_double), allocatable :: pressure_c(:, :), temperature_c(:, :), humidity_c(:, :)
      real(c_double), allocatable :: host_density_c(:, :), dry_density_c(:, :), host_number_density_c(:, :), &
                                     dry_number_density_c(:, :)
      integer(c_int) :: conversion_status
      integer(c_int) :: operation_status
      integer(c_int64_t) :: import_generation
      integer :: acceptance_environment_status, acceptance_read_status
      character(len=64) :: acceptance_soil_text
      real(kind_phys) :: acceptance_soil_moisture
      logical :: publication_eligible
      logical :: process_active
      handle%id = context_id
      handle%generation = context_generation
      call catchem_ccpp_process_is_active(handle, 'dust', process_active, errmsg, errflg)
      if (errflg /= 0 .or. .not. process_active) return
      errmsg = ''
      errflg = 0
      call catchem_ccpp_context_get_mapping(handle, host_to_catchem, operation_status)
      if (operation_status /= 0_c_int) then
         errmsg = 'Dust run requires the shared host CCPP constituent map from scheme initialization.'
         errflg = int(operation_status)
         return
      end if
      allocate (pressure_c(size(air_pressure, 1), size(air_pressure, 2)), &
                temperature_c(size(air_temperature, 1), size(air_temperature, 2)), &
                humidity_c(size(specific_humidity, 1), size(specific_humidity, 2)), &
                host_density_c(size(air_pressure, 1), size(air_pressure, 2)), &
                dry_density_c(size(air_pressure, 1), size(air_pressure, 2)), &
                host_number_density_c(size(air_pressure, 1), size(air_pressure, 2)), &
                dry_number_density_c(size(air_pressure, 1), size(air_pressure, 2)), &
                host_density(size(air_pressure, 1), size(air_pressure, 2)), &
                dry_density(size(air_pressure, 1), size(air_pressure, 2)), &
                host_number_density(size(air_pressure, 1), size(air_pressure, 2)), &
                dry_number_density(size(air_pressure, 1), size(air_pressure, 2)), &
                delp(size(air_pressure, 1), size(air_pressure, 2)))
      pressure_c = real(air_pressure, c_double)
      temperature_c = real(air_temperature, c_double)
      humidity_c = real(specific_humidity, c_double)
      call catchem_gfs_density_state(pressure_c, temperature_c, humidity_c, host_density_c, dry_density_c, &
                                     host_number_density_c, dry_number_density_c, conversion_status)
      if (conversion_status /= 0_c_int) then
         errmsg = 'Dust requires physical pressure, temperature, and specific humidity at every active grid point.'
         errflg = int(conversion_status)
         return
      end if
      host_density = real(host_density_c, kind_phys)
      dry_density = real(dry_density_c, kind_phys)
      host_number_density = real(host_number_density_c, kind_phys)
      dry_number_density = real(dry_number_density_c, kind_phys)
      if (size(pedge, 1) /= size(air_pressure, 1) .or. size(pedge, 2) /= size(air_pressure, 2) + 1) then
         errmsg = 'Dust requires interface pressure with one more level than layer pressure.'
         errflg = 1
         return
      end if
      delp = abs(pedge(:, 2:) - pedge(:, :size(pedge, 2) - 1))
      ! Bind this call's host fields in a fresh import generation.
      call catchem_ccpp_begin_import(handle, errmsg, errflg, 'dust')
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_3d(handle, 'AIRDEN', host_density, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_3d(handle, 'DELP', delp, errmsg, errflg)
      if (errflg /= 0) return
      call bind_3d(handle, 'PEDGE', pedge, errmsg, errflg)
      if (errflg /= 0) return
      call bind_vertical(handle, 'SOILM', soilm, 2, errmsg, errflg)
      if (errflg /= 0) return
      call bind_2d(handle, 'CLAYFRAC', clayfrac, errmsg, errflg)
      if (errflg /= 0) return
      call bind_2d(handle, 'FRLAKE', frlake, errmsg, errflg)
      if (errflg /= 0) return
      call bind_2d(handle, 'FRSNO', frsno, errmsg, errflg)
      if (errflg /= 0) return
      call bind_2d(handle, 'GVF', gvf, errmsg, errflg)
      if (errflg /= 0) return
      call bind_2d(handle, 'LAI', lai, errmsg, errflg)
      if (errflg /= 0) return
      call bind_2d(handle, 'RDRAG', rdrag, errmsg, errflg)
      if (errflg /= 0) return
      call bind_2d(handle, 'SNDFRC', sandfrac, errmsg, errflg)
      if (errflg /= 0) return
      call bind_2d(handle, 'GWETTOP', gwetTop, errmsg, errflg)
      if (errflg /= 0) return
      call bind_2d(handle, 'SSM', ssm, errmsg, errflg)
      if (errflg /= 0) return
      call bind_2d(handle, 'TS', ts, errmsg, errflg)
      if (errflg /= 0) return
      call bind_2d(handle, 'U10M', u10m, errmsg, errflg)
      if (errflg /= 0) return
      call bind_2d(handle, 'V10M', v10m, errmsg, errflg)
      if (errflg /= 0) return
      call bind_2d(handle, 'USTAR', ustar, errmsg, errflg)
      if (errflg /= 0) return
      call bind_2d(handle, 'USTAR_THRESHOLD', ustar_threshold, errmsg, errflg)
      if (errflg /= 0) return
      call bind_2d(handle, 'Z0', z0, errmsg, errflg)
      if (errflg /= 0) return
      if (present(lwi)) then
         allocate (integer_mask_as_real(size(lwi)))
         integer_mask_as_real = real(lwi, kind_phys)
         call catchem_ccpp_bind_met_2d(handle, 'LWI', integer_mask_as_real, errmsg, errflg)
         if (errflg /= 0) return
      else if (present(landfrac)) then
         allocate (integer_mask_as_real(size(landfrac)))
         integer_mask_as_real = merge(1.0_kind_phys, 0.0_kind_phys, landfrac > 0.0_kind_phys)
         call catchem_ccpp_bind_met_2d(handle, 'LWI', integer_mask_as_real, errmsg, errflg)
         if (errflg /= 0) return
      end if
      call get_environment_variable('CATCHEM_ACCEPTANCE_SOIL_MOISTURE', acceptance_soil_text, &
                                    status=acceptance_environment_status)
      if (acceptance_environment_status == 0 .and. len_trim(acceptance_soil_text) > 0) then
         acceptance_soil_moisture = -1.0_kind_phys
         read (acceptance_soil_text, *, iostat=acceptance_read_status) acceptance_soil_moisture
         if (acceptance_read_status /= 0 .or. .not. ieee_is_finite(acceptance_soil_moisture) .or. &
             acceptance_soil_moisture <= 0.0_kind_phys .or. acceptance_soil_moisture > 1.0_kind_phys) then
            errmsg = 'Dust acceptance soil moisture must be finite and in (0,1] m3/m3.'
            errflg = 1
            return
         end if
         allocate (ginoux_surface_moisture(size(air_pressure, 1)))
         ginoux_surface_moisture = acceptance_soil_moisture
         call catchem_ccpp_bind_met_2d(handle, 'SSM', ginoux_surface_moisture, errmsg, errflg)
         if (errflg /= 0) return
         write (output_unit, '(a,es24.16)') 'CATCHEM_ACCEPTANCE_INPUT name=dust_soil_moisture_m3_m3 VALUE=', &
            real(acceptance_soil_moisture, c_double)
      else if (present(ssm)) then
         call bind_2d(handle, 'SSM', ssm, errmsg, errflg)
         if (errflg /= 0) return
      else if (present(soilm)) then
         if (size(soilm, 2) < 1) then
            errmsg = 'Dust requires at least one soil layer to derive the surface soil moisture input.'
            errflg = 1
            return
         end if
         allocate (ginoux_surface_moisture(size(soilm, 1)))
         ginoux_surface_moisture = soilm(:, 1)
         call catchem_ccpp_bind_met_2d(handle, 'SSM', ginoux_surface_moisture, errmsg, errflg)
         if (errflg /= 0) return
      end if
      allocate (staging(size(constituents, 1), size(constituents, 2), size(host_to_catchem)))
      ! Gather the complete mapped constituent state into CATChem staging arrays.
      call catchem_ccpp_gather(handle, host_to_catchem, constituents, host_density, dry_density, &
                               host_number_density, dry_number_density, staging, errmsg, errflg)
      if (errflg /= 0) return
      initial_staging = staging
      call catchem_ccpp_run_selected(handle, host_to_catchem, staging, interval_id, site, int(subcycle), 'dust', &
                        real(duration_seconds, c_double), operation_status, publication_eligible, import_generation, errmsg, errflg)
      if (errflg /= 0) return
      write (output_unit, '(a,es24.16)') 'CATCHEM_SIGNAL name=dust_constituent_max_abs_delta VALUE=', &
         maxval(abs(staging - initial_staging))
      ! Save the process result as a tendency for the host update scheme to apply.
      call catchem_ccpp_stage_tendency(handle, interval_token, 'dust', real(duration_seconds, c_double), &
                       host_to_catchem, constituents, staging, host_density, dry_density, host_number_density, dry_number_density, &
                                       errmsg, errflg)
      if (errflg == 0) write (output_unit, '(a)') 'CATCHEM_SELECTED_PROCESS=dust EXECUTIONS=1'
      if (errflg == 0) write (output_unit, '(a,a,a,i0,a,i0,a,es24.16)') &
         'CATCHEM_INVOCATION process=', 'dust', ' interval=', interval_id, ' subcycle=', subcycle, &
         ' duration_seconds=', real(duration_seconds, c_double)
   end subroutine catchem_dust_driver_run

   !> \brief Bind an optional column-by-level Dust input when the active variant provides it.
   !> \param handle Context receiving the input.
   !> \param name CATChem field name.
   !> \param values Optional host values.
   !> \param errmsg Error detail on failure.
   !> \param errflg CCPP error code.
   subroutine bind_3d(handle, name, values, errmsg, errflg)
      type(ccpp_context_handle), intent(in) :: handle
      character(len=*), intent(in) :: name
      real(kind_phys), optional, intent(in), target, contiguous :: values(:, :)
      character(len=*), intent(out) :: errmsg
      integer, intent(out) :: errflg
      errmsg = ''
      errflg = 0
      if (present(values)) call catchem_ccpp_bind_met_3d(handle, name, values, errmsg, errflg)
   end subroutine bind_3d

   !> \brief Bind an optional column-by-singleton Dust input when supplied by the host.
   !> \param handle Context receiving the input.
   !> \param name CATChem field name.
   !> \param values Optional host values.
   !> \param errmsg Error detail on failure.
   !> \param errflg CCPP error code.
   subroutine bind_2d(handle, name, values, errmsg, errflg)
      type(ccpp_context_handle), intent(in) :: handle
      character(len=*), intent(in) :: name
      real(kind_phys), optional, intent(in), target, contiguous :: values(:)
      character(len=*), intent(out) :: errmsg
      integer, intent(out) :: errflg
      errmsg = ''
      errflg = 0
      if (present(values)) call catchem_ccpp_bind_met_2d(handle, name, values, errmsg, errflg)
   end subroutine bind_2d

   !> \brief Bind an optional column-by-soil-layer field with its distinct semantic axis.
   !> \param handle Context receiving the input.
   !> \param name CATChem field name.
   !> \param values Optional host values.
   !> \param axis SemanticAxis ordinal.
   !> \param errmsg Error detail on failure.
   !> \param errflg CCPP error code.
   subroutine bind_vertical(handle, name, values, axis, errmsg, errflg)
      type(ccpp_context_handle), intent(in) :: handle
      character(len=*), intent(in) :: name
      real(kind_phys), optional, intent(in), target, contiguous :: values(:, :)
      integer, intent(in) :: axis
      character(len=*), intent(out) :: errmsg
      integer, intent(out) :: errflg
      errmsg = ''
      errflg = 0
      if (present(values)) call catchem_ccpp_bind_met_vertical(handle, name, values, axis, errmsg, errflg)
   end subroutine bind_vertical

   !> \brief Mark the Dust participant finalized for its host context.
   !> \param context_id Host-owned context slot.
   !> \param context_generation Context generation.
   !> \param errmsg Error detail on failure.
   !> \param errflg CCPP error code, zero on success.
   !> \section arg_table_catchem_dust_driver_final Argument Table
   !! \htmlinclude arg_table_catchem_dust_driver_final.hml
   !!
   subroutine catchem_dust_driver_final(context_id, context_generation, errmsg, errflg)
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
         errmsg = 'Dust finalization received an invalid context generation.'
         errflg = 1
      end if
   end subroutine catchem_dust_driver_final
end module catchem_dust_driver

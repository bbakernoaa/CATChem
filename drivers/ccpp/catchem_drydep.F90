!> \file catchem_drydep.F90
!! \brief Selected-process CCPP driver for CATChem dry deposition.
module catchem_drydep_driver
   use iso_c_binding, only: c_double, c_int, c_int64_t
   use iso_fortran_env, only: output_unit
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
   public :: catchem_drydep_driver_register, catchem_drydep_driver_init, &
             catchem_drydep_driver_run, catchem_drydep_driver_final
contains
   !> Register all species from the active mechanism for DryDep.
  !! @param context_id Host-owned CATChem context slot.
  !! @param context_generation Generation protecting the context slot.
  !! @param constituent_props Unique CCPP constituent registration.
  !! @param errmsg Registration error detail.
  !! @param errflg Zero on success; nonzero on failure.
   !> \section arg_table_catchem_drydep_driver_register Argument Table
   !! \htmlinclude arg_table_catchem_drydep_driver_register.hml
   !!
   subroutine catchem_drydep_driver_register(context_id, context_generation, constituent_props, errmsg, errflg)
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
         errmsg = 'DryDep registration requires a bound nonempty CATChem mechanism manifest.'
         errflg = max(1, int(status))
         return
      end if
      allocate (constituent_props(species_count), stat=errflg)
      if (errflg /= 0) then
         errmsg = 'DryDep could not allocate its constituent registration output.'
         return
      end if
      call catchem_ccpp_register_constituents(handle, constituent_props, errmsg, errflg)
   end subroutine catchem_drydep_driver_register

   !> Validate and retain the full host-to-mechanism constituent mapping.
  !! @param context_id Host-owned CATChem context slot.
  !! @param context_generation Generation protecting the context slot.
  !! @param constituent_properties Merged CCPP constituent property pointers.
  !! @param errmsg Initialization error detail.
  !! @param errflg Zero on success; nonzero on failure.
   !> \section arg_table_catchem_drydep_driver_init Argument Table
   !! \htmlinclude arg_table_catchem_drydep_driver_init.hml
   !!
   subroutine catchem_drydep_driver_init(context_id, context_generation, constituent_properties, errmsg, errflg)
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
         errmsg = 'DryDep could not install its verified context-local CCPP constituent map.'
         errflg = int(status)
      end if
   end subroutine catchem_drydep_driver_init

   !> Import GFS v17 meteorology and the full constituent array, execute DryDep only, and stage its tendency.
  !! @param context_id Host-owned CATChem context slot.
  !! @param context_generation Context generation.
  !! @param interval_id Host physics interval identity.
  !! @param interval_token Shared host tendency interval token.
  !! @param site Host coupling site.
  !! @param subcycle Host process subcycle.
  !! @param duration_seconds Invocation duration in seconds.
  !! @param constituents Complete CCPP constituent array in host order.
  !! @param pressure Layer pressure in Pa.
  !! @param temperature Layer temperature in K.
  !! @param humidity Specific humidity in kg kg-1.
  !! @param pressure_interface Pressure at model interfaces in Pa.
  !! @param geopotential_interface Interface geopotential in m2 s-2.
  !! @param cloud_fraction Three-dimensional instantaneous cloud fraction.
  !! @param surface_pressure Surface pressure in Pa.
  !! @param skin_temperature Surface skin temperature in K.
  !! @param latitude Latitude in radians from the GFS host, converted to degrees before CATChem import.
  !! @param longitude Longitude in radians from the GFS host, converted to degrees before CATChem import.
  !! @param friction_velocity Surface friction velocity in m s-1.
  !! @param sensible_heat_flux Upward sensible heat flux in W m-2.
  !! @param pbl_thickness Boundary-layer thickness in m.
  !! @param land_use Dominant vegetation classification index; zero on a land
  !!        column with land fraction or positive LAI uses the GFS SCM NoahMP
  !!        vegetation fallback category 7.
  !! @param leaf_area_index Leaf area index in m2 m-2.
  !! @param snow_fraction Surface snow area fraction.
  !! @param shortwave_down Surface downwelling shortwave flux in W m-2.
  !! @param roughness Surface momentum roughness length in cm, converted to m before CATChem import.
  !! @param lake_fraction Lake area fraction.
  !! @param normalized_soil_wetness Normalized top-layer soil wetness.
  !! @param land_fraction Land area fraction.
  !! @param sea_ice_fraction Sea-ice area fraction over sea.
  !! @param wind_u Eastward 10 m wind in m s-1.
  !! @param wind_v Northward 10 m wind in m s-1.
  !! @param errmsg Run error detail.
  !! @param errflg Zero on success; nonzero on failure.
   !> \section arg_table_catchem_drydep_driver_run Argument Table
   !! \htmlinclude arg_table_catchem_drydep_driver_run.hml
   !!
   subroutine catchem_drydep_driver_run(context_id, context_generation, interval_id, interval_token, site, subcycle, &
                      duration_seconds, constituents, pressure, temperature, humidity, pressure_interface, geopotential_interface, &
                   cloud_fraction, surface_pressure, skin_temperature, latitude, longitude, friction_velocity, sensible_heat_flux, &
                                pbl_thickness, land_use, leaf_area_index, snow_fraction, shortwave_down, roughness, lake_fraction, &
                                        normalized_soil_wetness, land_fraction, sea_ice_fraction, wind_u, wind_v, errmsg, errflg)
      integer(c_int), intent(in) :: context_id, subcycle
      integer(c_int64_t), intent(in) :: context_generation, interval_id, interval_token
      character(len=*), intent(in) :: site
      real(kind_phys), intent(in) :: duration_seconds, constituents(:, :, :)
      real(kind_phys), intent(in) :: pressure(:, :), temperature(:, :), humidity(:, :), pressure_interface(:, :)
      real(kind_phys), intent(in) :: geopotential_interface(:, :), cloud_fraction(:, :), surface_pressure(:), &
                                     skin_temperature(:)
      real(kind_phys), intent(in) :: latitude(:), longitude(:), friction_velocity(:), sensible_heat_flux(:)
      real(kind_phys), intent(in) :: pbl_thickness(:), leaf_area_index(:), snow_fraction(:), shortwave_down(:)
      real(kind_phys), intent(in) :: roughness(:), lake_fraction(:), normalized_soil_wetness(:), land_fraction(:)
      real(kind_phys), intent(in) :: sea_ice_fraction(:), wind_u(:), wind_v(:)
      integer, intent(in) :: land_use(:)
      character(len=*), intent(out) :: errmsg
      integer, intent(out) :: errflg
      type(ccpp_context_handle) :: handle
      integer, allocatable :: host_to_catchem(:)
      real(kind_phys), allocatable :: height_interface(:, :), host_density(:, :), dry_density(:, :)
      real(kind_phys), allocatable :: host_number_density(:, :), dry_number_density(:, :), lwi(:), normalized_land_use(:)
      real(kind_phys), allocatable :: latitude_degrees(:), longitude_degrees(:), roughness_metres(:)
      real(c_double), allocatable :: pressure_c(:, :), temperature_c(:, :), humidity_c(:, :)
      real(c_double), allocatable :: host_density_c(:, :), dry_density_c(:, :), host_number_density_c(:, :), &
                                     dry_number_density_c(:, :)
      real(c_double), allocatable :: staging(:, :, :), initial_staging(:, :, :)
      integer(c_int) :: status, operation_status
      integer(c_int64_t) :: import_generation
      logical :: publication_eligible
      integer :: col

      logical :: process_active
      handle%id = context_id
      handle%generation = context_generation
      call catchem_ccpp_process_is_active(handle, 'drydep', process_active, errmsg, errflg)
      if (errflg /= 0 .or. .not. process_active) return
      errmsg = ''
      errflg = 0
      call catchem_ccpp_context_get_mapping(handle, host_to_catchem, operation_status)
      if (operation_status /= 0_c_int) then
         errmsg = 'DryDep run requires the shared host constituent map from scheme initialization.'
         errflg = int(operation_status)
         return
      end if
      if (any(shape(temperature) /= shape(pressure)) .or. any(shape(humidity) /= shape(pressure)) .or. &
          any(shape(cloud_fraction) /= shape(pressure)) .or. size(pressure_interface, 1) /= size(pressure, 1) .or. &
          size(pressure_interface, 2) /= size(pressure, 2) + 1 .or. &
          size(geopotential_interface, 1) /= size(pressure, 1) .or. &
          size(geopotential_interface, 2) /= size(pressure, 2) + 1) then
         errmsg = 'DryDep layer and interface input extents are inconsistent.'
         errflg = 1
         return
      end if
      if (any([size(surface_pressure), size(skin_temperature), size(latitude), size(longitude), size(friction_velocity), &
               size(sensible_heat_flux), size(pbl_thickness), size(land_use), size(leaf_area_index), size(snow_fraction), &
               size(shortwave_down), size(roughness), size(lake_fraction), size(normalized_soil_wetness), &
               size(land_fraction), size(sea_ice_fraction), size(wind_u), size(wind_v)] /= size(pressure, 1))) then
         errmsg = 'DryDep requires one complete surface input value per host column.'
         errflg = 1
         return
      end if
      allocate (pressure_c(size(pressure, 1), size(pressure, 2)), temperature_c(size(pressure, 1), size(pressure, 2)), &
                humidity_c(size(pressure, 1), size(pressure, 2)), host_density_c(size(pressure, 1), size(pressure, 2)), &
                dry_density_c(size(pressure, 1), size(pressure, 2)), host_number_density_c(size(pressure, 1), size(pressure, 2)), &
                dry_number_density_c(size(pressure, 1), size(pressure, 2)), host_density(size(pressure, 1), size(pressure, 2)), &
                dry_density(size(pressure, 1), size(pressure, 2)), host_number_density(size(pressure, 1), size(pressure, 2)), &
             dry_number_density(size(pressure, 1), size(pressure, 2)), height_interface(size(pressure, 1), size(pressure, 2) + 1), &
                lwi(size(pressure, 1)), normalized_land_use(size(pressure, 1)), &
                latitude_degrees(size(pressure, 1)), longitude_degrees(size(pressure, 1)), &
                roughness_metres(size(pressure, 1)))
      pressure_c = real(pressure, c_double)
      temperature_c = real(temperature, c_double)
      humidity_c = real(humidity, c_double)
      call catchem_gfs_density_state(pressure_c, temperature_c, humidity_c, host_density_c, dry_density_c, &
                                     host_number_density_c, dry_number_density_c, status)
      if (status /= 0_c_int) then
         errmsg = 'DryDep requires positive physical pressure and temperature with valid humidity.'
         errflg = int(status)
         return
      end if
      host_density = real(host_density_c, kind_phys)
      dry_density = real(dry_density_c, kind_phys)
      host_number_density = real(host_number_density_c, kind_phys)
      dry_number_density = real(dry_number_density_c, kind_phys)
      height_interface = geopotential_interface/9.80665_kind_phys
      latitude_degrees = latitude*(180.0_kind_phys/acos(-1.0_kind_phys))
      longitude_degrees = longitude*(180.0_kind_phys/acos(-1.0_kind_phys))
      roughness_metres = roughness/100.0_kind_phys
      do col = 1, size(pressure, 1)
         ! GFS SCM ARM land cases can leave vtype at zero while NoahMP resolves
         ! that category to vegetation type 7 for its surface climatology.
         ! Preserve explicit host categories and apply that same fallback when
         ! the SCM's land fraction is missing but its canopy LAI identifies land.
         normalized_land_use(col) = real(land_use(col), kind_phys)
         if (land_use(col) == 0 .and. (land_fraction(col) > 0.0_kind_phys .or. &
                                       leaf_area_index(col) > 0.0_kind_phys) .and. &
             sea_ice_fraction(col) <= 0.5_kind_phys) normalized_land_use(col) = 7.0_kind_phys
         if (sea_ice_fraction(col) > 0.5_kind_phys) then
            lwi(col) = 2.0_kind_phys
         else if (land_fraction(col) > 0.0_kind_phys .or. leaf_area_index(col) > 0.0_kind_phys) then
            lwi(col) = 1.0_kind_phys
         else
            lwi(col) = 0.0_kind_phys
         end if
      end do

      ! Bind this call's host fields in a fresh import generation.
      call catchem_ccpp_begin_import(handle, errmsg, errflg, 'drydep')
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_3d(handle, 'PMID', pressure, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_3d(handle, 'T', temperature, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_3d(handle, 'QV', humidity, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_3d(handle, 'AIRDEN', host_density, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_3d(handle, 'CLDF', cloud_fraction, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_vertical(handle, 'PEDGE', pressure_interface, 1, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_vertical(handle, 'Z', height_interface, 1, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_2d(handle, 'PS', surface_pressure, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_2d(handle, 'TS', skin_temperature, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_2d(handle, 'LAT', latitude_degrees, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_2d(handle, 'LON', longitude_degrees, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_2d(handle, 'USTAR', friction_velocity, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_2d(handle, 'HFLUX', sensible_heat_flux, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_2d(handle, 'PBLH', pbl_thickness, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_2d(handle, 'DLUSE', normalized_land_use, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_2d(handle, 'LAI', leaf_area_index, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_2d(handle, 'FRSNO', snow_fraction, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_2d(handle, 'SWGDN', shortwave_down, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_2d(handle, 'Z0', roughness_metres, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_2d(handle, 'FRLAKE', lake_fraction, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_2d(handle, 'GWETTOP', normalized_soil_wetness, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_2d(handle, 'LWI', lwi, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_2d(handle, 'U10M', wind_u, errmsg, errflg)
      if (errflg /= 0) return
      call catchem_ccpp_bind_met_2d(handle, 'V10M', wind_v, errmsg, errflg)
      if (errflg /= 0) return
      allocate (staging(size(constituents, 1), size(constituents, 2), size(host_to_catchem)))
      ! Gather the complete mapped constituent state into CATChem staging arrays.
      call catchem_ccpp_gather(handle, host_to_catchem, constituents, host_density, dry_density, &
                               host_number_density, dry_number_density, staging, errmsg, errflg)
      if (errflg /= 0) return
      initial_staging = staging
      call catchem_ccpp_run_selected(handle, host_to_catchem, staging, interval_id, site, int(subcycle), 'drydep', &
                        real(duration_seconds, c_double), operation_status, publication_eligible, import_generation, errmsg, errflg)
      if (errflg /= 0) return
      write (*, '(a,es24.16)') 'CATCHEM_SIGNAL name=drydep_constituent_max_abs_delta VALUE=', &
         maxval(abs(staging - initial_staging))
      ! Save the process result as a tendency for the host update scheme to apply.
      call catchem_ccpp_stage_tendency(handle, interval_token, 'drydep', real(duration_seconds, c_double), &
                       host_to_catchem, constituents, staging, host_density, dry_density, host_number_density, dry_number_density, &
                                       errmsg, errflg)
      if (errflg == 0) write (*, '(a)') 'CATCHEM_SELECTED_PROCESS=drydep EXECUTIONS=1'
      if (errflg == 0) write (output_unit, '(a,a,a,i0,a,i0,a,es24.16)') &
         'CATCHEM_INVOCATION process=', 'drydep', ' interval=', interval_id, ' subcycle=', subcycle, &
         ' duration_seconds=', real(duration_seconds, c_double)
   end subroutine catchem_drydep_driver_run

   !> Finalize the DryDep participant for its host context generation.
  !! @param context_id Host-owned CATChem context slot.
  !! @param context_generation Context generation.
  !! @param errmsg Error detail.
  !! @param errflg Zero on success; nonzero on failure.
   !> \section arg_table_catchem_drydep_driver_final Argument Table
   !! \htmlinclude arg_table_catchem_drydep_driver_final.hml
   !!
   subroutine catchem_drydep_driver_final(context_id, context_generation, errmsg, errflg)
      integer(c_int), intent(in) :: context_id
      integer(c_int64_t), intent(in) :: context_generation
      character(len=*), intent(out) :: errmsg
      integer, intent(out) :: errflg
      errmsg = ''
      errflg = 0
      if (context_id < 0_c_int .or. context_generation <= 0_c_int64_t) then
         errmsg = 'DryDep finalization received an invalid context identity.'
         errflg = 1
      end if
   end subroutine catchem_drydep_driver_final
end module catchem_drydep_driver

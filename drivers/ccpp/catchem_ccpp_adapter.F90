! Per-scheme adapter between the CCPP unified constituent array and a
! context-owned CATChem model. CCPP indices remain one-based at this boundary.
module catchem_ccpp_adapter
  use iso_c_binding, only: c_char, c_double, c_int, c_int32_t, c_int64_t, c_ptr, c_loc, c_null_char
  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
  use machine, only: kind_phys
  use ccpp_constituent_prop_mod, only: ccpp_constituent_properties_t, ccpp_constituent_prop_ptr_t
  use CATChem_API, only: CATChem_Model
  use CATChem_API, only: CATChem_Manifest_Species_v1
  use catchem_ccpp_conversion, only: catchem_gas_mass_ratio_to_dry_ppmv, &
    catchem_dry_ppmv_to_gas_mass_ratio, catchem_aerosol_mass_ratio_to_dry_ug_kg, &
    catchem_host_ppmv_to_dry, CATCHEM_AMOUNT_MOLE_FRACTION, CATCHEM_AMOUNT_MASS_RATIO, &
    CATCHEM_AIR_DRY, CATCHEM_AIR_MOIST, CATCHEM_AIR_TOTAL_CONDENSATE, CATCHEM_CONVERSION_OK
  use catchem_ccpp_context, only: ccpp_context_handle, catchem_ccpp_context_species, &
    catchem_ccpp_context_model, catchem_ccpp_context_stage, catchem_ccpp_context_species_count, &
    catchem_ccpp_context_set_mapping, catchem_ccpp_context_get_mapping, &
    catchem_ccpp_context_stage_tendency, catchem_ccpp_context_apply_tendency, &
    catchem_ccpp_context_cancel_tendency, catchem_ccpp_context_cancel, catchem_ccpp_context_buffers, &
    CATCHEM_CONTEXT_SUCCESS
  implicit none
  private
  integer, parameter :: amount_unspecified = 0, amount_mole_fraction = 1
  integer, parameter :: amount_mass_ratio = 2, amount_number = 3
  integer, parameter :: basis_dry = 1, basis_moist = 2, basis_total = 3
  public :: catchem_ccpp_register_constituents, catchem_ccpp_build_mapping
  public :: catchem_ccpp_gather, catchem_ccpp_run_selected, catchem_ccpp_convert_to_host
  public :: catchem_ccpp_stage_tendency, catchem_ccpp_apply_tendency, catchem_ccpp_cancel_tendency
  public :: catchem_ccpp_begin_import, catchem_ccpp_bind_met_2d, catchem_ccpp_bind_met_3d
  public :: catchem_ccpp_bind_met_vertical

  interface
    integer(c_int) function catchem_state_bind_met_2d_checked(state_ptr, name, values, ncol, nsingleton) &
        bind(C, name="catchem_state_bind_met_2d_checked")
      import :: c_ptr, c_char, c_int
      type(c_ptr), value :: state_ptr, values
      character(kind=c_char), intent(in) :: name(*)
      integer(c_int), value :: ncol, nsingleton
    end function
    integer(c_int) function catchem_state_bind_met_3d_checked(state_ptr, name, values, ncol, nlev, nsingleton) &
        bind(C, name="catchem_state_bind_met_3d_checked")
      import :: c_ptr, c_char, c_int
      type(c_ptr), value :: state_ptr, values
      character(kind=c_char), intent(in) :: name(*)
      integer(c_int), value :: ncol, nlev, nsingleton
    end function
    integer(c_int) function catchem_state_bind_met_3d_axis_checked(state_ptr, name, values, ncol, nvertical, &
                                                                    nsingleton, axis) &
        bind(C, name="catchem_state_bind_met_3d_axis_checked")
      import :: c_ptr, c_char, c_int
      type(c_ptr), value :: state_ptr, values
      character(kind=c_char), intent(in) :: name(*)
      integer(c_int), value :: ncol, nvertical, nsingleton, axis
    end function
    integer(c_int) function catchem_state_begin_import_generation(state_ptr) &
        bind(C, name="catchem_state_begin_import_generation")
      import :: c_ptr, c_int
      type(c_ptr), value :: state_ptr
    end function
    integer(c_int) function catchem_state_bind_unified_chemistry_checked(state_ptr, values, ncol, nlev, nspec) &
        bind(C, name="catchem_state_bind_unified_chemistry_checked")
      import :: c_ptr, c_int
      type(c_ptr), value :: state_ptr, values
      integer(c_int), value :: ncol, nlev, nspec
    end function
  end interface

contains

  !> \brief Start one timestep input generation before binding this call's host fields.
  !> \param handle Host-owned context receiving the imports.
  !> \param errmsg Error detail when the context is absent or invalid.
  !> \param errflg CCPP error code, zero on success.
  subroutine catchem_ccpp_begin_import(handle, errmsg, errflg)
    type(ccpp_context_handle), intent(in) :: handle
    character(len=*), intent(out) :: errmsg
    integer, intent(out) :: errflg
    type(CATChem_Model), pointer :: model
    integer(c_int) :: status
    errmsg = ''
    errflg = 0
    call catchem_ccpp_context_model(handle, model, status)
    if (status /= CATCHEM_CONTEXT_SUCCESS) then
      errmsg = 'CATChem import requires a valid initialized context.'
      errflg = int(status)
      return
    end if
    status = catchem_state_begin_import_generation(model%state_mgr_ptr)
    if (status /= 0_c_int) then
      errmsg = 'CATChem could not start an input import generation.'
      errflg = int(status)
    end if
  end subroutine catchem_ccpp_begin_import

  !> \brief Bind a column-by-singleton host field in the active import generation.
  !> \param handle Context owning the CATChem model.
  !> \param name Canonical meteorology field name.
  !> \param values Host field values.
  !> \param errmsg Error detail on failure.
  !> \param errflg CCPP error code, zero on success.
  subroutine catchem_ccpp_bind_met_2d(handle, name, values, errmsg, errflg)
    type(ccpp_context_handle), intent(in) :: handle
    character(len=*), intent(in) :: name
    real(kind_phys), intent(in), target, contiguous :: values(:)
    character(len=*), intent(out) :: errmsg
    integer, intent(out) :: errflg
    type(CATChem_Model), pointer :: model
    integer(c_int) :: status
    errmsg = ''
    errflg = 0
    if (size(values) == 0) then
      errmsg = 'CATChem cannot bind an empty host field '//trim(name)//'.'
      errflg = 1
      return
    end if
    if (storage_size(values(1)) /= storage_size(0.0_c_double)) then
      errmsg = 'CATChem host field '//trim(name)//' does not use the required double-precision storage.'
      errflg = 1
      return
    end if
    call catchem_ccpp_context_model(handle, model, status)
    if (status == CATCHEM_CONTEXT_SUCCESS) status = catchem_state_bind_met_2d_checked( &
      model%state_mgr_ptr, trim(name)//c_null_char, c_loc(values(1)), int(size(values),c_int), 1_c_int)
    if (status /= 0_c_int) then
      errmsg = 'CATChem failed to bind host field '//trim(name)//'.'
      errflg = int(status)
    end if
  end subroutine catchem_ccpp_bind_met_2d

  !> \brief Bind a column-by-level host field in the active import generation.
  !> \param handle Context owning the CATChem model.
  !> \param name Canonical meteorology field name.
  !> \param values Host field values.
  !> \param errmsg Error detail on failure.
  !> \param errflg CCPP error code, zero on success.
  subroutine catchem_ccpp_bind_met_3d(handle, name, values, errmsg, errflg)
    type(ccpp_context_handle), intent(in) :: handle
    character(len=*), intent(in) :: name
    real(kind_phys), intent(in), target, contiguous :: values(:,:)
    character(len=*), intent(out) :: errmsg
    integer, intent(out) :: errflg
    type(CATChem_Model), pointer :: model
    integer(c_int) :: status
    errmsg = ''
    errflg = 0
    if (size(values,1) == 0 .or. size(values,2) == 0) then
      errmsg = 'CATChem cannot bind an empty host field '//trim(name)//'.'
      errflg = 1
      return
    end if
    if (storage_size(values(1,1)) /= storage_size(0.0_c_double)) then
      errmsg = 'CATChem host field '//trim(name)//' does not use the required double-precision storage.'
      errflg = 1
      return
    end if
    call catchem_ccpp_context_model(handle, model, status)
    if (status == CATCHEM_CONTEXT_SUCCESS) status = catchem_state_bind_met_3d_checked( &
      model%state_mgr_ptr, trim(name)//c_null_char, c_loc(values(1,1)), &
      int(size(values,1),c_int), int(size(values,2),c_int), 1_c_int)
    if (status /= 0_c_int) then
      errmsg = 'CATChem failed to bind host field '//trim(name)//'.'
      errflg = int(status)
    end if
  end subroutine catchem_ccpp_bind_met_3d

  !> \brief Bind a host field with a nonstandard vertical axis such as soil layers.
  !> \param handle Context owning the CATChem model.
  !> \param name Canonical meteorology field name.
  !> \param values Host field values.
  !> \param axis SemanticAxis ordinal used by the checked state API.
  !> \param errmsg Error detail on failure.
  !> \param errflg CCPP error code, zero on success.
  subroutine catchem_ccpp_bind_met_vertical(handle, name, values, axis, errmsg, errflg)
    type(ccpp_context_handle), intent(in) :: handle
    character(len=*), intent(in) :: name
    real(kind_phys), intent(in), target, contiguous :: values(:,:)
    integer, intent(in) :: axis
    character(len=*), intent(out) :: errmsg
    integer, intent(out) :: errflg
    type(CATChem_Model), pointer :: model
    integer(c_int) :: status
    errmsg = ''
    errflg = 0
    if (size(values,1) == 0 .or. size(values,2) == 0) then
      errmsg = 'CATChem cannot bind an empty host field '//trim(name)//'.'
      errflg = 1
      return
    end if
    if (storage_size(values(1,1)) /= storage_size(0.0_c_double)) then
      errmsg = 'CATChem host field '//trim(name)//' does not use the required double-precision storage.'
      errflg = 1
      return
    end if
    call catchem_ccpp_context_model(handle, model, status)
    if (status == CATCHEM_CONTEXT_SUCCESS) status = catchem_state_bind_met_3d_axis_checked( &
      model%state_mgr_ptr, trim(name)//c_null_char, c_loc(values(1,1)), int(size(values,1),c_int), &
      int(size(values,2),c_int), 1_c_int, int(axis,c_int))
    if (status /= 0_c_int) then
      errmsg = 'CATChem failed to bind host field '//trim(name)//' with its requested vertical axis.'
      errflg = int(status)
    end if
  end subroutine catchem_ccpp_bind_met_vertical

  !> \brief Implements catchem ccpp register constituents for the CATChem CCPP boundary.
  !> \param handle Argument used by catchem_ccpp_register_constituents.
  !> \param props Argument used by catchem_ccpp_register_constituents.
  !> \param errmsg Argument used by catchem_ccpp_register_constituents.
  !> \param errflg Argument used by catchem_ccpp_register_constituents.
  subroutine catchem_ccpp_register_constituents(handle, props, errmsg, errflg)
    type(ccpp_context_handle), intent(in) :: handle
    type(ccpp_constituent_properties_t), intent(out) :: props(:)
    character(len=*), intent(out) :: errmsg
    integer, intent(out) :: errflg
    type(CATChem_Manifest_Species_v1) :: d
    character(len=512) :: name, standard_name, units, mixing_ratio_type
    integer(c_int) :: status
    integer :: i
    real(kind_phys) :: mw
    logical :: gas, aerosol, advected

    errmsg = ''
    errflg = 0
    if (size(props) <= 0) then
      errmsg = 'CATChem registration requires at least one configured species.'
      errflg = 1
      return
    end if
    do i = 1, size(props)
      call catchem_ccpp_context_species(handle, i, d, name, standard_name, units, status)
      if (status /= CATCHEM_CONTEXT_SUCCESS) then
        errmsg = 'CATChem registration could not read immutable manifest species metadata.'
        errflg = int(status)
        return
      end if
      gas = btest(d%flags,0)
      aerosol = btest(d%flags,1)
      advected = btest(d%flags,2)
      if ((gas .and. aerosol) .or. (advected .and. .not. gas .and. .not. aerosol)) then
        errmsg = 'Advected CATChem species must have exactly one gas/aerosol classification: '//trim(name)
        errflg = 1
        return
      end if
      select case (d%air_basis)
      case (basis_dry)
        mixing_ratio_type = 'dry'
      case (basis_moist)
        mixing_ratio_type = 'moist'
      case (basis_total)
        mixing_ratio_type = 'wet'
      case default
        errmsg = 'CATChem species has no supported explicit CCPP air basis: '//trim(name)
        errflg = 1
        return
      end select
      mw = real(d%molecular_weight_kg_mol,kind_phys)
      if (gas) then
        if (.not. ieee_is_finite(mw)) then
          errmsg = 'CATChem gas species has nonfinite molar mass: '//trim(name)
          errflg = 1
          return
        end if
        if (mw <= 0.0_kind_phys) then
          errmsg = 'CATChem gas species has nonpositive molar mass: '//trim(name)
          errflg = 1
          return
        end if
      end if
      if (gas) then
        call props(i)%instantiate(std_name=trim(standard_name), long_name=trim(name), diag_name=trim(name), &
          units=trim(units), vertical_dim='vertical_layer_dimension', advected=advected, &
          default_value=0.0_kind_phys, min_value=0.0_kind_phys, molar_mass=mw, &
          mixing_ratio_type=trim(mixing_ratio_type), errcode=errflg, errmsg=errmsg)
      else
        call props(i)%instantiate(std_name=trim(standard_name), long_name=trim(name), diag_name=trim(name), &
          units=trim(units), vertical_dim='vertical_layer_dimension', advected=advected, &
          default_value=0.0_kind_phys, min_value=0.0_kind_phys, &
          mixing_ratio_type=trim(mixing_ratio_type), errcode=errflg, errmsg=errmsg)
      end if
      if (errflg /= 0) return
    end do
  end subroutine

  !> \brief Implements catchem ccpp build mapping for the CATChem CCPP boundary.
  !> \param handle Argument used by catchem_ccpp_build_mapping.
  !> \param props Argument used by catchem_ccpp_build_mapping.
  !> \param host_to_catchem Argument used by catchem_ccpp_build_mapping.
  !> \param errmsg Argument used by catchem_ccpp_build_mapping.
  !> \param errflg Argument used by catchem_ccpp_build_mapping.
  subroutine catchem_ccpp_build_mapping(handle, props, host_to_catchem, errmsg, errflg)
    type(ccpp_context_handle), intent(in) :: handle
    type(ccpp_constituent_prop_ptr_t), intent(in) :: props(:)
    integer, allocatable, intent(out) :: host_to_catchem(:)
    character(len=*), intent(out) :: errmsg
    integer, intent(out) :: errflg
    type(CATChem_Manifest_Species_v1) :: d
    character(len=512) :: name, expected_name, units, actual_name, actual_units, actual_long, actual_diag, actual_vertical
    integer(c_int) :: status
    integer :: i, j, index, property_rc, nspec
    logical :: found, advected, is_mass, is_volume, is_number, is_dry, is_moist, is_wet
    logical :: thermo_active, water_species
    real(kind_phys) :: mw, min_value, default_value

    errmsg = ''
    errflg = 0
    call catchem_ccpp_context_species_count(handle,nspec,status)
    if (status /= CATCHEM_CONTEXT_SUCCESS .or. nspec <= 0) then
      errmsg = 'CATChem mapping could not query the registered mechanism size.'
      errflg = max(1,int(status))
      return
    end if
    allocate(host_to_catchem(nspec))
    host_to_catchem = 0
    do i = 1, size(props)
      call props(i)%standard_name(actual_name,property_rc,errmsg)
      if (property_rc /= 0) then
        errflg = property_rc
        return
      end if
      call props(i)%units(actual_units,property_rc,errmsg)
      if (property_rc /= 0) then
        errflg = property_rc
        return
      end if
      call props(i)%long_name(actual_long,property_rc,errmsg)
      if (property_rc /= 0) then
        errflg = property_rc
        return
      end if
      call props(i)%diagnostic_name(actual_diag,property_rc,errmsg)
      if (property_rc /= 0) then
        errflg = property_rc
        return
      end if
      call props(i)%vertical_dimension(actual_vertical,property_rc,errmsg)
      if (property_rc /= 0) then
        errflg = property_rc
        return
      end if
      call props(i)%is_advected(advected,property_rc,errmsg)
      if (property_rc /= 0) then
        errflg = property_rc
        return
      end if
      call props(i)%is_mass_mixing_ratio(is_mass,property_rc,errmsg)
      if (property_rc /= 0) then
        errflg = property_rc
        return
      end if
      call props(i)%is_volume_mixing_ratio(is_volume,property_rc,errmsg)
      if (property_rc /= 0) then
        errflg = property_rc
        return
      end if
      call props(i)%is_number_concentration(is_number,property_rc,errmsg)
      if (property_rc /= 0) then
        errflg = property_rc
        return
      end if
      call props(i)%is_dry(is_dry,property_rc,errmsg)
      if (property_rc /= 0) then
        errflg = property_rc
        return
      end if
      call props(i)%is_moist(is_moist,property_rc,errmsg)
      if (property_rc /= 0) then
        errflg = property_rc
        return
      end if
      call props(i)%is_wet(is_wet,property_rc,errmsg)
      if (property_rc /= 0) then
        errflg = property_rc
        return
      end if
      call props(i)%is_thermo_active(thermo_active,property_rc,errmsg)
      if (property_rc /= 0) then
        errflg = property_rc
        return
      end if
      call props(i)%is_water_species(water_species,property_rc,errmsg)
      if (property_rc /= 0) then
        errflg = property_rc
        return
      end if
      call props(i)%minimum(min_value,property_rc,errmsg)
      if (property_rc /= 0) then
        errflg = property_rc
        return
      end if
      call props(i)%default_value(default_value,property_rc,errmsg)
      if (property_rc /= 0) then
        errflg = property_rc
        return
      end if
      call props(i)%molar_mass(mw,property_rc,errmsg)
      if (property_rc /= 0) then
        errflg = property_rc
        return
      end if
      call props(i)%const_index(index,property_rc,errmsg)
      if (property_rc /= 0 .or. index <= 0) then
        errmsg = 'CCPP constituent property has no positive registered 1-based index.'
        errflg = 1
        return
      end if

      do j = 1, nspec
        call catchem_ccpp_context_species(handle,j,d,name,expected_name,units,status)
        if (status /= CATCHEM_CONTEXT_SUCCESS) then
          errmsg = 'CATChem mapping could not read manifest metadata.'
          errflg = int(status)
          return
        end if
        if (trim(actual_name) /= trim(expected_name)) cycle
        if (host_to_catchem(j) /= 0) then
          errmsg = 'Two CCPP host slots share one CATChem standard name: '//trim(actual_name)
          errflg = 1
          return
        end if
        if (trim(actual_units) /= trim(units) .or. (advected .neqv. btest(d%flags,2)) .or. &
            trim(actual_long) /= trim(name) .or. trim(actual_diag) /= trim(name) .or. &
            trim(actual_vertical) /= 'vertical_layer_dimension' .or. thermo_active .or. water_species .or. &
            abs(min_value) > tiny(1.0_kind_phys) .or. abs(default_value) > tiny(1.0_kind_phys)) then
          errmsg = 'CCPP full property signature conflicts with CATChem registration: '//trim(name)
          errflg = 1
          return
        end if
        select case (d%amount_type)
        case (amount_unspecified,amount_mass_ratio)
          found = is_mass
        case (amount_mole_fraction)
          found = is_volume
        case (amount_number)
          found = is_number
        case default
          found = .false.
        end select
        if (.not. found) then
          errmsg = 'CCPP amount type conflicts with CATChem manifest: '//trim(name)
          errflg = 1
          return
        end if
        select case (d%air_basis)
        case (basis_dry)
          found = is_dry
        case (basis_moist)
          found = is_moist
        case (basis_total)
          found = is_wet
        case default
          found = .false.
        end select
        if (.not. found) then
          errmsg = 'CCPP air basis conflicts with CATChem manifest: '//trim(name)
          errflg = 1
          return
        end if
        if (btest(d%flags,0)) then
          if (.not. ieee_is_finite(mw)) then
            errmsg = 'CCPP gas molar mass is nonfinite: '//trim(name)
            errflg = 1
            return
          end if
          if (abs(real(mw,c_double)-d%molecular_weight_kg_mol) > &
              1.0e-10_c_double*max(1.0_c_double,abs(d%molecular_weight_kg_mol))) then
            errmsg = 'CCPP molar mass conflicts with CATChem manifest: '//trim(name)
            errflg = 1
            return
          end if
        else if (mw < huge(1.0_kind_phys)) then
          errmsg = 'CCPP aerosol property unexpectedly carries a gas molar mass: '//trim(name)
          errflg = 1
          return
        end if
        host_to_catchem(j) = index
      end do
    end do
    do j = 1, nspec
      if (host_to_catchem(j) == 0) then
        call catchem_ccpp_context_species(handle,j,d,name,expected_name,units,status)
        errmsg = 'CCPP host registry is missing required CATChem constituent: '//trim(expected_name)
        errflg = max(1,int(status))
        return
      end if
    end do
    do i = 1, nspec
      if (any(host_to_catchem(1:i-1) == host_to_catchem(i))) then
        errmsg = 'Two distinct CATChem species map to the same CCPP host slot.'
        errflg = 1
        return
      end if
    end do
    call catchem_ccpp_context_set_mapping(handle,host_to_catchem,status)
    if (status /= CATCHEM_CONTEXT_SUCCESS) then
      errmsg = 'CATChem context rejected the instance-local constituent map.'
      errflg = int(status)
      return
    end if
  end subroutine

  !> \brief Implements catchem ccpp gather for the CATChem CCPP boundary.
  !> \param handle Argument used by catchem_ccpp_gather.
  !> \param host_to_catchem Argument used by catchem_ccpp_gather.
  !> \param host Argument used by catchem_ccpp_gather.
  !> \param host_density Argument used by catchem_ccpp_gather.
  !> \param dry_density Argument used by catchem_ccpp_gather.
  subroutine catchem_ccpp_gather(handle, host_to_catchem, host, host_density, dry_density, &
                                 host_number_density, dry_number_density, staging, errmsg, errflg)
    type(ccpp_context_handle), intent(in) :: handle
    integer, intent(in) :: host_to_catchem(:)
    real(kind_phys), intent(in) :: host(:,:,:)
    real(kind_phys), intent(in) :: host_density(:,:), dry_density(:,:)
    real(kind_phys), intent(in) :: host_number_density(:,:), dry_number_density(:,:)
    real(c_double), intent(out) :: staging(:,:,:)
    character(len=*), intent(out) :: errmsg
    integer, intent(out) :: errflg
    type(CATChem_Manifest_Species_v1) :: d
    character(len=512) :: name, standard_name, units
    integer(c_int) :: cstatus, cvt_status, amount_type, air_basis
    integer :: i, slot, col, lev
    real(c_double) :: host_value, converted
    errmsg = ''
    errflg = 0
    if (size(staging,1) /= size(host,1) .or. size(staging,2) /= size(host,2) .or. &
        size(staging,3) /= size(host_to_catchem) .or. size(host_density,1) /= size(host,1) .or. &
        size(host_density,2) /= size(host,2) .or. size(dry_density,1) /= size(host,1) .or. &
        size(dry_density,2) /= size(host,2) .or. size(host_number_density,1) /= size(host,1) .or. &
        size(host_number_density,2) /= size(host,2) .or. size(dry_number_density,1) /= size(host,1) .or. &
        size(dry_number_density,2) /= size(host,2)) then
      errmsg = 'CATChem gather extents do not match host grid and manifest species.'
      errflg = 1
      return
    end if
    do i = 1, size(host_to_catchem)
      slot = host_to_catchem(i)
      if (slot < 1 .or. slot > size(host,3)) then
        errmsg = 'CATChem gather map contains an invalid CCPP 1-based slot.'
        errflg = 1
        return
      end if
      call catchem_ccpp_context_species(handle,i,d,name,standard_name,units,cstatus)
      if (cstatus /= CATCHEM_CONTEXT_SUCCESS) then
        errmsg = 'CATChem gather could not read species conversion metadata.'
        errflg = int(cstatus)
        return
      end if
      air_basis = int(d%air_basis,c_int)
      amount_type = int(d%amount_type,c_int)
      if (amount_type /= CATCHEM_AMOUNT_MOLE_FRACTION .and. amount_type /= CATCHEM_AMOUNT_MASS_RATIO) then
        errmsg = 'CATChem gather rejects unsupported constituent amount type: '//trim(name)
        errflg = 1
        return
      end if
      if (amount_type == CATCHEM_AMOUNT_MOLE_FRACTION .and. .not. btest(d%flags,0)) then
        errmsg = 'Aerosol constituent cannot use a gas mole-fraction representation: '//trim(name)
        errflg = 1
        return
      end if
      do col = 1, size(host,1)
        do lev = 1, size(host,2)
          host_value = real(host(col,lev,slot),c_double)
          if (btest(d%flags,0)) then
            if (amount_type == CATCHEM_AMOUNT_MASS_RATIO) then
              call catchem_gas_mass_ratio_to_dry_ppmv(host_value,amount_type,air_basis, &
                d%molecular_weight_kg_mol,real(host_density(col,lev),c_double), &
                real(dry_density(col,lev),c_double),converted,cvt_status)
            else
              call catchem_host_ppmv_to_dry(host_value,amount_type,air_basis, &
                real(host_number_density(col,lev),c_double),real(dry_number_density(col,lev),c_double), &
                converted,cvt_status)
            end if
          else
            call catchem_aerosol_mass_ratio_to_dry_ug_kg(host_value,amount_type,air_basis, &
              real(host_density(col,lev),c_double),real(dry_density(col,lev),c_double),converted,cvt_status)
          end if
          if (cvt_status /= CATCHEM_CONVERSION_OK .or. .not. ieee_is_finite(converted)) then
            errmsg = 'CATChem physical conversion failed for constituent '//trim(name)
            errflg = 1
            return
          end if
          staging(col,lev,i) = converted
        end do
      end do
    end do
  end subroutine

  !> \brief Implements catchem ccpp convert to host for the CATChem CCPP boundary.
  !> \param handle Argument used by catchem_ccpp_convert_to_host.
  !> \param host_to_catchem Argument used by catchem_ccpp_convert_to_host.
  !> \param canonical Argument used by catchem_ccpp_convert_to_host.
  !> \param host_density Argument used by catchem_ccpp_convert_to_host.
  !> \param dry_density Argument used by catchem_ccpp_convert_to_host.
  subroutine catchem_ccpp_convert_to_host(handle, host_to_catchem, canonical, host_density, dry_density, &
                                          host_number_density, dry_number_density, host_candidate, errmsg, errflg)
    type(ccpp_context_handle), intent(in) :: handle
    integer, intent(in) :: host_to_catchem(:)
    real(c_double), intent(in) :: canonical(:,:,:)
    real(kind_phys), intent(in) :: host_density(:,:), dry_density(:,:)
    real(kind_phys), intent(in) :: host_number_density(:,:), dry_number_density(:,:)
    real(kind_phys), intent(inout) :: host_candidate(:,:,:)
    character(len=*), intent(out) :: errmsg
    integer, intent(out) :: errflg
    type(CATChem_Manifest_Species_v1) :: d
    character(len=512) :: name, standard_name, units
    integer(c_int) :: cstatus, cvt_status, amount_type, air_basis
    integer :: i, slot, col, lev
    real(c_double) :: value, converted, dry_ratio

    errmsg = ''
    errflg = 0
    if (size(host_to_catchem) <= 0 .or. size(canonical,3) <= 0) then
      errmsg = 'CATChem output conversion requires at least one mapped species.'
      errflg = 1
      return
    end if
    if (size(host_candidate,1) /= size(canonical,1) .or. size(host_candidate,2) /= size(canonical,2) .or. &
        size(host_to_catchem) /= size(canonical,3) .or. &
        size(host_density,1) /= size(canonical,1) .or. size(host_density,2) /= size(canonical,2) .or. &
        size(dry_density,1) /= size(canonical,1) .or. size(dry_density,2) /= size(canonical,2) .or. &
        size(host_number_density,1) /= size(canonical,1) .or. size(host_number_density,2) /= size(canonical,2) .or. &
        size(dry_number_density,1) /= size(canonical,1) .or. size(dry_number_density,2) /= size(canonical,2)) then
      errmsg = 'CATChem output conversion extents do not match the mapped host and canonical arrays.'
      errflg = 1
      return
    end if
    if (any(host_to_catchem <= 0) .or. maxval(host_to_catchem) > size(host_candidate,3)) then
      errmsg = 'CATChem output map contains an invalid CCPP 1-based slot.'
      errflg = 1
      return
    end if
    do i = 1, size(host_to_catchem)
      slot = host_to_catchem(i)
      if (slot < 1 .or. slot > size(host_candidate,3)) then
        errmsg = 'CATChem output map contains an invalid CCPP 1-based slot.'
        errflg = 1
        return
      end if
      call catchem_ccpp_context_species(handle,i,d,name,standard_name,units,cstatus)
      if (cstatus /= CATCHEM_CONTEXT_SUCCESS) then
        errmsg = 'CATChem output conversion could not read species metadata.'
        errflg = int(cstatus)
        return
      end if
      amount_type = int(d%amount_type,c_int)
      air_basis = int(d%air_basis,c_int)
      do col = 1, size(canonical,1)
        do lev = 1, size(canonical,2)
          value = canonical(col,lev,i)
          if (.not. ieee_is_finite(value)) then
            errmsg = 'CATChem output contains a nonfinite value: '//trim(name)
            errflg = 1
            return
          end if
          if (value < 0.0_c_double) then
            errmsg = 'CATChem output contains a negative or nonfinite value: '//trim(name)
            errflg = 1
            return
          end if
          if (btest(d%flags,0)) then
            if (amount_type == CATCHEM_AMOUNT_MOLE_FRACTION) then
              select case (air_basis)
              case (CATCHEM_AIR_DRY)
                converted = value
                cvt_status = CATCHEM_CONVERSION_OK
              case (CATCHEM_AIR_MOIST,CATCHEM_AIR_TOTAL_CONDENSATE)
                if (.not. ieee_is_finite(real(host_number_density(col,lev),c_double)) .or. &
                    .not. ieee_is_finite(real(dry_number_density(col,lev),c_double))) then
                  cvt_status = 1_c_int
                  converted = 0.0_c_double
                else
                  if (host_number_density(col,lev) <= 0.0_kind_phys .or. dry_number_density(col,lev) <= 0.0_kind_phys) then
                    cvt_status = 1_c_int
                    converted = 0.0_c_double
                  else
                    converted = value * real(dry_number_density(col,lev),c_double) / &
                      real(host_number_density(col,lev),c_double)
                    cvt_status = merge(CATCHEM_CONVERSION_OK,1_c_int,ieee_is_finite(converted))
                  end if
                end if
              case default
                cvt_status = 1_c_int
                converted = 0.0_c_double
              end select
            else if (amount_type == CATCHEM_AMOUNT_MASS_RATIO) then
              call catchem_dry_ppmv_to_gas_mass_ratio(value,amount_type,d%molecular_weight_kg_mol,dry_ratio,cvt_status)
              if (cvt_status == CATCHEM_CONVERSION_OK) then
                call convert_dry_mass_ratio_to_host(dry_ratio,air_basis,host_density(col,lev), &
                  dry_density(col,lev),converted,cvt_status)
              end if
            else
              cvt_status = 1_c_int
              converted = 0.0_c_double
            end if
          else
            if (amount_type /= CATCHEM_AMOUNT_MASS_RATIO) then
              cvt_status = 1_c_int
              converted = 0.0_c_double
            else
              dry_ratio = value * 1.0e-9_c_double
              call convert_dry_mass_ratio_to_host(dry_ratio,air_basis,host_density(col,lev), &
                dry_density(col,lev),converted,cvt_status)
            end if
          end if
          if (cvt_status /= CATCHEM_CONVERSION_OK .or. .not. ieee_is_finite(converted)) then
            errmsg = 'CATChem output conversion failed for constituent '//trim(name)
            errflg = 1
            return
          end if
          if (converted > real(huge(0.0_kind_phys),c_double)) then
            errmsg = 'Converted host output exceeds the representable host kind: '//trim(name)
            errflg = 1
            return
          end if
          host_candidate(col,lev,slot) = real(converted,kind_phys)
          if (.not. ieee_is_finite(host_candidate(col,lev,slot))) then
            errmsg = 'Converted host output is nonfinite for constituent '//trim(name)
            errflg = 1
            return
          end if
        end do
      end do
    end do
  end subroutine

  !> \brief Implements catchem ccpp stage tendency for the CATChem CCPP boundary.
  !> \param handle Argument used by catchem_ccpp_stage_tendency.
  !> \param token Argument used by catchem_ccpp_stage_tendency.
  !> \param process Argument used by catchem_ccpp_stage_tendency.
  !> \param duration_seconds Argument used by catchem_ccpp_stage_tendency.
  !> \param host_map Argument used by catchem_ccpp_stage_tendency.
  !> \param before_host Argument used by catchem_ccpp_stage_tendency.
  subroutine catchem_ccpp_stage_tendency(handle, token, process, duration_seconds, host_map, before_host, &
                                         after_canonical, host_density, dry_density, host_number_density, &
                                         dry_number_density, errmsg, errflg)
    type(ccpp_context_handle), intent(in) :: handle
    integer(c_int64_t), intent(in) :: token
    character(len=*), intent(in) :: process
    real(c_double), intent(in) :: duration_seconds
    integer, intent(in) :: host_map(:)
    real(kind_phys), intent(in) :: before_host(:,:,:)
    real(c_double), intent(in) :: after_canonical(:,:,:)
    real(kind_phys), intent(in) :: host_density(:,:), dry_density(:,:)
    real(kind_phys), intent(in) :: host_number_density(:,:), dry_number_density(:,:)
    character(len=*), intent(out) :: errmsg
    integer, intent(out) :: errflg
    real(kind_phys), allocatable :: after_host(:,:,:)
    real(c_double), allocatable :: contribution(:,:,:)
    integer(c_int) :: status
    integer :: i, col, lev, slot, ierr
    real(c_double) :: initial_value, final_value, rate

    errmsg = ''
    errflg = 0
    ! Photolysis publishes derived rates, never a chemical concentration tendency.
    if (trim(process) == 'photolysis') return
    if (.not. ieee_is_finite(duration_seconds)) then
      errmsg = 'CATChem contribution duration is nonfinite.'
      errflg = 1
      call discard_staged_output(handle)
      return
    end if
    if (duration_seconds <= 0.0_c_double) then
      errmsg = 'CATChem contribution duration must be positive.'
      errflg = 1
      call discard_staged_output(handle)
      return
    end if
    if (size(host_map) /= size(after_canonical,3) .or. &
        size(before_host,1) /= size(after_canonical,1) .or. &
        size(before_host,2) /= size(after_canonical,2)) then
      errmsg = 'CATChem contribution input extents do not match the registered species and grid.'
      errflg = 1
      call discard_staged_output(handle)
      return
    end if
    allocate(after_host(size(before_host,1),size(before_host,2),size(before_host,3)), &
             contribution(size(after_canonical,1),size(after_canonical,2),size(after_canonical,3)),stat=ierr)
    if (ierr /= 0) then
      errmsg = 'CATChem could not allocate temporary validated contribution storage.'
      errflg = 1
      call discard_staged_output(handle)
      return
    end if
    after_host = before_host
    call catchem_ccpp_convert_to_host(handle,host_map,after_canonical,host_density,dry_density, &
      host_number_density,dry_number_density,after_host,errmsg,errflg)
    if (errflg /= 0) then
      call discard_staged_output(handle)
      return
    end if
    do i = 1, size(host_map)
      slot = host_map(i)
      if (slot < 1 .or. slot > size(before_host,3)) then
        errmsg = 'CATChem contribution map contains an invalid CCPP slot.'
        errflg = 1
        call discard_staged_output(handle)
        return
      end if
      do col = 1, size(after_canonical,1)
        do lev = 1, size(after_canonical,2)
          initial_value = real(before_host(col,lev,slot),c_double)
          final_value = real(after_host(col,lev,slot),c_double)
          if (.not. ieee_is_finite(initial_value)) then
            errmsg = 'CATChem before-state contains a nonfinite mapped host value.'
            errflg = 1
            call discard_staged_output(handle)
            return
          end if
          if (initial_value < 0.0_c_double) then
            errmsg = 'CATChem before-state contains a negative mapped concentration.'
            errflg = 1
            call discard_staged_output(handle)
            return
          end if
          if (duration_seconds < 1.0_c_double) then
            if (abs(final_value-initial_value) > huge(0.0_c_double)*duration_seconds) then
              errmsg = 'CATChem host-unit contribution would overflow.'
              errflg = 1
              call discard_staged_output(handle)
              return
            end if
          end if
          rate = (final_value-initial_value)/duration_seconds
          if (.not. ieee_is_finite(rate)) then
            errmsg = 'CATChem host-unit contribution is nonfinite.'
            errflg = 1
            call discard_staged_output(handle)
            return
          end if
          contribution(col,lev,i) = rate
        end do
      end do
    end do
    call catchem_ccpp_context_stage_tendency(handle,token,contribution,status)
    if (status /= CATCHEM_CONTEXT_SUCCESS) then
      call discard_staged_output(handle)
      errmsg = 'CATChem context rejected the token-bound host tendency contribution.'
      errflg = int(status)
    end if
  end subroutine

  !> \brief Implements discard staged output for the CATChem CCPP boundary.
  !> \param handle Argument used by discard_staged_output.
  subroutine discard_staged_output(handle)
    type(ccpp_context_handle), intent(in) :: handle
    integer(c_int) :: ignored_status
    call catchem_ccpp_context_cancel(handle,ignored_status)
  end subroutine

  !> \brief Implements catchem ccpp apply tendency for the CATChem CCPP boundary.
  !> \param handle Argument used by catchem_ccpp_apply_tendency.
  !> \param token Argument used by catchem_ccpp_apply_tendency.
  !> \param duration_seconds Argument used by catchem_ccpp_apply_tendency.
  !> \param host_map Argument used by catchem_ccpp_apply_tendency.
  !> \param host_tendency Argument used by catchem_ccpp_apply_tendency.
  !> \param errmsg Argument used by catchem_ccpp_apply_tendency.
  !> \param errflg Argument used by catchem_ccpp_apply_tendency.
  subroutine catchem_ccpp_apply_tendency(handle, token, duration_seconds, host_map, host_tendency, errmsg, errflg)
    type(ccpp_context_handle), intent(in) :: handle
    integer(c_int64_t), intent(in) :: token
    real(c_double), intent(in) :: duration_seconds
    integer, intent(in) :: host_map(:)
    real(kind_phys), intent(inout) :: host_tendency(:,:,:)
    character(len=*), intent(out) :: errmsg
    integer, intent(out) :: errflg
    real(c_double), pointer, contiguous :: baseline(:,:,:), working(:,:,:), contribution(:,:,:)
    real(c_double), allocatable :: mapped_tendency(:,:,:)
    integer(c_int) :: status
    integer :: i, col, lev, slot, ierr
    real(c_double) :: current_value, added_value, increment
    integer, allocatable :: mapped_indices(:)

    errmsg = ''
    errflg = 0
    if (.not. ieee_is_finite(duration_seconds) .or. duration_seconds <= 0.0_c_double) then
      errmsg = 'CATChem update interval duration must be finite and positive.'
      errflg = 1
      return
    end if
    call catchem_ccpp_context_get_mapping(handle,mapped_indices,status)
    if (status /= CATCHEM_CONTEXT_SUCCESS) then
      errmsg = 'CATChem update interstitial has no registered constituent map.'
      errflg = int(status)
      return
    end if
    if (size(mapped_indices) /= size(host_map)) then
      errmsg = 'CATChem update interstitial map extent differs from its context binding.'
      errflg = 1
      return
    end if
    if (any(mapped_indices /= host_map)) then
      errmsg = 'CATChem update interstitial received a stale or mismatched map.'
      errflg = 1
      return
    end if
    call catchem_ccpp_context_buffers(handle,baseline,working,contribution,status)
    if (status /= CATCHEM_CONTEXT_SUCCESS) then
      errmsg = 'CATChem update interstitial could not access its owned contribution.'
      errflg = int(status)
      return
    end if
    if (size(host_map) <= 0) then
      errmsg = 'CATChem update interstitial requires at least one mapped species.'
      errflg = 1
      return
    end if
    if (size(host_tendency,1) /= size(contribution,1) .or. size(host_tendency,2) /= size(contribution,2) .or. &
        size(host_tendency,3) < maxval(host_map)) then
      errmsg = 'CATChem host tendency extents do not match the mapped contribution.'
      errflg = 1
      return
    end if
    allocate(mapped_tendency(size(contribution,1),size(contribution,2),size(contribution,3)),stat=ierr)
    if (ierr /= 0) then
      errmsg = 'CATChem could not allocate atomic tendency validation storage.'
      errflg = 1
      return
    end if
    do i = 1, size(host_map)
      slot = host_map(i)
      if (slot <= 0 .or. slot > size(host_tendency,3)) then
        errmsg = 'CATChem update interstitial map has an invalid CCPP index.'
        errflg = 1
        return
      end if
      mapped_tendency(:,:,i) = real(host_tendency(:,:,slot),c_double)
      if (any(.not. ieee_is_finite(mapped_tendency(:,:,i)))) then
        errmsg = 'Mapped host tendency contains a nonfinite value; update was not applied.'
        errflg = 1
        return
      end if
      do lev = 1, size(contribution,2)
        do col = 1, size(contribution,1)
          current_value = mapped_tendency(col,lev,i)
          if (abs(contribution(col,lev,i)) > huge(0.0_c_double)/duration_seconds) then
            errmsg = 'Tendency increment would overflow when scaled by the host interval.'
            errflg = 1
            return
          end if
          increment = duration_seconds*contribution(col,lev,i)
          if (abs(current_value) > huge(0.0_c_double)-abs(increment)) then
            errmsg = 'Mapped tendency addition would overflow; update was not applied.'
            errflg = 1
            return
          end if
          added_value = current_value + increment
          if (.not. ieee_is_finite(added_value) .or. &
              abs(added_value) > real(huge(0.0_kind_phys),c_double)) then
            errmsg = 'Tendency addition is not representable in host precision; update was not applied.'
            errflg = 1
            return
          end if
        end do
      end do
    end do
    call catchem_ccpp_context_apply_tendency(handle,token,duration_seconds,mapped_tendency,status)
    if (status /= CATCHEM_CONTEXT_SUCCESS) then
      errmsg = 'CATChem tendency token is absent, stale, or already consumed.'
      errflg = int(status)
      return
    end if
    do i = 1, size(host_map)
      host_tendency(:,:,host_map(i)) = real(mapped_tendency(:,:,i),kind_phys)
    end do
  end subroutine

  !> \brief Implements catchem ccpp cancel tendency for the CATChem CCPP boundary.
  !> \param handle Argument used by catchem_ccpp_cancel_tendency.
  !> \param token Argument used by catchem_ccpp_cancel_tendency.
  !> \param errmsg Argument used by catchem_ccpp_cancel_tendency.
  !> \param errflg Argument used by catchem_ccpp_cancel_tendency.
  subroutine catchem_ccpp_cancel_tendency(handle, token, errmsg, errflg)
    type(ccpp_context_handle), intent(in) :: handle
    integer(c_int64_t), intent(in) :: token
    character(len=*), intent(out) :: errmsg
    integer, intent(out) :: errflg
    integer(c_int) :: status
    errmsg = ''
    errflg = 0
    call catchem_ccpp_context_cancel_tendency(handle,token,status)
    if (status /= CATCHEM_CONTEXT_SUCCESS) then
      errmsg = 'CATChem pending contribution could not be canceled for this token.'
      errflg = int(status)
    end if
  end subroutine

  !> \brief Implements convert dry mass ratio to host for the CATChem CCPP boundary.
  !> \param dry_ratio Argument used by convert_dry_mass_ratio_to_host.
  !> \param air_basis Argument used by convert_dry_mass_ratio_to_host.
  !> \param host_density Argument used by convert_dry_mass_ratio_to_host.
  !> \param dry_density Argument used by convert_dry_mass_ratio_to_host.
  !> \param host_ratio Argument used by convert_dry_mass_ratio_to_host.
  !> \param status Argument used by convert_dry_mass_ratio_to_host.
  subroutine convert_dry_mass_ratio_to_host(dry_ratio,air_basis,host_density,dry_density,host_ratio,status)
    real(c_double), intent(in) :: dry_ratio
    integer(c_int), intent(in) :: air_basis
    real(kind_phys), intent(in) :: host_density,dry_density
    real(c_double), intent(out) :: host_ratio
    integer(c_int), intent(out) :: status
    host_ratio = 0.0_c_double
    status = 1_c_int
    select case (air_basis)
    case (CATCHEM_AIR_DRY)
      host_ratio = dry_ratio
    case (CATCHEM_AIR_MOIST,CATCHEM_AIR_TOTAL_CONDENSATE)
      if (.not. ieee_is_finite(real(host_density,c_double))) return
      if (.not. ieee_is_finite(real(dry_density,c_double))) return
      if (host_density <= 0.0_kind_phys .or. dry_density <= 0.0_kind_phys) return
      host_ratio = dry_ratio * real(dry_density,c_double) / real(host_density,c_double)
    case default
      return
    end select
    if (.not. ieee_is_finite(host_ratio)) then
      host_ratio = 0.0_c_double
      return
    end if
    status = CATCHEM_CONVERSION_OK
  end subroutine

  !> \brief Implements catchem ccpp run selected for the CATChem CCPP boundary.
  !> \param handle Argument used by catchem_ccpp_run_selected.
  !> \param host_map Argument used by catchem_ccpp_run_selected.
  !> \param staging Argument used by catchem_ccpp_run_selected.
  !> \param interval_id Argument used by catchem_ccpp_run_selected.
  !> \param site Argument used by catchem_ccpp_run_selected.
  !> \param subcycle Argument used by catchem_ccpp_run_selected.
  !> \param process Argument used by catchem_ccpp_run_selected.
  subroutine catchem_ccpp_run_selected(handle, host_map, staging, interval_id, site, subcycle, process, &
                                       duration_seconds, operation_status, publication_eligible, import_generation, &
                                       errmsg, errflg)
    type(ccpp_context_handle), intent(in) :: handle
    integer, intent(in) :: host_map(:), subcycle
    real(c_double), intent(inout), target, contiguous :: staging(:,:,:)
    integer(c_int64_t), intent(in) :: interval_id
    character(len=*), intent(in) :: site, process
    real(c_double), intent(in) :: duration_seconds
    integer(c_int), intent(out) :: operation_status
    logical, intent(out) :: publication_eligible
    integer(c_int64_t), intent(out) :: import_generation
    character(len=*), intent(out) :: errmsg
    integer, intent(out) :: errflg
    type(CATChem_Model), pointer :: model
    integer(c_int) :: status
    integer, allocatable :: registered_map(:)
    integer :: i, j

    errmsg = ''
    errflg = 0
    operation_status = 0_c_int
    publication_eligible = .false.
    import_generation = 0_c_int64_t
    call catchem_ccpp_context_get_mapping(handle,registered_map,status)
    if (status /= CATCHEM_CONTEXT_SUCCESS) then
      errmsg = 'CATChem dispatch requires a valid context-owned constituent map.'
      errflg = int(status)
      return
    end if
    if (size(registered_map) /= size(host_map)) then
      errmsg = 'CATChem dispatch map differs from the context-owned registration map.'
      errflg = 1
      return
    end if
    if (any(registered_map /= host_map)) then
      errmsg = 'CATChem dispatch map differs from the context-owned registration map.'
      errflg = 1
      return
    end if
    if (size(host_map) /= size(staging,3)) then
      errmsg = 'CATChem dispatch map extent does not match the staged species extent.'
      errflg = 1
      return
    end if
    do i = 1, size(host_map)
      if (host_map(i) <= 0) then
        errmsg = 'CATChem dispatch map contains a nonpositive CCPP index.'
        errflg = 1
        return
      end if
      do j = 1, i-1
        if (host_map(i) == host_map(j)) then
          errmsg = 'Two CATChem species map to one physical CCPP constituent slot.'
          errflg = 1
          return
        end if
      end do
    end do
    call catchem_ccpp_context_model(handle,model,status)
    if (status /= CATCHEM_CONTEXT_SUCCESS) then
      errmsg = 'CATChem dispatch received an absent or stale context.'
      errflg = int(status)
      return
    end if
    status = catchem_state_bind_unified_chemistry_checked(model%state_mgr_ptr, &
      c_loc(staging(1,1,1)),int(size(staging,1),c_int),int(size(staging,2),c_int),int(size(staging,3),c_int))
    if (status /= 0_c_int) then
      errmsg = 'CATChem failed to bind the checked unified constituent input.'
      errflg = int(status)
      return
    end if
    call model%run_selected_process(interval_id,site,int(subcycle,c_int32_t),process,duration_seconds, &
      operation_status,publication_eligible,import_generation,errflg)
    if (errflg /= 0) then
      errmsg = model%last_error
      return
    end if
    if (.not. publication_eligible) then
      errmsg = 'CATChem selected dispatch did not produce a publishable result.'
      errflg = 1
      return
    end if
    if (any(.not. ieee_is_finite(staging))) then
      errmsg = 'CATChem output staging contains a nonfinite value; no output was published.'
      errflg = 1
      return
    end if
    call catchem_ccpp_context_stage(handle,staging,status)
    if (status /= CATCHEM_CONTEXT_SUCCESS) then
      errmsg = 'CATChem context rejected the validated output staging.'
      errflg = int(status)
    end if
  end subroutine
end module catchem_ccpp_adapter

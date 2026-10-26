module machine
   use iso_fortran_env, only: real64
   implicit none
   integer, parameter :: kind_phys = real64
end module machine

module ccpp_constituent_prop_mod
   use machine, only: kind_phys
   implicit none
   !> \\brief Minimal locked-framework property contract used by CCPP stub builds.
   type :: ccpp_constituent_properties_t
      character(len=1024) :: std_name = '', long_name_val = '', diag_name = '', units_val = '', vertical_dim = ''
      logical :: advected = .false., is_mass = .true., is_volume = .false., is_number = .false.
      logical :: dry_flag = .true., moist_flag = .false., wet_flag = .false., thermo_active = .false., water_species = .false.
      real(kind_phys) :: min_value = 0.0_kind_phys, default_val = 0.0_kind_phys, mw = huge(1.0_kind_phys)
      integer :: const_index_value = 0
   contains
      procedure :: instantiate
      procedure :: const_index => prop_const_index
      procedure :: set_const_index => prop_set_const_index
      procedure :: standard_name => prop_standard_name
      procedure :: long_name => prop_long_name
      procedure :: diagnostic_name => prop_diagnostic_name
      procedure :: units => prop_units
      procedure :: vertical_dimension => prop_vertical_dimension
      procedure :: is_advected => prop_is_advected
      procedure :: is_mass_mixing_ratio => prop_is_mass_mixing_ratio
      procedure :: is_volume_mixing_ratio => prop_is_volume_mixing_ratio
      procedure :: is_number_concentration => prop_is_number_concentration
      procedure :: is_dry => prop_is_dry
      procedure :: is_moist => prop_is_moist
      procedure :: is_wet => prop_is_wet
      procedure :: is_thermo_active => prop_is_thermo_active
      procedure :: is_water_species => prop_is_water_species
      procedure :: minimum => prop_minimum
      procedure :: default_value => prop_default_value
      procedure :: molar_mass => prop_molar_mass
   end type
   !> \\brief Pointer view of one CCPP constituent descriptor.
   type :: ccpp_constituent_prop_ptr_t
      type(ccpp_constituent_properties_t), pointer :: p => null()
   contains
      procedure :: set => ptr_set
      procedure :: const_index => ptr_const_index
      procedure :: standard_name => ptr_standard_name
      procedure :: long_name => ptr_long_name
      procedure :: diagnostic_name => ptr_diagnostic_name
      procedure :: units => ptr_units
      procedure :: vertical_dimension => ptr_vertical_dimension
      procedure :: is_advected => ptr_is_advected
      procedure :: is_mass_mixing_ratio => ptr_is_mass_mixing_ratio
      procedure :: is_volume_mixing_ratio => ptr_is_volume_mixing_ratio
      procedure :: is_number_concentration => ptr_is_number_concentration
      procedure :: is_dry => ptr_is_dry
      procedure :: is_moist => ptr_is_moist
      procedure :: is_wet => ptr_is_wet
      procedure :: is_thermo_active => ptr_is_thermo_active
      procedure :: is_water_species => ptr_is_water_species
      procedure :: minimum => ptr_minimum
      procedure :: default_value => ptr_default_value
      procedure :: molar_mass => ptr_molar_mass
   end type
contains
   !> \brief Associates a contract-test property pointer with its descriptor.
   !> \param self Pointer view to update.
   !> \param property Target descriptor.
   !> \param errcode Zero on success when present.
   !> \param errmsg Error detail when present.
   subroutine ptr_set(self, property, errcode, errmsg)
      class(ccpp_constituent_prop_ptr_t), intent(inout) :: self
      type(ccpp_constituent_properties_t), pointer, intent(in) :: property
      integer, optional, intent(out) :: errcode
      character(len=*), optional, intent(out) :: errmsg
      self%p => property
      if (present(errcode)) errcode = 0
      if (present(errmsg)) errmsg = ''
   end subroutine

   !> \brief Sets the physical registry slot in contract-test metadata.
   !> \param self Descriptor to update.
   !> \param index One-based registry slot.
   !> \param errcode Zero on success when present.
   !> \param errmsg Error detail when present.
   subroutine prop_set_const_index(self, index, errcode, errmsg)
      class(ccpp_constituent_properties_t), intent(inout) :: self
      integer, intent(in) :: index
      integer, optional, intent(out) :: errcode
      character(len=*), optional, intent(out) :: errmsg
      self%const_index_value = index
      if (present(errcode)) errcode = 0
      if (present(errmsg)) errmsg = ''
   end subroutine

   !> \brief Instantiates a contract-test constituent descriptor.
   subroutine instantiate(self, std_name, long_name, diag_name, units, vertical_dim, advected, default_value, min_value, &
                          molar_mass, water_species, mixing_ratio_type, errcode, errmsg)
      class(ccpp_constituent_properties_t), intent(inout) :: self
      character(len=*), intent(in) :: std_name, long_name, diag_name, units, vertical_dim
      logical, optional, intent(in) :: advected, water_species
      real(kind_phys), optional, intent(in) :: default_value, min_value, molar_mass
      character(len=*), optional, intent(in) :: mixing_ratio_type
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      self%std_name = std_name; self%long_name_val = long_name; self%diag_name = diag_name
      self%units_val = units; self%vertical_dim = vertical_dim
      if (present(advected)) self%advected = advected
      if (present(default_value)) self%default_val = default_value
      if (present(min_value)) self%min_value = min_value
      if (present(molar_mass)) self%mw = molar_mass
      if (present(water_species)) self%water_species = water_species
      if (present(mixing_ratio_type)) then
         self%dry_flag = trim(mixing_ratio_type) == 'dry'
         self%moist_flag = trim(mixing_ratio_type) == 'moist'
         self%wet_flag = trim(mixing_ratio_type) == 'wet'
      end if
      self%is_volume = index(std_name, 'volume_mixing_ratio') > 0
      self%is_number = index(std_name, 'number_concentration') > 0
      self%is_mass = .not. (self%is_volume .or. self%is_number)
      errcode = 0; errmsg = ''
   end subroutine
   !> \brief Reads standard name from a contract-test property pointer.
   subroutine ptr_standard_name(self, value, errcode, errmsg)
      class(ccpp_constituent_prop_ptr_t), intent(in) :: self
      character(len=*), intent(out) :: value
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      errcode = 0; errmsg = ''
      if (.not. associated(self%p)) then
         errcode = 1; errmsg = 'unassociated constituent property'; value = ''; return
      end if
      value = self%p%std_name
   end subroutine
   !> \brief Reads long name from a contract-test property pointer.
   subroutine ptr_long_name(self, value, errcode, errmsg)
      class(ccpp_constituent_prop_ptr_t), intent(in) :: self
      character(len=*), intent(out) :: value
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      errcode = 0; errmsg = ''
      if (.not. associated(self%p)) then
         errcode = 1; errmsg = 'unassociated constituent property'; value = ''; return
      end if
      value = self%p%long_name_val
   end subroutine
   !> \brief Reads diagnostic name from a contract-test property pointer.
   subroutine ptr_diagnostic_name(self, value, errcode, errmsg)
      class(ccpp_constituent_prop_ptr_t), intent(in) :: self
      character(len=*), intent(out) :: value
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      errcode = 0; errmsg = ''
      if (.not. associated(self%p)) then
         errcode = 1; errmsg = 'unassociated constituent property'; value = ''; return
      end if
      value = self%p%diag_name
   end subroutine
   !> \brief Reads units from a contract-test property pointer.
   subroutine ptr_units(self, value, errcode, errmsg)
      class(ccpp_constituent_prop_ptr_t), intent(in) :: self
      character(len=*), intent(out) :: value
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      errcode = 0; errmsg = ''
      if (.not. associated(self%p)) then
         errcode = 1; errmsg = 'unassociated constituent property'; value = ''; return
      end if
      value = self%p%units_val
   end subroutine
   !> \brief Reads vertical dimension from a contract-test property pointer.
   subroutine ptr_vertical_dimension(self, value, errcode, errmsg)
      class(ccpp_constituent_prop_ptr_t), intent(in) :: self
      character(len=*), intent(out) :: value
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      errcode = 0; errmsg = ''
      if (.not. associated(self%p)) then
         errcode = 1; errmsg = 'unassociated constituent property'; value = ''; return
      end if
      value = self%p%vertical_dim
   end subroutine
   !> \brief Reads is advected from a contract-test property pointer.
   subroutine ptr_is_advected(self, value, errcode, errmsg)
      class(ccpp_constituent_prop_ptr_t), intent(in) :: self
      logical, intent(out) :: value
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      errcode = 0; errmsg = ''
      if (.not. associated(self%p)) then
         errcode = 1; errmsg = 'unassociated constituent property'; value = .false.; return
      end if
      value = self%p%advected
   end subroutine
   !> \brief Reads is mass mixing ratio from a contract-test property pointer.
   subroutine ptr_is_mass_mixing_ratio(self, value, errcode, errmsg)
      class(ccpp_constituent_prop_ptr_t), intent(in) :: self
      logical, intent(out) :: value
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      errcode = 0; errmsg = ''
      if (.not. associated(self%p)) then
         errcode = 1; errmsg = 'unassociated constituent property'; value = .false.; return
      end if
      value = self%p%is_mass
   end subroutine
   !> \brief Reads is volume mixing ratio from a contract-test property pointer.
   subroutine ptr_is_volume_mixing_ratio(self, value, errcode, errmsg)
      class(ccpp_constituent_prop_ptr_t), intent(in) :: self
      logical, intent(out) :: value
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      errcode = 0; errmsg = ''
      if (.not. associated(self%p)) then
         errcode = 1; errmsg = 'unassociated constituent property'; value = .false.; return
      end if
      value = self%p%is_volume
   end subroutine
   !> \brief Reads is number concentration from a contract-test property pointer.
   subroutine ptr_is_number_concentration(self, value, errcode, errmsg)
      class(ccpp_constituent_prop_ptr_t), intent(in) :: self
      logical, intent(out) :: value
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      errcode = 0; errmsg = ''
      if (.not. associated(self%p)) then
         errcode = 1; errmsg = 'unassociated constituent property'; value = .false.; return
      end if
      value = self%p%is_number
   end subroutine
   !> \brief Reads is dry from a contract-test property pointer.
   subroutine ptr_is_dry(self, value, errcode, errmsg)
      class(ccpp_constituent_prop_ptr_t), intent(in) :: self
      logical, intent(out) :: value
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      errcode = 0; errmsg = ''
      if (.not. associated(self%p)) then
         errcode = 1; errmsg = 'unassociated constituent property'; value = .false.; return
      end if
      value = self%p%dry_flag
   end subroutine
   !> \brief Reads is moist from a contract-test property pointer.
   subroutine ptr_is_moist(self, value, errcode, errmsg)
      class(ccpp_constituent_prop_ptr_t), intent(in) :: self
      logical, intent(out) :: value
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      errcode = 0; errmsg = ''
      if (.not. associated(self%p)) then
         errcode = 1; errmsg = 'unassociated constituent property'; value = .false.; return
      end if
      value = self%p%moist_flag
   end subroutine
   !> \brief Reads is wet from a contract-test property pointer.
   subroutine ptr_is_wet(self, value, errcode, errmsg)
      class(ccpp_constituent_prop_ptr_t), intent(in) :: self
      logical, intent(out) :: value
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      errcode = 0; errmsg = ''
      if (.not. associated(self%p)) then
         errcode = 1; errmsg = 'unassociated constituent property'; value = .false.; return
      end if
      value = self%p%wet_flag
   end subroutine
   !> \brief Reads is thermo active from a contract-test property pointer.
   subroutine ptr_is_thermo_active(self, value, errcode, errmsg)
      class(ccpp_constituent_prop_ptr_t), intent(in) :: self
      logical, intent(out) :: value
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      errcode = 0; errmsg = ''
      if (.not. associated(self%p)) then
         errcode = 1; errmsg = 'unassociated constituent property'; value = .false.; return
      end if
      value = self%p%thermo_active
   end subroutine
   !> \brief Reads is water species from a contract-test property pointer.
   subroutine ptr_is_water_species(self, value, errcode, errmsg)
      class(ccpp_constituent_prop_ptr_t), intent(in) :: self
      logical, intent(out) :: value
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      errcode = 0; errmsg = ''
      if (.not. associated(self%p)) then
         errcode = 1; errmsg = 'unassociated constituent property'; value = .false.; return
      end if
      value = self%p%water_species
   end subroutine
   !> \brief Reads minimum from a contract-test property pointer.
   subroutine ptr_minimum(self, value, errcode, errmsg)
      class(ccpp_constituent_prop_ptr_t), intent(in) :: self
      real(kind_phys), intent(out) :: value
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      errcode = 0; errmsg = ''
      if (.not. associated(self%p)) then
         errcode = 1; errmsg = 'unassociated constituent property'; value = 0.0_kind_phys; return
      end if
      value = self%p%min_value
   end subroutine
   !> \brief Reads default value from a contract-test property pointer.
   subroutine ptr_default_value(self, value, errcode, errmsg)
      class(ccpp_constituent_prop_ptr_t), intent(in) :: self
      real(kind_phys), intent(out) :: value
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      errcode = 0; errmsg = ''
      if (.not. associated(self%p)) then
         errcode = 1; errmsg = 'unassociated constituent property'; value = 0.0_kind_phys; return
      end if
      value = self%p%default_val
   end subroutine
   !> \brief Reads molar mass from a contract-test property pointer.
   subroutine ptr_molar_mass(self, value, errcode, errmsg)
      class(ccpp_constituent_prop_ptr_t), intent(in) :: self
      real(kind_phys), intent(out) :: value
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      errcode = 0; errmsg = ''
      if (.not. associated(self%p)) then
         errcode = 1; errmsg = 'unassociated constituent property'; value = 0.0_kind_phys; return
      end if
      value = self%p%mw
   end subroutine
   !> \brief Returns the one-based index stored on a constituent property pointer.
   subroutine ptr_const_index(self, index, errcode, errmsg)
      class(ccpp_constituent_prop_ptr_t), intent(in) :: self
      integer, intent(out) :: index
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      index = 0; errcode = 0; errmsg = ''
      if (.not. associated(self%p)) then
         errcode = 1; errmsg = 'unassociated constituent property'; return
      end if
      index = self%p%const_index_value
   end subroutine
   !> \brief Reads standard name from a contract-test descriptor.
   subroutine prop_standard_name(self, value, errcode, errmsg)
      class(ccpp_constituent_properties_t), intent(in) :: self
      character(len=*), intent(out) :: value
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      value = self%std_name; errcode = 0; errmsg = ''
   end subroutine
   !> \brief Reads long name from a contract-test descriptor.
   subroutine prop_long_name(self, value, errcode, errmsg)
      class(ccpp_constituent_properties_t), intent(in) :: self
      character(len=*), intent(out) :: value
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      value = self%long_name_val; errcode = 0; errmsg = ''
   end subroutine
   !> \brief Reads diagnostic name from a contract-test descriptor.
   subroutine prop_diagnostic_name(self, value, errcode, errmsg)
      class(ccpp_constituent_properties_t), intent(in) :: self
      character(len=*), intent(out) :: value
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      value = self%diag_name; errcode = 0; errmsg = ''
   end subroutine
   !> \brief Reads units from a contract-test descriptor.
   subroutine prop_units(self, value, errcode, errmsg)
      class(ccpp_constituent_properties_t), intent(in) :: self
      character(len=*), intent(out) :: value
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      value = self%units_val; errcode = 0; errmsg = ''
   end subroutine
   !> \brief Reads vertical dimension from a contract-test descriptor.
   subroutine prop_vertical_dimension(self, value, errcode, errmsg)
      class(ccpp_constituent_properties_t), intent(in) :: self
      character(len=*), intent(out) :: value
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      value = self%vertical_dim; errcode = 0; errmsg = ''
   end subroutine
   !> \brief Reads is advected from a contract-test descriptor.
   subroutine prop_is_advected(self, value, errcode, errmsg)
      class(ccpp_constituent_properties_t), intent(in) :: self
      logical, intent(out) :: value
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      value = self%advected; errcode = 0; errmsg = ''
   end subroutine
   !> \brief Reads is mass mixing ratio from a contract-test descriptor.
   subroutine prop_is_mass_mixing_ratio(self, value, errcode, errmsg)
      class(ccpp_constituent_properties_t), intent(in) :: self
      logical, intent(out) :: value
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      value = self%is_mass; errcode = 0; errmsg = ''
   end subroutine
   !> \brief Reads is volume mixing ratio from a contract-test descriptor.
   subroutine prop_is_volume_mixing_ratio(self, value, errcode, errmsg)
      class(ccpp_constituent_properties_t), intent(in) :: self
      logical, intent(out) :: value
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      value = self%is_volume; errcode = 0; errmsg = ''
   end subroutine
   !> \brief Reads is number concentration from a contract-test descriptor.
   subroutine prop_is_number_concentration(self, value, errcode, errmsg)
      class(ccpp_constituent_properties_t), intent(in) :: self
      logical, intent(out) :: value
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      value = self%is_number; errcode = 0; errmsg = ''
   end subroutine
   !> \brief Reads is dry from a contract-test descriptor.
   subroutine prop_is_dry(self, value, errcode, errmsg)
      class(ccpp_constituent_properties_t), intent(in) :: self
      logical, intent(out) :: value
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      value = self%dry_flag; errcode = 0; errmsg = ''
   end subroutine
   !> \brief Reads is moist from a contract-test descriptor.
   subroutine prop_is_moist(self, value, errcode, errmsg)
      class(ccpp_constituent_properties_t), intent(in) :: self
      logical, intent(out) :: value
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      value = self%moist_flag; errcode = 0; errmsg = ''
   end subroutine
   !> \brief Reads is wet from a contract-test descriptor.
   subroutine prop_is_wet(self, value, errcode, errmsg)
      class(ccpp_constituent_properties_t), intent(in) :: self
      logical, intent(out) :: value
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      value = self%wet_flag; errcode = 0; errmsg = ''
   end subroutine
   !> \brief Reads is thermo active from a contract-test descriptor.
   subroutine prop_is_thermo_active(self, value, errcode, errmsg)
      class(ccpp_constituent_properties_t), intent(in) :: self
      logical, intent(out) :: value
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      value = self%thermo_active; errcode = 0; errmsg = ''
   end subroutine
   !> \brief Reads is water species from a contract-test descriptor.
   subroutine prop_is_water_species(self, value, errcode, errmsg)
      class(ccpp_constituent_properties_t), intent(in) :: self
      logical, intent(out) :: value
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      value = self%water_species; errcode = 0; errmsg = ''
   end subroutine
   !> \brief Reads minimum from a contract-test descriptor.
   subroutine prop_minimum(self, value, errcode, errmsg)
      class(ccpp_constituent_properties_t), intent(in) :: self
      real(kind_phys), intent(out) :: value
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      value = self%min_value; errcode = 0; errmsg = ''
   end subroutine
   !> \brief Reads default value from a contract-test descriptor.
   subroutine prop_default_value(self, value, errcode, errmsg)
      class(ccpp_constituent_properties_t), intent(in) :: self
      real(kind_phys), intent(out) :: value
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      value = self%default_val; errcode = 0; errmsg = ''
   end subroutine
   !> \brief Reads molar mass from a contract-test descriptor.
   subroutine prop_molar_mass(self, value, errcode, errmsg)
      class(ccpp_constituent_properties_t), intent(in) :: self
      real(kind_phys), intent(out) :: value
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      value = self%mw; errcode = 0; errmsg = ''
   end subroutine
   !> \brief Returns the one-based index stored on a constituent descriptor.
   integer function prop_const_index(self, errcode, errmsg) result(value)
      class(ccpp_constituent_properties_t), intent(in) :: self
      integer, intent(out) :: errcode
      character(len=*), intent(out) :: errmsg
      value = self%const_index_value; errcode = 0; errmsg = ''
   end function
end module ccpp_constituent_prop_mod

module ccpp_const_utils
   use ccpp_constituent_prop_mod, only: ccpp_constituent_prop_ptr_t
   implicit none
contains
   !> \\brief Resolves a constituent standard name in a pointer-array view.
   !> \\param props Full ordered pointer array to search.
   !> \\param name Requested standard name.
   !> \\param idx One-based index, or a nonpositive value on failure.
   !> \\param errmsg Error detail when the name is absent.
   !> \\param errflg Zero on success and nonzero on failure.
   subroutine ccpp_const_get_idx(props, name, idx, errmsg, errflg)
      type(ccpp_constituent_prop_ptr_t), intent(in) :: props(:)
      character(len=*), intent(in) :: name
      integer, intent(out) :: idx
      character(len=*), intent(out) :: errmsg
      integer, intent(out) :: errflg
      integer :: i
      idx = -1
      do i = 1, size(props)
         if (associated(props(i)%p)) then
            if (trim(props(i)%p%std_name) == trim(name)) idx = i
         end if
      end do
      if (idx > 0) then
         errflg = 0
         errmsg = ''
      else
         errflg = 1
         errmsg = 'species not found'
      end if
   end subroutine ccpp_const_get_idx
end module ccpp_const_utils

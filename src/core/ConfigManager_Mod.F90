!> \file ConfigManager_Mod.F90
!! \brief Lightweight backward-compatible Fortran wrapper for configuration values
!!
module ConfigManager_Mod
   use Precision_Mod, only: fp
   use Error_Mod, only: CC_SUCCESS, CC_FAILURE
   use iso_c_binding, only: c_ptr, c_associated, c_int, c_char, c_double, c_null_char

   implicit none
   private

   public :: ConfigManagerType, ConfigDataType, EmissionCategoryMapping, EmisSpeciesMappingEntry, EmissionMappingConfig, &
      RuntimeConfig, FilePathConfig

   ! Interface mapping back to catchem_api.cpp for configuration and emission mapping queries
   interface
      integer(c_int) function catchem_config_get_bool_path(core_ptr, path, default_val) &
         bind(C, name="catchem_config_get_bool_path")
         import :: c_ptr, c_int, c_char
         type(c_ptr), value :: core_ptr
         character(kind=c_char), intent(in) :: path(*)
         integer(c_int), value :: default_val
      end function

      subroutine catchem_config_get_string_path(core_ptr, path, val_out, default_val) &
         bind(C, name="catchem_config_get_string_path")
         import :: c_ptr, c_char
         type(c_ptr), value :: core_ptr
         character(kind=c_char), intent(in) :: path(*), default_val(*)
         character(kind=c_char), intent(out) :: val_out(*)
      end subroutine

      real(c_double) function catchem_config_get_double_path(core_ptr, path, default_val) &
         bind(C, name="catchem_config_get_double_path")
         import :: c_ptr, c_char, c_double
         type(c_ptr), value :: core_ptr
         character(kind=c_char), intent(in) :: path(*)
         real(c_double), value :: default_val
      end function

      integer(c_int) function catchem_config_get_int_path(core_ptr, path, default_val) &
         bind(C, name="catchem_config_get_int_path")
         import :: c_ptr, c_char, c_int
         type(c_ptr), value :: core_ptr
         character(kind=c_char), intent(in) :: path(*)
         integer(c_int), value :: default_val
      end function

      integer(c_int) function catchem_config_get_array_path_count(core_ptr, path) &
         bind(C, name="catchem_config_get_array_path_count")
         import :: c_ptr, c_char, c_int
         type(c_ptr), value :: core_ptr
         character(kind=c_char), intent(in) :: path(*)
      end function

      subroutine catchem_config_get_array_path_item(core_ptr, path, idx, val_out) &
         bind(C, name="catchem_config_get_array_path_item")
         import :: c_ptr, c_char, c_int
         type(c_ptr), value :: core_ptr
         character(kind=c_char), intent(in) :: path(*)
         integer(c_int), value :: idx
         character(kind=c_char), intent(out) :: val_out(*)
      end subroutine

      integer(c_int) function catchem_config_is_emission_mapping_loaded(core_ptr) &
         bind(C, name="catchem_config_is_emission_mapping_loaded")
         import :: c_ptr, c_int
         type(c_ptr), value :: core_ptr
      end function

      integer(c_int) function catchem_config_get_emission_category_count(core_ptr) &
         bind(C, name="catchem_config_get_emission_category_count")
         import :: c_ptr, c_int
         type(c_ptr), value :: core_ptr
      end function

      subroutine catchem_config_get_emission_category_name(core_ptr, cat_idx, name_out) &
         bind(C, name="catchem_config_get_emission_category_name")
         import :: c_ptr, c_int, c_char
         type(c_ptr), value :: core_ptr
         integer(c_int), value :: cat_idx
         character(kind=c_char), intent(out) :: name_out(*)
      end subroutine

      integer(c_int) function catchem_config_get_emission_field_count(core_ptr, cat_idx) &
         bind(C, name="catchem_config_get_emission_field_count")
         import :: c_ptr, c_int
         type(c_ptr), value :: core_ptr
         integer(c_int), value :: cat_idx
      end function

      subroutine catchem_config_get_emission_field_info(core_ptr, cat_idx, field_idx, field_out, units_out, n_map_out) &
         bind(C, name="catchem_config_get_emission_field_info")
         import :: c_ptr, c_int, c_char
         type(c_ptr), value :: core_ptr
         integer(c_int), value :: cat_idx, field_idx
         character(kind=c_char), intent(out) :: field_out(*), units_out(*)
         integer(c_int), intent(out) :: n_map_out
      end subroutine

      subroutine catchem_config_get_emission_mapping_item(core_ptr, cat_idx, field_idx, map_idx, species_out, scale_out) &
         bind(C, name="catchem_config_get_emission_mapping_item")
         import :: c_ptr, c_int, c_char, c_double
         type(c_ptr), value :: core_ptr
         integer(c_int), value :: cat_idx, field_idx, map_idx
         character(kind=c_char), intent(out) :: species_out(*)
         real(c_double), intent(out) :: scale_out
      end subroutine
   end interface

   type :: EmisSpeciesMappingEntry
      character(len=128) :: emission_field = ""
      character(len=64) :: units = ""
      character(len=128) :: long_name = ""
      integer :: n_mappings = 0
      character(len=64), allocatable :: map(:)
      real(fp), allocatable :: scale(:)
      integer, allocatable :: index(:)
   end type EmisSpeciesMappingEntry

   type :: EmissionCategoryMapping
      character(len=128) :: category_name = ""
      logical :: is_active = .false.
      integer :: n_emission_species = 0
      type(EmisSpeciesMappingEntry), allocatable :: species_mappings(:)
   end type EmissionCategoryMapping

   type :: EmissionMappingConfig
      logical :: is_loaded = .false.
      integer :: n_categories = 0
      type(EmissionCategoryMapping), allocatable :: categories(:)
   end type EmissionMappingConfig

   type :: RuntimeConfig
      integer :: Output_Frequency = 3600
      integer :: CompressLev = 0
      logical :: latlon_output = .true.
      logical :: DiagEnabled = .true.
      character(len=64), allocatable :: diag_species(:)
   end type RuntimeConfig

   type :: FilePathConfig
      character(len=256) :: Output_Directory = "./output"
      character(len=256) :: Output_Prefix = "catchem_diag"
   end type FilePathConfig

   type :: ConfigDataType
      type(EmissionMappingConfig) :: emission_mapping
      type(RuntimeConfig) :: runtime
      type(FilePathConfig) :: file_paths
   end type ConfigDataType

   type :: ConfigManagerType
      type(c_ptr) :: core_ptr = c_null_ptr
      type(ConfigDataType) :: config_data
   contains
      procedure :: get_logical => config_get_logical
      procedure :: get_string => config_get_string
      procedure :: get_real => config_get_real
      procedure :: get_array => config_get_array
      procedure :: populate_emission_mapping_from_core
   end type ConfigManagerType

contains

   !> Helper to convert standard Fortran string to null-terminated C char array
   subroutine string_to_c(f_str, c_arr)
      character(len=*), intent(in) :: f_str
      character(kind=c_char), intent(out) :: c_arr(*)
      integer :: i, f_len

      f_len = len_trim(f_str)
      do i = 1, f_len
         c_arr(i) = f_str(i:i)
      end do
      c_arr(f_len+1) = c_null_char
   end subroutine string_to_c

   subroutine config_get_logical(this, path, val, rc, default)
      class(ConfigManagerType), intent(in) :: this
      character(len=*), intent(in) :: path
      logical, intent(out) :: val
      integer, intent(out) :: rc
      logical, intent(in), optional :: default

      integer(c_int) :: def_c, res_c
      character(kind=c_char) :: c_path(256)

      rc = CC_SUCCESS
      def_c = 0
      if (present(default)) then
         if (default) def_c = 1
      end if

      if (c_associated(this%core_ptr)) then
         call string_to_c(path, c_path)
         res_c = catchem_config_get_bool_path(this%core_ptr, c_path, def_c)
         val = (res_c /= 0)
      else
         val = .false.
         if (present(default)) val = default
      end if
   end subroutine config_get_logical

   subroutine config_get_string(this, path, val, rc, default)
      class(ConfigManagerType), intent(in) :: this
      character(len=*), intent(in) :: path
      character(len=*), intent(out) :: val
      integer, intent(out) :: rc
      character(len=*), intent(in), optional :: default

      character(kind=c_char) :: c_path(256), c_def(256), c_out(256)

      rc = CC_SUCCESS
      c_def(1) = c_null_char
      if (present(default)) call string_to_c(default, c_def)

      if (c_associated(this%core_ptr)) then
         call string_to_c(path, c_path)
         call catchem_config_get_string_path(this%core_ptr, c_path, c_out, c_def)
         val = trim(c_to_f_string(c_out))
      else
         val = ""
         if (present(default)) val = default
      end if
   end subroutine config_get_string

   subroutine config_get_real(this, path, val, rc, default)
      class(ConfigManagerType), intent(in) :: this
      character(len=*), intent(in) :: path
      real(fp), intent(out) :: val
      integer, intent(out) :: rc
      real(fp), intent(in), optional :: default

      real(c_double) :: def_c, res_c
      character(kind=c_char) :: c_path(256)

      rc = CC_SUCCESS
      def_c = 0.0_c_double
      if (present(default)) def_c = real(default, c_double)

      if (c_associated(this%core_ptr)) then
         call string_to_c(path, c_path)
         res_c = catchem_config_get_double_path(this%core_ptr, c_path, def_c)
         val = real(res_c, fp)
      else
         val = 0.0_fp
         if (present(default)) val = default
      end if
   end subroutine config_get_real

   subroutine config_get_array(this, path, val, rc, default_values)
      class(ConfigManagerType), intent(in) :: this
      character(len=*), intent(in) :: path
      character(len=*), allocatable, intent(out) :: val(:)
      integer, intent(out) :: rc
      character(len=*), intent(in), optional :: default_values(:)

      character(kind=c_char) :: c_path(256), c_out(256)
      integer :: n_items, i

      rc = CC_SUCCESS
      if (c_associated(this%core_ptr)) then
         call string_to_c(path, c_path)
         n_items = int(catchem_config_get_array_path_count(this%core_ptr, c_path))
         if (n_items > 0) then
            if (allocated(val)) deallocate(val)
            allocate(val(n_items))
            do i = 1, n_items
               call catchem_config_get_array_path_item(this%core_ptr, c_path, int(i - 1, c_int), c_out)
               val(i) = trim(c_to_f_string(c_out))
            end do
            return
         end if
      end if

      if (present(default_values)) then
         if (allocated(val)) deallocate(val)
         allocate(val(size(default_values)))
         val = default_values
      else
         if (allocated(val)) deallocate(val)
         allocate(val(0))
      end if
   end subroutine config_get_array

   !> \brief Converts null-terminated C character array to Fortran string
   pure function c_to_f_string(c_str) result(f_str)
      character(kind=c_char), intent(in) :: c_str(*)
      character(len=128) :: f_str
      integer :: i
      f_str = ""
      do i = 1, 128
         if (c_str(i) == c_null_char) exit
         f_str(i:i) = c_str(i)
      end do
   end function c_to_f_string

   !> \brief Populates Fortran emission mapping configuration from C++ Core
   !> \param[inout] this ConfigManager facade instance
   !> \param[in] core_ptr C pointer to catchem::Core
   subroutine populate_emission_mapping_from_core(this, core_ptr)
      class(ConfigManagerType), intent(inout) :: this
      type(c_ptr), intent(in) :: core_ptr

      integer :: n_cats, n_fields, n_maps, icat, ifield, imap, rc_dummy
      character(kind=c_char) :: c_cat_name(128), c_field(128), c_units(64), c_spec(64)
      real(c_double) :: scale_val
      real(fp) :: freq_val

      if (.not. c_associated(core_ptr)) return
      this%core_ptr = core_ptr

      ! Populate runtime and file paths parameters from C++ path queries
      call this%get_real("diagnostics/output/frequency", freq_val, rc_dummy, default=3600.0_fp)
      this%config_data%runtime%Output_Frequency = int(freq_val)

      call this%get_logical("diagnostics/output/enabled", this%config_data%runtime%DiagEnabled, rc_dummy, default=.true.)
      call this%get_logical("diagnostics/output/latlon_output", this%config_data%runtime%latlon_output, rc_dummy, default=.true.)
      call this%get_string("diagnostics/output/directory", this%config_data%file_paths%Output_Directory, rc_dummy, default="./output")
      call this%get_string("diagnostics/output/prefix", this%config_data%file_paths%Output_Prefix, rc_dummy, default="catchem_diag")
      call this%get_array("diagnostics/output/diag_list", this%config_data%runtime%diag_species, rc_dummy)

      if (catchem_config_is_emission_mapping_loaded(core_ptr) == 0) return

      n_cats = int(catchem_config_get_emission_category_count(core_ptr))
      this%config_data%emission_mapping%n_categories = n_cats
      if (allocated(this%config_data%emission_mapping%categories)) then
         deallocate(this%config_data%emission_mapping%categories)
      end if
      allocate(this%config_data%emission_mapping%categories(n_cats))

      do icat = 1, n_cats
         call catchem_config_get_emission_category_name(core_ptr, int(icat - 1, c_int), c_cat_name)
         this%config_data%emission_mapping%categories(icat)%category_name = trim(c_to_f_string(c_cat_name))
         this%config_data%emission_mapping%categories(icat)%is_active = .true.

         n_fields = int(catchem_config_get_emission_field_count(core_ptr, int(icat - 1, c_int)))
         this%config_data%emission_mapping%categories(icat)%n_emission_species = n_fields
         if (allocated(this%config_data%emission_mapping%categories(icat)%species_mappings)) then
            deallocate(this%config_data%emission_mapping%categories(icat)%species_mappings)
         end if
         allocate(this%config_data%emission_mapping%categories(icat)%species_mappings(n_fields))

         do ifield = 1, n_fields
            call catchem_config_get_emission_field_info(core_ptr, int(icat - 1, c_int), int(ifield - 1, c_int), &
               c_field, c_units, n_maps)

            associate(entry => this%config_data%emission_mapping%categories(icat)%species_mappings(ifield))
               entry%emission_field = trim(c_to_f_string(c_field))
               entry%units          = trim(c_to_f_string(c_units))
               entry%n_mappings     = n_maps
               if (allocated(entry%map)) deallocate(entry%map)
               if (allocated(entry%scale)) deallocate(entry%scale)
               if (allocated(entry%index)) deallocate(entry%index)
               allocate(entry%map(n_maps), entry%scale(n_maps), entry%index(n_maps))

               do imap = 1, n_maps
                  call catchem_config_get_emission_mapping_item(core_ptr, int(icat - 1, c_int), &
                     int(ifield - 1, c_int), int(imap - 1, c_int), c_spec, scale_val)
                  entry%map(imap)   = trim(c_to_f_string(c_spec))
                  entry%scale(imap) = real(scale_val, fp)
                  entry%index(imap) = 0
               end do
            end associate
         end do
      end do

      this%config_data%emission_mapping%is_loaded = .true.
   end subroutine populate_emission_mapping_from_core
end subroutine populate_emission_mapping_from_core

end module ConfigManager_Mod

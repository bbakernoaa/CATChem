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

   ! Interface mapping back to catchem_api.cpp for emission mapping queries
   interface
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
      integer :: Output_Frequency = 1
      integer :: CompressLev = 0
      logical :: latlon_output = .false.
      logical :: DiagEnabled = .true.
      character(len=64), allocatable :: diag_species(:)
   end type RuntimeConfig

   type :: FilePathConfig
      character(len=256) :: Output_Directory = "./"
      character(len=256) :: Output_Prefix = "catchem_diag"
   end type FilePathConfig

   type :: ConfigDataType
      type(EmissionMappingConfig) :: emission_mapping
      type(RuntimeConfig) :: runtime
      type(FilePathConfig) :: file_paths
   end type ConfigDataType

   type :: ConfigManagerType
      type(ConfigDataType) :: config_data
   contains
      procedure :: get_logical => config_get_logical
      procedure :: get_string => config_get_string
      procedure :: get_real => config_get_real
      procedure :: get_array => config_get_array
      procedure :: populate_emission_mapping_from_core
   end type ConfigManagerType

contains

   subroutine config_get_logical(this, path, val, rc, default)
      class(ConfigManagerType), intent(in) :: this
      character(len=*), intent(in) :: path
      logical, intent(out) :: val
      integer, intent(out) :: rc
      logical, intent(in), optional :: default

      associate(unused1 => this, unused2 => path)
      end associate

      val = .false.
      if (present(default)) val = default
      rc = CC_SUCCESS
   end subroutine config_get_logical

   subroutine config_get_string(this, path, val, rc, default)
      class(ConfigManagerType), intent(in) :: this
      character(len=*), intent(in) :: path
      character(len=*), intent(out) :: val
      integer, intent(out) :: rc
      character(len=*), intent(in), optional :: default

      associate(unused1 => this, unused2 => path)
      end associate

      val = ""
      if (present(default)) val = default
      rc = CC_SUCCESS
   end subroutine config_get_string

   subroutine config_get_real(this, path, val, rc, default)
      class(ConfigManagerType), intent(in) :: this
      character(len=*), intent(in) :: path
      real(fp), intent(out) :: val
      integer, intent(out) :: rc
      real(fp), intent(in), optional :: default

      associate(unused1 => this, unused2 => path)
      end associate

      val = 0.0_fp
      if (present(default)) val = default
      rc = CC_SUCCESS
   end subroutine config_get_real

   subroutine config_get_array(this, path, val, rc, default_values)
      class(ConfigManagerType), intent(in) :: this
      character(len=*), intent(in) :: path
      character(len=*), allocatable, intent(out) :: val(:)
      integer, intent(out) :: rc
      character(len=*), intent(in), optional :: default_values(:)

      associate(unused1 => this, unused2 => path)
      end associate

      if (present(default_values)) then
         allocate(val(size(default_values)))
         val = default_values
      else
         allocate(val(0))
      end if
      rc = CC_SUCCESS
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

      integer :: n_cats, n_fields, n_maps, icat, ifield, imap
      character(kind=c_char) :: c_cat_name(128), c_field(128), c_units(64), c_spec(64)
      real(c_double) :: scale_val

      if (.not. c_associated(core_ptr)) return
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

end module ConfigManager_Mod

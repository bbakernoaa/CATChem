!> \file CATChem_API.F90
!! \brief Modernized high-level BIND(C) API for host model integration
!!
!! This module provides the standard, backward-compatible CATChem_Model
!! derived type used by Earth System drivers (like NUOPC and UFS).
!! It delegates all memory management, synchronization, and process scheduling
!! directly to the modernized top-down C++ Core via standard BIND(C) bridges.
!!
module CATChem_API
   use iso_c_binding
   use catchem_bridge_precision, only: fp

   implicit none
   private

   public :: CATChem_Model

   !=========================================================================
   ! C-API Interfaces to catchem_api.cpp
   !=========================================================================
   interface
      type(c_ptr) function catchem_core_create_from_config(config_file) bind(C, name="catchem_core_create_from_config")
         import :: c_char, c_ptr
         character(kind=c_char), intent(in) :: config_file(*)
      end function

      type(c_ptr) function catchem_core_create_from_config_with_grid(config_file, ncols, nlevels) &
         bind(C, name="catchem_core_create_from_config_with_grid")
         import :: c_char, c_ptr, c_int
         character(kind=c_char), intent(in) :: config_file(*)
         integer(c_int), value :: ncols, nlevels
      end function

      subroutine catchem_core_destroy(core_ptr) bind(C, name="catchem_core_destroy")
         import :: c_ptr
         type(c_ptr), value :: core_ptr
      end subroutine

      subroutine catchem_register_carbchem_cpp() bind(C, name="catchem_register_carbchem_cpp")
      end subroutine

      subroutine catchem_register_drydep_cpp() bind(C, name="catchem_register_drydep_cpp")
      end subroutine

      subroutine catchem_register_dust_cpp() bind(C, name="catchem_register_dust_cpp")
      end subroutine

      subroutine catchem_register_seasalt_cpp() bind(C, name="catchem_register_seasalt_cpp")
      end subroutine

      subroutine catchem_register_settling_cpp() bind(C, name="catchem_register_settling_cpp")
      end subroutine

      subroutine catchem_register_so4chem_cpp() bind(C, name="catchem_register_so4chem_cpp")
      end subroutine

      subroutine catchem_register_wetdep_cpp() bind(C, name="catchem_register_wetdep_cpp")
      end subroutine

      type(c_ptr) function catchem_core_get_state_manager(core_ptr) bind(C, name="catchem_core_get_state_manager")
         import :: c_ptr
         type(c_ptr), value :: core_ptr
      end function

      subroutine catchem_core_add_process_by_name(core_ptr, name) bind(C, name="catchem_core_add_process_by_name")
         import :: c_ptr, c_char
         type(c_ptr), value :: core_ptr
         character(kind=c_char), intent(in) :: name(*)
      end subroutine

      integer(c_int) function catchem_core_get_num_processes(core_ptr) bind(C, name="catchem_core_get_num_processes")
         import :: c_ptr, c_int
         type(c_ptr), value :: core_ptr
      end function

      subroutine catchem_core_run_timestep(core_ptr, dt) bind(C, name="catchem_core_run_timestep")
         import :: c_ptr, c_double
         type(c_ptr), value :: core_ptr
         real(c_double), value :: dt
      end subroutine

      subroutine catchem_state_bind_met_3d(state_ptr, name, ptr) bind(C, name="catchem_state_bind_met_3d")
         import :: c_ptr, c_char
         type(c_ptr), value :: state_ptr
         character(kind=c_char), intent(in) :: name(*)
         type(c_ptr), value :: ptr
      end subroutine

      subroutine catchem_state_bind_met_2d(state_ptr, name, ptr) bind(C, name="catchem_state_bind_met_2d")
         import :: c_ptr, c_char
         type(c_ptr), value :: state_ptr
         character(kind=c_char), intent(in) :: name(*)
         type(c_ptr), value :: ptr
      end subroutine

      subroutine catchem_state_bind_unified_chemistry(state_ptr, ptr) bind(C, name="catchem_state_bind_unified_chemistry")
         import :: c_ptr
         type(c_ptr), value :: state_ptr
         type(c_ptr), value :: ptr
      end subroutine

      subroutine catchem_state_sync_to_device(state_ptr) bind(C, name="catchem_state_sync_to_device")
         import :: c_ptr
         type(c_ptr), value :: state_ptr
      end subroutine

      subroutine catchem_state_sync_to_host(state_ptr) bind(C, name="catchem_state_sync_to_host")
         import :: c_ptr
         type(c_ptr), value :: state_ptr
      end subroutine

      type(c_ptr) function catchem_state_get_species_conc_pointer(state_ptr, index) &
         bind(C, name="catchem_state_get_species_conc_pointer")
         import :: c_ptr, c_int
         type(c_ptr), value :: state_ptr
         integer(c_int), value :: index
      end function

      subroutine catchem_get_grid_dimensions(core_ptr, nx, ny, nz) bind(C, name="catchem_get_grid_dimensions")
         import :: c_ptr, c_int
         type(c_ptr), value :: core_ptr
         integer(c_int), intent(out) :: nx, ny, nz
      end subroutine

      real(c_double) function catchem_get_config_timestep(core_ptr) bind(C, name="catchem_get_config_timestep")
         import :: c_ptr, c_double
         type(c_ptr), value :: core_ptr
      end function

      integer(c_int) function catchem_config_get_output_frequency(core_ptr) &
         bind(C, name="catchem_config_get_output_frequency")
         import :: c_ptr, c_int
         type(c_ptr), value :: core_ptr
      end function

      integer(c_int) function catchem_config_get_compress_level(core_ptr) &
         bind(C, name="catchem_config_get_compress_level")
         import :: c_ptr, c_int
         type(c_ptr), value :: core_ptr
      end function

      subroutine catchem_config_get_output_directory(core_ptr, buffer, max_len) &
         bind(C, name="catchem_config_get_output_directory")
         import :: c_ptr, c_char, c_int
         type(c_ptr), value :: core_ptr
         character(kind=c_char), intent(out) :: buffer(*)
         integer(c_int), value :: max_len
      end subroutine

      subroutine catchem_config_get_output_prefix(core_ptr, buffer, max_len) &
         bind(C, name="catchem_config_get_output_prefix")
         import :: c_ptr, c_char, c_int
         type(c_ptr), value :: core_ptr
         character(kind=c_char), intent(out) :: buffer(*)
         integer(c_int), value :: max_len
      end subroutine

      integer(c_int) function catchem_config_get_latlon_output(core_ptr) &
         bind(C, name="catchem_config_get_latlon_output")
         import :: c_ptr, c_int
         type(c_ptr), value :: core_ptr
      end function

      integer(c_int) function catchem_config_get_diag_enabled(core_ptr) &
         bind(C, name="catchem_config_get_diag_enabled")
         import :: c_ptr, c_int
         type(c_ptr), value :: core_ptr
      end function

      integer(c_int) function catchem_config_get_diag_species_count(core_ptr) &
         bind(C, name="catchem_config_get_diag_species_count")
         import :: c_ptr, c_int
         type(c_ptr), value :: core_ptr
      end function

      subroutine catchem_config_get_diag_species_at(core_ptr, index, buffer, max_len) &
         bind(C, name="catchem_config_get_diag_species_at")
         import :: c_ptr, c_char, c_int
         type(c_ptr), value :: core_ptr
         integer(c_int), value :: index
         character(kind=c_char), intent(out) :: buffer(*)
         integer(c_int), value :: max_len
      end subroutine

      integer(c_int) function catchem_config_get_process_active(core_ptr, process_name) &
         bind(C, name="catchem_config_get_process_active")
         import :: c_ptr, c_char, c_int
         type(c_ptr), value :: core_ptr
         character(kind=c_char), intent(in) :: process_name(*)
      end function

      integer(c_int) function catchem_config_has_emission_mapping(core_ptr) &
         bind(C, name="catchem_config_has_emission_mapping")
         import :: c_ptr, c_int
         type(c_ptr), value :: core_ptr
      end function

      type(c_ptr) function catchem_diag_get_pointer(core_ptr, name) bind(C, name="catchem_diag_get_pointer")
         import :: c_ptr, c_char
         type(c_ptr), value :: core_ptr
         character(kind=c_char), intent(in) :: name(*)
      end function

      subroutine catchem_diag_register(core_ptr, name, desc, units, rank, dim1, dim2, dim3) &
         bind(C, name="catchem_diag_register")
         import :: c_ptr, c_char, c_int
         type(c_ptr), value :: core_ptr
         character(kind=c_char), intent(in) :: name(*)
         character(kind=c_char), intent(in) :: desc(*)
         character(kind=c_char), intent(in) :: units(*)
         integer(c_int), value :: rank, dim1, dim2, dim3
      end subroutine

      subroutine catchem_diag_sync_to_host(core_ptr) bind(C, name="catchem_diag_sync_to_host")
         import :: c_ptr
         type(c_ptr), value :: core_ptr
      end subroutine

      integer(c_int) function catchem_diag_get_count(core_ptr) bind(C, name="catchem_diag_get_count")
         import :: c_ptr, c_int
         type(c_ptr), value :: core_ptr
      end function

      subroutine catchem_diag_get_name_at(core_ptr, index, name_out) bind(C, name="catchem_diag_get_name_at")
         import :: c_ptr, c_int, c_char
         type(c_ptr), value :: core_ptr
         integer(c_int), value :: index
         character(kind=c_char), intent(out) :: name_out(*)
      end subroutine

      integer(c_int) function catchem_state_get_species_count(state_ptr) bind(C, name="catchem_state_get_species_count")
         import :: c_ptr, c_int
         type(c_ptr), value :: state_ptr
      end function

      real(c_double) function catchem_state_get_species_mw(state_ptr, index) bind(C, name="catchem_state_get_species_mw")
         import :: c_ptr, c_int, c_double
         type(c_ptr), value :: state_ptr
         integer(c_int), value :: index
      end function
   end interface

   !=========================================================================
   ! CATChem_Model Derived Type
   !=========================================================================
   type :: CATChem_Model
      type(c_ptr) :: cpp_core_ptr = c_null_ptr
      type(c_ptr) :: state_mgr_ptr = c_null_ptr
      character(len=64), allocatable, public :: required_fields(:)
      integer :: nx = 0
      integer :: ny = 0
      integer :: nz = 0
      logical :: initialized = .false.
   contains
      procedure :: initialize => model_initialize
      procedure :: finalize => model_finalize
      procedure :: add_process => model_add_process
      procedure :: get_num_processes => model_get_num_processes
      procedure :: run_timestep => model_run_timestep
      procedure :: get_diagnostic_names => model_get_diagnostic_names
      procedure :: get_diagnostic => model_get_diagnostic
      procedure :: register_diagnostic => model_register_diagnostic
      procedure :: get_diagnostic_ptr => model_get_diagnostic_ptr
      procedure :: get_species_conc_ptr => model_get_species_conc_ptr
      procedure :: get_diag_index_from_field => model_get_diag_index_from_field
      procedure :: get_required_met_index => model_get_required_met_index
      procedure :: get_grid_dimensions => model_get_grid_dimensions
      procedure :: is_initialized => model_is_initialized
      procedure :: bind_met_3d => model_bind_met_3d
      procedure :: bind_met_2d => model_bind_met_2d
      procedure :: bind_unified_chemistry_3d => model_bind_unified_chemistry_3d
      procedure :: bind_unified_chemistry_4d => model_bind_unified_chemistry_4d
      generic :: bind_unified_chemistry => bind_unified_chemistry_3d, bind_unified_chemistry_4d
      procedure :: get_output_frequency => model_get_output_frequency
      procedure :: get_compress_level => model_get_compress_level
      procedure :: get_output_directory => model_get_output_directory
      procedure :: get_output_prefix => model_get_output_prefix
      procedure :: is_latlon_output_enabled => model_is_latlon_output_enabled
      procedure :: is_diag_enabled => model_is_diag_enabled
      procedure :: get_diag_species_count => model_get_diag_species_count
      procedure :: get_diag_species_at => model_get_diag_species_at
      procedure :: is_process_active => model_is_process_active
      procedure :: has_emission_mapping => model_has_emission_mapping
   end type CATChem_Model

contains

   ! Helper to convert standard Fortran string to null-terminated C char array
   subroutine to_c_string(f_str, c_arr)
      character(len=*), intent(in) :: f_str
      character(kind=c_char), intent(out) :: c_arr(*)
      integer :: i, f_len

      f_len = len_trim(f_str)
      do i = 1, f_len
         c_arr(i) = f_str(i:i)
      end do
      c_arr(f_len+1) = c_null_char
   end subroutine to_c_string

   ! Initialize CATChem model and load configuration
   subroutine model_initialize(this, config_file, nx, ny, nz, nsoil, nsoiltype, nsurftype, rc)
      class(CATChem_Model), intent(inout) :: this
      character(len=*), intent(in) :: config_file
      integer, intent(in) :: nx, ny, nz
      integer, intent(in), optional :: nsoil, nsoiltype, nsurftype
      integer, intent(out) :: rc

      character(kind=c_char) :: c_filename(512)

      call to_c_string(config_file, c_filename)

      call catchem_register_carbchem_cpp()
      call catchem_register_drydep_cpp()
      call catchem_register_dust_cpp()
      call catchem_register_seasalt_cpp()
      call catchem_register_settling_cpp()
      call catchem_register_so4chem_cpp()
      call catchem_register_wetdep_cpp()

      ! Configuration comes from YAML; grid dimensions are dictated by host
      this%cpp_core_ptr = catchem_core_create_from_config_with_grid( &
         c_filename, int(nx*ny, c_int), int(nz, c_int))
      if (.not. c_associated(this%cpp_core_ptr)) then
         rc = -1
         return
      end if

      this%state_mgr_ptr = catchem_core_get_state_manager(this%cpp_core_ptr)

      ! Host-local dimensions are authoritative
      this%nx = nx
      this%ny = ny
      this%nz = nz

      this%initialized = .true.
      rc = 0
   end subroutine model_initialize

   ! Finalize model and release memory
   subroutine model_finalize(this, rc)
      class(CATChem_Model), intent(inout) :: this
      integer, intent(out) :: rc

      if (c_associated(this%cpp_core_ptr)) then
         call catchem_core_destroy(this%cpp_core_ptr)
         this%cpp_core_ptr = c_null_ptr
         this%state_mgr_ptr = c_null_ptr
      end if
      this%initialized = .false.
      rc = 0
   end subroutine model_finalize

   ! Register process list
   subroutine model_add_process(this, rc)
      class(CATChem_Model), intent(inout) :: this
      integer, intent(out) :: rc

      if (c_associated(this%cpp_core_ptr)) then
         rc = 0
      else
         rc = -1
      end if
   end subroutine model_add_process

   ! Get number of active processes
   function model_get_num_processes(this) result(num_processes)
      class(CATChem_Model), intent(inout) :: this
      integer :: num_processes

      num_processes = int(catchem_core_get_num_processes(this%cpp_core_ptr))
   end function model_get_num_processes

   ! Execute standard timestep
   subroutine model_run_timestep(this, timestep, dt, rc)
      class(CATChem_Model), intent(inout) :: this
      integer, intent(in) :: timestep
      real(fp), intent(in) :: dt
      integer, intent(out) :: rc

      if (c_associated(this%cpp_core_ptr)) then
         call catchem_state_sync_to_device(this%state_mgr_ptr)
         call catchem_core_run_timestep(this%cpp_core_ptr, real(dt, c_double))
         call catchem_state_sync_to_host(this%state_mgr_ptr)
         rc = 0
      else
         rc = -1
      end if
   end subroutine model_run_timestep

   ! Get available diagnostic field names
   subroutine model_get_diagnostic_names(this, diagnostic_names, diagnostic_fields, rc)
      class(CATChem_Model), intent(inout) :: this
      character(len=*), allocatable, intent(out) :: diagnostic_names(:)
      character(len=*), allocatable, optional, intent(out) :: diagnostic_fields(:)
      integer, intent(out) :: rc

      integer :: i, count
      character(kind=c_char) :: c_name(64)
      character(len=64) :: f_name

      count = int(catchem_diag_get_count(this%cpp_core_ptr))
      allocate(diagnostic_names(count))
      if (present(diagnostic_fields)) allocate(diagnostic_fields(count))

      do i = 1, count
         call catchem_diag_get_name_at(this%cpp_core_ptr, i - 1, c_name)
         f_name = ""
         block
            integer :: j
            do j = 1, 64
               if (c_name(j) == c_null_char) exit
               f_name(j:j) = c_name(j)
            end do
         end block
         diagnostic_names(i) = trim(f_name)
         if (present(diagnostic_fields)) diagnostic_fields(i) = trim(f_name)
      end do

      rc = 0
   end subroutine model_get_diagnostic_names

   ! Retrieve individual diagnostic data mapped directly from C++ heap
   subroutine model_get_diagnostic(this, diagnostic_name, diagnostic_data, rc)
      class(CATChem_Model), intent(inout) :: this
      character(len=*), intent(in) :: diagnostic_name
      real(fp), allocatable, intent(out) :: diagnostic_data(:,:,:)
      integer, intent(out) :: rc

      character(kind=c_char) :: c_name(64)
      type(c_ptr) :: raw_ptr
      real(c_double), pointer :: f_ptr(:,:,:) => null()

      call to_c_string(diagnostic_name, c_name)
      raw_ptr = catchem_diag_get_pointer(this%cpp_core_ptr, c_name)

      if (.not. c_associated(raw_ptr)) then
         rc = -1
         return
      end if

      ! Direct zero-copy slice map using ISO_C_BINDING!
      call c_f_pointer(raw_ptr, f_ptr, [this%nx, this%ny, this%nz])

      allocate(diagnostic_data(this%nx, this%ny, this%nz))
      diagnostic_data = real(f_ptr, fp)
      rc = 0
   end subroutine model_get_diagnostic

   ! Find index of registered diagnostic name
   function model_get_diag_index_from_field(this, field_name) result(found_index)
      class(CATChem_Model), intent(inout) :: this
      character(len=*), intent(in) :: field_name
      integer :: found_index

      character(len=128), allocatable :: diagnostic_names(:)
      integer :: rc, i

      found_index = 0
      call this%get_diagnostic_names(diagnostic_names, rc=rc)
      if (allocated(diagnostic_names)) then
         do i = 1, size(diagnostic_names)
            if (trim(field_name) == trim(diagnostic_names(i))) then
               found_index = i
               exit
            end if
         end do
      end if
   end function model_get_diag_index_from_field

   ! Required met field utility
   function model_get_required_met_index(this, var_name) result(found_index)
      class(CATChem_Model), intent(inout) :: this
      character(len=*), intent(in) :: var_name
      integer :: found_index

      found_index = 1
   end function model_get_required_met_index

   ! Grid dimension query
   subroutine model_get_grid_dimensions(this, nx, ny, nz)
      class(CATChem_Model), intent(in) :: this
      integer, intent(out) :: nx, ny, nz

      nx = this%nx
      ny = this%ny
      nz = this%nz
   end subroutine model_get_grid_dimensions

   ! Status checking
   function model_is_initialized(this) result(is_initialized)
      class(CATChem_Model), intent(in) :: this
      logical :: is_initialized

      is_initialized = this%initialized
   end function model_is_initialized

   ! Bind a 3D meteorological field
   subroutine model_bind_met_3d(this, name, arr)
      class(CATChem_Model), intent(inout) :: this
      character(len=*), intent(in) :: name
      real(c_double), target, contiguous, intent(in) :: arr(:,:,:)

      character(kind=c_char) :: c_name(64)

      call to_c_string(name, c_name)
      call catchem_state_bind_met_3d(this%state_mgr_ptr, c_name, c_loc(arr))
   end subroutine model_bind_met_3d

   ! Bind a 2D meteorological field
   subroutine model_bind_met_2d(this, name, arr)
      class(CATChem_Model), intent(inout) :: this
      character(len=*), intent(in) :: name
      real(c_double), target, contiguous, intent(in) :: arr(:,:)

      character(kind=c_char) :: c_name(64)

      call to_c_string(name, c_name)
      call catchem_state_bind_met_2d(this%state_mgr_ptr, c_name, c_loc(arr))
   end subroutine model_bind_met_2d

   ! Bind unified chemical concentrations 3D array
   subroutine model_bind_unified_chemistry_3d(this, arr)
      class(CATChem_Model), intent(inout) :: this
      real(c_double), target, contiguous, intent(in) :: arr(:,:,:)

      call catchem_state_bind_unified_chemistry(this%state_mgr_ptr, c_loc(arr))
   end subroutine model_bind_unified_chemistry_3d

   ! Bind unified chemical concentrations 4D array
   subroutine model_bind_unified_chemistry_4d(this, arr)
      class(CATChem_Model), intent(inout) :: this
      real(c_double), target, contiguous, intent(in) :: arr(:,:,:,:)

      call catchem_state_bind_unified_chemistry(this%state_mgr_ptr, c_loc(arr))
   end subroutine model_bind_unified_chemistry_4d

   ! Register a 3D diagnostic field in the C++ DiagnosticManager
   subroutine model_register_diagnostic(this, name, desc, units, dims, rc)
      class(CATChem_Model), intent(inout) :: this
      character(len=*), intent(in) :: name, desc, units
      integer, intent(in) :: dims(3)
      integer, intent(out) :: rc

      character(kind=c_char) :: c_name(64), c_desc(128), c_units(64)

      rc = -1
      if (.not. c_associated(this%cpp_core_ptr)) return
      call to_c_string(name, c_name)
      call to_c_string(desc, c_desc)
      call to_c_string(units, c_units)
      call catchem_diag_register(this%cpp_core_ptr, c_name, c_desc, c_units, &
         3_c_int, int(dims(1), c_int), int(dims(2), c_int), int(dims(3), c_int))
      rc = 0
   end subroutine model_register_diagnostic

   subroutine model_get_species_conc_ptr(this, species_index, ptr3d, dims, rc)
      class(CATChem_Model), intent(inout) :: this
      integer, intent(in) :: species_index
      real(fp), pointer, intent(out) :: ptr3d(:,:,:)
      integer, intent(in) :: dims(3)
      integer, intent(out) :: rc
      type(c_ptr) :: raw_ptr

      rc = -1
      nullify(ptr3d)
      if (.not. c_associated(this%state_mgr_ptr)) return
      raw_ptr = catchem_state_get_species_conc_pointer(this%state_mgr_ptr, int(species_index, c_int))
      if (.not. c_associated(raw_ptr)) return
      call c_f_pointer(raw_ptr, ptr3d, dims)
      rc = 0
   end subroutine model_get_species_conc_ptr

   ! Map the C++-owned storage of a registered 3D diagnostic for in-place
   ! writes (zero-copy; the same memory NUOPC export and NetCDF output read)
   subroutine model_get_diagnostic_ptr(this, name, ptr3d, dims, rc)
      class(CATChem_Model), intent(inout) :: this
      character(len=*), intent(in) :: name
      real(fp), pointer, intent(out) :: ptr3d(:, :, :)
      integer, intent(in) :: dims(3)
      integer, intent(out) :: rc

      character(kind=c_char) :: c_name(64)
      type(c_ptr) :: raw_ptr

      rc = -1
      nullify(ptr3d)
      if (.not. c_associated(this%cpp_core_ptr)) return
      call to_c_string(name, c_name)
      raw_ptr = catchem_diag_get_pointer(this%cpp_core_ptr, c_name)
      if (.not. c_associated(raw_ptr)) return
      call c_f_pointer(raw_ptr, ptr3d, dims)
      rc = 0
   end subroutine model_get_diagnostic_ptr

   function model_get_output_frequency(this) result(freq)
      class(CATChem_Model), intent(in) :: this
      integer :: freq
      freq = int(catchem_config_get_output_frequency(this%cpp_core_ptr))
   end function model_get_output_frequency

   function model_get_compress_level(this) result(clev)
      class(CATChem_Model), intent(in) :: this
      integer :: clev
      clev = int(catchem_config_get_compress_level(this%cpp_core_ptr))
   end function model_get_compress_level

   subroutine model_get_output_directory(this, dir_out)
      class(CATChem_Model), intent(in) :: this
      character(len=*), intent(out) :: dir_out
      character(kind=c_char) :: c_buf(256)
      integer :: i
      call catchem_config_get_output_directory(this%cpp_core_ptr, c_buf, 256_c_int)
      dir_out = ""
      do i = 1, 256
         if (c_buf(i) == c_null_char) exit
         dir_out(i:i) = c_buf(i)
      end do
   end subroutine model_get_output_directory

   subroutine model_get_output_prefix(this, prefix_out)
      class(CATChem_Model), intent(in) :: this
      character(len=*), intent(out) :: prefix_out
      character(kind=c_char) :: c_buf(256)
      integer :: i
      call catchem_config_get_output_prefix(this%cpp_core_ptr, c_buf, 256_c_int)
      prefix_out = ""
      do i = 1, 256
         if (c_buf(i) == c_null_char) exit
         prefix_out(i:i) = c_buf(i)
      end do
   end subroutine model_get_output_prefix

   function model_is_latlon_output_enabled(this) result(enabled)
      class(CATChem_Model), intent(in) :: this
      logical :: enabled
      enabled = (catchem_config_get_latlon_output(this%cpp_core_ptr) /= 0_c_int)
   end function model_is_latlon_output_enabled

   function model_is_diag_enabled(this) result(enabled)
      class(CATChem_Model), intent(in) :: this
      logical :: enabled
      enabled = (catchem_config_get_diag_enabled(this%cpp_core_ptr) /= 0_c_int)
   end function model_is_diag_enabled

   function model_get_diag_species_count(this) result(count)
      class(CATChem_Model), intent(in) :: this
      integer :: count
      count = int(catchem_config_get_diag_species_count(this%cpp_core_ptr))
   end function model_get_diag_species_count

   subroutine model_get_diag_species_at(this, index, species_name)
      class(CATChem_Model), intent(in) :: this
      integer, intent(in) :: index
      character(len=*), intent(out) :: species_name
      character(kind=c_char) :: c_buf(128)
      integer :: i
      call catchem_config_get_diag_species_at(this%cpp_core_ptr, int(index - 1, c_int), c_buf, 128_c_int)
      species_name = ""
      do i = 1, 128
         if (c_buf(i) == c_null_char) exit
         species_name(i:i) = c_buf(i)
      end do
   end subroutine model_get_diag_species_at

   function model_is_process_active(this, process_name) result(active)
      class(CATChem_Model), intent(in) :: this
      character(len=*), intent(in) :: process_name
      logical :: active
      character(kind=c_char) :: c_name(128)
      call to_c_string(process_name, c_name)
      active = (catchem_config_get_process_active(this%cpp_core_ptr, c_name) /= 0_c_int)
   end function model_is_process_active

   function model_has_emission_mapping(this) result(has_mapping)
      class(CATChem_Model), intent(in) :: this
      logical :: has_mapping
      has_mapping = (catchem_config_has_emission_mapping(this%cpp_core_ptr) /= 0_c_int)
   end function model_has_emission_mapping

end module CATChem_API

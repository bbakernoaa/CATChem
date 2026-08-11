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
   use precision_mod, only: fp
   use StateManager_Mod, only: StateManagerType

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

      type(c_ptr) function catchem_core_get_state_manager(core_ptr) bind(C, name="catchem_core_get_state_manager")
         import :: c_ptr
         type(c_ptr), value :: core_ptr
      end function

      subroutine catchem_core_add_process_by_name(core_ptr, name) bind(C, name="catchem_core_add_process_by_name")
         import :: c_ptr, c_char
         type(c_ptr), value :: core_ptr
         character(kind=c_char), intent(in) :: name(*)
      end subroutine

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

      subroutine catchem_get_grid_dimensions(core_ptr, nx, ny, nz) bind(C, name="catchem_get_grid_dimensions")
         import :: c_ptr, c_int
         type(c_ptr), value :: core_ptr
         integer(c_int), intent(out) :: nx, ny, nz
      end subroutine

      real(c_double) function catchem_get_config_timestep(core_ptr) bind(C, name="catchem_get_config_timestep")
         import :: c_ptr, c_double
         type(c_ptr), value :: core_ptr
      end function

      type(c_ptr) function catchem_diag_get_pointer(core_ptr, name) bind(C, name="catchem_diag_get_pointer")
         import :: c_ptr, c_char
         type(c_ptr), value :: core_ptr
         character(kind=c_char), intent(in) :: name(*)
      end function

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
      !> Fortran-facing state facade over the C++-owned state: sub-states are
      !> allocated and their met arrays bound into the C++ StateManager by
      !> model_initialize; get_state_manager returns this instance.
      type(StateManagerType), pointer :: facade => null()
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
      procedure :: get_diag_index_from_field => model_get_diag_index_from_field
      procedure :: get_required_met_index => model_get_required_met_index
      procedure :: get_grid_dimensions => model_get_grid_dimensions
      procedure :: is_initialized => model_is_initialized
      procedure :: bind_met_3d => model_bind_met_3d
      procedure :: bind_met_2d => model_bind_met_2d
      procedure :: bind_unified_chemistry_3d => model_bind_unified_chemistry_3d
      procedure :: bind_unified_chemistry_4d => model_bind_unified_chemistry_4d
      generic :: bind_unified_chemistry => bind_unified_chemistry_3d, bind_unified_chemistry_4d
      procedure :: get_diagnostic_manager => model_get_diagnostic_manager
      procedure :: get_state_manager => model_get_state_manager
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

      ! Configuration (species, processes, runtime options) comes from the
      ! YAML; grid dimensions are dictated by the host (e.g. UFS per-rank
      ! domain decomposition) — the YAML grid section applies to standalone
      ! runs only. Columns are flattened as nx*ny.
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

      ! Construct the Fortran state facade over the C++-owned state: allocate
      ! the sub-state containers, allocate the Fortran-owned met arrays, and
      ! bind them into the C++ StateManager so both sides share buffers.
      call model_build_facade(this, nx, ny, nz, nsoil, nsoiltype, nsurftype, rc)
      if (rc /= 0) return

      this%initialized = .true.
      rc = 0
   end subroutine model_initialize

   !> Allocate the Fortran state facade and bind its met arrays into the
   !> C++ StateManager. After this, StateManagerType getters return live
   !> sub-states and get_cpp_field retrievals resolve to the same memory.
   subroutine model_build_facade(this, nx, ny, nz, nsoil, nsoiltype, nsurftype, rc)
      use MetState_Mod, only: MetStateType
      class(CATChem_Model), intent(inout) :: this
      integer, intent(in) :: nx, ny, nz
      integer, intent(in), optional :: nsoil, nsoiltype, nsurftype
      integer, intent(out) :: rc

      type(MetStateType), pointer :: met

      allocate(this%facade)
      this%facade%cpp_ptr = this%state_mgr_ptr

      allocate(this%facade%error_mgr)
      allocate(this%facade%time_state)
      allocate(this%facade%chem_state)
      allocate(this%facade%config_mgr)
      allocate(this%facade%met_state)

      ! Geometry + allocation of all core met arrays (Fortran-owned)
      call this%facade%met_state%init(nx, ny, nz, nsoil, nsoiltype, nsurftype, &
         this%facade%error_mgr, rc)
      if (rc /= 0) return

      met => this%facade%met_state

      ! Optional surface fields retrieved by get_met_state_ptr but not part of
      ! the 'ALL' allocation set
      if (.not. associated(met%FROCEAN)) allocate(met%FROCEAN(nx, ny))
      if (.not. associated(met%FRSEAICE)) allocate(met%FRSEAICE(nx, ny))
      if (.not. associated(met%SST)) allocate(met%SST(nx, ny))

      ! Bind every array that get_met_state_ptr retrieves, so retrieval
      ! resolves to this same memory instead of nullifying the members.
      call bind_met_3d_ptr(this, "T", met%T)
      call bind_met_3d_ptr(this, "QV", met%QV)
      call bind_met_3d_ptr(this, "RH", met%RH)
      call bind_met_3d_ptr(this, "PMID", met%PMID)
      call bind_met_3d_ptr(this, "PEDGE", met%PEDGE)
      call bind_met_3d_ptr(this, "AIRDEN", met%AIRDEN)
      call bind_met_3d_ptr(this, "AIRDEN_DRY", met%AIRDEN_DRY)
      call bind_met_3d_ptr(this, "BXHEIGHT", met%BXHEIGHT)
      call bind_met_3d_ptr(this, "DELP", met%DELP)
      call bind_met_3d_ptr(this, "DELP_DRY", met%DELP_DRY)

      call bind_met_2d_ptr(this, "PS", met%PS)
      call bind_met_2d_ptr(this, "TS", met%TS)
      call bind_met_2d_ptr(this, "PBLH", met%PBLH)
      call bind_met_2d_ptr(this, "USTAR", met%USTAR)
      call bind_met_2d_ptr(this, "HFLUX", met%HFLUX)
      call bind_met_2d_ptr(this, "OBK", met%OBK)
      call bind_met_2d_ptr(this, "LAT", met%LAT)
      call bind_met_2d_ptr(this, "LON", met%LON)
      call bind_met_2d_ptr(this, "Z0", met%Z0)
      call bind_met_2d_ptr(this, "AREA_M2", met%AREA_M2)
      call bind_met_2d_ptr(this, "FROCEAN", met%FROCEAN)
      call bind_met_2d_ptr(this, "FRSEAICE", met%FRSEAICE)
      call bind_met_2d_ptr(this, "SST", met%SST)

      rc = 0
   end subroutine model_build_facade

   !> Bind a Fortran-owned 3D pointer array into the C++ StateManager.
   subroutine bind_met_3d_ptr(this, name, arr)
      class(CATChem_Model), intent(inout) :: this
      character(len=*), intent(in) :: name
      real(fp), pointer, intent(in) :: arr(:,:,:)
      character(kind=c_char) :: c_name(64)
      if (.not. associated(arr)) return
      call to_c_string(name, c_name)
      call catchem_state_bind_met_3d(this%state_mgr_ptr, c_name, c_loc(arr))
   end subroutine bind_met_3d_ptr

   !> Bind a Fortran-owned 2D pointer array into the C++ StateManager.
   subroutine bind_met_2d_ptr(this, name, arr)
      class(CATChem_Model), intent(inout) :: this
      character(len=*), intent(in) :: name
      real(fp), pointer, intent(in) :: arr(:,:)
      character(kind=c_char) :: c_name(64)
      if (.not. associated(arr)) return
      call to_c_string(name, c_name)
      call catchem_state_bind_met_2d(this%state_mgr_ptr, c_name, c_loc(arr))
   end subroutine bind_met_2d_ptr

   ! Finalize model and release memory
   subroutine model_finalize(this, rc)
      class(CATChem_Model), intent(inout) :: this
      integer, intent(out) :: rc

      if (c_associated(this%cpp_core_ptr)) then
         call catchem_core_destroy(this%cpp_core_ptr)
         this%cpp_core_ptr = c_null_ptr
         this%state_mgr_ptr = c_null_ptr
      end if
      ! Release the facade containers (their pointer-array members are
      ! intentionally left to process teardown; the C++ side never owned them)
      if (associated(this%facade)) then
         if (associated(this%facade%met_state)) deallocate(this%facade%met_state)
         if (associated(this%facade%chem_state)) deallocate(this%facade%chem_state)
         if (associated(this%facade%config_mgr)) deallocate(this%facade%config_mgr)
         if (associated(this%facade%error_mgr)) deallocate(this%facade%error_mgr)
         if (associated(this%facade%time_state)) deallocate(this%facade%time_state)
         deallocate(this%facade)
      end if
      this%initialized = .false.
      rc = 0
   end subroutine model_finalize

   ! Register process list
   subroutine model_add_process(this, rc)
      class(CATChem_Model), intent(inout) :: this
      integer, intent(out) :: rc

      ! Process registration and instantiation are managed dynamically in the modern C++ orchestrator
      rc = 0
   end subroutine model_add_process

   ! Get number of active processes
   function model_get_num_processes(this) result(num_processes)
      class(CATChem_Model), intent(inout) :: this
      integer :: num_processes

      ! Dummy backward compatible process count
      num_processes = 7
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
      real(c_double), target, intent(in) :: arr(:,:,:)

      character(kind=c_char) :: c_name(64)

      call to_c_string(name, c_name)
      call catchem_state_bind_met_3d(this%state_mgr_ptr, c_name, c_loc(arr))
   end subroutine model_bind_met_3d

   ! Bind a 2D meteorological field
   subroutine model_bind_met_2d(this, name, arr)
      class(CATChem_Model), intent(inout) :: this
      character(len=*), intent(in) :: name
      real(c_double), target, intent(in) :: arr(:,:)

      character(kind=c_char) :: c_name(64)

      call to_c_string(name, c_name)
      call catchem_state_bind_met_2d(this%state_mgr_ptr, c_name, c_loc(arr))
   end subroutine model_bind_met_2d

   ! Bind unified chemical concentrations 3D array
   subroutine model_bind_unified_chemistry_3d(this, arr)
      class(CATChem_Model), intent(inout) :: this
      real(c_double), target, intent(in) :: arr(:,:,:)

      call catchem_state_bind_unified_chemistry(this%state_mgr_ptr, c_loc(arr))
   end subroutine model_bind_unified_chemistry_3d

   ! Bind unified chemical concentrations 4D array
   subroutine model_bind_unified_chemistry_4d(this, arr)
      class(CATChem_Model), intent(inout) :: this
      real(c_double), target, intent(in) :: arr(:,:,:,:)

      call catchem_state_bind_unified_chemistry(this%state_mgr_ptr, c_loc(arr))
   end subroutine model_bind_unified_chemistry_4d

   ! Get pointer to DiagnosticManager
   function model_get_diagnostic_manager(this) result(ptr)
      use DiagnosticManager_Mod, only: DiagnosticManagerType
      class(CATChem_Model), intent(inout) :: this
      type(DiagnosticManagerType), pointer :: ptr
      type(DiagnosticManagerType), save, target :: saved_diag_mgr
      ptr => saved_diag_mgr
   end function model_get_diagnostic_manager

   ! Get pointer to the Fortran state facade (constructed by initialize)
   function model_get_state_manager(this) result(ptr)
      class(CATChem_Model), intent(inout) :: this
      type(StateManagerType), pointer :: ptr
      if (.not. associated(this%facade)) then
         error stop "CATChem_Model%get_state_manager: model not initialized "// &
            "(CATChem_Model%initialize constructs the state facade)"
      end if
      ptr => this%facade
   end function model_get_state_manager

end module CATChem_API

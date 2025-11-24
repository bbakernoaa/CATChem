!> \file settlingProcess_Mod.F90
!! \brief settling process implementation
!! \ingroup process_modules
!!
!! \author CATChem Development Team
!! \date 2025
!! \version 1.0
!!
!! Gravitational settling process for atmospheric particles
!!
module settlingProcess_Mod
   use precision_mod
   use state_mod, only : StateContainerType
   use error_mod
   use ProcessInterface_Mod
   use StokesschemeScheme_Mod
   use IntermediatereynoldsschemeScheme_Mod
   use settlingCommon_Mod

   implicit none
   private

   public :: settlingProcessType

   !> settling process type extending ProcessInterface
   type, extends(ProcessInterface) :: settlingProcessType
      private

      ! Process-specific configuration
      character(len=32) :: selected_scheme = 'Stokesscheme'

   contains
      ! Required ProcessInterface methods
      procedure :: init => settling_process_init
      procedure :: run => settling_process_run
      procedure :: finalize => settling_process_finalize
   end type settlingProcessType

contains

   !> Initialize settling process
   subroutine settling_process_init(this, container, rc)
      class(settlingProcessType), intent(inout) :: this
      type(StateContainerType), intent(inout) :: container
      integer, intent(out) :: rc

      type(ErrorManagerType), pointer :: error_mgr
      character(len=256) :: message

      rc = CC_SUCCESS

      error_mgr => container%get_error_manager()

      ! Set process metadata
      this%name = 'settling'
      this%version = '1.0'
      this%description = 'Gravitational settling process for atmospheric particles'


      ! Validate inputs
      ! Basic validation
      if (.not. container%is_initialized) then
         call error_mgr%report_error("StateContainer not initialized")
         rc = -1
         return
      end if

      this%is_initialized = .true.
      this%is_active = .true.

      write(message, '(A,A,A)') 'settling process initialized with scheme: ', &
         trim(this%selected_scheme)
      call error_mgr%report_info(message)

   end subroutine settling_process_init

   !> Run settling process
   subroutine settling_process_run(this, container, rc)
      class(settlingProcessType), intent(inout) :: this
      type(StateContainerType), intent(inout) :: container
      integer, intent(out) :: rc

      type(MetStateType), pointer :: met_state
      type(ChemStateType), pointer :: chem_state
      type(ErrorManagerType), pointer :: error_mgr

      rc = CC_SUCCESS

      if (.not. this%is_ready()) then
         rc = -1
         return
      end if

      ! Get state pointers
      met_state => container%get_met_state_ptr()
      chem_state => container%get_chem_state_ptr()
      error_mgr => container%get_error_manager()

      ! Execute scheme-specific calculations
      select case (trim(this%selected_scheme))
       case ('Stokesscheme')
         call this%run_stokesscheme_scheme(container, rc)
       case ('Intermediatereynoldsscheme')
         call this%run_intermediatereynoldsscheme_scheme(container, rc)
       case default
         call error_mgr%report_error("Unknown scheme: " // trim(this%selected_scheme))
         rc = -1
         return
      end select

   end subroutine settling_process_run

   !> Finalize settling process
   subroutine settling_process_finalize(this, rc)
      class(settlingProcessType), intent(inout) :: this
      integer, intent(out) :: rc

      rc = CC_SUCCESS

      this%is_initialized = .false.
      this%is_active = .false.

   end subroutine settling_process_finalize

   !> Run Stokesscheme scheme
   subroutine run_stokesscheme_scheme(this, container, rc)
      class(settlingProcessType), intent(inout) :: this
      type(StateContainerType), intent(inout) :: container
      integer, intent(out) :: rc

      type(MetStateType), pointer :: met_state
      type(ChemStateType), pointer :: chem_state

      ! Local arrays for scheme interface

      rc = CC_SUCCESS

      ! Get state pointers
      met_state => container%get_met_state_ptr()
      chem_state => container%get_chem_state_ptr()

      ! TODO: Extract data from StateContainer to local arrays
      ! This is where you'd get the actual meteorological and chemical data
      ! from the StateContainer and prepare it for the scheme

      ! TODO: Allocate arrays based on grid dimensions
      ! call get_grid_dimensions(nx, ny, nz, n_species)
      ! allocate(temperature(nx, ny, nz), ...)

      ! Call the scheme calculation
      call stokesscheme_calculate( &
         this%dt, rc)

      if (rc /= CC_SUCCESS) return

      ! TODO: Update StateContainer with results
      ! This is where you'd take the results from the scheme and update
      ! the appropriate state in the StateContainer

   end subroutine run_stokesscheme_scheme

   !> Run Intermediatereynoldsscheme scheme
   subroutine run_intermediatereynoldsscheme_scheme(this, container, rc)
      class(settlingProcessType), intent(inout) :: this
      type(StateContainerType), intent(inout) :: container
      integer, intent(out) :: rc

      type(MetStateType), pointer :: met_state
      type(ChemStateType), pointer :: chem_state

      ! Local arrays for scheme interface

      rc = CC_SUCCESS

      ! Get state pointers
      met_state => container%get_met_state_ptr()
      chem_state => container%get_chem_state_ptr()

      ! TODO: Extract data from StateContainer to local arrays
      ! This is where you'd get the actual meteorological and chemical data
      ! from the StateContainer and prepare it for the scheme

      ! TODO: Allocate arrays based on grid dimensions
      ! call get_grid_dimensions(nx, ny, nz, n_species)
      ! allocate(temperature(nx, ny, nz), ...)

      ! Call the scheme calculation
      call intermediatereynoldsscheme_calculate( &
         this%dt, rc)

      if (rc /= CC_SUCCESS) return

      ! TODO: Update StateContainer with results
      ! This is where you'd take the results from the scheme and update
      ! the appropriate state in the StateContainer

   end subroutine run_intermediatereynoldsscheme_scheme

end module settlingProcess_Mod

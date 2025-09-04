# Creating New Processes

This guide walks you through the complete process of creating a new atmospheric process in CATChem manually, from initial design to testing and integration. Please only use this guide for processes where the **[Process Generator](generator-tutorial.md)** is not applicable.

## Planning Your Process

Before writing code, carefully plan your process implementation:

### 1. Define the Chemical Process

- **What does it do?** (e.g., "Calculate dry deposition of gas-phase species")
- **What inputs does it need?** (meteorology, species concentrations, surface properties)
- **What outputs does it produce?** (updated concentrations, fluxes, diagnostics)
- **What schemes/algorithms will it support?** (resistance model, big-leaf model, etc.)

### 2. Identify Dependencies

- Which species does it affect?
- What meteorological fields are required?
- Does it need emissions data or surface properties?
- What diagnostic outputs should it provide?

### 3. Choose Integration Approach

- **Column-based**: Most processes (chemistry, deposition, settling)
- **Horizontal**: Some emission processes
- **3D**: Advection, turbulent mixing

## Step-by-Step Implementation

### Step 1: Create Directory Structure

```bash
cd src/process
mkdir newprocess
cd newprocess
mkdir schemes

# Create required files
touch newprocessProcess_Mod.F90
touch newprocessCommon_Mod.F90
touch CMakeLists.txt
touch schemes/CMakeLists.txt
```

### Step 2: Use the Process Generator

CATChem provides a Python generator to create process templates:

```bash
cd util
python catchem_generate_process.py \
    --name newprocess \
    --description "New atmospheric process" \
    --schemes scheme1,scheme2 \
    --species O3,NO2,PM25 \
    --diagnostics rate,flux
```

This creates the basic file structure and boilerplate code.

### Step 3: Implement the Process Module

Edit `newprocessProcess_Mod.F90`:

```fortran
!> \file newprocessProcess_Mod.F90
!! \brief New atmospheric process implementation
!! \ingroup process_modules
!!
!! \author Your Name
!! \date 2025
!! \version 1.0
!!
!! Detailed description of what this process does,
!! including physical equations and assumptions.
!!
module newprocessProcess_Mod
   use precision_mod
   use state_mod, only : StateContainerType
   use error_mod
   use ProcessInterface_Mod
   use newprocessCommon_Mod
   ! Add scheme modules
   use Scheme1Scheme_Mod
   use Scheme2Scheme_Mod

   implicit none
   private

   public :: newprocessProcessType

   !> New process type extending ProcessInterface
   type, extends(ProcessInterface) :: newprocessProcessType
      private

      ! Process-specific configuration
      character(len=32) :: selected_scheme = 'scheme1'
      real(fp) :: process_parameter = 1.0_fp
      logical :: enable_diagnostics = .true.

      ! Process-specific data
      real(fp), allocatable :: work_array(:,:,:)

   contains
      ! Required ProcessInterface methods
      procedure :: init => newprocess_init
      procedure :: run => newprocess_run
      procedure :: finalize => newprocess_finalize

      ! Process-specific methods
      procedure, private :: setup_diagnostics
      procedure, private :: validate_configuration
   end type newprocessProcessType

contains

   !> Initialize new process
   subroutine newprocess_init(this, container, rc)
      class(newprocessProcessType), intent(inout) :: this
      type(StateContainerType), intent(inout) :: container
      integer, intent(out) :: rc

      type(ErrorManagerType), pointer :: error_mgr
      type(ConfigDataType), pointer :: config
      character(len=256) :: message
      integer :: local_rc

      rc = CC_SUCCESS
      error_mgr => container%get_error_manager()

      call error_mgr%push_context("newprocess_init")

      ! Set process metadata
      this%name = 'newprocess'
      this%version = '1.0'
      this%description = 'New atmospheric process'

      ! Get configuration
      config => container%get_config_ptr()

      ! Read process-specific configuration
      call config%get_value('processes.newprocess.scheme', &
                           this%selected_scheme, local_rc)
      if (local_rc /= CC_SUCCESS) then
         call error_mgr%report_warning("Using default scheme: " // &
                                      trim(this%selected_scheme))
      end if

      call config%get_value('processes.newprocess.process_parameter', &
                           this%process_parameter, local_rc)
      if (local_rc /= CC_SUCCESS) then
         call error_mgr%report_warning("Using default process parameter")
      end if

      ! Validate configuration
      call this%validate_configuration(container, local_rc)
      if (local_rc /= CC_SUCCESS) then
         call error_mgr%report_error("Configuration validation failed")
         rc = local_rc
         call error_mgr%pop_context()
         return
      end if

      ! Allocate work arrays
      ! Get grid dimensions from container
      associate (grid => container%get_grid_manager())
         allocate(this%work_array(grid%nx, grid%ny, grid%nz))
         this%work_array = 0.0_fp
      end associate

      ! Set up diagnostics
      if (this%enable_diagnostics) then
         call this%setup_diagnostics(container, local_rc)
         if (local_rc /= CC_SUCCESS) then
            call error_mgr%report_error("Diagnostic setup failed")
            rc = local_rc
            call error_mgr%pop_context()
            return
         end if
      end if

      this%is_initialized = .true.
      this%is_active = .true.

      write(message, '(A,A,A)') 'New process initialized with scheme: ', &
                                trim(this%selected_scheme)
      call error_mgr%report_info(message)

      call error_mgr%pop_context()

   end subroutine newprocess_init

   !> Run new process
   subroutine newprocess_run(this, container, rc)
      class(newprocessProcessType), intent(inout) :: this
      type(StateContainerType), intent(inout) :: container
      integer, intent(out) :: rc

      type(MetStateType), pointer :: met_state
      type(ChemStateType), pointer :: chem_state
      type(ErrorManagerType), pointer :: error_mgr
      integer :: local_rc

      rc = CC_SUCCESS

      if (.not. this%is_ready()) then
         rc = CC_FAILURE
         return
      end if

      error_mgr => container%get_error_manager()
      call error_mgr%push_context("newprocess_run")

      ! Get state pointers
      met_state => container%get_met_state_ptr()
      chem_state => container%get_chem_state_ptr()

      ! Execute scheme-specific calculations
      select case (trim(this%selected_scheme))
      case ('scheme1')
         call scheme1_calculate(container, this%work_array, local_rc)
      case ('scheme2')
         call scheme2_calculate(container, this%work_array, local_rc)
      case default
         call error_mgr%report_error("Unknown scheme: " // &
                                    trim(this%selected_scheme))
         rc = CC_FAILURE
         call error_mgr%pop_context()
         return
      end select

      if (local_rc /= CC_SUCCESS) then
         call error_mgr%report_error("Scheme calculation failed")
         rc = local_rc
         call error_mgr%pop_context()
         return
      end if

      ! Update diagnostics if enabled
      if (this%enable_diagnostics) then
         call this%update_diagnostics(container, local_rc)
      end if

      call error_mgr%pop_context()

   end subroutine newprocess_run

   !> Finalize new process
   subroutine newprocess_finalize(this, rc)
      class(newprocessProcessType), intent(inout) :: this
      integer, intent(out) :: rc

      rc = CC_SUCCESS

      ! Deallocate work arrays
      if (allocated(this%work_array)) then
         deallocate(this%work_array)
      end if

      this%is_initialized = .false.
      this%is_active = .false.

   end subroutine newprocess_finalize

   !> Validate process configuration
   subroutine validate_configuration(this, container, rc)
      class(newprocessProcessType), intent(inout) :: this
      type(StateContainerType), intent(inout) :: container
      integer, intent(out) :: rc

      type(ErrorManagerType), pointer :: error_mgr
      character(len=32), parameter :: valid_schemes(2) = &
         ['scheme1', 'scheme2']
      integer :: i
      logical :: scheme_found

      rc = CC_SUCCESS
      error_mgr => container%get_error_manager()

      ! Validate scheme selection
      scheme_found = .false.
      do i = 1, size(valid_schemes)
         if (trim(this%selected_scheme) == trim(valid_schemes(i))) then
            scheme_found = .true.
            exit
         end if
      end do

      if (.not. scheme_found) then
         call error_mgr%report_error("Invalid scheme: " // &
                                    trim(this%selected_scheme))
         rc = CC_FAILURE
         return
      end if

      ! Validate parameter ranges
      if (this%process_parameter <= 0.0_fp) then
         call error_mgr%report_error("Process parameter must be positive")
         rc = CC_FAILURE
         return
      end if

   end subroutine validate_configuration

   !> Set up diagnostic outputs
   subroutine setup_diagnostics(this, container, rc)
      class(newprocessProcessType), intent(inout) :: this
      type(StateContainerType), intent(inout) :: container
      integer, intent(out) :: rc

      type(DiagnosticManagerType), pointer :: diag_mgr
      type(GridManagerType), pointer :: grid_mgr

      rc = CC_SUCCESS
      diag_mgr => container%get_diagnostic_manager()
      grid_mgr => container%get_grid_manager()

      ! Register diagnostic fields
      call diag_mgr%register_field('newprocess_rate', &
                                  'New process rate', &
                                  'kg/m3/s', &
                                  [grid_mgr%nx, grid_mgr%ny, grid_mgr%nz])

      call diag_mgr%register_field('newprocess_flux', &
                                  'New process flux', &
                                  'kg/m2/s', &
                                  [grid_mgr%nx, grid_mgr%ny])

   end subroutine setup_diagnostics

end module newprocessProcess_Mod
```

### Step 4: Implement Common Utilities

Edit `newprocessCommon_Mod.F90`:

```fortran
!> \file newprocessCommon_Mod.F90
!! \brief Common utilities for new process
!! \ingroup process_modules
!!
module newprocessCommon_Mod
   use precision_mod
   use constants, only : R_GAS, AVOGADRO

   implicit none
   private

   public :: calculate_common_parameter
   public :: convert_units

   ! Process-specific constants
   real(fp), parameter :: PROCESS_CONSTANT = 1.23e-4_fp

contains

   !> Calculate common parameter used by multiple schemes
   pure function calculate_common_parameter(temperature, pressure) result(param)
      real(fp), intent(in) :: temperature  !< Temperature [K]
      real(fp), intent(in) :: pressure     !< Pressure [Pa]
      real(fp) :: param                    !< Common parameter

      param = PROCESS_CONSTANT * (temperature / 273.15_fp) * &
              (101325.0_fp / pressure)

   end function calculate_common_parameter

   !> Convert concentration units
   pure function convert_units(conc_in, molar_mass, temp, press) result(conc_out)
      real(fp), intent(in) :: conc_in      !< Input concentration [mol/m3]
      real(fp), intent(in) :: molar_mass   !< Molar mass [g/mol]
      real(fp), intent(in) :: temp         !< Temperature [K]
      real(fp), intent(in) :: press        !< Pressure [Pa]
      real(fp) :: conc_out                 !< Output concentration [μg/m3]

      conc_out = conc_in * molar_mass * 1.0e6_fp

   end function convert_units

end module newprocessCommon_Mod
```

### Step 5: Implement Schemes

Create `schemes/Scheme1Scheme_Mod.F90`:

```fortran
!> \file Scheme1Scheme_Mod.F90
!! \brief Scheme 1 implementation for new process
!! \ingroup scheme_modules
!!
module Scheme1Scheme_Mod
   use precision_mod
   use state_mod, only : StateContainerType
   use error_mod
   use newprocessCommon_Mod

   implicit none
   private

   public :: scheme1_calculate

contains

   !> Calculate using Scheme 1 algorithm
   subroutine scheme1_calculate(container, work_array, rc)
      type(StateContainerType), intent(inout) :: container
      real(fp), intent(inout) :: work_array(:,:,:)
      integer, intent(out) :: rc

      type(MetStateType), pointer :: met_state
      type(ChemStateType), pointer :: chem_state
      real(fp), pointer :: temperature(:,:,:)
      real(fp), pointer :: pressure(:,:,:)
      real(fp), pointer :: species_conc(:,:,:)
      integer :: i, j, k

      rc = CC_SUCCESS

      ! Get state data
      met_state => container%get_met_state_ptr()
      chem_state => container%get_chem_state_ptr()

      temperature => met_state%get_field('temperature')
      pressure => met_state%get_field('pressure')
      species_conc => chem_state%get_species_ptr('O3')  ! Example

      ! Scheme 1 calculations
      do k = 1, size(work_array, 3)
         do j = 1, size(work_array, 2)
            do i = 1, size(work_array, 1)
               work_array(i,j,k) = calculate_common_parameter( &
                  temperature(i,j,k), pressure(i,j,k))

               ! Update species concentration
               species_conc(i,j,k) = species_conc(i,j,k) * &
                  (1.0_fp - work_array(i,j,k) * container%dt)
            end do
         end do
      end do

   end subroutine scheme1_calculate

end module Scheme1Scheme_Mod
```

### Step 6: Create Build Configuration

Create `CMakeLists.txt`:

```cmake
# Process: newprocess
set(PROCESS_NAME newprocess)

# Source files
set(PROCESS_SOURCES
    newprocessProcess_Mod.F90
    newprocessCommon_Mod.F90
)

# Create process library
add_library(${PROCESS_NAME}_process ${PROCESS_SOURCES})

# Set module output directory
set_target_properties(${PROCESS_NAME}_process PROPERTIES
    Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules
)

# Link dependencies
target_link_libraries(${PROCESS_NAME}_process
    PRIVATE
        catchem_core
        catchem_process_interface
)

# Include directories
target_include_directories(${PROCESS_NAME}_process
    PRIVATE
        ${CMAKE_BINARY_DIR}/modules
)

# Add schemes subdirectory
add_subdirectory(schemes)

# Link scheme libraries
target_link_libraries(${PROCESS_NAME}_process
    PRIVATE
        ${PROCESS_NAME}_schemes
)
```

Create `schemes/CMakeLists.txt`:

```cmake
# Schemes for newprocess
set(PROCESS_NAME newprocess)

# Scheme source files
set(SCHEME_SOURCES
    Scheme1Scheme_Mod.F90
    Scheme2Scheme_Mod.F90
)

# Create schemes library
add_library(${PROCESS_NAME}_schemes ${SCHEME_SOURCES})

# Set module output directory
set_target_properties(${PROCESS_NAME}_schemes PROPERTIES
    Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules
)

# Link dependencies
target_link_libraries(${PROCESS_NAME}_schemes
    PRIVATE
        catchem_core
        ${PROCESS_NAME}_process
)

# Include directories
target_include_directories(${PROCESS_NAME}_schemes
    PRIVATE
        ${CMAKE_BINARY_DIR}/modules
)
```

### Step 7: Update Parent CMakeLists.txt

Add to `src/process/CMakeLists.txt`:

```cmake
# Add new process
add_subdirectory(newprocess)

# Update process library dependencies
target_link_libraries(catchem_processes
    PUBLIC
        # ...existing processes...
        newprocess_process
)
```

### Step 8: Create Configuration Template

Create `parm/config/newprocess_config.yml`:

```yaml
# New Process Configuration Template
newprocess:
  enabled: true
  scheme: scheme1
  timestep: 60.0  # seconds

  parameters:
    process_parameter: 1.0
    enable_diagnostics: true

  species:
    - O3
    - NO2
    - PM25

  diagnostics:
    - name: newprocess_rate
      description: "New process rate"
      units: "kg/m3/s"
      output_frequency: hourly

    - name: newprocess_flux
      description: "New process flux"
      units: "kg/m2/s"
      output_frequency: hourly
```

### Step 9: Create Unit Tests

Create `tests/test_newprocess.F90`:

```fortran
!> \file test_newprocess.F90
!! \brief Unit tests for new process
!!
program test_newprocess
   use newprocessProcess_Mod
   use testing_mod
   use state_mod

   implicit none

   type(newprocessProcessType) :: process
   type(StateContainerType) :: container
   integer :: rc

   call testing_init("New Process Tests")

   ! Test 1: Process initialization
   call test_process_init()

   ! Test 2: Process calculation
   call test_process_run()

   ! Test 3: Process finalization
   call test_process_finalize()

   call testing_finalize()

contains

   subroutine test_process_init()
      call testing_start_test("Process Initialization")

      ! Create test state container
      call create_test_state_container(container)

      ! Initialize process
      call process%init(container, rc)
      call assert_equal(rc, CC_SUCCESS, "Process initialization failed")
      call assert_true(process%is_ready(), "Process not ready after init")

      call testing_end_test()
   end subroutine test_process_init

   subroutine test_process_run()
      call testing_start_test("Process Calculation")

      ! Run process
      call process%run(container, rc)
      call assert_equal(rc, CC_SUCCESS, "Process run failed")

      ! Verify results
      ! Add specific tests for your process outputs

      call testing_end_test()
   end subroutine test_process_run

   subroutine test_process_finalize()
      call testing_start_test("Process Finalization")

      call process%finalize(rc)
      call assert_equal(rc, CC_SUCCESS, "Process finalization failed")

      call testing_end_test()
   end subroutine test_process_finalize

end program test_newprocess
```

## Testing and Validation

### Unit Testing

```bash
# Build and run unit tests
cd build
make test_newprocess
./tests/test_newprocess
```

### Integration Testing

```bash
# Test with full model
make catchem_test
./tests/catchem_test --config test_newprocess_config.yml
```

### Performance Testing

```bash
# Profile performance
make catchem_profile
./profile/catchem_profile --config newprocess_profile.yml
```

## Integration Checklist

Before integrating your process:

- [x] All unit tests pass
- [x] Integration tests pass
- [x] Documentation is complete
- [x] Code follows style guidelines
- [x] Error handling is robust
- [x] Configuration is validated
- [x] Diagnostics are working
- [x] Memory leaks are checked
- [x] Performance is acceptable

## Common Pitfalls

1. **Memory leaks**: Always deallocate in finalize()
2. **Uninitialized pointers**: Check pointer association before use
3. **Missing error handling**: Always check return codes
4. **Configuration errors**: Validate all user inputs
5. **Thread safety**: Avoid global variables in processes
6. **Performance issues**: Profile before optimizing

## Best Practices Summary

1. **Start simple**: Implement basic functionality first
2. **Test early and often**: Write tests as you develop
3. **Document everything**: Include Doxygen comments
4. **Handle errors gracefully**: Use the error manager
5. **Follow conventions**: Match existing code style
6. **Validate inputs**: Check all user-provided data
7. **Profile performance**: Ensure acceptable speed

For more examples, see the existing processes in `src/process/` and refer to the [Process Templates](templates.md) guide.

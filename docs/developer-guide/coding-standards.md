# Coding Standards

CATChem coding standards ensure consistency, maintainability, and performance across the codebase.

## Fortran Standards {#fortran}

### Language Version
- **Target**: Fortran 2008 with selected 2018 features
- **Compiler Support**: gfortran 9+, Intel Fortran 2019+
- **Portability**: Code must compile on major HPC systems

### Modern Fortran Features
```fortran
! Use modern Fortran constructs
use precision_mod, only: fp
use ieee_arithmetic, only: ieee_is_finite

! Prefer allocatable over pointers
real(real64), allocatable :: data(:,:)

! Use derived types with type-bound procedures
type :: ProcessType
contains
  procedure :: init => Process_init
  procedure :: run => Process_run
end type
```

## Naming Conventions

### Modules
```fortran
! Module names: PascalCase with _Mod suffix
module SettlingProcess_Mod
module DiagnosticManager_Mod
module StateContainer_Mod
```

### Types
```fortran
! Type names: PascalCase with Type suffix
type :: SettlingProcessType
type :: StateContainerType
type :: ConfigDataType
```

### Variables and Procedures
```fortran
! Variables: snake_case
integer :: num_species
real(fp) :: settling_velocity
character(len=:), allocatable :: config_file

! Procedures: snake_case
subroutine compute_settling_velocity()
function get_slip_correction() result(correction)
```

### Constants
```fortran
! Constants: UPPER_CASE
integer, parameter :: MAX_SPECIES = 100
real(fp), parameter :: GRAVITATIONAL_ACCELERATION = 9.80665_fp
character(*), parameter :: DEFAULT_CONFIG_FILE = "catchem.yml"
```

## Code Structure

### Module Organization
```fortran
module MyProcess_Mod
  use precision_mod, only: fp
  use error_mod, only: CC_SUCCESS, CC_FAILURE
  implicit none
  private

  ! Public interface
  public :: MyProcessType

  ! Type definitions
  type :: MyProcessType
    private
    ! Private data members
  contains
    private
    ! Public procedures
    procedure, public :: init => MyProcess_init
    procedure, public :: run => MyProcess_run
    procedure, public :: finalize => MyProcess_finalize
  end type

contains

  ! Implementation

end module
```

### Procedure Structure
```fortran
!> @brief Brief description of the procedure
!>
!> Detailed description of what the procedure does,
!> including any important algorithms or assumptions.
!>
!> @param[in] input_param Description of input parameter
!> @param[in,out] inout_param Description of input/output parameter
!> @param[out] output_param Description of output parameter
!> @param[out] rc Return code (CC_SUCCESS on success)
!>
!> @author Author Name
!> @date Date created
subroutine my_procedure(input_param, inout_param, output_param, rc)
  implicit none

  ! Arguments
  real(fp), intent(in) :: input_param
  real(fp), intent(inout) :: inout_param
  real(fp), intent(out) :: output_param
  integer, intent(out) :: rc

  ! Local variables
  real(fp) :: local_var
  integer :: i

  ! Initialize return code
  rc = CC_SUCCESS

  ! Implementation

  ! Error handling
  if (error_condition) then
    rc = CC_FAILURE
    return
  end if

end subroutine
```

## Documentation Standards

### Doxygen Comments
```fortran
!> @brief Process gravitational settling of particles
!>
!> This module implements gravitational settling using Stokes law
!> with Cunningham slip correction for small particles.
!>
!> Key features:
!> - Temperature-dependent dynamic viscosity
!> - Slip correction for sub-micron particles
!> - CFL-stable time integration
!>
!> @author John Doe
!> @date 2025-01-15
!> @version 1.0
module SettlingProcess_Mod
```

### Inline Comments
```fortran
! Calculate dynamic viscosity using Sutherland's law
mu = mu_ref * (temperature / t_ref)**1.5 * (t_ref + s_temp) / (temperature + s_temp)

! Apply Cunningham slip correction for small particles
if (particle_radius < mean_free_path) then
  slip_correction = 1.0_fp + (mean_free_path / particle_radius) * &
                    (a1 + a2 * exp(-a3 * particle_radius / mean_free_path))
end if
```

### Header Comments
```fortran
!===============================================================================
!> @file settling_process.F90
!> @brief Gravitational settling process implementation
!> @details
!> This file contains the implementation of gravitational settling
!> for atmospheric particles, including sophisticated physics for
!> small particles and numerical stability controls.
!>
!> @author CATChem Development Team
!> @date 2025
!> @copyright Apache 2.0 License
!===============================================================================
```

## Error Handling

### Return Codes
```fortran
! Standard return codes
integer, parameter :: CC_SUCCESS = 0
integer, parameter :: CC_FAILURE = -1
integer, parameter :: CC_INVALID_INPUT = -2
integer, parameter :: CC_MEMORY_ERROR = -3
integer, parameter :: CC_IO_ERROR = -4

! Always check return codes
call some_procedure(data, rc)
if (rc /= CC_SUCCESS) then
  call error_handler%log_error("Procedure failed", rc)
  return
end if
```

### Error Context
```fortran
! Provide error context
subroutine my_procedure(data, rc)
  ! ...

  call sub_procedure(data, rc)
  if (rc /= CC_SUCCESS) then
    call error_handler%push_context("my_procedure", "sub_procedure failed")
    return
  end if

end subroutine
```

## Performance Guidelines {#performance}

### Memory Management
```fortran
! Prefer allocatable arrays
real(fp), allocatable :: work_array(:,:)

! Allocate once, reuse when possible
if (.not. allocated(work_array)) then
  allocate(work_array(nx, ny))
end if

! Clean up when done
if (allocated(work_array)) deallocate(work_array)
```

### Loop Optimization
```fortran
! Inner loops should be over the fastest-varying dimension (first index in Fortran)
do j = 1, ny
  do i = 1, nx  ! Inner loop over first index
    array(i, j) = compute_value(i, j)
  end do
end do

! Use compiler directives for vectorization
!DIR$ SIMD
do i = 1, n
  result(i) = expensive_function(input(i))
end do
```

### Precision and Constants
```fortran
! Use precision module
use precision_mod, only: fp

! All real literals should have kind specifier
real(fp), parameter :: PI = 3.141592653589793_fp
real(fp) :: value = 1.0_fp

! Use named constants instead of magic numbers
real(fp), parameter :: BOLTZMANN_CONSTANT = 1.380649e-23_fp
```

## Testing Standards {#testing}

### Unit Test Structure
```fortran
program test_my_module
  use testing_mod
  use MyModule_Mod
  implicit none

  call test_suite_begin("MyModule Tests")

  call test_initialization()
  call test_computation()
  call test_error_handling()

  call test_suite_end()

contains

  subroutine test_initialization()
    call test_begin("Initialization test")

    ! Test implementation

    call test_end()
  end subroutine

end program
```

### Test Coverage
- Every public procedure must have unit tests
- Test both normal operation and error conditions
- Include boundary value testing
- Test with realistic data ranges

## Code Review Guidelines

### Pre-Review Checklist
- [x] Code follows naming conventions
- [x] All procedures are documented
- [x] Unit tests are included
- [x] No compiler warnings
- [x] Performance considerations addressed

### Review Criteria
- **Correctness**: Does the code do what it's supposed to?
- **Clarity**: Is the code easy to understand?
- **Performance**: Are there obvious performance issues?
- **Maintainability**: Will this be easy to modify later?
- **Testing**: Are there adequate tests?

## Build System

### CMake Standards
```cmake
# Use modern CMake (3.15+)
cmake_minimum_required(VERSION 3.15)

# Set clear variable names
set(CATCHEM_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/src)
set(CATCHEM_BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR})

# Use target-based approach
add_library(catchem_core ${CORE_SOURCES})
target_include_directories(catchem_core PUBLIC ${INCLUDE_DIRS})
target_link_libraries(catchem_core PUBLIC ${DEPENDENCIES})
```

### Compiler Flags
```cmake
# Debug flags
set(CMAKE_Fortran_FLAGS_DEBUG "-g -O0 -fbounds-check -fcheck=all")

# Release flags
set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -funroll-loops -march=native")

# Warning flags
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall -Wextra")
```

## Version Control

### Commit Messages
```
feat: add Stokes settling scheme with slip correction

- Implement temperature-dependent dynamic viscosity
- Add Cunningham slip correction for small particles
- Include CFL-stable time integration
- Add comprehensive unit tests

Closes #123
```

### Branch Naming
- `feature/settling-process` - New features
- `bugfix/memory-leak` - Bug fixes
- `hotfix/critical-issue` - Critical fixes
- `refactor/state-container` - Code refactoring

### Pull Request Standards
- Clear description of changes
- Reference related issues
- Include test results
- Update documentation as needed

---

*These standards ensure CATChem maintains high code quality and developer productivity. All contributions must follow these guidelines.*

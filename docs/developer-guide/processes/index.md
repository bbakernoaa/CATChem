# Process Development

This section covers developing new processes and schemes for CATChem. See the **[CATChem User Guide](../../user-guide/index.md#process-documentation)** for a description of all processes available in CATChem.

!!! note "The process generator code is currently being developed. Please check back frequently for updates to the documentation on how to use the process generator. Further updates on this section will be coming soon!"

## Quick Links

- **[Architecture Overview](architecture.md)** - Understanding process design patterns
- **[Generator System Overview](generator-system-overview.md)** - Complete system documentation
- **[Process Generator Tutorial](generator-tutorial.md)** - Complete guide to using the automated process generator
- **[Generator Demo](generator-demo.md)** - Hands-on example with a real process
- **[Creating Custom Processes](creating.md)** - Manual process development
- **[Templates and Patterns](templates.md)** - Code templates and best practices
- **[Testing Processes](testing.md)** - Testing strategies and frameworks

## What's New

## Overview

CATChem processes are modular components that implement specific atmospheric transport, chemical, emission, or loss schemes. Each process follows a standardized interface and lifecycle.

## Process Architecture

All processes inherit from the base `ProcessInterface` and implement:

```fortran
module MyProcess_Mod
  use ProcessInterface_Mod
  implicit none

  type, extends(ProcessInterface) :: MyProcessType
  contains
    procedure :: init => MyProcess_init
    procedure :: run => MyProcess_run
    procedure :: finalize => MyProcess_finalize
  end type

contains

  subroutine MyProcess_init(this, container, rc)
    ! Initialize process
  end subroutine

  subroutine MyProcess_run(this, container, rc)
    ! Execute process for one time step
  end subroutine

  subroutine MyProcess_finalize(this, rc)
    ! Clean up resources
  end subroutine

end module
```

## Creating New Processes

### 1. Process Template

Use the process generator to create a new process:

```bash
# Generate process template
python util/catchem_generate_process.py \
  --process-name "MyProcess" \
  --category "transport" \
  --output-dir src/process/myprocess/
```

### 2. Implement Required Methods

```fortran
! Required interface methods
subroutine init(this, container, rc)
subroutine run(this, container, rc)
subroutine finalize(this, rc)

! Optional interface methods
subroutine set_parameter(this, name, value, rc)
subroutine get_diagnostic(this, name, field, rc)
```

### 3. Process Registration

Register your process in `ProcessRegistry_Mod`:

```fortran
! Add to process registry
call registry%register_process("MyProcess", create_MyProcess)
```

## Scheme Development

### Scheme Architecture

Schemes implement specific algorithms within a process:

```fortran
module MyScheme_Mod
  use SchemeInterface_Mod
  implicit none

  type, extends(SchemeInterface) :: MySchemeType
  contains
    procedure :: compute => MyScheme_compute
  end type

contains

  subroutine MyScheme_compute(this, input, output, rc)
    ! Implement scheme physics
  end subroutine

end module
```

### Scheme Generation

```bash
# Generate scheme template
python util/catchem_generate_process.py \
  --scheme-name "MyScheme" \
  --process-name "MyProcess" \
  --output-dir src/process/myprocess/schemes/
```

## Testing Processes

### Unit Tests

Create comprehensive unit tests:

```fortran
program test_myprocess
  use MyProcess_Mod
  use testing_mod
  implicit none

  call test_init()
  call test_run()
  call test_finalize()

contains

  subroutine test_init()
    ! Test initialization
  end subroutine

  subroutine test_run()
    ! Test execution
  end subroutine

end program
```

### Integration Tests

Test process integration:

```bash
# Run process tests
cd build
ctest -R test_myprocess -V
```

## Documentation

Document your process:

```fortran
!> @brief My atmospheric process
!>
!> This process implements [chemical description].
!>
!> @author Your Name
!> @date 2025
!>
!> @param[in,out] container State container
!> @param[out] rc Return code
module MyProcess_Mod
```

## See Also

- [Process Architecture](architecture.md)
- [Testing Processes](testing.md)
- [Process Templates](templates.md)

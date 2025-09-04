# Error Handling Developer Guide

CATChem implements a comprehensive error handling system that provides robust error management, graceful degradation, and detailed error reporting. Everything you need to know about the error handling system to develop, modify, and extend CATChem is described in this guide.

## Overview

The error handling system provides:

- **Structured Error Management** - Hierarchical error codes and messages
- **Graceful Degradation** - Continue operation when possible
- **Detailed Error Reporting** - Context-aware error messages with stack traces
- **Recovery Mechanisms** - Automatic error recovery where feasible
- **Integration with Logging** - Seamless integration with the logging system

## Error Manager Architecture

### Core Error Manager

```fortran
module ErrorManager_Mod
  use precision_mod
  implicit none
  private

  ! Error severity levels
  integer, parameter, public :: ERROR_LEVEL_INFO    = 1
  integer, parameter, public :: ERROR_LEVEL_WARNING = 2
  integer, parameter, public :: ERROR_LEVEL_ERROR   = 3
  integer, parameter, public :: ERROR_LEVEL_FATAL   = 4

  ! Error codes
  integer, parameter, public :: ERROR_SUCCESS           = 0
  integer, parameter, public :: ERROR_INVALID_INPUT     = 1001
  integer, parameter, public :: ERROR_FILE_IO          = 1002
  integer, parameter, public :: ERROR_MEMORY_ALLOC     = 1003
  integer, parameter, public :: ERROR_CONVERGENCE      = 1004
  integer, parameter, public :: ERROR_CONFIGURATION    = 1005
  integer, parameter, public :: ERROR_PHYSICS_INVALID  = 1006
  integer, parameter, public :: ERROR_MASS_CONSERVATION = 1007

  type :: ErrorContextType
    character(len=64) :: module_name
    character(len=64) :: procedure_name
    integer :: line_number
    character(len=256) :: additional_info
  end type ErrorContextType

  type, public :: ErrorManagerType
    private
    character(len=1024) :: error_message
    integer :: error_code = ERROR_SUCCESS
    integer :: error_level = ERROR_LEVEL_INFO
    type(ErrorContextType) :: context
    logical :: has_error = .false.

    ! Error history for debugging
    character(len=256) :: error_stack(100)
    integer :: stack_depth = 0

  contains
    procedure, public :: log_error => error_manager_log_error
    procedure, public :: log_warning => error_manager_log_warning
    procedure, public :: log_info => error_manager_log_info
    procedure, public :: has_error => error_manager_has_error
    procedure, public :: get_error_message => error_manager_get_message
    procedure, public :: get_error_code => error_manager_get_code
    procedure, public :: clear_error => error_manager_clear
    procedure, public :: print_stack_trace => error_manager_print_stack

    ! Error recovery methods
    procedure, public :: attempt_recovery => error_manager_attempt_recovery
    procedure, public :: set_recovery_action => error_manager_set_recovery
  end type ErrorManagerType

  ! Global error manager instance
  type(ErrorManagerType), public :: global_error_manager

contains

  subroutine error_manager_log_error(this, message, error_code, context)
    class(ErrorManagerType), intent(inout) :: this
    character(len=*), intent(in) :: message
    integer, intent(in), optional :: error_code
    type(ErrorContextType), intent(in), optional :: context

    this%has_error = .true.
    this%error_level = ERROR_LEVEL_ERROR
    this%error_message = message

    if (present(error_code)) then
      this%error_code = error_code
    else
      this%error_code = ERROR_INVALID_INPUT
    end if

    if (present(context)) then
      this%context = context
    end if

    ! Add to error stack
    call add_to_error_stack(this, message)

    ! Log to system logger
    call log_to_system('ERROR', message, this%error_code)
  end subroutine error_manager_log_error

end module ErrorManager_Mod
```

## Error Handling Patterns

### Function-Level Error Handling

```fortran
subroutine safe_array_allocation(array, dimensions, rc)
  real(fp), allocatable, intent(out) :: array(:,:,:)
  integer, intent(in) :: dimensions(3)
  integer, intent(out) :: rc

  type(ErrorContextType) :: context
  integer :: alloc_stat

  rc = ERROR_SUCCESS

  ! Set error context
  context%module_name = 'UtilityModule'
  context%procedure_name = 'safe_array_allocation'
  context%line_number = __LINE__
  write(context%additional_info, '(A,3I0)') 'Array dimensions: ', dimensions

  ! Validate input
  if (any(dimensions <= 0)) then
    call global_error_manager%log_error( &
      'Invalid array dimensions: all dimensions must be positive', &
      ERROR_INVALID_INPUT, context)
    rc = ERROR_INVALID_INPUT
    return
  end if

  ! Check memory requirements
  if (product(int(dimensions,8)) > max_array_size) then
    call global_error_manager%log_error( &
      'Requested array size exceeds memory limits', &
      ERROR_MEMORY_ALLOC, context)
    rc = ERROR_MEMORY_ALLOC
    return
  end if

  ! Attempt allocation
  allocate(array(dimensions(1), dimensions(2), dimensions(3)), &
           stat=alloc_stat)

  if (alloc_stat /= 0) then
    write(context%additional_info, '(A,I0)') &
      'Allocation failed with stat: ', alloc_stat
    call global_error_manager%log_error( &
      'Memory allocation failed', ERROR_MEMORY_ALLOC, context)
    rc = ERROR_MEMORY_ALLOC
    return
  end if

  ! Success
  call global_error_manager%log_info('Array allocated successfully')
end subroutine safe_array_allocation
```

### Process-Level Error Handling

```fortran
subroutine chemistry_process_run(this, container, rc)
  class(ChemistryProcessType), intent(inout) :: this
  type(StateContainerType), intent(inout) :: container
  integer, intent(out) :: rc

  type(ErrorContextType) :: context
  real(fp) :: mass_before, mass_after, mass_error
  integer :: solver_rc, validation_rc

  rc = ERROR_SUCCESS
  context%module_name = 'ChemistryProcess'
  context%procedure_name = 'chemistry_process_run'

  ! Pre-process validation
  call validate_chemistry_inputs(container, validation_rc)
  if (validation_rc /= ERROR_SUCCESS) then
    call global_error_manager%log_error( &
      'Chemistry input validation failed', validation_rc, context)
    rc = validation_rc
    return
  end if

  ! Store initial mass for conservation check
  mass_before = calculate_total_mass(container)

  ! Run chemistry solver
  call run_chemistry_solver(this, container, solver_rc)

  if (solver_rc /= ERROR_SUCCESS) then
    select case (solver_rc)
    case (ERROR_CONVERGENCE)
      ! Attempt recovery with reduced timestep
      call global_error_manager%log_warning( &
        'Chemistry solver convergence failure, attempting recovery')
      call attempt_chemistry_recovery(this, container, solver_rc)

      if (solver_rc /= ERROR_SUCCESS) then
        call global_error_manager%log_error( &
          'Chemistry solver failed after recovery attempt', solver_rc, context)
        rc = solver_rc
        return
      end if

    case default
      call global_error_manager%log_error( &
        'Chemistry solver failed', solver_rc, context)
      rc = solver_rc
      return
    end select
  end if

  ! Post-process validation
  mass_after = calculate_total_mass(container)
  mass_error = abs(mass_after - mass_before) / mass_before

  if (mass_error > mass_conservation_tolerance) then
    write(context%additional_info, '(A,E12.4)') &
      'Mass conservation error: ', mass_error

    if (mass_error > critical_mass_error_threshold) then
      call global_error_manager%log_error( &
        'Critical mass conservation violation', ERROR_MASS_CONSERVATION, context)
      rc = ERROR_MASS_CONSERVATION
      return
    else
      call global_error_manager%log_warning( &
        'Mass conservation warning: ' // trim(context%additional_info))
    end if
  end if
end subroutine chemistry_process_run
```

## Error Recovery Mechanisms

### Automatic Recovery

```fortran
subroutine attempt_chemistry_recovery(this, container, rc)
  class(ChemistryProcessType), intent(inout) :: this
  type(StateContainerType), intent(inout) :: container
  integer, intent(out) :: rc

  real(fp) :: original_timestep, recovery_timestep
  integer :: substeps, i, substep_rc

  rc = ERROR_SUCCESS

  ! Store original timestep
  original_timestep = this%timestep

  ! Try with reduced timestep
  substeps = 4
  recovery_timestep = original_timestep / real(substeps, fp)

  call global_error_manager%log_info( &
    'Attempting chemistry recovery with sub-stepping')

  do i = 1, substeps
    this%timestep = recovery_timestep
    call run_chemistry_solver(this, container, substep_rc)

    if (substep_rc /= ERROR_SUCCESS) then
      ! Recovery failed
      this%timestep = original_timestep
      rc = substep_rc
      call global_error_manager%log_error( &
        'Chemistry recovery failed at substep ' // char(i + 48))
      return
    end if
  end do

  ! Restore original timestep
  this%timestep = original_timestep

  call global_error_manager%log_info( &
    'Chemistry recovery successful with sub-stepping')
end subroutine attempt_chemistry_recovery
```

### Fallback Algorithms

```fortran
subroutine settling_with_fallback(this, container, rc)
  class(SettlingProcessType), intent(inout) :: this
  type(StateContainerType), intent(inout) :: container
  integer, intent(out) :: rc

  character(len=32) :: primary_scheme, fallback_scheme
  integer :: primary_rc, fallback_rc

  rc = ERROR_SUCCESS
  primary_scheme = this%selected_scheme

  ! Try primary scheme
  call run_settling_scheme(this, container, primary_scheme, primary_rc)

  if (primary_rc == ERROR_SUCCESS) then
    return  ! Success with primary scheme
  end if

  ! Primary scheme failed, try fallback
  call global_error_manager%log_warning( &
    'Primary settling scheme failed, trying fallback: ' // primary_scheme)

  select case (trim(primary_scheme))
  case ('intermediate_reynolds')
    fallback_scheme = 'stokes'
  case ('stokes')
    fallback_scheme = 'constant_velocity'
  case default
    fallback_scheme = 'stokes'
  end select

  call run_settling_scheme(this, container, fallback_scheme, fallback_rc)

  if (fallback_rc == ERROR_SUCCESS) then
    call global_error_manager%log_info( &
      'Fallback settling scheme successful: ' // fallback_scheme)
    return
  end if

  ! Both schemes failed
  call global_error_manager%log_error( &
    'All settling schemes failed', primary_rc)
  rc = primary_rc
end subroutine settling_with_fallback
```

## Validation and Assertion Framework

### Input Validation

```fortran
module ValidationUtils_Mod
  use ErrorManager_Mod
  implicit none

contains

  subroutine validate_positive(value, name, rc)
    real(fp), intent(in) :: value
    character(len=*), intent(in) :: name
    integer, intent(out) :: rc

    type(ErrorContextType) :: context

    rc = ERROR_SUCCESS

    if (value <= 0.0) then
      context%module_name = 'ValidationUtils'
      context%procedure_name = 'validate_positive'
      write(context%additional_info, '(A,E12.4)') &
        trim(name) // ' must be positive, got: ', value

      call global_error_manager%log_error( &
        'Validation failed: ' // trim(name) // ' must be positive', &
        ERROR_INVALID_INPUT, context)
      rc = ERROR_INVALID_INPUT
    end if
  end subroutine validate_positive

  subroutine validate_range(value, min_val, max_val, name, rc)
    real(fp), intent(in) :: value, min_val, max_val
    character(len=*), intent(in) :: name
    integer, intent(out) :: rc

    type(ErrorContextType) :: context

    rc = ERROR_SUCCESS

    if (value < min_val .or. value > max_val) then
      context%module_name = 'ValidationUtils'
      context%procedure_name = 'validate_range'
      write(context%additional_info, '(A,3E12.4)') &
        trim(name) // ' out of range: ', value, min_val, max_val

      call global_error_manager%log_error( &
        'Validation failed: ' // trim(name) // ' out of valid range', &
        ERROR_INVALID_INPUT, context)
      rc = ERROR_INVALID_INPUT
    end if
  end subroutine validate_range

  subroutine assert_mass_conservation(mass_before, mass_after, tolerance, rc)
    real(fp), intent(in) :: mass_before, mass_after, tolerance
    integer, intent(out) :: rc

    real(fp) :: relative_error
    type(ErrorContextType) :: context

    rc = ERROR_SUCCESS
    relative_error = abs(mass_after - mass_before) / mass_before

    if (relative_error > tolerance) then
      context%module_name = 'ValidationUtils'
      context%procedure_name = 'assert_mass_conservation'
      write(context%additional_info, '(A,E12.4,A,E12.4)') &
        'Mass conservation error: ', relative_error, ' > ', tolerance

      call global_error_manager%log_error( &
        'Mass conservation assertion failed', &
        ERROR_MASS_CONSERVATION, context)
      rc = ERROR_MASS_CONSERVATION
    end if
  end subroutine assert_mass_conservation

end module ValidationUtils_Mod
```

## Error Reporting and Debugging

### Detailed Error Messages

```fortran
subroutine generate_detailed_error_report(error_mgr)
  type(ErrorManagerType), intent(in) :: error_mgr

  character(len=1024) :: report
  integer :: unit

  if (.not. error_mgr%has_error()) return

  ! Generate comprehensive error report
  write(report, '(A)') '=== CATChem Error Report ==='
  write(report, '(A,A)') report, new_line('A') // 'Error Code: '
  write(report, '(A,I0)') trim(report), error_mgr%get_error_code()
  write(report, '(A,A,A)') trim(report), new_line('A') // 'Message: ', &
    trim(error_mgr%get_error_message())
  write(report, '(A,A,A)') trim(report), new_line('A') // 'Module: ', &
    trim(error_mgr%context%module_name)
  write(report, '(A,A,A)') trim(report), new_line('A') // 'Procedure: ', &
    trim(error_mgr%context%procedure_name)
  write(report, '(A,A,I0)') trim(report), new_line('A') // 'Line: ', &
    error_mgr%context%line_number
  write(report, '(A,A,A)') trim(report), new_line('A') // 'Additional Info: ', &
    trim(error_mgr%context%additional_info)

  ! Write to error log file
  open(newunit=unit, file='catchem_error_report.log', position='append')
  write(unit, '(A)') trim(report)
  write(unit, '(A)') '=== Stack Trace ==='
  call error_mgr%print_stack_trace(unit)
  write(unit, '(A)') '==================='
  write(unit, '(A)') ''
  close(unit)

  ! Also print to console in debug mode
  if (debug_mode) then
    print *, trim(report)
    call error_mgr%print_stack_trace(output_unit)
  end if
end subroutine generate_detailed_error_report
```

## Integration with Host Models

### Host Model Error Interface

```fortran
! Interface for host model error handling
subroutine catchem_get_error_status(error_code, error_message, message_len)
  integer, intent(out) :: error_code
  character(len=*), intent(out) :: error_message
  integer, intent(in) :: message_len

  if (global_error_manager%has_error()) then
    error_code = global_error_manager%get_error_code()
    error_message = global_error_manager%get_error_message()
  else
    error_code = ERROR_SUCCESS
    error_message = 'No error'
  end if
end subroutine catchem_get_error_status

subroutine catchem_clear_error_status()
  call global_error_manager%clear_error()
end subroutine catchem_clear_error_status
```

## Best Practices

### 1. Consistent Error Handling
```fortran
! Always check return codes
call some_procedure(input_data, rc)
if (rc /= ERROR_SUCCESS) then
  ! Handle error appropriately
  return
end if
```

### 2. Meaningful Error Messages
```fortran
! Good: Specific, actionable error message
call error_mgr%log_error( &
  'Temperature out of range: ' // trim(temp_str) // ' K. ' // &
  'Valid range is 200-350 K for this process.')

! Avoid: Vague error message
call error_mgr%log_error('Invalid temperature')
```

### 3. Error Context
```fortran
! Always provide context for debugging
context%module_name = 'ChemistryProcess'
context%procedure_name = 'run_chemistry'
context%line_number = __LINE__
call error_mgr%log_error('Solver convergence failed', ERROR_CONVERGENCE, context)
```

This error handling system provides robust error management capabilities that help maintain system stability and provide clear debugging information for developers.

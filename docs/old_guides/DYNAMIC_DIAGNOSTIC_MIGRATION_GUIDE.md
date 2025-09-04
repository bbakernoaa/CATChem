# Dynamic Diagnostic System Migration Guide

## Overview

This guide provides instructions for migrating from the static `diagstate_mod` to the new dynamic diagnostic system that integrates with the CATChem framework (StateContainer, ProcessManager, ConfigManager, ErrorManager).

## System Architecture

The new diagnostic system consists of several key components:

### Core Components

1. **DiagnosticInterface_Mod**: Core data types and interfaces
   - `DiagnosticDataType`: Union-like storage for different data types
   - `DiagnosticFieldType`: Individual diagnostic field with metadata
   - `DiagnosticRegistryType`: Per-process collection of diagnostics

2. **DiagnosticManager_Mod**: Central management system
   - `DiagnosticManagerType`: Manages all process diagnostics
   - Integrates with StateContainer, ErrorManager
   - Handles diagnostic collection and output

3. **ProcessInterface Enhancements**:
   - Added `register_diagnostics()` method
   - Added `update_diagnostics()` method
   - Added `get_diagnostic_registry()` method

4. **StateContainer Integration**:
   - Added `DiagnosticManagerType` to StateContainer
   - Added accessor methods for diagnostic manager
   - Automatic initialization during container setup

## Migration Steps

### 1. Update Process Initialization

**Before (using static diagstate):**
```fortran
subroutine dust_process_init(this, container, rc)
   ! Initialize process...
   this%is_initialized = .true.
end subroutine
```

**After (using dynamic diagnostics):**
```fortran
subroutine dust_process_init(this, container, rc)
   ! Initialize process...
   this%is_initialized = .true.

   ! Register diagnostics
   call this%register_diagnostics(container, rc)
   if (rc /= CC_SUCCESS) then
      call error_mgr%report_error(ERROR_INITIALIZATION, 'Failed to register diagnostics', rc)
      return
   end if
end subroutine
```

### 2. Implement Diagnostic Registration

Create a `register_diagnostics` method for each process:

```fortran
subroutine dust_process_register_diagnostics(this, container, rc)
   use DiagnosticInterface_Mod, only: DiagnosticFieldType, DIAG_REAL_2D

   type(DiagnosticFieldType) :: diag_field
   type(DiagnosticRegistryType), pointer :: diag_registry

   ! Call parent to register process
   call this%ProcessInterface%register_diagnostics(container, rc)
   if (rc /= CC_SUCCESS) return

   ! Get diagnostic registry
   call this%get_diagnostic_registry(container, diag_registry, rc)
   if (rc /= CC_SUCCESS) return

   ! Register process-specific diagnostics
   call diag_field%create('total_dust_flux', &
                          'Total dust emission flux', &
                          'kg m-2 s-1', DIAG_REAL_2D, this%name, rc)
   if (rc == CC_SUCCESS) then
      call diag_registry%register_field(diag_field, rc)
   endif
end subroutine
```

### 3. Implement Diagnostic Updates

Create an `update_diagnostics` method for each process:

```fortran
subroutine dust_process_update_diagnostics(this, container, rc)
   use DiagnosticInterface_Mod, only: DiagnosticRegistryType, DiagnosticFieldType

   type(DiagnosticRegistryType), pointer :: diag_registry
   type(DiagnosticFieldType), pointer :: diag_field

   ! Get diagnostic registry
   call this%get_diagnostic_registry(container, diag_registry, rc)
   if (rc /= CC_SUCCESS) return

   ! Update diagnostic values
   diag_field => diag_registry%get_field_ptr('total_dust_flux', rc)
   if (rc == CC_SUCCESS .and. associated(diag_field)) then
      call diag_field%update_data(array_2d=this%dust_flux_2d)
   endif
end subroutine
```

### 4. Update Process Run Methods

**Before:**
```fortran
subroutine dust_process_run(this, container, rc)
   ! Run dust calculations...

   ! Update static diagnostic state
   call update_diagstate_manually(container, this%dust_flux)
end subroutine
```

**After:**
```fortran
subroutine dust_process_run(this, container, rc)
   ! Run dust calculations...

   ! Update diagnostics using new system
   call this%update_diagnostics(container, rc)
   if (rc /= CC_SUCCESS) then
      call error_mgr%report_error(ERROR_STATE_UPDATE, 'Failed to update diagnostics', rc)
      return
   end if
end subroutine
```

### 5. Update StateContainer Usage

**Before:**
```fortran
type(StateContainerType) :: container
call container%init('my_container', rc)
! Diagnostics handled separately
```

**After:**
```fortran
type(StateContainerType) :: container
call container%init('my_container', rc)

! Initialize diagnostic manager
call container%init_diagnostic_manager(rc)
if (rc /= CC_SUCCESS) then
   ! Handle error
end if
```

### 6. Update ProcessManager Integration

The ProcessManager automatically handles diagnostic registration if processes implement the new interface:

```fortran
type(ProcessManagerType) :: proc_mgr
type(DiagnosticManagerType), pointer :: diag_mgr

call proc_mgr%init(rc)
call proc_mgr%add_process('dust', 'afwa', container, rc)

! Diagnostic manager collects all process diagnostics
diag_mgr => container%get_diagnostic_manager_ptr()
call diag_mgr%collect_all_diagnostics(container, rc)
call diag_mgr%write_output(container, rc)
```

## Framework Integration

### ErrorManager Integration

All diagnostic operations use the existing ErrorManager for consistent error handling:

```fortran
error_mgr => container%get_error_manager()
call error_mgr%push_context('register_diagnostics', 'Registering process diagnostics')

! Diagnostic operations...

if (rc /= CC_SUCCESS) then
   call error_mgr%report_error(rc, 'Diagnostic operation failed')
   call error_mgr%pop_context()
   return
endif

call error_mgr%pop_context()
```

### ConfigManager Integration

Diagnostic configuration can be managed through the existing ConfigManager:

```fortran
! In configuration file (YAML)
diagnostics:
  output_frequency: 1    # timesteps
  output_format: netcdf
  enabled_processes:
    - dust
    - seasalt
    - drydep
  fields:
    dust:
      total_dust_flux:
        enabled: true
        frequency: hourly
```

### StateContainer Integration

The diagnostic manager is fully integrated into the StateContainer lifecycle:

```fortran
! Automatic initialization
call container%init('my_container', rc)
call container%init_diagnostic_manager(rc)

! Automatic cleanup
call container%finalize(rc)  ! Cleans up diagnostic manager too

! Access patterns
diag_mgr => container%get_diagnostic_manager_ptr()
call diag_mgr%collect_all_diagnostics(container, rc)
```

## Backward Compatibility

### Dual Mode Operation

During migration, both systems can operate simultaneously:

```fortran
subroutine update_diagnostic_state(this, container, rc)
   ! Update new dynamic diagnostics
   call this%update_diagnostics(container, rc)

   ! Also update legacy diagnostics for compatibility
   call this%update_legacy_diagnostic_state(container, rc)
end subroutine
```

### Migration Timeline

1. **Phase 1**: Implement new diagnostic system alongside existing
2. **Phase 2**: Migrate processes one by one to new system
3. **Phase 3**: Deprecate and remove static diagstate_mod
4. **Phase 4**: Clean up legacy compatibility code

## Benefits of New System

### 1. Framework Integration
- Uses existing ErrorManager for consistent error handling
- Integrates with StateContainer for dependency injection
- Works with ProcessManager for automatic process management
- Leverages ConfigManager for configuration management

### 2. Dynamic Registration
- Processes register their own diagnostics at runtime
- No need to modify central diagnostic state for new processes
- Supports process-specific diagnostic metadata

### 3. Type Safety
- Strong typing for diagnostic data types
- Compile-time checking of diagnostic field access
- Automatic memory management with finalizers

### 4. Flexibility
- Support for multiple data types (scalar, 1D, 2D, 3D arrays)
- Configurable output frequencies per diagnostic
- Runtime enable/disable of individual diagnostics

### 5. Maintainability
- Clear separation of concerns
- Process-specific diagnostic code stays with process
- Easier to add new processes and diagnostics

## Examples

See the updated `DustProcess_Mod.F90` for a complete example of:
- Diagnostic registration in `dust_process_register_diagnostics`
- Diagnostic updates in `dust_process_update_diagnostics`
- Integration with process lifecycle

## Troubleshooting

### Common Issues

1. **Diagnostic registration fails**
   - Ensure diagnostic manager is initialized before process init
   - Check that process name is unique
   - Verify diagnostic field names are unique per process

2. **Diagnostic updates fail**
   - Ensure diagnostic fields are properly initialized with data storage
   - Check data type consistency between registration and update
   - Verify array dimensions match

3. **Memory issues**
   - Use finalizers to ensure proper cleanup
   - Check for memory leaks in diagnostic data storage
   - Monitor memory usage with `container%get_memory_usage()`

### Debug Mode

Enable verbose error reporting:
```fortran
call error_mgr%init(verbose=.true.)
call diag_mgr%print_summary()  ! Shows all registered diagnostics
```

This migration leverages the existing CATChem framework patterns and provides a solid foundation for future diagnostic system enhancements.

# CATChem API Improvements: Run Phases and Configuration-Driven Initialization

## Overview

This document describes the key improvements made to the CATChem API to support:

1. **Multiple run phases** for flexible process execution
2. **Configuration-driven initialization** where `nspecies` comes from config files
3. **Enhanced process manager** configuration and control
4. **Proper initialization sequence** following the order: main config → species YAML → emission YAML

## Key Changes

### 1. Configuration-Driven Initialization

**Problem**: Previously, `nspecies` had to be set directly in the API, which was inflexible and error-prone.

**Solution**: `nspecies` is now read from configuration files during initialization.

```fortran
config%nx = 144; config%ny = 91; config%nz = 72
! nspecies is NOT set - read from species config file
config%config_file = 'CATChem_config.yml'
config%species_file = 'CATChem_species.yml'
call catchem%init_from_config_files(config, rc)
```

**Initialization sequence**:
1. **Main config file** (`CATChem_config.yml`) - grid settings, process toggles, general configuration
2. **Species file** (`CATChem_species.yml`) - species definitions, determines `nspecies`
3. **Emission file** (`emission_config.yml`) - emission source configuration (optional)

### 2. Multiple Run Phases Support

**Problem**: Previous API only supported running all processes together in a single phase.

**Solution**: Added multi-phase execution with configurable phase sequences.

```fortran
! Configure run phases
config%enable_run_phases = .true.
config%run_phase_names = ['emissions', 'chemistry', 'transport', 'deposition']
call catchem%setup_run_phases(config%run_phase_names, rc)

! Run individual phases
call catchem%run_phase('emissions', rc)
call catchem%run_phase('chemistry', rc)

! Run all phases in sequence
call catchem%run_all_phases(rc)

! Traditional single-phase execution still supported
call catchem%run_timestep(input_data, output_data, rc)
```

**Benefits**:
- Flexible process orchestration
- Better control over when specific processes run
- Support for complex coupling scenarios
- Easier debugging and testing

### 3. Enhanced Process Manager Configuration

**Problem**: Limited control over process manager behavior and settings.

**Solution**: Added configuration methods for process manager customization.

```fortran
! Configure process manager
call catchem%configure_process_manager(max_processes=100, &
                                      enable_column_batching=.true., rc=rc)

! Set custom process manager (advanced users)
call catchem%set_process_manager(custom_manager, rc)

! Access process manager directly
call catchem%get_process_manager(process_manager)
```

## API Changes Summary

### New Configuration Fields

```fortran
type :: CATChemConfigType
   ! Grid dimensions (unchanged)
   integer :: nx, ny, nz
   ! nspecies removed - now read from config files

   ! Configuration files (NEW)
   character(len=256) :: config_file = ''     ! Main config (read first)
   character(len=256) :: species_file = ''    ! Species config (read second)
   character(len=256) :: emission_file = ''   ! Emission config (read third)

   ! Run phase support (NEW)
   logical :: enable_run_phases = .false.
   character(len=64), allocatable :: run_phase_names(:)

   ! Existing fields...
end type
```

### New Methods

#### Initialization
- `init_from_config_files(config, rc)` - Modern config-driven initialization
- `init(config, rc)` - Legacy initialization (deprecated but maintained for compatibility)

#### Run Phase Management
- `setup_run_phases(phase_names, rc)` - Configure multi-phase execution
- `run_phase(phase_name, rc)` - Run specific phase
- `run_all_phases(rc)` - Run all configured phases
- `get_phase_names(phase_names, rc)` - Get configured phase names
- `get_current_phase()` - Get current phase index

#### Process Manager Configuration
- `configure_process_manager(max_processes, enable_column_batching, rc)` - Configure manager settings
- `set_process_manager(process_manager, rc)` - Set custom manager
- `get_process_manager(process_manager)` - Access manager directly

## Migration Guide

### For Existing Code

1. **Update initialization** (optional but recommended):
   ```fortran
   ! Replace this:
   config%nspecies = 25
   call catchem%init(config, rc)

   ! With this:
   config%config_file = 'CATChem_config.yml'
   config%species_file = 'CATChem_species.yml'
   call catchem%init_from_config_files(config, rc)
   ```

2. **Existing code continues to work** - all legacy methods maintained for compatibility

3. **Add run phases** (optional):
   ```fortran
   config%enable_run_phases = .true.
   config%run_phase_names = ['emissions', 'chemistry', 'transport']
   call catchem%setup_run_phases(config%run_phase_names, rc)
   ```

### For New Code

1. **Use modern initialization** with config files
2. **Consider run phases** for complex applications
3. **Use process manager configuration** for performance tuning

## Configuration File Examples

### Main Config (`CATChem_config.yml`)
```yaml
simulation:
  name: test
  start_date: 20240501 0000
  end_date: 20240501 0100
  species_filename: CATChem_species.yml

grid:
  number_of_levels: 28

process:
  dust:
    activate: true
    scheme_opt: 1
  drydep:
    activate: true
    scheme_opt: 1
```

### Species Config (`CATChem_species.yml`)
```yaml
# Species definitions (determines nspecies automatically)
so2:
  name: so2
  description: Sulfur dioxide
  is_drydep: false

so4:
  name: so4
  description: sulfate
  is_aerosol: true
  is_drydep: true

# Additional species...
```

## Benefits

1. **Flexibility**: Multi-phase execution allows complex process orchestration
2. **Maintainability**: Configuration-driven approach reduces hardcoded values
3. **Compatibility**: Legacy API methods preserved for existing code
4. **Performance**: Better process manager control and column batching options
5. **Modularity**: Clear separation between configuration and code

## Future Enhancements

1. **Dynamic phase configuration** from YAML files
2. **Process dependencies** and automatic phase ordering
3. **Conditional phase execution** based on runtime conditions
4. **Performance profiling** per phase
5. **Phase-specific diagnostics** and output control

The improved API maintains backward compatibility while providing modern, flexible capabilities for complex atmospheric chemistry modeling scenarios.

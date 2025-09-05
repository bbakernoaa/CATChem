# YAML-CPP Integration for CATChem

## Overview

This document summarizes the replacement of QFYAML and FYAML with yaml-cpp in the CATChem configuration management system. The integration provides a modern, high-level Fortran interface to yaml-cpp with generic procedures that can handle all data types seamlessly.

## Changes Made

### 1. New YAML Interface Module (`src/external/yaml_interface/`)

**Files Created:**
- `yaml_interface_mod.F90` - High-level Fortran interface with generic procedures
- `yaml_interface.cpp` - C++ wrapper for yaml-cpp library
- `yaml_interface.h` - C header for the wrapper functions
- `CMakeLists.txt` - Build configuration for the YAML interface

**Key Features:**
- **Generic Interfaces**: `yaml_get`, `yaml_set`, and `yaml_get_array` work with any supported data type
- **Type Safety**: Automatic type conversion between Fortran and C++
- **Memory Management**: Proper cleanup of YAML nodes
- **Error Handling**: Return codes and optional default values
- **Array Support**: Handle arrays of integers, reals, and strings
- **Multiple Precision**: Support for both single and double precision reals

### 2. Updated ConfigManager Module (`src/core/ConfigManager_Mod.F90`)

**Changes Made:**
- Replaced `fyaml_t` with `yaml_node_t` for YAML data storage
- Updated imports to use `yaml_interface_mod` instead of `fyaml`
- Modified initialization and cleanup methods
- Updated configuration loading methods to use yaml-cpp
- Implemented generic getter methods using the new interface

**Key Improvements:**
- Modern YAML parsing with yaml-cpp backend
- Better error handling and validation
- Cleaner, more maintainable code structure
- Type-safe configuration access

### 3. Build System Updates

**CMakeLists.txt Changes:**
- `src/external/CMakeLists.txt` - Added yaml-cpp and yaml_interface subdirectories
- `src/core/CMakeLists.txt` - Updated to link with yaml_interface instead of fyaml
- `examples/CMakeLists.txt` - Added YAML interface example

**Build Order:**
1. yaml-cpp (external library)
2. yaml_interface (our wrapper)
3. CATChem_core (depends on yaml_interface)

### 4. Example Program

**File:** `examples/yaml_interface_example.F90`

Demonstrates the usage of the new high-level interface:

```fortran
! Generic interface examples
call yaml_get(config, 'output/directory', output_dir, status, '/default/path')
call yaml_get(config, 'species/count', num_species, status, 50)
call yaml_get(config, 'simulation/timestep', timestep_dp, status, 30.0_real64)

! Array interface examples
call yaml_get_array(config, 'species/ids', species_ids, status, actual_size)
call yaml_get_array(config, 'emissions/rates', emission_rates, status, actual_size)

! Setting values
call yaml_set(config, 'runtime/version', '2.0', status)
call yaml_set(config, 'runtime/threads', 8, status)
```

## API Features

### Generic `yaml_get` Interface

```fortran
! Signatures for different types
call yaml_get(node, key, string_value, rc [, default_value])
call yaml_get(node, key, integer_value, rc [, default_value])
call yaml_get(node, key, real_value, rc [, default_value])
call yaml_get(node, key, logical_value, rc [, default_value])
```

**Features:**
- Automatic type detection and conversion
- Optional default values
- Return codes for error handling
- Support for single/double precision reals

### Generic `yaml_set` Interface

```fortran
! Signatures for different types
call yaml_set(node, key, string_value, rc)
call yaml_set(node, key, integer_value, rc)
call yaml_set(node, key, real_value, rc)
call yaml_set(node, key, logical_value, rc)
```

### Generic `yaml_get_array` Interface

```fortran
! Signatures for different array types
call yaml_get_array(node, key, string_array, rc, actual_size)
call yaml_get_array(node, key, integer_array, rc, actual_size)
call yaml_get_array(node, key, real_array, rc, actual_size)
```

**Features:**
- Returns actual array size
- Handles arrays of any supported type
- Memory-safe operations

## Usage Examples

### Basic Configuration Loading

```fortran
use yaml_interface_mod
type(yaml_node_t) :: config
integer :: rc

! Load from file
config = yaml_load_file('config.yaml')
if (.not. c_associated(config%ptr)) then
    write(*,*) 'Error loading config file'
    stop 1
endif

! Get values with defaults
call yaml_get(config, 'output/directory', output_dir, rc, './output')
call yaml_get(config, 'simulation/timestep', timestep, rc, 60.0)

! Clean up
call yaml_destroy_node(config)
```

### ConfigManager Usage

```fortran
use ConfigManager_Mod
type(ConfigManagerType) :: config_mgr
integer :: rc

call config_mgr%init(rc)
call config_mgr%load_from_file('CATChem_config.yml', rc)

! Access configuration through high-level interface
call config_mgr%get_string('output/directory', output_dir, rc)
call config_mgr%get_real('processes/dust/scale_factor', scale_factor, rc)
```

## Benefits

1. **Modern YAML Support**: Full YAML 1.2 specification support via yaml-cpp
2. **Type Safety**: Compile-time type checking with generic interfaces
3. **Ease of Use**: Single interface works for all data types
4. **Memory Safety**: Automatic cleanup and proper resource management
5. **Performance**: Direct C++ integration without text parsing overhead
6. **Maintainability**: Clean, modern Fortran code with clear separation of concerns

## Migration Notes

- Old `fyaml_get()` calls are replaced with generic `yaml_get()` calls
- YAML node cleanup is now handled by `yaml_destroy_node()`
- Configuration validation is built into the ConfigManager
- Array handling is now type-safe and memory-efficient

## Future Enhancements

- Schema validation with detailed error messages
- Support for YAML anchors and references
- Configuration templates and presets
- Hot-reloading of configuration files
- Integration with command-line and environment variable overrides

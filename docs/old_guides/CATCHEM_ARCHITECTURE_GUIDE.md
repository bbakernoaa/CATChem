# CATChem General Architecture Guide

## Overview

CATChem (Configurable ATmospheric Chemistry) is a modular atmospheric chemistry modeling system designed to provide flexible, maintainable, and scientifically accurate chemical and aerosol process simulations. This guide provides a comprehensive overview of CATChem's architecture, design patterns, and integration points.

## Table of Contents

1. [System Architecture](#system-architecture)
2. [Core Design Patterns](#core-design-patterns)
3. [Module Organization](#module-organization)
4. [Interface Layers](#interface-layers)
5. [Data Flow](#data-flow)
6. [Configuration System](#configuration-system)
7. [Error Handling](#error-handling)
8. [Build System](#build-system)
9. [Testing Framework](#testing-framework)
10. [Integration Points](#integration-points)

---

## System Architecture

### High-Level Architecture

CATChem follows a layered, modular architecture with clear separation of concerns:

```
┌─────────────────────────────────────────────────────────────────┐
│                    HOST MODEL INTEGRATION                       │
├─────────────────────────────────────────────────────────────────┤
│   CCPP Driver    │   NUOPC Driver   │   Standalone Driver       │
├─────────────────────────────────────────────────────────────────┤
│                      CATChem API Layer                          │
├─────────────────────────────────────────────────────────────────┤
│                    Process Management                           │
│  ┌─────────────┐ ┌─────────────┐ ┌─────────────┐ ┌─────────────┐│
│  │    Dust     │ │  Sea Salt   │ │   Dry Dep   │ │  Chemistry  ││
│  │   Process   │ │   Process   │ │   Process   │ │   Process   ││
│  └─────────────┘ └─────────────┘ └─────────────┘ └─────────────┘│
├─────────────────────────────────────────────────────────────────┤
│                      Core Infrastructure                        │
│  ┌─────────────┐ ┌─────────────┐ ┌─────────────┐ ┌─────────────┐│
│  │   State     │ │    Grid     │ │ Diagnostic  │ │   Error     ││
│  │ Management  │ │ Management  │ │ Management  │ │ Management  ││
│  └─────────────┘ └─────────────┘ └─────────────┘ └─────────────┘│
├─────────────────────────────────────────────────────────────────┤
│                    Foundation Layer                             │
│  ┌─────────────┐ ┌─────────────┐ ┌─────────────┐ ┌─────────────┐│
│  │ Precision   │ │ Constants   │ │ Utilities   │ │   Config    ││
│  │    Types    │ │             │ │             │ │   Manager   ││
│  └─────────────┘ └─────────────┘ └─────────────┘ └─────────────┘│
└─────────────────────────────────────────────────────────────────┘
```

### Key Architectural Principles

1. **Modularity**: Each component is self-contained with well-defined interfaces
2. **Extensibility**: New processes and drivers can be added without modifying existing code
3. **Maintainability**: Clear separation of concerns and consistent coding patterns
4. **Performance**: Efficient memory management and computational optimization
5. **Interoperability**: Multiple interface layers for different host models
6. **Testability**: Comprehensive testing framework with unit and integration tests

---

## Core Design Patterns

### 1. Dependency Injection Container

The `StateContainerType` implements dependency injection for managing all system components:

```fortran
type :: StateContainerType
   ! Core state objects
   type(ConfigDataType), allocatable :: config
   type(MetStateType), allocatable :: met_state
   type(ChemStateType), allocatable :: chem_state
   type(EmisStateType), allocatable :: emis_state

   ! Modern infrastructure
   type(DiagnosticManagerType), allocatable :: diag_mgr
   type(GridManagerType), allocatable :: grid_mgr
   type(ErrorManagerType) :: error_mgr
end type
```

**Benefits:**
- Centralized state management
- Consistent lifecycle management
- Easy testing and mocking
- Clear dependency relationships

### 2. Factory Pattern

The `ProcessFactoryType` creates process instances dynamically:

```fortran
type :: ProcessFactoryType
contains
   procedure :: create_process
   procedure :: register_process_type
   procedure :: list_available_processes
end type
```

**Benefits:**
- Runtime process selection
- Easy addition of new processes
- Consistent process creation
- Configuration-driven instantiation

### 3. Builder Pattern

The `StateBuilderType` provides flexible state container construction:

```fortran
type :: StateBuilderType
contains
   procedure :: init
   procedure :: with_config
   procedure :: with_grid
   procedure :: build
end type
```

**Benefits:**
- Flexible initialization
- Step-by-step construction
- Validation during building
- Readable configuration code

### 4. Strategy Pattern

Process implementations use strategy pattern for scheme selection:

```fortran
type, extends(ProcessInterface) :: DustProcess
   class(DustSchemeInterface), allocatable :: scheme
contains
   procedure :: run => dust_run
end type
```

**Benefits:**
- Algorithm selection at runtime
- Easy addition of new schemes
- Consistent process interface
- Scientific flexibility

### 5. Observer Pattern

Diagnostic system uses observer pattern for data collection:

```fortran
type :: DiagnosticManagerType
   type(DiagnosticRegistryType), allocatable :: registries(:)
contains
   procedure :: register_observer
   procedure :: notify_observers
   procedure :: collect_diagnostics
end type
```

**Benefits:**
- Loose coupling between processes and diagnostics
- Dynamic diagnostic registration
- Efficient data collection
- Runtime diagnostic control

---

## Module Organization

### Core Modules (`src/core/`)

| Module | Purpose | Key Types |
|--------|---------|-----------|
| `precision_mod.F90` | Floating-point precision definitions | `fp`, `sp`, `dp` |
| `constants.F90` | Physical and mathematical constants | Universal constants |
| `error_mod.F90` | Error handling infrastructure | `ErrorManagerType` |
| `state_mod.F90` | State container and management | `StateContainerType` |
| `config_mod.F90` | Configuration management | `ConfigManagerType` |
| `*state_mod.F90` | Individual state objects | `MetStateType`, etc. |

### Process Modules (`src/process/`)

| Process | Directory | Purpose |
|---------|-----------|---------|
| Dust | `dust/` | Dust emission and transport |
| Sea Salt | `seasalt/` | Sea salt emission |
| Dry Deposition | `drydep/` | Dry deposition calculations |
| Chemistry | `chem/` | Gas-phase and multiphase chemistry |
| Plume Rise | `plumerise/` | Vertical distribution of emissions |

### Interface Drivers (`drivers/`)

| Driver | Purpose | Host Integration |
|--------|---------|------------------|
| CCPP | Common Community Physics Package | UFS, FV3, etc. |
| NUOPC | NUOPC/ESMF framework | Earth System Models |

### API Layer (`src/api/`)

| Module | Purpose |
|--------|---------|
| `catchem.F90` | Main API interface |
| `run_mod.F90` | Process execution routines |

---

## Interface Layers

### 1. Process Interface Layer

All atmospheric processes implement the `ProcessInterface`:

```fortran
type, abstract :: ProcessInterface
contains
   procedure(init_interface), deferred :: init
   procedure(run_interface), deferred :: run
   procedure(finalize_interface), deferred :: finalize
   procedure :: register_diagnostics
   procedure :: update_diagnostics
end type
```

**Key Features:**
- Standardized process lifecycle
- Diagnostic integration
- Column virtualization support
- Multi-scheme capability

### 2. Driver Interface Layer

Host model integration through standardized drivers:

#### CCPP Driver
- CCPP-compliant metadata
- Host model data transformation
- Error handling integration
- Memory management

#### NUOPC Driver
- ESMF field management
- CF-compliant I/O
- NUOPC time management
- Parallel execution support

### 3. Configuration Interface

YAML-based configuration system:

```yaml
simulation:
  name: "test_run"
  start_date: "2024-01-01T00:00:00"

processes:
  dust:
    activate: true
    scheme: "fengsha"
  chemistry:
    activate: true
    mechanism: "cb6r3"
```

---

## Data Flow

### Initialization Flow

```
1. Host Model Start
   ↓
2. Driver Init (CCPP/NUOPC)
   ↓
3. Configuration Loading
   ↓
4. StateContainer Building
   ↓
5. Process Factory Setup
   ↓
6. Process Registration
   ↓
7. Grid Manager Init
   ↓
8. Diagnostic Manager Init
   ↓
9. Ready for Execution
```

### Runtime Flow

```
1. Host Model Physics Step
   ↓
2. Data Transformation (Host → CATChem)
   ↓
3. Grid Virtualization
   ↓
4. Process Execution
   │ ├── Column Processing
   │ ├── 3D Processing
   │ └── Diagnostic Updates
   ↓
5. Data Transformation (CATChem → Host)
   ↓
6. Host Model Continues
```

### Column Virtualization Flow

```
3D Grid State
   ↓
Column Iterator
   ↓
Virtual Column Creation
   ↓
Process Execution on Column
   ↓ (for each column)
Results Aggregation
   ↓
3D Grid State Update
```

---

## Configuration System

### Configuration Hierarchy

1. **Default Configuration**: Built-in defaults
2. **Site Configuration**: Site-specific settings
3. **Run Configuration**: Run-specific parameters
4. **Runtime Overrides**: Command-line or API overrides

### Configuration Validation

```fortran
type :: ConfigValidatorType
contains
   procedure :: validate_physics_consistency
   procedure :: validate_grid_parameters
   procedure :: validate_time_stepping
   procedure :: validate_process_dependencies
end type
```

### Configuration Examples

#### Basic Configuration
```yaml
simulation:
  name: "basic_run"
  timestep: 3600

processes:
  dust:
    activate: true
    scheme: "ginoux"
```

#### Advanced Configuration
```yaml
simulation:
  name: "advanced_run"
  chemistry_timestep: 1800
  diagnostics_frequency: 3600

grid:
  column_processing: true
  virtualization_enabled: true

processes:
  chemistry:
    activate: true
    mechanism: "racm2"
    solver: "rosenbrock"
    rtol: 1.0e-3

diagnostics:
  output_frequency: 3600
  fields:
    - "O3"
    - "NO2"
    - "dust_concentration"
```

---

## Error Handling

### Error Management Architecture

```fortran
type :: ErrorManagerType
   integer :: error_count
   type(ErrorContextType), allocatable :: context_stack(:)
contains
   procedure :: push_context
   procedure :: pop_context
   procedure :: report_error
   procedure :: report_warning
end type
```

### Error Categories

| Category | Code Range | Usage |
|----------|------------|-------|
| Success | 0 | Successful operations |
| Configuration | 1000-1999 | Configuration errors |
| Memory | 2000-2999 | Memory allocation errors |
| Process | 3000-3999 | Process execution errors |
| I/O | 4000-4999 | Input/output errors |
| Grid | 5000-5999 | Grid management errors |

### Error Handling Best Practices

```fortran
! Always check return codes
call some_operation(container, rc)
if (rc /= CC_SUCCESS) then
   call error_mgr%report_error(ERROR_PROCESS_FAILURE, &
                              'Operation failed', rc, &
                              'calling_routine', &
                              'Check input parameters')
   return
endif
```

---

## Build System

### CMake Structure

```
CMakeLists.txt              # Root build configuration
├── src/
│   ├── CMakeLists.txt       # Source build rules
│   ├── core/CMakeLists.txt  # Core modules
│   ├── process/CMakeLists.txt # Process modules
│   └── api/CMakeLists.txt   # API layer
├── drivers/
│   ├── ccpp/CMakeLists.txt  # CCPP driver
│   └── nuopc/CMakeLists.txt # NUOPC driver
└── tests/CMakeLists.txt     # Test suite
```

### Build Options

| Option | Default | Purpose |
|--------|---------|---------|
| `BUILD_PROCESSES` | OFF | Build process modules |
| `BUILD_API` | OFF | Build API layer |
| `BUILD_DRIVERS` | OFF | Build driver interfaces |
| `BUILD_TESTS` | ON | Build test suite |
| `ENABLE_OPENMP` | OFF | OpenMP parallelization |

### Dependencies

**Required:**
- Modern Fortran compiler (2008+)
- CMake 3.15+
- fyaml (YAML parser)

**Optional:**
- ESMF/NUOPC (for NUOPC driver)
- NetCDF (for I/O)
- MICM (for chemistry)

---

## Testing Framework

### Test Organization

```
tests/
├── unit/               # Unit tests
│   ├── test_state.f90
│   ├── test_process.f90
│   └── test_config.f90
├── integration/        # Integration tests
│   ├── test_dust.f90
│   └── test_chemistry.f90
└── system/            # System tests
    ├── test_ccpp.f90
    └── test_nuopc.f90
```

### Test Utilities

```fortran
module testing_mod
contains
   procedure :: assert_equal
   procedure :: assert_near
   procedure :: setup_test_container
   procedure :: cleanup_test_container
end module
```

### Running Tests

```bash
# Build and run all tests
cmake --build build -j
ctest --test-dir build/tests

# Run specific test categories
ctest --test-dir build/tests -R "unit"
ctest --test-dir build/tests -R "integration"
```

---

## Integration Points

### Host Model Integration

#### CCPP Integration
1. **Metadata Compliance**: CCPP-compliant metadata files
2. **Data Transformation**: Host ↔ CATChem data conversion
3. **Error Handling**: CCPP error reporting
4. **Memory Management**: CCPP memory lifecycle

#### NUOPC Integration
1. **Component Registration**: NUOPC component setup
2. **Field Management**: ESMF field creation and management
3. **Time Management**: NUOPC clock integration
4. **Parallel Execution**: MPI and ESMF parallelization

### External Libraries

#### MICM Integration
- Chemistry solver integration
- Mechanism configuration
- Performance optimization
- Error handling

#### ESMF Integration
- Grid management
- Field operations
- Regridding capabilities
- Parallel I/O

---

## Performance Considerations

### Memory Management
- Allocatable arrays for dynamic sizing
- Memory pooling for frequent allocations
- Cleanup routines for proper deallocation

### Computational Optimization
- Column virtualization for cache efficiency
- Vectorization-friendly algorithms
- OpenMP parallelization support

### I/O Optimization
- Buffered I/O operations
- Parallel NetCDF when available
- Efficient diagnostic collection

---

## Future Architecture Enhancements

### Planned Improvements
1. **GPU Support**: CUDA/HIP acceleration
2. **Advanced Parallelization**: Task-based parallelism
3. **Machine Learning Integration**: ML-based parameterizations
4. **Cloud Integration**: Cloud-native deployment
5. **Real-time Capabilities**: Online data assimilation

### Extensibility Points
1. **New Process Types**: Framework for new process categories
2. **Alternative Solvers**: Pluggable solver architectures
3. **Custom Diagnostics**: User-defined diagnostic calculations
4. **External Coupling**: Advanced coupling frameworks

---

## Conclusion

CATChem's architecture provides a robust, flexible foundation for atmospheric chemistry modeling. The modular design, consistent patterns, and comprehensive infrastructure enable:

- Easy integration with multiple host models
- Straightforward addition of new processes
- Maintainable and testable code
- High performance and scalability
- Scientific accuracy and flexibility

This architecture supports both current modeling needs and future enhancements, ensuring CATChem remains a valuable tool for the atmospheric chemistry community.

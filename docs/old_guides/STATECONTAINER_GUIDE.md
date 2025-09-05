# StateContainer and Sub-States Architecture Guide

## Overview

The StateContainer system in CATChem implements a modern dependency injection pattern that centralizes all state management while maintaining clean separation of concerns. This guide provides detailed information about the StateContainer architecture and its constituent sub-states.

## Table of Contents

1. [StateContainer Architecture](#statecontainer-architecture)
2. [Core Sub-States](#core-sub-states)
3. [State Lifecycle Management](#state-lifecycle-management)
4. [State Access Patterns](#state-access-patterns)
5. [Memory Management](#memory-management)
6. [Error Handling Integration](#error-handling-integration)
7. [Advanced Features](#advanced-features)
8. [Usage Examples](#usage-examples)
9. [Best Practices](#best-practices)
10. [Migration Guide](#migration-guide)

---

## StateContainer Architecture

### Design Philosophy

The StateContainer follows the dependency injection container pattern, providing:

1. **Centralized State Management**: All state objects are managed in one place
2. **Controlled Access**: Type-safe accessors for state objects
3. **Lifecycle Management**: Consistent initialization and cleanup
4. **Dependency Injection**: Components receive dependencies rather than creating them
5. **Testability**: Easy mocking and testing of individual components

### StateContainer Structure

```fortran
type :: StateContainerType
   private

   ! Core state objects
   type(ConfigDataType), allocatable :: config
   type(ConfigManagerType), allocatable :: config_mgr
   type(MetStateType), allocatable :: met_state
   type(ChemStateType), allocatable :: chem_state
   type(EmisStateType), allocatable :: emis_state
   type(DiagStateType), allocatable :: diag_state  ! Legacy

   ! Modern infrastructure
   type(DiagnosticManagerType), allocatable :: diag_mgr
   type(GridManagerType), allocatable :: grid_mgr

   ! Error handling
   type(ErrorManagerType) :: error_mgr

   ! Container metadata
   logical :: is_initialized = .false.
   logical :: is_valid = .false.
   character(len=256) :: name = ''
   integer :: creation_time

contains
   ! [Methods described in following sections]
end type StateContainerType
```

### Key Design Principles

1. **Encapsulation**: All state objects are private, accessed through public methods
2. **Immutability**: Const accessors prevent unwanted modifications
3. **Validation**: Built-in validation ensures state consistency
4. **Error Handling**: Integrated error management with context tracking
5. **Resource Management**: Automatic cleanup and memory management

---

## Core Sub-States

### 1. Configuration State (`ConfigDataType`)

**Purpose**: Stores all runtime configuration parameters

**Structure**:
```fortran
type :: ConfigDataType
   ! Simulation parameters
   character(len=256) :: simulation_name
   character(len=32) :: start_date
   character(len=32) :: end_date
   real(fp) :: chemistry_timestep
   real(fp) :: transport_timestep

   ! Process activation flags
   logical :: dust_active
   logical :: seasalt_active
   logical :: chemistry_active
   logical :: drydep_active

   ! Grid parameters
   integer :: nx, ny, nz
   real(fp) :: dx, dy

   ! Species configuration
   integer :: n_species
   character(len=32), allocatable :: species_names(:)

   ! Process-specific parameters
   type(DustConfigType) :: dust_config
   type(ChemConfigType) :: chem_config
   type(EmisConfigType) :: emis_config

contains
   procedure :: init => config_init
   procedure :: validate => config_validate
   procedure :: finalize => config_finalize
   procedure :: load_from_file => config_load_from_file
   procedure :: save_to_file => config_save_to_file
end type ConfigDataType
```

**Key Features**:
- YAML-based configuration loading
- Hierarchical parameter organization
- Runtime validation
- Default value management
- Process-specific parameter groups

**Example Configuration**:
```yaml
simulation:
  name: "test_run"
  start_date: "2024-01-01T00:00:00"
  chemistry_timestep: 3600

processes:
  dust:
    activate: true
    scheme: "fengsha"
    alpha: 1.0

grid:
  nx: 100
  ny: 100
  nz: 28
```

### 2. Meteorological State (`MetStateType`)

**Purpose**: Contains all meteorological fields required for chemistry calculations

**Structure**:
```fortran
type :: MetStateType
   ! Grid dimensions
   integer :: nx, ny, nz

   ! Meteorological fields (3D)
   real(fp), allocatable :: temperature(:,:,:)    ! [K]
   real(fp), allocatable :: pressure(:,:,:)       ! [Pa]
   real(fp), allocatable :: humidity(:,:,:)       ! [kg/kg]
   real(fp), allocatable :: wind_u(:,:,:)         ! [m/s]
   real(fp), allocatable :: wind_v(:,:,:)         ! [m/s]
   real(fp), allocatable :: wind_w(:,:,:)         ! [m/s]
   real(fp), allocatable :: air_density(:,:,:)    ! [kg/m³]
   real(fp), allocatable :: cloud_water(:,:,:)    ! [kg/kg]
   real(fp), allocatable :: cloud_ice(:,:,:)      ! [kg/kg]

   ! Surface fields (2D)
   real(fp), allocatable :: surface_pressure(:,:)      ! [Pa]
   real(fp), allocatable :: surface_temperature(:,:)   ! [K]
   real(fp), allocatable :: surface_humidity(:,:)      ! [kg/kg]
   real(fp), allocatable :: friction_velocity(:,:)     ! [m/s]
   real(fp), allocatable :: sensible_heat_flux(:,:)    ! [W/m²]
   real(fp), allocatable :: latent_heat_flux(:,:)      ! [W/m²]
   real(fp), allocatable :: precipitation_rate(:,:)    ! [mm/hr]
   real(fp), allocatable :: snow_depth(:,:)            ! [m]
   real(fp), allocatable :: soil_moisture(:,:,:)       ! [m³/m³]
   real(fp), allocatable :: soil_temperature(:,:,:)    ! [K]

   ! Radiation fields
   real(fp), allocatable :: solar_zenith_angle(:,:)    ! [radians]
   real(fp), allocatable :: shortwave_down(:,:)        ! [W/m²]
   real(fp), allocatable :: longwave_down(:,:)         ! [W/m²]
   real(fp), allocatable :: albedo(:,:)                ! [0-1]

   ! Derived fields
   real(fp), allocatable :: boundary_layer_height(:,:) ! [m]
   real(fp), allocatable :: mixing_ratio(:,:,:)        ! [kg/kg]
   real(fp), allocatable :: potential_temperature(:,:,:) ! [K]

   ! Land surface properties
   integer, allocatable :: land_use_category(:,:)
   real(fp), allocatable :: vegetation_fraction(:,:)
   real(fp), allocatable :: leaf_area_index(:,:)
   real(fp), allocatable :: roughness_length(:,:)

contains
   procedure :: init => met_init
   procedure :: finalize => met_finalize
   procedure :: validate => met_validate
   procedure :: update_derived_fields => met_update_derived
   procedure :: interpolate_temporal => met_interpolate_temporal
   procedure :: check_bounds => met_check_bounds
end type MetStateType
```

**Key Features**:
- Comprehensive meteorological field coverage
- Automatic derived field calculation
- Temporal interpolation capabilities
- Bounds checking and validation
- Units and metadata management

### 3. Chemical State (`ChemStateType`)

**Purpose**: Manages chemical species concentrations and related properties

**Structure**:
```fortran
type :: ChemStateType
   ! Grid and species dimensions
   integer :: nx, ny, nz, n_species

   ! Species information
   character(len=32), allocatable :: species_names(:)
   real(fp), allocatable :: molecular_weights(:)     ! [g/mol]
   real(fp), allocatable :: henry_constants(:)       ! [mol/L/atm]

   ! Concentration fields
   real(fp), allocatable :: concentrations(:,:,:,:)  ! [mixing ratio] (nx,ny,nz,species)
   real(fp), allocatable :: concentrations_prev(:,:,:,:) ! Previous timestep

   ! Aerosol properties (if applicable)
   integer :: n_size_bins
   real(fp), allocatable :: size_bin_bounds(:)       ! [μm]
   real(fp), allocatable :: aerosol_number(:,:,:,:)  ! [#/cm³] (nx,ny,nz,bins)
   real(fp), allocatable :: aerosol_mass(:,:,:,:,:)  ! [μg/m³] (nx,ny,nz,bins,species)
   real(fp), allocatable :: aerosol_density(:,:,:,:) ! [g/cm³]
   real(fp), allocatable :: aerosol_diameter(:,:,:,:) ! [μm]

   ! Chemical reaction rates (optional)
   integer :: n_reactions
   real(fp), allocatable :: reaction_rates(:,:,:,:)  ! [various units]

   ! Photolysis rates
   integer :: n_photolysis
   real(fp), allocatable :: photolysis_rates(:,:,:,:) ! [1/s]

   ! Deposition velocities
   real(fp), allocatable :: dry_deposition_velocity(:,:,:) ! [m/s]
   real(fp), allocatable :: wet_deposition_rate(:,:,:)     ! [1/s]

contains
   procedure :: init => chem_init
   procedure :: finalize => chem_finalize
   procedure :: validate => chem_validate
   procedure :: find_species => chem_find_species
   procedure :: get_concentration => chem_get_concentration
   procedure :: set_concentration => chem_set_concentration
   procedure :: add_species => chem_add_species
   procedure :: remove_species => chem_remove_species
   procedure :: convert_units => chem_convert_units
   procedure :: check_conservation => chem_check_conservation
   procedure :: apply_boundary_conditions => chem_apply_bc
end type ChemStateType
```

**Key Features**:
- Flexible species management
- Multiple concentration representations
- Aerosol size distribution support
- Unit conversion capabilities
- Conservation checking
- Boundary condition application

### 4. Emission State (`EmisStateType`)

**Purpose**: Manages emission sources and fluxes

**Structure**:
```fortran
type :: EmisStateType
   ! Grid dimensions
   integer :: nx, ny, nz, n_species

   ! Emission fluxes
   real(fp), allocatable :: surface_emissions(:,:,:)    ! [mol/m²/s] (nx,ny,species)
   real(fp), allocatable :: elevated_emissions(:,:,:,:) ! [mol/m³/s] (nx,ny,nz,species)
   real(fp), allocatable :: biogenic_emissions(:,:,:)   ! [mol/m²/s]
   real(fp), allocatable :: anthropogenic_emissions(:,:,:) ! [mol/m²/s]
   real(fp), allocatable :: wildfire_emissions(:,:,:,:) ! [mol/m³/s]

   ! Emission source information
   integer :: n_point_sources
   type(PointSourceType), allocatable :: point_sources(:)

   integer :: n_area_sources
   type(AreaSourceType), allocatable :: area_sources(:)

   ! Temporal profiles
   real(fp), allocatable :: hourly_profile(:,:)    ! [0-1] (24,species)
   real(fp), allocatable :: daily_profile(:,:)     ! [0-1] (7,species)
   real(fp), allocatable :: monthly_profile(:,:)   ! [0-1] (12,species)

   ! Speciation profiles
   integer :: n_profiles
   type(SpeciationProfile), allocatable :: profiles(:)

contains
   procedure :: init => emis_init
   procedure :: finalize => emis_finalize
   procedure :: validate => emis_validate
   procedure :: add_point_source => emis_add_point_source
   procedure :: add_area_source => emis_add_area_source
   procedure :: apply_temporal_profile => emis_apply_temporal
   procedure :: apply_speciation => emis_apply_speciation
   procedure :: update_emissions => emis_update
   procedure :: get_total_emissions => emis_get_total
end type EmisStateType
```

**Key Features**:
- Multiple emission source types
- Temporal profile application
- Chemical speciation
- Point and area source management
- Real-time emission updates

### 5. Diagnostic State (`DiagStateType`) - Legacy

**Purpose**: Legacy diagnostic management (being replaced by DiagnosticManager)

**Note**: This is being phased out in favor of the new dynamic diagnostic system.

### 6. Grid Manager (`GridManagerType`)

**Purpose**: Advanced grid management and column virtualization

**Structure**:
```fortran
type :: GridManagerType
   ! Grid geometry
   type(GridGeometryType) :: geometry

   ! Column virtualization
   logical :: virtualization_enabled
   integer :: n_columns
   type(VirtualColumnType), allocatable :: columns(:)

   ! Iterator support
   type(ColumnIteratorType) :: iterator

   ! Parallel decomposition
   integer :: n_tasks
   integer, allocatable :: task_assignment(:)

contains
   procedure :: init => grid_init
   procedure :: finalize => grid_finalize
   procedure :: create_virtual_columns => grid_create_columns
   procedure :: get_column_iterator => grid_get_iterator
   procedure :: distribute_tasks => grid_distribute_tasks
end type GridManagerType
```

**Key Features**:
- Column virtualization for performance
- Iterator patterns for grid traversal
- Parallel task distribution
- Geometry management

### 7. Diagnostic Manager (`DiagnosticManagerType`)

**Purpose**: Modern, dynamic diagnostic management system

**Structure**:
```fortran
type :: DiagnosticManagerType
   ! Process registries
   type(DiagnosticRegistryType), allocatable :: process_registries(:)
   integer :: n_registries

   ! Output management
   character(len=256) :: output_directory
   integer :: output_frequency
   logical :: output_enabled

   ! Collection management
   logical :: collection_active
   integer :: collection_counter

contains
   procedure :: init => diag_mgr_init
   procedure :: finalize => diag_mgr_finalize
   procedure :: register_process => diag_mgr_register_process
   procedure :: collect_diagnostics => diag_mgr_collect
   procedure :: write_output => diag_mgr_write_output
   procedure :: print_summary => diag_mgr_print_summary
end type DiagnosticManagerType
```

**Key Features**:
- Process-specific diagnostic registries
- Runtime diagnostic registration
- Configurable output management
- Integrated with StateContainer lifecycle

---

## State Lifecycle Management

### Initialization Sequence

```fortran
! 1. Container initialization
call container%init('main_container', rc)

! 2. Core state allocation and initialization
call container%init_config(rc)
call container%init_met_state(nx, ny, nz, rc)
call container%init_chem_state(nx, ny, nz, n_species, rc)
call container%init_emis_state(nx, ny, nz, n_species, rc)

! 3. Infrastructure initialization
call container%init_grid_manager(rc)
call container%init_diagnostic_manager(rc)

! 4. Cross-state dependencies
call container%link_dependencies(rc)

! 5. Validation
call container%validate(rc)
```

### Builder Pattern Usage

```fortran
type(StateBuilderType) :: builder
type(StateContainerType) :: container

! Step-by-step construction
call builder%init()
call builder%with_config('config.yml')
call builder%with_grid(100, 100, 28)
call builder%with_species(['O3', 'NO2', 'CO'])
call builder%with_processes(['dust', 'chemistry'])
call builder%build(container, rc)
```

### Finalization Sequence

```fortran
! 1. Process cleanup
call container%finalize_processes(rc)

! 2. State cleanup (reverse order)
call container%finalize_diagnostic_manager(rc)
call container%finalize_grid_manager(rc)
call container%finalize_emis_state(rc)
call container%finalize_chem_state(rc)
call container%finalize_met_state(rc)
call container%finalize_config(rc)

! 3. Container cleanup
call container%finalize(rc)
```

---

## State Access Patterns

### Read-Only Access

```fortran
! Get const reference to state
type(MetStateType), pointer :: met_state
met_state => container%get_met_state()

! Safe read operations
temperature_value = met_state%temperature(i,j,k)
pressure_value = met_state%pressure(i,j,k)
```

### Mutable Access

```fortran
! Get mutable pointer for modifications
type(ChemStateType), pointer :: chem_state
chem_state => container%get_chem_state_ptr()

! Modify state
chem_state%concentrations(i,j,k,species_idx) = new_value
call chem_state%validate(rc)
```

### Error-Safe Access

```fortran
type(MetStateType), pointer :: met_state
type(ErrorManagerType), pointer :: error_mgr

error_mgr => container%get_error_manager()
call error_mgr%push_context('process_meteorology')

met_state => container%get_met_state_ptr()
if (.not. associated(met_state)) then
   call error_mgr%report_error(ERROR_NOT_INITIALIZED, &
                              'MetState not initialized', rc)
   return
endif

! Use met_state safely...

call error_mgr%pop_context()
```

---

## Memory Management

### Automatic Memory Management

The StateContainer provides automatic memory management through:

1. **Allocatable Components**: All state objects use allocatable arrays
2. **Automatic Deallocation**: Finalizers handle cleanup
3. **Memory Tracking**: Built-in memory usage monitoring
4. **Leak Detection**: Debug mode memory leak detection

### Memory Usage Monitoring

```fortran
integer(8) :: memory_usage
character(len=256) :: memory_report

! Get current memory usage
memory_usage = container%get_memory_usage()

! Generate detailed memory report
call container%generate_memory_report(memory_report)
write(*,*) 'Memory usage: ', memory_report
```

### Memory Optimization

```fortran
! Enable memory optimization
call container%set_memory_optimization(.true.)

! Compact memory layout
call container%compact_memory(rc)

! Memory pooling for frequent allocations
call container%enable_memory_pooling(.true.)
```

---

## Error Handling Integration

### Context-Aware Error Handling

```fortran
type(ErrorManagerType), pointer :: error_mgr

error_mgr => container%get_error_manager()
call error_mgr%push_context('state_initialization')

! State operations with error context
call some_state_operation(container, rc)
if (rc /= CC_SUCCESS) then
   call error_mgr%report_error(ERROR_STATE_OPERATION, &
                              'State operation failed', rc, &
                              'container_init', &
                              'Check state consistency')
   call error_mgr%pop_context()
   return
endif

call error_mgr%pop_context()
```

### Error Recovery

```fortran
! Attempt state recovery
call container%attempt_recovery(rc)
if (rc == CC_SUCCESS) then
   write(*,*) 'State recovery successful'
else
   write(*,*) 'State recovery failed, aborting'
   call container%emergency_shutdown()
endif
```

---

## Advanced Features

### State Serialization

```fortran
! Save state to file
call container%save_state('checkpoint.nc', rc)

! Load state from file
call container%load_state('checkpoint.nc', rc)

! Binary serialization for performance
call container%serialize_binary('state.bin', rc)
call container%deserialize_binary('state.bin', rc)
```

### State Validation

```fortran
! Comprehensive validation
call container%validate(rc)

! Specific validation types
call container%validate_physics_consistency(rc)
call container%validate_grid_consistency(rc)
call container%validate_species_conservation(rc)
```

### State Comparison

```fortran
type(StateContainerType) :: container1, container2
logical :: are_equal
real(fp) :: max_difference

! Compare two states
call compare_containers(container1, container2, are_equal, max_difference)

! Detailed comparison report
call generate_comparison_report(container1, container2, 'comparison.txt')
```

### State Interpolation

```fortran
! Temporal interpolation between states
call interpolate_states(state_t1, state_t2, target_time, interpolated_state, rc)

! Spatial interpolation
call interpolate_to_grid(source_container, target_grid, target_container, rc)
```

---

## Usage Examples

### Basic StateContainer Usage

```fortran
program basic_example
   use state_mod

   type(StateContainerType) :: container
   type(MetStateType), pointer :: met_state
   integer :: rc

   ! Initialize container
   call container%init('basic_example', rc)
   if (rc /= CC_SUCCESS) stop 'Initialization failed'

   ! Initialize meteorological state
   call container%init_met_state(100, 100, 28, rc)
   if (rc /= CC_SUCCESS) stop 'Met state initialization failed'

   ! Get meteorological state
   met_state => container%get_met_state_ptr()

   ! Use meteorological state
   met_state%temperature(:,:,:) = 288.0_fp
   met_state%pressure(:,:,:) = 101325.0_fp

   ! Validate state
   call container%validate(rc)
   if (rc /= CC_SUCCESS) stop 'Validation failed'

   ! Print container information
   call container%print_info()

   ! Cleanup
   call container%finalize(rc)
end program basic_example
```

### Advanced Builder Pattern

```fortran
program builder_example
   use state_mod

   type(StateBuilderType) :: builder
   type(StateContainerType) :: container
   integer :: rc

   ! Configure builder
   call builder%init()
   call builder%with_name('advanced_example')
   call builder%with_config_file('config.yml')
   call builder%with_grid(200, 200, 40)
   call builder%with_species(['O3', 'NO', 'NO2', 'CO', 'CH4'])
   call builder%with_processes(['dust', 'seasalt', 'chemistry', 'drydep'])
   call builder%with_diagnostics(.true.)
   call builder%with_grid_virtualization(.true.)

   ! Build container
   call builder%build(container, rc)
   if (rc /= CC_SUCCESS) stop 'Build failed'

   ! Container is ready for use
   call container%print_info()

   ! Cleanup
   call container%finalize(rc)
end program builder_example
```

### Error Handling Example

```fortran
subroutine safe_state_operation(container, rc)
   type(StateContainerType), intent(inout) :: container
   integer, intent(out) :: rc

   type(ErrorManagerType), pointer :: error_mgr
   type(ChemStateType), pointer :: chem_state

   error_mgr => container%get_error_manager()
   call error_mgr%push_context('safe_state_operation')

   ! Attempt to get chemical state
   chem_state => container%get_chem_state_ptr()
   if (.not. associated(chem_state)) then
      call error_mgr%report_error(ERROR_NOT_INITIALIZED, &
                                 'Chemical state not available', rc)
      call error_mgr%pop_context()
      return
   endif

   ! Perform operation with validation
   call chem_state%update_concentrations(new_values, rc)
   if (rc /= CC_SUCCESS) then
      call error_mgr%report_error(ERROR_STATE_UPDATE, &
                                 'Failed to update concentrations', rc, &
                                 'safe_state_operation', &
                                 'Check concentration values for validity')
      call error_mgr%pop_context()
      return
   endif

   ! Validate result
   call chem_state%validate(rc)
   if (rc /= CC_SUCCESS) then
      call error_mgr%report_error(ERROR_VALIDATION, &
                                 'State validation failed after update', rc)
      call error_mgr%pop_context()
      return
   endif

   call error_mgr%pop_context()
   rc = CC_SUCCESS
end subroutine safe_state_operation
```

---

## Best Practices

### 1. State Access

**Do:**
- Always check return codes
- Use const accessors when possible
- Validate state after modifications
- Use error context management

**Don't:**
- Access state objects directly
- Modify state without validation
- Ignore error conditions
- Mix initialization orders

### 2. Memory Management

**Do:**
- Use automatic memory management features
- Monitor memory usage in production
- Clean up resources properly
- Use memory pooling for performance

**Don't:**
- Manual memory management
- Memory leaks in error paths
- Excessive memory allocation/deallocation
- Large stack allocations

### 3. Error Handling

**Do:**
- Use error contexts for debugging
- Provide meaningful error messages
- Handle all error conditions
- Implement error recovery where possible

**Don't:**
- Ignore error codes
- Generic error messages
- Unhandled error conditions
- Silent failures

### 4. Performance

**Do:**
- Use column virtualization
- Batch operations when possible
- Profile memory access patterns
- Optimize for cache locality

**Don't:**
- Unnecessary state copies
- Inefficient grid traversal
- Memory fragmentation
- Excessive validation in inner loops

---

## Migration Guide

### From Legacy Global Variables

**Before:**
```fortran
! Global variables (legacy)
type(MetStateType) :: met_state
type(ChemStateType) :: chem_state
```

**After:**
```fortran
! StateContainer approach
type(StateContainerType) :: container
type(MetStateType), pointer :: met_state

met_state => container%get_met_state_ptr()
```

### From Direct State Allocation

**Before:**
```fortran
! Direct allocation (legacy)
allocate(met_state%temperature(nx,ny,nz))
call met_state%init(nx, ny, nz)
```

**After:**
```fortran
! StateContainer management
call container%init_met_state(nx, ny, nz, rc)
met_state => container%get_met_state_ptr()
```

### Process Migration

**Before:**
```fortran
subroutine old_process(met_state, chem_state)
   type(MetStateType), intent(in) :: met_state
   type(ChemStateType), intent(inout) :: chem_state
   ! Process implementation
end subroutine
```

**After:**
```fortran
subroutine new_process(container)
   type(StateContainerType), intent(inout) :: container

   type(MetStateType), pointer :: met_state
   type(ChemStateType), pointer :: chem_state

   met_state => container%get_met_state()
   chem_state => container%get_chem_state_ptr()

   ! Process implementation with error handling
end subroutine
```

---

## Conclusion

The StateContainer system provides a robust, modern foundation for state management in CATChem. Its benefits include:

1. **Centralized Management**: All state in one place
2. **Type Safety**: Strong typing and validation
3. **Error Handling**: Integrated error management
4. **Performance**: Optimized memory access patterns
5. **Maintainability**: Clean, testable architecture
6. **Extensibility**: Easy addition of new state types

This architecture supports current needs while providing a foundation for future enhancements, ensuring CATChem remains maintainable and performant as it evolves.

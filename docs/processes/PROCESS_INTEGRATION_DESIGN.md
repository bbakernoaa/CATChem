# Process Integration Architecture: External Data + Plume Rise

## Overview

The crossover between external emission data and plume rise calculations requires a cooperative architecture where multiple processes work together. Here's how we can handle this efficiently.

## Process Interaction Patterns

### 1. **Data Flow Architecture**

```
External Files (NetCDF/ASCII)
    ↓
Driver File I/O
    ↓
External Emission Data Process
    ↓ (provides source parameters)
Plume Rise Process
    ↓ (computes effective heights)
Emission Distribution Process
    ↓ (distributes emissions vertically)
Chemical Transport Model
```

### 2. **Cooperative Process Design**

#### External Emission Data Process
**Responsibilities:**
- Load and manage fire/stack parameter files
- Provide FRP, burn area, stack parameters
- Handle temporal profiles and scaling
- Manage spatial interpolation

**Key Data Structures:**
```fortran
type :: fire_source_data_t
   real(fp) :: latitude, longitude
   real(fp) :: frp                   ! Fire Radiative Power [MW]
   real(fp) :: burn_area             ! Burned area [m²]
   real(fp) :: fuel_load             ! Fuel loading [kg/m²]
   character(len=32) :: fire_type    ! 'wildfire', 'prescribed', etc.
   logical :: active                 ! Source active flag
end type

type :: stack_source_data_t
   real(fp) :: latitude, longitude
   real(fp) :: stack_height          ! Physical stack height [m]
   real(fp) :: stack_diameter        ! Stack diameter [m]
   real(fp) :: exit_velocity         ! Exit velocity [m/s]
   real(fp) :: exit_temperature      ! Exit temperature [K]
   real(fp) :: heat_flux             ! Heat flux [MW]
   character(len=64) :: sector       ! Industry sector
   logical :: active                 ! Source active flag
end type
```

#### Plume Rise Process
**Responsibilities:**
- Receive source parameters from external data process
- Compute effective emission heights using meteorology
- Provide injection height profiles
- Handle different plume rise schemes

**Input Sources:**
- **From External Data Process**: FRP, stack parameters, source locations
- **From Meteorology**: Wind speed, temperature, atmospheric stability
- **From Configuration**: Scheme parameters, thresholds

### 3. **Process Communication Interface**

#### Option A: Direct Process Communication
```fortran
! External Emission Data Process provides source data
type(fire_source_data_t), pointer :: fire_sources(:) => null()
type(stack_source_data_t), pointer :: stack_sources(:) => null()

! Plume Rise Process accesses source data
call external_data_process%get_fire_sources(i, j, fire_sources, rc)
call plume_rise_process%compute_heights(fire_sources, met_fields, heights, rc)
```

#### Option B: Shared Data Container (Recommended)
```fortran
! Shared elevated source container
type :: elevated_source_container_t
   type(fire_source_data_t), allocatable :: fires(:)
   type(stack_source_data_t), allocatable :: stacks(:)
   real(fp), allocatable :: effective_heights(:)  ! Computed by plume rise
   real(fp), allocatable :: injection_profiles(:,:)  ! Height distribution
contains
   procedure :: update_from_external_data
   procedure :: compute_plume_rise
   procedure :: get_injection_profile
end type
```

### 4. **Enhanced External Emission Data Schemes**

#### Fire Data Handler (Enhanced Surface2D)
```fortran
type, extends(Surface2DScheme) :: FireDataHandler
   type(fire_source_data_t), allocatable :: fire_sources(:)
contains
   procedure :: load_fire_parameters
   procedure :: apply_temporal_scaling
   procedure :: validate_frp_data
   procedure :: get_fires_in_cell
end type
```

#### Stack Data Handler (Enhanced Elevated3D)
```fortran
type, extends(Elevated3DScheme) :: StackDataHandler
   type(stack_source_data_t), allocatable :: stack_sources(:)
contains
   procedure :: load_stack_parameters
   procedure :: validate_stack_data
   procedure :: get_stacks_in_cell
end type
```

## Implementation Strategy

### Phase 1: Basic Cooperation
1. **External Data Process** loads fire/stack parameters from files
2. **Plume Rise Process** accesses this data through well-defined interfaces
3. Driver coordinates both processes during initialization and runtime

### Phase 2: Shared Data Container
1. Create `ElevatedSourceContainer` for shared fire/stack data
2. External Data Process populates source parameters
3. Plume Rise Process adds computed heights
4. Both processes access shared container

### Phase 3: Optimization
1. Implement spatial indexing for efficient source lookup
2. Add caching for repeated plume rise calculations
3. Optimize memory usage for large source datasets

## Example Configuration

### External Emission Data Process
```yaml
name: external_emission_data
schemes:
  - name: fire_data_handler
    description: Handler for fire emission data including FRP and burn area
    parameters:
      frp_threshold: 1.0  # MW
      temporal_profiles: true
      fire_types: ["wildfire", "prescribed", "agricultural"]

  - name: stack_data_handler
    description: Handler for industrial stack parameters
    parameters:
      minimum_height: 10.0  # m
      include_fugitive: false
      temporal_profiles: true
```

### Plume Rise Process
```yaml
name: plume_rise
schemes:
  - name: briggs_fires
    description: Briggs plume rise for fires using FRP data
    parameters:
      frp_to_heat_efficiency: 0.3  # Convert FRP to convective heat flux
      minimum_frp: 1.0  # MW

  - name: briggs_stacks
    description: Briggs plume rise for stacks using stack parameters
    parameters:
      building_downwash: true
      terrain_effects: true
```

## Process Execution Flow

### Initialization
```fortran
! 1. External Data Process loads source files
call external_data%load_fire_data("FIRMS_2024.nc", rc)
call external_data%load_stack_data("EPA_NEI_stacks.nc", rc)

! 2. Create shared elevated source container
call elevated_container%initialize(external_data, rc)

! 3. Initialize plume rise process with source data
call plume_rise%initialize(config, elevated_container, rc)
```

### Runtime (each timestep)
```fortran
! 1. Update temporal scaling for sources
call external_data%update_temporal_scaling(current_time, rc)

! 2. Compute plume rise for active sources
call plume_rise%compute_all_sources(met_fields, elevated_container, rc)

! 3. Distribute emissions vertically using computed heights
call emission_distributor%apply_injection_profiles(elevated_container, emissions, rc)
```

## File Format Support

### Fire Data Files
```netcdf
// Example fire data NetCDF structure
dimensions:
    time = UNLIMITED ;
    fire_source = 10000 ;

variables:
    double time(time) ;
        time:units = "hours since 2024-01-01 00:00:00" ;

    double latitude(fire_source) ;
    double longitude(fire_source) ;

    float frp(time, fire_source) ;
        frp:units = "MW" ;
        frp:long_name = "Fire Radiative Power" ;

    float burn_area(time, fire_source) ;
        burn_area:units = "m2" ;

    char fire_type(fire_source, 32) ;
```

### Stack Data Files
```netcdf
dimensions:
    stack_source = 5000 ;

variables:
    double latitude(stack_source) ;
    double longitude(stack_source) ;

    float stack_height(stack_source) ;
        stack_height:units = "m" ;

    float stack_diameter(stack_source) ;
        stack_diameter:units = "m" ;

    float exit_velocity(stack_source) ;
        exit_velocity:units = "m/s" ;

    float exit_temperature(stack_source) ;
        exit_temperature:units = "K" ;
```

## Benefits of This Approach

### 1. **Clear Separation of Concerns**
- External Data Process: File I/O, data management, temporal scaling
- Plume Rise Process: Physics calculations, meteorological dependencies
- Driver: Coordination, memory management, error handling

### 2. **Flexibility**
- Different plume rise schemes for different source types
- Configurable data sources and file formats
- Optional plume rise (can use fixed heights if needed)

### 3. **Performance**
- Processes can be optimized independently
- Caching of expensive plume rise calculations
- Efficient spatial indexing of sources

### 4. **Maintainability**
- Each process has focused responsibilities
- Clear interfaces between processes
- Independent testing and validation

### 5. **Extensibility**
- Easy to add new source types
- New plume rise schemes can be added
- Support for new file formats

## Future Enhancements

### 1. **Advanced Fire Modeling**
- Integration with fire behavior models
- Fuel consumption calculations
- Fire weather dependencies

### 2. **Complex Plume Rise**
- 3D plume trajectory models
- Plume merging and interactions
- Chemical transformation in plumes

### 3. **Data Assimilation**
- Real-time fire detection integration
- Satellite data ingestion
- Dynamic source parameter updates

This architecture provides a clean separation between data management and computation while allowing the necessary cooperation for handling the crossover between external data and plume rise calculations.

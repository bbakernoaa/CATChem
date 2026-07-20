# External Emission Data + Plume Rise: Complete Solution

## What We've Created

We've successfully addressed your question about plume rise and external data dependencies by creating **two separate but cooperative processes** using the CATChem process generator.

## Process 1: External Emission Data Process

### Purpose
Manages external emission inventory data from files including fire parameters and stack characteristics.

### Key Features
- **Data Management**: Loads, validates, and provides access to external emission data
- **Three Handler Schemes**:
  - `surface_2d`: Standard 2D surface emission inventories
  - `elevated_3d`: 3D elevated emissions with vertical distribution
  - `temporal_profiles`: Time-varying emission profiles and scaling factors

### Fire Data Support
- Fire Radiative Power (FRP) from satellite data
- Burn area information
- Fire location and timing
- Temporal scaling factors

### Stack Data Support
- Industrial stack parameters (height, diameter, exit conditions)
- Point source locations
- Emission rates by sector

## Process 2: Plume Rise Process

### Purpose
Computes effective emission heights for elevated sources using meteorological data and source parameters.

### Key Features
- **Physics-Based Calculations**: Real-time plume rise computation
- **Three Computational Schemes**:
  - `briggs_fires`: Briggs formulation for wildfire emissions using FRP
  - `briggs_stacks`: Briggs formulation for industrial point sources
  - `freitas_fires`: Advanced 1D plume model for large fires

### Meteorological Dependencies
- Wind speed and direction
- Temperature profiles
- Atmospheric stability
- Boundary layer height
- Humidity (for advanced schemes)

## How They Work Together

### 1. **Data Flow**
```
Fire/Stack Files → External Data Process → Source Parameters
                                              ↓
Meteorological Data → Plume Rise Process → Effective Heights
                                              ↓
                     Emission Distribution → Chemical Transport Model
```

### 2. **Cooperative Architecture**
- **External Data Process** provides fire FRP, stack parameters, and source locations
- **Plume Rise Process** uses this data plus meteorology to compute injection heights
- **Driver** coordinates both processes and manages shared data

### 3. **Shared Data Structures**
```fortran
type :: fire_source_data_t
   real(fp) :: latitude, longitude
   real(fp) :: frp                   ! Fire Radiative Power [MW]
   real(fp) :: burn_area             ! Burned area [m²]
   real(fp) :: fuel_load             ! Fuel loading [kg/m²]
   character(len=32) :: fire_type    ! 'wildfire', 'prescribed', etc.
end type

type :: stack_source_data_t
   real(fp) :: latitude, longitude
   real(fp) :: stack_height          ! Physical stack height [m]
   real(fp) :: stack_diameter        ! Stack diameter [m]
   real(fp) :: exit_velocity         ! Exit velocity [m/s]
   real(fp) :: exit_temperature      ! Exit temperature [K]
end type
```

## Addressing the Crossover Problem

### Your Insight Was Correct
Plumerise **does** depend on external input files:
- **Fire data**: FRP, burn area, fuel characteristics from satellite/ground observations
- **Stack data**: Physical and operational parameters from regulatory databases
- **Terrain data**: Surface elevation affecting plume development

### Our Solution
**Separate but cooperative processes** that each handle their domain expertise:

#### External Data Process Handles:
- ✅ File I/O and data management
- ✅ Data validation and quality control
- ✅ Temporal interpolation and scaling
- ✅ Spatial interpolation of source parameters
- ✅ Source classification and categorization

#### Plume Rise Process Handles:
- ✅ Physics-based plume rise calculations
- ✅ Meteorological dependencies
- ✅ Real-time atmospheric stability effects
- ✅ Complex plume development algorithms
- ✅ Injection height distribution

### Benefits of This Approach

1. **Clear Separation of Concerns**
   - Data management vs. physics computation
   - File I/O vs. meteorological calculations
   - Static parameters vs. dynamic conditions

2. **Flexibility**
   - Can use external data without plume rise (fixed heights)
   - Can use plume rise with different data sources
   - Easy to swap schemes independently

3. **Maintainability**
   - Each process has focused responsibilities
   - Independent testing and validation
   - Clear interfaces between components

4. **Performance**
   - Optimize data access separately from computations
   - Cache expensive plume rise calculations
   - Parallel processing where appropriate

## Configuration Example

### External Data Configuration
```yaml
external_emission_data:
  schemes:
    - name: fire_data_handler
      parameters:
        frp_threshold: 1.0  # MW
        temporal_profiles: true
    - name: stack_data_handler
      parameters:
        minimum_height: 10.0  # m
```

### Plume Rise Configuration
```yaml
plume_rise:
  schemes:
    - name: briggs_fires
      parameters:
        frp_to_heat_efficiency: 0.3
        buoyancy_efficiency: 0.9
    - name: briggs_stacks
      parameters:
        building_downwash: true
```

## Generated Files

### External Emission Data Process
- `src/process/external_emission_data/ProcessExternalEmissionDataInterface_Mod.F90`
- `src/process/external_emission_data/schemes/ExternalEmissionDataScheme_Surface2D_Mod.F90`
- `src/process/external_emission_data/schemes/ExternalEmissionDataScheme_Elevated3D_Mod.F90`
- `src/process/external_emission_data/schemes/ExternalEmissionDataScheme_TemporalProfiles_Mod.F90`

### Plume Rise Process
- `src/process/plume_rise/ProcessPlumeRiseInterface_Mod.F90`
- `src/process/plume_rise/schemes/PlumeRiseScheme_BriggsFires_Mod.F90`
- `src/process/plume_rise/schemes/PlumeRiseScheme_BriggsStacks_Mod.F90`
- `src/process/plume_rise/schemes/PlumeRiseScheme_FreitasFires_Mod.F90`

### Documentation
- `docs/processes/external_emission_data/`
- `docs/processes/plume_rise/`
- `docs/processes/PROCESS_INTEGRATION_DESIGN.md`

## Next Steps

### 1. **Integration with Existing Code**
- Update CMakeLists.txt to include both processes
- Integrate with ProcessFactory
- Add driver coordination logic

### 2. **Data Structure Enhancement**
- Implement shared elevated source container
- Add fire/stack parameter data structures
- Create process communication interfaces

### 3. **Real Implementation**
- Replace template algorithms with actual Briggs/Freitas formulations
- Add real fire data file readers (FIRMS, MODIS, VIIRS)
- Implement stack parameter database readers

### 4. **Testing and Validation**
- Unit tests for individual schemes
- Integration tests for process cooperation
- Validation against known plume rise datasets

## Conclusion

This solution elegantly handles the crossover between external data and plume rise by:

1. **Recognizing the crossover** and designing cooperative processes
2. **Separating concerns** while enabling necessary data sharing
3. **Using the process generator** to create standardized, maintainable code
4. **Providing flexibility** for different data sources and calculation methods
5. **Setting up for future enhancement** with more sophisticated models

The architecture acknowledges that plume rise calculations need external data (FRP, stack parameters) while keeping the responsibilities clear and the code maintainable.

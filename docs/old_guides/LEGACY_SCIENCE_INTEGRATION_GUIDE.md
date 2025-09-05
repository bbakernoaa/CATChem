# Legacy Science Module Integration Guide

## Overview

This guide demonstrates how to integrate existing science modules (like the Fengsha dust scheme and DustCommon utilities) into the new CATChem process architecture. The key principles are:

1. **Process Interface Handles Framework Interactions**: The process interface manages all StateManager interactions, diagnostics, and error handling
2. **Science Routines Keep Simple Interfaces**: Legacy science codes receive only the data they need without StateContainer dependencies
3. **Clear Separation of Concerns**: Framework logic stays in the interface, science logic stays in scheme modules
4. **Minimal Changes to Science Code**: Existing science routines require minimal or no changes

## Architecture Overview

```
┌─────────────────────────────────────────┐
│          ProcessManager                 │
│  (Framework orchestration)              │
└─────────────┬───────────────────────────┘
              │
┌─────────────▼───────────────────────────┐
│     Process{Name}Interface_Mod          │
│  • StateManager interactions           │
│  • Diagnostics management              │
│  • Error handling & validation         │
│  • Input/output data marshaling        │
└─────────────┬───────────────────────────┘
              │
┌─────────────▼───────────────────────────┐
│     Legacy Science Modules              │
│  • {Scheme}Scheme_Mod (e.g., Fengsha)  │
│  • {Common}_Mod (e.g., DustCommon)     │
│  • Pure science calculations           │
│  • Simple, scalar/1D interfaces        │
└─────────────────────────────────────────┘
```

## Integration Components

### 1. Process Interface Template (`process_main_with_legacy_integration.f90.j2`)

The enhanced template includes several key methods for legacy integration:

#### Core Integration Methods:
- `call_legacy_scheme()`: Orchestrates calling legacy science code
- `extract_inputs_for_scheme()`: Extracts data from StateManager for science routines
- `apply_scheme_outputs()`: Applies science results back to StateManager

#### Framework Methods:
- `register_diagnostics()`: Sets up automatic and process-specific diagnostics
- `update_diagnostics()`: Updates diagnostic fields
- `validate_inputs()` & `validate_config()`: Input validation and error checking

### 2. Legacy Science Modules

#### Fengsha Scheme (`DustScheme_Fengsha_Mod.F90`)
```fortran
! Simple, StateContainer-independent interface
subroutine run_fengsha_scheme(n_dust_species, &
                             ! Meteorological inputs (scalars)
                             wind_friction_velocity, wind_speed_10m, surface_temperature, &
                             air_density, soil_moisture_top, surface_roughness, &
                             ! Surface properties (scalars)
                             clay_fraction, sand_fraction, soil_type, &
                             ocean_fraction, land_ice_fraction, snow_fraction, &
                             sediment_supply_map, roughness_drag, &
                             ! Size distribution (1D arrays)
                             effective_radius, lower_radius, upper_radius, &
                             ! Configuration parameters
                             alpha_scale, beta_scale, moist_opt, drag_opt, horiz_flux_opt, &
                             ! Outputs (1D array for size bins)
                             total_emission, emission_per_species, rc)
```

#### DustCommon Utilities (`DustCommon_Mod.F90`)
```fortran
! Shared utility functions used by multiple schemes
public :: Fecan_SoilMoisture, Shao_SoilMoisture, KokDistribution
public :: Soil_Erosion_Potential, Draxler_HorizFlux, Kawamura_HorizFlux
public :: MB95_DragPartition, MB97_threshold_velocity
```

## Integration Workflow

### 1. Data Flow: StateManager → Legacy Science

```fortran
! Extract inputs from StateManager
call this%extract_inputs_for_scheme(chem_state, met_state, grid_state, icol, &
                                   n_dust_species, dust_species_indices, dust_species_names, &
                                   wind_friction_velocity, wind_speed_10m, surface_temperature, &
                                   air_density, soil_moisture_top, surface_roughness, &
                                   clay_fraction, sand_fraction, soil_type, &
                                   ocean_fraction, land_ice_fraction, snow_fraction, &
                                   sediment_supply_map, roughness_drag, &
                                   effective_radius, lower_radius, upper_radius, rc)
```

Key features:
- **Column-by-column processing**: Framework loops over columns, science works on scalars
- **Automatic species selection**: Uses `get_species_by_attribute('is_dust', .true.)`
- **Unit conversion**: Framework handles any needed unit conversions
- **Error handling**: All StateManager calls include error checking

### 2. Science Calculation: Legacy Scheme Execution

```fortran
! Call legacy science routine with simple interface
call run_fengsha_scheme(n_dust_species, &
                       wind_friction_velocity, wind_speed_10m, surface_temperature, &
                       air_density, soil_moisture_top, surface_roughness, &
                       clay_fraction, sand_fraction, soil_type, &
                       ocean_fraction, land_ice_fraction, snow_fraction, &
                       sediment_supply_map, roughness_drag, &
                       effective_radius, lower_radius, upper_radius, &
                       this%alpha_scale, this%beta_scale, &
                       this%moist_opt, this%drag_opt, this%horiz_flux_opt, &
                       total_emission, emission_per_species, rc)
```

Key features:
- **Simple interface**: Only fundamental physics variables, no framework types
- **Scheme selection**: Framework handles scheme switching via select case
- **Parameter access**: Configuration parameters accessed as `this%param_name`
- **Error propagation**: Science routines return error codes

### 3. Data Flow: Legacy Science → StateManager

```fortran
! Apply outputs back to chemistry state
call this%apply_scheme_outputs(chem_state, icol, dt, &
                              n_dust_species, dust_species_indices, &
                              total_emission, emission_per_species, rc)
```

Key features:
- **Tendency updates**: Adds emission rates to existing tendencies
- **Unit conversion**: Converts science outputs to framework units (μg/m²/s → kg/kg/s)
- **Diagnostic updates**: Automatically updates emission diagnostics
- **Memory management**: Cleans up temporary arrays

## Configuration Integration

### YAML Configuration (`dust_emission_config.yaml`)
```yaml
# Schemes available for this process
schemes:
  - "Fengsha"      # FENGSHA dust emission scheme
  - "Ginoux"       # Ginoux et al. scheme

# Species selection (automatic)
species_selection:
  is_dust: true    # Select all species with is_dust=true

# Process parameters (accessible as this%param_name)
parameters:
  alpha_scale:
    type: "real(fp)"
    default: 1.0
    description: "Alpha scaling factor for dust emissions"
    min: 0.1
    max: 10.0

  beta_scale:
    type: "real(fp)"
    default: 1.0
    description: "Beta scaling factor for dust emissions"
    min: 0.1
    max: 10.0
```

## Diagnostics Integration

### Automatic Diagnostics
The framework automatically creates:
- **Tendency diagnostics**: `dust_tendency_<species>` for each dust species
- **Emission diagnostics**: `dust_emission_<species>` for each dust species

### Process-Specific Diagnostics
Additional diagnostics defined in config:
```yaml
process_diagnostics:
  dust_total_emission:
    description: "Total dust emission flux"
    units: "kg/m²/s"
    dimensions: 2

  dust_threshold_velocity:
    description: "Threshold friction velocity for dust emission"
    units: "m/s"
    dimensions: 2
```

## Error Handling Integration

### Framework-Level Error Handling
```fortran
! All StateManager interactions include error checking
chem_state => state_manager%get_chem_state(rc)
if (rc /= 0) then
   call error_handler(routine, 'Failed to get chemistry state', rc)
   return
end if
```

### Science-Level Error Handling
```fortran
! Legacy schemes return error codes
call run_fengsha_scheme(..., rc)
if (rc /= 0) then
   call error_handler(routine, 'Legacy scheme execution failed', rc)
   return
end if
```

### Validation Integration
```fortran
! Input validation before science calculation
call this%validate_inputs(chem_state, met_state, rc)
if (rc /= 0) then
   call error_handler(routine, 'Input validation failed', rc)
   return
end if
```

## Best Practices for Integration

### 1. Keep Science Modules Framework-Independent
- Science routines should not `use` any CATChem framework modules
- Use simple Fortran types: `real(fp)`, `integer`, arrays
- Return error codes rather than calling framework error handlers

### 2. Handle All Framework Interactions in Process Interface
- StateManager access, diagnostics, error handling in interface only
- Science modules focus purely on calculations
- Framework handles memory management for interface arrays

### 3. Use Generalized Species Selection
- Use attributes like `is_dust`, `is_seasalt` rather than hardcoded species
- Allow science to work with any number of species in category
- Framework automatically finds and validates species

### 4. Maintain Backward Compatibility
- Existing science routines should require minimal changes
- Add new interfaces rather than modifying existing ones
- Preserve original science algorithm logic

### 5. Comprehensive Error Handling
- Validate all inputs before calling science routines
- Check error codes from all StateManager operations
- Provide meaningful error messages with context

## CMakeLists.txt Configuration

### Process Library Dependencies
```cmake
# Process libraries depend only on CATChem_core
target_link_libraries(dust_process
  PRIVATE
    CATChem_core
    ${NETCDF_LIBRARIES}
)

# Process libraries do NOT depend on other processes
# ProcessManager depends on process libraries, not vice versa
```

### Build Order
1. **CATChem_core**: Core framework types and utilities
2. **Process libraries**: Individual process implementations (dust, seasalt, etc.)
3. **ProcessManager**: Orchestrates all processes

This ensures proper dependency flow and avoids circular dependencies.

## Testing Strategy

### Unit Testing Legacy Science
```fortran
! Test science routines with simple inputs
call run_fengsha_scheme(test_inputs..., rc)
assert(rc == 0)
assert(emission_output > 0.0_fp)
```

### Integration Testing
```fortran
! Test full process interface
call dust_interface%run(state_manager, dt, rc)
assert(rc == 0)
assert(tendencies_updated)
assert(diagnostics_available)
```

### Validation Testing
```fortran
! Test with realistic meteorological data
! Compare outputs with reference solutions
! Verify conservation and physical bounds
```

## Migration Path for Existing Science

### Step 1: Extract Science Routine
- Copy existing scheme to new `{Scheme}Scheme_Mod.F90`
- Remove StateContainer dependencies
- Simplify interface to use scalar/1D inputs
- Add error return code

### Step 2: Create Process Interface
- Use the integration template
- Implement `extract_inputs_for_scheme()`
- Implement `apply_scheme_outputs()`
- Add scheme to `call_legacy_scheme()` select case

### Step 3: Update Configuration
- Add scheme to YAML config `schemes:` list
- Define process parameters in `parameters:` section
- Specify diagnostics in `process_diagnostics:` section

### Step 4: Test and Validate
- Generate process with `catchem_generate_process.py`
- Build and test with framework
- Validate against existing results

This approach allows gradual migration of legacy science while maintaining compatibility and ensuring robust framework integration.

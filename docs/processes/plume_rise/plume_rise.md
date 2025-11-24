# PlumeRise Process

**Process Type:** Emission
**Description:** Plume rise process for computing effective emission heights of elevated sources. Handles fire emissions, industrial stacks, and other elevated point sources. Works cooperatively with external emission data process for source parameters.
**Author:** CATChem Development Team
**Generated:** 2025-07-07T15:59:26.947386

## Overview

The PlumeRise process implements Plume rise process for computing effective emission heights of elevated sources. Handles fire emissions, industrial stacks, and other elevated point sources. Works cooperatively with external emission data process for source parameters.. This process provides a modular, extensible framework for emission calculations within the CATChem chemical transport model.

## Available Schemes

### BriggsFires Scheme

**Name:** `briggs_fires`
**Description:** Briggs plume rise formulation for wildfire and prescribed burning
**Author:** CATChem Development Team

#### Parameters

| Parameter | Default | Range | Description |
|-----------|---------|--------|-------------|
| `fire_frp_threshold` |  |  -  |  |
| `buoyancy_efficiency` |  |  -  |  |
| `entrainment_constant` |  |  -  |  |
| `wind_speed_min` |  |  -  |  |
| `atmospheric_stability_method` |  |  -  |  |

#### Required Meteorological Fields

- `wind_speed` - Meteorological field required for scheme computation
- `temperature` - Meteorological field required for scheme computation
- `pressure` - Meteorological field required for scheme computation
- `boundary_layer_height` - Meteorological field required for scheme computation

#### Available Diagnostics

| Diagnostic | Units | Description |
|------------|-------|-------------|
| `effective_height` | m | Computed effective emission height |
| `plume_buoyancy_flux` | m4/s3 | Buoyancy flux at source |
| `stability_parameter` | s-2 | Atmospheric stability parameter |

### BriggsStacks Scheme

**Name:** `briggs_stacks`
**Description:** Briggs plume rise formulation for industrial point sources
**Author:** CATChem Development Team

#### Parameters

| Parameter | Default | Range | Description |
|-----------|---------|--------|-------------|
| `momentum_efficiency` |  |  -  |  |
| `buoyancy_efficiency` |  |  -  |  |
| `building_downwash` |  |  -  |  |
| `terrain_following` |  |  -  |  |

#### Required Meteorological Fields

- `wind_speed` - Meteorological field required for scheme computation
- `temperature` - Meteorological field required for scheme computation
- `pressure` - Meteorological field required for scheme computation
- `atmospheric_stability` - Meteorological field required for scheme computation

#### Available Diagnostics

| Diagnostic | Units | Description |
|------------|-------|-------------|
| `effective_height` | m | Effective emission height including stack height |
| `momentum_flux` | m4/s2 | Momentum flux at stack exit |
| `buoyancy_flux` | m4/s3 | Buoyancy flux at stack exit |

### FreitasFires Scheme

**Name:** `freitas_fires`
**Description:** Freitas 1D plume rise model for large fires
**Author:** CATChem Development Team

#### Parameters

| Parameter | Default | Range | Description |
|-----------|---------|--------|-------------|
| `vertical_resolution` |  |  -  |  |
| `max_height` |  |  -  |  |
| `entrainment_alpha` |  |  -  |  |
| `moisture_effects` |  |  -  |  |

#### Required Meteorological Fields

- `wind_speed` - Meteorological field required for scheme computation
- `temperature` - Meteorological field required for scheme computation
- `pressure` - Meteorological field required for scheme computation
- `relative_humidity` - Meteorological field required for scheme computation
- `vertical_velocity` - Meteorological field required for scheme computation

#### Available Diagnostics

| Diagnostic | Units | Description |
|------------|-------|-------------|
| `injection_height` | m | Final injection height distribution |
| `plume_top_height` | m | Maximum plume height reached |
| `water_vapor_flux` | kg/m2/s | Water vapor emission flux |


## Process Interface

### Species

The plume_rise process operates on the following chemical species:


### Required Inputs

#### Meteorological Fields
- `wind_speed` - Required meteorological input
- `temperature` - Required meteorological input
- `pressure` - Required meteorological input


### Process Diagnostics

| Diagnostic | Units | Description |
|------------|-------|-------------|
| `total_elevated_sources` | count | Number of active elevated emission sources |
| `avg_injection_height` | m | Average injection height across all sources |
| `max_injection_height` | m | Maximum injection height computed |
| `plume_rise_computation_time` | s | CPU time for plume rise calculations |
| `unstable_atmosphere_fraction` | dimensionless | Fraction of domain with unstable atmospheric conditions |

## Usage

### Basic Integration

```fortran
use PlumeRiseProcessCreator_Mod
use PlumeRiseCommon_Mod

! Create process instance
type(PlumeRiseProcess_t) :: process
call create_plume_rise_process(process, config_data)

! Use process in model time step
call process%run(state, dt)
```

### Scheme Selection

The process supports multiple schemes. Select your desired scheme:

```fortran
! Use BriggsFires scheme
process%scheme_name = "briggs_fires"
```
```fortran
! Use BriggsStacks scheme
process%scheme_name = "briggs_stacks"
```
```fortran
! Use FreitasFires scheme
process%scheme_name = "freitas_fires"
```

## Implementation Details

### Pure Science Kernels

Each scheme is implemented as a pure science kernel with no infrastructure dependencies:

```fortran
! BriggsFires scheme
pure subroutine compute_briggs_fires( &
   num_layers, num_species, params, &
   wind_speed, &   temperature, &   pressure, &   boundary_layer_height, &
   species_conc, emission_flux)
```
```fortran
! BriggsStacks scheme
pure subroutine compute_briggs_stacks( &
   num_layers, num_species, params, &
   wind_speed, &   temperature, &   pressure, &   atmospheric_stability, &
   species_conc, emission_flux)
```
```fortran
! FreitasFires scheme
pure subroutine compute_freitas_fires( &
   num_layers, num_species, params, &
   wind_speed, &   temperature, &   pressure, &   relative_humidity, &   vertical_velocity, &
   species_conc, emission_flux)
```

### Host Model Responsibilities

The host model (CATChem infrastructure) handles:

- Parameter initialization and validation
- Input array validation and error handling
- Memory management and array allocation
- Integration with model time stepping
- Diagnostic output management

## Configuration

### YAML Configuration Example

```yaml
processes:
  plume_rise:
    enabled: true
    scheme: "briggs_fires"
    parameters:
      fire_frp_threshold:
      buoyancy_efficiency:
      entrainment_constant:
      wind_speed_min:
      atmospheric_stability_method:
    diagnostics:
      enabled: true
      output_frequency: "daily"
```

## Technical Specifications

- **Parallelization:** Column
- **Memory Requirements:** Low
- **Timestep Dependency:** Dependent
- **Multiphase Support:** No
- **Size Bin Support:** No
- **Vectorization:** Supported

## Files Generated

### Source Code
- `src/process/plume_rise/ProcessPlumeRiseInterface_Mod.F90` - Main process interface
- `src/process/plume_rise/PlumeRiseCommon_Mod.F90` - Common types and parameters
- `src/process/plume_rise/PlumeRiseProcessCreator_Mod.F90` - Process factory
- `src/process/plume_rise/schemes/PlumeRiseScheme_BriggsFires_Mod.F90` - Briggs plume rise formulation for wildfire and prescribed burning
- `src/process/plume_rise/schemes/PlumeRiseScheme_BriggsStacks_Mod.F90` - Briggs plume rise formulation for industrial point sources
- `src/process/plume_rise/schemes/PlumeRiseScheme_FreitasFires_Mod.F90` - Freitas 1D plume rise model for large fires

### Tests
- `tests/process/plume_rise/unit/` - Unit tests
- `tests/process/plume_rise/integration/` - Integration tests

### Documentation
- `docs/processes/plume_rise/plume_rise.md` - This documentation

## Contributing

When modifying or extending this process:

1. **Science Changes:** Modify the scheme modules in `schemes/`
2. **Interface Changes:** Update the main interface module
3. **New Schemes:** Add new scheme modules and update the creator
4. **Tests:** Add corresponding unit and integration tests
5. **Documentation:** Update this documentation file

## References


---
*This documentation was automatically generated by the CATChem Process Generator on 2025-07-07T15:59:26.947386*

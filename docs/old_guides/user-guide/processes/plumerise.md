# Plume Rise Process

The plume rise process in CATChem simulates the vertical transport of emissions from elevated point sources, including power plants, industrial facilities, and wildfire plumes.

## Overview

Plume rise is essential for:
- **Emission distribution**: Vertical allocation of point source emissions
- **Air quality modeling**: Accurate representation of stack emissions
- **Wildfire modeling**: Smoke injection height determination
- **Dispersion modeling**: Initial plume characteristics

## Process Description

### Physical Mechanisms

1. **Buoyancy-driven rise**: Thermal effects from hot exhaust
2. **Momentum-driven rise**: Initial vertical velocity from stack
3. **Atmospheric stability**: Environmental lapse rate effects
4. **Wind shear**: Horizontal transport and plume bending
5. **Precipitation scavenging**: Wet removal during rise

### Mathematical Formulation

Plume rise height follows the Briggs formulation:

```
Δh = f(F, u, s, h_mix)
```

Where:
- `Δh`: Plume rise height (m)
- `F`: Buoyancy flux (m⁴/s³)
- `u`: Wind speed (m/s)
- `s`: Atmospheric stability parameter
- `h_mix`: Mixing height (m)

## Supported Schemes

### Briggs Plume Rise

Default scheme based on Briggs (1984):

```yaml
plume_rise:
  scheme: "briggs"
  parameters:
    buoyancy_flux_calculation: true
    wind_speed_dependence: true
    stability_correction: true
    mixing_height_limit: true
```

**Features**:
- Separate formulations for stable/unstable conditions
- Wind speed dependent rise
- Mixing height constraints
- Gradual vs final rise options

### SMOKE Plume Rise

Integration with SMOKE emission processing:

```yaml
plume_rise:
  scheme: "smoke"
  parameters:
    stack_parameters: true
    temporal_variation: true
    multi_stack_facilities: true
    fire_specific_parameters: true
```

**Features**:
- Stack-specific parameters
- Temporal emission variations
- Multiple stack facilities
- Fire-specific formulations

## Configuration

### Basic Setup

```yaml
processes:
  - name: "plume_rise"
    type: "emission"
    scheme: "briggs"
    parameters:
      point_source_file: "point_sources.nc"
      stack_parameters_file: "stack_params.nc"
      fire_emissions_file: "fire_emissions.nc"
```

### Point Source Configuration

```yaml
point_sources:
  - facility_id: "power_plant_001"
    location: [lon, lat]
    stack_height: 150.0              # m
    stack_diameter: 4.0              # m
    exit_velocity: 15.0              # m/s
    exit_temperature: 450.0          # K
    emission_rate: 1000.0            # kg/hr
    species: ["SO2", "NOx", "PM2.5"]

  - facility_id: "industrial_002"
    location: [lon, lat]
    stack_height: 80.0               # m
    stack_diameter: 2.5              # m
    exit_velocity: 10.0              # m/s
    exit_temperature: 350.0          # K
    emission_rate: 500.0             # kg/hr
    species: ["VOC", "CO", "NH3"]
```

### Wildfire Configuration

```yaml
fire_emissions:
  plume_rise_method: "freitas"
  parameters:
    fire_radiative_power: true       # Use FRP for buoyancy
    convective_fraction: 0.8         # Fraction of emissions in plume
    smoldering_fraction: 0.2         # Near-surface emissions
    injection_height_distribution: "uniform"

  # Size-dependent parameters
  fire_size_classes:
    - size_range: [0, 100]           # hectares
      injection_height: [0, 500]     # m
      convective_fraction: 0.5

    - size_range: [100, 1000]        # hectares
      injection_height: [500, 2000]  # m
      convective_fraction: 0.8

    - size_range: [1000, 10000]      # hectares
      injection_height: [2000, 8000] # m
      convective_fraction: 0.9
```

### Advanced Parameters

```yaml
plume_rise:
  scheme: "briggs"

  # Buoyancy calculation
  buoyancy:
    method: "temperature_difference"
    ambient_temperature_profile: true
    stack_gas_properties: true
    heat_capacity_ratio: 1.0

  # Stability parameterization
  stability:
    method: "richardson_number"
    critical_richardson: 0.25
    wind_speed_height: 10.0          # m
    temperature_gradient_method: "lapse_rate"

  # Numerical parameters
  numerical:
    integration_method: "analytical"
    vertical_resolution: 10.0        # m
    maximum_rise_iterations: 100
    convergence_tolerance: 1.0       # m

  # Environmental limits
  limits:
    minimum_wind_speed: 1.0          # m/s
    maximum_rise_height: 3000.0      # m
    mixing_height_fraction: 0.8      # Fraction of mixing height
```

## Input Requirements

### Stack Parameters

Required for each point source:

| Parameter | Units | Description |
|-----------|-------|-------------|
| `stack_height` | m | Physical stack height |
| `stack_diameter` | m | Internal stack diameter |
| `exit_velocity` | m/s | Exhaust velocity |
| `exit_temperature` | K | Exhaust temperature |
| `emission_rate` | kg/s | Mass emission rate |
| `location` | degrees | Longitude/latitude |

### Meteorological Data

Required atmospheric conditions:

| Field | Units | Description |
|-------|-------|-------------|
| `wind_speed` | m/s | Wind speed profile |
| `wind_direction` | degrees | Wind direction |
| `temperature` | K | Temperature profile |
| `pressure` | Pa | Pressure profile |
| `mixing_height` | m | Planetary boundary layer height |
| `surface_heat_flux` | W/m² | Surface sensible heat flux |

### Fire-Specific Data

For wildfire applications:

| Field | Units | Description |
|-------|-------|-------------|
| `fire_radiative_power` | MW | Fire radiative power |
| `burned_area` | m² | Fire burned area |
| `fuel_consumption` | kg/m² | Fuel consumption rate |
| `fire_duration` | hours | Fire duration |
| `fire_type` | - | Vegetation type |

## Output Variables

### Plume Characteristics

| Variable | Units | Description |
|----------|-------|-------------|
| `plume_rise_height` | m | Final plume rise height |
| `effective_stack_height` | m | Stack height + plume rise |
| `buoyancy_flux` | m⁴/s³ | Buoyancy flux parameter |
| `momentum_flux` | m⁴/s² | Momentum flux parameter |
| `plume_bottom_height` | m | Bottom of plume injection |
| `plume_top_height` | m | Top of plume injection |

### Injection Profiles

```yaml
diagnostics:
  plume_injection:
    - variable: "vertical_emission_profile"
      units: "kg/m/s"
      description: "Vertical emission distribution"

    - variable: "injection_layer_fractions"
      units: "-"
      description: "Fraction of emissions by layer"

    - variable: "effective_injection_height"
      units: "m"
      description: "Mass-weighted injection height"
```

### Quality Control

```yaml
quality_control:
  checks:
    - "plume_rise_reasonable"         # < 3 km typically
    - "buoyancy_flux_positive"        # Physical constraint
    - "stack_parameters_valid"        # Reasonable values
    - "meteorology_available"         # Required fields present
```

## Scheme Formulations

### Briggs Formulations

#### Unstable Conditions (Ri < 0)

```
Δh = 1.6 * F^(1/3) * x^(2/3) / u
```

#### Stable Conditions (Ri > 0)

```
Δh = 2.6 * (F/(u*s))^(1/3)
```

#### Neutral Conditions

```
Δh = 1.6 * F^(1/3) * x^(2/3) / u
```

Where:
- `F`: Buoyancy flux = g * v_s * r² * (T_s - T_a) / T_s
- `x`: Downwind distance
- `u`: Wind speed
- `s`: Stability parameter

### Freitas Fire Formulation

For wildfire plumes:

```
H_inj = α * (FRP)^β * (H_pbl)^γ
```

Where:
- `H_inj`: Injection height
- `FRP`: Fire radiative power
- `H_pbl`: Boundary layer height
- `α`, `β`, `γ`: Empirical parameters

## Validation and Evaluation

### Observational Data

- **Lidar measurements**: Plume height observations
- **Aircraft sampling**: In-plume measurements
- **Satellite observations**: Smoke plume heights
- **Ground-based remote sensing**: Plume tracking

### Benchmark Cases

```yaml
validation_cases:
  - name: "power_plant_plumes"
    observations: "EPA_plume_heights.nc"
    metrics: ["bias", "rmse", "correlation"]

  - name: "wildfire_injection"
    observations: "MISR_plume_heights.nc"
    metrics: ["injection_height", "plume_top"]

  - name: "industrial_stacks"
    observations: "facility_measurements.nc"
    metrics: ["effective_stack_height"]
```

## Performance Considerations

### Computational Efficiency

```yaml
performance:
  optimization:
    analytical_solutions: true       # Use analytical formulas
    lookup_tables: true              # Cache stability parameters
    vectorization: true              # Process multiple sources

  memory:
    stack_data_cache: 1000          # Number of cached stacks
    meteorology_buffer: 100         # Timesteps of met data
```

### Parallel Processing

- Independent calculation per point source
- Efficient memory layout for vectorization
- Minimal communication requirements

## Troubleshooting

### Common Issues

1. **Unrealistic plume heights**:
   ```yaml
   # Check input parameters
   max_plume_rise: 2000.0    # m
   min_buoyancy_flux: 1.0    # m⁴/s³
   ```

2. **Numerical instabilities**:
   ```yaml
   # Improve numerical stability
   min_wind_speed: 0.5       # m/s
   stability_damping: true
   ```

3. **Missing meteorological data**:
   ```yaml
   # Use default profiles
   default_temperature_profile: true
   default_wind_profile: true
   ```

### Diagnostic Tools

```bash
# Analyze plume rise calculations
catchem_plume_analysis --input point_sources.nc --output plume_analysis.nc

# Validate against observations
python validate_plume_rise.py --model output.nc --obs plume_heights.nc
```

## Scientific References

1. **Briggs, G. A.** (1984). Plume rise and buoyancy effects. *Atmospheric Science and Power Production* (pp. 327-366). US Department of Energy.

2. **Freitas, S. R., et al.** (2007). Including the sub-grid scale plume rise of vegetation fires in low resolution atmospheric transport models. *Atmospheric Chemistry and Physics*, 7(13), 3385-3398.

3. **Sofiev, M., et al.** (2012). An operational system for the assimilation of the satellite information on wild-land fires for the needs of air quality modelling and forecasting. *Atmospheric Chemistry and Physics*, 12(2), 1089-1106.

4. **Paugam, R., et al.** (2016). A review of approaches to estimate wildfire plume injection height within large-scale atmospheric chemical transport models. *Atmospheric Chemistry and Physics*, 16(2), 907-925.

## Related Documentation

- **[Emissions Processing](emissions.md)** - Emission inventory processing
- **[Vertical Mixing](verticalmixing.md)** - Atmospheric mixing processes
- **[Process Architecture](../../developer-guide/processes/architecture.md)** - Implementation details
- **[Configuration Guide](../configuration.md)** - General configuration principles

---

*The plume rise process provides critical emission distribution capabilities for point sources and wildfire modeling. For technical support, consult the [developer guide](../../developer-guide/processes/index.md).*

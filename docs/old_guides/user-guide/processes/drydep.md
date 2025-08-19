# Dry Deposition Process

The dry deposition process in CATChem simulates the removal of gaseous and particulate species from the atmosphere to the Earth's surface through dry deposition mechanisms.

## Overview

Dry deposition is a critical atmospheric removal process that affects:
- **Surface air quality**: Direct removal of pollutants
- **Ecosystem impacts**: Nutrient and pollutant deposition to vegetation and soils
- **Model mass balance**: Conservation of chemical species
- **Surface-atmosphere exchange**: Bidirectional flux processes

## Process Description

### Physical Mechanisms

The dry deposition process includes:

1. **Aerodynamic Transport**: Movement through the atmospheric boundary layer
2. **Quasi-laminar Layer Transfer**: Transport through the thin layer near the surface
3. **Surface Uptake**: Absorption or adsorption at the surface

### Resistance Analog

Dry deposition is parameterized using a resistance analog:

```
Vd = 1 / (Ra + Rb + Rc)
```

Where:
- `Ra`: Aerodynamic resistance
- `Rb`: Quasi-laminar boundary layer resistance
- `Rc`: Surface resistance
- `Vd`: Deposition velocity

## Supported Schemes

### Wesely Scheme

Default parameterization based on Wesely (1989):

```yaml
dry_deposition:
  scheme: "wesely"
  parameters:
    landuse_categories: 24
    seasonal_variation: true
    stomatal_resistance: true
    cuticular_resistance: true
```

**Features**:
- Land use specific parameterizations
- Seasonal LAI variations
- Stomatal and non-stomatal pathways
- Temperature and moisture dependencies

### Zhang Scheme (Particles)

Particle dry deposition following Zhang et al. (2001):

```yaml
dry_deposition:
  scheme: "zhang_particles"
  parameters:
    size_bins: 8
    collection_efficiency: true
    brownian_diffusion: true
    impaction: true
    interception: true
```

**Features**:
- Size-dependent deposition
- Multiple collection mechanisms
- Surface roughness effects
- Particle density considerations

## Configuration

### Basic Setup

```yaml
processes:
  - name: "dry_deposition"
    type: "loss"
    scheme: "wesely"
    species:
      - "O3"
      - "NO2"
      - "SO2"
      - "NH3"
      - "HNO3"
    parameters:
      min_deposition_velocity: 1.0e-5  # m/s
      max_deposition_velocity: 0.1     # m/s
      landuse_data: "landuse.nc"
      lai_data: "lai_monthly.nc"
```

### Advanced Configuration

```yaml
dry_deposition:
  scheme: "wesely"

  # Species-specific parameters
  species_parameters:
    O3:
      f0: 1.0        # Reactivity factor
      henry_constant: 1.0e-2
      reactivity: 1.0
    NO2:
      f0: 0.1
      henry_constant: 1.0e-2
      reactivity: 0.1
    SO2:
      f0: 0.0
      henry_constant: 1.3
      reactivity: 8.0

  # Surface parameters
  surface_parameters:
    z0_momentum: "z0m.nc"      # Surface roughness
    z0_heat: "z0h.nc"          # Scalar roughness
    landuse: "landuse.nc"      # Land use categories
    lai: "lai_monthly.nc"      # Leaf area index

  # Environmental dependencies
  environmental:
    temperature_dependence: true
    moisture_dependence: true
    solar_angle_dependence: true
    snow_correction: true
```

## Input Requirements

### Meteorological Data

Required meteorological fields:

| Field | Units | Description |
|-------|-------|-------------|
| `temperature_2m` | K | 2-meter air temperature |
| `humidity_2m` | kg/kg | 2-meter specific humidity |
| `wind_speed_10m` | m/s | 10-meter wind speed |
| `friction_velocity` | m/s | Surface friction velocity |
| `surface_pressure` | Pa | Surface pressure |
| `incoming_solar` | W/m² | Incoming solar radiation |

### Surface Data

Required surface characteristics:

| Field | Units | Description |
|-------|-------|-------------|
| `land_use_category` | - | Land use classification (1-24) |
| `surface_roughness` | m | Momentum roughness length |
| `leaf_area_index` | m²/m² | Leaf area index |
| `snow_depth` | m | Snow depth |
| `soil_moisture` | m³/m³ | Surface soil moisture |

### Species Properties

Required for each depositing species:

```yaml
species_properties:
  O3:
    molecular_weight: 48.0      # g/mol
    henry_constant: 1.0e-2      # M/atm
    reactivity_factor: 1.0      # dimensionless
    diffusivity: 1.6e-5         # m²/s
```

## Output Variables

### Diagnostic Fields

| Variable | Units | Description |
|----------|-------|-------------|
| `dry_deposition_flux` | kg/m²/s | Surface deposition flux |
| `deposition_velocity` | m/s | Bulk deposition velocity |
| `aerodynamic_resistance` | s/m | Aerodynamic resistance |
| `boundary_layer_resistance` | s/m | Quasi-laminar resistance |
| `surface_resistance` | s/m | Surface resistance |

### Integrated Outputs

```yaml
diagnostics:
  dry_deposition:
    - variable: "total_deposition"
      units: "kg/m²"
      frequency: "daily"
      description: "Daily accumulated dry deposition"

    - variable: "deposition_velocity"
      units: "m/s"
      frequency: "hourly"
      description: "Instantaneous deposition velocity"
```

## Performance Considerations

### Computational Efficiency

- **Lookup tables**: Pre-computed resistance values for common conditions
- **Vectorization**: Efficient processing of multiple species
- **Caching**: Store frequently used surface parameters

```yaml
performance:
  optimization:
    lookup_tables: true
    cache_surface_data: true
    vectorize_species: true

  memory:
    resistance_cache_size: 1000
    surface_data_buffer: 100
```

### Parallel Scalability

- Column-independent calculations
- Efficient memory access patterns
- Minimal inter-process communication

## Validation and Testing

### Observational Comparisons

The dry deposition module has been validated against:

- **CASTNET**: Clean Air Status and Trends Network
- **NADP**: National Atmospheric Deposition Program
- **European monitoring**: EMEP network observations
- **Flux tower measurements**: Eddy covariance data

### Reference Benchmarks

```bash
# Run validation test case
ctest -R test_dry_deposition_validation

# Compare with reference implementation
python validate_dry_deposition.py --reference wesely_original
```

## Troubleshooting

### Common Issues

1. **Unrealistic deposition velocities**:
   ```yaml
   # Check surface data quality
   surface_data_quality_check: true
   deposition_velocity_limits: [1e-6, 0.1]  # m/s
   ```

2. **Mass balance problems**:
   ```yaml
   # Enable conservation checks
   conservation_checks: true
   mass_balance_tolerance: 1e-12
   ```

3. **Performance issues**:
   ```yaml
   # Optimize lookup tables
   lookup_table_resolution: 50  # vs default 100
   cache_efficiency_target: 0.95
   ```

### Diagnostic Tools

```bash
# Check deposition velocity ranges
catchem_diagnose_drydep --input model_output.nc --check velocity_range

# Validate surface resistance calculations
catchem_validate_surface_resistance --landuse landuse.nc --lai lai.nc
```

## Scientific References

1. **Wesely, M. L.** (1989). Parameterization of surface resistances to gaseous dry deposition in regional-scale numerical models. *Atmospheric Environment*, 23(6), 1293-1304.

2. **Zhang, L., et al.** (2003). A size-segregated particle dry deposition scheme for an atmospheric aerosol module. *Atmospheric Environment*, 37(4), 549-560.

3. **Petroff, A., & Zhang, L.** (2010). Development and validation of a size-resolved particle dry deposition scheme for atmospheric modeling. *Geoscientific Model Development*, 3(1), 209-220.

## Related Documentation

- **[Process Architecture](../../developer-guide/processes/architecture.md)** - Understanding process implementation
- **[Configuration Guide](../configuration.md)** - General configuration principles
- **[Performance Tuning](../performance.md)** - Optimization strategies
- **[Wet Deposition Process](wetdep.md)** - Complementary removal process

---

*For technical support or questions about the dry deposition process, consult the [developer documentation](../../developer-guide/processes/index.md) or contact the development team.*

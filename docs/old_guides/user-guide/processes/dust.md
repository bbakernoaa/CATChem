# Dust Emission and Transport Process

The dust process in CATChem simulates the emission, transport, and deposition of mineral dust aerosols from natural and anthropogenic sources.

## Overview

Dust processes are critical for:
- **Radiative forcing**: Direct and indirect effects on climate
- **Air quality**: Particulate matter concentrations and visibility
- **Biogeochemical cycles**: Iron and nutrient transport
- **Human health**: Respiratory and cardiovascular impacts

## Process Description

### Physical Mechanisms

1. **Saltation**: Horizontal transport of sand particles
2. **Sandblasting**: Vertical emission of fine dust particles
3. **Aggregation**: Particle size evolution during transport
4. **Deposition**: Dry and wet removal processes

### Emission Parameterization

Dust emission flux follows the formulation:

```
F = α * S * (u* - u*t)² * (1 + u*t/u*)
```

Where:
- `F`: Dust emission flux (kg/m²/s)
- `α`: Emission factor
- `S`: Source strength function
- `u*`: Friction velocity (m/s)
- `u*t`: Threshold friction velocity (m/s)

## Supported Schemes

### GOCART Dust Scheme

Default scheme based on Ginoux et al. (2001):

```yaml
dust:
  scheme: "gocart"
  parameters:
    size_bins: 5
    source_function: "topographic"
    threshold_velocity: "iversen_white"
    emission_factor: 1.0
```

**Features**:
- Topographic source function
- Size-resolved emission and transport
- Threshold velocity parameterization
- Seasonal vegetation masking

### AFWA Dust Scheme

Air Force Weather Agency scheme:

```yaml
dust:
  scheme: "afwa"
  parameters:
    size_bins: 5
    soil_texture: true
    soil_moisture_inhibition: true
    snow_masking: true
    vegetation_masking: true
```

**Features**:
- Soil texture dependencies
- Soil moisture inhibition
- Enhanced threshold calculations
- Dust devil parameterization

## Configuration

### Basic Setup

```yaml
processes:
  - name: "dust"
    type: "emission"
    scheme: "gocart"
    size_bins: 5
    parameters:
      emission_factor: 1.0
      source_file: "dust_source.nc"
      threshold_file: "dust_threshold.nc"
      vegetation_file: "vegetation_fraction.nc"
```

### Size Bin Configuration

```yaml
dust:
  size_bins:
    - name: "dust_bin1"
      diameter_range: [0.1, 1.0]    # μm
      effective_diameter: 0.5       # μm
      density: 2650.0               # kg/m³

    - name: "dust_bin2"
      diameter_range: [1.0, 1.8]    # μm
      effective_diameter: 1.4       # μm
      density: 2650.0               # kg/m³

    - name: "dust_bin3"
      diameter_range: [1.8, 3.0]    # μm
      effective_diameter: 2.4       # μm
      density: 2650.0               # kg/m³

    - name: "dust_bin4"
      diameter_range: [3.0, 6.0]    # μm
      effective_diameter: 4.5       # μm
      density: 2650.0               # kg/m³

    - name: "dust_bin5"
      diameter_range: [6.0, 10.0]   # μm
      effective_diameter: 8.0       # μm
      density: 2650.0               # kg/m³
```

### Advanced Parameters

```yaml
dust:
  scheme: "gocart"

  # Emission parameters
  emission:
    clay_fraction_file: "clay_fraction.nc"
    sand_fraction_file: "sand_fraction.nc"
    source_strength_file: "dust_source.nc"
    alpha_coefficient: 1.0e-9         # kg*s²/m⁵

  # Threshold parameters
  threshold:
    method: "iversen_white"
    particle_diameter: 75.0           # μm (reference)
    particle_density: 2650.0          # kg/m³
    fluid_threshold: 0.2              # m/s

  # Environmental controls
  controls:
    vegetation_masking: true
    vegetation_threshold: 0.3         # fraction
    soil_moisture_inhibition: true
    moisture_threshold: 0.2           # m³/m³
    snow_masking: true
    snow_threshold: 0.001             # m

  # Transport parameters
  transport:
    gravitational_settling: true
    dry_deposition: true
    wet_deposition: true
    aggregation: false
```

## Input Requirements

### Meteorological Data

Required atmospheric fields:

| Field | Units | Description |
|-------|-------|-------------|
| `wind_speed_10m` | m/s | 10-meter wind speed |
| `friction_velocity` | m/s | Surface friction velocity |
| `boundary_layer_height` | m | Planetary boundary layer height |
| `temperature_2m` | K | 2-meter temperature |
| `relative_humidity_2m` | % | 2-meter relative humidity |
| `precipitation_rate` | mm/hr | Precipitation rate |

### Surface Data

Required surface characteristics:

| Field | Units | Description |
|-------|-------|-------------|
| `dust_source_strength` | - | Dust source function (0-1) |
| `clay_fraction` | - | Soil clay fraction (0-1) |
| `sand_fraction` | - | Soil sand fraction (0-1) |
| `vegetation_fraction` | - | Vegetation cover fraction (0-1) |
| `soil_moisture` | m³/m³ | Surface soil moisture |
| `snow_depth` | m | Snow depth |
| `surface_roughness` | m | Aerodynamic roughness length |

### Geographic Data

```yaml
geographic_data:
  topography: "topography.nc"         # Surface elevation
  land_use: "landuse.nc"              # Land use categories
  soil_texture: "soil_texture.nc"     # Soil texture classification
  arid_mask: "arid_regions.nc"        # Arid/semi-arid regions
```

## Output Variables

### Emission Diagnostics

| Variable | Units | Description |
|----------|-------|-------------|
| `dust_emission_flux` | kg/m²/s | Total dust emission flux |
| `dust_emission_bin{n}` | kg/m²/s | Size-resolved emission flux |
| `threshold_velocity` | m/s | Threshold friction velocity |
| `friction_velocity` | m/s | Actual friction velocity |
| `emission_factor` | - | Dimensionless emission factor |

### Transport Diagnostics

| Variable | Units | Description |
|----------|-------|-------------|
| `dust_concentration` | kg/kg | Dust mass mixing ratio |
| `dust_aod` | - | Dust aerosol optical depth |
| `dust_settling_flux` | kg/m²/s | Gravitational settling flux |
| `dust_column_loading` | kg/m² | Total column dust loading |

### Deposition Diagnostics

```yaml
diagnostics:
  dust_deposition:
    - variable: "dust_dry_deposition"
      units: "kg/m²/s"
      description: "Dry deposition flux"

    - variable: "dust_wet_deposition"
      units: "kg/m²/s"
      description: "Wet deposition flux"

    - variable: "dust_total_deposition"
      units: "kg/m²"
      frequency: "daily"
      description: "Daily accumulated deposition"
```

## Validation and Evaluation

### Observational Datasets

Validation against:

- **AERONET**: Aerosol optical depth measurements
- **MODIS**: Satellite aerosol retrievals
- **Surface stations**: PM10 and PM2.5 measurements
- **Dust event catalogs**: Regional dust storm databases

### Benchmark Metrics

```yaml
validation:
  metrics:
    - "aerosol_optical_depth"
    - "surface_pm10_concentration"
    - "dust_event_frequency"
    - "transport_patterns"

  regions:
    - "sahara_desert"
    - "middle_east"
    - "east_asia"
    - "australia"
```

### Performance Targets

| Metric | Target | Current |
|--------|--------|---------|
| AOD bias | < 20% | 15% |
| PM10 correlation | > 0.6 | 0.68 |
| Event detection | > 70% | 75% |
| Seasonal cycle | < 30% RMSE | 25% |

## Transport Characteristics

### Settling Velocities

Size-dependent settling velocities:

```yaml
settling:
  method: "stokes_slip_correction"
  slip_correction: true
  shape_factor: 1.0                   # Spherical particles

  # Typical values for dust bins
  bin_settling_velocities:            # m/s
    - 0.001    # 0.5 μm
    - 0.003    # 1.4 μm
    - 0.008    # 2.4 μm
    - 0.030    # 4.5 μm
    - 0.080    # 8.0 μm
```

### Optical Properties

Wavelength-dependent optical properties:

```yaml
optics:
  refractive_index: 1.53 + 0.003i     # At 550 nm
  size_distribution: "lognormal"
  geometric_std_dev: 2.0

  # Extinction efficiency by size bin
  extinction_efficiency:
    - 0.1      # bin 1
    - 0.3      # bin 2
    - 0.6      # bin 3
    - 1.2      # bin 4
    - 2.0      # bin 5
```

## Regional Configurations

### Sahara Desert

```yaml
sahara_dust:
  emission_factor: 1.0
  source_regions: ["sahara", "sahel"]
  transport_patterns: ["atlantic", "mediterranean"]
  seasonal_cycle: true
```

### Asian Dust

```yaml
asian_dust:
  emission_factor: 0.8
  source_regions: ["gobi", "taklamakan", "loess_plateau"]
  transport_patterns: ["pacific", "korea_japan"]
  anthropogenic_sources: true
```

## Troubleshooting

### Common Issues

1. **Excessive emissions**:
   ```yaml
   # Adjust emission factor
   emission_factor: 0.5
   max_emission_flux: 1.0e-6  # kg/m²/s
   ```

2. **Insufficient transport**:
   ```yaml
   # Check settling parameters
   settling_velocity_factor: 0.8
   turbulent_mixing: enhanced
   ```

3. **Unrealistic deposition**:
   ```yaml
   # Validate deposition schemes
   dry_deposition_velocity: 0.001  # m/s
   wet_scavenging_coefficient: 1.0e-4
   ```

### Diagnostic Tools

```bash
# Analyze dust emissions
catchem_dust_analysis --input output.nc --region sahara

# Compare with observations
python validate_dust_aod.py --model output.nc --obs aeronet_data.nc
```

## Scientific References

1. **Ginoux, P., et al.** (2001). Sources and distributions of dust aerosols simulated with the GOCART model. *Journal of Geophysical Research*, 106(D17), 20255-20273.

2. **Marticorena, B., & Bergametti, G.** (1995). Modeling the atmospheric dust cycle: 1. Design of a soil-derived dust emission scheme. *Journal of Geophysical Research*, 100(D8), 16415-16430.

3. **Shao, Y., et al.** (2011). Dust cycle: An emerging core theme in Earth system science. *Aeolian Research*, 2(4), 181-204.

4. **Kok, J. F., et al.** (2014). An improved dust emission model–Part 1: Model description and comparison against measurements. *Atmospheric Chemistry and Physics*, 14(23), 13023-13041.

## Related Documentation

- **[Sea Salt Process](seasalt.md)** - Marine aerosol counterpart
- **[Settling Process](settling.md)** - Gravitational settling details
- **[Process Architecture](../../developer-guide/processes/architecture.md)** - Implementation details
- **[Configuration Guide](../configuration.md)** - General configuration principles

---

*The dust emission and transport process represents a critical component of atmospheric aerosol modeling. For technical questions or support, consult the [developer guide](../../developer-guide/processes/index.md).*

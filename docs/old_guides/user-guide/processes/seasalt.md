# Sea Salt Aerosol Process

The sea salt process in CATChem simulates the production, transport, and deposition of marine-derived aerosol particles from ocean surfaces.

## Overview

Sea salt aerosols are important for:
- **Climate impacts**: Cloud condensation nuclei and radiative effects
- **Air quality**: Particulate matter in coastal regions
- **Chemical processes**: Reaction surfaces and heterogeneous chemistry
- **Visibility**: Marine boundary layer optical properties

## Process Description

### Physical Mechanisms

1. **Wave breaking**: Bubble bursting and droplet production
2. **Spume generation**: High wind speed droplet tearing
3. **Evaporation**: Water loss and particle size evolution
4. **Hygroscopic growth**: Water uptake with relative humidity

### Emission Parameterization

Sea salt emission follows the wind speed dependence:

```
dF/dr = A * U10^B * exp(-C/r)
```

Where:
- `dF/dr`: Size-resolved emission flux (particles/m²/s/μm)
- `U10`: 10-meter wind speed (m/s)
- `r`: Particle radius (μm)
- `A`, `B`, `C`: Empirical parameters

## Supported Schemes

### Gong 2003 Scheme

Default parameterization based on Gong (2003):

```yaml
seasalt:
  scheme: "gong2003"
  parameters:
    size_bins: 4
    wind_speed_dependence: "cubic"
    whitecap_coverage: "monahan_muircheartaigh"
    temperature_correction: true
```

**Features**:
- Wind speed dependent emission
- Size-resolved particle production
- Whitecap coverage parameterization
- Temperature and salinity corrections

### Jaeglé 2011 Scheme

Enhanced scheme with sea surface temperature dependence:

```yaml
seasalt:
  scheme: "jaegle2011"
  parameters:
    size_bins: 4
    sst_dependence: true
    salinity_dependence: true
    organics_suppression: true
```

**Features**:
- Sea surface temperature effects
- Salinity variations
- Organic film suppression
- Enhanced fine particle emission

## Configuration

### Basic Setup

```yaml
processes:
  - name: "seasalt"
    type: "emission"
    scheme: "gong2003"
    size_bins: 4
    parameters:
      emission_factor: 1.0
      ocean_mask_file: "ocean_mask.nc"
      sst_file: "sea_surface_temperature.nc"
```

### Size Bin Configuration

```yaml
seasalt:
  size_bins:
    - name: "seasalt_bin1"
      diameter_range: [0.01, 0.5]      # μm
      effective_diameter: 0.2          # μm
      density: 2200.0                  # kg/m³
      hygroscopicity: 1.16             # κ parameter

    - name: "seasalt_bin2"
      diameter_range: [0.5, 1.5]       # μm
      effective_diameter: 1.0          # μm
      density: 2200.0                  # kg/m³
      hygroscopicity: 1.16             # κ parameter

    - name: "seasalt_bin3"
      diameter_range: [1.5, 5.0]       # μm
      effective_diameter: 3.0          # μm
      density: 2200.0                  # kg/m³
      hygroscopicity: 1.16             # κ parameter

    - name: "seasalt_bin4"
      diameter_range: [5.0, 15.0]      # μm
      effective_diameter: 10.0         # μm
      density: 2200.0                  # kg/m³
      hygroscopicity: 1.16             # κ parameter
```

### Advanced Parameters

```yaml
seasalt:
  scheme: "gong2003"

  # Emission parameters
  emission:
    wind_speed_threshold: 3.0         # m/s
    wind_speed_max: 50.0              # m/s
    whitecap_formulation: "monahan_muircheartaigh"
    spume_droplet_production: true

  # Environmental dependencies
  environment:
    sea_surface_temperature: true
    sea_surface_salinity: true
    organic_film_suppression: false
    wave_age_dependence: false

  # Particle properties
  particles:
    dry_diameter_fraction: 0.8        # Relative to wet diameter
    crystallization_rh: 45.0          # % RH
    deliquescence_rh: 75.0            # % RH
    shape_factor: 1.08                # Non-sphericity

  # Transport parameters
  transport:
    gravitational_settling: true
    hygroscopic_growth: true
    dry_deposition: true
    wet_deposition: true
```

## Input Requirements

### Meteorological Data

Required oceanic and atmospheric fields:

| Field | Units | Description |
|-------|-------|-------------|
| `wind_speed_10m` | m/s | 10-meter wind speed over ocean |
| `sea_surface_temperature` | K | Sea surface temperature |
| `sea_surface_salinity` | psu | Sea surface salinity |
| `relative_humidity` | % | Near-surface relative humidity |
| `air_temperature` | K | Near-surface air temperature |
| `precipitation_rate` | mm/hr | Precipitation rate |

### Surface Data

Required ocean surface characteristics:

| Field | Units | Description |
|-------|-------|-------------|
| `ocean_mask` | - | Ocean/land mask (1/0) |
| `sea_ice_fraction` | - | Sea ice coverage fraction |
| `whitecap_fraction` | - | Whitecap coverage (optional) |
| `wave_height` | m | Significant wave height (optional) |
| `wave_period` | s | Wave period (optional) |

### Chemical Composition

Sea salt composition parameters:

```yaml
composition:
  nacl_fraction: 0.775                # Mass fraction of NaCl
  mgcl2_fraction: 0.098               # Mass fraction of MgCl₂
  caso4_fraction: 0.061               # Mass fraction of CaSO₄
  kcl_fraction: 0.036                 # Mass fraction of KCl
  other_fraction: 0.030               # Other salts

  # Ion fractions
  na_fraction: 0.306
  cl_fraction: 0.550
  mg_fraction: 0.037
  so4_fraction: 0.077
  ca_fraction: 0.012
  k_fraction: 0.011
```

## Output Variables

### Emission Diagnostics

| Variable | Units | Description |
|----------|-------|-------------|
| `seasalt_emission_flux` | kg/m²/s | Total sea salt emission flux |
| `seasalt_emission_bin{n}` | kg/m²/s | Size-resolved emission flux |
| `whitecap_fraction` | - | Whitecap coverage fraction |
| `emission_number_flux` | #/m²/s | Number emission flux |

### Transport Diagnostics

| Variable | Units | Description |
|----------|-------|-------------|
| `seasalt_concentration` | kg/kg | Sea salt mass mixing ratio |
| `seasalt_number` | #/kg | Sea salt number concentration |
| `seasalt_aod` | - | Sea salt aerosol optical depth |
| `hygroscopic_growth_factor` | - | Particle growth factor |

### Deposition Diagnostics

```yaml
diagnostics:
  seasalt_deposition:
    - variable: "seasalt_dry_deposition"
      units: "kg/m²/s"
      description: "Dry deposition flux"

    - variable: "seasalt_wet_deposition"
      units: "kg/m²/s"
      description: "Wet deposition flux"

    - variable: "seasalt_settling_flux"
      units: "kg/m²/s"
      description: "Gravitational settling flux"
```

## Hygroscopic Properties

### Growth Parameterization

Hygroscopic growth follows:

```
GF(RH) = (1 + κ * RH/(1-RH))^(1/3)
```

Where:
- `GF`: Growth factor (wet/dry diameter ratio)
- `κ`: Hygroscopicity parameter (≈1.16 for sea salt)
- `RH`: Relative humidity (fraction)

### Size-Dependent Hygroscopicity

```yaml
hygroscopicity:
  method: "kappa_kohler"

  size_dependence:
    - diameter: 0.1          # μm
      kappa: 1.20            # Higher for small particles
    - diameter: 1.0          # μm
      kappa: 1.16            # Standard sea salt
    - diameter: 10.0         # μm
      kappa: 1.12            # Lower for large particles

  rh_grid:
    min_rh: 0.30             # 30%
    max_rh: 0.99             # 99%
    resolution: 0.01         # 1%
```

## Optical Properties

### Refractive Index

```yaml
optics:
  refractive_index:
    real: 1.544                       # At 550 nm
    imaginary: 1.0e-6                 # Weakly absorbing

  wavelength_dependence:
    enabled: true
    reference_wavelength: 550.0       # nm

  size_dependent_optics:
    method: "mie_theory"
    shape_correction: 1.08            # Non-sphericity factor
```

### Aerosol Optical Depth

```yaml
aod_calculation:
  wavelengths: [340, 440, 500, 675, 870, 1020]  # nm
  extinction_efficiency: "mie_lookup"
  mass_extinction_coefficient: 5.0              # m²/g (approx)
```

## Regional Variations

### Arctic Ocean

```yaml
arctic_seasalt:
  emission_factor: 0.3                # Reduced due to ice cover
  sea_ice_suppression: true
  temperature_correction: true
  enhanced_fine_mode: false
```

### Tropical Oceans

```yaml
tropical_seasalt:
  emission_factor: 1.2                # Enhanced due to higher SST
  organic_suppression: true
  enhanced_spume_production: true
  biological_suppression: seasonal
```

### Coastal Regions

```yaml
coastal_seasalt:
  land_influence: true
  surf_zone_enhancement: 1.5
  continental_deposition: enhanced
  urban_interaction: true
```

## Validation and Evaluation

### Observational Datasets

- **AERONET**: Coastal aerosol optical depth
- **Aircraft campaigns**: Marine boundary layer measurements
- **Ship-based observations**: Surface concentration measurements
- **Satellite retrievals**: Marine aerosol properties

### Performance Metrics

```yaml
validation:
  metrics:
    - "surface_sodium_concentration"
    - "marine_aod"
    - "size_distribution"
    - "emission_flux_estimates"

  target_accuracy:
    surface_concentration: 50%        # Factor of 2
    aod: 30%                         # Relative error
    size_distribution: 40%           # Geometric mean diameter
```

## Troubleshooting

### Common Issues

1. **Excessive emissions**:
   ```yaml
   # Reduce emission factors
   emission_factor: 0.8
   wind_speed_cap: 25.0  # m/s
   ```

2. **Unrealistic hygroscopic growth**:
   ```yaml
   # Check RH limits
   max_relative_humidity: 0.95
   growth_factor_limit: 3.0
   ```

3. **Mass conservation problems**:
   ```yaml
   # Enable conservation checks
   mass_conservation: true
   tolerance: 1.0e-10
   ```

### Diagnostic Tools

```bash
# Analyze sea salt emissions
catchem_seasalt_analysis --input output.nc --region marine

# Validate against observations
python validate_seasalt.py --model output.nc --obs ship_data.nc
```

## Scientific References

1. **Gong, S. L.** (2003). A parameterization of sea‐salt aerosol source function for sub‐ and super‐micron particles. *Global Biogeochemical Cycles*, 17(4).

2. **Jaeglé, L., et al.** (2011). Global distribution of sea salt aerosols: new constraints from in situ and remote sensing observations. *Atmospheric Chemistry and Physics*, 11(7), 3137-3157.

3. **Monahan, E. C., et al.** (1986). Oceanic whitecaps and their role in air-sea exchange processes. *Advances in Geophysics* (pp. 1-52). Academic Press.

4. **Lewis, E. R., & Schwartz, S. E.** (2004). *Sea Salt Aerosol Production: Mechanisms, Methods, Measurements, and Models*. American Geophysical Union.

## Related Documentation

- **[Dust Process](dust.md)** - Terrestrial aerosol counterpart
- **[Settling Process](settling.md)** - Gravitational removal
- **[Process Architecture](../../developer-guide/processes/architecture.md)** - Implementation details
- **[Configuration Guide](../configuration.md)** - General configuration principles

---

*The sea salt aerosol process provides essential marine aerosol capabilities for atmospheric chemistry and climate modeling. For technical support, see the [developer guide](../../developer-guide/processes/index.md).*

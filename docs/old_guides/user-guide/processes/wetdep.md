# Wet Deposition Process

The wet deposition process in CATChem simulates the removal of gaseous and particulate species from the atmosphere through precipitation scavenging mechanisms.

## Overview

Wet deposition is a major atmospheric removal process that includes:
- **In-cloud scavenging**: Uptake during cloud droplet formation
- **Below-cloud scavenging**: Collection by falling precipitation
- **Ice processes**: Scavenging in mixed-phase and ice clouds
- **Chemical processing**: Aqueous-phase reactions in cloud droplets

## Process Description

### Physical Mechanisms

1. **Nucleation Scavenging**: Particles acting as cloud condensation nuclei (CCN)
2. **Impact Scavenging**: Collision and collection by cloud droplets
3. **Below-Cloud Washout**: Collection by falling raindrops
4. **Snow Scavenging**: Uptake by ice crystals and snowflakes

### Scavenging Parameterization

The wet removal rate is calculated as:

```
dC/dt = -Λ * C
```

Where:
- `C`: Species concentration
- `Λ`: Scavenging coefficient (s⁻¹)
- `dC/dt`: Rate of concentration change

## Supported Schemes

### Default Scavenging Scheme

```yaml
wet_deposition:
  scheme: "default"
  parameters:
    in_cloud_scavenging: true
    below_cloud_scavenging: true
    ice_processes: true
    aqueous_chemistry: false
```

**Features**:
- Precipitation-rate dependent scavenging
- Species-specific solubility factors
- Temperature-dependent partitioning
- Separate rain and snow scavenging

### Advanced Aqueous Chemistry

```yaml
wet_deposition:
  scheme: "aqueous_chemistry"
  parameters:
    cloud_chemistry: true
    ph_calculation: true
    ionic_strength: true
    activity_coefficients: true
```

**Features**:
- Full aqueous-phase chemistry
- pH-dependent solubility
- Ionic interactions
- Chemical enhancement factors

## Configuration

### Basic Setup

```yaml
processes:
  - name: "wet_deposition"
    type: "loss"
    scheme: "default"
    species:
      - "SO2"
      - "HNO3"
      - "NH3"
      - "H2O2"
      - "PM2.5"
    parameters:
      min_precipitation_rate: 1.0e-6  # mm/hr
      scavenging_efficiency: 0.1      # dimensionless
```

### Advanced Configuration

```yaml
wet_deposition:
  scheme: "default"

  # In-cloud scavenging
  in_cloud:
    enabled: true
    cloud_fraction_threshold: 0.01
    liquid_water_threshold: 1.0e-6    # kg/kg
    ice_water_threshold: 1.0e-6       # kg/kg

  # Below-cloud scavenging
  below_cloud:
    enabled: true
    rain_scavenging: true
    snow_scavenging: true
    minimum_rate: 1.0e-6              # mm/hr

  # Species-specific parameters
  species_parameters:
    SO2:
      henry_constant: 1.3             # M/atm
      dissociation_constant: 1.7e-2   # M
      reactivity_factor: 1.0
    HNO3:
      henry_constant: 2.1e5           # M/atm
      dissociation_constant: 15.4     # M
      reactivity_factor: 1.0
    NH3:
      henry_constant: 58.0            # M/atm
      dissociation_constant: 1.7e-5   # M
      reactivity_factor: 1.0

  # Particle scavenging
  particles:
    size_dependent: true
    collection_efficiency: "beard_grover"
    washout_coefficient: 1.0e-4       # s⁻¹ (mm/hr)⁻¹
```

## Input Requirements

### Meteorological Data

Required precipitation and cloud data:

| Field | Units | Description |
|-------|-------|-------------|
| `precipitation_rate` | mm/hr | Surface precipitation rate |
| `convective_precip` | mm/hr | Convective precipitation |
| `stratiform_precip` | mm/hr | Stratiform precipitation |
| `cloud_fraction` | - | Grid-cell cloud fraction |
| `cloud_liquid_water` | kg/kg | Cloud liquid water content |
| `cloud_ice_water` | kg/kg | Cloud ice water content |
| `temperature` | K | Temperature profile |
| `pressure` | Pa | Pressure profile |

### Cloud Physics Data

For advanced schemes:

| Field | Units | Description |
|-------|-------|-------------|
| `droplet_number` | #/kg | Cloud droplet number concentration |
| `droplet_diameter` | m | Mean droplet diameter |
| `precipitation_type` | - | Rain/snow/mixed classification |
| `updraft_velocity` | m/s | Cloud updraft velocity |

### Chemical Properties

Required for each scavenged species:

```yaml
species_properties:
  SO2:
    molecular_weight: 64.0          # g/mol
    henry_constant: 1.3             # M/atm at 298K
    temperature_dependence: 3120    # K (d ln H / d(1/T))
    dissociation: true              # Acid/base chemistry
    reactivity_factor: 1.0          # Chemical enhancement
```

## Output Variables

### Diagnostic Fields

| Variable | Units | Description |
|----------|-------|-------------|
| `wet_deposition_flux` | kg/m²/s | Surface wet deposition flux |
| `scavenging_coefficient` | s⁻¹ | Bulk scavenging coefficient |
| `in_cloud_scavenging` | s⁻¹ | In-cloud removal rate |
| `below_cloud_scavenging` | s⁻¹ | Below-cloud removal rate |
| `precipitation_ph` | - | Precipitation pH (if calculated) |

### Accumulated Outputs

```yaml
diagnostics:
  wet_deposition:
    - variable: "total_wet_deposition"
      units: "kg/m²"
      frequency: "daily"
      description: "Daily accumulated wet deposition"

    - variable: "event_wet_deposition"
      units: "kg/m²"
      frequency: "event"
      description: "Per-precipitation-event deposition"

    - variable: "wet_deposition_ph"
      units: "-"
      frequency: "event"
      description: "Volume-weighted precipitation pH"
```

## Process Interactions

### Coupling with Cloud Microphysics

```yaml
coupling:
  cloud_microphysics: true
  droplet_activation: true
  ice_nucleation: true
  precipitation_formation: true
```

### Chemical Interactions

```yaml
chemistry:
  aqueous_reactions: true
  gas_liquid_equilibrium: true
  ph_buffering: true
  ionic_interactions: true
```

## Performance Considerations

### Computational Efficiency

```yaml
performance:
  optimization:
    lookup_tables: true              # Pre-computed Henry's constants
    vectorization: true              # Species loop vectorization
    precipitation_threshold: 1.0e-6  # mm/hr minimum for calculations

  memory:
    scavenging_cache: 500           # Cached coefficient arrays
    species_buffer: 50              # Species property buffer
```

### Parallel Processing

- Column-independent calculations
- Efficient precipitation event handling
- Optimized memory access patterns

## Validation and Testing

### Observational Data

Validation against monitoring networks:

- **NADP**: National Atmospheric Deposition Program
- **EMEP**: European Monitoring and Evaluation Programme
- **EANET**: Acid Deposition Monitoring Network in East Asia
- **CASTNet**: Clean Air Status and Trends Network

### Benchmark Tests

```bash
# Run wet deposition validation
ctest -R test_wet_deposition_validation

# Compare with observations
python validate_wet_deposition.py --network NADP --year 2020
```

## Sensitivity and Uncertainty

### Key Sensitivities

1. **Henry's constants**: 20-50% uncertainty in solubility
2. **Precipitation rates**: Direct impact on scavenging efficiency
3. **Cloud properties**: Affects in-cloud vs below-cloud partitioning
4. **Chemical enhancement**: pH-dependent solubility effects

### Uncertainty Quantification

```yaml
uncertainty:
  monte_carlo: true
  parameter_ranges:
    henry_constants: [0.5, 2.0]      # Multiplicative factors
    scavenging_efficiency: [0.05, 0.2]
    precipitation_uncertainty: [0.8, 1.2]
```

## Troubleshooting

### Common Issues

1. **Excessive removal rates**:
   ```yaml
   # Limit maximum scavenging
   max_scavenging_coefficient: 1.0e-3  # s⁻¹
   precipitation_rate_cap: 100.0       # mm/hr
   ```

2. **Mass conservation problems**:
   ```yaml
   # Enable conservation checks
   mass_conservation: true
   tolerance: 1.0e-10
   ```

3. **Unrealistic pH values**:
   ```yaml
   # Constrain pH range
   min_ph: 3.0
   max_ph: 8.0
   ph_buffering: true
   ```

### Diagnostic Tools

```bash
# Check scavenging coefficients
catchem_diagnose_wetdep --input output.nc --check scavenging_rates

# Validate precipitation chemistry
catchem_validate_precip_chem --observations nadp_data.nc
```

## Scientific References

1. **Seinfeld, J. H., & Pandis, S. N.** (2016). *Atmospheric Chemistry and Physics* (3rd ed.). Chapter 19: Wet Deposition.

2. **Sportisse, B.** (2007). A review of parameterizations for modelling dry deposition and scavenging of radionuclides. *Atmospheric Environment*, 41(13), 2683-2698.

3. **Henzing, J. S., et al.** (2006). Overview of the LOTOS-EUROS (v1.0) chemistry transport model for intercomparison projects. *Geoscientific Model Development*, 6(4), 1233-1259.

4. **Barth, M. C., et al.** (2000). Sulfur chemistry in the NCAR CCM: Description, evaluation, features and sensitivity to aqueous chemistry. *Journal of Geophysical Research*, 105(D1), 387-415.

## Related Documentation

- **[Dry Deposition Process](drydep.md)** - Complementary removal process
- **[Process Architecture](../../developer-guide/processes/architecture.md)** - Understanding process implementation
- **[Configuration Guide](../configuration.md)** - General configuration principles
- **[Performance Tuning](../performance.md)** - Optimization strategies

---

*The wet deposition process is under active development. For the latest updates and technical support, consult the [developer documentation](../../developer-guide/processes/index.md).*

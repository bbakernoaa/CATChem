# Settling Process

<span class="process-badge process-badge--transport">Transport</span>

The **Settling Process** handles gravitational settling of atmospheric particles, including sophisticated physics for small particles with slip correction and temperature-dependent dynamic viscosity.

## Overview

Gravitational settling is a critical transport process for aerosols and particles in the atmosphere. The CATChem settling process implements state-of-the-art physics including:

- **Stokes Law**: Fundamental settling physics for spherical particles
- **Cunningham Slip Correction**: Accurate treatment of small particles (< 1 μm)
- **Dynamic Viscosity**: Temperature-dependent air viscosity using Sutherland's law
- **Shape Factors**: Support for non-spherical particles
- **CFL-Stable Integration**: Subcycling for numerical stability

## Available Schemes

### Stokes Scheme (Recommended)

Modern implementation with comprehensive physics:

```yaml
processes:
  - name: "settling"
    scheme: "Stokesscheme"
    enabled: true
    parameters:
      shape_factor: 1.0  # Spherical particles
      cfl_max: 0.8       # CFL stability limit
```

**Features:**
- ✅ Sutherland's law for dynamic viscosity
- ✅ Cunningham slip correction for small particles
- ✅ CFL-stable subcycling
- ✅ Comprehensive error handling
- ✅ Temperature-dependent physics
- ✅ Column virtualization optimized

### Intermediate Reynolds Scheme

For particles in the intermediate Reynolds number regime:

```yaml
processes:
  - name: "settling"
    scheme: "Intermediatereynoldsscheme"
    enabled: true
```

**Use when:** Reynolds numbers > 0.1 (larger particles, higher velocities)

## Physics

### Settling Velocity Calculation

The settling velocity is calculated using Stokes law with corrections:

$$v_s = \frac{2g(\rho_p - \rho_a)r^2 C_c}{9\mu \chi}$$

Where:
- $v_s$ = settling velocity [m/s]
- $g$ = gravitational acceleration [m/s²]
- $\rho_p$ = particle density [kg/m³]
- $\rho_a$ = air density [kg/m³]
- $r$ = particle radius [m]
- $C_c$ = Cunningham slip correction factor
- $\mu$ = dynamic viscosity [kg/m/s]
- $\chi$ = dynamic shape factor

### Cunningham Slip Correction

For particles smaller than the mean free path of air:

$$C_c = 1 + \frac{\lambda}{r}(A_1 + A_2 e^{-A_3 r/\lambda})$$

Where:
- $\lambda$ = mean free path of air [m]
- $A_1 = 1.257$, $A_2 = 0.4$, $A_3 = 1.1$ (empirical constants)

### Dynamic Viscosity (Sutherland's Law)

Temperature-dependent air viscosity:

$$\mu(T) = \mu_0 \frac{T^{3/2}}{T + S}$$

Where:
- $\mu_0 = 1.458 \times 10^{-6}$ [kg m⁻¹ s⁻¹ K⁻¹·⁵]
- $S = 110.4$ K (Sutherland temperature)
- $T$ = air temperature [K]

## Configuration

### Required Input Data

The settling process requires the following meteorological fields:

| Field | Units | Description |
|-------|-------|-------------|
| `temperature` | K | Air temperature |
| `pressure` | Pa | Air pressure |
| `air_density` | kg/m³ | Air density |

### Required Particle Properties

| Property | Units | Description | Default |
|----------|-------|-------------|---------|
| `particle_radius` | m | Effective particle radius | Required |
| `particle_density` | kg/m³ | Particle density | Required |
| `shape_factor` | - | Dynamic shape factor | 1.0 |

### Example Configuration

```yaml
# Settling process configuration
processes:
  - name: "settling"
    scheme: "Stokesscheme"
    enabled: true

    # Process parameters
    parameters:
      cfl_max: 0.8                    # Maximum CFL number
      max_substeps: 20                # Maximum subcycles
      min_settling_velocity: 1.0e-8   # Minimum velocity [m/s]

    # Particle properties (per species)
    particle_properties:
      dust_small:
        radius: 0.5e-6               # 0.5 μm
        density: 2650.0              # Quartz density
        shape_factor: 1.2            # Non-spherical

      dust_large:
        radius: 5.0e-6               # 5 μm
        density: 2650.0
        shape_factor: 1.3

      sea_salt:
        radius: 1.0e-6               # 1 μm
        density: 2170.0              # NaCl density
        shape_factor: 1.0            # Spherical

      sulfate:
        radius: 0.3e-6               # 0.3 μm
        density: 1770.0              # (NH4)2SO4
        shape_factor: 1.0

# Output configuration
output:
  diagnostics:
    settling_velocity:
      enabled: true
      frequency: 3600               # Every hour
    settling_flux:
      enabled: true
      frequency: 3600
    cfl_number:
      enabled: false                # Debug only
      frequency: 3600
```

## Diagnostics

The settling process provides comprehensive diagnostic output:

### Standard Diagnostics

| Variable | Units | Description |
|----------|-------|-------------|
| `settling_velocity` | m/s | Gravitational settling velocity |
| `settling_flux` | kg/m²/s | Vertical mass flux |

### Detailed Diagnostics

| Variable | Units | Description |
|----------|-------|-------------|
| `dynamic_viscosity` | kg/m/s | Air dynamic viscosity |
| `slip_correction` | - | Cunningham slip correction |

### Debug Diagnostics

| Variable | Units | Description |
|----------|-------|-------------|
| `cfl_number` | - | CFL number for time stepping |
| `reynolds_number` | - | Particle Reynolds number |
| `mean_free_path` | m | Air mean free path |

## Performance

### Column Virtualization

The settling process is optimized for column processing:

<div class="performance-metric">
  <span>Column Processing Speedup</span>
  <span class="performance-metric__value">8-12x faster</span>
</div>

<div class="performance-metric">
  <span>Memory Efficiency</span>
  <span class="performance-metric__value">60% reduction</span>
</div>

<div class="performance-metric">
  <span>Cache Performance</span>
  <span class="performance-metric__value">3x better locality</span>
</div>

### Numerical Stability

- **CFL-Stable Integration**: Automatic subcycling when $v_s \Delta t > 0.8 \Delta z$
- **Robust Bounds Checking**: Prevents unphysical values
- **Error Recovery**: Graceful handling of numerical issues

## Usage Examples

### Basic Usage

```fortran
use settlingProcess_Mod
use State_Mod

type(settlingProcessType) :: settling
type(StateContainerType) :: container
integer :: rc

! Initialize
call settling%init(container, rc)

! Run settling for one time step
call settling%run(container, rc)

! Check diagnostics
call settling%get_diagnostic('settling_velocity', velocity_field, rc)
```

### Advanced Configuration

```fortran
! Configure settling with custom parameters
call settling%set_parameter('cfl_max', 0.9_fp)
call settling%set_parameter('max_substeps', 15)

! Set particle properties for specific species
call settling%set_particle_radius('dust_fine', 0.3e-6_fp)
call settling%set_particle_density('dust_fine', 2650.0_fp)
call settling%set_shape_factor('dust_fine', 1.15_fp)
```

### Column Processing

```fortran
! Enable column processing (default)
call settling%enable_column_processing(.true.)

! Process specific column
call settling%run_column(column_data, container, rc)
```

## Validation

### Test Cases

The settling process includes comprehensive validation:

1. **Analytical Solutions**: Comparison with analytical Stokes law
2. **Laboratory Data**: Validation against settling chamber experiments
3. **Intercomparison**: Cross-validation with other models
4. **Limiting Cases**: Behavior in extreme conditions

### Benchmarks

| Test Case | Description | Expected Result |
|-----------|-------------|-----------------|
| `stokes_analytical` | Pure Stokes settling | < 1% error |
| `slip_correction` | Small particle regime | Matches theory |
| `temperature_dependence` | Variable temperature | Sutherland's law |
| `cfl_stability` | Large time steps | Stable integration |

Run validation tests:

```bash
cd build
ctest -R settling_validation -V
```

## Troubleshooting

### Common Issues

??? question "Excessive settling velocities"

    **Symptoms:** Unrealistically high settling velocities

    **Solutions:**
    - Check particle radius units (should be meters)
    - Verify particle density values
    - Ensure temperature field is reasonable

    ```yaml
    # Debug configuration
    diagnostics:
      settling_velocity: {enabled: true}
      reynolds_number: {enabled: true}
    ```

??? question "CFL instability warnings"

    **Symptoms:** Frequent subcycling warnings

    **Solutions:**
    - Reduce maximum CFL number
    - Check vertical grid resolution
    - Verify time step size

    ```yaml
    parameters:
      cfl_max: 0.5          # More conservative
      max_substeps: 25      # Allow more subcycles
    ```

??? question "Small particle behavior"

    **Symptoms:** Unexpected behavior for sub-micron particles

    **Solutions:**
    - Verify slip correction is enabled
    - Check mean free path calculation
    - Review particle size distribution

    ```yaml
    diagnostics:
      slip_correction: {enabled: true}
      mean_free_path: {enabled: true}
    ```

### Performance Issues

??? question "Slow settling calculation"

    **Solutions:**
    - Enable column processing (default)
    - Reduce diagnostic frequency
    - Check for unnecessary subcycling

    ```yaml
    # Optimized configuration
    parameters:
      cfl_max: 0.8          # Less subcycling
    output:
      diagnostics:
        settling_velocity:
          frequency: 3600     # Less frequent output
    ```

### Debugging

Enable verbose diagnostics for debugging:

```yaml
processes:
  - name: "settling"
    debug_level: "verbose"
    diagnostics:
      settling_velocity: {enabled: true}
      cfl_number: {enabled: true}
      slip_correction: {enabled: true}
      dynamic_viscosity: {enabled: true}
```

## References

1. **Seinfeld, J. H., & Pandis, S. N. (2016)**. *Atmospheric Chemistry and Physics: From Air Pollution to Climate Change*. 3rd Edition.

2. **Cunningham, E. (1910)**. On the velocity of steady fall of spherical particles through fluid medium. *Proceedings of the Royal Society A*, 83(563), 357-365.

3. **Sutherland, W. (1893)**. The viscosity of gases and molecular force. *Philosophical Magazine*, 36(223), 507-531.

4. **Hinds, W. C. (1999)**. *Aerosol Technology: Properties, Behavior, and Measurement of Airborne Particles*. 2nd Edition.

## See Also

- [Process Architecture Guide](../../developer-guide/processes/architecture.md)
- [Column Virtualization](../../guides/column-virtualization.md)
- [Vertical Mixing Process](verticalmixing.md)
- [API Reference](../../api/CATChem/settling/)

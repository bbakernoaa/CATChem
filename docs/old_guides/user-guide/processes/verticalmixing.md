# Vertical Mixing Process

<span class="process-badge process-badge--transport">Transport</span>

The **Vertical Mixing Process** handles turbulent mixing in the atmospheric boundary layer using the YSU (Yonsei University) planetary boundary layer scheme.

## Overview

Vertical mixing is a critical transport process that redistributes atmospheric constituents throughout the boundary layer. The YSU scheme provides realistic representation of:

- **Boundary Layer Height**: Dynamic calculation of mixing layer depth
- **Turbulent Mixing**: Vertical redistribution of chemical species
- **Entrainment**: Mixing at the boundary layer top
- **Surface Layer**: Near-surface turbulent transport

## Available Schemes

### YSU Scheme (Default)

The Yonsei University boundary layer scheme:

```yaml
processes:
  - name: "verticalmixing"
    scheme: "YSU"
    enabled: true
    parameters:
      mixing_length_scale: "default"
```

**Features:**
- ✅ Non-local mixing representation
- ✅ Convective and stable boundary layers
- ✅ Entrainment zone treatment
- ✅ Surface layer integration

### Scale-Aware YSU

Enhanced version for high-resolution applications:

```yaml
processes:
  - name: "verticalmixing"
    scheme: "ScaleAwareYSU"
    enabled: true
    parameters:
      grid_scale_factor: 1.0
```

## Physics

### Mixing Coefficient Calculation

The YSU scheme calculates eddy diffusivity as:

$$K = \kappa w_s z \left(1 - \frac{z}{h}\right)^2$$

Where:
- $K$ = eddy diffusivity [m²/s]
- $\kappa$ = von Karman constant (0.4)
- $w_s$ = convective velocity scale [m/s]
- $z$ = height above surface [m]
- $h$ = boundary layer height [m]

### Species Transport

Chemical species are mixed according to:

$$\frac{\partial C}{\partial t} = \frac{\partial}{\partial z}\left(K \frac{\partial C}{\partial z}\right)$$

Where $C$ is the species concentration.

## Configuration

### Required Input Data

| Field | Units | Description |
|-------|-------|-------------|
| `temperature` | K | Air temperature profile |
| `u_wind` | m/s | Zonal wind component |
| `v_wind` | m/s | Meridional wind component |
| `surface_heat_flux` | W/m² | Surface sensible heat flux |
| `friction_velocity` | m/s | Surface friction velocity |

### Example Configuration

```yaml
# Vertical mixing configuration
processes:
  - name: "verticalmixing"
    scheme: "YSU"
    enabled: true

    parameters:
      pbl_height_max: 3000.0      # Maximum PBL height [m]
      mixing_length_scale: 150.0  # Mixing length scale [m]
      entrainment_factor: 0.2     # Entrainment rate factor

# Output diagnostics
output:
  diagnostics:
    pbl_height:
      enabled: true
      frequency: 3600
    mixing_coefficient:
      enabled: true
      frequency: 3600
```

## Diagnostics

### Standard Diagnostics

| Variable | Units | Description |
|----------|-------|-------------|
| `pbl_height` | m | Planetary boundary layer height |
| `mixing_coefficient` | m²/s | Vertical eddy diffusivity |

### Advanced Diagnostics

| Variable | Units | Description |
|----------|-------|-------------|
| `convective_velocity` | m/s | Convective velocity scale |
| `mixing_ratio_tendency` | kg/kg/s | Species mixing tendency |

## Performance

The vertical mixing process is optimized for:

- **Column Processing**: Efficient vertical column operations
- **Implicit Integration**: Stable numerical methods
- **Vectorization**: SIMD-optimized calculations

## Usage Examples

### Basic Usage

```fortran
use YSUVerticalDispersionProcess_Mod

type(YSUVerticalDispersionProcessType) :: mixing
call mixing%init(container, rc)
call mixing%run(container, rc)
```

### Advanced Configuration

```fortran
! Set custom parameters
call mixing%set_parameter('pbl_height_max', 2500.0_fp)
call mixing%set_parameter('entrainment_factor', 0.25_fp)
```

## See Also

- [Settling Process](settling.md) - Gravitational settling
- [Process Architecture](../../developer-guide/processes/architecture.md)
- [YSU Scheme Documentation](../../api/CATChem/_y_s_u_vertical_dispersion_process___mod_8_f90/)

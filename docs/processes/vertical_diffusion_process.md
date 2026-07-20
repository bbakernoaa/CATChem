# YSU Vertical Dispersion Process Documentation

## Overview

Yonsei University (YSU) planetary boundary layer vertical dispersion scheme for atmospheric transport, implementing scale-aware mixing with enhanced turbulence parameterization.

**Process Type:** transport
**Author:** CATChem Development Team
**Version:** 1.0
**Date:** 2025

## Features

### Available Schemes

- **scaleAwareYSU**: Modern scale-aware YSU implementation with:
  - Enhanced entrainment calculations
  - Scale-aware mixing coefficients
  - Improved turbulence parameterization
  - CATChem constants integration
  - Modern diagnostic capabilities

### Physical Features

- **Scale-aware mixing**: Accounts for grid resolution effects on turbulent mixing
- **Enhanced entrainment**: Improved boundary layer top entrainment calculations
- **Robust numerics**: CFL-stable integration with modern error handling
- **Comprehensive diagnostics**: Full suite of boundary layer diagnostics


## Usage

### Initialization

```fortran
use ysuverticaldispersionProcess_Mod
type(ysuverticaldispersionProcessType) :: process
type(StateContainerType) :: container
integer :: rc

! Initialize the process
call process%init(container, rc)
```

### Running the Process

```fortran
! Run the process
call process%run(container, rc)
```

### Finalization

```fortran
! Clean up
call process%finalize(rc)
```

## Configuration

The YSU vertical dispersion process requires meteorological data and boundary layer parameters.

### Required Meteorological Fields

- **temperature**: Air temperature [K]
- **potential_temperature**: Potential temperature [K]
- **pressure**: Air pressure [Pa]
- **air_density**: Air density [kg/m³]
- **wind_u**: Zonal wind component [m/s]
- **wind_v**: Meridional wind component [m/s]
- **boundary_layer_height**: Planetary boundary layer height [m]
- **surface_heat_flux**: Surface sensible heat flux [W/m²]
- **friction_velocity**: Surface friction velocity [m/s]

### Available Diagnostics

- **mixing_coefficients**: Vertical mixing coefficients [m²/s]
- **entrainment_rate**: Entrainment rate at PBL top [m/s]
- **boundary_layer_height**: Diagnosed PBL height [m]
- **mixing_length**: Turbulent mixing length scale [m]
- **richardson_number**: Bulk Richardson number [dimensionless]
- **stability_parameter**: Atmospheric stability parameter [dimensionless]

## Physical Parameterization

### YSU Scheme Overview

The YSU scheme parameterizes vertical turbulent transport in the planetary boundary layer using:

1. **Mixing Length Formulation**: Height-dependent mixing length with entrainment effects
2. **Stability Functions**: Richardson number-based stability corrections
3. **Entrainment Parameterization**: Enhanced entrainment at the boundary layer top
4. **Scale Awareness**: Grid-resolution dependent mixing coefficients

### Key Equations

#### Mixing Coefficient

```
K_h = κ * u* * h * φ_h(z/h) * (1 - z/h)²
```

Where:
- `κ` = von Kármán constant (0.4)
- `u*` = friction velocity [m/s]
- `h` = boundary layer height [m]
- `φ_h` = stability function for heat
- `z` = height above surface [m]

#### Entrainment Rate

```
w_e = A * u*³ / (h * g * ∂θ_v/∂z)
```

Where:
- `A` = entrainment efficiency coefficient
- `g` = gravitational acceleration [m/s²]
- `∂θ_v/∂z` = virtual potential temperature gradient at PBL top [K/m]


## Implementation Details

### Process Structure

```
src/process/ysuverticaldispersion/
├── ysuverticaldispersionProcess_Mod.F90     # Main process module
├── ysuverticaldispersionCommon_Mod.F90      # Common utilities
└── schemes/                               # Scheme implementations
    ├── standardYSUScheme_Mod.F90
    ├── enhancedYSUScheme_Mod.F90
    ├── scaleAwareYSUScheme_Mod.F90
```

### Testing

Unit tests are available in:
```
tests/process/ysuverticaldispersion/
└── test_ysuverticaldispersion_process.F90
```

Run tests with:
```bash
ctest -R ysuverticaldispersion
```

## References

- [CATChem Process Architecture Guide](../developer-guide/processes/architecture.md)
- [ProcessInterface Documentation](../api/index.md#process-interface)

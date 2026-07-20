# settling Process Documentation

## Overview

Gravitational settling process for atmospheric particles implementing physics-based settling schemes with temperature-dependent viscosity, slip correction, and robust numerical integration.

**Process Type:** transport
**Author:** CATChem Development Team
**Version:** 1.0
**Date:** 2025

## Features

### Available Schemes

- **Stokesscheme**: Advanced Stokes settling with Cunningham slip correction and temperature-dependent dynamic viscosity
  - Sutherland's formula for dynamic viscosity
  - Cunningham slip correction for small particles
  - CFL-stable time integration with automatic subcycling
  - Support for non-spherical particles via shape factors
  - Robust error handling and bounds checking

- **Intermediatereynoldsscheme**: Intermediate Reynolds number settling scheme for larger particles

### Physical Features

- **Dynamic viscosity**: Temperature-dependent air viscosity using Sutherland's law
- **Slip correction**: Cunningham slip correction for particles smaller than mean free path
- **Shape factors**: Support for non-spherical particles
- **CFL stability**: Automatic subcycling to maintain numerical stability
- **Error handling**: Comprehensive bounds checking and validation


## Usage

### Initialization

```fortran
use settlingProcess_Mod
type(settlingProcessType) :: process
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

The settling process requires particle properties and meteorological data to calculate settling velocities.

### Required Meteorological Fields

- **temperature**: Air temperature [K]
- **pressure**: Air pressure [Pa]
- **air_density**: Air density [kg/m³]

### Required Tracer Properties

- **particle_radius**: Effective particle radius [m]
- **particle_density**: Particle density [kg/m³]

### Optional Tracer Properties

- **shape_factor**: Dynamic shape factor for non-spherical particles [dimensionless, default: 1.0]

### Available Diagnostics

- **settling_velocity**: Gravitational settling velocity [m/s]
- **settling_flux**: Vertical settling mass flux [kg/m²/s]
- **cfl_number**: CFL number for settling time step [dimensionless]
- **dynamic_viscosity**: Temperature-dependent air dynamic viscosity [kg/m/s]
- **slip_correction**: Cunningham slip correction factor [dimensionless]

## Physical Equations

### Stokes Settling Velocity

The basic Stokes settling velocity is calculated as:

```
v_s = (2 * g * (ρ_p - ρ_air) * r²) / (9 * μ)
```

Where:
- `g` = gravitational acceleration [9.81 m/s²]
- `ρ_p` = particle density [kg/m³]
- `ρ_air` = air density [kg/m³]
- `r` = particle radius [m]
- `μ` = dynamic viscosity [kg/m/s]

### Dynamic Viscosity (Sutherland's Law)

```
μ(T) = μ₀ * T^1.5 / (T + S)
```

Where:
- `μ₀` = 1.458e-6 kg/(m·s·K^1.5)
- `S` = 110.4 K (Sutherland temperature)
- `T` = temperature [K]

### Cunningham Slip Correction

```
C_c = 1 + (λ/r) * [A₁ + A₂ * exp(-A₃ * r/λ)]
```

Where:
- `λ` = mean free path [m]
- `A₁` = 1.257, `A₂` = 0.4, `A₃` = 1.1 (slip coefficients)

### Final Settling Velocity

```
v_settling = v_s * C_c / χ
```

Where `χ` is the dynamic shape factor for non-spherical particles.


## Implementation Details

### Process Structure

```
src/process/settling/
├── settlingProcess_Mod.F90     # Main process module
├── settlingCommon_Mod.F90      # Common utilities
└── schemes/                               # Scheme implementations
    ├── StokesschemeScheme_Mod.F90
    ├── IntermediatereynoldsschemeScheme_Mod.F90
```

### Testing

Unit tests are available in:
```
tests/process/settling/
└── test_settling_process.F90
```

Run tests with:
```bash
ctest -R settling
```

## References

- [CATChem Process Architecture Guide](../developer-guide/processes/architecture.md)
- [ProcessInterface Documentation](../api/index.md#process-interface)

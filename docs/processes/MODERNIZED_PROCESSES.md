# CATChem Modernized Processes Overview

This document provides an overview of the recently modernized atmospheric processes in CATChem, highlighting the improvements in physics, numerics, and software architecture.

## Modernized Processes

### 1. Settling Process

**Location**: `src/process/settling/`
**Type**: Transport (gravitational settling)
**Status**: ✅ Completed

#### Key Improvements

- **Advanced Stokes Scheme**:
  - Temperature-dependent dynamic viscosity using Sutherland's law
  - Cunningham slip correction for small particles
  - Support for non-spherical particles via shape factors
  - CFL-stable subcycling for numerical stability

- **Modern Architecture**:
  - Clean separation of process and scheme modules
  - Scheme subdirectory organization
  - Comprehensive error handling and bounds checking
  - Full diagnostic variable suite

#### Available Schemes

- **StokesScheme**: Advanced Stokes settling with slip correction
- **IntermediatereynoldsScheme**: For larger particles (intermediate Reynolds numbers)

#### Diagnostics

- settling_velocity, settling_flux, cfl_number
- dynamic_viscosity, slip_correction

---

### 2. YSU Vertical Dispersion Process

**Location**: `src/process/ysuverticaldispersion/`
**Type**: Transport (vertical mixing)
**Status**: ✅ Completed

#### Key Improvements

- **Scale-Aware YSU Scheme**:
  - Enhanced entrainment calculations
  - Grid-resolution dependent mixing coefficients
  - Improved turbulence parameterization
  - Modern diagnostic capabilities

- **Architecture Enhancements**:
  - Removed legacy scheme variants
  - CATChem constants integration
  - Modern apply_tendency interface
  - Column processing optimization

#### Available Schemes

- **scaleAwareYSU**: Modern scale-aware implementation (only scheme retained)

#### Diagnostics

- mixing_coefficients, entrainment_rate, boundary_layer_height
- mixing_length, richardson_number, stability_parameter

---

## Architecture Improvements

### Generator Enhancements

The process generator has been updated to support:

- **Scheme Subdirectories**: Automatic creation of `schemes/` subdirectories
- **Modern Templates**: Updated process templates with modern CATChem patterns
- **Diagnostic Configuration**: YAML-driven diagnostic variable registration
- **CMake Integration**: Automatic build system updates

### Code Quality

All modernized processes feature:

- **Error Handling**: Comprehensive error checking with context tracking
- **Bounds Checking**: Input validation and physical bounds enforcement
- **Documentation**: Extensive inline documentation and external docs
- **Testing**: Unit tests and integration tests
- **Diagnostics**: Full diagnostic variable support

### Performance

- **Column Processing**: Optimized column-wise processing for performance
- **Memory Efficiency**: Minimal allocations, extensive use of pointers
- **Numerical Stability**: CFL-aware time stepping and robust algorithms

## Configuration

All processes use YAML configuration files with structured parameter definitions:

```yaml
process_config:
  name: "process_name"
  description: "Process description"
  schemes:
    - name: "scheme_name"
      description: "Scheme description"
  diagnostics:
    variable_name:
      description: "Variable description"
      units: "units"
      dimensions: ["dim1", "dim2"]
```

## Usage

### Initialization

```fortran
use processProcess_Mod
type(processProcessType) :: process
type(StateContainerType) :: container
integer :: rc

call process%init(container, rc)
```

### Execution

```fortran
call process%run(container, rc)
```

### Cleanup

```fortran
call process%finalize(rc)
```

## Next Steps

1. **Additional Processes**: Apply the same modernization approach to other processes
2. **Testing**: Comprehensive testing of all modernized processes
3. **Documentation**: Complete API documentation generation
4. **Integration**: Full integration testing with model workflows

## References

- [Settling Process Documentation](settling_process.md)
- [YSU Vertical Dispersion Documentation](vertical_diffusion_process.md)
- [CATChem Process Architecture Guide](../developer-guide/processes/architecture.md)

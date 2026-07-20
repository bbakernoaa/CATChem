# Configuration System

CATChem uses a flexible YAML-based configuration system for all model settings.

## Overview

The configuration system provides:

- **Hierarchical Structure**: Organized configuration with inheritance
- **Type Safety**: Automatic validation and type checking
- **Environment Variables**: Support for environment variable substitution
- **Validation**: Comprehensive input validation
- **Documentation**: Self-documenting configuration files

## Configuration File Structure

```yaml
# Main CATChem configuration file
site_name: "My CATChem Run"
description: "Description of this model run"

# Model domain and timing
domain:
  grid_type: "lat_lon"           # lat_lon, cubed_sphere, unstructured
  resolution: "0.25deg"          # Grid resolution
  vertical_levels: 64            # Number of vertical levels

time:
  start_date: "2025-01-01T00:00:00Z"
  end_date: "2025-01-02T00:00:00Z"
  time_step: 300                 # Seconds

# Atmospheric processes
processes:
  - name: "settling"
    scheme: "Stokesscheme"
    enabled: true
    parameters:
      cfl_max: 0.8
      max_substeps: 20

  - name: "chemistry"
    scheme: "GOCART"
    enabled: true
    parameters:
      solver_tolerance: 1.0e-6

# Chemical species
species:
  - name: "dust1"
    long_name: "Fine dust"
    units: "kg/kg"
    molecular_weight: 28.0

  - name: "dust2"
    long_name: "Coarse dust"
    units: "kg/kg"
    molecular_weight: 28.0

# Input/Output
input:
  meteorology:
    file: "met_data.nc"
    format: "netcdf"

  emissions:
    file: "emissions.nc"
    format: "netcdf"

output:
  file: "catchem_output.nc"
  format: "netcdf"
  frequency: 3600               # Output frequency in seconds

  # Diagnostic output
  diagnostics:
    settling_velocity:
      enabled: true
      frequency: 3600
      description: "Gravitational settling velocity"

    concentration:
      enabled: true
      frequency: 1800
      description: "Species concentrations"
```

## Configuration Loading

### Basic Usage

```fortran
use ConfigManager_Mod

type(ConfigManagerType) :: config
integer :: rc

! Load configuration
call config%load_file("catchem.yml", rc)
if (rc /= CC_SUCCESS) then
  call error_handler%log_error("Failed to load configuration", rc)
  stop
end if

! Access configuration values
real(fp) :: time_step
logical :: process_enabled
character(len=:), allocatable :: scheme_name

call config%get("time.time_step", time_step, rc)
call config%get("processes.settling.enabled", process_enabled, rc)
call config%get("processes.settling.scheme", scheme_name, rc)
```

### Process Configuration

```fortran
! Get process-specific configuration
type(ConfigDataType), pointer :: process_config

process_config => config%get_section("processes.settling")
if (.not. associated(process_config)) then
  call error_handler%log_error("Settling process not configured")
  return
end if

! Access process parameters
real(fp) :: cfl_max
call process_config%get("parameters.cfl_max", cfl_max, default=0.8_fp, rc=rc)
```

## Configuration Validation

### Automatic Validation

```fortran
! Configuration schema definition
type(ConfigSchemaType) :: schema

! Define required fields
call schema%add_required("time.start_date", "string")
call schema%add_required("time.end_date", "string")
call schema%add_required("time.time_step", "real", min_value=1.0_fp)

! Define optional fields with defaults
call schema%add_optional("output.frequency", "integer", default=3600)

! Validate configuration
call config%validate(schema, rc)
if (rc /= CC_SUCCESS) then
  call error_handler%log_error("Configuration validation failed", rc)
end if
```

### Custom Validation

```fortran
! Custom validation for process parameters
subroutine validate_settling_config(config, rc)
  type(ConfigDataType), intent(in) :: config
  integer, intent(out) :: rc

  real(fp) :: cfl_max
  integer :: max_substeps

  ! Validate CFL limit
  call config%get("cfl_max", cfl_max, rc)
  if (cfl_max <= 0.0_fp .or. cfl_max > 1.0_fp) then
    call error_handler%log_error("CFL limit must be between 0 and 1")
    rc = CC_INVALID_INPUT
    return
  end if

  ! Validate substep limit
  call config%get("max_substeps", max_substeps, rc)
  if (max_substeps < 1 .or. max_substeps > 100) then
    call error_handler%log_error("Max substeps must be between 1 and 100")
    rc = CC_INVALID_INPUT
    return
  end if

  rc = CC_SUCCESS
end subroutine
```

## Environment Variables

### Variable Substitution

```yaml
# Use environment variables in configuration
input:
  meteorology:
    file: !ENV ${METDATA_PATH}/met_${YYYYMMDD}.nc

output:
  directory: !ENV [OUTPUT_DIR, "./output"]  # With default fallback

# System configuration
parallel:
  num_threads: !ENV [OMP_NUM_THREADS, 4]

database:
  connection: !ENV DATABASE_URL  # Required environment variable
```

### Environment Setup

```bash
# Set environment variables
export METDATA_PATH="/data/meteorology"
export OUTPUT_DIR="/scratch/catchem/output"
export OMP_NUM_THREADS=8
export DATABASE_URL="postgresql://user:pass@host/db"

# Run CATChem
./catchem_driver --config production.yml
```

## Configuration Inheritance

### Base Configuration

```yaml
# base.yml - Common configuration
domain:
  grid_type: "lat_lon"
  vertical_levels: 64

time:
  time_step: 300

processes:
  - name: "settling"
    scheme: "Stokesscheme"
    enabled: true
    parameters:
      cfl_max: 0.8
```

### Derived Configuration

```yaml
# experiment.yml - Specific experiment
INHERIT: base.yml

site_name: "Dust Storm Experiment"

# Override specific settings
time:
  start_date: "2025-03-15T00:00:00Z"
  end_date: "2025-03-20T00:00:00Z"

# Add experiment-specific processes
processes:
  - name: "dust"
    scheme: "AFWA"
    enabled: true
```

## Advanced Features

### Conditional Configuration

```yaml
# Platform-specific settings
system: !ENV [SYSTEM_TYPE, "generic"]

processes:
  - name: "settling"
    scheme: "Stokesscheme"
    enabled: true
    parameters:
      # Different parameters for different systems
      cfl_max: !CASE
        - condition: ${system} == "hpc"
          value: 0.9
        - condition: ${system} == "workstation"
          value: 0.6
        - default: 0.8
```

### Dynamic Configuration

```fortran
! Runtime configuration updates
call config%set("processes.settling.parameters.cfl_max", 0.9_fp, rc)
call config%save_file("updated_config.yml", rc)

! Temporary configuration overrides
call config%push_context()
call config%set("output.frequency", 1800, rc)
! ... run with modified config ...
call config%pop_context()  ! Restore original values
```

## Configuration Tools

### Validation Utility

```bash
# Validate configuration file
catchem_config_validator --config experiment.yml --schema catchem.schema

# Check for missing required fields
catchem_config_validator --check-required experiment.yml

# Generate configuration template
catchem_config_generator --template basic > new_config.yml
```

### Configuration Diff

```bash
# Compare configurations
catchem_config_diff base.yml experiment.yml

# Show effective configuration after inheritance
catchem_config_show --effective experiment.yml
```

## Best Practices

### Organization

1. **Hierarchical Structure**: Group related settings together
2. **Clear Naming**: Use descriptive parameter names
3. **Documentation**: Include comments explaining settings
4. **Validation**: Always validate configuration before use
5. **Defaults**: Provide sensible defaults for optional parameters

### Performance

```yaml
# Performance-oriented configuration
performance:
  # Memory management
  memory:
    initial_pool_size: "1GB"
    max_pool_size: "8GB"

  # Threading
  threading:
    num_threads: !ENV [OMP_NUM_THREADS, 8]
    thread_affinity: "close"

  # I/O optimization
  io:
    buffer_size: "64MB"
    parallel_io: true
```

### Debugging

```yaml
# Debug configuration
debug:
  enabled: !ENV [DEBUG_MODE, false]
  level: "verbose"

  # Process-specific debugging
  processes:
    settling: "debug"
    chemistry: "info"

logging:
  level: !ENV [LOG_LEVEL, "info"]
  file: "catchem.log"
  console: true
```

## Troubleshooting

### Common Issues

- **YAML Syntax Errors**: Use YAML validator to check syntax
- **Missing Environment Variables**: Check variable names and availability
- **Type Mismatches**: Verify data types match expected types
- **Invalid Values**: Check parameter ranges and constraints

### Debug Configuration Loading

```fortran
! Enable configuration debugging
call config%set_debug_mode(.true.)
call config%load_file("config.yml", rc)

! This will print detailed loading information:
! - File inheritance chain
! - Environment variable substitutions
! - Type conversions
! - Validation results
```

---

*The configuration system is designed to be both powerful and user-friendly, supporting everything from simple test cases to complex operational deployments.*

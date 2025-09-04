# Configuration System Developer Guide

CATChem uses a flexible YAML-based configuration system that provides type-safe parameter management, validation, and hierarchical organization. Everything you need to know about the configuration system to develop, modify, and extend CATChem is described in this guide.

## Overview

The configuration system provides:

- **Hierarchical YAML Configuration** - Organized, human-readable configuration files
- **Type-Safe Parameter Access** - Fortran interfaces with compile-time type checking
- **Runtime Validation** - Parameter bounds checking and consistency validation
- **Default Values** - Sensible defaults with optional overrides
- **Environment Variable Support** - Dynamic configuration through environment variables

## Configuration Architecture

### Configuration Manager

```fortran
module ConfigManager_Mod
  use precision_mod
  use yaml_mod
  implicit none
  private

  type, public :: ConfigManagerType
    private
    type(yaml_node), pointer :: root_node => null()
    character(len=256) :: config_file_path
    logical :: is_initialized = .false.
  contains
    procedure, public :: init => config_manager_init
    procedure, public :: get_value => config_get_value
    procedure, public :: get_array => config_get_array
    procedure, public :: has_key => config_has_key
    procedure, public :: validate => config_validate
    procedure, public :: finalize => config_manager_finalize

    ! Generic interface for different data types
    generic :: get => get_integer, get_real, get_logical, get_string
    procedure, private :: get_integer
    procedure, private :: get_real
    procedure, private :: get_logical
    procedure, private :: get_string
  end type ConfigManagerType

contains

  subroutine config_manager_init(this, config_file, rc)
    class(ConfigManagerType), intent(inout) :: this
    character(len=*), intent(in) :: config_file
    integer, intent(out) :: rc

    ! Load and parse YAML configuration file
    call yaml_load_file(config_file, this%root_node, rc)
    if (rc /= 0) return

    this%config_file_path = config_file
    this%is_initialized = .true.

    ! Validate configuration structure
    call this%validate(rc)
  end subroutine config_manager_init

end module ConfigManager_Mod
```

## Configuration File Structure

### Main Configuration File (`catchem_config.yml`)

```yaml
# CATChem Main Configuration File
simulation:
  name: "Example Atmospheric Chemistry Simulation"
  start_date: "2023-01-01T00:00:00Z"
  end_date: "2023-01-02T00:00:00Z"
  timestep: 60.0  # seconds

domain:
  nx: 100
  ny: 100
  nz: 50
  dx: 1000.0  # meters
  dy: 1000.0  # meters

# Process configuration
processes:
  - name: emissions
    enabled: true
    config_file: "emission_config.yml"

  - name: chemistry
    enabled: true
    scheme: cb6
    solver: rosenbrock
    timestep: 60.0
    parameters:
      rtol: 1.0e-3
      atol: 1.0e-8

  - name: settling
    enabled: true
    scheme: stokes
    species: [PM25, PM10, DUST_1, DUST_2, DUST_3]
    parameters:
      particle_density: 2650.0  # kg/m³

  - name: dry_deposition
    enabled: false
    scheme: resistance
    parameters:
      reference_height: 10.0  # meters
      surface_roughness: 0.1  # meters

# Species configuration
species:
  gas_phase: [O3, NO, NO2, NO3, N2O5, HNO3, CO, CO2, CH4, SO2]
  aerosol_phase: [PM25, PM10, DUST_1, DUST_2, DUST_3, DUST_4, DUST_5]

# I/O configuration
input:
  meteorology:
    file: "met_data.nc"
    format: netcdf
    variables:
      temperature: "temp"
      pressure: "pres"
      humidity: "qv"

  emissions:
    file: "emissions.nc"
    format: netcdf

output:
  frequency: hourly
  file: "catchem_output.nc"
  format: netcdf
  compression: true
  diagnostics:
    include_tendencies: true
    include_process_rates: false

# Parallel configuration
parallel:
  openmp:
    enabled: true
    num_threads: 4
  mpi:
    enabled: false

# Logging configuration
logging:
  level: info  # debug, info, warning, error
  file: "catchem.log"
  console: true
```

### Process-Specific Configuration (`emission_config.yml`)

```yaml
# Emission Process Configuration
emission_sources:
  anthropogenic:
    enabled: true
    file: "anthro_emissions.nc"
    scaling_factors:
      NOx: 1.0
      VOC: 1.2
      CO: 0.9
    temporal_profile:
      type: diurnal
      file: "temporal_profiles.nc"

  biogenic:
    enabled: true
    model: megan
    parameters:
      base_emission_factor: 1.0
      temperature_response: 0.09  # K^-1
      light_response: 0.001       # mol/mol

  dust:
    enabled: true
    scheme: ginoux
    parameters:
      threshold_velocity: 0.2  # m/s
      alpha: 1.0e-4

species_mapping:
  NO: [NO_emis]
  NO2: [NO2_emis]
  CO: [CO_emis]
  VOC: [ISOP_emis, TERP_emis]
```

## Configuration Access Patterns

### Type-Safe Parameter Access

```fortran
subroutine initialize_process_config(config_mgr, process_config)
  type(ConfigManagerType), intent(in) :: config_mgr
  type(ProcessConfigType), intent(out) :: process_config

  integer :: rc

  ! Get required parameters with validation
  call config_mgr%get('processes.chemistry.timestep', &
                      process_config%timestep, rc)
  if (rc /= 0) then
    call error_handler%log_error('Missing required parameter: timestep')
    return
  end if

  ! Get optional parameters with defaults
  call config_mgr%get('processes.chemistry.parameters.rtol', &
                      process_config%rtol, default=1.0e-3_fp, rc)

  ! Get string parameters
  call config_mgr%get('processes.chemistry.scheme', &
                      process_config%scheme, default='cb6', rc)

  ! Get array parameters
  call config_mgr%get_array('species.gas_phase', &
                           process_config%gas_species, rc)

  ! Validate parameter ranges
  if (process_config%timestep <= 0.0) then
    call error_handler%log_error('Timestep must be positive')
    rc = 1
    return
  end if
end subroutine initialize_process_config
```

### Hierarchical Parameter Access

```fortran
! Access nested parameters using dot notation
call config_mgr%get('processes.settling.parameters.particle_density', &
                    particle_density, rc)

! Check if optional sections exist
if (config_mgr%has_key('processes.settling.diagnostics')) then
  call config_mgr%get_array('processes.settling.diagnostics.outputs', &
                           diagnostic_list, rc)
end if

! Environment variable substitution
call config_mgr%get('input.meteorology.file', &
                    met_file, default='${CATCHEM_DATA}/met.nc', rc)
```

## Configuration Validation

### Built-in Validation

```fortran
module ConfigValidator_Mod
  implicit none

  type :: ConfigValidatorType
  contains
    procedure :: validate_simulation_config
    procedure :: validate_process_config
    procedure :: validate_species_config
    procedure :: validate_io_config
  end type ConfigValidatorType

contains

  subroutine validate_simulation_config(this, config_mgr, rc)
    class(ConfigValidatorType), intent(in) :: this
    type(ConfigManagerType), intent(in) :: config_mgr
    integer, intent(out) :: rc

    real(fp) :: timestep
    character(len=32) :: start_date, end_date

    rc = 0

    ! Validate required parameters exist
    if (.not. config_mgr%has_key('simulation.timestep')) then
      call log_error('Missing required parameter: simulation.timestep')
      rc = 1
      return
    end if

    ! Validate parameter values
    call config_mgr%get('simulation.timestep', timestep)
    if (timestep <= 0.0 .or. timestep > 3600.0) then
      call log_error('Timestep must be between 0 and 3600 seconds')
      rc = 1
      return
    end if

    ! Validate date format
    call config_mgr%get('simulation.start_date', start_date)
    if (.not. is_valid_iso_date(start_date)) then
      call log_error('Invalid start_date format, use ISO 8601')
      rc = 1
      return
    end if
  end subroutine validate_simulation_config

end module ConfigValidator_Mod
```

### Custom Validation Rules

```fortran
! Process-specific validation
subroutine validate_chemistry_config(config_mgr, rc)
  type(ConfigManagerType), intent(in) :: config_mgr
  integer, intent(out) :: rc

  character(len=32) :: scheme, solver
  real(fp) :: rtol, atol

  ! Validate scheme-solver compatibility
  call config_mgr%get('processes.chemistry.scheme', scheme)
  call config_mgr%get('processes.chemistry.solver', solver)

  if (scheme == 'cb6' .and. solver == 'euler') then
    call log_warning('Euler solver not recommended for CB6 mechanism')
  end if

  ! Validate solver tolerances
  call config_mgr%get('processes.chemistry.parameters.rtol', rtol)
  call config_mgr%get('processes.chemistry.parameters.atol', atol)

  if (rtol < atol) then
    call log_error('Relative tolerance must be >= absolute tolerance')
    rc = 1
    return
  end if

  rc = 0
end subroutine validate_chemistry_config
```

## Advanced Configuration Features

### Configuration Inheritance

```yaml
# Base configuration
base_settling: &settling_base
  enabled: true
  scheme: stokes
  parameters:
    particle_density: 2650.0

processes:
  # Inherit base configuration and override specific values
  dust_settling:
    <<: *settling_base
    species: [DUST_1, DUST_2, DUST_3, DUST_4, DUST_5]

  pm_settling:
    <<: *settling_base
    species: [PM25, PM10]
    parameters:
      particle_density: 1500.0  # Override for PM
```

### Conditional Configuration

```yaml
processes:
  chemistry:
    enabled: true
    scheme: ${CHEM_SCHEME:-cb6}  # Environment variable with default
    parameters:
      rtol: !expr |
        if ${HIGH_ACCURACY:-false}:
          1.0e-4
        else:
          1.0e-3
```

### Configuration Templates

```yaml
# Template definitions
templates:
  standard_settling: &standard_settling
    enabled: true
    scheme: stokes
    diagnostics: [settling_velocity, particle_flux]

  high_accuracy_chemistry: &high_accuracy
    solver: rosenbrock
    parameters:
      rtol: 1.0e-4
      atol: 1.0e-8

# Use templates
processes:
  dust_settling:
    <<: *standard_settling
    species: [DUST_1, DUST_2, DUST_3]

  pm_settling:
    <<: *standard_settling
    species: [PM25, PM10]

  chemistry:
    <<: *high_accuracy
    scheme: cb6
```

## Integration with State Management

### Configuration-Driven State Initialization

```fortran
subroutine initialize_state_from_config(config_mgr, state_container, rc)
  type(ConfigManagerType), intent(in) :: config_mgr
  type(StateContainerType), intent(inout) :: state_container
  integer, intent(out) :: rc

  character(len=32), allocatable :: gas_species(:)
  character(len=32), allocatable :: aerosol_species(:)
  integer :: nx, ny, nz

  ! Get domain dimensions
  call config_mgr%get('domain.nx', nx, rc)
  call config_mgr%get('domain.ny', ny, rc)
  call config_mgr%get('domain.nz', nz, rc)

  ! Get species lists
  call config_mgr%get_array('species.gas_phase', gas_species, rc)
  call config_mgr%get_array('species.aerosol_phase', aerosol_species, rc)

  ! Initialize state container with configuration
  call state_container%init(nx, ny, nz, gas_species, aerosol_species, rc)

  ! Configure diagnostics from config
  call configure_diagnostics_from_config(config_mgr, state_container, rc)
end subroutine initialize_state_from_config
```

## Best Practices

### 1. Configuration Organization
```yaml
# Good: Organized, hierarchical structure
simulation:
  name: "Test Case"
  timestep: 60.0

processes:
  - name: chemistry
    enabled: true
    scheme: cb6

# Avoid: Flat, unorganized structure
simulation_name: "Test Case"
timestep: 60.0
chemistry_enabled: true
chemistry_scheme: cb6
```

### 2. Parameter Validation
```fortran
! Always validate critical parameters
if (timestep <= 0.0) then
  call error_handler%log_error('Invalid timestep')
  rc = 1
  return
end if

! Provide meaningful error messages
if (.not. is_valid_scheme(scheme_name)) then
  call error_handler%log_error('Invalid scheme: ' // trim(scheme_name) // &
                               '. Available schemes: ' // available_schemes_list)
end if
```

### 3. Default Values
```fortran
! Provide sensible defaults for optional parameters
call config_mgr%get('processes.chemistry.parameters.rtol', &
                    rtol, default=1.0e-3_fp, rc)

! Document default behavior
! Default solver tolerance provides good balance of accuracy and performance
```

### 4. Environment Variable Support
```yaml
# Enable flexible deployment
input:
  meteorology:
    file: "${CATCHEM_DATA}/met_${CASE_NAME}.nc"

output:
  directory: "${CATCHEM_OUTPUT}/${CASE_NAME}"
```

This configuration system provides a robust foundation for managing complex atmospheric chemistry simulations while maintaining flexibility and usability.

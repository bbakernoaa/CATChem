# CATChem High-Level API Guide

## Table of Contents

1. [Overview](#overview)
2. [Getting Started](#getting-started)
3. [Basic Usage](#basic-usage)
4. [Advanced Features](#advanced-features)
5. [Field Mapping System](#field-mapping-system)
6. [Process-Level Diagnostics](#process-level-diagnostics)
7. [Error Handling](#error-handling)
8. [Performance Considerations](#performance-considerations)
9. [Integration Examples](#integration-examples)
10. [Migration from Legacy API](#migration-from-legacy-api)
11. [Troubleshooting](#troubleshooting)
12. [API Reference](#api-reference)

---

## Overview

The CATChem High-Level API provides a simplified, modern interface for integrating atmospheric chemistry modeling into different host modeling systems. It abstracts away the complexity of the underlying StateContainer architecture while providing clean, intuitive interfaces for the most common use cases.

### Key Features

- **Simple initialization** with sensible defaults
- **Flexible data exchange** with automatic field mapping
- **Process-level diagnostics** access
- **Column virtualization** for performance optimization
- **Comprehensive error handling** with clear messages
- **Bidirectional field mapping** between host and CATChem fields
- **Support for 1D, 2D, and 3D data** with automatic reshaping
- **Modern Fortran design** with object-oriented interfaces

### Design Philosophy

The API follows a layered approach:
- **Simple Interface**: `CATChemInstanceType` for basic use cases
- **Flexible Interface**: `FlexibleDataExchangeType` for advanced field mapping
- **Legacy Compatibility**: Access to underlying StateContainer for power users

---

## Getting Started

### Prerequisites

- Modern Fortran compiler (GFortran 9+, Intel Fortran 19+)
- CATChem core libraries
- YAML configuration support (optional but recommended)

### Basic Setup

```fortran
program simple_catchem_example
   use CATChemAPI_Mod
   implicit none

   type(CATChemInstanceType) :: catchem
   type(CATChemConfigType) :: config
   type(CATChemDataType) :: input_data, output_data
   integer :: rc

   ! Configure CATChem
   config%nx = 144
   config%ny = 91
   config%nz = 72
   config%nspecies = 10
   config%enable_dust = .true.
   config%enable_drydep = .true.

   ! Initialize
   call catchem%init(config, rc)
   if (rc /= CATCHEM_SUCCESS) then
      print *, "Failed to initialize CATChem"
      stop 1
   end if

   ! Set up your data and run...

end program
```

---

## Basic Usage

### 1. Configuration

The `CATChemConfigType` provides a simple way to configure CATChem:

```fortran
type(CATChemConfigType) :: config

! Grid dimensions
config%nx = 144           ! Longitude points
config%ny = 91            ! Latitude points
config%nz = 72            ! Vertical levels
config%nspecies = 25      ! Number of chemical species

! Time parameters
config%dt = 3600.0_fp     ! Time step in seconds
config%nsteps = 24        ! Number of time steps

! Process selection
config%enable_dust = .true.
config%enable_seasalt = .true.
config%enable_drydep = .true.
config%enable_external_emis = .true.

! Performance options
config%use_column_processing = .true.
config%enable_diagnostics = .true.

! Configuration files
config%config_file = 'catchem_config.yml'
config%species_file = 'species_list.yml'
```

### 2. Initialization

```fortran
type(CATChemInstanceType) :: catchem
integer :: rc

! Initialize with configuration
call catchem%init(config, rc)
if (rc /= CATCHEM_SUCCESS) then
   call catchem%get_error_message(error_msg)
   print *, "Initialization failed: ", trim(error_msg)
   stop 1
end if

! Setup grid coordinates (optional but recommended)
call catchem%setup_grid(lats, lons, levels, rc)
```

### 3. Adding Processes

```fortran
! Add processes to the simulation
call catchem%add_process('dust', rc=rc)
call catchem%add_process('seasalt', rc=rc)
call catchem%add_process('drydep', 'drydep_config.yml', rc)

! Check if ready to run
if (.not. catchem%is_ready_to_run()) then
   print *, "CATChem is not ready to run"
   stop 1
end if
```

### 4. Data Exchange

#### Simple Data Passing

```fortran
type(CATChemDataType) :: input_data, output_data

! Allocate and set input data
allocate(input_data%temperature(nx, ny, nz))
allocate(input_data%pressure(nx, ny, nz))
allocate(input_data%concentrations(nx, ny, nz, nspecies))

! Fill with your data...
input_data%temperature = host_temperature
input_data%pressure = host_pressure
input_data%concentrations = host_concentrations

! Run a single time step
call catchem%run_timestep(input_data, output_data, rc)

! Extract results
host_updated_concentrations = output_data%concentrations
host_dust_emissions = output_data%dust_emissions
```

#### Multiple Time Steps

```fortran
type(CATChemDataType) :: input_data(24), output_data(24)
integer :: step

! Prepare data for each time step...
do step = 1, 24
   ! Set up input_data(step)...
end do

! Run all time steps
call catchem%run_multiple_steps(24, input_data, output_data, rc)
```

---

## Advanced Features

### Field Mapping System

The field mapping system provides flexible, bidirectional data exchange between host model fields and CATChem internal fields with automatic unit conversion and dimension handling.

#### Setting Up Field Mappings

```fortran
type(FlexibleDataExchangeType) :: data_exchange
integer :: rc

! Initialize the data exchange system
call data_exchange%setup_flexible_exchange(nx, ny, nz, rc)

! Add field mappings
call catchem%add_field_mapping('met', 'temperature', 'host_temp', 'real64', rc=rc)
call catchem%add_field_mapping('met', 'pressure', 'host_pres', 'real64', 100.0_fp, rc) ! Pa to hPa conversion
call catchem%add_field_mapping('chem', 'O3', 'ozone_vmr', 'real32', rc=rc)
call catchem%add_field_mapping('emis', 'NOx_emissions', 'nox_flux', 'real64', rc=rc)
```

#### Field Mapping Configuration File

You can also define mappings in a YAML configuration file:

```yaml
# field_mappings.yml
meteorology:
  - catchem_field: temperature
    host_field: host_temp
    data_type: real64
    units: K
    conversion_factor: 1.0
    required: true

  - catchem_field: pressure
    host_field: host_pres
    data_type: real64
    units: Pa
    conversion_factor: 0.01  # Pa to hPa
    required: true

chemistry:
  - catchem_field: O3
    host_field: ozone_vmr
    data_type: real32
    units: mol/mol
    conversion_factor: 1.0
    required: false

emissions:
  - catchem_field: NOx_emissions
    host_field: nox_surface_flux
    data_type: real64
    units: mol/m2/s
    conversion_factor: 1.0
    required: false
```

Load mappings from file:

```fortran
call catchem%setup_field_mapping('field_mappings.yml', error_handler)
```

#### Using Flexible Data Exchange

```fortran
! Fill CATChem states from host data
call catchem%fill_met_state_from_host(data_exchange, rc)
call catchem%fill_chem_state_from_host(data_exchange, rc)
call catchem%fill_emis_state_from_host(data_exchange, rc)

! Run processes...
call catchem%run_timestep(input_data, output_data, rc)

! Extract results back to host
call catchem%extract_met_state_to_host(data_exchange, rc)
call catchem%extract_chem_state_to_host(data_exchange, rc)
call catchem%extract_diagnostics_to_host(data_exchange, rc)
```

---

## Process-Level Diagnostics

CATChem provides detailed process-level diagnostics that can be accessed through the high-level API.

### Discovering Available Diagnostics

```fortran
character(len=64), allocatable :: diagnostic_names(:)
character(len=32), allocatable :: process_names(:)
character(len=256), allocatable :: diagnostic_info(:)

! List all processes with diagnostics
call catchem%list_process_diagnostics(process_names, diagnostic_info, rc)

do i = 1, size(diagnostic_info)
   print *, trim(diagnostic_info(i))
end do

! Get diagnostics for a specific process
call catchem%get_available_diagnostics('dust', diagnostic_names, rc)
```

### Accessing Specific Diagnostics

```fortran
type(CATChemDiagnosticType) :: diagnostic

! Get a specific diagnostic field
call catchem%get_process_diagnostic('dust', 'emission_flux', diagnostic, rc)

if (diagnostic%is_available) then
   print *, "Field: ", trim(diagnostic%field_name)
   print *, "Description: ", trim(diagnostic%description)
   print *, "Units: ", trim(diagnostic%units)
   print *, "Dimensions: ", diagnostic%dimensions

   ! Access the data
   if (diagnostic%dimensions == 3) then
      ! Use diagnostic%data_3d
   else if (diagnostic%dimensions == 2) then
      ! Use diagnostic%data_2d
   end if
end if
```

### Getting All Diagnostics for a Process

```fortran
type(CATChemDiagnosticType), allocatable :: diagnostics(:)

call catchem%get_all_process_diagnostics('dust', diagnostics, rc)

do i = 1, size(diagnostics)
   if (diagnostics(i)%is_available) then
      print *, "Available: ", trim(diagnostics(i)%field_name)
   end if
end do
```

---

## Error Handling

The API provides comprehensive error handling with clear, actionable error messages.

### Return Codes

```fortran
integer, parameter :: CATCHEM_SUCCESS = 0
integer, parameter :: CATCHEM_FAILURE = -1
```

### Getting Error Information

```fortran
integer :: rc
character(len=256) :: error_msg

call catchem%some_operation(rc)
if (rc /= CATCHEM_SUCCESS) then
   call catchem%get_error_message(error_msg)
   print *, "Error: ", trim(error_msg)
   ! Handle the error appropriately
end if
```

### Validation

```fortran
type(CATChemDataType) :: data

! Validate data before use
call catchem%validate_data(data, rc)
if (rc /= CATCHEM_SUCCESS) then
   print *, "Data validation failed"
   ! Fix data issues...
end if
```

---

## Performance Considerations

### Column Virtualization

Column virtualization improves performance by processing data in column-wise chunks:

```fortran
config%use_column_processing = .true.  ! Enable (default)
```

### Memory Management

The API handles memory management automatically, but you can optimize by:

1. **Reusing data structures** between time steps
2. **Pre-allocating arrays** to expected sizes
3. **Using appropriate precision** (real32 vs real64)

### Parallel Processing

The API is designed to be thread-safe for column-wise parallelization:

```fortran
!$OMP PARALLEL DO
do col = 1, num_columns
   call process_column(col, ...)
end do
!$OMP END PARALLEL DO
```

---

## Integration Examples

### Example 1: Weather Research and Forecasting (WRF) Integration

```fortran
module wrf_catchem_interface
   use CATChemAPI_Mod
   implicit none

   type :: WRFCATChemType
      type(CATChemInstanceType) :: catchem
      type(FlexibleDataExchangeType) :: data_exchange
      logical :: initialized = .false.
   end type

contains

   subroutine initialize_wrf_catchem(this, wrf_config)
      type(WRFCATChemType), intent(inout) :: this
      type(WRFConfigType), intent(in) :: wrf_config

      type(CATChemConfigType) :: catchem_config
      integer :: rc

      ! Map WRF configuration to CATChem
      catchem_config%nx = wrf_config%nx
      catchem_config%ny = wrf_config%ny
      catchem_config%nz = wrf_config%nz
      catchem_config%nspecies = wrf_config%num_chem_species
      catchem_config%dt = wrf_config%dt

      ! Initialize CATChem
      call this%catchem%init(catchem_config, rc)

      ! Set up field mappings
      call setup_wrf_field_mappings(this)

      this%initialized = .true.
   end subroutine

   subroutine run_wrf_chemistry(this, wrf_state)
      type(WRFCATChemType), intent(inout) :: this
      type(WRFStateType), intent(inout) :: wrf_state

      ! Map WRF data to CATChem
      call map_wrf_to_catchem(wrf_state, this%data_exchange)

      ! Run chemistry
      call this%catchem%fill_met_state_from_host(this%data_exchange, rc)
      call this%catchem%fill_chem_state_from_host(this%data_exchange, rc)
      call this%catchem%run_timestep(input_data, output_data, rc)
      call this%catchem%extract_chem_state_to_host(this%data_exchange, rc)

      ! Map results back to WRF
      call map_catchem_to_wrf(this%data_exchange, wrf_state)
   end subroutine

end module
```

### Example 2: GEOS-Chem Integration

```fortran
module geos_catchem_bridge
   use CATChemAPI_Mod
   implicit none

contains

   subroutine geos_chem_integration()
      type(CATChemInstanceType) :: catchem
      type(CATChemConfigType) :: config
      type(CATChemDataType) :: input_data, output_data
      integer :: rc

      ! Configure for GEOS-Chem grid
      config%nx = 144
      config%ny = 91
      config%nz = 72
      config%nspecies = size(geos_species_list)
      config%enable_dust = .true.
      config%enable_seasalt = .true.
      config%enable_drydep = .true.

      call catchem%init(config, rc)
      call catchem%setup_grid(geos_lats, geos_lons, geos_levels, rc)

      ! Add relevant processes
      call catchem%add_process('dust', 'geos_dust_config.yml', rc)
      call catchem%add_process('seasalt', 'geos_seasalt_config.yml', rc)
      call catchem%add_process('drydep', 'geos_drydep_config.yml', rc)

      ! Time stepping loop
      do step = 1, num_time_steps
         ! Prepare input data from GEOS-Chem state
         call prepare_geos_input(input_data, geos_state, step)

         ! Run CATChem
         call catchem%run_timestep(input_data, output_data, rc)

         ! Update GEOS-Chem state
         call update_geos_state(output_data, geos_state, step)
      end do

      call catchem%finalize(rc)
   end subroutine

end module
```

---

## Migration from Legacy API

### Key Differences

| Legacy API | New High-Level API |
|------------|-------------------|
| Complex StateContainer setup | Simple `CATChemConfigType` |
| Manual process management | Automatic process handling |
| Direct state access required | Clean data exchange types |
| Limited error information | Comprehensive error handling |
| Fixed field mappings | Flexible field mapping system |

### Migration Steps

1. **Replace StateContainer setup**:
   ```fortran
   ! Old way
   type(StateContainerType) :: container
   type(StateBuilderType) :: builder
   ! Complex setup...

   ! New way
   type(CATChemInstanceType) :: catchem
   type(CATChemConfigType) :: config
   call catchem%init(config, rc)
   ```

2. **Simplify process management**:
   ```fortran
   ! Old way
   call process_manager%register_process(dust_process)
   call process_manager%initialize_process('dust', container)

   ! New way
   call catchem%add_process('dust', rc=rc)
   ```

3. **Use simplified data exchange**:
   ```fortran
   ! Old way
   call met_state%set_field('temperature', temp_data)
   call chem_state%set_species_concentration('O3', ozone_data)

   ! New way
   input_data%temperature = temp_data
   input_data%concentrations(:,:,:,ozone_index) = ozone_data
   call catchem%run_timestep(input_data, output_data, rc)
   ```

---

## Troubleshooting

### Common Issues

#### 1. Initialization Failures

**Problem**: CATChem fails to initialize
```
Error: Grid dimensions must be positive
```

**Solution**: Check configuration parameters
```fortran
! Ensure all dimensions are positive
config%nx = 144  ! Must be > 0
config%ny = 91   ! Must be > 0
config%nz = 72   ! Must be > 0
config%nspecies = 25  ! Must be > 0
```

#### 2. Data Validation Errors

**Problem**: Data validation fails
```
Error: Array dimensions do not match grid configuration
```

**Solution**: Ensure data arrays match configured dimensions
```fortran
! Check array sizes match configuration
if (size(input_data%temperature, 1) /= config%nx) then
   print *, "Temperature array x-dimension mismatch"
end if
```

#### 3. Process Configuration Issues

**Problem**: Process fails to load
```
Error: Process configuration file not found
```

**Solution**: Check file paths and format
```fortran
! Use absolute paths or ensure files are in working directory
call catchem%add_process('dust', '/full/path/to/dust_config.yml', rc)
```

#### 4. Field Mapping Problems

**Problem**: Field mapping fails
```
Error: Unknown CATChem field: 'temp'
```

**Solution**: Use correct field names
```fortran
! Check available field names in documentation
call catchem%add_field_mapping('met', 'temperature', 'host_temp', 'real64', rc=rc)
! Not: 'temp' -> Use: 'temperature'
```

### Debugging Tips

1. **Enable detailed error reporting**:
   ```fortran
   config%enable_diagnostics = .true.
   ```

2. **Check return codes**:
   ```fortran
   call catchem%some_operation(rc)
   if (rc /= CATCHEM_SUCCESS) then
      call catchem%get_error_message(error_msg)
      print *, "Debug: ", trim(error_msg)
   end if
   ```

3. **Validate data regularly**:
   ```fortran
   call catchem%validate_data(input_data, rc)
   ```

4. **Use process diagnostics**:
   ```fortran
   call catchem%list_process_diagnostics(processes, info, rc)
   ```

---

## API Reference

### Types

#### CATChemConfigType
Configuration type for basic CATChem setup.

**Components:**
- `nx, ny, nz` - Grid dimensions
- `nspecies` - Number of chemical species
- `dt` - Time step [s]
- `enable_*` - Process enable flags
- `use_column_processing` - Performance option
- `enable_diagnostics` - Diagnostic enable flag
- `config_file` - Main configuration file path

#### CATChemDataType
Simple data exchange type.

**Components:**
- `temperature(:,:,:)` - Temperature [K]
- `pressure(:,:,:)` - Pressure [Pa]
- `humidity(:,:,:)` - Specific humidity [kg/kg]
- `wind_u(:,:,:)`, `wind_v(:,:,:)` - Wind components [m/s]
- `concentrations(:,:,:,:)` - Species concentrations [mol/mol]
- `emission_rates(:,:,:,:)` - Emission rates [mol/m²/s]
- Diagnostic outputs: `dust_emissions`, `seasalt_emissions`, `drydep_velocity`

#### CATChemInstanceType
Main CATChem instance type.

**Key Methods:**
- `init(config, rc)` - Initialize with configuration
- `setup_grid(lats, lons, levels, rc)` - Setup grid geometry
- `add_process(name, config, rc)` - Add a process
- `run_timestep(input, output, rc)` - Run single time step
- `run_multiple_steps(n, input, output, rc)` - Run multiple steps
- `get_concentrations(data, rc)` - Get updated concentrations
- `get_diagnostics(data, rc)` - Get diagnostic data
- `finalize(rc)` - Clean up and finalize

#### FlexibleDataExchangeType
Enhanced data exchange with field mapping.

**Components:**
- `field_mapping_registry` - Field mapping registry
- Dynamic data storage arrays with field name mappings
- Dimension tracking

#### FieldMappingType
Field mapping definition.

**Components:**
- `host_field_name` - Host model field name
- `catchem_field_name` - CATChem internal field name
- `field_type` - Type: 'met', 'chem', 'emis', 'diag'
- `units` - Field units
- `scale_factor` - Unit conversion factor
- `offset` - Unit conversion offset
- `is_required` - Whether field is required
- `is_3d` - Whether field is 3D

### Constants

```fortran
integer, parameter :: CATCHEM_SUCCESS = 0
integer, parameter :: CATCHEM_FAILURE = -1
```

### Error Handling

All routines return an integer return code (`rc`). Use `CATCHEM_SUCCESS` and `CATCHEM_FAILURE` constants for checking success/failure.

Get detailed error messages using:
```fortran
call instance%get_error_message(error_message)
```

---

## Best Practices

1. **Always check return codes** after API calls
2. **Validate input data** before processing
3. **Use appropriate precision** for your application
4. **Enable column processing** for better performance
5. **Set up field mappings** for complex integrations
6. **Use process diagnostics** for debugging and validation
7. **Clean up resources** with `finalize()` when done
8. **Test with simple configurations** before scaling up

---

## Support and Contributing

- **Documentation**: Additional examples in `docs/examples/`
- **Issues**: Report bugs and feature requests in the project repository
- **Development**: See `docs/develop/developers-guide.rst` for contributing guidelines

---

*This guide covers CATChem API version 2.0. For legacy API documentation, see `docs/legacy/`.*

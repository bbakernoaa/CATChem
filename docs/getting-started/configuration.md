# Configuration Guide

Learn how to configure Canopy-App for your specific modeling needs.

## Overview

Canopy-App uses a Fortran namelist file for configuration. The main configuration file is typically named `namelist.canopy` and contains several namelist groups.

## Namelist Structure

```fortran
&canopy_inputs
    ! Input/Output settings
/

&canopy_options
    ! Model options and switches
/

&canopy_physics
    ! Physical parameterization settings
/
```

## Input/Output Configuration

### Basic I/O Settings

```fortran
&canopy_inputs
    ! Date and time
    in_date = '20220701'        ! Input date (YYYYMMDD)
    in_time = '00'              ! Input time (HH)

    ! Input files
    file_in = 'input_data.nc'   ! Main input file
    infmt_opt = 1               ! Input format (1=NetCDF, 2=text)

    ! Output files
    file_out = 'output.nc'      ! Main output file
    outfmt_opt = 1              ! Output format (1=NetCDF, 2=text)

    ! Optional point data
    file_pnt = 'point_data.txt' ! Point output file
/
```

### File Format Options

| Option | Value | Description |
|--------|-------|-------------|
| `infmt_opt` | 1 | NetCDF input format |
| `infmt_opt` | 2 | Text input format |
| `outfmt_opt` | 1 | NetCDF output format |
| `outfmt_opt` | 2 | Text output format |

## Model Options

### Physics Switches

```fortran
&canopy_options
    ! Core physics
    opt_canmet  = 1             ! Canopy meteorology (0=off, 1=on)
    opt_bioem   = 1             ! Biogenic emissions (0=off, 1=on)
    opt_drydep  = 1             ! Dry deposition (0=off, 1=on)
    opt_phot    = 1             ! Photolysis rates (0=off, 1=on)

    ! Radiation options
    opt_rad     = 1             ! Radiation transfer (0=off, 1=on)
    opt_solarzen = 1            ! Solar zenith angle calc (0=off, 1=on)

    ! Chemical options
    opt_chem    = 0             ! Chemistry (0=off, 1=on)
/
```

### Advanced Options

```fortran
&canopy_physics
    ! Canopy structure
    z_canbot    = 0.0           ! Canopy bottom height (m)
    z_cantop    = 20.0          ! Canopy top height (m)
    ncanlevs    = 10            ! Number of canopy levels

    ! Meteorology
    stability_opt = 1           ! Stability correction (1=on, 0=off)

    ! Emissions
    bioem_opt   = 1             ! Biogenic emission algorithm
    temp_opt    = 1             ! Temperature dependence option

    ! Deposition
    drydep_opt  = 1             ! Dry deposition algorithm
    stom_opt    = 1             ! Stomatal resistance option
/
```

## Configuration Examples

### Basic Research Setup

```fortran
&canopy_inputs
    in_date = '20220701'
    in_time = '12'
    file_in = 'gfs_input.nc'
    file_out = 'canopy_research.nc'
    infmt_opt = 1
    outfmt_opt = 1
/

&canopy_options
    opt_canmet = 1
    opt_bioem = 1
    opt_drydep = 1
    opt_phot = 1
    opt_rad = 1
/
```

### Production Run Setup

```fortran
&canopy_inputs
    in_date = '20220701'
    in_time = '00'
    file_in = 'operational_input.nc'
    file_out = 'operational_output.nc'
    file_pnt = 'point_output.txt'
    infmt_opt = 1
    outfmt_opt = 1
/

&canopy_options
    opt_canmet = 1
    opt_bioem = 1
    opt_drydep = 1
    opt_phot = 1
    opt_rad = 1
    opt_solarzen = 1
/

&canopy_physics
    z_cantop = 25.0
    ncanlevs = 15
    stability_opt = 1
    bioem_opt = 2
    drydep_opt = 2
/
```

### Sensitivity Study Setup

```fortran
&canopy_inputs
    in_date = '20220701'
    in_time = '12'
    file_in = 'sensitivity_input.nc'
    file_out = 'sensitivity_test.nc'
/

&canopy_options
    opt_canmet = 1
    opt_bioem = 1
    opt_drydep = 0    ! Turn off dry deposition
    opt_phot = 1
    opt_rad = 1
/

&canopy_physics
    z_cantop = 15.0   ! Lower canopy height
    ncanlevs = 8      ! Fewer levels for speed
    bioem_opt = 1     ! Basic emission algorithm
/
```

## Validation and Testing

### Configuration Validation

The model performs several validation checks:

- Date and time format validation
- File existence checks
- Parameter range validation
- Physics option compatibility

### Testing Your Configuration

```bash
# Test configuration without full run
./canopy_app --check-config

# Run with verbose output
./canopy_app --verbose

# Dry run (parse inputs only)
./canopy_app --dry-run
```

## Common Configuration Issues

!!! error "File Not Found"
    **Problem**: Input files not found
    **Solution**: Check file paths are relative to run directory

!!! warning "Parameter Out of Range"
    **Problem**: Physics parameters outside valid ranges
    **Solution**: Check parameter documentation and valid ranges

!!! tip "Performance Tuning"
    **Tip**: Adjust `ncanlevs` based on computational resources and accuracy needs

## Advanced Configuration

### Environment Variables

```bash
# Set NetCDF library path
export NETCDF=/usr/local/netcdf

# Set number of OpenMP threads
export OMP_NUM_THREADS=4

# Set stack size for large arrays
ulimit -s unlimited
```

### Custom Physics Parameters

For advanced users, physics parameters can be modified in the source code modules:

- `canopy_const_mod.F90` - Physical constants
- `canopy_bioparm_mod.F90` - Biogenic emission parameters
- `canopy_canopts_mod.F90` - Model options and defaults

## Next Steps

- [Learn about input file formats](../user-guide/input-files.md)
- [Understand model physics](../science/model-description.md)
- [Explore examples](../examples/basic.md)
- [Check troubleshooting guide](../user-guide/troubleshooting.md)

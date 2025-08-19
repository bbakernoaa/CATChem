# Output Files

CATChem generates various types of output files containing simulation results, diagnostics, and metadata. This guide describes the output formats, file organization, and how to work with CATChem output data.

## Overview

CATChem output files are organized into several categories:

- **Concentration files** - Species concentrations and mixing ratios
- **Diagnostic files** - Process rates, tendencies, and internal variables
- **Restart files** - Complete model state for continuation runs
- **Log files** - Runtime information and error messages
- **Monitoring files** - Performance and system metrics

## File Organization

### Default Directory Structure

```
output/
├── concentrations/
│   ├── catchem_conc_20240115_00.nc
│   ├── catchem_conc_20240115_01.nc
│   └── ...
├── diagnostics/
│   ├── catchem_diag_20240115_00.nc
│   ├── catchem_diag_20240115_01.nc
│   └── ...
├── restart/
│   ├── restart_20240115_0000.nc
│   ├── restart_20240115_0600.nc
│   └── ...
├── logs/
│   ├── catchem.log
│   ├── catchem_diagnostics.log
│   └── performance.log
└── monitoring/
    ├── memory_usage.txt
    ├── timing_summary.txt
    └── load_balance.txt
```

### File Naming Conventions

**Configuration:**
```yaml
output:
  directory: "output/"

  concentrations:
    file_template: "catchem_conc_%Y%m%d_%H.nc"
    frequency: 3600  # seconds

  diagnostics:
    file_template: "catchem_diag_%Y%m%d_%H.nc"
    frequency: 3600

  restart:
    file_template: "restart_%Y%m%d_%H%M.nc"
    frequency: 21600  # every 6 hours
```

**Template Variables:**
- `%Y` - 4-digit year (2024)
- `%m` - 2-digit month (01-12)
- `%d` - 2-digit day (01-31)
- `%H` - 2-digit hour (00-23)
- `%M` - 2-digit minute (00-59)

## Concentration Files

### File Format

Concentration files contain atmospheric species concentrations and related variables:

```bash
ncdump -h catchem_conc_20240115_12.nc
```

```
netcdf catchem_conc_20240115_12 {
dimensions:
    time = 1 ;
    lev = 50 ;
    lat = 80 ;
    lon = 100 ;
    species = 89 ;

variables:
    double time(time) ;
        time:units = "hours since 2024-01-15 00:00:00" ;
        time:calendar = "gregorian" ;
        time:standard_name = "time" ;

    float lev(lev) ;
        lev:units = "Pa" ;
        lev:standard_name = "air_pressure" ;
        lev:positive = "down" ;

    float lat(lat) ;
        lat:units = "degrees_north" ;
        lat:standard_name = "latitude" ;

    float lon(lon) ;
        lon:units = "degrees_east" ;
        lon:standard_name = "longitude" ;

    char species_names(species, 32) ;
        species_names:long_name = "Chemical species names" ;

    // Concentration variables
    float O3(time, lev, lat, lon) ;
        O3:units = "mol/mol" ;
        O3:standard_name = "mole_fraction_of_ozone_in_air" ;
        O3:long_name = "Ozone mixing ratio" ;
        O3:_FillValue = -999.f ;

    float NO2(time, lev, lat, lon) ;
        NO2:units = "mol/mol" ;
        NO2:standard_name = "mole_fraction_of_nitrogen_dioxide_in_air" ;
        NO2:long_name = "Nitrogen dioxide mixing ratio" ;

    float SO2(time, lev, lat, lon) ;
        SO2:units = "mol/mol" ;
        SO2:standard_name = "mole_fraction_of_sulfur_dioxide_in_air" ;
        SO2:long_name = "Sulfur dioxide mixing ratio" ;

    float PM25(time, lev, lat, lon) ;
        PM25:units = "kg/m3" ;
        PM25:long_name = "PM2.5 mass concentration" ;

    // Meteorological variables (if requested)
    float temperature(time, lev, lat, lon) ;
        temperature:units = "K" ;
        temperature:standard_name = "air_temperature" ;

    float pressure(time, lev, lat, lon) ;
        pressure:units = "Pa" ;
        pressure:standard_name = "air_pressure" ;

// Global attributes
:Conventions = "CF-1.8" ;
:title = "CATChem Atmospheric Chemistry Simulation" ;
:source = "CATChem v2.1.0" ;
:history = "Created on 2024-01-15 12:00:00 UTC" ;
:references = "https://github.com/NOAA-GSL/CATChem" ;
:simulation_start_time = "2024-01-15T00:00:00Z" ;
:simulation_end_time = "2024-01-16T00:00:00Z" ;
:domain_description = "Regional domain: 25-45N, 130-105W" ;
}
```

### Variable Types

**Gas-phase Species:**
- Units: `mol/mol` (mole fraction) or `ppbv` (parts per billion by volume)
- Examples: O3, NO, NO2, CO, SO2, NH3, HCHO, C2H6, etc.

**Aerosol Species:**
- Mass concentration: `kg/m3` or `μg/m3`
- Number concentration: `#/m3`
- Examples: PM2.5, PM10, SO4, NO3, NH4, BC, OC, dust, sea salt

**Derived Variables:**
- Total PM2.5: Sum of all aerosol species < 2.5 μm
- Total PM10: Sum of all aerosol species < 10 μm
- AOD (Aerosol Optical Depth): Column-integrated extinction

### Configuration Options

**Basic Configuration:**
```yaml
output:
  concentrations:
    enabled: true
    frequency: 3600  # hourly output
    variables:
      gas_phase: ["O3", "NO2", "SO2", "CO", "NH3"]
      aerosol: ["PM25", "PM10", "SO4", "NO3", "NH4"]
      meteorology: ["temperature", "pressure", "humidity"]
```

**Advanced Configuration:**
```yaml
output:
  concentrations:
    enabled: true
    frequency: 3600

    # Variable selection
    variables:
      all_species: false  # Output all species
      gas_phase: ["O3", "NO2", "SO2", "CO"]
      aerosol: ["PM25", "SO4", "NO3", "NH4", "BC", "OC"]
      meteorology: ["temperature", "pressure", "humidity"]

    # Vertical levels
    vertical_levels:
      type: "pressure"  # pressure, sigma, height
      levels: [1000, 925, 850, 700, 500, 300, 200, 100]  # hPa

    # Spatial subsetting
    domain:
      lat_min: 30.0
      lat_max: 40.0
      lon_min: -120.0
      lon_max: -110.0

    # Temporal averaging
    time_averaging:
      enabled: true
      method: "mean"  # mean, max, min, instantaneous
      window: 3600  # seconds

    # Quality control
    quality_control:
      range_check: true
      fill_missing: true
      compression: true
      compression_level: 4
```

## Diagnostic Files

### Process Diagnostics

Diagnostic files contain detailed process information:

```
netcdf catchem_diag_20240115_12 {
dimensions:
    time = 1 ;
    lev = 50 ;
    lat = 80 ;
    lon = 100 ;
    process = 10 ;
    reaction = 150 ;

variables:
    // Chemistry diagnostics
    float reaction_rates(time, reaction, lev, lat, lon) ;
        reaction_rates:units = "mol/m3/s" ;
        reaction_rates:long_name = "Chemical reaction rates" ;

    float O3_tendency_chemistry(time, lev, lat, lon) ;
        O3_tendency_chemistry:units = "mol/mol/s" ;
        O3_tendency_chemistry:long_name = "O3 tendency from chemistry" ;

    float NO2_tendency_chemistry(time, lev, lat, lon) ;
        NO2_tendency_chemistry:units = "mol/mol/s" ;

    // Emission diagnostics
    float NOx_emission_rate(time, lat, lon) ;
        NOx_emission_rate:units = "mol/m2/s" ;
        NOx_emission_rate:long_name = "NOx emission rate" ;

    float SO2_emission_rate(time, lat, lon) ;
        SO2_emission_rate:units = "mol/m2/s" ;

    // Deposition diagnostics
    float O3_dry_deposition_velocity(time, lat, lon) ;
        O3_dry_deposition_velocity:units = "m/s" ;
        O3_dry_deposition_velocity:long_name = "O3 dry deposition velocity" ;

    float O3_dry_deposition_flux(time, lat, lon) ;
        O3_dry_deposition_flux:units = "mol/m2/s" ;

    // Process timing
    float process_cpu_time(time, process) ;
        process_cpu_time:units = "seconds" ;
        process_cpu_time:long_name = "CPU time per process" ;
}
```

### Configuration

```yaml
output:
  diagnostics:
    enabled: true
    frequency: 3600

    chemistry:
      reaction_rates: true
      production_rates: true
      loss_rates: true
      tendencies: ["O3", "NO2", "SO2"]

    emissions:
      emission_rates: true
      by_sector: true
      by_species: ["NOx", "SO2", "CO", "NH3"]

    deposition:
      dry_deposition:
        velocities: true
        fluxes: true
        resistance: true
      wet_deposition:
        scavenging_rates: true
        washout_rates: true

    transport:
      vertical_mixing: true
      horizontal_advection: false
      settling_velocities: true

    performance:
      timing: true
      memory_usage: true
      load_balance: true
```

## Restart Files

### File Format

Restart files contain complete model state for continuation runs:

```
netcdf restart_20240115_1200 {
dimensions:
    time = 1 ;
    lev = 50 ;
    lat = 80 ;
    lon = 100 ;
    species = 89 ;

variables:
    // Complete species state
    float species_concentrations(species, lev, lat, lon) ;
        species_concentrations:units = "mol/mol" ;

    // Meteorological state
    float temperature(lev, lat, lon) ;
    float pressure(lev, lat, lon) ;
    float humidity(lev, lat, lon) ;

    // Process-specific state
    float aerosol_number(lev, lat, lon) ;
    float cloud_water(lev, lat, lon) ;

    // Boundary conditions
    float boundary_conditions(species, lev, lat, lon) ;

    // Model configuration
    int model_timestep ;
    double simulation_time ;
    char configuration_hash(64) ;
}
```

### Configuration

```yaml
output:
  restart:
    enabled: true
    frequency: 21600  # every 6 hours
    directory: "restart/"

    # Compression to save space
    compression: true
    compression_level: 6

    # Cleanup old restart files
    retention:
      keep_count: 4  # Keep last 4 restart files
      cleanup_frequency: 24  # Check every 24 hours
```

## Log Files

### Main Log File

The main log file (`catchem.log`) contains runtime information:

```
2024-01-15 12:00:00 [INFO] CATChem v2.1.0 starting
2024-01-15 12:00:00 [INFO] Configuration file: config/catchem_config.yml
2024-01-15 12:00:00 [INFO] Domain: 100x80x50 grid points
2024-01-15 12:00:00 [INFO] Simulation period: 2024-01-15 00:00 to 2024-01-16 00:00
2024-01-15 12:00:01 [INFO] Processes enabled: chemistry, emissions, dry_deposition
2024-01-15 12:00:01 [INFO] MPI: 16 processes, 2D decomposition (4x4)
2024-01-15 12:00:01 [INFO] OpenMP: 4 threads per process
2024-01-15 12:00:02 [INFO] Input data validation complete
2024-01-15 12:00:02 [INFO] Model initialization complete
2024-01-15 12:00:02 [INFO] Starting time integration
2024-01-15 12:00:03 [INFO] Timestep 1/288 (2024-01-15 00:05:00)
2024-01-15 12:00:04 [INFO] Timestep 2/288 (2024-01-15 00:10:00)
...
2024-01-15 12:05:00 [INFO] Timestep 60/288 (2024-01-15 05:00:00)
2024-01-15 12:05:00 [INFO] Hourly output written: output/catchem_conc_20240115_05.nc
2024-01-15 12:05:00 [INFO] Performance: 1.2 seconds per timestep
```

### Diagnostic Log File

Detailed diagnostic information (`catchem_diagnostics.log`):

```
2024-01-15 12:00:00 [DIAG] Memory usage: 2.4 GB
2024-01-15 12:00:00 [DIAG] Load balance: 95% efficiency
2024-01-15 12:00:00 [DIAG] Chemistry solver: 150 iterations average
2024-01-15 12:00:00 [DIAG] I/O time: 0.1 seconds per output
2024-01-15 12:00:00 [DIAG] Process timing (seconds):
2024-01-15 12:00:00 [DIAG]   Chemistry: 0.85
2024-01-15 12:00:00 [DIAG]   Emissions: 0.15
2024-01-15 12:00:00 [DIAG]   Deposition: 0.08
2024-01-15 12:00:00 [DIAG]   Transport: 0.12
```

## Working with Output Files

### Python Analysis

**Basic Data Reading:**
```python
import xarray as xr
import numpy as np
import matplotlib.pyplot as plt

# Read concentration file
ds = xr.open_dataset('output/catchem_conc_20240115_12.nc')

# Print basic information
print(f"Variables: {list(ds.data_vars)}")
print(f"Dimensions: {ds.dims}")
print(f"Time range: {ds.time.min().values} to {ds.time.max().values}")

# Extract surface ozone
o3_surface = ds.O3.isel(lev=0)  # Surface level
print(f"Surface O3 range: {o3_surface.min().values:.2e} to {o3_surface.max().values:.2e}")

# Simple plot
o3_surface.plot()
plt.title('Surface Ozone Concentration')
plt.savefig('surface_o3.png')
plt.show()
```

**Time Series Analysis:**
```python
# Read multiple files
files = ['output/catchem_conc_20240115_{:02d}.nc'.format(h) for h in range(24)]
ds = xr.open_mfdataset(files, concat_dim='time')

# Calculate domain average
o3_avg = ds.O3.mean(dim=['lat', 'lon'])

# Plot vertical profile
o3_avg.isel(time=12).plot(y='lev', yincrease=False)
plt.xlabel('O3 mixing ratio (mol/mol)')
plt.ylabel('Pressure (Pa)')
plt.title('Ozone Vertical Profile at 12:00 UTC')
plt.savefig('o3_profile.png')
```

**Spatial Analysis:**
```python
# Calculate maximum values
o3_max = ds.O3.max(dim='time')

# Plot spatial distribution
fig, ax = plt.subplots(figsize=(10, 8))
o3_max.isel(lev=0).plot(ax=ax, cmap='viridis')
ax.set_title('Maximum Surface Ozone')
plt.savefig('o3_max_spatial.png')

# Calculate statistics
print(f"Domain mean: {ds.O3.mean().values:.2e}")
print(f"Domain std: {ds.O3.std().values:.2e}")
print(f"Domain max: {ds.O3.max().values:.2e}")
```

### NCO/CDO Analysis

**Basic Operations:**
```bash
# Calculate time averages
cdo timemean catchem_conc_20240115_*.nc daily_mean.nc

# Extract specific variables
ncks -v O3,NO2,SO2 catchem_conc_20240115_12.nc subset.nc

# Calculate spatial averages
cdo fldmean catchem_conc_20240115_12.nc spatial_mean.nc

# Extract vertical levels
cdo sellevel,1000,925,850 catchem_conc_20240115_12.nc lower_levels.nc
```

**Advanced Analysis:**
```bash
# Calculate monthly means
cdo monmean catchem_conc_202401*.nc monthly_mean.nc

# Compute differences
cdo sub simulation1.nc simulation2.nc difference.nc

# Calculate percentiles
cdo timpctl,95 catchem_conc_*.nc percentile_95.nc

# Regrid to different resolution
cdo remapbil,target_grid.txt input.nc output_regridded.nc
```

### R Analysis

**Basic Reading:**
```r
library(ncdf4)
library(fields)
library(ggplot2)

# Open NetCDF file
nc <- nc_open("output/catchem_conc_20240115_12.nc")

# Read variables
o3 <- ncvar_get(nc, "O3")
lat <- ncvar_get(nc, "lat")
lon <- ncvar_get(nc, "lon")

# Close file
nc_close(nc)

# Plot surface ozone
image.plot(lon, lat, o3[,,1,1],
           main="Surface Ozone",
           xlab="Longitude",
           ylab="Latitude")
```

## Data Validation

### Quality Control Checks

**Automatic Validation:**
```python
def validate_output(filename):
    """Validate CATChem output file"""

    ds = xr.open_dataset(filename)

    # Check for missing data
    missing_count = ds.isnull().sum().sum()
    if missing_count > 0:
        print(f"Warning: {missing_count} missing values found")

    # Check for reasonable ranges
    for var in ['O3', 'NO2', 'SO2']:
        if var in ds.variables:
            values = ds[var].values
            if np.any(values < 0):
                print(f"Warning: Negative values in {var}")
            if np.any(values > 1e-3):  # 1000 ppmv
                print(f"Warning: Extremely high values in {var}")

    # Check CF compliance
    try:
        assert 'units' in ds.O3.attrs
        assert 'standard_name' in ds.O3.attrs
        print("CF compliance check passed")
    except AssertionError:
        print("Warning: CF compliance issues found")

    print(f"Validation complete for {filename}")

# Validate all output files
import glob
for file in glob.glob("output/catchem_conc_*.nc"):
    validate_output(file)
```

**Manual Validation:**
```bash
# Check file integrity
ncdump -t output/catchem_conc_20240115_12.nc > /dev/null
echo "File integrity: $?"

# Validate CF conventions
cfchecker output/catchem_conc_20240115_12.nc

# Check for NaN values
python -c "
import xarray as xr
import numpy as np
ds = xr.open_dataset('output/catchem_conc_20240115_12.nc')
nan_count = np.isnan(ds.to_array()).sum()
print(f'NaN values: {nan_count.values}')
"
```

## Performance Monitoring

### Output Performance

**I/O Timing:**
```yaml
output:
  performance:
    timing: true
    io_buffer_size: 64  # MB
    parallel_io: true

  monitoring:
    file_sizes: true
    write_times: true
    compression_ratios: true
```

**Monitoring Script:**
```python
import time
import os
import glob

def monitor_output_performance():
    """Monitor output file performance"""

    output_dir = "output/"

    while True:
        # Check file sizes
        files = glob.glob(f"{output_dir}/*.nc")
        total_size = sum(os.path.getsize(f) for f in files)

        print(f"Total output size: {total_size/1e9:.2f} GB")
        print(f"Number of files: {len(files)}")

        # Check recent files
        recent_files = [f for f in files
                       if time.time() - os.path.getmtime(f) < 3600]
        print(f"Files written in last hour: {len(recent_files)}")

        time.sleep(300)  # Check every 5 minutes

monitor_output_performance()
```

## Best Practices

### File Management

1. **Organize by date/time** - Use consistent naming conventions
2. **Compress large files** - Use NetCDF compression for space efficiency
3. **Archive old data** - Move completed runs to long-term storage
4. **Validate output** - Check file integrity and scientific reasonableness
5. **Monitor disk usage** - Prevent running out of space during simulations

### Analysis Workflow

1. **Plan analysis** - Decide what variables you need before running
2. **Use appropriate tools** - Choose tools based on data size and complexity
3. **Document procedures** - Keep track of analysis steps and parameters
4. **Validate results** - Compare with observations and other models
5. **Share findings** - Make analysis code and results available

### Data Sharing

1. **Follow CF conventions** - Ensure metadata completeness
2. **Include provenance** - Document model version and configuration
3. **Provide documentation** - Include analysis scripts and procedures
4. **Use standard formats** - NetCDF with appropriate compression
5. **Archive systematically** - Use consistent directory structures

## References

- [Input Files Guide](input-files.md)
- [Configuration Reference](configuration.md)
- [Running CATChem](running.md)
- [NetCDF Documentation](https://www.unidata.ucar.edu/software/netcdf/)
- [CF Conventions](http://cfconventions.org/)
- [xarray Documentation](https://xarray.pydata.org/)

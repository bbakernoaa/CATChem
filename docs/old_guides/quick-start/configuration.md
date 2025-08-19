# Configuration

This guide covers the essential configuration options for CATChem, helping you set up your first simulation and understand the key parameters.

## Overview

CATChem uses YAML configuration files to control all aspects of the simulation:

- **Main configuration** - Overall simulation settings
- **Process configurations** - Specific settings for each atmospheric process
- **Input/output settings** - Data file locations and formats
- **Performance tuning** - Parallel and optimization settings

## Main Configuration File

### Basic Structure

```yaml
# catchem_config.yml - Main configuration file

# Basic simulation settings
model:
  name: "My CATChem Simulation"
  version: "2.1.0"
  simulation_start: "2024-01-15T00:00:00"
  simulation_end: "2024-01-15T12:00:00"
  timestep: 300  # seconds

# Computational domain
domain:
  grid_type: "latlon"
  nx: 100  # grid points in longitude
  ny: 80   # grid points in latitude
  nz: 50   # vertical levels

# Geographic bounds (degrees)
  lon_min: -130.0
  lon_max: -105.0
  lat_min: 25.0
  lat_max: 45.0

# Atmospheric processes to include
processes:
  - name: chemistry
    enabled: true
    scheme: cb6_full

  - name: emissions
    enabled: true
    scheme: edgar_v6

  - name: dry_deposition
    enabled: true
    scheme: wesely1989

# Input data specifications
input:
  meteorology:
    file_template: "met_%Y%m%d_%H.nc"
    directory: "/data/meteorology/"

  emissions:
    file_template: "emis_%Y%m%d.nc"
    directory: "/data/emissions/"

# Output specifications
output:
  directory: "/output/"
  file_template: "catchem_%Y%m%d_%H.nc"
  frequency: 3600  # hourly output
  variables: ["O3", "NO2", "SO2", "PM25"]

# Logging and diagnostics
logging:
  level: "INFO"
  file: "logs/catchem.log"

diagnostics:
  enabled: true
  frequency: 1800  # every 30 minutes
```

### Time Configuration

**Simulation Period:**
```yaml
model:
  # ISO 8601 format recommended
  simulation_start: "2024-01-15T00:00:00"
  simulation_end: "2024-01-16T00:00:00"

  # Timestep in seconds
  timestep: 300  # 5 minutes

  # Time zone (optional, default UTC)
  timezone: "UTC"
```

**Common Timestep Values:**
- `60` - 1 minute (very high resolution)
- `300` - 5 minutes (typical for urban studies)
- `600` - 10 minutes (standard for regional modeling)
- `1800` - 30 minutes (coarse temporal resolution)
- `3600` - 1 hour (very coarse, mainly for testing)

### Domain Configuration

**Regular Lat-Lon Grid:**
```yaml
domain:
  grid_type: "latlon"

  # Grid dimensions
  nx: 100        # longitude points
  ny: 80         # latitude points
  nz: 50         # vertical levels

  # Geographic bounds
  lon_min: -130.0  # western boundary
  lon_max: -105.0  # eastern boundary
  lat_min: 25.0    # southern boundary
  lat_max: 45.0    # northern boundary

  # Resolution
  dx: 0.25       # degrees longitude (optional, calculated from bounds)
  dy: 0.25       # degrees latitude (optional, calculated from bounds)
```

**Vertical Coordinate:**
```yaml
domain:
  # Vertical coordinate system
  vertical:
    type: "pressure"  # pressure, sigma, hybrid

    # For pressure coordinates (Pa)
    levels: [100000, 97500, 95000, 92500, 90000, ...]

    # For sigma coordinates
    # levels: [1.0, 0.995, 0.99, 0.98, 0.97, ...]
```

**Common Domain Sizes:**
- **Testing**: 10x10x10 (very small, fast)
- **Urban**: 50x50x30 (city-scale)
- **Regional**: 100x80x50 (multi-state region)
- **Continental**: 300x200x60 (CONUS-scale)
- **Global**: 720x360x72 (global modeling)

## Process Configuration

### Chemistry

**Basic Chemistry Setup:**
```yaml
processes:
  - name: chemistry
    enabled: true
    scheme: cb6_full  # Chemical mechanism

    # Solver settings
    solver: rosenbrock
    solver_options:
      relative_tolerance: 1.0e-3
      absolute_tolerance: 1.0e-12
      max_iterations: 1000

    # Initial conditions
    initial_conditions:
      O3: 40.0e-9    # 40 ppbv
      NO2: 10.0e-9   # 10 ppbv
      SO2: 5.0e-9    # 5 ppbv
```

**Available Chemical Mechanisms:**
- `cb6_simple` - Simplified Carbon Bond 6 (testing)
- `cb6_full` - Complete Carbon Bond 6 (recommended)
- `racm2` - Regional Atmospheric Chemistry Mechanism 2
- `saprc07` - Statewide Air Pollution Research Center 2007
- `mozart4` - Model for Ozone and Related Chemical Tracers 4

### Emissions

**Emission Sources:**
```yaml
processes:
  - name: emissions
    enabled: true
    scheme: edgar_v6

    # Anthropogenic emissions
    anthropogenic:
      inventory: "EDGAR_v6"
      sectors: ["power", "industry", "transport", "residential"]
      temporal_profiles:
        diurnal: "profiles/diurnal.nc"
        weekly: "profiles/weekly.nc"

    # Biogenic emissions
    biogenic:
      model: "MEGAN"
      enabled: true

    # Point sources
    point_sources:
      enabled: true
      file: "data/point_sources.csv"
```

### Deposition

**Dry Deposition:**
```yaml
processes:
  - name: dry_deposition
    enabled: true
    scheme: wesely1989

    # Land use data
    land_use_file: "data/land_use.nc"

    # Species-specific settings
    species_settings:
      O3:
        surface_resistance: 100.0  # s/m
      NO2:
        surface_resistance: 50.0
      SO2:
        surface_resistance: 10.0
```

**Wet Deposition:**
```yaml
processes:
  - name: wet_deposition
    enabled: true
    scheme: basic_scavenging

    # Scavenging coefficients
    scavenging:
      SO2: 1.0e-4   # s⁻¹
      HNO3: 5.0e-4
      NH3: 2.0e-4
```

## Input Configuration

### Meteorological Data

**Basic Meteorology Setup:**
```yaml
input:
  meteorology:
    # File naming pattern
    file_template: "met_%Y%m%d_%H.nc"
    directory: "/data/meteorology/"

    # Update frequency
    update_frequency: 3600  # seconds

    # Required variables (automatically detected)
    # temperature, pressure, humidity, winds, etc.

    # Data validation
    validation:
      enabled: true
      range_checks: true
```

**Advanced Meteorology Options:**
```yaml
input:
  meteorology:
    # Multiple data sources
    sources:
      - name: "primary"
        directory: "/data/met_primary/"
        priority: 1
      - name: "backup"
        directory: "/data/met_backup/"
        priority: 2

    # Interpolation settings
    interpolation:
      method: "linear"  # linear, cubic, nearest
      extrapolation: "constant"

    # Quality control
    qc:
      temperature_bounds: [200, 350]  # Kelvin
      pressure_bounds: [10000, 110000]  # Pa
      humidity_bounds: [0, 1]  # kg/kg
```

### Emission Data

**Emission File Setup:**
```yaml
input:
  emissions:
    anthropogenic:
      file_template: "emis_anthro_%Y%m.nc"
      directory: "/data/emissions/anthro/"

    biogenic:
      file_template: "emis_bio_%Y%m%d.nc"
      directory: "/data/emissions/bio/"

    # Point sources
    point_sources:
      file: "/data/emissions/point_sources.csv"
      format: "csv"  # csv, netcdf
```

## Output Configuration

### Basic Output

**Standard Output Setup:**
```yaml
output:
  # Output location
  directory: "/output/catchem_run/"

  # File naming
  file_template: "catchem_%Y%m%d_%H.nc"

  # Output frequency
  frequency: 3600  # every hour

  # Variables to output
  variables:
    concentrations: ["O3", "NO2", "SO2", "CO", "PM25"]
    meteorology: ["temperature", "pressure", "humidity"]
    diagnostics: ["O3_production", "NOx_emissions"]
```

### Advanced Output Options

**Detailed Output Configuration:**
```yaml
output:
  # Multiple output streams
  streams:
    - name: "hourly_surface"
      frequency: 3600
      variables: ["O3", "NO2", "SO2"]
      levels: [0]  # surface only

    - name: "daily_3d"
      frequency: 86400  # daily
      variables: ["O3", "NO2"]
      levels: "all"
      time_averaging: "mean"

  # File format options
  format:
    type: "netcdf4"
    compression: true
    compression_level: 4

  # Spatial subsetting
  domain_subset:
    lon_min: -120.0
    lon_max: -110.0
    lat_min: 30.0
    lat_max: 40.0
```

## Performance Configuration

### Parallel Processing

**MPI Configuration:**
```yaml
parallel:
  # Domain decomposition
  decomposition: "2d"  # 1d, 2d, 3d
  px: 4  # processors in x-direction
  py: 4  # processors in y-direction
  pz: 1  # processors in z-direction (usually 1)

  # Load balancing
  load_balance: true
  dynamic_balance: false

  # Communication
  communication: "nonblocking"  # blocking, nonblocking
```

**OpenMP Configuration:**
```yaml
parallel:
  openmp:
    threads: 4
    schedule: "dynamic"  # static, dynamic, guided
    affinity: "close"    # close, spread, master
```

### Memory Management

**Memory Optimization:**
```yaml
memory:
  # Memory allocation strategy
  allocation: "pool"  # pool, standard
  pool_size_mb: 1024

  # Garbage collection
  gc_frequency: 100  # timesteps
  gc_aggressive: false

  # Precision settings
  precision: "double"  # single, double
  minimize_arrays: false
```

### I/O Optimization

**I/O Performance:**
```yaml
io:
  # Buffer sizes
  buffer_size_mb: 64

  # Parallel I/O
  parallel_netcdf: true
  collective_io: true

  # Asynchronous I/O
  async_io: true
  io_threads: 2
```

## Common Configuration Patterns

### Development/Testing

**Small, Fast Configuration:**
```yaml
model:
  simulation_start: "2024-01-15T00:00:00"
  simulation_end: "2024-01-15T06:00:00"  # 6 hours
  timestep: 600  # 10 minutes

domain:
  nx: 20
  ny: 16
  nz: 10

processes:
  - name: chemistry
    enabled: true
    scheme: cb6_simple  # Simplified chemistry

output:
  frequency: 3600
  variables: ["O3", "NO2"]  # Essential variables only
```

### Production/Research

**Full-Featured Configuration:**
```yaml
model:
  simulation_start: "2024-01-01T00:00:00"
  simulation_end: "2024-02-01T00:00:00"  # 1 month
  timestep: 300  # 5 minutes

domain:
  nx: 200
  ny: 160
  nz: 60

processes:
  - name: chemistry
    enabled: true
    scheme: cb6_full
  - name: emissions
    enabled: true
    scheme: edgar_v6
  - name: dry_deposition
    enabled: true
    scheme: wesely1989
  - name: wet_deposition
    enabled: true
    scheme: basic_scavenging
  - name: settling
    enabled: true
    scheme: stokes

parallel:
  decomposition: "2d"
  px: 8
  py: 8

output:
  frequency: 3600
  variables: ["O3", "NO2", "SO2", "CO", "PM25", "PM10"]

diagnostics:
  enabled: true
  chemistry:
    reaction_rates: true
    budgets: true
```

### High-Performance Computing

**HPC-Optimized Configuration:**
```yaml
model:
  timestep: 300

domain:
  nx: 400
  ny: 300
  nz: 72

parallel:
  decomposition: "2d"
  px: 16
  py: 12  # 192 total processes
  load_balance: true

memory:
  allocation: "pool"
  pool_size_mb: 2048

io:
  parallel_netcdf: true
  buffer_size_mb: 128
  async_io: true

gpu:
  enabled: true
  processes: ["chemistry", "emissions"]
```

## Configuration Validation

### Automatic Validation

**Dry Run Check:**
```bash
# Validate configuration without running
catchem_driver --dry-run catchem_config.yml

# Check for common issues
catchem_validate_config catchem_config.yml
```

**Configuration Checker:**
```python
# Python validation script
import yaml

def validate_config(config_file):
    """Validate CATChem configuration"""

    with open(config_file, 'r') as f:
        config = yaml.safe_load(f)

    # Check required sections
    required_sections = ['model', 'domain', 'processes', 'input', 'output']
    for section in required_sections:
        if section not in config:
            print(f"ERROR: Missing required section: {section}")
            return False

    # Check simulation period
    start = config['model']['simulation_start']
    end = config['model']['simulation_end']
    if start >= end:
        print("ERROR: simulation_end must be after simulation_start")
        return False

    # Check domain bounds
    domain = config['domain']
    if domain['lon_min'] >= domain['lon_max']:
        print("ERROR: lon_max must be greater than lon_min")
        return False

    print("Configuration validation passed")
    return True

# Validate your configuration
validate_config('catchem_config.yml')
```

### Common Configuration Errors

**1. Time Format Errors:**
```yaml
# WRONG
simulation_start: "2024-1-15 0:0:0"

# CORRECT
simulation_start: "2024-01-15T00:00:00"
```

**2. Domain Size Mismatch:**
```yaml
# WRONG - inconsistent grid points and bounds
nx: 100
lon_min: -130.0
lon_max: -105.0  # This gives dx = 0.25°, but nx=100 gives dx = 0.25°

# CORRECT - consistent resolution
nx: 100
dx: 0.25  # Explicitly specify resolution
```

**3. Process Dependencies:**
```yaml
# WRONG - wet deposition needs precipitation data
processes:
  - name: wet_deposition
    enabled: true
# But no precipitation in meteorology

# CORRECT - ensure meteorology includes precipitation
input:
  meteorology:
    required_variables: ["temperature", "pressure", "humidity", "precipitation"]
```

## Best Practices

### Configuration Management

1. **Use version control** - Track configuration changes
2. **Document settings** - Add comments explaining choices
3. **Validate before running** - Use dry-run checks
4. **Start simple** - Begin with basic settings, add complexity
5. **Keep backups** - Save working configurations

### Performance Tuning

1. **Match domain to resources** - Don't over-specify domain size
2. **Balance processes vs cores** - Consider MPI decomposition
3. **Monitor memory usage** - Adjust allocation settings
4. **Optimize I/O** - Use appropriate output frequencies
5. **Profile runs** - Identify bottlenecks

### Scientific Accuracy

1. **Use appropriate timesteps** - Balance accuracy vs performance
2. **Include necessary processes** - Don't disable important physics
3. **Validate inputs** - Check meteorology and emissions
4. **Compare with observations** - Validate results
5. **Document assumptions** - Record configuration rationale

## Templates

### Template Files

CATChem includes configuration templates for common use cases:

```bash
# List available templates
catchem_templates --list

# Create configuration from template
catchem_templates --create urban_air_quality --output my_config.yml

# Available templates:
# - urban_air_quality
# - regional_transport
# - global_chemistry
# - testing_minimal
# - hpc_production
```

### Custom Templates

**Create Your Own Template:**
```yaml
# template_regional.yml
model:
  name: "Regional Template"
  simulation_start: "${START_DATE}"
  simulation_end: "${END_DATE}"
  timestep: ${TIMESTEP}

domain:
  nx: ${NX}
  ny: ${NY}
  nz: ${NZ}
  lon_min: ${LON_MIN}
  lon_max: ${LON_MAX}
  lat_min: ${LAT_MIN}
  lat_max: ${LAT_MAX}

# Use template
catchem_apply_template template_regional.yml \
  --set START_DATE=2024-01-15T00:00:00 \
  --set END_DATE=2024-01-16T00:00:00 \
  --set TIMESTEP=300 \
  --set NX=100 NY=80 NZ=50 \
  --output final_config.yml
```

## Next Steps

After setting up your configuration:

1. **Validate the setup** - Run `catchem_driver --dry-run`
2. **Test with small domain** - Ensure everything works
3. **Scale up gradually** - Increase domain size and complexity
4. **Monitor performance** - Adjust settings as needed
5. **Document your setup** - Save successful configurations

## References

- [First Run Guide](first-run.md)
- [Installation Guide](installation.md)
- [User Guide Configuration](../user-guide/configuration.md)
- [Running CATChem](../user-guide/running.md)
- [Performance Tuning](../user-guide/performance.md)

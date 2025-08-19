# First Run

This guide walks you through running CATChem for the first time, from basic setup to analyzing your first results.

## Prerequisites

Before running CATChem, ensure you have:

- ✅ **Completed installation** - See [Installation Guide](installation.md)
- ✅ **Test data available** - Download from [CATChem test data repository](https://github.com/NOAA-GSL/CATChem-testdata)
- ✅ **Basic understanding** - Read [Model Overview](../user-guide/model-overview.md)

## Quick Test Run

### 1. Download Test Data

```bash
# Download test case data
wget https://github.com/NOAA-GSL/CATChem-testdata/archive/v1.0.tar.gz
tar -xzf v1.0.tar.gz
cd CATChem-testdata-1.0

# Link to CATChem installation
export CATCHEM_ROOT=/path/to/catchem
export PATH=$CATCHEM_ROOT/bin:$PATH
```

### 2. Prepare Run Directory

```bash
# Create run directory
mkdir -p ~/catchem_first_run
cd ~/catchem_first_run

# Copy test configuration
cp $CATCHEM_ROOT/examples/simple_test/catchem_config.yml .

# Create directory structure
mkdir -p input/{meteorology,emissions} output logs
```

### 3. Basic Configuration

Edit `catchem_config.yml`:

```yaml
# CATChem First Run Configuration
model:
  name: "First Test Run"
  version: "2.1.0"
  simulation_start: "2024-01-15T00:00:00"
  simulation_end: "2024-01-15T06:00:00"  # 6-hour test
  timestep: 300  # 5 minutes

domain:
  grid_type: "latlon"
  nx: 20  # Small domain for testing
  ny: 16
  nz: 10
  lon_min: -110.0
  lon_max: -105.0
  lat_min: 35.0
  lat_max: 40.0

processes:
  - name: chemistry
    enabled: true
    scheme: cb6_simple  # Simplified chemistry

  - name: emissions
    enabled: true
    scheme: simple_point_sources

  - name: dry_deposition
    enabled: false  # Disable for first test

input:
  meteorology:
    file_template: "met_%Y%m%d_%H.nc"
    directory: "input/meteorology/"

  emissions:
    file_template: "emis_%Y%m%d.nc"
    directory: "input/emissions/"

output:
  directory: "output/"
  frequency: 3600  # Hourly output
  variables: ["O3", "NO2", "SO2"]

logging:
  level: "INFO"
  file: "logs/catchem.log"
```

### 4. Run the Model

**Single Processor:**
```bash
# Run CATChem
catchem_driver catchem_config.yml

# Monitor progress
tail -f logs/catchem.log
```

**Parallel (4 cores):**
```bash
# Update configuration for parallel run
sed -i 's/nx: 20/nx: 20\n  parallel:\n    px: 2\n    py: 2/' catchem_config.yml

# Run with MPI
mpirun -np 4 catchem_driver catchem_config.yml
```

### 5. Check Results

```bash
# List output files
ls -la output/

# Check file contents
ncdump -h output/catchem_20240115_01.nc

# Quick visualization (if Python available)
python -c "
import xarray as xr
import matplotlib.pyplot as plt
ds = xr.open_dataset('output/catchem_20240115_01.nc')
ds.O3.isel(lev=0).plot()
plt.title('Surface Ozone - First CATChem Run')
plt.savefig('first_run_o3.png')
print('Plot saved as first_run_o3.png')
"
```

## Understanding Your Results

### Expected Output

A successful run should produce:

```
output/
├── catchem_20240115_01.nc  # Hour 1 output
├── catchem_20240115_02.nc  # Hour 2 output
├── catchem_20240115_03.nc  # Hour 3 output
├── catchem_20240115_04.nc  # Hour 4 output
├── catchem_20240115_05.nc  # Hour 5 output
└── catchem_20240115_06.nc  # Hour 6 output

logs/
└── catchem.log             # Runtime log
```

### Log File Analysis

**Successful run log:**
```
2024-01-15 10:00:00 [INFO] CATChem v2.1.0 starting
2024-01-15 10:00:00 [INFO] Configuration: catchem_config.yml
2024-01-15 10:00:00 [INFO] Domain: 20x16x10 (3200 columns)
2024-01-15 10:00:00 [INFO] Processes: chemistry, emissions
2024-01-15 10:00:01 [INFO] Initialization complete
2024-01-15 10:00:01 [INFO] Starting time integration
2024-01-15 10:00:02 [INFO] Timestep 1/72 completed
...
2024-01-15 10:01:00 [INFO] Hour 1 output written
...
2024-01-15 10:06:00 [INFO] Simulation completed successfully
2024-01-15 10:06:00 [INFO] Total runtime: 6 minutes
```

### Data Validation

**Basic Checks:**
```bash
# Check file sizes (should be similar)
ls -lh output/*.nc

# Verify no missing data
python -check_output.py
```

**check_output.py:**
```python
import xarray as xr
import glob

files = glob.glob('output/catchem_*.nc')
print(f"Found {len(files)} output files")

for file in files:
    ds = xr.open_dataset(file)

    # Check for missing data
    missing = ds.isnull().sum().sum()
    if missing > 0:
        print(f"WARNING: {file} has {missing.values} missing values")
    else:
        print(f"✓ {file} - no missing data")

    # Check concentration ranges
    for var in ['O3', 'NO2', 'SO2']:
        if var in ds.variables:
            min_val = ds[var].min().values
            max_val = ds[var].max().values
            print(f"  {var}: {min_val:.2e} to {max_val:.2e}")

    ds.close()

print("Basic validation complete")
```

## Common First-Run Issues

### 1. Missing Input Data

**Error:**
```
ERROR: Cannot find meteorology file: input/meteorology/met_20240115_00.nc
```

**Solution:**
```bash
# Download test meteorology data
wget https://example.com/catchem_test_met.nc
cp catchem_test_met.nc input/meteorology/met_20240115_00.nc

# Or use built-in test data generator
catchem_generate_testdata --met --domain 20x16x10 --output input/meteorology/
```

### 2. Configuration Errors

**Error:**
```
ERROR: Invalid configuration: processes.chemistry.scheme 'cb6_simple' not found
```

**Solution:**
```bash
# List available schemes
catchem_driver --list-schemes

# Update configuration
sed -i 's/cb6_simple/cb6_basic/' catchem_config.yml
```

### 3. Memory Issues

**Error:**
```
ERROR: Cannot allocate memory for domain decomposition
```

**Solution:**
```bash
# Reduce domain size
sed -i 's/nx: 20/nx: 10/' catchem_config.yml
sed -i 's/ny: 16/ny: 8/' catchem_config.yml

# Or increase available memory
ulimit -v unlimited
```

### 4. Compiler/Library Issues

**Error:**
```
error while loading shared libraries: libnetcdff.so.7
```

**Solution:**
```bash
# Load required modules
module load netcdf-fortran

# Or set library path
export LD_LIBRARY_PATH=/path/to/netcdf/lib:$LD_LIBRARY_PATH
```

## Next Steps

### 1. Explore Results

**Visualize your data:**
```python
import xarray as xr
import matplotlib.pyplot as plt

# Load all output files
ds = xr.open_mfdataset('output/catchem_*.nc', concat_dim='time')

# Create time series
o3_avg = ds.O3.mean(dim=['lat', 'lon', 'lev'])
o3_avg.plot()
plt.title('Domain-Average Ozone')
plt.xlabel('Time')
plt.ylabel('Ozone (mol/mol)')
plt.savefig('ozone_timeseries.png')
plt.show()

# Create spatial plot
ds.O3.isel(time=0, lev=0).plot()
plt.title('Surface Ozone at Start')
plt.savefig('ozone_spatial.png')
plt.show()
```

### 2. Try Different Configurations

**Enable more processes:**
```yaml
processes:
  - name: chemistry
    enabled: true
    scheme: cb6_full  # More complete chemistry

  - name: emissions
    enabled: true
    scheme: edgar_v6  # Realistic emissions

  - name: dry_deposition
    enabled: true
    scheme: wesely1989

  - name: settling
    enabled: true
    scheme: stokes
```

**Longer simulation:**
```yaml
model:
  simulation_start: "2024-01-15T00:00:00"
  simulation_end: "2024-01-16T00:00:00"  # 24 hours
```

**Higher resolution:**
```yaml
domain:
  nx: 40
  ny: 32
  nz: 20
```

### 3. Real Data

**Use real meteorological data:**
```bash
# Download GFS data
wget https://nomads.ncep.noaa.gov/cgi-bin/filter_gfs_0p25.pl?...

# Convert to CATChem format
catchem_convert_met gfs_data.grib2 input/meteorology/
```

**Use real emission data:**
```bash
# Download EDGAR emissions
wget https://edgar.jrc.ec.europa.eu/dataset_ghg50
catchem_convert_emissions edgar_data.nc input/emissions/
```

### 4. Advanced Features

**GPU acceleration:**
```yaml
gpu:
  enabled: true
  processes: [chemistry]
```

**Detailed diagnostics:**
```yaml
diagnostics:
  enabled: true
  chemistry:
    reaction_rates: true
    production_loss: true
```

**Restart capability:**
```yaml
restart:
  enabled: true
  frequency: 3600
```

## Validation Checklist

Before considering your first run successful:

- [ ] **Simulation completed** without errors
- [ ] **Output files created** for all requested times
- [ ] **Log file shows** normal progression
- [ ] **Concentrations are reasonable** (not negative, not extreme)
- [ ] **Mass is conserved** (check diagnostics)
- [ ] **Performance is acceptable** for your system
- [ ] **Results can be visualized** and analyzed

## Getting Help

If you encounter issues:

1. **Check the log file** - Most issues are reported there
2. **Verify configuration** - Use `catchem_driver --dry-run config.yml`
3. **Test with simpler setup** - Reduce domain size, disable processes
4. **Check system resources** - Memory, disk space, libraries
5. **Consult documentation** - [User Guide](../user-guide/index.md), [Troubleshooting](../user-guide/troubleshooting.md)
6. **Ask for help** - [GitHub Issues](https://github.com/NOAA-GSL/CATChem/issues)

## Success Criteria

Your first run is successful when you can:

✅ **Run the model** without errors
✅ **Generate output files** with reasonable data
✅ **Visualize results** showing expected patterns
✅ **Understand the workflow** from configuration to analysis
✅ **Modify configuration** for different scenarios

Congratulations! You've successfully run CATChem. Now you can:

- Explore [Configuration Options](configuration.md)
- Learn about [Advanced Features](../user-guide/index.md)
- Try [Example Cases](examples.md)
- Join the [Community](https://github.com/NOAA-GSL/CATChem/discussions)

## References

- [Installation Guide](installation.md)
- [Configuration Guide](configuration.md)
- [User Guide](../user-guide/index.md)
- [Example Cases](examples.md)
- [Troubleshooting](../user-guide/troubleshooting.md)

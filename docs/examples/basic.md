# Basic Examples

Step-by-step examples for common Canopy-App modeling scenarios.

## Example 1: Basic Forest Simulation

### Scenario
Simulate a deciduous forest canopy for a summer day with meteorological forcing from GFS data.

### Input Files Required

1. **Namelist file** (`namelist.canopy`)
2. **Meteorological data** (NetCDF format)
3. **Optional point data** for validation

### Step 1: Prepare Input Data

```bash
# Create working directory
mkdir forest_example
cd forest_example

# Copy input files
cp ../input/gfs.t12z.20220701.sfcf000.canopy.nc .
cp ../input/namelist.canopy .
```

### Step 2: Configure Namelist

Edit `namelist.canopy`:

```fortran
&canopy_inputs
    in_date = '20220701'
    in_time = '00'
    file_in = 'gfs.t12z.20220701.sfcf000.canopy.nc'
    file_out = 'forest_output.nc'
    infmt_opt = 1    ! NetCDF input
    outfmt_opt = 1   ! NetCDF output
/

&canopy_options
    opt_canmet = 1   ! Enable canopy meteorology
    opt_bioem = 1    ! Enable biogenic emissions
    opt_drydep = 1   ! Enable dry deposition
    opt_phot = 1     ! Enable photolysis
    opt_rad = 1      ! Enable radiation
/

&canopy_physics
    z_canbot = 0.5   ! Canopy bottom height (m)
    z_cantop = 25.0  ! Canopy top height (m)
    ncanlevs = 15    ! Number of canopy levels
/
```

### Step 3: Run the Model

```bash
# Compile if needed
cd ../src
make

# Run the simulation
cd ../forest_example
../src/canopy_app
```

### Step 4: Expected Output

```
 CANOPY-APP: Starting model execution...
 Reading namelist: namelist.canopy
 Input file: gfs.t12z.20220701.sfcf000.canopy.nc
 Processing date: 20220701, time: 00

 Canopy configuration:
   Height: 0.5 to 25.0 m
   Levels: 15

 Computing canopy meteorology...
 Computing radiation transfer...
 Computing biogenic emissions...
 Computing dry deposition...
 Computing photolysis rates...

 Writing output: forest_output.nc
 CANOPY-APP: Model execution completed successfully
```

### Step 5: Examine Results

```bash
# Check output file
ncdump -h forest_output.nc

# Quick visualization (if NCO tools available)
ncview forest_output.nc
```

## Example 2: Biogenic Emission Study

### Scenario
Focus on isoprene and monoterpene emissions from an oak forest during peak growing season.

### Enhanced Configuration

```fortran
&canopy_inputs
    in_date = '20220715'  ! Mid-July for peak emissions
    in_time = '14'        ! Afternoon for high temperatures
    file_in = 'met_input_july.nc'
    file_out = 'emissions_study.nc'
    file_pnt = 'emission_points.txt'  ! Point output for analysis
/

&canopy_options
    opt_canmet = 1
    opt_bioem = 1    ! Focus on emissions
    opt_drydep = 0   ! Disable for this study
    opt_phot = 1     ! Needed for light-dependent emissions
    opt_rad = 1      ! Required for PAR calculations
/

&canopy_physics
    z_canbot = 1.0
    z_cantop = 20.0
    ncanlevs = 20    ! Higher resolution for emission gradients
    bioem_opt = 2    ! Advanced emission algorithm
/
```

### Analysis Workflow

```bash
# Run emission study
./canopy_app -n emission_namelist.canopy

# Extract emission data (Python example)
python << EOF
import netCDF4 as nc
import numpy as np
import matplotlib.pyplot as plt

# Read output
data = nc.Dataset('emissions_study.nc', 'r')
heights = data.variables['height'][:]
isoprene = data.variables['isoprene_emission'][:]
monoterpenes = data.variables['monoterpene_emission'][:]

# Plot vertical profiles
plt.figure(figsize=(10, 6))
plt.subplot(1, 2, 1)
plt.plot(isoprene, heights)
plt.xlabel('Isoprene (μg/m³/s)')
plt.ylabel('Height (m)')
plt.title('Isoprene Emission Profile')

plt.subplot(1, 2, 2)
plt.plot(monoterpenes, heights)
plt.xlabel('Monoterpenes (μg/m³/s)')
plt.ylabel('Height (m)')
plt.title('Monoterpene Emission Profile')

plt.tight_layout()
plt.savefig('emission_profiles.png')
print("Emission profiles saved to emission_profiles.png")
EOF
```

## Example 3: Dry Deposition Analysis

### Scenario
Analyze ozone dry deposition to a mixed forest canopy under different atmospheric conditions.

### Configuration

```fortran
&canopy_inputs
    in_date = '20220801'
    in_time = '12'
    file_in = 'ozone_study_input.nc'
    file_out = 'deposition_output.nc'
/

&canopy_options
    opt_canmet = 1
    opt_bioem = 0    ! Disable emissions for this study
    opt_drydep = 1   ! Focus on deposition
    opt_phot = 0     ! Not needed for deposition
    opt_rad = 1      ! For stomatal calculations
/

&canopy_physics
    z_canbot = 0.1
    z_cantop = 18.0
    ncanlevs = 12
    drydep_opt = 2   ! Advanced deposition algorithm
    stom_opt = 1     ! Include stomatal resistance
/
```

### Key Output Variables

```bash
# Variables to examine in output
ncdump -v deposition_velocity,stomatal_resistance,aerodynamic_resistance deposition_output.nc
```

## Example 4: Radiation Transfer Study

### Scenario
Examine PAR and NIR radiation attenuation through a dense coniferous canopy.

### Specialized Configuration

```fortran
&canopy_inputs
    in_date = '20220621'  ! Summer solstice
    in_time = '12'        ! Solar noon
    file_in = 'solar_input.nc'
    file_out = 'radiation_study.nc'
/

&canopy_options
    opt_canmet = 1
    opt_bioem = 1    ! PAR needed for emissions
    opt_drydep = 0
    opt_phot = 1     ! Radiation-dependent
    opt_rad = 1      ! Main focus
    opt_solarzen = 1 ! Solar angle calculations
/

&canopy_physics
    z_canbot = 0.0
    z_cantop = 30.0  ! Tall coniferous forest
    ncanlevs = 25    ! High resolution for radiation
/
```

## Example 5: Sensitivity Analysis

### Scenario
Test model sensitivity to canopy height and leaf area density.

### Parameter Sweep Script

```bash
#!/bin/bash
# Sensitivity analysis script

# Arrays of parameter values
heights=(15 20 25 30)
lai_values=(3 4 5 6)

for height in "${heights[@]}"; do
    for lai in "${lai_values[@]}"; do
        # Create unique directory
        run_dir="sensitivity_h${height}_lai${lai}"
        mkdir $run_dir
        cd $run_dir

        # Copy base namelist and modify
        cp ../base_namelist.canopy namelist.canopy
        sed -i "s/z_cantop = .*/z_cantop = ${height}.0/" namelist.canopy

        # Run model
        ../src/canopy_app

        # Save key results
        mv canopy_output.nc "output_h${height}_lai${lai}.nc"

        cd ..
    done
done

echo "Sensitivity analysis complete"
```

## Example 6: Multi-Day Simulation

### Scenario
Simulate canopy processes over a full growing season week.

### Time Series Configuration

```fortran
&canopy_inputs
    in_date = '20220701'
    in_time = '00'
    file_in = 'gfs_week_201907.nc'  ! Week of data
    file_out = 'weekly_simulation.nc'
    time_series = .true.  ! Multi-time simulation
/
```

### Batch Processing

```bash
# Process multiple days
for day in {01..07}; do
    sed -i "s/in_date = .*/in_date = '202207${day}'/" namelist.canopy
    ./canopy_app
    mv canopy_output.nc "output_2022070${day}.nc"
done

# Combine outputs (if NCO tools available)
ncrcat output_202207*.nc weekly_combined.nc
```

## Example 7: Model Validation

### Scenario
Compare model results against flux tower observations.

### Validation Workflow

```python
import netCDF4 as nc
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy import stats

# Read model output
model_data = nc.Dataset('validation_output.nc', 'r')
model_time = model_data.variables['time'][:]
model_flux = model_data.variables['sensible_heat_flux'][:]

# Read observations (example format)
obs_data = pd.read_csv('tower_observations.csv')
obs_time = pd.to_datetime(obs_data['datetime'])
obs_flux = obs_data['sensible_heat_flux']

# Align time series (simplified)
# ... time alignment code ...

# Calculate statistics
correlation = stats.pearsonr(model_flux, obs_flux)[0]
rmse = np.sqrt(np.mean((model_flux - obs_flux)**2))
bias = np.mean(model_flux - obs_flux)

print(f"Validation Statistics:")
print(f"Correlation: {correlation:.3f}")
print(f"RMSE: {rmse:.2f} W/m²")
print(f"Bias: {bias:.2f} W/m²")

# Create scatter plot
plt.figure(figsize=(8, 6))
plt.scatter(obs_flux, model_flux, alpha=0.6)
plt.plot([obs_flux.min(), obs_flux.max()], [obs_flux.min(), obs_flux.max()], 'r--', lw=2)
plt.xlabel('Observed Flux (W/m²)')
plt.ylabel('Modeled Flux (W/m²)')
plt.title(f'Model vs Observations (R={correlation:.3f})')
plt.savefig('validation_scatter.png')
```

## Troubleshooting Common Issues

### Issue 1: Model Crashes

**Symptoms:** Segmentation fault or floating point error

**Solutions:**
```bash
# Check input file integrity
ncdump -h input_file.nc

# Increase stack size
ulimit -s unlimited

# Run with debugging
gdb ./canopy_app
(gdb) run
(gdb) backtrace  # after crash
```

### Issue 2: Unrealistic Results

**Symptoms:** Extreme values or NaN in output

**Solutions:**
1. Verify input data ranges
2. Check namelist parameter values
3. Reduce time step if needed
4. Enable model debugging output

### Issue 3: Slow Performance

**Solutions:**
```bash
# Compile with optimization
make FFLAGS="-O3 -fast"

# Reduce vertical resolution
# Set ncanlevs = 10 instead of 20

# Profile the code
gprof ./canopy_app > profile.txt
```

## Advanced Usage Tips

### Custom Emission Factors

Edit `canopy_bioparm_mod.F90` to modify emission factors:

```fortran
! Custom isoprene emission factor
efiso = 24000.0  ! μg/g/h for oak
```

### Output Customization

Modify output variables in `canopy_ncf_io_mod.F90`:

```fortran
! Add custom output variable
call write_var_2d(ncid, 'custom_variable', custom_data)
```

### Performance Monitoring

```bash
# Time the simulation
time ./canopy_app

# Memory usage
/usr/bin/time -v ./canopy_app
```

## Next Steps

- **[Advanced Examples](advanced.md)** - Complex multi-physics simulations
- **[Science Documentation](../science/model-description.md)** - Understanding the physics
- **[API Reference](../api/overview.md)** - Code structure and functions
- **[Development Guide](../development/contributing.md)** - Extending the model

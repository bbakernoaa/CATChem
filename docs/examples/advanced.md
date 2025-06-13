# Advanced Examples

Complex modeling scenarios and advanced applications of the Canopy-App system.

## Example 1: Coupled Chemistry-Canopy Simulation

### Scenario
Simulate chemical reactions within the canopy including biogenic VOC emissions and their subsequent chemistry.

### Enhanced Physics Configuration

```fortran
&canopy_inputs
    in_date = '20220715'
    in_time = '10'
    file_in = 'chemistry_input.nc'
    file_out = 'chemistry_output.nc'
    chem_input = 'initial_chemistry.nc'
/

&canopy_options
    opt_canmet = 1
    opt_bioem = 1
    opt_drydep = 1
    opt_phot = 1
    opt_rad = 1
    opt_chem = 1     ! Enable chemistry module
/

&canopy_chemistry
    chem_mechanism = 'CBMZ'  ! Chemical mechanism
    dt_chem = 60.0           ! Chemistry time step (s)
    nspecies = 67            ! Number of chemical species
/
```

### Chemical Species Integration

The model tracks multiple chemical species through vertical profiles:

- **Biogenic VOCs**: Isoprene, α-pinene, β-pinene, limonene
- **Nitrogen oxides**: NO, NO₂, HNO₃
- **Ozone and radicals**: O₃, OH, HO₂, RO₂
- **Secondary products**: Formaldehyde, acetaldehyde, PAN

## Example 2: Fire Emission Simulation

### Scenario
Model canopy processes during and after a prescribed burn or wildfire event.

### Fire-Specific Configuration

```fortran
&canopy_inputs
    in_date = '20220801'
    in_time = '14'
    file_in = 'fire_meteorology.nc'
    file_out = 'fire_simulation.nc'
    fire_input = 'burn_characteristics.nc'
/

&canopy_options
    opt_canmet = 1
    opt_bioem = 1
    opt_drydep = 1
    opt_fire = 1     ! Enable fire module
    opt_rad = 1
/

&canopy_fire
    fire_type = 2           ! Prescribed burn
    fuel_moisture = 0.15    ! Fuel moisture content
    burn_efficiency = 0.85  ! Combustion efficiency
    flame_height = 2.5      ! Average flame height (m)
/
```

### Fire Emission Factors

```python
# Python analysis of fire emissions
import netCDF4 as nc
import numpy as np
import matplotlib.pyplot as plt

# Read fire simulation output
data = nc.Dataset('fire_simulation.nc', 'r')

# Extract fire-related variables
fire_pm25 = data.variables['fire_pm25_emission'][:]
fire_co = data.variables['fire_co_emission'][:]
fire_co2 = data.variables['fire_co2_emission'][:]
heights = data.variables['height'][:]

# Calculate total emissions
total_pm25 = np.trapz(fire_pm25, heights)
total_co = np.trapz(fire_co, heights)
total_co2 = np.trapz(fire_co2, heights)

print(f"Total Fire Emissions:")
print(f"PM2.5: {total_pm25:.2f} g/m²/s")
print(f"CO: {total_co:.2f} g/m²/s")
print(f"CO₂: {total_co2:.2f} g/m²/s")

# Plot emission profiles
fig, axes = plt.subplots(1, 3, figsize=(15, 5))

axes[0].plot(fire_pm25, heights)
axes[0].set_xlabel('PM2.5 (g/m³/s)')
axes[0].set_ylabel('Height (m)')
axes[0].set_title('PM2.5 Emission Profile')

axes[1].plot(fire_co, heights)
axes[1].set_xlabel('CO (g/m³/s)')
axes[1].set_ylabel('Height (m)')
axes[1].set_title('CO Emission Profile')

axes[2].plot(fire_co2, heights)
axes[2].set_xlabel('CO₂ (g/m³/s)')
axes[2].set_ylabel('Height (m)')
axes[2].set_title('CO₂ Emission Profile')

plt.tight_layout()
plt.savefig('fire_emission_profiles.png')
```

## Example 3: Aerosol-Canopy Interactions

### Scenario
Simulate aerosol deposition and scattering effects within forest canopy.

### Aerosol Configuration

```fortran
&canopy_inputs
    in_date = '20220801'
    in_time = '12'
    file_in = 'aerosol_input.nc'
    file_out = 'aerosol_output.nc'
    aerosol_input = 'size_distribution.nc'
/

&canopy_options
    opt_canmet = 1
    opt_bioem = 1
    opt_drydep = 1
    opt_aerosol = 1  ! Enable aerosol module
    opt_rad = 1
/

&canopy_aerosol
    size_bins = 12          ! Number of size bins
    density_aerosol = 1.5   ! Aerosol density (g/cm³)
    hygroscopic = .true.    ! Include hygroscopic growth
/
```

### Aerosol Analysis

```python
# Aerosol size distribution analysis
import netCDF4 as nc
import numpy as np
import matplotlib.pyplot as plt

data = nc.Dataset('aerosol_output.nc', 'r')

# Extract aerosol data
size_bins = data.variables['diameter_bins'][:]  # μm
number_conc = data.variables['number_concentration'][:]  # #/cm³
mass_conc = data.variables['mass_concentration'][:]  # μg/m³
heights = data.variables['height'][:]

# Calculate deposition velocity by size
deposition_vel = data.variables['aerosol_deposition_velocity'][:]

# Plot size-resolved deposition
plt.figure(figsize=(12, 8))

plt.subplot(2, 2, 1)
plt.loglog(size_bins, number_conc[0, :])  # Surface level
plt.xlabel('Diameter (μm)')
plt.ylabel('Number Conc. (#/cm³)')
plt.title('Size Distribution at Surface')

plt.subplot(2, 2, 2)
plt.semilogx(size_bins, deposition_vel)
plt.xlabel('Diameter (μm)')
plt.ylabel('Deposition Velocity (cm/s)')
plt.title('Size-Resolved Deposition')

plt.subplot(2, 2, 3)
for i in range(0, len(heights), 3):
    plt.semilogx(size_bins, mass_conc[i, :], label=f'{heights[i]:.1f} m')
plt.xlabel('Diameter (μm)')
plt.ylabel('Mass Conc. (μg/m³)')
plt.title('Vertical Mass Distribution')
plt.legend()

plt.subplot(2, 2, 4)
total_mass = np.sum(mass_conc, axis=1)
plt.plot(total_mass, heights)
plt.xlabel('Total Mass (μg/m³)')
plt.ylabel('Height (m)')
plt.title('Total Mass Profile')

plt.tight_layout()
plt.savefig('aerosol_analysis.png')
```

## Example 4: Urban Canopy Simulation

### Scenario
Model air quality processes in an urban forest/park setting with background pollution.

### Urban Configuration

```fortran
&canopy_inputs
    in_date = '20220715'
    in_time = '14'
    file_in = 'urban_meteorology.nc'
    file_out = 'urban_canopy.nc'
    background_conc = 'urban_background.nc'
/

&canopy_options
    opt_canmet = 1
    opt_bioem = 1    ! Urban trees still emit
    opt_drydep = 1   ! Important for pollutant removal
    opt_phot = 1     ! Modified by urban environment
    opt_rad = 1
    opt_urban = 1    ! Enable urban effects
/

&canopy_urban
    building_height = 15.0    ! Average building height (m)
    street_width = 20.0       ! Street canyon width (m)
    building_density = 0.4    ! Building area fraction
    traffic_emissions = .true. ! Include vehicle emissions
/
```

### Urban Air Quality Analysis

```python
# Urban air quality analysis
import netCDF4 as nc
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# Read urban simulation
data = nc.Dataset('urban_canopy.nc', 'r')

# Key urban pollutants
no2_conc = data.variables['NO2_concentration'][:]  # ppb
o3_conc = data.variables['O3_concentration'][:]    # ppb
pm25_conc = data.variables['PM25_concentration'][:] # μg/m³
heights = data.variables['height'][:]
time = data.variables['time'][:]

# Calculate air quality index equivalent
def calculate_aqi(no2, o3, pm25):
    # Simplified AQI calculation
    no2_aqi = no2 / 0.1  # NO2 threshold ~0.1 ppm
    o3_aqi = o3 / 0.07   # O3 threshold ~0.07 ppm
    pm25_aqi = pm25 / 35 # PM2.5 threshold ~35 μg/m³
    return np.maximum.reduce([no2_aqi, o3_aqi, pm25_aqi])

aqi_profile = calculate_aqi(no2_conc, o3_conc, pm25_conc)

# Plot urban air quality
fig, axes = plt.subplots(2, 2, figsize=(12, 10))

# Pollutant profiles
axes[0, 0].plot(no2_conc, heights, 'r-', label='NO₂')
axes[0, 0].plot(o3_conc, heights, 'b-', label='O₃')
axes[0, 0].set_xlabel('Concentration (ppb)')
axes[0, 0].set_ylabel('Height (m)')
axes[0, 0].set_title('Gas Concentrations')
axes[0, 0].legend()

axes[0, 1].plot(pm25_conc, heights, 'brown')
axes[0, 1].set_xlabel('PM2.5 (μg/m³)')
axes[0, 1].set_ylabel('Height (m)')
axes[0, 1].set_title('Particulate Matter')

axes[1, 0].plot(aqi_profile, heights, 'purple')
axes[1, 0].set_xlabel('AQI Equivalent')
axes[1, 0].set_ylabel('Height (m)')
axes[1, 0].set_title('Air Quality Index Profile')

# Canopy benefit analysis
no2_reduction = data.variables['NO2_deposition_flux'][:]
o3_reduction = data.variables['O3_deposition_flux'][:]

axes[1, 1].plot(no2_reduction, heights, 'r-', label='NO₂ removal')
axes[1, 1].plot(o3_reduction, heights, 'b-', label='O₃ removal')
axes[1, 1].set_xlabel('Removal Rate (ppb/h)')
axes[1, 1].set_ylabel('Height (m)')
axes[1, 1].set_title('Pollutant Removal by Canopy')
axes[1, 1].legend()

plt.tight_layout()
plt.savefig('urban_air_quality.png')

# Calculate total canopy benefit
total_no2_removal = np.trapz(no2_reduction, heights)
total_o3_removal = np.trapz(o3_reduction, heights)

print(f"Urban Canopy Benefits:")
print(f"NO₂ removal: {total_no2_removal:.3f} ppb·m/h")
print(f"O₃ removal: {total_o3_removal:.3f} ppb·m/h")
```

## Example 5: Climate Change Impact Study

### Scenario
Assess how future climate conditions affect canopy processes and air quality.

### Future Climate Configuration

```fortran
&canopy_inputs
    in_date = '20500715'  ! Future date
    in_time = '14'
    file_in = 'future_climate.nc'
    file_out = 'climate_impact.nc'
/

&canopy_options
    opt_canmet = 1
    opt_bioem = 1
    opt_drydep = 1
    opt_phot = 1
    opt_rad = 1
/

&canopy_physics
    co2_concentration = 550.0  ! Future CO₂ (ppm)
    temperature_offset = 3.0   ! Climate warming (K)
/

&canopy_climate
    phenology_shift = 30      ! Earlier spring (days)
    growing_season_extend = 45 ! Longer growing season (days)
    drought_stress = 1.2      ! Increased drought stress factor
/
```

### Climate Impact Analysis

```bash
#!/bin/bash
# Climate impact assessment script

# Baseline simulation (current climate)
echo "Running baseline simulation..."
cp baseline_namelist.canopy namelist.canopy
./canopy_app
mv canopy_output.nc baseline_output.nc

# Future climate scenarios
temp_increases=(2.0 3.0 4.0 5.0)
co2_levels=(450 550 650 750)

for temp in "${temp_increases[@]}"; do
    for co2 in "${co2_levels[@]}"; do
        echo "Running scenario: +${temp}K, ${co2}ppm CO₂"

        # Modify namelist
        cp future_template.canopy namelist.canopy
        sed -i "s/temperature_offset = .*/temperature_offset = ${temp}/" namelist.canopy
        sed -i "s/co2_concentration = .*/co2_concentration = ${co2}.0/" namelist.canopy

        # Run simulation
        ./canopy_app

        # Save results
        mv canopy_output.nc "scenario_T${temp}_CO2${co2}.nc"
    done
done

echo "Climate impact analysis complete"
```

### Impact Quantification

```python
# Quantify climate change impacts
import netCDF4 as nc
import numpy as np
import matplotlib.pyplot as plt
import glob

# Read all scenario files
scenario_files = glob.glob('scenario_*.nc')
baseline = nc.Dataset('baseline_output.nc', 'r')

# Extract baseline data
baseline_bioem = np.sum(baseline.variables['total_biogenic_emissions'][:])
baseline_o3_dep = np.sum(baseline.variables['O3_deposition_flux'][:])

# Initialize results storage
results = {'temp': [], 'co2': [], 'bioem_change': [], 'o3dep_change': []}

for file in scenario_files:
    # Parse scenario parameters
    temp = float(file.split('_T')[1].split('_')[0])
    co2 = float(file.split('CO2')[1].split('.nc')[0])

    # Read scenario data
    data = nc.Dataset(file, 'r')
    scenario_bioem = np.sum(data.variables['total_biogenic_emissions'][:])
    scenario_o3_dep = np.sum(data.variables['O3_deposition_flux'][:])

    # Calculate percentage changes
    bioem_change = (scenario_bioem - baseline_bioem) / baseline_bioem * 100
    o3dep_change = (scenario_o3_dep - baseline_o3_dep) / baseline_o3_dep * 100

    results['temp'].append(temp)
    results['co2'].append(co2)
    results['bioem_change'].append(bioem_change)
    results['o3dep_change'].append(o3dep_change)

    data.close()

# Create impact visualization
fig, axes = plt.subplots(1, 2, figsize=(12, 5))

# Biogenic emission changes
scatter1 = axes[0].scatter(results['temp'], results['bioem_change'],
                          c=results['co2'], cmap='viridis', s=60)
axes[0].set_xlabel('Temperature Increase (K)')
axes[0].set_ylabel('Biogenic Emission Change (%)')
axes[0].set_title('Climate Impact on Biogenic Emissions')
plt.colorbar(scatter1, ax=axes[0], label='CO₂ (ppm)')

# Ozone deposition changes
scatter2 = axes[1].scatter(results['temp'], results['o3dep_change'],
                          c=results['co2'], cmap='viridis', s=60)
axes[1].set_xlabel('Temperature Increase (K)')
axes[1].set_ylabel('O₃ Deposition Change (%)')
axes[1].set_title('Climate Impact on O₃ Deposition')
plt.colorbar(scatter2, ax=axes[1], label='CO₂ (ppm)')

plt.tight_layout()
plt.savefig('climate_impacts.png')

# Summary statistics
print("Climate Change Impact Summary:")
print(f"Biogenic emissions: {np.mean(results['bioem_change']):.1f}% ± {np.std(results['bioem_change']):.1f}%")
print(f"O₃ deposition: {np.mean(results['o3dep_change']):.1f}% ± {np.std(results['o3dep_change']):.1f}%")
```

## Example 6: Model Ensemble Analysis

### Scenario
Run ensemble simulations with perturbed parameters to quantify model uncertainty.

### Ensemble Configuration

```python
#!/usr/bin/env python3
# Ensemble simulation setup
import numpy as np
import subprocess
import os

# Define parameter uncertainty ranges
parameters = {
    'z_cantop': {'base': 20.0, 'range': 0.2},      # ±20%
    'lai_total': {'base': 4.0, 'range': 0.3},      # ±30%
    'emission_factor': {'base': 1.0, 'range': 0.5}, # ±50%
    'roughness_length': {'base': 0.1, 'range': 0.3} # ±30%
}

# Number of ensemble members
n_ensemble = 100

# Generate ensemble members
ensemble_dir = 'ensemble_runs'
os.makedirs(ensemble_dir, exist_ok=True)

for i in range(n_ensemble):
    member_dir = f"{ensemble_dir}/member_{i:03d}"
    os.makedirs(member_dir, exist_ok=True)

    # Generate perturbed parameters
    perturbed_params = {}
    for param, config in parameters.items():
        # Log-normal perturbation
        factor = np.random.lognormal(0, config['range'])
        perturbed_params[param] = config['base'] * factor

    # Create namelist for this member
    with open(f"{member_dir}/namelist.canopy", 'w') as f:
        f.write(f"""&canopy_inputs
    in_date = '20220715'
    in_time = '14'
    file_in = '../input_data.nc'
    file_out = 'ensemble_output.nc'
/

&canopy_options
    opt_canmet = 1
    opt_bioem = 1
    opt_drydep = 1
    opt_phot = 1
    opt_rad = 1
/

&canopy_physics
    z_cantop = {perturbed_params['z_cantop']:.2f}
    roughness_scale = {perturbed_params['roughness_length']:.3f}
/

&canopy_bioemi
    efiso_scale = {perturbed_params['emission_factor']:.3f}
/
""")

    # Save parameter values for analysis
    with open(f"{member_dir}/parameters.txt", 'w') as f:
        for param, value in perturbed_params.items():
            f.write(f"{param}: {value:.4f}\n")

print(f"Generated {n_ensemble} ensemble members")
```

### Ensemble Execution

```bash
#!/bin/bash
# Run ensemble simulations in parallel

ensemble_dir="ensemble_runs"
max_jobs=8  # Adjust based on available cores

cd $ensemble_dir

# Function to run single ensemble member
run_member() {
    member_dir=$1
    echo "Running ensemble member: $member_dir"
    cd $member_dir
    ../../src/canopy_app > log.txt 2>&1
    cd ..
}

export -f run_member

# Run ensemble members in parallel
find . -name "member_*" -type d | xargs -n1 -P$max_jobs bash -c 'run_member "$@"' _

echo "Ensemble simulation complete"
```

### Ensemble Analysis

```python
# Analyze ensemble results
import netCDF4 as nc
import numpy as np
import matplotlib.pyplot as plt
import glob
import pandas as pd

# Collect ensemble results
ensemble_results = []
parameter_data = []

member_dirs = sorted(glob.glob('ensemble_runs/member_*'))

for member_dir in member_dirs:
    output_file = f"{member_dir}/ensemble_output.nc"
    param_file = f"{member_dir}/parameters.txt"

    if os.path.exists(output_file) and os.path.exists(param_file):
        # Read model output
        data = nc.Dataset(output_file, 'r')
        total_emissions = np.sum(data.variables['total_biogenic_emissions'][:])
        max_deposition = np.max(data.variables['O3_deposition_velocity'][:])
        data.close()

        # Read parameters
        params = {}
        with open(param_file, 'r') as f:
            for line in f:
                key, value = line.strip().split(': ')
                params[key] = float(value)

        ensemble_results.append({
            'total_emissions': total_emissions,
            'max_deposition': max_deposition,
            **params
        })

# Convert to DataFrame for analysis
df = pd.DataFrame(ensemble_results)

# Statistical analysis
print("Ensemble Statistics:")
print(f"Total Emissions: {df['total_emissions'].mean():.2e} ± {df['total_emissions'].std():.2e}")
print(f"Max Deposition: {df['max_deposition'].mean():.4f} ± {df['max_deposition'].std():.4f}")

# Sensitivity analysis
correlations = df.corr()['total_emissions'].drop('total_emissions')
print("\nParameter Sensitivities (correlation with total emissions):")
for param, corr in correlations.items():
    print(f"{param}: {corr:.3f}")

# Visualization
fig, axes = plt.subplots(2, 2, figsize=(12, 10))

# Emission uncertainty
axes[0, 0].hist(df['total_emissions'], bins=20, alpha=0.7)
axes[0, 0].set_xlabel('Total Emissions')
axes[0, 0].set_ylabel('Frequency')
axes[0, 0].set_title('Emission Uncertainty')

# Parameter sensitivity
axes[0, 1].scatter(df['emission_factor'], df['total_emissions'])
axes[0, 1].set_xlabel('Emission Factor Scale')
axes[0, 1].set_ylabel('Total Emissions')
axes[0, 1].set_title('Emission Factor Sensitivity')

# Deposition uncertainty
axes[1, 0].hist(df['max_deposition'], bins=20, alpha=0.7)
axes[1, 0].set_xlabel('Max Deposition Velocity')
axes[1, 0].set_ylabel('Frequency')
axes[1, 0].set_title('Deposition Uncertainty')

# Multi-parameter sensitivity
axes[1, 1].scatter(df['z_cantop'], df['total_emissions'],
                   c=df['lai_total'], cmap='viridis')
axes[1, 1].set_xlabel('Canopy Height')
axes[1, 1].set_ylabel('Total Emissions')
axes[1, 1].set_title('Multi-parameter Effects')

plt.tight_layout()
plt.savefig('ensemble_analysis.png')
```

## Performance Optimization

### High-Performance Computing

```bash
#!/bin/bash
#SBATCH --job-name=canopy_ensemble
#SBATCH --ntasks=100
#SBATCH --time=04:00:00
#SBATCH --mem-per-cpu=2G

# HPC ensemble execution
module load netcdf-fortran
module load python/3.9

# Compile optimized version
cd src
make clean
make FFLAGS="-O3 -march=native -ffast-math"

# Run ensemble with SLURM job arrays
srun --ntasks=1 --cpus-per-task=1 ensemble_member.sh $SLURM_ARRAY_TASK_ID
```

### Memory Optimization

```fortran
! In canopy_alloc.F90 - optimize memory allocation
subroutine optimize_memory_allocation()
    ! Use smaller precision for less critical variables
    real(kind=real32), allocatable :: temp_arrays(:,:)

    ! Deallocate unused arrays immediately
    if (allocated(temporary_array)) deallocate(temporary_array)

    ! Pack arrays to minimize memory footprint
    call pack_vertical_arrays()
end subroutine
```

## Next Steps

- **[Model Development](../development/contributing.md)** - Contributing to the codebase
- **[Performance Tuning](../development/optimization.md)** - Advanced optimization techniques
- **[Integration Examples](../development/coupling.md)** - Coupling with other models
- **[Basic Examples](basic.md)** - Start with simpler scenarios

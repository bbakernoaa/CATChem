# Examples

This guide provides complete examples for common CATChem use cases, from simple test cases to complex operational configurations.

## Example 1: Urban Air Quality

### Scenario
Model air quality over a major metropolitan area with focus on ozone and particulate matter formation.

### Configuration

```yaml
# urban_air_quality.yml
model:
  name: "Urban Air Quality - Los Angeles Basin"
  version: "2.1.0"
  simulation_start: "2024-07-15T00:00:00"  # Summer ozone season
  simulation_end: "2024-07-18T00:00:00"    # 3-day episode
  timestep: 300  # 5-minute timestep for urban chemistry

domain:
  grid_type: "latlon"
  nx: 120        # ~2 km resolution
  ny: 100
  nz: 30         # Focus on boundary layer

  # Los Angeles Basin
  lon_min: -119.5
  lon_max: -116.5
  lat_min: 33.0
  lat_max: 35.5

processes:
  - name: chemistry
    enabled: true
    scheme: cb6_full
    solver: rosenbrock
    solver_options:
      relative_tolerance: 1.0e-3

  - name: emissions
    enabled: true
    scheme: carb_emfac  # California-specific emissions

    anthropogenic:
      sectors: ["mobile", "point", "area"]
      temporal_profiles:
        diurnal: "data/profiles/la_diurnal.nc"
        weekly: "data/profiles/weekday_weekend.nc"

    biogenic:
      model: "MEGAN"

  - name: dry_deposition
    enabled: true
    scheme: wesely1989

  - name: vertical_mixing
    enabled: true
    scheme: ysu_pbl

input:
  meteorology:
    file_template: "met_wrf_%Y%m%d_%H.nc"
    directory: "/data/meteorology/wrf/"
    update_frequency: 3600

  emissions:
    anthropogenic:
      file_template: "emis_carb_%Y%m%d.nc"
      directory: "/data/emissions/carb/"
    biogenic:
      file_template: "emis_megan_%Y%m%d_%H.nc"
      directory: "/data/emissions/megan/"

output:
  directory: "/output/urban_aq/"
  file_template: "urban_aq_%Y%m%d_%H.nc"
  frequency: 3600  # Hourly output

  variables:
    concentrations: ["O3", "NO2", "NO", "CO", "SO2", "PM25", "PM10"]
    meteorology: ["temperature", "pressure", "humidity", "wind_speed"]

diagnostics:
  enabled: true
  chemistry:
    ozone_budget: true
    voc_reactivity: true
  emissions:
    by_sector: true
    diurnal_patterns: true

parallel:
  decomposition: "2d"
  px: 6
  py: 5  # 30 processors total

logging:
  level: "INFO"
  file: "logs/urban_aq.log"
```

### Running the Example

```bash
# Prepare run directory
mkdir urban_air_quality_run
cd urban_air_quality_run

# Copy configuration
cp urban_air_quality.yml catchem_config.yml

# Create directory structure
mkdir -p data/{meteorology,emissions} output logs

# Download example data (if available)
wget https://example.com/catchem_urban_testdata.tar.gz
tar -xzf catchem_urban_testdata.tar.gz

# Run simulation
mpirun -np 30 catchem_driver catchem_config.yml

# Monitor progress
tail -f logs/urban_aq.log
```

### Expected Results

**Surface Ozone Pattern:**
- Peak concentrations 100-150 ppbv during afternoon
- Spatial gradient from coast to inland
- Clear diurnal cycle with morning NO titration

**PM2.5 Composition:**
- Morning peak from traffic emissions
- Afternoon secondary formation
- Spatial hotspots near major sources

## Example 2: Regional Transport

### Scenario
Study regional transport of pollution from major source regions across multiple states.

### Configuration

```yaml
# regional_transport.yml
model:
  name: "Regional Transport - Eastern US"
  version: "2.1.0"
  simulation_start: "2024-06-01T00:00:00"
  simulation_end: "2024-06-08T00:00:00"  # 1-week episode
  timestep: 600  # 10-minute timestep

domain:
  grid_type: "latlon"
  nx: 200        # ~12 km resolution
  ny: 150
  nz: 40

  # Eastern United States
  lon_min: -100.0
  lon_max: -65.0
  lat_min: 25.0
  lat_max: 50.0

processes:
  - name: chemistry
    enabled: true
    scheme: cb6_full

  - name: emissions
    enabled: true
    scheme: nei2017    # National Emissions Inventory

  - name: dry_deposition
    enabled: true
    scheme: wesely1989

  - name: wet_deposition
    enabled: true
    scheme: basic_scavenging

  - name: settling
    enabled: true
    scheme: stokes

  - name: vertical_mixing
    enabled: true
    scheme: ysu_pbl

input:
  meteorology:
    file_template: "met_name_%Y%m%d_%H.nc"
    directory: "/data/meteorology/nam/"

  emissions:
    file_template: "emis_nei_%Y%m.nc"
    directory: "/data/emissions/nei/"

  boundary_conditions:
    file_template: "bc_mozart_%Y%m%d.nc"
    directory: "/data/boundary_conditions/"

output:
  directory: "/output/regional_transport/"

  # Multiple output streams
  streams:
    - name: "hourly_surface"
      file_template: "surface_%Y%m%d_%H.nc"
      frequency: 3600
      variables: ["O3", "NO2", "SO2", "PM25"]
      levels: [0]  # Surface only

    - name: "daily_3d"
      file_template: "daily_3d_%Y%m%d.nc"
      frequency: 86400  # Daily
      variables: ["O3", "NO2", "SO2"]
      time_averaging: "mean"

diagnostics:
  enabled: true
  transport:
    column_budgets: true
    cross_section_fluxes: true
  chemistry:
    regional_budgets: true

parallel:
  decomposition: "2d"
  px: 10
  py: 8  # 80 processors
```

### Analysis Script

```python
# analyze_regional_transport.py
import xarray as xr
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import cartopy.feature as cfeature

def analyze_transport_episode():
    """Analyze regional transport patterns"""

    # Load surface data
    ds_surface = xr.open_mfdataset('output/regional_transport/surface_*.nc')

    # Load 3D data
    ds_3d = xr.open_mfdataset('output/regional_transport/daily_3d_*.nc')

    # Create transport analysis plots
    fig = plt.figure(figsize=(15, 10))

    # Surface ozone evolution
    for i, day in enumerate([1, 3, 5, 7]):
        ax = plt.subplot(2, 4, i+1, projection=ccrs.PlateCarree())

        o3_day = ds_surface.O3.isel(time=day*24)  # Daily average

        im = ax.contourf(ds_surface.lon, ds_surface.lat, o3_day*1e9,
                        levels=np.arange(20, 100, 10),
                        transform=ccrs.PlateCarree(),
                        cmap='viridis')

        ax.add_feature(cfeature.COASTLINE)
        ax.add_feature(cfeature.BORDERS)
        ax.add_feature(cfeature.STATES, linewidth=0.5)
        ax.set_title(f'Surface O3 - Day {day}')

        plt.colorbar(im, ax=ax, label='O3 (ppbv)')

    # Vertical cross-sections
    for i, day in enumerate([1, 3, 5, 7]):
        ax = plt.subplot(2, 4, i+5)

        # Cross-section at 40N
        o3_cross = ds_3d.O3.isel(time=day).sel(lat=40.0, method='nearest')

        im = ax.contourf(o3_cross.lon, o3_cross.lev/100, o3_cross*1e9,
                        levels=np.arange(20, 80, 10),
                        cmap='plasma')

        ax.invert_yaxis()
        ax.set_xlabel('Longitude')
        ax.set_ylabel('Pressure (hPa)')
        ax.set_title(f'O3 Cross-section - Day {day}')

        plt.colorbar(im, ax=ax, label='O3 (ppbv)')

    plt.tight_layout()
    plt.savefig('regional_transport_analysis.png', dpi=300)

    # Calculate transport metrics
    print("Regional Transport Analysis:")

    # Average concentrations by region
    northeast = ds_surface.sel(lon=slice(-80, -70), lat=slice(40, 45))
    southeast = ds_surface.sel(lon=slice(-85, -75), lat=slice(30, 35))
    midwest = ds_surface.sel(lon=slice(-90, -80), lat=slice(35, 45))

    print(f"Northeast O3: {northeast.O3.mean().values*1e9:.1f} ppbv")
    print(f"Southeast O3: {southeast.O3.mean().values*1e9:.1f} ppbv")
    print(f"Midwest O3: {midwest.O3.mean().values*1e9:.1f} ppbv")

# Run analysis
analyze_transport_episode()
```

## Example 3: Global Chemistry

### Scenario
Global atmospheric chemistry simulation for climate and air quality research.

### Configuration

```yaml
# global_chemistry.yml
model:
  name: "Global Chemistry - MOZART"
  version: "2.1.0"
  simulation_start: "2024-01-01T00:00:00"
  simulation_end: "2024-02-01T00:00:00"  # 1 month
  timestep: 1800  # 30-minute timestep

domain:
  grid_type: "latlon"
  nx: 360        # 1-degree resolution
  ny: 180
  nz: 72         # Full troposphere/stratosphere

  # Global domain
  lon_min: -180.0
  lon_max: 180.0
  lat_min: -90.0
  lat_max: 90.0

processes:
  - name: chemistry
    enabled: true
    scheme: mozart4_full

    # Photolysis
    photolysis:
      lookup_table: "data/photolysis/trop_strat_lut.nc"
      solar_cycle: true

  - name: emissions
    enabled: true
    scheme: multi_inventory

    inventories:
      - name: "EDGAR_v6"
        region: "global"
        sectors: ["power", "industry", "transport", "residential", "agriculture"]
      - name: "GFED4"  # Fire emissions
        region: "global"
        type: "biomass_burning"
      - name: "MEGAN"   # Biogenic
        region: "global"
        type: "biogenic"

  - name: dry_deposition
    enabled: true
    scheme: wesely1989_global

  - name: wet_deposition
    enabled: true
    scheme: neu_prather

  - name: settling
    enabled: true
    scheme: stokes_global

input:
  meteorology:
    file_template: "met_era5_%Y%m%d.nc"
    directory: "/data/meteorology/era5/"

  emissions:
    edgar:
      file_template: "edgar_v6_%Y%m.nc"
      directory: "/data/emissions/edgar/"
    gfed:
      file_template: "gfed4_%Y%m%d.nc"
      directory: "/data/emissions/gfed/"
    megan:
      file_template: "megan_%Y%m.nc"
      directory: "/data/emissions/megan/"

output:
  directory: "/output/global_chemistry/"

  streams:
    - name: "monthly_3d"
      file_template: "global_chem_%Y%m.nc"
      frequency: 86400  # Daily, monthly files
      variables: ["O3", "CO", "NO2", "SO2", "CH4", "aerosols"]

    - name: "daily_surface"
      file_template: "surface_%Y%m%d.nc"
      frequency: 86400
      variables: ["O3", "NO2", "SO2", "PM25"]
      levels: [0]

diagnostics:
  enabled: true
  chemistry:
    global_budgets: true
    ozone_column: true
    ch4_lifetime: true
  emissions:
    global_totals: true
    regional_totals: true

parallel:
  decomposition: "2d"
  px: 12
  py: 10  # 120 processors

performance:
  memory_optimization: true
  io_optimization: true
```

## Example 4: Operational Forecast

### Scenario
Operational air quality forecasting system running 24/7 with real-time data.

### Configuration

```yaml
# operational_forecast.yml
model:
  name: "Operational AQ Forecast"
  version: "2.1.0"
  # Times set dynamically by forecast system
  simulation_start: "${FORECAST_START}"
  simulation_end: "${FORECAST_END}"
  timestep: 300

domain:
  grid_type: "latlon"
  nx: 300
  ny: 200
  nz: 50

  # CONUS domain
  lon_min: -130.0
  lon_max: -60.0
  lat_min: 20.0
  lat_max: 55.0

processes:
  - name: chemistry
    enabled: true
    scheme: cb6_full

  - name: emissions
    enabled: true
    scheme: operational_v2024

    # Real-time emission processing
    real_time:
      enabled: true
      wildfire_detection: true
      traffic_updates: true

  - name: dry_deposition
    enabled: true
    scheme: wesely1989

  - name: data_assimilation
    enabled: true
    scheme: 3dvar
    observations: ["surface", "satellite", "aircraft"]

input:
  meteorology:
    # Real-time GFS forecasts
    file_template: "gfs_%Y%m%d_%H_f%{forecast_hour:03d}.nc"
    directory: "/data/realtime/gfs/"

  emissions:
    # Near-real-time emissions
    file_template: "emis_nrt_%Y%m%d.nc"
    directory: "/data/realtime/emissions/"

  observations:
    # For data assimilation
    surface_sites: "/data/observations/airnow/"
    satellite: "/data/observations/satellite/"

output:
  directory: "/output/forecast/${FORECAST_CYCLE}/"

  # Forecast products
  products:
    - name: "surface_forecast"
      file_template: "aq_forecast_%Y%m%d_%H_f%{forecast_hour:03d}.nc"
      frequency: 3600
      variables: ["O3", "NO2", "SO2", "PM25", "PM10"]
      levels: [0]

    - name: "exceedance_forecast"
      file_template: "exceedances_%Y%m%d_%H.nc"
      frequency: 3600
      variables: ["O3_8hr", "PM25_24hr"]
      thresholds:
        O3_8hr: 70.0e-9    # ppbv
        PM25_24hr: 35.0e-6  # μg/m³

# Operational settings
operational:
  # Automated processing
  automation:
    enabled: true
    max_runtime: 7200  # 2 hours max
    restart_on_failure: true

  # Quality control
  quality_control:
    automatic_validation: true
    bias_correction: true
    outlier_detection: true

  # Products and distribution
  products:
    web_output: true
    ftp_distribution: true
    alert_system: true

# Monitoring and alerting
monitoring:
  enabled: true
  metrics: ["runtime", "memory", "accuracy"]
  alerts:
    email: ["operator@agency.gov"]
    runtime_threshold: 7200  # seconds
    accuracy_threshold: 0.8  # correlation

parallel:
  decomposition: "2d"
  px: 15
  py: 10  # 150 processors
```

### Operational Script

```bash
#!/bin/bash
# operational_forecast.sh - Daily forecast cycle

set -e  # Exit on error

# Configuration
FORECAST_DATE=$(date +%Y%m%d)
FORECAST_HOUR=$(date +%H)
CATCHEM_ROOT="/opt/catchem"
RUN_DIR="/operational/catchem_forecast"

# Setup run directory
cd $RUN_DIR
mkdir -p runs/${FORECAST_DATE}_${FORECAST_HOUR}
cd runs/${FORECAST_DATE}_${FORECAST_HOUR}

# Set forecast times
export FORECAST_START="${FORECAST_DATE}T${FORECAST_HOUR}:00:00"
export FORECAST_END=$(date -d "${FORECAST_DATE} ${FORECAST_HOUR}:00 +48 hours" +%Y-%m-%dT%H:%M:%S)
export FORECAST_CYCLE="${FORECAST_DATE}_${FORECAST_HOUR}"

# Process configuration template
envsubst < ../../operational_forecast.yml > catchem_config.yml

# Check data availability
echo "Checking input data availability..."
python3 $CATCHEM_ROOT/scripts/check_input_data.py catchem_config.yml

# Run forecast
echo "Starting forecast run at $(date)"
mpirun -np 150 $CATCHEM_ROOT/bin/catchem_driver catchem_config.yml

# Post-process results
echo "Post-processing forecast products..."
python3 $CATCHEM_ROOT/scripts/generate_forecast_products.py output/

# Distribute products
echo "Distributing forecast products..."
rsync -av output/ /web/forecast/
scp output/exceedances*.nc forecast-server:/data/alerts/

# Archive results
echo "Archiving forecast data..."
tar -czf ${FORECAST_CYCLE}_forecast.tar.gz output/ logs/
mv ${FORECAST_CYCLE}_forecast.tar.gz /archive/

echo "Forecast cycle completed at $(date)"
```

## Example 5: Research Study

### Scenario
Detailed process study examining the impact of different emission scenarios on air quality.

### Configuration

```yaml
# research_study.yml
model:
  name: "Emission Scenario Study"
  version: "2.1.0"
  simulation_start: "2024-06-01T00:00:00"
  simulation_end: "2024-09-01T00:00:00"  # Summer season
  timestep: 300

domain:
  # High-resolution domain
  grid_type: "latlon"
  nx: 400
  ny: 300
  nz: 60

  # Northeastern US
  lon_min: -80.0
  lon_max: -65.0
  lat_min: 38.0
  lat_max: 48.0

# Scenario configuration
scenarios:
  - name: "baseline"
    description: "Current emissions (2024)"
    emissions: "nei2024"

  - name: "50pct_mobile"
    description: "50% reduction in mobile emissions"
    emissions: "nei2024"
    emission_scaling:
      mobile: 0.5

  - name: "zero_power"
    description: "Zero power plant emissions"
    emissions: "nei2024"
    emission_scaling:
      power: 0.0

  - name: "future_2030"
    description: "Projected 2030 emissions"
    emissions: "nei2030_projection"

processes:
  - name: chemistry
    enabled: true
    scheme: cb6_full

    # Detailed chemistry diagnostics
    diagnostics:
      reaction_rates: true
      production_loss: ["O3", "NO2", "SO2", "PM25"]
      sensitivity_analysis: true

  - name: emissions
    enabled: true
    scheme: research_detailed

    # Sector-specific tracking
    sector_tracking: true
    tagged_species: ["NOx", "SO2", "PM25"]

  - name: dry_deposition
    enabled: true
    scheme: wesely1989

  - name: wet_deposition
    enabled: true
    scheme: advanced_scavenging

input:
  meteorology:
    # High-quality reanalysis
    file_template: "met_era5_%Y%m%d_%H.nc"
    directory: "/data/meteorology/era5/"

  emissions:
    baseline:
      file_template: "emis_nei2024_%Y%m.nc"
      directory: "/data/emissions/nei2024/"
    future:
      file_template: "emis_nei2030_%Y%m.nc"
      directory: "/data/emissions/nei2030/"

output:
  directory: "/output/research_study/${SCENARIO}/"

  # Comprehensive output for analysis
  streams:
    - name: "hourly_full"
      frequency: 3600
      variables: "all"

    - name: "daily_statistics"
      frequency: 86400
      time_averaging: ["mean", "max", "min"]
      variables: ["O3", "NO2", "SO2", "PM25"]

    - name: "monthly_budgets"
      frequency: 2592000  # Monthly
      variables: ["chemistry_budgets", "emission_totals", "deposition_totals"]

# Advanced diagnostics for research
diagnostics:
  enabled: true

  chemistry:
    ozone_production_efficiency: true
    nox_limitation: true
    voc_reactivity: true
    aerosol_formation: true

  emissions:
    source_apportionment: true
    sector_contributions: true
    spatial_patterns: true

  process_analysis:
    integrated_reaction_rates: true
    integrated_process_rates: true

  validation:
    surface_sites: "/data/observations/epa_aqm/"
    aircraft: "/data/observations/aircraft/"
    satellite: "/data/observations/omi_tropomi/"

parallel:
  decomposition: "2d"
  px: 20
  py: 15  # 300 processors
```

### Research Analysis Script

```python
# research_analysis.py
import xarray as xr
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

def analyze_emission_scenarios():
    """Comprehensive analysis of emission scenario impacts"""

    scenarios = ['baseline', '50pct_mobile', 'zero_power', 'future_2030']

    # Load data for all scenarios
    data = {}
    for scenario in scenarios:
        data[scenario] = xr.open_mfdataset(
            f'output/research_study/{scenario}/hourly_full_*.nc'
        )

    # 1. Seasonal average concentrations
    fig, axes = plt.subplots(2, 2, figsize=(15, 12))

    for i, scenario in enumerate(scenarios):
        ax = axes[i//2, i%2]

        # Summer average surface O3
        o3_summer = data[scenario].O3.sel(time=slice('2024-06', '2024-08')).mean(dim='time')
        o3_surface = o3_summer.isel(lev=0) * 1e9  # Convert to ppbv

        im = o3_surface.plot(ax=ax, cmap='viridis', vmin=30, vmax=80)
        ax.set_title(f'{scenario.replace("_", " ").title()}\nO3 (ppbv)')

    plt.tight_layout()
    plt.savefig('scenario_spatial_comparison.png', dpi=300)

    # 2. Regional average time series
    # Define regions
    urban_region = data['baseline'].sel(lon=slice(-75, -73), lat=slice(40, 42))  # NYC area
    rural_region = data['baseline'].sel(lon=slice(-78, -76), lat=slice(42, 44))  # Upstate NY

    plt.figure(figsize=(15, 8))

    for scenario in scenarios:
        # Urban region
        o3_urban = data[scenario].O3.sel(lon=slice(-75, -73), lat=slice(40, 42)).mean(dim=['lon', 'lat', 'lev'])
        o3_urban_daily = o3_urban.resample(time='1D').max()  # Daily max

        plt.subplot(2, 1, 1)
        (o3_urban_daily * 1e9).plot(label=scenario.replace('_', ' '))

        # Rural region
        o3_rural = data[scenario].O3.sel(lon=slice(-78, -76), lat=slice(42, 44)).mean(dim=['lon', 'lat', 'lev'])
        o3_rural_daily = o3_rural.resample(time='1D').max()

        plt.subplot(2, 1, 2)
        (o3_rural_daily * 1e9).plot(label=scenario.replace('_', ' '))

    plt.subplot(2, 1, 1)
    plt.title('Urban Region (NYC) - Daily Max O3')
    plt.ylabel('O3 (ppbv)')
    plt.legend()
    plt.grid(True)

    plt.subplot(2, 1, 2)
    plt.title('Rural Region (Upstate NY) - Daily Max O3')
    plt.ylabel('O3 (ppbv)')
    plt.xlabel('Date')
    plt.legend()
    plt.grid(True)

    plt.tight_layout()
    plt.savefig('scenario_timeseries.png', dpi=300)

    # 3. Statistical analysis
    results = []

    for scenario in scenarios:
        # Calculate summer statistics
        o3_summer = data[scenario].O3.sel(time=slice('2024-06', '2024-08'))
        o3_surface = o3_summer.isel(lev=0)

        # Domain statistics
        mean_o3 = float(o3_surface.mean() * 1e9)
        max_o3 = float(o3_surface.max() * 1e9)
        p95_o3 = float(o3_surface.quantile(0.95) * 1e9)

        results.append({
            'Scenario': scenario.replace('_', ' ').title(),
            'Mean O3 (ppbv)': f'{mean_o3:.1f}',
            'Max O3 (ppbv)': f'{max_o3:.1f}',
            '95th Percentile (ppbv)': f'{p95_o3:.1f}'
        })

    # Create summary table
    df_results = pd.DataFrame(results)
    print("\nScenario Comparison - Summer 2024 Surface O3")
    print("=" * 60)
    print(df_results.to_string(index=False))

    # 4. Relative changes
    baseline_data = data['baseline']

    plt.figure(figsize=(12, 10))

    for i, scenario in enumerate(['50pct_mobile', 'zero_power', 'future_2030']):
        ax = plt.subplot(2, 2, i+1)

        # Calculate relative change
        scenario_data = data[scenario]
        baseline_o3 = baseline_data.O3.sel(time=slice('2024-06', '2024-08')).mean(dim='time').isel(lev=0)
        scenario_o3 = scenario_data.O3.sel(time=slice('2024-06', '2024-08')).mean(dim='time').isel(lev=0)

        relative_change = (scenario_o3 - baseline_o3) / baseline_o3 * 100

        im = relative_change.plot(ax=ax, cmap='RdBu_r', vmin=-20, vmax=20)
        ax.set_title(f'{scenario.replace("_", " ").title()}\nO3 Change (%)')

    plt.tight_layout()
    plt.savefig('scenario_relative_changes.png', dpi=300)

    print("\nAnalysis complete. Generated plots:")
    print("- scenario_spatial_comparison.png")
    print("- scenario_timeseries.png")
    print("- scenario_relative_changes.png")

# Run analysis
if __name__ == "__main__":
    analyze_emission_scenarios()
```

## Running Multiple Scenarios

### Batch Processing Script

```bash
#!/bin/bash
# run_scenarios.sh

scenarios=("baseline" "50pct_mobile" "zero_power" "future_2030")

for scenario in "${scenarios[@]}"; do
    echo "Running scenario: $scenario"

    # Set scenario environment
    export SCENARIO=$scenario

    # Create run directory
    mkdir -p runs/$scenario
    cd runs/$scenario

    # Generate configuration
    envsubst < ../../research_study.yml > catchem_config.yml

    # Run simulation
    mpirun -np 300 catchem_driver catchem_config.yml

    # Check results
    if [ $? -eq 0 ]; then
        echo "Scenario $scenario completed successfully"
    else
        echo "ERROR: Scenario $scenario failed"
        exit 1
    fi

    cd ../..
done

echo "All scenarios completed. Running analysis..."
python3 research_analysis.py
```

## Best Practices for Examples

### Documentation

1. **Document assumptions** - Clearly state what each example represents
2. **Explain parameters** - Comment on key configuration choices
3. **Provide context** - Include scientific background and expected results
4. **Show analysis** - Include post-processing and visualization examples

### Reproducibility

1. **Version control** - Track all configuration files
2. **Document dependencies** - List required data and software versions
3. **Automate workflows** - Use scripts for complex processing
4. **Archive results** - Save example outputs for comparison

### Testing

1. **Start small** - Test with reduced domains/time periods
2. **Validate results** - Compare with observations or other models
3. **Check performance** - Monitor computational requirements
4. **Document issues** - Record problems and solutions

## Getting Example Data

Many examples require specific input data. Check these sources:

- **CATChem Test Data**: https://github.com/NOAA-GSL/CATChem-testdata
- **Meteorological Data**: NCEP, ECMWF, NCAR
- **Emission Inventories**: EPA NEI, EDGAR, GFED
- **Observational Data**: EPA AirNow, NOAA, satellite datasets

## Next Steps

After trying these examples:

1. **Modify configurations** - Adapt examples to your domain/time period
2. **Add complexity** - Include additional processes or diagnostics
3. **Compare results** - Validate against observations
4. **Document your setup** - Create your own example configurations
5. **Share with community** - Contribute examples back to the project

## References

- [Configuration Guide](configuration.md)
- [First Run](first-run.md)
- [User Guide](../user-guide/index.md)
- [Developer Guide](../developer-guide/index.md)
- [CATChem GitHub](https://github.com/NOAA-GSL/CATChem)

# Diagnostics

CATChem provides comprehensive diagnostic capabilities for monitoring simulation progress, analyzing model behavior, and validating results. This guide covers the diagnostic system and how to use it effectively.

## Overview

CATChem diagnostics are organized into several categories:

- **Runtime diagnostics** - Real-time monitoring during simulation
- **Process diagnostics** - Detailed analysis of individual atmospheric processes
- **Performance diagnostics** - Computational efficiency and resource usage
- **Scientific diagnostics** - Chemical budgets, mass balance, and validation
- **Quality assurance** - Automated checks and validation metrics

## Runtime Diagnostics

### Basic Monitoring

**Configuration:**
```yaml
diagnostics:
  runtime:
    enabled: true
    frequency: 300  # seconds (5 minutes)
    output_file: "logs/runtime_diagnostics.log"

    # Basic monitoring
    simulation_progress: true
    memory_usage: true
    cpu_usage: true

    # Process timing
    process_timing: true
    timestep_timing: true

    # Quality checks
    mass_conservation: true
    negative_concentrations: true
    extreme_values: true
```

**Sample Output:**
```
2024-01-15 12:00:00 [RUNTIME] Timestep 120/1440 (8.3% complete)
2024-01-15 12:00:00 [RUNTIME] Simulation time: 2024-01-15 10:00:00
2024-01-15 12:00:00 [RUNTIME] Memory usage: 2.4 GB (15% of available)
2024-01-15 12:00:00 [RUNTIME] CPU usage: 95% (16 cores)
2024-01-15 12:00:00 [RUNTIME] Timestep time: 1.2 seconds
2024-01-15 12:00:00 [RUNTIME] Process timing (ms):
2024-01-15 12:00:00 [RUNTIME]   Chemistry: 850
2024-01-15 12:00:00 [RUNTIME]   Emissions: 150
2024-01-15 12:00:00 [RUNTIME]   Deposition: 80
2024-01-15 12:00:00 [RUNTIME]   Transport: 120
2024-01-15 12:00:00 [RUNTIME] Mass conservation: 99.8%
2024-01-15 12:00:00 [RUNTIME] Negative values: 0 (chemistry cleaned)
```

### Progress Monitoring

**Web Dashboard:**
```yaml
diagnostics:
  web_dashboard:
    enabled: true
    port: 8080
    update_frequency: 60  # seconds

    # Dashboard components
    components:
      - simulation_progress
      - concentration_maps
      - time_series
      - performance_metrics
      - error_logs
```

**Monitoring Script:**
```python
import matplotlib.pyplot as plt
import numpy as np
import time
from datetime import datetime

def create_monitoring_dashboard():
    """Create real-time monitoring dashboard"""

    fig, axes = plt.subplots(2, 2, figsize=(12, 8))
    plt.ion()  # Interactive mode

    while True:
        # Read latest diagnostics
        diag_data = read_diagnostics_log()

        # Update plots
        axes[0,0].clear()
        axes[0,0].plot(diag_data['timesteps'], diag_data['memory_usage'])
        axes[0,0].set_title('Memory Usage')
        axes[0,0].set_ylabel('GB')

        axes[0,1].clear()
        axes[0,1].plot(diag_data['timesteps'], diag_data['timestep_time'])
        axes[0,1].set_title('Timestep Performance')
        axes[0,1].set_ylabel('Seconds')

        axes[1,0].clear()
        axes[1,0].bar(diag_data['process_names'], diag_data['process_times'])
        axes[1,0].set_title('Process Timing')
        axes[1,0].set_ylabel('Milliseconds')

        axes[1,1].clear()
        axes[1,1].plot(diag_data['timesteps'], diag_data['mass_conservation'])
        axes[1,1].set_title('Mass Conservation')
        axes[1,1].set_ylabel('Percentage')

        plt.tight_layout()
        plt.pause(30)  # Update every 30 seconds

# Run monitoring
create_monitoring_dashboard()
```

## Process Diagnostics

### Chemistry Diagnostics

**Configuration:**
```yaml
diagnostics:
  chemistry:
    enabled: true
    frequency: 3600  # hourly
    output_file: "output/chemistry_diagnostics.nc"

    # Reaction analysis
    reaction_rates: true
    production_rates: ["O3", "NO2", "SO2", "HNO3"]
    loss_rates: ["O3", "NO2", "SO2", "HNO3"]

    # Solver diagnostics
    solver_iterations: true
    solver_convergence: true
    solver_errors: true

    # Chemical budgets
    species_budgets: ["O3", "NOx", "SOx"]
    family_budgets: ["NOy", "SOx", "HOx"]

    # Photolysis
    photolysis_rates: true
    photolysis_diagnostics: true
```

**Generated Diagnostics:**
```python
import xarray as xr

# Read chemistry diagnostics
chem_diag = xr.open_dataset('output/chemistry_diagnostics.nc')

# Available variables
print(chem_diag.variables)
# O3_production_rate, O3_loss_rate, O3_net_tendency
# reaction_rates, solver_iterations, photolysis_rates

# Analyze ozone budget
o3_prod = chem_diag.O3_production_rate
o3_loss = chem_diag.O3_loss_rate
o3_net = o3_prod - o3_loss

print(f"Domain-average O3 production: {o3_prod.mean().values:.2e} mol/mol/s")
print(f"Domain-average O3 loss: {o3_loss.mean().values:.2e} mol/mol/s")
print(f"Net O3 tendency: {o3_net.mean().values:.2e} mol/mol/s")
```

### Emission Diagnostics

**Configuration:**
```yaml
diagnostics:
  emissions:
    enabled: true
    frequency: 3600
    output_file: "output/emission_diagnostics.nc"

    # Emission rates by source
    by_sector: true
    by_species: ["NOx", "SO2", "CO", "NH3", "VOC"]

    # Temporal patterns
    diurnal_cycle: true
    weekly_cycle: true
    seasonal_cycle: true

    # Spatial distribution
    spatial_totals: true
    hotspot_analysis: true

    # Point sources
    point_source_details: true
    plume_rise_diagnostics: true
```

**Analysis Example:**
```python
# Read emission diagnostics
emis_diag = xr.open_dataset('output/emission_diagnostics.nc')

# Calculate total emissions by sector
nox_by_sector = emis_diag.NOx_emissions_by_sector.sum(dim=['lat', 'lon'])
print("NOx emissions by sector (kg/s):")
for i, sector in enumerate(emis_diag.sector_names):
    print(f"  {sector.values}: {nox_by_sector[i].values:.2f}")

# Analyze diurnal cycle
nox_diurnal = emis_diag.NOx_emissions.groupby('time.hour').mean()
nox_diurnal.plot()
plt.title('NOx Emissions Diurnal Cycle')
plt.xlabel('Hour of Day')
plt.ylabel('Emission Rate (mol/m²/s)')
plt.savefig('nox_diurnal_cycle.png')
```

### Deposition Diagnostics

**Configuration:**
```yaml
diagnostics:
  deposition:
    enabled: true
    frequency: 3600
    output_file: "output/deposition_diagnostics.nc"

    dry_deposition:
      velocities: ["O3", "NO2", "SO2", "NH3"]
      fluxes: ["O3", "NO2", "SO2", "NH3"]
      resistances: ["aerodynamic", "boundary_layer", "surface"]

    wet_deposition:
      scavenging_coefficients: ["SO2", "HNO3", "NH3"]
      washout_rates: ["SO4", "NO3", "NH4"]
      precipitation_rates: true
```

### Transport Diagnostics

**Configuration:**
```yaml
diagnostics:
  transport:
    enabled: true
    frequency: 3600
    output_file: "output/transport_diagnostics.nc"

    vertical_mixing:
      mixing_coefficients: true
      boundary_layer_height: true
      mixing_fluxes: ["O3", "NO2", "SO2"]

    horizontal_transport:
      advection_fluxes: ["O3", "NO2", "SO2"]
      diffusion_fluxes: ["O3", "NO2", "SO2"]

    settling:
      settling_velocities: ["dust", "seasalt", "SO4", "NO3"]
      settling_fluxes: ["dust", "seasalt", "SO4", "NO3"]
```

## Performance Diagnostics

### Computational Performance

**Configuration:**
```yaml
diagnostics:
  performance:
    enabled: true
    frequency: 300  # 5 minutes
    output_file: "logs/performance_diagnostics.log"

    # Timing analysis
    process_timing: true
    io_timing: true
    communication_timing: true

    # Resource usage
    memory_usage: true
    cpu_utilization: true
    gpu_utilization: true  # if available

    # Parallel efficiency
    load_balance: true
    communication_overhead: true
    scalability_metrics: true
```

**Performance Analysis:**
```python
def analyze_performance(log_file):
    """Analyze CATChem performance diagnostics"""

    # Read performance log
    with open(log_file, 'r') as f:
        lines = f.readlines()

    # Parse timing data
    chemistry_times = []
    memory_usage = []

    for line in lines:
        if 'Chemistry:' in line:
            time_ms = float(line.split(':')[1].strip().split()[0])
            chemistry_times.append(time_ms)
        elif 'Memory usage:' in line:
            mem_gb = float(line.split(':')[1].strip().split()[0])
            memory_usage.append(mem_gb)

    # Calculate statistics
    avg_chem_time = np.mean(chemistry_times)
    max_memory = np.max(memory_usage)

    print(f"Average chemistry time: {avg_chem_time:.1f} ms")
    print(f"Peak memory usage: {max_memory:.1f} GB")

    # Plot performance trends
    plt.figure(figsize=(12, 6))

    plt.subplot(1, 2, 1)
    plt.plot(chemistry_times)
    plt.title('Chemistry Solver Performance')
    plt.xlabel('Timestep')
    plt.ylabel('Time (ms)')

    plt.subplot(1, 2, 2)
    plt.plot(memory_usage)
    plt.title('Memory Usage')
    plt.xlabel('Timestep')
    plt.ylabel('Memory (GB)')

    plt.tight_layout()
    plt.savefig('performance_trends.png')

# Analyze performance
analyze_performance('logs/performance_diagnostics.log')
```

### Load Balancing

**Configuration:**
```yaml
diagnostics:
  load_balance:
    enabled: true
    frequency: 1800  # 30 minutes
    output_file: "logs/load_balance.log"

    # Domain decomposition analysis
    processor_timing: true
    communication_patterns: true
    work_distribution: true

    # Imbalance detection
    imbalance_threshold: 0.1  # 10%
    hotspot_detection: true
    recommendations: true
```

**Load Balance Analysis:**
```python
def analyze_load_balance(log_file):
    """Analyze load balancing efficiency"""

    # Read processor timing data
    proc_times = {}
    with open(log_file, 'r') as f:
        for line in f:
            if 'Processor' in line and 'time:' in line:
                proc_id = int(line.split()[1])
                time_sec = float(line.split()[-1])
                proc_times[proc_id] = time_sec

    # Calculate load balance metrics
    times = list(proc_times.values())
    min_time = min(times)
    max_time = max(times)
    avg_time = np.mean(times)

    imbalance = (max_time - min_time) / avg_time
    efficiency = min_time / max_time

    print(f"Load balance metrics:")
    print(f"  Imbalance: {imbalance:.2%}")
    print(f"  Efficiency: {efficiency:.2%}")
    print(f"  Min time: {min_time:.2f} s")
    print(f"  Max time: {max_time:.2f} s")
    print(f"  Avg time: {avg_time:.2f} s")

    # Visualize load distribution
    plt.figure(figsize=(10, 6))
    plt.bar(proc_times.keys(), proc_times.values())
    plt.axhline(avg_time, color='red', linestyle='--', label='Average')
    plt.title('Load Distribution Across Processors')
    plt.xlabel('Processor ID')
    plt.ylabel('Time (seconds)')
    plt.legend()
    plt.savefig('load_balance.png')
```

## Scientific Diagnostics

### Mass Conservation

**Configuration:**
```yaml
diagnostics:
  mass_conservation:
    enabled: true
    frequency: 3600
    output_file: "output/mass_conservation.nc"

    # Species to check
    species: ["O3", "NOx", "SOx", "total_mass"]

    # Check types
    global_mass: true
    process_level: true
    domain_boundaries: true

    # Tolerance levels
    warning_threshold: 0.01  # 1%
    error_threshold: 0.05    # 5%
```

**Mass Balance Analysis:**
```python
def check_mass_conservation(diag_file):
    """Check mass conservation in CATChem simulation"""

    ds = xr.open_dataset(diag_file)

    # Calculate mass changes
    for species in ['O3', 'NOx', 'SOx']:
        if f'{species}_mass_change' in ds.variables:
            mass_change = ds[f'{species}_mass_change']

            # Calculate percentage change
            initial_mass = mass_change.isel(time=0)
            final_mass = mass_change.isel(time=-1)
            percent_change = (final_mass - initial_mass) / initial_mass * 100

            print(f"{species} mass conservation:")
            print(f"  Initial mass: {initial_mass.values:.2e} kg")
            print(f"  Final mass: {final_mass.values:.2e} kg")
            print(f"  Change: {percent_change.values:.2f}%")

            if abs(percent_change.values) > 1.0:
                print(f"  WARNING: Mass not conserved for {species}")
            else:
                print(f"  OK: Mass conserved for {species}")
```

### Budget Analysis

**Configuration:**
```yaml
diagnostics:
  budgets:
    enabled: true
    frequency: 3600
    output_file: "output/species_budgets.nc"

    # Budget components
    species: ["O3", "NOx", "SOx"]
    processes: ["chemistry", "emissions", "deposition", "transport"]

    # Analysis options
    spatial_budgets: true
    temporal_budgets: true
    process_contributions: true
```

**Budget Analysis:**
```python
def analyze_species_budget(budget_file, species='O3'):
    """Analyze species budget"""

    ds = xr.open_dataset(budget_file)

    # Extract budget terms
    chemistry = ds[f'{species}_chemistry_tendency']
    emissions = ds[f'{species}_emission_tendency']
    deposition = ds[f'{species}_deposition_tendency']
    transport = ds[f'{species}_transport_tendency']

    # Calculate domain averages
    chem_avg = chemistry.mean(dim=['lat', 'lon'])
    emis_avg = emissions.mean(dim=['lat', 'lon'])
    depo_avg = deposition.mean(dim=['lat', 'lon'])
    trans_avg = transport.mean(dim=['lat', 'lon'])

    # Plot budget components
    plt.figure(figsize=(12, 8))

    plt.subplot(2, 2, 1)
    chem_avg.plot()
    plt.title(f'{species} Chemistry Tendency')
    plt.ylabel('mol/mol/s')

    plt.subplot(2, 2, 2)
    emis_avg.plot()
    plt.title(f'{species} Emissions Tendency')
    plt.ylabel('mol/mol/s')

    plt.subplot(2, 2, 3)
    depo_avg.plot()
    plt.title(f'{species} Deposition Tendency')
    plt.ylabel('mol/mol/s')

    plt.subplot(2, 2, 4)
    trans_avg.plot()
    plt.title(f'{species} Transport Tendency')
    plt.ylabel('mol/mol/s')

    plt.tight_layout()
    plt.savefig(f'{species}_budget_analysis.png')

    # Calculate process contributions
    total_tendency = chem_avg + emis_avg + depo_avg + trans_avg

    print(f"{species} Budget Analysis:")
    print(f"  Chemistry: {chem_avg.mean().values:.2e} mol/mol/s")
    print(f"  Emissions: {emis_avg.mean().values:.2e} mol/mol/s")
    print(f"  Deposition: {depo_avg.mean().values:.2e} mol/mol/s")
    print(f"  Transport: {trans_avg.mean().values:.2e} mol/mol/s")
    print(f"  Total: {total_tendency.mean().values:.2e} mol/mol/s")
```

## Quality Assurance

### Automated Checks

**Configuration:**
```yaml
diagnostics:
  quality_assurance:
    enabled: true
    frequency: 300  # 5 minutes
    output_file: "logs/qa_checks.log"

    # Checks to perform
    negative_concentrations: true
    extreme_values: true
    nan_values: true
    mass_conservation: true

    # Thresholds
    thresholds:
      O3_max: 1.0e-3    # 1000 ppmv
      NO2_max: 1.0e-3   # 1000 ppmv
      SO2_max: 1.0e-3   # 1000 ppmv
      mass_conservation: 0.01  # 1%

    # Actions
    actions:
      log_warnings: true
      abort_on_error: false
      email_alerts: true
```

**Quality Check Script:**
```python
def run_quality_checks(conc_file):
    """Run automated quality checks on CATChem output"""

    ds = xr.open_dataset(conc_file)

    issues = []

    # Check for negative concentrations
    for var in ['O3', 'NO2', 'SO2']:
        if var in ds.variables:
            negative_count = (ds[var] < 0).sum()
            if negative_count > 0:
                issues.append(f"Negative {var} values: {negative_count.values}")

    # Check for extreme values
    for var in ['O3', 'NO2', 'SO2']:
        if var in ds.variables:
            max_val = ds[var].max()
            if max_val > 1.0e-3:  # 1000 ppmv
                issues.append(f"Extreme {var} value: {max_val.values:.2e}")

    # Check for NaN values
    nan_count = ds.to_array().isnull().sum()
    if nan_count > 0:
        issues.append(f"NaN values found: {nan_count.values}")

    # Report results
    if issues:
        print("Quality assurance issues found:")
        for issue in issues:
            print(f"  - {issue}")
        return False
    else:
        print("All quality checks passed")
        return True

# Run checks on all output files
import glob
for file in glob.glob("output/catchem_conc_*.nc"):
    print(f"Checking {file}...")
    run_quality_checks(file)
```

### Validation Metrics

**Configuration:**
```yaml
diagnostics:
  validation:
    enabled: true
    frequency: 3600
    output_file: "output/validation_metrics.nc"

    # Comparison with observations
    observations:
      surface_sites: "data/surface_observations.nc"
      aircraft: "data/aircraft_observations.nc"
      satellite: "data/satellite_observations.nc"

    # Metrics to calculate
    metrics: ["bias", "rmse", "correlation", "mae"]

    # Species to validate
    species: ["O3", "NO2", "SO2", "PM25"]
```

## Visualization

### Diagnostic Plots

**Real-time Plotting:**
```python
def create_diagnostic_plots():
    """Create standard diagnostic plots"""

    # Read latest output
    ds = xr.open_dataset('output/catchem_conc_latest.nc')

    # Create multi-panel plot
    fig, axes = plt.subplots(2, 3, figsize=(15, 10))

    # Surface concentrations
    ds.O3.isel(lev=0).plot(ax=axes[0,0], cmap='viridis')
    axes[0,0].set_title('Surface O3')

    ds.NO2.isel(lev=0).plot(ax=axes[0,1], cmap='plasma')
    axes[0,1].set_title('Surface NO2')

    ds.SO2.isel(lev=0).plot(ax=axes[0,2], cmap='inferno')
    axes[0,2].set_title('Surface SO2')

    # Vertical profiles (domain average)
    ds.O3.mean(dim=['lat', 'lon']).plot(ax=axes[1,0], y='lev', yincrease=False)
    axes[1,0].set_title('O3 Vertical Profile')

    ds.NO2.mean(dim=['lat', 'lon']).plot(ax=axes[1,1], y='lev', yincrease=False)
    axes[1,1].set_title('NO2 Vertical Profile')

    ds.SO2.mean(dim=['lat', 'lon']).plot(ax=axes[1,2], y='lev', yincrease=False)
    axes[1,2].set_title('SO2 Vertical Profile')

    plt.tight_layout()
    plt.savefig('diagnostic_plots.png', dpi=150)
    plt.close()

# Create plots every hour
import time
while True:
    create_diagnostic_plots()
    time.sleep(3600)  # Wait 1 hour
```

### Dashboard Creation

**HTML Dashboard:**
```python
def create_html_dashboard():
    """Create HTML dashboard for diagnostics"""

    html = """
    <!DOCTYPE html>
    <html>
    <head>
        <title>CATChem Diagnostics Dashboard</title>
        <meta http-equiv="refresh" content="300">
        <style>
            body { font-family: Arial, sans-serif; margin: 20px; }
            .grid { display: grid; grid-template-columns: 1fr 1fr; gap: 20px; }
            .panel { border: 1px solid #ccc; padding: 15px; border-radius: 5px; }
            .status-ok { color: green; }
            .status-warning { color: orange; }
            .status-error { color: red; }
        </style>
    </head>
    <body>
        <h1>CATChem Diagnostics Dashboard</h1>
        <p>Last updated: {timestamp}</p>

        <div class="grid">
            <div class="panel">
                <h2>Simulation Status</h2>
                <p>Progress: {progress}%</p>
                <p>Current time: {sim_time}</p>
                <p>Status: <span class="status-ok">Running</span></p>
            </div>

            <div class="panel">
                <h2>Performance</h2>
                <p>Memory usage: {memory_gb:.1f} GB</p>
                <p>Timestep time: {timestep_time:.2f} seconds</p>
                <p>Load balance: {load_balance:.1f}%</p>
            </div>

            <div class="panel">
                <h2>Quality Checks</h2>
                <p>Mass conservation: <span class="status-ok">✓</span></p>
                <p>Negative values: <span class="status-ok">✓</span></p>
                <p>Extreme values: <span class="status-ok">✓</span></p>
            </div>

            <div class="panel">
                <h2>Concentrations</h2>
                <img src="diagnostic_plots.png" width="100%">
            </div>
        </div>
    </body>
    </html>
    """

    # Fill in template with current data
    from datetime import datetime

    dashboard = html.format(
        timestamp=datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
        progress=get_simulation_progress(),
        sim_time=get_current_sim_time(),
        memory_gb=get_memory_usage(),
        timestep_time=get_timestep_time(),
        load_balance=get_load_balance()
    )

    with open('dashboard.html', 'w') as f:
        f.write(dashboard)

# Update dashboard every 5 minutes
import time
while True:
    create_html_dashboard()
    time.sleep(300)
```

## Best Practices

### Diagnostic Strategy

1. **Start simple** - Begin with basic runtime diagnostics
2. **Add detail gradually** - Increase diagnostic detail as needed
3. **Monitor continuously** - Use real-time monitoring for long runs
4. **Validate early** - Check results frequently during development
5. **Document findings** - Keep records of diagnostic analysis

### Performance Optimization

1. **Balance detail vs. performance** - Diagnostics have computational cost
2. **Use appropriate frequencies** - Don't over-diagnose
3. **Optimize I/O** - Use efficient file formats and compression
4. **Archive systematically** - Manage diagnostic data storage
5. **Automate analysis** - Use scripts for routine diagnostic tasks

### Troubleshooting

1. **Check diagnostics first** - Look at diagnostic output before other debugging
2. **Correlate issues** - Look for patterns across different diagnostic types
3. **Compare with baselines** - Use known-good runs as reference
4. **Escalate systematically** - Start with simple checks, add complexity
5. **Document solutions** - Keep track of diagnostic-based fixes

## References

- [Configuration Guide](configuration.md)
- [Output Files](output-files.md)
- [Performance Guide](performance.md)
- [Troubleshooting](troubleshooting.md)
- [Developer Guide](../developer-guide/index.md)

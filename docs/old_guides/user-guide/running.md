# Running CATChem

This guide covers how to execute CATChem simulations, from basic single-processor runs to large-scale parallel simulations on HPC systems.

## Overview

CATChem can be run in several different modes:

- **Standalone mode** - Independent atmospheric chemistry simulations
- **Coupled mode** - Integrated with host weather/climate models
- **Column mode** - Single-column simulations for testing and validation
- **Ensemble mode** - Multiple simulations with parameter variations

## Quick Start

### Basic Run

**Single Processor:**
```bash
# Navigate to run directory
cd /path/to/catchem_run

# Run with configuration file
./catchem_driver config/catchem_config.yml
```

**With MPI:**
```bash
# Run on 4 processors
mpirun -np 4 ./catchem_driver config/catchem_config.yml

# Run on specific hosts
mpirun -np 8 --hostfile hosts.txt ./catchem_driver config/catchem_config.yml
```

### Run Directory Setup

Create a organized run directory:
```bash
mkdir catchem_run_20240115
cd catchem_run_20240115

# Create directory structure
mkdir -p config data/meteorology data/emissions output logs

# Copy configuration files
cp /path/to/catchem_config.yml config/
cp /path/to/chemistry_config.yml config/

# Link or copy input data
ln -s /data/meteorology/* data/meteorology/
ln -s /data/emissions/* data/emissions/

# Set up run script
cp /path/to/run_catchem.sh .
```

## Configuration

### Main Configuration File

The main configuration file controls all aspects of the simulation:

```yaml
# catchem_config.yml
model:
  name: "Regional Air Quality Simulation"
  version: "2.1.0"
  simulation_start: "2024-01-15T00:00:00"
  simulation_end: "2024-01-16T00:00:00"
  timestep: 300  # seconds

domain:
  grid_type: "latlon"
  nx: 100
  ny: 80
  nz: 50
  lon_min: -130.0
  lon_max: -105.0
  lat_min: 25.0
  lat_max: 45.0

parallel:
  decomposition: "2d"
  px: 2  # processors in x-direction
  py: 2  # processors in y-direction

processes:
  - name: chemistry
    enabled: true
    scheme: cb6_full
    timestep_factor: 1

  - name: emissions
    enabled: true
    scheme: edgar_v6

  - name: dry_deposition
    enabled: true
    scheme: wesely1989

input:
  meteorology:
    file_template: "met_%Y%m%d_%H.nc"
    directory: "data/meteorology/"
    update_frequency: 3600

output:
  directory: "output/"
  file_template: "catchem_%Y%m%d_%H.nc"
  frequency: 3600
  variables: ["O3", "NO2", "SO2", "PM25"]

logging:
  level: "INFO"
  file: "logs/catchem.log"

diagnostics:
  enabled: true
  file: "logs/catchem_diagnostics.log"
  frequency: 1800
```

### Runtime Options

**Command Line Options:**
```bash
./catchem_driver [options] config_file

Options:
  --help, -h              Show help message
  --version, -v           Show version information
  --verbose, -V           Enable verbose output
  --debug, -d             Enable debug mode
  --dry-run              Validate configuration without running
  --restart file         Restart from checkpoint file
  --profile              Enable performance profiling
  --threads N            Set number of OpenMP threads
```

**Examples:**
```bash
# Dry run to validate configuration
./catchem_driver --dry-run config/catchem_config.yml

# Debug mode with verbose output
./catchem_driver --debug --verbose config/catchem_config.yml

# Restart from checkpoint
./catchem_driver --restart output/restart_20240115_12.nc config/catchem_config.yml

# With performance profiling
./catchem_driver --profile config/catchem_config.yml
```

## Parallel Execution

### OpenMP Threading

**Environment Variables:**
```bash
export OMP_NUM_THREADS=4
export OMP_PLACES=cores
export OMP_PROC_BIND=close
export OMP_SCHEDULE=dynamic
```

**In Configuration:**
```yaml
parallel:
  openmp:
    threads: 4
    schedule: dynamic
    affinity: close
```

### MPI Parallelization

**Domain Decomposition:**
```yaml
parallel:
  decomposition: "2d"  # 1d, 2d, or 3d
  px: 4  # processors in x-direction
  py: 4  # processors in y-direction
  pz: 1  # processors in z-direction (usually 1)

  load_balance: true
  communication: "nonblocking"
```

**MPI Execution Examples:**
```bash
# Standard MPI run
mpirun -np 16 ./catchem_driver config.yml

# With processor binding
mpirun -np 16 --bind-to core --map-by socket \
  ./catchem_driver config.yml

# Hybrid MPI+OpenMP
export OMP_NUM_THREADS=4
mpirun -np 4 --map-by socket:pe=4 \
  ./catchem_driver config.yml
```

### GPU Acceleration

**GPU Configuration:**
```yaml
gpu:
  enabled: true
  device_count: 2
  memory_pool_mb: 4096
  processes: [chemistry, emissions]

parallel:
  gpu_aware_mpi: true
```

**GPU Execution:**
```bash
# Single GPU
export CUDA_VISIBLE_DEVICES=0
./catchem_driver --gpu config.yml

# Multiple GPUs with MPI
export CUDA_VISIBLE_DEVICES=0,1,2,3
mpirun -np 4 ./catchem_driver --gpu config.yml
```

## Job Submission

### SLURM

**Basic Job Script:**
```bash
#!/bin/bash
#SBATCH --job-name=catchem-run
#SBATCH --account=atmospheric-chem
#SBATCH --partition=compute
#SBATCH --ntasks=64
#SBATCH --ntasks-per-node=16
#SBATCH --time=04:00:00
#SBATCH --output=logs/catchem_%j.out
#SBATCH --error=logs/catchem_%j.err

# Load modules
module purge
module load intel/2021.3.0
module load impi/2021.3.0
module load netcdf/4.7.4

# Set environment
source setup_catchem_env.sh
export OMP_NUM_THREADS=2
export OMP_PLACES=cores

# Change to run directory
cd $SLURM_SUBMIT_DIR

# Run CATChem
srun ./catchem_driver config/catchem_config.yml

echo "Job completed at $(date)"
```

**GPU Job Script:**
```bash
#!/bin/bash
#SBATCH --job-name=catchem-gpu
#SBATCH --partition=gpu
#SBATCH --nodes=2
#SBATCH --ntasks-per-node=4
#SBATCH --gpus-per-node=4
#SBATCH --time=02:00:00

module load nvhpc/22.2 cuda/11.7

export CUDA_VISIBLE_DEVICES=0,1,2,3
export CUDA_MPS=1

srun --gpu-bind=closest ./catchem_driver --gpu config.yml
```

### PBS/Torque

**Job Script:**
```bash
#!/bin/bash
#PBS -N catchem-run
#PBS -A atmospheric-modeling
#PBS -q standard
#PBS -l nodes=4:ppn=16
#PBS -l walltime=04:00:00
#PBS -j oe
#PBS -o logs/catchem_${PBS_JOBID}.log

cd $PBS_O_WORKDIR
source setup_catchem_env.sh

export OMP_NUM_THREADS=2

mpirun -hostfile $PBS_NODEFILE -np 64 \
  ./catchem_driver config/catchem_config.yml
```

### LSF

**Job Script:**
```bash
#!/bin/bash
#BSUB -J catchem-run
#BSUB -P atmospheric-chem
#BSUB -q general
#BSUB -n 64
#BSUB -R "span[ptile=16]"
#BSUB -W 04:00
#BSUB -o logs/catchem_%J.out
#BSUB -e logs/catchem_%J.err

source setup_catchem_env.sh
cd $LS_SUBCWD

mpirun.lsf ./catchem_driver config/catchem_config.yml
```

## Monitoring and Control

### Runtime Monitoring

**Progress Monitoring:**
```bash
# Monitor log files
tail -f logs/catchem.log

# Check diagnostic output
tail -f logs/catchem_diagnostics.log

# Monitor resource usage
top -p $(pgrep catchem)
htop -p $(pgrep catchem)
```

**Performance Monitoring:**
```yaml
# In configuration file
monitoring:
  performance_metrics: true
  memory_usage: true
  load_balance: true
  output_frequency: 100  # timesteps

diagnostics:
  timing:
    processes: true
    io_operations: true
    communication: true

  memory:
    high_water_mark: true
    allocations: true

  load_balance:
    domain_decomposition: true
    process_timing: true
```

### Real-time Diagnostics

**Built-in Diagnostics:**
```python
# Python monitoring script
import matplotlib.pyplot as plt
import xarray as xr
import time

def monitor_simulation():
    """Monitor CATChem simulation in real-time"""

    while True:
        try:
            # Read latest output
            ds = xr.open_dataset('output/catchem_latest.nc')

            # Plot key variables
            fig, axes = plt.subplots(2, 2, figsize=(12, 10))

            ds.O3.isel(lev=0).plot(ax=axes[0,0], title='Surface O3')
            ds.NO2.isel(lev=0).plot(ax=axes[0,1], title='Surface NO2')
            ds.SO2.isel(lev=0).plot(ax=axes[1,0], title='Surface SO2')
            ds.PM25.isel(lev=0).plot(ax=axes[1,1], title='Surface PM2.5')

            plt.tight_layout()
            plt.savefig('monitoring/current_state.png')
            plt.close()

        except FileNotFoundError:
            pass

        time.sleep(300)  # Update every 5 minutes

# Run monitoring
monitor_simulation()
```

### Job Control

**Signal Handling:**
```bash
# Graceful shutdown (saves restart file)
kill -TERM $(pgrep catchem)

# Force shutdown
kill -KILL $(pgrep catchem)

# Pause simulation (if supported)
kill -STOP $(pgrep catchem)

# Resume simulation
kill -CONT $(pgrep catchem)
```

**Checkpoint and Restart:**
```yaml
# Configuration for checkpointing
checkpointing:
  enabled: true
  frequency: 3600  # seconds
  directory: "restart/"
  compression: true

restart:
  automatic: true
  on_failure: true
  max_attempts: 3
```

## Output Management

### Output Configuration

```yaml
output:
  directory: "output/"
  file_template: "catchem_%Y%m%d_%H.nc"
  frequency: 3600  # seconds
  compression: true

  variables:
    concentrations: ["O3", "NO2", "SO2", "CO", "PM25"]
    meteorology: ["temperature", "pressure", "humidity"]
    diagnostics: ["reaction_rates", "emission_rates"]

  formats:
    - netcdf4
    - grib2  # optional

  quality_control:
    range_check: true
    units_check: true
    cf_compliance: true
```

### Output Monitoring

**Disk Space Management:**
```bash
# Monitor output directory size
du -sh output/

# Compress old output files
find output/ -name "*.nc" -mtime +7 -exec gzip {} \;

# Archive completed runs
tar -czf archive/catchem_run_20240115.tar.gz output/ logs/

# Clean up temporary files
find . -name "*.tmp" -delete
```

**Output Validation:**
```bash
# Check output file integrity
ncdump -t output/catchem_20240115_12.nc > /dev/null

# Validate CF conventions
cfchecker output/catchem_20240115_12.nc

# Check for missing data
python -c "
import xarray as xr
ds = xr.open_dataset('output/catchem_20240115_12.nc')
print('Missing data:', ds.isnull().sum().sum().values)
"
```

## Performance Optimization

### Runtime Optimization

**Configuration Tuning:**
```yaml
performance:
  # I/O optimization
  io_buffer_size: 64  # MB
  async_io: true
  parallel_netcdf: true

  # Memory optimization
  memory_pool: true
  pool_size_mb: 1024

  # Computation optimization
  vectorization: true
  cache_optimization: true
```

**Environment Tuning:**
```bash
# Memory settings
export OMP_STACKSIZE=256M
ulimit -s unlimited

# I/O settings
export NETCDF_BUFFER_SIZE=64M
export HDF5_USE_FILE_LOCKING=FALSE

# MPI settings
export OMPI_MCA_btl_tcp_if_include=ib0
export I_MPI_FABRICS=shm:dapl
```

### Performance Monitoring

**Built-in Profiling:**
```bash
# Run with profiling enabled
./catchem_driver --profile config.yml

# Analyze profile results
python analyze_profile.py catchem_profile.json
```

**External Profiling:**
```bash
# Intel VTune
vtune -collect hotspots ./catchem_driver config.yml

# GNU gprof
export FCFLAGS="-pg -O2"
make clean && make
./catchem_driver config.yml
gprof catchem_driver gmon.out > profile.txt

# Valgrind (for memory issues)
valgrind --tool=memcheck --leak-check=full \
  ./catchem_driver config.yml
```

## Troubleshooting

### Common Runtime Issues

**Memory Problems:**
```bash
# Check memory usage
/usr/bin/time -v ./catchem_driver config.yml

# Monitor memory during run
while true; do
  ps aux | grep catchem | grep -v grep
  sleep 60
done

# Reduce memory usage
# In configuration:
memory:
  reduce_precision: true  # Use single precision
  minimize_arrays: true
  garbage_collection: aggressive
```

**I/O Problems:**
```bash
# Check disk space
df -h output/

# Check file permissions
ls -la output/

# Test I/O performance
dd if=/dev/zero of=output/test_file bs=1M count=1000
rm output/test_file

# Parallel I/O issues
export HDF5_DISABLE_VERSION_CHECK=1
export NETCDF_BUFFER_SIZE=64M
```

**MPI Problems:**
```bash
# Test MPI connectivity
mpirun -np 4 hostname

# Check MPI installation
which mpirun
mpirun --version

# Debug MPI issues
export OMPI_MCA_btl_base_verbose=100
mpirun -np 4 ./catchem_driver config.yml

# Use different MPI implementation
module swap openmpi intelmpi
```

### Error Diagnosis

**Log Analysis:**
```bash
# Check for errors in logs
grep -i error logs/catchem.log
grep -i warning logs/catchem.log
grep -i fail logs/catchem.log

# Analyze crash patterns
grep -B5 -A5 "ABORT\|CRASH\|FATAL" logs/catchem.log

# Monitor for hangs
timeout 3600 ./catchem_driver config.yml
```

**Debug Mode:**
```bash
# Run in debug mode
./catchem_driver --debug --verbose config.yml

# With debugger
gdb --args ./catchem_driver config.yml
# In gdb:
# (gdb) run
# (gdb) bt  # after crash
```

### Recovery Procedures

**Restart from Checkpoint:**
```bash
# Find latest restart file
ls -t restart/restart_*.nc | head -1

# Restart simulation
./catchem_driver --restart restart/restart_20240115_1200.nc config.yml
```

**Partial Recovery:**
```yaml
# In configuration file
recovery:
  skip_corrupted_files: true
  interpolate_missing_data: true
  continue_on_error: true
  max_errors: 10
```

## Best Practices

### Simulation Setup

1. **Test with small domains** before production runs
2. **Validate input data** thoroughly
3. **Use appropriate timesteps** for stability
4. **Monitor resource usage** during runs
5. **Save intermediate results** frequently
6. **Document run parameters** and changes

### Performance

1. **Profile before optimizing** to identify bottlenecks
2. **Use appropriate parallel decomposition** for your domain
3. **Balance computation and I/O** requirements
4. **Monitor memory usage** and optimize accordingly
5. **Use scratch space** for temporary files
6. **Archive completed runs** to save space

### Operations

1. **Automate routine tasks** with scripts
2. **Monitor job queues** and resource usage
3. **Plan for failures** with checkpointing
4. **Validate output** regularly
5. **Maintain consistent environments** across runs
6. **Document procedures** for reproducibility

## References

- [Input Files Guide](input-files.md)
- [Output Files Guide](output-files.md)
- [Configuration Reference](configuration.md)
- [Performance Tuning](performance.md)
- [Troubleshooting Guide](troubleshooting.md)
- [HPC Installation Guide](../developer-guide/hpc-installation.md)

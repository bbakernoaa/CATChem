# Quick Start Guide

Get CATChem running in just a few minutes! This guide will walk you through installation, basic configuration, and your first model run.

## Prerequisites

Before installing CATChem, ensure you have:

- **Fortran Compiler**: Modern Fortran 2008+ compiler (gfortran 9+, Intel ifort, etc.)
- **MPI Library**: OpenMPI, Intel MPI, or MPICH
- **CMake**: Version 3.15 or newer
- **NetCDF**: NetCDF-Fortran library
- **YAML**: YAML-Fortran library (optional, for advanced configuration)

=== "Ubuntu/Debian"

    ```bash
    sudo apt-get update
    sudo apt-get install gfortran cmake libnetcdff-dev libopenmpi-dev
    ```

=== "RHEL/CentOS"

    ```bash
    sudo yum install gcc-gfortran cmake netcdf-fortran-devel openmpi-devel
    # or with dnf
    sudo dnf install gcc-gfortran cmake netcdf-fortran-devel openmpi-devel
    ```

=== "macOS"

    ```bash
    brew install gcc cmake netcdf open-mpi
    ```

=== "HPC Systems"

    ```bash
    # Example for NOAA HPC systems
    module load intel netcdf cmake
    ```

## Installation

### 1. Clone the Repository

```bash
git clone https://github.com/NOAA-GSL/CATChem.git
cd CATChem
```

### 2. Configure Build

```bash
mkdir build
cd build

# Basic configuration
cmake -DCMAKE_BUILD_TYPE=Release ..

# Advanced configuration
cmake -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_Fortran_COMPILER=gfortran \
      -DENABLE_MPI=ON \
      -DENABLE_OPENMP=ON \
      -DNetCDF_ROOT=/path/to/netcdf \
      ..
```

### 3. Build

```bash
# Build with all available cores
make -j$(nproc)

# Or specify number of cores
make -j8
```

### 4. Test Installation

```bash
# Run unit tests
ctest

# Run specific tests
ctest -R settling
ctest -R ysuverticaldispersion
```

## First Run

### Test Case

Run a simple test case to verify your installation:

```bash
# From the build directory
cd build

# Run settling process test
./tests/test_settling_integration

# Run column processing test
./tests/test_column_processing
```

### Basic Driver Example

```bash
# Run with sample configuration
./src/api/catchem_driver --config ../parm/config/catchem_test.yml
```

## Configuration

### Basic Configuration File

Create a simple configuration file `my_config.yml`:

```yaml
# CATChem Basic Configuration
model:
  name: "my_test"
  description: "Basic CATChem test run"

# Grid configuration
grid:
  nx: 10
  ny: 10
  nz: 20
  dx: 1000.0  # meters
  dy: 1000.0  # meters

# Time configuration
time:
  start_time: "2025-01-01T00:00:00"
  end_time: "2025-01-01T01:00:00"
  time_step: 60.0  # seconds

# Processes to run
processes:
  - name: "settling"
    scheme: "Stokesscheme"
    enabled: true

  - name: "ysuverticaldispersion"
    scheme: "scaleAwareYSU"
    enabled: true

# Chemistry configuration
chemistry:
  mechanism: "cb6r3_ae7"
  species:
    - "O3"
    - "NO2"
    - "SO2"
    - "PM25"

# Output configuration
output:
  format: "netcdf"
  frequency: 3600  # seconds
  variables:
    - "all_species"
    - "settling_velocity"
    - "vertical_diffusion_coeff"
```

### Run with Your Configuration

```bash
./src/api/catchem_driver --config my_config.yml
```

## Integration Example

### Simple Fortran Integration

```fortran
program catchem_example
   use CATChemAPI_Mod
   use precision_mod

   implicit none

   type(CATChemType) :: catchem
   character(len=256) :: config_file = "my_config.yml"
   integer :: rc

   ! Initialize CATChem
   call catchem%init(config_file, rc)
   if (rc /= 0) then
      write(*,*) 'Failed to initialize CATChem'
      stop 1
   endif

   ! Main time loop
   do while (.not. catchem%is_finished())
      call catchem%step(rc)
      if (rc /= 0) then
         write(*,*) 'Error in CATChem step'
         exit
      endif
   enddo

   ! Finalize
   call catchem%finalize(rc)

   write(*,*) 'CATChem run completed successfully!'

end program catchem_example
```

Compile and run:

```bash
gfortran -I../build/include catchem_example.f90 -L../build/lib -lcatchem_api -o my_example
./my_example
```

## What's Next?

🎉 **Congratulations!** You now have CATChem running. Here's what to explore next:

<div class="grid cards" markdown>

- [:material-book-open-page-variant: **User Guide**](../user-guide/index.md)

  ---

  Learn about model configuration, processes, and advanced features

- [:material-chart-line: **Performance Tuning**](../user-guide/performance.md)

  ---

  Optimize CATChem for your system and use case

- [:material-cog: **Process Configuration**](../user-guide/processes/index.md)

  ---

  Understand and configure atmospheric processes

- [:material-bug: **Troubleshooting**](../user-guide/troubleshooting.md)

  ---

  Common issues and solutions

</div>

## Common Issues

### Build Problems

??? question "CMake can't find NetCDF"

    ```bash
    # Specify NetCDF path explicitly
    cmake -DNetCDF_ROOT=/usr/local \
          -DNetCDF_Fortran_ROOT=/usr/local \
          ..
    ```

??? question "Compiler not found"

    ```bash
    # Specify Fortran compiler
    cmake -DCMAKE_Fortran_COMPILER=gfortran ..
    # or
    export FC=gfortran
    cmake ..
    ```

??? question "MPI issues"

    ```bash
    # Load MPI module (HPC systems)
    module load mpi

    # Or specify MPI compiler
    cmake -DCMAKE_Fortran_COMPILER=mpif90 ..
    ```

### Runtime Problems

??? question "Segmentation fault"

    Check stack limits:
    ```bash
    ulimit -s unlimited
    export OMP_STACKSIZE=64M
    ```

??? question "Configuration file not found"

    ```bash
    # Use absolute path
    ./catchem_driver --config /full/path/to/config.yml
    ```

??? question "NetCDF output issues"

    Ensure NetCDF libraries are in your path:
    ```bash
    export LD_LIBRARY_PATH=/path/to/netcdf/lib:$LD_LIBRARY_PATH
    ```

## Getting Help

- 📚 **Documentation**: [Complete User Guide](../user-guide/index.md)
- 🐛 **Issues**: [GitHub Issues](https://github.com/NOAA-GSL/CATChem/issues)
- 💬 **Discussions**: [GitHub Discussions](https://github.com/NOAA-GSL/CATChem/discussions)
- 📧 **Contact**: [gsl.help@noaa.gov](mailto:gsl.help@noaa.gov)

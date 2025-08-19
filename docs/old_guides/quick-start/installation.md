# Installation Guide

Complete installation instructions for CATChem on various systems and configurations.

## 🖥️ System Requirements

### Hardware Requirements
- **Memory**: Minimum 4GB RAM, 16GB+ recommended for large simulations
- **Storage**: 10GB for source and build, additional space for input/output data
- **CPU**: Modern multi-core processor, x86_64 architecture

### Software Dependencies

#### Required
- **Fortran Compiler**: GNU gfortran 9+, Intel ifort 19+, or PGI/NVIDIA HPC SDK
- **CMake**: Version 3.12 or later
- **NetCDF-Fortran**: Version 4.5 or later
- **MPI Library**: OpenMPI 3.0+, Intel MPI, or MPICH 3.0+

#### Optional
- **HDF5**: For advanced NetCDF features
- **YAML-Fortran**: For configuration file parsing (included as submodule)
- **Python**: For utility scripts and visualization tools

## 📦 Installation Methods

### Method 1: Source Installation (Recommended)

#### 1. Get the Source Code
```bash
# Clone from GitHub
git clone https://github.com/CATChem/cc_restructure.git
cd cc_restructure

# Initialize submodules
git submodule update --init --recursive
```

#### 2. Set Environment Variables
```bash
# NetCDF libraries
export NetCDF_ROOT=/path/to/netcdf
export NetCDF_FORTRAN_ROOT=/path/to/netcdf-fortran

# MPI (if not in standard location)
export MPI_ROOT=/path/to/mpi

# Optional: Optimization flags
export FFLAGS="-O3 -march=native"
```

#### 3. Configure Build
```bash
mkdir build && cd build

# Basic configuration
cmake .. \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_INSTALL_PREFIX=$HOME/catchem

# Advanced configuration
cmake .. \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_Fortran_COMPILER=gfortran \
  -DCMAKE_INSTALL_PREFIX=$HOME/catchem \
  -DENABLE_OPENMP=ON \
  -DENABLE_TESTING=ON
```

#### 4. Build and Install
```bash
# Build (use -j for parallel compilation)
make -j4

# Run tests (optional but recommended)
make test

# Install
make install
```

### Method 2: Container Installation

#### Docker
```bash
# Pull pre-built image
docker pull catchem/catchem:latest

# Or build from source
docker build -t catchem/catchem .

# Run container
docker run -it -v $(pwd):/data catchem/catchem
```

#### Singularity
```bash
# Build from Docker image
singularity build catchem.sif docker://catchem/catchem:latest

# Run simulation
singularity exec catchem.sif catchem config.yml
```

## 🔧 System-Specific Instructions

### Ubuntu/Debian
```bash
# Install dependencies
sudo apt update
sudo apt install build-essential cmake gfortran \
                 libnetcdff-dev libopenmpi-dev \
                 python3 python3-pip

# Clone and build
git clone https://github.com/CATChem/cc_restructure.git
cd cc_restructure
mkdir build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
make -j$(nproc)
```

### CentOS/RHEL/Rocky Linux
```bash
# Install dependencies
sudo yum groupinstall "Development Tools"
sudo yum install cmake gcc-gfortran netcdf-fortran-devel \
                 openmpi-devel python3

# Set up environment
module load mpi/openmpi-x86_64

# Build as above
```

### macOS
```bash
# Install dependencies via Homebrew
brew install cmake gcc netcdf openmpi

# Set compiler
export CC=gcc-11
export FC=gfortran-11

# Build as above
```

### HPC Systems

#### NCAR Cheyenne
```bash
module purge
module load intel/19.1.1 mpt/2.22 cmake/3.18.2
module load netcdf/4.7.4

# Build with Intel compiler
cmake .. -DCMAKE_Fortran_COMPILER=ifort
```

#### NOAA WCOSS2
```bash
module load intel/19.1.3.304 NetCDF/4.7.4-intel-19.1.3.304
module load cmake/3.20.1

# Build configuration
cmake .. -DCMAKE_BUILD_TYPE=Release
```

## ✅ Verification

### Test Installation
```bash
# Check executable
./catchem --version

# Run basic test
./catchem --test

# Run full test suite
ctest -V
```

### Performance Test
```bash
# Create test configuration
cat > test_config.yml << EOF
model:
  name: "performance_test"
  timestep: 300
grid:
  nx: 50
  ny: 50
  nz: 30
processes:
  settling:
    enabled: true
EOF

# Run performance test
time mpirun -np 4 ./catchem test_config.yml
```

## 🐛 Troubleshooting

### Common Build Issues

#### NetCDF Not Found
```bash
# Explicitly set NetCDF paths
cmake .. -DNetCDF_ROOT=/usr/local \
         -DNetCDF_FORTRAN_ROOT=/usr/local
```

#### Compiler Issues
```bash
# Force specific compiler
cmake .. -DCMAKE_Fortran_COMPILER=gfortran-9

# Debug build issues
cmake .. -DCMAKE_BUILD_TYPE=Debug -DCMAKE_VERBOSE_MAKEFILE=ON
```

#### MPI Problems
```bash
# Test MPI installation
mpirun -np 2 hostname

# Set MPI compiler wrapper
cmake .. -DCMAKE_Fortran_COMPILER=mpif90
```

### Runtime Issues

#### Permission Errors
```bash
# Make executable
chmod +x catchem

# Check file permissions
ls -la catchem
```

#### Library Loading
```bash
# Check dependencies
ldd catchem

# Set library path
export LD_LIBRARY_PATH=/path/to/libs:$LD_LIBRARY_PATH
```

## 📚 Additional Resources

- **[Build Options Reference](../developer-guide/build-system.md)** - Complete CMake options
- **[HPC Installation Guide](../developer-guide/hpc-installation.md)** - Detailed HPC instructions
- **[Container Guide](../developer-guide/containers.md)** - Docker and Singularity details

## 🔗 Next Steps

After successful installation:
1. **[First Run](first-run.md)** - Run your first simulation
2. **[Configuration](configuration.md)** - Learn about configuration options
3. **[Examples](examples.md)** - Try example configurations

---

*Having installation issues? Check our [GitHub Issues](https://github.com/CATChem/cc_restructure/issues) or start a [Discussion](https://github.com/CATChem/cc_restructure/discussions).*

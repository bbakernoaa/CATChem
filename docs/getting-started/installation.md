# Installation Guide

This guide will help you install and set up the Canopy-App modeling system.

## Prerequisites

### System Requirements

- **Operating System**: Linux, macOS, or Windows (WSL recommended)
- **Compiler**: Modern Fortran compiler (gfortran, ifort, or similar)
- **Memory**: Minimum 4 GB RAM, 8 GB+ recommended
- **Storage**: At least 1 GB free disk space

### Required Dependencies

#### Fortran Compiler
```bash
# On Ubuntu/Debian
sudo apt-get install gfortran

# On CentOS/RHEL
sudo yum install gcc-gfortran

# On macOS with Homebrew
brew install gcc
```

#### NetCDF Library (Optional but recommended)
```bash
# On Ubuntu/Debian
sudo apt-get install libnetcdf-dev libnetcdff-dev

# On CentOS/RHEL
sudo yum install netcdf-devel netcdf-fortran-devel

# On macOS with Homebrew
brew install netcdf netcdf-fortran
```

## Installation Methods

### Method 1: Clone from GitHub (Recommended)

```bash
# Clone the repository
git clone https://github.com/canopy-app/canopy-app.git
cd canopy-app

# Navigate to source directory
cd src

# Compile the model
make
```

### Method 2: Download Release

1. Go to the [releases page](https://github.com/canopy-app/canopy-app/releases)
2. Download the latest release archive
3. Extract and compile:

```bash
tar -xzf canopy-app-v1.0.tar.gz
cd canopy-app-v1.0/src
make
```

## Compilation

### Using the Makefile

The project includes a Makefile for easy compilation:

```bash
# Standard compilation
make

# Clean build files
make clean

# Debug build
make debug

# Parallel compilation (faster)
make -j4
```

### Manual Compilation

If you prefer manual compilation or need custom settings:

```bash
# Basic compilation command
gfortran -O2 -o canopy_app *.F90

# With NetCDF support
gfortran -O2 -I/usr/include -L/usr/lib -lnetcdff -lnetcdf -o canopy_app *.F90

# Debug version
gfortran -g -O0 -fcheck=all -Wall -o canopy_app *.F90
```

## Verification

### Test Installation

Run the basic test to verify your installation:

```bash
# Run with example data
./canopy_app

# Check version information
./canopy_app --version
```

### Expected Output

A successful installation should produce output similar to:

```
=========================================
    CANOPY-APP ATMOSPHERIC MODEL
=========================================
Version: 1.0
Build Date: 2024-XX-XX
Compiler: GNU Fortran
NetCDF Support: Enabled
=========================================
```

## Troubleshooting

### Common Issues

!!! warning "Compilation Errors"
    **Issue**: `gfortran: command not found`
    **Solution**: Install a Fortran compiler (see prerequisites)

!!! warning "NetCDF Errors"
    **Issue**: `fatal error: netcdf.mod: No such file or directory`
    **Solution**: Install NetCDF development libraries or compile without NetCDF

!!! warning "Permission Errors"
    **Issue**: `Permission denied` when running executable
    **Solution**: Make the file executable: `chmod +x canopy_app`

### Environment Variables

You may need to set environment variables for libraries:

```bash
# For NetCDF
export NETCDF_ROOT=/usr/local
export LD_LIBRARY_PATH=$NETCDF_ROOT/lib:$LD_LIBRARY_PATH

# For Intel Fortran
export INTEL_LICENSE_FILE=/opt/intel/licenses
```

### Getting Help

If you encounter issues during installation:

1. Check the [troubleshooting section](../user-guide/troubleshooting.md)
2. Search [existing issues](https://github.com/canopy-app/canopy-app/issues)
3. Create a [new issue](https://github.com/canopy-app/canopy-app/issues/new) with:
   - Your operating system
   - Compiler version
   - Complete error messages
   - Installation method used

## Next Steps

Once installation is complete:

1. 📖 Read the [Quick Start Guide](quickstart.md)
2. ⚙️ Configure your [model settings](configuration.md)
3. 🚀 Run your [first simulation](../examples/basic.md)

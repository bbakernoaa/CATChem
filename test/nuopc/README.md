# CATChem NUOPC Interface Testing

This directory contains tests for the CATChem NUOPC interface and cap components. The tests are designed to validate different levels of functionality based on available dependencies.

## Test Levels

### 1. Minimal Compilation Test (No Dependencies)
- **File**: `test_compilation_minimal.F90`
- **Requirements**: Only gfortran compiler
- **Purpose**: Validates basic Fortran compilation and array operations
- **Run**: `make test_basic`

### 2. Core Module Test (CATChem Modules)
- **File**: `test_compilation_stub.F90`
- **Requirements**: CATChem core modules (precision_mod, Error_Mod, etc.)
- **Purpose**: Tests compilation with CATChem dependencies
- **Run**: `make test_basic` (automatically selected if modules available)

### 3. NUOPC Interface Test (Full Integration)
- **Files**: `test_nuopc_cap.F90`, `test_diagnostic_output.F90`
- **Requirements**: ESMF library, NetCDF library, CATChem modules
- **Purpose**: Tests full NUOPC cap and diagnostic output functionality
- **Run**: `make test_esmf`

## Quick Start

### Check Dependencies
```bash
make info
```

### Run All Available Tests
```bash
make test_all
```

### Run Specific Test Levels
```bash
# Basic compilation test only
make test_basic

# ESMF/NUOPC tests (if ESMF available)
make test_esmf
```

## Detailed Test Descriptions

### Minimal Compilation Test
Tests basic Fortran functionality:
- Precision type definitions
- Constants and parameters
- File I/O operations
- Array operations and memory management
- Error handling patterns

**Expected Output**:
```
=========================================
CATChem Minimal Compilation Test
=========================================
Test 1: Testing precision types...
  ✓ Precision types working
Test 2: Testing constants...
  ✓ Constants working
Test 3: Testing file I/O...
  ✓ File I/O working correctly
Test 4: Testing array operations...
  ✓ Array operations working correctly
=========================================
ALL MINIMAL TESTS PASSED!
```

### NUOPC Cap Test
Tests NUOPC integration:
- Driver component creation
- Component service registration
- Basic ESMF/NUOPC framework functionality
- Memory management in NUOPC context

### Diagnostic Output Test
Tests diagnostic output functionality:
- ESMF grid creation
- ESMF field operations
- Time management
- Mock diagnostic data generation
- File output validation (would use AQMIO in full version)

## Configuration Files

### CATChem_test_config.yml
Test configuration for CATChem model:
- Grid configuration (10x8 with 20 levels)
- Time configuration (6-hour test run)
- Process configuration (dust, sea salt, chemistry)
- Diagnostic output settings

### CATChem_test_field_mapping.yml
Field mapping configuration for NUOPC:
- Import fields (meteorological variables)
- Export fields (chemistry diagnostics)
- Standard names and unit mappings
- Dimension specifications

## Dependencies

### Required
- **gfortran**: GNU Fortran compiler
- **make**: Build system

### Optional (for full testing)
- **ESMF**: Earth System Modeling Framework
  - Install: `conda install esmf` or system package manager
  - Verify: `pkg-config --exists esmf`
- **NetCDF-Fortran**: Network Common Data Form library
  - Install: `conda install netcdf-fortran` or system package manager
  - Verify: `pkg-config --exists netcdf-fortran`

### Installing Dependencies on Different Systems

#### Conda/Mamba (Recommended)
```bash
conda install esmf netcdf-fortran
```

#### Ubuntu/Debian
```bash
sudo apt-get install libesmf-dev libnetcdff-dev pkg-config
```

#### macOS (Homebrew)
```bash
brew install esmf netcdf-fortran pkg-config
```

#### CentOS/RHEL
```bash
sudo yum install esmf-devel netcdf-fortran-devel pkgconfig
```

## Troubleshooting

### Common Issues

1. **"Cannot open module file '*.mod'"**
   - Solution: Run minimal test first: `make test_basic`
   - This indicates missing CATChem core modules

2. **"ESMF not found"**
   - Solution: Install ESMF library or run basic tests only
   - Check: `pkg-config --exists esmf`

3. **"NetCDF not found"**
   - Solution: Install NetCDF-Fortran library
   - Check: `pkg-config --exists netcdf-fortran`

4. **Compilation errors with gfortran**
   - Check gfortran version: `gfortran --version`
   - Minimum recommended: GCC 7.0+

### Debug Mode
To run tests with additional debug information:
```bash
make clean
FCFLAGS="-g -O0 -fbacktrace -fcheck=all" make test_all
```

### Verbose Output
To see detailed compilation commands:
```bash
make test_all VERBOSE=1
```

## Integration with CATChem Build System

These tests can be integrated into the main CATChem CMake build system by adding:

```cmake
# In main CMakeLists.txt
if(BUILD_TESTING)
  add_subdirectory(test/nuopc)
endif()
```

## Expected Results

### Successful Test Run
All tests should pass with clear success messages. Output files will be created in `./test_output/` directory.

### Partial Success
If ESMF is not available, basic tests should still pass with a message indicating NUOPC tests were skipped.

### Test Failure
Failed tests will:
- Display clear error messages
- Exit with non-zero status
- Leave debug files for inspection

## Next Steps

After successful testing:

1. **Integration Testing**: Test with real meteorological data
2. **Performance Testing**: Benchmark diagnostic output performance
3. **Multi-PET Testing**: Test parallel execution with multiple processors
4. **Coupling Testing**: Test integration with actual atmospheric models

## Contributing

When adding new tests:
1. Follow the existing naming convention
2. Add appropriate error checking
3. Include cleanup procedures
4. Update this README
5. Test on multiple systems/compilers

## Files in This Directory

- `test_compilation_minimal.F90` - Minimal compilation test
- `test_compilation_stub.F90` - Core module compilation test
- `test_nuopc_cap.F90` - NUOPC cap integration test
- `test_diagnostic_output.F90` - Diagnostic output functionality test
- `CATChem_test_config.yml` - Test model configuration
- `CATChem_test_field_mapping.yml` - Test field mapping configuration
- `CMakeLists.txt` - CMake build configuration
- `Makefile` - Make build configuration
- `README.md` - This documentation file
# Troubleshooting

This guide helps you diagnose and solve common issues when using CATChem.

## Quick Diagnosis

### Check System Status

```bash
# Check if CATChem is running
ps aux | grep catchem

# Check log files
tail -f catchem.log

# Verify configuration
catchem_driver --validate-config config.yml
```

## Common Issues

### Build and Installation Issues

??? question "CMake configuration fails"

    **Symptoms:** CMake cannot find dependencies

    **Solutions:**
    ```bash
    # Set environment variables
    export NetCDF_ROOT=/path/to/netcdf
    export HDF5_ROOT=/path/to/hdf5

    # Clean and reconfigure
    rm -rf build
    mkdir build && cd build
    cmake -DCMAKE_BUILD_TYPE=Release ..
    ```

??? question "Compilation errors"

    **Symptoms:** Fortran compilation fails

    **Solutions:**
    - Check compiler version compatibility
    - Verify all dependencies are installed
    - Check for missing modules or libraries

    ```bash
    # Check compiler version
    gfortran --version

    # Verify NetCDF installation
    nc-config --all
    ```

### Runtime Issues

??? question "Segmentation fault on startup"

    **Symptoms:** Immediate crash with segfault

    **Solutions:**
    1. Check input file paths and permissions
    2. Verify memory limits
    3. Run with debugger

    ```bash
    # Run with gdb
    gdb ./catchem_driver
    (gdb) run --config config.yml
    (gdb) bt  # Get backtrace after crash
    ```

??? question "Configuration file not found"

    **Symptoms:** `ERROR: Cannot read configuration file`

    **Solutions:**
    ```bash
    # Check file exists and is readable
    ls -la config.yml

    # Validate YAML syntax
    python -c "import yaml; yaml.safe_load(open('config.yml'))"
    ```

??? question "Invalid species or field names"

    **Symptoms:** `ERROR: Species 'dust1' not found in state container`

    **Solutions:**
    - Check species configuration
    - Verify field mapping
    - Review input data files

    ```yaml
    # Example species configuration
    species:
      - name: "dust1"
        units: "kg/kg"
        description: "Fine dust"
    ```

### Performance Issues

??? question "Extremely slow execution"

    **Symptoms:** Much slower than expected

    **Solutions:**
    1. Check diagnostic output frequency
    2. Verify column processing is enabled
    3. Review time step size

    ```yaml
    # Optimize settings
    processes:
      - name: "settling"
        parameters:
          cfl_max: 0.8  # Increase for larger time steps

    output:
      diagnostics:
        settling_velocity:
          frequency: 3600  # Less frequent output
    ```

??? question "Out of memory errors"

    **Symptoms:** `ERROR: Cannot allocate memory`

    **Solutions:**
    ```bash
    # Check system memory
    free -h

    # Monitor memory usage
    top -p $(pgrep catchem_driver)
    ```

    ```yaml
    # Reduce memory usage
    state_management:
      garbage_collection: true
      max_memory_usage: "8GB"
    ```

### Physics and Science Issues

??? question "Unrealistic settling velocities"

    **Symptoms:** Settling velocities too high/low

    **Solutions:**
    1. Check particle size distribution
    2. Verify temperature and pressure fields
    3. Review slip correction settings

    ```yaml
    # Debug settling
    output:
      diagnostics:
        settling_velocity: {enabled: true}
        slip_correction: {enabled: true}
        particle_reynolds: {enabled: true}
    ```

??? question "Mass conservation issues"

    **Symptoms:** Species concentrations becoming negative or unrealistic

    **Solutions:**
    1. Reduce time step size
    2. Check boundary conditions
    3. Enable mass conservation diagnostics

    ```yaml
    # Mass conservation checking
    processes:
      - name: "settling"
        parameters:
          enforce_mass_conservation: true
          negative_concentration_handling: "clip"
    ```

### Input/Output Issues

??? question "NetCDF file errors"

    **Symptoms:** `ERROR: Cannot read NetCDF file`

    **Solutions:**
    ```bash
    # Check NetCDF file
    ncdump -h input_file.nc

    # Verify file permissions
    ls -la input_file.nc

    # Test NetCDF installation
    nc-config --version
    ```

??? question "Missing required fields"

    **Symptoms:** `ERROR: Required field 'temperature' not found`

    **Solutions:**
    - Check input file variable names
    - Verify field mapping configuration
    - Review units and dimensions

    ```yaml
    # Field mapping example
    field_mapping:
      temperature:
        input_name: "T"
        units: "K"
      pressure:
        input_name: "P"
        units: "Pa"
    ```

## Debugging Tools

### Enable Verbose Logging

```yaml
# Increase logging detail
logging:
  level: "debug"           # error, warning, info, debug, trace
  file: "catchem_debug.log"
  console: true

  # Process-specific logging
  processes:
    settling: "debug"
    chemistry: "info"
```

### Built-in Diagnostics

```yaml
# Enable comprehensive diagnostics
debug:
  enabled: true
  level: "verbose"

diagnostics:
  # State container diagnostics
  state_summary:
    enabled: true
    frequency: 600

  # Process timing
  process_timing:
    enabled: true
    frequency: 300

  # Memory usage
  memory_usage:
    enabled: true
    frequency: 600
```

### External Tools

```bash
# Memory leak detection with Valgrind
valgrind --leak-check=full ./catchem_driver --config config.yml

# Performance profiling
perf record ./catchem_driver --config config.yml
perf report

# Fortran debugging with gdb
gdb --args ./catchem_driver --config config.yml
```

## Log File Analysis

### Common Log Patterns

```bash
# Find errors
grep -i error catchem.log

# Check timing information
grep "Process.*took" catchem.log

# Memory usage patterns
grep "Memory usage" catchem.log | tail -20

# Configuration issues
grep -i "config\|yaml" catchem.log
```

### Understanding Error Messages

| Error Pattern | Meaning | Solution |
|---------------|---------|----------|
| `RC != 0` | Return code indicates failure | Check preceding error messages |
| `Allocation failed` | Memory allocation error | Reduce memory usage or increase limits |
| `Invalid index` | Array bounds error | Check array dimensions and indices |
| `NetCDF Error` | File I/O problem | Verify file paths and permissions |

## Getting Help

### Before Asking for Help

1. **Check logs** for error messages
2. **Verify configuration** syntax and values
3. **Test with minimal example** to isolate the issue
4. **Search documentation** for similar issues

### Information to Provide

When reporting issues, include:

- **CATChem version** and build information
- **Complete error message** from logs
- **Configuration file** (or relevant sections)
- **System information** (OS, compiler, libraries)
- **Steps to reproduce** the problem

### Where to Get Help

- **GitHub Issues**: [Report bugs and request features](https://github.com/NOAA-GSL/CATChem/issues)
- **GitHub Discussions**: [Ask questions and share ideas](https://github.com/NOAA-GSL/CATChem/discussions)
- **Documentation**: Search this documentation site
- **Email Support**: [gsl.help@noaa.gov](mailto:gsl.help@noaa.gov)

## Prevention

### Best Practices

1. **Start Simple**: Begin with basic configuration
2. **Test Incrementally**: Add complexity gradually
3. **Monitor Resources**: Track memory and CPU usage
4. **Validate Inputs**: Check input data quality
5. **Regular Backups**: Save working configurations

### Configuration Validation

```bash
# Validate configuration before running
catchem_driver --validate-config config.yml --dry-run

# Check input files
catchem_driver --check-inputs config.yml
```

---

*If you encounter issues not covered here, please check the [GitHub Issues](https://github.com/NOAA-GSL/CATChem/issues) or create a new issue with detailed information.*

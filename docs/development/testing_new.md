# Testing

This document describes the testing framework and procedures for the Canopy-App model, including unit tests, integration tests, and validation approaches.

## Testing Framework

### Overview

The Canopy-App uses a comprehensive testing strategy:

1. **Unit Tests**: Test individual modules and subroutines
2. **Integration Tests**: Test component interactions
3. **System Tests**: Test complete model runs
4. **Validation Tests**: Compare against observations
5. **Performance Tests**: Monitor computational performance
6. **Regression Tests**: Ensure changes don't break functionality

### Test Organization

```
test/
├── unit/                    # Unit tests for individual modules
│   ├── test_canopy_rad.F90
│   ├── test_canopy_phot.F90
│   ├── test_canopy_wind.F90
│   └── ...
├── integration/             # Integration tests
│   ├── test_full_physics.F90
│   ├── test_io_chain.F90
│   └── ...
├── system/                  # Full system tests
│   ├── basic_run/
│   ├── chemistry_run/
│   └── ...
├── validation/              # Validation against observations
│   ├── fluxnet_sites/
│   ├── field_campaigns/
│   └── ...
├── performance/             # Performance benchmarks
│   ├── scaling_tests/
│   └── memory_tests/
└── framework/               # Testing utilities
    ├── test_framework.F90
    └── test_utilities.F90
```

## Unit Testing

### Test Framework

```fortran
module canopy_test_framework
  implicit none
  private

  public :: assert_equal, assert_real_equal, assert_true, assert_false
  public :: assert_range, setup_test, teardown_test
  public :: run_test_suite

  integer, save :: test_count = 0
  integer, save :: test_passed = 0
  integer, save :: test_failed = 0

contains

  subroutine assert_equal(expected, actual, message)
    integer, intent(in) :: expected, actual
    character(len=*), intent(in) :: message

    test_count = test_count + 1
    if (expected == actual) then
      test_passed = test_passed + 1
      write(*,'(A,I0,A)') 'PASS [', test_count, ']: ' // trim(message)
    else
      test_failed = test_failed + 1
      write(*,'(A,I0,A,I0,A,I0)') 'FAIL [', test_count, ']: ' // trim(message) // &
                                  ' (expected=', expected, ', actual=', actual, ')'
    end if
  end subroutine

  subroutine assert_real_equal(expected, actual, tolerance, message)
    real(r8), intent(in) :: expected, actual, tolerance
    character(len=*), intent(in) :: message

    test_count = test_count + 1
    if (abs(expected - actual) <= tolerance) then
      test_passed = test_passed + 1
      write(*,'(A,I0,A)') 'PASS [', test_count, ']: ' // trim(message)
    else
      test_failed = test_failed + 1
      write(*,'(A,I0,A,ES12.5,A,ES12.5,A,ES12.5)') &
        'FAIL [', test_count, ']: ' // trim(message) // &
        ' (expected=', expected, ', actual=', actual, ', diff=', abs(expected-actual), ')'
    end if
  end subroutine

  subroutine assert_range(value, min_val, max_val, message)
    real(r8), intent(in) :: value, min_val, max_val
    character(len=*), intent(in) :: message

    test_count = test_count + 1
    if (value >= min_val .and. value <= max_val) then
      test_passed = test_passed + 1
      write(*,'(A,I0,A)') 'PASS [', test_count, ']: ' // trim(message)
    else
      test_failed = test_failed + 1
      write(*,'(A,I0,A,ES12.5,A,ES12.5,A,ES12.5)') &
        'FAIL [', test_count, ']: ' // trim(message) // &
        ' (value=', value, ', min=', min_val, ', max=', max_val, ')'
    end if
  end subroutine

  subroutine run_test_suite()
    write(*,'(A)') '========================================'
    write(*,'(A,I0)') 'Total tests: ', test_count
    write(*,'(A,I0)') 'Passed: ', test_passed
    write(*,'(A,I0)') 'Failed: ', test_failed
    write(*,'(A,F6.2,A)') 'Success rate: ', real(test_passed)/real(test_count)*100, '%'
    write(*,'(A)') '========================================'

    if (test_failed > 0) then
      stop 1  ! Exit with error code
    end if
  end subroutine

end module
```

### Example Unit Tests

#### Testing Radiation Module

```fortran
module test_canopy_rad
  use canopy_test_framework
  use canopy_rad_mod
  use canopy_const_mod, only: r8
  implicit none

contains

  subroutine test_solar_zenith_angle()
    real(r8) :: zenith, expected
    real(r8) :: lat, lon, julian_day, hour

    ! Test case: Noon at equinox, equator
    lat = 0.0_r8
    lon = 0.0_r8
    julian_day = 80.0_r8  ! Spring equinox
    hour = 12.0_r8

    call calc_solar_zenith_angle(lat, lon, julian_day, hour, zenith)
    expected = 0.0_r8  ! Should be directly overhead

    call assert_real_equal(expected, zenith, 0.01_r8, 'Solar zenith at equinox noon')
  end subroutine

  subroutine test_radiation_extinction()
    real(r8) :: ppfd_top, ppfd_bottom, lai, k_par
    real(r8) :: expected

    ! Test exponential extinction
    ppfd_top = 2000.0_r8
    lai = 4.0_r8
    k_par = 0.5_r8

    call calc_ppfd_profile(ppfd_top, lai, k_par, ppfd_bottom)
    expected = ppfd_top * exp(-k_par * lai)

    call assert_real_equal(expected, ppfd_bottom, 1.0_r8, 'PPFD exponential extinction')
  end subroutine

  subroutine test_two_stream_radiation()
    real(r8) :: ppfd_down(10), ppfd_up(10)
    real(r8) :: lai_profile(10)
    real(r8) :: leaf_refl, leaf_trans
    integer :: i

    ! Setup test canopy
    do i = 1, 10
      lai_profile(i) = 0.4_r8  ! 0.4 LAI per layer
    end do
    leaf_refl = 0.1_r8
    leaf_trans = 0.05_r8

    call two_stream_radiation(lai_profile, leaf_refl, leaf_trans, &
                             ppfd_down, ppfd_up)

    ! Test that downward radiation decreases with depth
    do i = 2, 10
      if (ppfd_down(i) >= ppfd_down(i-1)) then
        call assert_true(.false., 'Downward PPFD should decrease with depth')
        exit
      end if
    end do
    call assert_true(.true., 'Downward PPFD decreases with depth')

    ! Test conservation
    call assert_range(sum(ppfd_up) + sum(ppfd_down), 0.8_r8, 1.2_r8, &
                     'Radiation conservation')
  end subroutine

  subroutine run_radiation_tests()
    write(*,'(A)') 'Running radiation module tests...'
    call test_solar_zenith_angle()
    call test_radiation_extinction()
    call test_two_stream_radiation()
  end subroutine

end module
```

#### Testing Photosynthesis Module

```fortran
module test_canopy_phot
  use canopy_test_framework
  use canopy_phot_mod
  use canopy_const_mod, only: r8
  implicit none

contains

  subroutine test_farquhar_model()
    real(r8) :: vcmax, jmax, ppfd, ci, temp, o2
    real(r8) :: anet, expected

    ! Standard C3 parameters
    vcmax = 60.0_r8
    jmax = 120.0_r8
    ppfd = 1000.0_r8
    ci = 240.0_r8  ! μmol/mol
    temp = 298.15_r8  ! 25°C
    o2 = 210000.0_r8  ! μmol/mol

    call farquhar_photosynthesis(vcmax, jmax, ppfd, ci, temp, o2, anet)

    ! Expected range for these conditions
    call assert_range(anet, 15.0_r8, 35.0_r8, 'Farquhar photosynthesis rate')
  end subroutine

  subroutine test_temperature_response()
    real(r8) :: vcmax25, temp, vcmax_temp
    real(r8) :: expected_ratio

    vcmax25 = 60.0_r8
    temp = 308.15_r8  ! 35°C

    call temperature_response_vcmax(vcmax25, temp, vcmax_temp)

    ! Should increase with temperature (Q10 ~ 2)
    expected_ratio = vcmax_temp / vcmax25
    call assert_range(expected_ratio, 1.5_r8, 3.0_r8, 'Vcmax temperature response')
  end subroutine

  subroutine test_light_response()
    real(r8) :: jmax, alpha, ppfd, j_rate
    real(r8) :: theta = 0.7_r8

    jmax = 120.0_r8
    alpha = 0.3_r8

    ! Test low light (should be light-limited)
    ppfd = 100.0_r8
    call light_response_j(jmax, alpha, theta, ppfd, j_rate)
    call assert_range(j_rate, 20.0_r8, 40.0_r8, 'J rate at low light')

    ! Test high light (should approach Jmax)
    ppfd = 2000.0_r8
    call light_response_j(jmax, alpha, theta, ppfd, j_rate)
    call assert_range(j_rate, 100.0_r8, 120.0_r8, 'J rate at high light')
  end subroutine

  subroutine run_photosynthesis_tests()
    write(*,'(A)') 'Running photosynthesis module tests...'
    call test_farquhar_model()
    call test_temperature_response()
    call test_light_response()
  end subroutine

end module
```

### Running Unit Tests

```bash
#!/bin/bash
# run_unit_tests.sh

echo "Compiling unit tests..."
cd test/unit

# Compile test framework
gfortran -c ../../src/canopy_const_mod.F90
gfortran -c ../framework/test_framework.F90

# Compile and run each test
for test_file in test_*.F90; do
  module_name=$(basename $test_file .F90)
  echo "Running $module_name..."

  # Compile dependencies
  deps=$(grep "use canopy_" $test_file | sed 's/.*use \(canopy_[^,]*\).*/\1/' | sort | uniq)
  for dep in $deps; do
    if [ -f "../../src/${dep}.F90" ]; then
      gfortran -c "../../src/${dep}.F90"
    fi
  done

  # Compile and run test
  gfortran -c $test_file
  gfortran -o ${module_name}.exe *.o
  ./${module_name}.exe

  if [ $? -ne 0 ]; then
    echo "FAILED: $module_name"
    exit 1
  fi
done

echo "All unit tests passed!"
```

## Integration Testing

### Physics Integration Tests

```fortran
module test_full_physics
  use canopy_test_framework
  use canopy_app
  implicit none

contains

  subroutine test_energy_conservation()
    real(r8) :: net_radiation, sensible_heat, latent_heat
    real(r8) :: energy_balance, tolerance

    ! Run single timestep
    call canopy_calcs(1)

    ! Get energy components
    net_radiation = sum(rnet)
    sensible_heat = sum(hflx)
    latent_heat = sum(lhflx)

    ! Check energy balance
    energy_balance = net_radiation - sensible_heat - latent_heat
    tolerance = 0.05 * abs(net_radiation)  ! 5% tolerance

    call assert_real_equal(0.0_r8, energy_balance, tolerance, &
                          'Surface energy balance')
  end subroutine

  subroutine test_mass_conservation()
    real(r8) :: evapotranspiration, water_input
    real(r8) :: mass_balance, tolerance

    ! Run timestep with known inputs
    water_input = 0.0_r8  ! No precipitation
    call canopy_calcs(1)

    evapotranspiration = sum(et_rate) * dt
    mass_balance = water_input - evapotranspiration
    tolerance = 1.0e-6_r8  ! Very small tolerance for mass

    call assert_real_equal(water_input, evapotranspiration, tolerance, &
                          'Water mass balance')
  end subroutine

  subroutine test_carbon_cycle()
    real(r8) :: photosynthesis, respiration, nee

    call canopy_calcs(1)

    photosynthesis = sum(anet_sun + anet_shade)
    respiration = sum(resp_leaf + resp_soil)
    nee = respiration - photosynthesis  ! Net ecosystem exchange

    ! During growing season with good light, should be carbon sink
    call assert_range(nee, -50.0_r8, 10.0_r8, 'Net ecosystem exchange')
  end subroutine

  subroutine run_integration_tests()
    write(*,'(A)') 'Running integration tests...'

    ! Initialize model with test configuration
    call canopy_init()

    call test_energy_conservation()
    call test_mass_conservation()
    call test_carbon_cycle()

    call canopy_dealloc()
  end subroutine

end module
```

## System Testing

### Basic Model Run Test

```bash
#!/bin/bash
# test_basic_run.sh

echo "Testing basic model run..."

# Create test directory
mkdir -p test_run
cd test_run

# Create minimal namelist
cat > namelist.canopy << EOF
&CANOPY_OPTIONS
 nlat = 1
 nlon = 1
 ntime = 24
 dx = 100.0
 dy = 100.0
 infmt_opt = 2
/
EOF

# Create minimal input file
cat > input_data.txt << EOF
# Test meteorological data
# Time: 2022-07-01_12:00:00
295.15  0.012  101325.0  0.45  5.2  270.0  850.0
295.20  0.012  101320.0  0.46  5.3  272.0  855.0
EOF

# Run model
timeout 60 ../canopy_app.exe

if [ $? -eq 0 ]; then
  echo "Basic run test PASSED"

  # Check output files exist
  if [ -f "canopy_output.nc" ]; then
    echo "Output file created successfully"
  else
    echo "WARNING: Output file not created"
  fi
else
  echo "Basic run test FAILED"
  exit 1
fi

cd ..
rm -rf test_run
```

### Chemistry Test

```bash
#!/bin/bash
# test_chemistry.sh

echo "Testing with chemistry enabled..."

mkdir -p chemistry_test
cd chemistry_test

# Namelist with chemistry
cat > namelist.canopy << EOF
&CANOPY_OPTIONS
 nlat = 1
 nlon = 1
 ntime = 12
 chemistry_enabled = .true.
 biogenic_emissions = .true.
/

&CHEMISTRY_OPTIONS
 n_species = 5
 species_names = 'CO2', 'H2O', 'O3', 'NO', 'ISOP'
/
EOF

# Create input with chemistry data
# ... (create appropriate input files) ...

timeout 120 ../canopy_app.exe

if [ $? -eq 0 ]; then
  echo "Chemistry test PASSED"

  # Check for chemistry output
  if command -v ncdump &> /dev/null; then
    if ncdump -h canopy_output.nc | grep -q "isop_emis"; then
      echo "Chemistry variables found in output"
    else
      echo "WARNING: Chemistry variables not found"
    fi
  fi
else
  echo "Chemistry test FAILED"
  exit 1
fi

cd ..
rm -rf chemistry_test
```

## Validation Testing

### Flux Tower Comparison

```python
#!/usr/bin/env python3
# validate_fluxnet.py

import numpy as np
import netCDF4 as nc
import pandas as pd
import matplotlib.pyplot as plt
from scipy.stats import pearsonr

def load_model_output(filename):
    """Load model output"""
    ds = nc.Dataset(filename, 'r')
    data = {
        'time': ds.variables['time'][:],
        'sensible_heat': ds.variables['sensible_heat'][:],
        'latent_heat': ds.variables['latent_heat'][:],
        'co2_flux': ds.variables['co2_flux'][:]
    }
    ds.close()
    return data

def load_observations(filename):
    """Load flux tower observations"""
    obs = pd.read_csv(filename)
    return {
        'time': obs['time'].values,
        'sensible_heat': obs['H'].values,  # W/m²
        'latent_heat': obs['LE'].values,   # W/m²
        'co2_flux': obs['NEE'].values      # μmol/m²/s
    }

def calculate_statistics(model, obs):
    """Calculate validation statistics"""
    # Remove missing values
    mask = ~(np.isnan(model) | np.isnan(obs))
    model_clean = model[mask]
    obs_clean = obs[mask]

    if len(model_clean) == 0:
        return {'r': np.nan, 'rmse': np.nan, 'bias': np.nan, 'n': 0}

    # Correlation coefficient
    r, _ = pearsonr(model_clean, obs_clean)

    # Root mean square error
    rmse = np.sqrt(np.mean((model_clean - obs_clean)**2))

    # Bias
    bias = np.mean(model_clean - obs_clean)

    return {'r': r, 'rmse': rmse, 'bias': bias, 'n': len(model_clean)}

def validate_site(site_name, model_file, obs_file):
    """Validate model against observations for one site"""
    print(f"Validating {site_name}...")

    # Load data
    model_data = load_model_output(model_file)
    obs_data = load_observations(obs_file)

    # Interpolate to common time grid
    # (Simplified - real implementation would be more sophisticated)

    results = {}
    for var in ['sensible_heat', 'latent_heat', 'co2_flux']:
        stats = calculate_statistics(model_data[var], obs_data[var])
        results[var] = stats

        print(f"  {var}:")
        print(f"    r = {stats['r']:.3f}")
        print(f"    RMSE = {stats['rmse']:.2f}")
        print(f"    Bias = {stats['bias']:.2f}")
        print(f"    n = {stats['n']}")

    return results

def main():
    """Run validation for multiple sites"""
    sites = {
        'US-Ha1': ('model_output_ha1.nc', 'observations_ha1.csv'),
        'US-MMS': ('model_output_mms.nc', 'observations_mms.csv'),
        'CA-Oas': ('model_output_oas.nc', 'observations_oas.csv')
    }

    all_results = {}

    for site_name, (model_file, obs_file) in sites.items():
        try:
            results = validate_site(site_name, model_file, obs_file)
            all_results[site_name] = results
        except Exception as e:
            print(f"Failed to validate {site_name}: {e}")

    # Summary statistics
    print("\n=== VALIDATION SUMMARY ===")
    for var in ['sensible_heat', 'latent_heat', 'co2_flux']:
        r_values = [all_results[site][var]['r'] for site in all_results
                   if not np.isnan(all_results[site][var]['r'])]
        if r_values:
            print(f"{var}: mean r = {np.mean(r_values):.3f} ± {np.std(r_values):.3f}")

    # Check performance criteria
    performance_ok = True
    for site in all_results:
        for var in ['sensible_heat', 'latent_heat']:
            r = all_results[site][var]['r']
            if r < 0.6:  # Minimum acceptable correlation
                print(f"WARNING: Low correlation for {var} at {site}: r = {r:.3f}")
                performance_ok = False

    if performance_ok:
        print("Validation PASSED")
        return 0
    else:
        print("Validation FAILED")
        return 1

if __name__ == '__main__':
    exit(main())
```

## Performance Testing

### Scaling Tests

```bash
#!/bin/bash
# test_scaling.sh

echo "Testing parallel scaling..."

# Test different thread counts
for threads in 1 2 4 8 16; do
  export OMP_NUM_THREADS=$threads
  echo "Testing with $threads threads:"

  # Run multiple times for statistical significance
  times=()
  for run in {1..3}; do
    start_time=$(date +%s.%N)
    ./canopy_app.exe test_config.nml > /dev/null 2>&1
    end_time=$(date +%s.%N)
    runtime=$(echo "$end_time - $start_time" | bc)
    times+=($runtime)
  done

  # Calculate average time
  avg_time=$(echo "${times[@]}" | awk '{sum=0; for(i=1;i<=NF;i++)sum+=$i; print sum/NF}')
  echo "  Average time: $avg_time seconds"

  # Calculate efficiency (compared to single thread)
  if [ $threads -eq 1 ]; then
    serial_time=$avg_time
    efficiency=100
  else
    speedup=$(echo "scale=2; $serial_time / $avg_time" | bc)
    efficiency=$(echo "scale=1; $speedup / $threads * 100" | bc)
  fi

  echo "  Efficiency: $efficiency%"
done
```

### Memory Tests

```bash
#!/bin/bash
# test_memory.sh

echo "Testing memory usage..."

# Monitor memory during execution
valgrind --tool=massif --time-unit=ms ./canopy_app.exe test_config.nml

# Check for memory leaks
valgrind --leak-check=full --show-leak-kinds=all ./canopy_app.exe test_config.nml > valgrind.log 2>&1

# Parse results
if grep -q "definitely lost: 0 bytes" valgrind.log; then
  echo "Memory leak test PASSED"
else
  echo "Memory leak test FAILED"
  grep "definitely lost" valgrind.log
  exit 1
fi

# Check peak memory usage
peak_memory=$(grep "mem_heap_B" massif.out.* | sort -k2 -n | tail -1 | awk '{print $2}')
echo "Peak memory usage: $(($peak_memory / 1024 / 1024)) MB"

# Cleanup
rm -f massif.out.* valgrind.log
```

## Continuous Integration

### GitHub Actions Workflow

```yaml
# .github/workflows/test.yml
name: Continuous Integration

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  test:
    runs-on: ubuntu-latest

    strategy:
      matrix:
        compiler: [gfortran, ifort]

    steps:
    - uses: actions/checkout@v3

    - name: Install dependencies
      run: |
        sudo apt-get update
        sudo apt-get install -y gfortran libnetcdff-dev

    - name: Compile model
      run: |
        cd src
        make clean
        make FC=${{ matrix.compiler }}

    - name: Run unit tests
      run: |
        cd test
        ./run_unit_tests.sh

    - name: Run integration tests
      run: |
        cd test
        ./run_integration_tests.sh

    - name: Run system tests
      run: |
        cd test
        ./run_system_tests.sh

    - name: Check performance
      run: |
        cd test
        ./test_performance.sh
```

## Test Data Management

### Test Data Repository

```bash
#!/bin/bash
# setup_test_data.sh

# Download test datasets
mkdir -p test_data

# Meteorological test data
wget -O test_data/met_data.nc \
  "https://example.com/canopy_test_data/meteorology.nc"

# Flux tower observations
wget -O test_data/fluxnet_obs.csv \
  "https://example.com/canopy_test_data/fluxnet.csv"

# Validation datasets
for site in US-Ha1 US-MMS CA-Oas; do
  wget -O test_data/${site}_obs.csv \
    "https://example.com/canopy_test_data/${site}.csv"
done

echo "Test data setup complete"
```

This comprehensive testing framework ensures the reliability, accuracy, and performance of the Canopy-App model across different use cases and computational environments.

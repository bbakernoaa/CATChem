# Testing Developer Guide

Comprehensive testing strategy for CATChem development.

## Testing Philosophy

CATChem follows a multi-layered testing approach:

- **Unit Tests**: Individual module and procedure testing
- **Integration Tests**: Component interaction testing
- **System Tests**: End-to-end model validation
- **Performance Tests**: Benchmarking and profiling
- **Regression Tests**: Continuous validation against reference results

## Test Organization

```
tests/
├── unit/              # Unit tests for individual modules
├── integration/       # Integration and component tests
├── system/           # Full model system tests
├── performance/      # Performance benchmarks
├── regression/       # Regression test suite
└── fixtures/         # Test data and configurations
```

## Unit Testing

### Test Framework

CATChem uses a custom Fortran testing framework:

```fortran
program test_settling_process
  use testing_mod
  use settlingProcess_Mod
  implicit none

  call test_suite_begin("Settling Process Tests")

  call test_initialization()
  call test_stokes_velocity()
  call test_slip_correction()
  call test_cfl_stability()

  call test_suite_end()

contains

  subroutine test_stokes_velocity()
    call test_begin("Stokes velocity calculation")

    ! Test implementation
    call assert_equal(expected, actual, tolerance, "Velocity calculation")

    call test_end()
  end subroutine

end program
```

### Running Unit Tests

```bash
# Build and run all unit tests
cd build
make test

# Run specific test suite
ctest -R settling -V

# Run with detailed output
ctest --output-on-failure
```

### Writing Unit Tests

```fortran
! Test template
subroutine test_my_procedure()
  use testing_mod
  use MyModule_Mod
  implicit none

  ! Setup
  real(fp) :: input_data(10)
  real(fp) :: expected_result
  real(fp) :: actual_result
  integer :: rc

  ! Initialize test data
  input_data = [1.0, 2.0, 3.0, ...]
  expected_result = 42.0

  ! Execute
  call my_procedure(input_data, actual_result, rc)

  ! Verify
  call assert_success(rc, "Procedure should succeed")
  call assert_near(expected_result, actual_result, 1.0e-10_fp, &
                   "Result should match expected value")

end subroutine
```

## Integration Testing

### Process Integration

```fortran
! Test process integration with StateContainer
program test_process_integration
  use settlingProcess_Mod
  use state_mod
  implicit none

  type(StateContainerType) :: container
  type(settlingProcessType) :: settling
  integer :: rc

  ! Initialize state container with test data
  call setup_test_container(container)

  ! Test process lifecycle
  call settling%init(container, rc)
  call assert_success(rc, "Process initialization")

  call settling%run(container, rc)
  call assert_success(rc, "Process execution")

  call settling%finalize(rc)
  call assert_success(rc, "Process finalization")

end program
```

### Multi-Process Testing

```fortran
! Test multiple processes working together
call test_process_chain()

subroutine test_process_chain()
  ! Test emission -> chemistry -> settling -> deposition chain
  call emission_process%run(container, rc)
  call chemistry_process%run(container, rc)
  call settling_process%run(container, rc)
  call deposition_process%run(container, rc)

  ! Verify mass conservation
  call verify_mass_balance(container)
end subroutine
```

## System Testing

### Full Model Tests

```bash
# Run complete model test cases
cd tests/system

# Basic functionality test
./test_basic_run.sh

# Physics validation test
./test_physics_validation.sh

# Performance benchmark
./test_performance_benchmark.sh
```

### Test Configurations

```yaml
# Test configuration example
test_case: "settling_validation"
description: "Validate settling physics against analytical solution"

model_config:
  time_step: 60.0
  run_duration: 3600.0

processes:
  - name: "settling"
    scheme: "Stokesscheme"
    enabled: true

validation:
  reference_data: "analytical_solution.nc"
  tolerance: 1.0e-6
  fields_to_check: ["settling_velocity", "concentration"]
```

## Performance Testing

### Benchmarking

```fortran
! Performance benchmark
program benchmark_settling
  use iso_fortran_env
  use settlingProcess_Mod
  implicit none

  integer(int64) :: start_time, end_time, count_rate
  real(fp) :: elapsed_time

  call system_clock(start_time, count_rate)

  ! Run benchmark
  do i = 1, num_iterations
    call settling%run(container, rc)
  end do

  call system_clock(end_time)
  elapsed_time = real(end_time - start_time, fp) / real(count_rate, fp)

  print *, "Elapsed time: ", elapsed_time, " seconds"
  print *, "Iterations per second: ", num_iterations / elapsed_time

end program
```

### Profiling

```bash
# Profile with gprof
gfortran -pg -O2 -o catchem_profile src/*.f90
./catchem_profile
gprof catchem_profile gmon.out > profile.txt

# Profile with perf
perf record ./catchem_driver --config test.yml
perf report
```

## Regression Testing

### Continuous Integration

```yaml
# GitHub Actions workflow
name: CATChem Tests
on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v2
    - name: Setup Fortran
      uses: fortran-lang/setup-fortran@v1
    - name: Build
      run: |
        mkdir build && cd build
        cmake .. && make
    - name: Test
      run: |
        cd build && ctest --output-on-failure
```

### Reference Results

```bash
# Generate reference results
./generate_reference.sh config/reference_case.yml

# Compare against reference
./compare_results.sh output.nc reference/output_ref.nc
```

## Test Data Management

### Test Fixtures

```fortran
! Test data generation
subroutine setup_test_atmosphere(container)
  type(StateContainerType), intent(inout) :: container

  ! Create realistic atmospheric profile
  call container%set_field("temperature", temp_profile, rc)
  call container%set_field("pressure", pres_profile, rc)
  call container%set_field("density", dens_profile, rc)

end subroutine
```

### Data Validation

```bash
# Validate test input data
python scripts/validate_test_data.py tests/fixtures/
```

## Debugging Tests

### Test Debugging

```fortran
! Debug test failures
if (test_failed) then
  print *, "Test failed at line: ", __LINE__
  print *, "Expected: ", expected
  print *, "Actual: ", actual
  print *, "Difference: ", abs(expected - actual)

  ! Dump debug information
  call dump_state_container(container, "debug_output.nc")
end if
```

### Memory Testing

```bash
# Check for memory leaks
valgrind --leak-check=full ./test_program

# Check for array bounds violations
gfortran -fbounds-check -o test_debug test.f90
```

## Best Practices

### Test Development

1. **Test-Driven Development**: Write tests before implementation
2. **Comprehensive Coverage**: Test all code paths and edge cases
3. **Realistic Data**: Use physically meaningful test data
4. **Clear Assertions**: Make test failures informative
5. **Fast Execution**: Keep unit tests quick to run

### Test Maintenance

1. **Regular Updates**: Keep tests current with code changes
2. **Reference Management**: Maintain and update reference results
3. **Documentation**: Document test purposes and expected outcomes
4. **Automation**: Automate test execution and reporting

## Continuous Integration

### Automated Testing

```bash
# Pre-commit hooks
#!/bin/bash
# Run tests before each commit
cd build && make test
if [ $? -ne 0 ]; then
  echo "Tests failed - commit aborted"
  exit 1
fi
```

### Quality Gates

- All unit tests must pass
- Code coverage > 80%
- Performance regression < 5%
- Memory leak detection clean
- Static analysis warnings addressed

---

*Testing is critical for maintaining CATChem's reliability and performance. Every contribution should include appropriate tests.*

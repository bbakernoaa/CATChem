# Performance Guide

This guide covers performance optimization, profiling, and best practices for CATChem development.

## Overview

CATChem is designed for high-performance atmospheric chemistry modeling with several key optimization strategies:

- **Column Virtualization**: Efficient memory layout and cache utilization
- **Process Modularity**: Minimal overhead between processes
- **Memory Management**: Reduced allocations and optimized data structures
- **Parallel Scalability**: Linear scaling to thousands of cores

## Performance Architecture

### Column Virtualization

CATChem's column virtualization provides significant performance benefits:

```fortran
! Traditional approach - poor cache utilization
do k = 1, nlevs
  do j = 1, nlats
    do i = 1, nlons
      call process_point(state(i,j,k))
    end do
  end do
end do

! CATChem virtualized approach - optimized cache usage
do col = 1, virtual_columns
  call process_column(virtual_state(col))
end do
```

**Benefits:**

- Faster column processing
- Better cache utilization
- Vectorization opportunities
- Reduced memory bandwidth

### Memory Optimization

**State Container Design:**

- Contiguous memory layout
- Minimal copying between processes
- Efficient species indexing
- Cache-friendly data structures

**Allocation Strategy:**

- Pre-allocated buffers
- Pool-based memory management
- Minimal runtime allocations
- NUMA-aware placement

## Profiling {#profiling}

### Built-in Profiling

CATChem includes built-in timing and profiling capabilities:

```yaml
# Configuration for profiling
profiling:
  enabled: true
  level: detailed  # basic, detailed, comprehensive
  output: catchem_profile.json
  processes:
    - chemistry
    - emissions
    - transport
```

**Profiling Output:**
```json
{
  "total_time": 45.2,
  "processes": {
    "chemistry": {"time": 32.1, "calls": 1440},
    "emissions": {"time": 8.7, "calls": 1440},
    "transport": {"time": 4.4, "calls": 1440}
  }
}
```

### External Profiling Tools

**Intel VTune:**
```bash
# Compile with profiling support
export FCFLAGS="-g -O2 -prof-gen=srcpos"
make clean && make

# Run with VTune
vtune -collect hotspots -app-working-dir . ./catchem_driver
```

**NVIDIA Nsight:**
```bash
# For GPU profiling
nsys profile --trace=cuda,openmp ./catchem_driver
```

**GNU gprof:**
```bash
# Compile with gprof support
export FCFLAGS="-pg -O2"
make clean && make

# Run and analyze
./catchem_driver
gprof catchem_driver gmon.out > profile.txt
```

## Optimization Strategies

### Compiler Optimization

**Recommended Compiler Flags:**

**Intel Fortran:**
```bash
export FCFLAGS="-O3 -xHOST -ipo -no-prec-div -fp-model fast=2"
```

**GNU Fortran:**
```bash
export FCFLAGS="-O3 -march=native -ffast-math -funroll-loops"
```

**NVIDIA HPC SDK:**
```bash
export FCFLAGS="-O3 -fast -Mipa=fast"
```

### Process-Level Optimization

**Chemistry Optimization:**
```fortran
! Use efficient solvers
type(chemistry_config) :: chem_config
chem_config%solver = 'rosenbrock'  ! vs 'euler'
chem_config%adaptive_timestep = .true.
chem_config%error_tolerance = 1.0e-3
```

**Transport Optimization:**
```fortran
! Minimize vertical levels processing
type(transport_config) :: trans_config
trans_config%skip_levels = [1, nlev]  ! Surface and top
trans_config%vectorized = .true.
```

### Memory Optimization

**Configuration Tuning:**
```yaml
# Optimize memory usage
memory:
  allocation_strategy: pool
  buffer_size_mb: 512
  numa_aware: true

state_management:
  cache_size: 1000  # Number of columns to cache
  prefetch_enabled: true
```

## Parallel Performance

### OpenMP Optimization

**Thread Configuration:**
```bash
export OMP_NUM_THREADS=16
export OMP_PLACES=cores
export OMP_PROC_BIND=close
export OMP_SCHEDULE=dynamic
```

**Code Optimization:**
```fortran
!$OMP PARALLEL DO PRIVATE(col_state) SCHEDULE(DYNAMIC)
do col = 1, num_virtual_columns
  col_state = extract_column(state, col)
  call process_column(col_state)
  call update_column(state, col, col_state)
end do
!$OMP END PARALLEL DO
```

### MPI Scaling

**Domain Decomposition:**

- 2D horizontal decomposition
- Load balancing based on computational cost
- Minimal communication overhead

**Recommended MPI Settings:**
```bash
mpirun -np 256 \
  --map-by ppr:16:node \
  --bind-to core \
  --mca btl_openib_want_fork_support 1 \
  ./catchem_driver
```

## Performance Monitoring

### Runtime Monitoring

**Configuration:**
```yaml
monitoring:
  performance_metrics: true
  memory_usage: true
  load_balance: true
  output_frequency: 100  # timesteps
```

**Key Metrics:**

- Time per timestep
- Memory high-water mark
- Load balancing efficiency
- Cache hit rates

### Automated Performance Testing

**Performance Regression Tests:**
```bash
# Run performance benchmarks
cd tests/performance
make benchmark

# Compare against baseline
python compare_performance.py --baseline v2.0 --current HEAD
```

## Common Performance Issues

### Memory Bandwidth Limitation

**Symptoms:**

- Low CPU utilization
- High memory access times
- Poor scaling with threads

**Solutions:**

- Optimize data layout
- Reduce memory allocations
- Use compiler prefetch hints

### Load Imbalance

**Symptoms:**

- Some processes finish much earlier
- Poor parallel efficiency
- Idle cores during execution

**Solutions:**

- Dynamic load balancing
- Better domain decomposition
- Work stealing algorithms

### Cache Misses

**Symptoms:**

- High L3 cache miss rate
- Memory stalls
- Poor single-thread performance

**Solutions:**

- Column virtualization
- Data structure optimization
- Loop blocking/tiling

## Performance Best Practices

### Development Guidelines

**Profile Early and Often**

- Use built-in profiling
- Test with realistic problem sizes
- Focus on hot spots

**Memory-First Design**

- Design for cache efficiency
- Minimize allocations
- Use contiguous data structures

**Parallel-Aware Algorithms**

- Minimize synchronization
- Design for NUMA systems
- Consider false sharing

### Configuration Guidelines

**Right-Size Resources**

- Match threads to cores
- Consider memory bandwidth
- Balance computation vs I/O

**Optimize for Use Case**
- Operational vs research
- Real-time vs batch
- Accuracy vs speed trade-offs

## Platform-Specific Optimization

### Intel Xeon Systems

**Optimizations:**

- Use Intel MKL for linear algebra
- Enable AVX-512 instructions
- NUMA-aware thread placement

### AMD EPYC Systems

**Optimizations:**

- Consider CCX boundaries
- Optimize for memory bandwidth
- Use AMD BLIS/LIBFLAME

### GPU Acceleration

**NVIDIA GPU Support:**
```fortran
!$acc parallel loop collapse(2)
do k = 1, nlevs
  do col = 1, ncols
    call chemistry_kernel(state(col, k))
  end do
end do
!$acc end parallel loop
```

**Configuration:**
```yaml
gpu:
  enabled: true
  device_count: 4
  memory_pool_mb: 8192
  processes: [chemistry, emissions]
```

## Performance Validation

### Benchmarking Suite

**Standard Benchmarks:**

- Single column chemistry
- Regional domain (100x100)
- Global domain (720x360)
- Scaling tests (1-1000 cores)

**Performance Targets:**

- Single timestep: < 0.1s (regional)
- Memory usage: < 2GB per process
- Parallel efficiency: > 80% (to 256 cores)

### Continuous Integration

**Automated Performance Testing:**

- Regression detection
- Performance trend analysis
- Comparison with baselines

## Troubleshooting

### Performance Debugging

**Common Commands:**
```bash
# Check system resources
top -p $(pgrep catchem)
numastat -p $(pgrep catchem)

# Profile memory usage
valgrind --tool=massif ./catchem_driver

# Check MPI communication
mpiP ./catchem_driver
```

### Performance Metrics Analysis

**Key Performance Indicators:**

- Time per timestep
- Memory bandwidth utilization
- Cache hit rates
- MPI communication overhead
- Load balance efficiency

## References

- **[Column Virtualization Guide](core/column-virtualization.md)**
- **[State Management](core/state-management.md)**
- **[Build System Optimization](../user-guide/build-system.md)**
- **[Testing Performance](testing.md#performance-testing)**

# Column Virtualization

CATChem's column virtualization system provides efficient 1D processing with automatic parallelization and optimized memory access patterns.

## Overview

Column virtualization treats atmospheric columns as the primary computational unit, enabling:

- **Efficient Memory Access**: Improved cache locality
- **Natural Parallelization**: Column-level concurrency
- **Process Isolation**: Independent column processing
- **Scalable Performance**: Linear scaling with core count

## Architecture

### Column Interface

```fortran
module ColumnInterface_Mod
  implicit none

  type :: VirtualColumnType
    real(fp), pointer :: temperature(:) => null()    ! K
    real(fp), pointer :: pressure(:) => null()       ! Pa
    real(fp), pointer :: density(:) => null()        ! kg/m³
    real(fp), pointer :: species(:,:) => null()      ! kg/kg
    integer :: column_index
    integer :: num_levels
  contains
    procedure :: extract_from_state => vc_extract_from_state
    procedure :: update_state => vc_update_state
    procedure :: is_valid => vc_is_valid
  end type VirtualColumnType

  type :: ColumnProcessorType
    integer :: batch_size = 1000
    logical :: use_openmp = .true.
  contains
    procedure :: process_batch => cp_process_batch
    procedure :: set_batch_size => cp_set_batch_size
    procedure :: enable_openmp => cp_enable_openmp
  end type ColumnProcessorType

  type :: ColumnViewType
    class(*), pointer :: data_ptr => null()
    integer :: dimensions(3)
    integer :: column_index
  contains
    procedure :: create_view => cv_create_view
    procedure :: get_column_pointer => cv_get_column_pointer
  end type ColumnViewType

  interface
    subroutine process_column(column_data, params, rc)
      type(VirtualColumnType), intent(inout) :: column_data
    end subroutine
  end interface

end module
```

### Column Processing Loop

```fortran
! Automatic parallelization with batch processing
type(ColumnProcessorType) :: processor
type(VirtualColumnType), allocatable :: columns(:)

! Configure batch processing
call processor%set_batch_size(1000)
call processor%enable_openmp(.true.)

! Process columns in batches
!$OMP PARALLEL DO PRIVATE(column_data)
do i = 1, num_columns
  call extract_column(state_manager, i, columns(i), rc)
  call process_column(columns(i), params, rc)
  call update_column(state_manager, i, columns(i), rc)
end do
!$OMP END PARALLEL DO
```

## Implementation Guide

### 1. Column-Aware Process Design

```fortran
module MyProcess_Mod
  use ColumnInterface_Mod
  implicit none

  type :: MyProcessType
  contains
    procedure :: run_column => MyProcess_run_column
  end type

contains

  subroutine MyProcess_run_column(this, column_data, rc)
    class(MyProcessType), intent(inout) :: this
    type(ColumnDataType), intent(inout) :: column_data
    integer, intent(out) :: rc

    ! Process single column efficiently
    do k = 1, size(column_data%temperature)
      ! Column-local computation
    end do

  end subroutine

end module
```

### 2. Memory Layout Optimization

```fortran
! Optimize data layout for column access
type :: StateContainerType
  ! Column-major storage (Fortran default)
  real(fp), allocatable :: temperature(:,:)  ! (nlevels, ncolumns)
  real(fp), allocatable :: species(:,:,:)    ! (nlevels, nspecies, ncolumns)
end type
```

### 3. Chunked Processing

```fortran
! Process columns in chunks for better memory usage
integer, parameter :: CHUNK_SIZE = 1000

do chunk_start = 1, num_columns, CHUNK_SIZE
  chunk_end = min(chunk_start + CHUNK_SIZE - 1, num_columns)

  !$OMP PARALLEL DO
  do i = chunk_start, chunk_end
    call process_column(i, state_container, rc)
  end do
  !$OMP END PARALLEL DO
end do
```

## Advanced Features

### Dynamic Load Balancing

```fortran
! Adaptive work distribution
type :: LoadBalancer
  integer :: work_per_column(:)
  integer :: thread_assignments(:)
contains
  procedure :: balance_load
end type
```

### Column Dependencies

```fortran
! Handle column interactions
type :: ColumnDependencies
  integer :: neighbor_columns(:,:)
  logical :: requires_halo_exchange
contains
  procedure :: exchange_halos
end type
```

### Memory Pool Management

```fortran
! Efficient memory reuse
type :: ColumnMemoryPool
  type(ColumnDataType) :: pool(:)
  logical :: in_use(:)
contains
  procedure :: get_column
  procedure :: return_column
end type
```

## Debugging Column Processing

### Column-Level Diagnostics

```fortran
! Enable column diagnostics
type :: ColumnDiagnostics
  real(fp) :: min_values(:)
  real(fp) :: max_values(:)
  real(fp) :: mean_values(:)
contains
  procedure :: collect_stats
end type
```

### Validation Checks

```fortran
! Validate column data
subroutine validate_column(column_data, rc)
  type(ColumnDataType), intent(in) :: column_data
  integer, intent(out) :: rc

  ! Check for NaN/Inf values
  if (any(.not. ieee_is_finite(column_data%temperature))) then
    rc = -1
    return
  end if

  ! Check physical bounds
  if (any(column_data%temperature < 0.0_fp)) then
    rc = -2
    return
  end if

end subroutine
```

## Best Practices

### 1. Minimize Column Data Size

```fortran
! Keep column data compact
type :: EfficientColumnType
  real(fp) :: essential_fields(nlevels, nfields)
  ! Avoid unnecessary data
end type
```

### 2. Vectorization-Friendly Code

```fortran
! Write vectorizable loops
!DIR$ SIMD
do k = 1, nlevels
  settling_velocity(k) = compute_velocity(radius(k), density(k))
end do
```

### 3. Memory Access Patterns

```fortran
! Access data in column order (Fortran column-major)
do i = 1, ncolumns        ! Outer loop over columns
  do k = 1, nlevels       ! Inner loop over levels
    ! Process temperature(k, i)
  end do
end do
```

## Configuration

```yaml
# Column processing settings
architecture:
  column_processing:
    enabled: true
    chunk_size: 1000              # Columns per chunk
    max_memory_per_chunk: "512MB" # Memory limit

    # Threading
    num_threads: 8                # OpenMP threads
    thread_affinity: "close"      # Thread binding

    # Load balancing
    dynamic_balancing: true
    rebalance_frequency: 100      # Steps between rebalancing
```

## Troubleshooting

### Performance Issues

```yaml
# Debug column performance
debug:
  column_timing: true
  memory_usage: true
  load_balance: true
```

### Common Problems

- **Memory fragmentation**: Use memory pools
- **Load imbalance**: Enable dynamic balancing
- **Cache misses**: Optimize data layout
- **Thread contention**: Adjust thread count

## See Also

- [Process Architecture](../developer-guide/processes/index.md)
- [StateContainer Guide](statecontainer.md)

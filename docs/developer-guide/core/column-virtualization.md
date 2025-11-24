# Column Virtualization Developer Guide

CATChem's column virtualization system enables efficient atmospheric chemistry processing by treating 3D atmospheric problems as a collection of independent 1D columns. Everything you need to know about the column virtualization system to develop, modify, and extend CATChem is described in this guide.

## Overview

Column virtualization is a key performance optimization that:

- **Reduces memory pressure** by processing one column at a time
- **Enables efficient parallelization** through independent column processing
- **Improves cache performance** with contiguous memory access patterns
- **Simplifies physics implementations** with 1D vertical profiles

## Architecture

### 3D to 1D Transformation

```fortran
! Original 3D approach
real(fp) :: concentrations(nx, ny, nz, nspecies)

! Column virtualization approach
type :: ColumnDataType
  real(fp), allocatable :: concentrations(:, :)  ! (nz, nspecies)
  real(fp), allocatable :: temperature(:)        ! (nz)
  real(fp), allocatable :: pressure(:)           ! (nz)
  real(fp), allocatable :: density(:)            ! (nz)
end type ColumnDataType
```

### Column Processor

The column processor manages the transformation and iteration:

```fortran
module ColumnProcessor_Mod
  implicit none

  private
  integer :: nx, ny, nz, nspecies
  type(ColumnDataType) :: working_column

  procedure :: process_domain => process_3d_domain
  procedure :: extract_column => extract_1d_column
  procedure :: insert_column => insert_1d_column

contains

  subroutine process_3d_domain(this, domain_data, processes)
    class(ColumnProcessorType), intent(inout) :: this
    type(DomainDataType), intent(inout) :: domain_data
    type(ProcessListType), intent(inout) :: processes

    integer :: i, j

    ! Loop over all horizontal grid points
    do j = 1, this%ny
      do i = 1, this%nx
        ! Extract 1D column
        call this%extract_column(domain_data, i, j)

        ! Process column through all physics
        call run_column_processes(this%working_column, processes)

        ! Insert results back into 3D domain
        call this%insert_column(domain_data, i, j)
      end do
    end do
  end subroutine process_3d_domain

end module ColumnProcessor_Mod
```

## Benefits

### Performance Advantages

**Memory Efficiency**

- Working with small 1D arrays instead of large 3D arrays
- Better cache locality and reduced memory bandwidth requirements
- Lower memory footprint enables larger problem sizes

**Parallelization**

- Columns are independent and can be processed in parallel
- Simple OpenMP parallelization over horizontal grid points
- Scalable to many-core architectures

**Algorithm Simplicity**

- Physics algorithms work with familiar 1D vertical profiles
- Easier to develop and debug process implementations
- Natural mapping to atmospheric physics equations

### Code Example: Parallel Processing

```fortran
subroutine process_domain_parallel(this, domain_data, processes)
  class(ColumnProcessorType), intent(inout) :: this
  type(DomainDataType), intent(inout) :: domain_data
  type(ProcessListType), intent(inout) :: processes

  integer :: i, j, thread_id
  type(ColumnDataType) :: thread_columns(omp_get_max_threads())

  !$OMP PARALLEL DO PRIVATE(i, j, thread_id) SCHEDULE(DYNAMIC)
  do j = 1, this%ny
    do i = 1, this%nx
      thread_id = omp_get_thread_num() + 1

      ! Extract column data
      call extract_column(domain_data, i, j, thread_columns(thread_id))

      ! Process column
      call run_column_processes(thread_columns(thread_id), processes)

      ! Insert results
      call insert_column(domain_data, i, j, thread_columns(thread_id))
    end do
  end do
end subroutine process_domain_parallel
```

## Column Data Structure

### Core Column Type

```fortran
type :: ColumnDataType
  ! Vertical dimensions
  integer :: nz                              ! Number of vertical levels
  integer :: nspecies                        ! Number of chemical species

  ! Meteorological data
  real(fp), allocatable :: temperature(:)    ! Temperature (K)
  real(fp), allocatable :: pressure(:)       ! Pressure (Pa)
  real(fp), allocatable :: density(:)        ! Air density (kg/m³)
  real(fp), allocatable :: humidity(:)       ! Specific humidity (kg/kg)
  real(fp), allocatable :: height(:)         ! Height above surface (m)

  ! Chemical concentrations
  real(fp), allocatable :: concentrations(:,:) ! (nz, nspecies) mixing ratios
  real(fp), allocatable :: tendencies(:,:)     ! (nz, nspecies) time tendencies

  ! Physics-specific data
  real(fp), allocatable :: settling_velocity(:,:) ! (nz, nspecies)
  real(fp), allocatable :: deposition_velocity(:) ! (nspecies) surface values

  ! Diagnostic data
  real(fp), allocatable :: process_rates(:,:,:) ! (nz, nspecies, nprocesses)

  procedure :: initialize => column_initialize
  procedure :: cleanup => column_cleanup
  procedure :: copy_from_domain => extract_from_3d
  procedure :: copy_to_domain => insert_to_3d
end type ColumnDataType
```

### Column Operations

```fortran
! Extract column from 3D domain
subroutine extract_from_3d(this, domain, i, j)
  class(ColumnDataType), intent(inout) :: this
  type(DomainDataType), intent(in) :: domain
  integer, intent(in) :: i, j  ! Horizontal indices

  ! Copy meteorological data
  this%temperature(:) = domain%temperature(i, j, :)
  this%pressure(:) = domain%pressure(i, j, :)
  this%density(:) = domain%density(i, j, :)

  ! Copy chemical concentrations
  this%concentrations(:, :) = domain%concentrations(i, j, :, :)

  ! Initialize tendencies
  this%tendencies(:, :) = 0.0_fp
end subroutine extract_from_3d

! Insert column back into 3D domain
subroutine insert_to_3d(this, domain, i, j)
  class(ColumnDataType), intent(in) :: this
  type(DomainDataType), intent(inout) :: domain
  integer, intent(in) :: i, j  ! Horizontal indices

  ! Update concentrations with tendencies
  domain%concentrations(i, j, :, :) = this%concentrations(:, :)

  ! Store diagnostic data if requested
  if (allocated(domain%column_diagnostics)) then
    domain%column_diagnostics(i, j, :, :, :) = this%process_rates(:, :, :)
  end if
end subroutine insert_to_3d
```

## Process Integration

### Column-Aware Process Interface

Processes implement column-specific methods:

```fortran
type, extends(ProcessInterface) :: MyProcessType
  procedure :: run_column => my_process_column
end type MyProcessType

subroutine my_process_column(this, column_data, dt, rc)
  class(MyProcessType), intent(inout) :: this
  type(ColumnDataType), intent(inout) :: column_data
  real(fp), intent(in) :: dt
  integer, intent(out) :: rc

  integer :: k, ispec
  real(fp) :: process_rate

  ! Process each vertical level
  do k = 1, column_data%nz
    do ispec = 1, column_data%nspecies
      ! Calculate process rate for this level and species
      process_rate = calculate_rate(column_data%temperature(k), &
                                   column_data%pressure(k), &
                                   column_data%concentrations(k, ispec))

      ! Apply tendency
      column_data%tendencies(k, ispec) = column_data%tendencies(k, ispec) + &
                                        process_rate * dt

      ! Store diagnostic
      column_data%process_rates(k, ispec, this%process_id) = process_rate
    end do
  end do

  rc = 0
end subroutine my_process_column
```

## Implementation Guidelines

### Best Practices

1. **Minimize Data Copying**
   ```fortran
   ! Good: Work with column data directly
   settling_vel = calculate_settling(column%temperature, column%pressure)

   ! Avoid: Unnecessary temporary arrays
   temp_array = column%temperature
   settling_vel = calculate_settling(temp_array, column%pressure)
   ```

2. **Vectorize Column Operations**
   ```fortran
   ! Good: Vectorized operations
   column%tendencies(:, ispec) = column%tendencies(:, ispec) + &
                                rate_array(:) * dt

   ! Less efficient: Element-by-element loops
   do k = 1, nz
     column%tendencies(k, ispec) = column%tendencies(k, ispec) + &
                                  rate_array(k) * dt
   end do
   ```

3. **Handle Boundary Conditions**
   ```fortran
   ! Surface boundary condition
   column%deposition_velocity(ispec) = calculate_deposition_velocity(&
     column%temperature(1), column%pressure(1))

   ! Top boundary condition
   column%concentrations(nz, ispec) = apply_top_boundary(&
     column%concentrations(nz, ispec))
   ```

### Performance Considerations

- **Thread Safety**: Column data should be thread-local for parallel processing
- **Memory Layout**: Use contiguous arrays for better cache performance
- **Vectorization**: Write column operations to enable compiler vectorization
- **Load Balancing**: Use dynamic scheduling for varying computational loads

## Debugging and Validation

### Column-Level Diagnostics

```fortran
! Debug output for specific column
if (debug_column .and. i == debug_i .and. j == debug_j) then
  print *, 'Column (', i, ',', j, ') after process:', process_name
  print *, 'Temperature profile:', column%temperature
  print *, 'Concentration profile for O3:', column%concentrations(:, io3)
  print *, 'Process rates:', column%process_rates(:, io3, process_id)
end if
```

### Mass Conservation Checking

```fortran
subroutine check_column_conservation(column, species_names)
  type(ColumnDataType), intent(in) :: column
  character(len=*), intent(in) :: species_names(:)

  integer :: ispec
  real(fp) :: column_mass_before, column_mass_after

  do ispec = 1, column%nspecies
    column_mass_before = sum(column%concentrations(:, ispec) * &
                            column%density(:) * column%layer_thickness(:))

    ! Apply tendencies
    column_mass_after = sum((column%concentrations(:, ispec) + &
                            column%tendencies(:, ispec)) * &
                           column%density(:) * column%layer_thickness(:))

    if (abs(column_mass_after - column_mass_before) > conservation_tolerance) then
      print *, 'Mass conservation violation for ', species_names(ispec)
      print *, 'Before:', column_mass_before, 'After:', column_mass_after
    end if
  end do
end subroutine check_column_conservation
```

## Integration with Host Models

Column virtualization enables easy integration with different host model grid structures:

```fortran
! FV3 cubed-sphere grid
subroutine process_cubed_sphere_tile(tile_data, processes)
  type(CubedSphereTileType), intent(inout) :: tile_data
  type(ProcessListType), intent(inout) :: processes

  integer :: i, j
  type(ColumnDataType) :: column

  do j = tile_data%js, tile_data%je
    do i = tile_data%is, tile_data%ie
      call extract_cubed_sphere_column(tile_data, i, j, column)
      call run_column_processes(column, processes)
      call insert_cubed_sphere_column(tile_data, i, j, column)
    end do
  end do
end subroutine process_cubed_sphere_tile
```

This architecture provides a clean abstraction that allows the same atmospheric chemistry processes to work efficiently with different host model grid structures and parallelization strategies.

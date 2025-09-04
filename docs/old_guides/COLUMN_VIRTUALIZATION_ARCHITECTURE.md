# Column Virtualization Architecture

## Overview

This document describes the comprehensive column virtualization system implemented in CATChem that allows all processes to operate on virtual columns while maintaining full 3D spatial awareness through a sophisticated grid management system.

## Key Components

### 1. GridManager_Mod.F90
The central grid management system that provides:
- **Grid virtualization**: Processes see 1D columns, grid manager handles 3D
- **Unified grid interface** for 1D, 2D, and 3D models
- **Zero-copy column access** through smart pointers
- **Thread-safe parallelization** support (OpenMP/OpenACC)
- **Dynamic grid configuration** and decomposition

#### Key Types:
- `GridManagerType`: Main grid management class
- `GridGeometryType`: Grid geometry configuration
- `GridDecompositionType`: Parallel domain decomposition
- `ColumnIteratorType`: Iterator for processing all columns

### 2. Enhanced ColumnInterface_Mod.F90
Enhanced column interface providing:
- **VirtualColumnType**: Complete column abstraction for processes
- **ColumnProcessorType**: Batch processing of multiple columns
- **Column-to-grid mapping**: Seamless 1D-to-3D translation

#### Key Features:
- Processes only see 1D column data
- Grid manager handles all 3D complexity
- Zero-copy access through pointers
- Automatic data synchronization

### 3. Enhanced ProcessInterface_Mod.F90
Extended process interface supporting:
- **ColumnProcessInterface**: New base class for column-aware processes
- **Column processing methods**: `run_column()`, `init_column_processing()`
- **Batch processing support**: Process multiple columns efficiently

### 4. Enhanced state_mod.F90
State container integration:
- **GridManagerType** integration in `StateContainerType`
- **Grid manager accessors** for processes
- **Unified state management** with column virtualization

## Architecture Benefits

### 1. Process Independence
- **Processes see only 1D columns**: Simplified process development
- **Grid structure agnostic**: Same process works on 1D, 2D, 3D grids
- **No grid-specific code**: Process logic independent of spatial structure

### 2. Full 3D Spatial Awareness
- **Grid manager maintains 3D relationships**: Spatial interpolation, neighbor finding
- **Coordinate transformations**: Handle different grid projections
- **Distance calculations**: Geographic and Cartesian distances
- **Halo exchange**: Parallel processing support

### 3. Performance Optimization
- **Zero-copy access**: Pointers to actual grid data
- **Batch processing**: Process multiple columns efficiently
- **Parallel support**: Thread-safe column processing
- **Memory efficiency**: No data duplication

### 4. Flexibility
- **Multiple grid types**: Cartesian, geographic, projected coordinates
- **Dynamic grid configuration**: Runtime grid setup
- **Column iterator**: Flexible column processing patterns
- **Fallback support**: Traditional 3D processing when needed

## Usage Examples

### 1. Basic Column Processing

```fortran
type, extends(ColumnProcessInterface) :: MyProcessType
contains
   procedure :: run_column => my_run_column
end type

subroutine my_run_column(this, column, rc)
   class(MyProcessType), intent(inout) :: this
   type(VirtualColumnType), intent(inout) :: column
   integer, intent(out) :: rc

   integer :: k, nlev
   real(fp) :: temperature, concentration

   call column%get_metadata(lat, lon, area, nlev)

   do k = 1, nlev
      temperature = column%get_met_field('temperature', k)
      concentration = column%get_chem_field('CO', k)

      ! Process logic here - only sees 1D column data
      concentration = concentration * some_function(temperature)

      call column%set_chem_field('CO', k, concentration)
   enddo

   call column%apply_to_grid(rc)  ! Updates 3D grid automatically
end subroutine
```

### 2. Process with Grid Manager Integration

```fortran
subroutine my_process_run(this, container, rc)
   class(MyProcessType), intent(inout) :: this
   type(StateContainerType), intent(inout) :: container
   integer, intent(out) :: rc

   type(GridManagerType), pointer :: grid_mgr
   type(ColumnIteratorType) :: iterator
   type(VirtualColumnType) :: column

   ! Get grid manager - handles all 3D complexity
   grid_mgr => container%get_grid_manager_ptr()

   ! Create iterator for all columns
   iterator = grid_mgr%create_column_iterator()

   ! Process all columns - sees only 1D data
   do while (iterator%has_next())
      call iterator%next(rc)
      column = iterator%get_current_column()
      call this%run_column(column, rc)
   enddo
end subroutine
```

### 3. Batch Column Processing

```fortran
type(ColumnProcessorType) :: processor
type(VirtualColumnType) :: column
integer :: i, j

call processor%init(container, max_columns, rc)

! Add columns to processor
do j = 1, ny
   do i = 1, nx
      call processor%add_column(i, j, rc)
   enddo
enddo

! Process all columns in batch
call processor%process_all(my_column_process, rc)
```

## Grid Manager Configuration

### 1. Grid Geometry Setup

```fortran
type(GridManagerType) :: grid_mgr
type(ErrorManagerType) :: error_mgr

! Initialize for 3D Cartesian grid
call grid_mgr%init(nx=100, ny=100, nz=50, error_mgr, &
                   grid_type=GRID_TYPE_3D, &
                   coord_system=COORD_CARTESIAN, rc)

! Or for geographic grid
call grid_mgr%init(nx=360, ny=180, nz=50, error_mgr, &
                   grid_type=GRID_TYPE_3D, &
                   coord_system=COORD_LONLAT, rc)
```

### 2. Parallel Decomposition

```fortran
type(GridDecompositionType) :: decomp
type(GridGeometryType) :: geometry

! Initialize decomposition for MPI parallelism
call decomp%init(geometry, n_procs=8, my_rank=2, rc)

! Get local domain bounds
call decomp%get_local_bounds(i_start, i_end, j_start, j_end)
```

## Integration with Existing Code

### 1. Minimal Changes Required
- **Existing processes**: Can continue using traditional 3D interfaces
- **Gradual migration**: Processes can be updated incrementally
- **Backward compatibility**: All existing functionality preserved

### 2. Process Conversion Steps
1. Extend `ColumnProcessInterface` instead of `ProcessInterface`
2. Implement `run_column()` method
3. Use `VirtualColumnType` for column data access
4. Let grid manager handle 3D complexity

### 3. State Container Integration
- Grid manager automatically integrated in `StateContainerType`
- Access via `container%get_grid_manager_ptr()`
- No changes to existing state management

## Performance Considerations

### 1. Memory Usage
- **Zero-copy design**: No data duplication
- **Smart pointers**: Direct access to grid data
- **Efficient iteration**: Minimal overhead

### 2. Parallelization
- **Thread-safe column processing**: OpenMP/OpenACC compatible
- **Domain decomposition**: MPI parallel support
- **Load balancing**: Even distribution of columns

### 3. Scalability
- **Batch processing**: Efficient for large numbers of columns
- **Iterator pattern**: Memory-efficient column traversal
- **Lazy evaluation**: Column data loaded on demand

## Future Enhancements

### 1. Advanced Grid Operations
- **Grid refinement**: Adaptive mesh refinement support
- **Multi-grid**: Hierarchical grid structures
- **Nested grids**: Regional refinement capabilities

### 2. Enhanced Parallelization
- **GPU acceleration**: CUDA/OpenACC column processing
- **Hybrid parallelism**: MPI + OpenMP optimization
- **Load balancing**: Dynamic work distribution

### 3. Additional Coordinate Systems
- **Spherical coordinates**: Atmospheric modeling
- **Cylindrical coordinates**: Specialized applications
- **Custom projections**: User-defined coordinate systems

## Conclusion

The column virtualization system provides a powerful abstraction that:
- **Simplifies process development** by hiding grid complexity
- **Maintains full 3D spatial awareness** through the grid manager
- **Enables flexible grid configurations** without code changes
- **Optimizes performance** through zero-copy access and parallelization
- **Preserves backward compatibility** with existing processes

This architecture ensures that all processes operate on virtualized columns while the grid manager transparently handles all 3D spatial relationships, achieving the goal of full column virtualization at the process level while maintaining 3D spatial context.

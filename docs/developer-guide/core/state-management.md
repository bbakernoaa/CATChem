# State Management Developer Guide

CATChem's state management system provides a modern, flexible architecture for handling model state data through the StateContainer pattern with dependency injection. Everything you need to know about the state management system to develop, modify, and extend CATChem is described in this guide.

## Overview

The state management system replaces traditional global variables with a structured, maintainable approach:

- **StateContainer**: Central container for all model state
- **StateBuilder**: Flexible construction and initialization
- **Dependency Injection**: Components receive dependencies rather than accessing globals
- **Type Safety**: Strong typing prevents common errors
- **Memory Management**: Automatic cleanup and validation

## StateContainer Architecture

### Core Components

The `StateContainerType` encapsulates all model state:

```fortran
type :: StateContainerType
   private

   ! Core state objects
   type(ConfigDataType),    allocatable :: config
   type(ConfigManagerType), allocatable :: config_mgr
   type(MetStateType),     allocatable :: met_state
   type(ChemStateType),    allocatable :: chem_state
   type(EmisStateType),    allocatable :: emis_state
   type(DiagStateType),    allocatable :: diag_state

   ! Modern diagnostic system
   type(DiagnosticManagerType), allocatable :: diag_mgr

   ! Grid management
   type(GridManagerType), allocatable :: grid_mgr

   ! Error handling
   type(ErrorManagerType) :: error_mgr

   ! Container metadata
   logical :: is_initialized = .false.
   logical :: is_valid = .false.
   character(len=64) :: build_id = ''

contains
   ! Initialization and lifecycle
   procedure :: init => state_container_init
   procedure :: finalize => state_container_finalize
   procedure :: validate => state_container_validate

   ! State access methods
   procedure :: get_config_ptr
   procedure :: get_met_state_ptr
   procedure :: get_chem_state_ptr
   procedure :: get_emis_state_ptr
   procedure :: get_diagnostic_manager
   procedure :: get_error_manager
   procedure :: get_grid_manager

   ! Utility methods
   procedure :: is_ready
   procedure :: get_build_info
   procedure :: serialize_state
end type StateContainerType
```

### State Access Patterns

#### Pointer-Based Access

Get pointers to state components for efficient access:

```fortran
subroutine process_chemistry(container, rc)
   type(StateContainerType), intent(inout) :: container
   integer, intent(out) :: rc

   type(MetStateType), pointer :: met_state
   type(ChemStateType), pointer :: chem_state
   type(ErrorManagerType), pointer :: error_mgr

   ! Get state pointers
   met_state => container%get_met_state_ptr()
   chem_state => container%get_chem_state_ptr()
   error_mgr => container%get_error_manager()

   ! Access temperature field
   real(fp), pointer :: temperature(:,:,:)
   temperature => met_state%get_field('temperature')

   ! Access O3 concentrations
   real(fp), pointer :: o3_conc(:,:,:)
   o3_conc => chem_state%get_species_ptr('O3')

   ! Modify concentrations
   o3_conc = o3_conc * 1.1_fp  ! Example modification

   rc = CC_SUCCESS
end subroutine process_chemistry
```

#### Safe Access with Validation

Always validate state before use:

```fortran
subroutine safe_state_access(container, rc)
   type(StateContainerType), intent(inout) :: container
   integer, intent(out) :: rc

   type(ErrorManagerType), pointer :: error_mgr

   rc = CC_SUCCESS
   error_mgr => container%get_error_manager()

   ! Validate container state
   if (.not. container%is_ready()) then
      call error_mgr%report_error("StateContainer not ready")
      rc = CC_FAILURE
      return
   end if

   ! Validate specific state components
   if (.not. associated(container%get_met_state_ptr())) then
      call error_mgr%report_error("MetState not available")
      rc = CC_FAILURE
      return
   end if

   ! Proceed with operations...
end subroutine safe_state_access
```

## StateBuilder Pattern

### Flexible Construction

Use the StateBuilder for flexible state container construction:

```fortran
program example_state_building
   use state_mod

   type(StateBuilderType) :: builder
   type(StateContainerType) :: container
   integer :: rc

   ! Initialize builder
   call builder%init()

   ! Configure components
   call builder%with_config('config.yml')
   call builder%with_grid(100, 100, 50)
   call builder%with_species(['O3', 'NO', 'NO2'])
   call builder%with_diagnostics_enabled(.true.)

   ! Build container
   call builder%build(container, rc)
   if (rc /= CC_SUCCESS) then
      print *, "Failed to build state container"
      stop 1
   end if

   ! Use container
   call some_model_process(container, rc)

   ! Cleanup
   call container%finalize(rc)

end program example_state_building
```

### StateBuilder Configuration

```fortran
type :: StateBuilderType
   private

   character(len=256) :: config_file = ''
   integer :: nx = 0, ny = 0, nz = 0
   character(len=32), allocatable :: species_list(:)
   logical :: enable_diagnostics = .true.
   logical :: enable_emissions = .true.
   logical :: enable_column_virtualization = .true.

contains
   procedure :: init => builder_init
   procedure :: with_config => builder_with_config
   procedure :: with_grid => builder_with_grid
   procedure :: with_species => builder_with_species
   procedure :: with_diagnostics_enabled => builder_with_diagnostics
   procedure :: build => builder_build
   procedure :: finalize => builder_finalize
end type StateBuilderType
```

## State Component Details

### MetState - Meteorological State

Manages atmospheric conditions:

```fortran
type :: MetStateType
   private

   ! 3D meteorological fields
   real(fp), allocatable :: temperature(:,:,:)    ! K
   real(fp), allocatable :: pressure(:,:,:)       ! Pa
   real(fp), allocatable :: air_density(:,:,:)    ! kg/m³
   real(fp), allocatable :: u_wind(:,:,:)         ! m/s
   real(fp), allocatable :: v_wind(:,:,:)         ! m/s
   real(fp), allocatable :: w_wind(:,:,:)         ! m/s
   real(fp), allocatable :: humidity(:,:,:)       ! kg/kg

   ! 2D surface fields
   real(fp), allocatable :: surface_pressure(:,:) ! Pa
   real(fp), allocatable :: surface_temp(:,:)     ! K
   real(fp), allocatable :: land_fraction(:,:)    ! 0-1

   ! Field registry
   type(FieldRegistryType) :: field_registry

contains
   procedure :: init => met_state_init
   procedure :: get_field => met_state_get_field
   procedure :: set_field => met_state_set_field
   procedure :: register_field => met_state_register_field
   procedure :: finalize => met_state_finalize
end type MetStateType
```

### ChemState - Chemical State

Manages species concentrations:

```fortran
type :: ChemStateType
   private

   ! Species concentrations [mol/m³] or [μg/m³]
   real(fp), allocatable :: concentrations(:,:,:,:)  ! (nx,ny,nz,nspec)

   ! Species metadata
   type(SpeciesManagerType) :: species_mgr
   integer :: n_species = 0
   character(len=32), allocatable :: species_names(:)
   real(fp), allocatable :: molar_masses(:)           ! g/mol

   ! Unit conversion factors
   real(fp), allocatable :: molar_to_mass(:)
   real(fp), allocatable :: mass_to_molar(:)

contains
   procedure :: init => chem_state_init
   procedure :: get_species_ptr => chem_state_get_species_ptr
   procedure :: get_species_index => chem_state_get_species_index
   procedure :: add_species => chem_state_add_species
   procedure :: convert_units => chem_state_convert_units
   procedure :: finalize => chem_state_finalize
end type ChemStateType
```

### EmisState - Emission State

Manages emission fluxes:

```fortran
type :: EmisStateType
   private

   ! Emission fluxes [mol/m²/s] or [kg/m²/s]
   real(fp), allocatable :: surface_emissions(:,:,:)  ! (nx,ny,nspec)
   real(fp), allocatable :: volume_emissions(:,:,:,:) ! (nx,ny,nz,nspec)

   ! Emission metadata
   type(EmissionManagerType) :: emis_mgr
   logical :: has_surface_emissions = .false.
   logical :: has_volume_emissions = .false.

contains
   procedure :: init => emis_state_init
   procedure :: get_surface_flux => emis_state_get_surface_flux
   procedure :: get_volume_flux => emis_state_get_volume_flux
   procedure :: set_emissions => emis_state_set_emissions
   procedure :: finalize => emis_state_finalize
end type EmisStateType
```

## Advanced State Management

### State Validation

Comprehensive validation ensures state consistency:

```fortran
subroutine state_container_validate(this, rc)
   class(StateContainerType), intent(inout) :: this
   integer, intent(out) :: rc

   type(ErrorManagerType), pointer :: error_mgr
   integer :: local_rc

   rc = CC_SUCCESS
   error_mgr => this%get_error_manager()

   call error_mgr%push_context("state_container_validate")

   ! Validate initialization
   if (.not. this%is_initialized) then
      call error_mgr%report_error("StateContainer not initialized")
      rc = CC_FAILURE
      call error_mgr%pop_context()
      return
   end if

   ! Validate core components
   if (.not. allocated(this%config)) then
      call error_mgr%report_error("Configuration not allocated")
      rc = CC_FAILURE
   end if

   if (.not. allocated(this%met_state)) then
      call error_mgr%report_error("MetState not allocated")
      rc = CC_FAILURE
   end if

   if (.not. allocated(this%chem_state)) then
      call error_mgr%report_error("ChemState not allocated")
      rc = CC_FAILURE
   end if

   ! Validate component consistency
   call this%validate_grid_consistency(local_rc)
   if (local_rc /= CC_SUCCESS) then
      call error_mgr%report_error("Grid consistency validation failed")
      rc = local_rc
   end if

   call this%validate_species_consistency(local_rc)
   if (local_rc /= CC_SUCCESS) then
      call error_mgr%report_error("Species consistency validation failed")
      rc = local_rc
   end if

   ! Set validation status
   this%is_valid = (rc == CC_SUCCESS)

   call error_mgr%pop_context()

end subroutine state_container_validate
```

### Memory Management

Automatic memory management with cleanup:

```fortran
subroutine state_container_finalize(this, rc)
   class(StateContainerType), intent(inout) :: this
   integer, intent(out) :: rc

   integer :: local_rc

   rc = CC_SUCCESS

   ! Finalize state components in reverse order
   if (allocated(this%diag_mgr)) then
      call this%diag_mgr%finalize(local_rc)
      deallocate(this%diag_mgr)
   end if

   if (allocated(this%grid_mgr)) then
      call this%grid_mgr%finalize(local_rc)
      deallocate(this%grid_mgr)
   end if

   if (allocated(this%emis_state)) then
      call this%emis_state%finalize(local_rc)
      deallocate(this%emis_state)
   end if

   if (allocated(this%chem_state)) then
      call this%chem_state%finalize(local_rc)
      deallocate(this%chem_state)
   end if

   if (allocated(this%met_state)) then
      call this%met_state%finalize(local_rc)
      deallocate(this%met_state)
   end if

   if (allocated(this%config)) then
      call this%config%finalize(local_rc)
      deallocate(this%config)
   end if

   ! Reset container state
   this%is_initialized = .false.
   this%is_valid = .false.
   this%build_id = ''

end subroutine state_container_finalize
```

### Thread Safety

For parallel applications, use thread-safe access patterns:

```fortran
!$OMP PARALLEL DO PRIVATE(local_rc)
do i = 1, n_columns
   ! Each thread gets its own column data
   type(VirtualColumnType) :: column

   ! Extract column from container (thread-safe)
   call container%get_column(i, column, local_rc)

   ! Process column independently
   call process_column(column, local_rc)

   ! Update container (thread-safe)
   call container%set_column(i, column, local_rc)
end do
!$OMP END PARALLEL DO
```

## Integration with Processes

### Process-State Interaction

Processes interact with state through standardized interfaces:

```fortran
subroutine settling_process_run(this, container, rc)
   class(settlingProcessType), intent(inout) :: this
   type(StateContainerType), intent(inout) :: container
   integer, intent(out) :: rc

   type(MetStateType), pointer :: met_state
   type(ChemStateType), pointer :: chem_state
   type(DiagnosticManagerType), pointer :: diag_mgr
   real(fp), pointer :: temperature(:,:,:)
   real(fp), pointer :: pm25_conc(:,:,:)
   real(fp), allocatable :: settling_velocity(:,:,:)

   ! Get state components
   met_state => container%get_met_state_ptr()
   chem_state => container%get_chem_state_ptr()
   diag_mgr => container%get_diagnostic_manager()

   ! Get required fields
   temperature => met_state%get_field('temperature')
   pm25_conc => chem_state%get_species_ptr('PM25')

   ! Allocate work arrays
   allocate(settling_velocity(size(temperature,1), &
                             size(temperature,2), &
                             size(temperature,3)))

   ! Calculate settling
   call calculate_settling_velocity(temperature, pm25_conc, settling_velocity)

   ! Apply settling
   call apply_settling(pm25_conc, settling_velocity, container%dt)

   ! Update diagnostics
   call diag_mgr%update_field('settling_velocity', settling_velocity)

   deallocate(settling_velocity)
   rc = CC_SUCCESS

end subroutine settling_process_run
```

## Best Practices

### State Access Guidelines

1. **Always validate**: Check container state before access
2. **Use pointers efficiently**: Get pointers once, reuse them
3. **Handle errors gracefully**: Check return codes and report errors
4. **Clean up resources**: Deallocate temporary arrays
5. **Thread-safe patterns**: Use appropriate synchronization for parallel code

### Memory Management

1. **RAII pattern**: Initialize in constructors, cleanup in destructors
2. **Avoid leaks**: Always deallocate what you allocate
3. **Pointer safety**: Check association before dereferencing
4. **Automatic cleanup**: Use StateBuilder for automatic resource management

### Performance Optimization

1. **Minimize allocations**: Reuse work arrays where possible
2. **Efficient access patterns**: Use contiguous memory access
3. **Lazy initialization**: Only allocate what you need
4. **Profile memory usage**: Monitor for excessive allocations

### Error Handling

1. **Comprehensive validation**: Validate all state components
2. **Informative errors**: Provide context in error messages
3. **Graceful degradation**: Handle partial failures appropriately
4. **Error propagation**: Always check and propagate return codes

The StateContainer system provides a robust foundation for CATChem's modular architecture while maintaining high performance and reliability.

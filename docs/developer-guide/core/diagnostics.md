# Diagnostic System Developer Guide

CATChem's diagnostic system provides comprehensive monitoring, analysis, and output capabilities for atmospheric chemistry simulations. Everything you need to know about the diagnostics system to develop, modify, and extend CATChem is described in this guide.

## Overview

The diagnostic system enables:

- **Flexible Output Configuration** - User-defined diagnostic outputs
- **Process-Level Monitoring** - Track individual process contributions
- **Multi-Format Output** - NetCDF, HDF5, and custom formats
- **Performance Monitoring** - Runtime performance diagnostics
- **Real-Time Analysis** - Live monitoring and analysis capabilities

## Architecture

### Diagnostic Manager

```fortran
module DiagnosticManager_Mod
  use precision_mod
  use netcdf_mod
  implicit none
  private

  type :: DiagnosticFieldType
    character(len=64) :: name
    character(len=128) :: description
    character(len=32) :: units
    character(len=16) :: frequency  ! 'timestep', 'hourly', 'daily'
    integer :: dimensions(4)        ! (nx, ny, nz, nt)
    real(fp), allocatable :: data(:,:,:,:)
    logical :: enabled = .false.
  end type DiagnosticFieldType

  type, public :: DiagnosticManagerType
    private
    type(DiagnosticFieldType), allocatable :: fields(:)
    integer :: num_fields = 0
    character(len=256) :: output_file
    logical :: is_initialized = .false.

    ! Output configuration
    logical :: compress_output = .true.
    integer :: output_precision = 4  ! bytes

  contains
    procedure, public :: init => diagnostic_manager_init
    procedure, public :: register_field => register_diagnostic_field
    procedure, public :: update_field => update_diagnostic_field
    procedure, public :: write_output => write_diagnostic_output
    procedure, public :: finalize => diagnostic_manager_finalize

    ! Field access methods
    procedure, public :: get_field_ptr => get_diagnostic_field_ptr
    procedure, public :: get_field_index => get_diagnostic_field_index
    procedure, public :: is_field_enabled => is_diagnostic_field_enabled
  end type DiagnosticManagerType

contains

  subroutine diagnostic_manager_init(this, config_mgr, rc)
    class(DiagnosticManagerType), intent(inout) :: this
    type(ConfigManagerType), intent(in) :: config_mgr
    integer, intent(out) :: rc

    ! Load diagnostic configuration
    call load_diagnostic_config(this, config_mgr, rc)
    if (rc /= 0) return

    ! Initialize output file
    call init_diagnostic_output(this, rc)

    this%is_initialized = .true.
  end subroutine diagnostic_manager_init

end module DiagnosticManager_Mod
```

## Configuration

### Diagnostic Configuration (`diagnostics.yml`)

```yaml
# Diagnostic System Configuration
output:
  file: "diagnostics.nc"
  format: netcdf4
  compression: true
  precision: float32

  # Global attributes
  attributes:
    title: "CATChem Diagnostic Output"
    institution: "Your Institution"
    contact: "user@institution.edu"

# Field definitions
fields:
  # Concentration fields
  - name: "O3_concentration"
    description: "Ozone mixing ratio"
    units: "mol/mol"
    frequency: "hourly"
    dimensions: [nx, ny, nz, time]
    enabled: true

  - name: "NO2_concentration"
    description: "Nitrogen dioxide mixing ratio"
    units: "mol/mol"
    frequency: "hourly"
    dimensions: [nx, ny, nz, time]
    enabled: true

  # Process rate fields
  - name: "chemistry_O3_rate"
    description: "Chemical production/loss rate of O3"
    units: "mol/mol/s"
    frequency: "hourly"
    dimensions: [nx, ny, nz, time]
    enabled: false  # Disabled by default (large output)

  - name: "settling_PM25_rate"
    description: "Settling rate for PM2.5"
    units: "mol/mol/s"
    frequency: "hourly"
    dimensions: [nx, ny, nz, time]
    enabled: true

  # Integrated diagnostics
  - name: "column_O3"
    description: "Column-integrated ozone"
    units: "DU"
    frequency: "hourly"
    dimensions: [nx, ny, time]
    enabled: true

  - name: "surface_deposition_flux"
    description: "Surface deposition flux"
    units: "kg/m2/s"
    frequency: "hourly"
    dimensions: [nx, ny, nspecies, time]
    enabled: true

# Process-specific diagnostics
processes:
  chemistry:
    enabled: true
    fields:
      - reaction_rates
      - photolysis_rates
      - equilibrium_constants

  settling:
    enabled: true
    fields:
      - settling_velocity
      - particle_flux

  dry_deposition:
    enabled: true
    fields:
      - deposition_velocity
      - resistance_components

# Performance diagnostics
performance:
  enabled: true
  frequency: "timestep"
  fields:
    - process_timing
    - memory_usage
    - convergence_metrics
```

## Field Registration and Management

### Field Registration

```fortran
subroutine register_process_diagnostics(diag_mgr, process_name)
  type(DiagnosticManagerType), intent(inout) :: diag_mgr
  character(len=*), intent(in) :: process_name

  integer :: rc

  select case (trim(process_name))
  case ('chemistry')
    ! Register chemistry diagnostics
    call diag_mgr%register_field('chemistry_O3_rate', &
                                'O3 chemical production rate', &
                                'mol/mol/s', &
                                dimensions=[nx, ny, nz, nt], &
                                frequency='hourly', rc=rc)

    call diag_mgr%register_field('photolysis_rates', &
                                'Photolysis reaction rates', &
                                's^-1', &
                                dimensions=[nx, ny, nz, nreactions, nt], &
                                frequency='hourly', rc=rc)

  case ('settling')
    ! Register settling diagnostics
    call diag_mgr%register_field('settling_velocity', &
                                'Particle settling velocity', &
                                'm/s', &
                                dimensions=[nx, ny, nz, nspecies, nt], &
                                frequency='hourly', rc=rc)

  case ('dry_deposition')
    ! Register dry deposition diagnostics
    call diag_mgr%register_field('deposition_velocity', &
                                'Dry deposition velocity', &
                                'm/s', &
                                dimensions=[nx, ny, nspecies, nt], &
                                frequency='hourly', rc=rc)
  end select
end subroutine register_process_diagnostics
```

### Field Updates

```fortran
! Update diagnostic field during process execution
subroutine update_chemistry_diagnostics(diag_mgr, chemistry_data)
  type(DiagnosticManagerType), intent(inout) :: diag_mgr
  type(ChemistryDataType), intent(in) :: chemistry_data

  integer :: rc

  ! Update reaction rates
  if (diag_mgr%is_field_enabled('chemistry_O3_rate')) then
    call diag_mgr%update_field('chemistry_O3_rate', &
                              chemistry_data%o3_production_rate, rc)
  end if

  ! Update photolysis rates
  if (diag_mgr%is_field_enabled('photolysis_rates')) then
    call diag_mgr%update_field('photolysis_rates', &
                              chemistry_data%photolysis_rates, rc)
  end if
end subroutine update_chemistry_diagnostics
```

## Output Formats

### NetCDF Output

```fortran
subroutine write_netcdf_output(diag_mgr, timestep, rc)
  type(DiagnosticManagerType), intent(inout) :: diag_mgr
  integer, intent(in) :: timestep
  integer, intent(out) :: rc

  integer :: ncid, varid, dimids(4)
  integer :: i, field_idx
  real(fp), allocatable :: output_data(:,:,:,:)

  ! Open NetCDF file
  call nf90_open(diag_mgr%output_file, NF90_WRITE, ncid)

  ! Write each enabled field
  do i = 1, diag_mgr%num_fields
    if (.not. diag_mgr%fields(i)%enabled) cycle

    ! Get variable ID
    call nf90_inq_varid(ncid, diag_mgr%fields(i)%name, varid)

    ! Write data with appropriate slicing
    select case (size(diag_mgr%fields(i)%dimensions))
    case (3)  ! 2D field + time
      call nf90_put_var(ncid, varid, &
                        diag_mgr%fields(i)%data(:,:,1,1), &
                        start=[1,1,timestep])
    case (4)  ! 3D field + time
      call nf90_put_var(ncid, varid, &
                        diag_mgr%fields(i)%data(:,:,:,1), &
                        start=[1,1,1,timestep])
    end select
  end do

  ! Write time coordinate
  call nf90_inq_varid(ncid, 'time', varid)
  call nf90_put_var(ncid, varid, real(timestep), start=[timestep])

  call nf90_close(ncid)
  rc = 0
end subroutine write_netcdf_output
```

### Custom Output Formats

```fortran
! Binary output for high-performance applications
subroutine write_binary_output(diag_mgr, timestep, rc)
  type(DiagnosticManagerType), intent(inout) :: diag_mgr
  integer, intent(in) :: timestep
  integer, intent(out) :: rc

  integer :: unit, i
  character(len=256) :: filename

  write(filename, '(A,I0.8,A)') 'diagnostics_', timestep, '.bin'

  open(newunit=unit, file=filename, form='unformatted', access='stream')

  ! Write header
  write(unit) diag_mgr%num_fields, timestep

  ! Write field data
  do i = 1, diag_mgr%num_fields
    if (.not. diag_mgr%fields(i)%enabled) cycle

    write(unit) diag_mgr%fields(i)%name
    write(unit) diag_mgr%fields(i)%dimensions
    write(unit) diag_mgr%fields(i)%data
  end do

  close(unit)
  rc = 0
end subroutine write_binary_output
```

## Advanced Features

### Conditional Diagnostics

```fortran
! Enable diagnostics based on runtime conditions
subroutine enable_conditional_diagnostics(diag_mgr, state_container)
  type(DiagnosticManagerType), intent(inout) :: diag_mgr
  type(StateContainerType), intent(in) :: state_container

  real(fp) :: max_o3_concentration

  ! Enable detailed chemistry diagnostics if O3 is high
  max_o3_concentration = maxval(state_container%get_species_concentration('O3'))

  if (max_o3_concentration > 120.0e-9) then  ! 120 ppb
    call diag_mgr%enable_field('chemistry_O3_rate')
    call diag_mgr%enable_field('photolysis_rates')
    call diag_mgr%enable_field('reaction_rates')
  end if
end subroutine enable_conditional_diagnostics
```

### Derived Diagnostics

```fortran
! Calculate derived quantities
subroutine calculate_derived_diagnostics(diag_mgr, state_container)
  type(DiagnosticManagerType), intent(inout) :: diag_mgr
  type(StateContainerType), intent(in) :: state_container

  real(fp), allocatable :: column_o3(:,:)
  real(fp), allocatable :: o3_conc(:,:,:)
  real(fp), allocatable :: pressure(:,:,:)
  integer :: i, j, k

  ! Calculate column-integrated ozone
  o3_conc = state_container%get_species_concentration('O3')
  pressure = state_container%get_met_field('pressure')

  allocate(column_o3(size(o3_conc,1), size(o3_conc,2)))

  do j = 1, size(o3_conc, 2)
    do i = 1, size(o3_conc, 1)
      column_o3(i,j) = 0.0
      do k = 1, size(o3_conc, 3)
        ! Convert to Dobson units
        column_o3(i,j) = column_o3(i,j) + o3_conc(i,j,k) * &
                        (pressure(i,j,k) - pressure(i,j,k+1)) / gravity * &
                        dobson_unit_conversion
      end do
    end do
  end do

  ! Update diagnostic field
  call diag_mgr%update_field('column_O3', column_o3)
end subroutine calculate_derived_diagnostics
```

### Performance Diagnostics

```fortran
module PerformanceDiagnostics_Mod
  use precision_mod
  implicit none

  type :: ProcessTimingType
    character(len=32) :: process_name
    real(fp) :: total_time = 0.0
    real(fp) :: min_time = huge(1.0_fp)
    real(fp) :: max_time = 0.0
    integer :: call_count = 0
  end type ProcessTimingType

  type :: PerformanceMonitorType
    type(ProcessTimingType), allocatable :: process_timings(:)
    integer :: num_processes = 0
    real(fp) :: total_simulation_time = 0.0
  contains
    procedure :: start_timer
    procedure :: stop_timer
    procedure :: get_timing_summary
  end type PerformanceMonitorType

contains

  subroutine start_timer(this, process_name, timer_id)
    class(PerformanceMonitorType), intent(inout) :: this
    character(len=*), intent(in) :: process_name
    integer, intent(out) :: timer_id

    ! Find or create process timing entry
    timer_id = find_process_index(this, process_name)
    if (timer_id == 0) then
      call add_process_timer(this, process_name, timer_id)
    end if

    ! Record start time
    call cpu_time(this%process_timings(timer_id)%start_time)
  end subroutine start_timer

  subroutine stop_timer(this, timer_id)
    class(PerformanceMonitorType), intent(inout) :: this
    integer, intent(in) :: timer_id

    real(fp) :: end_time, elapsed_time

    call cpu_time(end_time)
    elapsed_time = end_time - this%process_timings(timer_id)%start_time

    ! Update timing statistics
    associate(timing => this%process_timings(timer_id))
      timing%total_time = timing%total_time + elapsed_time
      timing%min_time = min(timing%min_time, elapsed_time)
      timing%max_time = max(timing%max_time, elapsed_time)
      timing%call_count = timing%call_count + 1
    end associate
  end subroutine stop_timer

end module PerformanceDiagnostics_Mod
```

## Integration with Processes

### Process-Level Diagnostics

```fortran
! Example: Settling process with diagnostics
subroutine settling_run_with_diagnostics(this, container, rc)
  class(SettlingProcessType), intent(inout) :: this
  type(StateContainerType), intent(inout) :: container
  integer, intent(out) :: rc

  type(DiagnosticManagerType), pointer :: diag_mgr
  real(fp), allocatable :: settling_velocity(:,:,:,:)
  integer :: timer_id

  ! Get diagnostic manager
  diag_mgr => container%get_diagnostic_manager_ptr()

  ! Start performance timer
  call diag_mgr%start_timer('settling', timer_id)

  ! Allocate diagnostic arrays
  allocate(settling_velocity(nx, ny, nz, nspecies))

  ! Perform settling calculations
  call calculate_settling_velocities(this, container, settling_velocity)
  call apply_settling(container, settling_velocity)

  ! Update diagnostics
  if (diag_mgr%is_field_enabled('settling_velocity')) then
    call diag_mgr%update_field('settling_velocity', settling_velocity, rc)
  end if

  ! Stop performance timer
  call diag_mgr%stop_timer(timer_id)

  rc = 0
end subroutine settling_run_with_diagnostics
```

## Best Practices

### 1. Efficient Memory Management
```fortran
! Use pointers for large diagnostic arrays
type(DiagnosticManagerType) :: diag_mgr
real(fp), pointer :: large_array(:,:,:,:)

! Only allocate when field is enabled
if (diag_mgr%is_field_enabled('large_diagnostic')) then
  allocate(large_array(nx, ny, nz, nspecies))
  ! ... calculations ...
  call diag_mgr%update_field('large_diagnostic', large_array)
  deallocate(large_array)
end if
```

### 2. Selective Output
```fortran
! Enable expensive diagnostics only when needed
if (debug_mode .or. detailed_analysis_requested) then
  call diag_mgr%enable_field('expensive_diagnostic')
end if
```

### 3. Output Optimization
```yaml
# Compress large diagnostic fields
fields:
  - name: "detailed_chemistry_rates"
    compression: true
    precision: float32  # Reduce precision for large fields
    enabled: false      # Disable by default
```

This diagnostic system provides comprehensive monitoring capabilities while maintaining performance and flexibility for various analysis needs.

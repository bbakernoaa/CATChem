# Process-Level Diagnostics with CATChem High-Level API

## Overview

The CATChem high-level API provides comprehensive access to process-specific diagnostic data through the modern diagnostic system. Each process (dust, sea salt, external emissions, etc.) can register its own diagnostic fields that are automatically managed and accessible to users.

## Key Features

### 🔍 **Diagnostic Discovery**
- List all available processes with diagnostics
- Discover diagnostic fields for each process
- Get field metadata (units, description, dimensions)

### 📊 **Flexible Data Access**
- Access individual diagnostic fields by name
- Get all diagnostics for a process at once
- Support for 2D, 3D, and scalar diagnostic data

### ⏱️ **Real-time Monitoring**
- Access diagnostics during simulation
- Monitor diagnostic evolution over time
- Validate process behavior

## API Methods

### Diagnostic Discovery

```fortran
! List all processes with available diagnostics
call catchem%list_process_diagnostics(process_names, diagnostic_info, rc)

! Get available diagnostic fields for a specific process
call catchem%get_available_diagnostics('Dust', diagnostic_names, rc)
```

### Individual Diagnostic Access

```fortran
type(CATChemDiagnosticType) :: diagnostic

! Get a specific diagnostic field
call catchem%get_process_diagnostic('Dust', 'surface_emission_flux', diagnostic, rc)

if (diagnostic%is_available) then
   write(*,*) 'Field: ', trim(diagnostic%field_name)
   write(*,*) 'Units: ', trim(diagnostic%units)
   write(*,*) 'Description: ', trim(diagnostic%description)

   ! Access data based on dimensions
   select case (diagnostic%dimensions)
   case (2)
      ! 2D data (e.g., surface fields)
      write(*,*) 'Max value:', maxval(diagnostic%data_2d)
   case (3)
      ! 3D data (e.g., column fields)
      write(*,*) 'Max value:', maxval(diagnostic%data_3d)
   case (0)
      ! Scalar data (e.g., global totals)
      write(*,*) 'Value:', diagnostic%scalar_data
   end select
endif
```

### Bulk Diagnostic Access

```fortran
type(CATChemDiagnosticType), allocatable :: all_diagnostics(:)

! Get all diagnostics for a process
call catchem%get_all_process_diagnostics('ExternalEmission', all_diagnostics, rc)

do i = 1, size(all_diagnostics)
   if (all_diagnostics(i)%is_available) then
      write(*,*) 'Processing field:', trim(all_diagnostics(i)%field_name)
      ! Process diagnostic data...
   endif
end do
```

## Process-Specific Diagnostics

### Dust Process Diagnostics

```fortran
! Common dust diagnostics
call catchem%get_process_diagnostic('Dust', 'surface_emission_flux', diagnostic, rc)     ! [kg/m²/s]
call catchem%get_process_diagnostic('Dust', 'total_column_emission', diagnostic, rc)    ! [kg/m²/s]
call catchem%get_process_diagnostic('Dust', 'threshold_velocity', diagnostic, rc)       ! [m/s]
call catchem%get_process_diagnostic('Dust', 'friction_velocity', diagnostic, rc)        ! [m/s]
call catchem%get_process_diagnostic('Dust', 'saltation_flux', diagnostic, rc)           ! [kg/m²/s]
```

### Sea Salt Process Diagnostics

```fortran
! Common sea salt diagnostics
call catchem%get_process_diagnostic('SeaSalt', 'surface_emission_flux', diagnostic, rc)  ! [kg/m²/s]
call catchem%get_process_diagnostic('SeaSalt', 'whitecap_fraction', diagnostic, rc)      ! [dimensionless]
call catchem%get_process_diagnostic('SeaSalt', 'wind_speed_10m', diagnostic, rc)         ! [m/s]
call catchem%get_process_diagnostic('SeaSalt', 'sea_surface_temperature', diagnostic, rc)! [K]
```

### External Emission Process Diagnostics

```fortran
! Common external emission diagnostics
call catchem%get_process_diagnostic('ExternalEmission', 'applied_emission_rate', diagnostic, rc)    ! [mol/m²/s]
call catchem%get_process_diagnostic('ExternalEmission', 'species_tendency', diagnostic, rc)         ! [mol/mol/s]
call catchem%get_process_diagnostic('ExternalEmission', 'total_emissions_applied', diagnostic, rc)  ! [mol/m²/s]
call catchem%get_process_diagnostic('ExternalEmission', 'config_validation_status', diagnostic, rc) ! [dimensionless]
```

### Dry Deposition Process Diagnostics

```fortran
! Common dry deposition diagnostics
call catchem%get_process_diagnostic('DryDeposition', 'deposition_velocity', diagnostic, rc)        ! [m/s]
call catchem%get_process_diagnostic('DryDeposition', 'aerodynamic_resistance', diagnostic, rc)     ! [s/m]
call catchem%get_process_diagnostic('DryDeposition', 'surface_resistance', diagnostic, rc)         ! [s/m]
call catchem%get_process_diagnostic('DryDeposition', 'deposition_flux', diagnostic, rc)            ! [mol/m²/s]
```

## Common Usage Patterns

### 1. Real-time Monitoring During Simulation

```fortran
do step = 1, n_timesteps
   ! Run simulation step
   call catchem%run_timestep(input_data(step), output_data(step), rc)

   ! Monitor key diagnostics
   call catchem%get_process_diagnostic('Dust', 'surface_emission_flux', diagnostic, rc)
   if (diagnostic%is_available .and. allocated(diagnostic%data_2d)) then
      dust_total = sum(diagnostic%data_2d)
      write(*,'(A,I0,A,E12.4)') 'Step ', step, ': Total dust emission = ', dust_total
   endif

   ! Check for process issues
   call catchem%get_process_diagnostic('ExternalEmission', 'config_validation_status', diagnostic, rc)
   if (diagnostic%is_available .and. diagnostic%scalar_data < 0.0_fp) then
      write(*,*) 'WARNING: External emission configuration issue detected'
   endif
end do
```

### 2. Post-simulation Analysis

```fortran
! Run full simulation
call catchem%run_multiple_steps(n_steps, input_data, output_data, rc)

! Analyze final state
call catchem%get_all_process_diagnostics('Dust', dust_diagnostics, rc)
call catchem%get_all_process_diagnostics('SeaSalt', seasalt_diagnostics, rc)

! Generate summary report
call generate_process_summary_report(dust_diagnostics, seasalt_diagnostics)
```

### 3. Process Validation and Debugging

```fortran
! Enable detailed diagnostics for debugging
config%enable_diagnostics = .true.

call catchem%init(config, rc)
call catchem%add_process('Dust', rc=rc)

! Check process initialization
call catchem%get_process_diagnostic('Dust', 'initialization_status', diagnostic, rc)
if (diagnostic%scalar_data /= 1.0_fp) then
   write(*,*) 'ERROR: Dust process initialization failed'
endif

! Run single step with detailed monitoring
call catchem%run_timestep(input_data, output_data, rc)

! Validate process outputs
call validate_dust_process_outputs(catchem)
```

### 4. Custom Diagnostic Processing

```fortran
subroutine process_dust_size_distribution(catchem)
   type(CATChemInstanceType), intent(inout) :: catchem
   type(CATChemDiagnosticType) :: diagnostic
   character(len=64) :: bin_names(5) = ['bin1_emission', 'bin2_emission', 'bin3_emission', &
                                       'bin4_emission', 'bin5_emission']
   real(fp) :: total_emission, bin_fraction
   integer :: i, rc

   total_emission = 0.0_fp

   ! Get emission for each size bin
   do i = 1, 5
      call catchem%get_process_diagnostic('Dust', bin_names(i), diagnostic, rc)
      if (diagnostic%is_available .and. allocated(diagnostic%data_2d)) then
         bin_fraction = sum(diagnostic%data_2d)
         total_emission = total_emission + bin_fraction
         write(*,'(A,I0,A,F8.2,A)') 'Dust bin ', i, ': ', bin_fraction*100.0_fp, '% of total'
      endif
   end do

   write(*,'(A,E12.4,A)') 'Total dust emission: ', total_emission, ' kg/m²/s'

end subroutine
```

## Data Types and Structures

### CATChemDiagnosticType

```fortran
type :: CATChemDiagnosticType
   character(len=64) :: field_name          ! Diagnostic field name
   character(len=128) :: description        ! Field description
   character(len=32) :: units              ! Field units
   character(len=32) :: process_name       ! Source process name
   integer :: dimensions                   ! Number of dimensions (0, 2, or 3)
   logical :: is_available                 ! Whether field is available
   real(fp), allocatable :: data_2d(:,:)   ! 2D diagnostic data (nx, ny)
   real(fp), allocatable :: data_3d(:,:,:) ! 3D diagnostic data (nx, ny, nz)
   real(fp) :: scalar_data                 ! Scalar diagnostic data
end type
```

## Error Handling

```fortran
call catchem%get_process_diagnostic('NonExistentProcess', 'some_field', diagnostic, rc)
if (rc /= CATCHEM_SUCCESS) then
   call catchem%get_error_message(error_msg)
   write(*,'(A,A)') 'Diagnostic access failed: ', trim(error_msg)
endif

! Always check if diagnostic is available
if (.not. diagnostic%is_available) then
   write(*,*) 'Diagnostic field not available or not computed'
endif
```

## Best Practices

### 1. **Check Availability**
Always check `diagnostic%is_available` before accessing data arrays.

### 2. **Handle Different Dimensions**
Use `diagnostic%dimensions` to determine the appropriate data array to access.

### 3. **Monitor Performance**
Accessing diagnostics has minimal overhead, but avoid excessive calls in tight loops.

### 4. **Validate Units**
Check `diagnostic%units` to ensure proper interpretation of data.

### 5. **Process-Specific Knowledge**
Understand what each process computes to effectively use its diagnostics.

## Integration with Host Models

### WRF-Chem Example

```fortran
! In WRF-Chem physics routine
call catchem%run_timestep(wrf_input, wrf_output, rc)

! Extract dust emissions for WRF diagnostics
call catchem%get_process_diagnostic('Dust', 'surface_emission_flux', diagnostic, rc)
if (diagnostic%is_available) then
   dustem_wrf(:,:) = diagnostic%data_2d(:,:)  ! Copy to WRF array
endif
```

### GEOS-Chem Example

```fortran
! In GEOS-Chem emission routine
call catchem%get_process_diagnostic('ExternalEmission', 'applied_emission_rate', diagnostic, rc)
if (diagnostic%is_available) then
   ! Add to GEOS-Chem emission arrays
   State_Chm%Species(:,:,:,ind_NO) = State_Chm%Species(:,:,:,ind_NO) + &
                                   diagnostic%data_3d(:,:,:)
endif
```

This comprehensive diagnostic access system provides both simplicity for basic use cases and power for advanced analysis, while maintaining the clean separation between the high-level API and the detailed underlying interfaces.

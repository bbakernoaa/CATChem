# CATChem API Quick Reference

## Essential Types

```fortran
use CATChemAPI_Mod

type(CATChemInstanceType) :: catchem
type(CATChemConfigType) :: config
type(CATChemDataType) :: input_data, output_data
integer :: rc
```

## Basic Workflow

```fortran
! 1. Configure (modern approach - configuration-driven)
config%nx = 144; config%ny = 91; config%nz = 72
! NOTE: nspecies is now read from configuration files, not set directly
config%config_file = 'CATChem_config.yml'
config%species_file = 'CATChem_species.yml'
config%emission_file = 'emission_config.yml'

! 2. Initialize (recommended modern method)
call catchem%init_from_config_files(config, rc)
! OR legacy method: call catchem%init(config, rc)
call catchem%setup_grid(lats, lons, levels, rc)

! 3. Configure run phases (optional)
config%enable_run_phases = .true.
config%run_phase_names = ['emissions', 'chemistry', 'transport', 'deposition']
call catchem%setup_run_phases(config%run_phase_names, rc)

! 4. Add processes
call catchem%add_process('dust', rc=rc)
call catchem%add_process('drydep', 'drydep_config.yml', rc)

! 5. Prepare data
allocate(input_data%temperature(nx,ny,nz))
allocate(input_data%concentrations(nx,ny,nz,nspecies))
! ... fill with your data ...

! 6. Run (single phase or all phases)
call catchem%run_timestep(input_data, output_data, rc)
! OR run specific phase: call catchem%run_phase('emissions', rc)
! OR run all phases: call catchem%run_all_phases(rc)

! 7. Extract results
updated_concentrations = output_data%concentrations
dust_flux = output_data%dust_emissions

! 8. Clean up
call catchem%finalize(rc)
```

## Data Fields

### Meteorological Inputs
- `temperature(:,:,:)` - Temperature [K]
- `pressure(:,:,:)` - Pressure [Pa]
- `humidity(:,:,:)` - Specific humidity [kg/kg]
- `wind_u(:,:,:)`, `wind_v(:,:,:)` - Wind [m/s]
- `surface_pressure(:,:)` - Surface pressure [Pa]

### Chemical Data
- `concentrations(:,:,:,:)` - Species concentrations [ppm or ug/m3]
- `emission_rates(:,:,:,:)` - Emission rates [mol/m²/s]

### Diagnostic Outputs
- `dust_emissions(:,:,:)` - Dust flux [kg/m²/s]
- `seasalt_emissions(:,:,:)` - Sea salt flux [kg/m²/s]
- `drydep_velocity(:,:,:)` - Deposition velocity [m/s]

## Error Handling

```fortran
if (rc /= CATCHEM_SUCCESS) then
   call catchem%get_error_message(error_msg)
   print *, "Error: ", trim(error_msg)
end if
```

## Process Diagnostics

```fortran
type(CATChemDiagnosticType) :: diagnostic

! Get specific diagnostic
call catchem%get_process_diagnostic('dust', 'emission_flux', diagnostic, rc)
if (diagnostic%is_available) then
   ! Use diagnostic%data_2d or diagnostic%data_3d
end if

! List all available diagnostics
call catchem%list_process_diagnostics(process_names, diagnostic_info, rc)
```

## Field Mapping (Advanced)

```fortran
type(FlexibleDataExchangeType) :: data_exchange

! Setup mappings
call catchem%add_field_mapping('met', 'temperature', 'host_temp', 'real64', rc=rc)
call catchem%add_field_mapping('chem', 'O3', 'ozone_vmr', 'real32', rc=rc)

! Use flexible exchange
call catchem%fill_met_state_from_host(data_exchange, rc)
call catchem%extract_chem_state_to_host(data_exchange, rc)
```

## Configuration Options

```fortran
! Grid dimensions (required)
config%nx = 144; config%ny = 91; config%nz = 72

! Configuration files (recommended initialization method)
config%config_file = 'CATChem_config.yml'    ! Main config (read first)
config%species_file = 'CATChem_species.yml'  ! Species config (determines nspecies)
config%emission_file = 'emission_config.yml' ! Emission config (optional)

! Process toggles
config%enable_dust = .true.           ! Enable dust emissions
config%enable_seasalt = .true.        ! Enable sea salt emissions
config%enable_drydep = .true.         ! Enable dry deposition
config%enable_external_emis = .true.  ! Enable external emissions

! Run phase configuration
config%enable_run_phases = .true.     ! Enable multi-phase execution
config%run_phase_names = ['emissions', 'chemistry', 'transport']  ! Phase sequence

! Performance options
config%use_column_processing = .true. ! Enable column virtualization
config%enable_diagnostics = .true.    ! Enable diagnostic output
```

## Return Codes

```fortran
integer, parameter :: CATCHEM_SUCCESS = 0
integer, parameter :: CATCHEM_FAILURE = -1
```

## Key Methods

| Method | Purpose |
|--------|---------|
| `init_from_config_files(config, rc)` | Modern config-driven initialization |
| `init(config, rc)` | Legacy initialization (deprecated) |
| `setup_grid(lats, lons, levels, rc)` | Setup grid coordinates |
| `setup_run_phases(phase_names, rc)` | Configure multi-phase execution |
| `add_process(name, config, rc)` | Add a process |
| `run_timestep(input, output, rc)` | Run single time step |
| `run_phase(phase_name, rc)` | Run specific phase |
| `run_all_phases(rc)` | Run all configured phases |
| `get_concentrations(data, rc)` | Get updated concentrations |
| `get_diagnostics(data, rc)` | Get diagnostic data |
| `validate_data(data, rc)` | Validate input data |
| `is_ready_to_run()` | Check if ready |
| `finalize(rc)` | Clean up |

## Run Phase Management

```fortran
! Configure phases
call catchem%setup_run_phases(['emissions', 'chemistry', 'transport'], rc)

! Run individual phases
call catchem%run_phase('emissions', rc)
call catchem%run_phase('chemistry', rc)

! Run all phases in sequence
call catchem%run_all_phases(rc)

! Get phase information
call catchem%get_phase_names(phase_names, rc)
current_phase = catchem%get_current_phase()
```

## Process Manager Configuration

```fortran
! Configure process manager settings
call catchem%configure_process_manager(max_processes=100, &
                                      enable_column_batching=.true., rc=rc)

! Set custom process manager (advanced)
call catchem%set_process_manager(custom_manager, rc)

! Access process manager directly (power users)
call catchem%get_process_manager(process_manager)
```

## Common Patterns

### Multiple Time Steps
```fortran
type(CATChemDataType) :: input_data(24), output_data(24)
call catchem%run_multiple_steps(24, input_data, output_data, rc)
```

### Data Validation
```fortran
call catchem%validate_data(input_data, rc)
if (rc /= CATCHEM_SUCCESS) then
   ! Fix data issues...
end if
```

### Species Names
```fortran
character(len=64) :: species_names(nspecies)
call catchem%get_species_names(species_names, rc)
```

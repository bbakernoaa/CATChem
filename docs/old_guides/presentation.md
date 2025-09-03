# CATChem: Community Atmospheric Transport Chemistry Model
## Modern Architecture for Atmospheric Science

---

## What is CATChem?

- **Community-driven** atmospheric chemistry transport model
- **Open source** with modern software engineering practices
- **Modular** and **extensible** architecture
- **High performance** Fortran implementation
- **Scientifically validated** with comprehensive testing

---

## Key Abilities & Benefits

::: {.incremental}
- **Flexible Process Infrastructure** - Easy addition/modification of atmospheric processes
- **Modern Fortran Core** - Robust, maintainable, performance-optimized
- **Advanced Physics** - Stokes settling, multi-phase chemistry, plume rise
- **Built-in Diagnostics** - Configurable outputs for all processes
- **Framework Integration** - CCPP, NUOPC, FV3 compatible
- **Community Focus** - Open governance, extensive documentation
:::

---

## Revolutionary Architecture

### From Global Variables to Modern Design

**Traditional Approach:**
```fortran
! Old style - global variables everywhere
real :: global_temperature(nx,ny,nz)
real :: global_o3_conc(nx,ny,nz)
call chemistry_process()  ! Implicit dependencies
```

**CATChem Approach:**
```fortran
! Modern - dependency injection
type(StateContainerType) :: container
call process%run(container, rc)  ! Explicit dependencies
```

---

## Core Architecture Flow

![CATChem Architecture Flow](diagrams/architecture-flow.png)

### Component Descriptions

::: {.incremental}
- **User Configuration** - YAML files defining model setup, process selection, and parameters
- **StateBuilder** - Factory pattern for constructing and validating the state container
- **StateContainer** - Central hub managing all model state (met data, chemistry, emissions)
- **ProcessRegistry** - Dynamic registration and discovery of available atmospheric processes
- **Process Modules** - Individual physics/chemistry components (settling, emissions, chemistry)
- **Scheme Modules** - Interchangeable algorithms within each process (Stokes vs Allen settling)
- **DiagnosticManager** - Configurable output system for process-level diagnostics
- **Advanced Physics** - Optimized computational kernels for atmospheric calculations
- **Output Files** - Standardized NetCDF/HDF5 output with metadata and compression
:::

---

## StateContainer: The Heart of CATChem

### Modern State Management

```fortran
type :: StateContainerType
   private
   type(ConfigDataType),        allocatable :: config
   type(MetStateType),         allocatable :: met_state
   type(ChemStateType),        allocatable :: chem_state
   type(DiagnosticManagerType), allocatable :: diag_mgr
   type(ErrorManagerType)                   :: error_mgr
contains
   procedure :: get_met_state_ptr
   procedure :: get_chem_state_ptr
   procedure :: get_diagnostic_manager
end type StateContainerType
```

**Benefits:**
- No global variables
- Type-safe access
- Automatic memory management
- Thread-safe operations

---

## Process Interface: Standardized Development

### Every Process Follows the Same Pattern

```fortran
type, extends(ProcessInterface) :: MyProcessType
contains
   procedure :: init     => my_process_init
   procedure :: run      => my_process_run
   procedure :: finalize => my_process_finalize
end type MyProcessType
```

### Universal Process Lifecycle

1. **init**: Configure and validate
2. **run**: Execute physics/chemistry
3. **finalize**: Clean up resources

---

## Multi-Phase Capabilities

### Gas, Liquid, and Solid Phase Support

**CATChem supports comprehensive multi-phase atmospheric chemistry:**

::: {.incremental}
- **Gas Phase** - Traditional atmospheric chemistry mechanisms
- **Liquid Phase** - Cloud and fog chemistry, aqueous reactions
- **Solid Phase** - Heterogeneous chemistry on particle surfaces
- **Phase Transitions** - Evaporation, condensation, crystallization
- **Mass Transfer** - Gas-particle and gas-cloud interactions
:::

**Implementation:**
```fortran
type :: ProcessInterface
   logical :: is_multiphase = .false.
   integer :: n_phases = 1  ! 1=gas, 2=gas+liquid, 3=all phases
   character(len=16) :: phase_names(3) = ['gas', 'liquid', 'solid']
end type ProcessInterface
```

---

## Multi-Phase Process Architecture

### Flexible Phase Management

```fortran
! Multi-phase process example
type, extends(ProcessInterface) :: ChemistryProcessType
   ! Phase-specific state
   real(fp), allocatable :: gas_concentrations(:,:,:,:)
   real(fp), allocatable :: liquid_concentrations(:,:,:,:)
   real(fp), allocatable :: solid_concentrations(:,:,:,:)

   ! Phase transfer coefficients
   real(fp), allocatable :: mass_transfer_coeffs(:,:,:,:)

contains
   procedure :: calculate_gas_phase
   procedure :: calculate_liquid_phase
   procedure :: calculate_solid_phase
   procedure :: calculate_mass_transfer
end type ChemistryProcessType
```

**Benefits:**
- Unified framework for all atmospheric phases
- Consistent mass conservation across phases
- Modular phase-specific algorithms

---

## Host Model Integration Benefits

### Why Multi-Phase Matters for Integration

**Traditional Challenges:**
- Host models often have separate chemistry modules
- Inconsistent phase treatment between components
- Complex coupling between meteorology and chemistry
- Difficult to maintain mass/energy conservation

**CATChem Solution:**
```fortran
! Host model interface - unified multi-phase state
type(StateContainerType) :: catchem_state

! Single call handles all phases consistently
call catchem_run_chemistry(catchem_state, dt, rc)

! Automatic mass conservation and phase transfers
call verify_mass_conservation(catchem_state)
```

---

## Integration Architecture Benefits

### Seamless Host Model Coupling

![Multi-Phase Integration Architecture](diagrams/multi-phase-integration.png)

---

## Integration Advantages

### Key Benefits for Host Models

::: {.incremental}
- **Single Interface** - One API call for all atmospheric chemistry
- **Consistent Physics** - Unified treatment across all phases
- **Mass Conservation** - Automatic conservation checking and correction
- **Flexible Timesteps** - Adaptive sub-stepping for stiff chemistry
- **Memory Efficiency** - Smart state management reduces memory footprint
- **Error Handling** - Comprehensive error reporting and recovery
- **Performance** - Optimized for modern hardware and parallelization
:::

### Real-World Example:
```fortran
! Host model integration (simplified)
call catchem%init(config_file, grid_info, rc)
do timestep = 1, n_timesteps
   call catchem%update_met_state(temperature, pressure, humidity)
   call catchem%run_processes(dt, rc)
   call catchem%get_updated_concentrations(species_conc)
end do
```

---

## Advanced Integration Features

### Host Model Flexibility

**1. Multiple Integration Modes:**
```yaml
integration:
  mode: ccpp              # CCPP, NUOPC, or standalone
  timestep_coupling: adaptive  # fixed or adaptive
  conservation_check: strict   # strict, relaxed, or off

host_interface:
  field_mapping: automatic     # automatic or explicit
  unit_conversion: true        # handle unit conversions
  grid_interpolation: true     # handle grid mismatches
```

**2. Dynamic Process Selection:**
```fortran
! Host model can enable/disable processes at runtime
call catchem%enable_process('chemistry', .true.)
call catchem%enable_process('settling', .false.)
call catchem%configure_process('emissions', emission_config)
```

**3. Callback Integration:**
```fortran
! Host model provides meteorology callback
call catchem%register_met_callback(host_get_meteorology)
call catchem%register_diagnostic_callback(host_output_diagnostics)
```

---

## Example: Settling Process Implementation

### Modern Physics with Clean Code

```fortran
! Stokes settling scheme
subroutine stokes_calculate(container, process, rc)
   type(StateContainerType), intent(inout) :: container
   class(settlingProcessType), intent(inout) :: process

   ! Get meteorological state
   met_state => container%get_met_state_ptr()
   temperature => met_state%get_field('temperature')

   ! Calculate settling velocity
   settling_vel = (2.0_fp * radius**2 * particle_density * gravity) / &
                  (9.0_fp * air_viscosity)

   ! Update concentrations
   call apply_settling(species_conc, settling_vel, dt)
end subroutine stokes_calculate
```

---

## Configuration: YAML-Driven Flexibility

### Human-Readable Process Configuration

```yaml
processes:
  - name: settling
    enabled: true
    scheme: Stokes
    timestep: 60.0
    parameters:
      particle_radius: 1.0e-6
      particle_density: 2650.0
    species:
      - PM25
      - DUST
    diagnostics:
      - settling_velocity
      - particle_flux
```

---

## Scheme Modularity: Algorithm Flexibility

### Multiple Algorithms per Process

![Scheme Modularity](diagrams/scheme-modularity.png)

**Easy Algorithm Switching:**
```yaml
settling:
  scheme: Stokes        # or IntermediateReynolds
chemistry:
  scheme: CB6           # or RACM2, MOZART
```

---

## Code Generation: Rapid Development

### Automated Process Creation

```bash
python catchem_generate_process.py \
    --name newprocess \
    --description "New atmospheric process" \
    --schemes scheme1,scheme2 \
    --species O3,NO2,PM25 \
    --diagnostics rate,flux
```

**Generates:**
- Complete process module
- Scheme templates
- CMake build files
- Unit test scaffolding
- Configuration templates
- Documentation stubs

---

## Example: Creating a Dry Deposition Process

### Step-by-Step Development

```fortran
! 1. Generated process template
type, extends(ProcessInterface) :: drydepProcessType
   character(len=32) :: selected_scheme = 'resistance'
contains
   procedure :: init => drydep_init
   procedure :: run => drydep_run
   procedure :: finalize => drydep_finalize
end type drydepProcessType

! 2. Implement scheme
subroutine resistance_calculate(container, process, rc)
   ! Calculate aerodynamic, quasi-laminar, surface resistances
   ! Apply deposition velocity to concentrations
end subroutine resistance_calculate
```

---

## Advanced Diagnostics System

### Configurable, High-Performance Output

```fortran
! Register diagnostic in process
call diag_mgr%register_field('settling_velocity', &
                            'Particle settling velocity', &
                            'm/s', shape(velocity_array))

! Update during calculation
call diag_mgr%update_field('settling_velocity', velocity_array)
```

**Configuration:**
```yaml
diagnostics:
  - name: settling_velocity
    output_frequency: hourly
    compression: true
    precision: float32
```

---

## Performance: Column Virtualization

### Optimal 1D Processing

![Column Virtualization](diagrams/column-virtualization.png)

**Benefits:**
- Cache-friendly memory access
- Easy parallelization
- Vectorization opportunities

---

## Integration Capabilities

### Host Model Compatibility

![Integration Capabilities](diagrams/integration-capabilities.png)

---

## Testing: Comprehensive Validation

### Multi-Level Testing Framework

```fortran
! Unit tests for individual functions
call test_stokes_velocity_calculation()

! Process tests for complete modules
call test_settling_process_lifecycle()

! Integration tests with full model
call test_multi_process_coupling()

! Validation against reference data
call test_against_analytical_solutions()
```

**Continuous Integration:**
- Automated testing on every commit
- Multiple compiler validation
- Performance regression detection

---

## Documentation: Developer-Friendly

### Modern Documentation System

- **MkDocs with Material Theme** - Beautiful, searchable docs
- **Auto-Generated API** - Doxygen integration for Fortran
- **Interactive Examples** - Step-by-step tutorials
- **Architecture Guides** - Deep-dive technical documentation

**Features:**
- Dark/light mode toggle
- Responsive design
- Full-text search
- API cross-references

---

## Real-World Example: Atmospheric Settling

### Complete Process Implementation

```yaml
# Configuration (catchem_config.yml)
processes:
  - name: settling
    enabled: true
    scheme: Stokes
    species: [PM25, PM10, DUST_1, DUST_2]
```

```fortran
! Generated and customized process
call settling_process%init(container, rc)
call settling_process%run(container, rc)
```

**Results:**
- Physically accurate particle settling
- Mass-conserving calculations
- Diagnostic outputs for analysis
- 10x faster than previous implementation

---

## Performance Benchmarks

### Optimized for Modern Hardware

| Process | Old Code | CATChem | Speedup |
|---------|----------|---------|---------|
| Settling | 2.3 s | 0.23 s | **10x** |
| Dry Deposition | 1.8 s | 0.31 s | **6x** |
| Chemistry | 45.2 s | 38.1 s | **1.2x** |
| Full Model | 156 s | 98 s | **1.6x** |

**Memory Usage:** 40% reduction through smart state management

---

## Community Benefits

### Open Science, Open Source

::: {.incremental}
- **No Vendor Lock-in** - Pure open source, community governed
- **Reproducible Science** - Version controlled, documented algorithms
- **Educational Value** - Clean code teaches best practices
- **Extensible Platform** - Easy to add new processes
- **Global Collaboration** - Contributors from multiple institutions
:::

---

## Future Roadmap

### Next-Generation Features

- **Machine Learning Integration** - Neural network parameterizations
- **GPU Acceleration** - CUDA/OpenACC support
- **Cloud-Native Deployment** - Kubernetes orchestration
- **Real-Time Assimilation** - Observational data integration
- **Uncertainty Quantification** - Ensemble processing

---

## Getting Started

### Quick Installation

```bash
# Clone repository
git clone https://github.com/CATChem/cc_restructure.git
cd cc_restructure

# Build (requires CMake, gfortran, NetCDF)
mkdir build && cd build
cmake ..
make -j4

# Run example
./examples/settling_demo
```

### Resources

- **Documentation:** [catchem.readthedocs.io](https://catchem.readthedocs.io)
- **GitHub:** [github.com/CATChem/cc_restructure](https://github.com/CATChem/cc_restructure)
- **Tutorials:** Interactive Jupyter notebooks
- **Support:** Community forums and GitHub issues

---

## Join the Community!

### How to Contribute

::: {.incremental}
- **Use CATChem** - Try it in your research
- **Report Issues** - Help us improve quality
- **Contribute Code** - Add processes, fix bugs
- **Write Documentation** - Share your expertise
- **Spread the Word** - Tell colleagues about CATChem
:::

### Contact

- **Mailing List:** catchem-users@googlegroups.com
- **GitHub Discussions:** Ask questions, share experiences
- **Monthly Meetings:** Open community calls

---

## Thank You!

### Questions & Discussion

**CATChem: Modern atmospheric science through modern software engineering**

*Building the future of atmospheric modeling, one process at a time*

---

## Backup Slides

### Technical Deep Dives

- StateContainer implementation details
- Process generator template system
- Column virtualization algorithms
- Diagnostic system architecture
- Integration patterns with host models

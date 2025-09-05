# Process Generator Tutorial

The CATChem Process Generator is a powerful tool that automates the creation of new atmospheric processes following established architecture patterns. This tutorial will walk you through creating processes from simple examples to complex multi-phase chemistry.

## Table of Contents

1. [Quick Start](#quick-start)
2. [Generator Overview](#generator-overview)
3. [Basic Examples](#basic-examples)
4. [Advanced Examples](#advanced-examples)
5. [Configuration Files](#configuration-files)
6. [Generated Documentation](#generated-documentation)
7. [Customizing Templates](#customizing-templates)
8. [Best Practices](#best-practices)

## Quick Start

The process generator is located in `tools/process_generator/process_generator.py`. Here's the simplest way to create a new process:

```bash
cd /path/to/catchem/tools/process_generator
python process_generator.py generate --config configs/seasalt_emission.yaml
```

This command generates:

- Process module: `src/process/seasalt/ProcessSeaSaltInterface_Mod.F90`
- Common module: `src/process/seasalt/SeaSaltCommon_Mod.F90` 
- Scheme modules: `src/process/seasalt/schemes/`
- Test files: `tests/process/seasalt/`
- CMake files: Updated automatically
- Documentation: `docs/processes/seasalt/`

## Generator Overview

### Command Structure

```bash
python process_generator.py COMMAND [OPTIONS]

Commands:
  generate    Generate complete process implementation
  validate    Validate configuration file
  template    Create template configuration file
```

### Supported Process Types

The generator supports several process types, each with specific patterns and templates:

| Type | Description | Common Use Cases |
|------|-------------|------------------|
| `emission` | Source processes | Anthropogenic/biogenic emissions |
| `transformation` | Chemical/physical transformations | Gas-phase chemistry, aerosol processes |
| `loss` | Removal processes | Dry deposition, radioactive decay |
| `sink` | Deposition processes | Wet deposition, surface uptake |
| `transport` | Movement processes | Advection, diffusion |
| `multiphase_chemistry` | Multi-phase reactions | Cloud chemistry, heterogeneous reactions |
| `aqueous_chemistry` | Aqueous-only reactions | Fog chemistry, droplet processes |

### Command Line Options

```bash
python util/catchem_generate_process.py [OPTIONS]

Required:
  --name NAME              Process name (e.g., DryDeposition, Chemistry)
  --type TYPE              Process type (see table above)

Optional:
  --schemes SCHEMES        Comma-separated list of scheme names
  --species SPECIES        Comma-separated list of species
  --description TEXT       Process description
  --author AUTHOR          Author name (default: from git config)
  --solver SOLVER          For chemistry: euler, rk4, rosenbrock, kpp, micm
  --phases PHASES          For multiphase: gas, liquid, solid
  --config FILE            Use YAML configuration file
  --output-dir DIR         Output directory (default: auto-detected)
  --dry-run                Show what would be generated without creating files
  --verbose                Enable verbose output
```

## Basic Examples

### Example 1: Simple Dry Deposition Process

Let's create a dry deposition process with two schemes:

```bash
python util/catchem_generate_process.py \
    --name DryDeposition \
    --type loss \
    --schemes resistance,zhang2001 \
    --species O3,NO2,SO2,PM25,PM10 \
    --description "Dry deposition of gases and particles to surface"
```

**Generated Structure:**
```
src/process/drydep/
├── DryDepositionProcess_Mod.F90          # Main process module
├── CMakeLists.txt                        # Build configuration
└── schemes/
    ├── DryDepositionResistanceScheme_Mod.F90    # Resistance scheme
    └── DryDepositionZhang2001Scheme_Mod.F90     # Zhang 2001 scheme

tests/
└── test_drydeposition.F90                # Unit tests

docs/processes/drydeposition/
├── index.md                              # Process documentation
├── schemes.md                            # Scheme descriptions
└── examples.md                           # Usage examples
```

**Generated Process Module (simplified):**
```fortran
module DryDepositionProcess_Mod
  use ProcessInterface_Mod
  use ErrorManager_Mod
  use ConfigManager_Mod
  implicit none
  private

  type, extends(ProcessInterface), public :: DryDepositionProcessType
    private
    character(len=32) :: selected_scheme = 'resistance'
    ! Process-specific data members
  contains
    procedure, public :: init => drydep_init
    procedure, public :: run => drydep_run
    procedure, public :: finalize => drydep_finalize
    procedure, private :: validate_inputs
    procedure, private :: calculate_loss_rates
    procedure, private :: apply_loss_processes
  end type DryDepositionProcessType

contains

  subroutine drydep_init(this, container, rc)
    class(DryDepositionProcessType), intent(inout) :: this
    type(StateContainerType), intent(inout) :: container
    integer, intent(out) :: rc

    ! Initialize process
    ! Load configuration
    ! Validate inputs
    ! Set up diagnostics
  end subroutine drydep_init

  ! ... other methods
end module DryDepositionProcess_Mod
```

### Example 2: Particle Settling Process

```bash
python util/catchem_generate_process.py \
    --name Settling \
    --type transformation \
    --schemes stokes,intermediate_reynolds \
    --species PM25,PM10,DUST_1,DUST_2,DUST_3,DUST_4,DUST_5 \
    --description "Gravitational settling of particles"
```

This generates a settling process with two different velocity calculation schemes.

### Example 3: Biogenic Emissions

```bash
python util/catchem_generate_process.py \
    --name BiogenicEmissions \
    --type emission \
    --schemes megan,beis \
    --species ISOP,TERP,NO,NH3,CO \
    --description "Biogenic emissions from vegetation"
```

## Advanced Examples

### Example 4: Multi-Phase Chemistry Process

For complex chemistry processes, use the multiphase chemistry type:

```bash
python util/catchem_generate_process.py \
    --name AqueousChemistry \
    --type multiphase_chemistry \
    --phases gas,liquid \
    --solver micm \
    --schemes cb6_aqueous,racm2_aqueous \
    --species O3,NO2,NO,SO2,NH3,H2O2,HNO3 \
    --description "Gas-liquid phase chemistry in clouds and fog"
```

**Generated Multi-Phase Structure:**
```fortran
type, extends(ProcessInterface) :: AqueousChemistryProcessType
  private
  ! Phase-specific concentrations
  real(fp), allocatable :: gas_conc(:,:,:,:)
  real(fp), allocatable :: liquid_conc(:,:,:,:)

  ! Phase transfer data
  real(fp), allocatable :: henry_constants(:)
  real(fp), allocatable :: mass_transfer_coeffs(:,:,:,:)

  ! Solver configuration
  character(len=16) :: solver_type = 'micm'

contains
  procedure :: init => aqueous_init
  procedure :: run => aqueous_run
  procedure :: calculate_gas_phase
  procedure :: calculate_liquid_phase
  procedure :: calculate_mass_transfer
  procedure :: partition_species
  procedure :: check_phase_balance
end type AqueousChemistryProcessType
```

### Example 5: Transport Process

```bash
python util/catchem_generate_process.py \
    --name Advection \
    --type transport \
    --schemes upwind,ppm,weno \
    --species ALL \
    --description "Advective transport of chemical species"
```

## Configuration Files

For complex processes, use YAML configuration files instead of command-line arguments:

### Example Configuration: `my_process_config.yaml`

```yaml
process:
  name: ComplexChemistry
  type: multiphase_chemistry
  description: "Complex atmospheric chemistry with multiple mechanisms"
  author: "Jane Scientist"

  # Multi-phase configuration
  phases: [gas, liquid, solid]
  solver: micm

  # Schemes for different chemical mechanisms
  schemes:
    - name: cb6_full
      description: "Complete CB6 mechanism"
      phases: [gas]
    - name: cb6_aqueous
      description: "CB6 with aqueous chemistry"
      phases: [gas, liquid]
    - name: heterogeneous
      description: "Heterogeneous surface chemistry"
      phases: [gas, solid]

  # Species list
  species:
    gas_phase: [O3, NO, NO2, NO3, N2O5, HNO3, CO, CO2, CH4, SO2, H2SO4]
    liquid_phase: [O3_aq, NO2_aq, HNO3_aq, SO2_aq, H2SO4_aq]
    solid_phase: [NO3_s, SO4_s, NH4_s]

  # Diagnostics configuration
  diagnostics:
    process_level: [reaction_rates, phase_partitioning, mass_conservation]
    scheme_level: [individual_reaction_rates, equilibrium_constants]
    species_level: [concentration_tendencies, phase_transfers]

  # Testing configuration
  testing:
    unit_tests: true
    integration_tests: true
    reference_data: "test_data/complex_chemistry_reference.nc"

  # Documentation
  documentation:
    include_examples: true
    include_theory: true
    include_validation: true
```

**Use the configuration:**
```bash
python util/catchem_generate_process.py --config my_process_config.yaml
```

## Generated Documentation

The generator automatically creates comprehensive documentation for your process:

### Documentation Structure

```
docs/processes/[process_name]/
├── index.md           # Main process documentation
├── schemes.md         # Detailed scheme descriptions
├── examples.md        # Usage examples and tutorials
├── theory.md          # Scientific background (if requested)
├── validation.md      # Validation and testing (if requested)
├── api.md            # API reference (auto-generated from code)
└── references.md      # Scientific references
```

### Example Generated Documentation

**`docs/processes/drydeposition/index.md`:**
```markdown
# Dry Deposition Process

## Overview

The dry deposition process calculates the removal of gases and particles
from the atmosphere to the Earth's surface through turbulent transport
and surface uptake.

## Scientific Background

Dry deposition velocities are calculated using a resistance analogy:

$$v_d = \\frac{1}{r_a + r_b + r_c}$$

Where:
- $r_a$ = aerodynamic resistance
- $r_b$ = quasi-laminar boundary layer resistance
- $r_c$ = surface resistance

## Configuration

```yaml
processes:
  - name: dry_deposition
    enabled: true
    scheme: resistance  # or zhang2001
    species: [O3, NO2, SO2, PM25, PM10]
    parameters:
      reference_height: 10.0      # m
      surface_roughness: 0.1      # m
```

## Available Schemes

### Resistance Scheme (`resistance`)

Classic three-resistance model based on Wesely (1989).

**Parameters:**

- `reference_height`: Reference height for meteorological data (m)
- `surface_roughness`: Surface roughness length (m)
- `canopy_height`: Vegetation canopy height (m, optional)

### Zhang 2001 Scheme (`zhang2001`)

Size-dependent scheme for particles (Zhang et al., 2001).

**Parameters:**

- `particle_density`: Particle density (kg/m³)
- `particle_diameter`: Particle diameter (m)

## Usage Example

```fortran
! Configure and run dry deposition
type(DryDepositionProcessType) :: drydep_process
type(StateContainerType) :: container

call drydep_process%init(container, rc)
call drydep_process%run(container, rc)
```

## Diagnostics

Available diagnostic outputs:

- `deposition_velocity`: Species-specific deposition velocities (m/s)
- `surface_flux`: Deposition flux to surface (kg/m²/s)
- `aerodynamic_resistance`: Aerodynamic resistance (s/m)
- `surface_resistance`: Surface resistance (s/m)

## References

1. Wesely, M. L. (1989). Parameterization of surface resistances...
2. Zhang, L., et al. (2001). A size-segregated particle dry deposition scheme...
```

### Auto-Generated API Documentation

The generator creates API documentation by parsing the generated Fortran code:

**`docs/processes/drydeposition/api.md`:**
```markdown
# Dry Deposition API Reference

## DryDepositionProcessType

Main process type for dry deposition calculations.

### Public Methods

#### init(container, rc)
Initialize the dry deposition process.

**Arguments:**
- `container` (StateContainerType, inout): State container
- `rc` (integer, out): Return code

#### run(container, rc)
Execute dry deposition calculations.

**Arguments:**
- `container` (StateContainerType, inout): State container
- `rc` (integer, out): Return code

### Configuration Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| scheme | string | 'resistance' | Deposition scheme |
| reference_height | real | 10.0 | Reference height (m) |
| surface_roughness | real | 0.1 | Surface roughness (m) |
```

## Customizing Templates

You can customize the code generation by modifying templates in `util/templates/`:

### Template Files

- `process_main.f90.j2`: Main process module template
- `process_multiphase.f90.j2`: Multi-phase process template
- `scheme_simple.f90.j2`: Basic scheme template
- `test_process.f90.j2`: Unit test template
- `process_documentation.md.j2`: Documentation template

### Example Template Customization

Edit `util/templates/process_main.f90.j2` to add custom methods:

```fortran
{# Custom template section #}
{% if process_type == 'emission' %}
  procedure, private :: calculate_emission_factors
  procedure, private :: apply_temporal_scaling
{% endif %}
```

### Creating New Templates

1. Create new template file: `util/templates/my_template.f90.j2`
2. Register in generator: Edit `ProcessGenerator.process_types`
3. Use template variables: `{{ process_name }}`, `{{ schemes }}`, etc.

## Best Practices

### 1. Naming Conventions

- **Process names**: PascalCase (e.g., `DryDeposition`, `GasPhaseChemistry`)
- **Scheme names**: snake_case (e.g., `resistance`, `zhang2001`, `cb6_full`)
- **File names**: Follow process name + suffix

### 2. MetState Field Usage

```yaml
# Good: Use standard MetState field names
schemes:
  - name: my_scheme
    required_met_fields: [FROCEAN, SST, USTAR, T, QV]  # Standard field names
    affects_full_column: false  # Surface-only processing

# Check available fields first
# python process_generator.py fields --type all --verbose
```

### 3. Process Design

- **Single responsibility**: Each process should handle one chemical operation or task
- **Scheme modularity**: Use schemes for different algorithms, not different operations or tasks
- **State dependencies**: Minimize dependencies between state components
- **Field requirements**: Use `affects_full_column` attribute correctly

### 4. Configuration

```yaml
# Good: Clear, organized configuration with field discovery
schemes:
  - name: gong97
    description: "Gong 1997 sea salt emission scheme"
    required_met_fields: [FROCEAN, FRSEAICE, SST, U10M, V10M]  # Discovered automatically
    affects_full_column: false  # Surface-only emission
    parameters:
      scale_factor:
        default: 1.0
        description: "Emission scale factor"

# Avoid: Unclear or overly complex configuration
schemes:
  - name: gong97
    everything_enabled: true
    params: {sf: 1.0, all_fields: yes}
```

### 5. Field Discovery Integration

```bash
# Always check field availability first
python process_generator.py fields --type all

# Validate configuration with automatic field discovery
python process_generator.py validate --config my_process.yaml

# Generate with explicit MetState path if needed
python process_generator.py generate --config my_process.yaml --metstate ../../src/core/metstate_mod.F90
```

### 6. Testing

Always generate with unit tests enabled:
```bash
python process_generator.py generate --config my_process.yaml  # Tests included by default
```

### 7. Documentation

Use descriptive names and include scientific background:
```yaml
process:
  name: "seasalt"
  description: "Process for computing sea salt aerosol emissions over ocean surfaces"
  author: "Barry Baker & Wei Li"
  
schemes:
  - name: gong97
    description: "Gong 1997 sea salt emission scheme"
    reference: "Gong et al. [1997]"
```

## Troubleshooting

### Common Issues

**1. Import errors in generated code**

- Check that all dependencies are properly specified
- Verify module names follow CATChem conventions

**2. Build failures**

- Ensure CMake files are properly updated
- Check that all required dependencies are available

**3. Test failures**

- Verify test data paths are correct
- Check that reference data matches expected format

**4. Documentation not generated**

- Ensure output directory has write permissions
- Check that all template files are present

### Getting Help

1. **Check generated logs**: Use `--verbose` flag for detailed output
2. **Dry run first**: Use `--dry-run` to preview without creating files
3. **Community support**: Post questions on GitHub Discussions
4. ****[Process Development Guide](index.md)****: See Process Development Guide for more details

## Next Steps

After generating your process:

1. **Review generated code**: Understand the structure and modify as needed
2. **Implement physics**: Add the actual scientific calculations to scheme modules
3. **Add tests**: Extend the generated tests with specific test cases
4. **Run tests**: Verify your implementation works correctly
5. **Update documentation**: Add scientific references and validation data
6. **Submit PR**: Contribute your process back to the community!

For more detailed guidance on implementing chemistry, see:

- **[Process Architecture Guide](architecture.md)**
- **[Creating Custom Processes](creating.md)**
- **[Testing Guide](testing.md)**

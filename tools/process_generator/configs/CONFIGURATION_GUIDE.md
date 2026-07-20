# CATChem Process Generator Configuration Guide

This guide explains the YAML configuration format for defining CATChem atmospheric chemistry processes.

## Configuration Structure

### Process Metadata

```yaml
process:
  name: "ProcessName"                    # Human-readable process name
  class_name: "ProcessClassName"         # Fortran class name (used in module names)
  description: "Process description"     # Brief description
  version: "1.0.0"                      # Version string
  author: "Author Name"                  # Author information
  process_type: "emission"               # emission, chemistry, deposition, aerosol
  default_scheme: "scheme_name"          # Default algorithm scheme
```

### Species Configuration

```yaml
species:                                # List of chemical species handled
  - "species1"
  - "species2"
  - "species3"
```

### Meteorological Field Dependencies

Meteorological fields can be defined at the process level (common to all schemes) or scheme level (scheme-specific):

```yaml
required_met_fields:
  - name: "field_name"                  # Name used in CATChem state manager
    variable_name: "fortran_var_name"   # Fortran variable name in generated code
    description: "Field description"    # Human-readable description
    units: "field_units"               # Physical units
    dimensions: "scalar"                # scalar, 1d, 2d, 3d

    # For 2D/3D fields, specify dimension meanings:
    dim1_description: "first_dimension"     # e.g., "species", "size_bin"
    dim2_description: "second_dimension"    # e.g., "vertical_level", "mode"
    dim3_description: "third_dimension"     # e.g., "time", "wavelength"

    # For 2D/3D fields, specify dimension sizes:
    first_dimension_size: "this%n_species"   # Can be constant or variable
    second_dimension_size: "n_levels"        # Can be constant or variable
    third_dimension_size: "n_wavelengths"    # Can be constant or variable
```

#### Dimension Types

- **`scalar`**: Single value per column (e.g., surface temperature, solar zenith angle)
- **`1d`**: Profile with vertical levels (e.g., temperature, pressure, humidity)
- **`2d`**: Matrix data (e.g., photolysis rates by species×level, size distribution by bin×level)
- **`3d`**: Tensor data (e.g., aerosol mass by species×bin×level)

#### Common Dimension Sizes

- `n_levels` - Number of vertical levels
- `this%n_species` - Number of species handled by this process
- `this%n_size_bins` - Number of size bins (for aerosol processes)
- Constants like `12` for months, `24` for hours, etc.

### Process Schemes

Multiple algorithms can be defined for the same process. Each scheme can have its own type and expected outputs:

```yaml
schemes:
  - name: "scheme1"
    class_name: "Scheme1Class"
    description: "First algorithm"
    scheme_type: "emission"              # emission, chemistry, deposition, aerosol
    algorithm: "Algorithm description"    # Brief algorithm description
    implementation: "Implementation details"  # Implementation specifics

    # Expected outputs from this scheme
    outputs:
      - name: "output_variable"
        description: "Output description"
        units: "output_units"
        type: "scalar"                   # scalar, array
        dimensions: "species"            # For arrays: species, size_bins, levels, etc.

    # Scheme-specific meteorological fields
    required_met_fields:
      - name: "scheme_specific_field"
        # ... field definition

    # Scheme-specific diagnostics
    diagnostics:
      - name: "scheme_diagnostic"
        # ... diagnostic definition

  - name: "scheme2"
    # ... second algorithm
```

#### Scheme Types

- **`emission`**: Produces emission rates or fluxes
- **`chemistry`**: Modifies species concentrations through chemical reactions
- **`deposition`**: Removes species through deposition processes
- **`aerosol`**: Modifies aerosol size distributions and composition

#### Scheme Outputs

Define what your scheme produces:

```yaml
outputs:
  - name: "species_conc"              # Updated concentrations
    description: "Modified species concentrations"
    units: "molecules/cm3"
    type: "array"
    dimensions: "species"

  - name: "emission_rates"            # Emission rates
    description: "Species emission rates"
    units: "molecules/cm3/s"
    type: "array"
    dimensions: "species"

  - name: "temperature_factor"        # Scalar diagnostics
    description: "Temperature response factor"
    units: "dimensionless"
    type: "scalar"
```

### Diagnostics

Diagnostics can be defined at process or scheme level:

```yaml
diagnostics:
  - name: "diagnostic_name"
    description: "Diagnostic description"
    units: "diagnostic_units"
    type: "diagnostic_type"           # See types below

    # Optional: species-specific diagnostics
    species: "species_name"

    # Optional: reaction-specific diagnostics
    reaction: "reaction_name"
```

#### Diagnostic Types

- **`emission_rate`**: Emission rates and fluxes
- **`column_total`**: Column-integrated quantities
- **`surface_flux`**: Surface exchange fluxes
- **`reaction_rate`**: Chemical reaction rates
- **`temperature_dependence`**: Temperature sensitivity factors
- **`humidity_effect`**: Humidity influence factors
- **`efficiency`**: Process efficiency measures
- **`rate_constant`**: Effective rate constants

### Process Parameters

Optional configuration parameters:

```yaml
parameters:
  parameter1: value1
  parameter2: value2
  # Process-specific parameters
```

### Runtime Configuration

Optional runtime settings:

```yaml
runtime:
  frequency: "every_timestep"         # "every_timestep", "hourly", "daily"
  parallel: true                      # Enable parallel processing
  memory_efficient: false             # Use memory-efficient algorithms
```

## Generated Code Structure

The process generator creates several Fortran modules based on the configuration:

### Main Interface Module
- `Process{ClassName}Interface_Mod.F90` - Main process interface
- Implements `ProcessInterface` abstract type
- Handles meteorological field retrieval using the YAML configuration
- Dynamic allocation and cleanup based on field dimensions

### Generated Met Field Code

For each meteorological field in the YAML, the generator creates:

```fortran
! Declaration based on dimensions
{% if field.dimensions == "scalar" %}
real(fp) :: {{ field.variable_name }}              ! {{ field.description }} [{{ field.units }}]
{% elif field.dimensions == "1d" %}
real(fp), allocatable :: {{ field.variable_name }}(:)  ! {{ field.description }} [{{ field.units }}]
{% elif field.dimensions == "2d" %}
real(fp), allocatable :: {{ field.variable_name }}(:,:)  ! {{ field.description }} [{{ field.units }}]
{% endif %}

! Allocation based on dimensions
{% if field.dimensions == "1d" %}
allocate({{ field.variable_name }}(n_levels))
{% elif field.dimensions == "2d" %}
allocate({{ field.variable_name }}({{ field.first_dimension_size }}, {{ field.second_dimension_size }}))
{% endif %}

! Retrieval based on dimensions
{% if field.dimensions == "scalar" %}
call container%get_met_field_scalar('{{ field.name }}', i_col, {{ field.variable_name }}, rc)
{% elif field.dimensions == "1d" %}
call container%get_met_field('{{ field.name }}', i_col, {{ field.variable_name }}, rc)
{% elif field.dimensions == "2d" %}
call container%get_met_field_2d('{{ field.name }}', i_col, {{ field.variable_name }}, rc)
{% endif %}

! Cleanup
{% if field.dimensions in ["1d", "2d"] %}
deallocate({{ field.variable_name }})
{% endif %}
```

## Example Configurations

See the example configuration files:

- `example_emission_process.yaml` - Biogenic emissions with PAR and temperature dependencies
- `example_chemistry_process.yaml` - Gas-phase chemistry with photolysis rates
- `example_aerosol_process.yaml` - Aerosol coagulation with size bins and 3D fields

## Best Practices

1. **Use descriptive names**: Make field names and descriptions clear
2. **Specify units**: Always include physical units in the configuration
3. **Document dimensions**: Use clear dimension descriptions
4. **Consistent naming**: Use consistent variable naming conventions
5. **Validate fields**: Ensure meteorological field names match those available in CATChem state manager
6. **Test configurations**: Use the process generator to validate YAML syntax and structure

## Validation

The process generator validates:
- YAML syntax and structure
- Required fields are present
- Dimension specifications are consistent
- Species and field names are valid
- Diagnostic configurations are complete

Run validation with:
```bash
python process_generator.py your_config.yaml --validate-only
```

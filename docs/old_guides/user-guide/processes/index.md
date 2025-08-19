# CATChem Processes

CATChem implements atmospheric chemistry and transport using a modular process-based architecture. Each physical or chemical process is implemented as a separate, self-contained module.

## Process Categories

### 🧪 Chemistry Processes
- **[Atmospheric Chemistry](chemistry.md)** - Gas-phase and aerosol chemistry reactions

### 🏭 Emission Processes
- **[External Emissions](emissions.md)** - Anthropogenic and biogenic emissions
- **[Dust Processes](dust.md)** - Mineral dust emission and transport
- **[Sea Salt](seasalt.md)** - Marine aerosol generation
- **[Plume Rise](plumerise.md)** - Wildfire and point source plume rise

### 🌪️ Transport Processes
- **[Settling](settling.md)** - Gravitational settling with slip correction
- **[Vertical Mixing](verticalmixing.md)** - YSU boundary layer mixing scheme

### 🌧️ Loss Processes
- **[Dry Deposition](drydep.md)** - Surface deposition processes
- **[Wet Deposition](wetdep.md)** - Precipitation scavenging

## Process Architecture

All CATChem processes follow a consistent interface pattern:

```fortran
! Common process interface
type(ProcessType) :: process
call process%init(container, rc)
call process%run(container, rc)
call process%finalize(rc)
```

Each process operates on the central `StateContainer` which manages all chemical species and meteorological fields.

## Configuration

Processes are configured in the main CATChem configuration file:

```yaml
processes:
  - name: "settling"
    scheme: "Stokesscheme"
    enabled: true
    parameters:
      cfl_max: 0.8

  - name: "chemistry"
    scheme: "GOCART"
    enabled: true
```

## See Also

- [Developer Guide - Process Development](../../developer-guide/processes/index.md)
- [Configuration System](../configuration.md)
- [API Reference](../../api/index.md)

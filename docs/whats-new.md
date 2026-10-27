# What's New

## Recent Updates

### Unreleased

#### 🌫️ On-Demand Aerosol Diagnostics
- **Host-Pulled PM and Optics** - A host model can now request particulate
  matter (pm1, pm2_5, pm10), optical properties (extinction, scattering,
  asymmetry, lidar backscatter), and column optical depths (AOD and friends) at
  its own wavelengths, without registering an output diagnostic or advancing a
  chemistry step. The read is pure and deterministic, so it is safe to call from
  inside a step.
- **New pm1 Cut** - pm1 joins pm2_5 and pm10. The cut is defined once, inside the
  aerosol library, so the three cuts cannot drift apart.
- **Runtime Catalogue** - Every quantity is discoverable and self-describing at
  runtime, including its canonical units, so hosts no longer hard-code names or
  scales.
- **Per-Species Attribution** - PM and optical quantities can be attributed to
  individual aerosol species for source-attribution and emission-sensitivity
  work. The species-summed blocks recover the bulk value exactly at every point.
- **Graceful Build Gate** - With the aerosol utility not built in, the endpoints
  still link and report `CATCHEM_QUANTITY_NOT_AVAILABLE` with the reconfigure
  remedy, so a host can detect and skip the feature from the API alone.
- **C and Fortran Surfaces** - A flat C API plus `CATChem_API.F90` wrappers that
  size and allocate the output for the caller. Hosts need no C++ or library
  headers.
- **Performance** - A PM + optics request adds about 0.5 % to a step at the
  1024 x 72 reference tile, inside the 5 % budget.

📖 See [On-Demand Aerosol Diagnostics](user-guide/on-demand-diagnostics.md) and
the [Quantity Registry](core-concepts/on-demand-quantity-registry.md).

### Version 2.0.0 (Latest)

CATChem version 2.0 was released in July 2026 (**[v2.0.0](https://github.com/ufs-community/CATChem/releases/tag/v2.0.0)**) and is integrated into the **[UFS Weather Model](https://github.com/ufs-community/ufs-weather-model)** as a NUOPC component. See **[UFS-Chem](ufschem/index.md)** for details.

#### 🚀 Major Features
- **Modernized Process Infrastructure** - Complete overhaul of process and scheme architecture
- **Stokes Settling Scheme** - Advanced physics-based settling with slip correction
- **Improved Documentation** - Comprehensive user and developer guides with API references
- **YAML Configuration** - Flexible, modern configuration system
- **StateContainer** - All core states use a common interface and are accessible through a common "container"
- **ConfigManager** - Hierarchical YAML configuration loading with file inheritance, environment variable substitution, and validation

#### 🔧 Process Updates
- **Settling Process** - Renamed from `settlingvelocity` to `settling` with new Stokes scheme
- **Modular Schemes** - Support for multiple schemes per process with subdirectories
- **Enhanced Diagnostics** - Process-level diagnostic output and monitoring
- **Template Generator** - Improved code generation for new processes

#### 📚 Documentation
- **MkDocs + Material Theme** - Modern documentation with NOAA color palette
- **API Documentation** - Auto-generated API docs with MkDoxy integration
- **User Guides** - Comprehensive process documentation and tutorials
- **Developer Resources** - Architecture guides and contribution guidelines
- **Consolidated Process Docs** - Process documentation reorganized to one page per implemented process, with verified DOI citations

#### 🛠️ Technical Improvements
- **CMake Modernization** - Updated build system for modular components
- **Error Handling** - Robust error management and validation
- **Code Standards** - Consistent coding practices and style guidelines

## Previous Versions

### Version 1.x
- Legacy settling velocity implementations
- Basic process framework
- Original CCPP and NUOPC integration

---

For detailed release notes and migration guides, visit our **[GitHub releases](https://github.com/UFS-Community/CATChem/releases)**.

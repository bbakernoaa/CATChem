# Emissions Process

The emissions process handles the injection of chemical species into the atmosphere from various anthropogenic and biogenic sources.

## 🏭 Overview

CATChem's emissions process provides:

- **Anthropogenic emissions** - Industrial, mobile, and residential sources
- **Biogenic emissions** - Vegetation and soil emissions
- **Fire emissions** - Wildfire and prescribed burning
- **Point sources** - Large stationary sources with stack parameters

## ⚙️ Configuration

### Basic Configuration

```yaml
emissions:
  enabled: true

  # Emission source types
  sources:
    anthropogenic:
      enabled: true
      inventory: "NEI2017"
      file: "anthro_emis_2017.nc"
      temporal_profile: "monthly"

    biogenic:
      enabled: true
      model: "MEGAN3"
      landuse_file: "landuse.nc"
      temperature_dependent: true

    fires:
      enabled: true
      file: "fire_emis_daily.nc"
      plume_rise: true

    point_sources:
      enabled: true
      file: "point_sources.nc"
      stack_parameters: true
```

## 🌱 Emission Types

### Anthropogenic Emissions
- Mobile sources (on-road, off-road, aviation, marine)
- Stationary sources (power plants, industrial facilities)
- Area sources (residential, commercial, agricultural)

### Biogenic Emissions
- Isoprene and monoterpenes from vegetation
- Soil NOx emissions
- Sea salt and DMS from oceans

### Fire Emissions
- Wildfire emissions with real-time data
- Prescribed burning
- Agricultural burning

## 📊 Diagnostics

```yaml
diagnostics:
  emission_rates:
    - "EMI_NOX"      # NOx emission rate
    - "EMI_VOC"      # VOC emission rate
    - "EMI_CO"       # CO emission rate
    - "EMI_SO2"      # SO2 emission rate

  source_attribution:
    - "ANTH_NOX"     # Anthropogenic NOx
    - "BIOG_ISOP"    # Biogenic isoprene
    - "FIRE_CO"      # Fire CO emissions

  spatial_totals:
    - "TOTAL_NOX"    # Total NOx by grid cell
    - "TOTAL_PM25"   # Total PM2.5 emissions
```

## 🔧 Processing Features

### Temporal Processing
- Hourly, daily, monthly temporal profiles
- Diurnal cycles for different source categories
- Seasonal variations for biogenic emissions

### Spatial Processing
- Grid-to-grid mapping and interpolation
- Point source allocation to grid cells
- Vertical distribution profiles

### Chemical Speciation
- VOC speciation profiles
- PM2.5 and PM10 composition
- Toxic species mapping

## 🚀 Performance Optimization

### File I/O Optimization
- Efficient NetCDF reading with chunking
- Memory-mapped file access for large inventories
- Compressed emission files support

### Computational Efficiency
- Sparse emission processing (zero-emission grid cells skipped)
- Vectorized operations for biogenic calculations
- Parallel processing of emission sectors

## 🔍 Technical Implementation

### Data Structures
```fortran
type :: emission_state_type
  real(r8), allocatable :: anthro_emis(:,:,:)    ! Anthropogenic emissions
  real(r8), allocatable :: biog_emis(:,:,:)      ! Biogenic emissions
  real(r8), allocatable :: fire_emis(:,:,:)      ! Fire emissions
  real(r8), allocatable :: point_emis(:,:,:)     ! Point source emissions
end type
```

### Integration Points
- Pre-chemistry emission injection
- Plume rise calculations for point sources
- Surface flux boundary conditions

## 📚 Quality Assurance

### Validation Checks
- Mass conservation verification
- Emission total comparisons with inventories
- Spatial distribution verification

### Error Handling
- Missing file detection and fallback options
- Invalid emission rate checking
- Temporal consistency validation

## 🔗 Related Documentation

- **[External Emission API](../../api/CATChem/_external_emission_process___mod_8_f90/)** - Detailed API reference
- **[Configuration Guide](../configuration.md)** - General configuration principles
- **[Input Files](../input-files.md)** - Emission file formats and requirements

---

*Emissions are fundamental to atmospheric chemistry modeling. Proper configuration ensures accurate representation of source contributions to air quality.*

# On-Demand Quantity Registry

This page is the normative reference for the quantities the on-demand API can
answer, and for the exact definition of the particulate-matter cuts. It
complements the [user guide](../user-guide/on-demand-diagnostics.md), which
covers how to make a request.

A host should not hard-code this list: the catalogue is enumerable at runtime
(`catchem_ondemand_quantity_count_checked` / `..._name_at_checked` /
`..._units_checked`), and every name reports its canonical units. The table
below documents what each quantity means.

## Quantity shapes

Every quantity has one of four shapes, which fixes how many values a request
returns and how they are laid out:

| Shape | Values per wavelength | Layout |
|-------|-----------------------|--------|
| 3D field | `ncols * nlevels` | `(cell, level)`, cell fastest |
| 2D column | `ncols` | `(cell)` — level-0 column or surface field |
| Optics coefficient | `ncols * nlevels` | band-major: all `(cell, level)` of band 0, then band 1, … |
| Optics AOT | `ncols` | `(cell)` per band |

Within every shape the ordering is column-major with `cell` varying fastest,
so a host's Fortran `(:, :, :)` or C array of `(cell, level, …)` maps with no
transpose. Requests for several names are **name-major**: the block for name
`i` follows the blocks of names `0 … i-1`, so a host can index a block it knows
by name.

## Diagnostic quantities

| Name | Shape | Units | Meaning |
|------|-------|-------|---------|
| `pm1` | 3D | `kg m^-3` | Particulate matter with dry diameter at or below 1 µm |
| `pm2_5` | 3D | `kg m^-3` | … at or below 2.5 µm |
| `pm10` | 3D | `kg m^-3` | … at or below 10 µm |
| `mass_concentration` | 3D | `kg m^-3` | Total aerosol mass concentration |
| `number_concentration` | 3D | `particles m^-3` | Aerosol number concentration |
| `surface_area_density` | 3D | `m^2 m^-3` | Aerosol surface area per volume of air |
| `aerosol_liquid_water` | 3D | `kg m^-3` | Aerosol liquid water content |
| `settling_velocity` | 3D | `m s^-1` | Gravitational settling velocity |
| `surface_mass` | 2D | `kg m^-3` | Column-total mass at the surface level |
| `column_mass` | 2D | `kg m^-2` | Column-integrated mass |
| `surface_pm2_5_mass` | 2D | `kg m^-3` | Column pm2.5 total at the surface level |
| `column_pm2_5_mass` | 2D | `kg m^-2` | Column-integrated pm2.5 |

## Optical quantities

Requested with a wavelength array; the wavelength is **free-space metres**.

| Name | Shape | Units | Meaning |
|------|-------|-------|---------|
| `extinction` | coefficient | `m^-1` | Aerosol extinction coefficient |
| `scattering` | coefficient | `m^-1` | Scattering coefficient |
| `backscatter` | coefficient | `m^-1 sr^-1` | Backscatter coefficient |
| `asymmetry` | coefficient | fraction | Scattering asymmetry parameter |
| `lidar_backscatter` | coefficient | `m^-1 sr^-1` | Lidar backscatter coefficient |
| `single_scattering_albedo` | coefficient | fraction | Scattering divided by extinction |
| `aod` | AOT | dimensionless | Aerosol optical depth (extinction column integral) |
| `scattering_aot` | AOT | dimensionless | Scattering column integral |
| `fine_mode_aod` | AOT | dimensionless | Fine-mode extinction column integral |
| `fine_mode_scattering_aot` | AOT | dimensionless | Fine-mode scattering column integral |
| `pm2_5_aod` | AOT | dimensionless | pm2.5 extinction column integral |
| `pm2_5_scattering_aot` | AOT | dimensionless | pm2.5 scattering column integral |

`single_scattering_albedo` is **derived, not a library slot**: it is the
scattering coefficient divided by the extinction coefficient. Where extinction
is zero — a column with no aerosol mass — there is no albedo, so the point
reports `NotInSource` (4) with a value of 0 rather than an ok-flagged
divide-by-nothing. Every other optical quantity is read straight from the
library's optics slab.

An AOT is the **column integral** of its coefficient over the host's bound
layer thicknesses (`BXHEIGHT`, or the hydrostatic thickness derived from `DELP`
when `BXHEIGHT` is absent), over the host's local tile only. It is therefore
the thickness-weighted sum of the per-level coefficients returned by the same
call — an identity the test suite asserts directly, so an AOT is never an
independent second model of the atmosphere.

## Particulate-matter cut definition

A PM cut is an **all-or-nothing modal cut**, evaluated per configured modal
species:

> A modal species contributes its *entire* mass to a cut when its configured
> `dry_particle_diameter` is **at or below** the cut diameter, and contributes
> **nothing** when it is above.

Concretely, with the three cuts at 1 µm, 2.5 µm and 10 µm:

| Modal dry diameter | `pm1` | `pm2_5` | `pm10` |
|--------------------|-------|---------|--------|
| 0.8 µm | included | included | included |
| 2.0 µm | excluded | included | included |
| 3.0 µm | excluded | excluded | included |

Two properties follow, and both are asserted in the test suite:

- **A cut is a sum over modal types**, not a weighted average. `pm2_5` for a
  column carrying a 0.8 µm and a 2.0 µm species is the sum of both masses.
- **A point above the cut returns a real zero with an available status**, not
  an unavailable status: an all-mass-above-cut column genuinely has no
  sub-micron material.

This is a deliberately different definition from the one some coupled drivers
use, where a host that carries aerosol in explicit size bins weights a bin
whose range *straddles* a cut by the fraction of its geometric range below the
cut. The two products coexist and neither is a parity target for the other:

- **On-demand PM** (this API) is the aerosol library's modal cut above. A
  modal species is indivisible, so a species cannot be partially inside a cut.
- **Legacy coupled-driver PM** (`pm25` / `pm10` in the NUOPC interface) applies
  the geometric-range fraction per transport bin, so a species whose bin range
  spans a cut contributes only part of its mass.

If you are migrating between them, expect a difference exactly for species
whose size distribution straddles a cut diameter, and expect them to agree
where every bin sits wholly on one side.

## Per-species attribution and its identity

When a host names a species, each named species gets its own block, computed by
running the library kernels with that species' mass and the others zeroed. Both
kernels are linear in species mass, so:

$$\text{value}(\text{all species}) = \sum_{s} \text{value}(s)$$

pointwise, for every quantity and every grid point. This identity is a
property of the implementation, not an approximation — it is what makes
per-species attribution trustworthy for source attribution and
emission-sensitivity work, and the test suite asserts it for both a
particulate-matter cut and an optical coefficient.

Availability codes describe whether a value *exists*, not where it came from. A
species with neither a sourced optics curve nor a modelled optical contribution
reports `NotInSource`; a species the analytical solver can still model keeps
its computed value and reports `Available`, even though it has no curve in the
library's pinned tables. Flagging such a species as absent would hide a real
optical contribution and break the sum identity above.

## Build gate

The registry is static and answers even when the aerosol utility is not built
into the library, but the *compute* path is present only with
`-DCATCHEM_BUILD_EXAERO=ON`. With the utility absent the count endpoint answers
`0` with `CATCHEM_QUANTITY_NOT_AVAILABLE` so a host can skip the feature from
the API alone, rather than discovering the gap one call at a time.
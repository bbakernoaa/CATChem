# CATChem CCPP process drivers

CATChem exposes one individually schedulable CCPP scheme per process: Dust, SeaSalt,
DryDep, WetDep, Settling, SO4Chem, CarbChem, Photolysis, and GasChem. A generated CCPP
cap calls the process selected by its host suite. CATChem does not run an implicit full
internal schedule from these process drivers. CCPP-SCM is used only by the test workflow;
it is not a production dependency of CATChem.

## CCPP entry points and ownership

Each `drivers/ccpp/catchem_<process>.meta` declares `<process>`'s four entry points:
`catchem_<process>_driver_register`, `_init`, `_run`, and `_final`. Register returns the
suite's allocated `ccpp_constituent_properties_t` entries. Init binds the generated
constituent-property pointers to a context. Run accepts the full
`ccpp_constituents(horizontal_loop_extent, vertical_layer_dimension,
number_of_ccpp_constituents)` array, explicit process meteorology, context generation,
interval ID/token, coupling site, subcycle and duration. Final releases that process's
context participation. The generated CCPP cap supplies its normal `errmsg/errflg`
lifecycle contract; scheme run calls also return `errcode/errmsg` as declared in metadata.

The test SDFs keep the host `phys_ps` and `phys_ts` groups unchanged and schedule CATChem in
two separate groups: `catchem1` for surface emissions/removal after `phys_ps`, and `catchem2`
for photolysis and chemistry after `phys_ts`. The host calls those groups at the corresponding
integration points and synchronizes its constituent array between them. Apply each staged
concentration tendency immediately after its process. Runtime-disabled processes return without
importing fields, and a tendency update with no staged contribution is a no-op. The `_ps` test
suites change prescribed surface forcing while reusing the same process drivers.

GasChem requires fresh CATChem photolysis rates. The test suite named `PhotolysisGasChem`
expresses that prerequisite explicitly by running Photolysis before GasChem; there is no
standalone GasChem suite that implicitly runs the Photolysis process.

Create a CATChem context through `catchem_ccpp_context` before registration, using a
unique context ID and generation per host instance. The host owns the full CCPP constituent
state and tendency arrays. CATChem owns only context-local canonical state and reusable
baseline, working and tendency buffers. The host must merge all scheme and host properties
and allocate the shared arrays before process initialization. The adapter maps species by
CCPP standard name and checks units, mixing-ratio type, air basis, vertical dimension and
advected status. Species `name` is the default standard name; set `ccpp_standard_name`
only when the host's actual canonical standard name differs. Do not assign private
CATChem-only constituents to duplicate an existing tracer.

All process schemes use the full constituent registry. `drivers/ccpp/*.meta` is the
authoritative argument table; the table below summarizes required physical input families.
Units are the metadata units and must be supplied by the host or derived using the
documented adapter recipe.

| Process | Run metadata fields (`standard_name` and units) | Derived input state | Constituent effect |
|---|---|---|---|
| Dust | `air_pressure` (Pa), `air_temperature` (K), `specific_humidity` (kg kg-1), `air_pressure_at_interface` (Pa), `lake_area_fraction` (frac), `land_area_fraction` (frac), `sea_land_ice_mask` (flag), `surface_snow_area_fraction_over_land` (frac), `volume_fraction_of_condensed_water_in_soil` (frac), `normalized_soil_wetness` (frac), `surface_skin_temperature` (K), `x_wind_at_10m` / `y_wind_at_10m` (m s-1) | Scheme inputs derive from GFS v17 SCM fields as recorded in `dust.yml`. | emission tendency |
| SeaSalt | `air_pressure` (Pa), `air_temperature` (K), `specific_humidity` (kg kg-1), `sea_area_fraction` / `sea_ice_area_fraction_of_sea_area_fraction` (frac), `latitude_in_degree` (degree_north), `longitude_in_degree` (degree_east), `surface_friction_velocity` (m s-1), interface pressure (Pa), lake/land/snow/soil fractions, skin temperature (K), 10 m winds (m s-1) | Surface fractions and meteorology are taken from the selected SCM case; see `seasalt.yml`. | emission tendency |
| DryDep | `air_pressure` / `surface_air_pressure` (Pa), `air_temperature` (K), `specific_humidity` (kg kg-1), interface pressure (Pa), `geopotential_at_interface` (m2 s-2), cloud fraction (frac), skin temperature (K), latitude / longitude (radian), friction velocity (m s-1), sensible heat (W m-2), PBL thickness (m), vegetation type (index), LAI (none), snow/lake/land/sea-ice fractions, shortwave (W m-2), roughness (cm), normalized soil wetness, 10 m winds (m s-1) | RH, moist density, layer heights and solar cosine follow the declared recipes in `drydep.yml`. | removal tendency |
| WetDep | `air_pressure` (Pa), `air_temperature` (K), `specific_humidity` (kg kg-1), ice/liquid large-scale precipitation flux (kg m-2 s-1), coupled total/convective precipitation accumulations (m), interface pressure (Pa), lake/land/snow/soil fractions, skin temperature (K), 10 m winds (m s-1) | Interface flux interpolation and large-scale/convective amount split are described in `wetdep.yml`. | removal tendency |
| Settling | `air_pressure` (Pa), `air_temperature` (K), `specific_humidity` (kg kg-1), `geopotential_at_interface` (m2 s-2), interface pressure (Pa), lake/land/snow/soil fractions, skin temperature (K), 10 m winds (m s-1) | Interface height, RH and moist density are derived by the driver from host thermodynamics/geopotential; see `settling.yml`. | sedimentation tendency |
| SO4Chem | `air_pressure` (Pa), `air_temperature` (K), `specific_humidity` (kg kg-1), interface pressure (Pa), geopotential (m2 s-2), cloud fraction (frac), sensible heat (W m-2), latitude / longitude (degree_north / degree_east), PBL height (m), friction velocity and 10 m winds (m s-1), sea-land-ice flag, roughness (cm) | Derived thermodynamics and surface fields follow `so4chem.yml`. | sulfur gas/aerosol tendencies |
| CarbChem | `air_pressure` (Pa), `air_temperature` (K), `specific_humidity` (kg kg-1), interface pressure (Pa) | Dry-air density and layer geometry are derived from host thermodynamics and pressure; see `carbchem.yml`. | carbonaceous aerosol aging tendency |
| Photolysis | `air_pressure` (Pa), `air_temperature` (K), `specific_humidity` (kg kg-1), interface pressure (Pa), interface geopotential (m2 s-2), cloud fraction (frac), latitude / longitude (degree_north / degree_east), PBL height (m), friction velocity and 10 m winds (m s-1), sea-land-ice flag, roughness (cm); loop counter/extent are indices | Host clock and SCM ozone profile are explicit; no synthetic default ozone profile is substituted. | publishes derived photolysis rates; no concentration tendency |
| GasChem | Photolysis fields above plus `surface_upward_sensible_heat_flux_for_chemistry_coupling` (W m-2) | Fresh, context/time-matched Photolysis rates are required when the mechanism consumes them. | gas and aerosol chemistry tendency |

The process case manifests in `tests/ccpp-scm/cases/` carry the exact per-case
input list and basis. Use those manifests and metadata rather than treating this summary
as a replacement for a host field audit.

## Constituents, units and air basis

For gas species, the adapter supports dry-air ppmv and mass mixing ratios; for aerosol
species, it supports mass mixing ratios. `amount_type` and `air_basis` must be explicit
when supplied in CATChem species configuration. Supported bases are `dry`, `moist`, and
`total_with_condensate`. Gas mass/volume conversion uses molecular weight and the host
dry-air density. Aerosol mass mixing ratio is converted to the process-native dry-air
micrograms per kilogram. Moist/total host bases are converted using both host-basis and
dry-air densities. Number concentration, ambiguous units, conflicting duplicate standard
names and incomplete physical metadata are rejected before publication.

The full framework array remains in the host's constituent slot order. CATChem resolves
each configured species by standard name, so chemistry slots may be permuted or separated
by unrelated tracers. Unmapped state and pre-existing tendency contributions remain
untouched. Examples for the 23-species demonstration and independently derived conversion
checks live in the [CCPP example README](https://github.com/bbakernoaa/CATChem/blob/main/examples/ccpp/README.md)
and the [constituent conformance manifest](https://github.com/bbakernoaa/CATChem/blob/main/tests/ccpp-scm/cases/constituent-conformance.yml).

## State updates, clocks and rate dependencies

For a concentration-changing process, CATChem computes a candidate state and stages the
interval-average tendency. It does not advance or overwrite host prognostic constituents.
The host applies each staged tendency once at its chosen interstitial update boundary;
later schemes then read the resulting host array. The shared interval token, subcycle and
site distinguish invocations and prevent duplicate application. The host's clock remains
authoritative; duration is passed in seconds for every call and is not advanced by CATChem.

Photolysis writes context-local derived-rate products. GasChem requiring photolysis checks
that rates are present and fresh for the current context/time; stale or absent rates fail
before tendency publication. Failed calls publish neither partial tendencies nor newly
valid derived products. Do not run unrelated schemes through a legacy all-process wrapper.

## Supported build and capability limits

The CCPP-SCM test tuple and one-suite CI runner are documented in the
[CCPP-SCM README](https://github.com/bbakernoaa/CATChem/blob/main/tests/ccpp-scm/README.md). Its source and extracted input
checksums are locked in `tests/ccpp-scm/source-lock.json` and `tests/ccpp-scm/data-lock.json`.
CI runs the GFS v17 emission/removal suite on GABLS3. Broader historical case evidence remains in
the acceptance report; neither establishes compatibility with every host model or suite.

`CATCHEM_CCPP_PHYSICAL_RESTART_SUPPORTED` and
`CATCHEM_CCPP_CONCURRENT_CONTEXT_EXECUTION_SUPPORTED` are both zero. Cold start is
supported; concentration-only or mid-interval restore is rejected. Context state is
isolated, but hosts must serialize CATChem native process calls because some science
libraries use process-global resources.

`CATCHEM_CCPP_TEST_STUBS=ON` is a contract-only test configuration. It verifies interfaces
and boundary behavior but is not a real CCPP build or science result. The old
`ccpp_catchem_interface` monolithic implementation is compatibility-only and is not the
process-driver API documented here.

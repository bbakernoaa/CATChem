# On-Demand Aerosol Diagnostics

CATChem can answer aerosol questions *on demand*: a host model asks for the
particulate matter or optical properties of its own local tile, right now, and
gets values back without running a chemistry step and without registering an
output field in advance.

The classic CATChem model registers a diagnostic, steps the physics, and reads
the field afterwards. The on-demand API inverts that: the host pulls. Nothing is
written to output, nothing is scheduled, and the prognostic state is never
advanced — the read assembles a snapshot of the bound state, computes into
scratch, and copies the result out.

## When to use it

Use the on-demand API when a host needs a value that a coupling or a diagnostic
step already has in hand:

- a radiation step wants the aerosol optical depth for the same column it just
  stepped;
- an emission-sensitivity or source-attribution study needs PM2.5 broken down
  by species;
- an interactive or verification path wants pm1 / pm2_5 / pm10 without adding a
  field to the model's output stream.

Prefer the ordinary diagnostic interface when the quantity should appear in the
model's history, be written by a parallel I/O task, or be produced once per
step regardless of whether anything consumes it.

## Availability

The feature requires the aerosol utility to be built in:

```bash
cmake -DCATCHEM_BUILD_EXAERO=ON ...
```

With the utility absent the endpoints still link — they are part of the
published surface — and answer `CATCHEM_QUANTITY_NOT_AVAILABLE` (18) with a
message naming the reconfigure flag. A host can therefore detect the missing
feature from the API alone and skip it, rather than discovering it per call.

## Enabling it in the model configuration

```yaml
simulation:
  name: my_model
  species_filename: /path/to/host_species.yml

diagnostics:
  on_demand:
    enabled: true
    exaero_config: /path/to/exaero_species.yml
```

`exaero_config` points at the aerosol library's own species configuration (the
same document the settling engine consumes). Each host aerosol species must
carry a `__mie_name` matching a modal species in that file; a host aerosol
whose `__mie_name` is absent from it is a configuration error, reported loudly
at first use rather than silently excluded.

## Discovering the catalogue

Before requesting anything, a host can enumerate what is available. The
catalogue is static, so it answers even before any state is bound:

```c
int count = 0;
catchem_ondemand_quantity_count_checked(core, &count);   /* 24 quantities */

char name[64], units[64];
catchem_ondemand_quantity_name_at_checked(core, 0, name, sizeof(name));
catchem_ondemand_quantity_units_checked(core, name, units, sizeof(units));
```

Every quantity reports canonical units (for example `kg m^-3` for the PM cuts,
`m^-1` for a coefficient, dimensionless for an albedo), so a host never has to
hard-code a scale.

## Requesting bulk particulate matter

```c
const char* names[3] = {"pm1", "pm2_5", "pm10"};
double values[3 * ncols * nlevels];
int    status[3 * ncols * nlevels];

int rc = catchem_ondemand_get_pm_checked(core, names, 3,
                                         values, status,
                                         3 * ncols * nlevels,
                                         3 * ncols * nlevels);
```

Values come back **name-major**: the block for `names[i]` starts at
`i * ncols * nlevels` in `(cell, level)` order (column-major, cell fastest),
matching the Fortran layout a host already keeps its state in.

`status` is optional (pass `NULL`), but reading it is how a host tells a real
zero from an absent value — see [Availability codes](#availability-codes).

## Requesting optical properties and AOD

```c
const char* names[3] = {"extinction", "scattering", "aod"};
const double wavelengths[1] = {5.5e-7};          /* 550 nm, free-space metres */

double values[2 * ncols * nlevels + ncols];       /* two coefficients + one AOD */
int    status[2 * ncols * nlevels + ncols];

int rc = catchem_ondemand_get_optics_checked(core, names, 3,
                                             wavelengths, 1,
                                             values, status,
                                             sizeof(values)/sizeof(double),
                                             sizeof(status)/sizeof(int));
```

Blocks are again name-major, but the per-name length depends on the quantity's
kind: a **coefficient** (extinction, scattering, backscatter, asymmetry, …)
occupies `num_wavelengths * ncols * nlevels` points laid out band-major — all
`(cell, level)` points of band 0, then band 1, and so on. A **column
integral** (`aod` and the other AOT quantities) occupies
`num_wavelengths * ncols`. `values_length` is checked against the sum over the
requested names, and an undersized buffer is rejected before any value is
trusted.

Wavelengths are **free-space metres**, not micrometres and not nm. A value
outside the configured band range is **rejected, never clamped**: the call
returns `CATCHEM_QUANTITY_NOT_AVAILABLE` and the message names the offending
wavelength together with the valid range, so a host can correct the request
instead of silently receiving an extrapolation.

## The general form

`catchem_ondemand_get_quantity_checked` reaches any catalogue quantity:

```c
/* Diagnostic name: wavelengths are ignored. */
catchem_ondemand_get_quantity_checked(core, "pm2_5", NULL, 0, NULL, 0,
                                      values, status, vlen, slen);

/* Optics name: wavelengths required. */
catchem_ondemand_get_quantity_checked(core, "extinction", wavelengths, 1, NULL, 0,
                                      values, status, vlen, slen);
```

## Per-species attribution

Passing a species list attributes the quantity to individual species — one
output block per name, in the order requested:

```c
const char* species[2] = {"dust_aero", "sea_salt_aero"};
double values[2 * ncols * nlevels];
int    status[2 * ncols * nlevels];

int rc = catchem_ondemand_get_quantity_checked(core, "pm2_5", NULL, 0,
                                              species, 2,
                                              values, status, vlen, slen);
```

A `NULL` list with count zero is the bulk (all-species) value. Attribution runs
the same kernels once per named species, so **the species-summed blocks recover
the bulk value at every point** — that identity is part of the contract, not a
rounding coincidence, and it is asserted in the test suite. A subset therefore
costs only the species it names.

Species may be named either by the host's own short name (`dust_aero`) or by
the modal label (`DU`); both resolve through the `__mie_name` join. An unknown
name fails with `CATCHEM_INVALID_INDEX` and the message names it, before any
compute runs.

## Availability codes

Every delivered value carries a status, and the distinction the codes draw is
between *a value that is zero* and *a value that does not exist*:

| Code | Meaning |
|------|---------|
| 0 `Available` | A value computed from the configured curves. |
| 2 `Interpolated` | Between grid points; the value is an interpolation. |
| 4 `NotInSource` | No value exists here; the slot holds 0. |

`NotInSource` is reported for a species that has neither a sourced optics curve
nor a modelled optical contribution, and for a point whose slab read is
non-finite. A curve-less species that the analytical solver *can* still model
keeps its computed value and reports `Available` — the code describes whether a
value exists, not whether it came from a table or from a solver, and hiding a
real modelled value would break the summed-equals-bulk identity above.

## Purity and determinism

A read never advances the model. The prognostic concentration buffer is
byte-identical before and after a full sweep of PM, optics, and attribution
requests, and repeating a request with unchanged inputs returns byte-identical
outputs. A host may therefore pull values from inside a step, or pull the same
value twice for a consistency check, without affecting the trajectory.

## Performance

At the reference tile (1024 columns x 72 levels) one PM request (three cuts)
plus one optics request (extinction and AOD at 550 nm) added **0.487 %** to a
configured settling step on the development machine — inside the 5 % budget.
The measurement is reproducible:

```bash
OMP_PROC_BIND=false ./test_catchem_api_on_demand --bench 15
```

Timing is opt-in precisely so it never decides a CTest pass/fail on a loaded
or contended machine. If per-species attribution ever dominates a host's
profile, the documented remedy is to request the bulk value plus only the
species subset actually needed — not to change the contract.

## Fortran hosts

`CATChem_API.F90` mirrors every endpoint with a friendly wrapper that takes
Fortran arrays and allocates the output for you:

```fortran
real(c_double), allocatable :: values(:)
integer(c_int), allocatable :: status(:)
integer :: rc

call model%get_ondemand_pm(['pm1', 'pm2_5', 'pm10'], values, status, rc)

call model%get_ondemand_optics(['extinction', 'scattering', 'aod'], &
                                [5.5e-7_c_double], values, status, rc)

call model%get_ondemand_quantity('pm2_5', values, rc, &
                                 species=['dust_aero', 'sea_salt_aero'])
```

The wrappers size each block from the catalogue's own shape query, so a host
never has to know whether a name is a coefficient or a column integral. A
failed call leaves the reason in `model%last_error`.

## Error handling

Every endpoint has a plain form (no return value) and a `_checked` form
returning a status code; prefer the checked form. On failure the message is
available through `catchem_get_last_error`, and the Fortran wrappers copy it
into `last_error`. The codes a host is most likely to meet:

| Code | Constant | Cause |
|------|----------|-------|
| 1 | `CATCHEM_NULL_ARGUMENT` | A required pointer or a positive-length output was missing. |
| 4 | `CATCHEM_EXTENT_MISMATCH` | The output buffer cannot hold the requested blocks; the message states the required extent. |
| 5 | `CATCHEM_INVALID_INDEX` | Unknown quantity, unknown species, or a name from the wrong family. |
| 8 | `CATCHEM_INVALID_STATE` | No state is bound, or a required field is missing. |
| 17 | `CATCHEM_PHYSICAL_VALIDATION_FAILURE` | A bound aerosol mixing ratio is negative or non-finite. |
| 18 | `CATCHEM_QUANTITY_NOT_AVAILABLE` | Utility not built, not configured, or a wavelength outside the band range. |

See [Quantity registry and cut definitions](../core-concepts/on-demand-quantity-registry.md)
for the full quantity list and the exact particulate-matter cut rule.
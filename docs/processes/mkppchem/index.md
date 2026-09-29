# MKPP Chemistry (mkppchem) Process Documentation

## Overview

`mkppchem` is an opt-in native C++ chemistry process that integrates a
multiphase atmospheric mechanism generated ahead of time by MKPP (the NOAA-EMC
Model Kernel Production Pipeline). It is an alternative to the hand-written
`so4chem` + `carbchem` process pair: instead of interpreting a mechanism at
runtime, the solver kernels for a pinned mechanism definition are generated and
compiled into the binary, and the process drives them through a Kokkos staging
bridge.

- **Namespace / Class**: `catchem::MkppChemProcess : public catchem::ProcessInterface`
- **Header Location**: `src/process/mkppchem/catchem_process_mkppchem.hpp`
- **Source Location**: `src/process/mkppchem/catchem_process_mkppchem.cpp`
- **Build gate**: `CATCHEM_BUILD_MKPP=ON` (see
  [Build System](../../user-guide/build-system.md) and
  [MKPP Platform Support](../../user-guide/mkpp-platform-support.md))

Two mechanisms currently ship compiled in the default build:

| Mechanism | Species | Reactions | Photolysis | Equilibrium inputs |
|---|---|---|---|---|
| `gocart` (default) | 26 (OH, NO3, H2O2, DMS, SO2, SO4, carbonaceous/dust/sea-salt aerosol bins, nitrate block) | 28 | 1 (`jh2o2`) | T, RH, cloud liquid water |
| `chapman` | 4 (O, O2, O3, M) | 4 | 2 | none (rates are environment-independent) |

## Architecture & How It Works

`MkppChemProcess::run` executes a fixed per-timestep pipeline on the Kokkos
execution space (host with OpenMP, or GPU when the build targets one):

1. **Field acquisition**: the bound device views for `T`, `RH`, `PMID`,
   `AIRDEN_DRY` (and `CLDF`/`QV` when cloud water is derived) are synced to
   device. All are required to be current for the import generation — the
   execution plan validates their presence before `run()` is ever called.
2. **Error reset**: the per-cell error view is set to the no-error sentinel.
   Every validation below flags the *first* failing reason per cell and never
   overwrites it, so the reported cause is the earliest, not a downstream
   artifact.
3. **Physical-admissibility validation**: `T > 0`, `PMID > 0`,
   `0 <= RH <= 1`, `AIRDEN_DRY > 0`, all finite. A flagged cell is skipped by
   the solve, never solved with a fabricated value.
4. **Gather + unit conversion**: CATChem's mixing ratios (mol/mol) are
   converted to the molar concentrations (mol/m³) the solver consumes,
   `conc × ρ_molar`, where `ρ_molar = AIRDEN_DRY / 0.0289644 kg/mol`. The
   species slot order is the mechanism's declaration order, resolved through a
   device-side slot → CATChem-index map.
5. **Environment fill**: temperature and humidity columns are written into the
   solver's equilibrium-input buffer, plus a cloud-water column when the
   mechanism exposes one (see below). A mechanism with no equilibrium inputs at
   all (chapman) skips the fill entirely.
6. **Pre-solve validation**: every staged concentration must be finite and
   non-negative; a bad cell is flagged and skipped, never solved on garbage.
7. **Photolysis refresh**: each photolysis rate is re-read from its bound
   diagnostic on *every* timestep through the device view — never cached
   across steps — or, in smoke mode, re-applied from the definition's
   host-forcing constants.
8. **Solve**: the generated Rosenbrock kernels run per cell. The bridge probes
   the generated `integrate` overloads at compile time and selects the one the
   mechanism actually exposes (equilibrium-input form, or the plain
   `(dt, state, jvals)` form).
9. **Error reduction**: a single reduction over the per-cell error view
   reports the lowest-index failing cell (stable across ranks and reruns), the
   failure reason, and the offending species when identifiable. Any failing
   cell is fatal — the run never continues on a silently skipped column.
10. **Tendencies**: the solver's per-cell change is rate-normalised with the
    same molar density and published into the packed `mkppchem_tendency`
    diagnostic when diagnostics are enabled.
11. **Scatter + inverse conversion**: solved molar concentrations are divided
    by `ρ_molar` and written back to the mixing-ratio state — mapped species
    only. CATChem tracers the mechanism does not advance are bit-for-bit
    unchanged.

## Configuration

Probed under both `processes/mkppchem/` and `process/mkppchem/`, matching the
`gaschem` convention.

```yaml
processes:
  mkppchem:
    activate: true
    mechanism: "gocart"          # any mechanism compiled into the build
    definition_file: "parm/config/openatmos/gocart/mechanism.yaml"
    # initial_step_seed: 0.1     # optional runtime ramp-seed override (see below)
    diagnostics: true
    diag_species: []             # empty = every advanced species
    photolysis_smoke_mode: false # isolated-mechanism testing only
    cloud_liquid_water:
      mode: auto                 # auto | host_field | derived
      # host_field: QLI          # required when mode is host_field
```

| Key | Type | Default | Constraint |
|---|---|---|---|
| `activate` | bool | `false` | — |
| `mechanism` | string | `gocart` | must name a mechanism compiled into the build |
| `definition_file` | string | the selected mechanism's shipped definition | must exist and be the definition the mechanism was generated from |
| `initial_step_seed` | float | the seed baked into the compiled artifact | optional runtime override of the adaptive-ramp seed (first substep = `dt * seed`); a finite positive fraction, else fatal at init. A larger seed takes fewer mandatory substeps per host step (faster, slightly lower accuracy). Configured on an artifact predating the runtime seed member fails at init, never silently ignored |
| `diagnostics` | bool | `false` | — |
| `diag_species` | list | all advanced species | each entry must resolve to a mapped species |
| `cloud_liquid_water/mode` | string | `auto` | `auto` \| `host_field` \| `derived` |
| `cloud_liquid_water/host_field` | string | probe list | required when `mode: host_field` |
| `photolysis_smoke_mode` | bool | `false` | logs a prominent warning; never for a physics run |

An unknown key under the block, or a `mkppchem` scheduled alongside `so4chem`
or `carbchem` (which would double-apply the same species' chemistry), is
rejected at configuration load — the earliest possible gate, before any process
is constructed.

### Rejected configurations (all fatal, all before any timestep)

| Condition | Message names |
|---|---|
| Unknown key under the block | the key |
| `mkppchem` with `so4chem`/`carbchem` in one process order | both processes and the shared species |
| `mechanism` not compiled into the build | the request and the available list |
| `definition_file` missing, or declaring a different mechanism, or a different digest | the configured path and the compiled definition |
| `mode: host_field` with the named field absent | the field name |
| `mode: derived` with a derivation input absent | the missing input |
| `photolysis_smoke_mode: true` with photolysis also scheduled or bound | the contradiction |
| `initial_step_seed` not a finite positive fraction (garbage, `0`, negative, `nan`, `inf`) | the offending value |
| `initial_step_seed` set while the compiled artifact predates the runtime seed member | the mechanism name and the remedy (regenerate or remove the option) |
| A mechanism with photolysis reactions, no smoke mode, no photolysis source | the unresolved labels |

Every fatal follows the `FATAL ERROR: mkppchem <context>: <reason>` convention
and is byte-identical on every rank, so a distributed failure is diagnosable
from any single rank's log. `init()` logs the full effective setup at
informational verbosity: selected mechanism (and whether the default was
used), definition path and provenance digest, species/reaction/photolysis/
equilibrium counts, the slot↔index↔name mapping triplets, unadvanced tracers,
J-value labels and their source, required met fields, cloud-water origin, and
staging footprint.

## Mechanism Selection and the Single-Parse Design

The runtime never parses a mechanism YAML. The structural facts of each
compiled mechanism (species order, photolysis labels and host-forcing
constants, reaction counts, equilibrium-channel indices, provenance digest)
are baked into one aggregated generated metadata header,
`src/process/mkppchem/catchem_mkpp_mechanism_meta.hpp` — a `Facts` struct per
mechanism plus the definition table — by
`tools/openatmos/generate_catchem_species.py`. The same generator emits the
matching CATChem species configuration under `tests/Configs/Mkpp/`, and the
shipped OpenAtmos definitions live under `parm/config/openatmos/<mechanism>/`.
Adding a mechanism means adding its definition to the generator command:
never a new header file, and never a hand-edited traits specialization or
registry entry.

What this buys:

- **One source of truth.** The runtime species state and the solver kernels
  come from the same generated facts, so they cannot silently diverge.
- **A CI gate instead of a runtime parse.**
  `tools/openatmos/verify_mechanism_provenance.py` re-parses the pinned
  definition and asserts the generated artifact and metadata header still match
  it; a stale header fails CI, never a live run.
- **A configuration-time compiled set.** `CATCHEM_MKPP_MECHANISMS` (default
  `gocart;chapman`) is the build's mechanism list. It sets MKPP's own
  `MKPP_HOST_SELECTED_MECHANISMS` / `MKPP_HOST_MECH_<UPPER>` gating macros, so
  only the selected mechanisms' generated headers are parsed, the process's
  solver registry, the metadata table, and MKPP's host registry are compiled
  from the same list, and a narrowed build carries no dead mechanism code.

Selecting a mechanism that was not compiled in is fatal at `init()` naming the
request and the available list. A configured `definition_file` that declares a
different mechanism, or whose digest differs from the one the solver was
generated from, is fatal naming both definitions — the file is checked
textually for those two scalars, so the process links no YAML parser.

## Cloud Liquid Water

The gocart mechanism gates its aqueous `SO2 + H2O2 -> SO4` pathway on cloud
water content with a `1.0e-6` threshold. The hybrid origin is configurable:

| `mode` | Behavior |
|---|---|
| `auto` (default) | probe host fields `CLDLIQ`, `CLOUD_LIQUID_WATER`, `QLI` in order; use the first present, else derive |
| `host_field` | require the named field; fatal if absent |
| `derived` | require `CLDF`, `QV`, `T`, `PMID`; fatal if any absent |

Under `auto` a present host field wins over derivation; an explicit `derived`
request is honored, never silently overridden. The origin is always logged at
`init()`, so a parity difference is attributable to the cloud-water source
rather than to the solver.

**Derivation physical basis and limitations.** The derived value is the grid-
mean condensate content from fields CATChem already carries (its state has no
cloud-liquid-water member today): with the Magnus/Tetens saturation vapor
pressure `e_sat = 611.2 · exp(17.67 (T − 273.15) / (T − 29.65))` and
`q_sat = ε e_sat / (PMID − (1 − ε) e_sat)`, the content is
`CLDF · max(QV − q_sat, 0) · ρ_dry` with `ρ_dry = PMID · g / (R_d T)` — a
kg/m³ water *content*, the unit the mechanism's threshold expects, not a
dimensionless fraction. It is a bulk approximation: it assumes condensate
excess over saturation is uniformly distributed across the grid cell's cloud
fraction, ignores ice/liquid partitioning below 273.15 K, and uses the
liquid-water Magnus coefficients at all temperatures. The formula is evaluated
in-kernel (no host round trip, no temporary field), guards division by zero and
the `T ≤ 29.65 K` Magnus singularity, and flags (rather than fabricates) a cell
whose inputs make it non-finite or negative.

**Gate semantics.** The compiled gocart artifact exposes the cloud-water
equilibrium input and its aqueous `SO2 + H2O2 -> SO4` rate is gated by a
smooth step around a `1e-6` kg/m³ content threshold, so the gate consumes the
same water *content* unit the derivation produces. The interim provenance guard
at `init()` protects the pairing from the other direction: a cloud-gated
definition paired with an artifact that exposes no cloud-water input is fatal
naming both sides, never a silent ungated run. The cloud-gated mechanism source
and regenerated artifact live on the MKPP submodule branch
`feat/cloud-gated-equilibrium`; the regeneration is confirmed reproducible (the
committed artifact rebuilds byte-for-byte from the mechanism source), and the
gate's behavior is verified end-to-end through the process by the parity
harness's `--cloudy-sulfur` mode (see below). What remains open is a
*pointwise* sulfur match against `so4chem`: the two schemes parameterize
sulfate formation differently — a full GOCART Henry's-law in-cloud model with
its own gas-phase path, versus a lumped gated bimolecular term plus a separate
gas-phase term — so their SO2/SO4 tendencies diverge by construction both
in-cloud and dry, and a physically consistent in-cloud comparison requires the
climatological oxidant and cloud fields of an ExtData-driven run. The carbon
parity is unaffected.

## Photolysis Coupling

A mechanism with photolysis reactions cannot run silently without its rates:

- **Coupled**: the photolysis process publishes per-label diagnostics
  (`photolysis_rate_<label>`); the binding is resolved at `init()` from the
  definition's labels, and every J-rate is re-read from the device view on
  every timestep. A label with no matching diagnostic is fatal naming the
  label and the definition.
- **Smoke mode** (`photolysis_smoke_mode: true`): the definition's host-forcing
  constants (gocart: `jh2o2 = 1e-5 s⁻¹`; chapman: `1e-5` and `1e-3 s⁻¹`) are
  applied as fixed J-values. This is for isolated-mechanism testing only; it
  logs a prominent warning every run and is mutually exclusive with a
  scheduled/bound photolysis source (that contradiction is fatal).

## Unit-Conversion Contract

CATChem's chemistry state is volume mixing ratio (mol/mol); MKPP's generated
kernels consume molar concentration (mol/m³). The bridge converts through the
cell's *dry* air molar density, `ρ_molar = AIRDEN_DRY / 0.0289644`, on gather
and its inverse on scatter, so a dry-air density change moves no mass across
species and unmapped tracers are untouched. The molecular weight is the shared
core constant; a test asserts the bridge and the core agree at the constant's
own precision.

## Backend & Dependency Prerequisites

`CATCHEM_BUILD_MKPP=ON` requires `CATCHEM_ENABLE_KOKKOS=ON`, Kokkos ≥ 5.1.1,
KokkosKernels, and a C++23-capable compiler — each enforced at configure time
with a `FATAL_ERROR` naming the missing dependency and the remedy, so a
mis-provisioned environment fails during configuration, never mid-compile. The
modulefile `catchem_mkpp` (loaded after the platform module) provisions Kokkos
and KokkosKernels from `$KOKKOS_DIR`/`$KOKKOS_KERNELS_DIR` or the stack's
modules. The default `OFF` build is completely unchanged. Platform
verification status is tracked honestly in
[MKPP Platform Support](../../user-guide/mkpp-platform-support.md): only local
macOS OpenMP has been exercised so far.

## Verification Suite

| Test | What it proves |
|---|---|
| `test_catchem_mkppchem_units` | registration and field contract, every configuration-time fatal (unknown key, mechanism not compiled, mismatched definition, process-order conflict), species-mapping failures (unmatched, collision), provenance failures, deterministic repeat-fatal messages |
| `test_catchem_mkppchem` | one gocart timestep end-to-end (finite/non-negative output, sulfur inventory conserved, tendency diagnostic consistent with the scatter), plus the chapman second-mechanism column run (selection, 4-slot mapping, chemistry actually advanced) |
| `test_catchem_mkppchem_coupled` / `_smoke` | photolysis coupling: J responds to solar zenith angle, unresolved-label and missing-source fatals, smoke-mode host forcing sustains OH |
| `test_catchem_mkppchem_properties` | property-based invariants over seeded randomized input: finiteness, non-negativity, sulfur conservation, tendency consistency, corrupt cells flagged exactly |
| `test_mkppchem_carbon_parity` | process-vs-process parity against the `carbchem` + `so4chem` baseline on the shared carbon species; cross-backend (OpenMP vs serial) determinism; zero-copy verification when profiler data is available. With `--cloudy-sulfur` it additionally runs a candidate-only gate-activation check: two runs share an elevated sulfur seed and differ only in cloud liquid water (a host-field `CLDLIQ` slice), and the aqueous `SO2 -> SO4` transfer must switch on with cloud water, dwarf the cloud-free gain, and conserve sulfur 1:1 |
| `verify_mechanism_provenance.py` | CI gate: definition ↔ generated artifact ↔ metadata header agreement for every compiled mechanism |
| `test_build_options.cmake` | configure-time negative cases: Kokkos off/old, KokkosKernels missing, C++23 unsupported — each asserts the fatal message and that compilation is never reached |

The carbon half of the parity gate passes. The sulfur (`SO2`/`SO4`) half is
verified as *gate activation* (the candidate-only `--cloudy-sulfur` check above)
rather than as a pointwise match against `so4chem`, because the two schemes'
sulfate chemistry differs by construction; a pointwise in-cloud sulfur parity
needs a climatological oxidant/cloud sounding and is left open for an
ExtData-driven run.

## Related Documentation

- [GasChem](../gaschem/index.md) — the MICM-based gas-phase chemistry process
- [SO4Chem](../so4chem/so4chem.md) — the hand-written sulfur chemistry `mkppchem` replaces
- [CarbChem](../carbchem/index.md) — the hand-written carbon chemistry `mkppchem` replaces
- [MKPP Platform Support](../../user-guide/mkpp-platform-support.md) — per-platform verification matrix
- [Build System](../../user-guide/build-system.md) — opt-in build configuration

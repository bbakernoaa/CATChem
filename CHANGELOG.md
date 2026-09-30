# CHANGELOG

<!-- version list -->

## Unreleased

### Features

- **mkppchem**: vendor MKPP as a pinned git submodule at `src/external/mkpp`
  (SHA `2d26cd8dfee4212db205e29efe79ff7fc9686746`, no moving `branch` key),
  opt-in via `CATCHEM_BUILD_MKPP` (default `OFF`; requires
  `CATCHEM_ENABLE_KOKKOS=ON`, Kokkos >= 5.1.1, and a C++23-capable compiler).
  Strictly additive — the default build is byte-identical to before.
  - New `mkppchem` chemistry process: runs an MKPP ahead-of-time-generated
    mechanism inside the CATChem core, one cell-launch per timestep over the
    mapped tracer set. Mechanism selection is configuration-driven
    (`processes/mkppchem/mechanism:`, default `gocart`, uncompiled or
    provenance-mismatched names fail at init naming the offender); runtime
    facts (species, reactions, photolysis, equilibrium inputs) come from a
    single aggregated generated meta header (one `Facts` struct per mechanism,
    plus a generated definition table and solver registry, so adding a
    mechanism needs no hand-written C++) that CI verifies against the
    OpenAtmos definition and the compiled artifact. Cloud water is derived
    from grid-mean condensate when the host does not supply it; photolysis
    rates couple to the `photolysis` process diagnostics
    (`photolysis_rate_<label>`), with an explicit smoke mode for isolated
    testing. The Rosenbrock adaptive-ramp seed is runtime-tunable via
    `processes/mkppchem/initial_step_seed` -- a larger seed takes fewer
    mandatory substeps per host step (faster) at slightly lower accuracy --
    without recompiling the generated artifact; unset keeps the seed baked into
    the artifact, and a configured value on an artifact predating the runtime
    seed member fails at init rather than being silently ignored. Validated
    against `carbchem`+`so4chem` for the carbon species
    (`BC1`/`BC2`/`OC1`/`OC2`). For the aqueous sulfur pathway (`SO2`/`SO4`) the
    cloud-water gate is verified by a candidate-only gate-activation check
    (`--cloudy-sulfur`: the `SO2 -> SO4` transfer switches on with cloud liquid
    water and conserves sulfur 1:1); a pointwise in-cloud match against
    `so4chem` is not asserted because the two schemes parameterize sulfate
    formation differently. See
    `docs/processes/mkppchem/index.md` and
    `docs/user-guide/mkpp-platform-support.md`.
  - Upstream MKPP gained cloud-liquid-water gated reactions (a third
    `EquilibriumInput` + a smooth sigmoid on the gated flux), driving the
    GOCART aqueous `SO2 + H2O2 -> SO4` pathway. The fix and the regenerated
    artifact are committed on MKPP branch `feat/cloud-gated-equilibrium` and the
    artifact rebuilds byte-for-byte from the mechanism source; the submodule
    gitlink re-pin to that SHA is pending the upstream push.
  - MKPP upstream follow-ups filed from this integration:
    (1) merge the cloud-gated mechanism source and regenerated artifact from
    `feat/cloud-gated-equilibrium` into MKPP `develop` and push, so the CATChem
    submodule gitlink can re-pin to a fetchable SHA (the artifact itself is
    confirmed to rebuild byte-for-byte from the mechanism source); (2) return a
    convergence status from `integrate` (or cap adaptive iterations) so the host
    can report true non-convergence instead of inferring it from bad output;
    (3) emit a machine-readable definition provenance identity into the generated
    header, replacing CATChem's structural count/name-set proxy check; (4) emit
    photolysis labels as data rather than a comment, removing the
    definition-order dependency; (5) consider a per-cell
    `HostExecutionParams` variant carrying views so the host can use
    `MechanismRegistry::execute` instead of owning the launch; (6) confirm
    whether `cxx_std_23` is genuinely required — the `requires` expressions in
    `dispatcher.hpp` and the generated `if constexpr` patterns are C++20, so
    relaxing to 20 would shrink the platform risk surface considerably.
- **settling**: implement the legacy `simple_scheme: true` optics-table path in
  the C++ core. A top-level `mie:` section (`directory` + type→file `files` map)
  loads GOCART2G NetCDF Mie tables once at init, and each aerosol species binds
  to a table via its `__mie_name` (bin from the short-name digit), mirroring the
  upstream `chemstate_init_mie_data` chain. The GOCART2G `Chem_SettlingSimple`
  kernel then drives wet-particle settling. Misconfiguration (empty `mie.files`,
  unreadable directory, missing/uncloseable table, unmatched `__mie_name`) aborts
  at init naming the offending item, with no silent fallback to the metadata path.
  Numerically certified against the `upstream/develop` Fortran oracle to
  single-precision round-off (max abs 1.03e-18, 1 step; 1.97e-17, 24 steps).
- **diagnostics**: per-process scheme diagnostics now match the legacy Fortran
  core across settling, dust, seasalt, carbchem, and so4chem. Each process
  honors an optional `processes/<name>/diag_species:` subset (default = the
  process's own runtime-resolved species set; unknown names fail at init), and
  registered fields are written to `catchem_diag*.nc` whenever runtime
  diagnostics are enabled — `diagnostics.output/process_diagnostics` is now a
  deprecated no-op retained only for configuration compatibility. This closes
  silent-wrong-diagnostic bugs: dust/seasalt per-bin ids now live in the local
  bin space the schemes search, carbchem no longer registers the whole catalog,
  and so4chem writes each `Production_rate_<sp>` from its own slot instead of
  duplicating the first into every species.

### Fixes

- **settling**: restore the upstream GOCART2G `Chem_Settling` compute path. The
  process now calls the legacy Fortran kernel with the full GOCART2G argument
  contract (`PMID`, `DELP`, `Z` edges, wet-radius Mie data) instead of the
  branch-local C++ `Stokes`/`Maring` reimplementation, which has been deleted.
  The metadata path also restores the upstream `swelling_method` option and
  leaves RH unmodified before entering `Chem_Settling`.
- **drydep**: the GOCART aerosol scheme consumes geometric height (`Z`) for its
  surface layer slot instead of the pressure edge array, removing the
  `log(0)`-driven NaN in `drydepf` when `PEDGE` is uniform.
- **core**: derived `CLDFRC` is only generated when no host cloud fraction is
  current for the import generation, so host-imported `CLDFRC` is preserved
  (matching the legacy column-sum semantics).
- **core**: physical constants now match upstream `constants.F90` exactly:
  `RSTARG = 8.3144598` J/K/mol, `H2O_MW = 18.016` g/mol, `Rd = 287.0` J/K/kg
  (both the C++ `catchem_constants.hpp` and the Fortran
  `catchem_bridge_constants` copies).
- **nuopc/aqmio**: `AQMIO_WriteGlobalAttrs` now performs its open/redef/put_att
  sequence only on the root PET of the VM it is given (new optional `iocomp`
  argument, same guard `AQMIO_Write1D` already used). Previously every PET ran
  the loop over per-tile files, so on a multi-tile cubed-sphere run all ranks
  entered define mode on the same HDF5 file concurrently; the losing file was
  left truncated or invalid and every later write to it failed with
  `NetCDF: HDF error`, `NetCDF: Unknown file format` and a misleading
  `Permission denied` from the `NF90_CLOBBER` fallback. `write_global_attributes`
  drops its own (uninitialized `localPet`) rank guard and adds an
  `ESMF_VMBarrier` so the other PETs do not open the files for field data until
  the root PET has left define mode and closed them.

### Testing

- Added an optics-table settling parity harness (`tests/run_settling_optics_parity.py`)
  that drives the C++ core and the legacy `legacy_parity_runner` oracle on identical
  met/chemistry inputs with `simple_scheme: true`, comparing snapshots and writing
  `parity_settling_optics/report.json`.
- Added `tests/test_catchem_settling_optics.cpp` (US1 init/run + US3 failure cases)
  and `tests/test_catchem_config_mie.cpp` (top-level `mie:` parsing and strict-schema
  acceptance).
- Added a legacy-parity oracle harness (`tests/run_settling_parity.py`,
  `tests/run_drydep_parity.py`) that compares the C++ core against
  `upstream/develop` Fortran snapshots at rtol 1e-6 / atol 1e-10.
- Added `tests/test_catchem_index_contract.cpp` verifying the import-to-core
  column-major slotting for 3-D and interface fields.
- Added the `mkppchem` test suite (label `mkppchem`): unit tests for unit
  conversion, dry-air molar density, species mapping, cloud-water derivation,
  mechanism selection/provenance, and every failure path (message content
  asserted, not just the throw); property tests for sulfur conservation and
  non-negativity/finiteness over randomized input; an isolated-column smoke
  test and a coupled photolysis integration test; and
  `tests/run_mkppchem_parity.py`, which drives the carbon-species parity
  comparison against `carbchem`+`so4chem` plus the determinism gates (repeat
  byte-identity, column decomposition, failure-message stability) and the
  zero-copy backend check. Negative configure cases (Kokkos off, Kokkos too
  old, KokkosKernels absent) run under `test_build_options`.

## v2.1.0-rc.2 (2026-09-10)

### Bug Fixes

- Use pygrep to be platform agnostic ([#194](https://github.com/ufs-community/CATChem/pull/194),
  [`d0d3138`](https://github.com/ufs-community/CATChem/commit/d0d31388969c98fe1549243677866ecba315c782))

### Chores

- Add initial CODEOWNERS ([#194](https://github.com/ufs-community/CATChem/pull/194),
  [`d0d3138`](https://github.com/ufs-community/CATChem/commit/d0d31388969c98fe1549243677866ecba315c782))

- Add initial CODEOWNERS file ([#194](https://github.com/ufs-community/CATChem/pull/194),
  [`d0d3138`](https://github.com/ufs-community/CATChem/commit/d0d31388969c98fe1549243677866ecba315c782))

- **modulefiles**: 🤖 sync from ufs-weather-model
  ([#189](https://github.com/ufs-community/CATChem/pull/189),
  [`91edb9a`](https://github.com/ufs-community/CATChem/commit/91edb9a5064192bbca3c9c608f18831686f42a81))

### Documentation

- Add info for a specific user ([#194](https://github.com/ufs-community/CATChem/pull/194),
  [`d0d3138`](https://github.com/ufs-community/CATChem/commit/d0d31388969c98fe1549243677866ecba315c782))

### Features

- Add standalone driver for offline simulation
  ([#169](https://github.com/ufs-community/CATChem/pull/169),
  [`d228c8b`](https://github.com/ufs-community/CATChem/commit/d228c8b16b8b6e8542ffe81f9913cb25759c7c8e))

- Rename to chem-core-team ([#194](https://github.com/ufs-community/CATChem/pull/194),
  [`d0d3138`](https://github.com/ufs-community/CATChem/commit/d0d31388969c98fe1549243677866ecba315c782))


## v2.1.0-rc.1 (2026-08-24)

### Features

- Enhance the modulefile sync workflow following CECE
  ([#188](https://github.com/ufs-community/CATChem/pull/188),
  [`8c924bc`](https://github.com/ufs-community/CATChem/commit/8c924bc7f0abf54f91b0ba5abc650b969018e9ab))

## v2.0.0 (2026-07-20)


## v0.1.0-rc.2 (2026-07-13)

### Features

- Support GFortran 12 ([#162](https://github.com/ufs-community/CATChem/pull/162),
  [`06bf670`](https://github.com/ufs-community/CATChem/commit/06bf6700608d585a0ae8c45e94e8f77c6708fe73))


## v0.1.0-rc.1 (2026-07-09)

### Features

- GOCART-2G processes for GCAFS ([#159](https://github.com/ufs-community/CATChem/pull/159),
  [`c1ecaf3`](https://github.com/ufs-community/CATChem/commit/c1ecaf3cbdbd0097d5949a491935493f4b061a65))

- Link MUSICA ([#154](https://github.com/ufs-community/CATChem/pull/154),
  [`5b50860`](https://github.com/ufs-community/CATChem/commit/5b50860ab50f390ce4bab4fb8f7a24f9537fd5a7))

- **ci**: Add semantic release, docker build jobs & find yaml-cpp
  ([#158](https://github.com/ufs-community/CATChem/pull/158),
  [`a8ff444`](https://github.com/ufs-community/CATChem/commit/a8ff444744ff94981615785ebf595c53ea220824))

- **ci**: Workaround for protected branch push & address semver upgrade issues
  ([#166](https://github.com/ufs-community/CATChem/pull/166),
  [`cd7851f`](https://github.com/ufs-community/CATChem/commit/cd7851f4a4135a33752df41a7f70c65ca9f1c261))


## v0.0.1 (2026-05-21)

## What's Changed
* Add first version of /docs for ReadtheDocs page by @colin-harkins in https://github.com/ufs-community/CATChem/pull/1
* Restructure Kate's UFS-CCPP-Chem code by @jianheACM in https://github.com/ufs-community/CATChem/pull/3
* Update issue/PR templates by @bbakernoaa in https://github.com/ufs-community/CATChem/pull/103
* Deploy Doxygen docs to GH Pages by @zmoon in https://github.com/ufs-community/CATChem/pull/109
* Update pre-commit config by @zmoon in https://github.com/ufs-community/CATChem/pull/112
* Doxysphinx by @zmoon in https://github.com/ufs-community/CATChem/pull/110
* Incorporate MICM into build and start chem process structure by @zmoon in https://github.com/ufs-community/CATChem/pull/114
* Update GOCART submodule to v2.4.0 by @zmoon in https://github.com/ufs-community/CATChem/pull/118
* Feature/cc restructure by @bbakernoaa in https://github.com/ufs-community/CATChem/pull/125
* Remove javascript code that makes the mermaid plots not work correctly on ReadTheDocs right now by @rschwant in https://github.com/ufs-community/CATChem/pull/126
* Add dry and wet deposition processes by @lwcugb in https://github.com/ufs-community/CATChem/pull/127
* Update logos on docs and in repo by @rschwant in https://github.com/ufs-community/CATChem/pull/129
* Fix logo, update concept figure, update Jian's paper reference by @rschwant in https://github.com/ufs-community/CATChem/pull/130
* Fix develop branch CI builds by @zmoon in https://github.com/ufs-community/CATChem/pull/133
* Update MUSICA submodule to latest version 0.15.0 by @rschwant in https://github.com/ufs-community/CATChem/pull/131
* ci: Add matrix builds for CATChem with optional MUSICA support by @bbakernoaa in https://github.com/ufs-community/CATChem/pull/135
* Add offline surface emission read module and SO4 simple chemistry from GOCART by @lwcugb in https://github.com/ufs-community/CATChem/pull/128
* Add weekly modulefile sync workflow to update and clean references by @bbakernoaa in https://github.com/ufs-community/CATChem/pull/139
* Some tweaks for Intel oneAPI build  by @zmoon in https://github.com/ufs-community/CATChem/pull/147
* Fix some warnings by @zmoon in https://github.com/ufs-community/CATChem/pull/146
* Feature/ocbc by @lwcugb in https://github.com/ufs-community/CATChem/pull/132
* Group Dependabot GitHub Actions updates by @zmoon in https://github.com/ufs-community/CATChem/pull/155
* Add support for reading MICM species files by @mbruckner-work in https://github.com/ufs-community/CATChem/pull/138

## New Contributors
* @colin-harkins made their first contribution in https://github.com/ufs-community/CATChem/pull/1
* @jianheACM made their first contribution in https://github.com/ufs-community/CATChem/pull/3
* @zmoon made their first contribution in https://github.com/ufs-community/CATChem/pull/109
* @rschwant made their first contribution in https://github.com/ufs-community/CATChem/pull/126
* @lwcugb made their first contribution in https://github.com/ufs-community/CATChem/pull/127
* @mbruckner-work made their first contribution in https://github.com/ufs-community/CATChem/pull/138

**Full Changelog**: https://github.com/ufs-community/CATChem/commits/v0.0.1

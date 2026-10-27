# CCPP-SCM single-suite test

CCPP-SCM is a test host, not a CATChem build dependency. Production CCPP scheme sources and
metadata live in `drivers/ccpp/`; CATChem suite XML lives in `tests/ccpp/suites/`. The test runner
checks out the locked SCM, framework, physics, and CATChem source revisions under `build/`, stages
the needed test data, and applies SCM host/build adaptations only to that disposable checkout.
Nothing in the checked-out CCPP-SCM source tree is maintained in this project.

## Run the CI suite locally

The CI job is [`.github/workflows/ccpp-scm-suite.yml`](../../.github/workflows/ccpp-scm-suite.yml).
It runs `CATChem_EmissionChemistryRemoval` with GFS v17 physics on the
`gabls3_noahmp` SCM case. The selected CATChem calls are Dust, SeaSalt, WetDep, Settling, DryDep,
SO4Chem, and CarbChem.

Build the test image and run the suite from the repository root:

```sh
docker build --platform linux/amd64 -f tests/ccpp-scm/docker/Dockerfile \
  -t catchem-ccpp-scm-ci .
docker run --rm --platform linux/amd64 \
  --mount type=bind,source="$PWD",target=/workspace \
  --workdir /workspace catchem-ccpp-scm-ci \
  bash -lc 'python3 tests/run_ccpp_scm_suite.py'
```

The runner locks source and data revisions, reuses a supplied local data directory when available,
downloads SCM's static case/physics inputs and the GFS v17 GOCART climatology if required files are
missing. It also retrieves Thompson microphysics lookup tables used by the GFS physics suite. It
then configures capgen for this suite, builds the SCM host, and runs the case. It fails on
absent input files, capgen/build errors, a failed process schedule,
missing NetCDF output, or failed scientific acceptance. Logs and the summary are written under
`build/ccpp-scm-ci/evidence/`; process and data evidence is under
`build/ccpp-scm-ci/cases/`.

To retrieve and validate the data without building or running the model, use the same script's
data-only mode:

```sh
python3 tests/run_ccpp_scm_suite.py --data-only --file_dir=/path/to/files
```

`--file_dir` is the persistent SCM data root: the script creates/uses
`/path/to/files/processed_case_input/` and `/path/to/files/physics_input_data/` there. The same
option on a normal suite run reuses existing data and stores newly retrieved files at that path.
The path is recorded in `build/ccpp-scm-ci/evidence/data-preparation.json`. An existing partial
local tree can also be supplied with `--data-dir`; the runner retrieves only when locked required
inputs remain missing, then verifies the data checksums. If both are given, `--data-dir` seeds the
persistent `--file_dir` cache.

The [CCPP Physics SCM CI workflow](https://github.com/NCAR/ccpp-physics/blob/main/.github/workflows/ci_scm_ccpp_prebuild.yml)
checks a physics checkout by initializing it in SCM and running `ccpp_prebuild.py`. This test
complements that metadata/prebuild check by building and executing a locked SCM case with
CATChem's process suite.

SCM data downloads use the release pinned in [`source-lock.json`](source-lock.json), and all 163
expected extracted files are checked against [`data-lock.json`](data-lock.json). The GOCART
climatology is large (about 12 GB), and Thompson lookup tables add about 340 MB; allow enough disk space for the downloaded archives,
extracted data, SCM build and output.

## Run without network access

`--offline` disables both source cloning and input downloads. Prepare these items beforehand:

- the complete, locked source checkouts (SCM with initialized submodules, CATChem with its
  GOCART/MUSICA submodules, and the locked framework and physics repositories);
- a complete SCM `scm/data` tree with the case inputs, GOCART climatology, and Thompson tables;
- the already built test container or equivalent compiler/library environment.

Point `CCPP_SCM_SOURCE` and `CATCHEM_SOURCE` at the SCM and CATChem checkouts. The script derives
the framework, physics, CMakeModules, GOCART and MUSICA paths from those checkouts by default;
set `CCPP_FRAMEWORK_SOURCE`, `CCPP_PHYSICS_SOURCE`, `CCPP_CMAKEMODULES_SOURCE`, `GOCART_SOURCE`
or `MUSICA_SOURCE` to override a path. Every repository revision is checked against
[`source-lock.json`](source-lock.json), and the data is checked against
[`data-lock.json`](data-lock.json). Then run in the prepared environment:

```sh
CCPP_SCM_SOURCE=/path/to/ccpp-scm \
CATCHEM_SOURCE=/path/to/locked-CATChem \
python3 tests/run_ccpp_scm_suite.py --offline --file_dir=/path/to/files
```

`--data-dir` may point to an SCM checkout, its `scm/data` directory, or the data directory itself.
For `--file_dir`, pass the directory containing `processed_case_input/` and `physics_input_data/`.
In offline mode, all locked source repositories/submodules and every required input file must
already be present and match their pinned revisions/checksums.

## Evidence

- Source and input revisions: `tests/ccpp-scm/source-lock.json`.
- CATChem process suite definitions: `tests/ccpp/suites/`.
- CATChem case manifests and disposable SCM host patches: `tests/ccpp-scm/`.
- CI result summary: `build/ccpp-scm-ci/evidence/single-suite.json`.

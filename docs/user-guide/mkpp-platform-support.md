# MKPP build prerequisites and platform matrix

The `mkppchem` process is generated ahead of time by
[MKPP](https://github.com/NOAA-EMC/mkpp) and is strictly opt-in:
`CATCHEM_BUILD_MKPP` defaults to `OFF`, and with it `OFF` the build has no
Kokkos version floor, no KokkosKernels dependency, no C++23 requirement, and no
MKPP submodule content . This page records what turning it `ON`
requires, and the honest state of verification on each supported platform.

## Required when `CATCHEM_BUILD_MKPP=ON`

| Prerequisite | Minimum | Enforced at configure |
|---|---|---|
| Kokkos | 5.1.1 | `find_package` + version guard: fails naming the found version, the required minimum, and the `Kokkos_ROOT`/`Kokkos_DIR` remedy |
| KokkosKernels | any (matching the Kokkos install) | `find_package(KokkosKernels)`: fails naming the missing package and the `KokkosKernels_DIR` remedy |
| Kokkos backend | `CATCHEM_ENABLE_KOKKOS=ON` | fails naming both options; MKPP cannot build against the serial-free core |
| C++23 compiler | `-std=c++23` accepted | `check_cxx_compiler_flag`: fails naming compiler id/version and the remedy |

Every one of these is a **configure-time** `FATAL_ERROR`: a non-conforming
environment never reaches compilation, so there is no mid-build failure mode
. The negative cases are covered by `tests/test_build_options.cmake`
(run with `cmake -P tests/test_build_options.cmake`).

## Modulefiles

The baseline `catchem_<platform>` modules deliberately do **not** reference
Kokkos version, KokkosKernels, C++23, or the submodule. The MKPP prerequisites
live in a separate opt-in module, `catchem_mkpp.lua`, loaded after the platform
module:

```bash
module load catchem_derecho.intel   # or catchem_hera.gnu, etc.
module load catchem_mkpp            # adds Kokkos >= 5.1.1 + KokkosKernels
cmake -B build -DCATCHEM_BUILD_MKPP=ON -DCATCHEM_ENABLE_KOKKOS=ON ...
```

`catchem_mkpp.lua` resolves Kokkos in this order:

1. `KOKKOS_DIR` / `KOKKOS_KERNELS_DIR` -- site-provisioned install prefixes,
   prepended to `CMAKE_PREFIX_PATH`.
2. `module load kokkos/<ver>` + `module load kokkos-kernels/<ver>` -- when the
   stack already provides them. Pin a site build with `KOKKOS_VER` /
   `KOKKOS_KERNELS_VER` (values below the 5.1.1 floor are rejected at load).

If neither is available the module fails loudly with the provisioning remedy --
the same contract the configure guard enforces, just earlier.

## Platform matrix (research record)

The MKPP feature is **research-stage** (spec Assumption A7): it is not yet
certified for R2O on any operational NOAA platform. The rows below record what
has actually been *exercised*, not what is expected to work.

| Platform / configuration | Toolchain (modulefile) | Kokkos + KokkosKernels >= 5.1.1 | C++23 configure guard | MKPP build exercised |
|---|---|---|---|---|
| Local macOS (Apple Silicon) | Homebrew LLVM Clang 22 (`catchem_macosx.*`) | system Kokkos 5.1.1 (Homebrew) | pass | **yes** -- full `mkppchem` suite, OpenMP backend |
| Local Linux (dev container) | GCC (12/14) | build-from-source 5.1.1 | pass (GCC >= 12 has `-std=c++23`) | **no** -- container image does not yet enable MKPP |
| GitHub CI (ubuntu-24.04) | GCC 12, GCC 14, oneAPI 2025.3 | Kokkos 5.1.1 built in workflow | (not probed) | **no** -- CI builds `CATCHEM_ENABLE_KOKKOS=ON` but never `CATCHEM_BUILD_MKPP=ON`; KokkosKernels is not installed |
| derecho (intel / intel-llvm / gnu) | oneAPI 2024.2.1 / GCC 12.4.0 | not in stack | unverified | **no** |
| hera (intel / intel-llvm / gnu) | oneAPI 2024.2.1 / GCC 13.3.0 | not in stack | unverified | **no** |
| hercules (intel / intel-llvm / gnu) | oneAPI 2024.2.1 / GCC 13.3.0 | not in stack | unverified | **no** |
| gaeac5 / gaeac6 (intel / intel-llvm) | oneAPI 2023.2.0 | not in stack | unverified | **no** |
| orion / ursa (intel / intel-llvm / gnu) | oneAPI 2024.2.1 / GCC 12.4.0 | not in stack | unverified | **no** |
| wcoss2 (intel) | Intel 19.1.3 (PrgEnv-intel 8.1.0) | not in stack | expected to fail the guard (19.1 predates C++23) | **no** |
| acorn (intel) | stack-intel (unpinned) | not in stack | unverified | **no** |
| aws-ec2 / noaacloud (intel) | oneAPI 2024.2.1 | not in stack | unverified | **no** |

Honest reading of the table:

- The only machine where the MKPP process has been built and run end to end in
  this workspace is the local macOS/OpenMP configuration above. The HPC rows
  are **not** failures -- they are simply unexercised, because this development
  environment cannot reach those systems.
- None of the NOAA production stacks (spack-stack 1.9.x era modulefiles) ship a
  Kokkos/KokkosKernels pair at the 5.1.1 floor, and the container/CI workflows
  do not install KokkosKernels at all. On those platforms the correct behavior
  today is the configure-time `FATAL_ERROR` naming the missing dependency --
  which is exactly what the configure guards are designed to do.
- A platform is "supported for MKPP" only once it has been
  *exercised*: provision Kokkos 5.1.1 + KokkosKernels (via `KOKKOS_DIR`/
  `KOKKOS_KERNELS_DIR` or a stack module), configure with
  `CATCHEM_BUILD_MKPP=ON`, and run the `mkppchem` ctest suite in both Debug and
  Release. Record the result in the row above.
- The platform-matrix-in-CI goal remains **open** until the CI
  workflows add a KokkosKernels install step and a `CATCHEM_BUILD_MKPP=ON` job;
  until then the feature must be described as locally verified only.

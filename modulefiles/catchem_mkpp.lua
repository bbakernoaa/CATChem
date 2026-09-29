help([[
Adds the MKPP chemistry prerequisites (Kokkos >= 5.1.1 and KokkosKernels) to a
CATChem build environment.

Load this AFTER the platform module (for example `module load catchem_derecho.intel`)
and configure with -DCATCHEM_BUILD_MKPP=ON. It is deliberately a separate,
opt-in module: the baseline catchem_* modules must not gain any dependency on
Kokkos version, KokkosKernels, C++23, or the MKPP submodule (the feature is
strictly additive), and not every platform's stack ships a
Kokkos new enough for MKPP.

Versions are overridable through the environment so a site can pin its own
provisioned build:

  KOKKOS_VER           Kokkos version to load (floor 5.1.1)
  KOKKOS_KERNELS_VER   KokkosKernels version to load
  KOKKOS_DIR           skip `module load` and point CMake at an install
  KOKKOS_KERNELS_DIR   same for KokkosKernels (CMake cache variable)
]])

local kokkos_ver = os.getenv("KOKKOS_VER") or "5.1.1"
-- MKPP cannot build against an older Kokkos; refuse to load rather
-- than let the configure fail with a dependency message the user could not
-- have expected from the module. The configure-time guard remains the
-- authoritative gate -- this only catches an obvious module mis-pin early.
local function version_below(v, major, minor, patch)
    local vmaj, vmin, vpat = v:match("^(%d+)%.(%d+)%.?(%d*)$")
    if not vmaj then
        return false -- unparseable: leave the decision to the configure guard
    end
    vmaj, vmin, vpat = tonumber(vmaj), tonumber(vmin), tonumber(vpat) or 0
    if vmaj ~= major then return vmaj < major end
    if vmin ~= minor then return vmin < minor end
    return vpat < patch
end

if version_below(kokkos_ver, 5, 1, 1) then
    LError("KOKKOS_VER=" .. kokkos_ver .. " is below the 5.1.1 floor the MKPP process requires")
end

-- When the site provides Kokkos/KokkosKernels outside the module system, the
-- *_DIR variables hand them to CMake directly and nothing is loaded.
-- Otherwise the module load itself fails loudly when the stack does not ship
-- a Kokkos -- the same "name the dependency and the remedy" contract the
-- configure-time guard enforces, just earlier in the workflow.
local kokkos_dir = os.getenv("KOKKOS_DIR")
local kernels_dir = os.getenv("KOKKOS_KERNELS_DIR")

if kokkos_dir then
    prepend_path("CMAKE_PREFIX_PATH", kokkos_dir)
else
    load("kokkos/" .. kokkos_ver)
end

if kernels_dir then
    prepend_path("CMAKE_PREFIX_PATH", kernels_dir)
else
    load(pathJoin("kokkos-kernels", os.getenv("KOKKOS_KERNELS_VER") or kokkos_ver))
end

whatis("Description: CATChem MKPP chemistry prerequisites (Kokkos + KokkosKernels)")

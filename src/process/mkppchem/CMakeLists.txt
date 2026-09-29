# src/process/mkppchem/CMakeLists.txt
#
# The opt-in MKPP chemistry process. Built only when CATCHEM_BUILD_MKPP=ON
# (guarded by the parent src/process/CMakeLists.txt). Mirrors the gaschem
# process target: a STATIC library on the Kokkos-backed C++ core, with the
# third-party (MKPP) headers consumed as SYSTEM includes so CATChem's own
# warning flags never report findings inside generated code.

# The mechanisms compiled INTO this binary. Each entry must have a generated
# source directory under MKPP's mkpp-generated/ tree; the generated solver
# registry (catchem_mkpp_mechanism_registry.hpp) is the C++ counterpart of
# this list, gated by the macros set below. Keeping the selection here (not a
# runtime scan) is what makes "mechanism not compiled into the build" a
# configure-time fact. The default covers every mechanism with
# a checked-in generated tree, so a plain -DCATCHEM_BUILD_MKPP=ON build can
# select either by name; narrowing the list is a configure-time option.
set(
  CATCHEM_MKPP_MECHANISMS
  "gocart;chapman"
  CACHE STRING
  "Semicolon-separated MKPP mechanisms to compile into the mkppchem process"
)

set(_mkppchem_srcs
  catchem_process_mkppchem.cpp
  catchem_mkpp_mechanism.cpp
  catchem_mkpp_cloud_water.cpp
  catchem_mkpp_state_bridge.cpp
)

# The YAML parser (catchem_mkpp_mechanism_parse.cpp) is deliberately NOT in this
# list: the runtime builds its definition facts from the generated metadata
# header (catchem_mkpp_mechanism_meta.hpp), not from a YAML parse, so yaml-cpp
# stays off the process link (the single-parse design). The parser is compiled
# into the CI/unit-test target instead, which exercises its failure paths.

# MKPP's mkpp_host is an INTERFACE-only target: the generated per-mechanism
# chunk translation units (rates, jacobian, solve, supernodal_factorize,
# supernodal_solve) are NOT compiled by MKPP's own CMake, so this library
# compiles them itself -- only for the selected mechanisms.
# MKPP's helper mkpp_add_compiled_mechanism(<target> <dir> <mech>) creates a
# STATIC library named by the CALLER (its e2e tests use e2e_<mech>_compiled_kernels),
# not a fixed object-library name; CATChem deliberately does not call it and
# embeds the chunk sources directly in this one target instead, so the process
# library and its generated kernels always share one link line and one set of
# MKPP_HOST_* gating macros.
set(_mkpp_generated_dir ${PROJECT_SOURCE_DIR}/src/external/mkpp/mkpp-generated)
foreach(_mech IN LISTS CATCHEM_MKPP_MECHANISMS)
  set(_mech_dir "${_mkpp_generated_dir}/${_mech}")
  if(NOT IS_DIRECTORY "${_mech_dir}")
    message(
      FATAL_ERROR
      "CATCHEM_MKPP_MECHANISMS names '${_mech}' but ${_mech_dir} does not "
      "exist; regenerate the mechanism with MKPP before enabling it."
    )
  endif()
  file(GLOB _mech_srcs CONFIGURE_DEPENDS "${_mech_dir}/*.cpp")
  if(NOT _mech_srcs)
    message(
      FATAL_ERROR
      "No generated sources found for MKPP mechanism '${_mech}' in ${_mech_dir}"
    )
  endif()
  list(APPEND _mkppchem_srcs ${_mech_srcs})
endforeach()

add_library(catchem_process_mkppchem STATIC ${_mkppchem_srcs})

# MKPP's own host headers (mechanisms.hpp / mechanism_manager.hpp) aggregate
# every mechanism in mkpp-generated/ unless the host narrows them. These macros
# are MKPP's documented gating contract (its e2e tests use the same pair): with
# MKPP_HOST_SELECTED_MECHANISMS set, each header is included only when its
# MKPP_HOST_MECH_<UPPER> counterpart is defined, so a build that compiled only
# gocart never parses another mechanism's 5,800-line header. The same macros
# gate CATChem's per-mechanism include and registry entry, keeping MKPP's
# registry and CATChem's adapter set in lockstep with this list.
target_compile_definitions(catchem_process_mkppchem PRIVATE MKPP_HOST_SELECTED_MECHANISMS)
foreach(_mech IN LISTS CATCHEM_MKPP_MECHANISMS)
  string(TOUPPER "${_mech}" _mech_upper)
  target_compile_definitions(catchem_process_mkppchem PRIVATE MKPP_HOST_MECH_${_mech_upper})
endforeach()

target_include_directories(
  catchem_process_mkppchem
  PUBLIC ${CMAKE_CURRENT_SOURCE_DIR} ${PROJECT_SOURCE_DIR}/src/core
)

# The generated chunk TUs include only <cmath>, but the generated registry
# header (catchem_mkpp_mechanism_registry.hpp) pulls MKPP's mechanism manager,
# which includes each selected mechanism's generated artifact header. Expose
# the generated tree as a SYSTEM include so the generated 5,800-line headers do
# not trip CATChem's Debug warning-as-error gate.
target_include_directories(
  catchem_process_mkppchem
  SYSTEM
  PUBLIC ${_mkpp_generated_dir}
)

target_link_libraries(
  catchem_process_mkppchem
  PUBLIC
    CATChem_core_cpp
    CATChem_core
    Kokkos::kokkos
    MKPP::mkpp_host
)

# NOTE: yaml-cpp is intentionally NOT linked here. The runtime consumes the
# generated metadata header, never a YAML parse, so the process library must not
# pull in a YAML dependency. (CATChem_core_cpp links yaml-cpp PUBLIC for its own
# config parsing, but that is the core's concern, not the process's.)

# mkpp_host declares cxx_std_23 and links KokkosKernels when enabled; those
# usage requirements arrive transitively through the MKPP::mkpp_host link above.
# Its include directories are already re-exported as SYSTEM by the
# CATCHEM_BUILD_MKPP block in src/external/CMakeLists.txt (which resolves the
# alias to the real target before setting the property), so consumers of this
# library see the generated headers as system headers without any further work
# here -- and an ALIAS target like MKPP::mkpp_host cannot carry properties.

# External dependencies

# Create a basic utilities library that other modules can depend on
add_library(catchem_utilities ../process/catchem_bridge_utilities.F90)

set_target_properties(
  catchem_utilities
  PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include
)

# The precision module MUST be compiled with the SAME real-8 promotion the
# process libraries use unconditionally, so its kind(0.0) is 8-byte and the
# compile-time precision guard validates against the real promotion.  Do NOT
# gate this on the global Fortran flags: the promotion is per-target.
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU|G95")
  target_compile_options(
    catchem_utilities
    PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-fdefault-real-8;-fdefault-double-8>
  )
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel|IntelLLVM")
  target_compile_options(
    catchem_utilities
    PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-r8>
  )
endif()

#
# GOCART-2G Process Library
#

set(
  _srcs
  GOCART/Process_Library/GOCART2G_Process.F90
  GOCART/Process_Library/GOCART2G_MieMod.F90
)
set(_lib GOCART2G)

add_library(${_lib} ${_srcs})
# GOCART2G holds the shared science kernels (Chem_Settling, wet/dry removal,
# emission helpers) called by the process schemes.  It MUST be promoted to
# real-8 unconditionally to match those schemes and the `fp` kind; gating on
# the global Fortran flags (which the per-target promotion does not set) would
# leave GOCART2G in single precision while its callers run double -- a silent
# precision/ABI mismatch across the call boundary.
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU|G95")
  target_compile_options(${_lib} PRIVATE -ffree-line-length-none)
  target_compile_options(
    ${_lib}
    PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-fdefault-real-8;-fdefault-double-8>
  )
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel|IntelLLVM")
  target_compile_options(${_lib} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-r8>)
endif()

#find_package(NetCDF REQUIRED COMPONENTS Fortran)

# Use NetCDF from parent scope (already found by main UFS CMakeLists.txt or standalone CMakeLists.txt)
if(TARGET NetCDF::NetCDF_Fortran)
  target_link_libraries(${_lib} NetCDF::NetCDF_Fortran)
endif()

set_target_properties(
  ${_lib}
  PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include
)

#
# yaml-cpp
#

if(NOT yaml-cpp_FOUND AND NOT TARGET yaml-cpp::yaml-cpp AND NOT TARGET yaml-cpp)
  #  message(
  #    FATAL_ERROR
  #    "yaml-cpp NOT FOUND BY find_package (Temporary error for testing)"
  #  )
  message(STATUS "yaml-cpp not found on system, using internal version")

  include(FetchContent)

  # Configure yaml-cpp build options
  set(YAML_CPP_BUILD_TESTS OFF CACHE BOOL "Build yaml-cpp tests")
  set(YAML_CPP_BUILD_TOOLS OFF CACHE BOOL "Build yaml-cpp tools")
  set(YAML_CPP_BUILD_CONTRIB OFF CACHE BOOL "Build yaml-cpp contrib")

  # Declare yaml-cpp as already available via FetchContent
  # This prevents MUSICA from fetching their own copy
  FetchContent_Declare(yaml-cpp SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/yaml-cpp)
  FetchContent_MakeAvailable(yaml-cpp)
endif()

# Ensure both yaml-cpp and yaml-cpp::yaml-cpp targets exist regardless of how it was found
if(TARGET yaml-cpp AND NOT TARGET yaml-cpp::yaml-cpp)
  add_library(yaml-cpp::yaml-cpp ALIAS yaml-cpp)
endif()
if(TARGET yaml-cpp::yaml-cpp AND NOT TARGET yaml-cpp)
  add_library(yaml-cpp ALIAS yaml-cpp::yaml-cpp)
endif()

#
# MUSICA Fortran API (optional, controlled by CATCHEM_BUILD_MUSICA)
#

if(CATCHEM_BUILD_MUSICA)
  # Set MUSICA build options before adding the subdirectory
  set(MICM_ENABLE_TESTS OFF)
  set(MUSICA_BUILD_C_CXX_INTERFACE ON)
  set(MUSICA_BUILD_FORTRAN_INTERFACE ON)
  set(MUSICA_ENABLE_INSTALL OFF)
  set(MUSICA_ENABLE_TESTS OFF)
  set(MUSICA_ENABLE_MICM ON)
  set(MUSICA_ENABLE_TUVX ON)
  set(MUSICA_ENABLE_CARMA OFF)

  # Add MUSICA as a subdirectory
  add_subdirectory(musica)

  # MUSICA and the dependencies it fetches (micm, mechanism_configuration) are
  # ordinary in-tree targets, so their include directories are not treated as
  # system headers and CATChem's Debug warning flags would report findings
  # inside them. Re-export their include directories as SYSTEM.
  foreach(
    _dep
    musica
    musica::micm
    micm::micm
    micm
    musica::mechanism_configuration
    mechanism_configuration
  )
    if(TARGET ${_dep})
      get_target_property(_dep_real ${_dep} ALIASED_TARGET)
      if(NOT _dep_real)
        set(_dep_real ${_dep})
      endif()
      set_property(
        TARGET ${_dep_real}
        APPEND
        PROPERTY
          INTERFACE_SYSTEM_INCLUDE_DIRECTORIES
            $<TARGET_PROPERTY:${_dep_real},INTERFACE_INCLUDE_DIRECTORIES>
      )
    endif()
  endforeach()

  # musica-fortran exports -ffree-line-length-none as a PUBLIC compile option
  # with no language guard (src/external/musica/fortran/CMakeLists.txt), so
  # every C++ translation unit that links it gets
  # "cc1plus: warning: command-line option '-ffree-line-length-none' is valid
  # for Fortran but not for C++". Re-scope its interface compile options to
  # Fortran here rather than patching the submodule.
  if(TARGET musica-fortran)
    get_target_property(_mf_opts musica-fortran INTERFACE_COMPILE_OPTIONS)
    if(_mf_opts)
      set(_mf_scoped "")
      foreach(_opt IN LISTS _mf_opts)
        if(_opt MATCHES "COMPILE_LANGUAGE")
          list(APPEND _mf_scoped "${_opt}")
        else()
          list(APPEND _mf_scoped "$<$<COMPILE_LANGUAGE:Fortran>:${_opt}>")
        endif()
      endforeach()
      set_target_properties(
        musica-fortran
        PROPERTIES INTERFACE_COMPILE_OPTIONS "${_mf_scoped}"
      )
    endif()
  endif()
endif()

#
# MKPP multiphase chemistry (optional, controlled by CATCHEM_BUILD_MKPP)
#

if(CATCHEM_BUILD_MKPP)
  # KokkosKernels is a hard requirement when MKPP is active.
  # Find it HERE, before add_subdirectory(mkpp), so MKPP's own unpinned
  # FetchContent fallback for kokkos-kernels never fires and the version stays
  # governed by CATChem's toolchain rather than MKPP's default tag. The find is
  # QUIET: on failure the message below names both remedies -- pointing
  # KokkosKernels_DIR/CMAKE_PREFIX_PATH at an install, or disabling MKPP --
  # and never lets the configure reach a compiler.
  find_package(KokkosKernels QUIET)
  if(NOT KokkosKernels_FOUND)
    message(
      FATAL_ERROR
      "CATCHEM_BUILD_MKPP=ON requires KokkosKernels, which was not found. "
      "KokkosKernels is a hard requirement when the MKPP process is active "
      "(the generated solver's batched kernels are built on it); this build "
      "cannot proceed to compilation without it. Point CMake at an install "
      "with -DKokkosKernels_DIR=<path>/lib/cmake/KokkosKernels (or add it to "
      "CMAKE_PREFIX_PATH), or configure with -DCATCHEM_BUILD_MKPP=OFF."
    )
  endif()

  # Set MKPP's build options before adding the subdirectory. These must be
  # CACHE ... FORCE: MKPP declares each with option(), which reads the cache,
  # so a plain set() would be overridden by that option() call. CATChem pins
  # the batched-KokkosKernels path on and everything that pulls extra fetched
  # dependencies (adjoint, solver comparison, MICM compare) or MKPP's own
  # googletest suite off. CATChem gates its tests behind CATCHEM_BUILD_TESTING,
  # so MKPP's BUILD_TESTING is independent and forced OFF here.
  set(MKPP_ENABLE_KOKKOS_KERNELS ON CACHE BOOL "" FORCE)
  set(MKPP_ENABLE_ADJOINT OFF CACHE BOOL "" FORCE)
  set(MKPP_ENABLE_SOLVER_COMPARISON OFF CACHE BOOL "" FORCE)
  set(MKPP_ENABLE_MICM_COMPARE OFF CACHE BOOL "" FORCE)
  set(BUILD_TESTING OFF CACHE BOOL "" FORCE)

  # MKPP's inner find_package(Kokkos) calls are guarded (see the notes in
  # src/external/mkpp/CMakeLists.txt): with the Kokkos targets already created
  # by the CATCHEM_ENABLE_KOKKOS block above -- whether system-found or
  # FetchContent-built -- MKPP adopts them instead of re-resolving against a
  # different on-disk installation. KokkosKernels' exported config is likewise
  # self-guarding, so MKPP's QUIET re-find returns early.
  add_subdirectory(mkpp)

  # mkpp_host and the generated solver headers are third-party code: re-export
  # their include directories as SYSTEM so CATChem's Debug warning-as-error gate
  # never fires on findings inside generated code. Alias targets
  # cannot carry properties, so resolve to the real target first, mirroring the
  # MUSICA loop above.
  foreach(_dep mkpp_host MKPP::mkpp_host)
    if(TARGET ${_dep})
      get_target_property(_dep_real ${_dep} ALIASED_TARGET)
      if(NOT _dep_real)
        set(_dep_real ${_dep})
      endif()
      set_property(
        TARGET ${_dep_real}
        APPEND
        PROPERTY
          INTERFACE_SYSTEM_INCLUDE_DIRECTORIES
            $<TARGET_PROPERTY:${_dep_real},INTERFACE_INCLUDE_DIRECTORIES>
      )
    endif()
  endforeach()
endif()

#
# exaero aerosol utility (optional, controlled by CATCHEM_BUILD_EXAERO)
#

if(CATCHEM_BUILD_EXAERO)
  # exaero lives in the MKPP dependency tree (src/external/mkpp/src/exaero)
  # and is consumed through it, never re-vendored.  MKPP's own build graph
  # does not add this subdirectory, so there is no double-add regardless of
  # CATCHEM_BUILD_MKPP.  CACHE ... FORCE: exaero declares each knob with
  # option(), which reads the cache, so a plain set() would be overridden.
  # The chemistry driver stays OFF (its inputs are not restored upstream),
  # and exaero's standalone test runners stay OFF -- CATChem gates its own
  # tests behind CATCHEM_BUILD_TESTING and re-runs the ABI surface the
  # exaero suite already certifies upstream.
  set(EXAERO_BUILD_CHEMISTRY OFF CACHE BOOL "" FORCE)
  set(EXAERO_BUILD_TESTS OFF CACHE BOOL "" FORCE)

  # exaero's build graph bakes the pinned GEOSmie optics tables into a C++
  # header through a Python generator (numpy + netCDF4) whenever the
  # committed artifact is older than its inputs.  Locate the interpreter here
  # -- exaero's own REQUIRED find then adopts it -- and probe the two import
  # prerequisites at configure time so the failure names the missing package
  # instead of surfacing mid-build.
  find_package(Python3 REQUIRED COMPONENTS Interpreter)
  execute_process(
    COMMAND ${Python3_EXECUTABLE} -c "import netCDF4, numpy"
    RESULT_VARIABLE _exaero_py_probe
    ERROR_QUIET
    OUTPUT_QUIET
  )
  if(NOT _exaero_py_probe EQUAL 0)
    message(
      FATAL_ERROR
      "CATCHEM_BUILD_EXAERO=ON requires the Python packages netCDF4 and "
      "numpy (the exaero build-time MIE-table generator), but "
      "${Python3_EXECUTABLE} cannot import them. Install them into that "
      "interpreter (e.g. ${Python3_EXECUTABLE} -m pip install netCDF4 "
      "numpy), point CMake at a conforming interpreter with "
      "-DPython3_EXECUTABLE=<path>, or configure with "
      "-DCATCHEM_BUILD_EXAERO=OFF."
    )
  endif()

  # The parent scope already created the Kokkos targets (the
  # CATCHEM_ENABLE_KOKKOS block in the top-level CMakeLists runs before this
  # file is added) and the yaml-cpp target above, so exaero's guarded
  # find_package(Kokkos) adopts the existing targets and its
  # find_package(yaml-cpp REQUIRED) resolves against the vendored copy.
  add_subdirectory(
    ${CMAKE_CURRENT_SOURCE_DIR}/mkpp/src/exaero
    ${CMAKE_CURRENT_BINARY_DIR}/exaero-build
  )

  # exaero is third-party code: re-export its include directories as SYSTEM so
  # CATChem's Debug warning-as-error gate never fires on findings inside the
  # headers.  Alias targets cannot carry properties, so resolve to the real
  # target first, mirroring the MUSICA/MKPP loops above.
  foreach(_dep exaero)
    if(TARGET ${_dep})
      get_target_property(_dep_real ${_dep} ALIASED_TARGET)
      if(NOT _dep_real)
        set(_dep_real ${_dep})
      endif()
      set_property(
        TARGET ${_dep_real}
        APPEND
        PROPERTY
          INTERFACE_SYSTEM_INCLUDE_DIRECTORIES
            $<TARGET_PROPERTY:${_dep_real},INTERFACE_INCLUDE_DIRECTORIES>
      )
    endif()
  endforeach()
endif()

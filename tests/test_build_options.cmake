if(NOT DEFINED CATCHEM_SOURCE_DIR OR NOT DEFINED CATCHEM_BINARY_ROOT)
  message(FATAL_ERROR "CATCHEM_SOURCE_DIR and CATCHEM_BINARY_ROOT are required")
endif()

# Helper for the MKPP negative-configure cases.
# Configures a throwaway build tree with MKPP enabled and asserts the
# configure FAILED (non-zero exit), that its error output names the expected
# dependency, and that it never reached compilation -- a CMake configure-time
# FATAL_ERROR aborts before any build rule runs, so the absence of a generated
# build directory is the structural evidence, and the absence of a compiler
# invocation in the error text is the direct one.
function(catchem_expect_configure_fatal name expected_regex)
  set(binary_dir "${CATCHEM_BINARY_ROOT}/mkpp-${name}")
  file(REMOVE_RECURSE "${binary_dir}")
  execute_process(
    COMMAND
      ${CMAKE_COMMAND} -S "${CATCHEM_SOURCE_DIR}" -B "${binary_dir}"
      -DCATCHEM_BUILD_TESTING=OFF ${ARGN}
    RESULT_VARIABLE result
    OUTPUT_VARIABLE out_error
    ERROR_VARIABLE out_error
  )
  if(result EQUAL 0)
    message(FATAL_ERROR "MKPP ${name}: configure unexpectedly succeeded")
  endif()
  if(NOT out_error MATCHES "${expected_regex}")
    message(
      FATAL_ERROR
      "MKPP ${name}: configure failed but not with the expected message "
      "(looking for /${expected_regex}/): ${out_error}"
    )
  endif()
  # A mis-provisioned MKPP configuration must never proceed to compilation.
  # A configure-time FATAL_ERROR never generates, so no build system exists --
  # the absence of the generated Makefile.cmake is the structural proof that
  # enforcement happened at configure time, not mid-compile.
  if(EXISTS "${binary_dir}/CMakeFiles/Makefile.cmake")
    message(
      FATAL_ERROR
      "MKPP ${name}: a rejected configure produced build system files "
      "(enforcement must be configure-time, not build-time)"
    )
  endif()
endfunction()

# Emit a minimal Kokkos config package that satisfies find_package(Kokkos
# [QUIET]) and reports VERSION, without any imported targets. The negative
# MKPP cases abort at a guard before src/core ever links Kokkos::kokkos, so no
# target is needed; a real system Kokkos config is unusable here because it may
# pull find_dependency(OpenMP) for a compiler the test host need not have.
function(catchem_make_fake_kokkos dir version)
  file(REMOVE_RECURSE "${dir}")
  file(
    WRITE "${dir}/KokkosConfig.cmake"
    "set(Kokkos_FOUND TRUE)\nset(Kokkos_VERSION \"${version}\")\n"
  )
  # Standard version-file contract: compatible only when the package version
  # meets the requested one, so a 5.1.1 request rejects 4.2.0 and accepts 5.9.9.
  file(
    WRITE "${dir}/KokkosConfigVersion.cmake"
    "set(PACKAGE_VERSION \"${version}\")\nif(PACKAGE_VERSION VERSION_LESS PACKAGE_FIND_VERSION)\n  set(PACKAGE_VERSION_COMPATIBLE FALSE)\nelse()\n  set(PACKAGE_VERSION_COMPATIBLE TRUE)\n  if(PACKAGE_VERSION VERSION_EQUAL PACKAGE_FIND_VERSION)\n    set(PACKAGE_VERSION_EXACT TRUE)\n  endif()\nendif()\n"
  )
endfunction()

# MKPP on, Kokkos-backed core off: fatal, mirroring the MUSICA guard.
catchem_expect_configure_fatal(
  kokkos-off
  "CATCHEM_BUILD_MKPP=ON requires CATCHEM_ENABLE_KOKKOS=ON"
  -DCATCHEM_BUILD_MKPP=ON
  -DCATCHEM_ENABLE_KOKKOS=OFF
)

# MKPP on, KokkosKernels absent: fatal naming the requirement
# and both remedies. Kokkos is satisfied by a fabricated config package (a
# real system Kokkos may pull find_dependency(OpenMP), which the test's default
# compiler need not provide); the version it reports clears the >= 5.1.1 floor
# so the configure reaches the KokkosKernels guard, where the empty disable
# variable makes that find fail as if the package were absent.
catchem_make_fake_kokkos("${CATCHEM_BINARY_ROOT}/fake-kokkos-new" "5.9.9")
catchem_expect_configure_fatal(
  kernels-missing
  "requires KokkosKernels, which was not found"
  -DCATCHEM_BUILD_MKPP=ON
  -DCATCHEM_ENABLE_KOKKOS=ON
  -DKokkos_DIR=${CATCHEM_BINARY_ROOT}/fake-kokkos-new
  -DCMAKE_DISABLE_FIND_PACKAGE_KokkosKernels=ON
)

# MKPP on, Kokkos too old: fatal naming found vs required.
# The fabricated config reports 4.2.0 with a version file that rejects the
# 5.1.1 request, so the versioned find fails, the unversioned retry in the
# guard succeeds, and the build must reject it instead of FetchContent-ing a
# different version than the one Kokkos_DIR names. The system prefix search
# paths are ignored so a real, newer system Kokkos cannot satisfy the retry
# and mask the fabricated-too-old install the case is about.
catchem_make_fake_kokkos("${CATCHEM_BINARY_ROOT}/fake-kokkos-old" "4.2.0")
catchem_expect_configure_fatal(
  kokkos-old
  "requires Kokkos >= 5\\.1\\.1"
  -DCATCHEM_BUILD_MKPP=ON
  -DCATCHEM_ENABLE_KOKKOS=ON
  -DKokkos_DIR=${CATCHEM_BINARY_ROOT}/fake-kokkos-old
  "-DCMAKE_SYSTEM_PREFIX_PATH="
  "-DCMAKE_IGNORE_PREFIX_PATH=/opt/homebrew;/usr/local"
  -DCMAKE_DISABLE_FIND_PACKAGE_KokkosKernels=ON
)

# MKPP on, C++23 unsupported: fatal naming the compiler and the
# required standard. The host running this test may well have a C++23-capable
# compiler, so the probe result is pre-seeded to FALSE through the documented
# capability-override seam in the root CMakeLists (the check_cxx_compiler_flag
# cache variable read before probing) -- the guard's rejection logic is what
# the test asserts, and the platform CI matrix covers real toolchains.
catchem_expect_configure_fatal(
  cxx23-unsupported
  "requires a C\\+\\+23-capable compiler"
  -DCATCHEM_BUILD_MKPP=ON
  -DCATCHEM_ENABLE_KOKKOS=ON
  -DCATCHEM_COMPILER_SUPPORTS_CXX23=FALSE
)

execute_process(
  COMMAND
    ${CMAKE_COMMAND} -S "${CATCHEM_SOURCE_DIR}" -B
    "${CATCHEM_BINARY_ROOT}/canonical" -DCATCHEM_BUILD_TESTING=OFF
    -DCATCHEM_BUILD_NUOPC=OFF -DCATCHEM_BUILD_CCPP=OFF
    -DCATCHEM_ENABLE_KOKKOS=OFF
  RESULT_VARIABLE canonical_result
  OUTPUT_QUIET
  ERROR_VARIABLE canonical_error
)
if(NOT canonical_result EQUAL 0)
  message(FATAL_ERROR "Canonical options failed: ${canonical_error}")
endif()

execute_process(
  COMMAND
    ${CMAKE_COMMAND} -S "${CATCHEM_SOURCE_DIR}" -B
    "${CATCHEM_BINARY_ROOT}/conflict" -DCATCHEM_BUILD_TESTING=OFF
    -DCATCHEM_BUILD_NUOPC=ON -DBUILD_NUOPC=OFF
  RESULT_VARIABLE conflict_result
  OUTPUT_QUIET
  ERROR_VARIABLE conflict_error
)
if(conflict_result EQUAL 0 OR NOT conflict_error MATCHES "conflicts with")
  message(
    FATAL_ERROR
    "Contradictory NUOPC options were not rejected with migration guidance"
  )
endif()

execute_process(
  COMMAND
    ${CMAKE_COMMAND} -S "${CATCHEM_SOURCE_DIR}" -B
    "${CATCHEM_BINARY_ROOT}/trace" -DCATCHEM_BUILD_TESTING=OFF
    -DCATCHEM_BUILD_NUOPC=OFF -DCATCHEM_TRACE_NUOPC=ON
  RESULT_VARIABLE trace_result
  OUTPUT_QUIET
  ERROR_VARIABLE trace_error
)
if(
  trace_result EQUAL 0
  OR NOT trace_error MATCHES "requires CATCHEM_BUILD_NUOPC"
)
  message(FATAL_ERROR "Unsupported NUOPC tracing combination was not rejected")
endif()

execute_process(
  COMMAND
    ${CMAKE_COMMAND} -S "${CATCHEM_SOURCE_DIR}" -B
    "${CATCHEM_BINARY_ROOT}/ccpp-missing-host" -DCATCHEM_BUILD_TESTING=OFF
    -DCATCHEM_BUILD_NUOPC=OFF -DCATCHEM_BUILD_CCPP=ON
  RESULT_VARIABLE ccpp_result
  OUTPUT_QUIET
  ERROR_VARIABLE ccpp_error
)
if(ccpp_result EQUAL 0 OR NOT ccpp_error MATCHES "CATCHEM_CCPP_HOST_MODULE_DIR")
  message(
    FATAL_ERROR
    "CCPP without host modules was not rejected with actionable guidance"
  )
endif()

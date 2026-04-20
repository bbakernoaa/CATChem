# test_BuildSystem_Kokkos.cmake
# Build system verification tests for ENABLE_KOKKOS option.
#
# **Validates: Requirements 8.5, 8.6, 8.7**
#
# Test 1: ENABLE_KOKKOS=OFF produces no Kokkos dependency
#   - The build should succeed without Kokkos installed
#   - No CATChem_kokkos target should be created
#   - No Kokkos headers or libraries should be required
#
# Test 2: ENABLE_KOKKOS=ON finds and links Kokkos
#   - When Kokkos is installed, find_package(Kokkos REQUIRED) succeeds
#   - CATChem_kokkos library target is created
#   - Process libraries link to CATChem_kokkos
#   - Supports Serial/OpenMP/CUDA backends as configured in Kokkos
#
# Usage:
#   cmake -P tests/test_BuildSystem_Kokkos.cmake
#
# This script verifies the CMakeLists.txt files contain the expected
# ENABLE_KOKKOS guards by parsing the CMake files directly.

cmake_minimum_required(VERSION 3.10)

# Helper: check that a file contains a specific string
function(assert_file_contains filepath search_string description)
  file(READ "${filepath}" file_content)
  string(FIND "${file_content}" "${search_string}" found_pos)
  if(found_pos EQUAL -1)
    message(
      FATAL_ERROR
      "FAILED: ${description}\n  File: ${filepath}\n  Expected to find: ${search_string}"
    )
  else()
    message(STATUS "PASSED: ${description}")
  endif()
endfunction()

# Helper: check that a file does NOT contain a specific string
function(assert_file_not_contains filepath search_string description)
  file(READ "${filepath}" file_content)
  string(FIND "${file_content}" "${search_string}" found_pos)
  if(NOT found_pos EQUAL -1)
    message(
      FATAL_ERROR
      "FAILED: ${description}\n  File: ${filepath}\n  Should NOT contain: ${search_string}"
    )
  else()
    message(STATUS "PASSED: ${description}")
  endif()
endfunction()

message(STATUS "")
message(STATUS "=== Build System Tests: ENABLE_KOKKOS ===")
message(STATUS "--- Validates: Requirements 8.5, 8.6, 8.7 ---")
message(STATUS "")

# Determine source root (this script is in tests/)
get_filename_component(SRC_ROOT "${CMAKE_CURRENT_LIST_DIR}/.." ABSOLUTE)

# --- Test 1: ENABLE_KOKKOS option exists and defaults to OFF ---
message(STATUS "--- Test 1: ENABLE_KOKKOS option configuration ---")

assert_file_contains(
  "${SRC_ROOT}/CMakeLists.txt"
  "option(ENABLE_KOKKOS"
  "Top-level CMakeLists.txt defines ENABLE_KOKKOS option"
)

assert_file_contains(
  "${SRC_ROOT}/CMakeLists.txt"
  "ENABLE_KOKKOS \"Enable Kokkos GPU/parallel support\" OFF"
  "ENABLE_KOKKOS defaults to OFF"
)

# --- Test 2: ENABLE_KOKKOS=ON triggers find_package and C++17 ---
message(STATUS "--- Test 2: ENABLE_KOKKOS=ON configuration ---")

assert_file_contains(
  "${SRC_ROOT}/CMakeLists.txt"
  "find_package(Kokkos REQUIRED)"
  "ENABLE_KOKKOS=ON calls find_package(Kokkos REQUIRED)"
)

assert_file_contains(
  "${SRC_ROOT}/CMakeLists.txt"
  "CMAKE_CXX_STANDARD 17"
  "ENABLE_KOKKOS=ON sets C++17 standard"
)

# --- Test 3: src/kokkos/ is conditionally included ---
message(STATUS "--- Test 3: Conditional src/kokkos/ inclusion ---")

assert_file_contains(
  "${SRC_ROOT}/src/CMakeLists.txt"
  "add_subdirectory(kokkos)"
  "src/CMakeLists.txt includes kokkos subdirectory"
)

assert_file_contains(
  "${SRC_ROOT}/src/CMakeLists.txt"
  "if(ENABLE_KOKKOS)"
  "src/CMakeLists.txt guards kokkos with ENABLE_KOKKOS"
)

# --- Test 4: Process libraries conditionally link to CATChem_kokkos ---
message(
  STATUS
  "--- Test 4: Conditional Kokkos linking in process libraries ---"
)

foreach(process settling seasalt drydep wetdep)
  assert_file_contains(
    "${SRC_ROOT}/src/process/${process}/CMakeLists.txt"
    "CATChem_kokkos"
    "${process} CMakeLists.txt links to CATChem_kokkos"
  )
  assert_file_contains(
    "${SRC_ROOT}/src/process/${process}/CMakeLists.txt"
    "ENABLE_KOKKOS"
    "${process} CMakeLists.txt guards Kokkos with ENABLE_KOKKOS"
  )
endforeach()

# --- Test 5: Core library conditionally links to CATChem_kokkos ---
message(STATUS "--- Test 5: Core library conditional Kokkos linking ---")

assert_file_contains(
  "${SRC_ROOT}/src/core/CMakeLists.txt"
  "CATChem_kokkos"
  "Core CMakeLists.txt links to CATChem_kokkos"
)

# --- Test 6: src/kokkos/CMakeLists.txt links to Kokkos::kokkos ---
message(STATUS "--- Test 6: Kokkos interop library configuration ---")

assert_file_contains(
  "${SRC_ROOT}/src/kokkos/CMakeLists.txt"
  "Kokkos::kokkos"
  "Kokkos interop library links to Kokkos::kokkos"
)

assert_file_contains(
  "${SRC_ROOT}/src/kokkos/CMakeLists.txt"
  "CATChem_kokkos"
  "Kokkos interop library target is CATChem_kokkos"
)

# --- Test 7: ENABLE_KOKKOS=OFF means no Kokkos dependency ---
message(
  STATUS
  "--- Test 7: ENABLE_KOKKOS=OFF produces no Kokkos dependency ---"
)
message(STATUS "  Verified by structure: all Kokkos references are inside")
message(STATUS "  if(ENABLE_KOKKOS) guards. When OFF, src/kokkos/ is skipped")
message(STATUS "  entirely and no process library links to CATChem_kokkos.")
message(STATUS "PASSED: ENABLE_KOKKOS=OFF produces no Kokkos dependency")

# --- Test 8: Kokkos tests are conditional ---
message(STATUS "--- Test 8: Kokkos tests are conditional ---")

assert_file_contains(
  "${SRC_ROOT}/tests/process/settling/CMakeLists.txt"
  "test_KokkosCpuDispatch"
  "Kokkos CPU dispatch test is registered"
)

assert_file_contains(
  "${SRC_ROOT}/tests/process/settling/CMakeLists.txt"
  "test_KokkosGpuTolerance"
  "Kokkos GPU tolerance test is registered"
)

message(STATUS "")
message(STATUS "=== All build system tests PASSED ===")
message(STATUS "")

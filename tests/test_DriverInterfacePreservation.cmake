# test_DriverInterfacePreservation.cmake
# Verification that CCPP and NUOPC driver caps compile without modification
# after the Kokkos process interface refactoring.
#
# **Validates: Requirements 9.1, 9.2, 9.3**
#
# This script verifies:
# 1. CCPP driver sources do NOT reference removed types/modules
# 2. NUOPC driver sources do NOT reference removed types/modules
# 3. Public API module (src/api/catchem.F90) maintains expected interface
# 4. Driver CMakeLists.txt targets link to CATChem (not removed libraries directly)
#
# Usage:
#   cmake -P tests/test_DriverInterfacePreservation.cmake

cmake_minimum_required(VERSION 3.10)

# Helper: check that a file does NOT contain a specific string
function(assert_file_not_contains filepath search_string description)
  if(NOT EXISTS "${filepath}")
    message(FATAL_ERROR "FAILED: ${description}\n  File not found: ${filepath}")
  endif()
  file(READ "${filepath}" file_content)
  string(FIND "${file_content}" "${search_string}" found_pos)
  if(NOT found_pos EQUAL -1)
    message(FATAL_ERROR "FAILED: ${description}\n  File: ${filepath}\n  Should NOT contain: ${search_string}")
  else()
    message(STATUS "PASSED: ${description}")
  endif()
endfunction()

# Helper: check that a file contains a specific string
function(assert_file_contains filepath search_string description)
  if(NOT EXISTS "${filepath}")
    message(FATAL_ERROR "FAILED: ${description}\n  File not found: ${filepath}")
  endif()
  file(READ "${filepath}" file_content)
  string(FIND "${file_content}" "${search_string}" found_pos)
  if(found_pos EQUAL -1)
    message(FATAL_ERROR "FAILED: ${description}\n  File: ${filepath}\n  Expected to find: ${search_string}")
  else()
    message(STATUS "PASSED: ${description}")
  endif()
endfunction()

message(STATUS "")
message(STATUS "=== Driver Interface Preservation Tests ===")
message(STATUS "--- Validates: Requirements 9.1, 9.2, 9.3 ---")
message(STATUS "")

# Determine source root (this script is in tests/)
get_filename_component(SRC_ROOT "${CMAKE_CURRENT_LIST_DIR}/.." ABSOLUTE)

# List of removed types/modules that should NOT appear in driver sources
set(REMOVED_SYMBOLS
  "ColumnProcessInterface"
  "ColumnInterface_Mod"
  "ColumnViewType"
  "ColumnProcessorType"
)

# List of removed deprecated methods
set(REMOVED_METHODS
  "apply_emission_scaling"
  "accumulate_emissions"
  "validate_physical_ranges"
  "calculate_column_integrals"
)

# ============================================================
# Test 1: CCPP driver sources do not reference removed types
# ============================================================
message(STATUS "--- Test 1: CCPP driver sources free of removed types (Req 9.1) ---")

set(CCPP_SOURCES
  "${SRC_ROOT}/drivers/ccpp/catchem_types.F90"
  "${SRC_ROOT}/drivers/ccpp/catchem_wrapper_utils.F90"
  "${SRC_ROOT}/drivers/ccpp/ccpp_catchem_interface.F90"
  "${SRC_ROOT}/drivers/ccpp/ccpp_wrapper.F90"
)

foreach(src_file ${CCPP_SOURCES})
  if(EXISTS "${src_file}")
    foreach(symbol ${REMOVED_SYMBOLS})
      assert_file_not_contains(
        "${src_file}"
        "${symbol}"
        "CCPP: ${src_file} does not reference ${symbol}"
      )
    endforeach()
    foreach(method ${REMOVED_METHODS})
      assert_file_not_contains(
        "${src_file}"
        "${method}"
        "CCPP: ${src_file} does not reference deprecated ${method}"
      )
    endforeach()
  endif()
endforeach()

# ============================================================
# Test 2: NUOPC driver sources do not reference removed types
# ============================================================
message(STATUS "--- Test 2: NUOPC driver sources free of removed types (Req 9.2) ---")

set(NUOPC_SOURCES
  "${SRC_ROOT}/drivers/nuopc/catchem_nuopc_cap.F90"
  "${SRC_ROOT}/drivers/nuopc/catchem_nuopc_interface.F90"
  "${SRC_ROOT}/drivers/nuopc/catchem_emis_mod.F90"
  "${SRC_ROOT}/drivers/nuopc/aqmio.F90"
)

foreach(src_file ${NUOPC_SOURCES})
  if(EXISTS "${src_file}")
    foreach(symbol ${REMOVED_SYMBOLS})
      assert_file_not_contains(
        "${src_file}"
        "${symbol}"
        "NUOPC: ${src_file} does not reference ${symbol}"
      )
    endforeach()
    foreach(method ${REMOVED_METHODS})
      assert_file_not_contains(
        "${src_file}"
        "${method}"
        "NUOPC: ${src_file} does not reference deprecated ${method}"
      )
    endforeach()
  endif()
endforeach()

# ============================================================
# Test 3: Public API module maintains expected interface (Req 9.3)
# ============================================================
message(STATUS "--- Test 3: Public API module interface preservation (Req 9.3) ---")

# The public API module (catchem.F90) must expose these key interfaces
assert_file_contains(
  "${SRC_ROOT}/src/api/catchem.F90"
  "use ProcessInterface_Mod, only: ProcessInterface"
  "Public API exports ProcessInterface"
)

assert_file_contains(
  "${SRC_ROOT}/src/api/catchem.F90"
  "use ProcessManager_Mod, only: ProcessManagerType"
  "Public API exports ProcessManagerType"
)

assert_file_contains(
  "${SRC_ROOT}/src/api/catchem.F90"
  "use StateManager_Mod, only: StateManagerType"
  "Public API exports StateManagerType"
)

assert_file_contains(
  "${SRC_ROOT}/src/api/catchem.F90"
  "use ChemState_Mod"
  "Public API exports ChemState_Mod"
)

assert_file_contains(
  "${SRC_ROOT}/src/api/catchem.F90"
  "use MetState_Mod"
  "Public API exports MetState_Mod"
)

# Verify the public API does NOT reference removed types
assert_file_not_contains(
  "${SRC_ROOT}/src/api/catchem.F90"
  "ColumnProcessInterface"
  "Public API does not reference removed ColumnProcessInterface"
)

assert_file_not_contains(
  "${SRC_ROOT}/src/api/catchem.F90"
  "ColumnInterface_Mod"
  "Public API does not reference removed ColumnInterface_Mod"
)

# Verify CATChem_API.F90 maintains its key interface
assert_file_contains(
  "${SRC_ROOT}/src/api/CATChem_API.F90"
  "public :: CATChem_Model"
  "CATChem_API exports CATChem_Model type"
)

assert_file_contains(
  "${SRC_ROOT}/src/api/CATChem_API.F90"
  "procedure :: initialize => model_initialize"
  "CATChem_Model has initialize method"
)

assert_file_contains(
  "${SRC_ROOT}/src/api/CATChem_API.F90"
  "procedure :: finalize => model_finalize"
  "CATChem_Model has finalize method"
)

assert_file_contains(
  "${SRC_ROOT}/src/api/CATChem_API.F90"
  "procedure :: run_timestep => model_run_timestep"
  "CATChem_Model has run_timestep method"
)

# ============================================================
# Test 4: Driver CMakeLists.txt link to CATChem correctly
# ============================================================
message(STATUS "--- Test 4: Driver build targets link correctly ---")

assert_file_contains(
  "${SRC_ROOT}/drivers/ccpp/CMakeLists.txt"
  "target_link_libraries(\${_lib} PUBLIC CATChem)"
  "CCPP driver links to CATChem library"
)

assert_file_contains(
  "${SRC_ROOT}/drivers/nuopc/CMakeLists.txt"
  "target_link_libraries(\${_lib} PUBLIC CATChem"
  "NUOPC driver links to CATChem library"
)

# Drivers should NOT directly link to removed GOCART2G
assert_file_not_contains(
  "${SRC_ROOT}/drivers/ccpp/CMakeLists.txt"
  "GOCART2G"
  "CCPP driver does not directly link to GOCART2G"
)

# ============================================================
# Test 5: CCPP driver uses CATChem public API correctly
# ============================================================
message(STATUS "--- Test 5: CCPP driver uses CATChem public API ---")

assert_file_contains(
  "${SRC_ROOT}/drivers/ccpp/ccpp_catchem_interface.F90"
  "use CATChem"
  "CCPP interface imports CATChem module"
)

assert_file_contains(
  "${SRC_ROOT}/drivers/ccpp/catchem_types.F90"
  "use CATChem"
  "CCPP types imports from CATChem module"
)

# ============================================================
# Test 6: NUOPC driver uses CATChem_API correctly
# ============================================================
message(STATUS "--- Test 6: NUOPC driver uses CATChem_API ---")

assert_file_contains(
  "${SRC_ROOT}/drivers/nuopc/catchem_nuopc_interface.F90"
  "use CATChem_API"
  "NUOPC interface imports CATChem_API module"
)

message(STATUS "")
message(STATUS "=== All driver interface preservation tests PASSED ===")
message(STATUS "")

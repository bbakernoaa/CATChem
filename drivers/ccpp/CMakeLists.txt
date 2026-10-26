set(
  _srcs
  ccpp_catchem_interface.F90
  catchem_ccpp_context.F90
  catchem_ccpp_layout.F90
  catchem_ccpp_conversion.F90
  catchem_ccpp_adapter.F90
  catchem_ccpp_lifecycle.F90
  catchem_dust.F90
  catchem_seasalt.F90
  catchem_wetdep.F90
  catchem_settling.F90
  catchem_so4chem.F90
  catchem_so4chem_ps.F90
  catchem_carbchem.F90
  catchem_carbchem_ps.F90
  catchem_drydep.F90
  catchem_drydep_ps.F90
  catchem_photolysis.F90
  catchem_photolysis_ps.F90
  catchem_gaschem.F90
  catchem_gaschem_ps.F90
  catchem_apply_tendency.F90
)

# The retained monolithic compatibility prototype intentionally accepts host
# fields that the selected-process interface does not consume.  Keep its
# unused-dummy warnings local to this source instead of weakening diagnostics
# for the new adapters.
set_property(
  SOURCE ccpp_catchem_interface.F90
  APPEND
  PROPERTY
    COMPILE_OPTIONS
      "$<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-Wno-unused-dummy-argument>"
)

if(CATCHEM_CCPP_TEST_STUBS)
  add_library(
    catchem_ccpp_test_stubs
    ${PROJECT_SOURCE_DIR}/tests/ccpp_test_stubs.F90
  )
  set_target_properties(
    catchem_ccpp_test_stubs
    PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/ccpp-test-modules
  )
elseif(NOT CATCHEM_CCPP_HOST_MODULE_DIR)
  message(
    FATAL_ERROR
    "CATCHEM_BUILD_CCPP requires CATCHEM_CCPP_HOST_MODULE_DIR containing the host CCPP modules, "
    "or CATCHEM_CCPP_TEST_STUBS=ON for the contract-only test harness"
  )
endif()

set(_lib CCPP_CATChem_interface)

add_library(${_lib} ${_srcs})
target_link_libraries(${_lib} PUBLIC CATChem)
if(CATCHEM_CCPP_TEST_STUBS)
  target_link_libraries(${_lib} PUBLIC catchem_ccpp_test_stubs)
  target_include_directories(
    ${_lib}
    PRIVATE ${CMAKE_BINARY_DIR}/ccpp-test-modules
  )
else()
  target_include_directories(${_lib} PRIVATE ${CATCHEM_CCPP_HOST_MODULE_DIR})
  if(CATCHEM_CCPP_HOST_TARGET)
    if(NOT TARGET ${CATCHEM_CCPP_HOST_TARGET})
      message(
        FATAL_ERROR
        "CATCHEM_CCPP_HOST_TARGET='${CATCHEM_CCPP_HOST_TARGET}' is not a CMake target"
      )
    endif()
    target_link_libraries(${_lib} PUBLIC ${CATCHEM_CCPP_HOST_TARGET})
  endif()
endif()

set_target_properties(
  ${_lib}
  PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include
)
target_compile_options(${_lib} PRIVATE ${_catchem_compiler_options})

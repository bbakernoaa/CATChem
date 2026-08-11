foreach(opt IN LISTS _catchem_compiler_options)
  add_compile_options("$<$<COMPILE_LANGUAGE:Fortran>:${opt}>")
endforeach()

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/catchem_test_config.hpp.in"
  "${CMAKE_CURRENT_BINARY_DIR}/catchem_test_config.hpp"
)

include_directories("${CMAKE_CURRENT_BINARY_DIR}")

# Automatically link the musica configs folder so that CTest can find relative NetCDF files on GHA CI
if(NOT EXISTS "${CMAKE_CURRENT_BINARY_DIR}/configs")
  file(
    CREATE_LINK
      "${PROJECT_SOURCE_DIR}/src/external/musica/configs"
      "${CMAKE_CURRENT_BINARY_DIR}/configs"
    SYMBOLIC
  )
endif()

# Restore compilation of foundational Fortran testing helper library
add_library(testing testing_mod.f90)
target_link_libraries(testing PRIVATE CATChem_core)
set_target_properties(
  testing
  PROPERTIES
    Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include
    LINKER_LANGUAGE Fortran
)
target_include_directories(testing PRIVATE ${CMAKE_BINARY_DIR}/include)

# Restored Low-Level Foundational Unit Tests
macro(add_fortran_test name_test)
  add_executable(${name_test} ${name_test}.f90)
  target_link_libraries(${name_test} PRIVATE CATChem_core testing)
  set_target_properties(
    ${name_test}
    PROPERTIES
      Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include
      LINKER_LANGUAGE Fortran
  )
  target_include_directories(${name_test} PRIVATE ${CMAKE_BINARY_DIR}/include)
  add_dependencies(${name_test} CATChem_core)
  add_test(
    NAME ${name_test}
    COMMAND ${name_test}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  )
endmacro()

add_fortran_test(test_Precision)
add_fortran_test(test_Error)
add_fortran_test(test_Constants)
add_fortran_test(test_GridGeometry)
add_fortran_test(test_TimeState)
add_fortran_test(test_MetState)

# High-level API init-sequence test — mirrors the NUOPC cap's usage of
# CATChem_Model (facade construction, met binding, timestep)
if(TARGET CATChem)
  add_executable(test_CATChem_API test_CATChem_API.f90)
  target_link_libraries(test_CATChem_API PRIVATE CATChem CATChem_core)
  set_target_properties(
    test_CATChem_API
    PROPERTIES
      Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include
      LINKER_LANGUAGE Fortran
  )
  target_include_directories(
    test_CATChem_API
    PRIVATE ${CMAKE_BINARY_DIR}/include
  )
  add_dependencies(test_CATChem_API CATChem)
  add_test(
    NAME test_CATChem_API
    COMMAND test_CATChem_API
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  )
endif()

# UnitConversion needs specific include search dirs
add_executable(test_UnitConversion test_UnitConversion.f90)
target_link_libraries(test_UnitConversion PRIVATE CATChem_core testing)
set_target_properties(
  test_UnitConversion
  PROPERTIES
    Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include
    LINKER_LANGUAGE Fortran
)
target_include_directories(
  test_UnitConversion
  PRIVATE ${CMAKE_BINARY_DIR}/include
)
add_dependencies(test_UnitConversion CATChem_core)
add_test(
  NAME test_UnitConversion
  COMMAND test_UnitConversion
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

# C++ Interop and Integration Suite — runs in both build flavors; the
# CATChem_core_cpp link carries Kokkos (or standalone mdspan) transitively.
add_executable(test_catchem_logger test_catchem_logger.cpp)
target_link_libraries(test_catchem_logger PRIVATE CATChem_core_cpp)
add_test(NAME test_catchem_logger COMMAND test_catchem_logger)

add_executable(test_catchem_interop test_catchem_interop.cpp)
target_link_libraries(
  test_catchem_interop
  PRIVATE
    CATChem_core_cpp
    CATChem_core
    CATChem_process_settling
    CATChem_process_seasalt
    CATChem_process_drydep
    CATChem_process_wetdep
    CATChem_process_so4chem
    CATChem_process_dust
    CATChem_process_carbchem
)
set_target_properties(test_catchem_interop PROPERTIES LINKER_LANGUAGE CXX)
add_test(NAME test_catchem_interop COMMAND test_catchem_interop)

# Modern Randomized Property-Based Testing
add_executable(test_catchem_properties test_catchem_properties.cpp)
target_link_libraries(
  test_catchem_properties
  PRIVATE
    CATChem_core_cpp
    CATChem_core
    CATChem_process_settling
    CATChem_process_seasalt
    CATChem_process_drydep
    CATChem_process_wetdep
    CATChem_process_so4chem
    CATChem_process_dust
    CATChem_process_carbchem
)
set_target_properties(test_catchem_properties PROPERTIES LINKER_LANGUAGE CXX)
add_test(NAME test_catchem_properties COMMAND test_catchem_properties)

if(CATCHEM_BUILD_MUSICA)
  # Photolysis Process Unit/Integration Test
  add_executable(test_catchem_photolysis test_catchem_photolysis.cpp)
  target_link_libraries(
    test_catchem_photolysis
    PRIVATE CATChem_core_cpp CATChem_core catchem_process_photolysis yaml-cpp
  )
  set_target_properties(test_catchem_photolysis PROPERTIES LINKER_LANGUAGE CXX)
  add_test(NAME test_catchem_photolysis COMMAND test_catchem_photolysis)

  # Coupled Process Integration Test
  add_executable(test_catchem_gaschem test_catchem_gaschem.cpp)
  target_link_libraries(
    test_catchem_gaschem
    PUBLIC
      CATChem_core_cpp
      CATChem_core
      catchem_process_photolysis
      catchem_process_gaschem
  )
  set_target_properties(test_catchem_gaschem PROPERTIES LINKER_LANGUAGE CXX)
  add_test(NAME test_catchem_gaschem COMMAND test_catchem_gaschem)
endif()

add_executable(test_catchem_gaschem_units test_catchem_gaschem_units.cpp)
add_test(NAME test_catchem_gaschem_units COMMAND test_catchem_gaschem_units)

# ESMF-backed NUOPC transform test — builds and runs ONLY when the cap is
# built (CATCHEM_BUILD_NUOPC=ON). cc_nuopc links ESMF/NetCDF/MPI PUBLIC,
# so its usage requirements arrive transitively; its Fortran modules live
# in the cap's module directory.
if(CATCHEM_BUILD_NUOPC)
  # libesmf.a references the MPI C++ bindings; an executable link needs
  # them explicitly (the cap static library never resolves them itself).
  find_package(MPI COMPONENTS Fortran CXX REQUIRED)
  add_executable(test_nuopc_transform test_nuopc_transform.f90)
  target_link_libraries(
    test_nuopc_transform
    PRIVATE cc_nuopc CATChem CATChem_core MPI::MPI_Fortran MPI::MPI_CXX
  )
  set_target_properties(
    test_nuopc_transform
    PROPERTIES
      Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include
      LINKER_LANGUAGE Fortran
  )
  target_include_directories(
    test_nuopc_transform
    PRIVATE
      ${CMAKE_BINARY_DIR}/include
      $<TARGET_PROPERTY:cc_nuopc,Fortran_MODULE_DIRECTORY>
  )
  # Stage the production field mapping and the test model configs into the
  # test's working directory.
  file(
    COPY
      ${PROJECT_SOURCE_DIR}/drivers/nuopc/CATChem_field_mapping.yml
      ${CMAKE_CURRENT_SOURCE_DIR}/CATChem_new_config.yml
      ${CMAKE_CURRENT_SOURCE_DIR}/CATChem_species.yml
      ${CMAKE_CURRENT_SOURCE_DIR}/CATChem_emission.yml
    DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
  )
  add_test(
    NAME test_nuopc_transform
    COMMAND test_nuopc_transform
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  )
endif()

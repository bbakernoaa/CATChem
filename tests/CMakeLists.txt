foreach(opt IN LISTS _catchem_compiler_options)
  add_compile_options("$<$<COMPILE_LANGUAGE:Fortran>:${opt}>")
endforeach()

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

# New C++ Interop and Integration Suite
if(ENABLE_KOKKOS)
  add_executable(test_catchem_interop test_catchem_interop.cpp)
  target_link_libraries(
    test_catchem_interop
    PRIVATE
      CATChem_core_cpp
      CATChem_core
      Kokkos::kokkos
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
      Kokkos::kokkos
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

  # Photolysis Process Unit/Integration Test
  add_executable(test_catchem_photolysis test_catchem_photolysis.cpp)
  target_link_libraries(
    test_catchem_photolysis
    PRIVATE
      CATChem_core_cpp
      CATChem_core
      Kokkos::kokkos
      catchem_process_photolysis
      yaml-cpp
  )
  set_target_properties(test_catchem_photolysis PROPERTIES LINKER_LANGUAGE CXX)
  add_test(NAME test_catchem_photolysis COMMAND test_catchem_photolysis)
endif()

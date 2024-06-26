add_library(testing testing_mod.f90)
set_target_properties(testing PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)

enable_testing()

add_executable(test_main test_main.F90)
target_link_libraries(test_main PRIVATE CATChem_core)
target_link_libraries(test_main PRIVATE CATChem)
set_target_properties(test_main PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)

add_test(
  NAME test_main
  COMMAND test_main
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

add_test(
  NAME test_main_missing_config
  COMMAND test_main
  WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
set_property(TEST test_main_missing_config PROPERTY WILL_FAIL TRUE)

add_executable(test_dust test_dust.f90)
target_link_libraries(test_dust PRIVATE CATChem_core)
target_link_libraries(test_dust PRIVATE CATChem)
target_link_libraries(test_dust PRIVATE CATChem_process_dust)
target_link_libraries(test_dust PRIVATE testing)
set_target_properties(test_dust PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)

add_test(
  NAME test_ccpr_dust
  COMMAND test_dust
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

add_executable(test_seasalt test_seasalt.F90)
target_link_libraries(test_seasalt PRIVATE CATChem_core)
target_link_libraries(test_seasalt PRIVATE CATChem)
target_link_libraries(test_seasalt PRIVATE CATChem_process_seasalt)
target_link_libraries(test_seasalt PRIVATE testing)
set_target_properties(test_seasalt PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)

add_test(
  NAME test_ccpr_seasalt
  COMMAND test_seasalt
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

add_library(catchem_process_gaschem STATIC catchem_process_gaschem.cpp)

target_include_directories(
  catchem_process_gaschem
  PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${PROJECT_SOURCE_DIR}/src/core
    ${PROJECT_SOURCE_DIR}/src/external/musica/include
    ${CMAKE_BINARY_DIR}/_deps/micm-src/include
    ${CMAKE_BINARY_DIR}/_deps/mechanism_configuration-src/include
)

target_link_libraries(
  catchem_process_gaschem
  PUBLIC CATChem_core_cpp CATChem_core musica yaml-cpp Kokkos::kokkos
)

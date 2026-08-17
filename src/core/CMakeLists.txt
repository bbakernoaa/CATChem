set(
  _cpp_core_srcs
  catchem_core.cpp
  catchem_api.cpp
  catchem_diagnostic.cpp
  catchem_config_manager.cpp
  catchem_grid_manager.cpp
  catchem_state_manager.cpp
  catchem_emission_manager.cpp
  catchem_logger.cpp
)
add_library(CATChem_core_cpp STATIC ${_cpp_core_srcs})
target_link_libraries(CATChem_core_cpp PUBLIC yaml-cpp::yaml-cpp)
target_include_directories(CATChem_core_cpp PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

if(CATCHEM_ENABLE_KOKKOS)
  target_link_libraries(CATChem_core_cpp PUBLIC Kokkos::kokkos)
  target_compile_definitions(CATChem_core_cpp PUBLIC CATCHEM_ENABLE_KOKKOS)
else()
  target_link_libraries(CATChem_core_cpp PUBLIC mdspan::mdspan)
endif()

# Instead of removing CATChem_core, we make it an INTERFACE library that depends on CATChem_core_cpp
# and external libraries, so downstream targets still build without modification.
add_library(CATChem_core INTERFACE)
target_link_libraries(CATChem_core INTERFACE CATChem_core_cpp catchem_utilities)

if(TARGET GOCART2G)
  target_link_libraries(CATChem_core INTERFACE GOCART2G)
endif()
if(TARGET musica)
  target_link_libraries(CATChem_core INTERFACE musica::musica-fortran)
endif()

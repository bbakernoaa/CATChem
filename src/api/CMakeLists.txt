set(_srcs CATChem_API.F90)
add_library(CATChem_api ${_srcs})
target_link_libraries(CATChem_api PUBLIC CATChem_core)
set_target_properties(
  CATChem_api
  PROPERTIES
    Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include
    LINKER_LANGUAGE Fortran
)
target_include_directories(CATChem_api PUBLIC ${CMAKE_BINARY_DIR}/include)

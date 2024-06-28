set(_srcs catchem.F90)

set(_lib CATChem)

add_library(${_lib} ${_srcs})
target_link_libraries(${_lib} PUBLIC CATChem_core)
target_link_libraries(${_lib} PUBLIC CATChem_process_dust)
target_link_libraries(${_lib} PUBLIC CATChem_process_seasalt)
set_target_properties(${_lib} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)

# Kokkos Interop Layer CMakeLists.txt
# Provides C++ dispatch functions callable from Fortran via ISO_C_BINDING
# Only compiled when ENABLE_KOKKOS=ON

# C++ dispatch sources
set(
  _kokkos_cpp_srcs
  kokkos_dispatch_settling.cpp
  kokkos_dispatch_seasalt.cpp
  kokkos_dispatch_drydep.cpp
  kokkos_dispatch_wetdep.cpp
)

# Fortran ISO_C_BINDING interface module
set(_kokkos_f90_srcs KokkosDispatch_Mod.F90)

set(_lib CATChem_kokkos)
add_library(${_lib} ${_kokkos_cpp_srcs} ${_kokkos_f90_srcs})

# Link to Kokkos (found by top-level CMakeLists.txt when ENABLE_KOKKOS=ON)
target_link_libraries(${_lib} PUBLIC Kokkos::kokkos)

# Fortran module output directory
set_target_properties(
  ${_lib}
  PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include
)

# Include directories for Fortran .mod files and C++ headers
target_include_directories(
  ${_lib}
  PUBLIC ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_SOURCE_DIR}/src/core
  PRIVATE ${CMAKE_BINARY_DIR}/include
)

# Compiler-specific Fortran flags (for the .F90 source)
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set_source_files_properties(
    ${_kokkos_f90_srcs}
    PROPERTIES COMPILE_OPTIONS "-fdefault-real-8;-fdefault-double-8"
  )
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set_source_files_properties(
    ${_kokkos_f90_srcs}
    PROPERTIES COMPILE_OPTIONS "-r8"
  )
endif()

# Install targets
install(
  TARGETS ${_lib}
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib
  RUNTIME DESTINATION bin
)

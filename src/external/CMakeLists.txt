# External dependencies

# TODO: GOCART-2G Process Library - currently disabled as source files are missing
# Uncomment when GOCART source files are available
#
# GOCART-2G Process Library
#

set(
  _srcs
  GOCART/Process_Library/GOCART2G_Process.F90
  GOCART/Process_Library/GOCART2G_MieMod.F90
)
set(_lib GOCART2G)

add_library(${_lib} ${_srcs})
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU|G95")
  target_compile_options(${_lib} PRIVATE -ffree-line-length-none)
endif()

find_package(NetCDF REQUIRED COMPONENTS Fortran)

target_link_libraries(${_lib} NetCDF::NetCDF_Fortran)

# Add yaml-cpp as a subdirectory so its target is available
add_subdirectory(yaml-cpp)

set_target_properties(
  ${_lib}
  PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include
)

#
# MICM Fortran API - using submodule v0.11.1.4
#

# Set MUSICA build options before adding the subdirectory
set(MICM_ENABLE_TESTS OFF)
set(MICM_ENABLE_EXAMPLES OFF)
set(MUSICA_BUILD_C_CXX_INTERFACE ON)
set(MUSICA_BUILD_FORTRAN_INTERFACE ON)
set(MUSICA_ENABLE_INSTALL OFF)
set(MUSICA_ENABLE_TESTS OFF)
set(MUSICA_ENABLE_MICM ON)
set(MUSICA_ENABLE_TUVX ON)

# Add MUSICA as a subdirectory (from submodule v0.11.1.4)
add_subdirectory(musica)

# Add our yaml interface (now that yaml-cpp is available from MUSICA)
add_subdirectory(yaml_interface)

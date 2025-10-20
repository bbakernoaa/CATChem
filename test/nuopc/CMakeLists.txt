# CMake test configuration for CATChem NUOPC interface
cmake_minimum_required(VERSION 3.15)

# Test for CATChem NUOPC interface compilation
project(CATChemNUOPCTest LANGUAGES Fortran CXX)

# Set ESMF paths based on homebrew installation
set(ESMF_ROOT "/opt/homebrew/Cellar/esmf/8.8.0")
set(ESMF_MODDIR "${ESMF_ROOT}/mod/modO/Darwin.gfortran.64.openmpi.default")
set(ESMF_LIBDIR "${ESMF_ROOT}/lib/libO/Darwin.gfortran.64.openmpi.default")

# NetCDF paths (from pkg-config)
set(NETCDF_FFLAGS "-I/opt/homebrew/Cellar/netcdf-fortran/4.6.1_1/include -I/opt/homebrew/Cellar/netcdf/4.9.2_2/include")
set(NETCDF_LIBS "-L/opt/homebrew/Cellar/netcdf-fortran/4.6.1_1/lib -lnetcdff")

# MPI (OpenMPI via Homebrew)
set(MPI_ROOT "/opt/homebrew/Cellar/open-mpi/5.0.8")
set(MPI_FFLAGS "-I${MPI_ROOT}/include")
set(MPI_LIBS "-L${MPI_ROOT}/lib -lmpi_usempif08 -lmpi_usempi_ignore_tkr -lmpi_mpifh -lmpi")

# Set compiler (use mpifort for ESMF compatibility)
set(CMAKE_Fortran_COMPILER "mpifort")

# Set compiler flags
set(CMAKE_Fortran_FLAGS "-g -O2 -I${ESMF_MODDIR} ${NETCDF_FFLAGS} -DHAVE_NETCDF=1")
set(CMAKE_Fortran_FLAGS_DEBUG "-g -O0 -fbacktrace")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3")

# Include directories for CATChem source
include_directories(${CMAKE_SOURCE_DIR}/../../src/core)
include_directories(${CMAKE_SOURCE_DIR}/../../src/api)
include_directories(${CMAKE_SOURCE_DIR}/../../drivers/nuopc)
include_directories(${ESMF_MODDIR})

# Include CATChem build directory for compiled modules
include_directories(${CMAKE_SOURCE_DIR}/../../build/include)

# Check if ESMF is available
if(EXISTS "${ESMF_LIBDIR}/libesmf.a" OR EXISTS "${ESMF_LIBDIR}/libesmf.so")
  set(ESMF_FOUND TRUE)
  message(STATUS "ESMF found at: ${ESMF_ROOT}")
else()
  set(ESMF_FOUND FALSE)
  message(WARNING "ESMF not found at expected location: ${ESMF_LIBDIR}")
endif()

# Test targets
if(ESMF_FOUND)
  # Full NUOPC test (requires ESMF)
  add_executable(test_nuopc_cap
    test_nuopc_cap.F90
    ${CMAKE_SOURCE_DIR}/../../src/api/CATChem_API.F90
    ${CMAKE_SOURCE_DIR}/../../drivers/nuopc/aqmio.F90
    ${CMAKE_SOURCE_DIR}/../../drivers/nuopc/catchem_nuopc_interface.F90
    ${CMAKE_SOURCE_DIR}/../../drivers/nuopc/catchem_nuopc_cap.F90
  )
  
  # Link with CATChem core components (using absolute library paths)
  target_link_libraries(test_nuopc_cap PRIVATE 
    ${CMAKE_SOURCE_DIR}/../../build/src/core/libCATChem_core.a
    ${CMAKE_SOURCE_DIR}/../../build/src/process/seasalt/libCATChem_process_seasalt.a
    ${CMAKE_SOURCE_DIR}/../../build/src/external/yaml_interface/libyaml_interface.a
    ${CMAKE_SOURCE_DIR}/../../build/src/external/libcatchem_utilities.a
    ${CMAKE_SOURCE_DIR}/../../build/_deps/yaml-cpp-build/libyaml-cppd.a
  )
  
  # Then link external libraries
  target_link_libraries(test_nuopc_cap PRIVATE
    "-L${ESMF_LIBDIR} -lesmf"
    "${NETCDF_LIBS}"
    "${MPI_LIBS}"
    c++
  )
  
  # Set module directory and include paths (following test suite pattern)
  set_target_properties(test_nuopc_cap
    PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  )
  
  target_include_directories(test_nuopc_cap PRIVATE
    ${CMAKE_BINARY_DIR}/include
    ${CMAKE_SOURCE_DIR}/../../src/core
    ${CMAKE_SOURCE_DIR}/../../src/api  
    ${CMAKE_SOURCE_DIR}/../../drivers/nuopc
    ${ESMF_MODDIR}
  )
  
  # Note: Dependencies managed through library paths since this is standalone
  
  # Add test
  add_test(
    NAME nuopc_cap_test
    COMMAND test_nuopc_cap
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  )
  
  message(STATUS "NUOPC cap test configured with full CATChem integration")
else()
  message(WARNING "ESMF not found - NUOPC tests will be skipped")
endif()

# Copy test configuration files
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/CATChem_test_config.yml
  ${CMAKE_CURRENT_BINARY_DIR}/CATChem_test_config.yml
  COPYONLY
)

configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/CATChem_test_field_mapping.yml
  ${CMAKE_CURRENT_BINARY_DIR}/CATChem_test_field_mapping.yml
  COPYONLY
)

# Create output directory
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/test_output)

enable_testing()
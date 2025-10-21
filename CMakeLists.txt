cmake_minimum_required(VERSION 3.10)

project(CATChem VERSION 0.1.0 LANGUAGES Fortran C CXX)

# Set C++ standard for yaml-cpp compatibility
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include(CMakePrintHelpers)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../CMakeModules/Modules")

# If GNU compiler and Debug build type, add extra debug flags
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Debug)
endif()
cmake_print_variables(
  CMAKE_Fortran_COMPILER
  CMAKE_Fortran_COMPILER_ID
  CMAKE_BUILD_TYPE
)
set(_catchem_compiler_options "")

# Handle Intel compiler deprecation warnings and incompatible flags
if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  # Always disable the deprecation warning for ifort
  list(APPEND _catchem_compiler_options -diag-disable=6379,10448)
  # Also disable problematic flags that might come from external dependencies
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -diag-disable=6379,10448")
  # Disable memory sanitizer to avoid linking issues
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fno-sanitize=all")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fno-sanitize=all")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-sanitize=all")
endif()

if(
  CMAKE_Fortran_COMPILER_ID MATCHES "GNU|G95"
  AND CMAKE_BUILD_TYPE MATCHES "Debug"
)
  list(
    APPEND
    _catchem_compiler_options
    -Og
    -Wall
    -Wextra
    -pedantic
    -fcheck=all
    -ffpe-trap=invalid,zero,overflow
    -fbacktrace
    -fmax-errors=0
  )
elseif(
  CMAKE_Fortran_COMPILER_ID MATCHES "Intel"
  AND CMAKE_BUILD_TYPE MATCHES "Debug"
)
  list(
    APPEND
    _catchem_compiler_options
    -O0
    -warn
    all
    -check
    all
    -implicitnone
    -fpe0
    -traceback
    -error-limit
    0
  )
elseif(
  CMAKE_Fortran_COMPILER_ID MATCHES "IntelLLVM"
  AND CMAKE_BUILD_TYPE MATCHES "Debug"
)
  list(
    APPEND
    _catchem_compiler_options
    -O0
    -warn
    all
    -check
    all
    -implicitnone
    -fpe0
    -traceback
    -error-limit
    0
  )
endif()

add_subdirectory(src)
add_subdirectory(tests)
add_subdirectory(drivers/nuopc)

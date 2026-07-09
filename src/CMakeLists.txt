# Always build external dependencies first (yaml-cpp and yaml_interface are required for core)
add_subdirectory(external)

add_subdirectory(core)

# Make process modules optional (some depend on external libraries)
option(
  BUILD_PROCESSES
  "Build process modules (may require external dependencies)"
  ON
)
if(BUILD_PROCESSES)
  add_subdirectory(process)
endif()

# Kokkos interop layer (only when ENABLE_KOKKOS=ON)
if(ENABLE_KOKKOS)
  add_subdirectory(kokkos)
endif()

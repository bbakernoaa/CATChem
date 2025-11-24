# Settling Velocity Schemes CMakeLists.txt

# Add Stokes scheme
add_library(stokesscheme_scheme StokesschemeScheme_Mod.F90)

# Link dependencies
target_link_libraries(stokesscheme_scheme precision_mod constants)

# Include directories
target_include_directories(
  stokesscheme_scheme
  PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}
)

# Install targets
install(TARGETS stokesscheme_scheme DESTINATION lib)

# Install module files
install(
  FILES ${CMAKE_CURRENT_BINARY_DIR}/stokesscheme_mod.mod
  DESTINATION include
)

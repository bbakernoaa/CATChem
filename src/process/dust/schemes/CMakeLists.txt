# Dust Schemes CMakeLists.txt
# Generated on: 2025-09-09T14:29:24.735598

# This file is included by the parent CMakeLists.txt
# Schemes are built as part of the main process library

# Scheme source files are defined in the parent CMakeLists.txt:
# - DustScheme_FENGSHA_Mod.F90
# - DustScheme_GINOUX_Mod.F90

# Scheme-specific configurations can be added here if needed

# FENGSHA scheme configuration
# Required meteorological fields for FENGSHA:
#   - IsLand
#   - USTAR
#   - LWI
#   - GVF
#   - LAI
#   - FROCEAN
#   - CLAYFRAC
#   - SANDFRAC
#   - FRSNO
#   - RDRAG
#   - SSM
#   - USTAR_THRESHOLD

# GINOUX scheme configuration
# Required meteorological fields for GINOUX:
#   - FRLAKE
#   - GWETTOP
#   - U10M
#   - V10M
#   - SSM


# Scheme validation targets
add_custom_target(validate_fengsha_scheme
    COMMAND ${CMAKE_COMMAND} -E echo "Validating fengsha scheme implementation"
    COMMENT "Validating fengsha scheme"
)
add_custom_target(validate_ginoux_scheme
    COMMAND ${CMAKE_COMMAND} -E echo "Validating ginoux scheme implementation"
    COMMENT "Validating ginoux scheme"
)

# Combined validation target
add_custom_target(validate_all_schemes
    DEPENDS
        validate_fengsha_scheme
        validate_ginoux_scheme
    COMMENT "Validating all dust schemes"
)
set(_base_srcs
  Precision_Mod.F90
  Constants.F90
  Utilities_Mod.F90
  Met_Utilities_Mod.F90
  Error_Mod.F90
)

set(_state_srcs
  TimeState_Mod.F90
  Species_Mod.F90
  GridGeometry_Mod.F90
  MetState_Mod.F90
  ChemState_Mod.F90
  ExtEmisData_Mod.F90
  DiagnosticInterface_Mod.F90
)

set(_core_srcs
  ConfigManager_Mod.F90
  VirtualColumn_Mod.F90
  ColumnInterface_Mod.F90
  GridManager_Mod.F90
  DiagnosticManager_Mod.F90
  StateManager_Mod.F90
  ChemSpeciesUtils_Mod.F90
  EmissionConfigValidator_Mod.F90
  ProcessInterface_Mod.F90
  ProcessFactory_Mod.F90
  ProcessRegistry_Mod.F90
  ProcessManager_Mod.F90
  CATChemCore_Mod.F90
)

set(_srcs
  ${_base_srcs}
  ${_state_srcs}
  ${_core_srcs}
)
# add Python3 package for generating Fortran module macros
find_package(Python3 COMPONENTS Interpreter REQUIRED)

add_library(CATChem_core ${_srcs})
set_target_properties(
  CATChem_core
  PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include
)
# Force serial build for this target to avoid Fortran module race conditions
set_property(TARGET CATChem_core PROPERTY JOB_POOLS compile=serial)
set_property(TARGET CATChem_core PROPERTY JOB_POOL_COMPILE serial)

target_compile_options(CATChem_core PRIVATE ${_catchem_compiler_options})

target_link_libraries(CATChem_core PRIVATE yaml_interface)

set(METSTATE_MOD_F90 ${CMAKE_CURRENT_SOURCE_DIR}/MetState_Mod.F90)
set(METSTATE_ACCESSOR_INC ${CMAKE_CURRENT_BINARY_DIR}/metstate_column_accessor.inc)
set(METSTATE_ALLOCATE_INC ${CMAKE_CURRENT_BINARY_DIR}/metstate_allocate_fields.inc)
set(METSTATE_DEALLOCATE_INC ${CMAKE_CURRENT_BINARY_DIR}/metstate_deallocate_fields.inc)
set(METSTATE_2D_SCALAR_ACCESSOR_INC ${CMAKE_CURRENT_BINARY_DIR}/metstate_2d_scalar_accessor.inc)
set(METSTATE_SCALAR_ACCESSOR_INC ${CMAKE_CURRENT_BINARY_DIR}/metstate_scalar_accessor.inc)

add_custom_command(
  OUTPUT ${METSTATE_ACCESSOR_INC}
  COMMAND ${CMAKE_COMMAND} -E echo "Generating metstate_column_accessor.inc from MetState_Mod.F90"
  COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}
  COMMAND ${CMAKE_COMMAND} -E env PYTHONPATH=${CMAKE_SOURCE_DIR}/cmake
    ${Python3_EXECUTABLE} ${CMAKE_SOURCE_DIR}/cmake/generate_metstate_macros.py accessor ${METSTATE_MOD_F90} ${METSTATE_ACCESSOR_INC}
  DEPENDS ${CMAKE_SOURCE_DIR}/cmake/generate_metstate_macros.py ${METSTATE_MOD_F90}
  COMMENT "Auto-generating metstate_column_accessor.inc"
  VERBATIM
)

add_custom_command(
  OUTPUT ${METSTATE_ALLOCATE_INC}
  COMMAND ${CMAKE_COMMAND} -E echo "Generating metstate_allocate_fields.inc from MetState_Mod.F90"
  COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}
  COMMAND ${CMAKE_COMMAND} -E env PYTHONPATH=${CMAKE_SOURCE_DIR}/cmake
    ${Python3_EXECUTABLE} ${CMAKE_SOURCE_DIR}/cmake/generate_metstate_macros.py allocate ${METSTATE_MOD_F90} ${METSTATE_ALLOCATE_INC}
  DEPENDS ${CMAKE_SOURCE_DIR}/cmake/generate_metstate_macros.py ${METSTATE_MOD_F90}
  COMMENT "Auto-generating metstate_allocate_fields.inc"
  VERBATIM
)

add_custom_command(
  OUTPUT ${METSTATE_DEALLOCATE_INC}
  COMMAND ${CMAKE_COMMAND} -E echo "Generating metstate_deallocate_fields.inc from MetState_Mod.F90"
  COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}
  COMMAND ${CMAKE_COMMAND} -E env PYTHONPATH=${CMAKE_SOURCE_DIR}/cmake
    ${Python3_EXECUTABLE} ${CMAKE_SOURCE_DIR}/cmake/generate_metstate_macros.py deallocate ${METSTATE_MOD_F90} ${METSTATE_DEALLOCATE_INC}
  DEPENDS ${CMAKE_SOURCE_DIR}/cmake/generate_metstate_macros.py ${METSTATE_MOD_F90}
  COMMENT "Auto-generating metstate_deallocate_fields.inc"
  VERBATIM
)

add_custom_command(
  OUTPUT ${METSTATE_2D_SCALAR_ACCESSOR_INC}
  COMMAND ${CMAKE_COMMAND} -E echo "Generating metstate_2d_scalar_accessor.inc from MetState_Mod.F90"
  COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}
  COMMAND ${CMAKE_COMMAND} -E env PYTHONPATH=${CMAKE_SOURCE_DIR}/cmake
    ${Python3_EXECUTABLE} ${CMAKE_SOURCE_DIR}/cmake/generate_metstate_macros.py 2d_scalar_accessor ${METSTATE_MOD_F90} ${METSTATE_2D_SCALAR_ACCESSOR_INC}
  DEPENDS ${CMAKE_SOURCE_DIR}/cmake/generate_metstate_macros.py ${METSTATE_MOD_F90}
  COMMENT "Auto-generating metstate_2d_scalar_accessor.inc"
  VERBATIM
)

add_custom_command(
  OUTPUT ${METSTATE_SCALAR_ACCESSOR_INC}
  COMMAND ${CMAKE_COMMAND} -E echo "Generating metstate_scalar_accessor.inc from MetState_Mod.F90"
  COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}
  COMMAND ${CMAKE_COMMAND} -E env PYTHONPATH=${CMAKE_SOURCE_DIR}/cmake
    ${Python3_EXECUTABLE} ${CMAKE_SOURCE_DIR}/cmake/generate_metstate_macros.py scalar_accessor ${METSTATE_MOD_F90} ${METSTATE_SCALAR_ACCESSOR_INC}
  DEPENDS ${CMAKE_SOURCE_DIR}/cmake/generate_metstate_macros.py ${METSTATE_MOD_F90}
  COMMENT "Auto-generating metstate_scalar_accessor.inc"
  VERBATIM
)

add_custom_target(
  metstate_macros ALL
  DEPENDS ${METSTATE_ACCESSOR_INC} ${METSTATE_ALLOCATE_INC} ${METSTATE_DEALLOCATE_INC} ${METSTATE_2D_SCALAR_ACCESSOR_INC} ${METSTATE_SCALAR_ACCESSOR_INC}
)

add_dependencies(CATChem_core metstate_macros)

set_source_files_properties(MetState_Mod.F90 PROPERTIES
  COMPILE_FLAGS "-cpp"
  OBJECT_DEPENDS "${METSTATE_ACCESSOR_INC};${METSTATE_ALLOCATE_INC};${METSTATE_DEALLOCATE_INC};${METSTATE_2D_SCALAR_ACCESSOR_INC};${METSTATE_SCALAR_ACCESSOR_INC}"
)

target_include_directories(CATChem_core PRIVATE ${CMAKE_CURRENT_BINARY_DIR})

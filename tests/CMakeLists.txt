add_compile_options(${_catchem_compiler_options})

if(ENABLE_KOKKOS)
    add_executable(test_catchem_interop test_catchem_interop.cpp)
    target_link_libraries(test_catchem_interop PRIVATE CATChem_core)
    target_include_directories(test_catchem_interop PRIVATE ${CMAKE_BINARY_DIR}/include)
    add_dependencies(test_catchem_interop CATChem_core)
    
    # Required to link against Kokkos and C++ core
    if(TARGET CATChem_core_cpp)
        target_link_libraries(test_catchem_interop PRIVATE CATChem_core_cpp Kokkos::kokkos yaml-cpp)
    endif()
    
    # Add dependency on all processes
    target_link_libraries(test_catchem_interop PRIVATE CATChem_process_drydep CATChem_process_seasalt CATChem_process_wetdep CATChem_process_so4chem CATChem_process_dust CATChem_process_carbchem CATChem_process_settling)
endif()
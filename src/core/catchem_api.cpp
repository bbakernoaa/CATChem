/**
 * @file catchem_api.cpp
 * @brief Compatibility translation unit for the flat CATChem C API.
 *
 * @details The public BIND(C) endpoints declared in catchem_api.hpp are
 * implemented by subsystem in the sibling catchem_api_lifecycle.cpp,
 * catchem_api_state.cpp, catchem_api_config.cpp, catchem_api_diagnostics.cpp,
 * catchem_api_manifest.cpp and catchem_api_utilities.cpp sources. This file
 * exists only so that a build referencing the aggregate header still pulls in
 * a translation unit; it contributes no symbols of its own.
 */
#include "catchem_api.hpp"

// Compatibility translation unit. Public C exports are organized by subsystem
// in the adjacent catchem_api_*.cpp sources.

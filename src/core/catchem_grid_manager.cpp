/**
 * @file catchem_grid_manager.cpp
 * @brief Out-of-line definitions for GridManager's constructor and coordinate
 *        bindings.
 *
 * @details Documentation is inherited from catchem_grid_manager.hpp; the
 * bindings wrap the caller's pointer in a rank-2 InteropField sized from the
 * stored nx/ny extents, so the host buffer is referenced, never copied.
 */
// src/core/catchem_grid_manager.cpp
#include "catchem_grid_manager.hpp"
#include <vector>

namespace catchem {

    GridManager::GridManager(int nx, int ny, int nz) {
        geometry.nx = nx;
        geometry.ny = ny;
        geometry.nz = nz;
        is_initialized = true;
    }

    void GridManager::bind_lat(double* ptr) {
        geometry.lat = std::make_shared<InteropField<double, 2>>(ptr, std::vector<int>{geometry.nx, geometry.ny});
    }

    void GridManager::bind_lon(double* ptr) {
        geometry.lon = std::make_shared<InteropField<double, 2>>(ptr, std::vector<int>{geometry.nx, geometry.ny});
    }

    void GridManager::bind_area(double* ptr) {
        geometry.grid_area = std::make_shared<InteropField<double, 2>>(ptr, std::vector<int>{geometry.nx, geometry.ny});
    }

} // namespace catchem

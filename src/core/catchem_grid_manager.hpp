/**
 * @file catchem_grid_manager.hpp
 * @brief Static grid dimensions and coordinate fields for a CATChem run.
 *
 * @details Holds the tile extents (nx, ny, nz) together with optional
 * host-allocated latitude, longitude, and cell-area fields wrapped as
 * InteropField views. Like the meteorology bindings, the manager never owns
 * the underlying buffers: the caller (typically a Fortran host or the NUOPC
 * cap) allocates them and must keep them alive for the manager's lifetime.
 * This is a lightweight descriptor; per-column meteorology lives in MetState
 * and is accessed through StateManager instead.
 */
#pragma once
#include "catchem_interop_field.hpp"
#include <memory>

namespace catchem {

    /**
     * @brief Grid extents and the optional shared bindings for static
     *        coordinate fields.
     *
     * @details The InteropField members are shared references over
     * caller-owned host memory; they stay null until the corresponding bind_*
     * call on GridManager. Copying the struct shares the views, not the data.
     */
    struct GridGeometry {
        int nx = 1; ///< Number of grid cells along x (columns per tile row).
        int ny = 1; ///< Number of grid cells along y (tile rows).
        int nz = 1; ///< Number of vertical layers carried with the geometry.

        std::shared_ptr<InteropField<double, 2>> lat;       ///< Latitude [deg], bound by bind_lat(); null until bound.
        std::shared_ptr<InteropField<double, 2>> lon;       ///< Longitude [deg], bound by bind_lon(); null until bound.
        std::shared_ptr<InteropField<double, 2>> grid_area; ///< Cell area [m2], bound by bind_area(); null until bound.
        // dz and z_levels can be added as 1D fields if needed globally
    };

    /**
     * @brief Owner of the static grid description used by drivers and processes.
     *
     * @details Constructed once with the tile extents; coordinate fields are
     * then bound from externally allocated Fortran/C host arrays. The manager
     * holds only shared_ptr views and never frees caller memory. Not
     * thread-safe for concurrent binding; bind during initialization only.
     */
    class GridManager {
    public:
        GridGeometry geometry;       ///< Extents and coordinate views; see GridGeometry.
        bool is_initialized = false; ///< Set true by the constructor; no teardown path exists.

        /**
         * @brief Record the tile extents and mark the manager initialized.
         *
         * @param nx Cells along x; stored as-is, not validated for positivity.
         * @param ny Cells along y; stored as-is, not validated for positivity.
         * @param nz Vertical layers; stored as-is, not validated for positivity.
         *
         * @note The extents are captured by later bind_* calls, so binding
         * non-positive dimensions surfaces as an InteropField construction
         * error, not here.
         */
        GridManager(int nx, int ny, int nz);

        /**
         * @brief Bind externally allocated latitude data (Fortran interop).
         * @param ptr Host array of nx*ny doubles in column-major (LayoutLeft)
         *            order; caller retains ownership and it must outlive the
         *            manager. Must be non-null.
         *
         * @note Replaces any previous lat binding; the old shared_ptr is
         * dropped but its memory was never owned here.
         */
        void bind_lat(double* ptr);

        /// @copydoc bind_lat
        void bind_lon(double* ptr);

        /// @copydoc bind_lat
        void bind_area(double* ptr);
    };

} // namespace catchem

/**
 * @file catchem_emission_manager.hpp
 * @brief In-memory description of external emission sources and their fields.
 *
 * @details Declares the data model behind the emission-mapping YAML file: one
 * EmissionCategory per source sector (e.g. anthropogenic, biomass burning,
 * dust), each holding per-species EmissionField records with the metadata the
 * NUOPC/Fortran driver needs to create MAPL ExtData imports. The C++ side is a
 * pure description container: no file I/O and no interpolation happen here
 * (time interpolation of the actual data lives in
 * drivers/nuopc/catchem_emis_mod.F90), and all buffers are plain std::vector
 * host memory owned by the structs themselves.
 */
#pragma once
#include <map>
#include <memory>
#include <string>
#include <vector>

namespace catchem {

    /**
     * @brief Metadata and (optionally) loaded data for one emission source field.
     *
     * @details Describes a single mapped variable inside an emission
     * category: its identity, grid extents, scaling factor, and either a
     * gridded buffer or point-source arrays. Owns its vector data; copying is
     * a deep copy.
     */
    struct EmissionField {
        std::string field_name;   ///< Short field key as written in the mapping file.
        std::string long_name;    ///< Descriptive long name for diagnostics.
        std::string units;        ///< Field units string, e.g. "kg m-2 s-1".
        int nx = 0;               ///< Grid extent along x; filled by the loader when gridded data is read.
        int ny = 0;               ///< Grid extent along y; filled by the loader when gridded data is read.
        int nz = 1;               ///< Vertical levels of the source file (1 for surface emissions).
        double factors = 1.0;     ///< Multiplicative scaling applied to every value on load.
        bool is_loaded = false;   ///< True once source data has been read into emission_data or the point arrays.
        bool is_valid = false;    ///< True when the field passed the loader's structural checks.
        bool diagnostic = false;  ///< Emit this field to diagnostic output when true.
        bool time_interpolate = true; ///< Request temporal interpolation between file times.
        std::string interpolation_method = "bilinear"; ///< Spatial regrid method name used by the driver.

        // Point source metadata
        int npts = 0;                    ///< Number of point sources; 0 for gridded fields.
        std::vector<double> lat;         ///< Point-source latitudes [deg], length npts.
        std::vector<double> lon;         ///< Point-source longitudes [deg], length npts.
        std::vector<double> stkdm;       ///< Stack diameters [m], length npts.
        std::vector<double> stkht;       ///< Stack heights [m], length npts.
        std::vector<double> stktk;       ///< Stack temperatures [K], length npts.
        std::vector<double> stkve;       ///< Stack exit velocities [m s-1], length npts.
        std::vector<int> ip;             ///< i grid indices of each point source (1-based legacy convention).
        std::vector<int> jp;             ///< j grid indices of each point source (1-based legacy convention).
        std::vector<double> pemis;       ///< Emitted amount per point source, length npts.
        std::vector<double> pbot;        ///< Bottom injection level per point source, length npts.
        std::vector<double> ptop;        ///< Top injection level per point source, length npts.

        /// Gridded buffer laid out nx * ny * nz * n_times (host, row of times last).
        std::vector<double> emission_data;
    };

    /**
     * @brief One external emission sector and its mapped source fields.
     *
     * @details Mirrors the ExtEmisCategoryType of the Fortran NUOPC driver.
     * ConfigManager parses the MAPL temporal options (daily_hold,
     * monthly_anchor) into this same struct so configuration validation and
     * the emission loader share one description; interpolation behaviour is
     * implemented in drivers/nuopc/catchem_emis_mod.F90, not here.
     */
    struct EmissionCategory {
        std::string category_name;    ///< Sector key, e.g. "anthro" or "biomass".
        std::string description;      ///< Human-readable sector description.
        bool is_active = true;        ///< Sector participates in the run when true.
        bool gridded = true;          ///< Source is a regular grid (vs. point sources).
        bool is_2d = true;            ///< Grid is surface-only (nz == 1) when true.
        bool diagnostic = true;       ///< Include sector output in diagnostics when true.
        double global_scale = 1.0;    ///< Multiplicative scaling applied to the whole sector.
        double topfraction = -1.0;    ///< Fraction injected above the surface; negative means "scheme default".
        std::string source_file;      ///< Path of the emission data file for this sector.
        std::string format;           ///< Source file format hint (e.g. netcdf, asc).
        std::string frequency;        ///< Temporal frequency of the source file (MAPL style, e.g. "1day").
        std::string regrid_method = "none";    ///< Spatial regridding method requested from ExtData.
        std::string time_interpolation = "none"; ///< Temporal interpolation method requested from ExtData.
        std::string vertical_dist = "none";      ///< Vertical distribution scheme name.
        // MAPL ExtData temporal options (feature 014, US4).  These mirror the
        // driver's ExtEmisCategoryType for configuration validation and option
        // propagation; the interpolation behaviour itself lives in the Fortran
        // NUOPC driver (drivers/nuopc/catchem_emis_mod.F90).
        bool daily_hold = false;      ///< Hold the daily value instead of interpolating within a day.
        std::string monthly_anchor = "climatological"; ///< Monthly climatology anchoring: "climatological" or "file".

        /// Source fields keyed by field_name; owned by this category.
        std::map<std::string, EmissionField> fields;
    };

    /**
     * @brief Registry of emission categories parsed from configuration.
     *
     * @details A thin owning container: categories are copied in via
     * add_category() and looked up by name. No I/O or interpolation is
     * performed here; the NUOPC driver consumes the descriptions. Not
     * thread-safe for concurrent mutation.
     */
    class EmissionManager {
    public:
        std::map<std::string, EmissionCategory> categories; ///< Categories keyed by category_name.
        bool is_loaded = false; ///< Set true once at least one category has been added.

        EmissionManager() = default;

        /**
         * @brief Insert or replace a category under its own name.
         * @param category Category description; copied into the registry.
         *
         * @note Also flips is_loaded to true; there is no removal path.
         */
        void add_category(const EmissionCategory& category);

        /**
         * @brief Test for the presence of a category.
         * @param name Category key to look up (exact, case-sensitive).
         * @return true when a category with that key has been added.
         */
        bool has_category(const std::string& name) const;

        /**
         * @brief Look up a category without copying it.
         * @param name Category key to look up (exact, case-sensitive).
         * @return Pointer into the internal map, or nullptr when absent.
         *
         * @note The pointer is owned by the manager and invalidated by any
         * later add_category() for the same key; do not cache across mutation.
         */
        const EmissionCategory* get_category(const std::string& name) const;
    };

} // namespace catchem

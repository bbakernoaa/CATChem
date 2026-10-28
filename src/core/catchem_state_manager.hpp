/**
 * @file catchem_state_manager.hpp
 * @brief Central simulation state: field registry, bindings, and derived
 *        meteorology for one column-set.
 *
 * @details StateManager is the hub between a host model (UFS/WRF/MPAS driver,
 * CCPP-SCM cap) and the CATChem processes. It owns the three sub-states
 * (MetState, ChemState, TimeState), a name-keyed registry of InteropField
 * views, and the import-generation counter that distinguishes fresh host data
 * from stale. Host bindings (bind_met_field_2d/3d, bind_unified_chemistry)
 * wrap caller memory in shared views — the host retains ownership and every
 * buffer must outlive the state, since re-binding only swaps the pointer.
 * Derived fields (BXHEIGHT, AIRDEN, AIRDEN_DRY, DELP, OBK, RH, ...) are
 * computed only when the host did not supply a current value for the active
 * generation; buffers CATChem derives itself are allocated into
 * owned_buffers, which the state frees on destruction.
 *
 * Process code should not read the maps directly: prepare_field_access()
 * validates a FieldAccessContract against the stored FieldContract (units,
 * axes, persistence, generation currency), synchronises the field to the
 * requested execution space, and complete_field_access() records which side
 * wrote last so later syncs move data in the right direction. A host that
 * re-imports fields must call begin_import_generation() first, which
 * invalidates every Timestep-persistence met field.
 *
 * Thread-safety: the manager is single-writer per column set; concurrent
 * access from multiple threads requires external serialisation. Kokkos
 * kernels inside the derive_* methods use LayoutLeft views (Fortran order,
 * index (column, level, singleton)); the non-Kokkos fallback loops index the
 * same host buffers as col + level*n_cols.
 *
 * @see catchem_interop_field.hpp for the view/sync machinery,
 * catchem_field_contract.hpp for the contract enums, and
 * catchem_met_utilities.hpp for the physics formulas.
 */
#pragma once
#include "catchem_chem_state.hpp"
#include "catchem_config_manager.hpp"
#include "catchem_constants.hpp"
#include "catchem_interop_field.hpp"
#include "catchem_met_state.hpp"
#include "catchem_met_utilities.hpp"
#include "catchem_physical_validation.hpp"
#include "catchem_time_state.hpp"
#include <algorithm>
#include <cmath>
#include <iostream>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>
#include <yaml-cpp/yaml.h>

namespace catchem {

    /// Diagnostic collection manager attached to a run; owned elsewhere via shared_ptr.
    class DiagnosticManager;

    /**
     * @brief Aggregate root holding grid, met, chemistry, clock, and field registry.
     *
     * @details Lifetime: constructed by the driver after the grid is known,
     * used for the whole run, destroyed after all processes release their
     * views. Sub-states are embedded by value; config and diagnostic managers
     * are optional shared_ptr attachments. The class is copyable only through
     * its members' shared_ptr semantics — treat one instance as the single
     * mutable owner per column set.
     */
    class StateManager {
    public:
        /**
         * @brief Size the state and generate its random trace ID.
         *
         * @param nc Number of flattened grid columns; must match every later
         *           host binding's first extent.
         * @param nl Number of vertical model layers.
         * @param ns Number of chemical species in the mechanism.
         *
         * @note The 8-character trace ID is drawn from rand() without
         * seeding, so it is stable per process but not cryptographically
         * unique; it exists only to tag log records.
         */
        StateManager(int nc, int nl, int ns);

        int column_count() const noexcept { return n_cols; }  ///< Flattened grid columns (nc) the state is sized for.
        int level_count() const noexcept { return n_levels; } ///< Vertical model layers (nl).
        int species_count() const noexcept { return n_species; } ///< Mechanism species count (ns).
        std::size_t current_import_generation() const noexcept {
            return import_generation;
        } ///< Monotonic counter bumped by begin_import_generation().

        MetState& meteorology() noexcept { return met; }             ///< Mutable met field registry.
        const MetState& meteorology() const noexcept { return met; } ///< Read-only met field registry.
        ChemState& chemistry() noexcept { return chem; }             ///< Mutable chemistry/mechanism state.
        const ChemState& chemistry() const noexcept { return chem; } ///< Read-only chemistry/mechanism state.
        TimeState& clock() noexcept { return time; }                 ///< Mutable simulation clock.
        const TimeState& clock() const noexcept { return time; }     ///< Read-only simulation clock.

        std::shared_ptr<ConfigManager> config_manager() const noexcept {
            return config_mgr;
        } ///< Attached config manager, or null. Shared ownership.
        std::shared_ptr<DiagnosticManager> diagnostic_manager() const noexcept {
            return diag_mgr;
        } ///< Attached diagnostic manager, or null. Shared ownership.
        void attach_config_manager(std::shared_ptr<ConfigManager> manager) {
            config_mgr = std::move(manager);
        } ///< Install (or replace, with std::move(nullptr) to detach) the config manager.
        void attach_diagnostic_manager(std::shared_ptr<DiagnosticManager> manager) {
            diag_mgr = std::move(manager);
        } ///< Install (or replace) the diagnostic manager.

        const std::string& configuration_path() const noexcept {
            return config_file_path;
        } ///< Path recorded by the driver, or empty when unset.
        void set_configuration_path(std::string path) {
            config_file_path = std::move(path);
        } ///< Record which configuration file backs this state.
        const std::string& runtime_trace_id() const noexcept {
            return trace_id;
        } ///< 8-character log trace tag generated at construction.
        void set_runtime_trace_id(std::string id) {
            trace_id = std::move(id);
        } ///< Override the trace tag (e.g. from a host run identifier).

        PhysicalValidationPolicy validation_policy() const noexcept {
            return physical_validation_policy;
        } ///< Policy applied by the derive_* physical checks.
        void set_validation_policy(PhysicalValidationPolicy policy) noexcept {
            physical_validation_policy = policy;
        } ///< Install the policy parsed from physical_validation/policy.
        const PhysicalValidationReport& validation_report() const noexcept {
            return physical_validation_report;
        } ///< Findings from the most recent derive_* validation (cleared at each derive start).

        /**
         * @brief Map a field name to its canonical CATChem key.
         *
         * @param name Any accepted spelling: canonical, alias, or long form
         *             (e.g. "TEMPERATURE", "sphum"); case is normalised first.
         * @return The canonical key ("T", "QV", ...) when the name is known,
         *         otherwise the uppercased input unchanged. Unknown names are
         *             never rejected here.
         */
        static std::string canonical_field_name(std::string name) {
            auto key = canonicalize_field_identity(std::move(name));
            static const std::unordered_map<std::string, std::string> aliases{
                {"TEMPERATURE", "T"},
                {"TEMP", "T"},
                {"SPHUM", "QV"},
                {"RELATIVE_HUMIDITY", "RH"},
                {"PRESSURE_MID", "PMID"},
                {"PRESSURE_EDGE", "PEDGE"},
                {"AIR_DENSITY", "AIRDEN"},
                {"AIR_DENSITY_DRY", "AIRDEN_DRY"},
                {"MAIRDEN", "AIRDEN"},
                {"BOX_HEIGHT", "BXHEIGHT"},
                {"DZ", "BXHEIGHT"},
                {"DELZ", "BXHEIGHT"},
                {"LATITUDE", "LAT"},
                {"LONGITUDE", "LON"},
                {"SURFACE_PRESSURE", "PS"},
                {"SKIN_TEMPERATURE", "TS"},
                {"SST", "TS"},
                {"FRICTION_VELOCITY", "USTAR"},
                {"PRESSURE_THICKNESS_OF_ATMOSPHERIC_LAYER", "DELP"},
                {"CLAY_FRACTION", "CLAYFRAC"},
                {"LAKE_FRACTION", "FRLAKE"},
                {"SNOW_FRACTION", "FRSNO"},
                {"VEGETATION_FRACTION", "GVF"},
                {"LEAF_AREA_INDEX", "LAI"},
                {"LAND_WATER_ICE_MASK", "LWI"},
                {"DRAG_COEFFICIENT", "CMM"},
                {"SAND_FRACTION", "SNDFRC"},
                {"SOIL_MOISTURE", "SOILM"},
                {"SURFACE_SOIL_MOISTURE", "GWETTOP"},
                {"U_10M", "U10M"},
                {"V_10M", "V10M"},
                {"THRESHOLD_FRICTION_VELOCITY", "USTAR_THRESHOLD"},
                {"ROUGHNESS_LENGTH", "Z0"},
                {"HPBL", "PBLH"},
                {"OL", "OBK"},
                {"GEOMETRIC_HEIGHT_EDGE", "Z"}};
            const auto found = aliases.find(key);
            return found == aliases.end() ? key : found->second;
        }

        /**
         * @brief Look up a field by name in the state's registries.
         *
         * @tparam Rank Field rank, 1 to 3; selects the registry searched.
         * @param name  Canonical or aliased field name; canonicalised first.
         * @return Shared view over the field, or nullptr when unbound.
         *
         * @note Rank 1 resolves only in the generic fields_1d registry; ranks
         * 2 and 3 fall back to the MetState alias maps, so host-bound
         * meteorology is reachable through the same call.
         */
        template <int Rank> std::shared_ptr<InteropField<double, Rank>> find_field(const std::string& name) const {
            const auto key = canonical_field_name(name);
            if constexpr (Rank == 1) {
                auto found = fields_1d.find(key);
                return found == fields_1d.end() ? nullptr : found->second;
            } else if constexpr (Rank == 2) {
                auto found = fields_2d.find(key);
                if (found != fields_2d.end())
                    return found->second;
                return met.get_2d({key.c_str()});
            } else {
                auto found = fields_3d.find(key);
                if (found != fields_3d.end())
                    return found->second;
                return met.get_3d({key.c_str()});
            }
        }

        /**
         * @brief Validate a scheme's access request and synchronise the field.
         *
         * @param access Contract describing the requested field: canonical
         *               name, expected units, expected axis order (its size
         *               selects the rank), persistence, and execution-space
         *               intent. Borrowed for the call only.
         * @return true when the field exists, its stored contract matches the
         *         requested units (skipped for CONCENTRATION, which is
         *         species-native), axes, and persistence, and — for
         *         Timestep-persistence fields — its data is current in the
         *         active import generation. On success the field has been
         *         synced to the requested space (Device or Host/Either).
         *
         * @note Returns false (no exception) for unknown names, contract
         * mismatches, stale generations, and ranks outside 1-3. The special
         * name CONCENTRATION routes to the unified chemistry array without a
         * generation check. Call complete_field_access() after writing.
         */
        bool prepare_field_access(const FieldAccessContract& access) {
            const auto contract_matches = [&access](const auto& field) {
                const bool concentration = canonical_field_name(access.canonical_name) == "CONCENTRATION";
                return field && (concentration || field->contract.units == access.units) &&
                       field->contract.axes == access.axes && field->contract.persistence == access.persistence;
            };
            const int rank = static_cast<int>(access.axes.size());
            if (canonical_field_name(access.canonical_name) == "CONCENTRATION") {
                if (!contract_matches(chem.conc))
                    return false;
                if (access.execution_space == ExecutionSpaceIntent::Device)
                    chem.conc->sync_to_device();
                else
                    chem.conc->sync_to_host();
                return true;
            }
            if (rank == 1) {
                auto field = find_field<1>(access.canonical_name);
                if (!contract_matches(field) ||
                    (access.persistence == PersistencePolicy::Timestep && !field->is_current(import_generation)))
                    return false;
                if (access.execution_space == ExecutionSpaceIntent::Device)
                    field->sync_to_device();
                else
                    field->sync_to_host();
                return true;
            }
            if (rank == 2) {
                auto field = find_field<2>(access.canonical_name);
                if (!contract_matches(field) ||
                    (access.persistence == PersistencePolicy::Timestep && !field->is_current(import_generation)))
                    return false;
                if (access.execution_space == ExecutionSpaceIntent::Device)
                    field->sync_to_device();
                else
                    field->sync_to_host();
                return true;
            }
            if (rank == 3) {
                auto field = find_field<3>(access.canonical_name);
                if (!contract_matches(field) ||
                    (access.persistence == PersistencePolicy::Timestep && !field->is_current(import_generation)))
                    return false;
                if (access.execution_space == ExecutionSpaceIntent::Device)
                    field->sync_to_device();
                else
                    field->sync_to_host();
                return true;
            }
            return false;
        }

        /**
         * @brief Record that a scheme finished writing a field.
         *
         * @param access The same contract passed to prepare_field_access();
         *               only contracts whose intent includes a write
         *               (Write or ReadWrite) have any effect.
         *
         * @note Marks the field's latest writer as host or device according
         * to the contract's execution-space intent so subsequent syncs move
         * data in the correct direction. Silently does nothing when the field
         * is no longer bound.
         */
        void complete_field_access(const FieldAccessContract& access) {
            if (!access.writes())
                return;
            const bool device = access.execution_space == ExecutionSpaceIntent::Device;
            if (canonical_field_name(access.canonical_name) == "CONCENTRATION") {
                if (chem.conc)
                    device ? chem.conc->mark_device_modified() : chem.conc->mark_host_modified();
                return;
            }
            const int rank = static_cast<int>(access.axes.size());
            if (rank == 1) {
                auto field = find_field<1>(access.canonical_name);
                if (field)
                    device ? field->mark_device_modified() : field->mark_host_modified();
            } else if (rank == 2) {
                auto field = find_field<2>(access.canonical_name);
                if (field)
                    device ? field->mark_device_modified() : field->mark_host_modified();
            } else if (rank == 3) {
                auto field = find_field<3>(access.canonical_name);
                if (field)
                    device ? field->mark_device_modified() : field->mark_host_modified();
            }
        }

        /**
         * @brief Read-only host pointer for a named field.
         *
         * @tparam Rank Field rank, 1 to 3.
         * @param name                Canonical or aliased field name.
         * @param required_generation When non-zero, the field must be current
         *                            in this import generation or the call
         *                            fails; 0 skips the check.
         * @return Host pointer (synced to host, referenced not copied), or
         *         nullptr when the field is missing or stale. The pointer is
         *         valid only until the field's host pointer is rebound or the
         *         backing memory changes.
         */
        template <int Rank>
        const double* read_field(const std::string& name, std::size_t required_generation = 0) const {
            auto field = find_field<Rank>(name);
            if (!field)
                return nullptr;
            if (required_generation != 0 && !field->is_current(required_generation))
                return nullptr;
            return field->host_read();
        }

        /**
         * @brief Writable host pointer for a named field.
         *
         * @tparam Rank Field rank, 1 to 3.
         * @param name                Canonical or aliased field name.
         * @param required_generation When non-zero, the field must be current
         *                            in this import generation or the call
         *                            fails; 0 skips the check.
         * @return Host pointer (synced to host, with the host side marked as
         *         the newest writer). nullptr when the field is missing or
         *         stale.
         */
        template <int Rank> double* write_field(const std::string& name, std::size_t required_generation = 0) {
            auto field = find_field<Rank>(name);
            if (!field)
                return nullptr;
            if (required_generation != 0 && !field->is_current(required_generation))
                return nullptr;
            return field->host_write();
        }

        /**
         * @brief Default persistence policy for a field name.
         *
         * @param name Canonical or aliased field name.
         * @return PersistencePolicy::Persistent for the static coordinates
         *         LAT, LON, and AREA_M2; PersistencePolicy::Timestep for
         *         everything else, including unknown names.
         */
        static PersistencePolicy persistence_for(const std::string& name) {
            const auto key = canonical_field_name(name);
            return (key == "LAT" || key == "LON" || key == "AREA_M2") ? PersistencePolicy::Persistent
                                                                      : PersistencePolicy::Timestep;
        }

        /**
         * @brief Unit string associated with a canonical field name.
         *
         * @param name Canonical or aliased field name.
         * @return The CF-style unit string the registry uses for that field
         *         (temperatures "K", pressures "Pa", densities "kg/m3", winds
         *         "m/s", heights "m", mixing ratios "kg/kg", fractions "1" or
         *         "frac", fluxes "W/m2", and so on), or "" when the name is
         *         not in the table.
         *
         * @note Used only by populate_contract() during binding; a field
         * bound under an unknown name gets an empty units string and will
         * fail any contract check that compares units.
         */
        static std::string units_for(const std::string& name) {
            const auto key = canonical_field_name(name);
            // Temperature
            if (key == "T" || key == "TS" || key == "T2M" || key == "SOILT")
                return "K";
            // Water vapor
            if (key == "QV" || key == "QV2M")
                return "kg/kg";
            // Unitless variables
            if (key == "RH" || key == "CLDFRC" || key == "SUNCOSMID")
                return "1";
            // Pressure related fields
            if (key == "PMID" || key == "PEDGE" || key == "PS" || key == "DELP")
                return "Pa";
            // Height related fields
            if (key == "Z" || key == "ZMID" || key == "BXHEIGHT" || key == "PBLH" || key == "ORO")
                return "m";
            // Density
            if (key == "AIRDEN" || key == "AIRDEN_DRY" || key == "MAIRDEN")
                return "kg/m3";
            // Coordinates
            if (key == "LAT" || key == "LON")
                return "degrees";
            // Cell area
            if (key == "AREA_M2")
                return "m2";
            // Wind fields
            if (key == "U" || key == "V" || key == "U10M" || key == "V10M" || key == "USTAR" ||
                key == "USTAR_THRESHOLD")
                return "m/s";
            // Precipitation
            if (key == "PRECLSC" || key == "PRECCON")
                return "m";
            if (key == "WCA")
                return "m3";
            // Rain / ice flux rates
            if (key == "PFILSAN" || key == "PFLLSAN")
                return "kg/m2/s";
            // Re-evaporation tendency
            if (key == "REEVAPLS")
                return "kg/kg/s";
            // Green Vegetative Fraction
            if (key == "GVF")
                return "frac";
            // Fractional ocean, seaice, snow
            if (key == "FROCEAN" || key == "FRSEAICE" || key == "FRSNO")
                return "frac";
            // Leaf Area index
            if (key == "LAI" || key == "FRLAI")
                return "m2/m2";
            // Land Parameters
            if (key == "CLAYFRAC" || key == "FRLAKE" || key == "LWI" || key == "DLUSE" || key == "DSOILTYPE" ||
                key == "SNDFRC" || key == "GWETTOP" || key == "CLDF" || key == "SSM" || key == "RDRAG" ||
                key == "FRLANDUSE" || key == "ILAND")
                return "1";
            // Sea Surface temperature
            if (key == "SST")
                return "K";
            // Canopy fields
            if (key == "CMM" || key == "RCA")
                return "s/m";
            // Soil Moisture
            if (key == "SOILM")
                return "m3/m3";
            // Roughness and Obhokov length
            if (key == "Z0" || key == "Z0H" || key == "OBK")
                return "m";
            // Heat fluxes and downward shortwave
            if (key == "HFLUX" || key == "EFLUX" || key == "HFLUX_UP" || key == "SWGDN")
                return "W/m2";
            return "";
        }

        /**
         * @brief Alternate spellings registered for a canonical field name.
         *
         * @param name Canonical or aliased field name.
         * @return The alias list stored on the contract (e.g. T yields
         *         TEMPERATURE and TEMP); empty for names without aliases.
         */
        static std::vector<std::string> aliases_for(const std::string& name) {
            const auto key = canonical_field_name(name);
            if (key == "T")
                return {"TEMPERATURE", "TEMP"};
            if (key == "QV")
                return {"SPHUM"};
            if (key == "RH")
                return {"RELATIVE_HUMIDITY"};
            if (key == "PMID")
                return {"PRESSURE_MID"};
            if (key == "PEDGE")
                return {"PRESSURE_EDGE"};
            if (key == "Z")
                return {"GEOMETRIC_HEIGHT_EDGE"};
            if (key == "AIRDEN")
                return {"AIR_DENSITY"};
            if (key == "AIRDEN_DRY")
                return {"AIR_DENSITY_DRY"};
            if (key == "BXHEIGHT")
                return {"DZ", "DELZ"};
            if (key == "LAT")
                return {"LATITUDE"};
            if (key == "LON")
                return {"LONGITUDE"};
            return {};
        }

        /**
         * @brief Fill a field's contract from the name-based metadata tables.
         *
         * @tparam Rank Field rank, 1 to 3.
         * @param name  Canonical or aliased field name; drives canonical_name,
         *              units, aliases, and persistence.
         * @param field Field whose contract member is written in place; must
         *              outlive the call (modified, not copied).
         * @param axes  Semantic axis order for the field's dimensions; moved
         *              into the contract.
         *
         * @note extents are not touched here — they come from the field's
         * construction-time immutable_extents.
         */
        template <int Rank>
        static void populate_contract(const std::string& name, InteropField<double, Rank>& field,
                                      std::vector<SemanticAxis> axes) {
            field.contract.canonical_name = canonical_field_name(name);
            field.contract.units = units_for(name);
            field.contract.axes = std::move(axes);
            field.contract.aliases = aliases_for(name);
            field.contract.persistence = persistence_for(name);
        }

        /**
         * @brief Start a new host import generation and invalidate stale data.
         *
         * @details Increments the import generation counter, then invalidates
         * every bound 2D/3D meteorology field whose persistence is
         * Timestep. Persistent fields (coordinates, areas) and the unified
         * chemistry array keep their generation stamp, so hosts may bind
         * them only once.
         *
         * @note Call before re-binding host data each timestep; fields bound
         * afterwards are stamped with the new generation and are visible to
         * prepare_field_access().
         */
        void begin_import_generation() {
            ++import_generation;
            for (auto& [name, field] : met.fields_2d)
                if (field && field->contract.persistence == PersistencePolicy::Timestep)
                    field->invalidate();
            for (auto& [name, field] : met.fields_3d)
                if (field && field->contract.persistence == PersistencePolicy::Timestep)
                    field->invalidate();
        }

        /**
         * @brief Load a species mechanism file into the chemistry state.
         *
         * @param filename Path to a CATChem species YAML file; resolved as
         *                 written (relative paths depend on the caller's
         *                 working directory, not configuration_path()).
         *
         * @note Rebuilds ChemState wholesale through the attached
         * ConfigManager when one is present; without a config manager a
         * temporary one is used, so mechanism identity and fingerprint fall
         * back to defaults. Throws std::invalid_argument on empty or
         * duplicate species names.
         */
        void load_species_config(const std::string& filename) { chem.load_species_config(filename, config_mgr.get()); }

        /**
         * @brief Bind (or rebind) a host two-dimensional meteorology field.
         *
         * @param name Canonical or aliased field name; stored under its
         *             canonical key plus case aliases in MetState.
         * @param ptr  Contiguous host buffer of n_cols entries (LayoutLeft,
         *             column axis first). The caller retains ownership and
         *             the buffer must outlive the state.
         *
         * @note An existing field keeps its contract and only swaps its host
         * pointer; a new field gets the default contract for its name and is
         * stamped with the current import generation.
         */
        void bind_met_field_2d(const std::string& name, double* ptr) {
            const auto canonical_name = canonical_field_name(name);
            auto field = met.get_2d({canonical_name.c_str()});
            if (field) {
                field->update_host_pointer(ptr);
                field->set_generation(import_generation);
            } else {
                auto new_field = std::make_shared<InteropField<double, 2>>(ptr, std::vector<int>{n_cols, 1});
                populate_contract(canonical_name, *new_field, {SemanticAxis::Column, SemanticAxis::Singleton});
                met.bind_2d_field(canonical_name, new_field);
                new_field->set_generation(import_generation);
            }
        }

        /**
         * @brief Bind a host three-dimensional meteorology field with its vertical-axis contract.
         * @param name Canonical or aliased CATChem meteorology field name.
         * @param ptr Contiguous host pointer for the column-by-vertical data.
         *            Caller retains ownership; must outlive the state.
         *
         * @note Interface-level fields (PEDGE, Z, PFILSAN, PFLLSAN) are bound
         * with n_levels+1 vertical entries and SemanticAxis::Interface; all
         * other names use n_levels and SemanticAxis::Level. Delegates to
         * bind_met_field_3d_contract(), which enforces the axis on rebind.
         */
        void bind_met_field_3d(const std::string& name, double* ptr) {
            std::string upper_name = canonical_field_name(name);
            const bool is_interface =
                upper_name == "PEDGE" || upper_name == "Z" || upper_name == "PFILSAN" || upper_name == "PFLLSAN";
            bind_met_field_3d_contract(name, ptr, is_interface ? n_levels + 1 : n_levels,
                                       is_interface ? SemanticAxis::Interface : SemanticAxis::Level);
        }

        /**
         * @brief Bind a 3D met field with an explicit vertical extent and axis.
         *
         * @param name            Canonical or aliased field name.
         * @param ptr             Contiguous host buffer laid out (column,
         *                        vertical, singleton) in LayoutLeft order;
         *                        caller retains ownership.
         * @param vertical_extent Entries along the vertical axis (n_levels for
         *                        layer fields, n_levels+1 for interfaces, or
         *                        category counts such as the 20 land-use
         *                        slots).
         * @param vertical_axis   Semantic meaning of that axis (Level,
         *                        Interface, SoilLayer, ...).
         *
         * @note Throws std::invalid_argument when rebinding an existing field
         * under a different vertical contract (extent, rank, or axis), so a
         * host cannot silently change a field's geometry mid-run.
         */
        void bind_met_field_3d_contract(const std::string& name, double* ptr, int vertical_extent,
                                        SemanticAxis vertical_axis) {
            const auto canonical_name = canonical_field_name(name);
            auto field = met.get_3d({canonical_name.c_str()});
            if (field) {
                if (field->extent(1) != static_cast<std::size_t>(vertical_extent) || field->contract.axes.size() != 3 ||
                    field->contract.axes[1] != vertical_axis)
                    throw std::invalid_argument("Rebinding field with a different vertical contract: " + name);
                field->update_host_pointer(ptr);
                field->set_generation(import_generation);
            } else {
                auto new_field =
                    std::make_shared<InteropField<double, 3>>(ptr, std::vector<int>{n_cols, vertical_extent, 1});
                populate_contract(canonical_name, *new_field,
                                  {SemanticAxis::Column, vertical_axis, SemanticAxis::Singleton});
                met.bind_3d_field(canonical_name, new_field);
                new_field->set_generation(import_generation);
            }
        }

        /**
         * @brief Host write pointer for the first current 3D met candidate.
         * @param names Candidate field names tried in order by MetState alias
         *              lookup; null entries are skipped.
         * @return Writable host pointer, or nullptr when no candidate is
         *         bound with AvailabilityState::Current.
         *
         * @note The InteropField::host_write() call behind the returned
         * pointer syncs the field to host and marks it host-modified, so
         * writes through it are tracked without an extra sync call.
         */
        double* find_3d_ptr(std::initializer_list<const char*> names) const {
            auto f = met.get_3d(names);
            if (f && f->availability == AvailabilityState::Current)
                return f->host_write();
            return nullptr;
        }

        /**
         * @brief Host write pointer for the first current 2D met candidate.
         * @param names Candidate field names tried in order by MetState alias
         *              lookup; null entries are skipped.
         * @return Writable host pointer, or nullptr when no candidate is
         *         bound with AvailabilityState::Current.
         *
         * @note As with find_3d_ptr(), the host_write() behind the pointer
         * syncs to host and marks the field host-modified.
         */
        double* find_2d_ptr(std::initializer_list<const char*> names) const {
            auto f = met.get_2d(names);
            if (f && f->availability == AvailabilityState::Current)
                return f->host_write();
            return nullptr;
        }

        /**
         * @brief Bind (or rebind) the unified chemistry concentration array.
         *
         * @param ptr Host buffer of n_cols * n_levels * n_species doubles in
         *            LayoutLeft order (column fastest, then level, then
         *            species). The caller retains ownership and the buffer
         *            must outlive the state.
         *
         * @note First bind installs the CONCENTRATION contract: units
         * "species-native" (each species' own configured units), axes
         * (Column, Level, Species), persistence Persistent. Rebinds swap only
         * the host pointer; the generation stamp is not refreshed here.
         */
        void bind_unified_chemistry(double* ptr) {
            if (chem.conc) {
                chem.conc->update_host_pointer(ptr);
            } else {
                chem.conc =
                    std::make_shared<InteropField<double, 3>>(ptr, std::vector<int>{n_cols, n_levels, n_species});
                chem.conc->contract.canonical_name = "CONCENTRATION";
                chem.conc->contract.units = "species-native";
                chem.conc->contract.axes = {SemanticAxis::Column, SemanticAxis::Level, SemanticAxis::Species};
                chem.conc->contract.persistence = PersistencePolicy::Persistent;
            }
        }

        /**
         * @brief Derives layer thicknesses (BXHEIGHT / dz) hydrostatically.
         *
         * Computes the vertical distance of each grid cell based on pressure edges, temperature, and moisture content.
         *
         * @details Skips entirely when a current host-provided BXHEIGHT exists
         * for the active import generation (the host value is authoritative),
         * and returns silently when PEDGE or T is missing or not Current.
         * Before mutating anything it scans every (column, level) point in
         * host order (index col + level*n_cols): PEDGE must be finite,
         * positive, and strictly descending, and T must be finite and
         * positive; violations are recorded in validation_report() and, under
         * PhysicalValidationPolicy::Reject, throw std::domain_error before
         * BXHEIGHT is touched. Under WarnAndClamp/CountAndContinue the kernel
         * clamps instead: T to a 1 K floor, QV to [0, 0.9999] (NaN to 0), and
         * invalid layers to zero thickness.
         *
         * @note When BXHEIGHT was never bound, a buffer is allocated into
         * owned_buffers (freed with the state). The Kokkos path runs an
         * MDRange kernel over LayoutLeft views and marks the result
         * device-modified; the host fallback is equivalent serial code.
         * @see catchem_met_utilities.hpp::hydrostatic_layer_thickness.
         */
        void derive_bxheight() {
            // Use a current host-provided layer thickness when available.
            // Hydrostatic reconstruction is only the fallback for hosts that
            // do not exchange BXHEIGHT.
            if (met.BXHEIGHT && met.BXHEIGHT->is_current(import_generation))
                return;
            if (!met.PEDGE || !met.T || met.PEDGE->availability != AvailabilityState::Current ||
                met.T->availability != AvailabilityState::Current)
                return;

            physical_validation_report.clear();
            met.PEDGE->sync_to_host();
            met.T->sync_to_host();
            if (met.QV)
                met.QV->sync_to_host();
            const double* physical_pedge = met.PEDGE->host_data();
            const double* physical_temperature = met.T->host_data();
            for (int icol = 0; icol < n_cols; ++icol)
                for (int ilev = 0; ilev < n_levels; ++ilev) {
                    const auto location = static_cast<std::size_t>(icol + ilev * n_cols);
                    const double lower = physical_pedge[icol + ilev * n_cols];
                    const double upper = physical_pedge[icol + (ilev + 1) * n_cols];
                    const double temperature = physical_temperature[location];
                    if (!std::isfinite(lower) || !std::isfinite(upper) || lower <= 0.0 || upper <= 0.0 ||
                        lower <= upper)
                        physical_validation_report.observe("PEDGE", "positive-descending", lower, location,
                                                           "invalid layers produce zero thickness");
                    if (!std::isfinite(temperature) || temperature <= 0.0)
                        physical_validation_report.observe("T", "finite-positive", temperature, location,
                                                           "temperature is clamped to 1 K");
                }
            if (physical_validation_policy == PhysicalValidationPolicy::Reject && !physical_validation_report.empty())
                throw std::domain_error("Physical validation failed before BXHEIGHT mutation:\n" +
                                        physical_validation_report.format());

            if (!met.BXHEIGHT) {
                auto buf = std::make_shared<std::vector<double>>(n_cols * n_levels, 0.0);
                owned_buffers.push_back(buf);
                bind_met_field_3d("BXHEIGHT", buf->data());
            }

            met.PEDGE->sync_to_device();
            met.T->sync_to_device();
            if (met.QV)
                met.QV->sync_to_device();
            int nc = n_cols;
            int nl = n_levels;

            auto pedge = met.PEDGE->view();
            auto temp = met.T->view();
            auto bxheight = met.BXHEIGHT->view();
            // Resolve QV outside the kernel: capturing `met` would capture `this`
            // (deprecated under C++20 with [=], and not device-safe). When QV is
            // absent, `qv` aliases `temp` purely to have a view of the right type;
            // it is never read in that case.
            const bool have_qv = met.QV && met.QV->availability == AvailabilityState::Current;
            auto qv = have_qv ? met.QV->view() : temp;

#ifdef CATCHEM_ENABLE_KOKKOS
            Kokkos::parallel_for(
                "derive_bxheight_kernel",
                Kokkos::MDRangePolicy<Kokkos::DefaultExecutionSpace, Kokkos::Rank<2>>({0, 0}, {nc, nl}),
                KOKKOS_LAMBDA(int icol, int ilev) {
                    double p_lower = pedge(icol, ilev, 0);
                    double p_upper = pedge(icol, ilev + 1, 0);

                    if (p_upper > 0.0 && p_lower > 0.0 && p_lower > p_upper) {
                        double q_val = have_qv ? qv(icol, ilev, 0) : 0.0;
                        if (!(q_val >= 0.0 && q_val < 1.0))
                            q_val = q_val == q_val ? (q_val < 0.0 ? 0.0 : 0.9999) : 0.0;
                        double t_val = temp(icol, ilev, 0);
                        if (!(t_val > 0.0))
                            t_val = 1.0;
                        bxheight(icol, ilev, 0) =
                            met_utilities::hydrostatic_layer_thickness(p_lower, p_upper, t_val, q_val);
                    } else {
                        bxheight(icol, ilev, 0) = 0.0;
                    }
                });
#else
            for (int icol = 0; icol < nc; ++icol) {
                for (int ilev = 0; ilev < nl; ++ilev) {
                    double p_lower = pedge(icol, ilev, 0);
                    double p_upper = pedge(icol, ilev + 1, 0);

                    if (p_upper > 0.0 && p_lower > 0.0 && p_lower > p_upper) {
                        double q_val = have_qv ? qv(icol, ilev, 0) : 0.0;
                        if (!std::isfinite(q_val))
                            q_val = 0.0;
                        q_val = std::clamp(q_val, 0.0, 0.9999);
                        double t_val = temp(icol, ilev, 0);
                        if (!std::isfinite(t_val) || t_val <= 0.0)
                            t_val = 1.0;
                        bxheight(icol, ilev, 0) =
                            met_utilities::hydrostatic_layer_thickness(p_lower, p_upper, t_val, q_val);
                    } else {
                        bxheight(icol, ilev, 0) = 0.0;
                    }
                }
            }
#endif
            met.BXHEIGHT->mark_device_modified();
            met.BXHEIGHT->set_generation(import_generation);
        }

        /**
         * @brief Derives dry air density (AIRDEN_DRY) using the Ideal Gas Law.
         *
         * Calculates dry air mass densities based on mid-point pressures, temperature, and specific humidity.
         *
         * @details Skips when a current host AIRDEN_DRY exists, returns
         * silently when PMID or T is missing or not Current, and otherwise
         * validates PMID (finite-positive), T (finite-positive, clamped to
         * 1 K), and QV (finite-[0,1), clamped to [0,0.9999]) across every
         * point before any mutation. Under PhysicalValidationPolicy::Reject,
         * findings throw std::domain_error with the formatted report.
         *
         * @note Allocates its own n_cols*n_levels buffer into owned_buffers
         * when the host did not bind AIRDEN_DRY. QV absent means dry q = 0.
         * @see catchem_met_utilities.hpp::dry_air_density.
         */
        void derive_airden_dry() {
            // A current host field is authoritative.  Derive only when the
            // host did not supply AIRDEN_DRY for this import generation.
            if (met.AIRDEN_DRY && met.AIRDEN_DRY->is_current(import_generation))
                return;
            if (!met.PMID || !met.T || met.PMID->availability != AvailabilityState::Current ||
                met.T->availability != AvailabilityState::Current)
                return;

            physical_validation_report.clear();
            met.PMID->sync_to_host();
            met.T->sync_to_host();
            if (met.QV)
                met.QV->sync_to_host();
            const double* physical_pmid = met.PMID->host_data();
            const double* physical_temperature = met.T->host_data();
            const double* physical_humidity = met.QV ? met.QV->host_data() : nullptr;
            for (int icol = 0; icol < n_cols; ++icol)
                for (int ilev = 0; ilev < n_levels; ++ilev) {
                    const auto location = static_cast<std::size_t>(icol + ilev * n_cols);
                    const double pressure = physical_pmid[location];
                    const double temperature = physical_temperature[location];
                    const double humidity = physical_humidity ? physical_humidity[location] : 0.0;
                    if (!std::isfinite(pressure) || pressure <= 0.0)
                        physical_validation_report.observe("PMID", "finite-positive", pressure, location);
                    if (!std::isfinite(temperature) || temperature <= 0.0)
                        physical_validation_report.observe("T", "finite-positive", temperature, location,
                                                           "temperature is clamped to 1 K");
                    if (!std::isfinite(humidity) || humidity < 0.0 || humidity >= 1.0)
                        physical_validation_report.observe("QV", "finite-[0,1)", humidity, location,
                                                           "humidity is clamped to [0,0.9999]");
                }
            if (physical_validation_policy == PhysicalValidationPolicy::Reject && !physical_validation_report.empty())
                throw std::domain_error("Physical validation failed before AIRDEN_DRY mutation:\n" +
                                        physical_validation_report.format());

            if (!met.AIRDEN_DRY) {
                auto buf = std::make_shared<std::vector<double>>(n_cols * n_levels, 0.0);
                owned_buffers.push_back(buf);
                bind_met_field_3d("AIRDEN_DRY", buf->data());
            }

            met.PMID->sync_to_device();
            met.T->sync_to_device();
            if (met.QV)
                met.QV->sync_to_device();
            int nc = n_cols;
            int nl = n_levels;

            auto pmid = met.PMID->view();
            auto temp = met.T->view();
            auto airden_dry = met.AIRDEN_DRY->view();
            // See derive_bxheight: resolve QV outside the kernel.
            const bool have_qv = met.QV && met.QV->availability == AvailabilityState::Current;
            auto qv = have_qv ? met.QV->view() : temp;

#ifdef CATCHEM_ENABLE_KOKKOS
            Kokkos::parallel_for(
                "derive_airden_dry_kernel",
                Kokkos::MDRangePolicy<Kokkos::DefaultExecutionSpace, Kokkos::Rank<2>>({0, 0}, {nc, nl}),
                KOKKOS_LAMBDA(int icol, int ilev) {
                    double q = have_qv ? qv(icol, ilev, 0) : 0.0;
                    if (!(q >= 0.0 && q < 1.0))
                        q = q == q ? (q < 0.0 ? 0.0 : 0.9999) : 0.0;
                    double pressure = pmid(icol, ilev, 0);
                    if (!(pressure > 0.0))
                        pressure = 1.0;
                    double t_val = temp(icol, ilev, 0);
                    if (!(t_val > 0.0))
                        t_val = 1.0;
                    airden_dry(icol, ilev, 0) = met_utilities::dry_air_density(pressure, t_val, q);
                });
#else
            for (int icol = 0; icol < nc; ++icol) {
                for (int ilev = 0; ilev < nl; ++ilev) {
                    double q = have_qv ? qv(icol, ilev, 0) : 0.0;
                    if (!std::isfinite(q))
                        q = 0.0;
                    q = std::clamp(q, 0.0, 0.9999);
                    double pressure = pmid(icol, ilev, 0);
                    if (!std::isfinite(pressure) || pressure <= 0.0)
                        pressure = 1.0;
                    double t_val = temp(icol, ilev, 0);
                    if (!std::isfinite(t_val) || t_val <= 0.0)
                        t_val = 1.0;
                    airden_dry(icol, ilev, 0) = met_utilities::dry_air_density(pressure, t_val, q);
                }
            }
#endif
            met.AIRDEN_DRY->mark_device_modified();
            met.AIRDEN_DRY->set_generation(import_generation);
        }

        // AIRDEN is the GOCART moist air density (the value GOCART2G calls
        // MAIRDEN):  P / (R_d * T * (1 + (AIRMW/H2OMW - 1) * QV)).  AIRDEN_DRY
        // is the separate humidity-corrected dry quantity derived above, so the
        // two names never alias.  MAIRDEN canonicalises to AIRDEN, so hosts and
        // schemes that bind or read MAIRDEN resolve to this same storage.
        // When the host supplies no current QV the moisture term is zero, which
        // reproduces the previous dry value rather than failing.

        /**
         * @brief Derive the GOCART moist air density AIRDEN (alias MAIRDEN).
         *
         * @details Host-provided current AIRDEN wins; otherwise computes
         * P/(R_d*T*(1 + (AIRMW/H2OMW - 1)*QV)) serially on the host from
         * current PMID and T, with QV omitted (moisture term 0) when it is
         * absent or stale. The molecular-weight ratio comes from
         * catchem::constants, not a hardcoded literal.
         *
         * @throws std::runtime_error when PMID or T is missing or not current
         *         for the active import generation.
         * @note Result is marked host-modified and stamped with the current
         * generation; a missing AIRDEN storage is allocated into
         * owned_buffers first.
         */
        void derive_airden() {
            if (met.AIRDEN && met.AIRDEN->is_current(import_generation))
                return;
            if (!met.PMID || !met.T || !met.PMID->is_current(import_generation) ||
                !met.T->is_current(import_generation))
                throw std::runtime_error("Cannot derive AIRDEN: requires current PMID and T");
            auto airden = find_field<3>("AIRDEN");
            if (!airden) {
                auto buffer = std::make_shared<std::vector<double>>(static_cast<std::size_t>(n_cols) * n_levels, 0.0);
                owned_buffers.push_back(buffer);
                bind_met_field_3d("AIRDEN", buffer->data());
                airden = find_field<3>("AIRDEN");
            }
            met.PMID->sync_to_host();
            met.T->sync_to_host();
            const bool have_qv = met.QV && met.QV->is_current(import_generation);
            if (have_qv)
                met.QV->sync_to_host();
            const double* p = met.PMID->host_data();
            const double* t = met.T->host_data();
            const double* qv = have_qv ? met.QV->host_data() : nullptr;
            // Moisture coefficient from the authoritative molecular weights,
            // not a hardcoded literal (AIRMW/H2OMW - 1).
            const double moist_factor = constants::AIR_MW / constants::H2O_MW - 1.0;
            double* output = airden->host_write();
            for (int i = 0; i < n_cols * n_levels; ++i) {
                const double w = qv ? qv[i] : 0.0;
                output[i] = p[i] / (constants::RD * t[i] * (1.0 + moist_factor * w));
            }
            airden->mark_host_modified();
            airden->set_generation(import_generation);
        }

        // Derive DELP from the host pressure interfaces.  The sign is not
        // hidden with abs(): a non-descending host vertical coordinate is an
        // invalid layer and produces zero, making the contract error visible.

        /**
         * @brief Derive layer pressure thickness DELP [Pa] from PEDGE.
         *
         * @details A current host DELP is authoritative; otherwise DELP at
         * layer k is pressure_thickness(PEDGE(k), PEDGE(k+1)) computed on the
         * host. The sign is intentionally not masked: a non-descending
         * vertical coordinate yields zero rather than a plausible-looking
         * absolute value.
         *
         * @throws std::runtime_error when PEDGE is missing or not current.
         */
        void derive_delp() {
            // An imported DELP is authoritative.  Only derive it from PEDGE
            // when the caller did not provide a current layer thickness.
            if (const auto delp = find_field<3>("DELP"); delp && delp->is_current(import_generation))
                return;
            if (!met.PEDGE || !met.PEDGE->is_current(import_generation))
                throw std::runtime_error("Cannot derive DELP: PEDGE is not current");
            auto delp = find_field<3>("DELP");
            if (!delp) {
                auto buffer = std::make_shared<std::vector<double>>(static_cast<std::size_t>(n_cols) * n_levels, 0.0);
                owned_buffers.push_back(buffer);
                bind_met_field_3d("DELP", buffer->data());
                delp = find_field<3>("DELP");
            }
            met.PEDGE->sync_to_host();
            const double* pedge = met.PEDGE->host_data();
            double* output = delp->host_write();
            for (int level = 0; level < n_levels; ++level)
                for (int column = 0; column < n_cols; ++column) {
                    const std::size_t index = static_cast<std::size_t>(column + level * n_cols);
                    output[index] = met_utilities::pressure_thickness(pedge[index], pedge[index + n_cols]);
                }
            delp->mark_host_modified();
            delp->set_generation(import_generation);
        }

        /**
         * @brief Derive the Monin-Obukhov surface length scale OBK [m].
         *
         * @details GOCART-aligned input selection: the lowest model layer
         * temperature (level index 0, bottom-to-top) replaces the surface skin
         * temperature TS, and the moist AIRDEN replaces the dry P/(R_d*T), so
         * TS is not a required input here. A current host OBK (alias OL) is
         * authoritative; otherwise the field is allocated into owned_buffers
         * and computed serially on the host via monin_obukhov_length(USTAR,
         * T(level 0), HFLUX, AIRDEN) with column-major indexing (index c is
         * the lowest layer of column c). derive_airden() runs first to
         * guarantee the moist density, preserving any host-provided AIRDEN.
         *
         * @throws std::runtime_error when USTAR, HFLUX, PMID, or T is missing
         *         or not current, or when AIRDEN remains unavailable.
         * @see catchem_met_utilities.hpp::monin_obukhov_length.
         */
        void derive_obk() {
            if (const auto obk = find_field<2>("OBK"); obk && obk->is_current(import_generation))
                return;
            // GOCART-aligned input selection: the lowest model layer temperature
            // (level index 0, bottom-to-top) replaces the surface skin
            // temperature TS, and the moist AIRDEN (contract section 2) replaces
            // the dry P/(R_d*T).  TS is therefore no longer a required input;
            // a missing profile T is reported rather than substituted with TS.
            if (!met.USTAR || !met.HFLUX || !met.PMID || !met.T || !met.USTAR->is_current(import_generation) ||
                !met.HFLUX->is_current(import_generation) || !met.PMID->is_current(import_generation) ||
                !met.T->is_current(import_generation))
                throw std::runtime_error("Cannot derive OBK: requires current USTAR, HFLUX, PMID, and T");
            auto obk = find_field<2>("OBK");
            if (!obk) {
                auto buffer = std::make_shared<std::vector<double>>(n_cols, 0.0);
                owned_buffers.push_back(buffer);
                bind_met_field_2d("OBK", buffer->data());
                obk = find_field<2>("OBK");
            }
            // Ensure the moist density is available; a host-provided AIRDEN is
            // preserved by derive_airden's own precedence guard.
            derive_airden();
            const auto airden = find_field<3>("AIRDEN");
            if (!airden)
                throw std::runtime_error("Cannot derive OBK: AIRDEN is unavailable");
            met.USTAR->sync_to_host();
            met.HFLUX->sync_to_host();
            met.T->sync_to_host();
            airden->sync_to_host();
            const double* ustar = met.USTAR->host_data();
            const double* hflux = met.HFLUX->host_data();
            const double* t = met.T->host_data();
            const double* rho = airden->host_data();
            double* output = obk->host_write();
            for (int c = 0; c < n_cols; ++c)
                // Column-major (col, level): index c is the lowest layer of column c.
                output[c] = met_utilities::monin_obukhov_length(ustar[c], t[c], hflux[c], rho[c]);
            obk->mark_host_modified();
            obk->set_generation(import_generation);
        }

        // Legacy metstate_mod defaults missing ocean salinity to zero. This
        // disables the optional salinity-dependent ocean chemistry treatment
        // while retaining a host-provided salinity field when one is available.

        /**
         * @brief Ensure a SALINITY field exists, defaulting to zero [ppt].
         *
         * @details Keeps a current host-provided salinity untouched;
         * otherwise binds a fresh all-zero per-column buffer into
         * owned_buffers, matching the legacy metstate_mod default that turns
         * off salinity-dependent ocean chemistry. Never throws.
         */
        void derive_salinity() {
            if (const auto salinity = find_field<2>("SALINITY"); salinity && salinity->is_current(import_generation))
                return;
            auto buffer = std::make_shared<std::vector<double>>(n_cols, 0.0);
            owned_buffers.push_back(buffer);
            bind_met_field_2d("SALINITY", buffer->data());
            auto salinity = find_field<2>("SALINITY");
            salinity->mark_host_modified();
            salinity->set_generation(import_generation);
        }

        // Reconstruct the legacy 20-category land-use fields from UFS's
        // dominant land-use index and leaf-area index. DLUSE=0 is water,
        // which legacy CATChem places in category 17 (one-based).  The derived
        // FRLAI/FRLANDUSE/ILAND fields are generic surface descriptors; any
        // process that declares them in its contract may consume them.

        /**
         * @brief Rebuild the legacy 20-category land-use descriptor fields.
         *
         * @details Derives, from a current DLUSE (and LAI for FRLAI), the
         * three (n_cols, 20, 1) SoilLayer-axis fields the legacy dry-deposition
         * scheme expects: FRLAI (leaf area index placed in the DLUSE category
         * slot, with categories 15-17 zeroed), FRLANDUSE (one-hot category
         * indicator with water DLUSE=0 mapped to category 17 one-based), and
         * ILAND (constant category index 1-20 in every column). Each output is
         * skipped when the host already provides a current version and is
         * allocated into owned_buffers otherwise.
         *
         * @throws std::runtime_error when DLUSE is missing or not current, or
         *         when FRLAI must be derived but LAI is missing or stale.
         */
        void derive_landuse_categories() {
            constexpr int n_landuse = 20;
            auto dluse = find_field<2>("DLUSE");
            if (!dluse || !dluse->is_current(import_generation))
                throw std::runtime_error("Cannot derive dry-deposition land use: requires current DLUSE");

            if (const auto frlai = find_field<3>("FRLAI"); !frlai || !frlai->is_current(import_generation)) {
                auto lai = find_field<2>("LAI");
                if (!lai || !lai->is_current(import_generation))
                    throw std::runtime_error("Cannot derive FRLAI: requires current LAI and DLUSE");
                auto buffer = std::make_shared<std::vector<double>>(static_cast<std::size_t>(n_cols) * n_landuse, 0.0);
                owned_buffers.push_back(buffer);
                dluse->sync_to_host();
                lai->sync_to_host();
                const double* land_type = dluse->host_data();
                const double* leaf_area = lai->host_data();
                for (int column = 0; column < n_cols; ++column) {
                    const int category = static_cast<int>(land_type[column]);
                    if (category >= 1 && category <= n_landuse)
                        buffer->at(static_cast<std::size_t>(column + (category - 1) * n_cols)) = leaf_area[column];
                    for (int slot = 14; slot <= 16; ++slot)
                        buffer->at(static_cast<std::size_t>(column + slot * n_cols)) = 0.0;
                }
                bind_met_field_3d_contract("FRLAI", buffer->data(), n_landuse, SemanticAxis::SoilLayer);
                auto out = find_field<3>("FRLAI");
                out->mark_host_modified();
                out->set_generation(import_generation);
            }

            if (const auto frlanduse = find_field<3>("FRLANDUSE");
                !frlanduse || !frlanduse->is_current(import_generation)) {
                auto buffer = std::make_shared<std::vector<double>>(static_cast<std::size_t>(n_cols) * n_landuse, 0.0);
                owned_buffers.push_back(buffer);
                dluse->sync_to_host();
                const double* land_type = dluse->host_data();
                for (int column = 0; column < n_cols; ++column) {
                    int category = static_cast<int>(land_type[column]);
                    if (category == 0)
                        category = 17;
                    if (category >= 1 && category <= n_landuse)
                        buffer->at(static_cast<std::size_t>(column + (category - 1) * n_cols)) = 1.0;
                }
                bind_met_field_3d_contract("FRLANDUSE", buffer->data(), n_landuse, SemanticAxis::SoilLayer);
                auto out = find_field<3>("FRLANDUSE");
                out->mark_host_modified();
                out->set_generation(import_generation);
            }

            if (const auto iland = find_field<3>("ILAND"); !iland || !iland->is_current(import_generation)) {
                auto buffer = std::make_shared<std::vector<double>>(static_cast<std::size_t>(n_cols) * n_landuse, 0.0);
                owned_buffers.push_back(buffer);
                for (int category = 0; category < n_landuse; ++category)
                    for (int column = 0; column < n_cols; ++column)
                        buffer->at(static_cast<std::size_t>(column + category * n_cols)) = category + 1;
                bind_met_field_3d_contract("ILAND", buffer->data(), n_landuse, SemanticAxis::SoilLayer);
                auto out = find_field<3>("ILAND");
                out->mark_host_modified();
                out->set_generation(import_generation);
            }
        }

        /**
         * @brief Derive relative humidity RH [0-1] when the host did not supply it.
         *
         * @details A current host RH wins; otherwise RH is computed serially
         * on the host from T, QV, and PMID via met_utilities::relative_humidity
         * into a fresh owned buffer. No generation currency check is applied
         * to the inputs beyond their availability state.
         *
         * @throws std::runtime_error when T, QV, or PMID is unbound.
         */
        void derive_relative_humidity() {
            if (const auto rh = find_field<3>("RH"); rh && rh->is_current(import_generation))
                return;
            if (!met.T || !met.QV || !met.PMID)
                throw std::runtime_error("Cannot derive RH: requires T, QV, and PMID");
            auto buffer = std::make_shared<std::vector<double>>(static_cast<std::size_t>(n_cols) * n_levels, 0.0);
            owned_buffers.push_back(buffer);
            bind_met_field_3d("RH", buffer->data());
            met.T->sync_to_host();
            met.QV->sync_to_host();
            met.PMID->sync_to_host();
            const double* t = met.T->host_data();
            const double* qv = met.QV->host_data();
            const double* p = met.PMID->host_data();
            for (int i = 0; i < n_cols * n_levels; ++i)
                buffer->at(i) = met_utilities::relative_humidity(t[i], qv[i], p[i]);
            met.RH->mark_host_modified();
            met.RH->set_generation(import_generation);
        }

        /**
         * @brief Derive the column-total cloud fraction CLDFRC [-].
         *
         * @details Legacy parity with metstate_mod: CLDFRC is the sum of the
         * 3D CLDF over all levels for each column (`CLDFRC = SUM(CLDF, dim=3)`),
         * not the surface layer. The sum may exceed one where clouds overlap
         * several layers and upstream applies no clamp, so neither does this.
         * A current host CLDFRC is kept untouched.
         *
         * @throws std::runtime_error when no CLDF field is bound.
         */
        void derive_column_cloud_fraction() {
            if (const auto out = find_field<2>("CLDFRC"); out && out->is_current(import_generation))
                return;
            auto cldf = find_field<3>("CLDF");
            if (!cldf)
                throw std::runtime_error("Cannot derive CLDFRC: requires CLDF");
            auto buffer = std::make_shared<std::vector<double>>(n_cols, 0.0);
            owned_buffers.push_back(buffer);
            bind_met_field_2d("CLDFRC", buffer->data());
            cldf->sync_to_host();
            const double* src = cldf->host_data();
            // Legacy parity: the upstream metstate_mod derives CLDFRC as the
            // column-total cloud fraction (`CLDFRC(:,:) = SUM(CLDF, DIM=3)`),
            // not the surface layer.  The sum may exceed one where clouds
            // overlap multiple layers; upstream applies no clamp, so neither
            // do we.
            for (int c = 0; c < n_cols; ++c) {
                double total = 0.0;
                for (int lev = 0; lev < n_levels; ++lev)
                    total += src[static_cast<std::size_t>(c) + static_cast<std::size_t>(lev) * n_cols];
                buffer->at(c) = total;
            }
            auto derived = find_field<2>("CLDFRC");
            derived->mark_host_modified();
            derived->set_generation(import_generation);
        }

        /**
         * @brief Derive the mid-timestep cosine of the solar zenith angle.
         *
         * @details SUNCOSMID per column = TimeState::get_cos_sza(LAT, LON,
         * mid_timestep=true), i.e. evaluated at the middle of the current
         * step and clamped to [-1, 1]. A current host-provided SUNCOSMID is
         * authoritative and returned unchanged.
         *
         * @throws std::runtime_error when LAT or LON is unbound.
         */
        void derive_suncosmid() {
            if (const auto out = find_field<2>("SUNCOSMID"); out && out->is_current(import_generation))
                return;
            if (!met.LAT || !met.LON)
                throw std::runtime_error("Cannot derive SUNCOSMID: requires LAT and LON");
            auto buffer = std::make_shared<std::vector<double>>(n_cols, 0.0);
            owned_buffers.push_back(buffer);
            bind_met_field_2d("SUNCOSMID", buffer->data());
            met.LAT->sync_to_host();
            met.LON->sync_to_host();
            const double* lat = met.LAT->host_data();
            const double* lon = met.LON->host_data();
            for (int c = 0; c < n_cols; ++c)
                buffer->at(c) = time.get_cos_sza(lat[c], lon[c], true);
            auto derived = find_field<2>("SUNCOSMID");
            derived->mark_host_modified();
            derived->set_generation(import_generation);
        }

        // Derive large-scale/anvil precipitation re-evaporation [kg/kg/s].
        // This is a process-owned physical diagnostic, not a missing-input
        // fallback.  Its source fields are validated before any mutation.

        /**
         * @brief Derive the large-scale precipitation re-evaporation tendency.
         *
         * @details Process-owned physical diagnostic (used by WetDep), not a
         * missing-input fallback: a current host REEVAPLS is kept, otherwise
         * the tendency [kg kg-1 s-1] is computed on the host from T, QV,
         * PMID, PEDGE, and the interface-level ice/liquid fluxes PFILSAN and
         * PFLLSAN via met_utilities::large_scale_reevaporation. Matching
         * legacy metstate_mod, layer k uses the flux at interface k (the
         * n_levels+1 extent is retained for divergence consumers). All source
         * fields must be Current before any mutation.
         *
         * @throws std::runtime_error when any of T, QV, PMID, PEDGE, PFILSAN,
         *         PFLLSAN is unbound or not current for the active generation.
         */
        void derive_reevapls() {
            // Preserve a current host-provided re-evaporation tendency.
            // The diagnostic calculation below is the fallback for hosts
            // that do not exchange REEVAPLS.
            if (const auto reevapls = find_field<3>("REEVAPLS"); reevapls && reevapls->is_current(import_generation))
                return;
            auto pfilsan = find_field<3>("PFILSAN");
            auto pfllsan = find_field<3>("PFLLSAN");
            if (!met.T || !met.QV || !met.PMID || !met.PEDGE || !pfilsan || !pfllsan ||
                !met.T->is_current(import_generation) || !met.QV->is_current(import_generation) ||
                !met.PMID->is_current(import_generation) || !met.PEDGE->is_current(import_generation) ||
                !pfilsan->is_current(import_generation) || !pfllsan->is_current(import_generation))
                throw std::runtime_error(
                    "WetDep cannot derive REEVAPLS: requires current T, QV, PMID, PEDGE, PFILSAN, and PFLLSAN");

            auto reevapls = find_field<3>("REEVAPLS");
            if (!reevapls) {
                auto buffer = std::make_shared<std::vector<double>>(static_cast<std::size_t>(n_cols) * n_levels, 0.0);
                owned_buffers.push_back(buffer);
                bind_met_field_3d("REEVAPLS", buffer->data());
                reevapls = find_field<3>("REEVAPLS");
            }
            met.T->sync_to_host();
            met.QV->sync_to_host();
            met.PMID->sync_to_host();
            met.PEDGE->sync_to_host();
            pfilsan->sync_to_host();
            pfllsan->sync_to_host();
            double* output = reevapls->host_write();
            const double* temperature = met.T->host_data();
            const double* humidity = met.QV->host_data();
            const double* pmid = met.PMID->host_data();
            const double* pedge = met.PEDGE->host_data();
            const double* ice_flux = pfilsan->host_data();
            const double* liquid_flux = pfllsan->host_data();
            for (int level = 0; level < n_levels; ++level)
                for (int column = 0; column < n_cols; ++column) {
                    const std::size_t index = static_cast<std::size_t>(column + level * n_cols);
                    output[index] = met_utilities::large_scale_reevaporation(
                        temperature[index], humidity[index], pmid[index], pedge[index], pedge[index + n_cols],
                        // Match legacy metstate_mod: the layer-k re-evaporation
                        // diagnostic uses the flux at interface k.  The
                        // nlev+1 extent is retained for consumers that need
                        // the adjacent interface (for example flux divergence).
                        ice_flux[index], liquid_flux[index]);
                }
            reevapls->mark_host_modified();
            reevapls->set_generation(import_generation);
        }

        /**
         * @brief Push every registered field's host data to its device mirror.
         *
         * @details Walks fields_1d/2d/3d, the MetState maps, and the unified
         * chemistry array, calling each field's sync_to_device(). Fields whose
         * latest writer is not the host are skipped, and host-only builds
         * perform bookkeeping only. Intended for the driver's
         * host-to-device transition before scheme execution.
         */
        void sync_to_device() {
            for (auto& [k, v] : fields_1d)
                v->sync_to_device();
            for (auto& [k, v] : fields_2d)
                v->sync_to_device();
            for (auto& [k, v] : fields_3d)
                v->sync_to_device();
            for (auto& [k, v] : met.fields_2d)
                v->sync_to_device();
            for (auto& [k, v] : met.fields_3d)
                v->sync_to_device();
            if (chem.conc)
                chem.conc->sync_to_device();
        }

        /**
         * @brief Check that the bound chemistry array matches the grid/mechanism.
         *
         * @throws std::runtime_error when a bound concentration field's
         *         extents differ from (n_cols, n_levels, n_species). Absent
         *         chemistry (conc null) is not an error here.
         */
        void validate_ready_for_execution() const {
            if (chem.conc && (chem.conc->extent(0) != static_cast<std::size_t>(n_cols) ||
                              chem.conc->extent(1) != static_cast<std::size_t>(n_levels) ||
                              chem.conc->extent(2) != static_cast<std::size_t>(n_species)))
                throw std::runtime_error("Chemistry concentration contract does not match the active grid/mechanism");
        }

        /**
         * @brief Pull every registered field's device data back to the host.
         *
         * @details Mirror of sync_to_device(): walks all registries and the
         * unified chemistry array, calling each field's sync_to_host(); only
         * fields whose device mirror holds the newest values transfer. Used
         * at the device-to-host transition before diagnostics or restart writes.
         */
        void sync_to_host() {
            for (auto& [k, v] : fields_1d)
                v->sync_to_host();
            for (auto& [k, v] : fields_2d)
                v->sync_to_host();
            for (auto& [k, v] : fields_3d)
                v->sync_to_host();
            for (auto& [k, v] : met.fields_2d)
                v->sync_to_host();
            for (auto& [k, v] : met.fields_3d)
                v->sync_to_host();
            if (chem.conc)
                chem.conc->sync_to_host();
        }

        /**
         * @brief Bind a generic one-dimensional state field under an exact key.
         *
         * @param name Registry key, stored verbatim (no canonicalisation).
         * @param ptr  Host buffer of n_cols doubles owned by the caller, which
         *             must keep it alive for the field's lifetime.
         *
         * @note Rebinds swap the host pointer of the existing view; new names
         * get a bare InteropField with no contract populated and no generation
         * stamp, so contract-checked access will not resolve them.
         */
        void bind_field_1d(const std::string& name, double* ptr) {
            auto it = fields_1d.find(name);
            if (it != fields_1d.end() && it->second) {
                it->second->update_host_pointer(ptr);
            } else {
                fields_1d[name] = std::make_shared<InteropField<double, 1>>(ptr, std::vector<int>{n_cols});
            }
        }

        /**
         * @brief Bind a generic two-dimensional state field under an exact key.
         *
         * @param name Registry key, stored verbatim (no canonicalisation).
         * @param ptr  Host buffer of n_cols entries (the second extent is a
         *             singleton) owned by the caller for the field's lifetime.
         *
         * @note Same semantics as bind_field_1d(): rebinds swap the host
         * pointer, new entries carry no populated contract or generation.
         */
        void bind_field_2d(const std::string& name, double* ptr) {
            auto it = fields_2d.find(name);
            if (it != fields_2d.end() && it->second) {
                it->second->update_host_pointer(ptr);
            } else {
                fields_2d[name] = std::make_shared<InteropField<double, 2>>(ptr, std::vector<int>{n_cols, 1});
            }
        }

        /**
         * @brief Bind a generic three-dimensional state field under an exact key.
         *
         * @param name Registry key, stored verbatim (no canonicalisation).
         * @param ptr  Host buffer of n_cols * n_levels * n_species doubles in
         *             (column, level, species) order, owned by the caller for
         *             the field's lifetime.
         *
         * @note Same semantics as bind_field_1d(): rebinds swap the host
         * pointer, new entries carry no populated contract or generation.
         */
        void bind_field_3d(const std::string& name, double* ptr) {
            auto it = fields_3d.find(name);
            if (it != fields_3d.end() && it->second) {
                it->second->update_host_pointer(ptr);
            } else {
                fields_3d[name] =
                    std::make_shared<InteropField<double, 3>>(ptr, std::vector<int>{n_cols, n_levels, n_species});
            }
        }

        /**
         * @brief Writable host pointer for a 1D field by exact key.
         * @param name Registry key (no canonicalisation applied).
         * @return Host pointer (device data synced back first), or nullptr
         *         when the name is unbound.
         */
        double* get_host_pointer_1d(const std::string& name) {
            if (fields_1d.find(name) == fields_1d.end())
                return nullptr;
            return fields_1d.at(name)->host_write();
        }

        /**
         * @brief Read-only host pointer for a 1D field by exact key.
         * @param name Registry key (no canonicalisation applied).
         * @return Host pointer (synced to host, no write marking), or nullptr
         *         when the name is unbound.
         */
        const double* get_host_read_pointer_1d(const std::string& name) {
            const auto found = fields_1d.find(name);
            return found == fields_1d.end() ? nullptr : found->second->host_read();
        }

        /**
         * @brief Writable host pointer for a 2D field, generic then met registry.
         * @param name Registry key tried first in fields_2d, then in the
         *              MetState 2D map (exact match in both).
         * @return Host pointer, or nullptr when unbound in both registries.
         */
        double* get_host_pointer_2d(const std::string& name) {
            if (fields_2d.find(name) != fields_2d.end())
                return fields_2d.at(name)->host_write();
            if (met.fields_2d.find(name) != met.fields_2d.end())
                return met.fields_2d.at(name)->host_write();
            return nullptr;
        }

        /**
         * @brief Read-only host pointer for a 2D field, generic then met registry.
         * @param name Registry key tried first in fields_2d, then in the
         *              MetState 2D map.
         * @return Host pointer, or nullptr when unbound in both registries.
         */
        const double* get_host_read_pointer_2d(const std::string& name) {
            if (fields_2d.find(name) != fields_2d.end())
                return fields_2d.at(name)->host_read();
            if (met.fields_2d.find(name) != met.fields_2d.end())
                return met.fields_2d.at(name)->host_read();
            return nullptr;
        }

        /**
         * @brief Writable host pointer for a 3D field, generic then met registry.
         * @param name Registry key tried first in fields_3d, then in the
         *              MetState 3D map.
         * @return Host pointer, or nullptr when unbound in both registries.
         */
        double* get_host_pointer_3d(const std::string& name) {
            if (fields_3d.find(name) != fields_3d.end())
                return fields_3d.at(name)->host_write();
            if (met.fields_3d.find(name) != met.fields_3d.end())
                return met.fields_3d.at(name)->host_write();
            return nullptr;
        }

        /**
         * @brief Read-only host pointer for a 3D field, generic then met registry.
         * @param name Registry key tried first in fields_3d, then in the
         *              MetState 3D map.
         * @return Host pointer, or nullptr when unbound in both registries.
         */
        const double* get_host_read_pointer_3d(const std::string& name) {
            if (fields_3d.find(name) != fields_3d.end())
                return fields_3d.at(name)->host_read();
            if (met.fields_3d.find(name) != met.fields_3d.end())
                return met.fields_3d.at(name)->host_read();
            return nullptr;
        }

        /**
         * @brief Cumulative host/device transfer counters over all fields.
         *
         * @return (transfers, bytes) summed over every registered field and
         *         the unified chemistry array, where bytes counts each sync of
         *         a field as its full immutable size times sizeof(double).
         *
         * @note Counters are monotonic since field construction and never
         * reset; intended for run-summary logging, not per-step accounting.
         */
        std::pair<std::size_t, std::size_t> transfer_statistics() const {
            std::size_t transfers = 0;
            std::size_t bytes = 0;
            const auto collect = [&transfers, &bytes](const auto& fields) {
                for (const auto& [name, field] : fields) {
                    (void)name;
                    const auto count = field->host_to_device_sync_count + field->device_to_host_sync_count;
                    std::size_t elements = 1;
                    for (const auto extent : field->immutable_extents)
                        elements *= extent;
                    transfers += count;
                    bytes += count * elements * sizeof(double);
                }
            };
            collect(fields_1d);
            collect(fields_2d);
            collect(fields_3d);
            collect(met.fields_2d);
            collect(met.fields_3d);
            if (chem.conc) {
                const auto count = chem.conc->host_to_device_sync_count + chem.conc->device_to_host_sync_count;
                std::size_t elements = 1;
                for (const auto extent : chem.conc->immutable_extents)
                    elements *= extent;
                transfers += count;
                bytes += count * elements * sizeof(double);
            }
            return {transfers, bytes};
        }

    private:
        int n_cols;    ///< Flattened grid columns fixed at construction.
        int n_levels;  ///< Vertical model layers fixed at construction.
        int n_species; ///< Mechanism species count fixed at construction.
        std::size_t import_generation =
            0; ///< Current host-import generation counter (0 before the first begin_import_generation()).
        PhysicalValidationPolicy physical_validation_policy =
            PhysicalValidationPolicy::WarnAndClamp;          ///< Policy for the derive_* physical checks.
        PhysicalValidationReport physical_validation_report; ///< Findings from the most recent validation scan.

        std::string config_file_path;                ///< Configuration path recorded by the driver.
        std::string trace_id;                        ///< 8-character log trace tag.
        std::shared_ptr<ConfigManager> config_mgr;   ///< Attached configuration manager (shared, may be null).
        std::shared_ptr<DiagnosticManager> diag_mgr; ///< Attached diagnostic manager (shared, may be null).

        std::unordered_map<std::string, std::shared_ptr<InteropField<double, 1>>>
            fields_1d; ///< Generic 1D registry keyed by exact name.
        std::unordered_map<std::string, std::shared_ptr<InteropField<double, 2>>>
            fields_2d; ///< Generic 2D registry keyed by exact name.
        std::unordered_map<std::string, std::shared_ptr<InteropField<double, 3>>>
            fields_3d; ///< Generic 3D registry keyed by exact name.
        std::vector<std::shared_ptr<std::vector<double>>>
            owned_buffers; ///< Buffers allocated by the derive_* methods; freed with the state.

        MetState met;   ///< Meteorology field registry and convenience members.
        ChemState chem; ///< Chemistry/mechanism state.
        TimeState time; ///< Simulation clock.
    };

} // namespace catchem

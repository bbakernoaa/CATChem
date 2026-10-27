/**
 * @file catchem_config_manager.hpp
 * @brief YAML configuration parsing, typed accessors, and validation.
 *
 * @details ConfigManager loads the top-level CATChem YAML (plus separately
 * the species mechanism file and the emission mapping file) into the typed
 * ConfigData aggregate, and answers path-based queries such as
 * "simulation/timestep" or "processes/dust/alpha". Paths under
 * "processes/<name>/" are resolved against the parsed ProcessConfig blocks
 * first and fall back to the raw YAML tree, so scheme options stay reachable
 * through one interface. Relative dependency paths (species file, emission
 * file, per-process source_file) are resolved against the directory of the
 * configuration that declares them. validate() collects ValidationIssue
 * records rather than throwing; validate_or_throw() turns any error-severity
 * issue into std::invalid_argument. The parsed tree is host-only state: no
 * Kokkos views, and mutation during concurrent reads is not supported.
 */
#pragma once
#include "catchem_emission_manager.hpp"
#include "catchem_physical_validation.hpp"
#include <map>
#include <string>
#include <string_view>
#include <vector>
#include <yaml-cpp/yaml.h>

namespace catchem {

    // Forward declaration: ProcessConfig carries a back-pointer to the
    // resolved configuration, which is defined further below.
    struct ConfigData;

    /**
     * @brief Resolve a dependency path against the YAML file that declares it.
     *
     * @param config_file Path of the declaring configuration file; its directory
     *                    is the base for relative paths. May be empty.
     * @param path        Dependency path as written in the file. Absolute
     *                    paths (leading '/') and empty strings are returned
     *                    unchanged.
     * @return The joined path, or path unchanged when config_file has no
     *         directory separator.
     */
    inline std::string resolve_config_path(const std::string& config_file, const std::string& path) {
        if (path.empty() || path.front() == '/')
            return path;
        const auto slash = config_file.find_last_of("/\\");
        return slash == std::string::npos ? path : config_file.substr(0, slash + 1) + path;
    }

    /// Severity of a validation finding; Error blocks a strict run.
    enum class ValidationSeverity {
        Warning, ///< Advisory finding; does not fail validate_or_throw().
        Error    ///< Blocking finding; validate_or_throw() raises std::invalid_argument.
    };

    /// One configuration validation finding, with an actionable correction.
    struct ValidationIssue {
        ValidationSeverity severity = ValidationSeverity::Error; ///< Warning or Error.
        std::string category; ///< Finding family: schema, range, diagnostics, mechanism, process, emissions.
        std::string path;     ///< YAML path the finding refers to, e.g. "simulation/nx".
        std::string message;  ///< Human-readable problem statement.
        std::string suggested_correction; ///< Concrete edit that resolves the finding.
    };

    /**
     * @brief Accumulated configuration validation findings.
     *
     * @details Owned by ConfigManager and cleared on every validate() call.
     * Value data; safe to copy for reporting.
     */
    struct ValidationReport {
        std::vector<ValidationIssue> issues; ///< Findings in detection order.

        /// True when at least one issue carries ValidationSeverity::Error.
        bool has_errors() const;

        /// Render every issue as one "error|warning [category] path: message (correction)" line.
        std::string format() const;
    };

    /// Basic simulation file and verbosity settings from YAML.
    struct SimulationConfig {
        std::string name;              ///< Run label from simulation/name.
        std::string start_date;        ///< Run start timestamp as written (parsed by the driver, not here).
        std::string end_date;          ///< Run end timestamp as written.
        std::string species_filename;  ///< simulation/species_filename as written; resolve against config dir.
        std::string emission_filename; ///< simulation/emission_filename as written; resolve against config dir.
        bool verbose_enabled = false;  ///< simulation/verbose/activate switch.
        std::string log_level;         ///< simulation/verbose/log_level, as written in the YAML (empty when unset)
        std::vector<std::string> watch_species; ///< Species names selected for verbose per-process bounds logging.
        bool watch_all_species = false;         ///< True when simulation/verbose/watch_species is "all".
    };

    /// Grid dimensions and integration controls consumed by the drivers.
    struct RuntimeConfig {
        int nx = 1;         ///< simulation/nx: grid cells along x; validated > 0.
        int ny = 1;         ///< simulation/ny: grid cells along y; validated > 0.
        int nz = 1;         ///< Vertical levels; overwritten by grid/number_of_levels when present.
        double dt = 3600.0; ///< simulation/timestep (or legacy dt) [s]; validated 0 < dt <= 86400.
        int nsteps = 1;     ///< simulation/nsteps: number of transport steps; validated > 0.
    };

    /// @brief Grid dimensions and static grid settings from YAML.
    struct GridConfig {
        int number_of_levels = 1;      ///< grid/number_of_levels: model layers, synced into runtime.nz.
        int number_of_soil_layers = 0; ///< grid/number_of_soil_layers; validated >= 0.
    };

    /// @brief Runtime timestep settings from YAML.
    struct TimestepConfig {
        int transport_timestep_in_s = 0; ///< timesteps/transport_timestep_in_s [s]; validated >= 0.
        int chemistry_timestep_in_s = 0; ///< timesteps/chemistry_timestep_in_s [s]; validated >= 0.
    };

    /// Diagnostic output settings from YAML.
    struct DiagnosticOutputConfig {
        bool enabled = false;               ///< diagnostics/output/enabled switch.
        std::string directory;              ///< Output directory for diagnostic files.
        std::string prefix;                 ///< File name prefix for diagnostic files.
        int frequency = 0;                  ///< Write interval in model steps; validated > 0 when enabled.
        std::string format;                 ///< Output format; only "netcdf" passes validation (empty = default).
        int compress_lev = 0;               ///< NetCDF deflate level, validated 0-9 when enabled.
        bool process_diagnostics = false;   ///< Also collect per-process diagnostics.
        std::vector<std::string> diag_list; ///< Explicit variable list to write (empty = manager default set).
        /// Run-level NetCDF global attributes (institution, references, ...).
        /// Written to every diagnostic file; user entries override core defaults
        /// on key collision (spec FR-011).
        std::map<std::string, std::string> attributes;
    };

    /// In-memory diagnostic collection buffer settings from YAML.
    struct DiagnosticCollectionConfig {
        bool enabled = false; ///< diagnostics/collection/enabled switch.
        int buffer_size = 0;  ///< Collected samples held before flush; validated > 0 when enabled.
    };

    /// @brief Top-level diagnostic settings from YAML.
    struct DiagnosticsConfig {
        DiagnosticOutputConfig output;         ///< File-writing settings.
        DiagnosticCollectionConfig collection; ///< In-memory collection settings.
    };

    /// @brief Process activation and nested process settings from YAML.
    struct ProcessConfig {
    private:
        YAML::Node settings_node; ///< Clone of the process block, kept for typed option queries.
        friend class ConfigManager;

    public:
        bool activate = false;                 ///< processes/<name>/activate switch.
        bool diagnostics = false;              ///< processes/<name>/diagnostics switch.
        std::string scheme;                    ///< Selected scheme name for the process.
        std::vector<std::string> diag_species; ///< Species to diagnose for this process.

        /// Back-pointer to the resolved configuration this block belongs to,
        /// set by ConfigManager at the end of load_from_file(). A registered
        /// settings validator receives it through the ProcessConfig it is
        /// handed, so a process-level check can inspect the whole run (for
        /// example, a process-order conflict against the active list) without
        /// reaching for the global singleton. Null for a ProcessConfig built
        /// directly (unit tests, programmatic settings), where no run context
        /// is available.
        const ConfigData* run_context = nullptr;

        /// Attach the cloned YAML node holding this process's options (set during parsing).
        void set_settings_node(const YAML::Node& node) { settings_node = node; }

        /**
         * @brief Read a boolean option from the process block.
         * @param key         Slash-separated path inside the block, e.g. "gocart/alpha".
         * @param default_val Value returned when the path is absent, null, or
         *                    not convertible; "true/1/yes/y/on" and
         *                    "false/0/no/n/off" (case-insensitive) are
         *                    accepted as text scalars.
         * @return The parsed flag, or default_val.
         */
        bool get_bool(std::string_view key, bool default_val = false) const;

        /**
         * @brief Read a numeric option from the process block.
         * @param key         Slash-separated path inside the block.
         * @param default_val Value returned when the path is absent or not
         *                    convertible (std::stod is tried on text scalars).
         * @return The parsed value, or default_val.
         */
        double get_double(std::string_view key, double default_val = 0.0) const;

        /**
         * @brief Read an integer option from the process block.
         * @param key         Slash-separated path inside the block.
         * @param default_val Value returned when the path is absent or not
         *                    convertible (std::stoi is tried on text scalars).
         * @return The parsed value, or default_val.
         */
        int get_int(std::string_view key, int default_val = 0) const;

        /**
         * @brief Read a string option from the process block.
         * @param key         Slash-separated path inside the block.
         * @param default_val Value returned when the path is absent or null.
         * @return The scalar text; the YAML null spellings "null", "NULL",
         *         and "~" are treated as unset and yield default_val.
         */
        std::string get_string(std::string_view key, std::string_view default_val = "") const;

        /**
         * @brief Read a numeric sequence option from the process block.
         * @param key Slash-separated path inside the block.
         * @return The sequence values; empty when the path is absent or not a
         *         sequence.
         * @throws std::invalid_argument when an element is not numeric (a
         *         non-numeric list is a configuration error, not a default).
         */
        std::vector<double> get_vector(std::string_view key) const;

        /// @brief List every nested option path as "<scheme>.<key>".
        ///
        /// Framework keys (activate, diagnostics, scheme, gas_scheme,
        /// aero_scheme, diag_species) are excluded because the process layer
        /// consumes them directly.  Process registration supplies a
        /// validator that checks these paths against the scheme's accepted
        /// options so a typo or removed parameter fails at initialization
        /// instead of silently keeping its compiled default.
        std::vector<std::string> option_paths() const;
    };

    /// @brief Species metadata loaded from a CATChem species YAML file.
    struct SpeciesConfig {
        std::string name;                 ///< Canonical short name (key for the mechanism).
        std::string long_name;            ///< Descriptive name; falls back to name when absent.
        std::string description;          ///< Free-text description.
        std::vector<std::string> aliases; ///< Alternate accepted names.
        std::vector<std::string> roles;   ///< Functional role labels for role-based lookup.
        std::string ccpp_standard_name;   ///< CF/CCPP standard name; falls back to name when absent.
        std::string units;                ///< Concentration units as configured.
        std::string amount_type; ///< "mole_fraction", "mass_mixing_ratio", "number_concentration", or "ppmv" alias.
        std::string air_basis;   ///< "dry", "moist", or "total_with_condensate".

        bool is_gas = false;     ///< Gas-phase species.
        bool is_aerosol = false; ///< Aerosol-phase species.
        bool is_tracer = false;  ///< Passive tracer flag.
        bool is_advected = true; ///< Participates in transport.
        bool is_drydep = false;  ///< Dry-deposition participant (also set from do_drydep).
        bool is_wetdep = false;  ///< Wet-deposition participant (also set from do_wetdep).
        // Optional per-process overrides.  Defaults are derived from the
        // phase/deposition metadata above so existing mechanisms are unchanged.
        bool do_settling = true;     ///< Gravitational settling applies (loader defaults it to is_aerosol).
        bool is_photolysis = false;  ///< Photolysis participant.
        bool is_gocart_aero = false; ///< Handled by GOCART aerosol microphysics.
        bool is_dust = false;        ///< Mineral-dust mode species.
        bool is_seasalt = false;     ///< Sea-salt mode species.
        bool is_hydrophilic = true;  ///< Water uptake at elevated RH (defaults to the soluble behaviour).

        double molecular_weight_kg_mol =
            0.0;                   ///< Molecular weight [kg mol-1]; derived from mw_g/1000 when only mw_g given.
        double mw_g = 0.0;         ///< Molecular weight [g mol-1]; recomputed from the kg/mol value.
        double density = 0.0;      ///< Particle bulk density [kg m-3].
        double radius = 0.0;       ///< Representative mode radius [m].
        double lower_radius = 0.0; ///< Lower bin-edge radius [m].
        double upper_radius = 0.0; ///< Upper bin-edge radius [m].
        double viscosity = 0.0;    ///< Dynamic viscosity parameter [Pa s].

        // Dry deposition parameters
        double dd_f0 = 0.0;             ///< Emerson f0 resistance parameter [-].
        double dd_hstar = 0.0;          ///< Canopy compensation height [m].
        double dd_DvzAerSnow = 0.0;     ///< Deposition velocity over snow [m s-1].
        double dd_DvzMinVal_snow = 0.0; ///< Minimum deposition velocity over snow [m s-1].
        double dd_DvzMinVal_land = 0.0; ///< Minimum deposition velocity over land [m s-1].

        // Wet deposition parameters
        double henry_k0 = 0.0;                               ///< Henry's law constant at 298 K [M atm-1].
        double henry_cr = 0.0;                               ///< Henry temperature-dependence coefficient [K].
        double henry_pKa = 0.0;                              ///< Acid dissociation exponent [-].
        double wd_retfactor = 0.0;                           ///< Convective retention factor [-].
        bool wd_LiqAndGas = false;                           ///< Species exists in liquid and gas cloud phases.
        double wd_convfacI2G = 0.0;                          ///< Convective ice-to-gas conversion factor [-].
        std::vector<double> wd_rainouteff = {0.0, 0.0, 0.0}; ///< Rainout efficiency per precip category [-].
        double wd_reevap_frac = 0.5;                         ///< Re-applied fraction of evaporated precipitation [-].

        // Chemical loss rate and background volume-mixing ratio
        double t_chem_loss = -1.0;     ///< First-order chemical loss [s-1]; negative disables it.
        double BackgroundVV = 1.0e-20; ///< Background volume mixing ratio floor [mol mol-1].
        std::string mie_name;          ///< Aerosol optics type code selecting a Mie table (e.g. "DU").
    };

    /// @brief Mapping for one external emission source field.
    struct EmissionFieldMapping {
        std::string long_name;        ///< Descriptive name of the source field.
        std::string units;            ///< Units of the source field as configured.
        std::vector<double> scale;    ///< Scaling factors, one per mapped target species.
        std::vector<std::string> map; ///< Target species (or MET_ inputs) receiving this field; must match scale size.
    };

    /// @brief Emission mapping category from a CATChem emission YAML file.
    struct EmissionCategoryMapping {
        std::map<std::string, EmissionFieldMapping> fields; ///< Source fields keyed by field name.
    };

    /// @brief Aerosol optics (Mie) table inputs from the top-level "mie:" section.
    ///
    /// Mirrors the legacy upstream/develop configuration: "mie.directory" holds the
    /// table directory and "mie.files" maps an aerosol type code (SS, DU, BC, ...) to
    /// the NetCDF optics file supplying it.  Species bind to a type through their
    /// "__mie_name" attribute.  "files" keeps the YAML declaration order so every rank
    /// loads the same tables in the same sequence (determinism).
    struct MieConfig {
        std::string directory = "./";                           ///< Directory holding the optics tables.
        std::vector<std::pair<std::string, std::string>> files; ///< (type code, file name) pairs in declaration order.
    };

    /**
     * @brief Typed aggregate of everything parsed from the configuration.
     *
     * @details Populated by ConfigManager::load_from_file() and the two
     * load_*_file() methods; consumers read it directly after loading. The
     * emission_categories mirror shares its struct with the emission manager
     * (EmissionCategory) so validation and the Fortran driver see one source
     * of truth.
     */
    struct ConfigData {
        SimulationConfig simulation;               ///< Run identity, dates, and verbosity settings.
        RuntimeConfig runtime;                     ///< Grid sizes, timestep [s], and step count.
        GridConfig grid;                           ///< Level and soil-layer counts.
        TimestepConfig timesteps;                  ///< Transport/chemistry sub-timestep overrides [s].
        DiagnosticsConfig diagnostics;             ///< Output and collection settings.
        std::string species_filename;              ///< simulation:species_filename, as written in the YAML
        std::vector<std::string> active_processes; ///< Process names listed under run_phases, in declaration order.
        std::map<std::string, ProcessConfig> processes; ///< Parsed process blocks keyed by process name.
        std::vector<SpeciesConfig> species; ///< Mechanism species in file order (after load_species_file()).
        std::string mechanism_identity;     ///< mechanism/identity label, empty when unset.
        std::vector<std::string> mechanism_capabilities; ///< mechanism/capabilities tags.
        PhysicalValidationPolicy physical_validation_policy =
            PhysicalValidationPolicy::Reject; ///< physical_validation/policy; unknown strings throw at parse.
        std::map<std::string, EmissionCategoryMapping>
            emission_mappings; ///< Emission mapping file contents keyed by category.
        /// Runtime emission-category mirror parsed from processes/extemis/*.
        /// Holds the MAPL ExtData temporal options (daily_hold, monthly_anchor)
        /// so configuration validation and option-propagation tests can inspect
        /// them; the interpolation behaviour lives in the Fortran NUOPC driver.
        std::map<std::string, EmissionCategory> emission_categories;
        MieConfig mie; ///< Aerosol optics table configuration.
    };

    /**
     * @brief Loads, stores, queries, and validates the CATChem YAML configuration.
     *
     * @details Owns the parsed YAML tree (root_node) and the typed ConfigData
     * projection of it. The typed accessors below are the supported read path;
     * data is public so drivers can consume it wholesale. Loading replaces
     * previous state (processes, categories, and the requested sections are
     * cleared first). Not thread-safe: load and validate during
     * initialization, read afterwards.
     */
    class ConfigManager {
    private:
        YAML::Node root_node; ///< Retained parsed tree backing the path queries.

    public:
        // When false, load_from_file() suppresses the stdout echo of the parsed
        // YAML.  The NUOPC driver disables it on non-root PETs so the effective
        // configuration is printed once per run instead of once per PET.
        // Defaults to true: standalone and test runs keep the historical
        // behavior of printing exactly what the core parsed.
        static bool echo_config_to_stdout; ///< Process-wide stdout echo switch (mutable global state).

        ConfigData data;                    ///< Typed projection of the parsed configuration.
        bool is_loaded = false;             ///< True after a successful load_from_file(); reset when a load throws.
        std::string config_file_path;       ///< Path of the last loaded top-level configuration file.
        ValidationReport validation_report; ///< Result of the most recent validate() call.

        ConfigManager() = default;

        /**
         * @brief Parse the top-level configuration file into data.
         * @param filename Path to the YAML configuration; must exist and be
         *                 readable. Relative dependency values inside it are
         *                 resolved against this file's directory by the
         *                 getters, not here.
         *
         * @note Echoes the parsed YAML to stdout unless echo_config_to_stdout
         * is false. Throws yaml-cpp exceptions, std::invalid_argument for a
         * bad physical_validation/policy or log_level or malformed mie.files,
         * and leaves is_loaded false when any throw happens (the error is also
         * printed to std::cerr before rethrow).
         */
        void load_from_file(const std::string& filename);

        /**
         * @brief Parse a species mechanism file into data.species.
         * @param filename Path to a YAML sequence of species entries; each
         *                 attribute may also appear with a "__" prefix, which
         *                 takes precedence over the bare key.
         *
         * @note Replaces the whole species list. A non-sequence or undefined
         * root yields an empty list without error; malformed numeric values
         * throw through yaml-cpp.
         */
        void load_species_file(const std::string& filename);

        /**
         * @brief Parse an emission mapping file into data.emission_mappings.
         * @param filename Path to a YAML mapping of category -> field ->
         *                 {long_name, units, scale, map}; a non-map root
         *                 leaves the (cleared) mappings empty.
         */
        void load_emission_mapping_file(const std::string& filename);

        /**
         * @brief Run all configuration checks and store the findings.
         * @param strict When true, also report unknown top-level keys as
         *               schema errors; compatibility mode (false) tolerates them.
         * @return Reference to validation_report, cleared and rebuilt on every
         *         call (ranges, diagnostics, mechanism uniqueness, known
         *         process names, emission map/scale consistency and species
         *         membership, monthly_anchor values).
         *
         * @note Does not throw; callers decide via has_errors() whether a
         * warning-only report is acceptable.
         */
        const ValidationReport& validate(bool strict = true);

        /**
         * @brief validate() followed by a hard failure on errors.
         * @param strict Forwarded to validate().
         * @throws std::invalid_argument with the formatted report when any
         *         issue has ValidationSeverity::Error. Warnings never throw.
         */
        void validate_or_throw(bool strict = true);

        /**
         * @brief Read a boolean by slash-separated YAML path.
         * @param path        Tree path, e.g. "simulation/verbose/activate" or
         *                    "processes/dust/activate" (process paths consult
         *                    the parsed ProcessConfig blocks first).
         * @param default_val Returned when the path is missing or unconvertible;
         *                    text scalars true/1/yes/y/on and false/0/no/n/off
         *                    (case-insensitive) are accepted.
         * @return The parsed flag, or default_val.
         */
        bool get_bool(std::string_view path, bool default_val = false) const;

        /**
         * @brief Read a double by slash-separated YAML path.
         * @param path        Tree path; "processes/..." prefixes resolve
         *                    against the process block first, falling back to
         *                    the raw tree when the value equals default_val.
         * @param default_val Returned when the path is missing or unconvertible.
         * @return The parsed value, or default_val.
         */
        double get_double(std::string_view path, double default_val = 0.0) const;

        /**
         * @brief Read an int by slash-separated YAML path.
         * @param path        Tree path; "processes/..." prefixes resolve
         *                    against the process block first.
         * @param default_val Returned when the path is missing or unconvertible.
         * @return The parsed value, or default_val.
         */
        int get_int(std::string_view path, int default_val = 0) const;

        /**
         * @brief Read a string by slash-separated YAML path.
         * @param path        Tree path; "processes/..." prefixes resolve
         *                    against the process block first ("scheme" is
         *                    served from the parsed block).
         * @param default_val Returned when the path is missing or null.
         * @return The scalar text. Paths containing "source_file" or
         *         "filename" are resolved relative to config_file_path; the
         *         null spellings "null", "NULL", "Null", "~" yield
         *         default_val.
         */
        std::string get_string(std::string_view path, std::string_view default_val = "") const;

        /**
         * @brief Read a sequence of scalars by slash-separated YAML path.
         * @param path Tree path; "processes/..." prefixes resolve against the
         *             process block first.
         * @return The sequence entries as strings; empty when the path is
         *         missing or not a sequence.
         */
        std::vector<std::string> get_string_list(std::string_view path) const;

        /**
         * @brief Report whether a process is switched on.
         * @param process_name Process key, e.g. "drydep".
         * @return true when processes/<name>/activate, the legacy
         *         process/<name>/activate spelling, or the parsed block's
         *         activate flag is set; false otherwise.
         */
        bool is_process_active(std::string_view process_name) const;

        /**
         * @brief Report whether an external-emission category is usable.
         * @param category_name Emission sector key, e.g. "anthro".
         * @return false when the emission mapping file has no entry for the
         *         category; otherwise the configured activate flag, defaulting
         *         to true when the category block omits it.
         */
        bool is_category_active(std::string_view category_name) const;

        /**
         * @brief Find the data file a process declares, if any.
         * @param process_name Process key to probe.
         * @return The first source_file/input_file/filename/file/path value
         *         found in the process block, under the "dust" block, or under
         *         the "extemis" block (then its "dust" child) — resolved
         *         relative to config_file_path; empty when none exists.
         *
         * @note Boolean and placeholder scalars ("none", "null", "~", "true",
         * "false", "0", "1") are never treated as file settings.
         */
        std::string find_process_file_setting(std::string_view process_name) const;
    };

} // namespace catchem

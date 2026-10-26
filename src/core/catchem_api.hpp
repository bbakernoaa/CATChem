/**
 * @file catchem_api.hpp
 * @brief Flat, BIND(C) linkable API endpoints for CATChem host model integration.
 *
 * @details This header is CATChem's stable C ABI: every symbol is
 * `extern "C"` and safe to call from Fortran (via `bind(C)` interfaces) or C.
 * It comes in two generations:
 * - `*_checked` entry points return a catchem_dataflow_status code, never
 *   throw across the boundary, and record a thread-local detail on failure
 *   that catchem_get_last_error() renders into a caller buffer. New code
 *   should use only these.
 * - The legacy void/pointer-returning functions predate the status protocol
 *   and are kept for existing drivers; each has a checked sibling where one
 *   exists.
 *
 * Handles are opaque `void*` values (Core, manifest, StateManager) owned by
 * CATChem; the create/destroy pair named in each function's docs defines the
 * lifetime, and using a handle after its destroy returns
 * CATCHEM_INVALID_HANDLE rather than crashing.
 *
 * String outputs follow a caller-owned-buffer protocol: the caller supplies
 * `buffer` + `buffer_len` and an out `required_len` receiving the length
 * including the NUL. If the buffer is shorter than required, the call fails
 * with CATCHEM_EXTENT_MISMATCH and copies nothing. Passing buffer == NULL
 * with buffer_len == 0 (where supported) is a pure size probe: the call
 * succeeds and only fills required_len, so hosts can query-then-allocate.
 *
 * Index conventions differ by family and are stated per function:
 * manifest indices are 0-based; StateManager species indices are 1-based to
 * match the Fortran species lists.
 *
 * @see catchem_error.hpp (BoundaryStatus, the C++ mirror of the enum below),
 * catchem_core.hpp, catchem_state_manager.hpp.
 */

#pragma once

#include "catchem_precision.hpp"
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Status codes returned by every checked CATChem C entry point.
 *
 * @details The integer values mirror catchem::BoundaryStatus in
 * catchem_error.hpp exactly; hosts may compare against either spelling.
 * Codes 0-9 are shared with the older DataflowStatus used by field
 * contracts (catchem_field_contract.hpp); 10 onward are specific to the
 * C boundary.
 */
enum catchem_dataflow_status {
    CATCHEM_SUCCESS = 0,                             ///< Operation completed; outputs are valid.
    CATCHEM_NULL_ARGUMENT = 1,                       ///< A required pointer (handle, buffer, name, descriptor, output) was null.
    CATCHEM_MISSING_FIELD = 2,                       ///< The named state or diagnostic field is not registered.
    CATCHEM_RANK_MISMATCH = 3,                       ///< A diagnostic register call used a rank other than 2 or 3.
    CATCHEM_EXTENT_MISMATCH = 4,                     ///< Declared extents disagree with the state, or a caller buffer is shorter than the required length.
    CATCHEM_INVALID_INDEX = 5,                       ///< A species, process, field, alias or slot index is out of range for its container.
    CATCHEM_STALE_GENERATION = 6,                    ///< A registered handle's generation differs from the requested one.
    CATCHEM_DUPLICATE_MAPPING = 7,                   ///< Reserved: no code path currently returns it.
    CATCHEM_INVALID_STATE = 8,                       ///< Prerequisite state absent or invalid (no active mechanism, bad axis ordinal, rejected config-driven create), or a rejected std::invalid_argument at the boundary.
    CATCHEM_INTERNAL_ERROR = 9,                      ///< An unexpected exception escaped the operation, or handle registration failed.
    CATCHEM_INVALID_HANDLE = 10,                     ///< Handle is unregistered (destroyed or never created), owned elsewhere, or its parent is gone.
    CATCHEM_WRONG_HANDLE_TYPE = 11,                  ///< Handle is valid but registered under a different type than the call expects.
    CATCHEM_RUNTIME_UNAVAILABLE = 12,                ///< Reserved: no code path currently returns it.
    CATCHEM_CONTRACT_VIOLATION = 13,                 ///< A versioned descriptor has an unsupported schema_version/struct_size, an interval or invocation is malformed, or a manifest/Core cross-validation fingerprint mismatched.
    CATCHEM_INVALID_CONFIGURATION = 14,              ///< A configuration or mechanism file failed to parse or validate.
    CATCHEM_PROCESS_FAILURE = 15,                    ///< A selected process threw during catchem_core_run_process_checked (see the operation_status out-param).
    CATCHEM_SHUTDOWN_FAILURE = 16,                   ///< A process finalize() threw during core destroy; remaining finalizers still ran and the Core was released.
    CATCHEM_PHYSICAL_VALIDATION_FAILURE = 17         ///< Derived meteorology failed physical domain checks (e.g. negative pressure in a log/exp kernel).
};

/**
 * @brief Versioned species metadata returned by manifest discovery.
 *
 * @details Forward-compatibility convention shared by every *_v1 descriptor:
 * the caller sets schema_version = 1 and struct_size = sizeof(struct) before
 * the call; a checked entry point rejects any other combination with
 * CATCHEM_CONTRACT_VIOLATION. The *_required_len fields report the buffer
 * size (including NUL) the corresponding strings need; see the
 * caller-owned-buffer protocol documented at the top of this file.
 */
typedef struct catchem_manifest_species_v1 {
    uint32_t schema_version;                 ///< Must be 1 on input.
    uint32_t struct_size;                    ///< Must be sizeof(*this) on input.
    uint32_t flags;                          ///< Bit 0: gas, bit 1: aerosol, bit 2: advected.
    uint32_t amount_type;                    ///< AmountType ordinal: 0 unspecified, 1 mole fraction, 2 mass ratio, 3 number concentration (catchem_species_metadata.hpp).
    uint32_t air_basis;                      ///< AirBasis ordinal: 0 unspecified, 1 dry, 2 moist, 3 total with condensate (catchem_species_metadata.hpp).
    double molecular_weight_kg_mol;          ///< Molecular weight [kg mol-1].
    int name_required_len;                   ///< Buffer size (incl. NUL) needed for the short name.
    int standard_name_required_len;          ///< Buffer size (incl. NUL) needed for the CF standard name.
    int units_required_len;                  ///< Buffer size (incl. NUL) needed for the units string.
    int alias_count;                         ///< Number of aliases readable via catchem_manifest_get_species_alias_checked().
    int role_count;                          ///< Number of semantic roles readable via catchem_manifest_get_species_role_checked().
} catchem_manifest_species_v1;

/** @brief Versioned process availability and CCPP scheme metadata. */
typedef struct catchem_manifest_process_v1 {
    uint32_t schema_version;    ///< Must be 1 on input (see catchem_manifest_species_v1 for the convention).
    uint32_t struct_size;       ///< Must be sizeof(*this) on input.
    int available;              ///< Non-zero when the process is compiled into this build and registrable.
    int name_required_len;      ///< Buffer size (incl. NUL) needed for the process name.
    int scheme_required_len;    ///< Buffer size (incl. NUL) needed for the active CCPP scheme name.
} catchem_manifest_process_v1;

/** @brief Host-supplied identity and duration for one selected process call. */
typedef struct catchem_process_invocation_v1 {
    uint32_t schema_version;    ///< Must be 1 on input (see catchem_manifest_species_v1 for the convention).
    uint32_t struct_size;       ///< Must be sizeof(*this) on input.
    uint64_t interval_id;       ///< Must equal the id of the currently open host interval.
    const char* site;           ///< Host grid-tile/column identifier; non-empty, not retained after the call.
    uint32_t subcycle;          ///< Host sub-cycle ordinal within the interval.
    const char* process;        ///< Process name as reported by manifest discovery; not retained after the call.
    double duration_seconds;    ///< Integration duration for this invocation [s]; must be positive.
} catchem_process_invocation_v1;

/** @brief Host-authoritative clock and duration for a process interval. */
typedef struct catchem_host_interval_v1 {
    uint32_t schema_version;    ///< Must be 1 on input (see catchem_manifest_species_v1 for the convention).
    uint32_t struct_size;       ///< Must be sizeof(*this) on input.
    uint64_t interval_id;       ///< Caller-chosen unique id pairing begin/end_interval and every invocation.
    int year, month, day, hour, minute, second, day_of_year; ///< UTC wall-clock components at the interval start.
    double duration_seconds;    ///< Interval duration [s]; must be positive.
} catchem_host_interval_v1;

/** @brief Summary of a process's field, diagnostic, product, and recovery contract. */
typedef struct catchem_process_contract_v1 {
    uint32_t schema_version;            ///< Must be 1 on input (see catchem_manifest_species_v1 for the convention).
    uint32_t struct_size;               ///< Must be sizeof(*this) on input.
    uint32_t recovery_capability;       ///< RecoveryCapability ordinal: 0 reconstructible, 1 requires reimport, 2 requires reinitialize (catchem_process_interface.hpp).
    int process_name_required_len;      ///< Buffer size (incl. NUL) needed for the process name.
    int field_count;                    ///< Number of field entries readable via catchem_core_get_process_field_checked().
    int diagnostic_count;               ///< Number of diagnostics the process registers.
    int product_requirement_count;      ///< Number of products the process consumes from other processes.
    int produced_product_count;         ///< Number of products the process publishes.
} catchem_process_contract_v1;

/** @brief Rank, axes, access, persistence, and unit metadata for one process field. */
typedef struct catchem_process_field_v1 {
    uint32_t schema_version;      ///< Must be 1 on input (see catchem_manifest_species_v1 for the convention).
    uint32_t struct_size;         ///< Must be sizeof(*this) on input.
    int rank;                     ///< Declared rank (axes.size()); at most 4 or the query fails.
    int axes[4];                  ///< SemanticAxis ordinal per dimension (Column, Level, Interface, SoilLayer, Species, Category, Singleton — see catchem_field_contract.hpp); unused slots are -1.
    int persistence;              ///< PersistencePolicy ordinal: 0 timestep (re-imported every step), 1 persistent (static across the run).
    int requirement;              ///< FieldRequirement ordinal: 0 required (absence is an error), 1 optional (scheme degrades gracefully).
    int access;                   ///< AccessIntent ordinal: 0 read, 1 write, 2 read-write.
    int execution_space;          ///< ExecutionSpaceIntent ordinal: 0 host, 1 device, 2 either.
    int produced;                 ///< Non-zero when the process writes the field rather than consuming it.
    int name_required_len;        ///< Buffer size (incl. NUL) needed for the field name.
    int units_required_len;       ///< Buffer size (incl. NUL) needed for the units string.
} catchem_process_field_v1;

/**
 * @brief Create a Core with host-provided grid and species extents.
 *
 * @param nc       Number of horizontal columns (nx*ny of the host-local tile); must be positive.
 * @param nl       Number of vertical levels; must be positive.
 * @param ns       Number of chemical species; must be positive.
 * @param core_out On success receives the opaque Core handle; always set to
 *                 NULL before the call and on failure. Release it with
 *                 catchem_core_destroy_checked().
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT (core_out null),
 *         CATCHEM_EXTENT_MISMATCH (a non-positive extent), or
 *         CATCHEM_INTERNAL_ERROR (allocation/registration failure).
 */
int catchem_core_create_checked(int nc, int nl, int ns, void** core_out);
/**
 * @brief Create a Core from configuration and return its checked handle.
 *
 * @details Grid extents come from the file's grid section; registers the
 * built-in C++ process wrappers (weakly-linked schemes absent from the
 * build are skipped).
 * @param config_file Null-terminated YAML path; parsed eagerly, so a bad
 *                    file fails here rather than at first run.
 * @param core_out    On success receives the opaque Core handle; NULL on any
 *                    failure. Release with catchem_core_destroy_checked().
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, CATCHEM_INVALID_STATE
 *         (configuration rejected or unreadable), or CATCHEM_INTERNAL_ERROR.
 */
int catchem_core_create_from_config_checked(const char* config_file, void** core_out);
/**
 * @brief Create a configured Core sized to the host-local grid.
 *
 * @details Configuration comes from the file; the grid is sized by the host,
 * which is required under domain decomposition (e.g. UFS per-rank tiles).
 * @param config_file Null-terminated YAML path.
 * @param ncols       Host-local column count (nx*ny of this rank's tile); must be positive.
 * @param nlevels     Number of vertical levels; must be positive.
 * @param core_out    On success receives the opaque Core handle; NULL on any
 *                    failure. Release with catchem_core_destroy_checked().
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, CATCHEM_EXTENT_MISMATCH
 *         (non-positive grid), CATCHEM_INVALID_STATE (configuration rejected),
 *         or CATCHEM_INTERNAL_ERROR.
 */
int catchem_core_create_from_config_with_grid_checked(const char* config_file, int ncols, int nlevels, void** core_out);
/**
 * @brief Open immutable process and species metadata from configuration.
 *
 * @details A manifest is read-only discovery data; it is independent of any
 * Core and may be opened before creating one.
 * @param config_file  Null-terminated YAML path naming the mechanism file.
 * @param manifest_out On success receives the opaque manifest handle; NULL on
 *                     failure. Release with catchem_manifest_destroy_checked().
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT,
 *         CATCHEM_INVALID_CONFIGURATION (file failed to parse/validate), or
 *         CATCHEM_INTERNAL_ERROR.
 */
int catchem_manifest_open_checked(const char* config_file, void** manifest_out);
/**
 * @brief Return the species count in an open manifest.
 * @param manifest  Handle from catchem_manifest_open_checked().
 * @param count_out Receives the number of mechanism species.
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, CATCHEM_INVALID_HANDLE,
 *         CATCHEM_WRONG_HANDLE_TYPE, or CATCHEM_INVALID_STATE.
 */
int catchem_manifest_get_species_count_checked(void* manifest, int* count_out);
/**
 * @brief Copy one species name using a caller-owned buffer.
 * @param manifest     Handle from catchem_manifest_open_checked().
 * @param index        0-based species index (manifest indices are 0-based).
 * @param buffer       Destination; pass NULL with buffer_len 0 to probe size only.
 * @param buffer_len   Capacity of @p buffer including the NUL.
 * @param required_len Receives the needed length including the NUL; required.
 * @return CATCHEM_SUCCESS, or CATCHEM_NULL_ARGUMENT, CATCHEM_INVALID_INDEX,
 *         CATCHEM_EXTENT_MISMATCH (buffer too short; nothing copied),
 *         CATCHEM_INVALID_HANDLE, CATCHEM_WRONG_HANDLE_TYPE, CATCHEM_INVALID_STATE.
 */
int catchem_manifest_get_species_name_checked(void* manifest, int index, char* buffer, int buffer_len, int* required_len);
/**
 * @brief Copy one species CF standard name using a caller-owned buffer.
 * @param manifest     Handle from catchem_manifest_open_checked().
 * @param index        0-based species index.
 * @param buffer       Destination; pass NULL with buffer_len 0 to probe size only.
 * @param buffer_len   Capacity of @p buffer including the NUL.
 * @param required_len Receives the needed length including the NUL; required.
 * @return CATCHEM_SUCCESS, or the same status codes as
 *         catchem_manifest_get_species_name_checked().
 */
int catchem_manifest_get_species_standard_name_checked(void* manifest, int index, char* buffer, int buffer_len, int* required_len);
/**
 * @brief Copy a species descriptor and its name, standard name, and units.
 * @param manifest          Handle from catchem_manifest_open_checked().
 * @param index             0-based species index.
 * @param descriptor        Caller-filled catchem_manifest_species_v1 (schema_version
 *                          and struct_size must be set on input); receives the metadata.
 * @param name              Buffer for the short name; capacity must be at least
 *                          descriptor->name_required_len.
 * @param name_len          Capacity of @p name including the NUL.
 * @param standard_name     Buffer for the CF standard name; capacity must be at
 *                          least descriptor->standard_name_required_len.
 * @param standard_name_len Capacity of @p standard_name including the NUL.
 * @param units             Buffer for the units string; capacity must be at least
 *                          descriptor->units_required_len.
 * @param units_len         Capacity of @p units including the NUL.
 * @return CATCHEM_SUCCESS, or CATCHEM_NULL_ARGUMENT, CATCHEM_CONTRACT_VIOLATION
 *         (bad schema_version/struct_size), CATCHEM_INVALID_INDEX,
 *         CATCHEM_EXTENT_MISMATCH (any buffer too short; no strings copied),
 *         or the handle/state codes of catchem_manifest_get_species_name_checked().
 */
int catchem_manifest_get_species_v1_checked(void* manifest, int index, catchem_manifest_species_v1* descriptor,
                                            char* name, int name_len, char* standard_name, int standard_name_len,
                                            char* units, int units_len);
/**
 * @brief Copy one alias for a species using a caller-owned buffer.
 * @param manifest     Handle from catchem_manifest_open_checked().
 * @param species_index 0-based species index.
 * @param alias_index  0-based alias index, below the descriptor's alias_count.
 * @param buffer       Destination; NULL with buffer_len 0 probes size only.
 * @param buffer_len   Capacity of @p buffer including the NUL.
 * @param required_len Receives the needed length including the NUL; required.
 * @return CATCHEM_SUCCESS, or the same status codes as
 *         catchem_manifest_get_species_name_checked().
 */
int catchem_manifest_get_species_alias_checked(void* manifest, int species_index, int alias_index, char* buffer,
                                               int buffer_len, int* required_len);
/**
 * @brief Copy one semantic role for a species using a caller-owned buffer.
 * @param manifest     Handle from catchem_manifest_open_checked().
 * @param species_index 0-based species index.
 * @param role_index   0-based role index, below the descriptor's role_count.
 * @param buffer       Destination; NULL with buffer_len 0 probes size only.
 * @param buffer_len   Capacity of @p buffer including the NUL.
 * @param required_len Receives the needed length including the NUL; required.
 * @return CATCHEM_SUCCESS, or the same status codes as
 *         catchem_manifest_get_species_name_checked().
 */
int catchem_manifest_get_species_role_checked(void* manifest, int species_index, int role_index, char* buffer,
                                              int buffer_len, int* required_len);
/**
 * @brief Return the process count in an open manifest.
 * @param manifest  Handle from catchem_manifest_open_checked().
 * @param count_out Receives the number of known processes (available or not).
 * @return CATCHEM_SUCCESS or the handle/argument codes of
 *         catchem_manifest_get_species_count_checked().
 */
int catchem_manifest_get_process_count_checked(void* manifest, int* count_out);
/**
 * @brief Copy one process name and report its availability.
 * @param manifest     Handle from catchem_manifest_open_checked().
 * @param index        0-based process index.
 * @param name         Destination buffer; NULL with name_len 0 probes size only.
 * @param name_len     Capacity of @p name including the NUL.
 * @param required_len Receives the needed length including the NUL; required.
 * @param available    Receives non-zero when the process is compiled into this
 *                     build; required.
 * @return CATCHEM_SUCCESS or the handle/argument/index/extent codes of
 *         catchem_manifest_get_species_name_checked().
 */
int catchem_manifest_get_process_checked(void* manifest, int index, char* name, int name_len, int* required_len, int* available);
/**
 * @brief Copy a process descriptor, name, and CCPP scheme name.
 * @param manifest   Handle from catchem_manifest_open_checked().
 * @param index      0-based process index.
 * @param descriptor Caller-filled catchem_manifest_process_v1 (schema_version
 *                   and struct_size set on input); receives availability and buffer sizes.
 * @param name       Buffer for the process name; capacity at least descriptor->name_required_len.
 * @param name_len   Capacity of @p name including the NUL.
 * @param scheme     Buffer for the active scheme name; capacity at least
 *                   descriptor->scheme_required_len.
 * @param scheme_len Capacity of @p scheme including the NUL.
 * @return CATCHEM_SUCCESS, or CATCHEM_NULL_ARGUMENT, CATCHEM_CONTRACT_VIOLATION,
 *         CATCHEM_INVALID_INDEX, CATCHEM_EXTENT_MISMATCH, or the handle/state
 *         codes of catchem_manifest_get_species_name_checked().
 */
int catchem_manifest_get_process_v1_checked(void* manifest, int index, catchem_manifest_process_v1* descriptor,
                                            char* name, int name_len, char* scheme, int scheme_len);
/**
 * @brief Copy the immutable manifest fingerprint.
 *
 * @details The fingerprint summarizes the mechanism file and species content;
 * it is what catchem_manifest_validate_config_checked() and
 * catchem_manifest_validate_core_checked() compare.
 * @param manifest     Handle from catchem_manifest_open_checked().
 * @param buffer       Destination; NULL with buffer_len 0 probes size only.
 * @param buffer_len   Capacity of @p buffer including the NUL.
 * @param required_len Receives the needed length including the NUL; required.
 * @return CATCHEM_SUCCESS or the handle/argument/extent codes of
 *         catchem_manifest_get_species_name_checked().
 */
int catchem_manifest_get_fingerprint_checked(void* manifest, char* buffer, int buffer_len, int* required_len);
/**
 * @brief Verify that a configuration matches the open manifest.
 * @param manifest    Handle from catchem_manifest_open_checked().
 * @param config_file Null-terminated YAML path to check; its mechanism must
 *                    produce the same fingerprint.
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT,
 *         CATCHEM_INVALID_CONFIGURATION (candidate file unreadable/invalid),
 *         CATCHEM_CONTRACT_VIOLATION (fingerprint differs), or the handle/state
 *         codes of catchem_manifest_get_species_name_checked().
 */
int catchem_manifest_validate_config_checked(void* manifest, const char* config_file);
/**
 * @brief Verify that a Core was initialized from the open manifest.
 * @param manifest Handle from catchem_manifest_open_checked().
 * @param core     Core handle from a catchem_core_create*_checked() entry point.
 * @return CATCHEM_SUCCESS, CATCHEM_CONTRACT_VIOLATION (Core's mechanism
 *         fingerprint differs), or the handle/argument codes of the other
 *         manifest queries.
 */
int catchem_manifest_validate_core_checked(void* manifest, void* core);
/**
 * @brief Destroy an open manifest handle.
 * @param manifest Handle from catchem_manifest_open_checked(); invalid
 *                 afterwards. Safe to call while readers hold leases — the
 *                 registry blocks until they drain.
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, CATCHEM_INVALID_HANDLE, or
 *         CATCHEM_WRONG_HANDLE_TYPE.
 */
int catchem_manifest_destroy_checked(void* manifest);
/**
 * @brief Begin a host-owned process interval with its authoritative clock.
 *
 * @details Selected-process runs are grouped into intervals: exactly one
 * interval must be open per Core, every catchem_core_run_process_checked()
 * references its interval_id, and catchem_core_end_interval_checked() closes
 * it. Re-opening without ending fails as a contract violation.
 * @param core     Core handle from a catchem_core_create*_checked() entry point.
 * @param interval Caller-filled catchem_host_interval_v1 (schema_version and
 *                 struct_size set on input); copied, not retained.
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, CATCHEM_INVALID_HANDLE,
 *         CATCHEM_WRONG_HANDLE_TYPE, or CATCHEM_CONTRACT_VIOLATION (bad
 *         schema/size, non-finite or non-positive duration, or an interval
 *         already open).
 */
int catchem_core_begin_interval_checked(void* core, const catchem_host_interval_v1* interval);
/**
 * @brief Run one selected process invocation and report publication eligibility.
 *
 * @details The identity (interval_id, site, subcycle, process) must be unique
 * within the interval; repeating it is rejected. The process must be
 * scheduled on the Core and its contract fields satisfied by the current
 * import generation.
 * @param core                 Core handle; an interval must be open.
 * @param invocation           Caller-filled catchem_process_invocation_v1
 *                             (schema_version and struct_size set on input); copied, not retained.
 * @param operation_status     Receives the SelectedOperationStatus ordinal:
 *                             0 succeeded, 1 rejected (identity/contract),
 *                             2 failed (the scheme threw). Required.
 * @param publication_eligible Receives non-zero when the Core allows the
 *                             process's products to be published for this
 *                             interval; required.
 * @param import_generation_out Receives the state import generation the run
 *                             used, so hosts can detect stale inputs; required.
 * @return CATCHEM_SUCCESS, CATCHEM_PROCESS_FAILURE (status 2; cause in
 *         catchem_get_last_error()), CATCHEM_CONTRACT_VIOLATION (status 1 or
 *         bad descriptor), CATCHEM_NULL_ARGUMENT, or the handle codes of
 *         catchem_core_begin_interval_checked().
 */
int catchem_core_run_process_checked(void* core, const catchem_process_invocation_v1* invocation,
                                     int* operation_status, int* publication_eligible,
                                     uint64_t* import_generation_out);
/**
 * @brief End the matching host-owned process interval.
 * @param core        Core handle with the interval open.
 * @param interval_id Must equal the id passed to the open
 *                    catchem_core_begin_interval_checked(); a mismatch is a
 *                    contract violation.
 * @return CATCHEM_SUCCESS, CATCHEM_INVALID_HANDLE, CATCHEM_WRONG_HANDLE_TYPE,
 *         CATCHEM_CONTRACT_VIOLATION (id mismatch or no interval open), or
 *         CATCHEM_INTERNAL_ERROR.
 */
int catchem_core_end_interval_checked(void* core, uint64_t interval_id);
/**
 * @brief Read the contract for one configured process.
 * @param core             Core handle from a catchem_core_create*_checked() entry point.
 * @param process_index    0-based index below catchem_core_get_num_processes().
 * @param descriptor       Caller-filled catchem_process_contract_v1 (schema_version
 *                         and struct_size set on input); receives the counts and recovery capability.
 * @param process_name     Buffer receiving the process name; capacity at least
 *                         descriptor->process_name_required_len.
 * @param process_name_len Capacity of @p process_name including the NUL.
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, CATCHEM_CONTRACT_VIOLATION
 *         (bad descriptor schema/size), CATCHEM_INVALID_INDEX,
 *         CATCHEM_EXTENT_MISMATCH (name buffer too short), or the handle codes
 *         of catchem_core_begin_interval_checked().
 */
int catchem_core_get_process_contract_checked(void* core, int process_index,
                                              catchem_process_contract_v1* descriptor,
                                              char* process_name, int process_name_len);
/**
 * @brief Read one field contract for a configured process.
 * @param core           Core handle from a catchem_core_create*_checked() entry point.
 * @param process_index  0-based process index below the contract count.
 * @param field_index    0-based field index below the contract's field_count.
 * @param descriptor     Caller-filled catchem_process_field_v1 (schema_version
 *                       and struct_size set on input); receives rank, axes, and policies.
 * @param field_name     Buffer for the field name; capacity at least
 *                       descriptor->name_required_len.
 * @param field_name_len Capacity of @p field_name including the NUL.
 * @param units          Buffer for the units string; capacity at least
 *                       descriptor->units_required_len.
 * @param units_len      Capacity of @p units including the NUL.
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, CATCHEM_CONTRACT_VIOLATION
 *         (bad descriptor or field rank above 4), CATCHEM_INVALID_INDEX,
 *         CATCHEM_EXTENT_MISMATCH (a buffer too short), or the handle codes
 *         of catchem_core_begin_interval_checked().
 */
int catchem_core_get_process_field_checked(void* core, int process_index, int field_index,
                                           catchem_process_field_v1* descriptor,
                                           char* field_name, int field_name_len,
                                           char* units, int units_len);
/**
 * @brief Destroy a Core handle and its owned resources.
 *
 * @details Blocks until every outstanding lease on the Core drains, then runs
 * process finalize() hooks in reverse schedule order and frees the Core. The
 * StateManager handle it produced is invalidated with it.
 * @param core_ptr Core handle; invalid afterwards even on ShutdownFailure
 *                 (the Core is always deleted).
 * @return CATCHEM_SUCCESS, CATCHEM_INVALID_HANDLE / CATCHEM_WRONG_HANDLE_TYPE
 *         (null, stale, destroyed, or wrong-type handle), or
 *         CATCHEM_SHUTDOWN_FAILURE (a finalize() threw; remaining finalizers
 *         still ran — read catchem_get_last_error() for the cause).
 */
int catchem_core_destroy_checked(void* core_ptr);
/**
 * @brief Return the StateManager handle owned by a Core.
 * @param core_ptr  Core handle from a catchem_core_create*_checked() entry point.
 * @param state_out Receives the opaque StateManager handle, valid until the
 *                  owning Core is destroyed; do not destroy it separately.
 *                  NULL on failure.
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, or the handle codes of
 *         catchem_core_begin_interval_checked().
 */
int catchem_core_get_state_manager_checked(void* core_ptr, void** state_out);
/**
 * @brief Copy the most recent boundary error into a caller-owned buffer.
 *
 * @details Reads this thread's thread-local record (operation [object]: cause),
 * written by the last failing checked call on this thread. Each checked call
 * clears the record on entry, so a stale cause is never returned after a
 * success.
 * @param buffer  Destination; truncated to fit, always NUL-terminated. Required.
 * @param max_len Capacity of @p buffer including the NUL; must be positive.
 * @return The status code of the recorded error, or CATCHEM_SUCCESS when no
 *         error is recorded, or CATCHEM_NULL_ARGUMENT for a bad buffer.
 */
int catchem_get_last_error(char* buffer, int max_len);

/**
 * @brief Creates the C++ Core orchestrator instance.
 * @param nc Number of horizontal columns.
 * @param nl Number of vertical levels.
 * @param ns Number of chemical species.
 * @return Opacity-wrapped void* handle pointing to catchem::Core.
 */
void* catchem_core_create(int nc, int nl, int ns);

/**
 * @brief Creates the C++ Core orchestrator from a YAML config file.
 * @param config_file Null-terminated filesystem path.
 * @return Opacity-wrapped void* handle pointing to catchem::Core.
 */
void* catchem_core_create_from_config(const char* config_file);

/**
 * @brief Creates the Core from a YAML config file with host-supplied grid dimensions.
 *
 * Configuration comes from the file; the grid is sized by the host (required
 * under domain decomposition, e.g. UFS per-rank tiles).
 * @param config_file Null-terminated filesystem path.
 * @param ncols Host-local number of columns (nx*ny).
 * @param nlevels Number of vertical levels.
 * @return Opacity-wrapped void* handle pointing to catchem::Core, or NULL on failure.
 */
void* catchem_core_create_from_config_with_grid(const char* config_file, int ncols, int nlevels);

/**
 * @brief Destroys the Core orchestrator instance and releases heap memory.
 * @param core_ptr Pointer to the active catchem::Core instance.
 */
void catchem_core_destroy(void* core_ptr);

/**
 * @brief Extracts the underlying StateManager handle from Core.
 * @param core_ptr Pointer to the active catchem::Core instance.
 * @return Pointer to catchem::StateManager.
 */
void* catchem_core_get_state_manager(void* core_ptr);

/**
 * @brief Bind a 1D double field to the StateManager registry.
 *
 * @details Legacy wrapper: sizes the binding from the state's column count
 * and forwards to catchem_state_bind_1d_checked(), swallowing its status.
 * @param state_ptr StateManager handle from catchem_core_get_state_manager_checked().
 * @param name      Field name; matched case-insensitively through the alias table.
 * @param ptr       Host data of length column_count(); non-owning — the caller
 *                  must keep it alive and re-bind before reuse after invalidation.
 * @note Prefer catchem_state_bind_1d_checked(), which reports failures.
 */
void catchem_state_bind_1d(void* state_ptr, const char* name, double* ptr);
/**
 * @brief Bind a 1D double field with an explicit extent check.
 * @param state_ptr StateManager handle; not retained.
 * @param name      Field name; must be non-null.
 * @param ptr       Host data; must be non-null and remain valid while bound.
 * @param dim1      Element count; must equal the state's column_count().
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, CATCHEM_EXTENT_MISMATCH
 *         (bad dim1), or the handle codes of catchem_core_begin_interval_checked().
 */
int catchem_state_bind_1d_checked(void* state_ptr, const char* name, double* ptr, int dim1);

/**
 * @brief Bind a 2D double field to the StateManager registry.
 *
 * @details Legacy wrapper over catchem_state_bind_2d_checked() with the
 * state's extents; failures are silent.
 * @param state_ptr StateManager handle.
 * @param name      Field name.
 * @param ptr       Host data [column_count, 1]; non-owning.
 * @note Prefer the checked sibling.
 */
void catchem_state_bind_2d(void* state_ptr, const char* name, double* ptr);
/**
 * @brief Bind a 2D double field with explicit extent checks.
 * @param state_ptr StateManager handle; not retained.
 * @param name      Field name; must be non-null.
 * @param ptr       Host data; must be non-null and remain valid while bound.
 * @param dim1      First extent; must equal column_count().
 * @param dim2      Second extent; must be 1 for 2D state fields.
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, CATCHEM_EXTENT_MISMATCH,
 *         or the handle codes of catchem_core_begin_interval_checked().
 */
int catchem_state_bind_2d_checked(void* state_ptr, const char* name, double* ptr, int dim1, int dim2);

/**
 * @brief Bind a 3D double field to the StateManager registry.
 *
 * @details Legacy wrapper over catchem_state_bind_3d_checked() with the
 * state's extents; failures are silent.
 * @param state_ptr StateManager handle.
 * @param name      Field name.
 * @param ptr       Host data [column_count, level_count, 1]; non-owning.
 * @note Prefer the checked sibling.
 */
void catchem_state_bind_3d(void* state_ptr, const char* name, double* ptr);
/**
 * @brief Bind a 3D double field with explicit extent checks.
 * @param state_ptr StateManager handle; not retained.
 * @param name      Field name; must be non-null.
 * @param ptr       Host data; must be non-null and remain valid while bound.
 * @param dim1      Column extent; must equal column_count().
 * @param dim2      Level extent; must equal level_count().
 * @param dim3      Trailing extent; must be 1 for 3D state fields.
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, CATCHEM_EXTENT_MISMATCH,
 *         or the handle codes of catchem_core_begin_interval_checked().
 */
int catchem_state_bind_3d_checked(void* state_ptr, const char* name, double* ptr, int dim1, int dim2, int dim3);

/**
 * @brief Bind a 2D meteorological field by name.
 *
 * @details Legacy wrapper over catchem_state_bind_met_2d_checked(); failures
 * are silent. Met fields live in the MetState registry rather than the
 * generic field maps.
 * @param state_ptr StateManager handle.
 * @param name      Met field name (alias-folded, e.g. "T", "TEMP").
 * @param ptr       Host data [column_count, 1]; non-owning.
 */
void catchem_state_bind_met_2d(void* state_ptr, const char* name, double* ptr);

/**
 * @brief Bind a 3D meteorological field by name.
 *
 * @details Legacy wrapper over catchem_state_bind_met_3d_checked(); failures
 * are silent.
 * @param state_ptr StateManager handle.
 * @param name      Met field name.
 * @param ptr       Host data [column_count, level_count, 1]; non-owning.
 */
void catchem_state_bind_met_3d(void* state_ptr, const char* name, double* ptr);
/**
 * @brief Start a new import generation, invalidating timestep-persistent fields.
 *
 * @details Call once per host import step before re-binding fields whose
 * contents changed; fields with PersistencePolicy::Timestep are considered
 * stale for the previous generation until re-bound. Persistent fields
 * (coordinates, areas) survive the bump.
 * @param state_ptr StateManager handle.
 * @return CATCHEM_SUCCESS, the handle codes of
 *         catchem_core_begin_interval_checked(), or CATCHEM_INTERNAL_ERROR.
 */
int catchem_state_begin_import_generation(void* state_ptr);
/**
 * @brief Select how derived-meteorology validation failures are treated.
 * @param state_ptr StateManager handle.
 * @param policy    PhysicalValidationPolicy ordinal: 0 reject (default),
 *                  1 warn-and-clamp, 2 count-and-continue
 *                  (catchem_physical_validation.hpp). Out-of-range is rejected.
 * @return CATCHEM_SUCCESS, CATCHEM_INVALID_STATE (bad ordinal), or the handle
 *         codes of catchem_core_begin_interval_checked().
 */
int catchem_state_set_physical_validation_policy_checked(void* state_ptr, int policy);
/**
 * @brief Read the accumulated physical-validation report.
 * @param state_ptr     StateManager handle.
 * @param issue_count   Receives the number of violations recorded since the
 *                      last reset; required.
 * @param detail        Buffer receiving the formatted multi-line report
 *                      (truncated to fit, NUL-terminated); required.
 * @param detail_length Capacity of @p detail; must be positive.
 * @return CATCHEM_SUCCESS (outputs may be zero/empty), CATCHEM_NULL_ARGUMENT,
 *         or the handle codes of catchem_core_begin_interval_checked().
 */
int catchem_state_get_physical_validation_report_checked(void* state_ptr, int* issue_count, char* detail,
                                                         int detail_length);
/**
 * @brief Bind a 2D meteorological field with an explicit extent check.
 * @param state_ptr StateManager handle; not retained.
 * @param name      Met field name; must be non-null.
 * @param ptr       Host data; must be non-null and remain valid while bound.
 * @param dim1      First extent; must equal column_count().
 * @param dim2      Second extent; must be 1.
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, CATCHEM_EXTENT_MISMATCH,
 *         or the handle codes of catchem_core_begin_interval_checked().
 */
int catchem_state_bind_met_2d_checked(void* state_ptr, const char* name, double* ptr, int dim1, int dim2);
/**
 * @brief Bind a 3D meteorological field with explicit extent checks.
 * @param state_ptr StateManager handle; not retained.
 * @param name      Met field name; must be non-null.
 * @param ptr       Host data; must be non-null and remain valid while bound.
 * @param dim1      Column extent; must equal column_count().
 * @param dim2      Level extent; must equal level_count().
 * @param dim3      Trailing extent; must be 1.
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, CATCHEM_EXTENT_MISMATCH,
 *         or the handle codes of catchem_core_begin_interval_checked().
 */
int catchem_state_bind_met_3d_checked(void* state_ptr, const char* name, double* ptr, int dim1, int dim2, int dim3);
/**
 * @brief Bind a 3D meteorological field declaring its vertical semantics.
 * @param state_ptr     StateManager handle; not retained.
 * @param name          Met field name; must be non-null.
 * @param ptr           Host data; must be non-null and remain valid while bound.
 * @param dim1          Column extent; must equal column_count().
 * @param dim2          Vertical extent; must match the declared axis (level_count
 *                      for Level, level_count+1 for Interface).
 * @param dim3          Trailing extent; must be 1.
 * @param semantic_axis Vertical-axis ordinal: 1 Level (layer midpoints) or
 *                      2 Interface (layer boundaries) from SemanticAxis
 *                      (catchem_field_contract.hpp); other ordinals are rejected.
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, CATCHEM_EXTENT_MISMATCH,
 *         CATCHEM_INVALID_STATE (bad axis ordinal), or the handle codes of
 *         catchem_core_begin_interval_checked().
 */
int catchem_state_bind_met_3d_axis_checked(void* state_ptr, const char* name, double* ptr, int dim1, int dim2, int dim3,
                                           int semantic_axis);

/**
 * @brief Binds the contiguous, multi-species unified chemistry concentrations array.
 *
 * @details Legacy wrapper over catchem_state_bind_unified_chemistry_checked()
 * with the state's extents; failures are silent.
 * @param state_ptr StateManager handle.
 * @param ptr       Host data [column_count, level_count, species_count] in
 *                  the state's species order; non-owning, written in place by
 *                  chemistry processes.
 */
void catchem_state_bind_unified_chemistry(void* state_ptr, double* ptr);
/**
 * @brief Bind the unified chemistry array with explicit extent checks.
 * @param state_ptr StateManager handle; not retained.
 * @param ptr       Host data; must be non-null and remain valid while bound.
 * @param dim1      Column extent; must equal column_count().
 * @param dim2      Level extent; must equal level_count().
 * @param dim3      Species extent; must equal species_count().
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, CATCHEM_EXTENT_MISMATCH,
 *         or the handle codes of catchem_core_begin_interval_checked().
 */
int catchem_state_bind_unified_chemistry_checked(void* state_ptr, double* ptr, int dim1, int dim2, int dim3);
/**
 * @brief Mark the unified chemistry array host-modified after host writes.
 *
 * @details Call after the host changes concentration values in place between
 * imports so the next device sync pushes host contents instead of pulling
 * stale device data.
 * @param state_ptr StateManager handle.
 * @return CATCHEM_SUCCESS, CATCHEM_INVALID_STATE (no array bound), or the
 *         handle codes of catchem_core_begin_interval_checked().
 */
int catchem_state_mark_chem_host_modified(void* state_ptr);

/**
 * @brief Sets current simulation time within the state.
 *
 * @details Legacy wrapper over catchem_state_set_time_checked(); failures are
 * silent.
 * @param state_ptr StateManager handle.
 * @param yr,mo,dy  UTC calendar date components.
 * @param hr,mn,sc  UTC time-of-day components.
 * @param doy       Day of year (1-366); recomputed by the clock when inconsistent.
 * @param tstep     Timestep length [s].
 */
void catchem_state_set_time(void* state_ptr, int yr, int mo, int dy, int hr, int mn, int sc, int doy, double tstep);
/**
 * @brief Set the simulation clock with validation.
 * @param state_ptr StateManager handle.
 * @param yr,mo,dy  UTC calendar date; validated as a real date.
 * @param hr,mn,sc  UTC time of day; must be within 0-23, 0-59, 0-59.
 * @param doy       Day of year; must agree with the date.
 * @param tstep     Timestep length [s]; must be positive and finite.
 * @return CATCHEM_SUCCESS, CATCHEM_INVALID_STATE (rejected clock update —
 *         the state keeps its previous clock), or the handle codes of
 *         catchem_core_begin_interval_checked().
 */
int catchem_state_set_time_checked(void* state_ptr, int yr, int mo, int dy, int hr, int mn, int sc, int doy,
                                   double tstep);

/**
 * @brief Synchronizes registered host pointers to Kokkos device memory space.
 *
 * @details Legacy wrapper over catchem_state_sync_to_device_checked().
 * @param state_ptr StateManager handle.
 */
void catchem_state_sync_to_device(void* state_ptr);
/**
 * @brief Copy host-marked fields into their device mirrors.
 * @param state_ptr StateManager handle.
 * @return CATCHEM_SUCCESS, CATCHEM_INVALID_HANDLE, CATCHEM_WRONG_HANDLE_TYPE,
 *         or CATCHEM_INTERNAL_ERROR (device sync failure).
 */
int catchem_state_sync_to_device_checked(void* state_ptr);

/**
 * @brief Synchronizes Kokkos device calculations back to host buffers.
 *
 * @details Legacy wrapper over catchem_state_sync_to_host_checked(). After
 * this call the caller's bound host arrays contain the latest device values.
 * @param state_ptr StateManager handle.
 */
void catchem_state_sync_to_host(void* state_ptr);
/**
 * @brief Copy device-modified fields back into caller host buffers.
 * @param state_ptr StateManager handle.
 * @return CATCHEM_SUCCESS, CATCHEM_INVALID_HANDLE, CATCHEM_WRONG_HANDLE_TYPE,
 *         or CATCHEM_INTERNAL_ERROR (device sync failure).
 */
int catchem_state_sync_to_host_checked(void* state_ptr);

/**
 * @brief Retrieves direct host pointers from the 1D, 2D, or 3D fields.
 *
 * @details Each returns the bound host address for a name (alias-folded,
 * case-insensitive), or nullptr when the handle is invalid, the field is
 * unbound, or its name is unknown. The pointer remains valid until the next
 * import-generation bump that drops the binding.
 * @param state_ptr StateManager handle.
 * @param name      Field name.
 * @return Borrowed host pointer to the field data, or nullptr.
 */
double* catchem_state_get_pointer_1d(void* state_ptr, const char* name);
/// @copydoc catchem_state_get_pointer_1d
double* catchem_state_get_pointer_2d(void* state_ptr, const char* name);
/// @copydoc catchem_state_get_pointer_1d
double* catchem_state_get_pointer_3d(void* state_ptr, const char* name);
/**
 * @brief Checked form of catchem_state_get_pointer_3d().
 * @param state_ptr StateManager handle.
 * @param name      Field name.
 * @param ptr_out   Receives the borrowed host pointer, or nullptr when the
 *                  field is unbound; required.
 * @return CATCHEM_SUCCESS (even for an unbound field — check *ptr_out), or
 *         the handle/argument codes of catchem_state_bind_1d_checked().
 */
int catchem_state_get_pointer_3d_checked(void* state_ptr, const char* name, void** ptr_out);
/**
 * @brief Borrow the [column, level] slab for one species of the unified array.
 *
 * @details Legacy form; returns nullptr on any failure.
 * @param state_ptr     StateManager handle.
 * @param species_index 1-based species index (Fortran convention).
 * @return Borrowed host pointer into the bound unified chemistry array, or nullptr.
 */
double* catchem_state_get_species_conc_pointer(void* state_ptr, int species_index);
/**
 * @brief Checked form of catchem_state_get_species_conc_pointer().
 * @param state_ptr     StateManager handle.
 * @param species_index 1-based species index (Fortran convention).
 * @param dim1          Expected column extent; must equal column_count().
 * @param dim2          Expected level extent; must equal level_count().
 * @param ptr_out       Receives the borrowed slab pointer; required.
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, CATCHEM_INVALID_INDEX,
 *         CATCHEM_EXTENT_MISMATCH, CATCHEM_INVALID_STATE (array unbound), or
 *         the handle codes of catchem_core_begin_interval_checked().
 */
int catchem_state_get_species_conc_pointer_checked(void* state_ptr, int species_index, int dim1, int dim2,
                                                   double** ptr_out);

/**
 * @brief Executes a single timestepped execution over scheduled processes.
 * @param core_ptr Core handle from catchem_core_create*().
 * @param dt       Step size [s]; must be positive and finite.
 * @return 0 on success; CATCHEM_INVALID_STATE when a scheduled process threw
 *         (partial state possible — inspect the outcome), CATCHEM_INTERNAL_ERROR
 *         for unknown exceptions, or the handle codes of
 *         catchem_core_begin_interval_checked().
 * @see catchem_core_get_timestep_outcome()
 */
int catchem_core_run_timestep(void* core_ptr, double dt);
/**
 * @brief Copy out the outcome of the most recent timestep or selected-process run.
 * @param core_ptr             Core handle.
 * @param status               Receives the TimestepStatus ordinal: 0 NotStarted,
 *                             1 Running, 2 Succeeded, 3 PartialUpdate,
 *                             4 ValidationFailed (catchem_process_interface.hpp).
 * @param timestep             Receives the monotonic Core step ordinal.
 * @param duration             Receives the step/interval duration [s].
 * @param import_generation    Receives the state import generation executed against.
 * @param process_index        Receives the schedule index of the failing process.
 * @param state_classification Receives the StateClassification ordinal: 0 Reusable,
 *                             1 RequiresReimport, 2 RequiresReinitialize — what the
 *                             host owes before the next execution.
 * @param process_name         Buffer receiving the failing process name ("" otherwise).
 * @param process_name_len     Capacity of @p process_name.
 * @param cause                Buffer receiving the failure/validation reason text.
 * @param cause_len            Capacity of @p cause.
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT (any required output is null),
 *         or the handle codes of catchem_core_begin_interval_checked().
 */
int catchem_core_get_timestep_outcome(void* core_ptr, int* status, long long* timestep, double* duration,
                                      long long* import_generation, int* process_index, int* state_classification,
                                      char* process_name, int process_name_len, char* cause, int cause_len);

/**
 * @brief Registers and attaches an active physics process handler.
 *
 * @details Legacy wrapper: ensures the built-in process factories are
 * registered, then creates the named process, initialises it against the
 * Core's StateManager, and appends it to the schedule.
 * @param core_ptr Core handle.
 * @param name     Registered process name (e.g. "dust", "seasalt", "so4chem");
 *                 an unknown name throws a C++ exception across the extern "C"
 *                 boundary — validate against catchem_core_get_num_processes_checked()
 *                 sources or prefer the checked lifecycle APIs.
 */
void catchem_core_add_process_by_name(void* core_ptr, const char* name);

/**
 * @brief Returns the number of active physics processes scheduled on the Core.
 *
 * @details Legacy wrapper over catchem_core_get_num_processes_checked();
 * returns 0 when the handle is invalid.
 * @param core_ptr Core handle.
 * @return Scheduled process count, or 0 on failure.
 */
int catchem_core_get_num_processes(void* core_ptr);
/**
 * @brief Checked form of catchem_core_get_num_processes().
 * @param core_ptr  Core handle.
 * @param count_out Receives the scheduled process count; required.
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, or the handle codes of
 *         catchem_core_begin_interval_checked().
 */
int catchem_core_get_num_processes_checked(void* core_ptr, int* count_out);
/**
 * @brief Count the host fields the scheduled processes require before execution.
 * @param core_ptr  Core handle.
 * @param count_out Receives the number of distinct required host field names; required.
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, or the handle codes of
 *         catchem_core_begin_interval_checked().
 */
int catchem_core_get_required_host_field_count_checked(void* core_ptr, int* count_out);
/**
 * @brief Name one required host field by index.
 * @param core_ptr      Core handle.
 * @param index         0-based ordinal below the count from
 *                      catchem_core_get_required_host_field_count_checked().
 * @param name_out      Buffer receiving the field name (NUL-terminated).
 * @param name_out_len  Capacity of @p name_out; must be positive.
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, CATCHEM_INVALID_INDEX, or the
 *         handle codes of catchem_core_begin_interval_checked().
 */
int catchem_core_get_required_host_field_name_checked(void* core_ptr, int index, char* name_out, int name_out_len);

// Grid and Configuration API
/**
 * @brief Read the grid geometry from the Core's GridManager.
 *
 * @details Legacy wrapper: each non-null output receives its dimension; a null
 * core pointer leaves all outputs untouched.
 * @param core_ptr Core handle.
 * @param nx       Receives the x (west-east) cell count; optional.
 * @param ny       Receives the y (south-north) cell count; optional.
 * @param nz       Receives the z (vertical) level count; optional.
 */
void catchem_get_grid_dimensions(void* core_ptr, int* nx, int* ny, int* nz);
/**
 * @brief Read the configured runtime timestep.
 * @param core_ptr Core handle.
 * @return runtime.dt [s], or 0.0 for a null handle.
 */
double catchem_get_config_timestep(void* core_ptr);
/**
 * @brief Read the diagnostic output cadence.
 * @param core_ptr Core handle.
 * @return diagnostics.output.frequency [timesteps between writes], or 0 for a null handle.
 */
int catchem_config_get_output_frequency(void* core_ptr);
/**
 * @brief Read the NetCDF output compression level.
 * @param core_ptr Core handle.
 * @return diagnostics.output.compress_lev (0-9), or 0 for a null handle.
 */
int catchem_config_get_compress_level(void* core_ptr);
/**
 * @brief Read the diagnostic output directory.
 * @param core_ptr Core handle; a null handle yields the default "./".
 * @param buffer  Receives the path (truncated to fit, NUL-terminated); may be null.
 * @param max_len Capacity of @p buffer; non-positive writes nothing.
 */
void catchem_config_get_output_directory(void* core_ptr, char* buffer, int max_len);
/**
 * @brief Read the diagnostic output file-name prefix.
 * @param core_ptr Core handle.
 * @param buffer  Receives the prefix (truncated to fit, NUL-terminated); may be null.
 * @param max_len Capacity of @p buffer.
 */
void catchem_config_get_output_prefix(void* core_ptr, char* buffer, int max_len);
/**
 * @brief Check whether output uses lat-lon field layout.
 * @param core_ptr Core handle.
 * @return 1 when diagnostics.output.format == "latlon", else 0.
 */
int catchem_config_get_latlon_output(void* core_ptr);
/**
 * @brief Check whether diagnostic file output is enabled.
 * @param core_ptr Core handle.
 * @return 1 when diagnostics.output.enabled, else 0 (also 0 for a null handle).
 */
int catchem_config_get_diag_enabled(void* core_ptr);
/**
 * @brief Check whether per-process diagnostic fields are enabled.
 * @param core_ptr Core handle.
 * @return 1 when diagnostics.output.process_diagnostics, else 0.
 */
int catchem_config_get_process_diagnostics_enabled(void* core_ptr);
/**
 * @brief Count the species listed for diagnostic output.
 * @param core_ptr Core handle.
 * @return Size of diagnostics.output.diag_list, or 0 for a null handle.
 */
int catchem_config_get_diag_species_count(void* core_ptr);
/**
 * @brief Read one diag_list species name by index.
 * @param core_ptr Core handle; a null handle writes nothing.
 * @param index    0-based ordinal below catchem_config_get_diag_species_count();
 *                 out of range yields "".
 * @param buffer   Receives the species name (NUL-terminated); may be null.
 * @param max_len  Capacity of @p buffer.
 */
void catchem_config_get_diag_species_at(void* core_ptr, int index, char* buffer, int max_len);
/// diagnostics.output.attributes (feature 013, FR-011): run-level NetCDF global
/// attributes.  Iteration order is the map's key order, so it is deterministic.
/**
 * @brief Count configured run-level NetCDF global attributes.
 * @param core_ptr Core handle.
 * @return Number of diagnostics.output.attributes entries, or 0 for a null handle.
 */
int catchem_config_get_output_attribute_count(void* core_ptr);
/**
 * @brief Read the key of one output attribute by iteration index.
 * @param core_ptr Core handle.
 * @param index    0-based ordinal below catchem_config_get_output_attribute_count();
 *                 out of range yields "".
 * @param buffer   Receives the attribute name; may be null.
 * @param max_len  Capacity of @p buffer.
 */
void catchem_config_get_output_attribute_key_at(void* core_ptr, int index, char* buffer, int max_len);
/**
 * @brief Read the value of one output attribute by iteration index.
 * @param core_ptr Core handle.
 * @param index    0-based ordinal matching catchem_config_get_output_attribute_key_at().
 * @param buffer   Receives the attribute value; may be null.
 * @param max_len  Capacity of @p buffer.
 */
void catchem_config_get_output_attribute_value_at(void* core_ptr, int index, char* buffer, int max_len);
/// Path of the YAML the configuration was loaded from ("" before load).
/**
 * @brief Read the source path of the loaded YAML configuration.
 * @param core_ptr Core handle.
 * @param buffer   Receives the path ("" when no file was loaded); may be null.
 * @param max_len  Capacity of @p buffer.
 */
void catchem_config_get_config_file_path(void* core_ptr, char* buffer, int max_len);
/// Build provenance baked in at configure time (feature 013, FR-011).
/**
 * @brief Read the project version string baked in at configure time.
 * @param buffer  Receives the version (NUL-terminated); may be null.
 * @param max_len Capacity of @p buffer.
 */
void catchem_get_build_version(char* buffer, int max_len);
/**
 * @brief Read the Git commit hash baked in at configure time.
 * @param buffer  Receives the commit identifier (NUL-terminated); may be null.
 * @param max_len Capacity of @p buffer.
 */
void catchem_get_build_commit(char* buffer, int max_len);
/**
 * @brief Check whether a process is activated in configuration.
 * @param core_ptr     Core handle.
 * @param process_name YAML processes/ key (e.g. "dust"); null yields 0.
 * @return 1 when the process exists with activate true, else 0 (also 0 for an
 *         unknown name or null handle).
 */
int catchem_config_get_process_active(void* core_ptr, const char* process_name);
/**
 * @brief Check whether any emission category mappings are configured.
 * @param core_ptr Core handle.
 * @return 1 when emission_mappings is non-empty, else 0.
 */
int catchem_config_has_emission_mapping(void* core_ptr);
/**
 * @brief Count configured emission categories.
 * @param core_ptr Core handle.
 * @return Number of emission_mappings entries, or 0 for a null handle.
 */
int catchem_config_get_emission_category_count(void* core_ptr);
/**
 * @brief Read one emission category name by iteration index.
 * @param core_ptr Core handle; a null handle yields "".
 * @param index    0-based ordinal below catchem_config_get_emission_category_count();
 *                 out of range yields "".
 * @param name_out Receives the category name; may be null.
 * @param max_len  Capacity of @p name_out.
 */
void catchem_config_get_emission_category_name_at(void* core_ptr, int index, char* name_out, int max_len);
/**
 * @brief Check whether an emission category is active.
 * @param core_ptr     Core handle.
 * @param category_name Configured category name; null yields 0.
 * @return 1 when the category exists and is active, else 0.
 */
int catchem_config_is_emission_category_active(void* core_ptr, const char* category_name);
/**
 * @brief Count the fields of one emission category.
 * @param core_ptr      Core handle.
 * @param category_name Configured category name; null yields 0.
 * @return Number of field entries, or 0 when the category is unknown.
 */
int catchem_config_get_emission_field_count(void* core_ptr, const char* category_name);
/**
 * @brief Read one emission field name by index within a category.
 * @param core_ptr      Core handle.
 * @param category_name Configured category name.
 * @param field_idx     0-based ordinal below catchem_config_get_emission_field_count().
 * @param name_out      Receives the field name; may be null.
 * @param max_len       Capacity of @p name_out.
 */
void catchem_config_get_emission_field_name_at(void* core_ptr, const char* category_name, int field_idx, char* name_out,
                                               int max_len);
/**
 * @brief Read the declared units of one emission field.
 * @param core_ptr      Core handle.
 * @param category_name Configured category name.
 * @param field_name    Field name within the category.
 * @param units_out     Receives the units string; may be null.
 * @param max_len       Capacity of @p units_out.
 */
void catchem_config_get_emission_field_units(void* core_ptr, const char* category_name, const char* field_name,
                                             char* units_out, int max_len);
/**
 * @brief Count species mapping entries for one emission field.
 * @param core_ptr      Core handle.
 * @param category_name Configured category name.
 * @param field_name    Field name within the category.
 * @return Number of target-species entries, or 0 when the field is unknown.
 */
int catchem_config_get_emission_species_map_count(void* core_ptr, const char* category_name, const char* field_name);
/**
 * @brief Read one species mapping entry of an emission field.
 * @param core_ptr          Core handle.
 * @param category_name     Configured category name.
 * @param field_name        Field name within the category.
 * @param map_idx           0-based ordinal below
 *                          catchem_config_get_emission_species_map_count().
 * @param target_species_out Receives the target species name ("" when no entry).
 * @param max_len           Capacity of @p target_species_out.
 * @param scale_out         Receives the emission scale factor (1.0 when absent); optional.
 * @param species_idx_out   Receives the 1-based runtime species index, or -1 when
 *                          the target is not in the mechanism; optional.
 */
void catchem_config_get_emission_species_map_at(void* core_ptr, const char* category_name, const char* field_name,
                                                int map_idx, char* target_species_out, int max_len, double* scale_out,
                                                int* species_idx_out);

/**
 * @brief Safely queries a boolean configuration setting by path.
 * @param core_ptr Pointer to catchem::Core instance.
 * @param yaml_path Slash-separated path in config (e.g. "processes/extemis/activate").
 * @param default_val Fallback value if key is missing or incompatible.
 * @return 1 for true, 0 for false, or default_val.
 */
int catchem_config_get_yaml_bool(void* core_ptr, const char* yaml_path, int default_val);

/**
 * @brief Safely queries a double configuration setting by path.
 * @param core_ptr Pointer to catchem::Core instance.
 * @param yaml_path Slash-separated path in config.
 * @param default_val Fallback value if key is missing.
 * @return Double configuration value or default_val.
 */
double catchem_config_get_yaml_double(void* core_ptr, const char* yaml_path, double default_val);

/**
 * @brief Safely queries an integer configuration setting by path.
 * @param core_ptr Pointer to catchem::Core instance.
 * @param yaml_path Slash-separated path in config.
 * @param default_val Fallback value if key is missing.
 * @return Integer configuration value or default_val.
 */
int catchem_config_get_yaml_int(void* core_ptr, const char* yaml_path, int default_val);

/**
 * @brief Safely queries a string configuration setting by path, resolving relative file paths.
 * @param core_ptr Pointer to catchem::Core instance.
 * @param yaml_path Slash-separated path in config.
 * @param val_out Output buffer for null-terminated string.
 * @param max_len Maximum length of output buffer.
 * @param default_val Fallback string if key is missing.
 */
void catchem_config_get_yaml_string(void* core_ptr, const char* yaml_path, char* val_out, int max_len,
                                    const char* default_val);

/**
 * @brief Locates static FENGSHA/dust input file path via ConfigManager process settings.
 * @param core_ptr Pointer to catchem::Core instance.
 * @param val_out Output buffer for null-terminated string.
 * @param max_len Maximum length of output buffer.
 */
void catchem_config_find_fengsha_static_file(void* core_ptr, char* val_out, int max_len);

/**
 * @brief Returns sequence length for a list configuration path.
 * @param core_ptr Pointer to catchem::Core instance.
 * @param yaml_path Slash-separated path in config.
 * @return Number of elements in sequence, or 0.
 */
int catchem_config_get_yaml_list_count(void* core_ptr, const char* yaml_path);

/**
 * @brief Retrieves a string element at index from a list configuration path.
 * @param core_ptr Pointer to catchem::Core instance.
 * @param yaml_path Slash-separated path in config.
 * @param index 0-based element index.
 * @param val_out Output buffer for string element.
 * @param max_len Maximum length of output buffer.
 */
void catchem_config_get_yaml_list_at(void* core_ptr, const char* yaml_path, int index, char* val_out, int max_len);

/**
 * @brief Enable or disable echoing the parsed YAML to stdout on configuration load.
 * @param enabled Non-zero to echo (the default), zero to suppress.
 *
 * The NUOPC driver calls this with zero on non-root PETs so the effective
 * configuration appears once per coupled run instead of once per PET.
 */
void catchem_set_config_echo_enabled(int enabled);

// Diagnostic API
/**
 * @brief Register an output diagnostic field (legacy form).
 *
 * @details Wrapper over catchem_diag_register_checked(); failures are silent.
 * @param core_ptr Core handle.
 * @param name     Diagnostic field name (unique; re-registering replaces).
 * @param desc     Human-readable description written to NetCDF.
 * @param units    CF-style units string written to NetCDF.
 * @param rank     2 or 3; other ranks are rejected by the checked form.
 * @param dim1,dim2,dim3 Extent per dimension; ignored for rank 2.
 */
void catchem_diag_register(void* core_ptr, const char* name, const char* desc, const char* units, int rank, int dim1,
                           int dim2, int dim3);
/**
 * @brief Register an output diagnostic field with status reporting.
 * @param core_ptr Core handle.
 * @param name     Diagnostic field name; must be non-null.
 * @param desc     Description string; must be non-null.
 * @param units    Units string; must be non-null.
 * @param rank     2 (dims dim1,dim2) or 3 (dims dim1,dim2,dim3); anything else
 *                 returns CATCHEM_RANK_MISMATCH.
 * @param dim1,dim2,dim3 Extent per dimension.
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, CATCHEM_RANK_MISMATCH,
 *         CATCHEM_INVALID_STATE (duplicate/conflicting registration), or the
 *         handle codes of catchem_core_begin_interval_checked().
 */
int catchem_diag_register_checked(void* core_ptr, const char* name, const char* desc, const char* units, int rank,
                                  int dim1, int dim2, int dim3);
/**
 * @brief Register a diagnostic with a semantic axis contract and reset policy.
 * @param core_ptr    Core handle.
 * @param name        Diagnostic field name; must be non-null.
 * @param desc        Description string; must be non-null.
 * @param units       Units string; must be non-null.
 * @param rank        2 or 3; anything else returns CATCHEM_RANK_MISMATCH.
 * @param dims        Array of @p rank extents; must be non-null.
 * @param axes        Array of @p rank SemanticAxis ordinals (0 Column, 1 Level,
 *                    2 Interface, 3 SoilLayer, 4 Species, 5 Category, 6 Singleton);
 *                    out-of-range ordinals return CATCHEM_INVALID_STATE.
 * @param policy      DiagnosticPolicy ordinal: 0 Instantaneous,
 *                    1 TimestepAccumulated, 2 Persistent.
 * @param reset_value Value applied to the field when its policy resets it
 *                    (typically 0.0 or a fill value).
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, CATCHEM_RANK_MISMATCH,
 *         CATCHEM_INVALID_STATE, or the handle codes of
 *         catchem_core_begin_interval_checked().
 * @note This entry point carries no per-slot labels, so a packed Species or
 *       Category axis cannot satisfy the unpacking contract here; output-time
 *       unpacking of such fields fails loud.
 */
int catchem_diag_register_contract_checked(void* core_ptr, const char* name, const char* desc, const char* units,
                                           int rank, const int* dims, const int* axes, int policy, double reset_value);
/**
 * @brief Read a diagnostic field's freshness contract.
 * @param core_ptr      Core handle.
 * @param name          Field name; must be non-null.
 * @param generation    Receives the state import generation the data belongs to.
 * @param availability  Receives the AvailabilityState ordinal: 0 Unavailable,
 *                      1 Current, 2 Stale (catchem_field_contract.hpp).
 * @param latest_writer Receives the LatestWriter ordinal: 0 Uninitialized,
 *                      1 HostCurrent, 2 DeviceCurrent, 3 Synchronized.
 * @param policy        Receives the DiagnosticPolicy ordinal (see
 *                      catchem_diag_register_contract_checked()).
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, CATCHEM_MISSING_FIELD (not
 *         registered), or the handle codes of catchem_core_begin_interval_checked().
 */
int catchem_diag_get_contract(void* core_ptr, const char* name, int* generation, int* availability, int* latest_writer,
                              int* policy);
/**
 * @brief Borrow the mutable host buffer of a diagnostic field (legacy form).
 *
 * @details Returns the writable host pointer without performing any device
 * sync; the caller is responsible for marking the field host-modified
 * (catchem_diag_mark_host_modified()) if a device mirror exists. Returns
 * nullptr for an invalid handle, null name, or unknown field.
 * @param core_ptr Core handle.
 * @param name     Field name.
 * @return Borrowed mutable host pointer, or nullptr.
 */
void* catchem_diag_get_pointer(void* core_ptr, const char* name);
/**
 * @brief Read a diagnostic field's rank (legacy form).
 * @param core_ptr Core handle.
 * @param name     Field name.
 * @return 2 or 3 for a registered field, or 0 on any failure.
 */
int catchem_diag_get_rank(void* core_ptr, const char* name);
/**
 * @brief Checked form of catchem_diag_get_rank().
 * @param core_ptr Core handle.
 * @param name     Field name.
 * @param rank_out Receives the rank; required.
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, CATCHEM_MISSING_FIELD, or the
 *         handle codes of catchem_core_begin_interval_checked().
 */
int catchem_diag_get_rank_checked(void* core_ptr, const char* name, int* rank_out);
/**
 * @brief Read a diagnostic field's extents (legacy form).
 * @param core_ptr Core handle.
 * @param name     Field name.
 * @param dims_out Buffer of at least 3 ints receiving the extents (unused
 *                 entries 0); may be null.
 */
void catchem_diag_get_dims(void* core_ptr, const char* name, int* dims_out);
/**
 * @brief Read a diagnostic field's extents with status reporting.
 * @param core_ptr     Core handle.
 * @param name         Field name.
 * @param dims_out     Buffer receiving extents, zero-filled first; required.
 * @param dims_length  Capacity of @p dims_out; must be >= the field rank.
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, CATCHEM_MISSING_FIELD,
 *         CATCHEM_EXTENT_MISMATCH (buffer shorter than rank), or the handle
 *         codes of catchem_core_begin_interval_checked().
 */
int catchem_diag_get_dims_checked(void* core_ptr, const char* name, int* dims_out, int dims_length);
/**
 * @brief Borrow a diagnostic's host read pointer with a full shape check.
 * @param core_ptr Core handle.
 * @param name     Field name.
 * @param rank     Expected rank; must match the registered field.
 * @param dims     Expected extents, @p rank entries; each must match.
 * @param ptr_out  Receives the borrowed pointer, or nullptr; required.
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, CATCHEM_MISSING_FIELD,
 *         CATCHEM_RANK_MISMATCH, CATCHEM_EXTENT_MISMATCH, CATCHEM_INVALID_STATE
 *         (field has no host buffer), or the handle codes of
 *         catchem_core_begin_interval_checked().
 */
int catchem_diag_get_pointer_checked(void* core_ptr, const char* name, int rank, const int* dims, void** ptr_out);
/**
 * @brief Mark a diagnostic host-modified so the next sync pushes to device.
 * @param core_ptr Core handle; null is rejected.
 * @param name     Field name; null is rejected.
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, CATCHEM_MISSING_FIELD (not
 *         registered), or CATCHEM_INTERNAL_ERROR.
 */
int catchem_diag_mark_host_modified(void* core_ptr, const char* name);
/**
 * @brief Mark a diagnostic device-modified so the next sync pulls to host.
 * @param core_ptr Core handle; null is rejected.
 * @param name     Field name; null is rejected.
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, CATCHEM_MISSING_FIELD, or
 *         CATCHEM_INTERNAL_ERROR.
 */
int catchem_diag_mark_device_modified(void* core_ptr, const char* name);
/**
 * @brief Pull all device-modified diagnostics into their host buffers.
 * @param core_ptr Core handle.
 */
void catchem_diag_sync_to_host(void* core_ptr);
/**
 * @brief Reset every diagnostic according to its policy.
 *
 * @details Instantaneous and TimestepAccumulated fields return to their reset
 * values; Persistent fields keep their contents (DiagnosticManager::reset_all).
 * @param core_ptr Core handle.
 */
void catchem_diag_reset(void* core_ptr);
/**
 * @brief Count registered diagnostic fields (legacy form).
 * @param core_ptr Core handle.
 * @return Field count, or 0 on failure.
 */
int catchem_diag_get_count(void* core_ptr);
/**
 * @brief Checked form of catchem_diag_get_count().
 * @param core_ptr  Core handle.
 * @param count_out Receives the registered field count; required.
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, or the handle codes of
 *         catchem_core_begin_interval_checked().
 */
int catchem_diag_get_count_checked(void* core_ptr, int* count_out);
/**
 * @brief Read a diagnostic field name by index (legacy form).
 * @param core_ptr Core handle.
 * @param index    0-based ordinal below catchem_diag_get_count().
 * @param name_out Buffer of at least 64 characters receiving the name.
 */
void catchem_diag_get_name_at(void* core_ptr, int index, char* name_out);
/**
 * @brief Read a diagnostic field name by index with status reporting.
 * @param core_ptr    Core handle.
 * @param index       0-based ordinal below the registered field count; out of
 *                     range yields CATCHEM_INVALID_INDEX.
 * @param name_out    Buffer receiving the name (cleared first); required.
 * @param name_length Capacity of @p name_out; must be positive.
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, CATCHEM_INVALID_INDEX, or the
 *         handle codes of catchem_core_begin_interval_checked().
 */
int catchem_diag_get_name_at_checked(void* core_ptr, int index, char* name_out, int name_length);
/**
 * @brief Read a diagnostic field's units string.
 * @param core_ptr     Core handle.
 * @param name         Field name.
 * @param units_out    Buffer receiving the units (cleared first); required.
 * @param units_length Capacity of @p units_out; must be positive.
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, CATCHEM_MISSING_FIELD, or the
 *         handle codes of catchem_core_begin_interval_checked().
 */
int catchem_diag_get_units_checked(void* core_ptr, const char* name, char* units_out, int units_length);
/**
 * @brief Read a diagnostic field's description string.
 * @param core_ptr   Core handle.
 * @param name       Field name.
 * @param desc_out   Buffer receiving the description (cleared first); required.
 * @param desc_length Capacity of @p desc_out; must be positive.
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, CATCHEM_MISSING_FIELD, or the
 *         handle codes of catchem_core_begin_interval_checked().
 */
int catchem_diag_get_description_checked(void* core_ptr, const char* name, char* desc_out, int desc_length);
/// SemanticAxis ordinals per dimension (length == rank). axes_length must be >= rank.
/**
 * @brief Read a diagnostic field's per-dimension semantic axes.
 * @param core_ptr    Core handle.
 * @param name        Field name.
 * @param axes_out    Buffer receiving SemanticAxis ordinals, zero-filled to
 *                     @p axes_length first; required.
 * @param axes_length Capacity of @p axes_out; must be >= the field rank.
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, CATCHEM_MISSING_FIELD,
 *         CATCHEM_EXTENT_MISMATCH (buffer shorter than rank), or the handle
 *         codes of catchem_core_begin_interval_checked().
 */
int catchem_diag_get_axes_checked(void* core_ptr, const char* name, int* axes_out, int axes_length);
/// Label for slot `slot` (0-based) of the field's packed (Species/Category) dimension.
/// Non-zero when the field has no packed dimension or `slot` is out of range.
/**
 * @brief Read one per-slot label of a diagnostic's packed dimension.
 * @param core_ptr    Core handle.
 * @param name        Field name.
 * @param slot        0-based index into the packed Species/Category dimension.
 * @param label_out   Buffer receiving the label (cleared first); required.
 * @param label_length Capacity of @p label_out; must be positive.
 * @return CATCHEM_SUCCESS, or non-zero (CATCHEM_NULL_ARGUMENT,
 *         CATCHEM_MISSING_FIELD, CATCHEM_INVALID_INDEX) when the field has no
 *         packed labels or @p slot is out of range.
 */
int catchem_diag_get_unpack_label_at_checked(void* core_ptr, const char* name, int slot, char* label_out,
                                             int label_length);

// YAML Species Metadata
/**
 * @brief Load species metadata from a YAML file into the state.
 *
 * @details Legacy wrapper: replaces the runtime species list, name index, and
 * gas/aerosol category lists from the file. Failures print to stderr and leave
 * the previous metadata in place.
 * @param state_ptr StateManager handle.
 * @param filename  Path to the species YAML definition.
 */
void catchem_state_load_species_config(void* state_ptr, const char* filename);
/**
 * @brief Count species in the runtime mechanism (legacy form).
 * @param state_ptr StateManager handle.
 * @return Species count; dereferences a null handle (use the checked form there).
 */
int catchem_state_get_species_count(void* state_ptr);
/**
 * @brief Checked form of catchem_state_get_species_count().
 * @param state_ptr StateManager handle.
 * @param count_out Receives the species count; required.
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, or the handle codes of
 *         catchem_core_begin_interval_checked().
 */
int catchem_state_get_species_count_checked(void* state_ptr, int* count_out);
/**
 * @brief Look up a species' 1-based index by name (legacy form).
 * @param state_ptr StateManager handle; null returns -1.
 * @param name      Species name; case-insensitive (uppercased before lookup).
 * @return 1-based index matching the Fortran convention, or -1 when not found.
 */
int catchem_state_get_species_index(void* state_ptr,
                                    const char* name); // returns 1-based index matching Fortran, or -1 if not found
/**
 * @brief Checked form of catchem_state_get_species_index().
 * @param state_ptr  StateManager handle.
 * @param name       Species name; case-insensitive.
 * @param index_out  Receives the 1-based index, or 0 on failure; required.
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, CATCHEM_INVALID_INDEX (not
 *         in the mechanism), or the handle codes of
 *         catchem_core_begin_interval_checked().
 */
int catchem_state_get_species_index_checked(void* state_ptr, const char* name, int* index_out);

// Categorized counts and list getters
/**
 * @brief Count gas-phase species in the runtime mechanism.
 * @param state_ptr StateManager handle.
 * @return Number of species flagged gas-phase.
 */
int catchem_state_get_gas_species_count(void* state_ptr);
/**
 * @brief List the 1-based indices of gas-phase species.
 * @param state_ptr   StateManager handle.
 * @param indices_out Buffer of at least catchem_state_get_gas_species_count()
 *                    entries receiving 1-based species indices (Fortran order).
 */
void catchem_state_get_gas_indices(void* state_ptr, int* indices_out); // populates 1-based indices
/**
 * @brief Count aerosol species in the runtime mechanism.
 * @param state_ptr StateManager handle.
 * @return Number of species flagged aerosol.
 */
int catchem_state_get_aerosol_species_count(void* state_ptr);
/**
 * @brief List the 1-based indices of aerosol species.
 * @param state_ptr   StateManager handle.
 * @param indices_out Buffer of at least catchem_state_get_aerosol_species_count()
 *                    entries receiving 1-based species indices (Fortran order).
 */
void catchem_state_get_aerosol_indices(void* state_ptr, int* indices_out);

// Individual property getters (by 1-based index)
/**
 * @brief Read a species' molecular weight (legacy form).
 * @param state_ptr StateManager handle.
 * @param index     1-based species index (Fortran convention).
 * @return Molecular weight [g/mol], or 0.0 for an out-of-range index.
 */
double catchem_state_get_species_mw(void* state_ptr, int index);
/**
 * @brief Checked form of catchem_state_get_species_mw().
 * @param state_ptr            StateManager handle.
 * @param index                1-based species index.
 * @param molecular_weight_out Receives the molecular weight [g/mol]; required.
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, CATCHEM_INVALID_INDEX, or the
 *         handle codes of catchem_core_begin_interval_checked().
 */
int catchem_state_get_species_mw_checked(void* state_ptr, int index, double* molecular_weight_out);
/**
 * @brief Check whether a species is gas-phase (legacy form).
 * @param state_ptr StateManager handle.
 * @param index     1-based species index.
 * @return 1 when gas-phase, 0 otherwise or for an out-of-range index.
 */
int catchem_state_is_species_gas(void* state_ptr, int index);
/**
 * @brief Checked form of catchem_state_is_species_gas().
 * @param state_ptr StateManager handle.
 * @param index     1-based species index.
 * @param value_out Receives 1/0 (zeroed first); required.
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, CATCHEM_INVALID_INDEX, or the
 *         handle codes of catchem_core_begin_interval_checked().
 */
int catchem_state_is_species_gas_checked(void* state_ptr, int index, int* value_out);
/**
 * @brief Check whether a species is an aerosol (legacy form).
 * @param state_ptr StateManager handle.
 * @param index     1-based species index.
 * @return 1 when aerosol, 0 otherwise or for an out-of-range index.
 */
int catchem_state_is_species_aerosol(void* state_ptr, int index);
/**
 * @brief Checked form of catchem_state_is_species_aerosol().
 * @param state_ptr StateManager handle.
 * @param index     1-based species index.
 * @param value_out Receives 1/0 (zeroed first); required.
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, CATCHEM_INVALID_INDEX, or the
 *         handle codes of catchem_core_begin_interval_checked().
 */
int catchem_state_is_species_aerosol_checked(void* state_ptr, int index, int* value_out);
/**
 * @brief Read a species' short name (legacy form).
 * @param state_ptr StateManager handle.
 * @param index     1-based species index.
 * @param name_out  Buffer of at least 64 characters receiving the short name
 *                  ("" for an out-of-range index).
 */
void catchem_state_get_species_name_at(void* state_ptr, int index, char* name_out);
/**
 * @brief Checked form of catchem_state_get_species_name_at().
 * @param state_ptr   StateManager handle.
 * @param index       1-based species index.
 * @param name_out    Buffer receiving the short name (cleared first); required.
 * @param name_length Capacity of @p name_out; must be positive.
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, CATCHEM_INVALID_INDEX, or the
 *         handle codes of catchem_core_begin_interval_checked().
 */
int catchem_state_get_species_name_at_checked(void* state_ptr, int index, char* name_out, int name_length);
/**
 * @brief Check whether a species is advected by transport.
 * @param state_ptr StateManager handle.
 * @param index     1-based species index.
 * @param value_out Receives 1/0 (zeroed first); required.
 * @return CATCHEM_SUCCESS, CATCHEM_NULL_ARGUMENT, CATCHEM_INVALID_INDEX, or the
 *         handle codes of catchem_core_begin_interval_checked().
 */
int catchem_state_get_species_is_advected_checked(void* state_ptr, int index, int* value_out);
/**
 * @brief Read a species' long (display) name (legacy form).
 * @param state_ptr StateManager handle.
 * @param index     1-based species index.
 * @param name_out  Buffer of at least 64 characters receiving the long name.
 */
void catchem_state_get_species_long_name_at(void* state_ptr, int index, char* name_out);
/**
 * @brief Read a species' description text (legacy form).
 * @param state_ptr StateManager handle.
 * @param index     1-based species index.
 * @param desc_out  Buffer of at least 64 characters receiving the description.
 */
void catchem_state_get_species_desc_at(void* state_ptr, int index, char* desc_out);
/**
 * @brief Read the Mie-optics table name for an aerosol species (legacy form).
 * @param state_ptr StateManager handle.
 * @param index     1-based species index.
 * @param mie_out   Buffer of at least 64 characters receiving the Mie name (""
 *                  for non-aerosol species or an out-of-range index).
 */
void catchem_state_get_species_mie_name(void* state_ptr, int index, char* mie_out);

// Physics derivations
/**
 * @brief Compute the boundary-layer height field from bound met (legacy form).
 *
 * @details Wrapper over catchem_state_derive_bxheight_checked(); failures are
 * silent. Reads the bound met fields and writes the derived bxheight field.
 * @param state_ptr StateManager handle.
 */
void catchem_state_derive_bxheight(void* state_ptr);
/**
 * @brief Derive the boundary-layer height with status reporting.
 * @param state_ptr StateManager handle.
 * @return CATCHEM_SUCCESS, CATCHEM_PHYSICAL_VALIDATION_FAILURE (inputs outside
 *         the physical validity range — the field keeps its previous values),
 *         CATCHEM_INTERNAL_ERROR, or the handle codes of
 *         catchem_core_begin_interval_checked().
 */
int catchem_state_derive_bxheight_checked(void* state_ptr);
/**
 * @brief Compute the dry air density field from bound met (legacy form).
 *
 * @details Wrapper over catchem_state_derive_airden_dry_checked(); failures
 * are silent.
 * @param state_ptr StateManager handle.
 */
void catchem_state_derive_airden_dry(void* state_ptr);
/**
 * @brief Derive dry air density with status reporting.
 * @param state_ptr StateManager handle.
 * @return CATCHEM_SUCCESS, CATCHEM_PHYSICAL_VALIDATION_FAILURE (inputs outside
 *         the physical validity range), CATCHEM_INTERNAL_ERROR, or the handle
 *         codes of catchem_core_begin_interval_checked().
 */
int catchem_state_derive_airden_dry_checked(void* state_ptr);

// TimeState C-Linkable API
// Thin extern "C" wrappers over catchem::TimeState (catchem_time_state.hpp).
// Every wrapper is exception-safe: entry failures return 0 / -1 / 0.0 rather
// than propagating a C++ exception across the C boundary.
/**
 * @brief Allocate a standalone TimeState (independent of any Core handle).
 * @return Opaque pointer to the new TimeState, or nullptr if allocation failed.
 * @see catchem_time_state_destroy()
 */
void* catchem_time_state_create();
/**
 * @brief Release a TimeState created by catchem_time_state_create().
 * @param ptr TimeState pointer; null is a safe no-op. The pointer is invalid after.
 */
void catchem_time_state_destroy(void* ptr);
/**
 * @brief Set the calendar fields and recompute derived quantities.
 * @param ptr      TimeState pointer.
 * @param year     Calendar year.
 * @param month    Month (1-12).
 * @param day      Day of month.
 * @param hour     Hour (0-23).
 * @param minute   Minute (0-59).
 * @param second   Second (0-59).
 * @param timestep Nominal timestep [s] used by mid-timestep astronomy.
 * @return 0 on success, -1 on failure (e.g. null pointer).
 */
int catchem_time_state_init(void* ptr, int year, int month, int day, int hour, int minute, int second, double timestep);
/**
 * @brief Advance the clock by @p dt seconds, rolling over date fields.
 * @param ptr TimeState pointer.
 * @param dt  Elapsed time [s].
 * @return 0 on success, -1 on failure.
 */
int catchem_time_state_advance(void* ptr, double dt);
/**
 * @brief Reset the clock to 2000-01-01 00:00:00 with a 3600 s timestep.
 * @param ptr TimeState pointer.
 * @return 0 on success, -1 on failure.
 */
int catchem_time_state_reset(void* ptr);
/**
 * @brief Current calendar year.
 * @param ptr TimeState pointer.
 * @return Year, or 0 on failure.
 */
int catchem_time_state_get_year(void* ptr);
/**
 * @brief Current month.
 * @param ptr TimeState pointer.
 * @return Month (1-12), or 0 on failure.
 */
int catchem_time_state_get_month(void* ptr);
/**
 * @brief Current day of month.
 * @param ptr TimeState pointer.
 * @return Day, or 0 on failure.
 */
int catchem_time_state_get_day(void* ptr);
/**
 * @brief Current hour of day.
 * @param ptr TimeState pointer.
 * @return Hour (0-23), or 0 on failure.
 */
int catchem_time_state_get_hour(void* ptr);
/**
 * @brief Current minute of hour.
 * @param ptr TimeState pointer.
 * @return Minute (0-59), or 0 on failure.
 */
int catchem_time_state_get_minute(void* ptr);
/**
 * @brief Current second of minute.
 * @param ptr TimeState pointer.
 * @return Second (0-59), or 0 on failure.
 */
int catchem_time_state_get_second(void* ptr);
/**
 * @brief Current nominal timestep length.
 * @param ptr TimeState pointer.
 * @return Timestep [s], or 0.0 on failure.
 */
double catchem_time_state_get_timestep(void* ptr);
/**
 * @brief Julian date of the current clock.
 * @param ptr TimeState pointer.
 * @return Julian date [days], or 0.0 on failure.
 */
double catchem_time_state_get_julian_date(void* ptr);
/**
 * @brief Day of year of the current clock.
 * @param ptr TimeState pointer.
 * @return Day of year (1-366), or 0 on failure.
 */
int catchem_time_state_get_doy(void* ptr);
/**
 * @brief Cosine of the solar zenith angle at a location.
 * @param ptr          TimeState pointer.
 * @param lat          Latitude [degrees north].
 * @param lon          Longitude [degrees east].
 * @param mid_timestep true evaluates at the timestep midpoint, false at its start.
 * @return cos(SZA), clamped to the physical range; 0.0 on failure or night.
 */
double catchem_time_state_get_cos_sza(void* ptr, double lat, double lon, bool mid_timestep);
/**
 * @brief Standard timezone offset for a longitude.
 * @param ptr Unused; accepted for API symmetry.
 * @param lon Longitude [degrees east]; offset = trunc(lon/15) clamped to [-12, 14].
 * @return Hour offset from UTC (0 on failure).
 */
int catchem_time_state_get_timezone_offset(void* ptr, double lon);
/**
 * @brief Leap-year test (Gregory calendar).
 * @param year Calendar year.
 * @return true when @p year has 366 days.
 */
bool catchem_time_state_is_leap_year(int year);
/**
 * @brief Days in a month of a given year.
 * @param month Month (1-12).
 * @param year  Calendar year (leap rules apply to February).
 * @return Day count, or 0 for an out-of-range month.
 */
int catchem_time_state_get_days_in_month(int month, int year);
/**
 * @brief Global holiday test (Jan 1 and Dec 25).
 * @param month Month (1-12).
 * @param day   Day of month.
 * @return true when the date is a global holiday.
 */
bool catchem_time_state_is_global_holiday(int month, int day);
/**
 * @brief US holiday test (Jul 4 and Nov 22-28).
 * @param month Month (1-12).
 * @param day   Day of month.
 * @return true when the date is a US holiday.
 */
bool catchem_time_state_is_us_holiday(int month, int day);

// UnitConversion C-Linkable API
// Wrappers over catchem::unit_conversion (catchem_unit_conversion.hpp). On an
// unknown unit pair or internal failure each sets *rc = -1 and returns the
// input value unchanged, so callers must check *rc even when the result looks
// plausible.
/**
 * @brief Convert a concentration between volume-mixing and mass/volume units.
 * @param val       Input value in @p from_units.
 * @param from_units Source unit, e.g. "PPM"/"PPMV", "PPB"/"PPBV", "PPT",
 *                   "UG/M3", "MG/M3", "MOLEC/CM3" (case-insensitive; aliases
 *                   "UG M-3", "UG/M^3" etc. accepted).
 * @param to_units  Destination unit from the same vocabulary.
 * @param mw        Species molecular weight [g/mol] for mass-unit pairs.
 * @param temp      Temperature [K] for ideal-gas molar volume.
 * @param press     Pressure [Pa] for ideal-gas molar volume.
 * @param rc        Receives 0 on success or -1 when the pair is unsupported.
 * @return Converted value, or @p val unchanged when unsupported.
 */
double catchem_convert_concentration(double val, const char* from_units, const char* to_units, double mw, double temp,
                                     double press, int* rc);
/**
 * @brief Convert a pressure between supported units (e.g. "PA", "HPA", "MB", "ATM").
 * @param val       Input pressure in @p from_units.
 * @param from_units Source unit name (case-insensitive).
 * @param to_units  Destination unit name.
 * @param rc        Receives 0 on success or -1 when the pair is unsupported.
 * @return Converted pressure, or @p val unchanged when unsupported.
 */
double catchem_convert_pressure(double val, const char* from_units, const char* to_units, int* rc);
/**
 * @brief Convert a temperature between supported units (e.g. "K", "C", "F").
 * @param val       Input temperature in @p from_units.
 * @param from_units Source unit name (case-insensitive).
 * @param to_units  Destination unit name.
 * @param rc        Receives 0 on success or -1 when the pair is unsupported.
 * @return Converted temperature, or @p val unchanged when unsupported.
 */
double catchem_convert_temperature(double val, const char* from_units, const char* to_units, int* rc);
/**
 * @brief Convert a surface flux between supported units.
 * @param val       Input flux in @p from_units.
 * @param from_units Source unit name (case-insensitive).
 * @param to_units  Destination unit name.
 * @param mw        Species molecular weight [g/mol] when mass basis changes.
 * @param rc        Receives 0 on success or -1 when the pair is unsupported.
 * @return Converted flux, or @p val unchanged when unsupported.
 */
double catchem_convert_flux(double val, const char* from_units, const char* to_units, double mw, int* rc);
/**
 * @brief Convert a reaction rate constant between supported unit pairs.
 * @param val       Input rate constant in @p from_units.
 * @param from_units Source unit name (case-insensitive).
 * @param to_units  Destination unit name.
 * @param rc        Receives 0 on success or -1 when the pair is unsupported.
 * @return Converted rate constant, or @p val unchanged when unsupported.
 */
double catchem_convert_rate_constant(double val, const char* from_units, const char* to_units, int* rc);
/**
 * @brief Convert a mass between supported units (e.g. "KG", "G", "MG", "UG").
 * @param val       Input mass in @p from_units.
 * @param from_units Source unit name (case-insensitive).
 * @param to_units  Destination unit name.
 * @param rc        Receives 0 on success or -1 when the pair is unsupported.
 * @return Converted mass, or @p val unchanged when unsupported.
 */
double catchem_convert_mass_units(double val, const char* from_units, const char* to_units, int* rc);
/**
 * @brief Dry or virtual air density from temperature and pressure.
 * @param temp         Air temperature [K].
 * @param press        Air pressure [Pa].
 * @param humidity     Relative humidity (0-1) or mixing ratio, per the
 *                     catchem::unit_conversion convention; ignored when
 *                     @p use_humidity is false.
 * @param use_humidity When true, correct the density for water vapor.
 * @return Density [kg/m3], or 0.0 on failure.
 */
double catchem_calculate_air_density(double temp, double press, double humidity, bool use_humidity);
/**
 * @brief Molecular weight from a chemical formula string.
 * @param formula Hill-style formula, e.g. "SO2", "O3", "C7H16".
 * @return Molecular weight [g/mol], or 0.0 for an unparseable formula.
 */
double catchem_calculate_molecular_weight(const char* formula);
/**
 * @brief Convert between imperial/customary units within a category.
 * @param val       Input value in @p from_units.
 * @param from_units Source unit name.
 * @param to_units  Destination unit name.
 * @param category  Unit family, e.g. "length", "pressure", "speed"; selects the
 *                   conversion table.
 * @param rc        Receives 0 on success or -1 when the pair/category is unsupported.
 * @return Converted value, or @p val unchanged on failure.
 */
double catchem_convert_imperial(double val, const char* from_units, const char* to_units, const char* category,
                                int* rc);
/**
 * @brief In-place array form of catchem_convert_concentration().
 * @param values     Buffer of concentrations converted element by element; owned
 *                   by the caller and modified in place.
 * @param size       Number of elements in @p values.
 * @param from_units Source unit (shared by all elements).
 * @param to_units   Destination unit.
 * @param mw         Molecular weight [g/mol] for mass-unit pairs.
 * @param temp       Temperature [K].
 * @param press      Pressure [Pa].
 * @return 0 on success; on the first unsupported pair, conversion stops and
 *         that element's rc (-1) is returned — earlier elements stay converted.
 */
int catchem_convert_process_concentration_units(catchem::fp* values, int size, const char* from_units,
                                                const char* to_units, catchem::fp mw, catchem::fp temp,
                                                catchem::fp press);
/**
 * @brief In-place array form of catchem_convert_flux().
 * @param values     Buffer of fluxes converted element by element; caller-owned.
 * @param size       Number of elements in @p values.
 * @param from_units Source unit.
 * @param to_units   Destination unit.
 * @param mw         Molecular weight [g/mol] when mass basis changes.
 * @return 0 on success; -1 from the first failing element (earlier elements
 *         stay converted).
 */
int catchem_convert_process_flux_units(catchem::fp* values, int size, const char* from_units, const char* to_units,
                                       catchem::fp mw);

// =========================================================================
// Species Metadata and Property Query C-API
// =========================================================================
// The accessors below are generated by an X-macro: the three property macros
// are defined, catchem_species_properties.def is included (each entry expands
// to one declaration here and one definition in catchem_api_config.cpp), then
// the macros are undefined. See the header comment of
// catchem_species_properties.def for the entry format and the generated names:
//   CATCHEM_SPECIES_DOUBLE_PROPERTY(api, m, fb) ->
//       double catchem_state_get_species_api(void* state_ptr, int index);
//   CATCHEM_SPECIES_BOOL_PROPERTY(api, m) ->
//       int catchem_state_get_species_api(void* state_ptr, int index);   // 0/1
//   CATCHEM_SPECIES_LEGACY_BOOL_PROPERTY(api, m) ->
//       int catchem_state_is_species_api(void* state_ptr, int index);    // 0/1
// Each takes a 1-based species index (Fortran convention) and returns the
// entry's fallback value (0.0 / 0) for an out-of-range index or null state.
void catchem_state_get_species_name_at(void* state_ptr, int index, char* name_out);
void catchem_state_get_species_long_name_at(void* state_ptr, int index, char* name_out);
void catchem_state_get_species_desc_at(void* state_ptr, int index, char* desc_out);
/// Expands to a double-returning accessor for one SpeciesMetadata member.
#define CATCHEM_SPECIES_DOUBLE_PROPERTY(api, member, fallback)                                                         \
    double catchem_state_get_species_##api(void* state_ptr, int index);
/// Expands to a 0/1 int-returning accessor for one boolean SpeciesMetadata member.
#define CATCHEM_SPECIES_BOOL_PROPERTY(api, member) int catchem_state_get_species_##api(void* state_ptr, int index);
/// Expands to the older catchem_state_is_species_* spelling of a boolean accessor.
#define CATCHEM_SPECIES_LEGACY_BOOL_PROPERTY(api, member)                                                              \
    int catchem_state_is_species_##api(void* state_ptr, int index);
#include "catchem_species_properties.def"
#undef CATCHEM_SPECIES_LEGACY_BOOL_PROPERTY
#undef CATCHEM_SPECIES_BOOL_PROPERTY
#undef CATCHEM_SPECIES_DOUBLE_PROPERTY

/**
 * @brief Read a species' three wet-deposition rainout-efficiency coefficients.
 * @param state_ptr StateManager handle.
 * @param index     1-based species index (Fortran convention).
 * @param eff_out   Buffer of at least 3 doubles receiving the coefficients;
 *                  zero-filled for an out-of-range index or invalid state.
 */
void catchem_state_get_species_wd_rainouteff(void* state_ptr, int index, double* eff_out);
void catchem_state_get_species_mie_name(void* state_ptr, int index, char* name_out);

// =========================================================================
// Meteorological Core Calculation C-API
// =========================================================================
// Exception-free pass-throughs to catchem::met_utilities (see
// catchem_met_utilities.hpp for the derivation and edge-case notes of each
// kernel). Units below are those of the underlying kernels.
/**
 * @brief Potential temperature of a parcel brought to the reference pressure.
 * @param temp      Air temperature [K].
 * @param press     Ambient pressure [Pa].
 * @param sfc_press Reference (surface) pressure [Pa].
 * @return Potential temperature [K]; unguarded division when press <= 0.
 */
double catchem_met_potential_temperature(double temp, double press, double sfc_press);
/**
 * @brief Density-equivalent temperature of moist air.
 * @param temp Air temperature [K].
 * @param qv   Water vapour mixing ratio [kg kg-1].
 * @return Virtual temperature [K]: temp (1 + 0.61 qv).
 */
double catchem_met_virtual_temperature(double temp, double qv);
/**
 * @brief Dew point from temperature and relative humidity (inverse Magnus).
 * @param temp Air temperature [K].
 * @param rh   Relative humidity as a fraction (0-1), not percent.
 * @return Dew-point temperature [K].
 */
double catchem_met_dew_point(double temp, double rh);
/**
 * @brief Relative humidity from vapour mixing ratio (GOCART-aligned form).
 * @param temp  Air temperature [K].
 * @param qv    Water vapour mixing ratio [kg kg-1].
 * @param press Air pressure [Pa].
 * @return Relative humidity as a fraction, clamped to [0.005, 0.99].
 */
double catchem_met_relative_humidity(double temp, double qv, double press);
/**
 * @brief Saturation vapour pressure over water (Magnus form).
 * @param temp Air temperature [K].
 * @return Saturation vapour pressure [Pa]: 611.2 exp(17.67 (T-273.15)/(T-29.65)).
 */
double catchem_met_saturation_vapor_pressure(double temp);
/**
 * @brief Monin-Obukhov length from surface-layer fluxes.
 * @param ustar Friction velocity [m s-1].
 * @param t0    Surface-layer temperature [K].
 * @param hflux Sensible heat flux [W m-2]; positive upward.
 * @param rho   Air density [kg m-3].
 * @return Obukhov length [m]: -(u*^3 rho CP T0)/(0.41 G0 H); degenerate inputs
 *         return a large-magnitude sentinel rather than dividing by zero.
 */
double catchem_met_monin_obukhov_length(double ustar, double t0, double hflux, double rho);
/**
 * @brief Friction velocity from surface shear stress.
 * @param tau Surface shear stress [Pa]; magnitude used.
 * @param rho Air density [kg m-3].
 * @return u* = sqrt(|tau|/rho) [m s-1]; 0 unless rho > 0.
 */
double catchem_met_friction_velocity(double tau, double rho);
/**
 * @brief Cunningham slip correction for submicron particles.
 * @param dp     Particle diameter [m].
 * @param lambda Air mean free path [m].
 * @return Dimensionless factor 1 + 2 (lambda/dp)(1.257 + 0.4 exp(-1.1 dp/lambda));
 *         exactly 1.0 when lambda is 0.
 */
double catchem_met_cunningham_correction_factor(double dp, double lambda);
/**
 * @brief Kinetic-theory mean free path of air molecules.
 * @param temp Air temperature [K].
 * @param press Air pressure [Pa].
 * @return Mean free path [m]; guarded for non-positive pressure.
 */
double catchem_met_mean_free_path_air(double temp, double press);
/**
 * @brief Solar zenith angle and its cosine for a site and time.
 * @param doy     Day of year (1-366).
 * @param hour    Universal time hour (0-24); local time derived from @p lon_rad.
 * @param lat_rad Site latitude [radians].
 * @param lon_rad Site longitude [radians, east positive].
 * @param sza_deg Receives the zenith angle [degrees], always 0-180.
 * @param cossza  Receives cos(zenith), clamped to [-1, 1]; 0.0/0.0 on failure.
 */
void catchem_met_solar_zenith_angle(int doy, double hour, double lat_rad, double lon_rad, double* sza_deg,
                                    double* cossza);
/**
 * @brief Convert specific humidity to a mixing ratio.
 * @param q Specific humidity [kg kg-1].
 * @return Mixing ratio [kg kg-1]: q / (1 - q). Singular at q = 1; unguarded.
 */
double catchem_met_mixing_ratio(double q);
/**
 * @brief Convert a mixing ratio to specific humidity.
 * @param r Mixing ratio [kg kg-1].
 * @return Specific humidity [kg kg-1]: r / (1 + r).
 */
double catchem_met_specific_humidity(double r);
/**
 * @brief Dry adiabatic lapse rate.
 * @return G0 / CP [K m-1] (~0.00976 K/m) — the lapse rate of a dry adiabat.
 */
double catchem_met_dry_adiabatic_lapse_rate();
/**
 * @brief Bulk Richardson number between the surface and height z.
 * @param t0 Surface temperature [K].
 * @param tz Temperature at height z [K].
 * @param u  Wind speed at height z [m s-1].
 * @param z  Height above surface [m].
 * @return Dimensionless (G0/T0)(Tz-T0)z/u^2; u = 0 yields the stabilizing sentinel.
 */
double catchem_met_bulk_richardson_number(double t0, double tz, double u, double z);
/**
 * @brief Classify surface-layer stability from the Obukhov length.
 * @param l Monin-Obukhov length [m].
 * @return -1 unstable (L < -200 m), +1 stable (L > 200 m), 0 otherwise.
 */
int catchem_met_stability_classification(double l);
/**
 * @brief Saturation mixing ratio over water.
 * @param p Air pressure [Pa].
 * @param t Air temperature [K].
 * @return Mixing ratio [kg kg-1]: 0.622 es / (p - es). Singular when es approaches p.
 */
double catchem_met_saturation_mixing_ratio(double p, double t);
/**
 * @brief Latent heat of vaporization, linear in temperature.
 * @param t Air temperature [K].
 * @return Lv [J kg-1]: 2.501e6 - 2.361e3 (T - 273.15).
 */
double catchem_met_latent_heat_vaporization(double t);
/**
 * @brief Psychrometric constant.
 * @param p  Air pressure [Pa].
 * @param lv Latent heat of vaporization [J kg-1].
 * @return gamma = CP p / (0.622 Lv) [Pa K-1].
 */
double catchem_met_psychrometric_constant(double p, double lv);
/**
 * @brief Neutral logarithmic wind profile.
 * @param ustar Friction velocity [m s-1].
 * @param z     Measurement height [m].
 * @param z0    Roughness length [m].
 * @return Wind speed [m s-1]: (u-star/0.41) ln(z/z0); 0 unless z > z0 > 0.
 */
double catchem_met_wind_profile_loglaw(double ustar, double z, double z0);
/**
 * @brief Squared Brunt-Vaisala frequency for a dry stratified column.
 * @param t0   Reference temperature [K].
 * @param dtdz Environmental temperature gradient [K m-1].
 * @return N^2 [s-2]: (G0/T0)(dTdz + G0/CP); positive means statically stable.
 */
double catchem_met_brunt_vaisala_frequency(double t0, double dtdz);
/**
 * @brief Businger-Dyer profile function for momentum.
 * @param zeta Stability parameter z/L (dimensionless).
 * @return psi_m: 2 ln((1+sqrt(1-16 zeta))/2) when zeta < 0, else -5 zeta.
 */
double catchem_met_psi_m_businger(double zeta);
/**
 * @brief Businger-Dyer profile function for heat.
 * @param zeta Stability parameter z/L (dimensionless).
 * @return psi_h: same functional form as catchem_met_psi_m_businger().
 */
double catchem_met_psi_h_businger(double zeta);
/**
 * @brief Arrhenius temperature dependence of a rate coefficient.
 * @param a  Pre-exponential factor [units of the rate].
 * @param ea Activation energy [J mol-1].
 * @param t  Temperature [K]; 0 yields exp(-inf) = 0.
 * @return A exp(-Ea / (R T)).
 */
double catchem_met_arrhenius_rate(double a, double ea, double t);
/**
 * @brief van' t Hoff temperature scaling of a Henry's law constant.
 * @param h0 Reference solubility at temperature t0.
 * @param dh Enthalpy of dissolution [J mol-1].
 * @param t  Temperature [K]; singular at 0.
 * @param t0 Reference temperature [K].
 * @return H0 exp(-dH/R (1/T - 1/T0)).
 */
double catchem_met_henrys_law_constant(double h0, double dh, double t, double t0);
/**
 * @brief Scale a photolysis frequency by illumination geometry.
 * @param j0 Daytime photolysis rate [s-1].
 * @param sza Solar zenith angle [radians].
 * @return J0 max(0, cos(sza)) [s-1]; zero on the night side.
 */
double catchem_met_photolysis_rate_scaling(double j0, double sza);
/**
 * @brief Convert a volume mixing ratio to mass concentration (ideal gas).
 * @param ppm Mixing ratio [ppm].
 * @param m   Molecular weight [g/mol].
 * @param t   Temperature [K].
 * @param p   Pressure [Pa].
 * @return Mass concentration [ug m-3]: ppm 1e-6 p M / (R T) 1e3.
 */
double catchem_met_ppm_to_ugm3(double ppm, double m, double t, double p);
/**
 * @brief Inverse of catchem_met_ppm_to_ugm3().
 * @param ugm3 Mass concentration [ug m-3].
 * @param m    Molecular weight [g/mol].
 * @param t    Temperature [K].
 * @param p    Pressure [Pa].
 * @return Volume mixing ratio [ppm].
 */
double catchem_met_ugm3_to_ppm(double ugm3, double m, double t, double p);
/**
 * @brief Stokes settling velocity of a small sphere.
 * @param dp    Particle diameter [m].
 * @param rho_p Particle material density [kg m-3].
 * @param rho_a Air density [kg m-3].
 * @param mu    Dynamic viscosity of air [Pa s].
 * @param cc    Cunningham slip correction factor (dimensionless).
 * @return Settling velocity [m s-1]: dp^2 (rho_p - rho_a) G0 Cc / (18 mu).
 */
double catchem_met_stokes_settling_velocity(double dp, double rho_p, double rho_a, double mu, double cc);
/**
 * @brief Stokes number of a particle in a flow over an obstacle scale.
 * @param rho_p Particle material density [kg m-3].
 * @param d_p   Particle diameter [m].
 * @param u     Flow speed [m s-1].
 * @param mu    Dynamic viscosity of air [Pa s].
 * @param l     Obstacle length scale [m].
 * @return Dimensionless St = rho_p d_p^2 U / (18 mu l); 0 unless mu > 0 and l > 0.
 */
double catchem_met_stokes_number(double rho_p, double d_p, double u, double mu, double l);
/**
 * @brief Radioactive decay to a remaining quantity.
 * @param n0     Initial quantity [any count].
 * @param lambda Decay constant [s-1] (consistent with @p t's unit).
 * @param t      Elapsed time [same unit as 1/lambda].
 * @return N0 exp(-lambda t) [same unit as @p n0].
 */
double catchem_met_nuclear_decay(double n0, double lambda, double t);

/**
 * @brief Horizontal column count of the state grid (legacy form).
 * @param state_ptr StateManager handle.
 * @return column_count(), or 0 for a null handle.
 */
int catchem_state_get_nx(void* state_ptr);

/**
 * @brief Grid y dimension of the state (legacy form).
 * @param state_ptr StateManager handle.
 * @return ny from the state geometry, or 0 for a null handle.
 */
int catchem_state_get_ny(void* state_ptr);

/**
 * @brief Vertical level count of the state grid (legacy form).
 * @param state_ptr StateManager handle.
 * @return level_count(), or 0 for a null handle.
 */
int catchem_state_get_nz(void* state_ptr);

#ifdef __cplusplus
}
#endif

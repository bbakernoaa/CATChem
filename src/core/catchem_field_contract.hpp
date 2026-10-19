/**
 * @file catchem_field_contract.hpp
 * @brief Semantic metadata describing how a field may be exchanged between
 *        the CATChem core and a process or host driver.
 *
 * @details Declares the vocabulary shared by StateManager, the process layer,
 * and the C API: the DataflowStatus error codes returned by field-transfer
 * helpers, the semantic axis and persistence enums that classify a field, and
 * the two contract aggregates. FieldContract describes a bound field as it
 * exists in storage (name, units, extents, axes, aliases, persistence);
 * FieldAccessContract describes what one scheme demands of a field (units,
 * axes, read/write intent, execution space, requirement level).
 * StateManager::prepare_field_access() is the arbiter between the two, and
 * mismatches are reported through DataflowStatus rather than exceptions.
 * Everything here is plain value data with no ownership semantics; the file
 * is header-only and has no device code.
 */
#pragma once

#include <algorithm>
#include <cctype>
#include <cstddef>
#include <string>
#include <vector>

namespace catchem {

    /**
     * @brief Error codes returned by field-transfer and dataflow helpers.
     *
     * @details Values are fixed integers so they cross the C/Fortran ABI
     * stably; zero means success and every non-zero constant names a specific
     * structural failure the caller can branch on.
     */
    enum class DataflowStatus : int {
        Success = 0,         ///< Operation completed as requested.
        NullArgument = 1,    ///< A required pointer or view argument was null.
        MissingField = 2,    ///< No field is bound for the requested canonical name.
        RankMismatch = 3,    ///< Requested rank differs from the bound field's rank.
        ExtentMismatch = 4,  ///< Same rank but at least one extent differs.
        InvalidIndex = 5,    ///< A column, level, or species index is out of bounds.
        StaleGeneration = 6, ///< The field was not refreshed in the required import generation.
        DuplicateMapping = 7,///< Two mappings claim the same target field.
        InvalidState = 8,    ///< The field or manager is not in a state that allows the operation.
        InternalError = 9    ///< An unexpected internal fault; report it upstream.
    };

    /// Physical meaning of one field dimension, used to validate axis order.
    enum class SemanticAxis {
        Column,     ///< Horizontal grid column (flattened tile index).
        Level,      ///< Model layer midpoint.
        Interface,  ///< Layer boundary (n_levels + 1 extent).
        SoilLayer,  ///< Sub-surface layer or land-use category slot.
        Species,    ///< Chemical species index into the mechanism.
        Category,   ///< Emission or aerosol mode category.
        Singleton   ///< Trivial length-1 dimension kept for rank compatibility.
    };

    /// How long a field's contents stay valid across import generations.
    enum class PersistencePolicy {
        Timestep,  ///< Re-imported every timestep; invalidated at a new generation.
        Persistent ///< Static across the run (coordinates, areas); never invalidated.
    };

    /// Availability of a bound field's data for the current import.
    enum class AvailabilityState {
        Unavailable, ///< No valid data; the field must not be read.
        Current,     ///< Data present and valid for the active generation.
        Stale        ///< Data present but from an older generation.
    };

    /// Which side last wrote the buffer, driving host/device sync decisions.
    enum class LatestWriter {
        Uninitialized, ///< No host or device write recorded yet.
        HostCurrent,   ///< Host buffer holds the newest values.
        DeviceCurrent, ///< Device mirror holds the newest values.
        Synchronized   ///< Host and device agree; no transfer needed.
    };

    /// Caller's intended use of a field in one access contract.
    enum class AccessIntent {
        Read,      ///< Field is consumed only.
        Write,     ///< Field is produced only.
        ReadWrite  ///< Field is both consumed and updated in place.
    };

    /// Where the caller wants the field's data to live during access.
    enum class ExecutionSpaceIntent {
        Host,   ///< Sync to host and hand out host pointers.
        Device, ///< Sync to device and hand out device views.
        Either  ///< No preference; the host path is used.
    };

    /// Whether a scheme cannot run without the field.
    enum class FieldRequirement {
        Required, ///< Absent or mismatched field is an error.
        Optional  ///< Scheme degrades gracefully when the field is missing.
    };

    /**
     * @brief Uppercase a field name so lookups ignore case.
     * @param name Field identifier; copied, not modified in place.
     * @return The uppercase canonical identity used as a map key.
     */
    inline std::string canonicalize_field_identity(std::string name) {
        std::transform(name.begin(), name.end(), name.begin(),
                       [](unsigned char value) { return static_cast<char>(std::toupper(value)); });
        return name;
    }

    /**
     * @brief Description of a bound field as it exists in storage.
     *
     * @details Owned by the InteropField that carries it and compared against
     * a scheme's FieldAccessContract before access is granted. The struct is
     * plain data; nothing here allocates or references device memory.
     */
    struct FieldContract {
        std::string canonical_name;                 ///< Uppercase field identifier.
        std::string units;                          ///< CF-style unit string, e.g. "K" or "kg/m3".
        std::vector<std::size_t> extents;           ///< Size of each dimension, one entry per axis.
        std::vector<SemanticAxis> axes;             ///< Semantic meaning of each dimension.
        std::vector<std::string> aliases;           ///< Alternate names accepted for this field.
        PersistencePolicy persistence = PersistencePolicy::Timestep; ///< Validity window of the data.

        /**
         * @brief Report whether the contract is self-consistent.
         * @return true when name, units, and extents are non-empty, extents
         *         and axes have equal size, and no extent is zero.
         */
        bool structurally_valid() const noexcept {
            if (canonical_name.empty() || units.empty() || extents.empty() || extents.size() != axes.size())
                return false;
            for (const auto extent : extents) {
                if (extent == 0)
                    return false;
            }
            return true;
        }

        /**
         * @brief Compare two contracts for interchange compatibility.
         * @param other Contract to test against this one.
         * @return true when the canonical names agree case-insensitively and
         *         units, extents, axes, and persistence are exactly equal.
         *
         * @note Aliases are deliberately ignored: only the canonical identity
         * and the physical description must match.
         */
        bool compatible_with(const FieldContract& other) const noexcept {
            return canonicalize_field_identity(canonical_name) == canonicalize_field_identity(other.canonical_name) &&
                   units == other.units && extents == other.extents && axes == other.axes &&
                   persistence == other.persistence;
        }
    };

    /**
     * @brief What one scheme demands of a field during an access request.
     *
     * @details StateManager matches this against the bound field's
     * FieldContract in prepare_field_access(); the intent members select the
     * sync direction, the write-back bookkeeping in complete_field_access(),
     * and the generation-staleness check. Plain value data with no ownership.
     */
    struct FieldAccessContract {
        std::string canonical_name;                 ///< Requested field identifier.
        std::string units;                          ///< Units the scheme expects, e.g. "kg/m3".
        std::vector<SemanticAxis> axes;             ///< Axis order the scheme expects; its size gives the rank.
        PersistencePolicy persistence = PersistencePolicy::Timestep; ///< Persistence the scheme assumes.
        FieldRequirement requirement = FieldRequirement::Required;   ///< Whether the field is mandatory.
        AccessIntent access = AccessIntent::Read;                    ///< Read, write, or in-place update.
        ExecutionSpaceIntent execution_space = ExecutionSpaceIntent::Either; ///< Where data must live.
        bool produced = false;                      ///< Set when the scheme creates the field itself.

        /// True when the scheme consumes the field (Read or ReadWrite intent).
        bool reads() const noexcept { return access == AccessIntent::Read || access == AccessIntent::ReadWrite; }
        /// True when the scheme modifies the field (Write or ReadWrite intent).
        bool writes() const noexcept { return access == AccessIntent::Write || access == AccessIntent::ReadWrite; }
    };

} // namespace catchem

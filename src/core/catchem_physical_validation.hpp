/**
 * @file catchem_physical_validation.hpp
 * @brief Policy and aggregated findings for physical-range validation.
 *
 * @details Derived-meteorology entry points (StateManager::derive_bxheight()
 * and derive_airden_dry()) scan their inputs before mutating anything and
 * record every out-of-range value here. The policy - parsed from the YAML
 * physical_validation/policy setting or set at runtime through
 * catchem_state_set_physical_validation_policy_checked() - decides the
 * reaction: Reject throws std::domain_error before any write (surfaced as
 * PhysicalValidationFailure at the C boundary), while WarnAndClamp and
 * CountAndContinue let the kernels clamp invalid values and keep the
 * aggregated report for the host to read back.
 */
#pragma once
#include <cstddef>
#include <limits>
#include <string>
#include <vector>
namespace catchem {
    /// What the derive_* kernels do when an input fails a physical range rule.
    enum class PhysicalValidationPolicy {
        Reject,          ///< Throw std::domain_error before mutating any output field.
        WarnAndClamp,    ///< Keep going; kernels clamp invalid values and the report is retained.
        CountAndContinue ///< Same clamp behaviour as WarnAndClamp in the current kernels; distinguished only by intent.
    };
    /**
     * @brief Aggregated findings for one (field, rule) pair.
     *
     * @details observe() merges repeats of the same pair into one entry rather
     * than storing every offending point, so a report stays small even for
     * million-cell columns.
     */
    struct PhysicalIssue {
        std::string field, rule; ///< Field and rule, e.g. "PEDGE" / "positive-descending".
        std::size_t count = 0;   ///< Violating values observed for this pair.
        double observed_min = std::numeric_limits<double>::infinity();  ///< Smallest violating value [field units].
        double observed_max = -std::numeric_limits<double>::infinity(); ///< Largest violating value [field units].
        std::vector<std::size_t> locations; ///< First 16 violating flat indices (col + level*n_cols); capped.
        std::string correction;             ///< Kernel fallback note (clamp/skip) for the operator.
    };
    /**
     * @brief The findings one derive_* scan produced, in first-seen order.
     *
     * @details Owned by StateManager as a member; cleared at the start of each
     * scan, so it always describes the most recent validation attempt. Read
     * back at the C boundary through
     * catchem_state_get_physical_validation_report_checked().
     */
    class PhysicalValidationReport {
    public:
        /// Drop all recorded issues (called before each fresh input scan).
        void clear() { issues_.clear(); }
        /**
         * @brief Record one violating value, merging into an existing (field, rule) issue.
         *
         * @param field     Validated field name.
         * @param rule      Rule identifier that was violated.
         * @param value     The violating value [field's units]; updates min/max.
         * @param location  Flat index of the violating point (col + level*n_cols).
         * @param correction Optional note on the kernel's fallback behaviour.
         *
         * @note Only the first 16 locations per issue are kept; counts and
         *       extrema are exact regardless of volume.
         */
        void observe(std::string field, std::string rule, double value, std::size_t location,
                     std::string correction = {});
        /// True when no violation was recorded.
        bool empty() const noexcept { return issues_.empty(); }
        /// Number of distinct (field, rule) violations, not violating values.
        std::size_t issue_count() const noexcept { return issues_.size(); }
        /// The aggregated issues, first-seen order (borrowed view).
        const std::vector<PhysicalIssue>& issues() const noexcept { return issues_; }
        /// One line per issue: field, rule, count, observed range and correction.
        std::string format() const;

    private:
        std::vector<PhysicalIssue> issues_; ///< Aggregated findings, one per (field, rule).
    };
} // namespace catchem

/**
 * @file catchem_validation.hpp
 * @brief Severity-tagged issue collection used by schedule and config validation.
 *
 * @details ExecutionPlan::compile() accumulates ValidationIssueDetail entries
 * instead of failing on the first problem, so a host sees every contract
 * defect in one report. (ConfigManager keeps its own separate ValidationIssue
 * types for YAML checks; these serve the schedule compiler.) The report is a
 * plain value type: no locking, no allocation policy beyond the backing
 * vector, and format() renders one line per issue for logging or exception text.
 */
#pragma once

#include <algorithm>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

namespace catchem {

    /// How much an issue matters: Warning is informational, Error blocks the schedule.
    enum class IssueSeverity { Warning, Error };

    /**
     * @brief One validation finding, with the fix the author expects.
     */
    struct ValidationIssueDetail {
        IssueSeverity severity = IssueSeverity::Error; ///< Error rejects the schedule; Warning does not.
        std::string category;                          ///< Machine-readable check name, e.g. "dependency-order".
        std::string path;                              ///< Offending object: process name or config path.
        std::string message;                           ///< What is wrong, for humans.
        std::string correction;                        ///< How to fix it; may be empty.
    };

    /**
     * @brief Append-only collection of validation issues with a text renderer.
     */
    class ValidationIssueReport {
    public:
        /// Record one issue; issues keep insertion order for deterministic reports.
        void add(ValidationIssueDetail issue) { issues_.push_back(std::move(issue)); }
        /// True when at least one recorded issue has Error severity.
        bool has_errors() const noexcept {
            return std::any_of(issues_.begin(), issues_.end(),
                               [](const auto& issue) { return issue.severity == IssueSeverity::Error; });
        }
        /// True when nothing was recorded.
        bool empty() const noexcept { return issues_.empty(); }
        /// The recorded issues, oldest first (borrowed view).
        const std::vector<ValidationIssueDetail>& issues() const noexcept { return issues_; }
        /**
         * @brief Render one newline-terminated line per issue.
         * @return Text like `error [category] path: message (correction)`; empty
         *         string for an empty report. Used in rejection messages.
         */
        std::string format() const {
            std::ostringstream output;
            for (const auto& issue : issues_) {
                output << (issue.severity == IssueSeverity::Error ? "error" : "warning") << " [" << issue.category
                       << "] " << issue.path << ": " << issue.message;
                if (!issue.correction.empty())
                    output << " (" << issue.correction << ")";
                output << '\n';
            }
            return output.str();
        }

    private:
        std::vector<ValidationIssueDetail> issues_; ///< Recorded findings, in insertion order.
    };

} // namespace catchem

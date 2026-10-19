/**
 * @file catchem_selected_dispatch.hpp
 * @brief Scheduling ledger for host-driven selected-process execution.
 *
 * @details Under NUOPC/CCPP the host, not CATChem, decides which process runs
 * when, and may sub-cycle it. SelectedProcessLedger enforces the rules that
 * make that safe: exactly one interval open at a time, strictly new interval
 * identities, unique (interval, site, subcycle, process) invocation keys,
 * durations no longer than the enclosing interval, and required product
 * producers completed before their consumers. It owns scheduling validation
 * only - the actual process body arrives as a callback from Core, so this
 * header stays independent of the state managers and is header-only.
 *
 * Not thread-safe on its own; Core guards every call with its lifecycle mutex.
 * @see Core::begin_interval/run_selected_process/end_interval for the callers.
 */
#pragma once

#include <cmath>
#include <cstdint>
#include <functional>
#include <map>
#include <set>
#include <stdexcept>
#include <string>
#include <tuple>
#include <utility>
#include <vector>
#include "catchem_process_interface.hpp"

namespace catchem {

    /**
     * @brief One schedule entry as the ledger sees it.
     */
    struct SelectedProcessSpec {
        std::string name;                           ///< Process name; must be non-empty.
        bool active = false;                        ///< Only active names may be selected.
        std::vector<std::string> required_producers; ///< Must complete before this process may run.
    };

    /// Contract ledger shared by checked selected-process entry points. It owns
    /// scheduling validation only; callbacks remain the Core's one-process body.
    /**
     * @brief Validates interval and invocation identities for selected dispatch.
     *
     * @details Constructed from the active schedule; rebuilding it (as Core does
     * after any schedule change) discards interval state, which Core refuses
     * while an interval is open. Duplicate names in the spec list become
     * ambiguous and are rejected at run() rather than resolved by order.
     */
    class SelectedProcessLedger {
    public:
        /**
         * @brief Build the ledger from the active schedule.
         * @param specs One entry per scheduled process; order is irrelevant.
         * @throws std::invalid_argument when any spec has an empty name.
         */
        explicit SelectedProcessLedger(std::vector<SelectedProcessSpec> specs) {
            for (auto& spec : specs) {
                // Copy the key before moving the spec. Function argument
                // evaluation order otherwise permits the move to empty the
                // name before emplace copies it.
                const auto name = spec.name;
                if (name.empty())
                    throw std::invalid_argument("Selected-process schedule has an empty process name");
                if (!specs_.emplace(name, std::move(spec)).second)
                    ambiguous_.insert(name);
            }
        }

        /**
         * @brief Open a host interval and reset per-interval bookkeeping.
         * @param id       Host interval identity; must be non-zero and differ
         *                 from the previously closed interval.
         * @param duration Interval length [s]; must be finite and positive.
         * @throws std::logic_error when an interval is already open, or
         *         std::invalid_argument on a repeated/zero id or bad duration.
         */
        void begin_interval(std::uint64_t id, double duration) {
            if (open_) throw std::logic_error("A selected-process interval is already open");
            if (id == 0 || id == last_interval_ || !std::isfinite(duration) || duration <= 0.0)
                throw std::invalid_argument("Interval identity must be new and duration must be positive and finite");
            interval_ = id;
            duration_ = duration;
            open_ = true;
            invocations_.clear();
            completed_.clear();
            ++interval_count_;
        }

        /**
         * @brief Admit and execute exactly one selected-process invocation.
         *
         * @param key           Invocation identity; its interval must equal the
         *                      open interval and site/process must be non-empty.
         * @param duration      This invocation's duration [s]; finite, positive
         *                      and at most the open interval's duration.
         * @param selected_body The process body, invoked only if every check
         *                      passes; never retried, and only one body runs
         *                      per admitted call.
         * @return Succeeded with publication_eligible true when the body
         *         returns; Failed with the body's text (or "Unknown
         *         selected-process failure") when it throws; Rejected with the
         *         reason when admission fails - no interval open, bad duration,
         *         incomplete identity, unknown/inactive process, ambiguous
         *         name, reused invocation key, or an unmet required producer.
         * @note Never throws: rejection and failure are reported in the
         *       outcome so the C boundary can map them to ContractViolation or
         *       ProcessFailure respectively.
         */
        SelectedOperationOutcome run(const SelectedInvocationKey& key, double duration,
                                     const std::function<void()>& selected_body) {
            SelectedOperationOutcome result;
            result.process = key.process;
            if (!open_) return rejected(result, "No host interval is open");
            if (!std::isfinite(duration) || duration <= 0.0 || duration > duration_)
                return rejected(result, "Invocation duration must be positive, finite, and no longer than its interval");
            if (key.interval != interval_ || key.site.empty() || key.process.empty() || !selected_body)
                return rejected(result, "Invocation identity or selected operation is incomplete");
            const auto found = specs_.find(key.process);
            if (found == specs_.end() || !found->second.active)
                return rejected(result, "Selected process is unavailable");
            if (ambiguous_.count(key.process) != 0)
                return rejected(result, "Selected process name is ambiguous in the active schedule");
            const auto identity = std::make_tuple(key.interval, key.site, key.subcycle, key.process);
            if (!invocations_.insert(identity).second)
                return rejected(result, "Invocation key has already been used in this interval");
            for (const auto& producer : found->second.required_producers)
                if (completed_.count(producer) == 0)
                    return rejected(result, "Required producer has not completed: " + producer);
            try {
                selected_body(); // exactly one process body is admitted here
                completed_.insert(key.process);
                result.status = SelectedOperationStatus::Succeeded;
                result.publication_eligible = true;
                return result;
            } catch (const std::exception& error) {
                result.status = SelectedOperationStatus::Failed;
                result.cause = error.what();
            } catch (...) {
                result.status = SelectedOperationStatus::Failed;
                result.cause = "Unknown selected-process failure";
            }
            return result;
        }

        /**
         * @brief Close the open interval.
         * @param id Must equal the open interval's id.
         * @throws std::logic_error when no interval is open or the id does not
         *         match. The id is remembered so the next begin_interval()
         *         rejects an immediate reuse.
         */
        void end_interval(std::uint64_t id) {
            if (!open_ || id != interval_) throw std::logic_error("Interval close does not match the open interval");
            open_ = false;
            last_interval_ = interval_;
        }

        /// Total number of intervals opened since construction.
        std::size_t interval_count() const noexcept { return interval_count_; }
        /// Number of invocations admitted in the current (or last) interval.
        std::size_t invocation_count() const noexcept { return invocations_.size(); }
        /// True while a host interval is open.
        bool interval_open() const noexcept { return open_; }

    private:
        /// Mark @p result Rejected with @p cause and return it.
        SelectedOperationOutcome rejected(SelectedOperationOutcome result, std::string cause) const {
            result.status = SelectedOperationStatus::Rejected;
            result.cause = std::move(cause);
            return result;
        }
        std::map<std::string, SelectedProcessSpec> specs_;             ///< Schedule entries by process name.
        std::set<std::string> ambiguous_; ///< Names registered more than once; always rejected.
        /// Invocation keys already admitted in this interval, for uniqueness.
        std::set<std::tuple<std::uint64_t, std::string, std::uint32_t, std::string>> invocations_;
        std::set<std::string> completed_; ///< Processes that succeeded this interval.
        std::uint64_t interval_ = 0;                                   ///< Identity of the open interval.
        std::uint64_t last_interval_ = 0; ///< The most recently closed interval id.
        double duration_ = 0.0;                                        ///< Open interval's duration [s].
        std::size_t interval_count_ = 0;                               ///< Intervals opened since construction.
        bool open_ = false;                                            ///< Whether an interval is currently open.
    };

} // namespace catchem

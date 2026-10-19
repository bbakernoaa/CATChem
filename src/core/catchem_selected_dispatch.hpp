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

    struct SelectedProcessSpec {
        std::string name;
        bool active = false;
        std::vector<std::string> required_producers;
    };

    /// Contract ledger shared by checked selected-process entry points. It owns
    /// scheduling validation only; callbacks remain the Core's one-process body.
    class SelectedProcessLedger {
    public:
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

        void end_interval(std::uint64_t id) {
            if (!open_ || id != interval_) throw std::logic_error("Interval close does not match the open interval");
            open_ = false;
            last_interval_ = interval_;
        }

        std::size_t interval_count() const noexcept { return interval_count_; }
        std::size_t invocation_count() const noexcept { return invocations_.size(); }
        bool interval_open() const noexcept { return open_; }

    private:
        SelectedOperationOutcome rejected(SelectedOperationOutcome result, std::string cause) const {
            result.status = SelectedOperationStatus::Rejected;
            result.cause = std::move(cause);
            return result;
        }
        std::map<std::string, SelectedProcessSpec> specs_;
        std::set<std::string> ambiguous_;
        std::set<std::tuple<std::uint64_t, std::string, std::uint32_t, std::string>> invocations_;
        std::set<std::string> completed_;
        std::uint64_t interval_ = 0;
        std::uint64_t last_interval_ = 0;
        double duration_ = 0.0;
        std::size_t interval_count_ = 0;
        bool open_ = false;
    };

} // namespace catchem

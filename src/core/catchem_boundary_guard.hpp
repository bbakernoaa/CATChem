/**
 * @file catchem_boundary_guard.hpp
 * @brief Shared admission, output-initialisation and exception-translation
 *        helpers for the checked CATChem C entry points.
 *
 * @details BoundaryGuard is the single funnel through which every
 * catchem_api_*.cpp entry point admits an opaque handle, pre-sets its outputs
 * to a safe value, and converts escaping C++ exceptions into BoundaryStatus
 * codes. The mapping here differs deliberately from the older
 * guard_boundary_call() in catchem_error.hpp: std::invalid_argument means a
 * host broke a published API contract (ContractViolation), not a bad internal
 * state, so integrators can tell "your arguments are wrong" from "the model
 * is broken".
 */
#pragma once
#include "catchem_error.hpp"
#include "catchem_handle_registry.hpp"
#include <string>
#include <utility>

namespace catchem {

    /**
     * @brief Static helpers enforcing the C-boundary admission and error rules.
     *
     * @details All members are static; the class carries no state. Thread
     * safety comes from HandleRegistry (internally locked) and the
     * thread_local boundary-error record, so concurrent host threads admit and
     * fail independently.
     */
    class BoundaryGuard {
    public:
        /**
         * @brief Validate a handle and take its admission lease.
         *
         * @param handle    Opaque pointer supplied by the host; may be null,
         *                  which is reported rather than dereferenced.
         * @param type      The HandleType the operation requires.
         * @param operation Stable operation name recorded on failure; null is
         *                  stored as the empty string.
         * @param lease     Output; on success it holds the RAII lease that
         *                  keeps the handle alive for the caller's scope. The
         *                  caller must not dereference @p handle beyond it.
         * @return Success, or the registry's failure code (NullArgument,
         *         InvalidHandle or WrongHandleType) recorded as the thread's
         *         last boundary error. StaleGeneration cannot occur here:
         *         admit() queries no generation filter.
         */
        static BoundaryStatus admit(const void* handle, HandleType type, const char* operation,
                                    AdmissionLease& lease) noexcept {
            auto acquired = HandleRegistry::instance().acquire(handle, type);
            if (acquired.first != BoundaryStatus::Success)
                return set_boundary_error(acquired.first, operation ? operation : "", "handle",
                                          "null, stale, destroyed, closing, or wrong-type handle");
            lease = std::move(acquired.second);
            return BoundaryStatus::Success;
        }

        /**
         * @brief Pre-set an output argument to a safe value before validation.
         *
         * @tparam Output  The output's value type (int, double, pointer, ...).
         * @param output   Caller-owned output pointer; null is reported.
         * @param safe_value Value written when @p output is valid, so a failed
         *                  call never leaves host-visible memory uninitialised.
         * @param operation Stable operation name recorded on failure.
         * @param name     Output's argument name recorded on failure; null
         *                  becomes "output".
         * @return true when the value was written; false (with NullArgument
         *         recorded) when @p output was null.
         */
        template <typename Output>
        static bool initialize_output(Output* output, Output safe_value, const char* operation,
                                      const char* name) noexcept {
            if (!output) {
                set_boundary_error(BoundaryStatus::NullArgument, operation ? operation : "", name ? name : "output",
                                   "output pointer is null");
                return false;
            }
            *output = std::move(safe_value);
            return true;
        }

        /**
         * @brief Run a checked operation, translating escapes into statuses.
         *
         * @tparam Function Zero-argument callable performing the work.
         * @param operation Stable operation name recorded on failure; null is
         *                  stored as the empty string.
         * @param object    Argument or field name recorded on failure.
         * @param function  The work; any exception it throws is caught here so
         *                  nothing crosses the BIND(C) edge.
         * @return Success on normal return; ContractViolation for
         *         std::invalid_argument (a broken API contract such as a bad
         *         schema version or interval identity); InternalError for any
         *         other std::exception (cause = what()) or non-standard
         *         exception.
         *
         * @note Clears the previous error on entry, so Success guarantees an
         *       empty record afterwards.
         */
        template <typename Function>
        static BoundaryStatus invoke(const char* operation, const char* object, Function&& function) noexcept {
            clear_boundary_error();
            try {
                std::forward<Function>(function)();
                return BoundaryStatus::Success;
            } catch (const std::invalid_argument& error) {
                return set_boundary_error(BoundaryStatus::ContractViolation, operation ? operation : "",
                                          object ? object : "", error.what());
            } catch (const std::exception& error) {
                return set_boundary_error(BoundaryStatus::InternalError, operation ? operation : "",
                                          object ? object : "", error.what());
            } catch (...) {
                return set_boundary_error(BoundaryStatus::InternalError, operation ? operation : "",
                                          object ? object : "", "unknown non-standard exception");
            }
        }
    };

} // namespace catchem

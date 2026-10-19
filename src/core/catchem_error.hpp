/**
 * @file catchem_error.hpp
 * @brief The status vocabulary and thread-local error record for the C boundary.
 *
 * @details Every enumerator of BoundaryStatus is the single source of truth for
 * one CATCHEM_* constant in the flat C API (catchem_api.hpp), which repeats the
 * same integer values; status_code() in catchem_api_internal.hpp is the only
 * conversion between them, so the two lists must stay in lock-step.
 *
 * Failures are reported through a thread-local BoundaryError rather than an
 * exception: the checked entry points catch everything at the boundary and
 * record operation, object and cause, which catchem_get_last_error() then
 * renders. Because the record is thread_local, one thread's failure never
 * obscures another's, and each checked call clears the record on entry so a
 * stale cause is never read after a success.
 *
 * The legacy ErrorCode/ErrorManager pair predates this vocabulary and is kept
 * only for older call sites that print a context stack to std::cerr; new code
 * must use BoundaryStatus.
 */
#pragma once

#include <exception>
#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>

namespace catchem {

    /**
     * @brief Status codes returned by every checked CATChem C entry point.
     *
     * @details The integer values mirror the CATCHEM_* constants in
     * catchem_api.hpp exactly, so a host may compare against either spelling.
     * Codes 0-9 are shared with the older DataflowStatus used by field
     * contracts (catchem_field_contract.hpp); 10 onward are handle-, runtime-,
     * contract- and lifecycle-specific to the C boundary.
     *
     * The exact condition behind each code, as emitted by the current sources:
     * - NullArgument: a required pointer is null (handle, output buffer, field
     *   name, or versioned descriptor) before any work is attempted.
     * - MissingField: a named diagnostic or 3D state field is not registered.
     * - RankMismatch: a diagnostic field is registered with a rank other than
     *   2 or 3; the C entry point returns the CATCHEM_RANK_MISMATCH spelling
     *   directly (see the enumerator note for the enum-spelling status).
     * - ExtentMismatch: caller-declared extents disagree with the StateManager
     *   (columns/levels/species, or PEDGE/Z/PFILSAN/PFLLSAN level+1), a create
     *   extent is non-positive, or a string buffer is shorter than the
     *   reported required length.
     * - InvalidIndex: a species, process, field, diagnostic, alias, role or
     *   label slot index is negative or at/past the container end.
     * - StaleGeneration: the handle is registered but its generation differs
     *   from the requested one (HandleRegistry::acquire()).
     * - DuplicateMapping: reserved; no code path currently returns either spelling.
     * - InvalidState: an axis ordinal or validation policy is outside its
     *   enumeration, a diagnostic has no packed dimension to unpack, the Core
     *   has no active mechanism, a config-driven constructor or timestep was
     *   rejected, or RuntimeLease::request_finalize() ran with a lease held;
     *   also guard_boundary_call()'s translation of std::invalid_argument.
     * - InternalError: any other escaping std::exception, a non-standard
     *   exception, or a HandleRegistry insertion failure.
     * - InvalidHandle: the pointer is unregistered, its owner differs or is
     *   gone, or it is closing; also close_and_wait() on an unknown handle.
     * - WrongHandleType: registered, but under a different HandleType than the
     *   operation requires, e.g. a State handle where a Core is expected.
     * - RuntimeUnavailable: reserved; no code path currently returns either spelling.
     * - ContractViolation: a versioned descriptor's schema_version/struct_size
     *   is unsupported, an interval or invocation identity is empty or its
     *   duration non-finite/longer than its interval, a field rank exceeds the
     *   v1 descriptor capacity, or BoundaryGuard::invoke() translated
     *   std::invalid_argument.
     * - InvalidConfiguration: a manifest or configuration file failed to parse
     *   or validate.
     * - ProcessFailure: an admitted selected process body threw; the cause is
     *   the process's own failure text.
     * - ShutdownFailure: Core::shutdown() threw while finalizing processes in
     *   catchem_core_destroy_checked(); the Core is deleted regardless.
     * - PhysicalValidationFailure: a derived-meteorology input made the result
     *   undefined (std::domain_error from derive_bxheight/derive_airden_dry).
     * @see catchem_get_last_error() for the recorded cause of a failure.
     */
    enum class BoundaryStatus : int {
        Success = 0,                    ///< The operation completed and its outputs are valid.
        NullArgument = 1,               ///< A required pointer argument (handle, buffer, name, descriptor) was null.
        MissingField = 2,               ///< The named diagnostic or state field is not registered.
        RankMismatch = 3,               ///< A diagnostic register call used a rank other than 2 or 3; emitted as the CATCHEM_RANK_MISMATCH spelling by the C entry point.
        ExtentMismatch = 4,             ///< Declared extents or buffer lengths disagree with the state contract.
        InvalidIndex = 5,               ///< An index argument is outside its container's valid range.
        StaleGeneration = 6,            ///< The handle exists but predates the requested generation.
        DuplicateMapping = 7,           ///< Reserved: mirrors CATCHEM_DUPLICATE_MAPPING; no code path currently returns either spelling.
        InvalidState = 8,               ///< The callee is not in a state that admits this call.
        InternalError = 9,              ///< An unexpected escaping exception or registry failure; cause carries what().
        InvalidHandle = 10,             ///< The pointer is not a usable handle of the expected type.
        WrongHandleType = 11,           ///< The handle is registered under a different HandleType.
        RuntimeUnavailable = 12,        ///< Reserved: mirrors CATCHEM_RUNTIME_UNAVAILABLE; no code path currently returns either spelling.
        ContractViolation = 13,         ///< A versioned descriptor or interval argument breaks its contract.
        InvalidConfiguration = 14,      ///< A configuration or manifest file failed to parse or validate.
        ProcessFailure = 15,            ///< An admitted process body threw during execution.
        ShutdownFailure = 16,           ///< Process finalization failed during handle destruction.
        PhysicalValidationFailure = 17  ///< Physical validation rejected an input to a derived field.
    };

    /**
     * @brief The most recent boundary failure on this thread.
     *
     * @details Populated by set_boundary_error() and read by
     * catchem_get_last_error(); cleared at the entry of every checked call, so
     * a Success return never leaves a stale record behind.
     */
    struct BoundaryError {
        BoundaryStatus status = BoundaryStatus::Success; ///< The status code to return to the C caller.
        std::string operation;                           ///< Stable operation name, e.g. "core_run_process".
        std::string object;                              ///< Offending argument or field name; empty when unspecified.
        std::string cause;                               ///< Human-readable reason, usually an exception's what().
    };

    /// The per-thread error record. thread_local by design: concurrent host
    /// threads (e.g. per-PET NUOPC calls) must not overwrite each other's cause.
    inline thread_local BoundaryError boundary_error;

    /// Reset this thread's record to Success, clearing operation/object/cause.
    inline void clear_boundary_error() noexcept {
        boundary_error = {};
    }

    /**
     * @brief Record a boundary failure on this thread.
     *
     * @param status    Status code to report.
     * @param operation Moved into the record; stable operation name.
     * @param object    Moved into the record; offending argument or field name.
     * @param cause     Moved into the record; human-readable reason.
     * @return @p status unchanged, so callers can write `return set_boundary_error(...)`.
     *
     * @note noexcept: recording an error must never itself throw out of the C
     *       boundary. The strings may allocate, and a failed allocation
     *       terminates rather than crossing the BIND(C) edge.
     */
    inline BoundaryStatus set_boundary_error(BoundaryStatus status, std::string operation, std::string object,
                                             std::string cause) noexcept {
        boundary_error = {status, std::move(operation), std::move(object), std::move(cause)};
        return status;
    }

    /**
     * @brief This thread's boundary-error record.
     * @return Reference to the thread_local BoundaryError, valid until the next
     *         set_boundary_error() or clear_boundary_error() on this thread.
     */
    inline const BoundaryError& last_boundary_error() noexcept {
        return boundary_error;
    }

    /**
     * @brief Run a C++ operation, translating every escape into a status code.
     *
     * @tparam Function Zero-argument callable performing the work.
     * @param operation Stable operation name recorded on failure; may be null,
     *                  which is stored as the empty string.
     * @param object    Argument or field name recorded on failure; may be null.
     * @param function  The work to perform. It must not itself cross the C
     *                  boundary; only its status does.
     * @return Success when the callable returns normally; InvalidState when it
     *         throws std::invalid_argument; InternalError for any other
     *         std::exception (cause = what()) or non-standard exception.
     *
     * @note Clears any previous error first, so Success always means the record
     *       was reset rather than inherited. This is the older boundary helper;
     *       the checked C API now uses BoundaryGuard::invoke(), which maps
     *       std::invalid_argument to ContractViolation instead.
     * @see BoundaryGuard::invoke() in catchem_boundary_guard.hpp
     */
    template <typename Function>
    BoundaryStatus guard_boundary_call(const char* operation, const char* object, Function&& function) noexcept {
        clear_boundary_error();
        try {
            function();
            return BoundaryStatus::Success;
        } catch (const std::invalid_argument& error) {
            return set_boundary_error(BoundaryStatus::InvalidState, operation ? operation : "", object ? object : "",
                                      error.what());
        } catch (const std::exception& error) {
            return set_boundary_error(BoundaryStatus::InternalError, operation ? operation : "", object ? object : "",
                                      error.what());
        } catch (...) {
            return set_boundary_error(BoundaryStatus::InternalError, operation ? operation : "", object ? object : "",
                                      "unknown non-standard exception");
        }
    }

    /// Legacy integer error codes used by ErrorManager::report_error(); they do
    /// not map onto the BoundaryStatus/CATCHEM_* vocabulary and are retained
    /// only for existing call sites that print a context stack.
    enum ErrorCode { SUCCESS = 0, FAILURE = -1, INVALID_INPUT = 1001, INVALID_STATE = 1003, MEMORY_ALLOCATION = 1007 };

    /**
     * @brief Legacy error printer that annotates messages with a context stack.
     *
     * @details Instances are local to the reporting component: the context
     * stack is ordinary instance state, so pushing on one manager is invisible
     * to another. Output goes straight to std::cerr; nothing is recorded for
     * later retrieval, which is why the C boundary uses BoundaryError instead.
     */
    class ErrorManager {
    private:
        std::vector<std::string> context_stack; ///< Pushed scope labels, outermost first.

    public:
        /// Append a scope label shown by later report_error() calls.
        void push_context(const std::string& ctx) { context_stack.push_back(ctx); }
        /// Remove the innermost scope label; a no-op on an empty stack.
        void pop_context() {
            if (!context_stack.empty()) {
                context_stack.pop_back();
            }
        }
        /**
         * @brief Print a code, message and the current context chain to std::cerr.
         *
         * @param code Legacy ErrorCode value.
         * @param msg  Message text; never validated or localized.
         *
         * @note Does not throw and does not record anything: use the checked
         *       boundary helpers when a host model must observe the failure.
         */
        void report_error(ErrorCode code, const std::string& msg) {
            std::cerr << "[CATChem C++ Error " << code << "] " << msg << " | Context: ";
            for (const auto& ctx : context_stack) {
                std::cerr << ctx << " -> ";
            }
            std::cerr << "End\n";
        }
    };

    /**
     * @brief Fail fast when a process bridge is missing a required field pointer.
     *
     * @param process_name Process reporting the failure; must not be null.
     * @param field_name   Field whose binding is missing; must not be null.
     * @param ptr          Resolved field pointer, or null when unbound.
     *
     * @throws std::runtime_error when @p ptr is null, with a "FATAL ERROR:"
     *         prefix naming the process and field. Checked C entry points
     *         translate that into InternalError via their catch handlers.
     */
    inline void require_field_pointer(const char* process_name, const char* field_name, const void* ptr) {
        if (ptr == nullptr) {
            throw std::runtime_error(std::string("FATAL ERROR: ") + process_name + " process missing required field " +
                                     field_name);
        }
    }

} // namespace catchem

/**
 * @file catchem_api_internal.hpp
 * @brief Shared plumbing for the checked flat-C-API translation units.
 *
 * @details Every catchem_api_*.cpp source reduces its argument validation,
 * handle admission, error recording and string copying to the five inline
 * helpers below, so the status vocabulary and the boundary-error record stay
 * identical across subsystems. The helpers are @c inline and live in
 * catchem::api_internal to avoid ODR conflicts between the separate objects.
 *
 * The built-in process registration entry points are declared as weak symbols
 * so the API links against a core that was built without a given scheme: the
 * registration function pointer is null and register_builtin_processes() skips
 * it instead of failing to link.
 */
#pragma once

#include "catchem_boundary_guard.hpp"
#include "catchem_error.hpp"
#include "catchem_handle_registry.hpp"
#include <cstring>
#include <mutex>
#include <string>

/// Marks a declaration as weak on GCC/Clang so a missing definition still links.
/// Expands to nothing on other toolchains, where the symbol becomes required.
#if defined(__GNUC__) || defined(__clang__)
#define CATCHEM_WEAK_SYMBOL __attribute__((weak))
#else
#define CATCHEM_WEAK_SYMBOL
#endif

/// Fortran-side process registration entry points, one per bundled scheme.
/// Each is weak: a null function pointer means the scheme was not built in.
extern "C" {
void catchem_register_carbchem_cpp() CATCHEM_WEAK_SYMBOL;
void catchem_register_drydep_cpp() CATCHEM_WEAK_SYMBOL;
void catchem_register_dust_cpp() CATCHEM_WEAK_SYMBOL;
void catchem_register_seasalt_cpp() CATCHEM_WEAK_SYMBOL;
void catchem_register_settling_cpp() CATCHEM_WEAK_SYMBOL;
void catchem_register_so4chem_cpp() CATCHEM_WEAK_SYMBOL;
void catchem_register_wetdep_cpp() CATCHEM_WEAK_SYMBOL;
}

namespace catchem::api_internal {

    /**
     * @brief Project a BoundaryStatus onto the flat C integer status code.
     * @param status Internal boundary status.
     * @return Its numeric value, matching the CATCHEM_* constants one for one.
     */
    inline int status_code(BoundaryStatus status) {
        return static_cast<int>(status);
    }

    /**
     * @brief Admit an opaque handle for one operation, taking its lease.
     *
     * @param handle   Caller-supplied opaque pointer (Core, State or Manifest
     *                 handle). The registry, not the caller, decides validity.
     * @param type     Expected HandleType; a mismatch is rejected.
     * @param operation Stable operation name recorded in the boundary error.
     * @param admission Output lease the caller must hold for the whole time it
     *                 dereferences @p handle; it is moved into here by
     *                 BoundaryGuard::admit() and released at scope exit.
     * @return CATCHEM_SUCCESS, or CATCHEM_NULL_ARGUMENT, CATCHEM_INVALID_HANDLE
     *         or CATCHEM_WRONG_HANDLE_TYPE from HandleRegistry::acquire().
     *
     * @note The handle is only safe to use while the returned lease is alive;
     *       destroy paths block in close_and_wait() until every lease drains.
     */
    inline int admit_handle(void* handle, HandleType type, const char* operation, AdmissionLease& admission) {
        return status_code(BoundaryGuard::admit(handle, type, operation, admission));
    }

    /**
     * @brief Record a boundary failure and return its C status code.
     *
     * @param status  The BoundaryStatus being reported.
     * @param operation Stable operation name, e.g. "state_get_species_mw".
     * @param object  Name of the offending argument or field; may be null and
     *                is then recorded as the empty string.
     * @param cause   Human-readable reason, retrieved by catchem_get_last_error.
     * @return The integer form of @p status, so call sites can `return fail(...)`.
     */
    inline int fail(BoundaryStatus status, const char* operation, const char* object, const char* cause) {
        return status_code(set_boundary_error(status, operation, object ? object : "", cause));
    }

    /**
     * @brief Truncate-copy a string into a caller-owned C buffer.
     *
     * @param source    Value to copy.
     * @param buffer    Caller-owned destination; nothing happens when null.
     * @param max_length Capacity of @p buffer including the terminator; nothing
     *                   happens when it is zero or negative.
     *
     * @note Always NUL-terminates, and never writes past @p max_length - 1.
     */
    inline void copy_string_to_buffer(const std::string& source, char* buffer, int max_length) {
        if (!buffer || max_length <= 0)
            return;
        std::strncpy(buffer, source.c_str(), static_cast<std::size_t>(max_length - 1));
        buffer[max_length - 1] = '\0';
    }

    /**
     * @brief Register every bundled C++ process wrapper exactly once.
     *
     * @details Calls each weak Fortran registration symbol whose address is
     * non-null, so schemes absent from the build are skipped. Guarded by a
     * function-local std::once_flag: repeated calls (one per checked create
     * entry point) are free after the first, and the call is thread-safe.
     * @see catchem_core_create_from_config_checked(), catchem_core_add_process_by_name
     */
    inline void register_builtin_processes() {
        static std::once_flag registered;
        std::call_once(registered, [] {
            if (catchem_register_carbchem_cpp)
                catchem_register_carbchem_cpp();
            if (catchem_register_drydep_cpp)
                catchem_register_drydep_cpp();
            if (catchem_register_dust_cpp)
                catchem_register_dust_cpp();
            if (catchem_register_seasalt_cpp)
                catchem_register_seasalt_cpp();
            if (catchem_register_settling_cpp)
                catchem_register_settling_cpp();
            if (catchem_register_so4chem_cpp)
                catchem_register_so4chem_cpp();
            if (catchem_register_wetdep_cpp)
                catchem_register_wetdep_cpp();
        });
    }

} // namespace catchem::api_internal

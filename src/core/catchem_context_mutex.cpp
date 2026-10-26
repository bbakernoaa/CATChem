/**
 * @file catchem_context_mutex.cpp
 * @brief Process-wide lock guarding the CCPP context registry.
 *
 * @details The Fortran/CCPP side registers physics contexts by name and has no
 * thread safety of its own, so drivers serialise every registry touch through
 * these two C entry points. The mutex is anonymous-namespace static: this
 * translation unit owns the only instance, and lock()/unlock() are the only
 * ways to reach it.
 * @see catchem_context_registry_lock()/catchem_context_registry_unlock() are
 *      the BIND(C) pair used by the NUOPC and CCPP drivers.
 */
#include <mutex>

namespace {
    std::mutex context_registry_mutex; ///< Guards the Fortran-side context registry.
}

/**
 * @brief Take the context-registry lock for the calling thread.
 * @note Blocks until acquired; must always be paired with
 *       catchem_context_registry_unlock() on the same thread, including on
 *       error paths. Not reentrant: a second lock() on the same thread is
 *       undefined behaviour for std::mutex.
 */
extern "C" void catchem_context_registry_lock() {
    context_registry_mutex.lock();
}
/** @brief Release the lock taken by catchem_context_registry_lock(). */
extern "C" void catchem_context_registry_unlock() {
    context_registry_mutex.unlock();
}

#include <mutex>

namespace {
std::mutex context_registry_mutex;
}

extern "C" void catchem_context_registry_lock() { context_registry_mutex.lock(); }
extern "C" void catchem_context_registry_unlock() { context_registry_mutex.unlock(); }

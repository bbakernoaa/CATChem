/** @file ccpp_context_finalize_failure.cpp
 * @brief Test-only attachment of throwing finalizers to an initialized Core.
 */
#include "catchem_core.hpp"
#include "catchem_runtime_lease.hpp"
#include <memory>
#include <stdexcept>

namespace {
int finalized_count = 0;
/** @brief Empty process whose finalizer can report a deterministic error. */
class FinalizeProbe final : public catchem::ProcessInterface {
    int ordinal_;
public:
    /** @brief Configure the finalization order and failure identity. */
    explicit FinalizeProbe(int ordinal) : ordinal_(ordinal) {}
    /** @brief Return a unique name so both probes enter the same schedule. */
    std::string get_name() const override { return "finalize_probe_" + std::to_string(ordinal_); }
    /** @brief This process needs no state initialization. */
    void init(std::shared_ptr<catchem::StateManager>) override {}
    /** @brief This process performs no physical update. */
    void run(std::shared_ptr<catchem::StateManager>) override {}
    /** @brief Record cleanup and raise distinct errors to check first-error preservation. */
    void finalize() override {
        ++finalized_count;
        throw std::runtime_error("injected finalizer " + std::to_string(ordinal_));
    }
};
}
/** @brief Attach already initialized probes directly to a test context's Core. */
extern "C" int catchem_test_attach_finalize_failure(void* opaque_core) noexcept {
    try {
        auto& core = *static_cast<catchem::Core*>(opaque_core);
        finalized_count = 0;
        for (int ordinal : {1, 2}) {
            auto process = std::make_shared<FinalizeProbe>(ordinal);
            process->init(core.get_state_manager());
            core.add_process(process);
        }
        return 0;
    } catch (...) { return 1; }
}
/** @brief Return how many throwing finalizers completed their cleanup action. */
extern "C" int catchem_test_finalized_count() noexcept { return finalized_count; }
/** @brief Return live runtime owners so failed teardown can be checked for leaks. */
extern "C" int catchem_test_runtime_lease_count() noexcept {
    return static_cast<int>(catchem::RuntimeLease::snapshot().lease_count);
}

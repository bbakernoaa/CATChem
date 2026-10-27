#include "catchem_api.hpp"
#include "catchem_core.hpp"
#include <cassert>
#include <memory>
#include <stdexcept>
#include <string>

namespace {
    /** @brief Raise controlled failures to verify selected-operation recovery and clock restoration. */
    class ThrowingProcess final : public catchem::ProcessInterface {
    public:
        /** @brief Configure a failing process and optional expected execution duration in seconds. */
        ThrowingProcess(std::string name, catchem::RecoveryCapability recovery, bool unknown = false,
                        double expected_duration = 0.0)
            : name_(std::move(name)), recovery_(recovery), unknown_(unknown), expected_duration_(expected_duration) {}
        /** @brief Return the selected invocation identity. */
        std::string get_name() const override { return name_; }
        /** @brief Declare the recovery obligation used after a failed operation. */
        catchem::ProcessContract get_contract() const override {
            auto contract = catchem::make_contract(name_);
            contract.recovery = recovery_;
            return contract;
        }
        /** @brief Initialize the resource-free probe. */
        void init(std::shared_ptr<catchem::StateManager>) override {}
        /** @brief Check the selected duration before raising the configured failure. */
        void run(std::shared_ptr<catchem::StateManager> state) override {
            if (expected_duration_ > 0.0)
                assert(state->clock().timestep == expected_duration_);
            if (unknown_)
                throw 23;
            throw std::runtime_error("injected process failure");
        }
        /** @brief Finalize the resource-free probe. */
        void finalize() override {}

    private:
        std::string name_;
        catchem::RecoveryCapability recovery_;
        bool unknown_;
        double expected_duration_;
    };
} // namespace

/// @brief Verify failures report recovery obligations and never admit an implicit retry.
int main() {
    {
        catchem::Core core(1, 1, 1);
        core.add_process(
            std::make_shared<ThrowingProcess>("reimport", catchem::RecoveryCapability::RequiresReimport, false, 30.0));
        catchem::TimeState clock;
        clock.year = 2024;
        clock.month = 2;
        clock.day = 29;
        clock.doy = 60;
        clock.timestep = 60.0;
        core.begin_interval(11, clock);
        auto failed = core.run_selected_process({11, "column-1", 0, "reimport"}, 30.0);
        assert(failed.status == catchem::SelectedOperationStatus::Failed);
        assert(!failed.publication_eligible);
        assert(failed.cause == "injected process failure");
        assert(core.get_state_manager()->clock().timestep == 60.0);
        bool close_rejected = false;
        try {
            core.end_interval(11);
        } catch (const std::runtime_error&) {
            close_rejected = true;
        }
        assert(close_rejected);
        bool retry_rejected = false;
        try {
            core.begin_interval(12, clock);
        } catch (const std::runtime_error&) {
            retry_rejected = true;
        }
        assert(retry_rejected);
    }
    {
        catchem::Core core(1, 1, 1);
        core.add_process(
            std::make_shared<ThrowingProcess>("reinitialize", catchem::RecoveryCapability::RequiresReinitialize));
        bool failed = false;
        try {
            core.run_timestep(60.0);
        } catch (const std::runtime_error&) {
            failed = true;
        }
        assert(failed);
        assert(core.get_timestep_outcome().state == catchem::StateClassification::RequiresReinitialize);
        core.get_state_manager()->begin_import_generation();
        failed = false;
        std::string reinit_reason;
        try {
            core.run_timestep(60.0);
        } catch (const std::runtime_error& error) {
            failed = true;
            reinit_reason = error.what();
        }
        assert(failed);
        assert(reinit_reason.find("reinitialization") != std::string::npos);
    }
    {
        catchem::Core core(1, 1, 1);
        core.add_process(
            std::make_shared<ThrowingProcess>("unknown", catchem::RecoveryCapability::Reconstructible, true));
        bool failed = false;
        try {
            core.run_timestep(60.0);
        } catch (...) {
            failed = true;
        }
        assert(failed);
        assert(core.get_timestep_outcome().cause == "unknown non-standard exception");
        assert(core.get_timestep_outcome().state == catchem::StateClassification::RequiresReimport);
        core.get_state_manager()->begin_import_generation();
        failed = false;
        try {
            core.run_timestep(60.0);
        } catch (...) {
            failed = true;
        }
        assert(failed);
        assert(core.get_timestep_outcome().cause == "unknown non-standard exception");
    }
    {
        catchem::SelectedProcessLedger ledger({{"process", true, {}}});
        ledger.begin_interval(21, 60.0);
        int publications = 0;
        auto failed = ledger.run({21, "site", 0, "process"}, 30.0,
                                 [] { throw std::runtime_error("post-sync validation failed"); });
        if (failed.publication_eligible)
            ++publications;
        auto duplicate = ledger.run({21, "site", 0, "process"}, 30.0, [] {});
        if (duplicate.publication_eligible)
            ++publications;
        assert(failed.status == catchem::SelectedOperationStatus::Failed);
        assert(duplicate.status == catchem::SelectedOperationStatus::Rejected);
        assert(publications == 0);
        ledger.end_interval(21);
    }
    {
        catchem::Core core(1, 1, 1);
        core.add_process(std::make_shared<ThrowingProcess>("unknown_selected",
                                                           catchem::RecoveryCapability::RequiresReimport, true, 7.5));
        catchem::TimeState clock;
        clock.year = 2024;
        clock.month = 2;
        clock.day = 29;
        clock.doy = 60;
        clock.timestep = 45.0;
        core.begin_interval(31, clock);
        const auto failed = core.run_selected_process({31, "phys_ts", 1, "unknown_selected"}, 7.5);
        assert(failed.status == catchem::SelectedOperationStatus::Failed);
        assert(!failed.publication_eligible);
        assert(core.get_state_manager()->clock().timestep == 45.0);
    }
    return 0;
}

#include "catchem_core.hpp"
#include "catchem_execution_plan.hpp"
#include <cassert>
#include <cmath>
#include <memory>
#include <stdexcept>
#include <string>
#include <vector>

namespace {

/** @brief A minimal selected process that records the clock at each site call. */
class ClockProcess final : public catchem::ProcessInterface {
public:
    /** @brief Create a process with an execution ledger. */
    explicit ClockProcess(std::vector<std::string>& ledger) : ledger_(ledger) {}
    /** @brief Return the process name used in selected invocation keys. */
    std::string get_name() const override { return "clock_probe"; }
    /** @brief Declare an empty contract so the test isolates interval orchestration. */
    catchem::ProcessContract get_contract() const override { return catchem::make_contract("clock_probe"); }
    /** @brief Initialize the test process. */
    void init(std::shared_ptr<catchem::StateManager>) override {}
    /** @brief Record the current host physics clock and invocation boundary. */
    void run(std::shared_ptr<catchem::StateManager> state) override {
        const auto& clock = state->clock();
        ledger_.push_back(std::to_string(clock.year) + "-" + std::to_string(clock.month) + "-" +
                          std::to_string(clock.day) + "T" + std::to_string(clock.hour) + ":" +
                          std::to_string(clock.minute) + ":" + std::to_string(clock.second) + "/" +
                          std::to_string(clock.doy) + "/" + std::to_string(clock.timestep));
    }
    /** @brief Finalize the test process. */
    void finalize() override {}
private:
    std::vector<std::string>& ledger_;
};

}

/** @brief Check host-controlled interval identity, clock, repeated site, and diagnostic boundaries. */
int main() {
    std::vector<std::string> ledger;
    catchem::Core core(1, 1, 1);
    core.add_process(std::make_shared<ClockProcess>(ledger));
    auto state = core.get_state_manager();
    auto diagnostics = core.get_diagnostic_manager();
    diagnostics->register_field("split_probe", "split diagnostic", "1", catchem::DiagType::FIELD_2D, {1, 1});

    catchem::TimeState clock;
    clock.year = 2024;
    clock.month = 2;
    clock.day = 29;
    clock.hour = 23;
    clock.minute = 55;
    clock.second = 0;
    clock.timestep = 45.0;
    clock.calculate_derived_fields();
    core.begin_interval(101, clock);
    const auto interval_generation = diagnostics->generation();
    *static_cast<double*>(diagnostics->get_host_write_pointer("split_probe")) = 4.0;

    const catchem::SelectedInvocationKey first{101, "phys_ps", 1, "clock_probe"};
    const catchem::SelectedInvocationKey second{101, "phys_ps", 2, "clock_probe"};
    auto first_result = core.run_selected_process(first, 15.0);
    assert(first_result.status == catchem::SelectedOperationStatus::Succeeded);
    assert(state->clock().timestep == 45.0);
    state->begin_import_generation();
    auto repeated_init = [&] {
        // The CCPP context wrapper owns open idempotence; Core's checked
        // selected-dispatch API correctly accepts one open call per interval.
        diagnostics->begin_interval(101);
        diagnostics->begin_interval(101);
    };
    repeated_init();
    assert(diagnostics->generation() == interval_generation); // same identity does not reset diagnostics
    assert(std::abs(*static_cast<const double*>(diagnostics->get_host_read_pointer("split_probe")) - 4.0) < 1.0e-12);
    auto second_result = core.run_selected_process(second, 15.0);
    assert(second_result.status == catchem::SelectedOperationStatus::Succeeded);
    assert(ledger.size() == 2);
    assert(ledger.front() == "2024-2-29T23:55:0/60/15.000000");
    assert(ledger.front() == ledger.back()); // calendar stays fixed; bridges receive the admitted duration
    assert(state->clock().timestep == 45.0);

    bool duplicate_rejected = false;
    const auto duplicate = core.run_selected_process(first, 15.0);
    duplicate_rejected = duplicate.status == catchem::SelectedOperationStatus::Rejected;
    assert(duplicate_rejected);
    const auto fractional = core.run_selected_process({101, "phys_ts", 3, "clock_probe"}, 22.5);
    assert(fractional.status == catchem::SelectedOperationStatus::Succeeded);
    assert(ledger.back() == "2024-2-29T23:55:0/60/22.500000");
    assert(state->clock().timestep == 45.0);
    core.end_interval(101);

    clock.hour = 0;
    clock.minute = 0;
    clock.second = 0;
    clock.timestep = 30.0;
    clock.calculate_derived_fields();
    core.begin_interval(102, clock);
    assert(diagnostics->generation() == interval_generation + 1);
    assert(std::abs(*static_cast<const double*>(diagnostics->get_host_read_pointer("split_probe"))) < 1.0e-12);
    const catchem::SelectedInvocationKey third{102, "phys_ts", 1, "clock_probe"};
    auto third_result = core.run_selected_process(third, 30.0);
    assert(third_result.status == catchem::SelectedOperationStatus::Succeeded);
    assert(ledger.size() == 4);
    assert(ledger.back().find("2024-2-29T0:0:0/60/30.000000") != std::string::npos);

    catchem::TimeState invalid = clock;
    invalid.day = 30;
    invalid.doy = 61;
    bool invalid_rejected = false;
    try { core.begin_interval(103, invalid); }
    catch (const std::out_of_range&) { invalid_rejected = true; }
    assert(invalid_rejected);
    core.end_interval(102);
    core.shutdown();
    return 0;
}

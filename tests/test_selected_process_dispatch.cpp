#include "catchem_selected_dispatch.hpp"
#include "catchem_core.hpp"
#include "catchem_api.hpp"
#include "catchem_test_config.hpp"
#include <cassert>
#include <stdexcept>

using catchem::SelectedInvocationKey;
using catchem::SelectedOperationStatus;
using catchem::SelectedProcessLedger;
using catchem::SelectedProcessSpec;

extern "C" {
void catchem_register_carbchem_cpp();
void catchem_register_drydep_cpp();
void catchem_register_dust_cpp();
void catchem_register_seasalt_cpp();
void catchem_register_settling_cpp();
void catchem_register_so4chem_cpp();
void catchem_register_wetdep_cpp();
}

/// @brief Exercise selected-process ordering, interval, failure, and API contracts.
int main() {
    // Duplicate names keep legacy schedule construction valid while remaining
    // unavailable to ambiguous selected dispatch.
    SelectedProcessLedger ambiguous({{"legacy", true, {}}, {"legacy", true, {}}});
    ambiguous.begin_interval(1, 30.0);
    auto ambiguous_call = ambiguous.run({1, "site", 0, "legacy"}, 30.0, [] {});
    assert(ambiguous_call.status == SelectedOperationStatus::Rejected);
    assert(ambiguous_call.cause.find("ambiguous") != std::string::npos);
    ambiguous.end_interval(1);

    SelectedProcessLedger ledger({{"photolysis", true, {}}, {"gaschem", true, {"photolysis"}},
                                  {"disabled", false, {}}});
    bool photo_called = false, gas_called = false;
    ledger.begin_interval(1, 60.0);
    auto early = ledger.run({1, "gas", 0, "gaschem"}, 30.0, [&] { gas_called = true; });
    assert(early.status == SelectedOperationStatus::Rejected && !gas_called);
    int fallback_schedule_calls = 0;
    auto unavailable = ledger.run({1, "off", 0, "disabled"}, 30.0,
                                  [&] { ++fallback_schedule_calls; });
    assert(unavailable.status == SelectedOperationStatus::Rejected);
    assert(fallback_schedule_calls == 0);
    auto too_long = ledger.run({1, "long", 0, "photolysis"}, 61.0, [] {});
    assert(too_long.status == SelectedOperationStatus::Rejected);
    int admitted_calls = 0;
    auto producer = ledger.run({1, "photo", 0, "photolysis"}, 30.0, [&] { ++admitted_calls; photo_called = true; });
    assert(producer.status == SelectedOperationStatus::Succeeded && producer.publication_eligible);
    auto duplicate = ledger.run({1, "photo", 0, "photolysis"}, 30.0, [&] { ++admitted_calls; });
    assert(duplicate.status == SelectedOperationStatus::Rejected && admitted_calls == 1);
    auto consumer = ledger.run({1, "gas-after-photo", 0, "gaschem"}, 30.0, [&] { ++admitted_calls; gas_called = true; });
    assert(consumer.status == SelectedOperationStatus::Succeeded && gas_called && admitted_calls == 2);
    auto failure = ledger.run({1, "bad", 0, "photolysis"}, 30.0, [] { throw std::runtime_error("physics failed"); });
    assert(failure.status == SelectedOperationStatus::Failed && !failure.publication_eligible);
    assert(failure.cause == "physics failed");
    ledger.end_interval(1);
    assert(ledger.interval_count() == 1);
    ledger.begin_interval(2, 60.0);
    assert(ledger.interval_count() == 2 && ledger.invocation_count() == 0);
    auto new_interval_consumer = ledger.run({2, "gas", 0, "gaschem"}, 30.0, [] {});
    assert(new_interval_consumer.status == SelectedOperationStatus::Rejected);
    ledger.end_interval(2);
    bool invalid_duration = false;
    try { ledger.begin_interval(2, 10.0); }
    catch (const std::invalid_argument&) { invalid_duration = true; }
    assert(invalid_duration);
    assert(photo_called);

    const std::string config = std::string(catchem::test::SOURCE_DIR) + "/tests/fixtures/platform_integrity_valid.yml";
    const std::string contract_config = std::string(catchem::test::SOURCE_DIR) + "/tests/CATChem_nuopc_lifecycle_config.yml";
    catchem::Core core(config);
    catchem::TimeState interval_time;
    interval_time.year=2024; interval_time.month=2; interval_time.day=29; interval_time.doy=60;
    interval_time.timestep=60.0;
    core.begin_interval(101, interval_time);
    auto absent = core.run_selected_process({101, "site", 0, "not-configured"}, 30.0);
    assert(absent.status == SelectedOperationStatus::Rejected && !absent.publication_eligible);
    core.end_interval(101);
    assert(core.get_timestep_outcome().timestep == 1);

    void* api_core = nullptr;
    catchem_register_carbchem_cpp(); catchem_register_drydep_cpp(); catchem_register_dust_cpp();
    catchem_register_seasalt_cpp(); catchem_register_settling_cpp(); catchem_register_so4chem_cpp();
    catchem_register_wetdep_cpp();
    assert(catchem_core_create_from_config_checked(contract_config.c_str(), &api_core) == CATCHEM_SUCCESS);
    int configured_processes = 0;
    assert(catchem_core_get_num_processes_checked(api_core, &configured_processes) == CATCHEM_SUCCESS);
    assert(configured_processes > 0);
    catchem_process_contract_v1 process_contract{};
    process_contract.schema_version = 1;
    process_contract.struct_size = sizeof(process_contract);
    char process_name[128]{};
    assert(catchem_core_get_process_contract_checked(api_core, 0, &process_contract,
                                                      process_name, sizeof(process_name)) == CATCHEM_SUCCESS);
    assert(process_contract.process_name_required_len > 1 && process_contract.field_count > 0);
    catchem_process_field_v1 process_field{};
    process_field.schema_version = 1;
    process_field.struct_size = sizeof(process_field);
    char field_name[128]{}, field_units[64]{};
    assert(catchem_core_get_process_field_checked(api_core, 0, 0, &process_field, field_name,
                                                   sizeof(field_name), field_units, sizeof(field_units)) == CATCHEM_SUCCESS);
    assert(process_field.rank > 0 && process_field.name_required_len > 1 && process_field.units_required_len > 1);
    process_field = {};
    process_field.schema_version = 1;
    process_field.struct_size = sizeof(process_field);
    char short_name[1] = {'x'};
    assert(catchem_core_get_process_field_checked(api_core, 0, 0, &process_field, short_name,
                                                   sizeof(short_name), field_units, sizeof(field_units)) == CATCHEM_EXTENT_MISMATCH);
    assert(short_name[0] == 'x');
    void* state_handle = nullptr;
    assert(catchem_core_get_state_manager_checked(api_core, &state_handle) == CATCHEM_SUCCESS);
    assert(catchem_state_set_time_checked(state_handle, 2024, 2, 29, 12, 0, 0, 59, 60.0) == CATCHEM_CONTRACT_VIOLATION);
    assert(catchem_state_set_time_checked(state_handle, 2024, 2, 29, 12, 0, 0, 60, 60.0) == CATCHEM_SUCCESS);
    catchem_host_interval_v1 host_interval{};
    host_interval.schema_version=1; host_interval.struct_size=sizeof(host_interval); host_interval.interval_id=501;
    host_interval.year=2024; host_interval.month=2; host_interval.day=29; host_interval.day_of_year=60;
    host_interval.duration_seconds=60.0;
    host_interval.day_of_year=59;
    assert(catchem_core_begin_interval_checked(api_core, &host_interval) == CATCHEM_CONTRACT_VIOLATION);
    host_interval.day_of_year=60;
    assert(catchem_core_begin_interval_checked(api_core, &host_interval) == CATCHEM_SUCCESS);
    catchem_process_invocation_v1 invocation{};
    invocation.schema_version = 1;
    invocation.struct_size = sizeof(invocation);
    invocation.interval_id = 501;
    invocation.site = "missing-site";
    invocation.process = "missing-process";
    invocation.duration_seconds = 30.0;
    int operation_status = -1, publish = -1;
    uint64_t import_generation = UINT64_MAX;
    assert(catchem_core_run_process_checked(api_core, &invocation, &operation_status, &publish,
                                            &import_generation) == CATCHEM_CONTRACT_VIOLATION);
    assert(operation_status == static_cast<int>(SelectedOperationStatus::Rejected) && publish == 0);
    assert(catchem_core_end_interval_checked(api_core, 501) == CATCHEM_SUCCESS);
    assert(catchem_core_destroy_checked(api_core) == CATCHEM_SUCCESS);

    void* interface_core = nullptr;
    assert(catchem_core_create_checked(1, 1, 1, &interface_core) == CATCHEM_SUCCESS);
    void* interface_state = nullptr;
    assert(catchem_core_get_state_manager_checked(interface_core, &interface_state) == CATCHEM_SUCCESS);
    double interface_flux[2] = {0.0, 0.0};
    assert(catchem_state_bind_met_3d_checked(interface_state, "PFILSAN", interface_flux, 1, 2, 1) ==
           CATCHEM_SUCCESS);
    assert(catchem_state_bind_met_3d_checked(interface_state, "PFLLSAN", interface_flux, 1, 2, 1) ==
           CATCHEM_SUCCESS);
    assert(catchem_state_bind_met_3d_checked(interface_state, "PFILSAN", interface_flux, 1, 1, 1) ==
           CATCHEM_EXTENT_MISMATCH);
    assert(catchem_core_destroy_checked(interface_core) == CATCHEM_SUCCESS);
    return 0;
}

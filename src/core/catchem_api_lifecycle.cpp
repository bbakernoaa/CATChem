/**
 * @file catchem_api_lifecycle.cpp
 * @brief C entry points for Core create/destroy, state, and process handles.
 *
 * @details Implements the handle lifecycle of catchem_api.hpp: create/destroy
 * of Core, StateManager (registered as a Borrowed child of its Core), and
 * process admission by contract or name. destroy runs close_and_wait,
 * invalidate_children, and shutdown before delete, reporting ShutdownFailure
 * if any step throws but deleting the Core regardless. run_process_checked
 * maps TimestepOutcome/SelectedOperationOutcome values to status codes
 * (Failed to ProcessFailure, Rejected to ContractViolation). Member
 * documentation lives on catchem_api.hpp.
 */
#include "catchem_api.hpp"
#include "catchem_api_internal.hpp"
#include "catchem_core.hpp"
#include "catchem_diagnostic_manager.hpp"
#include "catchem_process_registry.hpp"
#include "catchem_state_manager.hpp"
#include "catchem_unit_conversion.hpp"
#include <algorithm>
#include <cmath>
#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>

using namespace catchem::api_internal;

extern "C" {
void* catchem_core_create(int nc, int nl, int ns) {
    void* core = nullptr;
    (void)catchem_core_create_checked(nc, nl, ns, &core);
    return core;
}

int catchem_core_create_checked(int nc, int nl, int ns, void** core_out) {
    catchem::clear_boundary_error();
    if (!core_out)
        return fail(catchem::BoundaryStatus::NullArgument, "core_create", "core_out", "output pointer is null");
    *core_out = nullptr;
    if (nc <= 0 || nl <= 0 || ns <= 0)
        return fail(catchem::BoundaryStatus::ExtentMismatch, "core_create", "dimensions",
                    "column, level, and species extents must be positive");
    try {
        auto* core = new catchem::Core(nc, nl, ns);
        if (!catchem::HandleRegistry::instance().add(
                core, {catchem::HandleType::Core, 1, 1, nullptr, catchem::HandleOwnership::Owned})) {
            delete core;
            return fail(catchem::BoundaryStatus::InternalError, "core_create", "registry",
                        "could not register core handle");
        }
        *core_out = core;
        return CATCHEM_SUCCESS;
    } catch (const std::exception& error) {
        return fail(catchem::BoundaryStatus::InternalError, "core_create", "core", error.what());
    } catch (...) {
        return fail(catchem::BoundaryStatus::InternalError, "core_create", "core", "unknown exception");
    }
}

int catchem_core_begin_interval_checked(void* core_ptr, const catchem_host_interval_v1* interval) {
    catchem::AdmissionLease admission;
    const int admitted = admit_handle(core_ptr, catchem::HandleType::Core, "core_begin_interval", admission);
    if (admitted != CATCHEM_SUCCESS)
        return admitted;
    if (!interval)
        return fail(catchem::BoundaryStatus::NullArgument, "core_begin_interval", "interval",
                    "interval descriptor is null");
    if (interval->schema_version != 1 || interval->struct_size < sizeof(catchem_host_interval_v1))
        return fail(catchem::BoundaryStatus::ContractViolation, "core_begin_interval", "interval",
                    "unsupported schema version or descriptor size");
    try {
        catchem::TimeState time;
        time.year = interval->year;
        time.month = interval->month;
        time.day = interval->day;
        time.hour = interval->hour;
        time.minute = interval->minute;
        time.second = interval->second;
        time.doy = interval->day_of_year;
        time.timestep = interval->duration_seconds;
        static_cast<catchem::Core*>(core_ptr)->begin_interval(interval->interval_id, time);
        return CATCHEM_SUCCESS;
    } catch (const std::exception& error) {
        return fail(catchem::BoundaryStatus::ContractViolation, "core_begin_interval", "interval", error.what());
    } catch (...) {
        return fail(catchem::BoundaryStatus::InternalError, "core_begin_interval", "interval", "unknown exception");
    }
}

int catchem_core_run_process_checked(void* core_ptr, const catchem_process_invocation_v1* invocation,
                                     int* operation_status, int* publication_eligible,
                                     uint64_t* import_generation_out) {
    catchem::AdmissionLease admission;
    const int admitted = admit_handle(core_ptr, catchem::HandleType::Core, "core_run_process", admission);
    if (admitted != CATCHEM_SUCCESS)
        return admitted;
    if (!invocation || !operation_status || !publication_eligible || !import_generation_out)
        return fail(catchem::BoundaryStatus::NullArgument, "core_run_process", "argument",
                    "invocation and result outputs are required");
    if (invocation->schema_version != 1 || invocation->struct_size < sizeof(catchem_process_invocation_v1) ||
        !invocation->site || !invocation->process || !std::isfinite(invocation->duration_seconds))
        return fail(catchem::BoundaryStatus::ContractViolation, "core_run_process", "invocation",
                    "invalid schema, size, identity, or duration");
    try {
        catchem::SelectedInvocationKey key{invocation->interval_id, invocation->site, invocation->subcycle,
                                           invocation->process};
        const auto outcome =
            static_cast<catchem::Core*>(core_ptr)->run_selected_process(key, invocation->duration_seconds);
        *operation_status = static_cast<int>(outcome.status);
        *publication_eligible = outcome.publication_eligible ? 1 : 0;
        *import_generation_out = outcome.import_generation;
        if (outcome.status == catchem::SelectedOperationStatus::Succeeded)
            return CATCHEM_SUCCESS;
        if (outcome.status == catchem::SelectedOperationStatus::Failed)
            return fail(catchem::BoundaryStatus::ProcessFailure, "core_run_process", invocation->process,
                        outcome.cause.c_str());
        return fail(catchem::BoundaryStatus::ContractViolation, "core_run_process", invocation->process,
                    outcome.cause.c_str());
    } catch (const std::exception& error) {
        return fail(catchem::BoundaryStatus::InternalError, "core_run_process", invocation->process, error.what());
    } catch (...) {
        return fail(catchem::BoundaryStatus::InternalError, "core_run_process", invocation->process,
                    "unknown exception");
    }
}

int catchem_core_end_interval_checked(void* core_ptr, uint64_t interval_id) {
    catchem::AdmissionLease admission;
    const int admitted = admit_handle(core_ptr, catchem::HandleType::Core, "core_end_interval", admission);
    if (admitted != CATCHEM_SUCCESS)
        return admitted;
    try {
        static_cast<catchem::Core*>(core_ptr)->end_interval(interval_id);
        return CATCHEM_SUCCESS;
    } catch (const std::exception& error) {
        return fail(catchem::BoundaryStatus::ContractViolation, "core_end_interval", "interval", error.what());
    } catch (...) {
        return fail(catchem::BoundaryStatus::InternalError, "core_end_interval", "interval", "unknown exception");
    }
}

int catchem_core_get_process_contract_checked(void* core_ptr, int process_index,
                                              catchem_process_contract_v1* descriptor, char* process_name,
                                              int process_name_len) {
    catchem::AdmissionLease admission;
    const int admitted = admit_handle(core_ptr, catchem::HandleType::Core, "core_get_process_contract", admission);
    if (admitted != CATCHEM_SUCCESS)
        return admitted;
    if (!descriptor || !process_name)
        return fail(catchem::BoundaryStatus::NullArgument, "core_get_process_contract", "argument",
                    "descriptor and process-name buffer are required");
    if (descriptor->schema_version != 1 || descriptor->struct_size < sizeof(*descriptor))
        return fail(catchem::BoundaryStatus::ContractViolation, "core_get_process_contract", "descriptor",
                    "unsupported schema version or descriptor size");
    auto* core = static_cast<catchem::Core*>(core_ptr);
    if (process_index < 0 || static_cast<std::size_t>(process_index) >= core->get_num_processes())
        return fail(catchem::BoundaryStatus::InvalidIndex, "core_get_process_contract", "process_index",
                    "process index is out of range");
    const auto& contract = core->get_process_contract(static_cast<std::size_t>(process_index));
    descriptor->process_name_required_len = static_cast<int>(contract.process_name.size() + 1);
    descriptor->field_count = static_cast<int>(contract.fields.size());
    descriptor->diagnostic_count = static_cast<int>(contract.diagnostics.size());
    descriptor->product_requirement_count = static_cast<int>(contract.products.size());
    descriptor->produced_product_count = static_cast<int>(contract.produced_products.size());
    descriptor->recovery_capability = static_cast<uint32_t>(contract.recovery);
    if (process_name_len < descriptor->process_name_required_len || process_name_len <= 0)
        return fail(catchem::BoundaryStatus::ExtentMismatch, "core_get_process_contract", "process_name",
                    "buffer is too short; descriptor strings were not copied");
    copy_string_to_buffer(contract.process_name, process_name, process_name_len);
    return CATCHEM_SUCCESS;
}

int catchem_core_get_process_field_checked(void* core_ptr, int process_index, int field_index,
                                           catchem_process_field_v1* descriptor, char* field_name, int field_name_len,
                                           char* units, int units_len) {
    catchem::AdmissionLease admission;
    const int admitted = admit_handle(core_ptr, catchem::HandleType::Core, "core_get_process_field", admission);
    if (admitted != CATCHEM_SUCCESS)
        return admitted;
    if (!descriptor || !field_name || !units)
        return fail(catchem::BoundaryStatus::NullArgument, "core_get_process_field", "argument",
                    "descriptor, name, and units buffers are required");
    if (descriptor->schema_version != 1 || descriptor->struct_size < sizeof(*descriptor))
        return fail(catchem::BoundaryStatus::ContractViolation, "core_get_process_field", "descriptor",
                    "unsupported schema version or descriptor size");
    auto* core = static_cast<catchem::Core*>(core_ptr);
    if (process_index < 0 || static_cast<std::size_t>(process_index) >= core->get_num_processes())
        return fail(catchem::BoundaryStatus::InvalidIndex, "core_get_process_field", "process_index",
                    "process index is out of range");
    const auto& contract = core->get_process_contract(static_cast<std::size_t>(process_index));
    if (field_index < 0 || static_cast<std::size_t>(field_index) >= contract.fields.size())
        return fail(catchem::BoundaryStatus::InvalidIndex, "core_get_process_field", "field_index",
                    "field index is out of range");
    const auto& field = contract.fields[static_cast<std::size_t>(field_index)];
    descriptor->rank = static_cast<int>(field.axes.size());
    if (descriptor->rank > 4)
        return fail(catchem::BoundaryStatus::ContractViolation, "core_get_process_field", "rank",
                    "field rank exceeds the v1 descriptor capacity");
    for (int i = 0; i < 4; ++i)
        descriptor->axes[i] = i < descriptor->rank ? static_cast<int>(field.axes[i]) : -1;
    descriptor->persistence = static_cast<int>(field.persistence);
    descriptor->requirement = static_cast<int>(field.requirement);
    descriptor->access = static_cast<int>(field.access);
    descriptor->execution_space = static_cast<int>(field.execution_space);
    descriptor->produced = field.produced ? 1 : 0;
    descriptor->name_required_len = static_cast<int>(field.canonical_name.size() + 1);
    descriptor->units_required_len = static_cast<int>(field.units.size() + 1);
    if (field_name_len < descriptor->name_required_len || units_len < descriptor->units_required_len ||
        field_name_len <= 0 || units_len <= 0)
        return fail(catchem::BoundaryStatus::ExtentMismatch, "core_get_process_field", "buffer",
                    "one or more buffers are too short; no field strings were copied");
    copy_string_to_buffer(field.canonical_name, field_name, field_name_len);
    copy_string_to_buffer(field.units, units, units_len);
    return CATCHEM_SUCCESS;
}

void* catchem_core_create_from_config(const char* config_file) {
    void* core = nullptr;
    (void)catchem_core_create_from_config_checked(config_file, &core);
    return core;
}

int catchem_core_create_from_config_checked(const char* config_file, void** core_out) {
    catchem::clear_boundary_error();
    if (core_out)
        *core_out = nullptr;
    if (!config_file || !core_out)
        return fail(catchem::BoundaryStatus::NullArgument, "core_create_from_config", "argument",
                    "configuration path and output pointer are required");
    try {
        register_builtin_processes();
        auto* core = new catchem::Core(config_file);
        catchem::HandleRegistry::instance().add(
            core, {catchem::HandleType::Core, 1, 1, nullptr, catchem::HandleOwnership::Owned});
        *core_out = core;
        return CATCHEM_SUCCESS;
    } catch (const std::exception& error) {
        return fail(catchem::BoundaryStatus::InvalidState, "core_create_from_config", config_file, error.what());
    } catch (...) {
        return fail(catchem::BoundaryStatus::InternalError, "core_create_from_config", config_file,
                    "unknown exception");
    }
}

void* catchem_core_create_from_config_with_grid(const char* config_file, int ncols, int nlevels) {
    void* core = nullptr;
    (void)catchem_core_create_from_config_with_grid_checked(config_file, ncols, nlevels, &core);
    return core;
}

int catchem_core_create_from_config_with_grid_checked(const char* config_file, int ncols, int nlevels,
                                                      void** core_out) {
    catchem::clear_boundary_error();
    if (core_out)
        *core_out = nullptr;
    if (!config_file || !core_out)
        return fail(catchem::BoundaryStatus::NullArgument, "core_create_from_config_with_grid", "argument",
                    "configuration path and output pointer are required");
    if (ncols <= 0 || nlevels <= 0)
        return fail(catchem::BoundaryStatus::ExtentMismatch, "core_create_from_config_with_grid", "grid",
                    "column and level extents must be positive");
    try {
        register_builtin_processes();
        auto* core = new catchem::Core(config_file, ncols, nlevels);
        catchem::HandleRegistry::instance().add(
            core, {catchem::HandleType::Core, 1, 1, nullptr, catchem::HandleOwnership::Owned});
        *core_out = core;
        return CATCHEM_SUCCESS;
    } catch (const std::exception& error) {
        return fail(catchem::BoundaryStatus::InvalidState, "core_create_from_config_with_grid", config_file,
                    error.what());
    } catch (...) {
        return fail(catchem::BoundaryStatus::InternalError, "core_create_from_config_with_grid", config_file,
                    "unknown exception");
    }
}

void catchem_core_destroy(void* core_ptr) {
    (void)catchem_core_destroy_checked(core_ptr);
}

int catchem_core_destroy_checked(void* core_ptr) {
    catchem::clear_boundary_error();
    std::cerr << "core destroy trace: handle close begin\n";
    const auto status = catchem::HandleRegistry::instance().close_and_wait(core_ptr, catchem::HandleType::Core);
    std::cerr << "core destroy trace: handle close complete\n";
    if (status != catchem::BoundaryStatus::Success)
        return fail(status, "core_destroy", "core", "null, stale, destroyed, or wrong-type handle");
    std::cerr << "core destroy trace: child invalidation begin\n";
    catchem::HandleRegistry::instance().invalidate_children(core_ptr);
    std::cerr << "core destroy trace: child invalidation complete\n";
    auto* core = static_cast<catchem::Core*>(core_ptr);
    try {
        std::cerr << "core destroy trace: shutdown begin\n";
        core->shutdown();
        std::cerr << "core destroy trace: shutdown complete\n";
        std::cerr << "core destroy trace: delete begin\n";
        delete core;
        std::cerr << "core destroy trace: delete complete\n";
        return CATCHEM_SUCCESS;
    } catch (const std::exception& error) {
        const std::string cause = error.what();
        std::cerr << "core destroy trace: delete after shutdown exception\n";
        delete core;
        return fail(catchem::BoundaryStatus::ShutdownFailure, "core_destroy", "process cleanup", cause.c_str());
    } catch (...) {
        std::cerr << "core destroy trace: delete after unknown exception\n";
        delete core;
        return fail(catchem::BoundaryStatus::ShutdownFailure, "core_destroy", "process cleanup",
                    "unknown cleanup failure");
    }
}

void* catchem_core_get_state_manager(void* core_ptr) {
    void* state = nullptr;
    (void)catchem_core_get_state_manager_checked(core_ptr, &state);
    return state;
}

int catchem_core_get_state_manager_checked(void* core_ptr, void** state_out) {
    catchem::clear_boundary_error();
    if (!state_out)
        return fail(catchem::BoundaryStatus::NullArgument, "core_get_state", "state_out", "output pointer is null");
    *state_out = nullptr;
    catchem::AdmissionLease admission;
    const int status = admit_handle(core_ptr, catchem::HandleType::Core, "core_get_state", admission);
    if (status != CATCHEM_SUCCESS)
        return status;
    auto* core = static_cast<catchem::Core*>(core_ptr);
    auto* state = core->get_state_manager().get();
    const auto existing = catchem::HandleRegistry::instance().validate(state, catchem::HandleType::State, core_ptr);
    if (existing != catchem::BoundaryStatus::Success) {
        catchem::HandleRegistry::instance().remove(state);
        catchem::HandleRegistry::instance().add(
            state, {catchem::HandleType::State, 1, 1, core_ptr, catchem::HandleOwnership::Borrowed});
    }
    *state_out = state;
    return CATCHEM_SUCCESS;
}

int catchem_get_last_error(char* buffer, int max_len) {
    if (!buffer || max_len <= 0)
        return CATCHEM_NULL_ARGUMENT;
    const auto& error = catchem::last_boundary_error();
    std::string message = error.operation;
    if (!error.object.empty())
        message += " [" + error.object + "]";
    if (!error.cause.empty())
        message += ": " + error.cause;
    copy_string_to_buffer(message, buffer, max_len);
    return status_code(error.status);
}

int catchem_core_run_timestep(void* core_ptr, double dt) {
    catchem::AdmissionLease admission;
    const int handle_status = admit_handle(core_ptr, catchem::HandleType::Core, "core_run_timestep", admission);
    if (handle_status != CATCHEM_SUCCESS)
        return handle_status;
    try {
        auto* core = static_cast<catchem::Core*>(core_ptr);
        core->run_timestep(dt);
        return 0;
    } catch (const std::exception& error) {
        return fail(catchem::BoundaryStatus::InvalidState, "core_run_timestep", "timestep", error.what());
    } catch (...) {
        return fail(catchem::BoundaryStatus::InternalError, "core_run_timestep", "timestep", "unknown exception");
    }
}

int catchem_core_get_timestep_outcome(void* core_ptr, int* status, long long* timestep, double* duration,
                                      long long* import_generation, int* process_index, int* state_classification,
                                      char* process_name, int process_name_len, char* cause, int cause_len) {
    if (!status || !timestep || !duration || !import_generation || !process_index || !state_classification ||
        !process_name || !cause)
        return CATCHEM_NULL_ARGUMENT;
    catchem::AdmissionLease admission;
    const int handle_status = admit_handle(core_ptr, catchem::HandleType::Core, "core_get_timestep_outcome", admission);
    if (handle_status != CATCHEM_SUCCESS)
        return handle_status;
    const auto& outcome = static_cast<catchem::Core*>(core_ptr)->get_timestep_outcome();
    *status = static_cast<int>(outcome.status);
    *timestep = static_cast<long long>(outcome.timestep);
    *duration = outcome.duration;
    *import_generation = static_cast<long long>(outcome.import_generation);
    *process_index = static_cast<int>(outcome.process_index);
    *state_classification = static_cast<int>(outcome.state);
    copy_string_to_buffer(outcome.process_name, process_name, process_name_len);
    copy_string_to_buffer(outcome.cause, cause, cause_len);
    return CATCHEM_SUCCESS;
}

void catchem_core_add_process_by_name(void* core_ptr, const char* name) {
    register_builtin_processes();
    auto* core = static_cast<catchem::Core*>(core_ptr);
    auto process = catchem::ProcessRegistry::get_instance().create(name);
    process->init(core->get_state_manager());
    core->add_process(process);
}

int catchem_core_get_num_processes(void* core_ptr) {
    int count = 0;
    (void)catchem_core_get_num_processes_checked(core_ptr, &count);
    return count;
}

int catchem_core_get_num_processes_checked(void* core_ptr, int* count_out) {
    if (!count_out)
        return fail(catchem::BoundaryStatus::NullArgument, "core_get_num_processes", "count_out",
                    "output pointer is null");
    *count_out = 0;
    catchem::AdmissionLease admission;
    const int status = admit_handle(core_ptr, catchem::HandleType::Core, "core_get_num_processes", admission);
    if (status != CATCHEM_SUCCESS)
        return status;
    *count_out = static_cast<int>(static_cast<catchem::Core*>(core_ptr)->get_num_processes());
    return CATCHEM_SUCCESS;
}

int catchem_core_get_required_host_field_count_checked(void* core_ptr, int* count_out) {
    if (!count_out)
        return fail(catchem::BoundaryStatus::NullArgument, "core_get_required_host_field_count", "count_out",
                    "output pointer is null");
    *count_out = 0;
    catchem::AdmissionLease admission;
    const int status =
        admit_handle(core_ptr, catchem::HandleType::Core, "core_get_required_host_field_count", admission);
    if (status != CATCHEM_SUCCESS)
        return status;
    *count_out = static_cast<int>(static_cast<catchem::Core*>(core_ptr)->get_required_host_fields().size());
    return CATCHEM_SUCCESS;
}

int catchem_core_get_required_host_field_name_checked(void* core_ptr, int index, char* name_out, int name_out_len) {
    if (!name_out || name_out_len <= 0)
        return fail(catchem::BoundaryStatus::NullArgument, "core_get_required_host_field_name", "name_out",
                    "a non-empty output buffer is required");
    name_out[0] = '\0';
    catchem::AdmissionLease admission;
    const int status =
        admit_handle(core_ptr, catchem::HandleType::Core, "core_get_required_host_field_name", admission);
    if (status != CATCHEM_SUCCESS)
        return status;
    const auto fields = static_cast<catchem::Core*>(core_ptr)->get_required_host_fields();
    if (index < 0 || static_cast<std::size_t>(index) >= fields.size())
        return fail(catchem::BoundaryStatus::InvalidIndex, "core_get_required_host_field_name", "index",
                    "required host field index is out of range");
    copy_string_to_buffer(fields[static_cast<std::size_t>(index)], name_out, name_out_len);
    return CATCHEM_SUCCESS;
}
}

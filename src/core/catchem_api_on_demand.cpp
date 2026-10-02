// On-demand aerosol diagnostics API endpoints (flat BIND(C) surface).
//
// The enumeration trio (count / name / units) exposes the service's normative
// quantity registry so a host can discover the catalogue before requesting
// values.  Like every endpoint here it admits the opaque Core handle through
// the BoundaryGuard, never throws across the C boundary, and names the
// offending input plus the remedy in the last-error message.  With the
// aerosol utility not built into this library, the count endpoint reports
// zero and CATCHEM_QUANTITY_NOT_AVAILABLE so a host can skip the feature
// instead of discovering it per call.
#include "catchem_api.hpp"
#include "catchem_api_internal.hpp"
#include "catchem_core.hpp"

#include <cstring>
#include <string>
#include <vector>

using namespace catchem::api_internal;

extern "C" {

int catchem_ondemand_quantity_count_checked(void* core_ptr, int* count_out) {
    if (count_out)
        *count_out = 0;
    if (!count_out)
        return fail(catchem::BoundaryStatus::NullArgument, "ondemand_quantity_count", "count_out",
                    "an integer output is required");
    catchem::AdmissionLease admission;
    const int status = admit_handle(core_ptr, catchem::HandleType::Core, "ondemand_quantity_count", admission);
    if (status != CATCHEM_SUCCESS)
        return status;
    try {
        if (!catchem::OnDemandService::available()) {
            return fail(catchem::BoundaryStatus::QuantityNotAvailable, "ondemand_quantity_count", "on_demand",
                        "the aerosol utility is not built into this CATChem library; reconfigure with "
                        "-DCATCHEM_BUILD_EXAERO=ON");
        }
        *count_out = catchem::OnDemandService::quantity_count();
        return CATCHEM_SUCCESS;
    } catch (...) {
        return fail(catchem::BoundaryStatus::InternalError, "ondemand_quantity_count", "core",
                    "unexpected failure enumerating on-demand quantities");
    }
}

void catchem_ondemand_quantity_count(void* core_ptr, int* count_out) {
    (void)catchem_ondemand_quantity_count_checked(core_ptr, count_out);
}

int catchem_ondemand_quantity_name_at_checked(void* core_ptr, int index, char* name_out, int name_length) {
    if (name_out && name_length > 0)
        name_out[0] = '\0';
    if (!name_out || name_length <= 0)
        return fail(catchem::BoundaryStatus::NullArgument, "ondemand_quantity_name", "name_out",
                    "a positive-length name output is required");
    catchem::AdmissionLease admission;
    const int status = admit_handle(core_ptr, catchem::HandleType::Core, "ondemand_quantity_name", admission);
    if (status != CATCHEM_SUCCESS)
        return status;
    try {
        if (!catchem::OnDemandService::available()) {
            return fail(catchem::BoundaryStatus::QuantityNotAvailable, "ondemand_quantity_name", "on_demand",
                        "the aerosol utility is not built into this CATChem library; reconfigure with "
                        "-DCATCHEM_BUILD_EXAERO=ON");
        }
        const char* name = catchem::OnDemandService::quantity_name_at(index);
        if (name == nullptr) {
            return fail(catchem::BoundaryStatus::InvalidIndex, "ondemand_quantity_name", "index",
                        "quantity index is outside the catalogue; query the count first");
        }
        copy_string_to_buffer(name, name_out, name_length);
        return CATCHEM_SUCCESS;
    } catch (...) {
        return fail(catchem::BoundaryStatus::InternalError, "ondemand_quantity_name", "core",
                    "unexpected failure naming an on-demand quantity");
    }
}

void catchem_ondemand_quantity_name_at(void* core_ptr, int index, char* name_out) {
    (void)catchem_ondemand_quantity_name_at_checked(core_ptr, index, name_out, 64);
}

int catchem_ondemand_quantity_units_checked(void* core_ptr, const char* name, char* units_out, int units_length) {
    if (units_out && units_length > 0)
        units_out[0] = '\0';
    if (!name || !units_out || units_length <= 0)
        return fail(catchem::BoundaryStatus::NullArgument, "ondemand_quantity_units", "name",
                    "quantity name and a positive-length units output are required");
    catchem::AdmissionLease admission;
    const int status = admit_handle(core_ptr, catchem::HandleType::Core, "ondemand_quantity_units", admission);
    if (status != CATCHEM_SUCCESS)
        return status;
    try {
        if (!catchem::OnDemandService::available()) {
            return fail(catchem::BoundaryStatus::QuantityNotAvailable, "ondemand_quantity_units", "on_demand",
                        "the aerosol utility is not built into this CATChem library; reconfigure with "
                        "-DCATCHEM_BUILD_EXAERO=ON");
        }
        const int count = catchem::OnDemandService::quantity_count();
        for (int i = 0; i < count; ++i) {
            const char* candidate = catchem::OnDemandService::quantity_name_at(i);
            if (candidate && std::string(candidate) == name) {
                copy_string_to_buffer(catchem::OnDemandService::quantity_units_at(i), units_out, units_length);
                return CATCHEM_SUCCESS;
            }
        }
        return fail(catchem::BoundaryStatus::InvalidIndex, "ondemand_quantity_units", name,
                    "unknown on-demand quantity; enumerate the catalogue first");
    } catch (...) {
        return fail(catchem::BoundaryStatus::InternalError, "ondemand_quantity_units", "core",
                    "unexpected failure reading on-demand units");
    }
}

void catchem_ondemand_quantity_units(void* core_ptr, const char* name, char* units_out) {
    (void)catchem_ondemand_quantity_units_checked(core_ptr, name, units_out, 64);
}

int catchem_ondemand_get_pm_checked(void* core_ptr, const char* const* quantity_names, int num_names,
                                    double* values_out, int* status_out, int values_length, int status_length) {
    if (values_out && values_length > 0)
        std::memset(values_out, 0, static_cast<std::size_t>(values_length) * sizeof(double));
    if (status_out && status_length > 0)
        std::memset(status_out, 0, static_cast<std::size_t>(status_length) * sizeof(int));
    if (!quantity_names || num_names <= 0)
        return fail(catchem::BoundaryStatus::NullArgument, "ondemand_get_pm", "quantity_names",
                    "at least one quantity name is required");
    if (!values_out || values_length <= 0)
        return fail(catchem::BoundaryStatus::NullArgument, "ondemand_get_pm", "values_out",
                    "a positive-length values output is required");
    if (status_out && status_length < values_length)
        return fail(catchem::BoundaryStatus::ExtentMismatch, "ondemand_get_pm", "status_out",
                    "the status buffer must cover every value slot");
    catchem::AdmissionLease admission;
    const int status = admit_handle(core_ptr, catchem::HandleType::Core, "ondemand_get_pm", admission);
    if (status != CATCHEM_SUCCESS)
        return status;
    try {
        auto* core = static_cast<catchem::Core*>(core_ptr);
        if (!catchem::OnDemandService::available()) {
            return fail(catchem::BoundaryStatus::QuantityNotAvailable, "ondemand_get_pm", "on_demand",
                        "the aerosol utility is not built into this CATChem library; reconfigure with "
                        "-DCATCHEM_BUILD_EXAERO=ON");
        }
        auto& service = core->on_demand_service();
        if (!service.configured()) {
            return fail(catchem::BoundaryStatus::QuantityNotAvailable, "ondemand_get_pm", "on_demand",
                        "the on-demand utility is not configured; set diagnostics/on_demand/enabled and "
                        "diagnostics/on_demand/exaero_config");
        }
        auto state = core->get_state_manager();
        if (!state) {
            return fail(catchem::BoundaryStatus::InvalidState, "ondemand_get_pm", "state",
                        "the core has no state to read");
        }
        std::vector<std::string> names;
        names.reserve(static_cast<std::size_t>(num_names));
        for (int i = 0; i < num_names; ++i) {
            if (!quantity_names[i]) {
                const std::string cause =
                    "a null quantity name at index " + std::to_string(i) + " is not allowed";
                return fail(catchem::BoundaryStatus::NullArgument, "ondemand_get_pm", "quantity_names", cause.c_str());
            }
            names.emplace_back(quantity_names[i]);
        }
        // Every diagnostic-family quantity spans the full (cell, level) grid
        // per name in the name-major layout, so the extent is exact.
        const std::size_t block = static_cast<std::size_t>(state->column_count()) *
                                  static_cast<std::size_t>(state->level_count());
        const std::size_t needed = static_cast<std::size_t>(num_names) * block;
        if (static_cast<std::size_t>(values_length) < needed) {
            const std::string cause = "the values buffer holds " + std::to_string(values_length) + " entries; " +
                                      std::to_string(num_names) + " names x " + std::to_string(block) +
                                      " grid points need " + std::to_string(needed);
            return fail(catchem::BoundaryStatus::ExtentMismatch, "ondemand_get_pm", "values_out", cause.c_str());
        }
        std::string error;
        const int rc = service.read_pm_bulk(names, *state, values_out, status_out,
                                            static_cast<std::size_t>(values_length), error);
        if (rc != CATCHEM_SUCCESS) {
            return fail(static_cast<catchem::BoundaryStatus>(rc), "ondemand_get_pm", "on_demand", error.c_str());
        }
        return CATCHEM_SUCCESS;
    } catch (...) {
        return fail(catchem::BoundaryStatus::InternalError, "ondemand_get_pm", "core",
                    "unexpected failure reading on-demand particulate matter");
    }
}

void catchem_ondemand_get_pm(void* core_ptr, const char* const* quantity_names, int num_names, double* values_out,
                             int* status_out, int values_length, int status_length) {
    (void)catchem_ondemand_get_pm_checked(core_ptr, quantity_names, num_names, values_out, status_out, values_length,
                                          status_length);
}

}  // extern "C"

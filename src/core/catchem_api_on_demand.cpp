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
#include <memory>
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

int catchem_ondemand_quantity_is_columnar_checked(void* core_ptr, const char* name, int* is_columnar_out) {
    if (is_columnar_out)
        *is_columnar_out = 0;
    if (!name || !is_columnar_out)
        return fail(catchem::BoundaryStatus::NullArgument, "ondemand_quantity_is_columnar", "name",
                    "a quantity name and an integer output are required");
    catchem::AdmissionLease admission;
    const int status = admit_handle(core_ptr, catchem::HandleType::Core, "ondemand_quantity_is_columnar", admission);
    if (status != CATCHEM_SUCCESS)
        return status;
    try {
        if (!catchem::OnDemandService::available()) {
            return fail(catchem::BoundaryStatus::QuantityNotAvailable, "ondemand_quantity_is_columnar", "on_demand",
                        "the aerosol utility is not built into this CATChem library; reconfigure with "
                        "-DCATCHEM_BUILD_EXAERO=ON");
        }
        const catchem::QuantityInfo* info = catchem::OnDemandService::find_quantity(name);
        if (info == nullptr) {
            const std::string why = std::string("'") + name + "' is not a known on-demand quantity";
            return fail(catchem::BoundaryStatus::InvalidIndex, "ondemand_quantity_is_columnar", name, why.c_str());
        }
        // A columnar (2D) quantity occupies one point per cell; every other
        // kind occupies one per (cell, level).  This is a static catalogue
        // fact, so the query needs no bound state.
        *is_columnar_out = (info->kind == catchem::OnDemandKind::Diagnostic2D ||
                            info->kind == catchem::OnDemandKind::Optical2D)
                               ? 1
                               : 0;
        return CATCHEM_SUCCESS;
    } catch (...) {
        return fail(catchem::BoundaryStatus::InternalError, "ondemand_quantity_is_columnar", "core",
                    "unexpected failure reading on-demand quantity shape");
    }
}

void catchem_ondemand_quantity_is_columnar(void* core_ptr, const char* name, int* is_columnar_out) {
    (void)catchem_ondemand_quantity_is_columnar_checked(core_ptr, name, is_columnar_out);
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

// Shared preconditions for the value-returning endpoints: gate, configured
// utility, and a bound state.  Returns CATCHEM_SUCCESS or a boundary status,
// filling 'service'/'state' on success.  'operation' names the endpoint in
// any failure message.
namespace {
int admit_service(void* core_ptr, const char* operation, catchem::OnDemandService*& service,
                  std::shared_ptr<catchem::StateManager>& state, const char*& object, const char*& cause) {
    object = cause = nullptr;
    service = nullptr;
    if (!catchem::OnDemandService::available()) {
        object = "on_demand";
        cause = "the aerosol utility is not built into this CATChem library; reconfigure with "
                "-DCATCHEM_BUILD_EXAERO=ON";
        (void)operation;
        return CATCHEM_QUANTITY_NOT_AVAILABLE;
    }
    auto* core = static_cast<catchem::Core*>(core_ptr);
    service = &core->on_demand_service();
    if (!service->configured()) {
        object = "on_demand";
        cause = "the on-demand utility is not configured; set diagnostics/on_demand/enabled and "
                "diagnostics/on_demand/exaero_config";
        return CATCHEM_QUANTITY_NOT_AVAILABLE;
    }
    state = core->get_state_manager();
    if (!state) {
        object = "state";
        cause = "the core has no state to read";
        return CATCHEM_INVALID_STATE;
    }
    return CATCHEM_SUCCESS;
}

bool is_optics_kind(catchem::OnDemandKind kind) {
    return kind == catchem::OnDemandKind::Optical3D || kind == catchem::OnDemandKind::Optical2D ||
           kind == catchem::OnDemandKind::OpticalSsa;
}

bool is_diagnostic_kind(catchem::OnDemandKind kind) {
    return kind == catchem::OnDemandKind::Diagnostic3D || kind == catchem::OnDemandKind::Diagnostic2D;
}

// Points a single optics name occupies for 'bands' wavelengths.
std::size_t optics_points(const catchem::QuantityInfo& info, std::size_t bands, std::size_t cells,
                          std::size_t levels) {
    return (info.kind == catchem::OnDemandKind::Optical2D) ? bands * cells : bands * cells * levels;
}

// Points a single diagnostic name occupies.
std::size_t diagnostic_points(const catchem::QuantityInfo& info, std::size_t cells, std::size_t levels) {
    return (info.kind == catchem::OnDemandKind::Diagnostic2D) ? cells : cells * levels;
}
}  // namespace

int catchem_ondemand_get_optics_checked(void* core_ptr, const char* const* quantity_names, int num_names,
                                        const double* wavelengths_m, int num_wavelengths, double* values_out,
                                        int* status_out, int values_length, int status_length) {
    if (values_out && values_length > 0)
        std::memset(values_out, 0, static_cast<std::size_t>(values_length) * sizeof(double));
    if (status_out && status_length > 0)
        std::memset(status_out, 0, static_cast<std::size_t>(status_length) * sizeof(int));
    if (!quantity_names || num_names <= 0)
        return fail(catchem::BoundaryStatus::NullArgument, "ondemand_get_optics", "quantity_names",
                    "at least one quantity name is required");
    if (!wavelengths_m || num_wavelengths <= 0)
        return fail(catchem::BoundaryStatus::NullArgument, "ondemand_get_optics", "wavelengths_m",
                    "at least one wavelength is required for an optics request");
    if (!values_out || values_length <= 0)
        return fail(catchem::BoundaryStatus::NullArgument, "ondemand_get_optics", "values_out",
                    "a positive-length values output is required");
    if (status_out && status_length < values_length)
        return fail(catchem::BoundaryStatus::ExtentMismatch, "ondemand_get_optics", "status_out",
                    "the status buffer must cover every value slot");
    catchem::AdmissionLease admission;
    const int status = admit_handle(core_ptr, catchem::HandleType::Core, "ondemand_get_optics", admission);
    if (status != CATCHEM_SUCCESS)
        return status;
    try {
        catchem::OnDemandService* service = nullptr;
        std::shared_ptr<catchem::StateManager> state;
        const char* object = nullptr;
        const char* cause = nullptr;
        const int admitted = admit_service(core_ptr, "ondemand_get_optics", service, state, object, cause);
        if (admitted != CATCHEM_SUCCESS)
            return fail(static_cast<catchem::BoundaryStatus>(admitted), "ondemand_get_optics", object, cause);
        std::vector<std::string> names;
        names.reserve(static_cast<std::size_t>(num_names));
        std::size_t needed = 0;
        const std::size_t bands = static_cast<std::size_t>(num_wavelengths);
        const std::size_t cells = static_cast<std::size_t>(state->column_count());
        const std::size_t levels = static_cast<std::size_t>(state->level_count());
        for (int i = 0; i < num_names; ++i) {
            if (!quantity_names[i]) {
                const std::string why = "a null quantity name at index " + std::to_string(i) + " is not allowed";
                return fail(catchem::BoundaryStatus::NullArgument, "ondemand_get_optics", "quantity_names", why.c_str());
            }
            names.emplace_back(quantity_names[i]);
            const catchem::QuantityInfo* info = catchem::OnDemandService::find_quantity(names.back());
            if (info == nullptr || !is_optics_kind(info->kind)) {
                const std::string why = "'" + names.back() +
                                        "' is not an optics quantity; use the PM endpoint for diagnostic names";
                return fail(catchem::BoundaryStatus::InvalidIndex, "ondemand_get_optics", "quantity_names", why.c_str());
            }
            needed += optics_points(*info, bands, cells, levels);
        }
        if (static_cast<std::size_t>(values_length) < needed) {
            const std::string why = "the values buffer holds " + std::to_string(values_length) + " entries; " +
                                    std::to_string(num_names) + " optics names x " + std::to_string(num_wavelengths) +
                                    " wavelengths need " + std::to_string(needed);
            return fail(catchem::BoundaryStatus::ExtentMismatch, "ondemand_get_optics", "values_out", why.c_str());
        }
        std::string error;
        const int rc = service->read_optics_bulk(names, wavelengths_m, num_wavelengths, *state, values_out, status_out,
                                                 static_cast<std::size_t>(values_length), error);
        if (rc != CATCHEM_SUCCESS) {
            return fail(static_cast<catchem::BoundaryStatus>(rc), "ondemand_get_optics", "on_demand", error.c_str());
        }
        return CATCHEM_SUCCESS;
    } catch (...) {
        return fail(catchem::BoundaryStatus::InternalError, "ondemand_get_optics", "core",
                    "unexpected failure reading on-demand optical properties");
    }
}

void catchem_ondemand_get_optics(void* core_ptr, const char* const* quantity_names, int num_names,
                                 const double* wavelengths_m, int num_wavelengths, double* values_out, int* status_out,
                                 int values_length, int status_length) {
    (void)catchem_ondemand_get_optics_checked(core_ptr, quantity_names, num_names, wavelengths_m, num_wavelengths,
                                              values_out, status_out, values_length, status_length);
}

int catchem_ondemand_get_quantity_checked(void* core_ptr, const char* quantity_name, const double* wavelengths_m,
                                          int num_wavelengths, const char* const* species_names, int num_species,
                                          double* values_out, int* status_out, int values_length, int status_length) {
    if (values_out && values_length > 0)
        std::memset(values_out, 0, static_cast<std::size_t>(values_length) * sizeof(double));
    if (status_out && status_length > 0)
        std::memset(status_out, 0, static_cast<std::size_t>(status_length) * sizeof(int));
    if (!quantity_name)
        return fail(catchem::BoundaryStatus::NullArgument, "ondemand_get_quantity", "quantity_name",
                    "a quantity name is required");
    if (!values_out || values_length <= 0)
        return fail(catchem::BoundaryStatus::NullArgument, "ondemand_get_quantity", "values_out",
                    "a positive-length values output is required");
    if (status_out && status_length < values_length)
        return fail(catchem::BoundaryStatus::ExtentMismatch, "ondemand_get_quantity", "status_out",
                    "the status buffer must cover every value slot");
    catchem::AdmissionLease admission;
    const int status = admit_handle(core_ptr, catchem::HandleType::Core, "ondemand_get_quantity", admission);
    if (status != CATCHEM_SUCCESS)
        return status;
    try {
        catchem::OnDemandService* service = nullptr;
        std::shared_ptr<catchem::StateManager> state;
        const char* object = nullptr;
        const char* cause = nullptr;
        const int admitted = admit_service(core_ptr, "ondemand_get_quantity", service, state, object, cause);
        if (admitted != CATCHEM_SUCCESS)
            return fail(static_cast<catchem::BoundaryStatus>(admitted), "ondemand_get_quantity", object, cause);
        const catchem::QuantityInfo* info = catchem::OnDemandService::find_quantity(quantity_name);
        if (info == nullptr) {
            const std::string why =
                std::string("'") + quantity_name + "' is not a known on-demand quantity; enumerate the catalogue first";
            return fail(catchem::BoundaryStatus::InvalidIndex, "ondemand_get_quantity", "quantity_name", why.c_str());
        }
        if (is_optics_kind(info->kind) && (!wavelengths_m || num_wavelengths <= 0)) {
            const std::string why =
                std::string("the optics quantity '") + quantity_name + "' requires at least one wavelength";
            return fail(catchem::BoundaryStatus::NullArgument, "ondemand_get_quantity", "wavelengths_m", why.c_str());
        }
        // Bulk form only for now: species attribution is a later milestone.
        if (species_names != nullptr && num_species > 0) {
            const std::string why = std::string("per-species attribution for '") + quantity_name +
                                    "' is not yet supported; request the bulk value with species_names = NULL";
            return fail(catchem::BoundaryStatus::InvalidIndex, "ondemand_get_quantity", "species_names", why.c_str());
        }
        const std::size_t cells = static_cast<std::size_t>(state->column_count());
        const std::size_t levels = static_cast<std::size_t>(state->level_count());
        const std::size_t bands = static_cast<std::size_t>(num_wavelengths > 0 ? num_wavelengths : 1);
        std::size_t needed = 0;
        if (is_diagnostic_kind(info->kind)) {
            needed = diagnostic_points(*info, cells, levels);
        } else if (is_optics_kind(info->kind)) {
            needed = optics_points(*info, bands, cells, levels);
        }
        if (static_cast<std::size_t>(values_length) < needed) {
            const std::string why = "the values buffer holds " + std::to_string(values_length) +
                                    " entries; quantity '" + quantity_name + "' needs " + std::to_string(needed);
            return fail(catchem::BoundaryStatus::ExtentMismatch, "ondemand_get_quantity", "values_out", why.c_str());
        }
        std::string error;
        const int rc = service->read_quantity(quantity_name, wavelengths_m, num_wavelengths, nullptr, 0, *state,
                                              values_out, status_out, static_cast<std::size_t>(values_length), error);
        if (rc != CATCHEM_SUCCESS) {
            return fail(static_cast<catchem::BoundaryStatus>(rc), "ondemand_get_quantity", "on_demand", error.c_str());
        }
        return CATCHEM_SUCCESS;
    } catch (...) {
        return fail(catchem::BoundaryStatus::InternalError, "ondemand_get_quantity", "core",
                    "unexpected failure reading an on-demand quantity");
    }
}

void catchem_ondemand_get_quantity(void* core_ptr, const char* quantity_name, const double* wavelengths_m,
                                   int num_wavelengths, const char* const* species_names, int num_species,
                                   double* values_out, int* status_out, int values_length, int status_length) {
    (void)catchem_ondemand_get_quantity_checked(core_ptr, quantity_name, wavelengths_m, num_wavelengths, species_names,
                                                num_species, values_out, status_out, values_length, status_length);
}

}  // extern "C"
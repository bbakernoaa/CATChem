#include "catchem_api.hpp"
#include "catchem_api_internal.hpp"
#include "catchem_core.hpp"
#include "catchem_manifest.hpp"
#include <memory>

using namespace catchem::api_internal;
namespace {
    /** @brief Copy a string to a caller buffer after reporting its required extent. */
    int string_result(void* handle, const std::string& value, char* buffer, int length, int* required) {
        if (!required)
            return fail(catchem::BoundaryStatus::NullArgument, "manifest_get_string", "required_len",
                        "required length pointer is null");
        *required = static_cast<int>(value.size() + 1);
        if (length == 0 && !buffer)
            return CATCHEM_SUCCESS;
        if (!buffer || length < *required)
            return fail(catchem::BoundaryStatus::ExtentMismatch, "manifest_get_string", "buffer",
                        "caller buffer is too short; no data was copied");
        copy_string_to_buffer(value, buffer, length);
        (void)handle;
        return CATCHEM_SUCCESS;
    }

    /** @brief Check whether a caller buffer can hold a NUL-terminated string. */
    bool string_capacity_ok(const std::string& value, char* buffer, int length) {
        return buffer && length >= static_cast<int>(value.size() + 1);
    }

    /** @brief Acquire a manifest handle lease and resolve its shared manifest. */
    int with_manifest(void* handle, catchem::AdmissionLease& lease,
                      std::shared_ptr<catchem::MechanismManifest>& manifest) {
        const int status = admit_handle(handle, catchem::HandleType::Manifest, "manifest", lease);
        if (status != CATCHEM_SUCCESS)
            return status;
        manifest = *static_cast<std::shared_ptr<catchem::MechanismManifest>*>(handle);
        return CATCHEM_SUCCESS;
    }
} // namespace

extern "C" {
int catchem_manifest_open_checked(const char* path, void** out) {
    catchem::clear_boundary_error();
    if (out)
        *out = nullptr;
    if (!path || !out)
        return fail(catchem::BoundaryStatus::NullArgument, "manifest_open", "argument",
                    "configuration path and output pointer are required");
    try {
        auto* holder =
            new std::shared_ptr<catchem::MechanismManifest>(std::make_shared<catchem::MechanismManifest>(path));
        if (!catchem::HandleRegistry::instance().add(
                holder, {catchem::HandleType::Manifest, 1, 1, nullptr, catchem::HandleOwnership::Owned})) {
            delete holder;
            return fail(catchem::BoundaryStatus::InternalError, "manifest_open", "registry",
                        "could not register manifest");
        }
        *out = holder;
        return CATCHEM_SUCCESS;
    } catch (const std::exception& e) {
        return fail(catchem::BoundaryStatus::InvalidConfiguration, "manifest_open", path, e.what());
    } catch (...) {
        return fail(catchem::BoundaryStatus::InternalError, "manifest_open", path, "unknown exception");
    }
}
int catchem_manifest_get_species_count_checked(void* h, int* out) {
    catchem::AdmissionLease l;
    std::shared_ptr<catchem::MechanismManifest> m;
    int s = with_manifest(h, l, m);
    if (s)
        return s;
    if (!out)
        return fail(catchem::BoundaryStatus::NullArgument, "manifest_species_count", "count_out", "output is null");
    *out = static_cast<int>(m->species().size());
    return CATCHEM_SUCCESS;
}
int catchem_manifest_get_species_name_checked(void* h, int i, char* b, int n, int* r) {
    catchem::AdmissionLease l;
    std::shared_ptr<catchem::MechanismManifest> m;
    int s = with_manifest(h, l, m);
    if (s)
        return s;
    if (i < 0 || static_cast<std::size_t>(i) >= m->species().size())
        return fail(catchem::BoundaryStatus::InvalidIndex, "manifest_species_name", "index",
                    "species index is out of range");
    return string_result(h, m->species()[i].name, b, n, r);
}
int catchem_manifest_get_species_standard_name_checked(void* h, int i, char* b, int n, int* r) {
    catchem::AdmissionLease l;
    std::shared_ptr<catchem::MechanismManifest> m;
    int s = with_manifest(h, l, m);
    if (s)
        return s;
    if (i < 0 || static_cast<std::size_t>(i) >= m->species().size())
        return fail(catchem::BoundaryStatus::InvalidIndex, "manifest_species_standard_name", "index",
                    "species index is out of range");
    return string_result(h, m->species()[i].standard_name, b, n, r);
}
int catchem_manifest_get_species_v1_checked(void* h, int i, catchem_manifest_species_v1* d, char* name, int nn,
                                            char* standard, int sn, char* units, int un) {
    catchem::AdmissionLease l;
    std::shared_ptr<catchem::MechanismManifest> m;
    int s = with_manifest(h, l, m);
    if (s)
        return s;
    if (!d)
        return fail(catchem::BoundaryStatus::NullArgument, "manifest_species_v1", "descriptor",
                    "descriptor pointer is null");
    if (d->schema_version != 1 || d->struct_size < sizeof(catchem_manifest_species_v1))
        return fail(catchem::BoundaryStatus::ContractViolation, "manifest_species_v1", "descriptor",
                    "unsupported schema version or descriptor size");
    if (i < 0 || static_cast<std::size_t>(i) >= m->species().size())
        return fail(catchem::BoundaryStatus::InvalidIndex, "manifest_species_v1", "index",
                    "species index is out of range");
    const auto& v = m->species()[i];
    d->name_required_len = static_cast<int>(v.name.size() + 1);
    d->standard_name_required_len = static_cast<int>(v.standard_name.size() + 1);
    d->units_required_len = static_cast<int>(v.units.size() + 1);
    if (!string_capacity_ok(v.name, name, nn) || !string_capacity_ok(v.standard_name, standard, sn) ||
        !string_capacity_ok(v.units, units, un))
        return fail(catchem::BoundaryStatus::ExtentMismatch, "manifest_species_v1", "buffer",
                    "one or more caller buffers are too short; descriptor strings were not copied");
    d->flags = (v.gas ? 1u : 0u) | (v.aerosol ? 2u : 0u) | (v.advected ? 4u : 0u);
    d->molecular_weight_kg_mol = v.molecular_weight_kg_mol;
    d->amount_type = static_cast<uint32_t>(v.host_representation.amount_type);
    d->air_basis = static_cast<uint32_t>(v.host_representation.air_basis);
    d->alias_count = static_cast<int>(v.aliases.size());
    d->role_count = static_cast<int>(v.roles.size());
    copy_string_to_buffer(v.name, name, nn);
    copy_string_to_buffer(v.standard_name, standard, sn);
    copy_string_to_buffer(v.units, units, un);
    return CATCHEM_SUCCESS;
}
int catchem_manifest_get_species_alias_checked(void* h, int si, int ai, char* b, int n, int* r) {
    catchem::AdmissionLease l;
    std::shared_ptr<catchem::MechanismManifest> m;
    int s = with_manifest(h, l, m);
    if (s)
        return s;
    if (si < 0 || static_cast<std::size_t>(si) >= m->species().size())
        return fail(catchem::BoundaryStatus::InvalidIndex, "manifest_species_alias", "species_index",
                    "species index is out of range");
    const auto& aliases = m->species()[si].aliases;
    if (ai < 0 || static_cast<std::size_t>(ai) >= aliases.size())
        return fail(catchem::BoundaryStatus::InvalidIndex, "manifest_species_alias", "alias_index",
                    "alias index is out of range");
    return string_result(h, aliases[ai], b, n, r);
}
int catchem_manifest_get_species_role_checked(void* h, int si, int ri, char* b, int n, int* r) {
    catchem::AdmissionLease l;
    std::shared_ptr<catchem::MechanismManifest> m;
    int s = with_manifest(h, l, m);
    if (s)
        return s;
    if (si < 0 || static_cast<std::size_t>(si) >= m->species().size())
        return fail(catchem::BoundaryStatus::InvalidIndex, "manifest_species_role", "species_index",
                    "species index is out of range");
    const auto& roles = m->species()[si].roles;
    if (ri < 0 || static_cast<std::size_t>(ri) >= roles.size())
        return fail(catchem::BoundaryStatus::InvalidIndex, "manifest_species_role", "role_index",
                    "role index is out of range");
    return string_result(h, roles[ri], b, n, r);
}
int catchem_manifest_get_process_count_checked(void* h, int* out) {
    catchem::AdmissionLease l;
    std::shared_ptr<catchem::MechanismManifest> m;
    int s = with_manifest(h, l, m);
    if (s)
        return s;
    if (!out)
        return fail(catchem::BoundaryStatus::NullArgument, "manifest_process_count", "count_out", "output is null");
    *out = static_cast<int>(m->processes().size());
    return CATCHEM_SUCCESS;
}
int catchem_manifest_get_process_checked(void* h, int i, char* b, int n, int* r, int* available) {
    catchem::AdmissionLease l;
    std::shared_ptr<catchem::MechanismManifest> m;
    int s = with_manifest(h, l, m);
    if (s)
        return s;
    if (i < 0 || static_cast<std::size_t>(i) >= m->processes().size())
        return fail(catchem::BoundaryStatus::InvalidIndex, "manifest_process", "index",
                    "process index is out of range");
    if (!available)
        return fail(catchem::BoundaryStatus::NullArgument, "manifest_process", "available", "output is null");
    s = string_result(h, m->processes()[i].name, b, n, r);
    if (!s)
        *available = m->processes()[i].available;
    return s;
}
int catchem_manifest_get_process_v1_checked(void* h, int i, catchem_manifest_process_v1* d, char* name, int n,
                                            char* scheme, int scheme_len) {
    catchem::AdmissionLease l;
    std::shared_ptr<catchem::MechanismManifest> m;
    int s = with_manifest(h, l, m);
    if (s)
        return s;
    if (!d)
        return fail(catchem::BoundaryStatus::NullArgument, "manifest_process_v1", "descriptor",
                    "descriptor pointer is null");
    if (d->schema_version != 1 || d->struct_size < sizeof(catchem_manifest_process_v1))
        return fail(catchem::BoundaryStatus::ContractViolation, "manifest_process_v1", "descriptor",
                    "unsupported schema version or descriptor size");
    if (i < 0 || static_cast<std::size_t>(i) >= m->processes().size())
        return fail(catchem::BoundaryStatus::InvalidIndex, "manifest_process_v1", "index",
                    "process index is out of range");
    const auto& v = m->processes()[i];
    d->name_required_len = static_cast<int>(v.name.size() + 1);
    d->scheme_required_len = static_cast<int>(v.scheme.size() + 1);
    if (!string_capacity_ok(v.name, name, n) || !string_capacity_ok(v.scheme, scheme, scheme_len))
        return fail(catchem::BoundaryStatus::ExtentMismatch, "manifest_process_v1", "buffer",
                    "caller buffer is too short; descriptor strings were not copied");
    d->available = v.available ? 1 : 0;
    copy_string_to_buffer(v.name, name, n);
    copy_string_to_buffer(v.scheme, scheme, scheme_len);
    return CATCHEM_SUCCESS;
}
int catchem_manifest_get_fingerprint_checked(void* h, char* b, int n, int* r) {
    catchem::AdmissionLease l;
    std::shared_ptr<catchem::MechanismManifest> m;
    int s = with_manifest(h, l, m);
    if (s)
        return s;
    return string_result(h, m->fingerprint(), b, n, r);
}
int catchem_manifest_validate_config_checked(void* h, const char* path) {
    catchem::AdmissionLease l;
    std::shared_ptr<catchem::MechanismManifest> m;
    int s = with_manifest(h, l, m);
    if (s)
        return s;
    if (!path)
        return fail(catchem::BoundaryStatus::NullArgument, "manifest_validate_config", "config_file",
                    "configuration path is null");
    try {
        catchem::MechanismManifest candidate(path);
        if (candidate.fingerprint() != m->fingerprint())
            return fail(catchem::BoundaryStatus::ContractViolation, "manifest_validate_config", path,
                        "configuration or species content differs from the registered manifest fingerprint");
        return CATCHEM_SUCCESS;
    } catch (const std::exception& e) {
        return fail(catchem::BoundaryStatus::InvalidConfiguration, "manifest_validate_config", path, e.what());
    } catch (...) {
        return fail(catchem::BoundaryStatus::InternalError, "manifest_validate_config", path, "unknown exception");
    }
}
int catchem_manifest_validate_core_checked(void* h, void* core_ptr) {
    catchem::AdmissionLease manifest_lease;
    std::shared_ptr<catchem::MechanismManifest> manifest;
    const int manifest_status = with_manifest(h, manifest_lease, manifest);
    if (manifest_status != CATCHEM_SUCCESS)
        return manifest_status;
    catchem::AdmissionLease core_lease;
    const int core_status = admit_handle(core_ptr, catchem::HandleType::Core, "manifest_validate_core", core_lease);
    if (core_status != CATCHEM_SUCCESS)
        return core_status;
    try {
        const auto state = static_cast<catchem::Core*>(core_ptr)->get_state_manager();
        const auto mechanism = state ? state->chemistry().mechanism : nullptr;
        if (!mechanism)
            return fail(catchem::BoundaryStatus::InvalidState, "manifest_validate_core", "core",
                        "Core has no active mechanism");
        if (mechanism->fingerprint != manifest->fingerprint())
            return fail(catchem::BoundaryStatus::ContractViolation, "manifest_validate_core", "core",
                        "active Core mechanism differs from the immutable manifest binding");
        return CATCHEM_SUCCESS;
    } catch (const std::exception& e) {
        return fail(catchem::BoundaryStatus::InvalidConfiguration, "manifest_validate_core", "core", e.what());
    } catch (...) {
        return fail(catchem::BoundaryStatus::InternalError, "manifest_validate_core", "core", "unknown exception");
    }
}
int catchem_manifest_destroy_checked(void* h) {
    auto& registry = catchem::HandleRegistry::instance();
    const int s = static_cast<int>(registry.close_and_wait(h, catchem::HandleType::Manifest));
    if (s == CATCHEM_SUCCESS)
        delete static_cast<std::shared_ptr<catchem::MechanismManifest>*>(h);
    return s;
}
}

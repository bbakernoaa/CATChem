// On-demand aerosol diagnostics service (host-pulled PM / optics / AOD).
//
// The service never advances state: it assembles a library-layout snapshot of
// the bound host state, runs the aerosol library's compute through the
// bridge, and copies the requested blocks out.  Host chemistry conc is a
// mixing ratio [ug/kg]; the library consumes mass concentration [kg/m^3], so
// the snapshot applies the same unit conversion the settling engine does
// (conc * 1e-9 * AIRDEN) and aggregates transport bins sharing one Mie name
// into the package's modal species slot (the join key the settling engine
// established).
//
// Availability codes: the library's diagnostic and optics slabs carry values,
// not per-point status codes.  The service derives each point's status from
// the inputs it controls: a masked request for a host species with no
// configured modal slot is rejected before compute; a point whose slab read is
// non-finite (a missing slab or out-of-range slot, never a real physical
// zero) reports NotInSource with a zero value; and a bulk pass reports
// Available at every point.  Per-species optics additionally consult the
// library's curve store: a species with neither a sourced Mie curve nor a
// modelled optical contribution reports NotInSource rather than an
// ok-flagged zero a reader could not distinguish from a real one.
#include "catchem_ondemand_service.hpp"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstdio>
#include <limits>
#include <string>
#include <vector>

#include "catchem_state_manager.hpp"

#ifdef CATCHEM_HAVE_EXAERO
#include "catchem_exaero_bridge.hpp"
#endif

namespace catchem {

namespace {

// Availability codes mirrored from the bridge's attribute-status constants
// (they must stay equal to exaero's AttributeStatus values).
constexpr int kStatusAvailable = 0;
constexpr int kStatusNotInSource = 4;

// Standard gravity for the hydrostatic layer-thickness fallback [m s^-2].
constexpr double kGravity = 9.80665;

// Default optics-table wavelength domain [m] when the species YAML names no
// explicit band range: the GOCART radiation band interval the pinned Mie
// tables cover.  A wavelength outside it is rejected, never answered by the
// library's analytical fallback (which would be silent wrong physics).
constexpr double kDefaultBandMinM = 0.2e-6;
constexpr double kDefaultBandMaxM = 4.0e-6;

std::string to_upper(std::string value) {
    std::transform(value.begin(), value.end(), value.begin(),
                   [](unsigned char c) { return static_cast<char>(std::toupper(c)); });
    return value;
}

// Compact scientific rendering for error messages: std::to_string's fixed
// six-decimal form would print a micrometre-scale wavelength band as
// "0.000000", hiding the very range the message exists to name.
std::string format_si(double value) {
    char buffer[32];
    std::snprintf(buffer, sizeof(buffer), "%g", value);
    return std::string(buffer);
}

}  // namespace

// --- Normative quantity registry ---------------------------------------------
// name -> kind -> library slot -> units.  The order is the catalogue the
// enumeration endpoints expose; entries are append-only so a host may rely on
// index stability within a release line.
namespace {

using Kind = OnDemandKind;

const std::vector<QuantityInfo>& quantity_registry() {
    static const std::vector<QuantityInfo> registry = {
        // Diagnostic-family quantities (library diagnostics slab).
        {"pm1", Kind::Diagnostic3D, 11, "kg m^-3"},
        {"pm2_5", Kind::Diagnostic3D, 1, "kg m^-3"},
        {"pm10", Kind::Diagnostic3D, 2, "kg m^-3"},
        {"mass_concentration", Kind::Diagnostic3D, 0, "kg m^-3"},
        {"number_concentration", Kind::Diagnostic3D, 3, "particles m^-3"},
        {"surface_area_density", Kind::Diagnostic3D, 4, "m^2 m^-3"},
        {"aerosol_liquid_water", Kind::Diagnostic3D, 5, "kg m^-3"},
        {"settling_velocity", Kind::Diagnostic3D, 6, "m s^-1"},
        {"surface_mass", Kind::Diagnostic2D, 7, "kg m^-3"},
        {"column_mass", Kind::Diagnostic2D, 8, "kg m^-2"},
        {"surface_pm2_5_mass", Kind::Diagnostic2D, 9, "kg m^-3"},
        {"column_pm2_5_mass", Kind::Diagnostic2D, 10, "kg m^-2"},
        // Optics-family quantities (library optics slab at host wavelengths).
        {"extinction", Kind::Optical3D, 0, "m^-1"},
        {"scattering", Kind::Optical3D, 1, "m^-1"},
        {"backscatter", Kind::Optical3D, 2, "m^-1 sr^-1"},
        {"asymmetry", Kind::Optical3D, 3, "fraction"},
        {"lidar_backscatter", Kind::Optical3D, 4, "m^-1 sr^-1"},
        {"single_scattering_albedo", Kind::OpticalSsa, -1, "fraction"},
        {"aod", Kind::Optical2D, 5, "dimensionless"},
        {"scattering_aot", Kind::Optical2D, 6, "dimensionless"},
        {"fine_mode_aod", Kind::Optical2D, 7, "dimensionless"},
        {"fine_mode_scattering_aot", Kind::Optical2D, 8, "dimensionless"},
        {"pm2_5_aod", Kind::Optical2D, 9, "dimensionless"},
        {"pm2_5_scattering_aot", Kind::Optical2D, 10, "dimensionless"},
    };
    return registry;
}

}  // namespace

int OnDemandService::quantity_count() {
    return static_cast<int>(quantity_registry().size());
}

bool OnDemandService::available() {
#ifdef CATCHEM_HAVE_EXAERO
    return true;
#else
    return false;
#endif
}

const char* OnDemandService::quantity_name_at(int index) {
    const auto& registry = quantity_registry();
    if (index < 0 || index >= static_cast<int>(registry.size())) {
        return nullptr;
    }
    return registry[static_cast<std::size_t>(index)].name;
}

const char* OnDemandService::quantity_units_at(int index) {
    const auto& registry = quantity_registry();
    if (index < 0 || index >= static_cast<int>(registry.size())) {
        return nullptr;
    }
    return registry[static_cast<std::size_t>(index)].units;
}

const QuantityInfo* OnDemandService::find_quantity(const std::string& name) {
    const auto& registry = quantity_registry();
    for (const auto& info : registry) {
        if (name == info.name) {
            return &info;
        }
    }
    return nullptr;
}

// --- pimpl --------------------------------------------------------------------
// The library-facing state lives here so the public header stays free of the
// bridge include and the service's object layout is identical whether or not
// the aerosol utility is built into this library (a gate-OFF Impl simply has
// no package).
struct OnDemandService::Impl {
#ifdef CATCHEM_HAVE_EXAERO
    std::unique_ptr<exaero_util::ExaeroPackageHandle> package;
#endif
    std::vector<std::string> modal_names;   ///< package species labels, own order
    std::vector<std::pair<int, int>> slot_map;  ///< (host index, modal slot)

    // Request staging: host pointers (read-only, valid for one request) plus
    // the assembled modal mass state and derived thickness.
    const double* temperature = nullptr;
    const double* pressure = nullptr;
    const double* air_density = nullptr;
    const double* relative_humidity = nullptr;
    std::vector<double> thickness;
    std::vector<double> modal_state;
};

// --- lifecycle ---------------------------------------------------------------

OnDemandService::OnDemandService() : impl_(std::make_unique<Impl>()) {}
OnDemandService::~OnDemandService() {
#ifdef CATCHEM_HAVE_EXAERO
    if (impl_ && impl_->package) {
        impl_->package->release();
        impl_->package.reset();
    }
#endif
    if (env_owned_) {
#ifdef CATCHEM_HAVE_EXAERO
        exaero_util::ExaeroPackageHandle::finalize_environment();
#endif
        env_owned_ = false;
    }
}

OnDemandService::OnDemandService(OnDemandService&& other) noexcept = default;
OnDemandService& OnDemandService::operator=(OnDemandService&& other) noexcept = default;

int OnDemandService::configure(const std::string& exaero_config_path, bool enabled) {
    if (enabled && exaero_config_path.empty()) {
        // A rejected configure leaves the service unconfigured: an enabled
        // utility without a species YAML cannot bring up, and a half-set
        // configuration would report a confusing bring-up error later.
        return CATCHEM_INVALID_CONFIGURATION;
    }
    enabled_ = enabled;
    exaero_config_path_ = exaero_config_path;
    return CATCHEM_SUCCESS;
}

int OnDemandService::ensure_ready(StateManager& state, std::string& error) {
    if (!enabled_) {
        error = "the on-demand aerosol utility is not configured; enable it with an "
                "'on_demand' configuration section naming an exaero species YAML";
        return CATCHEM_QUANTITY_NOT_AVAILABLE;
    }
#ifdef CATCHEM_HAVE_EXAERO
    if (ready_) {
        return CATCHEM_SUCCESS;
    }
    return bring_up(state, error);
#else
    (void)state;
    error = "the aerosol utility is not built into this CATChem library; "
            "reconfigure with -DCATCHEM_BUILD_EXAERO=ON";
    return CATCHEM_QUANTITY_NOT_AVAILABLE;
#endif
}

#ifdef CATCHEM_HAVE_EXAERO

int OnDemandService::bring_up(StateManager& state, std::string& error) {
    // Order mirrors the settling engine: the shared Kokkos runtime first
    // (reference-counted), then the package from the configured species YAML.
    // Any failure releases exactly what was acquired -- never a half-up
    // service and never a leaked runtime reference.
    exaero_util::ExaeroPackageHandle::init_environment();
    env_owned_ = true;
    impl_->package = std::make_unique<exaero_util::ExaeroPackageHandle>();
    std::string init_error;
    if (!impl_->package->initialize_from_file(exaero_config_path_, init_error)) {
        impl_->package.reset();
        exaero_util::ExaeroPackageHandle::finalize_environment();
        env_owned_ = false;
        error = "on-demand aerosol utility could not initialize the exaero package from '" + exaero_config_path_ +
                "': " + init_error;
        return CATCHEM_INVALID_CONFIGURATION;
    }
    modal_species_ = impl_->package->species_count();
    if (modal_species_ <= 0) {
        impl_->package->release();
        impl_->package.reset();
        exaero_util::ExaeroPackageHandle::finalize_environment();
        env_owned_ = false;
        error = "exaero configuration '" + exaero_config_path_ + "' defines no species";
        return CATCHEM_INVALID_CONFIGURATION;
    }
    impl_->modal_names.clear();
    impl_->modal_names.reserve(static_cast<std::size_t>(modal_species_));
    for (int i = 0; i < modal_species_; ++i) {
        impl_->modal_names.push_back(impl_->package->species_name(i));
    }
    band_min_m_ = kDefaultBandMinM;
    band_max_m_ = kDefaultBandMaxM;
    if (!validate_and_map_species(state, error)) {
        impl_->package->release();
        impl_->package.reset();
        exaero_util::ExaeroPackageHandle::finalize_environment();
        env_owned_ = false;
        return CATCHEM_INVALID_CONFIGURATION;
    }
    ready_ = true;
    return CATCHEM_SUCCESS;
}

bool OnDemandService::validate_and_map_species(StateManager& state, std::string& error) {
    // Every host aerosol must bind to a configured modal type through its
    // __mie_name (the join the settling engine established); an unbindable
    // aerosol is a configuration error, not a silent exclusion.  Gases and
    // non-aerosol tracers are simply not part of the map.
    impl_->slot_map.clear();
    const auto& chem = state.chemistry();
    for (std::size_t i = 0; i < chem.species_list.size(); ++i) {
        const SpeciesMetadata& meta = chem.species_list[i];
        if (!meta.is_aerosol) {
            continue;
        }
        const std::string mie = meta.mie_name.empty() ? meta.short_name : meta.mie_name;
        const int modal = impl_->package->species_index(mie);
        if (modal < 0) {
            error = "host aerosol species '" + meta.short_name + "' has __mie_name '" + mie +
                    "' which is not defined in the exaero configuration '" + exaero_config_path_ + "'";
            return false;
        }
        impl_->slot_map.emplace_back(static_cast<int>(i), modal);
    }
    if (impl_->slot_map.empty()) {
        error = "the bound state carries no aerosol species for the on-demand utility to diagnose";
        return false;
    }
    return true;
}

int OnDemandService::assemble_inputs(StateManager& state, std::string& error) {
    const int cells = state.column_count();
    const int levels = state.level_count();
    if (cells <= 0 || levels <= 0) {
        error = "on-demand service: state has no columns or levels";
        return CATCHEM_INVALID_STATE;
    }
    const auto& met = state.meteorology();
    if (!met.T || !met.AIRDEN || !met.PMID || !met.RH) {
        error = "on-demand service: meteorology T/AIRDEN/PMID/RH must be bound before a request";
        return CATCHEM_INVALID_STATE;
    }
    const auto& chem = state.chemistry();
    if (!chem.conc) {
        error = "on-demand service: chemistry conc must be bound before a request";
        return CATCHEM_INVALID_STATE;
    }
    if (static_cast<int>(chem.conc->extent(0)) != cells || static_cast<int>(chem.conc->extent(1)) != levels) {
        error = "on-demand service: chemistry conc extents disagree with the state grid";
        return CATCHEM_INVALID_STATE;
    }

    impl_->temperature = met.T->host_read();
    impl_->pressure = met.PMID->host_read();
    impl_->air_density = met.AIRDEN->host_read();
    impl_->relative_humidity = met.RH->host_read();
    const double* conc = chem.conc->host_read();
    if (impl_->temperature == nullptr || impl_->pressure == nullptr || impl_->air_density == nullptr ||
        impl_->relative_humidity == nullptr || conc == nullptr) {
        error = "on-demand service: a required field has no host storage";
        return CATCHEM_INVALID_STATE;
    }

    const std::size_t points = static_cast<std::size_t>(cells) * static_cast<std::size_t>(levels);

    // Layer thickness [m] for the AOT column integral: the host-provided
    // BXHEIGHT when present, otherwise the hydrostatic derivation from DELP.
    const double* host_thickness = met.BXHEIGHT ? met.BXHEIGHT->host_read() : nullptr;
    if (host_thickness != nullptr) {
        impl_->thickness.assign(host_thickness, host_thickness + points);
    } else {
        const double* delp = state.read_field<3>("DELP");
        if (delp == nullptr) {
            error = "on-demand service: neither BXHEIGHT nor DELP is bound; a layer thickness is required";
            return CATCHEM_INVALID_STATE;
        }
        impl_->thickness.assign(points, 0.0);
        for (std::size_t p = 0; p < points; ++p) {
            const double rho = impl_->air_density[p];
            impl_->thickness[p] = (rho > 0.0) ? delp[p] / (rho * kGravity) : 0.0;
        }
    }

    // Modal mass state [kg/m^3], (cell, level, species) column-major,
    // aggregated from host bins sharing a Mie name with the settling engine's
    // unit conversion (ug/kg -> kg/kg, then * AIRDEN).
    impl_->modal_state.assign(points * static_cast<std::size_t>(modal_species_), 0.0);
    for (const auto& map : impl_->slot_map) {
        const double* src = conc + static_cast<std::size_t>(map.first) * points;
        const std::size_t modal_offset = static_cast<std::size_t>(map.second) * points;
        for (std::size_t p = 0; p < points; ++p) {
            const double value = src[p];
            if (!std::isfinite(value) || value < 0.0) {
                error = "on-demand service: aerosol species '" +
                        chem.species_list[static_cast<std::size_t>(map.first)].short_name +
                        "' has a non-finite or negative mixing ratio at point " + std::to_string(p);
                return CATCHEM_PHYSICAL_VALIDATION_FAILURE;
            }
            impl_->modal_state[modal_offset + p] += value * 1.0e-9 * impl_->air_density[p];
        }
    }

    cells_ = cells;
    levels_ = levels;
    return CATCHEM_SUCCESS;
}

int OnDemandService::run_diagnostics(const int* species_keep, int num_keep, std::string& error) {
    const double* thickness = impl_->thickness.data();
    bool ok = false;
    if (species_keep == nullptr || num_keep <= 0) {
        ok = impl_->package->compute_diagnostics(cells_, levels_, modal_species_, impl_->temperature, impl_->pressure,
                                                 impl_->air_density, impl_->relative_humidity, thickness,
                                                 impl_->modal_state.data(), error);
    } else {
        ok = impl_->package->compute_diagnostics_masked(cells_, levels_, modal_species_, impl_->temperature,
                                                        impl_->pressure, impl_->air_density, impl_->relative_humidity,
                                                        thickness, impl_->modal_state.data(), species_keep, num_keep,
                                                        error);
    }
    return ok ? CATCHEM_SUCCESS : CATCHEM_INTERNAL_ERROR;
}

int OnDemandService::run_optics(const double* wavelengths, int num_bands, const int* species_keep, int num_keep,
                                std::string& error) {
    const double* thickness = impl_->thickness.data();
    bool ok = false;
    if (species_keep == nullptr || num_keep <= 0) {
        ok = impl_->package->compute_optics(cells_, levels_, num_bands, modal_species_, wavelengths,
                                            impl_->temperature, impl_->pressure, impl_->air_density,
                                            impl_->relative_humidity, thickness, impl_->modal_state.data(), error);
    } else {
        ok = impl_->package->compute_optics_masked(cells_, levels_, num_bands, modal_species_, wavelengths,
                                                   impl_->temperature, impl_->pressure, impl_->air_density,
                                                   impl_->relative_humidity, thickness, impl_->modal_state.data(),
                                                   species_keep, num_keep, error);
    }
    return ok ? CATCHEM_SUCCESS : CATCHEM_INTERNAL_ERROR;
}

int OnDemandService::list_species(std::vector<std::string>& names, std::string& error) {
    if (!ready_) {
        error = "on-demand service: bring-up has not completed";
        return CATCHEM_INVALID_STATE;
    }
    names = impl_->modal_names;
    return CATCHEM_SUCCESS;
}

// Resolve a species label (modal type name or host short name) to a package
// species-axis index; -1 when neither the configuration nor the bound state
// carries it.
int OnDemandService::resolve_species(StateManager& state, const std::string& species) const {
    const std::string wanted = to_upper(species);
    for (int i = 0; i < modal_species_; ++i) {
        if (to_upper(impl_->modal_names[static_cast<std::size_t>(i)]) == wanted) {
            return i;
        }
    }
    for (const auto& map : impl_->slot_map) {
        const auto& meta = state.chemistry().species_list[static_cast<std::size_t>(map.first)];
        if (to_upper(meta.short_name) == wanted) {
            return map.second;
        }
    }
    return -1;
}

// --- read paths ---------------------------------------------------------------

int OnDemandService::read_pm(const std::string& species, const std::string& name, StateManager& state, double* values,
                             int* status_out, std::size_t values_len, std::string& error) {
    int rc = ensure_ready(state, error);
    if (rc != CATCHEM_SUCCESS) {
        return rc;
    }
    const QuantityInfo* info = find_quantity(name);
    if (info == nullptr || (info->kind != Kind::Diagnostic3D && info->kind != Kind::Diagnostic2D)) {
        error = "unknown particulate-matter quantity '" + name + "'; request a diagnostic quantity from the "
                "on-demand catalogue";
        return CATCHEM_INVALID_INDEX;
    }
    std::vector<int> keep;
    if (!species.empty()) {
        const int modal = resolve_species(state, species);
        if (modal < 0) {
            error = "species '" + species + "' is not an aerosol species of the bound state or the exaero "
                    "configuration";
            return CATCHEM_INVALID_INDEX;
        }
        keep.push_back(modal);
    }
    rc = assemble_inputs(state, error);
    if (rc != CATCHEM_SUCCESS) {
        return rc;
    }
    rc = run_diagnostics(keep.empty() ? nullptr : keep.data(), static_cast<int>(keep.size()), error);
    if (rc != CATCHEM_SUCCESS) {
        error = "on-demand diagnostics compute failed for '" + name + "': " + error;
        return rc;
    }

    const std::size_t points = (info->kind == Kind::Diagnostic3D)
                                   ? static_cast<std::size_t>(cells_) * static_cast<std::size_t>(levels_)
                                   : static_cast<std::size_t>(cells_);
    if (values_len < points) {
        error = "on-demand output buffer too small for '" + name + "': need " + std::to_string(points) + " values";
        return CATCHEM_EXTENT_MISMATCH;
    }
    const int slot = info->slot;
    for (std::size_t p = 0; p < points; ++p) {
        const int cell = static_cast<int>(p % static_cast<std::size_t>(cells_));
        const int level =
            (info->kind == Kind::Diagnostic3D) ? static_cast<int>(p / static_cast<std::size_t>(cells_)) : 0;
        double value = impl_->package->diagnostic(slot, cell, level);
        int status = kStatusAvailable;
        if (!std::isfinite(value)) {
            // The bridge answers NaN only for an out-of-range slot or a
            // missing slab -- never a real physical zero.  Report the absence
            // instead of dressing it up as an ok-flagged zero.
            value = 0.0;
            status = kStatusNotInSource;
        }
        values[p] = value;
        if (status_out != nullptr) {
            status_out[p] = status;
        }
    }
    return CATCHEM_SUCCESS;
}

int OnDemandService::read_pm_bulk(const std::vector<std::string>& names, StateManager& state, double* values,
                                  int* status_out, std::size_t values_len, std::string& error) {
    if (names.empty()) {
        error = "on-demand bulk PM request names no quantities";
        return CATCHEM_NULL_ARGUMENT;
    }
    // Validate the whole request before any compute: every name must be a
    // diagnostic-family quantity, so a typo fails loudly rather than
    // half-filling the buffer.
    std::vector<const QuantityInfo*> infos;
    infos.reserve(names.size());
    for (const std::string& name : names) {
        const QuantityInfo* info = find_quantity(name);
        if (info == nullptr || (info->kind != Kind::Diagnostic3D && info->kind != Kind::Diagnostic2D)) {
            error = "unknown particulate-matter quantity '" + name +
                    "'; request a diagnostic quantity from the on-demand catalogue";
            return CATCHEM_INVALID_INDEX;
        }
        infos.push_back(info);
    }
    int rc = ensure_ready(state, error);
    if (rc != CATCHEM_SUCCESS) {
        return rc;
    }
    rc = assemble_inputs(state, error);
    if (rc != CATCHEM_SUCCESS) {
        return rc;
    }
    rc = run_diagnostics(nullptr, 0, error);
    if (rc != CATCHEM_SUCCESS) {
        error = "on-demand diagnostics compute failed: " + error;
        return rc;
    }

    const std::size_t cells = static_cast<std::size_t>(cells_);
    const std::size_t levels = static_cast<std::size_t>(levels_);
    const std::size_t block = cells * levels;
    if (values_len < infos.size() * block) {
        error = "on-demand output buffer too small: " + std::to_string(infos.size()) + " names need " +
                std::to_string(infos.size() * block) + " values";
        return CATCHEM_EXTENT_MISMATCH;
    }
    for (std::size_t i = 0; i < infos.size(); ++i) {
        const QuantityInfo* info = infos[i];
        const int slot = info->slot;
        const std::size_t base = i * block;
        const std::size_t filled = (info->kind == Kind::Diagnostic3D) ? block : cells;
        for (std::size_t p = 0; p < block; ++p) {
            double value = 0.0;
            int status = kStatusNotInSource;
            if (p < filled) {
                const int cell = static_cast<int>(p % cells);
                const int level = (info->kind == Kind::Diagnostic3D) ? static_cast<int>(p / cells) : 0;
                value = impl_->package->diagnostic(slot, cell, level);
                status = kStatusAvailable;
                if (!std::isfinite(value)) {
                    value = 0.0;
                    status = kStatusNotInSource;
                }
            }
            values[base + p] = value;
            if (status_out != nullptr) {
                status_out[base + p] = status;
            }
        }
    }
    return CATCHEM_SUCCESS;
}

int OnDemandService::read_optics(const std::string& name, const double* wavelengths, int num_bands,
                                 StateManager& state, double* values, int* status_out, std::size_t values_len,
                                 std::string& error) {
    int rc = ensure_ready(state, error);
    if (rc != CATCHEM_SUCCESS) {
        return rc;
    }
    const QuantityInfo* info = find_quantity(name);
    if (info == nullptr ||
        (info->kind != Kind::Optical3D && info->kind != Kind::Optical2D && info->kind != Kind::OpticalSsa)) {
        error = "unknown optical quantity '" + name + "'; request an optics quantity from the on-demand catalogue";
        return CATCHEM_INVALID_INDEX;
    }
    if (wavelengths == nullptr || num_bands <= 0) {
        error = "optical quantity '" + name + "' requires at least one wavelength";
        return CATCHEM_NULL_ARGUMENT;
    }
    for (int b = 0; b < num_bands; ++b) {
        const double lambda = wavelengths[b];
        if (!std::isfinite(lambda) || lambda < band_min_m_ || lambda > band_max_m_) {
            error = "wavelength " + format_si(lambda) + " m for '" + name +
                    "' lies outside the configured optics band range [" + format_si(band_min_m_) + ", " +
                    format_si(band_max_m_) + "] m; request a wavelength inside the band";
            return CATCHEM_QUANTITY_NOT_AVAILABLE;
        }
    }
    rc = assemble_inputs(state, error);
    if (rc != CATCHEM_SUCCESS) {
        return rc;
    }
    rc = run_optics(wavelengths, num_bands, nullptr, 0, error);
    if (rc != CATCHEM_SUCCESS) {
        error = "on-demand optics compute failed for '" + name + "': " + error;
        return rc;
    }

    const std::size_t cells = static_cast<std::size_t>(cells_);
    const std::size_t levels = static_cast<std::size_t>(levels_);
    if (info->kind == Kind::Optical2D) {
        const std::size_t points = cells * static_cast<std::size_t>(num_bands);
        if (values_len < points) {
            error =
                "on-demand output buffer too small for '" + name + "': need " + std::to_string(points) + " values";
            return CATCHEM_EXTENT_MISMATCH;
        }
        for (int b = 0; b < num_bands; ++b) {
            for (std::size_t c = 0; c < cells; ++c) {
                double value = impl_->package->optic(info->slot, static_cast<int>(c), 0, b);
                int status = kStatusAvailable;
                if (!std::isfinite(value)) {
                    value = 0.0;
                    status = kStatusNotInSource;
                }
                const std::size_t idx = c + static_cast<std::size_t>(b) * cells;
                values[idx] = value;
                if (status_out != nullptr) {
                    status_out[idx] = status;
                }
            }
        }
        return CATCHEM_SUCCESS;
    }

    const std::size_t points = cells * levels * static_cast<std::size_t>(num_bands);
    if (values_len < points) {
        error = "on-demand output buffer too small for '" + name + "': need " + std::to_string(points) + " values";
        return CATCHEM_EXTENT_MISMATCH;
    }
    for (int b = 0; b < num_bands; ++b) {
        for (std::size_t level = 0; level < levels; ++level) {
            for (std::size_t c = 0; c < cells; ++c) {
                const std::size_t idx = c + level * cells + static_cast<std::size_t>(b) * cells * levels;
                double value = 0.0;
                int status = kStatusAvailable;
                if (info->kind == Kind::OpticalSsa) {
                    const double ext = impl_->package->optic(0, static_cast<int>(c), static_cast<int>(level), b);
                    const double sca = impl_->package->optic(1, static_cast<int>(c), static_cast<int>(level), b);
                    if (!std::isfinite(ext) || ext <= 0.0 || !std::isfinite(sca)) {
                        // An all-zero-mass column has no albedo: report the
                        // absence rather than inventing 0 or NaN.
                        value = 0.0;
                        status = kStatusNotInSource;
                    } else {
                        value = sca / ext;
                    }
                } else {
                    value = impl_->package->optic(info->slot, static_cast<int>(c), static_cast<int>(level), b);
                    if (!std::isfinite(value)) {
                        value = 0.0;
                        status = kStatusNotInSource;
                    }
                }
                values[idx] = value;
                if (status_out != nullptr) {
                    status_out[idx] = status;
                }
            }
        }
    }
    return CATCHEM_SUCCESS;
}

int OnDemandService::read_optics_bulk(const std::vector<std::string>& names, const double* wavelengths, int num_bands,
                                      StateManager& state, double* values, int* status_out, std::size_t values_len,
                                      std::string& error) {
    if (names.empty()) {
        error = "on-demand bulk optics request names no quantities";
        return CATCHEM_NULL_ARGUMENT;
    }
    std::vector<const QuantityInfo*> infos;
    infos.reserve(names.size());
    for (const std::string& name : names) {
        const QuantityInfo* info = find_quantity(name);
        if (info == nullptr ||
            (info->kind != Kind::Optical3D && info->kind != Kind::Optical2D && info->kind != Kind::OpticalSsa)) {
            error = "unknown optical quantity '" + name + "'; request an optics quantity from the on-demand catalogue";
            return CATCHEM_INVALID_INDEX;
        }
        infos.push_back(info);
    }
    if (wavelengths == nullptr || num_bands <= 0) {
        error = "an on-demand optics request requires at least one wavelength";
        return CATCHEM_NULL_ARGUMENT;
    }
    // Band bounds are only known after bring-up, so the out-of-band test runs
    // after ensure_ready -- never against an uninitialised range.
    int rc = ensure_ready(state, error);
    if (rc != CATCHEM_SUCCESS) {
        return rc;
    }
    for (int b = 0; b < num_bands; ++b) {
        const double lambda = wavelengths[b];
        if (!std::isfinite(lambda) || lambda < band_min_m_ || lambda > band_max_m_) {
            error = "wavelength " + format_si(lambda) + " m lies outside the configured optics band range [" +
                    format_si(band_min_m_) + ", " + format_si(band_max_m_) +
                    "] m; request a wavelength inside the band";
            return CATCHEM_QUANTITY_NOT_AVAILABLE;
        }
    }
    rc = assemble_inputs(state, error);
    if (rc != CATCHEM_SUCCESS) {
        return rc;
    }
    rc = run_optics(wavelengths, num_bands, nullptr, 0, error);
    if (rc != CATCHEM_SUCCESS) {
        error = "on-demand optics compute failed: " + error;
        return rc;
    }

    const std::size_t cells = static_cast<std::size_t>(cells_);
    const std::size_t levels = static_cast<std::size_t>(levels_);
    const std::size_t bands = static_cast<std::size_t>(num_bands);
    // Each coefficient (3D / derived-albedo) name occupies bands * cells *
    // levels points, band-major; each AOT (2D) name occupies bands * cells.
    std::size_t total = 0;
    for (const QuantityInfo* info : infos) {
        total += bands * cells * (info->kind == Kind::Optical2D ? 1 : levels);
    }
    if (values_len < total) {
        error = "on-demand output buffer too small: " + std::to_string(total) + " values required for " +
                std::to_string(infos.size()) + " optics names";
        return CATCHEM_EXTENT_MISMATCH;
    }
    std::size_t base = 0;
    for (const QuantityInfo* info : infos) {
        const int slot = info->slot;
        const std::size_t per_band = (info->kind == Kind::Optical2D) ? cells : cells * levels;
        for (std::size_t b = 0; b < bands; ++b) {
            for (std::size_t p = 0; p < per_band; ++p) {
                double value = 0.0;
                int status = kStatusAvailable;
                if (info->kind == Kind::Optical2D) {
                    value = impl_->package->optic(slot, static_cast<int>(p), 0, static_cast<int>(b));
                } else if (info->kind == Kind::OpticalSsa) {
                    const int cell = static_cast<int>(p % cells);
                    const int level = static_cast<int>(p / cells);
                    const double ext = impl_->package->optic(0, cell, level, static_cast<int>(b));
                    const double sca = impl_->package->optic(1, cell, level, static_cast<int>(b));
                    if (!std::isfinite(ext) || ext <= 0.0 || !std::isfinite(sca)) {
                        value = 0.0;
                        status = kStatusNotInSource;
                    } else {
                        value = sca / ext;
                    }
                } else {
                    const int cell = static_cast<int>(p % cells);
                    const int level = static_cast<int>(p / cells);
                    value = impl_->package->optic(slot, cell, level, static_cast<int>(b));
                }
                if (!std::isfinite(value)) {
                    value = 0.0;
                    status = kStatusNotInSource;
                }
                const std::size_t idx = base + b * per_band + p;
                values[idx] = value;
                if (status_out != nullptr) {
                    status_out[idx] = status;
                }
            }
        }
        base += bands * per_band;
    }
    return CATCHEM_SUCCESS;
}

int OnDemandService::read_quantity_by_species(const std::string& name, const double* wavelengths, int num_bands,
                                              const std::vector<std::string>& species, StateManager& state,
                                              double* values, int* status_out, std::size_t values_len,
                                              std::string& error) {
    if (species.empty()) {
        error = "an on-demand attribution request names no species; use the bulk form for the all-species value";
        return CATCHEM_NULL_ARGUMENT;
    }
    const QuantityInfo* info = find_quantity(name);
    if (info == nullptr) {
        error = "unknown on-demand quantity '" + name + "'; consult the catalogue via the enumeration endpoints";
        return CATCHEM_INVALID_INDEX;
    }
    const bool is_optics =
        (info->kind == Kind::Optical3D || info->kind == Kind::Optical2D || info->kind == Kind::OpticalSsa);
    if (is_optics && (wavelengths == nullptr || num_bands <= 0)) {
        error = "optical quantity '" + name + "' requires at least one wavelength";
        return CATCHEM_NULL_ARGUMENT;
    }

    int rc = ensure_ready(state, error);
    if (rc != CATCHEM_SUCCESS) {
        return rc;
    }

    // Resolve every species name through the modal-type / host-short-name
    // join before any compute, so a bad request fails loudly by name rather
    // than mid-attribution.  Duplicates would double-count a block.
    std::vector<int> keep;
    keep.reserve(species.size());
    for (const std::string& s : species) {
        const int modal = resolve_species(state, s);
        if (modal < 0) {
            error = "species '" + s + "' is not an aerosol species of the bound state or the exaero "
                    "configuration";
            return CATCHEM_INVALID_INDEX;
        }
        for (const int prev : keep) {
            if (prev == modal) {
                error = "species '" + s + "' is requested twice in one attribution; name each species once";
                return CATCHEM_INVALID_INDEX;
            }
        }
        keep.push_back(modal);
    }

    if (is_optics) {
        for (int b = 0; b < num_bands; ++b) {
            const double lambda = wavelengths[b];
            if (!std::isfinite(lambda) || lambda < band_min_m_ || lambda > band_max_m_) {
                error = "wavelength " + format_si(lambda) + " m lies outside the configured optics band range [" +
                        format_si(band_min_m_) + ", " + format_si(band_max_m_) +
                        "] m; request a wavelength inside the band";
                return CATCHEM_QUANTITY_NOT_AVAILABLE;
            }
        }
    }

    rc = assemble_inputs(state, error);
    if (rc != CATCHEM_SUCCESS) {
        return rc;
    }

    const std::size_t cells = static_cast<std::size_t>(cells_);
    const std::size_t levels = static_cast<std::size_t>(levels_);
    const std::size_t bands = is_optics ? static_cast<std::size_t>(num_bands) : 1;
    // Per-species block size follows the quantity's family: a diagnostic
    // covers (cell, level) or (cell); an optics coefficient or derived albedo
    // covers (cell, level) per band; an AOT covers (cell) per band.
    std::size_t block = 0;
    switch (info->kind) {
        case Kind::Diagnostic3D:
            block = cells * levels;
            break;
        case Kind::Diagnostic2D:
            block = cells;
            break;
        case Kind::Optical3D:
        case Kind::OpticalSsa:
            block = bands * cells * levels;
            break;
        case Kind::Optical2D:
            block = bands * cells;
            break;
    }
    const std::size_t total = keep.size() * block;
    if (values_len < total) {
        error = "on-demand output buffer too small: " + std::to_string(total) + " values required for " +
                std::to_string(keep.size()) + " species x " + std::to_string(block) + " points";
        return CATCHEM_EXTENT_MISMATCH;
    }

    const int slot = info->slot;
    // Curve availability per species: a label with a Mie curve in the library's
    // pinned source tables is backed by sourced data; one without is modelled
    // analytically.  This decides how a zero optics block must be reported, not
    // whether a nonzero analytical value may be returned -- a species can have
    // real optical properties without a sourced curve, and hiding that value
    // would break the species-summed-equals-bulk invariant.
    std::vector<char> curve_backed(keep.size(), 0);
    if (is_optics) {
        // A representative query point selects which curve value would back an
        // available result; it never decides whether a curve exists.
        const double oracle_rh = (impl_->relative_humidity != nullptr) ? impl_->relative_humidity[0] : 0.0;
        for (std::size_t s = 0; s < keep.size(); ++s) {
            int availability = kStatusNotInSource;
            std::string oracle_error;
            if (!impl_->package->query_optics_availability(keep[s], oracle_rh, wavelengths[0], availability,
                                                           oracle_error)) {
                error = "on-demand optics availability query failed: " + oracle_error;
                return CATCHEM_INTERNAL_ERROR;
            }
            curve_backed[s] = exaero_util::ExaeroPackageHandle::attribute_value_ok(availability) ? 1 : 0;
        }
    }

    for (std::size_t s = 0; s < keep.size(); ++s) {
        const int modal = keep[s];
        const std::size_t base = s * block;
        double* out = values + base;
        int* out_status = (status_out != nullptr) ? (status_out + base) : nullptr;

        const int one = modal;
        if (is_optics) {
            rc = run_optics(wavelengths, num_bands, &one, 1, error);
            if (rc != CATCHEM_SUCCESS) {
                error = "on-demand per-species optics compute failed for '" + name + "': " + error;
                return rc;
            }
            for (std::size_t b = 0; b < bands; ++b) {
                for (std::size_t p = 0; p < cells * (info->kind == Kind::Optical2D ? 1 : levels); ++p) {
                    const std::size_t per_band = (info->kind == Kind::Optical2D) ? cells : cells * levels;
                    double value = 0.0;
                    int status = kStatusAvailable;
                    if (info->kind == Kind::Optical2D) {
                        value = impl_->package->optic(slot, static_cast<int>(p), 0, static_cast<int>(b));
                    } else if (info->kind == Kind::OpticalSsa) {
                        const int cell = static_cast<int>(p % cells);
                        const int level = static_cast<int>(p / cells);
                        const double ext = impl_->package->optic(0, cell, level, static_cast<int>(b));
                        const double sca = impl_->package->optic(1, cell, level, static_cast<int>(b));
                        if (!std::isfinite(ext) || ext <= 0.0 || !std::isfinite(sca)) {
                            value = 0.0;
                            status = kStatusNotInSource;
                        } else {
                            value = sca / ext;
                        }
                    } else {
                        const int cell = static_cast<int>(p % cells);
                        const int level = static_cast<int>(p / cells);
                        value = impl_->package->optic(slot, cell, level, static_cast<int>(b));
                    }
                    if (!std::isfinite(value)) {
                        value = 0.0;
                        status = kStatusNotInSource;
                    } else if (value <= 0.0 && curve_backed[s] == 0) {
                        // No sourced curve and no modelled optical contribution:
                        // report the absence rather than an ok-flagged zero that
                        // a reader cannot distinguish from a real physical zero.
                        status = kStatusNotInSource;
                    }
                    const std::size_t idx = b * per_band + p;
                    out[idx] = value;
                    if (out_status != nullptr) {
                        out_status[idx] = status;
                    }
                }
            }
        } else {
            rc = run_diagnostics(&one, 1, error);
            if (rc != CATCHEM_SUCCESS) {
                error = "on-demand per-species diagnostics compute failed for '" + name + "': " + error;
                return rc;
            }
            for (std::size_t p = 0; p < block; ++p) {
                const int cell = static_cast<int>(p % cells);
                const int level = (info->kind == Kind::Diagnostic3D) ? static_cast<int>(p / cells) : 0;
                double value = impl_->package->diagnostic(slot, cell, level);
                int status = kStatusAvailable;
                if (!std::isfinite(value)) {
                    value = 0.0;
                    status = kStatusNotInSource;
                }
                out[p] = value;
                if (out_status != nullptr) {
                    out_status[p] = status;
                }
            }
        }
    }
    return CATCHEM_SUCCESS;
}

int OnDemandService::read_quantity(const std::string& name, const double* wavelengths, int num_bands,
                                   const std::string* species_names, int num_species_filter, StateManager& state,
                                   double* values, int* status_out, std::size_t values_len, std::string& error) {
    const QuantityInfo* info = find_quantity(name);
    if (info == nullptr) {
        error = "unknown on-demand quantity '" + name + "'; consult the catalogue via the enumeration endpoints";
        return CATCHEM_INVALID_INDEX;
    }
    std::string species;
    if (species_names != nullptr && num_species_filter > 0) {
        if (num_species_filter == 1) {
            species = species_names[0];
        } else {
            error = "on-demand general quantity form accepts at most one species filter; request each species "
                    "separately";
            return CATCHEM_INVALID_INDEX;
        }
    }
    switch (info->kind) {
        case Kind::Diagnostic3D:
        case Kind::Diagnostic2D:
            return read_pm(species, name, state, values, status_out, values_len, error);
        case Kind::Optical3D:
        case Kind::Optical2D:
        case Kind::OpticalSsa:
            return read_optics(name, wavelengths, num_bands, state, values, status_out, values_len, error);
    }
    error = "on-demand quantity '" + name + "' has an unsupported kind";
    return CATCHEM_INTERNAL_ERROR;
}

#else  // CATCHEM_HAVE_EXAERO not defined: the utility is not built in.

int OnDemandService::bring_up(StateManager& state, std::string& error) {
    (void)state;
    error = "the aerosol utility is not built into this CATChem library; "
            "reconfigure with -DCATCHEM_BUILD_EXAERO=ON";
    return CATCHEM_QUANTITY_NOT_AVAILABLE;
}

bool OnDemandService::validate_and_map_species(StateManager& state, std::string& error) {
    (void)state;
    error = "the aerosol utility is not built into this CATChem library; "
            "reconfigure with -DCATCHEM_BUILD_EXAERO=ON";
    return false;
}

int OnDemandService::assemble_inputs(StateManager& state, std::string& error) {
    (void)state;
    error = "the aerosol utility is not built into this CATChem library; "
            "reconfigure with -DCATCHEM_BUILD_EXAERO=ON";
    return CATCHEM_QUANTITY_NOT_AVAILABLE;
}

int OnDemandService::run_diagnostics(const int*, int, std::string&) {
    return CATCHEM_QUANTITY_NOT_AVAILABLE;
}

int OnDemandService::run_optics(const double*, int, const int*, int, std::string&) {
    return CATCHEM_QUANTITY_NOT_AVAILABLE;
}

int OnDemandService::list_species(std::vector<std::string>& names, std::string& error) {
    (void)names;
    error = "the aerosol utility is not built into this CATChem library; "
            "reconfigure with -DCATCHEM_BUILD_EXAERO=ON";
    return CATCHEM_QUANTITY_NOT_AVAILABLE;
}

int OnDemandService::resolve_species(StateManager& state, const std::string& species) const {
    (void)state;
    (void)species;
    return -1;
}

int OnDemandService::read_pm(const std::string&, const std::string&, StateManager&, double*, int*, std::size_t,
                             std::string& error) {
    error = "the aerosol utility is not built into this CATChem library; "
            "reconfigure with -DCATCHEM_BUILD_EXAERO=ON";
    return CATCHEM_QUANTITY_NOT_AVAILABLE;
}

int OnDemandService::read_pm_bulk(const std::vector<std::string>&, StateManager&, double*, int*, std::size_t,
                                  std::string& error) {
    error = "the aerosol utility is not built into this CATChem library; "
            "reconfigure with -DCATCHEM_BUILD_EXAERO=ON";
    return CATCHEM_QUANTITY_NOT_AVAILABLE;
}

int OnDemandService::read_optics(const std::string&, const double*, int, StateManager&, double*, int*, std::size_t,
                                 std::string& error) {
    error = "the aerosol utility is not built into this CATChem library; "
            "reconfigure with -DCATCHEM_BUILD_EXAERO=ON";
    return CATCHEM_QUANTITY_NOT_AVAILABLE;
}

int OnDemandService::read_optics_bulk(const std::vector<std::string>&, const double*, int, StateManager&, double*,
                                      int*, std::size_t, std::string& error) {
    error = "the aerosol utility is not built into this CATChem library; "
            "reconfigure with -DCATCHEM_BUILD_EXAERO=ON";
    return CATCHEM_QUANTITY_NOT_AVAILABLE;
}

int OnDemandService::read_quantity_by_species(const std::string&, const double*, int, const std::vector<std::string>&,
                                              StateManager&, double*, int*, std::size_t, std::string& error) {
    error = "the aerosol utility is not built into this CATChem library; "
            "reconfigure with -DCATCHEM_BUILD_EXAERO=ON";
    return CATCHEM_QUANTITY_NOT_AVAILABLE;
}

int OnDemandService::read_quantity(const std::string&, const double*, int, const std::string*, int, StateManager&,
                                   double*, int*, std::size_t, std::string& error) {
    error = "the aerosol utility is not built into this CATChem library; "
            "reconfigure with -DCATCHEM_BUILD_EXAERO=ON";
    return CATCHEM_QUANTITY_NOT_AVAILABLE;
}

#endif  // CATCHEM_HAVE_EXAERO

}  // namespace catchem

// catchem_exaero_bridge.cpp
//
// Implementation of the thin exaero utility bridge.  All library contact goes
// through exaero's extern "C" host surface (IAerosolPackage_C.h) plus the
// Kokkos-free Environment wrapper; the process layer never sees Kokkos or
// mdspan symbols through this translation unit's dependencies.
#include "catchem_exaero_bridge.hpp"

#include <exaero/AerosolIndices.hpp>
#include <exaero/AttributeQuery.hpp>
#include <exaero/Environment.hpp>
#include <exaero/IAerosolPackage_C.h>

#include <cmath>
#include <cstddef>
#include <fstream>
#include <limits>
#include <mutex>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

static_assert(exaero::diagnostic_indices::NUM_DIAGNOSTICS == catchem::exaero_util::kDiagnosticSlots,
              "bridge slab extent disagrees with the exaero diagnostic ABI");
static_assert(exaero::optical_indices::NUM_OPTICS == catchem::exaero_util::kOpticalSlots,
              "bridge optics slab extent disagrees with the exaero optics ABI");
static_assert(exaero::microphysical_indices::NUM_ATTRIBUTES == catchem::exaero_util::kMicrophysicalSlots,
              "bridge attribute extent disagrees with the exaero microphysical ABI");

namespace catchem {
    namespace exaero_util {

        namespace {

            // CCPP-style error surface used by every exaero C ABI call: a caller-sized
            // message buffer (the library writes at most 255 characters plus terminator)
            // and a 0/1 fatal flag.
            constexpr std::size_t kErrmsgSize = 256;

            // Reference-counted Kokkos runtime bring-up.  Each enabling process calls
            // init/finalize around its own lifecycle; the runtime is initialized on the
            // first init and finalized on the last matching finalize, so one process
            // tearing down cannot strand another mid-step.
            std::mutex g_env_mutex;
            int g_env_refs = 0;

        } // namespace

        int diagnostic_slot(const std::string& name) {
            if (name == "mass_concentration")
                return exaero::diagnostic_indices::MASS_CONCENTRATION;
            if (name == "pm2_5_concentration")
                return exaero::diagnostic_indices::PM2_5_CONCENTRATION;
            if (name == "pm10_concentration")
                return exaero::diagnostic_indices::PM10_CONCENTRATION;
            if (name == "number_concentration")
                return exaero::diagnostic_indices::NUMBER_CONCENTRATION;
            if (name == "surface_area_density")
                return exaero::diagnostic_indices::SURFACE_AREA_DENSITY;
            if (name == "aerosol_liquid_water")
                return exaero::diagnostic_indices::AEROSOL_LIQUID_WATER;
            if (name == "gravitational_settling_velocity")
                return exaero::diagnostic_indices::GRAVITATIONAL_SETTLING_VELOCITY;
            if (name == "surface_mass")
                return exaero::diagnostic_indices::SURFACE_MASS;
            if (name == "column_mass")
                return exaero::diagnostic_indices::COLUMN_MASS;
            if (name == "surface_pm2_5_mass")
                return exaero::diagnostic_indices::SURFACE_PM2_5_MASS;
            if (name == "column_pm2_5_mass")
                return exaero::diagnostic_indices::COLUMN_PM2_5_MASS;
            if (name == "pm1")
                return exaero::diagnostic_indices::PM1_CONCENTRATION;
            return -1;
        }

        int optical_slot(const std::string& name) {
            if (name == "extinction")
                return exaero::optical_indices::EXTINCTION_COEFF;
            if (name == "scattering")
                return exaero::optical_indices::SCATTERING_COEFF;
            if (name == "backscatter")
                return exaero::optical_indices::BACKSCATTER_COEFF;
            if (name == "asymmetry")
                return exaero::optical_indices::ASYMMETRY_FACTOR;
            if (name == "lidar_backscatter")
                return exaero::optical_indices::LIDAR_BACKSCATTER;
            if (name == "aod")
                return exaero::optical_indices::EXTINCTION_AOT;
            if (name == "scattering_aot")
                return exaero::optical_indices::SCATTERING_AOT;
            if (name == "fine_mode_aod")
                return exaero::optical_indices::FINE_MODE_EXTINCTION_AOT;
            if (name == "fine_mode_scattering_aot")
                return exaero::optical_indices::FINE_MODE_SCATTERING_AOT;
            if (name == "pm2_5_aod")
                return exaero::optical_indices::PM2_5_EXTINCTION_AOT;
            if (name == "pm2_5_scattering_aot")
                return exaero::optical_indices::PM2_5_SCATTERING_AOT;
            return -1;
        }

        void ExaeroPackageHandle::init_environment() {
            std::lock_guard<std::mutex> lock(g_env_mutex);
            if (g_env_refs == 0) {
                exaero::initialize_environment();
            }
            ++g_env_refs;
        }

        void ExaeroPackageHandle::finalize_environment() {
            std::lock_guard<std::mutex> lock(g_env_mutex);
            if (g_env_refs <= 0) {
                return; // unbalanced call: nothing was initialized by this owner
            }
            --g_env_refs;
            if (g_env_refs == 0) {
                exaero::finalize_environment();
            }
        }

        ExaeroPackageHandle::~ExaeroPackageHandle() {
            release();
        }

        ExaeroPackageHandle::ExaeroPackageHandle(ExaeroPackageHandle&& other) noexcept
            : package_(other.package_), slab_(std::move(other.slab_)), slab_cells_(other.slab_cells_),
              slab_levels_(other.slab_levels_), optics_slab_(std::move(other.optics_slab_)),
              optics_cells_(other.optics_cells_), optics_levels_(other.optics_levels_),
              optics_bands_(other.optics_bands_) {
            other.package_ = nullptr;
            other.slab_cells_ = 0;
            other.slab_levels_ = 0;
            other.optics_cells_ = 0;
            other.optics_levels_ = 0;
            other.optics_bands_ = 0;
        }

        ExaeroPackageHandle& ExaeroPackageHandle::operator=(ExaeroPackageHandle&& other) noexcept {
            if (this != &other) {
                release();
                package_ = other.package_;
                slab_ = std::move(other.slab_);
                slab_cells_ = other.slab_cells_;
                slab_levels_ = other.slab_levels_;
                optics_slab_ = std::move(other.optics_slab_);
                optics_cells_ = other.optics_cells_;
                optics_levels_ = other.optics_levels_;
                optics_bands_ = other.optics_bands_;
                other.package_ = nullptr;
                other.slab_cells_ = 0;
                other.slab_levels_ = 0;
                other.optics_cells_ = 0;
                other.optics_levels_ = 0;
                other.optics_bands_ = 0;
            }
            return *this;
        }

        bool ExaeroPackageHandle::initialize_from_yaml(const std::string& yaml_text, std::string& error) {
            error.clear();
            if (package_ != nullptr) {
                error = "exaero bridge: package already initialized";
                return false;
            }
            package_ = exaero_create_gocart_package();
            if (package_ == nullptr) {
                error = "exaero bridge: library failed to create a GOCART package";
                return false;
            }
            char errmsg[kErrmsgSize] = {0};
            int errflg = 1;
            exaero_initialize_package(package_, yaml_text.c_str(), errmsg, &errflg);
            if (errflg != 0) {
                error = std::string("exaero bridge: package initialization failed: ") + errmsg;
                release();
                return false;
            }
            return true;
        }

        bool ExaeroPackageHandle::initialize_from_file(const std::string& path, std::string& error) {
            std::ifstream input(path);
            if (!input.is_open()) {
                error = "exaero bridge: cannot open configuration file '" + path + "'";
                return false;
            }
            std::ostringstream buffer;
            buffer << input.rdbuf();
            return initialize_from_yaml(buffer.str(), error);
        }

        int ExaeroPackageHandle::species_index(const std::string& name) const {
            if (package_ == nullptr) {
                return -1;
            }
            return exaero_get_species_index(package_, name.c_str());
        }

        int ExaeroPackageHandle::species_count() const {
            if (package_ == nullptr) {
                return 0;
            }
            // The C ABI name accessor yields an empty name past the last
            // species, so probing forward counts the configured species.
            // The bound guards a pathological accessor; normal configurations
            // hold far fewer species than the cap.
            char name[64];
            int count = 0;
            while (count < 4096) {
                exaero_get_species_name(package_, count, name, static_cast<int>(sizeof(name)));
                if (name[0] == '\0') {
                    break;
                }
                ++count;
            }
            return count;
        }

        std::string ExaeroPackageHandle::species_name(int index) const {
            if (package_ == nullptr || index < 0) {
                return {};
            }
            char name[64];
            exaero_get_species_name(package_, index, name, static_cast<int>(sizeof(name)));
            return std::string(name);
        }

        bool ExaeroPackageHandle::compute_diagnostics(int num_cells, int num_levels, int num_species,
                                                      const double* temperature, const double* pressure,
                                                      const double* air_density, const double* relative_humidity,
                                                      const double* layer_thickness, const double* state,
                                                      std::string& error) {
            error.clear();
            if (package_ == nullptr) {
                error = "exaero bridge: compute requested before the package was "
                        "initialized";
                return false;
            }
            if (num_cells <= 0 || num_levels <= 0 || num_species <= 0) {
                error = "exaero bridge: non-positive grid extent";
                return false;
            }
            if (temperature == nullptr || pressure == nullptr || air_density == nullptr ||
                relative_humidity == nullptr || layer_thickness == nullptr || state == nullptr) {
                error = "exaero bridge: null input field";
                return false;
            }
            slab_.assign(static_cast<std::size_t>(num_cells) * num_levels * kDiagnosticSlots, 0.0);
            char errmsg[kErrmsgSize] = {0};
            int errflg = 1;
            exaero_compute_diagnostics(package_, num_cells, num_levels, num_species, temperature, pressure, air_density,
                                       relative_humidity, layer_thickness, state, slab_.data(), errmsg, &errflg);
            if (errflg != 0) {
                error = std::string("exaero bridge: diagnostics compute failed: ") + errmsg;
                slab_.clear();
                slab_cells_ = 0;
                slab_levels_ = 0;
                return false;
            }
            // The C ABI entry does not fence; drain the device before any host read of
            // the slab so the result is correct on every execution space.
            exaero::fence_environment();
            slab_cells_ = num_cells;
            slab_levels_ = num_levels;
            return true;
        }

        double ExaeroPackageHandle::diagnostic(int slot, int cell, int level) const {
            if (slot < 0 || slot >= kDiagnosticSlots || cell < 0 || level < 0 || cell >= slab_cells_ ||
                level >= slab_levels_ || slab_.empty()) {
                return std::numeric_limits<double>::quiet_NaN();
            }
            // Column-major (cell, level, slot): cell varies fastest.
            return slab_[static_cast<std::size_t>(slot) * slab_levels_ * slab_cells_ +
                         static_cast<std::size_t>(level) * slab_cells_ + static_cast<std::size_t>(cell)];
        }

        bool ExaeroPackageHandle::compute_optics(int num_cells, int num_levels, int num_bands, int num_species,
                                                 const double* wavelengths, const double* temperature,
                                                 const double* pressure, const double* air_density,
                                                 const double* relative_humidity, const double* layer_thickness,
                                                 const double* state, std::string& error) {
            error.clear();
            if (package_ == nullptr) {
                error = "exaero bridge: optics compute requested before the package was initialized";
                return false;
            }
            if (num_cells <= 0 || num_levels <= 0 || num_bands <= 0 || num_species <= 0) {
                error = "exaero bridge: non-positive optics grid extent";
                return false;
            }
            if (wavelengths == nullptr || temperature == nullptr || pressure == nullptr || air_density == nullptr ||
                relative_humidity == nullptr || layer_thickness == nullptr || state == nullptr) {
                error = "exaero bridge: null optics input field";
                return false;
            }
            optics_slab_.assign(static_cast<std::size_t>(num_cells) * num_levels * num_bands * kOpticalSlots, 0.0);
            char errmsg[kErrmsgSize] = {0};
            int errflg = 1;
            exaero_compute_optics(package_, num_cells, num_levels, num_bands, num_species, wavelengths, temperature,
                                  pressure, air_density, relative_humidity, layer_thickness, state, optics_slab_.data(),
                                  errmsg, &errflg);
            if (errflg != 0) {
                // The library reports out-of-band wavelengths here; its message names
                // the offending value, so forward it verbatim rather than clamping.
                error = std::string("exaero bridge: optics compute failed: ") + errmsg;
                optics_slab_.clear();
                optics_cells_ = 0;
                optics_levels_ = 0;
                optics_bands_ = 0;
                return false;
            }
            // Fence before host reads, matching the diagnostics path.
            exaero::fence_environment();
            optics_cells_ = num_cells;
            optics_levels_ = num_levels;
            optics_bands_ = num_bands;
            return true;
        }

        double ExaeroPackageHandle::optic(int slot, int cell, int level, int band) const {
            if (slot < 0 || slot >= kOpticalSlots || cell < 0 || level < 0 || band < 0 || cell >= optics_cells_ ||
                level >= optics_levels_ || band >= optics_bands_ || optics_slab_.empty()) {
                return std::numeric_limits<double>::quiet_NaN();
            }
            // Column-major (cell, level, band, slot): cell varies fastest.
            const std::size_t idx =
                (((static_cast<std::size_t>(slot) * optics_bands_ + static_cast<std::size_t>(band)) * optics_levels_ +
                  static_cast<std::size_t>(level)) *
                     optics_cells_ +
                 static_cast<std::size_t>(cell));
            return optics_slab_[idx];
        }

        namespace {

            // Copy 'state' (column-major (cell, level, species)) with every species
            // axis NOT listed in 'keep' zeroed, so a masked kernel pass yields exactly
            // the kept species' contribution.  Returns false on a malformed index list
            // (out-of-range or duplicate) rather than computing a wrong mask.
            bool masked_species_state(const double* state, int num_cells, int num_levels, int num_species,
                                      const int* keep, int num_keep, std::vector<double>& out) {
                const std::size_t points = static_cast<std::size_t>(num_cells) * num_levels;
                out.assign(points * static_cast<std::size_t>(num_species), 0.0);
                std::vector<bool> selected(static_cast<std::size_t>(num_species), false);
                for (int i = 0; i < num_keep; ++i) {
                    const int s = keep[i];
                    if (s < 0 || s >= num_species || selected[static_cast<std::size_t>(s)]) {
                        return false;
                    }
                    selected[static_cast<std::size_t>(s)] = true;
                }
                for (int s = 0; s < num_species; ++s) {
                    if (!selected[static_cast<std::size_t>(s)]) {
                        continue;
                    }
                    const double* src = state + static_cast<std::size_t>(s) * points;
                    double* dst = out.data() + static_cast<std::size_t>(s) * points;
                    for (std::size_t p = 0; p < points; ++p) {
                        dst[p] = src[p];
                    }
                }
                return true;
            }

        } // namespace

        bool ExaeroPackageHandle::compute_diagnostics_masked(int num_cells, int num_levels, int num_species,
                                                             const double* temperature, const double* pressure,
                                                             const double* air_density, const double* relative_humidity,
                                                             const double* layer_thickness, const double* state,
                                                             const int* species_keep, int num_keep, std::string& error) {
            std::vector<double> masked;
            if (!masked_species_state(state, num_cells, num_levels, num_species, species_keep, num_keep, masked)) {
                error = "exaero bridge: masked diagnostics request names an out-of-range or duplicate species index";
                return false;
            }
            return compute_diagnostics(num_cells, num_levels, num_species, temperature, pressure, air_density,
                                       relative_humidity, layer_thickness, masked.data(), error);
        }

        bool ExaeroPackageHandle::compute_optics_masked(int num_cells, int num_levels, int num_bands, int num_species,
                                                        const double* wavelengths, const double* temperature,
                                                        const double* pressure, const double* air_density,
                                                        const double* relative_humidity, const double* layer_thickness,
                                                        const double* state, const int* species_keep, int num_keep,
                                                        std::string& error) {
            std::vector<double> masked;
            if (!masked_species_state(state, num_cells, num_levels, num_species, species_keep, num_keep, masked)) {
                error = "exaero bridge: masked optics request names an out-of-range or duplicate species index";
                return false;
            }
            return compute_optics(num_cells, num_levels, num_bands, num_species, wavelengths, temperature, pressure,
                                  air_density, relative_humidity, layer_thickness, masked.data(), error);
        }

        bool ExaeroPackageHandle::attribute_value_ok(int status) {
            switch (status) {
            case kAttributeAvailable:
            case kAttributeAvailableFile:
            case kAttributeInterpolated:
            case kAttributeAvailableConfig:
                return true;
            default:
                return false;
            }
        }

        bool ExaeroPackageHandle::compute_wet_sizing(int num_cells, int num_levels, const double* relative_humidity,
                                                     double radius_dry, double density_dry, int species_index,
                                                     std::vector<double>& radius_wet_out,
                                                     std::vector<double>& density_wet_out,
                                                     std::vector<int>& status_out, std::string& error) {
            error.clear();
            if (package_ == nullptr) {
                error = "exaero bridge: wet sizing requested before the package was initialized";
                return false;
            }
            if (num_cells <= 0 || num_levels <= 0) {
                error = "exaero bridge: non-positive grid extent";
                return false;
            }
            if (relative_humidity == nullptr) {
                error = "exaero bridge: null relative-humidity field";
                return false;
            }
            if (species_index < 0) {
                error = "exaero bridge: negative species index";
                return false;
            }
            if (!(radius_dry > 0.0) || !(density_dry > 0.0)) {
                error = "exaero bridge: non-positive dry particle sizing";
                return false;
            }
            const std::size_t points = static_cast<std::size_t>(num_cells) * num_levels;
            radius_wet_out.assign(points, 0.0);
            density_wet_out.assign(points, 0.0);
            status_out.assign(points, kAttributeNotInSource);
            char errmsg[kErrmsgSize] = {0};
            int errflg = 1;
            // The library resolves the curve growth factor per point and anchors
            // the mixing on the caller's dry particle; a species with no curve
            // reports NotInSource and leaves radius/density at zero.
            exaero_compute_wet_sizing(package_, static_cast<int>(points), relative_humidity, species_index, radius_dry,
                                      density_dry, radius_wet_out.data(), density_wet_out.data(), status_out.data(),
                                      errmsg, &errflg);
            if (errflg != 0) {
                error = std::string("exaero bridge: wet sizing compute failed: ") + errmsg;
                return false;
            }
            // Fence before host reads, matching the diagnostics path.
            exaero::fence_environment();
            return true;
        }

        void ExaeroPackageHandle::release() {
            if (package_ != nullptr) {
                exaero_free_package(package_);
                package_ = nullptr;
            }
            slab_.clear();
            slab_cells_ = 0;
            slab_levels_ = 0;
        }

    } // namespace exaero_util
} // namespace catchem

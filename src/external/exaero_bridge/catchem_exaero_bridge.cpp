// catchem_exaero_bridge.cpp
//
// Implementation of the thin exaero utility bridge.  All library contact goes
// through exaero's extern "C" host surface (IAerosolPackage_C.h) plus the
// Kokkos-free Environment wrapper; the process layer never sees Kokkos or
// mdspan symbols through this translation unit's dependencies.
#include "catchem_exaero_bridge.hpp"

#include <exaero/AerosolIndices.hpp>
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

static_assert(exaero::diagnostic_indices::NUM_DIAGNOSTICS == catchem::exaero_util::kDiagnosticSlots,
              "bridge slab extent disagrees with the exaero diagnostic ABI");

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
              slab_levels_(other.slab_levels_) {
            other.package_ = nullptr;
            other.slab_cells_ = 0;
            other.slab_levels_ = 0;
        }

        ExaeroPackageHandle& ExaeroPackageHandle::operator=(ExaeroPackageHandle&& other) noexcept {
            if (this != &other) {
                release();
                package_ = other.package_;
                slab_ = std::move(other.slab_);
                slab_cells_ = other.slab_cells_;
                slab_levels_ = other.slab_levels_;
                other.package_ = nullptr;
                other.slab_cells_ = 0;
                other.slab_levels_ = 0;
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

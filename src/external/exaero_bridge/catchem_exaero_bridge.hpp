// catchem_exaero_bridge.hpp
//
// Thin C++ lifecycle + gather helper that lets a CATChem process own one
// exaero aerosol package and read its derived-diagnostics slab.  It is a
// supplementary utility: a process that does not enable it keeps its existing
// static-property science path unchanged.
//
// Why the C ABI rather than the C++ interface header: exaero's concrete
// GocartPackage class is a private include of the library target, so a
// consumer cannot construct one directly.  The extern "C" surface
// (exaero_create_gocart_package / exaero_compute_diagnostics / ...) is the
// documented host boundary and the only reachable construction path.  It is
// also Kokkos- and mdspan-free, so this header pulls neither dependency into
// the process layer, and the opaque handle it yields is exactly the value the
// Fortran exaero_interface module passes around -- a scheme can reuse the
// same handle through the Fortran binding without a second C++ shim.
//
// The fence question: the C ABI compute entry points do not fence the device
// before returning.  On the OpenMP/host execution space the launch is already
// synchronous, but the bridge fences explicitly through exaero's Kokkos-free
// Environment wrapper so a host read of the slab is correct on any backend.
//
// Layout contract (explicit, never resolved by a silent transpose): the
// aerosol state and the diagnostic slab are Fortran column-major.  State is
// (cell, level, species); the slab is (cell, level, diagnostic_slot) with the
// slot order fixed by exaero's normative diagnostic indices.  A flat pointer
// from a Fortran (cell, level, species) array is forwarded zero-copy.
#pragma once

#include <string>
#include <vector>

namespace catchem {
    namespace exaero_util {

        // Total diagnostic slots in the output slab (the slab's third extent).
        // Mirrors exaero's NUM_DIAGNOSTICS; kept here so the header stays free of the
        // library include.  Asserted equal to the library constant in the .cpp.
        inline constexpr int kDiagnosticSlots = 11;

        // Total microphysical attribute slots queried per (cell, level) point.
        // Mirrors exaero's microphysical NUM_ATTRIBUTES; asserted equal in the .cpp.
        inline constexpr int kMicrophysicalSlots = 9;

        // Map a configuration diagnostic name to its exaero slot code.  Names are the
        // lower-snake_case forms used in process configuration (for example
        // "gravitational_settling_velocity", "number_concentration").  Returns -1 for
        // an unknown name so the caller can fail fast rather than read slot -1.
        int diagnostic_slot(const std::string& name);

        // Owns one exaero package handle for a single enabling process, plus the host
        // staging for the most recent diagnostics slab.  Non-copyable (one handle, one
        // owner); movable so it can live in a process object's storage.
        class ExaeroPackageHandle {
        public:
            ExaeroPackageHandle() = default;
            ~ExaeroPackageHandle();

            ExaeroPackageHandle(const ExaeroPackageHandle&) = delete;
            ExaeroPackageHandle& operator=(const ExaeroPackageHandle&) = delete;
            ExaeroPackageHandle(ExaeroPackageHandle&& other) noexcept;
            ExaeroPackageHandle& operator=(ExaeroPackageHandle&& other) noexcept;

            // One-time Kokkos runtime bring-up / teardown.  Reference-counted so several
            // enabling processes each call init/finalize without double-initializing or
            // tearing the runtime down while another process still needs it.
            static void init_environment();
            static void finalize_environment();

            // Create the package and initialize it from a YAML configuration document
            // (inline text).  On any library error the handle is released and false is
            // returned with a filled 'error' message -- never a silent half-initialized
            // handle.
            bool initialize_from_yaml(const std::string& yaml_text, std::string& error);

            // Create the package and initialize it from a YAML file at 'path'.  Reads
            // the file then delegates to initialize_from_yaml, so a configuration that
            // names an exaero species set can live in its own file.
            bool initialize_from_file(const std::string& path, std::string& error);

            // Resolve a species label to the package's packed species-axis index
            // (zero-based; -1 when the label is absent from the active configuration).
            int species_index(const std::string& name) const;

            // Run the derived-diagnostics step for one column block.  'state' is a flat
            // column-major (cell, level, species) array of mass concentrations
            // [kg m^-3]; the five environmental arrays are column-major (cell, level).
            // The resulting (cell, level, slot) slab is retained internally for
            // diagnostic() reads.  Returns false and fills 'error' on failure.
            bool compute_diagnostics(int num_cells, int num_levels, int num_species, const double* temperature,
                                     const double* pressure, const double* air_density, const double* relative_humidity,
                                     const double* layer_thickness, const double* state, std::string& error);

            // Read one diagnostic slot at (cell, level) from the last compute.  'slot'
            // is an exaero diagnostic index (see diagnostic_slot).  Returns NaN when no
            // slab is present or the indices are out of range, so a missing compute
            // surfaces as a non-finite value rather than a stale or zero reading.
            double diagnostic(int slot, int cell, int level) const;

            // Per-point availability codes of an exaero attribute query, mirrored
            // from the library's AttributeStatus enum so consumers do not need the
            // library include.  A value is usable when attribute_value_ok() is true;
            // the other codes say why a value is absent (no curve for the species,
            // category deselected) and must not be read as zero.
            static constexpr int kAttributeAvailable = 0;
            static constexpr int kAttributeAvailableFile = 1;
            static constexpr int kAttributeInterpolated = 2;
            static constexpr int kAttributeNotActivated = 3;
            static constexpr int kAttributeNotInSource = 4;
            static constexpr int kAttributeAvailableConfig = 5;

            // True when an attribute query status carries a usable value.
            static bool attribute_value_ok(int status);

            // Size one species' wet particle over a (cell, level) block,
            // anchored on the CALLER's dry particle: the library resolves its
            // curve's hygroscopic growth factor at each point's relative
            // humidity and applies it to 'radius_dry'/'density_dry', so a
            // growth of exactly 1 returns the dry particle bit-for-bit.  The
            // met fields other than 'relative_humidity' are not needed; the
            // RH array is column-major (cell, level), the same flat layout a
            // Fortran-consumer host keeps its state in.  The outputs are sized
            // num_cells*num_levels in that order; 'status_out' carries the
            // per-point availability code above (points without a usable
            // value leave radius/density at 0 and must fall back to another
            // sizing source).  Returns false and fills 'error' only when the
            // library call itself failed.
            bool compute_wet_sizing(int num_cells, int num_levels, const double* relative_humidity, double radius_dry,
                                    double density_dry, int species_index, std::vector<double>& radius_wet_out,
                                    std::vector<double>& density_wet_out, std::vector<int>& status_out,
                                    std::string& error);

            // The opaque exaero handle for a Fortran scheme that calls the compute
            // directly through the exaero_interface module.  Null before
            // initialize_from_* and after release.
            void* raw_handle() const { return package_; }
            bool valid() const { return package_ != nullptr; }

            // Release the package handle (idempotent; safe on a default-constructed
            // object).  Called by the destructor.
            void release();

        private:
            void* package_ = nullptr;  // exaero_package_t (opaque void*)
            std::vector<double> slab_; // (cell, level, slot), column-major
            int slab_cells_ = 0;
            int slab_levels_ = 0;
        };

    } // namespace exaero_util
} // namespace catchem

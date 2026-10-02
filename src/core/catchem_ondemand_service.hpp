#ifndef CATCHEM_ONDEMAND_SERVICE_HPP
#define CATCHEM_ONDEMAND_SERVICE_HPP

#include <catchem_api.hpp>  // CATCHEM_* status codes

#include <cstddef>
#include <cstdint>
#include <memory>
#include <string>
#include <vector>

namespace catchem {

class StateManager;  // owns chem/met state, column/level counts, read_field

namespace exaero_util {
class ExaeroPackageHandle;  // external/exaero_bridge lifecycle + slab wrapper
}

/**
 * @brief What a quantity value covers: a 3D field, a 2D column-integrated
 * field, a scalar, or a derived ratio. Selects the output shape and path.
 */
enum class OnDemandKind {
    Diagnostic3D,  ///< value[cell + level * cells] from the diagnostics slab
    Diagnostic2D,  ///< value[cell] (level-0 column/surface field)
    Optical3D,     ///< value[cell + level * cells + band * cells * levels]
    Optical2D,     ///< value[cell + band * cells] (AOT, level-0 column field)
    OpticalSsa,    ///< derived scattering/extinction, 3D shape, zero-guarded
};

/**
 * @brief One entry of the normative quantity registry (the on-demand API's
 * catalogue). Names are the public strings; slot indexes the library slab.
 */
struct QuantityInfo {
    const char* name;
    OnDemandKind kind;
    int slot;        ///< library diagnostic/optics slot; -1 for derived
    const char* units;
};

/**
 * @brief On-demand aerosol diagnostics service (optics library-backed).
 *
 * Owns a lazily brought-up optics package and answers host-model quantity
 * requests ("give me pm2.5 / AOD / extinction here and now") WITHOUT
 * advancing any state: every read path is pure — compute into internal
 * scratch, copy out. The package is configured once from a library species
 * YAML and validated against the host species registry (each host aerosol
 * must map, via its Mie name, onto a configured modal species).
 *
 * Host state semantics: chemistry conc is a mixing ratio [µg/kg]; the
 * library wants per-species mass concentration [kg/m³], so the service
 * converts conc × 1e-9 × air_density and sums transport bins sharing one Mie
 * name into the modal slot. Meteorological inputs follow the layer convention
 * of the existing process kernels (T, PMID, RH, BXHEIGHT at the same
 * (cell, level)); a missing layer thickness falls back to the DELP-derived
 * field the state manager exposes.
 *
 * Thread safety: bring-up is idempotent; compute paths reuse the single
 * package slab and are intended for the API boundary's serial calls.
 */
class OnDemandService {
  public:
    OnDemandService();
    ~OnDemandService();
    OnDemandService(const OnDemandService&) = delete;
    OnDemandService& operator=(const OnDemandService&) = delete;
    OnDemandService(OnDemandService&&) noexcept;
    OnDemandService& operator=(OnDemandService&&) noexcept;

    /**
     * @brief Configure from the on-demand config node. Records the species
     * YAML path; performs no heavy work (bring-up is lazy).
     * @return CATCHEM_SUCCESS, or CATCHEM_INVALID_CONFIGURATION if enabled
     *         without a species config path.
     */
    int configure(const std::string& exaero_config_path, bool enabled);

    /** True when the on-demand utility is configured and enabled. */
    bool configured() const { return enabled_; }

    /**
     * @brief Lazy, idempotent bring-up against the current state (validates
     * the host species registry against the package on first call).
     * @return CATCHEM_SUCCESS or a failure status.
     */
    int ensure_ready(StateManager& state, std::string& error);

    /** Public catalogue: total registered quantity count. */
    static int quantity_count();
    /**
     * @brief True when the aerosol utility is built into this library.
     *
     * The catalogue itself is static; this reports whether the compute path
     * behind it exists, so enumeration endpoints can answer "0 quantities,
     * not available" (the gate-OFF remedy) instead of a per-call surprise.
     */
    static bool available();
    /** Public catalogue: quantity name at index, or nullptr past the end. */
    static const char* quantity_name_at(int index);
    /** Public catalogue: units at index, or nullptr past the end. */
    static const char* quantity_units_at(int index);
    /** Public catalogue: lookup by name; returns nullptr when unknown. */
    static const QuantityInfo* find_quantity(const std::string& name);

    /**
     * @brief Enumerate the configured modal species names (package order).
     * @return CATCHEM_SUCCESS or a failure status if not ready.
     */
    int list_species(std::vector<std::string>& names, std::string& error);

    /**
     * @brief Read a PM-family quantity for one named species ("" = bulk sum,
     * a name = that species' masked contribution). Values are laid out
     * (cell, level) for 3D kinds, (cell) for 2D kinds; 'status_out' (may be
     * null) receives the per-point availability code.
     * @return CATCHEM_SUCCESS, CATCHEM_QUANTITY_NOT_AVAILABLE, or a failure.
     */
    int read_pm(const std::string& species, const std::string& name, StateManager& state, double* values,
                int* status_out, std::size_t values_len, std::string& error);

    /**
     * @brief Read several bulk PM-family quantities in one compute. The
     * output is name-major: the block for name i starts at
     * i * (cells * levels) and holds that quantity's (cell, level) grid.
     * A 2D (surface/column) diagnostic fills only the first 'cells' slots of
     * its block; the remaining points report NotInSource, as no value exists
     * for them. An unknown or non-diagnostic name fails with
     * CATCHEM_INVALID_INDEX naming it, before any compute runs.
     */
    int read_pm_bulk(const std::vector<std::string>& names, StateManager& state, double* values, int* status_out,
                     std::size_t values_len, std::string& error);

    /**
     * @brief Read an optical quantity at one or more wavelengths.
     * Coefficient (3D) kinds are band-major [band][level][cell]; AOT (2D)
     * kinds are [band][cell]. 'status_out' (may be null) receives the
     * per-point availability code.
     * @return CATCHEM_SUCCESS, CATCHEM_QUANTITY_NOT_AVAILABLE (out-of-band
     *         wavelength), or a failure status.
     */
    int read_optics(const std::string& name, const double* wavelengths, int num_bands, StateManager& state,
                    double* values, int* status_out, std::size_t values_len, std::string& error);

    /**
     * @brief General dispatch: any registered quantity. Diagnostic names
     * ignore wavelengths; optics names require them. A non-empty species list
     * narrows either family to the named species' masked contribution.
     */
    int read_quantity(const std::string& name, const double* wavelengths, int num_bands,
                      const std::string* species_names, int num_species_filter, StateManager& state, double* values,
                      int* status_out, std::size_t values_len, std::string& error);

  private:
    int bring_up(StateManager& state, std::string& error);
    bool validate_and_map_species(StateManager& state, std::string& error);
    /// Convert host state into library layout (modal-aggregated [kg/m³]).
    int assemble_inputs(StateManager& state, std::string& error);
    /// Run a diagnostics compute (bulk or species-masked) into the slab.
    int run_diagnostics(const int* species_keep, int num_keep, std::string& error);
    /// Run an optics compute (bulk or species-masked) into the slab.
    int run_optics(const double* wavelengths, int num_bands, const int* species_keep, int num_keep,
                   std::string& error);
    /// Resolve a species label (modal type or host short name) to a package
    /// species-axis index; -1 when neither carries it.
    int resolve_species(StateManager& state, const std::string& species) const;

    struct HostToModal {
        int host_index;  ///< index into chem.conc species axis
        int modal_slot;  ///< index into the package species axis
    };

    // Library-facing state (package handle, species map, request staging)
    // lives behind a pimpl so this header stays free of the bridge include
    // and the class layout is identical whether or not the utility is built.
    struct Impl;
    std::unique_ptr<Impl> impl_;

    std::string exaero_config_path_;
    bool enabled_ = false;
    bool env_owned_ = false;  ///< whether this service holds a Kokkos env ref
    bool ready_ = false;      ///< package up and species mapping validated

    int cells_ = 0;
    int levels_ = 0;
    int modal_species_ = 0;  ///< package species count
    double band_min_m_ = 0.0;
    double band_max_m_ = 0.0;
};

}  // namespace catchem

#endif  // CATCHEM_ONDEMAND_SERVICE_HPP

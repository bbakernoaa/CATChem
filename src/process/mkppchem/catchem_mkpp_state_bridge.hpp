// src/process/mkppchem/catchem_mkpp_state_bridge.hpp
//
// Device-resident staging bridge between CATChem's (column, level, species)
// LayoutLeft concentration field and the MKPP generated solver's per-cell
// contiguous species vector.
//
// Layout contract (explicit, never resolved by silent transposition):
//   CATChem `conc`: (col, lev, species) LayoutLeft -> strides (1, ncol,
//     ncol*nlev). Species is the SLOWEST-varying dimension.
//   `species_buf`:  (cell, species) LayoutRight    -> strides (num_species, 1).
//     Species is CONTIGUOUS within a cell row (stride 1), which is what the
//     generated `state.data()[0..NUM_SPECIES-1]` linear indexing requires; a
//     LayoutLeft (cell, species) buffer would make the species axis strided by
//     num_cells and `state.data()[i]` would read the wrong species.
//   The gather/scatter kernels therefore perform an explicit stride change.
//   They are `Kokkos::parallel_for` over cells with both endpoints in the
//   active execution space's memory space, so on the GPU pathway nothing
//   touches host memory, and on OpenMP (HostSpace == DeviceSpace) the repack
//   is same-space with no transfer at all.
//
// Cell indexing follows CATChem's existing flattening convention
//   i_cell = ilev * ncol + icol
// so a reported failing cell index decodes unambiguously to (column, level).
//
// Unit conversion is owned by this process and
// applied inside the gather/scatter kernels:
//   inbound : c_molar = c_mol_per_mol * rho_molar_dry
//   outbound: c_mol_per_mol = c_molar / rho_molar_dry
// with rho_molar_dry = AIRDEN_DRY / (AIR_MW * 1e-3), computed once per cell.
// NOTE the deliberate divergence from gaschem, whose contract is ppmv: there
// are no 1e-6 / 1e6 factors here. CATChem's CONCENTRATION contract unit is
// mol/mol.
#pragma once

#ifdef CATCHEM_ENABLE_KOKKOS

#include "catchem_constants.hpp"
#include "catchem_mkpp_mechanism.hpp"

#include <Kokkos_Core.hpp>

#include <cmath>
#include <cstdint>
#include <vector>

namespace catchem::mkpp {

    /// Error codes for the CATChem-owned per-cell status view. Mirrors MKPP's
    /// own CellErrorCode (dispatcher.hpp) so the codes are familiar, but is
    /// owned here because the generated `integrate` returns void.
    enum CellError : int {
        CELL_SUCCESS = 0,
        CELL_ERR_NON_FINITE_INPUT = 1,
        CELL_ERR_NUMERICAL_FAILURE = 2,
        CELL_ERR_NON_PHYSICAL_MET = 3
    };

    /// Sentinel for an unfilled error slot, so a `Kokkos::Min<int>` reduction
    /// over the error view is order-independent and reports the LOWEST failing
    /// cell index.
    inline constexpr int kNoError = 0x7FFFFFFF;

    /// Persistent, process-owned device staging buffers. Allocated once in
    /// init() and reused every timestep (no per-timestep allocation).
    class StagingBuffers {
    public:
        using ExecSpace = Kokkos::DefaultExecutionSpace;
        using MemSpace = ExecSpace::memory_space;

        StagingBuffers() = default;

        /// Size and allocate for a grid of `num_cells` cells. `num_species` is
        /// the compiled mechanism's NUM_SPECIES, `num_photolysis` its
        /// NUM_PHOTOLYSIS, and `num_eq_inputs` its declared EquilibriumInput
        /// count -- table-driven, so a regenerated artifact with a third
        /// (cloud-water) input needs no source change.
        void allocate(int num_cells, int num_species, int num_photolysis, int num_eq_inputs);

        int num_cells() const noexcept { return num_cells_; }
        int num_species() const noexcept { return num_species_; }
        // clang-format off
        // Per-cell 2-D staging buffers are LayoutRight so the species (second)
        // axis is contiguous within a cell row -- required by the generated
        // solver's state.data() linear indexing. error_status is 1-D.
        // These return Kokkos View *handles* by value; a const method may still
        // hand out a mutable view (handle semantics), which is what the kernels
        // need.
        Kokkos::View<double**, Kokkos::LayoutRight, MemSpace> species() const noexcept { return species_buf_; }
        Kokkos::View<double**, Kokkos::LayoutRight, MemSpace> jvals() const noexcept { return jvals_buf_; }
        Kokkos::View<double**, Kokkos::LayoutRight, MemSpace> env() const noexcept { return env_buf_; }
        Kokkos::View<int*, Kokkos::LayoutLeft, MemSpace> error_status() const noexcept { return error_status_; }
        /// Pre-solve snapshot of `species` (same units, mol/m3): captured right
        /// after the gather so the post-solve tendency needs no second read of
        /// `conc` and no per-timestep allocation.
        Kokkos::View<double**, Kokkos::LayoutRight, MemSpace> before() const noexcept { return before_buf_; }
        /// Per-cell/per-slot tendency in mol/mol/s, computed
        /// after the solve and published by the diagnostics path.
        Kokkos::View<double**, Kokkos::LayoutRight, MemSpace> tendency() const noexcept { return tendency_buf_; }
        // clang-format on

        /// Device bytes occupied by every staging buffer, for the init log
        /// ("the staging buffer dimensions and the device memory
        /// they occupy").
        std::size_t bytes() const noexcept {
            auto view_bytes = [](auto const& v) {
                return static_cast<std::size_t>(v.size()) * sizeof(typename std::decay_t<decltype(v)>::value_type);
            };
            return view_bytes(species_buf_) + view_bytes(jvals_buf_) + view_bytes(env_buf_) + view_bytes(error_status_) +
                   view_bytes(before_buf_) + view_bytes(tendency_buf_);
        }

    private:
        int num_cells_ = 0;
        int num_species_ = 0;
        int num_photolysis_ = 0;
        int num_eq_inputs_ = 0;
        Kokkos::View<double**, Kokkos::LayoutRight, MemSpace> species_buf_;
        Kokkos::View<double**, Kokkos::LayoutRight, MemSpace> jvals_buf_;
        Kokkos::View<double**, Kokkos::LayoutRight, MemSpace> env_buf_;
        Kokkos::View<int*, Kokkos::LayoutLeft, MemSpace> error_status_;
        Kokkos::View<double**, Kokkos::LayoutRight, MemSpace> before_buf_;
        Kokkos::View<double**, Kokkos::LayoutRight, MemSpace> tendency_buf_;
    };

    /// Dry-air molar density [mol/m3] from dry-air mass density [kg/m3].
    /// One value per cell, used for every species in that cell.
    KOKKOS_INLINE_FUNCTION double dry_air_molar_density(double airden_dry_kg_m3) {
        constexpr double air_mw_kg = 28.9644 * 1.0e-3; // constants::AIR_MW, g/mol -> kg/mol
        return airden_dry_kg_m3 / air_mw_kg;
    }

    /// Decode a flat cell index to (column, level) using i_cell = ilev*ncol+icol.
    KOKKOS_INLINE_FUNCTION void decode_cell(int i_cell, int ncol, int& icol, int& ilev) {
        icol = i_cell % ncol;
        ilev = i_cell / ncol;
    }

    // A device view of the species mapping: the gather/scatter kernels cannot
    // capture a std::vector, so the process builds this small device array once
    // in init() and reuses it.
    struct DeviceSpeciesMap {
        Kokkos::View<int*, Kokkos::LayoutLeft, StagingBuffers::MemSpace> slot_to_catchem; // [slot] -> catchem_index
    };

    /// Gather + convert: for each cell and each mapped species, read
    /// conc(col, lev, catchem_index) [mol/mol], multiply by that cell's dry-air
    /// molar density, and write species_buf(cell, mechanism_slot) [mol/m3].
    /// The stride change (species slowest -> fastest) is explicit here.
    template <typename ConcView, typename MetView>
    inline void gather_species(const ConcView& conc, const MetView& airden_dry, const DeviceSpeciesMap& map, int num_species,
                               int num_cells, int ncol, Kokkos::View<double**, Kokkos::LayoutRight, StagingBuffers::MemSpace>& out) {
        Kokkos::parallel_for(
            "mkppchem_gather", Kokkos::RangePolicy<StagingBuffers::ExecSpace>(0, num_cells),
            KOKKOS_LAMBDA(int i_cell) {
                int icol, ilev;
                decode_cell(i_cell, ncol, icol, ilev);
                const double rho_molar = dry_air_molar_density(airden_dry(icol, ilev, 0));
                for (int slot = 0; slot < num_species; ++slot) {
                    const int cidx = map.slot_to_catchem(slot);
                    out(i_cell, slot) = conc(icol, ilev, cidx) * rho_molar;
                }
            });
    }

    /// Scatter + convert: for each cell and each mapped species only, read
    /// species_buf(cell, slot) [mol/m3], divide by the cell's dry-air molar
    /// density, and write conc(col, lev, catchem_index) [mol/mol]. Unmapped
    /// CATChem species columns are never assigned.
    template <typename ConcView, typename MetView>
    inline void scatter_species(const Kokkos::View<double**, Kokkos::LayoutRight, StagingBuffers::MemSpace>& species_buf,
                                const MetView& airden_dry, const DeviceSpeciesMap& map, int num_species, int num_cells, int ncol,
                                const ConcView& conc) {
        Kokkos::parallel_for(
            "mkppchem_scatter", Kokkos::RangePolicy<StagingBuffers::ExecSpace>(0, num_cells),
            KOKKOS_LAMBDA(int i_cell) {
                int icol, ilev;
                decode_cell(i_cell, ncol, icol, ilev);
                const double rho_molar = dry_air_molar_density(airden_dry(icol, ilev, 0));
                for (int slot = 0; slot < num_species; ++slot) {
                    const int cidx = map.slot_to_catchem(slot);
                    conc(icol, ilev, cidx) = species_buf(i_cell, slot) / rho_molar;
                }
            });
    }

    /// Fill the per-cell equilibrium-input vector (T, RH, and -- when the
    /// artifact exposes it -- cloud liquid water). `cloud_water` may be null
    /// when the artifact declares only two inputs. Slot indices come from the
    /// artifact's declared EquilibriumInput enum (table-driven).
    template <typename MetView>
    inline void fill_environment(const MetView& t, const MetView& rh, const MetView* cloud_water, int num_cells, int ncol,
                                 int env_idx_temperature, int env_idx_humidity, int env_idx_cloud_water,
                                 Kokkos::View<double**, Kokkos::LayoutRight, StagingBuffers::MemSpace>& env_buf) {
        Kokkos::parallel_for(
            "mkppchem_env", Kokkos::RangePolicy<StagingBuffers::ExecSpace>(0, num_cells),
            KOKKOS_LAMBDA(int i_cell) {
                int icol, ilev;
                decode_cell(i_cell, ncol, icol, ilev);
                env_buf(i_cell, env_idx_temperature) = t(icol, ilev, 0);
                env_buf(i_cell, env_idx_humidity) = rh(icol, ilev, 0);
                if (env_idx_cloud_water >= 0 && cloud_water != nullptr)
                    env_buf(i_cell, env_idx_cloud_water) = (*cloud_water)(icol, ilev, 0);
            });
    }

    /// Refresh one `jvals` column from a bound photolysis-rate diagnostic.
    /// `rates` is the (column, level) device
    /// view of `photolysis_rate_<label>`; the cell flattening matches the
    /// staging buffers (i_cell = ilev * ncol + icol). Called for every bound
    /// label every timestep, so a rate is never reused from a previous step
    /// and never cached across steps.
    template <typename RatesView>
    inline void fill_jvals_from_diagnostic(const RatesView& rates, int num_cells, int ncol, int jvals_slot,
                                           Kokkos::View<double**, Kokkos::LayoutRight, StagingBuffers::MemSpace>& jvals) {
        Kokkos::parallel_for(
            "mkppchem_jvals", Kokkos::RangePolicy<StagingBuffers::ExecSpace>(0, num_cells),
            KOKKOS_LAMBDA(int i_cell) {
                const int icol = i_cell % ncol;
                const int ilev = i_cell / ncol;
                jvals(i_cell, jvals_slot) = rates(icol, ilev);
            });
    }

    /// Fill every `jvals` column from the definition's host-forcing constants
    /// (smoke mode). Isolated-mechanism testing only: the
    /// constants are author-supplied values, not computed J-rates, so OH
    /// production cannot respond to solar zenith angle in this mode. `forcing`
    /// is a device view of NUM_PHOTOLYSIS constants indexed by jvals slot.
    inline void fill_jvals_from_host_forcing(const Kokkos::View<const double*, Kokkos::LayoutLeft,
                                                                 StagingBuffers::MemSpace>& forcing,
                                             int num_cells,
                                             Kokkos::View<double**, Kokkos::LayoutRight, StagingBuffers::MemSpace>& jvals) {
        const int num_photolysis = static_cast<int>(forcing.size());
        Kokkos::parallel_for(
            "mkppchem_jvals_smoke",
            Kokkos::MDRangePolicy<StagingBuffers::ExecSpace, Kokkos::Rank<2>>({0, 0}, {num_cells, num_photolysis}),
            KOKKOS_LAMBDA(int i_cell, int j) { jvals(i_cell, j) = forcing(j); });
    }

    /// Device-side twin of `mkpp::derive_cloud_liquid_water` (the documented
    /// grid-mean condensate derivation). Same formula,
    /// same constants, evaluated per cell inside the environment-fill kernel so
    /// the derived path introduces no host round trip and no temporary
    /// field. Non-physical inputs flag the cell instead of fataling
    /// from a kernel: a flagged cell is skipped by the solve and reported by
    /// the reduction, which is the same fail-loud contract as the host version.
    KOKKOS_INLINE_FUNCTION double derive_cloud_liquid_water_device(double cldf, double qv, double t, double pmid,
                                                                   bool& ok) {
        ok = true;
        const double eps = static_cast<double>(constants::H2O_MW) / static_cast<double>(constants::AIR_MW);
        // Magnus denominator guard: T at or below 29.65 K is unphysical here
        // (validate_met has already required T > 0); flag rather than overflow.
        if (t <= 29.65 || !Kokkos::isfinite(cldf) || cldf < 0.0 || cldf > 1.0 || !Kokkos::isfinite(qv) || qv < 0.0) {
            ok = false;
            return 0.0;
        }
        const double e_sat = 611.2 * Kokkos::exp(17.67 * (t - 273.15) / (t - 29.65));
        const double denom = pmid - (1.0 - eps) * e_sat;
        if (!(denom > 0.0)) {
            ok = false; // pressure at or below saturation vapor pressure: unphysical column
            return 0.0;
        }
        const double q_sat = eps * e_sat / denom;
        const double rho_dry = pmid * static_cast<double>(constants::G0) / (static_cast<double>(constants::RD) * t);
        const double excess = qv > q_sat ? (qv - q_sat) : 0.0; // zero where CLDF == 0 or QV <= q_sat
        const double cldliq = cldf * excess * rho_dry;
        if (!Kokkos::isfinite(cldliq) || cldliq < 0.0) {
            ok = false;
            return 0.0;
        }
        return cldliq;
    }

    /// Environment fill for the Derived cloud-water origin: same T/RH columns
    /// as `fill_environment`, but the cloud-water column is computed in-kernel
    /// from CLDF/QV/T/PMID. A cell whose derivation inputs are non-physical is
    /// flagged (first failure wins) and skipped by the solve, never solved with
    /// a fabricated zero gate value.
    template <typename MetView>
    inline void fill_environment_derived(const MetView& t, const MetView& rh, const MetView& cldf, const MetView& qv,
                                         const MetView& pmid, int num_cells, int ncol, int env_idx_temperature,
                                         int env_idx_humidity, int env_idx_cloud_water,
                                         Kokkos::View<double**, Kokkos::LayoutRight, StagingBuffers::MemSpace>& env_buf,
                                         Kokkos::View<int*, Kokkos::LayoutLeft, StagingBuffers::MemSpace>& errors) {
        Kokkos::parallel_for(
            "mkppchem_env_derived", Kokkos::RangePolicy<StagingBuffers::ExecSpace>(0, num_cells),
            KOKKOS_LAMBDA(int i_cell) {
                int icol, ilev;
                decode_cell(i_cell, ncol, icol, ilev);
                env_buf(i_cell, env_idx_temperature) = t(icol, ilev, 0);
                env_buf(i_cell, env_idx_humidity) = rh(icol, ilev, 0);
                if (env_idx_cloud_water >= 0) {
                    bool ok = true;
                    const double clw = derive_cloud_liquid_water_device(cldf(icol, ilev, 0), qv(icol, ilev, 0),
                                                                        t(icol, ilev, 0), pmid(icol, ilev, 0), ok);
                    env_buf(i_cell, env_idx_cloud_water) = clw;
                    if (!ok) {
                        const int current = errors(i_cell);
                        if (current == kNoError || current == CELL_SUCCESS)
                            errors(i_cell) = CELL_ERR_NON_PHYSICAL_MET;
                    }
                }
            });
    }

    /// Tendency in the published unit (mol/mol/s):
    ///   tendency = (species_after - species_before) / rho_molar_dry / dt
    /// with the same per-cell rho_molar_dry as the gather/scatter, so
    /// tendency is exactly the mixing-ratio change the scatter wrote back,
    /// rate-normalised. Computed for every slot; publication filters to
    /// the mapped species the diagnostic set declares.
    template <typename MetView>
    inline void compute_tendencies(const StagingBuffers& buffers, const MetView& airden_dry, int ncol, double dt) {
        auto species = buffers.species();
        auto before = buffers.before();
        auto tendency = buffers.tendency();
        const int num_cells = buffers.num_cells();
        const int num_species = buffers.num_species();
        Kokkos::parallel_for(
            "mkppchem_tendency", Kokkos::RangePolicy<StagingBuffers::ExecSpace>(0, num_cells),
            KOKKOS_LAMBDA(int i_cell) {
                int icol, ilev;
                decode_cell(i_cell, ncol, icol, ilev);
                const double rho_molar = dry_air_molar_density(airden_dry(icol, ilev, 0));
                for (int s = 0; s < num_species; ++s)
                    tendency(i_cell, s) = (species(i_cell, s) - before(i_cell, s)) / rho_molar / dt;
            });
    }

    /// Publish the selected slots of the tendency buffer into the packed
    /// diagnostic field (Column, Level, Species: LayoutLeft, species fastest
    /// within a (col, lev) pair -- the same convention as the other
    /// species-packed diagnostics). `diag_slots` maps diagnostic column ->
    /// mechanism slot; cells are (ilev*ncol + icol) as everywhere here.
    inline void publish_tendencies(const Kokkos::View<double**, Kokkos::LayoutRight, StagingBuffers::MemSpace>& tendency,
                                   const Kokkos::View<int*, Kokkos::LayoutLeft, StagingBuffers::MemSpace>& diag_slots,
                                   Kokkos::View<double***, Kokkos::LayoutLeft, StagingBuffers::MemSpace> out, int ncol,
                                   int num_cells) {
        const int n_diag = static_cast<int>(diag_slots.size());
        Kokkos::parallel_for(
            "mkppchem_publish_tendency", Kokkos::RangePolicy<StagingBuffers::ExecSpace>(0, num_cells),
            KOKKOS_LAMBDA(int i_cell) {
                const int icol = i_cell % ncol;
                const int ilev = i_cell / ncol;
                for (int d = 0; d < n_diag; ++d)
                    out(icol, ilev, d) = tendency(i_cell, diag_slots(d));
            });
    }

    /// Pre-solve validation: for each cell, verify every staged species value
    /// is finite and non-negative; on failure write CELL_ERR_NON_FINITE_INPUT.
    /// Never solve on garbage input. The error view is assumed to
    /// hold the kNoError sentinel except where an earlier check wrote a code;
    /// the first failure wins (checks never overwrite an existing code).
    void validate_pre_solve(const StagingBuffers& buffers);

    /// Post-solve validation: same check on the solver output; on failure
    /// write CELL_ERR_NUMERICAL_FAILURE.
    void validate_post_solve(StagingBuffers& buffers);

    /// Physical-admissibility validation of the bound met fields before the
    /// solve: T > 0, PMID > 0, 0 <= RH <= 1, AIRDEN_DRY > 0, all finite.
    /// Writes CELL_ERR_NON_PHYSICAL_MET on the offending cell.
    template <typename MetView>
    inline void validate_met(const MetView& t, const MetView& rh, const MetView& pmid, const MetView& airden_dry, int num_cells,
                             int ncol, Kokkos::View<int*, Kokkos::LayoutLeft, StagingBuffers::MemSpace>& error_status) {
        Kokkos::parallel_for(
            "mkppchem_metcheck", Kokkos::RangePolicy<StagingBuffers::ExecSpace>(0, num_cells),
            KOKKOS_LAMBDA(int i_cell) {
                int icol, ilev;
                decode_cell(i_cell, ncol, icol, ilev);
                const double tv = t(icol, ilev, 0);
                const double rv = rh(icol, ilev, 0);
                const double pv = pmid(icol, ilev, 0);
                const double av = airden_dry(icol, ilev, 0);
                const bool ok = std::isfinite(tv) && tv > 0.0 && std::isfinite(rv) && rv >= 0.0 && rv <= 1.0 &&
                                std::isfinite(pv) && pv > 0.0 && std::isfinite(av) && av > 0.0;
                if (!ok)
                    error_status(i_cell) = CELL_ERR_NON_PHYSICAL_MET;
            });
    }

    /// Advance one timestep: for each cell, construct the solver and call
    /// integrate() on the contiguous per-cell species row. This is the
    /// CATChem-owned launch that bypasses MKPP's scalar MechanRegistry::execute.
    /// `species` is the staging species view; each cell's row is a
    /// (cell, species) LayoutRight subview whose stride(1) == 1, so
    /// `state.data()[0..NUM_SPECIES-1]` reads the right species.
    ///
    /// Templated on the generated solver-kernel type so this header -- and the
    /// bridge translation unit that includes it -- never pulls in the 5863-line
    /// generated mechanism header; the process translation unit instantiates
    /// this with the selected mechanism's `SolverKernels<ExecSpace>`.
    ///
    /// A cell whose pre-solve error flag is set is skipped, never solved on
    /// garbage. Post-solve finiteness/non-negativity is validated
    /// separately by validate_post_solve. `env_idx_temperature` /
    /// `env_idx_humidity` / `env_idx_cloud_water` are the artifact's declared
    /// EquilibriumInput slots (table-driven), so the
    /// (temp, rh, clw) arguments are read from the right columns regardless of
    /// enum ordering. `env_idx_cloud_water` is negative when the artifact
    /// exposes only (temp, rh); the overload set is then probed from the most
    /// specific signature (temp, rh, clw) down to the plain (dt, state, jvals)
    /// form, so a mechanism that gains a third equilibrium input needs no change
    /// here beyond passing its declared slot index.
    ///
    /// `initial_step_seed` overrides the generated solver's adaptive-ramp seed
    /// (first substep = dt_total * seed) for every cell in this launch. A
    /// non-positive value -- the default -- leaves the artifact's baked
    /// compile-time seed in force, so callers that do not tune it are
    /// unaffected. Artifacts predating the runtime seed member simply ignore
    /// the override (detected at compile time, never silently at runtime).
    template <typename SolverKernelsType>
    inline void solve_cells(StagingBuffers& buffers, double dt_total, int env_idx_temperature, int env_idx_humidity,
                            int env_idx_cloud_water, double initial_step_seed = 0.0) {
        auto species = buffers.species();
        auto env = buffers.env();
        auto jvals = buffers.jvals();
        auto errors = buffers.error_status();
        const int num_cells = buffers.num_cells();
        Kokkos::parallel_for(
            "mkppchem_solve", Kokkos::RangePolicy<StagingBuffers::ExecSpace>(0, num_cells),
            KOKKOS_LAMBDA(int i_cell) {
                const int e = errors(i_cell);
                if (e != kNoError && e != CELL_SUCCESS)
                    return; // an earlier check flagged this cell; never solve on bad input
                auto state = Kokkos::subview(species, i_cell, Kokkos::ALL());
                SolverKernelsType solver;
                if constexpr (requires { solver.initial_step_seed; }) {
                    if (initial_step_seed > 0.0)
                        solver.initial_step_seed = initial_step_seed;
                }
                const double* jptr = &jvals(i_cell, 0);
                if constexpr (requires {
                                  solver.integrate(dt_total, state, jptr, env(i_cell, env_idx_temperature),
                                                   env(i_cell, env_idx_humidity), env(i_cell, env_idx_cloud_water));
                              }) {
                    solver.integrate(dt_total, state, jptr, env(i_cell, env_idx_temperature), env(i_cell, env_idx_humidity),
                                     env(i_cell, env_idx_cloud_water));
                } else if constexpr (requires {
                                         solver.integrate(dt_total, state, jptr, env(i_cell, env_idx_temperature),
                                                          env(i_cell, env_idx_humidity));
                                     }) {
                    solver.integrate(dt_total, state, jptr, env(i_cell, env_idx_temperature), env(i_cell, env_idx_humidity));
                } else {
                    solver.integrate(dt_total, state, jptr);
                }
            });
    }

    /// Deterministic reduction of the error view to the lowest failing cell
    /// index (Kokkos::Min<int> over a sentinel-initialized view), after a
    /// fence. Returns kNoError when every cell succeeded.
    int reduce_first_error(const StagingBuffers& buffers);

    /// Reset the error view to the sentinel before a timestep's checks run.
    void clear_errors(StagingBuffers& buffers);

} // namespace catchem::mkpp

#endif // CATCHEM_ENABLE_KOKKOS

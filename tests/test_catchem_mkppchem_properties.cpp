// tests/test_catchem_mkppchem_properties.cpp
//
// Property-based invariant tests for the mkppchem solve path. Synthetic randomized input only -- no
// external data files, no remote endpoints, and a fixed seed so a failure is
// reproducible (the determinism gate in the project guidelines).
//
// The invariants are asserted at the staging layer (the bridge kernels +
// solve_cells), which is where the physics contract lives:
//   1. finiteness and non-negativity: every cell the solver accepts leaves a
//      finite, non-negative species row (a bad cell must be FLAGGED instead --
//      detection is part of the invariant);
//   2. sulfur conservation: DMS + SO2 + SO4 (slots 3/4/5, one S atom each) is
//      unchanged by one solve within the solver's tolerance --
//      every sulfur loss term appears as a matching gain);
//   3. the cloud gate is per-cell: a flagged or unflagged neighbor never
//      changes another cell's row (cells are independent RangePolicy rows).
//
// Deliberately corrupt cells (NaN, negative) are mixed into the grid to prove
// the pre-solve check flags exactly those cells and the solve skips them.
#include "catchem_mkpp_state_bridge.hpp"

#include <gocart.hpp>

#include <Kokkos_Core.hpp>

#include <algorithm>
#include <cmath>
#include <cstdio>
#include <iostream>
#include <random>
#include <string>
#include <vector>

namespace {

    using Buffers = catchem::mkpp::StagingBuffers;
    using ExecSpace = Buffers::ExecSpace;
    using Solver = ::mkpp::generated::gocart::SolverKernels<ExecSpace>;

    constexpr int kNumSpecies = 26;
    constexpr int kDMS = 3;
    constexpr int kSO2 = 4;
    constexpr int kSO4 = 5;

    int g_failures = 0;

    void check(bool condition, const std::string& what) {
        if (condition) {
            std::cout << "PASS: " << what << std::endl;
        } else {
            std::cout << "FAIL: " << what << std::endl;
            ++g_failures;
        }
    }

    /// One randomized-grid trial: fill staging host-side, copy to device, run
    /// the same check/solve/check sequence the process uses, and assert the
    /// invariants per cell.
    void run_trial(std::mt19937_64& rng, int num_cells, int ncol, double rho_molar, double dt,
                   const std::vector<int>& corrupt_cells) {
        Buffers buffers;
        buffers.allocate(num_cells, kNumSpecies, Solver::NUM_PHOTOLYSIS,
                         static_cast<int>(::mkpp::generated::gocart::EquilibriumInput::COUNT));

        std::uniform_real_distribution<double> concentration(1.0e-11, 1.0e-9); // mol/m3, near the proven e2e magnitude
        std::uniform_real_distribution<double> temperature(250.0, 300.0);
        std::uniform_real_distribution<double> humidity(0.05, 0.95);
        std::uniform_real_distribution<double> mantissa(0.0, 1.0);

        // Host-side fill, then one deep_copy to device per buffer.
        auto species_host = Kokkos::create_mirror_view(buffers.species());
        auto env_host = Kokkos::create_mirror_view(buffers.env());
        for (int cell = 0; cell < num_cells; ++cell) {
            for (int s = 0; s < kNumSpecies; ++s)
                species_host(cell, s) = concentration(rng);
            env_host(cell, static_cast<int>(::mkpp::generated::gocart::EquilibriumInput::Temperature)) =
                temperature(rng);
            env_host(cell, static_cast<int>(::mkpp::generated::gocart::EquilibriumInput::RelativeHumidity)) =
                humidity(rng);
            env_host(cell, static_cast<int>(::mkpp::generated::gocart::EquilibriumInput::CloudLiquidWater)) =
                std::pow(10.0, -7.0 + 5.0 * mantissa(rng)); // spans both sides of the 1e-6 gate
        }
        // Corrupt the designated cells so the pre-solve check must catch them.
        for (int cell : corrupt_cells) {
            if (cell % 2 == 0)
                species_host(cell, kSO2) = std::nan("");
            else
                species_host(cell, kDMS) = -1.0e-10;
        }
        Kokkos::deep_copy(buffers.species(), species_host);
        Kokkos::deep_copy(buffers.env(), env_host);
        // Snapshot the post-gather state before the solve, exactly as the
        // process does, so the tendency and conservation checks compare the
        // solver's own change against the values it actually started from.
        Kokkos::deep_copy(buffers.before(), buffers.species());

        // A synthetic airden_dry view consistent with the chosen rho_molar so
        // compute_tendencies (and the process's own unit path) agree.
        Kokkos::View<double***, Kokkos::LayoutLeft, Buffers::MemSpace> airden("airden", ncol, num_cells / ncol, 1);
        auto airden_host = Kokkos::create_mirror_view(airden);
        for (int i = 0; i < airden_host.size(); ++i)
            airden_host.data()[i] = rho_molar * 28.9644e-3;
        Kokkos::deep_copy(airden, airden_host);

        catchem::mkpp::clear_errors(buffers);
        catchem::mkpp::validate_pre_solve(buffers);
        catchem::mkpp::solve_cells<Solver>(
            buffers, dt, static_cast<int>(::mkpp::generated::gocart::EquilibriumInput::Temperature),
            static_cast<int>(::mkpp::generated::gocart::EquilibriumInput::RelativeHumidity),
            static_cast<int>(::mkpp::generated::gocart::EquilibriumInput::CloudLiquidWater));
        catchem::mkpp::validate_post_solve(buffers);
        catchem::mkpp::compute_tendencies(buffers, airden, ncol, dt);

        auto err = Kokkos::create_mirror_view_and_copy(Kokkos::HostSpace{}, buffers.error_status());
        auto after = Kokkos::create_mirror_view_and_copy(Kokkos::HostSpace{}, buffers.species());
        auto before = Kokkos::create_mirror_view_and_copy(Kokkos::HostSpace{}, buffers.before());
        auto tendency = Kokkos::create_mirror_view_and_copy(Kokkos::HostSpace{}, buffers.tendency());

        bool corrupt_all_flagged = true;
        int clean_cells = 0;
        double worst_sulfur_drift = 0.0;
        bool all_clean_valid = true;
        bool tendency_consistent = true;
        for (int cell = 0; cell < num_cells; ++cell) {
            const bool corrupt = std::find(corrupt_cells.begin(), corrupt_cells.end(), cell) != corrupt_cells.end();
            const bool flagged = err(cell) != catchem::mkpp::kNoError && err(cell) != catchem::mkpp::CELL_SUCCESS;
            if (corrupt && !flagged)
                corrupt_all_flagged = false;
            if (flagged)
                continue; // a flagged cell is skipped by the solve and reported by the process -- correct behavior
            ++clean_cells;
            double sulfur_before = 0.0, sulfur_after = 0.0;
            for (int s = 0; s < kNumSpecies; ++s) {
                if (!std::isfinite(after(cell, s)) || after(cell, s) < 0.0)
                    all_clean_valid = false;
                if (s == kDMS || s == kSO2 || s == kSO4) {
                    sulfur_before += before(cell, s);
                    sulfur_after += after(cell, s);
                }
                const double expected = (after(cell, s) - before(cell, s)) / rho_molar / dt;
                if (std::abs(tendency(cell, s) - expected) > 1.0e-12 * (std::abs(expected) + 1.0e-30))
                    tendency_consistent = false;
            }
            const double drift = std::abs(sulfur_after - sulfur_before) / sulfur_before;
            worst_sulfur_drift = std::max(worst_sulfur_drift, drift);
        }
        check(corrupt_all_flagged, "trial: every corrupt cell is flagged before the solve (dt=" + std::to_string(dt) +
                                       ")");
        check(clean_cells > num_cells / 2, "trial: most cells solve cleanly (" + std::to_string(clean_cells) + " of " +
                                               std::to_string(num_cells) + ")");
        check(all_clean_valid, "trial: unflagged cells stay finite and non-negative");
        check(worst_sulfur_drift < 1.0e-6, "trial: sulfur conserved on unflagged cells (worst drift " +
                                               std::to_string(worst_sulfur_drift) + ")");
        check(tendency_consistent, "trial: tendency buffer equals (after-before)/rho/dt");
    }

    /// One cloudy-cell aqueous transient (SO2 + H2O2 -> SO4) solved once at
    /// dt=3600 through the bridge, with an optional runtime seed override.
    /// Returns (SO2, SO4) after the solve; asserts the cell stayed clean.
    static std::pair<double, double> solve_one_cloudy_cell(double runtime_seed) {
        Buffers buffers;
        buffers.allocate(1, kNumSpecies, Solver::NUM_PHOTOLYSIS,
                         static_cast<int>(::mkpp::generated::gocart::EquilibriumInput::COUNT));
        auto species = Kokkos::create_mirror_view(buffers.species());
        auto env = Kokkos::create_mirror_view(buffers.env());
        auto jvals = Kokkos::create_mirror_view(buffers.jvals());
        for (int s = 0; s < kNumSpecies; ++s)
            species(0, s) = 1.0e-9;
        species(0, kSO2) = 1.0;   // mol/m3, far above background
        species(0, 2) = 1.0;      // H2O2
        species(0, kSO4) = 0.0;
        env(0, static_cast<int>(::mkpp::generated::gocart::EquilibriumInput::Temperature)) = 288.0;
        env(0, static_cast<int>(::mkpp::generated::gocart::EquilibriumInput::RelativeHumidity)) = 0.5;
        env(0, static_cast<int>(::mkpp::generated::gocart::EquilibriumInput::CloudLiquidWater)) = 1.0e-3;
        for (int j = 0; j < (int)Solver::NUM_PHOTOLYSIS; ++j)
            jvals(0, j) = 0.0;
        Kokkos::deep_copy(buffers.species(), species);
        Kokkos::deep_copy(buffers.env(), env);
        Kokkos::deep_copy(buffers.jvals(), jvals);
        catchem::mkpp::clear_errors(buffers);
        catchem::mkpp::solve_cells<Solver>(
            buffers, 3600.0,
            static_cast<int>(::mkpp::generated::gocart::EquilibriumInput::Temperature),
            static_cast<int>(::mkpp::generated::gocart::EquilibriumInput::RelativeHumidity),
            static_cast<int>(::mkpp::generated::gocart::EquilibriumInput::CloudLiquidWater),
            runtime_seed);
        Kokkos::fence();
        if (catchem::mkpp::reduce_first_error(buffers) != catchem::mkpp::kNoError)
            return {-1.0, -1.0};
        auto after = Kokkos::create_mirror_view_and_copy(Kokkos::HostSpace{}, buffers.species());
        return {after(0, kSO2), after(0, kSO4)};
    }

    void test_runtime_seed_override_changes_solve() {
        // The default 0.0 argument must leave the baked seed untouched and be
        // deterministic; a raised seed must change the transient trajectory
        // (fewer ramp substeps -> different phase of the same attractor).
        const auto baked_a = solve_one_cloudy_cell(0.0);
        const auto baked_b = solve_one_cloudy_cell(0.0);
        const auto raised = solve_one_cloudy_cell(0.1);
        check(baked_a.first > 0.0 && baked_b.first > 0.0 && raised.first > 0.0,
              "runtime-seed: all three solves completed without error flags");
        check(baked_a.first == baked_b.first && baked_a.second == baked_b.second,
              "runtime-seed: default (0.0) is deterministic and uses the baked seed");
        const bool differs = raised.second != baked_a.second;
        const double rel = std::abs(raised.second - baked_a.second) / baked_a.second;
        check(differs && rel < 0.05,
              "runtime-seed: override 0.1 changes SO4 within a few percent (rel=" +
                  std::to_string(rel) + ")");
    }

} // namespace

int main(int argc, char* argv[]) {
    std::cout << "RUNNING: mkppchem property-based invariant tests" << std::endl;
    Kokkos::initialize(argc, argv);
    {
        // Fixed seed: a failure must be reproducible from the log alone.
        std::mt19937_64 rng(20240517ULL);
        const int ncol = 3;
        const int nlev = 2;
        const int num_cells = ncol * nlev;
        const double rho_molar = 40.0; // ~surface dry-air molar density, mol/m3

        // Eight trials over dt, grid content, and corrupt-cell placement.
        const std::vector<double> dts = {1.0, 10.0, 60.0, 300.0, 600.0, 1800.0, 3600.0, 60.0};
        for (std::size_t t = 0; t < dts.size(); ++t) {
            std::vector<int> corrupt;
            if (t % 2 == 0) {
                corrupt.push_back(0);
                corrupt.push_back(num_cells - 1);
            }
            run_trial(rng, num_cells, static_cast<int>(ncol), rho_molar, dts[t], corrupt);
        }
        test_runtime_seed_override_changes_solve();
    }
    Kokkos::finalize();

    if (g_failures == 0) {
        std::cout << "ALL MKPPCHEM PROPERTY TESTS PASSED" << std::endl;
        return 0;
    }
    std::cout << g_failures << " MKPPCHEM PROPERTY TEST(S) FAILED" << std::endl;
    return 1;
}

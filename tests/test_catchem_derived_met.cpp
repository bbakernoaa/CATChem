#include "catchem_core.hpp"
#include "catchem_kokkos_compat.hpp"
#include "catchem_state_manager.hpp"
#include <cmath>
#include <iostream>
#include <memory>
#include <vector>

namespace {

    // CLDFRC is the vertical SUM of the layer cloud fractions (upstream
    // SUM(CLDF, DIM=3)), clamped to [0, 1].  Copying only the surface level
    // (the pre-011 behaviour) severely underestimates it (spec 011 US3).

    enum FieldFlag : unsigned {
        F_CLDF = 1u << 0,
        F_CLDFRC_HOST = 1u << 1,
    };

    // Flat index into a bound (n_cols, n_levels) column-major buffer.
    std::size_t flat(int n_cols, int column, int level) {
        return static_cast<std::size_t>(column) + static_cast<std::size_t>(level) * n_cols;
    }

} // namespace

int main(int argc, char* argv[]) {
    Kokkos::initialize(argc, argv);
    int failures = 0;
    auto check = [&](bool condition, const std::string& label) {
        std::cout << (condition ? "  PASS: " : "  FAIL: ") << label << '\n';
        if (!condition)
            ++failures;
    };

    {
        std::cout << "==========================================" << std::endl;
        std::cout << "RUNNING TEST: Derived MET (CLDFRC) Unit Test" << std::endl;
        std::cout << "==========================================" << std::endl;

        const int n_cols = 4;
        const int n_levels = 5;
        const int n_species = 1;

        // --- CLDFRC == clamp(sum_lev CLDF, 0, 1) per column ------------------
        {
            auto core = std::make_shared<catchem::Core>(n_cols, n_levels, n_species);
            auto state = core->get_state_manager();
            std::vector<double> cldf(n_cols * n_levels, 0.0);
            for (int c = 0; c < n_cols; ++c)
                for (int lev = 0; lev < n_levels; ++lev)
                    cldf[flat(n_cols, c, lev)] = 0.1 + 0.01 * lev; // column sum = 0.6
            state->bind_met_field_3d("CLDF", cldf.data());

            state->derive_surface_cloud_fraction();
            const double* cldfrc = state->read_field<2>("CLDFRC");
            check(cldfrc != nullptr, "CLDFRC derived and readable");
            bool sum_ok = cldfrc != nullptr;
            for (int c = 0; c < n_cols && sum_ok; ++c)
                sum_ok = std::abs(cldfrc[c] - 0.6) < 1.0e-12;
            check(sum_ok, "CLDFRC equals the vertical sum of CLDF (0.6)");
        }

        // --- Column sums above one clamp to unity ----------------------------
        {
            auto core = std::make_shared<catchem::Core>(n_cols, n_levels, n_species);
            auto state = core->get_state_manager();
            std::vector<double> cldf(n_cols * n_levels, 0.5); // column sum = 2.5
            state->bind_met_field_3d("CLDF", cldf.data());

            state->derive_surface_cloud_fraction();
            const double* cldfrc = state->read_field<2>("CLDFRC");
            bool clamp_ok = cldfrc != nullptr;
            for (int c = 0; c < n_cols && clamp_ok; ++c)
                clamp_ok = std::abs(cldfrc[c] - 1.0) < 1.0e-12;
            check(clamp_ok, "CLDFRC clamps a super-unity column sum to 1");
        }

        // --- Host-provided CLDFRC is preserved untouched ---------------------
        {
            auto core = std::make_shared<catchem::Core>(n_cols, n_levels, n_species);
            auto state = core->get_state_manager();
            std::vector<double> cldf(n_cols * n_levels, 0.5);
            std::vector<double> host_cldfrc(n_cols, 0.99);
            state->bind_met_field_3d("CLDF", cldf.data());
            state->bind_met_field_2d("CLDFRC", host_cldfrc.data());

            state->derive_surface_cloud_fraction();
            const double* cldfrc = state->read_field<2>("CLDFRC");
            bool preserved = cldfrc != nullptr;
            for (int c = 0; c < n_cols && preserved; ++c)
                preserved = std::abs(cldfrc[c] - 0.99) < 1.0e-12;
            check(preserved, "host-provided CLDFRC is not overwritten by derivation");
        }

        std::cout << (failures == 0 ? "SUCCESS: all derived-met assertions passed.\n"
                                    : "FAILURE: " + std::to_string(failures) + " derived-met assertion(s) failed.\n");
    }
    Kokkos::finalize();
    return failures == 0 ? 0 : 1;
}

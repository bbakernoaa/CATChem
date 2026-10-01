// test_settling_gocart_kernels.cpp
//
// Unit tests for the native C++ GOCART settling kernels. These are pure
// science checks with no dependency on the aerosol library, the Fortran bridge,
// or the build gate, so they run in every configuration. The expected values
// were generated from an independent transcription of the upstream Fortran
// kernels and cross-checked against hand-computed Stokes limits.
#include "settling_gocart_kernels.hpp"

#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <vector>

namespace {

    int failures = 0;

    // Relative closeness for double-precision science values.
    void expect_close(double actual, double expected, const char* label, double rtol = 1e-12) {
        const double scale = std::max(1.0, std::fabs(expected));
        if (!(std::fabs(actual - expected) <= rtol * scale)) {
            std::fprintf(stderr, "FAIL %s: actual=%.15e expected=%.15e\n", label, actual, expected);
            ++failures;
        }
        else {
            std::printf("ok   %s = %.15e\n", label, actual);
        }
    }

    void expect_true(bool cond, const char* label) {
        if (!cond) {
            std::fprintf(stderr, "FAIL %s\n", label);
            ++failures;
        }
        else {
            std::printf("ok   %s\n", label);
        }
    }

    // ---- Fall speed -----------------------------------------------------

    void test_vsettle_fine_hydrophobic() {
        // A very small particle stays in the Stokes regime (Re < 0.01) so the
        // drag correction is not applied. Golden value from the Fortran port.
        double v = catchem::settling::calc_vsettle(1e-7, 1000.0, 1.2, 296.16, 9.80665);
        expect_close(v, 2.1618430219862855e-06, "vsettle fine (Stokes+slip)");
    }

    void test_vsettle_coarse_drag_corrected() {
        // A coarse dust-like particle exceeds Re 0.01 and takes the
        // Pruppacher-Klett branch. Golden value from the Fortran port.
        double v = catchem::settling::calc_vsettle(5e-5, 2500.0, 1.2, 296.16, 9.80665);
        expect_close(v, 0.5455584235363783, "vsettle coarse (drag-corrected)");
        double v2 = catchem::settling::calc_vsettle(2e-5, 2170.0, 0.4, 220.0, 9.80665);
        expect_close(v2, 0.13012257905955177, "vsettle coarse2 (drag-corrected)");
    }

    void test_vsettle_stokes_limit() {
        // Analytic Stokes cross-check: for a slip-free, drag-free regime the
        // kernel reduces to (2/9) * rho_p * r^2 * g / mu. Verify the viscosity
        // helper matches the Sutherland equation at the reference temperature.
        double mu = catchem::settling::dynamic_viscosity(296.16);
        expect_close(mu, 1.8325e-05, "dynamic_viscosity at 296.16 K");
    }

    // ---- Maring correction ---------------------------------------------

    void test_maring_clamp() {
        std::vector<double> v = {5.0e-3, 1.0e-3, 0.33e-2, 0.0};
        catchem::settling::apply_maring_correction(v.data(), static_cast<int>(v.size()));
        expect_close(v[0], 5.0e-3 - 0.33e-2, "maring subtracts upward velocity");
        expect_close(v[1], 1.0e-9, "maring clamps small v to 1e-9");
        expect_close(v[2], 1.0e-9, "maring clamps zero-crossing to 1e-9");
        expect_close(v[3], 1.0e-9, "maring clamps negative to 1e-9");
    }

    // ---- Swelling -------------------------------------------------------

    void test_fitzgerald() {
        double r, rho;
        catchem::settling::swelling_fitzgerald(1e-6, 2170.0, 0.9, &r, &rho);
        expect_close(r, 2.1603267285292603e-06, "fitzgerald wet radius @ rh 0.9");
        expect_close(rho, 1116.0452971315058, "fitzgerald wet density @ rh 0.9");
        // Below the 0.80 deliquescence threshold the particle is unchanged.
        catchem::settling::swelling_fitzgerald(1e-6, 2170.0, 0.5, &r, &rho);
        expect_close(r, 1e-6, "fitzgerald dry below threshold (radius)");
        expect_close(rho, 2170.0, "fitzgerald dry below threshold (density)");
        // Very humid case exercises the theta switch above 0.97.
        catchem::settling::swelling_fitzgerald(1e-6, 2170.0, 0.99, &r, &rho);
        expect_close(r, 2.791770437118977e-06, "fitzgerald wet radius @ rh 0.99");
        expect_close(rho, 1053.7708311507017, "fitzgerald wet density @ rh 0.99");
    }

    void test_gerber() {
        double r, rho;
        catchem::settling::swelling_gerber(1e-6, 2170.0, 0.9, &r, &rho);
        expect_close(r, 2.0876929017440163e-06, "gerber wet radius @ rh 0.9");
        expect_close(rho, 1128.5837294648409, "gerber wet density @ rh 0.9");
        catchem::settling::swelling_gerber(1e-6, 2170.0, 0.5, &r, &rho);
        expect_close(r, 1.3067468246910081e-06, "gerber wet radius @ rh 0.5");
        expect_close(rho, 1524.3382030300168, "gerber wet density @ rh 0.5");
    }

    void test_swell_dispatch() {
        double r0, rho0, r1, rho1;
        // flag 0 => no swelling.
        catchem::settling::swell(1e-6, 2170.0, 0.9, 0, &r0, &rho0);
        expect_close(r0, 1e-6, "swell flag 0 radius");
        expect_close(rho0, 2170.0, "swell flag 0 density");
        // flag 1 => Fitzgerald.
        catchem::settling::swell(1e-6, 2170.0, 0.9, 1, &r1, &rho1);
        catchem::settling::swelling_fitzgerald(1e-6, 2170.0, 0.9, &r0, &rho0);
        expect_close(r1, r0, "swell flag 1 == fitzgerald");
        // flag 2 => Gerber.
        catchem::settling::swell(1e-6, 2170.0, 0.9, 2, &r1, &rho1);
        catchem::settling::swelling_gerber(1e-6, 2170.0, 0.9, &r0, &rho0);
        expect_close(r1, r0, "swell flag 2 == gerber");
    }

    // ---- Pressure lid ---------------------------------------------------

    void test_find_klid() {
        std::vector<double> press = {300.0, 10.0, 1.0, 0.1, 0.05};
        int k = catchem::settling::find_klid(press.data(), static_cast<int>(press.size()), 0.01);
        expect_true(k == 2, "find_klid nearest 0.01 hPa (Pa=1) -> zero-based 2");
        std::vector<double> press2 = {1e5, 1e4, 1e3, 1e2, 10.0};
        int k2 = catchem::settling::find_klid(press2.data(), static_cast<int>(press2.size()), 0.01);
        expect_true(k2 == 4, "find_klid nearest 0.01 hPa (Pa=10) -> zero-based 4");
    }

    // ---- Layer thickness ------------------------------------------------

    void test_layer_thickness() {
        // Edges top-to-bottom: e[0] (top) .. e[n]. Level k spans e[k]..e[k+1].
        std::vector<double> edges = {70000.0, 60000.0, 40000.0, 0.0}; // n = 3
        std::vector<double> dz(3);
        catchem::settling::layer_thickness(edges.data(), 3, dz.data());
        expect_close(dz[0], 10000.0, "dz top level");
        expect_close(dz[1], 20000.0, "dz middle level");
        expect_close(dz[2], 40000.0, "dz bottom level (to surface edge)");
    }

    // ---- Column mass ----------------------------------------------------

    void test_column_mass() {
        std::vector<double> qa = {1e-6, 2e-6, 3e-6};
        std::vector<double> delp = {50000.0, 30000.0, 20000.0};
        double cm = catchem::settling::column_mass(qa.data(), delp.data(), 0, 3, 9.80665);
        expect_close(cm, 1.733517562062e-02, "column_mass full");
        double cm_partial = catchem::settling::column_mass(qa.data(), delp.data(), 1, 3, 9.80665);
        double expect_partial = (2e-6 * 30000.0 + 3e-6 * 20000.0) / 9.80665;
        expect_close(cm_partial, expect_partial, "column_mass from klid=1");
    }

    // ---- Transport solver ----------------------------------------------

    void test_solver_noop_when_cfl_coarse() {
        // Fine velocities => dt_cfl (0.1/max tau) >= cdt, so the upstream
        // scheme takes nSubSteps = 0 and does NOT settle this step. Faithful
        // ports must reproduce that exact no-op edge.
        std::vector<double> delp = {50000.0, 30000.0, 20000.0};
        std::vector<double> dz = {1000.0, 2000.0, 1500.0};
        std::vector<double> vs = {1e-2, 1e-2, 1e-2};
        std::vector<double> qa = {1e-6, 2e-6, 3e-6};
        std::vector<double> qa_in = qa;
        catchem::settling::settling_solver_ufs(600.0, delp.data(), dz.data(), vs.data(), qa.data(), 3);
        expect_close(qa[0], qa_in[0], "solver noop top");
        expect_close(qa[1], qa_in[1], "solver noop middle");
        expect_close(qa[2], qa_in[2], "solver noop bottom");
    }

    void test_solver_substepped() {
        // Strong velocities => sub-stepping engages. Golden output values from
        // the independent Fortran transcription (n=3, dt=200).
        std::vector<double> delp = {50000.0, 30000.0, 20000.0};
        std::vector<double> dz = {1000.0, 2000.0, 1500.0};
        std::vector<double> vs = {0.5, 0.3, 0.2};
        std::vector<double> qa = {1e-6, 2e-6, 3e-6};
        catchem::settling::settling_solver_ufs(600.0, delp.data(), dz.data(), vs.data(), qa.data(), 3);
        expect_close(qa[0], 7.29e-07, "solver substep top (loss only)");
        expect_close(qa[1], 2.2626626666666664e-06, "solver substep middle");
        expect_close(qa[2], 3.042585111111112e-06, "solver substep bottom");
        for (double x : qa)
            expect_true(x >= 0.0, "solver non-negative");
    }

    void test_solver_mass_balance() {
        // The solver only removes mass at the top level (loss to space above
        // the lid); interior transfers conserve the weighted column mass. The
        // flux equals (cmass_before - cmass_after)/cdt and is >= 0.
        std::vector<double> delp = {50000.0, 30000.0, 20000.0};
        std::vector<double> dz = {1000.0, 2000.0, 1500.0};
        std::vector<double> vs = {0.05, 0.05, 0.05};
        std::vector<double> qa = {1e-6, 2e-6, 3e-6};
        double cm_before = catchem::settling::column_mass(qa.data(), delp.data(), 0, 3, 9.80665);
        catchem::settling::settling_solver_ufs(3600.0, delp.data(), dz.data(), vs.data(), qa.data(), 3);
        double cm_after = catchem::settling::column_mass(qa.data(), delp.data(), 0, 3, 9.80665);
        expect_close(qa[0], 8.281e-07, "solver mass-balance top");
        expect_close(qa[1], 2.1037999999999997e-06, "solver mass-balance middle");
        expect_close(qa[2], 2.91675e-06, "solver mass-balance bottom");
        expect_close(cm_after, 1.660648641483e-02, "solver cmass_after");
        double flux = (cm_before - cm_after) / 3600.0;
        expect_close(flux, 2.024136682761e-07, "solver flux");
        expect_true(flux >= 0.0, "solver flux non-negative");
        expect_true(cm_after <= cm_before, "solver loses mass only at top");
    }

} // namespace

int main() {
    test_vsettle_fine_hydrophobic();
    test_vsettle_coarse_drag_corrected();
    test_vsettle_stokes_limit();
    test_maring_clamp();
    test_fitzgerald();
    test_gerber();
    test_swell_dispatch();
    test_find_klid();
    test_layer_thickness();
    test_column_mass();
    test_solver_noop_when_cfl_coarse();
    test_solver_substepped();
    test_solver_mass_balance();

    if (failures != 0) {
        std::fprintf(stderr, "\n%d settling kernel test(s) FAILED\n", failures);
        return EXIT_FAILURE;
    }
    std::printf("\nAll settling kernel tests passed\n");
    return EXIT_SUCCESS;
}

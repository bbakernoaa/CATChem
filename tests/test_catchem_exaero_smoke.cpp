// test_catchem_exaero_smoke.cpp
//
// Gate-ON smoke test for the exaero aerosol utility bridge.  It exercises the
// full C++/library path a CATChem process will use -- create + initialize a
// package, resolve a species, run the derived-diagnostics step on a synthetic
// column, fence, and read the gravitational settling velocity out of the slab
// -- and checks the result against the analytic Stokes value so a wiring
// regression (wrong slot order, a silent transpose, a broken handle) fails
// loudly instead of producing plausible-but-wrong physics.
//
// The reference case is deliberately chosen to have a closed-form answer: a
// single non-hygroscopic species with no curve table bound.  With kappa = 0
// and no GEOSmie table, the solver's wet sizing collapses to the dry diameter
// (growth factor 1) and the wet density to the dry density, so the
// mass-weighted Stokes settling velocity reduces exactly to
//
//   v_g = rho_dry * d^2 * g / (18 * mu)
//
// with the constants exaero's solver uses (g = 9.80665 m/s^2, dynamic
// viscosity mu = 1.825e-5 kg/(m s)).  The mass concentration cancels in the
// mass-weighted average over a single species, so the answer is independent
// of the injected mass -- a useful invariant the test also checks.
#include "catchem_exaero_bridge.hpp"

#include <cmath>
#include <cstdlib>
#include <iostream>
#include <string>
#include <vector>

namespace {

    // Analytic constants mirrored from exaero's solver (documented above).
    constexpr double kGravity = 9.80665;
    constexpr double kDynViscosity = 1.825e-5;

    // One-species, single-cell, single-level GOCART configuration.  dry_particle_
    // diameter and dry_density fix the analytic v_g; hygroscopicity 0 keeps the
    // wet size equal to the dry size at any RH.
    const char* kConfigYaml = R"YAML(
species:
  - name: "Smoke"
    dry_density: 1000.0
    molecular_weight: 100.0
    dry_particle_diameter: 2.0e-6
    hygroscopicity: 0.0
    lognormal_sigma: 1.5
    lognormal_dg: 2.0e-6
    refractive_index_real: 1.55
    refractive_index_imag: 0.002
)YAML";

    bool check(bool condition, const std::string& label) {
        if (!condition) {
            std::cerr << "FAIL: " << label << std::endl;
        }
        return condition;
    }

} // namespace

int main() {
    using catchem::exaero_util::diagnostic_slot;
    using catchem::exaero_util::ExaeroPackageHandle;

    bool ok = true;
    ExaeroPackageHandle::init_environment();

    ExaeroPackageHandle handle;
    std::string error;
    ok &= check(handle.initialize_from_yaml(kConfigYaml, error), "initialize_from_yaml: " + error);
    ok &= check(handle.valid(), "handle is valid after init");

    // Species resolution: the configured label maps to the packed index 0; an
    // absent label returns -1 (never a silent wrong index).
    ok &= check(handle.species_index("Smoke") == 0, "species_index(Smoke) resolves to 0");
    ok &= check(handle.species_index("NoSuchSpecies") == -1, "species_index(unknown) returns -1");

    // Synthetic column: 1 cell, 1 level, 1 species.  Environmental fields are
    // (cell, level); state is (cell, level, species).  All column-major, so a
    // single cell/level makes the flat layout unambiguous.
    const int n_cells = 1;
    const int n_levels = 1;
    const int n_species = 1;
    std::vector<double> temperature = {298.0};
    std::vector<double> pressure = {101325.0};
    std::vector<double> air_density = {1.2};
    std::vector<double> relative_humidity = {0.5};
    std::vector<double> layer_thickness = {100.0};
    std::vector<double> state = {1.0e-6}; // [kg m^-3]

    ok &= check(handle.compute_diagnostics(n_cells, n_levels, n_species, temperature.data(), pressure.data(),
                                           air_density.data(), relative_humidity.data(), layer_thickness.data(),
                                           state.data(), error),
                "compute_diagnostics: " + error);

    const int slot = diagnostic_slot("gravitational_settling_velocity");
    ok &= check(slot == 6, "diagnostic_slot(gravitational_settling_velocity)==6");
    ok &= check(diagnostic_slot("not_a_real_slot") == -1, "diagnostic_slot(unknown) returns -1");

    const double vg = handle.diagnostic(slot, 0, 0);
    const double rho_dry = 1000.0;
    const double d_dry = 2.0e-6;
    const double vg_expected = rho_dry * d_dry * d_dry * kGravity / (18.0 * kDynViscosity);

    // Physical post-conditions: finite, non-negative (a fall velocity), and
    // downward-positive as the solver defines it.
    ok &= check(std::isfinite(vg), "settling velocity is finite");
    ok &= check(vg >= 0.0, "settling velocity is non-negative");
    ok &= check(std::abs(vg - vg_expected) <= 1.0e-6 * std::abs(vg_expected),
                "settling velocity matches analytic Stokes value");

    // Mass-independence invariant: doubling the injected mass must not change
    // the mass-weighted single-species velocity.
    std::vector<double> state_double = {2.0e-6};
    ok &= check(handle.compute_diagnostics(n_cells, n_levels, n_species, temperature.data(), pressure.data(),
                                           air_density.data(), relative_humidity.data(), layer_thickness.data(),
                                           state_double.data(), error),
                "recompute at doubled mass: " + error);
    const double vg_double = handle.diagnostic(slot, 0, 0);
    ok &= check(std::abs(vg_double - vg) <= 1.0e-9 * std::abs(vg),
                "settling velocity is mass-independent for one species");

    // Out-of-range reads return NaN rather than a stale or zero value.
    ok &= check(std::isnan(handle.diagnostic(slot, 5, 5)), "out-of-range diagnostic read is NaN");

    handle.release();
    ok &= check(!handle.valid(), "handle invalid after release");

    // ---------------------------------------------------------------------
    // Per-species caller-anchored wet sizing (the surface the C++ settling
    // port uses).  The library resolves its curve's growth factor at each
    // point's RH and applies it to the CALLER's dry particle.  A species
    // bound to a hydrophobic curve (DU, growth 1) must return the dry
    // particle bit-for-bit -- the parity guarantee against the Fortran
    // baseline.  A species bound to a hygroscopic curve (SeaSalt -> SS) must
    // swell (wet radius > dry, wet density diluted toward water while
    // conserving the dry-plus-condensate mass).  A species with no curve
    // (Smoke) must report an unavailable status and leave the outputs at
    // zero so the caller falls back to its own swelling parameterization
    // rather than reading a silent 0 as physical.
    // ---------------------------------------------------------------------
    const char* kTwoSpeciesYaml = R"YAML(
species:
  - name: "DU"
    dry_density: 2600.0
    molecular_weight: 100.0
    dry_particle_diameter: 2.0e-6
    hygroscopicity: 0.1
    lognormal_sigma: 1.5
    lognormal_dg: 1.0e-6
    refractive_index_real: 1.55
    refractive_index_imag: 0.002
  - name: "SeaSalt"
    dry_density: 2200.0
    molecular_weight: 58.44
    dry_particle_diameter: 0.2e-6
    hygroscopicity: 0.5
    lognormal_sigma: 2.0
    lognormal_dg: 0.15e-6
    refractive_index_real: 1.43
    refractive_index_imag: 1.0e-8
    mie_table:
      source: "SS"
  - name: "Smoke"
    dry_density: 1000.0
    molecular_weight: 100.0
    dry_particle_diameter: 2.0e-6
    hygroscopicity: 0.0
    lognormal_sigma: 1.5
    lognormal_dg: 2.0e-6
    refractive_index_real: 1.55
    refractive_index_imag: 0.002
)YAML";

    ExaeroPackageHandle sizing;
    ok &= check(sizing.initialize_from_yaml(kTwoSpeciesYaml, error), "sizing package init: " + error);
    const int du_index = sizing.species_index("DU");
    const int ss_index = sizing.species_index("SeaSalt");
    const int smoke_index = sizing.species_index("Smoke");
    ok &= check(du_index >= 0, "DU resolves to a species index");
    ok &= check(ss_index >= 0, "SeaSalt resolves to a species index");
    ok &= check(smoke_index >= 0, "Smoke resolves to a species index");

    const double radius_dry = 1.0e-7;  // [m]
    const double density_dry = 2600.0; // [kg m^-3]
    std::vector<double> radius_out, density_out;
    std::vector<int> status_out;

    // Hydrophobic DU: growth 1 at every stored RH, so the anchored sizing is
    // the caller's dry particle, exactly.
    ok &= check(sizing.compute_wet_sizing(n_cells, n_levels, relative_humidity.data(), radius_dry, density_dry,
                                          du_index, radius_out, density_out, status_out, error),
                "compute_wet_sizing(DU): " + error);
    ok &= check(radius_out.size() == static_cast<std::size_t>(n_cells * n_levels), "DU radius output sized");
    ok &= check(ExaeroPackageHandle::attribute_value_ok(status_out[0]), "DU wet sizing is available");
    ok &= check(radius_out[0] == radius_dry, "hydrophobic DU keeps the dry radius bit-for-bit");
    ok &= check(density_out[0] == density_dry, "hydrophobic DU keeps the dry density bit-for-bit");

    // Hygroscopic SeaSalt: the curve swells the caller's particle; wet radius
    // grows, wet density dilutes toward water, and dry-plus-condensate mass is
    // conserved (the physical invariant behind the volume-weighted mixing).
    ok &= check(sizing.compute_wet_sizing(n_cells, n_levels, relative_humidity.data(), radius_dry, density_dry,
                                          ss_index, radius_out, density_out, status_out, error),
                "compute_wet_sizing(SeaSalt): " + error);
    ok &= check(ExaeroPackageHandle::attribute_value_ok(status_out[0]), "SeaSalt wet sizing is available");
    ok &= check(std::isfinite(radius_out[0]) && radius_out[0] > radius_dry, "SeaSalt swells the wet radius");
    ok &= check(density_out[0] < density_dry && density_out[0] > 1000.0, "SeaSalt density dilutes toward water");
    {
        const double dry_vol = (4.0 / 3.0) * M_PI * radius_dry * radius_dry * radius_dry;
        const double wet_vol = (4.0 / 3.0) * M_PI * radius_out[0] * radius_out[0] * radius_out[0];
        const double mass_wet = density_out[0] * wet_vol;
        const double mass_parts = density_dry * dry_vol + 1000.0 * (wet_vol - dry_vol);
        ok &= check(std::abs(mass_wet - mass_parts) <= 1.0e-9 * std::abs(mass_parts),
                    "SeaSalt wet sizing conserves dry-plus-condensate mass");
    }

    // A curve-less species reports an unavailable status and leaves the value
    // at the zero the bridge initialized it to (never a fabricated number).
    ok &= check(sizing.compute_wet_sizing(n_cells, n_levels, relative_humidity.data(), radius_dry, density_dry,
                                          smoke_index, radius_out, density_out, status_out, error),
                "compute_wet_sizing(Smoke): " + error);
    ok &= check(!ExaeroPackageHandle::attribute_value_ok(status_out[0]), "Smoke wet sizing is unavailable");
    ok &= check(status_out[0] == ExaeroPackageHandle::kAttributeNotInSource, "Smoke status is NotInSource");
    ok &= check(radius_out[0] == 0.0 && density_out[0] == 0.0, "unavailable sizing leaves zeros");

    sizing.release();

    ExaeroPackageHandle::finalize_environment();

    if (!ok) {
        std::cerr << "exaero smoke test FAILED" << std::endl;
        return EXIT_FAILURE;
    }
    std::cout << "exaero smoke test PASSED (v_g = " << vg << " m/s, expected " << vg_expected << ")" << std::endl;
    return EXIT_SUCCESS;
}

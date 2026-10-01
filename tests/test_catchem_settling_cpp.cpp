// Gate-ON native-engine settling parity test (scheme: gocart_cpp).
//
// This is a standalone executable that owns the full aerosol-library runtime
// lifecycle: unlike the Fortran-bridge settling test (which drives the science
// through a Fortran callback and never touches Kokkos), the native engine
// initializes and finalizes a reference-counted Kokkos runtime inside the
// settling process.  Folding that into a suite whose main() already owns
// Kokkos::initialize/finalize would let a process finalize() tear the runtime
// down mid-suite, so the engine path gets its own process.  A single sticky
// environment reference is held for the whole run, so the per-process
// init/finalize pairs only ever bump the count 1 <-> 2 and a premature
// Kokkos::finalize is impossible.
//
// The parity contract, mirroring the Fortran scheme:
//   * The Fortran `gocart` metadata path applies ParticleSwelling (Fitzgerald)
//     only where RH > 0.80; below that ceiling every species stays dry.  The
//     fixture holds every point at or below that ceiling, so the Fortran
//     baseline sizes EVERY species with its dry radius and density.
//   * The native engine sizes a species from exaero's caller-anchored wet
//     sizing where the species' curve is bound, and from the same Fitzgerald/
//     Gerber parameterization otherwise.  A hydrophobic curve (growth factor
//     exactly 1) returns the caller's dry particle, so the engine feeds the
//     identical (radius, density) to the identical ported solver and reproduces
//     the Fortran tendency to floating-point roundoff.  A hygroscopic curve
//     (growth factor > 1) swells the particle where the Fortran baseline does
//     not, so it settles faster and leaves less mass -- the positive control
//     that the curve is actually driving the result.
//   * Species are classified by querying the library directly, independent of
//     either settling run, so the assertions are not circular.
//
// Invariants checked on the engine output: non-negativity, no mass creation,
// and determinism (a repeated run is byte-identical).  Configuration errors
// (a species missing from the library config, the optics-table scheme on the
// native engine) must abort init() with a named remedy, never a silent fallback.

#include "catchem_config_manager.hpp"
#include "catchem_core.hpp"
#include "catchem_process_registry.hpp"
#include "catchem_state_manager.hpp"
#include "catchem_test_config.hpp"
#include "catchem_exaero_bridge.hpp"

#include <cmath>
#include <cstring>
#include <functional>
#include <fstream>
#include <iostream>
#include <memory>
#include <stdexcept>
#include <string>
#include <vector>

extern "C" {
void catchem_register_settling_cpp();
}

namespace {

    // A small vertical profile that keeps every settling input finite and
    // physically ordered (temperature lapse, decreasing pressure, ascending
    // geometric height).  Relative humidity is capped at or below the Fortran
    // Fitzgerald activation ceiling (0.80) so the baseline stays dry for every
    // species and the hydrophobic-parity argument holds.
    struct Fixture {
        int n_cols = 4;
        int n_levels = 5;
        int n_species = 22;
        std::vector<double> T, AIRDEN, DELP, RH, PMID, Zedge, conc;

        Fixture() {
            T.assign(n_cols * n_levels, 0.0);
            AIRDEN.assign(n_cols * n_levels, 0.0);
            DELP.assign(n_cols * n_levels, 0.0);
            RH.assign(n_cols * n_levels, 0.0);
            PMID.assign(n_cols * n_levels, 0.0);
            Zedge.assign(n_cols * (n_levels + 1), 0.0);
            conc.assign(static_cast<std::size_t>(n_cols) * n_levels * n_species, 1.0e-8);
            for (int c = 0; c < n_cols; ++c)
                for (int lev = 0; lev < n_levels; ++lev) {
                    std::size_t i = c + static_cast<std::size_t>(lev) * n_cols;
                    T[i] = 288.15 - 6.5 * lev;
                    PMID[i] = 101300.25 * std::exp(-lev / 8.0);
                    AIRDEN[i] = 1.2 * std::exp(-lev / 8.0);
                    DELP[i] = 5000.0;
                    // Surface 0.78, decreasing upward: every point <= 0.80.
                    RH[i] = 0.78 * std::exp(-lev / 6.0);
                }
            for (int c = 0; c < n_cols; ++c)
                for (int lev = 0; lev <= n_levels; ++lev)
                    Zedge[c + static_cast<std::size_t>(lev) * n_cols] = 100.0 * lev;
        }
    };

    // Flat index into the bound (n_cols, n_levels, n_species) concentration
    // buffer: column fastest, then level, then species.
    std::size_t flat(const Fixture& fix, int column, int level, int species) {
        return static_cast<std::size_t>(species) * fix.n_levels * fix.n_cols +
               static_cast<std::size_t>(level) * fix.n_cols + column;
    }

    std::shared_ptr<catchem::ConfigManager> settling_config(const std::string& scheme, const std::string& exaero_config,
                                                            bool simple_scheme = false) {
        auto config = std::make_shared<catchem::ConfigManager>();
        config->load_from_file("CATChem_new_config.yml");
        YAML::Node gocart;
        gocart["scale_factor"] = 1.0;
        gocart["simple_scheme"] = simple_scheme;
        gocart["swelling_method"] = 1;
        gocart["correction_maring"] = false;
        gocart["maring_dust_only"] = true;
        YAML::Node settings;
        settings["gocart"] = gocart;
        if (!exaero_config.empty())
            settings["exaero_config"] = exaero_config;
        auto& proc = config->data.processes["settling"];
        proc.activate = true;
        proc.scheme = scheme;
        proc.set_settings_node(settings);
        return config;
    }

    // The Default 22-species inventory path, resolved against the configured
    // absolute test directory with a working-directory fallback.
    std::string default_species_path() {
        const std::string relative = "Configs/Default/CATChem_species.yml";
        const std::string absolute = std::string(catchem::test::TEST_DIR) + "/" + relative;
        if (std::ifstream(absolute).good())
            return absolute;
        for (const std::string& candidate :
             {relative, "tests/" + relative, "../tests/" + relative, "../../tests/" + relative})
            if (std::ifstream(candidate).good())
                return candidate;
        return absolute;
    }

    // Build a bound state from a fresh fixture and attach the settling config.
    std::shared_ptr<catchem::StateManager> bind_state(const std::shared_ptr<catchem::Core>& core, Fixture& fix,
                                                      const std::shared_ptr<catchem::ConfigManager>& config) {
        auto state = core->get_state_manager();
        state->attach_config_manager(config);
        state->load_species_config(default_species_path());
        if (static_cast<int>(state->chemistry().species_list.size()) != fix.n_species)
            throw std::runtime_error("settling fixture species count mismatch");

        state->bind_met_field_3d("T", fix.T.data());
        state->bind_met_field_3d("AIRDEN", fix.AIRDEN.data());
        state->bind_met_field_3d("DELP", fix.DELP.data());
        state->bind_met_field_3d("RH", fix.RH.data());
        state->bind_met_field_3d("Z", fix.Zedge.data());
        state->bind_met_field_3d("PMID", fix.PMID.data());
        fix.conc.assign(static_cast<std::size_t>(fix.n_cols) * fix.n_levels * fix.n_species, 1.0e-8);
        state->bind_unified_chemistry(fix.conc.data());
        state->clock().timestep = 3600.0;
        return state;
    }

    // One settling species' packed identity plus its library classification.
    struct SpeciesInfo {
        int packed_index;
        std::string short_name;
        std::string mie_name;
        double radius_dry_m;
        double density_dry;
        bool hydrophobic; // curve returns the dry particle at every fixture point
        // Peak wet/dry size ratio over the fixture's usable points, and the
        // matching wet/dry density ratio at that point.  Both are 1 for a
        // hydrophobic curve; the radius ratio is the hygroscopic growth factor
        // and quantifies how much larger (and therefore faster-settling) the
        // engine makes the particle relative to the dry Fortran baseline.
        double swell_ratio_max = 1.0;
        double density_ratio_at_swell = 1.0;
    };

    // Run one settling scheme to completion and return the resulting
    // concentration buffer.  The process is destroyed before returning so the
    // native engine releases its library handle (dropping the environment
    // reference back to the sticky baseline) before the next run.
    std::vector<double> run_scheme(const std::string& scheme, const std::string& exaero_config, Fixture& out_fix,
                                   bool simple_scheme = false) {
        auto core = std::make_shared<catchem::Core>(out_fix.n_cols, out_fix.n_levels, out_fix.n_species);
        auto config = settling_config(scheme, exaero_config, simple_scheme);
        auto state = bind_state(core, out_fix, config);
        auto settling = catchem::ProcessRegistry::get_instance().create("settling");
        settling->init(state);
        settling->run(state);
        state->sync_to_host();
        std::vector<double> result = out_fix.conc;
        settling->finalize();
        settling.reset();
        return result;
    }

    bool threw_with(const std::function<void()>& fn, const std::string& expected) {
        try {
            fn();
        } catch (const std::exception& error) {
            return std::string(error.what()).find(expected) != std::string::npos;
        }
        return false;
    }

} // namespace

int main() {
    using catchem::exaero_util::ExaeroPackageHandle;

    int failures = 0;
    auto check = [&](bool condition, const std::string& label) {
        std::cout << (condition ? "  PASS: " : "  FAIL: ") << label << '\n';
        if (!condition)
            ++failures;
    };

    catchem_register_settling_cpp();

    const std::string exaero_config =
        std::string(catchem::test::TEST_DIR) + "/Configs/exaero/settling_species.yml";
    const std::string exaero_config_du_only =
        std::string(catchem::test::TEST_DIR) + "/Configs/exaero/settling_species_du_only.yml";

    // A sticky environment reference kept for the whole run so the per-process
    // init/finalize pairs never drive the count to zero early.
    ExaeroPackageHandle::init_environment();

    // ---------------------------------------------------------------------
    // Enumerate the settling species and classify each by querying the library
    // directly: a species whose bound curve returns its dry radius at every
    // fixture point is hydrophobic (growth factor 1) and must reproduce the
    // Fortran baseline; one that swells is hygroscopic and must settle faster.
    // A species with no bound curve falls back to the same parameterization the
    // Fortran baseline uses at this RH, so it also matches.  Classification is
    // independent of either settling run.
    // ---------------------------------------------------------------------
    std::vector<SpeciesInfo> species;
    {
        Fixture probe;
        auto core = std::make_shared<catchem::Core>(probe.n_cols, probe.n_levels, probe.n_species);
        auto config = settling_config("gocart", "");
        auto state = bind_state(core, probe, config);

        ExaeroPackageHandle handle;
        handle.init_environment();
        std::string error;
        const bool init_ok = handle.initialize_from_file(exaero_config, error);
        check(init_ok, "classification: library package initializes from config: " + error);
        if (init_ok) {
            for (int ispec : state->chemistry().settling_indices) {
                const auto& meta = state->chemistry().species_list[static_cast<std::size_t>(ispec)];
                SpeciesInfo info;
                info.packed_index = ispec;
                info.short_name = meta.short_name;
                info.mie_name = meta.mie_name;
                info.radius_dry_m = meta.radius * 1.0e-6;
                info.density_dry = meta.density;

                const int pkg_index = handle.species_index(meta.mie_name);
                std::vector<double> radius_wet, density_wet;
                std::vector<int> status;
                bool curve_ok = false;
                if (pkg_index >= 0 &&
                    handle.compute_wet_sizing(probe.n_cols, probe.n_levels, probe.RH.data(), info.radius_dry_m,
                                              info.density_dry, pkg_index, radius_wet, density_wet, status, error)) {
                    bool any_ok = false;
                    bool all_dry = true;
                    for (std::size_t p = 0; p < radius_wet.size(); ++p) {
                        if (!ExaeroPackageHandle::attribute_value_ok(status[p]))
                            continue;
                        any_ok = true;
                        if (radius_wet[p] != info.radius_dry_m || density_wet[p] != info.density_dry)
                            all_dry = false;
                        const double ratio = radius_wet[p] / info.radius_dry_m;
                        if (ratio > info.swell_ratio_max) {
                            info.swell_ratio_max = ratio;
                            info.density_ratio_at_swell = density_wet[p] / info.density_dry;
                        }
                    }
                    // A curve-bound species is hydrophobic only if it has a
                    // usable point AND every usable point is the dry particle.
                    curve_ok = any_ok;
                    info.hydrophobic = any_ok && all_dry;
                }
                else {
                    // No curve / query failure: the engine falls back to the
                    // configured swelling parameterization, which at this RH is
                    // the same dry result the Fortran baseline produces.
                    info.hydrophobic = true;
                }
                species.push_back(info);
                std::cout << "  classify: " << info.short_name << " [mie " << info.mie_name << "] "
                          << (info.hydrophobic ? "hydrophobic (matches baseline)"
                                               : (curve_ok ? "hygroscopic (settles faster)" : "no curve (matches)"))
                          << "  peak growth factor " << info.swell_ratio_max << "  (wet/dry density "
                          << info.density_ratio_at_swell << ")\n";
            }
        }
        handle.release();
        handle.finalize_environment();
    }

    check(!species.empty(), "at least one settling species is configured");

    // ---------------------------------------------------------------------
    // Parity: run the Fortran baseline and the native engine on identical
    // fixtures.  Every hydrophobic species must match the baseline to roundoff
    // (this validates the whole ported solver chain: swelling gate, fall speed,
    // lid, transport, unit conversions).  Hygroscopic species settle faster in
    // the engine than in the dry baseline, so they leave no more mass; at least
    // one must actually differ, proving the curve drives the result.
    // ---------------------------------------------------------------------
    {
        Fixture fix_f, fix_e;
        const std::vector<double> conc_fortran = run_scheme("gocart", "", fix_f);
        const std::vector<double> conc_engine = run_scheme("gocart_cpp", exaero_config, fix_e);
        check(conc_fortran.size() == conc_engine.size() && !conc_engine.empty(),
              "engine and baseline produce same-shaped buffers");

        int n_hydro = 0, n_hygro = 0, n_hygro_differ = 0;
        for (const auto& info : species) {
            double max_rel = 0.0;
            bool finite = true;
            bool non_negative = true;
            for (int c = 0; c < fix_f.n_cols; ++c)
                for (int lev = 0; lev < fix_f.n_levels; ++lev) {
                    const std::size_t idx = flat(fix_f, c, lev, info.packed_index);
                    const double f = conc_fortran[idx];
                    const double e = conc_engine[idx];
                    if (!std::isfinite(e))
                        finite = false;
                    if (e < 0.0)
                        non_negative = false;
                    const double denom = std::max(std::abs(f), 1.0e-12);
                    max_rel = std::max(max_rel, std::abs(e - f) / denom);
                }
            if (info.hydrophobic) {
                ++n_hydro;
                check(finite && non_negative && max_rel <= 1.0e-10,
                      "hydrophobic '" + info.short_name + "' matches baseline to roundoff (max rel " +
                          std::to_string(max_rel) + ")");
            }
            else {
                ++n_hygro;
                check(finite && non_negative, "hygroscopic '" + info.short_name + "' output finite & non-negative");
                if (max_rel > 1.0e-6)
                    ++n_hygro_differ;
                // Report the physical magnitude of the divergence.  In the
                // small-particle (Stokes) regime the fall speed scales with
                // density times the square of the diameter, so the swollen
                // particle settles growth-factor^2 x (wet/dry density) times
                // faster than the dry Fortran baseline.  This is the settled
                // fraction per unit time the engine gains over the baseline;
                // the pointwise concentration difference below then reflects
                // how much of that speed-up had time to move mass in one step.
                const double vsettle_ratio = info.swell_ratio_max * info.swell_ratio_max * info.density_ratio_at_swell;
                std::cout << "  settling diff: " << info.short_name << " growth factor " << info.swell_ratio_max
                          << ", wet/dry density " << info.density_ratio_at_swell << " -> fall speed " << vsettle_ratio
                          << "x the dry baseline; max pointwise rel " << max_rel
                          << (max_rel > 1.0e-6 ? " (differs)" : " (unchanged: fine bin, little motion in one step)")
                          << '\n';
            }
        }

        check(n_hydro > 0, "fixture includes at least one hydrophobic species");
        check(n_hygro > 0, "fixture includes at least one hygroscopic species");
        check(n_hygro_differ > 0, "at least one hygroscopic species differs from the dry baseline (curve live)");

        // Settling only removes mass and a swollen particle settles no slower,
        // so the engine leaves no more mass than the dry baseline overall.
        double engine_total = 0.0, fortran_total = 0.0;
        for (std::size_t i = 0; i < conc_engine.size(); ++i) {
            engine_total += conc_engine[i];
            fortran_total += conc_fortran[i];
        }
        check(engine_total <= fortran_total + 1.0e-6 * std::abs(fortran_total),
              "engine does not create mass relative to baseline");
    }

    // ---------------------------------------------------------------------
    // Determinism: a repeated native-engine run is byte-identical.
    // ---------------------------------------------------------------------
    {
        Fixture a, b;
        const std::vector<double> first = run_scheme("gocart_cpp", exaero_config, a);
        const std::vector<double> second = run_scheme("gocart_cpp", exaero_config, b);
        bool identical = first.size() == second.size();
        for (std::size_t i = 0; identical && i < first.size(); ++i)
            if (std::memcmp(&first[i], &second[i], sizeof(double)) != 0)
                identical = false;
        check(identical, "native engine is deterministic (repeat run byte-identical)");
    }

    // ---------------------------------------------------------------------
    // Fail-fast configuration errors (never a silent fallback).
    // ---------------------------------------------------------------------
    {
        // A species present in the transport inventory but missing from the
        // library config aborts init() naming the config.
        Fixture fix;
        auto core = std::make_shared<catchem::Core>(fix.n_cols, fix.n_levels, fix.n_species);
        auto config = settling_config("gocart_cpp", exaero_config_du_only);
        auto state = bind_state(core, fix, config);
        auto settling = catchem::ProcessRegistry::get_instance().create("settling");
        check(threw_with([&]() { settling->init(state); }, "not defined in the exaero configuration"),
              "missing species in library config aborts init() with a named remedy");
    }
    {
        // The optics-table branch is not implemented on the native engine.
        Fixture fix;
        auto core = std::make_shared<catchem::Core>(fix.n_cols, fix.n_levels, fix.n_species);
        auto config = settling_config("gocart_cpp", exaero_config, /*simple_scheme=*/true);
        auto state = bind_state(core, fix, config);
        auto settling = catchem::ProcessRegistry::get_instance().create("settling");
        check(threw_with([&]() { settling->init(state); }, "does not implement the optics-table path"),
              "simple_scheme on gocart_cpp aborts init() directing to the Fortran scheme");
    }

    ExaeroPackageHandle::finalize_environment();

    std::cout << (failures == 0 ? "SUCCESS: native-engine settling parity assertions passed.\n"
                                : "FAILURE: " + std::to_string(failures) + " parity assertion(s) failed.\n");
    return failures == 0 ? 0 : 1;
}

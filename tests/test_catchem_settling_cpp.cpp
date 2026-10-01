// Gate-ON native-engine settling parity + swelling-model validation.
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
// The Fortran `gocart` metadata path resizes a particle only where its
// configured swelling method is active: Fitzgerald (method 1) resizes above its
// 0.80 deliquescence threshold and leaves EVERY species dry at or below it;
// Gerber (method 2) resizes at all relative humidities.  The native engine
// gates the library's caller-anchored wet sizing on the same activation rule,
// so it can reproduce the baseline exactly where the baseline does not swell.
// This executable checks two things:
//
//   (A) Sub-gate replication.  With every fixture point at or below the 0.80
//       threshold, the Fortran baseline is dry for all species and the engine
//       stays dry too, so the engine must reproduce the Fortran tendency for
//       EVERY settling species to floating-point roundoff -- not merely the
//       hydrophobic ones.  This is the replication contract.
//
//   (B) Active-regime model validation.  Above the threshold both the Fortran
//       baseline and the engine resize particles, but from different sources:
//       the Fortran uses the species-blind Fitzgerald parameterization (it
//       swells dust and black carbon too), while the engine uses the library's
//       per-species growth-factor curves (which correctly keep hydrophobic
//       species dry).  They are different models and CANNOT be bit-identical
//       there.  This block MEASURES and reports the divergence -- the growth
//       factor, the wet/dry density, and the resulting Stokes fall-speed ratio
//       of each sizing -- rather than asserting equality, and it runs both
//       schemes end-to-end to confirm the engine is live (differs from the
//       Fortran result) and physically sane (finite, non-negative, no mass
//       creation) above the gate.
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
#include "settling_gocart_kernels.hpp"

#include <cmath>
#include <cstring>
#include <functional>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <memory>
#include <stdexcept>
#include <string>
#include <vector>

extern "C" {
void catchem_register_settling_cpp();
}

namespace {

    namespace sci = catchem::settling;
    using catchem::exaero_util::ExaeroPackageHandle;

    // A small vertical profile that keeps every settling input finite and
    // physically ordered (temperature lapse, decreasing pressure, ascending
    // geometric height).  `active` selects the humidity regime: false holds
    // every point at or below the Fortran Fitzgerald activation ceiling (0.80)
    // so the baseline stays dry for every species; true lifts every point above
    // it so both the baseline and the engine resize particles.
    struct Fixture {
        int n_cols = 4;
        int n_levels = 5;
        int n_species = 22;
        std::vector<double> T, AIRDEN, DELP, RH, PMID, Zedge, conc;

        explicit Fixture(bool active = false) {
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
                    // Sub-gate: surface 0.78 decreasing upward (all <= 0.80).
                    // Active: 0.92 down to 0.84 (all > 0.80).
                    RH[i] = active ? (0.92 - 0.02 * lev) : (0.78 * std::exp(-lev / 6.0));
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

    // One settling species' packed identity plus its dry sizing and library
    // curve presence.
    struct SpeciesInfo {
        int packed_index;
        std::string short_name;
        std::string mie_name;
        double radius_dry_m;
        double density_dry;
        bool curve_bound; // the library has a usable curve for this species
    };

    // Run one settling scheme to completion and return the resulting
    // concentration buffer.  The process is destroyed before returning so the
    // native engine releases its library handle (dropping the environment
    // reference back to the sticky baseline) before the next run.
    std::vector<double> run_scheme(const std::string& scheme, const std::string& exaero_config, bool active) {
        Fixture fix(active);
        auto core = std::make_shared<catchem::Core>(fix.n_cols, fix.n_levels, fix.n_species);
        auto config = settling_config(scheme, exaero_config);
        auto state = bind_state(core, fix, config);
        auto settling = catchem::ProcessRegistry::get_instance().create("settling");
        settling->init(state);
        settling->run(state);
        state->sync_to_host();
        std::vector<double> result = fix.conc;
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

    // The maximum pointwise relative difference between two concentration
    // buffers over the (cell, level) block of one species.
    double max_rel_diff(const Fixture& fix, const std::vector<double>& a, const std::vector<double>& b,
                        int species) {
        double worst = 0.0;
        for (int c = 0; c < fix.n_cols; ++c)
            for (int lev = 0; lev < fix.n_levels; ++lev) {
                const std::size_t idx = flat(fix, c, lev, species);
                const double denom = std::max(std::abs(a[idx]), 1.0e-12);
                worst = std::max(worst, std::abs(b[idx] - a[idx]) / denom);
            }
        return worst;
    }

} // namespace

int main() {
    int failures = 0;
    auto check = [&](bool condition, const std::string& label) {
        std::cout << (condition ? "  PASS: " : "  FAIL: ") << label << '\n';
        if (!condition)
            ++failures;
    };

    catchem_register_settling_cpp();

    const std::string exaero_config = std::string(catchem::test::TEST_DIR) + "/Configs/exaero/settling_species.yml";
    const std::string exaero_config_du_only =
        std::string(catchem::test::TEST_DIR) + "/Configs/exaero/settling_species_du_only.yml";

    // A sticky environment reference kept for the whole run so the per-process
    // init/finalize pairs never drive the count to zero early.
    ExaeroPackageHandle::init_environment();

    // ---------------------------------------------------------------------
    // Enumerate the settling species and record each one's dry sizing and
    // whether the library carries a usable curve for it.  This is a direct
    // library query, independent of either settling run.
    // ---------------------------------------------------------------------
    std::vector<SpeciesInfo> species;
    {
        Fixture probe(false);
        auto core = std::make_shared<catchem::Core>(probe.n_cols, probe.n_levels, probe.n_species);
        auto config = settling_config("gocart", "");
        auto state = bind_state(core, probe, config);

        ExaeroPackageHandle handle;
        handle.init_environment();
        std::string error;
        const bool init_ok = handle.initialize_from_file(exaero_config, error);
        check(init_ok, "library package initializes from config: " + error);
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
                info.curve_bound = false;
                if (pkg_index >= 0 &&
                    handle.compute_wet_sizing(probe.n_cols, probe.n_levels, probe.RH.data(), info.radius_dry_m,
                                              info.density_dry, pkg_index, radius_wet, density_wet, status, error)) {
                    for (int st : status)
                        if (ExaeroPackageHandle::attribute_value_ok(st)) {
                            info.curve_bound = true;
                            break;
                        }
                }
                species.push_back(info);
                std::cout << "  species: " << info.short_name << " [mie " << info.mie_name << "] "
                          << (info.curve_bound ? "curve-bound" : "no curve (Fitzgerald fallback)") << '\n';
            }
        }
        handle.release();
        handle.finalize_environment();
    }

    check(!species.empty(), "at least one settling species is configured");

    // ---------------------------------------------------------------------
    // (A) Sub-gate replication.  Every point at or below the 0.80 threshold:
    // the Fortran baseline is dry for all species and the engine stays dry too,
    // so the engine must reproduce the Fortran tendency for EVERY species to
    // roundoff.  This is the replication contract.
    // ---------------------------------------------------------------------
    std::cout << "\n  === (A) sub-gate replication (all RH <= 0.80) ===\n";
    {
        // Fixture used only for its grid dimensions when comparing buffers.
        Fixture probe(false);
        const std::vector<double> conc_fortran = run_scheme("gocart", "", false);
        const std::vector<double> conc_engine = run_scheme("gocart_cpp", exaero_config, false);
        check(conc_fortran.size() == conc_engine.size() && !conc_engine.empty(),
              "engine and baseline produce same-shaped buffers");

        double worst_overall = 0.0;
        bool all_match = true;
        for (const auto& info : species) {
            const double rel = max_rel_diff(probe, conc_fortran, conc_engine, info.packed_index);
            worst_overall = std::max(worst_overall, rel);
            if (rel > 1.0e-10)
                all_match = false;
        }
        check(all_match, "EVERY settling species matches the Fortran baseline to roundoff below the gate (worst rel " +
                             std::to_string(worst_overall) + ")");

        // Determinism: a repeated native-engine run is byte-identical.
        const std::vector<double> again = run_scheme("gocart_cpp", exaero_config, false);
        bool identical = conc_engine.size() == again.size();
        for (std::size_t i = 0; identical && i < again.size(); ++i)
            if (std::memcmp(&conc_engine[i], &again[i], sizeof(double)) != 0)
                identical = false;
        check(identical, "native engine is deterministic below the gate (repeat run byte-identical)");
    }

    // ---------------------------------------------------------------------
    // (B) Active-regime model validation.  Above the 0.80 threshold both the
    // Fortran baseline and the engine resize particles, from different sources.
    // Compare the two sizings directly at a representative point, then confirm
    // the engine is live and physically sane end-to-end.
    // ---------------------------------------------------------------------
    std::cout << "\n  === (B) active-regime validation (all RH > 0.80) ===\n";
    {
        const double rh = 0.90;       // representative super-gate relative humidity
        const double rhoa = 1.2;      // surface air density
        const double temp = 288.15;   // surface temperature

        ExaeroPackageHandle handle;
        handle.init_environment();
        std::string error;
        if (handle.initialize_from_file(exaero_config, error)) {
            std::cout << std::fixed << std::setprecision(4);
            std::cout << "  species     mie   exaero gf  exaero v [m/s]   Fitzgerald gf  Fitzgerald v [m/s]  v ratio\n";
            for (const auto& info : species) {
                const int pkg_index = handle.species_index(info.mie_name);
                std::vector<double> radius_wet(1, 0.0), density_wet(1, 0.0);
                std::vector<int> status(1, 0);
                double gf_exaero = 1.0, v_exaero = 0.0;
                if (pkg_index >= 0 &&
                    handle.compute_wet_sizing(1, 1, &rh, info.radius_dry_m, info.density_dry, pkg_index, radius_wet,
                                              density_wet, status, error) &&
                    ExaeroPackageHandle::attribute_value_ok(status[0])) {
                    gf_exaero = radius_wet[0] / info.radius_dry_m;
                    v_exaero = sci::calc_vsettle(radius_wet[0], density_wet[0], rhoa, temp, sci::kGravity);
                }
                else {
                    // No usable curve: the engine would fall back to the
                    // same Fitzgerald sizing the Fortran baseline uses, so the
                    // two agree by construction.  Report gf = 1 divergence = 0.
                    v_exaero = sci::calc_vsettle(info.radius_dry_m, info.density_dry, rhoa, temp, sci::kGravity);
                }

                double radius_fitz = 0.0, rhop_fitz = 0.0;
                sci::swelling_fitzgerald(info.radius_dry_m, info.density_dry, rh, &radius_fitz, &rhop_fitz);
                const double gf_fitz = radius_fitz / info.radius_dry_m;
                const double v_fitz = sci::calc_vsettle(radius_fitz, rhop_fitz, rhoa, temp, sci::kGravity);
                const double ratio = v_fitz > 0.0 ? v_exaero / v_fitz : 1.0;
                std::cout << "  " << std::setw(9) << info.short_name << " " << std::setw(4) << info.mie_name << "   "
                          << std::setw(8) << gf_exaero << "   " << std::setw(11) << v_exaero << "        "
                          << std::setw(8) << gf_fitz << "        " << std::setw(11) << v_fitz << "   " << std::setw(7)
                          << ratio << (info.curve_bound ? "" : " (fallback)") << '\n';
            }
        }
        else {
            check(false, "active-regime: library package initializes from config: " + error);
        }
        handle.release();
        handle.finalize_environment();

        // End-to-end: run both schemes above the gate and confirm the engine is
        // live (differs from the dry-only path is not the test here -- both
        // swell, but from different sizings) and physically sane.
        Fixture probe(true);
        const std::vector<double> conc_fortran = run_scheme("gocart", "", true);
        const std::vector<double> conc_engine = run_scheme("gocart_cpp", exaero_config, true);

        bool finite = true, non_negative = true;
        for (double v : conc_engine) {
            if (!std::isfinite(v))
                finite = false;
            if (v < 0.0)
                non_negative = false;
        }
        check(finite && non_negative, "engine output above the gate is finite and non-negative");

        // The engine and the Fortran baseline now disagree for at least the
        // curve-bound hygroscopic species (different wet sizes).  This is the
        // accepted model difference above the gate, not a replication failure.
        double worst_curve = 0.0;
        for (const auto& info : species)
            if (info.curve_bound)
                worst_curve = std::max(worst_curve, max_rel_diff(probe, conc_fortran, conc_engine, info.packed_index));
        check(worst_curve > 0.0, "engine uses the library curve above the gate (differs from Fitzgerald sizing, worst rel " +
                                    std::to_string(worst_curve) + ")");

        // Settling only removes mass, so each scheme's column total must not
        // exceed the 1.0e-8 initial concentration it started from.  (The two
        // schemes can retain different amounts -- different wet sizes settle at
        // different rates -- but neither may create mass.)
        const double initial_total = conc_engine.size() * 1.0e-8;
        double engine_total = 0.0, fortran_total = 0.0;
        for (double v : conc_engine)
            engine_total += v;
        for (double v : conc_fortran)
            fortran_total += v;
        check(engine_total <= initial_total * (1.0 + 1.0e-6),
              "engine does not create mass above the gate");
        check(fortran_total <= initial_total * (1.0 + 1.0e-6), "baseline does not create mass above the gate");
    }

    // ---------------------------------------------------------------------
    // Fail-fast configuration errors (never a silent fallback).
    // ---------------------------------------------------------------------
    {
        // A species present in the transport inventory but missing from the
        // library config aborts init() naming the config.
        Fixture fix(false);
        auto core = std::make_shared<catchem::Core>(fix.n_cols, fix.n_levels, fix.n_species);
        auto config = settling_config("gocart_cpp", exaero_config_du_only);
        auto state = bind_state(core, fix, config);
        auto settling = catchem::ProcessRegistry::get_instance().create("settling");
        check(threw_with([&]() { settling->init(state); }, "not defined in the exaero configuration"),
              "missing species in library config aborts init() with a named remedy");
    }
    {
        // The optics-table branch is not implemented on the native engine.
        Fixture fix(false);
        auto core = std::make_shared<catchem::Core>(fix.n_cols, fix.n_levels, fix.n_species);
        auto config = settling_config("gocart_cpp", exaero_config, /*simple_scheme=*/true);
        auto state = bind_state(core, fix, config);
        auto settling = catchem::ProcessRegistry::get_instance().create("settling");
        check(threw_with([&]() { settling->init(state); }, "does not implement the optics-table path"),
              "simple_scheme on gocart_cpp aborts init() directing to the Fortran scheme");
    }

    ExaeroPackageHandle::finalize_environment();

    std::cout << (failures == 0 ? "\nSUCCESS: sub-gate replication exact; active-regime divergence measured.\n"
                                : "\nFAILURE: " + std::to_string(failures) + " assertion(s) failed.\n");
    return failures == 0 ? 0 : 1;
}

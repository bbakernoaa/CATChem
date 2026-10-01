// Gate-ON native-engine settling: fidelity to the GOCART radiation tables.
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
// The reference for the native engine's wet sizing is the GOCART
// radiation-table settling path (Chem_SettlingSimple, the legacy
// simple_scheme: true scheme): it queries the Mie optics tables for the wet
// effective radius and density at whatever relative humidity it is given, at
// EVERY point -- there is no deliquescence gate and no swelling-method flag at
// all.  The library's caller-anchored wet sizing reads those same pinned
// GEOSmie tables (its growth factor is rEff(bin,rh)/rEff(bin,0) from exactly
// those files), so applying it ungated at every RH mimics the table path's
// sizing rule: hydrophobic species (dust, carbon) have a constant rEff across
// the whole RH axis and stay dry by the table's own physics, while sulfate and
// sea salt swell continuously from near-zero RH.  This executable checks:
//
//   (A) Curve fidelity to the radiation tables.  The library's growth factor
//       is sampled across the RH range per species and compared against
//       values interpolated directly from the pinned GEOSmie rEff tables at
//       RH 0.78 (SU 1.62077, SS 1.56489, DU/BC/OC exactly 1), with monotonicity
//       in RH and dry-at-all-RH for dust and black carbon.  This is the
//       external control that the engine's sizing source really is the optics
//       table, not a re-derived parameterization.
//
//   (B) End-to-end below RH 0.80 against the Fortran metadata scheme (gocart,
//       swelling_method 1 = Fitzgerald).  That scheme is a no-op below 0.80 for
//       every species, so species the tables keep dry there (dust, carbon)
//       match the Fortran tendency to floating-point roundoff, while
//       hygroscopic species swell per the table and differ.  That difference
//       IS the table path's behavior (the optics tables grow SS/SU at all RH)
//       -- the positive control that the curve is live, not a replication
//       failure.  Determinism is checked on the engine output.
//
//   (C) End-to-end above RH 0.80.  The metadata path swells every species
//       there (Fitzgerald is species-blind) while the tables swell only the
//       hygroscopic ones, so the engine (tables) and the metadata scheme
//       (Fitzgerald) diverge for every species.  This block runs both schemes,
//       reports the per-species growth factors and fall-speed ratio, and
//       asserts only physical sanity (finite, non-negative, no mass creation).
//
// Configuration errors (a species missing from the library config, the
// optics-table scheme on the native engine) must abort init() with a named
// remedy, never a silent fallback.

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
#include <map>
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
    // geometric height).  `swelling` selects the humidity regime: false holds
    // every point at or below 0.80 (where the Fortran metadata path is inert
    // for every species); true lifts every point above 0.80 (where it swells
    // every species).
    struct Fixture {
        int n_cols = 4;
        int n_levels = 5;
        int n_species = 22;
        std::vector<double> T, AIRDEN, DELP, RH, PMID, Zedge, conc;

        explicit Fixture(bool swelling = false) {
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
                    // Sub-0.80: surface 0.78 decreasing upward (all <= 0.80).
                    // Super-0.80: 0.92 down to 0.84 (all > 0.80).
                    RH[i] = swelling ? (0.92 - 0.02 * lev) : (0.78 * std::exp(-lev / 6.0));
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

    // One settling species' packed identity, dry sizing, and the shape of its
    // library growth-factor curve as sampled across the RH range.
    struct SpeciesInfo {
        int packed_index;
        std::string short_name;
        std::string mie_name;
        double radius_dry_m;
        double density_dry;
        bool curve_bound = false;   // the library has a usable curve for this species
        bool dry_below_080 = true;  // growth factor stays 1 at every sampled RH <= 0.80
        bool dry_at_all_rh = true;  // growth factor stays 1 at every sampled RH
        double gf_at_078 = 1.0;     // growth factor at relative humidity 0.78
        double gf_at_099 = 1.0;     // growth factor at relative humidity 0.99
    };

    // Run one settling scheme to completion and return the resulting
    // concentration buffer.  The process is destroyed before returning so the
    // native engine releases its library handle (dropping the environment
    // reference back to the sticky baseline) before the next run.
    std::vector<double> run_scheme(const std::string& scheme, const std::string& exaero_config, bool swelling) {
        Fixture fix(swelling);
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

    // Growth factors interpolated from the pinned GEOSmie radiation tables
    // (MieTableData.hpp rEff / rEff at RH 0, linear in RH between the 0.75 and
    // 0.80 nodes) at relative humidity 0.78, per aerosol type.  These are the
    // magnitudes the optics tables themselves encode -- the sizes the GOCART
    // table settling path gives a particle -- so the library curve must
    // reproduce them for the engine to mimic that path.  Dust, black carbon,
    // and organic carbon (at the curve's radius node) have a constant rEff
    // across the RH axis.
    const std::map<std::string, double> kTableGfAt078 = {
        {"SU", 1.62077}, {"SS", 1.56489}, {"DU", 1.0}, {"BC", 1.0}, {"OC", 1.0},
    };

    // Sampled RH grid for the curve-shape checks (ascending).
    const std::vector<double> kRhSamples = {0.05, 0.30, 0.50, 0.70, 0.78, 0.90, 0.99};

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
    // Enumerate the settling species and sample each one's library growth-
    // factor curve across the RH range.  This is a direct library query,
    // independent of either settling run.
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
                if (pkg_index >= 0 &&
                    handle.compute_wet_sizing(1, static_cast<int>(kRhSamples.size()), kRhSamples.data(),
                                              info.radius_dry_m, info.density_dry, pkg_index, radius_wet, density_wet,
                                              status, error)) {
                    bool monotone = true;
                    double prev_gf = 0.0;
                    for (std::size_t i = 0; i < kRhSamples.size(); ++i) {
                        if (!ExaeroPackageHandle::attribute_value_ok(status[i]))
                            continue;
                        info.curve_bound = true;
                        const double gf = radius_wet[i] / info.radius_dry_m;
                        if (gf + 1.0e-12 < prev_gf)
                            monotone = false;
                        prev_gf = gf;
                        if (kRhSamples[i] <= 0.80 && gf > 1.0 + 1.0e-9)
                            info.dry_below_080 = false;
                        if (gf > 1.0 + 1.0e-9)
                            info.dry_at_all_rh = false;
                        if (kRhSamples[i] == 0.78)
                            info.gf_at_078 = gf;
                        if (kRhSamples[i] == 0.99)
                            info.gf_at_099 = gf;
                    }
                    if (info.curve_bound && !monotone)
                        check(false, "curve monotone in RH for " + info.short_name);
                }
                species.push_back(info);
                std::cout << "  species: " << info.short_name << " [mie " << info.mie_name << "] "
                          << (info.curve_bound ? "curve-bound" : "no curve (parameterization fallback)")
                          << (info.dry_at_all_rh ? " dry-at-all-RH" : "")
                          << (info.curve_bound && !info.dry_below_080 ? " swells-below-0.80" : "") << '\n';
            }
        }
        handle.release();
        handle.finalize_environment();
    }

    check(!species.empty(), "at least one settling species is configured");

    // ---------------------------------------------------------------------
    // (A) Curve fidelity to the pinned GEOSmie radiation tables.  The growth
    // factor at RH 0.78 must match the table-interpolated value per aerosol
    // type, dust and black carbon must stay exactly dry across the whole RH
    // range (their rEff is constant in the tables), and the hygroscopic types
    // must swell continuously from low RH (no deliquescence gate).
    // ---------------------------------------------------------------------
    std::cout << "\n  === (A) curve fidelity to the radiation tables ===\n";
    {
        bool anchors_ok = true;
        bool dry_species_ok = true;
        bool swells_ok = true;
        for (const auto& info : species) {
            if (!info.curve_bound)
                continue;
            auto expected = kTableGfAt078.find(info.mie_name);
            if (expected == kTableGfAt078.end())
                continue;
            if (std::abs(info.gf_at_078 - expected->second) > 2.0e-3) {
                anchors_ok = false;
                std::cout << "    anchor mismatch: " << info.short_name << " gf(0.78)=" << info.gf_at_078
                          << " expected " << expected->second << '\n';
            }
            if ((info.mie_name == "DU" || info.mie_name == "BC") && !info.dry_at_all_rh)
                dry_species_ok = false;
            // Sulfate and sea salt swell at the lowest sampled RH (their rEff
            // rises from RH 0 in the tables) and exceed gf 1.5 near saturation.
            if (info.mie_name == "SU" && (info.dry_below_080 || info.gf_at_099 < 1.5))
                swells_ok = false;
            if (info.mie_name == "SS" && (info.dry_below_080 || info.gf_at_099 < 1.5))
                swells_ok = false;
        }
        check(anchors_ok, "library growth factors match the GEOSmie table anchors at RH 0.78 (SU 1.62077, SS 1.56489)");
        check(dry_species_ok, "dust and black carbon curves stay exactly dry (gf = 1) across the full RH range");
        check(swells_ok, "sulfate and sea salt swell below 0.80 (no gate) and exceed gf 1.5 near saturation");
    }

    // ---------------------------------------------------------------------
    // (B) End-to-end below RH 0.80 against the Fortran metadata scheme
    // (gocart, Fitzgerald).  That scheme is inert below 0.80 for every
    // species, so species the tables keep dry there (dust, carbon) must
    // reproduce the Fortran tendency to roundoff, while hygroscopic species
    // swell per the table -- the table path's own behavior, and the positive
    // control that the curve is live.
    // ---------------------------------------------------------------------
    std::cout << "\n  === (B) sub-0.80 run against the Fortran metadata scheme ===\n";
    {
        // Fixture used only for its grid dimensions when comparing buffers.
        Fixture probe(false);
        const std::vector<double> conc_fortran = run_scheme("gocart", "", false);
        const std::vector<double> conc_engine = run_scheme("gocart_cpp", exaero_config, false);
        check(conc_fortran.size() == conc_engine.size() && !conc_engine.empty(),
              "engine and Fortran metadata scheme produce same-shaped buffers");

        bool dry_match = true;
        bool live_control = false;
        for (const auto& info : species) {
            const double rel = max_rel_diff(probe, conc_fortran, conc_engine, info.packed_index);
            if (info.dry_below_080) {
                if (rel > 1.0e-10) {
                    dry_match = false;
                    std::cout << "    dry-species mismatch: " << info.short_name << " rel " << rel << '\n';
                }
            }
            else if (rel > 1.0e-6) {
                live_control = true;
            }
        }
        check(dry_match, "species the tables keep dry below 0.80 match the Fortran metadata scheme to roundoff");
        check(live_control, "hygroscopic species swell below 0.80 per the tables (curve live; the metadata path "
                            "stays dry there by design)");

        // Determinism: a repeated native-engine run is byte-identical.
        const std::vector<double> again = run_scheme("gocart_cpp", exaero_config, false);
        bool identical = conc_engine.size() == again.size();
        for (std::size_t i = 0; identical && i < again.size(); ++i)
            if (std::memcmp(&conc_engine[i], &again[i], sizeof(double)) != 0)
                identical = false;
        check(identical, "native engine is deterministic (repeat run byte-identical)");
    }

    // ---------------------------------------------------------------------
    // (C) End-to-end above RH 0.80.  The metadata path swells every species
    // there (Fitzgerald is species-blind) while the tables swell only the
    // hygroscopic ones, so the engine (tables) and the metadata scheme
    // (Fitzgerald) diverge for every species.  Report the divergence and
    // assert only physical sanity.
    // ---------------------------------------------------------------------
    std::cout << "\n  === (C) super-0.80 run (table sizing vs species-blind Fitzgerald) ===\n";
    {
        const double rh = 0.90;     // representative super-0.80 relative humidity
        const double rhoa = 1.2;    // surface air density
        const double temp = 288.15; // surface temperature

        std::cout << std::fixed << std::setprecision(4);
        std::cout << "  species     mie   table gf  table v [m/s]   Fitzgerald gf  Fitzgerald v [m/s]  v ratio\n";
        ExaeroPackageHandle handle;
        handle.init_environment();
        std::string error;
        if (handle.initialize_from_file(exaero_config, error)) {
            for (const auto& info : species) {
                const int pkg_index = handle.species_index(info.mie_name);
                std::vector<double> radius_wet(1, 0.0), density_wet(1, 0.0);
                std::vector<int> status(1, 0);
                double gf_table = 1.0, v_table = 0.0;
                if (pkg_index >= 0 &&
                    handle.compute_wet_sizing(1, 1, &rh, info.radius_dry_m, info.density_dry, pkg_index, radius_wet,
                                              density_wet, status, error) &&
                    ExaeroPackageHandle::attribute_value_ok(status[0])) {
                    gf_table = radius_wet[0] / info.radius_dry_m;
                    v_table = sci::calc_vsettle(radius_wet[0], density_wet[0], rhoa, temp, sci::kGravity);
                }
                else {
                    v_table = sci::calc_vsettle(info.radius_dry_m, info.density_dry, rhoa, temp, sci::kGravity);
                }

                double radius_fitz = 0.0, rhop_fitz = 0.0;
                sci::swelling_fitzgerald(info.radius_dry_m, info.density_dry, rh, &radius_fitz, &rhop_fitz);
                const double gf_fitz = radius_fitz / info.radius_dry_m;
                const double v_fitz = sci::calc_vsettle(radius_fitz, rhop_fitz, rhoa, temp, sci::kGravity);
                const double ratio = v_fitz > 0.0 ? v_table / v_fitz : 1.0;
                std::cout << "  " << std::setw(9) << info.short_name << " " << std::setw(4) << info.mie_name << "   "
                          << std::setw(8) << gf_table << "   " << std::setw(11) << v_table << "        "
                          << std::setw(8) << gf_fitz << "        " << std::setw(11) << v_fitz << "   " << std::setw(7)
                          << ratio << (info.curve_bound ? "" : " (fallback)") << '\n';
            }
        }
        else {
            check(false, "super-0.80: library package initializes from config: " + error);
        }
        handle.release();
        handle.finalize_environment();

        const std::vector<double> conc_fortran = run_scheme("gocart", "", true);
        const std::vector<double> conc_engine = run_scheme("gocart_cpp", exaero_config, true);

        bool finite = true, non_negative = true;
        for (double v : conc_engine) {
            if (!std::isfinite(v))
                finite = false;
            if (v < 0.0)
                non_negative = false;
        }
        check(finite && non_negative, "engine output above 0.80 is finite and non-negative");

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
        check(engine_total <= initial_total * (1.0 + 1.0e-6), "engine does not create mass above 0.80");
        check(fortran_total <= initial_total * (1.0 + 1.0e-6), "metadata scheme does not create mass above 0.80");
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

    std::cout << (failures == 0 ? "\nSUCCESS: engine sizing reproduces the GOCART radiation-table curve.\n"
                                : "\nFAILURE: " + std::to_string(failures) + " assertion(s) failed.\n");
    return failures == 0 ? 0 : 1;
}

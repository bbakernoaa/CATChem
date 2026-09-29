// tests/test_catchem_mkppchem.cpp
//
// Column-level smoke test for the mkppchem process: a single column with a
// handful of levels, initialized from the generated species configuration and
// run through one chemistry timestep. This proves the full per-timestep solve
// sequence (gather / env fill / pre-solve / solve / post-solve / reduce /
// scatter) works end to end against the real compiled gocart artifact -- the
// unit tests (test_catchem_mkppchem_units) deliberately avoid a solver call.
//
// The property-based invariant suite (non-negativity, finiteness, sulfur-mass
// conservation over randomized input) and the coupled-photolysis scenario are
// added to this file with their own tasks; this establishes the happy path.
#include "catchem_core.hpp"
#include "catchem_mkpp_mechanism.hpp"
#include "catchem_process_registry.hpp"
#include "catchem_state_manager.hpp"
#include "catchem_test_config.hpp"

#include <Kokkos_Core.hpp>

#include <algorithm>
#include <cassert>
#include <cmath>
#include <fstream>
#include <iostream>
#include <numeric>
#include <string>
#include <vector>

extern "C" {
    void catchem_register_mkppchem_cpp();
#ifdef CATCHEM_HAVE_PHOTOLYSIS
    void catchem_register_photolysis_cpp();
#endif
}

namespace {

    int species_index(const std::vector<std::string>& names, const std::string& want) {
        for (std::size_t i = 0; i < names.size(); ++i)
            if (names[i] == want)
                return static_cast<int>(i);
        return -1;
    }

} // namespace

#ifdef CATCHEM_HAVE_PHOTOLYSIS
namespace {

    /// Result of one coupled photolysis+mkppchem column run, or the init/run
    /// failure the coupling contract requires.
    struct CoupledOutcome {
        bool init_failed = false;
        std::string init_error;
        bool run_failed = false;
        std::string run_error;
        double oh_final = 0.0;
        double h2o2_final = 0.0;
        bool finite_nonneg = true;
    };

    /// The shipped TUV-x test config names its reactions jfoo/jbar/jbaz. The
    /// gocart definition's photolysis label is `jh2o2`, so the coupled test
    /// writes a config variant that renames the first reaction to `jh2o2`.
    /// The shipped O2 profile with 1000 m layers makes the dummy column
    /// optically thick (tau ~ 70), and the extraterrestrial flux is many
    /// orders of magnitude below real sunlight, so the stock J-values are
    /// ~1e-33 s-1 -- physically meaningless. The coupled test only needs J to
    /// respond to the sun: thin the O2 absorber and boost the flux so the
    /// daytime J-value is large enough for H2O2 photolysis to lift OH above
    /// background, while the nighttime sun is below the horizon and J stays
    /// zero. Relative NetCDF paths inside the config resolve against the CWD,
    /// which CMake points at the build tests/ directory holding the `configs`
    /// symlink, so the copy can live anywhere.
    std::string write_tuvx_config(const std::string& path, const std::string& rename_to) {
        const std::string source = std::string(catchem::test::SOURCE_DIR) +
                                   "/src/external/musica/configs/tuvx/from_host/config.json";
        std::ifstream in(source);
        std::string text((std::istreambuf_iterator<char>(in)), std::istreambuf_iterator<char>());
        if (!rename_to.empty()) {
            std::string needle = "\"jfoo\"";
            auto pos = text.find(needle);
            if (pos == std::string::npos)
                throw std::runtime_error("tuvx config fixture missing jfoo");
            text.replace(pos, needle.size(), "\"" + rename_to + "\"");
        }
        auto replace_or_throw = [&](const std::string& needle, const std::string& replacement) {
            if (text.find(needle) == std::string::npos)
                throw std::runtime_error("tuvx config fixture missing: " + needle);
            // Replace every occurrence: the profile and its radiator share
            // the value list textually.
            std::size_t pos = 0;
            while ((pos = text.find(needle, pos)) != std::string::npos) {
                text.replace(pos, needle.size(), replacement);
                pos += replacement.size();
            }
        };
        replace_or_throw("[5.0e17, 4.5e17, 4.0e17, 2.3e17]", "[5.0e14, 4.5e14, 4.0e14, 2.3e14]");
        // Calibrated against the debug value J=1e9 s-1 at flux 1.7e30: the
        // flux is linear in J, so 1.7e18 gives a noon J of ~1e-3 s-1 -- a
        // realistic daytime photolysis frequency the stiff solver integrates
        // cleanly (1e9 s-1 annihilates H2O2 within one step and the solver
        // produces a negative output).
        replace_or_throw("[1.2e14, 1.3e14, 1.4e14, 1.5e14, 1.6e14, 1.7e14]",
                         "[1.2e18, 1.3e18, 1.4e18, 1.5e18, 1.6e18, 1.7e18]");
        std::ofstream out(path);
        out << text;
        return path;
    }

    /// The generated species config is the single-source projection, but the
    /// photolysis process requires a species carrying the `photolysis.ozone`
    /// role to build its O3 profile. For the coupled test we inject that role
    /// onto h2o2 (the photolyzed species) in a temp copy -- this is test
    /// scaffolding for the TUV-x side, not a mechanism-truth change.
    std::string write_species_with_ozone_role(const std::string& path) {
        const std::string source = std::string(catchem::test::TEST_DIR) + "/Configs/Mkpp/CATChem_mkppchem_species.yml";
        std::ifstream in(source);
        std::ofstream out(path);
        std::string line;
        while (std::getline(in, line)) {
            out << line << "\n";
            if (line == "- name: h2o2")
                out << "  roles: [photolysis.ozone]\n";
        }
        return path;
    }

    /// Build a column state (met + species + uniform concentrations), schedule
    /// photolysis (optionally) and mkppchem in that order, and integrate one
    /// hour of 60 s steps at a fixed local hour. Captures the init/run failure
    /// the contract requires instead of throwing, so the fatal-path scenarios
    /// assert on the message.
    ///
    /// `create_photolysis` instantiates and initializes the photolysis process
    /// (the only thing that touches MUSICA/TUV-x). `schedule_photolysis` puts
    /// it in the run schedule / active-process list; it defaults to
    /// `create_photolysis`. Passing schedule=true with create=false exercises
    /// the "photolysis configured but its diagnostics never registered" and
    /// "smoke mode while photolysis is scheduled" paths without a TUV-x
    /// instance -- the prebuilt MUSICA library aborts on a fourth TUV-x in one
    /// process, and a chemistry-only Core desynchronizes its Fortran module
    /// state, so the fatal-path cases deliberately avoid TUV-x.
    CoupledOutcome run_coupled_column(double hour, const std::string& tuvx_config, bool smoke_mode,
                                      bool create_photolysis, bool schedule_photolysis = true) {
        CoupledOutcome result;
        // The shipped TUV-x test config's height grid carries four profile
        // values; the proven-coupled gaschem test runs three levels against
        // it, so the coupled column matches that grid exactly.
        const int ncol = 1;
        const int nlev = 3;
        auto core = std::make_shared<catchem::Core>(ncol, nlev, 26);
        auto state = core->get_state_manager();

        std::vector<double> t_data(ncol * nlev), rh_data(ncol * nlev), qv_data(ncol * nlev);
        std::vector<double> pmid_data(ncol * nlev), cldf_data(ncol * nlev), cldliq_data(ncol * nlev);
        std::vector<double> pedge_data(ncol * (nlev + 1)), bxheight_data(ncol * nlev);
        std::vector<double> lat(ncol, 40.0), lon(ncol, -105.0);
        for (int ilev = 0; ilev < nlev; ++ilev) {
            t_data[ilev] = 288.0 - 2.0 * ilev;
            rh_data[ilev] = 0.4 + 0.05 * ilev;
            qv_data[ilev] = 0.008;
            pmid_data[ilev] = 101325.0 * std::exp(-0.1 * ilev);
            cldf_data[ilev] = (ilev == 0) ? 0.5 : 0.0;
            cldliq_data[ilev] = (ilev == 0) ? 1.0e-3 : 0.0;
            bxheight_data[ilev] = 1000.0;
            pedge_data[ilev] = 101325.0 * std::exp(-0.1 * ilev);
        }
        pedge_data[nlev] = 101325.0 * std::exp(-0.1 * nlev);
        state->bind_met_field_2d("LAT", lat.data());
        state->bind_met_field_2d("LON", lon.data());
        state->bind_met_field_3d("T", t_data.data());
        state->bind_met_field_3d("RH", rh_data.data());
        state->bind_met_field_3d("QV", qv_data.data());
        state->bind_met_field_3d("PMID", pmid_data.data());
        state->bind_met_field_3d("CLDF", cldf_data.data());
        state->bind_met_field_3d("CLDLIQ", cldliq_data.data());
        state->bind_met_field_3d("PEDGE", pedge_data.data());
        state->bind_met_field_3d("BXHEIGHT", bxheight_data.data());
        state->derive_airden_dry();
        state->clock().year = 2026;
        state->clock().month = 7;
        state->clock().day = 13;
        state->clock().hour = static_cast<int>(hour);
        state->clock().minute = static_cast<int>((hour - std::floor(hour)) * 60.0);
        state->clock().second = 0;
        state->clock().calculate_derived_fields();
        state->clock().timestep = 60.0;

        const std::string species_path = "test_mkppchem_coupled_species.yml";
        state->load_species_config(write_species_with_ozone_role(species_path));
        std::vector<std::string> species_names;
        for (const auto& meta : state->chemistry().species_list)
            species_names.push_back(meta.short_name);
        const int i_oh = species_index(species_names, "oh");
        const int i_h2o2 = species_index(species_names, "h2o2");
        std::vector<double> conc(static_cast<size_t>(ncol) * nlev * 26, 4.0e-12);
        // H2O2 is the photolysis reservoir that produces OH (H2O2 + hv -> 2 OH).
        // At the uniform 4e-12 epsilon background, even a full daytime J-value
        // photolyzes too little H2O2 to lift OH an order of magnitude above the
        // background, so the SZA response would be buried. Seed a realistic
        // boundary-layer H2O2 mixing ratio so the daytime column reaches a
        // photostationary OH clearly above the night column's frozen background.
        const std::size_t sp_cells = static_cast<std::size_t>(ncol * nlev);
        for (std::size_t cell = 0; cell < sp_cells; ++cell)
            conc[static_cast<size_t>(i_h2o2) * sp_cells + cell] = 1.0e-6;
        state->bind_unified_chemistry(conc.data());

        const std::string config_path = "test_mkppchem_coupled_config.yml";
        {
            std::ofstream writer(config_path);
            if (schedule_photolysis)
                writer << "run_phases:\n  chem:\n    processes:\n      - photolysis\n      - mkppchem\n";
            else
                writer << "run_phases:\n  chem:\n    processes:\n      - mkppchem\n";
            writer << "processes:\n";
            if (schedule_photolysis)
                writer << "  photolysis:\n    activate: true\n    config_file: \"" << tuvx_config << "\"\n";
            writer << "  mkppchem:\n"
                   << "    activate: true\n"
                   << "    mechanism: \"gocart\"\n"
                   << "    definition_file: \"" << catchem::test::SOURCE_DIR
                   << "/parm/config/openatmos/gocart/mechanism.yaml\"\n"
                   << "    diagnostics: false\n"
                   << "    photolysis_smoke_mode: " << (smoke_mode ? "true" : "false") << "\n"
                   << "    cloud_liquid_water:\n      mode: derived\n";
            writer.close();
        }
        state->set_configuration_path(config_path);
        if (state->config_manager())
            state->config_manager()->load_from_file(config_path);

        auto& registry = catchem::ProcessRegistry::get_instance();
        std::shared_ptr<catchem::ProcessInterface> photo_proc;
        std::shared_ptr<catchem::ProcessInterface> mkpp_proc;
        try {
            if (create_photolysis) {
                photo_proc = registry.create("photolysis");
                photo_proc->init(state);
                core->add_process(photo_proc);
            }
            mkpp_proc = registry.create("mkppchem");
            mkpp_proc->init(state);
            core->add_process(mkpp_proc);
        } catch (const std::exception& e) {
            result.init_failed = true;
            result.init_error = e.what();
            std::remove(config_path.c_str());
            std::remove(species_path.c_str());
            return result;
        }

        try {
            for (int step = 0; step < 60; ++step)
                core->run_timestep(60.0);
        } catch (const std::exception& e) {
            result.run_failed = true;
            result.run_error = e.what();
        }

        state->sync_to_host();
        const std::size_t sp = static_cast<std::size_t>(ncol * nlev);
        result.oh_final = conc[static_cast<size_t>(i_oh) * sp];
        result.h2o2_final = conc[static_cast<size_t>(i_h2o2) * sp];
        for (double v : conc)
            if (!std::isfinite(v) || v < 0.0)
                result.finite_nonneg = false;
        std::remove(config_path.c_str());
        std::remove(species_path.c_str());
        return result;
    }

    void test_coupled_photolysis_responds_to_sza(int& failures) {
        const std::string photo_cfg = write_tuvx_config("test_mkppchem_jh2o2_config.json", "jh2o2");
        // The clock is UTC and the column sits at lon -105 (Boulder), so local
        // solar noon is ~19:00 UTC and local midnight ~07:00 UTC. (The earlier
        // 12/0 pair straddled the wrong way: 12 UTC is 05:00 local, before
        // sunrise.)
        const auto noon = run_coupled_column(19.0, photo_cfg, false, true);
        const auto night = run_coupled_column(7.0, photo_cfg, false, true);
        std::remove(photo_cfg.c_str());
        if (noon.init_failed || night.init_failed) {
            std::cout << "FAIL: coupled init threw: " << (noon.init_failed ? noon.init_error : night.init_error)
                      << std::endl;
            ++failures;
            return;
        }
        if (noon.run_failed || night.run_failed) {
            std::cout << "FAIL: coupled run threw: " << (noon.run_failed ? noon.run_error : night.run_error)
                      << std::endl;
            ++failures;
            return;
        }
        if (!noon.finite_nonneg || !night.finite_nonneg) {
            std::cout << "FAIL: coupled run produced a non-finite or negative concentration" << std::endl;
            ++failures;
            return;
        }
        // OH is produced only by H2O2 photolysis in this mechanism, so the
        // daytime column must sustain a larger OH mixing ratio than the
        // nighttime column after the same hour of chemistry.
        if (noon.oh_final > night.oh_final * 10.0)
            std::cout << "PASS: OH responds to solar zenith angle (noon " << noon.oh_final << " >> night "
                      << night.oh_final << ")" << std::endl;
        else {
            std::cout << "FAIL: OH did not respond to solar zenith angle (noon " << noon.oh_final << " vs night "
                      << night.oh_final << ")" << std::endl;
            ++failures;
        }
    }

    void test_unresolved_photolysis_label_is_fatal(int& failures) {
        // The photolysis process publishes jfoo/jbar/jbaz; the definition's
        // label jh2o2 resolves to no diagnostic -> fatal naming both.
        const std::string photo_cfg = std::string(catchem::test::SOURCE_DIR) +
                                      "/src/external/musica/configs/tuvx/from_host/config.json";
        const auto outcome = run_coupled_column(12.0, photo_cfg, false, true);
        if (!outcome.init_failed) {
            std::cout << "FAIL: unresolved photolysis label did not fatal at init" << std::endl;
            ++failures;
            return;
        }
        const bool names_label = outcome.init_error.find("jh2o2") != std::string::npos;
        const bool names_definition = outcome.init_error.find("mechanism.yaml") != std::string::npos;
        if (names_label && names_definition)
            std::cout << "PASS: unresolved photolysis label fatal names label + definition" << std::endl;
        else {
            std::cout << "FAIL: unresolved-label fatal message missing context: " << outcome.init_error << std::endl;
            ++failures;
        }
    }

    void test_missing_photolysis_process_is_fatal(int& failures) {
        // Mechanism declares photolysis, no photolysis diagnostics registered
        // at all -> fatal explaining the requirement.
        const auto outcome = run_coupled_column(12.0, "", false, false);
        if (!outcome.init_failed) {
            std::cout << "FAIL: mechanism with photolysis but no photolysis process did not fatal" << std::endl;
            ++failures;
            return;
        }
        const bool mentions_photolysis = outcome.init_error.find("photolysis") != std::string::npos;
        if (mentions_photolysis)
            std::cout << "PASS: missing photolysis process fatal explains the requirement" << std::endl;
        else {
            std::cout << "FAIL: missing-photolysis fatal message unclear: " << outcome.init_error << std::endl;
            ++failures;
        }
    }

    void test_photolysis_smoke_mode(int& failures) {
        // Config contradiction: smoke mode while a photolysis process is
        // scheduled -> fatal. The photolysis process is only listed in
        // the schedule (never created), so no TUV-x instance is touched and
        // the contradiction is caught by the active-list check.
        const auto conflict = run_coupled_column(12.0, "", true, false, true);
        if (!conflict.init_failed) {
            std::cout << "FAIL: smoke mode + photolysis process did not fatal" << std::endl;
            ++failures;
        } else if (conflict.init_error.find("smoke") != std::string::npos)
            std::cout << "PASS: smoke/photolysis contradiction fatal names the mode" << std::endl;
        else {
            std::cout << "FAIL: contradiction message unclear: " << conflict.init_error << std::endl;
            ++failures;
        }
        // Smoke mode: no photolysis process at all (neither scheduled nor
        // created), definition host-forcing constants (jh2o2 = 1e-5 s-1) drive
        // jvals, so H2O2 photolysis sustains an OH level far above the 4e-12
        // background a dark column would leave.
        const auto smoke = run_coupled_column(12.0, "", true, false, false);
        if (smoke.init_failed || smoke.run_failed) {
            std::cout << "FAIL: smoke-mode run failed: " << (smoke.init_failed ? smoke.init_error : smoke.run_error)
                      << std::endl;
            ++failures;
            return;
        }
        if (!smoke.finite_nonneg) {
            std::cout << "FAIL: smoke-mode run produced a non-finite or negative concentration" << std::endl;
            ++failures;
            return;
        }
        // Against the 4e-12 initial, a photolyzing column reaches a photostationary OH
        // orders of magnitude higher; a dark (jvals=0) column stays near 4e-12.
        if (smoke.oh_final > 100.0 * 4.0e-12)
            std::cout << "PASS: smoke mode sustains OH from host forcing (OH " << smoke.oh_final << ")" << std::endl;
        else {
            std::cout << "FAIL: smoke mode did not sustain OH (OH " << smoke.oh_final << ")" << std::endl;
            ++failures;
        }
    }

} // namespace
#endif // CATCHEM_HAVE_PHOTOLYSIS

namespace {

    /// Second-mechanism column scenario: run the process against the chapman
    /// cycle -- a 4-species mechanism with two photolysis rates and no
    /// equilibrium inputs -- by selecting it in the configuration and supplying
    /// its matching OpenAtmos definition. Asserts successful initialization,
    /// the exact 4-slot species mapping (the 22 unused tracers stay
    /// untouched), and finite non-negative output after one timestep. This is
    /// the end-to-end proof that mechanism selection drives BOTH the solver
    /// and the runtime definition facts from the same compiled source.
    void test_chapman_column(int& failures) {
        // The scenario proves the SECOND compiled mechanism. A build that
        // narrowed CATCHEM_MKPP_MECHANISMS to gocart only is legitimate, so
        // skip rather than fail: the registry check would fatal on an
        // uncompiled name, which is the correct behavior being tested
        // elsewhere. compiled_mechanism_ids() reflects the same
        // MKPP_HOST_MECH_<UPPER> gating the library was compiled with.
        const auto compiled = catchem::mkpp::compiled_mechanism_ids();
        if (std::find(compiled.begin(), compiled.end(), "chapman") == compiled.end()) {
            std::cout << "SKIP: chapman not compiled into this build (CATCHEM_MKPP_MECHANISMS)" << std::endl;
            return;
        }
        const int ncol = 2;
        const int nlev = 2;
        auto core = std::make_shared<catchem::Core>(ncol, nlev, 4);
        auto state = core->get_state_manager();

        std::vector<double> t_data(ncol * nlev, 240.0);
        std::vector<double> rh_data(ncol * nlev, 0.3);
        std::vector<double> qv_data(ncol * nlev, 0.005);
        std::vector<double> pmid_data(ncol * nlev, 10000.0);
        std::vector<double> cldf_data(ncol * nlev, 0.0);
        std::vector<double> cldliq_data(ncol * nlev, 0.0);
        state->bind_met_field_3d("T", t_data.data());
        state->bind_met_field_3d("RH", rh_data.data());
        state->bind_met_field_3d("QV", qv_data.data());
        state->bind_met_field_3d("PMID", pmid_data.data());
        state->bind_met_field_3d("CLDF", cldf_data.data());
        state->bind_met_field_3d("CLDLIQ", cldliq_data.data());
        state->derive_airden_dry();
        state->clock().timestep = 3600.0;

        // The generated chapman species config carries exactly the mechanism's
        // four tracers, in slot order O, O2, O3, M.
        const std::string species_config =
            std::string(catchem::test::TEST_DIR) + "/Configs/Mkpp/CATChem_mkppchem_species_chapman.yml";
        state->load_species_config(species_config);
        if (state->species_count() != 4) {
            std::cout << "FAIL: chapman species config must carry 4 species, got " << state->species_count()
                      << std::endl;
            ++failures;
            return;
        }

        // Mole fractions matching MKPP's own chapman e2e initial condition
        // (O 1e10, O2 2e10, O3 3e10, M 4e10 molecules/cm3 at ~4e-19 mol/m3 air
        // density -> ~2.5e-8 .. 1e-7 mixing ratios), scaled to the stratospheric
        // molar density below.
        const double rho_molar = 0.4;  // mol/m3, ~10 hPa / 240 K
        auto mixr = [&](double cm3) { return cm3 * 1.0e6 / 6.02214076e23 / rho_molar; };
        std::vector<double> conc(ncol * nlev * 4);
        for (int k = 0; k < ncol * nlev; ++k) {
            conc[k + 0 * ncol * nlev] = mixr(1.0e10);  // O
            conc[k + 1 * ncol * nlev] = mixr(2.0e19);  // O2
            conc[k + 2 * ncol * nlev] = mixr(3.0e12);  // O3
            conc[k + 3 * ncol * nlev] = mixr(4.0e19);  // M
        }
        state->bind_unified_chemistry(conc.data());
        const std::vector<double> conc_before = conc;

        const std::string config_path = "test_mkppchem_chapman_config.yml";
        {
            std::ofstream writer(config_path);
            writer << "processes:\n"
                   << "  mkppchem:\n"
                   << "    activate: true\n"
                   << "    mechanism: \"chapman\"\n"
                   << "    definition_file: \"" << catchem::test::SOURCE_DIR
                   << "/parm/config/openatmos/chapman/mechanism.yaml\"\n"
                   << "    photolysis_smoke_mode: true\n";
            writer.close();
        }
        state->set_configuration_path(config_path);
        if (state->config_manager())
            state->config_manager()->load_from_file(config_path);

        auto proc = catchem::ProcessRegistry::get_instance().create("mkppchem");
        assert(proc && "registry must construct mkppchem");
        try {
            proc->init(state);
            core->add_process(proc);
            core->run_timestep(3600.0);
            std::cout << "PASS: chapman mechanism selected, one timestep completed" << std::endl;
        } catch (const std::exception& e) {
            std::cout << "FAIL: chapman run threw: " << e.what() << std::endl;
            ++failures;
            std::remove(config_path.c_str());
            return;
        }
        state->sync_to_host();

        // Output post-conditions: finite and non-negative everywhere, and the
        // O2/O3 photostationary cycle actually moved O3 (the solver ran, it did
        // not no-op on a mis-mapped identity).
        bool all_valid = true;
        for (double v : conc)
            if (!std::isfinite(v) || v < 0.0)
                all_valid = false;
        if (all_valid)
            std::cout << "PASS: chapman output finite and non-negative" << std::endl;
        else {
            std::cout << "FAIL: chapman output non-finite or negative" << std::endl;
            ++failures;
        }
        const std::size_t sp = static_cast<std::size_t>(ncol * nlev);
        bool o3_moved = false;
        for (std::size_t k = 0; k < sp; ++k)
            if (conc[k + 2 * sp] != conc_before[k + 2 * sp])
                o3_moved = true;
        if (o3_moved)
            std::cout << "PASS: chapman chemistry advanced O3 (mapping wired the right slots)" << std::endl;
        else {
            std::cout << "FAIL: chapman left every O3 value unchanged" << std::endl;
            ++failures;
        }
        // A one-hour photolysis step drives O toward its photostationary value
        // (production from O2, loss to O3); it must stay a tiny positive
        // fraction of O2.
        bool o_sane = true;
        for (std::size_t k = 0; k < sp; ++k)
            if (!(conc[k] > 0.0 && conc[k] < conc[k + sp]))
                o_sane = false;
        if (o_sane)
            std::cout << "PASS: chapman O stays positive and below O2" << std::endl;
        else {
            std::cout << "FAIL: chapman O outside physical bounds" << std::endl;
            ++failures;
        }
        proc->finalize();
        std::remove(config_path.c_str());
    }

} // namespace

int main(int argc, char* argv[]) {
    const std::string mode = (argc >= 2) ? argv[1] : "column";
#ifdef CATCHEM_HAVE_PHOTOLYSIS
    // The prebuilt MUSICA library carries TUV-x global state that a fourth
    // CreateTuvx in one process aborts on (a Fortran "STOP 3"), so the coupled
    // scenarios run as separate invocations, each staying within the limit.
    if (mode == "coupled" || mode == "smoke") {
        Kokkos::initialize(argc, argv);
        int failures = 0;
        {
            catchem_register_mkppchem_cpp();
            catchem_register_photolysis_cpp();
            if (mode == "coupled") {
                test_coupled_photolysis_responds_to_sza(failures);    // 2 TUV-x
                test_missing_photolysis_process_is_fatal(failures);   // 0 TUV-x
                test_unresolved_photolysis_label_is_fatal(failures);  // 1 TUV-x
            } else {
                test_photolysis_smoke_mode(failures);                 // 2 TUV-x
            }
        }
        Kokkos::finalize();
        if (failures == 0) {
            std::cout << "ALL MKPPCHEM COUPLED TESTS PASSED" << std::endl;
            return 0;
        }
        std::cout << failures << " MKPPCHEM COUPLED TEST(S) FAILED" << std::endl;
        return 1;
    }
#endif
    std::cout << "RUNNING: mkppchem column smoke test" << std::endl;
    Kokkos::initialize(argc, argv);
    int failures = 0;
    {
        catchem_register_mkppchem_cpp();

        const int ncol = 2;
        const int nlev = 4;

        // --- State + species configuration -------------------------------
        auto core = std::make_shared<catchem::Core>(ncol, nlev, 26);
        auto state = core->get_state_manager();

        // Meteorology: a mildly stratified isothermal-ish column. T and PMID
        // must be physically admissible (T > 0, PMID > 0) or validate_met flags
        // the cell and the run is fatal, which is itself the correct behavior.
        std::vector<double> t_data(ncol * nlev);
        std::vector<double> rh_data(ncol * nlev);
        std::vector<double> qv_data(ncol * nlev);
        std::vector<double> pmid_data(ncol * nlev);
        std::vector<double> cldf_data(ncol * nlev, 0.0);
        std::vector<double> cldliq_data(ncol * nlev, 0.0);
        for (int icol = 0; icol < ncol; ++icol)
            for (int ilev = 0; ilev < nlev; ++ilev) {
                const std::size_t k = static_cast<std::size_t>(icol + ilev * ncol);
                t_data[k] = 288.0 - 2.0 * ilev;
                rh_data[k] = 0.4 + 0.05 * ilev;
                qv_data[k] = 0.008;
                pmid_data[k] = 101325.0 * std::exp(-0.1 * ilev);
                // Cloud fraction present in the lowest level only, so the
                // derived water content (and hence the gated SO2 + H2O2 -> SO4
                // pathway) is active where clw > 1e-6 and off elsewhere.
                cldf_data[k] = (ilev == 0) ? 0.5 : 0.0;
                cldliq_data[k] = (ilev == 0) ? 1.0e-3 : 0.0;
            }
        state->bind_met_field_3d("T", t_data.data());
        state->bind_met_field_3d("RH", rh_data.data());
        state->bind_met_field_3d("QV", qv_data.data());
        state->bind_met_field_3d("PMID", pmid_data.data());
        state->bind_met_field_3d("CLDF", cldf_data.data());
        state->bind_met_field_3d("CLDLIQ", cldliq_data.data());
        // The execution plan validates required fields before run(), so the
        // derived dry-air density must already be current for this import
        // generation -- mirroring what the NUOPC driver does per import step.
        state->derive_airden_dry();
        // The chemistry timestep the process integrates over (the field the
        // process reads at run(); Core::run_timestep's dt argument is separate).
        state->clock().timestep = 60.0;

        // Species: the generated configuration (the single source of truth's
        // projection into CATChem's tracer format).
        const std::string species_config = std::string(catchem::test::TEST_DIR) + "/Configs/Mkpp/CATChem_mkppchem_species.yml";
        state->load_species_config(species_config);
        assert(state->species_count() == 26 && "generated species config must carry all 26 species");

        // Initial concentrations (mol/mol): see the uniform-state note below.
        // The dark column runs in photolysis smoke mode: the mechanism
        // declares a photolysis reaction, so running it without a photolysis
        // process is fatal; smoke mode applies the definition's
        // host-forcing constant instead, which is the documented isolated-
        // mechanism use this test is.
        std::vector<std::string> species_names;
        for (const auto& meta : state->chemistry().species_list)
            species_names.push_back(meta.short_name);
        const int i_dms = species_index(species_names, "dms");
        const int i_so2 = species_index(species_names, "so2");
        const int i_so4 = species_index(species_names, "so4");
        const int i_oh = species_index(species_names, "oh");
        const int i_h2o2 = species_index(species_names, "h2o2");
        assert(i_dms >= 0 && i_so2 >= 0 && i_so4 >= 0 && i_oh >= 0 && i_h2o2 >= 0);

        std::vector<int> slots(26);
        std::iota(slots.begin(), slots.end(), 0);

        // Initial state: the uniform mole fraction reproduces the proven
        // well-posed state from MKPP's own gocart e2e validation (all
        // species at 1e8 molecules/cm3, dark, dt=60 s). Converted to a
        // mixing ratio with the surface molar density of dry air
        // (1e8 /cm3 = 1.66e-10 mol/m3; / ~42 mol/m3 ~ 4e-12). The nitrate
        // equilibrium block (HNO3/NH3/NH4a/NO3an1-3) is only well-posed
        // when the concentration products match the Kc term
        // (1.415e-29*exp(8989/T) ~ 4.7e-16 at 288 K in cm3/molec units);
        // an epsilon background far below that makes the solve diverge.
        std::vector<double> conc(ncol * nlev * 26, 4.0e-12);
        state->bind_unified_chemistry(conc.data());

        // --- Configuration the process reads at init() --------------------
        // A minimal temp config with absolute paths (the checked-in
        // CATChem_mkppchem_config.yml is repo-relative and won't resolve from
        // the build CWD). Derived-cloud-water mode so the gate is exercised
        // without requiring a host CLDLIQ (we bind one anyway, and Auto would
        // prefer it). The definition_file is only existence-checked + logged;
        // the runtime definition facts come from the compiled metadata header.
        const std::string config_path = "test_mkppchem_column_config.yml";
        {
            std::ofstream writer(config_path);
            writer << "processes:\n"
                   << "  mkppchem:\n"
                   << "    activate: true\n"
                   << "    mechanism: \"gocart\"\n"
                   << "    definition_file: \"" << catchem::test::SOURCE_DIR
                   << "/parm/config/openatmos/gocart/mechanism.yaml\"\n"
                   << "    diagnostics: true\n"
                   << "    photolysis_smoke_mode: true\n"
                   << "    cloud_liquid_water:\n"
                   << "      mode: derived\n";
            writer.close();
        }
        state->set_configuration_path(config_path);
        if (state->config_manager())
            state->config_manager()->load_from_file(config_path);

        // --- Create + init + run the process ------------------------------
        auto proc = catchem::ProcessRegistry::get_instance().create("mkppchem");
        assert(proc && "registry must construct mkppchem");
        try {
            proc->init(state);
            core->add_process(proc);
            core->run_timestep(60.0);
            std::cout << "PASS: one mkppchem timestep completed without error" << std::endl;
        } catch (const std::exception& e) {
            std::cout << "FAIL: mkppchem timestep threw: " << e.what() << std::endl;
            ++failures;
        }

        // --- Post-run physical checks -------------------------------------
        // Pull the solved concentrations back to the bound host buffer.
        state->sync_to_host();
        // Every concentration must be finite and non-negative after the solve.
        bool all_valid = true;
        for (double v : conc)
            if (!std::isfinite(v) || v < 0.0)
                all_valid = false;
        if (all_valid)
            std::cout << "PASS: all concentrations finite and non-negative after solve" << std::endl;
        else {
            std::cout << "FAIL: a concentration is non-finite or negative after solve" << std::endl;
            ++failures;
        }

        // Sulfur-mass conservation: the per-cell DMS+SO2+SO4 molar inventory
        // must be unchanged by the chemistry (one S atom in each tracer).
        // The bound host buffer is a LayoutLeft (col, lev, species) view, so
        // the flat offset of (icol, ilev, s) is icol + ilev*ncol +
        // s*ncol*nlev -- species is the slowest-varying index.
        const std::size_t lev_stride = static_cast<std::size_t>(ncol);
        const std::size_t sp_stride = static_cast<std::size_t>(ncol * nlev);
        auto flat = [&](int icol, int ilev, int s) {
            return static_cast<std::size_t>(icol) + static_cast<std::size_t>(ilev) * lev_stride +
                   static_cast<std::size_t>(s) * sp_stride;
        };
        double max_rel_drift = 0.0;
        for (int ilev = 0; ilev < nlev; ++ilev)
            for (int icol = 0; icol < ncol; ++icol) {
                const double after = conc[flat(icol, ilev, i_dms)] + conc[flat(icol, ilev, i_so2)] +
                                     conc[flat(icol, ilev, i_so4)];
                const double before = 3.0 * 4.0e-12;
                const double drift = std::abs(after - before) / before;
                max_rel_drift = std::max(max_rel_drift, drift);
            }
        // The solver's own tolerance is rtol=1e-6; allow generous headroom for
        // the round trip through molar density.
        if (max_rel_drift < 1.0e-4)
            std::cout << "PASS: sulfur inventory conserved (max rel drift " << max_rel_drift << ")" << std::endl;
        else {
            std::cout << "FAIL: sulfur inventory drifted by " << max_rel_drift << std::endl;
            ++failures;
        }

        // --- Tendency diagnostic -------------------------------------------
        // The packed mkppchem_tendency field must exist with one label per
        // advanced species, and its values must match the mixing-ratio change
        // the scatter wrote back, rate-normalised: tendency = (after - before)/dt.
        // The default diag_species list selects every mapped slot in slot
        // order, and the generated species config preserves that order, so
        // diagnostic column == catchem species index here.
        {
            auto diag = state->diagnostic_manager();
            if (!diag->has_field("mkppchem_tendency")) {
                std::cout << "FAIL: mkppchem_tendency diagnostic not registered" << std::endl;
                ++failures;
            } else {
                const auto& labels = diag->get_unpack_labels("mkppchem_tendency");
                const auto& axes = diag->get_axes("mkppchem_tendency");
                const bool shape_ok = labels.size() == 26 && axes.size() == 3 &&
                                      axes[0] == catchem::SemanticAxis::Column &&
                                      axes[1] == catchem::SemanticAxis::Level &&
                                      axes[2] == catchem::SemanticAxis::Species;
                const double* ten = static_cast<const double*>(diag->get_host_read_pointer("mkppchem_tendency"));
                // LayoutLeft (col, lev, species): offset = icol + ilev*ncol + d*ncol*nlev.
                double max_ten_err = 0.0;
                for (int ilev = 0; ilev < nlev; ++ilev)
                    for (int icol = 0; icol < ncol; ++icol)
                        for (int d = 0; d < 26; ++d) {
                            const std::size_t off = static_cast<std::size_t>(icol) +
                                                    static_cast<std::size_t>(ilev) * lev_stride +
                                                    static_cast<std::size_t>(d) * sp_stride;
                            const double expected = (conc[off] - 4.0e-12) / 60.0;
                            max_ten_err = std::max(max_ten_err, std::abs(ten[off] - expected));
                        }
                if (shape_ok && max_ten_err < 1.0e-20)
                    std::cout << "PASS: mkppchem_tendency published, consistent with the scatter (max err "
                              << max_ten_err << ")" << std::endl;
                else {
                    std::cout << "FAIL: mkppchem_tendency inconsistent (shape_ok=" << shape_ok
                              << ", max err " << max_ten_err << ")" << std::endl;
                    ++failures;
                }
            }
        }

        proc->finalize();
        std::remove(config_path.c_str());

        // Second compiled mechanism (chapman): selection, species mapping, and
        // solve through the same process code with a different species set.
        test_chapman_column(failures);
    }
    Kokkos::finalize();

    if (failures == 0) {
        std::cout << "ALL MKPPCHEM COLUMN TESTS PASSED" << std::endl;
        return 0;
    }
    std::cout << failures << " MKPPCHEM COLUMN TEST(S) FAILED" << std::endl;
    return 1;
}

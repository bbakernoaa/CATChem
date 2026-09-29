// tests/test_catchem_mkppchem_units.cpp
//
// Unit-level checks for the mkppchem process that need no solver run:
// registration, the field contract (device intent, required fields), the
// settings validator, the unit-conversion identities of the state bridge
// (mol/mol -> mol/m3 -> mol/mol reversibility, dry-air molar density), and the
// configuration-validation failure paths. Every failure-path check asserts the
// *content* of the fatal message -- the offending name, path, or count, not
// merely that something threw -- so the cause is locatable from the run log
// alone. The failure-path checks report through a counter rather than assert()
// so the gate stays live in Release builds (assert() compiles out under
// NDEBUG); the pre-existing structural checks keep their assert() form.
#include "catchem_config_manager.hpp"
#include "catchem_constants.hpp"
#include "catchem_core.hpp"
#include "catchem_process_interface.hpp"
#include "catchem_process_mkppchem.hpp"
#include "catchem_process_registry.hpp"
#include "catchem_state_manager.hpp"
#include "catchem_mkpp_mechanism.hpp"
#include "catchem_mkpp_state_bridge.hpp"
#include "catchem_api.hpp"
#include "catchem_test_config.hpp"

#include <Kokkos_Core.hpp>
#include <yaml-cpp/yaml.h>

#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdio>
#include <filesystem>
#include <fstream>
#include <functional>
#include <iostream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

extern "C" {
void catchem_register_mkppchem_cpp();
}

namespace {

    int g_failures = 0;

    /// NDEBUG-independent check: the failure-path assertions below are the
    /// gate for the validation implementation, so a miss must abort the test
    /// in Release and Debug alike.
    void require(bool condition, const std::string& what) {
        if (condition) {
            std::cout << "PASS: " << what << std::endl;
        } else {
            std::cerr << "FAIL: " << what << std::endl;
            ++g_failures;
        }
    }

    /// Run `fn`, return the message of the std::exception it threw, or the
    /// empty string if it did not throw.
    std::string catch_message(const std::function<void()>& fn) {
        try {
            fn();
        } catch (const std::exception& e) {
            return e.what();
        } catch (...) {
            return "<non-std exception>";
        }
        return {};
    }

    bool has_field(const catchem::ProcessContract& contract, const std::string& name) {
        return std::any_of(contract.fields.begin(), contract.fields.end(),
                           [&](const auto& f) { return f.canonical_name == name; });
    }

    const catchem::FieldAccessContract& field(const catchem::ProcessContract& contract, const std::string& name) {
        for (const auto& f : contract.fields)
            if (f.canonical_name == name)
                return f;
        throw std::runtime_error("contract has no field '" + name + "'");
    }

    void test_registration() {
        catchem_register_mkppchem_cpp();
        auto& registry = catchem::ProcessRegistry::get_instance();
        require(registry.has_process("mkppchem"), "mkppchem registers under ProcessNames::MkppChem");
        auto proc = registry.create("mkppchem");
        require(proc != nullptr, "registry constructs a mkppchem process");
        if (proc)
            require(proc->get_name() == "mkppchem", "get_name returns the registered name");
    }

    void test_contract_device_intent() {
        const catchem::ProcessContract contract = catchem::MkppChemProcess().get_contract();
        require(contract.structurally_valid(), "contract is structurally valid");
        require(contract.process_name == "mkppchem", "contract names the process");

        // Every field the solver touches must declare Device intent, or the
        // execution plan forces a host sync the zero-copy path avoids.
        bool intent_ok = true;
        for (const std::string& name : {"CONCENTRATION", "T", "RH", "PMID", "AIRDEN_DRY"}) {
            if (!has_field(contract, name)) {
                intent_ok = false;
                continue;
            }
            const auto& f = field(contract, name);
            if (f.execution_space != catchem::ExecutionSpaceIntent::Device ||
                f.requirement != catchem::FieldRequirement::Required)
                intent_ok = false;
        }
        require(intent_ok, "solver fields declare Device intent and are Required");
        require(field(contract, "CONCENTRATION").access == catchem::AccessIntent::ReadWrite,
                "concentration is the one read/write field");

        // Cloud water and CLDF are Optional at the contract level: whether they
        // are required depends on the mechanism's gating and the configured
        // mode, enforced fatally at init(), not statically here.
        bool optional_ok = true;
        for (const std::string& name : {"CLDLIQ", "CLDF"}) {
            if (!has_field(contract, name) ||
                field(contract, name).requirement != catchem::FieldRequirement::Optional)
                optional_ok = false;
        }
        require(optional_ok, "cloud-water candidates are present and Optional in the contract");
    }

    void test_settings_validator() {
        auto& registry = catchem::ProcessRegistry::get_instance();
        catchem::ProcessConfig config;

        // A known nested option block must validate cleanly.
        YAML::Node accepted;
        accepted["cloud_liquid_water"]["mode"] = "auto";
        config.set_settings_node(accepted);
        const std::string accepted_err =
            catch_message([&] { registry.validate_settings("mkppchem", config); });
        require(accepted_err.empty(), "known nested option validates cleanly" +
                                          (accepted_err.empty() ? "" : ": " + accepted_err));

        // An unknown nested option must be rejected, naming the key.
        YAML::Node bogus;
        bogus["not_a_real_option"]["value"] = 1;
        config.set_settings_node(bogus);
        const std::string msg = catch_message([&] { registry.validate_settings("mkppchem", config); });
        require(!msg.empty() && msg.find("not_a_real_option/value") != std::string::npos,
                "unknown option rejected at configuration time naming the offending path");

        // A scalar runtime seed is a process option the validator must not
        // reject (only nested scheme blocks are option containers).
        YAML::Node seeded;
        seeded["initial_step_seed"] = 0.1;
        seeded["cloud_liquid_water"]["mode"] = "auto";
        config.set_settings_node(seeded);
        const std::string seed_err =
            catch_message([&] { registry.validate_settings("mkppchem", config); });
        require(seed_err.empty(), "scalar initial_step_seed validates cleanly" +
                                      (seed_err.empty() ? "" : ": " + seed_err));
    }

    void test_initial_step_seed_parsing() {
        // Unset (empty string) means "use the baked compile-time seed".
        require(!catchem::mkpp::parse_initial_step_seed("").has_value(),
                "empty seed string means unset (baked default)");
        // A valid fraction parses through with its exact value.
        const auto ok = catchem::mkpp::parse_initial_step_seed("0.1");
        require(ok.has_value() && *ok == 0.1, "valid seed 0.1 parses");
        const auto frac = catchem::mkpp::parse_initial_step_seed("1.0e-6");
        require(frac.has_value() && frac.value() == 1.0e-6, "literal 1.0e-6 parses");
        // Anything the integrator could stall on must be fatal, naming the value.
        for (const std::string& bad : {"0", "-1.0", "not-a-number", "nan", "inf", ""}) {
            if (bad.empty())
                continue;
            const std::string msg =
                catch_message([&] { catchem::mkpp::parse_initial_step_seed(bad); });
            require(!msg.empty() && msg.find(bad) != std::string::npos,
                    "invalid seed '" + bad + "' is fatal naming the value");
        }
    }

    // ---- unit-conversion identities of the state bridge ----

    using ConcView = Kokkos::View<double***, Kokkos::LayoutLeft>;
    using MetView = Kokkos::View<double***, Kokkos::LayoutLeft>;

    /// mol/mol -> mol/m3 -> mol/mol must be the identity within a tight
    /// relative tolerance across the physical dry-air density envelope and a
    /// broad mixing-ratio envelope (one rho_molar per cell, applied uniformly
    /// to every species in that cell).
    void test_conversion_reversibility() {
        const int ncol = 3;
        const int nlev = 4;
        const int nspecies = 6;
        const int num_cells = ncol * nlev;

        // Dry-air mass densities spanning the physical envelope plus its edges.
        const std::vector<double> airden = {0.5, 0.61, 0.98, 1.2};
        // Mixing-ratio envelope: ppt-scale through order-unity, plus exact 0.
        const std::vector<double> mr_envelope = {0.0, 1e-12, 7.3e-9, 2.5e-4, 0.05, 0.95};

        ConcView conc("conc", ncol, nlev, nspecies);
        MetView airden_v("airden", ncol, nlev, 1);
        auto conc_h = Kokkos::create_mirror_view(conc);
        auto airden_h = Kokkos::create_mirror_view(airden_v);

        // A slot permutation, so the test also proves gather/scatter agree on
        // the mapping direction (slot != catchem index everywhere).
        catchem::mkpp::DeviceSpeciesMap map;
        map.slot_to_catchem =
            Kokkos::View<int*, Kokkos::LayoutLeft, catchem::mkpp::StagingBuffers::MemSpace>("slots", nspecies);
        auto slots_h = Kokkos::create_mirror_view(map.slot_to_catchem);
        for (int s = 0; s < nspecies; ++s)
            slots_h(s) = (s * 2 + 1) % nspecies;
        Kokkos::deep_copy(map.slot_to_catchem, slots_h);

        bool gather_ok = true;
        bool round_trip_ok = true;
        for (std::size_t a = 0; a < airden.size(); ++a) {
            const double rho_dry = airden[a];
            for (int icol = 0; icol < ncol; ++icol)
                for (int ilev = 0; ilev < nlev; ++ilev) {
                    airden_h(icol, ilev, 0) = rho_dry;
                    for (int s = 0; s < nspecies; ++s)
                        conc_h(icol, ilev, s) = mr_envelope[static_cast<std::size_t>((s + a) % mr_envelope.size())];
                }
            Kokkos::deep_copy(conc, conc_h);
            Kokkos::deep_copy(airden_v, airden_h);

            // The staging buffer type the process actually uses: the bridge
            // kernels take StagingBuffers::MemSpace views, and the default-
            // trait View is a distinct type, so name the space explicitly.
            Kokkos::View<double**, Kokkos::LayoutRight, catchem::mkpp::StagingBuffers::MemSpace> staged("staged",
                                                                                                        num_cells,
                                                                                                        nspecies);
            catchem::mkpp::gather_species(conc, airden_v, map, nspecies, num_cells, ncol, staged);
            // Outbound on the same buffer: in-place round-trip, exactly the
            // sequence run() performs between gather and scatter.
            catchem::mkpp::scatter_species(staged, airden_v, map, nspecies, num_cells, ncol, conc);

            auto conc_out_h = Kokkos::create_mirror_view_and_copy(Kokkos::HostSpace{}, conc);
            auto staged_h = Kokkos::create_mirror_view_and_copy(Kokkos::HostSpace{}, staged);
            // The staged molar values must themselves equal conc*rho_molar
            // (one value per cell, applied to every species). The bridge's
            // constant is the double 28.9644 g/mol; constants::AIR_MW is `fp`
            // (float in single-precision builds), so mirror the bridge here.
            const double rho_molar = rho_dry / (28.9644 * 1.0e-3);
            for (int ilev = 0; ilev < nlev; ++ilev)
                for (int icol = 0; icol < ncol; ++icol) {
                    const int i_cell = ilev * ncol + icol; // the documented flattening
                    for (int slot = 0; slot < nspecies; ++slot) {
                        // gather reads conc at the MAPPED catchem index for
                        // this slot, not at the slot number itself.
                        const int cidx = slots_h(slot);
                        const double c0 = conc_h(icol, ilev, cidx);
                        const double staged_val = staged_h(i_cell, slot);
                        const double expected = c0 * rho_molar;
                        const double scale = std::max(1.0, std::abs(expected));
                        if (std::abs(staged_val - expected) / scale >= 1e-12)
                            gather_ok = false;
                        // scatter writes back to the same mapped index.
                        const double round_trip = conc_out_h(icol, ilev, cidx);
                        if (std::abs(round_trip - c0) / std::max(1.0, std::abs(c0)) >= 1e-12)
                            round_trip_ok = false;
                    }
                }
        }
        require(gather_ok, "gather produces conc * rho_molar_dry for every cell and species");
        require(round_trip_ok, "mol/mol -> mol/m3 -> mol/mol is the identity across the density envelope");
    }

    /// rho_molar_dry = AIRDEN_DRY / (AIR_MW * 1e-3). The bridge hardcodes the
    /// double 28.9644 g/mol; CATChem's constants::AIR_MW is `fp`, which is
    /// float in single-precision builds -- so the cross-check against the
    /// shared constant tolerates float representation, while the formula check
    /// against the bridge's own value stays exact.
    void test_dry_air_molar_density() {
        const double air_mw_kg = 28.9644 * 1.0e-3;
        const double air_mw_kg_shared = catchem::constants::AIR_MW * 1.0e-3;
        require(std::abs(air_mw_kg - air_mw_kg_shared) <= 1e-6 * air_mw_kg,
                "bridge's AIR_MW matches constants::AIR_MW within float precision");
        bool formula_ok = true;
        for (double rho_dry : {0.5, 0.8, 1.0, 1.2}) {
            const double got = catchem::mkpp::dry_air_molar_density(rho_dry);
            const double want = rho_dry / air_mw_kg;
            if (std::abs(got - want) > 1e-12 * std::abs(want))
                formula_ok = false;
        }
        require(formula_ok, "dry-air molar density matches AIRDEN_DRY/(AIR_MW*1e-3)");
        require(catchem::mkpp::dry_air_molar_density(0.0) == 0.0,
                "rho=0 maps to exactly 0, never NaN/inf (no unguarded division)");
    }

    // ---- configuration-validation failure paths ----
    //
    // Each case feeds one synthetic defect and asserts the fatal message names
    // the offender (path, species, count, or process), so the cause is
    // locatable from the run log alone.

    /// Write `content` to a uniquely named temp YAML file, returning its path.
    std::string write_temp_yaml(const std::string& tag, const std::string& content) {
        const auto path = std::filesystem::temp_directory_path() / ("catchem_mkppchem_units_" + tag + ".yaml");
        std::ofstream writer(path);
        writer << content;
        writer.close();
        return path.string();
    }

    catchem::mkpp::MechanismDefinition minimal_definition() {
        catchem::mkpp::MechanismDefinition def;
        def.name = "TestMechanism";
        def.mkpp_mechanism_id = "gocart";
        def.definition_path = "parm/config/openatmos/gocart/mechanism.yaml";
        def.species_order = {"OH", "NO3", "H2O2", "DMS", "SO2", "SO4"};
        def.photolysis_labels = {"jno3"};
        def.equilibrium_inputs = {"Temperature", "RelativeHumidity"};
        def.num_reactions = 2;
        return def;
    }

    catchem::mkpp::CompiledMechanismDescriptor minimal_artifact() {
        catchem::mkpp::CompiledMechanismDescriptor artifact;
        artifact.identifier = "gocart";
        artifact.num_species = 6;
        artifact.num_photolysis = 1;
        artifact.num_equilibrium_inputs = 2;
        artifact.artifact_path = "compiled:gocart";
        artifact.species_names = {"OH", "NO3", "H2O2", "DMS", "SO2", "SO4"};
        return artifact;
    }

    void test_definition_parse_failures() {
        using catchem::mkpp::parse_openatmos_definition;

        // Missing file: the message names the path.
        const std::string missing = "/nonexistent/catchem_mkppchem_units_missing.yaml";
        std::string msg = catch_message([&] { parse_openatmos_definition(missing); });
        require(msg.find("FATAL ERROR:") != std::string::npos && msg.find(missing) != std::string::npos,
                "missing definition is fatal and names the path");

        // Unparseable YAML: the message names the path.
        const std::string malformed = write_temp_yaml("malformed", "name: [unclosed\n  broken: : yaml\n");
        msg = catch_message([&] { parse_openatmos_definition(malformed); });
        require(msg.find("FATAL ERROR:") != std::string::npos && msg.find(malformed) != std::string::npos,
                "unparseable definition is fatal and names the path");

        // A .json path is rejected even though YAML would load it: the JSON
        // files in the MKPP tree are derived migration artifacts.
        msg = catch_message([&] { parse_openatmos_definition("some/derived/mechanism.json"); });
        require(msg.find("FATAL ERROR:") != std::string::npos && msg.find("mechanism.json") != std::string::npos &&
                    msg.find("YAML") != std::string::npos,
                "json definition path is rejected, naming the path and the YAML requirement");

        // Duplicate species name (case-insensitive): names the offender.
        const std::string dup = write_temp_yaml(
            "dup",
            "name: DupMech\nmetadata:\n  mkpp_mechanism_id: gocart\nspecies:\n"
            "  - name: OH\n  - name: so2\n  - name: oh\nreactions:\n"
            "  - type: ARRHENIUS\n    reactants: [{species name: OH}]\n    products: [{species name: SO2}]\n");
        msg = catch_message([&] { parse_openatmos_definition(dup); });
        require(msg.find("FATAL ERROR:") != std::string::npos && msg.find("duplicate") != std::string::npos &&
                    msg.find("oh") != std::string::npos,
                "duplicate species name is fatal, naming the offender");

        // Empty species name: says empty and names the file.
        const std::string empty = write_temp_yaml(
            "empty",
            "name: EmptyMech\nmetadata:\n  mkpp_mechanism_id: gocart\nspecies:\n"
            "  - name: \"\"\nreactions:\n"
            "  - type: ARRHENIUS\n    reactants: [{species name: OH}]\n    products: [{species name: OH}]\n");
        msg = catch_message([&] { parse_openatmos_definition(empty); });
        require(msg.find("FATAL ERROR:") != std::string::npos && msg.find("empty") != std::string::npos &&
                    msg.find(empty) != std::string::npos,
                "empty species name is fatal, naming the file");

        // Missing mechanism id in metadata: names the missing key.
        const std::string noid = write_temp_yaml("noid", "name: NoId\nspecies:\n  - name: OH\nreactions: []\n");
        msg = catch_message([&] { parse_openatmos_definition(noid); });
        require(msg.find("FATAL ERROR:") != std::string::npos && msg.find("mkpp_mechanism_id") != std::string::npos,
                "definition without metadata.mkpp_mechanism_id is fatal, naming the missing key");

        // Reaction referencing an undeclared species: names the species.
        const std::string ghost = write_temp_yaml(
            "ghost",
            "name: GhostMech\nmetadata:\n  mkpp_mechanism_id: gocart\nspecies:\n  - name: OH\nreactions:\n"
            "  - type: ARRHENIUS\n    reactants: [{species name: OH}]\n"
            "    products: [{species name: UNOBTANIUM}]\n");
        msg = catch_message([&] { parse_openatmos_definition(ghost); });
        require(msg.find("FATAL ERROR:") != std::string::npos && msg.find("UNOBTANIUM") != std::string::npos,
                "undeclared reaction participant is fatal, naming the species");

        // Control: a well-formed minimal definition parses with the right facts.
        const std::string good = write_temp_yaml(
            "good",
            "name: GoodMech\nmetadata:\n  mkpp_mechanism_id: gocart\nspecies:\n"
            "  - name: OH\n  - name: NO3\n  - name: NO2\nreactions:\n"
            "  - type: ARRHENIUS\n    reactants: [{species name: OH}, {species name: NO3}]\n"
            "    products: [{species name: NO2}]\n"
            "  - type: PHOTOLYSIS\n    name: jno2\n    reactants: [{species name: NO2}]\n");
        const std::string good_err = catch_message([&] {
            const auto def = parse_openatmos_definition(good);
            if (def.species_order.size() != 3 || def.photolysis_labels.size() != 1 ||
                def.photolysis_labels[0] != "jno2")
                throw std::runtime_error("parsed structural facts are wrong");
        });
        require(good_err.empty(), "well-formed definition parses with correct species/photolysis order" +
                                      (good_err.empty() ? "" : ": " + good_err));
    }

    void test_provenance_failures() {
        using catchem::mkpp::validate_provenance;

        // Mechanism id mismatch: names both identities and the definition path.
        auto def = minimal_definition();
        auto artifact = minimal_artifact();
        artifact.identifier = "saprc11";
        std::string msg = catch_message([&] { validate_provenance(def, artifact); });
        require(msg.find("FATAL ERROR:") != std::string::npos && msg.find("gocart") != std::string::npos &&
                    msg.find("saprc11") != std::string::npos && msg.find("mechanism.yaml") != std::string::npos,
                "provenance id mismatch is fatal, naming both identities and the definition path");

        // Species count mismatch: names both counts.
        def = minimal_definition();
        artifact = minimal_artifact();
        artifact.num_species = 5;
        msg = catch_message([&] { validate_provenance(def, artifact); });
        require(msg.find("FATAL ERROR:") != std::string::npos && msg.find("6") != std::string::npos &&
                    msg.find("5") != std::string::npos && msg.find("species") != std::string::npos,
                "species count mismatch is fatal, naming both counts");

        // Photolysis count mismatch: names the artifact.
        def = minimal_definition();
        artifact = minimal_artifact();
        artifact.num_photolysis = 0;
        msg = catch_message([&] { validate_provenance(def, artifact); });
        require(msg.find("FATAL ERROR:") != std::string::npos && msg.find("photolysis") != std::string::npos &&
                    msg.find("compiled:gocart") != std::string::npos,
                "photolysis count mismatch is fatal, naming the artifact");

        // Species name-set divergence (renamed on one side only): names the
        // divergent entries on both sides.
        def = minimal_definition();
        artifact = minimal_artifact();
        artifact.species_names = {"OH", "NO3", "H2O2", "DMS", "SO2", "SULFATE"};
        msg = catch_message([&] { validate_provenance(def, artifact); });
        require(msg.find("FATAL ERROR:") != std::string::npos && msg.find("SULFATE") != std::string::npos &&
                    msg.find("SO4") != std::string::npos,
                "name-set mismatch is fatal, naming definition-only and artifact-only entries");

        // Cloud-gated definition against an artifact without the cloud-water
        // equilibrium input: fatal, never a silent ungated run.
        def = minimal_definition();
        artifact = minimal_artifact();
        def.has_cloud_gated_reaction = true;
        artifact.num_equilibrium_inputs = 2;
        msg = catch_message([&] { validate_provenance(def, artifact); });
        require(msg.find("FATAL ERROR:") != std::string::npos && msg.find("cloud_liquid_water") != std::string::npos &&
                    msg.find("mechanism.yaml") != std::string::npos && msg.find("compiled:gocart") != std::string::npos,
                "cloud gate declared without artifact support is fatal, naming both sides");

        // Control: a consistent pair validates cleanly.
        const std::string ok_err =
            catch_message([&] { validate_provenance(minimal_definition(), minimal_artifact()); });
        require(ok_err.empty(), "consistent definition/artifact validates cleanly" +
                                    (ok_err.empty() ? "" : ": " + ok_err));
    }

    void test_species_mapping_failures() {
        using catchem::mkpp::build_species_mapping;

        // Unmatched mechanism species: all reported, never dropped or aliased.
        auto def = minimal_definition();
        def.species_order = {"OH", "SO2", "MYSTERY1", "MYSTERY2"};
        std::unordered_map<std::string, int> catchem_index = {{"OH", 0}, {"SO2", 1}, {"NO3", 2}};
        auto mapping = build_species_mapping(def, catchem_index);
        const bool both_reported =
            mapping.unmatched_mechanism.size() == 2 &&
            std::find(mapping.unmatched_mechanism.begin(), mapping.unmatched_mechanism.end(), "MYSTERY1") !=
                mapping.unmatched_mechanism.end() &&
            std::find(mapping.unmatched_mechanism.begin(), mapping.unmatched_mechanism.end(), "MYSTERY2") !=
                mapping.unmatched_mechanism.end();
        require(both_reported, "unmatched mechanism species are all reported in canonical form");
        require(mapping.mapped.size() == 2, "resolvable species still map correctly alongside the failures");
        require(std::find(mapping.unadvanced_catchem.begin(), mapping.unadvanced_catchem.end(), "NO3") !=
                    mapping.unadvanced_catchem.end(),
                "CATChem species the mechanism does not advance are reported (informational side)");

        // Two slots resolving to one CATChem index (a case-insensitive
        // duplicate): reported as a collision, the index claimed once.
        def.species_order = {"OH", "oh2"};
        std::unordered_map<std::string, int> collide = {{"OH", 0}, {"OH2", 0}};
        mapping = build_species_mapping(def, collide);
        require(mapping.unmatched_mechanism.size() == 1 && mapping.mapped.size() == 1,
                "two slots resolving to one CATChem index is a reported collision, not a double map");
    }

    /// Mechanism selection is pure string logic over
    /// the mechanisms compiled into the build -- the shipped default when the
    /// configuration names none, the fatal naming the request and enumerating
    /// what IS available otherwise.
    void test_mechanism_selection() {
        using catchem::mkpp::select_mechanism;

        bool used_default = false;
        const std::vector<std::string> available = {"gocart", "chapman"};

        // No configuration: the shipped default is selected and recorded as defaulted.
        size_t idx = select_mechanism("", available, "gocart", used_default);
        require(idx == 0 && used_default, "an empty request selects the default mechanism and records it as defaulted");

        // Explicit selection of a non-default compiled mechanism resolves by name.
        used_default = true;
        idx = select_mechanism("chapman", available, "gocart", used_default);
        require(idx == 1 && !used_default, "an explicit request selects the named mechanism, not the default");

        // Unknown mechanism: fatal naming the request and every available name.
        std::string msg = catch_message([&] { select_mechanism("saprc99", available, "gocart", used_default); });
        require(msg.find("FATAL ERROR:") != std::string::npos && msg.find("saprc99") != std::string::npos &&
                    msg.find("gocart") != std::string::npos && msg.find("chapman") != std::string::npos,
                "an uncompiled mechanism selection is fatal, naming the request and the available mechanisms");
    }

    /// Scenario 2 through the process: a configuration naming a mechanism that
    /// was not compiled into the build must fail at init, naming the request
    /// and the mechanisms this build does have.
    void test_init_uncompiled_mechanism_is_fatal() {
        auto& registry = catchem::ProcessRegistry::get_instance();
        auto core = std::make_shared<catchem::Core>(2, 2, 26);
        auto state = core->get_state_manager();

        const std::string run_config =
            (std::filesystem::temp_directory_path() / "catchem_mkppchem_units_uncompiled.yml").string();
        {
            std::ofstream writer(run_config);
            writer << "processes:\n"
                   << "  mkppchem:\n"
                   << "    activate: true\n"
                   << "    mechanism: \"saprc99\"\n";
        }
        state->set_configuration_path(run_config);
        if (state->config_manager())
            state->config_manager()->load_from_file(run_config);

        auto proc = registry.create("mkppchem");
        const std::string msg = catch_message([&] { proc->init(state); });
        require(msg.find("FATAL ERROR:") != std::string::npos && msg.find("saprc99") != std::string::npos &&
                    msg.find("gocart") != std::string::npos,
                "init with an uncompiled mechanism is fatal, naming the request and the compiled mechanisms");
        std::remove(run_config.c_str());
    }

    /// Scenario 3: a compiled mechanism paired with a definition it was NOT
    /// generated from must fail at init, naming BOTH definitions (the
    /// configured file and the compiled mechanism's own definition), before
    /// any timestep runs.
    void test_definition_file_mismatch_is_fatal() {
        auto& registry = catchem::ProcessRegistry::get_instance();
        auto core = std::make_shared<catchem::Core>(2, 2, 26);
        auto state = core->get_state_manager();

        const std::string foreign_def =
            (std::filesystem::temp_directory_path() / "catchem_mkppchem_units_foreign_def.yml").string();
        {
            std::ofstream writer(foreign_def);
            writer << "# Impostor definition: a different mechanism's identity.\n"
                   << "name: Impostor_Mechanism\n"
                   << "metadata:\n"
                   << "  mkpp_mechanism_id: saprc99\n"
                   << "  legacy_source_sha256: 0000000000000000000000000000000000000000000000000000000000000000\n"
                   << "species: []\n";
        }
        const std::string run_config =
            (std::filesystem::temp_directory_path() / "catchem_mkppchem_units_mismatch.yml").string();
        {
            std::ofstream writer(run_config);
            writer << "processes:\n"
                   << "  mkppchem:\n"
                   << "    activate: true\n"
                   << "    mechanism: \"gocart\"\n"
                   << "    definition_file: \"" << foreign_def << "\"\n";
        }
        state->set_configuration_path(run_config);
        if (state->config_manager())
            state->config_manager()->load_from_file(run_config);

        auto proc = registry.create("mkppchem");
        const std::string msg = catch_message([&] { proc->init(state); });
        require(msg.find("FATAL ERROR:") != std::string::npos && msg.find("saprc99") != std::string::npos &&
                    msg.find("gocart") != std::string::npos && msg.find(foreign_def) != std::string::npos &&
                    msg.find("mechanism.yaml") != std::string::npos,
                "a definition not generated from the selected mechanism is fatal, naming both definitions");
        std::remove(foreign_def.c_str());
        std::remove(run_config.c_str());
    }

    /// Scenario 4: with no mechanism setting the shipped default is used, and
    /// the initialization log records that the default was selected.
    void test_default_mechanism_is_logged() {
        auto& registry = catchem::ProcessRegistry::get_instance();
        auto core = std::make_shared<catchem::Core>(2, 2, 26);
        auto state = core->get_state_manager();

        // A complete, valid setup so init() reaches its log line: met fields
        // including a host cloud-water field, the mechanism's species, and
        // smoke mode (mkppchem runs alone here, so no photolysis process is
        // scheduled and the photolysis-rate diagnostics would be fatal).
        const int ncells = 4;
        std::vector<double> t(ncells, 290.0), rh(ncells, 0.5), qv(ncells, 0.008), pmid(ncells, 90000.0),
            cldliq(ncells, 1.0e-4);
        state->bind_met_field_3d("T", t.data());
        state->bind_met_field_3d("RH", rh.data());
        state->bind_met_field_3d("QV", qv.data());
        state->bind_met_field_3d("PMID", pmid.data());
        state->bind_met_field_3d("CLDLIQ", cldliq.data());
        state->derive_airden_dry();
        state->load_species_config(std::string(catchem::test::TEST_DIR) + "/Configs/Mkpp/CATChem_mkppchem_species.yml");

        const std::string run_config =
            (std::filesystem::temp_directory_path() / "catchem_mkppchem_units_default_log.yml").string();
        {
            std::ofstream writer(run_config);
            writer << "processes:\n"
                   << "  mkppchem:\n"
                   << "    activate: true\n"
                   << "    photolysis_smoke_mode: true\n";
        }
        state->set_configuration_path(run_config);
        if (state->config_manager())
            state->config_manager()->load_from_file(run_config);

        // Logger INFO output goes to std::clog; capture it around init().
        std::ostringstream captured;
        auto* old_buf = std::clog.rdbuf(captured.rdbuf());
        auto proc = registry.create("mkppchem");
        const std::string msg = catch_message([&] { proc->init(state); });
        std::clog.rdbuf(old_buf);
        require(msg.empty(), "init with no mechanism setting succeeds (default selected)" + (msg.empty() ? "" : ": " + msg));
        require(captured.str().find("mechanism_default=true") != std::string::npos,
                "the init log records that the default mechanism was selected");

        // An explicit selection must NOT be logged as defaulted.
        const std::string explicit_config =
            (std::filesystem::temp_directory_path() / "catchem_mkppchem_units_explicit_log.yml").string();
        {
            std::ofstream writer(explicit_config);
            writer << "processes:\n"
                   << "  mkppchem:\n"
                   << "    activate: true\n"
                   << "    mechanism: \"gocart\"\n"
                   << "    photolysis_smoke_mode: true\n";
        }
        state->set_configuration_path(explicit_config);
        if (state->config_manager())
            state->config_manager()->load_from_file(explicit_config);
        std::ostringstream captured2;
        old_buf = std::clog.rdbuf(captured2.rdbuf());
        auto proc2 = registry.create("mkppchem");
        const std::string msg2 = catch_message([&] { proc2->init(state); });
        std::clog.rdbuf(old_buf);
        require(msg2.empty(), "init with an explicit mechanism setting succeeds" + (msg2.empty() ? "" : ": " + msg2));
        require(captured2.str().find("mechanism_default=false") != std::string::npos,
                "an explicit mechanism selection is logged as not-defaulted");
        std::remove(run_config.c_str());
        std::remove(explicit_config.c_str());
    }

    /// init() against an inventory missing mechanism species (the Default
    /// 22-species list lacks the nitrate chain HNO3/NH3/NH4a/NO3an1-3) must be
    /// fatal, naming the unmatched species and both sides.
    void test_init_unmatched_species_is_fatal() {
        auto& registry = catchem::ProcessRegistry::get_instance();
        auto core = std::make_shared<catchem::Core>(2, 2, 22);
        auto state = core->get_state_manager();
        state->load_species_config(std::string(catchem::test::TEST_DIR) + "/Configs/Default/CATChem_species.yml");
        auto proc = registry.create("mkppchem");
        const std::string msg = catch_message([&] { proc->init(state); });
        require(msg.find("FATAL ERROR:") != std::string::npos &&
                    msg.find("no CATChem counterpart") != std::string::npos && msg.find("HNO3") != std::string::npos,
                "init against an inventory missing mechanism species is fatal, naming the species");
        require(msg.find("mechanism.yaml") != std::string::npos && msg.find("compiled:gocart") != std::string::npos,
                "the species-mapping fatal names both the definition and the compiled artifact");
    }

    /// A cloud-gated mechanism with no cloud-water source (no CLDLIQ, and the
    /// derivation inputs incomplete -- T/RH/PMID bound but no QV) must fail
    /// loudly rather than substitute zero.
    void test_init_cloud_water_unresolvable_is_fatal() {
        auto& registry = catchem::ProcessRegistry::get_instance();
        auto core = std::make_shared<catchem::Core>(2, 2, 26);
        auto state = core->get_state_manager();
        std::vector<double> t(4, 290.0), rh(4, 0.5), pmid(4, 90000.0);
        state->bind_met_field_3d("T", t.data());
        state->bind_met_field_3d("RH", rh.data());
        state->bind_met_field_3d("PMID", pmid.data());
        state->load_species_config(std::string(catchem::test::TEST_DIR) + "/Configs/Mkpp/CATChem_mkppchem_species.yml");
        auto proc = registry.create("mkppchem");
        const std::string msg = catch_message([&] { proc->init(state); });
        require(msg.find("FATAL ERROR:") != std::string::npos && msg.find("cloud") != std::string::npos,
                "init with no resolvable cloud-water source is fatal, naming the cloud-water problem");
    }

    /// The same defect must produce the same fatal message on every attempt.
    /// In a distributed run each rank evaluates the same deterministic checks
    /// against the same configuration, so all ranks fail identically and none
    /// can advance into a collective a failed rank will not reach.
    void test_failure_is_deterministic_and_rank_consistent() {
        auto& registry = catchem::ProcessRegistry::get_instance();
        std::string first;
        bool all_same = true;
        for (int trial = 0; trial < 3; ++trial) {
            auto core = std::make_shared<catchem::Core>(2, 2, 22);
            auto state = core->get_state_manager();
            state->load_species_config(std::string(catchem::test::TEST_DIR) + "/Configs/Default/CATChem_species.yml");
            auto proc = registry.create("mkppchem");
            const std::string msg = catch_message([&] { proc->init(state); });
            if (msg.empty())
                all_same = false;
            else if (first.empty())
                first = msg;
            else if (msg != first)
                all_same = false;
        }
        require(all_same && !first.empty(),
                "repeated identical init failures throw byte-identical messages (every rank fails the same way)");
    }

    /// The runtime error reduction must report the LOWEST failing cell index,
    /// reproducibly, so the diagnostic is identical regardless of thread count
    /// or scheduling order.
    void test_error_reduction_reports_lowest_cell() {
        using catchem::mkpp::StagingBuffers;
        const int num_cells = 40;
        StagingBuffers buffers;
        buffers.allocate(num_cells, 26, 1, 3);
        auto species = buffers.species();
        auto host = Kokkos::create_mirror_view_and_copy(Kokkos::HostSpace{}, species);
        for (int c = 0; c < num_cells; ++c)
            for (int s = 0; s < 26; ++s)
                host(c, s) = 1.0e-12;
        // Corrupt three cells; the reduction must name the lowest (7).
        for (int c : {23, 7, 31})
            host(c, 3) = -1.0;
        Kokkos::deep_copy(species, host);

        bool stable = true;
        for (int trial = 0; trial < 3; ++trial) {
            catchem::mkpp::clear_errors(buffers);
            catchem::mkpp::validate_pre_solve(buffers);
            if (catchem::mkpp::reduce_first_error(buffers) != 7)
                stable = false;
        }
        require(stable, "error reduction reports the lowest failing cell (7), reproducibly");

        // Control: a clean grid reduces to the no-error sentinel.
        for (int c = 0; c < num_cells; ++c)
            host(c, 3) = 1.0e-12;
        Kokkos::deep_copy(species, host);
        catchem::mkpp::clear_errors(buffers);
        catchem::mkpp::validate_pre_solve(buffers);
        require(catchem::mkpp::reduce_first_error(buffers) == catchem::mkpp::kNoError,
                "clean cells reduce to the no-error sentinel");
    }

    /// A fatal raised while a process is being added through the C boundary
    /// must be reported as a boundary status, not propagated as a C++
    /// exception: the Fortran callers have no handler, so an escaping throw
    /// terminates that rank abruptly while its peers wait in a collective.
    void test_fatal_at_c_boundary_is_reported_not_thrown() {
        void* core = nullptr;
        const int created = catchem_core_create_checked(2, 2, 26, &core);
        require(created == CATCHEM_SUCCESS && core != nullptr, "C boundary created a core for the abort-path check");
        if (created != CATCHEM_SUCCESS || !core)
            return;
        // No species inventory is loaded, so mkppchem init() is a guaranteed
        // fatal (every mechanism species is unmatched).
        bool threw = false;
        int status = CATCHEM_SUCCESS;
        try {
            status = catchem_core_add_process_by_name_checked(core, "mkppchem");
        } catch (const std::exception& e) {
            threw = true;
            std::cerr << "  (the boundary threw instead of reporting: " << e.what() << ")" << std::endl;
        } catch (...) {
            threw = true;
        }
        require(!threw, "add_process_by_name reports a fatal through the boundary, not a C++ exception");
        if (!threw) {
            require(status != CATCHEM_SUCCESS, "the checked boundary returns a failure status for a fatal init");
            char buffer[512] = {0};
            const int error_status = catchem_get_last_error(buffer, static_cast<int>(sizeof(buffer)));
            const std::string message(buffer);
            require(error_status != CATCHEM_SUCCESS && message.find("FATAL ERROR:") != std::string::npos,
                    "the boundary records the fatal message for the caller to retrieve");
        }
        catchem_core_destroy_checked(core);
    }

    /// A process order that activates mkppchem together with a GOCART chemistry
    /// process advancing the same species must be fatal: running both silently
    /// double-applies the chemistry. Checked in init() even when the
    /// conflicting process is only named in the schedule, not constructed here.
    void test_process_order_conflict_is_fatal() {
        auto& registry = catchem::ProcessRegistry::get_instance();
        const std::string species_file =
            std::string(catchem::test::TEST_DIR) + "/Configs/Mkpp/CATChem_mkppchem_species.yml";

        for (const char* conflicting : {"so4chem", "carbchem"}) {
            auto core = std::make_shared<catchem::Core>(2, 2, 26);
            auto state = core->get_state_manager();

            // A complete met set including a host cloud-water field, so the
            // only remaining defect is the process-order conflict itself.
            const int ncol = 2;
            const int nlev = 2;
            std::vector<double> t(ncol * nlev, 290.0), rh(ncol * nlev, 0.5), qv(ncol * nlev, 0.008),
                pmid(ncol * nlev, 90000.0), cldliq(ncol * nlev, 1.0e-4);
            state->bind_met_field_3d("T", t.data());
            state->bind_met_field_3d("RH", rh.data());
            state->bind_met_field_3d("QV", qv.data());
            state->bind_met_field_3d("PMID", pmid.data());
            state->bind_met_field_3d("CLDLIQ", cldliq.data());
            state->derive_airden_dry();
            state->load_species_config(species_file);

            const std::string config_path = (std::filesystem::temp_directory_path() /
                                             ("catchem_mkppchem_units_conflict_" + std::string(conflicting) + ".yml"))
                                                .string();
            {
                std::ofstream writer(config_path);
                writer << "processes:\n"
                       << "  mkppchem:\n"
                       << "    activate: true\n"
                       << "    mechanism: \"gocart\"\n"
                       << "run_phases:\n"
                       << "  chem:\n"
                       << "    processes:\n"
                       << "      - mkppchem\n"
                       << "      - " << conflicting << "\n";
            }
            state->set_configuration_path(config_path);
            if (state->config_manager())
                state->config_manager()->load_from_file(config_path);

            auto proc = registry.create("mkppchem");
            const std::string msg = catch_message([&] { proc->init(state); });
            const bool names_both =
                msg.find("mkppchem") != std::string::npos && msg.find(conflicting) != std::string::npos;
            require(msg.find("FATAL ERROR:") != std::string::npos && names_both,
                    std::string("mkppchem + ") + conflicting + " in one process order is fatal, naming both processes");
            std::remove(config_path.c_str());
        }
    }

} // namespace

int main(int argc, char* argv[]) {
    std::cout << "RUNNING: mkppchem unit tests (registration, contract, validator, units, failure paths)" << std::endl;
    // The Core constructor echoes parsed YAML to stdout; these tests write
    // throwaway configs, so silence it to keep the log readable.
    catchem::ConfigManager::echo_config_to_stdout = false;
    Kokkos::initialize(argc, argv);
    {
        test_registration();
        test_contract_device_intent();
        test_settings_validator();
        test_initial_step_seed_parsing();
        test_conversion_reversibility();
        test_dry_air_molar_density();
        test_definition_parse_failures();
        test_provenance_failures();
        test_mechanism_selection();
        test_species_mapping_failures();
        test_init_uncompiled_mechanism_is_fatal();
        test_definition_file_mismatch_is_fatal();
        test_default_mechanism_is_logged();
        test_init_unmatched_species_is_fatal();
        test_init_cloud_water_unresolvable_is_fatal();
        test_failure_is_deterministic_and_rank_consistent();
        test_error_reduction_reports_lowest_cell();
        test_fatal_at_c_boundary_is_reported_not_thrown();
        test_process_order_conflict_is_fatal();
    }
    Kokkos::finalize();
    if (g_failures == 0) {
        std::cout << "ALL MKPPCHEM UNIT TESTS PASSED" << std::endl;
        return 0;
    }
    std::cerr << g_failures << " MKPPCHEM UNIT TEST(S) FAILED" << std::endl;
    return 1;
}

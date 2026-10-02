// On-demand aerosol diagnostics API: contract scenarios.
//
// A standalone executable because the compute path owns a reference-counted
// Kokkos runtime: the service brings it up lazily and tears it down with the
// Core, and a suite whose main() already drives Kokkos::initialize/finalize
// cannot share that lifecycle.  A single sticky environment reference is held
// for the whole run so per-service init/finalize pairs only bump the count
// 1 <-> 2 and a premature finalize is impossible (the settling gate-ON test
// established this pattern).
//
// The catalogue endpoints work without a configured utility: the
// registry is static and answers whenever the aerosol library is built.  The
// compute scenarios configure a Core from an in-memory YAML with an
// on_demand section and drive the service against a synthetic bound state.
//
// With the aerosol utility NOT built (gate-OFF build), the same source
// compiles without the bridge and asserts the build remedy instead: every
// endpoint answers CATCHEM_QUANTITY_NOT_AVAILABLE naming the reconfigure
// flag, so a host can detect the missing feature from the API alone.

#include "catchem_api.hpp"
#include "catchem_ondemand_service.hpp"
#include "catchem_state_manager.hpp"
#include "catchem_test_config.hpp"

#include <cmath>
#include <cstdio>
#include <cstring>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>

#ifndef CATCHEM_ONDEMAND_GATE_OFF
#include "catchem_exaero_bridge.hpp"
#endif

namespace {

int failures = 0;

void check(bool condition, const std::string& label) {
    std::cout << (condition ? "  PASS: " : "  FAIL: ") << label << '\n';
    if (!condition)
        ++failures;
}

std::string last_error() {
    char buffer[512];
    buffer[0] = '\0';
    catchem_get_last_error(buffer, static_cast<int>(sizeof(buffer)));
    return std::string(buffer);
}

#ifndef CATCHEM_ONDEMAND_GATE_OFF

// Synthetic bound state for the compute scenarios: a 2 x 3 x 2 column with
// two host aerosols (fine_aero -> FINE at 0.8 um, coarse_aero -> COARSE at
// 2.0 um).  Masses are chosen so the modal diameters sit strictly inside or
// outside every PM cut (pm1 = 1.0 um, pm2_5 = 2.5 um, pm10 = 10 um): FINE is
// in all three, COARSE is in pm2_5 and pm10 but out of pm1.  A 2.0 um
// particle is below the 2.5 um cut, so the all-or-nothing modal cut (the
// on-demand definition) is exercised unambiguously rather than a straddling
// size bin.  Fields are column-major (cell fastest) to match the layout the
// bridge hands the library.
struct Us1State {
    static constexpr int kCells = 2;
    static constexpr int kLevels = 3;
    static constexpr int kSpecies = 2;
    static constexpr int kPoints = kCells * kLevels;

    catchem::StateManager state;
    std::vector<double> temperature;
    std::vector<double> air_density;
    std::vector<double> p_mid;
    std::vector<double> relative_humidity;
    std::vector<double> thickness;
    std::vector<double> concentration;

    Us1State() :
        state(kCells, kLevels, kSpecies),
        temperature(kPoints),
        air_density(kPoints),
        p_mid(kPoints),
        relative_humidity(kPoints),
        thickness(kPoints),
        concentration(static_cast<std::size_t>(kPoints) * kSpecies, 0.0) {
        for (int level = 0; level < kLevels; ++level) {
            for (int cell = 0; cell < kCells; ++cell) {
                const int point = cell + level * kCells;
                temperature[point] = 290.0;
                air_density[point] = 1.2;
                p_mid[point] = 101325.0;
                relative_humidity[point] = 0.1;
                thickness[point] = 100.0;
            }
        }
        state.bind_met_field_3d("T", temperature.data());
        state.bind_met_field_3d("AIRDEN", air_density.data());
        state.bind_met_field_3d("PMID", p_mid.data());
        state.bind_met_field_3d("RH", relative_humidity.data());
        state.bind_met_field_3d("BXHEIGHT", thickness.data());
        state.bind_unified_chemistry(concentration.data());
    }

    // µg/kg mass of a host species at (cell, level); index order matches the
    // unified chemistry layout (species stride over the point index).
    void set_mass(int species, int cell, int level, double ug_per_kg) {
        concentration[static_cast<std::size_t>(species) * kPoints + cell + level * kCells] = ug_per_kg;
    }

    double expected_kg_per_m3(int species, int cell, int level) const {
        const double ug_per_kg =
            concentration[static_cast<std::size_t>(species) * kPoints + cell + level * kCells];
        return ug_per_kg * 1.0e-9 * air_density[cell + level * kCells];
    }
};

#endif  // CATCHEM_ONDEMAND_GATE_OFF

}  // namespace

int main() {
#ifdef CATCHEM_ONDEMAND_GATE_OFF
    std::cout << "== on-demand API: gate-OFF abort (build remedy) ==\n";
    void* core = nullptr;
    if (catchem_core_create_checked(2, 2, 1, &core) != CATCHEM_SUCCESS) {
        std::cout << "  FAIL: core creation for the gate-OFF probe\n";
        return 1;
    }
    int count = -1;
    const int count_status = catchem_ondemand_quantity_count_checked(core, &count);
    check(count_status == CATCHEM_QUANTITY_NOT_AVAILABLE, "count endpoint answers QUANTITY_NOT_AVAILABLE (18)");
    check(count == 0, "count is 0 when the utility is not built");
    check(last_error().find("CATCHEM_BUILD_EXAERO=ON") != std::string::npos,
          "the last-error message names the reconfigure remedy");
    char name[64];
    check(catchem_ondemand_quantity_name_at_checked(core, 0, name, 64) == CATCHEM_QUANTITY_NOT_AVAILABLE,
          "name enumeration answers QUANTITY_NOT_AVAILABLE");
    char units[64];
    check(catchem_ondemand_quantity_units_checked(core, "pm2_5", units, 64) == CATCHEM_QUANTITY_NOT_AVAILABLE,
          "units lookup answers QUANTITY_NOT_AVAILABLE");
    const char* pm_names[1] = {"pm2_5"};
    double pm_values[4] = {0.0, 0.0, 0.0, 0.0};
    check(catchem_ondemand_get_pm_checked(core, pm_names, 1, pm_values, nullptr, 4, 0) == CATCHEM_QUANTITY_NOT_AVAILABLE,
          "the bulk PM endpoint answers QUANTITY_NOT_AVAILABLE");
    check(last_error().find("CATCHEM_BUILD_EXAERO=ON") != std::string::npos,
          "the PM failure message names the reconfigure remedy");
    const double gate_lambda = 5.5e-7;
    check(catchem_ondemand_get_optics_checked(core, pm_names, 1, &gate_lambda, 1, pm_values, nullptr, 4, 0) ==
              CATCHEM_QUANTITY_NOT_AVAILABLE,
          "the optics endpoint answers QUANTITY_NOT_AVAILABLE");
    check(last_error().find("CATCHEM_BUILD_EXAERO=ON") != std::string::npos,
          "the optics failure message names the reconfigure remedy");
    check(catchem_ondemand_get_quantity_checked(core, "extinction", &gate_lambda, 1, nullptr, 0, pm_values, nullptr, 4,
                                                0) == CATCHEM_QUANTITY_NOT_AVAILABLE,
          "the general quantity endpoint answers QUANTITY_NOT_AVAILABLE");
    check(last_error().find("CATCHEM_BUILD_EXAERO=ON") != std::string::npos,
          "the general endpoint failure message names the reconfigure remedy");
    catchem_core_destroy(core);
#else
    std::cout << "== on-demand API: quantity catalogue ==\n";
    // Sticky environment reference: the service's own bring-up/teardown then
    // only ever bumps the count 1 <-> 2 across scenarios.
    catchem::exaero_util::ExaeroPackageHandle::init_environment();

    void* core = nullptr;
    if (catchem_core_create_checked(2, 2, 1, &core) != CATCHEM_SUCCESS) {
        std::cout << "  FAIL: core creation\n";
        return 1;
    }

    int count = -1;
    check(catchem_ondemand_quantity_count_checked(core, &count) == CATCHEM_SUCCESS, "count endpoint succeeds");
    check(count == 24, "the catalogue holds 24 quantities (12 diagnostic + 12 optics-family)");

    // Walk the catalogue: every index names a quantity, every name reports
    // units, and the index past the end fails loudly by name.
    bool names_ok = true;
    bool units_ok = true;
    int enumerated = 0;
    for (int i = 0; i < count; ++i) {
        char name[64];
        if (catchem_ondemand_quantity_name_at_checked(core, i, name, static_cast<int>(sizeof(name))) !=
            CATCHEM_SUCCESS) {
            names_ok = false;
            break;
        }
        char units[64];
        if (catchem_ondemand_quantity_units_checked(core, name, units, static_cast<int>(sizeof(units))) !=
            CATCHEM_SUCCESS) {
            units_ok = false;
            break;
        }
        ++enumerated;
    }
    check(names_ok && enumerated == count, "every catalogue index names a quantity");
    check(units_ok, "every catalogue name reports canonical units");

    char name[64];
    check(catchem_ondemand_quantity_name_at_checked(core, count, name, 64) == CATCHEM_INVALID_INDEX,
          "an index past the catalogue fails with INVALID_INDEX");
    check(last_error().find("index") != std::string::npos, "the failure message names the offending input");

    char units[64];
    check(catchem_ondemand_quantity_units_checked(core, "not_a_quantity", units, 64) == CATCHEM_INVALID_INDEX,
          "an unknown quantity name fails with INVALID_INDEX");
    check(last_error().find("not_a_quantity") != std::string::npos, "the failure message names the quantity");

    check(catchem_ondemand_quantity_count_checked(nullptr, &count) != CATCHEM_SUCCESS,
          "a null handle is rejected at the boundary");
    check(catchem_ondemand_quantity_count_checked(core, nullptr) == CATCHEM_NULL_ARGUMENT,
          "a null output is rejected");

    // The utility is built but this Core carries no on_demand configuration
    // (a grid-only Core): the PM endpoint answers the configuration remedy
    // rather than computing against an unconfigured service.
    const char* probe_names[1] = {"pm2_5"};
    double probe_values[2 * 2 * 1] = {0.0, 0.0, 0.0, 0.0};
    check(catchem_ondemand_get_pm_checked(core, probe_names, 1, probe_values, nullptr, 4, 0) ==
              CATCHEM_QUANTITY_NOT_AVAILABLE,
          "a Core without on_demand configuration answers QUANTITY_NOT_AVAILABLE");
    check(last_error().find("diagnostics/on_demand") != std::string::npos,
          "the unconfigured message names the configuration node");
    catchem_core_destroy(core);

    // --- Bulk particulate matter -------------------------------------------
    // The service reads PM through the aerosol library's modal dry-diameter
    // cut: a modal species contributes its full mass to a cut when its
    // configured dry particle diameter is at or below the cut, otherwise no
    // part of it counts.  (A host's own per-size-bin products may instead
    // weight a bin whose range straddles a cut by the fraction of its
    // geometric range below the cut; that is a different definition and is
    // deliberately not what this endpoint answers.)
    std::cout << "== on-demand API: bulk PM ==\n";
    constexpr int kAvailable = 0;
    constexpr int kNotInSource = 4;

    Us1State fixture;
    // Distinct masses per point so an aggregation mix-up cannot hide behind a
    // symmetric value: fine_aero carries (cell + 1) * (level + 1) * 10 ug/kg,
    // coarse_aero carries (cell + 1) * (level + 1) * 20 ug/kg.
    for (int level = 0; level < Us1State::kLevels; ++level) {
        for (int cell = 0; cell < Us1State::kCells; ++cell) {
            const double base = (cell + 1) * (level + 1) * 10.0;
            fixture.set_mass(0, cell, level, base);
            fixture.set_mass(1, cell, level, 2.0 * base);
        }
    }

    catchem::OnDemandService service;
    std::string error;
    const std::string modal_config = std::string(catchem::test::TEST_DIR) + "/Configs/exaero/ondemand_two_species.yml";
    const std::string host_config = std::string(catchem::test::TEST_DIR) + "/Configs/exaero/ondemand_species.yml";
    fixture.state.load_species_config(host_config);
    check(service.configure(modal_config, true) == CATCHEM_SUCCESS, "the service configures from a modal YAML");

    const std::size_t points = Us1State::kPoints;
    std::vector<double> values(points, -1.0);
    std::vector<int> statuses(points, -1);

    auto read = [&](const std::string& species, const std::string& name) {
        std::fill(values.begin(), values.end(), -1.0);
        std::fill(statuses.begin(), statuses.end(), -1);
        error.clear();
        return service.read_pm(species, name, fixture.state, values.data(), statuses.data(), values.size(), error);
    };

    // A point's expected bulk mass: the sum over modal species whose diameter
    // lies at or below the cut, in kg/m^3.
    auto expected_bulk = [&](const std::vector<int>& species_used, int cell, int level) {
        double total = 0.0;
        for (const int species : species_used)
            total += fixture.expected_kg_per_m3(species, cell, level);
        return total;
    };

    bool pm_ok = true;
    if (read("", "pm2_5") != CATCHEM_SUCCESS) {
        pm_ok = false;
        std::cout << "  (pm2_5 request failed: " << error << ")\n";
    }
    for (std::size_t p = 0; p < points && pm_ok; ++p) {
        const int cell = static_cast<int>(p % Us1State::kCells);
        const int level = static_cast<int>(p / Us1State::kCells);
        if (statuses[p] != kAvailable || values[p] != expected_bulk({0, 1}, cell, level)) {
            pm_ok = false;
            break;
        }
    }
    check(pm_ok, "pm2_5 equals the fine plus coarse mass at every point");

    bool pm10_ok = true;
    if (read("", "pm10") != CATCHEM_SUCCESS) {
        pm10_ok = false;
        std::cout << "  (pm10 request failed: " << error << ")\n";
    }
    for (std::size_t p = 0; p < points && pm10_ok; ++p) {
        const int cell = static_cast<int>(p % Us1State::kCells);
        const int level = static_cast<int>(p / Us1State::kCells);
        if (statuses[p] != kAvailable || values[p] != expected_bulk({0, 1}, cell, level)) {
            pm10_ok = false;
            break;
        }
    }
    check(pm10_ok, "pm10 equals the fine plus coarse mass at every point");

    // pm1 excludes the 2.0 um modal species and keeps the 0.8 um one.  Before
    // the library grows a PM1 diagnostic this reads an out-of-range slot and
    // the service reports the absence rather than a fabricated zero.
    bool pm1_ok = true;
    const int pm1_status = read("", "pm1");
    if (pm1_status != CATCHEM_SUCCESS) {
        pm1_ok = false;
        std::cout << "  (pm1 request failed: " << error << ")\n";
    }
    for (std::size_t p = 0; p < points && pm1_ok; ++p) {
        const int cell = static_cast<int>(p % Us1State::kCells);
        const int level = static_cast<int>(p / Us1State::kCells);
        if (statuses[p] != kAvailable || values[p] != expected_bulk({0}, cell, level)) {
            pm1_ok = false;
            break;
        }
    }
    check(pm1_ok, "pm1 equals the fine-mode mass only, at every point");

    // A column carrying only the coarse mode answers a real zero for pm1 --
    // an ok-flagged zero, never a missing-value report.
    bool zero_ok = true;
    for (int level = 0; level < Us1State::kLevels; ++level)
        fixture.set_mass(0, 0, level, 0.0);
    if (read("", "pm1") != CATCHEM_SUCCESS) {
        zero_ok = false;
    }
    for (int level = 0; level < Us1State::kLevels && zero_ok; ++level) {
        if (statuses[level * Us1State::kCells] != kAvailable || values[level * Us1State::kCells] != 0.0)
            zero_ok = false;
    }
    check(zero_ok, "a column above every PM1 cut answers an ok-flagged zero, not a missing value");
    for (int level = 0; level < Us1State::kLevels; ++level)
        fixture.set_mass(0, 0, level, (0 + 1) * (level + 1) * 10.0);

    // Per-species attribution: a named filter answers that species' share.
    bool fine_ok = true;
    if (read("fine_aero", "pm2_5") != CATCHEM_SUCCESS) {
        fine_ok = false;
    }
    for (std::size_t p = 0; p < points && fine_ok; ++p) {
        const int cell = static_cast<int>(p % Us1State::kCells);
        const int level = static_cast<int>(p / Us1State::kCells);
        if (statuses[p] != kAvailable || values[p] != expected_bulk({0}, cell, level))
            fine_ok = false;
    }
    check(fine_ok, "a fine-mode species filter answers the fine-mode mass alone");
    bool coarse_pm1_ok = true;
    if (read("COARSE", "pm1") != CATCHEM_SUCCESS) {
        coarse_pm1_ok = false;
    }
    for (std::size_t p = 0; p < points && coarse_pm1_ok; ++p) {
        if (statuses[p] != kAvailable || values[p] != 0.0)
            coarse_pm1_ok = false;
    }
    check(coarse_pm1_ok, "a coarse-mode species filter answers zero for pm1 (diameter above the cut)");

    // A name the catalogue does not hold fails loudly and names the input.
    error.clear();
    const int unknown_status = read("", "pm100");
    check(unknown_status == CATCHEM_INVALID_INDEX, "an unknown PM name fails with INVALID_INDEX");
    check(error.find("pm100") != std::string::npos, "the failure message names the unknown quantity");
    error.clear();
    const int ghost_status = read("no_such_species", "pm2_5");
    check(ghost_status == CATCHEM_INVALID_INDEX, "a species absent from state and config fails with INVALID_INDEX");
    check(error.find("no_such_species") != std::string::npos, "the failure message names the unknown species");

    // --- Bulk PM through the flat C endpoint --------------------------------
    // Drive the same PM read through a config-enabled Core rather than the
    // service directly, so the endpoint's argument validation, name-major
    // layout, and per-point statuses are exercised end to end.  A temporary
    // configuration turns the utility on and points it at the modal fixture;
    // the host species inventory comes from the same YAML the service tests
    // used.
    std::cout << "== on-demand API: bulk PM endpoint ==\n";
    const std::string config_text =
        "simulation:\n"
        "  name: ondemand_c2\n"
        "  species_filename: " +
        host_config +
        "\n"
        "grid:\n"
        "  number_of_levels: 3\n"
        "diagnostics:\n"
        "  on_demand:\n"
        "    enabled: true\n"
        "    exaero_config: " +
        modal_config + "\n";
    const std::string config_path = "ondemand_c2_config.yml";
    {
        std::ofstream out(config_path);
        out << config_text;
    }
    void* api_core = nullptr;
    const int create_status =
        catchem_core_create_from_config_with_grid_checked(config_path.c_str(), Us1State::kCells, Us1State::kLevels,
                                                          &api_core);
    check(create_status == CATCHEM_SUCCESS && api_core != nullptr, "a configured Core builds from the YAML");

    if (api_core != nullptr) {
        void* state_ptr = nullptr;
        check(catchem_core_get_state_manager_checked(api_core, &state_ptr) == CATCHEM_SUCCESS && state_ptr != nullptr,
              "the Core exposes its state manager");

        Us1State fields;  // reuse the fixture's storage + mass helpers
        for (int level = 0; level < Us1State::kLevels; ++level) {
            for (int cell = 0; cell < Us1State::kCells; ++cell) {
                const double base = (cell + 1) * (level + 1) * 10.0;
                fields.set_mass(0, cell, level, base);
                fields.set_mass(1, cell, level, 2.0 * base);
            }
        }
        bool bound = true;
        bound &= catchem_state_bind_met_3d_checked(state_ptr, "T", fields.temperature.data(), Us1State::kCells,
                                                   Us1State::kLevels, 1) == CATCHEM_SUCCESS;
        bound &= catchem_state_bind_met_3d_checked(state_ptr, "AIRDEN", fields.air_density.data(), Us1State::kCells,
                                                   Us1State::kLevels, 1) == CATCHEM_SUCCESS;
        bound &= catchem_state_bind_met_3d_checked(state_ptr, "PMID", fields.p_mid.data(), Us1State::kCells,
                                                   Us1State::kLevels, 1) == CATCHEM_SUCCESS;
        bound &= catchem_state_bind_met_3d_checked(state_ptr, "RH", fields.relative_humidity.data(), Us1State::kCells,
                                                   Us1State::kLevels, 1) == CATCHEM_SUCCESS;
        bound &= catchem_state_bind_met_3d_checked(state_ptr, "BXHEIGHT", fields.thickness.data(), Us1State::kCells,
                                                   Us1State::kLevels, 1) == CATCHEM_SUCCESS;
        bound &= catchem_state_bind_unified_chemistry_checked(state_ptr, fields.concentration.data(),
                                                              Us1State::kCells, Us1State::kLevels,
                                                              Us1State::kSpecies) == CATCHEM_SUCCESS;
        check(bound, "the host met + chemistry fields bind through the C surface");

        const char* names[3] = {"pm1", "pm2_5", "pm10"};
        const std::size_t block = Us1State::kPoints;
        std::vector<double> out_values(3 * block, -1.0);
        std::vector<int> out_status(3 * block, -1);
        const int pm_status = catchem_ondemand_get_pm_checked(api_core, names, 3, out_values.data(),
                                                              out_status.data(), static_cast<int>(out_values.size()),
                                                              static_cast<int>(out_status.size()));
        check(pm_status == CATCHEM_SUCCESS, "the bulk PM endpoint answers three names in one call");

        auto block_matches = [&](int name_index, const std::vector<int>& species_used) {
            for (std::size_t p = 0; p < block; ++p) {
                const int cell = static_cast<int>(p % Us1State::kCells);
                const int level = static_cast<int>(p / Us1State::kCells);
                double want = 0.0;
                for (const int s : species_used)
                    want += fields.expected_kg_per_m3(s, cell, level);
                if (out_status[static_cast<std::size_t>(name_index) * block + p] != kAvailable ||
                    out_values[static_cast<std::size_t>(name_index) * block + p] != want)
                    return false;
            }
            return true;
        };
        check(block_matches(1, {0, 1}), "the pm2_5 block equals fine plus coarse at every point");
        check(block_matches(2, {0, 1}), "the pm10 block equals fine plus coarse at every point");
        check(block_matches(0, {0}), "the pm1 block equals the fine-mode mass at every point");

        // A buffer that cannot hold all three blocks is rejected before the
        // values are trusted, naming the required extent.
        std::vector<double> small(block, 0.0);  // one block only
        const int small_status = catchem_ondemand_get_pm_checked(api_core, names, 3, small.data(), nullptr,
                                                                 static_cast<int>(small.size()), 0);
        check(small_status == CATCHEM_EXTENT_MISMATCH, "an undersized values buffer fails with EXTENT_MISMATCH");

        // An unknown name in the list fails the whole request by name.
        const char* bad_names[2] = {"pm2_5", "pm999"};
        std::vector<double> ok_values(2 * block, 0.0);
        const int bad_status = catchem_ondemand_get_pm_checked(api_core, bad_names, 2, ok_values.data(), nullptr,
                                                               static_cast<int>(ok_values.size()), 0);
        check(bad_status == CATCHEM_INVALID_INDEX, "an unknown name in the list fails with INVALID_INDEX");
        check(last_error().find("pm999") != std::string::npos, "the failure message names the unknown quantity");

        catchem_core_destroy(api_core);
    }
    std::remove(config_path.c_str());

    // --- Optical quantities at host wavelengths ------------------------------
    // The service answers the library's optics slab at the wavelengths the
    // host asks for, inside the configured band range only: an out-of-band
    // request is rejected with the offending wavelength and the valid range
    // named, never silently clamped or answered by an analytical fallback.
    std::cout << "== on-demand API: optics ==\n";
    const double lambda = 5.5e-7;
    std::vector<double> ext(points, -1.0), sca(points, -1.0), albedo(points, -1.0);
    std::vector<double> aod(Us1State::kCells, -1.0);
    std::vector<int> ext_status(points, -1), sca_status(points, -1), albedo_status(points, -1),
        aod_status(Us1State::kCells, -1);

    error.clear();
    int rc = service.read_optics("extinction", &lambda, 1, fixture.state, ext.data(), ext_status.data(), ext.size(),
                                 error);
    check(rc == CATCHEM_SUCCESS, "extinction at 550 nm succeeds");
    rc = service.read_optics("scattering", &lambda, 1, fixture.state, sca.data(), sca_status.data(), sca.size(),
                             error);
    check(rc == CATCHEM_SUCCESS, "scattering at 550 nm succeeds");
    rc = service.read_optics("aod", &lambda, 1, fixture.state, aod.data(), aod_status.data(), aod.size(), error);
    check(rc == CATCHEM_SUCCESS, "the column AOD at 550 nm succeeds");

    bool optics_positive = (rc == CATCHEM_SUCCESS);
    for (std::size_t p = 0; p < points; ++p) {
        optics_positive &= (ext_status[p] == kAvailable && ext[p] > 0.0);
        optics_positive &= (sca_status[p] == kAvailable && sca[p] > 0.0 && sca[p] < ext[p]);
    }
    check(optics_positive, "extinction and scattering are positive with scattering below extinction everywhere");

    // The AOT is the layer-thickness-weighted column integral of extinction
    // from the same call -- the internal-consistency invariant, not a second
    // independent model of the atmosphere.
    bool aod_ok = true;
    for (int cell = 0; cell < Us1State::kCells; ++cell) {
        double want = 0.0;
        for (int level = 0; level < Us1State::kLevels; ++level)
            want += ext[cell + level * Us1State::kCells] * 100.0;  // BXHEIGHT in the fixture
        const double got = aod[cell];
        aod_ok &= (aod_status[cell] == kAvailable && std::abs(got - want) <= 1e-9 * want);
    }
    check(aod_ok, "the column AOD equals the thickness-weighted sum of per-level extinction");

    // Single scattering albedo is derived, not a library slot: scattering
    // over extinction wherever extinction is positive.
    error.clear();
    rc = service.read_optics("single_scattering_albedo", &lambda, 1, fixture.state, albedo.data(),
                             albedo_status.data(), albedo.size(), error);
    check(rc == CATCHEM_SUCCESS, "the derived single scattering albedo succeeds");
    bool albedo_ok = true;
    for (std::size_t p = 0; p < points; ++p) {
        albedo_ok &= (albedo_status[p] == kAvailable && std::abs(albedo[p] - sca[p] / ext[p]) <= 1e-12);
    }
    check(albedo_ok, "single scattering albedo equals scattering divided by extinction at every point");

    // A point with no aerosol mass has no albedo: the request reports the
    // absence with a zero value, never an ok-flagged divide-by-nothing.
    for (int species = 0; species < Us1State::kSpecies; ++species)
        fixture.set_mass(species, 0, 0, 0.0);
    rc = service.read_optics("single_scattering_albedo", &lambda, 1, fixture.state, albedo.data(),
                             albedo_status.data(), albedo.size(), error);
    check(rc == CATCHEM_SUCCESS && albedo_status[0] == kNotInSource && albedo[0] == 0.0,
          "a mass-free point reports the albedo as not-in-source with a zero value");
    for (int species = 0; species < Us1State::kSpecies; ++species) {
        const double base = (0 + 1) * (0 + 1) * 10.0 * (species == 1 ? 2.0 : 1.0);
        fixture.set_mass(species, 0, 0, base);
    }

    // An out-of-band wavelength is rejected, and the rejection names both
    // the wavelength and the valid band range in a form a host can act on.
    const double out_of_band = 1.0;  // metres -- not a radiation-band wavelength
    error.clear();
    std::vector<double> rejected(points, 0.0);
    const int reject_status = service.read_optics("extinction", &out_of_band, 1, fixture.state, rejected.data(),
                                                  nullptr, rejected.size(), error);
    check(reject_status == CATCHEM_QUANTITY_NOT_AVAILABLE, "a 1 m wavelength is rejected, never clamped");
    check(error.find("1 m") != std::string::npos, "the rejection names the offending wavelength");
    check(error.find("2e-07") != std::string::npos && error.find("4e-06") != std::string::npos,
          "the rejection names the valid band range at micrometre scale");

    // --- Optical quantities through the flat C endpoint ---------------------
    // Drive the optics read through a config-enabled Core so the endpoint's
    // name-major layout (coefficient blocks of bands x cells x levels, AOT
    // blocks of bands x cells), extent checking, and out-of-band rejection
    // are exercised end to end, not just the service method.
    std::cout << "== on-demand API: optics endpoint ==\n";
    {
        const std::string config_text =
            "simulation:\n"
            "  name: ondemand_optics\n"
            "  species_filename: " +
            host_config +
            "\n"
            "grid:\n"
            "  number_of_levels: 3\n"
            "diagnostics:\n"
            "  on_demand:\n"
            "    enabled: true\n"
            "    exaero_config: " +
            modal_config + "\n";
        const std::string config_path = "ondemand_optics_config.yml";
        {
            std::ofstream out(config_path);
            out << config_text;
        }
        void* optics_core = nullptr;
        const int create_status =
            catchem_core_create_from_config_with_grid_checked(config_path.c_str(), Us1State::kCells,
                                                              Us1State::kLevels, &optics_core);
        check(create_status == CATCHEM_SUCCESS && optics_core != nullptr,
              "a configured Core builds for the optics endpoint");

        if (optics_core != nullptr) {
            void* state_ptr = nullptr;
            check(catchem_core_get_state_manager_checked(optics_core, &state_ptr) == CATCHEM_SUCCESS &&
                      state_ptr != nullptr,
                  "the optics Core exposes its state manager");

            Us1State fields;
            for (int level = 0; level < Us1State::kLevels; ++level) {
                for (int cell = 0; cell < Us1State::kCells; ++cell) {
                    const double base = (cell + 1) * (level + 1) * 10.0;
                    fields.set_mass(0, cell, level, base);
                    fields.set_mass(1, cell, level, 2.0 * base);
                }
            }
            bool bound = true;
            bound &= catchem_state_bind_met_3d_checked(state_ptr, "T", fields.temperature.data(), Us1State::kCells,
                                                       Us1State::kLevels, 1) == CATCHEM_SUCCESS;
            bound &= catchem_state_bind_met_3d_checked(state_ptr, "AIRDEN", fields.air_density.data(), Us1State::kCells,
                                                       Us1State::kLevels, 1) == CATCHEM_SUCCESS;
            bound &= catchem_state_bind_met_3d_checked(state_ptr, "PMID", fields.p_mid.data(), Us1State::kCells,
                                                       Us1State::kLevels, 1) == CATCHEM_SUCCESS;
            bound &= catchem_state_bind_met_3d_checked(state_ptr, "RH", fields.relative_humidity.data(),
                                                       Us1State::kCells, Us1State::kLevels, 1) == CATCHEM_SUCCESS;
            bound &= catchem_state_bind_met_3d_checked(state_ptr, "BXHEIGHT", fields.thickness.data(),
                                                       Us1State::kCells, Us1State::kLevels, 1) == CATCHEM_SUCCESS;
            bound &= catchem_state_bind_unified_chemistry_checked(state_ptr, fields.concentration.data(),
                                                                  Us1State::kCells, Us1State::kLevels,
                                                                  Us1State::kSpecies) == CATCHEM_SUCCESS;
            check(bound, "the optics Core binds met + chemistry fields");

            const char* opt_names[3] = {"extinction", "scattering", "aod"};
            const double lambdas[1] = {lambda};
            // extinction + scattering are coefficient (1 band x cells x levels);
            // aod is a column (1 band x cells).  Name-major total:
            const std::size_t coeff = Us1State::kPoints;  // cells * levels at one band
            const std::size_t column = Us1State::kCells;  // cells at one band
            const std::size_t total = 2 * coeff + column;
            std::vector<double> opt_values(total, -1.0);
            std::vector<int> opt_status(total, -1);
            const int optics_status =
                catchem_ondemand_get_optics_checked(optics_core, opt_names, 3, lambdas, 1, opt_values.data(),
                                                    opt_status.data(), static_cast<int>(opt_values.size()),
                                                    static_cast<int>(opt_status.size()));
            check(optics_status == CATCHEM_SUCCESS, "the optics endpoint answers three names at one wavelength");

            // The endpoint's extinction/scattering blocks must reproduce the
            // service-level values from the same masses, and the AOT block the
            // thickness-weighted column integral -- name-major layout verified.
            bool ext_block_ok = true, sca_block_ok = true, aod_block_ok = true;
            for (std::size_t p = 0; p < coeff; ++p) {
                ext_block_ok &= (opt_status[p] == kAvailable && std::abs(opt_values[p] - ext[p]) <= 1e-12);
                sca_block_ok &= (opt_status[coeff + p] == kAvailable &&
                                 std::abs(opt_values[coeff + p] - sca[p]) <= 1e-12);
            }
            for (std::size_t c = 0; c < column; ++c) {
                aod_block_ok &= (opt_status[2 * coeff + c] == kAvailable &&
                                 std::abs(opt_values[2 * coeff + c] - aod[c]) <= 1e-9 * (1.0 + aod[c]));
            }
            check(ext_block_ok, "the endpoint extinction block matches the service read (name-major offset 0)");
            check(sca_block_ok, "the endpoint scattering block follows at offset cells x levels");
            check(aod_block_ok, "the endpoint AOT block follows the coefficient blocks at offset cells");

            // The general single-quantity form reaches the same values: optics
            // with a wavelength, and a diagnostic name ignoring wavelengths.
            std::vector<double> one(coeff, -1.0);
            std::vector<int> one_status(coeff, -1);
            const int gq_optics = catchem_ondemand_get_quantity_checked(optics_core, "extinction", lambdas, 1, nullptr,
                                                                        0, one.data(), one_status.data(),
                                                                        static_cast<int>(one.size()),
                                                                        static_cast<int>(one_status.size()));
            bool gq_ext_ok = (gq_optics == CATCHEM_SUCCESS);
            for (std::size_t p = 0; p < coeff && gq_ext_ok; ++p) {
                gq_ext_ok &= (one_status[p] == kAvailable && std::abs(one[p] - ext[p]) <= 1e-12);
            }
            check(gq_ext_ok, "the general form returns extinction for an optics name with a wavelength");

            std::vector<double> pm_one(coeff, -1.0);
            std::vector<int> pm_status(coeff, -1);
            const int gq_pm = catchem_ondemand_get_quantity_checked(optics_core, "pm2_5", nullptr, 0, nullptr, 0,
                                                                    pm_one.data(), pm_status.data(),
                                                                    static_cast<int>(pm_one.size()),
                                                                    static_cast<int>(pm_status.size()));
            bool gq_pm_ok = (gq_pm == CATCHEM_SUCCESS);
            for (std::size_t p = 0; p < coeff && gq_pm_ok; ++p) {
                const int cell = static_cast<int>(p % Us1State::kCells);
                const int level = static_cast<int>(p / Us1State::kCells);
                const double want = fields.expected_kg_per_m3(0, cell, level) + fields.expected_kg_per_m3(1, cell, level);
                gq_pm_ok &= (pm_status[p] == kAvailable && pm_one[p] == want);
            }
            check(gq_pm_ok, "the general form returns bulk pm2_5 for a diagnostic name without wavelengths");

            // An out-of-band wavelength is rejected through the endpoint,
            // naming the wavelength and the valid range.
            const double bad_lambda = 1.0;
            std::vector<double> rejected(total, 0.0);
            const int reject_status =
                catchem_ondemand_get_optics_checked(optics_core, opt_names, 3, &bad_lambda, 1, rejected.data(), nullptr,
                                                    static_cast<int>(rejected.size()), 0);
            check(reject_status == CATCHEM_QUANTITY_NOT_AVAILABLE, "the optics endpoint rejects an out-of-band λ");
            check(last_error().find("1 m") != std::string::npos, "the endpoint rejection names the wavelength");

            // A diagnostic name sent to the optics endpoint is rejected by name.
            const char* wrong_names[1] = {"pm2_5"};
            std::vector<double> unused(total, 0.0);
            const int wrong_status = catchem_ondemand_get_optics_checked(optics_core, wrong_names, 1, lambdas, 1,
                                                                         unused.data(), nullptr,
                                                                         static_cast<int>(unused.size()), 0);
            check(wrong_status == CATCHEM_INVALID_INDEX, "a non-optics name fails the optics endpoint by name");
            check(last_error().find("pm2_5") != std::string::npos, "the failure names the non-optics quantity");

            // An undersized buffer is rejected before values are trusted.
            std::vector<double> small(coeff, 0.0);  // one block, three requested
            const int small_status = catchem_ondemand_get_optics_checked(optics_core, opt_names, 3, lambdas, 1,
                                                                         small.data(), nullptr,
                                                                         static_cast<int>(small.size()), 0);
            check(small_status == CATCHEM_EXTENT_MISMATCH, "an undersized optics buffer fails EXTENT_MISMATCH");

            // A species subset is not yet wired through the general form.
            const char* species[1] = {"fine_aero"};
            std::vector<double> sp_out(coeff, 0.0);
            const int sp_status = catchem_ondemand_get_quantity_checked(optics_core, "extinction", lambdas, 1, species,
                                                                        1, sp_out.data(), nullptr,
                                                                        static_cast<int>(sp_out.size()), 0);
            check(sp_status == CATCHEM_INVALID_INDEX, "a species filter is rejected until attribution lands");

            catchem_core_destroy(optics_core);
        }
        std::remove(config_path.c_str());
    }

    catchem::exaero_util::ExaeroPackageHandle::finalize_environment();
#endif

    if (failures == 0) {
        std::cout << "ALL ON-DEMAND API CHECKS PASSED\n";
        return 0;
    }
    std::cout << failures << " ON-DEMAND API CHECK(S) FAILED\n";
    return 1;
}

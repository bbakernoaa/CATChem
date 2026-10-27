/**
 * @file catchem_process_so4chem.cpp
 * @brief Implements the GOCART sulfate-chemistry adapter and its Fortran bridge call.
 *
 * @details Every bridge field pointer is borrowed from StateManager and must
 * outlive the call; the persistent oxidant/production vectors are
 * process-owned across timesteps and round-trip through the bridge in place.
 * Bridge diagnostics arrive in one packed [n_cols, n_levels, n_diag_species]
 * scratch buffer that run() scatters into per-species Production_rate_<sp>
 * fields, while PSO4/DMS products are copied straight from the persistent
 * accumulators. Units: gases ppmv, aerosols ug/kg; production rates kg/kg/s
 * (budget check converts with 1e9 × dt to ug/kg).
 */
#include "catchem_process_so4chem.hpp"
#include "catchem_diagnostic_manager.hpp"
#include "catchem_error.hpp"
#include "catchem_logger.hpp"
#include "catchem_process_registry.hpp"
#include <algorithm>
#include <array>
#include <iomanip>
#include <sstream>

extern "C" {
/**
 * @brief Flat C entry point into the Fortran sulfur-chemistry science bridge.
 *
 * @details Implemented in src/process/so4chem/SO4chemScienceBridge.F90, which
 * reconstitutes the flat buffers, iterates columns, and drives the GOCART
 * SulfateChemDriver. Met arrays are intent(in) host views over [n_cols,
 * n_levels] (2-D surface inputs over [n_cols]); `conc` is intent(inout) and
 * written in place with gases in ppmv and SO4/MSA in ug/kg; `tendency` is
 * caller scratch. The c_* persistent arrays are intent(inout) cross-timestep
 * state owned by the C++ adapter (see SO4chemProcess members) and updated in
 * place per column, except `c_pso2_dms`, which is bound for ABI completeness
 * but left untouched (see the member note). `c_diag_prod_rate` receives one
 * [n_cols, n_levels] slab per diagnostic slot in GLOBAL 1-based
 * `diagnostic_species_id` order.
 *
 * @note The bridge returns no status code: a scheme failure stops the run
 *       inside Fortran. `z0h` must already be thermal roughness in metres —
 *       run() supplies the Z0-derived fallback before the call. Calendar
 *       components drive the scheme's daily H2O2 oxidant recycle.
 */
void run_so4chem_science_bridge(int n_cols, int n_levels, int n_species, double dt, int diagnostics,
                                int gocart_update_so2, int gocart_do_drydep, int year, int month, int day, int hour,
                                int minute, int second, double* airden, double* cldf, double* delp, double* pmid,
                                double* t_air, double* z_edges, double* hflux, double* lat, double* lon, int* lwi,
                                double* pblh, double* u10m, double* ustar, double* v10m, double* z0h,
                                double* species_mw_g, const char* species_names, double* conc, double* tendency,
                                int* c_firsttime, int* c_nymd_last, int* c_nhms_last_recycle, double* c_xh2o2_init,
                                double* c_pso4_g_so2, double* c_pso4_aq_so2, double* c_pso2_dms, double* c_dms_flux,
                                double* c_diag_prod_rate, const int* diagnostic_species_id, int n_diag_species);
}

namespace catchem {

    ProcessContract SO4chemProcess::get_contract() const {
        return make_contract(
            get_name(), {host_field_3d("T", "K"), host_field_3d("PMID", "Pa"), host_field_interface("PEDGE", "Pa"),
                         host_field_interface("Z", "m"), host_field_3d("DELP", "Pa"), host_field_3d("AIRDEN", "kg/m3"),
                         host_field_3d("CLDF", "1"), host_field_2d("HFLUX", "W/m2"),
                         host_field_2d("LAT", "degrees", FieldRequirement::Required, AccessIntent::Read,
                                       PersistencePolicy::Persistent),
                         host_field_2d("LON", "degrees", FieldRequirement::Required, AccessIntent::Read,
                                       PersistencePolicy::Persistent),
                         host_field_2d("PBLH", "m"), host_field_2d("USTAR", "m/s"), host_field_2d("U10M", "m/s"),
                         host_field_2d("V10M", "m/s"), host_field_2d("LWI", "1"),
                         host_field_2d("Z0H", "m", FieldRequirement::Optional),
                         host_field_2d("Z0", "m", FieldRequirement::Optional), host_concentration()});
    }

    SO4chemProcess::SO4chemProcess() : active_scheme("gocart"), diagnostics_enabled(true) {}

    // Documentation lives on the header declaration; this reminder preserves the
    // derivation/validation contract locally: derive shared meteorology and
    // validate the SO4Chem roughness alternative.
    void SO4chemProcess::prepare_inputs(std::shared_ptr<StateManager> state) {
        state->derive_delp();
        state->derive_airden();
        // GOCART's surface resistance needs thermal roughness. A host may
        // supply Z0H directly or provide momentum roughness Z0 for the
        // documented conversion in run(); reject the case where neither is
        // current before entering the science bridge.
        if (!state->read_field<2>("Z0H") && !state->read_field<2>("Z0"))
            throw std::runtime_error("SO4Chem requires current Z0H or fallback Z0 roughness input");
    }

    void SO4chemProcess::init(std::shared_ptr<StateManager> state) {
        const auto config = state->config_manager();
        if (!config)
            throw std::invalid_argument("SO4Chem requires a runtime YAML configuration");
        const auto configured = config->data.processes.find("so4chem");
        if (configured == config->data.processes.end() || configured->second.scheme != "gocart")
            throw std::invalid_argument("SO4Chem requires processes.so4chem.scheme: gocart");
        diagnostics_enabled = configured->second.diagnostics;

        // Read scheme tuning options from the runtime YAML.  Each lookup falls
        // back to the compiled default declared in SO4chemCommon_Mod.F90, so a
        // configuration that omits the option keeps current behavior.
        const auto& settings = configured->second;
        gocart_update_so2 = settings.get_bool("gocart/update_so2", gocart_update_so2);
        gocart_do_drydep = settings.get_bool("gocart/do_drydep", gocart_do_drydep);

        // Surface the effective scheme options so the run log confirms what
        // was parsed from the runtime YAML and will be passed to the bridge.
        Logger::debug(state.get(), "SO4Chem scheme options",
                      {{"scheme", active_scheme},
                       {"gocart/update_so2", gocart_update_so2 ? "true" : "false"},
                       {"gocart/do_drydep", gocart_do_drydep ? "true" : "false"}});

        // Preserve the unit contract of ProcessSO4chemInterface_Mod and
        // SO4chemScheme_GOCART_Mod: gases are carried in ppmv, while SO4 and
        // MSA are aerosol mass in ug/kg.  The science scheme has fixed
        // conversions for these four species, so accepting a different phase
        // classification would silently corrupt source strengths and lifetimes.
        struct SpeciesUnitContract {
            const char* name;
            bool is_gas;
        };
        constexpr std::array<SpeciesUnitContract, 4> unit_contract = {
            {{"dms", true}, {"so2", true}, {"so4", false}, {"msa", false}}};
        const auto& chemistry = state->chemistry();
        if (!chemistry.mechanism)
            throw std::invalid_argument("SO4Chem requires a loaded species mechanism");
        for (const auto& expected : unit_contract) {
            if (!chemistry.mechanism->contains(expected.name))
                throw std::invalid_argument(std::string("SO4Chem requires species '") + expected.name + "'");
            const auto index = chemistry.mechanism->index_of(expected.name);
            const auto& metadata = chemistry.species_list[index];
            if (metadata.is_gas != expected.is_gas || metadata.is_aerosol == expected.is_gas) {
                throw std::invalid_argument(std::string("SO4Chem requires '") + expected.name + "' to be a " +
                                            (expected.is_gas ? "gas (ppmv)" : "aerosol (ug/kg)"));
            }
        }

        // 1. Allocate persistent states
        firsttime.assign(state->column_count(), 1);
        nymd_last.assign(state->column_count(), 0);
        nhms_last_recycle.assign(state->column_count(), 0);
        xh2o2_init.assign(state->column_count() * state->level_count(), 0.0);
        pso4_g_so2.assign(state->column_count() * state->level_count(), 0.0);
        pso4_aq_so2.assign(state->column_count() * state->level_count(), 0.0);
        pso2_dms.assign(state->column_count() * state->level_count(), 0.0);
        dms_flux.assign(state->column_count(), 0.0);

        // 2. Resolve the diagnostic species set (parity with legacy).  The
        // GOCART scheme matches diagnostic_species_id(diag_idx) == species_idx
        // where species_idx is the GLOBAL catalog position, so ids live in
        // global 1-based space.  Default set = the species the legacy so4chem
        // process is configured with (the sulfur chain plus its oxidants); the
        // scheme only fills MSA/SO2/SO4 slots, so the others register as zeros
        // exactly as legacy does.  An explicit diag_species overrides the
        // default and must name a real mechanism species (fail-loud).  Names
        // resolve case-insensitively against the mechanism.
        // Built before the diagnostics gate so the ids are always available to
        // run() (mirrors dust/seasalt/carbchem).
        diagnostic_species_id.clear();
        {
            const std::vector<std::string> default_sulfur = {"dms", "so2", "so4", "msa", "h2o2", "oh", "no3", "dms_in"};
            const auto& requested = settings.diag_species.empty() ? default_sulfur : settings.diag_species;
            for (const auto& species_name : requested) {
                if (!chemistry.mechanism->contains(species_name)) {
                    if (settings.diag_species.empty())
                        continue; // default set: tolerate species absent from this mechanism
                    throw std::invalid_argument("SO4Chem diag_species names an unknown species: " + species_name);
                }
                diagnostic_species_id.push_back(static_cast<int>(chemistry.mechanism->index_of(species_name)) + 1);
            }
        }

        if (!diagnostics_enabled)
            return;

        // 3. Register diagnostics.  Axes are explicit so the writer never
        // shape-guesses: a single-level column would otherwise mis-resolve a
        // _per_level field as a singleton (feature 013).
        if (state->diagnostic_manager()) {
            std::vector<int> dims_2d = {state->column_count(), state->level_count()};
            std::vector<int> dims_1d = {state->column_count(), 1};
            const std::vector<SemanticAxis> axes_level = {SemanticAxis::Column, SemanticAxis::Level};
            const std::vector<SemanticAxis> axes_single = {SemanticAxis::Column, SemanticAxis::Singleton};

            state->diagnostic_manager()->register_field_contract("PSO4_from_gaseous_SO2_per_level", "PSO4 gas source",
                                                                 "kg/kg/s", DiagType::FIELD_2D, dims_2d,
                                                                 DiagnosticPolicy::Instantaneous, 0.0, axes_level);
            state->diagnostic_manager()->register_field_contract("PSO4_from_aqueous_SO2_per_level", "PSO4 aq source",
                                                                 "kg/kg/s", DiagType::FIELD_2D, dims_2d,
                                                                 DiagnosticPolicy::Instantaneous, 0.0, axes_level);
            state->diagnostic_manager()->register_field_contract("DMS_emission_flux", "DMS emission surface flux",
                                                                 "kg/m2/s", DiagType::FIELD_2D, dims_1d,
                                                                 DiagnosticPolicy::Instantaneous, 0.0, axes_single);

            // One Production_rate_<sp> field per selected species (names
            // unchanged from the legacy per-species convention).  run() fills
            // each field from its OWN slot of the bridge's packed buffer.
            for (const int global_index : diagnostic_species_id) {
                const auto& meta = state->chemistry().species_list[static_cast<std::size_t>(global_index - 1)];
                std::string diag_name = "Production_rate_" + meta.short_name;
                state->diagnostic_manager()->register_field_contract(diag_name, "Production rate " + meta.short_name,
                                                                     "kg/kg/s", DiagType::FIELD_2D, dims_2d,
                                                                     DiagnosticPolicy::Instantaneous, 0.0, axes_level);
            }
        }
    }

    void SO4chemProcess::run(std::shared_ptr<StateManager> state) {

        // 1. Retrieve 3D Meteorological variables
        double* airden_ptr = state->write_field<3>("AIRDEN");

        double* pmid_ptr = state->write_field<3>("PMID");
        double* t_ptr = state->write_field<3>("T");
        double* z_ptr = state->write_field<3>("Z");
        double* cldf_ptr = state->write_field<3>("CLDF");

        double* delp_ptr = state->write_field<3>("DELP");

        // 2. Retrieve 2D Surface Met variables
        double* hflux_ptr = state->write_field<2>("HFLUX");

        double* lat_ptr = state->write_field<2>("LAT");

        double* lon_ptr = state->write_field<2>("LON");

        double* pblh_ptr = state->write_field<2>("PBLH");

        double* ustar_ptr = state->write_field<2>("USTAR");

        double* u10m_ptr = state->write_field<2>("U10M");

        double* v10m_ptr = state->write_field<2>("V10M");

        double* lwi_ptr = state->write_field<2>("LWI");
        std::vector<int> lwi(state->column_count());
        require_field_pointer("SO4chem", "LWI", lwi_ptr);
        for (int col = 0; col < state->column_count(); ++col)
            lwi[col] = static_cast<int>(lwi_ptr[col]);

        require_field_pointer("SO4chem", "AIRDEN", airden_ptr);
        require_field_pointer("SO4chem", "PMID", pmid_ptr);
        require_field_pointer("SO4chem", "T", t_ptr);
        require_field_pointer("SO4chem", "Z", z_ptr);
        require_field_pointer("SO4chem", "DELP", delp_ptr);
        require_field_pointer("SO4chem", "CLDF", cldf_ptr);
        require_field_pointer("SO4chem", "HFLUX", hflux_ptr);
        require_field_pointer("SO4chem", "LAT", lat_ptr);
        require_field_pointer("SO4chem", "LON", lon_ptr);
        require_field_pointer("SO4chem", "PBLH", pblh_ptr);
        require_field_pointer("SO4chem", "USTAR", ustar_ptr);
        require_field_pointer("SO4chem", "U10M", u10m_ptr);
        require_field_pointer("SO4chem", "V10M", v10m_ptr);

        // SulfateChemDriver uses thermal roughness in its resistance term.
        // NUOPC provides momentum roughness (Z0), but not necessarily thermal
        // roughness (Z0H). Preserve an explicitly supplied Z0H; otherwise use
        // the UFS/GOCART convention after NUOPC has converted Z0 from cm to m:
        // Z0H = 0.01 * raw Z0 = Z0 in the already-converted CATChem state.
        const double* z0h_ptr = state->read_field<2>("Z0H");
        std::vector<double> z0h_from_z0;
        if (!z0h_ptr) {
            const double* z0_ptr = state->read_field<2>("Z0");
            require_field_pointer("SO4chem", "Z0 (required when Z0H is unavailable)", z0_ptr);
            z0h_from_z0.resize(state->column_count());
            for (int col = 0; col < state->column_count(); ++col)
                z0h_from_z0[col] = 0.01 * z0_ptr[col];
            z0h_ptr = z0h_from_z0.data();
        }

        // 3. Chemical and Tendency Views
        double* conc_ptr = state->chemistry().conc ? state->chemistry().conc->host_write() : nullptr;
        require_field_pointer("SO4chem", "CHEM_CONC", conc_ptr);
        const int diagnostic_slab = state->column_count() * state->level_count();
        std::vector<double> concentration_before(conc_ptr, conc_ptr + static_cast<std::size_t>(diagnostic_slab) *
                                                                          state->species_count());

#ifdef CATCHEM_TRACE_NUOPC
        // This is deliberately immediately before the science bridge.  It
        // reports the exact post-AQMIO/post-emission values consumed by
        // GOCART, including the concentration units used by SO4chem.  A
        // mismatch here is an exchange/units problem; a match here with a
        // divergent production rate isolates the issue to the kernel/met.
        const auto& chemistry_for_trace = state->chemistry();
        const auto trace_species = [&](const char* name) {
            if (!chemistry_for_trace.mechanism || !chemistry_for_trace.mechanism->contains(name))
                return;
            const auto index = chemistry_for_trace.mechanism->index_of(name);
            const auto* values =
                conc_ptr + static_cast<std::size_t>(index) * state->column_count() * state->level_count();
            const auto count = static_cast<std::size_t>(state->column_count()) * state->level_count();
            auto [lo, hi] = std::minmax_element(values, values + count);
            Logger::debug(state.get(), "SO4chem pre-bridge concentration",
                          {{"species", name}, {"min", std::to_string(*lo)}, {"max", std::to_string(*hi)}});
        };
        trace_species("dms");
        trace_species("so2");
        trace_species("so4");
        trace_species("msa");
        trace_species("h2o2");
        trace_species("oh");
        trace_species("no3");
        trace_species("dms_in");
#endif

        // Allocate local tendencies buffer
        std::vector<double> mock_tendency(state->column_count() * state->level_count() * state->species_count(), 0.0);

        // 4. Retrieve species properties from ChemState
        std::vector<double> mw_g(state->species_count(), 0.0);
        for (size_t i = 0; i < state->chemistry().species_list.size(); ++i) {
            if (!(state->chemistry().species_list[i].mw_g > 0.0))
                throw std::runtime_error("SO4Chem species '" + state->chemistry().species_list[i].short_name +
                                         "' requires an explicit molecular weight");
            mw_g[i] = state->chemistry().species_list[i].mw_g;
        }

        // 5. Invoke flat science bridge.  diagnostic_species_id (built in
        // init) holds GLOBAL 1-based catalog indices — the space the GOCART
        // scheme matches on.  The packed diag_prod_rate scratch receives one
        // [ncol, nz] slab per selected species so each Production_rate_<sp>
        // field gets its OWN slot (the former pso4_so2 buffer collapsed every
        // species onto slot 1).  When diagnostics are off, forward a valid
        // size-1 id dummy holding 0 and a zero count (mirrors the other
        // bridges' no_diag_species guard).
        const bool forward_diag = diagnostics_enabled && !diagnostic_species_id.empty();
        static const int no_diag_species = 0;
        const int* diag_ids = forward_diag ? diagnostic_species_id.data() : &no_diag_species;
        const int n_diag_species = forward_diag ? static_cast<int>(diagnostic_species_id.size()) : 0;
        const int slab = state->column_count() * state->level_count();
        std::vector<double> diag_prod_rate(static_cast<size_t>(slab) * (forward_diag ? n_diag_species : 1), 0.0);
        run_so4chem_science_bridge(
            state->column_count(), state->level_count(), state->species_count(), state->clock().timestep,
            diagnostics_enabled ? 1 : 0, gocart_update_so2 ? 1 : 0, gocart_do_drydep ? 1 : 0, state->clock().year,
            state->clock().month, state->clock().day, state->clock().hour, state->clock().minute, state->clock().second,
            airden_ptr, cldf_ptr, delp_ptr, pmid_ptr, t_ptr, z_ptr, hflux_ptr, lat_ptr, lon_ptr, lwi.data(), pblh_ptr,
            u10m_ptr, ustar_ptr, v10m_ptr, const_cast<double*>(z0h_ptr), mw_g.data(),
            state->chemistry().species_names_c_arr.data(), conc_ptr, mock_tendency.data(), firsttime.data(),
            nymd_last.data(), nhms_last_recycle.data(), xh2o2_init.data(), pso4_g_so2.data(), pso4_aq_so2.data(),
            pso2_dms.data(), dms_flux.data(), diag_prod_rate.data(), diag_ids, n_diag_species);

        // Close each selected GOCART SO4/MSA production diagnostic against
        // that species' updated aerosol concentration. The bridge emits rates
        // in kg/kg/s while aerosol state is ug/kg. An explicit diagnostic
        // subset may omit one product, so only close products with a matching
        // registered diagnostic slot.
        for (const char* species_name : {"so4", "msa"}) {
            if (!diagnostics_enabled)
                break;
            if (!state->chemistry().mechanism->contains(species_name))
                continue;
            const auto species_index = state->chemistry().mechanism->index_of(species_name);
            if (state->chemistry().species_list[species_index].is_gas)
                throw std::runtime_error(std::string("SO4Chem production budget requires aerosol species ") +
                                         species_name);
            int diagnostic_slot = -1;
            for (int diagnostic = 0; diagnostic < n_diag_species; ++diagnostic) {
                if (diagnostic_species_id[diagnostic] == static_cast<int>(species_index + 1)) {
                    diagnostic_slot = diagnostic;
                    break;
                }
            }
            if (diagnostic_slot < 0)
                continue;
            const std::size_t species_offset = species_index * diagnostic_slab;
            const std::size_t diagnostic_offset = static_cast<std::size_t>(diagnostic_slot) * diagnostic_slab;
            double aerosol_change_ug_kg = 0.0;
            double integrated_production_ug_kg = 0.0;
            double column_production_kg_m2 = 0.0;
            for (int cell = 0; cell < diagnostic_slab; ++cell) {
                aerosol_change_ug_kg += conc_ptr[species_offset + cell] - concentration_before[species_offset + cell];
                integrated_production_ug_kg +=
                    diag_prod_rate[diagnostic_offset + cell] * state->clock().timestep * 1.0e9;
                column_production_kg_m2 +=
                    diag_prod_rate[diagnostic_offset + cell] * state->clock().timestep * delp_ptr[cell] / 9.80665;
            }
            std::ostringstream record;
            record << std::setprecision(17) << "CATCHEM_COLUMN_TERM process=so4chem species=" << species_name
                   << " source=" << column_production_kg_m2 << " sink=0 units=kg_m-2";
            Logger::info(state.get(), record.str());
            constexpr double production_budget_absolute_tolerance_ug_kg = 1.0e-10;
            const double production_budget_absolute_error_ug_kg =
                std::abs(aerosol_change_ug_kg - integrated_production_ug_kg);
            const double production_budget_scale_ug_kg =
                std::abs(aerosol_change_ug_kg) + std::abs(integrated_production_ug_kg);
            const double production_budget_residual =
                production_budget_absolute_error_ug_kg / std::max(production_budget_scale_ug_kg, 1.0e-30);
            std::ostringstream budget_record;
            budget_record << "CATCHEM_BUDGET name=so4chem_production species=" << species_name
                          << " RESIDUAL=" << std::scientific << std::setprecision(16) << production_budget_residual
                          << " TOTAL=" << aerosol_change_ug_kg << " BIN_SUM=" << integrated_production_ug_kg
                          << " ABS_ERROR=" << production_budget_absolute_error_ug_kg
                          << " ABS_TOLERANCE=" << production_budget_absolute_tolerance_ug_kg;
            Logger::info(state.get(), budget_record.str());
        }

        // 6. Map persistent column diagnostics straight to registered C++ Diagnostics Views
        if (state->diagnostic_manager() && diagnostics_enabled) {
            double* diag_pso4_g =
                (double*)state->diagnostic_manager()->get_host_write_pointer("PSO4_from_gaseous_SO2_per_level");
            double* diag_pso4_aq =
                (double*)state->diagnostic_manager()->get_host_write_pointer("PSO4_from_aqueous_SO2_per_level");
            double* diag_dms_flux = (double*)state->diagnostic_manager()->get_host_write_pointer("DMS_emission_flux");

            if (diag_pso4_g)
                std::copy(pso4_g_so2.begin(), pso4_g_so2.end(), diag_pso4_g);
            if (diag_pso4_aq)
                std::copy(pso4_aq_so2.begin(), pso4_aq_so2.end(), diag_pso4_aq);
            if (diag_dms_flux)
                std::copy(dms_flux.begin(), dms_flux.end(), diag_dms_flux);

            // Scatter each packed slot into its own Production_rate_<sp> field
            // (field names unchanged from the legacy per-species convention).
            for (int d = 0; d < n_diag_species; ++d) {
                const auto& meta =
                    state->chemistry().species_list[static_cast<std::size_t>(diagnostic_species_id[d]) - 1];
                std::string diag_name = "Production_rate_" + meta.short_name;
                double* diag_prod = (double*)state->diagnostic_manager()->get_host_write_pointer(diag_name);
                if (diag_prod)
                    std::copy(diag_prod_rate.begin() + static_cast<std::ptrdiff_t>(d) * slab,
                              diag_prod_rate.begin() + static_cast<std::ptrdiff_t>(d + 1) * slab, diag_prod);
            }
        }

        if (state->chemistry().conc)
            state->chemistry().conc->mark_host_modified();
    }

} // namespace catchem

extern "C" {
/**
 * @brief Register the "so4chem" process and its settings schema with the ProcessRegistry.
 *
 * @details Called once by the API layer's built-in registration pass. The
 * validator accepts the two gocart/update_so2 and gocart/do_drydep options
 * under processes.so4chem, so an unknown nested option fails at
 * initialization instead of silently keeping its compiled default.
 */
void catchem_register_so4chem_cpp() {
    catchem::ProcessRegistry::get_instance().register_process(
        "so4chem", []() { return std::make_shared<catchem::SO4chemProcess>(); }, {},
        catchem::make_settings_validator("so4chem", {"gocart/update_so2", "gocart/do_drydep"}));
}
}

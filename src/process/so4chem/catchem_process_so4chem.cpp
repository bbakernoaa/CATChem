#include "catchem_process_so4chem.hpp"
#include "catchem_diagnostic_manager.hpp"
#include "catchem_error.hpp"
#include "catchem_logger.hpp"
#include "catchem_process_registry.hpp"
#include <array>
#include <iostream>

extern "C" {
void run_so4chem_science_bridge(int n_cols, int n_levels, int n_species, double dt, int diagnostics,
                                int gocart_update_so2, int year, int month, int day, int hour, int minute, int second,
                                double* airden, double* cldf, double* delp, double* pmid, double* t_air,
                                double* z_edges, double* hflux, double* lat, double* lon, int* lwi, double* pblh,
                                double* u10m, double* ustar, double* v10m, double* z0h, double* species_mw_g,
                                const char* species_names, double* conc, double* tendency, bool* c_firsttime,
                                int* c_nymd_last, int* c_nhms_last_recycle, double* c_xh2o2_init, double* c_pso4_so2,
                                double* c_pso4_g_so2, double* c_pso4_aq_so2, double* c_pso2_dms, double* c_dms_flux,
                                const int* diagnostic_species_id, int n_diag_species);
}

namespace catchem {

    ProcessContract SO4chemProcess::get_contract() const {
        return {get_name(),
                {host_field_3d("T", "K"), host_field_3d("PMID", "Pa"), host_field_interface("PEDGE", "Pa"),
                 host_field_interface("Z", "m"), host_field_3d("DELP", "Pa"), host_field_3d("AIRDEN", "kg/m3"),
                 host_field_3d("CLDF", "1"), host_field_2d("HFLUX", "W/m2"),
                 host_field_2d("LAT", "degrees", FieldRequirement::Required, AccessIntent::Read,
                               PersistencePolicy::Persistent),
                 host_field_2d("LON", "degrees", FieldRequirement::Required, AccessIntent::Read,
                               PersistencePolicy::Persistent),
                 host_field_2d("PBLH", "m"), host_field_2d("USTAR", "m/s"), host_field_2d("U10M", "m/s"),
                 host_field_2d("V10M", "m/s"), host_field_2d("LWI", "1"),
                 host_field_2d("Z0H", "m", FieldRequirement::Optional), host_concentration()},
                {}};
    }

    SO4chemProcess::SO4chemProcess() : active_scheme("gocart"), diagnostics_enabled(true) {}

    void SO4chemProcess::prepare_inputs(std::shared_ptr<StateManager> state) {
        state->derive_delp();
        state->derive_airden();
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
        gocart_update_so2 = configured->second.get_bool("gocart/update_so2", gocart_update_so2);

        // Surface the effective scheme options so the run log confirms what
        // was parsed from the runtime YAML and will be passed to the bridge.
        Logger::info(state.get(), "SO4Chem scheme options",
                     {{"scheme", active_scheme}, {"gocart/update_so2", gocart_update_so2 ? "true" : "false"}});

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
        pso4_so2.assign(state->column_count() * state->level_count(), 0.0);
        pso4_g_so2.assign(state->column_count() * state->level_count(), 0.0);
        pso4_aq_so2.assign(state->column_count() * state->level_count(), 0.0);
        pso2_dms.assign(state->column_count() * state->level_count(), 0.0);
        dms_flux.assign(state->column_count(), 0.0);

        // 2. Register diagnostics
        if (state->diagnostic_manager()) {
            std::vector<int> dims_2d = {state->column_count(), state->level_count()};
            std::vector<int> dims_1d = {state->column_count(), 1};

            state->diagnostic_manager()->register_field("PSO4_from_gaseous_SO2_per_level", "PSO4 gas source", "kg/kg/s",
                                                        DiagType::FIELD_2D, dims_2d);
            state->diagnostic_manager()->register_field("PSO4_from_aqueous_SO2_per_level", "PSO4 aq source", "kg/kg/s",
                                                        DiagType::FIELD_2D, dims_2d);
            state->diagnostic_manager()->register_field("DMS_emission_flux", "DMS emission surface flux", "kg/m2/s",
                                                        DiagType::FIELD_2D, dims_1d);

            const auto configured = state->config_manager() ? state->config_manager()->data.processes.find("so4chem")
                                                            : std::map<std::string, ProcessConfig>::const_iterator{};
            const bool has_config =
                state->config_manager() && configured != state->config_manager()->data.processes.end();
            const auto diagnostic_names = has_config ? configured->second.diag_species : std::vector<std::string>{};
            for (const auto& species_name : diagnostic_names) {
                if (state->chemistry().mechanism && state->chemistry().mechanism->contains(species_name)) {
                    const auto i = state->chemistry().mechanism->index_of(species_name);
                    const auto& meta = state->chemistry().species_list[i];
                    std::string diag_name = "Production_rate_" + meta.short_name;
                    state->diagnostic_manager()->register_field(diag_name, "Production rate " + meta.short_name,
                                                                "kg/kg/s", DiagType::FIELD_2D, dims_2d);

                    // Track diagnostic species index (1-based)
                    diagnostic_species_id.push_back(i + 1);
                }
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

        double* z0h = state->write_field<2>("Z0H");
        require_field_pointer("SO4chem", "Z0H", z0h);

        // 3. Chemical and Tendency Views
        double* conc_ptr = state->chemistry().conc ? state->chemistry().conc->host_write() : nullptr;
        require_field_pointer("SO4chem", "CHEM_CONC", conc_ptr);

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

        // 5. Invoke flat science bridge
        run_so4chem_science_bridge(
            state->column_count(), state->level_count(), state->species_count(), state->clock().timestep,
            diagnostics_enabled ? 1 : 0, gocart_update_so2 ? 1 : 0, state->clock().year, state->clock().month,
            state->clock().day, state->clock().hour, state->clock().minute, state->clock().second, airden_ptr, cldf_ptr,
            delp_ptr, pmid_ptr, t_ptr, z_ptr, hflux_ptr, lat_ptr, lon_ptr, lwi.data(), pblh_ptr, u10m_ptr, ustar_ptr,
            v10m_ptr, z0h, mw_g.data(), state->chemistry().species_names_c_arr.data(), conc_ptr, mock_tendency.data(),
            (bool*)firsttime.data(), nymd_last.data(), nhms_last_recycle.data(), xh2o2_init.data(), pso4_so2.data(),
            pso4_g_so2.data(), pso4_aq_so2.data(), pso2_dms.data(), dms_flux.data(), diagnostic_species_id.data(),
            diagnostic_species_id.size());

        // 6. Map persistent column diagnostics straight to registered C++ Diagnostics Views
        if (state->diagnostic_manager() && diagnostics_enabled) {
            double* diag_pso4_g =
                (double*)state->diagnostic_manager()->get_host_pointer("PSO4_from_gaseous_SO2_per_level");
            double* diag_pso4_aq =
                (double*)state->diagnostic_manager()->get_host_pointer("PSO4_from_aqueous_SO2_per_level");
            double* diag_dms_flux = (double*)state->diagnostic_manager()->get_host_pointer("DMS_emission_flux");

            if (diag_pso4_g)
                std::copy(pso4_g_so2.begin(), pso4_g_so2.end(), diag_pso4_g);
            if (diag_pso4_aq)
                std::copy(pso4_aq_so2.begin(), pso4_aq_so2.end(), diag_pso4_aq);
            if (diag_dms_flux)
                std::copy(dms_flux.begin(), dms_flux.end(), diag_dms_flux);

            // Map individual species production rate arrays (mapping levels and columns)
            for (const auto diagnostic_index : diagnostic_species_id) {
                const auto& meta = state->chemistry().species_list[static_cast<std::size_t>(diagnostic_index - 1)];
                std::string diag_name = "Production_rate_" + meta.short_name;
                double* diag_prod = (double*)state->diagnostic_manager()->get_host_pointer(diag_name);
                if (diag_prod) {
                    std::copy(pso4_so2.begin(), pso4_so2.end(), diag_prod);
                }
            }
        }

        if (state->chemistry().conc)
            state->chemistry().conc->mark_host_modified();
    }

} // namespace catchem

extern "C" {
void catchem_register_so4chem_cpp() {
    catchem::ProcessRegistry::get_instance().register_process(
        "so4chem", []() { return std::make_shared<catchem::SO4chemProcess>(); }, {},
        catchem::make_settings_validator("so4chem", {"gocart/update_so2"}));
}
}

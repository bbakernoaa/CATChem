#include "catchem_process_settling.hpp"
#include "catchem_error.hpp"
#include "catchem_logger.hpp"
#include "catchem_process_registry.hpp"
#include <iostream>
#include <stdexcept>

namespace catchem {

    extern "C" void run_settling_science_bridge(int n_columns, int n_levels, int n_species, double dt,
                                                int swelling_method, int correction_maring, double* airden,
                                                double* delp, double* rh, double* temperature, double* z_edge,
                                                const int* aerosol_indices, const double* radius, const double* density,
                                                double* concentration);

    ProcessContract SettlingProcess::get_contract() const {
        ProcessContract contract{get_name(),
                                 {host_field_3d("T", "K"), host_field_3d("AIRDEN", "kg/m3"),
                                  host_field_3d("DELP", "Pa"), host_field_3d("RH", "1"), host_field_interface("Z", "m"),
                                  host_concentration()},
                                 {}};
        for (auto& field : contract.fields)
            field.execution_space = ExecutionSpaceIntent::Device;
        return contract;
    }

    SettlingProcess::SettlingProcess() : active_scheme("c++_kokkos"), fortran_callback(nullptr) {}

    void SettlingProcess::init(std::shared_ptr<StateManager> state) {
        const auto config = state->config_manager();
        if (!config)
            throw std::invalid_argument("Settling requires a runtime YAML configuration");
        const auto configured = config->data.processes.find("settling");
        if (configured == config->data.processes.end() || configured->second.scheme != "gocart")
            throw std::invalid_argument("Settling requires processes.settling.scheme: gocart");
        active_scheme = configured->second.scheme;

        // Read scheme tuning options from the runtime YAML.  Each lookup falls
        // back to the compiled default declared in SettlingCommon_Mod.F90.
        gocart_scale_factor = configured->second.get_double("gocart/scale_factor", gocart_scale_factor);
        gocart_simple_scheme = configured->second.get_bool("gocart/simple_scheme", gocart_simple_scheme);
        gocart_swelling_method = configured->second.get_int("gocart/swelling_method", gocart_swelling_method);
        gocart_correction_maring = configured->second.get_bool("gocart/correction_maring", gocart_correction_maring);
        if (!(gocart_scale_factor > 0.0))
            throw std::invalid_argument("Settling gocart scale_factor must be positive");
        if (gocart_swelling_method != 1 && gocart_swelling_method != 2)
            throw std::invalid_argument("Settling gocart swelling_method must be 1 (Fitzgerald) or 2 (Gerber)");

        // Surface the effective scheme options so the run log confirms what
        // was parsed from the runtime YAML and reaches the settling kernel.
        // The metadata path corresponds to legacy simple_scheme: false.
        // Mie-table settling remains intentionally unsupported by C++.
        if (gocart_simple_scheme)
            throw std::invalid_argument(
                "Settling simple_scheme requires Mie tables and is unsupported by the C++ core");
        Logger::info(state.get(), "Settling scheme options",
                     {{"scheme", active_scheme},
                      {"gocart/scale_factor", std::to_string(gocart_scale_factor)},
                      {"gocart/correction_maring", gocart_correction_maring ? "true" : "false"},
                      {"gocart/simple_scheme", "false (species metadata)"},
                      {"gocart/swelling_method", std::to_string(gocart_swelling_method)}});

        int num_aerosols = state->chemistry().aerosol_indices.size();
        if (num_aerosols > 0) {
#ifdef CATCHEM_ENABLE_KOKKOS
            dev_aero_indices =
                Kokkos::View<int*, Kokkos::DefaultExecutionSpace::memory_space>("dev_aero_indices", num_aerosols);
            dev_radius_dry =
                Kokkos::View<double*, Kokkos::DefaultExecutionSpace::memory_space>("dev_radius_dry", num_aerosols);
            dev_rhop_dry =
                Kokkos::View<double*, Kokkos::DefaultExecutionSpace::memory_space>("dev_rhop_dry", num_aerosols);

            auto host_aero_indices = Kokkos::create_mirror_view(dev_aero_indices);
            auto host_radius_dry = Kokkos::create_mirror_view(dev_radius_dry);
            auto host_rhop_dry = Kokkos::create_mirror_view(dev_rhop_dry);
#else
            host_aero_indices.assign(num_aerosols, 0);
            host_radius_dry.assign(num_aerosols, 0.0);
            host_rhop_dry.assign(num_aerosols, 0.0);
#endif

            for (int i = 0; i < num_aerosols; ++i) {
                int ispec = state->chemistry().aerosol_indices[i];
                host_aero_indices[i] = ispec;
                double r_val = state->chemistry().species_list[ispec].radius;
                double d_val = state->chemistry().species_list[ispec].density;
                if (!(r_val > 0.0 && d_val > 0.0))
                    throw std::runtime_error("Settling aerosol '" + state->chemistry().species_list[ispec].short_name +
                                             "' requires explicit radius and density");
                host_radius_dry[i] = r_val * 1e-6; // Species properties are configured in microns.
                host_rhop_dry[i] = d_val;
            }

#ifdef CATCHEM_ENABLE_KOKKOS
            Kokkos::deep_copy(dev_aero_indices, host_aero_indices);
            Kokkos::deep_copy(dev_radius_dry, host_radius_dry);
            Kokkos::deep_copy(dev_rhop_dry, host_rhop_dry);
#endif
        }
    }

    void SettlingProcess::set_fortran_bridge_callback(std::function<void(void*)> cb) {
        fortran_callback = cb;
    }

    void SettlingProcess::run(std::shared_ptr<StateManager> state) {
        if (fortran_callback) {
            // Fallback for tests explicitly requesting the Fortran bridge
            fortran_callback(static_cast<void*>(state.get()));
            if (state->chemistry().conc)
                state->chemistry().conc->mark_host_modified();
            return;
        }

        int num_aerosols = state->chemistry().aerosol_indices.size();
        if (num_aerosols == 0)
            return;

        require_field_pointer("Settling", "T", state->meteorology().T ? state->meteorology().T->host_data() : nullptr);
        require_field_pointer("Settling", "AIRDEN",
                              state->meteorology().AIRDEN ? state->meteorology().AIRDEN->host_data() : nullptr);
        double* delp = state->write_field<3>("DELP");
        double* z_edge = state->write_field<3>("Z");
        require_field_pointer("Settling", "DELP", delp);
        require_field_pointer("Settling", "RH",
                              state->meteorology().RH ? state->meteorology().RH->host_data() : nullptr);
        require_field_pointer("Settling", "Z", z_edge);
        require_field_pointer("Settling", "CHEM_CONC",
                              state->chemistry().conc ? state->chemistry().conc->host_data() : nullptr);

        run_settling_science_bridge(
            state->column_count(), state->level_count(), num_aerosols, state->clock().timestep, gocart_swelling_method,
            gocart_correction_maring ? 1 : 0, state->meteorology().AIRDEN->host_data(), delp,
            state->meteorology().RH->host_data(), state->meteorology().T->host_data(), z_edge, host_aero_indices.data(),
            host_radius_dry.data(), host_rhop_dry.data(), state->chemistry().conc->host_write());
        state->chemistry().conc->mark_host_modified();
    }

    void SettlingProcess::finalize() {
        // Kokkos views will be deallocated automatically when their reference count goes to zero.
    }

} // namespace catchem

extern "C" {
void catchem_register_settling_cpp() {
    catchem::ProcessRegistry::get_instance().register_process(
        "settling", []() { return std::make_shared<catchem::SettlingProcess>(); }, {},
        catchem::make_settings_validator("settling", {"gocart/scale_factor", "gocart/simple_scheme",
                                                      "gocart/swelling_method", "gocart/correction_maring"}));
}
}

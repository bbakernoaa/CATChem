#include "catchem_process_settling.hpp"
#include "catchem_error.hpp"
#include "catchem_process_registry.hpp"
#include "catchem_settling_physics.hpp"
#include <iostream>

namespace catchem {

    ProcessContract SettlingProcess::get_contract() const {
        ProcessContract contract{get_name(),
                                 {host_field_3d("T", "K"), host_field_3d("PMID", "Pa", FieldRequirement::Optional),
                                  host_field_3d("AIRDEN_DRY", "kg/m3", FieldRequirement::Optional),
                                  host_field_3d("BXHEIGHT", "m"),
                                  host_field_3d("AIRDEN", "kg/m3", FieldRequirement::Optional),
                                  host_field_interface("PEDGE", "Pa"), host_concentration()},
                                 {}};
        for (auto& field : contract.fields)
            field.execution_space = ExecutionSpaceIntent::Device;
        return contract;
    }

    SettlingProcess::SettlingProcess() : active_scheme("c++_kokkos"), fortran_callback(nullptr) {}

    void SettlingProcess::init(std::shared_ptr<StateManager> state) {
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
                host_radius_dry[i] = (r_val > 0.0 ? r_val : 1.0) * 1e-6; // Convert microns to meters
                host_rhop_dry[i] = d_val > 0.0 ? d_val : 2500.0;
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

        if (!state->meteorology().BXHEIGHT && state->meteorology().PEDGE && state->meteorology().T) {
            state->derive_bxheight();
        }
        if (!state->meteorology().AIRDEN && state->meteorology().AIRDEN_DRY) {
            state->meteorology().AIRDEN = state->meteorology().AIRDEN_DRY;
        }
        if (!state->meteorology().AIRDEN && !state->meteorology().AIRDEN_DRY && state->meteorology().PMID &&
            state->meteorology().T) {
            state->derive_airden_dry();
            state->meteorology().AIRDEN = state->meteorology().AIRDEN_DRY;
        }

        require_field_pointer("Settling", "T", state->meteorology().T ? state->meteorology().T->host_data() : nullptr);
        require_field_pointer("Settling", "AIRDEN",
                              state->meteorology().AIRDEN ? state->meteorology().AIRDEN->host_data() : nullptr);
        require_field_pointer("Settling", "PEDGE",
                              state->meteorology().PEDGE ? state->meteorology().PEDGE->host_data() : nullptr);
        require_field_pointer("Settling", "BXHEIGHT",
                              state->meteorology().BXHEIGHT ? state->meteorology().BXHEIGHT->host_data() : nullptr);
        require_field_pointer("Settling", "CHEM_CONC",
                              state->chemistry().conc ? state->chemistry().conc->host_data() : nullptr);

        settling::SettlingFunctor functor;
        functor.conc = state->chemistry().conc->view();
        functor.t = state->meteorology().T->view();
        functor.airden = state->meteorology().AIRDEN->view();
        functor.pedge = state->meteorology().PEDGE->view();
        functor.dz = state->meteorology().BXHEIGHT->view();

#ifdef CATCHEM_ENABLE_KOKKOS
        functor.aerosol_indices = dev_aero_indices;
        functor.aerosol_radius = dev_radius_dry;
        functor.aerosol_density = dev_rhop_dry;
#else
        functor.aerosol_indices =
            MdspanTypeHelper<int, 1>::type(host_aero_indices.data(), static_cast<int>(host_aero_indices.size()));
        functor.aerosol_radius =
            MdspanTypeHelper<double, 1>::type(host_radius_dry.data(), static_cast<int>(host_radius_dry.size()));
        functor.aerosol_density =
            MdspanTypeHelper<double, 1>::type(host_rhop_dry.data(), static_cast<int>(host_rhop_dry.size()));
#endif

        functor.cdt = state->clock().timestep;
        functor.n_levels = state->level_count();

#ifdef CATCHEM_ENABLE_KOKKOS
        Kokkos::parallel_for("settling_compute_c++",
                             Kokkos::MDRangePolicy<Kokkos::DefaultExecutionSpace, Kokkos::Rank<2>>(
                                 {0, 0}, {state->column_count(), num_aerosols}),
                             functor);
        Kokkos::fence();
#else
        for (int icol = 0; icol < state->column_count(); ++icol)
            for (int iaero = 0; iaero < num_aerosols; ++iaero)
                functor(icol, iaero);
#endif
    }

    void SettlingProcess::finalize() {
        // Kokkos views will be deallocated automatically when their reference count goes to zero.
    }

} // namespace catchem

extern "C" {
void catchem_register_settling_cpp() {
    catchem::ProcessRegistry::get_instance().register_process(
        "settling", []() { return std::make_shared<catchem::SettlingProcess>(); });
}
}

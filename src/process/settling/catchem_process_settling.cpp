#include "catchem_process_settling.hpp"
#include "catchem_error.hpp"
#include "catchem_process_registry.hpp"
#include "catchem_settling_physics.hpp"
#include <iostream>

namespace catchem {

    SettlingProcess::SettlingProcess() : active_scheme("c++_kokkos"), fortran_callback(nullptr) {}

    void SettlingProcess::init(std::shared_ptr<StateManager> state) {
        int num_aerosols = state->chem.aerosol_indices.size();
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
                int ispec = state->chem.aerosol_indices[i];
                host_aero_indices[i] = ispec;
                double r_val = state->chem.species_list[ispec].radius;
                double d_val = state->chem.species_list[ispec].density;
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
            state->sync_to_host();
            fortran_callback(static_cast<void*>(state.get()));
            state->sync_to_device();
            return;
        }

        int num_aerosols = state->chem.aerosol_indices.size();
        if (num_aerosols == 0)
            return;

        if (!state->met.BXHEIGHT && state->met.PEDGE && state->met.T) {
            state->derive_bxheight();
        }
        if (!state->met.AIRDEN && state->met.AIRDEN_DRY) {
            state->met.AIRDEN = state->met.AIRDEN_DRY;
        }
        if (!state->met.AIRDEN && !state->met.AIRDEN_DRY && state->met.PMID && state->met.T) {
            state->derive_airden_dry();
            state->met.AIRDEN = state->met.AIRDEN_DRY;
        }

        require_field_pointer("Settling", "T", state->met.T ? state->met.T->host_data() : nullptr);
        require_field_pointer("Settling", "AIRDEN", state->met.AIRDEN ? state->met.AIRDEN->host_data() : nullptr);
        require_field_pointer("Settling", "PEDGE", state->met.PEDGE ? state->met.PEDGE->host_data() : nullptr);
        require_field_pointer("Settling", "BXHEIGHT", state->met.BXHEIGHT ? state->met.BXHEIGHT->host_data() : nullptr);
        require_field_pointer("Settling", "CHEM_CONC", state->chem.conc ? state->chem.conc->host_data() : nullptr);

        settling::SettlingFunctor functor;
        functor.conc = state->chem.conc->view();
        functor.t = state->met.T->view();
        functor.airden = state->met.AIRDEN->view();
        functor.pedge = state->met.PEDGE->view();
        functor.dz = state->met.BXHEIGHT->view();

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

        functor.cdt = state->time.timestep;
        functor.n_levels = state->n_levels;

#ifdef CATCHEM_ENABLE_KOKKOS
        Kokkos::parallel_for("settling_compute_c++",
                             Kokkos::MDRangePolicy<Kokkos::DefaultExecutionSpace, Kokkos::Rank<2>>(
                                 {0, 0}, {state->n_cols, num_aerosols}),
                             functor);
        Kokkos::fence();
#else
        for (int icol = 0; icol < state->n_cols; ++icol)
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

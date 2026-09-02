#pragma once
#include "catchem_kokkos_compat.hpp"
#include "catchem_process_interface.hpp"
#include <functional>
#include <vector>

namespace catchem {

    class SettlingProcess : public ProcessInterface {
    private:
        std::string active_scheme;
        std::function<void(void*)> fortran_callback;

        // GOCART scheme options staged from the runtime configuration
        // (processes/settling/gocart/*).  scale_factor and correction_maring
        // act on the C++ settling kernel directly.  simple_scheme and
        // swelling_method select wet-particle Mie/swelling treatments that
        // the dry-radius C++ kernel does not implement; they are accepted
        // for configuration parity but currently have no effect there.
        double gocart_scale_factor = 1.0;
        bool gocart_simple_scheme = false;
        int gocart_swelling_method = 1;
        bool gocart_correction_maring = false;

#ifdef CATCHEM_ENABLE_KOKKOS
        // Device Views for aerosol properties
        Kokkos::View<int*, Kokkos::DefaultExecutionSpace::memory_space> dev_aero_indices;
        Kokkos::View<double*, Kokkos::DefaultExecutionSpace::memory_space> dev_radius_dry;
        Kokkos::View<double*, Kokkos::DefaultExecutionSpace::memory_space> dev_rhop_dry;
#else
        // Host-only aerosol property arrays (mdspans over these feed the kernel)
        std::vector<int> host_aero_indices;
        std::vector<double> host_radius_dry;
        std::vector<double> host_rhop_dry;
#endif

    public:
        SettlingProcess();
        std::string get_name() const override { return "settling"; }
        ProcessContract get_contract() const override;
        void prepare_inputs(std::shared_ptr<StateManager> state) override;
        void init(std::shared_ptr<StateManager> state) override;
        void run(std::shared_ptr<StateManager> state) override;
        void finalize() override;

        // For legacy tests
        void set_fortran_bridge_callback(std::function<void(void*)> cb);
    };

} // namespace catchem

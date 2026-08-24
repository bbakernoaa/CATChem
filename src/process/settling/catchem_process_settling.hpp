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
        void init(std::shared_ptr<StateManager> state) override;
        void run(std::shared_ptr<StateManager> state) override;
        void finalize() override;

        // For legacy tests
        void set_fortran_bridge_callback(std::function<void(void*)> cb);
    };

} // namespace catchem

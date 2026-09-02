#pragma once
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

        // The Fortran science bridge resolves these canonical names against
        // the full chemistry species list.  Do not pass C++ indices across
        // this language boundary: C++ is zero-based and Fortran is one-based.
        std::vector<char> aerosol_species_names;
        std::vector<double> host_radius_dry;
        std::vector<double> host_rhop_dry;

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

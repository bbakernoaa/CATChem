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
        // (processes/settling/gocart/*).  They are forwarded verbatim to the
        // Fortran science bridge, which reproduces the upstream GOCART2G
        // settling path.  scale_factor is retained for configuration parity;
        // the upstream metadata path does not consume it.  simple_scheme
        // requires Mie tables and stays unsupported (init rejects it).
        double gocart_scale_factor = 1.0;
        bool gocart_simple_scheme = false;
        double gocart_swelling_rh_max = 0.95;
        bool gocart_correction_maring = false;
        bool gocart_maring_dust_only = true;

        // The Fortran science bridge resolves these canonical names against
        // the full chemistry species list.  Do not pass C++ indices across
        // this language boundary: C++ is zero-based and Fortran is one-based.
        std::vector<char> aerosol_species_names;
        std::vector<double> host_radius_dry; // micrometres, as configured
        std::vector<double> host_rhop_dry;
        std::vector<int> host_is_dust;        // 0/1 per settling species
        std::vector<int> host_is_hydrophilic; // 0/1 per settling species (drives wet swelling)

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

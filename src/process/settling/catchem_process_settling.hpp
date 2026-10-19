#pragma once
#include "catchem_process_interface.hpp"
#include <functional>
#include <string>
#include <vector>

namespace catchem {

    /**
     * @brief Runs the GOCART aerosol settling process through its Fortran bridge.
     *
     * The instance owns its immutable Mie-table handle and all species mappings
     * used by its bridge calls.
     */
    class SettlingProcess : public ProcessInterface {
    private:
        std::string active_scheme;
        std::function<void(void*)> fortran_callback;

        // GOCART scheme options staged from the runtime configuration
        // (processes/settling/gocart/*).  They are forwarded verbatim to the
        // Fortran science bridge, which reproduces the upstream GOCART2G
        // settling path.  scale_factor is retained for configuration parity;
        // the upstream metadata path does not consume it.  simple_scheme selects
        // the optics-table (Mie) path; when true the tables named by the
        // top-level "mie:" section are loaded during init and every settling
        // species must resolve one through its __mie_name.
        double gocart_scale_factor = 1.0;
        bool gocart_simple_scheme = false;
        int gocart_swelling_method = 1;
        bool gocart_correction_maring = false;
        bool gocart_maring_dust_only = true;

        // The Fortran science bridge resolves these canonical names against
        // the full chemistry species list.  Do not pass C++ indices across
        // this language boundary: C++ is zero-based and Fortran is one-based.
        std::vector<char> aerosol_species_names;
        std::vector<double> host_radius_dry; // micrometres, as configured
        std::vector<double> host_rhop_dry;
        std::vector<int> host_is_dust; // 0/1 per settling species
        // Per settling species' __mie_name (32-byte fixed width, same packing as
        // aerosol_species_names).  The Fortran bridge maps these to loaded table
        // indices; empty means unresolved and aborts initialization on the optics path.
        std::vector<char> aerosol_mie_names;
        // Context-owned immutable Fortran table set; zero when simple_scheme is off.
        int mie_handle = 0;

        // Per-process scheme diagnostics (specs: process-diagnostics-parity).
        // diagnostics_enabled mirrors processes/settling/diagnostics;
        // diagnostic_species_id holds 1-based LOCAL positions within the
        // aerosol subset the bridge gathers (species_mie_map order), matching
        // the species_idx the scheme loops over.
        bool diagnostics_enabled = false;
        std::vector<int> diagnostic_species_id;
        std::vector<std::string> diagnostic_species_names; // parallel to ids, for run() lookups

    public:
        /** @brief Constructs a settling process with the default scheme settings. */
        SettlingProcess();
        /** @brief Releases any Mie-table context still owned by this process. */
        ~SettlingProcess() override;
        /** @brief Returns the canonical process name. */
        std::string get_name() const override { return "settling"; }
        /** @brief Describes the host fields read and written by settling. */
        ProcessContract get_contract() const override;
        /** @brief Derives required host inputs before contract validation. */
        void prepare_inputs(std::shared_ptr<StateManager> state) override;
        /** @brief Reads process options and prepares immutable species and table mappings. */
        void init(std::shared_ptr<StateManager> state) override;
        /** @brief Executes settling for the current host interval. */
        void run(std::shared_ptr<StateManager> state) override;
        /** @brief Releases resources owned by this initialized process instance. */
        void finalize() override;

        // For legacy tests
        /** @brief Installs the compatibility callback used by legacy tests. */
        void set_fortran_bridge_callback(std::function<void(void*)> cb);
    };

} // namespace catchem

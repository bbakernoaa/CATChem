// src/process/photolysis/catchem_process_photolysis.hpp
#pragma once
#include "catchem_process_interface.hpp"
#include <memory>
#include <musica/tuvx/tuvx.hpp>
#include <string>
#include <unordered_set>

namespace catchem {

    /** @class PhotolysisProcess
     *  @brief Runs MUSICA TUV-x and publishes mechanism-mapped J-rate products.
     */
    class PhotolysisProcess : public ProcessInterface {
    private:
        std::string config_path;
        musica::TUVX* tuvx_instance = nullptr;
        musica::Mappings photo_mappings;

        // Keep raw map pointers alive
        musica::GridMap* grids = nullptr;
        musica::ProfileMap* profiles = nullptr;
        musica::RadiatorMap* radiators = nullptr;

        /** @brief Register a profile only when the selected MUSICA configuration does not define it.
         *  @param state CATChem state used for diagnostics.
         *  @param config_defined_profiles Profile names supplied by the MUSICA configuration.
         *  @param name MUSICA profile name.
         *  @param units MUSICA profile units.
         *  @param grid Vertical or wavelength grid for the profile.
         *  @param default_val Fallback value used to seed the profile.
         *  @param num_vals Number of profile values.
         *  @param err MUSICA error object.
         */
        void register_profile_if_missing(const StateManager* state,
                                         const std::unordered_set<std::string>& config_defined_profiles,
                                         const char* name, const char* units, musica::Grid* grid, double default_val,
                                         std::size_t num_vals, musica::Error* err);

    public:
        /** @brief Construct an uninitialized photolysis process. */
        PhotolysisProcess();
        /** @brief Release owned MUSICA TUV-x resources. */
        ~PhotolysisProcess() override;

        /** @brief Return the registered process name. */
        std::string get_name() const override { return std::string(ProcessNames::Photolysis); }
        /** @brief Declare required met fields and the ozone role dependency. */
        ProcessContract get_contract() const override;

        /** @brief Initialize MUSICA grids, profiles, radiators, and output products.
         *  @param state CATChem state and runtime configuration.
         */
        void init(std::shared_ptr<StateManager> state) override;
        /** @brief Run TUV-x for every column and publish the resulting J-rates.
         *  @param state CATChem state with current meteorology and constituents.
         */
        void run(std::shared_ptr<StateManager> state) override;
        /** @brief Finalize the MUSICA resources owned by this process. */
        void finalize() override;
    };

} // namespace catchem

// src/process/photolysis/catchem_process_photolysis.hpp
#pragma once
#include "catchem_process_interface.hpp"
#include <musica/tuvx/tuvx.hpp>
#include <memory>
#include <string>

namespace catchem {

    class PhotolysisProcess : public ProcessInterface {
    private:
        std::string config_path;
        musica::TUVX* tuvx_instance = nullptr;
        musica::Mappings photo_mappings;

        // Keep raw map pointers alive
        musica::GridMap* grids = nullptr;
        musica::ProfileMap* profiles = nullptr;
        musica::RadiatorMap* radiators = nullptr;

    public:
        PhotolysisProcess();
        ~PhotolysisProcess() override;

        std::string get_name() const override { return "photolysis"; }

        void init(std::shared_ptr<StateManager> state) override;
        void run(std::shared_ptr<StateManager> state) override;
        void finalize() override;
    };

} // namespace catchem

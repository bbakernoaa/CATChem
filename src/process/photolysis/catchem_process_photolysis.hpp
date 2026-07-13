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
        std::unique_ptr<musica::TUVX> tuvx_instance;
        musica::Mappings photo_mappings;

    public:
        PhotolysisProcess();
        ~PhotolysisProcess() override;

        std::string get_name() const override { return "photolysis"; }

        void init(std::shared_ptr<StateManager> state) override;
        void run(std::shared_ptr<StateManager> state) override;
        void finalize() override;
    };

} // namespace catchem

// src/process/photolysis/catchem_process_photolysis.cpp
#include "catchem_process_photolysis.hpp"
#include "catchem_process_registry.hpp"
#include <iostream>

namespace catchem {

    PhotolysisProcess::PhotolysisProcess() : config_path("") {}
    PhotolysisProcess::~PhotolysisProcess() = default;

    void PhotolysisProcess::init(std::shared_ptr<StateManager> state) {
        std::cout << "DEBUG: PhotolysisProcess init" << std::endl;
    }

    void PhotolysisProcess::run(std::shared_ptr<StateManager> state) {
        std::cout << "DEBUG: PhotolysisProcess run" << std::endl;
    }

    void PhotolysisProcess::finalize() {
        std::cout << "DEBUG: PhotolysisProcess finalize" << std::endl;
    }

} // namespace catchem

extern "C" {
void catchem_register_photolysis_cpp() {
    catchem::ProcessRegistry::get_instance().register_process(
        "photolysis", []() { return std::make_shared<catchem::PhotolysisProcess>(); });
}
}

#pragma once
#include "catchem_process_interface.hpp"
#include <functional>

namespace catchem {

class WetDepProcess : public ProcessInterface {
private:
    std::string active_scheme;
    std::function<void(void*)> fortran_callback;

public:
    WetDepProcess();
    std::string get_name() const override { return "wetdep"; }
    void init(std::shared_ptr<StateManager> state) override;
    void set_fortran_bridge_callback(std::function<void(void*)> cb);
    void run(std::shared_ptr<StateManager> state) override;
    void finalize() override {}
};

} // namespace catchem

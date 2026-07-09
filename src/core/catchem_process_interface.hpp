#pragma once
#include "catchem_state_manager.hpp"
#include <memory>
#include <string>

namespace catchem {

    class ProcessInterface {
    public:
        virtual ~ProcessInterface() = default;
        virtual std::string get_name() const = 0;
        virtual void init(std::shared_ptr<StateManager> state) = 0;
        virtual void run(std::shared_ptr<StateManager> state) = 0;
        virtual void finalize() = 0;
    };

} // namespace catchem

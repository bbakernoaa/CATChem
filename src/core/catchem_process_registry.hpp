#pragma once
#include <string>
#include <memory>
#include <unordered_map>
#include <functional>
#include <stdexcept>
#include "catchem_process_interface.hpp"

namespace catchem {

using ProcessCreator = std::function<std::shared_ptr<ProcessInterface>()>;

class ProcessRegistry {
private:
    std::unordered_map<std::string, ProcessCreator> creators;

    // Singleton private constructor
    ProcessRegistry() = default;

public:
    static ProcessRegistry& get_instance() {
        static ProcessRegistry instance;
        return instance;
    }

    void register_process(const std::string& name, ProcessCreator creator) {
        creators[name] = creator;
    }

    bool has_process(const std::string& name) const {
        return creators.find(name) != creators.end();
    }

    std::shared_ptr<ProcessInterface> create(const std::string& name) {
        if (!has_process(name)) {
            throw std::invalid_argument("Process not registered in C++: " + name);
        }
        return creators.at(name)();
    }

    void clear() {
        creators.clear();
    }
};

} // namespace catchem

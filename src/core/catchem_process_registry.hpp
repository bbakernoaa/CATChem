#pragma once
#include "catchem_process_interface.hpp"
#include <functional>
#include <memory>
#include <mutex>
#include <stdexcept>
#include <string>
#include <unordered_map>

namespace catchem {

    using ProcessCreator = std::function<std::shared_ptr<ProcessInterface>()>;
    using ProcessContractFactory = std::function<ProcessContract()>;
    using ProcessSettingsValidator = std::function<void(const ProcessConfig&)>;

    struct ProcessRegistryEntry {
        ProcessCreator creator;
        ProcessContractFactory contract_factory;
        ProcessSettingsValidator settings_validator;
    };

    class ProcessRegistry {
    private:
        std::unordered_map<std::string, ProcessRegistryEntry> creators;
        mutable std::mutex mutex_;

        // Singleton private constructor
        ProcessRegistry() = default;

    public:
        static ProcessRegistry& get_instance() {
            static ProcessRegistry instance;
            return instance;
        }

        void register_process(const std::string& name, ProcessCreator creator,
                              ProcessContractFactory contract_factory = {},
                              ProcessSettingsValidator settings_validator = {}) {
            std::lock_guard<std::mutex> lock(mutex_);
            creators[name] = {std::move(creator), std::move(contract_factory), std::move(settings_validator)};
        }

        bool has_process(const std::string& name) const {
            std::lock_guard<std::mutex> lock(mutex_);
            return creators.find(name) != creators.end();
        }

        std::shared_ptr<ProcessInterface> create(const std::string& name) {
            ProcessCreator creator;
            {
                std::lock_guard<std::mutex> lock(mutex_);
                const auto found = creators.find(name);
                if (found == creators.end())
                    throw std::invalid_argument("Process not registered in C++: " + name);
                creator = found->second.creator;
            }
            return creator();
        }

        void validate_settings(const std::string& name, const ProcessConfig& settings) const {
            ProcessSettingsValidator validator;
            {
                std::lock_guard<std::mutex> lock(mutex_);
                const auto found = creators.find(name);
                if (found == creators.end()) throw std::invalid_argument("Process not registered in C++: " + name);
                validator = found->second.settings_validator;
            }
            if (validator) validator(settings);
        }

        void clear() {
            std::lock_guard<std::mutex> lock(mutex_);
            creators.clear();
        }
    };

} // namespace catchem

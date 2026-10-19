/**
 * @file catchem_process_registry.hpp
 * @brief Name-keyed factory registry for C++ process wrappers.
 *
 * @details Each scheme's wrapper registers a creator (plus an optional
 * contract factory and settings validator) under its process name, so Core
 * can instantiate configured processes from YAML strings without link-time
 * knowledge of the schemes. The registry is a locked singleton: registration
 * happens once through the weak catchem_register_*_cpp entry points, and
 * create()/validate_settings() are safe to call from any thread.
 * @see catchem_api_internal.hpp register_builtin_processes() for the bulk
 *      registration path, and src/process for the wrappers themselves.
 */
#pragma once
#include "catchem_process_interface.hpp"
#include <functional>
#include <memory>
#include <mutex>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

namespace catchem {

    /// Builds a fresh process instance; called by Core when a schedule entry is activated.
    using ProcessCreator = std::function<std::shared_ptr<ProcessInterface>()>;
    /// Produces the process's contract without constructing the full wrapper (optional).
    using ProcessContractFactory = std::function<ProcessContract()>;
    /// Rejects unknown YAML option paths for one process; throws std::invalid_argument (optional).
    using ProcessSettingsValidator = std::function<void(const ProcessConfig&)>;

    /// The three callbacks one scheme registers under its process name.
    struct ProcessRegistryEntry {
        ProcessCreator creator;                             ///< Instantiates the wrapper.
        ProcessContractFactory contract_factory;            ///< Optional contract probe without an instance.
        ProcessSettingsValidator settings_validator;        ///< Optional strict-option checker.
    };

    /**
     * @brief Thread-safe singleton mapping process names to their factories.
     *
     * @details All public methods lock mutex_, so registration during library
     * load and creation during Core initialization can interleave safely.
     * Entries persist for the process lifetime except through clear(), which
     * tests use; production code should not clear a populated registry.
     */
    class ProcessRegistry {
    private:
        std::unordered_map<std::string, ProcessRegistryEntry> creators;
        mutable std::mutex mutex_;

        // Singleton private constructor
        ProcessRegistry() = default;

    public:
        /// The one registry instance for the process.
        static ProcessRegistry& get_instance() {
            static ProcessRegistry instance;
            return instance;
        }

        /**
         * @brief Register (or replace) the factories for one process name.
         * @param name              Schedule name as it appears in run_phases,
         *                          e.g. "dust".
         * @param creator           Factory producing the wrapper instance.
         * @param contract_factory  Optional contract probe; empty when unused.
         * @param settings_validator Optional strict-option checker; empty means
         *                          no option validation for this process.
         * @note Re-registration overwrites the previous entry silently; the
         *       once_flag in register_builtin_processes() keeps that from
         *       happening twice in production.
         */
        void register_process(const std::string& name, ProcessCreator creator,
                              ProcessContractFactory contract_factory = {},
                              ProcessSettingsValidator settings_validator = {}) {
            std::lock_guard<std::mutex> lock(mutex_);
            creators[name] = {std::move(creator), std::move(contract_factory), std::move(settings_validator)};
        }

        /// True when a creator is registered under @p name.
        bool has_process(const std::string& name) const {
            std::lock_guard<std::mutex> lock(mutex_);
            return creators.find(name) != creators.end();
        }

        /**
         * @brief Instantiate the process registered under @p name.
         * @param name Schedule name of a registered process.
         * @return Fresh, not yet initialized wrapper instance; the caller owns it
         *         (Core holds it in its schedule until shutdown finalizes it).
         * @throws std::invalid_argument when @p name is not registered.
         * @note The creator runs outside the registry lock so a wrapper
         *       constructor may safely query the registry.
         */
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

        /**
         * @brief Run the registered validator for @p name over its YAML settings.
         * @param name     Schedule name of a registered process.
         * @param settings The process's parsed options block.
         * @throws std::invalid_argument when @p name is unregistered, or from
         *         the validator itself (e.g. make_settings_validator() on an
         *         unknown option path).
         */
        void validate_settings(const std::string& name, const ProcessConfig& settings) const {
            ProcessSettingsValidator validator;
            {
                std::lock_guard<std::mutex> lock(mutex_);
                const auto found = creators.find(name);
                if (found == creators.end())
                    throw std::invalid_argument("Process not registered in C++: " + name);
                validator = found->second.settings_validator;
            }
            if (validator)
                validator(settings);
        }

        /// Drop every registration; intended for test isolation, not production.
        void clear() {
            std::lock_guard<std::mutex> lock(mutex_);
            creators.clear();
        }
    };

    /**
     * @brief Build a settings validator that rejects unknown scheme options.
     *
     * Each process registers this alongside its creator so that every nested
     * `processes/<name>/<scheme>/<key>` entry in the runtime YAML must appear
     * in `accepted_paths`.  A misspelled or removed option therefore fails at
     * initialization with the accepted schema listed, instead of silently
     * leaving the scheme on its compiled default.  Paths use the same
     * slash-separated form as ProcessConfig::get_* (for example
     * "fengsha/alpha").
     */
    inline ProcessSettingsValidator make_settings_validator(std::string process_name,
                                                            std::vector<std::string> accepted_paths) {
        std::set<std::string> accepted(accepted_paths.begin(), accepted_paths.end());
        return [process_name = std::move(process_name), accepted = std::move(accepted)](const ProcessConfig& settings) {
            std::vector<std::string> unknown;
            for (const auto& path : settings.option_paths()) {
                if (!accepted.count(path))
                    unknown.push_back(path);
            }
            if (unknown.empty())
                return;
            std::ostringstream message;
            message << "Unknown option(s) for process '" << process_name << "':";
            for (const auto& key : unknown)
                message << " " << key << ",";
            message.seekp(-1, std::ios_base::end); // drop trailing comma
            message << ". Accepted options:";
            for (const auto& key : accepted)
                message << " " << key << ",";
            message.seekp(-1, std::ios_base::end);
            message << '.';
            throw std::invalid_argument(message.str());
        };
    }

} // namespace catchem

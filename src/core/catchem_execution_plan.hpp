#pragma once

#include "catchem_process_interface.hpp"
#include "catchem_validation.hpp"
#include <memory>
#include <cstdint>
#include <string>
#include <unordered_map>
#include <vector>

namespace catchem {

    /** @brief Compiled contracts and per-interval provenance for active processes. */
    class ExecutionPlan {
    public:
        /** @brief Provenance attached to a successfully published derived product. */
        struct ProductProvenance {
            std::string producer;
            std::string units;
            std::string mechanism_identity;
            std::uint64_t interval = 0;
            std::size_t import_generation = 0;
            bool valid = false;
        };
        /** @brief Validate active process contracts and build product indexes. */
        void compile(const std::vector<std::shared_ptr<ProcessInterface>>& processes,
                     const MechanismDefinition* mechanism);
        /** @brief Return compilation issues for the active schedule. */
        const ValidationIssueReport& validation() const noexcept { return validation_; }
        /** @brief Return the immutable compiled contract at an active index. */
        const ProcessContract& contract(std::size_t index) const { return contracts_.at(index); }
        /** @brief Return the number of active contracts. */
        std::size_t size() const noexcept { return contracts_.size(); }
        /** @brief Prepare input fields before the selected process runs. */
        void prepare(std::size_t index, StateManager& state) const;
        /** @brief Complete host/device bookkeeping after a process runs. */
        void complete(std::size_t index, StateManager& state) const;
        /** @brief Mark declared products current for a successful invocation. */
        void publish_products(std::size_t index, const std::string& mechanism_identity,
                              std::uint64_t interval, std::size_t import_generation);
        /** @brief Invalidate products declared by one process. */
        void invalidate_products(std::size_t index) noexcept;
        /** @brief Invalidate every product in the active schedule. */
        void invalidate_all_products() noexcept;
        /** @brief Check product provenance against mechanism, interval, and import generation. */
        bool product_is_current(const std::string& name, const std::string& mechanism_identity,
                                std::uint64_t interval, std::size_t import_generation) const;
        /** @brief Return product provenance, or null when the schedule has no producer. */
        const ProductProvenance* product_provenance(const std::string& name) const noexcept;

    private:
        std::vector<ProcessContract> contracts_;
        ValidationIssueReport validation_;
        std::unordered_map<std::string, ProductProvenance> products_;
    };

} // namespace catchem

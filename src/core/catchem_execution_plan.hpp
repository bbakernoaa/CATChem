/**
 * @file catchem_execution_plan.hpp
 * @brief Compiled process-contract schedule with derived-product provenance.
 *
 * @details The plan is rebuilt by Core::add_process() after every schedule
 * change: compile() validates each ProcessContract, cross-checks mechanism
 * roles/capabilities, detects conflicting producers, missing product
 * producers and dependency-order violations, and records everything as a
 * ValidationIssueReport instead of throwing. Per-invocation it drives the
 * field-access bookkeeping (prepare/complete) and tracks whether each
 * declared derived product is current for a given mechanism fingerprint,
 * host interval and import generation.
 */
#pragma once

#include "catchem_process_interface.hpp"
#include "catchem_validation.hpp"
#include <memory>
#include <cstdint>
#include <string>
#include <unordered_map>
#include <vector>

namespace catchem {

    /**
     * @brief Compiled contracts and per-interval provenance for active processes.
     *
     * @details Owned by Core as a value member; not thread-safe on its own —
     * Core's lifecycle mutex serialises compile/publish/invalidate against
     * execution reads. Contracts are immutable between compiles; product
     * provenance mutates per invocation.
     */
    class ExecutionPlan {
    public:
        /** @brief Provenance attached to a successfully published derived product. */
        struct ProductProvenance {
            std::string producer;             ///< Process that declared the product.
            std::string units;                ///< Declared units of the product value.
            std::string mechanism_identity;   ///< Mechanism fingerprint at publication; empty until published.
            std::uint64_t interval = 0;       ///< Interval identity (host id, or Core step counter) at publication.
            std::size_t import_generation = 0; ///< State import generation the inputs belonged to.
            bool valid = false;               ///< False after any invalidation until the producer republishes.
        };
        /**
         * @brief Validate active process contracts and build product indexes.
         * @param processes  The full scheduled process list, in execution order.
         * @param mechanism  Active mechanism definition, or null to skip role and
         *                   capability checks.
         * @note Rebuilds from scratch (previous contracts, products and issues
         *       are discarded). Problems are collected in validation(), never
         *       thrown; the caller (Core::add_process) rejects the schedule when
         *       validation().has_errors() is true.
         */
        void compile(const std::vector<std::shared_ptr<ProcessInterface>>& processes,
                     const MechanismDefinition* mechanism);
        /** @brief Return compilation issues for the active schedule. */
        const ValidationIssueReport& validation() const noexcept { return validation_; }
        /** @brief Return the immutable compiled contract at an active index. */
        const ProcessContract& contract(std::size_t index) const { return contracts_.at(index); }
        /** @brief Return the number of active contracts. */
        std::size_t size() const noexcept { return contracts_.size(); }
        /**
         * @brief Prepare input fields before the selected process runs.
         * @param index 0-based contract index; must be below size().
         * @param state StateManager whose field registry resolves the accesses.
         * @throws std::runtime_error naming the process and field when a
         *         Required read field is not resolvable in @p state.
         */
        void prepare(std::size_t index, StateManager& state) const;
        /**
         * @brief Complete host/device bookkeeping after a process runs.
         * @param index 0-based contract index; must be below size().
         * @param state StateManager owning the accessed fields.
         */
        void complete(std::size_t index, StateManager& state) const;
        /**
         * @brief Mark declared products current after a successful invocation.
         * @param index              0-based contract index of the producing process.
         * @param mechanism_identity Mechanism fingerprint to stamp on the products.
         * @param interval           Interval identity the publication belongs to.
         * @param import_generation  State import generation of the inputs used.
         * @note Call only after complete() succeeded; Core invalidates all
         *       products instead when the process throws.
         */
        void publish_products(std::size_t index, const std::string& mechanism_identity,
                              std::uint64_t interval, std::size_t import_generation);
        /**
         * @brief Invalidate products declared by one process.
         * @param index 0-based contract index; an out-of-range index is ignored.
         */
        void invalidate_products(std::size_t index) noexcept;
        /**
         * @brief Invalidate every product in the active schedule.
         * @note Used after any process failure: with state possibly half
         *       updated, no downstream product can be trusted as current.
         */
        void invalidate_all_products() noexcept;
        /**
         * @brief Check product provenance against mechanism, interval, and import generation.
         * @param name               Product name as declared in a contract.
         * @param mechanism_identity Fingerprint the consumer expects.
         * @param interval           Interval identity the consumer is executing in.
         * @param import_generation  Import generation the consumer's inputs came from.
         * @return true only when a producer exists and its valid provenance
         *         matches all three; false for unknown names or stale data.
         */
        bool product_is_current(const std::string& name, const std::string& mechanism_identity,
                                std::uint64_t interval, std::size_t import_generation) const;
        /**
         * @brief Return product provenance, or null when the schedule has no producer.
         * @param name Product name as declared in a contract.
         * @return Borrowed pointer into the plan's map; invalidated by the next
         *         compile(). The valid flag, not the pointer, says whether the
         *         product data may be used.
         */
        const ProductProvenance* product_provenance(const std::string& name) const noexcept;

    private:
        std::vector<ProcessContract> contracts_;                  ///< Compiled contracts in execution order.
        ValidationIssueReport validation_;                        ///< Issues found by the last compile().
        std::unordered_map<std::string, ProductProvenance> products_; ///< Product name to latest provenance.
    };

} // namespace catchem

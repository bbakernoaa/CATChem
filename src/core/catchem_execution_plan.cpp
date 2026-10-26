/**
 * @file catchem_execution_plan.cpp
 * @brief Contract compilation and product-provenance bookkeeping for ExecutionPlan.
 *
 * @details compile() runs five ordered checks over the schedule: structural
 * contract validity, mechanism role/capability requirements, conflicting
 * producers of the same shared field, product producer/units agreement,
 * required-product availability, and producer-before-consumer ordering.
 * All findings go into the ValidationIssueReport; only prepare() throws, and
 * only when a Required read field cannot be resolved against the StateManager.
 * Member documentation lives on catchem_execution_plan.hpp.
 */
#include "catchem_execution_plan.hpp"
#include <stdexcept>
#include <unordered_map>

namespace catchem {
    namespace {
        void hash_bytes(std::uint64_t& hash, const void* data, std::size_t size) {
            const auto* bytes = static_cast<const unsigned char*>(data);
            for (std::size_t i = 0; i < size; ++i) { hash ^= bytes[i]; hash *= 1099511628211ULL; }
        }

        template <int Rank>
        void hash_field(std::uint64_t& hash, StateManager& state, const std::string& name) {
            auto field = state.find_field<Rank>(name);
            const bool present = static_cast<bool>(field);
            hash_bytes(hash, &present, sizeof(present));
            if (!field) return;
            for (const auto extent : field->immutable_extents) hash_bytes(hash, &extent, sizeof(extent));
            const auto count = [&] { std::size_t n = 1; for (auto e : field->immutable_extents) n *= e; return n; }();
            const auto* values = field->host_read();
            hash_bytes(hash, values, count * sizeof(double));
        }
    }

    void ExecutionPlan::compile(const std::vector<std::shared_ptr<ProcessInterface>>& processes,
                                const MechanismDefinition* mechanism) {
        contracts_.clear();
        products_.clear();
        validation_ = {};
        std::unordered_map<std::string, std::pair<std::size_t, FieldAccessContract>> produced;
        for (std::size_t process_index = 0; process_index < processes.size(); ++process_index) {
            const auto& process = processes[process_index];
            auto contract = process->get_contract();
            if (contract.process_name.empty())
                contract.process_name = process->get_name();
            if (!contract.structurally_valid())
                validation_.add({IssueSeverity::Error, "process-contract", contract.process_name,
                                 "process contract is structurally incomplete", "declare identity, units, and axes"});
            if (mechanism) {
                for (const auto& requirement : contract.mechanism_requirements) {
                    if (!requirement.role.empty() && requirement.required && !mechanism->has_role(requirement.role))
                        validation_.add({IssueSeverity::Error, "mechanism-role", contract.process_name,
                                         "missing required role " + requirement.role, "map the role in the mechanism"});
                    if (!requirement.capability.empty() && requirement.required &&
                        !mechanism->has_capability(requirement.capability))
                        validation_.add({IssueSeverity::Error, "mechanism-capability", contract.process_name,
                                         "missing required capability " + requirement.capability,
                                         "declare capability in the mechanism"});
                }
            }
            for (const auto& field : contract.fields) {
                const auto key = canonicalize_field_identity(field.canonical_name);
                if (field.produced) {
                    const auto prior = produced.find(key);
                    if (prior != produced.end() &&
                        (prior->second.second.units != field.units || prior->second.second.axes != field.axes))
                        validation_.add({IssueSeverity::Error, "incompatible-producers", contract.process_name,
                                         "output " + key + " conflicts with an earlier producer",
                                         "use one units/rank/axis contract for the shared output"});
                    else
                        produced.emplace(key, std::make_pair(process_index, field));
                }
            }
            contracts_.push_back(std::move(contract));
        }

        for (const auto& contract : contracts_) {
            for (const auto& declaration : contract.produced_products) {
                auto [it, inserted] = products_.emplace(declaration.name, ProductProvenance{
                    contract.process_name, declaration.units, {}, 0, 0, 0, declaration.input_versioned, {}, false});
                if (!inserted && (it->second.producer != contract.process_name ||
                                  it->second.units != declaration.units))
                    validation_.add({IssueSeverity::Error, "incompatible-products", contract.process_name,
                                     "derived product " + declaration.name + " has conflicting producers or units",
                                     "declare each product with one producer and one units contract"});
            }
        }

        for (const auto& contract : contracts_) {
            for (const auto& requirement : contract.products) {
                if (!requirement.required) continue;
                const auto producer = products_.find(requirement.name);
                if (producer == products_.end()) {
                    validation_.add({IssueSeverity::Error, "missing-product-producer", contract.process_name,
                                     "required derived product " + requirement.name + " has no active producer",
                                     "enable its producer before the consumer or remove the PHOTO dependency"});
                } else if (!requirement.producer.empty() && producer->second.producer != requirement.producer) {
                    validation_.add({IssueSeverity::Error, "product-producer-mismatch", contract.process_name,
                                     "required derived product " + requirement.name + " is produced by " +
                                         producer->second.producer + " instead of " + requirement.producer,
                                     "declare the configured product producer in the active schedule"});
                }
            }
        }

        for (std::size_t consumer_index = 0; consumer_index < contracts_.size(); ++consumer_index) {
            for (const auto& field : contracts_[consumer_index].fields) {
                if (!field.reads() || field.requirement != FieldRequirement::Required)
                    continue;
                const auto producer = produced.find(canonicalize_field_identity(field.canonical_name));
                if (producer != produced.end() && producer->second.first > consumer_index)
                    validation_.add({IssueSeverity::Error, "dependency-order", contracts_[consumer_index].process_name,
                                     "required field " + canonicalize_field_identity(field.canonical_name) +
                                         " is produced later in the schedule",
                                     "move its producer before this consumer in active_processes"});
            }
        }
    }

    void ExecutionPlan::prepare(std::size_t index, StateManager& state) const {
        const auto& process = contracts_.at(index);
        for (const auto& access : process.fields) {
            const bool found = state.prepare_field_access(access);
            if (!found && access.requirement == FieldRequirement::Required)
                throw std::runtime_error("Process " + process.process_name + " missing required field " +
                                         access.canonical_name);
        }
    }

    void ExecutionPlan::complete(std::size_t index, StateManager& state) const {
        for (const auto& access : contracts_.at(index).fields)
            state.complete_field_access(access);
    }

    void ExecutionPlan::publish_products(std::size_t index, const std::string& mechanism_identity,
                                         std::uint64_t interval, std::size_t import_generation,
                                         std::uint64_t fingerprint,
                                         const std::unordered_map<std::string, std::uint64_t>& input_fields) {
        const auto& process = contracts_.at(index);
        for (const auto& declaration : process.produced_products) {
            auto& product = products_.at(declaration.name);
            product.input_versioned = declaration.input_versioned;
            product.input_fingerprint = fingerprint;
            product.input_field_fingerprints = input_fields;
            product.mechanism_identity = mechanism_identity;
            product.interval = interval;
            product.import_generation = import_generation;
            product.valid = true;
        }
    }

    void ExecutionPlan::invalidate_products(std::size_t index) noexcept {
        if (index >= contracts_.size()) return;
        for (const auto& declaration : contracts_[index].produced_products) {
            auto product = products_.find(declaration.name);
            if (product != products_.end()) product->second.valid = false;
        }
    }

    void ExecutionPlan::invalidate_all_products() noexcept {
        for (auto& [name, product] : products_) product.valid = false;
    }

    bool ExecutionPlan::product_is_current(const std::string& name, const std::string& mechanism_identity,
                                           std::uint64_t interval, std::size_t import_generation,
                                           std::uint64_t fingerprint) const {
        const auto* product = product_provenance(name);
        return product && product->valid && product->mechanism_identity == mechanism_identity &&
               product->interval == interval &&
               (product->input_versioned ? product->input_fingerprint == fingerprint
                                         : product->import_generation == import_generation);
    }

    std::uint64_t ExecutionPlan::input_fingerprint(std::size_t index, StateManager& state) const {
        const auto fields = input_field_fingerprints(index, state);
        std::uint64_t hash = 14695981039346656037ULL;
        const auto& process = contracts_.at(index);
        for (const auto& access : process.fields) {
            if (!access.reads()) continue;
            const auto found = fields.find(access.canonical_name);
            if (found != fields.end()) hash_bytes(hash, &found->second, sizeof(found->second));
        }
        const auto clock = fields.find("__clock");
        if (clock != fields.end()) hash_bytes(hash, &clock->second, sizeof(clock->second));
        return hash;
    }

    std::unordered_map<std::string, std::uint64_t> ExecutionPlan::input_field_fingerprints(
        std::size_t index, StateManager& state) const {
        std::unordered_map<std::string, std::uint64_t> fields;
        const auto& process = contracts_.at(index);
        for (const auto& access : process.fields) {
            if (!access.reads()) continue;
            std::uint64_t hash = 14695981039346656037ULL;
            hash_bytes(hash, access.canonical_name.data(), access.canonical_name.size());
            const auto rank = access.axes.size();
            if (canonicalize_field_identity(access.canonical_name) == "CONCENTRATION") {
                const auto& field = state.chemistry().conc;
                const bool present = static_cast<bool>(field);
                hash_bytes(hash, &present, sizeof(present));
                if (field) {
                    const auto* values = field->host_read();
                    const auto mechanism = state.chemistry().mechanism;
                    std::size_t hashed_species = 0;
                    for (const auto& requirement : process.mechanism_requirements) {
                        if (requirement.role.empty() || !mechanism || !mechanism->has_role(requirement.role)) continue;
                        const auto species = mechanism->index_for_role(requirement.role);
                        if (species >= field->immutable_extents[2]) continue;
                        hash_bytes(hash, &species, sizeof(species));
                        ++hashed_species;
                        for (std::size_t level = 0; level < field->immutable_extents[1]; ++level)
                            for (std::size_t column = 0; column < field->immutable_extents[0]; ++column) {
                                const auto offset = column + field->immutable_extents[0] *
                                    (level + field->immutable_extents[1] * species);
                                hash_bytes(hash, values + offset, sizeof(double));
                            }
                    }
                    if (hashed_species == 0) {
                        for (const auto extent : field->immutable_extents) hash_bytes(hash, &extent, sizeof(extent));
                        std::size_t count = 1;
                        for (const auto extent : field->immutable_extents) count *= extent;
                        hash_bytes(hash, values, count * sizeof(double));
                    }
                }
            } else if (rank == 1) hash_field<1>(hash, state, access.canonical_name);
            else if (rank == 2) hash_field<2>(hash, state, access.canonical_name);
            else if (rank == 3) hash_field<3>(hash, state, access.canonical_name);
            fields[access.canonical_name] = hash;
        }
        std::uint64_t hash = 14695981039346656037ULL;
        const auto& clock = state.clock();
        hash_bytes(hash, &clock.year, sizeof(clock.year)); hash_bytes(hash, &clock.month, sizeof(clock.month));
        hash_bytes(hash, &clock.day, sizeof(clock.day)); hash_bytes(hash, &clock.hour, sizeof(clock.hour));
        hash_bytes(hash, &clock.minute, sizeof(clock.minute)); hash_bytes(hash, &clock.second, sizeof(clock.second));
        hash_bytes(hash, &clock.timestep, sizeof(clock.timestep));
        fields["__clock"] = hash;
        return fields;
    }

    std::uint64_t ExecutionPlan::product_input_fingerprint(const std::string& name, StateManager& state) const {
        const auto* product = product_provenance(name);
        if (!product) return 0;
        for (std::size_t i = 0; i < contracts_.size(); ++i)
            if (contracts_[i].process_name == product->producer) return input_fingerprint(i, state);
        return 0;
    }

    std::unordered_map<std::string, std::uint64_t> ExecutionPlan::product_input_field_fingerprints(
        const std::string& name, StateManager& state) const {
        const auto* product = product_provenance(name);
        if (!product) return {};
        for (std::size_t i = 0; i < contracts_.size(); ++i)
            if (contracts_[i].process_name == product->producer) return input_field_fingerprints(i, state);
        return {};
    }

    const ExecutionPlan::ProductProvenance* ExecutionPlan::product_provenance(const std::string& name) const noexcept {
        const auto product = products_.find(name);
        return product == products_.end() ? nullptr : &product->second;
    }

} // namespace catchem

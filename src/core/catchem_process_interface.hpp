/**
 * @file catchem_process_interface.hpp
 * @brief Process lifecycle interface and contract vocabulary for CATChem schemes.
 *
 * @details This header defines what a physics process must expose (the
 * ProcessInterface lifecycle) and what it promises (the ProcessContract data
 * structs) so Core can schedule, validate and execute it without knowing the
 * scheme. The lifecycle order enforced by Core is: register in the
 * ProcessRegistry -> init(state) at schedule time -> per step: prepare_inputs
 * -> contract field preparation (ExecutionPlan::prepare) -> run -> field
 * bookkeeping (ExecutionPlan::complete) -> product publication; and
 * finalize() exactly once in reverse order at shutdown.
 *
 * Field, axis and access types come from catchem_field_contract.hpp
 * (FieldAccessContract, SemanticAxis, AccessIntent, ...); diagnostics from
 * catchem_diagnostic_manager.hpp. The C boundary serialises these contracts
 * into catchem_process_contract_v1/catchem_process_field_v1 descriptors.
 */
#pragma once
#include "catchem_state_manager.hpp"
#include <cstdint>
#include <memory>
#include <string>
#include <string_view>
#include <vector>

namespace catchem {

    /// Progress of the Core through one timestep or selected-process interval.
    enum class TimestepStatus { NotStarted, Running, Succeeded, PartialUpdate, ValidationFailed };
    /// What the host must do before the Core will execute again after a failure.
    enum class StateClassification { Reusable, RequiresReimport, RequiresReinitialize };

    /**
     * @brief Outcome of the most recent timestep or selected-process run.
     *
     * @details Core writes this before, during and after each step; the flat C
     * API copies it out field by field via
     * catchem_core_get_timestep_outcome(). A PartialUpdate status always
     * taints the Core, and @p state says how to recover.
     */
    struct TimestepOutcome {
        TimestepStatus status = TimestepStatus::NotStarted; ///< Where the step got to.
        std::size_t timestep = 0;                           ///< Monotonic Core step ordinal.
        double duration = 0.0;                              ///< Step/interval duration [s].
        std::size_t import_generation = 0;                  ///< State import generation the step executed against.
        std::string process_name;                           ///< Failing process when PartialUpdate; else empty.
        std::size_t process_index = 0;                      ///< Schedule index of the failing process.
        std::string cause;                                  ///< Failure text (what()) or validation reason.
        StateClassification state = StateClassification::Reusable; ///< Recovery the host owes after this result.
    };

    /// Canonical schedule names of the bundled processes, matching YAML
    /// run_phases entries and ProcessRegistry keys.
    struct ProcessNames {
        static constexpr std::string_view GasChem = "gaschem";       ///< Gas-phase chemistry scheme.
        static constexpr std::string_view Photolysis = "photolysis"; ///< Photolysis rate production.
        static constexpr std::string_view Settling = "settling";     ///< Gravitational settling.
        static constexpr std::string_view Dust = "dust";             ///< Mineral dust emission.
        static constexpr std::string_view SeaSalt = "seasalt";       ///< Sea-salt emission.
        static constexpr std::string_view CarbChem = "carbchem";     ///< Carbon-bromine chemistry scheme.
        static constexpr std::string_view SO4Chem = "so4chem";       ///< Sulfate chemistry scheme.
    };

    /**
     * @brief One mechanism role or capability a process needs from the
     *        mechanism definition. Checked by ExecutionPlan::compile().
     */
    struct MechanismRequirement {
        std::string role;       ///< Species role the process looks up; empty when unused.
        std::string capability; ///< Mechanism capability flag; empty when unused.
        bool required = true;   ///< When false, absence is not a validation error.
    };

    /**
     * @brief A diagnostic output a process declares, registered with the
     *        DiagnosticManager when the process initializes.
     */
    struct DiagnosticDeclaration {
        std::string canonical_name;                                  ///< Field name, matched case-insensitively.
        std::string units;                                           ///< CF-style units string, e.g. "kg m-2 s-1".
        std::vector<SemanticAxis> axes;                              ///< One axis per dimension; axis 0 must be Column.
        PersistencePolicy persistence = PersistencePolicy::Timestep; ///< Reset each timestep, or persist.
    };

    /**
     * @brief A derived product a process consumes, with its expected producer.
     */
    struct ProductRequirement {
        std::string name;     ///< Product name, e.g. a photolysis rate set.
        std::string producer; ///< Process that must publish it; required when required is true.
        bool required = true; ///< When false, an absent producer is not a validation error.
    };

    /** @brief A derived product a process publishes to later consumers. */
    struct ProductDeclaration {
        std::string name;             ///< Product name; must be unique per producer/units pair.
        std::string units;            ///< Units the producer guarantees.
        bool input_versioned = false; ///< Product freshness follows producer read values, not import sequence alone.
    };

    /**
     * @brief How one species class is represented natively inside a process,
     *        for cross-scheme unit reconciliation.
     */
    struct ProcessSpeciesRepresentation {
        std::string species_role;                         ///< Mechanism role selecting the species set.
        std::string units;                                ///< Units the process reads/writes for those species.
        AmountType amount_type = AmountType::Unspecified; ///< Mixing-ratio basis; must be specified.
        AirBasis air_basis = AirBasis::Unspecified;       ///< Dry/moist basis; must be specified.
        std::string denominator_recipe;                   ///< Named recipe for the air-mass denominator.
    };

    /// What the host owes the Core after this process fails mid-run.
    enum class RecoveryCapability { Reconstructible, RequiresReimport, RequiresReinitialize };

    /**
     * @brief Host-supplied identity of one selected-process invocation.
     * @details The tuple (interval, site, subcycle, process) must be unique
     * within an interval; repeating it is rejected by SelectedProcessLedger.
     */
    struct SelectedInvocationKey {
        std::uint64_t interval = 0; ///< Must equal the open host interval id.
        std::string site;           ///< Host grid-tile or column identifier; non-empty.
        std::uint32_t subcycle = 0; ///< Host sub-cycle ordinal within the interval.
        std::string process;        ///< Schedule name of the process to run; non-empty.
    };

    /// Result of admitting (or refusing) one selected-process invocation.
    enum class SelectedOperationStatus { Succeeded, Rejected, Failed };
    /**
     * @brief Outcome returned by SelectedProcessLedger::run() and Core::run_selected_process().
     */
    struct SelectedOperationOutcome {
        /// Succeeded: the body ran; Rejected: never admitted; Failed: the body threw.
        SelectedOperationStatus status = SelectedOperationStatus::Rejected;
        std::string process;                 ///< Echoed invocation process name.
        std::string cause;                   ///< Rejection or failure reason; empty on success.
        std::uint64_t import_generation = 0; ///< Import generation at execution; freshness key.
        bool publication_eligible = false;   ///< True only for Succeeded; host may publish.
    };

    // Build instances with make_contract() below rather than positional braces,
    // so adding a member here never silently changes call sites.
    /**
     * @brief Everything Core knows about one process before it runs.
     *
     * @details Compiled into ExecutionPlan at schedule time; a contract that
     * fails structurally_valid() or whose requirements cannot be satisfied is
     * a validation error that rejects the whole schedule. Instances are
     * value-copied into the plan, so processes may rebuild theirs freely.
     * @see make_contract() for the recommended construction path.
     */
    struct ProcessContract {
        std::string process_name;                ///< Schedule name; must be non-empty.
        std::vector<FieldAccessContract> fields; ///< Fields read/written; need name, units, axes.
        /// Roles/capabilities the mechanism must supply.
        std::vector<MechanismRequirement> mechanism_requirements;
        std::vector<DiagnosticDeclaration> diagnostics;    ///< Diagnostics registered at init.
        std::vector<ProductRequirement> products;          ///< Products consumed; required ones name a producer.
        std::vector<ProductDeclaration> produced_products; ///< Products published by this process.
        /// Species representations read natively by this process.
        std::vector<ProcessSpeciesRepresentation> native_representations;
        std::string concentration_amount_type = "species-native"; ///< Amount-type label for reconciliation.
        /// Named air-mass denominator recipe.
        std::string denominator_recipe = "configured-species-representation";
        /// Host recovery owed if this process fails mid-run.
        RecoveryCapability recovery = RecoveryCapability::RequiresReinitialize;
        /**
         * @return true when the process name, all field/diagnostic/product/
         *         representation entries, and required product producers are
         *         populated. A false result makes ExecutionPlan::compile() emit
         *         a "process-contract" error and Core rejects the schedule.
         */
        bool structurally_valid() const noexcept {
            if (process_name.empty())
                return false;
            for (const auto& field : fields)
                if (field.canonical_name.empty() || field.units.empty() || field.axes.empty())
                    return false;
            for (const auto& diagnostic : diagnostics)
                if (diagnostic.canonical_name.empty() || diagnostic.units.empty() || diagnostic.axes.empty())
                    return false;
            for (const auto& product : products)
                if (product.name.empty() || (product.required && product.producer.empty()))
                    return false;
            for (const auto& product : produced_products)
                if (product.name.empty() || product.units.empty())
                    return false;
            for (const auto& representation : native_representations)
                if (representation.species_role.empty() || representation.units.empty() ||
                    representation.amount_type == AmountType::Unspecified || representation.denominator_recipe.empty())
                    return false;
            return true;
        }
    };

    /**
     * @brief Build a 3D host field access (Column, Level, Singleton).
     * @param name         Canonical field name; matched case-insensitively.
     * @param units        Units string, e.g. "K" or "kg m-2 s-1".
     * @param requirement  Required (default) or Optional for schedule validation.
     * @param access       Read (default), Write, or ReadWrite intent.
     * @return FieldAccessContract pinned to Host execution space and Timestep
     *         persistence, ready to move into a ProcessContract.
     */
    inline FieldAccessContract host_field_3d(std::string name, std::string units,
                                             FieldRequirement requirement = FieldRequirement::Required,
                                             AccessIntent access = AccessIntent::Read) {
        return {std::move(name),
                std::move(units),
                {SemanticAxis::Column, SemanticAxis::Level, SemanticAxis::Singleton},
                PersistencePolicy::Timestep,
                requirement,
                access,
                ExecutionSpaceIntent::Host};
    }

    /**
     * @brief Build a host field access on model interfaces (Column, Interface, Singleton).
     * @param name         Canonical field name.
     * @param units        Units string.
     * @param requirement  Required (default) or Optional.
     * @param access       Read (default), Write, or ReadWrite intent.
     * @return FieldAccessContract whose vertical axis has level_count()+1 slots.
     */
    inline FieldAccessContract host_field_interface(std::string name, std::string units,
                                                    FieldRequirement requirement = FieldRequirement::Required,
                                                    AccessIntent access = AccessIntent::Read) {
        return {std::move(name),
                std::move(units),
                {SemanticAxis::Column, SemanticAxis::Interface, SemanticAxis::Singleton},
                PersistencePolicy::Timestep,
                requirement,
                access,
                ExecutionSpaceIntent::Host};
    }

    /**
     * @brief Build a host field access on soil layers (Column, SoilLayer, Singleton).
     * @param name         Canonical field name.
     * @param units        Units string.
     * @param requirement  Required (default) or Optional.
     * @param access       Read (default), Write, or ReadWrite intent.
     * @return FieldAccessContract for the land-surface layer dimension.
     */
    inline FieldAccessContract host_field_soil_layer(std::string name, std::string units,
                                                     FieldRequirement requirement = FieldRequirement::Required,
                                                     AccessIntent access = AccessIntent::Read) {
        return {std::move(name),
                std::move(units),
                {SemanticAxis::Column, SemanticAxis::SoilLayer, SemanticAxis::Singleton},
                PersistencePolicy::Timestep,
                requirement,
                access,
                ExecutionSpaceIntent::Host};
    }

    /**
     * @brief Build a 2D column-only host field access (Column, Singleton).
     * @param name         Canonical field name.
     * @param units        Units string.
     * @param requirement  Required (default) or Optional.
     * @param access       Read (default), Write, or ReadWrite intent.
     * @param persistence  Timestep (default) or Persistent across steps.
     * @return FieldAccessContract with no vertical axis, e.g. surface fluxes.
     */
    inline FieldAccessContract host_field_2d(std::string name, std::string units,
                                             FieldRequirement requirement = FieldRequirement::Required,
                                             AccessIntent access = AccessIntent::Read,
                                             PersistencePolicy persistence = PersistencePolicy::Timestep) {
        return {std::move(name),
                std::move(units),
                {SemanticAxis::Column, SemanticAxis::Singleton},
                persistence,
                requirement,
                access,
                ExecutionSpaceIntent::Host};
    }

    /**
     * @brief Build the canonical CONCENTRATION access (Column, Level, Species).
     * @param access ReadWrite by default; pass Read for a diagnostic-only consumer.
     * @return Persistent FieldAccessContract with species-native units: the
     *         unified multi-species array bound by
     *         catchem_state_bind_unified_chemistry().
     */
    inline FieldAccessContract host_concentration(AccessIntent access = AccessIntent::ReadWrite) {
        return {"CONCENTRATION",
                "species-native",
                {SemanticAxis::Column, SemanticAxis::Level, SemanticAxis::Species},
                PersistencePolicy::Persistent,
                FieldRequirement::Required,
                access,
                ExecutionSpaceIntent::Host};
    }

    // Named constructor for ProcessContract. Members are assigned individually,
    // so a member added to the struct later takes its default here and no
    // call site needs to change (positional brace-init would instead warn at
    // every site, or silently default, depending on the compiler flags).
    /**
     * @brief Named constructor for ProcessContract.
     * @param process_name            Schedule name, e.g. ProcessNames::Dust.
     * @param fields                  Field accesses the process reads/writes.
     * @param mechanism_requirements  Roles/capabilities the mechanism must supply.
     * @param diagnostics             Diagnostics to register at init time.
     * @return Contract with the remaining members left at their defaults
     *         (recovery = RequiresReinitialize, concentration_amount_type =
     *         "species-native").
     */
    inline ProcessContract make_contract(std::string process_name, std::vector<FieldAccessContract> fields = {},
                                         std::vector<MechanismRequirement> mechanism_requirements = {},
                                         std::vector<DiagnosticDeclaration> diagnostics = {}) {
        ProcessContract contract;
        contract.process_name = std::move(process_name);
        contract.fields = std::move(fields);
        contract.mechanism_requirements = std::move(mechanism_requirements);
        contract.diagnostics = std::move(diagnostics);
        return contract;
    }

    /**
     * @brief Lifecycle interface every scheduled physics process implements.
     *
     * @details Ownership: Core holds the wrapper in a std::shared_ptr for the
     * schedule's lifetime and passes it a shared_ptr\<StateManager\> on each
     * call, so a process may retain state between run() invocations but must
     * release it in finalize(). The order Core enforces is:
     * 1. get_contract() at ExecutionPlan::compile() (schedule time, before any
     *    execution) -- and again after every schedule change;
     * 2. init(state) once, when the process is added to the schedule;
     * 3. per timestep or selected invocation, in this order:
     *    prepare_inputs(state) -> ExecutionPlan::prepare() (binds the fields
     *    the contract declares) -> run(state) -> ExecutionPlan::complete()
     *    (host/device bookkeeping) -> product publication;
     * 4. finalize() exactly once, in reverse schedule order, at shutdown.
     *
     * A process that throws inside run() taints the Core and invalidates all
     * derived products; contract.recovery then says what the host owes before
     * the next successful step.
     *
     * Thread safety: Core's lifecycle mutex serialises all callbacks, so a
     * process never sees concurrent prepare/run/finalize calls, but it must
     * not assume the same thread across steps.
     * @see FortranProcess for the BIND(C) bridge implementation, and
     *      ExecutionPlan for contract compilation and field preparation.
     */
    class ProcessInterface {
    public:
        virtual ~ProcessInterface() = default;
        /**
         * @brief Canonical schedule name of this process.
         * @return Name string; must match the YAML run_phases entry and the
         *         ProcessRegistry key, e.g. "dust".
         */
        virtual std::string get_name() const = 0;
        /**
         * @brief This process's field, diagnostic, product and recovery contract.
         * @return Freshly built ProcessContract (a value copy; Core stores its
         *         own compiled version). Must satisfy structurally_valid().
         * @note Default returns a name-only contract with no fields: valid for
         *       processes that touch no state. Called at compile time, before
         *       init(), so it must not depend on initialized resources.
         */
        virtual ProcessContract get_contract() const { return make_contract(get_name()); }
        // Populate process-owned derived inputs before the execution plan
        // validates the process contract.  Host imports must already be bound.
        /**
         * @brief Derive process-owned inputs after host imports are bound.
         * @param state Shared StateManager the field registry lives in; the
         *              process may read host fields but must not cache raw
         *              pointers past finalize().
         * @note Runs before ExecutionPlan::prepare() on every step, so required
         *       fields a process computes itself (rather than imports) can be
         *       declared in the contract. Default is a no-op.
         */
        virtual void prepare_inputs(std::shared_ptr<StateManager> state) { (void)state; }
        /**
         * @brief Acquire resources and register diagnostics for this process.
         * @param state Shared StateManager the process binds its fields against.
         * @throws Anything: Core treats a failure here as a construction
         *         failure and finalizes the processes already initialized.
         * @note Called exactly once, before the first run(), by
         *       Core::add_configured_processes() or
         *       catchem_core_add_process_by_name().
         */
        virtual void init(std::shared_ptr<StateManager> state) = 0;
        /**
         * @brief Advance the process by one step or interval.
         * @param state Shared StateManager whose bound fields are read and
         *              written; concentrations are species-native mixing ratios.
         * @note Called after prepare_inputs() and field preparation, and before
         *       ExecutionPlan::complete(). Throwing marks the step
         *       PartialUpdate, taints the Core and invalidates all products.
         */
        virtual void run(std::shared_ptr<StateManager> state) = 0;
        /**
         * @brief Release resources acquired by init().
         * @note Called exactly once in reverse schedule order by
         *       Core::shutdown(), and only for processes whose init() returned
         *       normally. Must be exception-safe enough to free its own
         *       resources; a throw is recorded as ShutdownFailure by the C
         *       destroy path but does not stop the remaining finalizers.
         */
        virtual void finalize() = 0;
    };

} // namespace catchem

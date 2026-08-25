#pragma once
#include "catchem_state_manager.hpp"
#include <memory>
#include <string>
#include <string_view>
#include <vector>

namespace catchem {

    enum class TimestepStatus { NotStarted, Running, Succeeded, PartialUpdate, ValidationFailed };
    enum class StateClassification { Reusable, RequiresReimport, RequiresReinitialize };

    struct TimestepOutcome {
        TimestepStatus status = TimestepStatus::NotStarted;
        std::size_t timestep = 0;
        double duration = 0.0;
        std::size_t import_generation = 0;
        std::string process_name;
        std::size_t process_index = 0;
        std::string cause;
        StateClassification state = StateClassification::Reusable;
    };

    struct ProcessNames {
        static constexpr std::string_view GasChem = "gaschem";
        static constexpr std::string_view Photolysis = "photolysis";
        static constexpr std::string_view Settling = "settling";
        static constexpr std::string_view Dust = "dust";
        static constexpr std::string_view SeaSalt = "seasalt";
        static constexpr std::string_view CarbChem = "carbchem";
        static constexpr std::string_view SO4Chem = "so4chem";
    };

    struct MechanismRequirement {
        std::string role;
        std::string capability;
        bool required = true;
    };

    struct DiagnosticDeclaration {
        std::string canonical_name;
        std::string units;
        std::vector<SemanticAxis> axes;
        PersistencePolicy persistence = PersistencePolicy::Timestep;
    };

    struct ProcessContract {
        std::string process_name;
        std::vector<FieldAccessContract> fields;
        std::vector<MechanismRequirement> mechanism_requirements;
        std::vector<DiagnosticDeclaration> diagnostics;

        bool structurally_valid() const noexcept {
            if (process_name.empty())
                return false;
            for (const auto& field : fields)
                if (field.canonical_name.empty() || field.units.empty() || field.axes.empty())
                    return false;
            for (const auto& diagnostic : diagnostics)
                if (diagnostic.canonical_name.empty() || diagnostic.units.empty() || diagnostic.axes.empty())
                    return false;
            return true;
        }
    };

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

    inline FieldAccessContract host_concentration(AccessIntent access = AccessIntent::ReadWrite) {
        return {"CONCENTRATION",
                "mol/mol",
                {SemanticAxis::Column, SemanticAxis::Level, SemanticAxis::Species},
                PersistencePolicy::Persistent,
                FieldRequirement::Required,
                access,
                ExecutionSpaceIntent::Host};
    }

    class ProcessInterface {
    public:
        virtual ~ProcessInterface() = default;
        virtual std::string get_name() const = 0;
        virtual ProcessContract get_contract() const { return {get_name(), {}, {}}; }
        virtual void init(std::shared_ptr<StateManager> state) = 0;
        virtual void run(std::shared_ptr<StateManager> state) = 0;
        virtual void finalize() = 0;
    };

} // namespace catchem

#include "catchem_config_manager.hpp"
#include "catchem_core_architecture_test_helpers.hpp"
#include "catchem_execution_plan.hpp"
#include "catchem_process_drydep.hpp"
#include "catchem_process_dust.hpp"
#include "catchem_process_registry.hpp"
#include "catchem_process_seasalt.hpp"
#include "catchem_process_so4chem.hpp"
#include "catchem_process_wetdep.hpp"
#include <algorithm>
#include <cassert>
#include <iostream>
#include <string>
#include <yaml-cpp/yaml.h>

extern "C" {
void catchem_register_drydep_cpp();
void catchem_register_so4chem_cpp();
}

class ContractProcess : public catchem::test::RecordingProcess {
public:
    using RecordingProcess::RecordingProcess;
    catchem::ProcessContract get_contract() const override {
        return catchem::make_contract(get_name(), {{"TEMPERATURE",
                                                    "K",
                                                    {catchem::SemanticAxis::Column, catchem::SemanticAxis::Level,
                                                     catchem::SemanticAxis::Singleton},
                                                    catchem::PersistencePolicy::Timestep,
                                                    catchem::FieldRequirement::Required,
                                                    catchem::AccessIntent::Read,
                                                    catchem::ExecutionSpaceIntent::Host}});
    }
};

class ProducerProcess : public catchem::test::RecordingProcess {
public:
    using RecordingProcess::RecordingProcess;
    catchem::ProcessContract get_contract() const override {
        auto output =
            catchem::host_field_3d("DERIVED", "1", catchem::FieldRequirement::Required, catchem::AccessIntent::Write);
        output.produced = true;
        return catchem::make_contract(get_name(), {output});
    }
};

class ConsumerProcess : public catchem::test::RecordingProcess {
public:
    using RecordingProcess::RecordingProcess;
    catchem::ProcessContract get_contract() const override {
        return catchem::make_contract(get_name(), {catchem::host_field_3d("DERIVED", "1")});
    }
};

/** @brief Verify process field, product, and dependency contracts. */
int main() {
    std::vector<std::string> events;
    std::vector<std::shared_ptr<catchem::ProcessInterface>> processes;
    processes.push_back(std::make_shared<ContractProcess>("contract", events));
    catchem::ExecutionPlan plan;
    plan.compile(processes, nullptr);
    assert(!plan.validation().has_errors());
    assert(plan.contract(0).fields.front().canonical_name == "TEMPERATURE");

    const std::vector<catchem::ProcessContract> surface_builtins{
        catchem::DustProcess().get_contract(), catchem::SeaSaltProcess().get_contract(),
        catchem::DryDepProcess().get_contract(), catchem::SO4chemProcess().get_contract()};
    for (const auto& contract : surface_builtins) {
        assert(contract.structurally_valid());
        bool has_surface_input = false;
        for (const auto& field : contract.fields)
            has_surface_input = has_surface_input || field.axes.size() == 2;
        assert(has_surface_input);
    }
    const auto dust_contract = catchem::DustProcess().get_contract();
    const auto soil_field =
        std::find_if(dust_contract.fields.begin(), dust_contract.fields.end(),
                     [](const catchem::FieldAccessContract& field) { return field.canonical_name == "SOILM"; });
    assert(soil_field != dust_contract.fields.end());
    assert(soil_field->units == "m3/m3");
    const std::vector<catchem::SemanticAxis> soil_axes{catchem::SemanticAxis::Column, catchem::SemanticAxis::SoilLayer,
                                                       catchem::SemanticAxis::Singleton};
    assert(soil_field->axes == soil_axes);
    const auto dust_lai_field =
        std::find_if(dust_contract.fields.begin(), dust_contract.fields.end(),
                     [](const catchem::FieldAccessContract& field) { return field.canonical_name == "LAI"; });
    assert(dust_lai_field != dust_contract.fields.end());
    assert(dust_lai_field->requirement == catchem::FieldRequirement::Required);
    const auto so4_contract = catchem::SO4chemProcess().get_contract();
    for (const auto* name : {"Z0H", "Z0"}) {
        const auto field =
            std::find_if(so4_contract.fields.begin(), so4_contract.fields.end(),
                         [name](const catchem::FieldAccessContract& item) { return item.canonical_name == name; });
        assert(field != so4_contract.fields.end());
        assert(field->requirement == catchem::FieldRequirement::Optional);
    }
    auto so4_state = std::make_shared<catchem::StateManager>(1, 1, 1);
    std::vector<double> layer_field{1.0};
    so4_state->bind_met_field_3d("DELP", layer_field.data());
    so4_state->bind_met_field_3d("AIRDEN", layer_field.data());
    catchem::SO4chemProcess so4_process;
    bool missing_roughness_rejected = false;
    try {
        so4_process.prepare_inputs(so4_state);
    } catch (const std::runtime_error& error) {
        missing_roughness_rejected = std::string(error.what()).find("Z0H or fallback Z0") != std::string::npos;
    }
    assert(missing_roughness_rejected);
    so4_state->bind_met_field_2d("Z0H", layer_field.data());
    so4_process.prepare_inputs(so4_state);

    const auto jacob_wetdep_contract = catchem::WetDepProcess().get_contract();
    assert(jacob_wetdep_contract.structurally_valid());
    for (const auto* field_name : {"PMID", "AIRDEN_DRY", "QV", "REEVAPLS"}) {
        const auto field = std::find_if(
            jacob_wetdep_contract.fields.begin(), jacob_wetdep_contract.fields.end(),
            [field_name](const catchem::FieldAccessContract& item) { return item.canonical_name == field_name; });
        assert(field != jacob_wetdep_contract.fields.end());
        assert(field->requirement == catchem::FieldRequirement::Required);
    }
    auto wetdep_gocart_process = std::make_shared<catchem::WetDepProcess>();
    auto wetdep_config_manager = std::make_shared<catchem::ConfigManager>();
    catchem::ProcessConfig wetdep_config;
    wetdep_config.activate = true;
    wetdep_config.diagnostics = false;
    wetdep_config.scheme = "gocart";
    wetdep_config_manager->data.processes["wetdep"] = wetdep_config;
    auto wetdep_state = std::make_shared<catchem::StateManager>(1, 1, 0);
    wetdep_state->attach_config_manager(wetdep_config_manager);
    wetdep_gocart_process->init(wetdep_state);
    const auto gocart_wetdep_contract = wetdep_gocart_process->get_contract();
    for (const auto* jacob_only_name : {"PMID", "AIRDEN_DRY", "QV", "REEVAPLS"}) {
        const auto field = std::find_if(gocart_wetdep_contract.fields.begin(), gocart_wetdep_contract.fields.end(),
                                        [jacob_only_name](const catchem::FieldAccessContract& item) {
                                            return item.canonical_name == jacob_only_name;
                                        });
        assert(field == gocart_wetdep_contract.fields.end());
    }
    const auto required_field = [&gocart_wetdep_contract](const std::string& name) {
        return std::find_if(
            gocart_wetdep_contract.fields.begin(), gocart_wetdep_contract.fields.end(),
            [&name](const catchem::FieldAccessContract& field) { return field.canonical_name == name; });
    };
    for (const auto* rain_name : {"PRECCON", "PRECLSC"}) {
        const auto rain = required_field(rain_name);
        assert(rain != gocart_wetdep_contract.fields.end());
        assert(rain->requirement == catchem::FieldRequirement::Required);
        assert((rain->axes ==
                std::vector<catchem::SemanticAxis>{catchem::SemanticAxis::Column, catchem::SemanticAxis::Singleton}));
    }
    for (const auto* flux_name : {"PFILSAN", "PFLLSAN"}) {
        const auto flux = required_field(flux_name);
        assert(flux != gocart_wetdep_contract.fields.end());
        assert((flux->axes == std::vector<catchem::SemanticAxis>{catchem::SemanticAxis::Column,
                                                                 catchem::SemanticAxis::Interface,
                                                                 catchem::SemanticAxis::Singleton}));
    }
    std::vector<double> interface_field{0.0, 0.0};
    std::vector<double> level_field{0.0};
    wetdep_state->bind_met_field_3d("T", level_field.data());
    wetdep_state->bind_met_field_3d("PEDGE", interface_field.data());
    wetdep_state->bind_met_field_3d("AIRDEN", level_field.data());
    wetdep_state->bind_met_field_3d("PFILSAN", interface_field.data());
    wetdep_state->bind_met_field_3d("PFLLSAN", interface_field.data());
    plan.compile({wetdep_gocart_process}, nullptr);
    bool missing_gocart_rain_rejected = false;
    try {
        plan.prepare(0, *wetdep_state);
    } catch (const std::runtime_error& error) {
        missing_gocart_rain_rejected = std::string(error.what()).find("PRECCON") != std::string::npos;
    }
    assert(missing_gocart_rain_rejected);

    std::vector<std::shared_ptr<catchem::ProcessInterface>> reversed;
    reversed.push_back(std::make_shared<ConsumerProcess>("consumer", events));
    reversed.push_back(std::make_shared<ProducerProcess>("producer", events));
    plan.compile(reversed, nullptr);
    assert(plan.validation().has_errors());
    assert(plan.validation().format().find("dependency-order") != std::string::npos);

    std::vector<std::shared_ptr<catchem::ProcessInterface>> ordered;
    ordered.push_back(std::make_shared<ProducerProcess>("producer", events));
    ordered.push_back(std::make_shared<ConsumerProcess>("consumer", events));
    plan.compile(ordered, nullptr);
    assert(!plan.validation().has_errors());

    // --- US2 settings-validator allowlists (FR-021, contract K-3) ------------
    // The three GOCART-faithful routing keys must be accepted by the
    // registered per-process validators, and a misspelled sibling in the same
    // scheme block must still be rejected, so a typo cannot silently leave a
    // scheme on its compiled default.
    catchem_register_drydep_cpp();
    catchem_register_so4chem_cpp();
    auto& registry = catchem::ProcessRegistry::get_instance();
    auto accept = [&](const char* process, const YAML::Node& settings) {
        catchem::ProcessConfig config;
        config.activate = true;
        config.set_settings_node(settings);
        try {
            registry.validate_settings(process, config);
        } catch (const std::exception& error) {
            std::cerr << "FAIL: validator rejected legal option for " << process << ": " << error.what() << '\n';
            return false;
        }
        return true;
    };
    auto reject = [&](const char* process, const YAML::Node& settings, const std::string& expected_key) {
        catchem::ProcessConfig config;
        config.activate = true;
        config.set_settings_node(settings);
        try {
            registry.validate_settings(process, config);
        } catch (const std::exception& error) {
            return std::string(error.what()).find(expected_key) != std::string::npos;
        }
        return false;
    };
    YAML::Node drydep_ok;
    drydep_ok["wesely"]["skip_so2"] = true;
    drydep_ok["gocart"]["skip_sulfate_aero"] = true;
    assert(accept("drydep", drydep_ok));
    YAML::Node drydep_bad;
    drydep_bad["wesely"]["skip_so3"] = true;
    assert(reject("drydep", drydep_bad, "wesely/skip_so3"));
    YAML::Node so4chem_ok;
    so4chem_ok["gocart"]["do_drydep"] = true;
    assert(accept("so4chem", so4chem_ok));
    YAML::Node so4chem_bad;
    so4chem_bad["gocart"]["do_wetdep"] = true;
    assert(reject("so4chem", so4chem_bad, "gocart/do_wetdep"));
    return 0;
}

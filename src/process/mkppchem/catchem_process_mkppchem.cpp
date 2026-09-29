// src/process/mkppchem/catchem_process_mkppchem.cpp
//
// The `mkppchem` process: compiled-mechanism selection, configuration,
// provenance validation, species mapping, cloud-water resolution, staging
// allocation, and the per-timestep solve sequence (gather / env fill / solve /
// scatter with solver-failure detection). This is the only translation unit in
// the library that includes a generated MKPP mechanism header, so the set of
// solvers built into the binary is fixed at configure time
// (CATCHEM_MKPP_MECHANISMS) -- which is exactly the required semantics:
// "mechanism not compiled into the build" is a build fact,
// reported against the list actually available.
//
// The definition facts the runtime needs come from the single aggregated
// generated metadata header (catchem_mkpp_mechanism_meta.hpp), never from a
// YAML parse: the build-time generator parses each pinned OpenAtmos
// definition once and the CI provenance verifier re-checks that the header
// still matches it. So yaml-cpp stays off this library's link (the
// single-parse design). The solver registry this process selects from is
// generated too (catchem_mkpp_mechanism_registry.hpp), so adding a mechanism
// never edits this file.
//
// Tendency diagnostics are published through DiagnosticManager as one packed
// (Column, Level, Species) field, `mkppchem_tendency`, whose Species labels are
// the selected advanced species.
#include "catchem_process_mkppchem.hpp"

#include "catchem_config_manager.hpp"
#include "catchem_diagnostic_manager.hpp"
#include "catchem_logger.hpp"
#include "catchem_process_registry.hpp"
#include "catchem_state_manager.hpp"

#include "catchem_mkpp_mechanism.hpp"
#include "catchem_mkpp_state_bridge.hpp"

// The generated aggregated solver registry: mechanism id -> adapter factory,
// gated by the same MKPP_HOST_MECH_<UPPER> macros CMake sets from
// CATCHEM_MKPP_MECHANISMS. It pulls in the hand-written adapter machinery
// (catchem_mkpp_mechanism_traits.hpp) and MKPP's compiled-mechanism manager,
// which is what makes the mechanism set a single build fact shared with MKPP:
// the generated artifact headers are limited to the selected mechanisms by
// MKPP_HOST_SELECTED_MECHANISMS / MKPP_HOST_MECH_<UPPER> (set per
// CATCHEM_MKPP_MECHANISMS in src/external/CMakeLists.txt), so only the
// selected mechanisms' large headers compile here. Without those macros it
// would pull every mechanism MKPP knows about (ts1, saprc99, ...).
#include "catchem_mkpp_mechanism_registry.hpp"

#include <Kokkos_Core.hpp>

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <filesystem>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

namespace catchem {

    namespace {

        void fatal(const std::string& context, const std::string& reason) {
            throw std::runtime_error("FATAL ERROR: mkppchem " + context + ": " + reason);
        }

        /// Read a process option under both the `processes/` and the legacy
        /// `process/` prefix, matching the gaschem convention.
        std::string config_string(const std::shared_ptr<ConfigManager>& config, const std::string& key,
                                  const std::string& fallback) {
            if (!config)
                return fallback;
            for (const std::string& prefix : {"processes/mkppchem/", "process/mkppchem/"}) {
                std::string value = config->get_string(prefix + key, "");
                if (!value.empty())
                    return value;
            }
            return fallback;
        }

        bool config_bool(const std::shared_ptr<ConfigManager>& config, const std::string& key, bool fallback) {
            if (!config)
                return fallback;
            for (const std::string& prefix : {"processes/mkppchem/", "process/mkppchem/"})
                if (config->get_bool(prefix + key, fallback) != fallback)
                    return config->get_bool(prefix + key, fallback);
            return fallback;
        }

        std::vector<std::string> config_string_list(const std::shared_ptr<ConfigManager>& config,
                                                    const std::string& key) {
            if (!config)
                return {};
            for (const std::string& prefix : {"processes/mkppchem/", "process/mkppchem/"}) {
                std::vector<std::string> value = config->get_string_list(prefix + key);
                if (!value.empty())
                    return value;
            }
            return {};
        }

    } // namespace

    struct MkppChemProcess::Impl {
        mkpp::MechanismDefinition definition;
        mkpp::SpeciesMapping mapping;
        mkpp::CloudWaterSource cloud_water;
        std::unique_ptr<mkpp::SolverAdapter> solver;
        // Grid shape captured at init() so run() performs no allocation and no
        // re-derivation. The staging buffers and device species map live behind
        // the solver adapter, which owns the mechanism types.
        int ncol = 0;
        int nlev = 0;
        int num_cells = 0;
        // Diagnostic publication: the mechanism slots selected for the
        // packed tendency field, in slot order, plus the device copy the
        // publish kernel reads (kernels cannot capture a std::vector).
        std::vector<int> diag_slots;
        Kokkos::View<int*, Kokkos::LayoutLeft, mkpp::StagingBuffers::MemSpace> diag_slots_dev;
        // Photolysis coupling: the diagnostic name bound to each jvals
        // slot, in definition declaration order -- index i is jvals slot i, so
        // the binding order is the positional contract and never Diagnostic-
        // Manager map order. Empty when the mechanism declares no photolysis
        // or when smoke mode supplies the host-forcing constants instead.
        std::vector<std::string> photolysis_diag_names;
        // Smoke mode: the definition's host-forcing constants, copied to
        // the device once at init so the per-timestep fill reads no host
        // memory. Empty unless photolysis_smoke_mode_ is set.
        Kokkos::View<double*, Kokkos::LayoutLeft, mkpp::StagingBuffers::MemSpace> smoke_forcing_dev;
    };

    MkppChemProcess::MkppChemProcess() = default;
    MkppChemProcess::~MkppChemProcess() = default;

    ProcessContract MkppChemProcess::get_contract() const {
        // Device intent throughout: the solver launches a per-cell Kokkos
        // kernel directly on the state's device-resident memory, so declaring
        // Host would force a synchronization the zero-copy path exists to
        // avoid.
        //
        // Cloud liquid water and CLDF are declared Optional here because the
        // contract is built before configuration is read: whether they are
        // required depends on the mechanism's gating and the configured mode,
        // and that is enforced at resolution time in init(), which is fatal on
        // a missing source.
        std::vector<FieldAccessContract> fields;
        fields.push_back(device_concentration(AccessIntent::ReadWrite));
        fields.push_back(device_field_3d("T", "K"));
        fields.push_back(device_field_3d("RH", "1"));
        fields.push_back(device_field_3d("PMID", "Pa"));
        fields.push_back(device_field_3d("AIRDEN_DRY", "kg/m3"));
        fields.push_back(device_field_3d("CLDLIQ", "kg/m3", FieldRequirement::Optional));
        fields.push_back(device_field_3d("CLDF", "1", FieldRequirement::Optional));
        // The tendency diagnostic is one packed (Column, Level, Species) field
        // whose Species labels are the resolved mapping, registered at init()
        // when diagnostics are enabled -- the same axes-driven pattern
        // carbchem/drydep/settling use, so the generic writer unpacks one
        // variable per advanced species.
        std::vector<DiagnosticDeclaration> diagnostics;
        diagnostics.push_back({"mkppchem_tendency", "mol/mol/s",
                               {SemanticAxis::Column, SemanticAxis::Level, SemanticAxis::Species},
                               PersistencePolicy::Timestep});
        return make_contract(get_name(), std::move(fields), {}, std::move(diagnostics));
    }

    void MkppChemProcess::init(std::shared_ptr<StateManager> state) {
        if (!state)
            fatal("init", "null state");
        const auto config = state->config_manager();

        // 1. Configuration. The configured
        //    mechanism may be empty: the shipped default (gocart) is then
        //    selected and the init log records that the default was used.
        mechanism_ = config_string(config, "mechanism", "");
        definition_file_ = config_string(config, "definition_file", "");
        diagnostics_enabled_ = config_bool(config, "diagnostics", false);
        diag_species_ = config_string_list(config, "diag_species");
        photolysis_smoke_mode_ = config_bool(config, "photolysis_smoke_mode", false);
        // Optional runtime ramp-seed override. Unset (empty) keeps the artifact's
        // baked compile-time seed; a configured value is validated here so a
        // typo fails at configuration time rather than altering solver behavior
        // unnoticed mid-run.
        initial_step_seed_ = mkpp::parse_initial_step_seed(config_string(config, "initial_step_seed", ""));
        cloud_water_config_.mode =
            mkpp::parse_cloud_water_mode(config_string(config, "cloud_liquid_water/mode", "auto"));
        cloud_water_config_.host_field = config_string(config, "cloud_liquid_water/host_field", "");
        if (cloud_water_config_.mode == mkpp::CloudWaterMode::HostField && cloud_water_config_.host_field.empty())
            fatal("configuration",
                  "cloud_liquid_water/mode is 'host_field' but cloud_liquid_water/host_field names no field");

        // 2. Mechanism selection against what is compiled in.
        //    The CATChem registry is the authority for what can run -- it pairs
        //    each id with its compiled solver adapter. MKPP's own host registry
        //    is consulted only to cross-check the same build fact: an id CATChem
        //    can construct but MKPP's registry rejects would be a broken build,
        //    and MKPP raises a bare std::runtime_error, so it is caught and
        //    re-raised with CATChem context rather than allowed to escape.
        const auto& registry = mkpp::mechanism_registry();
        std::vector<std::string> compiled_ids;
        compiled_ids.reserve(registry.size());
        for (const auto& entry : registry)
            compiled_ids.push_back(entry.first);
        bool used_default = false;
        const size_t selected_index = mkpp::select_mechanism(mechanism_, compiled_ids, "gocart", used_default);
        mechanism_ = compiled_ids[selected_index];
        try {
            // MKPP's own registry must agree that the mechanism exists; for an
            // unknown name it raises a bare std::runtime_error, which is caught
            // here and re-raised with CATChem context rather than escaping into
            // the run log.
            ::mkpp::host::MechanismRegistry::get_info(mechanism_);
        } catch (const std::exception& e) {
            fatal("mechanism", std::string("compiled-mechanism registry check failed: ") + e.what());
        }
        impl_ = std::make_unique<Impl>();
        impl_->solver = registry[selected_index].second();
        auto& solver = *impl_->solver;

        // A configured runtime seed needs an artifact generated with the seed
        // member; otherwise the option would be silently ignored and the solve
        // would use a different ramp than the run log claims. Fail loudly.
        if (initial_step_seed_.has_value() && !solver.supports_runtime_seed())
            fatal("configuration", "initial_step_seed is configured but the compiled '"
                                       + mechanism_ +
                                       "' artifact predates the runtime seed member; regenerate the mechanism "
                                       "with MKPP or remove the option");
        if (initial_step_seed_.has_value())
            solver.set_initial_step_seed(*initial_step_seed_);

        // 3. Definition facts come from the SELECTED mechanism's Facts struct
        //    in the single aggregated generated metadata header, NOT from a
        //    runtime YAML parse (the single-parse design). The build-time
        //    generator baked each pinned definition's structural facts into
        //    that header; the CI provenance verifier re-parses the definition
        //    and asserts the header still matches, so a stale header is a CI
        //    failure, never a silent runtime divergence.
        //    A configured `definition_file` is more than a log entry: the
        //    runtime species state and the solver must come from the same
        //    source, so the file's metadata block is checked against the
        //    selected mechanism's compiled provenance before anything else
        //    loads -- a definition the mechanism was not generated from is
        //    fatal naming both definitions.
        impl_->definition = mkpp::definition_from_meta(mechanism_);
        if (!definition_file_.empty()) {
            if (!std::filesystem::is_regular_file(definition_file_))
                fatal("configuration", "configured definition_file '" + definition_file_ +
                                           "' does not exist; it must be the OpenAtmos definition the selected "
                                           "mechanism was generated from");
            mkpp::verify_definition_provenance(definition_file_, mechanism_, impl_->definition.legacy_source_sha256,
                                               impl_->definition.definition_path);
            impl_->definition.definition_path = definition_file_;
        }

        // 4. Provenance against the compiled artifact. validate_provenance
        //    owns the R1 interim guard: a cloud-gated definition paired with an
        //    artifact exposing no cloud-water equilibrium input is fatal there,
        //    naming both paths -- never a silent ungated run.
        const std::string artifact_path = "compiled:" + solver.mechanism_id();
        const mkpp::CompiledMechanismDescriptor descriptor{solver.mechanism_id(), solver.num_species(),
                                                           solver.num_photolysis(), solver.num_equilibrium_inputs(),
                                                           artifact_path, {}};
        mkpp::validate_provenance(impl_->definition, descriptor);

        // 5. Species mapping: every mechanism species must resolve to a
        //    CATChem tracer; no zero-fill, no alias.
        std::unordered_map<std::string, int> catchem_name_to_index;
        const auto& species_list = state->chemistry().species_list;
        for (size_t i = 0; i < species_list.size(); ++i)
            catchem_name_to_index[mkpp::canonical_upper(species_list[i].short_name)] = static_cast<int>(i);
        impl_->mapping = mkpp::build_species_mapping(impl_->definition, catchem_name_to_index);
        if (!impl_->mapping.unmatched_mechanism.empty()) {
            std::ostringstream names;
            for (size_t i = 0; i < impl_->mapping.unmatched_mechanism.size(); ++i)
                names << (i ? ", " : "") << impl_->mapping.unmatched_mechanism[i];
            fatal("species mapping", "mechanism species have no CATChem counterpart: " + names.str() +
                                         " (definition '" + impl_->definition.definition_path + "', artifact '" +
                                         artifact_path + "')");
        }

        // 5b. Process-order conflict re-check. The registered settings
        //     validator already rejects an mkppchem + so4chem/carbchem run at
        //     configuration load; this re-check covers drivers that build the
        //     process list programmatically without going through
        //     validate_settings, so a double-applied tendency is caught here
        //     too. It runs before any allocation and names both processes and
        //     the shared species.
        if (config) {
            const auto conflicts = mkpp::find_process_order_conflict(config->data.active_processes,
                                                                     impl_->definition.species_order);
            if (!conflicts.empty())
                fatal("configuration", "process-order conflict: " +
                                           mkpp::describe_process_order_conflicts(conflicts) +
                                           "; running both would double-apply the same species' chemistry");
        }

        // 6. Grid sanity before any allocation.
        const int ncol = state->column_count();
        const int nlev = state->level_count();
        if (ncol <= 0 || nlev <= 0 || state->species_count() <= 0)
            fatal("init", "non-positive grid: columns=" + std::to_string(ncol) +
                              " levels=" + std::to_string(nlev) + " species=" + std::to_string(state->species_count()));
        impl_->ncol = ncol;
        impl_->nlev = nlev;
        impl_->num_cells = ncol * nlev;

        // 7. Cloud water resolution. Only a cloud-gated
        //    mechanism consults the source; resolve_cloud_water is fatal on an
        //    unresolvable one, naming the mode and the missing field or
        //    derivation input.
        if (impl_->definition.has_cloud_gated_reaction) {
            const auto& met = state->meteorology();
            impl_->cloud_water = mkpp::resolve_cloud_water(
                cloud_water_config_,
                [&](const std::string& name) { return met.get_3d({name.c_str()}); },
                [&](const std::string& name) { return met.has_3d({name.c_str()}); });
        }

        // 8. Device species map + staging buffers, allocated once here and
        //    reused every timestep. The adapter owns the mechanism types, so
        //    the process never names a generated class here.
        solver.prepare(impl_->num_cells, impl_->mapping);

        // 8b. Photolysis coupling plan. The definition's declaration order IS
        //     the jvals slot order, so the binding is built by iterating the
        //     labels -- never DiagnosticManager's map order. Smoke mode is a
        //     contradiction when a photolysis source is also present; a
        //     photolysis-consuming mechanism with no source at all is fatal,
        //     and a label with no bound diagnostic is fatal naming
        //     the label and the definition.
        {
            std::vector<std::string> registered;
            if (auto dm = state->diagnostic_manager())
                registered = dm->get_registered_names();
            const std::vector<std::string> no_processes;
            const std::vector<std::string>& active =
                config ? config->data.active_processes : no_processes;
            const auto plan = mkpp::plan_photolysis_binding(impl_->definition, photolysis_smoke_mode_, active,
                                                            registered);
            if (plan.smoke_mode) {
                Logger::warn(state.get(),
                             "photolysis_smoke_mode is ON: J-values are the definition's host-forcing constants, "
                             "NOT computed photolysis rates. Isolated mechanism testing only -- never use for a "
                             "physics run.",
                             {{"photolysis", "smoke mode"}});
                impl_->smoke_forcing_dev =
                    Kokkos::View<double*, Kokkos::LayoutLeft, mkpp::StagingBuffers::MemSpace>(
                        "mkppchem_smoke_forcing", static_cast<int>(plan.smoke_forcing.size()));
                auto host_forcing = Kokkos::create_mirror_view(impl_->smoke_forcing_dev);
                for (size_t j = 0; j < plan.smoke_forcing.size(); ++j)
                    host_forcing(static_cast<int>(j)) = plan.smoke_forcing[j];
                Kokkos::deep_copy(impl_->smoke_forcing_dev, host_forcing);
            }
            impl_->photolysis_diag_names = plan.diag_names;
        }

        // 9. Tendency diagnostics: one packed
        //    (Column, Level, Species) field whose Species labels are the
        //    selected advanced species, in slot order -- the same axes-driven
        //    pattern carbchem/drydep/settling use, so the generic writer
        //    unpacks one variable per species. `diag_species` selects a subset
        //    (each entry must resolve to a mapped species); empty
        //    means every advanced species.
        if (diagnostics_enabled_ && state->diagnostic_manager()) {
            std::vector<std::string> labels;
            impl_->diag_slots.clear();
            if (diag_species_.empty()) {
                for (const auto& entry : impl_->mapping.mapped) {
                    impl_->diag_slots.push_back(entry.mechanism_slot);
                    labels.push_back(state->chemistry()
                                         .species_list[static_cast<size_t>(entry.catchem_index)]
                                         .short_name);
                }
            } else {
                for (const auto& wanted : diag_species_) {
                    const std::string key = mkpp::canonical_upper(wanted);
                    const auto entry = std::find_if(impl_->mapping.mapped.begin(), impl_->mapping.mapped.end(),
                                                     [&](const mkpp::SpeciesMappingEntry& m) {
                                                         return m.canonical_name == key;
                                                     });
                    if (entry == impl_->mapping.mapped.end())
                        fatal("diagnostics", "diag_species entry '" + wanted +
                                                 "' is not a species the mechanism advances (definition '" +
                                                 impl_->definition.definition_path + "')");
                    impl_->diag_slots.push_back(entry->mechanism_slot);
                    labels.push_back(
                        state->chemistry().species_list[static_cast<size_t>(entry->catchem_index)].short_name);
                }
            }
            const std::vector<int> dims = {ncol, nlev, static_cast<int>(impl_->diag_slots.size())};
            const std::vector<SemanticAxis> axes = {SemanticAxis::Column, SemanticAxis::Level, SemanticAxis::Species};
            state->diagnostic_manager()->register_field_contract("mkppchem_tendency", "MKPP chemistry tendency",
                                                                 "mol/mol/s", DiagType::FIELD_3D, dims,
                                                                 DiagnosticPolicy::Instantaneous, 0.0, axes, labels);
            // The publish kernel needs the slot list on device; build the copy
            // once here (run() allocates nothing).
            impl_->diag_slots_dev =
                Kokkos::View<int*, Kokkos::LayoutLeft, mkpp::StagingBuffers::MemSpace>("mkppchem_diag_slots",
                                                                                       static_cast<int>(impl_->diag_slots.size()));
            auto host_slots = Kokkos::create_mirror_view(impl_->diag_slots_dev);
            for (size_t d = 0; d < impl_->diag_slots.size(); ++d)
                host_slots(static_cast<int>(d)) = impl_->diag_slots[d];
            Kokkos::deep_copy(impl_->diag_slots_dev, host_slots);
        }

        initialized_ = true;
        // Initialization log: everything a contributor needs to confirm
        // a correct setup from
        // the log alone, at informational verbosity.
        std::ostringstream mapping_log;
        for (size_t i = 0; i < impl_->mapping.mapped.size(); ++i) {
            const auto& m = impl_->mapping.mapped[i];
            const auto& meta = state->chemistry().species_list[static_cast<size_t>(m.catchem_index)];
            mapping_log << (i ? "; " : "") << m.mechanism_slot << "<->" << m.catchem_index << "<->" << meta.short_name;
        }
        std::ostringstream unadvanced;
        for (size_t i = 0; i < impl_->mapping.unadvanced_catchem.size(); ++i)
            unadvanced << (i ? ", " : "") << impl_->mapping.unadvanced_catchem[i];
        std::ostringstream jvals;
        for (size_t i = 0; i < impl_->definition.photolysis_labels.size(); ++i)
            jvals << (i ? ", " : "") << impl_->definition.photolysis_labels[i] << "@" << i;
        Logger::info(state.get(), "mkppchem initialized",
                     {{"mechanism", solver.mechanism_id()},
                      {"mechanism_default", used_default ? "true" : "false"},
                      {"definition", impl_->definition.definition_path},
                      {"provenance", impl_->definition.identity()},
                      {"species", std::to_string(solver.num_species())},
                      {"reactions", std::to_string(impl_->definition.num_reactions)},
                      {"photolysis", std::to_string(solver.num_photolysis())},
                      {"equilibrium_inputs", std::to_string(solver.num_equilibrium_inputs())},
                      {"mapped", std::to_string(impl_->mapping.mapped.size())},
                      {"mapping", mapping_log.str()},
                      {"unadvanced", unadvanced.str().empty() ? "none" : unadvanced.str()},
                      {"jvals_labels", jvals.str().empty() ? "none" : jvals.str()},
                      {"initial_step_seed",
                       initial_step_seed_.has_value() ? std::to_string(*initial_step_seed_) + " (runtime)"
                                                      : "baked (compile-time)"},
                      {"jvals_source", impl_->smoke_forcing_dev.size() > 0
                                           ? "smoke (host-forcing constants)"
                                           : (impl_->photolysis_diag_names.empty()
                                                  ? "none"
                                                  : "photolysis diagnostics (per-timestep device refresh)")},
                      {"met_fields", "T,RH,PMID,AIRDEN_DRY" +
                                         std::string(impl_->definition.has_cloud_gated_reaction ? ",CLDF,QV" : "")},
                      {"cloud_water", impl_->definition.has_cloud_gated_reaction ? impl_->cloud_water.description
                                                                                 : "not required"},
                      {"staging_cells", std::to_string(impl_->num_cells)},
                      {"staging_bytes", std::to_string(solver.staging_bytes())},
                      {"diagnostics", diagnostics_enabled_ ? "mkppchem_tendency" : "off"}});
    }

    void MkppChemProcess::run(std::shared_ptr<StateManager> state) {
        if (!initialized_)
            fatal("run", "called before a successful init()");
        if (!state)
            fatal("run", "null state");
        auto& solver = *impl_->solver;
        auto& buffers = solver.buffers();
        const int ncol = impl_->ncol;
        const int num_cells = impl_->num_cells;

        // Timestep must be finite and > 0 before anything reads it as a
        // denominator; a zero or NaN step would divide the tendency by zero and
        // let the solver integrate a meaningless interval.
        const double dt = state->clock().timestep;
        if (!std::isfinite(dt) || dt <= 0.0)
            fatal("run", "chemistry timestep must be finite and greater than zero, got " + std::to_string(dt));

        // 1. Derived fields are retained across timesteps but invalidated when a
        //    new import generation begins, so pointer existence alone does not
        //    mean AIRDEN_DRY is current. Derive, then require currency of every
        //    field the solve consumes.
        auto& met = state->meteorology();
        if (met.PMID && met.T)
            state->derive_airden_dry();
        const std::size_t generation = state->current_import_generation();
        auto require_current = [&](const std::shared_ptr<InteropField<double, 3>>& field, const char* name) {
            if (!field)
                fatal("run", std::string("required field '") + name + "' is not bound");
            if (!field->is_current(generation))
                fatal("run", std::string("required field '") + name +
                                 "' is stale for import generation " + std::to_string(generation));
        };
        require_current(met.T, "T");
        require_current(met.RH, "RH");
        require_current(met.PMID, "PMID");
        require_current(met.AIRDEN_DRY, "AIRDEN_DRY");
        if (!state->chemistry().conc)
            fatal("run", "chemical concentration field is not bound");
        if (!state->chemistry().conc->is_current(generation))
            fatal("run", "chemical concentration field is stale for import generation " +
                             std::to_string(generation));

        // 2. Sync host-authored state to device. This is the only host-device
        //    transfer, and it is a no-op when the writer side is already current
        //    or when HostSpace == DeviceSpace (the zero-copy contract).
        state->chemistry().conc->sync_to_device();
        met.T->sync_to_device();
        met.RH->sync_to_device();
        met.PMID->sync_to_device();
        met.AIRDEN_DRY->sync_to_device();
        auto conc = state->chemistry().conc->view();
        auto temp = met.T->view();
        auto rh = met.RH->view();
        auto pmid = met.PMID->view();
        auto airden = met.AIRDEN_DRY->view();

        // Reset the per-cell error view to the sentinel. Every check below flags
        // the FIRST failure and never overwrites an existing code, so the
        // reported reason is the earliest one, not a downstream artifact.
        auto errors = buffers.error_status();
        mkpp::clear_errors(buffers);

        // 3. Physical-admissibility of the bound met fields (T > 0, PMID > 0,
        //    0 <= RH <= 1, AIRDEN_DRY > 0, all finite). A flagged cell is
        //    skipped by the solve and reported by the reduction below -- never
        //    solved with a fabricated value.
        mkpp::validate_met(temp, rh, pmid, airden, num_cells, ncol, errors);

        // 4. Gather + unit conversion: mol/mol -> mol/m3 into the contiguous
        //    per-cell species row (species fastest, matching state.data()).
        //    The accessors hand out View handles by value; bind them to locals
        //    so the kernels' non-const reference parameters have an lvalue.
        auto species_buf = buffers.species();
        auto env_buf = buffers.env();
        mkpp::gather_species(conc, airden, solver.device_map(), solver.num_species(), num_cells, ncol, species_buf);

        // 5. Environment fill. The cloud-water column is bound only when the
        //    artifact exposes a third equilibrium input; the source (host field
        //    or in-kernel derivation) was resolved at init(). A mechanism with
        //    no equilibrium inputs at all (env_count == 0, e.g. chapman, whose
        //    rates are temperature- and humidity-independent) has a zero-width
        //    env buffer and its integrate() reads none, so the whole fill is
        //    skipped -- writing column -1 would be out of bounds.
        if (solver.num_equilibrium_inputs() > 0) {
            if (impl_->definition.has_cloud_gated_reaction && solver.env_cloud_water() >= 0) {
                if (impl_->cloud_water.is_host() && impl_->cloud_water.field) {
                    impl_->cloud_water.field->sync_to_device();
                    auto cloud = impl_->cloud_water.field->view();
                    mkpp::fill_environment(temp, rh, &cloud, num_cells, ncol, solver.env_temperature(),
                                           solver.env_humidity(), solver.env_cloud_water(), env_buf);
                } else {
                    // Derived origin: CLDF and QV are required for the in-kernel
                    // water-content formula; validate_met already covered T/PMID.
                    auto cldf_field = met.get_3d({"CLDF"});
                    auto qv_field = met.get_3d({"QV"});
                    require_current(cldf_field, "CLDF");
                    require_current(qv_field, "QV");
                    cldf_field->sync_to_device();
                    qv_field->sync_to_device();
                    auto cldf = cldf_field->view();
                    auto qv = qv_field->view();
                    mkpp::fill_environment_derived(temp, rh, cldf, qv, pmid, num_cells, ncol, solver.env_temperature(),
                                                   solver.env_humidity(), solver.env_cloud_water(), env_buf, errors);
                }
            } else {
                // Ungated (or the artifact exposes no cloud-water slot): the
                // pointer is only consulted when the slot index is valid, so a
                // typed null keeps the template deduction honest.
                mkpp::fill_environment(temp, rh, static_cast<const decltype(temp)*>(nullptr), num_cells, ncol,
                                       solver.env_temperature(), solver.env_humidity(), solver.env_cloud_water(), env_buf);
            }
        } // env fill (skipped entirely for a mechanism with no equilibrium inputs)

        // Snapshot the post-gather state so the tendency is the solver's own
        // change, and so the pre-solve check sees the values about to be solved.
        Kokkos::deep_copy(buffers.before(), buffers.species());

        // 6. Pre-solve validation: every staged species finite and >= 0. A bad
        //    cell is flagged and skipped by the solve (never solved on garbage).
        mkpp::validate_pre_solve(buffers);

        // 6b. Photolysis coupling refresh. Every bound J-rate is
        //     re-read from its diagnostic into the jvals staging buffer on
        //     EVERY timestep -- never cached across steps -- and through the
        //     device view, not a host pointer, so a GPU-resident rate is read
        //     correctly. Smoke mode instead re-applies the definition's
        //     host-forcing constants (a fixed value each step, kept on device).
        //     A mechanism with no photolysis leaves the zero-filled buffer.
        {
            auto jvals = buffers.jvals();
            if (!impl_->photolysis_diag_names.empty()) {
                auto dm = state->diagnostic_manager();
                for (size_t slot = 0; slot < impl_->photolysis_diag_names.size(); ++slot) {
                    const auto rates = dm->get_device_view_2d(impl_->photolysis_diag_names[slot]);
                    mkpp::fill_jvals_from_diagnostic(rates, num_cells, ncol, static_cast<int>(slot), jvals);
                }
            } else if (impl_->smoke_forcing_dev.size() > 0) {
                mkpp::fill_jvals_from_host_forcing(impl_->smoke_forcing_dev, num_cells, jvals);
            }
        }

        // 7. Solve: CATChem-owned per-cell launch of the generated solver.
        solver.solve(dt);

        // 8. Post-solve validation: the solver returns void, so CATChem owns
        //    detecting a non-finite or negative output cell.
        mkpp::validate_post_solve(buffers);

        // 9. Reduce to the lowest failing cell (fences inside), then report.
        //    Reporting the LOWEST index -- not an arbitrary one -- is what makes
        //    the message reproducible across rank counts and backends.
        const int first_error = mkpp::reduce_first_error(buffers);
        if (first_error != mkpp::kNoError) {
            int icol = 0, ilev = 0;
            mkpp::decode_cell(first_error, ncol, icol, ilev);
            // The reduction already fenced, so a single host mirror of the error
            // view is consistent. This path is fatal-only, so the copy cost is
            // irrelevant; a rank-0 subview mirror would be the alternative.
            const auto err_host =
                Kokkos::create_mirror_view_and_copy(Kokkos::HostSpace{}, buffers.error_status());
            const int code = err_host(first_error);
            // Identify the offending species by reading the staged row on the
            // host (a single cell, after the reduction's fence).
            auto row = Kokkos::create_mirror_view_and_copy(Kokkos::HostSpace{},
                                                           Kokkos::subview(buffers.species(), first_error, Kokkos::ALL()));
            std::string species;
            for (int s = 0; s < solver.num_species(); ++s) {
                if (!std::isfinite(row(s)) || row(s) < 0.0) {
                    species = solver.mechanism_id() + " slot " + std::to_string(s);
                    break;
                }
            }
            const char* reason = code == mkpp::CELL_ERR_NON_PHYSICAL_MET
                                     ? "non-physical meteorology (T/RH/PMID/AIRDEN_DRY)"
                                 : code == mkpp::CELL_ERR_NON_FINITE_INPUT
                                     ? "non-finite or negative input concentration"
                                     : "solver produced a non-finite or negative output";
            fatal("solve", std::string("cell (column ") + std::to_string(icol + 1) + ", level " +
                               std::to_string(ilev + 1) + ") [flat index " + std::to_string(first_error) +
                               "] failed: " + reason + (species.empty() ? "" : "; offending species: " + species) +
                               " -- mechanism '" + solver.mechanism_id() + "'");
        }

        // 10. Tendencies (mol/mol/s) for the diagnostics path: the solver's
        //     per-cell change, rate-normalised with the same rho_molar as the
        //     gather/scatter. Published into the packed diagnostic field when
        //     diagnostics are enabled.
        mkpp::compute_tendencies(buffers, airden, ncol, dt);
        if (diagnostics_enabled_ && !impl_->diag_slots.empty() && state->diagnostic_manager()) {
            auto diag_view = state->diagnostic_manager()->get_device_view_3d("mkppchem_tendency");
            mkpp::publish_tendencies(buffers.tendency(), impl_->diag_slots_dev, diag_view, ncol, num_cells);
            state->diagnostic_manager()->mark_device_modified("mkppchem_tendency");
        }

        // 11. Scatter + inverse conversion, mapped species only, so CATChem
        //     species the mechanism does not advance are bit-for-bit unchanged.
        mkpp::scatter_species(buffers.species(), airden, solver.device_map(), solver.num_species(), num_cells, ncol,
                              conc);

        // 12. The kernels wrote the device view of conc directly; mark it
        //     device-current so the next host reader syncs from device, not from
        //     a now-stale host buffer.
        state->chemistry().conc->mark_device_modified();
    }

    void MkppChemProcess::finalize() {
        impl_.reset();
        initialized_ = false;
    }

} // namespace catchem

extern "C" {
void catchem_register_mkppchem_cpp() {
    // The settings validator covers the nested option blocks (unknown keys
    // rejected) and the process-order conflict: a run whose active list pairs
    // mkppchem with an existing GOCART chemistry process advancing the same
    // species would silently double-apply the chemistry, so it fails at
    // configuration load -- the earliest gate, before any process is built. The
    // framework keys (activate, diagnostics, diag_species, scheme) are consumed
    // by the process layer and excluded from option_paths() by design.
    auto key_validator =
        catchem::make_settings_validator("mkppchem", {"cloud_liquid_water/mode", "cloud_liquid_water/host_field"});
    auto conflict_validator = [](const catchem::ProcessConfig& settings) {
        // The run context is only present for a ProcessConfig produced by a
        // full configuration load; a directly-constructed block has no active
        // list to inspect, so the conflict is left to the init() re-check.
        if (!settings.run_context)
            return;
        // The conflict is defined by the SELECTED mechanism's species, not the
        // default's: a chapman run shares no species with so4chem/carbchem, so
        // the gocart overlap must not be reported for it. An unknown id falls
        // back to the default's species -- init() reports the real cause.
        std::string mechanism = settings.get_string("mechanism", "");
        const auto compiled = catchem::mkpp::compiled_mechanism_ids();
        if (std::find(compiled.begin(), compiled.end(), mechanism) == compiled.end())
            mechanism = "gocart";
        const auto conflicts = catchem::mkpp::find_process_order_conflict(settings.run_context->active_processes,
                                                                          catchem::mkpp::definition_from_meta(mechanism).species_order);
        if (conflicts.empty())
            return;
        throw std::invalid_argument("mkppchem process-order conflict: " +
                                    catchem::mkpp::describe_process_order_conflicts(conflicts) +
                                    "; running both would double-apply the same species' chemistry");
    };
    catchem::ProcessRegistry::get_instance().register_process(
        std::string(catchem::ProcessNames::MkppChem), []() { return std::make_shared<catchem::MkppChemProcess>(); },
        []() { return catchem::MkppChemProcess().get_contract(); },
        [key_validator, conflict_validator](const catchem::ProcessConfig& settings) {
            key_validator(settings);
            conflict_validator(settings);
        });
}
}

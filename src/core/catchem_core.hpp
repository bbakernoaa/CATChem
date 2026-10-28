/**
 * @file catchem_core.hpp
 * @brief Central Orchestration Engine for the CATChem modern C++ framework.
 *
 * @details Core owns the whole simulation object graph - ConfigManager,
 * GridManager, StateManager, DiagnosticManager and the scheduled processes -
 * and is the C++ object behind the opaque Core handle of the flat C API. A
 * lifecycle_mutex_ serialises schedule changes, timesteps and interval
 * transitions so no process can be finalized or recompiled underneath an
 * active bridge call. Construction acquires a process-global RuntimeLease
 * (see catchem_runtime_lease.hpp); shutdown releases it after finalizing
 * every initialized process in reverse order.
 *
 * Failure handling is explicit: a process that throws taints the Core, marks
 * every derived product and the diagnostic generation as failed, and refuses
 * the next timestep until the host reimports state at a newer import
 * generation. The last TimestepOutcome records what failed and how to recover.
 */

#pragma once
#include "catchem_config_manager.hpp"
#include "catchem_diagnostic_manager.hpp"
#include "catchem_execution_plan.hpp"
#include "catchem_grid_manager.hpp"
#include "catchem_process_interface.hpp"
#include "catchem_runtime_lease.hpp"
#include "catchem_selected_dispatch.hpp"
#include "catchem_state_manager.hpp"
#include <memory>
#include <mutex>
#include <string>
#include <vector>

namespace catchem {

    /**
     * @brief Inputs for the single staged Core construction pipeline.
     *
     * @details One of the three named factories builds the options; all
     * constructors funnel through initialize(). @p use_configuration_grid
     * selects whether the grid comes from the YAML (standalone) or from the
     * host (domain-decomposed coupled runs). Species count is taken from the
     * configuration whenever a config file is supplied.
     */
    struct CoreCreateOptions {
        std::string config_file;             ///< YAML path; empty means dimensions-only construction.
        int columns = 1;                     ///< Host-local column count (nx*ny) when the host owns the grid.
        int levels = 1;                      ///< Vertical level count.
        int species = 0;                     ///< Chemical species count; ignored when config_file is set.
        bool use_configuration_grid = false; ///< true to read nx/ny/nz from the YAML runtime section.

        /// Options for a dimensions-only Core (no YAML): grid and species come from the arguments.
        static CoreCreateOptions direct_dimensions(int columns, int levels, int species);
        /// Options for a fully configured Core whose grid also comes from the YAML.
        static CoreCreateOptions configured(std::string config_file);
        /// Options for a configured Core with a host-supplied (decomposed) grid.
        static CoreCreateOptions configured_with_host_grid(std::string config_file, int columns, int levels);
    };

    /**
     * @class Core
     * @brief High-performance orchestration class managing the atmospheric physics lifecycle.
     *
     * The Core class coordinates grid layout, meteorological states, and schedules
     * chemical processes. It acts as the single source of truth for the physical simulation.
     *
     * @details Thread safety: every public mutator takes lifecycle_mutex_, so
     * one Core may be driven from a single host thread while other threads
     * only read accessors. The object graph is shared-ownership: managers are
     * handed out as std::shared_ptr and the C API hands out their addresses as
     * Borrowed child handles of the Core handle, whose destroy path deletes
     * this object and invalidates those child handles.
     * @see CoreCreateOptions for construction inputs, catchem_api.hpp for the
     *      checked C entry points that wrap this class.
     */
    class Core {
    private:
        RuntimeLease runtime_lease_;                              ///< Process-global execution-runtime lease.
        std::shared_ptr<ConfigManager> config_mgr;                ///< Shared configuration manager.
        std::shared_ptr<GridManager> grid_mgr;                    ///< Shared grid details.
        std::shared_ptr<StateManager> state_mgr;                  ///< Reference-counted memory state.
        std::shared_ptr<DiagnosticManager> diag_mgr;              ///< Global diagnostics registry.
        std::vector<std::shared_ptr<ProcessInterface>> processes; ///< Scheduled physics processes.
        TimestepOutcome last_outcome_;                            ///< Result of the most recent timestep/selected run.
        /// Monotonic step ordinal; doubles as the product-interval identity
        /// when no host interval is open.
        std::size_t timestep_counter_ = 0;
        std::uint64_t active_interval_id_ = 0; ///< Open host interval id; 0 when none.
        /// Set when a process threw; blocks further execution until the host
        /// reimports state at a newer import generation.
        bool tainted_ = false;
        ExecutionPlan execution_plan_;                             ///< Compiled contracts and product provenance.
        std::unique_ptr<SelectedProcessLedger> selected_dispatch_; ///< Ledger for selected-process intervals.
        mutable std::mutex lifecycle_mutex_;      ///< Serialises edits, timesteps, intervals, shutdown.
        std::vector<bool> initialized_processes_; ///< True once init() ran; shutdown finalizes those.
        bool shutdown_ = false;                   ///< Set by shutdown(); later add/run calls rejected.

        /** @brief Creates and initializes the processes requested by configuration. */
        void add_configured_processes();

        /** Execute one already-resolved process under the lifecycle lock. */
        void execute_process(std::size_t index, bool verbose);
        /// Rebuild the SelectedProcessLedger from the current schedule; throws
        /// std::logic_error if a selected interval is open.
        void rebuild_selected_dispatch();

        /** @brief Performs the common ordered construction stages. */
        void initialize(const CoreCreateOptions& options);

    public:
        explicit Core(const CoreCreateOptions& options);

        /**
         * @brief Constructs the Core with dimensions.
         * @param nc Number of contiguous columns.
         * @param nl Number of vertical levels.
         * @param ns Number of chemical species.
         */
        Core(int nc, int nl, int ns);

        /**
         * @brief Constructs the Core and loads configuration parameters from a YAML file.
         * @param config_file Path to the YAML configuration file.
         * @throws std::runtime_error If the file cannot be read or parsed.
         */
        Core(const std::string& config_file);

        /**
         * @brief Constructs the Core from a YAML file with host-supplied grid dimensions.
         *
         * Configuration (species, processes, runtime options) comes from the file;
         * the grid is sized by the host — required under domain decomposition
         * (e.g. UFS per-rank tiles), where the YAML grid section does not apply.
         * @param config_file Path to the YAML configuration file.
         * @param nc Number of contiguous columns (host-local).
         * @param nl Number of vertical levels.
         * @throws std::runtime_error If the file cannot be read or parsed.
         */
        Core(const std::string& config_file, int nc, int nl);
        ~Core() noexcept;

        /**
         * @brief Finalizes initialized processes exactly once, in reverse schedule order.
         *
         * @details Takes the lifecycle lock, calls finalize() on every process
         * whose init() completed, releases the runtime lease, and marks the
         * Core shut down. If several processes throw, the first exception is
         * rethrown after all finalizers have run, so cleanup is never skipped.
         * Idempotent: a second call returns immediately.
         * @note The destructor calls this and swallows any exception; hosts
         *       that must observe cleanup failures should call it directly.
         */
        void shutdown();

        /** @brief Get the configuration manager. */
        std::shared_ptr<ConfigManager> get_config_manager();

        /** @brief Get the grid manager. */
        std::shared_ptr<GridManager> get_grid_manager();

        /**
         * @brief Get the state manager.
         * @return Shared ownership of the StateManager; the C API registers the
         *         returned object's address as a Borrowed child handle of this
         *         Core, so it must not be used after Core destruction.
         */
        std::shared_ptr<StateManager> get_state_manager();

        /** @brief Get the diagnostic manager. */
        std::shared_ptr<DiagnosticManager> get_diagnostic_manager();

        /** @brief Get the number of scheduled physics processes. */
        std::size_t get_num_processes() const;
        /**
         * @brief Return the compiled contract for one scheduled process.
         * @param index 0-based schedule index; must be below get_num_processes().
         * @return Reference into the ExecutionPlan's stored contract; valid only
         *         until the next add_process()/shutdown() recompiles the plan.
         * @throws std::out_of_range when @p index is not a valid contract index.
         */
        const ProcessContract& get_process_contract(std::size_t index) const { return execution_plan_.contract(index); }

        /**
         * @brief Return the unique required host-input field names declared by
         *        the active process contracts.
         *
         * Derived state and driver-owned static inputs are intentionally not
         * distinguished here; integrations use their own field contracts to
         * decide which of these requirements are supplied by their imports.
         */
        std::vector<std::string> get_required_host_fields() const;
        /**
         * @brief Return the outcome recorded for the most recent step or run.
         * @return Reference to the internal TimestepOutcome; overwritten by the
         *         next run_timestep() or run_selected_process().
         */
        const TimestepOutcome& get_timestep_outcome() const noexcept { return last_outcome_; }

        /**
         * @brief Registers a new physics process into the simulation schedule.
         * @param process Shared pointer to the process interface.
         *
         * @note The process must already have had init() called; this method
         *       recompiles the ExecutionPlan and rejects the schedule with
         *       std::invalid_argument (listing the validation issues) when the
         *       new contract introduces errors, restoring the previous
         *       schedule. Throws std::logic_error after shutdown().
         */
        void add_process(std::shared_ptr<ProcessInterface> process);

        /**
         * @brief Executes a single timestep.
         * @param dt Timestep duration in seconds.
         * @throws std::out_of_range If dt is non-positive. (The implementation
         *         also rejects non-finite dt and dt greater than 86400 s.)
         * @throws std::logic_error after shutdown(), or std::runtime_error when
         *         the Core is tainted and no newer import generation arrived.
         * @note A process failure leaves status PartialUpdate, taints the
         *       Core, invalidates all products and marks the diagnostic
         *       generation failed before rethrowing; inspect
         *       get_timestep_outcome() for the failing process and cause.
         */
        void run_timestep(double dt);

        /**
         * @brief Executes a single timestep using the default step size from configuration.
         * @throws std::out_of_range when the configured dt fails validation.
         */
        void run_timestep();

        /**
         * @brief Open a host-owned interval for selected-process execution.
         *
         * @param interval_id   Host-chosen identifier; must be new (not equal
         *                      to the immediately previous interval) and non-zero.
         * @param interval_time Host clock plus the interval duration in
         *                      @c timestep; must satisfy TimeState::valid().
         *
         * @details Installs @p interval_time as the Core clock, validates the
         * state for execution, opens the SelectedProcessLedger and advances the
         * diagnostic interval. Throws std::logic_error after shutdown(),
         * std::out_of_range on an invalid clock, and std::runtime_error when
         * the Core is tainted without a newer import generation.
         */
        void begin_interval(std::uint64_t interval_id, const TimeState& interval_time);

        /**
         * @brief Run exactly one scheduled process inside the open interval.
         *
         * @param key      Invocation identity (interval, site, subcycle,
         *                 process). The interval must match the open one and
         *                 the identity must not have been used before.
         * @param duration Seconds this invocation covers; must be positive,
         *                 finite and no longer than the open interval.
         * @return The ledger's SelectedOperationOutcome: Succeeded (with
         *         publication_eligible true and the post-run import
         *         generation), Failed (process threw, Core tainted), or
         *         Rejected (never admitted; cause explains why).
         * @note Does not throw for a rejected or failed operation; the flat C
         *       API maps the outcome to ProcessFailure/ContractViolation.
         */
        SelectedOperationOutcome run_selected_process(const SelectedInvocationKey& key, double duration);

        /**
         * @brief Close the matching host interval and commit the step.
         * @param interval_id Must equal the currently open interval id.
         * @throws std::logic_error after shutdown(), or when no interval is
         *         open / the id does not match. Throws std::runtime_error when
         *         a selected-process failure tainted the Core during this
         *         interval. On success diagnostics are synced to host and the
         *         outcome is recorded as Succeeded.
         */
        void end_interval(std::uint64_t interval_id);
    };

} // namespace catchem

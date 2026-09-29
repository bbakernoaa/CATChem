// src/process/mkppchem/catchem_mkpp_mechanism_traits.hpp
//
// The hand-written machinery the generated solver registry instantiates: a
// type-erased solver adapter and the generic compile-time traits that read a
// generated mechanism's Facts struct. This is the ONE place mechanism
// specifics are wired; it is deliberately mechanism-free.
//
// Why generic: the generated MKPP headers expose NUM_SPECIES/NUM_PHOTOLYSIS as
// members of SolverKernels but the EquilibriumInput enum at namespace scope
// (keyed by mechanism name, unreachable from the Solver type), which is what
// forced a per-mechanism MechanismTraits specialization. The build-time
// generator now bakes the equilibrium-channel indices into each Facts struct
// (derived from the definition, since MKPP's enum order is fixed), so the
// traits read them from Facts and one template serves every mechanism. Adding
// a mechanism therefore adds a definition to the generator command, not a
// specialization here.
//
// Memory-layout authority stays with the artifact: num_species and
// num_photolysis come from SolverKernels (the solver writes exactly that many
// slots per cell), while the equilibrium indices and id come from Facts. The
// CI provenance verifier cross-checks definition == artifact == Facts for
// every one of them, so a disagreement is a build failure, never a runtime
// divergence.
#pragma once

#ifdef CATCHEM_ENABLE_KOKKOS

#include "catchem_mkpp_mechanism.hpp"
#include "catchem_mkpp_state_bridge.hpp"

#include <Kokkos_Core.hpp>

#include <cstddef>
#include <memory>
#include <string>
#include <string_view>
#include <vector>

namespace catchem::mkpp {

    /// Compile-time facts of one generated mechanism, read from its Facts
    /// struct. `Solver` supplies the array dimensions the staging buffers are
    /// sized to; `Facts` supplies the identity and the equilibrium-channel
    /// indices the namespace-scoped artifact enum cannot be reached for.
    /// `env_cloud_water` is -1 for a mechanism whose channel set has no third
    /// input, so the bridge's requires-probe selects the matching integrate()
    /// overload -- never hardcoded at a call site.
    template <typename Solver, typename Facts>
    struct MechanismTraits {
        static constexpr std::string_view id = Facts::mechanism_id;
        static constexpr int num_species = Solver::NUM_SPECIES;
        static constexpr int num_photolysis = Solver::NUM_PHOTOLYSIS;
        static constexpr int env_temperature = Facts::env_temperature;
        static constexpr int env_humidity = Facts::env_humidity;
        static constexpr int env_cloud_water = Facts::env_cloud_water;
        static constexpr int env_count = Facts::env_count;
    };

    /// Type-erased view of one compiled mechanism so the process holds a
    /// std::unique_ptr<SolverAdapter> without exposing a Kokkos or
    /// generated-header type through the pimpl. Everything that needs a
    /// mechanism type stays in the adapter: the process side never casts back
    /// to a concrete instantiation.
    struct SolverAdapter {
        virtual ~SolverAdapter() = default;
        virtual std::string mechanism_id() const = 0;
        virtual int num_species() const = 0;
        virtual int num_photolysis() const = 0;
        virtual int num_equilibrium_inputs() const = 0;
        /// Allocate the staging buffers and the device species map for a
        /// grid of `num_cells` cells and the given slot mapping.
        virtual void prepare(int num_cells, const mkpp::SpeciesMapping& mapping) = 0;
        /// The process-owned staging buffers (species/env/jvals/error/
        /// before/tendency), valid after prepare().
        virtual mkpp::StagingBuffers& buffers() = 0;
        /// The device copy of the slot -> CATChem-index map, built in prepare().
        virtual const mkpp::DeviceSpeciesMap& device_map() const = 0;
        /// The artifact's declared EquilibriumInput slot indices (table-
        /// driven, R1 forward-compat); env_cloud_water() is -1 when the
        /// artifact exposes no cloud-water input.
        virtual int env_temperature() const = 0;
        virtual int env_humidity() const = 0;
        virtual int env_cloud_water() const = 0;
        /// CATChem-owned per-cell launch of the generated solver: a
        /// RangePolicy over cells calling integrate() on each cell's
        /// contiguous species row. Cells flagged by an earlier check are
        /// skipped, never solved on bad input.
        virtual void solve(double dt_total) = 0;
        /// Runtime override of the generated artifact's baked ramp seed
        /// (first substep = dt_total * seed). 0.0 means "keep the baked
        /// value". supports_runtime_seed() is false for artifacts that
        /// predate the seed member; setting a seed on those is fatal at
        /// init, never silently ignored.
        virtual bool supports_runtime_seed() const = 0;
        virtual void set_initial_step_seed(double seed) = 0;
        virtual double initial_step_seed() const = 0;
        /// Device memory occupied by the per-cell staging buffers, for the
        /// init log.
        virtual std::size_t staging_bytes() const = 0;
    };

    /// One concrete compiled mechanism: the generated solver paired with its
    /// baked Facts. Instantiated by the generated registry, one per mechanism
    /// selected at configure time.
    template <typename Solver, typename Facts>
    struct MechanismAdapter final : SolverAdapter {
        mkpp::StagingBuffers staging_;
        mkpp::DeviceSpeciesMap slot_map_;
        double runtime_seed_ = 0.0;  // 0.0 = use the artifact's baked seed

        std::string mechanism_id() const override { return std::string(MechanismTraits<Solver, Facts>::id); }
        int num_species() const override { return MechanismTraits<Solver, Facts>::num_species; }
        int num_photolysis() const override { return MechanismTraits<Solver, Facts>::num_photolysis; }
        int num_equilibrium_inputs() const override { return MechanismTraits<Solver, Facts>::env_count; }
        mkpp::StagingBuffers& buffers() override { return staging_; }
        const mkpp::DeviceSpeciesMap& device_map() const override { return slot_map_; }
        int env_temperature() const override { return MechanismTraits<Solver, Facts>::env_temperature; }
        int env_humidity() const override { return MechanismTraits<Solver, Facts>::env_humidity; }
        int env_cloud_water() const override { return MechanismTraits<Solver, Facts>::env_cloud_water; }

        void solve(double dt_total) override {
            mkpp::solve_cells<Solver>(staging_, dt_total, env_temperature(), env_humidity(), env_cloud_water(),
                                      runtime_seed_);
        }

        bool supports_runtime_seed() const override {
            // Compile-time probe against the linked artifact: a generated
            // header predating the seed member has no such field, and a
            // configured runtime seed would then be silently ignored.
            if constexpr (requires(const Solver& s) { s.initial_step_seed; })
                return true;
            else
                return false;
        }
        void set_initial_step_seed(double seed) override { runtime_seed_ = seed; }
        double initial_step_seed() const override { return runtime_seed_; }

        std::size_t staging_bytes() const override { return staging_.bytes(); }

        void prepare(int num_cells, const mkpp::SpeciesMapping& mapping) override {
            staging_.allocate(num_cells, num_species(), num_photolysis(), num_equilibrium_inputs());

            std::vector<int> slot_to_catchem(static_cast<size_t>(num_species()), 0);
            for (const auto& entry : mapping.mapped)
                slot_to_catchem[static_cast<size_t>(entry.mechanism_slot)] = entry.catchem_index;
            slot_map_.slot_to_catchem =
                Kokkos::View<int*, Kokkos::LayoutLeft, mkpp::StagingBuffers::MemSpace>("mkppchem_slotmap",
                                                                                       num_species());
            auto host = Kokkos::create_mirror_view(slot_map_.slot_to_catchem);
            for (int s = 0; s < num_species(); ++s)
                host(s) = slot_to_catchem[static_cast<size_t>(s)];
            Kokkos::deep_copy(slot_map_.slot_to_catchem, host);
        }
    };

} // namespace catchem::mkpp

#endif // CATCHEM_ENABLE_KOKKOS

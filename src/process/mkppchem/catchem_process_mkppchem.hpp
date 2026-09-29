// src/process/mkppchem/catchem_process_mkppchem.hpp
//
// The `mkppchem` process: an opt-in alternative to the GOCART-like
// `so4chem` + `carbchem` pair, advancing an MKPP ahead-of-time-compiled
// mechanism through its per-cell Kokkos solver directly on CATChem's shared
// chemical state.
//
// The process owns configuration, compiled-mechanism selection, definition
// parsing, provenance validation (including the cloud-gated-reaction guard),
// the species mapping, cloud-water resolution, staging-buffer allocation, and
// the per-timestep solve sequence (gather / env fill / solve / scatter with
// solver-failure detection) plus diagnostic publication.
//
// The class is Kokkos- and MKPP-free by design: everything that needs the
// generated solver header or a Kokkos type lives behind the pimpl in the
// mechanism translation unit, so consumers that only need the process name or
// contract do not pull in a C++23/GPU toolchain surface.
#pragma once

#include "catchem_mkpp_cloud_water.hpp"
#include "catchem_process_interface.hpp"

#include <memory>
#include <optional>
#include <string>
#include <vector>

namespace catchem {

    class MkppChemProcess : public ProcessInterface {
    public:
        MkppChemProcess();
        ~MkppChemProcess() override;

        MkppChemProcess(const MkppChemProcess&) = delete;
        MkppChemProcess& operator=(const MkppChemProcess&) = delete;
        MkppChemProcess(MkppChemProcess&&) = delete;
        MkppChemProcess& operator=(MkppChemProcess&&) = delete;

        std::string get_name() const override { return std::string(ProcessNames::MkppChem); }
        ProcessContract get_contract() const override;

        void init(std::shared_ptr<StateManager> state) override;
        void run(std::shared_ptr<StateManager> state) override;
        void finalize() override;

    private:
        // Resolved configuration.
        std::string mechanism_ = "gocart";
        std::string definition_file_;
        bool diagnostics_enabled_ = false;
        std::vector<std::string> diag_species_;
        mkpp::CloudWaterConfig cloud_water_config_;
        bool photolysis_smoke_mode_ = false;
        // Runtime ramp-seed override read from configuration; nullopt keeps the
        // artifact's baked compile-time seed.
        std::optional<double> initial_step_seed_;

        // Mechanism-typed state (definition, mapping, cloud-water source,
        // staging buffers): owned by the pimpl so this header stays free of
        // MKPP and Kokkos includes.
        struct Impl;
        std::unique_ptr<Impl> impl_;

        bool initialized_ = false;
    };

} // namespace catchem

extern "C" {
void catchem_register_mkppchem_cpp();
}

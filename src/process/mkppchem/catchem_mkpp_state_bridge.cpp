// src/process/mkppchem/catchem_mkpp_state_bridge.cpp
//
// Non-template definitions for the device staging bridge. The gather/scatter,
// environment-fill, met-validation, and solve kernels are templates in the
// header (they are instantiated by the process translation unit, which is the
// only one that includes the generated mechanism header); this unit owns the
// buffer allocation and the error-status bookkeeping, none of which needs the
// generated artifact.
#include "catchem_mkpp_state_bridge.hpp"

namespace catchem::mkpp {

    void StagingBuffers::allocate(int num_cells, int num_species, int num_photolysis, int num_eq_inputs) {
        num_cells_ = num_cells;
        num_species_ = num_species;
        num_photolysis_ = num_photolysis;
        num_eq_inputs_ = num_eq_inputs;

        // LayoutRight so each cell's species / jvals / env row is contiguous.
        species_buf_ = Kokkos::View<double**, Kokkos::LayoutRight, MemSpace>("mkppchem_species", num_cells, num_species);
        jvals_buf_ = Kokkos::View<double**, Kokkos::LayoutRight, MemSpace>("mkppchem_jvals", num_cells, num_photolysis);
        env_buf_ = Kokkos::View<double**, Kokkos::LayoutRight, MemSpace>("mkppchem_env", num_cells, num_eq_inputs);
        error_status_ = Kokkos::View<int*, Kokkos::LayoutLeft, MemSpace>("mkppchem_error", num_cells);
        before_buf_ = Kokkos::View<double**, Kokkos::LayoutRight, MemSpace>("mkppchem_before", num_cells, num_species);
        tendency_buf_ = Kokkos::View<double**, Kokkos::LayoutRight, MemSpace>("mkppchem_tendency", num_cells, num_species);

        // The view constructor zero-fills; the reduction in reduce_first_error()
        // relies on the sentinel instead, so overwrite it explicitly.
        clear_errors(*this);
    }

    namespace {

        /// Flag a cell unless an earlier check already did: the FIRST failure in
        /// the timestep's check sequence wins, so the reported reason is the
        /// earliest one rather than a downstream artifact of solving bad input.
        KOKKOS_INLINE_FUNCTION void flag_if_clean(Kokkos::View<int*, Kokkos::LayoutLeft, StagingBuffers::MemSpace> errors, int i_cell,
                                                  int code) {
            const int current = errors(i_cell);
            if (current == kNoError || current == CELL_SUCCESS)
                errors(i_cell) = code;
        }

        /// True when every staged species in this cell is finite and >= 0.
        KOKKOS_INLINE_FUNCTION bool species_row_is_valid(Kokkos::View<double**, Kokkos::LayoutRight, StagingBuffers::MemSpace> species,
                                                         int i_cell, int num_species) {
            for (int s = 0; s < num_species; ++s) {
                const double v = species(i_cell, s);
                if (!Kokkos::isfinite(v) || v < 0.0)
                    return false;
            }
            return true;
        }

    } // namespace

    void validate_pre_solve(const StagingBuffers& buffers) {
        auto species = buffers.species();
        auto errors = buffers.error_status();
        const int num_cells = buffers.num_cells();
        const int num_species = buffers.num_species();
        Kokkos::parallel_for(
            "mkppchem_precheck", Kokkos::RangePolicy<StagingBuffers::ExecSpace>(0, num_cells),
            KOKKOS_LAMBDA(int i_cell) {
                if (!species_row_is_valid(species, i_cell, num_species))
                    flag_if_clean(errors, i_cell, CELL_ERR_NON_FINITE_INPUT);
            });
    }

    void validate_post_solve(StagingBuffers& buffers) {
        auto species = buffers.species();
        auto errors = buffers.error_status();
        const int num_cells = buffers.num_cells();
        const int num_species = buffers.num_species();
        Kokkos::parallel_for(
            "mkppchem_postcheck", Kokkos::RangePolicy<StagingBuffers::ExecSpace>(0, num_cells),
            KOKKOS_LAMBDA(int i_cell) {
                if (!species_row_is_valid(species, i_cell, num_species))
                    flag_if_clean(errors, i_cell, CELL_ERR_NUMERICAL_FAILURE);
            });
    }

    void clear_errors(StagingBuffers& buffers) {
        auto errors = buffers.error_status();
        const int num_cells = buffers.num_cells();
        Kokkos::parallel_for(
            "mkppchem_clearenv", Kokkos::RangePolicy<StagingBuffers::ExecSpace>(0, num_cells),
            KOKKOS_LAMBDA(int i_cell) { errors(i_cell) = kNoError; });
    }

    int reduce_first_error(const StagingBuffers& buffers) {
        auto errors = buffers.error_status();
        const int num_cells = buffers.num_cells();
        Kokkos::fence();
        // Reduce to the lowest FAILING CELL INDEX (not the lowest code), so the
        // result names one reproducible cell regardless of execution order.
        int first_cell = kNoError;
        Kokkos::parallel_reduce(
            "mkppchem_firsterror", Kokkos::RangePolicy<StagingBuffers::ExecSpace>(0, num_cells),
            KOKKOS_LAMBDA(int i_cell, int& update) {
                const int e = errors(i_cell);
                if (e != kNoError && e != CELL_SUCCESS && i_cell < update)
                    update = i_cell;
            },
            Kokkos::Min<int>(first_cell));
        return first_cell;
    }

} // namespace catchem::mkpp

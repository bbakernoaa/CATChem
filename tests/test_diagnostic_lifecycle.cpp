#include "catchem_api.hpp"
#include "catchem_core.hpp"
#include "catchem_diagnostic_manager.hpp"
#include <cassert>
#include <stdexcept>
#include <vector>

int main() {
    void* core_handle = catchem_core_create(2, 3, 1);
    assert(core_handle);
    auto* core = static_cast<catchem::Core*>(core_handle);
    auto manager = core->get_diagnostic_manager();
    const std::vector<int> dims = {2, 1};
    const std::vector<catchem::SemanticAxis> axes = {catchem::SemanticAxis::Column, catchem::SemanticAxis::Singleton};
    manager->register_field_contract("instant", "instantaneous value", "1", catchem::DiagType::FIELD_2D, dims,
                                     catchem::DiagnosticPolicy::Instantaneous, 0.0, axes);
    manager->register_field_contract("accumulated", "timestep accumulation", "kg", catchem::DiagType::FIELD_2D, dims,
                                     catchem::DiagnosticPolicy::TimestepAccumulated, -1.0, axes);
    manager->register_field_contract("persistent", "persistent value", "m", catchem::DiagType::FIELD_2D, dims,
                                     catchem::DiagnosticPolicy::Persistent, 0.0, axes);

    static_cast<double*>(manager->get_host_write_pointer("instant"))[0] = 7.0;
    static_cast<double*>(manager->get_host_write_pointer("accumulated"))[0] = 8.0;
    static_cast<double*>(manager->get_host_write_pointer("persistent"))[0] = 9.0;
    manager->begin_timestep();
    assert(static_cast<const double*>(manager->get_host_read_pointer("instant"))[0] == 0.0);
    assert(static_cast<const double*>(manager->get_host_read_pointer("accumulated"))[0] == -1.0);
    assert(static_cast<const double*>(manager->get_host_read_pointer("persistent"))[0] == 9.0);
    assert(manager->get_field("instant")->latest_writer == catchem::LatestWriter::Synchronized);
    assert(manager->get_field("persistent")->latest_writer == catchem::LatestWriter::HostCurrent);

    for (int timestep = 2; timestep <= 3; ++timestep) {
        static_cast<double*>(manager->get_host_write_pointer("instant"))[0] = timestep;
        static_cast<double*>(manager->get_host_write_pointer("accumulated"))[0] += timestep;
        manager->begin_timestep();
        assert(manager->get_field("instant")->generation == static_cast<std::size_t>(timestep));
        assert(manager->get_field("persistent")->generation == static_cast<std::size_t>(timestep));
        assert(static_cast<const double*>(manager->get_host_read_pointer("persistent"))[0] == 9.0);
    }

    manager->register_field_contract("instant", "instantaneous value", "1", catchem::DiagType::FIELD_2D, dims,
                                     catchem::DiagnosticPolicy::Instantaneous, 0.0, axes);
    bool mismatch_rejected = false;
    try {
        manager->register_field_contract("instant", "different meaning", "1", catchem::DiagType::FIELD_2D, dims,
                                         catchem::DiagnosticPolicy::Instantaneous, 0.0, axes);
    } catch (const std::invalid_argument&) {
        mismatch_rejected = true;
    }
    assert(mismatch_rejected);

    // --- feature 013: unpack_labels contract (lenient subset) ---
    const std::vector<int> bin_dims = {2, 3};
    const std::vector<catchem::SemanticAxis> bin_axes = {catchem::SemanticAxis::Column,
                                                         catchem::SemanticAxis::Category};
    const std::vector<std::string> bin_labels = {"DUST1", "DUST2", "DUST3"};
    manager->register_field_contract("per_bin", "per bin", "kg", catchem::DiagType::FIELD_2D, bin_dims,
                                     catchem::DiagnosticPolicy::Instantaneous, 0.0, bin_axes, bin_labels);
    assert(manager->get_axes("per_bin") == bin_axes);
    assert(manager->get_unpack_labels("per_bin") == bin_labels);
    // A field with no packed dimension carries no labels.
    assert(manager->get_unpack_labels("instant").empty());
    // INV-9: re-registration with the same contract but differing labels is rejected.
    bool label_mismatch_rejected = false;
    try {
        manager->register_field_contract("per_bin", "per bin", "kg", catchem::DiagType::FIELD_2D, bin_dims,
                                         catchem::DiagnosticPolicy::Instantaneous, 0.0, bin_axes,
                                         {"DUST1", "DUST2", "WRONG"});
    } catch (const std::invalid_argument&) {
        label_mismatch_rejected = true;
    }
    assert(label_mismatch_rejected);
    // INV-8: the leading axis of every process diagnostic must be Column.
    bool non_column_rejected = false;
    try {
        manager->register_field_contract("bad_axis", "bad", "kg", catchem::DiagType::FIELD_2D, bin_dims,
                                         catchem::DiagnosticPolicy::Instantaneous, 0.0,
                                         {catchem::SemanticAxis::Level, catchem::SemanticAxis::Singleton});
    } catch (const std::invalid_argument&) {
        non_column_rejected = true;
    }
    assert(non_column_rejected);
    // Determinism: get_registered_names() yields insertion order, not hash order.
    const auto names = manager->get_registered_names();
    assert(names.size() == 4);
    assert(names[0] == "instant" && names[1] == "accumulated" && names[2] == "persistent" && names[3] == "per_bin");

    assert(catchem_core_destroy_checked(core_handle) == CATCHEM_SUCCESS);
    return 0;
}

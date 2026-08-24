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
    const std::vector<catchem::SemanticAxis> axes = {catchem::SemanticAxis::Column,
                                                     catchem::SemanticAxis::Singleton};
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
    } catch (const std::invalid_argument&) { mismatch_rejected = true; }
    assert(mismatch_rejected);
    assert(catchem_core_destroy_checked(core_handle) == CATCHEM_SUCCESS);
    return 0;
}

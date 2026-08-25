#include "catchem_api.hpp"
#include <cassert>
#include <vector>

int main() {
    void* legacy = catchem_core_create(2, 3, 1);
    void* checked = nullptr;
    assert(catchem_core_create_checked(2, 3, 1, &checked) == CATCHEM_SUCCESS);
    assert(legacy && checked);
    void* checked_state = nullptr;
    void* legacy_state = catchem_core_get_state_manager(legacy);
    assert(catchem_core_get_state_manager_checked(checked, &checked_state) == CATCHEM_SUCCESS);
    assert(legacy_state && checked_state);
    std::vector<double> legacy_temperature(6, 280.0), checked_temperature(6, 280.0);
    catchem_state_bind_3d(legacy_state, "T", legacy_temperature.data());
    assert(catchem_state_bind_3d_checked(checked_state, "T", checked_temperature.data(), 2, 3, 1) == CATCHEM_SUCCESS);
    assert(catchem_state_get_pointer_3d(legacy_state, "T") == legacy_temperature.data());
    assert(catchem_state_get_pointer_3d(checked_state, "T") == checked_temperature.data());
    catchem_core_destroy(legacy);
    assert(catchem_core_destroy_checked(checked) == CATCHEM_SUCCESS);
    return 0;
}

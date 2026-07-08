#pragma once
#include <unordered_map>
#include <string>
#include <memory>
#include <vector>
#include "catchem_interop_field.hpp"

namespace catchem {

class StateManager {
public:
    int n_cols;
    int n_levels;
    int n_species;

    std::unordered_map<std::string, std::shared_ptr<InteropField<double, 1>>> fields_1d;
    std::unordered_map<std::string, std::shared_ptr<InteropField<double, 2>>> fields_2d;
    std::unordered_map<std::string, std::shared_ptr<InteropField<double, 3>>> fields_3d;

    StateManager(int nc, int nl, int ns) : n_cols(nc), n_levels(nl), n_species(ns) {}

    void bind_field_1d(const std::string& name, double* ptr) {
        fields_1d[name] = std::make_shared<InteropField<double, 1>>(ptr, std::vector<int>{n_cols});
    }

    void bind_field_2d(const std::string& name, double* ptr) {
        fields_2d[name] = std::make_shared<InteropField<double, 2>>(ptr, std::vector<int>{n_cols, n_levels});
    }

    void bind_field_3d(const std::string& name, double* ptr) {
        fields_3d[name] = std::make_shared<InteropField<double, 3>>(ptr, std::vector<int>{n_cols, n_levels, n_species});
    }

    void sync_to_device() {
        for (auto& [k, v] : fields_1d) v->sync_to_device();
        for (auto& [k, v] : fields_2d) v->sync_to_device();
        for (auto& [k, v] : fields_3d) v->sync_to_device();
    }

    void sync_to_host() {
        for (auto& [k, v] : fields_1d) v->sync_to_host();
        for (auto& [k, v] : fields_2d) v->sync_to_host();
        for (auto& [k, v] : fields_3d) v->sync_to_host();
    }
};

} // namespace catchem

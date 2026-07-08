#pragma once
#include <unordered_map>
#include <string>
#include <memory>
#include <vector>
#include <yaml-cpp/yaml.h>
#include "catchem_interop_field.hpp"
#include "catchem_met_state.hpp"
#include "catchem_chem_state.hpp"
#include "catchem_time_state.hpp"

namespace catchem {

class StateManager {
public:
    int n_cols;
    int n_levels;
    int n_species;

    std::unordered_map<std::string, std::shared_ptr<InteropField<double, 1>>> fields_1d;
    std::unordered_map<std::string, std::shared_ptr<InteropField<double, 2>>> fields_2d;
    std::unordered_map<std::string, std::shared_ptr<InteropField<double, 3>>> fields_3d;

    // Structured sub-states
    MetState met;
    ChemState chem;
    TimeState time;

    StateManager(int nc, int nl, int ns) : n_cols(nc), n_levels(nl), n_species(ns) {}

    void load_species_config(const std::string& filename) {
        chem.load_species_config(filename);
    }

    void bind_met_field_2d(const std::string& name, double* ptr) {
        auto field = std::make_shared<InteropField<double, 2>>(ptr, std::vector<int>{n_cols, n_levels});
        if (name == "PS") met.PS = field;
        else if (name == "TS") met.TS = field;
        else if (name == "PBLH") met.PBLH = field;
        else if (name == "USTAR") met.USTAR = field;
        else if (name == "HFLUX") met.HFLUX = field;
        else if (name == "OBK") met.OBK = field;
        else if (name == "LAT") met.LAT = field;
        else if (name == "LON") met.LON = field;
        met.fields_2d[name] = field;
    }

    void bind_met_field_3d(const std::string& name, double* ptr) {
        auto field = std::make_shared<InteropField<double, 3>>(ptr, std::vector<int>{n_cols, n_levels, 1}); // Using 1 for single-field layout
        if (name == "T") met.T = field;
        else if (name == "QV") met.QV = field;
        else if (name == "RH") met.RH = field;
        else if (name == "PMID") met.PMID = field;
        else if (name == "PEDGE") met.PEDGE = field;
        else if (name == "AIRDEN") met.AIRDEN = field;
        else if (name == "AIRDEN_DRY") met.AIRDEN_DRY = field;
        else if (name == "BXHEIGHT") met.BXHEIGHT = field;
        met.fields_3d[name] = field;
    }

    void bind_unified_chemistry(double* ptr) {
        chem.conc = std::make_shared<InteropField<double, 3>>(ptr, std::vector<int>{n_cols, n_levels, n_species});
    }

    void sync_to_device() {
        for (auto& [k, v] : fields_1d) v->sync_to_device();
        for (auto& [k, v] : fields_2d) v->sync_to_device();
        for (auto& [k, v] : fields_3d) v->sync_to_device();
        for (auto& [k, v] : met.fields_2d) v->sync_to_device();
        for (auto& [k, v] : met.fields_3d) v->sync_to_device();
        if (chem.conc) chem.conc->sync_to_device();
    }

    void sync_to_host() {
        for (auto& [k, v] : fields_1d) v->sync_to_host();
        for (auto& [k, v] : fields_2d) v->sync_to_host();
        for (auto& [k, v] : fields_3d) v->sync_to_host();
        for (auto& [k, v] : met.fields_2d) v->sync_to_host();
        for (auto& [k, v] : met.fields_3d) v->sync_to_host();
        if (chem.conc) chem.conc->sync_to_host();
    }

    void bind_field_1d(const std::string& name, double* ptr) {
        fields_1d[name] = std::make_shared<InteropField<double, 1>>(ptr, std::vector<int>{n_cols});
    }

    void bind_field_2d(const std::string& name, double* ptr) {
        fields_2d[name] = std::make_shared<InteropField<double, 2>>(ptr, std::vector<int>{n_cols, n_levels});
    }

    void bind_field_3d(const std::string& name, double* ptr) {
        fields_3d[name] = std::make_shared<InteropField<double, 3>>(ptr, std::vector<int>{n_cols, n_levels, n_species});
    }

    double* get_host_pointer_1d(const std::string& name) {
        if (fields_1d.find(name) == fields_1d.end()) return nullptr;
        return fields_1d.at(name)->host_view.data();
    }

    double* get_host_pointer_2d(const std::string& name) {
        if (fields_2d.find(name) != fields_2d.end()) return fields_2d.at(name)->host_view.data();
        if (met.fields_2d.find(name) != met.fields_2d.end()) return met.fields_2d.at(name)->host_view.data();
        return nullptr;
    }

    double* get_host_pointer_3d(const std::string& name) {
        if (fields_3d.find(name) != fields_3d.end()) return fields_3d.at(name)->host_view.data();
        if (met.fields_3d.find(name) != met.fields_3d.end()) return met.fields_3d.at(name)->host_view.data();
        return nullptr;
    }
};

} // namespace catchem

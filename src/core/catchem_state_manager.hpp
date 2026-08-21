#pragma once
#include "catchem_chem_state.hpp"
#include "catchem_config_manager.hpp"
#include "catchem_constants.hpp"
#include "catchem_interop_field.hpp"
#include "catchem_met_state.hpp"
#include "catchem_met_utilities.hpp"
#include "catchem_time_state.hpp"
#include <iostream>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>
#include <yaml-cpp/yaml.h>

namespace catchem {

    class DiagnosticManager;

    class StateManager {
    public:
        int n_cols;
        int n_levels;
        int n_species;

        std::string config_file_path;
        std::string trace_id;

        std::shared_ptr<ConfigManager> config_mgr;
        std::shared_ptr<DiagnosticManager> diag_mgr;

        std::unordered_map<std::string, std::shared_ptr<InteropField<double, 1>>> fields_1d;
        std::unordered_map<std::string, std::shared_ptr<InteropField<double, 2>>> fields_2d;
        std::unordered_map<std::string, std::shared_ptr<InteropField<double, 3>>> fields_3d;

        // Structured sub-states
        MetState met;
        ChemState chem;
        TimeState time;

        StateManager(int nc, int nl, int ns);

        void load_species_config(const std::string& filename) { chem.load_species_config(filename, config_mgr.get()); }

        void bind_met_field_2d(const std::string& name, double* ptr) {
            auto field = met.get_2d({name.c_str()});
            if (field) {
                field->update_host_pointer(ptr);
            } else {
                auto new_field = std::make_shared<InteropField<double, 2>>(ptr, std::vector<int>{n_cols, 1});
                met.bind_2d_field(name, new_field);
            }
        }

        void bind_met_field_3d(const std::string& name, double* ptr) {
            auto field = met.get_3d({name.c_str()});
            if (field) {
                field->update_host_pointer(ptr);
            } else {
                std::string upper_name = name;
                std::transform(upper_name.begin(), upper_name.end(), upper_name.begin(),
                               [](unsigned char c) { return std::toupper(c); });
                int nl = (upper_name == "PEDGE") ? n_levels + 1 : n_levels;
                auto new_field = std::make_shared<InteropField<double, 3>>(
                    ptr, std::vector<int>{n_cols, nl, 1}); // Using 1 for single-field layout
                met.bind_3d_field(name, new_field);
            }
        }

        double* find_3d_ptr(std::initializer_list<const char*> names) const {
            auto f = met.get_3d(names);
            if (f)
                return f->host_data();
            if (met.AIRDEN)
                return met.AIRDEN->host_data();
            if (met.AIRDEN_DRY)
                return met.AIRDEN_DRY->host_data();
            return nullptr;
        }

        double* find_2d_ptr(std::initializer_list<const char*> names) const {
            auto f = met.get_2d(names);
            if (f)
                return f->host_data();
            return nullptr;
        }

        void bind_unified_chemistry(double* ptr) {
            if (chem.conc) {
                chem.conc->update_host_pointer(ptr);
            } else {
                chem.conc =
                    std::make_shared<InteropField<double, 3>>(ptr, std::vector<int>{n_cols, n_levels, n_species});
            }
        }

        std::vector<std::shared_ptr<std::vector<double>>> owned_buffers;

        /**
         * @brief Derives layer thicknesses (BXHEIGHT / dz) hydrostatically.
         *
         * Computes the vertical distance of each grid cell based on pressure edges, temperature, and moisture content.
         */
        void derive_bxheight() {
            if (!met.PEDGE || !met.T)
                return;

            if (!met.BXHEIGHT) {
                auto buf = std::make_shared<std::vector<double>>(n_cols * n_levels, 0.0);
                owned_buffers.push_back(buf);
                bind_met_field_3d("BXHEIGHT", buf->data());
            }

            int nc = n_cols;
            int nl = n_levels;

            auto pedge = met.PEDGE->view();
            auto temp = met.T->view();
            auto bxheight = met.BXHEIGHT->view();

#ifdef CATCHEM_ENABLE_KOKKOS
            Kokkos::parallel_for(
                "derive_bxheight_kernel",
                Kokkos::MDRangePolicy<Kokkos::DefaultExecutionSpace, Kokkos::Rank<2>>({0, 0}, {nc, nl}),
                KOKKOS_LAMBDA(int icol, int ilev) {
                    double p_lower = pedge(icol, ilev, 0);
                    double p_upper = pedge(icol, ilev + 1, 0);

                    if (p_upper > 0.0 && p_lower > 0.0 && p_lower > p_upper) {
                        double q_val = met.QV ? met.QV->view()(icol, ilev, 0) : 0.0;
                        double virtual_t = met_utilities::virtual_temperature(temp(icol, ilev, 0), q_val);
                        bxheight(icol, ilev, 0) =
                            (constants::RD / constants::G0) * virtual_t * std::log(p_lower / p_upper);
                    } else {
                        bxheight(icol, ilev, 0) = 0.0;
                    }
                });
#else
            for (int icol = 0; icol < nc; ++icol) {
                for (int ilev = 0; ilev < nl; ++ilev) {
                    double p_lower = pedge(icol, ilev, 0);
                    double p_upper = pedge(icol, ilev + 1, 0);

                    if (p_upper > 0.0 && p_lower > 0.0 && p_lower > p_upper) {
                        double q_val = met.QV ? met.QV->view()(icol, ilev, 0) : 0.0;
                        double virtual_t = met_utilities::virtual_temperature(temp(icol, ilev, 0), q_val);
                        bxheight(icol, ilev, 0) =
                            (constants::RD / constants::G0) * virtual_t * std::log(p_lower / p_upper);
                    } else {
                        bxheight(icol, ilev, 0) = 0.0;
                    }
                }
            }
#endif
        }

        /**
         * @brief Derives dry air density (AIRDEN_DRY) using the Ideal Gas Law.
         *
         * Calculates dry air mass densities based on mid-point pressures, temperature, and specific humidity.
         */
        void derive_airden_dry() {
            if (!met.PMID || !met.T)
                return;

            if (!met.AIRDEN_DRY) {
                auto buf = std::make_shared<std::vector<double>>(n_cols * n_levels, 0.0);
                owned_buffers.push_back(buf);
                bind_met_field_3d("AIRDEN_DRY", buf->data());
            }

            int nc = n_cols;
            int nl = n_levels;

            auto pmid = met.PMID->view();
            auto temp = met.T->view();
            auto airden_dry = met.AIRDEN_DRY->view();

#ifdef CATCHEM_ENABLE_KOKKOS
            Kokkos::parallel_for(
                "derive_airden_dry_kernel",
                Kokkos::MDRangePolicy<Kokkos::DefaultExecutionSpace, Kokkos::Rank<2>>({0, 0}, {nc, nl}),
                KOKKOS_LAMBDA(int icol, int ilev) {
                    double q = met.QV ? met.QV->view()(icol, ilev, 0) : 0.0;
                    if (q >= 1.0)
                        q = 0.9999;
                    double avgw = (constants::AIR_MW / constants::H2O_MW) * q / (1.0 - q);
                    double xh2o = avgw / (1.0 + avgw);

                    double p_dry = pmid(icol, ilev, 0) * (1.0 - xh2o);
                    double t_val = temp(icol, ilev, 0);
                    if (t_val <= 0.0)
                        t_val = 1.0;
                    airden_dry(icol, ilev, 0) = p_dry / (constants::RD * t_val);
                });
#else
            for (int icol = 0; icol < nc; ++icol) {
                for (int ilev = 0; ilev < nl; ++ilev) {
                    double q = met.QV ? met.QV->view()(icol, ilev, 0) : 0.0;
                    if (q >= 1.0)
                        q = 0.9999;
                    double avgw = (constants::AIR_MW / constants::H2O_MW) * q / (1.0 - q);
                    double xh2o = avgw / (1.0 + avgw);

                    double p_dry = pmid(icol, ilev, 0) * (1.0 - xh2o);
                    double t_val = temp(icol, ilev, 0);
                    if (t_val <= 0.0)
                        t_val = 1.0;
                    airden_dry(icol, ilev, 0) = p_dry / (constants::RD * t_val);
                }
            }
#endif
        }

        void sync_to_device() {
            for (auto& [k, v] : fields_1d)
                v->sync_to_device();
            for (auto& [k, v] : fields_2d)
                v->sync_to_device();
            for (auto& [k, v] : fields_3d)
                v->sync_to_device();
            for (auto& [k, v] : met.fields_2d)
                v->sync_to_device();
            for (auto& [k, v] : met.fields_3d)
                v->sync_to_device();
            if (chem.conc)
                chem.conc->sync_to_device();
        }

        void sync_to_host() {
            for (auto& [k, v] : fields_1d)
                v->sync_to_host();
            for (auto& [k, v] : fields_2d)
                v->sync_to_host();
            for (auto& [k, v] : fields_3d)
                v->sync_to_host();
            for (auto& [k, v] : met.fields_2d)
                v->sync_to_host();
            for (auto& [k, v] : met.fields_3d)
                v->sync_to_host();
            if (chem.conc)
                chem.conc->sync_to_host();
        }

        void bind_field_1d(const std::string& name, double* ptr) {
            auto it = fields_1d.find(name);
            if (it != fields_1d.end() && it->second) {
                it->second->update_host_pointer(ptr);
            } else {
                fields_1d[name] = std::make_shared<InteropField<double, 1>>(ptr, std::vector<int>{n_cols});
            }
        }

        void bind_field_2d(const std::string& name, double* ptr) {
            auto it = fields_2d.find(name);
            if (it != fields_2d.end() && it->second) {
                it->second->update_host_pointer(ptr);
            } else {
                fields_2d[name] = std::make_shared<InteropField<double, 2>>(ptr, std::vector<int>{n_cols, 1});
            }
        }

        void bind_field_3d(const std::string& name, double* ptr) {
            auto it = fields_3d.find(name);
            if (it != fields_3d.end() && it->second) {
                it->second->update_host_pointer(ptr);
            } else {
                fields_3d[name] =
                    std::make_shared<InteropField<double, 3>>(ptr, std::vector<int>{n_cols, n_levels, n_species});
            }
        }

        double* get_host_pointer_1d(const std::string& name) {
            if (fields_1d.find(name) == fields_1d.end())
                return nullptr;
            return fields_1d.at(name)->host_data();
        }

        double* get_host_pointer_2d(const std::string& name) {
            if (fields_2d.find(name) != fields_2d.end())
                return fields_2d.at(name)->host_data();
            if (met.fields_2d.find(name) != met.fields_2d.end())
                return met.fields_2d.at(name)->host_data();
            return nullptr;
        }

        double* get_host_pointer_3d(const std::string& name) {
            if (fields_3d.find(name) != fields_3d.end())
                return fields_3d.at(name)->host_data();
            if (met.fields_3d.find(name) != met.fields_3d.end())
                return met.fields_3d.at(name)->host_data();
            return nullptr;
        }
    };

} // namespace catchem

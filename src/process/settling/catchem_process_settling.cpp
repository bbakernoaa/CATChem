#include "catchem_process_settling.hpp"
#include "catchem_diagnostic_manager.hpp"
#include "catchem_error.hpp"
#include "catchem_logger.hpp"
#include "catchem_process_registry.hpp"
#include "settling_gocart_kernels.hpp"
#ifdef CATCHEM_HAVE_EXAERO
#include "catchem_exaero_bridge.hpp"
#endif
#include <algorithm>
#include <cmath>
#include <filesystem>
#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>

namespace catchem {

    // Library-facing state of the native C++ settling engine (scheme
    // "gocart_cpp"), held behind a pimpl so this header stays free of the
    // aerosol-library includes.  The package handle is created during process
    // initialization and lives for the run; wet sizing is recomputed each
    // step from the current state.  The type is complete in every build so
    // the unique_ptr destructor is valid; it simply carries no members where
    // the aerosol utility is disabled.
    struct SettlingProcess::CppEngine {
#ifdef CATCHEM_HAVE_EXAERO
        catchem::exaero_util::ExaeroPackageHandle package;
        // Per settling species: index into the package's species axis.  Every
        // settling species must be named in the exaero configuration; a
        // species without a microphysical curve still resolves here and falls
        // back to the swelling parameterization per point at run time.
        std::vector<int> species_index;
#endif
    };

    extern "C" void run_settling_science_bridge(
        int n_columns, int n_levels, int n_aerosols, int n_total_species, double dt, double scale_factor,
        int swelling_method, int correction_maring, int maring_dust_only, const double* airden, const double* delp,
        const double* pmid, const double* rh, const double* temperature, const double* z_edge,
        const char* aerosol_species_names, const char* species_names, const int* species_is_dust, const double* radius,
        const double* density, double* concentration, int simple_scheme, const char* aerosol_mie_names,
        double* diag_velocity, double* diag_flux, const int* diagnostic_species_id, int n_diag_species, int* bridge_rc);

    // Loads the aerosol optics (Mie) tables named by the top-level "mie:" section
    // into the Fortran bridge store.  type_names/file_paths are fixed-width,
    // blank-padded character buffers packed contiguously (32 and 512 bytes per
    // entry).  init_rc receives the 1-based index of the first file that failed
    // to open, or 0 on success.  Idempotent: a second call replaces any
    // previously loaded set.
    extern "C" void run_settling_mie_init(int n_files, const char* type_names, const char* file_paths, int* init_rc);

    namespace {
        // Right-trim the blank padding of a fixed-width Fortran character buffer.
        std::string trim_trailing_spaces(std::string text) {
            text.erase(text.find_last_not_of(' ') + 1);
            return text;
        }
    } // namespace

    ProcessContract SettlingProcess::get_contract() const {
        // The current GOCART implementation invokes a Fortran science bridge
        // with host pointers.  Advertising these fields as device accesses
        // makes ExecutionPlan::complete() mark the device copy as the latest
        // writer after the bridge has modified host concentration memory.
        // A later host synchronization can then overwrite the settling result
        // (and the concentration presented to a coupled host) with stale data.
        return make_contract(get_name(),
                             {host_field_3d("T", "K"), host_field_3d("AIRDEN", "kg/m3"), host_field_3d("DELP", "Pa"),
                              host_field_3d("PMID", "Pa"), host_field_3d("RH", "1"), host_field_interface("Z", "m"),
                              host_concentration()});
    }

    SettlingProcess::SettlingProcess() : active_scheme("c++_kokkos"), fortran_callback(nullptr) {}

    // Defined here so the pimpl type above is complete at the destruction point.
    // The engine's package handle is released before the shared Kokkos
    // runtime it was created under; finalize() clears the same state, so the
    // null check makes double release impossible.
    SettlingProcess::~SettlingProcess() {
#ifdef CATCHEM_HAVE_EXAERO
        if (cpp_engine) {
            cpp_engine.reset();
            catchem::exaero_util::ExaeroPackageHandle::finalize_environment();
        }
#endif
    }

    void SettlingProcess::prepare_inputs(std::shared_ptr<StateManager> state) {
        // DELP, AIRDEN, and RH are optional host products. GOCART settling
        // requires all three, so make them current before the execution plan
        // validates this process contract.
        state->derive_delp();
        state->derive_airden();
        state->derive_relative_humidity();
    }

    void SettlingProcess::init(std::shared_ptr<StateManager> state) {
        const auto config = state->config_manager();
        if (!config)
            throw std::invalid_argument("Settling requires a runtime YAML configuration");
        const auto configured = config->data.processes.find("settling");
        if (configured == config->data.processes.end() ||
            (configured->second.scheme != "gocart" && configured->second.scheme != "gocart_cpp"))
            throw std::invalid_argument("Settling requires processes.settling.scheme: gocart or gocart_cpp");
        active_scheme = configured->second.scheme;
        use_cpp_engine = (active_scheme == "gocart_cpp");

        // Read scheme tuning options from the runtime YAML.  Each lookup falls
        // back to the compiled default declared in SettlingCommon_Mod.F90.
        gocart_scale_factor = configured->second.get_double("gocart/scale_factor", gocart_scale_factor);
        gocart_simple_scheme = configured->second.get_bool("gocart/simple_scheme", gocart_simple_scheme);
        gocart_swelling_method = configured->second.get_int("gocart/swelling_method", gocart_swelling_method);
        gocart_correction_maring = configured->second.get_bool("gocart/correction_maring", gocart_correction_maring);
        gocart_maring_dust_only = configured->second.get_bool("gocart/maring_dust_only", gocart_maring_dust_only);
        if (!(gocart_scale_factor > 0.0))
            throw std::invalid_argument("Settling gocart scale_factor must be positive");
        if (gocart_swelling_method != 1 && gocart_swelling_method != 2)
            throw std::invalid_argument("Settling gocart swelling_method must be 1 (Fitzgerald) or 2 (Gerber)");

        // The native C++ engine replaces the Fortran science bridge with the
        // ported settling kernels plus aerosol-library wet sizing.  It needs
        // the library at build time, so selecting it in a build without the
        // aerosol utility is a fatal configuration error rather than a silent
        // fallback to the Fortran path; and it implements the species-metadata
        // branch only, not the optics-table branch.
        std::string exaero_config_path;
        if (use_cpp_engine) {
#ifdef CATCHEM_HAVE_EXAERO
            if (gocart_simple_scheme)
                throw std::invalid_argument("Settling 'gocart_cpp' does not implement the optics-table path "
                                            "(gocart/simple_scheme: true); use scheme 'gocart' for Mie-table settling");
            exaero_config_path = configured->second.get_string("exaero_config", "");
            if (exaero_config_path.empty())
                throw std::invalid_argument("Settling scheme 'gocart_cpp' requires a species configuration file: "
                                            "set processes/settling/exaero_config to an exaero species YAML");
#else
            throw std::invalid_argument("Settling scheme 'gocart_cpp' requires the exaero aerosol utility: "
                                        "reconfigure with -DCATCHEM_BUILD_EXAERO=ON (and -DCATCHEM_ENABLE_KOKKOS=ON)");
#endif
        }

        diagnostics_enabled = configured->second.diagnostics;

        // Surface the effective scheme options so the run log confirms what
        // was parsed from the runtime YAML and reaches the settling kernel.
        // The metadata path corresponds to legacy simple_scheme: false.
        // simple_scheme: true restores the legacy optics-table path: the
        // NetCDF Mie tables named by the top-level "mie:" section are loaded
        // once here and the scheme selects Chem_SettlingSimple per species via
        // each species' __mie_name (specs/012).
        std::vector<std::string> mie_type_labels; // "<TYPE> -> <file>" for the log
        if (gocart_simple_scheme) {
            const auto& mie = config->data.mie;
            if (mie.files.empty())
                throw std::invalid_argument("Settling gocart/simple_scheme requires a non-empty 'mie.files' section "
                                            "mapping aerosol types to optics tables");
            const std::filesystem::path dir(mie.directory);
            if (mie.directory != "./" && !std::filesystem::is_directory(dir))
                throw std::invalid_argument("Settling optics directory is not a readable directory: '" + mie.directory +
                                            "'");
            std::vector<char> type_names, file_paths;
            type_names.reserve(mie.files.size() * 32);
            file_paths.reserve(mie.files.size() * 512);
            for (const auto& [type, file] : mie.files) {
                const std::filesystem::path joined = mie.directory == "./" ? std::filesystem::path(file) : dir / file;
                std::error_code ec;
                if (!std::filesystem::exists(joined, ec))
                    throw std::invalid_argument("Settling optics table for aerosol type '" + type + "' not found at '" +
                                                joined.string() + "'");
                type_names.insert(type_names.end(), 32, ' ');
                std::copy_n(type.begin(), std::min<size_t>(type.size(), 32), type_names.end() - 32);
                file_paths.insert(file_paths.end(), 512, ' ');
                const std::string path_str = joined.string();
                std::copy_n(path_str.begin(), std::min<size_t>(path_str.size(), 511), file_paths.end() - 512);
                mie_type_labels.push_back(type + " -> " + path_str);
            }
            int init_rc = 0;
            run_settling_mie_init(static_cast<int>(mie.files.size()), type_names.data(), file_paths.data(), &init_rc);
            if (init_rc != 0) {
                const auto& failed = mie.files[static_cast<size_t>(init_rc) - 1];
                throw std::runtime_error("Settling optics table load failed for file #" + std::to_string(init_rc) +
                                         " ('" + failed.first + "': " + failed.second + ")");
            }
            mie_initialized = true;
        }
        Logger::debug(
            state.get(), "Settling scheme options",
            {{"scheme", active_scheme},
             {"gocart/scale_factor", std::to_string(gocart_scale_factor)},
             {"gocart/correction_maring", gocart_correction_maring ? "true" : "false"},
             {"gocart/maring_dust_only", gocart_maring_dust_only ? "true" : "false"},
             {"gocart/simple_scheme", gocart_simple_scheme ? "true (optics tables)" : "false (species metadata)"},
             {"gocart/swelling",
              gocart_simple_scheme ? "optics-table rEff(rh) LUT" : "per-species __hydrophilic (Gerber when true)"},
             {"gocart/swelling_method", std::to_string(gocart_swelling_method)},
             {"gocart/optics_directory", gocart_simple_scheme ? config->data.mie.directory : "(unused)"},
             {"gocart/optics_tables", gocart_simple_scheme ? std::to_string(mie_type_labels.size()) : "0"}});
        for (const auto& label : mie_type_labels)
            Logger::debug(state.get(), "Settling optics table", {{"binding", label}});

        int num_aerosols = state->chemistry().settling_indices.size();
        if (num_aerosols > 0) {
            aerosol_species_names.assign(static_cast<size_t>(num_aerosols) * 32, ' ');
            aerosol_mie_names.assign(static_cast<size_t>(num_aerosols) * 32, ' ');
            host_radius_dry.assign(num_aerosols, 0.0);
            host_rhop_dry.assign(num_aerosols, 0.0);
            host_is_dust.assign(num_aerosols, 0);

            for (int i = 0; i < num_aerosols; ++i) {
                int ispec = state->chemistry().settling_indices[i];
                double r_val = state->chemistry().species_list[ispec].radius;
                double d_val = state->chemistry().species_list[ispec].density;
                if (!(r_val > 0.0 && d_val > 0.0))
                    throw std::runtime_error("Settling aerosol '" + state->chemistry().species_list[ispec].short_name +
                                             "' requires explicit radius and density");
                // Radii are configured in micrometres and cross the bridge in
                // micrometres; the legacy scheme performs the µm -> m conversion.
                host_radius_dry[i] = r_val;
                host_rhop_dry[i] = d_val;
                host_is_dust[i] = state->chemistry().species_list[ispec].is_dust ? 1 : 0;
                std::copy_n(state->chemistry().species_names_c_arr.data() + static_cast<size_t>(ispec) * 32, 32,
                            aerosol_species_names.data() + static_cast<size_t>(i) * 32);
                const std::string& mie_name = state->chemistry().species_list[ispec].mie_name;
                std::copy_n(mie_name.begin(), std::min<size_t>(mie_name.size(), 32),
                            aerosol_mie_names.data() + static_cast<size_t>(i) * 32);
            }

            if (gocart_simple_scheme) {
                // Defensive C++-side pre-resolution so misconfiguration names the
                // species at init instead of surfacing as the Fortran bridge_rc==2
                // backstop at the first step (specs/012 FR-009).
                for (int i = 0; i < num_aerosols; ++i) {
                    const int ispec = state->chemistry().settling_indices[i];
                    const std::string species_name = state->chemistry().species_list[ispec].short_name;
                    const std::string trimmed =
                        trim_trailing_spaces(std::string(aerosol_mie_names.data() + static_cast<size_t>(i) * 32, 32));
                    if (trimmed.empty())
                        throw std::invalid_argument("Settling simple_scheme requires __mie_name on species '" +
                                                    species_name + "' but it is empty");
                    bool resolved = false;
                    for (const auto& [type, file] : config->data.mie.files)
                        if (type == trimmed) {
                            resolved = true;
                            break;
                        }
                    if (!resolved)
                        throw std::invalid_argument("Settling species '" + species_name + "' has __mie_name '" +
                                                    trimmed + "' which matches no configured mie.files entry");
                }
            }
        } else if (gocart_simple_scheme) {
            throw std::invalid_argument("Settling simple_scheme is enabled but no aerosol species are configured");
        }

        // --- Per-process diagnostics (parity with legacy) ------------------
        // diagnostic_species_id indexes the aerosol subset (1..num_aerosols),
        // which is the species_idx space compute_gocart iterates.
        {
            const auto& aerosol_idx = state->chemistry().settling_indices;
            const auto& settings = configured->second;
            std::vector<int> selected_local; // 1-based positions into aerosol subset
            if (!settings.diag_species.empty()) {
                for (const auto& name : settings.diag_species) {
                    int found = -1;
                    for (size_t a = 0; a < aerosol_idx.size(); ++a) {
                        if (state->chemistry().species_list[aerosol_idx[a]].short_name == name) {
                            found = static_cast<int>(a) + 1;
                            break;
                        }
                    }
                    if (found < 0)
                        throw std::invalid_argument("Settling diag_species names a non-settling species: " + name);
                    selected_local.push_back(found);
                }
            } else {
                for (size_t a = 0; a < aerosol_idx.size(); ++a)
                    selected_local.push_back(static_cast<int>(a) + 1);
            }
            diagnostic_species_id = selected_local;
            diagnostic_species_names.clear();
            for (int local : selected_local)
                diagnostic_species_names.push_back(
                    state->chemistry().species_list[aerosol_idx[static_cast<size_t>(local) - 1]].short_name);

            if (diagnostics_enabled && state->diagnostic_manager() && !selected_local.empty()) {
                const int ndiag = static_cast<int>(selected_local.size());
                std::vector<int> dims_vel = {state->column_count(), state->level_count(), ndiag};
                std::vector<int> dims_flux = {state->column_count(), ndiag};
                // diagnostic_species_names is the packed-axis label list, built in
                // the same aerosol-subset order the scheme iterates (FR-006); the
                // NUOPC driver unpacks each field into one named variable per
                // species (feature 013).
                const std::vector<SemanticAxis> axes_vel = {SemanticAxis::Column, SemanticAxis::Level,
                                                            SemanticAxis::Species};
                const std::vector<SemanticAxis> axes_flux = {SemanticAxis::Column, SemanticAxis::Species};
                state->diagnostic_manager()->register_field_contract(
                    "settling_velocity_per_species_per_level", "Settling velocity", "m/s", DiagType::FIELD_3D, dims_vel,
                    DiagnosticPolicy::Instantaneous, 0.0, axes_vel, diagnostic_species_names);
                state->diagnostic_manager()->register_field_contract(
                    "settling_flux_per_species", "Settling column flux", "kg/m2/s", DiagType::FIELD_2D, dims_flux,
                    DiagnosticPolicy::Instantaneous, 0.0, axes_flux, diagnostic_species_names);
            }
        }

        // Bring up the aerosol library for the native engine last, after every
        // configuration check above has passed, so a rejected run never leaves
        // a Kokkos runtime or package instance behind.
        if (use_cpp_engine) {
#ifdef CATCHEM_HAVE_EXAERO
            init_cpp_engine(state, exaero_config_path);
#endif
        }
    }

    void SettlingProcess::set_fortran_bridge_callback(std::function<void(void*)> cb) {
        fortran_callback = cb;
    }

#ifdef CATCHEM_HAVE_EXAERO
    // Bring up the aerosol library for the native settling engine.  Order
    // matters: the shared Kokkos runtime first (reference-counted), then the
    // package from the configured species YAML, then species resolution.  Any
    // failure releases what was acquired and throws with the library's own
    // message -- never a half-initialized engine.
    void SettlingProcess::init_cpp_engine(std::shared_ptr<StateManager> state, const std::string& config_path) {
        namespace su = catchem::exaero_util;
        auto engine = std::make_unique<CppEngine>();
        su::ExaeroPackageHandle::init_environment();
        std::string error;
        if (!engine->package.initialize_from_file(config_path, error)) {
            su::ExaeroPackageHandle::finalize_environment();
            throw std::runtime_error("Settling 'gocart_cpp' could not initialize the exaero package from '" +
                                     config_path + "': " + error);
        }
        // Resolve each settling species into the package by its canonical
        // aerosol-type label (__mie_name: DU, SS, SU, BC, OC, ...), not its
        // transport short name (dust1, seas3, ...).  The exaero microphysical
        // curves are keyed by that same type label, so __mie_name is the join
        // that lets a species bind a real wet-sizing curve; the short name
        // would never match and every species would silently fall back to the
        // swelling parameterization.  A species whose type is absent from the
        // configuration is a configuration error, not a silent fallback: the
        // species metadata and the exaero YAML must agree.
        const int num_aerosols = static_cast<int>(host_radius_dry.size());
        engine->species_index.assign(static_cast<std::size_t>(num_aerosols), -1);
        for (int i = 0; i < num_aerosols; ++i) {
            const std::string mie_name =
                trim_trailing_spaces(std::string(aerosol_mie_names.data() + static_cast<std::size_t>(i) * 32, 32));
            const std::string short_name =
                trim_trailing_spaces(std::string(aerosol_species_names.data() + static_cast<std::size_t>(i) * 32, 32));
            const int idx = engine->package.species_index(mie_name);
            if (idx < 0) {
                su::ExaeroPackageHandle::finalize_environment();
                throw std::runtime_error("Settling 'gocart_cpp': species '" + short_name + "' has __mie_name '" +
                                         mie_name + "' which is not defined in the exaero configuration '" +
                                         config_path + "'");
            }
            engine->species_index[static_cast<std::size_t>(i)] = idx;
        }
        Logger::debug(state.get(), "Settling native engine ready",
                      {{"scheme", active_scheme},
                       {"exaero_config", config_path},
                       {"exaero_species", std::to_string(engine->species_index.size())}});
        cpp_engine = std::move(engine);
    }

    // One settling step with the ported science.  The structure mirrors the
    // Fortran scheme exactly: per species, request wet sizing over the whole
    // (column, level) block in host order, then per column reverse the
    // meteorology to top-to-bottom, derive dz and the pressure-lid index,
    // convert ug/kg -> kg/kg, size the particle (aerosol-library wet sizing
    // where a curve exists, the configured swelling parameterization
    // otherwise), compute fall speeds from the lid down, integrate the UFS
    // transport solver over the full column, and write replacement
    // tendencies plus the per-species diagnostics.  Units, vertical reversal,
    // and the zero clamp on output match the Fortran bridge.
    void SettlingProcess::run_cpp_engine(std::shared_ptr<StateManager> state) {
        namespace su = catchem::exaero_util;
        namespace sci = catchem::settling;

        const int n_columns = state->column_count();
        const int n_levels = state->level_count();
        const int n_aerosols = static_cast<int>(host_radius_dry.size());
        const double dt = state->clock().timestep;
        if (n_columns <= 0 || n_levels <= 0 || n_aerosols <= 0)
            return;
        const std::size_t points = static_cast<std::size_t>(n_columns) * n_levels;

        // Host fields are (column, level[, species]) column-major with level
        // index 0 at the surface; the science is column-wise top-to-bottom, so
        // a top index k maps to host level n_levels-1-k throughout.
        const double* t_host = state->meteorology().T->host_data();
        const double* rhoa_host = state->meteorology().AIRDEN->host_data();
        const double* rh_host = state->meteorology().RH->host_data();
        const double* delp_host = state->read_field<3>("DELP");
        const double* pmid_host = state->read_field<3>("PMID");
        const double* z_host = state->read_field<3>("Z"); // (column, n_levels+1) edges, bottom-to-top
        double* conc = state->chemistry().conc->host_write();

        double* diag_velocity = nullptr;
        double* diag_flux = nullptr;
        if (diagnostics_enabled && state->diagnostic_manager() && !diagnostic_species_id.empty()) {
            diag_velocity = static_cast<double*>(
                state->diagnostic_manager()->get_host_pointer("settling_velocity_per_species_per_level"));
            diag_flux =
                static_cast<double*>(state->diagnostic_manager()->get_host_pointer("settling_flux_per_species"));
        }

        std::vector<double> radius_block(points, 0.0), density_block(points, 0.0);
        std::vector<int> status(points, 0);
        std::vector<double> qa(static_cast<std::size_t>(n_levels));
        std::vector<double> radius_wet(static_cast<std::size_t>(n_levels));
        std::vector<double> rhop_wet(static_cast<std::size_t>(n_levels));
        std::vector<double> vs(static_cast<std::size_t>(n_levels), 0.0);
        std::vector<double> dz_col(static_cast<std::size_t>(n_levels));
        std::vector<double> pmid_col(static_cast<std::size_t>(n_levels));
        std::vector<double> delp_col(static_cast<std::size_t>(n_levels));
        std::vector<double> t_col(static_cast<std::size_t>(n_levels));
        std::vector<double> rhoa_col(static_cast<std::size_t>(n_levels));
        std::vector<double> rh_col(static_cast<std::size_t>(n_levels));
        std::vector<double> z_col(static_cast<std::size_t>(n_levels) + 1);

        for (int s = 0; s < n_aerosols; ++s) {
            const std::size_t sidx = static_cast<std::size_t>(s);
            const int ispec = state->chemistry().settling_indices[sidx];
            const std::string species_name = state->chemistry().species_list[static_cast<std::size_t>(ispec)].short_name;

            // Per-species Maring switch, identical to the scheme logic.
            const bool apply_maring =
                (gocart_maring_dust_only && host_is_dust[sidx] != 0) ||
                (!gocart_maring_dust_only && gocart_correction_maring);

            const double r_dry_m = host_radius_dry[sidx] * 1.0e-6; // configured µm -> m, as the Fortran call does
            const double rhop_dry = host_rhop_dry[sidx];

            // Anchored wet sizing for this species over the whole block: the
            // library resolves its curve's growth factor at each point's
            // relative humidity and applies it to the model's own dry
            // particle, so a hydrophobic species (growth 1) keeps the dry
            // radius and density exactly and the port matches the Fortran
            // baseline to roundoff.  Points whose species has no curve report
            // an unavailable status and fall back to the configured swelling
            // parameterization below, again matching the Fortran path.
            std::string error;
            if (!cpp_engine->package.compute_wet_sizing(n_columns, n_levels, rh_host, r_dry_m, rhop_dry,
                                                         cpp_engine->species_index[sidx], radius_block,
                                                         density_block, status, error))
                throw std::runtime_error("Settling 'gocart_cpp' wet sizing failed for species '" + species_name +
                                         "': " + error);

            for (int column = 0; column < n_columns; ++column) {
                const std::size_t cbase = static_cast<std::size_t>(column);
                // Reverse the meteorology to top-to-bottom (PrepMetVarsForGOCART).
                for (int k = 0; k < n_levels; ++k) {
                    const std::size_t host_point = cbase + static_cast<std::size_t>(n_columns) * (n_levels - 1 - k);
                    t_col[static_cast<std::size_t>(k)] = t_host[host_point];
                    rhoa_col[static_cast<std::size_t>(k)] = rhoa_host[host_point];
                    rh_col[static_cast<std::size_t>(k)] = rh_host[host_point];
                    delp_col[static_cast<std::size_t>(k)] = delp_host[host_point];
                    pmid_col[static_cast<std::size_t>(k)] = pmid_host[host_point];
                    // Mixing ratio with the unit conversion the scheme applies.
                    qa[static_cast<std::size_t>(k)] = conc[host_point + static_cast<std::size_t>(ispec) * points] * 1.0e-9;
                }
                for (int j = 0; j <= n_levels; ++j)
                    z_col[static_cast<std::size_t>(j)] =
                        z_host[cbase + static_cast<std::size_t>(n_columns) * (n_levels - j)];

                sci::layer_thickness(z_col.data(), n_levels, dz_col.data());
                const int klid0 = sci::find_klid(pmid_col.data(), n_levels, sci::kPressureLidHpa);

                // Wet radius/density per level.  The Fortran baseline resizes a
                // particle only where its configured swelling method is active
                // (Fitzgerald above the 0.80 deliquescence threshold, Gerber at
                // all RH).  Elsewhere the baseline leaves every species dry, so
                // the engine must too -- applying the library curve below the
                // activation gate would swell a particle the Fortran scheme
                // keeps dry and break replication.  Where swelling is active the
                // library's anchored sizing sets both from the caller's dry
                // particle for a species it has a curve for; without a curve the
                // configured swelling parameterization applies, matching the
                // Fortran metadata path.
                for (int k = 0; k < n_levels; ++k) {
                    const std::size_t host_point = cbase + static_cast<std::size_t>(n_columns) * (n_levels - 1 - k);
                    if (sci::swelling_active(rh_col[static_cast<std::size_t>(k)], gocart_swelling_method) &&
                        su::ExaeroPackageHandle::attribute_value_ok(status[host_point])) {
                        radius_wet[static_cast<std::size_t>(k)] = radius_block[host_point];
                        rhop_wet[static_cast<std::size_t>(k)] = density_block[host_point];
                    }
                    else {
                        sci::swell(r_dry_m, rhop_dry, rh_col[static_cast<std::size_t>(k)], gocart_swelling_method,
                                   &radius_wet[static_cast<std::size_t>(k)], &rhop_wet[static_cast<std::size_t>(k)]);
                    }
                }

                // Fall speed from the lid down; levels above stay zero, as in
                // the Fortran driver (whose whole-array Maring pass then lifts
                // them to the 1e-9 floor when the correction is on).
                for (int k = 0; k < n_levels; ++k)
                    vs[static_cast<std::size_t>(k)] =
                        (k >= klid0)
                            ? sci::calc_vsettle(radius_wet[static_cast<std::size_t>(k)],
                                                rhop_wet[static_cast<std::size_t>(k)],
                                                rhoa_col[static_cast<std::size_t>(k)],
                                                t_col[static_cast<std::size_t>(k)], sci::kGravity)
                            : 0.0;
                if (apply_maring)
                    sci::apply_maring_correction(vs.data(), n_levels);

                const double cmass_before = sci::column_mass(qa.data(), delp_col.data(), klid0, n_levels, sci::kGravity);
                sci::settling_solver_ufs(dt, delp_col.data(), dz_col.data(), vs.data(), qa.data(), n_levels);
                const double cmass_after = sci::column_mass(qa.data(), delp_col.data(), klid0, n_levels, sci::kGravity);
                const double flux = (cmass_before - cmass_after) / dt;

                // Write-back: reverse to host order, kg/kg -> ug/kg, clamped at
                // zero exactly like the bridge's max(0, tend) convention.
                for (int k = 0; k < n_levels; ++k) {
                    const std::size_t host_point = cbase + static_cast<std::size_t>(n_columns) * (n_levels - 1 - k);
                    const double tendency = std::max(0.0, qa[static_cast<std::size_t>(k)] * 1.0e9);
                    conc[host_point + static_cast<std::size_t>(ispec) * points] = tendency;
                    if (diag_velocity) {
                        // The scheme stores velocity with the level flip, so
                        // host level 0 (surface) takes the bottom entry; same
                        // layout the Fortran bridge produces.
                        for (size_t d = 0; d < diagnostic_species_id.size(); ++d) {
                            if (diagnostic_species_id[d] == s + 1) {
                                diag_velocity[cbase + static_cast<std::size_t>(n_columns) *
                                              ((n_levels - 1 - k) + static_cast<std::size_t>(n_levels) * d)] =
                                    vs[static_cast<std::size_t>(k)];
                                break;
                            }
                        }
                    }
                }
                if (diag_flux) {
                    for (size_t d = 0; d < diagnostic_species_id.size(); ++d) {
                        if (diagnostic_species_id[d] == s + 1) {
                            diag_flux[cbase + static_cast<std::size_t>(n_columns) * d] = flux;
                            break;
                        }
                    }
                }
            }
        }
    }
#endif

    void SettlingProcess::run(std::shared_ptr<StateManager> state) {
        if (fortran_callback) {
            // Fallback for tests explicitly requesting the Fortran bridge
            fortran_callback(static_cast<void*>(state.get()));
            if (state->chemistry().conc)
                state->chemistry().conc->mark_host_modified();
            return;
        }

        // The execution plan invokes prepare_inputs before run(), but direct
        // API users and focused process tests may call run() themselves.
        // These derivations are generation-aware and therefore preserve
        // host-provided fields while supplying only absent prerequisites.
        prepare_inputs(state);

        int num_aerosols = state->chemistry().settling_indices.size();
        if (num_aerosols == 0) {
            Logger::info(state.get(), "Settling skipped: no aerosol species registered", {});
            return;
        }

        require_field_pointer("Settling", "T", state->meteorology().T ? state->meteorology().T->host_data() : nullptr);
        require_field_pointer("Settling", "AIRDEN",
                              state->meteorology().AIRDEN ? state->meteorology().AIRDEN->host_data() : nullptr);
        const double* delp = state->read_field<3>("DELP");
        const double* z_edge = state->read_field<3>("Z");
        const double* pmid = state->read_field<3>("PMID");
        require_field_pointer("Settling", "DELP", delp);
        require_field_pointer("Settling", "PMID", pmid);
        require_field_pointer("Settling", "RH",
                              state->meteorology().RH ? state->meteorology().RH->host_data() : nullptr);
        require_field_pointer("Settling", "Z", z_edge);
        require_field_pointer("Settling", "CHEM_CONC",
                              state->chemistry().conc ? state->chemistry().conc->host_data() : nullptr);

#ifdef CATCHEM_HAVE_EXAERO
        // The native engine replaces the Fortran bridge entirely; it consumes
        // the same validated host fields and writes the same outputs.
        if (use_cpp_engine) {
            run_cpp_engine(state);
            state->chemistry().conc->mark_host_modified();
            return;
        }
#endif

        int bridge_rc = 0;
        double* diag_velocity = nullptr;
        double* diag_flux = nullptr;
        if (diagnostics_enabled && state->diagnostic_manager() && !diagnostic_species_id.empty()) {
            diag_velocity = static_cast<double*>(
                state->diagnostic_manager()->get_host_pointer("settling_velocity_per_species_per_level"));
            diag_flux =
                static_cast<double*>(state->diagnostic_manager()->get_host_pointer("settling_flux_per_species"));
        }
        const int n_diag_species = diagnostics_enabled ? static_cast<int>(diagnostic_species_id.size()) : 0;
        // The Fortran dummy argument is declared diagnostic_species_id(max(n_diag_species,1)),
        // so the bridge always receives a size-1 intent(in) array even when the count is zero
        // (it is never dereferenced in that case).  std::vector::data() of an empty vector may
        // be nullptr, which would form a Fortran pointer to nothing; pass a valid dummy instead.
        static const int no_diag_species = 0;
        const int* diag_ids = diagnostic_species_id.empty() ? &no_diag_species : diagnostic_species_id.data();

        run_settling_science_bridge(
            state->column_count(), state->level_count(), num_aerosols, state->species_count(), state->clock().timestep,
            gocart_scale_factor, gocart_swelling_method, gocart_correction_maring ? 1 : 0,
            gocart_maring_dust_only ? 1 : 0, state->meteorology().AIRDEN->host_data(), delp, pmid,
            state->meteorology().RH->host_data(), state->meteorology().T->host_data(), z_edge,
            aerosol_species_names.data(), state->chemistry().species_names_c_arr.data(), host_is_dust.data(),
            host_radius_dry.data(), host_rhop_dry.data(), state->chemistry().conc->host_write(),
            gocart_simple_scheme ? 1 : 0, aerosol_mie_names.data(), diag_velocity, diag_flux, diag_ids, n_diag_species,
            &bridge_rc);
        if (bridge_rc == 2)
            throw std::runtime_error(
                "Settling optics-table mapping failed: a settling species did not resolve to a loaded Mie table");
        if (bridge_rc != 0)
            throw std::runtime_error("Settling science bridge failed with status " + std::to_string(bridge_rc));
        state->chemistry().conc->mark_host_modified();
    }

    void SettlingProcess::finalize() {
        // Kokkos views will be deallocated automatically when their reference count goes to zero.
#ifdef CATCHEM_HAVE_EXAERO
        // Release the native engine first (its package handle must die before
        // the runtime it was created under), then the shared environment.
        if (cpp_engine) {
            cpp_engine.reset();
            catchem::exaero_util::ExaeroPackageHandle::finalize_environment();
        }
#endif
    }

} // namespace catchem

extern "C" {
void catchem_register_settling_cpp() {
    catchem::ProcessRegistry::get_instance().register_process(
        "settling", []() { return std::make_shared<catchem::SettlingProcess>(); }, {},
        catchem::make_settings_validator("settling",
                                         {"gocart/scale_factor", "gocart/simple_scheme", "gocart/swelling_method",
                                          "gocart/correction_maring", "gocart/maring_dust_only"}));
}
}

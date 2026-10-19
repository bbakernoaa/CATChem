/**
 * @file catchem_manifest.cpp
 * @brief Fingerprint computation and manifest construction from YAML.
 *
 * @details Public symbols here (configuration_fingerprint, the manifest
 * constructor and species_index) are documented at their declarations in
 * catchem_manifest.hpp; only the anonymous-namespace helpers are documented
 * in place. The fingerprint is a 64-bit FNV-1a digest rendered as 16 hex
 * characters, taken over the configuration file bytes plus the resolved
 * species file, or over the serialised species metadata when no file backs
 * the ConfigManager.
 */
#include "catchem_manifest.hpp"

#include <cmath>
#include <iomanip>
#include <fstream>
#include <sstream>
#include <stdexcept>

namespace catchem {
namespace {
    // Stable FNV-1a fingerprint over source path and content. This detects
    // ordinary edits but is not intended for adversarial collision resistance.

    /// Absorb the bytes of one string, plus a 0xff separator byte, into the FNV-1a state.
    void hash_bytes(std::uint64_t& hash, const std::string& value) {
        for (unsigned char c : value) {
            hash ^= c;
            hash *= 1099511628211ULL;
        }
        hash ^= 0xff;
        hash *= 1099511628211ULL;
    }

    /// Absorb every byte of one file into the FNV-1a state (binary mode).
    void hash_file(std::uint64_t& hash, const std::string& path) {
        std::ifstream input(path, std::ios::binary);
        if (!input) throw std::invalid_argument("Cannot fingerprint manifest dependency: " + path);
        char byte = 0;
        while (input.get(byte)) {
            hash ^= static_cast<unsigned char>(byte);
            hash *= 1099511628211ULL;
        }
    }

    /// True for the unit strings a manifest accepts: empty, kg kg-1, mol mol-1, ppmv, ug kg-1, kg m-3, mol m-3.
    bool supported_units(const std::string& units) {
        return units.empty() || units == "kg kg-1" || units == "mol mol-1" || units == "ppmv" ||
               units == "ug kg-1" || units == "kg m-3" || units == "mol m-3";
    }
} // namespace

std::string configuration_fingerprint(const ConfigManager& config) {
    std::uint64_t hash = 14695981039346656037ULL;
    hash_bytes(hash, config.config_file_path);
    if (!config.config_file_path.empty()) {
        hash_file(hash, config.config_file_path);
    } else {
        // Unit tests and embedded hosts may build ConfigManager directly.
        // Such configurations have no source file to hash, so fingerprint
        // the effective mechanism metadata instead of attempting to open an
        // empty path or assigning every in-memory mechanism the same ID.
        hash_bytes(hash, config.data.mechanism_identity);
        for (const auto& species : config.data.species) {
            hash_bytes(hash, species.name);
            hash_bytes(hash, species.ccpp_standard_name);
            hash_bytes(hash, species.units);
            hash_bytes(hash, species.amount_type);
            hash_bytes(hash, species.air_basis);
            hash_bytes(hash, species.long_name);
            hash_bytes(hash, species.description);
            hash_bytes(hash, species.mie_name);
            for (const auto& alias : species.aliases) hash_bytes(hash, alias);
            for (const auto& role : species.roles) hash_bytes(hash, role);
            std::ostringstream values;
            values << std::setprecision(17) << species.molecular_weight_kg_mol << ',' << species.mw_g << ','
                   << species.density << ',' << species.radius << ',' << species.lower_radius << ','
                   << species.upper_radius << ',' << species.viscosity << ',' << species.dd_f0 << ','
                   << species.dd_hstar << ',' << species.dd_DvzAerSnow << ',' << species.dd_DvzMinVal_snow << ','
                   << species.dd_DvzMinVal_land << ',' << species.henry_k0 << ',' << species.henry_cr << ','
                   << species.henry_pKa << ',' << species.wd_retfactor << ',' << species.wd_convfacI2G << ','
                   << species.wd_reevap_frac << ',' << species.t_chem_loss << ',' << species.BackgroundVV << ','
                   << species.is_gas << species.is_aerosol << species.is_tracer << species.is_advected
                   << species.is_drydep << species.is_wetdep << species.do_settling << species.is_photolysis
                   << species.is_gocart_aero << species.is_dust << species.is_seasalt << species.is_hydrophilic;
            hash_bytes(hash, values.str());
            for (const double efficiency : species.wd_rainouteff) {
                std::ostringstream value;
                value << std::setprecision(17) << efficiency;
                hash_bytes(hash, value.str());
            }
        }
    }
    if (!config.data.species_filename.empty()) {
        const std::string dependency = resolve_config_path(config.config_file_path, config.data.species_filename);
        if (!config.config_file_path.empty()) {
            hash_bytes(hash, dependency);
            hash_file(hash, dependency);
        }
    }
    std::ostringstream out;
    out << std::hex << std::setw(16) << std::setfill('0') << hash;
    return out.str();
}

MechanismManifest::MechanismManifest(const std::string& config_path) {
    if (config_path.empty()) throw std::invalid_argument("Manifest configuration path is empty");
    ConfigManager config;
    config.load_from_file(config_path);
    if (!config.data.species_filename.empty()) {
        const std::string species_path = resolve_config_path(config_path, config.data.species_filename);
        config.load_species_file(species_path);
    }
    config.validate_or_throw();

    identity_ = config.data.mechanism_identity.empty() ? "configured" : config.data.mechanism_identity;
    for (const auto& item : config.data.species) {
        const auto metadata = make_species_metadata(item);
        ManifestSpecies species;
        species.name = metadata.short_name;
        species.standard_name = metadata.ccpp_standard_name;
        species.units = metadata.units;
        species.aliases = metadata.aliases;
        species.roles = metadata.roles;
        species.host_representation = metadata.host_representation;
        species.process_representations = metadata.process_representations;
        species.gas = metadata.is_gas;
        species.aerosol = metadata.is_aerosol;
        species.advected = metadata.is_advected;
        species.molecular_weight_kg_mol = metadata.mw_g / 1000.0;
        if (species.name.empty() || species.standard_name.empty())
            throw std::invalid_argument("Manifest species name and CCPP standard name must be nonempty");
        if (!supported_units(species.units))
            throw std::invalid_argument("Unsupported units for species " + species.name + ": " + species.units);
        const auto add_name = [&](const std::string& name) {
            const auto key = canonical_species_name(name);
            if (key.empty() || !name_index_.emplace(key, species_.size()).second)
                throw std::invalid_argument("Species name or alias collision: " + name);
        };
        add_name(species.name);
        for (const auto& alias : species.aliases) add_name(alias);
        species_.push_back(std::move(species));
    }
    for (const auto& [name, process] : config.data.processes) {
        processes_.push_back({name, process.activate, process.scheme});
    }
    if (species_.empty()) throw std::invalid_argument("Manifest mechanism contains no species");
    fingerprint_ = configuration_fingerprint(config);
}

std::size_t MechanismManifest::species_index(const std::string& name_or_alias) const {
    const auto found = name_index_.find(canonical_species_name(name_or_alias));
    if (found == name_index_.end()) throw std::out_of_range("Unknown manifest species: " + name_or_alias);
    return found->second;
}

} // namespace catchem

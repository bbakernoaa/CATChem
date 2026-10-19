/**
 * @file catchem_species_metadata.cpp
 * @brief Implementation of make_species_metadata(), the SpeciesConfig to
 *        SpeciesMetadata normaliser.
 *
 * @details The only function here is declared in catchem_species_metadata.hpp
 * and inherits its documentation. The conversion is field-by-field copy with
 * three normalisation rules: aerosol-only flags are cleared for gases, the
 * legacy dry-air mass-ratio representation is applied when neither
 * amount_type nor air_basis is configured, and mw_g falls back to
 * molecular_weight_kg_mol scaled by 1000 when not given directly.
 */
#include "catchem_species_metadata.hpp"
#include "catchem_config_manager.hpp"

namespace catchem {
SpeciesMetadata make_species_metadata(const SpeciesConfig& sp) {
    SpeciesMetadata meta;
    meta.short_name = sp.name; meta.long_name = sp.long_name.empty() ? sp.name : sp.long_name;
    meta.description = sp.description; meta.aliases = sp.aliases; meta.roles = sp.roles;
    meta.ccpp_standard_name = sp.ccpp_standard_name.empty() ? sp.name : sp.ccpp_standard_name;
    meta.units = sp.units; meta.is_gas = sp.is_gas; meta.is_aerosol = sp.is_aerosol && !meta.is_gas;
    meta.host_representation.units = sp.units;
    if (sp.amount_type == "mole_fraction" || sp.amount_type == "ppmv") meta.host_representation.amount_type = AmountType::MoleFraction;
    else if (sp.amount_type == "mass_mixing_ratio") meta.host_representation.amount_type = AmountType::MassMixingRatio;
    else if (sp.amount_type == "number_concentration") meta.host_representation.amount_type = AmountType::NumberConcentration;
    else if (!sp.amount_type.empty()) throw std::invalid_argument("Unsupported species amount_type: " + sp.amount_type);
    if (sp.air_basis == "dry") meta.host_representation.air_basis = AirBasis::Dry;
    else if (sp.air_basis == "moist") meta.host_representation.air_basis = AirBasis::Moist;
    else if (sp.air_basis == "total_with_condensate") meta.host_representation.air_basis = AirBasis::TotalWithCondensate;
    else if (!sp.air_basis.empty()) throw std::invalid_argument("Unsupported species air_basis: " + sp.air_basis);
    // Preserve the historical CCPP registration contract for legacy entries
    // that predate explicit representations: kg/kg dry-air mass ratio.
    if (meta.host_representation.amount_type == AmountType::Unspecified &&
        meta.host_representation.air_basis == AirBasis::Unspecified) {
        meta.host_representation.amount_type = AmountType::MassMixingRatio;
        meta.host_representation.air_basis = AirBasis::Dry;
    }
    if (meta.host_representation.amount_type != AmountType::Unspecified && meta.host_representation.air_basis == AirBasis::Unspecified)
        throw std::invalid_argument("Explicit species amount_type requires an explicit air_basis: " + sp.name);
    meta.is_tracer = sp.is_tracer; meta.is_advected = sp.is_advected;
    meta.is_drydep = sp.is_drydep; meta.is_wetdep = sp.is_wetdep;
    meta.do_settling = sp.do_settling && meta.is_aerosol; meta.is_photolysis = sp.is_photolysis;
    meta.is_gocart_aero = sp.is_gocart_aero && !meta.is_gas;
    meta.is_dust = sp.is_dust && !meta.is_gas; meta.is_seasalt = sp.is_seasalt && !meta.is_gas;
    meta.is_hydrophilic = sp.is_hydrophilic;
    meta.mw_g = sp.mw_g > 0.0 ? sp.mw_g : sp.molecular_weight_kg_mol * 1000.0;
    meta.density = sp.density; meta.radius = sp.radius; meta.lower_radius = sp.lower_radius;
    meta.upper_radius = sp.upper_radius; meta.viscosity = sp.viscosity;
    meta.dd_f0 = sp.dd_f0; meta.dd_hstar = sp.dd_hstar; meta.dd_DvzAerSnow = sp.dd_DvzAerSnow;
    meta.dd_DvzMinVal_snow = sp.dd_DvzMinVal_snow; meta.dd_DvzMinVal_land = sp.dd_DvzMinVal_land;
    meta.henry_k0 = sp.henry_k0; meta.henry_cr = sp.henry_cr; meta.henry_pKa = sp.henry_pKa;
    meta.wd_retfactor = sp.wd_retfactor; meta.wd_LiqAndGas = sp.wd_LiqAndGas;
    meta.wd_convfacI2G = sp.wd_convfacI2G; meta.wd_rainouteff = sp.wd_rainouteff;
    meta.wd_reevap_frac = sp.wd_reevap_frac; meta.t_chem_loss = sp.t_chem_loss;
    meta.BackgroundVV = sp.BackgroundVV; meta.mie_name = sp.mie_name;
    return meta;
}
} // namespace catchem

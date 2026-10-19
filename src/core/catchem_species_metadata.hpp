/**
 * @file catchem_species_metadata.hpp
 * @brief Normalised species metadata and the indexed mechanism definition.
 *
 * @details Defines the runtime description of one chemical species
 * (SpeciesMetadata), the amount/air-basis representation vocabulary used at
 * the CCPP boundary (SpeciesRepresentation, AmountType, AirBasis), and
 * MechanismDefinition — an immutable-after-build index of a mechanism that
 * resolves names, aliases, and roles to 0-based species positions.
 * make_species_metadata() converts the raw YAML-facing SpeciesConfig into
 * normalised metadata, applying the legacy CCPP registration defaults when a
 * mechanism entry predates explicit representations. Everything here is
 * host-side value data: no Kokkos views, no device code, no ownership of
 * field buffers.
 *
 * @see catchem_config_manager.hpp (SpeciesConfig source),
 * catchem_chem_state.hpp (the loader that builds a MechanismDefinition).
 */
#pragma once
#include <algorithm>
#include <cctype>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <map>
#include <vector>

namespace catchem {

    /// Raw species entry parsed from a CATChem mechanism YAML file.
    struct SpeciesConfig;

    /// How a species amount is normalised in a field's units.
    enum class AmountType {
        Unspecified,        ///< No amount basis declared; legacy default applies.
        MoleFraction,       ///< Moles of species per mole of air (e.g. ppmv, mol mol-1).
        MassMixingRatio,    ///< kg of species per kg of air (kg kg-1).
        NumberConcentration ///< Species count per volume (e.g. molec cm-3, kg m-3 number).
    };

    /// Which air mass the denominator of a mixing ratio refers to.
    enum class AirBasis {
        Unspecified,           ///< No basis declared; legacy default applies.
        Dry,                   ///< Dry air only (condensate excluded).
        Moist,                 ///< Including water vapour.
        TotalWithCondensate    ///< Including cloud water and precipitation.
    };

    /**
     * @brief Unit/representation contract for one species in one consumer.
     *
     * @details Recorded per species for the host field (host_representation)
     * and per process (process_representations) so schemes can convert
     * without re-reading the mechanism file. Value data; strings are copies.
     */
    struct SpeciesRepresentation {
        std::string units;                            ///< Unit string, e.g. "kg kg-1".
        AmountType amount_type = AmountType::Unspecified; ///< Normalisation of the amount.
        AirBasis air_basis = AirBasis::Unspecified;   ///< Denominator air mass.
        std::string vertical_axis = "level";          ///< Vertical coordinate label the representation assumes.
        std::string denominator_recipe;               ///< Free-form recipe for reconstructing the denominator.
    };

    /**
     * @brief Normalised metadata for a single chemical species.
     *
     * @details Produced from SpeciesConfig by make_species_metadata(), which
     * resolves defaults (long name, CCPP standard name, legacy dry mass-ratio
     * representation) and enforces the aerosol-only flags. Plain value data:
     * copies are cheap enough for index-time use and own all their strings.
     */
    struct SpeciesMetadata {
        // Names
        std::string short_name;          ///< Canonical mechanism name (e.g. "SO4").
        std::string long_name;           ///< Descriptive name; defaults to short_name.
        std::string description;         ///< Free-text description from the mechanism file.
        std::vector<std::string> aliases;///< Alternate names accepted by lookups.
        std::vector<std::string> roles;  ///< Functional roles (e.g. "oxidant") for role-based lookup.
        std::string ccpp_standard_name;  ///< CF/CCPP standard name; defaults to short_name.
        std::string units;               ///< Species-native concentration units as configured.
        SpeciesRepresentation host_representation; ///< Representation of the host concentration field.
        std::map<std::string, SpeciesRepresentation> process_representations; ///< Per-process representations keyed by process name.

        // Classification switches
        bool is_gas = false;        ///< Gas-phase species.
        bool is_aerosol = false;    ///< Aerosol-phase species (forced false when is_gas).
        bool is_tracer = false;     ///< Passive tracer flag.
        bool is_advected = true;    ///< Participates in transport.
        bool is_drydep = false;     ///< Has a dry-deposition parameterisation.
        bool is_wetdep = false;     ///< Has a wet-deposition parameterisation.
        bool do_settling = false;   ///< Gravitational settling applies (aerosol-only).
        bool is_photolysis = false; ///< Participates in photolysis.
        bool is_gocart_aero = false;///< Handled by the GOCART aerosol microphysics (aerosol-only).
        bool is_dust = false;       ///< Mineral-dust mode species.
        bool is_seasalt = false;    ///< Sea-salt mode species.
        bool is_hydrophilic = true; ///< Takes up water at elevated RH (wet-size settling).

        // Physical / Numerical properties
        double mw_g = 0.0;          ///< Molecular weight [g mol-1].
        double density = 0.0;       ///< Particle bulk density [kg m-3] (aerosols).
        double radius = 0.0;        ///< Representative mode radius [m].
        double lower_radius = 0.0;  ///< Lower bin-edge radius [m] for sectional modes.
        double upper_radius = 0.0;  ///< Upper bin-edge radius [m] for sectional modes.
        double viscosity = 0.0;     ///< Dynamic viscosity parameter [Pa s] used by settling.

        // Dry deposition parameters
        double dd_f0 = 0.0;             ///< Emerson f0 resistance parameter [-].
        double dd_hstar = 0.0;          ///< Canopy compensation height h* [m].
        double dd_DvzAerSnow = 0.0;     ///< Deposition velocity over snow [m s-1].
        double dd_DvzMinVal_snow = 0.0; ///< Minimum deposition velocity over snow [m s-1].
        double dd_DvzMinVal_land = 0.0; ///< Minimum deposition velocity over land [m s-1].

        // Wet deposition parameters
        double henry_k0 = 0.0;                ///< Henry's law constant at 298 K [M atm-1].
        double henry_cr = 0.0;                ///< Temperature-dependence coefficient Cr [K].
        double henry_pKa = 0.0;               ///< Acid dissociation exponent pKa [-].
        double wd_retfactor = 0.0;            ///< Convective retention factor [-].
        bool wd_LiqAndGas = false;            ///< Species exists in both cloud phases (liquid and gas).
        double wd_convfacI2G = 0.0;           ///< Convective ice-to-gas conversion factor [-].
        std::vector<double> wd_rainouteff = {0.0, 0.0, 0.0}; ///< Rainout efficiency per precip category (lsc, conv, anvil) [-].
        double wd_reevap_frac = 0.5;          ///< Fraction of evaporated precipitation re-applied [-].

        // Chemical loss rate and background volume-mixing ratio
        double t_chem_loss = -1.0;        ///< First-order chemical loss rate [s-1]; negative disables the loss.
        double BackgroundVV = 1.0e-20;    ///< Background volume mixing ratio floor [mol mol-1].
        std::string mie_name;             ///< Aerosol optics type code (e.g. "DU") selecting a Mie table.
    };

    /**
     * @brief Convert a parsed configuration entry into normalised metadata.
     *
     * @param config SpeciesConfig as read from the mechanism YAML file; only
     *               borrowed for the call.
     * @return Fully populated SpeciesMetadata with defaults resolved.
     *
     * @note Enforces phase consistency: is_aerosol, is_gocart_aero, is_dust,
     * is_seasalt, and do_settling are cleared for gas species. An unknown
     * non-empty amount_type or air_basis, or an amount_type without an
     * explicit air_basis, throws std::invalid_argument. Legacy entries with
     * neither attribute default to dry-air mass mixing ratio, matching the
     * historical CCPP registration contract.
     */
    SpeciesMetadata make_species_metadata(const SpeciesConfig& config);

    /**
     * @brief Uppercase a species name for case-insensitive indexing.
     * @param name Species name, alias, or role; copied, not modified in place.
     * @return The uppercase canonical form used as every lookup key.
     */
    inline std::string canonical_species_name(std::string name) {
        std::transform(name.begin(), name.end(), name.begin(),
                       [](unsigned char value) { return static_cast<char>(std::toupper(value)); });
        return name;
    }

    /**
     * @brief Indexed definition of a complete chemical mechanism.
     *
     * @details Built once (by ChemState::load_from_config_manager() or at
     * configuration time) and then shared as const via shared_ptr; rebuild_index()
     * must run after populating species and before any lookup. Name, alias,
     * and role keys are canonicalised (uppercased) and must be unique and
     * non-empty — collisions throw std::invalid_argument. Not thread-safe for
     * mutation while readers are active; treat it as immutable after the
     * index build.
     */
    class MechanismDefinition {
    public:
        std::string identity;                  ///< Mechanism name from configuration (e.g. "finn01").
        std::string source;                    ///< Configuration file the mechanism was parsed from.
        std::string fingerprint;               ///< Stable hash of the configuration inputs.
        std::size_t generation = 1;            ///< Reserved mechanism-generation counter; set at construction.
        std::vector<SpeciesMetadata> species;  ///< Species in concentration-array index order.
        std::unordered_set<std::string> capabilities; ///< Uppercase capability tags declared for the mechanism.

        /**
         * @brief (Re)build the name/alias and role lookup indexes.
         *
         * @details Clears both maps and re-inserts every species under its
         * canonical short name and each canonical alias (name map) and role
         * (role map), mapping to its position in species.
         *
         * @note Throws std::invalid_argument when species is empty, a name,
         * alias, or role is empty after canonicalisation, or any key
         * duplicates an earlier species' key. Must be called before
         * index_of()/contains()/has_role().
         */
        void rebuild_index() {
            name_to_index_.clear();
            role_to_index_.clear();
            if (species.empty())
                throw std::invalid_argument("Chemical mechanism must contain at least one species");
            for (std::size_t index = 0; index < species.size(); ++index) {
                const auto canonical = canonical_species_name(species[index].short_name);
                if (canonical.empty())
                    throw std::invalid_argument("Chemical mechanism contains an empty species name");
                if (!name_to_index_.emplace(canonical, index).second)
                    throw std::invalid_argument("Chemical mechanism contains duplicate species name: " +
                                                species[index].short_name);
                for (const auto& alias : species[index].aliases) {
                    const auto canonical_alias = canonical_species_name(alias);
                    if (canonical_alias.empty() || !name_to_index_.emplace(canonical_alias, index).second)
                        throw std::invalid_argument("Chemical mechanism contains duplicate or empty species alias: " +
                                                    alias);
                }
                for (const auto& role : species[index].roles) {
                    const auto canonical_role = canonical_species_name(role);
                    if (canonical_role.empty() || !role_to_index_.emplace(canonical_role, index).second)
                        throw std::invalid_argument("Chemical mechanism contains duplicate or empty species role: " +
                                                    role);
                }
            }
        }

        /**
         * @brief Resolve a species name or alias to its concentration index.
         * @param name Species short name or alias; matched case-insensitively.
         * @return 0-based position of the species in the vector.
         * @throws std::out_of_range when the name is not part of the mechanism.
         */
        std::size_t index_of(const std::string& name) const {
            const auto found = name_to_index_.find(canonical_species_name(name));
            if (found == name_to_index_.end())
                throw std::out_of_range("Species is not present in the active mechanism: " + name);
            return found->second;
        }

        /**
         * @brief Test whether a name or alias belongs to the mechanism.
         * @param name Species short name or alias; matched case-insensitively.
         * @return true when index_of() would succeed for this name.
         */
        bool contains(const std::string& name) const {
            return name_to_index_.find(canonical_species_name(name)) != name_to_index_.end();
        }

        /**
         * @brief Test whether any species carries a functional role.
         * @param role Role label (e.g. "oxidant"); matched case-insensitively.
         * @return true when index_for_role() would succeed for this role.
         */
        bool has_role(const std::string& role) const {
            return role_to_index_.find(canonical_species_name(role)) != role_to_index_.end();
        }

        /**
         * @brief Resolve a species role to that species' concentration index.
         * @param role Role label; matched case-insensitively.
         * @return 0-based position of the species declaring the role.
         * @throws std::out_of_range when no species carries the role.
         */
        std::size_t index_for_role(const std::string& role) const {
            const auto found = role_to_index_.find(canonical_species_name(role));
            if (found == role_to_index_.end())
                throw std::out_of_range("Required species role is not present in the active mechanism: " + role);
            return found->second;
        }

        /**
         * @brief Test whether the mechanism declares a capability tag.
         * @param capability Capability name from the mechanism block; matched
         *                   case-insensitively.
         * @return true when the mechanism advertises the capability.
         */
        bool has_capability(const std::string& capability) const {
            return capabilities.find(canonical_species_name(capability)) != capabilities.end();
        }

    private:
        std::unordered_map<std::string, std::size_t> name_to_index_; ///< Canonical names/aliases -> species position.
        std::unordered_map<std::string, std::size_t> role_to_index_; ///< Canonical roles -> species position.
    };

} // namespace catchem

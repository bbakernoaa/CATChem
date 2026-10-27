/**
 * @file catchem_met_state.hpp
 * @brief Container for bound meteorological fields with alias lookup.
 *
 * @details MetState stores every host-supplied meteorology field twice: once
 * in the dynamic fields_2d/fields_3d maps (under the requested name plus its
 * uppercase and lowercase aliases) and once in a named convenience member
 * (T, QV, PMID, ...) when the name matches a standard core field. Both refer
 * to the same InteropField shared_ptr, so rebinding through bind_*_field()
 * keeps the maps and members consistent. Ownership of the underlying buffers
 * stays with the host model; MetState only holds views. StateManager drives
 * generation stamping and invalidation, and is the intended single writer.
 */
#pragma once
#include "catchem_interop_field.hpp"
#include <algorithm>
#include <cctype>
#include <initializer_list>
#include <memory>
#include <string>
#include <unordered_map>

namespace catchem {

    /**
     * @brief Bound meteorological fields, keyed lookup, and standard accessors.
     *
     * @details Plain aggregate: all members are public and the maps and
     * convenience pointers are intentionally kept in sync by the bind
     * helpers. Copying the struct shares every view (no deep copy of data).
     */
    struct MetState {
        /// Dynamic map storage for 2D and 3D meteorological fields
        std::unordered_map<std::string, std::shared_ptr<InteropField<double, 3>>>
            fields_3d; ///< 3D fields keyed by name and case aliases.
        std::unordered_map<std::string, std::shared_ptr<InteropField<double, 2>>>
            fields_2d; ///< 2D fields keyed by name and case aliases.

        /// Convenient direct accessors for standard core fields (bound views, null until supplied)
        std::shared_ptr<InteropField<double, 3>> T;          //!< Temperature [K]
        std::shared_ptr<InteropField<double, 3>> QV;         //!< Specific humidity [kg/kg]
        std::shared_ptr<InteropField<double, 3>> RH;         //!< Relative humidity [0-1]
        std::shared_ptr<InteropField<double, 3>> PMID;       //!< Mid-level pressure [Pa]
        std::shared_ptr<InteropField<double, 3>> PEDGE;      //!< Edge-level pressure [Pa]
        std::shared_ptr<InteropField<double, 3>> AIRDEN;     //!< Wet air density [kg/m³]
        std::shared_ptr<InteropField<double, 3>> AIRDEN_DRY; //!< Dry air density [kg/m³]
        std::shared_ptr<InteropField<double, 3>> BXHEIGHT;   //!< Layer thickness height [m]
        /// Cloud liquid water content [kg/kg]; the cloud-gated mkppchem
        /// aqueous pathway reads this to enable the SO2 -> SO4 flux.
        std::shared_ptr<InteropField<double, 3>> CLDLIQ;

        std::shared_ptr<InteropField<double, 2>> PS;    //!< Surface pressure [Pa]
        std::shared_ptr<InteropField<double, 2>> TS;    //!< Surface temperature [K]
        std::shared_ptr<InteropField<double, 2>> PBLH;  //!< Boundary layer height [m]
        std::shared_ptr<InteropField<double, 2>> USTAR; //!< Friction velocity [m/s]
        std::shared_ptr<InteropField<double, 2>> HFLUX; //!< Sensible heat flux [W/m²]
        std::shared_ptr<InteropField<double, 2>> OBK;   //!< Monin-Obukhov length [m]
        std::shared_ptr<InteropField<double, 2>> LAT;   //!< Latitude [deg]
        std::shared_ptr<InteropField<double, 2>> LON;   //!< Longitude [deg]

        /**
         * @brief Look up a 3D field by any of several candidate names.
         * @param candidates Names tried in order; null entries are skipped.
         * @return The first bound shared_ptr, or nullptr when none match.
         *
         * @note Keys are matched exactly (case-sensitive); callers pass
         * canonical uppercase names, and bind_3d_field() also stores the
         * upper/lowercase aliases of the bound name.
         */
        std::shared_ptr<InteropField<double, 3>> get_3d(std::initializer_list<const char*> candidates) const {
            for (const char* name : candidates) {
                if (!name)
                    continue;
                auto it = fields_3d.find(name);
                if (it != fields_3d.end() && it->second) {
                    return it->second;
                }
            }
            return nullptr;
        }

        /// @copydoc get_3d
        std::shared_ptr<InteropField<double, 2>> get_2d(std::initializer_list<const char*> candidates) const {
            for (const char* name : candidates) {
                if (!name)
                    continue;
                auto it = fields_2d.find(name);
                if (it != fields_2d.end() && it->second) {
                    return it->second;
                }
            }
            return nullptr;
        }

        /**
         * @brief Test whether any candidate 3D field is bound.
         * @param candidates Names tried in order by get_3d().
         * @return true when at least one candidate resolves to a bound field.
         */
        bool has_3d(std::initializer_list<const char*> candidates) const { return get_3d(candidates) != nullptr; }

        /**
         * @brief Test whether any candidate 2D field is bound.
         * @param candidates Names tried in order by get_2d().
         * @return true when at least one candidate resolves to a bound field.
         */
        bool has_2d(std::initializer_list<const char*> candidates) const { return get_2d(candidates) != nullptr; }

        /**
         * @brief Register a 2D field under a name and refresh its aliases.
         *
         * @param name Field key; also stored uppercased and lowercased so
         *             case-variant lookups resolve to the same view.
         * @param field View over host-owned data; shared, not copied. Null
         *              fields and empty names are ignored silently.
         *
         * @note When the uppercased name matches a standard field (PS, TS or
         * SST/SKIN_TEMPERATURE, PBLH or HPBL, USTAR or FRICTION_VELOCITY,
         * HFLUX, OBK or OL, LAT, LON) the matching convenience member is
         * repointed at the same shared_ptr.
         */
        void bind_2d_field(const std::string& name, std::shared_ptr<InteropField<double, 2>> field) {
            if (!field || name.empty())
                return;
            fields_2d[name] = field;

            // Generate upper and lower case aliases for key lookup
            std::string upper_name = name;
            std::transform(upper_name.begin(), upper_name.end(), upper_name.begin(),
                           [](unsigned char c) { return std::toupper(c); });
            fields_2d[upper_name] = field;

            std::string lower_name = name;
            std::transform(lower_name.begin(), lower_name.end(), lower_name.begin(),
                           [](unsigned char c) { return std::tolower(c); });
            fields_2d[lower_name] = field;

            // Sync standard convenience pointers dynamically
            if (upper_name == "PS" || upper_name == "SURFACE_PRESSURE")
                PS = field;
            else if (upper_name == "TS" || upper_name == "SST" || upper_name == "SKIN_TEMPERATURE")
                TS = field;
            else if (upper_name == "PBLH" || upper_name == "HPBL")
                PBLH = field;
            else if (upper_name == "USTAR" || upper_name == "FRICTION_VELOCITY")
                USTAR = field;
            else if (upper_name == "HFLUX")
                HFLUX = field;
            else if (upper_name == "OBK" || upper_name == "OL")
                OBK = field;
            else if (upper_name == "LAT" || upper_name == "LATITUDE")
                LAT = field;
            else if (upper_name == "LON" || upper_name == "LONGITUDE")
                LON = field;
        }

        /**
         * @brief Register a 3D field under a name and refresh its aliases.
         *
         * @param name Field key; also stored uppercased and lowercased so
         *             case-variant lookups resolve to the same view.
         * @param field View over host-owned data; shared, not copied. Null
         *               fields and empty names are ignored silently.
         *
         * @note When the uppercased name matches a standard field (T, QV, RH,
         * PMID, PEDGE, AIRDEN, AIRDEN_DRY, BXHEIGHT) the matching convenience
         * member is repointed at the same shared_ptr.
         */
        void bind_3d_field(const std::string& name, std::shared_ptr<InteropField<double, 3>> field) {
            if (!field || name.empty())
                return;
            fields_3d[name] = field;

            // Generate upper and lower case aliases for key lookup
            std::string upper_name = name;
            std::transform(upper_name.begin(), upper_name.end(), upper_name.begin(),
                           [](unsigned char c) { return std::toupper(c); });
            fields_3d[upper_name] = field;

            std::string lower_name = name;
            std::transform(lower_name.begin(), lower_name.end(), lower_name.begin(),
                           [](unsigned char c) { return std::tolower(c); });
            fields_3d[lower_name] = field;

            // Sync standard convenience pointers dynamically
            if (upper_name == "T" || upper_name == "TEMPERATURE" || upper_name == "TEMP")
                T = field;
            else if (upper_name == "QV" || upper_name == "SPHUM")
                QV = field;
            else if (upper_name == "RH" || upper_name == "RELATIVE_HUMIDITY")
                RH = field;
            else if (upper_name == "PMID" || upper_name == "PRESSURE_MID")
                PMID = field;
            else if (upper_name == "PEDGE" || upper_name == "PRESSURE_EDGE")
                PEDGE = field;
            else if (upper_name == "AIRDEN" || upper_name == "AIR_DENSITY") {
                AIRDEN = field;
            } else if (upper_name == "AIRDEN_DRY" || upper_name == "AIR_DENSITY_DRY") {
                AIRDEN_DRY = field;
            } else if (upper_name == "BXHEIGHT" || upper_name == "DZ" || upper_name == "DELZ")
                BXHEIGHT = field;
            else if (upper_name == "CLDLIQ" || upper_name == "CLOUD_LIQUID_WATER" || upper_name == "QLI")
                CLDLIQ = field;
        }

        /**
         * @brief Re-register every bound convenience member into the maps.
         *
         * @details Lets a host assign the named members directly (e.g. after
         * constructing fields itself) and still get alias lookups: each
         * non-null member is passed through bind_3d_field()/bind_2d_field()
         * under its canonical key. Members that are null are skipped, and
         * map entries bound under other names are untouched.
         */
        void register_fields() {
            if (T)
                bind_3d_field("T", T);
            if (QV)
                bind_3d_field("QV", QV);
            if (RH)
                bind_3d_field("RH", RH);
            if (PMID)
                bind_3d_field("PMID", PMID);
            if (PEDGE)
                bind_3d_field("PEDGE", PEDGE);
            if (AIRDEN)
                bind_3d_field("AIRDEN", AIRDEN);
            if (AIRDEN_DRY)
                bind_3d_field("AIRDEN_DRY", AIRDEN_DRY);
            if (BXHEIGHT)
                bind_3d_field("BXHEIGHT", BXHEIGHT);

            if (PS)
                bind_2d_field("PS", PS);
            if (TS)
                bind_2d_field("TS", TS);
            if (PBLH)
                bind_2d_field("PBLH", PBLH);
            if (USTAR)
                bind_2d_field("USTAR", USTAR);
            if (HFLUX)
                bind_2d_field("HFLUX", HFLUX);
            if (OBK)
                bind_2d_field("OBK", OBK);
            if (LAT)
                bind_2d_field("LAT", LAT);
            if (LON)
                bind_2d_field("LON", LON);
        }
    };

} // namespace catchem

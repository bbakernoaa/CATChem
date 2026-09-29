// src/process/mkppchem/catchem_mkpp_cloud_water.hpp
//
// Cloud liquid water source resolution for the MKPP mechanism's cloud-gated
// heterogeneous sulfur oxidation.
//
// Hybrid origin, per the resolved clarification:
//   - HostField: the host model supplied a cloud liquid water field; use it.
//   - Derived:   the host did not supply one; derive a water *content* from
//                fields CATChem already carries (CLDF, QV/RH, T, PMID), using
//                the documented physical relationship below.
//   - Unresolved: neither is available -> fatal at the point of resolution.
//
// Derivation (documented physical basis):
//   Cloud liquid water content is the condensate mass per unit volume of the
//   grid cell. CATChem carries a per-level cloud *fraction* CLDF (0-1, in the
//   `fields_3d` map) and specific humidity QV, but no in-cloud condensate
//   mixing ratio, so the fallback treats the grid-mean water above saturation
//   as cloud condensate wherever cloud is present:
//
//     q_sat(T, p)  = eps * e_sat(T) / (p - (1 - eps) * e_sat(T))   [kg/kg]
//     e_sat(T)     = 611.2 * exp(17.67 (T-273.15) / (T-29.65))     [Pa]  (Bolton/Magnus, liquid)
//     CLDLIQ       = CLDF * max(QV - q_sat, 0) * rho_dry           [kg/m3]
//
//   with rho_dry = PMID * g0 / (R_d T) the dry-air density from the ideal-gas
//   hydrostatic column, and eps = H2O_MW / AIR_MW. This is a grid-mean
//   condensate estimate: it is dimensionally a water content (kg/m3), the same
//   kind of quantity the mechanism's `> 1.0e-6` activation threshold consumes,
//   and it is zero where CLDF == 0 or QV <= q_sat, so it never
//   produces a non-finite value.
//
// Known limitations (must be stated in the process docs):
//   - It uses grid-mean supersaturation, not true in-cloud condensate, so it
//     under-estimates CLDLIQ at low cloud fraction (the classic grid-mean vs
//     in-cloud partitioning problem).
//   - The Magnus coefficients are for liquid water over -40..+50 C; below
//     freezing the partitioning between cloud ice and cloud water is ignored.
//   - When the host supplies CLDLIQ, this derivation is not used at all.
#pragma once

#include <functional>
#include <memory>
#include <string>

namespace catchem {
    template <typename T, int Rank> class InteropField;
}

namespace catchem::mkpp {

    /// Selected origin for the cloud liquid water quantity.
    enum class CloudWaterMode { Auto, HostField, Derived };

    /// Resolved origin after init (Unresolved is always a fatal error, never a
    /// value a run proceeds with).
    enum class CloudWaterOrigin { HostField, Derived, Unresolved };

    /// Configuration for cloud water resolution, read from the run config.
    struct CloudWaterConfig {
        CloudWaterMode mode = CloudWaterMode::Auto;
        std::string host_field; ///< named met field when mode == HostField
    };

    /// Result of resolving the cloud water source at init().
    struct CloudWaterSource {
        CloudWaterOrigin origin = CloudWaterOrigin::Unresolved;
        std::shared_ptr<InteropField<double, 3>> field; ///< the bound host field, when origin == HostField
        std::string description;                        ///< human-readable origin for the init log

        bool is_host() const { return origin == CloudWaterOrigin::HostField; }
    };

    /// Parse a mode string from configuration ("auto" | "host_field" | "derived").
    /// Unknown values are fatal naming the value.
    CloudWaterMode parse_cloud_water_mode(const std::string& value);

    /// Resolve the cloud water source.
    ///
    /// Precedence: under Auto a present host field wins over
    /// derivation; an explicit Derived request uses the derivation even when a
    /// host field exists, and the log records that choice. Every failure path
    /// is fatal naming the missing field or input — never zero, never
    /// a stale value, never an undocumented default.
    ///
    /// @param config        Requested mode and host field name.
    /// @param get_host_field Returns the named 3-D met field, or nullptr.
    /// @param met_present   Returns true when a named 3-D met field exists at all
    ///                      (used to verify every derivation input is bound).
    CloudWaterSource resolve_cloud_water(const CloudWaterConfig& config,
                                         const std::function<std::shared_ptr<InteropField<double, 3>>(const std::string&)>& get_host_field,
                                         const std::function<bool(const std::string&)>& met_present);

    /// Evaluate the documented derivation at one point.
    ///
    /// @param cldf Cloud fraction (0-1).
    /// @param qv   Specific humidity (kg/kg).
    /// @param t    Temperature (K).
    /// @param pmid Mid-level pressure (Pa).
    /// @return Cloud liquid water content (kg/m3), finite and >= 0.
    /// @throws std::runtime_error (`FATAL ERROR:` prefix) on non-physical input
    ///         (T <= 0, PMID <= 0, negative QV, non-finite any).
    double derive_cloud_liquid_water(double cldf, double qv, double t, double pmid);

} // namespace catchem::mkpp

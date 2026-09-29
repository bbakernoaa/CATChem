// src/process/mkppchem/catchem_mkpp_cloud_water.cpp
#include "catchem_mkpp_cloud_water.hpp"

#include "catchem_constants.hpp"
#include "catchem_interop_field.hpp"

#include <cmath>
#include <stdexcept>
#include <string>
#include <vector>

namespace catchem::mkpp {

    namespace {

        [[noreturn]] void fatal(const std::string& reason) {
            throw std::runtime_error("FATAL ERROR: mkppchem cloud water: " + reason);
        }

        // Fields the documented derivation consumes (see header for the basis).
        const std::vector<std::string>& derivation_inputs() {
            static const std::vector<std::string> inputs = {"CLDF", "QV", "T", "PMID"};
            return inputs;
        }

        // Default host cloud-liquid-water field names, probed in order.
        const std::vector<std::string>& host_candidates_default() {
            static const std::vector<std::string> candidates = {"CLDLIQ", "CLOUD_LIQUID_WATER", "QLI"};
            return candidates;
        }

    } // namespace

    CloudWaterMode parse_cloud_water_mode(const std::string& value) {
        if (value == "auto" || value.empty())
            return CloudWaterMode::Auto;
        if (value == "host_field")
            return CloudWaterMode::HostField;
        if (value == "derived")
            return CloudWaterMode::Derived;
        fatal("unknown cloud_liquid_water/mode '" + value + "' (expected auto | host_field | derived)");
    }

    CloudWaterSource resolve_cloud_water(const CloudWaterConfig& config,
                                         const std::function<std::shared_ptr<InteropField<double, 3>>(const std::string&)>& get_host_field,
                                         const std::function<bool(const std::string&)>& met_present) {
        auto probe_host = [&](const std::string& explicit_name) -> std::shared_ptr<InteropField<double, 3>> {
            std::vector<std::string> candidates;
            if (!explicit_name.empty())
                candidates.push_back(explicit_name);
            else
                candidates = host_candidates_default();
            for (const auto& name : candidates) {
                if (auto field = get_host_field(name))
                    return field;
            }
            return nullptr;
        };

        auto require_derivation_inputs = [&]() {
            for (const auto& name : derivation_inputs()) {
                if (!met_present(name))
                    fatal("derived cloud water requires met field '" + name +
                          "' which is not bound; refusing to gate the reaction on zero");
            }
        };

        CloudWaterSource source;

        switch (config.mode) {
            case CloudWaterMode::HostField: {
                auto field = probe_host(config.host_field);
                if (!field)
                    fatal("mode 'host_field' requested but the named cloud liquid water field '" +
                          (config.host_field.empty() ? std::string("<default candidates>") : config.host_field) +
                          "' is not bound");
                source.origin = CloudWaterOrigin::HostField;
                source.field = field;
                source.description = "host-supplied field";
                break;
            }
            case CloudWaterMode::Derived: {
                require_derivation_inputs();
                source.origin = CloudWaterOrigin::Derived;
                source.description = "derived from CLDF/QV/T/PMID (grid-mean condensate)";
                break;
            }
            case CloudWaterMode::Auto:
            default: {
                if (auto field = probe_host(config.host_field)) {
                    source.origin = CloudWaterOrigin::HostField;
                    source.field = field;
                    source.description = "host-supplied field (auto-selected)";
                } else {
                    require_derivation_inputs();
                    source.origin = CloudWaterOrigin::Derived;
                    source.description = "derived from CLDF/QV/T/PMID (auto-selected; no host field present)";
                }
                break;
            }
        }

        if (source.origin == CloudWaterOrigin::Unresolved)
            fatal("cloud water source could not be resolved");
        return source;
    }

    double derive_cloud_liquid_water(double cldf, double qv, double t, double pmid) {
        // Physical admissibility: fail loud on non-physical input
        // rather than propagate a non-finite or nonsense condensate.
        if (!std::isfinite(cldf) || cldf < 0.0 || cldf > 1.0)
            fatal("cloud fraction CLDF=" + std::to_string(cldf) + " is not a finite value in [0,1]");
        if (!std::isfinite(qv) || qv < 0.0)
            fatal("specific humidity QV=" + std::to_string(qv) + " is not finite and non-negative");
        if (!std::isfinite(t) || t <= 0.0)
            fatal("temperature T=" + std::to_string(t) + " K is not positive and finite");
        if (!std::isfinite(pmid) || pmid <= 0.0)
            fatal("mid-level pressure PMID=" + std::to_string(pmid) + " Pa is not positive and finite");

        constexpr double eps = constants::H2O_MW / constants::AIR_MW; // ~0.622
        // Bolton/Magnus saturation vapor pressure over liquid water [Pa].
        const double e_sat = 611.2 * std::exp(17.67 * (t - 273.15) / (t - 29.65));
        const double denom = pmid - (1.0 - eps) * e_sat;
        if (denom <= 0.0)
            fatal("saturation mixing ratio denominator is non-positive (PMID=" + std::to_string(pmid) +
                  " Pa, e_sat=" + std::to_string(e_sat) + " Pa): pressure below saturation vapor pressure");
        const double q_sat = eps * e_sat / denom;

        // Dry-air density from the ideal-gas hydrostatic column [kg/m3].
        const double rho_dry = pmid * constants::G0 / (constants::RD * t);

        // Grid-mean condensate: cloud fraction times water above saturation,
        // per unit volume. Zero where CLDF == 0 or QV <= q_sat (guard:
        // the max() keeps the excess non-negative, and no division by CLDF).
        const double excess = qv > q_sat ? (qv - q_sat) : 0.0;
        const double cldliq = cldf * excess * rho_dry;

        if (!std::isfinite(cldliq) || cldliq < 0.0)
            fatal("derived cloud liquid water is non-finite or negative: " + std::to_string(cldliq));
        return cldliq;
    }

} // namespace catchem::mkpp

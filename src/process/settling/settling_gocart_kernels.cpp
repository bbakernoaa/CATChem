// settling_gocart_kernels.cpp
//
// Faithful transcription of the GOCART2G gravitational-settling science into
// native C++. Every constant, arithmetic ordering, and control branch mirrors
// the upstream Fortran kernels so the port reproduces them to floating-point
// roundoff. Where the Fortran carries a quirk that is not a bug in practice
// (the pi literal typo below, the no-op sub-step edge in the UFS solver), the
// quirk is preserved deliberately rather than "fixed", because a parity test
// compares against the Fortran numbers.
#include "settling_gocart_kernels.hpp"

#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

namespace catchem {
    namespace settling {
        namespace {

            // --- Constants copied verbatim from GOCART2G_Process.F90 ---
            // Boltzmann constant [kg m^2 s^-1 K^-1 mol^-1] as used upstream.
            constexpr double kBoltz = 1.3807e-23;
            // Mass of the mean air molecule [kg].
            constexpr double kAirMoleculeMass = 4.8096e-26;
            // Pi literal as written in the Fortran kernel (note the digit
            // transposition, 3.141529265 not 3.141592653). Reproduced exactly so
            // the thermal-velocity factor matches the upstream fall speeds.
            constexpr double kPiFortran = 3.141529265;
            constexpr double kTwoOverNine = 2.0 / 9.0;

            // Corrected Sutherland reference viscosity and temperature anchors.
            constexpr double kSutherlandMu0 = 1.8325e-5;
            constexpr double kSutherlandT0 = 416.16;
            constexpr double kSutherlandTRef = 296.16;
            constexpr double kSutherlandOffset = 120.0;

            // Pruppacher & Klett regime-2 drag-correction polynomial (Horner
            // coefficients a0..a6 from the Fortran kernel).
            constexpr double kDragA0 = -3.18657;
            constexpr double kDragA1 = 0.992696;
            constexpr double kDragA2 = -1.53193e-3;
            constexpr double kDragA3 = -9.870593e-4;
            constexpr double kDragA4 = -5.78878e-4;
            constexpr double kDragA5 = 8.55176e-5;
            constexpr double kDragA6 = -3.27815e-6;
            // Reynolds threshold above which the drag correction is applied.
            constexpr double kReynoldsThreshold = 0.01;

            // Maring et al. (2003) upward-velocity offset [m s^-1].
            constexpr double kMaringUpward = 0.33e-2;
            constexpr double kMaringFloor = 1.0e-9;

            // Density of liquid water [kg m^-3] used by both swelling schemes.
            constexpr double kWaterDensity = 1000.0;
            // Fitzgerald (1975) sea-salt parameters.
            constexpr double kFitzgeraldAlphaNaCl = 1.35;
            constexpr double kFitzgeraldRhThreshold = 0.80;
            constexpr double kFitzgeraldRhMax = 0.995;
            // Gerber (1985) parameters (radius handled in cm internally).
            constexpr double kGerberC1 = 0.7674;
            constexpr double kGerberC2 = 3.079;
            constexpr double kGerberC3 = 2.573e-11;
            constexpr double kGerberC4 = -1.424;

            // Numerical guards copied from the UFS solver.
            constexpr double kSolverEps = 1.0e-30;
            constexpr double kSolverCfl = 0.1;

            // Thermal-velocity prefactor f_vt = 8 kb / pi / m_air.
            constexpr double kThermalVelocityFactor = 8.0 * kBoltz / kPiFortran / kAirMoleculeMass;

        } // namespace

        double dynamic_viscosity(double temperature_k) {
            // rmu = 1.8325e-5 * (416.16/(T+120)) * (T/296.16)^1.5
            return kSutherlandMu0 * (kSutherlandT0 / (temperature_k + kSutherlandOffset))
                 * std::pow(temperature_k / kSutherlandTRef, 1.5);
        }

        double calc_vsettle(double radius, double rhop, double rhoa, double tmpu, double grav) {
            const double rmu = dynamic_viscosity(tmpu);

            // Thermal velocity of an air molecule.
            const double vt = std::sqrt(tmpu * kThermalVelocityFactor);
            // Air molecule mean free path.
            const double rmfp = 2.0 * rmu / (rhoa * vt);
            // Knudsen number.
            const double rkn = rmfp / radius;
            // Cunningham slip correction (linearized Binkowski & Shankar form).
            const double bpm = 1.0 + 1.246 * rkn;

            // Stokes fall speed with slip, evaluated in the Fortran left-to-right
            // order to preserve roundoff parity.
            double vsettle = ((((kTwoOverNine * rhop) * radius) * radius) * grav * bpm) / rmu;

            // First Reynolds-number estimate from the Stokes value.
            double re = 2.0 * rhoa * radius * vsettle / rmu;

            // Drag correction (Pruppacher & Klett regime 2) once Re exceeds 0.01.
            if (re > kReynoldsThreshold) {
                const double x = std::log(24.0 * re / bpm);
                const double y = kDragA0 + x * (kDragA1 + x * (kDragA2 + x * (kDragA3 + x * (kDragA4 + x * (kDragA5 + kDragA6 * x)))));
                re = std::exp(y) * bpm;
                vsettle = 0.5 * rmu * re / (rhoa * radius);
            }
            return vsettle;
        }

        void calc_vsettle_profile(const double* radius_wet, const double* rhop_wet, const double* rhoa,
                                  const double* tmpu, int n, double grav, double* vsettle_out) {
            for (int k = 0; k < n; ++k) {
                vsettle_out[k] = calc_vsettle(radius_wet[k], rhop_wet[k], rhoa[k], tmpu[k], grav);
            }
        }

        void apply_maring_correction(double* vsettle, int n) {
            for (int k = 0; k < n; ++k) {
                vsettle[k] = std::max(kMaringFloor, vsettle[k] - kMaringUpward);
            }
        }

        void swelling_fitzgerald(double radius_dry, double rhop_dry, double rh, double* radius_out,
                                 double* rhop_out) {
            // Default to the dry particle; only the super-threshold branch swells.
            double radius = radius_dry;
            double rhop = rhop_dry;
            double sat = rh;
            if (sat > kFitzgeraldRhThreshold) {
                // Blows up near saturation, so cap at 0.995.
                sat = std::min(kFitzgeraldRhMax, sat);
                const double beta = std::exp((0.00077 * sat) / (1.009 - sat));
                const double theta =
                    (sat <= 0.97) ? 1.058 : 1.058 - (0.0155 * (sat - 0.97)) / (1.02 - std::pow(sat, 1.4));
                const double alpha1 = 1.2 * std::exp((0.066 * sat) / (theta - sat));
                const double alpha = kFitzgeraldAlphaNaCl * alpha1;
                radius = alpha * std::pow(radius_dry, beta);
                double rrat = radius_dry / radius;
                rrat = rrat * rrat * rrat;
                rhop = rrat * rhop_dry + (1.0 - rrat) * kWaterDensity;
            }
            *radius_out = radius;
            *rhop_out = rhop;
        }

        void swelling_gerber(double radius_dry, double rhop_dry, double rh, double* radius_out, double* rhop_out) {
            // Floor RH at the smallest positive real to avoid a log10(0) FPE,
            // matching tiny(1.0) under the real-8 build.
            const double sat = std::min(kFitzgeraldRhMax, std::max(rh, std::numeric_limits<double>::min()));
            const double rcm = radius_dry * 100.0; // radius in cm for the Gerber fit
            // The Fortran raises the bracket to **(1./3.), i.e. a power, not a
            // cube root; keep pow so the port matches to the last bit.
            const double bracket =
                kGerberC1 * std::pow(rcm, kGerberC2) / (kGerberC3 * std::pow(rcm, kGerberC4) - std::log10(sat)) + rcm * rcm * rcm;
            const double radius = 0.01 * std::pow(bracket, 1.0 / 3.0);
            double rrat = radius_dry / radius;
            rrat = rrat * rrat * rrat;
            const double rhop = rrat * rhop_dry + (1.0 - rrat) * kWaterDensity;
            *radius_out = radius;
            *rhop_out = rhop;
        }

        void swell(double radius_dry, double rhop_dry, double rh, int flag, double* radius_out, double* rhop_out) {
            switch (flag) {
            case 1:
                swelling_fitzgerald(radius_dry, rhop_dry, rh, radius_out, rhop_out);
                break;
            case 2:
                swelling_gerber(radius_dry, rhop_dry, rh, radius_out, rhop_out);
                break;
            case 0:
            default:
                // No swelling (and any unknown flag) leaves the particle dry,
                // mirroring the Fortran select-case default branch.
                *radius_out = radius_dry;
                *rhop_out = rhop_dry;
                break;
            }
        }

        int find_klid(const double* press, int n, double plid_hpa) {
            const double plid_pa = plid_hpa * 100.0;
            int klid = 0; // zero-based; Fortran starts at 1 and takes the first minimum
            double ref_diff = 150000.0;
            for (int k = 0; k < n; ++k) {
                const double diff = std::fabs(press[k] - plid_pa);
                if (diff < ref_diff) {
                    klid = k;
                    ref_diff = diff;
                }
            }
            return klid;
        }

        void layer_thickness(const double* hghte_edges, int n, double* dz_out) {
            // Edges are ordered top-to-bottom (length n+1). Level k spans edge k
            // to edge k+1; the bottom level reaches the surface edge at index n.
            for (int k = 0; k < n; ++k) {
                dz_out[k] = hghte_edges[k] - hghte_edges[k + 1];
            }
        }

        double column_mass(const double* qa, const double* delp, int klid0, int n, double grav) {
            const double one_over_g = 1.0 / grav;
            double cmass = 0.0;
            for (int k = klid0; k < n; ++k) {
                cmass += qa[k] * delp[k] * one_over_g;
            }
            return cmass;
        }

        void settling_solver_ufs(double cdt, const double* delp, const double* dz, const double* vs, double* qa,
                                 int n) {
            // tau = vs / dz per level.
            std::vector<double> tau(static_cast<std::size_t>(n));
            double max_tau = 0.0;
            for (int k = 0; k < n; ++k) {
                tau[static_cast<std::size_t>(k)] = vs[k] / dz[k];
                max_tau = std::max(max_tau, tau[static_cast<std::size_t>(k)]);
            }

            // CFL sub-step selection. Reproduces the upstream edge: when the
            // coarse CFL time already exceeds the step, nSubSteps is zero and no
            // transport happens this call.
            const double dt_cfl = kSolverCfl / max_tau;
            int n_sub_steps;
            double dt;
            if (dt_cfl >= cdt) {
                n_sub_steps = 0;
                dt = cdt;
            }
            else {
                n_sub_steps = std::max(1, static_cast<int>(std::ceil(cdt / dt_cfl)));
                dt = cdt / static_cast<double>(n_sub_steps);
            }

            std::vector<double> qa_old(static_cast<std::size_t>(n));
            for (int iit = 0; iit < n_sub_steps; ++iit) {
                std::copy(qa, qa + n, qa_old.begin());

                // Top level: loss only.
                double loss = std::min(1.0, std::max(0.0, dt * tau[0]));
                qa[0] = std::max(0.0, qa[0] * (1.0 - std::min(loss, 1.0)));

                // Interior and bottom: loss plus transfer from the level above,
                // weighted by the pressure-thickness ratio.
                for (int k = 1; k < n; ++k) {
                    loss = std::min(1.0, std::max(0.0, dt * tau[k]));
                    if (delp[k - 1] > kSolverEps && delp[k] > kSolverEps) {
                        const double transfer = (delp[k - 1] / delp[k]) * dt * tau[k - 1];
                        qa[k] = std::max(0.0, qa[k] * (1.0 - std::min(loss, 1.0)) + transfer * qa_old[k - 1]);
                    }
                    else {
                        qa[k] = std::max(0.0, qa[k] * (1.0 - std::min(loss, 1.0)));
                    }
                }
            }
        }

    } // namespace settling
} // namespace catchem

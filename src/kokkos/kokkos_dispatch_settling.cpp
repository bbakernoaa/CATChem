/**
 * @file kokkos_dispatch_settling.cpp
 * @brief Kokkos C++ dispatch for GOCART gravitational settling scheme.
 *
 * Provides a C-linkage function callable from Fortran via ISO_C_BINDING.
 * Wraps batch arrays in Kokkos Views and executes a parallel_for over
 * the batch dimension (one column per work item).
 *
 * The settling physics (Stokes velocity, CFL sub-stepping, particle swelling)
 * are implemented in C++ to match the Fortran SettlingPhysics_Mod routines.
 */

#include "kokkos_common.hpp"
#include <cmath>
#include <algorithm>

namespace catchem {
namespace settling {

using namespace catchem::constants;

// ---------------------------------------------------------------------------
// Device-callable helper functions matching SettlingPhysics_Mod
// ---------------------------------------------------------------------------

/**
 * Stokes settling velocity with Cunningham slip correction.
 * Matches settling_calc_vsettle in SettlingPhysics_Mod.F90.
 */
KOKKOS_INLINE_FUNCTION
double calc_vsettle(double radius, double rhop, double rhoa,
                    double tmpu, double grav) {
   // Dynamic viscosity (Sutherland's equation)
   double rmu = 1.8325e-5 * (416.16 / (tmpu + 120.0)) *
                Kokkos::pow(tmpu / 296.16, 1.5);

   // Thermal velocity of air molecule
   double vt = Kokkos::sqrt(tmpu * F_VT);

   // Mean free path
   double rmfp = 2.0 * rmu / (rhoa * vt);

   // Knudsen number
   double rkn = rmfp / radius;

   // Cunningham slip correction (linearized)
   double bpm = 1.0 + 1.246 * rkn;

   // Stokes fall speed (Re < 0.01 assumption)
   double vsettle = TWO_OVER_NINE * rhop * radius * radius * grav * bpm / rmu;

   // Reynolds number check for drag correction
   double re = 2.0 * rhoa * radius * vsettle / rmu;

   if (re > 0.01) {
      double x = Kokkos::log(24.0 * re / bpm);
      double y = A0 + x * (A1 + x * (A2 + x * (A3 + x * (A4 + x * (A5 + A6 * x)))));
      re = Kokkos::exp(y) * bpm;
      vsettle = 0.5 * rmu * re / (rhoa * radius);
   }

   return vsettle;
}

/**
 * Fitzgerald 1975 hygroscopic growth.
 * Matches settling_swelling_fitzgerald in SettlingPhysics_Mod.F90.
 */
KOKKOS_INLINE_FUNCTION
void swelling_fitzgerald(double rh_val, double radius_dry, double rhop_dry,
                         double& radius_out, double& rhop_out) {
   constexpr double alphaNaCl = 1.35;

   radius_out = radius_dry;
   rhop_out = rhop_dry;

   double sat = rh_val;
   if (sat > 0.80) {
      sat = Kokkos::fmin(0.995, sat);

      double beta = Kokkos::exp((0.00077 * sat) / (1.009 - sat));

      double theta;
      if (sat <= 0.97) {
         theta = 1.058;
      } else {
         theta = 1.058 - (0.0155 * (sat - 0.97)) /
                 (1.02 - Kokkos::pow(sat, 1.4));
      }

      double alpha1 = 1.2 * Kokkos::exp((0.066 * sat) / (theta - sat));
      double alpha = alphaNaCl * alpha1;

      radius_out = alpha * Kokkos::pow(radius_dry, beta);

      double rrat = radius_dry / radius_out;
      rrat = rrat * rrat * rrat;
      rhop_out = rrat * rhop_dry + (1.0 - rrat) * RHOW;
   }
}

/**
 * Gerber 1985 hygroscopic growth.
 * Matches settling_swelling_gerber in SettlingPhysics_Mod.F90.
 */
KOKKOS_INLINE_FUNCTION
void swelling_gerber(double rh_val, double radius_dry, double rhop_dry,
                     double& radius_out, double& rhop_out) {
   constexpr double c1 = 0.7674;
   constexpr double c2 = 3.079;
   constexpr double c3 = 2.573e-11;
   constexpr double c4 = -1.424;

   double sat = Kokkos::fmax(rh_val, 1.0e-30);
   sat = Kokkos::fmin(0.995, sat);

   double rcm = radius_dry * 100.0;  // m -> cm

   radius_out = 0.01 * Kokkos::pow(
      c1 * Kokkos::pow(rcm, c2) / (c3 * Kokkos::pow(rcm, c4) - Kokkos::log10(sat))
      + rcm * rcm * rcm,
      1.0 / 3.0);

   double rrat = radius_dry / radius_out;
   rrat = rrat * rrat * rrat;
   rhop_out = rrat * rhop_dry + (1.0 - rrat) * RHOW;
}

/**
 * Gerber 1985 for Ammonium Sulfate.
 * Matches settling_swelling_gerber_nh4so4 in SettlingPhysics_Mod.F90.
 */
KOKKOS_INLINE_FUNCTION
void swelling_gerber_nh4so4(double rh_val, double radius_dry, double rhop_dry,
                            double& radius_out, double& rhop_out) {
   constexpr double c1 = 0.4809;
   constexpr double c2 = 3.082;
   constexpr double c3 = 3.110e-11;
   constexpr double c4 = -1.428;

   double sat = Kokkos::fmax(rh_val, 1.0e-30);
   sat = Kokkos::fmin(0.995, sat);

   double rcm = radius_dry * 100.0;

   radius_out = 0.01 * Kokkos::pow(
      c1 * Kokkos::pow(rcm, c2) / (c3 * Kokkos::pow(rcm, c4) - Kokkos::log10(sat))
      + rcm * rcm * rcm,
      1.0 / 3.0);

   double rrat = radius_dry / radius_out;
   rrat = rrat * rrat * rrat;
   rhop_out = rrat * rhop_dry + (1.0 - rrat) * RHOW;
}

/**
 * Petters and Kreidenweis 2007 hygroscopic growth.
 * Matches settling_swelling_pk2007 in SettlingPhysics_Mod.F90.
 */
KOKKOS_INLINE_FUNCTION
void swelling_pk2007(double rh_val, double radius_dry, double rhop_dry,
                     double& radius_out, double& rhop_out) {
   double sat = Kokkos::fmin(0.99, rh_val);

   radius_out = radius_dry * Kokkos::pow(
      1.0 + 1.19 * sat / (1.0 - sat), 1.0 / 3.0);

   double rrat = radius_dry / radius_out;
   rrat = rrat * rrat * rrat;
   rhop_out = rrat * rhop_dry + (1.0 - rrat) * RHOW;
}

/**
 * Particle swelling dispatcher.
 * Matches settling_particle_swelling in SettlingPhysics_Mod.F90.
 */
KOKKOS_INLINE_FUNCTION
void particle_swelling(double rh_val, double radius_dry, double rhop_dry,
                       int flag, double& radius_out, double& rhop_out) {
   switch (flag) {
      case 0:
         radius_out = radius_dry;
         rhop_out = rhop_dry;
         break;
      case 1:
         swelling_fitzgerald(rh_val, radius_dry, rhop_dry, radius_out, rhop_out);
         break;
      case 2:
         swelling_gerber(rh_val, radius_dry, rhop_dry, radius_out, rhop_out);
         break;
      case 3:
         swelling_gerber_nh4so4(rh_val, radius_dry, rhop_dry, radius_out, rhop_out);
         break;
      case 4:
         swelling_pk2007(rh_val, radius_dry, rhop_dry, radius_out, rhop_out);
         break;
      default:
         radius_out = radius_dry;
         rhop_out = rhop_dry;
         break;
   }
}

} // namespace settling
} // namespace catchem

// ---------------------------------------------------------------------------
// C-linkage dispatch function
// ---------------------------------------------------------------------------

extern "C" {

/**
 * Kokkos dispatch for GOCART gravitational settling.
 *
 * Receives batch arrays from Fortran, wraps them in Kokkos Views,
 * and executes parallel_for over the batch dimension. Each work item
 * processes one column through the full settling physics:
 *   1. Particle swelling (hygroscopic growth)
 *   2. Stokes settling velocity with Cunningham slip correction
 *   3. Optional Maring upward velocity correction
 *   4. CFL-based sub-stepping time integration
 *
 * All arrays use column-major (LayoutLeft) ordering to match Fortran.
 *
 * @param n_cols           Number of columns in the batch
 * @param n_levels         Number of vertical levels
 * @param n_species        Number of chemical species
 * @param batch_airden     Air density [kg/m3], (n_cols, n_levels)
 * @param batch_delp       Pressure thickness [Pa], (n_cols, n_levels)
 * @param batch_pmid       Mid-layer pressure [Pa], (n_cols, n_levels)
 * @param batch_rh         Relative humidity [0-1], (n_cols, n_levels)
 * @param batch_t          Temperature [K], (n_cols, n_levels)
 * @param batch_z          Edge heights [m], (n_cols, n_levels+1)
 * @param dt               Time step [s]
 * @param species_radius   Dry particle radius per species [m], (n_species)
 * @param species_density  Dry particle density per species [kg/m3], (n_species)
 * @param swelling_flag    Particle swelling method (0-4)
 * @param correction_maring Apply Maring correction (0=no, 1=yes)
 * @param batch_conc       Species concentrations [kg/kg], (n_cols, n_levels, n_species) — inout
 * @param batch_tendency   Updated concentrations [kg/kg], (n_cols, n_levels, n_species) — out
 */
void kokkos_dispatch_settling_gocart(
    int n_cols, int n_levels, int n_species,
    const double* batch_airden,
    const double* batch_delp,
    const double* batch_pmid,
    const double* batch_rh,
    const double* batch_t,
    const double* batch_z,
    double dt,
    const double* species_radius,
    const double* species_density,
    int swelling_flag,
    int correction_maring,
    double* batch_conc,
    double* batch_tendency)
{
   using namespace catchem;
   using namespace catchem::settling;
   using namespace catchem::constants;

   const int nc = n_cols;
   const int nl = n_levels;
   const int ns = n_species;

   // Wrap Fortran pointers as unmanaged Kokkos Views (column-major)
   auto airden = wrap_const_2d(batch_airden, nc, nl);
   auto delp   = wrap_const_2d(batch_delp, nc, nl);
   auto pmid   = wrap_const_2d(batch_pmid, nc, nl);
   auto rh     = wrap_const_2d(batch_rh, nc, nl);
   auto t      = wrap_const_2d(batch_t, nc, nl);
   auto z      = wrap_const_2d(batch_z, nc, nl + 1);
   auto radius = wrap_const_1d(species_radius, ns);
   auto density = wrap_const_1d(species_density, ns);
   auto conc   = wrap_3d(batch_conc, nc, nl, ns);
   auto tend   = wrap_3d(batch_tendency, nc, nl, ns);

   const double grav = 9.80665;  // Standard gravity [m/s2]
   const bool do_maring = (correction_maring != 0);
   const int sw_flag = swelling_flag;

   // Parallel over columns — each work item processes one full column
   Kokkos::parallel_for("settling_gocart", nc, KOKKOS_LAMBDA(const int icol) {

      // Find pressure lid index (level closest to 1 Pa = 0.01 hPa)
      const double plid_pa = 0.01 * 100.0;  // 0.01 hPa -> Pa
      int klid = 0;
      double min_diff = Kokkos::fabs(pmid(icol, 0) - plid_pa);
      for (int k = 1; k < nl; ++k) {
         double diff = Kokkos::fabs(pmid(icol, k) - plid_pa);
         if (diff < min_diff) {
            klid = k;
            min_diff = diff;
         }
      }

      // Process each species
      for (int s = 0; s < ns; ++s) {
         double rad_dry = radius(s);
         double rho_dry = density(s);

         // Skip if radius <= 0
         if (rad_dry <= 0.0) {
            for (int k = 0; k < nl; ++k) {
               tend(icol, k, s) = conc(icol, k, s);
            }
            continue;
         }

         // Working array for concentration (stack-allocated, max 200 levels)
         // For production use with >200 levels, this would need dynamic allocation
         double qa[200];
         double vs[200];
         double dz[200];
         double rad_wet[200];
         double rho_wet[200];

         // Copy concentration to working array
         for (int k = 0; k < nl; ++k) {
            qa[k] = conc(icol, k, s);
         }

         // Compute layer thickness from edge heights
         for (int k = 0; k < nl; ++k) {
            dz[k] = z(icol, k + 1) - z(icol, k);
         }

         // Column mass before settling
         double one_over_g = 1.0 / grav;
         double cmass_before = 0.0;
         for (int k = klid; k < nl; ++k) {
            cmass_before += qa[k] * delp(icol, k) * one_over_g;
         }

         // Particle swelling for each level
         for (int k = 0; k < nl; ++k) {
            particle_swelling(rh(icol, k), rad_dry, rho_dry,
                              sw_flag, rad_wet[k], rho_wet[k]);
         }

         // Settling velocity
         for (int k = 0; k < nl; ++k) {
            vs[k] = 0.0;
         }
         for (int k = klid; k < nl; ++k) {
            vs[k] = calc_vsettle(rad_wet[k], rho_wet[k], airden(icol, k),
                                 t(icol, k), grav);
         }

         // Maring correction
         if (do_maring) {
            for (int k = 0; k < nl; ++k) {
               vs[k] = Kokkos::fmax(1.0e-9, vs[k] - V_UPWARD_MARING);
            }
         }

         // CFL-based sub-stepping (UFS solver, solver_type=2)
         // Compute tau = vs/dz
         double tau[200];
         double max_tau = 0.0;
         for (int k = 0; k < nl; ++k) {
            tau[k] = (dz[k] > 0.0) ? vs[k] / dz[k] : 0.0;
            if (tau[k] > max_tau) max_tau = tau[k];
         }

         int nSubSteps;
         double sub_dt;
         const double cfl_factor = 0.1;

         if (max_tau <= 0.0) {
            nSubSteps = 0;
            sub_dt = dt;
         } else {
            double dt_cfl = cfl_factor / max_tau;
            if (dt_cfl >= dt) {
               nSubSteps = 0;
               sub_dt = dt;
            } else {
               nSubSteps = static_cast<int>(Kokkos::ceil(dt / dt_cfl));
               if (nSubSteps < 1) nSubSteps = 1;
               sub_dt = dt / static_cast<double>(nSubSteps);
            }
         }

         // Time integration (UFS solver with numerical safeguards)
         const double eps = 1.0e-30;
         for (int iit = 0; iit < nSubSteps; ++iit) {
            // Store old values
            double qa_old[200];
            for (int k = 0; k < nl; ++k) qa_old[k] = qa[k];

            // Top layer (only loss)
            double loss = Kokkos::fmax(0.0, Kokkos::fmin(1.0, sub_dt * tau[nl - 1]));
            qa[nl - 1] = Kokkos::fmax(0.0, qa[nl - 1] * (1.0 - Kokkos::fmin(loss, 1.0)));

            // Interior and bottom layers
            for (int k = nl - 2; k >= 0; --k) {
               loss = Kokkos::fmax(0.0, Kokkos::fmin(1.0, sub_dt * tau[k]));

               if (delp(icol, k + 1) > eps && delp(icol, k) > eps) {
                  double transfer = (delp(icol, k + 1) / delp(icol, k)) *
                                    sub_dt * tau[k + 1];
                  qa[k] = Kokkos::fmax(0.0, qa[k] * (1.0 - Kokkos::fmin(loss, 1.0)))
                          + transfer * qa_old[k + 1];
               } else {
                  qa[k] = Kokkos::fmax(0.0, qa[k] * (1.0 - Kokkos::fmin(loss, 1.0)));
               }
            }
         }

         // Write results to tendency array
         for (int k = 0; k < nl; ++k) {
            tend(icol, k, s) = qa[k];
         }

      } // species loop
   }); // parallel_for

   Kokkos::fence();
}

} // extern "C"

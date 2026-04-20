/**
 * @file kokkos_dispatch_seasalt.cpp
 * @brief Kokkos C++ dispatch stubs for sea salt emission schemes.
 *
 * Provides C-linkage functions callable from Fortran via ISO_C_BINDING.
 * These are minimal stubs with correct signatures — the full Kokkos
 * parallel_for implementations will be added when the sea salt scheme
 * kernels are ported to C++.
 *
 * Schemes: Gong 1997, Gong 2003, GEOS-1.2
 */

#include "kokkos_common.hpp"

extern "C" {

/**
 * Kokkos dispatch stub for Gong 1997 sea salt emission scheme.
 *
 * @param n_cols         Number of columns in the batch
 * @param n_levels       Number of vertical levels
 * @param n_species      Number of sea salt species/bins
 * @param batch_u10m     10-m u-wind [m/s], (n_cols)
 * @param batch_v10m     10-m v-wind [m/s], (n_cols)
 * @param batch_frocean  Ocean fraction [0-1], (n_cols)
 * @param batch_frseaice Sea ice fraction [0-1], (n_cols)
 * @param dt             Time step [s]
 * @param batch_emission Emission flux [kg/m2/s], (n_cols, n_species) — out
 */
void kokkos_dispatch_seasalt_gong97(
    int n_cols, int n_levels, int n_species,
    const double* batch_u10m,
    const double* batch_v10m,
    const double* batch_frocean,
    const double* batch_frseaice,
    double dt,
    double* batch_emission)
{
   using namespace catchem;

   const int nc = n_cols;
   const int ns = n_species;

   // Stub: zero out emission array
   // TODO: Implement Gong 1997 sea salt emission physics in Kokkos parallel_for
   auto emission = wrap_2d(batch_emission, nc, ns);

   Kokkos::parallel_for("seasalt_gong97_stub", nc, KOKKOS_LAMBDA(const int icol) {
      for (int s = 0; s < ns; ++s) {
         emission(icol, s) = 0.0;
      }
   });

   Kokkos::fence();
}

/**
 * Kokkos dispatch stub for Gong 2003 sea salt emission scheme.
 */
void kokkos_dispatch_seasalt_gong03(
    int n_cols, int n_levels, int n_species,
    const double* batch_u10m,
    const double* batch_v10m,
    const double* batch_frocean,
    const double* batch_frseaice,
    double dt,
    double* batch_emission)
{
   using namespace catchem;

   const int nc = n_cols;
   const int ns = n_species;

   // Stub: zero out emission array
   // TODO: Implement Gong 2003 sea salt emission physics in Kokkos parallel_for
   auto emission = wrap_2d(batch_emission, nc, ns);

   Kokkos::parallel_for("seasalt_gong03_stub", nc, KOKKOS_LAMBDA(const int icol) {
      for (int s = 0; s < ns; ++s) {
         emission(icol, s) = 0.0;
      }
   });

   Kokkos::fence();
}

/**
 * Kokkos dispatch stub for GEOS-1.2 sea salt emission scheme.
 */
void kokkos_dispatch_seasalt_geos12(
    int n_cols, int n_levels, int n_species,
    const double* batch_u10m,
    const double* batch_v10m,
    const double* batch_frocean,
    const double* batch_frseaice,
    const double* batch_sst,
    double dt,
    double* batch_emission)
{
   using namespace catchem;

   const int nc = n_cols;
   const int ns = n_species;

   // Stub: zero out emission array
   // TODO: Implement GEOS-1.2 sea salt emission physics in Kokkos parallel_for
   auto emission = wrap_2d(batch_emission, nc, ns);

   Kokkos::parallel_for("seasalt_geos12_stub", nc, KOKKOS_LAMBDA(const int icol) {
      for (int s = 0; s < ns; ++s) {
         emission(icol, s) = 0.0;
      }
   });

   Kokkos::fence();
}

} // extern "C"

/**
 * @file kokkos_dispatch_wetdep.cpp
 * @brief Kokkos C++ dispatch stub for wet deposition scheme.
 *
 * Provides a C-linkage function callable from Fortran via ISO_C_BINDING.
 * This is a minimal stub with the correct signature — the full Kokkos
 * parallel_for implementation will be added when the wet deposition
 * scheme kernel is ported to C++.
 *
 * Scheme: Jacob
 */

#include "kokkos_common.hpp"

extern "C" {

/**
 * Kokkos dispatch stub for Jacob wet deposition scheme.
 *
 * @param n_cols          Number of columns in the batch
 * @param n_levels        Number of vertical levels
 * @param n_species       Number of chemical species
 * @param batch_t         Temperature [K], (n_cols, n_levels)
 * @param batch_precip    Precipitation rate [kg/m2/s], (n_cols)
 * @param batch_cloud_frac Cloud fraction [0-1], (n_cols, n_levels)
 * @param dt              Time step [s]
 * @param batch_conc      Species concentrations, (n_cols, n_levels, n_species) — inout
 * @param batch_wetdep    Wet deposition flux [kg/m2/s], (n_cols, n_species) — out
 */
void kokkos_dispatch_wetdep_jacob(
    int n_cols, int n_levels, int n_species,
    const double* batch_t,
    const double* batch_precip,
    const double* batch_cloud_frac,
    double dt,
    double* batch_conc,
    double* batch_wetdep)
{
   using namespace catchem;

   const int nc = n_cols;
   const int ns = n_species;

   // Stub: zero out wet deposition flux
   // TODO: Implement Jacob wet deposition physics in Kokkos parallel_for
   auto wetdep = wrap_2d(batch_wetdep, nc, ns);

   Kokkos::parallel_for("wetdep_jacob_stub", nc, KOKKOS_LAMBDA(const int icol) {
      for (int s = 0; s < ns; ++s) {
         wetdep(icol, s) = 0.0;
      }
   });

   Kokkos::fence();
}

} // extern "C"

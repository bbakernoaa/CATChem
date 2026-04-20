/**
 * @file kokkos_dispatch_drydep.cpp
 * @brief Kokkos C++ dispatch stubs for dry deposition schemes.
 *
 * Provides C-linkage functions callable from Fortran via ISO_C_BINDING.
 * These are minimal stubs with correct signatures — the full Kokkos
 * parallel_for implementations will be added when the dry deposition
 * scheme kernels are ported to C++.
 *
 * Schemes: Wesely, GOCART, Zhang
 */

#include "kokkos_common.hpp"

extern "C" {

/**
 * Kokkos dispatch stub for Wesely dry deposition scheme.
 *
 * @param n_cols       Number of columns in the batch
 * @param n_levels     Number of vertical levels
 * @param n_species    Number of chemical species
 * @param batch_t      Temperature [K], (n_cols, n_levels)
 * @param batch_ustar  Friction velocity [m/s], (n_cols)
 * @param batch_z0     Roughness length [m], (n_cols)
 * @param dt           Time step [s]
 * @param batch_conc   Species concentrations, (n_cols, n_levels, n_species) — inout
 * @param batch_depvel Deposition velocity [m/s], (n_cols, n_species) — out
 */
void kokkos_dispatch_drydep_wesely(
    int n_cols, int n_levels, int n_species,
    const double* batch_t,
    const double* batch_ustar,
    const double* batch_z0,
    double dt,
    double* batch_conc,
    double* batch_depvel)
{
   using namespace catchem;

   const int nc = n_cols;
   const int ns = n_species;

   // Stub: zero out deposition velocity
   // TODO: Implement Wesely dry deposition physics in Kokkos parallel_for
   auto depvel = wrap_2d(batch_depvel, nc, ns);

   Kokkos::parallel_for("drydep_wesely_stub", nc, KOKKOS_LAMBDA(const int icol) {
      for (int s = 0; s < ns; ++s) {
         depvel(icol, s) = 0.0;
      }
   });

   Kokkos::fence();
}

/**
 * Kokkos dispatch stub for GOCART dry deposition scheme.
 */
void kokkos_dispatch_drydep_gocart(
    int n_cols, int n_levels, int n_species,
    const double* batch_t,
    const double* batch_ustar,
    const double* batch_z0,
    double dt,
    double* batch_conc,
    double* batch_depvel)
{
   using namespace catchem;

   const int nc = n_cols;
   const int ns = n_species;

   // Stub: zero out deposition velocity
   // TODO: Implement GOCART dry deposition physics in Kokkos parallel_for
   auto depvel = wrap_2d(batch_depvel, nc, ns);

   Kokkos::parallel_for("drydep_gocart_stub", nc, KOKKOS_LAMBDA(const int icol) {
      for (int s = 0; s < ns; ++s) {
         depvel(icol, s) = 0.0;
      }
   });

   Kokkos::fence();
}

/**
 * Kokkos dispatch stub for Zhang dry deposition scheme.
 */
void kokkos_dispatch_drydep_zhang(
    int n_cols, int n_levels, int n_species,
    const double* batch_t,
    const double* batch_ustar,
    const double* batch_z0,
    double dt,
    double* batch_conc,
    double* batch_depvel)
{
   using namespace catchem;

   const int nc = n_cols;
   const int ns = n_species;

   // Stub: zero out deposition velocity
   // TODO: Implement Zhang dry deposition physics in Kokkos parallel_for
   auto depvel = wrap_2d(batch_depvel, nc, ns);

   Kokkos::parallel_for("drydep_zhang_stub", nc, KOKKOS_LAMBDA(const int icol) {
      for (int s = 0; s < ns; ++s) {
         depvel(icol, s) = 0.0;
      }
   });

   Kokkos::fence();
}

} // extern "C"

!> \file KokkosDispatch_Mod.F90
!! \brief Fortran ISO_C_BINDING interfaces for Kokkos C++ dispatch functions.
!!
!! This module provides Fortran-callable interfaces to the C-linkage dispatch
!! functions implemented in C++. Each dispatch function receives batch arrays
!! from the Fortran batch dispatch pattern and executes the corresponding
!! scheme kernel via Kokkos::parallel_for.
!!
!! Only compiled when ENABLE_KOKKOS=ON.
!!
!! \ingroup kokkos_interop
!!
module KokkosDispatch_Mod

   use iso_c_binding, only: c_int, c_double, c_ptr
   implicit none
   private

   public :: kokkos_dispatch_settling_gocart
   public :: kokkos_dispatch_seasalt_gong97
   public :: kokkos_dispatch_seasalt_gong03
   public :: kokkos_dispatch_seasalt_geos12
   public :: kokkos_dispatch_drydep_wesely
   public :: kokkos_dispatch_drydep_gocart
   public :: kokkos_dispatch_drydep_zhang
   public :: kokkos_dispatch_wetdep_jacob

   interface

      !> Dispatch settling GOCART scheme to Kokkos parallel execution.
      !!
      !! Wraps batch meteorological and chemical arrays in Kokkos Views and
      !! executes a parallel_for over the batch dimension (one column per work item).
      !!
      !! @param[in]     n_cols           Number of columns in the batch
      !! @param[in]     n_levels         Number of vertical levels
      !! @param[in]     n_species        Number of chemical species
      !! @param[in]     batch_airden     Air density (n_cols * n_levels) [kg/m3]
      !! @param[in]     batch_delp       Pressure thickness (n_cols * n_levels) [Pa]
      !! @param[in]     batch_pmid       Mid-layer pressure (n_cols * n_levels) [Pa]
      !! @param[in]     batch_rh         Relative humidity (n_cols * n_levels) [0-1]
      !! @param[in]     batch_t          Temperature (n_cols * n_levels) [K]
      !! @param[in]     batch_z          Edge heights (n_cols * (n_levels+1)) [m]
      !! @param[in]     dt               Time step [s]
      !! @param[in]     species_radius   Dry particle radius per species (n_species) [m]
      !! @param[in]     species_density  Dry particle density per species (n_species) [kg/m3]
      !! @param[in]     swelling_flag    Particle swelling method (0-4)
      !! @param[in]     correction_maring  Apply Maring correction (0=no, 1=yes)
      !! @param[in,out] batch_conc       Species concentrations (n_cols * n_levels * n_species) [kg/kg]
      !! @param[out]    batch_tendency   Updated concentrations (n_cols * n_levels * n_species) [kg/kg]
      subroutine kokkos_dispatch_settling_gocart( &
         n_cols, n_levels, n_species, &
         batch_airden, batch_delp, batch_pmid, batch_rh, batch_t, batch_z, &
         dt, species_radius, species_density, &
         swelling_flag, correction_maring, &
         batch_conc, batch_tendency) &
         bind(C, name="kokkos_dispatch_settling_gocart")

         import :: c_int, c_double, c_ptr

         integer(c_int), value, intent(in) :: n_cols
         integer(c_int), value, intent(in) :: n_levels
         integer(c_int), value, intent(in) :: n_species
         type(c_ptr), value, intent(in) :: batch_airden
         type(c_ptr), value, intent(in) :: batch_delp
         type(c_ptr), value, intent(in) :: batch_pmid
         type(c_ptr), value, intent(in) :: batch_rh
         type(c_ptr), value, intent(in) :: batch_t
         type(c_ptr), value, intent(in) :: batch_z
         real(c_double), value, intent(in) :: dt
         type(c_ptr), value, intent(in) :: species_radius
         type(c_ptr), value, intent(in) :: species_density
         integer(c_int), value, intent(in) :: swelling_flag
         integer(c_int), value, intent(in) :: correction_maring
         type(c_ptr), value, intent(in) :: batch_conc
         type(c_ptr), value, intent(in) :: batch_tendency

      end subroutine kokkos_dispatch_settling_gocart

      !> Dispatch sea salt Gong 1997 scheme to Kokkos parallel execution.
      !!
      !! @param[in]     n_cols           Number of columns in the batch
      !! @param[in]     n_levels         Number of vertical levels
      !! @param[in]     n_species        Number of chemical species
      !! @param[in]     batch_u10m       10-m u-wind (n_cols) [m/s]
      !! @param[in]     batch_v10m       10-m v-wind (n_cols) [m/s]
      !! @param[in]     batch_frocean    Ocean fraction (n_cols) [0-1]
      !! @param[in]     batch_frseaice   Sea ice fraction (n_cols) [0-1]
      !! @param[in]     dt               Time step [s]
      !! @param[out]    batch_emission   Emission flux (n_cols * n_species) [kg/m2/s]
      subroutine kokkos_dispatch_seasalt_gong97( &
         n_cols, n_levels, n_species, &
         batch_u10m, batch_v10m, batch_frocean, batch_frseaice, &
         dt, batch_emission) &
         bind(C, name="kokkos_dispatch_seasalt_gong97")

         import :: c_int, c_double, c_ptr

         integer(c_int), value, intent(in) :: n_cols
         integer(c_int), value, intent(in) :: n_levels
         integer(c_int), value, intent(in) :: n_species
         type(c_ptr), value, intent(in) :: batch_u10m
         type(c_ptr), value, intent(in) :: batch_v10m
         type(c_ptr), value, intent(in) :: batch_frocean
         type(c_ptr), value, intent(in) :: batch_frseaice
         real(c_double), value, intent(in) :: dt
         type(c_ptr), value, intent(in) :: batch_emission

      end subroutine kokkos_dispatch_seasalt_gong97

      !> Dispatch sea salt Gong 2003 scheme to Kokkos parallel execution.
      subroutine kokkos_dispatch_seasalt_gong03( &
         n_cols, n_levels, n_species, &
         batch_u10m, batch_v10m, batch_frocean, batch_frseaice, &
         dt, batch_emission) &
         bind(C, name="kokkos_dispatch_seasalt_gong03")

         import :: c_int, c_double, c_ptr

         integer(c_int), value, intent(in) :: n_cols
         integer(c_int), value, intent(in) :: n_levels
         integer(c_int), value, intent(in) :: n_species
         type(c_ptr), value, intent(in) :: batch_u10m
         type(c_ptr), value, intent(in) :: batch_v10m
         type(c_ptr), value, intent(in) :: batch_frocean
         type(c_ptr), value, intent(in) :: batch_frseaice
         real(c_double), value, intent(in) :: dt
         type(c_ptr), value, intent(in) :: batch_emission

      end subroutine kokkos_dispatch_seasalt_gong03

      !> Dispatch sea salt GEOS-1.2 scheme to Kokkos parallel execution.
      subroutine kokkos_dispatch_seasalt_geos12( &
         n_cols, n_levels, n_species, &
         batch_u10m, batch_v10m, batch_frocean, batch_frseaice, batch_sst, &
         dt, batch_emission) &
         bind(C, name="kokkos_dispatch_seasalt_geos12")

         import :: c_int, c_double, c_ptr

         integer(c_int), value, intent(in) :: n_cols
         integer(c_int), value, intent(in) :: n_levels
         integer(c_int), value, intent(in) :: n_species
         type(c_ptr), value, intent(in) :: batch_u10m
         type(c_ptr), value, intent(in) :: batch_v10m
         type(c_ptr), value, intent(in) :: batch_frocean
         type(c_ptr), value, intent(in) :: batch_frseaice
         type(c_ptr), value, intent(in) :: batch_sst
         real(c_double), value, intent(in) :: dt
         type(c_ptr), value, intent(in) :: batch_emission

      end subroutine kokkos_dispatch_seasalt_geos12

      !> Dispatch dry deposition Wesely scheme to Kokkos parallel execution.
      !!
      !! @param[in]     n_cols           Number of columns in the batch
      !! @param[in]     n_levels         Number of vertical levels
      !! @param[in]     n_species        Number of chemical species
      !! @param[in]     batch_t          Temperature (n_cols * n_levels) [K]
      !! @param[in]     batch_ustar      Friction velocity (n_cols) [m/s]
      !! @param[in]     batch_z0         Roughness length (n_cols) [m]
      !! @param[in]     dt               Time step [s]
      !! @param[in,out] batch_conc       Species concentrations (n_cols * n_levels * n_species)
      !! @param[out]    batch_depvel     Deposition velocity (n_cols * n_species) [m/s]
      subroutine kokkos_dispatch_drydep_wesely( &
         n_cols, n_levels, n_species, &
         batch_t, batch_ustar, batch_z0, &
         dt, batch_conc, batch_depvel) &
         bind(C, name="kokkos_dispatch_drydep_wesely")

         import :: c_int, c_double, c_ptr

         integer(c_int), value, intent(in) :: n_cols
         integer(c_int), value, intent(in) :: n_levels
         integer(c_int), value, intent(in) :: n_species
         type(c_ptr), value, intent(in) :: batch_t
         type(c_ptr), value, intent(in) :: batch_ustar
         type(c_ptr), value, intent(in) :: batch_z0
         real(c_double), value, intent(in) :: dt
         type(c_ptr), value, intent(in) :: batch_conc
         type(c_ptr), value, intent(in) :: batch_depvel

      end subroutine kokkos_dispatch_drydep_wesely

      !> Dispatch dry deposition GOCART scheme to Kokkos parallel execution.
      subroutine kokkos_dispatch_drydep_gocart( &
         n_cols, n_levels, n_species, &
         batch_t, batch_ustar, batch_z0, &
         dt, batch_conc, batch_depvel) &
         bind(C, name="kokkos_dispatch_drydep_gocart")

         import :: c_int, c_double, c_ptr

         integer(c_int), value, intent(in) :: n_cols
         integer(c_int), value, intent(in) :: n_levels
         integer(c_int), value, intent(in) :: n_species
         type(c_ptr), value, intent(in) :: batch_t
         type(c_ptr), value, intent(in) :: batch_ustar
         type(c_ptr), value, intent(in) :: batch_z0
         real(c_double), value, intent(in) :: dt
         type(c_ptr), value, intent(in) :: batch_conc
         type(c_ptr), value, intent(in) :: batch_depvel

      end subroutine kokkos_dispatch_drydep_gocart

      !> Dispatch dry deposition Zhang scheme to Kokkos parallel execution.
      subroutine kokkos_dispatch_drydep_zhang( &
         n_cols, n_levels, n_species, &
         batch_t, batch_ustar, batch_z0, &
         dt, batch_conc, batch_depvel) &
         bind(C, name="kokkos_dispatch_drydep_zhang")

         import :: c_int, c_double, c_ptr

         integer(c_int), value, intent(in) :: n_cols
         integer(c_int), value, intent(in) :: n_levels
         integer(c_int), value, intent(in) :: n_species
         type(c_ptr), value, intent(in) :: batch_t
         type(c_ptr), value, intent(in) :: batch_ustar
         type(c_ptr), value, intent(in) :: batch_z0
         real(c_double), value, intent(in) :: dt
         type(c_ptr), value, intent(in) :: batch_conc
         type(c_ptr), value, intent(in) :: batch_depvel

      end subroutine kokkos_dispatch_drydep_zhang

      !> Dispatch wet deposition Jacob scheme to Kokkos parallel execution.
      !!
      !! @param[in]     n_cols           Number of columns in the batch
      !! @param[in]     n_levels         Number of vertical levels
      !! @param[in]     n_species        Number of chemical species
      !! @param[in]     batch_t          Temperature (n_cols * n_levels) [K]
      !! @param[in]     batch_precip     Precipitation rate (n_cols) [kg/m2/s]
      !! @param[in]     batch_cloud_frac Cloud fraction (n_cols * n_levels) [0-1]
      !! @param[in]     dt               Time step [s]
      !! @param[in,out] batch_conc       Species concentrations (n_cols * n_levels * n_species)
      !! @param[out]    batch_wetdep     Wet deposition flux (n_cols * n_species) [kg/m2/s]
      subroutine kokkos_dispatch_wetdep_jacob( &
         n_cols, n_levels, n_species, &
         batch_t, batch_precip, batch_cloud_frac, &
         dt, batch_conc, batch_wetdep) &
         bind(C, name="kokkos_dispatch_wetdep_jacob")

         import :: c_int, c_double, c_ptr

         integer(c_int), value, intent(in) :: n_cols
         integer(c_int), value, intent(in) :: n_levels
         integer(c_int), value, intent(in) :: n_species
         type(c_ptr), value, intent(in) :: batch_t
         type(c_ptr), value, intent(in) :: batch_precip
         type(c_ptr), value, intent(in) :: batch_cloud_frac
         real(c_double), value, intent(in) :: dt
         type(c_ptr), value, intent(in) :: batch_conc
         type(c_ptr), value, intent(in) :: batch_wetdep

      end subroutine kokkos_dispatch_wetdep_jacob

   end interface

end module KokkosDispatch_Mod

!> \file DryDepScheme_GOCART_Mod.F90
!! \brief GOCART-2G aerosol dry deposition scheme
!!
!! Pure science kernel for gocart scheme in drydep process.
!! This module contains ONLY the computational algorithm with NO infrastructure dependencies.
!! Uses only basic Fortran types for maximum portability and reusability.
!!
!! SCIENCE CUSTOMIZATION GUIDE:
!! 1. Modify the algorithm in compute_gocart (search for "TODO")
!! 2. Add scheme-specific helper subroutines as needed
!! 3. Update physical constants for your scheme
!! 4. Customize the environmental response functions
!!
!! INFRASTRUCTURE RESPONSIBILITIES (handled by host model):
!! - Parameter initialization and validation
!! - Input array validation and error handling
!! - Memory management and array allocation
!! - Integration with host model time stepping
!!
!! Generated on: 2025-11-14T22:58:26.525574
!! Author: Wei Li & Lacey Holland
!! Reference: Allison et al. [2024] Benchmarking GOCART-2G in GEOS
module DryDepScheme_GOCART_Mod

   use precision_mod, only: fp
   use DryDepCommon_Mod, only: DryDepSchemeGOCARTConfig
   use Constants, only: PI  !load the constants needed for this scheme

   implicit none
   private

   ! Public interface - pure science only
   public :: compute_gocart

   ! Additional physical constants (modify as needed for your scheme)
   real(fp), parameter :: T_STANDARD = 303.15_fp    ! Standard reference temperature [K]
   real(fp), parameter :: DEFAULT_SCALING = 1.0e-9_fp ! Default emission scaling factor

contains

   !> Pure science computation for gocart scheme
   !!
   !! This is a pure computational kernel implementing GOCART-2G aerosol dry deposition scheme.
   !! NO error checking, validation, or infrastructure concerns.
   !! Host model must ensure all inputs are valid before calling.
   !!
   !! @param[in]  num_layers     Number of vertical layers
   !! @param[in]  num_species    Number of chemical species
   !! @param[in]  params         Scheme parameters (pre-validated by host)
   !! @param[in]  airden    AIRDEN field [appropriate units]
   !! @param[in]  frlake    FRLAKE field [appropriate units]
   !! @param[in]  gwettop    GWETTOP field [appropriate units]
   !! @param[in]  hflux    HFLUX field [appropriate units]
   !! @param[in]  lwi    LWI field [appropriate units]
   !! @param[in]  nlevs    NLEVS field [appropriate units]
   !! @param[in]  pblh    PBLH field [appropriate units]
   !! @param[in]  t    T field [appropriate units]
   !! @param[in]  tstep    Time step [s] - retrieved from process interface
   !! @param[in]  u10m    U10M field [appropriate units]
   !! @param[in]  ustar    USTAR field [appropriate units]
   !! @param[in]  v10m    V10M field [appropriate units]
   !! @param[in]  z0h    Z0H field [appropriate units]
   !! @param[in]  zmid    ZMID field [appropriate units]
   !! @param[in]  species_conc   Species concentrations [mol/mol] (num_layers, num_species)
   !! @param[inout] species_tendencies  Species tendency terms [mol/mol/s] (num_layers, num_species)
   !! @param[inout] drydep_con_per_species    Dry deposition concentration per species [ug/kg or ppm] (num_species)
   !! @param[inout] drydep_velocity_per_species    Dry deposition velocity [m/s] (num_species)
   !! @param[in] diagnostic_species_id Indices mapping diagnostic species to species array (optional, for per-species diagnostics)
   pure subroutine compute_gocart( &
      num_layers, &
      num_species, &
      params, &
      airden, &
      frlake, &
      gwettop, &
      hflux, &
      lwi, &
      nlevs, &
      pblh, &
      t, &
      tstep, &
      u10m, &
      ustar, &
      v10m, &
      z0h, &
      zmid, &
      species_density, &
      species_radius, &
      species_conc, &
      species_tendencies, &
      is_gas, &
      drydep_con_per_species, &
      drydep_velocity_per_species, &
      diagnostic_species_id &
      )

      ! Arguments
      integer, intent(in) :: num_layers
      integer, intent(in) :: num_species
      type(DryDepSchemeGOCARTConfig), intent(in) :: params
      real(fp), intent(in) :: airden(num_layers)    ! 3D atmospheric field
      real(fp), intent(in) :: frlake  ! Surface field - scalar
      real(fp), intent(in) :: gwettop  ! Surface field - scalar
      real(fp), intent(in) :: hflux  ! Surface field - scalar
      integer, intent(in) :: lwi  ! Surface field - scalar
      real(fp), intent(in) :: nlevs  ! Surface field - scalar
      real(fp), intent(in) :: pblh  ! Surface field - scalar
      real(fp), intent(in) :: t(num_layers)    ! 3D atmospheric field
      real(fp), intent(in) :: tstep  ! Time step [s] - from process interface
      real(fp), intent(in) :: u10m  ! Surface field - scalar
      real(fp), intent(in) :: ustar  ! Surface field - scalar
      real(fp), intent(in) :: v10m  ! Surface field - scalar
      real(fp), intent(in) :: z0h  ! Surface field - scalar
      real(fp), intent(in) :: zmid(num_layers)    ! 3D atmospheric field
      real(fp), intent(in) :: species_density(num_species)  ! Species density property
      real(fp), intent(in) :: species_radius(num_species)  ! Species radius property
      real(fp), intent(in) :: species_conc(num_layers, num_species)
      real(fp), intent(inout) :: species_tendencies(num_layers, num_species)
      logical, intent(in) :: is_gas(num_species)  ! Species type flags (true=gas, false=aerosol)
      real(fp), intent(inout), optional :: drydep_con_per_species(:)
      real(fp), intent(inout), optional :: drydep_velocity_per_species(:)
      integer, intent(in), optional :: diagnostic_species_id(:)  ! Indices mapping diagnostic species to species array

      ! Local variables
      integer :: k, species_idx
      integer :: diag_idx  ! For diagnostic species indexing
      real(fp) :: base_emission_factor
      real(fp) :: environmental_factor
      real(fp) :: species_factor

      ! Note: species_tendencies and diagnostic arrays are already initialized
      ! by the host ProcessInterface before calling this subroutine.
      ! Do not re-initialize them here.

      ! Main computation loop - CUSTOMIZE THIS SECTION FOR YOUR SCHEME
      do k = 1, num_layers

         ! TODO: Replace this generic implementation with your scheme's algorithm
         ! This is a placeholder that demonstrates the expected structure

         ! Initialize environmental factors
         environmental_factor = 1.0_fp

         ! Apply scheme-specific environmental responses based on meteorological fields
         ! Generic field usage (customize for your scheme)
         ! TODO: Consider how AIRDEN affects your emissions
         ! environmental_factor = environmental_factor * some_function(airden(k))
         ! Generic field usage (customize for your scheme)
         ! TODO: Consider how FRLAKE affects your emissions
         ! environmental_factor = environmental_factor * some_function(frlake(k))
         ! Generic field usage (customize for your scheme)
         ! TODO: Consider how GWETTOP affects your emissions
         ! environmental_factor = environmental_factor * some_function(gwettop(k))
         ! Generic field usage (customize for your scheme)
         ! TODO: Consider how HFLUX affects your emissions
         ! environmental_factor = environmental_factor * some_function(hflux(k))
         ! Generic field usage (customize for your scheme)
         ! TODO: Consider how LWI affects your emissions
         ! environmental_factor = environmental_factor * some_function(lwi(k))
         ! Generic field usage (customize for your scheme)
         ! TODO: Consider how NLEVS affects your emissions
         ! environmental_factor = environmental_factor * some_function(nlevs(k))
         ! Generic field usage (customize for your scheme)
         ! TODO: Consider how PBLH affects your emissions
         ! environmental_factor = environmental_factor * some_function(pblh(k))
         ! Generic field usage (customize for your scheme)
         ! TODO: Consider how T affects your emissions
         ! environmental_factor = environmental_factor * some_function(t(k))
         ! Generic field usage (customize for your scheme)
         ! TODO: Consider how TSTEP affects your emissions
         ! environmental_factor = environmental_factor * some_function(tstep(k))
         ! Generic field usage (customize for your scheme)
         ! TODO: Consider how U10M affects your emissions
         ! environmental_factor = environmental_factor * some_function(u10m(k))
         ! Generic field usage (customize for your scheme)
         ! TODO: Consider how USTAR affects your emissions
         ! environmental_factor = environmental_factor * some_function(ustar(k))
         ! Generic field usage (customize for your scheme)
         ! TODO: Consider how V10M affects your emissions
         ! environmental_factor = environmental_factor * some_function(v10m(k))
         ! Generic field usage (customize for your scheme)
         ! TODO: Consider how Z0H affects your emissions
         ! environmental_factor = environmental_factor * some_function(z0h(k))
         ! Generic field usage (customize for your scheme)
         ! TODO: Consider how ZMID affects your emissions
         ! environmental_factor = environmental_factor * some_function(zmid(k))

         ! Apply to each species
         do species_idx = 1, num_species
            ! Skip species that don't match scheme type (gas vs aerosol)
            if (is_gas(species_idx)) cycle
            ! Base emission factor (customize this for species-specific emissions)
            base_emission_factor = DEFAULT_SCALING

            ! Species-specific factor (customize based on species properties)
            species_factor = 1.0_fp  ! TODO: Add species-specific scaling

            ! Compute emission flux using your scheme's formula
            ! This is a simple example - replace with your actual algorithm
            species_tendencies(k, species_idx) = base_emission_factor * &
               environmental_factor * &
               species_factor * &
               (1.0_fp + species_conc(k, species_idx))

            ! Ensure non-negative emissions
            species_tendencies(k, species_idx) = max(0.0_fp, species_tendencies(k, species_idx))

            ! TODO: Update diagnostic fields here based on your scheme's requirements
            ! Each process should implement custom diagnostic calculations
            ! Example patterns:
            ! Per-species diagnostic: only update for diagnostic species
            if (present(drydep_con_per_species) .and. present(diagnostic_species_id)) then
               ! Find position of this species in diagnostic_species_id array
               do diag_idx = 1, size(diagnostic_species_id)
                  if (diagnostic_species_id(diag_idx) == species_idx) then
                     ! Add your custom dry deposition concentration per species calculation
                     drydep_con_per_species(diag_idx) = species_tendencies(k, species_idx) * 1.0_fp  ! TODO: Replace with actual calculation
                     exit
                  end if
               end do
            end if
            ! Per-species diagnostic: only update for diagnostic species
            if (present(drydep_velocity_per_species) .and. present(diagnostic_species_id)) then
               ! Find position of this species in diagnostic_species_id array
               do diag_idx = 1, size(diagnostic_species_id)
                  if (diagnostic_species_id(diag_idx) == species_idx) then
                     ! Add your custom dry deposition velocity calculation
                     drydep_velocity_per_species(diag_idx) = species_tendencies(k, species_idx) * 1.0_fp  ! TODO: Replace with actual calculation
                     exit
                  end if
               end do
            end if
         end do

      end do

   end subroutine compute_gocart

   ! =======================================================================
   ! SCHEME-SPECIFIC HELPER SUBROUTINES
   ! =======================================================================
   ! Add your custom scientific algorithms here as pure functions/subroutines
   ! Examples: environmental response functions, species-specific calculations, etc.

   !> Example helper function for environmental response
   pure function compute_environmental_response_gocart(met_value, reference_value) result(factor)
      real(fp), intent(in) :: met_value       ! Meteorological value
      real(fp), intent(in) :: reference_value ! Reference value
      real(fp) :: factor

      ! Simple exponential response - customize for your scheme
      factor = exp((met_value - reference_value) / reference_value)
      factor = max(0.0_fp, min(10.0_fp, factor))  ! Reasonable bounds
   end function compute_environmental_response_gocart

   !> Example helper function for species-specific scaling
   pure function compute_species_scaling_gocart(species_idx, params) result(scaling)
      integer, intent(in) :: species_idx
      type(DryDepSchemeGOCARTConfig), intent(in) :: params
      real(fp) :: scaling

      ! Species-specific scaling - customize for your scheme
      select case (species_idx)
       case (1)
         scaling = 1.0_fp    ! First species baseline
       case (2:3)
         scaling = 0.5_fp    ! Reduced emission for species 2-3
       case default
         scaling = 0.1_fp    ! Low emission for other species
      end select

   end function compute_species_scaling_gocart

end module DryDepScheme_GOCART_Mod

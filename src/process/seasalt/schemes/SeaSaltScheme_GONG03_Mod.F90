!> \file SeaSaltScheme_GONG03_Mod.F90
!! \brief Gong 2003 sea salt emission scheme with improved sub- and super-micron treatment
!!
!! Pure science kernel for gong03 scheme in seasalt process.
!! This module contains ONLY the computational algorithm with NO infrastructure dependencies.
!! Uses only basic Fortran types for maximum portability and reusability.
!!
!! SCIENCE CUSTOMIZATION GUIDE:
!! 1. Modify the algorithm in compute_gong03 (search for "TODO")
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
!! Generated on: 2025-11-14T23:01:21.740375
!! Author: Barry Baker
!! Reference: Gong [2003]
module SeaSaltScheme_GONG03_Mod

   use precision_mod, only: fp
   use SeaSaltCommon_Mod, only: SeaSaltSchemeGONG03Config
   use Constants, only: PI  !load the constants needed for this scheme

   implicit none
   private

   ! Public interface - pure science only
   public :: compute_gong03

   ! Additional physical constants (modify as needed for your scheme)
   real(fp), parameter :: T_STANDARD = 303.15_fp    ! Standard reference temperature [K]
   real(fp), parameter :: DEFAULT_SCALING = 1.0e-9_fp ! Default emission scaling factor

contains

   !> Pure science computation for gong03 scheme
   !!
   !! This is a pure computational kernel implementing Gong 2003 sea salt emission scheme with improved sub- and super-micron treatment.
   !! NO error checking, validation, or infrastructure concerns.
   !! Host model must ensure all inputs are valid before calling.
   !!
   !! @param[in]  num_layers     Number of vertical layers
   !! @param[in]  num_species    Number of chemical species
   !! @param[in]  params         Scheme parameters (pre-validated by host)
   !! @param[in]  delp    DELP field [appropriate units]
   !! @param[in]  frocean    FROCEAN field [appropriate units]
   !! @param[in]  frseaice    FRSEAICE field [appropriate units]
   !! @param[in]  sst    SST field [appropriate units]
   !! @param[in]  u10m    U10M field [appropriate units]
   !! @param[in]  v10m    V10M field [appropriate units]
   !! @param[in]  species_conc   Species concentrations [mol/mol] (num_layers, num_species)
   !! @param[inout] species_tendencies  Species tendency terms [mol/mol/s] (num_layers, num_species)
   !! @param[inout] seasalt_mass_emission_total    Sea salt mass emission flux total [kg/m2/s]
   !! @param[inout] seasalt_number_emission_total    Sea salt number emission flux total [kg/m2/s]
   !! @param[inout] seasalt_mass_emission_per_bin    Sea salt mass emission flux per bin [kg/m2/s] (num_species)
   !! @param[inout] seasalt_number_emission_per_bin    Sea salt number emission flux per bin [kg/m2/s] (num_species)
   !! @param[in] diagnostic_species_id Indices mapping diagnostic species to species array (optional, for per-species diagnostics)
   pure subroutine compute_gong03( &
      num_layers, &
      num_species, &
      params, &
      delp, &
      frocean, &
      frseaice, &
      sst, &
      u10m, &
      v10m, &
      species_density, &
      species_radius, &
      species_lower_radius, &
      species_upper_radius, &
      species_conc, &
      species_tendencies, &
      seasalt_mass_emission_total, &
      seasalt_number_emission_total, &
      seasalt_mass_emission_per_bin, &
      seasalt_number_emission_per_bin, &
      diagnostic_species_id &
      )

      ! Arguments
      integer, intent(in) :: num_layers
      integer, intent(in) :: num_species
      type(SeaSaltSchemeGONG03Config), intent(in) :: params
      real(fp), intent(in) :: delp(num_layers)    ! 3D atmospheric field
      real(fp), intent(in) :: frocean  ! Surface field - scalar
      real(fp), intent(in) :: frseaice  ! Surface field - scalar
      real(fp), intent(in) :: sst  ! Surface field - scalar
      real(fp), intent(in) :: u10m  ! Surface field - scalar
      real(fp), intent(in) :: v10m  ! Surface field - scalar
      real(fp), intent(in) :: species_density(num_species)  ! Species density property
      real(fp), intent(in) :: species_radius(num_species)  ! Species radius property
      real(fp), intent(in) :: species_lower_radius(num_species)  ! Species lower_radius property
      real(fp), intent(in) :: species_upper_radius(num_species)  ! Species upper_radius property
      real(fp), intent(in) :: species_conc(num_layers, num_species)
      real(fp), intent(inout) :: species_tendencies(num_layers, num_species)
      real(fp), intent(inout), optional :: seasalt_mass_emission_total
      real(fp), intent(inout), optional :: seasalt_number_emission_total
      real(fp), intent(inout), optional :: seasalt_mass_emission_per_bin(:)
      real(fp), intent(inout), optional :: seasalt_number_emission_per_bin(:)
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
         ! TODO: Consider how DELP affects your emissions
         ! environmental_factor = environmental_factor * some_function(delp(k))
         ! Generic field usage (customize for your scheme)
         ! TODO: Consider how FROCEAN affects your emissions
         ! environmental_factor = environmental_factor * some_function(frocean(k))
         ! Generic field usage (customize for your scheme)
         ! TODO: Consider how FRSEAICE affects your emissions
         ! environmental_factor = environmental_factor * some_function(frseaice(k))
         ! Generic field usage (customize for your scheme)
         ! TODO: Consider how SST affects your emissions
         ! environmental_factor = environmental_factor * some_function(sst(k))
         ! Generic field usage (customize for your scheme)
         ! TODO: Consider how U10M affects your emissions
         ! environmental_factor = environmental_factor * some_function(u10m(k))
         ! Generic field usage (customize for your scheme)
         ! TODO: Consider how V10M affects your emissions
         ! environmental_factor = environmental_factor * some_function(v10m(k))

         ! Apply to each species
         do species_idx = 1, num_species
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
            if (present(seasalt_mass_emission_total)) then
               ! Add your custom sea salt mass emission flux total calculation
               seasalt_mass_emission_total = seasalt_mass_emission_total + species_tendencies(k, species_idx) * 1.0_fp  ! TODO: Replace with actual calculation
            end if
            if (present(seasalt_number_emission_total)) then
               ! Add your custom sea salt number emission flux total calculation
               seasalt_number_emission_total = seasalt_number_emission_total + species_tendencies(k, species_idx) * 1.0_fp  ! TODO: Replace with actual calculation
            end if

            ! TODO: Update scheme-specific diagnostic fields here based on your scheme's requirements
            ! Each scheme should implement custom diagnostic calculations
            ! Example patterns:
            ! Per-species diagnostic: only update for diagnostic species
            if (present(seasalt_mass_emission_per_bin) .and. present(diagnostic_species_id)) then
               ! Find position of this species in diagnostic_species_id array
               do diag_idx = 1, size(diagnostic_species_id)
                  if (diagnostic_species_id(diag_idx) == species_idx) then
                     ! Add your custom sea salt mass emission flux per bin calculation
                     seasalt_mass_emission_per_bin(diag_idx) = species_tendencies(k, species_idx) * 1.0_fp  ! TODO: Replace with actual calculation
                     exit
                  end if
               end do
            end if
            ! Per-species diagnostic: only update for diagnostic species
            if (present(seasalt_number_emission_per_bin) .and. present(diagnostic_species_id)) then
               ! Find position of this species in diagnostic_species_id array
               do diag_idx = 1, size(diagnostic_species_id)
                  if (diagnostic_species_id(diag_idx) == species_idx) then
                     ! Add your custom sea salt number emission flux per bin calculation
                     seasalt_number_emission_per_bin(diag_idx) = species_tendencies(k, species_idx) * 1.0_fp  ! TODO: Replace with actual calculation
                     exit
                  end if
               end do
            end if
         end do

      end do

   end subroutine compute_gong03

   ! =======================================================================
   ! SCHEME-SPECIFIC HELPER SUBROUTINES
   ! =======================================================================
   ! Add your custom scientific algorithms here as pure functions/subroutines
   ! Examples: environmental response functions, species-specific calculations, etc.

   !> Example helper function for environmental response
   pure function compute_environmental_response_gong03(met_value, reference_value) result(factor)
      real(fp), intent(in) :: met_value       ! Meteorological value
      real(fp), intent(in) :: reference_value ! Reference value
      real(fp) :: factor

      ! Simple exponential response - customize for your scheme
      factor = exp((met_value - reference_value) / reference_value)
      factor = max(0.0_fp, min(10.0_fp, factor))  ! Reasonable bounds
   end function compute_environmental_response_gong03

   !> Example helper function for species-specific scaling
   pure function compute_species_scaling_gong03(species_idx, params) result(scaling)
      integer, intent(in) :: species_idx
      type(SeaSaltSchemeGONG03Config), intent(in) :: params
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

   end function compute_species_scaling_gong03

end module SeaSaltScheme_GONG03_Mod

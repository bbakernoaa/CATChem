!> \file SettlingScheme_GOCART_Mod.F90
!! \brief GOCART gravitational settling scheme
!!
!! Pure science kernel for gocart scheme in settling process.
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
!! Generated on: 2025-12-17T15:27:52.203209
!! Author: Wei Li
!! Reference: GOCART2G process library Chem_SettlingSimple function
module SettlingScheme_GOCART_Mod

   use precision_mod, only: fp
   use SettlingCommon_Mod, only: SettlingSchemeGOCARTConfig
   use error_mod, only: CC_SUCCESS, CC_Error
   use Constants, only: g0  !load the constants needed for this scheme

   implicit none
   private

   ! Public interface - pure science only
   public :: compute_gocart

   ! Additional physical constants (modify as needed for your scheme)
   real(fp), parameter :: plid = 0.01_fp    ! Pressure lid [hPa]

contains

   !> Pure science computation for gocart scheme
   !!
   !! This is a pure computational kernel implementing GOCART gravitational settling scheme.
   !! NO error checking, validation, or infrastructure concerns.
   !! Host model must ensure all inputs are valid before calling.
   !!
   !! @param[in]  num_layers     Number of vertical layers
   !! @param[in]  num_species    Number of chemical species
   !! @param[in]  params         Scheme parameters (pre-validated by host)
   !! @param[in]  airden    AIRDEN field [kg/m3]
   !! @param[in]  delp    DELP field [Pa]
   !! @param[in]  pmid    PMID field [Pa]
   !! @param[in]  rh    RH field [0-1]
   !! @param[in]  t    T field [K]
   !! @param[in]  tstep    Time step [s] - retrieved from process interface
   !! @param[in]  z     Z field [m] at layer edges, size num_layers+1
   !! @param[in]  species_radius    Species radius property [m] (pre-computed, possibly Mie-derived)
   !! @param[in]  species_density    Species density property [kg/m3] (pre-computed, possibly Mie-derived)
   !! @param[in]  species_conc   Species concentrations [ug/kg] (num_layers, num_species)
   !! @param[inout] species_tendencies  Updated species concentrations [ug/kg] (num_layers, num_species)
   !! @param[inout] settling_velocity_per_species_per_level    settling velocity per species per level [m/s] (num_layers, num_species)
   !! @param[inout] settling_flux_per_species    settling flux per species across column [kg/m2/s] (num_species)
   !! @param[in] diagnostic_species_id Indices mapping diagnostic species to species array (optional, for per-species diagnostics)
   subroutine compute_gocart( &
      num_layers, &
      num_species, &
      params, &
      airden, &
      delp, &
      pmid, &
      rh, &
      t, &
      tstep, &
      z, &
      species_radius, &
      species_density, &
      species_conc, &
      species_tendencies, &
      settling_velocity_per_species_per_level, &
      settling_flux_per_species, &
      diagnostic_species_id &
      )
      ! Uses
      use SettlingPhysics_Mod, only: settling_compute

      ! Arguments
      integer, intent(in) :: num_layers
      integer, intent(in) :: num_species
      type(SettlingSchemeGOCARTConfig), intent(in) :: params
      real(fp), intent(in) :: airden(num_layers)    ! Air density [kg/m3]
      real(fp), intent(in) :: delp(num_layers)      ! Pressure thickness [Pa]
      real(fp), intent(in) :: pmid(num_layers)      ! Mid-layer pressure [Pa]
      real(fp), intent(in) :: rh(num_layers)        ! Relative humidity [0-1]
      real(fp), intent(in) :: t(num_layers)         ! Temperature [K]
      real(fp), intent(in) :: tstep                 ! Time step [s]
      real(fp), intent(in) :: z(num_layers+1)       ! Geopotential height at edges [m]
      real(fp), intent(in) :: species_radius(:)     ! Species radius [m] (pre-computed)
      real(fp), intent(in) :: species_density(:)    ! Species density [kg/m3] (pre-computed)
      real(fp), intent(in) :: species_conc(num_layers, num_species)
      real(fp), intent(inout) :: species_tendencies(num_layers, num_species)
      real(fp), intent(inout), optional :: settling_velocity_per_species_per_level(:,:)
      real(fp), intent(inout), optional :: settling_flux_per_species(:)
      integer, intent(in), optional :: diagnostic_species_id(:)  ! Indices mapping diagnostic species to species array

      ! Local variables
      integer :: rc, species_idx, k
      integer :: diag_idx
      integer :: klid
      real(fp) :: qa(num_layers)           ! concentration in [kg/kg]
      real(fp) :: vsettle(num_layers)      ! settling velocity [m/s]
      real(fp) :: fluxout                  ! flux out across column [kg/m2/s]
      real(fp) :: plid_pa                  ! pressure lid in Pa
      real(fp) :: min_diff, diff

      ! Error information
      character(len=255) :: thisLoc
      character(len=512) :: ErrMsg
      ErrMsg  = ''
      ThisLoc = ' -> at compute_gocart (in process/settling/schemes/SettlingScheme_GOCART_Mod.F90)'

      ! Initialize
      RC = CC_SUCCESS

      ! Compute klid (pressure lid index) from 1D pmid array in native ordering
      ! In bottom-to-top ordering, find the level closest to the pressure lid
      plid_pa = plid * 100.0_fp  ! Convert from hPa to Pa
      klid = 1
      min_diff = abs(pmid(1) - plid_pa)
      do k = 2, num_layers
         diff = abs(pmid(k) - plid_pa)
         if (diff < min_diff) then
            klid = k
            min_diff = diff
         end if
      end do

      ! Main computation loop - apply to each species
      do species_idx = 1, num_species

         ! Convert from ug/kg to kg/kg
         qa(:) = species_conc(:, species_idx) * 1.0e-9_fp

         ! Call internalized settling physics directly with 1D column arrays
         ! No vertical reversal needed - settling_compute operates on native bottom-to-top ordering
         call settling_compute(num_layers, klid, tstep, g0, &
            species_radius(species_idx), species_density(species_idx), &
            params%swelling_method, &
            qa, t, airden, rh, z, delp, &
            vsettle_out=vsettle, fluxout=fluxout, &
            correction_maring=params%correction_maring, &
            solver_type=2, rc=rc)

         if (rc /= CC_SUCCESS .and. rc /= 100) then
            ErrMsg = 'Error in running settling_compute for species.'
            call CC_Error(trim(ErrMsg), RC, thisLoc)
            return
         end if

         ! Convert back from kg/kg to ug/kg and store as updated concentration
         species_tendencies(:, species_idx) = max(0.0_fp, qa(:) * 1.0e9_fp)

         ! Update diagnostic fields if requested
         if (present(settling_velocity_per_species_per_level) .and. present(diagnostic_species_id)) then
            do diag_idx = 1, size(diagnostic_species_id)
               if (diagnostic_species_id(diag_idx) == species_idx) then
                  settling_velocity_per_species_per_level(:, diag_idx) = vsettle(:)
                  exit
               end if
            end do
         end if

         if (present(settling_flux_per_species) .and. present(diagnostic_species_id)) then
            do diag_idx = 1, size(diagnostic_species_id)
               if (diagnostic_species_id(diag_idx) == species_idx) then
                  settling_flux_per_species(diag_idx) = fluxout
                  exit
               end if
            end do
         end if

      end do

   end subroutine compute_gocart

end module SettlingScheme_GOCART_Mod

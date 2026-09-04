! C ABI adapter for the legacy GOCART2G settling science path.
!
! This bridge reproduces the upstream/develop ProcessSettlingInterface
! execution exactly: one `compute_gocart` call per column covering all
! settling species, using the metadata (non-Mie) branch of the scheme.
! Units and layout follow specs/011-restore-numerical-parity/contracts/
! settling-science-bridge.md:
!   - every array is column-major with the flattened column fastest;
!   - the vertical order is bottom-to-top (compute_gocart reverses internally);
!   - concentrations are µg/kg on both sides of the boundary (kg/kg conversion
!     happens inside the scheme);
!   - radius is passed in µm (µm -> m conversion happens inside the scheme).
module SettlingScienceBridge_Mod
   use iso_c_binding, only: c_int, c_double, c_char
   use catchem_bridge_precision, only: fp
   use catchem_bridge_error, only: CC_SUCCESS
   use GOCART2G_MieMod, only: GOCART2G_Mie
   use SettlingCommon_Mod, only: SettlingSchemeGOCARTConfig
   use SettlingScheme_GOCART_Mod, only: compute_gocart
   implicit none
contains
   subroutine run_settling_science_bridge(n_columns, n_levels, n_aerosols, n_total_species, &
      dt, scale_factor, swelling_rh_max, correction_maring, maring_dust_only, &
      airden, delp, pmid, rh, temperature, z_edge, &
      aerosol_species_names, species_names, species_is_dust, species_is_hydrophilic, radius, density, &
      concentration, bridge_rc) &
      bind(C, name='run_settling_science_bridge')
      integer(c_int), value :: n_columns, n_levels, n_aerosols, n_total_species
      integer(c_int), value :: correction_maring, maring_dust_only
      real(c_double), value :: dt, scale_factor, swelling_rh_max
      real(c_double), intent(in) :: airden(n_columns,n_levels), delp(n_columns,n_levels)
      real(c_double), intent(in) :: pmid(n_columns,n_levels), rh(n_columns,n_levels)
      real(c_double), intent(in) :: temperature(n_columns,n_levels)
      real(c_double), intent(in) :: z_edge(n_columns,n_levels+1)
      character(kind=c_char), intent(in) :: aerosol_species_names(32,n_aerosols)
      character(kind=c_char), intent(in) :: species_names(32,n_total_species)
      integer(c_int), intent(in) :: species_is_dust(n_aerosols)
      integer(c_int), intent(in) :: species_is_hydrophilic(n_aerosols)
      real(c_double), intent(in) :: radius(n_aerosols), density(n_aerosols)
      real(c_double), intent(inout) :: concentration(n_columns,n_levels,n_total_species)
      integer(c_int), intent(out) :: bridge_rc

      type(SettlingSchemeGOCARTConfig) :: params
      type(GOCART2G_Mie), allocatable :: mie_data(:)
      character(len=32) :: aerosol_names(n_aerosols)
      integer :: target_species(n_aerosols)
      integer :: species_mie_map(n_aerosols)
      logical :: is_dust(n_aerosols)
      logical :: is_hydrophilic(n_aerosols)
      real(fp) :: species_radius(n_aerosols), species_density(n_aerosols)
      real(fp) :: airden_1d(n_levels), delp_1d(n_levels), pmid_1d(n_levels)
      real(fp) :: rh_1d(n_levels), t_1d(n_levels), z_1d(n_levels+1)
      real(fp) :: conc_2d(n_levels,n_aerosols), tend_2d(n_levels,n_aerosols)
      integer :: column, species, k

      bridge_rc = 0_c_int
      if (n_aerosols <= 0) return

      ! Resolve settling species against the full chemistry list by name
      ! (no index crossing the boundary); mirror upstream trimmed comparison.
      do species = 1, n_aerosols
         aerosol_names(species) = c_name_to_fortran(aerosol_species_names(:,species))
         target_species(species) = 0
         do k = 1, n_total_species
            if (trim(c_name_to_fortran(species_names(:,k))) == trim(aerosol_names(species))) then
               target_species(species) = k
               exit
            end if
         end do
         if (target_species(species) == 0) then
            bridge_rc = 1_c_int
            return
         end if
      end do

      ! Scheme parameters for the metadata (non-Mie) path.  scale_factor is
      ! retained for configuration compatibility but, exactly like upstream,
      ! compute_gocart does not consume it on this path.
      params%scheme_name = 'gocart'
      params%scale_factor = real(scale_factor, fp)
      params%simple_scheme = .false.
      params%swelling_rh_max = real(swelling_rh_max, fp)
      params%correction_maring = (correction_maring /= 0)
      params%maring_dust_only = (maring_dust_only /= 0)

      ! Metadata path: no Mie tables (mirrors upstream simple_scheme=false).
      allocate(mie_data(0))
      species_mie_map = 0
      is_dust = (species_is_dust /= 0)
      is_hydrophilic = (species_is_hydrophilic /= 0)
      do species = 1, n_aerosols
         ! Radii stay in µm: the scheme performs the µm -> m conversion.
         species_radius(species) = real(radius(species), fp)
         species_density(species) = real(density(species), fp)
      end do

      do column = 1, n_columns
         do k = 1, n_levels
            airden_1d(k) = real(airden(column,k), fp)
            delp_1d(k) = real(delp(column,k), fp)
            pmid_1d(k) = real(pmid(column,k), fp)
            rh_1d(k) = real(rh(column,k), fp)
            t_1d(k) = real(temperature(column,k), fp)
         end do
         do k = 1, n_levels + 1
            z_1d(k) = real(z_edge(column,k), fp)
         end do
         do species = 1, n_aerosols
            do k = 1, n_levels
               ! µg/kg both sides; kg/kg conversion happens inside the scheme.
               conc_2d(k,species) = real(concentration(column,k,target_species(species)), fp)
               tend_2d(k,species) = 0.0_fp
            end do
         end do

         ! One call per column for all settling species, exactly like the
         ! upstream run_gocart_scheme_column.  Scheme-internal failures report
         ! through CC_Error (stderr banner) and return without aborting, which
         ! is the legacy behavior; replacement tendencies are written back for
         ! whatever the scheme produced.
         call compute_gocart(n_levels, n_aerosols, params, &
            airden_1d, delp_1d, pmid_1d, rh_1d, t_1d, real(dt, fp), z_1d, &
            aerosol_names, mie_data, species_mie_map, species_radius, species_density, &
            is_dust, is_hydrophilic, conc_2d, tend_2d)

         do species = 1, n_aerosols
            do k = 1, n_levels
               ! Replacement tendencies (max(0, qa) applied inside the scheme).
               concentration(column,k,target_species(species)) = real(tend_2d(k,species), c_double)
            end do
         end do
      end do
   contains
      function c_name_to_fortran(c_name) result(name)
         character(kind=c_char), intent(in) :: c_name(32)
         character(len=32) :: name
         integer :: i
         name = ''
         do i = 1, size(c_name)
            name(i:i) = c_name(i)
         end do
         name = trim(adjustl(name))
      end function c_name_to_fortran
   end subroutine run_settling_science_bridge
end module SettlingScienceBridge_Mod

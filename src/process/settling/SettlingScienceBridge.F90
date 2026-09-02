! C ABI adapter for the metadata-based legacy GOCART settling implementation.
module SettlingScienceBridge_Mod
   use iso_c_binding, only: c_int, c_double, c_char
   use catchem_bridge_precision, only: fp
   use SettlingPhysics_Mod, only: settling_compute
   implicit none
contains
   subroutine run_settling_science_bridge(n_columns, n_levels, n_aerosols, n_total_species, dt, scale_factor, swelling_method, correction_maring, &
      airden, delp, rh, temperature, z_edge, aerosol_species_names, species_names, radius, density, concentration, bridge_rc) &
      bind(C, name='run_settling_science_bridge')
      integer(c_int), value :: n_columns, n_levels, n_aerosols, n_total_species, swelling_method, correction_maring
      real(c_double), value :: dt, scale_factor
      real(c_double), intent(in) :: airden(n_columns,n_levels), delp(n_columns,n_levels), rh(n_columns,n_levels)
      real(c_double), intent(in) :: temperature(n_columns,n_levels), z_edge(n_columns,n_levels+1)
      character(kind=c_char), intent(in) :: aerosol_species_names(32,n_aerosols), species_names(32,n_total_species)
      real(c_double), intent(in) :: radius(n_aerosols), density(n_aerosols)
      real(c_double), intent(inout) :: concentration(n_columns,n_levels,n_total_species)
      integer(c_int), intent(out) :: bridge_rc
      integer :: column, species, k, rc, target_species(n_aerosols)
      character(len=32) :: aerosol_name, target_name
      real(fp) :: qa(n_levels)

      bridge_rc = 0_c_int
      do species = 1, n_aerosols
         aerosol_name = c_name_to_fortran(aerosol_species_names(:,species))
         target_species(species) = 0
         do k = 1, n_total_species
            target_name = c_name_to_fortran(species_names(:,k))
            if (trim(target_name) == trim(aerosol_name)) then
               target_species(species) = k
               exit
            end if
         end do
         if (target_species(species) == 0) then
            bridge_rc = 1_c_int
            return
         end if
      end do

      do species = 1, n_aerosols
         do column = 1, n_columns
            do k = 1, n_levels
               ! CATChem stores aerosol mixing ratios in ug/kg.  The
               ! internalized GOCART solver operates in kg/kg.
               qa(k) = real(concentration(column,k,target_species(species)), fp) * 1.0e-9_fp
            end do
            call settling_compute(n_levels, 1, real(dt,fp), 9.80665_fp, real(radius(species),fp), &
               real(density(species),fp), swelling_method, qa, real(temperature(column,:),fp), &
               real(airden(column,:),fp), real(rh(column,:),fp), real(z_edge(column,:),fp), &
               real(delp(column,:),fp), correction_maring=(correction_maring /= 0), &
               scale_factor=real(scale_factor,fp), solver_type=2, rc=rc)
            if (rc /= 0) then
               bridge_rc = int(rc, c_int)
               return
            end if
            do k = 1, n_levels
               concentration(column,k,target_species(species)) = real(qa(k), c_double) * 1.0e9_c_double
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

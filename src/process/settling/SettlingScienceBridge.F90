! C ABI adapter for the metadata-based legacy GOCART settling implementation.
module SettlingScienceBridge_Mod
   use iso_c_binding, only: c_int, c_double
   use catchem_bridge_precision, only: fp
   use SettlingPhysics_Mod, only: settling_compute
   implicit none
contains
   subroutine run_settling_science_bridge(n_columns, n_levels, n_species, dt, scale_factor, swelling_method, correction_maring, &
      airden, delp, rh, temperature, z_edge, aerosol_indices, radius, density, concentration, bridge_rc) &
      bind(C, name='run_settling_science_bridge')
      integer(c_int), value :: n_columns, n_levels, n_species, swelling_method, correction_maring
      real(c_double), value :: dt, scale_factor
      real(c_double), intent(in) :: airden(n_columns,n_levels), delp(n_columns,n_levels), rh(n_columns,n_levels)
      real(c_double), intent(in) :: temperature(n_columns,n_levels), z_edge(n_columns,n_levels+1)
      integer(c_int), intent(in) :: aerosol_indices(n_species)
      real(c_double), intent(in) :: radius(n_species), density(n_species)
      real(c_double), intent(inout) :: concentration(n_columns,n_levels,*)
      integer(c_int), intent(out) :: bridge_rc
      integer :: column, species, k, rc
      real(fp) :: qa(n_levels)

      bridge_rc = 0_c_int
      do species = 1, n_species
         do column = 1, n_columns
            do k = 1, n_levels
               ! CATChem stores aerosol mixing ratios in ug/kg.  The
               ! internalized GOCART solver operates in kg/kg.
               qa(k) = real(concentration(column,k,aerosol_indices(species)), fp) * 1.0e-9_fp
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
               concentration(column,k,aerosol_indices(species)) = real(qa(k), c_double) * 1.0e9_c_double
            end do
         end do
      end do
   end subroutine run_settling_science_bridge
end module SettlingScienceBridge_Mod

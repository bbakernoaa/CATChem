!> \file catchem_ccpp_layout.F90
!! \brief Copy between the host constituent layout and CATChem's working layout.
!!
!! Host arrays are framework-owned and may be noncontiguous.  These routines
!! therefore use explicit maps and temporary arrays rather than assuming that
!! the host and CATChem storage orders match.
module catchem_ccpp_layout
   use iso_c_binding, only: c_double, c_int
   use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
   use machine, only: kind_phys
   implicit none
   private
   integer(c_int), parameter, public :: CATCHEM_LAYOUT_OK = 0_c_int
   integer(c_int), parameter, public :: CATCHEM_LAYOUT_INVALID = 1_c_int
   public :: catchem_ccpp_copy_layout, catchem_ccpp_scatter_layout
contains
   !> \brief Copy host fields by explicit constituent and vertical maps.
   !> \param host Full host array in column, host-layer, constituent order.
   !> \param host_to_core One-based host slot for each CATChem species.
   !> \param core_layer_to_host One-based host layer for each CATChem layer.
   !> \param canonical Output with contiguous CATChem column, layer, species order.
   !> \param status Zero on success; otherwise output is left unchanged.
   subroutine catchem_ccpp_copy_layout(host, host_to_core, core_layer_to_host, canonical, status)
      real(kind_phys), intent(in) :: host(:, :, :)
      integer, intent(in) :: host_to_core(:), core_layer_to_host(:)
      real(c_double), intent(inout) :: canonical(:, :, :)
      integer(c_int), intent(out) :: status
      real(c_double), allocatable :: candidate(:, :, :)
      real(c_double) :: value
      integer :: col, layer, species, ierr

      status = CATCHEM_LAYOUT_INVALID
      if (size(host, 1) <= 0 .or. size(host, 2) <= 0 .or. size(host, 3) <= 0) return
      if (size(canonical, 1) <= 0 .or. size(canonical, 2) <= 0 .or. size(canonical, 3) <= 0) return
      if (size(host, 1) /= size(canonical, 1) .or. size(host_to_core) /= size(canonical, 3) .or. &
          size(core_layer_to_host) /= size(canonical, 2) .or. size(host, 2) /= size(canonical, 2)) return
      if (any(host_to_core < 1) .or. any(host_to_core > size(host, 3))) return
      if (any(core_layer_to_host < 1) .or. any(core_layer_to_host > size(host, 2))) return

      ! Reject aliased map entries so a later scatter cannot overwrite a value
      ! that another CATChem species or layer also addresses.
      do species = 1, size(host_to_core)
         if (any(host_to_core(1:species - 1) == host_to_core(species))) return
      end do
      do layer = 1, size(core_layer_to_host)
         if (any(core_layer_to_host(1:layer - 1) == core_layer_to_host(layer))) return
      end do

      ! Fill a temporary first; the caller's canonical array changes only after
      ! every mapped host value has passed validation.
      allocate (candidate(size(canonical, 1), size(canonical, 2), size(canonical, 3)), stat=ierr)
      if (ierr /= 0) return
      do species = 1, size(host_to_core)
         do layer = 1, size(core_layer_to_host)
            do col = 1, size(host, 1)
               value = real(host(col, core_layer_to_host(layer), host_to_core(species)), c_double)
               if (.not. ieee_is_finite(value)) return
               candidate(col, layer, species) = value
            end do
         end do
      end do
      canonical = candidate
      status = CATCHEM_LAYOUT_OK
   end subroutine catchem_ccpp_copy_layout

   !> \brief Atomically scatter canonical CATChem values into mapped host slots.
   !> \param canonical Contiguous CATChem column, layer, species order.
   !> \param host_to_core One-based host slot for each CATChem species.
   !> \param core_layer_to_host One-based host layer for each CATChem layer.
   !> \param host Full host array; unmapped constituents remain unchanged.
   !> \param status Zero on success; otherwise host is left unchanged.
   subroutine catchem_ccpp_scatter_layout(canonical, host_to_core, core_layer_to_host, host, status)
      real(c_double), intent(in) :: canonical(:, :, :)
      integer, intent(in) :: host_to_core(:), core_layer_to_host(:)
      real(kind_phys), intent(inout) :: host(:, :, :)
      integer(c_int), intent(out) :: status
      real(kind_phys), allocatable :: candidate(:, :, :)
      integer :: col, layer, species, ierr

      status = CATCHEM_LAYOUT_INVALID
      if (size(host, 1) <= 0 .or. size(host, 2) <= 0 .or. size(host, 3) <= 0) return
      if (size(canonical, 1) <= 0 .or. size(canonical, 2) <= 0 .or. size(canonical, 3) <= 0) return
      if (size(host, 1) /= size(canonical, 1) .or. size(host_to_core) /= size(canonical, 3) .or. &
          size(core_layer_to_host) /= size(canonical, 2) .or. size(host, 2) /= size(canonical, 2)) return
      if (any(host_to_core < 1) .or. any(host_to_core > size(host, 3))) return
      if (any(core_layer_to_host < 1) .or. any(core_layer_to_host > size(host, 2))) return

      ! Validate one-to-one mappings before constructing a candidate host array.
      do species = 1, size(host_to_core)
         if (any(host_to_core(1:species - 1) == host_to_core(species))) return
      end do
      do layer = 1, size(core_layer_to_host)
         if (any(core_layer_to_host(1:layer - 1) == core_layer_to_host(layer))) return
      end do

      allocate (candidate(size(host, 1), size(host, 2), size(host, 3)), stat=ierr)
      if (ierr /= 0) return
      candidate = host

      ! Start from the full host state so constituents outside CATChem's map are
      ! preserved exactly.
      do species = 1, size(host_to_core)
         do layer = 1, size(core_layer_to_host)
            do col = 1, size(host, 1)
               if (.not. ieee_is_finite(canonical(col, layer, species))) return
               if (abs(canonical(col, layer, species)) > real(huge(0.0_kind_phys), c_double)) return
               candidate(col, core_layer_to_host(layer), host_to_core(species)) = &
                  real(canonical(col, layer, species), kind_phys)
               if (.not. ieee_is_finite(candidate(col, core_layer_to_host(layer), host_to_core(species)))) return
            end do
         end do
      end do
      host = candidate
      status = CATCHEM_LAYOUT_OK
   end subroutine catchem_ccpp_scatter_layout
end module catchem_ccpp_layout

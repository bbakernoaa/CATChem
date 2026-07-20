!> \file catchem_types.F90
!! \brief Container module for CATCHEM model data structures
!!
!! \details
!! Defines the core data structures used by CATCHEM chemistry model including
!! arrays for meteorological states, chemical species concentrations, emissions,
!! and diagnostic outputs. Provides the fundamental data container types needed
!! for chemistry calculations.
!!
!! \author Barry Baker
!!
!! \date 11/2024
!!
!! \ingroup catchem_ccpp_group
!!!>
module catchem_types

  use CATChem, only: GridStateType, MetStateType, ChemStateType, EmisStateType, DiagStateType

  implicit none
  private


  !> Container type for CATCHEM model data
  !!
  !! Holds arrays for meteorological, chemical, emission and diagnostic states
  !! along with the horizontal dimension parameter
  !!
  !! \ingroup catchem_ccpp_group
  !!!>
  type, public :: catchem_container_type
    ! Array dimensions
    integer :: im = 0  ! Horizontal dimension

    ! State arrays
    type(GridStateType)  :: GridState   !> Grid state (not an array)
    type(MetStateType),  allocatable :: MetState(:)    !> Meteorological state array
    type(ChemStateType), allocatable :: ChemState(:)   !> Chemical state array
    type(EmisStateType), allocatable :: EmisState(:)   !> Emission state array
    type(DiagStateType), allocatable :: DiagState(:)   !> Diagnostic state array

  end type catchem_container_type

end module catchem_types

!>\file catchem_config.F90
!! This file contains the CATChem configuration settings
!!
!! \author Barry.Baker@noaa.gov
!! \date 09/2024
!! namelist parameters etc for CATChem
!!
!! \defgroup CATChem_ccpp
!! \brief This is the Common Community Physics Package (CCPP) interface for CATChem
!! \ingroup CATChem_ccpp
!!
!!>
module catchem_config
    use machine, only  : kind_phys

    implicit none

    character(len=255) :: CATCHem_ConfigFile = './CATChem_config.yml'
    logical :: do_catchem = .false.
    logical :: export_catchem_diags = .false.
    integer :: n_dbg_lines = 3

end module catchem_config

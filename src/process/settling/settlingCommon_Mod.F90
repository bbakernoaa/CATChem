!> \file settlingCommon_Mod.F90
!! \brief Common utilities for settling process
!! \ingroup settling_modules
!!
!! \author CATChem Development Team
!! \date 2025
!! \version 1.0
!!
!! This module provides common utility functions for the settling process.
!! It contains pure computational routines that are independent of state management.
!!
module settlingCommon_Mod
   use precision_mod
   use error_mod

   implicit none
   private

   ! Public constants
   public :: SETTLING_VERSION
   public :: SETTLING_NAME

   ! Public utility functions

   ! Module constants
   real(fp), parameter :: SETTLING_VERSION = 1.0_fp
   character(len=*), parameter :: SETTLING_NAME = 'settling'


contains


end module settlingCommon_Mod

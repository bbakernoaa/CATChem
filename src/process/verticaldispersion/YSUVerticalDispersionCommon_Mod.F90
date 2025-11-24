!> \file ysuverticaldispersionCommon_Mod.F90
!! \brief Common utilities for ysuverticaldispersion process
!! \ingroup ysuverticaldispersion_modules
!!
!! \author CATChem Development Team
!! \date 2025
!! \version 1.0
!!
!! This module provides common utility functions for the ysuverticaldispersion process.
!! It contains pure computational routines that are independent of state management.
!!
module ysuverticaldispersionCommon_Mod
   use precision_mod
   use error_mod

   implicit none
   private

   ! Public constants
   public :: YSUVERTICALDISPERSION_VERSION
   public :: YSUVERTICALDISPERSION_NAME

   ! Public utility functions

   ! Module constants
   real(fp), parameter :: YSUVERTICALDISPERSION_VERSION = 1.0_fp
   character(len=*), parameter :: YSUVERTICALDISPERSION_NAME = 'ysuverticaldispersion'


contains


end module ysuverticaldispersionCommon_Mod

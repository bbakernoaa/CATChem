!> \file ccpp_catchem_interface.F90
!! \brief CCPP interface module for CATChem integration
!!
!! This is the modernized, CCPP-Compliant wrapper for interfacing the CATChem
!! chemistry model with the CCPP framework. All calculations and states are managed
!! by the high-performance C++ core, completely bypassing duplicate Fortran states.
!!
module ccpp_catchem_interface

  use iso_c_binding, only: c_loc, c_null_char, c_double, c_ptr, c_associated
  use machine, only: kind_phys
  use CATChem_API, only: CATChem_Model
  use Error_Mod, only: CC_SUCCESS, CC_FAILURE

implicit none

private

public :: ccpp_catchem_interface_init, ccpp_catchem_interface_run, ccpp_catchem_interface_finalize

! Private module-scope wrapper mapping directly to the C++ core
type(CATChem_Model), save :: cc_model

contains

   !> \brief Initialize the CATChem CCPP interface
   subroutine ccpp_catchem_interface_init(im, do_catchem, catchem_configfile_in, errmsg, errflg)
      implicit none

      character(len=*), intent(in) :: catchem_configfile_in
      logical,          intent(in) :: do_catchem
      integer,          intent(in) :: im

      character(len=*), intent(out) :: errmsg
      integer,          intent(out) :: errflg

      errmsg = ''
      errflg = 0

      if (.not. do_catchem) return

      ! Initialize the modern C++ core manager directly
      ! Passing standard default sizes (kte=127, nsoil=3, nsoiltype=5, nsurftype=20)
      call cc_model%initialize(catchem_configfile_in, im, 1, 127, 3, 5, 20, errflg)
      if (errflg /= CC_SUCCESS) then
         errmsg = 'Error initializing C++ Core via cc_model'
      end if

   end subroutine ccpp_catchem_interface_init

  !> \brief Finalize the CATChem CCPP interface
  subroutine ccpp_catchem_interface_finalize(do_catchem, errmsg, errflg)
   implicit none

   logical, intent(in) :: do_catchem

   character(len=*), intent(out) :: errmsg
   integer, intent(out) :: errflg

   errmsg = ''
   errflg = 0

   if (.not. do_catchem) return

   call cc_model%finalize(errflg)
   if (errflg /= CC_SUCCESS) then
         errmsg = 'Error finalising C++ Core via cc_model'
   end if

  end subroutine ccpp_catchem_interface_finalize

  !> \brief Execute CATChem chemistry calculations within CCPP framework
  subroutine ccpp_catchem_interface_run(im, kte, kme, garea, nsoil, nlndcat, nsoilcat, &
     lat, lon, &
     do_catchem, &
     dt, jdate, &
     xcosz, &
     lwi, frlanduse, gvf, seaicefrac, oceanfrac, lakefrac, landfrac, &
     stype, vtype, snowdepth, frsnow, lai, frsoil, pores, resid, &
     ustar, u10m, v10m, tskin, ts, hf2d, lf2d, znt, prsfc, pblh, &
     dswsfc, nirbmdi, nirdfdi, visbmdi, visdfdi, &
     sfc_alb_nir_dir, sfc_alb_nir_dif, sfc_alb_uvvis_dir, sfc_alb_uvvis_dif, &
     soilmoist, pr3d, phl3d, prl3d, tk3d, q3d, us3d, vs3d, rh, &
     delp, airden, pfl_lsan, pfl_isan, &
     rain_cpl, cldf, &
     dust_in, &
     ntrac, ntchs, ntchm, chemarr_phys, chemarr, &
     errmsg, errflg)

     implicit none

     integer, intent(in) :: im
     integer, intent(in) :: kte
     integer, intent(in) :: kme
     integer, intent(in) :: nsoil
     integer, intent(in) :: nlndcat
     integer, intent(in) :: nsoilcat
     real(kind_phys), dimension(im), intent(in), target :: garea
     real(kind_phys), dimension(im), intent(in), target :: lat
     real(kind_phys), dimension(im), intent(in), target :: lon
     real(kind_phys), dimension(im), intent(in), target :: xcosz

     real(kind_phys), intent(in) :: dt
     integer, intent(in) :: jdate(8)

     logical, intent(in) :: do_catchem

     integer, intent(in) :: ntrac
     integer, intent(in) :: ntchs
     integer, intent(in) :: ntchm
     real(kind_phys), dimension(im, kte, ntrac), intent(inout), target :: chemarr_phys
     real(kind_phys), dimension(im, kte, ntrac), intent(inout), target :: chemarr

     integer, dimension(im), intent(in), target                :: lwi
     integer, dimension(im), intent(in), target                :: stype
     integer, dimension(im), intent(in), target                :: vtype

     real(kind_phys), dimension(im, nlndcat), intent(in), target :: frlanduse
     real(kind_phys), dimension(im, nsoilcat), intent(in), target :: frsoil
     real(kind_phys), dimension(30), intent(in), target        :: pores
     real(kind_phys), dimension(30), intent(in), target        :: resid
     real(kind_phys), dimension(im), intent(in), target        :: seaicefrac
     real(kind_phys), dimension(im), intent(in), target        :: oceanfrac
     real(kind_phys), dimension(im), intent(in), target        :: frsnow
     real(kind_phys), dimension(im), intent(in), target        :: lakefrac
     real(kind_phys), dimension(im), intent(in), target        :: landfrac
     real(kind_phys), dimension(im), intent(in), target        :: gvf
     real(kind_phys), dimension(im), intent(in), target        :: lai

     real(kind_phys), dimension(im, nsoil), intent(in), target :: soilmoist
     real(kind_phys), dimension(im), intent(in), target        :: snowdepth
     real(kind_phys), dimension(im), intent(in), target        :: prsfc
     real(kind_phys), dimension(im), intent(in), target        :: pblh
     real(kind_phys), dimension(im), intent(in), target        :: u10m
     real(kind_phys), dimension(im), intent(in), target        :: v10m
     real(kind_phys), dimension(im), intent(in), target        :: ustar
     real(kind_phys), dimension(im), intent(in), target        :: tskin
     real(kind_phys), dimension(im), intent(in), target        :: ts
     real(kind_phys), dimension(im), intent(in), target        :: hf2d
     real(kind_phys), dimension(im), intent(in), target        :: lf2d
     real(kind_phys), dimension(im), intent(in), target        :: znt
     real(kind_phys), dimension(im), intent(in), target        :: dswsfc
     real(kind_phys), dimension(im), intent(in), target        :: sfc_alb_nir_dir
     real(kind_phys), dimension(im), intent(in), target        :: sfc_alb_nir_dif
     real(kind_phys), dimension(im), intent(in), target        :: sfc_alb_uvvis_dir
     real(kind_phys), dimension(im), intent(in), target        :: sfc_alb_uvvis_dif
     real(kind_phys), dimension(im), intent(in), target        :: nirbmdi
     real(kind_phys), dimension(im), intent(in), target        :: nirdfdi
     real(kind_phys), dimension(im), intent(in), target        :: visbmdi
     real(kind_phys), dimension(im), intent(in), target        :: visdfdi

     real(kind_phys), dimension(im, kme), intent(in), target :: pr3d
     real(kind_phys), dimension(im, kte), intent(in), target :: prl3d
     real(kind_phys), dimension(im, kte), intent(in), target :: delp
     real(kind_phys), dimension(im, kte), intent(in), target :: phl3d
     real(kind_phys), dimension(im, kte), intent(in), target :: tk3d
     real(kind_phys), dimension(im, kte), intent(in), target :: us3d
     real(kind_phys), dimension(im, kte), intent(in), target :: vs3d
     real(kind_phys), dimension(im, kte), intent(in), target :: q3d
     real(kind_phys), dimension(im, kte), intent(in), target :: airden
     real(kind_phys), dimension(im, kte), intent(in), target :: rh
     real(kind_phys), dimension(im, kte), intent(in), target :: pfl_lsan
     real(kind_phys), dimension(im, kte), intent(in), target :: pfl_isan

     real(kind_phys), dimension(im), intent(in), target        :: rain_cpl
     real(kind_phys), dimension(im), intent(in), target        :: cldf
     real(kind_phys), dimension(im, 12, 5), intent(in), target :: dust_in

     character(len=*), intent(out) :: errmsg
     integer, intent(out) :: errflg

     errmsg = ''
     errflg = 0

     if (.not. do_catchem) return

     ! 1. Direct Zero-Copy standard array bindings to unmanaged LayoutLeft C++ Views
     ! Bind volumetric meteorological fields
     call cc_model%bind_met_3d("T"//c_null_char, c_loc(tk3d(1,1)))
     call cc_model%bind_met_3d("QV"//c_null_char, c_loc(q3d(1,1)))
     call cc_model%bind_met_3d("RH"//c_null_char, c_loc(rh(1,1)))
     call cc_model%bind_met_3d("PMID"//c_null_char, c_loc(prl3d(1,1)))
     call cc_model%bind_met_3d("PEDGE"//c_null_char, c_loc(pr3d(1,1)))
     call cc_model%bind_met_3d("DELP"//c_null_char, c_loc(delp(1,1)))
     call cc_model%bind_met_3d("AIRDEN"//c_null_char, c_loc(airden(1,1)))

     ! Bind surface meteorological fields
     call cc_model%bind_met_2d("PS"//c_null_char, c_loc(prsfc(1)))
     call cc_model%bind_met_2d("TS"//c_null_char, c_loc(ts(1)))
     call cc_model%bind_met_2d("LAT"//c_null_char, c_loc(lat(1)))
     call cc_model%bind_met_2d("LON"//c_null_char, c_loc(lon(1)))
     call cc_model%bind_met_2d("PBLH"//c_null_char, c_loc(pblh(1)))
     call cc_model%bind_met_2d("USTAR"//c_null_char, c_loc(ustar(1)))
     call cc_model%bind_met_2d("HFLUX"//c_null_char, c_loc(hf2d(1)))
     call cc_model%bind_met_2d("AREA_M2"//c_null_char, c_loc(garea(1)))

     ! 2. Direct Zero-Copy chemical tracer concentration bindings
     call cc_model%bind_unified_chemistry(c_loc(chemarr_phys(1,1,ntchs)))

     ! 3. Parallelized C++ Central core scheduled timestep execution
     call cc_model%run_timestep(real(dt, fp), errflg)
     if (errflg /= CC_SUCCESS) then
         errmsg = 'Error executing scheduled processes inside modern C++ core'
         return
     end if

     ! 4. Synchronize modifications back: Since tracers are updated in-place,
     ! standard gas concentrations inside chemarr_phys match chemarr for CCPP.
     chemarr = chemarr_phys

   end subroutine ccpp_catchem_interface_run

end module ccpp_catchem_interface

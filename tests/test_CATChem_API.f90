!> \file test_CATChem_API.f90
!! \brief Modernized API integration test for the high-level CATChem_Model API.
!!
!! Tests CATChem_Model without Fortran core state facades:
!!   initialize -> bind_met_2d / bind_met_3d -> run_timestep -> finalize
program test_CATChem_API
   use iso_c_binding, only: c_double
   use CATChem_API, only: CATChem_Model
   use catchem_bridge_precision, only: fp
   implicit none

   type(CATChem_Model) :: model
   real(c_double), target, allocatable :: lat(:,:), lon(:,:), temp(:,:,:)
   real(c_double), target, allocatable :: sst(:,:), frocean(:,:), frseaice(:,:), ustar(:,:), u10m(:,:), v10m(:,:)
   real(c_double), target, allocatable :: delp(:,:,:), chem_conc(:,:,:)
   integer :: rc, g_nx, g_ny, g_nz
   integer, parameter :: nx = 4, ny = 2, nz = 5
   integer, parameter :: n_species = 22
   character(len=*), parameter :: config_file = 'CATChem_new_config.yml'
   logical :: exists

   inquire(file=config_file, exist=exists)
   if (.not. exists) then
      print *, 'FAIL: config fixture not found: ', config_file
      error stop 1
   end if

   ! 1. Initialize with host-local grid dimensions
   call model%initialize(config_file, nx, ny, nz, rc=rc)
   if (rc /= 0) then
      print *, 'FAIL: model initialize rc=', rc
      error stop 1
   end if
   if (model%nx /= nx .or. model%ny /= ny .or. model%nz /= nz) then
      print *, 'FAIL: model dims not host-supplied:', model%nx, model%ny, model%nz
      error stop 1
   end if
   if (model%get_num_processes() /= 1) then
      print *, 'FAIL: configured process count not owned by C++ Core:', model%get_num_processes()
      error stop 1
   end if
   if (.not. model%is_process_active('seasalt')) then
      print *, 'FAIL: process seasalt not active in config'
      error stop 1
   end if
   print *, 'PASS: initialize with host-local grid dimensions'

   ! 2. Grid dimensions check
   call model%get_grid_dimensions(g_nx, g_ny, g_nz)
   if (g_nx /= nx .or. g_ny /= ny .or. g_nz /= nz) then
      print *, 'FAIL: get_grid_dimensions returned unexpected dims:', g_nx, g_ny, g_nz
      error stop 1
   end if
   print *, 'PASS: grid dimensions query'

   ! 3. Bind 2D and 3D meteorology arrays directly
   allocate(lat(nx, ny))
   allocate(lon(nx, ny))
   allocate(sst(nx, ny))
   allocate(frocean(nx, ny))
   allocate(frseaice(nx, ny))
   allocate(ustar(nx, ny))
   allocate(u10m(nx, ny))
   allocate(v10m(nx, ny))
   allocate(temp(nx*ny, 1, nz))
   allocate(delp(nx*ny, 1, nz))
   allocate(chem_conc(nx*ny, nz, n_species))

   lat = 40.0_c_double
   lon = 250.0_c_double
   sst = 290.0_c_double
   frocean = 1.0_c_double
   frseaice = 0.0_c_double
   ustar = 0.5_c_double
   u10m = 5.0_c_double
   v10m = 2.0_c_double
   temp = 290.0_c_double
   delp = 1000.0_c_double
   chem_conc = 0.0_c_double

   call model%bind_met_2d('LAT', lat)
   call model%bind_met_2d('LON', lon)
   call model%bind_met_2d('SST', sst)
   call model%bind_met_2d('FROCEAN', frocean)
   call model%bind_met_2d('FRSEAICE', frseaice)
   call model%bind_met_2d('USTAR', ustar)
   call model%bind_met_2d('U10M', u10m)
   call model%bind_met_2d('V10M', v10m)
   call model%bind_met_3d('T', temp)
   call model%bind_met_3d('DELP', delp)
   call model%bind_unified_chemistry(chem_conc)
   print *, 'PASS: met arrays bound directly to C++ core'

   ! 4. A timestep runs
   call model%run_timestep(1, 300.0_fp, rc)
   if (rc /= 0) then
      print *, 'FAIL: run_timestep rc=', rc
      error stop 1
   end if
   print *, 'PASS: run_timestep'

   ! 5. Finalize
   call model%finalize(rc)
   if (rc /= 0) then
      print *, 'FAIL: finalize rc=', rc
      error stop 1
   end if
   print *, 'PASS: finalize'

   deallocate(lat, lon, sst, frocean, frseaice, ustar, temp, delp, chem_conc)

   print *, 'All CATChem_API init-sequence tests passed!'
end program test_CATChem_API

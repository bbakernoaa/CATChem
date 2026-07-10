!> \file ccpp_catchem_interface.F90
!! \brief CCPP interface module for CATChem integration with dynamic constituents
!!
module ccpp_catchem_interface

  use iso_c_binding,             only: c_loc, c_null_char, c_double, c_ptr, c_associated, c_int, c_char
  use machine,                   only: kind_phys
  use CATChem_API,               only: CATChem_Model
  use Error_Mod,                 only: CC_SUCCESS, CC_FAILURE
  use ccpp_constituent_prop_mod, only: ccpp_constituent_properties_t, ccpp_constituent_prop_ptr_t
  use precision_mod,             only: fp

  implicit none

  private

  public :: ccpp_catchem_interface_register, &
            ccpp_catchem_interface_init, &
            ccpp_catchem_interface_run, &
            ccpp_catchem_interface_finalize

  ! Module-scope wrapper mapping directly to the C++ core
  type(CATChem_Model), save :: cc_model

  ! Saved dynamic index mapper array to store host index for each species
  integer, allocatable, save :: catchem_indices_constituent_props(:)

  ! Linkable BIND(C) external declarations to avoid circular dependencies
  interface
     integer(c_int) function catchem_state_get_species_count(state_ptr) bind(C, name="catchem_state_get_species_count")
        import :: c_ptr, c_int
        type(c_ptr), value :: state_ptr
     end function

     subroutine catchem_state_get_species_name_at(state_ptr, index, name_out) bind(C, name="catchem_state_get_species_name_at")
        import :: c_ptr, c_int, c_char
        type(c_ptr), value :: state_ptr
        integer(c_int), value :: index
        character(kind=c_char), intent(out) :: name_out(*)
     end subroutine

     real(c_double) function catchem_state_get_species_mw(state_ptr, index) bind(C, name="catchem_state_get_species_mw")
        import :: c_ptr, c_int, c_double
        type(c_ptr), value :: state_ptr
        integer(c_int), value :: index
     end function

     integer(c_int) function catchem_state_get_species_is_advected(state_ptr, index) bind(C, name="catchem_state_get_species_is_advected")
        import :: c_ptr, c_int
        type(c_ptr), value :: state_ptr
        integer(c_int), value :: index
     end function

     subroutine catchem_state_bind_met_3d(state_ptr, name, ptr) bind(C, name="catchem_state_bind_met_3d")
        import :: c_ptr, c_char
        type(c_ptr), value :: state_ptr
        character(kind=c_char), intent(in) :: name(*)
        type(c_ptr), value :: ptr
     end subroutine

     subroutine catchem_state_bind_met_2d(state_ptr, name, ptr) bind(C, name="catchem_state_bind_met_2d")
        import :: c_ptr, c_char
        type(c_ptr), value :: state_ptr
        character(kind=c_char), intent(in) :: name(*)
        type(c_ptr), value :: ptr
     end subroutine

     subroutine catchem_state_bind_unified_chemistry(state_ptr, ptr) bind(C, name="catchem_state_bind_unified_chemistry")
        import :: c_ptr
        type(c_ptr), value :: state_ptr
        type(c_ptr), value :: ptr
     end subroutine
  end interface

contains

   ! Helper to convert null-terminated C buffers back to Fortran fixed strings
   subroutine c_to_f_string(c_str, f_str)
      character(kind=c_char), intent(in) :: c_str(*)
      character(len=*), intent(out) :: f_str
      integer :: i
      f_str = ""
      do i = 1, len(f_str)
         if (c_str(i) == c_null_char) exit
         f_str(i:i) = c_str(i)
      end do
   end subroutine c_to_f_string

   !> \brief Register dynamic constituent properties with CCPP
   subroutine ccpp_catchem_interface_register(constituent_props, errmsg, errflg)
      implicit none

      type(ccpp_constituent_properties_t), allocatable, intent(out) :: constituent_props(:)
      character(len=*),                                 intent(out) :: errmsg
      integer,                                          intent(out) :: errflg

      ! Local variables
      character(len=512)     :: config_path
      character(kind=c_char) :: c_buf(128)
      character(len=64)      :: f_name
      real(kind_phys)        :: molar_mass_g_mol, molar_mass_kg_mol
      logical                :: is_advected
      integer                :: n_spec, i
      type(c_ptr)            :: state_mgr

      errmsg = ''
      errflg = 0

      ! 1. Locate YAML configuration path via environment variable
      call get_environment_variable("CATCHEM_CONFIG", config_path)
      if (trim(config_path) == "") then
         config_path = "./tests/CATChem_config.yml"
      end if

      ! 2. Lightweight core initialization to load species configuration (spatial bounds are dummy 1, 1, 1 during register)
      call cc_model%initialize(config_path, 1, 1, 1, rc=errflg)
      if (errflg /= CC_SUCCESS) then
         errmsg = 'CATChem Register: Failed to load species configuration.'
         return
      end if

      state_mgr = cc_model%get_state_manager()
      n_spec = int(catchem_state_get_species_count(state_mgr))

      ! 3. Allocate and populate constituent_props
      allocate(constituent_props(n_spec), stat=errflg)
      if (errflg /= 0) then
         errmsg = 'CATChem Register: Memory allocation failure.'
         return
      end if

      do i = 1, n_spec
         call catchem_state_get_species_name_at(state_mgr, int(i, c_int), c_buf)
         call c_to_f_string(c_buf, f_name)

         molar_mass_g_mol = real(catchem_state_get_species_mw(state_mgr, int(i, c_int)), kind_phys)
         molar_mass_kg_mol = molar_mass_g_mol * 1.0e-3_kind_phys
         is_advected = (catchem_state_get_species_is_advected(state_mgr, int(i, c_int)) == 1)

         call constituent_props(i)%instantiate( &
            std_name      = trim(f_name), &
            long_name     = trim(f_name), &
            diag_name     = trim(f_name), &
            units         = 'kg kg-1', &
            vertical_dim  = 'vertical_layer_dimension', &
            default_value = 0.0_kind_phys, &
            min_value     = 0.0_kind_phys, &
            molar_mass    = molar_mass_kg_mol, &
            advected      = is_advected, &
            errcode       = errflg, &
            errmsg        = errmsg )
         if (errflg /= 0) return
      end do

      ! 4. Clean up lightweight setup so model can be fully initialized with real grid sizes during init phase
      call cc_model%finalize(errflg)

   end subroutine ccpp_catchem_interface_register

   !> \brief Initialize the CATChem CCPP interface
   subroutine ccpp_catchem_interface_init(im, kte, nsoil, nlndcat, nsoilcat, &
                                          do_catchem, catchem_configfile_in, &
                                          constituent_props_ptr, errmsg, errflg)
      use ccpp_const_utils, only: ccpp_const_get_idx
      implicit none

      integer,                           intent(in)  :: im
      integer,                           intent(in)  :: kte
      integer,                           intent(in)  :: nsoil
      integer,                           intent(in)  :: nlndcat
      integer,                           intent(in)  :: nsoilcat
      logical,                           intent(in)  :: do_catchem
      character(len=*),                  intent(in)  :: catchem_configfile_in
      type(ccpp_constituent_prop_ptr_t), intent(in)  :: constituent_props_ptr(:)
      character(len=*),                  intent(out) :: errmsg
      integer,                           intent(out) :: errflg

      character(kind=c_char) :: c_buf(128)
      character(len=64)      :: f_name
      integer                :: n_spec, i
      type(c_ptr)            :: state_mgr

      errmsg = ''
      errflg = 0

      if (.not. do_catchem) return

      ! 1. Fully initialize C++ Core manager dynamically with host grid and soil bounds
      call cc_model%initialize(catchem_configfile_in, im, 1, kte, &
                               nsoil=nsoil, nsoiltype=nsoilcat, nsurftype=nlndcat, &
                               rc=errflg)
      if (errflg /= CC_SUCCESS) then
         errmsg = 'CATChem Init: Failed to initialize C++ Core via cc_model'
         return
      end if

      state_mgr = cc_model%get_state_manager()
      n_spec = int(catchem_state_get_species_count(state_mgr))

      ! 2. Resolve index mappings from CCPP global constituent properties pointer
      if (allocated(catchem_indices_constituent_props)) deallocate(catchem_indices_constituent_props)
      allocate(catchem_indices_constituent_props(n_spec))

      do i = 1, n_spec
         call catchem_state_get_species_name_at(state_mgr, int(i, c_int), c_buf)
         call c_to_f_string(c_buf, f_name)

         call ccpp_const_get_idx(constituent_props_ptr, trim(f_name), &
                                 catchem_indices_constituent_props(i), errmsg, errflg)
         if (errflg /= 0) then
            errmsg = "CATChem Init: Missing required tracer: " // trim(f_name)
            return
         end if
      end do

   end subroutine ccpp_catchem_interface_init

  !> \brief Finalize the CATChem CCPP interface
  subroutine ccpp_catchem_interface_finalize(do_catchem, errmsg, errflg)
     implicit none

     logical, intent(in) :: do_catchem

     character(len=*), intent(out) :: errmsg
     integer,          intent(out) :: errflg

     errmsg = ''
     errflg = 0

     if (.not. do_catchem) return

     call cc_model%finalize(errflg)
     if (errflg /= CC_SUCCESS) then
         errmsg = 'CATChem Finalize: Error finalising C++ Core via cc_model'
     end if

     if (allocated(catchem_indices_constituent_props)) deallocate(catchem_indices_constituent_props)

  end subroutine ccpp_catchem_interface_finalize

  !> \brief Execute CATChem chemistry calculations with dynamic tracer support
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
     clayfrac, rdrag, sandfrac, ssm, ustar_threshold, &
     constituent_props, constituents, &
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
     real(kind_phys), dimension(im, snowdepth) :: snowdepth_not_used ! just matching names
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
     real(kind_phys), dimension(im), intent(in), target        :: clayfrac
     real(kind_phys), dimension(im), intent(in), target        :: rdrag
     real(kind_phys), dimension(im), intent(in), target        :: sandfrac
     real(kind_phys), dimension(im), intent(in), target        :: ssm
     real(kind_phys), dimension(im), intent(in), target        :: ustar_threshold

     type(ccpp_constituent_prop_ptr_t), intent(in)    :: constituent_props(:)
     real(kind_phys), target,           intent(inout) :: constituents(:,:,:)

     character(len=*), intent(out) :: errmsg
     integer,          intent(out) :: errflg

     ! Local variables
     real(kind_phys), target, allocatable :: subset_constituents(:,:,:)
     integer :: n_spec, i
     type(c_ptr) :: state_mgr

     errmsg = ''
     errflg = 0

     if (.not. do_catchem) return

     ! 1. Extract non-contiguous indices into a contiguous local subset array
     n_spec = size(catchem_indices_constituent_props)
     allocate(subset_constituents(im, kte, n_spec))

     do i = 1, n_spec
        subset_constituents(:,:,i) = constituents(:,:,catchem_indices_constituent_props(i))
     end do

     ! 2. Meterological pointer mapping (Direct Flat C-API bindings avoiding shape restrictions)
     state_mgr = cc_model%get_state_manager()

     ! 3D volumetric met fields
     call catchem_state_bind_met_3d(state_mgr, "T"//c_null_char, c_loc(tk3d(1,1)))
     call catchem_state_bind_met_3d(state_mgr, "QV"//c_null_char, c_loc(q3d(1,1)))
     call catchem_state_bind_met_3d(state_mgr, "RH"//c_null_char, c_loc(rh(1,1)))
     call catchem_state_bind_met_3d(state_mgr, "PMID"//c_null_char, c_loc(prl3d(1,1)))
     call catchem_state_bind_met_3d(state_mgr, "PEDGE"//c_null_char, c_loc(pr3d(1,1)))
     call catchem_state_bind_met_3d(state_mgr, "DELP"//c_null_char, c_loc(delp(1,1)))
     call catchem_state_bind_met_3d(state_mgr, "AIRDEN"//c_null_char, c_loc(airden(1,1)))
     call catchem_state_bind_met_3d(state_mgr, "AIRDEN_DRY"//c_null_char, c_loc(airden(1,1)))
     call catchem_state_bind_met_3d(state_mgr, "PFILSAN"//c_null_char, c_loc(pfl_isan(1,1)))
     call catchem_state_bind_met_3d(state_mgr, "PFLLSAN"//c_null_char, c_loc(pfl_lsan(1,1)))
     call catchem_state_bind_met_3d(state_mgr, "soil_moisture"//c_null_char, c_loc(soilmoist(1,1)))

     ! 2D surface met fields
     call catchem_state_bind_met_2d(state_mgr, "PS"//c_null_char, c_loc(prsfc(1)))
     call catchem_state_bind_met_2d(state_mgr, "TS"//c_null_char, c_loc(ts(1)))
     call catchem_state_bind_met_2d(state_mgr, "LAT"//c_null_char, c_loc(lat(1)))
     call catchem_state_bind_met_2d(state_mgr, "LON"//c_null_char, c_loc(lon(1)))
     call catchem_state_bind_met_2d(state_mgr, "PBLH"//c_null_char, c_loc(pblh(1)))
     call catchem_state_bind_met_2d(state_mgr, "USTAR"//c_null_char, c_loc(ustar(1)))
     call catchem_state_bind_met_2d(state_mgr, "HFLUX"//c_null_char, c_loc(hf2d(1)))
     call catchem_state_bind_met_2d(state_mgr, "AREA_M2"//c_null_char, c_loc(garea(1)))

     ! Auxiliary physics surface variables
     call catchem_state_bind_met_2d(state_mgr, "u_10m"//c_null_char, c_loc(u10m(1)))
     call catchem_state_bind_met_2d(state_mgr, "v_10m"//c_null_char, c_loc(v10m(1)))
     call catchem_state_bind_met_2d(state_mgr, "skin_temperature"//c_null_char, c_loc(tskin(1)))
     call catchem_state_bind_met_2d(state_mgr, "roughness_length"//c_null_char, c_loc(znt(1)))
     call catchem_state_bind_met_2d(state_mgr, "vegetation_fraction"//c_null_char, c_loc(gvf(1)))
     call catchem_state_bind_met_2d(state_mgr, "leaf_area_index"//c_null_char, c_loc(lai(1)))
     call catchem_state_bind_met_2d(state_mgr, "lake_fraction"//c_null_char, c_loc(lakefrac(1)))
     call catchem_state_bind_met_2d(state_mgr, "snow_fraction"//c_null_char, c_loc(frsnow(1)))

     ! Dynamic climatological dust variables mapped individually
     call catchem_state_bind_met_2d(state_mgr, "clay_fraction"//c_null_char, c_loc(clayfrac(1)))
     call catchem_state_bind_met_2d(state_mgr, "drag_coefficient"//c_null_char, c_loc(rdrag(1)))
     call catchem_state_bind_met_2d(state_mgr, "sand_fraction"//c_null_char, c_loc(sandfrac(1)))
     call catchem_state_bind_met_2d(state_mgr, "surface_soil_moisture"//c_null_char, c_loc(ssm(1)))
     call catchem_state_bind_met_2d(state_mgr, "threshold_friction_velocity"//c_null_char, c_loc(ustar_threshold(1)))

     ! 3. Bind local unified chemistry concentrations subset
     call catchem_state_bind_unified_chemistry(state_mgr, c_loc(subset_constituents(1,1,1)))

     ! 4. Central C++ Core timestep solver execution
     call cc_model%run_timestep(real(dt, fp), errflg)
     if (errflg /= CC_SUCCESS) then
         errmsg = 'CATChem Run Error: Scheduled process execution failed inside C++ Core.'
         deallocate(subset_constituents)
         return
     end if

     ! 5. Dynamic synchronisation back to the global host tracer array
     do i = 1, n_spec
        constituents(:,:,catchem_indices_constituent_props(i)) = subset_constituents(:,:,i)
     end do

     deallocate(subset_constituents)

   end subroutine ccpp_catchem_interface_run

end module ccpp_catchem_interface

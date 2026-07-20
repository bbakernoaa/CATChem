!> \file ccpp_catchem_interface.F90
!! \brief CCPP interface module for CATChem integration
!!
!! \defgroup catchem_ccpp_group CATChem CCPP Interface
!! \brief CCPP interface drivers and utilities for CATChem
!! \ingroup catchem
!!
!! This group contains all CCPP-compliant interface modules and utilities
!! for integrating CATChem with the Common Community Physics Package (CCPP)
!! framework. Includes initialization, run, and finalization routines.
!!
!! \details
!! This is the CCPP-Compliant wrapper for interfacing CATCHEM chemistry model with CCPP
!! framework. Handles data transformation and management between host model and
!! CATCHEM chemistry calculations. This module contains the init run and finalize functions and
!! subroutines that facilitate the data exchange and coordinate transformations
!! required for CATCHEM integration within CCPP-compliant models.
!!
!! \author Barry Baker and Wei Li, NOAA/OAR/ARL
!!
!! \date November 2024
!!
!! \note This is part of the CATCHEM-CCPP interface layer that enables
!!       chemistry calculations within the CCPP framework
!!
!> \brief Main CCPP interface module for CATChem
!!
!! This module provides the primary interface between CATChem and the
!! Common Community Physics Package (CCPP) framework. It handles initialization,
!! execution, and finalization of CATChem within CCPP-compliant models.
!!
!! \ingroup catchem_ccpp_group
module ccpp_catchem_interface

  use catchem_types, only: catchem_container_type !< CATChem container type

  !use physcons, only: g => con_g, pi => con_pi
  use machine, only: kind_phys !< Machine-dependent precision parameters
  !use catchem_config
  use CATChem !< Main CATChem module
  use catchem_wrapper_utils !< CATChem wrapper utilities

implicit none

private

!> \brief Public interface routines for CCPP integration
public :: ccpp_catchem_interface_init, ccpp_catchem_interface_run, ccpp_catchem_interface_finalize

!> \brief CATChem configuration object
!! \details Stores configuration parameters for CATChem model initialization and execution
type(ConfigType) :: Config

!> \brief Dust process state object
!! \details Contains state variables and parameters for dust emission and transport processes
type(DustStateType) :: DustState

!> \brief Sea salt process state object
!! \details Contains state variables and parameters for sea salt emission and transport processes
type(SeaSaltStateType) :: SeaSaltState

!> \brief Dry deposition process state object
!! \details Contains state variables and parameters for dry deposition processes
type(DryDepStateType) :: DryDepState

!> \brief Meteorological state object
!! \details Contains meteorological fields required for chemistry calculations
type(MetStateType) :: MetState

!> \brief Main CATChem container
!! \details Container object that holds all CATChem state variables and provides
!!          unified interface for chemistry calculations
type(catchem_container_type) :: CATChemStates

!   integer :: im    !> Number of horizontal points
!   integer :: kme   !> Number of vertical levels
!   integer :: nsoil !> Number of soil layers

contains



   !> \brief Initialize the CATChem CCPP interface
   !!
   !! This subroutine initializes the CATChem container and reads the configuration
   !! file to set up the chemistry model for CCPP integration. It allocates necessary
   !! data structures and prepares the model for chemistry calculations.
   !!
   !! \param[in] im Number of horizontal grid points
   !! \param[in] do_catchem Logical flag to enable/disable CATChem calculations
   !! \param[in] catchem_configfile_in Name of the CATChem configuration file
   !! \param[out] errmsg Error message string for CCPP error handling
   !! \param[out] errflg Error flag for CCPP error handling (0=success, /=0=error)
   !!
   !! \details
   !! This routine performs the following operations:
   !! - Checks if CATChem is enabled via the do_catchem flag
   !! - Initializes CATChem configuration from the specified config file
   !! - Allocates and initializes state objects for dust, sea salt, and dry deposition
   !! - Sets up the main CATChem container for the specified grid dimensions
   !!
   !! \note This routine must be called before any CATChem run routines
   !!
   !! \ingroup catchem_ccpp_group
   subroutine ccpp_catchem_interface_init(im, do_catchem, catchem_configfile_in,  &
                                  errmsg, errflg)
      implicit none

      ! Input parameters
      character(len=*), intent(in) :: catchem_configfile_in !< Configuration file path
      logical,          intent(in) :: do_catchem            !< Enable CATChem flag
      integer,          intent(in) :: im                    !< Horizontal dimension

      ! Output parameters
      character(len=*), intent(out) :: errmsg !< CCPP error message
      integer,          intent(out) :: errflg !< CCPP error flag

      ! Local variables
      CHARACTER(LEN=255) :: ThisLoc !< Location identifier for error reporting

      errmsg = ''
      errflg = 0
      ThisLoc = ' -> at catchem_interface_init (in drivers/ccpp/ccpp_catchem_inerface.F90)'

      if (.not. do_catchem) return

      ! Set global parameters
      !export_catchem_diags = export_catchem_diags_in
      !n_dbg_lines = n_dbg_lines_in


      call catchem_init(Config, CATChemStates, im, catchem_configfile_in, errflg, errmsg, &
      DustState, SeaSaltState, DryDepState)
      if (errflg /= CC_SUCCESS) then
         ErrMsg = 'Error in catchem_init'
         call cc_emit_error(ErrMsg, errflg, ThisLoc ) !TODO: consider rename the subroutine
         return
     end if


   end subroutine ccpp_catchem_interface_init

  !> \brief Finalize the CATChem CCPP interface
  !!
  !! This subroutine finalizes the CATChem container and cleans up all allocated
  !! memory and resources used by the chemistry model.
  !!
  !! \param[in] do_catchem Logical flag indicating if CATChem is enabled
  !! \param[out] errmsg Error message string for CCPP error handling
  !! \param[out] errflg Error flag for CCPP error handling (0=success, /=0=error)
  !!
  !! \details
  !! This routine performs cleanup operations including:
  !! - Deallocating state objects for dust, sea salt, and dry deposition processes
  !! - Cleaning up the main CATChem container
  !! - Releasing any allocated memory resources
  !!
  !! \note This routine should be called at the end of model execution to prevent
  !!       memory leaks and ensure proper cleanup
  !!
  !! \ingroup catchem_ccpp_group
  subroutine ccpp_catchem_interface_finalize(do_catchem, errmsg, errflg)

   implicit none

   ! Input parameters
   logical, intent(in) :: do_catchem !< Enable CATChem flag

   ! Output parameters
   character(len=*), intent(out) :: errmsg !< CCPP error message
   integer, intent(out) :: errflg          !< CCPP error flag

   ! Local variables
   CHARACTER(LEN=255) :: ThisLoc !< Location identifier for error reporting

   errmsg = ''
   errflg = 0
   ThisLoc = ' -> at catchem_interface_finalize (in drivers/ccpp/ccpp_catchem_inerface.F90)'

   if (.not. do_catchem) return

   call catchem_finalize (CATChemStates, DustState, SeaSaltState, DryDepState, errflg)
   if (errflg /= CC_SUCCESS) then
         ErrMsg = 'Error in catchem_finalize'
         call cc_emit_error(ErrMsg, errflg, ThisLoc )
         return
   end if

  end subroutine ccpp_catchem_interface_finalize

  !> \brief Execute CATChem chemistry calculations within CCPP framework
  !!
  !! This subroutine performs the main CATChem chemistry calculations including
  !! dust emission, sea salt emission, dry deposition, and other atmospheric
  !! chemistry processes for a single time step.
  !!
  !! \param[in] im Horizontal loop extent (number of grid points)
  !! \param[in] kte Vertical layer dimension (number of vertical levels)
  !! \param[in] kme Vertical interface dimension (number of levels + 1)
  !! \param[in] garea Grid cell area for each horizontal point (m²)
  !! \param[in] nsoil Number of soil layers
  !! \param[in] nlndcat Number of land surface vegetation categories
  !! \param[in] nsoilcat Number of soil type categories
  !! \param[in] lat Latitude of grid points (degrees North)
  !! \param[in] lon Longitude of grid points (degrees East)
  !! \param[in] do_catchem Logical flag to enable CATChem calculations
  !! \param[in] dt Physics time step (seconds)
  !! \param[in] jdate Current forecast date and time array
  !!
  !! \details
  !! This routine orchestrates the execution of various atmospheric chemistry
  !! processes including:
  !! - Dust emission and transport calculations
  !! - Sea salt emission and transport
  !! - Dry deposition processes
  !! - Chemical species concentration updates
  !! - Diagnostic output generation
  !!
  !! The routine interfaces with the host model's meteorological fields,
  !! land surface properties, and tracer arrays to perform comprehensive
  !! atmospheric chemistry calculations.
  !!
  !! \note This routine should be called once per physics time step
  !!
  !! \ingroup catchem_ccpp_group
  subroutine ccpp_catchem_interface_run(im, kte, kme, garea, nsoil, nlndcat, nsoilcat, &
     lat, lon, & ! Grid information
     do_catchem, & ! CATChem Flag on
     dt, jdate, & ! Time information
     xcosz, &
     lwi, frlanduse, gvf, seaicefrac, oceanfrac, lakefrac, landfrac, & ! land water specific variables
     stype, vtype, snowdepth, frsnow, lai, frsoil, pores, resid, & ! land surface variables
     ustar, u10m, v10m, tskin, ts, hf2d, lf2d, znt, prsfc, pblh, & !surface variables
     dswsfc, nirbmdi, nirdfdi, visbmdi, visdfdi, &  ! Radiation Fluxes
     sfc_alb_nir_dir, sfc_alb_nir_dif, sfc_alb_uvvis_dir, sfc_alb_uvvis_dif, & ! surface albedo
     soilmoist, pr3d, phl3d, prl3d, tk3d, q3d, us3d, vs3d, rh, & ! 3D Variables
     delp, airden, pfl_lsan, pfl_isan, & ! 3D Variables
     rain_cpl, cldf, & ! cloud variables
     dust_in, & ! Emissions
     ntrac, ntchs, ntchm, chemarr_phys, chemarr, & ! Chemistry Variables
     errmsg, errflg)

     implicit none
     ! ARGUMENTS
     !----------

     ! CCPP Interface Variables
     !-------------------------

     ! Grid information
     integer, intent(in) :: im       ! horizontal loop extent
     integer, intent(in) :: kte      ! vertical layer dimension
     integer, intent(in) :: kme      ! vertical interface dimension = number of vertical layers + 1
     !integer, intent(in) :: ktau     ! current forecast iteration
     !integer, intent(in) :: tile_num ! index of cube sphere tile
     integer, intent(in) :: nsoil    !> vertical_dimension_of_soil
     integer, intent(in) :: nlndcat !> number_of_vegetation_categories
     integer, intent(in) :: nsoilcat !> number of soil categories
     real(kind_phys), dimension(im), intent(in) :: garea !> grid area (m^2) of each grid cell
     !real(kind_phys), dimension(im), intent(in) :: rlat  !> radian latitude
     !real(kind_phys), dimension(im), intent(in) :: rlon  !> radian longitude
     real(kind_phys), dimension(im), intent(in) :: lat   !> latitude
     real(kind_phys), dimension(im), intent(in) :: lon   !> longitude
     real(kind_phys), dimension(im), intent(in) :: xcosz !> cosine of solar zenith angle

     ! Time information
     !integer, intent(in) :: idat(8)  ! initialization date and time (in iso order)
     integer, intent(in) :: jdate(8)    !> julian date
     real(kind_phys), intent(in) :: dt      ! physics timestep (s)
     !real(kind_phys), intent(in) :: julian  ! forecast julian day (days)

     ! MPI information
     !type(MPI_comm), intent(in) :: mpicomm
     !integer, intent(in) :: mpirank
     !integer, intent(in) :: mpiroot

     ! Logicals
     logical, intent(in) :: do_catchem

     ! tracer information
     integer, intent(in) :: ntrac    ! total number of tracers
     integer, intent(in) :: ntchs      ! index of first chemical tracer in tracer concentration array
     integer, intent(in) :: ntchm      ! number of chemical tracers
     !integer, intent(in) :: ntaero      ! number of aerosol tracers
     real(kind_phys), dimension(im, kte, ntrac), intent(inout) :: chemarr_phys
     real(kind_phys), dimension(im, kte, ntrac), intent(inout) :: chemarr

     ! Emissions
     !real(kind_phys), dimension(im, kte, 3), intent(in) :: emi_in ! 3 should be temporary... need to replace with either an input namelist option or have it be a pointer with variable dimension
     !real(kind_phys), dimension(im, kte, ntrac), intent(in) :: kemit_in !> emission inputs
     real(kind_phys), dimension(im, 12, 5), intent(in) :: dust_in         !> dust emission inputs
     !real(kind_phys), dimension(im), intent(in) :: pert_scale_anthro !> anthropogenic emission perturbation scale factor
     !real(kind_phys), dimension(im), intent(in) :: pert_scale_plume !> plume emission perturbation scale factor
     !real(kind_phys), dimension(im), intent(in) :: pert_scale_dust !> dust emission perturbation scale factor
     !real(kind_phys), dimension(im), intent(in) :: emis_amp_anthro !> anthropogenic emission amplitude
     !real(kind_phys), dimension(im), intent(in) :: emis_amp_plume !> plume emission amplitude
     !real(kind_phys), dimension(im), intent(in) :: emis_amp_seas !> seasonal emission amplitude
     !real(kind_phys), dimension(im), intent(in) :: emis_amp_dust !> dust emission amplitude
     !integer(kind_phys), intent(in) :: biomass_burn_opt_in !> biomass burning option
     !integer(kind_phys), intent(in) :: plumerise_flag_in !> plume rise flag
     !integer(kind_phys), intent(in) :: plumerisefire_freq_in !> plume rise fire frequency
     !real(kind_phys), dimension(im, kte), intent(inout) :: drydep !> dry deposition
     !real(kind_phys), dimension(im, kte), intent(inout) :: wetdpl !> wet deposition
     !real(kind_phys), dimension(im, kte), intent(inout) :: abem !> aerosol backscatter extinction mass
     !real(kind_phys), dimension(im, kte), intent(in) :: ca_emis_plume !> plume emission
     !logical, dimension(im, kte), intent(in) :: ca_sgs_emis !> SGS emission
     !logical, dimension(im, kte), intent(in) :: ca_sgs !> SGS
     !real(kind_phys), dimension(im, kte), intent(in) :: ca_sgs_gbbepx_frp !> SGS GBBEPx FRP
     !real(kind_phys), dimension(im, kte), intent(in) :: fire_GBBEPx !> GBBEPx fire emissions
     !real(kind_phys), dimension(im, kte), intent(in) :: fire_MODIS !> MODIS fire emissions
     !real(kind_phys), dimension(im, kte), intent(in) :: ebu !> EBU
     !real(kind_phys), dimension(im, kte), intent(in) :: sppt_wts !> SPPT weights
     !logical, intent(in) :: do_sppt_emis !> SPPT emission flag

     ! land surface information
     integer, dimension(im), intent(in)                :: lwi         !> sea land ice mask (sea = 0, land = 1, ice = 2)
     integer, dimension(im), intent(in)                :: stype       !> soil type
     integer, dimension(im), intent(in)                :: vtype       !> vegatation type

     real(kind_phys), dimension(im, nlndcat), intent(in) :: frlanduse     !> fraction of each land surface category
     real(kind_phys), dimension(im, nsoilcat), intent(in) :: frsoil       !> fraction of each soil type
     real(kind_phys), dimension(30), intent(in)        :: pores           !> porosity of each soil type
     real(kind_phys), dimension(30), intent(in)        :: resid           !> residual water content of each soil type
     real(kind_phys), dimension(im), intent(in)        :: seaicefrac      !> fractin of ice cover over ocean
     !real(kind_phys), dimension(im), intent(in)        :: landicefrac     !> fractin of ice cover over land
     real(kind_phys), dimension(im), intent(in)        :: oceanfrac       !> fraction of ocean cover
     real(kind_phys), dimension(im), intent(in)        :: frsnow        !> fraction of snow cover over land
     real(kind_phys), dimension(im), intent(in)        :: lakefrac        !> fraction of lake cover
     real(kind_phys), dimension(im), intent(in)        :: landfrac        !> fraction of land cover
     real(kind_phys), dimension(im), intent(in)        :: gvf             !> green vegetative fraction
     real(kind_phys), dimension(im), intent(in)        :: lai             !> leaf area index

     real(kind_phys), dimension(im, nsoil), intent(in) :: soilmoist     !> volumetric fraction of soil moisture for lsm
     !real(kind_phys), dimension(im, nsoil), intent(in) :: soiltemp      !> soil temperature (K)
     real(kind_phys), dimension(im), intent(in)        :: snowdepth     !> water equivalent snow depth (mm)
     real(kind_phys), dimension(im), intent(in)        :: prsfc          !> pressure at the surface (Pa)
     !real(kind_phys), dimension(im), intent(in)        :: prslp         !> sea level pressure (Pa)
     real(kind_phys), dimension(im), intent(in)        :: pblh          !> PBL Thickness determined by the PBL scheme (m)
     !real(kind_phys), dimension(im), intent(in)        :: kpbl          !> PBL level
     !real(kind_phys), dimension(im), intent(in)        :: hpbl_thetav   !> PBL Height based on modified parcel method (m)
     real(kind_phys), dimension(im), intent(in)        :: u10m          !> 10 m wind speed (m/s)
     real(kind_phys), dimension(im), intent(in)        :: v10m          !> 10 m wind speed (m/s)
     real(kind_phys), dimension(im), intent(in)        :: ustar         !> friction velocity (m/s)
     !real(kind_phys), dimension(im), intent(in)        :: psim          !> Monin-Obukhov similarity parameter for momentum at 10m
     !real(kind_phys), dimension(im), intent(in)        :: psih          !> Monin-Obukhov similarity parameter for heat at 10m
     real(kind_phys), dimension(im), intent(in)        :: tskin         !> skin temperature (K)
     !real(kind_phys), dimension(im), intent(in)        :: t2m           !> 2 m temperature (K)
     real(kind_phys), dimension(im), intent(in)        :: ts            !> surface temperature (K)
     !real(kind_phys), dimension(im), intent(in)        :: dpt2m         !> 2 m dew point temperature (K)
     real(kind_phys), dimension(im), intent(in)        :: hf2d          !> Sensible heat flux (W m-2)
     real(kind_phys), dimension(im), intent(in)        :: lf2d          !> Latent heat flux (W m-2)
     real(kind_phys), dimension(im), intent(in)        :: znt           !> surface roughness length in (cm)
     real(kind_phys), dimension(im), intent(in)        :: dswsfc        !> downward short wave flux (W m-2)
     !real(kind_phys), dimension(im), intent(in)        :: recmol        !> one over obukhov length (m-1)
     real(kind_phys), dimension(im), intent(in)        :: sfc_alb_nir_dir    !> surface near-infrared direct albedo
     real(kind_phys), dimension(im), intent(in)        :: sfc_alb_nir_dif    !> surface near-infrared diffuse albedo
     real(kind_phys), dimension(im), intent(in)        :: sfc_alb_uvvis_dir    !> surface ultraviolet-visible direct albedo
     real(kind_phys), dimension(im), intent(in)        :: sfc_alb_uvvis_dif    !> surface ultraviolet-visible diffuse albedo
     real(kind_phys), dimension(im), intent(in)        :: nirbmdi      !> surface near-infrared beam shortwave radiation (W/m2)
     real(kind_phys), dimension(im), intent(in)        :: nirdfdi      !> surface near-infrared diffuse shortwave radiation (W/m2)
     real(kind_phys), dimension(im), intent(in)        :: visbmdi      !> surface visible beam shortwave radiation (W/m2)
     real(kind_phys), dimension(im), intent(in)        :: visdfdi      !> surface visible diffuse shortwave radiation (W/m2)

     real(kind_phys), dimension(im, kme), intent(in) :: pr3d            !> air pressure at model layer interfaces (Pa)
     real(kind_phys), dimension(im, kte), intent(in) :: prl3d           !> pressure at the model level (Pa)
     !real(kind_phys), dimension(im, kme), intent(in) :: pr3d_dry        !> dry air pressure at model layer interfaces (Pa)
     !real(kind_phys), dimension(im, kte), intent(in) :: prl3d_dry       !> dry air pressure at the model level (Pa)
     real(kind_phys), dimension(im, kte), intent(in) :: delp            !> air pressure thickness at the model level (Pa)
     !real(kind_phys), dimension(im, kte), intent(in) :: delp_dry        !> dry air pressure thickness at the model level (Pa)
     !real(kind_phys), dimension(im, kme), intent(in) :: ph3d            !> geopotential at the model level interfaces (m2 s-2)
     real(kind_phys), dimension(im, kte), intent(in) :: phl3d           !> geopotential at the model layer (m2 s-2)
     real(kind_phys), dimension(im, kte), intent(in) :: tk3d            !> temperature at the model level (K)
     real(kind_phys), dimension(im, kte), intent(in) :: us3d            !> zonal wind at the model level (m/s)
     real(kind_phys), dimension(im, kte), intent(in) :: vs3d            !> meridional wind at the model level (m/s)
     real(kind_phys), dimension(im, kte), intent(in) :: q3d             !> specific humidity at the model level (kg/kg)
     real(kind_phys), dimension(im, kte), intent(in) :: airden         !> dry air density (kg/m3)
     !real(kind_phys), dimension(im, kte), intent(in) :: w               !> lagrangian_tendency_of_air_pressure
     !real(kind_phys), dimension(im, kte), intent(in) :: exch            !> atmospheric heat diffusivity
     real(kind_phys), dimension(im, kte), intent(in) :: rh              !> relative humidity
     real(kind_phys), dimension(im, kte), intent(in) :: pfl_lsan   !>  liquid flux from large scale precipitation (kg/m2/s)
     real(kind_phys), dimension(im, kte), intent(in) :: pfl_isan   !>  ice flux from large scale precipitation (kg/m2/s)

     ! precipitation information
     real(kind_phys), dimension(im), intent(in)        :: rain_cpl        !> total rain at this time step (m)
     !real(kind_phys), dimension(im), intent(in)        :: rainc_cpl       !> convective rain at this time step (m)
     real(kind_phys), dimension(im), intent(in)        :: cldf            !> total cloud fraction
     !real(kind_phys), dimension(im, kte), intent(in)   :: dqdt            !> instantaneous_water_vapor_specific_humidity_tendency_due_to_convection
     !integer, intent(in)                               :: chem_conv_tr_in !> catchem convective transport option

     ! Radiation
     !integer, intent(in) :: aer_ra_feedback_in  !> catchem aer radiation feedback option
     !integer, intent(in) :: aer_ra_frq_in       !> catchem_aer_ra_frq

     ! Output
     !-------
     character(len=*), intent(out) :: errmsg
     integer, intent(out) :: errflg
     CHARACTER(LEN=255)    :: ThisLoc

     errmsg = ''
     errflg = 0
     ThisLoc = ' -> at catchem_interface_run (in drivers/ccpp/ccpp_catchem_interface.F90)'

     ! Local
     !------
     !integer :: mpiid

     if (.not. do_catchem) return

     !give tracer from CCPP to CATChem
     call ccpp_to_cc(im, kte, ntchs, ntchm, CATChemStates%ChemState, chemarr_phys)

     ! Fill MetState Arrays
     call transform_ccpp_to_catchem(im, kme, kte, nsoil, nlndcat, nsoilcat, lat, lon, &      ! Grid Information
                                        dt, jdate, garea, &  ! Grid Information
                                        lwi, &  ! Model Options
                                        tk3d, q3d, pr3d, pr3d, prl3d, rh, &  ! Meteorological Variables; TODO: not using dry air pressre and delp for now
                                        us3d, vs3d, delp, phl3d, &  ! Meteorological Variables
                                        u10m, v10m, tskin, prsfc, ts, rain_cpl, &  ! Meteorological Variables
                                        cldf, airden, delp, &
                                        pfl_lsan, pfl_isan, &  ! precipitation variables
                                        snowdepth, vtype, stype, soilmoist, &  ! Surface Variables
                                        pblh, xcosz, &  ! Surface Variables
                                        ustar, hf2d, lf2d, &  ! Near-Surface Meteorology
                                        frsnow, gvf, lai, frlanduse, frsoil, pores, resid, &  ! Surface Variables
                                        znt, landfrac, oceanfrac, lakefrac, seaicefrac, &
                                        dswsfc, nirbmdi, nirdfdi, visbmdi, visdfdi, &  ! Radiation Fluxes
                                        sfc_alb_nir_dir, sfc_alb_nir_dif, sfc_alb_uvvis_dir, sfc_alb_uvvis_dif,&  ! surface albedo
                                        dust_in, & !Emissions
                                        CATChemStates%MetState, & ! CATChem States
                                        errmsg, errflg)

     ! Run CATChem
     call catchem_run(im, CATChemStates, DustState, SeaSaltState, DryDepState, errflg)
     if (errflg /= CC_SUCCESS) then
         ErrMsg = 'Error in catchem_run'
         call cc_emit_error(ErrMsg, errflg, ThisLoc ) !TODO: consider rename the subroutine
         return
     end if

     ! give chemical tracer back to CCPP
     call cc_to_ccpp(im, kte, ntchs, ntchm, CATChemStates%ChemState, chemarr)

   end subroutine ccpp_catchem_interface_run


end module ccpp_catchem_interface

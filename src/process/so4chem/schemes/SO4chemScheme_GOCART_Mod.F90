!> \file SO4chemScheme_GOCART_Mod.F90
!! \brief GOCART SO2 to SO4 production scheme
!!
!! Pure science kernel for gocart scheme in so4chem process.
!! This module contains ONLY the computational algorithm with NO infrastructure dependencies.
!! Uses only basic Fortran types for maximum portability and reusability.
!!
!! SCIENCE CUSTOMIZATION GUIDE:
!! 1. Modify the algorithm in compute_gocart (search for "TODO")
!! 2. Add scheme-specific helper subroutines as needed
!! 3. Update physical constants for your scheme
!! 4. Customize the environmental response functions
!!
!! INFRASTRUCTURE RESPONSIBILITIES (handled by host model):
!! - Parameter initialization and validation
!! - Input array validation and error handling
!! - Memory management and array allocation
!! - Integration with host model time stepping
!!
!! Generated on: 2026-02-11T13:30:17.194715
!! Author: Wei Li
!! Reference: GOCART2G process library SulfateChemDriver function
module SO4chemScheme_GOCART_Mod

   use precision_mod, only: fp
   use SO4chemCommon_Mod, only: SO4chemSchemeGOCARTConfig
   use error_mod, only: CC_SUCCESS, CC_Error
   use SO4chemPhysics_Mod, only: so4chem_driver
   use Met_Utilities_Mod, only: solar_zenith_angle

   implicit none
   private

   ! Public interface - pure science only
   public :: compute_gocart

   ! Additional physical constants (modify as needed for your scheme)
   real(fp), parameter :: plid = 0.01_fp    ! Pressure lid [hPa]

contains

   !> Pure science computation for gocart scheme
   !!
   !! This is a pure computational kernel implementing GOCART SO2 to SO4 production scheme.
   !! NO error checking, validation, or infrastructure concerns.
   !! Host model must ensure all inputs are valid before calling.
   !!
   !! @param[in]  num_layers     Number of vertical layers
   !! @param[in]  num_species    Number of chemical species
   !! @param[in]  params         Scheme parameters (pre-validated by host)
   !! @param[in]  g0    Required constant from Constants module
   !! @param[in]  Cpd    Required constant from Constants module
   !! @param[in]  AVO    Required constant from Constants module
   !! @param[in]  VON_KARMAN    Required constant from Constants module
   !! @param[in]  AIRMW    Required constant from Constants module
   !! @param[in]  PI    Required constant from Constants module
   !! @param[in]  year    Time parameter from TimeState (year)
   !! @param[in]  month    Time parameter from TimeState (month)
   !! @param[in]  day    Time parameter from TimeState (day)
   !! @param[in]  hour    Time parameter from TimeState (hour)
   !! @param[in]  minute    Time parameter from TimeState (minute)
   !! @param[in]  second    Time parameter from TimeState (second)
   !! @param[in]  airden    AIRDEN field [appropriate units]
   !! @param[in]  cldf    CLDF field [appropriate units]
   !! @param[in]  delp    DELP field [appropriate units]
   !! @param[in]  hflux    HFLUX field [appropriate units]
   !! @param[in]  lat    LAT field [appropriate units]
   !! @param[in]  lon    LON field [appropriate units]
   !! @param[in]  lwi    LWI field [appropriate units]
   !! @param[in]  pblh    PBLH field [appropriate units]
   !! @param[in]  pmid    PMID field [appropriate units]
   !! @param[in]  t    T field [appropriate units]
   !! @param[in]  tstep    Time step [s] - retrieved from process interface
   !! @param[in]  u10m    U10M field [appropriate units]
   !! @param[in]  ustar    USTAR field [appropriate units]
   !! @param[in]  v10m    V10M field [appropriate units]
   !! @param[in]  z    Z field [appropriate units]
   !! @param[in]  z0h    Z0H field [appropriate units]
   !! @param[in]  species_mw_g    Species mw_g property
   !! @param[in]  species_short_name    Species short_name property
   !! @param[in]  species_conc   Species concentrations [ppm or ug/kg] (num_layers, num_species)
   !! @param[inout] species_tendencies  Species tendency terms [mol/mol/s] (num_layers, num_species)
   !! Persistent state variables (per-column):
   !! @param[inout] firsttime    flag for first time step
   !! @param[inout] nymd_last    last day of H2O2 update
   !! @param[inout] nhms_last_recycle    last time step of H2O2 recycle
   !! @param[inout] xh2o2_init    H2O2 column initialization
   !! @param[inout] PSO4_from_SO2_per_level    total sulfate production rate from SO2 per level [kg/kg/s] (num_layers)
   !! @param[inout] PSO4_from_gaseous_SO2_per_level    sulfate production rate from gaseous SO2 per level [kg/kg/s] (num_layers)
   !! @param[inout] PSO4_from_aqueous_SO2_per_level    sulfate production rate from aqueous SO2 per level [kg/kg/s] (num_layers)
   !! @param[inout] PSO2_from_DMS_per_level    SO2 production rate from DMS per level [kg/kg/s] (num_layers)
   !! @param[inout] DMS_emission_flux    DMS emission flux at the surface [kg/m2/s]
   !! @param[in] diagnostic_species_id Indices mapping diagnostic species to species array (optional, for per-species diagnostics)
   subroutine compute_gocart( &
      num_layers, &
      num_species, &
      params, &
      g0, &
      Cpd, &
      AVO, &
      VON_KARMAN, &
      AIRMW, &
      PI, &
      year, &
      month, &
      day, &
      hour, &
      minute, &
      second, &
      airden, &
      cldf, &
      delp, &
      hflux, &
      lat, &
      lon, &
      lwi, &
      pblh, &
      pmid, &
      t, &
      tstep, &
      u10m, &
      ustar, &
      v10m, &
      z, &
      z0h, &
      species_mw_g, &
      species_short_name, &
      species_conc, &
      species_tendencies, &
      firsttime, &
      nymd_last, &
      nhms_last_recycle, &
      xh2o2_init, &
      Production_rate_per_species_per_level, &
      PSO4_from_gaseous_SO2_per_level, &
      PSO4_from_aqueous_SO2_per_level, &
      DMS_emission_flux, &
      diagnostic_species_id &
      )

      ! Arguments
      integer, intent(in) :: num_layers
      integer, intent(in) :: num_species
      type(SO4chemSchemeGOCARTConfig), intent(in) :: params
      real(fp), intent(in) :: g0  ! Required constant from Constants module
      real(fp), intent(in) :: Cpd  ! Required constant from Constants module
      real(fp), intent(in) :: AVO  ! Required constant from Constants module
      real(fp), intent(in) :: VON_KARMAN  ! Required constant from Constants module
      real(fp), intent(in) :: AIRMW  ! Required constant from Constants module
      real(fp), intent(in) :: PI  ! Required constant from Constants module
      integer, intent(in) :: year  ! Time parameter from TimeState
      integer, intent(in) :: month  ! Time parameter from TimeState
      integer, intent(in) :: day  ! Time parameter from TimeState
      integer, intent(in) :: hour  ! Time parameter from TimeState
      integer, intent(in) :: minute  ! Time parameter from TimeState
      integer, intent(in) :: second  ! Time parameter from TimeState
      real(fp), intent(in) :: airden(num_layers)    ! 3D atmospheric field
      real(fp), intent(in) :: cldf(num_layers)  ! Surface field - scalar
      real(fp), intent(in) :: delp(num_layers)    ! 3D atmospheric field
      real(fp), intent(in) :: hflux  ! Surface field - scalar
      real(fp), intent(in) :: lat  ! Surface field - scalar
      real(fp), intent(in) :: lon  ! Surface field - scalar
      integer, intent(in) :: lwi  ! Surface field - scalar
      real(fp), intent(in) :: pblh  ! Surface field - scalar
      real(fp), intent(in) :: pmid(num_layers)    ! 3D atmospheric field
      real(fp), intent(in) :: t(num_layers)    ! 3D atmospheric field
      real(fp), intent(in) :: tstep  ! Time step [s] - from process interface
      real(fp), intent(in) :: u10m  ! Surface field - scalar
      real(fp), intent(in) :: ustar  ! Surface field - scalar
      real(fp), intent(in) :: v10m  ! Surface field - scalar
      real(fp), intent(in) :: z(num_layers+1)  ! Edge field - requires nz+1 dimensions
      real(fp), intent(in) :: z0h  ! Surface field - scalar
      real(fp), intent(in) :: species_mw_g(:)  ! Species mw_g property
      character(len=32), intent(in) :: species_short_name(:)  ! Species short_name property
      real(fp), intent(in) :: species_conc(num_layers, num_species)
      real(fp), intent(inout) :: species_tendencies(num_layers, num_species)
      ! Per-column persistent state variables
      logical, intent(inout) :: firsttime  ! flag for first time step
      integer, intent(inout) :: nymd_last  ! last day of H2O2 update
      integer, intent(inout) :: nhms_last_recycle  ! last time step of H2O2 recycle
      real(fp), intent(inout), allocatable :: xh2o2_init(:)  ! H2O2 column initialization
      real(fp), intent(inout), optional :: Production_rate_per_species_per_level(:,:)
      real(fp), intent(inout), optional :: PSO4_from_gaseous_SO2_per_level(:)
      real(fp), intent(inout), optional :: PSO4_from_aqueous_SO2_per_level(:)
      real(fp), intent(inout), optional :: DMS_emission_flux
      integer, intent(in), optional :: diagnostic_species_id(:)  ! Indices mapping diagnostic species to species array

      ! Local variables
      integer :: klid  ! pressure lid index in native ordering
      integer :: k     ! loop index
      integer :: diag_idx  ! For diagnostic species indexing
      integer :: species_idx
      integer :: nDMS= -1, nSO2= -1, nSO4= -1, nMSA= -1, nDMS_IN= -1 ! index position of sulfates
      integer :: nOH= -1, nNO3= -1, nH2O2= -1 ! index position of oxidants
      integer :: nymd, nhms   !YYYYMMDD, HHMMSS time formats
      real(fp) :: fMassMSA, fMassDMS, fMassSO2, fMassSO4 ! gram molecular weights of species
      real(fp) :: deg2rad
      real(fp) :: plid_pa  ! pressure lid in Pa
      real(fp) :: diff, refDiff  ! for inline klid lookup
      ! 1D chemistry arrays in native ordering (bottom-to-top)
      real(fp), allocatable :: oh_clim(:)    !volume mixing ratio [mol/mol]
      real(fp), allocatable :: h2o2_clim(:)  !volume mixing ratio [mol/mol]
      real(fp), allocatable :: no3_clim(:)   !volume mixing ratio [mol/mol]
      real(fp), allocatable :: xoh(:), xno3(:), xh2o2(:)
      real(fp), allocatable :: dms(:), so2(:), so4(:), msa(:)
      ! 1D production rate arrays in native ordering
      real(fp), allocatable :: pso2_dms(:)   ! SO2 Prod from DMS oxidation [kg/kg/s]
      real(fp), allocatable :: pmsa_dms(:)   ! MSA Prod from DMS oxidation [kg/kg/s]
      real(fp), allocatable :: pso4g(:)      ! SO4 Prod from gaseous SO2 oxidation [kg/kg/s]
      real(fp), allocatable :: pso4aq(:)     ! SO4 Prod from aqueous SO2 oxidation [kg/kg/s]
      logical :: recycle_h2o2
      !error information
      integer :: RC
      character(len=256) :: errMsg
      character(len=256) :: thisLoc

      ! Initialize
      errMsg = ''
      thisLoc = ' -> at compute_gocart (in SO4chemScheme_GOCART_Mod.F90)'
      RC = 0

      deg2rad = PI/180.0_fp
      !construct time in yyyymmdd and hhmmss formats for use in gocart
      nymd = year*10000 + month*100 + day
      nhms = hour*10000 + minute*100 + second

      !get species indices for use in gocart
      do species_idx = 1, num_species
         if (species_short_name(species_idx) == 'SO2' .or. species_short_name(species_idx) == 'so2') then
            nSO2 = species_idx
         else if (species_short_name(species_idx) == 'SO4' .or. species_short_name(species_idx) == 'so4') then
            nSO4 = species_idx
         else if (species_short_name(species_idx) == 'DMS' .or. species_short_name(species_idx) == 'dms') then
            nDMS = species_idx
         else if (species_short_name(species_idx) == 'DMS_IN' .or. species_short_name(species_idx) == 'dms_in') then
            nDMS_IN = species_idx
         else if (species_short_name(species_idx) == 'MSA' .or. species_short_name(species_idx) == 'msa') then
            nMSA = species_idx
         else if (species_short_name(species_idx) == 'OH' .or. species_short_name(species_idx) == 'oh') then
            nOH = species_idx
         else if (species_short_name(species_idx) == 'NO3' .or. species_short_name(species_idx) == 'no3') then
            nNO3 = species_idx
         else if (species_short_name(species_idx) == 'H2O2' .or. species_short_name(species_idx) == 'h2o2') then
            nH2O2 = species_idx
         end if
      end do

      if (nSO2 == -1 .or. nSO4 == -1 .or. nDMS == -1 .or. nMSA == -1 .or. nOH == -1 .or. nNO3 == -1 .or. nH2O2 == -1) then
         errMsg = 'Error in compute_gocart: SO2, SO4, DMS, MSA, OH, NO3, and H2O2 must be present in species list.'
         write(*,'(A)') trim(errMsg)
         return
      end if

      ! Allocate 1D arrays in native ordering
      allocate(oh_clim(num_layers), h2o2_clim(num_layers), no3_clim(num_layers), &
         xoh(num_layers), xno3(num_layers), xh2o2(num_layers), &
         dms(num_layers), so2(num_layers), so4(num_layers), msa(num_layers), &
         pso2_dms(num_layers), pmsa_dms(num_layers), pso4g(num_layers), pso4aq(num_layers))

      ! Retrieve climatology fields in native ordering (no reversal)
      oh_clim(:) = species_conc(:, nOH) * 1.0e-6_fp    !change from ppm to mol/mol
      no3_clim(:) = species_conc(:, nNO3) * 1.0e-6_fp   !change from ppm to mol/mol
      h2o2_clim(:) = species_conc(:, nH2O2) * 1.0e-6_fp !change from ppm to mol/mol

      ! Initialize some variables for the first time
      if (firsttime) then
         nymd_last = nymd
         nhms_last_recycle = nhms
         if (.not. allocated(xh2o2_init)) then
            allocate(xh2o2_init(num_layers))
         end if
         xh2o2_init = h2o2_clim(:)  ! initialize H2O2 to climatology at first time step
         firsttime = .false.
      end if

      ! Update nymd_last
      if (nymd /= nymd_last) then
         nymd_last = nymd
         nhms_last_recycle = nhms  !reset recycle timer when day changes
      end if

      recycle_h2o2 = .false.
      !check if first time step or 3 hours have passed since last H2O2 recycle using actual time
      if ((nhms == nhms_last_recycle) .or. (nhms - nhms_last_recycle >= 30000)) then
         nhms_last_recycle = nhms
         recycle_h2o2 = .true.
      end if

      ! Retrieve sulfate species molecular weights
      fMassMSA = species_mw_g(nMSA)
      fMassDMS = species_mw_g(nDMS)
      fMassSO2 = species_mw_g(nSO2)
      fMassSO4 = species_mw_g(nSO4)

      ! Extract species concentrations in native ordering (no reversal) with unit conversions
      dms(:) = species_conc(:, nDMS) * 1.0e-9_fp                       !ug/kg ==> kg/kg
      so2(:) = species_conc(:, nSO2) * 1.0e-6_fp * fMassSO2 / AIRMW   ! ppm ==> kg/kg
      so4(:) = species_conc(:, nSO4) * 1.0e-9_fp                       !ug/kg ==> kg/kg
      msa(:) = species_conc(:, nMSA) * 1.0e-6_fp * fMassMSA / AIRMW   ! ppm ==> kg/kg

      ! Initialize oxidant working arrays
      xoh = 0.0_fp
      xno3 = 0.0_fp
      xh2o2 = xh2o2_init

      ! --- Inline 1D pressure-lid lookup on pmid array in native ordering ---
      plid_pa = plid * 100.0_fp
      klid = 1
      refDiff = 150000.0_fp
      do k = 1, num_layers
         diff = abs(pmid(k) - plid_pa)
         if (diff < refDiff) then
            klid = k
            refDiff = diff
         end if
      end do

      ! --- Call internalized chemistry driver ---
      call so4chem_driver(num_layers, klid, tstep, nymd, nhms, &
         lat * deg2rad, lon * deg2rad, &
         AIRMW, AVO, g0, fMassDMS, fMassSO2, fMassSO4, fMassMSA, &
         dms, so2, so4, msa, oh_clim, no3_clim, h2o2_clim, &
         xoh, xno3, xh2o2, xh2o2_init, recycle_h2o2, &
         t, airden, delp, cldf, lwi, &
         pso2_dms, pmsa_dms, pso4g, pso4aq, RC)

      if (RC /= 0) then
         ErrMsg = 'Error in compute_gocart: Failed in internalized SO4 chemistry driver.'
         write(*,'(A)') trim(ErrMsg)
      end if

      ! Write back tendencies directly from 1D arrays in native ordering (no reversal)
      if (params%update_so2) then
         species_tendencies(:, nSO2) = so2(:) * 1.0e6_fp * AIRMW / fMassSO2  ! kg/kg ==> ppm
      else
         species_tendencies(:, nSO2) = species_conc(:, nSO2)  !keep SO2 unchanged
      end if
      species_tendencies(:, nSO4) = so4(:) * 1.0e9_fp                        !kg/kg ==> ug/kg
      species_tendencies(:, nMSA) = msa(:) * 1.0e6_fp * AIRMW / fMassMSA     ! kg/kg ==> ppm
      species_tendencies(:, nDMS) = dms(:) * 1.0e9_fp                         ! kg/kg ==> ug/kg
      species_tendencies(:, nDMS_IN) = species_conc(:, nDMS_IN)  !DMS in ocean is unchanged
      species_tendencies(:, nOH) = species_conc(:, nOH)   !keep oxidants unchanged
      species_tendencies(:, nNO3) = species_conc(:, nNO3)
      species_tendencies(:, nH2O2) = species_conc(:, nH2O2)

      ! Per-species-per-level diagnostic: 2D array (levels, species)
      if (present(Production_rate_per_species_per_level) .and. present(diagnostic_species_id)) then
         do diag_idx = 1, size(diagnostic_species_id)
            if (diagnostic_species_id(diag_idx) == nMSA) then
               Production_rate_per_species_per_level(:, diag_idx) = pmsa_dms(:)
            end if
            if (diagnostic_species_id(diag_idx) == nSO2) then
               Production_rate_per_species_per_level(:, diag_idx) = pso2_dms(:)
            end if
            if (diagnostic_species_id(diag_idx) == nSO4) then
               Production_rate_per_species_per_level(:, diag_idx) = pso4g(:) + pso4aq(:)
            end if
         end do
      end if

      if (present(PSO4_from_gaseous_SO2_per_level)) then
         PSO4_from_gaseous_SO2_per_level = pso4g(:)
      end if

      if (present(PSO4_from_aqueous_SO2_per_level)) then
         PSO4_from_aqueous_SO2_per_level = pso4aq(:)
      end if

      ! Cleanup 1D arrays
      deallocate(oh_clim, h2o2_clim, no3_clim, xoh, xno3, xh2o2)
      deallocate(dms, so2, so4, msa, pso2_dms, pmsa_dms, pso4g, pso4aq)

   end subroutine compute_gocart

end module SO4chemScheme_GOCART_Mod

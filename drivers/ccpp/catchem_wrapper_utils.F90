!> \file catchem_wrapper_utils.F90
!> \brief CATCHEM-CCPP interface utilities module
!>
!> \details
!> Provides wrapper utilities for interfacing CATCHEM chemistry model with CCPP
!> framework. Handles data transformation and management between host model and
!> CATCHEM chemistry calculations.
!>
!> \author Barry Baker
!>
!> \date 11/2024
!>
!> \ingroup catchem_ccpp_group
!!!>
module catchem_wrapper_utils

    use CATChem, only: ConfigType, MetStateType, ChemStateType, &
                      EmisStateType, DiagStateType, cc_read_config, &
                      cc_allocate_metstate, cc_allocate_chemstate, &
                      cc_allocate_diagstate, cc_allocate_emisstate, &
                      cc_checkdeallocate, cc_checkallocate, CC_SUCCESS
    use catchem_types, only: catchem_container_type

    implicit none
    private

    public :: read_catchem_config
    public :: allocate_catchem_container
    public :: transform_ccpp_to_catchem

contains

    !> Reads and initializes the CATChem configuration from a specified file
    !!
    !! This subroutine reads configuration settings and initializes the states
    !! for meteorology, emissions, and chemistry using the CATChem interface.
    !!
    !! \param[inout] config         Configuration type containing CATChem settings
    !! \param[inout] catchem_states Grid state container for CATChem components
    !! \param[in]    config_file    Path to the CATChem configuration file
    !! \param[out]   errflg         Error flag (0=success, non-zero=failure)
    !! \param[out]   errmsg         Error message if errflg is non-zero
    !!
    !! \ingroup catchem_ccpp_group
    !!!>
    subroutine read_catchem_config(config, catchem_states, config_file, errflg, errmsg)
        type(ConfigType), intent(inout) :: config
        type(catchem_container_type), intent(inout) :: catchem_states
        character(len=*), intent(in) :: config_file
        integer, intent(out) :: errflg
        character(len=*), intent(out) :: errmsg

        call cc_read_config(config, &
                          catchem_states%MetState(1), &
                          catchem_states%EmisState(1), &
                          catchem_states%ChemState(1), &
                          errflg, &
                          config_file)
        if (errflg /= 0) then
            errmsg = 'Error reading CATChem configuration file: '//trim(config_file)
        end if
    end subroutine read_catchem_config

    !> Allocates memory for the entire catchem container
    !!
    !! \param[in]    config         CATChem configuration
    !! \param[inout] catchem_states Container for all CATChem states
    !! \param[in]    im            Number of horizontal points
    !! \param[in]    kme           Number of vertical levels
    !! \param[in]    nsoil         Number of soil levels
    !! \param[out]   errflg        Error flag (0=success, non-zero=failure)
    !! \param[out]   errmsg        Error message if errflg is non-zero
    !!
    !! \ingroup catchem_ccpp_group
    !!!>
    subroutine allocate_catchem_container(config, catchem_states, im, kme, nsoil, nLandTypes, errflg, errmsg)
        type(ConfigType), intent(in) :: config
        type(catchem_container_type), intent(inout) :: catchem_states
        integer, intent(in) :: im
        integer, intent(in) :: kme
        integer, intent(in) :: nsoil
        integer, intent(in) :: nLandTypes
        integer, intent(out) :: errflg
        character(len=*), intent(out) :: errmsg

        integer :: i

        ! Initialize
        errflg = 0
        errmsg = ''

        ! Allocate State arrays in the container
        if (.not. allocated(catchem_states%MetState)) then
            allocate(catchem_states%MetState(im), stat=errflg)
            if (check_allocation_error('catchem_states%MetState', errflg, errmsg)) return

            ! Initialize each MetState using CC API
            do i = 1, im
                call CC_Allocate_MetState(catchem_states%MetState(i), errflg)
                if (errflg /= CC_SUCCESS) then
                    errmsg = 'Error in CC_Allocate_MetState'
                    return
                endif
            end do
        endif

        if (.not. allocated(catchem_states%ChemState)) then
            allocate(catchem_states%ChemState(im), stat=errflg)
            if (check_allocation_error('catchem_states%ChemState', errflg, errmsg)) return

            ! Initialize each ChemState using CC API
            do i = 1, im
                call CC_Allocate_ChemState(catchem_states%ChemState(i), config%Species_File, MetState(i), errflg)
                if (errflg /= CC_SUCCESS) then
                    errmsg = 'Error in CC_Allocate_ChemState'
                    return
                endif
            end do
        endif

        if (.not. allocated(catchem_states%DiagState)) then
            allocate(catchem_states%DiagState(im), stat=errflg)
            if (check_allocation_error('catchem_states%DiagState', errflg, errmsg)) return

            ! Initialize each DiagState using CC API
            do i = 1, im
                call CC_Allocate_DiagState(catchem_states%DiagState(i), config, MetState(i), errflg)
                if (errflg /= CC_SUCCESS) then
                    errmsg = 'Error in CC_Allocate_DiagState'
                    return
                endif
            end do
        endif

        if (.not. allocated(catchem_states%EmisState)) then
            allocate(catchem_states%EmisState(im), stat=errflg)
            if (check_allocation_error('catchem_states%EmisState', errflg, errmsg)) return

            ! Initialize each EmisState using CC API
            do i = 1, im
                call CC_Allocate_EmisState(catchem_states%EmisState(i), MetState(i), errflg)
                if (errflg /= CC_SUCCESS) then
                    errmsg = 'Error in CC_Allocate_EmisState'
                    return
                endif
            end do
        endif


    end subroutine allocate_catchem_container

    !> Transforms CCPP meteorological arrays into CATChem meteorological and chemistry states
    !!
    !! \param[in] im         Number of horizontal grid points
    !! \param[in] kme        Number of vertical levels
    !! \param[in] temp       3D temperature field (K)
    !! \param[in] spechum    3D specific humidity field (kg/kg)
    !! \param[in] pfull      3D full level pressure (Pa)
    !! \param[in] phalf      3D half level pressure (Pa)
    !! \param[in] u          3D zonal wind (m/s)
    !! \param[in] v          3D meridional wind (m/s)
    !! \param[in] delp       3D pressure thickness (Pa)
    !! \param[in] zh         3D geopotential height (m)
    !! \param[in] kh         3D vertical diffusivity (m2/s)
    !! \param[in] prsl       3D layer mean pressure (Pa)
    !! \param[in] prslk      3D Exner function
    !! \param[in] u10m       10m u wind (m/s)
    !! \param[in] v10m       10m v wind (m/s)
    !! \param[in] tskin      Surface skin temperature (K)
    !! \param[in] ps         Surface pressure (Pa)
    !! \param[in] precip     Precipitation rate (kg/m2/s)
    !! \param[in] slmsk      Land-sea mask (1=land,0=sea,2=ice)
    !! \param[in] snowh      Snow depth (m)
    !! \param[in] vegtype    Vegetation type
    !! \param[in] soiltyp    Soil type
    !! \param[in] hf         Sensible heat flux (W/m2)
    !! \param[in] ust        Friction velocity (m/s)
    !! \param[in] zpbl       PBL height (m)
    !! \param[in] coszen     Cosine of solar zenith angle
    !! \param[in] albedo     Surface albedo
    !! \param[in] emis       Surface emissivity
    !! \param[in] ustar      Friction velocity (m/s)
    !! \param[in] shflx      Surface sensible heat flux (W/m2)
    !! \param[in] lhflx      Surface latent heat flux (W/m2)
    !! \param[in] snowc      Snow cover fraction (0-1)
    !! \param[in] vegfrac    Green vegetation fraction (0-1)
    !! \param[in] swdn       Surface downward SW radiation (W/m2)
    !! \param[in] swup       Surface upward SW radiation (W/m2)
    !! \param[in] lwdn       Surface downward LW radiation (W/m2)
    !! \param[in] lwup       Surface upward LW radiation (W/m2)
    !! \param[in] swdnc      Clear-sky surface downward SW radiation (W/m2)
    !! \param[in] swupc      Clear-sky surface upward SW radiation (W/m2)
    !! \param[in] lwdnc      Clear-sky surface downward LW radiation (W/m2)
    !! \param[in] lwupc      Clear-sky surface upward LW radiation (W/m2)
    !! \param[inout] MetState   CATChem meteorology state
    !! \param[inout] ChemState  CATChem chemistry state
    !! \param[out] errmsg     Error message
    !! \param[out] errflg     Error flag
    !!
    !! \ingroup catchem_ccpp_group
    !!!>
    subroutine transform_ccpp_to_catchem(nVert, nVertInterface, nsoil, nlndcat, nsoilcat, lat, lon, &      ! Grid Information
                                        ktau, dt, jdate, tile_num, area, xcosz, &  ! Grid Information
                                        aero_rad_freq_opt, aero_feedback_opt, plmrise_freq_opt, & ! Model Options
                                        lwi, dluse, soiltyp, vegtype_frac, &  ! Model Options
                                        im, kme, nVert, nVertInterface, nsoil, nlndcat, nsoilcat, &  ! Model Options
                                        temp, spechum, pfull, phalf, &  ! Meteorological Variables
                                        u, v, delp, zh, kh, prsl, prslk, &  ! Meteorological Variables
                                        u10m, v10m, tskin, ps, precip, &  ! Meteorological Variables
                                        slmsk, snowh, vegtype, soiltyp, &  ! Surface Variables
                                        hf, zpbl, coszen, albedo, emis, &  ! Surface Variables
                                        ustar, shflx, lhflx, &  ! Near-Surface Meteorology
                                        snowc, vegfrac, &  ! Surface Variables
                                        swdn, swup, lwdn, lwup, &  ! Radiation Fluxes
                                        swdnc, swupc, lwdnc, lwupc, &  ! Radiation Fluxes
                                        MetState, ChemState, EmisState, DiagState, &  ! CATChem States
                                        errmsg, errflg)  ! Error Handling
                                        MetState, ChemState, EmisState, DiagState)

      use CATChem, only: MetStateType, ChemStateType, DiagStateType, EmisStateType

      implicit none
      !! Transform CCPP meteorological arrays to CATChem states
      integer,  intent(in)    :: im              !> number of horizontal points
      integer,  intent(in)    :: kme             !> number of vertical levels
      integer, intent(in)     :: nVert           !> number of vertical levels
      integer, intent(in)     :: nVertInterface  !> number of vertical levels
      integer, intent(in)     :: nsoil           !> number of soil levels
      integer, intent(in)     :: nlndcat         !> number of land categories
      integer, intent(in)     :: nsoilcat        !> number of soil categories

      ! 3D/Layer Variables (dim(:,:))
      real(kind=phys), intent(in) :: temp(:,:)       !> temperature (K)
      real(kind=phys), intent(in) :: spechum(:,:)    !> specific humidity (kg/kg)
      real(kind=phys), intent(in) :: pfull(:,:)      !> full level pressure (Pa)
      real(kind=phys), intent(in) :: phalf(:,:)      !> half level pressure (Pa)
      real(kind=phys), intent(in) :: u(:,:)          !> zonal wind (m/s)
      real(kind=phys), intent(in) :: v(:,:)          !> meridional wind (m/s)
      real(kind=phys), intent(in) :: delp(:,:)       !> pressure thickness (Pa)
      real(kind=phys), intent(in) :: zh(:,:)         !> geopotential height (m)
      real(kind=phys), intent(in) :: kh(:,:)         !> vertical diffusivity (m2/s)
      real(kind=phys), intent(in) :: prsl(:,:)       !> layer mean pressure (Pa)
      real(kind=phys), intent(in) :: prslk(:,:)      !> Exner function

      ! Surface Variables (dim(:))
      real(kind=phys), intent(in) :: ps(:)           !> surface pressure (Pa)
      real(kind=phys), intent(in) :: tskin(:)        !> skin temperature (K)
      real(kind=phys), intent(in) :: slmsk(:)        !> land-sea mask (1=land,0=sea,2=ice)
      real(kind=phys), intent(in) :: snowh(:)        !> snow depth (m)
      real(kind=phys), intent(in) :: vegtype(:)      !> vegetation type
      real(kind=phys), intent(in) :: soiltyp(:)      !> soil type
      real(kind=phys), intent(in) :: snowc(:)        !> snow cover fraction (0-1)
      real(kind=phys), intent(in) :: vegfrac(:)      !> green vegetation fraction (0-1)

      ! Near-Surface Meteorology (dim(:))
      real(kind=phys), intent(in) :: u10m(:)         !> 10m u wind (m/s)
      real(kind=phys), intent(in) :: v10m(:)         !> 10m v wind (m/s)
      real(kind=phys), intent(in) :: ustar(:)        !> friction velocity (m/s)
      real(kind=phys), intent(in) :: zpbl(:)         !> PBL height (m)

      ! Surface Fluxes (dim(:))
      real(kind=phys), intent(in) :: hf(:)           !> sensible heat flux (W/m2)
      real(kind=phys), intent(in) :: shflx(:)        !> surface sensible heat flux (W/m2)
      real(kind=phys), intent(in) :: lhflx(:)        !> surface latent heat flux (W/m2)
      real(kind=phys), intent(in) :: precip(:)       !> precipitation rate (kg/m2/s)

      ! Radiation Properties (dim(:))
      real(kind=phys), intent(in) :: coszen(:)       !> cosine of solar zenith angle
      real(kind=phys), intent(in) :: albedo(:)       !> surface albedo
      real(kind=phys), intent(in) :: emis(:)         !> surface emissivity

      ! Radiation Fluxes (dim(:))
      real(kind=phys), intent(in) :: swdn(:)         !> downward shortwave radiation at surface (W/m2)
      real(kind=phys), intent(in) :: swup(:)         !> upward shortwave radiation at surface (W/m2)
      real(kind=phys), intent(in) :: lwdn(:)         !> downward longwave radiation at surface (W/m2)
      real(kind=phys), intent(in) :: lwup(:)         !> upward longwave radiation at surface (W/m2)
      real(kind=phys), intent(in) :: swdnc(:)        !> clear-sky downward shortwave radiation (W/m2)
      real(kind=phys), intent(in) :: swupc(:)        !> clear-sky upward shortwave radiation (W/m2)
      real(kind=phys), intent(in) :: lwdnc(:)        !> clear-sky downward longwave radiation (W/m2)2)
      real(kind=phys), intent(in) :: lwupc(:)        !> clear-sky upward longwave radiation (W/m2)

      ! Emissions
      real(kind=phys), intent(in) :: emi_in(:)         !> emissions
      ! CATChem States
      type(MetStateType),  intent(inout) :: MetState(:)    !> CATChem meteorology state
      type(ChemStateType), intent(inout) :: ChemState(:)   !> CATChem chemistry state
      type(EmisStateType), intent(inout) :: EmisState(:)   !> CATChem emission state
      type(DiagStateType), intent(inout) :: DiagState(:)   !> CATChem diagnostic state

      ! Error handling
      character(len=*), intent(out) :: errmsg    !> error message
      integer,          intent(out) :: errflg    !> error flag

      ! Local variables
      integer :: i, k

      ! Initialize error handling
      errmsg = ''
      errflg = 0

      ! Check dimensions
      if (size(MetState) /= im) then
      errmsg = 'MetState dimension mismatch'
      errflg = 1
      return
      endif

      ! Transform data for each horizontal point
      horiz: do i = 1, im
        ! Verify vertical dimension
        if (MetState(i)%nLEVS /= kme) then
        errmsg = 'Vertical dimension mismatch'
        errflg = 1
        return
        endif

        ! 3D/Layer Variables
        vert: do k = 1, kme
            MetState(i)%temp(k)    = temp(i,k)
            MetState(i)%spechum(k) = spechum(i,k)
            MetState(i)%pfull(k)   = pfull(i,k)
            MetState(i)%phalf(k)   = phalf(i,k)
            MetState(i)%u(k)       = u(i,k)
            MetState(i)%v(k)       = v(i,k)
            MetState(i)%delp(k)    = delp(i,k)
            MetState(i)%zh(k)      = zh(i,k)
            MetState(i)%kh(k)      = kh(i,k)
            MetState(i)%prsl(k)    = prsl(i,k)
            MetState(i)%prslk(k)   = prslk(i,k)
        end do vert

        ! Surface Variables
        MetState(i)%ps       = ps(i)
        MetState(i)%tskin    = tskin(i)
        MetState(i)%slmsk    = slmsk(i)
        MetState(i)%snowh    = snowh(i)
        MetState(i)%vegtype  = vegtype(i)
        MetState(i)%soiltyp  = soiltyp(i)
        MetState(i)%snowc    = snowc(i)
        MetState(i)%vegfrac  = vegfrac(i)

        ! Near-Surface Meteorology
        MetState(i)%u10m     = u10m(i)
        MetState(i)%v10m     = v10m(i)
        MetState(i)%zpbl     = zpbl(i)
        MetState(i)%ustar    = ustar(i)

        ! Surface Fluxes
        MetState(i)%hf       = hf(i)
        MetState(i)%shflx    = shflx(i)
        MetState(i)%lhflx    = lhflx(i)
        MetState(i)%precip   = precip(i)
        MetState(i)%ustar    = ustar(i)

        ! Radiation Properties
        MetState(i)%coszen   = coszen(i)
        MetState(i)%albedo   = albedo(i)
        MetState(i)%emis     = emis(i)

        ! Radiation Fluxes
        MetState(i)%swdn     = swdn(i)
        MetState(i)%swup     = swup(i)
        MetState(i)%lwdn     = lwdn(i)
        MetState(i)%lwup     = lwup(i)
        MetState(i)%swdnc    = swdnc(i)
        MetState(i)%swupc    = swupc(i)
        MetState(i)%lwdnc    = lwdnc(i)
        MetState(i)%lwupc    = lwupc(i)

      end do horiz

    end subroutine transform_ccpp_to_catchem

    !> Checks for allocation errors and sets error message
    !!
    !! \param[in] state_name Name of the state being allocated
    !! \param[in] errflg     Error flag from allocation
    !! \param[out] errmsg    Output error message if allocation failed
    !! \return has_error     True if allocation error occurred
    !! \ingroup catchem_ccpp_group
    !!!>
    function check_allocation_error(state_name, errflg, errmsg) result(has_error)
        character(len=*), intent(in) :: state_name
        integer, intent(in) :: errflg
        character(len=*), intent(out) :: errmsg
        logical :: has_error

        has_error = (errflg /= 0)
        if (has_error) then
            errmsg = 'Error allocating '//trim(state_name)//' - catchem_wrapper_init'
        end if
    end function check_allocation_error

end module catchem_wrapper_utils

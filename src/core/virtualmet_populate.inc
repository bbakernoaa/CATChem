! Generated macro for populating VirtualMetType with MetState field pointers
! This macro should be included in the populate_virtual_column subroutine
! Auto-generated from MetState field definitions

! Populate 3D atmospheric field pointers (vertical levels: nlev)
call this%met_state%get_field_ptr('AIRDEN', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%AIRDEN => column_ptr
end if

call this%met_state%get_field_ptr('AIRNUMDEN', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%AIRNUMDEN => column_ptr
end if

call this%met_state%get_field_ptr('AIRVOL', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%AIRVOL => column_ptr
end if

call this%met_state%get_field_ptr('AVGW', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%AVGW => column_ptr
end if

call this%met_state%get_field_ptr('BXHEIGHT', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%BXHEIGHT => column_ptr
end if

call this%met_state%get_field_ptr('CLDF', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%CLDF => column_ptr
end if

call this%met_state%get_field_ptr('CMFMC', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%CMFMC => column_ptr
end if

call this%met_state%get_field_ptr('DAIRMASS', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%DAIRMASS => column_ptr
end if

call this%met_state%get_field_ptr('DELP', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%DELP => column_ptr
end if

call this%met_state%get_field_ptr('DELP_DRY', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%DELP_DRY => column_ptr
end if

call this%met_state%get_field_ptr('DQRCU', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%DQRCU => column_ptr
end if

call this%met_state%get_field_ptr('DQRLSAN', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%DQRLSAN => column_ptr
end if

call this%met_state%get_field_ptr('DTRAIN', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%DTRAIN => column_ptr
end if

call this%met_state%get_field_ptr('F_OF_PBL', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%F_OF_PBL => column_ptr
end if

call this%met_state%get_field_ptr('F_UNDER_PBLTOP', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%F_UNDER_PBLTOP => column_ptr
end if

call this%met_state%get_field_ptr('MAIRDEN', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%MAIRDEN => column_ptr
end if

call this%met_state%get_field_ptr('OMEGA', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%OMEGA => column_ptr
end if

call this%met_state%get_field_ptr('PEDGE', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%PEDGE => column_ptr
end if

call this%met_state%get_field_ptr('PEDGE_DRY', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%PEDGE_DRY => column_ptr
end if

call this%met_state%get_field_ptr('PFICU', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%PFICU => column_ptr
end if

call this%met_state%get_field_ptr('PFILSAN', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%PFILSAN => column_ptr
end if

call this%met_state%get_field_ptr('PFLCU', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%PFLCU => column_ptr
end if

call this%met_state%get_field_ptr('PFLLSAN', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%PFLLSAN => column_ptr
end if

call this%met_state%get_field_ptr('PMID', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%PMID => column_ptr
end if

call this%met_state%get_field_ptr('PMID_DRY', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%PMID_DRY => column_ptr
end if

call this%met_state%get_field_ptr('QI', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%QI => column_ptr
end if

call this%met_state%get_field_ptr('QL', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%QL => column_ptr
end if

call this%met_state%get_field_ptr('QV', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%QV => column_ptr
end if

call this%met_state%get_field_ptr('RH', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%RH => column_ptr
end if

call this%met_state%get_field_ptr('SPHU', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%SPHU => column_ptr
end if

call this%met_state%get_field_ptr('T', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%T => column_ptr
end if

call this%met_state%get_field_ptr('TAUCLI', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%TAUCLI => column_ptr
end if

call this%met_state%get_field_ptr('TAUCLW', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%TAUCLW => column_ptr
end if

call this%met_state%get_field_ptr('THETA', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%THETA => column_ptr
end if

call this%met_state%get_field_ptr('TV', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%TV => column_ptr
end if

call this%met_state%get_field_ptr('U', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%U => column_ptr
end if

call this%met_state%get_field_ptr('V', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%V => column_ptr
end if

call this%met_state%get_field_ptr('Z', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%Z => column_ptr
end if

call this%met_state%get_field_ptr('ZMID', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%ZMID => column_ptr
end if

! Populate 3D categorical field pointers (NOT vertical levels!)
! WARNING: These have different dimensions - do not iterate with nlev!
call this%met_state%get_field_ptr('FRLAI', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%FRLAI => column_ptr
end if

call this%met_state%get_field_ptr('FRLANDUSE', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%FRLANDUSE => column_ptr
end if

call this%met_state%get_field_ptr('FRSOIL', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%FRSOIL => column_ptr
end if

call this%met_state%get_field_ptr('FRZ0', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%FRZ0 => column_ptr
end if

call this%met_state%get_field_ptr('SOILM', grid_i, grid_j, &
                                 col_ptr=column_ptr, rc=field_rc)
if (field_rc == 0 .and. associated(column_ptr)) then
   virtual_col%met%SOILM => column_ptr
end if

! Populate 2D scalar fields
call this%met_state%get_field_ptr('ALBD_NIR', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%ALBD_NIR = scalar_val
end if

call this%met_state%get_field_ptr('ALBD_UV', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%ALBD_UV = scalar_val
end if

call this%met_state%get_field_ptr('ALBD_VIS', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%ALBD_VIS = scalar_val
end if

call this%met_state%get_field_ptr('AREA_M2', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%AREA_M2 = scalar_val
end if

call this%met_state%get_field_ptr('CLAYFRAC', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%CLAYFRAC = scalar_val
end if

call this%met_state%get_field_ptr('CLDFRC', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%CLDFRC = scalar_val
end if

call this%met_state%get_field_ptr('CNV_FRC', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%CNV_FRC = scalar_val
end if

call this%met_state%get_field_ptr('CONV_DEPTH', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%CONV_DEPTH = scalar_val
end if

call this%met_state%get_field_ptr('EFLUX', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%EFLUX = scalar_val
end if

call this%met_state%get_field_ptr('FLASH_DENS', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%FLASH_DENS = scalar_val
end if

call this%met_state%get_field_ptr('FRLAKE', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%FRLAKE = scalar_val
end if

call this%met_state%get_field_ptr('FRLAND', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%FRLAND = scalar_val
end if

call this%met_state%get_field_ptr('FRLANDIC', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%FRLANDIC = scalar_val
end if

call this%met_state%get_field_ptr('FROCEAN', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%FROCEAN = scalar_val
end if

call this%met_state%get_field_ptr('FRSEAICE', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%FRSEAICE = scalar_val
end if

call this%met_state%get_field_ptr('FRSNO', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%FRSNO = scalar_val
end if

call this%met_state%get_field_ptr('FRVEG', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%FRVEG = scalar_val
end if

call this%met_state%get_field_ptr('GVF', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%GVF = scalar_val
end if

call this%met_state%get_field_ptr('GWETROOT', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%GWETROOT = scalar_val
end if

call this%met_state%get_field_ptr('GWETTOP', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%GWETTOP = scalar_val
end if

call this%met_state%get_field_ptr('HFLUX', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%HFLUX = scalar_val
end if

call this%met_state%get_field_ptr('LAI', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%LAI = scalar_val
end if

call this%met_state%get_field_ptr('LAT', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%LAT = scalar_val
end if

call this%met_state%get_field_ptr('LON', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%LON = scalar_val
end if

call this%met_state%get_field_ptr('OBK', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%OBK = scalar_val
end if

call this%met_state%get_field_ptr('PARDF', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%PARDF = scalar_val
end if

call this%met_state%get_field_ptr('PARDR', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%PARDR = scalar_val
end if

call this%met_state%get_field_ptr('PBLH', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%PBLH = scalar_val
end if

call this%met_state%get_field_ptr('PHIS', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%PHIS = scalar_val
end if

call this%met_state%get_field_ptr('PRECANV', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%PRECANV = scalar_val
end if

call this%met_state%get_field_ptr('PRECCON', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%PRECCON = scalar_val
end if

call this%met_state%get_field_ptr('PRECLSC', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%PRECLSC = scalar_val
end if

call this%met_state%get_field_ptr('PS', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%PS = scalar_val
end if

call this%met_state%get_field_ptr('PS_DRY', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%PS_DRY = scalar_val
end if

call this%met_state%get_field_ptr('PS_WET', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%PS_WET = scalar_val
end if

call this%met_state%get_field_ptr('QV2M', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%QV2M = scalar_val
end if

call this%met_state%get_field_ptr('RDRAG', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%RDRAG = scalar_val
end if

call this%met_state%get_field_ptr('SANDFRAC', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%SANDFRAC = scalar_val
end if

call this%met_state%get_field_ptr('SEAICE00', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%SEAICE00 = scalar_val
end if

call this%met_state%get_field_ptr('SEAICE10', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%SEAICE10 = scalar_val
end if

call this%met_state%get_field_ptr('SEAICE20', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%SEAICE20 = scalar_val
end if

call this%met_state%get_field_ptr('SEAICE30', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%SEAICE30 = scalar_val
end if

call this%met_state%get_field_ptr('SEAICE40', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%SEAICE40 = scalar_val
end if

call this%met_state%get_field_ptr('SEAICE50', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%SEAICE50 = scalar_val
end if

call this%met_state%get_field_ptr('SEAICE60', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%SEAICE60 = scalar_val
end if

call this%met_state%get_field_ptr('SEAICE70', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%SEAICE70 = scalar_val
end if

call this%met_state%get_field_ptr('SEAICE80', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%SEAICE80 = scalar_val
end if

call this%met_state%get_field_ptr('SEAICE90', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%SEAICE90 = scalar_val
end if

call this%met_state%get_field_ptr('SLP', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%SLP = scalar_val
end if

call this%met_state%get_field_ptr('SNODP', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%SNODP = scalar_val
end if

call this%met_state%get_field_ptr('SNOMAS', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%SNOMAS = scalar_val
end if

call this%met_state%get_field_ptr('SSM', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%SSM = scalar_val
end if

call this%met_state%get_field_ptr('SST', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%SST = scalar_val
end if

call this%met_state%get_field_ptr('SUNCOS', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%SUNCOS = scalar_val
end if

call this%met_state%get_field_ptr('SUNCOSmid', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%SUNCOSmid = scalar_val
end if

call this%met_state%get_field_ptr('SUNCOSsum', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%SUNCOSsum = scalar_val
end if

call this%met_state%get_field_ptr('SWGDN', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%SWGDN = scalar_val
end if

call this%met_state%get_field_ptr('SZAFACT', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%SZAFACT = scalar_val
end if

call this%met_state%get_field_ptr('T2M', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%T2M = scalar_val
end if

call this%met_state%get_field_ptr('TO3', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%TO3 = scalar_val
end if

call this%met_state%get_field_ptr('TROPP', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%TROPP = scalar_val
end if

call this%met_state%get_field_ptr('TS', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%TS = scalar_val
end if

call this%met_state%get_field_ptr('TSKIN', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%TSKIN = scalar_val
end if

call this%met_state%get_field_ptr('TropHt', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%TropHt = scalar_val
end if

call this%met_state%get_field_ptr('U10M', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%U10M = scalar_val
end if

call this%met_state%get_field_ptr('USTAR', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%USTAR = scalar_val
end if

call this%met_state%get_field_ptr('USTAR_THRESHOLD', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%USTAR_THRESHOLD = scalar_val
end if

call this%met_state%get_field_ptr('V10M', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%V10M = scalar_val
end if

call this%met_state%get_field_ptr('WILT', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%WILT = scalar_val
end if

call this%met_state%get_field_ptr('Z0', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%Z0 = scalar_val
end if

call this%met_state%get_field_ptr('Z0H', grid_i, grid_j, &
                                 scalar_val=scalar_val, rc=field_rc)
if (field_rc == 0) then
   virtual_col%met%Z0H = scalar_val
end if

! Note: Categorical 3D fields are automatically detected and marked.
! They require specific category indices, not column extraction.
! Scalar fields are not included as they are global, not column-specific.

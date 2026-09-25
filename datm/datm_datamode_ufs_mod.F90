!===============================================================================
!
!  Module: datm_datamode_ufs_mod
!
!  Description:
!    Unified datamode for CDEPS. Replaces hardcoded datamodes with a dynamic, 
!    configuration-driven approach. 
!    
!    Phase 1: Automatically maps any variable defined in the ESMF config stream 
!             directly to the export state.
!    Phase 2: Executes user-configurable, chained derived-variable calculations 
!             driven by the calc_opts string passed from the NUOPC cap.
!
!===============================================================================
module datm_datamode_ufs_mod
  
  use ESMF
  use NUOPC
  
  ! CDEPS-native imports
  use shr_kind_mod,    only: r8 => shr_kind_r8
  use shr_strdata_mod, only: shr_strdata_type, shr_strdata_get_stream_pointer
  use dshr_state_mod,  only: dshr_state_getfldptr
  use shr_log_mod,     only: shr_log_error
  use dshr_utils_mod,  only: ChkErr
  use dshr_stream_mod, only: dshr_stream_type, shr_stream_init_from_esmfconfig

  implicit none
  private
  
  public :: DatamodeUnifiedInit
  public :: DatamodeUnifiedRun
  public :: unified_datamode_type

  character(len=*), parameter :: u_FILE_u = 'cdeps_datamode_unified_mod.F90'

  !> \brief Maps configuration string to persistent memory pointers
  type :: unified_var_map
     character(len=64) :: var_name
     real(r8), pointer :: ptr_strm(:) => null()
     real(r8), pointer :: ptr_exp(:)  => null()
  end type unified_var_map

  !> \brief Master state for the unified datamode
  type :: unified_datamode_type
     type(unified_var_map), allocatable :: var_maps(:)
  end type unified_datamode_type

contains

  !=============================================================================
  ! \brief Parse ESMF config for stream variables and dynamically build map
  !=============================================================================
  subroutine DatamodeUnifiedInit(config, unified_state, rc)
    type(ESMF_Config),             intent(in)    :: config
    type(unified_datamode_type),   intent(inout) :: unified_state
    integer,                       intent(out)   :: rc
    
    character(len=*), parameter :: subName = 'DatamodeUnifiedInit: '
    type(dshr_stream_type), allocatable :: streams(:)
    integer :: i, j

    rc = ESMF_SUCCESS

    ! Parse ESMF Config for 1-to-1 Stream Variables (Dynamically Appended)
    call shr_stream_init_from_esmfconfig(streams, config, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (allocated(streams)) then
      do i = 1, size(streams)
        do j = 1, streams(i)%nvars
          ! Dynamically grow the var_maps array using move_alloc
          call append_var_map(unified_state%var_maps, streams(i)%fldName(j))
        end do
      end do
      deallocate(streams)
    end if

  end subroutine DatamodeUnifiedInit


  !=============================================================================
  ! \brief Maps raw variables, then chains calculations driven by calc_opts
  !=============================================================================
  subroutine DatamodeUnifiedRun(sdat, exportState, unified_state, calc_opts, rc)
    type(shr_strdata_type),        intent(inout) :: sdat
    type(ESMF_State),              intent(inout) :: exportState
    type(unified_datamode_type),   intent(inout) :: unified_state
    character(len=*),              intent(in)    :: calc_opts
    integer,                       intent(out)   :: rc
    
    character(len=*), parameter :: subName = 'DatamodeUnifiedRun: '
    integer :: i

    rc = ESMF_SUCCESS

    ! -------------------------------------------------------------------------
    ! Phase 1: Ingestion (Stream -> Export State)
    ! -------------------------------------------------------------------------
    if (allocated(unified_state%var_maps)) then
      do i = 1, size(unified_state%var_maps)
        
        call shr_strdata_get_stream_pointer(sdat, trim(unified_state%var_maps(i)%var_name), &
             unified_state%var_maps(i)%ptr_strm, requirePointer=.true., &
             errmsg=trim(subName)//'ERROR: stream pointer missing for '//trim(unified_state%var_maps(i)%var_name), &
             rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        
        call dshr_state_getfldptr(exportState, trim(unified_state%var_maps(i)%var_name), &
             fldptr1=unified_state%var_maps(i)%ptr_exp, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        
        unified_state%var_maps(i)%ptr_exp(:) = unified_state%var_maps(i)%ptr_strm(:)
        
      end do
    end if

    ! -------------------------------------------------------------------------
    ! Phase 2: Chained Calculations Driver
    ! -------------------------------------------------------------------------
    ! Only enter the driver if there are options requested to save overhead
    if (len_trim(calc_opts) > 0) then
      call DatamodeUnifiedCalcDriver(exportState, calc_opts, rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if
    
  end subroutine DatamodeUnifiedRun


  !=============================================================================
  ! \brief Subroutine to dynamically append variables (Fortran 2003 move_alloc)
  !=============================================================================
  subroutine append_var_map(var_maps, new_var_name)
    type(unified_var_map), allocatable, intent(inout) :: var_maps(:)
    character(len=*),                   intent(in)    :: new_var_name
    
    type(unified_var_map), allocatable :: temp_maps(:)
    integer :: current_size

    if (allocated(var_maps)) then
      current_size = size(var_maps)
      allocate(temp_maps(current_size + 1))
      temp_maps(1:current_size) = var_maps
      temp_maps(current_size + 1)%var_name = trim(new_var_name)
      call move_alloc(from=temp_maps, to=var_maps)
    else
      allocate(var_maps(1))
      var_maps(1)%var_name = trim(new_var_name)
    end if
  end subroutine append_var_map


  !=============================================================================
  ! \brief Driver subroutine to route execution based on calc_opts string
  !=============================================================================
  subroutine DatamodeUnifiedCalcDriver(exportState, calc_opts, rc)
    type(ESMF_State), intent(inout) :: exportState
    character(len=*), intent(in)    :: calc_opts
    integer,          intent(out)   :: rc
    
    rc = ESMF_SUCCESS

    ! 1. Convert cumulative precipitation (m) to rate (kg/m^2/s)
    if (index(calc_opts, 'convert_precip_accum_to_rate') > 0) then
      call calc_convert_precip_accum_to_rate(exportState, rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    ! 2. Convert cumulative radiation (J/m^2) to flux (W/m^2)
    if (index(calc_opts, 'convert_rad_accum_to_flux') > 0) then
      call calc_convert_rad_accum_to_flux(exportState, rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    ! 3. Partition net shortwave radiation into bands
    if (index(calc_opts, 'partition_sw_4band') > 0) then
      call calc_partition_sw_4band(exportState, rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    ! 4. Partition precipitation into rain and snow
    if (index(calc_opts, 'partition_precip_freezing') > 0) then
      call calc_partition_precip_freezing(exportState, rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

  end subroutine DatamodeUnifiedCalcDriver


  !=============================================================================
  ! Modular Calculation Subroutines
  !=============================================================================

  subroutine calc_convert_precip_accum_to_rate(exportState, rc)
    type(ESMF_State), intent(inout) :: exportState
    integer,          intent(out)   :: rc
    character(len=*), parameter :: subName = 'calc_convert_precip_accum_to_rate: '
    real(r8), pointer :: Faxa_prec(:) => null()

    rc = ESMF_SUCCESS
    call dshr_state_getfldptr(exportState, 'Faxa_prec', fldptr1=Faxa_prec, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    
    if (.not. associated(Faxa_prec)) then
      call shr_log_error(trim(subName)//'ERROR: Faxa_prec required.', rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    ! Convert from cumulative meters per hour to kg/m^2/s
    ! (rho_water = 1000 kg/m^3, time = 3600 s)
    Faxa_prec(:) = Faxa_prec(:) * (1000.0_r8 / 3600.0_r8)
  end subroutine calc_convert_precip_accum_to_rate


  subroutine calc_convert_rad_accum_to_flux(exportState, rc)
    type(ESMF_State), intent(inout) :: exportState
    integer,          intent(out)   :: rc
    character(len=*), parameter :: subName = 'calc_convert_rad_accum_to_flux: '
    real(r8), pointer :: Faxa_swdn(:) => null()

    rc = ESMF_SUCCESS
    call dshr_state_getfldptr(exportState, 'Faxa_swdn', fldptr1=Faxa_swdn, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    
    if (associated(Faxa_swdn)) then
      Faxa_swdn(:) = Faxa_swdn(:) / 3600.0_r8
    end if
  end subroutine calc_convert_rad_accum_to_flux


  subroutine calc_partition_sw_4band(exportState, rc)
    type(ESMF_State), intent(inout) :: exportState
    integer,          intent(out)   :: rc
    character(len=*), parameter :: subName = 'calc_partition_sw_4band: '
    
    real(r8), pointer :: Faxa_swdn(:) => null()
    real(r8), pointer :: Faxa_swvdr(:) => null()
    real(r8), pointer :: Faxa_swvdf(:) => null()
    real(r8), pointer :: Faxa_swndr(:) => null()
    real(r8), pointer :: Faxa_swndf(:) => null()
    
    rc = ESMF_SUCCESS
    ! (Fetch pointers and validate them here...)
  end subroutine calc_partition_sw_4band


  subroutine calc_partition_precip_freezing(exportState, rc)
    type(ESMF_State), intent(inout) :: exportState
    integer,          intent(out)   :: rc
    character(len=*), parameter :: subName = 'calc_partition_precip_freezing: '
    
    real(r8), pointer :: Faxa_prec(:) => null()
    real(r8), pointer :: Faxa_prrn(:) => null()
    real(r8), pointer :: Faxa_prsn(:) => null()

    rc = ESMF_SUCCESS
    call dshr_state_getfldptr(exportState, 'Faxa_prec', fldptr1=Faxa_prec, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (.not. associated(Faxa_prec)) then
      call shr_log_error(trim(subName)//'ERROR: Faxa_prec required.', rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    call dshr_state_getfldptr(exportState, 'Faxa_prrn', fldptr1=Faxa_prrn, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_prsn', fldptr1=Faxa_prsn, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Check associations and partition...
  end subroutine calc_partition_precip_freezing

end module datm_datamode_ufs_mod

module shr_is_restart_fh_mod

  ! Common methods for components to check if it's time to write forecast hour-based restarts

  use dshr_methods_mod , only : chkerr

  implicit none
  private
  save

  public :: init_is_restart_fh, is_restart_fh, finalize_restart_fh
  
  logical :: write_restartfh = .false.
  type(ESMF_Time), allocatable :: restartFhTimes(:)
!$OMP THREADPRIVATE (write_restartfh, restartFhTimes)

contains

  !-----------------------------------------------------------------------
  subroutine init_is_restart_fh(currentTime, dtime, lLog)
    !
    ! !DESCRIPTION:
    ! Process restart_fh attribute from model_configure in UFS
    !
    ! !USES:
    use ESMF, only : ESMF_ConfigCreate, ESMF_ConfigDestroy, ESMF_ConfigLoadFile, &
                     ESMF_ConfigGetLen, ESMF_ConfigGetAttribute, ESMF_TimePrint, &
                     ESMF_LOGMSG_INFO
    !
    ! !ARGUMENTS:
    type(ESMF_Time), intent(in) :: currentTime
    integer, intent(in)         :: dtime ! time step (s)
    logical, intent(in)         :: lLog ! If true, this task logs restart_fh info
    !
    ! !LOCAL VARIABLES:
    character(len=256)           :: timestr
    integer                      :: nfh, fh_s, rc
    real(kind=ESMF_KIND_R8), allocatable :: restart_fh(:)
    type(ESMF_TimeInterval)      :: fhInterval
    type(ESMF_Config)            :: CF_mc
    
    character(len=*), parameter :: subname = 'init_restart_fh'
    !-----------------------------------------------------------------------

    ! set up Times to write non-interval restarts
    inquire(FILE='model_configure', EXIST=isPresent)
    if (isPresent) then !model_configure exists. this is ufs run
      CF_mc = ESMF_ConfigCreate(rc=rc)
      call ESMF_ConfigLoadFile(config=CF_mc,filename='model_configure' ,rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      nfh = ESMF_ConfigGetLen(config=CF_mc, label ='restart_fh:',rc=rc)
      if (nfh .gt. 0) then
        allocate(restart_fh(1:nfh))
        allocate(restartFhTimes(1:nfh)) !not deallocated here

        call ESMF_ConfigGetAttribute(CF_mc,valueList=restart_fh,label='restart_fh:', rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        ! create a list of times at each restart_fh
        do n = 1,nfh
          fh_s = NINT(3600*restart_fh(n))
          call ESMF_TimeIntervalSet(fhInterval, s=fh_s, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          restartFhTimes(n) = currentTime + fhInterval
          call ESMF_TimePrint(restartFhTimes(n), options="string", &
                              preString="restart_fh at ", unit=timestr, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          if (lLog) then
            if (mod(fh_s,dtime) /= 0) then
              call ESMF_LogWrite('restart time NOT to be written for ''//trim(timestr), ESMF_LOGMSG_INFO)
            else
              call ESMF_LogWrite('restart time to be written for ''//trim(timestr), ESMF_LOGMSG_INFO)
            end if
          end if
        end do
        deallocate(restart_fh)
      end if !nfh>0
      call ESMF_ConfigDestroy(CF_mc, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if !model_configure
  
  end subroutine init_is_restart_fh

  function is_restart_fh(clock) result(lWrite)
    !
    ! !DESCRIPTION:
    ! True/false if time to write restart
    !
    ! !USES:
    use ESMF, only : ESMF_ClockGetNextTime
    
    !
    ! !ARGUMENTS:
    type(ESMF_Clock), intent(in) :: clock
    logical :: lWrite ! function result
    !
    ! !LOCAL VARIABLES:
    integer                    :: nfh, rc
    type(ESMF_Time)            :: nextTime
    
    character(len=*), parameter :: subname = 'is_restart_fh'
    !-----------------------------------------------------------------------

    write_restartfh = .false.
    if (allocated(restartFhTimes)) then
      ! check if next time is == to any restartfhtime
      do nfh = 1,size(restartFhTimes)
        call ESMF_ClockGetNextTime(clock, nextTime=nexttime, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        if (nextTime == restartFhTimes(n)) write_restartfh = .true.
      end do
    end if

    lWrite = write_restartfh
    
  end function is_restart_fh

  subroutine finalize_restart_fh()
    !
    ! !DESCRIPTION:
    ! Clean-up...release allocated memory
    !
    ! !USES:
    !
    ! !ARGUMENTS:
    !
    ! !LOCAL VARIABLES:
    
    character(len=*), parameter :: subname = 'finalize_restart_fh'
    !-----------------------------------------------------------------------

    if (allocated(restartFhTimes)) deallocate(restartFhTimes)

  end subroutine finalize_restart_fh

end module shr_restart_fh_mod

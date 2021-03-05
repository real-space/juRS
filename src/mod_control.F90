#include "config.h"

! #define DEBUG

!! @author Paul Baumeister
!! @version 3.0
!!
!! runtime control
module control
implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: sym = 'CTRL' !! module symbol

  public :: scf_run_control
#ifdef EXTENDED
  public :: test
#endif

  contains

  logical function scf_run_control( name, comm, eta, temp, delete ) result( run )
  use configuration, only: ControlFileNamePrefix
  use MPItools, only: MPImaster, MPIbcast0
  use configuration, only: o ! output unit, 0: no output
  implicit none
    ! parameters
    character(len=*), parameter     :: fun = ' scf_run_control: '
    integer, parameter              :: I_RUN  = 1
    integer, parameter              :: I_STOP = 0
    iounit_t, parameter             :: UNT = 13
    ! args
    character(len=*), intent(in)    :: name ! project name
    MPI_Comm, intent(in)            :: comm ! MPI communicator
    real, intent(inout), optional   :: eta  ! mixing parameter
    real, intent(inout), optional   :: temp ! smearing temperature
    logical, intent(in), optional   :: delete
    ! local vars
    status_t                        :: ios
    integer                         :: irn
    logical                         :: exists, del
    character(len=len(ControlFileNamePrefix)+len(name)) :: fn

    write( unit=fn, fmt='(9A)', iostat=ios ) trim(ControlFileNamePrefix), trim(name)

    del = .false. ; if( present( delete ) ) del = delete
    if( del ) then
      if( MPImaster(comm) ) then ! Master will delete the file
        open(unit=UNT,file=fn,action='read',status='old',iostat=ios)
        close(unit=UNT,status='delete',iostat=ios) ! delete file
      endif ! Master
      run = .true.
      return ! success
    endif ! delete file


    if( MPImaster(comm) ) then

      ! run control tries to open the control file
      inquire(file=fn,exist=exists,IOstat=ios)
      if( ios == 0 .and. exists ) then
        ! opening successful, try to read
        open(unit=UNT,file=fn,action='read',status='old',iostat=ios)
        read(unit=UNT,fmt=*,iostat=ios) irn
        if( present( eta  ) ) read(unit=UNT,fmt=*,iostat=ios) eta
        if( present( temp ) ) read(unit=UNT,fmt=*,iostat=ios) temp
        close(unit=UNT,iostat=ios) ! close file
      else  ! ios == 0
        irn = I_RUN
        ! create the control file
        open(unit=UNT,file=fn,action='write',status='unknown',iostat=ios)
        write(unit=UNT,fmt='(9(I0,A))') irn, '         ',I_RUN,':run, change to ',I_STOP,' for stop'
        if( present( eta  ) ) write(unit=UNT,fmt='(F7.3,A)') eta, '     mixing parameter'
        if( present( temp ) ) write(unit=UNT,fmt='(F10.6,A)')  temp, '  smearing temperature (in Ha)'
        close(unit=UNT,iostat=ios)
      endif ! ios == 0

    endif ! Master task

    ! Barrier for all processes
    if( present( eta  ) ) call MPIbcast0( eta,  comm )
    if( present( temp ) ) call MPIbcast0( temp, comm )
    call MPIbcast0( irn, comm )

    run = ( irn == I_RUN )
    if( .not. run .and. o>0) write(o,'(/,3A,/)') sym, fun, 'forced a STOP'

  endfunction ! scf_run_control

#ifdef EXTENDED
!+ extended

  status_t function test( )
    write(*,*,iostat=test) __FILE__,' no module test implemented!'
  endfunction ! test

!- extended
#endif

endmodule ! control

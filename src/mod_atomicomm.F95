#include "config.h"

! #define DEBUG

#ifdef DEBUG
#define cDBG
#else
#define cDBG !
#endif

!! full debug
#define cFDB !


#define ONE_PROC_RETURN
#ifdef ONE_PROC_RETURN
!! for each MPI task, the number of processes is inquired 1st and
!! the operation is only executed, if there are more than 1
#define c1PR
#else
!! otherwise the early return statement is commented out
#define c1PR !
#endif



#ifdef R1_C2
! This file will be preprocessed twice, first
! with R1_C2 == 1, and then with R1_C2 == 2
! then the two parts are concatenated to one source.
! This requires that the module head has to be
! appearing, when R1_C2 == 1 and the line "end module <name>"
! with R1_C2 == 2.

! in particular, the DoublePreprocessing is used here
! to generate code for real/complex
!    REAPLEX
! and also for implementations of real/integer
!    REALINT
! therefore, we have to take care of the MPI data type
! descriptor that changes from MPI_REAL to MPI_INTEGER
#if R1_C2 == 1

!! author Paul Baumeister
!! version 3.0
!! this module is planned to replace the collective
!! communications across the atomic communicators
!!
module atomicomm
  use configuration, only: o ! output unit, 0: no output, 6: stdout
implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: sym = 'Acomm' !! module symbol

  public :: AtomBcast
  public :: AtomReduce
  public :: Atomic_comm_lists
#ifdef EXTENDED
  public :: test
#endif

  interface AtomBcast
    module procedure AtomBcast_r1, AtomBcast_c1, AtomBcast_r2, AtomBcast_c2
  endinterface

  interface AtomReduce
    module procedure AtomReduce_r1, AtomReduce_c1, AtomReduce_r2, AtomReduce_c2
  endinterface

  status_t, private :: ierr

  contains

! end of module head
#endif


#if R1_C2 == 1
#define REAPLEX real
! and use PREC(1) for the MPIcalls
#else
#define REAPLEX complex
! and use PREC(2) for the MPIcalls
#endif


#if R1_C2 == 1
  subroutine AtomReduce_r1( &
#else
  subroutine AtomReduce_c1( &
#endif
                            q, comm, owner, list )
  use MPIconst, only: MPI_STATUS_SIZE, PREC ! PREC(1)=MPI_REAL, PREC(2)=MPI_COMPLEX
  use MPItools, only: MPImyrank
cDBG use MPIconst, only: MPI_INTEGER

    REAPLEX, intent(inout)      :: q(1:) ! q(np)
    MPI_Comm, intent(in)        :: comm ! grid communicator
    integer, intent(in)         :: owner ! atom owner rank
    integer, intent(in)         :: list(:) ! listed processes (only at owner)
#ifndef NOMPI
    character(len=*), parameter :: fun = ' AtomReduce: '
    integer, parameter          :: TAG = 11
    integer                     :: nn, il, irank, nl, me, istat(MPI_STATUS_SIZE)
    REAPLEX                     :: p(1:size(q))
cDBG  integer, parameter        :: GAT = 19
cDBG  integer                   :: nd

    me = MPImyrank( comm )
    nn = size(q) ! number of projectors
    nl = size(list) ! number of listed processes

    if( nl > 0 ) then ! owner
cDBG  if( me /= owner ) then
cDBG    write(*,*) sym, fun, 'ownership ERROR: me=',me,' owner=',owner,' size(list)=',nl!,' list=',list
cDBG    stop 'atomicomm: fatal ownership error, AtomReduce_1'
cDBG  endif

      ! receive tasks
      do il = 1, nl ! for the complete list
        irank = list(il) ! rank of listed process
        if( irank == me ) cycle ! do not recv from myself

cDBG    call MPI_Recv( nd, 1,   MPI_INTEGER, irank, GAT, comm, istat, ierr ) cDBG ; call check( __LINE__ )
cDBG    if( nd /= nn ) stop 'acomm: AtomReduce: array dim differs!'
        call MPI_Recv( p, nn, PREC( R1_C2 ), irank, TAG, comm, istat, ierr ) cDBG ; call check( __LINE__ )

        q = q + p ! add

      enddo ! il
    else  ! owner
cDBG    call MPI_Send( nn, 1,   MPI_INTEGER, owner, GAT, comm, ierr ) cDBG ; call check( __LINE__ )
        call MPI_Send( q, nn, PREC( R1_C2 ), owner, TAG, comm, ierr ) cDBG ; call check( __LINE__ )
    endif ! owner
#else
    if( size(list) > 1 ) stop 'AtomReduce: LIST has more than 1 entry, but -D NOMPI aktive!'
#endif
  endsubroutine ! AtomReduce




#if R1_C2 == 1
  subroutine AtomBcast_r1( &
#else
  subroutine AtomBcast_c1( &
#endif
                           q, comm, owner, list )
cDBG use MPIconst, only: MPI_INTEGER
  use MPIconst, only: MPI_STATUS_SIZE, PREC ! PREC(1)=MPI_REAL, PREC(2)=MPI_COMPLEX
  use MPItools, only: MPImyrank
    REAPLEX, intent(inout)      :: q(1:) ! q(np) ! intent: owner:(in), others:(out)
    MPI_Comm, intent(in)        :: comm ! grid communicator
    integer, intent(in)         :: owner ! atom owner rank
    integer, intent(in)         :: list(:) ! listed processes (only at owner)

#ifndef NOMPI
    character(len=*), parameter :: fun = ' AtomBcast: '
    integer, parameter          :: TAG = 12
    integer                     :: nn, il, irank, nl, me, istat(MPI_STATUS_SIZE)
cDBG  integer, parameter        :: GAT = 18
cDBG  integer                   :: nd ! array size
cDBG  nd = size(q)

    me = MPImyrank( comm )
    nn = size(q) ! number of projectors
    nl = size(list,1) ! number of listed processes

    if( nl > 0 ) then ! owner
cDBG  if( me /= owner ) stop 'atomicomm: fatal ownership error, AtomBcast_1'
      ! send tasks
      do il = 1, nl ! for the complete list
        irank = list(il) ! rank of listed process
        if( irank == me ) cycle ! do not send to myself

cDBG    call MPI_Recv( nd, 2,   MPI_INTEGER, irank, GAT, comm, istat, ierr ) cDBG ; call check( __LINE__ )
cDBG    if( nd /= nn ) stop 'acomm: AtomBcast: array dim differs!'
        call MPI_Send( q, nn, PREC( R1_C2 ), irank, TAG, comm, ierr ) cDBG ; call check( __LINE__ )

      enddo ! il
    else  ! owner
cDBG    call MPI_Send( nd, 2,   MPI_INTEGER, owner, GAT, comm, ierr ) cDBG ; call check( __LINE__ )
        call MPI_Recv( q, nn, PREC( R1_C2 ), owner, TAG, comm, istat, ierr ) cDBG ; call check( __LINE__ )
    endif ! owner
#else
    if( size(list) > 1 ) stop 'AtomBcast: LIST has more than 1 entry, but -D NOMPI aktive!'
#endif
  endsubroutine ! AtomBcast






#if R1_C2 == 1
  subroutine AtomReduce_r2( &
#else
  subroutine AtomReduce_c2( &
#endif
                            q, comm, owner, list )
cDBG use MPIconst, only: MPI_INTEGER
  use MPIconst, only: MPI_STATUS_SIZE, PREC ! PREC(1)=MPI_REAL, PREC(2)=MPI_COMPLEX
  use MPItools, only: MPImyrank

    REAPLEX, intent(inout)      :: q(1:,1:) ! q(n1,n2)
    MPI_Comm, intent(in)        :: comm ! grid communicator
    integer, intent(in)         :: owner ! atom owner rank
    integer, intent(in)         :: list(:) ! listed processes (only at owner)
#ifndef NOMPI
    character(len=*), parameter :: fun = ' AtomReduce: '
    integer, parameter          :: TAG = 21
    integer                     :: n1, n2, nn, il, irank, nl, me, istat(MPI_STATUS_SIZE)
    REAPLEX                     :: p(1:size(q,1),1:size(q,2))
cDBG  integer, parameter        :: GAT =  9
cDBG  integer                   :: nd(2) ! array size
cDBG  nd = shape(q)

    me = MPImyrank( comm )
    n1 = size(q,1) ! number of projectors
    n2 = size(q,2) ! number of independent sets
    nn = n1 * n2 ! number of elements
    nl = size(list,1) ! number of listed processes

cDBG  if( nl == 1 ) then
cDBG
cDBG  endif ! nl == 1

    if( nl > 0 ) then ! owner
cDBG  if( me /= owner ) stop 'atomicomm: fatal ownership error, AtomReduce_2'
      ! receive tasks
      do il = 1, nl ! for the complete list
        irank = list(il) ! rank of listed process
        if( irank == me ) cycle ! do not recv from myself

cDBG    call MPI_Recv( nd, 2,   MPI_INTEGER, irank, GAT, comm, istat, ierr ) cDBG ; call check( __LINE__ )
cDBG    if( nd(1) /= n1 ) stop 'acomm: AtomReduce: array dim#1 differs!'
cDBG    if( nd(2) /= n2 ) stop 'acomm: AtomReduce: array dim#2 differs!'
        call MPI_Recv( p, nn, PREC( R1_C2 ), irank, TAG, comm, istat, ierr ) cDBG ; call check( __LINE__ )

        q = q + p ! add

      enddo ! il
    else  ! owner
cDBG    call MPI_Send( nd, 2,   MPI_INTEGER, owner, GAT, comm, ierr ) cDBG ; call check( __LINE__ )
        call MPI_Send( q, nn, PREC( R1_C2 ), owner, TAG, comm, ierr ) cDBG ; call check( __LINE__ )
    endif ! owner
#else
    if( size(list) > 1 ) stop 'AtomReduce: LIST has more than 1 entry, but -D NOMPI aktive!'
#endif
  endsubroutine ! AtomReduce






#if R1_C2 == 1
  subroutine AtomBcast_r2( &
#else
  subroutine AtomBcast_c2( &
#endif
                           q, comm, owner, list )
cDBG use MPIconst, only: MPI_INTEGER
  use MPIconst, only: MPI_STATUS_SIZE, PREC ! PREC(1)=MPI_REAL, PREC(2)=MPI_COMPLEX
  use MPItools, only: MPImyrank

    REAPLEX, intent(inout)      :: q(1:,1:) ! q(np,ni) ! intent: owner:(in), others:(out)
    MPI_Comm, intent(in)        :: comm ! grid communicator
    integer, intent(in)         :: owner ! atom owner rank
    integer, intent(in)         :: list(:) ! listed processes (only at owner)
#ifndef NOMPI
    character(len=*), parameter :: fun = ' AtomBcast: '
    integer, parameter          :: TAG = 22
    integer                     :: n1, n2, nn, il, irank, nl, me, istat(MPI_STATUS_SIZE)
cDBG  integer, parameter        :: GAT =  8
cDBG  integer                   :: nd(2) ! array size
cDBG  nd = shape(q)

    me = MPImyrank( comm )
    n1 = size(q,1) ! number of projectors
    n2 = size(q,2) ! number of independent sets
    nn = n1 * n2 ! number of elements
    nl = size(list,1) ! number of listed processes

cDBG  if( nl == 1 ) then
cDBG
cDBG  endif ! nl == 1

cFDB  write(*,'(9(A,I0))') __FILE__, __LINE__ , ' AtomBcast_2 start'

    if( nl > 0 ) then ! owner
cDBG  if( me /= owner ) stop 'atomicomm: fatal ownership error, AtomBcast_2'
      ! send tasks
      do il = 1, nl ! for the complete list
        irank = list(il) ! rank of listed process
        if( irank == me ) cycle ! do not send to myself

cFDB    write(*,'(9(A,I0))') __FILE__, __LINE__ , ' AtomBcast_2 sending to ', irank

cDBG    call MPI_Recv( nd, 2,   MPI_INTEGER, irank, GAT, comm, istat, ierr ) cDBG ; call check( __LINE__ )
cDBG    if( nd(1) /= n1 ) stop 'acomm: AtomBcast: array dim#1 differs!'
cDBG    if( nd(2) /= n2 ) stop 'acomm: AtomBcast: array dim#2 differs!'
        call MPI_Send( q, nn, PREC( R1_C2 ), irank, TAG, comm, ierr ) cDBG ; call check( __LINE__ )

      enddo ! il
    else  ! owner
cFDB    write(*,'(9(A,I0))') __FILE__, __LINE__ , ' AtomBcast_2 receiving from ', owner
cDBG    call MPI_Send( nd, 2,   MPI_INTEGER, owner, GAT, comm, ierr ) cDBG ; call check( __LINE__ )
        call MPI_Recv( q, nn, PREC( R1_C2 ), owner, TAG, comm, istat, ierr ) cDBG ; call check( __LINE__ )
    endif ! owner
#else
    if( size(list) > 1 ) stop 'AtomBcast: LIST has more than 1 entry, but -D NOMPI aktive!'
#endif
cFDB  write(*,'(A,I0,9A)') __FILE__, __LINE__ , ' AtomBcast_2 done'
  endsubroutine ! AtomBcast



#if R1_C2 == 2
! module tail


  status_t function atomic_comm_lists( ja, contribute, comm, owner, list ) result( ierr )
  use configuration, only: ERROR
  use MPIconst, only: MPI_LOGICAL
cDBG  use MPItools, only: operator(.MPIdiff.)
  use MPItools, only: MPImyrank, MPInprocs
    integer, intent(in)             :: ja !! global atom index
    logical, intent(in)             :: contribute
    MPI_Comm, intent(in)            :: comm ! grid communicator
    integer, intent(in)             :: owner !! atom owner rank
    integer, _allocatable_, intent(inout) :: list(:)

    character(len=*), parameter     :: fun = ' acl: '
    integer                         :: me, np, nc, ir, ic
    logical, allocatable            :: con(:)

    ierr = -1 ! result for early return

#ifdef DEBUG
    if( ja .MPIdiff. comm ) then
      if(o>0) write(o,'(4A,I0,A,Z8.8)') sym, fun, ERROR, 'global atom index differs for atom# ', ja, ' along comm=0x', comm
      return ! error
    endif ! we are not talking about the same atom
    if( owner .MPIdiff. comm ) then
      if(o>0) write(o,'(4A,2(I0,A),Z8.8)') sym, fun, ERROR, 'atom owner rank differs for atom# ', ja, ' owner= ', owner, ' along comm=0x', comm
      return ! error
    endif ! we are not talking about the same atom
!   if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'atom# ', ja, ' owner is rank# ', owner
#endif

    me = MPImyrank( comm ) ! my rank in this communicator
    np = MPInprocs( comm ) ! number of all processes in this communicator
cDBG  write(*,'(3A,3(I0,A),L2)') sym, fun, 'me=', me, ' atom #', ja, ' is owned by rank #', owner, ' allocated(list)[0]=', _allocated_(list)
    if( _allocated_( list ) ) deallocate( list, stat=ierr )
!   nullify( list )
cDBG  write(*,'(3A,3(I0,A),L2)') sym, fun, 'me=', me, ' atom #', ja, ' is owned by rank #', owner, ' allocated(list)[1]=', _allocated_(list)


    allocate( con(0:np-1), stat=ierr ) !; if( ierr /= 0 ) return ! error
    con = .false. ! init (not necessary)
    if( me == owner ) then
      ! with this method the owner does not need to be part of the list
      ! but it is of course still useful to keep the amount of communication low
      !allocate( con(0:np-1), stat=ierr ) !; if( ierr /= 0 ) return ! error
      !con = .false. ! init (not necessary)

    endif ! owner task

#ifndef NOMPI
    
    ! all processes in comm send their information about if they contribute to this atom to the atom owner
    call MPI_Gather( contribute, 1, MPI_LOGICAL, &
                     con       , 1, MPI_LOGICAL, & ! watch out, con is only allocated on the owner site
                     owner, comm, ierr )
#else
    con = contribute
#endif

    if( me == owner ) then
      ! determine the number of contributing processes
      
      nc = count( con )
cDBG  if( nc < 1 ) stop 'ATM acomm: fatal error: number of contributing processes < 1!'

      allocate( list(nc), stat=ierr )
cDBG  if( ierr /= 0 ) stop 'ATM: allocation of a%plist failed!'

      ic = 0 ! init counter
      do ir = 0, np-1
        if( con(ir) ) then
          ic = ic+1 ! count up
          list( ic ) = ir ! add rank of this contributing process to the table of processes
        endif ! con(ir)
      enddo ! ir

cDBG  if( ic /= nc ) stop 'ATM: fatal error: wrong count of .TRUE. entries in CON!'
      !deallocate( con, stat=ierr )
cDBG  ! show table of contribution process ranks, write to unit 6, so all processes show theirs
cDBG  write(*,'(3A,999(I0,A))') sym, fun, 'me=', me, ' list for atom #', ja, ' is  ', ( list(ic),' ', ic=1,nc )
    else  ! owner
cDBG  write(*,'(3A,9(I0,A))') sym, fun, 'me=', me, ' atom #', ja, ' is owned by rank #', owner
      allocate( list(0), stat=ierr )
    endif ! owner task
cDBG  write(*,'(3A,3(I0,A),L2)') sym, fun, 'me=', me, ' atom #', ja, ' is owned by rank #', owner, ' allocated(list)[2]=', _allocated_(list)
    deallocate( con, stat=ierr )
    ierr = 0 ! success
  endfunction ! atomic_comm_lists

  subroutine check( line )
    ! use, only: ierr
    integer, intent(in) :: line
    if( ierr /= 0 ) write(*,'(2A,(I0,A))') sym, 'ERROR: ierr = ', ierr, ' in ', __FILE__ , ' at line ', line
  endsubroutine ! check

#ifdef EXTENDED
!+ extended

  status_t function test( )
    write(*,*,iostat=test) __FILE__,' no module test implemented!'
  endfunction ! test

!- extended
#endif

endmodule ! atomicomm
#endif
#endif

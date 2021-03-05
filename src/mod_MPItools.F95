#ifdef DEBUG_ALL
#define DEBUG
#endif

! #define DEBUG

#ifdef DEBUG
#define cDBG
#else
#define cDBG !
#endif



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

!! @author Paul Baumeister
!! @version 3.0
!!
!! wrapper module to avoid lengthy and messy MPI calls at the top level
module MPItools
  use configuration, only: o ! output unit
  use MPIconst, only: PREC ! PREC(1): MPI_REAL, PREC(2): MPI_COMPLEX
  use MPIconst, only: MPI_INTEGER, MPI_MIN, MPI_MAX, MPI_SUM, MPI_CHARACTER
implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: sym = 'MPItools' !! module symbol

  !! functions with the only argument comm
  public :: MPInprocs ! integer(default=1)
  public :: MPImyrank ! integer(default=0)
  public :: MPImaster ! logical(default=T)
  public :: MPIparallel ! logical(default=F)
  !! subroutines with the only argument comm
  public :: MPIbarrier ! subroutine
  !! functions with the arguments a and comm
  public :: operator(.MPIsum.)
  public :: operator(.MPImax.)
  public :: operator(.MPIdiff.)
  public :: operator(.MPIaverage.)
  !! subroutines with the arguments a and comm
  public :: MPIallsum ! subroutine
  public :: MPIbcast0 ! subroutine
  public :: MPIreduce0 ! subroutine
  public :: MPIsendrecv ! function
#ifdef EXTENDED
  public :: test
#endif

  interface operator(.MPIsum.)
    ! real    = real    .MPIsum. communicator
    ! integer = integer .MPIsum. communicator
    ! complex = complex .MPIsum. communicator
    module procedure MPIsum_r0, MPIsum_i0, MPIsum_c0
  endinterface

  interface operator(.MPImax.)
    ! real    = real    .MPIsum. communicator
    ! integer = integer .MPImax. communicator
    module procedure MPImax_r0, MPImax_i0
  endinterface
  !!! .MPImin. is not supported, but a simple workaround is a = -( (-a) .MPImax. comm )

  interface operator(.MPIdiff.)
    ! logical = ... .MPIdiff. communicator
    module procedure MPIdiff_r0, MPIdiff_r1, &
                     MPIdiff_i0, MPIdiff_i1
  endinterface

  interface operator(.MPIaverage.)
    ! real = real    .MPIaverage. communicator
    ! real = integer .MPIaverage. communicator
    module procedure MPIaverage_r0, MPIaverage_i0
  endinterface

  interface MPIbcast0
    ! call MPIbcast0( ..., comm [, sender] )
    module procedure MPIbcast0_c1, MPIbcast0_c2, &
       MPIbcast0_r0, MPIbcast0_r1, MPIbcast0_r2, &
       MPIbcast0_i0, MPIbcast0_i1, &
       MPIbcast0_s0, MPIbcast0_s1
  endinterface

  interface MPIreduce0
    ! call MPIreduce0( ..., comm [, receiver] )
    module procedure MPIreduce0_c1, MPIreduce0_c2, &
                     MPIreduce0_r1, MPIreduce0_r2
  endinterface

  interface MPIallsum
    ! call MPIallsum( ..., comm )
    module procedure MPIallsum_r1, MPIallsum_r2, &
                     MPIallsum_r3, MPIallsum_r4, &
                     MPIallsum_c1, MPIallsum_c2, &
                     MPIallsum_c3, MPIallsum_c4
  endinterface

  interface MPIsendrecv
    ! ist = MPIsendrecv( a, rank, b [, rbnk], comm )
    module procedure MPIsendrecv_i1, &
     MPIsendrecv_r2, MPIsendrecv_c2
  endinterface

  integer, private :: ierr ! dummy

  contains

  integer function MPInprocs( comm ) result( nprocs )
    integer, intent(in) :: comm
#ifdef NOMPI
    nprocs = 1
#else
    call MPI_Comm_size( comm, nprocs, ierr )
cDBG  if( ierr /= 0 ) stop 'MPInprocs: failed!'
#endif
  endfunction ! MPInprocs


  integer function MPImyrank( comm ) result( myrank )
    integer, intent(in) :: comm
#ifdef NOMPI
    myrank = 0
#else
    call MPI_Comm_rank( comm, myrank, ierr )
cDBG  if( ierr /= 0 ) stop 'MPImyrank: failed!'
#endif
  endfunction ! MPImyrank


  logical function MPImaster( comm ) result( master )
    integer, intent(in) :: comm
    master = ( MPImyrank( comm ) == 0 )
  endfunction ! MPImaster


  logical function MPIparallel( comm ) result( parallel )
    integer, intent(in) :: comm
    parallel = ( MPInprocs( comm ) > 1 )
  endfunction ! MPIparallel

  subroutine MPIbarrier( comm )
    integer, intent(in) :: comm
#ifndef NOMPI
    call MPI_Barrier( comm, ierr )
cDBG  if( ierr /= 0 ) stop 'MPIbarrier: failed!'
#endif
  endsubroutine ! MPIbarrier


  integer function MPIsum_i0( a, comm ) result( aall )  ! this function sums a over all processes
    integer, intent(in)       :: a ! scalar integer
    integer, intent(in)       :: comm
    aall = a

#ifndef NOMPI
c1PR  if( MPInprocs( comm ) <= 1 ) return !
    call MPI_Allreduce( a, aall, 1, MPI_INTEGER, MPI_SUM, comm, ierr )
cDBG  if( ierr /= 0 ) stop 'MPIsum_i0: failed!'
#endif
  endfunction ! MPIsum


  subroutine MPIbcast0_s0( a, comm, sender )
    character(len=*), intent(inout) :: a ! string
    integer, intent(in)             :: comm
    integer, intent(in), optional   :: sender

#ifndef NOMPI
    integer :: l, isend
c1PR  if( MPInprocs( comm ) <= 1 ) return !
    isend = 0 ; if( present( sender ) ) isend = sender
    l = len( a )
    ! Broadcast operation: the process 0 sends to all contributors
    call MPI_Bcast( a, l, MPI_CHARACTER, isend, comm, ierr )
cDBG  if( ierr /= 0 ) stop 'MPIbcast0_s0: failed!'
#endif
  endsubroutine ! MPIbcast0

  subroutine MPIbcast0_s1( a, comm, sender )
    character(len=*), intent(inout) :: a(:) ! string
    integer, intent(in)             :: comm
    integer, intent(in), optional   :: sender

#ifndef NOMPI
    integer :: l, nn, isend
c1PR  if( MPInprocs( comm ) <= 1 ) return !
    isend = 0 ; if( present( sender ) ) isend = sender
    l = len( a ) ! string length
    nn = size( a, 1 )
    ! Broadcast operation: the process 0 sends to all contributors
    call MPI_Bcast( a, l*nn, MPI_CHARACTER, isend, comm, ierr )
cDBG  if( ierr /= 0 ) stop 'MPIbcast0_s1: failed!'
#endif
  endsubroutine ! MPIbcast0



! end of module head
#endif
! from here, everything will be compiled twice



#if R1_C2 == 1
#define REALINT real
#define PREC_RI PREC(1)
#else
#define REALINT integer
#define PREC_RI MPI_INTEGER
#endif



#if R1_C2 == 1
  REALINT function MPImax_r0( &
#else
  REALINT function MPImax_i0( &
#endif
                              a, comm ) result( aall )
  ! this function finds the global maximu along comm
    REALINT, intent(in) :: a ! scalar real/int
    integer, intent(in) :: comm
    aall = a
#ifndef NOMPI
c1PR  if( MPInprocs( comm ) <= 1 ) return !
    call MPI_Allreduce( a, aall, 1, PREC_RI , MPI_MAX, comm, ierr )
cDBG  if( ierr /= 0 ) stop 'MPImax: failed!'
#endif
  endfunction ! MPImax


#if R1_C2 == 1
  logical function MPIdiff_r0( &
#else
  logical function MPIdiff_i0( &
#endif
                               a, comm ) result( diff )
  ! this function tests, if a differs along comm
    REALINT, intent(in) :: a ! scalar real/int
    integer, intent(in) :: comm

    REALINT             :: amm(-1:+1)
    diff = .false. ! init as false
#ifndef NOMPI
c1PR  if( MPInprocs( comm ) <= 1 ) return ! false
    ! find the min and max
    call MPI_Allreduce( a, amm(-1), 1, PREC_RI , MPI_MIN, comm, ierr )
cDBG  if( ierr /= 0 ) stop 'MPIdiff: failed! (min)'
    call MPI_Allreduce( a, amm(+1), 1, PREC_RI , MPI_MAX, comm, ierr )
cDBG  if( ierr /= 0 ) stop 'MPIdiff: failed! (min)'
    diff = ( amm(-1) /= amm(+1) ) ! min deviates from max
    if( diff ) then
      amm(0) = a ! show my value
#if R1_C2 == 1
      if(o>0) write(o,'(A,3ES10.2)') 'MPIdiff: dev[min,my,max] =', amm
#else
      if(o>0) write(o,'(A,3I6    )') 'MPIdiff: dev[min,my,max] =', amm
#endif
    endif ! min deviates from max
#endif
  endfunction ! MPIdiff


#if R1_C2 == 1
  logical function MPIdiff_r1( &
#else
  logical function MPIdiff_i1( &
#endif
                               a, comm ) result( diff )
  ! this function tests, if the values of a differ along comm
    REALINT, intent(in)       :: a(:) ! 1dim real/int array
    integer, intent(in)       :: comm

    integer                   :: nprocs, ng(1)
    REALINT, allocatable      :: t(:,:) ! temporary
    real                      :: amm(-1:+1)
    diff = .false. ! init as false
#ifndef NOMPI
c1PR  if( MPInprocs( comm ) <= 1 ) return ! true

    ng = shape(a) ! number of values to compare
cDBG  if( differ( ng, comm, __LINE__ ) ) stop 'MPIdiff_ri1: processes do not agree in array shapes!'
    allocate( t(ng(1),-1:+1) )    ! get help array
    ! find the min and max
    call MPI_Allreduce( a, t(:,-1), ng(1), PREC_RI , MPI_MIN, comm, ierr )
cDBG  if( ierr /= 0 ) stop 'MPIdiff_1: failed! (min)'
    call MPI_Allreduce( a, t(:,+1), ng(1), PREC_RI , MPI_MAX, comm, ierr )
cDBG  if( ierr /= 0 ) stop 'MPIdiff_1: failed! (max)'
    ! generate the difference
    t(:,0) = t(:,+1) - t(:,-1)
    ! evaluate the error array t(:,0)
    amm(-1) = real( minval( t(:,0) ) )
    amm( 0) = real( sum   ( t(:,0) ) )/real( ng(1) ) ! average
    amm(+1) = real( maxval( t(:,0) ) )
    diff = ( amm(+1) > 0. )
    if( diff .and. o>0) write(o,'(2A,3ES10.2)') 'MPIdiff: dev[min,avg,max] =', amm(-1:+1)
#endif
  endfunction ! MPIdiff



#if R1_C2 == 1
  real function MPIaverage_r0( &
#else
  real function MPIaverage_i0( &
#endif
                               a, comm ) result( avg )
  ! this function tests, if a has the same value(s) among each process in comm
    REALINT, intent(in)       :: a ! scalar real/int
    integer, intent(in)       :: comm

    avg = real( a ) ! serial result
#ifndef NOMPI
c1PR  if( MPInprocs( comm ) <= 1 ) return !
    call MPI_Allreduce( real( a ), avg, 1, PREC(1) , MPI_SUM, comm, ierr )
cDBG  if( ierr /= 0 ) stop 'MPIaverage: failed!'
    avg = avg/real( MPInprocs( comm ) )
#endif
  endfunction ! MPIaverage


#if R1_C2 == 1
  subroutine MPIbcast0_r0( &
#else
  subroutine MPIbcast0_i0( &
#endif
                           a, comm, sender )
    REALINT, intent(inout)          :: a
    integer, intent(in)             :: comm
    integer, intent(in), optional   :: sender

#ifndef NOMPI
    integer :: isend
c1PR  if( MPInprocs( comm ) <= 1 ) return !
    isend = 0 ; if( present( sender ) ) isend = sender
    ! Broadcast operation: the process 0 sends to all contributors
    call MPI_Bcast( a, 1, PREC_RI , isend, comm, ierr )
cDBG  if( ierr /= 0 ) stop 'MPIbcast0: failed!'
cDBG  if( a .MPIdiff. comm ) stop 'MPIbcast0_ri0: fatal error: after Bcast, arrays are not the same.'
#endif
  endsubroutine ! MPIbcast0



#undef REALINT
#undef PREC_RI


#if R1_C2 == 1
#define REAPLEX real
! use PREC(1) for the MPIcalls
#else
#define REAPLEX complex
! use PREC(2) for the MPIcalls
#endif



#if R1_C2 == 1
  subroutine MPIreduce0_r1( &
#else
  subroutine MPIreduce0_c1( &
#endif
                            a, comm, receiver )
    REAPLEX, intent(inout)          :: a(:)
    integer, intent(in)             :: comm
    integer, intent(in), optional   :: receiver

#ifndef NOMPI
    integer                   :: ng(1), nn, irecv
    REAPLEX                   :: a2(size(a))
c1PR  if( MPInprocs( comm ) <= 1 ) return !
    irecv = 0 ; if( present( receiver ) ) irecv = receiver
    ng = shape( a )
cDBG  if( differ( ng, comm, __LINE__ ) ) stop 'MPIreduce0_ri1: processes do not agree in array shapes!'
    nn = product(ng)
    ! Reduction operation: All processes sum values but the result is only stored in rank 0
    call MPI_Reduce( a, a2, nn, PREC( R1_C2 ), MPI_SUM, irecv, comm, ierr )
    if( MPImaster( comm ) ) a = a2 ! overwrite
cDBG  if( ierr /= 0 ) stop 'MPIreduce0_1: failed!'
#endif
  endsubroutine ! MPIreduce0




#if R1_C2 == 1
  subroutine MPIreduce0_r2( &
#else
  subroutine MPIreduce0_c2( &
#endif
                            a, comm, receiver )
    REAPLEX, intent(inout)          :: a(:,:)
    integer, intent(in)             :: comm
    integer, intent(in), optional   :: receiver

#ifndef NOMPI
    integer                   :: ng(2), nn, irecv
    REAPLEX                   :: a2(size(a,1),size(a,2))
c1PR  if( MPInprocs( comm ) <= 1 ) return !
    irecv = 0 ; if( present( receiver ) ) irecv = receiver
    ng = shape( a )
cDBG  if( differ( ng, comm, __LINE__ ) ) stop 'MPIreduce0_r2: processes do not agree in array shapes!'
    nn = product(ng)
    ! Reduction operation: All processes sum values but the result is only stored in rank 0
    call MPI_Reduce( a, a2, nn, PREC( R1_C2 ), MPI_SUM, irecv, comm, ierr )
    if( MPImaster( comm ) ) a = a2 ! overwrite
cDBG  if( ierr /= 0 ) stop 'MPIreduce0_2: failed!'
#endif
  endsubroutine ! MPIreduce0





#if R1_C2 == 1
  subroutine MPIbcast0_r2( &
#else
  subroutine MPIbcast0_c2( &
#endif
                           a, comm, sender )
    REAPLEX, intent(inout)          :: a(:,:)
    integer, intent(in)             :: comm
    integer, intent(in), optional   :: sender

#ifndef NOMPI
    integer                   :: ng(2), nn, isend
c1PR  if( MPInprocs( comm ) <= 1 ) return !
    isend = 0 ; if( present( sender ) ) isend = sender
    ng = shape( a )
cDBG  if( differ( ng, comm, __LINE__ ) ) then
cDBG    write(*,'(2A,9(I0,A))') sym, ' MPIbcast0_2: ng(1)=',ng(1),' ng(2)=',ng(2)
cDBG    stop 'MPIbcast0_2: processes do not agree in array shapes!'
cDBG  endif ! differ
    nn = product(ng)
    ! Broadcast operation: the process 0 sends to all contributors
    call MPI_Bcast( a, nn, PREC( R1_C2 ), isend, comm, ierr )
cDBG  if( ierr /= 0 ) stop 'MPIbcast0_2: failed!'
#if R1_C2 == 1
cDBG  if( reshape(a,(/nn/)) .MPIdiff. comm ) stop 'MPIbcast0_2: fatal error: after Bcast, arrays are not the same.'
#endif
#endif
  endsubroutine ! MPIbcast0


#if R1_C2 == 1
  subroutine MPIbcast0_r1( &
#else
  subroutine MPIbcast0_c1( &
#endif
                           a, comm, sender )
    REAPLEX, intent(inout)          :: a(:)
    integer, intent(in)             :: comm
    integer, intent(in), optional   :: sender

#ifndef NOMPI
    integer                   :: ng(1), nn, isend
c1PR  if( MPInprocs( comm ) <= 1 ) return !
    isend = 0 ; if( present( sender ) ) isend = sender

    ng = shape( a )
cDBG  if( differ( ng, comm, __LINE__ ) ) stop 'MPIbcast0_1: processes do not agree in array shapes!'
    nn = product(ng)
    ! Broadcast operation: the process 0 sends to all contributors
    call MPI_Bcast( a, nn, PREC( R1_C2 ), isend, comm, ierr )
cDBG  if( ierr /= 0 ) stop 'MPIbcast0_1: failed!'
#if R1_C2 == 1
cDBG  if( reshape(a,(/nn/)) .MPIdiff. comm ) stop 'MPIbcast0_1: fatal error: after Bcast, arrays are not the same.'
#endif
#endif
  endsubroutine ! MPIbcast0



#if R1_C2 == 1
  REAPLEX function MPIsum_r0( &  ! this function sums a over all processes
#else
  REAPLEX function MPIsum_c0( &  ! this function sums a over all processes
#endif
                              a, comm ) result( aall )
    REAPLEX, intent(in) :: a ! scalar real/complex
    integer, intent(in) :: comm
    aall = a
#ifndef NOMPI
c1PR  if( MPInprocs( comm ) <= 1 ) return !
    call MPI_Allreduce( a, aall, 1, PREC( R1_C2 ), MPI_SUM, comm, ierr )
cDBG  if( ierr /= 0 ) stop 'MPIsum: failed!'
#endif
  endfunction ! MPIsum



#if R1_C2 == 1
  subroutine MPIallsum_r1( &
#else
  subroutine MPIallsum_c1( &
#endif
                           a, comm )
    REAPLEX, intent(inout)    :: a(:)
    integer, intent(in)       :: comm

#ifndef NOMPI
    REAPLEX, allocatable      :: aall(:)
    integer                   :: ng(1), nn, ist  
c1PR  if( MPInprocs( comm ) <= 1 ) return !

    ng = shape( a )
cDBG  if( differ( ng, comm, __LINE__ ) ) stop 'MPIallsum_rc1: processes do not agree in array shapes!'
    nn = product(ng)
    allocate( aall(ng(1)), stat=ist )
    if( ist /= 0 ) stop 'MPItools: MPIallsum_1 failed to allocate memory.'
    call MPI_Allreduce( a, aall, nn, PREC( R1_C2 ), MPI_SUM, comm, ierr )
cDBG  if( ierr /= 0 ) stop 'MPIallsum_1: failed!'
    a = aall ! overwrite
#endif
  endsubroutine ! MPIallsum


#if R1_C2 == 1
  subroutine MPIallsum_r2( &
#else
  subroutine MPIallsum_c2( &
#endif
                           a, comm )
    REAPLEX, intent(inout)    :: a(:,:)
    integer, intent(in)       :: comm

    REAPLEX, allocatable      :: aall(:,:)
    integer                   :: ng(2), nn, ist   
#ifndef NOMPI
c1PR  if( MPInprocs( comm ) <= 1 ) return !

    ng = shape( a )
cDBG  if( differ( ng, comm, __LINE__ ) ) stop 'MPIallsum_rc2: processes do not agree in array shapes!'
    nn = product(ng)
    allocate( aall(ng(1),ng(2)), stat=ist )
    if( ist /= 0 ) stop 'MPItools: MPIallsum_2 failed to allocate memory.'
    call MPI_Allreduce( a, aall, nn, PREC( R1_C2 ), MPI_SUM, comm, ierr )
cDBG  if( ierr /= 0 ) stop 'MPIallsum_2: failed!'
    a = aall ! overwrite
#endif
  endsubroutine ! MPIallsum


#if R1_C2 == 1
  subroutine MPIallsum_r3( &
#else
  subroutine MPIallsum_c3( &
#endif
                           a, comm )
    REAPLEX, intent(inout)    :: a(:,:,:)
    integer, intent(in)       :: comm

    REAPLEX, allocatable      :: aall(:,:,:)
    integer                   :: ng(3), nn, ist  
#ifndef NOMPI
c1PR  if( MPInprocs( comm ) <= 1 ) return !

    ng = shape( a )
cDBG  if( differ( ng, comm, __LINE__ ) ) stop 'MPIallsum_rc3: processes do not agree in array shapes!'
    nn = product(ng)
    allocate( aall(ng(1),ng(2),ng(3)), stat=ist )
    if( ist /= 0 ) stop 'MPItools: MPIallsum_3 failed to allocate memory.'
    call MPI_Allreduce( a, aall, nn, PREC( R1_C2 ), MPI_SUM, comm, ierr )
cDBG  if( ierr /= 0 ) stop 'MPIallsum_3: failed!'
    a = aall ! overwrite
#endif
  endsubroutine ! MPIallsum


#if R1_C2 == 1
  subroutine MPIallsum_r4( &
#else
  subroutine MPIallsum_c4( &
#endif
                           a, comm )
    REAPLEX, intent(inout)    :: a(:,:,:,:)
    integer, intent(in)       :: comm

    REAPLEX, allocatable      :: aall(:,:,:,:)
    integer                   :: ng(4), nn, ist  
#ifndef NOMPI
c1PR  if( MPInprocs( comm ) <= 1 ) return !

    ng = shape( a )
cDBG  if( differ( ng, comm, __LINE__ ) ) stop 'MPIallsum_r4: processes do not agree in array shapes!'
    nn = product(ng)
    allocate( aall(ng(1),ng(2),ng(3),ng(4)), stat=ist )
    if( ist /= 0 ) stop 'MPItools: MPIallsum_4 failed to allocate memory.'
    call MPI_Allreduce( a, aall, nn, PREC( R1_C2 ), MPI_SUM, comm, ierr )
cDBG  if( ierr /= 0 ) stop 'MPIallsum_4: failed!'
    a = aall ! overwrite
#endif
  endsubroutine ! MPIallsum




#if R1_C2 == 1
  integer function MPIsendrecv_r2( &
#else
  integer function MPIsendrecv_c2( &
#endif
      a, rank, b, from, comm ) result( ist )
  use MPIconst, only: MPI_STATUS_SIZE
    REAPLEX, intent(in)             :: a(:,:)
    integer, intent(in)             :: rank
    REAPLEX, intent(out)            :: b(:,:)
    integer, intent(in), optional   :: from
    integer, intent(in)             :: comm

    integer, parameter              :: ND = 2 ! use as MPI tag
    integer                         :: rbnk, istat(MPI_STATUS_SIZE)
    integer                         :: na, nb
!     integer                         :: ma(ND), mb(ND)
cDBG  integer                         :: ng ! number of elements this process will get from rbnk
    ist = 0 ! result
#ifndef NOMPI
    rbnk = rank ; if( present(from) ) rbnk = from
c1PR
!     ma = shape( a ) ; na = product(ma) ; mb = shape( b ) ; nb = product(mb)
    na = size(a) ; nb = size(b)
cDBG  call MPI_sendrecv( na, 1, MPI_INTEGER, rank, 0, ng, 1, MPI_INTEGER, rbnk, 0, comm, istat, ist )
cDBG  if( ng /= nb ) stop 'MPIsendrecv_2: ERROR: sizes deviate!'
    call MPI_sendrecv( a, na, PREC( R1_C2 ), rank, ND, &
                       b, nb, PREC( R1_C2 ), rbnk, ND, &
                       comm, istat, ist )
#else
    b = a
#endif
  endfunction ! MPIsendrecv



! until here, everything will be compiled twice
#if R1_C2 == 2
! module tail
#undef REAPLEX


  integer function MPIsendrecv_i1( a, rank, b, from, comm ) result( ist )
  use MPIconst, only: MPI_STATUS_SIZE
    integer, intent(in)             :: a(:)
    integer, intent(in)             :: rank
    integer, intent(out)            :: b(:)
    integer, intent(in), optional   :: from
    integer, intent(in)             :: comm

    integer, parameter              :: ND = 1 ! number of dimensions
    integer                         :: rbnk, istat(MPI_STATUS_SIZE)
    integer                         :: na, nb
!     integer                         :: ma(ND), mb(ND)
cDBG  integer                         :: ng ! number of elements this process will get from rbnk
    ist = 0 ! result
#ifndef NOMPI
    rbnk = rank ; if( present(from) ) rbnk = from
c1PR
!     ma = shape( a ) ; na = product(ma) ; mb = shape( b ) ; nb = product(mb)
    na = size(a) ; nb = size(b)
cDBG  call MPI_sendrecv( na, 1, MPI_INTEGER, rank, 0, ng, 1, MPI_INTEGER, rbnk, 0, comm, istat, ist )
cDBG  if( ng /= nb ) stop 'MPIsendrecv_i1: ERROR: sizes deviate!'
    call MPI_sendrecv( a, na, MPI_INTEGER, rank, ND, &
                       b, nb, MPI_INTEGER, rbnk, ND, &
                       comm, istat, ist )
#else
    b = a
#endif
  endfunction ! MPIsendrecv


  subroutine MPIbcast0_i1( a, comm, sender )
    integer, intent(inout)          :: a(:)
    integer, intent(in)             :: comm
    integer, intent(in), optional   :: sender

#ifndef NOMPI
    integer :: ng(1), nn, isend
c1PR  if( MPInprocs( comm ) <= 1 ) return !
    isend = 0 ; if( present( sender ) ) isend = sender
    ng = shape( a )
cDBG  if( differ( ng, comm, __LINE__ ) ) stop 'MPIbcast0_ri1: processes do not agree in array shapes!'
    nn = product(ng)
    ! Broadcast operation: the process 0 sends to all contributors
    call MPI_Bcast( a, nn, MPI_INTEGER, isend, comm, ierr )
cDBG  if( ierr /= 0 ) stop 'MPIbcast0_1: failed!'
cDBG  if( a .MPIdiff. comm ) stop 'MPIbcast0_ri1: fatal error: after Bcast, arrays are not the same.'
#endif
  endsubroutine ! MPIbcast0



  logical function differ( dims, comm, line )
    integer, intent(in) :: dims(:), comm, line

#ifdef DEBUG
    integer                   :: nd(2), na(2), ndims!, ierr
    integer, allocatable      :: t(:,:,:)
#endif
    differ = .false.
#ifdef DEBUG
#ifndef NOMPI
c1PR  if( MPInprocs( comm ) <= 1 ) return ! false
    ndims = size(dims,1) ! number of integer values to compare
    nd(1:2) = (/-ndims,+ndims/)
    ! find the max
    call MPI_Allreduce( nd(1:2), na(1:2), 2, MPI_INTEGER, MPI_MAX, comm, ierr )
cDBG  if( ierr /= 0 ) stop 'MPI differ: failed! (1)'
    if( na(1) + na(2) /= 0 ) then
      write(*,*) __FILE__ , __LINE__ , 'try to test different numbers of dimensions, calling line was', line
      stop 'MPI differ: try to test different numbers of dimensions!'
    endif

    allocate( t(ndims,2,2), stat=ist ) ; if( ist /= 0 ) stop 'MPItools: differ failed to allocate memory!'
   
    t(:,1,1) = -dims(:) ! fill the array
    t(:,2,1) = +dims(:) ! fill the array

    call MPI_Allreduce( t(:,:,1), t(:,:,2), 2*ndims, MPI_INTEGER, MPI_MAX, comm, ierr ) ! find the max
cDBG  if( ierr /= 0 ) stop 'MPI differ: failed! (2)'
    differ = any( t(:,1,2)+t(:,2,2) /= 0 ) ! generate the difference
#endif
#endif
  endfunction ! differ

#ifdef EXTENDED
!+ extended

  integer function test( )
    write(*,*,iostat=test) __FILE__,' no module test implemented!'
  endfunction ! test

!- extended
#endif

endmodule ! MPItools
#endif
#endif

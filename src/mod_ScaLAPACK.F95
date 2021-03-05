
#ifdef DEBUG_ALL
#define DEBUG
#endif

! #define DEBUG
! #define TESTS


#ifdef DEBUG
#define cDBG
#else
#define cDBG !DBG
#endif

#ifdef TIME
#define cTIM
#else
#define cTIM !TIM
#endif



#ifndef BLOCKSIZE
#define BLOCKSIZE 32
! #error 'mod_ScaLAPACK.F95: ScaLAPACK needs a (positive) BLOCKSIZE'
#endif

#ifdef SINGLE_PRECISION
!!! replace the ScaLapack routine names
#define PDSYGVX pssygvx
#define PZHEGVX pchegvx

#endif



#ifdef R1_C2
! This file will be preprocessed twice, first
! with R1_C2 == 1, and then with R1_C2 == 2
! then the two parts are concatenated to one source.
! This requires that the module head has to be
! appearing, when R1_C2 == 1 and the line "end module <name>"
! with R1_C2 == 2.

! begin double preprocessing
#if R1_C2 == 1
! begin module head

!! @author Paul Baumeister and Shigeru Tsukamoto
!! @version 4.10
!!
!! wrapper module for the usage of ScaLAPACK
!! works in 3 steps: set, solve, get
module ScaLAPACK
  use configuration, only: o
implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: sym = 'Sca' !! module symbol
  character(len=*), parameter, private :: fun = ': ' !! for production

  ! public
  public :: init_matrix !! init and allocate
  public :: set_sym_matrix_entry
  public :: set_matrix_entry
  public :: solve_matrix !! solve
  public :: get_block_size
  public :: get_matrix_block
  public :: free_matrix !! dealocate and invalidate the matrices
  public :: test  !! compare ScaLAPACK and LAPACK for a toyHamiltonian

  public :: get_matrix
  public :: set_sym_entries
  public :: set_entry

  integer, parameter, public  :: ROW = 1
  integer, parameter, public  :: COL = 2
  integer, parameter, public  :: BLOCK_SIZE(ROW:COL) = BLOCKSIZE

  ! interfaces
  interface init_matrix
    module procedure step1_r, step1_c
  endinterface

  interface solve_matrix
    module procedure step2_r, step2_c
  endinterface

  interface set_matrix_entry
    module procedure set_matrix_entry_r, set_matrix_entry_c
  endinterface

  interface set_sym_matrix_entry
    module procedure set_symmetric_matrix_entry_r, set_hermitian_matrix_entry_c
  endinterface

  interface get_matrix_block
    module procedure get_matrix_block_r, get_matrix_block_c
  endinterface

  interface free_matrix
    module procedure free_matrix_r, free_matrix_c
  endinterface



  interface set_entry
    module procedure set_entry_r, set_entry_c
  endinterface

  interface set_sym_entries
    module procedure set_symmetric_entry_r, set_hermitian_entry_c
  endinterface

  interface get_matrix
    module procedure get_matrix_r, get_matrix_c
  endinterface


  interface operator( .div. ) !! ceiling integer divide
    module procedure divide_ii
  endinterface

  integer, parameter    :: MEM_MULT = 8 ! memory multiplyer
  real, parameter       :: ORFAC = 1E-6 ! orthogonality of eigenvectors


  integer, parameter :: ISTATUS_NOT_INITIALIZED = -9
  integer, parameter :: ISTATUS_GRID_DETERMINED = -8
  integer, parameter :: ISTATUS_SIZE_DETERMINED = -7
  integer, parameter :: ISTATUS_ARRAY_ALLOCATED = -6
  integer, parameter :: ISTATUS_ENTRIES_CLEARED = -5
  integer, parameter :: ISTATUS_RECEIVE_ENTRIES = -4
  integer, parameter :: ISTATUS_MATRIX_VERIFIED = -2
  integer, parameter :: ISTATUS_PROBLEMS_SOLVED =  0
  integer, parameter :: ISTATUS_NO_LAPACK_FOUND = 33
  !==================================================

  integer, parameter :: DESCR_SIZE = 9

  type, public :: ScaLinfo
    integer :: istatus = ISTATUS_NOT_INITIALIZED !< Status indicator
    integer :: Ndim = 0                          !< Dimension of square matrix
    integer :: ngrid(ROW:COL) = 1                !< Number of processe grids in row and column
    integer :: BS(ROW:COL) = BLOCK_SIZE          !< BLACS matrix block size in row and column
    integer :: nsize(ROW:COL) = 0                !< Boundaries (allocated size) of the local array
    integer :: irank(ROW:COL) = 0                !< MPI process rank on the BLACS process grid
    integer :: nreloads = 0
    integer :: icontext                          !< BLACS grid context (handle)
    integer :: descr(DESCR_SIZE)                 !< BLACS matrix descriptor
  endtype ScaLinfo

  type(ScaLinfo), save, protected :: si

contains


  !-+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0
  !--
  !> @brief Fortran function for stringificate ScaLinfo array of status variables
  character(len=16) function to_String( info ) result( S )
  implicit none
    type(ScaLinfo), intent(in) :: info !< array of status variables
    selectcase( info%istatus )
    case( ISTATUS_NOT_INITIALIZED ) ; S = 'not initialized'
    case( ISTATUS_GRID_DETERMINED ) ; S = 'grid determined'
    case( ISTATUS_SIZE_DETERMINED ) ; S = 'size determined'
    case( ISTATUS_ARRAY_ALLOCATED ) ; S = 'array allocated'
    case( ISTATUS_ENTRIES_CLEARED ) ; S = 'entries cleared'
    case( ISTATUS_RECEIVE_ENTRIES ) ; S = 'receive entries'
    case( ISTATUS_MATRIX_VERIFIED ) ; S = 'matrix verified'
    case( ISTATUS_PROBLEMS_SOLVED ) ; S = 'problems solved'
    case( ISTATUS_NO_LAPACK_FOUND ) ; S = 'no LAPACK found'
    case default ; write(unit=S,fmt='(A,I6)') '? info=', info%istatus
    endselect ! ist
  endfunction to_String
  !--
  !-+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0


  !-+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0
  !--
  !> @brief Fortran function for returning either row- or column-block size of the BLACS matrix.
  integer function get_block_size( irc ) result( BS )
  implicit none
    integer, intent(in) :: irc !< Only ROW(=1) or COL(=2) may be specified
    selectcase( irc )
    case( ROW ) ; BS = si%BS(ROW)
    case( COL ) ; BS = si%BS(COL)
    case default ; BS = 0
      stop 'Sca fatal error: get_block_size needs arguments 1:ROW or 2:COL!'
    endselect ! irc
  endfunction
  !--
  !-+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0


  !-+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0
  !--
  !> @brief Fortran function for determining a BLACS process grid.
  !> @details This function determines a BLACS process grid, which is used for solving a generalized
  !>          eigenvalue problem on a parallel computing cluster using ScaLAPACK libraries. Total
  !>          number of process available is obtained from the MPI communicator comm, and the numbers
  !>          of row and column processes are automatically determined so as to be an identical number
  !>          and form a square process grid.
  integer function determine_grid( comm ) result( ist )
  use configuration, only: WARNING
  use MPIconst, only: MPI_COMM_WORLD, MPI_INTEGER
  use MPItools, only: MPInprocs, MPImyrank, MPIbcast0, MPImaster
  implicit none
    ! parameters
cDBG  character(len=*), parameter :: fun = ' determine_grid: '
    ! arguments
    integer, intent(in) :: comm !< MPI communicator included in a BLACS process grid
    ! local vars
    integer :: np(ROW:COL) !! Number of processes on the grid
    integer :: ioff, icr, iwr, i, iam, nprocs, LIM
    integer, allocatable :: usermap(:)

    ist = 0

cDBG  if(o>0) write(o,'(9A)') sym, fun, 'Module Status = ', to_String( si )
cDBG  if( si%istatus > ISTATUS_NOT_INITIALIZED .and. o>0 ) &
cDBG    write(o,'(9A)') sym, fun, 'BLACS grid has been initialized before!'
    if( si%istatus > ISTATUS_NOT_INITIALIZED ) return ! 0

#ifndef NOScaLAPACK
    nprocs = MPInprocs( comm ) ! maximum number of processors
cDBG  if(o>0) write(o,'(3A,I6,9A)') sym, fun, 'Start MPI with', nprocs, ' processes!'
    np = floor( sqrt( nprocs+0.01 ) ) ! +0.01 safety
    LIM = 64 ! because 64x64 can be 16x16x16 in real-space
    ! on massively parallel computers, there are often way more processes
    ! running than the number of processes that would be efficient for
    ! ScaLAPACK, therefore we limit the number of processes in a BLACS grid
    if(any(np>LIM).and.o>0) write(o,'(3A,9(I0,A))') sym, fun, &
      'largest possible square BLACS grid is ',np(1),' x ',np(2),', limit to ',LIM,' x ',LIM
    np = min(max(1,np),LIM)
cDBG  if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'Init ScaLAPACK with ',np(ROW),' x ',np(COL),' processes!'
cDBG  if(o>0) write(o,'(3A,Z8.8,9(A,I0))') sym, fun, 'comm= 0x', comm, '  np= ',nprocs
#else
    nprocs = 1 ! can only run in serial
    np     = 1
cDBG  if(o>0) write(o,'(3A,9(I3,A))') sym, fun, 'Init LAPACK (serial)!'
#endif

    iwr = MPImyrank( MPI_COMM_WORLD ) ! find the WORLD rank of this process
    icr = MPImyrank( comm ) ! find the rank w.r.t. comm

    ioff = -1 ! init as -1:illegal rank value
    if( icr == 0 ) ioff = iwr ! master of comm sets ioff to his WORLD rank
    call MPIbcast0( ioff, comm ) ! master of comm sends his WORLD rank to all others in comm
    if( ioff == -1 ) stop 'ScaLAPACK fatal: process was excluded in Bcast!' ! some ioff was still illegal

    ! check, if the sub comm is a direct subcommunicator of the WORLD comm,
    ! i.e. the processes must be direct neighbors in both communicators for a fast communication.
    if( iwr-ioff /= icr ) stop 'ScaLAPACK fatal: needs a direct sub_comm of MPI_COMM_WORLD.'

    ! it is assumed, that only np(ROW) x np(COL) processes running will
    ! contribute to ScaLAPACK, hence and a new grid is set up
    ! starting at each master process of the sub comm
    allocate( usermap(np(ROW)*np(COL)), stat=i )
    usermap = (/ (ioff+i, i=0,np(ROW)*np(COL)-1) /)
cDBG  if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'usermap = ranks ', usermap(1), ' through ', usermap(np(ROW)*np(COL))

#ifndef NOScaLAPACK
    ! set up the BLACS process grid
    call BLACS_PINFO( iam, nprocs ) ! get default information
cDBG  if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'BLACS_PINFO: iam=',iam,' nprocs=',nprocs
    ! if in PVM, create a virtual machine if it does not exist
    if( nprocs < 1 ) then
      if( iam == 0 ) nprocs = np(ROW)*np(COL)
      call BLACS_SETUP( iam, nprocs )
    endif ! virtual machine
    call BLACS_GET( -1, 0, si%icontext ) ! create a default BLACS context
    call BLACS_GRIDMAP( si%icontext, usermap, np(ROW), np(ROW), np(COL) ) ! initialize the BLACS grid. arg #3: leading dim. of usermap
cDBG  if(o>0) write(o,'(3A,Z8.8)') sym, fun, 'BLACS_GRIDMAP: icontext= 0x', si%icontext
#else
    si%icontext = 0
#endif
    si%istatus = ISTATUS_GRID_DETERMINED ! modify the module status
cDBG  if(o>0) write(o,'(9A)') sym, fun, 'Module Status = ', to_String( si )

  endfunction determine_grid
  !--
  !-+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0


  !-+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0
  !--
  !-- function determine_size( N, comm )
  !> @brief Fortran function for setting Ndim, nsize, and descr of the global variable si.
  !--
  integer function determine_size( N, comm ) result( ist )
  use configuration, only: WARNING, ERROR
  use MPIconst, only: Wtime
  use MPItools, only: MPIbcast0
  implicit none
    ! parameters
cDBG  character(len=*), parameter :: fun = ' determine_size: '
    character(len=*), parameter :: toString(ROW:COL) = (/'ROW','COL'/)
    ! arguments
    integer, intent(in) :: N    !< Dimension of the global square matrix
    integer, intent(in) :: comm !< MPI subcommunicator including the square matrix
    ! local vars
    integer :: mb(1:2)=1, mn(1:2)=1, info

cDBG  if(o>0) write(o,'(9A)') sym, fun, 'Module Status = ', to_String( si )
    ist = determine_grid( comm ) ! initialize the BLACS grid

    if( si%istatus < ISTATUS_GRID_DETERMINED ) then
cDBG      if(o>0) write(o,'(9A)') sym, fun, ERROR, 'must initialize BLACS grid before!'
      stop 'Sca: must initialize BLACS grid before!'
    endif

!cDBG  if(o>0) write(o,'(9A)') sym, fun, 'Module Status = ', to_String( si )
!    if( si%istatus > ISTATUS_SIZE_DETERMINED ) then
!cDBG  if(o>0) write(o,'(9A)') sym, fun, 'matrix sizes have been determined before!'
!      return
!    endif

#ifndef NOScaLAPACK
    call BLACS_GRIDINFO( si%icontext, si%ngrid(ROW), si%ngrid(COL), si%irank(ROW), si%irank(COL) )

    ! the master tells all processes grid processes
    ! the size of the BLACS grid, so they can compute correct ranks
    call MPIbcast0( si%ngrid, comm )
#endif


    ! set module variables
    !================================================================================
    si%Ndim = max(0,N) !! dimension
cDBG  if(o>0) write(o,'(3A,I8,9A)') sym, fun, 'matrix dimension     N =', si%Ndim

cDBG  if(o>0) write(o,'(3A,2I8,9A)') sym, fun, 'max. # of all blocks   =', N .div. si%BS
    mb = si%Ndim .div. (si%BS*si%ngrid) ! max. number of local blocks
cDBG  if(o>0) write(o,'(3A,2I8,9A)') sym, fun, 'max. # of local blocks =', mb
    mn = mb * si%BS ! max. number of indices
cDBG  if(o>0) write(o,'(3A,2I8,9A)') sym, fun, 'max. local array dims  =', mn

    ! no memory is saved in the higher ranks, where less blocks or even fractions
    ! of blocks are; for simplicity all processes allocate the max. # of elements
    ! except for those processes outside of the BLACS grid
    ! but the two arrays aa and bb should be allocated, at least with 1
    if( any( si%irank < 0 ) ) mn = 1
    si%nsize = min(max(1,mn),si%Ndim) ! because leading dimension 0 is illegal in the grid descriptor
    !================================================================================

cDBG  if(o>0) write(o,'(9A)') sym, fun, 'Module Status = ', to_String( si )
    si%istatus = ISTATUS_SIZE_DETERMINED
cDBG  if(o>0) write(o,'(9A)') sym, fun, 'Module Status = ', to_String( si )

    si%descr = 0
#ifndef NOScaLAPACK
    if( any( si%irank < 0 ) ) return ! process rank outside the BLACS grid
    ! BLACS grid processes generate ScaLAPACK grid descriptors
    call DESCINIT( si%descr, si%Ndim, si%Ndim, si%BS(ROW), si%BS(COL), 0, 0, si%icontext, si%nsize(ROW), info )
#endif
cDBG  if(o>0) write(o,'(9A)') sym, fun, 'Module Status = ', to_String( si )

  endfunction determine_size
  !--
  !-+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0


! end of module head
#endif

#if R1_C2 == 1
#define REAPLEX real
#else
#define REAPLEX complex
#endif


#if R1_C2 == 1
  !! setter function for the distributed matrices AA and BB
  integer function set_symmetric_matrix_entry_r( &
#else
  integer function set_hermitian_matrix_entry_c( &
#endif
                                     a1, b1, j, aa, bb ) result( n012 )
  implicit none
    ! parameters
cDBG  character(len=*), parameter :: fun = ' set_s/h_matrix_entry: '
    ! arguments
    REAPLEX, intent(in) :: a1, b1 !! Hamiltonian and Overlap matrix element
    REAPLEX, intent(inout) :: aa(:,:), bb(:,:) !! Hamiltonian and Overlap matrix
    integer, intent(in) :: j(ROW:COL) !! global indices
    integer             :: l(ROW:COL) !! global indices

    if( j(ROW) == j(COL) ) then ! diagonal elements
#if R1_C2 == 1
      n012 = set_matrix_entry( a1, b1, j, aa, bb )
#else
      n012 = set_matrix_entry( cmplx(real(a1)), cmplx(real(b1)), j, aa, bb )
#endif
    else  ! diagonal elements
      n012 = set_matrix_entry( a1, b1, j, aa, bb )
      l(ROW:COL) = j(COL:ROW:-1) ! interchange
#if R1_C2 == 1
      n012 = n012 + set_matrix_entry( a1, b1, l, aa, bb )
#else
      n012 = n012 + set_matrix_entry( conjg(a1), conjg(b1), l, aa, bb )
#endif
      ! now n01 should be 2
    endif ! diagonal elements

  endfunction ! set_s/h_matrix_entry


#if R1_C2 == 1
  !! setter function for the distributed matrices AA and BB
  integer function set_matrix_entry_r( &
#else
  integer function set_matrix_entry_c( &
#endif
                                       a1, b1, j, aa, bb ) result( n01 )
#ifdef DEBUG
  use configuration, only: WARNING, ERROR
#endif
  implicit none
    ! parameters
cDBG  character(len=*), parameter :: fun = ' set_matrix_entry: '
    ! arguments
    REAPLEX, intent(in) :: a1, b1 !! Hamiltonian and Overlap matrix element
    REAPLEX, intent(inout) :: aa(:,:), bb(:,:) !! Hamiltonian and Overlap matrix
    integer, intent(in) :: j(ROW:COL) !! global indices
    integer :: i(ROW:COL)
#ifdef NOScaLAPACK
    i = j ! global indices are local indicies
#else
    ! local vars
    integer :: jb(ROW:COL), ib(ROW:COL)

    n01 = 0 ! result for early return
#ifdef DEBUG
    if( si%istatus /= ISTATUS_RECEIVE_ENTRIES ) then
      if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'BLACS grid needs init before!'
      stop 'Sca set_matrix_entry: Matrices not in RECEIVE mode.'
    endif ! ISTATUS_RECEIVE_ENTRIES
    if( j(ROW) < 1 ) stop 'Sca set_matrix_entry: global index JRow < 1.'
    if( j(COL) < 1 ) stop 'Sca set_matrix_entry: global index JCol < 1.'
    if( j(ROW) > si%Ndim ) stop 'Sca set_matrix_entry: global index JRow > Ndim.'
    if( j(COL) > si%Ndim ) stop 'Sca set_matrix_entry: global index JCol > Ndim.'
#endif
    if( si%irank(ROW) < 0 ) return ! 0
    if( si%irank(COL) < 0 ) return ! 0

    ! find global block index jb
    jb = ( j-1 )/si%BS !! global block index
    ib = jb/si%ngrid   !! block index of my blocks, in case this is mine
    ! if this is one of my blocks, ib*ngrid+irank==jb must hold, check it
    if( any( ib*si%ngrid+si%irank /= jb ) ) return ! 0 !! not my block
    ! now compute the local index i
    ! subtract the global block offset and add the local block offset
    i = j-( jb-ib )*si%BS
#endif

#ifdef DEBUG
    if( a1 /= a1 .or. b1 /= b1 ) stop 'Sca set_matrix_entry: received NaN values.'
    if( i(ROW) < 1 .or. i(COL) < 1 ) then
      if(o>0) write(o,'(4A,9(2I6,A))') sym, fun, ERROR, 'local indices < 1, j=', j, ' i=', i, ' rank=', si%irank
      stop 'Sca set_matrix_entry: local indices out of bounds, i(ROW) or i(COL) < 1.'
    endif
    if( any( i > si%nsize ) ) then
      if(o>0) write(o,'(3A,9(2I6,A))') sym, fun, 'local indices out of bounds, ir,ic=', i, ' jr,jc=', j
      if(o>0) write(o,'(3A,9(2I6,A))') sym, fun, 'matrix shape', si%nsize
      stop 'Sca set_matrix_entry: local indices exceed matrix chunk size.'
    endif
#endif

    ! set array values
    aa(i(ROW),i(COL)) = a1 ! Hamiltonian matrix element
    bb(i(ROW),i(COL)) = b1 ! Overlap matrix element

    n01 = 1 ! accepted
  endfunction ! set_matrix_entry













#if R1_C2 == 1
  !! setter function for the distributed matrix AA
  integer function set_symmetric_entry_r( &
#else
  integer function set_hermitian_entry_c( &
#endif
                                          a, off, aa ) result( n )
  implicit none
    ! parameters
cDBG  character(len=*), parameter :: fun = ' set_s/h_entry: '
    ! arguments
    REAPLEX, intent(in) :: a(:,:) !! Hamiltonian or Overlap matrix element
    integer, intent(in) :: off(ROW:COL) !! global index offsets
    REAPLEX, intent(inout) :: aa(:,:) !! Hamiltonian or Overlap matrix
    ! local vars
    integer             :: j(ROW:COL), i(ROW:COL) !! global index offsets interchanged
    integer             :: i1, i2
    REAPLEX :: a1

    n = 0
    do i2 = 1, size(a,2)   ; j(COL) = i2 + off(COL)
      do i1 = 1, size(a,1) ; j(ROW) = i1 + off(ROW)
        if( j(ROW) == j(COL) ) then ! diagonal elements
          a1 = real( a(i1,i2) ) ! conversion to real if complex
          n = n + set_entry( a1, j, aa )
        else
          a1 = a(i1,i2)
          n = n + set_entry( a1, j, aa )
          i = j(COL:ROW:-1) ! interchange
#if R1_C2 == 2
          a1 = conjg(a1)
#endif
          n = n + set_entry( a1, i, aa )
        endif ! diagonal elements
      enddo ! i1
    enddo ! i2

  endfunction ! set_s/h_entry


#if R1_C2 == 1
  !! setter function for the distributed matrix AA
  integer function set_entry_r( &
#else
  integer function set_entry_c( &
#endif
                                a1, j, aa ) result( n01 )
#ifdef DEBUG
  use configuration, only: WARNING, ERROR
#endif
  implicit none
    ! parameters
cDBG  character(len=*), parameter :: fun = ' set_entry: '
    ! arguments
    REAPLEX, intent(in) :: a1 !! Hamiltonian or Overlap matrix element
    integer, intent(in) :: j(ROW:COL) !! global indices
    REAPLEX, intent(inout) :: aa(:,:) !! Hamiltonian or Overlap matrix
    ! local vars
    integer :: i(ROW:COL)
#ifdef NOScaLAPACK
    i = j ! global indices are local indicies
#else
    integer :: jb(ROW:COL), ib(ROW:COL)

    n01 = 0 ! result for early return
#ifdef DEBUG
    if( si%istatus /= ISTATUS_RECEIVE_ENTRIES ) then
      if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'BLACS grid needs init before!'
      stop 'Sca set_matrix_entry: Matrices not in RECEIVE mode.'
    endif ! ISTATUS_RECEIVE_ENTRIES
    if( j(ROW) < 1 ) stop 'Sca set_matrix_entry: global index JRow < 1.'
    if( j(COL) < 1 ) stop 'Sca set_matrix_entry: global index JCol < 1.'
    if( j(ROW) > si%Ndim ) stop 'Sca set_matrix_entry: global index JRow > Ndim.'
    if( j(COL) > si%Ndim ) stop 'Sca set_matrix_entry: global index JCol > Ndim.'
#endif
    if( si%irank(ROW) < 0 ) return ! 0
    if( si%irank(COL) < 0 ) return ! 0

    ! find global block index jb
    jb = ( j-1 )/si%BS !! global block index
    ib = jb/si%ngrid   !! block index of my blocks, in case this is mine
    ! if this is one of my blocks, ib*ngrid+irank==jb must hold, check it
    if( any( ib*si%ngrid+si%irank /= jb ) ) return ! 0 !! not my block
    ! now compute the local index i
    ! subtract the global block offset and add the local block offset
    i = j-( jb-ib )*si%BS
#endif

#ifdef DEBUG
    if( a1 /= a1 ) stop 'Sca set_entry: received NaN values.'
    if( i(ROW) < 1 .or. i(COL) < 1 ) then
      if(o>0) write(o,'(4A,9(2I6,A))') sym, fun, ERROR, 'local indices < 1, j=', j, ' i=', i, ' rank=', si%irank
      stop 'Sca set_matrix_entry: local indices out of bounds, i(ROW) or i(COL) < 1.'
    endif
    if( any( i > si%nsize ) ) then
      if(o>0) write(o,'(3A,9(2I6,A))') sym, fun, 'local indices out of bounds, ir,ic=', i, ' jr,jc=', j
      if(o>0) write(o,'(3A,9(2I6,A))') sym, fun, 'matrix shape', si%nsize
      stop 'Sca set_matrix_entry: local indices exceed matrix chunk size.'
    endif
#endif

    ! set array values
    aa(i(ROW),i(COL)) = a1 ! Hamiltonian matrix element

    n01 = 1 ! accepted
  endfunction ! set_entry











#if R1_C2 == 1
  !! getter function for the entries of the distributed matrix ZZ
  !! todo: replace all stops by return ! false
  REAPLEX function get_matrix_block_r( &
#else
  REAPLEX function get_matrix_block_c( &
#endif
      cc, j, comm, zz, jb_memory ) result( z )
#ifdef DEBUG
  use configuration, only: WARNING, ERROR
#endif
  use MPIconst, only: PREC
  use MPItools, only: MPImyrank
  implicit none
    ! parameters
cDBG  character(len=*), parameter     :: fun = ' get_matrix_block: '
    ! arguments
    REAPLEX, intent(in)             :: cc(1:,1:)            !! array that contains the solution
    integer, intent(in)             :: j(ROW:COL)           !! global start indices
    integer, intent(in)             :: comm                 !! sub communicator (band_comm)
    REAPLEX, intent(inout)          :: zz(1:,1:)            !! temp array that contains block number jb_memory on entry
    integer, intent(inout)          :: jb_memory(ROW:COL)   !! global block indices of the block stored in zz
    ! local vars
    integer :: i(ROW:COL), jb(ROW:COL), ib(ROW:COL), ir(ROW:COL), irnk, ierr!, nBS2

#ifdef DEBUG
    if( any( j < 1 ) ) stop 'Sca get_matrix_block: global index J < 1.'
    if( any( j > si%Ndim ) ) stop 'Sca get_matrix_block: global index J > Ndim.'
#endif

#ifndef NOScaLAPACK
! begin Sca part

#ifdef DEBUG
    if( any( shape(zz) /= si%BS ) ) stop 'Sca get_matrix_block: output array ZZ in wrong shape!'
    if( si%istatus < ISTATUS_PROBLEMS_SOLVED ) then
      if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'problem must be solved before, status = ', to_String( si )
#ifndef NOScaLAPACK
      stop 'Sca get_matrix_block: solve eigenvalue problem first!'
#endif
    endif ! ISTATUS_RECEIVE_ENTRIES
    if( any( jb_memory > si%Ndim/si%BS ) ) stop 'Sca get_matrix_block: JB_MEMORY excceds bounds!'
#endif


#ifdef TESTS
    if( any( jb_memory < 0 ) ) then
cDBG  if(o>0) write(o,'(3A,9(2I4,A))') sym, fun, 'first time to receive data for ZZ!'
      si%nreloads = 0 ! init
    endif ! any block index of the block in local memory is out of bounds, [ -1 -1] means unset
#endif

    jb = ( j-1 )/si%BS !! find the global block index jb
    if( jb(ROW) /= jb_memory(ROW) .or. jb(COL) /= jb_memory(COL) ) then
      ! MISS ! needs to reload matrix chunk from the corresponding owning process

      ib = jb/si%ngrid           !! block index of my blocks, in case this is mine
      ir = jb-ib*si%ngrid        !! grid ranks of the owning process of the requested block

      ! find the owning process rank irnk
      ! it is assumed, that the first ngrid(ROW)*ngrid(COL) processes
      ! of this communicator contribute to the BLACS grid
      irnk = ir(ROW)+si%ngrid(ROW)*ir(COL)

      if( MPImyrank( comm ) == irnk ) then
        ! now compute the local offsets i that are only correct in the owning process
        i = ib*si%BS
        ! copy array values into zz array
! cDBG    if(o>0) write(o,'(3A,4I6,9A)') sym, fun, 'access cc array(',i(ROW)+1,i(ROW)+BS(ROW),i(COL)+1,i(COL)+BS(COL), ' )'
        zz = cc(i(ROW)+1:i(ROW)+si%BS(ROW),i(COL)+1:i(COL)+si%BS(COL)) ! load solution
      endif  ! my chunk

#ifndef NOMPI
      ! broadcast entire block along comm
      call MPI_Bcast( zz, size(zz), PREC( R1_C2 ), irnk, comm, ierr )
#endif
      ! mark that the local array zz contains block number jb now:
      jb_memory = jb

cDBG  si%nreloads = si%nreloads+1 ! count silently
!       if(o>0) write(o,'(3A,9(2I6,A))') sym, fun, 'try to find element', j, &
!         ' in block', jb, ' but found block', jb_memory, ' ==> reload from rank', irnk

    endif ! needs to reload

    ! now compute the local indices i ranging from 1 to BS
    i = j - jb*si%BS
    z = zz(i(ROW),i(COL)) ! get element

cDBG  if( z /= z ) stop 'Sca get_matrix_entry: NaN in z after Bcast.'
! end of Sca part
#else
! begin serial part

    z = cc(j(ROW),j(COL)) ! get element

! end of serial part
#endif
  endfunction ! get_matrix_block





  !-+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0
  !--
  !-- get_matrix
  !--
#if R1_C2 == 1
  !! getter function for the a chunk of the distributed matrix ZZ
  integer function get_matrix_r( &
#else
  integer function get_matrix_c( &
#endif
      cc, off, comm, z ) result( ist )
cDBG  use configuration, only: WARNING, ERROR
  use MPIconst, only: PREC
  use MPItools, only: MPImyrank
cDBG use MPItools, only: operator(.MPIdiff.)
  implicit none
    ! parameters
cDBG  character(len=*), parameter     :: fun = ' get_matrix: '
    ! arguments
    REAPLEX, intent(in)             :: cc(1:,1:)    !< BLACS matrix defined by derived-type ScaLInfo si
    integer, intent(in)             :: off(ROW:COL) !< global band index offsets
    integer, intent(in)             :: comm         !< sub communicator for band parallelization (band_comm)
    REAPLEX, intent(out)            :: z(1:,1:)     !< result

#ifndef NOScaLAPACK
! begin Sca part

    ! local vars
    REAPLEX, allocatable            :: zb(:,:) !< zb(si%BS(ROW),si%BS(COL))
    integer :: jbr, jbc, me
    integer :: i(ROW:COL), jb(ROW:COL), ib(ROW:COL), ir(ROW:COL), irnk
    integer :: il(2), iu(2), bl(2), bu(2), k
cDBG  integer :: nall

cDBG  if( any( off < 0 ) ) stop 'Sca get_matrix_block: global index offset < 0.'

    me = MPImyrank( comm )
cDBG  if( off .MPIdiff. comm ) stop 'Sca: get_matrix: offset differs along comm'
cDBG  if( shape(z) .MPIdiff. comm ) stop 'Sca: get_matrix: shape(z) differs along comm'

cDBG  if( si%istatus < ISTATUS_PROBLEMS_SOLVED ) then
cDBG    if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'problem must be solved before, status = ', to_String( si )
cDBG  endif ! ISTATUS_RECEIVE_ENTRIES


    !-- Allocate work array zb and initialize
    allocate( zb(si%BS(ROW),si%BS(COL)), stat=ist )
cDBG    if( ist /= 0 ) stop 'Sca: get_matrix: allocation zb failed.'
    zb (:,:)= 0.

cDBG  nall = 0

    ! jb = ( j-1 )/si%BS !! find the global block index jb of global index j
    ! now j = off+1:off+shape(z)
    do jbc = off(COL)/si%BS(COL), (off(COL)+size(z,COL)-1)/si%BS(COL)
      jb(COL) = jbc
      do jbr = off(ROW)/si%BS(ROW), (off(ROW)+size(z,ROW)-1)/si%BS(ROW)
        jb(ROW) = jbr

        !==============================================================================================
        ib = jb/si%ngrid    !! local block index, in case this block is mine
        ir = jb-ib*si%ngrid !! grid ranks of the owning process of the requested block

        ! find the owning process rank irnk
        ! it is assumed, that the first ngrid(ROW)*ngrid(COL) processes
        ! of this communicator contribute to the BLACS grid
        irnk = ir(ROW)+si%ngrid(ROW)*ir(COL)

        if( irnk == me ) then
          ! now compute the local offsets i that are only correct in the owning process
          i(:) = ib*si%BS(:)
          ! copy array values into zz array
cDBG          if(o>0) write(o,'(2A,4(A,I0),A,I0,X,I0)') sym, fun, 'access cc(',i(ROW)+1,':',min(i(ROW)+si%BS(ROW),size(cc,1)),',',i(COL)+1,':',min(i(COL)+si%BS(COL),size(cc,2)),') shape(cc) =',shape(cc)
          zb(1:min(i(ROW)+si%BS(ROW),size(cc,1)),1:min(i(COL)+si%BS(COL),size(cc,2))) = cc(i(ROW)+1:min(i(ROW)+si%BS(ROW),size(cc,1)),i(COL)+1:min(i(COL)+si%BS(COL),size(cc,2))) ! load solution
        else  ! my block
          zb(:,:) = 0.
        endif ! my block
#ifndef NOMPI
        ! broadcast this entire block along comm
        call MPI_Bcast( zb, size(zb), PREC( R1_C2 ), irnk, comm, ist )
#endif

        do k = ROW, COL
          ! lower bounds
          bl(k) = 1
          il(k) = 1 + jb(k)*si%BS(k) - off(k)
          if( il(k) < 1 ) then
            ! increase the lower bound
            bl(k) = bl(k) - ( il(k)-1 )
            il(k) = 1
          endif ! il < 1
          ! upper bounds
          bu(k) = si%BS(k)
          iu(k) = (1+jb(k))*si%BS(k) - off(k)
          if( iu(k) > size(z,k) ) then
            ! decrease the upper bounds
            bu(k) = bu(k) - ( iu(k)-size(z,k) )
            iu(k) = size(z,k)
          endif ! iu > size
cDBG          if( bu(k)-bl(k) /= iu(k)-il(k) ) stop 'Sca: 800 algorithm wrong'
        enddo ! k
cDBG          nall = nall + product(bu-bl+1)

        ! get elements
        z(il(ROW):iu(ROW),il(COL):iu(COL)) = zb(bl(ROW):bu(ROW),bl(COL):bu(COL))
        !==============================================================================================

      enddo ! jbr
    enddo ! jbc
cDBG  if( nall /= size(z) ) stop 'Sca: 810 algorithm wrong'

cDBG  if( any( z /= z ) ) stop 'Sca get_matrix: NaN in z after Bcast.'

! end of Sca part
#else
! begin serial part

    z(:,:) = cc(off(ROW)+1:off(ROW)+size(z,ROW),off(COL)+1:off(COL)+size(z,COL)) ! get elements
    ist = 0

! end of serial part
#endif
  endfunction ! get_matrix
  !--
  !-+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0




  !-+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0
  !!
  !! deallocate the matrices
  !! go into non-initialized mode
  !> @brief Fortran function for deallocating BLACS matrices and setting initial values to ScaLinfo
  !> @author Shigeru Tsukamoto (s.tsukamoto@fz-juelich.de)
  !> @details The function at first check if the BLACS arrays are allocated, and if the global
  !>          variable si has at least the allocated-status. To confirm whether the matrix array AA
  !>          and BB are associated to the global value si, compare the sizes of the local matrices.
  !>          After the confirmation, the matrices AA and BB are deallocated and the global value is
  !>          set to as not-initialized.
  !> @date 21. Dec. 2015 First created
  !> @todo Deallocation has to be executed only on the MPI processes involved in the ScaLAPACK(BLACS)
  !>       process grids. At this moment, deallocation is executed on all the processes, regardress
  !>       the process is the menber of the process grids or not.
#if R1_C2 == 1
  integer function free_matrix_r( &
#else
  integer function free_matrix_c( &
#endif
      comm, aa, bb ) result( ist )
  use configuration, only: WARNING
  implicit none
    ! parameters
cDBG    character(len=*), parameter :: fun = ' free_matrix: '
    ! arguments
    integer,              intent(in)    :: comm    !< MPI subcommunicator
    REAPLEX, allocatable, intent(inout) :: aa(:,:) !< BLACS matrices to be deallocated
    REAPLEX, allocatable, intent(inout) :: bb(:,:) !< BLACS matrices to be deallocated
    ! local vars
#ifndef NOScaLAPACK
    external :: BLACS_GRIDEXIT
#endif

    !--
    !-- Check global variable si and arguments
    !--
cDBG    if(o>0) write(o,'(9A)'  ) sym, fun, 'Module Status si = ', to_String( si )
cDBG    if(o>0) write(o,'(3A,L)') sym, fun, 'is_allocated( aa ) = ', allocated(aa)
cDBG    if(o>0) write(o,'(3A,L)') sym, fun, 'is_allocated( bb ) = ', allocated(bb)
    if( si%istatus < ISTATUS_ARRAY_ALLOCATED ) stop 'si%istatus < ISTATUS_ARRAY_ALLOCATED'
    if( .not. allocated( aa ) ) stop '.not. allocated( aa )'
    if( .not. allocated( bb ) ) stop '.not. allocated( bb )'

    !--
    !-- Check size of matrice AA & BB
    !--
cDBG    if(o>0) write(o,'(2A,2(A,2i0))') sym, fun, 'shape(AA) = ', shape(aa), ' si%nsize = ', si%nsize
    if( any( shape(aa) /= si%nsize ) ) stop 'any( shape(aa) /= si%nsize )'
cDBG    if(o>0) write(o,'(2A,2(A,2i0))') sym, fun, 'shape(BB) = ', shape(bb), ' si%nsize = ', si%nsize
    if( any( shape(bb) /= si%nsize ) ) stop 'any( shape(bb) /= si%nsize )'

    !--
    !-- Deallocate BLACS matrix array AA and BB
    !--
    deallocate( aa, bb, stat=ist )
    if( ist /= 0 ) stop 'ScaLAPACK failed to deallocate real/complex matrix arrays AA and/or BB.'
    si%istatus = ISTATUS_SIZE_DETERMINED
cDBG    if(o>0) write(o,'(3A,L)') sym, fun, 'is_allocated( aa ) = ', allocated(aa)
cDBG    if(o>0) write(o,'(3A,L)') sym, fun, 'is_allocated( bb ) = ', allocated(bb)
cDBG    if(o>0) write(o,'(9A)'  ) sym, fun, 'Module Status si = ', to_String( si )

    !--
    !-- Free BLACS context to release the resources used by the BLACS context
    !--
    si%descr = -1
#ifndef NOScaLAPACK
cDBG    if(o>0) write(o,'(9A)') sym, fun, 'Release BLACS context'
    if( si%icontext > -1 ) call BLACS_GRIDEXIT( si%icontext )
#endif
    si%istatus = ISTATUS_NOT_INITIALIZED
cDBG    if(o>0) write(o,'(9A)') sym, fun, 'Module Status si = ', to_String( si )

    return

  endfunction !-- free_matrix
  !--
  !-+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0




#if R1_C2 == 1
  integer function dealloc_ggridd_r( &
#else
  integer function dealloc_ggridd_c( &
#endif
   iiii ) result( ist )
  implicit none
    REAPLEX, intent(in) :: iiii
#ifndef NOScaLAPACK
    call BLACS_GRIDEXIT( si%icontext )
#endif
    ist = 0
  endfunction !dealloc_gridd





  !! initialize the grid, unless done before
  !! determine the sizes, unless done before
  !! allocate the matrices
  !! go into receive mode
#if R1_C2 == 1
  integer function step1_r( &
#else
  integer function step1_c( &
#endif
      N, comm, aa, bb ) result( ist )
  use configuration, only: WARNING
  use MPIconst, only: Wtime
  use MPItools, only: MPIbcast0
  implicit none
    ! parameters
cDBG  character(len=*), parameter :: fun = ' step1: '
    character(len=*), parameter :: toString(ROW:COL) = (/'ROW','COL'/)
    ! arguments
    integer, intent(in) :: N !! dim
    integer, intent(in) :: comm !! subcommunicator
    REAPLEX, allocatable, intent(inout) :: aa(:,:), bb(:,:)
    ! local vars
    integer :: nn(1:2)=1

cDBG  if(o>0) write(o,'(9A)') sym, fun, 'Module Status = ', to_String( si )
    ist = determine_size( N, comm ) ! initialize the BLACS grid

    nn = si%nsize ! abbreviation

    allocate( aa(nn(ROW),nn(COL)), bb(nn(ROW),nn(COL)), stat=ist )
    if( ist /= 0 ) stop 'ScaLAPACK failed to allocate real/complex matrix arrays AA and/or BB!'
    si%istatus = ISTATUS_ARRAY_ALLOCATED
cDBG  if(o>0) write(o,'(9A)') sym, fun, 'Module Status = ', to_String( si )

    aa = 0. ; bb = 0. ! clear arrays
    si%istatus = ISTATUS_ENTRIES_CLEARED
cDBG  if(o>0) write(o,'(9A)') sym, fun, 'Module Status = ', to_String( si )

    si%istatus = ISTATUS_RECEIVE_ENTRIES
cDBG  if(o>0) write(o,'(9A)') sym, fun, 'Module Status = ', to_String( si )

  endfunction ! step1


  !! check if the matrix has been fully set up
  !! run the ScaLAPACK routine
  !! communicate resulting eigenenergies
  !! go to result mode
#if R1_C2 == 1
  integer function step2_r( &
#else
  integer function step2_c( &
#endif
      N, comm, aa, bb, eig ) result( info )
  use MPIconst, only: Wtime
  use MPItools, only: MPIbcast0
  use MPItools, only: MPIparallel
  use configuration, only: WARNING
#ifdef NOScaLAPACK
  use LAPACK, only: generalized_eig
#endif
  implicit none
    ! parameters
cDBG  character(len=*), parameter :: fun = ' step2: '
    ! arguments
    integer, intent(in)       :: N !! dim
    integer, intent(in)       :: comm !! subcommunicator
    REAPLEX, intent(inout)    :: aa(:,:), bb(:,:)
    real, intent(out)         :: eig(:) ! (N)
    ! local vars
    integer :: neigval, neigvec, lcwrk, lrwrk, liwrk, nps, ist, i01
    real    :: time
    ! work arrays
    REAPLEX, allocatable :: zz(:,:)
    integer, allocatable :: iwork(:), ifail(:), iclst(:)
    real,    allocatable :: rwork(:), gap(:)
    complex, allocatable :: cwork(:)

cDBG integer(kind=8) :: mem
#ifdef NOScaLAPACK
    logical, save :: warn_NoSca = .true. ! init as true, so warn once only
#endif

    si%istatus = ISTATUS_MATRIX_VERIFIED
cDBG  if(o>0) write(o,'(9A)') sym, fun, 'Module Status = ', to_String( si )

#ifdef NOScaLAPACK
! begin serial

    ! solve the generalized eigenvalue problem using serial LAPACK
    if( warn_NoSca ) then
      if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'compiled without ScaLAPACK, use serial LAPACK!'
      warn_NoSca = .false. ! switch off warning after 1st time.
    endif ! warn_NoSca

cTIM time = Wtime() ! start

    info = generalized_eig( aa, bb, eig )

cTIM time = Wtime() - time ! stop time
cDBG cTIM if(o>0) write(o,'(3A,I0,A,F0.3,9(A,I0))') sym, fun, 'serial LAPACK: N = ', si%Ndim, ' needed ', time, ' sec on ', si%ngrid(ROW), ' x ', si%ngrid(COL)

! end serial
#else
! begin ScaLAPACK

    if( any( si%irank < 0 ) ) then
      ! wait for the solving processes to communicate the result via the Bcast0
      call MPIbcast0( eig, comm )
      si%istatus = ISTATUS_PROBLEMS_SOLVED
      info       = ISTATUS_PROBLEMS_SOLVED
      return
    endif ! process rank outside the BLACS grid

    ! call the ScaLAPACK routine

    nps = si%ngrid(ROW)*si%ngrid(COL)
    liwrk = 6*max(si%Ndim,nps+1,4)
    allocate( iwork(liwrk), iclst(2*nps), gap(nps), ifail(si%Ndim), stat=ist )
    if( ist /= 0 ) stop 'ScaLAPACK failed to allocate work arrays!'

    do i01 = 0, 1 ! 0:pre, 1:run

      if( i01 == 0 ) then
        allocate( cwork(2), rwork(2), stat=ist ) ; cwork = 0. ; rwork = 0. ! allocated work arrays small
        lcwrk = -1 ; lrwrk = -1 ! start a workspace query
      elseif( i01 == 1 ) then ! run
        ! cwork(1) and rwork(1) contain the minimum memory needed, often not enough to compute all eigenvectors
        lcwrk = MEM_MULT*nint(real(cwork(1))) ; deallocate( cwork, stat=ist )
        lrwrk = MEM_MULT*nint(     rwork(1) ) ; deallocate( rwork, stat=ist )
cDBG    mem = (liwrk+2*nps+si%Ndim)*4 + (lrwrk+nps)*8 + (lcwrk)*16
cDBG    if(o>0) write(o,'(3A,F12.3,9A)') sym, fun, 'work arrays use', mem/2.**20, ' MiByte'

        ! allocated work array with the suggested size
        allocate( cwork(lcwrk), rwork(lrwrk), stat=ist )
        if( ist /= 0 ) stop 'ScaLAPACK failed to allocate real[+complex] work arrays!'
        allocate( zz(size(aa,1),size(aa,2)), stat=ist )
        if( ist /= 0 ) stop 'ScaLAPACK failed to allocate ZZ array!'
      endif ! run

cTIM    time = Wtime() ! start
#if R1_C2 == 1
        ! http://www.netlib.org/scalapack/double/pdsygvx.f
        call PDSYGVX &
#else
        ! http://www.netlib.org/scalapack/complex16/pzhegvx.f
        call PZHEGVX &
#endif
            ( 1, 'V', 'A', 'L', si%Ndim, &          !          IBTYPE, JOBZ, RANGE, UPLO, N, &
              aa,1,1,si%descr, &                    !          A, IA, JA, DESCA, &
              bb,1,1,si%descr, &                    !          B, IB, JB, DESCB, &
              0.,0.,0,0, -1., &                     !          VL, VU, IL, IU, ABSTOL, &
!             neigval, neigvec, eig, -1., &         !          M, NZ, W, ORFAC, &
              neigval, neigvec, eig, ORFAC, &       !          M, NZ, W, ORFAC, &
              zz,1,1,si%descr, &                    !          Z, IZ, JZ, DESCZ, &
#if R1_C2 == 1
              rwork, lrwrk, &                       !          WORK, LWORK, &
#else
              cwork, lcwrk, &                       !          WORK, LWORK, &
              rwork, lrwrk, &                       !          RWORK, LRWORK, &
#endif
              iwork, liwrk, &                       !          IWORK, LIWORK, &
              ifail, iclst, gap, info )             !          IFAIL, ICLUSTR, GAP, INFO )

cTIM    time = Wtime() - time ! stop

    enddo ! i01 ! 0:pre 1:run
cTIM if(o>0) write(o,'(3A,I8,A,2I4,A,2I3,A,F10.3,9A)') sym, fun, 'N=', si%Ndim, ' BS=', si%BS, ' Grid=', si%ngrid, ' needed', time, ' sec'

    aa = zz ! store results of the diagonalization in matrix aa

cDBG  if( ( neigval < N .or. neigvec < N ) .and. o>0 ) write(o,'(4A,9(I6,A))')  sym, fun, &
#if R1_C2 == 1
cDBG     'PDSYGVX', &
#else
cDBG     'PZHEGVX', &
#endif
cDBG                ': N=', si%Ndim, ' found', neigval, ' eigenvalues and', neigvec, ' eigenvectors'

! end ScaLAPACK
#endif

!     ! these functions release the BLACS grid
!     call BLACS_GRIDEXIT( ictxt )
!     call BLACS_EXIT( 0 )

!! cTIM time = Wtime() ! start time
    call MPIbcast0( eig, comm ) ! rank0 of the subcomm communicates the result
!! cTIM time = Wtime() - time ! stop time
!! cTIM if(o>0 .and. MPIparallel(comm)) write(o,'(3A,I0,A,F0.3,9A)') sym, fun, 'Broadcast of ', si%Ndim, ' energy eigenvalues needed ', time, ' sec'

    if( info == 0 ) then
      si%istatus = ISTATUS_PROBLEMS_SOLVED
    elseif( info > 0 ) then
      if(o>0) write(o,'(3A,I3,9A)') sym, fun, 'ScaLAPACK routine complains, info =', info
      info    = 0 ! overwrite
      si%istatus = ISTATUS_PROBLEMS_SOLVED
    else  ! info >= 0
      if(o>0) write(o,'(3A,I3,9A)') sym, fun, 'ScaLAPACK routine FAILED, info =', info
      si%istatus = info
    endif ! info >= 0
cDBG  if(o>0) write(o,'(9A)') sym, fun, 'Module Status = ', to_String( si )

  endfunction ! step2



! end of double preprocessed part
#if R1_C2 == 2
! start module tail

  !! a redefinition of the integer divide
  !! to ceiling( nom/den )
  integer elemental function divide_ii( nom, den ) result( quo )
  implicit none
    integer, intent(in) :: nom, den
    if( den /= 0 ) then
      quo = (nom+den-1)/den ! integer divide
    else ; quo = 0 ! division by 0 not possible
    endif ! den == 0
  endfunction divide_ii



#ifdef TESTS
! from here, some test functions are given



  !! toyHamiltonian (with Airy potential)
  real function Hmt_element( Ndim, irow, icol ) result( e )
  implicit none
    integer, intent(in) :: Ndim, irow, icol !! dimension, global indices

    selectcase( irow-icol )
!     !  4th order FD   kinetic energy
!     ! 4th order FD: DENOM=12 c(0:2)=(/-30,16,-1/) !  4th order
!     case( -2,2 ) ; e = -0.5 * ( -1.)/12.
!     case( -1,1 ) ; e = -0.5 * ( 16.)/12.
!     case(    0 ) ; e = -0.5 * (-30.)/12. &
    !  8th order FD   kinetic energy
    ! 8th order FD: DENOM=5040 c(0:4)=(/-14350,8064,-1008,128,-9/) !  8th order
    case( -4,4 ) ; e = -0.5 * (    -9.)/5040.
    case( -3,3 ) ; e = -0.5 * (   128.)/5040.
    case( -2,2 ) ; e = -0.5 * ( -1008.)/5040.
    case( -1,1 ) ; e = -0.5 * (  8064.)/5040.
    case(    0 ) ; e = -0.5 * (-14350.)/5040. !&
    !                +   potential energy
!                      + real(irow-1)/real(Ndim) ! triangular potential ==> Airy functions
!                      + 0.25 *(irow-1-0.5*Ndim)**2 ! harmonic potential
    case default ; e = 0.
    endselect ! irow-icol
  endfunction

  !! toyOverlap matrix (unity)
  real function Smt_element( Ndim, irow, icol ) result( e )
  implicit none
    integer, intent(in) :: Ndim, irow, icol !! dimension, global indices
    selectcase( irow-icol )
    case(    0 ) ; e = 1.
    case default ; e = 0.
    endselect ! irow-icol
  endfunction


  integer function test_parallel( N, comm ) result( ist )
  use MPIconst, only: Wtime
  use MPItools, only: operator(.MPIsum.), MPIbarrier, MPInprocs
  implicit none
    ! parameters
cDBG  character(len=*), parameter :: fun = ' testN: '
    ! arguments
    integer, intent(in) :: N    !! dim
    integer, intent(in) :: comm !! subcommunicator
    ! local vars
    integer :: jr, jc, ne
    real :: time
    logical :: e

    integer :: jbm(ROW:COL) ! block indices of the block in memory
    integer :: jbr, jbc ! global block indices

    real :: h, s, block(si%BS(ROW),si%BS(COL)), rrow(N), Ene(N)
    real, allocatable :: hm(:,:), sm(:,:)

    if(o>0) write(o,'(3A,I9,9A)') sym, fun, 'start with', MPInprocs(comm), ' MPI procs'

    if(o>0) write(o,'(3A,Z8.8,A,I6)') sym, fun, 'call init_matrix with comm=0x', comm, '  N=', N
    ist = init_matrix( N, comm, hm, sm )

    call MPIbarrier( comm )
    ! Matrix setup with matrix elements of the
    ! toyHamiltonian and unity Overlap matrix
    time = Wtime() ! start

    ne = 0 ! init counter
    do jc = 1, N
      do jr = 1, N
        ! compute all matrix elements
        h = Hmt_element( N, jr, jc )
        s = Smt_element( N, jr, jc )
        ! offer them to all processes,
        ! the process that is supposed to will hopefully store it
        ne = ne + set_matrix_entry( h, s, (/jr,jc/), hm, sm )
      enddo ! ir
    enddo ! ic
    time = Wtime() - time
    if(o>0) write(o,'(3A,F10.3,9A)') sym, fun, 'setup took', time, ' sec'
    if(o>0) write(o,'(3A,2(I16,A),F7.2,9A)') sym, fun, 'accepted', ne, ' of', N*N, ' elements =', ne/(.01*N*N), '%'

    call MPIbarrier( comm )
    ne = ne .MPIsum. comm
    if(o>0) write(o,'(3A,2(I16,A),F7.2,9A)') sym, fun, 'total   ', ne, ' of', N*N, ' elements =', ne/(.01*N*N), '%'

    time = Wtime() ! start
    if(o>0) write(o,'(3A,Z8.8,A,I6)') sym, fun, 'call solve_matrix with comm=0x', comm, '  N=', N
    ist = solve_matrix( N, comm, hm, sm, Ene )
    time = Wtime() - time
    if(o>0) write(o,'(3A,F10.3,9A)') sym, fun, 'solve_matrix took', time, ' sec'

    call MPIbarrier( comm )
    if( ist == 0 ) then
      if(o>0) write(o,'(9A)') sym, fun, 'ScaLAPACK successful!'

      if(o>0) write(o,'(3A,I6,A,F10.3,A)') sym, fun, 'PDSYGV: N=', N, ' needed', time, ' sec'
      if(o>0) write(o,'(3A,9F16.6)') sym, fun, 'E =', Ene(1:min(9,N))
      if(o>0) write(o,'(3A,9F24.16)') sym, fun, 'E =', Ene(1:min(3,N))

    else  ! ist == 0
      if(o>0) write(o,'(3A,I3)') sym, fun, 'ScaLAPACK Failed!, info =', ist
#ifndef NOScaLAPACK
!       stop 'ScaLAPACK failed to solved the eigenvalue problem.'
      if( ist == 2 ) ist = 0 ! ignore error #2
#endif
    endif ! ist == 0


    ! 3rd method: block by block, touch every matrix element once, avoiding reloads
    time = Wtime() ! start
    ! write the eigenvectors to fort.11
    jbm = -1 ! init as out of bounds ==> means that block has not been filled with values
    do jbc = 0, N/si%BS(COL)
      do jbr = 0, N/si%BS(ROW)
        do jc = jbc*si%BS(COL)+1, min((jbc+1)*si%BS(COL),N)
          do jr = jbr*si%BS(ROW)+1, min((jbr+1)*si%BS(ROW),N)
            rrow(jr) = get_matrix_block( hm, (/jr,jc/), comm, block, jbm )
          enddo ! jr
        enddo ! jc
      enddo ! jbr
    enddo ! jbc
    time = Wtime() - time
    if(o>0) write(o,'(3A,F10.3,A,I9,9A)') sym, fun, 'b-get took', time, ' sec with', si%nreloads, ' reloads!'

  endfunction test_parallel


  !! solve the toyHamiltonian using serial LAPACK
  integer function test_serial( N ) result( ist )
  use LAPACK, only: generalized_eig
  use MPIconst, only: Wtime
  implicit none
    ! parameters
cDBG  character(len=*), parameter     :: fun = ' test1: '
    ! arguments
    integer, intent(in)             :: N !! dimension
    ! local vars
    integer                         :: jr, jc
    real                            :: time
    real, allocatable               :: m(:,:,:), e(:)

    allocate( m(N,N,2), e(N), stat=ist )
    if(o>0) write(o,'(3A,I3,9A)') sym, fun, 'allocated, status =', ist
    ! generate matrix elements
    do jc = 1, N
      do jr = 1, N
        m(jr,jc,1) = Hmt_element( N, jr, jc )
        m(jr,jc,2) = Smt_element( N, jr, jc )
      enddo ! ir
    enddo ! ic
    if(o>0) write(o,'(3A,I3,9A)') sym, fun, 'matrix setup done'

    time = Wtime() ! start
    ist = generalized_eig( m(:,:,1), m(:,:,2), e )
    time = Wtime() - time
    if(o>0) write(o,'(3A,I3,9A)') sym, fun, 'status =', ist
    if(o>0) write(o,'(3A,I6,A,F10.3,A)') sym, fun, ' DSYGV: N=', N, ' needed', time, ' sec'
    if(o>0) write(o,'(3A,9F16.6)') sym, fun, 'E =', e(1:min(9,N))
    if(o>0) write(o,'(3A,9F24.16)') sym, fun, 'E =', e(1:min(3,N))
cDBG  write(8,'(ES24.16)') e ! --> fort.8
#ifdef DEBUG
    do jc = 1, N
      write(12,'(9999ES16.6)') real(jc-1), m(:,jc,1)
    enddo ! jc
#endif
  endfunction test_serial



  integer function test() result( ist )
  use MPIconst, only: comm => MPI_COMM_WORLD
  use configuration, only: set_output_unit
  implicit none
    ! parameters
cDBG  character(len=*), parameter :: fun = ' test: '
    ! local vars
    integer :: irnk=0, N
    logical :: ex
#ifndef NOMPI
    call MPI_Init( ist )
    call MPI_Comm_rank( comm, irnk, ist )
#endif
    ist = set_output_unit( 0 ) ! no outputs
!     ist = set_output_unit( 100+irnk ) ! fort.100+rank in others
    if( irnk == 0 ) ist = set_output_unit( 6 ) ! switch on stdout in Master process

    N =   143      !!    0.025 sec -->             8.5e-9
    N =   453      !!    0.586 sec -->             6.3e-9
    N =  1434      !!   19.267 sec -->             6.5e-9 
!     N =  4534      !!  761.771 sec  -->            8.2e-9
!     N = 14336      !! 36906.358 sec --> prefactor 12.5e-9

    ist = test_parallel( N, comm )
    if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'ParallelStatus =', ist

    ! Serial LAPACK test
    if( irnk == 0 ) then
      ex = .false. ; inquire(file='serial',exist=ex,iostat=ist)
      if( ex ) then
        if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'start serial test'
        ist = test_serial( N )
        if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'SerialStatus =', ist
      elseif(o>0) then
        write(o,'(9A)') sym, fun, 'run <<touch serial>> for serial test!'
      endif ! ex
    endif ! Master

#ifndef NOMPI
    call MPI_Barrier( comm, ierr ) ! all procs have to wait for rank#0 to finish the serial test
    call MPI_Finalize( ierr )
#endif
  stop
  endsubroutine ! test

! test functions until to here
#else
  !! empty, activate PreProceesor line TESTS for a non-void test function
  integer function test()
    write(*,*,iostat=test) __FILE__,' activate PreProceesor line TESTS for a non-void test function!'
  endfunction ! test

#endif

endmodule ! ScaLAPACK

! end module tail
#endif
! end double preprocessing
#endif

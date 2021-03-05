#include "config.h"

#ifdef DEBUG_ALL
#define DEBUG
#endif

! #define DEBUG
! #define FULL_DEBUG

#ifdef DEBUG
#define cDBG  
#ifdef FULL_DEBUG
#define cFDB  
#else
#define cFDB !FDB
#endif
#else
#define cDBG !DBG
#define cFDB !FDB
#endif



#ifdef R1_C2

#if R1_C2 == 1

!! @author Paul Baumeister
!! @version 3.0
!!
!! loading and storing of data
module inout
  use configuration, only: o ! output unit, 0: no output
implicit none
  private ! default for the module namespace
  character(len=*), parameter, private :: sym = 'IO' !! module symbol

  public :: write_aDm
  public :: read_aDm
  public :: write_density
  public :: read_density
  public :: write_wfs
  public :: read_wfs
  public :: write_wfs_cube
  public :: read_wfs_cube

#ifdef EXTENDED
  public :: density2xsf
  public :: test
#endif

  integer, parameter :: HEADERLENGTH=1024 ! length of the ASCII-char header at the beginning of each binary file

  interface read_density
    module procedure read_density_r, read_density_c
  endinterface

  interface write_density
    module procedure write_density_r, write_density_c
  endinterface

  interface read_wfs
    module procedure read_wfs_r, read_wfs_c
  endinterface

  interface write_wfs
    module procedure write_wfs_r, write_wfs_c
  endinterface


  integer, parameter :: WLEN = 32 ! default word length for reading information out the header
  character(len=*), parameter :: HEADERFORMAT = '(8A,3(I0," "),I0,A,3(" ",F0.10),3(A,I0),9A)'
  character(len=*), parameter :: TIMESTAMPFORMAT = '(I4.4,6(A,I2.2))'
  real, parameter :: WARN_CELL_DIFFERS = 5E-6 ! |difference| > 0.001 %

#ifdef SINGLE_PRECISION
  character(len=*), parameter :: PREC_NAME(1:2) = (/'REAL     ','COMPLEX  '/)
#else
  character(len=*), parameter :: PREC_NAME(1:2) = (/'REAL8    ','COMPLEX16'/)
#endif

  contains

  

  character(len=HEADERLENGTH) function prepare_header( filename, quantity, ngps, cellsize, nbsk ) result( header )
  use configuration, only: WARNING, CodeName
    character(len=*), intent(in)      :: filename
    character(len=*), intent(in)      :: quantity
    integer, intent(in)               :: ngps(1:4) ! number of grid points
    real, intent(in)                  :: cellsize(3) ! extend of the supercell
    integer, intent(in)               :: nbsk(1:3) ! number bands,collinear spins,kpoints

    character(len=*), parameter       :: fun = ' prepare_header: '
    integer                           :: ios
    integer                           :: it(8)
    character(len=WLEN)               :: filecomment, d_and_t

    ! the filecomment is the first word in the header and must be one word
    write( unit=filecomment, fmt='(9A)', iostat=ios ) '#_',trim(adjustl(CodeName)),'_binary_datafile'

    call date_and_time( values=it ) ! get values
cFDB  it(5:) = 0 ! so one can compare 2 files created the same day by running a diff
cFDB  if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'timestamp will be 00:00:00 in DEBUG mode!'
    write( unit=d_and_t, fmt='(I4.4,2I2.2,A,3I2.2)', iostat=ios ) it(1:3),' ',it(5:7) ! write to string

    write( unit=header, fmt=HEADERFORMAT, IOstat=ios ) & ! elements
      trim(filecomment), &                        !   1
      '  file: "', trim(filename), '" ', &        !  2:3
      '  quantity: "', trim(quantity),'" ', &     !  4:5
      '  ngps: ', ngps(1:4), &                    !  6:10
      '  cell: ', cellsize(1:3), &                ! 11:14
      '  bands: ', nbsk(1), &                     ! 15:16
      '  spin: ', nbsk(2), &                      ! 17:18
      '  kpoints: ', nbsk(3), &                   ! 19:20
      '  date&time: ', trim(d_and_t)              ! 21:22
cDBG  if( ios /= 0 .and. o>0) write(o,'(3A,I0)') sym, fun, 'failed, maybe too much information, ios = ', ios

cFDB  if(o>0) write(o,fmt='(3A,/,A,/,3A)') sym, fun, 'header >>', trim(header), sym, fun, '<< header' ! show generated header
  endfunction ! prepare_header


  integer function read_header_from_file( filename, quantity, ngps, cellsize, nbsk ) result( ios )
  use configuration, only: ERROR
    character(len=*), intent(in)      :: filename
    character(len=*), intent(out)     :: quantity
    integer, intent(out)              :: ngps(1:4) ! number of grid points
    real, intent(out)                 :: cellsize(1:3) ! extend of the supercell
    integer, intent(out)              :: nbsk(1:3) ! number bands,collinear spins,kpoints

    character(len=*), parameter       :: fun = ' read_header: '
    integer, parameter                :: u = 20
    integer, parameter                :: NWORDS = 22

    integer                           :: close_ios
    logical                           :: lexist
    integer                           :: it(8)
    character(len=WLEN)               :: filecomment, d_and_t

    character(len=HEADERLENGTH)       :: header
cDBG  character(len=HEADERLENGTH)     :: header2
    character(len=WLEN)               :: word(NWORDS)
    integer                           :: in, iw

    ios = 1 ! error result value for early return

    quantity  = '<undefined>'
    ngps      = 0
    cellsize  = 0.
    nbsk      = 0

    inquire( file=filename, exist=lexist, iostat=ios )
    if( ios /= 0 ) then
      if(o>0) write(o,'(9A)') sym, fun, ERROR, 'unable to inquire existence of file "', trim(filename), '".'
      return ! return the error flag
    endif ! not lexist

    if( .not. lexist ) then
      if(o>0) write(o,'(9A)') sym, fun, ERROR, 'file "', trim(filename), '" does not exist.'
      ios = -1
      return ! return the error flag
    endif ! not lexist

#ifdef NOMPI
    open( unit=u, file=filename, action='read', status='old', iostat=ios, form='unformatted' ) !! BINARY
#else
    open( unit=u, file=filename, action='read', status='old', iostat=ios, form='formatted' ) !! ASCII
#endif

    if( ios /= 0 ) then
      if(o>0) write(o,'(9A)') sym, fun, ERROR, 'file "', trim(filename), '" cannot be opened.'
      close( unit=u )
      return ! return the error flag
    endif ! ios /= 0

#ifdef NOMPI
    header = '' ; read( unit=u, iostat=ios ) header !! BINARY
#else
    header = '' ; read( unit=u, iostat=ios, fmt='(A)' ) header !! ASCII
#endif
    if( ios /= 0 ) then
      if(o>0) write(o,'(9A)') sym, fun, ERROR, 'header cannot be read from file "', trim(filename), '".'
      close( unit=u, iostat=close_ios )
      return ! return the error flag
    endif ! ios /= 0

cFDB  if(o>0) write(o,'(3A,/,A,/,3A)') sym, fun, 'header ::', trim(header), sym, fun, ':: header'
    close( unit=u, iostat=close_ios )

    ! evaluate the header string
    word = '' ; read( unit=header, fmt=*, iostat=ios ) word
cFDB  if(o>0) write(o,'(99A)') sym, fun, 'words "', ( trim(word(iw)),' ' , iw=1,size(word) ), '".'
    if( ios /= 0 ) then
      if(o>0) write(o,'(9A)') sym, fun, 'Cannot find enough words in header of "', trim(filename), '".'
      if(o>0) write(o,'(99A)') sym, fun, 'words "', ( trim(word(iw)),' ' , iw=1,size(word) ), '".'
    endif ! ios /= 0

    quantity = word(5)
    ! if(o>0) write(o,'(9A)') sym, fun, 'quantity = word(5) =', quantity

    do in = 1, 4
      read( unit=word(6+in), fmt=*, iostat=ios ) ngps(in)
      if( ios /= 0 ) then
        if(o>0) write(o,'(3A,I1,9A)') sym, fun, 'number of grid points for dim #',in,&
          ' is not readable, got "', trim(word(6+in)), '".'
      endif ! ios /= 0
    enddo ! in

    do in = 1, 3
      read( unit=word(11+in), fmt=*, iostat=ios ) cellsize(in)
      if( ios /= 0 ) then
        if(o>0) write(o,'(3A,I1,9A)') sym, fun, 'cellsize for dim #',in,' is not readable, got "', &
          trim(word(11+in)), '".'
      endif ! ios /= 0
    enddo ! in

    do in = 1, 3
      read( unit=word(14+2*in), fmt=*, iostat=ios ) nbsk(in)
      if( ios /= 0 ) then
        if(o>0) write(o,'(9A)') sym, fun, 'number is not readable, got "', trim(word(14+2*in)), '" for "', &
          trim(word(13+2*in)),'".'
      endif ! ios /= 0
    enddo ! in

    ios = 0 ! result is success
  endfunction ! read_header_from_file


! end of the module head
#endif
! from here, everthing will be compiled twice


#if R1_C2 == 1
#define REAPLEX real
#else
#define REAPLEX complex
#endif


#if R1_C2 == 1
  integer function write_density_r( &
#else
  integer function write_density_c( &
#endif
                rho, g, filename, global ) result( ierr )
  use MPIconst
!   use MPIconst, only: MPI_OFFSET_KIND, Wtime, MPI_ORDER_FORTRAN, PREC, &
!                       MPI_MODE_RDWR, MPI_MODE_CREATE, MPI_INFO_NULL, &
!                       MPI_CHARACTER, MPI_INTEGER
  use MPItools, only: MPImaster, MPIbarrier
  use type_grid, only: grid
  use type_info, only: info
  use configuration, only: ERROR
  implicit none
    ! parameter
    character(len=*), parameter       :: fun = ' write_density: '
#ifdef NOMPI
    integer, parameter                :: U = 30
#endif
    ! arguments
    REAPLEX, intent(in)               :: rho(:,:,:,:) ! (g%ng(1),g%ng(2),g%ng(3),ns)
    type(grid), intent(in)            :: g ! grid descriptor
    character(len=*), intent(in)      :: filename
    type(info), intent(in)            :: global ! global information container
    ! local vars
    integer                           :: finfo, fhandle, i4
    integer                           :: ftype, status(MPI_STATUS_SIZE)
    integer(kind=MPI_OFFSET_KIND)     :: disp, offset

    real                              :: time
    character(len=HEADERLENGTH)       :: header

    if( size(rho) /= product(g%ng) ) then
      if(o>0) write(o,'(9A)') sym, fun, ERROR, 'size mismatch of input size.'
      ierr = product(g%ng) ; return
    endif ! size(rho) /= product(g%ng)

    ! header information are written to module character
    ! a variable called 'header'.
    header = prepare_header( filename, 'density', g%ng_all, g%s, (/global%nbands,global%nspins,global%nkpoints/) )
#ifdef NOMPI
! serial Part starts here

    open( unit=U, file=filename, status='unknown', iostat=ierr, action='write', form='unformatted' ) ! binary
    if( ierr /= 0 ) then
      if(o>0) write(o,'(9A)') sym, fun, 'unable to open "', trim(filename), '".'
      return
    endif ! ierr /= 0

    ! here the binary ('unformatted') writing is done
    write( unit=U, iostat=ierr ) header, rho

    if( ierr /= 0 ) then
      if(o>0) write(o,'(9A)') sym, fun, 'unable to write binary to "', trim(filename), '".'
cDBG  else ; if(o>0) write(o,'(9A)') sym, fun, '"density" written to "', trim(filename), '".'
    endif ! ierr

    close( unit=U, iostat=ierr )

! serial Part ends here
#else
! MPI-Part starts here

    call MPIbarrier( g%comm )
    time = Wtime( )

    finfo = MPI_INFO_NULL

    call MPI_File_open( g%comm, filename, IOR(MPI_MODE_WRONLY,MPI_MODE_CREATE), finfo, fhandle, ierr )

    disp   = 8
    offset = 0

    ! here the binary writing of the header is done
    if( MPImaster( g%comm ) ) &
    call MPI_File_write_at( fhandle, offset, header, HEADERLENGTH, MPI_CHARACTER, status, ierr )

    disp   = 8 + HEADERLENGTH
    offset = 0

    call MPI_Type_create_subarray( 3, g%ng_all(1:3), g%ng(1:3), g%ioff(1:3), MPI_ORDER_FORTRAN, PREC (R1_C2), ftype, ierr )
    call MPI_Type_commit( ftype, ierr )
    

    do i4 = 1, size(rho,4)
cDBG  if(o>0) write(o,'(3A,I1,9A)') sym, fun, 'write rho(:,:,:,', i4 , ')'
      call MPI_File_set_view( fhandle, disp, PREC (R1_C2), ftype, 'native', MPI_INFO_NULL, ierr )
      call MPI_File_write_at_all( fhandle, offset, rho(:,:,:,i4), product(g%ng(1:3)), PREC (R1_C2), status, ierr )
      disp = disp + 8 * product(g%ng_all(1:3))

    enddo ! i4

    call MPI_File_close( fhandle, ierr )

    if(o>0) write(o,'(9A)') sym, fun, '"density" written to "', trim(filename), '".'

    time = Wtime( ) - time
#ifdef TIME
    if(o>0) write(o,'(3A,F10.3,9A)') sym, fun, 'time', time, ' sec'
#endif

! MPI-Part ends here
#endif
  endfunction ! write_density

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#if R1_C2 == 1
  integer function read_density_r( & ! real-rank-4-array
#else
  integer function read_density_c( & ! complex-rank-4-array
#endif
                     rho, g, filename, global ) result( ierr )
  use MPIconst ! ...
!   use MPIconst, only: MPI_OFFSET_KIND, Wtime, MPI_ORDER_FORTRAN, PREC, &
!                       MPI_MODE_RDWR, MPI_MODE_CREATE, MPI_INFO_NULL, &
!                       MPI_CHARACTER, MPI_INTEGER
  use MPItools, only: MPImaster, MPIbcast0
  use type_grid, only: grid
  use type_info, only: info
  use configuration, only: WARNING
  implicit none
    ! parameters
    character(len=*), parameter       :: fun = ' read_density: '
    ! arguments
    REAPLEX, intent(out)              :: rho(:,:,:,:) ! (g%ng(1),g%ng(2),g%ng(3),ns)
    character(len=*), intent(in)      :: filename
    type(grid), intent(in)            :: g ! grid descriptor
    type(info), intent(in), optional  :: global ! global information container
    ! local vars
#ifndef NOMPI
    integer(kind=MPI_OFFSET_KIND)     :: disp, offset
    integer                           :: fh
    integer                           :: ftype
    integer                           :: status(MPI_STATUS_SIZE)
#else
    integer, parameter                :: U = 30
    character(len=HEADERLENGTH)       :: header
#endif
    integer                           :: nxyzs(4), nbsk(3), i4
    real                              :: cell(3), cell_diff(3), time
    character(len=WLEN)               :: quantity



    ierr = 1 ! return the error flag if early return

    rho = 0. ! init

    if( MPImaster( g%comm ) ) & ! Master process reads the header
      ierr = read_header_from_file( filename, quantity, nxyzs, cell, nbsk(1:3) )

    call MPIbcast0( ierr, g%comm )

    if( ierr /= 0 ) then ; if(o>0) write(o,'(9A)') sym, fun, 'header could not be read successfully.' ; return ; endif

    call MPIbcast0( nxyzs, g%comm )
    call MPIbcast0(  cell, g%comm )
    call MPIbcast0(  nbsk, g%comm )

    if( any( nxyzs(1:4) /= g%ng_all(1:4) ) ) then
      if(o>0) write(o,'(3A,9(4(" ",I0),A))') sym, fun, 'header indicates', &
        nxyzs(1:4), ' gridpoints, but array has shape is', g%ng_all(1:4)
      return ! the error flag
    endif ! total number of grid points differs
cDBG  if(o>0) write(o,'(9A)') sym, fun, 'number of grid points matches.'

    if( present( global ) ) then
      if( any( nbsk(1:3) > (/global%nbands,global%nspins,global%nkpoints/) ) ) then
        if(o>0) write(o,'(4A,9(3(" ",I0),A))') sym, fun, WARNING(0), &
          'found #of states ', nbsk(1:3), ' but assumed ', global%nbands,global%nspins,global%nkpoints, ' [b,s,k]'
      endif ! original density was generated with more states
    endif ! present global

    cell_diff = ( cell(1:3)-g%s(1:3) )/g%s(1:3)
    if( any( abs(cell_diff) > WARN_CELL_DIFFERS ) ) then
      if(o>0) write(o,'(4A,3(F0.3," "),9A)') sym, fun, WARNING(0), &
        'original cell differs by [', cell_diff*100., '] % in [x,y,z]-direction.'
    endif ! cell differs


#ifdef NOMPI
! serial version starts here

    open( unit=U, file=filename, action='read', status='old', iostat=ierr, form='unformatted' )
    ! opening must work, because it has worked before, when the header was read in.
    if( ierr /= 0 ) stop 'IO read_density: ERROR: failed when opening file for the 2nd time!'
    read( unit=U, iostat=ierr ) header, rho
    if( ierr /= 0 ) then
      if(o>0) write(o,'(9A)') sym, fun, 'could not read binary data from file "', trim(filename), '".'
      return ! the error flag
    endif ! ierr /= 0
    close( unit=U )


! serial version ends here
#else
! MPI-Part starts here
    time = Wtime( )

    call MPI_File_open( g%comm, filename, IOR(MPI_MODE_RDWR,MPI_MODE_CREATE), MPI_INFO_NULL, fh, ierr )
!     call MPI_File_open( g%comm, filename, IOR(MPI_MODE_RDONLY,MPI_MODE_CREATE), MPI_INFO_NULL, fh, ierr )

    disp   = 8 + HEADERLENGTH
    offset = 0

    call MPI_Type_create_subarray( 3, g%ng_all(1:3), g%ng(1:3), g%ioff(1:3), MPI_ORDER_FORTRAN, PREC (R1_C2), ftype, ierr )
    call MPI_Type_commit( ftype, ierr )
    

    do i4 = 1, size(rho,4)
cDBG  if(o>0) write(o,'(3A,I1,9A)') sym, fun, 'read rho(:,:,:,', i4, ')'
      call MPI_File_set_view( fh, disp, PREC (R1_C2), ftype, 'native', MPI_INFO_NULL, ierr )
      call MPI_File_read_at_all( fh, offset, rho(:,:,:,i4), product(g%ng(1:3)), PREC (R1_C2), status, ierr )

      if( ierr /= 0 ) then
        if(o>0) write(o,'(3A,I1,A,I3)') sym, fun, 'failed reading rho(:,:,:,', i4, '), status=', ierr
        return
      endif
      disp = disp + 8 * product(g%ng_all(1:3))

    enddo ! i4

    call MPI_File_close( fh, ierr )

    time = Wtime( ) - time
#ifdef TIME
    if(o>0) write(o,'(3A,F10.3,9A)') sym, fun, 'time', time, ' sec'
#endif

! MPI-Part ends here
#endif
  endfunction ! read_density

#if R1_C2 == 1
  integer function read_wfs_cube( wf, atomic_numbers, atomic_positions, gspacing, filename) result( ios )
  use type_grid, only: grid
  use type_info, only: info
  use type_atom, only: atom
  use type_species, only: species
  use configuration, only: ERROR
  implicit none
  ! parameter
  character(len=*), parameter       :: fun = ' read_wfs_cube: '
  integer, parameter :: U=30
  ! arguments
  REAPLEX, intent(out), allocatable :: wf(:,:,:)
  integer, intent(out), allocatable :: atomic_numbers(:)
  real, intent(out), allocatable :: atomic_positions(:,:) ! 3,num_atoms
  real, intent(out)              :: gspacing(3)
  string_t, intent(in)      :: filename
  integer :: num_atoms
  real :: unused_float
  integer :: size_grid(3)
  integer :: i

  #define CHECK_OPEN if( ios /= 0 ) then ; if(o>0) write(o,'(9A)') sym, fun, 'unable to open "', trim(filename), '".'; return; endif
  #define CHECK_READ if( ios /= 0 ) then ; if(o>0) write(o,'(9A)') sym, fun, 'unable to read from "', trim(filename), '".'; return; endif
  #define CHECK_ALLO if( ios /= 0 ) then ; if(o>0) write(o,'(9A)') sym, fun, 'unable to alloate'; return; endif

    ios = -1 ! ERROR


    open( unit=U, file=filename, status='old', iostat=ios, action='read', form='formatted' )
    CHECK_OPEN

    read(unit=U, fmt=*, iostat=ios) 
    CHECK_READ

    read(unit=U, fmt=*, iostat=ios)  ! read the first two commenting lines
    CHECK_READ

    read(unit=U, fmt=*, iostat=ios) num_atoms
    CHECK_READ

    allocate(atomic_numbers(num_atoms),atomic_positions(3,num_atoms),stat=ios)
    CHECK_ALLO

    read(unit=U, fmt=*, iostat=ios) size_grid(3), gspacing(3)
    CHECK_READ
    read(unit=U, fmt=*, iostat=ios) size_grid(2), unused_float,gspacing(2)
    CHECK_READ
    read(unit=U, fmt=*, iostat=ios) size_grid(1), unused_float,unused_float,gspacing(1)
    CHECK_READ

    allocate(wf(size_grid(1),size_grid(2),size_grid(3)))
    CHECK_ALLO

    do i=1,num_atoms
      read(unit=U, fmt=*, iostat=ios) atomic_numbers(i), unused_float, atomic_positions(:,i)
      CHECK_READ
    enddo

    read(unit=U, fmt=*, iostat=ios), wf(:,:,:)
    CHECK_READ

    close(unit=U)

  endfunction


#endif

#if R1_C2 == 1
  integer function write_wfs_cube( wfs, g, atoms, filename, global ) result( ios )
  use type_grid, only: grid
  use type_info, only: info
  use type_atom, only: atom
  use type_species, only: species
  use configuration, only: ERROR
  implicit none
  ! parameter
  character(len=*), parameter       :: fun = ' write_wfs_cube: '
  ! arguments
  REAPLEX, intent(in)                  :: wfs(:,:) ! (nxyzs,nbsk)
  type(grid), intent(in)            :: g ! grid descriptor (gc)
  type(atom), intent(in)            :: atoms(1:)
  character(len=*), intent(in)      :: filename
  type(info), intent(in)            :: global ! global information container
  ! local vars

  integer, parameter                :: U = 30
  character(len=278)                :: filename_b
  integer                           :: nloc(6), nall(6)
  integer :: band_i, i, j

  #undef  CHECK_OPEN
  #define CHECK_OPEN if( ios /= 0 ) then ; if(o>0) write(o,'(9A)') sym, fun, 'unable to open "', trim(filename_b), '".'; return; endif
  #define CHECK_WRITE if( ios /= 0 ) then ; if(o>0) write(o,'(9A)') sym, fun, 'unable to write to "', trim(filename_b), '".'; return; endif

    ios = -1 ! ERROR

    nloc(1:3) = g%ng(1:3)
    if( g%ng(4) > 1 ) stop 'IO: non-collinear not implemented!'
    nall(1:3) = g%ng_all(1:3)

    if( size(wfs,1) /= product(g%ng(1:3)) ) then 
      if(o>0) write(o,'(9A)') sym, fun, ERROR, 'mismatch of input #dim1.' 
      return ! error 
    endif ! sizes in grid descriptor do not match array

    if( global%nbands < 1 ) then ; ios=0 ; return ; endif ! no bands, no writing

    nloc(4:6) = (/global%nbnd,global%nspn,global%nkpt/)
    nall(4:6) = (/global%nbands,global%nspins,global%nkpoints/)

    if( any(nall /= nloc) ) then
      if(o>0) write(o,'(9a)') sym, fun, ERROR, 'function only works for 1 rank'
    endif

    if( size(wfs,2) /= product(nloc(4:6)) ) &
      then ; if(o>0) write(o,'(9A)') sym, fun, ERROR, 'mismatch of input #dim2. (wfs)' ; return ; endif

    do band_i = 1,global%nbands
      write( filename_b,fmt='(A,I0,A)') trim(filename), band_i, '.cube'
      write(*,*) filename_b
      open( unit=U, file=filename_b, status='unknown', iostat=ios, action='write', form='formatted' )
      CHECK_OPEN

      write( unit=U, fmt='(A)',iostat=ios) 'juRS CUBE FILE','OUTER LOOP: X, MIDDLE LOOP: Y, INNER LOOP: Z'
      CHECK_WRITE
      write( unit=U, fmt='(I5,3F12.6)', iostat=ios) size(atoms), 0.0, 0.0, 0.0
      CHECK_WRITE
      write( unit=U, fmt='(I5,3F12.6)', iostat=ios) (nall(4-i), (0.0,j=1,i-1), g%h(4-i), (0.0,j=i+1,3), i=1,3) ! the elementar vectors
      CHECK_WRITE
      write( unit=U, fmt='(I5,4F12.6)', iostat=ios) (atoms(i)%s%iZ, 0.0, atoms(i)%pos-g%off, i=1,size(atoms))! the atoms
      CHECK_WRITE
      write( unit=U, fmt='(6ES20.10E3)', iostat=ios) wfs(:,band_i)
      CHECK_WRITE
      close(unit=U)
    enddo

  endfunction


#endif



#if R1_C2 == 1
  integer function write_wfs_r( &
#else
  integer function write_wfs_c( &
#endif
                          wfs, g, filename, global, ene, occ, cartcomm ) result( ios )
!   use MPIconst, only: MPI_OFFSET_KIND, Wtime, MPI_ORDER_FORTRAN, PREC, &
!                       MPI_MODE_RDWR, MPI_MODE_CREATE, MPI_INFO_NULL, &
!                       MPI_CHARACTER, MPI_INTEGER
  use MPIconst
  use MPItools, only: MPImaster, MPIbarrier
  use type_grid, only: grid
  use type_info, only: info
  use configuration, only: ERROR
  implicit none
    ! parameter
    character(len=*), parameter       :: fun = ' write_wfs: '
    ! arguments
    REAPLEX, intent(in)               :: wfs(:,:) ! (nxyzs,nbsk)
    type(grid), intent(in)            :: g ! grid descriptor (gc)
    character(len=*), intent(in)      :: filename
    type(info), intent(in)            :: global ! global information container
    real, intent(in)                  :: ene(:), occ(:) ! nbsk
    MPI_Comm, intent(in)               :: cartcomm ! cartesian communicator
    ! local vars
    integer                           :: ioff(6)

    integer                           :: finfo, fhandle
    integer                           :: etype, ftype, status(MPI_STATUS_SIZE)
    integer(kind=MPI_OFFSET_KIND)     :: disp, offset

    real                              :: eo(2,size(wfs,2))
#ifdef NOMPI
    integer, parameter                :: U = 30
#endif
    character(len=HEADERLENGTH)       :: header
    integer                           :: nloc(6), nall(6)
    real                              :: time
    time = Wtime( )

    ios = -1 ! ERROR

    nloc(1:3) = g%ng(1:3)
    if( g%ng(4) > 1 ) stop 'IO: non-collinear not implemented!'
    nall(1:3) = g%ng_all(1:3)
    ioff(1:3) = g%ioff(1:3) ! ioffsets

    if( size(wfs,1) /= product(g%ng(1:3)) ) then 
      if(o>0) write(o,'(9A)') sym, fun, ERROR, 'mismatch of input #dim1.' 
      return ! error 
    endif ! sizes in grid descriptor do not match array

    if( global%nbands < 1 ) then ; ios=0 ; return ; endif ! no bands, no writing

    nloc(4:6) = (/global%nbnd,global%nspn,global%nkpt/)
    nall(4:6) = (/global%nbands,global%nspins,global%nkpoints/)
    ioff(4:6) = (/global%iobnd,global%iospn,global%iokpt/)

    if( size(wfs,2) /= product(nloc(4:6)) ) &
      then ; if(o>0) write(o,'(9A)') sym, fun, ERROR, 'mismatch of input #dim2. (wfs)' ; return ; endif
    if( size(ene,1) /= product(nloc(4:6)) ) &
      then ; if(o>0) write(o,'(9A)') sym, fun, ERROR, 'mismatch of input #dim2. (ene)' ; return ; endif
    if( size(occ,1) /= product(nloc(4:6)) ) &
      then ; if(o>0) write(o,'(9A)') sym, fun, ERROR, 'mismatch of input #dim2. (occ)' ; return ; endif

    eo(1,:) = ene
    eo(2,:) = occ

    ! header information are written to module character
    ! a variable called 'header'.
    header = prepare_header( filename, 'wavefunctions', g%ng_all, g%s, nall(4:6) )
#ifdef NOMPI
! serial Part starts here

    open( unit=U, file=filename, status='unknown', iostat=ios, action='write', form='unformatted' ) ! binary
    if( ios /= 0 ) then
      if(o>0) write(o,'(9A)') sym, fun, 'unable to open "', trim(filename), '".'
      return
    endif ! ios /= 0

    ! here the binary ('unformatted') writing is done
    write( unit=U, iostat=ios ) header, ene, occ, wfs

    if( ios /= 0 ) then
      if(o>0) write(o,'(9A)') sym, fun, 'unable to write binary to "', trim(filename), '".'
cDBG  else ; if(o>0) write(o,'(9A)') sym, fun, '"wavefunctions" written to "', trim(filename), '".'
    endif ! ios

    close( unit=U, iostat=ios )

! serial Part ends here
#else
! MPI-Part starts here

    call MPIbarrier( cartcomm )
    time = Wtime( ) ! start counting the timing here

    finfo = MPI_INFO_NULL
!   call MPI_File_open( cartcomm, filename, IOR(MPI_MODE_RDWR,MPI_MODE_CREATE), MPI_INFO_NULL, fhandle, ios )
    call MPI_File_open( cartcomm, filename, IOR(MPI_MODE_WRONLY,MPI_MODE_CREATE), finfo, fhandle, ios )

    ! binary writing of the header
    disp   = 8
    offset = 0
    if( MPImaster(cartcomm) ) &
    call MPI_File_write_at( fhandle, offset, header, HEADERLENGTH, MPI_CHARACTER, status, ios )

    ! binary writing of ene, occ
    disp   = 8 + HEADERLENGTH
    offset = 0

    nall(1:3) = (/1,1,2/)
    nloc(1:3) = (/1,1,2/)
    ioff(1:3) = 0 ! ioffsets
    call MPI_Type_create_subarray( 6, nall(1:6), nloc(1:6), ioff(1:6), MPI_ORDER_FORTRAN, PREC( 1 ), etype, ios )
    call MPI_Type_commit( etype, ios )
    call MPI_File_set_view( fhandle, disp, PREC( 1 ), etype, 'native', MPI_INFO_NULL, ios )
cDBG  if(ios /= 0) write(*,*) 'write: first set view: failed!'
    call MPI_File_write_at_all( fhandle, offset, eo, product(nloc(1:6)), PREC( 1 ), status, ios )

    ! binary writing of wfs
    disp   = 8 + HEADERLENGTH + 8 * product(nall(1:6))
    offset = 0

    nall(1:3) = g%ng_all(1:3) ! number of all grid points
    nloc(1:3) = g%ng(1:3) ! number of grid points in this domain
    ioff(1:3) = g%ioff(1:3) ! offset

cDBG  !write(*, *) 'nall', nall(1:6)
cDBG  !write(*, *) 'nloc', nloc(1:6)
cDBG  !write(*, *) 'ioff', ioff(1:6)

    call MPI_Type_create_subarray( 6, nall(1:6), nloc(1:6), ioff(1:6), MPI_ORDER_FORTRAN, PREC (R1_C2), ftype, ios )
    call MPI_Type_commit( ftype, ios )
cDBG  if(ios /= 0) write(*,*) 'write: type commit: failed!'
    !call MPI_File_set_view( fhandle, disp, PREC( R1_C2 ), ftype, 'native', MPI_INFO_NULL, ios )

    call MPI_File_set_view( fhandle, disp, PREC (R1_C2), ftype, 'native', MPI_INFO_NULL, ios )
    !PREC( 1 )_COMPLEX
cDBG  if(ios /= 0) write(*,*) 'write: second set view: failed!'
    call MPI_File_write_at_all( fhandle, offset, wfs, product(nloc(1:6)), PREC (R1_C2), status, ios )
cDBG  if(ios /= 0) write(*,*) 'write: write all: failed!'
    call MPI_File_close( fhandle, ios )

    if(o>0) write(o,'(9A)') sym, fun, '"wavefunctions" written to "', trim(filename), '".'

#ifdef TIME
    time = Wtime() - time
    if(o>0) write(o,'(3A,F0.3,9A)') sym, fun, 'took ', time, ' sec'
#endif

! MPI-Part ends here
#endif
  endfunction ! write_wfs

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




#if R1_C2 == 1
  integer function read_wfs_r( &
#else
  integer function read_wfs_c( &
#endif
      wfs, g, filename, global, ene, occ, cartcomm ) result( ierr )
!   use MPIconst, only: MPI_OFFSET_KIND, Wtime, MPI_ORDER_FORTRAN, PREC, &
!                       MPI_MODE_RDWR, MPI_MODE_CREATE, MPI_INFO_NULL, &
!                       MPI_CHARACTER, MPI_INTEGER
  use MPIconst
  use MPItools, only: MPImaster, MPIbcast0
  use type_grid, only: grid
  use type_info, only: info
  use configuration, only: ERROR, WARNING
  implicit none
    ! parameter
    character(len=*), parameter       :: fun = ' read_wfs: '
    ! arguments
    REAPLEX, intent(out)              :: wfs(:,:) ! (nxyz,nbsk)
    type(grid), intent(in)            :: g ! grid descriptor (gc)
    character(len=*), intent(in)      :: filename
    type(info), intent(in)            :: global ! global information container
    real, intent(out)                 :: ene(:), occ(:) ! nbsk
    MPI_Comm, intent(in)               :: cartcomm ! cartesian communicator
    ! local vars
    integer                           :: ioff(6)

    integer                           :: fhandle, ftype, status(MPI_STATUS_SIZE)!, req1
    integer(kind=MPI_OFFSET_KIND)     :: disp, offset

    real                              :: time
    real                              :: cell(3), cell_diff(3)
    character(WLEN)                   :: quantity
    integer                           :: nxyzs_got(4), nbsk_got(3)
    real, allocatable                 :: eo(:, :)
    REAPLEX, allocatable              :: wfs_file(:,:) ! (nxyz,nbsk) as in the file 

    integer                           :: n(6), nall(6)
    logical                           :: load_paired_do_polarized = .false.
    integer                           :: is, ib, ik, jj, ii
   
#ifdef NOMPI
    character(len=HEADERLENGTH)       :: header
    integer, parameter                :: U = 30
    integer                           :: ios

#endif

    ierr = 1 ! result for early return

    !!write (*,*) 'wfs size', size(wfs,1), size(wfs,2)

    n   (1:3) = g%ng(1:3)
    nall(1:3) = g%ng_all(1:3)
    if( g%ng(4) > 1 ) stop 'IO: non-collinear not implemented!'
    ioff(1:3) = g%ioff(1:3) ! ioffsets

    if( size(wfs,1) /= product(g%ng(1:3)) ) then
      if(o>0) write(o,'(9A)') sym, fun, ERROR, 'mismatch of input #dim1.' ; return ; endif ! size(wfs,1) /= product(g%ng)

    n   (4:6) = (/global%nbnd,global%nspn,global%nkpt/)
    nall(4:6) = (/global%nbands,global%nspins,global%nkpoints/)
    ioff(4:6) = (/global%iobnd,global%iospn,global%iokpt/)

    if( size(wfs,2) /= product(n(4:6)) ) then
      if(o>0) write(o,'(9A)') sym, fun, ERROR, 'mismatch of input #dim2. (WFS)' ; return ; endif !size(wfs,1) /= product(nbsk)
    if( size(ene) /= product(n(4:6)) ) then
      if(o>0) write(o,'(9A)') sym, fun, ERROR, 'mismatch of input #dim2. (ENE)' ; return ; endif !size(wfs,1) /= product(nbsk)
    if( size(occ) /= product(n(4:6)) ) then
      if(o>0) write(o,'(9A)') sym, fun, ERROR, 'mismatch of input #dim2. (OCC)' ; return ; endif ! size(wfs,1) /= product(nbsk)

    wfs = 0. ! init output arrays
    ene = 0.
    occ = 0.

    if( MPImaster( cartcomm ) ) & ! Master process reads the header
      ierr = read_header_from_file( filename, quantity, nxyzs_got, cell, nbsk_got )
    call MPIbcast0( ierr, cartcomm ) ! tell the status to all other processes

    if( ierr /= 0 ) then
      if(o>0) write(o,'(9A)') sym, fun, 'header could not be read successfully.'
      return ! the error flag
    endif ! ierr /= ISTATUS_SUCCESS

    call MPIbcast0( nxyzs_got, cartcomm )
    call MPIbcast0( cell, cartcomm )
    call MPIbcast0( nbsk_got, cartcomm )

    if( any( nall(1:3) /= nxyzs_got(1:3) ) ) then
      if(o>0) write(o,'(3A,3I6,I2,A,3I6,I2,9A)') sym, fun, 'header indicates ', &
        nxyzs_got(1:4), ' gridpoints, but array has shape', nall(1:3)
      return ! the error flag
cDBG  else ; if(o>0) write(o,'(9A)') sym, fun, 'number of grid points matches.'
    endif ! total number of grid points differs



!    if( any( nall(4:6) > nbsk_got ) ) then
!      if(o>0) write(o,'(3A,9(I6,I2,I6,A))') sym, fun, 'header indicates ', &
!        nbsk_got(1:3), ' states, but array has shape', nall(4:6)
!      return ! the error flag
!    elseif( any( nall(4:6) < nbsk_got ) ) then
!      if(o>0) write(o,'(4A,I6,I2,I6,A,I6,I2,I6,9A)') sym, fun, WARNING(0), &
!        'header indicates ', nbsk_got(1:3), ' states, but array has shape', nall(4:6)
!      if(o>0) write(o,'(9A)') sym, fun, ERROR, 'reading less states not properly implemented!'
!      return ! the error flag
!cDBG  else ; if(o>0) write(o,'(9A)') sym, fun, 'number of states matches.'
!    endif! numbers of states

    

    if( any( nall(4:4) > nbsk_got(1) ) ) then
      if(o>0) write(o,'(3A,9(I6,I2,I6,A))') sym, fun, 'header indicates ', &
        nbsk_got(1:3), ' states, but array has shape', nall(4:6)
      return ! the error flag
    elseif( any( nall(6:6) > nbsk_got(3) ) ) then
      if(o>0) write(o,'(3A,9(I6,I2,I6,A))') sym, fun, 'header indicates ', &
        nbsk_got(1:3), ' states, but array has shape', nall(4:6)
      return ! the error flag
    elseif( any( nall(5:5) > nbsk_got(2) ) ) then
      !! loading spin paired wavefunctions to do a spin polarized calculation
      load_paired_do_polarized = .true.
      if(o>0) write(o,*) sym, fun, 'header indicates spin paired wf, using as starting wf for spin polarized'
    elseif( any( nall(4:6) < nbsk_got ) ) then
      if(o>0) write(o,'(4A,I6,I2,I6,A,I6,I2,I6,9A)') sym, fun, WARNING(0), &
        'header indicates ', nbsk_got(1:3), ' states, but array has shape', nall(4:6)
      if(o>0) write(o,'(9A)') sym, fun, ERROR, 'reading less states not properly implemented!'
      return ! the error flag
cDBG  else ; if(o>0) write(o,'(9A)') sym, fun, 'number of states matches.'
    endif! numbers of states

    


    cell_diff = ( cell(1:3)-g%s(1:3) )/g%s(1:3)
    if( any( abs(cell_diff) > WARN_CELL_DIFFERS ) ) then
      if(o>0) write(o,'(4A,3(F0.3," "),9A)') sym, fun, WARNING(0), &
        'original cell differs by [', cell_diff*100., '] % in [x,y,z]-direction.'
    endif ! cell differs

#ifdef NOMPI
! serial version

    open( unit=U, file=filename, action='read', status='old', iostat=ierr, form='unformatted' )
    ! opening must work, because it has worked before, when the header was read in.
    if( ierr /= 0 ) then
      if(o>0) write(o,'(9A)') sym, fun, 'could not open binary data from file "', trim(filename), '" the second time.'
      return ! the error flag
    endif ! ierr /= 0
    read( unit=U, iostat=ierr ) header, ene, occ, wfs
    if( ierr /= 0 ) then
      if(o>0) write(o,'(9A)') sym, fun, 'could not read binary data from file "', trim(filename), '".'
      return ! the error flag
    endif ! ierr /= 0
    close( unit=U )

! serial Part ends here
#else
! MPI-Part starts here

    time = Wtime( ) ! start time

    call MPI_File_open( cartcomm, filename, IOR(MPI_MODE_RDWR,MPI_MODE_CREATE), MPI_INFO_NULL, fhandle, ierr )
!   call MPI_File_open( cartcomm, filename, IOR(MPI_MODE_RDONLY,MPI_MODE_CREATE), MPI_INFO_NULL, fhandle, ierr )

    ! binary writing of ene, occ
    disp   = 8 + HEADERLENGTH
    offset = 0

    nall(1:3) = (/1,1,2/)
    n(1:3)    = (/1,1,2/)
    ioff(1:3) = 0 ! ioffsets



    ! read from the file energy and occupation numbers

!! -----------------------------------------------------------------------------------------------------------------
    !allocate(eo(2,size(wfs,2)))
    !! allocate the array eo to accomodate what 
    !is in the file as described in the header
    allocate(eo(2,product(nbsk_got(1:3)))) 
    
    if (load_paired_do_polarized) then
      
      call MPI_Type_create_subarray( 4, &
         (/2, global%nbands,1,global%nkpoints/), &  ! global array shape
         (/2, global%nbnd,1,global%nkpt/), & ! local array shape
         (/0, global%iobnd,0,global%iokpt/), & ! offsets
          MPI_ORDER_FORTRAN, PREC( 1 ), ftype, ierr )
      call MPI_Type_commit( ftype, ierr )
      
     
      
      call MPI_File_set_view( fhandle, disp, PREC( 1 ), ftype, 'native', MPI_INFO_NULL, ierr )
       
      call MPI_File_read_at_all( fhandle, offset, eo, &
        2*global%nbnd*1*global%nkpt, & !number of elements to read 
        PREC( 1 ), status, ierr )
         

        !!! assign the read data to the 2 spin channels
      jj=1
      ii=1
      do ik=1, global%nkpt
        
          do ib=1, global%nbnd !! do spin channel 1
              ene(jj) = eo(1, ii)
              occ(jj) = 0.5*eo(2, ii)
              jj = jj+1
              ii = ii+1
          enddo
          ii=ii-global%nbnd
          do ib=1, global%nbnd !! do spin channel 2
              ene(jj) = eo(1, ii)
              occ(jj) = 0.5*eo(2, ii)
              jj = jj+1
              ii = ii+1
          enddo

      enddo
     
      !write (*,*) 'ene', ene(:)
      !write (*,*) 'occ', occ(:) 
      
    else 

      call MPI_Type_create_subarray( 4, &
         (/2, global%nbands,global%nspins,global%nkpoints/), &  ! global array shape
         (/2, global%nbnd,global%nspn,global%nkpt/), & ! local array shape
         (/0, global%iobnd,global%iospn,global%iokpt/), & ! offsets
          MPI_ORDER_FORTRAN, PREC( 1 ), ftype, ierr )
      call MPI_Type_commit( ftype, ierr )
      call MPI_File_set_view( fhandle, disp, PREC( 1 ), ftype, 'native', MPI_INFO_NULL, ierr )
      call MPI_File_read_at_all( fhandle, offset, eo, &
        2*global%nbnd*global%nspn*global%nkpt, & !number of elements to read 
        PREC( 1 ), status, ierr )
      
      
      
      ene = eo(1,:)
      occ = eo(2,:)


      !call MPI_Type_create_subarray( 6, nall(1:6), n(1:6), ioff(1:6), MPI_ORDER_FORTRAN, PREC( 1 ), ftype, ierr )
      !call MPI_Type_commit( ftype, ierr )
      !call MPI_File_set_view( fhandle, disp, PREC( 1 ), ftype, 'native', MPI_INFO_NULL, ierr )
      !call MPI_File_read_at_all( fhandle, offset, eo, product(n(1:6)), PREC( 1 ), status, ierr )
      !if( ierr /= 0 ) then 
      !  !write (*, *) 'error loading wfs 1'
      !  return
      !endif
      
      !ene = eo(1,:)
      !occ = eo(2,:)

    endif
    

    if( ierr /= 0 ) then 
        write (*, *) 'error loading wfs 1'
        return
    endif

!! -----------------------------------------------------------------------------------------------------------------
    if (load_paired_do_polarized) then
      disp   = 8 + HEADERLENGTH + 8 * 2*global%nbands*1*global%nkpoints
    else 
      disp   = 8 + HEADERLENGTH + 8 * 2*global%nbands*global%nspins*global%nkpoints
    endif 
    ! binary reading of wfs           size of the stuff that has been read !!!!
    
    offset = 0
    

    if (load_paired_do_polarized) then

      nall(1:3) = g%ng_all(1:3) ! number of all grid points
      n   (1:3) = g%ng(1:3) ! number of grid points in this domain
      ioff(1:3) = g%ioff(1:3) ! offset
  
      allocate(wfs_file(size(wfs,1),size(wfs,2)/2))
      nall(5) = 1  ! set number of spins to 1
      n(5) = 1  ! set number of spins to 1

      call MPI_Type_create_subarray( 6, nall(1:6), n(1:6), ioff(1:6), MPI_ORDER_FORTRAN, PREC (R1_C2), ftype, ierr )
      call MPI_Type_commit( ftype, ierr )
      call MPI_File_set_view( fhandle, disp, PREC (R1_C2), ftype, 'native', MPI_INFO_NULL, ierr )
      call MPI_File_read_at_all( fhandle, offset, wfs_file, product(n(1:6)), PREC (R1_C2), status, ierr )

      jj=1
      ii=1
      do ik=1, global%nkpt
        
          do ib=1, global%nbnd !! do spin channel 1
              !ene(jj) = eo(1, ii)
              wfs(:,jj) = wfs_file(:,ii)
              jj = jj+1
              ii = ii+1
          enddo
          ii=ii-global%nbnd
          do ib=1, global%nbnd !! do spin channel 2
              !ene(jj) = eo(1, ii)
              wfs(:,jj) = wfs_file(:,ii)
              jj = jj+1
              ii = ii+1
          enddo

      enddo
      deallocate(wfs_file)

    else 
      
      nall(1:3) = g%ng_all(1:3) ! number of all grid points
      n   (1:3) = g%ng(1:3) ! number of grid points in this domain
      ioff(1:3) = g%ioff(1:3) ! offset
  
      call MPI_Type_create_subarray( 6, nall(1:6), n(1:6), ioff(1:6), MPI_ORDER_FORTRAN, PREC (R1_C2), ftype, ierr )
      call MPI_Type_commit( ftype, ierr )
      call MPI_File_set_view( fhandle, disp, PREC (R1_C2), ftype, 'native', MPI_INFO_NULL, ierr )
      call MPI_File_read_at_all( fhandle, offset, wfs, product(n(1:6)), PREC (R1_C2), status, ierr )

    endif


    if( ierr /= 0 ) then 
      write (*, *) 'error reading wfs data '
      return
    endif
    
    

    call MPI_File_close( fhandle, ierr )

    if(o>0) write(o,'(9A)') sym, fun, '"wavefunctions" read from "', trim(filename), '".'

    time = Wtime( ) - time ! stop and time difference
#ifdef TIME
    if(o>0) write(o,'(3A,F0.3,9A)') sym, fun, 'took ', time, ' sec'
#endif

! MPI-Part ends here
#endif
  endfunction ! read_wfs

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#undef REAPLEX
#if R1_C2 == 2
! module tail


  integer function write_aDm( filename, a, owners, nspins, gridcomm ) result( istat )
  use configuration, only: WARNING, ERROR
  use type_atom, only: atom
  use MPIconst, only: PREC, MPI_INTEGER, Wtime, MPI_STATUS_SIZE
  use MPItools, only: MPImyrank, operator(.MPImax.), MPIbcast0, MPIbarrier
#ifdef DEBUG
  use MPItools, only: operator(.MPIdiff.)
#endif
  use configuration, only: ERROR, WARNING
  implicit none
    ! parameter
    character(len=*), parameter       :: fun = ' write_aDm: '
#ifdef FULL_DEBUG
    integer, parameter                :: o = 6 ! output to stdout on (all processes)
#endif
    integer, parameter                :: master = 0
    integer, parameter                :: ATOM_INDEX_NOT_FOUND = -9
    integer, parameter                :: U = 19
    integer, parameter                :: TWOELLP1(0:6) = (/1,3,5,7,9,11,13/)
    ! arguments
    character(len=*), intent(in)      :: filename
    type(atom), intent(in)            :: a(:) ! parallelized atom list
    integer, intent(in)               :: owners(:) ! owners of all atoms
    integer, intent(in)               :: nspins
    MPI_Comm, intent(in)               :: gridcomm
    ! local vars
    integer                           :: ios=0, ja, ia, me, ns, ii, np, nd, i1, i2, is, iZ
    integer                           :: nn(0:12), ierr, owner, status(MPI_STATUS_SIZE)
    real, allocatable                 :: dm(:,:)
    real                              :: time
    time = Wtime() ! start time

    istat = 1 ! result for early return

    selectcase( nspins )
    case( 1:2 ) ; ns = nspins ! write only a%Dm(:,:,1)
    case default 
      if(o>0) write(o,'(4A,I0)') sym, fun, ERROR, 'nspins should be in {1,2}! but found nspins = ',nspins
      istat = -1 ; return ! error
    endselect ! nspins
    ns = nspins

    me = MPImyrank( gridcomm ) ! grid rank

    ! master opens the ASCII file
    if(me==master) open(unit=U,file=filename,form='formatted',action='write',status='unknown',iostat=ios)

    call MPIbcast0( ios, gridcomm )
    if( ios /= 0 ) then
      if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'unable to open "', trim(filename), '".'
      istat = -1 ; return ! error
    endif ! ios /= 0

    ! write a comment line
    if(me==master) write( unit=U, fmt='(I0,A,/,I0,A)' ) size(owners), ' atoms',  nspins, ' spins'

    do ja = 1, size(owners) ! loop over all atoms
      owner = owners(ja)
cDBG  call MPIbarrier( gridcomm )
cDBG  if( owner .MPIdiff. gridcomm ) stop 'IO: write aDm: fatal: owners differ!'

cFDB  if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'start atom #',ja,' owned by rank #',owner,' and I am rank #',me

      if( owner == me ) then
cFDB    if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'atom #',ja,' is mine and I am rank #',me
        ia = ATOM_INDEX_NOT_FOUND ! not found
        do ii = 1, size(a)
          if( a(ii)%ja == ja ) ia = ii
        enddo ! ii
        if( ia == ATOM_INDEX_NOT_FOUND ) then
          write(*,'(4A,9(I0,A))') sym, fun, ERROR, 'rank #',me,' owns atom #',ja,' but cannot be found in a(:)!'
          istat = -1 ; return
        endif ! not found in parallelized atom list
        ! prepare dimensions
        nn     = 0 ! init
        nn(0:ubound(a(ia)%s%nn,1)) = a(ia)%s%nn
        ns = ubound(a(ia)%Dm,3)
        if( ns /= nspins ) then
          write(*,'(4A,9(I0,A))') sym, fun, ERROR, 'rank #',me,' owns atom #',ja,' but dim#3 of a%Dm is ',ns
          istat = -1 ; return
        endif ! not found in parallelized atom list
        iZ = a(ia)%s%iZ
        nn(10) = ns ! number of spins
        nn(11) = iZ ! atomic number
        nn(12) = ja ! global atom index
        ! prepare data
        np = sum( TWOELLP1 * nn(0:6) )
        nd = np*(np+1)/2 ! integer divide
        allocate( dm(nd,ns) )
        do is = 1, ns
          ii = 0
          do i1 = 1, np
            do i2 = 1, i1 ! triangular loop
              ii = ii+1
              dm(ii,is) = a(ia)%Dm(i2,i1,is)
            enddo ! i2
          enddo ! i1
        enddo ! is
cFDB    if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'owner prepared buffer ',nd,' for ',np,' projectors'
      endif ! owner == me

#ifndef NOMPI
!+mpi
      if( owner /= master ) then
        ! transfer to master, if required
        if( me == owner ) then
cFDB      if(o>0) write(o,'(3A,999(" ",I0))') sym, fun, 'owner  sends', nn
          call MPI_Send( nn,    13, MPI_INTEGER, master, 1, gridcomm, ierr ) ! send dimensions
          call MPI_Send( dm, nd*ns, PREC(  1  ), master, 2, gridcomm, ierr ) ! send data
        elseif( me == master ) then
          call MPI_Recv( nn,    13, MPI_INTEGER, owner,  1, gridcomm, status, ierr ) ! recv dimensions
cFDB      if(o>0) write(o,'(3A,999(" ",I0))') sym, fun, 'master recvs', nn
          ! prepare buffer array
          ns = nn(10)
          iZ = nn(11)
cDBG      if( nn(12) /= ja ) stop 'IO: inconsistent!'
          np = sum( TWOELLP1 * nn(0:6) )
          nd = np*(np+1)/2 ! integer divide
          allocate( dm(nd,ns) ) ; dm = 0. ! init
cFDB      if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'master prepared buffer ',nd,' for ',np,' projectors'
          ! buffer array ready
          call MPI_Recv( dm, nd*ns, PREC(  1  ), owner,  2, gridcomm, status, ierr ) ! recv data
        endif ! me == 0
cFDB  else ; if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'atom #',ja,' is owned by the master, no MPI needed!'
      endif ! owner /= master
!-mpi
#endif

      if( me == master ) then
        ! master writes aDm to file
        write( unit=U, fmt='(3(A,I0),9(" ",I0))' ) 'atom# ',ja,'  Z= ',iZ,'  nn= ', nn(0:6)
        do is = 1, ns
          write( unit=U, fmt='(9(A,I0))' ) 'spin# ',is,' ( of ',ns,' )'
          ii = 0
          do i1 = 1, np
            write( unit=U, fmt='(99F24.16)' ) dm( ii+1:ii+i1, is )
            ii = ii+i1 ! forward triangular offset
          enddo ! i1
        enddo ! is
        write( unit=U, fmt='(A)' ) '' ! empty line
      endif ! master

      if( owner == me .or. me == master ) deallocate( dm, stat=istat )

cDBG  call MPIbarrier( gridcomm )
    enddo ! ja
    
    if( me == 0 ) close( unit=U, iostat=ios ) ! master closes the ASCII file

    time = Wtime() - time ! time difference
#ifdef TIME
    if(o>0) write(o,'(3A,F10.3,9A)') sym, fun, 'time', time, ' sec'
#endif
    istat = 0 ! result
  endfunction ! write_aDm



  integer function read_aDm( filename, a, owners, nspins, gridcomm ) result( istat )
  use configuration, only: WARNING, ERROR
  use type_atom, only: atom
  use MPIconst, only: Wtime
  use MPItools, only: MPImyrank, operator(.MPIsum.), MPIallsum
  use configuration, only: ERROR, WARNING
  implicit none
    ! parameter
    character(len=*), parameter       :: fun = ' read_aDm: '
cFDB  integer, parameter                :: o = 6 ! output to stdout on (all processes)
cDBG  character(len=1), parameter       :: ELLCHAR(-1:6) = (/'?','s','p','d','f','g','h','+'/)
cDBG  character(len=2), parameter       :: ENNCHAR(0:4) = (/'? ','  ','* ','**','++'/)

    integer, parameter                :: U = 19
    integer, parameter                :: TWOELLP1(0:5) = 2*(/0,1,2,3,4,5/)+1
    ! arguments
    character(len=*), intent(in)      :: filename
    type(atom), intent(inout)         :: a(:) ! parallelized atom list
    integer, intent(in)               :: owners(:) ! owners of all atoms ! TODO: remove, only needed in debug mode
    integer, intent(in)               :: nspins
    MPI_Comm, intent(in)               :: gridcomm
    ! local vars
    integer                           :: ios=0, ja, ia, me, ns, ii, np, nd, i1, i2, is, iZ
    integer                           :: nn(0:5)
    real, allocatable                 :: dm(:,:)
    integer                           :: lios!, wios
    character(len=64)                 :: line
    character(len=6)                  :: wa, wz, wn
    integer                           :: na, nmya, nnf, nsa, isa, npa
    integer, allocatable              :: ia_list(:), ja_list(:), ok_list(:)
    integer                           :: ell, enn, emm
    integer, allocatable              :: ilnm(:)
    real                              :: na_adjust(-1:+1) = 0
    real                              :: time
    time = Wtime() ! start time

    istat = 1 ! result for early return

    selectcase( nspins )
    case( 1 ) ; ns = 1 ! write only a%Dm(:,:,1)
    case( 2 ) ; ns = 2 ! write a%Dm(:,:,1:2)
    case default ; if(o>0) write(o,'(9A)') sym, fun, ERROR, 'nspins should be in {1,2}!'
      return ! error
    endselect ! nspins

    me = MPImyrank( gridcomm ) ! grid rank

    !!stop

    ! open the ASCII file
    open(unit=U,file=filename,form='formatted',action='read',status='old',iostat=ios)

    if( ios /= 0 ) then
      if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'unable to open "', trim(filename), '".'
      return ! error
    endif ! ios /= 0

    ! read
!    read( unit=U, fmt='(I7,9A)', iostat=ios ) na ! ' atoms'
!    read( unit=U, fmt='(I7,9A)', iostat=ios ) ns ! ' spins'
    read( unit=U, fmt=*, iostat=ios ) na ! ' atoms'
    read( unit=U, fmt=*, iostat=ios ) ns ! ' spins'

cDBG  if( na /= size(owners) .and. o>0) write(o,'(4A,9(I0,A))') sym, fun, WARNING(0), 'file indicates ',na,' atoms, but requested ',size(owners)

    if( ns > nspins .and. o>0) write(o,'(4A,9(I0,A))') sym, fun, WARNING(0), &
        'file indicates ',ns,' spins, but requested ',nspins,'  ==> integrate!'
    if( ns < nspins .and. o>0) write(o,'(4A,9(I0,A))') sym, fun, WARNING(0), &
        'file indicates ',ns,' spins, but requested ',nspins,'  ==> split half!'

    ! prepare lists
    nmya = count( a(:)%owner == me ) ! number of my atoms
cDBG  if( count( owners == me ) /= nmya ) stop 'IO read aDm: owner inconsistency detected! (na)'
    allocate( ia_list(nmya), ja_list(nmya), ok_list(size(a)), stat=ios )
    if( ios /= 0 ) then
      if(o>0) write(o,'(9A)') sym, fun, ERROR, 'unable to allocate arrays!'
      return ! error
    endif ! ios /= 0
    ia_list = 0 ;  ja_list = 0 ;  ok_list = 0 ! init

    ii = 0 ! init
    do ia = 1, size(a)
      if( a(ia)%owner == me ) then
        ii = ii+1 ! count up
        ia_list(ii) = ia
        ja_list(ii) = a(ia)%ja
cDBG    if( a(ia)%ja < size(owners) ) then
cDBG      if( a(ia)%owner /= owners(a(ia)%ja) ) stop 'IO read aDm: owner inconsistency detected! (ja)'
cDBG    endif ! ja < size(owners)
      endif ! owner == me
      ok_list(ia) = 0 ! init as 0:not found
    enddo ! ia
cDBG  if( ii /= nmya ) stop 'IO read aDm: fatal counting error!'

    ia = 0 ! init atom pointer

    !=====================================================================
      line = '' ; read( unit=U, fmt='(A)', iostat=lios ) line ! read  3rd line
    do while( lios == 0 )
      selectcase( line(1:5) )
      case( 'atom#' ) ! new atom start here, read dimensions
        iZ = -6 ; ja = 0 ; nn = 0
        read(unit=line,fmt=*,iostat=ios) wa, ja, wz, iZ, wn, nn
        if( ios /= 0 ) stop 'IO read aDm: broken file!'
        if( ja <  1 ) return ! error, negative global atom index
        if( ja > na ) return ! error, more atoms than announced in the head line

        do ii = 1, nmya
          if( ja_list(ii) == ja ) ia = ia_list(ii)
        enddo ! ii

        if( ia > 0 ) then
          ! check if we are talking at least about the same atomic number
!           if( iZ /= a(ia)%s%Z ) then
          if( iZ /= a(ia)%s%iZ ) then
            ! write to unit 6 because the atom is not always owned by process 0
            write(6,'(6A,9(I0,A))') sym, fun, WARNING(0), 'expected ', trim(a(ia)%s%sym), ' but found Z= ',iZ,' for atom#',ja
          endif ! iZ does not match

          ! prepare for reading
          np = sum( TWOELLP1 * nn )
          nd = np*(np+1)/2 ! integer divide
          allocate( dm(nd,ns) )
        endif ! ia > 0

      case( 'spin#' )
        if( ia > 0 ) then
          read(unit=line,fmt=*,iostat=ios) wa, is
          if( ios /= 0 ) stop 'IO read aDm: broken file! (spin line)'
          if( is < 1 .or. is > ns ) stop 'IO read aDm: broken file! (spin index)'
          ! start reading
          ii = 0
          do i1 = 1, np
            read( unit=U, fmt=*, iostat=ios ) dm( ii+1:ii+i1, is )
            ii = ii+i1 ! forward triangular offset
          enddo ! i1

          if( is == ns ) then
            ! reading finished, start processing
            nsa = ubound(a(ia)%Dm,3)
            if( nsa /= nspins ) stop 'IO read aDm: dim#3 of aDm should be 0:NSPINS'
            npa = size(a(ia)%Dm,1)
            if( npa /= size(a(ia)%Dm,2) ) stop 'IO read aDm: aDm must be square!'
            if( npa /= sum( TWOELLP1 * a(ia)%s%nn ) ) stop 'IO read aDm: dim#1 and #2 must be match the number of projectors!'

cDBG        if( any( nn /= a(ia)%s%nn ) .and. o>0) write(o,'(4A,I0)') sym, fun, WARNING(0), '%nn differs for atom #',ja
            if( any( nn > a(ia)%s%nn ) ) na_adjust(-1) = na_adjust(-1)+1 ! lose information
            if( any( nn < a(ia)%s%nn ) ) na_adjust(+1) = na_adjust(+1)+1 ! no information given

            ! for the case that the numbers of projectors differ, we
            ! create an index table ilnm(:) that is 0, if the entry
            ! will be lost (i.e. the file contained more information
            ! than we can store
            allocate( ilnm(np) ) ; ilnm = 0 ! init 0: do not copy this entry
cDBG        ilnm = -6 ! test
            i1 = 0
            i2 = 0
            do ell = 0, 5
              do enn = 1, max( nn(ell), a(ia)%s%nn(ell) )
                if( enn <= nn(ell) .and. enn <= a(ia)%s%nn(ell) ) then
                  ! both setups have this projector
                  do emm = -ell, ell
                    i1 = i1+1
                    i2 = i2+1
                    ilnm(i1) = i2 ! these coefficients will be copied
                  enddo ! emm
                elseif( enn <= nn(ell) ) then
                  ! only the file has this projector
#ifdef DEBUG
cDBG              do emm = -ell, ell
cDBG                 i1 = i1+1
cDBG                ilnm(i1) =  0 ! these coefficients of the file will be lost
cDBG              enddo ! emm
#else
                  i1 = i1 + (2*ell+1) ! these coefficients of the file will be lost
#endif
cDBG              if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'information of the ', trim(a(ia)%s%sym),'-', ELLCHAR(ell), ENNCHAR(enn), 'wave lost.'
                elseif( enn <= a(ia)%s%nn(ell) ) then
                  ! only the atom has this projector
                  i2 = i2 + (2*ell+1) ! these coefficients of the atom will be stay 0.
cDBG              if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'the ', trim(a(ia)%s%sym),'-', ELLCHAR(ell), ENNCHAR(enn), 'wave will stay unoccpied.'
                else ! cannot happen because the enn loop goes to the max of both 
                  stop 'IO: fatal logic error, may not occur!'
                endif ! enn <
              enddo ! enn
            enddo  ! ell

cDBG        if( i1 /= np ) stop 'IO: counting error, after loops, i1 should match np!'
cDBG        if( i2 /= npa ) stop 'IO: counting error, after loops, i2 should match npa!'
cDBG        if( any( ilnm < 0 ) ) stop 'IO: error, after loops, all ilnm should be >= 0'
cDBG        if( any( ilnm == -6 ) ) stop 'IO: error, some ilnm have not been set'

            a(ia)%Dm = 0. ! init
            selectcase( nsa *4+ ns )
            case( 1 *4+ 1 ) ! copy without spin
              a(ia)%Dm(:,:,1) = aDm( npa, np, dm(:,1), ilnm )
              !write(*,*) 'case 1'
            case( 2 *4+ 1 ) ! split
              a(ia)%Dm(:,:,1) = aDm( npa, np, dm(:,1), ilnm )*0.5
              a(ia)%Dm(:,:,2) = aDm( npa, np, dm(:,1), ilnm )*0.5
              !write(*,*) 'case 2'
            case( 2 *4+ 2 ) ! copy with spin
              a(ia)%Dm(:,:,1) = aDm( npa, np, dm(:,1), ilnm )
              a(ia)%Dm(:,:,2) = aDm( npa, np, dm(:,2), ilnm )
              !write(*,*) 'case 3'
            case( 1 *4+ 2 ) ! integrate
              a(ia)%Dm(:,:,1) = aDm( npa, np, dm(:,1), ilnm ) + &
                                aDm( npa, np, dm(:,2), ilnm )
              !write(*,*) 'case 4'
            case default ; stop 'IO read aDm: case for NS,NSa not implemented!'
            endselect ! nsa, ns

            ! a%Dm(:,:,0) must always be sum( a%Dm(:,:,1:) )
            a(ia)%Dm(:,:,0) = 0.
            do isa = 1, nsa
              a(ia)%Dm(:,:,0) = a(ia)%Dm(:,:,0) + a(ia)%Dm(:,:,isa)
            enddo ! is

            ok_list(ia) = 1 ! set to 1:found
            ! reset atom pointer
            ia = 0
            deallocate( dm, ilnm )
          endif ! is == ns
        endif ! ia > 0
      case( ' ' ) ! ignore empty lines
cDBG  case default ; if(o>0) write(o,'(9A)') sym, fun, 'ignore line "', trim(line), '"'
      endselect ! line(1:5)

      line = '' ; read( unit=U, fmt='(A)', iostat=lios ) line ! read next line
    enddo ! while ios == 0
    !=====================================================================
    
    close( unit=U, iostat=ios ) ! close the ASCII file

    ! check if all atoms have been found
    nnf = count( ok_list( ia_list ) /= 1 ) .MPIsum. gridcomm ! synchronization here
    if( nnf > 0 ) then
      istat = 1 ! result
      if(o>0) write(o,'(4A,I0,9A)') sym, fun, WARNING(0), 'for ',nnf,' atoms, the atomic density matrix has not been found!'
    else  ! all states have been found
      istat = 0 ! result
    endif ! any not found atoms

    ! check how many atoms number of projectors have been adjusted
    call MPIallsum( na_adjust, gridcomm )
    if( o > 0 ) then
      if( na_adjust(-1) > 0 ) write(o,'(4A,I0,9A)') sym, fun, WARNING(0), 'for ',nint(na_adjust(-1)), &
        ' atoms some information was lost.'
      if( na_adjust(+1) > 0 ) write(o,'(4A,I0,9A)') sym, fun, WARNING(0), 'for ',nint(na_adjust(+1)), & 
        ' atoms no information was found.'
    endif

    time = Wtime() - time ! time difference
#ifdef TIME
    if(o>0) write(o,'(3A,F0.3,9A)') sym, fun, 'time ',time,' sec'
#endif
  contains

    function aDm( m, n, tria, ilnm ) ! copies from triangular representation to symmetric matrix
      integer, intent(in) :: m ! dimension of the aDm
      integer, intent(in) :: n ! dimension of input tria
      real, intent(in)    :: tria(n*(n+1)/2)
      integer, intent(in) :: ilnm(n) ! index list: if ilnm is 0, do not use this element
                                     ! if there are some indixes in aDm that are not hit
                                     ! by ilnm, these will stay empty
      real     :: aDm(m,m) ! result
      integer  :: ii, i1, i2, ilnm1, ilnm2

      aDm = 0.
      ii = 0
      do i1 = 1, n
        ilnm1 = ilnm(i1)
        if( ilnm1 > 0 ) then
cDBG      if( ilnm1 > m ) stop 'IO: aDm: index ilnm1 out of bounds.'
          do i2 = 1, i1 ! triangular loop
            ii = ii+1 ! count triangular index
            ilnm2 = ilnm(i2)
            if( ilnm2 > 0 ) then
cDBG          if( ilnm2 > m ) stop 'IO: aDm: index ilnm2 out of bounds.'
              aDm(i2,i1) = tria(ii)
              aDm(i1,i2) = tria(ii) ! symmetric
            endif ! ilnm2 > 0
          enddo ! i2
        else  ! ilnm1 > 0
          ii = ii+i1 ! forward triangular index
        endif ! ilnm1 > 0
      enddo ! i1
    endfunction ! aDm

  endfunction ! read_aDm


#include "config.h"

#ifdef EXTENDED
!+ extended

  status_t function density2xsf( name, cell, atoms ) result( ios )
  use configuration, only: o, RHO_FileNameExtension
  use type_atom, only: atom
  use type_grid, only: grid
    character(len=*), intent(in)        :: name
    type(grid), intent(in)              :: cell
    type(atom), intent(in)              :: atoms(:)

    real, allocatable                   :: rho(:,:,:,:)
    string_t                            :: rhoFile

    write(unit=rhoFile,fmt='(9A)',iostat=ios) trim(adjustl(name)),RHO_FileNameExtension
    if( ios /= 0 ) return

    allocate( rho(cell%ng_all(1),cell%ng_all(2),cell%ng_all(3),cell%ng(4)), stat=ios )
    if( ios /= 0 ) return
    rho = 0.

    if(o>0) write(o,'(9A)') '# try to read density from "', trim(rhoFile), '".'
    ios = read_density( rho, cell, filename=rhoFile )
    if( ios /= 0 ) return

    if(o>0) write(o,'(9A)') '# generate .xsf file'
    ios = write_xsf_file( name, cell, atoms, rho(:,:,:,1) )
  endfunction ! density2xsf


  status_t function write_xsf_file( name, cell, atoms, rho ) result( ios )
  use type_atom, only: atom
  use type_grid, only: grid
  use input, only: eval
  use constants, only: Ang => ANGSTROM
    character(len=*), intent(in)        :: name
    type(grid), intent(in)              :: cell
    type(atom), intent(in)              :: atoms(:)
    real, intent(in)                    :: rho(:,:,:)

    iounit_t, parameter :: xsf = 46
    string_t            :: xsfFile
    real                :: bravais(3,3), factor, maxdist, dist, cell_volume, shifts(3)=0.
    integer             :: i

    bravais = 0. ; do i = 1, 3 ; bravais(i,i) = cell%s(i) ; enddo ! i

    write(unit=xsfFile,fmt='(9A)',iostat=ios) trim(adjustl(name)),'.xsf'
    if( ios /= 0 ) return

    open(unit=xsf,file=xsfFile,action='write',status='unknown',iostat=ios)
    if( ios /= 0 ) return

    selectcase( count( cell%bc(1:3,1) == 1 ) )
    case( 3 ) ; write(xsf,'(A)') 'CRYSTAL'
      write(xsf,'(A,3(/,3F16.9))') 'PRIMVEC', bravais*Ang
      write(xsf,'(A)') 'PRIMCOORD'
      write(xsf,'(9(" ",I0))') size(atoms), 1
    case( 2 ) ; write(xsf,'(A)') 'SLAB'
      write(xsf,'(A,3(/,3F16.9))') 'PRIMVEC', bravais*Ang
      write(xsf,'(A)') 'PRIMCOORD'
      write(xsf,'(9(" ",I0))') size(atoms), 1
    case( 1 )
      write(xsf,'(A,3(/,3F16.9))') 'PRIMVEC', bravais*Ang
      write(xsf,'(A)') 'PRIMCOORD'
      write(xsf,'(9(" ",I0))') size(atoms), 1
    case default ; shifts = -.5
      write(xsf,'(A)') 'ATOMS'
    endselect ! number of periodic BCs

    do i = 1, size(atoms) 
      write(xsf,'(I3,3F16.9)') atoms(i)%s%iZ, atoms(i)%pos(1:3)*Ang
    enddo ! i

    write(xsf,'(A)') 'BEGIN_BLOCK_DATAGRID_3D', 'real_space_grid_density', 'DATAGRID_3D_UNKNOWN'
    write(xsf,'(9(" ",I0))') shape(rho)
    write(xsf,'(3(" ",F0.9))') (shifts(i)*bravais(i,i)*Ang,i=1,3), bravais*Ang

    maxdist = eval( '$xsf_max_z_dist', def=9E9 )

    cell_volume = bravais(1,1) * bravais(2,2) * bravais(3,3) * Ang**3 ! orthorhombic cells only
    factor = Ang**(-3) ; if( eval( '$xsf_use_norm', def=0. ) == 1. ) factor = 1./cell_volume

    do i = 1, size(rho,3)
      dist = (i-1) * bravais(3,3) / real( size(rho,3) )
      if( dist > maxdist ) factor = 0.

      write(xsf,'(5(1x,ES15.8))') rho(1:,1:,i)*factor
      if(o>0) write(o,'(A,I0,A,F0.3,A,F0.6)') '# plane #',i,' distance=',dist*Ang,' Ang charge=',sum(rho(:,:,i))

    enddo ! i
    write(xsf,'(A)') 'END_DATAGRID_3D', 'END_BLOCK_DATAGRID_3D'
    if(o>0) write(o,'(A,F0.6)') '# total charge=',sum(rho)
    close(xsf,iostat=ios)
  endfunction ! write_xsf_file


  status_t function test( ) result( ist ) ! tests read_rank3 and write_rank3
    integer, parameter    :: o=6
    string_t              :: q
    real                  :: s(3)
    integer               :: n(7), ngps(4) = (/3,3,3,1/)
    real, allocatable     :: den(:,:,:,:)

    allocate( den(ngps(1),ngps(2),ngps(3),ngps(4)), stat=ist ) ; if( ist /= 0 ) return
    den = 0. ; den(2,2,2,1) = 1.

    if(o>0) write(o,'(A,27F3.0)') 'array den: ', den
!     pause 'For testing the reading routine, please edit the header of the binary file "bin" now and continue then.'

    ist = read_header_from_file( filename='bin', quantity=q, ngps=n(1:4), cellsize=s, nbsk=n(5:7) )

    if( ist == 0 ) then ; if(o>0) write(o,'(3A,27F3.0)') 'quantity="',trim(q),'" array den: ', den
    else ; if(o>0) write(o,'(A)') 'reading failed.'
    endif ! ist == 0
  endfunction ! test

!- extended
#endif

endmodule ! inout
#endif
#endif

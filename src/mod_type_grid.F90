#include "config.h"

! #define DEBUG

!! @author Paul Baumeister
!! @version 3.0
!!
!! the grid descriptor for a 3dim cartesian grid
!! that is parallelized in a domain decomposition
module type_grid
#ifdef DEBUG
  use configuration, only: o ! output unit, 0: no output
#endif
implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: sym = 'tGRID' !! module symbol

  public :: grid
  public :: set
  public :: is_periodic
  public :: parallelize_grid
  public :: periodic_positions
  public :: change_cell
  public :: to_string
#ifdef EXTENDED
  public :: test
#endif

  interface to_string
    module procedure grid2string
  endinterface

  interface set
    module procedure grid_new, grid_copy
  endinterface

  interface grid_set
    module procedure grid_new
  endinterface


  ! keys
  integer, parameter, public    :: BC_INNER        = -1 !! only technical BC may assume this value
  integer, parameter, public    :: BC_FINITE       =  0 !! all values vanish outside the boundary (default)
  integer, parameter, public    :: BC_PERIODIC     =  1 !! connected to the other edge
  integer, parameter, public    :: BC_MIRROR       =  2 !! the lower boundary plane is a mirror
#ifdef GENERAL_CELL
  integer, parameter, public    :: BC_SHIFTED      =  3 !! the boundary plane is a mirror
  integer, parameter, public    :: BC_TWISTED      =  4 !! the boundary plane is a mirror
#endif
  integer, parameter, public    :: BC_EXTERNAL     =  5 !! the boundary plane is a mirror

  integer, parameter, public    :: BC_DEFAULT      = BC_FINITE ! configuration

  integer, parameter, public :: &
    NUMBER_OF_PERIODIC_IMAGES(0:5) = &
                (/ 1, & ! FINITE
                   3, & ! PERIODIC
                   2, & ! MIRROR
                   3, & ! SHIFTED
                   3, & ! TWISTED
                   1 /) ! EXTERNAL

  character(len=*), parameter, public :: BC_DESCRIPTION(-1:5)  = &
    (/ 'inner   ', & ! -1
       'finite  ', & !  0
       'periodic', & !  1
       'mirror  ', & !  2
       'shifted ', & !  3
       'twisted ', & !  4
       'external' /) !  5


  real, parameter       :: DEF_GRIDSPACING    = 0.333
  integer, parameter    :: DEF_FD_NEIGHBORS   = 2

  integer, parameter    :: NEIGHBOR_NONE      = -66
  integer, parameter    :: NEIGHBOR_MYSELF    = -100


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! TYPE GRID !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  type :: grid
    real                      :: s(3)      = 1.0 !! sizes of the rectangular unit cell
    real                      :: svol      = 1.0 !! volume of the rectangular cell
    real                      :: off(3) = -0.5*DEF_GRIDSPACING  !! process domain offset
    integer                   :: ioff(3) = 0 !! integer gridpoint offsets of the process
    ! ==> any grid point position vector r(3) = off+i*h
    real                      :: h(3) = DEF_GRIDSPACING   !! grid spacing
    real                      :: hvol = DEF_GRIDSPACING**3 !! grid volume element

    ! number of grid points ! ng(4) is the number of non-collinear spins
    integer                   :: ng(4)          = (/1,1,1,1/)    !! number of grid points in this domain
    integer                   :: ng_all(4)      = (/1,1,1,1/)    !! number of *all* grid points entire grid

    integer                   :: naprj = 0 !! number of atomic projectors in this domain
    !
    ! boundary conditions
    integer                   :: bc(3,2)  = 0 !! physical boundary conditions ! default boundary is finite
    integer                   :: tbc(3,2) = 0 !! technical boundary conditions ! default boundary is finite

    ! finite difference
    integer                   :: nf(3) = DEF_FD_NEIGHBORS !! number of finite difference neighbors
    integer                   :: nh(3) = DEF_FD_NEIGHBORS !! halo dim

    ! MPI-part
    !-----------------------------------------------------------------------------------------
    integer                   :: neighbor(3,2)  = 0      !! ranks of the cartesian neighbors
    integer                   :: nproc(3)       = 1      !! domain decomposition numbers
    integer                   :: nprocs         = 1      !! number of all processes (cell)
    MPI_Comm                  :: comm           = 0      !! cartesian cell communicator
    integer                   :: rank           = 0      !! rank
    integer                   :: rnk(3)         = 0      !! cartesian ranks
    MPI_Comm                  :: equi_comm      = 0      !! equivalent spaces communicator
    integer                   :: equi_rank      = 0      !! equivalent spaces rank
    integer                   :: equi_rnk(3)    = 0      !! equivalent spaces ranks (bands,spins,kpoints)
    !-----------------------------------------------------------------------------------------
    character(len=80)         :: name !! a name can be carried here
#ifdef GENERAL_CELL
    real                      :: basis(3,3) = 0. !! real space basis (upper triangular matrix)
    real                      :: shift(3)  = 0. !!
#endif
  endtype ! grid

#ifndef DEBUG
  integer, parameter          :: o = 0
#endif

  contains


  character(len=128) function grid2string( g ) result( str )
    type(grid), intent(in) :: g
    status_t :: ios
    integer :: id
    write( unit=str, fmt='(A,3(A,F0.6,A,I0),A,3I2,9A)', IOstat=ios ) &
      'grid', ( '  ', g%s(id), ' / ', g%ng(id) , id=1,3) , ' BC', g%bc(1:3,1), ' "',trim(g%name),'"'
  endfunction ! grid2string

  !! constructor function for the type grid
  type(grid) function grid_new( cellsize, ngridpoints, nspins, bc, shifts, name ) result( g )
  use configuration, only: ERROR, WARNING
    real, intent(in)                        :: cellsize(3) !! size of the rectangular supercell
    integer, intent(in)                     :: ngridpoints(3) !! number of grid points
    integer, intent(in)                     :: nspins !! nspins on this grid?
    integer, intent(in), optional           :: bc(3,2) !! boundary conditions
    real, intent(in), optional              :: shifts(3) !! for uneven BC, not tested
    character(len=*), intent(in), optional  :: name !!

    character(len=*), parameter       :: fun = ' grid set: '
    string_t                          :: gridname = ''
    integer                           :: id
#ifdef GENERAL_CELL
    real                              :: shft(3)
    logical                           :: bc_shift(3)
#endif
    if( present(name) ) gridname = adjustl(name)

    if( any(cellsize <= 0.0) ) stop 'grid_set: needs positive size'
    g%s = cellsize
    g%svol = product( g%s(1:3) )

    if( any( ngridpoints < 1 ) ) stop 'grid_set: needs positive #grid points'
    g%ng_all(1:3) = ngridpoints(1:3)
    g%ng(1:3) = g%ng_all(1:3)  ! not parallelized

    g%h = g%s/real(g%ng_all(1:3))
    g%hvol = product(g%h)
#ifdef DEBUG
    if(o>0) write(o,'(3A,1ES10.3E1,9A)') sym, fun, 'volume element dV =', g%hvol, '   ', trim(gridname)
#endif

    selectcase( nspins )
    case( 1, 2, 4 )
      ! non-collinear spin index
      g%ng(4)      = nspins
      g%ng_all(4)  = nspins
    case default
#ifdef DEBUG
      stop 'tGRID grid_set: nspins must be in {1,2,4}'
#endif
      g%ng(4)      = 1
      g%ng_all(4)  = 1
    endselect ! nspins


    if( present(bc) ) then
      g%bc = bc
      g%tbc = bc ! not parallelized
    else
      g%bc(:,:)  = 0 ! default
      g%tbc(:,:) = 0 ! default
    endif ! present(bc)

    g%ioff = 0
    ! compute the offset vector
    g%off = ( g%ioff-0.5 )*g%h - 0.5*g%s
    do id = 1, 3
      if( g%bc(id,1) == BC_MIRROR ) g%off(id) = ( g%ioff(id)-0.5 )*g%h(id)
    enddo ! id



#ifdef GENERAL_CELL
    call shifted_cell( g, shifts ) ! optional arg is passed on
#else
    ! only rectangular unit cell allowed
    if( present( shifts ) ) then
      if( any(shifts(1:3) /= 0. ) ) then
        if(o>0) write(o,'(4A,3F6.3,9A)') sym, fun, WARNING(0), 'shifts [', shifts, ' ] were ignored.'
      endif ! any shift /= 0.
    endif ! present shifts
#endif


#ifdef DEBUG
    do id = 1, 3
      if(o>0) write(o,'(9A)') sym, fun, 'boundary condition = low[ ', BC_DESCRIPTION(g%bc(id,1)) , '  ', BC_DESCRIPTION(g%bc(id,2)) , ' ]up'
    enddo ! id
#endif

    if( present(name) ) then
      g%name = adjustl( name )
    else  ! present name
      write( unit=g%name, fmt='(A,6I2,A,3I6,I2)' ) 'grid bc:', g%bc, ' ng:', g%ng_all
#ifdef DEBUG
      if(o>0) write(o,'(9A)') sym, fun, 'name = "', trim( g%name ), '".'
#endif
    endif ! present name

  endfunction ! grid_new


  !! constructor function for the type grid
  type(grid) function grid_copy( gc, Multiplier, nspins, name ) result( g )
  use configuration, only: o !, ERROR, WARNING
    type(grid), intent(in)                  :: gc ! grid to be copied
    integer, intent(in)                     :: Multiplier ! > 0
    integer, intent(in), optional           :: nspins ! in {1,2,4}
    character(len=*), intent(in), optional  :: name !

#ifdef DEBUG
    character(len=*), parameter             :: fun = ' grid_set: '
#else
    character(len=*), parameter             :: fun = ': '
#endif
    integer  :: id

    g = gc ! copy
    if( present( name ) ) g%name = adjustl( name )

    if( Multiplier < 1 ) stop 'tGRID grid_copy: cannot create an M-times denser grid with M < 1'
    g%ng    (1:3) = gc%ng    (1:3) * max( 1, Multiplier )
    g%ng_all(1:3) = gc%ng_all(1:3) * max( 1, Multiplier )
    g%ioff  (1:3) = g%ioff   (1:3) * max( 1, Multiplier )

    g%h = g%s/real(g%ng_all(1:3))
    g%hvol = product(g%h)

    ! recompute the offset vector
    g%off = ( g%ioff-0.5 )*g%h - 0.5*g%s
    do id = 1, 3
      if( g%bc(id,1) == BC_MIRROR ) g%off(id) = ( g%ioff(id)-0.5 )*g%h(id)
    enddo ! id

    if( present( nspins ) ) then
      selectcase( nspins )
      case( 1, 2, 4 )
        ! non-collinear spin index
        g%ng(4)      = nspins
        g%ng_all(4)  = nspins
      case default
#ifdef DEBUG
        stop 'tGRID grid_copy: nspins must be in {1,2,4}'
#endif
        g%ng(4)      = 1
        g%ng_all(4)  = 1
      endselect ! nspins
    endif ! present nspins

    if(o>0) write(o,'(4A,I0,9A)') sym, fun, trim(g%name),' = ',Multiplier,' x ',trim(gc%name)
#ifdef DEBUG
    if(o>0) write(o,'(4A,3I4,I2,A,3F7.3,A,F10.6,9A)') sym, fun, trim(g%name),':', g%ng_all, ' h=', g%h, ' aB h3=', g%hvol, ' aB^3'
#endif
  endfunction ! grid_copy


!   !! real = grid / grid
!   real function divide_gg( gnom, gden ) result( r )
!     type(grid), intent(in) :: gnom, gden
!
!     integer :: id
!     id = 1
!     r = gnom%ng(id)/real(gden%ng(id))
!   endfunction ! divide_gg
! 
!   !! grid = grid / integer
!   type(grid) function divide_gi( gnom, iden ) result( g )
!     type(grid), intent(in) :: gnom
!     integer, intent(in) :: iden
!
!     integer :: id
!     if( iden < 1 ) stop 'tGRID: divide_gi: integer denominator must be positive!'
!     g = gnom ! copy everything
!     if( any( modulo(g%ng(1:3),iden) /= 0 ) ) stop 'tGRID: cannot divide grid'
!     g%ng    (1:3) = g%ng    (1:3)/iden
!     g%ng_all(1:3) = g%ng_all(1:3)/iden
!     g%h     (1:3) = g%h     (1:3)/iden
!   endfunction ! divide_gi


  logical function is_periodic( bc ) result( p )
    integer, intent(in) :: bc !! physical boundary condition

    selectcase( bc )
    case( BC_FINITE   ) ; p =  .false.
    case( BC_PERIODIC ) ; p =  .true.
    case( BC_MIRROR   ) ; p =  .false.
#ifdef GENERAL_CELL
    case( BC_SHIFTED  ) ; p =  .true.
#endif
    case default ; stop 'tGRID: only PERIODIC, FINITE and MIRROR BC implemented.'
    endselect ! bc
  endfunction ! is_periodic


  type(grid) function change_cell( g0, cellsize, shifts ) result( g )
  use configuration, only: o, ERROR, WARNING
    type(grid), intent(in)     :: g0
    real, intent(in)           :: cellsize(3)
    real, intent(in), optional :: shifts(3)

    character(len=*), parameter :: fun = ' change_cell: '

    g = g0 ! copy

    if(o>0) write(o,'(3A,3(" ",F0.9),9A)') sym, fun, 'new cell size =', cellsize, ' aB'

    if( any( cellsize <= 0. ) ) stop 'change_cell: needs positive size'
    g%s = cellsize
    g%svol = product( g%s(1:3) )

    g%h = g%s / g%ng_all(1:3)
    g%hvol = product( g%h )

#ifdef DEBUG
    if(o>0) write(o,'(3A,F0.9,9A)') sym, fun, 'new volume element dV = ', g%hvol, ' aB^3  ', trim(g%name)
#endif

    ! keeps the parallelization (if there is one) by not changing g%ioff
    g%off = ( g%ioff-0.5 )*g%h - 0.5*g%s

#ifdef GENERAL_CELL
    call shifted_cell( g, shifts ) ! optional arg shifts is passed on
#else
    if( present(shifts) .and. o>0) write(o,'(3A,9(" ",F0.9))') sym, fun, 'shifts =',shifts
#endif

#ifdef DEBUG
    if(o>0) write(o,'(9A)') sym, fun, 'Reminder: the atomic positions have to be changed, too.'
#endif
  endfunction ! change_cell




#ifdef GENERAL_CELL
!+ general_cell

  subroutine shifted_cell( g, shifts )
  use configuration, only: ERROR, WARNING
    type(grid), intent(inout)  :: g
    real, intent(in), optional :: shifts(1:3)

    character(len=*), parameter :: fun = ' shifted_cell: '
    character(len=*), parameter :: SHIFT_DIRECTION(1:3) = (/'xy', 'yz', 'xz'/)
    real :: shft(1:3) ! a copy
    integer :: id, is
    
    g%basis(:,:) = 0. ! real space basis (upper triangular matrix)
    do id = 1, 3
      g%basis(id,id) = g%s(id) ! diagonal elements
    enddo ! id

    if( present( shifts ) ) then
      shft(1:3) = shifts(1:3)
#ifdef DEBUG
      if(o>0) write(o,'(3A,3F6.3,9A)') sym, fun, 'shifts [XY,YZ,XZ] = [', shft, ' ]'
#endif
      if( any( g%bc(1,1:2) /= BC_PERIODIC ) ) then
        shft(1:3) = 0.
      else  ! not periodic in x
        ! periodic in x
        if( any( g%bc(2,1:2) /= BC_SHIFTED ) ) then
          shft(1) = 0.
          shft(2) = 0.
        else  !
          if( any( g%bc(3,1:2) /= BC_SHIFTED ) ) then
            shft(2) = 0.
            shft(3) = 0.
          endif ! no shift in xy-direction, if the BC(Y) is not BC_Shifted
        endif ! no shift in xy-direction, if the BC(Y) is not BC_Shifted
      endif ! no shift at all, if x-direction is not periodic


      do is = 1, 3
        if( abs(shft(is)) > 0.5 ) then ! backfolding
#ifdef DEBUG
          if(o>0) write(o,'(9A)') sym, fun, 'shift > +/- 0.5 in ', SHIFT_DIRECTION(is), '-direction will be backfolded.'
#endif
          g%shift(is) = modulo( shft(is) - 0.5, 1.0 ) + 0.5
        else  ! |shft| > 1/2
          g%shift(is) = shft(is)
        endif ! |shft| > 1/2
      enddo ! is

      g%basis(1,2) = g%shift(1)*g%s(1) ! xy - element
      g%basis(2,3) = g%shift(2)*g%s(2) ! yz - element
      g%basis(1,3) = g%shift(3)*g%s(1) ! zx - element

    endif ! present shifts

#ifdef DEBUG
    if(o>0) then ! show
      write(o,'(9A)') sym, fun, 'real space basis A'
      do id = 1, 3
        write(o,'(A,3F12.6)') '   ', g%basis(:,id)
      enddo ! id
      write(o,'(9A)') '' ! empty line
    endif ! o/=0
#endif
  endsubroutine ! shifted_cell

!- general_cell
#endif


  integer function periodic_positions( g, fpos, origins, shifts, number_of_images ) &
  result( nnn ) ! number of neighboring images
  !
  ! generates a set of positions in which a periodic image
  ! of the atom appears, with the given bpoundary conditions
  !
  use configuration, only: ERROR, WARNING, StopOnError
    type(grid), intent(in)                  :: g
    real, intent(in)                        :: fpos(1:3) ! fractional positions [-1/2,1/2]
    real, intent(out)                       :: origins(:,1:) ! (1:3,1:usually 27)
    integer, intent(out), optional          :: shifts(:,1:) ! (1:3,1:usually 27)
    integer, intent(in), optional           :: number_of_images
!     real, intent(in), optional              :: rmax ! determine how many periodic images are required

    character(len=*), parameter             :: fun = ' periodic_positions: '
    character, parameter                    :: DIRECTION(1:3) = (/'x','y','z'/)
    integer                                 :: ishifts(1:3,size(origins,2))
    integer                                 :: ish(1:9,1:3) ! (1:1+2*number_of_images,3)
    real                                    :: ori(1:9,1:3) ! (1:1+2*number_of_images,3)
    real                                    :: forigin(1:3,1:125) ! origin in frac coords
    integer                                 :: id, np(1:3)!, ise, ipos, ipm(1:3), nk
    integer                                 :: ix, iy, iz
    integer                                 :: inn, bc(0:1), ixyz(1:3)
    integer                                 :: max_nnn ! max. number of nearest neighbors
    logical                                 :: trunc = .false.
    integer                                 :: npi = 1

    npi = 1 ; if( present( number_of_images ) ) npi = max(0,number_of_images)
#ifdef DEBUG
    if(o>0) write(o,'(3A,I2)') sym, fun, 'number of periodic images to each side is', npi
#endif

    nnn = 0 ! 0: error, because a meaningful result is at least the atomic position itself

    max_nnn = size( origins, 2 )

#ifdef DEBUG
    ! check input sizes
    if( size( ishifts, 2 ) /= max_nnn ) &
       stop 'periodic_positions: dim #2 of ISHIFTS does not match ishifts'
    if( size( origins, 1 ) /= 3 ) stop 'periodic_positions: dim #1 of ORIGINS /= 3'
#endif


#ifdef DEBUG
    ! check if the input fractional position is valid
    if( any( fpos < -0.5 ) .or. any( fpos > 0.5 ) ) then
      ! bounds are wrong
      if(o>0) write(o,'(9A)') sym, fun, 'fractional position is not in [-1/2 1/2]!'
      if( StopOnError ) stop 'typeGRID: periodic_positions: frac. pos out of bounds [-1/2 1/2].'
      return
    endif ! fpos not in [-1/2 1/2]
#endif

    ishifts = 0  ! init
    origins = 0. ! init

    do id = 1, 3
      bc(0) = g%bc(id,1) ! lower boundary condition
      bc(1) = g%bc(id,2) ! upper boundary condition
      np(id) = periodic_image( npi, g%bc(id,:), fpos=fpos(id), & ! request ni=1 periodic image to each side
                               ishifts=ish(:,id), origins=ori(:,id) )
    enddo !

    if(o>0) write(o,'(3A,3(I2,A),I4)') sym, fun, &
      'number of periodic images:', np(1), ' x', np(2), ' x', np(3), ' =', product(np)

    forigin = 0.

    nnn = 0 ! init
    ! spatial directions
    do iz = 1, np(3)
      do iy = 1, np(2)
        do ix = 1, np(1)

          ixyz(1:3) = (/ix,iy,iz/)

          nnn = nnn+1
          if( nnn <= max_nnn ) then
            do id = 1, 3
              ishifts(id,nnn) = ish(ixyz(id),id)
              forigin(id,nnn) = ori(ixyz(id),id)
            enddo ! id
          else  ! nnn <= max_nnn
            trunc = .true.
          endif ! nnn <= max_nnn

        enddo ! ix
      enddo ! iy
    enddo ! iz
#ifdef DEBUG
    if( nnn < 1 ) stop 'typeGRID: periodic_positions: ERROR: at least one image required.'
#endif
    if( trunc ) then
      if(o>0) write(o,'(9A)') sym, fun, WARNING(0), &
        'number of periodic images of an atom has been truncated.'
    endif ! truncated

    ! convert to absolute coords
    do inn = 1, nnn
      origins(:,inn) = forigin(:,inn) * g%s(:) ! diagonal real space basis
#ifdef GENERAL_CELL
      ! add off-diagonal parts
! ! ! ! !   xy - element   g%basis(1,2) = g%shift(1)*g%s(1) !
! ! ! ! !   yz - element   g%basis(2,3) = g%shift(2)*g%s(2) !
! ! ! ! !   xz - element   g%basis(1,3) = g%shift(3)*g%s(1) !
      origins(1,inn) = origins(1,inn) + g%basis(1,2) * forigin(2,inn)
      origins(2,inn) = origins(2,inn) + g%basis(2,3) * forigin(3,inn)
      origins(1,inn) = origins(1,inn) + g%basis(1,3) * forigin(3,inn)

!       ! Alternative
!       !-------------------------------------------------------------------
!       ! mathematically simplest:
!       origins(:,inn) = matmul( g%basis, forigin(:,inn) ) ! general real space basis
#endif
    enddo ! inn


#ifdef FULL_DEBUG
    if( o > 0 ) then
      write(o,'(/,3A,I4,A,3F7.3)') sym, fun, ' nnn =', nnn, ' original pos =', fpos
      do inn = 1, nnn
        write(o,'(3A,3I3,A,3F7.3,9A)') sym, fun, ' ishifts = [', &
                  ishifts(:,inn), ' ] pos = [', origins(:,inn)/g%s(:), ' ].'
      enddo ! inn
    endif ! o > 0
#endif

    if( present( shifts ) ) then
#ifdef DEBUG
      ! check output sizes
      if( size( shifts, 2 ) < max_nnn ) stop 'periodic_positions: dim #2 of SHIFTS does not match.'
      if( size( shifts, 1 ) /= 3 ) stop 'periodic_positions: dim #1 of SHIFTS /= 3'
#endif
      shifts = ishifts

    endif ! present( shifts )

  endfunction ! periodic_positions


  integer function periodic_image( npi, bc, fpos, ishifts, origins ) &
  result( n ) ! number of periodic images in this direction
  !
  ! generates a set of positions in which a periodic image
  ! of the atom appears, with the given boundary condition
  !
  ! Warning: the order is relevant, because the position itself
  ! is assumed to be the first entry [ishifts(1) and origins(1)]
  !
    integer, intent(in)  :: npi ! max number of periodic images
    integer, intent(in)  :: bc(0:1) ! 0:lower, 1:upper
    real, intent(in)     :: fpos ! fractional position [-1/2,1/2]
    integer, intent(out) :: ishifts(2*npi+1) !
    real, intent(out)    :: origins(2*npi+1) !

    character(len=*), parameter :: fun = ' periodic_image: '
    integer :: ii

#ifdef DEBUG
    if( bc(1) == BC_MIRROR ) stop 'typeGRID periodic_image: FATAL ERROR mirror BC is assumed at the lower boundary.'
#endif

    n = 0 ! init
    !----------------------------------------------------------
    selectcase( bc(0) ) ! lower boundary condition
    case( BC_FINITE )
      ! the position itself
      !----------------------------------------------------------
      n = n+1
      ishifts(n) = 0
      origins(n) = fpos
      !----------------------------------------------------------

    case( BC_MIRROR )
      ! the position itself
      n = n+1
      ishifts(n) = 0
      origins(n) = fpos
      ! the mirror image
      n = n+1
      ishifts(n) =  -1  ! phase factor eik^1 or eik^-1 both == -1.
      origins(n) = -1.0 * fpos
      !          -L/2       0       L/2
      !   ---------|--------|--------|--------
      !                  I <-- A

#ifdef GENERAL_CELL
    case( BC_PERIODIC, BC_SHIFTED )
#else
    case( BC_PERIODIC )
#endif
      do ii = -npi, 0 ! like this, the start from the lower left corner
!       do ii = 0, -ni, -1 ! like this, the forst image is the position itself
        n = n+1
        ishifts(n) = ii  ! phase factor eik^1
        origins(n) = fpos + real(ii)
        ! example: ii=1
        !          -L/2       0       L/2
        !   ---------|--------|--------|--------
        !      I <-------------- A
      enddo ! ii

      selectcase( bc(1) ) ! upper boundary condition
#ifdef GENERAL_CELL
      case( BC_PERIODIC, BC_SHIFTED )
#else
      case( BC_PERIODIC )
#endif
        do ii = 1, npi ! do not start from 0 here, because image#0 has been counted above
          n = n+1
          ishifts(n) = ii  ! phase factor eik^1
          origins(n) = fpos + real(ii)
          ! example: ii=1
          !          -L/2       0       L/2
          !   ---------|--------|--------|--------
          !       A -------------> I
        enddo ! ii
      endselect ! bc(1)
      !----------------------------------------------------------

    endselect ! bc(0)
    !----------------------------------------------------------

  endfunction ! periodic_image


  status_t function parallelize_grid( g, nproc, comm, rank, rnk, neighbor, &   ! required
                      equi_comm, equi_rank, equi_rnk ) result( ist )   ! optional
  use configuration, only: EqualDomainSizes, ERROR, WARNING
  use configuration, only: o ! output unit, 0: no output
  use MPIconst, only: MPI_COMM_SELF
    type(grid), intent(inout)         :: g
    integer, intent(in)               :: nproc(3)
    MPI_Comm, intent(in)              :: comm
    integer, intent(in)               :: rank
    integer, intent(in)               :: rnk(3)
    integer, intent(in)               :: neighbor(3,2)
    MPI_Comm, intent(in), optional    :: equi_comm
    integer, intent(in), optional     :: equi_rank, equi_rnk(3)

    character(len=*), parameter       :: fun = ' parallelize_grid: '
    character, parameter              :: DIR(1:3) = (/'x','y','z'/)
    integer                           :: id, ise, ir
    real                              :: balance, bal(1:3)

    ist = 0

    if( any( nproc < 1 ) ) stop 'parallelize_grid: needs positive #processors'

    g%nproc    = nproc
    g%nprocs   = product(nproc(1:3))
    g%comm     = comm
    g%rank     = rank
    g%rnk(1:3) = rnk(1:3)
    g%neighbor = neighbor

    if( present( equi_comm ) ) then
                                 g%equi_comm     = equi_comm
      if( present( equi_rank ) ) g%equi_rank     = equi_rank
      if( present( equi_rnk ) )  g%equi_rnk(1:3) = equi_rnk(1:3)
    else  ! present equi_comm
      g%equi_comm     = MPI_COMM_SELF
      g%equi_rank     = 0
      g%equi_rnk(1:3) = 0
    endif ! present equi_comm

    ! distribute
    if( EqualDomainSizes ) then

      g%ng(1:3) = (g%ng_all(1:3)-1)/g%nproc(1:3) + 1 ! integer divide
      ! adjust the total number if grid points
      if( o>0 .and. any( g%ng(1:3)*g%nproc(1:3) > g%ng_all(1:3) ) ) then
        write(o,'(4A,3I8,A)') sym, fun, WARNING(0), 'adjust number of grid points [', g%ng_all(1:3), ' ]'
        write(o,'(3A,3I8,A)') sym, fun, '... due to equal domain sizes to [', g%ng(1:3)*g%nproc(1:3), ' ]'
      endif ! adjusting
      g%ng_all(1:3) = g%ng(1:3)*g%nproc(1:3) ! adjust the total number of grid points

      g%h = g%s/real(g%ng_all(1:3)) ! adjust the grid spacing h
      g%hvol = product(g%h) ! and the volume element

      g%ioff(1:3) = g%rnk(1:3) * g%ng(1:3) ! integer grid point offset

    else  ! EqualDomainSizes

      do id = 1, 3
        bal(id) = distribute_LB( Nall=g%ng_all(id), nproc=g%nproc(id), irnk=g%rnk(id), & ! input
                                 ng=g%ng(id), ioff=g%ioff(id) ) ! output
      enddo ! id
      ! since g%ng_all has not changed,
      ! g%h and g%hvol do not need to be adjusted
      balance = product(bal(1:3))
      if(o>0 .and. balance < 0.999) write(o,'(3A,F0.1,9A)') sym, fun, &
          'estimated load balance ', balance*100., ' % (min/max ratio)'

    endif ! EqualDomainSizes

    !===================================================================================
    ! compute position offsets:
    !===================================================================================

    ! vector offset: the vector v(i1,i2,i3) points to the center of
    ! the grid cell, i.e. v(i1,i2,i3) = (/i1,i2,i3/)*h + g%off
    ! ==> 3 terms:
    !         cell center is the middle of the total grid  -->  -s/2
    !         domain decomposition gives the integer offset --> ioff*h
    !         position at the center of mass of the grid cell --> -h/2
    g%off = -0.5*g%s + ( -0.5 + g%ioff )*g%h

    ! for MIRROR bc, the center of the unit cell is at the lower corner ==> no -s/2 term
    do id = 1, 3
      if( g%bc(id,1) == BC_MIRROR ) then
        g%off(id) = ( -0.5 + g%ioff(id) )*g%h(id)
      endif ! bc == MIRROR
    enddo ! id

#ifdef DEBUG
    if(o>0) write(o,'(3A,3I3,A,3F10.3)') sym, fun, 'rnk=', g%rnk, ' g%off=', g%off
    if(o>0) write(o,'(3A,I9,A,3I10)') sym, fun, 'rank', g%rank, ' g%ioff', g%ioff
#endif
    !===================================================================================

#ifdef GENERAL_CELL
    stop 'NOT IMPLEMENTED: tGRID line 760'
#endif

    ! set the technical BC for the inner interfaces of domains
    do id = 1, 3
      do ise = 1, 2
        !  lowest rank = 0    (ise==1) and
        ! highest rank = Np-1 (ise==2)
        ir = 0 + (ise-1)*(g%nproc(id)-1)

        g%tbc(id,ise) = BC_INNER ! default for domain decomposition

        if( g%rnk(id) == ir ) then
          ! highest or lowest rank
          ! according the the physical boundary conditions g%bc:
          selectcase( g%bc(id,ise) )
          case( BC_FINITE )   ; g%tbc(id,ise) = BC_FINITE   ! task: set halos to zero, no communication
          case( BC_PERIODIC ) ; g%tbc(id,ise) = BC_PERIODIC ! task: send with Bloch factor, receive
          case( BC_MIRROR )   ; g%tbc(id,ise) = BC_MIRROR   ! task: mirror copy, no communication
          case default ; stop 'parallelize_grid: ERROR: unknown key for boundary condition.'
          endselect ! bc
        endif ! rnk == highest or lowest
#ifdef FULL_DEBUG
        if(o>0) write(o,'(3A,3I4,9(A,I2))') sym, fun, 'cart_rank=', g%rnk, ' id=',id,' ise=',ise,' bc=', g%tbc(id,ise)
#endif

      enddo ! ise
    enddo ! id

  endfunction ! parallelize_grid


  real function distribute_LB( Nall, nproc, irnk, ng, ioff ) result( balance )
  use configuration, only: o ! output unit
    integer, intent(in)             :: Nall !! number of all grid points
    integer, intent(in)             :: Nproc !! number of all processes
    integer, intent(in)             :: irnk !! rank if this process
    integer, intent(out)            :: ng !! number of grid points in this domain
    integer, intent(out)            :: ioff !! grid point offset for this domain

    !! if ever the number of grid points per domain is not equal,
    !! then the distribution of grid points should look like
    !!
    !! if isolated, we can expect that there are no atoms
    !! close to the iBC, then we can give more gridpoints to
    !! those processes close to the boundary.
    !!   iBC| n>,n>,n<,n<,...,n<,n>,n>,n> |iBC
    !!          2x      (N-5)x     3x
    !! if the number of n>''s is odd, place the smaller half to the lower BC, because
    !! in case the number of n> is 1, the master does not need the longest time
    !! (unfortunate because he''s got other stuff to do)
    !!
    !! if periodic, we can expect atoms everywhere,
    !!   pBC| n<,n<,n<,n>,...,n>,n>,n<,n< |pBC
    !!          3x      (N-5)x     2x
    !! the processes at the boundary need to perform a phase factor
    !! multiplication or in the case of some
    !! special BC (shifted, skew, energy-dep.) even more operations
    !! for the same reason as above
    !! the lower number of n< should be >= the upper nuber of n<

    ! the total number of grid points does not change, but
    ! the lower ranks may have less grid points

    character(len=*), parameter :: fun = ' distribute: '
    integer :: nmn, nmx, pmn, pmx, pn(3), nn(3), k, ir, io, i

    ng = -1 ! not successful

    nmn = Nall/Nproc ! integer divide
    if( nmn < 1 ) stop 'distribute: the minimum number of grid points per domain is < 1'
    nmx = (Nall-1)/Nproc +1 ! integer divide

    ! find a distribution, such that n<*nmn + (nproc-nless)*nmx = ng_all
    if( nmx == nmn ) then
      pmx = 0 ! no process has more grid points than th other
    elseif( nmx == nmn+1 ) then ! differ by one
      pmx = Nall - Nproc * nmn ! pmx grid points are left to be distributed
#ifdef DEBUG
      if( pmx < 0 ) stop 'distribute: fatal ERROR.'
#endif
    else ; stop 'distribute: fatal ERROR in integer division.'
    endif ! nmx == nmn
    pmn = Nproc - pmx ! pmn processes have the smaller number of grid points

    balance = real(nmn)/real(nmx) ! ratio
#ifdef DEBUG
    if(o>0) write(o,'(2A,9(I0,A))') sym, fun, Nall,' = ',pmn,' x ',nmn,' + ',pmx,' x ',nmx
#endif

    nn = nmx ; nn(2) = nmn ! center gets less grid points
    pn(1) = (pmx+0)/2     ! int div
    pn(2) =  pmn
    pn(3) = (pmx+1)/2 ! int div
#ifdef DEBUG
    if( sum( pn*nn ) /= Nall ) stop 'distribute: fatal implementation error, check integer divisions! (Nall)'
    if( sum( pn ) /= Nproc ) stop 'distribute: fatal implementation error, check integer divisions! (Nproc)'
#endif
    ! display
    if( pn(3) < 1 ) then ! this means that pmx == 0
      if(o>0) write(o,'(2A,I6,9(A,I3,A,I4))') sym, fun, Nall,' =          ',           pn(2),'x',nn(2)
    elseif( pn(1) < 1 ) then ! this means that pmx == 1
      if(o>0) write(o,'(2A,I6,9(A,I3,A,I4))') sym, fun, Nall,' =          ',           pn(2),'x',nn(2),' +',pn(3),'x',nn(3)
    else  ! this means that pmx > 1
      if(o>0) write(o,'(2A,I6,9(A,I3,A,I4))') sym, fun, Nall,' =',pn(1),'x',nn(1),' +',pn(2),'x',nn(2),' +',pn(3),'x',nn(3)
    endif ! ...

    io = 0 ! init grid point index offset counter
    ir = 0 ! init domain rank counter
    do k = 1, 3
      do i = 1, pn(k)
        if( irnk == ir ) then
          ioff = io
          ng = nn(k)
#ifndef DEBUG
          return ! successful
#endif
        endif ! irnk matches
        ir = ir+1     ! forward domain rank
        io = io+nn(k) ! forward grid point index offset
      enddo ! i
    enddo ! k
#ifdef DEBUG
    if( io /= Nall ) stop 'distribute: fatal implementation error, check integer divisions! (Nall) 2nd'
    if( ir /= Nproc ) stop 'distribute: fatal implementation error, check integer divisions! (Nproc) 2nd'
#endif
    if( ng <= 0 ) then
      write(*,'(3A,9(I0,A))') sym, fun, 'Nall=',Nall,' Nproc=',Nproc,' irnk=',irnk,'  ==>  ioff=',ioff,' ng=', ng
      write(*,'(2A,99(I0,A))') sym, fun, Nall,' = ',pmn,'x',nmn,'+',pmx,'x',nmx, &
       ' = ',pn(1),'x',nn(1),'+',pn(2),'x',nn(2),'+',pn(3),'x',nn(3)!,' ir=',ir,' io=',io
      stop 'tGRID: distribution failed, maybe the grid rank is ill!'
    endif ! ng <= 0
  endfunction ! distribute_LB




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! TOOLS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function distribute( ntot, n, equal, minmax ) result( dis )
    integer, intent(in) :: ntot, n
    logical, intent(out), optional :: equal
    integer, intent(out), optional :: minmax(2)

    integer               :: dis(0:n-1)
    integer               :: i1, ns

    if( ntot < 0 ) stop 'distribute: ntot < 0'
    if( n    < 1 ) stop 'distribute: n    < 1'

    ns = 0
    do i1=0,n-1
      dis(i1) = int(ntot/n)
      ns = ns + dis(i1)
    enddo ! i1
    i1 = n-1
    do while( ns < ntot )
      dis(i1) = dis(i1) + 1
      i1 = i1 - 1
      ns = ns + 1
    enddo ! while

    if( present(minmax) ) minmax = (/ dis(0),dis(n-1) /)
    if( present(equal) ) equal  = ( dis(0) == dis(n-1) )

    if( sum(dis) /= ntot ) stop 'distribute: malfunction'

  endfunction ! distribute



  integer function idistribute( ntot, n, m, equal, minmax ) result( idis )
    integer, intent(in) :: ntot, n, m
    logical, intent(out), optional :: equal
    integer, intent(out), optional :: minmax(2)

    logical, parameter :: STOPONERROR = .true.
    integer :: mn, lm ! minimum, limit

    if( ntot < 0 ) stop 'idistribute: ntot < 0'
    if( n    < 1 ) stop 'idistribute: n    < 1'
!         if(i<1.or.i>n) stop 'idistribute: i not in [1,n]'

    mn = int(ntot/n) ! the smaller number
    lm = ntot - n * mn ! the limit from where to fill with the larger number (nm+1)

    if( lm >= n ) stop 'idstribute: fatal error'

    if( m < 0 ) then
      if( STOPONERROR ) stop 'idistribute: i < 1'
      idis = 0
    elseif( m > n-1 ) then
      if( STOPONERROR ) stop 'idistribute: i > n'
      idis = 0
    elseif( m >= n - lm ) then
      idis = mn+1
    else  !
      idis = mn
    endif !

    if( present(minmax) ) minmax = (/ mn, mn+1 /)
    if( present(equal) ) equal = ( lm == 0 )

  endfunction ! idistribute

#ifdef EXTENDED
  status_t function test( ) result( ios )
    write(*,*,iostat=ios) __FILE__,' no module test implemented!'
  endfunction ! test
#endif

endmodule ! type_grid

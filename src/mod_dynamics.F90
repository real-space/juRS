#include "config.h"

#define DEBUG
! #define FULL_DEBUG


#ifdef DEBUG
#define cDBG  
#else
#define cDBG !DBG
#endif



!! @author Paul Baumeister
!! @version 3.0
!!
!! move the atoms
module dynamics
!
! dynamics of the atomic positions
! in the framework of the Born-Oppenheimer approximation
!
  use configuration, only: o ! output unit, 0: no output
implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: sym = 'DYN' !! module symbol

  public :: adjust_positions_to_grid
#ifdef EXTENDED
  public :: test
#endif

  character, parameter :: DIRECTION(0:4) = (/'?','x','y','z','#'/)

  contains

  status_t function adjust_positions_to_grid( g_old, g_new, a, mode ) result( ist )
  use configuration, only: WARNING
  use type_atom, only: atom, TYPE_ATOM_POS_ABSOLUTE, TYPE_ATOM_POS_FRACTIONAL
  use type_grid, only: grid
    type(grid), intent(in)                 :: g_old, g_new
    type(atom), intent(inout)              :: a(:)
    character(len=*), intent(in), optional :: mode

    character(len=*), parameter     :: fun = ' adjust_positions_to_grid: '
    integer, parameter :: KEY_MODE_FRAC=-1 ! all relative
    integer, parameter :: KEY_MODE_INP=0 ! depending on input
    integer, parameter :: KEY_MODE_ABS=1 ! all absolute
    integer :: id, nmiss, ia, na, key_mode
    real    :: change(3) ! ratio of change

    ! check if relative and absolute coordinates are consistent with the old grid
    nmiss = check_atomic_positions( g_old, a )

#ifdef GENERAL_CELL
    stop 'adjust_positions_to_grid: STOP, GENERAL_CELL not implemented.'
#endif

    ! output
    if(o>0) then
      do id = 1, 3
        change(id) = g_new%s(id)/g_old%s(id)
      enddo ! id
      if( all(change == 1.0 ) ) then
        write(o,'(9A)') sym, fun, 'no change of cell size!'
      else !
        do id = 1, 3
          if( change(id) /= 1.0 ) write(o,'(3A,F7.1,9A)') sym, fun, 'change of', &
            (change(id)-1.)*100., ' % in ', achar(119+id), '-direction.'
        enddo ! id
      endif ! all change
    endif ! o/=0


    if( present( mode ) ) then
      selectcase( mode )
      case( 'fractional', 'frac' ) ; key_mode = KEY_MODE_FRAC
      case( 'absolute', 'abs' ) ; key_mode = KEY_MODE_ABS
      case( 'input', 'inp' ) ; key_mode = KEY_MODE_INP
      case default ; key_mode = KEY_MODE_INP
      endselect ! mode
    else ; key_mode = KEY_MODE_INP ! default
#ifdef DEBUG
      if(o>0) write(o,'(9A)') sym, fun, 'has been called without argument mode, use default'
#endif
    endif ! present mode


    na = size( a, 1 ) ! number of atoms


    selectcase( key_mode )
    !==================================================================================
    case( KEY_MODE_FRAC )
      if(o>0) write(o,'(9A)') sym, fun, &
        'atomic positions will be transformed keeping the fractional coordinates'
      ! the new coordinate is determined from the relative position
      do ia = 1, na
        a(ia)%pos = g_new%s * a(ia)%relpos
      enddo ! ia
    !==================================================================================
    case( KEY_MODE_ABS )
      if(o>0) write(o,'(9A)') sym, fun, &
        'atomic positions will be transformed keeping the absolute coordinates'
      ! the new coordinate is determined from the absolute position
      do ia = 1, na
        a(ia)%relpos = a(ia)%pos / g_new%s
      enddo ! ia
    !==================================================================================
    case( KEY_MODE_INP )
      if(o>0) write(o,'(9A)') sym, fun, &
        'atomic positions will be transformed according to their way of input (abs/frac)'
      ! the new coordinate is determined from the absolute position
      do ia = 1, na
        selectcase( a(ia)%key_pos )
        case( TYPE_ATOM_POS_ABSOLUTE )   ; a(ia)%relpos = a(ia)%pos / g_new%s
        case( TYPE_ATOM_POS_FRACTIONAL ) ; a(ia)%pos = g_new%s * a(ia)%relpos
        case default ; stop 'adjust_positions_to_grid: a%KEY_POS is set to an undetermined value.'
        endselect ! a(ia)%key_pos
      enddo ! ia
    !==================================================================================
    case default ; stop 'adjust_positions_to_grid: no such key'
    endselect ! key_mode

#ifdef DEBUG
    ! check if relative and absolute coordinates are consistent with the new grid
    nmiss = check_atomic_positions( g_old, a, threshold=1.E-12 )
    if( nmiss > 0 .and. o>0) write(o,'(2A,I4,9A)') sym, fun, nmiss, ' positions are wrong on the new grid.'
#endif
    ist = 0
  endfunction ! adjust_positions_to_grid

  status_t function check_atomic_positions( g, a, threshold ) result( ist )
  use configuration, only: WARNING
  use type_atom, only: atom
  use type_grid, only: grid
  use unitsystem, only: Ang, Ang_
    type(grid), intent(in)     :: g
    type(atom), intent(inout)  :: a(:)
    real, intent(in), optional :: threshold

    character(len=*), parameter :: fun = ' check_atomic_positions: '
    integer :: id, ia, na, nmiss(3)
    real :: thres, a_err, v_err(3) ! vector error and absolute

    thres = 1E-6 ; if( present( threshold ) ) thres = abs( threshold )

    na = size( a, 1 ) ! number of atoms

    nmiss(1:3) = 0

    do ia = 1, na
      ! check if there is a missmatch with the grid
      v_err = a(ia)%pos - g%s*a(ia)%relpos
      do id = 1, 3
        a_err = abs( v_err(id) )
        if( a_err > thres ) then
#ifdef DEBUG
          if(o>0) write(o,'(3A,I6,A,F10.6,9A)') sym, fun, &
            'for atom #', a(ia)%ja, ' frac. and abs. coords differ by', &
            a_err*Ang, Ang_, ' in ', direction(id), '-direction.'
#endif
          nmiss(id) = nmiss(id)+1 ! count up
        endif ! |v_err| > 1E-6
      enddo ! id
    enddo ! ia

    if(o>0) then
      do id = 1, 3
        write(o,'(3A,I6,A,F10.6,9A)') sym, fun, WARNING(0), &
          nmiss(id), 'frac. and abs. coords differ by more than', &
          thres*Ang, Ang_, ' in ', direction(id), '-direction.'
      enddo ! id
    endif ! o/=0

    ist = sum( nmiss ) ! number of wrong coordinates
  endfunction ! check_atomic_positions

  real function norm2( v )
    real, intent(in) :: v(1:3)
    norm2 = sqrt( v(1)*v(1) + v(2)*v(2) + v(3)*v(3) )
  endfunction ! norm2

#ifdef EXTENDED
!+ extended

  status_t function timestep( o, a, dt, g, friction ) result( ist )
  use constants, only: NUCLEON_MASS
  use configuration, only: WARNING
  use type_atom, only: atom
  use type_grid, only: grid
    iounit_t, intent(in)             :: o !! output unit
    type(atom), intent(inout)        :: a(:) !! atoms
    real, intent(in)                 :: dt !! time step
    type(grid), intent(in), optional :: g !! grid descriptor
    real, intent(in), optional       :: friction !! friction

    character(len=*), parameter     :: fun = ' timestep: '
    real, parameter :: FEMTOSEC = 1.0
    integer :: ia
    real :: acc(1:3), fric, dvel(1:3), dpos(1:3), m ! mass
#ifdef DEBUG
    integer :: nmiss

    if( present( g ) ) then
      ! check if relative and absolute coordinates are consistent with the grid
      nmiss = check_atomic_positions( g, a )
    else ; if(o>0) write(o,'(9A)') sym, fun, 'no grid given ==> relative positions cannot be updated'
    endif ! present( g )

    if(o>0) write(o,'(3A,ES10.2E2,9A)') sym, fun, 'Delta t =', dt*FEMTOSEC, ' au' !' fs'
#endif

    fric = 0. ; if( present( friction ) ) fric = abs(friction)

    do ia = 1, size(a)
      m = a(ia)%weight
#ifdef DEBUG
      ! no atoms are lighter than Hydrogen = 1.00794 ( > 1.0 )
      if( m < 1.0 * NUCLEON_MASS ) stop 'dynamics: atomic mass is too light!'
#endif

      ! Caution: this expression can lead to friction forces that point the other way
      acc = ( a(ia)%frc - a(ia)%vel * fric  ) / m

#ifdef FULL_DEBUG
      if(o>0) then
        write(o,'(A)') ''
        write(o,'(3A,I0,3A,F5.1,9A)') sym, fun, 'atom #', a(ia)%ja, '  ', a(ia)%s%sym, '   mass =', m/NUCLEON_MASS, ' u'
        write(o,'(3A,3(3F10.3,A))') sym, fun, ' pos = [', a(ia)%pos, ' ]  frc = [', a(ia)%frc, ' ]'
        write(o,'(3A,3(3F10.3,A))') sym, fun, ' vel = [', a(ia)%vel, ' ]  acc = [', acc,       ' ]'
      endif ! o > 0
#endif
      ! update position
      dpos = a(ia)%vel * dt + 0.5 * acc * dt**2
      a(ia)%pos = a(ia)%pos + dpos * a(ia)%fix ! if the fix-values are 0., there is no displacement
      ! update velocity
      dvel = acc * dt
      a(ia)%vel = a(ia)%vel + dvel
      a(ia)%vel = a(ia)%vel * a(ia)%fix ! if fix is < 1, kinetic energy ist lost

#ifdef FULL_DEBUG
      if(o>0) then
        write(o,'(3A,ES16.6E2,A)') sym, fun, 'after dt =', dt
        write(o,'(3A,3(3F10.3,A))') sym, fun, ' vel = [', a(ia)%vel, ' ]  pos = [', a(ia)%pos, ' ]'
      endif ! o > 0
#endif
    enddo ! ia

    if( present( g ) ) then ! update the relative positions
      do ia = 1, size(a)
        a(ia)%relpos = a(ia)%pos / g%s
      enddo ! ia
    endif ! present( g )
    ist = 0
  endfunction ! timestep


  status_t function test( ) result( ios )
  use constants, only: NUCLEON_MASS
  use constants, only: ANGSTROM
  use type_species, only: species, species_set
  use type_atom, only: atom, atom_set, write_xyz_file
    character(len=*), parameter     :: fun = ' test: '
    real, parameter                 :: inANG = 1.0/ANGSTROM
    type(species), target           :: spec(0:9)
    type(atom), allocatable         :: a(:)
    integer                         :: natoms, ia, k
    iounit_t                        :: iu=13
    character(len=3)                :: ch3
    real                            :: pos(3)

    ! get atoms from file
    spec(0) = species_set( Z=6., weight=12.) ! Carbon
    spec(1) = species_set( Z=32., weight=72.61 ) ! Germanium
    spec(2) = species_set( Z=51., weight=121.76 ) ! Antimony
    spec(3) = species_set( Z=52., weight=127.6 ) ! Tellurium
    spec(4) = species_set( Z=38., weight=87.62 ) ! Strontium
    spec(5) = species_set( Z=22., weight=47.867 ) ! Titanium
    spec(6) = species_set( Z=8., weight=16.0 ) ! Oxygen

    open(iu,file='pos.in',iostat=ios,action='read',status='old')
    if(ios/=0) stop 'DYN test: opening of file "pos.in" failed.'
    read(iu,*,IOstat=ios) natoms
    if(ios/=0) stop 'DYN test: 1st line in file "pos.in" must contain the number of atoms.'
    read(iu,*,IOstat=ios) ch3 ! comment
    allocate( a(natoms) )
    do ia = 1, natoms
      read(iu,*,IOstat=ios) ch3, pos ! read in 3chars and 3 coordinates
      if(ios/=0) stop 'DYN test: reading of file "pos.in" failed.'

      selectcase( ch3 )
      case( 'Ge' ) ; k=1
      case( 'Sb' ) ; k=2
      case( 'Te' ) ; k=3
      case( 'Sr' ) ; k=4
      case( 'Ti' ) ; k=5
      case( 'O ' ) ; k=6
      case default ; k=0
      endselect ! ch3

      if(o>0) write(o,'(3A,I6,2A,3F10.6)') sym, fun, 'input coords atom#', ia, '   ', ch3, pos
      a(ia) = atom_set( spec(k), pos*inANG, ja=ia )
      a(ia)%vel = 0.0
      a(ia)%fix = 1.0 ! not fixed
    enddo ! ia
    close(iu)

!     ! molecular dynamics with the velocity verlet algorithm
!     call test_md( a )

    ! structural relaxation with the gdiis method
    ios = test_gdiis( a )


    ios = write_xyz_file( 'pos_out', a, comment='test dynamics.mod' ) ! in Angstrom
    ! write .out file in atomic units
    open(17,file='pos.out')
    write(17,'(I8,A)') natoms, ' # number of atoms'
    write(17,'(I3,3F16.10)') ( a(ia)%s%iZ, a(ia)%pos, ia=1,natoms )
    close(17)

    stop 'DYN test: done'

!     ! test empirical Lennard-Jones-6-12-potential
!     do ir = 10, 1000 ; r = dr*ir
!       ep = empirical_6_12_potential( (/r,0.,0./) )
!       frc(1) = -empirical_6_12_potential( (/r,0.,0./), derive2i=1 )
!       write(11,'(3ES16.6E2)') r, min(ep,mx), min(frc(1),mx)
!     enddo ! ir
!     if(o>0) write(o,'(9A)') sym, fun, 'empirical potential written to fort.11'

  endfunction ! test



  status_t function test_md( a ) result( ist )
  use type_atom, only: atom
    type(atom), intent(inout)       :: a(:)

    character(len=*), parameter     :: fs_ = ' fs' ! time unit
    real, parameter                 :: fs = 0.0241888432655553 ! femtoseconds
    character(len=*), parameter     :: fun = ' test_md: '
    integer                         :: natoms, ia, id
    integer                         :: itime, ntimesteps=500
    real                            :: time, dtime=1.0
    real                            :: et, ep, ek, frc(3)

    natoms = size(a,1)
    if(o>0) write(o,'(2A,I0,9A)') sym, fun, natoms, ' atoms'

    do itime = 0, ntimesteps
      ! ---------------------------------------------------------------------------------
      time = itime * dtime
!       if(o>0) write(o,'(3A,F10.3,9A)') sym, fun, 'time =', time*fs, fs_

      ek = 0.
      do ia = 1, natoms
        ek = ek + 0.5*a(ia)%weight * sum( a(ia)%vel(1:3)**2 ) ! 1/2 m v^2
      enddo ! ia

      ! compute potential energy and forces
      ep = get_potential_energy( a )

      et = ep + ek
      write(4,'(9F24.12)') time*fs, et, ek, ep

      if( mod(itime,128) == 0 ) then
        if(o>0) write(o,'(3A,3(F10.3,A))') sym, fun, 'energy ', ek, ' +', ep, ' =', et, ' Ha'
      endif ! it%16

      ! update atomic positions
      ist = timestep( 0, a, dtime, friction=8.0 )
      ! ---------------------------------------------------------------------------------
    enddo ! itime
    if(o>0) write(o,'(3A,3(F10.3,A))') sym, fun, 'energy ', ek, ' +', ep, ' =', et, ' Ha'

    if(o>0) write(o,'(9A)') sym, fun, 'forces'
!     do ia = 1, natoms
!       if(o>0) write(o,'(2A,I6,A,3F10.3)') sym, fun, ia, ' frc=', a(ia)%frc
!     enddo ! ia
    do id = 1, 3
      frc(id) = maxval(abs(a(:)%frc(id)))
    enddo ! id
    if(o>0) write(o,'(3A,3F10.3)') sym, fun, ' max |frc|=', frc
    if(o>0) write(o,'(3A,3F10.3)') sym, fun, ' max  frc =', maxval(frc)

  endfunction ! test_md


  status_t function test_gdiis( a ) result( ist )
  use type_atom, only: atom, write_xyz_style
    type(atom), intent(inout) :: a(:)

    character(len=*), parameter :: fun = ' test_gdiis: '
    integer, parameter :: MAXITER = 99 ! hard limit
    iounit_t                        :: iu = 13, tu = 15, ou = 0 ! 8  0: no output
    integer                         :: natoms, ia, iit, id, idof, ndof ! degrees of freedom
    real                            :: Epot, frc(3)!, pos(3), vel(3), dpos(3)
    real, allocatable               :: frcs(:), poss(:)
    integer, allocatable            :: indx(:)
    logical                         :: converged

    natoms = size(a,1) ! number of atoms
    if(o>0) write(o,'(2A,I0,9A)') sym, fun, natoms, ' atoms'

    ! create list of degrees of freedom
    allocate( indx(3*natoms) )
    ndof = 0
    do ia = 1, natoms
      do id = 1, 3
        if( a(ia)%fix(id) > 0. ) then
          ndof = ndof+1
          indx(ndof) = 4*ia+id ! <==> ia = indx/4, id = indx-4*ia
        endif ! atom can move
      enddo ! id
    enddo ! ia
    if(o>0) write(o,'(2A,9(I0,A))') sym, fun, ndof, ' (of max.', 3*natoms, ') degrees of freedom'
    allocate( frcs(ndof), poss(ndof) )

    open( unit=tu, file='trajectory.xyz', status='unknown' ) ! open trajectory file

    ! iterate until the forces converged to zero
    iit = 0 ! init
    converged = .false.
    do while( .not. converged .and. iit < MAXITER )
      ! ---------------------------------------------------------------------------------
      iit = iit+1 ! count up ! if(o>0) write(o,'(3A,I6,9A)') sym, fun, 'iit =', iit

      ! compute potential energy and forces
      Epot = get_potential_energy( a )
      if(o>0) write(o,'(3A,I4,A,F16.6,9A)') sym, fun, 'it#', iit, ' [Epot]', Epot, ' Ha'

      ! copy forces and positions
      do idof = 1, ndof
        ia = indx(idof)/4
        id = indx(idof)-4*ia
        frcs(idof) = a(ia)%frc(id)
        poss(idof) = a(ia)%pos(id)
      enddo ! idof

      ! update atomic positions using the DIIS method
      converged = gdiis( iit, frcs, poss, 1., ist, Criterion='max', Threshold=1.E-4 )

      ! set new positions
      do idof = 1, ndof
        ia = indx(idof)/4
        id = indx(idof)-4*ia
        a(ia)%dis(id) = poss(idof)-a(ia)%pos(id) ! here one could limit the displacement
        a(ia)%pos(id) = poss(idof) ! new position
      enddo ! idof

      ! write to the trajectory file
      ist = write_xyz_style( tu, a, comment='test gdiis', iteration=iit ) ! in Angstrom
      ! ---------------------------------------------------------------------------------
    enddo ! while

    close( unit=tu ) ! close trajectory file

    do id = 1, 3
      frc(id) = maxval(abs(a(:)%frc(id)))
    enddo ! id
    if(o>0) write(o,'(3A,3F10.3)') sym, fun, ' max |frc|=', frc
    if(o>0) write(o,'(3A,3F10.3)') sym, fun, ' max  frc =', maxval(frc)

  endfunction ! test_gdiis



  real function get_potential_energy( a ) result( E )
  use type_atom, only: atom
    type(atom), intent(inout) :: a(:)

    integer :: natoms, ia1, ia2
    real :: dpos(3), frc(3)

    natoms = size(a,1) ! number of atoms
    ! get forces and potential energy
    forall( ia1=1:natoms ) a(ia1)%frc = 0. ! init forces
    E = 0. ! init potential energy
    do ia1 = 1, natoms
      do ia2 = 1, ia1-1 ! avoid self-interaction
        dpos(1:3) = a(ia2)%pos - a(ia1)%pos
        E = E  + empirical_6_12_potential( dpos, derive2i=0 )
        frc(1) = empirical_6_12_potential( dpos, derive2i=1 )
        frc(2) = empirical_6_12_potential( dpos, derive2i=2 )
        frc(3) = empirical_6_12_potential( dpos, derive2i=3 )
        a(ia1)%frc = a(ia1)%frc + frc
        a(ia2)%frc = a(ia2)%frc - frc
      enddo ! ia2
    enddo ! ia1

  contains

    real function empirical_6_12_potential( rv, derive2i ) result( E )
    use constants, only: ANGSTROM
      real, intent(in) :: rv(1:3)
      integer, intent(in) :: derive2i

      character(len=*), parameter :: fun = ' empirical_6_12_potential: '
      real, parameter :: a0 =  3.02/ANGSTROM
      real, parameter :: E0 = -0.1 ! Ha
      real, parameter :: C6  =  2.*E0*a0**6
      real, parameter :: C12 = -1.*E0*a0**12
      real :: r2, ri

      r2 = rv(1)*rv(1) + rv(2)*rv(2) + rv(3)*rv(3)

      if( r2 < 1.E-12 ) r2 = 1.E-12

      if( derive2i > 0 ) then
#ifdef DEBUG
        if( derive2i > 3 ) stop 'empirical_6_12_potential: DERIVE2I out of bounds [1,3]'
#endif
        ri = rv(derive2i)
        E = -12. * C12 * r2**(-7) * ri - 6.* C6 * r2**(-4) * ri ! r^12 - r^6
#ifdef FULL_DEBUG
        if(o>0) write(o,'(3A,I2,9(A,F10.6))') sym, fun, 'd2i', derive2i, '  F =', E, ' Ha/aB, r=', sqrt(r2)
#endif
      else
        ! potential energy
        E = C12 * r2**(-6) + C6 * r2**(-3) ! r^12 - r^6
#ifdef FULL_DEBUG
        if(o>0) write(o,'(2A,9(F10.6,A))') sym, fun, E, ' Ha, r=', sqrt(r2)
#endif
      endif ! derive2i > 0

    endfunction ! empirical_6_12_potential

  endfunction ! get_potential_energy


  logical function gdiis( iit, grd, pos, Dt2mass, ist, &
     criterion, Threshold, comm, constraint ) result( converged )
  use configuration, only: WARNING, o
  use LAPACK, only: Solve_Ax_b
  use MPItools, only: MPIallsum
    integer, intent(in)                    :: iit !! number of the iteration
    real, intent(inout)                    :: pos(:) !! positions, will be updated
    real, intent(inout)                    :: grd(:) !! gradients, will be changed
    real, intent(in)                       :: Dt2mass !! DeltaT^2/Mass
    status_t, intent(out)                  :: ist !! status
    character(len=*), intent(in), optional :: criterion !! convergence criterion for force
    real, intent(in), optional             :: Threshold !! for force
    MPI_Comm, intent(in), optional         :: comm !! MPI communicator
    real, intent(in), optional             :: constraint(:) !! not implemented, please constrain grd in advance

    character(len=*), parameter     :: fun = ' gdiis: '
    real, parameter                 :: DEF_THRE = 1.E-3
    integer, parameter              :: NHISTMAX = 24
    integer, parameter              :: IP=1, IG=2

    real, allocatable, save         :: hist(:,:,:) ! (ndof,nhist,IP:IG)
    integer, save                   :: ihist=0, nhist=0

    integer                         :: ndof ! number of degrees of freedom
    integer                         :: i1, i2
    real                            :: dt2m, mam(-1:+2), thre
    real, allocatable               :: A(:,:), b(:), x(:)!, grd_n(:)

    dt2m = abs(Dt2mass)
    if( dt2m < 1E-6 ) then
      if(o>0) write(o,'(4A,ES12.3,9A)') sym, fun, WARNING(0), 'DeltaT^2/Mass =', dt2m, ' is very small!'
    endif ! dt2m very small
    thre = DEF_THRE ; if( present(Threshold) ) thre = max(1.E-12,abs(Threshold))

    ndof = size( grd )
cDBG  if( ndof /= size( pos ) ) stop 'DYN gdiis: dim of GRD and POS must match!'

    if( iit == 1 ) then ! first iteration
      if( allocated( hist ) ) deallocate( hist, stat=ist )
cDBG    if(o>0) write(o,'(3A,F12.3,9A)') sym, fun, 'allocate', 2*ndof*nhistmax*8./2.**10, ' kiByte of history.'
      allocate( hist(ndof,nhistmax,IP:IG), stat=ist )
      if( ist /= 0 ) stop 'DYN gdiis: failed to allocate HIST array.'
      hist  = 0. ! init
      nhist = 0  ! init
    endif ! iit == 1

    ihist = modulo( iit-1, nhistmax )+1
! cDBG if(o>0) write(o,'(3A,I3,A,I6,9A)') sym, fun, 'use slot#', ihist, ' for iteration#', iit, ' in the history.'
    nhist = max( ihist, nhist )

    ! store positions and gradients
    hist(:,ihist,IP) = pos
    hist(:,ihist,IG) = grd

    ! create matrix
    allocate( A(0:nhist,0:nhist), b(0:nhist), x(0:nhist), stat=ist )
    if( ist /= 0 ) stop 'DYN gdiis: failed to allocate DIIS matrix and vectors.'

    ! setup matrix
    A = 1.0 ; A(0,0) = 0.0 ! init all entries as 1.0
    b = 0.0 ; b(0)   = 1.0 ! init
    do i2 = 1, nhist
      do i1 = 1, i2 ! triangular loop
        A(i1,i2) = dot_product( hist(:,i1,IG), hist(:,i2,IG) )
        A(i2,i1) = A(i1,i2) ! matrix is symmetric, can be exploited here
        ! if the degrees of freedom are parallelized, insert reduction operation here !
      enddo ! i1
    enddo ! i2

    ! solve
    ist = Solve_Ax_b( A, b, x )
    if( ist /= 0 ) then
      if(o>0) write(o,'(3A,F16.6,9A)') sym, fun, 'inversion of A failed, use simple relaxation!'
      x = 0. ; x(ihist) = 1.
! cDBG  else ; if(o>0) write(o,'(3A,F16.6,9A)') sym, fun, 'Lagrange parameter =', x(0)
    endif ! ist

! cDBG  if(o>0) write(o,'(3A,99F10.3)') sym, fun, 'new linear comb. of old positions c=', &
! cDBG    ( x(i1), i1=ihist+1,nhist ), ( x(i1), i1=1,ihist )
! cDBG  if(o>0) write(o,'(3A,99I10  )') sym, fun, '                                  i=', &
! cDBG    ( i1, i1=ihist+1,nhist ), ( i1, i1=1,ihist )
    pos = matmul( hist(:,:nhist,IP), x(1:nhist) )
    grd = matmul( hist(:,:nhist,IG), x(1:nhist) )

    ! molecular dynamics position update
    pos = pos + dt2m * grd

    mam(-1) = minval( abs(grd) )
    mam( 0) = sum( abs(grd) )/ndof
    mam(+1) = maxval( abs(grd) )
    mam(+2) = sqrt( sum( grd**2 )/ndof )
!     if(o>0) write(o,'(3A,3F10.3,9A)') sym, fun, 'displacements [min,avg,max]', dt2m*mam, ' aB'
    if(o>0) write(o,'(3A,I4,A,ES12.4E2,3F12.6,9A)') sym, fun, 'it#', iit, ' effective forces [min,avg,max,rms]', mam, ' Ha/aB'
!     if(o>0) write(o,'(3A,4ES10.2E2,9A)') sym, fun, 'effective forces [min,avg,max,rms]', mam, ' Ha/aB'

    converged = ( mam(+1) < thre ) ! default criterion
    if( present(Criterion) ) then
      selectcase( Criterion )
      case( 'avg' ) ; converged = ( mam( 0) < thre )
      case( 'rms' ) ; converged = ( mam(+2) < thre )
      endselect ! Criterion
    endif ! present Criterion

  endfunction ! gdiis

!- extended
#endif
endmodule ! dynamics

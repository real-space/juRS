#include "config.h"

! #define DEBUG

#ifdef DEBUG
!!! remove comment from debug line
!!! ==> all debug lines will be included
#define cDBG

#else
!!! comment lines with !DBG
!!! ==> no debug lines will be included
#define cDBG !DBG

#endif

!! @author Paul Baumeister
!! @version 4.03
!!
!! electronic state occupation function using the
!! Fermi-Dirac statistics with a finite temperature
module FermiDirac
  use configuration, only: o ! output unit, 0: no output
implicit none
  private ! default for this module namespace

  public :: Fermidistribution
  public :: density_of_states !! parallel and in blocks
#ifdef EXTENDED
  public :: write_energies_and_weights !! not parallelized
  public :: test
#endif

  real, parameter :: DEFTEMPSMEARING = 1E-3  ! temperature (in Hartree)
  real, parameter :: MINTEMPSMEARING = 0.5**48 ! 1E-14 eV
  real, parameter :: MINDOSSMEARING = 36.75E-6 ! 1 meV  ! min temperature for DoS plots(in Hartree)
  real, parameter :: HUGEARG = 36. ! because exp(-36.) is less than the relative machine precision

  character(len=*), parameter, private :: fun = ': ', sym = 'FDD' !! module symbol

  contains

  status_t function Fermidistribution( nspins, psi, nele, temperature, fermilevel, &
                         comm, maxit, dos_ef, holu_gap, weights, occ ) result( ist )
  ! Computes the Fermi Dirac distribution for a given
  !  --> a set of energies
  !  --> a set of state weights
  !  --> a temperature
  !  --> a number of electrons
  ! iteratively with the bisection method.
  ! Returns
  !  <-- the set of occupation numbers
  !  <-- the Fermi-energy
  !  <-- the density of states at the Fermi-energy
  !  <-- the holu (highest occupied, lowest unoccupied) gap
  ! The holu gap is defined via the highest energy below
  ! and the lowest energy above the Fermi-energy
  !
  ! parallelized over states
  ! (comm must be the equi_comm, the communicator for equivalent spaces)
  ! finds the Fermi-level with the bisection method
  use configuration, only: WARNING, ERROR
  use type_state, only: state
  use MPItools, only: operator(.MPImax.), operator(.MPIsum.)
  use unitsystem, only: eV, eV_
  use constants, only: KELVIN
    integer, intent(in)               :: nspins
    type(state), intent(inout)        :: psi(1:)
    real, intent(in)                  :: nele !! number of all electrons
    real, intent(in)                  :: temperature
    real, intent(inout)               :: fermilevel
    MPI_Comm, intent(in)              :: comm !! state parallelization communicator (equi_comm)
    integer, intent(in), optional     :: maxit
    real, intent(out), optional       :: dos_ef ! = DoS(E_Fermi) * Temp_Fermi (unitless number)
    real, intent(out), optional       :: holu_gap ! = highest occupied to lowest unoccupied
    real, intent(in), optional        :: weights(:) !! optional weights for e.g. constraints
    real, intent(out), optional       :: occ(:) !! occupation numbers

    real, parameter                   :: NOTINIT = -666.
    integer, parameter                :: MAXITERATIONS = 199       ! default limit
    real, parameter                   :: TINYBISEC = 1.0E-13      ! convergence criterium
cDBG  character(len=*), parameter     :: LO_F_UP(-1:+1) = (/'lower','Fermi','upper'/)
cDBG  character(len=*), parameter     :: fun = ' Fermidistribution: '
    integer               :: maxiter = MAXITERATIONS   ! default maximum
    real, save            :: energyborders
    real, save            :: last_fermilevel = NOTINIT ! not initialized
    real, save            :: temp = DEFTEMPSMEARING ! init as default
    real                  :: temp_new, Mag, downup(1:2)
!   real                  :: eminmax(-1:+1)
    real, save            :: e(-1:+1), g(-1:+1)
    integer               :: k_bisec!, ierr
    integer               :: ib!, nb ! borders
    integer               :: ibsk, js
    logical               :: run
    real                  :: spinf, DoS(-1:1)!, gt, tt ! Density of states at the fermilevel
    real                  :: holu(-1:+1) ! highest occupied and lowest unoccupied energy
cDBG  integer             :: n_corr(-1:+1)
cDBG  real                :: e_start(-1:+1)

    ist = 1
    if( nele <= 0. ) then
      if(o>0) write(o,'(3A,9(F0.3,A))') sym, fun, &
        'no electron occupation required, Nele = ', nele, ' e, Fermi level at ',fermilevel*eV, eV_
      psi(:)%occ = 0. ; fermilevel = 0.
      return
    endif ! nele <= 0

cDBG    if( present( weights ) ) then
cDBG      if( size(weights) /= size(psi) ) &
cDBG        stop 'FDD fermidistribution: dim of optional weights does not match dim of PSI.'
cDBG    endif ! present weights

    if( last_fermilevel == NOTINIT ) then ! has not been initialized
      ! initialize

      energyborders = 0.1 ! Ha ! large step size for
      ! quickly approaching the region of the change of sign

      ! locally and globally
      e(-1) = -( ( -minval( psi(:)%ene ) ) .MPImax. comm )  ! workaround, because .MPImin. is not implemented
      e(+1) =       maxval( psi(:)%ene )   .MPImax. comm

    else ! has not been initialized

      e(-1) = fermilevel - energyborders
      e(+1) = fermilevel + energyborders

    endif ! has not been initialized

    ! temperature
    temp_new = abs(temperature) ! negtaive temperature is unphysical, 0.0 is okay
    if( temp_new /= temp ) then ! change?
      temp = temp_new
      ! show
      if(o>0) write(o,'(3A,I0,A,F0.6,9A)') sym, fun, 'Fermi temperature ', nint(temp*KELVIN), ' Kel, kBT = ', temp*eV, eV_
    endif ! temperature has changed

    spinf = 2.0 ; if( nspins > 1 ) spinf = 1.0
    mag = 0. ! magnetization

cDBG    e_start(-1:+1:2) = e(-1:+1:2)
cDBG    n_corr = 0

    k_bisec = 0 ! init iteration counter
    if( present(maxit) ) maxiter = maxit

    ! approaching the limits towards the Fermi-energy from both sides
    do ib = -1, +1, 2 ! ==> ib in {-1,1}

      g(ib) = occup( psi, spinf, e(ib), temp, comm, weights=weights )-nele

      do while( ib * g(ib) < 0.0 ) ! while lower level too high or upper level too low
        e(ib) = e(ib) + ib * energyborders ! ib provides the correct sign
        energyborders = energyborders * 1.1 ! get larger every iteration

        g(ib) = occup( psi, spinf, e(ib), temp, comm, weights=weights )-nele

cDBG    n_corr(ib) = n_corr(ib)+1 ! count how many times the energy limits have to be corrected
      enddo ! while
    enddo ! ib

cDBG     do ib = -1, +1, 2
cDBG       if( n_corr(ib) > 0 ) then
cDBG        if(o>0) write(o, '(5A,ES10.2E2,A,F10.6,A,I4,9A)') sym, fun, &
cDBG          'correct ', LO_F_UP(ib) ,' limit by', e(ib)-e_start(ib), &
cDBG          ' (dev=', g(ib), ' e) in', n_corr(ib), ' steps'
cDBG       endif ! n_corr > 0
cDBG     enddo ! ib


! cDBG  if(o>0) write(o, '(3A,2(2F10.3,A))') sym, fun, 'start [', e(-1), e(+1), ' ], difference [', g(-1), g(+1), ' ] e'

    run = .true. ! start, always run at least once
    do while( run )
      k_bisec = k_bisec+1 ! count up

      ! Mix a new guess
      if( e(+1)-e(-1) < temp .and. abs( g(+1) - g(-1) ) > 0. ) then
        ! linear weighted (conv. faster in the linear region)
        e( 0) = ( e(-1)*g(+1) - e(+1)*g(-1) )/( g(+1) - g(-1) )
      else  ! ...
        ! the arithmetic middle
        e(0) = 0.5*( e(-1) + e(+1) )
      endif ! |Delta e|

      ! compute the occupation with the new Fermi level e( 0)
      g( 0) = occup( psi, spinf, e( 0), temp, comm, weights=weights )-nele

      if( g( 0) > 0.0 ) then
        ! g(+1) and g( 0) have the same sign (hopefully +)
        ! so the zero must be between e(-1) and e( 0)
        e(+1) = e( 0) ; g(+1) = g( 0)
      else ! g( 0) < 0.
        e(-1) = e( 0) ; g(-1) = g( 0)
      endif ! gp*gf >= 0.0

      run = ( abs( g( 0) ) > TINYBISEC .and. k_bisec < maxiter )
    enddo ! while run

    ! calling "occup( r, set=.true. )" affects that the occupation numbers are set
    g( 0) = occup( psi, spinf, e( 0), temp, comm, set=.true., dos=DoS(0), weights=weights )-nele

    holu = homolumo( psi, e( 0), comm )

    if( present( dos_ef ) ) dos_ef = DoS(0)
    if( present( holu_gap ) ) holu_gap = holu(0)


    ! show dos(ef), holu, magnetization if applicatble
    if( nspins == 2 ) then

      ! compute the magnetic moment
      downup(1:2) = 0. ! init
      do ibsk = 1, size(psi)
        js = psi(ibsk)%jspn
cDBG    if( js < 1 .or. js > 2 ) stop 'FDD Fermidistribution: ERROR: coll. spin index is not in {1,2}'
        downup( js ) = downup( js ) + psi(ibsk)%occ * psi(ibsk)%wgt
      enddo ! ibsk
      ! magnetic moment = number of up-spins minus number of down-spins
      Mag = ( downup(2) - downup(1) ) .MPIsum. comm ! reduce via the state parallelization

      if(o>0) write(o,'(3A,F12.6,A,F12.6,2A,F12.6,A)') sym, fun, &
        'DoS(0)', DoS(0), ' HoLuGap', holu(0)*eV, eV_, ' Mag', Mag
    else  ! nspins == 2
      if(o>0) write(o,'(3A,F12.6,A,F12.6,2A,F12.6,A)') sym, fun, &
        'DoS(0)', DoS(0), ' HoLuGap', holu(0)*eV, eV_
    endif ! nspins == 2

    if( k_bisec >= maxiter ) then
      if(o>0) write(o,'(4A,I0,A,ES10.2E2,9A)') sym, fun, 'Warning, ', &
        'Fermi level not converged after ',k_bisec,' steps, charge deficit', g(0), ' e'
cDBG  elseif( k_bisec < 2 ) then ; if(o>0) write(o,'(3A)') sym, fun, 'converged immediately.'
cDBG  else ; if(o>0) write(o,'(3A,I0,A,ES10.2E2,9A)') sym, fun, 'after ',k_bisec,' steps, converged to', g(0), ' e'
    endif ! k_bisec >= maxiter

    fermilevel = e( 0) ! output
    ! internal quantities for the next iteration
    energyborders = max( 1E-3, abs( last_fermilevel - fermilevel ) )
    last_fermilevel = fermilevel ! store save variable for next iteration

    if( present( occ ) ) occ = psi(:)%occ ! export occupation numbers
    ist = 0
  endfunction ! Fermidistribution
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  real function occup( psi, spinf, eF, temp, comm, set, DoS, weights )
  use type_state, only: state
  use MPItools, only: operator(.MPIsum.), MPIallsum
    type(state), intent(inout)        :: psi(:) !! state information objects
    real, intent(in)                  :: spinf  !! number of collinear spins: 1.0 or 2.0
    real, intent(in)                  :: eF     !! Fermi energy
    real, intent(in)                  :: temp   !! smearing temperature
    integer, intent(in)               :: comm   !! communicator for parallelization over states (bands,kpoints)
    logical, intent(in), optional     :: set    !! if set is present and true, occupation numbers will be set
    real, intent(out), optional       :: DoS    !! Density of States at fermi energy
    real, intent(in), optional        :: weights(:)    !! optional weights for e.g. constraints

cDBG  character(len=*), parameter     :: fun = ' occup: '
    integer :: ibsk
    logical :: setoccval
    real    :: dE, wgt, tF, oc, fd, bd, beta, docc, dsef, pocc

#ifdef NaN_SEARCH
    if( count( psi(:)%wgt /= psi(:)%wgt ) > 0 ) stop 'FDD occup: some weights are NaN.'
    if( count( psi(:)%ene /= psi(:)%ene ) > 0 ) stop 'FDD occup: some energies are NaN.'
    if( ef /= ef ) stop 'FDD occup: E_{Fermi} is NaN.'
#endif

    ! if the logical argument is present and .true., setval is set to .true.
    setoccval = .false. ; if( present( set ) ) setoccval = set

    tF = max( MINTEMPSMEARING, abs( temp ) )
    beta = 1./tF ! in principle it does not matter if tF is zero, because then beta will not be used

    ! init
    occup = 0. ! number of all electrons that have been occupied (function result)
    docc  = 0. ! ammount of reoccupation
    dsef  = 0. ! density of states at the Fermi level (times temp)
    pocc  = 0. ! number of states with partial occupancy

!$omp parallel do private(ibsk,wgt,dE,oc,fd,bd) reduction(+:occup,dsef,pocc,docc)
    do ibsk = 1, size(psi)
      wgt = psi(ibsk)%wgt 
#ifdef CONSTR
      ! the optional argument "weights" makes problems, use O0 when important
#endif
      if( present( weights ) ) wgt = wgt*weights(ibsk)

      dE = psi(ibsk)%ene - eF

      ! this routine should work also if tf == 0. because of <=
      if( dE <= -HUGEARG*tF ) then
        ! negative number limit of the Fermi-Dirac distribution function
        oc = spinf
      elseif( dE > HUGEARG*tF ) then
        ! positive number limit of the Fermi-Dirac distribution function
        oc = 0.0
      else  ! |dE| > HUGEARG*tF
        ! if tF  < 0., this is unphysical
        ! if tF == 0., one of the two previous cases should have occurred.
cDBG    if( tf <= 0.0 ) stop 'FDD occup: ERROR: tF < 0 unphysical or tF = 0 logical error!'

        !    |______________
        !   1|              \
        !    |               |
        !   0|________________\___
        !    0              E_F
        ! Fermi-Dirac distribution function
        fd = 1. / ( exp( dE*beta ) + 1.0 )

        bd = (1.-fd)*fd ! derivative of the Fermi-Dirac distribution function w.r.t. (dE/tF)
        dsef = dsef + spinf * bd * wgt ! dsef counts how many states are at the Fermi level
        if( bd > 1E-6 ) pocc = pocc + spinf * wgt ! pocc is an easier way to count the states

        oc = spinf * fd
      endif ! |dE| > HUGEARG*tF
      ! write to the array of occupation numbers
      ! sum up the distributed charge
      occup = occup + oc * wgt

      if( setoccval ) then
        ! count the amount of changed occupation
        !      new - old value
        docc = docc + 0.5 * abs( oc - psi(ibsk)%occ ) * wgt
        psi(ibsk)%occ = oc ! set the value

      endif ! setoccval

    enddo ! ibsk
!$omp end parallel do

    occup = ( occup .MPIsum. comm )

    if( present( DoS ) ) DoS = (dsef*spinf) .MPIsum. comm ! density of States at the Fermi level

#ifdef NaN_SEARCH
    if( occup /= occup ) stop 'FDD occup: NaN in charge.'
#endif

#ifndef CONSTR
    if( setoccval ) then
      docc = docc .MPIsum. comm ! parallelization over states.
      pocc = pocc .MPIsum. comm ! parallelization over states.
      if(o>0) then ! show reoccupation, if present
        if( docc > 1E-6 ) then ; write(o,'(2A,F16.6,9A)') sym, fun, docc, ' electrons reoccupied.'
cDBG    else                   ; write(o,'(2A,A16  ,9A)') sym, fun, 'no', ' electrons reoccupied.'
        endif ! docc > ...
        if( pocc > 0. ) write(o,'(2A,F12.3,9A)') sym, fun, pocc, ' states partially occupied.'
      endif ! o>0
    endif ! setoccval
#endif
  endfunction ! occup

  real function d_Fermidirac( x, beta )
  ! shape function according to the negative of the
  ! derivative of the Fermi-Dirac distribution function
    real, intent(in)        :: x
    real, intent(in)        :: beta ! = 1/temperature

    real :: ex, et
cDBG  if( beta <= 0. ) stop 'FDD d_fermidirac: temp = 0 is too sharp, impossible, temp < 0 unphysical!'
    et = abs( x * beta )
    if( et < HUGEARG ) then
      ex = exp( et )
      d_fermidirac = ex * beta / ( ex + 1. )**2
    else  ! et < 10.
      d_fermidirac = 0.
    endif ! et < 10.
  endfunction ! d_Fermidirac

  function homolumo( psi, Ef, comm ) result( hgl )
  use type_state, only: state
  use MPItools, only: operator(.MPImax.)
    type(state), intent(inout)        :: psi(:) !! state information objects
    real, intent(in)                  :: Ef     !! fermi energy
    MPI_Comm, intent(in)              :: comm   !! communicator for parallelization over states (bands,kpoints)
    real                              :: hgl(-1:+1) !! result: homolumo information

    integer                           :: ibsk
    real                              :: ho, lu
    ho = Ef - 9E99 ! too large to be displayed with '(F10.1)' in mHa (*1000.)
    lu = Ef + 9E99 ! so when the gap cannot be calculated, this will be ******

    do ibsk = 1, size(psi)
      if( psi(ibsk)%ene <= Ef ) then
        ho = max( ho, psi(ibsk)%ene ) ! highest OCCUPIED
      else  ! E > Ef
        lu = min( lu, psi(ibsk)%ene ) ! lowest UNOCCUPIED
      endif ! E > Ef
    enddo ! ibsk

    hgl(-1) =      ho  .MPImax. comm
    hgl(+1) = -( (-lu) .MPImax. comm ) ! workaround, because .MPImin. is not implemented
    hgl( 0) = hgl(+1) - hgl(-1) ! gap
  endfunction ! homolumo

  !! computes the density of states in parallel
  !! The MPImaster will write it to a file "<name>.dos"
  !! uses buffer blocks for the communication along the state communicator
  integer function density_of_states( name, global, psi, Fermilevel, temperature, &
                      comm, weights, labels, comment ) result( ist )
  use type_info, only: info
  use type_state, only: state
  use configuration, only: WARNING, ERROR
  use configuration, only: MaxInputFileNameLen, DoS_FileNameExtension
  use toolbox, only: operator(+)
  use MPItools, only: MPImaster, operator(.MPImax.), MPIallsum
  use MPIconst, only: MPI_COMM_SELF
  use unitsystem, only: eV, eV_
  implicit none
    ! parameter
    character(len=*), parameter     :: fun = ' DoS: '
    logical, parameter              :: SECOND_SPIN_NEGATIVE = .true.
    real, parameter                 :: DEF_E_SAMPLING = 1.E-3
    integer, parameter              :: MBL = 256 ! block size ==> 256: 4kiByte
    iounit_t, parameter             :: u = 17 ! unit to write ASCII file
    character(len=*), parameter     :: YLMCHAR(1:16) =  &
     (/'s  ', &
       'px ','pz ','py ', &
       'dr2','dzx','dz2','dyz','dxy', &
       'f1 ','f2 ','f3 ','f4 ','f5 ','f6 ','f7 '/)
    ! arguments
    character(len=*), intent(in)    :: name
    type(info), intent(in)          :: global ! information objects
    type(state), intent(in)         :: psi(:) ! state information objects
    real, intent(in), optional      :: Fermilevel
    real, intent(in), optional      :: temperature
    MPI_Comm, intent(in), optional  :: comm
    real, intent(in), optional      :: weights(1:,:) ! (nlm,nbsk)
    character(len=*), intent(in), optional :: labels(1:)    !! optional labels dos
    character(len=*), intent(in), optional :: comment  !!
    ! local vars
    character(len=MaxInputFileNameLen+len(DoS_FileNameExtension)) :: filename
    integer                         :: nse, nee, ie, iee
    integer                         :: nbsk, ibsk, js, ns
    integer                         :: icomm
    logical                         :: m
    real                            :: emin, emax, espacing = DEF_E_SAMPLING
    real                            :: beta, temp
    real                            :: spinf
    real                            :: e, de, ef = 0.!, t ! Default Fermi energy (Ha)
    real                            :: fac(0:2)
    integer                         :: nbl, ibl, mbi, mb0
    real                            :: ebl(MBL), wgt, dfd
    real, allocatable               :: wbl(:,:,:) ! (ns,1:max(1,nw),MBL)
#define SHOW_HIST
#ifdef SHOW_HIST
    integer                         :: oh
    real, allocatable               :: dos(:,:) ! (ns,nee)
#endif
    integer                         :: nwm, nw, iw
#define SKIP_ZEROS
#ifdef SKIP_ZEROS
    logical                         :: lastzero, thiszero = .false. ! last or this point is  zero
    real                            :: thres ! threshold for not plotting
    real, parameter                 :: ZERO(1:2) = 0.0
cDBG  integer                       :: ndp=0, ndz=0
#endif
    ist = 0
    nbsk = size( psi, 1 ) ! number of states in this process
    if( nbsk < 1 ) return

    fac(0) = eV ! energy conversion factor
    fac(1:2) = 1./fac(0) ! the density of states will be scaled with 1/energy conversion factor

    ef = 0. ; if( present( Fermilevel ) ) ef = Fermilevel
    temp = DEFTEMPSMEARING ; if( present( temperature ) ) temp = abs(temperature)
    temp = max(temp,MINDOSSMEARING)
    beta = 1./temp
    icomm = MPI_COMM_SELF ; if( present( comm ) ) icomm = comm

    ! energy sampling, at least 1 micro Hartree
    espacing = max( 1E-6, 0.1 * temp ) ! in atomic Hartree units

    ! adjust espacing to the format F16.6 in the correct units
    espacing = 1E-6 * nint( 1E6 * espacing * fac(0) ) / fac(0)

    nw = 0 ; if( present( weights ) ) nw = size(weights,1)
cDBG  if( present( weights ) ) then
cDBG    if( size(weights,2) /= nbsk ) stop 'FDD: DoS: weights must have the same dim#2 as there are states!'
cDBG    if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'multiply ', nw, ' weights for a projected DoS.'
cDBG    if( nw < 1 ) stop 'FDD: DoS: weights seem to be not allocated!'
cDBG  endif ! weights present
    nwm = max(1,nw)

    ns = global%nspins ! number of collinear spins
    if( ns /= 1 .and. ns /= 2 ) stop 'FDD DoS: number of collinear spins must be in {1,2}!'
    spinf = 2.0 ; if( ns > 1 ) spinf = 1.0


    ! determine boundaries of the spectrum
    emin = - ( ( - minval( psi(:)%ene ) ) .MPImax. icomm )
    emax =         maxval( psi(:)%ene )   .MPImax. icomm
    ! subtract the Fermi level and enlarge window by some smearing (16x temp)
    emin = emin - ef - 32 * temp
    emax = emax - ef + 32 * temp

cDBG  if(o>0) write(o,'(3A,9(F0.6,A))') sym, fun, 'energy minmax [ ',emin,', ',emax,' ]'
cDBG  if( espacing <= 0. ) stop 'FDD density_of_states: fatal error: espacing <= 0.'
    ! adjust so the Fermi energy will be at exactly 0.0 ==> better for comparison
    nse = nint( emin/espacing ) ! start energy index
    nee = nint( emax/espacing ) ! end   energy index
    nee = min( -nse, nee ) ! limit by the range of occupied states below the Fermi-level



    ! determine number of blocks (0...nbl)
    nbl = ( nee+1-nse - 1 )/MBL ! integer divide
    mb0 = nee+1-nse - nbl * MBL ! number of energy points in the 0-th cycle
cDBG  if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'use ', nbl, ' full blocks of ', MBL, ' energy points and ', mb0,' in cycle #0'


! #ifdef DEBUG
    ! do not ouput this information if weights are given
    if(o>0 .and. nw<1) write(o,'(3A,2(F0.3,A),F0.6,9A)') sym, fun, &
      'energy sampling from ', emin*fac(0), ' to ', emax*fac(0), ' in steps of ', espacing*fac(0), eV_
    if(o>0 .and. nw<1) write(o,'(3A,F0.1,9(A,I0))') sym, fun, &
      'energy sampling in ',(nee+1.-nse)/MBL,' blocks of ',MBL,', ',nee+1-nse,' steps in total'
! #endif

    m = MPImaster( icomm ) ! only master performs file IO

    filename = name+DoS_FileNameExtension ! generate filename
cDBG  if(o>0) write(o,'(9A)') sym, fun, 'opening of file "', trim(filename),'" for writing.'
    ist = 0
    if( m ) open( unit=u, file=filename, status='unknown', iostat=ist ) ! master opens file
    if( ist /= 0 ) then
      if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'opening of file "', trim(filename),'" failed.'
      ! the results will be written to fort.17
    endif ! ist /= 0

    if( m ) then ! master writes file head information
      write(unit=u,fmt='(9A)')      '# Density of States'
      if( present(comment) ) &
      write(unit=u,fmt='(9A)')      '# ', trim(comment)
      write(unit=u,fmt='(9A)')      '# Energy unit', eV_
      write(u,'(A,F16.6,9A)')       '# shifted by ', -ef*eV, eV_, ' ==> Fermi level at 0.0'
      if( SECOND_SPIN_NEGATIVE .and. ns > 1 ) &
      write(unit=u,fmt='(9A)')      '# 2nd spin negative'
    endif ! master
    if( SECOND_SPIN_NEGATIVE ) fac(2) = -fac(1) ! invert the 2nd spin

#ifdef SKIP_ZEROS
    ! DoS values in Format F16.6 will be 0.000000 if dos*fac(1) < 0.4E-6
    thres = 0.4E-6*fac(0) ! fac(0)=1/fac(1)
    ! do not plot energy values, if there is a zero DoS
    if( m ) write(u,fmt='(A)')      '# energies for zero DoS-entries are skipped'
    thiszero = .false. ! init
cDBG  ndp = 0 ; ndz = 0 ! init
#endif

    if( m ) then ! master writes legends for the weights
      if( present( labels ) ) then
        if(ns==2) write(u,'(A,99A16)') '#   energy    ', ( labels(iw)+'_dn', labels(iw)+'_up', iw=1,min(nw,size(labels)) )
        if(ns==1) write(u,'(A,99A16)') '#   energy    ', labels(1:min(nw,size(labels)))
      else  ! present( labels )
        if(ns==2) write(u,'(A,99A16)') '#   energy    ', ( YLMCHAR(iw)+'_dn', YLMCHAR(iw)+'_up', iw=1,min(nw,16) )
        if(ns==1) write(u,'(A,99A16)') '#   energy    ', YLMCHAR(1:min(nw,16))
      endif ! present( labels )
    endif ! master

    allocate( wbl(ns,nwm,MBL), stat=ist )
    if( ist /= 0 .and. o>0) write(o,'(9A)') sym, fun, ERROR, 'failed to allocate weights WBL!'
    if( ist /= 0 ) return ! error
    wbl = 0. ! init

#ifdef SHOW_HIST
    oh = 0 ; if( nw < 1 ) oh = o
    if( oh > 0 ) then
      allocate( dos(ns,nse:nee), stat=ist )
      if( ist /= 0 ) oh = 0 ! switch off histogramm
      dos = 0. ! init
    endif ! oh
#endif


    iee = nse-1 ! init energy point
    mbi = mb0 ! set to the specific block size of the 0th iteration
    do ibl = 0, nbl ! for each block
      wbl = 0. ! init block array

      ! create density of the states in this process
      do ie = 1, mbi
        iee = iee+1 ! count up global energy counter

        ! sample the energies equidistantly
        e = iee * espacing

        ebl(ie) = e ! store energy value
        do ibsk = 1, nbsk
          js = psi(ibsk)%jspn ! collinear spin index
cDBG      if( js < 1 .or. js > ns ) stop 'FDD DoS: collinear spin index JS exceeds bounds!'
          de = e - ( psi(ibsk)%ene - ef )
          wgt = psi(ibsk)%wgt
          dfd = spinf * d_fermidirac( de , beta=beta ) ! beta = 1/temp
          if( nw > 0 ) then
            wbl(js,1:nw,ie) = wbl(js,1:nw,ie) + weights(1:nw,ibsk) * wgt * dfd ! weighted DoS
          else  ! nw == 0
            wbl(js,1,ie) = wbl(js,1,ie) + wgt * dfd ! usual total DoS
          endif ! nw == 0
        enddo ! ibsk
      enddo ! ie

      ! collective communication over the state communicator (equi_comm)
      call MPIallsum( wbl, icomm ) ! no matter, if ns is 1 or 2
      ! in principle, only wbl(1:ns,1:nwm,1:mbi) needs to be summed.

#ifdef SHOW_HIST
!       if(oh>0) write(o,'(3A,9(I0,A))') sym, fun, 'copy range ', iee+1-mbi, ' to ', iee, ' (nse=',nse, ' nee=', nee, ' mbi=',mbi, ')'
      if( oh > 0 ) dos(:,iee+1-mbi:iee) = wbl(:,1,1:mbi) ! it0: nse:nse+mb0, it1:
#endif

      ! master writes density of all states to the file
      if( m ) then
        do ie = 1, mbi
#ifdef SKIP_ZEROS
          ! skip writing zero entries wherever possible
cDBG      ndp=ndp+1 ! increase data point counter
          lastzero = thiszero ! pass          lzero: all DoS values at last energy are as small as 0.000000
          thiszero = all( wbl(:,:,ie) < thres ) ! all DoS values at this energy are as small as 0.000000

          if( lastzero .and. ( .not. thiszero ) ) then
            ! recover last step that was 0.000000 before writing the next nonzero step
            write(unit=u,fmt='(F12.6,99F16.6)') fac(0)*( ebl(ie) - espacing ), ( ZERO(1:ns), iw=1,nwm )
cDBG        ndz=ndz-1 ! correct skipped data point counter
          endif ! lreco

          if( .not. ( lastzero .and. thiszero ) ) then
#endif
                                         !       energy,   ( spindn(iw)  [, spinup(iw)]  for iw=1,nwm )
            write(unit=u,fmt='(F12.6,99F16.6)') fac(0)*ebl(ie), ( fac(1:ns)*wbl(1:ns,iw,ie), iw=1,nwm )

#ifdef SKIP_ZEROS
cDBG      else ; ndz=ndz+1 ! increase skipped data point counter
          endif ! lwrit
#endif
        enddo ! ie
      endif ! master

      mbi = MBL ! set to full block size
    enddo ! ibl

    if( m ) close( unit=u, iostat=ist ) ! master closes the file

#ifdef SKIP_ZEROS
cDBG  if(m.and.o>0) write(o,'(3A,9(I0,A))') sym, fun, 'skipped ', ndz, ' of ', ndp, ' data points in the total DoS!'
#endif

    if( iee /= nee ) then
cDBG  if(o>0) write(o,'(4A,9(I0,A))') sym, fun, ERROR, 'last energy point was ', iee, ', but expected ', nee
      ist = nee - iee ! Error
    endif ! iee /= nee ! counting error


#ifndef EXTENDED
!+ not extended
  endfunction ! density_of_states
!- not extended
#else
!+ extended

#ifdef SHOW_HIST
    if( oh > 0 ) iee = make_dos_hist( espacing, nse, dos )
  endfunction ! density_of_states

  status_t function make_dos_hist( espacing, ie0, dos ) result( ios )
  use unitsystem, only: eV, eV_
    real, intent(in)    :: espacing
    integer, intent(in) :: ie0 ! start index
    real, intent(in)    :: dos(:,ie0:)

#ifndef VERTICAL_COUNTS
! #define cDBG
    integer, parameter :: NDM=16, nBLANKS=2
    integer            :: ns, ie1, ne, nbin, ndig, ndis, ib1, ib0, is, ie, ib, iden, imult, mdig
    real               :: per_eV, step, f10, elim, mval
    real, allocatable  :: dr(:,:)
    string_t           :: myf, mya ! format strings
    character(len=NDM) :: sup, sdn, axs

    if( o <= 0 ) return ! do not write to unit 0

    per_eV = 1./eV

    ns = max( 1, min( size(dos,1), 2 ) )
    ie1 = ubound( dos, 2 )
    ne = ie1+1-ie0 ! number of energies

    nbin = 30 ! approximate number of bins
    ! find the step width as an nice power of 10 in the given unit system
    step = ne * espacing / nbin ! approximate step with, to be corrected
cDBG  write(o,'(3A,I0,A,9(F0.6,A))') sym, fun, 'histogram ne=',ne, ' step=', step*eV, eV_
    ndig = floor( log10( step * eV ) )
    f10 = 0.1**ndig
    ndis = floor( step * eV * f10 )
    step = max( 1E-6, ndis / ( f10 * eV ) )
cDBG  write(o,'(3A,2(I0,A),9(F0.6,A))') sym, fun, 'histogram ndig=',ndig, ' ndis=', ndis, ' corrected step=', step*eV, eV_
    ib1 = ceiling( ie1*espacing/step )
    ib0 =   floor( ie0*espacing/step )
    nbin = ib1+1-ib0 ! corrected number of bins
cDBG  write(o,'(3A,I0,A,9(F0.3,A))') sym, fun, 'histogram for ns=',ns, ' from ', ie0*espacing*eV, ' to ', ie1*espacing*eV, eV_
cDBG  write(o,'(3A,9(I0,A))') sym, fun, 'histogram ', nbin, ' bins collecting ', ne/nbin, ' values of ', ne, ' energies'

    allocate( dr(ib0:ib1,ns), stat=ios )
    dr = 0. ! init
    do is = 1, ns
      ie = ie0 ! start energy index
      do ib = ib0, ib1
        iden = 0 ! init denominator
        elim = ( ib + .5 ) * step
        do while( ie*espacing < elim .and. ie <= ie1 )
          dr(ib,is) = dr(ib,is) + dos(is,ie) ! add up dos values
          iden = iden+1 ! denominator
          ie = ie+1
        enddo ! while
        if( iden > 0 ) dr(ib,is) = dr(ib,is)/(1.*iden)
cDBG    write(99,'(9F12.6)') ib*step*eV, dr(ib,is)*per_eV, elim*eV
      enddo ! ib
    enddo ! is

    ! prepare formats
    mval = max( 1E-3, maxval( dr ) ) ! largest absolute value
    mdig = ceiling( log10( mval * per_eV ) ) ! how many digits are needed to display these numbers correctly
cDBG  write(o,'(3A,I0,A,F0.6,A)') sym, fun, 'histogram needs ',mdig,' digits to display the maximal value ', mval * per_eV
    imult = 10**max( 0, 6 - 3 * ( mdig / 3 ) ) ! integer divide
    myf = '(F16.3)'
    write(unit=mya,fmt='(9(A,I0))',iostat=ios) '(A,F6.',max(0,-ndig),',9A)'

    sdn = '' ; sup = '' ! init for the case ns==1
    write(unit=o,fmt='(/,3A,I0,9A)',iostat=ios) sym, fun, 'Density of States [ ',imult,' /',eV_, ' ]'
      sup = '        steps of'
      write(unit=axs,fmt=mya,iostat=ios) '  ',step*eV, '  ', eV_ ! axis
      write(unit=o,fmt='(9A)',iostat=ios) sup, trim(axs)
      axs = ' |       |' ! axis
    if( ns > 1 ) then
!       sup = '   Mayority spin' ; sdn = ' nips ytironiM  '
      sup = '         up-spin' ; sdn = ' dn-spin        '
      write(unit=o,fmt='(9A)',iostat=ios) sup, trim(axs), trim(sdn)
      sdn = ' ---------------'
    endif ! ns > 1
      sup = ' ---------------'
      write(unit=o,fmt='(9A)',iostat=ios) sup, trim(axs), trim(sdn)
      sdn = ''
    do ib = ib1, ib0, -1
      write(unit=axs,fmt=mya,iostat=ios) ' |',ib*step*eV,' |' ! axis
      if( ib == 0 ) axs = ' | Fermi |' ! axis
      if( ns > 1 ) then
        write(unit=sup,fmt=myf,iostat=ios) dr(ib,2) * per_eV * imult
        write(unit=sdn,fmt='(" ",a)',iostat=ios) adjustl(sup)
      endif ! ns > 1
        write(unit=sup,fmt=myf,iostat=ios) dr(ib,1) * per_eV * imult

      write(unit=o,fmt='(9A)',iostat=ios) sup, trim(axs), trim(sdn)
    enddo ! ib
    write(unit=o,fmt='(A)',iostat=ios) ! empty line

#else
    integer, parameter        :: NDM=4
    integer                   :: ns, ie1, is, ne, nbin, ib0, ib1, ib, id
    integer                   :: ie, ndig, ndis, iden, mdig, mdis, mult
    real                      :: mval, per_eV, step, elim, f10, ene
    real, allocatable         :: dr(:,:)
    integer, allocatable      :: di(:,:)
    character(len=8)          :: str ! string
    character(len=16)         :: scal(-3:NDM) ! strings
    character, allocatable    :: h(:,:) ! string

    if( o <= 0 ) return

    per_eV = 1./eV

    ns = max( 1, min( size(dos,1), 2 ) )
    ie1 = ubound( dos, 2 )
    ne = ie1+1-ie0 ! number of energies
    nbin = 60 ! approximate number of bins

    ! find the step width as an nice power of 10 in the given unit system
    step = espacing * ne / ( 1.* nbin ) ! approximate step with, to be corrected
cDBG  write(o,'(3A,I0,A,9(F0.6,A))') sym, fun, 'histogram ne=',ne, ' step=', step*eV, eV_
    ndig = floor( log10( step * eV ) )
    f10 = 10.**(-ndig)
    ndis = floor( step * eV * f10 )
    step = ndis / ( f10 * eV )
cDBG  write(o,'(3A,2(I0,A),9(F0.6,A))') sym, fun, 'histogram ndig=',ndig, ' ndis=', ndis, ' corrected step=', step*eV, eV_

    ib1 = ceiling( ie1*espacing/step )
    ib0 =   floor( ie0*espacing/step )

    nbin = ib1+1-ib0 ! corrected number of bins
cDBG  write(o,'(3A,I0,A,9(F0.3,A))') sym, fun, 'histogram for ns=',ns, ' from ', ie0*espacing*eV, ' to ', ie1*espacing*eV, eV_
cDBG  write(o,'(3A,9(I0,A))') sym, fun, 'histogram ', nbin, ' bins of ', ne/nbin, ' values for ', ne, ' energies'
    allocate( dr(ib0:ib1,ns), di(ib0:ib1,ns), stat=ios )
    dr = 0. ! init
    do is = 1, ns
      ie = ie0 ! start energy index
      do ib = ib0, ib1
        iden = 0 ! init denominator
        elim = ( ib + .5 ) * step
        do while( ie*espacing < elim .and. ie <= ie1 )
          dr(ib,is) = dr(ib,is) + dos(is,ie) ! add up dos values
          iden = iden+1 ! denominator
          ie = ie+1
        enddo ! while
        if( iden > 0 ) dr(ib,is) = dr(ib,is)/(1.*iden) ! normalize
cDBG    write(99,'(9F12.6)') ib*step*eV, dr(ib,is)*per_eV, elim*eV
      enddo ! ib
    enddo ! is

    mval = max( 1E-3, maxval( dr ) ) ! largest absolute value
    mdig = floor( log10( mval * per_eV ) ) ! see how many digits are there naturally (in the given energy unit)
cDBG  write(o,'(3A,F0.6,9A)') sym, fun, 'histogram max value ', mval * per_eV, ' per',eV_
    ! find the multiplier mult = 10^n such that the largest integer value has NDM+1 digits
    mult = 10 ** ( NDM - mdig )
    di = nint( mult * dr * per_eV ) ! conversion to integers
cDBG  write(o,'(3A,I0,A,I0,9A)') sym, fun, 'histogram max value ', maxval( di ), ' * 10^',mdig-NDM,' per',eV_

    do ib = ib0, ib1
cDBG  write(98,'(F12.6,2I9)') ib*step*eV, di(ib,:)
    enddo ! ib

    write(unit=o,fmt='(/,9A)',iostat=ios) sym, fun, 'Density of States (vertical counts)'
    allocate( h(-1:nbin,-2:NDM), stat=ios )
    scal = ''
    write(unit=scal(NDM),fmt='(A,I0)')      '-- 10^', mdig-0
    write(unit=scal(  0),fmt='(A,I0)')      '-- 10^', mdig-NDM
    if( mdig >= 0 .and. mdig <= NDM ) then
      write(unit=scal(NDM-mdig),fmt='(9A)') '-- 1 /', eV_ ! adjustl(eV_)
    endif
    write(unit=scal( -1),fmt='(F6.2,A)') 10./f10, eV_
    write(unit=scal( -2),fmt='(F6.2,A)')  1./f10, eV_
    do is = 1, ns
      ! generate histogramm
      h = '' ! init clear
      do ib = ib0, ib1
        ! generate scale
        ene = ib*step
        ie = nint( ene*eV*f10 )
        str = '' ; write(unit=str,fmt='(F8.1)',iostat=ios) 0.1*ie
cDBG    write(97,'(I9,2F12.6,2I9)') ib, ene*eV, 0.1*nint( ene*eV*f10 ), nint( ene*eV*f10 )

        h(ib-ib0,-2) = str(8:8)

        h(ib-ib0,-1) = '-'
        if( str(8:8) == '0' ) then
          h(ib-ib0,-1) = str(6:6)
          if( abs( ie ) > 99 ) h(ib-ib0-1,-1) = str(5:5)
        endif
        if( ib == 0 ) h(ib-ib0,-1) = '+' ! line ----+--

        if( di(ib,is) < 1 ) cycle ! leave empty
        str = '' ; write(unit=str,fmt='(I8)',iostat=ios) di(ib,is)
        h(ib-ib0,0:) = (/ ( str(8-id:8-id), id=0,NDM ) /)
      enddo ! ib
      ! display
      if( is == 1 ) then
        do id = NDM, -2, -1
          write(unit=o,fmt='(999A)',iostat=ios)  '  ', h(:,id), trim(scal(id))
        enddo ! id
      else  ! is == 1
        do id = -1, NDM
          write(unit=o,fmt='(999A)',iostat=ios)  '  ', h(:,id), trim(scal(id))
        enddo ! id
      endif ! is == 1
    enddo ! is
    write(unit=o,fmt='(A)',iostat=ios) ! empty line
#endif
#endif
  endfunction ! density_of_states

  !! writes energies and weights to a file
  status_t function write_energies_and_weights( name, global, psi, &
    Fermilevel, temperature, comm, weights, labels, comment ) result( ist )
  use type_info, only: info
  use type_state, only: state
  use configuration, only: WARNING, ERROR
  use configuration, only: MaxInputFileNameLen
  use toolbox, only: operator(+)
  use MPItools, only: MPImaster, operator(.MPImax.), MPIallsum, MPIparallel
  use MPIconst, only: MPI_COMM_SELF
  implicit none
    ! parameter
    character(len=*), parameter     :: fun = ' E&w: '
    character(len=*), parameter     :: E_w_FileNameExtension = '.ews'
    integer, parameter              :: u = 17 ! unit to write ASCII file
    character(len=*), parameter     :: YLMCHAR(1:16) =  &
     (/'s  ', &
       'px ','pz ','py ', &
       'dr2','dzx','dz2','dyz','dxy', &
       'f1 ','f2 ','f3 ','f4 ','f5 ','f6 ','f7 '/)
    ! arguments
    character(len=*), intent(in)    :: name ! project name
    type(info), intent(in)          :: global ! information objects
    type(state), intent(in)         :: psi(:) ! state information objects
    real, intent(in), optional      :: Fermilevel
    real, intent(in), optional      :: temperature !! not used
    MPI_Comm, intent(in), optional  :: comm
    real, intent(in), optional      :: weights(1:,:) ! (nlm,nbsk)
    character(len=*), intent(in), optional :: labels(1:)    !! optional labels dos
    character(len=*), intent(in), optional :: comment  !!
    ! local vars
    character(len=MaxInputFileNameLen+len(E_w_FileNameExtension)) :: filename
    integer                         :: nbsk, ibsk, ns
    integer                         :: icomm
    logical                         :: m
    real                            :: spinf
    real                            :: e, ef = 0. ! Default Fermi energy (Ha)
    integer                         :: nw, nwm, iw
    ist = 0

    ef = 0. ; if( present( Fermilevel ) ) ef = Fermilevel
    icomm = MPI_COMM_SELF ; if( present( comm ) ) icomm = comm

    if( MPIparallel(comm) ) then
      if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'functions is not yet parallelized! return.'
      return
    endif ! parallel

    nbsk = size( psi, 1 ) ! number of states in this process
    nw = 0 ; if( present( weights ) ) nw = size(weights,1)
cDBG  if( present( weights ) ) then
cDBG    if( size(weights,2) /= nbsk ) stop 'FDD: DoS: weights must have the same dim#2 as there are states!'
cDBG    if(o>0) write(o,'(3A,I4,9A)') sym, fun, 'multiply', nw, ' weights for a projected DoS.'
cDBG    if( nw < 1 ) stop 'FDD: DoS: weights seem to be not allocated!'
cDBG  endif ! weights present
    nwm = max(1,nw)

    ns = global%nspins ! number of collinear spins
    if( ns /= 1 .and. ns /= 2 ) stop 'FDD DoS: number of collinear spins must be in {1,2}!'
    spinf = 2.0 ; if( ns > 1 ) spinf = 1.0

    m = MPImaster( icomm ) ! only master performs file IO

    filename = name+E_w_FileNameExtension

cDBG  if(o>0) write(o,'(9A)') sym, fun, 'opening of file "', trim(filename),'" for writing.'
    ist = 0
    if( m ) open( unit=u, file=filename, status='unknown', iostat=ist ) ! master opens file
    if( ist /= 0 ) then
      if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'opening of file "', trim(filename),'" failed.'
      ! the results will be written to fort.17
    endif ! ist /= 0

    if( m ) then ! master writes file head information
      write(unit=u,fmt='(9A)') '# Energy and Weights of KohnSham States (atomic units)'
      if( present(comment) ) write(unit=u,fmt='(9A)') '# ', trim(comment)
      write(unit=u,fmt='(A,F0.6,9A)') '# shifted by ', -ef, ' Ha ==> Fermi level at 0.0'
    endif ! master

    if( m ) then ! master writes legends for the weights
      if( present( labels ) ) then
        if(ns==2) write(unit=u,fmt='(A,99A16)') '#   energy    ', ( labels(iw)+'_dn', labels(iw)+'_up', iw=1,min(nw,size(labels)) )
        if(ns==1) write(unit=u,fmt='(A,99A16)') '#   energy    ', labels(1:min(nw,size(labels)))
      else  ! present( labels )
        if(ns==2) write(unit=u,fmt='(A,99A16)') '#   energy    ', ( YLMCHAR(iw)+'_dn', YLMCHAR(iw)+'_up', iw=1,min(nw,16) )
        if(ns==1) write(unit=u,fmt='(A,99A16)') '#   energy    ', YLMCHAR(1:min(nw,16))
      endif ! present( labels )
    endif ! master

    if( m ) then
      do ibsk = 1, nbsk ! not parallelized here
        e = psi(ibsk)%ene - ef ! aligned to the Fermilevel (if present)
        if( nw > 0 ) then
          write(u,fmt='(999F16.6)') e, weights(1:nw,ibsk) * psi(ibsk)%wgt * spinf ! weighted with extra weights
        else  ! nw == 0
          write(u,fmt='(999F16.6)') e, psi(ibsk)%wgt * spinf ! for usual total DoS
        endif ! nw == 0
      enddo ! ibsk
    endif ! master

  endfunction ! write_energies_and_weights

  status_t function test( )
    write(*,*,iostat=test) __FILE__,' no module test implemented!'
  endfunction ! test

!- extended
#endif
endmodule ! FermiDirac

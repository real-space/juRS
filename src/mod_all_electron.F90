#include "config.h"

! #define TEST_NODELESS
! #define DEBUG
#define DETAILS
! #define FULL_DEBUG

#ifdef DEBUG
!!! remove comment from debug line
#define cDBG
#else
!!! comment line
#define cDBG !DBG
#endif

#define qn_ELL_t integer
#define qn_ENN_t integer
#define qn_iLM_t integer
#define qn_iNL_t integer
#define qn_iLN_t integer

! ATOM code
! everything is stored on radial grids
! multiplying spherical harmonics

!! @author Paul Baumeister, Andrea Nobile
!! @version 4.0
!! computes a self consistent spherically symmetric potential
!! for a given atomic configuration and performs pseudization
module all_electron
  use configuration, only: o
  use toolbox, only: operator(+)
#ifdef TEST_NODELESS
  use toolbox, only: cat
#endif
implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: sym = 'AE' !! module symbol

  public :: generate !! generate PAW data sets from a spherically symmetric DFT calculation
  public :: write2file
#ifdef EXTENDED
  public :: test
#endif

  interface generate
    module procedure generate_Z, generate_sym
  endinterface

  interface ellchar
    module procedure ell2char, char2ell
  endinterface

  interface write2file
    ! writes a file with x and 1 or more y data columns
    module procedure write2file_r1, write2file_r2, write2file_r3
  endinterface

  integer, parameter :: ITOT =  0 ! total (both)
  integer, parameter :: ICOR =  1 ! core
  integer, parameter :: IVAL =  2 ! valence
  integer, parameter :: IEXT =  3 ! external
  character(len=*), parameter :: icv2char(ITOT:IEXT) = (/'total  ','core   ','valence','extern '/)

  integer, parameter :: INIT =  0 ! not initialized
  integer, parameter :: IOLD =  1 ! old
  integer, parameter :: INEW =  2 ! new
  character(len=*), parameter :: age2char(INIT:INEW) = (/'ini','old','new'/)

  integer, parameter :: IPRE = -3 ! previous
  integer, parameter :: ICOU = -2 ! Coulomb
  integer, parameter :: IEIG = -1 ! eigenvalue
! integer, parameter :: ITOT =  0 ! total ! defined above
  integer, parameter :: IHTR =  1 ! Hartree
  integer, parameter :: IVXC =  2 ! Exchange correlation potential
  integer, parameter :: IEXC =  3 ! Exchange correlation energy
  character(len=*), parameter :: pot2char(ICOU:IEXC) = (/'Cou','Eig','TOT','Htr','vXC','eXC'/)

  integer, parameter :: I_TRU =  1 ! true quantities
  integer, parameter :: I_SMT =  2 ! smooth quantities
  integer, parameter :: I_PRJ =  3 ! projector quantities
  character(len=*), parameter :: tru2char(I_TRU:I_PRJ) = (/'tru','smt','prj'/)

  integer, parameter :: IUP = +1
  integer, parameter :: ITT = ITOT ! == 0
  integer, parameter :: IDN = -1
  character(len=*), parameter :: spin2char(-2:2) = (/' ?-',' dn','   ',' up',' ?+'/)

  character(len=*), parameter :: ELL_STRING = 'spdfghijkl'

  interface calc_atom
    module procedure calc_atom_vloc, calc_atom_sigma
  endinterface ! calc_atom

  real, parameter :: OCC_TINY = 1E-16
  real, parameter :: OCC_NONE = 0.5**64

  contains

  character function ell2char( ell ) result( chr )
    qn_ELL_t, intent(in) :: ell ! argument
cDBG  if( ell < 0 ) stop 'ell2char: invalid input, ELL < 0'
cDBG  if( ell >= len(ELL_STRING) ) stop 'ell2char: ELL too large.'
    chr = ELL_STRING(ell+1:ell+1)
  endfunction ell2char

  qn_ELL_t function char2ell( chr ) result( ell )
    character, intent(in) :: chr ! argument
    ell = index( ELL_STRING, chr )-1
! cDBG  if( ell < 0 ) stop 'char2ell: invalid input'
  endfunction char2ell

  qn_ENN_t function char2enn( chr ) result( enn )
    character, intent(in) :: chr
    selectcase( chr )
    case( '1':'9' ) ; enn = ichar(chr)-ichar('0')
    case default ; enn = 0 ! invalid
    endselect ! chr
  endfunction char2enn

  qn_iNL_t function ennell2inl( enn, ell ) result( inl )
    qn_ENN_t, intent(in)   :: enn
    qn_ELL_t, intent(in)   :: ell
    inl = 0
! cDBG  if( enn < 1 )  stop 'ennell2inl: ENN < 1 not allowed.'
! cDBG  if( ell < 0 )  stop 'ennell2inl: ELL < 0 not allowed.'
! cDBG  if( ell >= enn ) stop 'ennell2inl: ELL >= ENN not allowed.'
    if( enn < 1 .or. ell < 0 .or. ell >= enn ) return ! 0
    inl = ell+1+(enn*(enn-1))/2
  endfunction ennell2inl

  qn_ENN_t function inl2enn( inl, ell ) result( enn )
    qn_iNL_t, intent(in) :: inl
    qn_ELL_t, intent(out), optional :: ell
    selectcase( inl )
    case(  1: 1 ) ; enn = 1
    case(  2: 3 ) ; enn = 2
    case(  4: 6 ) ; enn = 3
    case(  7:10 ) ; enn = 4
    case( 11:15 ) ; enn = 5
    case( 16:21 ) ; enn = 6
    case( 22:28 ) ; enn = 7
    case( 29:36 ) ; enn = 8
    case( 37:45 ) ; enn = 9
    case( 46:   ) ; enn = 0
      if(o>0) write(o,'(9A)') 'inl2enn: not implemented'
      stop 'inl2enn: fatal error!'
cDBG  case(   :0  ) ; stop 'inl2enn: INL < 0 not allowed.'
cDBG  case default  ; stop 'inl2enn: fatal error!'
    endselect ! inl
    if( present( ell ) ) ell = inl-( 1+(enn*(enn-1))/2 )
  endfunction inl2enn

  qn_ELL_t function inl2ell( inl, enn ) result( ell )
    qn_iNL_t, intent(in) :: inl
    qn_ENN_t, intent(out), optional :: enn
    qn_ENN_t :: en
    en = inl2enn( inl, ell )
    if( present( enn ) ) enn = en
  endfunction inl2ell

  character(len=2) function inl2string( inl ) result( str )
    qn_iNL_t, intent(in)        :: inl
    qn_ENN_t                    :: enn
    qn_ELL_t                    :: ell
    status_t                    :: ios
    enn = inl2enn( inl, ell=ell )
    write(unit=str,fmt='(I1,A1)',iostat=ios) enn, ellchar(ell)
  endfunction inl2string

  !! writes a plot-able file from a multiple set of data
  status_t function write2file_r3( file, a, grid, factor ) result( ios )
    character(len=*), intent(in)          :: file
    real, intent(in)                      :: a(1:,:,1:)
    real, intent(in), optional            :: grid(1:), factor

    character(len=*), parameter           :: fun = ' write2file: '
    iounit_t, parameter                   :: unt=38
    integer                               :: ir, imx, i3
    real                                  :: f
    f = 1. ; if( present( factor ) ) f = factor

    open( unit=unt, file=file, action='write', iostat=ios )
    if( ios /= 0 ) then
      if(o>0) write(o,'(9A)') sym, fun, 'unable to open "', trim(file), '" for writing.'
      return
    endif ! ios /= 0

    imx = size( a, 1 )
    do i3 = 1, size( a, 3 )
      if( present( grid ) ) then
        imx = min( imx, size( grid, 1 ) )
        do ir = 1, imx
          write( unit=unt, fmt='(129ES24.16)', iostat=ios ) grid(ir), f*a(ir,:,i3)
        enddo ! ir
      else  ! present grid
        do ir = 1, imx
          write( unit=unt, fmt='(128ES24.16)', iostat=ios ) f*a(ir,:,i3)
        enddo ! ir
      endif ! present grid
      write( unit=unt, fmt='(A)', iostat=ios ) ! empty line
    enddo ! i3

    close( unit=unt, iostat=ios )
  endfunction ! write2file

  status_t function write2file_r2( file, a, grid, factor ) result( ios ) ! wrapper for the rank3 routine
    character(len=*), intent(in)          :: file
    real, intent(in)                      :: a(:,:)
    real, intent(in), optional            :: grid(:), factor

    ios = write2file_r3( file, reshape( a, (/size(a,1),size(a,2),1/) ), grid, factor )
  endfunction ! write2file

  status_t function write2file_r1( file, a, grid, factor ) result( ios ) ! wrapper for the rank3 routine
    character(len=*), intent(in)          :: file
    real, intent(in)                      :: a(:)
    real, intent(in), optional            :: grid(:), factor

    ios = write2file_r3( file, reshape( a, (/size(a,1),1,1/) ), grid, factor )
  endfunction ! write2file


  !! show the initial valence occupation and all core levels
  status_t function display_conf( u, Z, qt, occ, legend ) result( ios )
    integer, intent(in)               :: u
    real, intent(in)                  :: Z
    real, intent(in)                  :: qt ! total charge, non-zero if not neutral
    real, intent(in)                  :: occ(1:,ICOR:) ! spin-integrated occupation of core and valence states
    logical, intent(in), optional     :: legend ! explain the characters

cDBG  character(len=*), parameter :: fun = ' display_conf: '
    character(len=*), parameter :: ERROR_min = 'ERR<'
    character(len=*), parameter :: ERROR_max = 'ERR>'
    character(len=*), parameter :: core   = ' c  '
    character(len=*), parameter :: hole   = ' c* ' ! core hole
    character(len=*), parameter :: hline  = ' -----------------------------------------------'
    character(len=4)            :: t(0:5,1:8)
    integer                     :: wd, ioc
    qn_ELL_t                    :: ell, ellmax
    qn_ENN_t                    :: enn, ennmax
    qn_iNL_t                    :: inl
    real                        :: qe(ICOR:IVAL), full, ch

    ios = -1
    if( u <= 0 ) return ! do not write to unit 0

    t = '    '   ! init table
    ellmax = -1  ! init
    ennmax =  0  ! init

    qe = 0. ! init charges for core and valence
    ch = 0. ! /=0:found a core hole?
    do inl = 1, size(occ,1)
      enn = inl2enn( inl, ell=ell )
      if( any( occ(inl,ICOR:IVAL) > OCC_TINY ) ) then
        full = 2.*(2*ell+1)
        ellmax = max(ell,ellmax)
        ennmax = max(enn,ennmax)
        if( occ(inl,ICOR) > 0. ) then
          t(ell,enn) = core
          if( occ(inl,ICOR) < full ) then
            t(ell,enn) = hole ! a core hole has been detected
            ch = full - occ(inl,ICOR)
          elseif( occ(inl,ICOR) > full ) then
            t(ell,enn) = ERROR_max
          endif ! core occupation > 0 but not full
          qe(ICOR) = qe(ICOR) + min(max(0.,occ(inl,ICOR)),full)
        elseif( occ(inl,IVAL) > 0. ) then
          ioc = nint(occ(inl,IVAL))
          if( abs( occ(inl,IVAL) - real(ioc) ) < 1E-9 ) then ! occupation number is integer
            write(unit=t(ell,enn),fmt='(I2)') ioc
          else  ! occupation number is integer
            write(unit=t(ell,enn),fmt='(F4.1)') occ(inl,IVAL)
          endif ! occupation number is integer
          if( occ(inl,IVAL) > full ) t(ell,enn) = ERROR_max
          qe(IVAL) = qe(IVAL) + min(max(0.,occ(inl,IVAL)),full)
        endif ! occ > 0
      elseif( any( occ(inl,ICOR:IVAL) < 0. ) ) then
        t(ell,enn) = ERROR_min ! negative occupation
      endif ! any occ > 0.
    enddo ! inl

    write(u,'(A,F6.1)') '' ! blank line
    write(u,'(A,F6.1)') ' atomic number        ', Z
    write(u,'(A,F8.3)') ' core      charge     ', qe(ICOR)
    write(u,'(A,F8.3)') ' valence   charge     ', qe(IVAL)
    if( abs( qt ) > 1E-9 ) &
    write(u,'(A,F8.3)') ' charged              ', qt
    if( abs( ch ) > 1E-9 ) &
    write(u,'(A,F8.3)') ' core hole charge     ', ch
    wd = 4*(ellmax+3)+2 ! width (for horizontal lines)

    write(u,'(A )')   hline(:wd)
    write(u,'(99A)')  '   n |', ( '   ',ellchar(ell), ell=0,ellmax )
    write(u,'(A )')   hline(:wd)
    do enn = ennmax, 1, -1
      write(u,'(I4,A,9A4)',iostat=ios) enn, ' |  ', t(0:ellmax,enn)
    enddo ! enn
    write(u,'(A )')   hline(:wd)

    if( present(legend) ) then ; if( legend ) then
      write(u,'(9A)') '  ', trim(core), ': frozen core'
      if(ch>0.) write(u,'(9A)') ' ', trim(hole), ': core hole'
      write(u,'(9A)') '  ', ' #: valence'
      write(u,'(A)')  hline(:wd)
    endif ; endif ! present legend
    write(u,'(A,F6.1)') '' ! blank line

  endfunction ! display_conf


  status_t function initial_density( spin, g, q, rho, charged ) result( ist )
  use type_rgrid, only: rgrid
  use radial_potential, only: xc_functional
  use radial_potential, only: Hartree_potential
  use constants, only: Pi
    integer, intent(in)             :: spin
    type(rgrid), target, intent(in) :: g
    real, intent(in)                :: q(-1:1) ! q_dn, q_tot, q_up
    real, intent(out)               :: rho(0:,-spin:) ! potential(0:imx,spin,ITOT:IEXC)
    real, intent(in), optional      :: charged

    character(len=*), parameter     :: fun = ' initial_density: '
    real, pointer                   :: r(:) ! radial grid
    integer                         :: ir, is
    real                            :: alpha, beta, gamma, xion, g2, sumh, x, xa, xb, exa, exb
    r => g%r
    xion = 0.! ; if( present( charged ) ) xion = charged

    is = 0
    sumh     = q(is)/(2.-is*is)
    alpha    = 0.3058*(sumh)**(1./3.)
    beta     = sqrt(108./Pi)*max(0.,sumh-2.)*alpha
    if( 4.+3.2*xion < 0. ) stop 'AE initial_density: |XION| is too large ==> sqrt(neg.)!'
    gamma    = sqrt(4.+3.2*xion)
    g2       = gamma**3
    if( sumh < 2. ) g2 = 0.5*(sumh)*g2
    do ir = 1, ubound(r,1)
      x        = alpha*r(ir)
      xa       = 3.*x
      xb       = gamma*r(ir)
      exa      = 0. ; if( xa < 150. ) exa = exp(-xa)
      exb      = 0. ; if( xb < 150. ) exb = exp(-xb)
      do is = -spin, spin
        rho(ir,is) = (beta*sqrt(x)*exa/r(ir)**2 + g2*exb)*(2.-is*is)
      enddo ! is
    enddo ! ir
    rho(0,:) = rho(1,:) ! extrapolated 0-th order at the origin

cDBG  is = write2file( 'dmp/inirho', rho(99:,0), grid=g%r(99:) )
    if( any( rho /= rho ) ) stop 'AE initial_density: NaN in RHO.'
    ist = 0
  endfunction ! initial_density

#define GOOD_START_ENERGIES

  real function scf_atom( spin, g, Z, symbol, occ, kxc, rhoext, potext, sc_pot, rhocor, kincor, valeig, alleig, energies ) result( etot )
  use constants, only: Pi
  use type_rgrid, only: rgrid
  use type_rgrid, only: operator(.at.)
  use radial_potential, only: xc_functional
  use radial_potential, only: Hartree_potential
  use radial_integrator, only: shooting_method
#ifdef GOOD_START_ENERGIES
!   use radial_integrator, only: shoot_at_energy
  use type_element, only: neutral_level_energy ! database for Z=1:118
#endif
  use radial_integrator, only: integrate_outwards
  use configuration, only: WARNING, ERROR
  use unitsystem, only: eV, eV_
    integer, intent(in)             :: spin
    type(rgrid), target, intent(in) :: g
    real, intent(in)                :: Z  !! atomic number
    character(len=2), intent(in)    :: symbol
    real, intent(in)                :: occ(-1:,1:,ICOR:) !! occupation numbers (-1:+1,1:28,IC:IV)
    integer, intent(in)             :: kxc !! XC-key
    real, intent(in), optional      :: rhoext(0:,-spin:) !! (0:g%imx) external density
    real, intent(in), optional      :: potext(0:,-spin:) !! (0:g%imx) external potential
    real, intent(out), optional     :: sc_pot(0:) !! (0:g%imx) self_consistent potential
    real, intent(out), optional     :: rhocor(0:,1:) !! (0:g%imx,spin?) self_consistent core density
    real, intent(out), optional     :: kincor !! kinetic energy of the core
    real, intent(out), optional     :: valeig(1:,0:) !! (1:ennmax,0:ellmax) valence state eigenvalues (spin integrated with occupation weighting)
    real, intent(out), optional     :: alleig(-1:,1:) !! all state eigenvalues (-1:1,1:28) see occ array

    integer, parameter              :: MAXCYCLES = 199
    integer, parameter              :: MINCYCLES = 3
    real, parameter                 :: THRESHOLD = 1E-11
    integer, parameter              :: NRWF = 1 ! 1: dont store wf's. >28: store wf's
    ! character(len=*), parameter     :: fun = ' scf: ' !! replaced by the chem. symbol
    integer                         :: u = 0 ! output unit during iterations
    real, allocatable               :: r2rho(:,:,:) ! (0:g%imx,-spin:spin,IC:IV)
    real, allocatable               :: r2rwf2(:)  ! (0:g%imx) ! temporoy ==> no spin index
    real, allocatable               :: rho(:,:,:) ! (0:g%imx,-spin:spin,IT:IV)
    real, allocatable               :: pot(:,:,:,:) ! (0:g%imx,-spin:spin,IT:IV,IO:IN)
    real, allocatable               :: rwf(:,:,:)   ! r*wave function (large component only)
    integer                         :: icv, ioc, is, iupdn(0:spin)
    qn_iNL_t                        :: inl, mnl
    integer                         :: iwf, nwf, mwf, nsp
    integer                         :: irwf, ir
    status_t                        :: ist
    integer                         :: icyc
    integer                         :: isolved(45), inl_list(45)
    real                            :: eigenvalue(-spin:spin,45)
    real                            :: occup
    real                            :: egap(-spin:spin,ICOR:IVAL), dene, docc
    integer                         :: ell, enn ! quantum numbers
    real                            :: q(-1:+1), charged, ri, res, nrm, spinw8s(-1:+1)
    real                            :: alf, alpha = 0.33 !! straight mixing coefficient
    real                            :: E(IPRE:IEXC) ! energy
    logical                         :: run
#ifdef FULL_DEBUG
    character(len=32)               :: dummy
    status_t                        :: ios
#endif
    character(len=16)               :: task, potfile
    character(len=3)                :: ccv = '   '
    character(len=4)                :: fun = ' __ '
    real, intent(out), optional     :: energies(3) ! kinetic, electrostatic and exchange energy
    fun(2:3) = symbol !! chemical symbol

    selectcase( spin )
    case( 0 ) ; iupdn = 0
cDBG  if(o>0) write(o,'(9A)') sym, fun, 'start spin-integrated calculation.'
    case( 1 ) ; iupdn = (/+1,-1/)
cDBG  if(o>0) write(o,'(9A)') sym, fun, 'start spin-polarized calculation.'
    case default
cDBG  if(o>0) write(o,'(4A,I0)') sym, fun, ERROR, 'spin must be in {0,1}, but spin = ', spin
      stop 'AE scf_atom: SPIN must be in {0,1}'
    endselect ! spin

cDBG  if( any( occ(1,:,:)+occ(-1,:,:) /= occ(0,:,:) ) ) stop 'AE scf_atom: occupation numbers {dn,tot,up} must fulfill dn+up==tot!'
cDBG  if( ubound(occ,3) /= IVAL ) stop 'AE scf_atom: occupation numbers must have shape OCC(-1:+1,inl,ICOR:IVAL)'

    mnl = size(occ,2) ! max. number of shells
cDBG  if(o>0) write(o,'(9A)') sym, fun, 'highest      shell is ', inl2string(mnl)
    do while( mnl > 1 .and. all( occ(ITT,mnl,:) == 0. ) ) ; mnl = mnl-1 ; enddo ! max number of occupied shells
cDBG  if(o>0) write(o,'(9A)') sym, fun, 'highest occ. shell is ', inl2string(mnl)

cDBG  if(o>0) write(o,'(3A,28A5  )') sym, fun, '         ', ( inl2string(inl), inl=1,mnl )
cDBG  if(o>0) write(o,'(3A,28F5.1)') sym, fun, 'Core occ ', occ(0,1:mnl,ICOR)
cDBG  if(o>0) write(o,'(3A,28F5.1)') sym, fun, 'Valence  ', occ(0,1:mnl,IVAL)

    ! add all occupation numbers, check if the system is charge neutral
    do is = -1, +1
      q(is) = sum( occ(is,:,:) )
    enddo
    charged = q(ITT) - Z ! number of electrons - number of protons

    ist = display_conf( o, Z, charged, occ(ITT,:,:), legend=.true. ) ! show electronic configuration

cDBG  if( any( occ < 0. ) ) stop 'AE scf_atom: an occupation number is negative.'
cDBG  ! check the core occupation numbers
cDBG  do inl = 1, mnl
cDBG    enn = inl2enn( inl, ell )
cDBG    if( any( occ(-1:+1,inl,ICOR) > (2*ell+1)*(/1,2,1/) ) ) stop 'AE scf_atom: a core occupation number is larger than permitted.'
cDBG  enddo ! inl

    ! prepare densities and potentials
    allocate( r2rho(0:g%imx,-spin:spin,ICOR:IVAL), &
              rho(0:g%imx,-spin:spin,ITOT:IVAL), &
              pot(0:g%imx,-spin:spin,ITOT:IEXC,IOLD:INEW), & ! only one set of potentials (no potential mixing)
              stat=ist ) ; if( ist /= 0 ) stop 'AE scf_atom: allocation of densities and potentials failed.'
    rho = 0.
    pot = 0.

    if( present( rhoext ) ) then ! indicate treatment of the external density
      do is = -spin, spin
        nrm = sum( rhoext(:,is)*g%r**2*g%dr ) ! norm
        if(o>0) write(o,'(4A,F16.6,9A)') sym, fun, 'check norm ', icv2char(IEXT), nrm, ' e', trim(spin2char(is))
      enddo ! is
      if( any( occ(:,:,IVAL) > 0. ) ) then
        if(o>0) write(o,'(4A,F0.1,9A)') sym, fun, WARNING(0), 'external density for Z = ', Z, ' will replace valence density.'
      endif ! valence states occupied
    endif ! present rhoext

    u = o ! turn detailed outout on

    ! start the SCF-cycle
    task = 'Start'
    icyc = 0 ! init cycle counter
    res = 9E9 ! init as huge
    run = ( icyc < MAXCYCLES )

    do while( run )
      ! icyc = icyc+1 ! count up scf cycle counter ==> moved to the solver
      ! decide, if there will be an iteration after this one
      run = ( res > THRESHOLD )
      run = run .or. ( icyc <= MINCYCLES )
      run = run .and. ( icyc < MAXCYCLES )

      if( icyc > 2 ) u = 0 ! turn detailed outout off
      if( res < THRESHOLD*10. .or. icyc > MAXCYCLES-3 ) u = o ! close to last iterations ==> turn detailed output on

! cDBG  if(u>0) write(u,'(3A,3(I0,A),2(ES10.2,A),L2)') sym, fun, 'cycle #', icyc, ' max ',MAXCYCLES,' min ',MINCYCLES,' res ',res,' thres ',THRESHOLD,' run ',run
! cDBG  if(u>0) write(u,'(/3A,I3/)') sym, fun, 'cycle#', icyc

      !-------------------------------------------------------------------------------------------------------------------------------
     !
    !
   !
  !-----------------------------------------------------------------------------------------------------------------------------------
  !===================================================================================================================================
  !===================================================================================================================================
  !===================================================================================================================================
  selectcase( task )
  !===================================================================================================================================
  case( 'start', 'Start' ) ! task
    if(u>0) write(u,'(9A)') sym, fun, 'task "', trim(task), '".'


    ! init maxima for core and valence
    egap(:,ICOR) = -9E9
    egap(:,IVAL) =  9E9

    alf = 1. ! set the mixing for the 1st iteration to 100%
    pot = 0. ! init the potential array

    E(IPRE) = 0. ! init previous total energy (of last iterations)
    nwf = 0 ! init number of occupied states
    eigenvalue = 0. ! init eigenvalues
    do inl = 1, mnl
      enn = inl2enn( inl, ell ) ! calculate enn and ell quantum numbers

      icv = 0 ! init core or valence index
      if( occ(ITT,inl,ICOR) > OCC_TINY ) icv = icv+ICOR ! a core state
      if( occ(ITT,inl,IVAL) > OCC_TINY ) icv = icv+IVAL ! a valence state
      selectcase( icv )
      case( 0 )         ! unoccupied, do nothing
      case( ICOR:IVAL ) ! core or valence
        nwf = nwf+1 ! count up the number of occupied states
        inl_list(nwf) = inl ! set the index
! cDBG    if(o>0) write(o,'(9A)') sym, fun, 'treat ', inl2string(inl), ' state as ', icv2char(icv)
      case( ICOR+IVAL ) ; stop 'AE scf_atom: occupation of a shell in core AND valence not allowed.'
      case default      ; stop 'AE scf_atom: fatal error: icv should not occur with this value.'
      endselect ! icv ! shell occupied or not?

#ifdef GOOD_START_ENERGIES
      if( task == 'Start' ) then
        eigenvalue(:,inl) = neutral_level_energy( Z, inl ) ! get core levels of a neutral setup from the database
cDBG    if(o>0) write(o,'(5A,F16.6,9A)') sym, fun, 'treat ', inl2string(inl), ' state, E_start =', eigenvalue(0,inl)*eV, eV_
      else ! Start
#else
      if( .true. ) then
#endif
        ! preliminary guess for eigenvalue (Hydrogen picture)
        ! eigenvalue(:,inl) = -.5*(Z/enn)**2 ! Hartree unit system
        eigenvalue(:,inl) = -.5*(Z/enn)**2 * &  ! Hartree unit system
                            ( .783517 + 2.5791E-5 * (Z/enn)**2  ) * & ! fit for the correct 1s energy
                            exp(-.01*(enn-1)*Z)  ! screening effect for enn>1
        ! the reduction by 22% of the 1s value w.r.t. the Hydrogen
        ! energy -1/2(Z/n)^2 and linear correction are found empirically
        ! extracted from http://physics.nist.gov/PhysRefData/DFTdata/Tables/ptable.html
cDBG    if(o>0) write(o,'(7A,F12.3,9A)') sym, fun, 'treat ', inl2string(inl), ' state as ', icv2char(icv),'  E_guess =', eigenvalue(ITT,inl)*eV, eV_
      endif ! start energies parametrized
    enddo ! inl

    if(u>0) write(u,'(3A,99(" ",I0))') sym, fun, 'iln-indices of occupied states:', inl_list(1:nwf)
    if(u>0) write(u,'(3A,9(I0,A))') sym, fun, 'allocate ',min(NRWF,nwf),' x ',2*spin+1,' atomic wave functions.'
    allocate( rwf(0:g%imx,-spin:spin,min(NRWF,nwf)), r2rwf2(0:g%imx), stat=ist )
    if( ist /= 0 ) stop 'AE scf_atom: allocation of radial wave functions failed!'

    task = 'loadpot' ! set next task
!     task = 'guessrho' ! set next task
  !===================================================================================================================================
  case( 'loadpot' ) ! task
    if(u>0) write(u,'(9A)') sym, fun, 'task "', trim(task), '".'

    ! try to load a potential from ./pot/rV.00Z
    write( unit=potfile, fmt='(A,I3.3)', iostat=is ) 'pot/rV.', nint(Z)
    if(u>0) write(u,'(9A)') sym, fun, 'try to read potential from file "', trim(potfile), '".'
    open( unit=76, file=potfile, iostat=is )
    run = ( is == 0 )
    if( run ) then
      do ir = 1, g%imx
        read(76,*,iostat=is) ri, pot(ir,0,ITOT,IOLD)
        run = run .and. ( is == 0 )
      enddo ! ir
      close( unit=76, iostat=is )
      do is = -spin, spin
        pot(:,is,ITOT,IOLD) = ( pot(:,0,ITOT,IOLD) + Z )/g%r
      enddo ! is
      pot(0,:,:,:) = 0.
      if( run .and. u>0) write(u,'(9A)') sym, fun, 'potential found in file "', trim(potfile), '".'
    else  ! run
      if(o>0) write(o,'(9A)') sym, fun, 'failed to open file "', trim(potfile), '".'
    endif ! run

    task = 'guessrho' ! set next task
    if( run ) task = 'solve' ! set next task
    run = .true.
  !===================================================================================================================================
  case( 'guessrho' ) ! task
    if(u>0) write(u,'(9A)') sym, fun, 'task "', trim(task), '".'

    ist = initial_density( spin, g, q(-spin:spin), rho(:,:,ITOT), charged )

    task = 'genpot' ! set next task
  !===================================================================================================================================
  case( 'solve' ) ! task
    if(u>0) write(u,'(9A)') sym, fun, 'task "', trim(task), '".'

    if( .not. run .and. u>0) write(u,'(/,3A,/)') sym, fun, 'run = .FALSE.'

    ! show energies
    if(u>0) write(u,'(/,9A)')      sym, fun, 'Kohn-Sham states'
    if(u>0) write(u,'(4A,A16,9A)') sym, fun, 'c/v ', 'nl', 'KS-energy', eV_
    if(u>0) write(u,'(9A)')        sym, fun, '----------------------------'

    ! usually we only calculate the occupied states, however, in the last iteration (run is false)
    ! we calculate all states for all spins {-spin,total,+spin} in order to get a good eigenenergy 
    ! to be used as the energy parameter for the spin-integrated potential
    nsp = 1 ; mwf = min( ubound(occ,2), mnl+4 )
    if( run ) mwf = nwf ! manipulate the loop-limit to calculate only the occupied states
    if( run ) nsp = 1+spin ! compute only {dn,up} by jumping from is=-1 to is=+1

    if( nsp > 1 ) then
      if(u>0) write(u,'(9A)') sym, fun, 'state       occup  up-dn  Energy average      up-dn'
    elseif( spin > 0 ) then
      if(u>0) write(u,'(9A)') sym, fun, 'state       occup  up  dn   Energy level        up-E   dn-E'
    else  ! ...
      if(u>0) write(u,'(9A)') sym, fun, 'state   occup      Energy level'
    endif ! ...

    r2rho(:,:,ICOR:IVAL) = 0. ! initialize new r^2*densities

#ifdef TEST_NODELESS
    if( .not. run ) then
      open( unit=18, file='export_wf.'+cat(nint(Z),fmt='(I3.3)') )
      write(18,'(9(A,I0))') '<radial_grid elements="',g%imx,'" id="',1,'">'
      write(18,'(9ES16.6)') g%r
      write(18,'(A)') '</radial_grid>', ''
    endif ! not run
#endif

! !$omp parallel default(shared) private(iwf,inl,enn,ell,is,icv,ioc,ccv,occup,irwf,nrm,r2rwf2)
! !$omp do reduction(+:r2rho) schedule(static,1)
    do iwf = 1, mwf
      inl = iwf ; if( run ) inl = inl_list(iwf) ! get the inl index from the table
!       if( occ(ITT,inl,icv) == 0. ) cycle

      enn = inl2enn( inl, ell ) ! get enn and ell quantum numbers

      !==begin spin loop==========================
      do is = -spin, spin, nsp ! skip is==0, if spin==1, except for the last iteration when run is false

        icv = 0 ; occup = 0.
        do ioc = ICOR, IVAL
          if( occ(is,inl,ioc) /= 0. ) icv = ioc
        enddo
        ccv = icv2char(icv)(1:3)
        if( icv >= ICOR ) occup = occ(is,inl,icv)
        if( Z > 0. ) occup = max( 0., occup )

! cDBG    if(o>0) write(o,'(8A,F4.1)') sym, fun, 'solve ', inl2string(inl), trim(spin2char(is)), ' state as ', ccv, ' occ =', occup

        ! solve the eigenvalue problem using the shooting method
        irwf = min( max( 1, iwf ), NRWF ) ! find the index where the wave functions are stored, if NRWF==1, wave functions are dumped immediately
        isolved(inl) = shooting_method( enn, ell, g, Z, V=pot(:,is,ITOT,IOLD), E=eigenvalue(is,inl), rf=rwf(:,is,irwf), r2rho=r2rwf2 )

        if( isolved(inl) /= 0 ) then
cDBG      if(o>0) write(o,'(9A)') sym, fun, 'solving for ', inl2string(inl), trim(spin2char(is)) ,' state failed!'
          r2rwf2 = 0.
        else  ! isolved /= 0
! cDBG      if(u>0) write(u,'(6A,F16.6,9A)') sym, fun, ccv, ' ', inl2string(inl), spin2char(is), eigenvalue(is,inl)*eV, eV_
          nrm = sum( r2rwf2*g%dr ) ! norm
          if( occup > 0. .and. icv > 0 ) then
            ! add to new density
            r2rho(:,is,icv) = r2rho(:,is,icv) + occup/nrm * r2rwf2
          endif ! occup > 0.

#ifdef TEST_NODELESS
          if( .not. run ) then
            write(18,'(4(A,I0),A,F0.3,A,F0.9,A)') '<radial_wave_function grid="',1 &
              ,'" enn="',enn,'" ell="',ell,'" spin="',is,'" occ="',occup,'" energy="',eigenvalue(is,inl),'">'
            write(18,'(9ES16.6)') rwf(:,is,irwf)
            write(18,'(A)') '</radial_wave_function>', ''
          endif ! not run
#endif

        endif ! isolved /= 0

      enddo ! is
      !==end spin loop============================

      icv = 0 ; do ioc = ICOR, IVAL ; if( occ(ITT,inl,ioc) > OCC_TINY ) icv = ioc ; enddo
      if( nsp > 1 ) then
        ! find the energy level {tt} by averaging over {up,dn} since {tt} has not been calculated
        occup = maxval( occ(ITT,inl,ICOR:IVAL) )
        docc = maxval( occ(+spin,inl,ICOR:IVAL) ) - maxval( occ(-spin,inl,ICOR:IVAL) ) ! occupation difference in spin polarization up-dn
        dene = eigenvalue(+spin,inl) - eigenvalue(-spin,inl) ! energy difference due to spin polarization up-dn
        eigenvalue(ITT,inl) = 0.5*( eigenvalue(-spin,inl)+eigenvalue(+spin,inl) ) ! average
        if(u>0) write(u,'(6A,F7.3,SP,F7.3,SS,A,F16.6,SP,F10.6,SS,9A)') sym, fun, &
          ccv, ' ', inl2string(inl), '  f=', occup, docc, '  E=', eigenvalue(ITT,inl)*eV, dene*eV, eV_
      elseif( spin > 0 ) then
        if( ccv == 'tot' ) ccv = 'new'
        if(u>0) write(u,'(6A,F7.3,F5.1,F4.1,A,F16.6,SP,2F7.3,SS,9A)') sym, fun, &
          ccv, ' ', inl2string(inl), '  f=', occ((/ITT,IUP,IDN/),inl,max(icv,ICOR)), &
          '  E=', eigenvalue(ITT,inl)*eV, ( eigenvalue((/IUP,IDN/),inl)-eigenvalue(ITT,inl) )*eV, eV_
      else  ! ...
        if(u>0) write(u,'(6A,F7.3,A,F16.6,9A)') sym, fun, &
          ccv, ' ', inl2string(inl), '  f=', occup, '  E=', eigenvalue(ITT,inl)*eV, eV_
      endif ! spin

    enddo ! iwf
! !$omp end do
! !$omp end parallel

#ifdef TEST_NODELESS
    if( .not. run ) then
      do is = -spin, spin
        write(18,'(9(A,I0))') '<radial_potential spin="',is,'" grid="',1,'">'
        write(18,'(9ES16.6)') g%r*pot(:,is,ITOT,IOLD)-Z
        write(18,'(A)') '</radial_potential>', ''
      enddo ! is
      close( unit=18, iostat=ist )
    endif ! not run
#endif


!   write(7,'(I0,99F16.6)') icyc, eigenvalue(iupdn,inl_list(1:nwf)) ! writes the eigenvalues to fort.7
#ifdef DETAILS
    if(o>0) write(o,'(5A,99(" ",F0.6))') sym, fun, 'eigenvalues [',eV_,' ]', eigenvalue(iupdn,inl_list(1:nwf))*eV
#endif

    task = 'genrho' ! set next task
  !===================================================================================================================================
  case( 'genrho' ) ! task
    if(u>0) write(u,'(9A)') sym, fun, 'task "', trim(task), '".'

    ! divide new densities by r^2
    do icv = ICOR, IVAL
      if( spin > 0 ) r2rho(:,ITT,icv) = r2rho(:,IUP,icv) + r2rho(:,IDN,icv) ! total density == up+dn-density
      do is = -spin, spin
        if(u>0) write(u,'(4A,F16.6,9A)') sym, fun, 'check norm ', icv2char(icv), sum( r2rho(:,is,icv)*g%dr ), ' e', trim(spin2char(is))
        rho(1:,is,icv) = r2rho(1:,is,icv)/g%r(1:)**2 ! divide by r^2
        rho(0,is,icv) = rho(1,is,icv) ! extrapolate the origin (0-th order)
      enddo ! is
    enddo ! icv

    ! an external density will replace the valence density
    if( present( rhoext ) ) then
      if(u>0) write(u,'(9A)') sym, fun, 'external density replaced valence density.'
      if( size( rhoext, 2 ) < 2*spin+1 ) stop 'AE scf_atom: shape RHOEXT(:,-1:+1) expected!'
      do is = -spin, spin
        rho(:,is,IVAL) = rhoext(:,is)
        if(u>0) write(u,'(4A,F16.6,9A)') sym, fun, 'check norm ', icv2char(IEXT), sum( rho(:,is,IVAL)*g%r2dr ), ' e', trim(spin2char(is))
      enddo ! is
    endif ! present rhoext
    ! add the core and valence densities to a total charge density
    rho(:,:,ITOT) = rho(:,:,ICOR) + rho(:,:,IVAL)

    task = 'genpot' ! set next task
  !===================================================================================================================================
  case( 'genpot' ) ! task
    if(u>0) write(u,'(9A)') sym, fun, 'task "', trim(task), '".'

! cDBG  is = write2file( 'dmp/rho', rho(:,:,ITOT), grid=g%r(1:) )

      ! generate the exchange correlation potential ( and energy density ) from the density
      ist = xc_functional( spin, fpirho=rho(:,:,ITOT), Vxc=pot(:,:,IVXC,INEW), Exc=pot(:,ITT,IEXC,INEW), key=kxc, rgd=g%d, rgr=g%r, rgdrdi=g%dr )

! cDBG  is = write2file( 'dmp/exc', pot(9:,ITT,IEXC,INEW), grid=g%r(9:) )
cDBG  is = write2file( 'dmp/truvxc', pot(9:,ITT,IVXC,INEW), grid=g%r(9:) )

      ! generate the Hartee potential from the density
      ist = Hartree_potential( g, rho=rho(:,ITT,ITOT), vH=pot(:,ITT,IHTR,INEW) )
cDBG  is = write2file( 'dmp/truves', pot(9:,ITT,IHTR,INEW), grid=g%r(9:) )
#ifdef NOHartreePotential
      pot(:,:,IHTR,INEW) = 0.
#endif
      ! add the two potentials to an effective local potential (without -Z/r)
      do is = -spin, spin
        pot(:,is,ITOT,INEW) = pot(:,ITT,IHTR,INEW) + pot(:,is,IVXC,INEW)
      enddo ! is
cDBG  is = write2file( 'dmp/truveff', pot(9:,ITT,IHTR,INEW) + pot(9:,ITT,IVXC,INEW) - Z/g%r(9:), grid=g%r(9:) )

      if( present( potext ) ) then ! treatment of the external potential
        pot(:,:,ITOT,INEW) = pot(:,:,ITOT,INEW) + potext
      endif ! present potext

    task = 'etotal' ! set next task
  !===================================================================================================================================
  case( 'etotal' ) ! task
    if(u>0) write(u,'(9A)') sym, fun, 'task "', trim(task), '".'

      ! compute the total energy with the eigenvalues of the previous iteration
      E(IPRE+1:) = 0. ! init every energy contribution except the previous total energy
      do is = -spin, spin, 1+spin ! skip ITT
        E(IEIG) = E(IEIG) + sum( ( occ(is,:,ICOR)+occ(is,:,IVAL) ) * eigenvalue(is,:) ) ! eigenvalue sum
        E(IVXC) = E(IVXC) + sum( pot(:,is,IVXC,INEW)*rho(:,is,ITOT)*g%r2dr ) ! double counting correction
      enddo ! is
      E(ICOU) = -Z*sum( rho(:,ITT,ITOT)*g%r*g%dr ) ! Coulomb energy, not needed for Etot
      E(IHTR) = 0.5*sum( pot(:,ITT,IHTR,INEW)*rho(:,ITT,ITOT)*g%r2dr ) ! Hartree energy
      E(IEXC) = sum( pot(:,ITT,IEXC,INEW)*rho(:,ITT,ITOT)*g%r2dr ) ! XC energy
      E(ITOT) = E(IEIG) - E(IHTR) - E(IVXC) + E(IEXC) ! total energy

      if(u>0) then
        write(u,'(9A)') ! empty line
        write(u,'(3A,F18.6,9A)') sym, fun, 'Energy'
        write(u,'(3A,F18.6,9A)') sym, fun, pot2char(IEIG), E(IEIG)*eV, eV_
        write(u,'(3A,F18.6,9A)') sym, fun, '----------------------------'
        write(u,'(3A,F18.6,9A)') sym, fun, 'kin', (E(IEIG)-2*E(IHTR)-E(IVXC)-E(ICOU))*eV, eV_
        write(u,'(3A,F18.6,9A)') sym, fun, pot2char(IHTR), E(IHTR)*eV, eV_
        write(u,'(3A,F18.6,9A)') sym, fun, pot2char(ICOU), E(ICOU)*eV, eV_
        write(u,'(3A,F18.6,9A)') sym, fun, 'Ees',(E(ICOU)+E(IHTR))*eV, eV_
        write(u,'(3A,F18.6,9A)') sym, fun, pot2char(IEXC), E(IEXC)*eV, eV_
        write(u,'(3A,F18.6,9A)') sym, fun, '============================'
        write(u,'(3A,F18.6,9A)') sym, fun, pot2char(ITOT), E(ITOT)*eV, eV_
        write(u,'(3A,ES18.2,A)') sym, fun, 'dif', (E(ITOT)-E(IPRE))*eV, eV_
        write(u,'(9A)') ! empty line
      endif
      E(IPRE) = E(ITOT) ! store total energy of this iteration as previous total energy of the next iteration

      ! check difference of new and old potential density
      res = sum( rho(:,ITT,ITOT)*abs( pot(:,ITT,ITOT,INEW)-pot(:,ITT,ITOT,IOLD) )*g%r**2*g%dr )
      icyc = icyc+1 ! count up SCF cycle counter by one per solving step

      if(o>0) write(o,'(3A,I0,A,F24.14,2A,ES10.2,9A)') sym, fun, 'cycle #', icyc, '  Etot =', E(ITOT)*eV, eV_, ' residual', res*eV, eV_

    task = 'mixpot' ! set next task
  !===================================================================================================================================
  case( 'mixpot' ) ! task
    if(u>0) write(u,'(9A)') sym, fun, 'task "', trim(task), '".'

    if( .not. run ) then
      alf = 0. ! do not mix in the last iteration
      run  = .true. ! run one more iteration to do this task: 'solve'
    endif ! last iteration

! cDBG  if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'in cycle#', icyc, ' mix potentials with ', nint(100.*alf), '%'
    ! mix old and new potentials linearly
    pot(:,:,:,IOLD) = ( 1. - alf ) * pot(:,:,:,IOLD) + alf * pot(:,:,:,INEW)

    alf = alpha * icyc/(icyc+.5*Z+5.) ! set to mixing coefficient for the next iteration

    task = 'solve' ! set next task
  !===================================================================================================================================
  case default
    if(o>0) write(o,'(9A)') sym, fun, ERROR, 'undetermined task "', trim(task), '"'
    stop 'AE scf_atom: ERROR an undetermined task was invoked!'
  !===================================================================================================================================
  endselect ! task
  !===================================================================================================================================
  !-----------------------------------------------------------------------------------------------------------------------------------
   !
    !
     !
      !-------------------------------------------------------------------------------------------------------------------------------
    enddo ! while run


    ! store the self-consistent potential in a file
    write( unit=potfile, fmt='(A,I3.3)', iostat=is ) 'pot/rV.', nint(Z)
    is = write2file( potfile, g%r(1:)*pot(1:,0,ITOT,INEW)-Z, grid=g%r(1:) )
    if( is /= 0 .and. o>0) write(o,'(9A)') sym, fun, 'create directory "pot/" to store the self-consistent potentials.'


    if( any( egap(iupdn,IVAL) < egap(iupdn,ICOR) ) ) then
      if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'a core state is higher in energy than the lowest valence state!'
    endif ! lowest valence energy is lower than highest core energy

    if( res > THRESHOLD ) then
      if(o>0) write(o,'(4A,F0.1,9A)') sym, fun, WARNING(0), 'self-consistency for Z = ', Z, ' did not converge!'
    endif ! res > THRESHOLD

cDBG  is = write2file( 'dmp/cnvrho', rho(99:,:,:), grid=g%r(99:) )
cDBG  is = write2file( 'dmp/cnvrhoval', rho(99:,ITT,IVAL), grid=g%r(99:) )
cDBG  is = write2file( 'dmp/cnvpot', pot(99:,:,:,IOLD), grid=g%r(99:) )

    if(o>0) write(o,'(9A)') ! empty line

cDBG  is = write2file( 'dmp/Ves_ae', pot(1:,0,IHTR,INEW)-Z/g%r(1:), grid=g%r )

    ! optional output
    if( present( rhocor ) ) then
cDBG  if( ubound(rhocor,1) < g%imx ) stop 'scf_atom: dimension of RHOCOR does not match with the grid.'
      selectcase( 10 * size(rhocor,2) + spin )
      case( 10 ) ; rhocor(:,1) = rho(:,ITT,ICOR) ! spin integrated
      case( 11 ) ; rhocor(:,1) = rho(:,ITT,ICOR) ! spin average
      case( 20 ) ; rhocor(:,1) = rho(:,ITT,ICOR)*.5 ; rhocor(:,2) = rho(:,ITT,ICOR)*.5
      case( 21 ) ; rhocor(:,1) = rho(:,IUP,ICOR)    ; rhocor(:,2) = rho(:,IDN,ICOR)
      case( 30 ) ; rhocor(:,1) = rho(:,ITT,ICOR)*.5 ; rhocor(:,3) = rho(:,ITT,ICOR)*.5 ; rhocor(:,2) = rho(:,ITT,ICOR)
      case( 31 ) ; rhocor(:,1:3) = rho(:,IDN:IUP,ICOR)
      case default  ; stop 'AE scf_atom: cannot translate spin indices for RHOCOR!'
      endselect ! 10 * size(rhocor,2) + spin
    endif ! present rhocor

    ! optional output
    if( present( kincor ) ) then
      ! includes also the Coulomb potential -Z/r
      kincor = -sum( rho(:,ITT,ICOR)*( pot(:,ITT,ITOT,IOLD)*g%r - Z )*g%r*g%dr )
      do is = -spin, spin, 1+spin ! skip ITT
        kincor = kincor + sum( eigenvalue(is,:ubound(occ,2)) * occ(is,:,ICOR) )
      enddo ! is
cDBG  if(o>0) write(o,'(3A,F16.6,9A)') sym, fun, 'kinetic energy of the core:', kincor*eV, eV_
    endif ! present kincor

    ! optional output: self-consistent potential
    if( present( sc_pot ) ) then
cDBG  if( ubound(sc_pot,1) < g%imx ) stop 'scf_atom: dimension of SC_POT does not match with the grid.'
      sc_pot = pot(:,ITT,ITOT,IOLD)
    endif ! present rhocor

    ! optional output: eigenvalues in the valence regime
    if( present( valeig ) ) then
      valeig = 0. ! init
      do ell = 0, ubound(valeig,2)

        enn = ell+1
          inl = ennell2inl( enn, ell )
        if (inl <= ubound(occ,2)) then
          do while( occ(ITT,inl,ICOR) > 0. )
            enn = enn+1 ! try the next higher state
            inl = ennell2inl( enn, ell )
          enddo ! while

          if( occ(ITT,inl,IVAL) > 0. ) then
            valeig(1,ell) = eigenvalue(ITT,inl)
cDBG        if(o>0) write(o,'(5A,F16.6,9A)') sym, fun, &
cDBG          'use eigenvalue of the ', inl2string(inl), '-state as energy parameter', valeig(1,ell)*eV, eV_
          endif ! occ > 0
        endif ! inl in range

        enn = enn+1 ! try the next higher state
        inl = ennell2inl( enn, ell )

        if (inl <= ubound(occ,2)) then
         if( occ(ITT,inl,IVAL) > 0. ) then
          if (inl <= ubound(eigenvalue, 2)) then
            valeig(2,ell) = eigenvalue(ITT,inl)
cDBG        if(o>0) write(o,'(5A,F16.6,9A)') sym, fun, &
cDBG          'use eigenvalue of the ', inl2string(inl), '-state as  2nd e-parameter', valeig(2,ell)*eV, eV_
          endif
         endif ! occ > 0
        endif

      enddo ! ell
    endif ! present valeig

    
    ! optional output: all eigenvalues
    if( present( alleig ) ) then
      alleig(:,:) = 0. ! init
      alleig(-spin:spin,:) = eigenvalue(-spin:spin,:min(45,size(alleig,2)))
    endif ! present alleig
    
! cDBG  call store_pot( pot(:,ITT,ITOT:ITOT,IOLD), g%r, g%dr, Z )


    etot = E(ITOT) ! return value
    if (present(energies)) then
      energies(1) = E(IEIG)
      do is = -spin,spin,1+spin
        energies(1) = energies(1) - sum(rho(:,is,ITOT)*(pot(:,is,ITOT,INEW)*g%r-Z)*g%r*g%dr)
      enddo
      energies(2) = E(IEIG) - E(IHTR) - E(IVXC) - energies(1)
      energies(3) = E(IEXC)
    endif


! #define PLOTme
#ifdef PLOTme
!     !! ../fig/ShootingMethod/
!     is = write2file( 'dmp/cnvpot', ( pot(99:,ITOT,IOLD)-Z/g%r(99:) )*eV, grid=g%r(99:) )
!     irwf = 1
!     enn = 4
!     ell = 1
!     is = write2file( 'dmp/cnvpotp', ( pot(99:,ITOT,IOLD)-Z/g%r(99:) + 0.5*ell*(ell+1)/g%r(99:)**2 )*eV, grid=g%r(99:) )
! 
!     inl = ennell2inl( enn, ell )
!     ! Ge 4p energy: -4.051831 eV
!     ir = shooting_method( enn, ell, g, Z, V=pot(:,ITOT,IOLD), E=eigenvalue(inl), rf=rwf(:,irwf), r2rho=r2rwf2 )
!     is = write2file( 'dmp/cnv1s', rwf(9:,irwf), grid=g%r(9:) )
!     ir = shoot_at_energy( enn, ell, g, Z, V=pot(:,ITOT,IOLD), E=eigenvalue(inl)-5./27.21, rf=rwf(:,irwf), r2rho=r2rwf2 )
!     is = write2file( 'dmp/cnv1s+', rwf(9:,irwf), grid=g%r(9:) )
! 
!     stop 'PLOTme line 810'
#endif

#ifdef DEBUG
!     inl = 11 ! 5s
!     if(o>0) write(o,'(3A,I4)') sym, fun, 'inl =', inl
!     if(o>0) write(o,'(3A,ES10.2)') sym, fun, 'E =', eigenvalue(inl)
! 
!     is = write2file( 'dmp/rwf', rwf(9:,1), grid=-g%r(9:) )
!     is = write2file( 'dmp/pot', pot(9:,ITOT,IOLD)-Z/g%r(9:), grid=-g%r(9:) )
!     is = write2file( 'dmp/kin', (eigenvalue(inl)-pot(9:,ITOT,IOLD)+Z/g%r(9:)), grid=-g%r(9:) )
! 
!     is = write2file( 'dmp/rwf2', rwf(9:,1)**2, grid=g%r(9:) )
!     is = write2file( 'dmp/erwf2', rwf(9:,1)**2*eigenvalue(inl), grid=g%r(9:) )
!     is = write2file( 'dmp/epot_rwf2', rwf(9:,1)**2*(pot(9:,ITOT,IOLD)-Z/g%r(9:)), grid=g%r(9:) )
!     is = write2file( 'dmp/ekin_rwf2', rwf(9:,1)**2*(eigenvalue(inl)-pot(9:,ITOT,IOLD)+Z/g%r(9:)), grid=g%r(9:) )
!     stop 'AE: DEBUG line 844'
#endif
    deallocate( r2rho, r2rwf2, rho, pot, rwf, stat=ist )
  endfunction scf_atom





  !! generate 3 true partial waves at each given energy parameter
  !! and find the 1st and 2nd energy derivative
  status_t function true_partial_waves( g, Z, enpara, pot, wf ) result( ist )
  use type_rgrid, only: rgrid
  use radial_integrator, only: integrate_outwards
  use radial_integrator, only: shoot
  use unitsystem, only: eV, eV_
    type(rgrid), intent(in)         :: g  !! radial grid descriptor
    real, intent(in)                :: Z  !! atomic number
    real, intent(inout)             :: enpara(1:,0:) !! energy parameters(enn,ell)
    real, intent(in)                :: pot(0:) !! local potential (without -Z/r contribution), Hartree units
    real, allocatable, intent(out)  :: wf(:,:,:,:,:) !! radial wave functions

    character(len=*), parameter     :: fun = ' true_partial_waves: '
!     real, parameter                 :: FINITE_DE = 0.0009765625 ! Hartree == 0.027 eV
!    real, parameter                 :: FINITE_DE = 0.015625 ! Hartree == 0.425 eV
    real, parameter                 :: FINITE_DE = 30.0 * 0.015625 
    integer, parameter              :: IW = 0 !! wave function
    integer, parameter              :: IT = 1 !! kinetic energy operator times wave function
    ! arguments
    ! local vars
    real, allocatable               :: ewf(:,:)     ! r*wave function (large component only)
    real, allocatable               :: rf(:), gf(:,:)      ! r*wave function (large and small component only)
    qn_ELL_t                        :: ellmax, ell
    qn_ENN_t                        :: enn
    integer                         :: ipm, nnodes
    real                            :: e, e0, de, nrm, knk
    logical                         :: second
    real, allocatable               :: newenpara(:,:)

    ellmax = ubound(enpara,2)
    allocate(newenpara(1:ubound(enpara,1),0:ellmax))

    allocate( wf(0:g%imx,IW:IT,1:3,0:ellmax,I_TRU:I_SMT), stat=ist )
    if( ist /= 0 ) stop 'AE: true_partial_waves: failed to allocate WF (line 878)'
    allocate( gf(1:2,0:g%imx), & ! gf(1,:) large component, gf(2,:) small component
              ewf(0:g%imx,-1:+1), stat=ist ) ; if( ist /= 0 ) stop 'AE: true_partial_waves: failed to allocate!'
    ewf = 0. ; wf = 0.
    
    !if(o>0) write(o,'(3A,F0.3,2A,9(" ",F0.3))') sym, fun, 'at E +/- ', FINITE_DE*eV, eV_, ', E =', enpara(1,0:)*eV
    write(6,'(3A,F0.3,2A,9(" ",F0.3))') sym, fun, 'at E +/- ', FINITE_DE*eV, eV_, ', E =', enpara(1,0:)*eV

    ! create true partial waves
    do ell = 0, ellmax
      second = ( enpara(2,ell) /= 0. )

      if( second ) then

!         if(o>0) write(o,'(5A,2(" ",F0.6),9A)') sym, fun, 'for ', ellchar(ell),' at E =', enpara(1:2,ell)*eV, eV_
        de = enpara(2,ell) - enpara(1,ell)

        e = enpara(1,ell) 
        knk = shoot( Z, ell, g, pot(0:), E, ewf(:,-1), nnodes )
        if(o>0) write(o,'(5A,F0.6,2A,I0,A)') sym, fun, 'for ', ellchar(ell),' at E =', enpara(1,ell)*eV, eV_, ', found ', nnodes, ' nodes'
        nrm = sum( ewf(:,-1)**2 * g%dr )
        wf(:,IW,1,ell,I_TRU) = ewf(:,-1)/( g%r * sqrt(nrm) )

        e = enpara(2,ell) 
        knk = shoot( Z, ell, g, pot(0:), E, ewf(:,+1), nnodes )
        if(o>0) write(o,'(5A,F0.6,2A,I0,A)') sym, fun, 'for ', ellchar(ell),'* at E =', enpara(2,ell)*eV, eV_, ', found ', nnodes, ' nodes'
        nrm = sum( ewf(:,+1)**2 * g%dr )
        wf(:,IW,2,ell,I_TRU) = ewf(:,+1)/( g%r * sqrt(nrm) )

        ! third partial wave !! needs a change TODO , linear dependence
        wf(:,IW,3,ell,I_TRU) = ( ewf(:,+1) - ewf(:,-1) )/de ! FD energy derivative

        e = enpara(1,ell) 
        wf(1:,IT,1,ell,I_TRU) = ( e-(pot(1:)-Z/g%r(1:)) )*wf(1:,IW,1,ell,I_TRU)
        e = enpara(2,ell)
        wf(1:,IT,2,ell,I_TRU) = ( e-(pot(1:)-Z/g%r(1:)) )*wf(1:,IW,2,ell,I_TRU)

        ! third partial wave
        wf(1:,IT,3,ell,I_TRU) = ( wf(:,IT,2,ell,I_TRU) - wf(:,IT,1,ell,I_TRU) )/de ! FD energy derivative

        newenpara(:,ell) = enpara(:,ell)
      
  
      
      
      else  ! second valence state

        de = FINITE_DE
        e0 = enpara(1,ell) 
        ! generate 3 solutions of the SRA/Schro:dinger equation at/around the energy parameter e0
        do ipm = -1, +1 ; e = e0 + de*ipm
        !do ipm = -1, +1 ; e = e0 - de*ipm
          ! the enn-quantum number = #nodes+1
          enn = integrate_outwards( g, Z, ell, e, pot(0:), gf, irstop=g%imx )+1
          ewf(:,ipm) = gf(1,:)/g%r
          ewf(0,ipm) = ewf(1,ipm)
cDBG      if(o>0) write(o,'(3A,I2,2A,I2,A,F12.6,9A)') sym, fun, '(', ipm, ') ', ellchar(ell), enn-1, ' nodes, E =', e*eV, eV_
        enddo ! ipm
	if(.false.) then 
        ! create linear combinations for value, derivative and 2nd derivative
        ! at the energy parameter                                     [  0   1   0  ]
        wf(:,IW,1,ell,I_TRU) = (               1.0*ewf(:, 0)               )/de**0
        ! finite difference 1st energy derivative                     [-1/2  0  1/2 ]
        wf(:,IW,2,ell,I_TRU) = (-0.5*ewf(:,-1)              +0.5*ewf(:,+1) )/de**1
        ! finite difference 2nd energy derivative                     [  1  -2   1  ]
        wf(:,IW,3,ell,I_TRU) = ( 1.0*ewf(:,-1)-2.0*ewf(:, 0)+1.0*ewf(:,+1) )/de**2

        ! THE SCHRO:DINGER EQUATION READS
        ! (^T+VLOC)U = E*U
        ! THE SCHRO:DINGER EQUATION FOR THE 1ST ENERGY DERIVATIVE READS
        ! (^T+VLOC)UDOT = 1*U + E*UDOT
        ! THE SCHRO:DINGER EQUATION FOR THE 2ND ENERGY DERIVATIVE READS
        ! (^T+VLOC)UDOTDOT = UDOT + 1*UDOT + E*UDOTDOT = 2*UDOT + E*UDOTDOT

        ! ... AND IN MATRIX NOTATION
        !
        !     / U   \   /  E  O  O  \ / U   \
        !  ^H | UD  | = |  1  E  O  | | UD  |, WHERE ^H = ^T+V
        !     \ UDD /   \  O  2  E  / \ UDD /

        ! ... SO THE KINETIC ENERGY OPERATOR ^T GOES AS
        !
        !     / U   \   / E-V O  O  \ / U   \
        !  ^T | UD  | = |  1 E-V O  | | UD  |
        !     \ UDD /   \  O  2 E-V / \ UDD /

        ! kinetic energy operator onto energy derivatives
        e = e0
        wf(1:,IT,1,ell,I_TRU) = ( e-(pot(1:)-Z/g%r(1:)) )*wf(1:,IW,1,ell,I_TRU)!+ 0.0*wf(1:,IW,0,ell,I_TRU) ! u
        wf(1:,IT,2,ell,I_TRU) = ( e-(pot(1:)-Z/g%r(1:)) )*wf(1:,IW,2,ell,I_TRU) + 1.0*wf(1:,IW,1,ell,I_TRU) ! udot
        wf(1:,IT,3,ell,I_TRU) = ( e-(pot(1:)-Z/g%r(1:)) )*wf(1:,IW,3,ell,I_TRU) + 2.0*wf(1:,IW,2,ell,I_TRU) ! udotdot

	else 
	  
	  wf(:,IW,1,ell,I_TRU) = ewf(:, 0)
      wf(:,IW,2,ell,I_TRU) = ewf(:, 1)
      wf(:,IW,3,ell,I_TRU) = ewf(:,-1)
	  
	  e = e0 
          wf(1:,IT,1,ell,I_TRU) = ( e-(pot(1:)-Z/g%r(1:)) )*wf(1:,IW,1,ell,I_TRU)

	  e = e0 + de 
          wf(1:,IT,2,ell,I_TRU) = ( e-(pot(1:)-Z/g%r(1:)) )*wf(1:,IW,2,ell,I_TRU)

      e = e0 - de 
          wf(1:,IT,3,ell,I_TRU) = ( e-(pot(1:)-Z/g%r(1:)) )*wf(1:,IW,3,ell,I_TRU)
          
      newenpara(1,ell) = e0 
      newenpara(2,ell) = e0 + de 
      newenpara(3,ell) = e0 - de
  
    endif
    endif ! second valence state

    enddo ! ell
    wf(0,:,:,:,:) = 0. ! set r=0
    if( any( wf /= wf ) ) stop 'true_partial_waves: NaN in wf!'

  !!! create plots of the energy derivative
  !     il = 1
  !     is = write2file( 'dmp/6s_true_partial_waves', ewf(:,-1:+1), grid=g%r )
  !     ! ==> ~/RESULTS/PAW_Ge4s_deviations.agr
  !     do ipm = -1, +1, 2
  !       ! try to reproduce the partial waves 1.0 Hartree lower/higher
  !       ! than the energy parameter
  !       e = e0 + ipm * 1.0 ! Hartree
  !       enn = integrate_outwards( g, Z, ell, e, pot(0:), gf, irstop=g%imx ) + 1
  !       dwf(:,-1,0) = gf(1,:) ! original
  !       dwf(:, 0,0) = dwf(:,-1,il)+(e-e0)*dwf(:, 0,il) ! with 1st energy derivative
  !       dwf(:,+1,0) = dwf(:,-1,il)+(e-e0)*dwf(:, 0,il)+0.5*(e-e0)**2*dwf(:,+1,il) ! with 1st and 2nd energy derivative
  !       if(ipm==-1) is = write2file( 'dmp/6s_minus1Ha', dwf(:,-1:+1,0), grid=g%r )
  !       if(ipm==+1) is = write2file(  '6s_plus1Ha', dwf(:,-1:+1,0), grid=g%r )
  !     enddo ! ipm
  !     is = write2file( 'dmp/6s_true_pw0_de_d2e', dwf(:,-1:+1,il), grid=g%r )
    enpara = newenpara  
    deallocate( gf, ewf, stat=ist )
  endfunction ! true_partial_waves


  !! fit a 6-th order even polynomial (c0,c2,c4,c6) to a given function
  !! the is assumed s-like at the cutoff radius r(irc)
  status_t function pseudize_s( g, irc, tru, smt, c2 ) result( ist )
  use type_rgrid, only: rgrid
  use LAPACK, only: solve_Ax_b
  use configuration, only: WARNING
    type(rgrid), intent(in)         :: g !! radial grid descriptor
    integer, intent(in)             :: irc !! index of the cutoff radius
    real, intent(in)                :: tru(0:) !! true function
    real, intent(out)               :: smt(0:) !! smooth pseudo function
    real, intent(out), optional     :: c2(0:) !! coefficients

    character(len=*), parameter     :: fun = ' pseudize_s: '
    real                            :: A(0:3,0:3), b(0:3), c(0:3), rr
    integer                         :: i1, ir!, ip

    if( irc+1 > ubound(g%r,1) ) stop 'AE pseudize: cutoff index too large.'
    if( irc-2 < 20            ) stop 'AE pseudize: cutoff index too small.'

    do i1 = 0, 3
      ! 4 radial indices [irc-2,irc-1,irc+0,irc+1]
      ir = irc + i1 - 2 ! warning: not fully centered around irc
      rr = g%r(ir)**2
      ! set up a basis of 4 functions: r^0, r^0, r^4, r^6
      ! at 4 neighboring grid points around r(irc)
      A(i1,0:3) = rr**(/0,1,2,3/)
      ! b is the inhomogeneus right side of the set of linear equations
      b(i1) = tru(ir)
    enddo ! i1

    ist = solve_Ax_b( A, b, c ) ! solves A*x==b

    if( ist /= 0 ) then
      if(o>0) write(o,'(4A,I0)') sym, fun, WARNING(0), 'failed because solving returned status = ', ist
      smt(0:) = tru(0:) ! copy all
      return
    endif

    do ir = 0, irc
      rr = g%r(ir)**2
      smt(ir) = c(0) + rr*( c(1) + rr*( c(2) + rr*c(3) ) )
    enddo ! ir
    smt(irc+1:) = tru(irc+1:) ! copy true tail
    if( present(c2) ) c2(0:3) = c(0:3) ! output coefficients
  endfunction ! pseudize_s



  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  status_t function calc_atom_vloc( g, iZ, Z, symbol, occ, nn, rcut, kxc, spin_polarized, &
          config, check_logder, Vloc_method ) result( ist )
  use constants, only: Pi
  use type_rgrid, only: rgrid
  use type_rgrid, only: operator(.at.)
  use radial_potential, only: xc_functional
! use density_functionals, only: key2string
  use type_item, only: operator(.in.)
  use density_functionals, only: XCfnDict => Dictionary
  use radial_potential, only: Hartree_potential
  use radial_integrator, only: shooting_method
  use radial_integrator, only: integrate_outwards
  use configuration, only: WARNING
  use LAPACK, only: determinant3x3
  use LAPACK, only: invert
! use LAPACK, only: LU_decomposition_3x3
  use LAPACK, only: LU_decompose
  use LAPACK, only: LU_doolittle
  use pawdatafile, only: write_pawdata_file
  use unitsystem, only: eV, eV_, Ang, Ang_
    type(rgrid), target, intent(in) :: g
    integer, intent(in)             :: iZ !! species identifier
    real, intent(in)                :: Z  !! atomic number
    character(len=2), intent(in)    :: symbol
    real, intent(in)                :: occ(-1:,1:,ICOR:) !! occupation numbers (-1:+1,1:28,IC:IV)
    integer, intent(in)             :: nn(0:)
    real, intent(in)                :: rcut(0:6) !! rutoff radius
    integer, intent(in)             :: kxc !! xc-key
    logical, intent(in)             :: spin_polarized
    character(len=*), intent(in)    :: config
    logical, intent(in)             :: check_logder
    character, intent(in)           :: Vloc_method

    ! character(len=*), parameter     :: fun = ' calc: '
    integer, parameter              :: IW = 0 !! wave function
    integer, parameter              :: IT = 1 !! kinetic energy operator times wave function
    integer, parameter              :: IP = 2 !! projector of that wave function
    character(len=*), parameter     :: STARS(1:4) = (/'  ','* ','**','? '/) ! enn=1: no star, enn=2: *, enn=3: **
    iounit_t, save                  :: debug_unit = 0
    integer                         :: irc(0:6), ircc, ircp, i, iq, id
    qn_ENN_t                        :: en, en2, enn, enn0
    qn_ELL_t                        :: lmax, lloc, ell, el
    qn_iNL_t                        :: inl
    integer  :: iln
!     real     :: rhoext(0:g%imx) !! (0:g%imx) external density
    real, allocatable   :: wf(:,:,:,:,:) !! (0:g%imx,IW:IT,1:3,0:ellmax,I_TRU:I_SMT)
    real, allocatable   :: t4(:,:,:,:) !!
    real, allocatable   :: t3(:,:,:) !!
    real     :: pot(0:g%imx,I_TRU:I_SMT) !! (0:g%imx) self_consistent potential
    real     :: cmp(0:g%imx) !! (0:g%imx) exchange correlation of the smoothe density
    real     :: rhoaug(0:g%imx) !! (0:g%imx) self_consistent core density
    real     :: vxc(0:g%imx,0:0) !! (0:g%imx) exchange correlation of the smooth density
    real     :: ves(0:g%imx) !! (0:g%imx) electrostatic potential
    real     :: vBAR(0:g%imx) !! (0:g%imx) correction potential
    real     :: rho(0:g%imx,ICOR:IVAL,I_TRU:I_SMT) !! (0:g%imx,cor:val,tru:smt) self_consistent core density
    real     :: prj(0:g%imx,1:3,0:6) !! (0:g%imx,enn,ell) projectors
    real     :: pprj(0:g%imx,1:3) !! (0:g%imx) prelim. projectors
    real     :: swf(0:g%imx,I_TRU:I_SMT)
    real     :: mask(0:g%imx)
    real     :: kincor !! kinetic energy of the core
    real     :: valeig_ennell(3,0:6) !! (enn,ell) valence state eigenvalues
    real     :: valeig_iln(15,1) !! (inl,1) valence state eigenvalues
    real     :: c2(-1:4,IW:IP,1:3,0:6) !! (-1:4,IW:IP,1:3,ell)
    real     :: ct(0:4,3)
    real     :: c2cor(0:6)=0. !! (0:3)(I_SMT)
    real     :: c(0:4), cp(0:2) !! potential
    real     :: nrm, nrms, rc, det
    real     :: e, m(2,2), q(I_TRU:I_SMT), pp(3,3), ll(3,3), uu(3,3), li(3,3), ui(3,3)
    real     :: mat(3,3,IW:IP,I_TRU:I_SMT,0:6) ! mat(en2,enn,0:1,ell)
    real     :: occ_val(-1:+1,3,0:6) ! (spin,enn,ell)
    real     :: etot
    integer  :: nWexp, spin, is
    real, pointer :: r(:)
    logical  :: loc = .false. ! if true uses an atomic orbital to produce vloc (assigned true if V=s,p,d,f in input file
    character(len=4) :: fun = ' __ ' ! will be set to ' Sy ', where Sy is the chemical symbol

    real, allocatable   :: A(:,:) !!
    real, allocatable   :: L(:,:) !!
    real, allocatable   :: U(:,:) !!
    real, allocatable   :: invL(:,:) !!
    real, allocatable   :: invU(:,:) !!

    fun(2:3) = symbol

!     ! Confinement potential
!     real     :: extpot(0:g%imx)
!     extpot(0:g%imx) = 0.
!     extpot(0:g%imx) = exp( g%r - (g%Rmx-4.)  )
!     is = write2file( 'dmp/confinement', extpot, grid=g%r )

    r => g%r

    id = g .at. 0.01 ! for display, start plots at a finite radius

    c2 = 0. ! init
    
    

    spin = 0 ! calculate spin-paired
    if( any( occ(IDN,:,:) /= occ(IUP,:,:) ) ) then
      ! some orbital has a magnetization
      if( spin_polarized ) then
        spin = 1 ! calculate spin-polarized
        if(o>0) write(o,'(/,3A,/)') sym, fun, 'Magnetization found. Perform a spin-polarized AE calculation.'
      else  ! spin_polarized
        if(o>0) write(o,'(/,4A,/)') sym, fun, WARNING(0), 'Magnetization found, but spin-paired AE calculation!'
      endif ! spin_polarized
    else  ! magnetization
      if(o>0) write(o,'(/,3A,/)') sym, fun, 'no Magnetization found, perform a spin-paired AE calculation.'
    endif ! magnetization


    lmax = 0
    do ell = 0, ubound(nn,1)
      if( nn(ell) > 0 ) lmax = max( lmax, ell )
    enddo ! ell
    lloc = lmax+1

    etot = scf_atom( spin, g, Z, symbol, occ, kxc, sc_pot=pot(:,I_TRU), &
!                      potext=extpot, & !! here an external confinement potential can be introduced
                     rhocor=rho(:,ICOR:ICOR,I_TRU), kincor=kincor, valeig=valeig_ennell(1:3,0:lloc) )

    nWexp = min( 2*lmax, 4 )+1 ! Weinert exponent for the compensation charge densities
    nWexp = 3 ! Weinert exponent for the compensation charge densities

    ! create true partial waves by integrating the SRA equation outwards
    ist = true_partial_waves( g, Z, valeig_ennell(:,0:lloc), pot(:,I_TRU), wf )
    !write (*,*) 'valeig is ', valeig_ennell(:,0:lloc)*eV
    ! ... returns with the array allocated as wf(0:imx,IW:IT,1:3,0:lloc,I_TRU:I_SMT)
    if( any( wf /= wf ) ) stop 'AE calc: NaN in wf'
    
    !write (*,*) 'nn(ell)', nn(0:)
#ifdef DETAILS
    debug_unit = o ! turn outputs on
#else
    debug_unit = 0 ! turn outputs off
#endif

    rho(:,IVAL,I_TRU:I_SMT) = 0. ! init

    mask = 1.!( 1. - (g%r(:)/g%rmx)**4 )**4

    irc = g .at. rcut

    occ_val = 0. ! init
    do ell = 0, lloc

      if(debug_unit>0) write(debug_unit,'(4A,F0.3,2A,I0,9A)') sym, fun, ELLCHAR(ell), '-state is pseudized at ', r(irc(ell))*Ang, Ang_, ' = r(', irc(ell), ')'
!       i = g%imx ! normalize true partial waves to atomic solutions, if possible in full space
!       if( valeig(ell) == 0. ) i = irc(ell) ! otherwise only inside the sphere

      ! normalize true partial waves in full space with the mask

      enn = 1 ! ground state
      nrm = sum( wf(:,IW,enn,ell,I_TRU)**2 * g%r2dr )! * mask(:)
      if(debug_unit>0) write(debug_unit,'(5A,F0.9)') sym, fun, 'normalize ', ELLCHAR(ell), '-state, norm = ', nrm !,' (with mask).'
      nrm = 1./sqrt(nrm)
      wf(:,IW:IT,:,ell,I_TRU) = wf(:,IW:IT,:,ell,I_TRU)*nrm ! scale all states

      ! normalize also 2nd atomic states
      enn = 2
      if( valeig_ennell(enn,ell) /= 0. ) then
        nrm = sum( wf(:,IW,enn,ell,I_TRU)**2 * g%r2dr )! * mask(:)
        if(debug_unit>0) write(debug_unit,'(5A,F0.9)') sym, fun, 'normalize ', ELLCHAR(ell), '*-state, norm = ', nrm!,'  (with mask).'
        nrm = 1./sqrt(nrm)
        wf(:,IW:IT,enn,ell,I_TRU) = wf(:,IW:IT,enn,ell,I_TRU)*nrm
      endif ! second

      do enn = 1, 3

        swf(:,I_TRU) = wf(:,IW,enn,ell,I_TRU)/g%r**ell ! reduce to s-wave function

        ! pseudize with a polynomial c(0) + c(1) r^2 + c(2) r^4 + c(3) r^6
        ! matching value, 1st, 2nd and 3rd derivative at Rc
        ist = pseudize_s( g, irc(ell), tru=swf(:,I_TRU), smt=swf(:,I_SMT), c2=c2(0:3,IW,enn,ell) )

        if(debug_unit>0) write(debug_unit,'(5A,I0,A,9(" ",F0.6))') sym, fun, 'pseudize ', ELLCHAR(ell), '-state as r^',ell,'*poly(r^2), poly c_hom =', c2(0:3,IW,enn,ell)

        wf(:,IW,enn,ell,I_SMT) = swf(:,I_SMT)*g%r**ell ! regain  ell-wave function
        ! now each smooth partial wave can be written as
        !   rPhi = r^(ell+1) sum_{i=0,3}( c2(i,IW,enn,ell) r^(2i) )
        ! the kinetic energy operator in radial representation
        !   -1/(2m) d^2/dr^2 + (ell(ell+1))/(2mr^2)      where m=1 is the electron mass
        ! therefore, the kinetic energy operator applied to the smooth partial wave is given by
        !   r^(ell+1) sum_{i=0,2}( c2(i,IT,enn,ell) r^(2i) )
        ! and the coefficients IT are found from IW by
        !   c2(i,IT,enn,ell) = -1/2 c2(i+1,IW,enn,ell)*(2*ell+1)(2i+2+2*ell+1), i=0:2

        ! c2(0:2,IT,enn,ell) = 0.5*c2(1:3,IW,enn,ell)*( ell*(ell+1)-(ell+1+(/2,4,6/))*(ell+(/2,4,6/)) ) ! works
        c2( : ,IT,enn,ell) = 0.
        c2(0:2,IT,enn,ell) = -0.5*c2(1:3,IW,enn,ell)*(/2,4,6/)*( (2*ell+1) + (/2,4,6/) ) ! works, too
        !          0    1     2     3
        !
        !        / 0   2(D+2)             \    0
        ! -2mT = |      0    4(D+4)       |    1
        !        |            0    6(D+6) |    2
        !        \                  0     /    3  where D=2ell+1
        !
        i = irc(ell)
        wf(i:,IT,enn,ell,I_SMT) = wf(i:,IT,enn,ell,I_TRU) ! copy tail
        c(0:2) = c2(0:2,IT,enn,ell) ! abbrev.
        wf(:i,IT,enn,ell,I_SMT) = ( c(0) + c(1)*r(:i)**2 + c(2)*r(:i)**4 ) * g%r(:i)**ell  ! replace inside rcut
        !
      enddo ! enn
      is = write2file( 'dmp/smooth_partial_ell'+ell, wf(:,IW, :, ell, I_SMT), grid=g%r )
      ! find enn0
      enn0 = ell
      do enn = ell+1, 7
        inl = ennell2inl( enn, ell )
        if( occ(ITT,inl,ICOR) > 0. ) enn0 = enn
      enddo ! enn
      if( enn0 > ell ) then
        if(debug_unit>0) write(debug_unit,'(5A,I0,9A)') sym, fun, 'the highest core state for ', ELLCHAR(ell), ' is the ',enn0,ELLCHAR(ell),'-state.'
      else
        if(debug_unit>0) write(debug_unit,'(5A,I0,9A)') sym, fun, 'no core state for ', ELLCHAR(ell)
      endif

      ! create smooth valence density from enn==1 state, if occupied
      do enn = 1, nn(ell)
        inl = ennell2inl( enn0+enn, ell )
        if( any( occ(:,inl,IVAL) > 0. ) ) then

          occ_val(:,enn,ell) = min( max( 0., occ(:,inl,IVAL) ), 2.*ell+1. ) ! store occupation numbers for the PAW data file
          occ_val(0,enn,ell) = occ_val(+1,enn,ell) + occ_val(-1,enn,ell) ! up+dn spin

          i = irc(ell)
          nrm  = sum( wf(:,IW,enn,ell,I_TRU)**2 * g%r2dr )

          ! check the state localization
          nrms = sum( wf(:i,IW,enn,ell,I_TRU)**2 * g%r2dr(:i) )
          if( nrms > 0.9 * nrm ) then
            if(o>0) write(o,'(5A,F0.3,9A)') sym, fun, WARNING(0), inl2string(inl), '-state is localized ', nrms/(.01*nrm), ' % inside the sphere ==> core state?'
          else  !
            if(debug_unit>0) write(debug_unit,'(4A,F0.3,9A)') sym, fun, inl2string(inl), '-state is localized ', nrms/(.01*nrm), ' % in the sphere'
          endif ! state is located mostly inside the sphere

          if( nrm <= 0. ) stop 'calc_atom: enn=1 true partial wave cannot be normalized.'
          nrm = ( occ_val(ITT,enn,ell) )/nrm

          if(debug_unit>0) write(debug_unit,'(5A,9(F0.3,A))') sym, fun, 'construct density with valence state ', inl2string(inl), ' occ = ', occ_val(+1,enn,ell),' + ',occ_val(-1,enn,ell)
          do iq = I_TRU, I_SMT
            rho(:,IVAL,iq) = rho(:,IVAL,iq) + nrm * wf(:,IW,enn,ell,iq)**2
          enddo ! iq

        endif ! valence state occupied
      enddo ! inl

      if(debug_unit>0) write(debug_unit,'(9A)') sym, fun
       
    enddo ! ell

   


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

cDBG  is = write2file( 'dmp/rhotruval', rho(99:,IVAL,I_TRU), grid=g%r(99:) )

    if(o>0) write(o,'(3A,4F10.6,9A)') sym, fun, 'Augmentation Radius', r(irc(0:3))*Ang, Ang_

    ! pseudize the core density
    ircc = g .at. minval(rcut) ! radial index at the smallest cutoff radius
    if( rho(ircc,ICOR,I_TRU)/( rho(ircc,IVAL,I_TRU) + 1E-20 ) < 1E-9 ) then

      if(o>0) write(o,'(9A)') sym, fun, 'core charge is localized inside Rc, smooth core density vanishes.'
      rho(:,ICOR,I_SMT) = 0. ! set to zero

    else  ! rho_cor(Rc)/rho_val(Rc) << 1

#ifdef TRY
      ! new: pseudize rhoc with r^2-behaviour @ r=0
      if(o>0) write(o,'(9A)') sym, fun, 'pseudize rhoc with r^2-behaviour @ r=0'
      swf(1:,I_TRU) = rho(1:,ICOR,I_TRU)/g%r(1:)**2 ; swf(0,:) = 0. ! reduce by r^2
      ist = pseudize_s( g, ircc, tru=swf(:,I_TRU), smt=swf(:,I_SMT), c2=c2cor(1:4) )
      rho(:,ICOR,I_SMT) = swf(:,I_SMT)*g%r**2 ! get back r^2 factor
#else
      ! old: pseudize rhoc with a r^0-behaviour @ r=0
      if(o>0) write(o,'(9A)') sym, fun, 'pseudize rhoc with r^0-behaviour @ r=0'
      !! c2 and c2core is not used !!!!!!!!
      ist = pseudize_s( g, ircc, tru=rho(:,ICOR,I_TRU), smt=rho(:,ICOR,I_SMT), c2=c2cor(0:3) )
#endif
cDBG  do iq = I_TRU, I_SMT
cDBG    nrm = dot_product( rho(:,ICOR,iq), g%r2dr )
cDBG    nrms = dot_product( rho(:ircc,ICOR,iq), g%r2dr(:ircc) )
cDBG    if(debug_unit>0) write(debug_unit,'(5A,9(F12.6,A))') sym, fun, 'total ', tru2char(iq), ' core charge =', nrm, ' e (', nrms,' e inside Rc)'
cDBG  enddo ! iq

    endif ! rho_cor(Rc)/rho_val(Rc) << 1

    rho(0,:,:) = rho(1,:,:) ! extrapolate origin 0th order
    is = write2file( 'dmp/rhoval', rho(id:,IVAL,I_SMT), grid=g%r(id:) )
    is = write2file( 'dmp/rhocor', rho(id:,ICOR,I_SMT), grid=g%r(id:) )
    is = write2file( 'dmp/rhotot', rho(id:,ICOR,I_SMT)+rho(id:,IVAL,I_SMT), grid=g%r(id:) )
    do iq = I_TRU, I_SMT
      q(iq) = dot_product( rho(:,ICOR,iq)+rho(:,IVAL,iq), g%r2dr )
cDBG  if(debug_unit>0) write(debug_unit,'(4A,F16.6,9A)') sym, fun, 'total charge: ', tru2char(iq), q(iq), ' e'
    enddo ! iq

    ircp = g .at. minval(rcut)
    ! create compensation charge
    cmp = 0.
    cmp(:ircp) = ( r(ircp)**2 - r(:ircp)**2 )**nWexp ! Weinert function
    nrm = dot_product( cmp, g%r2dr )
    cmp = cmp/nrm ! normalize the Weinert function to a monopole moment of unity

cDBG  is = write2file( 'dmp/rhocmp', ( -Z+q(I_TRU)-q(I_SMT) )*cmp(id:), grid=g%r(id:) )
    rhoaug = rho(:,ICOR,I_SMT)+rho(:,IVAL,I_SMT)+ ( -Z+q(I_TRU)-q(I_SMT) )*cmp

    ! check norm of the augmented density
    nrm = dot_product( rhoaug, g%r2dr )
cDBG  if(debug_unit>0) write(debug_unit,'(4A,F16.6,9A)') sym, fun, 'total charge: ', 'aug', nrm, ' e'
    if( abs( nrm ) > 1E-9 .and. o>0) write(o,'(4A,F0.9,9A)') sym, fun, WARNING(0), 'rhoaug has, norm = ', nrm, ' e!'

#ifdef NOHartreePotential
 !   rhoaug = -Z*cmp
 !   nrm = dot_product( rhoaug, g%r2dr )
 !   if(o>0) write(o,'(4A,F12.6,9A)') sym, fun, WARNING(0), 'NOHartreePotential activated: rhoaug = -Z*cmp, norm =', nrm, ' e'
#endif

    is = write2file('dmp/rhoaug', rhoaug, g%r)

!     call xc_functional( spin=0, fpirho=rho(:,ICOR:ICOR,I_TRU)+rho(:,IVAL:IVAL,I_TRU), Vxc=vxc, Exc=ves, key=kxc, rgd=g%d, rgr=g%r, rgdrdi=g%dr ) ; ves = 0. ! dump
!     call Hartree_potential( g, rho=rho(:,ICOR,I_TRU)+rho(:,IVAL,I_TRU), vH=ves )
! cDBG  is = write2file( 'dmp/newtruves', ves, grid=g%r )
! cDBG  is = write2file( 'dmp/newtruvxc', vxc, grid=g%r )
! cDBG  is = write2file( 'dmp/newtruveff', vxc(:,0)+ves, grid=g%r )



! cDBG  is = write2file( 'dmp/smtrho', sum( rho(:,ICOR:IVAL,I_SMT), dim=2 )/(4*Pi), grid=g%r )
cDBG  is = write2file( 'dmp/smtrho', ( rho(:,ICOR:ICOR,I_SMT)+rho(:,IVAL:IVAL,I_SMT) )/(4*Pi), grid=g%r )
cDBG  is = write2file( 'dmp/rho', sum( rho(:,ICOR:IVAL,:), dim=2 )/(4*Pi), grid=g%r )
cDBG  is = write2file( 'dmp/smtrhoaug', rhoaug/(4*Pi), grid=g%r )
  !ircp = g .at. minval(rcut)
  !rho(ircp:,:,I_SMT) = 0.
    ! generate the exchange correlation potential from the smooth total density ! ves will be overwritten in the next step
    ist = xc_functional( spin=0, fpirho=rho(:,ICOR:ICOR,I_SMT)+rho(:,IVAL:IVAL,I_SMT), Vxc=vxc, Exc=ves, key=kxc, rgd=g%d, rgr=g%r, rgdrdi=g%dr ) ; ves = 0. ! dump
    ! generate the Hartee potential from the density
    ist = Hartree_potential( g, rho=rhoaug, vH=ves )

    is = write2file( 'dmp/smtves', ves, grid=g%r )
    is = write2file( 'dmp/smtvxc', vxc, grid=g%r )
    is = write2file( 'dmp/smtveff', vxc(:,0)+ves, grid=g%r )

    ! index of the innermost cutoff radius
    ircp = g .at. minval(rcut)
    !pot(1:,I_SMT) = pot(1:,I_TRU)-Z/r(1:) ! copy true tail ???????????
    
    pot(:,I_SMT) = vxc(:,0) + ves(:) 
    
    
    selectcase( Vloc_method )
    case( 'M', 'm' ) ! use the scheme recommended by Y. Morikawa

      if(o>0) write(o,'(9A)') sym, fun, 'pseudize local potential in Morikawa-style.'
      ! Vloc = c1*(r/rc)^2 + c2*(r/rc)^4 ===> Vloc(r=0) = 0

      ! / 1  1 \ / c1 \   /     V(r=rc)   \
      ! |      | |    | = |               |
      ! \ 2  4 / \ c2 /   \ rc*dV/dr|r=rc /
      m = reshape( (/ 2.0, -0.5, -1., 0.5 /), (/2,2/) )
      cp(1:2) = matmul( (/ pot(ircp,I_SMT), g%r(ircp)*( pot(ircp+1,I_SMT)-pot(ircp-1,I_SMT) )/(2.*g%dr(ircp)) /), m )
      pot(:ircp,I_SMT) = ( cp(1) + cp(2)*(g%r(:ircp)/g%r(ircp))**2 )*(g%r(:ircp)/g%r(ircp))**2 ! replace with this 2 parameter polynomial

    case( 'C', 'c' ) ! use the sinc-shaped potential

      if(o>0) write(o,'(9A)') sym, fun, 'pseudize local potential in Sinc-shape.'

      cp(2) = pot(ircp,I_SMT) ! value
      cp(1) = ( pot(ircp+1,I_SMT)-pot(ircp-1,I_SMT) )/( 2.*g%dr(ircp) ) ! finite difference derivative
      if(o>0) write(o,'(3A,F0.9,9A)') sym, fun, 'radius of the augmentation = ', g%r(ircp)*Ang, Ang_
      if(o>0) write(o,'(3A,F0.9,9A)') sym, fun, 'value of the local potential = ', cp(2)*eV, eV_
      if(o>0) write(o,'(3A,F0.9,9A)') sym, fun, 'derivative of the local potential = ', cp(1), ' au'
      if(o>0) write(o,'(3A,F0.9,9A)') sym, fun, 'normalized logder of the local potential = ', cp(1)/cp(2)*g%r(ircp)

      cp(0) = match_sinc( cp(1)/cp(2)*g%r(ircp) ) ! find k such that sinc(k*1) has the correct logder
      cp(1) = cp(0) / g%r(ircp)
      cp(2) = pot(ircp,I_SMT) / sinc( cp(0) ) ! prefactor
      if(o>0) write(o,'(3A,F0.9,9A)') sym, fun, 'local potential at the origin = ', cp(2)*eV, eV_

      ! generate smooth (repulsive) potential
      pot(:ircp,I_SMT) = cp(2) * sinc( cp(1) * r(:ircp) ) ! replace with a sinc-function inside rcut

!       do i = 1, g%imx
! !         write(9,'(9F24.12)') r(i)*Ang, max( nrm, pot(i,I_TRU)-Z/r(i) )*eV, pot(i,I_SMT)*eV
!         write(9,'(9F24.12)') r(i), max( -10., pot(i,I_TRU)-Z/r(i) ), pot(i,I_SMT)
!       enddo ! i
!       stop 'xmgrace -nxy fort.9 &'

    case( 's', 'S', '0' ) ; ell = 0 ; loc = .true.
    case( 'p', 'P', '1' ) ; ell = 1 ; loc = .true.
    case( 'd', 'D', '2' ) ; ell = 2 ; loc = .true.
    case( 'f', 'F', '3' ) ; ell = 3 ; loc = .true.
    case( 'g', 'G', '4' ) ; ell = 4 ; loc = .true.

    case default

      ! pseudize the potential with a parabola
      m = reshape( (/ g%r(ircp+0)**2, -g%r(ircp-1)**2, -1., 1. /), (/2,2/) )
      m = m / (( g%r(ircp+0)+g%r(ircp-1) )*( g%r(ircp+0)-g%r(ircp-1) ))
      cp(0:1) = matmul( pot(ircp-1:ircp+0,I_SMT), m )
      pot(:ircp,I_SMT) = ( cp(0) + cp(1)*r(:ircp)**2 ) ! replace with a parabola inside rcut

      if(o>0) write(o,'(3A,9(F0.6,A))') sym, fun, 'pseudize local potential as ', cp(0)*eV, ' + (r/R)^2 * ', cp(1)*r(ircp)**2*eV, eV_

    endselect ! Vloc_method


    if (loc) then! find the local potential by inverting the pseudo wave function of Lcut=Lmax+1
      !!       ( T + Vloc ) Psi_l = E_l Psi_l
      !!                      T Psi_l
      !! <==>  Vloc = E_l -  ---------
      !!                       Psi_l
      i = ircp ! abbreviation for the cuoff radius index
      !ell = lloc
  

      enn = 1
      pot(1:i,I_SMT) = valeig_ennell(1,ell) - wf(1:i,IT,enn,ell,I_SMT)/wf(1:i,IW,enn,ell,I_SMT)
      if(6>0) write(6,'(5A,F0.6,9A)') sym, fun, &
        'local smooth potential found by inverting the radial KS equation for ell=',ELLCHAR(ell),' at E = ', valeig_ennell(1,ell)*eV, eV_
      pot(0,I_SMT) = pot(1,I_SMT) ! extrapolate 0-th order

!       nrm = minval(pot(:,I_SMT))
!       do i = 1, g%imx
!         write(9,'(9F24.12)') r(i)*Ang, max( nrm, pot(i,I_TRU)-Z/r(i) )*eV, pot(i,I_SMT)*eV
!       enddo ! i
!       stop 'xmgrace -nxy fort.9 &'

    endif
    
    !is = write2file( 'dmp/vsmt', pot(:,I_SMT), grid=g%r )
    vBAR = 0.
    vBAR(:ircp) = pot(:ircp,I_SMT) - ves(:ircp) - vxc(:ircp,0)

cDBG  is = write2file( 'dmp/smtpot', pot(:,I_SMT), grid=g%r )
cDBG  is = write2file( 'dmp/pot', pot(id:,:), grid=g%r(id:) )
cDBG  is = write2file( 'dmp/vBAR', vBAR, grid=g%r )


  
    !!!!!!!!!!!!!!!! PROJECTORS SETUP   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    prj = 0. ! init
   
    debug_unit = 0

    do ell = 0, lmax
      pprj = 0. ! init prelim. projectors

      e = valeig_ennell(1,ell)
!       i = max(ircc,ircp,irc(ell))
      i = irc(ell)
      ! set up preliminary unorthogonalized projectors pprj
      

      !!! THIS IS THE DEFAULT PRELIMIARYPROJECTOR CONSTRUCTION
      !pprj(:i,1) = wf(:i,IT,1,ell,I_SMT) + ( pot(:i,I_SMT) - e )*wf(:i,IW,1,ell,I_SMT)!- 0.0*wf(:i,IW,0,ell,I_SMT)
      !pprj(:i,2) = wf(:i,IT,2,ell,I_SMT) + ( pot(:i,I_SMT) - e )*wf(:i,IW,2,ell,I_SMT) - 1.0*wf(:i,IW,1,ell,I_SMT)
      !pprj(:i,3) = wf(:i,IT,3,ell,I_SMT) + ( pot(:i,I_SMT) - e )*wf(:i,IW,3,ell,I_SMT) - 2.0*wf(:i,IW,2,ell,I_SMT)
  
      
      do enn = 1, nn(ell) !! CHANGED FROM STUFF ABOVE
        !e = valeig_ennell(enn,ell)
	pprj(:i,enn) = wf(:i,IT,enn,ell,I_SMT) + ( pot(:i,I_SMT) -  valeig_ennell(enn, ell) )*wf(:i,IW,enn,ell,I_SMT)
	!pprj(:i,enn) = wf(:i,IT,enn,ell,I_SMT) + ( pot(:i,I_SMT) -  e  )*wf(:i,IW,enn,ell,I_SMT)

	!pprj(:i,enn) = wf(:i,IW,enn,ell,I_SMT) 
	!!write (*,*) 'subtracting energy ', enn-1, 'for ell ', ell, 'and i have energy = ', e*eV
      enddo 


       if(ell==0) is = write2file( 'dmp/pprj_s', pprj, grid=g%r )
       if(ell==1) is = write2file( 'dmp/pprj_p', pprj, grid=g%r )
       if(ell==2) is = write2file( 'dmp/pprj_d', pprj, grid=g%r )

      i = irc(ell) ! abbrev.
!       Rc = r(i) !! but irc is not the center of the matching
      Rc = sum( r(i-2:i+1)*(/1,2,2,1/) )/6. ! interpolation of all 4 points
      if(debug_unit>0) write(debug_unit,'(/,9A)') sym, fun, ellchar(ell), ' ==========================================='
      if(debug_unit>0) write(debug_unit,'(4A,F0.6,9A)') sym, fun, ellchar(ell), ' Rcut = ', Rc*Ang, Ang_


      ! numerical evaluation of the overlap of preliminary projectors and smooth partial waves
      if(debug_unit>0) write(debug_unit,'(9A)') sym, fun, ellchar(ell), ' ==========================================='
      i = irc(ell) ! abbrev.
      do enn = 1, nn(ell) !! CHANGED FROM 3
        do en2 = 1, nn(ell) !! CHANGED FROM 3
          pp(enn,en2) = sum( pprj(:i,en2)*wf(:i,IW,enn,ell,I_SMT)*g%r2dr(:i) )
        enddo ! en2
      enddo ! enn
      if(debug_unit>0) write(debug_unit,'(9A)') sym, fun, ellchar(ell), ' numerical overlap'
      if(debug_unit>0) write(debug_unit,'( 3(ES16.6,2ES14.6,/),/)') pp



! begin ORTHOGONALIZATION
      !! need a matrices A, L, U nn(ell)*nn(ell) (number of projectors for channel ell) 
       !!! allocate the matrices needed to impose ortogonality of projectors 
      allocate(A(nn(ell), nn(ell)), stat=ist)
      allocate(L(nn(ell), nn(ell)), stat=ist)
      allocate(U(nn(ell), nn(ell)), stat=ist)
      
      
      allocate(invL(nn(ell), nn(ell)), stat=ist)
      allocate(invU(nn(ell), nn(ell)), stat=ist)
      !! WE ARE IN LOOP ON ELL
      !! copy <prep|psi> into A
       do enn = 1, nn(ell) 
         do en2 = 1, nn(ell) 
           A(enn, en2) = pp(enn, en2)
         enddo ! en2
       enddo ! enn
      
      !! do doolittle (plain, no pivoting) LU decomposition
      call LU_doolittle(A, L, U, nn(ell))

      
      !call LU_decomposition_3x3( A=pp, L=li, U=ui )
      

! cDBG      pp = matmul( li, ui ) ! check OK
! cDBG      if(o>0) write(o,'(4A,9(/,3ES14.6,/))') sym, fun, ellchar(ell), ' try to reproduce the overlap', pp

      !det = invert3x3( li, ll )
      !det = invert3x3( ui, uu )

      det = invert( L, invL )
      det = invert( U, invU )

      !MOD
      !det = invert(A, invU)
 !     invL = 0.
 !     do enn = 1, nn(ell)
!      invL(enn, enn) = 1.
!      enddo 
      !if(debug_unit>0) write(debug_unit,'(4A,/,3( 2(ES16.6,2ES14.6),/) /)') sym, fun, ellchar(ell), '   L, U', ( ll(:,enn), uu(:,enn), enn=1,3 )
      if(debug_unit>0) write(debug_unit,'(4A,/,3( 2(ES16.6,2ES14.6),/) /)') sym, fun, ellchar(ell), '   Linv, Uinv', ( invL(:,enn), invU(:,enn), enn=1,nn(ell) )
      ! new linear combination of partial waves with L


      allocate( t3(0:g%imx,IW:IT,1:nn(ell)), stat=ist ) ; if( ist /= 0 ) stop 'AE: failed to allocate T3'
      do iq = I_TRU, I_SMT
        t3(:,:,:) = wf(:,:,:,ell,iq) ! copy into temp.
        do enn = 1, nn(ell)
          wf(:,:,enn,ell,iq) = 0. ! init
          do en2 = nn(ell), 1, -1
            e = invL(enn,en2)
            wf(:,:,enn,ell,iq) = wf(:,:,enn,ell,iq) + e * t3(:,:,en2)
cDBG        if( debug_unit>0 .and. e /= 0. .and. iq==I_SMT ) write(debug_unit,'(A,I2,F10.6,9A)') 'add enn=', en2, e, ' times for ', ellchar(ell), STARS(enn)
          enddo ! en2
        enddo ! enn
      enddo ! iq
      deallocate( t3 )
 
!       ! compute new energies
!       valeig_ennell(1,ell) = valeig(ell)
!       valeig_ennell(2,ell) = valeig(ell)+ll(2,2)/ll(2,1)
!       valeig_ennell(3,ell) = valeig(ell)+sqrt(2*ll(3,3)/ll(3,1))

      ! show new energies
      if(debug_unit>0) write(debug_unit,'(3A,3(" ",F0.6),9A)') sym, fun, 'new energies', valeig_ennell(1:3,ell)*eV, eV_

!       ! set fake energies for higher projectors, so these states will not be selected as start orbitals
!       valeig_ennell(1,ell) = valeig(ell)
!       valeig_ennell(2,ell) = valeig(ell)+1.0
!       valeig_ennell(3,ell) = valeig(ell)+2.0

      if(debug_unit>0) write(debug_unit,'(9A)') ! empty line

      ! new linear combination of projectors with U
      ! prj has been initialized to 0
      ct(0:4,1:3) = c2(0:4,IP,1:3,ell) ! copy into temp.
      do enn = 1, nn(ell)
        prj(:,enn,ell) = 0. ! init
        c2(:,IP,enn,ell) = 0.
        do en2 = 1, nn(ell)
          e = invU(en2,enn) ! transposed !!!

         ! e = invU(enn,en2) ! no transposed !!!
          prj(:,enn,ell)     = prj(:,enn,ell)     + e * pprj(:,en2)
          c2(0:4,IP,enn,ell) = c2(0:4,IP,enn,ell) + e * ct(:,en2)
cDBG      if( debug_unit>0 .and. e /= 0. ) write(debug_unit,'(A,I2,ES10.2,9A)') 'add enn=', en2, e, ' times for ', ellchar(ell), STARS(enn)
        enddo ! en2
      enddo ! enn


      

      deallocate(A, stat=ist)
      deallocate(L, stat=ist)
      deallocate(U, stat=ist)
      deallocate(invL, stat=ist)
      deallocate(invU, stat=ist)

      do enn = 1, nn(ell) !! CHANGED FROM 3
        do en2 = 1, nn(ell) !! CHANGED FROM 3
          pp(enn,en2) = sum( prj(:i,en2,ell)*wf(:i,IW,enn,ell,I_SMT)*g%r2dr(:i) )
        enddo ! en2
      enddo ! enn
      
      if(debug_unit>0) write(debug_unit,'(4A,9(/,3F14.6))') sym, fun, ellchar(ell), ' numerical new overlap', pp
     !! if(6>0) write(6,'(4A,9(/,3F14.6))') sym, fun, ellchar(ell), ' numerical new overlap', pp
! end of ORTHOGONALIZATION

      i = irc(ell)-1 ! abbrev.
      do enn = 1, nn(ell) !! CHANGED FROM 3
        do en2 = 1, nn(ell) !! CHANGED FROM 3
          do iq = I_TRU, I_SMT
            mat(en2,enn,IW,iq,ell) = sum( wf(:i,IW,en2,ell,iq)*wf(:i,IW,enn,ell,iq)*g%r2dr(:i) ) ! 2-norm
            mat(en2,enn,IT,iq,ell) = sum( wf(:i,IT,en2,ell,iq)*wf(:i,IW,enn,ell,iq)*g%r2dr(:i) ) ! kinetic
          enddo ! iq
        enddo ! en2
      enddo ! enn

      if(debug_unit>0) write(debug_unit,'(9A)') 'norm deficit'
      if(debug_unit>0) write(debug_unit,'(3ES14.6)') mat(:,:,IW,I_TRU,ell)-mat(:,:,IW,I_SMT,ell) ! norm deficit
      if(debug_unit>0) write(debug_unit,'(9A)')

      if(debug_unit>0) write(debug_unit,'(9A)') 'kinetic energy deficit'
      if(debug_unit>0) write(debug_unit,'(3ES14.6)') mat(:,:,IT,I_TRU,ell)-mat(:,:,IT,I_SMT,ell) ! kinetic deficit
      if(debug_unit>0) write(debug_unit,'(9A)')

      if(debug_unit>0) write(debug_unit,'(9A)') sym, fun, ellchar(ell), ' end ======================================='

    enddo ! ell


    ! until here the projectors still have their r^ell part

    do ell = 0, lmax
      is = write2file( 'dmp/prj_ell'+ell, prj(:,:,ell), grid=g%r )
      do enn = 1, nn(ell) ! CHANGED!!!
        prj(1:,enn,ell) = prj(1:,enn,ell)*r(1:)**(-ell)
        prj(0 ,enn,ell) = prj(1 ,enn,ell) ! 0-th order extrapolation
      enddo ! enn
    enddo ! ell

    valeig_iln = 0.
    ! change the format
    allocate( t4(0:g%imx,sum(nn(0:lloc)),1,I_TRU:I_PRJ), stat=ist ) ; if( ist /= 0 ) stop 'AE: failed to allocate T4'
    t4 = 0.
    iln = 0
    do ell = 0, lmax
      do enn = 1, nn(ell)
        iln = iln+1
        t4(:,iln,1,I_TRU) = g%r*wf(:,IW,enn,ell,I_TRU)
        t4(:,iln,1,I_SMT) = g%r*wf(:,IW,enn,ell,I_SMT)
        t4(:,iln,1,I_PRJ) = prj(:,enn,ell)
        if(debug_unit>0) write(debug_unit,'(5A,I0)') sym, fun, ellchar(ell), STARS(enn), ' iln = ', iln
        valeig_iln(iln,1) = valeig_ennell(enn,ell)
        inl = ennell2inl( enn0+enn, ell )
      enddo ! enn
    enddo ! ell

    if( any(t4/=t4) ) stop 'AE calc_atom: NaN in t4 before writing PAW file.'
    
cDBG  if(o>0) write(o,'(3A,9(F5.1,F4.1))') sym, fun, ' occ_val(up,dn) =', ( occ_val(+1,1,ell), occ_val(-1,1,ell), ell=0,lloc )
    ! write pawdata file
    ist = write_pawdata_file( iZ, Z, sum(occ_val(0,:,:)), g%r, g%dr, nn, occ_val, &
!                            xc_name=key2string(kxc), nwexp=nwexp, &
                             xc_name=(kxc .in. XCfnDict), nwexp=nwexp, &
                             rhocor=rho(:,ICOR:ICOR,I_TRU:I_SMT), &
                             ecor=kincor, etot=etot, eval=valeig_iln, &
                             vbar=vBAR, rphi=t4, rcut=r(irc(0:)), &
                             dkin=mat(:,:,IT,I_TRU,0:)-mat(:,:,IT,I_SMT,0:), &
                             config=config )
    if( check_logder ) then
      if(o>0) write(o,'(9A)') sym, fun, 'test logarithmic derivatives!'
      ist = scattering_test( valeig_ennell, g, Z, symbol, lloc, nn, pot(:,I_TRU), pot(:,I_SMT), &
                            t4, dkin=mat(:,:,IT,I_TRU,0:)-mat(:,:,IT,I_SMT,0:), &
                            chdm=mat(:,:,IW,I_TRU,0:)-mat(:,:,IW,I_SMT,0:), &
                            E_window=(/-5.,.001,1./), rcut=maxval(rcut) )
    endif ! check_logder

    deallocate( t4, stat=ist )
  endfunction ! calc_atom_vloc


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  status_t function calc_atom_sigma( g, iZ, Z, symbol, occ, nn_in, rcut, kxc, spin_polarized, &
          config, check_logder, sigma, g_xml_arg ) result( ist )
  use constants, only: Pi, sqrt4pi
  use type_rgrid, only: rgrid
  use type_rgrid, only: operator(.at.)
  use radial_potential, only: xc_functional
! use density_functionals, only: key2string
  use type_item, only: operator(.in.)
  use density_functionals, only: XCfnDict => Dictionary
  use radial_potential, only: Hartree_potential
  use radial_integrator, only: shooting_method
  use radial_integrator, only: shoot
  use radial_integrator, only: integrate_outwards, integrate_outwards_inhomogeneous
  use configuration, only: WARNING
  use LAPACK, only: determinant3x3
  use LAPACK, only: invert
  use LAPACK, only: generalized_eig
! use pawdatafile, only: write_pawdata_file
  use paw_XMLwriter, only: valence_state, write_paw_xml
  use unitsystem, only: eV, eV_, Ang, Ang_
    type(rgrid), target, intent(in) :: g
    type(rgrid), target, intent(in), optional :: g_xml_arg
    type(rgrid), pointer :: g_xml
    integer, intent(in)             :: iZ !! species identifier
    real, intent(in)                :: Z  !! atomic number
    character(len=2), intent(in)    :: symbol
    real, intent(in)                :: occ(-1:,1:,ICOR:) !! occupation numbers (-1:+1,1:28,IC:IV)
    integer, intent(inout)          :: nn_in(0:)
    real, intent(in)                :: rcut(0:) !! cutoff radius for 0:compensation_charge, 1:potential, 2:core_density, 3:matching
    integer, intent(in)             :: kxc !! xc-key
    logical, intent(in)             :: spin_polarized
    character(len=*), intent(in)    :: config
    logical, intent(in)             :: check_logder
    real, intent(in)                :: sigma ! SHO projector spread

    ! character(len=*), parameter     :: fun = ' calc: '
    integer, parameter              :: IW = 0 !! wave function
    integer, parameter              :: IT = 1 !! kinetic energy operator times wave function
    integer, parameter              :: IP = 2 !! projector of that wave function
    character(len=*), parameter     :: STARS(0:3) = (/'  ','* ','**','*3'/) ! enn=1: no star, enn=2: *, enn=3: **
    iounit_t, save                  :: debug_unit = 0
    integer, parameter :: ELL_MAX = 7
    integer                         :: ircc, ircp, ircm, i, iq, id
    qn_ENN_t                        :: en, en2, en3, enn, enn0
    qn_ELL_t                        :: lmax, lloc, ell, el
    qn_iNL_t                        :: inl
    integer  :: iln, nn(0:ELL_MAX)
    real, allocatable   :: wf(:,:,:,:,:) !! (0:g%imx,IW:IP,0:3,0:ellmax,I_TRU:I_SMT)
    real, allocatable   :: t_wf(:,:,:) !! temporary object
    real, allocatable   :: wf_smt(:,:,:,:) !! temporary object
    real     :: pot(0:g%imx,I_TRU:I_SMT) !! (0:g%imx) self_consistent potential
    real     :: cmp(0:g%imx) !! (0:g%imx) exchange correlation of the smoothe density
    real     :: rhoaug(0:g%imx) !! (0:g%imx) self_consistent core density
    real     :: vxc(0:g%imx,0:0) !! (0:g%imx) exchange correlation of the smooth density
    real     :: ves(0:g%imx) !! (0:g%imx) electrostatic potential
    real     :: vzero(0:g%imx) !! (0:g%imx) correction potential
    real     :: rho(0:g%imx,ICOR:IVAL,I_TRU:I_SMT) !! (0:g%imx,cor:val,tru:smt) self_consistent core density
    real     :: prj(0:g%imx,0:3,0:ELL_MAX) !! (0:g%imx,enn,ell) projectors
    real     :: Gaussian(0:g%imx) ! with sigma
    real     :: swf(0:g%imx,I_TRU:I_SMT)
    real     :: kincor !! kinetic energy of the core
    real     :: valeig_ennell(0:3,0:ELL_MAX) !! (enn,ell) valence state eigenvalues
!     real     :: valeig_iln(15,1) !! (inl,1) valence state eigenvalues
    real     :: c2(-1:4,IW:IP,0:3,0:ELL_MAX) !! (-1:4,IW:IP,1:3,ell)
    real     :: ct(0:4,0:3)
    real     :: c2cor(0:ELL_MAX)=0. !! (0:3)(I_SMT)
    real     :: c(0:4), cp(0:2) !! potential
    real     :: nrm, nrms, rc, det, dot
    real     :: e, m(2,2), q(I_TRU:I_SMT), pp(0:3,0:3), pp3(0:2,0:2)
    real     :: mat(0:3,0:3,IW:IP,I_TRU:I_SMT,0:ELL_MAX) ! mat(en2,enn,IW:IP,ell)
    real     :: occ_val(-1:+1,0:3,0:ELL_MAX) ! (spin,enn,ell)
    real     :: alleig(-1:+1,1:28)
    real     :: etot
    real     :: alpha
    real     :: Ekin_smt(0:3,0:3), Olap_smt(0:3,0:3), Vkin_smt(0:3,0:3), eval_smt(0:3), c0
    real     :: gf(2,0:g%imx), c_hom, dg, scal
    real     :: rphi_hom(0:g%imx), dg_hom, vg_hom
    real     :: rphi_inh(0:g%imx), dg_inh, vg_inh
    real     :: rphi_tru(0:g%imx), dg_tru, vg_tru
    integer  :: spin, is, nnd(-2:9)
    real, pointer :: r(:)
    integer  :: enn0_ell(0:ELL_MAX)

    real, allocatable   :: radial_data(:,:), rdata_xml(:,:)
    real :: ae_energies(5), ncore, ked(4*ELL_MAX+4,4*ELL_MAX+4)
    integer :: ird, nrd ! number of radial data functions
    type(valence_state), allocatable :: vs(:)
    integer :: ivs, jvs, nvs ! number of valence states exported
    integer :: enn_vs(99)

    logical, parameter :: allow_rescaling = .true.
    integer :: i_switch
    real :: exp_decay_base

    real :: kin_pot_xc_energies(3)
    
    real :: knk
    integer :: nnodes

    character(len=4) :: fun = ' __ ' ! will be set to ' Sy ', where Sy is the chemical symbol

    if(present(g_xml_arg))then
      g_xml => g_xml_arg
    else
      g_xml => g
    end if


    fun(2:3) = symbol

    r => g%r

    id = g .at. .01 ! for display, start plots at a finite radius
!   id = 1 ! show all radial grid points, except the origin
    
    c2 = 0. ! init

    spin = 0 ! calculate spin-paired
    if( any( occ(IDN,:,:) /= occ(IUP,:,:) ) ) then
      ! some orbital has a magnetization
      if( spin_polarized ) then
        spin = 1 ! calculate spin-polarized
        if(o>0) write(o,'(/,3A,/)') sym, fun, 'Magnetization found. Perform a spin-polarized AE calculation.'
      else  ! spin_polarized
        if(o>0) write(o,'(/,4A,/)') sym, fun, WARNING(0), 'Magnetization found, but spin-paired AE calculation!'
      endif ! spin_polarized
    else  ! magnetization
      if(o>0) write(o,'(/,3A,/)') sym, fun, 'no Magnetization found, perform a spin-paired AE calculation.'
    endif ! magnetization
    

    nn(:) = 0
    lmax = -1
    do ell = 0, ubound(nn_in,1)
      nn(ell) = nn_in(ell)
      if( nn(ell) > 0 ) lmax = max( lmax, ell )
    enddo ! ell
    lloc = lmax+1
    lloc = min(lloc, ELL_MAX)

    ! self-consistent all-electron calculation of a spherically symmetric isolated atom
    etot = scf_atom( spin, g, Z, symbol, occ, kxc, sc_pot=pot(:,I_TRU), & ! generates the true potential
!                      potext=extpot, & !! here an external confinement potential can be introduced
                     rhocor=rho(:,ICOR:ICOR,I_TRU), kincor=kincor, alleig=alleig, &
                     energies=kin_pot_xc_energies)

cDBG  is = write2file( 'dmp/pottru', pot(id:,I_TRU), grid=g%r(id:) )
cDBG  is = write2file( 'dmp/rhotrucor', rho(id:,ICOR,I_TRU), grid=g%r(id:) )
                     
    ae_energies(1:3) = kin_pot_xc_energies
    ae_energies(4) = etot
    ae_energies(5) = kincor

    
    if(o>0) write(o,'(3a,2(es10.3," "),a)') sym, fun, 'core density at origin', rho(:1,ICOR,I_TRU), "Bohr^-3"
    ncore = sum( rho(:,ICOR,I_TRU) * g%r2dr )
    if(o>0) write(o,'(3a,f0.9,a)') sym, fun, 'core density has charge ', ncore, " e"

    ! find lmax for Hermite projectors:
    lmax = -1
    do ell = 0, ubound(nn,1)
      if( nn(ell) > 0 ) lmax = max( lmax, ell + 2*(nn(ell) - 1) )
    enddo ! ell
    if(o>0) write(o,'(3A,i0)') sym, fun, 'Hermite projectors up to lmax = ',lmax
    if(o>0) write(o,'(3A,99(i0," "))') sym, fun, 'nn = ',nn
    lmax = min(lmax, ELL_MAX)
    do ell = 0, ubound(nn, 1)
      if (nn(ell) > (lmax - ell)/2 + 1 .and. nn(ell) > 0) then
        if(o>0) write(o,'(3A,9(i0,a))') sym, fun, 'limit number of Hermite projectors for ell = ', &
           ell,' from ',nn(ell),' to ',(lmax - ell)/2 + 1
        nn(ell) = max(0,(lmax - ell)/2 + 1)
      endif
      if (nn(ell) > 4) then
        if(o>0) write(o,'(3A,9(i0,a))') sym, fun, 'hard-coded limit: reduce number of Hermite projectors for ell = ', &
           ell,' from ',nn(ell),' to ',4
        nn(ell) = 4
      endif
    enddo ! ell
    if(o>0) write(o,'(3A,i0)') sym, fun, 'Hermite projectors up to lmax = ',lmax
    if(o>0) write(o,'(3A,99(i0," "))') sym, fun, 'nn = ',nn
    
    !write (*,*) 'nn(ell)', nn(0:)
#ifdef DETAILS
    debug_unit = o ! turn outputs on
#else
    debug_unit = 0 ! turn outputs off
#endif

    rho(:,IVAL,I_TRU:I_SMT) = 0. ! init

if (sigma > 0) then    
    Gaussian(:) = 0
    alpha = -0.5/sigma**2
    i = g .at. (15*sigma) ! large enough
    Gaussian(:i) = exp(alpha*r(:i)**2)

    if(o>0) write(o,'(3a,9(f0.6,a))') sym, fun, 'cutoff radius for compensation  ', rcut(0)*Ang, Ang_
    ircp = g .at. rcut(1) ! potential cutoff radius
    if(o>0) write(o,'(3a,9(f0.6,a))') sym, fun, 'pseudize the local potential at ', (g .at. ircp)*Ang, Ang_
    ircc = g .at. rcut(2) ! cutoff radius for the core density
    if(o>0) write(o,'(3a,9(f0.6,a))') sym, fun, 'pseudize the core density    at ', (g .at. ircc)*Ang, Ang_
    ircm = g .at. rcut(3) ! matching radius
    if(o>0) write(o,'(3a,9(f0.6,a))') sym, fun, 'match true and smooth waves  at ', (g .at. ircm)*Ang, Ang_
else  ! sigma > 0
    ircp = g .at. minval(rcut)
    ircc = ircp
endif ! sigma > 0

      ! pseudize the potential with a parabola
      m = reshape( (/ g%r(ircp+0)**2, -g%r(ircp-1)**2, -1., 1. /), (/2,2/) )
      m = m / (( g%r(ircp+0)+g%r(ircp-1) )*( g%r(ircp+0)-g%r(ircp-1) ))
      cp(0:1) = matmul( pot(ircp-1:ircp+0,I_TRU) - Z/r(ircp-1:ircp+0), m )
      pot(ircp:,I_SMT) = pot(ircp:,I_TRU) - Z/r(ircp:) ! copy true tail
      pot(:ircp,I_SMT) = ( cp(0) + cp(1)*r(:ircp)**2 ) ! replace with a parabola inside rcut
cDBG  is = write2file( 'dmp/potsmt', pot(id:,I_SMT), grid=g%r(id:) )
      if(o>0) write(o,'(3A,9(F0.6,A))') sym, fun, 'pseudize local potential as ', cp(0)*eV, ' + (r/R)^2 * ', cp(1)*r(ircp)**2*eV, eV_
    
    allocate(   wf(0:g%imx,IW:IT,0:3,0:lloc,I_TRU:I_SMT) ) ; wf = 0
    allocate( t_wf(0:g%imx,IW:IT,0:3) )
    
    valeig_ennell = 0
    occ_val = 0. ! init
    do ell = 0, lloc
     
      allocate( wf_smt(0:g%imx,IW:IT,0:nn(ell)-1,0:nn(ell)-1) )
        
      ! find enn0
      enn0 = ell
      do enn = ell+1, 7
        inl = ennell2inl( enn, ell )
        if( occ(ITT,inl,ICOR) > 0. ) enn0 = enn
      enddo ! enn
      if( enn0 > ell ) then
        if(debug_unit>0) write(debug_unit,'(5A,I0,9A)') sym, fun, 'the highest core state for ', ELLCHAR(ell), ' is the ',enn0,ELLCHAR(ell),'-state.'
      elseif (ell < 4) then
        if(debug_unit>0) write(debug_unit,'(5A,I0,9A)') sym, fun, 'no core state for ', ELLCHAR(ell)
      endif
      enn0_ell(ell) = enn0 ! store

      do enn = 0, nn(ell) - 1
        inl = ennell2inl( enn0+enn+1, ell )
        if( occ(ITT,inl,IVAL) > 0. ) then
            valeig_ennell(enn,ell) = alleig(0,inl)
        else if (enn > 0) then
            valeig_ennell(enn,ell) = valeig_ennell(enn - 1,ell) + 1.0 ! one Hartree higher
        endif
        if(debug_unit>0) write(debug_unit,'(3A,I0,2A,f8.3,9(a,f12.6))') sym, fun, &
         'valence state for ',enn0+enn+1,ELLCHAR(ell), ' has occupation',occ(ITT,inl,IVAL),' and energy',valeig_ennell(enn,ell)*eV, eV_
      enddo ! enn    
      
      ! construct analytical projectors: radial basis functions of the spherical harmonic oscillator
      prj(:,:,ell) = 0. ! init projectors
      do enn = 0, nn(ell)-1
        prj(:,enn,ell) = Gaussian*r**(ell + 2*enn) ! prototype, not orthogonalized, not normalized
        do en2 = 0, enn-1 ! loop over all previous projectors in this ell-channel which are already orthogonalized
          dot = sum( prj(:,enn,ell) * prj(:,en2,ell) * g%r2dr )
          prj(:,enn,ell) = prj(:,enn,ell) - dot * prj(:,en2,ell) ! Gram-Schmidt
        enddo ! en2
        nrm = sum( prj(:,enn,ell)**2 * g%r2dr )
        nrm = 1d0/sqrt(nrm)
        prj(:,enn,ell) = prj(:,enn,ell) * nrm ! rescale to normalized
      enddo ! enn

      ! find smooth valence wave functions by linear solve with the projector as inhomogeneiety
      do enn = 0, nn(ell)-1
            E = valeig_ennell(enn,ell)
      
if (sigma > 0) then

            ! homogeneous true solution
            nnd(-2) = integrate_outwards( g, Z, ell, E, pot(:,I_TRU), & ! in
                                          gf, irstop=ircm, dg=dg ) ! derivative at end point
            dg_tru = dg ; vg_tru = gf(1,ircm)
            nnd(-2) = integrate_outwards( g, Z, ell, E, pot(:,I_TRU), gf ) ! solve again without stop criterion
            rphi_tru(1:) = gf(1,1:)/r(1:)
            rphi_tru(0) = 0 ; if (0 == ell) rphi_tru(0) = rphi_tru(1)
            
            wf(0 , :,enn,ell,I_TRU) = 0
            wf(0:,IW,enn,ell,I_TRU) = rphi_tru(0:)
            wf(1:,IT,enn,ell,I_TRU) = (E - (pot(1:,I_TRU) - Z/r(1:)))*rphi_tru(1:) ! kinetic
! cDBG  is = write2file( 'dmp/rphi_tru', rphi_tru, grid=g%r )

!             if (.false.) then
!               nrm = sum( wf(:,IW,enn,ell,I_TRU)**2 * g%r2dr )
!               if(debug_unit>0) write(debug_unit,'(6a,f0.6,2a,f0.9)') sym, fun, & 
!                 'normalize ', ELLCHAR(ell), trim(STARS(enn)), '-state at E = ',E*eV,eV_,'  norm = ',nrm
!               nrm = 1./sqrt(nrm)
!               wf(:,IW:IT,enn,ell,I_TRU) = wf(:,IW:IT,enn,ell,I_TRU)*nrm ! scale all states
!               dg_tru = dg_tru*nrm ; vg_tru = vg_tru*nrm ! scale all states
!             endif
            
            ! homogeneous smooth solution
            nnd(-1) = integrate_outwards( g, 0., ell, E, pot(:,I_SMT), & ! in !! Z=0.
                                          gf, irstop=ircm, dg=dg ) ! derivative at end point
            dg_hom = dg ; vg_hom = gf(1,ircm)
            nnd(-1) = integrate_outwards( g, 0., ell, E, pot(:,I_SMT), gf ) ! solve again without stop criterion
            rphi_hom(1:) = gf(1,1:)/r(1:)
            rphi_hom(0) = 0 ; if (0 == ell) rphi_hom(0) = rphi_hom(1)

            do en2 = 0, nn(ell)-1
            
                nnd(enn) = integrate_outwards_inhomogeneous( g, r*prj(:,en2,ell), ell, E, pot(:,I_SMT), & ! in
                                                            gf, irstop=ircm, dg=dg ) ! derivative at end point
                dg_inh = dg ; vg_inh = gf(1,ircm)
                nnd(enn) = integrate_outwards_inhomogeneous( g, r*prj(:,en2,ell), ell, E, pot(:,I_SMT), gf ) ! solve again without stop criterion
                rphi_inh(1:) = gf(1,1:)/r(1:)
                rphi_inh(0) = 0 ; if (0 == ell) rphi_inh(0) = rphi_inh(1)
              
! cDBG  is = write2file( 'dmp/rphi_inh', rphi_inh, grid=g%r )
! cDBG  is = write2file( 'dmp/rphi_hom', rphi_hom, grid=g%r )

            !! matching:
            !!         \psi(R) * \tilde\psi'\um{inh}(R) - \psi'(R) * \tilde\psi\um{inh}(R)  
            !!    c = ---------------------------------------------------------------------
            !!         \psi(R) * \tilde\psi'\um{hom}(R) - \psi'(R) * \tilde\psi\um{hom}(R)  
            !!
                c_hom =      (vg_tru*dg_inh - dg_tru*vg_inh) & 
                      / & ! ---------------------------------
                             (vg_tru*dg_hom - dg_tru*vg_hom)

                wf_smt(:,IW,en2,enn) = rphi_inh(:) - c_hom*rphi_hom(:)
                wf_smt(:7,IW,en2,enn) = (r(:7)/r(7))**ell * wf_smt(7,IW,en2,enn) ! regularize the first 8 grid points
                wf_smt(:,IT,en2,enn) = prj(:,en2,ell) + (E - pot(:,I_SMT))*wf_smt(:,IW,en2,enn) ! kinetic
    ! cDBG  is = write2file( 'dmp/rphi_smt_ell'+ell+'_enn'+enn+'_enn'+en2, wf_smt(:,IW:IT,en2,enn), grid=g%r )

                !! scale smooth wave to match the true wave not only in log-derivative but also in value
                scal = vg_tru / (vg_inh - c_hom*vg_hom)
                wf_smt(:,:,en2,enn) = wf_smt(:,:,en2,enn) * scal ! scale

            enddo ! en2

            if ((1 == nn(ell))) then ! or old method
                ! method so far based on energy-ordering argument (PASC19 paper)
                wf(:,:,enn,ell,I_SMT) = wf_smt(:,:,enn,enn) ! takes only the diagonal elements
            else
                ! create wf(:,:,enn,ell,I_SMT) as a linear combination of wf_smt(,,:,enn) along the :
                ! such that the kinetic energy <wf_smt(enn)|T|wf_smt(enn)> is minimized
                Ekin_smt = 0
                Olap_smt = 0
                do en2 = 0, nn(ell)-1
                  do en3 = 0, nn(ell)-1
                    ! kinetic energy only w.r.t. the radial coordinate
                    Ekin_smt(en3,en2) = sum( wf_smt(:,IT,en3,enn) * wf_smt(:,IW,en2,enn) * g%r2dr )
                    Vkin_smt(en3,en2) = sum( wf_smt(:,IW,en3,enn) * wf_smt(:,IW,en2,enn) * g%dr )*.5*ell*(ell + 1) ! the centrifugal potential
                    Olap_smt(en3,en2) = sum( wf_smt(:,IW,en3,enn) * wf_smt(:,IW,en2,enn) * g%r2dr )
                  enddo ! en3
                  if(debug_unit>0) write(debug_unit,'(3A,9es18.9)') sym, fun, 'Ekin_smt ', Ekin_smt(0:nn(ell)-1,en2)
                  if(debug_unit>0) write(debug_unit,'(3A,9es18.9)') sym, fun, 'Vkin_smt ', Vkin_smt(0:nn(ell)-1,en2)
                  if(debug_unit>0) write(debug_unit,'(3A,9es18.9)') sym, fun, 'Olap_smt ', Olap_smt(0:nn(ell)-1,en2)
                enddo ! en2
                Ekin_smt = Ekin_smt - Vkin_smt ! subtract the angular part, suggested by Morian Sonnet
                ! symmetrize Ekin_smt
                Ekin_smt = 0.5*(Ekin_smt + transpose(Ekin_smt))
                Olap_smt = 0.5*(Olap_smt + transpose(Olap_smt))
                ! now optimize the kinetic energy under the constraint that the sum of coefficients needs to be 1
!                 if (2 == nn(ell)) then
!                   ! constraint leads to reduction of degrees of freedom by 1 --> c1 = 1 - c0
!                   ! optimize c0*T00*c0 + c0*T01*(1-c0) + (1-c0)*T10*c0 + (1-c0)*T11*(1-c0) w.r.t. c0
!                   !        = c0^2*(T00 - T01 - T10 + T11) + c0*(T01 + T10 - 2*T11) + T11
!                   ! derivative w.r.t. c0: 2*c0*(T00 - T01 - T10 + T11) + (T01 + T10 - 2*T11)
!                   c0 = (Ekin_smt(1,1) - 0.5*Ekin_smt(0,1) - 0.5*Ekin_smt(1,0)) &
!                      / (Ekin_smt(0,0) - Ekin_smt(0,1) - Ekin_smt(1,0) + Ekin_smt(1,1))
!                   if(debug_unit>0) write(debug_unit,'(3A,f14.9,9f18.9)') sym, fun, 'Ekin_opt ', c0, (1-c0)
!                   wf(:,:,enn,ell,I_SMT) = c0*wf_smt(:,:,0,enn) + (1 - c0)*wf_smt(:,:,1,enn)
!                 else
!                   stop 'all_electron: kinetic energy optimiziation of smooth partial waves for nn(ell) > 2 not implemented!'
!                 endif
                is = generalized_eig( Ekin_smt(0:nn(ell)-1,0:nn(ell)-1), &
                                      Olap_smt(0:nn(ell)-1,0:nn(ell)-1), &
                                      eval_smt(0:nn(ell)-1) )
                wf(:,:,enn,ell,I_SMT) = 0
                scal = 1d0/sum(Ekin_smt(0:nn(ell)-1,0)) ! scaling such that the sum is 1
                do en2 = 0, nn(ell)-1
                  wf(:,:,enn,ell,I_SMT) = wf(:,:,enn,ell,I_SMT) + (scal*Ekin_smt(en2,0))*wf_smt(:,:,en2,enn)
                  if(debug_unit>0) write(debug_unit,'(3A,f14.9,9f18.9)') sym, fun, 'Ekin_vec ', scal*Ekin_smt(en2,0)
                enddo ! en2

            endif

! cDBG  is = write2file( 'dmp/wf', wf(:,:,enn,ell,:), grid=g%r(:) )

else  ! sigma > 0

            ! traditional method
            ircm = g .at. rcut(ell)
            knk = shoot( Z, ell, g, pot(0:,I_TRU), E, wf(:,IW,enn,ell,I_TRU), nnodes )
            t_wf(1:,IW,I_TRU) = wf(1:,IW,enn,ell,I_TRU) * g%r(1:)**(-ell-1) ! reduce to an s-like wave function
            t_wf(0 ,IW,I_TRU) = t_wf(1,IW,I_TRU) ! extrapolate zero-th order
            c2(:,:,enn,ell) = 0 ! init
            ! find smooth valence wave functions by pseudization with an even sixth order polynomial (1, r^2, r^4, r^6)
            ist = pseudize_s( g, ircm, tru=t_wf(:,IW,I_TRU), smt=t_wf(:,IW,I_SMT), c2=c2(0:3,IW,enn,ell) )
            wf(0:,IW,enn,ell,I_SMT) = t_wf(:,IW,I_SMT) * g%r(:)**ell ! get back from s-wave to ell-wave function
            wf(0:,IW,enn,ell,I_TRU) = t_wf(:,IW,I_TRU) * g%r(:)**ell ! get back from s-wave to ell-wave function
            c2(0:2,IT,enn,ell) = -0.5*c2(1:3,IW,enn,ell)*(/2,4,6/)*( 2*ell + 1 + (/2,4,6/) )
!             write(*,'(3a,2(i0,a),9f16.9)') sym, fun, 'ell=',ell,' enn=',enn,' matching coeff', c2(0:3,IW,enn,ell)
!             write(*,'(3a,2(i0,a),9i16)')   sym, fun, 'ell=',ell,' enn=',enn,'  coeff factors   ',-(/1,2,3/)*( 2*ell + 1 + (/2,4,6/) )
!             write(*,'(3a,2(i0,a),9f16.9)') sym, fun, 'ell=',ell,' enn=',enn,'  kinetic coeff', c2(0:2,IT,enn,ell)
            wf(1:,IT,enn,ell,I_TRU) = (E - (pot(1:,I_TRU) - Z/g%r(1:))) * wf(1:,IW,enn,ell,I_TRU) ! true kinetic energy
            wf(ircm:,IT,enn,ell,I_SMT) = wf(ircm:,IT,enn,ell,I_TRU) ! copy tail
            c(0:2) = c2(0:2,IT,enn,ell) ! abbrev.
            wf(:ircm,IT,enn,ell,I_SMT) = ( c(0) + g%r(:ircm)**2*c(1) + g%r(:ircm)**4*c(2) ) * g%r(:ircm)**ell ! replace inside rcut
            prj(:,enn,ell) = 0
            prj(:ircm,enn,ell) = wf(:ircm,IT,enn,ell,I_SMT) + (pot(:ircm,I_SMT) - E)*wf(:ircm,IW,enn,ell,I_SMT) ! preliminary projector
            ! analytical shape of the preliminary projector: even eighth order polynomial times r^ell
            c2(0:2,IP,enn,ell) = c2(0:2,IT,enn,ell) ! kinetic
            c2(0:3,IP,enn,ell) = c2(0:3,IP,enn,ell) + (cp(0) - E)*c2(0:3,IW,enn,ell) ! potential offset and energy
            c2(1:4,IP,enn,ell) = c2(1:4,IP,enn,ell) +  cp(1)     *c2(0:3,IW,enn,ell) ! potential curvature
!             write(*,'(3a,2(i0,a),9f16.9)') sym, fun, 'ell=',ell,' enn=',enn,' projector coeff', c2(0:4,IP,enn,ell)
            ! we can verify that the polynomial projectors exhibit at least a single zero at rcut(ell)
            ! some even have a quadratic zero (desirable for a smooth projector)

!             is = write2file( 'dmp/traditional_wf_ell'+ell+'_enn'+enn, wf(:,:,enn,ell,:), grid=g%r(:) )

endif ! sigma > 0

!             if (0 == enn) then
            if (.true.) then
              ! create smooth valence density from lowest valence state, if occupied
              inl = ennell2inl( enn0+enn+1, ell )
      !       if(debug_unit>0) write(debug_unit,'(3a,9(i0,a))') sym, fun, 'inl=',inl,' for enn=',enn0+enn+1,' ell=',ell
              if (inl < 1) write(*,'(4a,9(i0,a))') sym, fun, WARNING(0), 'inl == 0 for enn=',enn0+enn+1,' ell=',ell
              if (inl <= ubound(occ,2)) then
                if( any( occ(:,inl,IVAL) > 0. ) ) then

                  occ_val(:,enn,ell) = min( max( 0., occ(:,inl,IVAL) ), 2.*ell+1. ) ! store occupation numbers for the PAW data file
                  occ_val(0,enn,ell) = occ_val(+1,enn,ell) + occ_val(-1,enn,ell) ! up+dn spin

                  nrm = dot_product( wf(:,IW,enn,ell,I_TRU)**2, g%r2dr )
                  
                  ! check the state localization
                  i = ircc
                  nrms = dot_product( wf(:i,IW,enn,ell,I_TRU)**2, g%r2dr(:i) )
                  if( nrms > .90 * nrm ) then
                    if(o>0) write(o,'(6A,F0.3,9A)') sym, fun, WARNING(0), 'true ', &
                      inl2string(inl), '-state is localized ', nrms/(.01*nrm), ' % inside the sphere ==> core state?'
                  else  !
                    if(debug_unit>0) write(debug_unit,'(5A,F0.3,9A)') sym, fun, 'true ', &
                      inl2string(inl), '-state is localized ', nrms/(.01*nrm), ' % in the sphere'
                  endif ! state is located mostly inside the sphere

                  if( nrm <= 0. ) stop 'calc_atom: enn=0 true partial wave cannot be normalized.'
                  nrm = ( occ_val(ITT,enn,ell) )/nrm

                  if(debug_unit>0) write(debug_unit,'(5A,9(F0.3,A))') sym, fun, &
                    'construct density with valence state ', inl2string(inl), &
                    ' occ = ', occ_val(+1,enn,ell),' + ',occ_val(-1,enn,ell)
                  do iq = I_TRU, I_SMT
                    rho(:,IVAL,iq) = rho(:,IVAL,iq) + nrm * wf(:,IW,enn,ell,iq)**2
                    if(debug_unit>0) write(debug_unit,'(7A,9(F0.6,A))') sym, fun, &
                        'construct ',tru2char(iq),' density with valence state ', inl2string(inl), &
                        '  inside ', r(i), ' Bohr: ', nrm * dot_product( wf(:i,IW,enn,ell,iq)**2, g%r2dr(:i) ) ,' e'
                    if(debug_unit>0) write(debug_unit,'(7A,9(F0.6,A))') sym, fun, &
                        'construct ',tru2char(iq),' density with valence state ', inl2string(inl), &
                        ' outside ', r(i), ' Bohr: ', nrm * dot_product( wf(i+1:,IW,enn,ell,iq)**2, g%r2dr(i+1:) ) ,' e'
                  enddo ! iq
                  
                endif ! valence state occupied
              endif ! inl in range
            endif ! 0==enn
            
            if (allow_rescaling) then
              nrm = dot_product( wf(:,IW,enn,ell,I_TRU)**2, g%r2dr )
              nrm = 1.0_8/sqrt(nrm)
              wf(:,:,enn,ell,:) = wf(:,:,enn,ell,:)*nrm ! normalize eigenstates for GPAW and juRS
              c2(:,IW:IT,enn,ell) = c2(:,IW:IT,enn,ell)*nrm
            endif
            
      enddo ! enn

      
      if (allow_rescaling) then
        ! LU decomposition of pp
        
        do enn = 0, nn(ell)-1
          pp(enn,enn) = sum( wf(:,IW,enn,ell,I_SMT) * prj(:,enn,ell) * g%r2dr )
          nrm = 1.0_8/pp(enn,enn)
          if ( occ_val(0,enn,ell) > 0 ) then
            ! rescale projector to keep the normalization of the partial waves
            prj(:,enn,ell) = prj(:,enn,ell)*nrm
            c2(:,IP,enn,ell) = c2(:,IP,enn,ell)*nrm
          else
            ! rescale partial waves to keep the normalization of the projectors
            wf(:,:,enn,ell,:) = wf(:,:,enn,ell,:)*nrm
          endif
          ! Gram-Schmidt orthogonalize all higher partial waves in the dual space
          do en2 = enn+1, nn(ell)-1
            pp(en2,enn) = sum( wf(:,IW,en2,ell,I_SMT) * prj(:,enn,ell) * g%r2dr )
            wf(:,:,en2,ell,:) = wf(:,:,en2,ell,:) - pp(en2,enn) * wf(:,:,enn,ell,:)
            c2(:,IW:IT,en2,ell) = c2(:,IW:IT,en2,ell) - pp(en2,enn) * c2(:,IW:IT,enn,ell)
          enddo ! en2
          ! Gram-Schmidt orthogonalize all higher projectors in the dual space
          do en2 = enn+1, nn(ell)-1
            pp(enn,en2) = sum( wf(:,IW,enn,ell,I_SMT) * prj(:,en2,ell) * g%r2dr )
            prj(:,en2,ell) = prj(:,en2,ell) - pp(enn,en2) * prj(:,enn,ell)
            c2(:,IP,en2,ell) = c2(:,IP,en2,ell) - pp(enn,en2) * c2(:,IP,enn,ell)
          enddo ! en2

          ! rescale re-orthogonalized true partial waves to unity
          nrm = sqrt(sum( wf(:,IW,enn,ell,I_TRU) * wf(:,IW,enn,ell,I_TRU) * g%r2dr ))
          ! rescale projectors
          prj(:,enn,ell) = prj(:,enn,ell)*nrm
          c2(:,IP,enn,ell) = c2(:,IP,enn,ell)*nrm
          ! rescale partial waves with the inverse
          nrm = 1.d0/nrm
          wf(:,:,enn,ell,:) = wf(:,:,enn,ell,:)*nrm
          c2(:,IW:IT,enn,ell) = c2(:,IW:IT,enn,ell)*nrm
          
        enddo ! enn

if (sigma > 0) then
else  ! sigma > 0
          do enn = 0, nn(ell)-1 ! these are the polynomial coefficients(0:4) of the projectors for r-powers l+0,l+2,l+4,l+6,l+8
!            write(*,'(3a,2(i0,a),9f16.9)') sym, fun, 'ell=',ell,' enn=',enn,' new proj  coeff', c2(0:4,IP,enn,ell)
          enddo ! enn           ! we can verify that the polynomial projectors exhibit at least a single zero at rcut(ell)
endif ! sigma > 0
        
      else  ! allow_rescaling
      
      
        ! orthogonalize the smooth partial waves and projector with effect to the true partial waves
        pp = 0 ; do enn = 0, 3 ; pp(enn,enn) = 1 ; enddo ! enn init as unit matrix
        
        do enn = 0, nn(ell)-1
          do en2 = 0, nn(ell)-1
            pp(en2,enn) = sum( wf(:,IW,enn,ell,I_SMT) * prj(:,en2,ell) * g%r2dr )
          enddo ! en2
        enddo ! enn
      
        ! inversion of pp
        
        if (nn(ell) > 3) stop __LINE__ ! not implemented
        pp3 = pp(0:2,0:2)
        ist = invert(pp3)
        if (ist /= 0) then
          write(*,'(4a,i0,a,9(" ",f0.6))') sym, fun, WARNING(0), 'failed to invert for ell=',ell," matrix =",pp(0:2,0:2)
          stop __LINE__
        endif
        pp(0:2,0:2) = pp3
        
        do iq = I_TRU, I_SMT
          t_wf = wf(:,:,:,ell,iq) ! copy into some temporary
          do enn = 0, nn(ell)-1
            wf(:,:,enn,ell,iq) = 0 ! init
            do en2 = 0, nn(ell)-1
              wf(:,:,enn,ell,iq) = wf(:,:,enn,ell,iq) + pp(en2,enn) * t_wf(:,:,en2)
              ! warning: polynomial coefficients c2 are not updated !
            enddo ! en2
          enddo ! enn
        enddo ! iq
      
      endif ! allow_rescaling

! #ifdef DEBUG      
      !! check again
      do enn = 0, nn(ell)-1
        do en2 = 0, nn(ell)-1
          pp(en2,enn) = sum( wf(:,IW,enn,ell,I_SMT) * prj(:,en2,ell) * g%r2dr )
        enddo ! en2
      enddo ! enn
      if(o>0) write(o,"(3a,i0,a,99(f10.6))") sym, fun, " ell=",ell," check duality:",pp(0:nn(ell)-1,0:nn(ell)-1)
! #endif


      if(debug_unit>0) write(debug_unit,'(9A)') sym, fun

      deallocate( wf_smt, stat=is )
      
    enddo ! ell
cDBG  is = write2file( 'dmp/rhotruval', rho(id:,IVAL,I_TRU), grid=g%r(id:) )
cDBG  is = write2file( 'dmp/rhosmtval', rho(id:,IVAL,I_SMT), grid=g%r(id:) )
    is = write2file( 'dmp/smtwf0', wf(:,:,0:nn(0),0,I_SMT), grid=g%r(:) )
    is = write2file( 'dmp/smtwf1', wf(:,:,0:nn(1),1,I_SMT), grid=g%r(:) )

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!     if(o>0) write(o,'(3A,4F10.6,9A)') sym, fun, 'Augmentation Radius', r(irc(0:3))*Ang, Ang_

    ! pseudize the core density
    if( rho(ircc,ICOR,I_TRU)/( rho(ircc,IVAL,I_TRU) + 1E-20 ) < 1E-9 ) then

      if(o>0) write(o,'(9A)') sym, fun, 'core charge is localized inside Rc, smooth core density vanishes.'
      rho(:,ICOR,I_SMT) = 0. ! set to zero

    else  ! rho_cor(Rc)/rho_val(Rc) << 1

#ifdef  TRY
      ! new: pseudize rhoc with r^2-behaviour @ r=0
      !!! seems like this leads to more structure in vBAR
      if(o>0) write(o,'(9A)') sym, fun, 'pseudize rhoc with r^2-behaviour @ r=0'
      swf(1:,I_TRU) = rho(1:,ICOR,I_TRU)/g%r(1:)**2 ; swf(0,:) = 0. ! reduce by r^2
      ist = pseudize_s( g, ircc, tru=swf(:,I_TRU), smt=swf(:,I_SMT), c2=c2cor(1:4) )
      rho(:,ICOR,I_SMT) = swf(:,I_SMT)*g%r**2 ! get back r^2 factor
#else
      ! pseudize rhoc with a r^0-behaviour @ r=0
      if(o>0) write(o,'(9A)') sym, fun, 'pseudize rhoc with r^0-behaviour @ r=0'
      ist = pseudize_s( g, ircc, tru=rho(:,ICOR,I_TRU), smt=rho(:,ICOR,I_SMT), c2=c2cor(0:3) )
#endif

cDBG  do iq = I_TRU, I_SMT
cDBG    nrm = dot_product( rho(:,ICOR,iq), g%r2dr )
cDBG    nrms = dot_product( rho(:ircc,ICOR,iq), g%r2dr(:ircc) )
cDBG    if(debug_unit>0) write(debug_unit,'(5A,9(F12.6,A))') sym, fun, &
cDBG        'total ', tru2char(iq), ' core charge =', nrm, ' e (', nrms,' inside,', nrm - nrms,' outside Rc)'
cDBG  enddo ! iq

    endif ! rho_cor(Rc)/rho_val(Rc) << 1

    rho(0,:,:) = rho(1,:,:) ! extrapolate origin 0th order
cDBG  is = write2file( 'dmp/rhosmtcor', rho(id:,ICOR,I_SMT), grid=g%r(id:) )
cDBG  is = write2file( 'dmp/rhosmttot', rho(id:,ICOR,I_SMT) + rho(id:,IVAL,I_SMT), grid=g%r(id:) )
    do iq = I_TRU, I_SMT
      q(iq) = dot_product( rho(:,ICOR,iq) + rho(:,IVAL,iq), g%r2dr )
cDBG  if(debug_unit>0) write(debug_unit,'(4A,F16.6,9A)') sym, fun, 'total charge: ', tru2char(iq), q(iq), ' e'
    enddo ! iq

    ! create compensation charge
    if (sigma > 0) then
      alpha = 10.d0/rcut(0)**2
    else
      alpha = 10.d0/minval(rcut)**2
    endif
    cmp = exp(-alpha * r**2) ! Gaussian compensation charge
    where (alpha * r**2 > 200) cmp = 0
    nrm = dot_product( cmp, g%r2dr )
    cmp(:) = cmp(:)/nrm ! normalize the compensation charge to a monopole moment of unity
!!!! Solution in GPAW:
!         x = r / rcutcomp
!         gaussian = np.zeros(N)
!         self.gamma = gamma = 10.0
!         gaussian[:gmax] = np.exp(-gamma * x[:gmax]**2)
!         gt = 4 * (gamma / rcutcomp**2)**1.5 / sqrt(pi) * gaussian
!         t('Shape function alpha=%.3f' % (gamma / rcutcomp**2))
!         norm = np.dot(gt, dv) # dv = r**2 * dr
!         #  print norm, norm-1
!         assert abs(norm - 1) < 1e-2
!         gt /= norm
!!!! GPAW exports rcutcomp/sqrt(gamma) to the .xml file

    rhoaug(:) = rho(:,ICOR,I_SMT) + rho(:,IVAL,I_SMT) + ( q(I_TRU) - Z - q(I_SMT) )*cmp(:)

    ! check norm of the augmented density
    nrm = dot_product( rhoaug, g%r2dr )
cDBG  if(debug_unit>0) write(debug_unit,'(4A,F16.6,A,es9.2,a)') sym, fun, 'total charge: ', 'aug', nrm, ' e = ', nrm, ' e'
    if( abs( nrm ) > 1E-9 .and. o>0) write(o,'(4A,F0.9,9A)') sym, fun, WARNING(0), 'rhoaug has, norm = ',nrm,' e!'

cDBG  is = write2file( 'dmp/cmp', cmp, grid=g%r )
cDBG  is = write2file( 'dmp/rhocmp', ( -Z+q(I_TRU)-q(I_SMT) )*cmp, grid=g%r )
cDBG  is = write2file( 'dmp/rhoaug', rhoaug, g%r )

    ! generate the exchange correlation potential from the smooth total density ! ves will be overwritten in the next step
    ist = xc_functional( spin=0, fpirho=rho(:,ICOR:ICOR,I_SMT)+rho(:,IVAL:IVAL,I_SMT), Vxc=vxc, Exc=ves, key=kxc, rgd=g%d, rgr=g%r, rgdrdi=g%dr ) ; ves = 0. ! dump
    ! generate the Hartee potential from the density
    ist = Hartree_potential( g, rho=rhoaug, vH=ves )

    is = write2file( 'dmp/smtves', ves, grid=g%r )
    is = write2file( 'dmp/smtvxc', vxc, grid=g%r )
    is = write2file( 'dmp/smtveff', vxc(:,0) + ves, grid=g%r )

    vzero = 0.
    ircp = g .at. 2*rcut(1) ! allow 2 times the cutoff radius
    vzero(:ircp) = pot(:ircp,I_SMT) - ves(:ircp) - vxc(:ircp,0)

cDBG  is = write2file( 'dmp/smtpot', pot(:,I_SMT), grid=g%r )
cDBG  is = write2file( 'dmp/pot', pot(id:,:), grid=g%r(id:) )
cDBG  is = write2file( 'dmp/vBAR', vzero(9:), grid=g%r(9:) )

    ! prepare the export
    nvs = sum(nn) ! number of valence states for export
    nrd = 6 + 3*nvs
    allocate( vs(nvs), radial_data(0:g%imx,nrd) )
    ivs = 0
    ird = 6 ; radial_data = 0
    do ell = 0, ubound(nn, 1)
      if (sigma <= 0) ircm = g .at. rcut(ell)
      do enn = 0, nn(ell)-1
        ivs = ivs + 1
        vs(ivs)%l = ell
        vs(ivs)%n = enn0_ell(ell) + enn + 1 ! principal quantum number
        vs(ivs)%f = occ_val(0,enn,ell) ! 0:spin integrated
        vs(ivs)%rc = rcut(ell) ; if (sigma > 0) vs(ivs)%rc = 9*sigma

        vs(ivs)%e = valeig_ennell(enn,ell)
        write(unit=vs(ivs)%id, fmt="(i0,a)") vs(ivs)%n, ellchar(ell)

        ird = ird + 1
        vs(ivs)%ae = ird
        radial_data(:,ird) = wf(:,IW,enn,ell,I_TRU) ! true wave function
        
        ird = ird + 1
        vs(ivs)%ps = ird
        radial_data(:,ird) = wf(:,IW,enn,ell,I_SMT) ! pseudo wave function
        
        ird = ird + 1
        vs(ivs)%pr = ird
        radial_data(:ircm,ird) = prj(:ircm,enn,ell) ! projector, only up to the matching radius
        
        enn_vs(ivs) = enn
      enddo ! enn
    enddo ! ell
    radial_data(:,1) = rho(:,ICOR,I_TRU)/sqrt4pi ! true core density
    radial_data(0,1) = radial_data(1,1) + (g_xml%r(0) - g_xml%r(1))*(radial_data(2,1) - radial_data(1,1))/(g_xml%r(2) - g_xml%r(1)) ! linear extrapolation at origin
    radial_data(:,2) = rho(:,ICOR,I_SMT)/sqrt4pi ! pseudo core density
    radial_data(:,3) = rho(:,IVAL,I_SMT)/sqrt4pi ! pseudo valence density
    if (sigma <= 0) ircm = g .at. maxval(rcut)
    radial_data(:ircm,4) = vzero(:ircm)*sqrt4pi ! zero potential needs /sqrt4pi or *sqrt4pi ?

    allocate(rdata_xml(0:g_xml%imx,nrd))
    if (present(g_xml_arg)) then
        rdata_xml = 0
        do ird = 1, nrd
          !! exponential decay used for extrapolation, based on last two values
          exp_decay_base = (radial_data(g%imx,ird)/radial_data(g%imx-1,ird))**(1.d0/(r(g%imx) - r(g%imx-1)))
          if (exp_decay_base > 1.d0 .or. isnan(exp_decay_base) .or. radial_data(g%imx-1,ird) == 0) then ! something weird is happening, do not let function explode during extrapolation.
            exp_decay_base = 0.d0
          endif
          !! linear interpolation to get from g to g_xml (radial grids)
          i_switch = g_xml%imx+1
          do i = 0, g_xml%imx
            rc = g_xml%r(i)
            if ( rc > r(g%imx)) then
              i_switch = i
              exit
            endif
            ircc = min(g .at. rc, g%imx - 1)
            rdata_xml(i,ird) = radial_data(ircc,ird) + &
                               ( radial_data(ircc+1,ird) - radial_data(ircc,ird) )* &
                               ( rc - r(ircc) )/( r(ircc+1) - r(ircc) )
          enddo ! i linear part
          !! exponential extrapolation
          do i = i_switch, g_xml%imx
            rc = g_xml%r(i)
            rdata_xml(i,ird) = radial_data(g%imx,ird)*exp_decay_base**(rc-r(g%imx))
          enddo ! i
        enddo ! ird
    else
        rdata_xml = radial_data
    endif

    !! kinetic energy deficit matrix
    ked = 0
    do ivs = 1, nvs
      ell = vs(ivs)%l
      enn = enn_vs(ivs)
      do jvs = 1, nvs
        if (ell == vs(jvs)%l) then
          en2 = enn_vs(jvs)
          ked(ivs,jvs) = sum( wf(:,IT,enn,ell,I_TRU) * wf(:,IW,en2,ell,I_TRU) * g%r2dr ) &
                       - sum( wf(:,IT,enn,ell,I_SMT) * wf(:,IW,en2,ell,I_SMT) * g%r2dr )
        endif
      enddo ! jvs
    enddo ! ivs

    ked = 0.5*(ked + transpose(ked)) ! symmetrize

    ncore = nint(sum(occ(ITT,:,ICOR)))
    ist = write_paw_xml(iZ, [ncore, Z - ncore], config, ["LDA ", "PZ81"], &
                 ae_energies, vs, rcut(0)/sqrt(10.), rdata_xml, ked(:nvs,:nvs), g_xml)

  endfunction ! calc_atom_sigma

  

  status_t function scattering_test( enpara, g, Z, symbol, lmax, nn, vtru, vsmt, rwf, dkin, chdm, E_window, Rcut ) result( ios )
  use type_rgrid, only: rgrid, operator(.at.), operator(.at.)
  use radial_integrator, only: integrate_outwards
  use radial_integrator, only: integrate_outwards_inhomogeneous
  use configuration, only: WARNING
  use unitsystem, only: eV, eV_, Ang, Ang_
  use LAPACK, only: Solve_Ax_b
! cDBG  use toolbox, only: cat
    type(rgrid), target, intent(in) :: g
    real, intent(in)                :: Z  !! atomic number
    character(len=2), intent(in)    :: symbol
    qn_ELL_t, intent(in)            :: lmax
    integer, intent(in)             :: nn(0:) ! numbers of projectors per ell
    real, intent(in)                :: vtru(0:) ! true potential without -Z/r part
    real, intent(in)                :: vsmt(0:) ! smooth local potential
    real, intent(in)                :: rwf(0:,1:,1:,I_TRU:) ! radial wave functions
    real, intent(in)                :: dkin(3,3,0:lmax) ! kinetic energy deficit
    real, intent(in)                :: chdm(3,3,0:lmax) ! charge deficit
    real, intent(in)                :: E_window(-1:+1) ! start energy, 0:spacing, end energy
    real, intent(in)                :: Rcut
    real, intent(in)                :: enpara(3, 0:6)

    ! character(len=*), parameter     :: fun = ' scattering_test: '
    integer, parameter              :: IW = 0 !! wave function
    integer, parameter              :: IT = 1 !! kinetic energy operator times wave function
    integer, parameter              :: IP = 2 !! projector of that wave function
    character(len=*), parameter     :: STARS(1:4) = (/'  ','* ','**','? '/) ! enn=1: no star, enn=2: *, enn=3: **
    integer, save                   :: u = 0
!     real, parameter                 :: INV = 0.25 ! show inverse of logder, where logder is too large
    string_t                        :: logderfile
    real                            :: rprj(0:g%imx,0:3,0:lmax) ! smooth projector functions (ir,enn,ell)
    real                            :: Vcor(0:g%imx) ! core Potential
    integer                         :: nnodes, nnd(0:3), irstop, iq, nnan=0
    integer                         :: ie, ie_window(-1:+1), n, k, j, i
    qn_ELL_t                        :: ell
    qn_ENN_t                        :: en2, enn
    qn_iLN_t                        :: iln, iln0, iln1, iln2
    real                            :: dE, E!, dEmin, dEmax
    real                            :: gf(1:2,0:g%imx)
    real                            :: dg
    real                            :: logder(I_TRU:I_SMT,0:lmax+2), last_logder(I_TRU:I_SMT,0:lmax+2)
    real                            :: valder(-1:3,0:1) ! 0: value, 1: 1st derivative
    real                            :: aHm(3,3,0:lmax) ! atomic Hamiltonian matrix
    real                            :: aSm(3,3,0:lmax) ! atomic overlap matrix
    real                            :: matenpara(3,3,0:lmax) 
    real                            :: gfp(0:3,0:3)
    real                            :: mat(0:3,0:3)
    real                            :: x(0:3), b(0:3)

    character(len=4)                :: fun = ' __ ' ! == ' Sy ', where Sy is the chemical symbol
    fun(2:3) = symbol

    irstop = g .at. ( Rcut*1.5 )
    if(o>0) write(o,'(3A,F0.6,9A)') sym, fun, 'Logarithmic Derivatives at R = ', ( g .at. irstop )*Ang, Ang_

    gf = 0.

    dE = E_window(0) ! energy spacing
    ! adopt the energy spacing to the format F10.6
    dE = nint( dE * 1E6 * eV ) / ( 1E6 * eV )
    ie_window(-1) = floor  ( E_window(-1) / dE )
    ie_window(+1) = ceiling( E_window(+1) / dE )


    write(unit=logderfile,fmt='(9A)') trim(symbol),'_logder'
    u = 29
    open(unit=u,file=logderfile,action='write',status='unknown',IOstat=ios)
    if( ios /= 0 ) then
      if(o>0) write(o,'(3A,I2,A,F10.6,9A)') sym, fun, WARNING(0), 'opening file "', trim(logderfile), '" failed!'
      u = 0 ! no write
    endif
    if(u>0) write(u,'(3A,9I3)') '# Logarithmic Derivatives (in au) for ', symbol, '   nn =', nn
    if(u>0) write(u,'(A,F0.6,9A)')'# taken at R = ', ( g .at. irstop )*Ang, Ang_
    if(u>0) write(u,'(9A)') '# Energy in', eV_, ',     tru_s, smt_s,   tru_p, smt_p, ...'

    rprj = 0.
    iln = 0
    do ell = 0, lmax
      rprj(:,0,ell) = 0. ! prj(:,0,:) will always stay zero
      do enn = 1, nn(ell)
        iln = iln+1
        rprj(:,enn,ell) = rwf(:,iln,1,I_PRJ)*g%r**(ell+1)
      enddo ! enn
    enddo ! ell

    Vcor(0) = 0. ; Vcor(1:) = -Z/g%r(1:)
    Vcor(0) = Vcor(1)
    iln0 = 0
    do ell = 0, lmax
      aHm(:,:,ell) = 0. ! init atomic Hamiltonian matrix
      aSm(:,:,ell) = 0. ! init atomic overlap matrix
      n = nn(ell) ! abbreviation
      do enn = 1, n
        iln1 = iln0 + enn
        do en2 = 1, n
          iln2 = iln0 + en2
          ! construct the atomic non-local Hamiltonian elements
          aHm(en2,enn,ell) = 0.5*( dkin(en2,enn,ell)+dkin(enn,en2,ell) ) &
            + sum( rwf(:,iln2,1,I_TRU) * ( vtru + vcor ) * rwf(:,iln1,1,I_TRU) * g%dr(0:) ) &
            - sum( rwf(:,iln2,1,I_SMT) * (    vsmt     ) * rwf(:,iln1,1,I_SMT) * g%dr(0:) )
          aSm(en2,enn,ell) = sum( rwf(:,iln2,1,I_TRU) * rwf(:,iln1,1,I_TRU) * g%dr(0:) ) &
                           - sum( rwf(:,iln2,1,I_SMT) * rwf(:,iln1,1,I_SMT) * g%dr(0:) )
	  
	  !aSm(en2,enn,ell) = chdm(en2, enn, ell)
        enddo ! en2
      enddo ! enn
  
      !write (*,*) 'enpara ', enpara(:, ell)*eV
      !matenpara = 0.
      !do enn = 1, n
	!matenpara(enn, enn, ell) = enpara(enn, ell)
      !enddo 

      !matenpara(1:n, 1:n, ell) = matmul (matenpara(1:n, 1:n, ell), aSm(1:n,1:n,ell))
      !aHm(1:n, 1:n, ell) = aHm(1:n, 1:n, ell) + matenpara(1:n, 1:n, ell)
      !write (*,*) 'matenpara ', matenpara(:,:, ell)*eV
      
    
      
      !write(6,'(3A,I2,A,99F16.6)') sym, fun, 'ell =', ell, ' aHm', aHm(1:n,1:n,ell)*eV
      !write(6,'(3A,I2,A,99F16.6)') sym, fun, 'ell =', ell, ' aSm', aSm(1:n,1:n,ell)
      !write(6,'(3A,I2,A,99F16.6)') sym, fun, 'ell =', ell, ' chdm', chdm(1:n, 1:n, ell)
      !aHm(:,:,ell) = 0. ! init atomic Hamiltonian matrix
      !aSm(:,:,ell) = 0. ! init atomic overlap matrix
#ifdef FULL_DEBUG
cDBG  if(o>0) write(o,'(3A,I2,A,99F16.6)') sym, fun, 'ell =', ell, ' aHm', aHm(1:n,1:n,ell)
cDBG  if(o>0) write(o,'(3A,I2,A,99F16.6)') sym, fun, 'ell =', ell, ' aSm', aSm(1:n,1:n,ell)
cDBG  if( any( abs( aHm(:,:,ell) - transpose(aHm(:,:,ell)) ) > 1.E-12 ) ) stop 'AE: aHm not symmetric!'
#endif
      iln0 = iln0 + nn(ell) ! forward offset
    enddo ! ell
! cDBG  is = write2file( 'dmp/tru', vtru+vcor, grid=g%r )
! cDBG  is = write2file( 'dmp/smt', vsmt     , grid=g%r )


cDBG  if(o>0) write(o,'(3A,2F8.3,A,I0,A,F0.6,9A)') sym, fun, 'sample energies from [', &
cDBG    dE*ie_window(-1:+1:2)*eV, ' ] in ', ie_window(+1)+1-ie_window(-1), ' steps of ', dE*eV, eV_

    last_logder = +9E9 ! init as large value since logder is a falling function

    do ie = ie_window(-1), ie_window(+1), 1
      logder = 0. ! init
      E = ie*dE
#ifdef FULL_DEBUG
! cDBG  if(o>0) write(o,'(3A,F16.6,A)') sym, fun, 'E =', E*eV, eV_
#endif
      
      do ell = 0, lmax+2

        ! for the local potential
        nnodes = integrate_outwards( g, Z, ell, E, Vtru, & ! in
                                     gf, irstop=irstop, &
                                     dg=dg ) ! derivative at end point
#ifdef NaN_SEARCH
  if( any( gf /= gf ) ) stop 'AE scattering_test: NaN in GF (homogeneous)'
#endif
        logder(I_TRU,ell) = dg/gf(1,irstop)
#ifdef FULL_DEBUG
! cDBG    if(o>0) write(o,'(3A,I2,A,2ES16.6)') sym, fun, 'ell =', ell, ' (tru) valder =', gf(1,irstop), dg
#endif

#ifdef FULL_DEBUG
! cDBG    is = write2file( 'dmp/tru_'+ellchar(ell)+abs(ie), gf(1,9:)/g%r(9:), grid=g%r(9:) )
#endif


        ! now for the non-local potential
        gfp = 0.
        n = 0 ; if( ell <= lmax ) n = nn(ell) ! abbreviation

        do i = 0, n ! caution, i running from 0, because 0 is the homogeneous solution

          if( i > 0 ) then
            nnd(i) = integrate_outwards_inhomogeneous( &
                                     g, rprj(:,i,ell), ell, E, Vsmt, & ! in
                                     gf, irstop=irstop, &
                                     dg=dg ) ! derivative at end point
#ifdef FULL_DEBUG
! cDBG        call test_solution( g, 0., ell, Vsmt, E, s=gf(1,:), p=rprj(:,i,ell) ) ! test and stop
! cDBG        is = write2file( 'dmp/inh_'+ellchar(ell)+abs(ie)+'_'+i, gf(1,9:)/g%r(9:), grid=g%r(9:) )
#endif
          else  ! i > 0
            ! homogeneous solution
            nnd(i) = integrate_outwards( g, 0., ell, E, Vsmt, & ! in !! Z=0.
                                     gf, irstop=irstop, &
                                     dg=dg ) ! derivative at end point
#ifdef FULL_DEBUG
! cDBG        call test_solution( g, 0., ell, Vsmt, E, s=gf(1,:), p=rprj(:,i,ell) )
! cDBG        is = write2file( 'dmp/hom_'+ellchar(ell)+abs(ie), gf(1,9:)/g%r(9:), grid=g%r(9:) )
#endif
          endif ! i > 0

#ifdef NaN_SEARCH
  if( any( gf /= gf ) ) stop 'AE scattering_test: NaN in GF (inhomogeneous)'
#endif
          valder(i,1) = dg
          valder(i,0) = gf(1,irstop)
#ifdef FULL_DEBUG
! cDBG      if(o>0) write(o,'(3A,2(I2,A),99ES16.6)') sym, fun, 'ell =', ell, ' i =', i, ' valder =', valder(i,:)
#endif
          ! find the scalar product of the inhomogeneous solution with the projectors
          do j = 1, n
            gfp(i,j) = sum( gf(1,:) * rprj(:,j,ell) * g%dr )
          enddo ! j

        enddo ! i

        mat = 0.
        do i = 1, n
          do k = 0, n
            do j = 1, n
              mat(i,k) = mat(i,k) + gfp(k,j) * ( aHm(j,i,ell) - E * aSm(j,i,ell) )
            enddo ! j
          enddo ! k
          mat(i,i) = mat(i,i) + 1.
        enddo ! i
        i = 0
          mat(i,i) = mat(i,i) + 1.
#ifdef FULL_DEBUG
! cDBG    do i = 0, n
! cDBG      if(o>0) write(o,'(3A,I2,A,99F16.6)') sym, fun, 'ell =', ell, ' mat', mat(0:n,i)
! cDBG    enddo ! i
! cDBG    if(o>0) write(o,'(3A,I2,A,99F16.6)') sym, fun, ''
! cDBG    if(o>0) write(o,'(3A,I2,A,99F16.6)') sym, fun, 'ell =', ell, ' gfp', gfp(0:n,1:n)
#endif
        b = 0. ; b(0) = 1. ! arbitrary scaling here, because it enters nominator and denominator
        ios = Solve_Ax_b( mat(0:n,0:n), b(0:n), x(0:n) )
        if( ios /= 0 ) then
          if(6>0) write(6,'(3A,I0,A,F0.6,9A)') sym, fun, 'for ell = ', ell, ' E = ', E*eV, eV_, ' inversion failed!'
          x = 0 ; x(0) = 1. ! show the scattering of the local potential only
        endif
	!!write(*,*) 'got x ', x(0:n),' for ell ', ell
        valder(-1,0) = sum( x(0:n)*valder(0:n,0) )
        valder(-1,1) = sum( x(0:n)*valder(0:n,1) )
#ifdef FULL_DEBUG
! cDBG    if(o>0) write(o,'(3A,I2,A,2ES16.6,A,F10.6,9ES10.2)') sym, fun, 'ell =', ell, ' (smt) valder =', valder(-1,:), ' x=', x(0:n)
! cDBG    if(o>0) write(o,'(3A,I2,A,9F10.6)') sym, fun, 'ell =', ell, ' M*x=', matmul(mat(0:n,0:n),x(0:n))
#endif
        logder(I_SMT,ell) = valder(-1,1)/valder(-1,0)



      enddo ! ell
     

      ! write to file
      if( all( logder == logder ) ) then

        ! if(u>0) write(u,'(F16.6,99(ES18.6,ES16.6))') E*eV, logder
        ! use the inverse of tangens because atan(x) never gets bigger than Pi/2 (absolute)
        if(u>0) write(u,'(F16.6,88(F14.8,F12.8))') E*eV, atan(logder)
        do ell = 0, lmax+2
          do iq = I_TRU, I_SMT
            if( logder(iq,ell) > last_logder(iq,ell) ) then ! indicate the pole to be between the energy points ==> -dE/2
              if(o>0) write(o,'(3A,F16.6,99A)') sym, fun, &
                'pole at', (E-.5*dE)*eV, eV_, ( '     ', i=0,ell ), tru2char(iq),' ', ellchar(ell)
            endif ! pole detected !
          enddo ! iq
#ifdef FULL_DEBUG
! cDBG     write(unit=30+ell,fmt='(F16.6,99(ES18.6,ES16.6))') E*eV, atan(logder(:,ell))
#endif
        enddo ! ell
        last_logder = logder ! store information for the next energy point
      else  ! NaN
        nnan = nnan+1 ! count NaNs
      endif ! NaN
!       if(u>0) write(u,'(F16.6,99(ES18.6,ES16.6))') E*eV, logder(I_TRU,:)

    enddo ! ie
   !  write (*,*) 'dkin= ', dkin(:,:,:)
    if( nnan > 0 .and. o>0) write(o,'(4A,I0,9A)') sym, fun, WARNING(0), 'NaN appeared at ', nnan, ' energies!'

    if(u>0) close(unit=u,IOstat=ios)
    if(u>0.and.o>0) write(o,'(9A)') sym, fun, 'file "', trim(logderfile), '" written.'

  endfunction ! scattering_test

  real function match_sinc( logder ) result( x )
    real, intent(in) :: logder
    ! logarithmic derivative of sinc(k*r) at r=1 should match the given value
    !            sin(x)               x*cos(x)-sin(x)
    ! sinc(x) = --------    derived: -----------------
    !              x                        x^2
    !
    ! logder = x*cot(x)-1  with  Pi < x < 2*Pi
    !
    ! derivative w.r.t x is -1/sin^2-1/x^2
    !
    real :: res, dresdx, step

    x = 4.712389 ! 3Pi/2 start value
      res = x*cos(x)/sin(x)-1. - logder
    do while( abs( res ) > 1E-12 )
      dresdx = x/sin(x)**2 - cos(x)/sin(x)
      step = res/dresdx  ! Newton algorithm for finding zeros
      x = x + sign( min( 0.1, abs( step ) ) , step )
      if(o>0) write(o,'(2A,9(F0.12,A))') sym, 'match sinc: x=', x, ' d=', dresdx, ' s=',step
      res = x*cos(x)/sin(x)-1. - logder
    enddo ! while
    if(o>0) write(o,'(2A,9(F0.12,A))') sym, 'match sinc: x=', x
  endfunction ! match_sinc

  real elemental function sinc( x ) result( s )
    real, intent(in) :: x
    s = 1.0 ; if( abs( x ) > 1E-9 ) s = sin(x)/x
  endfunction ! sinc

  !! generate PAW data sets from a spherically symmetric DFT calculation
  status_t function generate_sym( sym, checks ) result( ist )
  use type_element, only: atomicnumber_by_symbol
  use configuration, only: WARNING, set_output_unit
    character(len=*), intent(in)    :: sym
    logical, intent(in)             :: checks

    character(len=*), parameter     :: fun = ' generate: '
    integer                         :: iZ

cDBG  if(o>0) write(o,'(9A)') sym, fun, 'element symbol = "', trim(sym), '".'
    iZ = atomicnumber_by_symbol( sym )

    ist = -9 ; if( iZ < 1 .or. iZ > 118 ) return

    ist = set_output_unit( 6 ) ! Caution: output unit of module configuration set to stdout

    ! generate a default configuration
    ist = generate_Z( iZ, xc=0, checks=checks, spin_polarized=.false. ) ! spin paired and default XC-functional

    if(o>0) write(o,'(9A)') trim(sym), fun, WARNING(1), trim(WARNING(0)), 's have been launched.'
cDBG  stop 'AE generate: Finalized (DEBUG MODE)'
    stop
  endfunction ! generate_sym

  !! generate PAW data sets from a spherically symmetric DFT calculation
  status_t function generate_Z( iZ, xc, checks, spin_polarized ) result( ist )
  use type_element, only: element, element_by_symbol
  use type_rgrid, only: rgrid, rgrid_create_exp, rgrid_eqn
  use configuration, only: WARNING, ERROR
  use configuration, only: keyword, I_KEYWORD_ELEM
  use input, only: eval
    integer, intent(in)             :: iZ
    integer, intent(in)             :: xc ! key of the XC-functional
    logical, intent(in)             :: checks
    logical, intent(in)             :: spin_polarized

    character(len=*), parameter     :: fun = ' generate: '
    integer, parameter              :: ELL_MAX = 7
    integer, parameter              :: ENN_MAX = 9
    integer, parameter              :: INL_MAX = ENN_MAX*(ENN_MAX+1)/2
    integer, parameter              :: ISPN_NOINIT =  -1
    integer, parameter              :: ISPN_EXPECT =   0
    integer, parameter              :: ISPN_FND1st =   1
    integer, parameter              :: ISPN_FND2nd =   2
    integer, parameter              :: ELL_KEYRCUT = -10
    integer, parameter              :: ELL_INVALID =  -1
    character, parameter            :: ELLCHAR(-1:ELL_MAX) = ['?','s','p','d','f','g','h','i','j']
    character, parameter            :: ENNCHAR( 0:ENN_MAX) = ['?','1','2','3','4','5','6','7','8','9']
    integer                         :: iZ_lim_occupied(INL_MAX)

    ! if Z >= iZ_lim_core(inl), the orbital #inl is treated as a core state by default
    integer, parameter              :: inl_selected(20) = &
    [  1, 2, 3, 4, 5, 7, 6, 8,11, 9,12,16,10,13,17,22,14,18,15,23 ]
    ! 1s 2s 2p 3s 3p 4s 3d 4p 5s 4d 5p 6s 4f 5d 6p 7s 5f 6d 5g 7p

    type(rgrid), target             :: g
    type(element)                   :: e
    integer                         :: nn(0:ELL_MAX)
    qn_ELL_t                        :: ellpre, ell, ell_loc
    qn_ENN_t                        :: een, enn, enn_lim
    qn_iNL_t                        :: inlpre, inl_corehole
    qn_iNL_t                        :: inl !! combindex for enn-ell quantum numbers
    qn_iLN_t                        :: iln
    real                            :: rcut(0:ELL_MAX), occ(-1:+1,1:INL_MAX,ICOR:IVAL)
    integer                   :: u = 6
    real                      :: q, Nve, Nce ! charge, Number of core electrons, Number of valence electrons
    character(len=16)         :: words(24), wd, next_wd
    integer                   :: iw, intZ
    integer                   :: ispn, ios
    real                      :: oc, rc, qel_corehole, full, coc(-1:+1), sigma
    integer                   :: icv(INL_MAX)
    character                 :: Vloc_method = ' '
    integer                   :: n_output_rgrid
  
    iZ_lim_occupied = 999
    iZ_lim_occupied(1:23) = [2,4,10,12,18,30,20,36,48,71,38,54,80,103,222,56,86,112,135,333,999,88,118]
    !! 1s 2s 2p 3s 3p 3d 4s 4p 4d 4f 5s 5p 5d 5f  5g  6s 6p 6d  6f  6g  6h  7s 7p  7d  7f  7g  7h  7i
    
    e = element_by_symbol( iZ ) ! load the configuration from module type_element

    if(o>0) write(o,'(9A)') sym, fun, 'configuration for ', trim(e%name)
    if(o>0) write(o,'(/,9A)') e%sym, '  ', e%config

    ist = -1 ! result -1: error, 0: valid

    icv     = ITOT ! init core valence switch
    occ     = 0. ! init occupation
    nn      = 0  ! no projectors
    ell_loc = 0  ! local potential for all ell >= ell0
    rcut    = 0. ! 0. is too small

    inl_corehole = 0  ! no core hole
    qel_corehole = 0. ! no core hole charge
    
    sigma = 0 ! no SHO projectors

    e%Z = 0. ; if( iZ < 121 ) e%Z = 1.*iZ
cDBG  if(u>0) write(u,'(3A,F0.1,9A)') sym, fun, 'Z = ', e%Z

    ellpre = ELL_INVALID ! init previous ell as invalid
    ispn = ISPN_NOINIT ! init previous spin as invalid

    words = '' ! init words
    read(unit=e%config,fmt=*,IOstat=ios) words ! read words from configuration string, ignore io-status
    iw = 1 ! init word counter
    do while( iw < size(words) )
      iw = iw+1 ! count up

      wd = words(iw-1)
      next_wd = words(iw)

      if( wd == '' ) cycle ! skip empty words

      enn = char2enn( wd(1:1) ) ! enn-quantum number
      ell = char2ell( wd(2:2) ) ! ell-character (spdfg)

cDBG  if(u>0) write(u,'(3A,I0,9A)') sym, fun, 'eval expression #',iw,' "',trim(wd),'"'
      !================================================================
      if( ell > ELL_INVALID .and. enn > ell ) then
      !================================================================
        inl = ennell2inl( enn, ell ) ! inl-combindex
        ! valid enn-ell pair
cDBG    if(u>0) write(u,'(3A,I0,9A)') sym, fun, 'found ',enn,ell2char(ell),'-state.'
        if( inl < 1 ) stop 'ell and enn valid, but inl-index < 1, ERROR!'

        icv(inl) = IVAL ! set to valence state

        ! set the number of projectors
        nn(ell) = nn(ell)+1 ! a new projector for this ell-channel
        if( wd(3:3) == '*' ) then
          nn(ell) = nn(ell)+1 ! add an excited state projector
          if( wd(4:4) == '*' ) nn(ell) = nn(ell)+1 ! add a 3rd projector
          if( nn(ell) > 2 .and. o>0) write(o,'(9A)') sym, fun, WARNING(0), '3rd ',ELLCHAR(ell),'-projector in test phase!'
!         if( nn(ell) > 3 ) stop '4th-projector not implemented!'
          if( nn(ell) > 3 .and. o>0) write(o,'(9A)') sym, fun, WARNING(0), 'more than 3 projectors not implemented!'
          nn(ell) = min( nn(ell), 3 ) ! never more than 3 projectors
        endif ! wd(3:3) == '*'
        ell_loc = max( ell_loc, ell+1 ) ! raise ell for local potential

!         occ(:,inl,IVAL) = OCC_NONE*(/1,2,1/) ! 0 < OCC_NONE < OCC_TINY ! mark this orbital as a valence state

        inlpre = inl
        ellpre = ell ! set the ell quantum number for occupation numbers following
        ispn   = ISPN_EXPECT ! indicate that an occupation number is expected next
      !================================================================
      elseif( wd(1:1) == 'z' .or. wd(1:1) == 'Z' ) then
      !================================================================
cDBG    if(u>0) write(u,'(3A,I0,9A)') sym, fun, 'found atomic core charge modifier "Z".'
cDBG    if(u>0) write(u,'(9A)') sym, fun, 'try to convert "', next_wd, '" to an atomic number.'
        read(unit=next_wd,fmt=*,iostat=ios) e%Z
        if( ios == 0 ) then
          iw = iw+1
          if(o>0) write(u,'(4A,I0,A,F0.3)') sym, fun, WARNING(0), 'element with formal Z = ',iZ,' modified to Z = ',e%Z
        else  ! ios
          if(o>0) write(u,'(9A)') sym, fun, WARNING(0), 'failed to convert "',trim(next_wd),'" to an atomic number!'
          e%Z = 1.*iZ
        endif ! ios
      !================================================================
      elseif( wd(1:1) == 'v' .or. wd(1:1) == 'V' ) then
      !================================================================
cDBG    if(u>0) write(u,'(3A,I0,9A)') sym, fun, 'found modifier "V" for the generation method of the local potential.'
        ios = index( wd, '=' )
        if( ios > 0 ) then
          Vloc_method = wd(ios+1:ios+1)
          if(o>0) write(o,'(4A,I0,9A)') sym, fun, WARNING(0), 'element with Z = ',iZ,' makes use of the method "',Vloc_method,'".'
        else  ! ios
          if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'failed to find "=" in "',trim(wd),'" to be converted to a Vloc_method!'
        endif ! ios
      !================================================================
      elseif( wd(1:1) == 'r' .or. wd(1:1) == 'R' .or. wd(1:1) == '|' ) then
      !================================================================
        ellpre = ELL_KEYRCUT ! indicate that one or more cutoff radii are expected
cDBG    if(u>0) write(u,'(9A)') sym, fun, 'expects cutoff radius in next expression.'
      !================================================================
      elseif( wd(1:4) == 'hole' ) then ! the keyword "hole" is followed closely by an orbital e.g. "hole2p"
      !================================================================
        if(o>0) write(o,'(9A)') sym, fun, 'try to set a corehole in orbital "',wd(5:6),'".'
        enn = char2enn( wd(5:5) ) ! enn-quantum number
        ell = char2ell( wd(6:6) ) ! ell-character (spdfg)
        inl = ennell2inl( enn, ell ) ! inl-combindex
        if( inl > 0 ) then
          inl_corehole = inl
          if(o>0) write(o,'(9A)') sym, fun, 'set a corehole in ',inl2string(inl),'-orbital.'
          read(unit=next_wd,fmt=*,IOstat=ios) qel_corehole
          if( ios == 0 ) then
            iw = iw+1
            if(o>0) write(o,'(5A,F0.3,9A)') sym, fun, 'set a corehole in ',inl2string(inl),'-orbital, strength = ',qel_corehole,' e.'
          else  ! ios
            qel_corehole = 1.0 ! default
            if(o>0) write(o,'(9A)') sym, fun, 'set a corehole of 1 electron in ',inl2string(inl),'-orbital.'
          endif ! ios
        else  ! inl
          if(o>0) write(o,'(3A,I0)') sym, fun, 'corehole could not be evaluated, inl-index = ',inl
        endif ! inl
      !================================================================
      elseif( wd(1:5) == 'sigma' ) then ! spread of SHO projectors
      !================================================================
cDBG    if(u>0) write(u,'(9A)') sym, fun, 'try to convert "', next_wd, '" to a sigma spread.'
        sigma = 0
        read(unit=next_wd,fmt=*,IOstat=ios) sigma
        if (ios == 0 .and. sigma > 0) then
          iw = iw+1
          if(o>0) write(o,'(9(3A,f0.6))') sym, fun, 'activate SHO projectors with sigma = ',sigma,' aB.'
        else  ! ios
          if(o>0) write(o,'(9A)') sym, fun, 'failed to activate SHO projectors.'
        endif ! ios
        ellpre = ELL_INVALID
      !================================================================
      elseif( ellpre > ELL_INVALID .and. ellpre <= ubound(occ,2) ) then
      !================================================================
        ! expects an occupation number for this ell-channel
        ell = ellpre
        inl = inlpre
        full = 2*ell+1.

        read(unit=wd,fmt=*,IOstat=ios) oc ! read an occupation number
        if( ios == 0 ) then
          if( e%Z > 0. ) oc = max( 0., oc ) ! no negative occupation numbers for regular elements
          selectcase( ispn ) ! decide on spin state
          case( ISPN_EXPECT ) ! assume that the 1st occupation number is for both spins, up&dn
cDBG        if(u>0) write(u,'(3A,F0.3,A,I0,9A)') sym, fun, 'found valence occupation number ',oc,' in expression #',iw,' "',trim(wd),'", use for both spins.'
            occ(-1:+1,inl,IVAL) = min( oc, 2.*full ) * (/.5,1.,.5/) ! both spins get half, limit by 2*ell+1
            ispn = ISPN_FND1st ! indicate, that one occupation number has been found
          case( ISPN_FND1st ) ! one occupation number has been found previously
            occ(+1,inl,IVAL) = min( 2.*occ(+1,inl,IVAL), full ) ! use the 1st occupation number for the 1st spin only
cDBG        if(u>0) write(u,'(3A,F0.3,A,I0,9A)') sym, fun, 'found 2 valence occupation numbers, use ',occ(+1,inl,IVAL),' in expression #',iw-1,' for the up-spin.'
cDBG        if(u>0) write(u,'(3A,F0.3,A,I0,9A)') sym, fun, 'use 2nd valence occupation number ',oc,' in expression #',iw,' "',trim(wd),'" for the dn-spin.'
            occ(-1,inl,IVAL) = min( oc, full ) ! use the 2nd occupation number for the 2nd spin
            occ( 0,inl,IVAL) = occ(+1,inl,IVAL) + occ(-1,inl,IVAL) ! update total occupation = up+dn-spin occupation
            ispn = ISPN_FND2nd ! indicate that two occupation numbers have been found
          case( ISPN_FND2nd ) ! two occupation numbers have already been found
            if(o>0) write(o,'(9A)') sym, fun, ERROR, 'a 3rd occupation number for ell=',ellchar(ell),' reading "',trim(e%config),'".'
            return ! error
          case( ISPN_NOINIT )
            if(o>0) write(o,'(9A)') sym, fun, ERROR, 'ISPN is ISPN_NOINIT!'
            return ! error
          case default
            if(o>0) write(o,'(9A)') sym, fun, ERROR, 'key ISPN out of range {0,1,2}'
            return ! error
          endselect ! ispn
        else  ! ios
          if(u>0) write(u,'(9A)') sym, fun, 'expects an occupation number (real)'
        endif ! ios

      !================================================================
      elseif( ellpre <= ELL_KEYRCUT ) then ! -10:s, -11:p, -12:d, ...
      !================================================================
        ell = ELL_KEYRCUT-ellpre
cDBG    if(u>0) write(u,'(9A)') sym, fun, 'expect cutoff radius for ell="',ellchar(ell),'" in "',trim(wd),'"'
        if( ell > ELL_MAX ) then
          ellpre = ELL_INVALID ! do not expect further radii
        else  ! ell too large
          read(unit=wd,fmt=*,IOstat=ios) rc
          if( ios == 0 ) then
            if( rc > 0. ) then ! okay
            elseif( rc == 0. ) then ! rc > 0
              if(o>0) write(o,'(9A)') sym, fun, ERROR, 'zero cutoff radius invalid.'
              return ! error
            else  ! rc > 0
              if(o>0) write(o,'(4A,F0.3,9A)') sym, fun, WARNING(0), 'cutoff radius < 0 invalid, received rcut = ', rc, ' aB, use |rcut|!'
            endif !
            rcut(ell:) = abs( rc ) ! set the cutoff radius for this ell and all higher ones
cDBG        if(u>0) write(u,'(5A,F0.3,9A)') sym, fun, 'cutoff radius for ell="',ellchar(ell),'" is ', rcut(ell), ' aB'

            ell = ell+1
            ellpre = ELL_KEYRCUT-ell ! set expectation to cutoff radius for the next ell-channel
          else  ! ios
cDBG        if(u>0) write(u,'(9A)') sym, fun, 'expects a cutoff radius for ell="',ellchar(ell),'" in "',trim(wd),'"'
          endif ! ios
        endif ! ell too large
      !================================================================
      else  ! ...
      !================================================================
cDBG    if(u>0) write(u,'(3A,I0,9A)') sym, fun, 'ignored expression #',iw,' "',trim(wd),'"'
      !================================================================
      endif ! ...
      !================================================================

    enddo ! while iw < size(w)

    intZ = nint( e%Z )

!   real, parameter :: OCC_TINY = 0.5**48 ! = 3.55E-15
!   real, parameter :: OCC_NONE = 0.5**64 ! = 5.42E-20

    do ell = 0, ENN_MAX-1
      full = 2*ell+1. ! full shell occupation of one spin-channel
      enn_lim = ENN_MAX ! init
      enn = ell
      do while( enn < ENN_MAX )
        enn = enn+1
        inl = ennell2inl( enn, ell )

        if( icv(inl) == IVAL ) then ! is a valence state
cDBG      if(o>0) write(o,'(5A,I2,9A)') sym, fun, 'add ',inl2string(inl),' valence state  inl=',inl
          enn_lim = min( enn_lim, enn )

        elseif( intZ >= iZ_lim_occupied(inl) ) then ! core state

          if( enn > enn_lim ) then ! there is a core state above a valence state
            if(o>0) write(o,'(99A)') sym, fun, ERROR, inl2string(inl),'-orbital is a core state by default,',&
                                      ' but higher than the ',ENNCHAR(enn_lim),ELLCHAR(ell),' valence state.'
            return ! error
          endif ! enn > enn_lim

          icv(inl) = ICOR ! set to core state
cDBG      if(o>0) write(o,'(5A,I2,9A)') sym, fun, 'add ',inl2string(inl),' to core states, inl=',inl
          occ(:,inl,ICOR) = full * (/1,2,1/) ! fully occupied

          ! create a core hole in this orbital, perferably in the dn-spin-channel
          if( inl == inl_corehole ) then
            occ(-1,inl,ICOR) = min( max( 0., full-qel_corehole     ), full )
            occ(+1,inl,ICOR) = min( max( 0., full-occ(-1,inl,ICOR) ), full )
            if(o>0) write(o,'(4A,2(F0.3,A),9A)') sym, fun, WARNING(0), &
              'core hole of ',full-occ(-1,inl,ICOR),' e(dn) and ',full-occ(+1,inl,ICOR),' e (up)', &
              ' in ',e%sym,' ',inl2string(inl),'-state created.'
            occ(0,inl,ICOR) = occ(-1,inl,ICOR) + occ(+1,inl,ICOR)
          endif ! core hole created
        else
          occ(:,inl,:) = 0. ! neither core nor valence state
        endif ! valence state

      enddo ! enn
    enddo ! ell

cDBG  if(o>0) write(o,'(3A,99A4)')   sym, fun, 'orbital ', ( inl2string( inl_selected(iw) ), iw=1,size(inl_selected) )
cDBG  if(o>0) write(o,'(3A,99F4.1)') sym, fun, 'valence ', occ(+1,inl_selected,IVAL)
cDBG  if(o>0) write(o,'(3A,99F4.1)') sym, fun, '        ', occ(-1,inl_selected,IVAL)
cDBG  if(o>0) write(o,'(3A,99F4.1)') sym, fun, 'core occ', occ(+1,inl_selected,ICOR)
cDBG  if(o>0) write(o,'(3A,99F4.1)') sym, fun, '        ', occ(-1,inl_selected,ICOR)

cDBG  if(o>0) write(o,'(3A,99F4.1)') ''
cDBG  if(o>0) write(o,'(3A,8i2,A,4f6.3,9A)') sym, fun, 'PAW-configuration: ', nn, ' Rcut ', rcut(0:3), ' aB'

    q = sum(occ(0,:,:)) - e%Z
    if( abs( q ) > 1E-9 .and. o>0) write(o,'(5A,F0.3,9A)') sym, fun, WARNING(0), &
               trim(e%name), ' was calculated while charged with ',q,' electrons!'

cDBG  if(o>0) ist = display_conf( o, e%Z, q, occ(0,:,:), legend=.true. )

    if(o>0) write(o,'(/4A/)') sym, fun, 'PAW for ', trim(e%name)

    if (sigma > 0) then
      rcut(3) = max(9*sigma, rcut(3)) ! automatic matching radius for sigma-potentials
    endif
    
    g = rgrid_create_exp( nint( 250.*sqrt(e%Z+9.) ) ) ! set the radial grid with default values

    n_output_rgrid = nint(eval('$output_rgrid',def=0.0))
    
!     if (sigma > 0) then
    if (.true.) then
      if ( n_output_rgrid == 0) then  ! exponential grid
        ist = calc_atom( g, iZ, e%Z, e%sym, occ, nn, rcut, xc, spin_polarized, &
                      config=e%config, check_logder=checks, sigma=sigma &
                     )
      else ! GPAW grid
        ist = calc_atom( g, iZ, e%Z, e%sym, occ, nn, rcut, xc, spin_polarized, &
                      config=e%config, check_logder=checks, sigma=sigma &
                       , g_xml_arg=rgrid_eqn('r=a*i/(n-i)', n=n_output_rgrid, a=0.4*9.e2/n_output_rgrid, d=0., b=0.) &
                     )
      endif ! softwitch output_rgrid
    else
      ist = calc_atom( g, iZ, e%Z, e%sym, occ, nn(0:ell_loc), rcut, xc, spin_polarized, &
                      config=e%config, check_logder=checks, Vloc_method=Vloc_method )
    endif

    if( abs( q ) > 1E-9 .and. o>0) write(o,'(5A,F0.3,9A)') sym, fun, WARNING(0), &
              trim(e%name), ' was calculated while charged with ',q,' electrons!'

    if(o>0) write(o,'(9A)') sym, fun, 'configuration for ', trim(e%name)
    if(o>0) write(o,'(/,5A,/)') trim(keyword(I_KEYWORD_ELEM)),'  ',e%sym,'  ',trim(e%config)

  endfunction ! generate_Z

#ifdef EXTENDED
!+ extended

  status_t function test( )
    write(*,*,iostat=test) __FILE__,' no module test implemented!'
  endfunction ! test

!- extended
#endif

endmodule ! all_electron

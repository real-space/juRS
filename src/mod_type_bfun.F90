#include "config.h"

! #define DEBUG
! #define FULL_DEBUG


#ifdef FULL_DEBUG
#define cDBG
#else
#define cDBG !DBG
#endif

!! @author Paul Baumeister, Andrea Nobile
!! @version 4.00
!!
!! one-dimensional radial function stored on an equidistant real space grid
!! for fast lookup and evaluation during the projector setup
module type_bfun
  use constants, only: Pi, SQRT2OVERPI
  use type_spline, only: spline
implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: sym = 'typeBFUN' !! module symbol

  public :: bfun
  public :: bfun_filtered
  public :: bfun_spline
  public :: operator( .at. )
  public :: operator( .derat. )
  public :: operator( .valderat. )
  public :: bfun_norm
  public :: scale_bfun
  public :: add_s_projector
  public :: add_bfun_bfun
#ifdef EXTENDED
  public :: test
#endif


  real, parameter :: DEF_DR = 0.5**6 !! pure powers of 1/2 are numerically good

  type :: bfun !! localized function given on an equidistant radial grid
    integer                 :: ell  = 0  !! ell quantum number
    real                    :: rcut = 0. !! maximal radius in real space (zero assumed beyond rcut)
    real                    :: dr   = DEF_DR !! grid-spacing
    real                    :: drinv= 1.0/DEF_DR !! inverse grid-spacing
    real, pointer           :: f(:) !! function in realspace, usually reduced by r^ell ! todo: allocatable here? 
    type(spline)            :: spl
    logical                 :: is_spline = .false.
  endtype ! bfun
 
  interface operator( .at. )
    module procedure eval
  endinterface

  interface operator( .derat. )
    module procedure eder
  endinterface

  interface operator( .valderat. )
    module procedure evalder
  endinterface

  contains

  type(bfun) function bfun_filtered( rf, rg, drg, ell, h, &
    threshold, reciprocal, radiusratio, ell_in, normalize, positive ) &
    result( b )
  use constants, only: Pi, SQRT2OVERPI
    real, intent(in)                          :: rf(0:) !! radial function (input)
    real, intent(in)                          :: rg(0:) !! radial grid (input)
    real, intent(in)                          :: drg(0:) !! radial grid spacings (input)
    integer, intent(in)                       :: ell !! angular momentum quantum number
    real, intent(in)                          :: h   !! grid spacing
    real, intent(in), optional                :: threshold !! assumed small mask value
    real, intent(in), optional                :: reciprocal !! mask width in reciprocal space
    real, intent(in), optional                :: radiusratio !! allow the new cutoff radius to be larger by this factor
    integer, intent(in), optional             :: ell_in !! ell-character during input
    integer, intent(in), optional             :: normalize !! modus to normalize the function after filtering
    logical, intent(in), optional             :: positive !! function must not be negative

    character(len=*), parameter               :: fun = ' filter: '
cDBG  iounit_t, parameter                     :: oI = 10 ! on input grid
cDBG  iounit_t, parameter                     :: oG = 20 ! in Bessel space
cDBG  iounit_t, parameter                     :: oF = 30 ! on equidistant grid
    integer, parameter                        :: N6(0:6) = (/0,1,2,3,4,5,6/)
    real, parameter                           :: P6(1:6) = 1./(1.*N6(1:6))
    integer, parameter                        :: IRMX = 511
    integer, parameter                        :: IQMX = 255
    integer, parameter                        :: DEF_INORM = 2 ! normalize via the square norm
    real, parameter                           :: DEF_THR = exp(-4.)
    real, parameter                           :: DEF_REC = 0.70710678
    real, parameter                           :: DEF_RCR = 1.6666666666
   
    integer                                   :: inorm, ellin, ir, nr, nrc, iq, ii
    real                                      :: thr, rec, rcr, r, dr, gmax, rc, q, dq, qcut, rc_old, norm(1:3), scal
    status_t                                  :: ist
    real, allocatable                         :: mrg(:) ! inverse of mask on input grid
    real                                      :: fr(0:IRMX), mr(0:IRMX), rgd(0:IRMX), drgd(0:IRMX)!, ft(0:IRMX)
    real                                      :: fq(0:IQMX), mq, qgd(0:IQMX), qm(0:6,2), km, rm(0:6,2)!, msr(0:IRMX)
#ifdef DEBUG
    iounit_t, save :: o = 6
    real, save                                :: h_last = -1.
    integer, save                             :: i_last = -1
    h_last = h
    i_last = inorm
    o = 0 ! no output
    if( h /= h_last .or. inorm /= i_last ) o = 6
cDBG  o = 6 ! output always
#else
    iounit_t, parameter :: o = 0 ! no output
#endif

    if( ell > 3 ) stop 'bfun_filtered: ELL > 3 not implemented!'
    if( ell < 0 ) stop 'bfun_filtered: ELL < 0 unphysical!'


    if(o>0) write(o,'(9A)') sym, fun, ''

    thr = DEF_THR ; if( present( threshold   ) ) thr = max( 1E-12, min( abs( threshold ), 0.1 ) )
    rec = DEF_REC ; if( present( reciprocal  ) ) rec = max( 0.10, abs( reciprocal ) )
    rcr = DEF_RCR ; if( present( radiusratio ) ) rcr = max( 1.00, abs( radiusratio ) )
    inorm = DEF_INORM ; if( present( normalize ) ) inorm = max( 0, normalize )
    if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'normalize function by |f|^', inorm

    gmax = Pi/h ! maximum frequency that can be represented on a grid with grid spacing h
    if(o>0) write(o,'(3A,9(F0.3,A))') sym, fun, 'grid spacing h = ', h, ' aB  gmax = ', gmax, ' sqRy  Ecut = ', .5*gmax**2, ' Ha'

    nr = ubound(rf,1)
    rc_old = rg(nr)
    if(o>0) write(o,'(3A,F0.6,9A)') sym, fun, 'old rcut = ', rc_old, ' aB'

    dr = 2*rc_old/IRMX
    if(o>0) write(o,'(3A,F0.6,A,F0.3,9A)') sym, fun, 'dr = ', dr, '  max r (grid) = ', IRMX*dr, ' aB'
    !!! adjust dr to some value that does not introduce any numerical errors, i.e. a purely integer power of 2.
    ii = floor( log( dr )/log( 0.5 ) )
    if(o>0) write(o,'(3A,I0,9(A,F16.12))') sym, fun, 'adjust dr to 0.5^', ii, ' ==> dr = ', .5**ii, ' aB'
    dr = 0.5**(ii) ! djust to some power of 0.5
    if(o>0) write(o,'(3A,F0.6,A,F0.3,9A)') sym, fun, 'dr = ', dr, '  max r (grid) = ', IRMX*dr, ' aB'


    ! the mask function should be smaller than the threshold at the new rcut
    ! exp( -1/2 (km rc)^2 ) = thr
    !          <==>
    !  km = sqrt( -2*log(thr) )/rc

    ! the ratio of cutoff radii is
    !          new rcut
    !  rcr = ------------
    !          old rcut

    ! determine the new cutoff radius
    rc = rcr * rc_old
    if(o>0) write(o,'(3A,9(F0.6,A))') sym, fun, 'rc = rcut * ', rcr, ' = ', rc, ' aB'
    nrc = min( max( 1, nint( rc/dr ) ), IRMX )

    if( nrc == IRMX ) stop 'tBFUN: not enough grid points on the radial grid!'
    rc = nrc*dr ! correct the cutoff radius to match a grid point exactly

    km = sqrt( -2.*log( thr ) )/rc ! decay constant of the Gaussian mask
    if(o>0) write(o,'(3A,9(F0.6,A))') sym, fun, 'R-mask exp{-(kr)^2/2)}, k = ', km, ' sqRy'

    ! rec: spread of the Gaussian to filter in reciprocal space
    qcut = rec*gmax ! ==> q-mask decays to 0.60653 at q=qcut

    if(o>0) write(o,'(3A,9(F0.6,A))') sym, fun, 'qcut = Pi/h * ', rec, ' = ', qcut, ' sqRy'
    dq = 4.*gmax/IQMX
    if(o>0) write(o,'(3A,9(F0.6,A))') sym, fun, 'dq = ', dq, ' max.q = ', IQMX*dq, ' sqRy'

    if(o>0) write(o,'(3A,9(F0.6,A))') sym, fun, 'Q-mask exp{-(qR)^2/2)}, R = ', 1./qcut, ' aB'
    if(o>0) write(o,'(3A,9(F0.6,A))') sym, fun, 'Q-mask exp{-(q/qc)^2/2)}, qc = ', qcut, ' sqRy'
    if(o>0) write(o,'(3A,9(F0.6,A))') sym, fun, 'rcut = ', rc_old, '  ---->  ', rc, ' aB'

    if( rg(nr) < rc_old ) stop 'tBFUN: input radial grid does not contain Rcut.'


#ifdef DEBUG
cDBG    if( h == h_last .and. inorm == i_last ) o = 0 ! outputs OFF
#endif

    ellin = ell ; if( present( ell_in ) ) ellin = ell_in
    if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'ell = ', ell, ' radial function is assumed as rf(:)*r^', ellin-ell

cDBG  if(oI>0) write(oI,'(/,A,I0)') '# ell = ', ell ! empty line before + ell
cDBG  if(oG>0) write(oG,'(/,A,I0)') '# ell = ', ell ! empty line before + ell
cDBG  if(oF>0) write(oF,'(/,A,I0)') '# ell = ', ell ! empty line before + ell

    ! create the mask function on the input grid
    norm = 0. ! init norm
    rm   = 0. ! init moments
    allocate( mrg(0:nr) ) ! inverse of mask function

    do ir = 0, nr
      r = rg(ir) ! input grid

      mrg(ir) = exp( 0.5*(km*r)**2 ) !! attention: original mask = exp( -0.5*(km*r)**2 )

cDBG  if(oI>0) write(oI,'(9F16.10)') r, rf(ir), rf(ir)*mrg(ir), 1.0/mrg(ir) ! grid, function, divided_by_mask, mask
      norm(1) = norm(1) + rf(ir)**inorm * r**(2+2*(ell-ellin)) * drg(ir) ! sum of for the square norm
cDBG  rm(:,1) = rm(:,1) + rf(ir)**2 * r**(2+2*(ell-ellin)) * drg(ir) * r**N6 ! moments
    enddo

    qm = 0. ! init moments
    ! Bessel transform the function/mask


    do iq = 0, IQMX
      q = iq * dq
      qgd(iq) = q ! set up q-grid for later usage

      ! unfiltered
      fq(iq) = sum( rf(:nr) * mrg(:) * rg(:nr)**(2+2*ell-ellin) * Jellxmell( ell, x=q*rg(:nr) ) * drg(:nr) ) * q**ell * SQRT2OVERPI

      ! create q-mask
      mq = exp( -0.5*(q/qcut)**2 ) ! decays to 0.60653 for q=qcut and 0.13534 for q=2*qcut

cDBG  if(oG>0) write(oG,'(9F16.10)') q, fq(iq), fq(iq)*mq, mq ! grid, function, function*mask, mask
cDBG  qm(0:,1) = qm(0:,1) + fq(iq)**2 * q**2 * q**N6 ! integrate <1>,<q>,<q^2>,... without mask
      fq(iq) = fq(iq) * mq ! apply the q-mask
cDBG  qm(0:,2) = qm(0:,2) + fq(iq)**2 * q**2 * q**N6 ! integrate <1>,<q>,<q^2>,... with mask

    enddo ! iq

    qm = qm * dq

    ! prepare radial grid
    ! prepare real space mask function

    ! create mask function on the equidistant grid

    do ir = 0, IRMX
      r = ir * dr
      rgd(ir)  = r   ! store the radial grid
      drgd(ir) = dr  ! store drdi
      mr(ir) = exp( -0.5*(km*r)**2 )
    enddo ! ir

    fr = 0. ! init

    do ir = 0, nrc
      r = rgd(ir) ! equidistant grid

      fr(ir) = sum( fq(:) * qgd(:)**(2+ell) * Jellxmell( ell, x=qgd(:)*r ) ) * dq * SQRT2OVERPI
      if( present( positive ) ) fr(ir) = max( 0.0, fr(ir) )
cDBG  if(oF>0) write(oF,'(9F16.6)') r, fr(ir)*mr(ir), fr(ir), mr(ir) ! grid, function, divided_by_mask, mask
      fr(ir) = fr(ir) * mr(ir) ! apply the r-mask

    enddo


    do ir = 0, nrc
      r = rgd(ir) ! equidistant grid
      norm(2) = norm(2) + fr(ir)**inorm * r**(2+2*ell) * dr ! sum for the square norm
cDBG  rm(:,2) = rm(:,2) + fr(ir)**2 * r**(2+2*ell) * dr * r**N6 ! moments
    enddo


    fr(nrc+1:) = 0.
cDBG  r = dr*IRMX

cDBG ! show old r-moments
cDBG if(o>0) write(o,'(3A,I2,A,6F10.3,A,F10.3)') sym, fun, 'ell =', ell, ' r-Moments:', (rm(1:,1)/rm(0,1))**P6, ' aB,   norm', rm(0,1)
cDBG ! show old q-moments
cDBG if(o>0) write(o,'(3A,I2,A,6F10.3,A,F10.3)') sym, fun, 'ell =', ell, ' q-Moments:', (qm(1:,1)/qm(0,1))**P6, ' sqRy, norm', qm(0,1)
cDBG ! show new q-moments
cDBG if(o>0) write(o,'(3A,I2,A,6F10.3,A,F10.3)') sym, fun, 'ell =', ell, ' q-maskedM:', (qm(1:,2)/qm(0,2))**P6, ' sqRy, norm', qm(0,2)
cDBG ! show new r-moments
cDBG if(o>0) write(o,'(3A,I2,A,6F10.3,A,F10.3)') sym, fun, 'ell =', ell, ' r-maskedM:', (rm(1:,2)/rm(0,2))**P6, ' aB,   norm', rm(0,2)

    if( inorm > 0 ) then
      ! correct for the norm loss
      if( norm(1) <= 0. ) &
! #ifdef DEBUG
!         stop 'tBFUN try to normalize a zero.'
! #else
      then
        if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'ell = ', ell, ' initial norm zero!'
        return
      endif
! #endif

      scal = ( norm(1)/norm(2) )**(1./real(inorm))
      fr = fr * scal
cDBG  if(o>0) write(o,'(3A,I0,9(A,F0.6))') sym, fun, 'ell = ', ell, &
cDBG    ' norm ', norm(1)**(1./real(inorm)), '  ---->  ', norm(2)**(1./real(inorm)), ' ==> scale with ', scal
cDBG  ! test, if the new norm is good
cDBG  norm(3) = sum( fr(:nrc)**inorm * rgd(:nrc)**(2+2*ell) ) * dr
cDBG  if(o>0) write(o,'(3A,I0,9(A,F0.6))') sym, fun, 'ell = ', ell, ' norm ', norm(3)**(1./real(inorm)), ' (scaled)'

#ifdef DEBUG
    else ; if(o>0) write(o,'(9A)') sym, fun, 'no normalization required'
#endif
    endif ! inorm > 0 ! normalization required

    b%ell   = ell
    b%rcut  = rc ! new cutoff radius
    b%dr    = dr   ! grid spacing
    b%drinv = 1.0/b%dr
    allocate( b%f(0-2:nrc+2), stat=ist ) ; if( ist /= 0 ) stop 'bfun_filtered: allocation of b%F failed.'
    !allocate( b%f(0-2:size(rg, 1)), stat=ist ) ; if( ist /= 0 ) stop 'bfun_filtered: allocation of b%F failed.'
    b%f(nrc:)     = 0. ! strictly localized
    b%f(0:nrc)    = fr(0:nrc) ! copy
    b%f(0-2:-1:1) = fr(0+2:1:-1) ! symmetric around the origin, so copy

    b%is_spline = .false.
cDBG  if(o>0) write(o,'(/,A,/,9(A,I0))') 'difference in real-space:', 'xmgrace fort.',oI,' fort.',oF,' &'
cDBG  if(o>0) write(o,'(  A,/,9(A,I0))') 'difference in    q-space:', 'xmgrace -nxy fort.',oG,' &'
cDBG  if(o>0) write(o,'(A)') '' ! empty line
  endfunction ! bfun_filtered


  type(bfun) function bfun_spline( rf, rg, ell, rc ) result( b )
  use constants, only: Pi, SQRT2OVERPI
  use type_spline, only: spline_create
  use type_rgrid, only: rgrid, rgrid_eqn
  use radial_interpolation, only: interpolate_on_grid
  !use all_electron, only: write2file

    real, intent(in)                          :: rf(0:) !! radial function (input)
    type(rgrid) ,intent(in)                   :: rg !! radial grid (input)
    
    integer, intent(in)                       :: ell !! angular momentum quantum number
    real, intent(in)                          :: rc ! rcut
    character(len=*), parameter               :: fun = ' bfun_spline: '

    type(rgrid)                               :: urg ! uniform radial grid
    status_t                                  :: ist
    real, allocatable                         :: urf(:)
    real                                      :: dr
    integer                                   :: ngr, i
    integer, save                             :: nwrite =0
    
    b%is_spline = .true.
    ngr = 100
    dr = rc/real(ngr)
    !write (*,*) 'rcut spline', rc
    allocate(urf(0:ngr), stat=ist )
    if( ist /= 0 ) stop 'bfun_spline failed allocation'

                         ! eqn, n, a, d, b    
    urg = rgrid_eqn('r=d*i', ngr+1, a=0., d=dr, b=0.)

    !ist =  write2file('interp', rf, grid=rg%r)
    call interpolate_on_grid(rg, rf, urg, urf)
    urf(ngr) = 0.
    !write(*,*)  'rc = ', rc
    if (nwrite .eq. 0) then 
      !ist =  write2file('interp', urf, grid=urg%r)
      nwrite = nwrite + 1
      !stop
    else if (nwrite .eq. 1) then 
      !ist =  write2file('interp1', urf, grid=urg%r)
      nwrite = nwrite + 1
    else if (nwrite .eq. 2) then 
      !ist =  write2file('interp2', urf, grid=urg%r)
      nwrite = nwrite + 1
      !stop
     

    else if (nwrite .eq. 3) then 
      !ist =  write2file('interp3', urf, grid=urg%r)
      nwrite = nwrite + 1
      !stop
    else if (nwrite .eq. 4) then 
      !ist =  write2file('interp4', urf, grid=urg%r)
      nwrite = nwrite + 1
      !stop
    endif  
    

    b%spl = spline_create( ell, dr, ngr, urf )
    
    !do the old way for now
    b%ell   = ell
    b%rcut  = rc 
    b%dr    = dr   
    b%drinv = 1.0/b%dr
    allocate( b%f(0-2:ngr+2), stat=ist ) ; if( ist /= 0 ) stop 'bfun_spline: allocation of b%F failed.'
    
    b%f(ngr:)     = 0. ! strictly localized
    b%f(0:ngr)    = urf(0:ngr) ! copy
    b%f(0-2:-1:1) = urf(0+2:1:-1) ! symmetric around the origin, so copy
    !b%f(-2:0) = urf(0)

  endfunction 




  !! Bessel function J_ell(x) multipied with x^(-ell)
  real elemental function Jellxmell( ell, x ) result( J )
    integer, intent(in) :: ell
    real, intent(in) :: x

    real, parameter     :: THRESHOLD = 1.E-8
    real, parameter     :: STARTVAL(0:3) = (/1.,3.,15.,105./)

    if( abs( x ) < THRESHOLD ) then
      j = 1./STARTVAL(ell)
      return
    endif
    selectcase( ell )
!     case(:-1) ; stop 'tBFUN Jellxmell: ell < 0 unphysical!'
    case( 0 ) ; J = (sin(x))/x                                         ! j0(x)
    case( 1 ) ; J = (sin(x)-x*cos(x))/x**3                             ! j1(x) / x
    case( 2 ) ; J = ((3.-x*x)*sin(x)-3.*x*cos(x))/x**5                 ! j2(x) / x^2
    case( 3 ) ; J = ((15.-6.*x*x)*sin(x)-(15.-x*x)*x*cos(x))/x**7      ! j3(x) / x^3
!     case( 4:) ; stop 'tBFUN Jellxmell: ell > 3 not implemented!'
    endselect ! ell
  endfunction ! Jellxmell


  !! eval the value of the radial function by interpolation
  real elemental function eval( b, r ) result( f )
    !use type_spline
    type(bfun), intent(in) :: b
    real, intent(in)       :: r

    real :: ra, x, xp1, xp0, xm1, xm2, a(-1:2)
    integer :: ir



    integer                  :: bb
    real                     :: u, val, der
    
    if(b%is_spline) then
      !  f = spline_eval(b%spl, r)
      !
      bb = r / b%spl%dr
      if (bb >= b%spl%n) then 
        val = 0.0
        f= val
        der = 0.0
        return
      endif

      u = r - real(bb) * b%spl%dr
      val = b%spl%data(4*bb) + u*(b%spl%data(4*bb+1) + u*(b%spl%data(4*bb+2) + u*b%spl%data(4*bb+3)))
      der = b%spl%data(4*bb+1) + u * (2.0 * b%spl%data(4*bb+2) + u * 3.0 * b%spl%data(4*bb+3));


      f = val
      return


    endif
    
 
    ! example: x = 0.25
    !        x
    ! ==o===o===o===o===
    !  -1  -0   1   2
!             [       ]*[ x-(-0)]*[ x-(+1)]*[ x-(+2)]   x*(x-1)*(x-2)
!     a(-1) = --------------------------------------- = -------------
!             [       ]*[-1-(-0)]*[-1-(+1)]*[-1-(+2)]        -6
! 
!             [ x-(-1)]*[       ]*[ x-(+1)]*[ x-(+2)]   (x+1)*(x-1)*(x-2)
!     a(-0) = --------------------------------------- = -----------------
!             [-0-(-1)]*[       ]*[-0-(+1)]*[-0-(+2)]         2
! 
!             [ x-(-1)]*[ x-(-0)]*[       ]*[ x-(+2)]   (x+1)*x*(x-2)
!     a(+1) = --------------------------------------- = -------------
!             [+1-(-1)]*[+1-(-0)]*[       ]*[+1-(+2)]        -2
! 
!             [ x-(-1)]*[ x-(-0)]*[ x-(+1)]*[       ]   (x+1)*x*(x-1)
!     a(+2) = --------------------------------------- = -------------
!             [+2-(-1)]*[+2-(-0)]*[+2-(+1)]*[       ]         6
! 



      f = 0.
      ra = abs( r )
      if( ra > b%rcut ) return
      !if(r < 0) return
      if( .not. associated( b%f ) ) return
      x = ra*b%drinv
      ir = int( x ) ! == floor( x )
      x = x - real(ir)
      xm2 = x - 2.
      xm1 = x - 1.
      xp0 = x + 0.
      xp1 = x + 1.

      a(-1) = (     xp0*xm1*xm2 )/(-6.)
      a(-0) = ( xp1    *xm1*xm2 )/(+2.)
      a(+1) = ( xp1*xp0    *xm2 )/(-2.)
      a(+2) = ( xp1*xp0*xm1     )/(+6.)
      f = sum( b%f(ir-1:ir+2)*a(-1:2) )

    !endif
   

    

  endfunction ! eval


  !! eval the 1st derivative
  real elemental function eder( b, r ) result( df )
    type(bfun), intent(in) :: b
    real, intent(in)       :: r

    real :: ra, x, xp1, xp0, xm1, xm2, a(-1:2)
    integer :: ir

    integer                  :: bb
    real                     :: u, val, der
    
    if(b%is_spline) then
      !  f = spline_eval(b%spl, r)
      !
      bb = r / b%spl%dr
      if (bb >= b%spl%n) then 
        val = 0.0
        
        der = 0.0
        df = der
        return
      endif

      u = r - real(bb) * b%spl%dr
      val = b%spl%data(4*bb) + u*(b%spl%data(4*bb+1) + u*(b%spl%data(4*bb+2) + u*b%spl%data(4*bb+3)))
      der = b%spl%data(4*bb+1) + u * (2.0 * b%spl%data(4*bb+2) + u * 3.0 * b%spl%data(4*bb+3));


      df = der
      return


    endif



    df = 0.
    if( .not. associated( b%f ) ) return
    ra = abs( r )
    if( ra > b%rcut ) return
    x = ra*b%drinv
    ir = int( x ) ! == floor( x )
    x = x - real(ir)
    xm2 = x - 2.
    xm1 = x - 1.
    xp0 = x + 0.
    xp1 = x + 1.

    a(-1) = (     xm1*xm2 +     xp0*xm2 +     xp0*xm1 )/(-6.)
    a(-0) = (     xm1*xm2 + xp1    *xm2 + xp1    *xm1 )/(+2.)
    a(+1) = ( xp0    *xm2 + xp1    *xm2 + xp1*xp0     )/(-2.)
    a(+2) = ( xp0*xm1     + xp1*xm1     + xp1*xp0     )/(+6.)
    df = sum( b%f(ir-1:ir+2)*a(-1:2) )*b%drinv

  endfunction ! eder

  !! combination of both, value and derivative
  function evalder( b, r ) result( f )
    type(bfun), intent(in) :: b
    real, intent(in)       :: r
    real                   :: f(0:1) ! result

    f(0) = eval( b, r )
    f(1) = eder( b, r )
  endfunction ! evalder


  real function bfun_norm( bf, power ) result( nrm )
    type(bfun), intent(in)        :: bf
    integer, intent(in), optional :: power

    integer, parameter :: DEFpow = 1 ! default
    integer :: ir, np
    real :: r

    nrm = 0.
    np = DEFpow ; if( present( power ) ) np = max(0,power)
    selectcase( np )
    case( 0 ) ! infinity norm
      do ir = 1, ubound(bf%f,1)-2
        r = ir*bf%dr
        nrm = max(nrm, bf%f(ir) * r**bf%ell )
      enddo ! ir
    case( 1 )
      do ir = 1, ubound(bf%f,1)-2
        r = ir*bf%dr
        nrm = nrm + r*r * abs( bf%f(ir) * r**bf%ell )
      enddo ! ir
    case( 2 )
      do ir = 1, ubound(bf%f,1)-2
        r = ir*bf%dr
!         nrm = nrm + r*r* ( bf%f(ir) * r**bf%ell )**2
        nrm = nrm + r**(2+2*bf%ell) * bf%f(ir) * bf%f(ir)
      enddo ! ir
    case default
      do ir = 1, ubound(bf%f,1)-2
        r = ir*bf%dr
        nrm = nrm + r*r* ( bf%f(ir) * r**bf%ell )**np
      enddo ! ir
    endselect ! np
    nrm = nrm*bf%dr
  endfunction ! bfun_norm

  

  subroutine scale_bfun( bf, factor )
    type(bfun), intent(inout)     :: bf
    real, intent(in)              :: factor
    if( factor /= factor ) stop 'tBFUN scale_bfun: received NaN scale factor!'
    if( associated( bf%f ) ) bf%f = bf%f * factor
  endsubroutine ! scale_bfun

  subroutine add_bfun_bfun( bf, factor, bfa )
    type(bfun), intent(inout)     :: bf
    real, intent(in)              :: factor
    type(bfun), intent(in)        :: bfa
    if( factor /= factor ) stop 'tBFUN add_bfun_bfun: received NaN scale factor!'
    if( associated( bf%f ) .and. associated( bfa%f ) ) bf%f = bf%f + factor * bfa%f
  endsubroutine ! add_bfun_bfun


  !! this subroutine adds a spherically symmetric function
  !! to some quantity on a cartesian grid. It assumes that
  !! the radial function is given as a type bfun.
  !! special use: smooth core charges, smooth correction potentials
  subroutine add_s_projector( bf, qnt, hg, origins, offset, factor, derive2i )
  use constants, only: Y00 => ONESQRTFOURPI
#ifdef DEBUG
  use unitsystem, only: Ang, Ang_
#endif
    type(bfun), intent(in)            :: bf !! localized radial function
    real, intent(inout)               :: qnt(:,:,:) !! quantity to add to
    real, intent(in)                  :: hg(1:3) !! grid spacings
    real, intent(in)                  :: origins(1:,1:) !! atomic origins(1:3,#periodic images)
    real, intent(in)                  :: offset(3) !! grid offset
    real, intent(in), optional        :: factor !! prefactor (additional to Y00)
    integer, intent(in), optional     :: derive2i !! derivative 0: underived, {1,2,3}; {d/dx,d/dy,d/dz}

    character(len=*), parameter       :: fun = ' add_s_projector: '
#ifdef DEBUG
    integer, parameter                :: o = 6
#else
    integer, parameter                :: o = 0
#endif
    integer                           :: ng(1:3), i1, i2, i3, ii, ise(3,2), ider
    real                              :: rcut, rcut2, rv(1:3), rs, rv2(1:3), rs2, ori(1:3), fac

    

    rcut  = bf%rcut
    rcut2 = rcut**2
#ifdef DEBUG
    if(o>0) write(o,'(3A,F0.3,9A)') sym, fun, 'rcut = ', rcut*Ang, Ang_
#endif

    ! extra prefactor, if present
    fac = Y00 ; if( present( factor ) ) fac = factor*Y00

    ! direction of derivative, 0: no derivative, {1,2,3}: {d/dx,d/dy,d/dz}
    ider = 0 ; if( present( derive2i ) ) ider = derive2i
    ! check range of ider
    selectcase( ider )
    case( 1, 2, 3 ) ! ok
    case( 0 ) ! ok
#ifdef DEBUG
      if( present(derive2i) .and. o/=0 ) write(o,'(9A)') sym, fun, 'Warning!', 'DERIVE2I present but 0.'
#endif
    case default ; stop 'add_s_projector: DERIVE2I out of bounds [1, 3].'
    endselect ! ider

    ng = shape( qnt )

#ifdef DEBUG
    if(o>0) write(o,'(3A,I0)') sym, fun, 'number of images = ', size(origins,2)
#endif
    ! for each periodic image
    do ii = 1, size(origins,2) ; ori = origins(1:3,ii)-offset(1:3)

      ! determine box around the origin
      ise(:,1) = max(  1, nint( (ori-rcut)/hg ) - 1 )
      ise(:,2) = min( ng, nint( (ori+rcut)/hg ) + 1 )

      ! if any of the end values is less than the start value,
      ! no operation has to be executed
      if( any( ise(:,2) < ise(:,1) ) ) cycle

      do i3 =     ise(3,1), ise(3,2) ; rv(3) = i3*hg(3)-ori(3) ; rv2(3) = rv(3)*rv(3)
        do i2 =   ise(2,1), ise(2,2) ; rv(2) = i2*hg(2)-ori(2) ; rv2(2) = rv(2)*rv(2)
          do i1 = ise(1,1), ise(1,2) ; rv(1) = i1*hg(1)-ori(1) ; rv2(1) = rv(1)*rv(1)

            rs2 = rv2(1) + rv2(2) + rv2(3)
            if( rs2 <= rcut2 ) then
              rs = sqrt(rs2+1.E-24) ! add some safety
              if( ider == 0 ) then    ! no interpolation
                qnt(i1,i2,i3) = qnt(i1,i2,i3) + fac * ( bf .at. rs )
              else  ! ider == 0       ! no interpolation
                qnt(i1,i2,i3) = qnt(i1,i2,i3) + fac * ( bf .derat. rs ) * rv(ider)/rs
              endif ! ider == 0

            endif ! r^2 <= rcut^2

          enddo ! ix
        enddo ! iy
      enddo ! iz

    enddo ! ii


  endsubroutine ! add_s_projector
 


#ifdef EXTENDED
  !! test function for this module
  status_t function test( ) result( ist )
    type(bfun) :: b
    real       :: f(0:999), r(0:999), dr(0:999)=0.001
    integer    :: i
    r = (/ ( i*0.001, i=0,999 ) /)
    f = (1.-r**2)**2 ! simplest Weinert function
    b = bfun_filtered( f, r, dr, ell=0, h=0.25 )
    ist = 0
  endfunction ! test

!- extended
#endif 

endmodule 

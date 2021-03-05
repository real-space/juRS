#include "config.h"

! #define DEBUG


!! @author Paul Baumeister
!! @version 3.0
!!
!! real-valued spherical harmonic functions r^ell Y_ell m
module harmonics
  use constants, only: f => ONESQRTFOURPI
implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: sym = 'SHX' !! module symbol

  public :: Xlmax_rl
  public :: d_Xlmax_rl_dri
  public :: d2_Xlmax_rl_dridri
  public :: Ylmax_rl !! complex
  public :: Xlm_rl

#ifdef EXTENDED
  public :: test ! self-test of the derived spherical harmonics
  public :: Ylm2Xlm ! this functions is required in CYL
  public :: dagger ! this functions is required in CYL

  interface dagger
    module procedure dagger_r, dagger_c
  endinterface
#endif

    !
    ! This module contains the real spherical harmonics,
    ! the ordering and signs of the linear combinations
    ! have been chosen according to the publication
    ! H. H.H. Homeier, E. O. Steinborn, J. Mol. Struc., 368 (1996) 31-37
    !
    ! Condon-Shortley??

  integer, parameter, public :: ELLMAX_IMPLEMENTED = 6 !! hard limit for ell

  contains

  function Xlmax_rl( ellmax, v ) result( Xlm )
  ! writes the spherical harmonics (real combinations) 
  ! X_{\ell m}(\hat{\vec{v}}) (m=-\ell..\ell) up to a given
  ! ellmax, multiplied with r^ell where r=|\vec{v}|
  !                                       PBaum Aug 08
  ! use constants, only: f => ONESQRTFOURPI
  implicit none
    ! arguments
    integer, intent(in)   :: ellmax
    real, intent(in)      :: v(3) !! vector v(1:3)
    ! result
    real                  :: Xlm(1:(ellmax+1)**2)
    ! local vars
    real                  :: r2, x2, y2, z2
    real                  :: r4, x4, y4, z4
    real                  :: r6, x6, y6, z6

#ifdef DEBUG
    if( ellmax < 0 ) stop 'Xlmax_rl: ellmax < 0 unphysical'
#else
    if( ellmax < 0 ) return
#endif

#define x v(1)
#define y v(2)
#define z v(3)

    ! ===  ell = 0  ===============================================================
    Xlm( 1) =  f ! s-orbital

    if( ellmax < 1 ) return
    ! ===  ell = 1  ===============================================================
    Xlm( 2) =  f*sqrt(3.)*x ! px-orbital
    Xlm( 3) =  f*sqrt(3.)*z ! pz-orbital
    Xlm( 4) =  f*sqrt(3.)*y ! py-orbital

    if( ellmax < 2 ) return
    ! === abbreviate ==============================================================
    x2 = x*x ; y2 = y*y ; z2 = z*z ; r2 = x2 + y2 + z2
    ! ===  ell = 2  ===============================================================
    Xlm( 5) =  f*sqrt( 3.75)*(x2-y2)    ! d-orbital !  eg
    Xlm( 6) =  f*sqrt(15.  )*z*x              ! d-orbital ! t2g
    Xlm( 7) =  f*sqrt( 1.25)*(3.*z2-r2)        ! d-orbital !  eg
    Xlm( 8) =  f*sqrt(15.  )*y*z              ! d-orbital ! t2g
    Xlm( 9) =  f*sqrt(15.  )*x*y              ! d-orbital ! t2g

    if( ellmax < 3 ) return
    ! ===  ell = 3  ===============================================================
    Xlm(10) =  f*sqrt(  4.375)*x*(x2-3.*y2)  ! f-orbital
    Xlm(11) =  f*sqrt( 26.25 )*z*(x2-y2)     ! f-orbital
    Xlm(12) =  f*sqrt(  2.625)*x*(5.*z2-r2)         ! f-orbital
    Xlm(13) =  f*sqrt(  1.75 )*z*(5.*z2-3.*r2)      ! f-orbital
    Xlm(14) =  f*sqrt(  2.625)*y*(5.*z2-r2)         ! f-orbital
    Xlm(15) =  f*sqrt(105.   )*x*y*z                ! f-orbital
    Xlm(16) =  f*sqrt(  4.375)*y*(3.*x2-y2)  ! f-orbital

    if( ellmax < 4 ) return
    ! === abbreviate ==============================================================
    x4 = x2*x2 ; y4 = y2*y2 ; z4 = z2*z2 ; r4 = r2*r2
    ! ===  ell = 4  ===============================================================
    Xlm(17) =  f*sqrt( 4.921875)*(x4-6.*x2*y2+y4)
    Xlm(18) =  f*sqrt(39.375   )*z*x*(x2-3.*y2)
    Xlm(19) =  f*sqrt( 2.8125  )*(x2-y2)*(7.*z2-r2)
    Xlm(20) =  f*sqrt( 5.625   )*x*z*(7.*z2-3.*r2)
    Xlm(21) =  f*sqrt( 0.140625)*(35.*z4-30.*z2*r2+3.*r4)
    Xlm(22) =  f*sqrt( 5.625   )*y*z*(7.*z2-3.*r2)
    Xlm(23) =  f*sqrt(11.25    )*x*y*(7.*z2-r2)
    Xlm(24) = -f*sqrt(39.375   )*z*y*(y2-3.*x2)
    Xlm(25) =  f*sqrt(78.75    )*x*y*(x2-y2)

    if( ellmax < 5 ) return
    ! ===  ell = 5  ===============================================================
    Xlm(26) =  f*sqrt(  5.4140625)*x*(x4-10.*x2*y2+5.*y4)
    Xlm(27) =  f*sqrt( 54.140625 )*(x4-6.*x2*y2+y4)*z
    Xlm(28) =  f*sqrt(  3.0078125)*x*(x2-3.*y2)*(9.*z2-r2)
    Xlm(29) =  f*sqrt( 72.1875   )*(x2-y2)*z*(3.*z2-r2)
    Xlm(30) =  f*sqrt(  2.578125 )*x*(21.*z4-14.*z2*r2+r4)
    Xlm(31) =  f*sqrt( 0.171875  )*z*(63.*z4-70.*z2*r2+15.*r4)
    Xlm(32) =  f*sqrt(  2.578125 )*y*(21.*z4-14.*z2*r2+r4)
    Xlm(33) =  f*sqrt(288.75     )*y*x*z*(3.*z2-r2)
    Xlm(34) = -f*sqrt(  3.0078125)*y*(y2-3.*x2)*(9.*z2-r2)
    Xlm(35) = -f*sqrt(866.25     )*y*x*(y2-x2)*z
    Xlm(36) =  f*sqrt(  5.4140625)*y*(y4-10.*y2*x2+5.*x4)

    if( ellmax < 6 ) return
    ! === abbreviate ==============================================================
    x6 = x4*x2 ; y6 = y4*y2 ; z6 = z4*z2 ; r6 = r4*r2
    ! ===  ell = 6  ===============================================================
    Xlm(37) =  f*sqrt( 5.865234375)*(x6-15.*x4*y2+15.*x2*y4-y6)
    Xlm(38) =  f*sqrt(70.3828125  )*x*(x4-10.*x2*y2+5.*y4)*z
    Xlm(39) =  f*sqrt( 3.19921875 )*(x4-6.*x2*y2+y4)*(11.*z2-r2)
    Xlm(40) =  f*sqrt(10.6640625  )*x*(x2-3.*y2)*z*(11.*z2-3.*r2)
    Xlm(41) =  f*sqrt( 2.666015625)*(x2-y2)*(33.*z4-18.*z2*r2+r4)
    Xlm(42) =  f*sqrt( 4.265625   )*x*z*(33.*z4-30.*z2*r2+5.*r2*r2)
    Xlm(43) =  f*sqrt( 0.05078125 )*(231.*z6-315.*z4*r2+105.*z2*r4-5.*r6)
    Xlm(44) =  f*sqrt( 4.265625   )*y*z*(33.*z4-30.*z2*r2+5.*r4)
    Xlm(45) =  f*sqrt(10.6640625  )*y*x*(33.*z4-18.*z2*r2+r4)
    Xlm(46) = -f*sqrt(10.6640625  )*y*(y2-3.*x2)*z*(11.*z2-3.*r2)
    Xlm(47) = -f*sqrt(51.1875     )*y*x*(y2-x2)*(11.*z2-r2)
    Xlm(48) =  f*sqrt(70.3828125  )*y*(y4-10.*y2*x2+5.*x4)*z
    Xlm(49) =  f*sqrt(23.4609375  )*x*y*(3.*y4-10.*x2*y2+3.*x4)

    if( ellmax < 7 ) return

    Xlm(50:) = 0.0
    write(*,'(4A)') sym, ' Xlmax_rl: ', 'warning, ellmax > 6 not implemented.'
    stop 'Xlmax_rl: ellmax larger than implemented (max 6).'

#undef x
#undef y
#undef z
  endfunction Xlmax_rl




  function d_Xlmax_rl_dri( ellmax, v, derive2i ) result( dXlm )
  ! writes the derivative of the spherical harmonics
  ! (real combinations) X_{\ell m}(\hat{\vec{v}}) (m=-\ell..\ell) up to a given
  ! ellmax, multiplied with r^ell where r=|\vec{v}|
  ! w.r.t each of its components
  !                                       PBaum Nov 08
  ! use constants, only: f => ONESQRTFOURPI
  implicit none
    ! arguments
    integer, intent(in)   :: ellmax !!
    real, intent(in)      :: v(3) !! vector v(1:3)
    integer, intent(in)   :: derive2i !! derive w.r.t. component v(i)
    ! result
    real                  :: dXlm(1:(ellmax+1)**2)
    ! local vars
    real                  :: r2, x2, y2, z2
    real                  :: r4, x4, y4, z4

#ifdef DEBUG
    if( ellmax < 0 ) stop 'd_Xlmax_rl_dri: ellmax should at least be 0.'
#endif

#define x v(1)
#define y v(2)
#define z v(3)

    ! === abbreviate ==============================================================
    x2 = x*x ; y2 = y*y ; z2 = z*z ; r2 = x2 + y2 + z2
    x4 = x2*x2 ; y4 = y2*y2 ; z4 = z2*z2 ; r4 = r2*r2

    dXlm = 0. ! init

    ! d_Xlmax_rl_dri( 1) =  f*0. ! s-orbital

    if( ellmax < 1 ) return

    selectcase( derive2i )
    case( 1 ) ! d/dX

      dXlm( 2) =  f*sqrt(3.)!*x ! px-orbital

      if( ellmax < 2 ) return
      ! ===  ell = 2  ===============================================================

      dXlm( 5) =  f*sqrt( 3.75)*2.*x !(x*x-y*y)    ! d-orbital
      dXlm( 6) =  f*sqrt(15.  ) * z !z*x              ! d-orbital
      dXlm( 7) =  f*sqrt( 1.25)*(-2.)*x!(3.*z*z-r2)        ! d-orbital
      dXlm( 9) =  f*sqrt(15.  ) * y!x*y              ! d-orbital

      if( ellmax < 3 ) return
      ! ===  ell = 3  ===============================================================
      dXlm(10) =  f*sqrt(  4.375)*3.*(x*x-y*y)!x*(x*x-3.*y*y)  ! f-orbital
      dXlm(11) =  f*sqrt( 26.25 )*2.*z*x!z*(x*x-y*y)     ! f-orbital
      dXlm(12) =  f*sqrt(  2.625)*(5.*z*z-r2-2.*x**2)!x*(5.*z*z-r2)         ! f-orbital
      dXlm(13) =  f*sqrt(  1.75 )*z*(-6.)*x!z*(5.*z*z-3.*r2)      ! f-orbital
      dXlm(14) =  f*sqrt(  2.625)*y*(-2.)*x!y*(5.*z*z-r2)         ! f-orbital
      dXlm(15) =  f*sqrt(105.   )*y*z!x* y*z                ! f-orbital
      dXlm(16) =  f*sqrt(  4.375)*y*6.*x!y*(3.*x*x-y*y)  ! f-orbital

      if( ellmax < 4 ) return
      ! ===  ell = 4  ===============================================================
      dXlm(17) =  f*sqrt( 4.921875)*(4.*x**3-12.*x*y**2)!(x**4-6.*x**2*y**2+y**4)
      dXlm(18) =  f*sqrt(39.375   )*z*3.*(x**2-y**2)!z*x*(x**2-3.*y**2)
      dXlm(19) =  f*sqrt( 2.8125  )*4.*x*(3.*z**2-x**2)!(x**2-y**2)*(7.*z**2-r2)
      dXlm(20) =  f*sqrt( 5.625   )*z*(7.*z**2-3.*r2-6.*x**2)!x*z*(7.*z**2-3.*r2)
      dXlm(21) =  f*sqrt( 0.140625)*(-60.*z**2*x+12.*r2*x)!(35.*z**4-30.*z**2*r2+3.*r2*r2)
      dXlm(22) =  f*sqrt( 5.625   )*y*z*(-6.)*x!y*z*(7.*z**2-3.*r2)
      dXlm(23) =  f*sqrt(11.25    )*y*(7.*z**2-r2-2.*x**2)!x*y*(7.*z**2-r2)
      dXlm(24) = -f*sqrt(39.375   )*z*y*(-6.)*x!z*y*(y**2-3.*x**2)
      dXlm(25) =  f*sqrt(78.75    )*y*(3.*x**2-y**2)!x*y*(x**2-y**2)

    case( 2 ) ! d/dY

      dXlm( 4) =  f*sqrt(3.)!*y ! py-orbital

      if( ellmax < 2 ) return

      dXlm( 5) =  f*sqrt( 3.75)*(-2.)*y !(x*x-y*y)    ! d-orbital
      dXlm( 7) =  f*sqrt( 1.25)*(-2.)*y!(3.*z*z-r2)        ! d-orbital
      dXlm( 8) =  f*sqrt(15.  ) * z!y*z              ! d-orbital
      dXlm( 9) =  f*sqrt(15.  ) * x!x*y              ! d-orbital

      if( ellmax < 3 ) return
      ! ===  ell = 3  ===============================================================
      dXlm(10) =  f*sqrt(  4.375)*x*(-6.)*y!x*(x*x-3.*y*y)  ! f-orbital
      dXlm(11) =  f*sqrt( 26.25 )*z*(-2.)*y!z*(x*x-y*y)     ! f-orbital
      dXlm(12) =  f*sqrt(  2.625)*x*(-2.)*y!x*(5.*z*z-r2)         ! f-orbital
      dXlm(13) =  f*sqrt(  1.75 )*z*(-6.)*y!z*(5.*z*z-3.*r2)      ! f-orbital
      dXlm(14) =  f*sqrt(  2.625)*(5.*z*z-r2-2.*y*y)!y*(5.*z*z-r2)         ! f-orbital
      dXlm(15) =  f*sqrt(105.   )*x*z!x* y*z                ! f-orbital
      dXlm(16) =  f*sqrt(  4.375)*3.*(x*x-y*y)!y*(3.*x*x-y*y)  ! f-orbital

      if( ellmax < 4 ) return
      ! ===  ell = 4  ===============================================================
      dXlm(17) =  f*sqrt( 4.921875)*y*4.*(y**2-3.*x**2)!(x**4-6.*x**2*y**2+y**4)
      dXlm(18) =  f*sqrt(39.375   )*z*x*(-6.)*y!z*x*(x**2-3.*y**2)
      dXlm(19) =  f*sqrt( 2.8125  )*4.*y*(y**2-3.*z**2)!(x**2-y**2)*(7.*z**2-r2)
      dXlm(20) =  f*sqrt( 5.625   )*x*z*(-6.)*y!x*z*(7.*z**2-3.*r2)
      dXlm(21) =  f*sqrt( 0.140625)*y*(-60.*z**2+12.*r2)!(35.*z**4-30.*z**2*r2+3.*r2*r2)
      dXlm(22) =  f*sqrt( 5.625   )*z*(7.*z**2-3.*r2-6.*y**2)!y*z*(7.*z**2-3.*r2)
      dXlm(23) =  f*sqrt(11.25    )*x*(7.*z**2-r2-2.*y**2)!x*y*(7.*z**2-r2)
      dXlm(24) = -f*sqrt(39.375   )*3.*z*(y**2-x**2)!z*y*(y**2-3.*x**2)
      dXlm(25) =  f*sqrt(78.75    )*x*(x**2-3.*y**2)!x*y*(x**2-y**2)

    case( 3 ) ! d/dZ

      dXlm( 3) =  f*sqrt(3.)!*z ! pz-orbital

      if( ellmax < 2 ) return

      dXlm( 6) =  f*sqrt(15.  ) * x !z*x              ! d-orbital
      dXlm( 7) =  f*sqrt( 1.25)*4.*z!(3.*z*z-r2)        ! d-orbital ! bug fixed: prev: 6.*z
      dXlm( 8) =  f*sqrt(15.  ) * y!y*z              ! d-orbital

      if( ellmax < 3 ) return
      ! ===  ell = 3  ===============================================================
    ! dXlm(10) =  f*sqrt(  4.375)*0.!x*(x*x-3.*y*y)  ! f-orbital
      dXlm(11) =  f*sqrt( 26.25 )*(x*x-y*y)!z*(x*x-y*y)     ! f-orbital
      dXlm(12) =  f*sqrt(  2.625)*x*8.*z!x*(5.*z*z-r2)         ! f-orbital
      dXlm(13) =  f*sqrt(  1.75 )*3.*(3.*z*z-r2)!z*(5.*z*z-3.*r2)      ! f-orbital
      dXlm(14) =  f*sqrt(  2.625)*y*8.*z!y*(5.*z*z-r2)         ! f-orbital
      dXlm(15) =  f*sqrt(105.   )*x*y!x* y*z                ! f-orbital
    ! dXlm(16) =  f*sqrt(  4.375)*0.!y*(3.*x*x-y*y)  ! f-orbital

      if( ellmax < 4 ) return
      ! ===  ell = 4  ===============================================================
    ! dXlm(17) =  f*sqrt( 4.921875)*0.!(x**4-6.*x**2*y**2+y**4)
      dXlm(18) =  f*sqrt(39.375   )*x*(x**2-3.*y**2)!z*x*(x**2-3.*y**2)
      dXlm(19) =  f*sqrt( 2.8125  )*(x**2-y**2)*12.*z!(x**2-y**2)*(7.*z**2-r2)
      dXlm(20) =  f*sqrt( 5.625   )*x*(15.*z**2-3.*r2)!x*z*(7.*z**2-3.*r2)
      dXlm(21) =  f*sqrt( 0.140625)*z*(80.*z**2-48.*r2)!(35.*z**4-30.*z**2*r2+3.*r2*r2)
      dXlm(22) =  f*sqrt( 5.625   )*y*(15.*z**2-3.*r2)!y*z*(7.*z**2-3.*r2)
      dXlm(23) =  f*sqrt(11.25    )*x*y*12.*z!x*y*(7.*z**2-r2)
      dXlm(24) = -f*sqrt(39.375   )*y*(y**2-3.*x**2)!z*y*(y**2-3.*x**2)
    ! dXlm(25) =  f*sqrt(78.75    )*0.!x*y*(x**2-y**2)

#ifdef DEBUG
    case default ; stop 'dXlm: DERIV2I out of bounds [1, 3].'
#endif
    endselect ! deriv2i

    if( ellmax < 5 ) return
!     stop 'dXlm: ellmax > 4 not yet implemented'

    dXlm(26:) = 0.
    stop 'd_Xlmax_rl_dri: ellmax larger than implemented (max 4).'

#undef x
#undef y
#undef z
  endfunction d_Xlmax_rl_dri




  function d2_Xlmax_rl_dridri( ellmax, v, derive2i, derive2ii ) result( dXlm )
  ! writes the second derivative of the spherical harmonics
  ! (real combinations) X_{\ell m}(\hat{\vec{v}}) (m=-\ell..\ell) up to a given
  ! ellmax, multiplied with r^ell where r=|\vec{v}|
  ! w.r.t each of its components
  !                   DW 17.4.2012                    PBaum Nov 08
  ! use constants, only: f => ONESQRTFOURPI
  implicit none
    ! arguments
    integer, intent(in)   :: ellmax !!
    real, intent(in)      :: v(3) !! vector v(1:3)
    integer, intent(in)   :: derive2i, derive2ii !! derive w.r.t. component v(i)
    ! return value
    real                  :: dXlm(1:(ellmax+1)**2)

    ! local variables
    real                  :: r2 ! = v(X)**2+v(Y)**2+v(Z)**2
    integer               :: der2i, der2ii
#ifdef DEBUG
    if( ellmax < 0 ) stop 'd_Xlmax_rl_dridri: ellmax should at least be 0.'
#endif

#define x v(1)
#define y v(2)
#define z v(3)

    r2 = x*x + y*y + z*z

    der2i  = min(derive2i,derive2ii)
    der2ii = max(derive2i,derive2ii)
    ! with this, the implementation only needs 6 rather than 9 second derivatives
    ! XX XY XZ
    !    YY YZ
    !       ZZ
#ifdef DEBUG
    if( der2i  < 1 ) stop 'd_Xlmax_rl_dridri: derive2i must be in range [1..3].'
    if( der2ii > 3 ) stop 'd_Xlmax_rl_dridri: derive2ii must be in range [1..3].'
#endif

    dXlm = 0.
    ! d_Xlmax_rl_dri( 1) =  0.!f ! s-orbital
    if( ellmax < 2 ) return

    selectcase( der2i )
    case( 1 ) ! der2i

      selectcase( der2ii )
      case( 1 ) ! der2ii XX
        dXlm( 5) =  f*sqrt( 3.75)*2. !(x*x-y*y)    ! d-orbital
        dXlm( 7) =  f*sqrt( 1.25)*(-2.)!(3.*z*z-r2)        ! d-orbital
      case( 2 ) ! der2ii XY
        dXlm( 9) =  f*sqrt(15.  ) * y!x*y              ! d-orbital
      case( 3 ) ! der2ii XZ
        dXlm( 6) =  f*sqrt(15.  ) * z !z*x              ! d-orbital
      endselect ! der2ii

      if( ellmax < 3 ) return

      selectcase( der2ii )
      case( 1 ) ! der2ii XX
        ! ===  ell = 3  ===============================================================
        dXlm(10) =  f*sqrt(  4.375)*3.*2*(x)!x*(x*x-3.*y*y)  ! f-orbital
        dXlm(11) =  f*sqrt( 26.25 )*2.*z!z*(x*x-y*y)     ! f-orbital
        dXlm(12) =  f*sqrt(  2.625)*(-6.*x)!x*(5.*z*z-r2)         ! f-orbital
        dXlm(13) =  f*sqrt(  1.75 )*z*(-6.)!z*(5.*z*z-3.*r2)      ! f-orbital
        dXlm(14) =  f*sqrt(  2.625)*y*(-2.)!y*(5.*z*z-r2)         ! f-orbital
        dXlm(16) =  f*sqrt(  4.375)*y*6.!y*(3.*x*x-y*y)  ! f-orbital
      case( 2 ) ! der2ii XY
        ! ===  ell = 3  ===============================================================
        dXlm(10) =  f*sqrt(  4.375)*3.*(-2.*y)!x*(x*x-3.*y*y)  ! f-orbital
        dXlm(12) =  f*sqrt(  2.625)*(-2.*y)!x*(5.*z*z-r2)         ! f-orbital
        dXlm(14) =  f*sqrt(  2.625)*(-2.)*x!y*(5.*z*z-r2)         ! f-orbital
        dXlm(15) =  f*sqrt(105.   )*z!x* y*z                ! f-orbital
        dXlm(16) =  f*sqrt(  4.375)*6.*x!y*(3.*x*x-y*y)  ! f-orbital
      case( 3 ) ! der2ii XZ
        ! ===  ell = 3  ===============================================================
        dXlm(11) =  f*sqrt( 26.25 )*2.*x!z*(x*x-y*y)     ! f-orbital
        dXlm(12) =  f*sqrt(  2.625)*(8.*z)!x*(5.*z*z-r2)         ! f-orbital
        dXlm(13) =  f*sqrt(  1.75 )*(-6.)*x!z*(5.*z*z-3.*r2)      ! f-orbital
        dXlm(15) =  f*sqrt(105.   )*y!x* y*z                ! f-orbital
      endselect ! der2ii

      if( ellmax < 4 ) return

      selectcase( der2ii )
      case( 1 ) ! der2ii XX
        ! ===  ell = 4  ===============================================================
        dXlm(17) =  f*sqrt( 4.921875)*(12.*x**2-12.*y**2)!(x**4-6.*x**2*y**2+y**4)
        dXlm(18) =  f*sqrt(39.375   )*z*3.*(2.*x)!z*x*(x**2-3.*y**2)
        dXlm(19) =  f*sqrt( 2.8125  )*4.*(3.*z**2-3.*x**2)!(x**2-y**2)*(7.*z**2-r2)
        dXlm(20) =  f*sqrt( 5.625   )*z*(-18.*x)!x*z*(7.*z**2-3.*r2)
        dXlm(21) =  f*sqrt( 0.140625)*(-60.*z**2+12.*r2+12.*2.*x**2)!(35.*z**4-30.*z**2*r2+3.*r2*r2)
        dXlm(22) =  f*sqrt( 5.625   )*y*z*(-6.)!y*z*(7.*z**2-3.*r2)
        dXlm(23) =  f*sqrt(11.25    )*y*(-6.*x)!x*y*(7.*z**2-r2)
        dXlm(24) = -f*sqrt(39.375   )*z*y*(-6.)!z*y*(y**2-3.*x**2)
        dXlm(25) =  f*sqrt(78.75    )*y*(6.*x)!x*y*(x**2-y**2)
      case( 2 ) ! der2ii XY
        ! ===  ell = 4  ===============================================================
        dXlm(17) =  f*sqrt( 4.921875)*(-24.*x*y)!(x**4-6.*x**2*y**2+y**4)
        dXlm(18) =  f*sqrt(39.375   )*z*6.*(-y)!z*x*(x**2-3.*y**2)
        dXlm(20) =  f*sqrt( 5.625   )*z*(-3.*2.*y)!x*z*(7.*z**2-3.*r2)
        dXlm(21) =  f*sqrt( 0.140625)*(12.*2.*z*x)!(35.*z**4-30.*z**2*r2+3.*r2*r2)
        dXlm(22) =  f*sqrt( 5.625   )*z*(-6.)*x!y*z*(7.*z**2-3.*r2)
        dXlm(23) =  f*sqrt(11.25    )*(7.*z**2-r2-2.*y**2-2.*x**2)!x*y*(7.*z**2-r2)
        dXlm(24) = -f*sqrt(39.375   )*z*(-6.)*x!z*y*(y**2-3.*x**2)
        dXlm(25) =  f*sqrt(78.75    )*(3.*x**2-3.*y**2)!x*y*(x**2-y**2)
      case( 3 ) ! der2ii XZ
        ! ===  ell = 4  ===============================================================
        dXlm(18) =  f*sqrt(39.375   )*3.*(x**2-y**2)!z*x*(x**2-3.*y**2)
        dXlm(19) =  f*sqrt( 2.8125  )*4.*x*(6.*z)!(x**2-y**2)*(7.*z**2-r2)
        dXlm(20) =  f*sqrt( 5.625   )*(12.*z**2-3.*r2-6.*x**2)!x*z*(7.*z**2-3.*r2)
        dXlm(21) =  f*sqrt( 0.140625)*(-60.*z*2.*x+12.*2.*z*x)!(35.*z**4-30.*z**2*r2+3.*r2*r2)
        dXlm(22) =  f*sqrt( 5.625   )*y*(-6.)*x!y*z*(7.*z**2-3.*r2)
        dXlm(23) =  f*sqrt(11.25    )*y*(7.*z*2.-2.*z)!x*y*(7.*z**2-r2)
        dXlm(24) = -f*sqrt(39.375   )*y*(-6.)*x!z*y*(y**2-3.*x**2)
      endselect ! der2ii

    case( 2 ) ! der2i

      selectcase( der2ii )
      case( 2 ) ! der2ii YY
        dXlm( 5) =  f*sqrt( 3.75)*(-2.) !(x*x-y*y)    ! d-orbital
        dXlm( 7) =  f*sqrt( 1.25)*(-2.)!(3.*z*z-r2)        ! d-orbital
      case( 3 ) ! der2ii YZ
        dXlm( 8) =  f*sqrt(15.  )!y*z              ! d-orbital
      endselect ! der2ii

      if( ellmax < 3 ) return

      selectcase( der2ii )
      case( 2 ) ! der2ii YY
        ! ===  ell = 3  ===============================================================
        dXlm(10) =  f*sqrt(  4.375)*x*(-6.)!x*(x*x-3.*y*y)  ! f-orbital
        dXlm(11) =  f*sqrt( 26.25 )*z*(-2.)!z*(x*x-y*y)     ! f-orbital
        dXlm(12) =  f*sqrt(  2.625)*x*(-2.)!x*(5.*z*z-r2)         ! f-orbital
        dXlm(13) =  f*sqrt(  1.75 )*z*(-6.)!z*(5.*z*z-3.*r2)      ! f-orbital
        dXlm(14) =  f*sqrt(  2.625)*(-6.*y)!y*(5.*z*z-r2)         ! f-orbital
        dXlm(16) =  f*sqrt(  4.375)*6.*(-y)!y*(3.*x*x-y*y)  ! f-orbital
      case( 3 ) ! der2ii YZ
        ! ===  ell = 3  ===============================================================
        dXlm(11) =  f*sqrt( 26.25 )*(-2.)*y!z*(x*x-y*y)     ! f-orbital
        dXlm(13) =  f*sqrt(  1.75 )*(-6.)*y!z*(5.*z*z-3.*r2)      ! f-orbital
        dXlm(14) =  f*sqrt(  2.625)*(10.*z-2.*z)!y*(5.*z*z-r2)         ! f-orbital
        dXlm(15) =  f*sqrt(105.   )*x!x* y*z                ! f-orbital
      endselect ! der2ii

      if( ellmax < 4 ) return

      selectcase( der2ii )
      case( 2 ) ! der2ii YY
        ! ===  ell = 4  ===============================================================
        dXlm(17) =  f*sqrt( 4.921875)*4.*(3.*y**2-3.*x**2)!(x**4-6.*x**2*y**2+y**4)
        dXlm(18) =  f*sqrt(39.375   )*z*x*(-6.)!z*x*(x**2-3.*y**2)
        dXlm(19) =  f*sqrt( 2.8125  )*4.*(3.*y**2-3.*z**2)!(x**2-y**2)*(7.*z**2-r2)
        dXlm(20) =  f*sqrt( 5.625   )*x*z*(-6.)!x*z*(7.*z**2-3.*r2)
        dXlm(21) =  f*sqrt( 0.140625)*(-60.*z**2+12.*r2+2.*y**2)!(35.*z**4-30.*z**2*r2+3.*r2*r2)
        dXlm(22) =  f*sqrt( 5.625   )*z*(-18.*y)!y*z*(7.*z**2-3.*r2)
        dXlm(23) =  f*sqrt(11.25)*x*(-6.*y)!x*y*(7.*z**2-r2)
        dXlm(24) = -f*sqrt(39.375   )*3.*z*(y*2.)!z*y*(y**2-3.*x**2)
        dXlm(25) =  f*sqrt(78.75    )*x*(-3.*y*2.)!x*y*(x**2-y**2)
      case( 3 ) ! der2ii YZ
        ! ===  ell = 4  ===============================================================
        dXlm(18) =  f*sqrt(39.375   )*x*(-6.)*y!z*x*(x**2-3.*y**2)
        dXlm(19) =  f*sqrt( 2.8125  )*4.*y*(-3.*z*2.)!(x**2-y**2)*(7.*z**2-r2)
        dXlm(20) =  f*sqrt( 5.625   )*x*(-6.)*y!x*z*(7.*z**2-3.*r2)
        dXlm(21) =  f*sqrt( 0.140625)*y*(-60.*z*2.+24.*z)!(35.*z**4-30.*z**2*r2+3.*r2*r2)
        dXlm(22) =  f*sqrt( 5.625)*(7.*3.*z**2-3.*r2-6.*z**2-6.*y**2)!y*z*(7.*z**2-3.*r2)
        dXlm(23) =  f*sqrt(11.25)*x*(12.*z)!x*y*(7.*z**2-r2)
        dXlm(24) = -f*sqrt(39.375   )*3.*(y**2-x**2)!z*y*(y**2-3.*x**2)
      endselect ! der2ii

    case( 3 ) ! der2i

#ifdef DEBUG
      if( der2ii /= 3 ) stop 'd2Xlmax: fatal error in Schwartz-symmetry.'
#endif

      dXlm( 7) =  f*sqrt( 1.25)*4.!(3.*z*z-r2)        ! d-orbital ! bug fixed: prev: 6.*z

      if( ellmax < 3 ) return
      ! ===  ell = 3  ===============================================================
      dXlm(12) =  f*sqrt(  2.625)*x*8.!x*(5.*z*z-r2)         ! f-orbital
      dXlm(13) =  f*sqrt(  1.75 )*3.*(6.*z-2.*z)!z*(5.*z*z-3.*r2)      ! f-orbital
      dXlm(14) =  f*sqrt(  2.625)*y*8.!y*(5.*z*z-r2)         ! f-orbital

      if( ellmax < 4 ) return
      ! ===  ell = 4  ===============================================================

      dXlm(19) =  f*sqrt( 2.8125  )*(x**2-y**2)*12.!(x**2-y**2)*(7.*z**2-r2)
      dXlm(20) =  f*sqrt( 5.625   )*x*(15.*z*2.-6.*z)!x*z*(7.*z**2-3.*r2)
      dXlm(21) =  f*sqrt( 0.140625)*(80.*3.*z**2-(48.*r2+2.*z*z))!(35.*z**4-30.*z**2*r2+3.*r2*r2)
      dXlm(22) =  f*sqrt( 5.625   )*y*(15.*z*2.-6.*z)!y*z*(7.*z**2-3.*r2)
      dXlm(23) =  f*sqrt(11.25    )*x*y*12.!x*y*(7.*z**2-r2)

#ifdef DEBUG
    case default ; stop 'd2Xlmax: DERIV2I out of bounds [1, 3].'
#endif
    endselect ! der2i

    if( ellmax < 5 ) return
!     stop 'd2_Xlmax_rl_dridri: ellmax > 4 not yet implemented'

    dXlm(26:) = 0.
    ! stop 'd2_Xlmax_rl_dridri: ellmax larger than implemented (max 4).'

#undef x
#undef y
#undef z
  endfunction d2_Xlmax_rl_dridri


  function Ylmax_rl( ellmax, v ) result( Ylm )
  ! generaters the complex spherical harmonics
  ! Y_{\ell m}(\hat{\vec{v}}) (m=-\ell..\ell) up to a given
  ! ellmax, multiplied with r^ell where r=|\vec{v}|
  !
  !                                       PBaum Aug 2008
  implicit none
    ! parameters
    complex, parameter    :: i = (0.,1.)
    ! arguments
    integer, intent(in)   :: ellmax
    real, intent(in)      :: v(3)
    ! return value
    complex               :: Ylm(1:(ellmax+1)**2)
    real                  :: r2 ! v(X)**2+v(Y)**2+v(Z)**2
    real                  :: x, y, z, z2
    complex               :: xpiy, xmiy, xpiy2, xmiy2

    x = v(1) ; y = v(2) ; z = v(3)
    xpiy = (x+i*y)
    xmiy = (x-i*y)

#ifdef DEBUG
    if( ellmax < 0 ) stop 'Ylmax_rl_complex: ellmax should at least be 0.'
#endif

    Ylm( 1) =  f ! s-orbital
    if( ellmax == 0 ) return

    Ylm( 2) =  f*sqrt(1.5)*xmiy ! p-orbital
    Ylm( 3) =  f*sqrt(3. )* z   ! p-orbital
    Ylm( 4) = -f*sqrt(1.5)*xpiy ! p-orbital
    if( ellmax == 1 ) return

    r2 = x*x + y*y + z*z

    Ylm( 5) =  f*sqrt(1.875)*xmiy*xmiy        ! d-orbital
    Ylm( 6) =  f*sqrt(7.5  )*xmiy*z           ! d-orbital
    Ylm( 7) =  f*sqrt(1.25 )*(3.*z**2-r2)     ! d-orbital
    Ylm( 8) = -f*sqrt(7.5  )*xpiy*z           ! d-orbital
    Ylm( 9) =  f*sqrt(1.875)*xpiy*xpiy        ! d-orbital
    if( ellmax == 2 ) return

    xpiy2 = xpiy*xpiy
    xmiy2 = xmiy*xmiy
    z2    = z*z

    Ylm(10) =  f*sqrt( 2.1875)*xmiy2*xmiy         ! f-orbital
    Ylm(11) =  f*sqrt(13.125 )*xmiy2*z            ! f-orbital
    Ylm(12) =  f*sqrt( 1.3125)*xmiy*(5.*z2-r2)    ! f-orbital
    Ylm(13) =  f*sqrt( 1.75  )*z*(5.*z2-3.*r2)    ! f-orbital
    Ylm(14) = -f*sqrt( 1.3125)*xpiy*(5.*z2-r2)    ! f-orbital
    Ylm(15) =  f*sqrt(13.125 )*xpiy2*z            ! f-orbital
    Ylm(16) = -f*sqrt( 2.1875)*xpiy2*xpiy         ! f-orbital
    if( ellmax == 3 ) return

    Ylm(17) =  f*sqrt( 2.4609375)*xmiy2*xmiy2
    Ylm(18) =  f*sqrt(19.6875   )*xmiy2*xmiy*z
    Ylm(19) =  f*sqrt( 1.40625  )*xmiy2*(7.*z2-r2)
    Ylm(20) =  f*sqrt( 2.8125   )*xmiy*z*(7.*z2-3.*r2)
    Ylm(21) =  f*sqrt( 0.140625 )*(35.*z2*z2-30.*z2*r2+3.*r2*r2)
    Ylm(22) = -f*sqrt( 2.8125   )*xpiy*z*(7.*z2-3.*r2)
    Ylm(23) =  f*sqrt( 1.40625  )*xpiy2*(7.*z2-r2)
    Ylm(24) = -f*sqrt(19.6875   )*xpiy2*xpiy*z
    Ylm(25) =  f*sqrt( 2.4609375)*xpiy2*xpiy2
    if( ellmax == 4 ) return

    Ylm(26) =  f*sqrt( 2.70703125)*xmiy2*xmiy2*xmiy
    Ylm(27) =  f*sqrt(27.0703125 )*xmiy2*xmiy2*z
    Ylm(28) =  f*sqrt( 1.50390625)*xmiy2*xmiy*(9.*z2-r2)
    Ylm(29) =  f*sqrt(36.09375   )*xmiy2*z*(3.*z2-r2)
    Ylm(30) =  f*sqrt( 1.2890625 )*xmiy*(21.*z2*z2-14.*z2*r2+r2*r2)
    Ylm(31) =  f*sqrt( 0.171875  )*z*(63.*z2*z2-70.*z2*r2+15.*r2*r2)
    Ylm(32) = -f*sqrt( 1.2890625 )*xpiy*(21.*z2*z2-14.*z2*r2+r2*r2)
    Ylm(33) =  f*sqrt(36.09375   )*xpiy2*z*(3.*z2-r2)
    Ylm(34) = -f*sqrt( 1.50390625)*xpiy2*xpiy*(9.*z2-r2)
    Ylm(35) =  f*sqrt(27.0703125 )*xpiy2*xpiy2*z
    Ylm(36) = -f*sqrt( 2.70703125)*xpiy2*xpiy2*xpiy
    if( ellmax == 5 ) return

    Ylm(37) =  f*sqrt( 2.9326171875)*xmiy2*xmiy2*xmiy2
    Ylm(38) =  f*sqrt(35.19140625  )*xmiy2*xmiy2*xmiy*z
    Ylm(39) =  f*sqrt( 1.599609375 )*xmiy2*xmiy2*(11.*z2-r2)
    Ylm(40) =  f*sqrt( 5.33203125  )*xmiy2*xmiy*z*(11.*z2-3.*r2)
    Ylm(41) =  f*sqrt( 1.3330078125)*xmiy2*(33.*z2*z2-18.*z2*r2+r2*r2)
    Ylm(42) =  f*sqrt( 2.1328125   )*xmiy*z*(33.*z2*z2-30.*z2*r2+5.*r2*r2)
    Ylm(43) =  f*sqrt( 0.05078125  )*(231.*z2*z2*z2-315.*z2*z2*r2+105.*z2*r2*r2-5.*r2*r2*r2)
    Ylm(44) = -f*sqrt( 2.1328125   )*xpiy*z*(33.*z2*z2-30.*z2*r2+5.*r2*r2)
    Ylm(45) =  f*sqrt( 1.3330078125)*xpiy2*(33.*z2*z2-18.*z2*r2+r2*r2)
    Ylm(46) = -f*sqrt( 5.33203125  )*xpiy2*xpiy*z*(11.*z2-3.*r2)
    Ylm(47) =  f*sqrt( 1.599609375 )*xpiy2*xpiy2*(11.*z2-r2)
    Ylm(48) = -f*sqrt(35.19140625  )*xpiy2*xpiy2*xpiy*z
    Ylm(49) =  f*sqrt( 2.9326171875)*xpiy2*xpiy2*xpiy2
    if( ellmax == 6 ) return

    Ylm(50:) = 0.0
    write(*,'(4A)') sym, ' Ylmax_rl: ', 'warning, ellmax > 6 not implemented.'
    stop 'Ylmax_rl: ellmax larger than implemented (max 6).'
  endfunction ! Ylmax_rl



  real function Xlm_rl( ilm, v ) result( Xlm )
  ! writes the spherical harmonic function (real combinations)
  ! Y_{\ell m}(\hat{\vec{v}}) for a given ell#emm-combindex ilm
  ! multiplied with r^ell where r=|\vec{v}|
  !                                       PBaum Aug 2008
  use constants, only: Pi
    integer, intent(in)   :: ilm
    real, intent(in)      :: v(3)

    integer, parameter    :: X=1, Y=2, Z=3
    integer               :: ell
    real                  :: xilm((ELLMAX_IMPLEMENTED+1)**2)
#ifdef DEBUG
    if( ilm < 1 ) stop 'SHX Xlm_rl: ilm < is a not allowed combindex for ell and emm.'
#endif
    ell = 0 ; do while( (ell+1)**2 < ilm ) ; ell = ell+1 ; enddo ! while

    xilm = Xlmax_rl( ell, v )
    Xlm = xilm(ilm)
  endfunction ! Xlm_rl


#ifdef EXTENDED
!+ extended


  function Xl_rl( ell, v ) result( Xlm )
  ! writes the spherical harmonics (real combinations) 
  ! X_{\ell m}(\hat{\vec{v}}) (m=-\ell..\ell) for a given
  ! ell, multiplied with r^ell where r=|\vec{v}|
  ! 
  !                                       PBaum Aug 08
  ! use constants, only: f => ONESQRTFOURPI
  implicit none
    ! arguments
    integer, intent(in)   :: ell
    real, intent(in)      :: v(3)
    ! return value
    real                  :: Xlm(1:2*ell+1)
    real                  :: r2 ! v(X)**2+v(Y)**2+v(Z)**2

#ifdef DEBUG
    if( ell < 0 ) stop 'Xl_rl: ell should at least be 0.'
#endif
    
#define x v(1)
#define y v(2)
#define z v(3)

    r2 = x**2 + y**2 + z**2

    selectcase( ell )
    case( 0 )
      Xlm( 1) =  f ! s-orbital
    case( 1 )
      Xlm( 1) =  f*sqrt(3.)*x ! px-orbital
      Xlm( 2) =  f*sqrt(3.)*z ! pz-orbital
      Xlm( 3) =  f*sqrt(3.)*y ! py-orbital
#ifdef P_ORDER_XYZ
      Xlm( 2) =  f*sqrt(3.)*y ! pz-orbital
      Xlm( 3) =  f*sqrt(3.)*z ! py-orbital
#endif
    case( 2 )
      Xlm( 1) =  f*sqrt( 3.75)*(x*x-y*y)    ! d-orbital !  eg
      Xlm( 2) =  f*sqrt(15.  ) * z*x              ! d-orbital ! t2g
      Xlm( 3) =  f*sqrt( 1.25)*(3.*z*z-r2)        ! d-orbital !  eg
      Xlm( 4) =  f*sqrt(15.  ) * y*z              ! d-orbital ! t2g
      Xlm( 5) =  f*sqrt(15.  ) * x*y              ! d-orbital ! t2g
    case( 3 )
      Xlm( 1) =  f*sqrt(  4.375)*x*(x*x-3.*y*y)  ! f-orbital
      Xlm( 2) =  f*sqrt( 26.25 )*z*(x*x-y*y)     ! f-orbital
      Xlm( 3) =  f*sqrt(  2.625)*x*(5.*z*z-r2)         ! f-orbital
      Xlm( 4) =  f*sqrt(  1.75 )*z*(5.*z*z-3.*r2)      ! f-orbital
      Xlm( 5) =  f*sqrt(  2.625)*y*(5.*z*z-r2)         ! f-orbital
      Xlm( 6) =  f*sqrt(105.   )*x* y*z                ! f-orbital
      Xlm( 7) =  f*sqrt(  4.375)*y*(3.*x*x-y*y)  ! f-orbital
    case( 4 )
      Xlm( 1) =  f*sqrt( 4.921875)*(x**4-6.*x**2*y**2+y**4)
      Xlm( 2) =  f*sqrt(39.375   )*z*x*(x**2-3.*y**2)
      Xlm( 3) =  f*sqrt( 2.8125  )*(x**2-y**2)*(7.*z**2-r2)
      Xlm( 4) =  f*sqrt( 5.625   )*x*z*(7.*z**2-3.*r2)
      Xlm( 5) =  f*sqrt( 0.140625)*(35.*z**4-30.*z**2*r2+3.*r2*r2)
      Xlm( 6) =  f*sqrt( 5.625   )*y*z*(7.*z**2-3.*r2)
      Xlm( 7) =  f*sqrt(11.25    )*x*y*(7.*z**2-r2)
      Xlm( 8) = -f*sqrt(39.375   )*z*y*(y**2-3.*x**2)
      Xlm( 9) =  f*sqrt(78.75    )*x*y*(x**2-y**2)
    case( 5 )
      Xlm( 1) =  f*sqrt(  5.4140625)*x*(x**4-10.*x**2*y**2+5.*y**4)
      Xlm( 2) =  f*sqrt( 54.140625 )*(x**4-6.*x**2*y**2+y**4)*z
      Xlm( 3) =  f*sqrt(  3.0078125)*x*(x**2-3.*y**2)*(9.*z**2-r2)
      Xlm( 4) =  f*sqrt( 72.1875   )*(x**2-y**2)*z*(3.*z**2-r2)
      Xlm( 5) =  f*sqrt(  2.578125 )*x*(21.*z**4-14.*z**2*r2+r2*r2)
      Xlm( 6) =  f*sqrt( 0.171875  )*z*(63.*z**4-70.*z**2*r2+15.*r2*r2)
      Xlm( 7) =  f*sqrt(  2.578125 )*y*(21.*z**4-14.*z**2*r2+r2*r2)
      Xlm( 8) =  f*sqrt(288.75     )*y*x*z*(3.*z**2-r2)
      Xlm( 9) = -f*sqrt(  3.0078125)*y*(y**2-3.*x**2)*(9.*z**2-r2)
      Xlm(10) = -f*sqrt(866.25     )*y*x*(y**2-x**2)*z
      Xlm(11) =  f*sqrt(  5.4140625)*y*(y**4-10.*y**2*x**2+5.*x**4)
    case( 6 )
      Xlm( 1) =  f*sqrt( 5.865234375)*(x**6-15.*x**4*y**2+15.*x**2*y**4-y**6)
      Xlm( 2) =  f*sqrt(70.3828125  )*x*(x**4-10.*x**2*y**2+5.*y**4) * z
      Xlm( 3) =  f*sqrt( 3.19921875 )*(x**4-6.*x**2*y**2+y**4) * (11.*z**2-r2)
      Xlm( 4) =  f*sqrt(10.6640625  )*x*(x**2-3.*y**2) * z*(11.*z**2-3.*r2)
      Xlm( 5) =  f*sqrt( 2.666015625)*(x**2-y**2) * (33.*z**4-18.*z**2*r2+r2*r2)
      Xlm( 6) =  f*sqrt( 4.265625   )*x * z*(33.*z**4-30.*z**2*r2+5.*r2*r2)
      Xlm( 7) =  f*sqrt( 0.05078125 )*(231.*z**6-315.*z**4*r2+105.*z**2*r2*r2-5.*r2*r2*r2)
      Xlm( 8) =  f*sqrt( 4.265625   )*y * z*(33.*z**4-30.*z**2*r2+5.*r2*r2)
      Xlm( 9) =  f*sqrt(10.6640625  )*y*x * (33.*z**4-18.*z**2*r2+r2*r2)
      Xlm(10) = -f*sqrt(10.6640625  )*y*(y**2-3.*x**2) * z*(11.*z**2-3.*r2)
      Xlm(11) = -f*sqrt(51.1875     )*y*x*(y**2-x**2) * (11.*z**2-r2)
      Xlm(12) =  f*sqrt(70.3828125  )*y*(y**4-10.*y**2*x**2+5.*x**4) * z
      Xlm(13) =  f*sqrt(23.4609375  )*(3.*x*y**5-10.*x**3*y**3+3.*x**5*y)
    case default
      Xlm   = 0.0
      write(*,'(9A)') sym, ' Xl_rl: ', 'warning, ell out of [0,6]'
    endselect ! ell

#undef x
#undef y
#undef z
  endfunction ! Xl_rl



  real elemental function GAMMA_function( two_x )
  ! the Gamma function: Watch out (integer) two_x = 2*x
  !                 /  x integer        (x-1)!  [ ! = integer factorial]
  ! Gamma( 2*x ) = <
  !                 \  x halfinteger    sqrt(Pi)*2^(-2x+1) (2x-1)!
  !
  !                                  PBaum Aug 2008
  use constants, only: sqrtPi
  implicit none
    ! arguments
    integer, intent(in) :: two_x ! argument
    ! local vars
    integer         :: n, nfac, i

!     if( two_x < 2 ) stop 'GAMMAFUNCTION: too small argument.' ! stop not possible in elemental func
    GAMMA_function = 0.
    if( two_x < 2 ) return

    n = int(two_x/2) ! round off

    if( modulo( two_x, 2 ) == 0 ) then
      ! gamma( n ) = (n-1)! (factorial)
      ! two_x = 2*n
      nfac = 1
      do i=1, n-1
        nfac = nfac*i
      enddo ! i
      GAMMA_function = real(nfac)
    else  ! modulo 2
      ! gamma( n + 1/2 ) = sqrt(pi)*2^(-2n-1)*(2n-1)! / (n-1)!
      nfac = 1
      do i=n, 2*n-1
        nfac = nfac*i
      enddo ! i
      GAMMA_function = sqrtPi*0.5**(2*n-1)*real(nfac)
    endif ! modulo 2

  endfunction ! GAMMA_function



  real function bessel_j( ell, r ) result( j )
  ! highly optimized function for the
  ! Bessel -j(r) functions computed by
  ! the power series expansion around
  ! the origin (r=0.0)
  !                   PBaum Aug 2008
  implicit none
    ! parameter
    real, parameter           :: ACCURACY = 1.E-16
    ! arguments
    integer, intent(in)       :: ell
    real, intent(in)          :: r
    ! local vars
    real                      :: mr2, db
    integer                   :: m, lpm

    ! uncommented !!!
    mr2 = - r * r
    lpm = 1
    db  = 1.
    do m = 1, ell
      lpm = lpm * 2 * m
      db  = db * r
    enddo ! m

    db = db / real(lpm)
    j = db
    m = 1
    lpm = ell + 1 ! == ell + m
    do while( abs(db) > ACCURACY )
      db = db * mr2 / real( 4 * m * lpm )
      j = j + db
      m = m + 1
      lpm = lpm + 1 ! == ell + m
    enddo ! while

    stop 'besselj by power series produces wrong results!'
  endfunction ! bessel_j


  subroutine test_bessel( )
    integer       :: i, ell
    real          :: r
    do i=0, 600
      r = i*0.01
      write(19,'(6ES24.16)') r, ( bessel_j(ell,r), ell=0,4 )
    enddo ! i
  endsubroutine ! test_bessel


  subroutine binominalcoefficients( N, bnc )
  !                             PBaum Aug 08
    integer, intent(in)             :: N
    integer, intent(out)            :: bnc(0:N)

    integer :: k, i
    bnc(1:N) = 0
    bnc(0) = 1
    do i=1, N
      do k=i, 1, -1
        bnc(k) = bnc(k-1) + bnc(k)
      enddo ! k
    enddo ! i

  endsubroutine ! binominalcoefficients



  function Ylm2Xlm( ell, dagger ) result( u ) ! <Y_{complex}(\ell m)|Y_{real}(\ell \mu)>
  ! use constants, only: SQRTHALF
  implicit none
    ! parameters
    character(len=*), parameter     :: fun = ' Y2X: '
#ifdef DEBUG
    integer, parameter              :: o = 6
#else
    integer, parameter              :: o = 0
#endif
    ! real, parameter                 :: s  =  ! sqrt(0.5)
#define SQRTHALF 0.7071067811865475244
    complex, parameter              :: R  = (SQRTHALF,0.0)
    complex, parameter              :: I  = (0.0,SQRTHALF)
    complex, parameter              :: ON = (1.0,0.0)
    complex, parameter              :: z  = (0.0,0.0)
    ! arguments
    integer, intent(in)             :: ell
    logical, intent(in), optional   :: dagger
    ! result
    complex             :: u(-ell:ell,-ell:ell)
    ! local vars
    integer             :: m
    logical             :: dag
    dag = .false. ; if( present( dagger ) ) dag = dagger

    u = z ! zero
    selectcase( ell )
    case( :-1 )
      if(o>0) write(o,'(3A,I3,A)') sym, fun, 'ell < 0 unphysical, ell =', ell
    !----------------------------------------------------------
    case( 0:ELLMAX_IMPLEMENTED )
      u(0,0) = ON ! m=0 ---> mu=0 with coeff 1.000
      if( .not. dag ) then
        ! regular
        do m = 1, ell
            u(-m,-m)     =  R
            u(-m,+m)     =  I
            u(+m,-m)     = (-1.)**m*R
            u(+m,+m)     = (-1.)**m*(-I)
        enddo ! m
      else  ! dagger
        ! adjoint
        do m = 1, ell
            ! 1st and 2nd index interchanged
            u(-m,-m)     =  R
            u(+m,-m)     =  -I ! complex conjugate
            u(-m,+m)     = (-1.)**m*R
            u(+m,+m)     = (-1.)**m*I ! complex conjugate
        enddo ! m
      endif ! dagger or no dagger
#ifdef DEBUG
    !----------------------------------------------------------
    case( ELLMAX_IMPLEMENTED+1: )
      if(o>0) write(o,'(3A,I2,A,I2)') sym, fun, &
        'ell too large, ell =', ell, ' maximum is ', ELLMAX_IMPLEMENTED+1
#endif
    endselect ! ilm

  endfunction ! Ylm2Xlm



  function c2r( ell ) result( u ) ! <Y_{complex}(\ell m)|Y_{real}(\ell \mu)>
#ifdef DEBUG
  use configuration, only: o
#endif
  ! use constants, only: SQRTHALF
  implicit none
    ! parameters
    character(len=*), parameter           :: fun = ' c2r: '
    ! real, parameter                       :: s  = SQRTHALF ! sqrt(0.5) 
#define SQRTHALF 0.7071067811865475244
    complex, parameter                    :: R  = (SQRTHALF,0.0)
    complex, parameter                    :: I  = (0.0,SQRTHALF)
    complex, parameter                    :: ON = (1.0,0.0)
    complex, parameter                    :: z  = (0.0,0.0)
    ! arguments
    integer, intent(in) :: ell
    ! result
    complex             :: u(2*ell+1,-ell:ell)
    ! local vars
    integer             :: m

    u = z ! zero
    selectcase( ell )
    case( :-1 ) ; stop 'SHX: c2r: ELL < 0 unphysical.'
    !----------------------------------------------------------
    case( 1 )  ! u(1:3,-1:1)
      u(3, 0) = ON ! m=0 ---> mu=0 with coeff 1.000
      u(1,-1) =  R
      u(2,+1) =  I
      u(2,-1) = (-1.)**m*R
      u(1,+1) = (-1.)**m*(-I)
    case( 0, 2:ELLMAX_IMPLEMENTED )
      u(ell+1+0,0) = ON ! m=0 ---> mu=0 with coeff 1.000
      do m = 1, ell
        u(ell+1-m,-m)     =  R
        u(ell+1-m,+m)     =  I
        u(ell+1+m,-m)     = (-1.)**m*R
        u(ell+1+m,+m)     = (-1.)**m*(-I)
      enddo ! m
#ifdef DEBUG
    !----------------------------------------------------------
    case( ELLMAX_IMPLEMENTED+1: )
      if(o>0) write(o,'(3A,I2,A,I2)') sym, fun, &
        'ell too large, ell =', ell, ' maximum is ', ELLMAX_IMPLEMENTED+1
#endif
    endselect ! ilm
  endfunction ! c2r


  function r2c( ell ) result( u ) ! <Y_{complex}(\ell m)|Y_{real}(\ell \mu)>
#ifdef DEBUG
  use configuration, only: o
#endif
  ! use constants, only: SQRTHALF
  implicit none
    ! parameters
    character(len=*), parameter           :: fun = ' r2c: '
    ! real, parameter                       :: s  = SQRTHALF ! sqrt(0.5)
#define SQRTHALF 0.7071067811865475244
    complex, parameter                    :: R  = (SQRTHALF,0.0)
    complex, parameter                    :: I  = (0.0,SQRTHALF)
    complex, parameter                    :: ON = (1.0,0.0)
    complex, parameter                    :: z  = (0.0,0.0)
    ! arguments
    integer, intent(in) :: ell
    ! result
    complex             :: u(-ell:ell,2*ell+1)
    ! local vars
    integer             :: m

    u = z
    selectcase( ell )
    case( :-1 ) ; stop 'SHX: r2c: ELL < 0 unphysical.'
    !----------------------------------------------------------
    case( 0:ELLMAX_IMPLEMENTED )
      u(0,ell+1+0) = ON ! m=0 ---> mu=0 with coeff 1.000
      do m = 1, ell
        u(-m,ell+1-m)     =  R
        u(+m,ell+1-m)     =  -I ! complex conjugate
        u(-m,ell+1+m)     = (-1.)**m*R
        u(+m,ell+1+m)     = (-1.)**m*I ! complex conjugate
      enddo ! m
#ifdef DEBUG
    !----------------------------------------------------------
    case( ELLMAX_IMPLEMENTED+1: )
      if(o>0) write(o,'(3A,I2,A,I2)') sym, fun, &
        'ell too large, ell =', ell, ' maximum is ', ELLMAX_IMPLEMENTED+1
#endif
    endselect ! ilm
  endfunction ! r2c


  function dagger_r( a ) result( ad )
    real, intent(in)            :: a(:,:)
    real                        :: ad(size(a,2),size(a,1)) ! result
#ifdef DEBUG
    if( size(a,1) /= size(a,2) ) stop 'SHX dagger: operator is not square'
#endif
    ad = transpose( a )
  endfunction ! dagger


  function dagger_c( a ) result( ad )
    complex, intent(in)         :: a(:,:)
    complex                     :: ad(size(a,2),size(a,1)) ! result
#ifdef DEBUG
    if( size(a,1) /= size(a,2) ) stop 'SHX dagger: operator is not square'
#endif
    ad = conjg(transpose( a ))
  endfunction ! dagger

  !! writes the expressions for spherical harmonics (real combinations) 
  character(len=12) function Xlm_rl_name( ilm ) result( n )
    integer, intent(in)   :: ilm

    selectcase( ilm )
#ifdef DEBUG
    case( :0 ) ; stop 'Xlm_rl_name: ILM should at least be 1'
#endif
    case( 1) ; n = 'S_1         ' !  =  f ! s-orbital

    case( 2) ; n = 'P_x         ' !  =  f*sqrt(3.)*v(X) ! px-orbital
    case( 3) ; n = 'P_z         ' !  =  f*sqrt(3.)*v(Z) ! pz-orbital
    case( 4) ; n = 'P_y         ' !  =  f*sqrt(3.)*v(Y) ! py-orbital

    case( 5) ; n = 'D_x2-y2     ' !  =  f*sqrt( 3.75)*(v(X)*v(X)-v(Y)*v(Y))    ! d-orbital !  eg
    case( 6) ; n = 'D_zx        ' !  =  f*sqrt(15.  ) * v(Z)*v(X)              ! d-orbital ! t2g
    case( 7) ; n = 'D_3z2-r2    ' !  =  f*sqrt( 1.25)*(3.*v(Z)*v(Z)-r2)        ! d-orbital !  eg
    case( 8) ; n = 'D_yz        ' !  =  f*sqrt(15.  ) * v(Y)*v(Z)              ! d-orbital ! t2g
    case( 9) ; n = 'D_xy        ' !  =  f*sqrt(15.  ) * v(X)*v(Y)              ! d-orbital ! t2g

    case(10) ; n = 'F_x(x2-3y2) ' !  =  f*sqrt(  4.375)*v(X)*(v(X)*v(X)-3.*v(Y)*v(Y))  ! f-orbital
    case(11) ; n = 'F_z(x2-y2)  ' !  =  f*sqrt( 26.25 )*v(Z)*(v(X)*v(X)-v(Y)*v(Y))     ! f-orbital
    case(12) ; n = 'F_x(5z2-r2) ' !  =  f*sqrt(  2.625)*v(X)*(5.*v(Z)*v(Z)-r2)         ! f-orbital
    case(13) ; n = 'F_z(5z2-3r2)' !  =  f*sqrt(  1.75 )*v(Z)*(5.*v(Z)*v(Z)-3.*r2)      ! f-orbital
    case(14) ; n = 'F_y(5z2-r2) ' !  = -f*sqrt(  2.625)*v(Y)*(5.*v(Z)*v(Z)-r2)         ! f-orbital
    case(15) ; n = 'F_xyz       ' !  = -f*sqrt(105.   )*v(X)* v(Y)*v(Z)                ! f-orbital
    case(16) ; n = 'F_y(3x2-y2) ' !  = -f*sqrt(  4.375)*v(Y)*(3.*v(X)*v(X)-v(Y)*v(Y))  ! f-orbital
#ifdef DEBUG
    case( 17: ) ; write(*,'(9A)') sym, ' ilm > 16 not implemented'
#endif
    case default ; n = ' '
    endselect ! ilm

  endfunction ! Xlm_rl_name

  !! writes the expressions for spherical harmonics (real combinations) 
  character(len=5) function Xlm_name( ilm ) result( n )
    integer, intent(in)   :: ilm

    selectcase( ilm )
#ifdef DEBUG
    case( :0 ) ; stop 'Xlm_name: ILM should at least be 1'
#else
    case( :0 ) ; n = 'ilm<1'
#endif
    case( 1) ; n = 's    ' !  =  f ! s-orbital

    case( 2) ; n = 'px   ' !  =  f*sqrt(3.)*v(X) ! px-orbital
    case( 3) ; n = 'pz   ' !  =  f*sqrt(3.)*v(Z) ! pz-orbital
    case( 4) ; n = 'py   ' !  =  f*sqrt(3.)*v(Y) ! py-orbital

    case( 5) ; n = 'dx2y2' !  =  f*sqrt( 3.75)*(v(X)*v(X)-v(Y)*v(Y))    ! d-orbital !  eg
    case( 6) ; n = 'dzx  ' !  =  f*sqrt(15.  ) * v(Z)*v(X)              ! d-orbital ! t2g
    case( 7) ; n = 'd3z2 ' !  =  f*sqrt( 1.25)*(3.*v(Z)*v(Z)-r2)        ! d-orbital !  eg
    case( 8) ; n = 'dyz  ' !  =  f*sqrt(15.  ) * v(Y)*v(Z)              ! d-orbital ! t2g
    case( 9) ; n = 'dxy  ' !  =  f*sqrt(15.  ) * v(X)*v(Y)              ! d-orbital ! t2g
    case( 17: ) ; write(n,'(I4)') ilm
    case default ; n = 'error'
    endselect ! ilm

  endfunction ! Xlm_name


  !! self test for the derived spherical harmonics comparing
  !! to a numerical 1st derivative of the spherical harmonics
  status_t function test_derivative( ) result( ist )
    character(len=*), parameter     :: fun = ' test_derivative: '
    real, parameter                 :: h = 0.5**8 ! step width for numerical derivatives
    iounit_t, parameter             :: o = 6 ! output to stdout
    integer, parameter              :: lm = 4

    real, parameter                 :: THRESHOLD = 1.E-4
    integer, parameter              :: Nsamp = 3
    real, parameter                 :: Psamp(-Nsamp:Nsamp) = (/-4.,-1.,-.5,0.,.5,1.,4./)
!     real, parameter                 :: THRESHOLD = 1.E-5
!     integer, parameter              :: Nsamp = 1
!     real, parameter                 :: Psamp(-Nsamp:Nsamp) = (/-1.,0.,1./)
    real    :: v(1:3), dv(3), mxThres
    real    :: XlmD(1:(lm+1)**2), dXlm(1:(lm+1)**2)
    integer :: id, ilm, i1, i2, i3, nerr(1:3)

    mxThres = 0. ! init
    do id = 1, 3 ! for all 3 spatial direction
      nerr(id) = 0 ! init
      dv = 0. ; dv(id) = h/2 ! move half a step in the direction of id and opposite
!       if(o>0) write(o,'(A)') '', fun, 'start next direction', '========================', ''
      do i3 = -Nsamp, Nsamp     ; v(3) = Psamp(i3)
        do i2 = -Nsamp, Nsamp   ; v(2) = Psamp(i2)
          do i1 = -Nsamp, Nsamp ; v(1) = Psamp(i1)

            XlmD = ( Xlmax_rl( lm, v+dv ) - Xlmax_rl( lm, v-dv ) )/h ! eval finite difference derivative
            dXlm = d_Xlmax_rl_dri( lm, v, id ) ! eval analytically derived function

            mxThres = max(mxThres, maxval(abs( dXlm - Xlmd )))
            if( any( abs( dXlm - Xlmd ) > THRESHOLD ) ) then
              do ilm = 1, (lm+1)**2
                if( abs( dXlm(ilm) - Xlmd(ilm) ) > THRESHOLD ) then
                  nerr(id) = nerr(id)+1
                  if(o>0) write(o,'(2(A,I3),2(A,F10.6),A,3F7.3)') 'id =', id, '  ilm =', ilm, '  dXlm =', dXlm(ilm), '  XlmD =', XlmD(ilm), '  v =', v
                endif ! deviates
              enddo ! ilm
            endif ! deviation

          enddo ! i1
        enddo ! i2
      enddo ! i3
    enddo ! id

    if( any( nerr > 0 ) ) then
      do id = 1, 3
        write(*,'(3A,9(I0,A))') sym, fun, ' id =', id, '   errors', nerr(id) , ' /', (2*Nsamp+1)**3*(lm+1)**2
      enddo ! id
      stop 'SHX test: FAILED, errors detected.'
      ist = sum(nerr(1:3))
    else  ! errors
      if(o>0) write(o,'(3A,ES10.2)') sym, fun, 'OK, max dev =', mxThres
    endif ! errors

  endfunction ! test_derivative


  status_t function test( )
    test = test_derivative( )
  endfunction

!- extended
#endif

endmodule ! harmonics

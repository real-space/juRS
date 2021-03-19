#include "config.h"

#define DEBUG

! #define EXPONENTIAL
! #define RECIPROCAL
! #define EQUIDISTANT

!! @author Paul Baumeister
!! @version 3.9
!!
!! radial grid descriptor
!! at run time you may choose from 6 different grid equations
!!
module type_rgrid
  use configuration, only: o
implicit none
  private !! default fot this module namespace

  ! public
  public :: rgrid
  public :: rgrid_create_exp
  public :: rgrid_eqn
  !public :: set
  public :: operator(.at.)
  public :: to_string
  public :: rgrideq2string
#ifdef EXTENDED
  public :: test
#endif

  type :: rgrid
    real          :: rmx = 0.      ! max radius
    integer       :: imx = 0       ! max index, ==N-1
    real          :: a   = 0.      ! prefactor
    real          :: d   = 0.      ! anisotropy
    real, pointer ::    r(:) !  r(0:imx)
    real, pointer ::   dr(:) ! dr(0:imx)
    real, pointer :: r2dr(:) ! r**2*dr(0:imx)
    integer       :: keq = 0 ! key for the equation
  endtype ! rgrid

  interface operator(.at.) ! real = rgrid .at. integer, integer = rgrid .at. real
    module procedure find_radius, find_index
  endinterface

  !interface set
  !  module procedure rgrid_use
  !endinterface
  
  interface to_string
    module procedure rgrid2string
  endinterface


  ! keys                                      equation            params
  integer, parameter, private :: K_EXP0 = 0 ! r=a*exp(d*i)        a and d
  integer, parameter, private :: K_REC1 = 2 ! r=a*i/(1-b*i)       a and b  ==> b=g%d
  integer, parameter, private :: K_REC0 = 3 ! r=a*i/(n-i)         a and n
  integer, parameter, private :: K_EXP1 = 1 ! r=a*(exp(d*i)-1)    a and d
  integer, parameter, private :: K_EQUI = 4 ! r=d*i                 d
  integer, parameter, private :: K_P5a4 = 5 ! r=(i/n+a)^5/a-a^4   a and n

!   real, parameter    :: DEFAULT_RMX = 18.897259886 ! 10 Ang
  real, parameter    :: DEFAULT_RMX = 9.448629943 ! 5.0 Ang
  real, parameter    :: MINIMUM_RMX = .9448629943 ! 0.5 Ang

  integer, parameter :: DEFAULT_IMX = 6000
  integer, parameter :: MINIMUM_IMX = 20

  contains

  string_t function rgrideq2string( g ) result( eq )
    type(rgrid), intent(in) :: g
    selectcase( g%keq )
    case( K_EXP0 ) ; eq = 'r=a*exp(d*i)'
    case( K_REC1 ) ; eq = 'r=a*i/(1-d*i)'
    case( K_REC0 ) ; eq = 'r=a*i/(n-i)'
    case( K_EXP1 ) ; eq = 'r=a*(exp(d*i)-1)'
    case( K_EQUI ) ; eq = 'r=d*i'
    case( K_P5a4 ) ; eq = 'r=(i/n+a)^5/a-a^4'
    case default   ; eq = ''
    endselect ! keq
  endfunction rgrideq2string

  character(len=128) function rgrid2string( g ) result( str )
    type(rgrid), intent(in)   :: g

    status_t :: ios
    string_t :: eq ! equation
    eq = rgrideq2string(g)
    write( unit=str, fmt='(A,F0.3,A,I0,2(A,ES10.3),A,I0,A,A18)', IOstat=ios ) &
      'rgrid rmx=',g%rmx,' imx=', g%imx,' a=',g%a,' d=', g%d,' keq=',g%keq,' ',trim(eq)
  endfunction rgrid2string


  type(rgrid) function rgrid_create_exp( n, rmx, ani ) result( gr )
    integer, intent(in)             :: n
    real, intent(in), optional      :: rmx
    real, intent(in), optional      :: ani

    character(len=*), parameter     :: fun  = ' rgrid_exp: '
    real, parameter                 :: MINIMUM_ANI = 1.E-4
    real, parameter                 :: DEFAULT_ANI = 1.5E-2
    real, parameter                 :: MAXIMUM_ANI = 1.E-1
    status_t                        :: ist
    integer                         :: i, ir
    real                            :: r, a, d, edi

    r = DEFAULT_RMX
    if( present( rmx ) ) r = max( abs(rmx), MINIMUM_RMX )

    i = max( abs(n-1), MINIMUM_IMX )

    d = DEFAULT_ANI
    if( present( ani ) ) d = max( abs(ani), MINIMUM_ANI )
                         d = min(        d, MAXIMUM_ANI )

! #define USE_K_EXP0

    ! normalization prefactor
#ifdef USE_K_EXP0
    a = r / exp(d*real(i)) ! K_EXP0
#else
    a = r / ( exp(d*real(i)) - 1. ) ! K_EXP1
#endif

    ! store grid parameters
    gr%a   = a
    gr%d   = d
    gr%imx = i
    gr%rmx = r

    if(o>0) write(o,'(2A,F0.3,A,I0,2(A,ES10.3),A,I0,9A)') __FILE__, &
      ' rmx=',gr%rmx,' imx=', gr%imx,' a=',gr%a,' d=', gr%d,' keq=',K_EXP1

    !write(6,'(3A,F0.3,A,I0,2(A,ES10.3),A,I0,9A)') sym, fun, &
    !  'rmx=',gr%rmx,' imx=', gr%imx,' a=',gr%a,' d=', gr%d,' keq=',K_EXP1

    allocate( gr%r(0:i), gr%dr(0:i), gr%r2dr(0:i), stat=ist )
    if( ist /= 0 ) stop 'rgrid_set: allocation of gr%R, gr%DR and/or gr%R2DR failed.'

#ifdef USE_K_EXP0
    ! determine radial mesh and derivative
    do ir = 0, i
      edi = exp(d*ir)
      gr%r(ir) = a*edi
      gr%dr(ir) = d*gr%r(ir)
      gr%r2dr(ir) = gr%r(ir)*gr%r(ir)*gr%dr(ir)
    enddo ! ir
    gr%keq = K_EXP0
#else
    ! determine radial mesh and derivative
    do ir = 0, i
      edi = exp(d*ir)
      gr%r(ir) = a*( edi - 1. )
      gr%dr(ir) = a*d*edi
      gr%r2dr(ir) = gr%r(ir)*gr%r(ir)*gr%dr(ir)
    enddo ! ir
    gr%keq = K_EXP1
#endif
    if(o>0) write(o,'(2A,ES12.3,A,F7.3)',iostat=ist) &
      __FILE__,'  r(0)  =', gr%r(0),'   r(max)  =', gr%r(gr%imx), &
      __FILE__,' dr(0)  =',gr%dr(0),'  dr(max)  =',gr%dr(gr%imx)
  endfunction ! rgrid_create_exp


  type(rgrid) function rgrid_eqn( eqn, n, a, d, b ) result( gr )
    character(len=*), intent(in)    :: eqn
    integer, intent(in)             :: n
    real, intent(in)                :: a, d, b

    status_t                        :: ist
    integer                         :: i, is, ik
    real                            :: edi, den ! temp

    gr%a   = a
    gr%d   = d
    gr%imx = n-1
    i      = n-1
    allocate( gr%r(0:i), gr%dr(0:i), gr%r2dr(0:i), stat=ist )
    if( ist /= 0 ) stop 'rgrid_set: allocation of gr%R, gr%DR and/or gr%R2DR failed.'

    ! determine radial mesh and derivative

    is = 0
    ik = 0
    ! initalize
    selectcase( eqn ) ! equation
    case( 'r=a*exp(d*i)' )      ; ik = K_EXP1
           if( a <= 0. .or. d <= 0. ) is = -1
    case( 'r=a*(exp(d*i)-1)' )  ; ik = K_EXP0
           if( a <= 0. .or. d <= 0. ) is = -1
    case( 'r=a*i/(1-b*i)' )     ; ik = K_REC1
           if( a <= 0. .or. b <= 0. ) is = -1
           gr%d = b ! renamed
    case( 'r=a*i/(n-i)' )       ; ik = K_REC0
           if( a <= 0. .or. n < 1 ) is = -1
    case( 'r=(i/n+a)^5/a-a^4' ) ; ik = K_P5a4
           if( a <= 0. .or. n < 1 ) is = -1
    case( 'r=d*i' )             ; ik = K_EQUI
           if( d <= 0. ) is = -1
    case default ; stop 'tRGrid: eqn is unknown!'
    endselect ! eqn
    if( is == -1 ) stop 'tRGrid: some grid parameter is not in the right value range!'
    gr%keq = ik

    do i = 0, gr%imx
      selectcase( gr%keq )
      case( K_EXP1 ) ! r=a*(exp(d*i)-1)
        edi = exp(d*real(i))
        gr%r (i) = a*(edi-1.)
        gr%dr(i) = a*d*edi
      case( K_EXP0 ) ! r=a*exp(d*i)
        edi = exp(d*real(i))
        gr%r (i) = a*edi
        gr%dr(i) = a*d*edi
      case( K_REC0 ) ! r=a*i/(n-i)
        den = real(n-i)
        gr%r (i) = a*i/den    
        gr%dr(i) = ((gr%r(i)+a)**2.0)/(a*real(n)) !a*n/den**2
      case( K_REC1 ) ! r=a*i/(1-b*i)
        den = (1.-b*i)
        gr%r (i) = a*i/den
        gr%dr(i) = a/den**2
      case( K_EQUI ) ! r=d*i
        gr%r (i) = d*i
        gr%dr(i) = d
      case( K_P5a4 ) ! r=(i/n+a)^5/a-a^4
        gr%r (i) = (i/real(n)+a)**5/a-a**4
        gr%dr(i) = 5*(i/real(n)+a)**4/real(n)
      case default ; stop 'tRGrid: keq has an undetermined value!'
      endselect ! keq
    enddo ! ir

    gr%r2dr = gr%r*gr%r*gr%dr
    gr%rmx = gr%r(gr%imx)
    if(o>0) write(o,'(2A,ES12.3,A,F7.3)',iostat=ist) &
      __FILE__,'  r(0)  =', gr%r(0),'   r(max)  =',gr%r(gr%imx), &
      __FILE__,' dr(0)  =',gr%dr(0),'  dr(max)  =',gr%dr(gr%imx)
  endfunction ! rgrid_eqn


  type(rgrid) function rgrid_use( r, dr ) result( gr )
    real, intent(in)                :: r(0:), dr(0:)

    status_t                        :: ist
    integer                         :: i

    if( size(r) /= size(dr) ) stop 'tRGRID grid_use: R and DR must have same size!'
    i = ubound(r,1)
    allocate( gr%r(0:i), gr%dr(0:i), gr%r2dr(0:i), stat=ist )
    if( ist /= 0 ) stop 'rgrid_use: allocation of gr%R, gr%DR and/or gr%R2DR failed.'

    gr%keq = K_EXP1 ! assume an exponential grid type K_EXP1

    gr%d   = log(dr(1)/dr(0)) ! anisotropy
    gr%a   = dr(0)/gr%d       ! offset

    gr%imx = i
    gr%rmx = r(i)
    ! copy
    gr%r  = r
    gr%dr = dr
    ! generate
    gr%r2dr = r*r*dr

  endfunction ! rgrid_use


  integer elemental function find_index( rg, r ) result( i )
    type(rgrid), intent(in)         :: rg
    real, intent(in)                :: r ! radius

    selectcase( rg%keq )
    case( K_EXP0 )    ! r=a*exp(d*i)
      i = nint( log(abs(r)/rg%a)/rg%d )
    case( K_REC1 )    ! r=a*i/(1-b*i) ! b=rg%d
      i = nint( abs(r)/( rg%a + rg%d*abs(r) ) )
    case( K_REC0 )    ! r=a*i/(n-i)   
      i = nint( (rg%imx+1.)*abs(r)/(abs(r)+rg%a) ) ! OK!
    case( K_EXP1 )    ! r=a*(exp(d*i)-1)   
      i = nint( log(abs(r)/rg%a+1.)/rg%d )
    case( K_EQUI )    ! r=d*i
      i = nint( abs(r)/rg%d )
    case( K_P5a4 )    ! r=(i/n+a)^5/a-a^4
!       stop 'tRGrid: r=(i/n+a)^5/a-a^4 not implemented'
      i = nint( (rg%imx+1.)*( ( abs(r)*rg%a + rg%a**5 )**0.2 - rg%a ) )
    case default
!       stop 'tRGrid: key not implemented'
    endselect ! keq
    i = min( i, rg%imx )

  endfunction ! find_index

  real elemental function find_radius( rg, i ) result( r )
    type(rgrid), intent(in)         :: rg
    integer, intent(in)             :: i ! index
    if( i < 0 ) then
      r = 0.
    elseif( i > rg%imx ) then
      r = rg%rmx
    else
      r = rg%r(i) ! access array
    endif
  endfunction ! find_radius

#ifdef EXTENDED
  status_t function test( )
    write(*,*,iostat=test) __FILE__,' no module test implemented!'
  endfunction ! test
#endif

endmodule ! type_rgrid

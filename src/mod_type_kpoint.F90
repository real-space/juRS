#include "config.h"

#ifdef DEBUG_ALL
#undef DEBUG
#endif

! #define DEBUG

!! @author Paul Baumeister
!! @version 3.0
!!
!! an element of the Brillouin zone sampling
module type_kpoint
#ifdef DEBUG
  use configuration, only: o ! output unit, 0: no output
#endif
implicit none
  private ! default for the module namespace
  character(len=*), parameter, private :: sym = 'tKPOINT' !! module symbol

  ! public
  public :: kpoint
  public :: kpoint_set
  public :: half_integer
  public :: kpoint_Gamma
#ifdef EXTENDED
  public :: to_string
  public :: test

  interface to_string
    module procedure kpoint2string
  endinterface
#endif

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! TYPE KPOINT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  type :: kpoint
    integer :: jk      =  0 ! global index
    real    :: k(3)    = 0. ! k-vector
    complex :: eik(3)  = 1. ! Bloch-phases exp(im*k*cell*2pi)
    real    :: w       = 1. ! weight
    real    :: klen    = 0. ! k-vector-length and path progress if in path
  endtype ! kpoint

  type(kpoint), parameter :: GAMMA = kpoint( 0, (/0.,0.,0./), (/ (1.,0.), (1.,0.), (1.,0.) /), 1., 0. )

#ifndef DEBUG
  iounit_t, parameter :: o = 0
#endif

  contains

  type(kpoint) function kpoint_set( k, weight, jk, kimaginary ) result( kp )
  use constants, only: Pi
  use configuration, only: WARNING
    character(len=*), parameter       :: fun = ' kpoint_set: '
    complex, parameter                :: im = ( 0.,1.)
    complex, parameter                :: m1 = (-1.,0.) ! exp(im*Pi)
    ! arguments
    real, intent(in)                  :: k(3)
    real, intent(in), optional        :: weight
    integer, intent(in), optional     :: jk
    real, intent(in), optional        :: kimaginary(1:3)

    ! backfolded into [-0.5,0.5]
    kp%k = mod( k - 0.5, 1.0 ) + 0.5

    kp%eik(1:3) = exp( im * 2.* Pi * kp%k(1:3) )
!     kp%eik(1:3) = m1**( 2. * kp%k(1:3) )
#ifdef DEBUG
    if(o>0) write(o,'(3A,3(F9.3,F7.3))') sym, fun, 'eik =', kp%eik(1:3)
#endif

    if( present( kimaginary ) ) then
      kp%eik(1:3) = exp( im * 2. * Pi * ( kp%k(1:3) + im * kimaginary(1:3) ) )
!       kp%eik(1:3) = m1**( 2. * kp%k(1:3) ) * exp( -2. * Pi * kimaginary(1:3) )
#ifdef DEBUG
      if(o>0) write(o,'(4A,3(" ",F0.6))') sym, fun, WARNING(0), &
        'imaginary part for k-vector present, |eik| =', abs(kp%eik(1:3))
#endif
    endif ! present( kimaginary )

    if( present(weight) ) then

      if( weight > 0. ) then
        kp%w = weight
      else  ! weight > 0.
        kp%w = 0.
#ifdef DEBUG
        if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'weight =< 0., set to 0.'
#endif
      endif ! weight > 0.

    else  ! present
      kp%w = 1.
    endif ! present

    if( present(jk) ) kp%jk = jk
  endfunction ! kpoint_set


  type(kpoint) function kpoint_Gamma( jk, wgt ) result( kp )
    integer, intent(in), optional :: jk
    real, intent(in), optional    :: wgt
    kp = GAMMA
    if( present( jk ) ) kp%jk = jk
    if( present( wgt ) ) kp%w = wgt
  endfunction ! kpoint_Gamma

  logical elemental function half_integer( kp ) result( l )
  ! this function returns true, if
  ! all k(1:3) are half integer.
  ! after backfolding, this should mean in {-0.5,0.0,0.5}
    type(kpoint), intent(in) :: kp
    l = all( 0.5 * nint( 2.*kp%k(1:3) ) == kp%k(1:3) )
  endfunction ! half_integer

#ifdef EXTENDED
!+ extended

  character(len=128) function kpoint2string( k ) result( str )
    type(kpoint), intent(in)  :: k

    status_t :: ios
    write( unit=str, fmt='(A,3(" ",F0.6),A,F0.6,A,I0,A,F0.6)', IOstat=ios ) &
      'kpoint', k%k, ' weight ', k%w, ' #', k%jk, ' len ',k%klen
  endfunction ! kpoint2string

  status_t function test( ) result( ios )
    write(*,*,iostat=ios) __FILE__,' no module test implemented!'
  endfunction ! test

!- extended
#endif

endmodule ! type_kpoint

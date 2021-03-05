#include "config.h"

! #define DEBUG

!! @author Paul Baumeister
!! @version 3.0
!!
!! contains criteria for convergence for iterative methods
module type_criteria
#ifdef DEBUG
  use configuration, only: o, WARNING
#endif
implicit none
  private ! default for this module namespace

  public :: criteria, set, assignment(=), go_on, criteria_read
#ifdef EXTENDED
  public :: to_string
  public :: test

  interface to_string
    module procedure criteria2string
  endinterface
#endif

  integer, parameter :: DEF_MAXIT = 99  ! maxiter is stronger
  integer, parameter :: DEF_MINIT =  0  ! than miniter
  real,    parameter :: DEF_THRES = 1E-9 ! and then threshold

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!  type(criteria)    !!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  type :: criteria  !! convergence criteria for any iterative process
    integer :: maxit = DEF_MAXIT    !! maximum number of iterations
    integer :: minit = DEF_MINIT    !! minimum number of iteration
    real    :: thres = DEF_THRES    !! convergence threshold
  endtype ! criteria

  integer, save, protected, public :: nwarnings = 0

  interface set ! function
    module procedure criteria_set
  endinterface

  interface assignment(=)
    module procedure criteria_read_line
  endinterface

#ifndef DEBUG
  integer, parameter :: o = 6 ! stdout
#endif

  contains

  type(criteria) function criteria_set( maxiter, miniter, threshold, name ) result( t )
    integer, intent(in), optional             :: maxiter !! maximum number of iterations
    integer, intent(in), optional             :: miniter !! minimum number of iterations
    real, intent(in), optional                :: threshold !! converged if residual < threshold
    character(len=*), intent(in), optional    :: name

!     status_t :: ios

    if( present( maxiter ) ) t%maxit = max( 0, maxiter )
    if( present( miniter ) ) t%minit = max( 0, miniter )
    if( present( threshold ) ) t%thres = max( 0., threshold )
    if( present( name ) ) then
!       t%name = adjustl(name)
!     else  ! present name
!       write(unit=t%name,fmt='(5A,ES8.1E2)',iostat=ios) &
!         'max ', trim(str6(t%maxit)),' min ', trim(str6(t%minit)),' <', t%thres
    endif ! present name
#ifdef DEBUG
    if( t%thres <= 0. .and. o/=0 ) write(o,'(9A)') __FILE__,' ',WARNING(0), &
      'convergence criteron <= 0.0 is numerically difficult!'
#endif
  endfunction ! criteria_set

  character(len=8) function str8( i ) result( str )
    integer, intent(in)     :: i
    write(unit=str,fmt='(I0)') i
  endfunction ! str

  subroutine criteria_read_line( c, line )
    type(criteria), intent(out)  :: c
    character(len=*), intent(in) :: line
    c = criteria_read( line )
  endsubroutine ! criteria_read_line

  type(criteria) function criteria_read( line ) result( t )
    character(len=*), intent(in) :: line !! line in the input file

    integer, parameter :: NW = 9 ! number of words
    character(len=16)  :: word(NW)
    status_t           :: ios
    integer            :: mx, mn, iw
    real               :: th
#ifdef DEBUG
    logical            :: show
    show = .true.
#endif
    ! defaults
    mx = DEF_MAXIT
    mn = DEF_MINIT
    th = DEF_THRES

    word = '' ! init
    ! decompose the line into a set of words
    read( unit=line, fmt=*, iostat=ios ) word

    !==========================================================
    iw = 1
    do while( iw < NW )
      !==========================================================
      ios = 1 ! must be different from 0
      selectcase( word(iw)(1:3) )
      case( 'max' ) ; read(unit=word(iw+1),fmt=*,iostat=ios) mx
      case( 'min' ) ; read(unit=word(iw+1),fmt=*,iostat=ios) mn
      case( 'con','thr','cnv','lim','<','<=' )
                      read(unit=word(iw+1),fmt=*,iostat=ios) th
#ifdef DEBUG
      ! "silent", "quiet" and "default" switch off the output in DEBUG mode
      case( 'sil', 'qui', 'def' ) ; show = .false.
      case( '', 'scf', 'poi', 'md' ) ! without warning
      case default ; if(o>0) write(o,'(9A)') __FILE__,' unrekognized (key)word "', trim(word(iw)), '".'
#else
      case default ! nothing
#endif
      endselect ! word(iw)
      if( ios == 0 ) iw = iw+1 ! the next word does not need to be read
      !==========================================================
      iw = iw+1
    enddo ! iw
    !==========================================================

    t = criteria_set( maxiter=mx, miniter=mn, threshold=th )
#ifdef DEBUG
    if(o>0 .and. show) write(o,'(9A)') __FILE__,' new set of criteria "', trim(to_string(t)), '".'
#endif
  endfunction ! criteria_read


  !! decision function for convergence, stop or continue
  !!  logic table: when should an iterative method continue running
  logical function go_on( c, it, res, message ) result( run )
    type(criteria), intent(in)                :: c !! criterion
    integer, intent(in), optional             :: it !! number of the current iteration
    real, intent(in), optional                :: res !! residual
    character(len=*), intent(out), optional   :: message !! not in use

    run = ( c%maxit > 0 )            ! if maxiter is set to 0, no run is possible
    if( present( res ) ) then
      run = ( res > c%thres )          ! the weakest criterion: run = not converged
    endif ! present res
    if( present( it ) ) then
      run = run .or.  ( it < c%minit ) ! if converged, but less than miniter, go on
      run = run .and. ( it < c%maxit ) ! the strongest criterion: stop iteration if maxiter is exceeded
    endif ! present it
    if( present( message ) ) message = '' ! not used
  endfunction ! go_on


#ifdef EXTENDED
!+ extended

  character(len=64) function criteria2string( c ) result( str )
    type(criteria), intent(in)      :: c
    status_t :: ios
    write(unit=str,fmt='(2(A,I0),A,ES10.2,9A)',IOstat=ios) &
      'criteria maxit ', c%maxit, ' minit ', c%minit, ' thres ', c%thres!, ' ', trim(c%name)
  endfunction ! criteria2string

  !! test function for this module
  status_t function test( ) result( ist )
    string_t :: input_line = "max 133 min -1 < 5.7"
    type(criteria) :: c
    string_t :: in
    c = set( maxiter=3 )! ; write(*,*) c%name
    c = criteria_read( "max 33 conv 1.2E-23" )! ; write(*,*) c%name
    c = criteria_read( "silent max 33 conv 1" )! ; write(*,*) c%name
    c = criteria_read( "max 66 < 1E-99" )! ; write(*,*) c%name
    c = criteria_read( "max -12 < 0.0" )! ; write(*,*) c%name
    write(*,*) 'please type in a criterium:' 
    write(*,*) 'input was ',input_line
    read(unit=input_line,fmt='(A)',iostat=ist) in
    c = criteria_read( in )! ; write(*,*) c%name
  endfunction ! test

!- extended
#endif

endmodule ! type_criteria

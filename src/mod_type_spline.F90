#include "config.h"


!! @author  Andrea Nobile, inspired by GPAW

module type_spline
  use configuration, only: o ! output unit, 0: no output
  use configuration, only: WARNING, ERROR
  use constants, only: Pi, sqrt4Pi
implicit none
  
  type :: spline    
    real, pointer       :: data(:) 
    real                :: ell
    real                :: dr
    integer             :: n
  endtype ! spline

  contains

  type(spline) function spline_create( ell, dr, n, f ) result( sp )
    integer, intent(in)           :: ell, n
    real, intent(in)              :: dr
    real, intent(in)              :: f(0:n)

    real                          :: c, p
    real, allocatable             :: f2(:), u(:)
    integer                       :: b, ist

    allocate(sp%data(0:4*(n+1)-1), stat=ist)
    sp%ell = ell
    sp%dr = dr
    sp%n = n

    allocate( f2(0:n), u(0:n-1), stat=ist)

    c = 3.0 / (dr * dr)
    f2(0) = -0.5
    u(0) = (f(1) - f(0)) * c
    do b=1,n-1
      p = 0.5 * f2(b - 1) + 2.0
      f2(b) = -0.5 / p
      u(b) = ((f(b + 1) - 2.0 * f(b) + f(b - 1)) * c - 0.5 * u(b - 1)) / p
    enddo ! b
 
    f2(n) = ((f(n - 1) * c - 0.5 * u(n - 1)) / (0.5 * f2(n - 1) + 1.0))

    do b=n-1, 0, -1
      f2(b) = f2(b) * f2(b + 1) + u(b)
    enddo ! b
 
    do b=0,n-1
      sp%data(4*b+0) = f(b)
      sp%data(4*b+1) = (f(b + 1) - f(b)) / dr - (f2(b) / 3.0 + f2(b + 1) / 6.0) * dr
      sp%data(4*b+2) = 0.5 * f2(b)
      sp%data(4*b+3) = (f2(b + 1) - f2(b)) / (6.0 * dr)
    enddo ! b

    if(ell > 0) sp%data(0:3) = 0.0
    
  endfunction ! create

  real function spline_eval( s, r ) result (res)
    type(spline), intent(in) :: s
    real, intent(in)         :: r
    integer                  :: b
    real                     :: u

    if (r >= s%n * s%dr) then 
      res = 0.0
      return
    endif
    b = r / s%dr
    u = r - real(b) * s%dr
    res = s%data(4*b) + u*(s%data(4*b+1) + u*(s%data(4*b+2) + u*s%data(4*b+3)))

  endfunction ! eval

  subroutine spline_eval_and_derive( s, r, val, der ) 
    type(spline), intent(in) :: s
    real, intent(in)         :: r
    real, intent(out)        :: val, der
    integer                  :: b
    real                     :: u

    if (r >= s%n * s%dr) then 
      val = 0.0
      der = 0.0
      return
    endif
    b = r / s%dr
    u = r - real(b) * s%dr
    val = s%data(4*b) + u*(s%data(4*b+1) + u*(s%data(4*b+2) + u*s%data(4*b+3)))
    der = s%data(4*b+1) + u * (2.0 * s%data(4*b+2) + u * 3.0 * s%data(4*b+3))

  endsubroutine ! eval_and_derive


  subroutine spline_free( s )
    type(spline), intent(inout) :: s
    deallocate(s%data)
  endsubroutine ! free

  subroutine spline_write2file(s, fname)
    type(spline), intent(in)     :: s
    character(len=*), intent(in) :: fname
    integer, parameter           :: u = 30
    integer                      :: i
    real                         :: r
    
    open(unit=u, file=trim(fname))
    do i=0,300
      r = i/300.
      write(u, *) r, spline_eval(s, r)
    enddo
    close(u)

  endsubroutine ! write2file

  real function spline_norm(s) result(res)
    type(spline), intent(in)     :: s
    integer                      :: i
    
    res = 0.0
    do i=0, s%n
      res = res + spline_eval(s, i*s%dr)*((i*s%dr)**s%ell)*(i*s%dr)**2*s%dr
    enddo

  endfunction ! norm

endmodule ! type_spline


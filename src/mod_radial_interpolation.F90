#include "config.h"

!! Andrea Nobile 11/2013


module radial_interpolation
  implicit none
  private

  public :: interpolate_parabolic
  public :: interpolate_on_grid
#ifdef EXTENDED
  public :: test
#endif

  contains

  real function interpolate_parabolic( x, y, xx ) result(yy)
    real, intent(in)        :: x(1:3)
    real, intent(in)        :: y(1:3)
    real, intent(in)        :: xx

    real                    :: pA, pB, pC
    
    pA = x(3) * (y(2) - y(1)) + x(2) * (y(1) - y(3)) + x(1) * (y(3) - y(2))
    pB = x(3)*x(3) * (y(1) - y(2)) + x(2)*x(2) * (y(3) - y(1)) + x(1)*x(1) * (y(2) - y(3))
    pC = x(2) * x(3) * (x(2) - x(3)) * y(1) + x(3) * x(1) * (x(3) - x(1)) * y(2) + x(1) * x(2) * (x(1) - x(2)) * y(3)

    yy = ( xx*(xx*pA + pB) + pC )/( (x(1) - x(2))*(x(1) - x(3))*(x(2) - x(3)) )

  endfunction ! interpolate_parabolic


  ! computes Lagrange interpolants h(j) is l_j(xi) h(j)[x_i] = delta_{ij}
  subroutine lagrange_eval(xi,n,xpts,h)
  implicit none
  integer, intent(in) :: n
  real, intent(in)   ::  xi,xpts(n)
  real, intent(out)  ::  h(n)

  integer dgr,i,j
  real prod1,prod2

  do dgr=1,n

    prod1 = 1.0
    prod2 = 1.0
    do i=1,n
      if(i /= dgr) then
        prod1 = prod1*(xi-xpts(i))
        prod2 = prod2*(xpts(dgr)-xpts(i))
      endif
    enddo
    h(dgr)=prod1/prod2

  enddo

  end subroutine lagrange_eval


!  tatic double interpolate_cubic(double *xval, const double *yval, double x)
!{
!  double h[4];
!    
!  lagrange_eval(x, 4, xval, h, NULL);
!  return h[0]*yval[0] + h[1]*yval[1] + h[2]*yval[2] + h[3]*yval[3];
!  
!}
  real function interpolate_cubic( x, y, xx ) result(yy)
    real, intent(in)        :: x(1:4)
    real, intent(in)        :: y(1:4)
    real, intent(in)        :: xx

    real                    :: h(4)

    call lagrange_eval(xx, 4, x, h)

    yy = h(1)*y(1) + h(2)*y(2) + h(3)*y(3) +h(4)*y(4);
    
  endfunction ! interpolate_cubic



!! this is wrong!!!
  subroutine interpolate_on_grid_cub( grid_in, f_in, grid_out, f_out )
    use type_rgrid
    implicit none
    type(rgrid), intent(in) :: grid_in
    type(rgrid), intent(in) :: grid_out    
    real, intent(in)        :: f_in(0:)
    real, intent(out)       :: f_out(0:)
    
    integer                 :: i,j
    real                    :: x(1:4)
    real                    :: y(1:4)
    real                    :: xx, dist1, dist2
    integer                 :: idx

    do i = 0, grid_out%imx
      xx = grid_out%r(i)

      do idx = 0, grid_in%imx-1
        if( grid_in%r(idx+1) > xx ) exit 
      enddo
                                                ! idx   idx+1 
      !! here we are going to do              o    o  x  o


      !! now check the distance between out interpolation point and the 2 closest points
      dist1 = xx - grid_in%r(idx)
      dist2 = grid_in%r(idx+1) - xx

      if(dist1 < dist2) then 
      !  idx = idx-1
      else 
         idx = idx + 1
      endif

      
      

      if(xx >= grid_in%rmx ) then
        f_out(i:) = 0.
        exit
      endif
      !idx = grid_in .at. xx
      
      ! check if the index is <= 0 or > imx-1
      if( idx <= 1 ) then 
        idx = 1
      elseif( idx >= (grid_in%imx-2) ) then
        idx = grid_in%imx - 2
      endif

      do j = 1, 4
        x(j) = grid_in%r(idx+j-2) ! idx-1, idx, idx+1
        y(j) = f_in(idx+j-2)
      enddo
      
      f_out(i) = interpolate_cubic( x, y, xx )
      
    enddo ! i

  endsubroutine ! interpolate_on_grid


  subroutine interpolate_on_grid( grid_in, f_in, grid_out, f_out )
    use type_rgrid
    implicit none
    type(rgrid), intent(in) :: grid_in
    type(rgrid), intent(in) :: grid_out    
    real, intent(in)        :: f_in(0:)
    real, intent(out)       :: f_out(0:)
    
    integer                 :: i,j
    real                    :: x(1:3)
    real                    :: y(1:3)
    real                    :: xx, dist1, dist2
    integer                 :: idx

    do i = 0, grid_out%imx
      xx = grid_out%r(i)

      do idx = 0, grid_in%imx-1
        if( grid_in%r(idx+1) > xx ) exit 
      enddo

      !! here we are going to do         o     o   x  o    


      !! now check the distance between out interpolation point and the 2 closest points
      !dist1 = xx - grid_in%r(idx)
      !dist2 = grid_in%r(idx+1) - xx

      !if(dist1 < dist2) then 
      !  !idx = idx-1
      !else 
      !  idx = idx +1
      !endif

      
      

      if(xx >= grid_in%rmx ) then
        f_out(i:) = 0.
        exit
      endif
      !idx = grid_in .at. xx
      
      ! check if the index is <= 0 or > imx-1
      if( idx <= 0 ) then 
        idx = 1
      elseif( idx >= (grid_in%imx-1) ) then
        idx = grid_in%imx - 1
      endif

      do j = 1, 3
        x(j) = grid_in%r(idx+j-2) ! idx-1, idx, idx+1
        y(j) = f_in(idx+j-2)
      enddo
      
      f_out(i) = interpolate_parabolic( x, y, xx )
      
    enddo ! i

  endsubroutine ! interpolate_on_grid

#ifdef EXTENDED
!+ extended

  status_t function test( ) result( ios )
    write(*,*,iostat=ios) __FILE__,' no module test implemented!'
  endfunction ! test

!- extended
#endif
endmodule ! radial_interpolation

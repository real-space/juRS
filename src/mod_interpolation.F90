#include "config.h"

! #define DEBUG
! #define FULL_DEBUG

#ifdef DEBUG
#define cDBG  
#else
#define cDBG !DBG
#endif



!! @author Andrea Nobile, Paul Baumeister
!! 
!!
!! performs interpolation from coarse to dense grids and vice versa
! the grid ponts are disposed like this, no points of dense and coarse are on top of each other
!
!         0     1     2
!
!       ----- ----- ----- --
!      |     |     |     |
!  0   |  X  |  X  |  X  |
!      |     |     |     |
!       ----- ----- ----- --
!      |     |o   o|o   o|o   1
!  1   |  X  |  X  |  X  |
!      |     |o   o|o   o|o   2
!       ----- ----- ----- --
!      |     |o   o|o   o|o   3
!  2   |  X  |  X  |  X  |
!      |     |o   o|o   o|o   4
!       ----- ----- ----- --
!      |     |o   o|o   o|o   5
!
!             1   2 3   4 5

module interpolation
!  use configuration, only: o ! output unit, 0: no output
implicit none
  private ! default for the module namespace
  character(len=*), parameter, private :: sym = 'ITP' !! module symbol

  public :: interpolate
  public :: test

  interface interpolate
    module procedure interpolate_r4, interpolate_r3
  endinterface

  
  contains


  status_t function interpolate_r3( vin, vout, g, gd, mscale, order ) result( ist )
cDBG  use configuration, only: o
  use type_grid, only: grid, BC_PERIODIC, BC_FINITE
  use configuration, only: o
  implicit none

    character(len=*), parameter     :: fun = ' interpolate: '

    real, intent(in)          :: vin(:,:,:)
    real, intent(out)         :: vout(:,:,:)
    type(grid), intent(in)    :: g  ! the smaller grid
    type(grid), intent(in)    :: gd  ! the bigger grid
    integer, intent(in)       :: mscale ! multiplyer
    integer, intent(in), optional :: order ! order of interpolation
   
    integer :: iord
    !iord = 1 ! linear
    !iord = 2 ! quadr
    iord = 3 ! quartic
    !if( present( order ) ) iord = max(1,order)

    !write (*,*) 'interpolate -> mscale ', mscale, 'order ', iord 
    ist = 0
    selectcase( mscale ) ! scalenumber
    
    case( -2 )
      ! interpolate from dense grid to coarse grid
      ! averaging over each set of dense grid points
      !ist = reduce_m2( vin, vout, g)
      if(iord == 1) then
        ist = reduce_linear( vin, vout, g)
      elseif (iord == 2) then
        ist = reduce_quadratic( vin, vout, g, gd)
      else 
        ist = reduce_quartic( vin, vout, g, gd)
     
      endif

    case( 2 )

      if( iord == 1 ) then
        ist = enlarge_linear( vin, vout, g )
      elseif (iord == 2) then
        ist = enlarge_quadratic( vin, vout, g )
      else
        ist = enlarge_quartic( vin, vout, g )
       
      endif 

    
    case( 1,-1 )
      ! no interpolation required
      vout = vin 

    case( 0 )
      ist = -1 ! error
    endselect ! mscale

  endfunction ! interpolate_r3



  !! wrapper routine for rank4 arrays
  status_t function interpolate_r4( vin, vout, g, gd, mscale, order ) result( ist )
  use type_grid, only: grid, BC_PERIODIC, BC_FINITE
  implicit none
    character(len=*), parameter     :: fun = ' interpolate: '

    real, intent(in)          :: vin(:,:,:,:)
    real, intent(out)         :: vout(:,:,:,:)
    type(grid), intent(in)    :: g  ! the smaller grid
    type(grid), intent(in)    :: gd  ! the bigger grid
    integer, intent(in)       :: mscale ! multiplyer
    integer, intent(in), optional :: order ! order of interpolation, pass as optional

    integer                   :: i4, n4

    n4 = size( vin, 4 )
cDBG  if( n4 /= 1 .and. n4 /= 2 ) stop 'interpolate: N4 not in {1,2}.'
cDBG  if( size( vout, 4 ) /= n4 ) stop 'interpolate: dim #4 of array VOUT does match N4.'

    do i4 = 1, n4
      ist = interpolate_r3( vin(:,:,:,i4), vout(:,:,:,i4), g, gd, mscale, order )
      if( ist /= 0 ) return
    enddo ! i4

  endfunction ! interpolate_r4

  !Andrea Nobile 4.2014
  ! computes Lagrange interpolants h(j) is l_j(xi) h(j)[x_i] = delta_{ij}
  subroutine lagrange_eval(xi,n,xpts,h)
  implicit none
  integer, intent(in) :: n
  real, intent(in)   ::  xi,xpts(n)
  real, intent(out)  ::  h(n)

  integer dgr,i,j
  double precision prod1,prod2

  do dgr=1,n

    prod1 = 1.0d0
    prod2 = 1.0d0
    do i=1,n
      if(i /= dgr) then
        prod1 = prod1*(xi-xpts(i))
        prod2 = prod2*(xpts(dgr)-xpts(i))
      endif
    enddo
    h(dgr)=prod1/prod2

  enddo

  end subroutine lagrange_eval


  !Andrea Nobile 4.2014
  subroutine lagrange_quadratic(inarr, outarr)
    real, intent(in)          :: inarr(3, 3, 3)
    real, intent(out)         :: outarr(2, 2, 2)

    integer :: ix, iy, iz, kx, ky, kz
    real    :: lx, ly, lz
    real    :: hx(3), hy(3), hz(3)
    real    :: xpts(3)

    xpts(1) = -1.
    xpts(2) = 0.
    xpts(3) = 1.

     do ix = 1,2
      lx = -0.75+0.5*real(ix)
      call lagrange_eval(lx, 3, xpts, hx)
      do iy = 1,2
        ly = -0.75+0.5*real(iy)
        call lagrange_eval(ly, 3, xpts, hy)
        do iz = 1,2
          lz = -0.75+0.5*real(iz)
          call lagrange_eval(lz, 3, xpts, hz)
          outarr(ix, iy, iz) = 0.
          do kx=1, 3
            do ky=1, 3
              do kz=1, 3
                outarr(ix, iy, iz) = outarr(ix, iy, iz) + &
                  hx(kx)*hy(ky)*hz(kz)*inarr(kx, ky, kz)
              enddo !kz
            enddo !ky
          enddo !kx
      
        enddo ! iz
      enddo  !iy
    enddo !ix

 
  endsubroutine

  !Andrea Nobile 4.2014
  subroutine lagrange_quartic(inarr, outarr)
    real, intent(in)          :: inarr(5, 5, 5)
    real, intent(out)         :: outarr(2, 2, 2)

    integer :: ix, iy, iz, kx, ky, kz
    real    :: lx, ly, lz
    real    :: hx(5), hy(5), hz(5)
    real    :: xpts(5)

    xpts(1) = -2.
    xpts(2) = -1.
    xpts(3) = 0.
    xpts(4) = 1.
    xpts(5) = 2.

     do ix = 1,2
      lx = -0.75+0.5*real(ix)
      call lagrange_eval(lx, 5, xpts, hx)
      do iy = 1,2
        ly = -0.75+0.5*real(iy)
        call lagrange_eval(ly, 5, xpts, hy)
        do iz = 1,2
          lz = -0.75+0.5*real(iz)
          call lagrange_eval(lz, 5, xpts, hz)
          outarr(ix, iy, iz) = 0.
          do kx=1, 5
            do ky=1, 5
              do kz=1, 5
                outarr(ix, iy, iz) = outarr(ix, iy, iz) + &
                  hx(kx)*hy(ky)*hz(kz)*inarr(kx, ky, kz)
              enddo !kz
            enddo !ky
          enddo !kx
      
        enddo !iz
      enddo  !iy
    enddo !ix

 
  endsubroutine
  

 !Andrea Nobile 4.2014
  status_t function enlarge_quartic( v, v3, g ) result( ist )
  use configuration, only: o ! output unit, 0: no output
  use type_grid, only: grid !, BC_PERIODIC, BC_FINITE
  use boundary, only: dataexchange
  implicit none
   
    character(len=*), parameter :: fun = ' enlarge_quartic: '

 
    real, intent(in)          ::  v(:,:,:)
    real, intent(out)         :: v3(:,:,:)
    type(grid), intent(in)    :: g
    ! local vars
    type(grid)                :: g0, g1, g2
    integer                   :: m(1:3) ! multiplyer mscale
    integer                   :: ix, iy, iz, ii, ik
    integer                   :: jx, jy, jz, id
    real, allocatable         :: t(:,:,:)
    real                      :: interp_values(2, 2, 2)

    ist = 0
    do id = 1, 3
      m(id) = nint( real(size(v3,id))/real(size(v,id))  )
      if( m(id) /= 2 ) then
        if(o>0) write(o,'(5A,9(I0,A))') sym, fun, 'mscale not equal to 2 in ', achar(119+id), '-direction: n(V)/n(v) =', size(v3,id), ' /', size(v,id)
        ist = id
        stop
      endif ! m(id) < 2
    enddo ! id

   
    allocate( t(-1:g%ng(1)+2,-1:g%ng(2)+2,-1:g%ng(3)+2), stat=ist )
    if( ist /= 0 ) stop 'enlarge_quadratic: failed to allocate!'

    t = 0. ! init entire array and copy into the center region of t
    t(1:g%ng(1),1:g%ng(2),1:g%ng(3)) = v(1:g%ng(1),1:g%ng(2),1:g%ng(3))

    g1 = g ! copy grid descriptor
    g1%nh = 2 ! set the halo-size to 2
    call dataexchange( g1, t ) ! fill the halos

    do iz = 1, g%ng(3)
      do iy = 1, g%ng(2)
        do ix = 1, g%ng(1)

          
          call lagrange_quartic(t(ix-2:ix+2, iy-2:iy+2, iz-2:iz+2), interp_values)
          !----------------------
          do jz = -1, 0
            do jy = -1, 0
              do jx = -1, 0
                v3(2*ix+jx,2*iy+jy,2*iz+jz) = interp_values(jx+2, jy+2, jz+2)
                 
              enddo ! jx
            enddo ! jy
          enddo ! jz
          !----------------------
        enddo ! ix
      enddo ! iy
    enddo ! iz

  endfunction ! enlarge_quartic



  !Andrea Nobile 4.2014
  status_t function enlarge_quadratic( v, v3, g ) result( ist )
  use configuration, only: o ! output unit, 0: no output
  use type_grid, only: grid !, BC_PERIODIC, BC_FINITE
  use boundary, only: dataexchange
  implicit none
   
    character(len=*), parameter :: fun = ' enlarge_quadratic: '

 
    real, intent(in)          ::  v(:,:,:)
    real, intent(out)         :: v3(:,:,:)
    type(grid), intent(in)    :: g
    ! local vars
    type(grid)                :: g0, g1, g2
    integer                   :: m(1:3) ! multiplyer mscale
    integer                   :: ix, iy, iz, ii, ik
    integer                   :: jx, jy, jz, id
    real, allocatable         :: t(:,:,:)
    real                      :: interp_values(2, 2, 2)

    ist = 0
    do id = 1, 3
      m(id) = nint( real(size(v3,id))/real(size(v,id))  )
      if( m(id) /= 2 ) then
        if(o>0) write(o,'(5A,9(I0,A))') sym, fun, 'mscale not equal to 2 in ', achar(119+id), '-direction: n(V)/n(v) =', size(v3,id), ' /', size(v,id)
        ist = id
        stop
      endif ! m(id) < 2
    enddo ! id

   
    allocate( t(0:g%ng(1)+1,0:g%ng(2)+1,0:g%ng(3)+1), stat=ist )
    if( ist /= 0 ) stop 'enlarge_quadratic: failed to allocate!'

    t = 0. ! init entire array and copy into the center region of t
    t(1:g%ng(1),1:g%ng(2),1:g%ng(3)) = v(1:g%ng(1),1:g%ng(2),1:g%ng(3))

    g1 = g ! copy grid descriptor
    g1%nh = 1 ! set the halo-size to 2
    call dataexchange( g1, t ) ! fill the halos

    do iz = 1, g%ng(3)
      do iy = 1, g%ng(2)
        do ix = 1, g%ng(1)

          call lagrange_quadratic(t(ix-1:ix+1, iy-1:iy+1, iz-1:iz+1), interp_values)
          
          !----------------------
          do jz = -1, 0
            do jy = -1, 0
              do jx = -1, 0
                v3(2*ix+jx,2*iy+jy,2*iz+jz) = interp_values(jx+2, jy+2, jz+2)
                 
              enddo ! jx
            enddo ! jy
          enddo ! jz
          !----------------------
        enddo ! ix
      enddo ! iy
    enddo ! iz

  endfunction ! enlarge_quadratic

  
  
  status_t function enlarge_linear( v, v3, g ) result( ist )
  use type_grid, only: grid, BC_PERIODIC, BC_FINITE
  use boundary, only: dataexchange
  implicit none
    ! parameters
    character(len=*), parameter     :: fun = ' enlarge_linear: '
    character(len=1), parameter     :: DIRECTIONCHAR(1:3) = (/'x','y','z'/)
    real, parameter                 :: a1 = 0.25
    real, parameter                 :: a3 = 0.75
    real, parameter                 :: a(-1:0) = (/a1,a3/)
    real, parameter                 :: b(-1:0) = (/a3,a1/)
    ! args
    real, intent(in)          ::  v(:,:,:)
    real, intent(out)         :: v3(:,:,:)
    type(grid), intent(in)    :: g
    ! local vars
    type(grid)                :: g1
    integer                   :: ix, iy, iz, jx, jy, jz, id
!    real                      :: t(0:g%ng(1)+1,0:g%ng(2)+1,0:g%ng(3)+1)
    real, allocatable         :: t(:,:,:) ! t(0:g%ng(1)+1,0:g%ng(2)+1,0:g%ng(3)+1)

cDBG  do id = 1, 3
cDBG    if( size( v, id ) /= g%ng(id) ) stop 'ITP enlarge_o1_m2: an array dim of V is wrong'
cDBG    if( size( v3, id ) /= 2*g%ng(id) ) stop 'ITP enlarge_o1_m2: an array dim of V3 is wrong'
cDBG  enddo ! id
    allocate( t(0:g%ng(1)+1,0:g%ng(2)+1,0:g%ng(3)+1), stat=ist )
    if( ist /= 0 ) stop 'ITP enlarge_o1_m2: failed to allocate!'

    t = 0. ! init entire array and copy into the center region of t
    t(1:g%ng(1),1:g%ng(2),1:g%ng(3)) = v(1:g%ng(1),1:g%ng(2),1:g%ng(3))

    g1 = g ! copy grid descriptor
    g1%nh = 1 ! set the halo-size to 1
    call dataexchange( g1, t ) ! fill the halos

#ifdef NaN_SEARCH
    if( any( t /= t ) ) stop 'ITP enlarge_o1_m2: NaN in t after dataexchange.'
#endif

!
!         0     1     2
!
!       ----- ----- ----- --
!      |     |     |     |
!  0   |  X  |  X  |  X  |
!      |     |     |     |
!       ----- ----- ----- --
!      |     |o   o|o   o|o   1
!  1   |  X  |  X  |  X  |
!      |     |o   o|o   o|o   2
!       ----- ----- ----- --
!      |     |o   o|o   o|o   3
!  2   |  X  |  X  |  X  |
!      |     |o   o|o   o|o   4
!       ----- ----- ----- --
!      |     |o   o|o   o|o   5
!
!             1   2 3   4 5
!_____________________________
!  o: (equidistant) dense grid points
!  X: coarse grid points (on halo-enlarged array)
!
!  with the 1-dim interpolation scheme
!  coarse       |---X---|---X---|---X---|
!           {1}    0.25    0.75
!           {2}            0.75  0.25
!  dense                |-o-|-o-|
!                        {1} {2}


!  then (in 2 dimensions): the dense grid point (1,1) is
!                          v3(1,1) =   0.25*( 0.25*v(0,0) + 0.75*v(0,1) )
!                                    + 0.75*( 0.25*v(1,0) + 0.75*v(1,1) )
!

!  with the constant expressions
!        1                        3
!  a1 = --- = 0.25   and    a3 = --- = 0.75
!        4                        4

    ! interpolate in all directions first order (linearly)

!!!!!!!$omp parallel do collapse(3) private(ix,iy,iz,jx,jy,jz)
    do iz = 1, g%ng(3)
      do iy = 1, g%ng(2)
        do ix = 1, g%ng(1)
          !----------------------
          do jz = -1, 0
            do jy = -1, 0
              do jx = -1, 0
                v3(2*ix+jx,2*iy+jy,2*iz+jz) = &
                  a(jx)*a(jy)*a(jz)* t(ix+jx  ,iy+jy  ,iz+jz  ) + &
                  b(jx)*a(jy)*a(jz)* t(ix+jx+1,iy+jy  ,iz+jz  ) + &
                  a(jx)*b(jy)*a(jz)* t(ix+jx  ,iy+jy+1,iz+jz  ) + &
                  b(jx)*b(jy)*a(jz)* t(ix+jx+1,iy+jy+1,iz+jz  ) + &
                  a(jx)*a(jy)*b(jz)* t(ix+jx  ,iy+jy  ,iz+jz+1) + &
                  b(jx)*a(jy)*b(jz)* t(ix+jx+1,iy+jy  ,iz+jz+1) + &
                  a(jx)*b(jy)*b(jz)* t(ix+jx  ,iy+jy+1,iz+jz+1) + &
                  b(jx)*b(jy)*b(jz)* t(ix+jx+1,iy+jy+1,iz+jz+1)
              enddo ! jx
            enddo ! jy
          enddo ! jz
          !----------------------
        enddo ! ix
      enddo ! iy
    enddo ! iz
!!!!!!!$omp end parallel do
    deallocate( t, stat=ist )
  endfunction ! enlarge_o1_m2



  status_t function reduce_linear( v, v3, g ) result( ist )
  use configuration, only: o ! output unit, 0: no output
  use configuration, only: ERROR
  use type_grid, only: grid
  implicit none
    ! parameters
    character(len=*), parameter     :: fun = ' reduce_m2: '
    ! args
    real, intent(in)          ::  v(:,:,:) ! dense grid
    real, intent(out)         :: v3(:,:,:) ! coarse grid
    type(grid), intent(in)    :: g ! the coarser grid (of the two)
    ! local vars
    integer                   :: ix, iy, iz

    ist = 0
    if( any( shape(v) /= 2 * shape(v3) ) ) then
      if(o>0) write(o,'(4A,9(" ",F0.3))') sym, fun, ERROR, &
        'array sizes are not matching with factor 2:', real(shape(v3))/real(shape(v))
      ist = 1
      return
    endif ! s(v) /= 2* s(v3)

!!!!!!!!$omp parallel do collapse(3) private(ix,iy,iz)
    do iz = 1, g%ng(3)
      do iy = 1, g%ng(2)
        do ix = 1, g%ng(1)
          v3(ix,iy,iz) = 0.125 * ( v(2*ix-1,2*iy-1,2*iz-1) + v(2*ix,2*iy-1,2*iz-1) + &
                                   v(2*ix-1,2*iy  ,2*iz-1) + v(2*ix,2*iy  ,2*iz-1) + &
                                   v(2*ix-1,2*iy-1,2*iz  ) + v(2*ix,2*iy-1,2*iz  ) + &
                                   v(2*ix-1,2*iy  ,2*iz  ) + v(2*ix,2*iy  ,2*iz  ) )
        enddo ! ix
      enddo ! iy
    enddo ! iz
!!!!!!!!$omp end parallel do
  endfunction ! reduce_m2




  subroutine fft_filter(arr, mesh_refinement)
  use FFT_tools, only: fft
  use constants, only: Pi
    real, intent(inout)        :: arr(:,:,:)
    real, intent(in)           ::mesh_refinement

    complex                    :: tmp_c(ubound(arr, 1), ubound(arr, 2), ubound(arr, 3))
    complex                    :: tmp_d(ubound(arr, 1), ubound(arr, 2), ubound(arr, 3))
    real                       :: f
    integer                    :: i, j, k
    integer                    :: ni, nj, nk

    ni = ubound(arr, 1)
    nj = ubound(arr, 2)
    nk = ubound(arr, 3)
  
    write (*,*) 'bounds arr 1 ',  ubound(arr, 1), ubound(arr, 2), ubound(arr, 3)  
    
    f = 1.0 / real(size(arr))

    tmp_c = cmplx( arr, 0. )
    
    tmp_d = fft( tmp_c )
    do k=1, nk
      do j=1, nj
        do i=1, ni
          if  ((i>(ni/4) .and. i<=(ni-(ni/4)+0) ) .or. ( j>(nj/4) .and. j<=nj-(nj/4) )  .or. ( k>(nk/4) .and. k<=nk-(nk/4) ) ) then 
            tmp_d(i,j,k) = 0.
          else
            tmp_d(i,j,k) = tmp_d(i,j,k)*f
          endif
        enddo
      enddo
    enddo
    
    tmp_c = fft( tmp_d , inv=.true. )
    arr = real(tmp_c)
  endsubroutine



! Andrea Nobile 25.04.2014
! this function does an aggregation of values defined on a dense grid 
! and place the aggregates on a coarse grid
! The aggreates are computed by using a 4th degree lagrange polynomial (as enlarge_quartic)
! We want to compute integrals of the form <f_d|g_d> where _d or _c is for 
! dense or coarse. if both functions are defined on the dense grid there is no need to aggregate
! now we suppose to have the function g defined on a coarse grid g_c (i.e. the wavefunctions)
! we want to compute <f_d|g_c> with f_d being for example the potential.
! we could proceed by interpolating g_c to a dense grid by lagrange polynomials g_d = I g_c
! and compute the integrals on the dense <f_d|I g_c>
! by applying daga and noticing that the integral is real valued we get   <f_d|I g_c>* =  <g_c|I^daga f_d>
! and now the integral is performed on a coarse grid using the aggregates f_c = I^daga f_d of f_d
! The aggregation (restriction) operator is thus defined as the I^daga
! The lagrange interpolation is defined exactly as in the enlarge_quadratic/quartic,
! CASE OF QUADRATIC INTERPOLATION/(RESTRICTION/AGGREGATION)
! The values of the interpolated function at points p and q , when using quadratic interpolation, 
! are defined by the the 3 values defined on the coarse grid a, b, and c
! 
!                       
! coarse       *   a   b   c   *    
! dense       * * * * p q * * * *
!                     
! 
! when aggregating, the value at points a, b, c must depend from the values at points p, q
! 
! The weigths assigned to the points p, q for the point a, b and c are computed in a straigthforward manner 
! Take point a as an example:
! the contribution of the interploation on p, q from point a is the value of the lagrange basis function at points p and q. 
! The lagrange basis functions are usually written as a small ell (l_a) with an index, now a, to indicate that it is 1 on a and 0 on b and c.
! This relation holds : l_j(x_i) = delta_{ij}
! We thus have that l_a(x_p) is the weight that must be assigned to point p when aggregating for point a
! The weights are rescaled by a volume factor of 1/8 to account for the different dV when integrating.
! One has to notice that points p and q take contrib from a, b, and c and as a consequence for example when aggregating,
! for point b, we have contributins from p, q and other 4 points, 2 on the left and 2 on the right of p, q.
! an analogous discussion applies for the quartic case
  status_t function reduce_quadratic( v, v3, g, gd ) result( ist )
  use configuration, only: o ! output unit, 0: no output
  use configuration, only: ERROR
  use type_grid, only: grid
  use boundary, only: dataexchange
  implicit none    
    real, intent(in)          ::  v(:,:,:) ! dense grid
    real, intent(out)         :: v3(:,:,:) ! coarse grid
    type(grid), intent(in)    :: g ! the coarser grid 
    type(grid), intent(in)    :: gd  ! the denser grid 
    
    type(grid)                :: g1
    integer                   :: ix, iy, iz, jx, jy, jz, kx, ky, kz
    real, allocatable         :: t(:,:,:)
    real                      :: tarr(3, 3, 3)
    real                      :: interp_values(2, 2, 2)

    allocate( t(-1:gd%ng(1)+2,-1:gd%ng(2)+2,-1:gd%ng(3)+2), stat=ist )
    t = 0. ! init entire array and copy into the center region of t
    t(1:gd%ng(1),1:gd%ng(2),1:gd%ng(3)) = v(1:gd%ng(1),1:gd%ng(2),1:gd%ng(3))

    g1 = gd 
    g1%nh = 2 ! set the halo-size to 2
    call dataexchange( g1, t ) ! fill the halos
    
    !write(*,*) 'filtering '
    !call fft_filter(t(1:gd%ng(1), 1:gd%ng(2), 1:gd%ng(3)), 3.0)  

    !call dataexchange( g1, t ) ! fill the halos

    ist = 0
    tarr = 0.

    do iz = 1, g%ng(3)
      do iy = 1, g%ng(2)
        do ix = 1, g%ng(1)
        
          v3(ix,iy,iz) = 0.
          do kz = 1, 3
            do ky = 1, 3
              do kx = 1, 3
                
                tarr(4-kx, 4-ky, 4-kz) = 1. 
                call lagrange_quadratic(tarr, interp_values)

                do jz = 0, 1
                  do jy = 0, 1
                    do jx = 0, 1

                      v3(ix,iy,iz) = v3(ix,iy,iz) + &
                        0.125*t(2*ix-1 + (kx-2)*2+jx, 2*iy-1 + (ky-2)*2 +jy, 2*iz-1 + (kz-2)*2 + jz) * &
                        interp_values(jx+1, jy+1, jz+1)

                    enddo ! jx
                  enddo ! jy
                enddo ! jz
                tarr(4-kx, 4-ky, 4-kz) = 0. !restore the 0 

              enddo !kx  
            enddo  !ky
          enddo  !kz

          
        enddo ! ix
      enddo ! iy
    enddo ! iz

  endfunction 

  !Andrea Nobile 4.2014
  status_t function reduce_quartic( v, v3, g, gd ) result( ist )
  use configuration, only: o ! output unit, 0: no output
  use configuration, only: ERROR
  use type_grid, only: grid
  use boundary, only: dataexchange
  implicit none   
    real, intent(in)          ::  v(:,:,:) ! dense grid
    real, intent(out)         :: v3(:,:,:) ! coarse grid
    type(grid), intent(in)    :: g ! the coarser grid 
    type(grid), intent(in)    :: gd  ! the denser grid 
 
    type(grid)                :: g1
    integer                   :: ix, iy, iz, jx, jy, jz, kx, ky, kz
    real, allocatable         :: t(:,:,:)
    real                      :: tarr(5, 5, 5)
    real                      :: interp_values(2, 2, 2)
    real                      :: interp_values_multi(2, 2, 2, 5, 5, 5)

    allocate( t(-3:gd%ng(1)+4,-3:gd%ng(2)+4,-3:gd%ng(3)+4), stat=ist )
    t = 0. ! init entire array and copy into the center region of t
    t(1:gd%ng(1),1:gd%ng(2),1:gd%ng(3)) = v(1:gd%ng(1),1:gd%ng(2),1:gd%ng(3))

    g1 = gd 
    g1%nh = 4 ! set the halo-size to 4
    call dataexchange( g1, t ) ! fill the halos
    !call fft_filter(t(1:gd%ng(1), 1:gd%ng(2), 1:gd%ng(3)), 3.0)
    !call dataexchange( g1, t )

    ist = 0

    tarr = 0.

    do kz = 1, 5
      do ky = 1, 5
        do kx = 1, 5
          tarr(6-kx, 6-ky, 6-kz) = 1. 
          call lagrange_quartic(tarr, interp_values_multi(:,:,:, kx, ky, kz))
          tarr(6-kx, 6-ky, 6-kz) = 0. 
        enddo
      enddo
    enddo

    do iz = 1, g%ng(3)
      do iy = 1, g%ng(2)
        do ix = 1, g%ng(1)
        
        v3(ix,iy,iz) = 0.
        do kz = 1, 5
          do ky = 1, 5
            do kx = 1, 5
              
              do jz = 0, 1
                do jy = 0, 1
                  do jx = 0, 1

                    
                    v3(ix,iy,iz) = v3(ix,iy,iz) + &
                      0.125*t(2*ix-1 + (kx-3)*2+jx, 2*iy-1 + (ky-3)*2 +jy, 2*iz-1 + (kz-3)*2 + jz) * &
                      interp_values_multi(jx+1, jy+1, jz+1, kx, ky, kz)


                  enddo ! jx
                enddo ! jy
              enddo ! jz

            enddo !kx  
          enddo  !ky
        enddo  !kz

          
        enddo ! ix
      enddo ! iy
    enddo ! iz

  endfunction 
  
   

  function interpolationweights( Nmesh, itp ) result( weights )
  ! weights for inverse interpolation
  implicit none
    ! parameter
    character(len=*), parameter           :: fun = ' interpolationweights: '
    ! arguments
    integer, intent(in)                   :: Nmesh, itp
    real                                  :: weights(1-itp*nmesh:itp*nmesh-1)
    ! local vars
    integer                               :: k, j, m, i
    real                                  :: prod

      weights = 0.

      do k=0, nmesh-1
        do j=-(itp-1), itp

          ! evaluate l_j(k)
          prod = 1.0
          do i=-(itp-1),itp
            if( i /= j ) prod = prod*real( k-i*nmesh)/real((j-i)*nmesh)
          enddo ! i

          m = j*nmesh - k
          if( abs(m) < itp*nmesh ) weights( m ) = prod
        enddo ! j
      enddo ! k

  endfunction ! interpolationweights



  status_t function test_me( ) result( ist )
  use configuration, only: o ! output unit, 0: no output
  use type_grid, only: grid, set

  implicit none
    ! parameter
    character(len=*), parameter           :: fun = ' test: '
    integer, parameter                    :: Natoms = 2
    ist = 0
  endfunction test_me


  status_t function test( )
    test = test_me()
  endfunction 


endmodule ! interpolation

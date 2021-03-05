#include "config.h"

! #define DEBUG
! #define FULL_DEBUG

!! @author Paul Baumeister
!! @version 4.04
!!
!! generates the potentials for the 1dim radial grid
module radial_potential
! use type_rgrid, only: rgrid
implicit none  
  private !! default for this module namespace
  character(len=*), parameter, private :: sym = 'rPOT' !! module symbol

  public :: xc_functional
  public :: Hartree_potential
#ifdef EXTENDED
  public :: test
#endif

  contains

  status_t function xc_functional( spin, fpirho, Vxc, Exc, rgd, rgr, rgdrdi, key ) result( ist )
  use constants, only: Pi
  use density_functionals, only: xc_potential, is_gradient_functional
  use gga_tools, only: derive_radial_function
  implicit none
    ! parameter
    character(len=*), parameter     :: fun = 'xc_functional: '
    ! arguments
    integer, intent(in)             :: spin ! 0:none, 1:polarized
    real, intent(in)                :: fpirho(1:,-spin:) !! 4*pi*density(r)
    real, intent(inout)             :: Vxc(1:,-spin:) !! xc-potential(r)
    real, intent(inout)             :: Exc(1:) !! xc-energy(r)
    real, intent(in)                :: rgd, rgr(1:), rgdrdi(1:) !! radial grid:, anisotropy, grid, derivative w.r.t index
    integer, intent(in), optional   :: key     !! key for xc-functional
    ! local vars
    real                            :: a(size(fpirho,1),1,1,-spin:2+3*spin) ! density(r)
    real                            :: da(size(fpirho,1),1,1,1:3,-1:1), d12(size(fpirho,1),1:2)
    integer                         :: ir, is

#ifdef DEBUG
    if( ubound( fpirho, 2 ) /= spin ) stop 'rPOT xc_functional: dim#2 of 4PiRHO is wrong!'
    if( ubound( Vxc, 2 ) /= spin ) stop 'rPOT xc_functional: dim#2 of VXC is wrong!'
#endif

    do is = -spin, spin
      a(:,1,1,is) = fpirho(:,is)/(4.*Pi)
    enddo ! is

    if( is_gradient_functional( key ) ) then
      if( size(rgdrdi,1) < size(fpirho,1) ) stop 'rPOT xc_functional: pass more DRDIs!'
      da = 0. ! init gradients

      if( spin > 0 ) then
        do is = -spin, spin
          call derive_radial_function( 2, a(:,1,1,is), d12(:,1), d12(:,2), d=rgd, drdi=rgdrdi(1:) )
          da(:,1,1,1,is) = abs( d12(:,1) ) ! |grad rho|
          da(:,1,1,2,is) = d12(:,2)*da(:,1,1,1,0) ! gggr = drr*dagrr = drr^2 * ddrr / agr = ddrr * agr
          da(:,1,1,3,is) = d12(:,2) + 2.*d12(:,1)/rgr(:) ! Laplacian
        enddo ! is
      else  ! spin
        call derive_radial_function( 2, a(:,1,1,0), d12(:,1), d12(:,2), d=rgd, drdi=rgdrdi(1:) )
        da(:,1,1,1, 0) = abs( d12(:,1) ) ! |grad rho|
        da(:,1,1,2, 0) = d12(:,2)*da(:,1,1,1,0) ! gggr = drr*dagrr = drr^2 * ddrr / agr = ddrr * agr
        da(:,1,1,3, 0) = d12(:,2) + 2.*d12(:,1)/rgr(:) ! Laplacian
        do is = -1, 1, 2
          da(:,1,1,1,is) = 0.5*da(:,1,1,1,0) ! |grad rho|
          da(:,1,1,2,is) = .25*da(:,1,1,2,0) ! gggr = drr*dagrr = drr^2 * ddrr / agr = ddrr * agr
          da(:,1,1,3,is) = 0.5*da(:,1,1,3,0) ! Laplacian
        enddo ! is
      endif ! spin

    endif ! calculate gradients

    ist = xc_potential( rho=a(:,:,:,-spin:spin), vxc=a(:,:,:,1+spin:1+3*spin), exc=a(:,:,:,2+3*spin), key=key, drho=da )

    do is = -spin, spin
      Vxc(1:,is) = a(:,1,1,1+2*spin+is)
    enddo ! is
    Exc(1:) = a(:,1,1,2+3*spin)

    Vxc(1,:) = 0. ; Exc(1) = 0. ! set to zero at the origin
#ifdef DEBUG_GGA
    open( unit=13, file='gga_vals' )
    do ir = 2, size(a,1)
      write(13,'(F16.9,9ES16.6)') rgr(ir), a(ir,1,1,1), da(ir,1,1,1,:)
    enddo ! ir
    close(13)
!     stop 'DEBUG_GGA radial_potentials line 68'
#endif
  endfunction xc_functional


  status_t function Hartree_potential( g, rho, vH ) result( ist )
  use type_rgrid, only: rgrid
  implicit none
    character(len=*), parameter     :: fun = ' Hartree_potential: '
    integer, parameter              :: ell = 0 ! fixed here
    ! arguments
    type(rgrid), intent(in)         :: g !! radial grid descriptor
    real, intent(in)                :: rho(0:) !! 4*Pi*density(r)
    real, intent(inout)             :: vH(0:)  !! Hartree-potential(r)
    ! local vars

    integer :: ir
    real    :: vh10, vh20

    vh20 = 0.
    do ir = 0, g%imx
      vh20 = vh20 + rho(ir)*g%r(ir)*g%dr(ir)
    enddo ! ir

    ir = 0
    vH(ir) = vh20

    vh10  = 0.
    do ir = 1, g%imx
      vh10 = vh10 + rho(ir)*g%r2dr(ir) !  g%r(ir)**2*g%dr(ir)
      vh20 = vh20 - rho(ir)*g%r(ir)*g%dr(ir)
      vH(ir) = vh10/g%r(ir) + vh20
    enddo ! ir
    ist = 0
  endfunction Hartree_potential


!   subroutine Hartree_potential( g, rho, vH )
!   use type_rgrid, only: rgrid
!   implicit none
!     character(len=*), parameter     :: fun = ' Hartree_potential: '
!     integer, parameter              :: ell = 0 ! fixed here
!     ! arguments
!     type(rgrid), intent(in)         :: g !! radial grid descriptor
!     real, intent(in)                :: rho(0:) !! 4*pi*density(r)
!     real, intent(inout)             :: vH(0:)  !! Hartree-potential(r)
!     ! local vars
!     real                            :: v1(0:g%imx)
!     real                            :: v2(0:g%imx)
!     real                            :: r1rho(0:g%imx)
!     real                            :: r2rho(0:g%imx)
! 
! 
!     r2rho = g%r**2*rho
!     ! determine hartree potential
!     !                                r
!     ! perform outwards integration : | 4*pi*(r'')**2*rho(r'')*dr''
!     !                                0
!     r2rho(0 ) = 0.
!     ! integrate inwards
!     call simint( r2rho, v1, 0, g%imx, g%dr )
!     !                              rmax
!     ! perform inwards integration : |   4*pi*r''*rho(r'')*dr''
!     !                               r
!     r1rho = g%r*rho
!     ! integrate outwards
!     call simint( r1rho, v2, g%imx, 0, g%dr )
! 
!     vH(0 ) =                    v2(0 )
!     vH(1:) = ( v1(1:)/g%r(1:) + v2(1:) )
! 
!   endsubroutine Hartree_potential
! 




! 
!   subroutine simint( fin, fout, istrt, iend, dr )
! !***********************************************************************
! !
! !     revised by D.Frielinghaus 29.08.97
! !
! !     this subroutine performs an ordinary 3-point simpson integration
! !     of the integral equation
! !                                   r
! !                       fout(r) =      fin(r') dr'
! !                                   0
! !
! !     by outwards integration or
! !                                  rmax
! !                       fout(r) =        fin(r') dr'
! !                                   r
! !
! !     by inwards integration respectively
! !
! !     inwards integration is performed if irstrt .gt. irend
! !
! !     before applying simpson integration 2 fout has to be known at 2
! !     grid points. fout(grid=1) = 0 and fout(grid=2) is determined
! !     by a 6-point lagrange integration with coefficients given in
! !     abramowitz, stegun, handbook of mathematical functions, handbook
! !     of mathematical functions, 1970,p 915.
! !
! !                                    stefan blu:gel ,issp, 1989
! !
! !***********************************************************************
!     ! parameter
!     character(len=*), parameter :: fun = ' simint: '
!     ! Lagrangian integration coefficients AKN
!     real, parameter         :: D = 1440. ! DENOMINATOR
!     real, parameter         :: AKN(6) = &
!       (/475.,1427.,-798.,482.,-173.,27./)
!     ! arguments
!     real, intent(in)      :: fin(0:)
!     real, intent(out)     :: fout(0:)
!     integer, intent(in)   :: istrt, iend
!     real, intent(in)      :: dr(0:)
!     ! local vars
!     integer                   :: k, i
! 
!     ! initialize first grid point with zero
!     fout(istrt) = 0
! 
!     if( istrt < iend ) then
!       ! perform outward integration
!       ! determine fout(grid=2) by 6-point lagrangian integration
!       fout(istrt+1) = 0
!       do k = 1, 6
!         fout(istrt+1) = fout(istrt+1) + &
!           AKN(k) * fin(istrt-1+k) * dr(istrt-1+k)
!       enddo ! k
!       fout(istrt+1) = fout(istrt+1) / D
! 
!       ! evaluate fout by 3-point simpson rule
!       do i = istrt+2, iend, +1
!         fout(i) = fout(i-2) + ( fin(i-2) * dr(i-2) &
!                           + 4 * fin(i-1) * dr(i-1) &
!                               + fin(i  ) * dr(i)   ) / 3.
!       enddo ! i
! 
!     elseif( istrt > iend ) then
!       ! perform inward integration
!       ! determine fout by 6-point lagrangian integration
!       fout(istrt-1) = 0
!       do k = 1, 6
!         fout(istrt-1) = fout(istrt-1) + &
!           AKN(k) * fin(istrt+1-k) * dr(istrt+1-k)
!       enddo ! k
!       fout(istrt-1) = fout(istrt-1) / D
!       ! evaluate fout(grid=2) by 3-point simpson rule
!       do i = istrt-2, iend, -1
!         fout(i) = fout(i+2) + ( fin(i+2) * dr(i+2) &
!                           + 4 * fin(i+1) * dr(i+1) &
!                               + fin(i  ) * dr(i)   ) / 3.
!       enddo ! i
! 
!     else  ! istrt > iend .or. istrt < iend
!       return
!     endif ! istrt > iend .or. istrt < iend
! 
!   return
!   endsubroutine simint

#ifdef EXTENDED
!+ extended

  status_t function test( )
    write(*,*,iostat=test) __FILE__,' no module test implemented!'
  endfunction ! test

!- extended
#endif

endmodule ! radial_potential

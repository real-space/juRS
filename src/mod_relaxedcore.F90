#include "config.h"

! #define DEBUG
! #define FULL_DEBUG


!! @author Paul Baumeister
!! @version 4.00
!!
!! translates smooth quantities such as
!! smooth electrostatic potentials or
!! smooth densities to a radial grid
module relaxedcore
  use configuration, only: o ! output unit
implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: sym = 'RLX' !! module symbol

  public :: grid2radial
#ifdef EXTENDED
  public :: test
#endif

#ifdef DEBUG
  integer, parameter                :: u = 6 ! output
#else
  integer, parameter                :: u = 0 ! no debug output
#endif

  contains

  status_t function grid2radial( g, data, origins, r, file, rdata, comm ) result( ist )
  ! uses a bessel tranform with j0
  use constants, only: Pi, Y00 => ONESQRTFOURPI
  use type_grid, only: grid
  use MPItools, only: MPIallsum, MPImaster
  use unitsystem, only: Ang, Ang_
  implicit none
    ! parameter
    character(len=*), parameter           :: fun = ' grid2radial: '
    integer, parameter                    :: WGT = 0 ! index
    ! arguments
    type(grid), intent(in)                  :: g !! grid descriptor
    real, intent(in)                        :: data(:,:,:) !! data on the grid
    real, intent(in)                        :: origins(1:,1:) !! origin of the atomic core
    real, intent(in)                        :: r(0:) !! radial grid
    character(len=*), intent(in), optional  :: file !! filename to write to
    real, intent(out), optional             :: rdata(0:) !! spherical part of data w.r.t to the origin
    MPI_Comm, intent(in), optional          :: comm !! communicator
    ! local vars
    integer               :: i1, i2, i3, ipi, npi
    real                  :: rv(3), rs, rv2(3), r2s, ori(3)
    integer               :: ir, iq, nq
    real                  :: qmax, dq, d, ha, dV, rmx2, rmax
    real, allocatable     :: fq(:), q(:)
#ifdef DEBUG
    real :: sqrt2pi ! = sqrt(2./Pi) ! prefactor for Bessel j0 transform
    if( any( shape(data) /= g%ng(1:3) ) ) stop 'RLX grid2radial: a dim of DATA does not match g%NG!'
    if( size(origins,1) /= 3 ) stop 'RLX grid2radial: dim#1 of ORIGINS must be 3.'
#endif


    rmax = r(ubound(r,1)) ! largest value on the radial grid
    rmx2 = rmax**2

    dV = product(g%h(1:3)) ! volume element
    ha = dV**(1./3.) ! geom. average grid spacing
    if( dV <= 0. ) stop 'RLX grid2radial: grid spacing g%H may not be zero!'
    qmax = Pi/ha ! maximum frequency expected
    nq   = floor(rmax/ha*1.5) !  (raised by 50%)
    dq   = qmax/nq

    if(o>0) write(o,'(3A,9(F0.3,2A))') sym, fun, 'Rmax = ', Rmax*Ang, Ang_,' average grid spacing = ', ha*Ang, Ang_
    if(o>0) write(o,'(3A,I0,9(A,F0.3))') sym, fun, 'j0-functions ', nq, ' x ', dq, ' sqRy = ', qmax, ' sqRy'

    allocate( q(0:nq), fq(0:nq), stat=ist ) ; fq = 0. ! init
    do iq = 0, nq
      q(iq) = iq*dq ! set up q-scale
    enddo ! iq

    npi = size(origins,2) ! number of periodic images
    do ipi = 1, npi ; ori = origins(1:3,ipi)-g%off(1:3) ! for each periodic image
      do i3 =     1, g%ng(3) ; rv(3)  = i3*g%h(3) - ori(3) ; rv2(3) = rv(3)*rv(3)
        do i2 =   1, g%ng(2) ; rv(2)  = i2*g%h(2) - ori(2) ; rv2(2) = rv(2)*rv(2)
          do i1 = 1, g%ng(1) ; rv(1)  = i1*g%h(1) - ori(1) ; rv2(1) = rv(1)*rv(1)
            !---------------------------------
            r2s = rv2(1) + rv2(2) + rv2(3)
!           if(u>0) write(u,'(3A,9(F10.3,A))') sym, fun, 'R^2 =', r2s, ' aB'
            if( r2s < rmx2 ) then
              !---------------------------------
              rs = sqrt( r2s )
              ! Bessel transform the function with j0(qr)
              fq = fq + data(i1,i2,i3) * bessel_j0( q(:)*rs )
              !---------------------------------
            endif ! r < Rmax
            !---------------------------------
          enddo ! i1
        enddo ! i2
      enddo ! i3
    enddo ! ipi

    if( present(comm) ) call MPIallsum( fq, comm )

#ifdef DEBUG
    sqrt2pi = sqrt(2./Pi) ! prefactor for Bessel j0 transform
    ! scale
    fq = fq * ( sqrt2pi * Y00 * dV )

    if(u>0) then
      write(u,'(3A,F10.6,9A)') sym, fun, 'spectrum, dq =', dq, ' /aB'
      do iq = 0, nq
        write(u,'(101ES16.6)') q(iq), fq(iq)
      enddo ! iq
    endif ! u>0

    ! scale
    fq = fq * ( sqrt2pi * Y00 * dq )
#else
    ! scale (both factors at a time)
    fq = fq * ( dV * dq /( 2.*Pi**2 ))
#endif

    if( present( rdata ) ) then
      do ir = 0, min(ubound(r,1),ubound(rdata,1)) ! transform back
        rdata(ir) = sum( q(:)**2 * bessel_j0( q(:)*r(ir) ) * fq(:) )
      enddo ! ir
    endif ! present rdata

    if( present( file ) ) then
      if( MPImaster(comm) ) then
        open(unit=144,file=file,iostat=ist)
        if( ist /= 0 ) then
          if(o>0) write(o,'(9A)') sym, fun, 'opening "', trim(file), '" failed.'
          return
        endif ! ist /= 0
        write(unit=144,fmt='(9A)') '# ', sym, fun, ' (atomic units)'
        do ir = 0, ubound(r,1) ! transform back
          d = sum( q(:)**2 * bessel_j0( q(:)*r(ir) ) * fq(:) )
          write(unit=144,fmt='(9ES24.12)') r(ir), d
        enddo ! ir
        close(unit=144,iostat=ist)
      endif ! MPImaster task
    endif ! present file

    deallocate( q, fq, stat=ist )

  contains
    ! ell=0 spherical Bessel function
    real elemental function bessel_j0( qr )
    implicit none
      real, intent( in ) :: qr
      bessel_j0 = 1.0
      if( abs( qr ) < 1.E-9 ) return
      bessel_j0 = sin( qr )/qr
    endfunction ! bessel_j0

  endfunction ! grid2radial


#ifdef EXTENDED
!+ extended

  status_t function test( ) result( ist )
  use type_grid, only: grid, set
  use toolbox, only: radial_hist
  use constants, only: Pi
  implicit none
    ! parameter
    character(len=*), parameter           :: fun = ' test: '
    iounit_t, parameter                   :: o = 6 ! output
    ! arguments
    ! local vars
    real, allocatable     :: dat(:,:,:), r(:), fr(:)
    real                  :: dr, ori(3,1) = 4.5
    type(grid)            :: g
    integer               :: nr, ir

#ifndef NOMPI
    call MPI_Init( ir )
#endif

    g = set( (/9.,9.1,9./), (/27,28,29/), nspins=1 )
    allocate( dat( g%ng(1),g%ng(2),g%ng(3) ), stat=ist ) ; dat = 0.
    dat(13,13:14,14) = 1.0
!     dat = 1.0 ! gives a step function

    ! a radial (equidistant) grid
    nr = 800
    allocate( r(0:nr), fr(0:nr), stat=ist )
    dr = 0.01
    do ir = 0, nr
      r(ir) = ir*dr
    enddo ! ir

    ist = grid2radial( g, dat, origins=ori, r=r, rdata=fr )

    do ir = 0, nr
      write(7,*) r(ir), fr(ir)
    enddo ! ir
    write(*,*) 'rgrid integral =', 4*Pi*sum( fr * r**2 ) * dr
    write(*,*) ' grid integral =', sum( dat ) * product(g%h)
    ist = radial_hist( dat, g%h, origin=(/4.5,4.5,4.5/), ellmax=2, unit=8 )
    stop 'xmgrace fort.7 fort.8 &'

  endfunction ! test

!- extended
#endif

endmodule ! relaxedcore

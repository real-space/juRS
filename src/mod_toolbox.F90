#include "config.h"

! #define DEBUG

!! @author Paul Baumeister
!! @version 3.0
!!
!! tools for debug
module toolbox
  use configuration, only: o ! output unit, 0: no output
! use harmonics, only: Ylm_rl
implicit none
  private ! default for this module namespace

  public :: convert2lowercase
  public :: operator(+)

#ifdef EXTENDED
  public :: test

  public :: Bessel_j
  public :: radialmesh
  public :: radial_hist
  public :: print_polar
  public :: write_bmp_file
  public :: write_vtk_file
  public :: write_to_file
  public :: visualize_density
  public :: build_dep_tree
  public :: mail_notify

  interface nl_index ! nl combindex of enn, ell or of a string
    module procedure nl_index_n_l, nl_index_str
  endinterface

  interface str_nl ! string from enn,ell or nl combindex
    module procedure str_nl_index, str_nl_n_l
  endinterface

  interface to_grid ! reshape in(:) --> out(:,:,:)
    module procedure to_grid_r
  endinterface

  interface radial_hist ! in : function on a grid and grid spacing
               ! out: decomposition onto one or a set of radial functions
    module procedure radial_hist_j0!, radial_hist_ilm, radial_hist_ellmax
  endinterface

  interface write_to_file
    module procedure write_to_file_r2, write_to_file_r1
  endinterface


  interface print_polar ! print complex numbers in polar representation
    module procedure print_polar_r, print_polar_short !, print_polar_c
  endinterface

  interface write_bmp_file
    module procedure write_bmp_file_r, write_bmp_file_c
  endinterface

  character(len=*), parameter, private :: sym = 'TBX' !! module symbol
#endif

  interface operator(+)
    module procedure cat_si, cat_is, cat_2s
  endinterface

  contains

#ifdef EXTENDED
!+ extended

  real function bessel_j( j, x )
    integer, intent(in) :: j
    real, intent(in)    :: x

    complex :: expx
    selectcase( j )
    case( :-1 )
      stop 'BesselFunctions: j < 0 impossible value.'
    case( 0 )
      ! sin(x) / x
      if( x /= 0. ) then
        bessel_j = sin(x)/x
      else  ! x /= 0.
        bessel_j = 1.
      endif ! x /= 0.
    case( 1 )
      ! sinx/x^2 - cos(x)/x
      if( x /= 0. ) then
        expx = exp( cmplx(0.,1.)*x )
        bessel_j = ( aimag(expx) - x*real(expx) )/(x*x)
      else  ! x /= 0.
        bessel_j = 0.
      endif ! x /= 0.
    case( 2 )
      ! (3/x^2-1)sin(x)/x - 3cos(x)/x^2
      if( x /= 0. ) then
        expx = exp( cmplx(0.,1.)*x )
        bessel_j = ( (3.-x*x)*aimag(expx) - 3.*x*real(expx) )/(x*x*x)
      else  ! x /= 0.
        bessel_j = 0.
      endif ! x /= 0.
    case default
      bessel_j = 0.
      write(*,'(A,I2)') 'Warning: Bessel_j function not implemented for j=', j
    endselect ! j

  endfunction ! bessel_j

  integer function nl_index_n_l( n, l )
    integer, intent(in)   :: n, l
#ifdef DEBUG
    if( n < 1 )  stop 'nl_index: n < 1 not allowed.'
    if( l < 0 )  stop 'nl_index: l < 0 not allowed.'
    if( l >= n ) stop 'nl_index: l >= n not allowed.'
#endif
    nl_index_n_l = l+1+n*(n-1)/2
  endfunction ! nl_index_n_l

  integer function nl_index_str( str )
    character(len=2), intent(in) :: str

    integer :: n, l
    character :: ell_char
    status_t :: ios

    read( unit=str, fmt='(I1,A1)', iostat=ios ) n, ell_char
    if( ios /= 0 ) then
      if(o>0) write(o,*) 'nl_index_str: cannot read "', str, '".'
      stop 'nl_index_str: bad input'
    endif ! ios /= 0

    selectcase( ell_char )
    case( 's', 'S' ) ; l = 0
    case( 'p', 'P' ) ; l = 1
    case( 'd', 'D' ) ; l = 2
    case( 'f', 'F' ) ; l = 3
    case( 'g', 'G' ) ; l = 4
    case( 'h', 'H' ) ; l = 5
    case( 'i', 'I' ) ; l = 6
    case default ; stop 'nl_index_str: unknown ell_char'
    endselect

    if( n < 1 )  stop 'nl_index: n < 1 not allowed.'
    if( l < 0 )  stop 'nl_index: l < 0 not allowed.'
    if( l >= n ) stop 'nl_index: l >= n not allowed.'

    nl_index_str = l+1+n*(n-1)/2
  endfunction ! nl_index_str

  character(len=2) function str_nl_index( nl )
    integer, intent(in) :: nl

    character, parameter :: ell_char(0:5) = (/'s','p','d','f','g','h'/)
    integer :: enn, ell
    enn = enn_nl( nl, l=ell )
    if( ell > 5 ) stop 'ell_char(0:5) not long enough.'
    write(unit=str_nl_index,fmt='(I1,A1)') enn, ell_char(ell)
  endfunction ! str_nl_index

  character(len=2) function str_nl_n_l( n, l )
    integer, intent(in) :: n, l
    character, parameter :: ell_char(0:5) = (/'s','p','d','f','g','h'/)
    if( l > 5 ) stop 'ell_char(0:5) not long enough.'
    write(unit=str_nl_n_l,fmt='(I1,A1)') n, ell_char(l)
  endfunction ! str_nl_n_l

  integer function enn_nl( nl, l )
    integer, intent(in) :: nl
    integer, intent(out), optional :: l

    integer :: enn
    if( nl < 1 ) stop 'enn_nl: nl < 1 not allowed.'

    enn = 1
!         do while( nl_index(enn,enn-1) < nl )
!       nl_index( enn, ell )  = ell+1+enn*(enn-1)/2
!       nl_index( enn, enn-1) = enn-1+1+enn*(enn-1)/2 = enn*(enn+1)/2
    do while( enn*(enn+1) < 2*nl )
      enn = enn + 1
    enddo
    enn_nl = enn
    if( present( l ) ) then
!           l = nl - nl_index( enn, 0 )
!           nl_index( enn, 0 ) = 1+enn*(enn-1)/2
        l = nl - ( 1 + enn*(enn-1)/2 )
    endif ! present( l )
  endfunction ! enn_nl

  elemental string_t function print_polar_c( c ) result( str )
  use constants, only: Pi
    complex, intent(in) :: c

    real:: a, phi
    a = abs( c ) ; phi = 0.
    if( a > 0. ) phi = aimag( log(c) )/Pi
    phi = modulo( phi, 1.0 )
    write(unit=str,fmt='(ES18.10E3,A6,F7.4)') a, '*e^ip*', phi
!     write(unit=s,fmt='(F18.14,A6,F7.4)') a, '*e^ip*', phi
  endfunction ! print_polar

  elemental string_t function print_polar_r( r ) result( str )
    real, intent(in) :: r
    write(unit=str,fmt='(F16.6)') r
  endfunction ! print_polar

  elemental character(len=16) function print_polar_short( c ) result( str )
    complex, intent(in)             :: c
    write(unit=str,fmt='(ES10.2)') abs(c)
  endfunction ! print_polar

  status_t function to_grid_r( s, g ) result( ist )
    real, intent(in)      :: s(:) ! values as one vector
    real, intent(out)     :: g(:,:,:) ! values a 3D array

    ist = size(s) - size(g)
    if( ist /= 0 ) return
    g = reshape( s, shape(g) )
  endfunction ! to_grid

  status_t function radial_hist_j0( data, hg, origin, ellmax, unit, file, comm ) result( ist )
  ! uses a bessel tranform with j0
  use constants, only: Pi, Y00 => ONESQRTFOURPI
  use harmonics, only: Xlmax_rl!, ellmax => ELLMAX_IMPLEMENTED
  use MPIconst, only: MPI_COMM_SELF
  use MPItools, only: MPIallsum, operator(.MPImax.), MPImaster
    real, intent(in)                      :: data(:,:,:)
    real, intent(in)                      :: hg(3)
    real, intent(in)                      :: origin(3)
    integer, intent(in)                   :: ellmax
    integer, intent(in), optional         :: unit  ! unit to write to
    character(len=*), intent(in), optional:: file  ! file to open
    MPI_Comm, intent(in), optional        :: comm

    character(len=*), parameter :: fun = ' radial_hist: '
    integer, parameter    :: WGT=0 ! index
    real, allocatable     :: flq(:,:), qs(:), fq(:), rqs(:), xlm(:)
    integer, parameter    :: ELLS(1:49) = (/0,1,1,1,2,2,2,2,2, &
      3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,6,6,6/)
    real                  :: r2l(1:49)
    integer               :: lmax, mlm
    integer               :: ng(3), i1, i2, i3 !, id
    real                  :: rv(3), rs, rv2(3), r2s
    iounit_t              :: unt
    logical               :: file_opened = .false.
    integer               :: nr, ir, ie, iq, nq
    real                  :: rmin, rmax, e2, edges(8), dr, r
    real                  :: qmax, q, dq, qr, sqrt2pi, m0
    MPI_Comm              :: icomm
    icomm = MPI_COMM_SELF ; if( present( comm ) ) icomm = comm
    unt = 19 ; if( present( unit ) ) unt=unit

    if( present( file ) ) then ! touch file
      open(unit=unt,file=file,action='write',status='unknown',iostat=ist)
      if( ist /= 0 ) write(*,'(5A,I0)') sym, fun, 'failed to touch "', trim(file), '", ios = ', ist
      if( ist /= 0 ) return
      close(unit=unt,iostat=ist)
    endif ! present file

    ng = shape( data )
    write(*,*) "ng", ng

    ! edges
    ie = 0
    do i3 = 0, 1 ; do i2 = 0, 1 ; do i1 = 0, 1
      ie = ie+1
      e2 = sum(  ( (1+(/i1,i2,i3/)*(ng(1:3)-1))*hg(1:3) - origin(1:3) )**2 )
      edges(ie) = sqrt( e2 )
    enddo ; enddo ; enddo ! i1 i2 i3

    rmax = maxval(edges) .MPImax. icomm ! largest radius
    rmin = -( (-minval(edges)) .MPImax. icomm ) ! smallest radius
    write(*,*) "rmin!!!", rmin

    dr = product(hg(1:3))**(1./3.)
    qmax = Pi/dr
    nr = rmax/dr
!     nq = 2*nr ! nint(nr*1.3)
    nq = nint(nr*1.333)

    dq = qmax/nq

    nr = rmin/dr ! display up to here

    sqrt2pi = sqrt(2./Pi)

    lmax = max(0,min(ellmax,6))
    mlm = (lmax+1)**2
    allocate( qs(0:nq), rqs(0:nq), stat=ist )
    do iq = 0, nq
      qs(iq) = iq*dq ! equidistant q-grid
    enddo ! iq

    if( lmax > 0 ) then
      allocate( flq(mlm,0:nq), xlm(mlm) ) ; flq = 0. ! init
      do i3 = 1, ng(3) ;     rv(3)  = i3*hg(3) - origin(3) ; rv2(3) = rv(3)*rv(3)
        do i2 = 1, ng(2) ;   rv(2)  = i2*hg(2) - origin(2) ; rv2(2) = rv(2)*rv(2)
          do i1 = 1, ng(1) ; rv(1)  = i1*hg(1) - origin(1) ; rv2(1) = rv(1)*rv(1)
            !---------------------------------
            r2s = rv2(1) + rv2(2) + rv2(3)
            if( r2s > 1E-12 ) then
              rs = sqrt( r2s )
              ! Bessel transform the function with j0(qr)*r^ell instead of j_ell(qr)
              r2l(1:mlm) = r2s**(-ells(1:mlm))
              do iq = 0, nq
                qr = iq*dq*rs
                flq(:,iq) = flq(:,iq) + (data(i1,i2,i3) * Bessel_j0( qr )) * Xlmax_rl( lmax, rv ) * r2l(1:mlm)
              enddo ! iq
            else
              ! Bessel transform the function with j0(qr)*r^ell instead of j_ell(qr)
              do iq = 0, nq
                flq(1,iq) = flq(1,iq) + data(i1,i2,i3) * 1.0 * Y00 ! Bessel_j0( qr )
              enddo ! iq
            endif ! r2s > eps
            !---------------------------------
          enddo ! i1
        enddo ! i2
      enddo ! i3
      flq = flq * ( sqrt2pi * hg(1)*hg(2)*hg(3) )
      call MPIallsum( flq, icomm )
    else  ! lmax > 0
      allocate( fq(0:nq), xlm(1) ) ; fq = 0. ! init
      ! lmax == 0 ==> simplify
      do i3 = 1, ng(3) ;     rv(3)  = i3*hg(3) - origin(3) ; rv2(3) = rv(3)*rv(3)
        do i2 = 1, ng(2) ;   rv(2)  = i2*hg(2) - origin(2) ; rv2(2) = rv(2)*rv(2)
          do i1 = 1, ng(1) ; rv(1)  = i1*hg(1) - origin(1) ; rv2(1) = rv(1)*rv(1)
            !---------------------------------
            r2s = rv2(1) + rv2(2) + rv2(3)
            rs = sqrt( r2s )
            rqs = rs * qs
            fq = fq + data(i1,i2,i3) * Bessel_j0( rqs )
            !---------------------------------
          enddo ! i1
        enddo ! i2
      enddo ! i3
      fq = fq * ( sqrt2pi * hg(1)*hg(2)*hg(3) ) * Y00
      call MPIallsum( fq, icomm )
    endif ! lmax > 0

    if( .not. MPImaster( icomm ) ) return


    if( present( file ) ) then ! open the file
      open( unit=unt, file=file, action='write', status='unknown', iostat=ist )
      file_opened = ( ist == 0 )
#ifdef DEBUG
      if( ist /= 0 ) write(*,'(5A,I0)') sym, fun, 'failed to open "', trim(file), '", ios = ', ist
#endif
      if( ist /= 0 ) return
    endif ! present file

    m0 = 0. ! init monopole moment
    ! and write to the file
    do ir = 0, nr  * 16  ! sample more r values
      r = ir*dr    / 16. ! on a denser grid
      if( lmax > 0 ) then
        xlm = 0.
        do iq = 1, nq
          q = iq*dq
          qr = q*r
          ! transform back
          xlm = xlm + q * q * Bessel_j0( qr ) * flq(:,iq)
        enddo ! iq
        xlm = xlm * ( sqrt2pi * dq * Y00 ) * r**ELLS(1:mlm)
      else  ! lmax > 0
        rqs = r*qs
        xlm(1) = sum( qs(1:)**2 * Bessel_j0( rqs(1:) ) * fq(1:) ) * sqrt2pi * dq * Y00
      endif ! lmax > 0
      write(unit=unt, fmt='(101ES16.8E2)' ) r, xlm
    enddo ! ir

    write(unit=unt,fmt='(A,999F10.3)') '#   q ', qs
    if( lmax > 0 ) then
      write(unit=unt,fmt='(A,999F10.3)') '# f(q)', flq(1,:)
    else  ! lmax > 0
      write(unit=unt,fmt='(A,999F10.3)') '# f(q)', fq
    endif ! lmax > 0

    if( file_opened ) close( unit=unt, iostat=ist )

  contains
    ! ell=0 spherical Bessel function
    real elemental function Bessel_j0( qr )
      real, intent(in) :: qr
      Bessel_j0 = 1.0
      if( abs( qr ) < 1E-9 ) return
      Bessel_j0 = sin( qr )/qr
    endfunction ! Bessel_j0

  endfunction ! radial_hist_j0


  status_t function write_to_file_r2( data, r, unit, file ) result( ios )
  use constants, only: Pi, Y00 => ONESQRTFOURPI
    real, intent(in)                       :: data(0:,1:) ! radial data(ir,ilm)
    real, intent(in)                       :: r(0:) ! radial grid
    iounit_t, intent(in), optional         :: unit  ! unit to write to
    character(len=*), intent(in), optional :: file  ! file to open

    character(len=*), parameter :: fun = ' write2file: '
    iounit_t :: unt = 19
    integer :: ir

    unt = 19 ; if( present( unit ) ) unt=unit

    if( present( file ) ) then ! open the file
      open( unit=unt, file=file, action='write', status='unknown', iostat=ios )
#ifdef DEBUG
      if( ios /= 0 ) write(*,'(5A,I3)') sym, fun, 'failed to open "', trim(file), '", ios =', ios
#endif
      if( ios /= 0 ) return
    endif ! present file

    do ir = 1, min(ubound(data,1),ubound(r,1))
      if( unt > 0 ) write( unit=unt, fmt='(F10.6,99F16.8)', IOstat=ios ) r(ir), data(ir,1:)*Y00
    enddo ! ir

    if( present( file ) ) close( unit=unt, IOstat=ios )
  endfunction ! write_to_file


  status_t function write_to_file_r1( data, r, unit, file ) result( ios )
    real, intent(in)                      :: data(0:) ! radial data(ir) ,ilm==1
    real, intent(in)                      :: r(0:) ! radial grid
    iounit_t, intent(in), optional        :: unit  ! unit to write to
    character(len=*), intent(in), optional:: file  ! file to open

    ios = write_to_file( reshape(data,(/size(data),1/)), r, unit, file )
  endfunction ! write_to_file


  status_t function radial_histogram( data, hg, origin, unit, file ) result( ist )
    real, intent(in)      :: data(:,:,:)
    real, intent(in)      :: hg(3)
    real, intent(in)      :: origin(3)
    iounit_t, intent(in), optional          :: unit  ! unit to write to
    character(len=*), intent(in), optional  :: file  ! file to open
  !! works only for spherically symmetric functions,
  !! because no integration over the angle is performed.
  !! however, it samples all the points, therefore a
  !! non-spherical function might look zig-zag-like.

    character(len=*), parameter     :: fun = ' radial_histogram: '
    real, allocatable     :: rd(:,:)
    integer               :: ng(3), i1, i2, i3, ie, ne
    real                  :: rv(3), rs, rv2(3), r2s
    iounit_t              :: unt
    logical               :: file_opened = .false.
    integer               :: np(3), ix, iy, iz
    integer               :: a(3,-1:+1), b(3,-1:+1)

    unt = 19 ; if( present( unit ) ) unt = unit

    file_opened = .false.
    if( present( file ) ) then ! open the file
      open( unit=unt, file=file, action='write', status='unknown', iostat=ist )
      if( ist == 0 ) then
        file_opened = .true.
      else  ! ist == 0
        if(o>0) write(o,'(5A,I0)') sym, fun, 'failed to open "', trim(file), '", IOstat=', ist
        return
      endif ! ist == 0
    endif ! present file

    ng(1:3) = shape(data)
    ne = product( ng(1:3) )
    allocate( rd(ne,0:1), stat=ist )

    np(:) = ng/2 ! half

    a(:,-1) = np
    b(:,-1) = 1
    a(:,+1) = np+1
    b(:,+1) = ng

    ie = 0

    ! these three outer loops ensure that
    ! the points with small distances to
    ! the origin are taken into account first, so
    ! the list of values is pre-ordered.
    do iz = -1, +1, 2
    do iy = -1, +1, 2
    do ix = -1, +1, 2

    do i3= a(3,iz), b(3,iz), iz
      rv(3)  = i3*hg(3) - origin(3)
      rv2(3) = rv(3)*rv(3)
      do i2= a(2,iy), b(2,iy), iy
        rv(2)  = i2*hg(2) - origin(2)
        rv2(2) = rv(2)*rv(2)
        do i1= a(1,ix), b(1,ix), ix
          rv(1)  = i1*hg(1) - origin(1)
          rv2(1) = rv(1)*rv(1)
          !---------------------------------
          ie = ie + 1
          r2s = rv2(1) + rv2(2) + rv2(3)
          rs = sqrt( r2s )
          rd(ie,0) = rs
          rd(ie,1) = data(i1,i2,i3)
          !---------------------------------
        enddo ! i1
      enddo ! i2
    enddo ! i3

    enddo ! ix
    enddo ! iy
    enddo ! iz

    if( ie /= ne ) stop 'TBX radial_histogram: counting error, number of elements wrong.'
    ist = write_ordered( unt, data=rd(:,0:1) )

    deallocate( rd, stat=ist )
    if( file_opened ) close( unit=unt, iostat=ist )
  endfunction ! radial_histogram


  status_t function write_ordered( unt, data ) result( ist ) ! warning: scales as N^2
    iounit_t, intent(in) :: unt
    real, intent(in)     :: data(:,1:) ! (#,:) !order after the first set.

    character(len=*), parameter     :: fun = ' write_ordered: '
    integer                         :: num(size(data,1)) ! num(#)
    integer                         :: is, in, im(1)

    if( size(data,1) < 1 ) return

    num = 0 ! init: 0 means has not been ordered yet

    do is = 1, size(data,1) ! number of data sets
      ! finds the index of the smallest element in the subset of elements
      ! where the corresponding num is still 0
      im = minloc( data(:,1), mask=(num==0) )
      in = im(1)
      num(in) = is ! stores the number and thus sets that entry > 0, so it
      ! will not be in the subset next round
      write( unit=unt, fmt='(203ES16.6E2)', iostat=ist ) data(in,:)
    enddo ! is

  endfunction ! write_ordered

  subroutine radialmesh( rmax_a_b, r, drdi )
    real, intent(inout) :: rmax_a_b(3)
    real, intent(out)   :: r(:), drdi(:)

    character(len=*), parameter :: fun  = ' radialmesh: '
    integer :: irmx, ir
    real :: ea, rpb, rmax, a, b

    irmx = size(r)
    rmax = rmax_a_b(1)
    a    = rmax_a_b(2)

    if( size(drdi) /= irmx ) then
      if(o>0) write(o,'(2A)') fun, 'size of drdi(:) wrong'
      stop 'wrong dimension'
    endif ! size(drdi) /= irmx


    if( irmx <= 25 ) then
      if(o>0) write(o,'(2A)') fun, 'number of radial grid points too small'
      stop 'irmx <= 25'
    endif ! irmx <= 25


    if( rmax <= 0. ) then
      if(o>0) write(o,'(2A)') fun, 'rmax must be positive'
      stop 'rmax <= 0.'
    endif ! rmax <= 0.


!---> redetermine b for given rmax

    b = rmax/ ( exp(a*real(irmx-1)) - 1.0 )

!---> determine radial mesh and derivative

    rpb = b
    ea = exp(a)
    do ir = 1, irmx
      ! r(ir) = (exp(a*(ir-1))-1)*b
      r(ir) = rpb - b
      drdi(ir) = a*rpb

      rpb = rpb*ea
    enddo ! ir

    rmax_a_b(1) = r(irmx)
    rmax_a_b(3) = b

    if(o>0) write(o,'(A,2(A,ES11.4))') fun, 'r(2)     =', r(2)   , '  r(irmx)     =', r(irmx)
    if(o>0) write(o,'(A,2(A,ES11.4))') fun, 'drdi(1)  =', drdi(1), '  drdi(irmx)  =', drdi(irmx)

  endsubroutine ! radialmesh



  function a_b_of_radial_exp_grid( drdi ) result( ab )
    real, intent(in)            :: drdi(:)
    integer                     :: ab(2) !! result

    real                        :: a, b
    ! drdi(1)= a*b
    ! drdi(2)= a*exp(a)*b
    a = log( drdi(2) / drdi(1) )
    b = drdi(1)/a
    ab = (/a,b/)
  endfunction ! a_b_of_radial_exp_grid

!   character(len=100) function glue_strings( s0, s1, s2, s3 )
!   implicit none
!     ! parameters
!     character(len=*), parameter           :: fun = ' glue_strings: '
!     ! arguments
!     character(len=*), intent(in)          :: s0
!     character(len=*), intent(in)          :: s1
!     character(len=*), intent(in), optional:: s2
!     character(len=*), intent(in), optional:: s3
!     ! local vars
!     integer                               :: ios, l
! 
!     l = len(s0) + len(s1)
!     write(unit=glue_strings,fmt='(9A)',iostat=ios) trim(s0), trim(s1)
!     if( present( s2 ) ) then
!       l = l + len(s2)
!       if( present( s3 ) ) then
!         l = l + len(s3)
!         write(unit=glue_strings,fmt='(9A)',iostat=ios) trim(s0), trim(s1), trim(s2), trim(s3)
!       else  ! present s3
!         write(unit=glue_strings,fmt='(9A)',iostat=ios) trim(s0), trim(s1), trim(s2)
! !         if(o>0) write(o,'(99A)') sym, fun, 's0 = "', trim(s0),'", s1 = "', trim(s1), '", s2 = "', trim(s2), '"'
!       endif ! present s3
!     else  ! present s2
!       if( present( s3 ) ) & ! this only happens calling glue_strings( "a", "b", s3="c" )
!         stop 'glue_strings: please do not use this function with an explicit interface!'
!     endif ! present s2
! 
! #ifdef DEBUG
!     if(o>0) then
!       if( l > len(glue_strings) ) write(o,'(3A,9(I0,A))') sym, fun, &
!         'received string of total length ', l, ' but return string has length ', len(glue_strings)
!       if( ios /= 0 ) write(o,'(3A,I0)') sym, fun, 'writing to string returned IOstatus=', ios
!     endif ! o/=0
! #endif
! !     if(o>0) write(o,'(9A)') sym, fun, 'return "', trim(glue_strings), '".'
!   endfunction glue_strings

  status_t function write_bmp_file_r( filename, data, style, phase ) result( ist )
    character(len=*), intent(in)          :: filename ! without ".bmp"
    real, intent(in)                      :: data(1:,1:) ! (nv,nh)
    character(len=*), intent(in), optional:: style
    real, intent(in), optional            :: phase(1:,1:) ! (nv,nh)
  ! writes a matrix or some other rank 2 array as a color encoded bitmap
  !
  ! character(len=*) filename name of the file to write without the file extension .bmp
  ! real data(:,:)            data to plot
  ! (optional)
  ! character(len=*) style    e.g. "inverse" makes highest values black, lowest white.

    character(len=*), parameter           :: fun = ' write_bmp_file: '
    integer, parameter                    :: M = 1
    integer, parameter                    :: HEADERLEN = 54   ! length of the header in bytes
    integer, parameter                    :: FRMTLEN = 16     !
    integer, parameter                    :: I_RED   = 3
    integer, parameter                    :: I_GREEN = 2
    integer, parameter                    :: I_BLUE  = 1
    real, parameter                       :: six_over_2Pi = 3./acos(-1.) ! 6./(2Pi)
    integer, parameter                    :: I_STYLE_DEFAULT = 0
    integer, parameter                    :: I_STYLE_INVERSE = 1
    integer, parameter                    :: I_STYLE_DENSITY = 2
    integer, parameter                    :: I_STYLE_AMINMAX = 3
    integer, parameter                    :: I_STYLE_REDBLUE = 4
    integer, parameter                    :: I_STYLE_COMPLEX = 8
    iounit_t, parameter                   :: u = 14

    character(len=len(filename)+4)        :: fname ! ".bmp" will be added
    character(len=HEADERLEN)              :: header
    string_t                              :: frmt
    integer                               :: istyle != I_STYLE_DEFAULT
    integer                               :: nh, nv, nall, nh4, nv4
    integer                               :: ih, iv
    integer                               :: ios, i256, ic
    character(len=3), allocatable         :: bgr(:,:) ! (nv,nh)
    character(len=3)                      :: white, color
    real                                  :: mnv, avg, mxv, den, dat
    real, parameter                       :: Sat = 1. ! saturation
    real                                  :: Hue60, Val, fHue, pVa, tVa, qVa
    integer                               :: iHue60, i256v(3)

    nh = size( data, 1 ) ! horizontal
    nv = size( data, 2 ) ! vertical
    nh4 = ((nh-1)/4+1)*4
    nv4 = ((nv-1)/4+1)*4

    white(1:1) = char(255) ; white(2:2) = char(255) ; white(3:3) = char(255)

    if( nh < 1 .or. nv < 1 ) return
    ! BMP (24bit depth)... this part works only when width is a multiple of 4.

    ! generate the filename with .bmp extension
    write( unit=fname, fmt='(9A)', iostat=ist ) trim(filename), '.bmp'

    open( unit=u, file=fname, action='write', status='unknown', iostat=ist )
#ifdef DEBUG
    if(o>0) write(o,'(A)') ! empty line
    if( ios /= 0 .and. o>0) write(o,'(5A,I0)') sym, fun, 'failed to open "', trim(fname) ,'" for writing, IOstatus=', ist
#endif
    if( ios /= 0 ) return

    allocate( bgr(m*nh4,m*nv4), stat=ist ) ; if( ist /= 0 ) return ! prepare data

    istyle = I_STYLE_DEFAULT
    if( present( phase ) ) istyle = I_STYLE_COMPLEX

    if( present( style ) ) then
      selectcase( style )
      case( 'density' ) ; istyle = I_STYLE_DENSITY
      case( 'invert'  ) ; istyle = I_STYLE_INVERSE
      case( 'minmax'  ) ; istyle = I_STYLE_AMINMAX
      case( 'redblue' ) ; istyle = I_STYLE_AMINMAX ! red positive, blue negative
      case( 'complex' ) ; istyle = I_STYLE_COMPLEX
      case default    ! ; istyle = I_STYLE_DEFAULT
      endselect ! style
    endif ! present style

    mnv = minval( data )             ! minimum
    avg = sum( data )/ real( nh*nv ) ! average
    mxv = maxval( data )             ! maximum

#ifdef DEBUG
    if(o>0) write(o,'(5A,9(ES10.2,A))') sym, fun, 'data for "', trim(fname) ,'"  min', mnv, '  avg', avg, '  max', mxv
#endif

    ! style dependent preparations
    selectcase( istyle )
    case( I_STYLE_DENSITY )
      den = 255./max( 1E-12, mxv ) ; mnv = 0.
    case( I_STYLE_AMINMAX, I_STYLE_REDBLUE )
      den = 255./( max( 1E-12, abs(mnv), abs(mxv) ) )**.25
    case( I_STYLE_COMPLEX )
      den = 255./sqrt( max( 1E-12, abs(mxv) ) )
    case( I_STYLE_INVERSE )
      den = -255./max( 1E-12, abs(mnv), abs(mxv) )
    case default
      den = 255./max( 1E-12, mxv-mnv )
    endselect ! istyle

    bgr = white ! init

    do iv = 1, nv
      do ih = 1, nh

        dat = data(ih,nv-iv+1) ! load

        selectcase( istyle )
        case( I_STYLE_INVERSE, I_STYLE_DENSITY )

          i256 = nint( den*(dat-mnv) )
          if( i256 < 256 ) i256 = i256 + 256
          color(1:1) = char( i256 )
          color(2:2) = color(1:1)
          color(3:3) = color(1:1)

        case( I_STYLE_AMINMAX )

          ic = 1 ; if( dat < 0. ) ic = -1 ! 1:red, -1:blue
          i256 = 255 - nint( abs(dat)**.25 * den )
          color(2-ic:2-ic) = char( i256 )
          color(2   :2   ) = char( i256 ) ! green
          color(2+ic:2+ic) = char(  255 ) ! 0xFF
        case( I_STYLE_COMPLEX )

          Val = sqrt( abs(dat) ) * den

          if( present( phase ) ) then

            ! 255full +---+---+---+---+---+---+
            !         |   |   |   |   |   |   |
            ! V-------RRRRR   |   |   |   RRRRR
            !         |   |R  |   |   |  R|   |
            !         |   | R |   |   | R |   |
            !         |   |  R|   |   |R  |   |
            ! V(1-S)--+   |   RRRRRRRRR   |   |
            !         |   |   |   |   |   |   |
            ! 0       +---+---+---+---+---+---+
            ! Hue     0   60 120 180 240 300 360 degree

            ! 255full +---+---+---+---+---+---+
            !         |   |   |   |   |   |   |
            ! V-------+   GGGGGGGGG   |   |   |
            !         |  G|   |   |G  |   |   |
            !         | G |   |   | G |   |   |
            !         |G  |   |   |  G|   |   |
            ! V(1-S)--G   |   |   |   GGGGGGGGG
            !         |   |   |   |   |   |   |
            ! 0       +---+---+---+---+---+---+
            ! Hue     0   60 120 180 240 300 360 degree

            ! 255full +---+---+---+---+---+---+
            !         |   |   |   |   |   |   |
            ! V-------+   |   |   BBBBBBBBB   |
            !         |   |   |  B|   |   |B  |
            !         |   |   | B |   |   | B |
            !         |   |   |B  |   |   |  B|
            ! V(1-S)--BBBBBBBBB   |   |   |   B
            !         |   |   |   |   |   |   |
            ! 0       +---+---+---+---+---+---+
            ! Hue     0   60 120 180 240 300 360 degree

            Hue60 = phase(ih,nv-iv+1) * six_over_2Pi
            iHue60 = floor( Hue60 )
            fHue = Hue60 - iHue60
            pVa = Val * ( 1. - Sat )
            qVa = Val * ( 1. - Sat * fHue )
            tVa = Val * ( 1. - Sat * ( 1. - fHue ) )

            selectcase( modulo( iHue60, 6 ) )
            !=================================
            case( 0 ) ; i256v = nint( (/ Val,tVa,pVa /) )
            case( 1 ) ; i256v = nint( (/ qVa,Val,pVa /) )
            case( 2 ) ; i256v = nint( (/ pVa,Val,tVa /) )
            case( 3 ) ; i256v = nint( (/ pVa,qVa,Val /) )
            case( 4 ) ; i256v = nint( (/ tVa,pVa,Val /) )
            case( 5 ) ; i256v = nint( (/ Val,pVa,qVa /) )
            !=================================
            endselect ! iHue60 % 6

            i256v = min(max( 0, i256v ), 255 )
          else  ! phase
            i256v = min(max( 0, nint( Val ) ), 255 ) ! only grey
          endif ! phase

          color(1:1) = char( i256v(3) )
          color(2:2) = char( i256v(2) )
          color(3:3) = char( i256v(1) )

        case default

          i256 = nint( dat*den )
          color(1:1) = char( i256 )
          color(2:2) = color(1:1)
          color(3:3) = color(1:1)

        endselect ! istyle
#ifdef DEBUG
!         if( i256 > 255 .or. i256 < 0 ) stop 'write_bmp_file: ERROR: i256 out of bounds [0,255]!'
        i256 = min(max( 0, i256 ), 255 )
#endif
        if( M == 1 ) then
          bgr(ih,iv) = color
        else
          bgr((ih-1)*m+1:ih*m,(iv-1)*m+1:iv*m) = color
        endif

      enddo ! ih
    enddo ! iv

#ifdef DEBUG
    if(o>0) write(o,'(9A)') sym, fun, 'now writing BMP(24bit) file "', trim(fname), '".'
#endif
    ! create the header
    ! header 1 (file header ; byte 1--14 )
    header( 1: 2) = 'BM'                ! signature: declaring this is a BMP file
    header( 3: 6) = num2bit4( 54 + 3*m*m*nh4*nv4 ) ! file size: header + RGB data
    header( 7: 8) = char(0)             ! reserved 1: may be 0
    header( 9:10) = char(0)             ! reserved 2: may be 0
    header(11:14) = num2bit4( 54 )      ! file offset to pixel array: must be 54 : total length of header
    ! header 2 (bit-map header ; byte 15--54 )
    header(15:18) = num2bit4( 40 )      ! must be 40 : length of bit-map header
    header(19:22) = num2bit4( m*nh4 )   ! width
    header(23:26) = num2bit4( m*nv4 )   ! height
    header(27:28) = num2bit2( 1 )       ! number of planes: usually 1
    header(29:30) = num2bit2( 24 )      ! color depth, bits per pixel: usually 24

    header(31:34) = num2bit4( 0 )       ! may be 0 : compression method index
    header(35:38) = num2bit4( 0 )       ! may be 0 : file size if compressed
    header(39:42) = num2bit4( 0 )       ! arbit. : pixel per meter, horizontal
    header(43:46) = num2bit4( 0 )       ! arbit. : pixel per meter, vertical
    header(47:50) = num2bit4( 0 )       ! may be 0 here : num. of color used
    header(51:54) = num2bit4( 0 )       ! may be 0 here : num. of important color

    ! generate format string for image data
!     nall = 3 * m * nh4 * m * nv4
    nall = m * nh4 * m * nv4
    if( nall >= 10**9 ) stop 'write_bmp_file: number of elements too large'
!     write( unit=frmt, fmt='(A,I0,A)' ) '(', nall, 'A1)'
    write( unit=frmt, fmt='(9(A,I0))' ) '(', nall, 'A3)'
    ! writing header part
    write( unit=u, fmt='(A54)', advance='no', iostat=ist ) header
    ! writing image data
    write( unit=u, fmt=frmt, advance='no', iostat=ist ) bgr ! writing in BGR order, not RGB.
    close( unit=u, iostat=ist )

#ifdef DEBUG
    if(o>0) write(o,'(9A)') sym, fun, 'file "', trim(fname), '" written.'
#endif
    deallocate( bgr, stat=ist )

  contains

    character(len=4) function num2bit4( i ) result( c )
      integer, intent(in) :: i
      integer :: j, k
      j = i              ; k = j / 256**3 ; c(4:4) = char(k)
      j = j - k * 256**3 ; k = j / 256**2 ; c(3:3) = char(k)
      j = j - k * 256**2 ; k = j / 256**1 ; c(2:2) = char(k)
      j = j - k * 256**1 ; k = j / 256**0 ; c(1:1) = char(k)
    endfunction ! num2bit4

    character(len=2) function num2bit2( i ) result( c )
      integer, intent(in) :: i
      integer :: j, k
      j = i              ; k = j / 256**1 ; c(2:2) = char(k)
      j = j - k * 256**1 ; k = j / 256**0 ; c(1:1) = char(k)
    endfunction ! num2bit2

  endfunction ! write_bmp_file

  ! complex interface
  status_t function write_bmp_file_c( filename, data, style ) result( ist )
    character(len=*), intent(in)           :: filename ! without ".bmp"-FileNameExtension
    complex, intent(in)                    :: data(1:,1:) ! (nv,nh)
    character(len=*), intent(in), optional :: style

    ist = write_bmp_file_r( filename, abs(data), style, phase=atan2(aimag(data),real(data)) )
  endfunction ! write_bmp_file


  status_t function visualize_density( file, rho, comm, direction, nscale ) result( ist )
  !=============================================================================
  ! Bitmap visualization, reduction in z-direction (or other)
  !=============================================================================
  use MPItools, only: MPInprocs
    character(len=*), intent(in)          :: file
    real, intent(in)                      :: rho(:,:,:,:)
    MPI_Comm, intent(in)                  :: comm
    character, intent(in), optional       :: direction
    integer, intent(in), optional         :: nscale

    character(len=*), parameter           :: fun = ' visualize_density: '
    character                             :: dir = 'z' ! default = z-direction
    integer                               :: ng(1:4), i4, id, n1, n2, ii, ns, i1, i2
    real, allocatable                     :: d2(:,:), ds2(:,:) ! 2-dim density array and enlarged 2-dim density array
#ifdef DEBUG
    character(len=16)                     :: nxn ! ", 8 x 8 pix"
    nxn = ', 1 x 1 pix'
#endif

    if( MPInprocs( comm ) > 1 ) return ! NOT PARALLEL

    ng = shape(rho) ; if( any( ng < 1 ) ) return

    dir = 'z' ; if( present( direction ) ) dir = direction

    selectcase( dir )
    case( 'x', 'X', '1' ) ; id = 1 ; n1 = ng(2) ; n2 = ng(3)
    case( 'y', 'Y', '2' ) ; id = 2 ; n1 = ng(1) ; n2 = ng(3)
    case( 'z', 'Z', '3' ) ; id = 3 ; n1 = ng(1) ; n2 = ng(2)
    case default ; ii = 1 ; id = 3 ; n1 = ng(1) ; n2 = ng(2) ! default = z-direction
#ifdef DEBUG
      if(o>0) write(o,'(9A)') sym, fun, 'bad input direction ="', dir, '", default is z-direction.'
#endif
    endselect ! dir
    allocate( d2(n1,n2), stat=ist )

    ! reduce along the one cartesian direction id
    d2 = 0.
    do i4 = 1, ng(4)
      selectcase( id )
      case( 1 ) ; do ii = 1, ng(id) ; d2 = d2 + rho(ii,:,:,i4) ; enddo ! ii
      case( 2 ) ; do ii = 1, ng(id) ; d2 = d2 + rho(:,ii,:,i4) ; enddo ! ii
      case( 3 ) ; do ii = 1, ng(id) ; d2 = d2 + rho(:,:,ii,i4) ; enddo ! ii
      case default ; stop 'visualize density: fatal error with ID!'
      endselect ! id
    enddo ! i4

    where( d2 < 0. ) ; d2 = 0. ; endwhere ! d2 < 0.
    ! apply sqrt operation to it
    d2 = sqrt( d2 )

    ns = 1 ; if( present( nscale ) ) ns = max( 1, abs(nscale) )
    if( ns < 2 ) then
      ! no enlarging requested
      ist = write_bmp_file( file, d2, style='density' )

    else  ! ns < 2
#ifdef DEBUG
!       write(unit=nxn,fmt='(3(A,I2))') ',', ns, ' x', ns, ' pix'
#endif
      ! enlarge the array:
      allocate( ds2(n1*ns,n2*ns), stat=ist )
      do i2 = 1, n2
        do i1 = 1, n1
          ds2( 1+(i1-1)*ns:i1*ns, 1+(i2-1)*ns:i2*ns ) = d2(i1,i2)
        enddo ! i1
      enddo ! i2
      ist = write_bmp_file( file, ds2, style='density' )
      deallocate( ds2, stat=ist )

    endif ! ns < 2
    deallocate( d2, stat=ist )
#ifdef DEBUG
!     if(o>0) write(o,'(9A)') sym, fun, '"', trim(file), '.bmp" written', trim(nxn), '.'
#endif
  endfunction ! visualize_density


  status_t function build_dep_tree( ) result( ios ) ! code maintainance tool
    character(len=*), parameter :: fun = ' build_dep_tree: '
    character(len=*), parameter :: FILENAME = 'use_dependency'
    character(len=*), parameter :: PRPDIR = 'prp' ! directory containing preprocesse sources
    character(len=*), parameter :: SRCDIR = 'src' ! directory containing simple sources
    integer, parameter   :: NMOD = 99 ! max number of modules, hard limit
    iounit_t, parameter  :: o = 6 ! stdout
    iounit_t, parameter  :: u = 13 ! file unit

    character(len=128)  :: line
!   integer             :: line_without_use = 0
    integer             :: line_cnt = 0                ! line number counter
    integer             :: i, imod, jmod = 0              ! module indices
    integer             :: nknmod = 0 ! number of known module names

    character(len=24)   :: fname, uname  ! file name, use name
    character(len=24)   :: mod_name(NMOD) = ''       ! names
    integer             :: mod_link(NMOD,NMOD) = 0 ! dependency matrix
    character(len=12)   :: ana
    integer             :: jlevel, mod_level(NMOD) = 0, ilevel
    integer             :: mod_ref(NMOD) = 0
!   logical             :: written(NMOD)

!     if(o>0) write(o,'(9A)') sym, fun, 'searching for "', FILENAME, '".'
    open( unit=u, file=FILENAME, status='old', iostat=ios )
    if( ios /= 0 ) then
      if(o>0) then ! explain how it works
        write(o,'(9A)') sym, fun, 'cannot find file "', FILENAME, '".'
        write(o,'(9A)') sym, fun, 'please run the following command ...'
        write(o,'(9A)') sym, fun, ' ... in the parent folder of the source'
        write(o,'(9A)') sym, fun, ' ... directory before running again!'
        write(o,'(9A)') 'grep " use " ', PRPDIR,'/*.f90 ', SRCDIR, '/*.f90 > ./',FILENAME
      endif ! o/=0
      return
    endif ! ios /= 0
    if(o>0) write(o,'(9A)') sym, fun, 'file "', FILENAME, '" found, start processing.'
    ! start of the routine

    line_cnt = 0 ! init
    do while( read_line( u, line ) == 0 )
      line_cnt = line_cnt + 1 ! count number of lines
!       write(o,'(A)') trim(line) ! this line just reproduces the file

      ana = analyze( line, fname, uname )
      selectcase( ana )
      case( 'success' )
!         write(o,'(9A)') trim(fname), ' -> ', trim(uname) ! see if the names are known

        ! check the file name
        imod = imodule( fname )
        if( imod == 0 ) then ! unknown
          nknmod = nknmod + 1
          imod = nknmod
          mod_name(nknmod) = fname
        endif ! imod == 0
        ! check the module name
        jmod = imodule( uname )
        if( jmod == 0 ) then ! unknown
          nknmod = nknmod + 1
          jmod = nknmod
          mod_name(nknmod) = uname
        endif ! jmod == 0

        ! set a new link or increase the link strength
        mod_link(jmod,imod) = mod_link(jmod,imod) + 1

      case( 'commented' ) ! nothing
      case default ! show error messages of function analyze
!         write(o,'(3A,I5,9A)') sym, fun, 'in line#', line_cnt, ': ', ana &
!                                         , ' line="', trim(line), '".'
      endselect ! ana

    enddo ! while lios == 0
    ! close the file
    close( unit=u, iostat=ios )

!     ! notify erroneous lines
!     if( line_without_use > 0 ) write(o,'(3A,I5,9A)') sym, fun, &
!       'in', line_without_use, ' lines, no "use" has been found.'

!     ! show the list of names
!     do imod = 1, nknmod
!       write(o,'(I3,A,A)') imod, ' -> ', trim(mod_name(imod))
!     enddo ! imod

    ! eval the level ! simple but works
    do i = 0, nknmod ! iteratively
      do imod = 1, nknmod
        do jmod = 1, nknmod
          if( mod_link(jmod,imod) > 0 ) &
            mod_level(imod) = max( mod_level(imod), mod_level(jmod)+1 )
        enddo ! jmod
      enddo ! imod
    enddo ! i ! iterations

!     ! show the list of links
!     do imod = 1, nknmod
!       write(o,'(I3,A,51I3)') imod, ' -> ', mod_link(1:nknmod,imod)
!     enddo ! imod

    ! check if the transpose matrix entry
    ! a(j,i) == zero, if a(i,j) is nonzero
    do imod = 1, nknmod
      do jmod = 1, imod
        if( mod_link(imod,jmod) /= 0 .and. mod_link(jmod,imod) /= 0 ) then
          write(o,'(9A)') mod_name(imod), ' <-> ', mod_name(jmod), ' detected double link'
          stop 'dependencies: double link, 2 modules include each other!'
        endif ! ... error
      enddo ! jmod
    enddo ! imod

    ! count references
    ! check for unused modules ( no referneces to it)
    mod_ref = 0
    do imod = 1, nknmod
      mod_ref(imod) = count( mod_link(imod,:) > 0 )
      if( mod_ref(imod) == 0 ) write(o,'(9A)') mod_name(imod), ' never referred!'
    enddo ! imod

    ! write a dependency file
    write(o,'(A)') '-------------------', &
                   '"use"-DEPENDENCIES'
    do ilevel = 0, maxval(mod_level)
      write(o,'(//,A/A,I3)') '-------------------', 'level', ilevel
      do imod = 1, nknmod
        if( mod_level(imod) == ilevel ) then
          write(o,'(/3A,I3)') trim(mod_name(imod))!, ':', '   level', mod_level(imod)
          write(o,'(2A,I3)')  '!!>', '-------------------------'
          write(o,'(2A,I3)')  '!! ', ' level', mod_level(imod)
          write(o,'(2A,I3)')  '!! ', ' references ', mod_ref(imod)
          write(o,'(2A,I3)')  '!!<', '-------------------------'
          do jlevel = 0, ilevel-1
          do jmod = 1, nknmod
            if( mod_link(jmod,imod) > 0 .and. mod_level(jmod)==jlevel ) &
              write(o,'(9A)') '!! ', '@see ', trim(mod_name(jmod))
              ! write(o,'(A2,A,A5,I5)') '  ', trim(mod_name(jmod)) !, '', mod_link(jmod,imod) 
              ! write(o,'(3A,I3,9A)') '  ', trim(mod_name(jmod)), '  (level', mod_level(jmod), ' )' 
          enddo ! jmod  
          enddo ! jlevel
        endif ! mod_level == ilevel
      enddo ! imod
    enddo ! ilevel

    ! write a short dependency file (only level structure)
    write(o,'(A)') '', '', '', '-------------------', 'LEVELSTRUCTURE'
    do ilevel = 0, maxval(mod_level)
      write(o,'(/A/A,I3,/A)') '----------', ' level', ilevel, '----------'
      do imod = 1, nknmod
        if( mod_level(imod) == ilevel ) then
          write(o,'(A)') trim(mod_name(imod))
        endif ! mod_level == ilevel
      enddo ! imod
    enddo ! ilevel
    write(o,'(A)') ''

    ! write a list for the Makefile
    write(o,'(////)')
    do ilevel = 0, 7!maxval(mod_level)
      write(o,'(A)') ''
      write(o,'(/A,I1,A)',advance='no') 'L',ilevel,'MODULES ='
      do imod = 1, nknmod
        if( mod_level(imod) == ilevel ) then
          write(o,'(3A)',advance='yes') ' \ '
          write(o,'(3A)',advance='no') 'mod_',trim(mod_name(imod)),'.o'
        endif ! mod_level == ilevel
      enddo ! imod
    enddo ! ilevel
    write(o,'(A)') ''

  contains

    character(len=12) function analyze( line, w1, w2 )
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !! analyze a given line for the structure
    !! *_<w1>.*:  [!]  use <w2>[, only:] *
    !! for processing the output of grepping " use "
    !! in all source files. the function avoids 
    !! certain names, which are no modules and 
    !! rekognizes !-commented use-statements.
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      character(len=*), intent(in)      :: line
      character(len=*), intent(out)    :: w1, w2

      character(len=64) :: mline  ! part of the line
      integer :: isco, idot, icol, icmt, iuse, ic_b, ionly ! indices

      analyze = 'unknown' ; w1 = '' ; w2 = ''

      icol = index( line, ':' ) ! first colon (:)
      if( icol < 3 ) then ; analyze = 'no colon' ; return ; endif
      ! the line has a colon (:)

      isco = index( line(1:icol), '_' ) ! first underscore (_) until colon
      if( isco < 1 ) then ; analyze = 'no uscore' ; return ; endif

      idot = index( line(isco:icol), '.' ) ! first dot (.) between underscore and colon
      if( idot < 1 ) then ; analyze = 'no dot' ; return ; endif

      w1 = line(isco+1:isco+idot-2)
!         write(o,'(A)') trim(w1)

      iuse = index( line(icol:), 'use ' ) ! use statement (use ) after colon
      if( iuse < 1 ) then ; analyze = 'no use' ; return ; endif

!       write(o,'(9A)') sym, fun, trim( line(icol+iuse+3:) )
!            , '     line="', trim(line), '".'
      icmt = index( line(icol:icol+iuse), '!' ) ! comment sign between colon and use
      if( icmt > 0 ) then ; analyze = 'commented' ; return ; endif
      ! statement is not commented out

!       write(o,'(9A)') sym, fun, 'commented line', ', line="', trim(line), '".'

      mline = line(icol+iuse+3:)
      ic_b = scan( mline, ', ' ) ! first comma or first blank
!       write(o,'(5A,I4)') sym, fun, 'end position in ', trim(mline), ' is', ic_b
      w2 = mline(1:ic_b-1)
      ! look for words which are certainly no modules
      selectcase( w2 )
      case( '', 'at', '=', '1', '"', '''', 'this', 'either', 'CG' )
        analyze = 'forbidden w!' ; return
      case( 'symbol', 'wave', 'orthogonal' )
        analyze = 'physical w!'  ; return
      case( 'default', 'default.', 'default.''', 'default:', 'default''' )
        analyze = '"default" w!' ; return
      case default
!         write(o,'(9A)') sym, fun, 'found ', trim(w1), ' -> ', trim(w2)
!            , '  line="', trim( line ),'".'
      endselect ! w2
      analyze = 'success'

      ! find whats after ", only:"
      ionly = index( mline(ic_b:), 'only:' ) ! use statement (use ) after colon
!       write(7,'(9A)') ' ', trim(w1), ' ==> ', trim(w2), '      ', trim(mline(ic_b+ionly+4:))

    endfunction ! analyze

    !! finds the index, if the module name is listed in mod_name(1:nknmod)
    integer function imodule( name )
      character(len=*), intent(in) :: name
      integer :: im
      imodule = 0 ! return 0 if not found
      im = nknmod ! init
      do while( im > 0 )
        if( name == mod_name(im) ) then
          imodule = im ; return
        else ; im = im-1
        endif ! name matches
      enddo ! while im > 0
    endfunction ! imodule

    !! returns 0, if a line could be read from unit u
    integer function read_line( u, l ) result( ios )
      integer, intent(in) :: u ! unit to read from
      character(len=*), intent(out) :: l
      read( unit=u, fmt='(A)', IOstat=ios ) l
    endfunction ! read_line

  endfunction ! build_dep_tree


  !! this procedure writes an e-mail, if
  !!        -- the compiler understands call system( "some command line" )
  !! and    -- the tool "mail" is installed and accessible (searchpath...)
  !!
  !! in arguments
  !!        -- address of recipient
  !!        -- name of the project, appears in the subject
  !!    optional
  !!        -- message --> the e-mail body will contain a timestamp,
  !!                       the project name and the message string.
  !!        -- textfile --> the e-mail body will be the textfile
  !!                       (textfile overrides message)
  !!
  !! result string m (short status message)
  !!              "ok"     --> mail sent
  !!              "ios"    --> command line has been truncated
  !!                           or opening of tmpfile has failed
  !!              "warn"   --> Warning: textfile overrides message
  !!              "nofile" --> textfile does not exist
  !!
  status_t function mail_notify( address, project, message, textfile ) result( ios )
  use configuration, only: CodeName ! Name of the code
    character(len=*), intent(in) :: address  ! e-mail recipient
    character(len=*), intent(in) :: project  ! name of the calculation project
    character(len=*), intent(in), optional :: message  ! the text of the email, if present.
    character(len=*), intent(in), optional :: textfile ! name of file which is the text, if present

    character(len=*), parameter     :: fun = ' mail_notify: '
    character(len=*), parameter     :: EXE = 'mail ' ! executable program
    character(len=*), parameter     :: TMPFILE = 'tmp' ! temporary text file
    character(len=*), parameter     :: ERRFILE = 'err' ! temporary text file
    character(len=*), parameter     :: SUBJECT = ' -s ' ! option to specify a subject
    character(len=*), parameter     :: TIMESTAMPFORMAT = '(I4.4,2("|",I2.2),"_",I2.2,2(":",I2.2))'
    iounit_t, parameter             :: u = 14 ! unit

    character(len=6)                      :: m = '' ! error message
    character(len=256)                    :: command ! command line
    character(len=20)                     :: timestamp ! date and time readable
    character(len=17)                     :: errors = '' ! reaction of the system
    integer                               :: t(8)    ! date&time
    logical                               :: existent

    if( address == '' ) then ; m = 'address!' ; return ; endif

    if( present( textfile ) ) then ! file given

      inquire( file=textfile, exist=existent, IOstat=ios )
      if( .not. existent ) then ! Warn
        write(*,'(9A)') sym, fun, 'Warning: textfile ="', trim(textfile), '" does not exist!'
        m = 'nofile'
        return ! error
      endif ! not existent

      ! prepare the command line
      write(unit=command,fmt='(99A)',IOstat=ios) &
        EXE, trim(address), SUBJECT, '"[',CodeName,'] ',trim(project),'" < ', trim(textfile), ' > ', ERRFILE

      call system( command ) ! Send the mail now !

      if( present( message ) ) then ! Warn, if the other optional argument gets lost
        write(*,'(9A)') sym, fun, 'Warning: optional argument textfile overrides message, message ="', trim(message), '".'
        m = 'warn'
      else  ! present message
        m = 'ok' ! success
      endif ! present message

    else  ! present textfile ! no file given

      call date_and_time( values=t )
      write(unit=timestamp,fmt=TIMESTAMPFORMAT) t(1:3), t(5:7) ! generate the time stamp

      ! prepare the body text
      open( unit=u, file=TMPFILE, status='unknown', IOstat=ios )
      ! ====================================================
      write(u,'(9A)') timestamp ! write time stamp

      ! show project name
      if( project /= '' ) write(u,'(9A)') 'Project name = "', trim(project), '"'

      ! show message
      if( present( message ) ) write(u,'(9A)') message

      ! prepare the command line
      write( unit=command, fmt='(99A)', iostat=ios ) &
        EXE, trim(address), SUBJECT, '"[',CodeName,'] ',trim(project),'" < ', TMPFILE, ' > ', ERRFILE

      call system( command ) ! Send the mail now !
      ! ====================================================
      close( unit=u, status='delete', IOstat=ios )
      m = 'ok' ! success

    endif ! present textfile

    open( unit=u, file=ERRFILE, status='unknown', IOstat=ios ) ! open the file "tmp"
    read( unit=u, fmt='(A)', IOstat=ios ) errors
    close( unit=u, IOstat=ios )
    if( errors /= '' ) then
      write(*,'(9A)') sym, fun, 'Warning: call system( "', exe, ' ..." ) returned "', trim(errors), '".'
      m = adjustl(errors)
    endif ! errors occured

    if( ios /= 0 ) m = 'ios'
    write(*,'(9A)') sym, fun, 'message ="', trim(m), '".'

  endfunction ! mail_notify

  status_t function write_vtk_file( filename, a ) result( ios )
    character(len=*), intent(in) :: filename
    real, intent(in)             :: a(:,:,:)

    character(len=*), parameter  :: fun = ' write_vtk_file: '
    character(len=3), parameter  :: XYZ = 'XYZ'
    iounit_t, parameter          :: u = 13
    string_t                     :: fname
    integer                      :: n(3), i, ir

    do i = 1 ,3
      n(i) = size( a, i )
    enddo
    if( any( n > 999 ) ) stop 'write_vtk_file: array dimension too large'

    write( unit=fname, fmt='(9A)', iostat=ios ) trim(filename), '.vtk'
    open( unit=u, file=fname, status='unknown', iostat=ios )
    if( ios /= 0 ) then
      if(o>0) write(o,'(5A,I0,9A)') sym, fun, 'file "', trim(fname), '" cannot be opened, ios = ', ios, ' return!'
      return
    endif ! ios /= 0
    write(u,'(9A)') '# vtk DataFile Version 3.0'
    write(u,'(9A)') 'vtk output'
    write(u,'(9A)') 'ASCII'
    write(u,'(9A)') 'DATASET RECTILINEAR_GRID'
    write(u,'(1A,3I4)') 'DIMENSIONS ', n(1:3)
    do i = 1, 3
      write(u,'(2A,I4,9A)' ) XYZ(i:i),'_COORDINATES ', n(i), ' float'
      write(u,'(999F10.3)') ( 0.5*(2*ir-n(i)-1) , ir=1,n(i) )
    enddo ! i
    write(u,'(A,I12)') 'CELL_DATA ', product(n-1)
    write(u,'(A,I12)') 'POINT_DATA ', product(n)
    write(u,'(9A)') 'SCALARS array float'
    write(u,'(9A)') 'LOOKUP_TABLE default'
    write(u,'(100ES16.6E3)') a
    write(u,'(9A)') ''
    close( unit=u, iostat=ios )

  endfunction ! write_vtk_file


  character elemental function to_lowercase( u ) result( l )
    character, intent(in) :: u

    integer, parameter :: A=ichar('A'), Z=ichar('Z'), aA=ichar('a')-ichar('A')
    integer :: i
    i = ichar( u )
    if( i >= A .and. i <= Z ) then
      l = achar( i + aA )
    else  ! captial letter
      l = u
    endif ! captial letter
  endfunction ! to_lowercase

!- extended
#endif

  !! converts strings to all-lower-case
  integer function convert2lowercase( u, l ) result( nc ) ! number of chars converted
    character(len=*), intent(in)  :: u !! string containing possible upper case characters
    character(len=*), intent(out) :: l !! string with only lower case characters

    integer, parameter :: A=ichar('A'), Z=ichar('Z'), aA=ichar('a')-ichar('A')
    integer :: ip, ic, lu

    nc = 0 ! init
    l = u ! string copy
    lu = len_trim(u)
    if( len(l) < lu ) stop 'convert2lowercase: string is cropped!'
    do ip = 1, lu
      ic = ichar( u(ip:ip) )
      if( ic >= A .and. ic <= Z ) then
        l(ip:ip) = achar( ic + aA )
        nc = nc+1 ! count up
      endif ! capital letter
    enddo ! ip
  endfunction ! convert2lowercase
! 
!   character(len=16) function cat_i( i, fmt ) result( str )
!     integer, intent(in) :: i
!     character(len=*), intent(in), optional :: fmt
! 
!     character(len=16) :: f = '(I0)'
!     status_t :: ios
! 
!     f = '(I0)' ; if( present(fmt) ) f = fmt
!     write(unit=str,fmt=f,iostat=ios) i
!   endfunction ! cat
! 
!   character(len=80) function cat_s_i( s, i, fmt ) result( str )
!     character(len=*), intent(in) :: s
!     integer, intent(in)          :: i
!     character(len=*), intent(in), optional:: fmt
! 
!     character(len=16) :: f = '(A,I0)'
!     status_t :: ios
! #ifdef DEBUG
!     integer                               :: l
!     l = len_trim(s) + log(1.*abs(i)) + 1
!     if(o>0) then
!       if( l > len(str) ) write(o,'(3A,9(I0,A))') sym, ' cat: ', 'received string of total length ', l, ' but return string has length ', len(str)
!       if( ios /= 0 ) write(o,'(3A,I0)') sym, ' cat: ', 'writing to string returned IOstatus=', ios
!     endif ! o>0
! #endif
!     if( present(fmt) ) f = fmt
!     write(unit=str,fmt=f,iostat=ios) trim(s), i
!   endfunction ! cat
! 
! 




  character(len=128) function cat_2s( s1, s2 ) result( str )
    character(len=*), intent(in) :: s1, s2
    status_t :: ios
#ifdef DEBUG
    integer :: l
    character(len=*), parameter :: fun = "cat_2s"
    l = len_trim(s1) + len_trim(s2)
    if(o>0) then
      if( l > len(str) ) write(0,'(3A,I4,A,I4)') sym, ' cat: ', &
        'received string of total length', l, ' but return string has length', len(str)
      if( ios /= 0 ) write(0,'(3A,I3)') sym, fun, 'writing to string returned IOstatus=', ios
    endif ! o/=0
#endif
    write(unit=str,fmt='(9A)',iostat=ios) trim(s1),trim(s2)
  endfunction ! cat

  ! module procedure cat_si, cat_is, cat_2s
  character(len=128) function cat_is( i1, s2 ) result( str )
    integer, intent(in)          :: i1
    character(len=*), intent(in) :: s2
    status_t :: ios
    write(unit=str,fmt='(I0,A)',iostat=ios) i1, trim(s2)
  endfunction ! cat

  character(len=128) function cat_si( s1, i2 ) result( str )
    character(len=*), intent(in) :: s1
    integer, intent(in)          :: i2
    status_t :: ios
    write(unit=str,fmt='(A,I0)',iostat=ios) trim(s1), i2
  endfunction ! cat

#ifdef EXTENDED
  status_t function test( ) result( ios )
    write(*,*,iostat=ios) __FILE__,' no module test implemented!'
  endfunction ! test
#endif

endmodule ! toolbox

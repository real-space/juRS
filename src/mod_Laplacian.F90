#include "config.h"

#ifdef DEBUG_ALL
#undef DEBUG
#endif

! #define DEBUG

#ifdef DEBUG
#define cDBG  
#else
#define cDBG ! 
#endif



!! @author Paul Baumeister
!! @version 3.0
!! @see configuration
!! @see constants
!! @see boundary
!! @see debugtools
!! @see type_grid
!! @see toolbox
!! computes the Laplacian (optimized for Poisson equation)
module Laplacian
  use configuration, only: o ! output unit, 0: no output
implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: sym = 'LPL' !! module symbol

  public :: Laplace
  public :: Laplace_smoother
  public :: prepare
#ifdef EXTENDED
  public :: test
#endif

  public :: Laplace_coefficients

  interface Laplace
    module procedure Laplace_fd_r !, Laplace_ms_r, Laplace_nm_r
  endinterface Laplace

  interface Laplace_smoother
    module procedure Laplace_fd_r_smoother
  endinterface

  interface prepare
    module procedure Laplace_fd_prepare_r !, Laplace_ms_prepare_r, Laplace_nm_prepare_r
  endinterface prepare

  logical, save :: init_Laplacian = .false.

  ! the finite-difference stencil for the 2nd derivative
  real, allocatable, save    :: vec(:,:,:)    ! halo enlarged vector
  real,              save    :: t1            ! temp
  real, allocatable, save    :: t2(:)         ! temp
  real, allocatable, save    :: t3(:,:)       ! temp
  real, allocatable, save    :: c(:,:,:)

#if 0
  interface Laplace_fd
    module procedure Laplace_fd_r
  endinterface Laplace_fd

  interface Laplace_fd_prepare
    module procedure Laplace_fd_prepare_r
  endinterface Laplace_fd_prepare

  ! Laplacian with Mehrstellen discretization
  interface Laplace_ms
    module procedure Laplace_ms_r
  endinterface Laplace_ms

  interface Laplace_ms_prepare
    module procedure Laplace_ms_prepare_r
  endinterface Laplace_ms_prepare

  ! Laplacian with Numerov discretization
  interface Laplace_nm
    module procedure Laplace_nm_r
  endinterface Laplace_nm

  interface Laplace_nm_prepare
    module procedure Laplace_nm_prepare_r
  endinterface Laplace_nm_prepare

  interface precon
    module procedure precon_r
  endinterface precon
#endif

  contains

  status_t function Laplace_fd_r( v, Lv, g) result( ist )
  use constants, only: Pi
  use type_grid, only: grid
  use boundary, only: dataexchange
    real, intent(in)            :: v(:,:,:)
    real, intent(out)           :: Lv(:,:,:)
    type(grid), intent(in)      :: g
    

    character(len=*), parameter :: fun = ' Laplace_fd: '
    integer                     :: ix, iy, iz, ii
    real                        :: tx, ty, tz, v0
    real                        :: cL(0:15,3)
    real, allocatable           :: vc(:,:,:)

! !$omp single
    allocate( vc(1-g%nh(1):g%ng(1)+g%nh(1),1-g%nh(2):g%ng(2)+g%nh(2),1-g%nh(3):g%ng(3)+g%nh(3)), stat=ist )
! !$omp end single

    if( ist /= 0 ) then
      if(o>0) write(o,'(3A,F0.3,9A)') sym, fun, 'failed to allocate ', product(g%ng(1:3)+2*g%nh) * .5**7,' kiByte'
      stop 'LPL: failed to allocate halo-enlarged array VC'
      return
    endif ! ist /= 0

! !$omp single
    do ii = 1, 3 ! prepare the finite-difference coefficients for each direction
      cL(0:g%Nf(ii),ii) = Laplace_coefficients( g%Nf(ii) ) / ( -4. * Pi * g%h(ii)**2 )
    enddo ! ii

    vc = 0.0 ! init
    vc(1:g%ng(1),1:g%ng(2),1:g%ng(3)) = v ! copy v into the central region of vc
    call dataexchange( g, vc ) ! fill the halos using MPI communication
    ist = 0



!$omp parallel do collapse(3) private(ix,iy,iz,ii,tx,ty,tz,v0) schedule(static)
    do iz = 1, g%Ng(3)
      do iy = 1, g%Ng(2)
        do ix = 1, g%Ng(1)

          v0 = vc(ix,iy,iz) ! abbreviation

          ! second derivative in x-direction
          tx = v0 * cL(0,1)
          do ii = 1, g%Nf(1)
            tx = tx + ( vc(ix+ii,iy,iz) + vc(ix-ii,iy,iz) ) * cL(ii,1)
          enddo ! ii

          ! second derivative in y-direction
          ty = v0 * cL(0,2)
          do ii = 1, g%Nf(2)
            ty = ty + ( vc(ix,iy+ii,iz) + vc(ix,iy-ii,iz) ) * cL(ii,2)
          enddo ! ii

          ! second derivative in z-direction
          tz = v0 * cL(0,3)
          do ii = 1, g%Nf(3)
            tz = tz + ( vc(ix,iy,iz+ii) + vc(ix,iy,iz-ii) ) * cL(ii,3)
          enddo ! ii

          Lv(ix,iy,iz) = tx + ty + tz 

        enddo ! ix
      enddo ! iy
    enddo ! iz
!$omp end parallel do



  endfunction ! Laplace_fd

  
  status_t function Laplace_fd_r_smoother( v, Lv, g, shift ) result( ist )
  use constants, only: Pi
  use type_grid, only: grid
  use boundary, only: dataexchange
    real, intent(in)            :: v(:,:,:)
    real, intent(out)           :: Lv(:,:,:)
    type(grid), intent(in)      :: g
    real, intent(in)            :: shift

    character(len=*), parameter :: fun = ' Laplace_fd: '
    integer                     :: ix, iy, iz, ii
    real                        :: tx, ty, tz, v0
    real                        :: cL(0:15,3)
    real, allocatable           :: vc(:,:,:)
    real                        :: coeff(2), w
    type(grid) :: gg

    gg = g
    gg%nf = 2
    gg%nh = 2

! !$omp single
    allocate( vc(1-gg%nh(1):gg%ng(1)+gg%nh(1),1-gg%nh(2):gg%ng(2)+gg%nh(2),1-gg%nh(3):gg%ng(3)+gg%nh(3)), stat=ist )
! !$omp end single

    if( ist /= 0 ) then
      if(o>0) write(o,'(3A,F0.3,9A)') sym, fun, 'failed to allocate ', product(g%ng(1:3)+2*g%nh) * .5**7,' kiByte'
      stop 'LPL: failed to allocate halo-enlarged array VC'
      return
    endif ! ist /= 0

! !$omp single
    do ii = 1, 3 ! prepare the finite-difference coefficients for each direction
      cL(0:g%Nf(ii),ii) = Laplace_coefficients( g%Nf(ii) ) / ( -4. * Pi * g%h(ii)**2 )
    enddo ! ii

    vc = 0.0 ! init
    vc(1:gg%ng(1),1:gg%ng(2),1:gg%ng(3)) = v ! copy v into the central region of vc
    call dataexchange( gg, vc ) ! fill the halos using MPI communication
    !write(*,*) 'nf= ', gg%nh
    ist = 0


    w = 40.0
    coeff(1) = 0.06 !0.1425
    coeff(2) = 0.03 !0.0600

!$omp parallel do collapse(3) private(ix,iy,iz,ii,tx,ty,tz,v0) schedule(static)
    do iz = 1, g%Ng(3)
      do iy = 1, g%Ng(2)
        do ix = 1, g%Ng(1)

          v0 = vc(ix,iy,iz) ! abbreviation

          ! second derivative in x-direction
          tx = v0*(1.0+w*0.125)
          do ii = 1, 2
            tx = tx + ( vc(ix+ii,iy,iz) + vc(ix-ii,iy,iz) )*coeff(ii)*w
          enddo ! ii

          ! second derivative in y-direction
          ty = 0.0 
          do ii = 1, 2
            ty = ty + ( vc(ix,iy+ii,iz) + vc(ix,iy-ii,iz) ) *coeff(ii)*w
          enddo ! ii

          ! second derivative in z-direction
          tz = 0.0
          do ii = 1, 2
            tz = tz + ( vc(ix,iy,iz+ii) + vc(ix,iy,iz-ii) ) *coeff(ii)*w
          enddo ! ii

          Lv(ix,iy,iz) = (tx + ty + tz + shift)

        enddo ! ix
      enddo ! iy
    enddo ! iz
!$omp end parallel do



  endfunction ! Laplace_fd

  


  status_t function Laplace_fd4_r( v, Lv, g ) result( ist )
  use constants, only: Pi
  use type_grid, only: grid
  use boundary, only: dataexchange
    real, intent(in)            :: v(:,:,:)
    real, intent(out)           :: Lv(:,:,:)
    type(grid), intent(in)      :: g

    character(len=*), parameter :: fun = ' Laplace_fd4: '
    integer, parameter          :: NF = 4
    real, parameter             :: COEFF(-NF:NF) = (/-9.,128.,-1008.,8064.,-14350.,8064.,-1008.,128.,-9./), DENOM = 5040.

    integer                     :: ix, iy, iz, ii
    real                        :: hh(1:3)
    real                        :: vt(1-4:g%ng(1)+4,1-4:g%ng(2)+4,1-4:g%ng(3)+4)
    real                        :: t1, t2(g%ng(1)), t3(g%ng(1),g%ng(2))


! cDBG  if( .not. allocated( c ) ) stop 'Laplace_FDiff: please call prepare function first! (C)'
! cDBG  if( .not. allocated(vec) ) stop 'Laplace_FDiff: please call prepare function first! (VEC)'
    if( any( g%nf /= NF ) ) stop 'Laplace_fd4: routine optimized for g%nf = 4'

! !$omp single
    hh(1:3) = 1.0/( (-4.*Pi)*g%h(1:3)**2 * DENOM )
    vt = 0.0
    ! copy v into the central region of vt
    vt( 1:g%ng(1), 1:g%ng(2), 1:g%ng(3) ) = v

    call dataexchange( g, vt )
    ist = 0
! !$omp end single

! !$omp sections private(ix,iy,iz,t1,t2,t3) reduction(+:Lv)
! !$omp section

    do iz = 1, g%Ng(3)
      do iy = 1, g%Ng(2)
        do ix = 1, g%Ng(1)
          t1 = 0.0
          do ii = -NF, NF
            t1 = t1 + vt(ix+ii,iy,iz) * COEFF(ii)
          enddo ! ii
          Lv(ix,iy,iz) = t1 * hh(1)
        enddo ! ix
      enddo ! iy
    enddo ! iz

! !$omp section

    do iz = 1, g%Ng(3)
      do iy = 1, g%Ng(2)
        t2 = 0.0
        do ii = -NF, NF
          t2(:) = t2(:) + vt(1:g%Ng(1),iy+ii,iz) * COEFF(ii)
        enddo ! ii
        Lv(:,iy,iz) = Lv(:,iy,iz) + t2 * hh(2)
      enddo ! iy
    enddo ! iz

! !$omp section

    do iz = 1, g%Ng(3)
      t3 = 0.0
      do ii = -NF, NF
        t3 = t3 + vt(1:g%Ng(1),1:g%Ng(2),iz+ii) * COEFF(ii)
      enddo ! ii
      Lv(:,:,iz) = Lv(:,:,iz) + t3 * hh(3)
    enddo ! iz

! !$omp end sections

  endfunction ! Laplace_fd4




  function Laplace_coefficients( nfd ) result( c2 )
  use constants, only: Pi
  use configuration, only: WARNING
cDBG  use configuration, only: ERROR
    integer, intent(in) :: nfd ! number of finite difference neighbors
    real                :: c2(0:nfd) ! result

    integer, parameter :: NFMAX = 10
    character(len=*), parameter :: fun = ' Laplace_coefficients: '
    integer, save       :: warn(0:1) = 1
    integer             :: nf

    c2(0:nfd) = 0. ! init needed
    if( nfd > NFMAX ) then
      nf = NFMAX
      if( warn(1) /= nfd ) then
        if(o>0) write(o,'(4A,9(I0,A))') sym, fun, WARNING(0), 'nfd has been truncated from ',nfd,' to ',NFMAX,' along one or more directions!'
        warn(1) = nfd ! disable warning for the next times
      endif ! warn(1)
    else  ! nfd > NFMAX
      nf = nfd
    endif ! nfd > NFMAX

    selectcase( nf )
    case( 1 ) ; c2(0:nf) = (/ -2, 1 /) / 1.
    case( 2 ) ; c2(0:nf) = (/ -30, 16, -1 /) / 12.
    case( 3 ) ; c2(0:nf) = (/ -490, 270, -27, 2 /) / 180.
    case( 4 ) ; c2(0:nf) = (/ -14350, 8064, -1008, 128, -9 /) / 5040.
    case( 5 ) ; c2(0:nf) = (/ -73766, 42000, -6000, 1000, -125, 8 /) / 25200.
    case( 6 ) ; c2(0:nf) = (/ -2480478, 1425600, -222750, 44000, -7425, 864, -50/) / 831600.
    case( 7 ) ; c2(0:nf) = (/ -3.02359410430839, 1.75, -.29166666666666669, .064814814814814811, -.013257575757575758, &
                               2.1212121212121214E-3, -2.2662522662522663E-4, 1.1892869035726179E-5 /)
    case( 8 ) ; c2(0:nf) = (/ -3.05484410430839, 1.7777777777777777, -.31111111111111112, .075420875420875416, -.017676767676767676, &
                               3.4809634809634810E-3, -5.1800051800051804E-4, 5.0742907885765031E-5, -2.4281274281274281E-6 /)
    case( 9 ) ; c2(0:nf) = (/ -3.0795354623330815, 1.8, -.32727272727272727, .084848484848484854, -.022027972027972027, &
                               5.034965034965035E-3, -9.324009324009324E-4, 1.2844298558584272E-4, -1.1569313039901276E-5, &
                               5.0784364509854706E-7 /)
    case(10 ) ; c2(0:nf) = (/ -3.0995354623330815, 1.8181818181818181, -.34090909090909088, .093240093240093247, -.026223776223776224, &
                               6.7132867132867133E-03, -1.4568764568764570E-03, 2.5184899134478967E-04, -3.2136980666392430E-05, &
                               2.6728612899923528E-06, -1.0825088224469030E-07 /)
    case( :0 )
      if( warn(0) /= nfd ) then
        if(o>0) write(o,'(4A,9(I0,A))') sym, fun, WARNING(0), 'no derivative is taken along one ore more directions, nfd = ', nfd
        warn(0) = nfd ! disable warning for the next times
      endif ! warn(0)
    case default ; stop 'LPL: Laplace_coefficients expected case!'
    endselect ! nfd

cDBG  if( abs( c2(0) + 2*sum( c2(1:) ) ) > 1E-12 ) then
cDBG    if(o>0) write(o,'(4A,I0,A,99F24.16)') sym, fun, ERROR, 'real FDcoeff are wrong for nf = ', nfd, ' c:', c2(0:)
cDBG    stop 'LPL prepare: real FDcoeff are wrong.'
cDBG  endif ! ic wrong

! cDBG  write(*,'(99F13.9)') c2(0:nfd)
  endfunction ! Laplace_coefficients



  status_t function Laplace_fd_prepare_r( g ) result( ist )  ! , rho, b )
  use constants, only: Pi
  use configuration, only: WARNING, ERROR
  use type_grid, only: grid
    type(grid), intent(in)      :: g

    character(len=*), parameter :: fun = ' Laplace_fd_prepare: '
    integer, parameter          :: DENOM = -1
    integer, parameter          :: NFMAX =  6
    integer(kind=8), parameter  :: ic(DENOM:NFMAX,0:NFMAX) = reshape( (/ & 
         ! DENOM,          0,      1,      2,    3,    4,  5,  6,
               1,          0,      0,      0,    0,    0,  0,  0, & ! Laplacian switched off
               1,         -2,      1,      0,    0,    0,  0,  0, & !  2nd order Laplacian(lowest)
              12,        -30,     16,     -1,    0,    0,  0,  0, & !  4th order
             180,       -490,    270,    -27,    2,    0,  0,  0, & !  6th order
            5040,     -14350,   8064,  -1008,  128,   -9,  0,  0, & !  8th order
           25200,     -73766,  42000,  -6000, 1000, -125,  8,  0, & ! 10th order
          831600,   -2480478,1425600,-222750,44000,-7425,864,-50  & ! 12th order
               /), (/NFMAX+2,NFMAX+1/) )
    ! continuation of ic without denom
    real, parameter            :: rc(0:10,7:10) = reshape( (/ &
  -3.0235941043083900E+00, &
   1.7500000000000000E+00, &
  -2.9166666666666669E-01, &
   6.4814814814814811E-02, &
  -1.3257575757575758E-02, &
   2.1212121212121214E-03, &
  -2.2662522662522663E-04, &
   1.1892869035726179E-05, &
   0.0000000000000000E+00, &
   0.0000000000000000E+00, &
   0.0000000000000000E+00, &

  -3.0548441043083900E+00, &
   1.7777777777777777E+00, &
  -3.1111111111111112E-01, &
   7.5420875420875416E-02, &
  -1.7676767676767676E-02, &
   3.4809634809634810E-03, &
  -5.1800051800051804E-04, &
   5.0742907885765031E-05, &
  -2.4281274281274281E-06, &
   0.0000000000000000E+00, &
   0.0000000000000000E+00, &

  -3.0795354623330815E+00, &
   1.8000000000000000E+00, &
  -3.2727272727272727E-01, &
   8.4848484848484854E-02, &
  -2.2027972027972027E-02, &
   5.0349650349650350E-03, &
  -9.3240093240093240E-04, &
   1.2844298558584272E-04, &
  -1.1569313039901276E-05, &
   5.0784364509854706E-07, &
   0.0000000000000000E+00, &

  -3.0995354623330815E+00, &
   1.8181818181818181E+00, &
  -3.4090909090909088E-01, &
   9.3240093240093247E-02, &
  -2.6223776223776224E-02, &
   6.7132867132867133E-03, &
  -1.4568764568764570E-03, &
   2.5184899134478967E-04, &
  -3.2136980666392430E-05, &
   2.6728612899923528E-06, &
  -1.0825088224469030E-07 /) , (/11,4/) )

    integer                     :: ii, mf, nf, id, nfs(1:3)

    mf = min( maxval( abs(g%nf) ), 10 )
    if( allocated( c ) ) deallocate( c )
    allocate( c(-mf:mf,3,1) )
    do id = 1, 3
      nf = min( max( 0, g%nf(id) ), 10 )
      nfs(id) = nf
      if( nf <= NFMAX ) then
        !================================================================
        ! 0:6
        !================================================================
cDBG    if( ic(0,nf) + 2*sum( ic(1:,nf) ) /= 0 ) then
cDBG      if(o>0) write(o,'(4A,I0,A,I9,I8,I7,I6,I5,I4,9I3)') sym, fun, ERROR, 'integer FDcoeff are wrong for nf=', nf, '  ic =', ic(0:,nf)
cDBG      stop 'LPL prepare: integer FDcoeff are wrong.'
cDBG    endif ! ic wrong
        do ii = 0, mf
          c( ii,id,1) = real( ic(ii,nf) )/( (-4.*Pi)*g%h(id)**2 * real(ic(DENOM,nf)) )
          c(-ii,id,1) = c( ii,id,1) ! symmetric
        enddo ! ii
        !================================================================
      elseif( nf <= 10 ) then
        !================================================================
        ! 7:10
        !================================================================
cDBG    if( abs( rc(0,nf) + 2*sum( rc(1:,nf) ) ) > 1.E-12 ) then
cDBG      if(o>0) write(o,'(4A,I0,A,99F24.16)') sym, fun, ERROR, 'real FDcoeff are wrong for nf=', nf, '  rc =', rc(0:,nf)
cDBG      stop 'LPL prepare: real FDcoeff are wrong.'
cDBG    endif ! ic wrong
        do ii = 0, mf
          c( ii,id,1) = rc(ii,nf)/( (-4.*Pi)*g%h(id)**2 )
          c(-ii,id,1) = c( ii,id,1) ! symmetric
        enddo ! ii
        !================================================================
      else ; stop 'LPL prepare, NF too large!'
      endif ! nf < NFMAX
    enddo ! id

    if( o > 0 ) then
      if( any( nfs < 1 ) ) write(o,'(4A,3I3,9A)') sym, fun, WARNING(0), &
        'no derivative is taken along one ore more directions, nf =[', nfs, ' ]'
      if( any( g%nf(:) > nfs ) ) write(o,'(4A,I0,A,3I3,9A)') sym, fun, WARNING(0), &
        'nf has been truncated to ',NFMAX,' along one ore more directions, nf =[', nfs, ' ]'
    endif ! o > 0

cDBG  if( any(g%ng < 1 ) ) stop 'set_Laplacian: # of grid points must be positive.'

    deallocate( t2, t3, vec, stat=ist  )
! cDBG  if(o>0) write(o,'(3A,3I3,9A)') sym, fun, 'halo size [', g%nh, ' ]'

    allocate( vec(  1-g%nh(1):g%ng(1)+g%nh(1), &
                    1-g%nh(2):g%ng(2)+g%nh(2), &
                    1-g%nh(3):g%ng(3)+g%nh(3) ), stat=ist )
    init_Laplacian = ( ist == 0 )
cDBG  if( ist /= 0 ) stop 'set_Laplacian: allocation of halo vector "vec" in Laplacian failed.'

    allocate( t2( g%ng(1) ), t3( g%ng(1),g%ng(2) ), stat=ist )
    if( ist /= 0 ) stop 'set_Laplace: allocation of halo vector "vec" in Laplacian failed.'

!     b = rho ! just copy, no modification
    ist = 0
  endfunction ! Laplace_fd_prepare




  status_t function precon_r( v, Pv, g ) result( ist )
  use type_grid, only: grid
  use boundary, only: dataexchange
    ! arguments
    real, intent(in)            :: v(:,:,:)
    real, intent(out)           :: Pv(:,:,:)
    type(grid), intent(in)      :: g

    character(len=*), parameter :: fun = ' precon: '
    integer                     :: i1, i2, i3, nh
    real                        :: w000, w100, w110, w111, t1

    vec = 0.0 ! clear halo-enlarged vector
    vec( 1:g%ng(1), 1:g%ng(2), 1:g%ng(3) ) = v ! copy v into the central region of vec

    call dataexchange( g, vec )
    ist = 0

    nh = 1
    selectcase( nh )
    case( 1 )

      ! w000 = 1. - 6.*w100 - 12.*w110 - 8.*w111
      ! w110 = w100/sqrt(2.)
      ! w111 = w100/sqrt(3.)
      ! w000 > 0. ==> 1. > 36.827*w100
      !          <==> w100 < 0.027154

      w100 = 0.013577
      w110 = w100/sqrt(2.)
      w111 = w100/sqrt(3.)
      w000 = 1. - 6.*w100 - 12.*w110 - 8.*w111

!$omp do collapse(3) private(i3,i2,i1,t1) schedule(static)
      do i3 = 1, g%Ng(3)
        do i2 = 1, g%Ng(2)
          do i1 = 1, g%Ng(1)

            t1 = w000 * vec(i1,i2,i3)
            t1 = t1 + w100*( vec(i1-1,i2,i3) + vec(i1+1,i2,i3) &
                           + vec(i1,i2-1,i3) + vec(i1,i2+1,i3) &
                           + vec(i1,i2,i3-1) + vec(i1,i2,i3+1) )
            t1 = t1 + w110*( vec(i1-1,i2-1,i3) + vec(i1+1,i2-1,i3) &
                           + vec(i1-1,i2+1,i3) + vec(i1+1,i2+1,i3) &
                           + vec(i1,i2-1,i3-1) + vec(i1,i2-1,i3+1) &
                           + vec(i1,i2+1,i3-1) + vec(i1,i2+1,i3+1) &
                           + vec(i1-1,i2,i3-1) + vec(i1+1,i2,i3-1) &
                           + vec(i1-1,i2,i3+1) + vec(i1+1,i2,i3+1) )
            t1 = t1 + w111*( vec(i1-1,i2-1,i3-1) + vec(i1+1,i2-1,i3-1) &
                           + vec(i1-1,i2+1,i3-1) + vec(i1+1,i2+1,i3-1) &
                           + vec(i1-1,i2-1,i3+1) + vec(i1+1,i2-1,i3+1) &
                           + vec(i1-1,i2+1,i3+1) + vec(i1+1,i2+1,i3+1) )
            Pv(i1,i2,i3) = t1

          enddo ! i1
        enddo ! i2
      enddo ! i3
!$omp end do

cDBG  case default ; stop 'Precon: only nf = 1 implemented!'
    endselect

  endfunction ! precon




  ! special routines for the Numerov method
  status_t function Laplace_nm_r( v, Lv, g ) result( ist )
  use type_grid, only: grid
  use boundary, only: dataexchange
    real, intent(in)            :: v(:,:,:)
    real, intent(out)           :: Lv(:,:,:)
    type(grid), intent(in)      :: g

    character(len=*), parameter :: fun = ' Numerov: '
    integer                     :: ix, iy, iz

cDBG  if( .not. allocated( c ) ) stop 'Laplace_Numerov: please call prepare function first! (C)'
cDBG  if( .not. allocated(vec) ) stop 'Laplace_Numerov: please call prepare function first! (VEC)'

    vec = 0.0

    ! copy v into the central region of vec
    vec( 1:g%ng(1), 1:g%ng(2), 1:g%ng(3) ) = v

    call dataexchange( g, vec )
    ist = 0

    do iz = 1, g%Ng(3)
      do iy = 1, g%Ng(2)
        do ix = 1, g%Ng(1)
          Lv(ix,iy,iz) = &
            sum(    c(  -1:  +1,  -1:  +1,  -1:  +1) &
                * vec(ix-1:ix+1,iy-1:iy+1,iz-1:iz+1) )
        enddo ! ix
      enddo ! iy
    enddo ! iz

  endfunction ! Laplace_nm


  status_t function Laplace_nm_prepare_r( rho, b, g ) result( ist )
  use constants, only: Pi
  use type_grid, only: grid
  use boundary, only: dataexchange
    real, intent(in)            :: rho(:,:,:)
    real, intent(out)           :: b(:,:,:)
    type(grid), intent(in)      :: g

    character(len=*), parameter :: fun = ' Numerov_prepare: '
    integer                     :: ix, iy, iz
    real                        :: h2(3)
cDBG  logical                     :: show = .false.
cDBG  real                        :: f
 

    if( allocated( c ) ) deallocate( c )
    allocate( c(-1:1,-1:1,-1:1) )

    ! right hand side stencil
    c(:,:,:) = 0.001

    c(0,:,:) = 0.01
    c(:,0,:) = 0.01
    c(:,:,0) = 0.01

    c(:,0,0) = 0.1
    c(0,:,0) = 0.1
    c(0,0,:) = 0.1

    c(0,0,0) = 1.0

    if( allocated( vec ) ) deallocate( vec )

    ! Halo size is 1 for each direction
    allocate( vec(  1-g%nh(1):g%ng(1)+g%nh(1), &
                    1-g%nh(2):g%ng(2)+g%nh(2), &
                    1-g%nh(3):g%ng(3)+g%nh(3) ), stat=ist )
cDBG  if( ist /= 0 ) stop 'set_Laplacian: allocation of halo vector "vec" in Laplacian failed.'

    vec = 0.0

    ! copy rho into the central region of vec
    vec( 1:g%ng(1), 1:g%ng(2), 1:g%ng(3) ) = rho

    call dataexchange( g, vec )

    ! apply the right hand side stencil to find b
    do iz=1, g%Ng(3)
      do iy=1, g%Ng(2)
        do ix=1, g%Ng(1)
          b(ix,iy,iz) = &
            sum(    c(  -1:  +1,  -1:  +1,  -1:  +1) &
                * vec(ix-1:ix+1,iy-1:iy+1,iz-1:iz+1) )
        enddo ! ix
      enddo ! iy
    enddo ! iz


    ! prepare the left hand side stencil (Laplacian)
    h2 = g%h*g%h*(-4.*Pi) ! factor (-4Pi) for the unit system
                          ! of the Poisson equation:
                          !
                          ! Laplace{ V_H } = -4Pi*rho
                          ! [rho] = e*a.u.^(-3) (a.u. == Bohr)
                          ! [V_H] = Hartree

    ! spatial diag
    c(:,:,:) = -0.012/h2(1) - 0.012/h2(2) - 0.012/h2(3)
    ! diag
    c(0,:,:) =              0.12/h2(2) + 0.12/h2(3) + 2. * 0.012/h2(1)
    c(:,0,:) = 0.12/h2(1)              + 0.12/h2(3) + 2. * 0.012/h2(2)
    c(:,:,0) = 0.12/h2(1) + 0.12/h2(2)              + 2. * 0.012/h2(3)
    ! main
    c(:,0,0) = 1.2/h2(1) - 2. * (             0.12/h2(2) + 0.12/h2(3) )
    c(0,:,0) = 1.2/h2(2) - 2. * ( 0.12/h2(1)             + 0.12/h2(3) )
    c(0,0,:) = 1.2/h2(3) - 2. * ( 0.12/h2(1) + 0.12/h2(2)             )
    ! central
    c(0,0,0) = -2. * ( 1.2/h2(1) + 1.2/h2(2) + 1.2/h2(3) )

cDBG  if( o > 0 .and. show ) then
cDBG    f = (-4.*Pi)
cDBG    write(o,'(3A,99F10.3)') ''
cDBG    write(o,'(3A,A10,99F10.3)') sym, fun, '    000      =', ' ', c(0,0,0) * f
cDBG    write(o,'(3A,99F10.3)')     sym, fun, '100 010 001  =', (/c(1,0,0), c(0,1,0), c(0,0,1)/) * f
cDBG    write(o,'(3A,99F10.3)')     sym, fun, '011 101 110  =', (/c(0,1,1), c(1,0,1), c(1,1,0)/) * f
cDBG    write(o,'(3A,A10,99F10.3)') sym, fun, '    111      =', ' ', c(1,1,1) * f
cDBG    write(o,'(3A,99F10.3)') ''
! cDBG  stop 'DEBUG mod_Laplacian.F90 line 333'
cDBG    show = .false.
cDBG  endif ! o > 0 and show

    ist = 0
  endfunction ! Laplace_nm_prepare




  ! special routines for the Mehrstellen method
  status_t function Laplace_ms_r( v, Lv, g ) result( ist )
  use type_grid, only: grid
  use boundary, only: dataexchange
    real, intent(in)            :: v(:,:,:)
    real, intent(out)           :: Lv(:,:,:)
    type(grid), intent(in)      :: g

    character(len=*), parameter :: fun = ' Mehrstellen: '
    integer                     :: ix, iy, iz
    real                        :: t

cDBG  if( .not. allocated( c ) ) stop 'Laplace_Mehrstellen: please call prepare function first! (C)'
cDBG  if( .not. allocated(vec) ) stop 'Laplace_Mehrstellen: please call prepare function first! (VEC)'

    vec = 0.0

    ! copy v into the central region of vec
    vec( 1:g%ng(1), 1:g%ng(2), 1:g%ng(3) ) = v

    call dataexchange( g, vec )

!$omp parallel do collapse(3) private(ix,iy,iz,t) schedule(static)
    do iz = 1, g%Ng(3)
      do iy = 1, g%Ng(2)
        do ix = 1, g%Ng(1)
          ! central
          t = c(0,0,0)*vec(ix,iy,iz)
          ! main
          t = t + c(1,0,0)*( vec(ix-1,iy,iz) + vec(ix+1,iy,iz) )
          t = t + c(0,1,0)*( vec(ix,iy-1,iz) + vec(ix,iy+1,iz) )
          t = t + c(0,0,1)*( vec(ix,iy,iz-1) + vec(ix,iy,iz+1) )
          ! diag
          t = t + c(1,1,0)*( vec(ix-1,iy-1,iz) + vec(ix+1,iy-1,iz) &
                           + vec(ix-1,iy+1,iz) + vec(ix+1,iy+1,iz) )
          t = t + c(0,1,1)*( vec(ix,iy-1,iz-1) + vec(ix,iy+1,iz-1) &
                           + vec(ix,iy-1,iz+1) + vec(ix,iy+1,iz+1) )
          t = t + c(1,0,1)*( vec(ix-1,iy,iz-1) + vec(ix-1,iy,iz+1) &
                           + vec(ix+1,iy,iz-1) + vec(ix+1,iy,iz+1) )
          ! store
          Lv(ix,iy,iz) = t
        enddo ! ix
      enddo ! iy
    enddo ! iz
!$omp end parallel do
    ist = 0
  endfunction ! Laplace_ms

  status_t function Laplace_ms_prepare_r( rho, b, g ) result( ist )
  use constants, only: Pi
  use type_grid, only: grid
  use boundary, only: dataexchange
    ! arguments
    real, intent(in)            :: rho(:,:,:)
    real, intent(out)           :: b(:,:,:)
    type(grid), intent(in)      :: g

    character(len=*), parameter :: fun = ' Mehrstellen_prepare: '
    integer                     :: ix, iy, iz
    real                        :: a, bx, by, bz, b0, cxy, cyz, czx
    real                        :: h2(3)
cDBG  logical                     :: show = .false.
cDBG  real                        :: f

    if( allocated( vec ) ) deallocate( vec )
    ! Halo size is 1 for each direction
    allocate( vec(  1-g%nh(1):g%ng(1)+g%nh(1), &
                    1-g%nh(2):g%ng(2)+g%nh(2), &
                    1-g%nh(3):g%ng(3)+g%nh(3) ), stat=ist )
cDBG  if( ist /= 0 ) stop 'set_Laplacian: allocation of halo vector "vec" in Laplacian failed.'

    vec = 0.0

    ! copy v into the central region of vec
    vec( 1:g%ng(1), 1:g%ng(2), 1:g%ng(3) ) = rho
    
    call dataexchange( g, vec )

    ! Central point finite difference formula
    !
    !  b = rho + h^2/12 L(1st order finite difference)[rho]
    !

    ! c1  = 1./12.
    ! central
    ! c0  = 1. - 6.*c1  ! = 0.5

    do iz=1, g%Ng(3)
      do iy=1, g%Ng(2)
        do ix=1, g%Ng(1)
          b(ix,iy,iz) = 0.5*vec(ix,iy,iz)  &
               + 0.5*( vec(ix-1,iy,iz) + vec(ix+1,iy,iz) &
                     + vec(ix,iy-1,iz) + vec(ix,iy+1,iz) &
                     + vec(ix,iy,iz-1) + vec(ix,iy,iz+1) )/6.
        enddo ! ix
      enddo ! iy
    enddo ! iz


    if( allocated( c ) ) deallocate( c )
    allocate( c(-1:1,-1:1,-1:1) )


    ! prepare the left hand side stencil (Laplacian)
    h2 = g%h*g%h*(-4.*Pi) ! factor (-4Pi) for the unit system
                          ! of the Poisson equation:
                          !
                          ! Laplace{ V_H } = -4Pi*rho
                          ! [rho] = e*a.u.^(-3) (a.u. == Bohr)
                          ! [V_H] = Hartree


    ! Mehrstellen coefficients 1/(6h^2)*
    !---------------------------------------
    !  0  1  0   |   1  2  1   |   0  1  0
    !  1  2  1   |   2 -24 2   |   1  2  1
    !  0  1  0   |   1  2  1   |   0  1  0
    !---------------------------------------


    h2 = g%h*g%h*4.*(-4.*Pi)
    ! diag
    cxy =  ( 1./( 12.*h2(1) ) + 1./( 12.*h2(2) ) )
    cyz =  ( 1./( 12.*h2(2) ) + 1./( 12.*h2(3) ) )
    czx =  ( 1./( 12.*h2(3) ) + 1./( 12.*h2(1) ) )
    ! main
    b0  =  1./( 6.*h2(1) ) + 1./( 6.*h2(2) ) + 1./( 6.*h2(3) )
    bx  =  5./( 6.*h2(1) ) - b0
    by  =  5./( 6.*h2(2) ) - b0
    bz  =  5./( 6.*h2(3) ) - b0
    ! central
    a   = - 8.*b0


    c(:,:,:)  =    0.
    c(:,:,0)  =  cxy
    c(0,:,:)  =  cyz
    c(:,0,:)  =  czx
    c(:,0,0)  =   bx
    c(0,:,0)  =   by
    c(0,0,:)  =   bz
    c(0,0,0)  =    a

cDBG  if( o > 0 .and. show ) then
cDBG    f = 6.*(-4.*Pi)
cDBG    write(o,'(3A,99F10.3)') ''
cDBG    write(o,'(3A,A10,99F10.3)') sym, fun, '    000      =', ' ', c(0,0,0) * f
cDBG    write(o,'(3A,99F10.3)')     sym, fun, '100 010 001  =', (/c(1,0,0), c(0,1,0), c(0,0,1)/) * f
cDBG    write(o,'(3A,99F10.3)')     sym, fun, '011 101 110  =', (/c(0,1,1), c(1,0,1), c(1,1,0)/) * f
cDBG    write(o,'(3A,A10,99F10.3)') sym, fun, '    111      =', ' ', c(1,1,1) * f
cDBG    write(o,'(3A,99F10.3)') ''
cDBG    show = .false.
cDBG  endif ! o > 0 and show

    ist = 0
  endfunction ! Laplace_ms_prepare


  status_t function precon_Nf1_r( g, v, Pv ) result( ist ) ! Preconditioner with Nf=1, rank4
  use boundary, only: dataexchange
  use type_grid, only: grid
    type(grid), intent(in):: g
    real, intent(in)      :: v (:,:,:)
    real, intent(out)     :: Pv(:,:,:)

    real, parameter       :: c0 = 1./2.
    real, parameter       :: c1 = 1./12. ! c0 + c1 * 6 neighbors = 1.0
    integer               :: ix, iy, iz!, is, ixyzs, ixyzs_start
    type(grid)            :: mygrid
    real                  :: vec(1-1:g%ng(1)+1,1-1:g%ng(2)+1,1-1:g%ng(3)+1)

    mygrid = g
    mygrid%nh = 1

    vec = 0.0
!       do is = 1, g%ng(4)

      do iz = 1, g%ng(3)
        do iy = 1, g%ng(2)
          do ix = 1, g%ng(1)
            vec(ix,iy,iz) = v(ix,iy,iz)!,is)
          enddo ! ix
        enddo ! iy
      enddo ! iz

      call dataexchange( mygrid, vec )
      ist = 0

  !        ixyzs = ixyzs_start
      do iz = 1, g%ng(3)
        do iy = 1, g%ng(2)
          do ix = 1, g%ng(1)
  !              ixyzs = ixyzs+1
            Pv(ix,iy,iz) = c0* vec(ix,iy,iz) + c1* ( &
              vec(ix-1,iy,iz) + vec(ix+1,iy,iz) + &
              vec(ix,iy-1,iz) + vec(ix,iy+1,iz) + &
              vec(ix,iy,iz-1) + vec(ix,iy,iz+1) )
          enddo ! ix
        enddo ! iy
      enddo ! iz

!   enddo ! is

  endfunction ! precon_Nf1

#ifdef EXTENDED
!+ extended

  status_t function test( ) result( ist )
  use configuration, only: o
  use type_grid, only: grid, set, BC_FINITE

#ifdef EXTENDED
  use toolbox, only: write_vtk_file
#endif
    integer, parameter  :: n(1:3) = 256
    real, parameter     :: s(1:3) = 0.125*n

    character(len=*), parameter     :: fun = ' test: '
    real                :: Lv(n(1),n(2),n(3)), v(n(1),n(2),n(3))
    type(grid)          :: g
    integer             :: nf, i1, i2, i3, bc(1:3,1:2) = BC_FINITE
    real                :: r2, rv(1:3), h23, q

    g = set( s, n, nspins=1, bc=bc ) ! BC_FINITE

    g%off = g%off - s/2. ! center

    h23 = g%hvol**(2./3.) * 1E-6

!! no collapse possible here
!$omp parallel do private(i3,i2,i1,rv,r2) schedule(static)
      do i3 = 1, g%ng(3)     ; rv(3) = i3*g%h(3) + g%off(3)
        do i2 = 1, g%ng(2)   ; rv(2) = i2*g%h(2) + g%off(2)
          do i1 = 1, g%ng(1) ; rv(1) = i1*g%h(1) + g%off(1)
            !---------------------------------
            r2 = rv(1)*rv(1) + rv(2)*rv(2) + rv(3)*rv(3)
!             v(i1,i2,i3) = 1./sqrt( r2 + h23 )
            if( r2 < 36. ) then ; v(i1,i2,i3) = exp( -0.5*r2 ) ; else ; v(i1,i2,i3) = 0. ; endif
            !---------------------------------
          enddo ! i1
        enddo ! i2
      enddo ! i3
!$omp end parallel do

    do nf = 0, 11
      g%nh = nf ; g%nf = nf ! halo size and finite difference range

      ist = Laplace( v, Lv, g )
      ! now Lv is supposed to be the density rho
!       q = sum( Lv(1+2*g%nh(1):g%ng(1)-2*g%nh(1), &
!                   1+2*g%nh(2):g%ng(2)-2*g%nh(2), &
!                   1+2*g%nh(3):g%ng(3)-2*g%nh(3)) ) * g%hvol
      q = sum( Lv ) * g%hvol
      if(o>0) write(o,'(3A,I0,A,F0.12)') sym, fun, 'nf = ',nf,' point charge q = ',q
    enddo ! nf

!     call radial_hist( file='Laplace_pointcharge', data=Lv, hg=g%h, origin=pos-g%off, ellmax='p' )
!     call radial_histogram( file='Laplace_pointcharge_s', data=Lv, hg=g%h, origin=pos-g%off, lm=1 )
#ifdef EXTENDED

#endif
  endfunction ! test

!- extended
#endif

endmodule ! Laplacian

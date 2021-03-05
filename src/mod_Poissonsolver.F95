#include "config.h"

#ifdef DEBUG
#undef DEBUG
! DEBUG off!
#endif

! #define DEBUG
! #define FULL_DEBUG


#ifdef R1_C2
! This file will be preprocessed twice, first
! with R1_C2 == 1, and then with R1_C2 == 2
! then the two parts are concatenated to one source.
! This requires that the module head has to be
! appearing, when R1_C2 == 1 and the line "end module <name>"
! with R1_C2 == 2.
#if R1_C2 == 1

!! @author Paul Baumeister
!! @version 3.0
!! @see configuration
!! @see constants
!! @see grace
!! @see MPIconst
!! @see debugtools
!! @see fft_singleton
!! @see type_grid
!! @see MPItools
!! @see communicators
!! @see toolbox
!! @see Laplacian
!!
module Poissonsolver
  use configuration, only: o ! output unit, 0: no output
  use MPIconst, only: PREC
  use type_item, only: item, operator(.in.)
implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: sym = 'PSS' !! module symbol

  public :: Poissonsolve
  public :: is_iterative_method
#ifdef EXTENDED
  public :: test
#endif

  ! keys
  integer, parameter :: KEY_UNKNOWN = -1

  integer, parameter :: KEY_FD = 0 ! Finite Difference
  integer, parameter :: KEY_MS = 1 ! Mehrstellen Method
  integer, parameter :: KEY_NM = 2 ! Numerov Method
  integer, parameter :: KEY_DI = 3 ! Direct Integration
  integer, parameter :: KEY_BT = 4 ! Bessel Transform
  integer, parameter :: KEY_FT = 5 ! Fast Fourier Trans

  integer, parameter :: KEY_DEFAULT = KEY_FD

  ! mapping words <--> keys for the input and output
  type(item), parameter, public :: Dictionary(14) = (/ &
    item('finite_difference',KEY_FD), item('fd',KEY_FD), item('FD',KEY_FD), &
    item('Mehrstellen',KEY_MS), &
    item('Numerov',KEY_NM), &
    item('direct',KEY_DI), &
    item('Bessel',KEY_BT), &
    item('fft',KEY_FT), item('Fourier',KEY_FT), &
    item('default',KEY_DEFAULT), item('',KEY_DEFAULT), &
    item('min',KEY_DEFAULT), item('max',KEY_DEFAULT), item('<',KEY_DEFAULT) /)

  interface cg_solve
!   module procedure cg_solve_precon
    module procedure cg_solve_no_precon
  endinterface

  interface spherical_solve
    module procedure spherical_solve_r
  endinterface

  interface direct_solve
    module procedure direct_solve_r
  endinterface

  interface norm2
    module procedure norm2_r, norm2_c
  endinterface

  interface norm1
    module procedure norm1_r, norm1_c
  endinterface

  interface scalar_product
    module procedure scalar_product_r, scalar_product_c
  endinterface

  interface fourier_solve
    module procedure fourier_solve_r!, fourier_solve_c
  endinterface

  integer, parameter :: CG_MAXITER_HARDLIMIT = 1000

  contains

  logical function is_iterative_method( key ) result( is )
    integer, intent(in) :: key

    selectcase( key )
    case( KEY_FD, KEY_NM, KEY_MS ) ; is = .true.
    case( KEY_DI, KEY_BT, KEY_FT ) ; is = .false.
    case default ; stop 'PSS: iterative_method: undetermined key value'
    endselect ! key

  endfunction ! is_iterative_method

  status_t function Poissonsolve( x, rho, g, threshold, res, &
                     maxiter, miniter, restart, key ) result( ist )
!   use Laplacian, only: Laplace, prepare
!   use Laplacian, only: Laplace_fd_r, Laplace_fd_prepare_r
!   use Laplacian, only: Laplace_nm_r, Laplace_nm_prepare_r
!   use Laplacian, only: Laplace_ms_r, Laplace_ms_prepare_r
  use type_grid, only: grid
  use MPIconst, only: Wtime
  use configuration, only: error
  use type_item, only: operator(.in.), min_key, max_key
    real, intent(inout)               :: x(:,:,:)       ! solve L*x = rho
    real, intent(in)                  :: rho(:,:,:)     ! inhomogeneous part rho
    type(grid), intent(in)            :: g
    real, intent(in)                  :: threshold      ! threshold
    real, intent(out)                 :: res            ! residual
    integer, intent(in), optional     :: maxiter        ! maximum number of iterations
    integer, intent(in), optional     :: miniter        ! minium number of iterations
    integer, intent(in), optional     :: restart        ! number of iterations before restart
    integer, intent(in), optional     :: key

    character(len=*), parameter       :: fun = ' Poisson solver '
    integer                           :: k
    logical                           :: ok
    real                              :: time
    time = Wtime()

    k = KEY_DEFAULT ; if( present( key ) ) k = key

    selectcase( k )
    case( KEY_DI )
      ist = direct_solve_r( x, rho, g )
      res = 0.

    case( KEY_BT )
      ist = spherical_solve_r( g, rho, x, origin=-g%off )
      res = 0.

    case( KEY_FT )
      ist = fourier_solve_r( x, rho, g )
      res = 0.


!     case( KEY_FD )
!       ist = cg_solve_r( x, rho, threshold, res, g, &
!                                     Laplace_fd_r, Laplace_fd_prepare_r, &
!                                     maxiter, miniter, restart )
! 
!     case( KEY_NM )
!       ist = cg_solve_r( x, rho, threshold, res, g, &
!                                     Laplace_nm_r, Laplace_nm_prepare_r, &
!                                     maxiter, miniter, restart )
! 
!     case( KEY_MS )
!       ist = cg_solve_r( x, rho, threshold, res, g, &
!                                     Laplace_ms_r, Laplace_ms_prepare_r, &
!                                     maxiter, miniter, restart )
! 
    case( KEY_UNKNOWN )
      write(*,'(99A)') sym, fun, error, 'unknown solver for the Poisson equation, please use one of ',&
        (' "',trim(k.in.Dictionary),'"', k=min_key(Dictionary),max_key(Dictionary))
      stop 'ERROR! unknown solver for the Poisson equation' 

    case default
      ist = cg_solve( x, rho, threshold, res, g, maxiter, miniter, restart )
      if( res > threshold ) ist = 1
    endselect ! k
    time = Wtime() - time
#ifdef TIME
    if(o>0) write(o,'(5A,F0.3,9A)') sym, fun, '"',trim(k.in.Dictionary),'" took ', time, ' sec'
#endif
  endfunction ! Poissonsolve


  status_t function cg_solve_no_precon( x, b, threshold, res, g, &
                       maxiter, miniter, restart ) result( ist )
  use type_grid, only: grid, BC_PERIODIC
  use Laplacian, only: Laplace
    real, intent(inout)               :: x(:,:,:)       ! solve A*x = b
    real, intent(in)                  :: b(:,:,:)       ! inhomogeneous part b
    real, intent(in)                  :: threshold      ! threshold
    real, intent(out)                 :: res            ! residual
    type(grid), intent(in)            :: g              ! grid descriptor
    integer, intent(in), optional     :: maxiter        ! maximum number of iterations
    integer, intent(in), optional     :: miniter        ! minium number of iterations
    integer, intent(in), optional     :: restart        ! number of iterations before restart
    ! CG can be switched off, only steepest descent by setting restart=1

    character(len=*), parameter       :: fun = ' CG solve: '
    real, parameter                   :: RZ_TINY = 1E-14
    real, parameter                   :: RS_TINY = 1E-10
    integer                           :: mxit, mnit ! maximum, minimum iterations
    integer                           :: it_restart, it = 0     ! iteration counter
    logical                           :: run !
    real                              :: xnorm
    real                              :: threshold2
    real                              :: alpha, beta, pAp, rzo, rzn, res2
    real                              :: res_start

    real, allocatable :: r(:,:,:), p(:,:,:), ax(:,:,:), ap(:,:,:)

    allocate( r (g%ng(1),g%ng(2),g%ng(3)), &
              p (g%ng(1),g%ng(2),g%ng(3)), &
              ax(g%ng(1),g%ng(2),g%ng(3)), &
              ap(g%ng(1),g%ng(2),g%ng(3)), stat=ist )
    if( ist /= 0 ) then
      if(o>0) write(o,'(3A,F0.3,9A)') sym, fun, 'failed to allocate ', 4 * product(g%ng(1:3))*8*.5**10, ' kiByte'
      stop 'PSS cg_solve: failed to allocate arrays!'
    endif !

    mxit = CG_MAXITER_HARDLIMIT ; if( present(maxiter) ) mxit = min( maxiter, CG_MAXITER_HARDLIMIT )
    mnit = 0                    ; if( present(miniter) ) mnit = max( 0, miniter )
    it_restart = 2**12          ; if( present(restart) ) it_restart = max(1,restart)

    threshold2 = g%svol * threshold**2
#ifdef DEBUG
    if( g%svol <= 0.0 ) stop 'cg_solve: the cell volume is not positive.'
#endif

    ! without preconditioning
    ! |Ax> = A|x>
    ! |r> = |b> - |Ax>
    ! rz_old = <r|r>
    ! |p> = |r>
    ! it = 0
    ! do while
    !   |Ap> = A|p>
    !   pAp = <p|Ap>
    !   alpha = rz_old / pAp
    !   |x> = |x> + alpha |p>
    !   |r> = |r> - alpha |Ap>
    !   res = <r|r>
    !   |r> = |r>
    !   rz_new = <r|r>
    !   beta = rz_new / rz_old
    !   rz_old = rz_new
    !   |p> = |r> + beta |p>
    !   it = it+1


    ! |Ax> := A|x>
    ist = Laplace( x, ax, g )
    if( ist /= 0 ) stop 'CG_solve: Laplacian failed!'

    ! |r> = |b> - A|x> = |b> - |Ax>
    r = b - ax

    ! res^2 = <r|r>
    res2 = norm2( r, g%hvol, g%comm )
    res = sqrt(res2/g%svol)
    res_start = res ! store staring residual

    ! rz_old = <r|r>
    rzo = norm2( r, g%hvol, g%comm )

    ! |p> = |r>
    p = r

    it = 0 ! init iteration counter

    ! number of iterations is less then maxiter?
    run = ( it < maxiter )
    do while( run )
      ! it = it+1
      it = it + 1
      !--------------------------------------
      ! begin of the CG iteration
      !--------------------------------------

      ! |ap> = A|p>
      ist = Laplace( p, ap, g )
      if( ist /= 0 ) stop 'CG_solve: Laplacian failed!'

      pAp = scalar_product( p, ap, g%hvol, g%comm )

      ! alpha = rz_old / pAp
      alpha = rzo / pAp
      if( abs(pAp) < RZ_TINY ) alpha = RZ_TINY

      ! |x> = |x> + alpha |p>
      x = x + alpha * p

      !============================================================
      ! special treatment of completely periodic case
      !============================================================
      if( all( g%bc(1:3,1) == BC_PERIODIC ) ) then
        xnorm = norm1( x, 1.0, g%comm ) ! 1.0 instead of g%hvol
        xnorm = xnorm/real( g%ng_all(1)*g%ng_all(2)*g%ng_all(3) )
        ! subtract the average potential
        x = x - xnorm
      endif ! 3 periodic BCs
      !============================================================

      if( mod( it, it_restart ) == 0 ) then
        ! |Ax> = A|x>
        ist = Laplace( x, ax, g )
       if( ist /= 0 ) stop 'CG_solve: Laplacian failed!'
        ! |r> = |b> - A|x> = |b> - |ax>
        r = b - ax
      else
        ! |r> = |r> - alpha |ap>
        r = r - alpha * ap
      endif

      ! res = <r|r>
      res2 = norm2( r, g%hvol, g%comm )

      ! rz_new = <r|r>
      rzn = res2

      ! beta = rz_new / rz_old
      beta = rzn / rzo
      if( rzo < RS_TINY ) then
        beta = 0.
        p = r
      else  ! it%it_restart == 0
      ! |p> = |z> + beta |p>
        p = r + beta * p
      endif ! it%it_restart == 0

!       if(o>0) write(o,'(3A,I5,9(A,ES10.2))') sym, fun, 'it#', it, ' alpha=', alpha, ' beta=', beta, ' res^2=', res2

      ! rz_old = rz_new
      rzo = rzn

      run = ( res2 > threshold2 )  ! residual fell below threshold ?
      run = run .and. ( it < mxit )  ! maximum number of steps exceeded ?
      run = run .or.  ( it < mnit )  ! minimum number of steps not reached ?
      !--------------------------------------
      ! end of the CG iteration
      !--------------------------------------
    enddo ! while( run )

    res = sqrt(res2/g%svol)

    ! show the result
    if(o>0) write(o,'(2A,2(ES9.2,A),I0,9A)') sym, fun, res_start, ' -->', res, ' e/aB^3 in ', it, ' iterations'

  endfunction cg_solve_no_precon



  status_t function cg_solve_precon( x, b, threshold, res, g, &
                       maxiter, miniter, restart ) result( ist )
  use type_grid, only: grid, BC_PERIODIC
  use Laplacian, only: Laplace
  implicit none
    ! parameters
    character(len=*), parameter       :: fun = ' CG solve: '
    real, parameter                   :: RZ_TINY = 1E-14
    real, parameter                   :: RS_TINY = 1E-10
    ! arguments
    real, intent(inout)               :: x(:,:,:)       ! solve A*x = b
    real, intent(in)                  :: b(:,:,:)       ! inhomogeneous part b
    real, intent(in)                  :: threshold      ! threshold
    real, intent(out)                 :: res            ! residual
    type(grid), intent(in)            :: g              ! grid descriptor
    integer, intent(in), optional     :: maxiter        ! maximum number of iterations
    integer, intent(in), optional     :: miniter        ! minium number of iterations
    integer, intent(in), optional     :: restart        ! number of iterations before restart
    ! CG can be switched off, only steepest descent by setting restart=1

    ! local variables
    integer                           :: mxit, mnit ! maximum, minimum iterations
    integer                           :: it_restart, it=0 ! iteration counter
    logical                           :: run !
    real                              :: xnorm
    real                              :: threshold2
    real                              :: alpha, beta, pAp, rzo, rzn, res2
    real                              :: res_start

!     real    :: r (g%ng(1),g%ng(2),g%ng(3))
!     real    :: p (g%ng(1),g%ng(2),g%ng(3))
!     real    :: ax(g%ng(1),g%ng(2),g%ng(3))
!     real    :: ap(g%ng(1),g%ng(2),g%ng(3))
!     real    :: z (g%ng(1),g%ng(2),g%ng(3))
    real, allocatable :: r(:,:,:), p(:,:,:), ax(:,:,:), ap(:,:,:), z(:,:,:)

    allocate( r (g%ng(1),g%ng(2),g%ng(3)), &
              p (g%ng(1),g%ng(2),g%ng(3)), &
              ax(g%ng(1),g%ng(2),g%ng(3)), &
              ap(g%ng(1),g%ng(2),g%ng(3)), &
              z (g%ng(1),g%ng(2),g%ng(3)), stat=ist )
    if( ist /= 0 ) then
      if(o>0) write(o,'(3A,F0.3,9A)') sym, fun, 'failed to allocate ', 5 * product(g%ng(1:3))*8*.5**10, ' kiByte'
      stop 'PSS cg_solve: failed to allocate arrays!'
    endif !


    mxit = CG_MAXITER_HARDLIMIT ; if( present(maxiter) ) mxit = min( maxiter, CG_MAXITER_HARDLIMIT )
    mnit = 0                    ; if( present(miniter) ) mnit = max( 0, miniter )
    it_restart = 2**12          ; if( present(restart) ) it_restart = max(1,restart)

    threshold2 = g%svol * threshold**2
#ifdef DEBUG
    if( g%svol <= 0.0 ) stop 'cg_solve: the cell volume is not positive.'
#endif

    ! |Ax> = A|x>
    ! |r> = |b> - |Ax>
    ! |z> = P|r>
    ! rz_old = <r|z>
    ! |p> = |z>
    ! it = 0
    ! do while
    !   |Ap> = A|p>
    !   pAp = <p|Ap>
    !   alpha = rz_old / pAp
    !   |x> = |x> + alpha |p>
    !   |r> = |r> - alpha |Ap>
    !   res = <r|r>
    !   |z> = P|r>
    !   rz_new = <r|z>
    !   beta = rz_new / rz_old
    !   rz_old = rz_new
    !   |p> = |z> + beta |p>
    !   it = it+1


    ! |Ax> := A|x>
    ist = Laplace( x, ax, g )
    if( ist /= 0 ) stop 'CG_solve: Laplacian failed!'

    ! |r> = |b> - A|x> = |b> - |Ax>
    r = b - ax

    ! res^2 = <r|r>
    res2 = norm2( r, g%hvol, g%comm )
    res = sqrt(res2/g%svol)
    res_start = res ! store staring residual

    ! |z> = |Pr> = P|r>
    ist = Precon_Nf4( g, r, z )
    if( ist /= 0 ) stop 'CG_solve: Precon_Nf4 failed!'

    ! rz_old = <r|z>
    rzo = scalar_product( r, z, g%hvol, g%comm )

    ! |p> = |z>
    p = z

    it = 0 ! init iteration counter

    ! number of iterations is less then maxiter?
    run = ( it < maxiter )
    do while( run )
      ! it = it+1
      it = it + 1
      !--------------------------------------
      ! begin of the CG iteration
      !--------------------------------------

      ! |ap> = A|p>
      ist = Laplace( p, ap, g )
      if( ist /= 0 ) stop 'CG_solve: Laplace failed!'

      pAp = scalar_product( p, ap, g%hvol, g%comm )

      ! alpha = rz_old / pAp
      alpha = rzo / pAp
      if( abs(pAp) < RZ_TINY ) alpha = RZ_TINY

      ! |x> = |x> + alpha |p>
      x = x + alpha * p

      !============================================================
      ! special treatment of completely periodic case
      !============================================================
      if( all( g%bc(1:3,1) == BC_PERIODIC ) ) then
        xnorm = norm1( x, 1.0, g%comm ) ! 1.0 instead of g%hvol
        xnorm = xnorm/real( g%ng_all(1)*g%ng_all(2)*g%ng_all(3) )
        ! subtract the average potential
        x = x - xnorm
      endif ! 3 periodic BCs
      !============================================================

      if( mod( it, it_restart ) == 0 ) then
        ! |Ax> = A|x>
        ist = Laplace( x, ax, g )
        if( ist /= 0 ) stop 'CG_solve: Laplace failed!'
        ! |r> = |b> - A|x> = |b> - |ax>
        r = b - ax
      else
        ! |r> = |r> - alpha |ap>
        r = r - alpha * ap
      endif

      ! res = <r|r>
      res2 = norm2( r, g%hvol, g%comm )

      ! |z> = |Pr> = P|r>
      ist = Precon_Nf4( g, r, z )
      if( ist /= 0 ) stop 'CG_solve: Precon_Nf4 failed!'

      ! rz_new = <r|z>
      rzn = scalar_product( r, z, g%hvol, g%comm )

      ! beta = rz_new / rz_old
      beta = rzn / rzo
      if( rzo < RS_TINY ) then
        beta = 0.
        p = z
      else  ! it%it_restart == 0
      ! |p> = |z> + beta |p>
        p = z + beta * p
      endif ! it%it_restart == 0

!       if(o>0) write(o,'(3A,I5,9(A,ES10.2))') sym, fun, 'it#', it, ' alpha=', alpha, ' beta=', beta, ' res^2=', res2

      ! rz_old = rz_new
      rzo = rzn

      run = ( res2 > threshold2 )  ! residual fell below threshold ?
      run = run .and. ( it < mxit )  ! maximum number of steps exceeded ?
      run = run .or.  ( it < mnit )  ! minimum number of steps not reached ?
      !--------------------------------------
      ! end of the CG iteration
      !--------------------------------------
    enddo ! while( run )

    res = sqrt(res2/g%svol)

    ! show the result
    if(o>0) write(o,'(2A,2(ES10.2,A),I0,9A)') sym, fun, res_start, ' ->', res, ' e/aB^3 in ',it,' iterations'

  endfunction cg_solve_precon




  status_t function precon_Nf4( g, v, Pv ) result( ist ) ! Preconditioner with Nf=4, rank4
  use boundary, only: dataexchange
  use type_grid, only: grid
  implicit none
  ! parameter
  character(len=*), parameter   :: fun = ' precon_Nf4: '
  ! arguments
  type(grid), intent(in)      :: g
  real, intent(in)            :: v (:,:,:)
  real, intent(out)           :: Pv(:,:,:)
  ! local variables
  integer  :: ix, iy, iz, is
  real     :: vec(1-4:g%ng(1)+4,1-4:g%ng(2)+4,1-4:g%ng(3)+4)
  real     :: c(-4:4,-4:4,-4:4), ct

    ! prepare coefficients c
    if( .not. all( g%nh == 4 ) ) stop 'precon_Nf4: all g%nh must be 4.'
    do iz = -4, 4
      do iy = -4, 4
        do ix = -4, 4
          is = ix*ix+iy*iy+iz*iz
          ct = 0. ; if( is <= (4+.5)**2 ) ct = exp(-1.*is)
          c( ix, iy, iz) = ct
        enddo ! ix
      enddo ! iy
    enddo ! iz

    vec = 0. ! init as zero and fill the center region with data
    vec(1:g%ng(1),1:g%ng(2),1:g%ng(3)) = v
    call dataexchange( g, vec )
    ist = 0

    do iz = 1, g%ng(3)
      do iy = 1, g%ng(2)
        do ix = 1, g%ng(1)
          Pv(ix,iy,iz) = sum( vec(ix-4:ix+4,iy-4:iy+4,iz-4:iz+4) * c )
        enddo ! ix
      enddo ! iy
    enddo ! iz

  endfunction precon_Nf4



  status_t function direct_solve_r( x, rho, g ) result( ist )
  use constants, only: Pi
  use type_grid, only: grid
  use type_grid, only: BC_FINITE
  use type_grid, only: BC_PERIODIC
  implicit none
    ! parameters
    character(len=*), parameter       :: fun = ' direct_solve: '
    integer, parameter                :: IDX=1, IDY=2, IDZ=3
    ! arguments
    real, intent(inout)               :: x(:,:,:)       ! solve L*x = rho
    real, intent(in)                  :: rho(:,:,:)     ! inhomogeneous part rho
    type(grid), intent(in)            :: g

    ! local variables
    real                              :: r1(3), r2(3), rr, t, h23
    integer                           :: i1, i2, i3
    integer                           :: j1, j2, j3
    integer                           :: k1, k2, k3
    real                              :: kv(3), green
    integer                           :: Nall, iall
    real                              :: completed, tenth

    Nall = product( g%ng )
    iall = 0
    tenth = 0.1
    completed = 0.0

  if( all( g%bc == BC_FINITE ) ) then
#ifdef DEBUG
    if(o>0) write(o,'(9A)') sym, fun, 'start.'
#endif
    h23 = g%hvol**(2./3.)

    x = 0.
    do j3 = 1, g%ng(3)
    do j2 = 1, g%ng(2)
    do j1 = 1, g%ng(1)
      r2 = (/j1,j2,j3/)*g%h
      t = 0.
      do i3 = 1, g%ng(3)
      do i2 = 1, g%ng(2)
      do i1 = 1, g%ng(1)
        r1 = (/i1,i2,i3/)*g%h
        rr = sum( (r1-r2)**2 ) + h23
        t = t + rho(i1,i2,i3) / sqrt(rr)
      enddo ! i1
      enddo ! i2
      enddo ! i3
      x(j1,j2,j3) = t * g%hvol ! * dV
#ifdef DEBUG
      iall = iall+1
      completed = real(iall)/real(Nall)
      if( completed > tenth ) then
        if(o>0) write(o,'(2A,I3,A)') sym, fun, nint(tenth*100.) ,' %'
        tenth = 0.1 * real( int( completed * 10. ) + 1 )
      endif !
#endif
    enddo ! j1
    enddo ! j2
    enddo ! j3


#ifdef DEBUG
    if(o>0) write(o,'(9A)') sym, fun, 'done.'
#endif

  elseif( all( g%bc == BC_PERIODIC ) ) then ! all BC FINITE
#ifdef DEBUG
    if(o>0) write(o,'(9A)') sym, fun, 'start.'
#endif

    x = 0.
    do j3 = 1, g%ng(3)
    do j2 = 1, g%ng(2)
    do j1 = 1, g%ng(1)
      r2 = (/j1,j2,j3/)*g%h
      t = 0.
      do i3 = 1, g%ng(3)
      do i2 = 1, g%ng(2)
      do i1 = 1, g%ng(1)
        r1 = (/i1,i2,i3/)*g%h
        green = 0.
        do k3 = -g%ng(3)/2, g%ng(3)/2
        do k2 = -g%ng(2)/2, g%ng(2)/2
        do k1 = -g%ng(1)/2, g%ng(1)/2
          if( k1*k1+k2*k2+k3*k3 == 0 ) cycle
          kv = (/k1,k2,k3/) / g%s(1:3)
          green = green + cos( sum( kv*(r1-r2) ) ) / sum( kv*kv )
        enddo ! k1
        enddo ! k2
        enddo ! k3
        t = t + rho(i1,i2,i3) * green
      enddo ! i1
      enddo ! i2
      enddo ! i3
      x(j1,j2,j3) = (4.*Pi) * (2.*Pi)**3 * t
#ifdef DEBUG
      iall = iall+1
      completed = real(iall)/real(Nall)
      if( completed > tenth ) then
        if(o>0) write(o,'(2A,I3,A)') sym, fun, nint(tenth*100.) ,' %'
        tenth = 0.1 * real( int( completed * 10. ) + 1 )
      endif !
#endif
    enddo ! j1
    enddo ! j2
    enddo ! j3

#ifdef DEBUG
    if(o>0) write(o,'(9A)') sym, fun, 'done.'
#endif
  else ! all BC FINITE

    ist = -1 ; return

  endif ! all BC
    ist = 0
  endfunction direct_solve_r




  status_t function fourier_solve_r( x, rho, g ) result( ist )
  use constants, only: Pi
  use type_grid, only: grid
  use type_grid, only: BC_PERIODIC
#ifdef EXTENDED
  use fft_tools, only: fft
#endif
    real, intent(inout)               :: x(:,:,:)       ! solve Laplace*x = rho
    real, intent(in)                  :: rho(:,:,:)     ! inhomogeneous part rho
    type(grid), intent(in)            :: g

    character(len=*), parameter       :: fun = ' fourier_solve: '
    integer, parameter                :: IDX=1, IDY=2, IDZ=3
    integer                           :: k1, k2, k3, nk(3)
    integer                           :: j1, j2, j3
    real                              :: kv(3), kk, f, gv(3)
    complex                           :: tmp_c(g%ng(1),g%ng(2),g%ng(3))
    complex                           :: rho_k(g%ng(1),g%ng(2),g%ng(3))
    complex                           :: x_k(g%ng(1),g%ng(2),g%ng(3))

    if( any( g%bc /= BC_PERIODIC ) ) stop 'direct_solve: all BCs must be periodic.'
    if( any( g%nproc /= 1 ) ) stop 'direct_solve: only for serial runs.'

    gv(1:3) = 2.*Pi/g%s(1:3)
    nk(1:3) = g%ng(1:3)/2



    ! the density is real-valued
    tmp_c = cmplx( rho, 0. )

    ! transform to Fourier space

    rho_k = fft( tmp_c )

    !write(*,*) 'rho_k(0) = ', rho_k(1,1,1)
    rho_k(1,1,1) = 0. ! charge neutrality, clear the k=(/0.,0.,0./)-component

    f = -4.*Pi / real( product( g%ng(1:3) ) )

!     ! for testing, if fft( fft( ... ), inv ) is the unity transformation
!     f = 1. / real( product( g%ng(1:3) ) )

    do j3 = 1, g%ng(3)
      k3 = j3-1
      if( k3 > nk(3) ) k3 = j3-1 - g%ng(3)
      kv(3) = gv(3) * k3
      do j2 = 1, g%ng(2)
        k2 = j2-1
        if( k2 > nk(2) ) k2 = j2-1 - g%ng(2)
        kv(2) = gv(2) * k2
        do j1 = 1, g%ng(1)
          k1 = j1-1
          if( k1 > nk(1) ) k1 = j1-1 - g%ng(1)
          kv(1) = gv(1) * k1

          kk = sum( kv(1:3)**2 ) + 1.E-12 ! safety
          !if(abs(kk) > 1.E-13 ) then 
            
          if(j1 == 1 .and. j2 == 1 .and. j3 == 1) then 
            x_k(j1,j2,j3) = 0.
          else 
            x_k(j1,j2,j3) = rho_k(j1,j2,j3) * f / ( -kk )  ! -4Pi/-k^2
          endif
          !else 
          !  x_k(j1,j2,j3) = 0.
          !endif
!           ! for testing, if fft( fft( ... ), inv=.true. ) is the unity transformation
!           x_k(j1,j2,j3) = rho_k(j1,j2,j3) * f

        enddo ! j1
      enddo ! j2
    enddo ! j3

    ! transform to real space
#ifdef EXTENDED
    tmp_c = fft( x_k, inv=.true. )
#else
    stop 'compile version with FFT module'
#endif

    ! throw away the imaginary part
    x = real( tmp_c )

#ifdef DEBUG
!     if(o>0) write(o,'(9A)') sym, fun, 'done.'
#endif
    ist = 0
  endfunction fourier_solve_r



  status_t function spherical_solve_r( g, rho, v, origin ) result( ist )
  ! uses a bessel transform with j0
  ! to solve the Poisson equation:
  !    Delta V(vec r) = -4Pi rho(vec r)
  ! for a spherically symmetric charge distribution rho(r):
  !    1/r^2 d/dr [ r^2 d/dr V(r) ] = -4Pi rho(r)
  ! in Fourier space:
  !
  !     q^2 V(q) = -4Pi rho(q)
  !
  use constants, only: Pi, Y00 => ONESQRTFOURPI
  use type_grid, only: grid
  use type_grid, only: BC_FINITE
  use MPItools, only: MPIallsum
  implicit none
    ! parameter
    character(len=*), parameter           :: fun = ' spherical_solve: '
#ifdef DEBUG
    integer, parameter                    :: unt = 23
#endif
    ! arguments
    type(grid), intent(in)                :: g
    real, intent(in)                      :: rho(:,:,:) ! augmented charge density
    real, intent(out)                     :: v(:,:,:) ! electrostatic potential
    real, intent(in), optional            :: origin(3)
    ! local vars
    real                  :: hg(3), ori(3)
    real, allocatable     :: fq(:)
    integer               :: ng(3), i1, i2, i3
    real                  :: rv(3), rs, r2s
    integer               :: nr, ie, iq, nq!, ir, ios, ierr
    real                  :: rmax, e2, edges(8), dr!, r
    real                  :: q, dq, qr, sqrt2pi, t!, d, p
#ifdef DEBUG
    real                  :: nrm, ene
#endif

    ori = 0. ; if( present( origin ) ) ori = origin

    hg(1:3) = g%h(1:3)
    if( any( g%bc /= BC_FINITE ) ) stop 'PSS spherical_solve: needs finite BC.'

    ng = shape( rho )
    ! edges
    ie = 0
    do i3 = 0, 1 ; do i2 = 0, 1 ; do i1 = 0, 1
      ie = ie+1
      e2 = sum( ( (1+(/i1,i2,i3/)*(ng(1:3)-1))*hg(1:3) - ori(1:3) )**2 )
      edges(ie) = sqrt( e2 )
    enddo ; enddo ; enddo ! i1 i2 i3

    rmax = maxval(edges) ! largest radius
    dr = product(hg(1:3))**(1./3.)
    nr = rmax/dr
    nq = nr
    dq = pi/rmax

    sqrt2pi = sqrt(2./Pi)

    allocate( fq(0:nq) ) ; fq = 0.

!$omp parallel
!$omp do private(i1,i2,i3,rv,r2s,rs,iq,qr) reduction(+:fq) collapse(2)
    do i3 = 1, ng(3)
      do i2 = 1, ng(2)
        do i1 = 1, ng(1)
          !---------------------------------
          rv(1:3) = [i1, i2, i3]*hg(1:3) - ori(1:3)
          r2s = rv(1)*rv(1) + rv(2)*rv(2) + rv(3)*rv(3)
          rs = sqrt( r2s )
          ! Bessel tranform the function
          do iq = 0, nq
            qr = iq*dq*rs
            fq(iq) = fq(iq) + rho(i1,i2,i3) * bessel_j0( qr )
          enddo ! iq
          v(i1,i2,i3) = rs ! store distance from the origin for later usage
          !---------------------------------
        enddo ! i1
      enddo ! i2
    enddo ! i3
!$omp end do
!$omp barrier
!$omp master

    call MPIallsum( fq, g%comm )
    ! scale with constant prefactors
    fq = fq * ( sqrt2pi * hg(1)*hg(2)*hg(3) * Y00 )

#ifdef DEBUG
!     if(o>0) write(o,'(3A,ES16.6)') sym, fun, 'q=0 component:', fq(0)
    ene = sum( fq(1:nq)**2 )*dq
    if(o>0) write(o,'(3A,9(ES16.6E2,A))') sym, fun, 'Energy', 4*Pi*0.5*ene, ' Ha (in q-space)'
#endif

#ifdef DEBUG
    if( .TRUE. ) then
!     if( present( file ) ) then
      open(unit=unt,file='dmp/sph_Poisson',IOstat=ios)
      if( ios /= 0 .and. o/=0) write(o,'(3A,I2)') sym, fun, 'write to file "dmp/sph_Poisson".'
      nrm = 0.
      ene = 0.
      do ir = 0, nr  * 64  ! sample more r values
        r = ir*dr    / 64. ! on a denser grid
        d = 0.
        p = 0.
        do iq = 1, nq
          q = iq*dq
          qr = q*r
          ! transform back
          d = d + q * q * Bessel_j0( qr ) * fq(iq) ! density
          p = p + 4.*Pi * Bessel_j0( qr ) * fq(iq) ! potential
        enddo ! iq
        d = d * ( sqrt2pi * dq * Y00 )
        p = p * ( sqrt2pi * dq * Y00 )
        nrm = nrm + r*r*(dr/64.)* d
        ene = ene + r*r*(dr/64.)* p*d
        write(unit=unt, fmt='(9ES16.6E2)' ) r, p, d
      enddo ! ir
      close(unit=unt,IOstat=ios)
      if(o>0) write(o,'(3A,9(ES16.6E2,A))') sym, fun, 'Energy', 4*Pi*0.5*ene, ' Ha, density norm', 4*Pi*nrm, ' e'
!       if( ios == 0 .and. o/=0) write(o,'(9A)') sym, fun, 'file "', trim(file), '" written.'
    endif ! present file
#endif

    ! apply the Poisson kernel in q-space (and prefactors)
    do iq = 0, nq
      ! q = iq*dq
      fq(iq) = fq(iq) * 4.*Pi * dq * Y00 * sqrt2pi ! /q**2
    enddo ! iq
    
!$omp end master
!$omp do private(i1,i2,i3,rs,t,iq,q,qr) collapse(2)
    do i3 = 1, ng(3)
      do i2 = 1, ng(2)
        do i1 = 1, ng(1)
          !----------------------------------
          rs = v(i1,i2,i3) ! load distance from the origin
          ! transform back
          t = 0. ! init temporary
          do iq = 1, nq
            q = iq*dq
            qr = q*rs
            t = t + fq(iq) * Bessel_j0( qr ) ! * q**2 * dq * Y00 * sqrt2pi
          enddo ! iq
          v(i1,i2,i3) = t
          !---------------------------------
        enddo ! i1
      enddo ! i2
    enddo ! i3
!$omp end do
!$omp end parallel

    deallocate( fq, stat=ist )

  return
  contains
    ! ell=0 spherical Bessel function
    real function bessel_j0( qr )
    implicit none
      real, intent( in ) :: qr
      bessel_j0 = 1.0
      if( abs( qr ) < 1.E-9 ) return
      bessel_j0 = sin( qr )/qr
    endfunction bessel_j0

  endfunction spherical_solve_r

! end of module head
#endif 

#if R1_C2 == 1
#define REAPLEX real
#else
#define REAPLEX complex
#endif





#if R1_C2 == 1
  REAPLEX function scalar_product_r( &
#else
  REAPLEX function scalar_product_c( &
#endif
                                     a, b, h3, comm ) result( s )
  use MPItools, only: operator(.MPIsum.)
  implicit none
    ! arguments
    REAPLEX, intent(in)            :: a(:,:,:), b(:,:,:)
    real, intent(in)               :: h3 ! product of grid spacings
    integer, intent(in), optional  :: comm

#if R1_C2 == 1
    s = sum( a*b )
#else
    s = sum( conjg(a)*b )
#endif
    if( present( comm ) ) s = s .MPIsum. comm
    s = s * h3

  endfunction ! scalar_product



#if R1_C2 == 1
  real function norm2_r( &
#else
  real function norm2_c( &
#endif
                         a, h3, comm ) result( s )
  use MPItools, only: operator(.MPIsum.)
  implicit none
    ! arguments
    REAPLEX, intent(in)            :: a(:,:,:)
    real, intent(in)               :: h3 ! product of grid spacings
    integer, intent(in), optional  :: comm

#if R1_C2 == 1
    s = sum( a*a )
#else
    s = sum( real( a*conjg(a) ) )
#endif
    if( present( comm ) ) s = s .MPIsum. comm
    s = s * h3

  endfunction ! norm2



#if R1_C2 == 1
  REAPLEX function norm1_r( &
#else
  REAPLEX function norm1_c( &
#endif
                            a, h3, comm ) result( s )
  use MPItools, only: operator(.MPIsum.)
  implicit none
    ! arguments
    REAPLEX, intent(in)            :: a(:,:,:)
    real, intent(in)               :: h3 ! product of grid spacings
    integer, intent(in), optional  :: comm

    s = sum( a )
    if( present( comm ) ) s = s .MPIsum. comm
    s = s * h3

  endfunction ! norm1



#if R1_C2 == 2
! tail of the module

#ifdef EXTENDED
!+ extended

  status_t function test( ) result( ist )
  use configuration, only: o
  use constants, only: Pi
  use type_grid, only: grid, set, BC_PERIODIC
  use Laplacian, only: Laplace
  use communicators, only: comm_init
#ifdef EXTENDED
  use toolbox, only: write_vtk_file
  use fft_tools, only: fft
  use grace, only: grace_plot
#endif
    character(len=*), parameter     :: fun = ' test: '
    integer, parameter  :: WHICH_TEST = 0 ! 1
    integer, parameter  :: n(1:3) = (/400,1,1/) ! odd numbers are good !
    real, parameter     :: s(1:3) = 1.*n ! <0.5 a.u. ==> >19.7 Ry cutoff energy
    real                ::   b(n(1),n(2),n(3))
    real                ::   v(n(1),n(2),n(3))
    real                ::  Lv(n(1),n(2),n(3))
    real                :: rho(n(1),n(2),n(3))
!   complex             ::   c(n(1),n(2),n(3),2)
    type(grid)          :: g
    integer             :: i1, i0!, i2, i3
    real                :: rv(3), kv(3)!, rr, r
    real                :: avg
    integer             :: bc(3,2) = BC_PERIODIC
    real                :: res!, off
    integer             :: ik, inf
    real                :: err(0:n(1),0:4)
    real                :: e(n(1),4)

    g = set( s, n, nspins=1, bc=bc ) ! BC_PERIODIC

    g%comm = comm_init( )

    g%nf = 1 ! finite difference range
    g%nf(2:3) = 0 ! switched off
    g%nh = g%nf


    if( WHICH_TEST == 0 ) then

      do inf = 1, 4

        g%nf(1) = inf ! finite difference range

        g%nh = g%nf ! adjust halo size

        do ik = 0, n(1)/2

          rho = 0.

          kv = 2.*Pi*(/ik,0,0/)/g%s
          ! prepare rho

          do i1 = 1, g%ng(1)
            rv(1) = i1*g%h(1) + g%off(1)
            !---------------------------------
            rho(i1,1,1) = cos( rv(1)*kv(1) )
            !---------------------------------
          enddo ! i1

          ! charge distribution rho prepared
          ! make it charge neutral
          avg = sum( rho )/size( rho )
          rho = rho - avg

          ist = fourier_solve( v, rho, g )

          ! compute the residual of the Poisson eq.
          ! when the discretized Laplacian is applied
          i0 = Laplace( v, Lv, g )

          avg = sum( Lv )/size( Lv )
          Lv = Lv - avg

          b = b - Lv
          res = sqrt( sum( b**2 )/size(b) )
      !     if(o>0) write(o,'(3A,ES16.6E2)') sym, fun, 'residual sqrt(<b^2>) =', res


          err(ik, 0 ) = kv(1)
          err(ik,inf) = res

        enddo ! ik
      enddo ! inf
#ifdef EXTENDED
      ist = grace_plot( file='FD_error_q.xmg', ydata=err(0:n(1)/2,1:4), xdata=err(0:,0) )
#endif
      do ik = 0, n(1)
        write(8,'(99ES16.6E2)') err(ik,0:4)
      enddo ! enddo ik

      stop 'error spectrum written to fort.8'


    endif ! WHICH_TEST == 0


    if( WHICH_TEST == 1 ) then

      do ik = 2, 2

      rho = 0.
  ! 
      kv(1) = 2.*Pi*ik/g%s(1)
      ! prepare rho
      do i1 = 1, g%ng(1)
        rv(1) = i1*g%h(1) + g%off(1)
        !---------------------------------
        rho(i1,1,1) = cos( rv(1)*kv(1) )
      enddo ! i1

      ! charge distribution rho prepared
      ! make it charge neutral
      avg = sum( rho )/size( rho )
      rho = rho - avg


      do inf = 1, 4

        g%nf(1) = inf ! finite difference range

        g%nh = g%nf ! adjust halo size

        ist = fourier_solve( v, rho, g )


        ! compute the residual of the Poisson eq.
        ! when the discretized Laplacian is applied
        i0 = Laplace( v, Lv, g )

  !       avg = sum( Lv )/size( Lv )
  !       Lv = Lv - avg


        b = b - Lv
        res = sqrt( sum( b**2 )/size(b) )
        if(o>0) write(o,'(3A,ES16.6E2)') sym, fun, 'residual sqrt(<b^2>) =', res
  !       err(0,ik) = kv(1)
  !       err(inf,ik) = res

        e(:,inf) = b(:,1,1)

      enddo ! inf

      enddo ! ik

      do i1 = 1, n(1)
        write(8,'(99ES16.6E2)') (i1-0.5)*g%h(1), e(i1,:)
      enddo ! i1
  !     stop 'error(x) for G=1 written to fort.8'
      stop 'error(x) for G=2 written to fort.8'

    endif ! WHICH_TEST == 1

#ifdef EXTENDED
!     ist = write_vtk_file( 'res', b )
#endif

!     off = 0.
!     do i1 = 1, g%ng(1)
!       write(8,'(99ES16.6E2)') (i1-0.5)*g%h(1)+off, v(i1,1,1), rho(i1,1,1), b(i1,1,1)
!     enddo ! i1
!     off = off + g%ng(1)*g%h(1)
!     write(8,'(A)') '#'
!     do i2 = 1, g%ng(2)
!       write(8,'(99ES16.6E2)') (i2-0.5)*g%h(2)+off, v(1,i2,1), rho(1,i2,1), b(1,i2,1)
!     enddo ! i2
!     off = off + g%ng(2)*g%h(2)
!     write(8,'(A)') '#'
!     do i3 = 1, g%ng(3)
!       write(8,'(99ES16.6E2)') (i3-0.5)*g%h(3)+off, v(1,1,i3), rho(1,1,i3), b(1,1,i3)
!     enddo ! i3
!     off = off + g%ng(3)*g%h(3)
!     write(8,'(A)') '#'
!     if(o>0) write(o,'(3A,I1)') sym, fun, 'high symmetry lines written to fort.', 8

  endfunction ! test

!- extended
#endif

endmodule ! Poissonsolver
#endif
#endif

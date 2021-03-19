#include "config.h"

#ifdef DEBUG_ALL
#define DEBUG
#endif

! #define DEBUG
! #define FULL_DEBUG


#ifdef DEBUG_GGA
#define DEBUG
#endif

#ifdef DEBUG
#define cDBG
#endif

#ifndef cDBG
#define cDBG !
#endif

! #define USE_LEBEDEV_GRID

!!! BLAS optimization:
#define BLAS_GEMM dgemm



!! @author Paul Baumeister
!! @version 3.0
!!
!! creates a Legendre grid for the solid angle
!! and provides the transformation Q_ell m <--> Q(theta,phi)
module spherical
#ifdef DEBUG
  use configuration, only: o ! output unit
#endif
#ifdef NaN_SEARCH
  use debugtools, only: NaN_search
#endif
implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: sym = 'SPH' !! module symbol

  public :: init_Gaunt
  public :: Gaunt_tensor
  public :: nPoints
  public :: init_Xlm
  public :: Xlm_transform
  public :: fcc_rotation_matrix
#ifdef EXTENDED
  public :: test
#endif

  interface Xlm_transform
    module procedure transform, transform_deriv, transform_backward_deriv
  endinterface

  real, allocatable, save, protected :: Gaunt_tensor(:,:,:) ! full R-Gaunt tensor

#ifndef DEBUG
  integer, parameter :: o = 0 ! no output
#endif

  integer, parameter :: KEY_GTYP_GAUSS = 0
  integer, parameter :: KEY_GTYP_LEBEDEV = 1

  ! the number of points
  integer, save, protected :: nPoints =  0 !  0: not allocated
  ! the maximum ell for the spherical harmonics
  integer, save            :: lmax = -1 ! -1: not allocated
  ! the type of grid
  integer, save            :: gtyp = KEY_GTYP_GAUSS ! Gauss-Legendre grid

  ! spherical harmonics of the points on the sphere
  real, allocatable, save :: XlmP(:,:) ! (1:(lmax+1)**2,nPoints)
  real, allocatable, save :: PXlm(:,:) ! (nPoints,1:(lmax+1)**2) ! contains weights
  ! cartesian coordinates of the points (and weights in the 0-component)
  real, allocatable, save :: spts(:,:) ! (0:3,nPoints) ! 0: weight

  ! derivatives for GGA
  real, allocatable, save :: dXlmP(:,:) ! (1:(lmax+1)**2,nPoints)
  real, allocatable, save :: d2XlmP(:,:) ! (1:(lmax+1)**2,nPoints)
  real, allocatable, save :: theta(:)   ! (nPoints) ! (used for GGA)

#ifdef DEBUG_GGA
  ! complex
  complex, allocatable, save :: YlmP(:,:) ! (1:(lmax+1)**2,nPoints)
  complex, allocatable, save :: PYlm(:,:) ! (nPoints,1:(lmax+1)**2) ! contains weights
  complex, allocatable, save :: dYlmP(:,:) ! (1:(lmax+1)**2,nPoints)
  complex, allocatable, save :: d2YlmP(:,:) ! (1:(lmax+1)**2,nPoints)
#endif

  contains

  status_t function init_gaunt( nsph, ellmax_prj, ellmax_rho ) result( ist )
  ! set the values of the gaunt_tensor:
  !
  !                                 /
  ! gaunt_tensor(ilm2,ilm1,ilm0) =  |dOmega Y_lm0 * Y_lm1 * Y_lm2
  !                                 /
  !
  ! where Y_lm are real linear combination of the spherical harmonics
  !                                                    PBaum Aug 08

    integer, intent(in)             :: nsph ! order of spherical points distribution
    integer, intent(in)             :: ellmax_prj ! ell-cutoff for partial waves
    integer, intent(in)             :: ellmax_rho ! ell-cutoff for densities in the sphere

    character(len=*), parameter     :: fun = ' init_gaunt: '
#ifdef FULL_DEBUG
    real, allocatable               :: Gell(:,:)
    integer                         :: ell, emm, mlm, ilm, ilm1
#endif

    ist = init_numerical_gaunt( nsph, ellmax_prj, ellmax_rho )

#ifdef NaN_SEARCH
    ist = NaN_search( Gaunt_tensor, fun, text='Gaunt tensor' )
#endif

#ifdef FULL_DEBUG
    ! show the Gaunt_tensor
    open(12,'dmp/gaunt_tensor',iostat=ist)
    write(12,'(A)') '', 'Gaunt_tensor:', ''

    mlm = (ellmax_prj+1)**2
    allocate( Gell(mlm,mlm), stat=ist ) ; if(ist/=0) return

    ilm = 0
    do ell = 0, ellmax_rho
      Gell = 0.
      do emm = -ell, ell
        ilm = ilm+1
        write(12,'(A,I3)') ' ilm=', ilm
        do ilm1 = 1, mlm
          write(12,'(16F10.6)') Gaunt_tensor(1:mlm,ilm1,ilm)
        enddo ! ilm1
        write(12,'(A)') ''
        Gell = Gell + matmul( Gaunt_tensor(1:mlm,1:mlm,ilm), Gaunt_tensor(1:mlm,1:mlm,ilm) )
      enddo ! emm
      write(12,'(A,I3)') ' 4.*Pi*sum( G(:,:,ell_emm)**2 , emm=-ell,ell ), ell=', ell
      do ilm1 = 1, s%mlm
        write(12,'(9F10.6)') 4.*Pi*Gell(1:mlm,ilm1)
      enddo ! ilm1
      write(12,'(A)') '', ''
    enddo ! ell

    deallocate( Gell, stat=ist )
    close(12,iostat=ist)
    if(o>0) write(o,'(9A)') sym, fun, 'DEBUG file "dmp/gaunt_tensor" written.'
#endif
  endfunction ! init_gaunt


  status_t function transform( ri, ro, forward ) result( ist )
  ! transform from the representation of radial mutipoles rm
  ! to the representation on points on a unit sphere rp or
  ! vice versa, if called with forward=.false.
  use configuration, only: WARNING
    real, intent(in)                :: ri(1:,:) ! (1:nr,ni) ! ni: (lmax+1)**2 nPoints
    real, intent(out)               :: ro(1:,:) ! (1:nr,no) ! no: nPoints        (lmax+1)**2
    logical, intent(in)             :: forward  !                 true        false

    character(len=*), parameter     :: fun = ' transform: '
#ifdef BLAS_GEMM
    real, parameter :: ONE = 1., ZERO = 0.
#endif
    ! arguments
    ! module vars
    ! integer                         :: lmax, nPoints
    ! real, allocatable, save         :: XlmP((lmax+1)^2,nPoints)
    ! real, allocatable, save         :: PXlm(nPoints,(lmax+1)^2) ! weighted for the back-tranform
    ! local vars
    integer :: nr, mlm, ilm, ip

#ifdef DEBUG
    ! checks
    if( nPoints <= 0 ) stop 'SPH transform: call init( nsph, ellmax ) before!'
    if( size(ri,1) /= size(ro,1) ) stop 'SPH transform: dim#1 of RI and RO must match.'
#endif

    if( forward ) then
      ! transform from multipoles to points on the sphere

#ifdef DEBUG
      ! checks
      if( size(ro,2) /= nPoints ) stop 'SPH transform: dim#2 of RO must be nPoints.'
      if( (lmax+1)**2 < size(ri,2) .and. o>0) write(o,'(4A,9(I0,A))') sym, fun, WARNING(0), 'sphere grid was initalized for lmax = ',lmax,', but tries to transform ',size(ri,2),' radial functions!'
#endif
      mlm = min( size( ri, 2 ), (lmax+1)**2 )
      ist = min(0, mlm-1)
      if( mlm < 1 ) stop 'SPH transform: no transformation performed, maybe Xlm are not set.'

#ifdef BLAS_GEMM
!     call DGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
      call BLAS_GEMM ('n','n',size(ro,1),nPoints,mlm,ONE,ri,size(ri,1),XlmP,size(XlmP,1),ZERO,ro,size(ro,1))
#else
      ro = 0.
      do ip = 1, nPoints
        do ilm = 1, mlm
          ro(:,ip) = ro(:,ip) + ri(:,ilm) * XlmP(ilm,ip)
        enddo ! ilm
      enddo ! ip
#endif

    else  ! forward
      ! transform from points on the sphere into multipoles
      ! using the weighting of the points [PXlm(0,:)]
#ifdef DEBUG
      ! checks
      if( size(ri,2) /= nPoints ) stop 'SPH transform: dim#2 of RI must be nPoints (back-transform)!'
      if( (lmax+1)**2 < size(ro,2) .and. o>0) write(o,'(4A,9(I0,A))') sym, fun, WARNING(0), 'sphere grid was initalized for lmax = ',lmax,', but tries to back-transform ',size(ro,2),' radial functions!'
#endif
      mlm = min( size( ro, 2 ), (lmax+1)**2 )
      ist = min(0, mlm-1)
      if( mlm < 1 ) stop 'SPH transform: no back-transformation performed, maybe Xlm are not set.'

#ifdef BLAS_GEMM
!     call DGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
      call BLAS_GEMM ('n','n',size(ro,1),mlm,nPoints,ONE,ri,size(ri,1),PXlm,size(PXlm,1),ZERO,ro,size(ro,1))
#else
      ro = 0.
      do ilm = 1, mlm
        do ip = 1, nPoints
          ro(:,ilm) = ro(:,ilm) + ri(:,ip) * PXlm(ip,ilm) ! PXlm contains point weights
        enddo ! ip
      enddo ! ilm
#endif
    endif ! forward
  endfunction ! transform


  status_t function transform_backward_deriv( pr, rm ) result( ist )
  ! transform from the the representation on points on a unit sphere rp
  ! back to the representation of radial mutipoles rm
    real, intent(in)                :: pr(:,1:) ! (nPoints,1:nr) radial function on sphere points
    real, intent(out)               :: rm(1:,:) ! (1:nr,(lmax+1)^2)
#ifdef BLAS_GEMM
    real, parameter :: ONE = 1., ZERO = 0.
#endif
    ! arguments
    ! module vars
    ! integer                         :: lmax, nPoints
    ! real, allocatable, save         :: PXlm(nPoints,(lmax+1)^2) ! weighted
    ! local vars
    integer :: nr, mlm, ilm, ip

#ifdef DEBUG
    if( nPoints <= 0 ) stop 'SPH transform: call init( nsph, ellmax ) before!'
    if( size(pr,1) < nPoints ) stop 'SPH transform: dim#1 of PR must be nPoints.'
    if( size(rm,1) /= size(pr,2) ) stop 'SPH transform: dim#1 of RM and dim#2 of PR must match.'
#endif
    mlm = min( size( rm, 2 ), (lmax+1)**2 )
    ist = min(0, mlm-1)
    if( mlm < 1 ) stop 'SPH transform: no transformation performed, maybe PXlm are not initialized.'

    ! transform from points on the sphere into multipoles
#ifdef BLAS_GEMM
!     call DGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
      call BLAS_GEMM ('t','n',size(rm,1),mlm,nPoints,ONE,pr,size(pr,1),PXlm,size(PXlm,1),ZERO,rm,size(rm,1))
#else
      rm = 0. ! init
      do ilm = 1, mlm
        do ip = 1, nPoints
          rm(:,ilm) = rm(:,ilm) + pr(ip,:) * PXlm(ip,ilm) ! PXlm contains weights
        enddo ! ip
      enddo ! ilm
#endif
  endfunction ! transform_backward_deriv


  status_t function transform_deriv( rm, rp, d, r, drdi, drho ) result( ist )
  use GGA_tools, only: derive_radial_function
  use configuration, only: WARNING
    integer, parameter     :: IAGR=1, GGGR=2, IG2R=3

    real, intent(in)       :: rm(1:,1:) ! (1:nr,(lmax+1)**2) ! density on radial lm-grid
    real, intent(out)      :: rp(1:,1:) ! (nPoints,1:nr) ! density on grid points
    real, intent(in)       :: d, r(0:), drdi(0:) ! radial grid: anisotropy, grid and grid derivative
    real, intent(out)      :: drho(:,1:,1:,IAGR:) ! (nPoints,1:nr,1,IAGR:IG2R) ! abs(grad(rho)

    character(len=*), parameter :: fun = ' transform_deriv: '
    integer :: nr, ilm, mlm, ir, ip, jlm, ell, emm, lmx
    real, allocatable :: Qdr(:,:), Qdrr(:,:), Qp(:,:)
    real :: m
    real :: rd ! density
    real :: rdr, rdt, rdf ! 1st derivatives
    real :: rdrr, rdtt, rdff, rdrt, rdtf, rdrf ! 2nd derivatives
    real :: lpl ! Laplacian
#ifdef DEBUG_GGA
    integer :: fu = 15 ! write to file
#endif
    logical, save :: warn_lmax_limited = .true.

    nr = size(rm,1)
#ifdef DEBUG_GGA
!     if(o>0) write(o,'(3A,2I6,A,9I6)') sym, fun, 'shape(rm)=', shape(rm), ' shape(drho)=', shape(drho)
#endif
    lmx = floor( sqrt( size(rm,2)-.9 ) )
    if( lmax < lmx .and. warn_lmax_limited ) then
      if(o>0) write(o,'(4A,9(I0,A))') sym, fun, WARNING(0), 'densities up to lmax = ',lmx,', but sphere grid is limited to lmax = ',lmax
      warn_lmax_limited = .false. ! switch off the warning after 1st time
    endif ! sphere grid is smaller than densities
    lmx = min( lmx, lmax )
    mlm = (lmx+1)**2
#ifdef DEBUG
!     if( size(rm,2) /= mlm ) stop 'SPH transform_deriv: dim #2 of RM should be the square number (lmx+1)^2!'
    if( size(rp,2) /= nr ) stop 'SPH transform_deriv: dim #2 of RP should be the # of radial grid points!'
    if( size(drho,1) /= nPoints ) stop 'SPH transform_deriv: dim #1 of DRHO should match the # of sphere grid points nPoints!'
    if( size(drho,2) /= nr ) stop 'SPH transform_deriv: dim #2 of DRHO should be the # of radial grid points!'
    if( size(drho,4) /= 3 ) stop 'SPH transform_deriv: dim #4 of DRHO must be 3: [IAGR,GGGR,IG2R]!'
#endif

    rp   = 0. ! init
    drho = 0. ! init
    allocate( Qdr(nr,mlm), Qdrr(nr,mlm), Qp(mlm,0:2), stat=ist ) ! transposed
    if( ist /= 0 ) return ! allocation error 

!$omp parallel

!$omp do private(ilm)
    do ilm = 1, mlm
      ! generate 1st and 2nd radial derivative with order 6 methods
      call derive_radial_function( 2, rm(:,ilm), Qdr(:,ilm), Qdrr(:,ilm), d=d, drdi=drdi(1:) )
#ifdef DEBUG_GGA
      if( ilm == 1 ) write(919,'(4ES24.16)') ( r(ir), rm(ir,ilm), Qdr(ir,ilm), Qdrr(ir,ilm), ir=1,nr )
      if( ilm == 1 ) write(919,'(A)') ''
#endif
    enddo ! ilm
!#omp end do

#ifdef DEBUG_GGA
    open( unit=15, file='dmp/gga_values', iostat=fu ) ; if( fu == 0 ) fu = 15
#endif

    ! loop over shells
    do ir = 1, nr

!$omp sections
!$omp section
      Qp(1:mlm,0) = rm(ir,1:mlm) ! transpose
!$omp section
      Qp(1:mlm,1) = Qdr(ir,1:mlm) ! transpose
!$omp section
      Qp(1:mlm,2) = Qdrr(ir,1:mlm) ! transpose
!$omp end sections


!$omp barrier

!$omp do private(ip,rd,rdr,rdt,rdf,rdrr,rdtt,rdff,rdrt,rdtf,rdrf,lpl,ilm,ell,emm,jlm,m)
      do ip = 1, nPoints
        !========================================================================================================

        ! initialization
        rd = 0.
        rdr = 0.
        rdt = 0.
        rdf = 0.
        rdrr = 0.
        rdtt = 0.
        rdff = 0.
        rdrt = 0.
        rdtf = 0.
        rdrf = 0.
#ifdef DEBUG_GGA
        lpl  = 0. ! laplacian
#endif
        ilm = 0
        do ell = 0, lmx
          do emm = -ell, ell
            ilm = ilm+1
            !
            ! Xlm are real spherical harmonics
            !
            ! for the complex spherical harmonics
            ! Ylm ~exp(i*emm*phi)
            ! ==> d/dphi Ylm = i*emm*Ylm and similarly
            ! ==> d^2/dphi^2 Ylm = -emm^2*Ylm
            !
            ! similarly the real spherical harmonics
            ! Xlm ~sin(phi) or ~cos(phi)
            ! ==> d/dphi Xlm = -emm*Xl(-m) and
            jlm = ilm-2*emm ! =ilm(ell,-emm)
            ! ==> d^2/dphi^2 Xlm = -emm^2*Xlm as above
            !
            ! we thus do not need to compute the derivatives w.r.t. phi
            !
            m = real(emm)
            !-----------------------------------------------

            !----- value --------------------------------
            rd   = rd   +      XlmP(ilm,ip) * Qp(ilm,0)
            !----- 1st derivatives ----------------------
            rdr  = rdr  +      XlmP(ilm,ip) * Qp(ilm,1)
            rdt  = rdt  +     dXlmP(ilm,ip) * Qp(ilm,0)
            rdf  = rdf  - m *  XlmP(jlm,ip) * Qp(ilm,0)
            !----- 2nd derivatives ----------------------
            rdrt = rdrt +     dXlmP(ilm,ip) * Qp(ilm,1)
            rdrf = rdrf - m *  XlmP(jlm,ip) * Qp(ilm,1)
            rdtf = rdtf - m * dXlmP(jlm,ip) * Qp(ilm,0)
            rdff = rdff - m*m *XlmP(ilm,ip) * Qp(ilm,0)
            rdtt = rdtt +    d2XlmP(ilm,ip) * Qp(ilm,0)
            rdrr = rdrr +      XlmP(ilm,ip) * Qp(ilm,2)
            !--------------------------------------------
#ifdef DEBUG_GGA
!             ell = floor( sqrt( ilm-.9 ) )
!             lpl  = lpl + XlmP(ilm,ip) * ( Qdrr(ir,ilm) + 2.* Qdr(ir,ilm)/r(ir) )
            lpl  = lpl + XlmP(ilm,ip) * ( Qp(ilm,2) + 2.* Qp(ilm,1)/r(ir) )
            if( ell > 0 .and. r(ir) > 0. ) &
            lpl  = lpl + XlmP(ilm,ip) * ell*(ell+1.)*rm(ir,ilm)/r(ir)**2
#endif
          enddo ! emm
        enddo ! ell

        rp(ip,ir) = rd ! store density value

        ! convert from radial*Xlm to cartesian coordinates
        drho(ip,ir,1,IAGR:IG2R) = convert_gradients( r(ir), theta(ip), &
                rd, & ! density
                rdr, rdt, rdf, & ! 1st derivatives
                rdrr, rdtt, rdff, rdrt, rdtf, rdrf ) ! 2nd derivatives
#ifdef DEBUG_GGA
        if(fu>0) write(fu,'(F10.6,99ES16.6)') r(ir), rp(ip,ir), drho(ip,ir,1,IAGR:IG2R), lpl
#endif
        !========================================================================================================
      enddo ! ip
!$omp end do
    enddo ! ir
!$omp end parallel

    deallocate( Qdr, Qdrr, Qp, stat=ist )
#ifdef DEBUG_GGA
!     if( fu > 0 ) stop 'mod_spherical.F90:236: dmp/gga_values written! DEBUG'
    if(o>0 .and. fu>0) write(o,'(A)') 'mod_spherical.F90:236: dmp/gga_values written! DEBUG'
#endif
  contains

    function convert_gradients( r, theta, &
      rd, rdr, rdt, rdf, rdrr, rdtt, rdff, rdrt, rdtf, rdrf ) result( agr_gggr_g2r )
!.....------------------------------------------------------------------
!     by use of charge density and its polar coord. gradient components
!c    calculate agr and others used to evaluate gradient
!c    contributions to potential and energy. t.a. 1996.
!.....------------------------------------------------------------------
!     ro=sum(ro*ylh), rdr=sum(drr*ylh), drdr=sum(ddrr*ylh),
!c    rdt=sum(ro*ylht1), rdtt=sum(ro*ylht2), ...
!c    rdf=sum(ro*ylhf1), rdff=sum(ro*ylhf2), ...
!c    rdtf=sum(ro*ylhtf), rdff=sum(ro*ylhf2), ...
!c    rdrt=sum(drr*ylht1),rdrf=sum(drr*ylhf1),

!     agr: abs(grad(ro))
!     g2r: laplacian(ro)
!c    gggr: grad(ro)*grad(agr), WARNING: gggr is bilinear in the density

!     not calculated:
!c    grgru,d: grad(ro)*grad(rou),for rod.,
!c    gzgr: grad(zeta)*grad(ro).

!     dagrr,-t,-f: d(agr)/dr, d(agr)/dth/r, d(agr)/dfi/r/sint.
!.....------------------------------------------------------------------
      real, intent(in)  :: r ! radius
      real, intent(in)  :: theta ! angle
      real, intent(in)  :: rd ! value
      real, intent(in)  :: rdr, rdt, rdf ! 1st derivatives d/dr, d/dtheta, d/dphi
      real, intent(in)  :: rdrr, rdtt, rdff, rdrt, rdtf, rdrf ! 2nd derivatives
      real              :: agr_gggr_g2r(IAGR:IG2R) ! results 

      real, parameter   :: CHSML = 1E-10 ! small charge value
      real              :: dagrf, dagrr, dagrt ! grad|grad rho| in polar coordinates
      real              :: r2, r3, rsint, sint, sint2, tant ! temp

      agr_gggr_g2r  = 0. ; if( r < 1E-14 ) return ! [0.,0.,0.]

      sint  = sin( theta )

      if( sint < 1E-14 ) return ! [0.,0.,0.]

      r2 = r*r ! radius squared
      r3 = r2*r ! radius cubed
      rsint = r*sint

      agr_gggr_g2r(IAGR) = sqrt( rdr**2 + (rdt/r)**2 + (rdf/rsint)**2 ) ! |grad rho| (result#1)

      if( agr_gggr_g2r(IAGR) < CHSML ) return ! [agr,0.,0.] with agr < CHSML

      tant  = tan( theta )
      sint2 = sint*sint

      dagrr = ( rdr*rdrr*r3 + rdt*(rdrt*r-rdt) + rdf*(rdrf*r-rdf)/sint2 ) / ( r3*agr_gggr_g2r(IAGR) )
      dagrt = ( rdr*rdrt*r2 + rdt*rdtt + rdf*(-rdf/tant+rdtf)/sint2 ) / ( r3*agr_gggr_g2r(IAGR) )
      dagrf = ( rdr*rdrf*r2 + rdt*rdtf + rdf*rdff/sint2 ) / ( r3*sint*agr_gggr_g2r(IAGR) )

      agr_gggr_g2r(GGGR) = rdr*dagrr + (rdt/r)*dagrt + (rdf/rsint)*dagrf ! (result#2) ! warning: gggr is bilinear in rho

      agr_gggr_g2r(IG2R) = rdrr + 2.*rdr/r + (rdtt+rdt/tant+rdff/sint2) / r2 ! (result#3)

    endfunction ! convert_gradients

  endfunction ! transform_deriv


  status_t function init_Xlm( nsph, ellmax ) result( ist )
  use harmonics, only: ELLMAX_IMPLEMENTED
  use configuration, only: WARNING
  use gga_tools, only: polar_angles
#ifdef USE_LEBEDEV_GRID
  use LebedevLaikov, only: create_Lebedev_grid
  use constants, only: Pi
#endif
    integer, intent(in)             :: nsph ! order of spherical points distribution
    integer, intent(in)             :: ellmax ! ell-cutoff for spherical harmonics

    character(len=*), parameter     :: fun = ' init_Xlm: '
    integer :: ip
    real    :: phi
#ifdef DEBUG_GGA
    integer :: ilm1, ilm2, lmx
    real :: ovl(99)
    complex :: ovlc(99)
#endif
    ! module vars
    ! integer                         :: lmax, nPoints
    ! real, allocatable, save         :: spts(:,:), XlmP(:,:), PXlm(:,:), dXlmP(:,:), d2XlmP(:,:)
    ! ... and many more

    ! checks
    if( nsph < 1 ) stop 'SPH init_Xlm init: order n must be positive.'

    lmax = min( max( 0, ellmax ), ELLMAX_IMPLEMENTED )
#ifdef DEBUG_GGA
    if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'lmax = ', lmax,  ' nsph = ', nsph, '  (GGA_DEBUG)'
#endif

    if( allocated(spts) ) deallocate( spts, stat=ist  )

#ifdef USE_LEBEDEV_GRID
    ist = create_Lebedev_grid( lmax, spts )
    nPoints = size( spts, 2 )
    spts(0,:) = spts(0,:)*4*Pi ! adjust weights
#else
    nPoints = generate_GaussLegendre_points( nsph, p=spts )
#endif

    if( nsph < lmax+1 .and. o>0) write(o,'(4A,9(I0,A))') sym, fun, WARNING(0), &
         'order n is ', nsph, ' which means only ', nPoints, ' points on the unit sphere.'

    ist = gen_Xlm( p=spts, ellmax=lmax )

#ifdef NaN_SEARCH
    ist = NaN_search( XlmP, fun, text='XlmP, basis of real spherical harmonics' )
    ist = NaN_search( PXlm, fun, text='PXlm, basis of real spherical harmonics' )
#endif
    ! prepare data for GGA
    deallocate( dXlmP, d2XlmP, theta, stat=ist )
    allocate( theta(nPoints), dXlmP((lmax+1)**2,nPoints), d2XlmP((lmax+1)**2,nPoints), stat=ist )

    do ip = 1, nPoints
      call polar_angles( spts(1:3,ip), theta(ip), phi )
      ist = gen_dXlm( lmax, spts(1:3,ip), dXlmP(:,ip), d2XlmP(:,ip) )
#ifdef DEBUG_GGA
! cDBG  write(8,'(A,999F5.2)') 'XlmP', XlmP(:,ip) ! show the Xlm to compare to a transformation of Ylm to Xlm
#endif
    enddo ! ip



  endfunction ! init_Xlm



  status_t function g_rule( n, x, w ) result( ist )
  ! determines a distribution of vales and weights for cos(theta)
  ! to sample the intgration over the azimuthal angle theta
  !
  ! this subroutine was taken from the FLEUR project
  !
  use constants, only: Pi
    integer, intent(in)       :: n    ! number of points to distribute along the azimuthal angle theta
    real, intent(out)         :: x(n) ! values of cos(theta)
    real, intent(out)         :: w(n) ! weights of each point

    real                      :: d1, d2pn, d3pn, d4pn, den, dp, dpn
    real                      :: e1, fx, h, pk, pkm1, pkp1, t, t1, u, v, x0
    real                      :: p
    integer                   :: i, it, k, m

    m = int( (n+1)/2 )
    e1 = n*(n+1)
    do i = 1, m
      t = (4*i-1)*Pi/real(4*n+2)
      x0 = (1.- (1.-1./n)/ (8.*n*n))*cos(t)
      ! determination of the weight
      do it = 1, 2
        pkm1 = 1.
        pk = x0
        do k= 2, n
          t1 = x0*pk
          pkp1 = t1 - pkm1 - (t1-pkm1)/k + t1
          pkm1 = pk
          pk = pkp1
        enddo ! k
        den = 1. - x0**2
        d1 = n* (pkm1-x0*pk)
        dpn = d1/den
        d2pn = (2.*x0*dpn-e1*pk)/den
        d3pn = (4.*x0*d2pn+ (2.-e1)*dpn)/den
        d4pn = (6.*x0*d3pn+ (6.-e1)*d2pn)/den
        u = pk/dpn
        v = d2pn/dpn
        h = -u* (1.+0.5*u* (v+u* (v*v-u*d3pn/ (3.*dpn))))
        p = pk + h* (dpn+0.5*h* (d2pn+h/3.* (d3pn+0.25*h*d4pn)))
        dp = dpn + h* (d2pn+0.5*h* (d3pn+h*d4pn/3.))
        h = h - p/dp
        x0 = x0 + h
      enddo ! it
      x(i) = x0
      fx = d1 - h*e1* (pk+0.5*h* (dpn+h/3.* (d2pn+0.25*h* (d3pn+0.2*h*d4pn))))
      w(i) = 2.*( 1.-x(i)**2 )/fx**2
    enddo ! i

    ! if n is odd, the middle value x(m) should be theta=pi/2, so cos(theta) is exactly 0.
    if( 2*m > n ) x(m) = 0.
    ist = 0
  endfunction ! g_rule






  status_t function init_numerical_gaunt( nsph, ellmax_prj, ellmax_rho ) result( ist )
  ! sets the gaunt_tensor by numerical integration
  ! To yield high accuracy, choose a high order for nsph e.g. 36
    integer, intent(in)             :: nsph ! order of spherical points distribution
    integer, intent(in)             :: ellmax_prj ! ell-cutoff for spherical harmonics
    integer, intent(in)             :: ellmax_rho ! ell-cutoff for spherical harmonics

!   ist = init_Xlm( nsph=nsph, ellmax=ellmax_rho )
    ist = numerical_R_gaunt( nsph, ellmax_prj, ellmax_rho &
#ifdef FULL_DEBUG
                            , outfilename='dmp/gaunt' )
#else
                            )
#endif
  endfunction ! init_numerical_gaunt





  integer function generate_GaussLegendre_points( nsph, p ) result( np )
  ! (=np) number of points generated
  ! generates a set of points on the unit sphere
  ! which allowes accurate numerical integration
  use constants, only: Pi
    integer, intent(in)             :: nsph  ! order of integration cartesian coordinates and weights of the points on the sphere
    real, allocatable, intent(out)  :: p(:,:) ! (0:3,np) ! p(0,:) contains the weights, p(1:3,:) are the coordinates

    character(len=*), parameter :: fun = ' GaussLegendre_points: '
    integer                     :: m, i, j, ij, isgn
    status_t                    :: ist
    real                        :: cos_theta( nsph ) ! distributed values of cos(theta)
    real                        :: weights( nsph ) ! weights
    real                        :: sin_theta
    real                        :: phi
    real                        :: wj, hw

    ! number of points: np = 2 * nsph^2 
    m = int( (nsph+1)/2 )
    np = 2 * nsph**2
    ! weights and cartesian coordinates for the points on the sphere
    allocate( p(0:3,np) )

    ! find the distribution of cos(theta) values and weights
    ist = g_rule( nsph, cos_theta, weights )

    wj = Pi/real(nsph) ! = pi/n
    hw = 0.5 * wj

    ij = 0 ! counter for points
    do j = 1, 2*nsph
      ! exp( i pi * (2j-1)/(2n) )
      phi = real(2*j-1)*hw

      do i = 1, int(nsph/2.)
        ! on the lower and upper hemisphere
        do isgn = -1, 1, 2
          ij = ij + 1
          sin_theta = sqrt( 1. - cos_theta(i)**2 )
          p(0,ij)   = weights(i) * wj ! weight
          p(1,ij)   = sin_theta * cos(phi)         ! x = sin(theta) cos(phi)
          p(2,ij)   = sin_theta * sin(phi)         ! y = sin(theta) sin(phi)
          p(3,ij)   = cos_theta(i) * isgn ! +1/-1  ! z = cos(theta)
        enddo ! isgn
      enddo ! i

      ! if the azimuthal angle is divided into an odd number,
      ! one value will hit the equator, i.e. cos(theta) = 0.
      if( 2*m > nsph ) then
        ! always, if nsph is odd
        i = m
        ij = ij + 1
        p(0,ij)   = weights(i) * wj ! weight
        p(1,ij)   = 1. * cos(phi)         ! x = sin(theta) cos(phi)
        p(2,ij)   = 1. * sin(phi)         ! y = sin(theta) sin(phi)
        p(3,ij)   = 0.                    ! z = cos(theta)
      endif ! 2*m > n

    enddo ! j
    ! ij has now the value of the highest index so it should by =nsp
#ifdef DEBUG
    if(o>0) write(o,'(3A,6I6)') sym, fun, 'nsph, nsph/2, m, ij, np  ', nsph, nsph/2, m, ij, np
#endif
    if( ij /= np ) stop 'GaussLegendre_points: fatal error, counting wrong.'

    ! the return value is the number of points generated
  endfunction ! generate_GaussLegendre_points


  !! allocate XlmP and PXlm and set the real spherical harmonics of each point p on the sphere
  status_t function gen_Xlm( ellmax, p ) result( ist )
  ! use, only: XlmP(1:(lmax+1)**2,nPoints), PXlm(nPoints,1:(lmax+1)**2)
  use harmonics, only: Xlmax_rl
    integer, intent(in) :: ellmax   ! ell-cutoff for spherical harmonics
    real, intent(in)    :: p(0:,1:) ! (0:3,np) weights and vectors of the points

    character(len=*), parameter :: fun = ' genXlm: '
    real, parameter :: THRESHOLD = 1E-12 ! threshold for deviation of the vector norm from 1.0
    integer :: np, ip
    real :: v(3), w

    if( ellmax < 0 ) stop 'genyXlm: ELLMAX < 0 is an invalid input.'

    np = size(p,2)
    !
    deallocate( XlmP, PXlm, stat=ist )
    allocate( XlmP((ellmax+1)**2,np), PXlm(np,(ellmax+1)**2), stat=ist )
    if( ist /= 0 ) stop 'genyXlm: allocation of XlmP and PXlm failed.'

    do ip = 1, np
      ! spherical harmonics at this point
      v = p(1:3,ip)
      w = p( 0 ,ip) ! the weight of the point
! #ifdef DEBUG
      v = v/sqrt( sum( v**2 ) ) ! renormalize
! #endif
      XlmP(:,ip) = Xlmax_rl( ellmax, v )
      PXlm(ip,:) = XlmP(:,ip) * w ! transposed and weighted
    enddo ! ip
  endfunction ! gen_Xlm






  status_t function numerical_R_gaunt( nsph, ellmax_prj, ellmax_rho, g, ind, outfilename ) result( ist )
  !
  ! computes the Gaunt coefficients numerically by
  ! integration over the points on the unit sphere
  !
  !                 /
  ! G(lm,lm1,lm2) = |dOmega Y_lm Y_lm1 Y_lm2
  !                 /
  !
  ! Y_lm are real-valued linear combination of spherical harmonics
  !
  use constants, only: Pi
  use harmonics, only: Xlmax_rl
#ifdef USE_LEBEDEV_GRID
  use LebedevLaikov, only: create_Lebedev_grid
#endif
  ! use, only: gaunt_tensor(:,:,:)
    integer, intent(in)                   :: nsph
    integer, intent(in)                   :: ellmax_rho ! --> lm
    integer, intent(in)                   :: ellmax_prj ! --> lm1, lm2
    real, allocatable, optional, intent(inout)        :: g(:)      ! (nnz) list of nonzero G-coeff
    integer, allocatable, optional, intent(inout)     :: ind(:,:)  ! (0:2,nnz) index list for g(:)
    character(len=*), intent(in), optional            :: outfilename

    character(len=*), parameter           :: fun = ' numerical_R_gaunt: '
    integer                               :: me 
    integer                               :: ii, ip, ilm, ilm1, ilm2
    real                                  :: sm
    integer, allocatable                  :: indx(:,:) ! (0:2,me)  !
    integer                               :: ie   ! element counter ie = 1, ME
    integer                               :: nnz  ! element counter for nonzeros
    integer                               :: unt = 11
    integer                               :: l0, m0, l1, m1, l2, m2
    logical                               :: nonzero
    integer                               :: ellmax, npt
    real, allocatable                     :: pts(:,:), Xlm(:,:)

    ellmax = max(1,ellmax_prj,ellmax_rho)

#ifdef USE_LEBEDEV_GRID
    ist = create_Lebedev_grid( ellmax, pts )
    npt = size( pts, 2 )
    pts(0,:) = pts(0,:)*4*Pi ! adjust weights
#else
    npt = generate_GaussLegendre_points( nsph=2*ellmax, p=pts )
#endif

    allocate( Xlm(npt,(ellmax+1)**2) )
    do ip = 1, npt
      Xlm(ip,:) = Xlmax_rl( ellmax, v=pts(1:3,ip) )
    enddo ! ip

    me = (ellmax_prj+1)**4*(ellmax_rho+1)**2
    if( allocated(gaunt_tensor) ) deallocate(gaunt_tensor)
    allocate( gaunt_tensor((ellmax_prj+1)**2,(ellmax_prj+1)**2,(ellmax_rho+1)**2) )
    gaunt_tensor = 0.
    allocate( indx(0:2,me) )

#ifdef DEBUG
    ! check weights of the spherical integration
    sm = sum( pts(0,1:npt) )
    if(o>0) then
      write(o,'(3A,F16.10)') sym, fun,      'sum(weights) =', sm
      write(o,'(3A,F16.10)') sym, fun,      '         4Pi =', 4.*Pi
      write(o,'(3A,ES14.4E2,A2)') sym, fun, '         dev =', 100.*(sm-4.*Pi)/(4.*Pi), ' %'
    endif ! o/=0
#endif

    if( present(outfilename) ) then
      open(unit=unt,file=outfilename,action='write',status='replace',iostat=ist)
      if( ist /= 0 ) unt = 6 
    endif ! present(outfilename)

    ie = 0
    nnz = 0

    ilm = 0
    do l0 = 0, ellmax_rho
      do m0 = -l0, l0
        ilm = ilm + 1

        ilm1 = 0
        do l1 = 0, ellmax_prj
          do m1 = -l1, l1
            ilm1 = ilm1 + 1

            ilm2 = 0
            do l2 = 0, ellmax_prj
              do m2 = -l2, l2
                ilm2 = ilm2 + 1
      !===========================================
      if( ilm2 >= ilm1 ) then

        sm = 0.
        nonzero = .true.
!         ! analytical considerations, why this ceoff is 0.
!         nonzero = ( modulo( l0+l1+l2, 2 ) == 0 )
!         nonzero = nonzero .and. ( l0 <= l1+l2 )
!         nonzero = nonzero .and. ( l0 >= abs(l1-l2) )
        if( nonzero ) then
          ie = ie + 1

          sm = 0.
          do ip = 1, npt
!             sm = sm + yylm(ilm,ip)*yylm(ilm1,ip)*yylm(ilm2,ip)*spts(0,ip)
            sm = sm + Xlm(ip,ilm)*Xlm(ip,ilm1)*Xlm(ip,ilm2)*pts(0,ip)
          enddo ! ip

          if( abs(sm) > 1.E-12 ) then
            nnz = nnz + 1 ! one more nonzero element

            if( present(outfilename) ) then
              write(unt,'(3(A,I2),A,F24.16,A,F24.16)') &
                '(', ilm, ') = (', ilm1, ')*(', ilm2, '): ', sm, '  4Pi g^2 =', 4.*Pi*sm*sm
            endif ! present(outfilename)

            indx(0,nnz) = ilm
            indx(1,nnz) = ilm1
            indx(2,nnz) = ilm2
            gaunt_tensor(ilm2,ilm1,ilm) = sm
            ! symmetric
            gaunt_tensor(ilm1,ilm2,ilm) = gaunt_tensor(ilm2,ilm1,ilm)

          endif ! |sm| > threshold
        else ! nonzero
          gaunt_tensor(ilm2,ilm1,ilm) = 0.
          gaunt_tensor(ilm1,ilm2,ilm) = 0.
        endif ! nonzero

      endif ! ilm2 >= ilm1
      !===========================================
              enddo ! m2
            enddo ! l2
          enddo ! m1
        enddo ! l1
      enddo ! m0
    enddo ! l0

    if( present(outfilename) ) then
      close(unit=unt,iostat=ist)
    endif ! present(outfilename)

#ifdef DEBUG
    if(o>0) write(o,'(2A,I4,A,I6,A,F6.2,A)') sym, fun, &
      nnz, ' of', ie, ' (', 100.*nnz/real(ie), ' %) elements are nonzero.'
#endif
!    if( ie /= ME ) stop 'numerical_R_gaunt: error in counting.'


    if( present( g ) ) then
      if( present( ind ) ) then
        if( allocated(g) ) deallocate( g, stat=ist )
        if( allocated(ind) ) deallocate( ind, stat=ist )
        allocate( g(1:nnz), ind(0:2,1:nnz), stat=ist )
        if( ist == 0 ) then
          do ii = 1, nnz
            ind(0:2,ii) = indx(0:2,ii)
            g(ii) = gaunt_tensor( ind(1,ii), ind(2,ii), ind(0,ii) )
          enddo ! ii
        else ! ist == 0
          stop 'numerical_R_gaunt: error in allocation of the optional arrays G and IND.'
        endif ! ist == 0
      else  ! present ind
        stop 'numerical_R_gaunt: optional arrays G given, but missing optional array IND.'
      endif ! present ind
    endif ! present g

    deallocate( indx, stat=ist )
  endfunction ! numerical_R_gaunt


  status_t function fcc_rotation_matrix( rotation ) result( ist )
  use harmonics, only: Xlmax_rl, ELLMAX_IMPLEMENTED
    real, intent(out), optional :: rotation(:,:,:)

    character(len=*), parameter :: fun = ' fcc_rotation_matrix: '
    integer, parameter :: ELLMAX = 3

  integer, parameter :: MROT6(3,3,6) = reshape( (/ &
                  1, 0, 0,   0, 1, 0,   0, 0, 1  ,&
                  0, 0, 1,   1, 0, 0,   0, 1, 0  ,&
                  0, 1, 0,   0, 0, 1,   1, 0, 0  ,&
                  0, 0, 1,   0, 1, 0,   1, 0, 0  ,&
                  0, 1, 0,   1, 0, 0,   0, 0, 1  ,&
                  1, 0, 0,   0, 0, 1,   0, 1, 0   &
          /), (/3,3,6/) ) ! reshape to 6 3x3 matrices

  integer, parameter :: MMIR8(3,3,8) = reshape( (/ &
                  1, 0, 0,   0, 1, 0,   0, 0, 1  ,&
                 -1, 0, 0,   0, 1, 0,   0, 0, 1  ,&
                  1, 0, 0,   0,-1, 0,   0, 0, 1  ,&
                 -1, 0, 0,   0,-1, 0,   0, 0, 1  ,&
                  1, 0, 0,   0, 1, 0,   0, 0,-1  ,&
                 -1, 0, 0,   0, 1, 0,   0, 0,-1  ,&
                  1, 0, 0,   0,-1, 0,   0, 0,-1  ,&
                 -1, 0, 0,   0,-1, 0,   0, 0,-1   &
          /), (/3,3,8/) ) ! reshape to 8 3x3 matrices
    integer, parameter :: T2G_EG(5) = (/5,7,6,8,9/)

    integer                   :: ipts, ilm1, ilm2, mlm, iop, i8, i6, u
    real                      :: s, rv(3)
    real, allocatable         :: rXlm(:,:) !(0:(ELLMAX+1)**2,nPoints)
    real                      :: rot((ELLMAX+1)**2,(ELLMAX+1)**2,48)
    real                      :: rrt((ELLMAX+1)**2,(ELLMAX+1)**2)
    real                      :: mat(3,3)

    u = 0
    if( .not. present(rotation) ) then
      ist = init_Xlm( nsph=12, ellmax=ELLMAX )
      u = 6
    endif !

    mlm = (min(lmax,ELLMAX)+1)**2

    ! check orthogonality and normalization of real spherical harmonics
    do ilm1 = 1, mlm
      do ilm2 = 1, mlm
        s = dot_product( XlmP(ilm2,1:nPoints), PXlm(1:nPoints,ilm1) ) ! PXlm contains weights
        if( ilm1 == ilm2 ) then
          if( abs(s-1.) > 1.E-13 .and. u>0 ) write(u,'(A,I3,ES10.2E2)') '<Y1|Y1> : ilm1 =', ilm2, s
        else
          if( abs(s) > 1.E-13 .and. u>0 ) write(u,'(A,2I3,ES10.2E2)') '<Y1|Y2> : ilm1, ilm2=', ilm1, ilm2, s
        endif ! ilm1 == ilm2
      enddo ! ilm2
    enddo ! ilm1

    allocate( rXlm(0:(ELLMAX+1)**2,nPoints) )

    iop = 0
    do i8 = 1, 8
      do i6 = 1, 6
        iop = iop+1
        mat = matmul( MROT6(:,:,i6), MMIR8(:,:,i8) )

      do ipts = 1, nPoints
        ! space vector p(1:3,ipts)
        rXlm(              0,ipts) = spts(0,ipts) ! weight of the point
        ! rotate vector
        if( abs( sqrt(sum(spts(1:3,ipts)**2)) - 1.0 ) > 1.E-6 ) stop 'SPX test_rotation_matrix: original vector not normalized'
!         rv(1:3) = matmul( MROT48(:,:,iop), spts(1:3,ipts) )
        rv(1:3) = matmul( mat, spts(1:3,ipts) )
        if( abs( sqrt(sum(rv**2)) - 1.0 ) > 1.E-6 ) stop 'SPX test_rotation_matrix: rotated vector not normalized'
        ! spherical harmonics at this point at the rotated vector
        rXlm(1:(ELLMAX+1)**2,ipts) = Xlmax_rl( ellmax=ELLMAX, v=rv )
      enddo ! ipts

      if(u>0) write(u,'(/,99i4)') i8, i6, iop
      do ilm1 = 1, mlm
        do ilm2 = 1, mlm
          s = 0.
          do ipts = 1, nPoints
!             s = s + rylm(ilm2,ipts)*yylm(ilm1,ipts)*spts(0,ipts) ! 0:weights
            s = s + rXlm(ilm2,ipts)*PXlm(ipts,ilm1) ! PXlm contains weights
          enddo ! ipts
          rot(ilm2,ilm1,iop) = s
        enddo ! ilm2
!         if(u>0) write(u,'(99F10.6)') rot(:,ilm1,iop)
      enddo ! ilm1
      ! only d-rotation
      if(u>0) write(u,'(9A)') '------------------------------'
!       do ilm1 = 5, 9
!         if(u>0) write(u,'(99F10.6)') rot(5:9,ilm1,iop)
!         if(u>0) write(u,'(9A)') short_d( rot(5:9,ilm1,iop) )
      do ilm1 = 1, 5
        if(u>0) write(u,'(9A)') short_d( rot(T2G_EG,T2G_EG(ilm1),iop) )
      enddo ! ilm1
      if(u>0) write(u,'(9A)') '------------------------------'

!       if(u>0) write(u,'(9I3)') nint( mat )

      enddo ! i6
    enddo ! i8

    if( present( rotation ) ) then
      rotation = 0.
      if( any( shape(rotation) < (/mlm,mlm,48/) ) ) stop 'fcc_rotation_matrix: outut array too small'
      rotation(1:mlm,1:mlm,1:48) = rot
      return
    endif ! present

!     do iop = 1, 48
!       rrt = matmul( rot(:,:,iop), transpose( rot(:,:,iop) ) )
! !       if(u>0) write(u,'(I6)') iop
! !       if(u>0) write(u,'(16F7.2)') rrt
!       if(u>0) write(u,'(I6,A,9I6)') iop, '  1,0:', count(abs(rrt-1.0)<1.E-12), count(abs(rrt)<1.E-12)
!     enddo ! iop

  endfunction ! fcc_rotation_matrix

  character(len=5) elemental function short_d( r ) result( s )
    real, intent(in) :: r
    selectcase( nint( 4*r*r ) )
    case( 0 )    ; s = '     '
    case( 1 )    ; s = '  1/2'   ! 0.5
    case( 3 )    ; s = '  s34'   ! 0.866
    case( 4 )    ; s = '  1  '   ! 1.0
    case default ; s = '  ?  '
    endselect ! [4r^2]
    if( r < -.1 )  s(2:2) = '-'
  endfunction ! short




  function transform_Ylm2Xlm( ell, Ylm ) result( Xlm )
  use constants, only: sqh => SQRTHALF
    complex, parameter  :: im = (0.,1.)
    integer, intent(in) :: ell
    complex, intent(in) :: Ylm(-ell:ell)
    real                :: Xlm(-ell:ell) ! result
    integer :: m
    complex :: cp, cm
    real    :: sgn

    Xlm(0) = real( Ylm(0) )
    sgn = -1.
    do m = 1, ell
      cp = 1. ; cm = sgn ! cm = (-1.)**m
      Xlm(-m) = real( cp * Ylm(-m) + cm * Ylm( m) )*sqh
      sgn = -sgn ! sgn = -(-1.)**m
      cp = im*sgn ; cm = im
      Xlm( m) = real( cp * Ylm( m) + cm * Ylm(-m) )*sqh
    enddo ! m
  endfunction ! transform_Ylm2Xlm


  status_t function gen_dXlm( ellmax, v, dXlm, d2Xlm ) result( ist )
  use harmonics, only: Ylmax_rl ! complex spherical harmonics
    integer, intent(in)     :: ellmax
    real, intent(in)        :: v(3)
    real, intent(out)       :: dXlm((ellmax+1)**2)
    real, intent(out)       :: d2Xlm((ellmax+1)**2)

    real, parameter         :: SMALL = 1E-12
    integer :: ell, emm, ilm, il0
    complex :: ylm((ellmax+1)**2)
    real    :: cph, sph, xy
    ! for 1st derivative
    complex :: dylm((ellmax+1)**2)
    integer :: lmmm0, lmmp1, lpmm0, lpmp1
    complex :: em1f, ep1f
    ! for 2nd derivative
    complex :: d2ylm((ellmax+1)**2)
    integer :: lmmm1, lmmp2, lpmm1, lpmp2
    complex :: em2f, ep2f

cDBG  real    :: Xlm((ellmax+1)**2)

    ! calculate sin and cos of phi
    xy = v(1)*v(1) + v(2)*v(2)
    if( xy > SMALL**2 ) then
      xy = sqrt(xy)
      cph = v(1)/xy
      sph = v(2)/xy
    else
      cph = 1.0
      sph = 0.0
    endif

    Ylm = Ylmax_rl( ellmax, v=v ) ! eval complex spherical harmonics for this vector v

    ! evaluate phase factors
    ep1f = cmplx(cph,sph)
    em1f = conjg(ep1f)
    ep2f = ep1f*ep1f
    em2f = em1f*em1f

    do ell = 0, ellmax
      il0 = ell*(ell+1)+1 ! ilm index of the central emm==0

      do emm = -ell, ell
        ilm = il0 + emm

        ! first derivative
        lmmm0 = ell - emm - 0
        lmmp1 = ell - emm + 1
        lpmm0 = ell + emm - 0
        lpmp1 = ell + emm + 1
!         write(*,'(2I2,A,9I4)') ell, emm, ' 1st', lmmm0, lmmp1, lpmm0, lpmp1

        dylm(ilm) = 0. ! init
        if( 1+emm <= ell ) dylm(ilm) = dylm(ilm) + 0.5  * isqrt( lmmm0 * lpmp1 ) * ylm(ilm+1) * em1f
        if( 1-emm <= ell ) dylm(ilm) = dylm(ilm) - 0.5  * isqrt( lpmm0 * lmmp1 ) * ylm(ilm-1) * ep1f

        ! second derivative
        d2ylm(ilm) = - 0.25 * ( lmmm0 * lpmp1 + lpmm0 * lmmp1 ) * ylm(ilm)
        lmmm1 = ell - emm - 1
        lmmp2 = ell - emm + 2
        lpmm1 = ell + emm - 1
        lpmp2 = ell + emm + 2
!         write(*,'(2I2,A,9I4)') ell, emm, ' 2nd', lmmm1, lmmp2, lpmm1, lpmp2

        if( 2+emm <= ell ) d2ylm(ilm) = d2ylm(ilm) + 0.25 * isqrt( lmmm1 * lmmm0 * lpmp1 * lpmp2 ) * ylm(ilm+2) * em2f
        if( 2-emm <= ell ) d2ylm(ilm) = d2ylm(ilm) + 0.25 * isqrt( lpmm1 * lpmm0 * lmmp1 * lmmp2 ) * ylm(ilm-2) * ep2f

      enddo ! emm

cDBG    Xlm(il0-ell:il0+ell) = transform_Ylm2Xlm( ell,   Ylm(il0-ell:il0+ell) )
       dXlm(il0-ell:il0+ell) = transform_Ylm2Xlm( ell,  dYlm(il0-ell:il0+ell) )
      d2Xlm(il0-ell:il0+ell) = transform_Ylm2Xlm( ell, d2Ylm(il0-ell:il0+ell) )

    enddo ! ell
#ifdef DEBUG_GGA
! cDBG  write(8,'(A,999F5.2)') 'XlmP', Xlm ! show this set of Xlm to compare to origial Xlm
#endif
    ist = 0
  contains

    real function isqrt( i )
      integer, intent(in) :: i
      isqrt = sqrt( real( i ) )
    endfunction isqrt

  endfunction ! gen_dXlm



#ifdef EXTENDED
!+ extended

  status_t function test( ) result( ist )
#ifdef DEBUG
      ist = -1
!     ist = get_c2r_matrix(  )
!     ist = fcc_rotation_matrix(  )
#else
#ifdef DEBUG_GGA
    ist = test_product_of_4_Xlm( )
#else
    write(*,*,iostat=ist) __FILE__,' tests available if compiled with -D DEBUG or -D DEBUG_GGA'
#endif
#endif
  endfunction ! test

!- extended
#endif
endmodule ! spherical

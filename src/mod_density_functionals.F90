#include "config.h"

! #define DEBUG

#ifdef DEBUG_GGA
#define DEBUG
#endif


!! @author Paul Baumeister, Andrea Nobile
!! @version 3.0
!!
!! collection of density functionals
module density_functionals
  use type_item, only: item
  use configuration, only: o
implicit none
  private ! default for the module namespace
  character(len=*), parameter, private :: fun = ': ', sym = 'density functionals' !! module symbol

  public :: xc_potential
  public :: is_gradient_functional
  public :: show_available_functionals
#ifdef EXTENDED
  public :: test
#endif

  ! keys
  integer, parameter :: K_NONE   = 1
  integer, parameter :: K_XALPHA = 2
  integer, parameter :: K_VWN80  = 3
  integer, parameter :: K_PZ81   = 4
  integer, parameter :: K_PZ_81  = 5 ! different implementation
  integer, parameter :: K_GRADIENT_FUNCTIONALS = 10
  integer, parameter :: K_PBE96  = 11

 
  ! mapping words <--> keys for the input and output
  type(item), parameter, public :: Dictionary(10) = (/ &
    item('PBE',K_PBE96),  item('PerdewBurkeErnzerhof',K_PBE96), &
    item('VWN',K_VWN80),  item('VoskoWilkNusair',K_VWN80), &
    item('PZ', K_PZ81 ), item('PerdewZunger', K_PZ81 ), item('PZ81', K_PZ81 ), &
    item('PZ_81',K_PZ_81), &
    item('Xalpha',K_XALPHA), &
    item('none',K_NONE) /)

  ! Vxc and Exc are 0.0, if the density is less than this
  real, parameter :: TINYDEN = 1E-20

  logical, parameter :: relativistic_corrections = .true. !! MacDonald correction

  contains

  logical function is_gradient_functional( key ) result( is )
    integer, intent(in)  :: key
    is = ( key >= K_GRADIENT_FUNCTIONALS )
  endfunction ! is_gradient_functional

  !! invokes the computation of vxc and exc from rho
  status_t function xc_potential( rho, vxc, exc, key, drho ) result( ist )
  use configuration, only: WARNING, ERROR
  use configuration, only: o
  use type_item, only: operator(.in.)
    real, intent(in)                  :: rho(:,:,:,:) ! dim#4 is the spin
    real, intent(out)                 :: vxc(:,:,:,:) ! dim#4 is the spin
    real, intent(out)                 :: exc(:,:,:)
    integer, intent(in)               :: key

#ifdef DEBUG
    character(len=*), parameter       :: fun = ' xc_potential: '
#endif
    real, intent(in), optional        :: drho(:,:,:,:,:)
    ! local vars
    integer                           :: nsi, nso
    logical, save                     :: warn(3,3) = .true.
    
#ifdef DEBUG
    if( any( rho < 0. ) ) then
      if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'rho has ',count(rho<0.),' of ',size(rho),' negative entries.'
    endif ! rho < 0.
#endif

    nsi = size(rho,4)
    nso = size(vxc,4)
    selectcase( 10 * nsi + nso  )
    case( 11, 22, 33 ) ! ok, no warning
    case( 12, 13 ) ! warn
      if( warn(nsi,nso) ) then
        if(o>0) write(o,'(4A,9(I0,A))') sym, fun, WARNING(0), 'convert from input spin dimension ',nsi,' to output spin dim ',nso
        warn(nsi,nso) = .false. ! do not warn about this conversion again
      endif ! need to warn?
    case( 21, 31, 23, 32 ) ! comment
      if( warn(nsi,nso) ) then
        if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'convert from input spin dimension ',nsi,' to output spin dim ',nso
        warn(nsi,nso) = .false. ! do not warn about this conversion again
      endif ! need to comment?
    case default
      if(o>0) write(*,'(4A,9(I0,A))') sym, fun, ERROR, 'spin dimensions are out of allowed range [1,3], input is ',nsi,' and output is ',nso
      stop 'DFT xc_potential: input or output spin dimension are out of allowed range [1,3]!'
    endselect ! spin dimensions

    selectcase( key )
    case( K_PZ81 )  ; ist = lda_PZ81( rho, vxc, exc ) ! Perdew Zunger 1981 LDA or LSDA
    case( K_XALPHA ); ist = Xalpha( rho, vxc, exc )
    case( K_VWN80 ) ; ist = lda_VWN( rho, vxc, exc )
    case( K_PZ_81 ) ; ist = lda_PZ_81( rho, vxc, exc ) ! Perdew Zunger 1981 LDA (new implementation)
    case( K_PBE96 )
      if( .not. present( drho ) ) stop 'XC: GGA: no density derivatives given!'
      ist = gga_PBE( rho, drho, vxc, exc )
    case( K_NONE  ) ; vxc = 0. ; exc = 0. ; if(o>0) write(o,'(9A)') sym, fun, 'Warning: Exc and Vxc set to zero'
    case default
      if(o>0) write(*,'(4A,I0,9A)') sym, fun, ERROR, 'no such functional (unknown key value: ',key,' )!'
      stop 'DFT xc_potential: no such functional (unknown key).'
    endselect ! k

  endfunction ! xc_potential


  status_t function gga_pbe( rho, drho, vxc, exc ) result( ist )
  use GGA_tools, only: easypbe
  use configuration, only: o, WARNING
  ! calculates the Generalized gradient approximation
  implicit none
    ! parameters
    character(len=*), parameter     :: fun = ' gga_PBE: '
    real, parameter                 :: HARTREE = 1.
    integer, parameter              :: IAGR = 1
    integer, parameter              :: GGGR = 2
    integer, parameter              :: IG2R = 3
    integer, parameter              :: dn = -1
    integer, parameter              :: tt =  0
    integer, parameter              :: up = +1
    real, parameter                 :: SMALL = 1E-14
    ! arguments
    real, intent(in)            :: rho (:,:,:,1:) ! (1:{1,2,3})
    real, intent(in)            :: drho(:,:,:,IAGR:,dn:) ! (IAGR:IG2R,dn:up)
    real, intent(out)           :: Vxc (:,:,:,1:) ! (1:ns)
    real, intent(out)           :: Exc (:,:,:)    ! scalar

    ! local vars
    integer :: nsr, nsv, i1, i2, i3, id, ng(3)
    real    :: ro(dn:up), agl(IAGR:IG2R,dn:up)
    real    :: ex_LSD, vx_LSD(dn:up), ec_LSD, vc_LSD(dn:up)
    real    :: ex_PBE, vx_PBE(dn:up), ec_PBE, vc_PBE(dn:up)

    ist = 0
    nsr = size(rho,4) ! input
    nsv = size(vxc,4) ! output
#ifdef DEBUG
    selectcase( size(rho,4) )
    case( 1:3 ) ! 0k
    case default ; stop 'DFT size(rho,4) must be in {1,2,3}'
    endselect ! size(rho,4)
    if( ubound(drho,5) /= up ) stop 'DFT: dim #5 of DRHO must be 3: (dn,total,up)!'
    if( ubound(drho,4) /= IG2R ) stop 'DFT: dim #4 of DRHO must be 3: (IAGR,GGGR,IG2R)!'
#endif

    do id = 1, 3
      ng(id) = size(rho,id)
#ifdef DEBUG
      if( size(Vxc,id) /= ng(id) ) stop 'gga_PBE: a dim of array VXC does not match RHO.'
      if( size(Exc,id) /= ng(id) ) stop 'gga_PBE: a dim of array EXC does not match RHO.'
#endif
    enddo ! id

!$omp parallel do collapse(3) private(i3,i2,i1,ro,agl,ex_PBE,ec_PBE,vx_PBE,vc_PBE,ex_LSD,ec_LSD,vx_LSD,vc_LSD)
    do i3 = 1, ng(3)
      do i2 = 1, ng(2)
        do i1 = 1, ng(1)

          ! assign densities
          selectcase( nsr )
          case( 3 ) ! rho = {dn,tt,up} = {1,2,3}
            ro(up) = rho(i1,i2,i3,3) ! load up-density
            ro(dn) = rho(i1,i2,i3,1) ! load dn-density
            ro(tt) = ro(up) + ro(dn)  ! total density = up-density + dn-density
#ifdef DEBUG
!           if( abs(ro(tt) - rho(i1,i2,i3,2)) > 1E-13*ro(tt) ) stop 'DFT pbe: total density inaccurate!'
#endif
          case( 2 ) ! rho = {up,dn} = {1,2}
            ro(up) = rho(i1,i2,i3,1) ! load up-density
            ro(dn) = rho(i1,i2,i3,2) ! load dn-density
            ro(tt) = ro(up) + ro(dn)  ! total density = up-density + dn-density
          case( 1 ) ! rho = {tt} = {1}
            ro(tt) = rho(i1,i2,i3,1) ! load total density = up-density + dn-density
            ro(up) = ro(tt)/2. ! make up-density
            ro(dn) = ro(tt)/2. ! make dn-density
          endselect ! size(rho,4)

          if( ro(tt) > SMALL ) then
            ! assign drhos

            agl = drho(i1,i2,i3,IAGR:IG2R,dn:up)

            call easypbe( 7, ro(up), agl(IAGR,up), agl(GGGR,up), agl(IG2R,up), &
                             ro(dn), agl(IAGR,dn), agl(GGGR,dn), agl(IG2R,dn), &
                                     agl(IAGR,tt), agl(GGGR,tt), 1, 1, &
              ex_LSD, vx_LSD(up), vx_LSD(dn), ec_LSD, vc_LSD(up), vc_LSD(dn), &
              ex_PBE, vx_PBE(up), vx_PBE(dn), ec_PBE, vc_PBE(up), vc_PBE(dn) )

            exc(i1,i2,i3) = ex_PBE + ec_PBE

            selectcase( nsv )
            case( 1 ) ! vxc = {tt} = {1}
              vxc(i1,i2,i3,1) = 0.5*( vx_PBE(up) + vc_PBE(up) + vx_PBE(dn) + vc_PBE(dn) ) ! average
            case( 2 ) ! vxc = {up,dn} = {1,2}
              vxc(i1,i2,i3,1) = vx_PBE(up) + vc_PBE(up)
              vxc(i1,i2,i3,2) = vx_PBE(dn) + vc_PBE(dn)
            case( 3 ) ! vxc = {dn,tt,up} = {1,2,3}
              vxc(i1,i2,i3,1) = vx_PBE(dn) + vc_PBE(dn)
              vxc(i1,i2,i3,2) = 0.5*( vx_PBE(up) + vc_PBE(up) + vx_PBE(dn) + vc_PBE(dn) ) ! average
              vxc(i1,i2,i3,3) = vx_PBE(up) + vc_PBE(up)
            case default ! stop 'PBE: dim#4 of Vxc must be in {[1,]2,3}!'
            endselect ! size(vxc,4)

          else  ! total density > SMALL
            vxc(i1,i2,i3,:) = 0.
            exc(i1,i2,i3) = 0.
          endif ! total density > SMALL

        enddo ! i1
      enddo ! i2
    enddo ! i3
!$omp end parallel do

#ifdef GGA_DEBUG
    write(99,'(A,9I6)') '# gga_pbe: shape(rho)=', shape(rho)
    do i3 = 1, size(rho,3)
      do i2 = 1, size(rho,2)
        do i1 = 1, size(rho,1)
          write(99,'(9ES16.6)') rho(i1,i2,i3,1), vxc(i1,i2,i3,1), exc(i1,i2,i3)
        enddo ! i1
        write(99,'(A)') ''
      enddo ! i2
      write(99,'(A)') ''
    enddo ! i3
#endif
  endfunction ! gga_pbe



  status_t function lda_PZ81( rho, Vxc, Exc ) result( ist ) ! LDA or LSDA
  use constants, only: Pi
  use configuration, only: o, WARNING
  implicit none
    ! parameters
    character(len=*), parameter :: fun = 'lda_PZ81: '
    real, parameter       :: THIRD = 1.0/3.0
    real, parameter       :: THIRD4 = 4.0/3.0

    ! arguments
    real, intent(in)            :: rho(:,:,:,:)
    real, intent(out)           :: Vxc(:,:,:,:)
    real, intent(out)           :: Exc(:,:,:)

    ! local vars
    integer      :: iz, iy, ix, ng(3), id
    integer      :: nsr, nsv
    real         :: Vup, Vdn, Vtt

    ist = 0
    nsr = size(rho,4) ! input
    nsv = size(vxc,4) ! output
! !     write(*,*) fun, 'rho: ', nsr, ' vxc: ', nsv

    do id = 1, 3
      ng(id) = size(rho,id)
#ifdef DEBUG
      if( size(Vxc,id) /= ng(id) ) stop 'lda_PZ81: a dim of array VXC does not match RHO.'
      if( size(Exc,id) /= ng(id) ) stop 'lda_PZ81: a dim of array EXC does not match RHO.'
#endif
    enddo ! id

    selectcase( nsr )
    case( 1 ) !! LDA part

#ifdef DEBUG
      if( nsv > nsr ) stop 'DFT PZ81: expect more output spins than input!'
#endif

! !$omp parallel do collapse(3) private(ix,iy,iz,ppp,rs,exc,srs,lrs)

!$omp parallel do collapse(3) private(ix,iy,iz,Vtt)
      do iz = 1, ng(3)
        do iy = 1, ng(2)
          do ix = 1, ng(1)

            Exc(ix,iy,iz) = lda_PZ81_kernel( rho(ix,iy,iz,1), Vtt )
            Vxc(ix,iy,iz,:) = Vtt

          enddo ! ix
        enddo ! iy
      enddo ! iz
!$omp end parallel do

    case( 2:3 ) ! LSDA part

!$omp parallel do collapse(3) private(ix,iy,iz,Vup,Vdn)
      do iz = 1, ng(3)
        do iy = 1, ng(2)
          do ix = 1, ng(1)

            Exc(ix,iy,iz) = lda_PZ81_kernel( rho=rho(ix,iy,iz,1)+rho(ix,iy,iz,nsr), Vup=Vup, mag=rho(ix,iy,iz,1)-rho(ix,iy,iz,nsr), Vdn=Vdn )
            selectcase( nsv )
            case( 1 ) ; Vxc(ix,iy,iz,1) = 0.5*( Vup + Vdn )
            case( 2 ) ; Vxc(ix,iy,iz,1) = Vup ; Vxc(ix,iy,iz,2) = Vdn
            case( 3 ) ; Vxc(ix,iy,iz,1) = Vup ; Vxc(ix,iy,iz,3) = Vdn ; Vxc(ix,iy,iz,2) = 0.5*( Vup + Vdn )
            case default ; stop 'DFT PZ81: dim#4 of Vxc must be one of 2 or 3 [or 1]!'
            endselect ! size(vx,4)

          enddo ! ix
        enddo ! iy
      enddo ! iz
!$omp end parallel do

    case( 4:5 ) ; stop 'DFT lda_PZ81: non-collinear not implemented' ; stop
    case default !! no such case
      if(o>0) write(o,'(9A)') sym, fun, 'rho(:,:,:,NS). NS should be {1:LDA, 2..3:LSDA, 4..5:noco-LSDA}.'
      if(o>0) stop 'DFT lda_PZ81: dim #4 of RHO is neither 1 (LDA) nor 2 (LSDA), nor 4 (noco-LSDA).' ; stop
    endselect ! nsr

#ifdef DEBUG
    ist = searchforNaN( vxc, name='exchange correlation potential' )
#endif
  endfunction ! lda_PZ81


  real function lda_PZ81_kernel( rho, Vup, mag, Vdn ) result( Exc ) ! LSDA
  use constants, only: Pi
  implicit none
    ! parameters
    real, parameter       :: THIRD = 1.0/3.0
    real, parameter       :: THIRD4 = 4.0/3.0
    ! arguments
    real, intent(in)            :: rho
    real, intent(out)           :: Vup
    real, intent(in), optional  :: mag
    real, intent(out), optional :: Vdn

    ! local vars
    real :: rs, srs, lrs ! LDA part
    real :: xi, fxi, dfxi, drs, eexp, eexf, deexp, deexf ! LSDA part
    real :: eecp, eecf, deecp, deecf
    real :: eexcp, eexcf, deexcp, deexcf, deexc, dexcxi

    if( .not. present( mag ) ) then ! LDA

      if( rho < TINYDEN ) then
        Vup = 0.
        Exc = 0.
      else  ! negligible
        rs=(3.0/(4.0*pi*rho))**THIRD
        if( rs >= 1.0 ) then
          srs = sqrt(rs)
          Exc=-0.1423/(1.0+1.0529*srs+0.3334*rs)
          Vup=Exc-rs/3.0*(0.1423*(0.3334+0.52645/srs)/(1.0+1.0529*srs+0.3334*rs)**2.0)
        else
          lrs = log(rs)
          Exc=-0.048+0.0311*lrs-0.0116*rs+0.002*rs*lrs
          Vup=Exc-rs/3.0*(0.0311/rs-0.0096+0.002*lrs)
        end if
        Exc=Exc-0.75*(2.25/pi**2)**THIRD/rs
        Vup=Vup-0.75*(2.25/pi**2)**THIRD/rs- rs/3.0*(0.75*(2.25/pi**2)**THIRD/rs**2.0)
      endif ! negligible

    else ! Magnetization

      if( rho < TINYDEN ) then
        Vup = 0.
        Vdn = 0.
        Exc = 0.
      else  ! negligible
        xi=mag/rho
        xi = min( max( -1., xi ), 1. )
        fxi= ((1.0+xi)**THIRD4+(1.0-xi)**THIRD4-2.0)/(2.0*(2.0**THIRD-1.0))
        dfxi=(2.0*(1.0+xi)**THIRD-2.0*(1.0-xi)**THIRD)/(2.0**THIRD-1.0)/3.0
        rs=(3.0/(4.0*pi*rho))**THIRD
        drs=-(0.75/pi)**THIRD/3.0*rho**(-THIRD4)
        eexp=-0.75*(2.25/pi**2)**THIRD/rs
        eexf=-0.75*(4.50/pi**2)**THIRD/rs
        deexp=0.75*(2.25/pi**2)**THIRD/rs**2
        deexf=0.75*(4.50/pi**2)**THIRD/rs**2
        if( rs > 1.0 ) then
          srs = sqrt(rs)
          eecp=-0.1423/(1.0+1.0529*srs+0.3334*rs)
          eecf=-0.0843/(1.0+1.3981*srs+0.2611*rs)
          deecp=0.1423*(0.3334+0.52645/srs)/(1.0+1.0529*srs+0.3334*rs)**2
          deecf=0.0843*(0.2611+0.69905/srs)/(1.0+1.3981*srs+0.2611*rs)**2
        else ! rs > 1.0
          lrs = log(rs)
          eecp=0.03110*lrs-0.0480+0.0020*rs*lrs-0.0116*rs
          eecf=0.01555*lrs-0.0269+0.0007*rs*lrs-0.0048*rs
          deecp=0.03110/rs+0.0020*lrs-0.0096
          deecf=0.01555/rs+0.0007*lrs-0.0041
        endif ! rs > 1.0
        eexcp=eexp+eecp
        eexcf=eexf+eecf
        deexcp=deexp+deecp
        deexcf=deexf+deecf
!         if( deexcp /= deexcp ) stop 'PZ81 produced NaN (deexcp)'
!         if( deexcf /= deexcf ) stop 'PZ81 produced NaN (deexcf)'
!         if( fxi /= fxi ) stop 'PZ81 produced NaN (fxi)'
        deexc=deexcp+(deexcf-deexcp)*fxi
        dexcxi=(eexcf-eexcp)*dfxi
        Exc = eexcp+(eexcf-eexcp)*fxi
!         if( deexc /= deexc ) stop 'PZ81 produced NaN (deexc)'
!         if( dexcxi /= dexcxi ) stop 'PZ81 produced NaN (dexcxi)'
!         Vx(ix,iy,iz,1) = Ex(ix,iy,iz)+ppp*deexc*drs+dexcxi*(1.0-xi)
!         Vx(ix,iy,iz,2) = Ex(ix,iy,iz)+ppp*deexc*drs-dexcxi*(1.0+xi)
        Vup = Exc+rho*deexc*drs+dexcxi*(1.-xi)
        Vdn = Exc+rho*deexc*drs-dexcxi*(1.+xi)
      endif ! negligible
    endif ! Magnetization

  endfunction ! lda_PZ81_kernel


  status_t function Xalpha( rho, Vx, Ex ) result( ist ) ! only exchange
  use constants, only: Pi
  implicit none
    ! parameters
    character(len=*), parameter     :: fun = ' Xalpha: '
    real, parameter                 :: THIRD = 1.0/3.0
    ! arguments
    real, intent(in)                :: rho(:,:,:,:)
    real, intent(out)               :: Vx(:,:,:,:)
    real, intent(out)               :: Ex(:,:,:)
    ! local vars
    integer      :: iz, iy, ix
    real         :: den, relc(0:1), evxc(0:1) ! 0:Exc, 1:Vxc
    logical      :: spin

    ist = 0
    spin = ( size(rho,4) == 2 )

    do iz = 1, size(rho,3)
      do iy = 1, size(rho,2)
        do ix = 1, size(rho,1)

          den = rho(ix,iy,iz,1)
          if( spin ) den = den + rho(ix,iy,iz,2)

          if( den > TINYDEN ) then

            if( relativistic_corrections ) then
              relc = relcor( den )
            else  
              relc = 1.0
            endif 

            evxc(0:1) = - relc(0:1) * ( den * 3./Pi )**THIRD

          else ! den > TINYDEN
            evxc = 0.0
          endif ! den > TINYDEN
          Vx(ix,iy,iz,1) = evxc(1)
          if( spin ) Vx(ix,iy,iz,2) = evxc(1)
          Ex(ix,iy,iz)   = 0.75 * evxc(0)
        enddo ! ix
      enddo ! iy
    enddo ! iz

  endfunction ! Xalpha

  function relcor( rho )
  !************************************************************************
  !
  ! calculate the relativistic  corrections for exchange as formulated by
  !              A.H. MacDonald and S.H. Vosko, J. Phys. C12, 2977 (1979)
  !
  !************************************************************************
  implicit none
    ! parameters
    character(len=*), parameter     :: fun = ' relcor: '
    real, parameter                 :: betac = 2.2576918e-2  ! alpha*(3pi)^(1/3)
    ! arguments
    real, intent(in)                :: rho
    ! result
    real                            :: relcor(0:1) ! 0:exc, 1:vxc
    ! local vars
    real                            :: beta, beta2, eta, xi, lnxi, betaeta

    beta = betac * rho**(1./3.)
    beta2 = beta*beta

    if( beta < 1.0E-3 ) then
      ! power series expansion
      ! 1 - 2/3 b^2 + 2/5 b^4
      relcor(0) = 1.0 - 2.0/3.0*beta2 + 0.4*beta2*beta2
      ! 1 - b^2 + 3/4 b^3 - 9/20 b^4
      relcor(1) = 1.0 + beta2 * ( -1.0 + 0.75*beta - 0.45*beta2 )

    else  ! beta <<

      eta = sqrt( 1.0 + beta2 )
      xi = beta + eta
      lnxi = log(xi)
      betaeta = beta*eta
      relcor(0) = 1.0 - 1.5*( (betaeta-lnxi)/beta2 )**2
      relcor(1) = -0.5 + 1.5*( lnxi/betaeta )

    endif ! beta <<

  endfunction ! relcor


  status_t function lda_vwn( den, vxc, exc ) result( ist )
  use configuration, only: ERROR
  use constants, only: Pi
  implicit none
    ! parameter
    character(len=*), parameter       :: fun = ' lda_VWN: '

!     real, parameter :: cex = 0.91633058742  ! 3/2 * ( 3/(2*pi) )^(2/3)
!     real, parameter :: fothrd = four * thrd
!     real, parameter :: AP = 0.0621814 , XP0 = -0.10498
!     real, parameter :: AF = 0.0310907 , XF0 = -0.32500
!     real, parameter :: AL =-0.03377373, XL0 = -0.0047584
!     real, parameter :: BP = 3.72744   , CP  = 12.9352
!     real, parameter :: BF = 7.06042   , CF  = 18.0578
!     real, parameter :: BL = 1.13107   , CL  = 13.0045
!     real, parameter :: QF = 4.7309269 , QL = 7.123109
!     real, parameter :: FDD0 = 1.70992093

    real, parameter :: HARTREE = 0.5
    real, parameter :: FOURPITHIRD = 4.*Pi/3.
    real, parameter :: THREEOVERPI = 3./Pi

    real, parameter :: AP =  0.0621814
    real, parameter :: BP =  3.72744
    real, parameter :: CP = 12.9352
    real, parameter :: QP =  6.1519908
    real, parameter :: XP0 =-0.10498
    real, parameter :: XPX0 = XP0*XP0 + BP*XP0 + CP

    ! arguments
    real, intent(in)  :: den(:,:,:,:)   ! charge density
    real, intent(out) :: vxc(:,:,:,:)   ! xc potential
    real, intent(out) :: exc(:,:,:)     ! xc energy
    ! local vars
    integer      :: iz, iy, ix, ng(3), id
    real         :: rho, ec, vc, decdr, x, xpx, relc(0:1)

    ist = 0
    do id= 1, 3
      ng(id) = size(den,id)
#ifdef DEBUG
      if( size(vxc,id) /= ng(id) ) stop 'lda_PZ81: a dim of array VXC does not match RHO.'
      if( size(exc,id) /= ng(id) ) stop 'lda_PZ81: a dim of array EXC does not match RHO.'
#endif
    enddo ! id

#ifdef DEBUG
    if( size(vxc,4) /= size(den,4) ) stop 'lda_PZ81: spin dim of VXC does not match RHO.'
#endif


    if( size(den,4) == 1 ) then
      ! LDA part


      do iz = 1, ng(3)
        do iy = 1, ng(2)
          do ix = 1, ng(1)
            rho = den(ix,iy,iz,1)
            if( rho > TINYDEN ) then

!  rho = max(1.E-15,den(ix,iy,iz,1))
!  rs  = ( 3/(4 Pi rho) )**(1./3.)
!  x   = sqrt(rs)
!  x = ( 3/(4 Pi rho) )**(1./6.)
              x     = (FOURPITHIRD*rho)**(-1./6.)
              xpx   = x*x + BP*x + CP

              ec    = fec( AP, x, xpx, XP0, xpx0, BP, QP )
              decdr = fdedr( rho, AP, x, xpx, XP0, BP, CP )
              vc    = ec + rho * decdr

              if( relativistic_corrections ) then
                relc = relcor( rho )
              else  
                relc = 1.0
              endif 

              Exc(ix,iy,iz)   = HARTREE * ec - relc(0)*0.75*( THREEOVERPI*rho )**(1./3.)
              Vxc(ix,iy,iz,1) = HARTREE * vc - relc(1)*( THREEOVERPI*rho )**(1./3.)
            else  ! rho > TINYDEN
              Exc(ix,iy,iz)   = 0.0
              Vxc(ix,iy,iz,1) = 0.0
            endif ! rho > TINYDEN
          enddo ! ix
        enddo ! iy
      enddo ! iz

    elseif( size(den,4) == 2 ) then
      ! LSDA part

      write(*,'(9A)') sym, fun, ERROR, 'VWN spin = 2 not implemented'
      stop 'VWN spin = 2 not implemented'

      do iz = 1, ng(3)
        do iy = 1, ng(2)
          do ix = 1, ng(1)
            rho = den(ix,iy,iz,1) + den(ix,iy,iz,2)
            if( rho > TINYDEN ) then
              Exc(ix,iy,iz)  = 0.0
              Vxc(ix,iy,iz,1)= 0.0
              Vxc(ix,iy,iz,2)= 0.0
            else  ! rho > TINYDEN
              Exc(ix,iy,iz)  = 0.0
              Vxc(ix,iy,iz,1)= 0.0
              Vxc(ix,iy,iz,2)= 0.0
            endif ! rho > TINYDEN
          enddo ! ix
        enddo ! iy
      enddo ! iz

    else ! size(den,4) == 1

      write(*,'(9A)') sym, fun, ERROR, 'rho(:,:,:,nspin). nspin should be 1 for LDA, 2 for LSDA'
      stop 'DFT lda_VWN: dim # 4 of RHO is neither 1 (LDA) nor 2 (LSDA).'

    endif ! size(rho,4) == 1

#ifdef DEBUG
    ist = searchforNaN( vxc, name='exchange correlation potential' )
#endif

  contains

    real function fec( ai, z, ziz, zi0, ziz0, bi, qi )
      real, intent(in) :: ai,z,ziz,zi0,ziz0,bi,qi
      fec = ai* ( log(z*z/ziz)+ 2.*bi/qi * atan(qi/(2.*z+bi)) - &
              bi*zi0/ziz0* ( log((z-zi0)**2/ziz) + &
              2.*(bi+ 2.*zi0)/qi*atan(qi/ (2.*z+bi))) )
    endfunction ! fec

    real function fdedr( rho, ai, z, ziz, zi0, bi, ci )
      real, intent(in) :: rho,ai,z,ziz,zi0,bi,ci
      fdedr = -ai*z/(3.*rho*ziz)*(ci/z-bi*zi0/(z-zi0))
    endfunction ! fdedr

  endfunction ! lda_vwn


  status_t function lda_pz_81( den, vxc, exc ) result( ist )
  use configuration, only: ERROR
  use constants, only: Pi
  implicit none
    ! parameter
    character(len=*), parameter       :: fun = ' lda_pz81: '

    real, parameter :: HARTREE = 0.5
    real, parameter :: FOURPITHIRD = 4.*Pi/3.
#ifndef KEI
    real, parameter :: THREEOVER2PI23 = (1.5/Pi)**(2./3.)
#else
    real, parameter :: THREEOVER2PI23 = 0.61088705771085716
#endif
    real, parameter :: AP  =  0.0622
    real, parameter :: BP  = -0.0960
    real, parameter :: CP  =  0.0040
    real, parameter :: DP  = -0.0232
    real, parameter :: GP  = -0.2846
    real, parameter :: B1P =  1.0529
    real, parameter :: B2P =  0.3334

    ! arguments
    real, intent(in)  :: den(:,:,:,:)   ! charge density
    real, intent(out) :: vxc(:,:,:,:)   ! xc potential
    real, intent(out) :: exc(:,:,:)     ! xc energy
    ! local vars
    integer      :: iz, iy, ix, ng(3), id
    real         :: rho, relc(0:1), rs, vx, ecp, vcp

    ist = 0
    do id= 1, 3
      ng(id) = size(den,id)
#ifdef DEBUG
      if( size(vxc,id) /= ng(id) ) stop 'lda_PZ81: a dim of array VXC does not match RHO.'
      if( size(exc,id) /= ng(id) ) stop 'lda_PZ81: a dim of array EXC does not match RHO.'
#endif
    enddo ! id

#ifdef DEBUG
    if( size(vxc,4) /= size(den,4) ) stop 'lda_PZ81: spin dim of VXC does not match RHO.'
#endif

    if( size(den,4) == 1 ) then
      ! LDA part


      do iz = 1, ng(3)
        do iy = 1, ng(2)
          do ix = 1, ng(1)
            rho = den(ix,iy,iz,1)
            if( rho > TINYDEN ) then

              rs = ( FOURPITHIRD * rho )**(-1./3.)
              if( rs >= 1. ) then
                ecp = fecl(rs,GP,B1P,B2P)
                vcp = fvcl(ecp,rs,B1P,B2P)
              else  ! rs > 1.
                ecp = fecs(rs,AP,BP,CP,DP)
                vcp = fvcs(rs,AP,BP,CP,DP)
              endif ! rs > 1.

              if( relativistic_corrections ) then
                relc = relcor( rho )
              else  
                relc = 1.0
              endif 

              vx = THREEOVER2PI23/rs
              Exc(ix,iy,iz)   = HARTREE * ecp - relc(0)*0.75*vx
              Vxc(ix,iy,iz,1) = HARTREE * vcp - relc(1)*vx
            else  ! rho > TINYDEN
              Exc(ix,iy,iz)   = 0.0
              Vxc(ix,iy,iz,1) = 0.0
            endif ! rho > TINYDEN
          enddo ! ix
        enddo ! iy
      enddo ! iz

    elseif( size(den,4) == 2 ) then
      ! LSDA part

      write(*,'(9A)') sym, fun, ERROR, 'pz81 spin = 2 not implemented'
      stop 'pz81 spin = 2 not implemented'

      do iz = 1, ng(3)
        do iy = 1, ng(2)
          do ix = 1, ng(1)
            rho = den(ix,iy,iz,1) + den(ix,iy,iz,2)
            if( rho > TINYDEN ) then
              Exc(ix,iy,iz)  = 0.0
              Vxc(ix,iy,iz,1)= 0.0
              Vxc(ix,iy,iz,2)= 0.0
            else  ! rho > TINYDEN
              Exc(ix,iy,iz)  = 0.0
              Vxc(ix,iy,iz,1)= 0.0
              Vxc(ix,iy,iz,2)= 0.0
            endif ! rho > TINYDEN
          enddo ! ix
        enddo ! iy
      enddo ! iz

    else ! size(den,4) == 1

      write(*,'(9A)') sym, fun, ERROR, 'rho(:,:,:,nspin). nspin should be 1 for LDA, 2 for LSDA'
      stop 'DFT lda_pz81: dim # 4 of RHO is neither 1 (LDA) nor 2 (LSDA).'

    endif ! size(rho,4) == 1

#ifdef DEBUG
    ist = searchforNaN( vxc, name='exchange correlation potential' )
#endif

  contains

    real function fecl(r,g,b1,b2)
      real, intent(in)      :: r,g,b1,b2
      fecl = g / ( 1. + b1*sqrt(r) + b2*r )
    endfunction ! fecl

    real function fvcl(ce,r,b1,b2)
      real, intent(in)      :: ce,r,b1,b2
      fvcl = ce* (1.+7./6.*b1*sqrt(r)+4./3.*b2*r)/(1.+b1*sqrt(r)+b2*r)
    endfunction ! fvcl

    real function fecs(r,a,b,c,d)
      real, intent(in)      :: r,a,b,c,d
      fecs = a*log(r) + b + c*r*log(r) + d*r
    endfunction ! fecs

    real function fvcs(r,a,b,c,d)
      real r,a,b,c,d
      fvcs = a*log(r) + (b-a/3.) + 2./3.*c*r*log(r) + (2.*d-c)*r/3.
    endfunction ! fvcs

  endfunction ! lda_pz_81

  status_t function show_available_functionals( unt ) result( ios )
    iounit_t, intent(in) :: unt
    integer :: i
    if(unt>0) write(unit=unt,fmt='(99A)',iostat=ios) 'Available functionals:', ( "  ", trim(Dictionary(i)%word), i=1,size(Dictionary) )
  endfunction ! show

#ifdef EXTENDED
!+ extended

  ! debugging tool
  integer function searchforNaN( array, name ) result( cnt )
    real, intent(in)                :: array(:,:,:,:)
    character(len=*), intent(in)    :: name ! name of the array
    cnt = count( array /= array )
    if(cnt>0 .and. o>0) write(o,'(4A,9(I0,A))') sym, ' searchforNaN: in array "', trim(name), '" ',cnt,' of ',size(array),' are NaN!'
  endfunction ! searchforNaN

  status_t function test( )
    test = show_available_functionals( 6 )
  endfunction ! test

!- extended
#endif
endmodule ! density_functionals


!
!                                       September 20, 1996 
! Dear Colleague,
! 
! 
! Please find below the routines for our new PBE functional, and  a
! uuencoded  gzipped  postscipt  file of the accompanying paper (to
! unpack, first uudecode, then gunzip).
! 
! The PBE is intended to replace PW91 and our earlier GGA''s for Exc.
! For most purposes, it should yield energies very close to those of
! PW91.  However, the derivation and analytic form have been simplified,
! and some spurious wiggles in the xc potential have been eliminated.
! 
! Individuals receiving the first round of this e-mail will also get
! a hard copy of the PBE manuscript by regular mail.  This manuscript
! has just been accepted by Phys. Rev. Lett.  Best regards,
!  
! John, Kieron, and Matthias.  
! -----------------------------------------------------------------
! c PBE alpha2.1:
! c Perdew-Burke-Ernzerhof generalized gradient approximation to the
! c density functional for exchange-correlation energy of a many-electron
! c system.
! c  --------------------------------------------------------------------
! c |WARNING!  PBE is a simplification of PW91, which yields almost      |
! c |identical numerical results with simpler formulas from a simpler    |
! c |derivation.  If you should find significant DIFFERENCES between     |
! c |PBE and PW91 results, please consult kieron@merlin.phy.tulane.edu   |
! c |or perdew@mailhost.tcs.tulane.edu.  Thank you.                      |
! c  --------------------------------------------------------------------
! c Note: Neglects small grad (zeta) contributions to the correlation
! c energy.
! c
! c Programs implement functional in PBE paper, July 1996 version.
! c 
! c----------------------------------------------------------------------
! c Main program testing PBE subroutines for exchange-correlation energy
! c and potentials, by application to unequal exponential
! c up- and down-spin densities, showing that the functional derivative
! c (exchange-correlation potential) correctly generates the energy change
! c due to a small change in the density.
! c Kieron Burke, July 2, 1996.
! C Atomic units are used, so all energies are in Hartrees and all
! c distances in bohrs.  
! c 1 Hartree=27.2116eV=627.5kcal/mol; 1bohr=0.529E-10m.
! c The output should be:
! c Fup Fdn Zup Zdn             Exc           CHNG1          CHNG
! c 1.0  .0 1.0  .5  -.311916530229   .000000000000   .0000000000
! c 1.0  .2 1.0  .5  -.336377065446  -.053880102756  -.0538804290
! c 1.0  .4 1.0  .5  -.369084886012  -.120463328976  -.1204642921
! c 1.0  .6 1.0  .5  -.406088525151  -.193370595518  -.1933723422
! c 1.0  .8 1.0  .5  -.446305936853  -.271252139632  -.2712547575
! c 1.0 1.0 1.0  .5  -.489150144888  -.353405855349  -.3534094042
! c 1.0 1.0  .5  .5  -.341059977353  -.316599687356  -.3166037653
! c 1.0 1.0 1.0 1.0  -.653407740519  -.309758886707  -.3097606837
! c 1.0 1.0 1.5 1.5  -.962039224827  -.307820467953  -.3078216918
! c 1.0 1.0 2.0 2.0 -1.269410948459  -.307021487395  -.3070225637
! c----------------------------------------------------------------------
! c----------------------------------------------------------------------
!       IMPLICIT REAL*8 (A-H,O-Z)
!       parameter(thrd=1.d0/3.d0,thrd2=2.d0*thrd)
!       pi=4.d0*datan(1.d0)
!       CONF=(3.D0*PI*pi)**THRD
!       CONRS=(3.D0/(4.D0*PI))**THRD
!       write(6,*)'Fup Fdn Zup Zdn             Exc'
!      1,'           CHNG1          CHNG'
! c----------------------------------------------------------------------
! c----------------------------------------------------------------------
! C BEGIN THE LOOP THAT SELECTS A TRIAL DENSITY
! c spin-densities are of the form
! c          rho(r)=f*(Z**3/pi)*dexp(-2*Z*r)
! c delzdn=small change in zdn to test potentials
! c jdens=counter for which density
!       DO JDENS = 1,10
!         FUP=1.D0
!         FDN=0.2D0*(JDENS-1)
!         ZUP=1.D0
!         ZDN=0.5D0
!         IF(JDENS.GT.6)then
!         FDN=1.D0
!           ZUP=0.5D0+0.5D0*(JDENS-7)
!           ZDN=ZUP
!       endif
!         DELZDN=1D-5
! c----------------------------------------------------------------------
! c----------------------------------------------------------------------
! C BEGIN THE LOOP THAT INCREMENTS THE DENSITY DIFFERENTIALLY
! c kdif=1=>density as above
! c kdif=2=>Zdn changed by DELZDN
!         DO KDIF=1,2
!           IF(KDIF.EQ.2)ZDN=ZDN+DELZDN
! c----------------------------------------------------------------------
! c----------------------------------------------------------------------
! C BEGIN THE RADIAL LOOP
! c sumexc=integrated exchange-correlation energy 
! c chng1=integrated xc energy change, based on vxc
! c nr=number of points in radial loop
! c rf=final value of r in integrals
! c dr=change in r
! c wt=weight of r in trapezoidal rule
! c dup=up density
! c agrup=|grad up|
! c delgrup=(grad up).(grad |grad up|) 
! c uplap=grad^2 up=Laplacian of up
! c dn,agrdn,delgrdn,dnlap=corresponding down quantities
! c d=up+dn
! c agrad=|grad rho|
! c delgrad=(grad rho).(grad |grad rho|) 
!           sumexc=0.0D0
!           CHNG1=0.0D0
!         nr=10000
!         rf=20.d0
!         dr=rf/real(nr)
!           DO I=1,nr
!             R=I*dr
!             WT=4.d0*PI*R*R*dr
!             DUP=FUP*(ZUP**3/PI)*DEXP(-2.D0*ZUP*R)
!             DDN=FDN*(ZDN**3/PI)*DEXP(-2.D0*ZDN*R)
!             ZDNNU=ZDN+DELZDN
!             DELDDN=FDN*(ZDNNU**3/PI)*DEXP(-2.D0*ZDNNU*R)-DDN
!           agrup=2.d0*zup*dup
!           delgrup=8.d0*(zup**3)*dup*dup
!           uplap=4.d0*zup*dup*(zup-1.d0/r)
!           agrdn=2.d0*zdn*ddn
!           delgrdn=8.d0*(zdn**3)*ddn*ddn
!           dnlap=4.d0*zdn*ddn*(zdn-1.d0/r)
!             D=DUP+DDN
!             agrad=2.d0*(ZUP*DUP+ZDN*DDN)
!           delgrad=4.d0*agrad*(ZUP**2*DUP+ZDN**2*DDN)
!             call easypbe(dup,agrup,delgrup,uplap,ddn,agrdn,delgrdn,
!      1           dnlap,agrad,delgrad,1,1,
!      1           exlsd,vxuplsd,vxdnlsd,eclsd,vcuplsd,vcdnlsd,
!      1           expw91,vxuppw91,vxdnpw91,ecpw91,vcuppw91,vcdnpw91,
!      1           expbe,vxuppbe,vxdnpbe,ecpbe,vcuppbe,vcdnpbe)
!           sumexc=sumexc+d*(expbe+ecpbe)*wt
!             CHNG1=CHNG1+(vxdnpbe+vcdnpbe)*DELDDN*WT/DELZDN
!         enddo
!           IF(KDIF.EQ.1)then
!           sumEXCO=sumEXC
!         endif
!         enddo
! c----------------------------------------------------------------------
! c----------------------------------------------------------------------
! C  CHNG: DIRECT XC ENERGY INCREMENT
! C  IF THE FUNCTIONAL DERIVATIVE IS CORRECT, THEN CHNG1=CHNG
!         CHNG=(sumEXC-sumEXCO)/DELZDN
!         PRINT 200,FUP,FDN,ZUP,ZDN,sumEXC,CHNG1,chng
!       enddo
!       STOP
!   200 FORMAT(4f4.1,2f16.12,f14.10)
!       END
! c----------------------------------------------------------------------
! c######################################################################
! c----------------------------------------------------------------------
!       subroutine easypbe(up,agrup,delgrup,uplap,dn,agrdn,delgrdn,dnlap,
!      1           agr,delgr,lcor,lpot,
!      1           exlsd,vxuplsd,vxdnlsd,eclsd,vcuplsd,vcdnlsd,
!      1           expw91,vxuppw91,vxdnpw91,ecpw91,vcuppw91,vcdnpw91,
!      1           expbe,vxuppbe,vxdnpbe,ecpbe,vcuppbe,vcdnpbe)
! c----------------------------------------------------------------------
! c----------------------------------------------------------------------
! c EASYPBE is a driver for the PBE subroutines, using simple inputs
! c K. Burke, May 14, 1996.
! c inputs: up=up density
! c     : agrup=|grad up|
! c     : delgrup=(grad up).(grad |grad up|) 
! c     : uplap=grad^2 up=Laplacian of up
! c     : dn,agrdn,delgrdn,dnlap=corresponding down quantities
! c     : agr=|grad rho|
! c     : delgr=(grad rho).(grad |grad rho|) 
! c     : lcor=flag to do correlation(=0=>don''t)
! c     : lpot=flag to do potential(=0=>don''t)
! c outputs: exlsd=LSD exchange energy density, so that
! c           ExLSD=int d^3r rho(r) exlsd(r)
! c      : vxuplsd=up LSD exchange potential
! c      : vxdnlsd=down LSD exchange potential
! c        : exclsd=LSD exchange-correlation energy density
! c      : vxcuplsd=up LSD exchange-correlation potential
! c      : vxcdnlsd=down LSD exchange-correlation potential
! c        : expw91,vxuppw91,vxdnpw91,ecpw91,etc.=PW91 quantities
! c        : expbe,vxuppbe,vxdnpbe,ecpbe,etc.=PBE quantities
! c----------------------------------------------------------------------
! c----------------------------------------------------------------------
! c needed constants:
! c pi32=3 pi**2
! c alpha=(9pi/4)**thrd
!       implicit real*8(a-h,o-z)
!       parameter(thrd=1.d0/3.d0,thrd2=2.d0*thrd)
!       parameter(pi32=29.608813203268075856503472999628d0)
!       parameter(pi=3.1415926535897932384626433832795d0)
!       parameter(alpha=1.91915829267751300662482032624669d0)
! c----------------------------------------------------------------------
! c----------------------------------------------------------------------
! c PBE exchange
! c use  Ex[up,dn]=0.5*(Ex[2*up]+Ex[2*dn]) (i.e., exact spin-scaling)
! c do up exchange
! c fk=local Fermi wavevector for 2*up=(3 pi^2 (2up))^(1/3) 
! c s=dimensionless density gradient=|grad rho|/ (2*fk*rho)_(rho=2*up)
! c u=delgrad/(rho^2*(2*fk)**3)_(rho=2*up)
! c v=Laplacian/(rho*(2*fk)**2)_(rho=2*up)
!       rho2=2.d0*up
!       if(rho2.gt.1d-18)then
!         fk=(pi32*rho2)**thrd
!         s=2.d0*agrup/(2.d0*fk*rho2)
!         u=4.d0*delgrup/(rho2*rho2*(2.d0*fk)**3)
!         v=2.d0*uplap/(rho2*(2.d0*fk)**2)
!         call exchpbe(rho2,s,u,v,0,lpot,exuplsd,vxuplsd)
!         call exchpw91(rho2,s,u,v,exuppw91,vxuppw91)
!         call exchpbe(rho2,s,u,v,1,lpot,exuppbe,vxuppbe)
!       else
!       exuplsd=0.d0
!       vxuplsd=0.d0
!       exuppw91=0.d0
!       vxuppw91=0.d0
!       exuppbe=0.d0
!       vxuppbe=0.d0
!       endif
! c repeat for down
!       rho2=2.d0*dn
!       if(rho2.gt.1d-18)then
!         fk=(pi32*rho2)**thrd
!         s=2.d0*agrdn/(2.d0*fk*rho2)
!         u=4.d0*delgrdn/(rho2*rho2*(2.d0*fk)**3)
!         v=2.d0*dnlap/(rho2*(2.d0*fk)**2)
!         call exchpbe(rho2,s,u,v,0,lpot,exdnlsd,vxdnlsd)
!         call exchpw91(rho2,s,u,v,exdnpw91,vxdnpw91)
!         call exchpbe(rho2,s,u,v,1,lpot,exdnpbe,vxdnpbe)
!       else
!       exdnlsd=0.d0
!       vxdnlsd=0.d0
!       exdnpw91=0.d0
!       vxdnpw91=0.d0
!       exdnpbe=0.d0
!       vxdnpbe=0.d0
!       endif
! 10    continue 
! c construct total density and contribution to ex
!       rho=up+dn
!       exlsd=(exuplsd*up+exdnlsd*dn)/rho
!       expw91=(exuppw91*up+exdnpw91*dn)/rho
!       expbe=(exuppbe*up+exdnpbe*dn)/rho
!       if(lcor.eq.0)return
! c----------------------------------------------------------------------
! c----------------------------------------------------------------------
! c Now do correlation
! c zet=(up-dn)/rho
! c g=phi(zeta)
! c rs=(3/(4pi*rho))^(1/3)=local Seitz radius=alpha/fk
! c sk=Ks=Thomas-Fermi screening wavevector=sqrt(4fk/pi)
! c twoksg=2*Ks*phi
! c t=correlation dimensionless gradient=|grad rho|/(2*Ks*phi*rho)
! c uu=delgrad/(rho^2*twoksg^3)
! c rholap=Laplacian
! c vv=Laplacian/(rho*twoksg^2)
! c ww=(|grad up|^2-|grad dn|^2-zet*|grad rho|^2)/(rho*twoksg)^2
! c ec=lsd correlation energy
! c vcup=lsd up correlation potential
! c vcdn=lsd down correlation potential
! c h=gradient correction to correlation energy
! c dvcup=gradient correction to up correlation potential
! c dvcdn=gradient correction to down correlation potential
!       if(rho.lt.1.d-18)return
!       zet=(up-dn)/rho
!       g=((1.d0+zet)**thrd2+(1.d0-zet)**thrd2)/2.d0
!       fk=(pi32*rho)**thrd
!       rs=alpha/fk
!       sk=sqrt(4.d0*fk/pi)
!       twoksg=2.d0*sk*g
!       t=agr/(twoksg*rho)
!       uu=delgr/(rho*rho*twoksg**3)
!       rholap=uplap+dnlap
!       vv=rholap/(rho*twoksg**2)
!       ww=(agrup**2-agrdn**2-zet*agr**2)/(rho*rho*twoksg**2)
!       call CORPBE(RS,ZET,T,UU,VV,WW,1,lpot,ec,vcup,vcdn,
!      1                  H,DVCUP,DVCDN)
!       eclsd=ec
!       ecpbe=ec+h
!       vcuplsd=vcup
!       vcdnlsd=vcdn
!       vcuppbe=vcup+dvcup
!       vcdnpbe=vcdn+dvcdn
!       call CORLSD(RS,ZET,EC,VCUP,VCDN,ECRS,ECZET,ALFC)
!       call CORPW91(RS,ZET,G,EC,ECRS,ECZET,T,UU,VV,WW,H,DVCUP,DVCDN)
!       ecpw91=ec+h
!       vcuppw91=vcup+dvcup
!       vcdnpw91=vcdn+dvcdn
!       return
!       end
! c----------------------------------------------------------------------
! c######################################################################
! c----------------------------------------------------------------------
!       SUBROUTINE EXCHPBE(rho,S,U,V,lgga,lpot,EX,VX)
! c----------------------------------------------------------------------
! C  PBE EXCHANGE FOR A SPIN-UNPOLARIZED ELECTRONIC SYSTEM
! c  K Burke''s modification of PW91 codes, May 14, 1996
! c  Modified again by K. Burke, June 29, 1996, with simpler Fx(s)
! c----------------------------------------------------------------------
! c----------------------------------------------------------------------
! C  INPUT rho : DENSITY
! C  INPUT S:  ABS(GRAD rho)/(2*KF*rho), where kf=(3 pi^2 rho)^(1/3)
! C  INPUT U:  (GRAD rho)*GRAD(ABS(GRAD rho))/(rho**2 * (2*KF)**3)
! C  INPUT V: (LAPLACIAN rho)/(rho*(2*KF)**2)
! c   (for U,V, see PW86(24))
! c  input lgga:  (=0=>don''t put in gradient corrections, just LDA)
! c  input lpot:  (=0=>don''t get potential and don''t need U and V)
! C  OUTPUT:  EXCHANGE ENERGY PER ELECTRON (EX) AND POTENTIAL (VX)
! c----------------------------------------------------------------------
! c----------------------------------------------------------------------
! c References:
! c [a]J.P.~Perdew, K.~Burke, and M.~Ernzerhof, submiited to PRL, May96
! c [b]J.P. Perdew and Y. Wang, Phys. Rev.  B {\bf 33},  8800  (1986);
! c     {\bf 40},  3399  (1989) (E).
! c----------------------------------------------------------------------
! c----------------------------------------------------------------------
! c Formulas:
! c     e_x[unif]=ax*rho^(4/3)  [LDA]
! c ax = -0.75*(3/pi)^(1/3)
! c     e_x[PBE]=e_x[unif]*FxPBE(s)
! c     FxPBE(s)=1+uk-uk/(1+ul*s*s)                 [a](13)
! c uk, ul defined after [a](13) 
! c----------------------------------------------------------------------
! c----------------------------------------------------------------------
!       IMPLICIT REAL*8 (A-H,O-Z)
!       parameter(thrd=1.d0/3.d0,thrd4=4.d0/3.d0)
!       parameter(pi=3.14159265358979323846264338327950d0)
!       parameter(ax=-0.738558766382022405884230032680836d0)
!       parameter(um=0.2195149727645171d0,uk=0.8040d0,ul=um/uk)
! c----------------------------------------------------------------------
! c----------------------------------------------------------------------
! c construct LDA exchange energy density
!       exunif = AX*rho**THRD
!       if(lgga.eq.0)then
!       ex=exunif
!         vx=ex*thrd4
!       return
!       endif
! c----------------------------------------------------------------------
! c----------------------------------------------------------------------
! c construct PBE enhancement factor
!       S2 = S*S
!       P0=1.d0+ul*S2
!       FxPBE = 1d0+uk-uk/P0
!       EX = exunif*FxPBE
!       if(lpot.eq.0)return
! c----------------------------------------------------------------------
! c----------------------------------------------------------------------
! C  ENERGY DONE. NOW THE POTENTIAL:
! c  find first and second derivatives of Fx w.r.t s.
! c  Fs=(1/s)*d FxPBE/ ds
! c  Fss=d Fs/ds
!       Fs=2.d0*uk*ul/(P0*P0)
!       Fss=-4.d0*ul*S*Fs/P0
! c----------------------------------------------------------------------
! c----------------------------------------------------------------------
! c calculate potential from [b](24) 
!       VX = exunif*(THRD4*FxPBE-(U-THRD4*S2*s)*FSS-V*FS)
!       RETURN
!       END
! c----------------------------------------------------------------------
! c######################################################################
! c----------------------------------------------------------------------
!       SUBROUTINE CORPBE(RS,ZET,T,UU,VV,WW,lgga,lpot,ec,vcup,vcdn,
!      1                  H,DVCUP,DVCDN)
! c----------------------------------------------------------------------
! c  Official PBE correlation code. K. Burke, May 14, 1996.
! C  INPUT: RS=SEITZ RADIUS=(3/4pi rho)^(1/3)
! C       : ZET=RELATIVE SPIN POLARIZATION = (rhoup-rhodn)/rho
! C       : t=ABS(GRAD rho)/(rho*2.*KS*G)  -- only needed for PBE
! C       : UU=(GRAD rho)*GRAD(ABS(GRAD rho))/(rho**2 * (2*KS*G)**3)
! C       : VV=(LAPLACIAN rho)/(rho * (2*KS*G)**2)
! C       : WW=(GRAD rho)*(GRAD ZET)/(rho * (2*KS*G)**2
! c       :  UU,VV,WW, only needed for PBE potential
! c       : lgga=flag to do gga (0=>LSD only)
! c       : lpot=flag to do potential (0=>energy only)
! c  output: ec=lsd correlation energy from [a]
! c        : vcup=lsd up correlation potential
! c        : vcdn=lsd dn correlation potential
! c        : h=NONLOCAL PART OF CORRELATION ENERGY PER ELECTRON
! c        : dvcup=nonlocal correction to vcup
! c        : dvcdn=nonlocal correction to vcdn
! c----------------------------------------------------------------------
! c----------------------------------------------------------------------
! c References:
! c [a] J.P.~Perdew, K.~Burke, and M.~Ernzerhof, 
! c     {\sl Generalized gradient approximation made simple}, sub.
! c     to Phys. Rev.Lett. May 1996.
! c [b] J. P. Perdew, K. Burke, and Y. Wang, {\sl Real-space cutoff
! c     construction of a generalized gradient approximation:  The PW91
! c     density functional}, submitted to Phys. Rev. B, Feb. 1996.
! c [c] J. P. Perdew and Y. Wang, Phys. Rev. B {\bf 45}, 13244 (1992).
! c----------------------------------------------------------------------
! c----------------------------------------------------------------------
!       IMPLICIT REAL*8 (A-H,O-Z)
! c thrd*=various multiples of 1/3
! c numbers for use in LSD energy spin-interpolation formula, [c](9).
! c      GAM= 2^(4/3)-2
! c      FZZ=f''(0)= 8/(9*GAM)
! c numbers for construction of PBE
! c      gamma=(1-log(2))/pi^2
! c      bet=coefficient in gradient expansion for correlation, [a](4).
! c      eta=small number to stop d phi/ dzeta from blowing up at 
! c          |zeta|=1.
!       parameter(thrd=1.d0/3.d0,thrdm=-thrd,thrd2=2.d0*thrd)
!       parameter(sixthm=thrdm/2.d0)
!       parameter(thrd4=4.d0*thrd)
!       parameter(GAM=0.5198420997897463295344212145565d0)
!       parameter(fzz=8.d0/(9.d0*GAM))
!       parameter(gamma=0.03109069086965489503494086371273d0)
!       parameter(bet=0.06672455060314922d0,delt=bet/gamma)
!       parameter(eta=1.d-12)
! c----------------------------------------------------------------------
! c----------------------------------------------------------------------
! c find LSD energy contributions, using [c](10) and Table I[c].
! c EU=unpolarized LSD correlation energy
! c EURS=dEU/drs
! c EP=fully polarized LSD correlation energy
! c EPRS=dEP/drs
! c ALFM=-spin stiffness, [c](3).
! c ALFRSM=-dalpha/drs
! c F=spin-scaling factor from [c](9).
! c construct ec, using [c](8)
!       rtrs=dsqrt(rs)
!       CALL gcor2(0.0310907D0,0.21370D0,7.5957D0,3.5876D0,1.6382D0,
!      1    0.49294D0,rtrs,EU,EURS)
!       CALL gcor2(0.01554535D0,0.20548D0,14.1189D0,6.1977D0,3.3662D0,
!      1    0.62517D0,rtRS,EP,EPRS)
!       CALL gcor2(0.0168869D0,0.11125D0,10.357D0,3.6231D0,0.88026D0,
!      1    0.49671D0,rtRS,ALFM,ALFRSM)
!       ALFC = -ALFM
!       Z4 = ZET**4
!       F=((1.D0+ZET)**THRD4+(1.D0-ZET)**THRD4-2.D0)/GAM
!       EC = EU*(1.D0-F*Z4)+EP*F*Z4-ALFM*F*(1.D0-Z4)/FZZ
! c----------------------------------------------------------------------
! c----------------------------------------------------------------------
! c LSD potential from [c](A1)
! c ECRS = dEc/drs [c](A2)
! c ECZET=dEc/dzeta [c](A3)
! c FZ = dF/dzeta [c](A4)
!       ECRS = EURS*(1.D0-F*Z4)+EPRS*F*Z4-ALFRSM*F*(1.D0-Z4)/FZZ
!       FZ = THRD4*((1.D0+ZET)**THRD-(1.D0-ZET)**THRD)/GAM
!       ECZET = 4.D0*(ZET**3)*F*(EP-EU+ALFM/FZZ)+FZ*(Z4*EP-Z4*EU
!      1        -(1.D0-Z4)*ALFM/FZZ)
!       COMM = EC -RS*ECRS/3.D0-ZET*ECZET
!       VCUP = COMM + ECZET
!       VCDN = COMM - ECZET
!       if(lgga.eq.0)return
! c----------------------------------------------------------------------
! c----------------------------------------------------------------------
! c PBE correlation energy
! c G=phi(zeta), given after [a](3)
! c DELT=bet/gamma
! c B=A of [a](8)
!       G=((1.d0+ZET)**thrd2+(1.d0-ZET)**thrd2)/2.d0
!       G3 = G**3
!       PON=-EC/(G3*gamma)
!       B = DELT/(DEXP(PON)-1.D0)
!       B2 = B*B
!       T2 = T*T
!       T4 = T2*T2
!       RS2 = RS*RS
!       RS3 = RS2*RS
!       Q4 = 1.D0+B*T2
!       Q5 = 1.D0+B*T2+B2*T4
!       H = G3*(BET/DELT)*DLOG(1.D0+DELT*Q4*T2/Q5)
!       if(lpot.eq.0)return
! c----------------------------------------------------------------------
! c----------------------------------------------------------------------
! C ENERGY DONE. NOW THE POTENTIAL, using appendix E of [b].
!       G4 = G3*G
!       T6 = T4*T2
!       RSTHRD = RS/3.D0
!       GZ=(((1.d0+zet)**2+eta)**sixthm-
!      1((1.d0-zet)**2+eta)**sixthm)/3.d0
!       FAC = DELT/B+1.D0
!       BG = -3.D0*B2*EC*FAC/(BET*G4)
!       BEC = B2*FAC/(BET*G3)
!       Q8 = Q5*Q5+DELT*Q4*Q5*T2
!       Q9 = 1.D0+2.D0*B*T2
!       hB = -BET*G3*B*T6*(2.D0+B*T2)/Q8
!       hRS = -RSTHRD*hB*BEC*ECRS
!       FACT0 = 2.D0*DELT-6.D0*B
!       FACT1 = Q5*Q9+Q4*Q9*Q9
!       hBT = 2.D0*BET*G3*T4*((Q4*Q5*FACT0-DELT*FACT1)/Q8)/Q8
!       hRST = RSTHRD*T2*hBT*BEC*ECRS
!       hZ = 3.D0*GZ*h/G + hB*(BG*GZ+BEC*ECZET)
!       hT = 2.d0*BET*G3*Q9/Q8
!       hZT = 3.D0*GZ*hT/G+hBT*(BG*GZ+BEC*ECZET)
!       FACT2 = Q4*Q5+B*T2*(Q4*Q9+Q5)
!       FACT3 = 2.D0*B*Q5*Q9+DELT*FACT2
!       hTT = 4.D0*BET*G3*T*(2.D0*B/Q8-(Q9*FACT3/Q8)/Q8)
!       COMM = H+HRS+HRST+T2*HT/6.D0+7.D0*T2*T*HTT/6.D0
!       PREF = HZ-GZ*T2*HT/G
!       FACT5 = GZ*(2.D0*HT+T*HTT)/G
!       COMM = COMM-PREF*ZET-UU*HTT-VV*HT-WW*(HZT-FACT5)
!       DVCUP = COMM + PREF
!       DVCDN = COMM - PREF
!       RETURN
!       END
! c----------------------------------------------------------------------
! c######################################################################
! c----------------------------------------------------------------------
!       SUBROUTINE GCOR2(A,A1,B1,B2,B3,B4,rtrs,GG,GGRS)
! c slimmed down version of GCOR used in PW91 routines, to interpolate
! c LSD correlation energy, as given by (10) of
! c J. P. Perdew and Y. Wang, Phys. Rev. B {\bf 45}, 13244 (1992).
! c K. Burke, May 11, 1996.
!       IMPLICIT REAL*8 (A-H,O-Z)
!       Q0 = -2.D0*A*(1.D0+A1*rtrs*rtrs)
!       Q1 = 2.D0*A*rtrs*(B1+rtrs*(B2+rtrs*(B3+B4*rtrs)))
!       Q2 = DLOG(1.D0+1.D0/Q1)
!       GG = Q0*Q2
!       Q3 = A*(B1/rtrs+2.D0*B2+rtrs*(3.D0*B3+4.D0*B4*rtrs))
!       GGRS = -2.D0*A*A1*Q2-Q0*Q3/(Q1*(1.d0+Q1))
!       RETURN
!       END
! c----------------------------------------------------------------------
! c######################################################################
! c----------------------------------------------------------------------
!       SUBROUTINE EXCHPW91(D,S,U,V,EX,VX)
! C  GGA91 EXCHANGE FOR A SPIN-UNPOLARIZED ELECTRONIC SYSTEM
! C  INPUT D : DENSITY
! C  INPUT S:  ABS(GRAD D)/(2*KF*D)
! C  INPUT U:  (GRAD D)*GRAD(ABS(GRAD D))/(D**2 * (2*KF)**3)
! C  INPUT V: (LAPLACIAN D)/(D*(2*KF)**2)
! C  OUTPUT:  EXCHANGE ENERGY PER ELECTRON (EX) AND POTENTIAL (VX)
!       IMPLICIT REAL*8 (A-H,O-Z)
!       parameter(a1=0.19645D0,a2=0.27430D0,a3=0.15084D0,a4=100.d0)
!       parameter(ax=-0.7385588D0,a=7.7956D0,b1=0.004d0)
!       parameter(thrd=0.333333333333D0,thrd4=1.33333333333D0)
! c for Becke exchange, set a3=b1=0
!       FAC = AX*D**THRD
!       S2 = S*S
!       S3 = S2*S
!       S4 = S2*S2
!       P0 = 1.D0/DSQRT(1.D0+A*A*S2)
!       P1 = DLOG(A*S+1.D0/P0)
!       P2 = DEXP(-A4*S2)
!       P3 = 1.D0/(1.D0+A1*S*P1+B1*S4)
!       P4 = 1.D0+A1*S*P1+(A2-A3*P2)*S2
!       F = P3*P4
!       EX = FAC*F
! C  LOCAL EXCHANGE OPTION
! C     EX = FAC
! C  ENERGY DONE. NOW THE POTENTIAL:
!       P5 = B1*S2-(A2-A3*P2)
!       P6 = A1*S*(P1+A*S*P0)
!       P7 = 2.D0*(A2-A3*P2)+2.D0*A3*A4*S2*P2-4.D0*B1*S2*F
!       FS = P3*(P3*P5*P6+P7)
!       P8 = 2.D0*S*(B1-A3*A4*P2)
!       P9 = A1*P1+A*A1*S*P0*(3.D0-A*A*S2*P0*P0)
!       P10 = 4.D0*A3*A4*S*P2*(2.D0-A4*S2)-8.D0*B1*S*F-4.D0*B1*S3*FS
!       P11 = -P3*P3*(A1*P1+A*A1*S*P0+4.D0*B1*S3)
!       FSS = P3*P3*(P5*P9+P6*P8)+2.D0*P3*P5*P6*P11+P3*P10+P7*P11
!       VX = FAC*(THRD4*F-(U-THRD4*S3)*FSS-V*FS)
! C  LOCAL EXCHANGE OPTION:
! C     VX = FAC*THRD4
!       RETURN
!       END
! c----------------------------------------------------------------------
! c######################################################################
! c----------------------------------------------------------------------
!       SUBROUTINE CORLSD(RS,ZET,EC,VCUP,VCDN,ECRS,ECZET,ALFC)
! C  UNIFORM-GAS CORRELATION OF PERDEW AND WANG 1991
! C  INPUT: SEITZ RADIUS (RS), RELATIVE SPIN POLARIZATION (ZET)
! C  OUTPUT: CORRELATION ENERGY PER ELECTRON (EC), UP- AND DOWN-SPIN
! C     POTENTIALS (VCUP,VCDN), DERIVATIVES OF EC WRT RS (ECRS) & ZET (ECZET)
! C  OUTPUT: CORRELATION CONTRIBUTION (ALFC) TO THE SPIN STIFFNESS
!       IMPLICIT REAL*8 (A-H,O-Z)
!       parameter(gam=0.5198421D0,fzz=1.709921D0)
!       parameter(thrd=0.333333333333D0,thrd4=1.333333333333D0)
!       F = ((1.D0+ZET)**THRD4+(1.D0-ZET)**THRD4-2.D0)/GAM
!       CALL GCOR(0.0310907D0,0.21370D0,7.5957D0,3.5876D0,1.6382D0,
!      1    0.49294D0,1.00D0,RS,EU,EURS)
!       CALL GCOR(0.01554535D0,0.20548D0,14.1189D0,6.1977D0,3.3662D0,
!      1    0.62517D0,1.00D0,RS,EP,EPRS)
!       CALL GCOR(0.0168869D0,0.11125D0,10.357D0,3.6231D0,0.88026D0,
!      1    0.49671D0,1.00D0,RS,ALFM,ALFRSM)
! C  ALFM IS MINUS THE SPIN STIFFNESS ALFC
!       ALFC = -ALFM
!       Z4 = ZET**4
!       EC = EU*(1.D0-F*Z4)+EP*F*Z4-ALFM*F*(1.D0-Z4)/FZZ
! C  ENERGY DONE. NOW THE POTENTIAL:
!       ECRS = EURS*(1.D0-F*Z4)+EPRS*F*Z4-ALFRSM*F*(1.D0-Z4)/FZZ
!       FZ = THRD4*((1.D0+ZET)**THRD-(1.D0-ZET)**THRD)/GAM
!       ECZET = 4.D0*(ZET**3)*F*(EP-EU+ALFM/FZZ)+FZ*(Z4*EP-Z4*EU
!      1        -(1.D0-Z4)*ALFM/FZZ)
!       COMM = EC -RS*ECRS/3.D0-ZET*ECZET
!       VCUP = COMM + ECZET
!       VCDN = COMM - ECZET
!       RETURN
!       END
! c----------------------------------------------------------------------
! c######################################################################
! c----------------------------------------------------------------------
!       SUBROUTINE GCOR(A,A1,B1,B2,B3,B4,P,RS,GG,GGRS)
! C  CALLED BY SUBROUTINE CORLSD
!       IMPLICIT REAL*8 (A-H,O-Z)
!       P1 = P + 1.D0
!       Q0 = -2.D0*A*(1.D0+A1*RS)
!       RS12 = DSQRT(RS)
!       RS32 = RS12**3
!       RSP = RS**P
!       Q1 = 2.D0*A*(B1*RS12+B2*RS+B3*RS32+B4*RS*RSP)
!       Q2 = DLOG(1.D0+1.D0/Q1)
!       GG = Q0*Q2
!       Q3 = A*(B1/RS12+2.D0*B2+3.D0*B3*RS12+2.D0*B4*P1*RSP)
!       GGRS = -2.D0*A*A1*Q2-Q0*Q3/(Q1**2+Q1)
!       RETURN
!       END
! c----------------------------------------------------------------------
! c######################################################################
! c----------------------------------------------------------------------
!       SUBROUTINE CORpw91(RS,ZET,G,EC,ECRS,ECZET,T,UU,VV,WW,H,
!      1                   DVCUP,DVCDN)
! C  pw91 CORRELATION, modified by K. Burke to put all arguments 
! c  as variables in calling statement, rather than in common block
! c  May, 1996.
! C  INPUT RS: SEITZ RADIUS
! C  INPUT ZET: RELATIVE SPIN POLARIZATION
! C  INPUT T: ABS(GRAD D)/(D*2.*KS*G)
! C  INPUT UU: (GRAD D)*GRAD(ABS(GRAD D))/(D**2 * (2*KS*G)**3)
! C  INPUT VV: (LAPLACIAN D)/(D * (2*KS*G)**2)
! C  INPUT WW:  (GRAD D)*(GRAD ZET)/(D * (2*KS*G)**2
! C  OUTPUT H: NONLOCAL PART OF CORRELATION ENERGY PER ELECTRON
! C  OUTPUT DVCUP,DVCDN:  NONLOCAL PARTS OF CORRELATION POTENTIALS
!       IMPLICIT REAL*8 (A-H,O-Z)
!       parameter(xnu=15.75592D0,cc0=0.004235D0,cx=-0.001667212D0)
!       parameter(alf=0.09D0)
!       parameter(c1=0.002568D0,c2=0.023266D0,c3=7.389D-6,c4=8.723D0)
!       parameter(c5=0.472D0,c6=7.389D-2,a4=100.D0)
!       parameter(thrdm=-0.333333333333D0,thrd2=0.666666666667D0)
!       BET = XNU*CC0
!       DELT = 2.D0*ALF/BET
!       G3 = G**3
!       G4 = G3*G
!       PON = -DELT*EC/(G3*BET)
!       B = DELT/(DEXP(PON)-1.D0)
!       B2 = B*B
!       T2 = T*T
!       T4 = T2*T2
!       T6 = T4*T2
!       RS2 = RS*RS
!       RS3 = RS2*RS
!       Q4 = 1.D0+B*T2
!       Q5 = 1.D0+B*T2+B2*T4
!       Q6 = C1+C2*RS+C3*RS2
!       Q7 = 1.D0+C4*RS+C5*RS2+C6*RS3
!       CC = -CX + Q6/Q7
!       R0 = 0.663436444d0*rs
!       R1 = A4*R0*G4
!       COEFF = CC-CC0-3.D0*CX/7.D0
!       R2 = XNU*COEFF*G3
!       R3 = DEXP(-R1*T2)
!       H0 = G3*(BET/DELT)*DLOG(1.D0+DELT*Q4*T2/Q5)
!       H1 = R3*R2*T2
!       H = H0+H1
! C  LOCAL CORRELATION OPTION:
! C     H = 0.0D0
! C  ENERGY DONE. NOW THE POTENTIAL:
!       CCRS = (C2+2.*C3*RS)/Q7 - Q6*(C4+2.*C5*RS+3.*C6*RS2)/Q7**2
!       RSTHRD = RS/3.D0
!       R4 = RSTHRD*CCRS/COEFF
!       GZ = ((1.D0+ZET)**THRDM - (1.D0-ZET)**THRDM)/3.D0
!       FAC = DELT/B+1.D0
!       BG = -3.D0*B2*EC*FAC/(BET*G4)
!       BEC = B2*FAC/(BET*G3)
!       Q8 = Q5*Q5+DELT*Q4*Q5*T2
!       Q9 = 1.D0+2.D0*B*T2
!       H0B = -BET*G3*B*T6*(2.D0+B*T2)/Q8
!       H0RS = -RSTHRD*H0B*BEC*ECRS
!       FACT0 = 2.D0*DELT-6.D0*B
!       FACT1 = Q5*Q9+Q4*Q9*Q9
!       H0BT = 2.D0*BET*G3*T4*((Q4*Q5*FACT0-DELT*FACT1)/Q8)/Q8
!       H0RST = RSTHRD*T2*H0BT*BEC*ECRS
!       H0Z = 3.D0*GZ*H0/G + H0B*(BG*GZ+BEC*ECZET)
!       H0T = 2.*BET*G3*Q9/Q8
!       H0ZT = 3.D0*GZ*H0T/G+H0BT*(BG*GZ+BEC*ECZET)
!       FACT2 = Q4*Q5+B*T2*(Q4*Q9+Q5)
!       FACT3 = 2.D0*B*Q5*Q9+DELT*FACT2
!       H0TT = 4.D0*BET*G3*T*(2.D0*B/Q8-(Q9*FACT3/Q8)/Q8)
!       H1RS = R3*R2*T2*(-R4+R1*T2/3.D0)
!       FACT4 = 2.D0-R1*T2
!       H1RST = R3*R2*T2*(2.D0*R4*(1.D0-R1*T2)-THRD2*R1*T2*FACT4)
!       H1Z = GZ*R3*R2*T2*(3.D0-4.D0*R1*T2)/G
!       H1T = 2.D0*R3*R2*(1.D0-R1*T2)
!       H1ZT = 2.D0*GZ*R3*R2*(3.D0-11.D0*R1*T2+4.D0*R1*R1*T4)/G
!       H1TT = 4.D0*R3*R2*R1*T*(-2.D0+R1*T2)
!       HRS = H0RS+H1RS
!       HRST = H0RST+H1RST
!       HT = H0T+H1T
!       HTT = H0TT+H1TT
!       HZ = H0Z+H1Z
!       HZT = H0ZT+H1ZT
!       COMM = H+HRS+HRST+T2*HT/6.D0+7.D0*T2*T*HTT/6.D0
!       PREF = HZ-GZ*T2*HT/G
!       FACT5 = GZ*(2.D0*HT+T*HTT)/G
!       COMM = COMM-PREF*ZET-UU*HTT-VV*HT-WW*(HZT-FACT5)
!       DVCUP = COMM + PREF
!       DVCDN = COMM - PREF
! C  LOCAL CORRELATION OPTION:
! C     DVCUP = 0.0D0
! C     DVCDN = 0.0D0
!       RETURN
!       END
! c----------------------------------------------------------------------

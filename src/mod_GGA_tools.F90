#include "config.h"

! #define DEBUG

#ifdef DEBUG_GGA
#define DEBUG
#endif

module gga_tools
implicit none
  private ! default for this module namespace

  public :: derive_radial_function
  public :: polar_angles
  public :: easypbe
#ifdef EXTENDED
  public :: test
#endif

  contains

  subroutine polar_angles( v, theta, phi )
  !***********************************************************************
  ! calculates the polar angle theta and phi of a vector with components
  ! vx, vy and vz.
  ! philipp kurz 2000-02-08
  !***********************************************************************
  use constants, only: Pi
    real, intent(in)  :: v(1:3)
    real, intent(out) :: theta, phi ! results

    real, parameter :: EPS = 1E-8
    real :: rxyz, rxy, r2xy

    r2xy = v(1)*v(1) + v(2)*v(2)
    rxyz = sqrt( r2xy + v(3)*v(3) )
    rxy = sqrt( r2xy )

    if( rxyz < EPS .or. rxy < EPS ) then
      theta = 0.0
      phi = 0.0
      return
    endif

    ! due to rounding errors vxy/vxyz can become > 1, if v(3) is very small.
    ! therefore, make sure that v(3) is not too small.
    if( abs(v(3)) < EPS ) then
      theta = pi / 2.
    else
      theta = asin( rxy / rxyz )
    endif
    if( v(3) < 0. ) theta = Pi - theta

    ! due to rounding errors vy/vxy can become > 1, if v(1) is very small.
    ! therefore, make sure that v(1) is not too small.
    if( abs( v(1) ) < EPS ) then
      phi = Pi / 2.
    else
      phi = asin( abs( v(2) ) / rxy )
    endif
    if( v(1) < 0. ) phi = Pi - phi
    if( v(2) < 0. ) phi = -phi

  endsubroutine ! polar_angles

  !! 1st and 2nd derivative of ro on the radial exponential grid
  !! or the uniform grid with increment d
  subroutine derive_radial_function( nFD, ro, d1r, d2r, d, drdi )
  use configuration, only: o
    integer, intent(in)       :: nFD ! order of the FiniteDifference approximation
    real, intent(in)          :: ro(1:) ! density
    real, intent(out)         :: d1r(1:) ! 1st derivative of ro
    real, intent(out)         :: d2r(1:) ! 2nd derivative of ro
    real, intent(in)          :: d ! anisotropy of the of the exponential grid
    real, intent(in)          :: drdi(1:) ! radial grid derivative w.r.t. the index of the exponential grid

    integer :: i, j, ie, is
    real    :: inv1, inv2, h

    h = 1.
    is = 1
    ie = size( ro,1 )
#ifdef DEBUG
    if( size(d1r,1) < ie ) stop 'derive_radial_function: dim of D1R is too small!'
    if( size(d2r,1) < ie ) stop 'derive_radial_function: dim of D2R is too small!'
#endif

    selectcase( nFD )
    case( 1 )
      inv1 = 1./(2*h)
      inv2 = 1./(1*h*h)

      i = is
      d1r(i+0) = -3*ro(i)+4*ro(i+1)-1*ro(i+2) !131
      d2r(i+0) =  1*ro(i)-2*ro(i+1)+1*ro(i+2) !231

      do j = is+nFD, ie-nFD
        d1r(j) = -1*ro(j-1)        +1*ro(j+1) !132
        d2r(j) =  1*ro(j-1)-2*ro(j)+1*ro(j+1) !232
      enddo ! j

      i = ie
      d1r(i-1) =  1*ro(i-2)-4*ro(i-1)+3*ro(i) !133
      d2r(i-1) =  1*ro(i-2)-2*ro(i-1)+1*ro(i) !233

    case( 2 )
      inv1 = 1./(24*h)
      inv2 = 1./(12*h*h)

      i = is
      d1r(i+0) = -50*ro(i) +96*ro(i+1) -72*ro(i+2)+32*ro(i+3) -6*ro(i+4) !151
      d2r(i+0) =  35*ro(i)-104*ro(i+1)+114*ro(i+2)-56*ro(i+3)+11*ro(i+4) !251

      d1r(i+1) = -6*ro(i)-20*ro(i+1)+36*ro(i+2)-12*ro(i+3)+2*ro(i+4) !152
      d2r(i+1) = 11*ro(i)-20*ro(i+1) +6*ro(i+2) +4*ro(i+3)-1*ro(i+4) !252

      do j = is+nFD, ie-nFD
        d1r(j) =  2*ro(j-2)-16*ro(j-1)         +16*ro(j+1)-2*ro(j+2) !153
        d2r(j) = -1*ro(j-2)+16*ro(j-1)-30*ro(j)+16*ro(j+1)-1*ro(j+2) !253
      enddo ! j

      i = ie
      d1r(i-1) = -2*ro(i-4)+12*ro(i-3)-36*ro(i-2)+20*ro(i-1) +6*ro(i) !154
      d2r(i-1) = -1*ro(i-4) +4*ro(i-3) +6*ro(i-2)-20*ro(i-1)+11*ro(i) !254

      d1r(i-0) =  6*ro(i-4)-32*ro(i-3) +72*ro(i-2) -96*ro(i-1)+50*ro(i) !155
      d2r(i-0) = 11*ro(i-4)-56*ro(i-3)+114*ro(i-2)-104*ro(i-1)+35*ro(i) !255

    case default
      if(o>0) write(o,'(A,I0)') 'derive_radial_function: nFD should be in [1,2] nFD = ', nFD
      stop 'derive_radial_function: derivative order should be in range [1,2]!'
    endselect ! nFD

    ! post-processing with the grid spacing
    d1r(is:ie) = d1r(is:ie)*inv1
    d2r(is:ie) = d2r(is:ie)*inv2

#ifdef DEBUG
    if( size(drdi,1) < ie ) stop 'grdchlh: error: not enough elements in DRDI'
#endif
    ! post-processing if the grid is exponential
    do j = is, ie
      d2r(j) = ( d2r(j) - d*d1r(j) )/drdi(j)**2
      d1r(j) = d1r(j)/drdi(j)
    enddo ! j

  endsubroutine ! derive_radial_function



      SUBROUTINE easypbe(icorr,  &
                        up,agrup,delgrup,uplap,&
                        dn,agrdn,delgrdn,dnlap,&
                        agr,delgr,lcor,lpot,   &
                        exlsd,vxuplsd,vxdnlsd, &
                        eclsd,vcuplsd,vcdnlsd, &
                        expbe,vxuppbe,vxdnpbe, &
                        ecpbe,vcuppbe,vcdnpbe)
      !----------------------------------------------------------------------
      !     easypbe---exchpbe
      !             --corpbe  --- pbecor2
      !----------------------------------------------------------------------
      ! easypbe is a driver for the pbe subroutines, using simple inputs
      ! k. burke, may 14, 1996.
      !----------------------------------------------------------------------

      ! .. Arguments ..
      integer, intent (in) :: icorr    ! selects different X-enhancement in exchpbe
      real,    intent (in) :: up       ! density (spin up)
      real,    intent (in) :: agrup    ! |grad up|
      real,    intent (in) :: delgrup  ! delgrup=(grad up).(grad |grad up|)  (in pw91: gggru)
      real,    intent (in) :: uplap    ! grad^2 up=laplacian of up           (in pw91: g2ru)
      real,    intent (in) :: dn       ! density (spin dn)
      real,    intent (in) :: agrdn    ! |grad down|
      real,    intent (in) :: delgrdn  ! delgrup=(grad dn).(grad |grad dn|)  (in pw91: gggru)
      real,    intent (in) :: dnlap    ! grad^2 dn=laplacian of dn           (in pw91: g2ru)
      real,    intent (in) :: agr      ! agr=|grad rho|,
      real,    intent (in) :: delgr    ! delgr=(grad rho).(grad |grad rho|)  (in pw91: gggr)
      integer, intent (in) :: lcor     ! flag to do correlation(=0=>don''t)
      integer, intent (in) :: lpot     ! flag to do potential  (=0=>don''t)
      real,    intent (out) :: exlsd            ! lsd exchange energy density
      real,    intent (out) :: vxuplsd,vxdnlsd  ! up/dn lsd exchange potential
      real,    intent (out) :: eclsd            ! lsd correlation energy density
      real,    intent (out) :: vcuplsd,vcdnlsd  ! up/dn lsd correlation potential
      real,    intent (out) :: expbe            ! note that : Exlsd=int d^3r rho(r) exlsd(r)
      real,    intent (out) :: vxuppbe,vxdnpbe  ! as above, but pbe quantities
      real,    intent (out) :: ecpbe            ! note that : Eclsd=int d^3r rho(r) eclsd(r)
      real,    intent (out) :: vcuppbe,vcdnpbe  ! as above, but pbe quantities

      ! .. local variables ..
      ! for exchange:
      real :: fk ! local fermi wavevector for 2*up=(3 pi^2 (2up))^(1/3)
      real :: s  ! dimensionless density gradient=|grad rho|/ (2*fk*rho)_(rho=2*up)
      real :: u  ! delgrad/(rho^2*(2*fk)**3)_(rho=2*up)
      real :: v  ! laplacian/(rho*(2*fk)**2)_(rho=2*up)
      ! for correlation:
      real :: zet    ! (up-dn)/rho
      real :: g      ! phi(zeta)
      real :: rs     ! (3/(4pi*rho))^(1/3)=local seitz radius=alpha/fk
      real :: sk     ! ks=thomas-fermi screening wavevector=sqrt(4fk/pi)
      real :: twoksg ! 2*ks*phi
      real :: t      ! correlation dimensionless gradient=|grad rho|/(2*ks*phi*rho)
      real :: uu     ! delgrad/(rho^2*twoksg^3)
      real :: rholap ! laplacian
      real :: vv     ! laplacian/(rho*twoksg^2)
      real :: ww     ! (|grad up|^2-|grad dn|^2-zet*|grad rho|^2)/(rho*twoksg)^2
      real :: ec     ! lsd correlation energy
      real :: vcup   ! lsd up correlation potential
      real :: vcdn   ! lsd down correlation potential
      real :: h      ! gradient correction to correlation energy
      real :: dvcup  ! gradient correction to up correlation potential
      real :: dvcdn  ! gradient correction to down correlation potential

      REAL :: eta,exdnlsd,exdnpbe,exuplsd,exuppbe,rho,rho2
      REAL, PARAMETER :: thrd = 1.e0/3.e0
      REAL, PARAMETER :: thrd2 = 2.e0*thrd
      REAL, PARAMETER :: pi = 3.1415926535897932384626433832795e0
      REAL, PARAMETER :: pi32 = 29.608813203268075856503472999628e0 ! 3 pi**2
      REAL, PARAMETER :: alpha=1.91915829267751300662482032624669e0 ! (9pi/4)**thrd

      rho2 = 2.e0*up

      !----------------------------------------------------------------------
      ! pbe exchange
      ! use  ex[up,dn]=0.5*(ex[2*up]+ex[2*dn]) (i.e., exact spin-scaling)
      ! do up exchange
      !----------------------------------------------------------------------

      IF (rho2 > 1e-18) THEN
          fk = (pi32*rho2)**thrd
          s = 2.e0*agrup/ (2.e0*fk*rho2)
          u = 4.e0*delgrup/ (rho2*rho2* (2.e0*fk)**3)
          v = 2.e0*uplap/ (rho2* (2.e0*fk)**2)

          CALL exchpbe(icorr,rho2,s,u,v,0,lpot,exuplsd,vxuplsd)
          CALL exchpbe(icorr,rho2,s,u,v,1,lpot,exuppbe,vxuppbe)

      ELSE

          exuplsd = 0.e0
          vxuplsd = 0.e0
          exuppbe = 0.e0
          vxuppbe = 0.e0

      ENDIF

! repeat for down
      rho2 = 2.e0*dn

      IF (rho2 > 1e-18) THEN

          fk = (pi32*rho2)**thrd
          s = 2.e0*agrdn/ (2.e0*fk*rho2)
          u = 4.e0*delgrdn/ (rho2*rho2* (2.e0*fk)**3)
          v = 2.e0*dnlap/ (rho2* (2.e0*fk)**2)

          CALL exchpbe(icorr,rho2,s,u,v,0,lpot,exdnlsd,vxdnlsd)
          CALL exchpbe(icorr,rho2,s,u,v,1,lpot,exdnpbe,vxdnpbe)

      ELSE

          exdnlsd = 0.e0
          vxdnlsd = 0.e0
          exdnpbe = 0.e0
          vxdnpbe = 0.e0

      ENDIF

! construct total density and contribution to ex
      rho = up + dn
      exlsd = (exuplsd*up+exdnlsd*dn)/rho
      expbe = (exuppbe*up+exdnpbe*dn)/rho

      IF (lcor == 0) RETURN
      !----------------------------------------------------------------------
      ! now do correlation
      !----------------------------------------------------------------------

      IF (rho < 1.e-18) RETURN

      zet = (up-dn)/rho

!9999+
!     eta: eta should not be smaller than 1.e-16.
!          otherwise will cause floating invalid.
!          if bigger, the last digit may differ
!          from the run by aix without this zet-guard.
      eta = 1.e-16
      zet = min(zet,1.0-eta)
      zet = max(zet,-1.0+eta)
!9999-

      g = ((1.e0+zet)**thrd2+ (1.e0-zet)**thrd2)/2.e0
      fk = (pi32*rho)**thrd
      rs = alpha/fk
      sk = sqrt(4.e0*fk/pi)
      twoksg = 2.e0*sk*g
      t = agr/ (twoksg*rho)
      uu = delgr/ (rho*rho*twoksg**3)
      rholap = uplap + dnlap
      vv = rholap/ (rho*twoksg**2)
      ww = (agrup**2-agrdn**2-zet*agr**2)/ (rho*rho*twoksg**2)

      CALL corpbe(icorr,rs,zet,t,uu,vv,ww,1,lpot,  &
                 ec,vcup,vcdn,h,dvcup,dvcdn)

      eclsd = ec
      ecpbe = ec + h
      vcuplsd = vcup
      vcdnlsd = vcdn
      vcuppbe = vcup + dvcup
      vcdnpbe = vcdn + dvcdn

      contains
      !----------------------------------------------------------------------
      !  pbe exchange for a spin-unpolarized electronic system
      !  k burke''s modification of pw91 codes, may 14, 1996
      !  modified again by k. burke, june 29, 1996, with simpler fx(s)
      !  inclusion of HSE function by M. Schlipf 2009
      !  removed HSE for juDFT DW
      !----------------------------------------------------------------------
      ! references:
      ! [a]j.p.~perdew, k.~burke, and m.~ernzerhof, submiited to prl, may96
      ! [b]j.p. perdew and y. wang, phys. rev.  b {\bf 33},  8800  (1986);
      !     {\bf 40},  3399  (1989) (e).
      ! [c] B.~Hammer, L.~B.~Hansen and J.~K.~Norskov PRB 59 7413 (1999)
      ! [d] J.~Heyd, G.~E.~Scuseria, M.~Ernzerhof, J. Chem. Phys. {\bf 118},
      !     8207 (2003)
      !----------------------------------------------------------------------

      SUBROUTINE exchpbe(icorr,rho,s,u,v,lgga,lpot,&
                        ex,vx)!,vx_sr)

      !USE m_hsefunctional, ONLY: aMix_HSE, calculateEnhancementFactor
      !USE m_constants,     ONLY: pimach
      !USE icorrkeys

      ! .. Arguments
      INTEGER, INTENT (IN)  :: icorr
      INTEGER, INTENT (IN)  :: lgga ! =0=>don''t put in gradient corrections, just lda
      INTEGER, INTENT (IN)  :: lpot ! =0=>don''t get potential and don''t need u and v
      REAL,    INTENT (IN)  :: rho  ! density
      REAL,    INTENT (IN)  :: s    ! abs(grad rho)/(2*kf*rho), where kf=(3 pi^2 rho)^(1/3)
      REAL,    INTENT (IN)  :: u    ! (grad rho)*grad(abs(grad rho))/(rho**2 * (2*kf)**3)
      REAL,    INTENT (IN)  :: v    ! (laplacian rho)/(rho*(2*kf)**2) [pw86(24)]
      REAL,    INTENT (OUT) :: ex,vx ! exchange energy per electron (ex) and potential (vx)

      ! new variables for the HSE functional
      !REAL,    INTENT (OUT) :: vx_sr ! short ranged part of potential
      !REAL :: kF,factor,fxhse
      !REAL :: dFx_ds,d2Fx_ds2,dFx_dkF,d2Fx_dsdkF

      ! .. local variables ..
      REAL :: um,uk,ul,exunif,fs,fss,fxpbe,p0,s2
      REAL :: xwu,css,dxwu,ddx               ! for wu-cohen
      REAL, PARAMETER :: teo = 10.e0/81.e0   ! for wu-cohen
      REAL, PARAMETER :: cwu = 0.0079325     ! for wu-cohen
      REAL, PARAMETER :: thrd=1.e0/3.e0
      REAL, PARAMETER :: thrd4=4.e0/3.e0
      REAL, PARAMETER :: ax=-0.738558766382022405884230032680836e0  ! -0.75*(3/pi)^(1/3)

      !----------------------------------------------------------------------
      ! uk, ul defined after [a](13)  (for icorr==7)
      !----------------------------------------------------------------------
      IF ((icorr == 7).OR.(icorr == 10).OR.(icorr == 11)) THEN
        uk=0.8040
      ELSEIF (icorr == 8) THEN
        uk=1.2450
      ELSEIF (icorr == 9) THEN    ! changed to [c]
        uk=0.8040
      !ELSEIF (icorr == icorr_pbe0   .OR. icorr == icorr_hse.OR. icorr == icorr_hseloc ) THEN
      !  uk=0.8040
      ELSE
        WRITE (6,'(//'' icorr is not correctly transferred. icorr='',i5)') icorr
        STOP
      ENDIF
      IF (icorr == 11) THEN  ! pbe_sol
        um=0.123456790123456d0
      ELSE
        um=0.2195149727645171e0
      ENDIF

      ul=um/uk
      !----------------------------------------------------------------------
      ! construct lda exchange energy density:
                                ! e_x[unif] = -0.75 * (3/pi)^(1/3) * rho^(4/3)
      !----------------------------------------------------------------------
      exunif = ax*rho**thrd
      IF (lgga == 0) THEN
          ex = exunif
          vx = ex*thrd4
          RETURN
      ENDIF
      !----------------------------------------------------------------------
      ! construct pbe enhancement factor
      !       e_x[pbe]=e_x[unif]*fxpbe(s)
      !       fxpbe(s)=1+uk-uk/(1+ul*s*s)                 [a](13)
      !----------------------------------------------------------------------
      s2 = s*s
!+gu
      !IF (icorr == 7 .OR. icorr ==  8 .OR. icorr == icorr_pbe0 .OR.  &
      !   icorr == icorr_hse .OR. icorr == icorr_hseloc ) THEN
      IF (icorr == 7 .OR. icorr ==  8 ) THEN
        ! calculate fxpbe
        p0 = 1.e0 + ul*s2
        fxpbe = 1e0 + uk - uk/p0
      ELSEIF (icorr == 9) THEN
        p0 = exp( - ul*s2 )
        fxpbe = 1e0 + uk * ( 1e0 - p0 )
      ELSEIF (icorr == 10) THEN
        css = 1+cwu*s2*s2
        xwu = teo*s2 + (um-teo)*s2*exp(-s2) + log(css)
        p0 = 1.e0 + xwu/uk
        fxpbe = 1e0 + uk - uk/p0
      ENDIF
!-gu
      ! Mixing of short and long range components
      !IF ( icorr == icorr_hse .OR. icorr == icorr_hseloc ) THEN
        ! ex = (1-a)ex,SR + ex,LR
        !    = (1-a)ex,SR + (ex,PBE - ex,SR)
        !    = (fxpbe - a*fxhse)*exunif
        ! Calculate the enhancement factor fxhse and its derivatives
        ! as integral over the exchange hole (cf. [d])
       ! kF = (3.0 * pimach()**2 * rho)**thrd
       ! CALL calculateEnhancementFactor(kF, s, fxhse, dFx_ds, d2Fx_ds2, &
       !                                dFx_dkF, d2Fx_dsdkF)
       ! ex = exunif * (fxpbe - aMix_HSE * fxhse )
      !ELSE
        ex = exunif*fxpbe
      !END IF
      IF (lpot == 0) RETURN
      !----------------------------------------------------------------------
      !  energy done. now the potential:
      !  find first and second derivatives of fx w.r.t s.
      !  fs=(1/s)*d fxpbe/ ds
      !  fss=d fs/ds
      !----------------------------------------------------------------------
!+gu
      !IF (icorr == 7 .OR. icorr == 8 .OR. icorr == icorr_pbe0 .OR.  &
      !   icorr == icorr_hse .OR. icorr == icorr_hseloc) THEN
      IF (icorr == 7 .OR. icorr == 8) THEN
        ! derivatives for the pbe part
        fs = 2.e0*uk*ul/ (p0*p0)
        fss = -4.e0*ul*s*fs/p0
      ELSEIF (icorr == 9) THEN
        fs = 2.e0*ul*p0
        fss = -2.e0*ul*s*fs
      ELSEIF (icorr == 10) THEN
        dxwu = 2*teo + 2*(um-teo)*exp(-s2)*(1-s2) + 4*cwu*s2/css
        fs = dxwu / (p0*p0)
        ddx = 4*s*((um-teo)*exp(-s2)*(s2-2)+2*cwu*(1-cwu*s2*s2)/css**2)
        fss = ( ddx - 2*s*dxwu*dxwu/(p0*uk) ) / (p0*p0)
      ENDIF
!-gu
      !----------------------------------------------------------------------
      ! calculate potential from [b](24)
      !----------------------------------------------------------------------
      vx = exunif* (thrd4*fxpbe- (u-thrd4*s2*s)*fss-v*fs)

      !IF ( icorr /= icorr_hse .AND. icorr /= icorr_hseloc ) RETURN

      !----------------------------------------------------------------------
      !     short ranged potential (HSE functional)
      !----------------------------------------------------------------------
      ! calculate fs and fss for the HSE functional
      ! where the 1st and 2nd derivative of Fx are known
      !fs    = dFx_ds / s
      !fss   = (d2Fx_ds2 - fs) / s
      !vx_sr = exunif * ( thrd4*fxhse - (u-thrd4*s2*s)*fss - v*fs  &
      !                + thrd*kF * ( dFx_dkF - d2Fx_dsdkF*s ) )

      ENDSUBROUTINE ! exchpbe

      !----------------------------------------------------------------------
      !  official pbe correlation code. k. burke, may 14, 1996.
      !----------------------------------------------------------------------
      ! references:
      ! [a] j.p.~perdew, k.~burke, and m.~ernzerhof,
      !     {\sl generalized gradient approximation made simple}, sub.
      !     to phys. rev.lett. may 1996.
      ! [b] j. p. perdew, k. burke, and y. wang, {\sl real-space cutoff
      !     construction of a generalized gradient approximation:  the pw91
      !     density functional}, submitted to phys. rev. b, feb. 1996.
      ! [c] j. p. perdew and y. wang, phys. rev. b {\bf 45}, 13244 (1992).
      !----------------------------------------------------------------------

      SUBROUTINE corpbe(&
                       icorr,rs,zet,t,uu,vv,ww,lgga,lpot,&
                       ec,vcup,vcdn,h,dvcup,dvcdn)
      !----------------------------------------------------------------------
      !  input: rs=seitz radius=(3/4pi rho)^(1/3)
      !       : zet=relative spin polarization = (rhoup-rhodn)/rho
      !       : t=abs(grad rho)/(rho*2.*ks*g)  -- only needed for pbe
      !       : uu=(grad rho)*grad(abs(grad rho))/(rho**2 * (2*ks*g)**3)
      !       : vv=(laplacian rho)/(rho * (2*ks*g)**2)
      !       : ww=(grad rho)*(grad zet)/(rho * (2*ks*g)**2
      !       :  uu,vv,ww, only needed for pbe potential
      !       : lgga=flag to do gga (0=>lsd only)
      !       : lpot=flag to do potential (0=>energy only)
      !  output: ec=lsd correlation energy from [a]
      !        : vcup=lsd up correlation potential
      !        : vcdn=lsd dn correlation potential
      !        : h=nonlocal part of correlation energy per electron
      !        : dvcup=nonlocal correction to vcup
      !        : dvcdn=nonlocal correction to vcdn
      !----------------------------------------------------------------------

      INTEGER, INTENT (IN)  :: icorr,lgga,lpot
      REAL,    INTENT (IN)  :: rs,zet,t,uu,vv,ww
      REAL,    INTENT (OUT) :: dvcdn,dvcup,ec,h,vcdn,vcup

      ! thrd*=various multiples of 1/3
      ! numbers for use in lsd energy spin-interpolation formula, [c](9).
      !      gam= 2^(4/3)-2
      !      fzz=f''(0)= 8/(9*gam)
      ! numbers for construction of pbe
      !      gamma=(1-log(2))/pi^2
      !      bet=coefficient in gradient expansion for correlation, [a](4).
      !      eta=small number to stop d phi/ dzeta from blowing up at
      !          |zeta|=1.

      REAL, PARAMETER :: thrd=1.e0/3.e0
      REAL, PARAMETER :: thrdm=-thrd
      REAL, PARAMETER :: thrd2=2.e0*thrd
      REAL, PARAMETER :: sixthm=thrdm/2.e0
      REAL, PARAMETER :: thrd4=4.e0*thrd
      REAL, PARAMETER :: gam=0.5198420997897463295344212145565e0
      REAL, PARAMETER :: fzz=8.e0/ (9.e0*gam)
      REAL, PARAMETER :: gamma=0.03109069086965489503494086371273e0
      REAL, PARAMETER :: eta=1.e-12
      !----------------------------------------------------------------------
      !----------------------------------------------------------------------
      ! find lsd energy contributions, using [c](10) and table i[c].
      ! eu=unpolarized lsd correlation energy
      ! eurs=deu/drs
      ! ep=fully polarized lsd correlation energy
      ! eprs=dep/drs
      ! alfm=-spin stiffness, [c](3).
      ! alfrsm=-dalpha/drs
      ! f=spin-scaling factor from [c](9).
      ! construct ec, using [c](8)

      REAL alfm,alfrsm,b,b2,bec,bg,comm,ecrs,eczet,ep,eprs,eu,eurs,    &
          f,fac,fact0,fact1,fact2,fact3,fact5,fz,g,g3,g4,gz,hb,hbt,hrs,&
          hrst,ht,htt,hz,hzt,pon,pref,q4,q5,q8,q9,rsthrd,rtrs,         &
          t2,t4,t6,z4,delt,bet

      IF (icorr == 11) THEN ! PBE_sol
        bet=0.046e0
      ELSE
        bet=0.06672455060314922e0
      ENDIF
      delt=bet/gamma

      rtrs = sqrt(rs)
      CALL pbecor2(0.0310907,0.21370,7.5957,3.5876,1.6382,0.49294,rtrs,eu,eurs)
      CALL pbecor2(0.01554535,0.20548,14.1189,6.1977,3.3662,0.62517,rtrs,ep,eprs)
      CALL pbecor2(0.0168869,0.11125,10.357,3.6231,0.88026,0.49671,rtrs,alfm,alfrsm)
      z4 = zet**4
      f = ((1.e0+zet)**thrd4+ (1.e0-zet)**thrd4-2.e0)/gam
      ec = eu* (1.e0-f*z4) + ep*f*z4 - alfm*f* (1.e0-z4)/fzz
      !----------------------------------------------------------------------
      !----------------------------------------------------------------------
      ! lsd potential from [c](a1)
      ! ecrs = dec/drs [c](a2)
      ! eczet=dec/dzeta [c](a3)
      ! fz = df/dzeta [c](a4)
      ecrs = eurs* (1.e0-f*z4) + eprs*f*z4 - alfrsm*f* (1.e0-z4)/fzz
      fz = thrd4* ((1.e0+zet)**thrd- (1.e0-zet)**thrd)/gam
      eczet = 4.e0* (zet**3)*f* (ep-eu+alfm/fzz) +fz* (z4*ep-z4*eu- (1.e0-z4)*alfm/fzz)
      comm = ec - rs*ecrs/3.e0 - zet*eczet
      vcup = comm + eczet
      vcdn = comm - eczet
      IF (lgga == 0) RETURN
      !----------------------------------------------------------------------
      !----------------------------------------------------------------------
      ! pbe correlation energy
      ! g=phi(zeta), given after [a](3)
      ! delt=bet/gamma
      ! b=a of [a](8)
      g = ((1.e0+zet)**thrd2+ (1.e0-zet)**thrd2)/2.e0
      g3 = g**3
      pon = -ec/ (g3*gamma)
      b = delt/ (exp(pon)-1.e0)
      b2 = b*b
      t2 = t*t
      t4 = t2*t2
      q4 = 1.e0 + b*t2
      q5 = 1.e0 + b*t2 + b2*t4
      h = g3* (bet/delt)*log(1.e0+delt*q4*t2/q5)
      IF (lpot == 0) RETURN
      !----------------------------------------------------------------------
      !----------------------------------------------------------------------
      ! energy done. now the potential, using appendix e of [b].
      g4 = g3*g
      t6 = t4*t2
      rsthrd = rs/3.e0
      gz = (((1.e0+zet)**2+eta)**sixthm- ((1.e0-zet)**2+eta)**sixthm)/3.e0
      fac = delt/b + 1.e0
      bg = -3.e0*b2*ec*fac/ (bet*g4)
      bec = b2*fac/ (bet*g3)
      q8 = q5*q5 + delt*q4*q5*t2
      q9 = 1.e0 + 2.e0*b*t2
      hb = -bet*g3*b*t6* (2.e0+b*t2)/q8
      hrs = -rsthrd*hb*bec*ecrs
      fact0 = 2.e0*delt - 6.e0*b
      fact1 = q5*q9 + q4*q9*q9
      hbt = 2.e0*bet*g3*t4* ((q4*q5*fact0-delt*fact1)/q8)/q8
      hrst = rsthrd*t2*hbt*bec*ecrs
      hz = 3.e0*gz*h/g + hb* (bg*gz+bec*eczet)
      ht = 2.e0*bet*g3*q9/q8
      hzt = 3.e0*gz*ht/g + hbt* (bg*gz+bec*eczet)
      fact2 = q4*q5 + b*t2* (q4*q9+q5)
      fact3 = 2.e0*b*q5*q9 + delt*fact2
      htt = 4.e0*bet*g3*t* (2.e0*b/q8- (q9*fact3/q8)/q8)
      comm = h + hrs + hrst + t2*ht/6.e0 + 7.e0*t2*t*htt/6.e0
      pref = hz - gz*t2*ht/g
      fact5 = gz* (2.e0*ht+t*htt)/g
      comm = comm - pref*zet - uu*htt - vv*ht - ww* (hzt-fact5)
      dvcup = comm + pref
      dvcdn = comm - pref

      ENDSUBROUTINE ! corpbe

      SUBROUTINE pbecor2(a,a1,b1,b2,b3,b4,rtrs,gg,ggrs)

      REAL, INTENT (IN)  :: a,a1,b1,b2,b3,b4,rtrs
      REAL, INTENT (OUT) :: gg,ggrs

      REAL :: q0,q1,q2,q3
      q0 = -2.e0*a* (1.e0+a1*rtrs*rtrs)
      q1 = 2.e0*a*rtrs* (b1+rtrs* (b2+rtrs* (b3+b4*rtrs)))
      q2 = log(1.e0+1.e0/q1)
      gg = q0*q2
      q3 = a* (b1/rtrs+2.e0*b2+rtrs* (3.e0*b3+4.e0*b4*rtrs))
      ggrs = -2.e0*a*a1*q2 - q0*q3/ (q1* (1.e0+q1))

      ENDSUBROUTINE ! pbecor2

      ENDSUBROUTINE ! easypbe

#ifdef EXTENDED
!+ extended

  status_t function test( )
    write(*,*,iostat=test) __FILE__,' no module test implemented!'
  endfunction ! test

!- extended
#endif

endmodule ! gga_tools

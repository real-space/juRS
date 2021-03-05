#ifdef DEBUG_ALL
#define DEBUG
#endif

! #define DEBUG
! #define FULL_DEBUG

!!! NONSRA solves the Schro:dinger equation rather than the Scalar-Relativic Approximation
! #define NONSRA

!!! uses the relativistic mass without the linearization of the sqrt-function
! #define SRA_SQRT

!! @author Paul Baumeister
!! @version 4.04
!!
!! integrates the radial SRA equation inwards
!! and outwards, solves the eigenvalue problem
!! with the shooting method
module radial_integrator
! use type_rgrid, only: rgrid
implicit none
  private !! default for this module namespace
  character(len=*), parameter, private :: sym = 'rINT' !! module symbol

  public :: shoot
  public :: shoot_at_energy
  public :: shooting_method
  public :: integrate_inwards
  public :: integrate_outwards
  public :: integrate_outwards_inhomogeneous
#ifdef EXTENDED
  public :: test
#endif

#ifdef DEBUG
  integer, parameter, private             :: o = 6 ! output to stdout
#else
  integer, private, save                  :: o = 0 ! no output at all
#endif

contains

  integer function shoot_at_energy( enn, ell, g, Z, V, E, rf, r2rho ) &
  result( ist )
  use type_rgrid, only: rgrid
  implicit none
    ! parameter
    character(len=*), parameter     :: fun = ' shoot_at_energy: '
    character(len=*), parameter     :: ib2char(-1:+1) = (/'lower','midle','upper'/)
    integer, parameter              :: IN=1, OUT=2
    real, parameter                 :: DEF_THRES = 1.E-12
    integer, parameter              :: DEF_MAXIT = 9999
    ! arguments
    integer, intent(in)             :: enn !! energy quantum number
    integer, intent(in)             :: ell !! angular momentum q-number
    type(rgrid), intent(in)         :: g !! grid descriptor
    real, intent(in)                :: Z !! core charge
    real, intent(in)                :: V(0:) !! local potential (Hartree) without -Z/r
    real, intent(in)                :: E !! energy value (Hartree)
    real, intent(out)               :: rf(0:) !! wave function
    real, intent(out)               :: r2rho(0:) !! density of that wave function
    ! local vars
    integer                         :: nnn, nno
    real                            :: knk

#ifdef DEBUG
    if( enn < 1 ) stop 'rINT shoot_at_energy: ENN < 1 unphysical.'
    if( ell >= enn ) stop 'rINT shoot_at_energy: ELL must be < ENN'
#endif
    nno = enn-1-ell ! number of nodes requested
    knk = shoot( Z, ell, g, V, E, rf, nnodes=nnn, r2rho=r2rho )
    if(o>0) write(o,'(2I3,9ES10.2)') nno, nnn, E, knk

#ifdef DEBUG
    if( nnn /= nno ) then ! number of nodes incorrect
      if(o>0) write(o,'(3A,2I3,9ES10.2)') sym, fun, 'number of nodes wrong   enn-1, nnn, E, kink', nno, nnn, E, knk
    endif ! nnn
#endif
    ist = nnn-nno

  endfunction shoot_at_energy



  integer function shooting_method( enn, ell, g, Z, V, E, rf, r2rho, &
                                    maxiter, threshold ) &
  result( ist )
  use type_rgrid, only: rgrid
  implicit none
    ! parameter
    character(len=*), parameter     :: fun = ' shooting_method: '
    character(len=*), parameter     :: ib2char(-1:+1) = (/'lower','midle','upper'/)
    integer, parameter              :: IN=1, OUT=2
    real, parameter                 :: DEF_THRES = 1.E-15
    integer, parameter              :: DEF_MAXIT =  999
    ! arguments
    integer, intent(in)             :: enn !! energy quantum number
    integer, intent(in)             :: ell !! angular momentum q-number
    type(rgrid), intent(in)         :: g !! grid descriptor
    real, intent(in)                :: Z !! core charge
    real, intent(in)                :: V(0:) !! local potential (Hartree) without -Z/r
    real, intent(inout)             :: E !! energy eigenvalue (Hartree)
    real, intent(out)               :: rf(0:) !! wave function
    real, intent(out)               :: r2rho(0:) !! density of that wave function
    integer, intent(in), optional   :: maxiter !! maximum number of iterations (default 999)
    real, intent(in), optional      :: threshold !! threshold for eigenvalue convergence (default 10^-12)
    ! local vars
    real                            :: max_dE, Pi
    integer                         :: nnodes, nnn(-1:+1), ib, nno
    logical                         :: converged
    integer                         :: iit, maxit
    real                            :: res, thres
    logical                         :: run
    real                            :: pE, dE, kink, ene(-1:+1), knk(-1:1), mdE(-1:+1)

#ifdef DEBUG
    if( enn < 1 ) stop 'rINT shooting_method: ENN < 1 unphysical.'
    if( ell >= enn ) stop 'rINT shooting_method: ELL must be < ENN'
#endif

    Pi = acos(-1.)
    max_dE = 0.000125 * 0.5*( (Z/enn)**2 + 0.1 ) ! for jumps, if the number of nodes is wrong
    nno = enn-1-ell ! number of nodes requested

    maxit = DEF_MAXIT ; if( present( maxiter ) )   maxit = max( 0, maxiter )
    thres = DEF_THRES ; if( present( threshold ) ) thres = max( 1E-16, threshold )

    mdE = 0.01

    ib = 0
    ene(ib) = E
    knk(ib) = shoot( Z, ell, g, V, ene(ib), rf, nnodes=nnn(ib) )
    if(o>0) write(o,'(3I3,9ES10.2)') ib, nno, nnn(ib), ene(ib), knk(ib)

    do while( nnn(ib) /= nno ) ! while number of nodes incorrect
      dE = -(nnn(ib)-nno) * max_dE
      ene(ib) = ene(ib) + dE
      if(o>0) write(o,'(A,2I3,ES10.2)') 'number of nodes wrong, enn-1, nnn, dE', nno, nnn(ib), dE
      knk(ib) = shoot( Z, ell, g, V, ene(ib), rf, nnodes=nnn(ib) )
      if(o>0) write(o,'(3I3,9ES10.2)') ib, nno, nnn(ib), ene(ib), knk(ib)
    enddo ! while

    ene(-1:+1) = ene( 0)
    knk(-1:+1) = knk( 0)
    nnn(-1:+1) = knk( 0)

    do ib = -1, +1, 2 ! ib in {-1,+1}
      do while( ib*knk(ib) > 0. ) ! while sign of kink is incorrect
        dE = ib * mdE(ib)
        ene(ib) = ene(ib) + dE
        mdE(ib) = mdE(ib) * 1.1 ! growing exponentially
        if(o>0) write(o,'(2A,9ES10.2)') ib2char(ib), ' sign of kink wrong, dE, mdE', dE, mdE(ib)
        knk(ib) = shoot( Z, ell, g, V, ene(ib), rf, nnodes=nnn(ib) )
        if(o>0) write(o,'(3I3,9ES10.2)') ib, nno, nnn(ib), ene(ib), knk(ib)
      enddo ! while
    enddo ! ib in {-1,+1}


    iit = 0
    run = .true.
    do while( run )
      iit = iit+1

      pE = ene(0) ! store energy value as previous


      knk( 0) = shoot( Z, ell, g, V, ene( 0), rf, nnodes=nnn( 0) )
      if(o>0) write(o,'(I6,2I3,9ES16.6)') iit, nno, nnn( 0), ene( 0), knk( 0), dE

      if( knk( 0) > 0. ) then
        ene(-1) = ene( 0)
        knk(-1) = knk( 0)
        nnn(-1) = nnn( 0)
      else  ! knk( 0) > 0.
        ene(+1) = ene( 0)
        knk(+1) = knk( 0)
        nnn(+1) = nnn( 0)
      endif ! knk( 0) > 0.

      ! new energy in the middle of ene(-1) and ene(+1)
      ene( 0) = ( ene(+1) + ene(-1) )/2

      dE = ene( 0) - pE

      res = abs( dE ) ! calculate energy change

      converged = ( res <= thres )
      run = ( .not. converged ) .and. ( iit < maxit )
    enddo ! while( run )

    E = ene( 0)

    ! status
    if( converged ) then          ; ist = 0
    elseif( iit >= maxit ) then   ; ist = maxit
    elseif( nnodes /= nno ) then  ; ist = -33
    else                          ; ist = -66 ! unknown error
    endif ! converged

    if( ist >= 0 ) then
      kink = shoot( Z, ell, g, V, E, rf, nnodes, r2rho ) ! compute also r2rho
    endif

#ifdef FULL_DEBUG
    if(o>0) write(o,'(3A,F24.16,A,ES10.2,A,I6)') sym, fun, 'E=', E, ' dE=', res, ' nit=', iit
#endif
!     write(8,'(3A,F24.16,A,ES10.2,A,I6)') sym, fun, 'E=', E, ' dE=', res, ' nit=', iit
  endfunction shooting_method


#undef FULL_DEBUG

  real function shoot( Z, ell, rg, V, E, rf, nnodes, r2rho ) result( kink )
  use type_rgrid, only: rgrid
  implicit none
    ! parameter
    character(len=*), parameter     :: fun = ' shoot: '
    integer, parameter              :: IN=1, OUT=2
    integer, parameter              :: G=1, F=2 ! index of large and small component
    ! arguments
    real, intent(in)                :: Z !! core charge
    integer, intent(in)             :: ell !! angular momentum quantum-number
    type(rgrid), intent(in)         :: rg !! radial grid descriptor
    real, intent(in)                :: V(0:) !! local potential (Hartree)
    real, intent(in)                :: E !! energy eigenvalue (Hartree)
    real, intent(out)               :: rf(0:) !! radial wave function
    integer, intent(out)            :: nnodes !! number of nodes
    real, intent(out), optional     :: r2rho(0:) !! density of that wave function
    ! local vars
    real                            :: gf(G:F,0:ubound(rf,1),IN:OUT)
    real                            :: dg(IN:OUT), s
    integer                         :: irs

    ! integrate the Schrodinger equation or SRA equation inwards until
    ! the first extremum (maximum/minimum) is reached
    nnodes = integrate_inwards( rg, real(Z), ell, E, V, gf(:,:,IN), &
                                irstopped=irs, dg=dg(IN) )
#ifdef DEBUG
    if( dg(IN)  /= dg(IN)  ) stop 'rINT shoot: NaN in dg(IN)'
#endif

    ! integrate the Schrodinger equation or SRA equation outwards
    ! stopping at the extremum at r(ir)
    nnodes = nnodes + integrate_outwards( rg, real(Z), ell, E, V, gf(:,:,OUT), &
                                 irstop=irs, dg=dg(OUT) ) 
#ifdef DEBUG
!     if( any( dg /= dg ) ) stop 'rINT shoot: NaN in dg'
    if( dg(OUT) /= dg(OUT) ) stop 'rINT shoot: NaN in dg(OUT)'
!     if( dg(IN)  /= dg(IN)  ) stop 'rINT shoot: NaN in dg(IN)'
#endif

#ifdef FULL_DEBUG
  if(o>0) write(o,'(3A,I6,A,F10.6)') sym, fun, 'irs=', irs, ' r=', rg%r(irs)
  if(o>0) write(o,'(3A,ES10.2)') sym, fun, 'dg( IN)=', dg(IN)/gf(G,irs,IN)
  if(o>0) write(o,'(3A,ES10.2)') sym, fun, 'dg(OUT)=', dg(OUT)/gf(G,irs,OUT)
#endif

    rf(0:irs) = gf(G,0:irs,OUT)
    ! match & scale the tail
    s = gf(G,irs,OUT) / gf(G,irs,IN)
    rf(irs: ) = gf(G,irs:,IN) * s

    ! the step in first derivative of the wave function
    ! corresponds to an additional delta-function in the potential
    ! with the area:
    if( gf(G,irs,OUT) /= 0. ) then
      kink = ( dg(OUT)-dg(IN)*s )/gf(G,irs,OUT)
    else
      kink = ( dg(OUT)-dg(IN)*s )/( gf(G,irs,OUT) + 1. )
    endif

    if( present( r2rho ) ) then
      ! create partial density of this wave function
#ifdef NONSRA
      r2rho = rf(:)**2 ! no small component, no relativistic norm correction
#else
      r2rho = rf(:)**2 ! no small component, no relativistic norm correction
!       r2rho(0:irs) = rf(0:irs)**2 + gf(F,0:irs,OUT)**2
!       r2rho(irs: ) = rf(irs: )**2 + (s*gf(F,irs:,IN))**2
#endif
    endif ! present r2rho

  endfunction shoot




  integer function integrate_inwards( rg, Z, ell, E, V, &               ! in
                                      gf, &                             ! out
                                      valder, irstart, irstop, &        ! in
                                      dg, irstopped ) &                 ! out
  result( nnodes )
  use type_rgrid, only: rgrid, operator(.at.)
  implicit none
    ! parameters
    character(len=*), parameter           :: fun = ' integrate_inwards: '
    integer, parameter                    :: G=1, F=2 ! index for large and small component
    integer, parameter                    :: h=-1 ! negative step
    real, parameter                       :: itp163(-1:1) = (/-.125,.75,.375/) ! [-1 6 3]/8
    ! arguments
    type(rgrid), intent(in)   :: rg !! radial grid descriptor
    real, intent(in)          :: Z !! core charge
    integer, intent(in)       :: ell !! ell quantum number
    real, intent(in)          :: E !! energy (Hartree)
    real, intent(in)          :: V(0:) !! local potential V(r) (Hartree)

    real, intent(out)         :: gf(G:,0:)
    ! optional
    real, intent(in), optional      :: valder(1:2) !! value and derivative
    integer, intent(in), optional   :: irstart !! index to start from
    integer, intent(in), optional   :: irstop !! index to stop at
    real, intent(out), optional     :: dg !! derivative
    integer, intent(out), optional  :: irstopped !! index where it stopped
    ! local vars
    integer                   :: irsta, irsto
    real, pointer             :: r(:), dr(:)
    real                      :: vd(G:F)
    real                      :: llp1 ! =ell(ell+1)
    real                      :: ri12, dri12, Vi12 ! interpolated
    real                      :: a(G:F,1:4) ! Runge-Kutta
    real                      :: d(G:F,1:3) ! Adam''s Moulton multistep derivatives
    real                      :: b(G:F)     ! temp
    real                      :: s(G:F,G:F), det
!     real                      :: kap
    integer                   :: ir, i3

#ifdef NaN_SEARCH
    if( any(v/=v) ) stop 'rINT integrate_inwards: NaN in v'
    if( e/=e ) stop 'rINT integrate_inwards: NaN in e'
    if( z/=z ) stop 'rINT integrate_inwards: NaN in z'
#endif

    ! use the radial grid
    r => rg%r ; dr => rg%dr

    ! ell - quantum number
    llp1 = ell*(ell+1.)

    nnodes = 0 ! init

    irsto = 5 ;           if( present(irstop) )  irsto = max( irstop,  irsto )
    irsta = ubound(V,1) ; if( present(irstart) ) irsta = min( irstart, irsta )

    if( irsta <= irsto ) then
      if( present(dg) ) dg = 0.
      if( present(irstopped) ) irstopped = irsto
      return
    endif ! irstart <= irstop

    ! determine a start index beyond the classical return radius
    ! however, it should not start to far outside since numerical
    ! problems might occur
!     ir = minloc( abs( V(9:)-E ), 1 )
!     ri12 = rg .at. ir
!     ir = rg .at. ( 5. * ri12 )
!     irsta = min( irstart, irsta )
!     write(*,*) sym, fun, 'at start, ir =', ir, ' energy', e

!     ir = irsto ; do while( ( E - V(ir) ) * r(ir) > Z .and. ir < ubound(V,1) ) ; ir = ir+1 ; enddo
!     ! now ir is roughly the classical return radius
!     kap = 0.
!     do while( kap < 48. .and. ir < ubound(V,1) )
!       ir = ir+1
!       kap = kap + sqrt( V(ir) - Z/r(ir) - E ) * dr(ir)
!     enddo ! while kap < 48
!     irsta = min( ir, irsta )

    ir = rg .at. ( 5. * Z / ( abs(E) + 0.1 ) )
    irsta = min( ir, irsta )

    gf(:,irsta:) = 0. ! init

    ir = irsta
#ifdef FULL_DEBUG
  if(o>0) write(o,*) sym, fun, 'at start, ir =', ir, ' energy', e
#endif

    s = sra( llp1, Z, E, V(ir), r(ir), dr(ir) )

    if( present( valder ) ) then
      vd = valder
    else  ! present valder
      vd(G) = 1E-8
!       ! assume an exponentially decaying function exp(-kappa*r)
!       Ekin = E-(V(ir)-Z/r(ir))  ! ==> -1/2 kappa^2 = Ekin
!       vd(2) = 0. ; if( Ekin < 0. ) vd(2) = -sqrt(-2.*Ekin)
      vd(F) = -vd(G)!*max(1.,Z)
    endif ! present valder

#ifdef FULL_DEBUG
  if(o>0) write(o,'(3A,2ES16.6)') sym, fun, 'valu, slop', vd
#endif

    gf(G,ir) =   vd(G)
    gf(F,ir) = ( vd(F) * dr(ir) + s(F,F) * vd(G) ) / s(G,F)

    a(G,1) = vd(F) * dr(ir)
    a(F,1) = dot_product( s(:,F), gf(:,ir) )

#ifdef FULL_DEBUG
  if(o>0) write(o,'(3A,2ES16.6)') sym, fun, 'a1 =      ', a(:,1)
  if(o>0) write(o,'(3A,I6,2ES16.6)') sym, fun, 'ir =', ir, gf(:,ir)
#endif

!   ! / dg \      / -x  u \ / g \
!   ! |    | = dr |       | |   |
!   ! \ df /      \ -y  x / \ f /
!
!       phi = (e+zz/r-v(k1))*dr/c
!       u = dr*c + real(1-nonsra)*phi
!       x = -dr/r
!       y = -fllp1*x*x/u + phi
!       g(k1) = valu
!       f(k1) = (slop*dr+x*valu)/u
!       q = 1.0/sqrt(ea)
!       ag1 = slop*dr
!       af1 = x*f(k1) - y*g(k1)


    ! Runge-Kutta for 3 points:
!     use 4th order Runge-Kutta method to generate starting points
!
!     gf_{i-1} = gf_{i} + h/6 (dgf_i + 4*dgf_{i-1/2} + dgf_{i-1}) + o(h**4)
!
!     dg_{i}     =  ag1             
!     dg_{i+1/2} = (ag2 + ag3)/2   
!     dg_{i+1  } =  ag4
!
!     for inwards integration h = - 1

    do i3 = 1, 3

      ir = ir+h ! backward = inward

      ! interpolate r(ir+1/2), dr(ir+1/2) 
      ! and V(ir+1/2) quadratically
       ri12 = sum( itp163* r(ir-1:ir+1) )
      dri12 = sum( itp163*dr(ir-1:ir+1) )
       Vi12 = sum( itp163* V(ir-1:ir+1) )
! #ifdef FULL_DEBUG
!   if(o>0) write(o,'(3A,I6,A,ES16.6)') sym, fun, 'V(ir.5) =', ir, '.5', Vi12
!   if(o>0) write(o,'(3A,I6,A,ES16.6)')   sym, fun, 'V(ir  ) =', ir, '  ', V(ir)
! #endif

      s = sra( llp1, Z, E, Vi12, ri12, dri12 ) ! half step !!!

      b = gf(:,ir+1) - .5*a(:,1)
      a(:,2) = matmul( s, b )
      b = gf(:,ir+1) - .5*a(:,2)
      a(:,3) = matmul( s, b )

      s = sra( llp1, Z, E, V(ir), r(ir), dr(ir) )

      b = gf(:,ir+1) - 1.*a(:,3)
      a(:,4) = matmul( s, b )

      ! compute the wave function at the next point (ir)
      gf(:,ir) = gf(:,ir+1) - ( a(:,1)+2*a(:,2)+2*a(:,3)+a(:,4) ) / 6.

#ifdef FULL_DEBUG
  if(o>0) write(o,'(3A,2ES16.6)') sym, fun, 'a1 =      ', a(:,1)
  if(o>0) write(o,'(3A,2ES16.6)') sym, fun, 'a2 =      ', a(:,2)
  if(o>0) write(o,'(3A,2ES16.6)') sym, fun, 'a3 =      ', a(:,3)
  if(o>0) write(o,'(3A,2ES16.6)') sym, fun, 'a4 =      ', a(:,4)
  if(o>0) write(o,'(3A,I6,2ES16.6)') sym, fun, 'ir =', ir, gf(:,ir)
#endif

      ! count nodes of g
      if( sign(1.,gf(G,ir)) * sign(1.,gf(G,ir-h)) < 0.0 ) nnodes = nnodes+1

      ! store the derivatives for Adam''s multistep method
      d(:,4-i3) = matmul( s, gf(:,ir) )

      if( ir == irsto ) then
#ifdef FULL_DEBUG
  if(o>0) write(o,*) sym, fun, 'ir =', ir, ' i3 =', i3, ' irstop reached.'
#endif
        if( present(dg) ) dg = a(G,1)
        if( present(irstopped) ) irstopped = irsto
#ifdef DEBUG
!         if( dg /= dg ) stop 'rINT integrate_inwards: NaN in dg at 1..3'
#endif
        return
      endif ! ir == irstop

    enddo ! i3 = 1, 3

! #ifdef FULL_DEBUG
!   if(o>0) write(o,*) sym, fun, 'gradients(G)', d(G,:)
!   if(o>0) write(o,*) sym, fun, 'gradients(F)', d(F,:)
! #endif

    ! main loop

    do while( ir > irsto )

      ir  = ir+h ! backward = inward

#ifdef FULL_DEBUG
!   if(o>0) write(o,*) sym, fun, 'loop, ir =', ir
#endif

      s = sra( llp1, Z, E, V(ir), r(ir), dr(ir) )
! #ifdef DEBUG
!       if( any(s/=s) ) write(*,*) ir, s
!       if( any(s/=s) ) stop 'rINT integrate_inwards: NaN in s at ir...'
! #endif

      b = ( 24/h*gf(:,ir-h) +19*d(:,1) -5*d(:,2) + d(:,3)  ) / 9.
! #ifdef DEBUG
!       if( any(b/=b) ) write(*,*) ir, b
!       if( any(b/=b) ) stop 'rINT integrate_inwards: NaN in b at ir...'
! #endif

      ! determinant( 8/3h - s )
      det = (8./(3.*h)-s(G,G))*(8./(3.*h)-s(F,F)) - s(F,G)*s(G,F)
      ! special property of s: inv(8/3h-s) = (8/3h+s)/det(8/3h-s)
      gf(:,ir) = ( matmul( s, b ) + 8./(3.*h)*b )/det
! #ifdef DEBUG
!       if( any(gf/=gf) ) write(*,*) ir, det, gf(:,ir)
!       if( any(gf/=gf) ) stop 'rINT integrate_inwards: NaN in gf at ir...'
! #endif

#ifdef FULL_DEBUG
  if(o>0) write(o,'(3A,I6,2ES16.6)') sym, fun, 'ir =', ir, gf(:,ir)
!   if( ir == 990 ) stop 'rINT DEBUG line 315'
#endif

      ! shift the derivatives of g and f to next grid point
      d(:,2:3) = d(:,1:2)

      ! compute the new derivative at ir-1
      d(:,1) = matmul( s, gf(:,ir) )
! #ifdef DEBUG
!       if( any(d/=d) ) stop 'rINT integrate_inwards: NaN in d at ir...'
! #endif

      ! jump out of the loop if maximum/minimum of g is reached
      if( sign(1.,gf(G,ir)) * sign(1.,d(G,1)) >= 0. ) then
#ifdef FULL_DEBUG
  if(o>0) write(o,*) sym, fun, 'ir =', ir, ' extremum reached.'
#endif
        if( present( dg ) ) dg = d(G,1)
#ifdef DEBUG
!         if( dg /= dg ) stop 'rINT integrate_inwards: NaN in dg at ir...'
#endif
        if( present( irstopped ) ) irstopped = ir
        return
      endif ! g * derivative of g >= 0.

    enddo ! while  ir > irstop

    ! store derivative at the cut radius to evaluate the eigen value

    if( present(irstopped) ) irstopped = max( ir, irsto )
    if( present( dg ) ) dg = d(G,1)
#ifdef DEBUG
!         if( dg /= dg ) stop 'rINT integrate_inwards: NaN in dg at end'
#endif
  endfunction integrate_inwards



  integer function integrate_outwards( rg, Z, ell, e, V, &          ! in
                                       gf, irstop, dg ) &           ! out
  result( nnodes )
  use type_rgrid, only: rgrid
  implicit none
    ! parameters
    character(len=*), parameter           :: fun = ' integrate_outwards: '
    integer, parameter                    :: G=1, F=2
    integer, parameter                    :: IRSTA=0
    integer, parameter                    :: h= 1 ! positive step
    ! arguments
    type(rgrid), intent(in)         :: rg !! radial grid descriptor
    real, intent(in)                :: Z  !! core charge
    integer, intent(in)             :: ell !! ell quantum number
    real, intent(in)                :: E !! energy (Hartree)
    real, intent(in)                :: V(0:) !! local potential  (Hartree)

    real, intent(out)               :: gf(G:,0:) !! large and small component
    integer, intent(in), optional   :: irstop !! stop index
    real, intent(out), optional     :: dg !! derivative
    ! local vars
    real, pointer             :: r(:), dr(:)
    real                      :: llp1 !  ell*(ell+1)
    real                      :: d(G:F,1:3) ! Adam''s Moulton multistep derivatives
    real                      :: b(G:F)   ! temp
    real                      :: s(G:F,G:F), det

    real                      :: rpow, ps(G:F,-1:10)
    integer                   :: ip, ir, i3, irsto

    r => rg%r ; dr => rg%dr

#ifdef NaN_SEARCH
    if( any(v/=v) ) stop 'rINT integrate_outwards: NaN in v'
    if( e/=e ) stop 'rINT integrate_outwards: NaN in e'
    if( z/=z ) stop 'rINT integrate_outwards: NaN in z'
#endif

#ifdef FULL_DEBUG
    if( size( gf, 2 ) < 5 ) stop 'rINT integrate_outwards: dim #2 of GF must be at least 5'
#endif

    nnodes = 0 ! init

    ! ell - quantum number
    llp1 = real( ell * (ell+1) )

    irsto = ubound(V,1) ; if( present( irstop ) ) irsto = min( irsto, irstop )

#ifdef FULL_DEBUG
  if(o>0) write(o,*) sym, fun, 'start'
#endif


    ! find the power series coeffients ps
    ! for the wave function at the origin
! #ifdef NONSRA
!     if( .true. ) then
! #else
!     if( Z <= 20. ) then
! #endif
!       ! non-relativistic
!       rpow = power_series( Z, ell, V(0)-E, ps )
!     else  ! Z <= 20. or NONSRA
    if( .true. ) then
      ! relativistic
      rpow = rel_power_series( Z, ell, V(0)-E, ps )
    endif ! Z <= 20. or NONSRA

!     if( Z > 0. ) then
!       if( ell > 0 ) then
!         rpow = power_series( Z, ell, V(0)-E, ps )
!       else  ! ell > 0
!         rpow = power_series_ell0( Z, V(0)-E, ps )
!       endif ! ell > 0 
!     else  ! Z > 0.
!       rpow = power_series_Z0( ell, V(0)-E, ps )
! !         rpow = power_series_ell0_Z0( V(0)-E, ps )
!     endif ! Z > 0.

!     rpow = power_series( Z, ell, V(0)-E, ps )


    ! evaluate the first 3 derivatives of g, f and the value of g, f
    ! at 3rd grid point by power series expansion

    ir = IRSTA ! = 0
    gf(:,ir) = 0.

    do i3 = 1, 3

      ir = i3

#ifdef FULL_DEBUG
  if(o>0) write(o,*) sym, fun, 'loop, i3=', i3, ' ir =', ir, ' r=', r(ir)
#endif
      !--------------------------------
      ! power series by Horner scheme
      !--------------------------------
      b = ps(:,10)
      do ip = 9, 0, -1
        b = b*r(ir) + ps(:,ip)
      enddo ! ip
      b = b * ( r(ir)**rpow )
      !--------------------------------
      gf(:,ir) = b

      ! count nodes of g
      if( sign(1.,gf(G,ir)) * sign(1.,gf(G,ir-h)) < 0.0 ) nnodes = nnodes+1

      s = sra( llp1, Z, E, V(ir), r(ir), dr(ir) )

      ! prepare the previous derivatives for ir=4
      d(:,4-i3) = matmul( s, gf(:,ir) )

    enddo ! i3

#ifdef FULL_DEBUG
  if(o>0) write(o,*) sym, fun, 'gradients(G)', d(G,:)
  if(o>0) write(o,*) sym, fun, 'gradients(F)', d(F,:)
#endif

    ! main loop
    do ir = 4, irsto, h ! forward = outward

#ifdef FULL_DEBUG
  if(o>0) write(o,*) sym, fun, 'loop, ir =', ir, ' r =', r(ir)
#endif

      s = sra( llp1, Z, E, V(ir), r(ir), dr(ir) )


      b = ( 24/h*gf(:,ir-h) +19*d(:,1) -5*d(:,2) + d(:,3)  ) / 9.

      ! determinant( 8/3h - s )
      det = (8./(3.*h)-s(G,G))*(8./(3.*h)-s(F,F)) - s(F,G)*s(G,F)
      ! special property of s: inv(8/3h-s) = (8/3h+s)/det(8/3h-s)
      gf(:,ir) = ( matmul( s, b ) + 8./(3.*h)*b )/det

      ! count nodes of g
      if( sign(1.,gf(G,ir)) * sign(1.,gf(G,ir-h)) < 0.0 ) nnodes = nnodes+1

      ! shift the derivatives of g and f to next grid point
      d(:,2:3) = d(:,1:2)

      ! compute the new derivative at ir-1
      d(:,1) = matmul( s, gf(:,ir) )

    enddo ! ir = ir, irstop

    ! store derivative at the cut radius to evaluate the eigenvalue
    if( present( dg ) ) dg = d(G,1)

  endfunction integrate_outwards















  !! this routine is a copy of the previous routine integrate_outwards
  !! with the difference, that no singular potential can be treated
  !! therefore Z=0 and an inhomogeneity is assumed, such that for the
  !! large component G
  !!
  !!  -1/2m d^2/dr^2 G(r) + ( ell(ell+1)/(2mr^2) + V(r) - E ) G(r) = p(r)
  !!
  !! where p(r) should behave like r^(ell+1) at the origin
  !!
  !! / dg \      / 1/r  m(r) \ / g \      / 0  \
  !! |    | = dr |           | |   | - dr |    |
  !! \ df /      \ 2W   -1/r / \ f /      \ 2p /
  !!
  integer function integrate_outwards_inhomogeneous( rg, p, ell, E, V, &          ! in
                                                     gf, irstop, dg ) &           ! out
  result( nnodes )
  use type_rgrid, only: rgrid
  implicit none
    ! parameters
    character(len=*), parameter           :: fun = ' integrate_outwards: '
    integer, parameter                    :: G=1, F=2
    integer, parameter                    :: IRSTA=0
    integer, parameter                    :: h= 1 ! positive step
    real, parameter                       :: m0 = 1. ! rest mass
    real, parameter                       :: Z = 0. ! core charge = 0, only smooth potentials without singularity
    ! arguments
    type(rgrid), intent(in)         :: rg !! radial grid descriptor
    real, intent(in)                :: p(0:) !! inhomogeneous part
    integer, intent(in)             :: ell !! ell quantum number
    real, intent(in)                :: E !! energy (Hartree)
    real, intent(in)                :: V(0:) !! local potential (Hartree)

    real, intent(out)               :: gf(G:,0:) !! large and small component
    integer, intent(in), optional   :: irstop !! stop index
    real, intent(out), optional     :: dg !! derivative
    ! local vars
    real, pointer             :: r(:), dr(:)
    real                      :: llp1 !  ell*(ell+1)
    real                      :: d(G:F,1:3) ! Adam''s Moulton multistep derivatives
    real                      :: b(G:F)   ! temp
    real                      :: s(G:F,G:F), det

    integer                   :: ir, i3, irsto

    r => rg%r ; dr => rg%dr

#ifdef NaN_SEARCH
    if( any(v/=v) ) stop 'rINT integrate_outwards_inhomogeneous: NaN in v'
    if( e/=e ) stop 'rINT integrate_outwards_inhomogeneous: NaN in e'
    if( z/=z ) stop 'rINT integrate_outwards_inhomogeneous: NaN in z'
#endif

#ifdef FULL_DEBUG
    if( size( gf, 2 ) < 5 ) stop 'rINT integrate_outwards_inhomogeneous: dim #2 of GF must be at least 5'
#endif

    nnodes = 0 ! init

    ! ell - quantum number
    llp1 = real( ell * (ell+1) )

    irsto = ubound(V,1) ; if( present( irstop ) ) irsto = min( irsto, irstop )

#ifdef FULL_DEBUG
  if(o>0) write(o,*) sym, fun, 'start'
#endif


    ! evaluate the first 3 derivatives of g, f and the value of g, f
    ! at 3 grid points from the inhomogeneous part

    ir = IRSTA ! = 0
    gf(:,ir) = 0.

    do i3 = 1, 3

      ir = i3

#ifdef FULL_DEBUG
  if(o>0) write(o,*) sym, fun, 'loop, i3=', i3, ' ir =', ir, ' r=', r(ir)
#endif
      ! the start behavior is according to the inhomogeneous part
      ! whereas the potential is assumed to be regular at the origin.
      if( abs( V(ir)-E ) < 1E-14 ) stop 'rINT integrate_outwards_inhomogeneous: Energy must differ from Potential value.'
      b(G) = p(ir)/( V(ir) - E ) ! the behaviour of p(ir) is assumed to be p0*r^(ell+1), too
      b(F) = 0. ! b(G) * m0 * ell / r(ir) ! ToDo: derive properly
      !--------------------------------
      gf(:,ir) = b

      ! count nodes of g
      if( sign(1.,gf(G,ir)) * sign(1.,gf(G,ir-h)) < 0.0 ) nnodes = nnodes+1

      s = sra( llp1, Z, E, V(ir), r(ir), dr(ir) )

      ! prepare the previous derivatives for ir=4
      ! inhomogeneity comes in                            here !!!
      d(:,4-i3) = matmul( s, gf(:,ir) ) - 2.* (/ 0., p(ir)*dr(ir) /)

    enddo ! i3

#ifdef FULL_DEBUG
  if(o>0) write(o,*) sym, fun, 'gradients(G)', d(G,:)
  if(o>0) write(o,*) sym, fun, 'gradients(F)', d(F,:)
#endif

    ! main loop
    do ir = 4, irsto, h ! forward = outward

#ifdef FULL_DEBUG
  if(o>0) write(o,*) sym, fun, 'loop, ir =', ir, ' r =', r(ir)
#endif

      s = sra( llp1, Z, E, V(ir), r(ir), dr(ir) )

!       ! inhomogeneity comes in                                                    here !!!
      b = ( 24/h*gf(:,ir-h) +19*d(:,1) -5*d(:,2) + d(:,3)  ) / 9.   -2.*(/ 0., dr(ir)*p(ir)*8./3. /)

      ! determinant( 8/3h - s )
      det = (8./(3.*h)-s(G,G))*(8./(3.*h)-s(F,F)) - s(F,G)*s(G,F)
      ! special property of s: inv(8/3h-s) = (8/3h+s)/det(8/3h-s)
      gf(:,ir) = ( matmul( s, b ) + 8./(3.*h)*b )/det


      ! count nodes of g
      if( sign(1.,gf(G,ir)) * sign(1.,gf(G,ir-h)) < 0.0 ) nnodes = nnodes+1

      ! shift the derivatives of g and f to next grid point
      d(:,2:3) = d(:,1:2)

      ! compute the new derivative at ir-1
      d(:,1) = matmul( s, gf(:,ir) )

    enddo ! ir = ir, irstop

    ! store derivative at the cut radius to evaluate the eigenvalue
    if( present( dg ) ) dg = d(G,1)

  endfunction integrate_outwards_inhomogeneous










  real function determinant_2x2( s ) result( d )
  ! the determinant of a 2x2 matrix
  implicit none
    real, intent(in)          :: s(2,2)
    d = s(1,1)*s(2,2) - s(1,2)*s(2,1)
  endfunction determinant_2x2


  real function power_series( Z, ell, V0mE, ps ) &
  result( rpow ) 
  implicit none
    real, parameter                       :: c = 1.0 ! fake light speed
    ! parameters
    character(len=*), parameter           :: fun = ' power_series_at_origin: '
    integer, parameter                    :: G=1, F=2
    ! arguments
    real, intent(in)                      :: Z
    integer, intent(in)                   :: ell
    real, intent(in)                      :: V0mE ! = local potential V(at r==0) - Energy (Hartree)
    real, intent(out)                     :: ps(G:F,-1:10)
    ! local vars
    integer                               :: ip

!---> a) small z or non-relativistically

    ps(:,:) = 0. ! init
    rpow = real(ell)
    ps(G,0) = 0. ! --> r^ell
    ps(G,1) = 1. ! --> r^(ell+1)
    do ip = 0, 8
      ps(G,ip+2) = 2.*( V0mE * ps(G,ip) - Z * ps(G,ip+1) ) &
                          /real( (ip+2+2*ell)*(ip+1) )
    enddo ! k
    ! => g(r) ~ r^ell*( r + ...)

    do ip = 0, 9
      ps(F,ip) = ps(G,ip+1)*real(ell+ip)*2
    enddo ! k

  endfunction power_series



  real function rel_power_series( Z, ell, V0mE, ps ) &
  result( s )
  ! relativistic 
  use constants, only: c => SPEED_OF_LIGHT
  implicit none
    ! parameters
    character(len=*), parameter           :: fun = ' rel_power_series: '
    integer, parameter                    :: G=1, F=2
    ! arguments
    real, intent(in)                      :: Z
    integer, intent(in)                   :: ell
    real, intent(in)                      :: V0mE ! = local potential V(at r==0) - Energy (Hartree)
    real, intent(out)                     :: ps(G:F,-1:10)
    ! local vars
    real                                  :: llp1, p21, p12
    real                                  :: alfa, beta, det, aa, bb
    integer                               :: k
!     real                                  :: VmE = 1006.25084142031/2.
!     p21 = ( VmE )/c

!---> b) relativistically, z>20

    llp1 = real(ell*(ell+1))
!     aa = -zz/c
    aa = -max(Z,1.)/(c)
    bb = llp1 - aa*aa
    p21 = ( V0mE )/c
!     p12 = c - p21
    p12 = 2.*c - p21
!     ps(:,-1) = 0.

    ! rpower: gf = ( sum_{i=0}^{10} ps(:,i) * r^(i+s) )
    s = sqrt( llp1+1.-aa*aa )

    ps(:,:) = 0. ! init
    ps(1,0) = 1.0
    ps(2,0) = (1.0-s)/aa
    do k = 1, 8
      alfa = p12*ps(2,k-1)
      beta = p21*aa*ps(1,k-1)
      if( ell /= 0 ) beta = beta - p12*aa*ps(1,k-1) - p12*p21*ps(1,k-2) + &
            (k+s)*p12*ps(2,k-1)
      det = k*(k+2*s)*aa
      ps(1,k) = (alfa*(k+s+1)*aa-aa*beta)/det
      ps(2,k) = (beta*(k+s-1)-bb*alfa)/det
    enddo ! k


  endfunction rel_power_series





  function schrodinger( llp1, Z, E, V, r, dr ) result( s )
  !! derivation operator for the Schr\""odinger equation
  !! The original Schr\""odinger equation
  !!      1             l(l+1)
  !!   - -- d^2g/dr^2 + ------ + [ V_Hxc(r) - Ze^2/r - E ]*g = 0
  !!     2m             2m r^2
  !! becomes (by reduction of order):
  !! / dg \      / 1/r   m  \ / g \
  !! |    | = dr |          | |   |
  !! \ df /      \ 2W  -1/r / \ f /
  !!
  !!            l(l+1)
  !! where  W = ------ + ( V_Hxc(r) - Ze^2/r - E )
  !!            2m r^2
  !!                   !!! Hartree units, m=1, e^2=1
  !!
  !! so       dg/dr = g/r + mf   <==>  mf = dg/dr - g/r
  !! and      df/dr = 2Wg - f/r
  !!
  !! then d^2g/dr^2 = dg/dr/r - g/r^2 + mdf/dr
  !!                = dg/dr/r - g/r^2 + m[2Wg - f/r]
  !!                = dg/dr/r - g/r^2 + 2mWg - [dg/dr - g/r]/r
  !!                = 2mWg
  !! <==> -1/(2m) d^2g/dr^2 + Wg = 0
  !!
!   use constants, only: c => SPEED_OF_LIGHT
!     real, parameter                 :: c = 137.0359895
  implicit none
    ! parameter
    real, parameter                 :: m0 = 1. ! rest mass
    real, parameter                 :: e2 = 1. ! e^2
    ! result
    real                            :: s(1:2,1:2)
    ! arguments
    real, intent(in)                :: llp1 ! = ell*(ell+1)
    real, intent(in)                :: Z ! core charge
    real, intent(in)                :: E ! energy  (Hartree)
    real, intent(in)                :: V ! V_Hxc  (Hartree)
    real, intent(in)                :: r
    real, intent(in)                :: dr
    s(1,1) =  dr/r    !  1/r
    s(2,2) = -s(1,1)  ! -1/r
    s(1,2) =  dr*m0   !  m (electron rest mass)
    s(2,1) =  dr*2.*( llp1 /(2.*m0*r*r) + V - Z*e2/r - E ) ! 2W

  endfunction schrodinger



  !! derivation operator for the
  !! scalar relativistic approximation of the Dirac equation
  !! Similar to the Schr\""odinger equation (above)
  !! / dg \      / 1/r  m(r) \ / g \
  !! |    | = dr |           | |   |
  !! \ df /      \ 2W   -1/r / \ f /
  !!
  !! but with the relativistic mass
  !!                   E-V_Hxc(r)+Ze^2/r
  !!  m(r) = m_0 ( 1 + ----------------- )
  !!                      2 m_0 c^2
  !! also affecting the potential term W
  !!              l(l+1)
  !! where  W = --------- + ( V_Hxc(r) - Ze^2/r - E )
  !!            2m(r) r^2
  !! Hartree units, m_0=1, e^2=1, c=1/alpha=137.036
  function sra( llp1, Z, E, V, r, dr ) result( s )
  use constants, only: c => SPEED_OF_LIGHT
  implicit none
    ! parameter
!     real, parameter                 :: c = 137.0359895 ! defined by module constants
    real, parameter                 :: m0 = 1. ! rest mass
    real, parameter                 :: e2 = 1. ! e^2
    ! result
    real                            :: s(1:2,1:2)
    ! arguments
    real, intent(in)                :: llp1 !! = ell*(ell+1)
    real, intent(in)                :: Z !! core charge == atomic number
    real, intent(in)                :: E !! energy (Hartree)
    real, intent(in)                :: V !! V_Hxc  (Hartree)
    real, intent(in)                :: r
    real, intent(in)                :: dr
#ifdef NONSRA
    s = schrodinger( llp1, Z, E, V, r, dr )
#else
    ! local vars
    real                            :: Ekin, mrel

    ! kinetic energy Ekin
    Ekin = ( E - ( V - Z*e2/r ) )
    ! relativistic mass mrel
#ifdef SRA_SQRT
    mrel = m0 * sqrt( 1. + Ekin/(m0*c*c) ) ! relativistic mass
#else
    mrel = m0 * ( 1. + 0.5*Ekin/(m0*c*c) ) ! approximation for m0*sqrt( 1 + Ekin/m0c^2 )
#endif

    s(1,1) =  dr/r     !          !! 1/r
    s(2,2) = -s(1,1)   ! = -dr/r  !! -1/r
    s(1,2) =  dr*mrel  !          !! m(r)
    s(2,1) =  dr*2.*( llp1/(2.*mrel*r*r) - Ekin ) !! 2W with m(r)

#endif
  endfunction ! sra

#ifdef EXTENDED
!+ extended

  integer function test( )
    write(*,*,iostat=test) __FILE__,' no module test implemented!'
  endfunction ! test

!- extended
#endif

endmodule ! radial_integrator

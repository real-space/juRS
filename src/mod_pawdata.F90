#include "config.h"
#include "pawxmlreader.h"

! #define DEBUG
! #define FULL_DEBUG

#define c_omp !$omp
! #define c_omp !!omp

#ifdef DEBUG
!!! remove comment from debug line
#define cDBG
#else
!!! comment line
#define cDBG !DBG
#endif

!! @author Andrea Nobile, Paul Baumeister


!! reads the PAW data from xml files and prepares the type(species)
module pawdata
  use configuration, only: o ! output unit, 0: no output
  use configuration, only: WARNING, ERROR
  use type_comp_desc, only: comp_desc
  use constants, only: Pi, sqrt4Pi
  use unitsystem, only: eV, eV_, Ang, Ang_
  use toolbox, only: operator(+)
#ifdef USE_PAWXMLREADER
  use mod_pawxmlreader, only: paw_atom, xc_type, xc_name
  use mod_pawxmlreader, only: SHAPE_GAUSS, SHAPE_BESSEL, SHAPE_SINC, SHAPE_EXP
  use mod_pawxmlreader, only: &
          pawxml_get_number_of_projectors, &
          pawxml_get_projector_rcut, &
          pawxml_get_state_ae_energy, &
          pawxml_get_state_occupation, &
          pawxml_get_kin_diff, &
          pawxml_get_generator_string, &
          pawxml_get_grid, &
          pawxml_get_rho_core, &
          pawxml_get_state_wave, &
          pawxml_get_v_zero, &
          pawxml_get_xc_name, &
          pawxml_get_xc_type, &
          pawxml_read, &
          pawxml_reader_cleanup
#endif
  use all_electron, only: write2file
implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: fun = ': ', sym = 'PAWdata' !! module symbol

  public :: read_pawdata
  public :: test

  !! ELLCHAR and ENNCHAR are only used for display, so we better allow some indices exceeding physical limits
  character, parameter :: ELLCHAR(-1:7) = (/'?','s','p','d','f','g','h','i','j'/)
  character(len=2), parameter :: ENNCHAR(0:4) = (/'? ','  ','* ','**','##'/)

  contains

  status_t function test( )
    write(*,*,iostat=test) __FILE__,' no module test implemented!'
  endfunction ! test

  real function generalized_number_of_nodes(nom, den) result(gnc)
    use constants, only: Pi
    real, intent(in) :: nom, den
    if (den < 0) then
        gnc = 0.5 - atan2(-nom, -den)/Pi
    else
        gnc = 0.5 - atan2(nom, den)/Pi
    endif
  endfunction ! generalized_number_of_nodes

  status_t function check_paw_setup( s, ndkin, xc_key ) result( ist )
  use type_species, only: species, ELLMAX, ENNMAX, I_TRU, I_SMT, I_PRJ, SPIN, I_FPRJ, I_VBAR, I_RHOC
  use type_rgrid, only: operator(.at.)
  use radial_potential, only: Hartree_potential, xc_functional
  use radial_integrator, only: integrate_outwards
  use radial_integrator, only: integrate_outwards_inhomogeneous
  use LAPACK, only: Solve_Ax_b
  use all_electron, only: write2file
  implicit none
    type(species), intent(in)       :: s
    real, intent(in)                :: ndkin(1:,1:,0:)
    integer, intent(in)             :: xc_key
    
    integer                         :: i, j, k, iln0, iln1, iln2, ell, n
    iounit_t                        :: u
    real, allocatable               :: rho_tru(:)
    real, allocatable               :: rho_smt(:), rho_aug(:)
    real, allocatable               :: rho(:,:)
    real, allocatable               :: cmp(:)
    real, allocatable               :: v_hartree_smt(:)
    real, allocatable               :: vxc_smt(:,:)
    real, allocatable               :: v_hartree_tru(:)
    real, allocatable               :: v_core(:)
    real, allocatable               :: vxc_tru(:,:)
    real, allocatable               :: aHm(:,:,:)
    real, allocatable               :: aSm(:,:,:)
    real                            :: nrm
    integer                         :: iq
    status_t                        :: ios
    integer                         :: ie
    real                            :: q_tru, q_smt, q_tru_core, q_smt_core
    real                            :: chd
    real                            :: ld_ene, ld_de
    real                            :: ld_emin=-5.0, ld_emax=1.0
    integer                         :: ld_steps=5000, nnan=0
    integer                         :: irstop
    real, allocatable               :: gf(:,:)
    real                            :: dg
    real                            :: logder(I_TRU:I_SMT,0:s%ellmax+2)
    real                            :: last_logder(I_TRU:I_SMT,0:s%ellmax+2)
    real, allocatable               :: logder_e(:,:,:)
    integer                         :: nnodes, nnd(0:3)
    real                            :: gfp(0:3,1:3)
    real                            :: mat(0:3,0:3)
    real                            :: mat2(0:3,0:3)
    real                            :: x(0:3), b(0:3)
    real                            :: valder(-1:3,0:1) ! 0: value, 1: 1st derivative
    string_t                        :: logderfile
    character(len=*), parameter     :: tru2char(I_TRU:I_PRJ) = (/'tru','smt','prj'/)
    real                            :: duality(4,4,0:s%ellmax)
    real                            :: occ
#ifndef USE_PAWXMLREADER
    integer, parameter :: SHAPE_GAUSS  = PAWXMLREADER_SHAPE_GAUSS, &
                          SHAPE_BESSEL = PAWXMLREADER_SHAPE_BESSEL, &
                          SHAPE_SINC   = PAWXMLREADER_SHAPE_SINC, &
                          SHAPE_EXP    = PAWXMLREADER_SHAPE_EXP
#endif

    allocate( rho_tru(0:s%g%imx), rho_smt(0:s%g%imx), rho_aug(0:s%g%imx), rho(0:s%g%imx,0:0) )
    allocate( vxc_smt(0:s%g%imx,0:0), vxc_tru(0:s%g%imx,0:0) )
    allocate( v_hartree_tru(0:s%g%imx), v_hartree_smt(0:s%g%imx) )
    allocate( v_core(0:s%g%imx), cmp(0:s%g%imx) )

    if(o>0) write(o,'(9A)') sym, fun, 'checking paw setup'
    !write(*,*) 'rcut = ',  s%rcut

    selectcase( s%cmp_desc%comp_type )
    case( SHAPE_GAUSS ) ; cmp(:) = exp(-(s%g%r(:)/s%cmp_desc%comp_rc)**2) 
    case( SHAPE_SINC )  ; cmp(:) = (sin(Pi*s%g%r(:)/s%cmp_desc%comp_rc)/Pi*s%g%r(:)/s%cmp_desc%comp_rc)**2
    case( SHAPE_EXP )   ; cmp(:) = exp(-(s%g%r(:)/s%cmp_desc%comp_rc)**s%cmp_desc%comp_lamb) 
    case( SHAPE_BESSEL ); stop 'cannot handle this shape function for charge compensator yet'
    case default        ; stop 'unknown handle for shape function for charge compensator'
    endselect ! comp_type
   
    nrm = dot_product( cmp, s%g%r2dr )
    cmp = cmp/nrm ! normalize the compensator function to a monopole moment of unity  
    !! build true and smooth density
    rho_tru(:) = 0.
    
    rho_smt(:) = sqrt4pi*s%rf(:,I_RHOC+I_SMT)
    rho_tru(:) = sqrt4pi*s%rf(:,I_RHOC+I_TRU)
    q_tru_core = sum(rho_tru(:)*s%g%r2dr(:))
    q_smt_core = sum(rho_smt(:)*s%g%r2dr(:))
    ist = write2file('dmp/rho_tru_core', rho_tru, grid=s%g%r(:))
    ist = write2file('dmp/rho_smt_core', rho_smt, grid=s%g%r(:))
    do i = 1, s%mln
      occ = s%occ_ln(1,i) + s%occ_ln(2,i)
      if(occ > 1e-6) then !! the state is occupied
        nrm = 1.0_8/sum(s%g%r2dr(:)*s%rwf(:,i,1,I_TRU)**2) !! this is important as we cannot guarantee that the partial waves are normalized
        rho_tru(:) = rho_tru(:) + (occ*nrm)*s%rwf(:,i,1,I_TRU)**2
        rho_smt(:) = rho_smt(:) + (occ*nrm)*s%rwf(:,i,1,I_SMT)**2
      endif ! state occupied
    enddo ! i
    !! now we have the complete rho
    q_tru = sum(rho_tru(:)*s%g%r2dr(:))
    q_smt = sum(rho_smt(:)*s%g%r2dr(:))
    ist = write2file('dmp/rho_tru', rho_tru, grid=s%g%r(:))
    ist = write2file('dmp/rho_smt', rho_smt, grid=s%g%r(:))

    if(o>0) write(o,'(3A,F0.9)') sym, fun, 'q_tru = ', q_tru
    if(o>0) write(o,'(3A,F0.9)') sym, fun, 'q_tru_core = ', q_tru_core
    if(o>0) write(o,'(3A,F0.9)') sym, fun, 'q_tru - q_tru_core = ', q_tru - q_tru_core
    if(o>0) write(o,'(3A,F0.9)') sym, fun, 'q_smt_core = ', q_smt_core
    if(o>0) write(o,'(3A,F0.9)') sym, fun, 'q_smt = ', q_smt
    if(o>0) write(o,'(3A,F0.9)') sym, fun, 'Delta0 -> q00 = ', (q_tru - q_smt - s%Z0)/sqrt4pi
    !ist = write2file('ae_core_on_gd', s%rf(:, I_RHOC+I_TRU), grid=s%g%r(:))

    chd = -s%Z0 + q_tru - q_smt
    rho_aug = rho_smt(:) + chd*cmp(:)
    ! check neutrality
    write(*,*) 'total smooth charge is ', sum(rho_aug*s%g%r2dr), q_tru - s%Z0
    ist = write2file('dmp/rho_aug', rho_aug, grid=s%g%r(:))

    ! generate the exchange correlation potential from the smooth total density 
    rho(:,0) = rho_smt(:)
    ist = xc_functional( spin=0, fpirho=rho, Vxc=vxc_smt, Exc=v_hartree_smt, key=xc_key, rgd=s%g%d, rgr=s%g%r, rgdrdi=s%g%dr ) 
    v_hartree_smt = 0.
    ! get V electrostatic of the smooth charge + compensator
    ist = Hartree_potential( s%g, rho=rho_aug, vH=v_hartree_smt )
 
    v_hartree_smt = v_hartree_smt + s%rf(:,I_VBAR)/sqrt4pi

    ! write(*,*) 'using xc key = ', xc_key
    rho(:,0) = rho_tru(:)
    ist = xc_functional( spin=0, fpirho=rho, Vxc=vxc_tru, Exc=v_hartree_tru, key=xc_key, rgd=s%g%d, rgr=s%g%r, rgdrdi=s%g%dr ) 
    v_hartree_tru = 0.
    ! get V electrostatic of the true charge 
    ist = Hartree_potential( s%g, rho=rho_tru(:), vH=v_hartree_tru )
   
   
    v_core(1:) = -s%Z0/s%g%r(1:)
    v_core(0) = v_core(1)

    ! ist = write2file('normal_v', v_smt(:), grid=s%g%r(:))
    ! write smooth and true potential on a filename
    ios = write2file('dmp/rho_smt_of_whatever', rho_smt, grid=s%g%r(:))
    ios = write2file('dmp/potential_of_whatever_tru', v_hartree_tru + v_core + vxc_tru(:,0), grid=s%g%r(:))
    ios = write2file('dmp/potential_of_whatever_smt', v_hartree_smt + vxc_smt(:,0), grid=s%g%r(:))

    allocate( aHm(4,4,0:s%ellmax), aSm(4,4,0:s%ellmax) )
  
    iln0 = 0
    do ell = 0, s%ellmax
      aHm(:,:,ell) = 0. ! init atomic Hamiltonian matrix correction
      aSm(:,:,ell) = 0. ! init atomic overlap matrix correction
      duality(:,:,ell) = 0. ! init the inner product between projectors and smooth partial waves
      do i = 1, s%nn(ell)
        iln1 = iln0 + i
        do j = 1, s%nn(ell)
          iln2 = iln0 + j
          aHm(j,i,ell) = .5*( ndkin(j,i,ell) + ndkin(i,j,ell) ) &
            + sum( s%rwf(:,iln2,1,I_TRU) * ( v_hartree_tru + v_core + vxc_tru(:,0) ) * s%rwf(:,iln1,1,I_TRU) * s%g%r2dr(:) ) &
            - sum( s%rwf(:,iln2,1,I_SMT) * ( v_hartree_smt     +      vxc_smt(:,0) ) * s%rwf(:,iln1,1,I_SMT) * s%g%r2dr(:) )
          aSm(j,i,ell) = sum( s%rwf(:,iln2,1,I_TRU) * s%rwf(:,iln1,1,I_TRU) * s%g%r2dr(:) ) &
                       - sum( s%rwf(:,iln2,1,I_SMT) * s%rwf(:,iln1,1,I_SMT) * s%g%r2dr(:) )
          duality(j,i,ell) = sum( s%rwf(:,iln2,1,I_PRJ) * s%rwf(:,iln1,1,I_SMT) * s%g%r2dr(:) )
        enddo ! j
        write(*,"(3A,I0,9F16.6)") sym, fun, '<prj|smt> l=', ell, duality(1:s%nn(ell),i,ell)
      enddo ! i
      iln0 = iln0 + s%nn(ell)
    enddo ! ell
    !write (*,*) 'dkin= ', ndkin(:,:,:)
!   write (*, '(/,A,9(/,4(4F16.6,/)))') 'H ', aHm(:,:,:)
!   write (*, '(/,A,9(/,4(4F16.6,/)))') 'S ', aSm(:,:,:)
    !! logarithmic derivative

    ld_de = (ld_emax - ld_emin)/real(ld_steps)
    irstop = s%g .at. 7.559 ! ( s%rcut*1.5 )  ! 4 Angstrom
    if(o>0) write(o,'(3A,F0.6,9A)') sym, fun, 'Logarithmic Derivatives at R = ', ( s%g .at. irstop )*Ang, Ang_
    
    allocate( logder_e(I_TRU:I_SMT,0:s%ellmax+2,0:ld_steps-1) )
    
c_omp parallel private(ie,gf,logder,ld_ene,iln0,ell,nnodes,gfp,n,i,j,k,nnd,valder,dg,mat,mat2,b,x)
    allocate( gf(1:2,0:s%g%imx) )
    gf = 0.
c_omp do schedule(static)
    do ie = 0, ld_steps-1
      ld_ene = ld_emin + ld_de*real(ie)
      logder = 0.
      iln0 = 0
      do ell = 0, s%ellmax+2
        ! for the local potential
        nnodes = integrate_outwards( s%g, s%Z0, ell, ld_ene, v_hartree_tru + vxc_tru(:,0), & ! in
                                     gf, irstop=irstop, &
                                     dg=dg ) ! derivative at end point

        logder(I_TRU,ell) = generalized_number_of_nodes(dg, gf(1,irstop))

        ! now for the non-local potential
        gfp = 0.
        n = 0 ; if( ell <= s%ellmax ) n = s%nn(ell) ! abbreviation

        do i = 0, n ! caution, i running from 0, because 0 is the homogeneous solution

          if( i > 0 ) then
            ! inhomogeneous solution
            nnd(i) = integrate_outwards_inhomogeneous( &
                        s%g, s%rwf(:,iln0+i,1,I_PRJ)*s%g%r(:), ell, ld_ene, v_hartree_smt + vxc_smt(:,0), & ! in
                        gf, irstop=irstop, &
                        dg=dg ) ! derivative at end point
          else  ! i > 0
            ! homogeneous solution
            nnd(i) = integrate_outwards( &
                        s%g, 0., ell, ld_ene, v_hartree_smt + vxc_smt(:,0), & ! in !! Z=0.
                        gf, irstop=irstop, &
                        dg=dg ) ! derivative at end point
          endif ! i > 0

          valder(i,1) = dg
          valder(i,0) = gf(1,irstop)

          ! find the scalar product of the inhomogeneous solution with the projectors
          do j = 1, n
            gfp(i,j) = sum( gf(1,:) * s%rwf(:,iln0+j,1,I_PRJ)*s%g%r(:) * s%g%dr )   
          enddo ! j

        enddo ! i

        mat = 0.
        do i = 1, n
          do k = 0, n
            do j = 1, n
              mat(i,k) = mat(i,k) + gfp(k,j) * ( aHm(j,i,ell) - ld_ene * aSm(j,i,ell) )
            enddo ! j
          enddo ! k
          mat(i,i) = mat(i,i) + 1.
        enddo ! i
        i = 0
          mat(i,i) = mat(i,i) + 1.
        !write(*,*) 'mat is ', mat(:,:) 
        b = 0. ; b(0) = 1. ! arbitrary scaling here, because it enters nominator and denominator
        mat2 = mat
        ios = Solve_Ax_b( mat(0:n,0:n), b(0:n), x(0:n) )
        if( ios /= 0 ) then
          if(o>0) write(o,'(3A,I0,A,F0.6,9A)') sym, fun, 'for ell = ',ell,' E = ',ld_ene*eV,eV_,' inversion failed!'
          x = 0 ; x(0) = 1. ! show the scattering of the local potential only
!         endif; if (0 == ell) then
!           write (*,*) ''
!           write (*,*) 'energy', ld_ene
!           write (*,*) 'mat ', mat2(0:n,0:n) ! show a copy of the matrix before Solve_Ax_b modified it
!           write (*,*) ' '
!           write (*,*) 'gfp ', gfp(0:n,1:n)
!           write (*,*) ' '
!           write (*,*) 'H ', aHm(:n,:n,ell)
!           write (*,*) 'S ', aSm(:n,:n,ell)
!           write (*,*) 'H-ES ', aHm(:n,:n,ell) - ld_ene*aSm(:n,:n,ell)
!           write (*,*) ' '
!           write (*,*) 'x ', x(0:n)
        endif
        !!write(*,*) 'got x ', x(0:n),' for ell ', ell
        valder(-1,0) = sum( x(0:n)*valder(0:n,0) )
        valder(-1,1) = sum( x(0:n)*valder(0:n,1) )

        logder(I_SMT,ell) = generalized_number_of_nodes(valder(-1,1), valder(-1,0))
        if( ell <= s%ellmax ) iln0 = iln0 + s%nn(ell)
      enddo ! ell
      
      logder_e(:,:,ie) = logder ! store in memory
    enddo ! ie
c_omp end do
    deallocate( gf )
c_omp end parallel    
     
    ! analyze the logarithmic derivatives and write to file
    write(unit=logderfile,fmt='(9(A,I0))') 'elem_',nint(s%Z0),'_logder'
    u = 29
    open(unit=u,file=logderfile,action='write',status='unknown',IOstat=ios)
    last_logder = -9.
    do ie = 0, ld_steps-1
      ld_ene = ld_emin + ld_de*real(ie)
      logder = logder_e(:,:,ie) ! load from memory
     
      if( all( logder == logder ) ) then

        ! if(u>0) write(u,'(F16.6,99(ES18.6,ES16.6))') E*eV, logder
        ! write to file
        ! use the inverse of tangens because atan(x) never gets bigger than Pi/2 (absolute)
        if(u>0) write(u,'(F16.6,88(F14.8,F12.8))') ld_ene*eV, logder
        do ell = 0, s%ellmax+2
          do iq = I_TRU, I_SMT
            if( logder(iq,ell) < last_logder(iq,ell) ) then ! indicate the pole to be between the energy points ==> -dE/2
              if(o>0) write(o,'(3A,F16.6,99A)') sym, fun, &
                'pole at', (ld_ene-.5*ld_de)*eV, eV_, ( '     ', i=0,ell ), tru2char(iq),' ', ellchar(ell)
            endif ! pole detected !
          enddo ! iq

        enddo ! ell
        last_logder = logder ! store information for the next energy point
      else  ! NaN
        nnan = nnan + count( logder /= logder ) ! count NaNs
      endif ! NaN
!       if(u>0) write(u,'(F16.6,99(ES18.6,ES16.6))') E*eV, logder(I_TRU,:)

    enddo ! ie
    close(unit=u)
    
    deallocate(logder_e)

    ist = diagonalize_pseudo( s, aHm, aSm, v_hartree_smt + vxc_smt(:,0) )
  endfunction ! check_paw_setup

  status_t function diagonalize_pseudo( s, aHm, aSm, v_smt ) result( ist )
    use type_species, only: species, ELLMAX, ENNMAX, I_TRU, I_SMT, I_PRJ, SPIN, I_FPRJ, I_VBAR, I_RHOC
    use type_rgrid, only: rgrid, rgrid_eqn
    use radial_interpolation, only: interpolate_on_grid
    use LAPACK, only: generalized_eig, solve_Ax_b
    use Laplacian, only: Laplace_coefficients
    

    type(species), intent(in)       :: s
    real, intent(in)                :: aHm(:,:,0:)
    real, intent(in)                :: aSm(:,:,0:)
    real, intent(in)                :: v_smt(:)
    
    integer, parameter              :: ngr_pts=380
    type(rgrid)                     :: unigr
    real, allocatable               :: Ham(:,:)
    real, allocatable               :: Ola(:,:)
    real, allocatable               :: V(:,:)
    real, allocatable               :: eig(:)
    real, allocatable               :: prj(:,:)
    real, allocatable               :: vsmt(:)

    real, allocatable               :: scalar_products_prj_sol(:)
    real, allocatable               :: prj_ovl(:,:)
    real, allocatable               :: sol_decomposition(:)
    real, allocatable               :: projected_sol(:)
    real                            :: sol_ortho_prj_part

    real, allocatable               :: Ham_prj(:,:)
    real, allocatable               :: Ola_prj(:,:)
    real, allocatable               :: V_prj(:,:)
    real, allocatable               :: eig_prj(:)
    real                            :: Laplace_coeffs(0:4) ! control the finite-difference order here

    integer                         :: ell, n, iln0, i, j, k, kk
    real                            :: dx 

    Laplace_coeffs = Laplace_coefficients(size(Laplace_coeffs)-1)
    
    if(o>0) write(o,'(/,9A)') sym, fun, 'diagonalizing spherical PAW Hamiltonian...'

    !! allocate ham and olap
    allocate( Ham(ngr_pts,ngr_pts), Ola(ngr_pts,ngr_pts), prj(0:ngr_pts,4), &
              vsmt( 0:ngr_pts), eig(ngr_pts), V(ngr_pts,ngr_pts), scalar_products_prj_sol(4), &
              prj_ovl(4,4), sol_decomposition(4), V_prj(4,4), eig_prj(4),  &
              projected_sol(ngr_pts), Ham_prj(4,4), Ola_prj(4,4), stat=ist )

    dx = s%g%r(s%g%imx)/ngr_pts
    if(o>0) write(o,'(3A,2(F0.3,2A),I0)') sym, fun, 'dx = ',dx*Ang,trim(Ang_), &
               ' = Rmax / nptr = ',s%g%r(s%g%imx)*Ang,trim(Ang_),' / ',ngr_pts

    Laplace_coeffs = (-0.5/(dx*dx))*Laplace_coeffs ! kinetic energy in finite-differences

    unigr = rgrid_eqn('r=d*i', ngr_pts+1, 0.0, dx, 0.0)
    
    vsmt(:) = 0.

    call interpolate_on_grid(s%g, v_smt(:), unigr, vsmt(:))
    !ist = write2file('normal_v', v_smt(:), grid=s%g%r(:))
    !ist = write2file('interp_v', vsmt(:), grid=unigr%r(:))
    
    iln0 = 0
    do ell = 0, s%ellmax + 1
      prj(:,:) = 0.
      n = 0; if (ell <= s%ellmax) n = min(s%nn(ell), 4) ! size of the projector space
      Ham(:,:) = 0.
      Ola(:,:) = 0.
      ! fill in prj
      do j = 1, n
        call interpolate_on_grid(s%g, s%rwf(:,iln0+j,1,I_PRJ), unigr, prj(:,j))
        prj(:,j) = prj(:,j)*unigr%r(:) ! multiply by r to work in r*R(r) coord
      enddo ! j
      
      do j = 1, ngr_pts
        Ham(j,j) = vsmt(j) + ell*(ell + 1)/(2.0*unigr%r(j)**2) + Laplace_coeffs(0)
      enddo ! j
    
      ! add kinetic energy operator (except middle term)
      do i = 1, size(Laplace_coeffs)-1
        do j = 1, ngr_pts-i
          Ham(j+i,j) = Laplace_coeffs(i)
          Ham(j,j+i) = Laplace_coeffs(i)
        enddo ! j
      enddo ! i
      
      do kk = 1, n
        do k = 1, n
          do i = 1, ngr_pts
            do j = 1, ngr_pts
              Ham(j,i) = Ham(j,i) + aHm(k,kk,ell)*prj(i,kk)*prj(j,k)*dx
              Ola(j,i) = Ola(j,i) + aSm(k,kk,ell)*prj(i,kk)*prj(j,k)*dx
            enddo ! j
          enddo ! i
        enddo ! k
      enddo ! kk

      do i = 1, ngr_pts
        Ola(i,i) = Ola(i,i) + 1.0
      enddo ! i


      if (n > 0) then
      
        ! from here on, projectors are L2-normalized
        do i = 1, n
          prj(:,i) = prj(:,i)/sqrt(sum(prj(:,i)**2)*dx)
        enddo

        ! before solving the generalized eigwert problem in real space, solve it in projectors space
        Ham_prj = matmul(transpose(prj(1:,1:n)),matmul(Ham(:,:), prj(1:,1:n)))
        Ola_prj = matmul(transpose(prj(1:,1:n)),matmul(Ola(:,:), prj(1:,1:n)))
        V_prj = Ham_prj

        if(o>0) write(o, '(3A,I0,9a)') sym, fun, "Hamiltonian in projector space for l=", ell, " in",trim(eV_)
        do i = 1, n
          if(o>0) write(o, '(4es16.6)') Ham_prj(i,1:n)*eV
        enddo ! i

        if(o>0) write(o, '(3A,I0)') sym, fun, "Overlap in projector space for l=", ell
        do i = 1, n
          if(o>0) write(o, '(4es16.6)') Ola_prj(i,1:n)
        enddo ! i

        ist = generalized_eig( V_prj(1:n,1:n), Ola_prj(1:n,1:n), eig_prj(1:n) )
        if(o>0 .and. ist .ne. 0) write(o,'(3A)') sym, fun, 'Solving in projectors space failed'

        do i = 1, n
          if(o>0) write(o,'(3A,I0,A,F12.6,2a,9(" ",f0.6))') sym, fun, 'Eigenstates in projector space for l=', ell, ' E=', eig_prj(i)*eV,trim(eV_),' coeffs', V_prj(:,i)
        enddo

      endif ! n > 0

        ! eig = 0. ; V = 0.
        V = Ham ! deep copy
        ist = generalized_eig( V, Ola, eig ) 

        if(o>0)         write(o,'(3A,I0,A,3F12.6,A)') sym, fun, 'lowest 3 PAW eigenvalues found for l=', ell, ' is', eig(1:3)*eV,trim(eV_)
        if(n>0 .and. o>0) write(o,'(3A,I0,A,9F12.6)') sym, fun, 'lowest input energy parameters for l=', ell, ' is', &
                              s%ene_ln(2,iln0+1:iln0+n)*eV
                              
      if (n > 0) then

        ! overlap matrix of normalized projectors
        do i = 1, n
          prj_ovl(i,i) = 1.0
          do j = i+1, n
            prj_ovl(j,i) = sum(prj(:,i)*prj(:,j))*dx
            prj_ovl(i,j) = prj_ovl(j,i)
          enddo ! j
          scalar_products_prj_sol(i) = sum(V(:,1)*prj(1:,i))*dx
        enddo ! i

        if(o>0) write(o, '(3A,I0)') sym, fun, 'overlaps of projectors for ell=', ell
        do i = 1, n
          if(o>0) write(o, '(20F12.6)') prj_ovl(i,1:n)
        enddo ! i

        if(o>0) write(o, '(3A,9(" ",f0.6))') sym, fun, 'scalar products with projectors for lowest eigenvalue', scalar_products_prj_sol(1:n)

        ist = solve_Ax_b(prj_ovl(1:n,1:n), scalar_products_prj_sol(1:n), sol_decomposition(1:n))

        if(o>0) write(o, '(3A,9(" ",f0.6))') sym, fun, 'decomposition with projectors for lowest eigvalue', sol_decomposition(1:n)
        projected_sol(:) = 0.0
        do i = 1, n
          projected_sol(:) = projected_sol(:) + sol_decomposition(i)*prj(1:,i)
        enddo !i
        if(o>0) write(o, '(3A,f0.3,a)') sym, fun, 'part of eigenstates orthogonal to projectors ', &
            (sum((V(:,1) - projected_sol(:))**2)*dx) / (sum(V(:,1)**2)*dx)*100, ' %'

        if(eig(1) < (1.1*s%ene_ln(2,iln0+1))) then
          if(o>0) write(o,*) s%name,' GHOST STATE on l=', ell, '!!!'
          if(o>0) write(o,'(9(4A,I0))') sym, fun, WARNING(0), 'ghost state on l=', ell, ' for ',trim(s%name)
          ist = write2file('dmp/ghost_state_ell'+ell, V(:,1), grid=unigr%r(1:))
          do j = 1, n
            ist = write2file('dmp/prj_ell'+ell+'_'+j, prj(1:,j), grid=unigr%r(1:))
          enddo ! j
        endif
        
        iln0 = iln0 + n
      endif ! n > 0
      if(o>0) write(o, '()') ! empty line
    enddo ! ell

  endfunction ! diagonalize_pseudo


  status_t function read_pawdata( filename, s, gridspacing, doublegrid, xc_functional, checkmode, MPIcomm, xml_format ) result( ist )
  ! reads PAW data generated by module all_electron
  use type_species, only: species, ELLMAX, ENNMAX, I_TRU, I_SMT, I_PRJ, SPIN, I_FPRJ, I_VBAR, I_RHOC
  use type_species, only: RWF_DIMENSION, RF_DIMENSION
  use type_species, only: prepare_species
  use type_rgrid, only: rgrid_create_exp, rgrid_eqn, operator(.at.)
  use type_bfun, only: operator(.at.)
  use type_item, only: operator(.in.)
  use density_functionals, only: XCfnDict => Dictionary
  use all_electron, only: write2file
  use configuration, only: o
  implicit none

    iounit_t, parameter             :: u = 8 !! file unit
    character(len=*), parameter     :: ENERGYUNIT(1:2) = (/'Hartree', 'Rydberg'/)

    character(len=*), intent(in)    :: filename
    type(species), intent(inout)    :: s
    real, intent(in)                :: gridspacing(1:3) !! (/coarse,dense,electrostatix/)
    integer, intent(in)             :: doublegrid(1:2) !! (/itp,nmesh/)
    character(len=*), intent(in)    :: xc_functional
    integer, intent(in)             :: checkmode
    integer, intent(in)             :: xml_format
    MPI_Comm, intent(in), optional  :: MPIcomm ! MPI communicator

#ifndef USE_PAWXMLREADER
    stop "mod_pawxmlreader is required! Ensure your C-compiled works!"
#else
   
    status_t                        :: lios, is
    integer                         :: ir, irmx, ell, enn, el2, en2, iln, iq, el, iline, nigno
    real                            :: Z, ecor, etot, e, occ(1:2), rcut(0:ELLMAX) !, r, q
    real, allocatable               :: rphi(:,:) ! (irmx,3)
    real, allocatable               :: rgrd(:,:) ! (irmx,2) ! r and dr

    character(len=80)               :: xml_filename
    character(len=*), parameter     :: xml_extension='.xml'
    type(paw_atom)                  :: pawatom

    integer                         :: i, j
    real                            :: ndkin(3,3,0:5)
    real                            :: mrcut, prcut
    integer                         :: xc_key
    real                            :: grid_a
    integer                         :: grid_istart
    integer                         :: grid_iend
    integer                         :: grid_n
    logical                         :: xml_file_exists

    character(len=32)               :: xcname, xctype ! do not change the len of the string!!! 
                                                  ! without changing pawxmlreader.c too 
    ist = 1 ! init as error
    
      xc_key = xc_functional .in. XCfnDict ! look up the XC-functional name in the dictionary
 
      write(unit=xml_filename, fmt='(9A)', iostat=lios) trim(adjustl(filename)), xml_extension
      if(o>0) write(o,'(9A)') sym, fun, 'read PAW data from file "', trim(xml_filename), '"'
      
      !write(*,*) 'calling pawxml_read from read_pawdata ', xml_filename
      inquire(file=xml_filename, exist=xml_file_exists, iostat=ist)
      if (ist /= 0) return
      if (.not. xml_file_exists) then
        if(o>0) write(o,'(9A)') sym, fun, 'PAW data file "',trim(xml_filename),'" cannot be located!'
        ist = -1
        return
      endif
      
      if(o>0) write(o,'(9A)') sym, fun, 'start to parse "', trim(xml_filename), '"'
      
      call pawxml_read(xml_filename)
      
      if(o>0) write(o,'(9A)') sym, fun, 'done parsing "', trim(xml_filename), '"'

      call pawxml_fill_paw_atom(pawatom)

      s%nve = pawatom%n_valence
      s%nce = pawatom%n_core
      s%eatom = pawatom%ae_energy_tot
      s%e_core_kin = pawatom%core_energy_kin
      s%es_energy = pawatom%ae_energy_es

      s%cmp_desc%comp_type = pawatom%shape_comp_type
      s%cmp_desc%comp_rc = pawatom%shape_comp_rc
      s%cmp_desc%comp_lamb = pawatom%shape_comp_lamb

      call pawxml_get_generator_string(s%generator_string)

      call pawxml_get_xc_name(xcname)
      call pawxml_get_xc_type(xctype)

      !write(*,*)  'got this xc functional ', xcname , xctype
      ! check if name corresponds to used functional 
      
      if( xcname == xc_functional ) then 
        !write (*,*) 'pseudo has the right xc functional!'
        if(o>0) write(o,'(9A)') sym, fun, 'pseudopotential xc and used xc match (', trim(xcname), ')'
      else 
        if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'pseudopotential xc functional (', trim(xcname), ') different from the one used! (', trim(xc_functional) , ')'  
      endif

      s%nn(:) = 0
      do ell = 0, ELLMAX
        s%nn(ell) = pawxml_get_number_of_projectors(ell)
      enddo ! ell
      s%nn = max(0,s%nn)

      s%ellmax = -1
      do ell = 0, ELLMAX
        if( s%nn(ell) > 0 ) s%ellmax = ell
      enddo ! ell
      
      s%mlm = (s%ellmax+1)**2
      s%mln = sum(s%nn)
      
      rcut(:) = 0.
      do ell = 0, s%ellmax
        rcut(ell) = pawxml_get_projector_rcut(ell)
      enddo 

      do ell = 0, s%ellmax
        do enn = 1, s%nn(ell)
          iln = iln_l_n( s%nn, ell, enn )
          s%ene_ln(1:2,iln) = pawxml_get_state_ae_energy( enn, ell )
          s%occ_ln(1:2,iln) = pawxml_get_state_occupation( enn, ell ) * 0.5 ! distribute equally onto both spins
        enddo ! enn
      enddo ! ell


    
      allocate( s%dkin(s%mln,s%mln), stat=is ) 
      s%dkin = 0. 
      
      !! fill dkin matrix
      do ell = 0, s%ellmax
        do enn = 1, s%nn(ell)
          do el2 = 0, s%ellmax
            do en2 = 1, s%nn(el2)
              s%dkin(iln_l_n( s%nn, ell, enn ),iln_l_n( s%nn, el2, en2 )) = pawxml_get_kin_diff(enn, ell, en2, el2)
            enddo ! en2
          enddo ! el2
        enddo ! enn
      enddo ! ell
      
      call pawxml_get_grid(grid_a, grid_istart, grid_iend, grid_n)
      !write(*,*) 'got grid params', grid_a, grid_istart, grid_iend, grid_n

      !write(*,*) 'creating grid for atom with Z =', s%Z0
      s%g = rgrid_create_exp( nint( 250.*sqrt(s%Z0+9.) ) ) ! set the radial grid with default values

      
      
      if( associated( s%rf ) ) deallocate( s%rf, stat=is ) 
      if( associated( s%rwf ) ) deallocate( s%rwf, stat=is ) 
      ! allocate the s%rf and s%rwf buffers
      allocate( s%rf(0:s%g%imx,RF_DIMENSION), stat=is ) ; s%rf = 0.
      allocate( s%rwf(0:s%g%imx,s%mln,SPIN,RWF_DIMENSION), stat=is ) ; s%rwf = 0.

      !! get core density from pwxml
      !!pawxml_get_rho_core(rho_smoot, rho_tru, grid, n)
      call pawxml_get_rho_core(s%rf(0,I_RHOC+I_SMT), s%rf(0,I_RHOC+I_TRU), s%g%r(0), s%g%imx+1, s%zcore)
      ist = write2file('dmp/rho_smt_import', s%rf(:,I_RHOC+I_SMT), grid=s%g%r(:))
      call pawxml_get_v_zero(s%rf(0,I_VBAR), s%g%r(0), s%g%imx+1)
      
      
      !is = write2file('rho_core_tru', s%rf(:, I_RHOC+I_TRU:I_RHOC+I_TRU), grid=s%g%r(:))
      do ell = 0, s%ellmax
        do enn = 1, s%nn(ell)
          iln = iln_l_n( s%nn, ell, enn )
          !!pawxml_get_state_wave(wave, grid, n, enn, ell, key)
          call pawxml_get_state_wave(s%rwf(0,iln,1,I_TRU), s%g%r(0), s%g%imx+1, enn, ell, AE_WAVE)
          call pawxml_get_state_wave(s%rwf(0,iln,1,I_SMT), s%g%r(0), s%g%imx+1, enn, ell, PSEUDO_WAVE)
          call pawxml_get_state_wave(s%rwf(0,iln,1,I_PRJ), s%g%r(0), s%g%imx+1, enn, ell, PROJ_WAVE)
            !is = write2file('partial_ae_wave', s%rwf(:,iln,1,I_TRU:I_PRJ), grid=s%g%r(:))

          !write(*,*) 'got state iln', iln
        enddo ! enn
      enddo ! ell
      
     !!ist =  write2file('partial_ae_wave', s%rwf(:,:,1,I_TRU), grid=s%g%r(:))
      !!stop
      s%rcut_chk = maxval( rcut(:) )
      s%rcut = 2.0*maxval( rcut(:) )
      s%nr = s%g .at. s%rcut
      
      prcut = 0.0
      mrcut = 0.5
      do ell = 0, s%ellmax
        do enn = 1, s%nn(ell)
          iln = iln_l_n( s%nn, ell, enn )
          do j=s%g%imx - 1, 0, -1
            if (abs(s%rwf(j,iln,1,I_PRJ)) > 1.0e-6) then 
              mrcut = s%g%r(j+1)
              exit
            endif
          enddo
          if(mrcut > prcut) then 
            prcut = mrcut
          endif
        enddo
      enddo
      s%prcut = prcut

      
      !write (*,*) 'got projector rcut = ', s%prcut, s%rcut
!      write (*,*) 'ELLMAX', ELLMAX
      call pawxml_reader_cleanup( ) ! free memory used by the parser

      do ell = 0, s%ellmax
        do enn = 1, s%nn(ell)
          do en2 = 1, s%nn(ell)
            ndkin(en2,enn,ell) = s%dkin(iln_l_n( s%nn, ell, en2 ),iln_l_n( s%nn, ell, enn ))
          enddo ! en2
        enddo ! enn
      enddo ! ell
      
      if( checkmode > 2 ) then 
        if(o>0) write(o, '(3A)') '', '', '', sym, fun, ' BEFORE filtering... '
        ist = check_paw_setup(s, ndkin, xc_key)
      endif ! checkmode

      ist = prepare_species( s, gridspacing, doublegrid, ndkin )

      if( checkmode > 1 ) then
        if(o>0) write(o, '(3A)') '', '', '', sym, fun, '  AFTER filtering... '
        ist = check_paw_setup(s, ndkin, xc_key)
      endif ! checkmode

      !!is = write2file('partial_ae_wave', s%rwf(:,5,1,I_TRU:I_PRJ), grid=s%g%r(:))
#endif
  endfunction ! read_pawdata
  

  integer function iln_l_n( nn, l, n ) result( iln )
      integer, intent(in)           :: nn(0:), l, n
      integer                       :: ell, enn

      iln = 0 ! init
      do ell = 0, ubound(nn,1)
        if( ell == l ) then
          do enn = 1, nn(ell)
            iln = iln+1
            if( enn == n ) return
          enddo !
        else  ! ell == l
          iln = iln+nn(ell) ! fast forward
        endif ! ell == l
      enddo ! iln
      iln = 0 ! does not exist
    endfunction ! iln_l_n

#if 0
  !! begin legacy code using a juRS-internal format and PAW data generated by mod_all_electrons.F90

  status_t function read_pawdata_p( filename, s, gridspacing, doublegrid, xc_functional, checkmode, MPIcomm, xml_format ) result( ist )
  ! reads PAW data generated by module all_electron
  use constants, only: Pi, sqrt4Pi
  use configuration, only: WARNING, ERROR, StopOnError, o
  use type_species, only: species, ELLMAX, ENNMAX, I_TRU, I_SMT, I_PRJ, SPIN, I_FPRJ, I_VBAR, I_RHOC
  use type_species, only: RWF_DIMENSION, RF_DIMENSION
  use type_element, only: show_electronic_configuration
  use type_rgrid, only: rgrid_create_exp, set,  operator(.at.)
  use type_bfun, only: operator(.at.)
  use unitsystem, only: Ang, Ang_
  use type_item, only: operator(.in.)
  use density_functionals, only: XCfnDict => Dictionary

  use pawdatafile, only: CommentChar !! = '#'
  ! keywords
  use pawdatafile, only: KEY_Zcor ! 'AtomicNumber=' !! Z
  use pawdatafile, only: KEY_Conf ! 'element' !! e%config
  use pawdatafile, only: KEY_XCFn ! 'ExchangeCorrelation=' !! kxc
  use pawdatafile, only: KEY_Eunt ! 'EnergyUnit=' !! ENERGYUNIT
  use pawdatafile, only: KEY_Etot ! 'TotalEnergy=' !! etot
  use pawdatafile, only: KEY_nVEs ! 'ValenceElectrons=' !! nve
  use pawdatafile, only: KEY_nWxp ! 'WeinertExponent=' !! nWexp
  use pawdatafile, only: KEY_Grid ! 'RadialGrid=' !! irmx ...
  use pawdatafile, only: KEY_nPrj ! 'Projectors=' !! nn(0:ellmax)
  use pawdatafile, only: KEY_RhoC ! 'CoreDensities' !! ...
  use pawdatafile, only: KEY_Ecor ! 'KineticCoreEnergy=' !! ecor
  use pawdatafile, only: KEY_Vbar ! 'ZeroPotential' !! ...
  use pawdatafile, only: KEY_rPhi ! 'PartialWaves' !! tru,smt,prj
  use pawdatafile, only: KEY_Rcut ! 'CutoffRadius=' !! Z
  use pawdatafile, only: KEY_Vers ! 'PawDataFileVersion=' !! Nversion
  use pawdatafile, only: KEY_dKin ! 'KineticEnergyDeficit=' !! dkin
  use pawdatafile, only: KEY_Warn ! 'Warning=' !! message appearing in the output
  use pawdatafile, only: KEY_Cmnt ! 'Comment=' !! some comment that appears in the output

  use MPIconst, only: MPI_COMM_SELF
  use MPItools, only: MPImaster, MPIparallel, MPIbcast0, MPInprocs
#ifdef USE_PAWXMLREADER
  use mod_pawxmlreader!, only: paw_atom, pawxml_get_number_of_projectors, pawxml_get_projector_rcut, ...
#else 
  use paw_XMLfile, only: read_paw_XML_file
#endif
  implicit none
    ! parameters
    iounit_t, parameter             :: u = 8 !! file unit
cDBG  character(len=*), parameter     :: fun = ' read(parallel): '
    character(len=*), parameter     :: ENERGYUNIT(1:2) = (/'Hartree', 'Rydberg'/)
    ! arguments
    character(len=*), intent(in)    :: filename
    type(species), intent(inout)    :: s
    real, intent(in)                :: gridspacing(1:3) !! (/coarse,dense,electrostatix/)
    integer, intent(in)             :: doublegrid(1:2) !! (/itp,nmesh/)
    character(len=*), intent(in)    :: xc_functional
    integer, intent(in)             :: checkmode
    integer, intent(in)             :: xml_format
    MPI_Comm, intent(in), optional  :: MPIcomm ! MPI communicator
    ! local vars
    MPI_Comm                        :: comm ! MPI communicator
    string_t                        :: keyword
    character(len=128)              :: line
    character(len=128)              :: config
    status_t                        :: lios, kios, ios(3), is
    integer                         :: ir, irmx, ell, enn, el2, en2, iln, iq, el, iline, nigno
    integer                         :: Nversion
    integer, parameter              :: NoPAWdataFile_version = -1
    real                            :: Z, ecor, etot, e, occ(1:2), rcut(0:ELLMAX) !, r, q
    real, allocatable               :: rphi(:,:) ! (irmx,3)
    real, allocatable               :: rgrd(:,:) ! (irmx,2) ! r and dr

    real                            :: eunit = 1.0 ! Hartree
    character(len=8)                :: eunt, xcf
    logical                         :: found(ENNMAX*(ELLMAX+1))
    logical                         :: p, m ! parallel master
    integer                         :: np_now
    integer, save                   :: np = 0 ! # of parallel tasks

#ifdef USE_PAWXMLREADER
    character(len=80)               :: xml_filename
    character(len=*), parameter     :: xml_extension='.xml'
    type(paw_atom)                  :: pawatom
    integer                         :: comp_type
    real                            :: comp_rc, comp_lamb
#endif
    integer                         :: i, j
    real                            :: ndkin(3,3,0:5)
    
#ifdef EXTENDED
    string_t                        :: fname
    iounit_t, parameter             :: uout = 81 !! out file unit
    character(len=*), parameter     :: Fext(I_TRU:I_FPRJ) = (/'.tpw','.spw','.prj','.fpr'/)
    character(len=*), parameter     :: Fpath = 'partial_waves/'
#endif
    
    character(len=32)               :: xcname, xctype ! do not change the len of the string!!! 
                                                      ! without changing pawxmlreader.c too 
    integer                         :: xc_key
    
    ist = 1 ! init as error
    if( filename == '' ) then
      if(o>0) write(o,'(9A)') sym, fun, 'received empty filename.'
      return ! error
    endif ! filename == ''

    if( present( MPIcomm ) ) then
      comm = MPIcomm
      m = MPImaster( comm ) ! true if i am the process with rank 0
      p = MPIparallel( comm ) ! true if there are others
      np_now = MPInprocs( comm )
      if( np_now /= np .and. p .and. o>0) write(o,'(3A,9(I0,A))') sym, fun, 'master reads PAW data for ',np_now,' processes.'
      np = np_now ! set save variable
    else  
      comm = MPI_COMM_SELF
      m = .true.  ! master
      p = .false. ! not parallel
      np = 1 ! set save variable
    endif
    
    xc_key = xc_functional .in. XCfnDict ! look up the XC-functional name in the dictionary

#ifdef USE_PAWXMLREADER
    if( xml_format > 0 ) then

      ! xml files use hartree units , eunit = 1.0
      eunit = 1.0
      write(unit=xml_filename, fmt='(9A)', iostat=lios) trim(adjustl(filename)), xml_extension
      !if(m) then
        
        !write(*,*) 'calling pawxml_read from read_pawdata ', xml_filename
        call pawxml_read(xml_filename)
        call pawxml_fill_paw_atom(pawatom)

        s%nve = pawatom%n_valence
        s%nce = pawatom%n_core
        s%eatom = pawatom%ae_energy_tot
        s%e_core_kin = pawatom%core_energy_kin

        comp_type = pawatom%shape_comp_type
        comp_rc = pawatom%shape_comp_rc
        comp_lamb = pawatom%shape_comp_lamb
  
        call pawxml_get_xc_name(xcname)
        call pawxml_get_xc_type(xctype)

        call pawxml_get_generator_string(s%generator_string)


        !write(*,*)  'got this xc functional ', xcname , xctype
        ! check if name corresponds to used functional 
        
        if( xcname == xc_functional ) then 
          !write (*,*) 'pseudo has the right xc functional!'
          if(o>0) write(o,'(9A)') sym, fun, 'pseudopotential xc and used xc match (', trim(xcname), ')'
        else 
          if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'pseudopotential xc functional (', trim(xcname), ') different from the one used! (', trim(xc_functional) , ')'  
        endif

        
        do ell = 0, ELLMAX
          s%nn(ell) = pawxml_get_number_of_projectors(ell)
        enddo ! ell
        s%nn = max(0,s%nn)

        s%ellmax = -1
        do ell = 0, ELLMAX
          if( s%nn(ell) > 0 ) s%ellmax = ell
        enddo ! ell
        
        s%mlm = (s%ellmax+1)**2
        s%mln = sum(s%nn)
        
        do ell = 0, s%ellmax
          rcut(ell) = pawxml_get_projector_rcut(ell)
        enddo 

        do ell = 0, s%ellmax
          do enn = 1, s%nn(ell)
            iln = iln_l_n( s%nn, ell, enn )
            s%ene_ln(1:2,iln) = pawxml_get_state_ae_energy( enn, ell ) / eunit
            s%occ_ln(1:2,iln) = pawxml_get_state_occupation( enn, ell ) * 0.5 ! distribute onto both spins
          enddo ! enn
        enddo ! ell
      
        allocate( s%dkin(s%mln,s%mln), stat=is ) 
        s%dkin = 0. 
        
        !! fill dkin matrix
        do ell = 0, s%ellmax
          do enn = 1,  s%nn(ell)
            do el2 = 0, s%ellmax
              do en2 = 1,  s%nn(el2)
                s%dkin(iln_l_n( s%nn, ell, enn ),iln_l_n( s%nn, el2, en2 )) = pawxml_get_kin_diff(enn, ell, en2, el2)
              enddo ! en2
            enddo ! el2
          enddo ! enn
        enddo ! ell
        
        !write(*,*) 'creating grid for atom with Z =', s%Z0
        s%g = rgrid_create_exp( nint( 250.*sqrt(s%Z0+9.) ) ) ! set the radial grid with default values

        
        if( associated( s%rf ) ) deallocate( s%rf, stat=is ) 
        if( associated( s%rwf ) ) deallocate( s%rwf, stat=is ) 
        ! allocate the s%rf and s%rwf buffers
        allocate( s%rf(0:s%g%imx,RF_DIMENSION), stat=is ) ; s%rf = 0.
        allocate( s%rwf(0:s%g%imx,s%mln,SPIN,RWF_DIMENSION), stat=is ) ; s%rwf = 0.

        !! get core density form pwxml
        !!pawxml_get_rho_core(rho_smoot, rho_tru, grid, n)
        call pawxml_get_rho_core(s%rf(0, I_RHOC+I_SMT), s%rf(0, I_RHOC+I_TRU), s%g%r(0), s%g%imx+1)
        call pawxml_get_v_zero(s%rf(0,I_VBAR), s%g%r(0), s%g%imx+1)
        
        ! check rho_core_tru to have the expected norm
        !convert to used convention
        !write(*,*) 'norm of core is ' , sqrt4pi*sum(s%rf(:, I_RHOC+I_TRU)*s%g%r2dr(:))
        !is = write2file('rho_core_smt', s%rf(:, I_RHOC+I_TRU:I_RHOC+I_SMT), grid=s%g%r(:))
        do ell = 0, s%ellmax
          do enn = 1, s%nn(ell)
            iln = iln_l_n( s%nn, ell, enn )
            !!pawxml_get_state_wave(wave, grid, n, enn, ell, key)
            call pawxml_get_state_wave(s%rwf(0,iln,1,I_TRU), s%g%r(0), s%g%imx+1, enn, ell, AE_WAVE)
            call pawxml_get_state_wave(s%rwf(0,iln,1,I_SMT), s%g%r(0), s%g%imx+1, enn, ell, PSEUDO_WAVE)
            call pawxml_get_state_wave(s%rwf(0,iln,1,I_PRJ), s%g%r(0), s%g%imx+1, enn, ell, PROJ_WAVE)
              !is = write2file('partial_ae_wave', s%rwf(:,iln,1,I_TRU:I_PRJ), grid=s%g%r(:))

            !! convert to jurs convention of r^-ell for projectors
            s%rwf(1:,iln,1,I_PRJ) = s%rwf(1:,iln,1,I_PRJ)*s%g%r(1:)**(-ell)
            s%rwf(0,iln,1,I_PRJ) = s%rwf(1,iln,1,I_PRJ)
            
          enddo ! enn
        enddo ! ell
        
        s%rcut = maxval( rcut(:) )
        s%nr = s%g .at. s%rcut
        s%nWexp = 3 
        
        ist = adjust_v_bar_for_weinert(s, comp_type, comp_rc, comp_lamb)

        call pawxml_reader_cleanup( ) ! free memory used by the parser

        do ell = 0, s%ellmax
          do enn = 1, s%nn(ell)
            do en2 = 1, s%nn(ell)
              ndkin(en2,enn,ell) = s%dkin(iln_l_n( s%nn, ell, en2 ),iln_l_n( s%nn, ell, enn ))
            enddo ! en2
          enddo ! enn
        enddo ! ell
        
        if( checkmode > 2 ) then 
          if(o>0) write(o,*) 'pseudopotential before filtering... '
          ist = check_paw_setup(s, ndkin, xc_key)
        endif ! checkmode

        ist = prepare_species( s, gridspacing, doublegrid, ndkin )

        if( checkmode > 1 ) then 
          do ell = 0, s%ellmax
            do enn = 1,  s%nn(ell)
              iln = iln_l_n( s%nn, ell, enn )
              do ir = 1, s%g%imx
                s%rwf(ir,iln,1,I_PRJ) = s%fprj(iln) .at. s%g%r(ir) 
              enddo ! ir 
              s%rwf(0,iln,1,I_PRJ) = s%rwf(1,iln,1,I_PRJ)
            enddo ! enn
          enddo ! ell

          if(o>0) write(o,*) 'pseudopotential AFTER filtering... '
          ist = check_paw_setup(s, ndkin, xc_key)
        endif ! checkmode
       
        !!is = write2file('partial_ae_wave', s%rwf(:,5,1,I_TRU:I_PRJ), grid=s%g%r(:))
      return 
    endif ! xml_format
#else
!       ! use the internal xml-reader in Fortran
!       ist = read_paw_XML_file( filename, s )
!       ist = prepare_species( s, gridspacing, doublegrid, ndkin )
!       return 
! 
!       stop 'no XML reader activated'
#endif

    ! open file
    if(m) open( unit=u, file=filename, status='old', action='read', iostat=ist )
    if(p) call MPIbcast0( ist, comm )
    if( ist /= 0 ) then 
      if(o>0) write(o,'(5A,I0)') sym, fun, 'unable to open "', trim(filename), '" for reading, ios = ', ist
      return ! error
    endif ! ist /= 0

    s%Z0  = real( s%iZ ) ! default
    etot  = 0.
    ecor  = 0.
    rcut  = 0.
    ios   = 0
    irmx  = 0
    iline = 0
    nigno = 0 ! number of ignored lines
    s%nn  = 0
    s%Nve = 0
    s%nWexp = 3 ! default
    s%mln   = 0
    found = .false.
    Nversion = NoPAWdataFile_version ! not a PAWdata file
    if( associated( s%rwf ) ) deallocate( s%rwf, stat=is ) 

      if(m) read( unit=u, fmt='(A)', iostat=lios ) line ! read  1st line of the file
      if(p) call MPIbcast0( lios, comm )                ! communicate the 1st line-IOstat
    do while( lios == 0 )
      if(p) call MPIbcast0( line, comm )                ! communicate the line (reading was successful)
      iline = iline+1 ! count up
      read( unit=line, fmt=*, iostat=kios ) keyword
      if( kios == 0 ) then

        if( keyword(1:1) /= CommentChar ) then
        !=========================================================================
        selectcase( keyword )
#ifndef USE_PAWXMLREADER
        case( '<?xml' )
          ! use the internal xml-reader in Fortran
          if(m) close( unit=u, iostat=ist ) ! close the file
          ist = read_paw_XML_file( filename, s )
          ist = prepare_species( s, gridspacing, doublegrid, ndkin )
          return 
#endif
        ! quantities
        case( KEY_Conf ) ! configuration string
          config = show_electronic_configuration( s%sym )
          if( line /= config ) then
            if(o>0) write(o,'(9A)') sym, fun, ERROR, 'electronic configuration missmatch for ', trim(s%name), ' (',trim(s%sym),'), found:'
            if(o>0) write(o,'(9A)') trim(line), '    in file "', trim(filename), '", but expected: '
            if(o>0) write(o,'(9A)') trim(config)
            if(o>0) write(o,'(9A)') ! empty line
            ist = 9 ; return ! error
          else  ! missmatch
            if(o>0) write(o,'(9A)') trim(line) ! show the configuration of the data set in the output
          endif ! missmatch

        case( KEY_XCFn ) ! type of xc functional
          read( unit=line, fmt=*, iostat=kios ) keyword, xcf
cDBG      if(o>0) write(o,'(9A)') sym, fun, 'exchange-correlation functional = "', trim(xcf), '"'
          if( xc_functional /= xcf ) then
            if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'XC functional expected "',trim(xc_functional),'" but found "',trim(xcf),'" for ', s%sym
          endif ! functional has a different abbreviation

        case( KEY_Eunt ) ! energy unit
          read( unit=line, fmt=*, iostat=kios ) keyword, eunt
          selectcase( eunt )
          case( 'Hartree' ) ; eunit = 1.0
          case( 'Rydberg' ) ; eunit = 2.0
          case default      ; eunit = 1.0
            if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'unknown energy unit "',trim(eunt),'" found, assume Hartree atomic units!'
          endselect ! eunt

        case( KEY_Zcor ) ! atomic number
          read( unit=line, fmt=*, iostat=kios ) keyword, Z
          selectcase( s%iZ )
          case( :0, 100: ) ! custom elements such as vacuum, etc.
            s%Z0 = Z  ! special case: modify the "identifier" of this species
            if( abs( Z - s%iZ ) > 1E-6 ) then
              if(o>0) write(o,'(6A,F0.3)') sym, fun, WARNING(0), 'special element ', s%sym, ' found, Z = ', Z
            endif
          case default ! regular behaviour 1:118
            ! s%Z0 == real( s%iZ )
            if( abs( Z - s%iZ ) > 1E-6 ) then
              if(o>0) write(o,'(3A,I0,A,F0.3)') sym, fun, 'ERROR! required Z = ', s%iZ, ', but found Z = ', Z
              ist = 118 ; return ! error
            endif ! Z
          endselect ! s%iZ

        case( KEY_Etot ) ! total energy
          read( unit=line, fmt=*, iostat=kios ) keyword, etot
          s%eatom = etot / eunit

        case( KEY_nVEs ) ! number of valence electrons
          read( unit=line, fmt=*, iostat=kios ) keyword, s%Nve

        case( KEY_nWxp ) ! Weinert exponent for the compensator
          read( unit=line, fmt=*, iostat=kios ) keyword, s%nWexp

        case( KEY_nPrj ) ! numbers of projectors
          read( unit=line, fmt=*, iostat=kios ) keyword, s%nn(0:)
          kios = 0 ! suppress warning
          s%nn = max(0,s%nn)
          ! determine the maximum ell for later loops only to run up to s%ellmax
          s%ellmax = -1
          do ell= 0, ELLMAX
            if( s%nn(ell) > 0 ) s%ellmax = ell
          enddo ! ell

          s%mlm = (s%ellmax+1)**2
          s%mln = sum(s%nn)
          if( s%mln < 1 .and. o>0) write(o,'(9A)') sym, fun, WARNING(0), 'no valence states for ', s%sym

        case( KEY_Rcut ) ! cutoff radii
          read( unit=line, fmt=*, iostat=kios ) keyword, rcut(0:)
          kios = 0 ! suppress warning

        case( KEY_Grid ) ! radial grid
          read( unit=line, fmt=*, iostat=kios ) keyword, irmx
          allocate( s%rf(0:irmx-1,RF_DIMENSION) ) ; s%rf = 0.
          allocate( rgrd(0:irmx-1,2) ) ; rgrd = 0.
          if(m) read( unit=u, fmt=*, iostat=ios(1) ) rgrd(:,1)     ! r(:)
          if(m) read( unit=u, fmt=*, iostat=ios(2) ) rgrd(:,2)  ! drdi(:)
          if(p) call MPIbcast0( ios(1:2), comm )
          if( any( ios(1:2) /= 0 ) ) then
            if(o>0) write(o,'(4A,I0,9A)') sym, fun, ERROR, 'expected 2 lines with ', irmx, ' numbers each for radial grid.'
            return ! error
          endif
          iline = iline+2 ! count up
          if(p) call MPIbcast0( rgrd, comm )

          ! create a type(rgrid)
          s%g = set( rgrd(:,1), rgrd(:,2) )
          deallocate( rgrd, stat=is )

        case( KEY_RhoC ) ! core densities
          if(m) read( unit=u, fmt=*, iostat=ios(1) ) s%rf(:,I_RHOC+I_TRU)
          if(m) read( unit=u, fmt=*, iostat=ios(2) ) s%rf(:,I_RHOC+I_SMT)
          if(p) call MPIbcast0( ios(1:2), comm )
          iline = iline+2 ! count up 2 lines
          if( any( ios(1:2) /= 0 ) ) then
            if(o>0) write(o,'(9A)') sym, fun, ERROR, 'reading core densities in "', trim(filename), '".'
            return ! error
          endif ! ios
          if(p) call MPIbcast0( s%rf(:,I_RHOC+I_TRU:I_RHOC+I_SMT), comm )
          ! Scale to radial function convention. Densities come in as 4*pi*rho
          ! and is stored as rho_{00}(r), such that rho(vec r) = Y_{00} * rho_{00}(|vec r|)
          s%rf(:,I_RHOC+I_TRU:I_RHOC+I_SMT) = s%rf(:,I_RHOC+I_TRU:I_RHOC+I_SMT)/sqrt4pi

        case( KEY_Ecor ) ! kinetic core energy
          read( unit=line, fmt=*, iostat=kios ) keyword, ecor
          s%e_core_kin = ecor / eunit

        case( KEY_Vbar ) ! zero potential
          if(m) read( unit=u, fmt=*, iostat=ios(3) ) s%rf(:,I_VBAR)
          if(p) call MPIbcast0( ios(3), comm )
          iline = iline+1 ! count up
          if( ios(3) /= 0 ) then
            if(o>0) write(o,'(9A)') sym, fun, ERROR, 'reading correction potential in "', trim(filename), '".'
            return ! error
          endif ! ios
          if(p) call MPIbcast0( s%rf(:,I_VBAR), comm )
          ! scale to radial function convention
          s%rf(:,I_VBAR) = s%rf(:,I_VBAR) * sqrt4pi / eunit

        case( KEY_rPhi ) ! partial waves and projectors
          s%mln = sum(s%nn)

          read( unit=line, fmt=*, iostat=kios ) keyword, ell, enn, occ, e
          if( kios == 0 ) then
            iln = iln_l_n( s%nn, ell, enn ) ! returns 0 if not found
            if( iln > 0 ) then

              allocate( rphi(0:irmx-1,I_TRU:I_PRJ), stat=is ) ; rphi = 0. ! allocate temp. read buffer

              if( m ) then ! master only reads
                read( unit=u, fmt=*, iostat=ios(1) ) rphi(:,I_TRU)
                read( unit=u, fmt=*, iostat=ios(2) ) rphi(:,I_SMT)
                read( unit=u, fmt=*, iostat=ios(3) ) rphi(:,I_PRJ) ! WARNING: from pawdatafileversion 3 on, this is stored prj*r^-ell
              endif ! master only
              if(p) call MPIbcast0( ios(1:3), comm )

              if( iln > s%mln ) cycle ! while lios == 0
cDBG          if( iln > s%mln ) stop 'PAWDATA: fatal error in counting MLN'

              s%ene_ln(1:2,iln) = e / eunit
              s%occ_ln(1:2,iln) = occ(1:2)

              iline = iline+3 ! count up 3 lines
              if( any( ios /= 0 ) ) then
                if(o>0) write(o,'(4A,I0,9A)') sym, fun, ERROR, 'expected 3 lines with ', &
                  irmx, ' numbers each for partial wave ', ELLCHAR(ell), trim(ENNCHAR(enn))
                return ! error
              endif ! ios /= 0
              found(iln) = all( ios(1:3) == 0 )

              if(p) call MPIbcast0( rphi, comm )

              if( .not. associated( s%rwf ) ) then
                ! allocate the radial partial wave functions
                allocate( s%rwf(0:irmx-1,s%mln,1,RWF_DIMENSION), stat=is )
              endif ! allocated (only 1st time)

              ! remove the r*wf
              do iq = I_TRU, I_SMT
cDBG            if( .not. associated( s%g%r ) ) stop 'PAWdata: set radial grid first!'
! cDBG            if(o>0) write(o,'(9(A,I0))') 'shape s%rwf(1:,iln,1,iq)=', shape(s%rwf(1:,iln,1,iq)), ' shape rphi(1:,iq)=', shape(rphi(1:,iq)), ' shape s%g%r(1:)=', shape(s%g%r(1:))
                s%rwf(1:,iln,1,iq) = rphi(1:,iq) / s%g%r(1:)
                s%rwf(0,iln,1,iq) = s%rwf(1,iln,1,iq) ! copy
              enddo ! iq
              s%rwf(:,iln,1,I_PRJ) = rphi(:,I_PRJ)

              deallocate( rphi, stat=is ) ! free temporary

            else  ! iln > 0
cDBG          if(o>0) write(o,'(9A)') sym, fun, 'line said "', trim(line), '".'
              ! found a partial wave but will not read it, because it has not been announced in s%nn
              if(o>0) write(o,'(9A,9I2)') sym, fun, WARNING(0), &
                'file contains inactive ', trim(s%sym), '-', ELLCHAR(ell), ENNCHAR(enn),'partial wave, nn =', s%nn
            endif ! iln > 0
          else  ! kios /= 0
            if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'keyword "', trim(keyword), '" should be followed by ell, enn, occ, E'
          endif ! kios /= 0

        case( KEY_dKin ) ! KineticEnergyDeficit
          allocate( s%dkin(s%mln,s%mln), stat=is ) ; s%dkin = 0. ! init
          lios = 0
          if( m ) then
            ! expect "#l=", ell
            iln = 0 ! offset
            do ell = 0, s%ellmax
              el = -1
              read( unit=u, fmt=*, iostat=lios ) keyword, el
              iline = iline+3 ! count up lines
              if( el == ell ) then
                do enn = 1, s%nn(ell)
                  read( unit=u, fmt=*, iostat=lios ) s%dkin(iln+1:iln+s%nn(ell),iln+enn)
                  iline = iline+3 ! count up lines
                  if( lios /= 0 ) then
                    if(o>0) write(o,'(9A)') sym, fun, ERROR, 'expected values for dkin in "', trim(filename), '".'
                    return ! error
                  endif ! lios
                enddo ! enn
              else  ! el == ell
!               if(o>0) write(o,'(9A)') sym, fun, ERROR, 'expected "#l=" in "', trim(filename), '".'
!               return ! error
                lios = ell+1
              endif ! el == ell
              iln = iln+s%nn(ell) ! forward
            enddo ! ell
          endif ! master
          if(p) call MPIbcast0( lios, comm )
          if( lios /= 0 ) then
            if(o>0) write(o,'(9A)') sym, fun, ERROR, 'expected "#l=" in "', trim(filename), '".'
            return ! error
          endif ! lios
          if(p) call MPIbcast0( s%dkin, comm )
#ifdef FULL_DEBUG
          ! confirm data
          do iln = 1, s%mln
            write(*,'(99ES16.6)') s%dkin(:,iln)
          enddo ! ell
#endif
        case( KEY_Vers ) ! version number
          read( unit=line, fmt=*, iostat=kios ) keyword, Nversion
          if( Nversion < 7 ) then
            if(o>0) write(o,'(4A,I0)') sym, fun, WARNING(0), 'old file format, version = ', Nversion
          endif ! Nversion < 7

        case( KEY_Cmnt ) ! show a comment in the output file
          if(o>0) write(o,'(9A)') trim(filename),': comment  "',trim(adjustl(line(len(KEY_Cmnt)+2:))), '".'

        case( KEY_Warn ) ! lunch a warning, useful for example if the PAW data file has been manipulated
          if(o>0) write(o,'(9A)') trim(filename),': ',WARNING(0),'"',trim(adjustl(line(len(KEY_Warn)+2:))), '"!'
        ! ------------------------------------------------------------
        case( '' ) ! nothing, the line is empty

        case default ! keyword
          ! display lines that are neither empty nor commented nor valid (as e.g. keywords, etc.)
cDBG      if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'line #', iline, ' of file "', trim(filename), '" is unrekognized'
cDBG      if(o>0) write(o,'(9A)') sym, fun, ' ... line: "', trim(line), '".'
          nigno = nigno+1 ! count ignored lines

        endselect ! keyword
        !=========================================================================
        endif ! keyword(1:1) /= COMMENTCHAR

        ! check again, if kios indicates an error, because then
        ! the keyword didnt appear with the correct number or type of data
        if( kios /= 0 ) then
          if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'line #', iline, &
            ' containing keyword "', trim(keyword), '" is corrupted! Line: "', trim(line), '".'
        endif ! kios

      else  ! kios == 0
        if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'cannot find a keyword in line #', iline, ' of file "', trim(filename), '".'
      endif ! kios == 0

      if(m) read( unit=u, fmt='(A)', iostat=lios ) line ! read next line of the file
      call MPIbcast0( lios, comm )                      ! communicate the next line-IOstat
    enddo ! while lios == 0
    if(m) close( unit=u, iostat=is )
!   if(p) call MPIbcast0( is, comm ) !! redundant if is is not checked afterwards
 
    ! check if the essential quantities have been found, otherwise file might be in wrong format
    if( Nversion <= NoPAWdataFile_version ) then
      if(o>0) write(o,'(9A)') sym, fun, ERROR, 'file "',trim(filename),'" does not seem to be in the right format, maybe try xml-reading!'
      ist = -99
      return ! file format error
    endif

    iln = 0
    do ell = 0, s%ellmax
      do enn = 1, s%nn(ell)
        iln = iln+1
        if( .not. found(iln) .and. o>0) write(o,'(9A)') sym, fun, WARNING(0), &
          'expected ', trim(s%sym), '-', ELLCHAR(ell), ENNCHAR(enn),'-partial wave was not found.'
      enddo ! enn
    enddo ! ell

    if( nigno > 0 .and. o>0) write(o,'(3A,9(I0,A))') sym, fun, WARNING(0), nigno, ' active lines have been ignored.'

#ifdef EXTENDED
!+ extended features
    if( m .and. checkmode > 1 ) then
      if(o>0) write(o,'(9A)') sym, fun, 'export ',trim(s%sym),' partial waves to directory "',trim(Fpath),'" if existent.'
      ! write a plotable file
      do iq = I_TRU, I_PRJ
        write( unit=fname, fmt='(9A)', iostat=lios ) trim(Fpath), trim(s%sym), Fext(iq)
        if( lios /= 0 ) cycle
        open( unit=uout, file=fname, action='write', status='unknown', iostat=lios )
        if( lios /= 0 ) cycle
        do ir = 1, min( ubound( s%rwf, 1 ), s%g .at. 5.66 )
          write( unit=uout, fmt='(99ES16.6)' ) s%g .at. ir, s%rwf(ir,:,1,iq)
        enddo ! ir
        close( unit=uout, iostat=lios )
      enddo ! iq
    endif ! checkmode
!- extended features
#endif

    ! find nr (outermost radius, where all projectors vanish)
    ir = ubound( s%rwf, 1 )
    do while( all( s%rwf(ir,:,1,I_PRJ) == 0. ) .and. ir > 0 ) ; ir = ir-1 ; enddo
    s%nr   = ir+1
    s%rcut = s%g .at. ir
cDBG  if(o>0) write(o,'(3A,I0,A,F0.3,9A)') sym, fun, 'set Nr = ', s%Nr, '  Rcut = ', s%rcut*Ang, Ang_

    do ell = 0, s%ellmax
      do enn = 1, s%nn(ell)
        do en2 = 1, s%nn(ell)
          ndkin(en2,enn,ell) = s%dkin(iln_l_n( s%nn, ell, en2 ),iln_l_n( s%nn, ell, enn ))
        enddo ! en2
      enddo ! enn
    enddo ! ell
    
#ifdef USE_PAWXMLREADER
    if(checkmode > 2 ) then 
      if(o>0) write(o,*) 'pseudopotential before filtering ..'
      ist = check_paw_setup(s, ndkin, xc_key)
    endif
#endif

    ist = prepare_species( s, gridspacing, doublegrid, ndkin )

    if( checkmode > 1 ) then 
      do ell = 0, s%ellmax
        do enn = 1, s%nn(ell)
          iln = iln_l_n( s%nn, ell, enn )
          do ir = 1, s%g%imx
            s%rwf(ir,iln,1,I_PRJ) = s%fprj(iln) .at. s%g%r(ir) 
          enddo 
          s%rwf(0,iln,1,I_PRJ) = 0
          if (ell == 0) s%rwf(0,iln,1,I_PRJ) = s%rwf(1,iln,1,I_PRJ)
        enddo ! enn
      enddo ! ell
#ifdef USE_PAWXMLREADER
      if(o>0) write(o,*) 'pseudopotential after filtering ..'
      ist = check_paw_setup(s, ndkin, xc_key)
#endif
    endif ! checkmode

#ifdef EXTENDED
!+ extended features
    if( m .and. checkmode > 1 ) then
      ! write a plotable file after filtering
      do iq = I_TRU, I_FPRJ
        write( unit=fname, fmt='(9A)', iostat=lios ) trim(Fpath), trim(s%sym), '.flt', Fext(iq)
        if( lios /= 0 ) cycle
        open( unit=uout, file=fname, action='write', status='unknown', iostat=lios )
        if( lios /= 0 ) cycle
        do ir = 1, min( ubound( s%rwf, 1 ), s%g .at. 5.66 )
          write( unit=uout, fmt='(99ES16.6)' ) s%g .at. ir, s%rwf(ir,:,1,iq)
        enddo ! ir
        close( unit=uout, iostat=lios )
      enddo ! iq
    endif ! checkmode > 1
!- extended features
#endif

  endfunction ! read_pawdata_p

  status_t function prepare_species( s, gridspacing, doublegrid, ndkin ) result( ist )
  use constants, only: Pi, sqrt4pi
  use configuration, only: WARNING, ERROR
  use type_species, only: species
  use type_species, only: I_RHOC, I_VBAR
  use type_species, only: I_TRU, I_SMT, SPIN
  use type_species, only: I_FPRJ, I_PRJ
  use type_bfun, only: operator( .at. )
  use type_bfun, only: bfun_filtered
  use unitsystem, only: Ang, Ang_
  use type_rgrid, only: operator(.at.)
  use LAPACK, only: LU_decompose, invert
  use LAPACK, only: LU_decomposition_3x3
  use LAPACK, only: invert3x3
cDBG  use type_bfun, only: scale_bfun
cDBG  use type_bfun, only: add_bfun_bfun
  use input, only: eval
  implicit none
    ! parameters
cDBG  character(len=*), parameter         :: fun = ' prepare_species: '
    character(len=*), parameter           :: BL = '                 '
    integer, parameter                    :: I_COARSE=1, I_DENSE=2, I_ESTATIX=3
    ! arguments
    type(species), intent(inout)          :: s
    real, intent(in)                      :: gridspacing(I_COARSE:I_ESTATIX) !! (/coarse,dense,electrostatix/)
    integer, intent(in)                   :: doublegrid(1:2) !! (/itp,nmesh/)
    ! local vars
    integer                               :: i, iln, ilm, j, jln, jlm, ij
    integer                               :: ell, enn, emm, ir
    integer                               :: nn, n1, n2, nrc
    real                                  :: GmaxRmax, r, rcr
    real, allocatable                     :: f(:,:), pw(:,:,:)
    real                                  :: prjspw(3,3), left(3,3), upper(3,3), q
    real                                  :: Linv(3,3), Uinv(3,3)
    real                                  :: dkin(3,3,0:5)
    real, intent(out)                     :: ndkin(3,3,0:5)

    character                             :: l
    
cDBG  character(len=16)                   :: filename
cDBG  real, allocatable                   :: del(:,:), Gell(:,:)
cDBG  integer                             :: i12
cDBG  iounit_t                            :: u
cDBG  u = o
#ifndef DEBUG
    iounit_t, parameter                   :: u = 0
    character(len=4)                      :: fun = ' Z: '
    fun(2:3) = s%sym
#endif

#ifdef NaN_SEARCH
  if( any( s%rwf(:,:,1,I_TRU:I_PRJ) /= s%rwf(:,:,1,I_TRU:I_PRJ) ) ) then
    if(o>0) write(o,'(3A,I0)') sym, fun, 'NaN discovered in s%rwf for species Z = ', s%Z
    stop 'PawData read_pawdata: NaN in s%RWF.'
  endif
#endif

    ! find cutoff index of the smooth potential correction
    ir = s%g%imx
    do while( s%rf(ir,I_VBAR) == 0. .and. ir > 1 )
      ir = ir-1
    enddo ! while
    s%irvccut = s%g .at. s%rcut !ir
    s%rvccut = s%rcut !s%g .at. ir

    ! find cutoff index of the smooth core density
    q = 0. ! charge outside r
    ir = s%g%imx
    do while( q < 1.E-9 .and. ir > 1 )
      q = q + s%rf(ir,I_RHOC+I_SMT)*s%g%r2dr(ir)*sqrt4pi
      ir = ir-1
    enddo ! while
    s%ircccut = ir
    s%rcccut = s%g .at. ir


! cDBG  if(o>0) write(o,'(3A,9(I6,A))') sym, s%sym, ' irbar =', s%irvccut, ' ircore =', s%ircccut
    if(o>0) write(o,'(3A,9(F0.3,A))') sym, fun, 'Rcut = ', ( s%g .at. s%irvccut )*Ang, '  Rcore = ', ( s%g .at. s%ircccut )*Ang, Ang_



    if( gridspacing(I_COARSE) < gridspacing(I_DENSE) ) stop 'PAWdata: gridspacings are interchanged!'

    ! count the number of projectors
    i = 0
    do ell = 0, s%ellmax
      i = i + s%nn(ell)*(2*ell+1)
    enddo ! ell
    s%mlnm = i

    ! if(o>0) write(o,'(A,I3,A,I3)') 'PAWdata: Z =', s%Z, ' mprj =', s%mlnm

    ! create the index lists ind_...
    allocate( s%ind_ell(s%mlnm), s%ind_enn(s%mlnm), s%ind_emm(s%mlnm) )
    allocate( s%ind_iln(s%mlnm), s%ind_ilm(s%mlnm) ) ! combindices

    i = 0
    iln = 0
    do ell = 0, s%ellmax
      do enn = 1, s%nn(ell)
        iln = iln+1
        ilm = ell**2 ! offset
        do emm = -ell, ell
          ilm = ilm+1
          i = i+1

          s%ind_ell(i) = ell
          s%ind_enn(i) = enn
          s%ind_emm(i) = emm
          s%ind_iln(i) = iln
          s%ind_ilm(i) = ilm

        enddo ! emm
      enddo ! enn
    enddo ! ell

!     if(u>0) write(u,'(4A,99I3)') sym, fun, s%sym, ' ilnm', ( ilnm ,ilnm=1,s%mlnm)
!     if(u>0) write(u,'(4A,99I3)') sym, fun, s%sym, ' ell ', s%ind_ell
!     if(u>0) write(u,'(4A,99I3)') sym, fun, s%sym, ' enn ', s%ind_enn
!     if(u>0) write(u,'(4A,99I3)') sym, fun, s%sym, ' emm ', s%ind_emm
!     if(u>0) write(u,'(4A,99I3)') sym, fun, s%sym, ' iln ', s%ind_iln
!     if(u>0) write(u,'(4A,99I3)') sym, fun, s%sym, ' ilm ', s%ind_ilm


    !!========================================================================
    !!== Filter the smooth core density ======================================
    !!========================================================================
    s%rhoc = bfun_filtered( s%rf(:s%ircccut,I_RHOC+I_SMT), s%g%r, s%g%dr, ell=0, &
                  h=gridspacing(I_ESTATIX), radiusratio=1.01, ell_in=0, normalize=1, positive=.true. )
    !!========================================================================
    !!== Filter the localized potential correction ===========================
    !!========================================================================
    s%vbar = bfun_filtered( s%rf(:s%irvccut,I_VBAR), s%g%r, s%g%dr, ell=0, &
                  h=gridspacing(I_ESTATIX), radiusratio=1.10, ell_in=0, normalize=0 )

#ifdef FULL_DEBUG
    ! plot the difference between filtered unfiltered rhoc and vbar
    do ir = 0, s%g%imx
      r = ( s%g .at. ir )
      write(99,'(9ES24.16)') r, s%rf(ir,I_VBAR), s%vbar .at. r
      write(98,'(9ES24.16)') r, s%rf(ir,I_RHOC+I_SMT), s%rhoc .at. r
    enddo ! ir
    if(o>0) write(o,'(9A)') sym, fun, 'filtered and raw rhoc --> fort.98, vBAR --> fort.99'
!     stop 'DEBUG line 728 mod_pawdata'
#endif




    !!========================================================================
    !!== Filter the projectors ===============================================
    !!========================================================================
    rcr = max( 1.0, eval( '$rcr', def=1.2 ) ) ! default is to allow +20%


    if(o>0 .and. rcr /= 1.2 ) write(o,'(3A,F0.3,9A)') sym, fun, 'filtering: new rcut / old rcut = ', rcr
    allocate( s%fprj( s%mln ), stat=ist )
    if( ist /= 0 ) stop 'pawdata: allocation of filtered projectors (BFUNs) failed.'


!! Rule of Thumb but certainly good for a recommendation
!!
!! Minimum number of grid points for a given configuration of projectors:
!! number of grid points entirely inside a sphere diameter 2*Rcut/h
!!  s:  1..2
!!  s*: 3..4
!!  p:  2..3
!!  p*: 4..5
!!  d:  3..4
!!  d*: 5..6
!!  f:  4..5
!!  f*: 6..7
!!
!!  1...2...3...4...5...6...7...8...9
!!    1...2...3...4...5...6...7...8...9 ! atom is placed exactly ON a grid point (odd)
!!    2...3...4...5...6...7...8...9..10 ! atom is placed BETWEEN two grid points (even)
!!    s       s*      s**
!!        p       p*      p**
!!            d       d*      d**
!!                f       f*      f**
!!
!! ==> better use the second line (even)
!!
!! means that e.g. Cu, rc=2.1 with h=0.6 can never represent a d**-projector
!!  or e.g. O, rc=1.1 with h=0.5 can at most represent a p*-projector

    R = s%rcut / gridspacing(I_COARSE)
    do ell = 0, s%ellmax
      do enn = s%nn(ell), 1, -1
        ! write a warning, if the above rule of thumb has been violated
        if( R < ell+enn ) then
          if(o>0) write(o,'(7A,F0.3,9A)') sym, fun, WARNING(0), &
           'grid very coarse for a ', ELLCHAR(ell), ENNCHAR(enn), '-projector! 2R/h = ', 2.*R
        endif ! R < ...
      enddo ! enn
    enddo ! ell

    iln = 0
    do ell = 0, s%ellmax
      do enn = 1, s%nn(ell)
        iln = iln + 1
        ! filter the projectors and store them on a equidistant radial grid
        s%fprj(iln) = bfun_filtered( s%rwf(:s%nr,iln,1,I_PRJ), s%g%r, s%g%dr, ell=ell, &
                           h=gridspacing(I_COARSE), radiusratio=rcr, ell_in=0, normalize=0 )
!                            h=gridspacing(I_COARSE), radiusratio=rcr, ell_in=0, normalize=2  )
      enddo ! enn
    enddo ! ell

    r = maxval( s%fprj(:)%rcut )
    nrc = s%g .at. r

    if(o>0) write(o,'(3A,F0.3,9A)') sym, fun, 'new rcut = ', r*Ang, Ang_


    iln = 0 ! offset
    do ell = 0, s%ellmax
      nn = s%nn(ell) ! abbr.
      l = ELLCHAR(ell)

      ! copy
      dkin(:,:,ell) = 0.
      if( .not. associated( s%dkin ) ) stop 'PAWdata s%dKin not found, maybe old PAW-data file format!'
      dkin(1:nn,1:nn,ell) = s%dkin(iln+1:iln+nn,iln+1:iln+nn)
      dkin(:,:,ell) = 0.5 * ( dkin(:,:,ell) + transpose( dkin(:,:,ell) ) ) ! symmetrize
      ndkin(:,:,ell) = dkin(:,:,ell)


        prjspw = identity( 3 )

        do n1 = 1, nn ! loop over projectors

          s%rwf(:,iln+n1,SPIN,I_FPRJ) = s%fprj(iln+n1) .at. s%g%r ! eval in real space
          do n2 = 1, nn ! loop over smooth partial waves
            prjspw(n2,n1) = radial_scalar_product( &
                  ! smooth partial wave x filtered projector function (givens as prj*r^-l)
                  s%rwf(:,iln+n2,SPIN,I_SMT), s%rwf(:,iln+n1,SPIN,I_FPRJ), s%g%r, s%g%r2dr, rpower=ell )
          enddo ! n2
          ! show matrix
cDBG      if(u>0) write(u,'(4A,I0,A,9F10.6)') sym, fun, '<',l,n1, '|...>   ', prjspw(:,n1)
        enddo ! n1


!!! begin reortho LU
        
        call LU_decomposition_3x3( prjspw, left, upper )
      
        

cDBG    ! show matrices
cDBG    if(u>0) write(u,'(9A)') ' L, U'
cDBG    do n1 = 1, 3
cDBG      if(u>0) write(u,'(9(F16.6,2F10.6))') left(:,n1), upper(:,n1)
cDBG    enddo ! n1

        if( invert3x3(  left, Linv ) == 0. ) stop 'PAWdata: result L of LU decomposition cannot be inverted!'
        if( invert3x3( upper, Uinv ) == 0. ) stop 'PAWdata: result U of LU decomposition cannot be inverted!'

        !!if( invert3x3(  prjspw, left ) == 0. ) stop 'PAWdata: matrix cannot be inverted!'
        upper = matmul( Uinv, Linv )

        allocate( pw(size(s%rwf,1),3,2) ) ! temp. partial waves
        pw = 0.
        pw(:,1:nn,:) = s%rwf(:,iln+1:iln+nn,SPIN,I_TRU:I_SMT) ! copy


        !!MOD
        do n1 = 1, nn
          s%rwf(:,iln+n1,SPIN,I_TRU:I_SMT) = 1.0 * pw(:,n1,:) ! Linv(n1,n1) is always 1.0
          do n2 = 1, n1-1 ! loop over other smooth and true partial waves
            s%rwf(:,iln+n1,SPIN,I_TRU:I_SMT) = s%rwf(:,iln+n1,SPIN,I_TRU:I_SMT) + Linv(n1,n2) * pw(:,n2,:)
cDBG        if(u>0) write(u,'(6A,F10.6,9A)') sym, fun, 'modify partial wave ', l, ennchar(n1), ' with', Linv(n1,n2), ' ', l, ennchar(n2)
          enddo ! n2
        enddo ! n1
        deallocate( pw )

        ! modify the kinetic energy deficit according to the changes in partial waves
        ndkin(:,:,ell) = matmul( Linv, matmul( dkin(:,:,ell), transpose(Linv) ) )

        !! MOD
        !!ndkin(:,:,ell) = dkin(:,:,ell)
        
!        if(6>0) write(u,'(9A)') ' ndkin'
!        do n1 = 1, 3
!          if(6>0) write(u,'(9(F16.6,2F11.6))') ndkin(:,n1,ell)
!        enddo ! n1
        
        ! modify the projectors with U
        allocate( f(size(s%fprj(1)%f),1:3) )
        f = 0.
        do n1 = 1, nn
          f(:,n1) = s%fprj(iln+n1)%f
          s%fprj(iln+n1)%f = 0.
        enddo ! n1

        do n1 = 1, nn
          do n2 = n1, nn 
            s%fprj(iln+n2)%f = s%fprj(iln+n2)%f + Uinv(n1,n2) * f(:,n1)
            !s%fprj(iln+n2)%f = s%fprj(iln+n2)%f + left(n1,n2) * f(:,n1)
          enddo ! n2
        enddo ! n1
        deallocate( f )

      iln = iln + nn ! forward index
    enddo ! ell

! #ifdef DEBUG
!   ! write projectors and filtered+reorthogonalized projectors to files
!   do ir = 1, s%g%imx
!     r = s%g .at. ir
!     write(7,'(99ES16.6)') r, s%rwf(ir,:,1,I_PRJ) ! unfiltered
!     write(8,'(99ES16.6)') r, s%fprj .at. r       ! filtered and reorthogonalized
!   enddo ! ir
! #endif



    ! compute the charge deficit tensor delta3
    !
    ! Delta3(i,j,ilm) = G(ilm,jlm,ilm)*chdm(iln,jln,ell)
    !
    !                     /R
    ! chdm(iln,jln,ell) = |dr r^2 r^ell [\phi_iln(r)\phi_jln(r)
    !                     /0        - \tilde\phi_iln(r)\tilde\phi_jln(r)]
    !
    ! set up the charge deficit matrix in the radial basis (emm-degenerate)
    allocate( s%chdm(s%mln,s%mln,0:2*s%ellmax), stat=ist ) ; s%chdm = 0.
    if( ist /= 0 ) stop 'PAWdata: allocation of s%chdm failed!'

    do ell = 0, 2*s%ellmax
      do iln = 1, s%mln
        do jln = 1, iln ! triangular loop (symmetric matrix)
          s%chdm(jln,iln,ell) = charge_deficit_matrix( s, ell, iln, jln )
          s%chdm(iln,jln,ell) = s%chdm(jln,iln,ell) ! symmetric
        enddo ! jln
      enddo ! iln
    enddo ! ell



    ! prepare the overlap matrix for the \hat{S} operator
    if( associated( s%overlap_matrix ) ) deallocate( s%overlap_matrix, stat=ist )
    allocate( s%overlap_matrix(s%mlnm,s%mlnm), stat=ist )
    if( ist /= 0 ) stop 'PAWdata: allocation of s%overlap_matrix failed!'
    
    ! the rank2 contributions for kinetic energy deficit
    allocate( s%dkin2(s%mlnm,s%mlnm), stat=ist ) ; s%dkin2 = 0.
    if( ist /= 0 ) stop 'set_tensors: allocation of s%DKIN2 failed.'

    !
    ! \hat{S} = unity + \sum_{ij} |p_i> ovlm_{ij} <p_j|
    !

    ell  = 0
    s%overlap_matrix = 0.

    do i = 1, s%mlnm
      iln = s%ind_iln(i)
      ilm = s%ind_ilm(i)
      do j = 1, i ! triangular loop (symmetric matrix)
        !---------------------------------------------------------
        jln = s%ind_iln(j)
        jlm = s%ind_ilm(j)

        if( ilm == jlm ) then ! delta(ilm,jlm)
          s%overlap_matrix(i,j) = s%chdm(iln,jln,ell)
          s%overlap_matrix(j,i) = s%overlap_matrix(i,j) ! symmetric

          ! use rotated dkin = ndkin
          s%dkin2(j,i) = ndkin(s%ind_enn(j),s%ind_enn(i),s%ind_ell(i))
          s%dkin2(i,j) = s%dkin2(j,i) ! symmetric

        endif ! delta(ilm,jlm)

      enddo ! j
    enddo ! i

#ifdef NaN_SEARCH
    if( any( s%overlap_matrix /= s%overlap_matrix ) ) then
      if(o>0) write(o,'(3A,I0,A,F0.3)') sym, fun, 'NaN discovered in s%overlap_matrix for species iZ=', s%iZ,' Z=',s%Z0
      stop 'PawData read_pawdata: NaN in s%OVERLAP_MATRIX.'
    endif
#endif

    ! set the constant monopole moment of the compensation charges
    ! which contains the negative ion charge and compensation for
    ! the smooth core charge density correction
    s%q00 = q00_monopole( s )

    !========================================================================
    !========================================================================
    !========================================================================


    ! Weinert construction:
    !========================================================================
cDBG  if(u>0) write(u,'(5A,I0,9A)') sym, fun, 'Weinert exponent for ',trim(s%sym),' is ', s%Nwexp
    ! nWexp may not be smaller than ellmax+1, because
    ! then the augmentation function will show a divergency
    ! if it is larger than ellmax, there is not even a discontinuity
    ! generate the compensation charge densities on the radial grid
    allocate( s%rcmp(0:s%nr,0:2*s%ellmax) )
    ist = generate_radial_compensation_densities( s, s%rcmp )
    if( ist /= 0 ) then
      if(o>0) write(o,'(4A,I0,9A)') sym, fun, ERROR, 'generation of radial compensation densities failed, ', ist,'k'
      return
    endif ! ist /= 0
    !========================================================================

    ist = prepare_chdt( s )
  endfunction ! prepare_species

  !! end legacy code
#endif

endmodule ! pawdata





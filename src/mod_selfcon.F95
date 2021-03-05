#include "config.h"

! #define DEBUG
! #define FULL_DEBUG
#define c$omp !!!$omp
! #define c$omp !$omp

! no matter if in DEBUG mode or not, with this defined,
! the integral of rho_aug will always be displayed
#define SHOW_CHARGE_OF_RHO_AUG


#ifdef DEBUG
!!! remove comment from debug line
!!! ==> all debug lines will be included
#define cDBG
#else
!!! comment lines with !DBG
!!! ==> no debug lines will be included
#define cDBG !DBG
#endif


#ifdef NaN_SEARCH
#define cNaN
#else
#define cNaN !NaN
#endif

#ifdef R1_C2
! This file will be preprocessed twice, first
! with R1_C2 == 1, and then with R1_C2 == 2
! then the two parts are concatenated to one source.
! This requires that the module head has to be
! appearing, when R1_C2 == 1 and the line "end module <name>"
! with R1_C2 == 2.
#if R1_C2 == 1



!! @author Paul Baumeister, Andrea Nobile
!! @version 4.04
!!
!! selfconsistency (abbreviated for debugging reasons)
!! generation of the potential
!! normalization of KS-states
!! solving the KS-equation
!! ...
!!
module selfcon
  use configuration, only: o ! output unit, 0: no output
implicit none
  private ! default for the module namespace
  character(len=*), parameter, private :: sym = 'SCF' !! module symbol

  ! public interfaces
  public :: rhov_from_psi
  interface rhov_from_psi
    module procedure rhov_from_psi_r, rhov_from_psi_c
  endinterface

  public :: pot_from_rho
  public :: allocate_rhov_c


  public :: relax_eigenstates
  interface relax_eigenstates
    module procedure relax_eigenstates_r, relax_eigenstates_c
  endinterface

  public :: normalize_states
  interface normalize_states
    module procedure normalize_states_r, normalize_states_c
  endinterface

  public :: integral
  interface integral
    module procedure integral_r3, integral_r4
  endinterface

  public :: sum_of_eigenvalues
  public :: test


  ! private interfaces
  interface find_density_matrix
    module procedure find_density_matrix_r, find_density_matrix_c
  endinterface


  interface construct_smooth_density
    module procedure construct_smooth_density_r, construct_smooth_density_c
  endinterface

  ! keys
  integer, parameter            :: I_ENE_TOT = 0
  integer, parameter            :: I_ENE_REF = 1
  integer, parameter            :: I_ENE_KIN = 2
  integer, parameter            :: I_ENE_HTR = 3
  integer, parameter            :: I_ENE_EXC = 4

  real, allocatable :: rhov_c(:,:,:,:)
 contains


  subroutine allocate_rhov_c(gc, global)
    use type_grid, only: grid
    use type_info, only: info
    type(grid), intent(in)                :: gc
    type(info), intent(in)                :: global         !! state object

    allocate(rhov_c(gc%ng(1),gc%ng(2),gc%ng(3),global%nspins)) !! smooth valence density [on gc]
    rhov_c = 0.
  endsubroutine


  real function pot_from_rho( global, &
    gc, gd, ge, nscale, &
    key_symmetry, &
    key_poisson, &
    poisson, &
    a, &
    sumeigvals, &
    efield, hfield, &
    rhov_d, &
    rhoc, &
    rhot, &
    rho_aug, &
    vBAR, &
    vXC, &
    ves, &
    vTOT, &
    vTOT_d, &
    vTOT_c, &
    rho_jellium ) &
  result( etotal )

  use type_info, only: info
  use type_atom, only: atom
  use type_grid, only: grid
  use type_kpoint, only: kpoint
  use type_criteria, only: criteria

  use configuration, only: WARNING

  use interpolation, only: interpolate
  use type_comp, only: add
  use density_functionals, only: xc_potential
  use density_functionals, only: is_gradient_functional
  use MPItools, only: operator(.MPIsum.), MPIallsum

  use input, only: eval
  use unitsystem, only: eV, eV_, Ang, Ang_

  implicit none
    ! parameters
    character(len=*), parameter           :: fun = ' pot_from_rho: '
    ! arguments
    type(info), intent(in)                :: global         !! state object
    type(grid), intent(in)                :: gc             !! coarse grid
    type(grid), intent(in)                :: gd             !! dense grid
    type(grid), intent(in)                :: ge             !! electrostatix grid
    integer, intent(in)                   :: nscale         !! grid refinement (ge/gd)
    integer, intent(in)                   :: key_symmetry   !! symmetrization if needed
    integer, intent(in)                   :: key_poisson    !! method for Poisson eq
    type(criteria), intent(in)            :: poisson        !! criteria for iterative method
    type(atom), intent(inout)             :: a(:)           !! atoms
    real, intent(in)                      :: sumeigvals
    real, intent(in)                      :: efield(1:3), hfield(1:3)
    real, intent(inout)                   :: rhov_d(:,:,:,:)      !! valence density [on gd]
    real, intent(in)                      :: rhoc(:,:,:,:)        !! core density [on ge]
    real, intent(inout)                   :: rhot(:,:,:,:)        !! rhov + rhoc [on ge]
    real, intent(inout)                   :: rho_aug(:,:,:)       !! augmented density [on ge]
    real, intent(in)                      :: vBAR(:,:,:,:)        !! local potential corrections [on ge]
    real, intent(inout)                   :: vXC(:,:,:,0:)        !! exchange correlation pot. 0:eXC [on ge]
    real, intent(inout)                   :: ves(:,:,:)           !! Electrostatic pot. [on ge]
    real, intent(out)                     :: vTOT(:,:,:,:)        !! total pot. [on ge]
    real, intent(out)                     :: vTOT_d(:,:,:,:)      !! total pot. [on gd]
    real, intent(out)                     :: vTOT_c(:,:,:,:)      !! total pot. [on gc]
    real, intent(in), optional            :: rho_jellium(:,:,:)   !! jellium density [on ge]

    integer                               :: ia ! index for atoms in the parallelized atom list
    status_t                              :: ist
    integer                               :: jspin, id, ii!, ispn  ! spin indices
    real                                  :: sint, r
    real                                  :: ebar


    real, allocatable :: derivatives(:,:,:,:,:) ! derivatives for GGA

    !allocate(rhov_c(gc%ng(1),gc%ng(2),gc%ng(3),global%nspins)) !! smooth valence density [on gc]

    ! generate a%GntDm = Gaunt * a%Dm
    ist = setup_radial_coefficients( a, comm=gc%comm )


    ! generate a%rho = a%GntDm * a%s%rwf * a%s%rwf + a%s%rhoc
    ! and the charge deficit multipole moments
    !         a%qlm = a%GntDm * a%s%chdm          + a%s%q00
    ist = setup_radial_densities( a, comm=gc%comm )



    ! correct possible negative density entries
    where( rhov_d < 0. ) ; rhov_d = 0. ; endwhere ! rho < 0.

    ! the valence density has to be interpolated to ge, ge/gd = nscale
    ist = interpolate( rhov_d, rhot, g=gd, gd=ge, mscale=nscale ) ! gd --> ge

    ! it is assumed, that the core density is spin paired, so
    ! if the calculation is spin polarized (nspins==2), only
    ! half of the core charge has to be added to upspin or
    ! downspin density.
    ! TOT = VAL + COR  [on ge]
    rhot = rhot + rhoc

    ! eval the XC potential on the dense grid
    ! the exchange correlation energy is stored in the is=0-component
    ! of the vXC array, because it is spin-independent
    if( is_gradient_functional( global%kfunctional ) ) then
      allocate( derivatives(ge%ng(1),ge%ng(2),ge%ng(3),1:3,-1:1), stat=ist )
      if( ist /= 0 ) stop 'scf: failed to allocate derivatives for density gradients!'
      ist = grid_derivative( ge, rho=rhot, derivative=derivatives )
    endif ! is_GGA
    ist = xc_potential( rho=rhot, &
                       vxc=vXC(:,:,:,1:global%nspins), &
                       exc=vXC(:,:,:,0), key=global%kfunctional, &
                       drho=derivatives )
    if( allocated(derivatives) ) deallocate( derivatives, stat=ist ) ! free memory

    ! transform the two atomic radial densities (TRU and SMT)
    ! onto points in the sphere to enable the evaluation of the
    ! exchange correlation potential in realspace (r,theta,phi)
    ist = xc_in_the_spheres( a, key=global%kfunctional, nspins=global%nspins, me=gc%rank )

    ! construction of the augmented charge density:
    ! start with the total (spin integrated) smooth density
    ! already containing the smooth core density [on the dense grid]

    selectcase( global%nspins )
    case( 1 ) ; rho_aug = rhot(:,:,:,1)
    case( 2 ) ; rho_aug = rhot(:,:,:,1) + rhot(:,:,:,2)
    case default ; stop 'SCF global%nspins not in {1,2}'
    endselect ! nspins

    if( present( rho_jellium ) ) then
      ! if jellium is present, the jellium charge distribution
      ! (usually negative smeared background charges) is added to the
      ! augmented charge before solving the electrostatic potential.
      rho_aug = rho_aug + rho_jellium

    endif ! present rho_jellium


    ! add the compensation charge densities to
    ! the augmented density on the dense grid
    do ia = 1, size(a) ! for each atom
      call add( cmp=a(ia)%cmp, qlm=a(ia)%qlm, rho=rho_aug )
    enddo


    ! the augmented charge density needs to be charge neutral in average
    sint = integral( rho_aug, ge ) ! rho_aug should be charge neutral in average now

    if( abs(sint) > 1E-3 .and. o>0) write(o,'(4A,9(F0.9,A))') sym, fun, WARNING(0), &
               'augmented density not neutral, found ', sint, ' e, use more vacuum!'


    ! enforce charge neutrality
    rho_aug = rho_aug - sint/ge%svol


    ist = poisson_solve( ge, rho_aug, ves, Poisson, key_poisson )
    if( ist /= 0 .and. o>0) write(o,'(9A)') sym, fun, 'Poisson solver did not converge.'


    do id = 1, 3
      if( efield(id) /= 0. ) then
        if(o>0) write(o,'(3A,F12.6,9A)') sym, fun, 'add electric field of', efield(id), ' Ha/aB in ', achar(119+id), '-direction.'
        do ii = 1, ge%ng(id)
          r = ii*ge%h(id)-ge%off(id)
          selectcase( id )
          case( 1 ) ; ves(ii,:,:) = ves(ii,:,:) + r*efield(id)
          case( 2 ) ; ves(:,ii,:) = ves(:,ii,:) + r*efield(id)
          case( 3 ) ; ves(:,:,ii) = ves(:,:,ii) + r*efield(id)
          endselect ! id
        enddo ! ii
      endif ! efield /= 0
    enddo ! id


    ! find the integral of the compensation charges on the dense
    ! grid with the electrostatic potential ves on the dense grid <hat n | ves>
    ! The result a%uHlm(:,I_AUG) is only stored in the atom-owner process
    ! then compare to a%uHlm(:,I_SMT) and align the electrostatic potentials
    ! in the spheres
    ist = Coulomb_multipole_moments( a, g=ge, Ves=ves, comm=gc%comm )


    ! Total effective potential on ge

    ! create the total (local) potentials (on the dense grid)
    ! from the exchange correlation potential, the Hartree/Coulomb
    ! potential computed from the augmented density and
    ! the atomic correction potential Vbar (localized in the spheres)
    ebar = 0.0
    do jspin = 1, global%nspins
      vTOT(:,:,:,jspin) = vXC(:,:,:,jspin) + ves(:,:,:) + vBAR(:,:,:,jspin)
      ebar = ebar + sum(vBAR(:,:,:,jspin)*rhot(:,:,:,jspin)) *ge%hvol
    enddo ! jspn
    ebar = ebar .MPIsum. ge%comm

    !write(*,*) 'ebar = ', ebar

    if( global%nspins == 2 ) then
      if( hfield(3) /= 0. ) then
        if(o>0) write(o,'(3A,9(F0.6,A))') sym, fun, 'add magnetic field of ', hfield(3)*eV, eV_ ! in z-direction
        do jspin = 1, global%nspins
          vTOT(:,:,:,jspin) = vTOT(:,:,:,jspin) + hfield(3)*(2.*jspin-3.) ! (2*jspin-3): {1,2} --> {-1,+1}
        enddo ! jspn
      endif ! hfield
    endif ! global%nspins


    ! setup atomic Hamiltonian matrices
    do ia = 1, size(a)
      if( gc%rank == a(ia)%owner ) ist = atomic_Hamiltonian_matrix( a(ia) )
    enddo ! ia


    ist = interpolate( vTOT, vTOT_d, g=gd, gd=ge, mscale=-nscale ) ! reduce total potential to gd

    ist = interpolate( vTOT_d, vTOT_c, g=gc, gd=gd, mscale=-2, order=3 ) ! reduce total potential to gc

    etotal = totalenergy( sumeigvals, a, gc, & ! ... via the KS energy eigenvalue sum
                          gc, rhov_c, vTOT_c, & ! pass the coarse grid quantities for double counting correction
                          ge, rho_aug, ves, &
                          ge, rhot, vXC(:,:,:,0), ebar ) ! vXC(:,:,:,0) == Exc, exchange-correlation energy density


  endfunction pot_from_rho


! end of the head part
#endif
! from here, everything will be comiled twice



#if R1_C2 == 1
! !! define the wave function type to be real
#define REAPLEX real
#else
! !! define the wave function type to be complex
#define REAPLEX complex
#endif





#if R1_C2 == 1
  !!
  !! The selfconsistency iteration aims to find
  !! the selfconsistent density for a given set of
  !! atoms and boundary conditions, i.e. the
  !! (generalized) eigenfunctions of the Hamiltionian
  !! H that arisies from the density via Hartree
  !! and Exchange correlation potential are supposed to
  !! reproduce that density, when self-consistency is
  !! reached. Therefore the "residual" is introduced
  !! as a norm of the change in density at each SC step.
  !! This norm has to be minimized.
  !!
  status_t function rhov_from_psi_r( &
#else
  !! see documentation of rhov_from_psi_r
  status_t function rhov_from_psi_c( &
#endif
      global, &
      kpt, &
      gc, gd, &
      key_symmetry, &
      a, &
      cpsi, &
      spsi, &
      psi_state, &
      rhov ) result( ist )
  use configuration, only: WARNING, ERROR
  use type_info, only: info
  use type_state, only: state
  use type_atom, only: atom
  use type_grid, only: grid
  use type_kpoint, only: kpoint
  use interpolation, only: interpolate

  implicit none
    ! parameters
    character(len=*), parameter           :: fun = ' rhov_from_psi: '
    ! arguments
    type(info), intent(in)                :: global         !! state object
    type(kpoint), intent(in)              :: kpt(:)         !! list of kpoints
    type(grid), intent(in)                :: gc             !! coarse grid
    type(grid), intent(in)                :: gd             !! dense grid
    integer, intent(in)                   :: key_symmetry   !! symmetrization if needed
    type(atom), intent(inout)             :: a(:)           !! atoms
    REAPLEX, intent(inout)                :: spsi(:,:)      !! smooth wave functions [on gc]
    REAPLEX, intent(out)                  :: cpsi(:,:)      !! projection coefficients
    type(state), intent(inout)            :: psi_state(:)   !! state object for each state
    real, intent(out)                     :: rhov(:,:,:,:)  !! valence density on gd

    integer                               :: n0
    logical, save                         :: first_call = .true.

    !if(first_call) then
    !  allocate(rhov_c(gc%ng(1),gc%ng(2),gc%ng(3),global%nspins)) !! smooth valence density [on gc]
    !  first_call = .false.
    !endif



    n0 = normalize_states( 0, & ! SCF iteration counter, set to 0
            gc, &               ! coarse grid descriptor
            a, &                ! local list of atoms
            global%nbnd, &      ! local # of bands
            global%nspn, &      ! local # of spins
            global%nkpt, &      ! local # of kpoints
            kpt, &              ! local list of kpoints
            spsi, &             ! local smooth wave functions
            psi_state, &        ! local state descriptors
            cpsi )              ! local projection coefficient vector (out)

    if( n0 > 0 ) then
      ! one or more states could not be normalized
      ! (the result of normalize_states is already global w.r.t. the state parallelization)
      if(o>0) write(o,'(4A,I0,9A)') sym, fun, ERROR, 'failed to normalize ', n0, ' states!'
      stop 'rhov_from_psi: ERROR one or more states could not be normalized.'
    endif ! some states could not be normalized

    ! construct the new atom centered density matrix from
    ! the projection coefficients and occupation numbers of the Kohn-Sham states
    ist = find_density_matrix( a=a, cpsi=cpsi, psi_state=psi_state, &
                              nbnd=global%nbnd, nspn=global%nspn, nkpt=global%nkpt, &
                              equi_comm=gc%equi_comm, me=gc%rank )

    ! construct new smooth charge density from the smooth Kohn-Sham wave functions

    ist = construct_smooth_density( gc, global%nbnd, global%nspn, global%nkpt, psi_state, spsi, rho=rhov_c )

    ! interpolate the smooth charge density
    ! on the coarse grid up to the dense grid
    ! if the smooth density has not been constructed on the dense grid
    ist = interpolate( rhov_c, rhov, g=gc, gd=gd, mscale=2, order=3 )

    !deallocate(rhov_c)
    ist = 0
  endfunction ! rho_from_psi





#if R1_C2 == 1
  status_t function normalize_states_r( &
#else
  status_t function normalize_states_c( &
#endif
                            iiteration, &
                            g, &
                            a, &
                            nbnd, nspn, nkpt, &
                            kpt, &
                            spsi, &
                            psi_state, &
                            cpsi ) &
  result( norm0 ) ! returns the number of states,
                  ! that could not be normalized
  ! types
  use type_grid, only: grid
  use type_atom, only: atom
  use type_state, only: state
  use type_kpoint, only: kpoint
#ifdef CONSTR
  use constraints, only: mask
#endif
  use operators, only: Hmt
  use operators, only: scalar_product ! scalar product
  use MPItools, only: operator(.MPIsum.), MPIallsum
  implicit none
    ! parameters
    character(len=*), parameter           :: fun = ' normalize_states: '
    real, parameter                       :: THRES = 1E-16 ! threshold
    ! arguments
    integer, intent(in)                   :: iiteration  ! SCF iteration counter
    type(grid), intent(in)                :: g           ! coarse grid
    type(atom), intent(in)                :: a(:)        ! atoms
    integer, intent(in)                   :: nbnd, nspn, nkpt  ! # of bands, spins, kpoints
    type(kpoint), intent(in)              :: kpt(:)      ! list of kpoints
    ! wave functions
    REAPLEX, intent(inout)                :: spsi(:,:)   ! smooth wave functions [on gc]
    type(state), intent(inout)            :: psi_state(:) ! state object for each state
    REAPLEX, intent(out)                  :: cpsi(:,:)   ! projection coefficients
    ! local vars
    REAPLEX                               :: Sspsi(size(spsi,1),nbnd)  ! Overlap matrix times spsi
    REAPLEX                               :: Hspsi(size(spsi,1),nbnd)  ! Hamiltonian times spsi
    real                                  :: zero(g%ng(1),g%ng(2),g%ng(3),1:1)
#ifdef CONSTR
    REAPLEX                               :: mspsi(size(spsi,1))  ! Mask times spsi
    integer                               :: ic
#endif
    integer                               :: ibsk ! combindex for bands, spins and kpoints
    integer                               :: ibnd ! index for bands
    integer                               :: ispn ! index for spins
    integer                               :: ikpt ! index for kpoints
    integer                               :: nbsk        ! dim#2 of cpsi and spsi and sspsi
    integer                               :: naprj       ! dim#1 of cpsi
    real                                  :: nrm2, scal(nbnd)

    zero = 0. ! init dummy local potential

    nbsk  = size( spsi, 2 )
    naprj = size( cpsi, 1 )
#ifdef DEBUG
!     if( ist /= 0 ) stop 'normalize_states: failed to allocate dummy potential (ZERO)!'
    if( size(spsi,1) /= product(g%ng) ) stop 'normalize_states: dim#1 of SPSI wrong.'
    if( size(sspsi,1) /= size(spsi,1) ) stop 'normalize_states: dim#1 of SSPSI wrong.'
    if( size(cpsi,2) /= nbsk ) stop 'normalize_states: dim#2 of CPSI wrong.'
#endif
    norm0 = 0 ! init the result (number of states that cannot be normalized)

#ifdef _SHOW_MEMORY
    if(o>0) write(o,'(3A,I0)') 'print_memusage() line ', __FILE__ ,':', __LINE__
    call print_memusage()
#endif

    ! normalize the states and
    ! determine the new projection coefficients
    ! by calling Smat with optional argument c_out
    ibsk = 0 ! counter for all bands
    do ikpt = 1, nkpt
      do ispn = 1, nspn

        ! the call of Smat with the optional argument cket returns the projection coefficients
        call Hmt( g, zero, 1, a, kpt(ikpt), ket=spsi(:,ibsk+1:ibsk+nbnd), Hket=Hspsi, Sket=Sspsi, cket=cpsi(:,ibsk+1:ibsk+nbnd) )
        ! for saving communication overhead, the norm could be evaluated locally first

        ! compute the inner product of each state with itself
        do ibnd = 1, nbnd
          scal(ibnd) = real( scalar_product( Sspsi(:,ibnd), spsi(:,ibsk+ibnd), h3=g%hvol ) )!, comm=g%comm ) ) ! collected communication !!
        enddo ! ibnd

        ! collected communication
        call MPIallsum( scal, g%comm )

        ! normalize each state
        do ibnd = 1, nbnd
          ibsk = ibsk+1
#ifdef FULL_DEBUG
cDBG      if(o>0) write(o,'(3A,I6,I2,I6,A,ES24.16)') sym, fun, '[ik,is,ib] ', ikpt, ispn, ibnd, '  norm =', nrm2
#endif
          ! nrm2 = <Psi|S|Psi> should be 1.0
          ! kinetic energy directly evaluated
          ! Ekin =  scalar_product( Hspsi, spsi(:,ibsk), h3=g%hvol, comm=g%comm )/nrm2

          nrm2 = scal(ibnd)
          if( nrm2 > THRES ) then
            ! scale
            scal(ibnd) = 1./sqrt(nrm2)
            spsi(:,ibsk) = scal(ibnd) * spsi(:,ibsk) ! scale the wave function
            cpsi(:,ibsk) = scal(ibnd) * cpsi(:,ibsk) ! and its coefficients
#ifdef CONSTR
            if( allocated( mask ) ) then
              Sspsi = scal(ibnd) * Sspsi
              do ic = 1, ubound(mask,2)
                mspsi = mask(:,ic) * spsi(:,ibsk)
                psi_state(ibsk)%cwgt(ic) = real( scalar_product( Sspsi(:,ibnd), mspsi, h3=g%hvol, comm=g%comm ) )
              enddo ! ic
cDBG          if(o>0) write(o,'(3A,I6,I2,I6,A,99F10.6)') sym, fun, '[ik,is,ib] ', ikpt, ispn, ibnd, '  constraint weights =', psi_state(ibsk)%w(1:ubound(mask,2))
            else  ! allocated mask
              psi_state(ibsk)%cwgt(:) = 0.
            endif ! allocated mask
#endif
          else
            norm0 = norm0+1 ! count states that cannot be normalized
cDBG        if(o>0) write(o,'(3A,I6,I2,I6,A,ES10.2)') sym, fun, '[ik,is,ib] ', ikpt, ispn, ibnd, '  tiny norm =', nrm2
          endif ! nrm2 < THRES

        enddo ! ibnd

      enddo ! ispn
    enddo ! ikpt


#ifdef DEBUG
    if( norm0 > 0 ) then
      ! some states could not be normalized
      if(o>0) write(o,'(2A,2(I0,A),ES10.2)') sym, fun, norm0, ' of ', &
        nkpt*nspn*nbnd, ' states could not be normalized, <Psi|S|Psi> <', THRES
    endif ! norm0 > 0
#endif
    ! allreduce the number of states that could not be normalized
    norm0 = norm0 .MPIsum. g%equi_comm ! state parallelization

  endfunction ! normalize_states




#if R1_C2 == 1
  real function relax_eigenstates_r( &
#else
  real function relax_eigenstates_c( &
#endif
                            iiteration, &
                            g, &
                            a, &
                            global, &
                            kpt, &
                            vloc, &
                            spsi, &
                            Sspsi, &
                            psi_state, &
                            key_solver, &
                            precond &
                          ) &
  result( residual )
  ! types
  use type_grid, only: grid
  use type_atom, only: atom
  use type_info, only: info
  use type_state, only: state
  use type_kpoint, only: kpoint
  ! methods
  use cg_eigensolver, only: reorder
  use cg_eigensolver, only: cg_eigen_solve
  use dav_eigensolver, only: dav_eigen_solve
  use diis_eigensolver, only: diis_eigen_solve
#ifdef PARALLELIZE_DIIS_BY_THREADS
  use diis_eigensolver, only: diis_eigen_solve_many
#endif
!   use subspace, only: pp_subspace_rotation ! bands parallel + ScaLAPACK
! #ifdef NOScaLAPACK
  ! if there is no ScaLAPACK, there is no reason to use the complicated
  ! algorithm for the matrix distribution
!   use diis_eigensolver, only: subspace_rotation ! serial
! #else
  use subspace, only: subspace_rotation ! ScaLAPACK
! #endif
#ifdef EXTENDED
  use diis_eigensolver, only: explicit_hamiltonian
  use sd_eigensolver, only: sd_update
#endif
  ! keys
  use constants, only: SOLVER_SUBSROT
  use constants, only: SOLVER_DESCENT
  use constants, only: SOLVER_CONGRAD
  use constants, only: SOLVER_DAVIDSO
  use constants, only: SOLVER_RMMDIIS
  use constants, only: SOLVER_XPLICIT
  use constants, only: SOLVER_NONE
  use constants, only: SOLVER_SCHEME_STABLE ! default
  use constants, only: SOLVER_SCHEME_SPEED
!   use constants, only: SOLVER_SCHEME_OCCUP
  ! tools
  use operators, only: scalar_product ! scalar product
  use MPIconst, only: Wtime
  use MPItools, only: MPIbarrier, operator(.MPIsum.)
! #ifdef DEBUG
  use MPItools, only: operator(.MPImax.)
  use MPItools, only: operator(.MPIaverage.)
!   use MPItools, only: get_median ! ( array, comm )
! #endif
  use input, only: eval
  implicit none
    ! parameters
    character(len=*), parameter           :: fun = ' relax_eigenstates: '
    integer, parameter                    :: I_XP=1, I_SD=2, I_CG=3, I_DI=4, I_SR=5, I_RO=6, I_DAV=7
    character(len=*), parameter           :: METHOD(0:7) = (/'     ',' Xact', ' SD  ', ' CG  ', ' DIIS', ' Sub ', ' reor', '  DAV'/)
    ! arguments
    integer, intent(in)                   :: iiteration  ! SCF iteration counter
    type(grid), intent(in)                :: g           ! coarse grid
    type(atom), intent(in)                :: a(:)        ! atoms

    type(info), intent(in)                :: global
    type(kpoint), intent(in)              :: kpt(:)      ! list of kpoints
    real, intent(in)                      :: vloc(:,:,:,:) ! local effective pot. [on gc]
    ! wave functions
    REAPLEX, intent(inout)                :: spsi(:,:)   ! smooth wave functions [on gc]
    REAPLEX, intent(inout)                :: Sspsi(:,:)  ! Overlap matrix times spsi (for one sk-set of bands)
    type(state), intent(inout)            :: psi_state(:) ! state object for each state
    integer, intent(in)                   :: key_solver   ! configuration for solving Kohn-Sham eq
    logical, intent(in), optional         :: precond      ! use preconditioning
    ! local vars
    integer                               :: ibsk ! combindex for bands, spins and kpoints
    integer                               :: ibnd ! index for bands
    integer                               :: ispn, jspin ! index for spins
    integer                               :: ikpt ! index for kpoints
    integer                               :: nbsk        ! dim#2 of cpsi and spsi and sspsi
    logical                               :: use_xp ! diagonalize the full Hamiltonian (expensive!)
    logical                               :: use_sd ! steepest descent
    logical                               :: use_cg ! orthogonlized conjugate gradients
    logical                               :: use_dav ! davidson
    logical                               :: use_di ! direct inversion of the iterative subspace
    logical                               :: use_sr ! subspace rotation
    logical                               :: conv
    integer                               :: sr
    integer                               :: nfail=0
#ifdef TIME
    real                                  :: tt, t(0:9)
    integer                               :: n=0, i, ii(1:6)=0
#endif
    real                                  :: res_avg, occ_wgt
cDBG  real                                  :: res_min, res_max, res_med

#ifdef PARALLELIZE_DIIS_BY_THREADS
    integer                               :: nblock, ibnd1, ibndm
    nblock = max( 0, nint( eval( '$block', def=0. ) ) )
#else
    integer, parameter :: nblock = 0
#endif

    call MPIbarrier( g%comm )
    call MPIbarrier( g%equi_comm )

#ifdef TIME
    t = 0. ; t(0) = Wtime() ! init
#endif

    nbsk = size( spsi, 2 )
cDBG  if( size(spsi,1) /= product(g%ng) ) stop 'relax_eigenstates: dim#1 of SPSI wrong.'
cDBG  if( size(sspsi,1) /= size(spsi,1) ) stop 'relax_eigenstates: dim#1 of SSPSI wrong.'
cDBG  if( size(sspsi,2) < global%nbnd ) stop 'relax_eigenstates: dim#2 of SSPSI too small.'

    ! set defaults
    use_sd = .false.
    use_cg = .false.
    use_dav = .false.
    use_di = .false.
    use_sr = .true.
    use_xp = .false.

    selectcase( key_solver )
    case( SOLVER_NONE )    ; use_sr = .false. ! nothing, warning: will NOT solve any problem
    case( SOLVER_DESCENT ) ; use_sd = ( iiteration > 1 ) ! only steepest descent
    case( SOLVER_CONGRAD ) ; use_cg = ( iiteration > 1 ) ! only conjugate gradients
    case( SOLVER_DAVIDSO ) ; use_dav = ( iiteration > 0 ) ! only conjugate gradients
    case( SOLVER_RMMDIIS ) ; use_di = ( iiteration > 1 ) ! only rmm-diis
    case( SOLVER_SUBSROT ) ; use_sr = .true. ! only subspace rotation, orbitals will NOT relax.
    case( SOLVER_XPLICIT ) ; use_xp = .true. ! orbitals will be exact
    ! schemes
    case( SOLVER_SCHEME_SPEED )
      ! fast configuration: sr & di > 1
      use_di = ( iiteration > 1 )
    case default ! ( SOLVER_SCHEME_STABILITY )
      ! very robust configuration: sr & cg > 1 & di > 8
      use_cg = ( iiteration > 1 )
      use_di = ( iiteration > 8 )
    endselect ! key_solver
    ! consequences
    use_sr = use_sr .or. use_di ! always run subspace after diis
    use_sr = use_sr .or. use_dav ! always run subspace after dav
    use_sr = use_sr .or. use_sd ! always run subspace after sd (sd has no orthogonalization)

    if( use_cg .and. global%nbnd < global%nbands ) stop 'SCF line 888: bands parallelized, cannot use CG method'
    !if( use_dav .and. global%nbnd < global%nbands ) stop 'SCF line 888: bands parallelized, cannot use DAV method'

    ! solve the eigenvalue equation for all bands, spins, and kpoints
    ibsk = 0 ! init the combindex for bands, spins and k-points
    nfail = 0 ! init failure counter
    do ikpt = 1, global%nkpt
cDBG  ! show the k-point
cDBG  if(o>0) write(o,'(3A,I4,A,3F7.3,9A)') sym, fun, 'kpoint#', kpt(ikpt)%jk, '[', kpt(ikpt)%k, ' ]'
      do ispn = 1, global%nspn ; jspin = ispn + global%iospn ! global spin index
#ifdef TIME
        tt = Time_tic() ! start stopwatch
#endif
        ! solve the eigenvalue equation H|psi> - E S|psi> = 0
        ! where H is the Kohn-Sham Hamiltonian,
        ! E is the Kohn-Sham energy eigenvalue
        ! |psi> is the Kohn-Sham state and
        ! S is the overlap matrix due to charge deficits
        ! of the non-norm-conservation in PAW.

        if( use_xp ) then
          ! solve by setting up an explicit hamiltonian matrix
          ! where the dim is the number of all grid points !! VERY EXPENSIVE and serial only
#ifdef EXTENDED
          sr = explicit_hamiltonian( g, vloc, jspin, a, g%naprj, kpt(ikpt), &
                 spsi(:,ibsk+1:ibsk+global%nbnd), psi_state(ibsk+1:ibsk+global%nbnd)%ene )
#else
          stop 'Explicit Hamiltonian solver not installed, activate extended features!'
#endif
          if(o>0) write(o,'(3A,I4,A,99F10.6)') sym, fun, ' e(Ha)', kpt(ikpt)%jk, ':', psi_state(ibsk+1:ibsk+min(global%nbnd,20))%ene
          psi_state(ibsk+1:ibsk+global%nbnd)%res = 1.E-12 ! as small as mashine precision
          ibsk = ibsk+global%nbnd
#ifdef TIME
          t(I_XP) = t(I_XP) + Time_tic() ! sum up the lap time
#endif
          cycle ! no other solver method needed
        endif ! use_xp

        if( use_sd ) then
#ifdef EXTENDED
          conv = sd_update( &
                          atm=a, & ! atoms
                          g=g, & ! coarse grid
                          kp=kpt(ikpt), & ! kpoint
                          vloc=vloc, & ! local potential
                          jspin=jspin, & ! spin index
                          energy=psi_state(ibsk+1:ibsk+global%nbnd)%ene, &
                          residual=psi_state(ibsk+1:ibsk+global%nbnd)%res, &
                          s=  spsi(:,ibsk+1:ibsk+global%nbnd), &
                          Ss=Sspsi(:,1:global%nbnd) &
cDBG                      , show_energy=.true. &
                          )
#else
          stop 'SteepestDescent solver not installed!'
#endif
          use_cg = .false.
          use_dav = .false.
          use_di = .false.
          use_sr = .true. ! necessary
#ifdef TIME
          t(I_SD) = t(I_SD) + Time_tic() ! sum up the lap time
#endif
        endif ! use_sd

        if( use_dav ) then
            !write (*,*) 'ibsk ', ibsk
            conv = dav_eigen_solve( &
                          atm=a, & ! atoms
                          g=g, & ! coarse grid
                          kp=kpt(ikpt), & ! kpoint
                          vloc=vloc, & ! local potential
                          jspin=jspin, & ! spin index
                          energy=psi_state(ibsk+1:ibsk+global%nbnd)%ene, &
                          residual=psi_state(ibsk+1:ibsk+global%nbnd)%res, &
                          threshold=1E-9, & ! in energy units (Ha)
                          maxiter=15,& !100 !200 ! psi_state(ibsk+ib)%maxiter, & ! maximum number of CG iterations
                          s=  spsi(:,ibsk+1:ibsk+global%nbnd), &
                                    band_comm=global%band_comm, &
                                    band_ioff=global%iobnd, &
                          precond=precond &
                        )
            !write(*,*) iiteration
#ifdef TIME
            t(I_DAV) = t(I_DAV) + Time_tic() ! sum up the lap time
#endif
            if( .not. conv ) nfail = nfail+global%nbnd ! count up
        endif ! use_dav


        if( use_cg .or. use_di ) then
          do ibnd = 1, global%nbnd
            ! uses the orthogonalized Conjugate Gradients method
            ! to find the lowest the eigenstates of the Kohn-Sham Hamiltonian
            if( use_cg ) then
              !write (*,*) 'ibsk ', ibsk
              conv = cg_eigen_solve( &
                            atm=a, & ! atoms
                            g=g, & ! coarse grid
                            kp=kpt(ikpt), & ! kpoint
                            vloc=vloc, & ! local potential
                            jspin=jspin, & ! spin index
                            energy=psi_state(ibsk+1:ibsk+global%nbnd)%ene, &
                            residual=psi_state(ibsk+1:ibsk+global%nbnd)%res, &
                            threshold=1E-9, & ! in energy units (Ha)
                            maxiter=12,&  ! psi_state(ibsk+ib)%maxiter, & ! maximum number of CG iterations
                            s=  spsi(:,ibsk+1:ibsk+global%nbnd), &
                            Ss=Sspsi(:,1:global%nbnd), &
                            ibnd=ibnd, & ! if ibnd is present, only this band will be optimized
                            precond=precond &
!cDBG                        , show_energy=(.not. use_di .and. .not. use_sr) &
                          )
#ifdef TIME
              t(I_CG) = t(I_CG) + Time_tic() ! sum up the lap time
#endif
            endif ! use_cg



            ! uses the Direct Inversion of the Iterative Subspace method
            ! to improve the eigenstates of the Kohn-Sham Hamiltonian
            conv = .false.
            if( use_di ) then
             if( nblock < 1 ) then
              conv = diis_eigen_solve( &
                                atm=a, & ! atoms
                                g=g, & ! coarse grid
                                kp=kpt(ikpt), & ! kpoint
                                vloc=vloc, & ! local potential
                                jspin=jspin, & ! spin index
                                s=spsi(:,ibsk+ibnd:ibsk+ibnd), & ! pass one wave function as rank2
                                energy=psi_state(ibsk+ibnd)%ene, &
                                residual=psi_state(ibsk+ibnd)%res, &
                                threshold=psi_state(ibsk+ibnd)%threshold, & ! in energy units (Ha)
                                mdiis=4, & ! max.# of DIIS iterations
                                ndiis=min(max(1,iiteration/2),4), & ! # of DIIS iterations ==> grows from 1 to 4 with the SCF it#
                                precon=precond, &
                                jbnd=psi_state(ibsk+ibnd)%jbnd & ! global band index
cDBG                            , show_energy=.false. &
                              )
             endif ! nblock < 1
#ifdef TIME
             t(I_DI) = t(I_DI) + Time_tic() ! sum up the lap time
#endif
            endif ! use_di
            if( .not. conv ) nfail = nfail+1 ! count up
          enddo ! ibnd

        endif ! use_di or cg


#ifdef PARALLELIZE_DIIS_BY_THREADS
        if( use_di ) then
          if( nblock > 0 ) then
            if(o>0) write(o,'(9(A,I0))') 'call diis in blocks of ', nblock, ' bands.'
            do ibnd = 0, (global%nbnd-1)/nblock
              ibnd1 = ibnd*nblock+1 ! update start band index
              ibndm = min( ibnd1+nblock-1, global%nbnd ) ! largest band index in this block
  !             if(o>0) write(o,'(9(A,I0))') 'call diis for bands(',ibnd1,':',ibndm,')'

              conv = diis_eigen_solve_many( &
                              atm=a, & ! atoms
                              g=g, & ! coarse grid
                              kp=kpt(ikpt), & ! kpoint
                              vloc=vloc, & ! local potential
                              jspin=jspin, & ! spin index
                              s=spsi(:,ibsk+ibnd1:ibsk+ibndm), & ! pass one wave function as rank2
                              energy=psi_state(ibsk+ibnd1:ibsk+ibndm)%ene, &
                              residual=psi_state(ibsk+ibnd1:ibsk+ibndm)%res, &
                              threshold=psi_state(ibsk+ibnd1:ibsk+ibndm)%threshold, & ! in energy units (Ha)
                              mdiis=4, & ! max.# of DIIS iterations
                              ndiis=min(max(1,iiteration/2),4), & ! # of DIIS iterations ==> grows from 1 to 4 with the SCF it#
                              precon=precond, &
                              jbnd=psi_state(ibsk+ibnd1:ibsk+ibndm)%jbnd & ! global band index
cDBG                          , show_energy=.false. &
                            )
            enddo ! ibnd
          endif ! use_di
#ifdef TIME
            t(I_DI) = t(I_DI) + Time_tic() ! sum up the lap time
#endif
        endif ! nblock < 1
#endif

        ! set up the Kohn-Sham Hamiltonian using the present set of wave functions
        ! as an efficient basis and diagonalize it directly.
        if( use_sr ) then
          !write(*,*) 'Im using subsp rot'
          sr = subspace_rotation( &
                                    g=g, & ! coarse grid
                                    vloc=vloc, & ! local potential
                                    jspin=jspin, & ! spin index
                                    atm=a, & ! atoms
                                    kp=kpt(ikpt), & ! kpoint
                                    energy=psi_state(ibsk+1:ibsk+global%nbnd)%ene, &
                                    s=spsi(:,ibsk+1:ibsk+global%nbnd), &
                                    Ss=Sspsi(:,1:global%nbnd+1), &
                                    band_comm=global%band_comm, &
                                    band_ioff=global%iobnd &
! cDBG                              , show_energy=.true. &
                                  )
!         if(o>0) write(o,'(3A,I4,A,99F10.6)') sym, fun, ' it', iiteration, ' E(Ha):', psi_state(ibsk+1:ibsk+min(global%nbnd,20))%ene
#ifdef TIME
          t(I_SR) = t(I_SR) + Time_tic() ! sum up the lap time
#endif
        elseif( use_cg ) then

          ! the energy tolerance ensures that (numerically) almost
          ! degenerate states do not lead to a reordering even though
          ! their order might be interchanged. this avoids oscillations
          call reorder( psi_state(ibsk+1:ibsk+global%nbnd)%ene, &
                        spsi(:,ibsk+1:ibsk+global%nbnd), &
                        energytolerance=1E-6 ) ! Ha
#ifdef TIME
          t(I_RO) = t(I_RO) + Time_tic() ! sum up the lap time
#endif
        endif ! use_sr or cg

cDBG    if(o>0) write(o,'(3A,I4,A,99F10.6)') sym, fun, ' e(Ha)', kpt(ikpt)%jk, ':', psi_state(ibsk+1:ibsk+min(global%nbnd,20))%ene
        ibsk = ibsk + global%nbnd ! set offset counter to the next spin-k-point set
      enddo ! ispn
    enddo ! ikpt

! cDBG  if( use_di ) then
! cDBG    write(44,'(I6,9999ES10.2E2)') iiteration, psi_state(:)%res
! cDBG  endif ! use_di

#ifdef _SHOW_MEMORY
    if(o>0) write(o,'(3A,I0)') 'print_memusage() line ', __FILE__ ,':', __LINE__
    call print_memusage()
#endif


#ifdef DEBUG
    res_min = -minval(psi_state(:)%res)
    res_max =  maxval(psi_state(:)%res)
    res_avg = ( sum(psi_state(:)%res) .MPIsum. g%equi_comm )/( size(psi_state,1) .MPIsum. g%equi_comm )
    res_min = -( res_min .MPImax. g%equi_comm )
    res_max =    res_max .MPImax. g%equi_comm
    if(o>0) write(o,'(3A,9ES16.6E2)') sym, fun, 'RESiduals: [min,avg,max]', res_min, res_avg, res_max
#endif
    occ_wgt = sum(psi_state(:)%occ*psi_state(:)%wgt) .MPIsum. g%equi_comm
    res_avg = sum(psi_state(:)%occ*psi_state(:)%wgt*psi_state(:)%res) .MPIsum. g%equi_comm
    res_avg = res_avg/occ_wgt

    residual = res_avg ! return value
cDBG  if(o>0) write(o,'(3A,ES10.2,A)') sym, fun, 'residual', residual

    nfail = nfail .MPIsum. g%equi_comm
cDBG  if( nfail > 0 .and. o>0) write(o,'(2A,I6,9A)') sym, fun, nfail, ' eigenstates did not converge properly.'

    call MPIbarrier( g%comm ) ! synchronization along the domain decomposition
    call MPIbarrier( g%equi_comm ) ! synchronization along then equivalent spaces
#ifdef TIME
    t(9) = Wtime() - t(0) ! total time
    if(o>0) write(o,'(2A,9(F10.3,A))') sym, fun, t(9)/real(global%nspn*global%nkpt), ' sec per set, tot. time', t(9), ' sec'
    t(0) = 0.
    ii=0 ; n=0
    if( use_xp ) then ; n=n+1 ; ii(n) = I_XP ; endif
    if( use_sd ) then ; n=n+1 ; ii(n) = I_SD ; endif
    if( use_cg ) then ; n=n+1 ; ii(n) = I_CG ; endif
    if( use_dav ) then ; n=n+1 ; ii(n) = I_DAV ; endif
    if( use_di ) then ; n=n+1 ; ii(n) = I_DI ; endif
    if( use_sr ) then ; n=n+1 ; ii(n) = I_SR ; endif
!   if( use_ro ) then ; n=n+1 ; ii(n) = I_RO ; endif ! reordering time neglected
    if(o>0) write(o,'(9A,9F10.3)') sym, fun, ( trim(METHOD(ii(i))), i=1,6 ), ' [ sec ]', t(ii(1:n))
#endif
  endfunction ! relax_eigenstates





#if R1_C2 == 1
  status_t function find_density_matrix_r( &
#else
  status_t function find_density_matrix_c( &
#endif
               a, cpsi, psi_state, nbnd, nspn, nkpt, equi_comm, me ) result( ist )
  !!!
  !!! This subroutine computes only the
  !!! real-symmetric part of the hermitian
  !!! atom centered density matrices.
  !!!
  use type_atom, only: atom, atomic_population
  use type_state, only: state
  use type_species, only: ELLMAX, ENNMAX
cNaN  use debugtools, only: NaN_search
  use MPItools, only: MPIallsum
#ifdef CONSTR
  use constraints, only: i_constraints
#endif
  implicit none
    ! parameters
    character(len=*), parameter     :: fun = ' find_density_matrix: '
    ! arguments
    type(atom), intent(inout)       :: a(:) ! atoms
    REAPLEX, intent(in)             :: cpsi(:,:) ! <p(alnm)|psi(bsk)>
    type(state), intent(in)         :: psi_state(:) ! occupation numbers and spin state
    integer, intent(in)             :: nbnd, nspn, nkpt
    integer, intent(in)             :: equi_comm !! communicator for state-parallelization
    integer, intent(in)             :: me !! grid rank for atom owner decision
    ! local vars
    real, allocatable               :: cp(:,:,:) ! temp array
    integer                         :: ia, ibsk, js, ispn, ikpt, ibnd
    integer                         :: i1, i2
    integer                         :: np, io, ns
    real                            :: occ
#ifdef CONSTR
    integer                         :: im ! mask index
    if(o>0) write(o,'(9A)') sym, fun, 'atoms use occupation numbers of their surrounding mask.'
#endif

cDBG  if( nbnd*nspn*nkpt /= size( cpsi, 2 ) ) stop 'find_density_matrix: NBND*NSPN*NKPT /= NBSK'
cNaN  call NaN_search( cpsi, fun, 'cpsi' )

!! Warning: do NOT loop over the atoms in OpenMP parallel, since their order matters in the MPI communication
!!          just open the parallel region here
!!! c$omp parallel
    do ia = 1, size(a)
      if( a(ia)%owner /= me ) cycle ! atom owner task
! c$omp single
#ifdef CONSTR
      im = i_constraints( a(ia)%ja ) ! index of the constraint mask for this atom
#endif
      ! abbreviations
      io = a(ia)%offprj ! offset of the projection coefficients for this atom in the c-vector
      np = a(ia)%s%mlnm ! number of projectors
      ns = ubound(a(ia)%Dm,3)
#ifdef DEBUG
      if( any( shape(a(ia)%Dm) /= (/np,np,1+ns/) ) ) stop 'find_density_matrix: shape a%Dm /= [NP,NP,NS]'
      if( any( psi_state(:)%jspn > min(2,ns) ) ) stop 'find_density_matrix: psi%JSPN out fo bounds [1,NS].'
#endif
      allocate( cp(np,np,1:ns), stat=ist )
      cp = 0. ! init temp array for a%Dm
! c$omp end single
! c$omp barrier
! c$omp do collapse(3) private(ikpt,ispn,ibnd,ibsk,js,occ,i1,i2) reduction(+:cp) schedule(dynamic)
      do ikpt = 1, nkpt
      do ispn = 1, nspn
      do ibnd = 1, nbnd
        ibsk = ibnd + nbnd*( (ispn-1) + nspn*(ikpt-1) ) ! total index for bands,spins,kpoint
        ! spin (up or down)
        js  = psi_state(ibsk)%jspn
        ! weighted occupation number
        occ = psi_state(ibsk)%occ * psi_state(ibsk)%wgt
#ifdef CONSTR
        occ = psi_state(ibsk)%cocc( im ) * psi_state(ibsk)%wgt ! use special occupation numbers of this mask
#endif
!         if(o>0) write(o,'(3A,4I4,F10.6)') sym, fun, 'ibnd, ispn, ikpt, jspn, occ', ibnd, ispn, ikpt, js, occ
        if( occ > 0. ) then ! the state is occupied

          do i2 = 1, np
            do i1 = 1, np ! here symmetry/hermiticity could be exploited
              cp(i1,i2,js) = cp(i1,i2,js) + occ * adb( cpsi(io+i1,ibsk), cpsi(io+i2,ibsk) )
            enddo ! i1
          enddo ! i2

cNaN    elseif( occ /= occ ) then ; stop 'SCF find_density_matrix: NaN in occupation number!'
cDBG    elseif( occ < 0. ) then ; stop 'SCF find_density_matrix: negative occupation number!'
        endif ! occ > 0

      enddo ! ibnd
      enddo ! ispn
      enddo ! ikpt
! c$omp end do
! c$omp barrier
! c$omp single

#ifdef DEBUG
      if( ibsk /= size(cpsi,2) ) stop 'find_density_matrix: combindex IBSK does not match dim#2 of CPSI after counting.'
      if( .not. associated( a(ia)%Dm ) ) then   ! this process does not own this atom
        if(o>0) write(o,'(3A,3(I6,A))') sym, fun, &
          'a%Dm is not allocated for atom j#', a(ia)%ja, ' i#', ia, ' proc#', me
        stop 'find_density_matrix: a%Dm not allocated, maybe process is not atom owner!'
      endif ! allocated( a%Dm )
#endif

      call MPIallsum( cp(:,:,:), equi_comm )
      a(ia)%Dm(:,:,1:ns) = cp

      deallocate( cp, stat=ist )

      if( ns > 1 ) &
      then ; a(ia)%Dm(:,:,0) = a(ia)%Dm(:,:,1) + a(ia)%Dm(:,:,2) ! spin
      else ; a(ia)%Dm(:,:,0) = a(ia)%Dm(:,:,1)                   ! no spin
      endif ! ns

      ! determine the atomic population of the different ell-channels
#ifdef DEBUG
      call atomic_population( a(ia), me, o )
#else
      call atomic_population( a(ia), me, 0 )
#endif
! c$omp end single
    enddo ! ia
! c$omp end parallel
    ist = 0
  contains

    real elemental function adb( a, b ) ! = real(a*conjg(b))
      REAPLEX, intent(in)  :: a, b
#if R1_C2 == 1
      adb = a * b
#else
!     adb = real( a * conjg( b ) )
      adb = real( a ) * real( b ) + aimag( a ) * aimag( b )
#endif
    endfunction adb

  endfunction ! find_density_matrix




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!! the smooth density is constructed on the coarse grid
  ! we generate the smooth density from
  ! the smooth wave functions on the coarse grid by
  !   rho(r)_dense = interpolate( sum_bsk |psi_bsk(r)_coarse|^2 )

#if R1_C2 == 1
  status_t function construct_smooth_density_r( &
#else
  status_t function construct_smooth_density_c( &
#endif
               g, nbnd, nspn, nkpt, psi_state, spsi, rho ) result( ist )
  use type_grid, only: grid
  use type_state, only: state
cNaN  use debugtools, only: NaN_search
  use MPItools, only: MPIallsum
#ifdef CONSTR
  use constraints, only: nc => n_constraints
  use constraints, only: mask
#endif
  implicit none
    ! parameters
    character(len=*), parameter     :: fun = ' construct_smooth_density: '
    ! arguments
    type(grid)                      :: g ! coarse grid descriptor
    integer, intent(in)             :: nbnd, nspn, nkpt
    type(state)                     :: psi_state(:)
    REAPLEX, intent(in)             :: spsi(:,:)
    real, intent(out)               :: rho(:,:,:,:) ! coarse grid density
    ! local vars
    integer                         :: nbsk, nxyzs, ibsk, incs, ibnd, ispn, ikpt, js
    real                            :: occ
#ifdef CONSTR
    real                            :: cw(nc) ! constraint weights
    integer                         :: ic
#endif

    nxyzs = size( spsi, 1 ) ! number of grid points
    nbsk = size( spsi, 2 ) ! number of states

cDBG  if( nbnd*nspn*nkpt /= nbsk ) stop 'construct_smooth_density: NBND*NSPN*NKPT /= NBSK'

cNaN  call NaN_search( spsi, fun, 'smooth wave functions' )
cNaN  call NaN_search( psi_state(:)%occ, fun, 'occupation numbers' )

    !
    ! \tilde{n} = <\tilde{\Psi}|  |\vec{r}><\vec{r}|  |\tilde{\Psi}>
    !
    ! \tilde{n}(\vec{r}) = \sum_{bsk} f_{bsk} * |\tilde{Psi}_{bsk}(\vec{r})|^2
    !
    ! bsk = runs over bands, (coll.) spins and kpoints
#ifdef CONSTR
    cw = 0.
#endif
    rho = 0. ! init
c$omp parallel do collapse(3) private(ikpt,ispn,ibnd,ibsk,js,occ) reduction(+:rho)
    do ikpt = 1, nkpt
      do ispn = 1, nspn
        do ibnd = 1, nbnd
          ibsk = ibnd + nbnd*( (ispn-1) + nspn*(ikpt-1) ) ! total index for bands,spins,kpoint

          js = psi_state(ibsk)%jspn ! global spin index
#ifdef CONSTR
          do ic = 1, nc
            occ = psi_state(ibsk)%cocc(ic) * psi_state(ibsk)%wgt
            cw(ic) = cw(ic) + occ * psi_state(ibsk)%cwgt(ic)
            rho(:,:,:,js) = rho(:,:,:,js) + occ * reshape( mask(:,ic)*abs2(spsi(:,ibsk)), g%ng(1:3) )
          enddo ! ic
#else
          occ = psi_state(ibsk)%occ * psi_state(ibsk)%wgt
          rho(:,:,:,js) = rho(:,:,:,js) + occ * reshape( abs2(spsi(:,ibsk)), g%ng(1:3) )
#endif
        enddo ! ibnd
      enddo ! ispn
    enddo ! ikpt
c$omp end parallel do

    call MPIallsum( rho, g%equi_comm )

#ifdef CONSTR
    call MPIallsum( cw, g%equi_comm )
    if(o>0) write(o,'(3A,99F12.6)') sym, fun, 'constraint weights  ', cw
#endif

cNaN  call NaN_search( rho, fun, 'smooth density' )
    ist = 0
  contains

    real elemental function abs2( rc )
    implicit none
      REAPLEX, intent(in)  :: rc
#if R1_C2 == 1
      abs2 = rc*rc
#else
      abs2 = real(rc)*real(rc) + aimag(rc)*aimag(rc)
#endif
    endfunction abs2

  endfunction ! construct_smooth_density

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




! until here, everything will be compiled twice
#if R1_C2 == 2
! start of the module tail part



  real function sum_of_eigenvalues( gc, psi ) result( E )
  use type_state, only: state
  use type_grid, only: grid
  use MPItools, only: operator(.MPIsum.)
  use unitsystem, only: eV, eV_
#ifdef CONSTR
  use constraints, only: nc => n_constraints
  use constraints, only: wc => w_constraints
#endif
  implicit none
    ! parameters
    character(len=*), parameter     :: fun = ' sum_of_eigenvalues: '
    ! arguments
    type(grid), intent(in)          :: gc      !! grid
    type(state), intent(in)         :: psi(:)  !! KS-state information

#ifdef CONSTR
    integer :: im
    E = 0.
    do im = 1, nc
      E = E + wc(im) * sum( psi(:)%cocc(im) * psi(:)%wgt * psi(:)%ene )
    enddo ! im
#else
    E = sum( psi(:)%occ * psi(:)%wgt * psi(:)%ene )
#endif
    E = E .MPIsum. gc%equi_comm ! reduction with other sets of states (kpoints,spins,bands)

  endfunction sum_of_eigenvalues



  real function totalenergy( sumeigvals, atm, ga, &
                             gd, rhov, vTOT, &
                             ge, rho_aug, ves, &
                             gx, rhot, eXC, ebar )
  use type_atom, only: atom
  use type_grid, only: grid
  use MPItools, only: operator(.MPIsum.), MPIallsum
  use unitsystem, only: eV, eV_
  !! warning, the two grids gd and ga have the same communicator !!
  implicit none
    ! parameters
    character(len=*), parameter     :: fun = ' totalenergy: '
    integer, parameter              :: I_T = I_ENE_TOT ! total energy
    integer, parameter              :: I_R = I_ENE_REF ! reference to isolated atoms
    integer, parameter              :: I_K = I_ENE_KIN ! kinetic energy
    integer, parameter              :: I_H = I_ENE_HTR ! hartree energy
    integer, parameter              :: I_X = I_ENE_EXC ! exchange correlation energy
    ! arguments
    real, intent(in)                :: sumeigvals
    type(atom), intent(in)          :: atm(:)        !! atoms
    type(grid), intent(in)          :: ga            !! grid descriptor for atoms, usually coarse grid

    type(grid), intent(in)          :: gd   !! density grid (coarse grid or dense grid)
    real, intent(in)                :: rhov(:,:,:,:) !! smooth valence density [on gd]
    real, intent(in)                :: vTOT(:,:,:,:) !! total local potential [on gd]

    type(grid), intent(in)          :: ge   !! extra dense grid
    real, intent(in)                :: rho_aug(:,:,:) !! augmented (neutral) density [on ge]
    real, intent(in)                :: ves(:,:,:)     !! electrostatic potential [on ge]

    type(grid), intent(in)          :: gx   !! grid
    real, intent(in)                :: rhot(:,:,:,:) !! smooth total density [on gx]
    real, intent(in)                :: eXC(:,:,:)    !! exchange-correlation energy density [on gx]
    real, intent(in)                :: ebar    !! energy due to rhot*vbar


    ! local vars
    real                            :: el(I_T:I_X), e(I_T:I_X), ea(I_T:I_X), e_adc, e_ldc, e_dc
    integer                         :: nspins, ispn, ia, natm
    real, save                      :: Eprev = 0. ! energy in previous iteration
    real                            :: deltaE
    real                            :: ehub !hubbard energy

cDBG  integer                       :: id

    nspins = size( rhov, 4 )   ! number of collinear spins
    natm = size( atm, 1 )   ! number of atoms

    e_adc = 0. ! init atomic double counting correction
    do ia = 1, natm
      ! atomic energy corrections
      if( atm(ia)%owner == ga%rank ) & ! contributions only from the owner process
        e_adc = e_adc + atomic_double_counting_corrections( atm(ia) ) * atm(ia)%symmetry_factor
    enddo ! ia
    e_adc = e_adc .MPIsum. ga%comm ! reduction over the unit cell

#ifdef DEBUG
    do id = 1, 3
      if( size(rhov,id) /= gd%ng(id) ) stop 'SCF totalenergy: RHOV has been passed on the wrong grid!'
      if( size(vTOT,id) /= gd%ng(id) ) stop 'SCF totalenergy: VTOT has been passed on the wrong grid!'
    enddo ! id
#endif


    ! double counting on the real-space grid
    e_ldc = ( sum( vTOT * rhov ) .MPIsum. gd%comm ) * gd%hvol ! integral valence rho * V (effective local potential)

    ! total double counting
    e_dc = e_ldc + e_adc

    ! the kinetic energy of the smooth wave functions is
    ! given by the eigenvalue sum minus double counting
    el(I_K) = sumeigvals - e_dc

    ! e(I_K) does not need to be reduced along the space comm any more,
    ! because it results from the (globally known) eigenvalues.

#ifdef DEBUG
    do id = 1, 3
      if( size(rho_aug,id) /= ge%ng(id) ) stop 'SCF totalenergy: RHO_AUG has been passed on the wrong grid!'
      if( size(ves,id) /= ge%ng(id) ) stop 'SCF totalenergy: VES has been passed on the wrong grid!'
    enddo ! id
#endif
    ! the smooth Hartree and Coulomb energy on the dense grid
    el(I_H) = 0.5 * ( sum( rho_aug * ves ) .MPIsum. ge%comm ) * ge%hvol ! 1/2 integral rho * V

#ifdef DEBUG
    do id = 1, 3
      if( size(rhot,id) /= gx%ng(id) ) stop 'SCF totalenergy: RHOT has been passed on the wrong grid!'
      if( size(eXC,id) /= gx%ng(id) ) stop 'SCF totalenergy: EXC has been passed on the wrong grid!'
    enddo ! id
#endif

    el(I_X) = 0. ! init
    do ispn = 1, nspins
      ! the exchange correlation energy
      el(I_X) = el(I_X) + sum( rhot(:,:,:,ispn) * eXC ) * gx%hvol ! no reduction, local sum
    enddo ! ispn
    el(I_X) = el(I_X) .MPIsum. gx%comm ! reduction

    ! atomic energy contributions
    ea(I_T:I_X) = 0. ! init
    do ia = 1, natm
      if( atm(ia)%owner == ga%rank ) & ! contributions only from the owner process
        ea(I_T:I_X) = ea(I_T:I_X) + atomic_energy_corrections( atm(ia) ) * atm(ia)%symmetry_factor
    enddo ! ia
    call MPIallsum( ea, ga%comm )

    ! the total energy is the sum of
    !   kinetic + Hartree + ExchangeCorrelation + atomic corrections
    e(I_T) = el(I_K) + el(I_H) + el(I_X) + ea(I_T) + ebar
    ! reference energy: total energy reduced by AE energy of the isolated atoms
    e(I_R) = el(I_K) + el(I_H) + el(I_X) + ea(I_R) + ebar

    ! each contribution
    e(I_K:I_X) = el(I_K:I_X) + ea(I_K:I_X)

    totalenergy = e(I_R)  ! return value == reference energy

    !! now add the Hubbard atomic corrections (+ U)
    ehub = 0.
    do ia = 1, natm
      if( atm(ia)%owner == ga%rank ) & ! contributions only from the owner process
        ehub = ehub + atomic_hubbard_corrections( atm(ia) ) * atm(ia)%symmetry_factor
    enddo ! ia

    ehub = ehub .MPIsum. ga%comm
    totalenergy = totalenergy + ehub

    ! change
    deltaE = totalenergy - Eprev ; if( Eprev == 0. ) deltaE = 0.
    Eprev  = totalenergy ! store for the next iteration

    if( o <= 0 ) return
    write(o,fmt='(A)')      '', '   -------------------------------------------'
    write(o,fmt='(A,F32.16,A)') '     Eeig', sumeigvals*eV, eV_
    write(o,fmt='(A)')          '   -------------------------------------------'
#ifdef DEBUG
    write(o,fmt='(A,F32.16,A)') '     Edc ',  e_dc *eV, eV_
    write(o,fmt='(A,F32.16,A)') '     Ldc ',  e_ldc*eV, eV_
    write(o,fmt='(A,F32.16,A)') '     Adc ',  e_adc*eV, eV_
    write(o,fmt='(A)')          '   -------------------------------------------'
    ! local(grid)
    write(o,fmt='(A,F32.16,A)') '     Lkin',el(I_K)*eV, eV_
    write(o,fmt='(A,F32.16,A)') '     Les ',el(I_H)*eV, eV_
    write(o,fmt='(A,F32.16,A)') '     Lxc ',el(I_X)*eV, eV_
    ! atomic(sphere)
    write(o,fmt='(A,F32.16,A)') '     Akin',ea(I_K)*eV, eV_
    write(o,fmt='(A,F32.16,A)') '     Aes ',ea(I_H)*eV, eV_
    write(o,fmt='(A,F32.16,A)') '     Axc ',ea(I_X)*eV, eV_
    write(o,fmt='(A)')          '   -------------------------------------------'
#endif
    write(o,fmt='(A,F32.16,A)') '     Ekin', e(I_K)*eV, eV_
    write(o,fmt='(A,F32.16,A)') '     Ees ', e(I_H)*eV, eV_
    write(o,fmt='(A,F32.16,A)') '     Exc ', e(I_X)*eV, eV_
    write(o,fmt='(A)')          '   -------------------------------------------'
    write(o,fmt='(A,F32.16,A)') '     Eall', e(I_T)*eV, eV_
    write(o,fmt='(A,F32.16,A)') '     Etot', totalenergy*eV, eV_
    write(o,fmt='(A)')          '   -------------------------------------------'
    write(o,fmt='(A,F32.16,A)') '     E-Ep', deltaE*eV, eV_
    write(o,fmt='(A)')          '   -------------------------------------------', ''

  endfunction totalenergy


  function atomic_energy_corrections( a ) result( aEc )
  use type_atom, only: atom
  use type_species, only: I_TRU, I_SMT
  use unitsystem, only: eV, eV_
  implicit none
    ! parameters
    character(len=*), parameter     :: fun = ' atomic_energy_corrections: '
    type(atom), intent(in)          :: a
    real                            :: aEc(I_ENE_TOT:I_ENE_EXC)

    real                            :: t, MB
    integer                         :: i1, i2, i3, i4
    integer                         :: nspins

    aEc = 0. ! init
    ! each contribution separately (for display only)
    aEc(I_ENE_KIN) = a%s%E_core_kin + atomic_kinetic_energy_corrections( a )! kinetic energy of the core electrons

    nspins = ubound(a%Dm, 3)

    !!vbar correction
    t = 0.
    do i1=1, a%s%mlnm
      do i2=1, a%s%mlnm
         t = t+ a%Dm(i2,i1,0)*a%s%MB_p_full(i2,i1)
      enddo
    enddo
    MB = t
    !write(*,*) 'MB atomic corr', t + a%s%MB

 !! compute electrostatic energy correction using the corection matrices and tensors
    aEc(I_ENE_HTR) = a%s%coulomb_M !+ a%s%es_energy
    t = 0.
    do i1=1, a%s%mlnm
      do i2=1, a%s%mlnm
         t = t+ a%Dm(i2,i1,0)*a%s%M_p_full(i2,i1)
      enddo
    enddo
    !write(*,*) 'atomic hartree correction Mp = ', t
    aEc(I_ENE_HTR) =  aEc(I_ENE_HTR) + t

    t = 0.
    do i1=1, a%s%mlnm
      do i2=1, a%s%mlnm
        do i3=1, a%s%mlnm
          do i4=1, a%s%mlnm
             t = t + a%Dm(i1,i2,0)*a%s%M_ijkl(i1, i2, i4, i3)*a%Dm(i4,i3,0)
          enddo
        enddo
      enddo
    enddo
    !write(*,*) 'atomic hartree correction Mpp = ', t

    aEc(I_ENE_HTR) = aEc(I_ENE_HTR) + t

    aEc(I_ENE_HTR) = aEc(I_ENE_HTR) + a%s%es_energy !- a%s%delta_zcore
    !write(*,*) 'atomic hartree correction = ', (aEc(I_ENE_HTR))*ev, (a%Ees(I_TRU)-a%Ees(I_SMT) - a%s%delta_zcore)*ev, aEc(I_ENE_HTR)- a%s%delta_zcore - a%s%es_energy



    !! uncomment this below to restore the old way correction
    !aEc(I_ENE_HTR) = a%Ees(I_TRU)-a%Ees(I_SMT) - a%s%delta_zcore


    aEc(I_ENE_EXC) = a%Exc(I_TRU) - a%Exc(I_SMT)

    ! sum of all contributions
    aEc(I_ENE_TOT) = aEc(I_ENE_KIN) + aEc(I_ENE_HTR) + aEc(I_ENE_EXC) + MB + a%s%MB
    ! reference is the self-consistent total energy of the isolated atomic setup
    !  ... that was used to generate the PAW data

    aEc(I_ENE_REF) = aEc(I_ENE_TOT) -a%s%Eatom
    ! a%Ees has been computed in the module radialHartree during the evaluation
    ! of the local Hartree+Coulomb potential in the spheres

    if(nspins > 1) then
      t = 0.
      do i1=1, a%s%mlnm
        do i2=1, a%s%mlnm
          t = t- a%Dm(i2,i1,1)*a%s%delta_n_matrix_full(i2,i1)
        enddo
      enddo

      do i1=1, a%s%mlnm
        do i2=1, a%s%mlnm
          t = t+ a%Dm(i2,i1,2)*a%s%delta_n_matrix_full(i2,i1)
        enddo
      enddo


cDBG      write(*,'(3a,i4,a,es16.8)') sym, fun, 'MAG atm', a%ja , ' = ', t
    endif
    !write(*,*) 'tote_atom', a%s%Eatom*ev, ev

  endfunction atomic_energy_corrections


  function atomic_hubbard_corrections( a ) result( ehu )
  use type_atom, only: atom
  use type_species, only: I_TRU, I_SMT
  implicit none
    type(atom), intent(in)          :: a
    real                            :: ehu

    real                            :: ehu_p
    real                            :: tr, tmp
    integer                         :: i, j, is, ns


    ehu = 0.

    ns = size(a%Hm, 3)

    !! add the hubbard energy correction
    do is=1, ns
      do i=1, a%s%mlnm
        if(a%s%ind_ell(i) .eq. 2) then
          ehu = ehu + a%Dm(i,i,is)
          tmp = 0.
          do j=1, a%s%mlnm
            if (a%s%ind_ell(j) .eq. 2) then
              tmp = tmp + a%Dm(i,j,is)*a%Dm(j,i,is)
            endif
          enddo !j
          ehu = ehu - tmp
        endif !i l==2
      enddo
    enddo

    !!hubbard_U_J
    ehu = 0.5*a%hubbard_U_J*ehu


    ehu_p = 0.0
    !! add the hubbard energy correction
    do is=1, ns
      do i=1, a%s%mlnm
        if(a%s%ind_ell(i) .eq. 1) then
          ehu_p = ehu_p + a%Dm(i,i,is)
          tmp = 0.
          do j=1, a%s%mlnm
            if (a%s%ind_ell(j) .eq. 1) then
              tmp = tmp + a%Dm(i,j,is)*a%Dm(j,i,is)
            endif
          enddo !j
          ehu_p = ehu_p - tmp
        endif !i l==1
      enddo
    enddo

    !!hubbard_U_J
    ehu_p = 0.5*a%hubbard_U_J_p*ehu_p

    !! add to the result for d to get the total d+p
    ehu = ehu + ehu_p

  endfunction atomic_hubbard_corrections


  real function atomic_double_counting_corrections( a ) result( e )
  use type_atom, only: atom
  implicit none
    type(atom), intent(in)          :: a ! atom

    real                            :: tb, tk


    tb = sum( a%Hm(:,:,1:) * a%Dm(:,:,1:) )
    !tk = atomic_kinetic_energy_corrections( a )
    e = tb !- tk

  endfunction atomic_double_counting_corrections



  real function atomic_kinetic_energy_corrections( a ) result( e )
  use type_atom, only: atom
  implicit none
    type(atom), intent(in)          :: a ! atom

    e = sum( a%s%dkin2 * a%Dm(:,:,0) )

  endfunction atomic_kinetic_energy_corrections




  !! a%GntDm = Gnt * a%Dm
  status_t function setup_radial_coefficients( a, comm ) result( ist )
  use type_atom, only: atom
  use MPItools, only: MPImyrank
  implicit none
    ! parameters
    character(len=*), parameter           :: fun = ' setup_radial_coefficients: '
    ! arguments
    type(atom), intent(inout)             :: a(:)
    MPI_Comm, intent(in)                  :: comm
    ! local vars
    integer                               :: ia, me

    ist = 0
    me = MPImyrank( comm )
! c$omp parallel do private(ia) schedule(dynamic,1)
    do ia = 1, size( a, 1 )
      if( a(ia)%owner /= me ) cycle ! atom owner task

      !  construct the coefficients for the radial charge densities
      !  from the atom-centered density matrix a%Dm
      ist = set_rad_coef( a(ia) )

    enddo ! ia
! c$omp end parallel do

  endfunction setup_radial_coefficients



  !! a%GntDm = Gnt * a%Dm
  status_t function set_rad_coef( a ) result( ist )
  use type_species, only: species, I_TRU, I_SMT, SPIN
  use type_species, only: I_RHOC
  use type_atom, only: atom
  use spherical, only: Gnt => gaunt_tensor
cNaN  use debugtools, only: NaN_search
  implicit none
    ! parameters
    character(len=*), parameter           :: fun = ' setup_radial_coefficients: '
    ! arguments
    type(atom), intent(inout)             :: a
    ! local vars
    integer                               :: is, ns, i1, i2, np, iln1, iln2, ilm1, ilm2
    integer                               :: mln, ilm, mlm2, ml

    !  construct the coefficients for the radial charge densities
    !  from the atom-centered density matrix a%Dm

    ns   = ubound( a%Dm, 3 )     ! spin
    ml   = size( a%s%rcmp, 2 )-1 ! max ell
    mln  = a%s%mln               ! max ell#enn index
    mlm2 = min( size( a%rho, 2 ), 49 ) ! max ell#emm index
    np   = a%s%mlnm              ! max ell#enn#emm index

#ifdef DEBUG
    if( .not. associated( a%GntDm ) ) stop 'setup_radial_coefficients: ERROR: in owner process, a%GntDm is not allocated!'
#endif
#define gd a%GntDm
    gd = 0. ! init

    do is = 1, ns
      !--------------------------------------
      do ilm = 1, mlm2
        ! contract Gaunt-coefficients with density matrix over emm-states
        do i1 = 1, np

          ilm1 = a%s%ind_ilm(i1) ! find the ilm index of the spherical harmonic function (combindex ell#emm)
          iln1 = a%s%ind_iln(i1) ! find the iln index of the partial wave (combindex ell#enn)

          !--------------------------------------------------------------------
#ifdef EXPLOIT_SYM
          do i2 = i1, 1, -1 ! triangular loop
#else
          do i2 = 1, np
#endif
            ilm2 = a%s%ind_ilm(i2) ! find the ilm index of the spherical harmonic function (combindex ell#emm)
            iln2 = a%s%ind_iln(i2) ! find the iln index of the partial wave (combindex ell#enn)
            ! contract the density matrix with the Gaunt-coefficients over all emm-states
            gd(iln2,iln1,ilm,is) = gd(iln2,iln1,ilm,is) + Gnt(ilm1,ilm2,ilm) * a%Dm(i2,i1,is)
#ifdef EXPLOIT_SYM
            gd(iln1,iln2,ilm,is) = gd(iln2,iln1,ilm,is) ! symmetric
#endif
          enddo ! ilnm2
          !--------------------------------------------------------------------
        enddo ! ilnm1
        ! now gd is set
      enddo ! ilm
      !--------------------------------------
    enddo ! is
cNaN  call NaN_search( gd(:,:,:,:), fun, 'temp. matrix GntDm = gaunt*a%dm' )
#undef gd
    ist = 0
  endfunction set_rad_coef




  !! a%rho = a%s%rwf * a%GntDm * a%s%rwf
  status_t function setup_radial_densities( a, comm ) result( ist )
  use type_species, only: species, I_TRU, I_SMT, I_RHOC, SPIN
  use type_atom, only: atom
!   use MPItools, only: MPIbcast0
  use MPItools, only: MPImyrank
  use atomicomm, only: AtomBcast
cNaN  use debugtools, only: NaN_search
cDBG  use constants, only: sqrt4pi
  implicit none
    ! parameters
    character(len=*), parameter           :: fun = ' setup_radial_densities: '
    ! arguments
    type(atom), intent(inout)             :: a(:)
    MPI_Comm, intent(in)                  :: comm
    ! local vars
    integer                               :: ia, me

    me = MPImyrank( comm )
    ist = 0
! c$omp parallel do private(ia) schedule(dynamic,1)
    do ia = 1, size(a)
      if( a(ia)%owner /= me ) cycle ! atom owner task
      !  construct the radial SMT and TRU charge densities
      !  with GntDm tensor (the atom-centered density matrix contracted with the Gaunt coefficients)
      !  and the smooth partial waves(SMT) and all electron partial waves(TRU), respectively
      ist = set_rad_rho( a(ia) )

    enddo ! ia
! c$omp parallel do

    ! communicate the compensation multipole moments qlm to the
    ! other processes that contribute to this atom
    do ia = 1, size(a)
      call AtomBcast( a(ia)%qlm, comm, a(ia)%owner, a(ia)%plist )
cDBG  if(o>0) write(o,'(3A,I0,A,F18.12)') sym, fun, 'a#', a(ia)%ja, ' q00 =', a(ia)%qlm(1)*SQRT4PI
    enddo ! ia

  endfunction setup_radial_densities


  status_t function set_rad_rho( a ) result( ist )
  use type_species, only: species, I_TRU, I_SMT, I_RHOC, SPIN
  use type_atom, only: atom
  use atomicomm, only: AtomBcast
cNaN  use debugtools, only: NaN_search
cDBG  use constants, only: sqrt4pi
  implicit none
    ! parameters
    character(len=*), parameter           :: fun = ' set_rad_rho: '
    ! arguments
    type(atom), intent(inout)             :: a
    ! local vars
    integer                               :: ns, is, iq, nr
    integer                               :: iln1, iln2!, ilm1, ilm2
    integer                               :: mln, ilm, mlm, ell, ellmax, emm
#ifdef EXPLOIT_SYM
    ! exploit the symmetry that the density matrix is hermitian
    ! and only the real part is stored, so that is symmetric
    real                                  :: f ! factor in {2.,1.}
#else
    real, parameter                       :: f = 1.0
#endif

    !  construct the radial SMT and TRU charge densities
    !  with GntDm tensor (the atom-centered density matrix contracted with the Gaunt coefficients)
    !  and the smooth partial waves(SMT) and all electron partial waves(TRU), respectively
    ns = ubound( a%GntDm, 4 )

cDBG  if( .not. associated( a%rho ) ) stop 'set_rad_rho: ERROR: in owner process, a%rho is not allocated!'
    a%rho(:,:,:,:) = 0. ! clear

    ellmax = a%s%ellmax         ! max ell
    mln = a%s%mln               ! max ell#enn index
    mlm = (2*ellmax+1)**2       ! max ell#emm index
    nr = a%s%nr                 ! number of radial grid points

    do is = 1, ns
      ! add the core densities (SMT and TRU)
      ! spherical symmetry of the core densities is assumed.
      ! I_RHOC is an offset, i.e. I_RHOC+ISMT --> smooth core density, I_RHOC+I_TRU --> true core density
      ilm = 1 ! ==> ell=0, emm=0, monopole
      do iq = I_TRU, I_SMT
        if( ns > 1 ) then ; a%rho(0:,1,is,iq) = 0.5 * a%s%rf(0:nr,I_RHOC+iq)
        else              ; a%rho(0:,1,is,iq) =       a%s%rf(0:nr,I_RHOC+iq)
        endif ! ns > 1
      enddo ! iq

      !--------------------------------------
      do ilm = 1, mlm
        do iq = I_TRU, I_SMT ! for the two quantities
          ! now sum up all radial wave functions
          do iln1 = 1, mln

            !--------------------------------------------------
#ifdef EXPLOIT_SYM
            ! run only over the lower triangular matrix
            f = 1. ! multiplicity for diagonal elements (first in the sum)
            do iln2 = iln1, 1, -1 ! triangular loop
#else
            do iln2 = 1, mln
#endif
              ! for all radial entries
              if( a%GntDm(iln2,iln1,ilm,is) /= 0. ) then

                a%rho(0:nr,ilm,is,iq) = a%rho(0:nr,ilm,is,iq) + f * &
                  a%GntDm(iln2,iln1,ilm,is) * &
                  a%s%rwf(0:nr,iln1,SPIN,iq) * &
                  a%s%rwf(0:nr,iln2,SPIN,iq)

              endif ! entry nonzero
#ifdef EXPLOIT_SYM
              f = 2. ! multiplicity for off-diagonal elements (from second)
#endif
            enddo ! iln2
            !--------------------------------------------------

          enddo ! iln1
        enddo ! iq
      enddo ! ilm
      !--------------------------------------
    enddo ! is

cNaN  call NaN_search( a%rho, fun, 'a%rho (cor+val)' )

    ! the total charge density rho(:,:,0,:) spin integrated
    do iq = I_TRU, I_SMT ! for the two quantities
      selectcase( ns )
      case( 1 ) ; a%rho(0:,:,0,iq) = a%rho(0:,:,1,iq)
      case( 2 ) ; a%rho(0:,:,0,iq) = a%rho(0:,:,1,iq) + a%rho(0:,:,2,iq)
      case default ; stop 'SCF set_rad_rho: NS should be in {1,2} (rho0).'
      endselect ! nspin
    enddo ! iq

cNaN  call NaN_search( a%rho, fun, 'a%rho_aug' )

    ! generate the qlm (multipole deficit moments)
    ilm = 0
    do ell = 0, 2*ellmax
      do emm = -ell, ell
        ilm = ilm+1

        a%qlm(ilm) = sum( a%s%chdm(:,:,ell) * a%GntDm(:,:,ilm,1) ) ! is=1
        if( ns > 1 ) a%qlm(ilm) = a%qlm(ilm) & !  second spin
                   + sum( a%s%chdm(:,:,ell) * a%GntDm(:,:,ilm,2) ) ! is=2

      enddo ! emm
    enddo ! ell

cDBG  if(o>0) write(o,'(3A,I0,9(A,F0.16))') sym, fun, 'atom #',a%ja,' q00(static)= ', a%s%q00*sqrt4pi, ' (dynamic)= ', a%qlm(1)*sqrt4pi
    ! monopole moment (ilm=1: ell=0, emm=0) contains the core charge deficit
    a%qlm(1) = a%qlm(1) + a%s%q00

#ifdef NOHartreePotential
    a%rho(:,:,0,:) = 0. ! rho_aug = 0.
    a%qlm = 0. ; a%qlm(1) = -a%s%Z/sqrt4pi
    if(o>0) write(o,'(9A)') sym, fun, 'NOHartreePotential activated: rhoaug = -Z*cmp'
#endif

cNaN  call NaN_search( a%qlm, fun, 'a%qlm' )

    ! add the compensation charge densities on the radial grid
    ! rho(:,ilm,0,:) are the TRU and SMT augmented densities
    ilm = 0
    do ell = 0, 2*ellmax
      do emm = -ell, ell
        ilm = ilm+1
!         ilm = ell*(ell+1)+emm+1
        a%rho(0:,ilm,0,I_SMT) = a%rho(0:,ilm,0,I_SMT) + a%qlm(ilm) * a%s%rcmp(0:,ell)
      enddo ! emm
    enddo ! ell

    ist = 0 ! result
  endfunction set_rad_rho




  status_t function Coulomb_multipole_moments( a, g, Ves, comm ) result( ist )
  use type_atom, only: atom
  use type_grid, only: grid
  use type_comp, only: project
cNaN  use debugtools, only: NaN_search
  use radial_Hartree, only: es_in_the_spheres
  use atomicomm, only: AtomReduce
  use MPItools, only: MPImyrank
  implicit none
    ! parameter
    character(len=*), parameter           :: fun = ' Coulomb_multipole_moments: '
    integer, parameter                    :: ellmax = 9 ! large enough
    ! arguments
    type(atom), intent(inout)             :: a(:) ! atoms
    type(grid), intent(in)                :: g ! grid
    real, intent(in)                      :: Ves(:,:,:) ! electrostatic potential on grid g
    MPI_Comm, intent(in)                  :: comm
    ! local vars
    integer                               :: me, ia, mlm2!, ell, ilm, emm, nr
    real                                  :: vlm((ellmax+1)**2)

    me = MPImyrank( comm )
    do ia = 1, size(a)

      ! evaluate the electrostatic potential of TRU, SMT density in the spheres
      ! and align the potential to the electrostatic potential Ves on the grid
      ! by a projection to the compensators.
      ! Compensators are normalized as
      !   /
      !   |d^3r cmp_{\ell m}(r) Y_{\ell'm'}(\hat r)|r|^ell = \delta_{\ell \ell'} \delta_{m m'}
      !   /
      !

      ! on the dense grid (gd):
      vlm = 0. ! init
      mlm2 = (2*a(ia)%s%ellmax+1)**2

      ! uhlm = <cmp|ves>

      ! project into the variable vlm
      call project( cmp=a(ia)%cmp, Ves=Ves, c=vlm(1:mlm2), dV=g%hvol )

cNaN  call NaN_search( vlm, fun, 'a%vlm (loc)' )

      call AtomReduce( vlm(1:mlm2), comm, a(ia)%owner, a(ia)%plist )

      if( me == a(ia)%owner ) then

        a(ia)%vlm = vlm(1:mlm2) ! store the multipole moments of the electrostatic potential
cNaN    call NaN_search( a(ia)%vlm, fun, 'a%uhlm (own)' )

        ! compute the electrostatic potentials on the radial grid
        ! and align them with the correct potential via a%vlm(1:mlm2)
        ist = es_in_the_spheres( a(ia) )

      endif ! me == owner

    enddo ! ia

  endfunction Coulomb_multipole_moments



  status_t function xc_in_the_spheres( a, key, nspins, me ) result( ist )
  ! transform the two radial densities (TRU and SMT) onto
  ! points in the sphere to enable the evaluation of the
  ! exchange correlation potential in realspace
  ! !!!
  ! !!! the xc-subroutines reqires rank4 input, so
  ! !!! an alternative is to put both quantities
  ! !!! (true and smooth) onto one grid at the same
  ! !!! time and call xc only once.
  use type_species, only: I_TRU, I_SMT
  use type_atom, only: atom
  use density_functionals, only: xc_potential
  use density_functionals, only: is_gradient_functional
  use spherical, only: Xlm_transform, nPoints
cNaN  use debugtools, only: NaN_search
  implicit none
    ! parameters
    character(len=*), parameter           :: fun = ' xc_in_the_spheres: '
    integer, parameter                    :: I_EXC = 0 ! the 0-entry of the spin index
    ! arguments
    type(atom), intent(inout)             :: a(:)
    integer, intent(in)                   :: key
    integer, intent(in)                   :: nspins
    integer, intent(in)                   :: me
    ! local vars
    integer                               :: iq, is, nr, npts, ia!, ip
    real, allocatable                     :: rho_p(:,:,:,:) ! the 3rd rank is only because
    real, allocatable                     :: vxc_p(:,:,:,:) ! the XC routines need the spin in dim#4
    real, allocatable                     :: drho(:,:,:,:,:) ! the XC routines need the spin in dim#5
    logical                               :: deriv
cDBG  logical :: warn_rho_less_than_zero = .false.
    integer :: ir
    integer :: ilm, mlm
    real :: t

    deriv = is_gradient_functional( key ) ! functional is a GGA

    do ia = 1, size(a) ; if( a(ia)%owner /= me ) cycle

      !! TODO introduce an additional one-time warning if the number of
      !! spherical grid points (ntransform) is too small to sample (2*a%s%ellmax+1)**2 spherical harmonics

      nr = size(a(ia)%rho,1) ! number of radial grid points
cNaN  call NaN_search( a(ia)%rho, fun, 'a%rho' )

      npts = nPoints ! number of points sampling a sphere shell
      if( deriv ) then ; allocate( rho_p(npts,nr,1:1,-1:+1), vxc_p(npts,nr,1:1,0:nspins), drho(npts,nr,1:1,1:3,-1:+1), stat=ist )
      else             ; allocate( rho_p(nr,npts,1:1,nspins), vxc_p(nr,npts,1:1,0:nspins), stat=ist )
      endif ! deriv

      ! for both quantities, true and smooth charge density
      do iq = I_TRU, I_SMT, I_SMT-I_TRU

          ! transform rho from multipoles to points on the sphere
        if( deriv ) then ! GGA
          if( nspins > 1 ) then
            ist = Xlm_transform( a(ia)%rho(:,:,1,iq),                     rho_p(:,:,1,+1), a(ia)%s%g%d, a(ia)%s%g%r, a(ia)%s%g%dr, drho(:,:,:,:,+1) )
            ist = Xlm_transform( a(ia)%rho(:,:,1,iq)+a(ia)%rho(:,:,2,iq), rho_p(:,:,1, 0), a(ia)%s%g%d, a(ia)%s%g%r, a(ia)%s%g%dr, drho(:,:,:,:, 0) )
            ist = Xlm_transform( a(ia)%rho(:,:,2,iq),                     rho_p(:,:,1,-1), a(ia)%s%g%d, a(ia)%s%g%r, a(ia)%s%g%dr, drho(:,:,:,:,-1) )
          else  ! spin
            ist = Xlm_transform( a(ia)%rho(:,:,1,iq)/2., rho_p(:,:,1,+1), a(ia)%s%g%d, a(ia)%s%g%r, a(ia)%s%g%dr, drho(:,:,:,:,+1) )
            ist = Xlm_transform( a(ia)%rho(:,:,1,iq)   , rho_p(:,:,1, 0), a(ia)%s%g%d, a(ia)%s%g%r, a(ia)%s%g%dr, drho(:,:,:,:, 0) )
!           ist = Xlm_transform( a(ia)%rho(:,:,1,iq)/2., rho_p(:,:,1,-1), a(ia)%s%g%d, a(ia)%s%g%r, a(ia)%s%g%dr, drho(:,:,:,:,-1) )
            rho_p(:,:,1,-1) = rho_p(:,:,1,+1) ; drho(:,:,:,:,-1) = drho(:,:,:,:,+1) ! copy dn-spin from up-spin (faster than calling transform again)
          endif ! spin
        else  ! deriv
          do is = 1, nspins
            ist = Xlm_transform( ri=a(ia)%rho(:,:,is,iq), ro=rho_p(:,:,1,is), forward=.true. )
          enddo ! is
        endif ! deriv

cNaN    call NaN_search( rho_p, fun, 'rho_p = density on the sphere points' )
cDBG    if( any(rho_p < 0.) ) warn_rho_less_than_zero = .true.

        ! evaluate the exchange correlation potential in real-space
        ist = xc_potential( rho=rho_p, vxc=vxc_p(:,:,:,1:nspins), exc=vxc_p(:,:,:,I_EXC), &
                           key=key, drho=drho )
cNaN    call NaN_search( vxc_p, fun, 'vxc_p = potential on sphere points' )

        do is = I_EXC, nspins ! is == 0 (I_EXC) stands for the XC energy density (I_EXC)
          ! back-transform vxc from points on the sphere to multipoles
          if( deriv ) then ! for GGAs, the index order is ip,ir in vxc_p
            ist = Xlm_transform( pr=vxc_p(:,:,1,is), rm=a(ia)%pot(:,:,is,iq) ) ! watch argument names
          else  ! deriv
            ist = Xlm_transform( ri=vxc_p(:,:,1,is), ro=a(ia)%pot(:,:,is,iq), forward=.false. )
          endif ! deriv
        enddo ! is

      enddo ! iq

      deallocate( rho_p, vxc_p, stat=ist ) ; if( deriv ) deallocate( drho, stat=ist )
#ifdef DEBUG_GGA
      if( deriv ) then
        write(842,'(3ES24.16)') ( a(ia)%s%g%r(ir), a(ia)%pot(ir,1,1,:)*.2820948, ir=1,a(ia)%s%nr )
        write(842,'(A)') ''
      endif
#endif
cNaN  call NaN_search( a(ia)%pot, fun, 'a%pot = potential in the sphere' )
cDBG  if( warn_rho_less_than_zero .and. o>0) write(o,'(3A,I0)') sym, fun, 'rho < 0 on sphere points for atom #', a(ia)%ja

!     ! local vars
!     real                            :: t
!     integer                         :: is, nr, iq, ilm, mlm, nspins
!

!     nspins = ubound( a(ia)%Dm, 3 )
      mlm  = size( a(ia)%rho, 2 )
      nr   = a(ia)%s%nr
      do iq = I_TRU, I_SMT, I_SMT-I_TRU
        t = 0.
        do is = 1, nspins
          !                /R                           /4Pi
          !  Exc = sum_ilm |dr r^2 n(r,lm) exc[n](r,lm) |dOmega Yilm Yilm
          !                /0                           /
          do ilm = 1, mlm
!           t = t + dot_product( a%rho(1:nr,ilm,is,iq) * a%pot(1:nr,ilm,I_EXC,iq), a%s%g%r2dr(1:nr) )
            t = t + sum( a(ia)%rho(1:nr,ilm,is,iq) * a(ia)%pot(1:nr,ilm,I_EXC,iq) * a(ia)%s%g%r2dr(1:nr) )
          enddo ! ilm
        enddo ! is
        a(ia)%Exc(iq) = t
      enddo ! iq

    enddo ! ia

  endfunction xc_in_the_spheres



  status_t function atomic_Hamiltonian_matrix( a ) result( ist )
  ! set up the matrix for the nonlocal Hamiltonian in the spheres
  use constants, only: sqrt4pi
  use constants, only: Y00 => ONESQRTFOURPI
  use type_atom, only: atom
  use type_species, only: matrix_blow_up
  use type_species, only: I_TRU, I_SMT, I_AUG => I_TRU
  use spherical, only: Gaunt_tensor
cNaN  use debugtools, only: NaN_search
  implicit none
    ! parameters
    character(len=*), parameter           :: fun = ' atomic_Hamiltonian_matrix: '
    ! arguments
    type(atom), intent(inout)             :: a
    ! local vars
    integer                               :: ns
    integer                               :: is, nr
    integer                               :: ilm, mlm, mlm2, mln, mlnm!, ilnm, iln
    integer                               :: iln1, iln2, i, j

    real, allocatable                     :: vloc_ln(:,:)
    real, allocatable                     :: vloc_lnm(:,:)
    real, allocatable                     :: gnt_lm(:,:)
    real, allocatable                     :: gnt_lnm(:,:)
    real, allocatable                     :: hu(:,:,:) !! i, j, spin i and j run over all nlm indexes (mlnm)

    mlnm = a%s%mlnm
    nr   = size( a%rho, 1 )
    mlm2 = min( size( a%rho, 2 ), 49 ) ! == (ellmax_rho+1)**2
    ns   = ubound( a%rho, 3 ) ! spin
    mlm  = (a%s%ellmax+1)**2
    mln  = a%s%mln



    allocate( gnt_lm(mlm,mlm), gnt_lnm(mlnm,mlnm), stat=ist )
    allocate( vloc_ln(mln,mln), vloc_lnm(mlnm,mlnm), stat=ist )

    !
    ! direct evaluation of the potential matrix elements
    ! uses only the kinetic energy deficit tensor for the atomic Hamiltonian
    !
    ! set up the atomic Hmt
    a%Hm(:,:,:) = 0. ! init

    ! kinetic energy deficit tensor
    a%Hm(:,:,1) = a%s%dkin2(:,:) ! init
!!!     if(o>0) write(o,'(3A,9(/,4F10.6))') sym, fun, 'kin:', a%Hm(:,:,:)

    if( ns > 1 ) a%Hm(:,:,2) = a%Hm(:,:,1) ! the kinetic part is the same for both spins, so copy

    ! the local potential contributions:
    ! <TRU|vxc+vht-Z/r|TRU>-<SMT|vxc+ves+vbar|SMT>


    do ilm = 1, mlm2

      gnt_lm = Gaunt_tensor(1:mlm,1:mlm,ilm)
      ! bring Gaunt_tensor for ilm to nlnm x nlnm - matrix shape
      call matrix_blow_up( a%s, m_lm=gnt_lm, m_lnm=gnt_lnm )

      do is = 1, ns

        do iln1 = 1, mln !! total number of projectors
          do iln2 = 1, mln
            ! eval the local potentials
            vloc_ln(iln2,iln1) = Hamiltonian_element( a, iln1, iln2, ilm, is )
          enddo ! iln2
        enddo ! iln1

        call matrix_blow_up( a%s, m_ln=vloc_ln, m_lnm=vloc_lnm )
        a%Hm(:,:,is) = a%Hm(:,:,is) + vloc_lnm(:,:) * gnt_lnm(:,:)

      enddo ! is

    enddo ! ilm

    deallocate( vloc_ln, vloc_lnm, gnt_lm, gnt_lnm, stat=ist )


    !! add the Hubbard U term as done in VASP: PHYSICAL REVIEW B 69, 075413 (2004) A. Rohrbach, J. Hafner, and G. Kresse


    a%hu(:,:,:) = 0.
    !!!eq. (4) of the above paper
    !! loop on i and j idxs and find when on d channel
    do i=1, mlnm
      do j=1, mlnm
        if((a%s%ind_ell(i) .eq. 2) .and. (a%s%ind_ell(j) .eq. 2)) then
          do is=1, ns
            a%hu(i,j,is) = -a%Dm(i,j,is)
          enddo
          if (i .eq. j) then
            a%hu(i,j,:) = a%hu(i,j,:) + 0.5
          endif

        endif
      enddo
    enddo
    a%hu = a%hu * a%hubbard_U_J ! multiply by U , this completes the 2nd term of eq. 4
    ! now we have to add it to the atomic KS hamiltonian
    a%Hm(:,:,:) = a%Hm(:,:,:) + a%hu(:,:,:)


    !!! now do for the p states
    a%hu(:,:,:) = 0.
    !!!eq. (4) of the above paper
    !! loop on i and j idxs and find when on d channel
    do i=1, mlnm
      do j=1, mlnm
        if((a%s%ind_ell(i) .eq. 1) .and. (a%s%ind_ell(j) .eq. 1)) then
          do is=1, ns
            a%hu(i,j,is) = -a%Dm(i,j,is)
          enddo
          if (i .eq. j) then
            a%hu(i,j,:) = a%hu(i,j,:) + 0.5
          endif

        endif
      enddo
    enddo
    a%hu = a%hu * a%hubbard_U_J_p ! multiply by U , this completes the 2nd term of eq. 4
    ! now we have to add it to the atomic KS hamiltonian
    a%Hm(:,:,:) = a%Hm(:,:,:) + a%hu(:,:,:)



  endfunction atomic_Hamiltonian_matrix



  !! for the direct method
  real function Hamiltonian_element( a, iln1, iln2, ilm, is ) result( e )
  use type_species, only: species, I_TRU, I_SMT, I_VES, I_VBAR
  use constants, only: sqrt4pi
  use type_atom, only: atom
  !
  !                       /R
  !     dH_element_{ij} = |dr r^2 \phi_{i} V_{loc}[ n ] \phi_j
  !                       /0        - \tilde{phi_i} \tilde V_{loc} [\tilde{n}] \tilde{\phi_j}
  ! needs to be multplied by a Gaunt tensor due to
  ! the integral over 3 spherical harmonic functions.
  implicit none
    ! parameters
    character(len=*), parameter           :: fun = ' Hamiltonian_element: '
    ! arguments
    type(atom), intent(in)                :: a          !! the atom
    integer, intent(in)                   :: iln1, iln2 !! [ell,enn]-combindices
    integer, intent(in)                   :: ilm, is    !! [ell,emm]-combindex, spin
    ! local vars
    integer                               :: ir!, ell
    real                                  :: v0

#ifdef DEBUG
    if( is < 1 .or. is > ubound(a%pot,3) ) stop 'SCF Hamiltonian_element: is out of bounds [1,ns]'
    if( iln1 < 1 .or. iln1 > ubound(a%s%rwf,2) ) stop 'SCF Hamiltonian_element: iln1 out of bounds [1,mln]'
    if( iln2 < 1 .or. iln2 > ubound(a%s%rwf,2) ) stop 'SCF Hamiltonian_element: iln2 out of bounds [1,mln]'
    if( ilm < 1 .or. ilm > ubound(a%pot,2) ) stop 'SCF Hamiltonian_element: ilm out of bounds [1,mlm]'
#endif
    ! find ell of ilm

    v0 = 0. ; if( ilm == 1 ) v0 = 1.

    e = 0. ! init result
    do ir = 1, a%s%nr
      e = e + (  ( & ! begin TRU
                   a%s%rwf(ir,iln1,1,I_TRU) * &    ! true partial wave1
                   a%s%rwf(ir,iln2,1,I_TRU) * &    ! true partial wave2
                   ( a%pot(ir,ilm,is,I_TRU) + &    ! true exchange-correlation potential
                     a%pot(ir,ilm,I_VES,I_TRU) &   ! true electrostatic potential
                   ) &
                 ) & ! end TRU
                -( & ! begin SMT
                   a%s%rwf(ir,iln1,1,I_SMT) * &    ! smooth partial wave1
                   a%s%rwf(ir,iln2,1,I_SMT) * &    ! smooth partial wave2
                   ( a%pot(ir,ilm,is,I_SMT) + &    ! smooth exchange-correlation potential
                     a%pot(ir,ilm,I_VES,I_SMT) + & ! smooth electrostatic potential
                     v0 * a%s%rf(ir,I_VBAR) &      ! smooth correction potential if ilm==1
                   ) &
                 ) & ! end SMT
              ) *  a%s%g%r2dr(ir)
    enddo ! ir
  endfunction Hamiltonian_element




  real function integral_r4( density, g ) result( s )
  use type_grid, only: grid
  use MPItools, only: operator( .MPIsum. )
  implicit none
    ! parameter
    character(len=*), parameter           :: fun = ' integral: '
    ! arguments
    real, intent(in)                      :: density(:,:,:,:)
    type(grid), intent(in)                :: g
    ! local vars
    integer :: id

    do id = 1, 3
      if( size(density,id) /= g%ng(id) ) stop 'SCF integral_r4: shape does not match g%ng!'
    enddo ! id

    s = ( sum(density) .MPIsum. g%comm ) * g%hvol

  endfunction ! integral_r4

  real function integral_r3( density, g ) result( s )
  use type_grid, only: grid
  use MPItools, only: operator( .MPIsum. )
  implicit none
    ! parameter
    character(len=*), parameter           :: fun = ' integral: '
    ! arguments
    real, intent(in)                      :: density(:,:,:)
    type(grid), intent(in)                :: g

    if( any( shape(density) /= g%ng(1:3) ) ) stop 'SCF integral_r3: shape does not match g%ng!'
    s = ( sum(density) .MPIsum. g%comm ) * g%hvol

  endfunction ! integral_r3


  status_t function poisson_solve( ge, rho_e, ves_e, pss, key ) result( ist )
  use type_criteria, only: criteria
  use type_grid, only: grid
  use Poissonsolver, only: poissonsolve
  use interpolation, only: interpolate
  implicit none
    ! parameters
    character(len=*), parameter           :: fun = ' Poisson: '
    ! arguments
    type(grid), intent(in)                :: ge
    real, intent(in)                      :: rho_e(:,:,:)
    real, intent(inout)                   :: ves_e(:,:,:)
    type(criteria), intent(in)            :: pss
    integer, intent(in)                   :: key
    ! local vars
    integer                               :: ip, np, np_max, ng(3), nh(3), mxi
    real                                  :: res!, sint
    real, allocatable                     :: rho_d(:,:,:), rho_c(:,:,:)
    real, allocatable                     :: ves_d(:,:,:), ves_c(:,:,:)
    type(grid)                            :: gd, gc

    ! see how well we can factorize the grid numbers in powers of 2
    ng = ge%ng(1:3)
    do ip = 1, 9
      if( all( modulo(ng,2**ip) == 0 ) ) np_max = ip
    enddo ! ip

    np = 0 !      no MultiGridMethod  0:ge



    mxi = 3 ; if( np < 1 ) mxi = pss%maxit

    ist = poissonsolve( x=ves_e, rho=rho_e, g=ge, &
                                threshold=pss%thres, &
                                res=res, &
                                maxiter=mxi, &
                                miniter=pss%minit, &
                                restart=30, &
                                key=key )


  endfunction poisson_solve


  real function Time_tic( ) result( t )
  use MPIconst, only: Wtime
    real, save :: prv=0. ! previous time the function has been called
    real       :: now
    now = Wtime( )
    t = now - prv
    prv = now ! store
  endfunction

  status_t function test() result( ist )
    ist = 0
    stop 'SCF: no module test implemented'
  endfunction

  status_t function grid_derivative( g, rho, derivative ) result( ist )
  !! calculate derivatives of charge on grid for GGA
  !! DanielWortmann 16.4.2012
  use type_grid, only: grid
  use forces, only: derive_id
  implicit none
    ! parameters
    character(len=*), parameter :: fun = ' grid_derivative: '
    real, parameter             :: SMALL = 1E-14
    integer, parameter          :: IAGR = 1
    integer, parameter          :: GGGR = 2
    integer, parameter          :: IG2R = 3

!     integer, parameter              :: R = 0
!     integer, parameter              :: X = 1
!     integer, parameter              :: Y = 2
!     integer, parameter              :: Z = 3
!     integer, parameter              :: XX = 4
!     integer, parameter              :: YY = 5
!     integer, parameter              :: ZZ = 6
!     integer, parameter              :: XY = 7
!     integer, parameter              :: YZ = 8
!     integer, parameter              :: ZX = 9
#ifdef NOSchwartzAssumption
!     integer, parameter              :: YX = 10
!     integer, parameter              :: ZY = 11
!     integer, parameter              :: XZ = 12
#endif

  ! arguments
  type(grid), intent(in)      :: g !! grid descriptor
  real, intent(in)            :: rho(:,:,:,1:) !! charge(one or two spins)
  real, intent(out)           :: derivative(:,:,:,IAGR:,-1:) !! derived array (nx,ny,nz,IAGR:IG2R,-1:+1)
  ! local vars
  integer           :: n(1:4), is, ns ! spin
  real, allocatable ::  ro(:,:,:)
  real, allocatable ::  dx(:,:,:), dy(:,:,:), dz(:,:,:)
  real, allocatable :: dxx(:,:,:),dyy(:,:,:),dzz(:,:,:)
  real, allocatable :: dxy(:,:,:),dyz(:,:,:),dzx(:,:,:)

! #define NOSchwartzAssumption
#ifdef NOSchwartzAssumption
  ! Schwartz assumption is that we can interchange the order of partial derivatives
  real, allocatable :: dyx(:,:,:),dzy(:,:,:),dxz(:,:,:)
#endif
    n = shape(rho)
cDBG  if( any( n(1:3) /= g%ng(1:3) ) ) stop 'SCF: grid_derivatives: grid descriptor does not match array sizes!'

    allocate( ro(n(1),n(2),n(3)), &
       dx(n(1),n(2),n(3)),  dy(n(1),n(2),n(3)),  dz(n(1),n(2),n(3)), &
      dxx(n(1),n(2),n(3)), dyy(n(1),n(2),n(3)), dzz(n(1),n(2),n(3)), &
#ifdef NOSchwartzAssumption
      dyx(n(1),n(2),n(3)), dzy(n(1),n(2),n(3)), dxz(n(1),n(2),n(3)), &
#endif
      dxy(n(1),n(2),n(3)), dyz(n(1),n(2),n(3)), dzx(n(1),n(2),n(3)), stat=ist )
    if( ist /= 0 ) stop 'SCF: grid_derivatives: failed to allocate derivative arrrays!'

    ns = 0 ; if( n(4) > 1 ) ns = 1

    do is = -ns, +ns ! dn,total,up-spin

      selectcase( n(4) )
      case( 1 ) ! no spin polarization {tt}
        ro = rho(:,:,:,1) ! total density
!         selectcase( is )
!         case( -1 ) ; ro = rho(:,:,:,1)*0.5 ! dn density
!         case(  0 ) ; ro = rho(:,:,:,1) ! total density
!         case( +1 ) ! copy from dn
!           derivative(:,:,:,:,is) = derivative(:,:,:,:,-1)
!           cycle ! is-loop, no need to calculate the derivatives again
!         endselect ! is
      case( 2 ) ! with spin polarization {up,dn}
        selectcase( is )
        case( -1 ) ; ro = rho(:,:,:,2) ! dn-density
        case(  0 ) ; ro = rho(:,:,:,1) + rho(:,:,:,2) ! total density
        case( +1 ) ; ro = rho(:,:,:,1) ! up-density
        endselect ! is
      case( 3: ) ! spins are given {dn,tt,up}
        ro = rho(:,:,:,2+is)
        if(o>0) write(o,'(4A,I0,9A)') sym, fun, __FILE__, ': line #', __LINE__, ' this feature has not been tested!'
      case( :0 ) ; return ! no array range
      endselect ! n4

! c$omp parallel

! c$omp sections
! c$omp section
      ist = derive_id( g, 1, ro, dx )
! c$omp section
      ist = derive_id( g, 2, ro, dy )
! c$omp section
      ist = derive_id( g, 3, ro, dz )
! c$omp section
      derivative(:,:,:,GGGR,is) = 0. ! init the GGGR field
! c$omp end sections


! c$omp barrier


! c$omp sections
! c$omp section
      derivative(:,:,:,IAGR,is) = sqrt( dx*dx + dy*dy + dz*dz ) ! calculate IAGR = | grad rho |
! c$omp section
      ist = derive_id( g, 1, dx, dxx )
! c$omp section
      ist = derive_id( g, 2, dy, dyy )
! c$omp section
      ist = derive_id( g, 3, dz, dzz )

! c$omp section
      ist = derive_id( g, 1, dy, dxy )
! c$omp section
      ist = derive_id( g, 2, dz, dyz )
! c$omp section
      ist = derive_id( g, 3, dx, dzx )
#ifdef NOSchwartzAssumption
! c$omp section
      ist = derive_id( g, 1, dz, dxz )
! c$omp section
      ist = derive_id( g, 2, dx, dyx )
! c$omp section
      ist = derive_id( g, 3, dy, dzy )
#endif
! c$omp end sections

! c$omp end parallel

      ! calculate GGGR = grad rho * grad | grad rho |
#ifdef NOSchwartzAssumption
      derivative(:,:,:,GGGR,is) = derivative(:,:,:,GGGR,is)+dx*(dx*dxx+dy*dxy+dz*dxz)
      derivative(:,:,:,GGGR,is) = derivative(:,:,:,GGGR,is)+dy*(dx*dyx+dy*dyy+dz*dyz)
      derivative(:,:,:,GGGR,is) = derivative(:,:,:,GGGR,is)+dz*(dx*dzx+dy*dzy+dz*dzz)
#else
      derivative(:,:,:,GGGR,is) = derivative(:,:,:,GGGR,is)+dx*(dx*dxx+dy*dxy+dz*dzx)
      derivative(:,:,:,GGGR,is) = derivative(:,:,:,GGGR,is)+dy*(dx*dxy+dy*dyy+dz*dyz)
      derivative(:,:,:,GGGR,is) = derivative(:,:,:,GGGR,is)+dz*(dx*dzx+dy*dyz+dz*dzz)
#endif

      where( derivative(:,:,:,IAGR,is) > SMALL )
        derivative(:,:,:,GGGR,is) = derivative(:,:,:,GGGR,is)/derivative(:,:,:,IAGR,is)
      elsewhere
        derivative(:,:,:,GGGR,is) = 0.
      endwhere

      ! calculate IG2R = Laplacian rho
      derivative(:,:,:,IG2R,is) = dxx + dyy + dzz

    enddo ! is
    deallocate( dx, dy, dz, &
#ifdef NOSchwartzAssumption
                     dyx, dzy, dxz, &
#endif
      dxx, dyy, dzz, dxy, dyz, dzx, stat=ist )

    if( ns == 0 ) then
      ! copy the spin-up and spin-dn arrays from the spin-tt component: Caution: GGGR is quadratic in the density
      do is = -1, +1, 2  ! is in {-1,+1}
        derivative(:,:,:,IAGR,is) = derivative(:,:,:,IAGR,0)/2. ! copy: half the total density gradients
        derivative(:,:,:,GGGR,is) = derivative(:,:,:,GGGR,0)/4. ! GGGR which is quadratic in rho
        derivative(:,:,:,IG2R,is) = derivative(:,:,:,IG2R,0)/2. ! copy: half the total density lapacian
      enddo ! is
    endif ! ns == 0

  endfunction grid_derivative


endmodule selfcon
! end of module tail
#endif
! end of file
#endif


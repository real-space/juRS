#include "config.h"

! #define DEBUG

#ifdef DEBUG
#define cDBG
#else
#define cDBG !
#endif


#define USE_ATOM_ORDER
!! USE_ATOM_ORDER means that the order is rearranged such that
!! as few as possible atoms block each other during communication

!! @author Paul Baumeister, Andrea Nobile
!! @version 4.00
!!
!! atom descriptor
!! Andrea Nobile, 4.2014 reviewed and modified to use gpaw setups
module type_atom
  use configuration, only: o ! output unit, 0: no output
  use constants, only: NUCLEON_MASS
  use type_species, only: species, I_TRU, I_SMT
  use type_comp, only: comp ! localized compensation density
  use type_proj, only: proj ! localized projector functions
implicit none
  private ! default for this module namespace
  character(len=*), parameter :: fun = ': ', sym = 'tATOM' !! module symbol

  public :: atom
  public :: atom_set
  public :: prepare_atoms
  public :: free_atoms
  public :: atomic_population
  public :: optimal_n_orbitals
  public :: write_xyz_file
  public :: write_xyz_style
#ifdef EXTENDED
  public :: type2string
  public :: to_string
  public :: test

  interface to_string
    module procedure atom2string
  endinterface
#endif

  ! defaults
  integer, parameter, public     :: ITP_DEF = 1 !! default interpolation order for DG of projectors
  integer, parameter, public     :: MSH_DEF = 2 !! default meshrefinement for DG of projectors

  integer, parameter, public  :: TYPE_ATOM_NOOWNER         = -1 !! none
  integer, parameter, public  :: TYPE_ATOM_POS_ABSOLUTE    = 0 !! position was specified in absolute coords
  integer, parameter, public  :: TYPE_ATOM_POS_FRACTIONAL  = 1 !! position was specified in fractions if the cell


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! TYPE ATOM !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  type :: atom
    type(species), pointer  :: s !! species
    integer                 :: ja = 0 !! global atom index (absolute)
    string_t                :: label = '' !! by default symbol and a count number
    integer                 :: owner = TYPE_ATOM_NOOWNER !! rank of the owner process
    integer, _allocatable_  :: plist(:) ! list of contributing process ranks (if owner)
    ! molecular dynamics
    real                    :: weight = NUCLEON_MASS !! can deviate from s%weight
    real                    :: magmom = 0.
    real                    :: fix(3) = 1. !! fixed positions if 0.0
    real                    :: frc(3) = 0. !! force
    real                    :: vel(3) = 0. !! velocity
    real                    :: pos(3) = 0. !! position absolute
    real                    :: dis(3) = 0. !! last absolute displacement
    real                    :: relpos(3) = 0. !! position relative to cell
    real                    :: hubbard_U_J = 0.0
    real                    :: hubbard_U_J_p = 0.0

    real                    :: imagepos(3,27) = 0. !! periodic image positions
    integer                 :: nimages = 1 !! number of periodic images (depends on BCs)
    integer                 :: key_pos = TYPE_ATOM_POS_ABSOLUTE !! switch to decide abs or frac
    real                    :: symmetry_factor = 1. !! not in use

    ! PAW method
    type(comp)              :: cmp !! localized compensation charges on the dense grid
    type(proj)              :: prj !! set of localized projector function on the coarse grid

    type(comp)              :: dcmp(3) !! derived localized compensation charges on the dense grid

    integer                 :: prjphase(1:3,27) = 0 ! indicators for the projectors prj
    ! offset of projections coefficients in a c-vector
    integer                 :: offprj = 0 !! offset in process-local coefficient vectors
    ! radial densities in multipoles
    real, _allocatable_     :: rho(:,:,:,:) !! density rho(r)_lm (s%nr,(2*s%lm+1)**2,ns,3)
    ! radial potentials in multipoles
    real, _allocatable_     :: pot(:,:,:,:) !! potential V(r)_lm (s%nr,(2*s%lm+1)**2,I_HARTREE_POT:ns,3)
    ! history of radial densities in multipoles
    real, _allocatable_     :: mix_hist(:,:,:,:,:) !! (see_a%Dm,0:nhist)
    ! atom centered density matrix
    real, _allocatable_     :: Dm(:,:,:) !! Density matrix (s%mlnm,s%mlnm,ns)
    ! atomic density matrix times Gaunt summed over emms
    real, _allocatable_     :: GntDm(:,:,:,:) !! Gaunt tensor times density matrix (s%mln,s%mln,(2*s%ml+1)**2,ns)
    ! atom centered Hamiltonian matrix
    real, _allocatable_     :: Hm(:,:,:) !! Non-local correction to the Hamiltonian (s%mlnm,s%mlnm,ns)
    real, _allocatable_     :: Hu(:,:,:) !! Non-local correction to the Hamiltonian for teh hubbard term (s%mlnm,s%mlnm,ns)
    ! augmentation charges
    real, _allocatable_     :: qlm(:) !! charge deficit multipole moments ((ellmax_rho+1)**2)
    ! integral of vH(r)*g_ell(r)
    real, _allocatable_     :: vlm(:) !! vlm ((ellmax_rho+1)**2)
    ! atomic_population in the electronic structure
    real, _allocatable_     :: atomic_population(:,:) !! population analysis for atomic states (s%mln,ns)
    ! energy corrections
    real                    :: Ees(I_TRU:I_SMT) !! electrostatic energy (I_TRU:I_SMT)
    real                    :: Exc(I_TRU:I_SMT) !! exchange correlation energy (I_TRU:I_SMT)
  endtype ! atom

  contains

  !! constructor for an atom
  type(atom) function atom_set( s, pos, ja, label, key_pos, symmetry_factor ) result( a )
  use type_species, only: species
    type(species), target, intent(in)       :: s      !! type species to point to
    real, intent(in)                        :: pos(3) !! position in abs or frac coordinates, depends on key_pos
    integer, intent(in), optional           :: ja     !! absolute atom index
    character(len=*), intent(in), optional  :: label  !! a label that is set in the input file
    integer, intent(in), optional           :: key_pos !! decides whether fractional or absolute coordinates, default is abs
    real, intent(in), optional              :: symmetry_factor !! default 1

    character(len=*), parameter :: fun = ' atom_set: '
    ! atomic species points is set to the species that
    ! is passed in the argument list of this function
    a%s => s
!     a%ellmax = 2*s%ml

    ! adopt the position (absolute)
    a%pos = pos
    a%imagepos(1:3,1) = a%pos(1:3)
    a%nimages = 1

    ! store the information if the positions has been
    ! input in absolute ore fractional coordinates
    if( present( key_pos ) ) then
      selectcase( key_pos )
      case( TYPE_ATOM_POS_ABSOLUTE )   ; a%key_pos = TYPE_ATOM_POS_ABSOLUTE
      case( TYPE_ATOM_POS_FRACTIONAL ) ; a%key_pos = TYPE_ATOM_POS_FRACTIONAL
      case default ; stop 'atom_set: unknown key_pos.'
      endselect ! key_pos
    else ; a%key_pos = TYPE_ATOM_POS_ABSOLUTE ! default
    endif ! present(pos_f)


    ! absolute atom number
    if( present(ja) ) a%ja = ja

    if( present(label) ) then ;  a%label = adjustl(label)
    else
      write( unit=a%label, fmt='(A,I0,A)' ) '#',a%ja, a%s%sym
      a%label = adjustl(a%label)
    endif ! present(label)
!     if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'label of atom #', a%ja, ' "', trim(a%label), '".'
    a%weight = s%weight ! copy
    a%symmetry_factor = 1. ; if( present( symmetry_factor ) ) a%symmetry_factor = symmetry_factor

  endfunction ! atom_set


  integer function prepare_atoms( a, atm, gc, gd, nspins, &
                     interpolationO, meshrefinement, checkmode ) &
           result( naprj ) !! number of all projectors in this process
  ! set up the lfuns from the rfuns of the species
  use type_species, only: species, ELLMAX_CMP, ELLMAX, ENNMAX, I_TRU, I_SMT, I_PRJ
  use type_proj, only: proj_set ! setup of a localized projector functions
  use type_comp, only: comp_set, comp_normalize
  use type_grid, only: grid, periodic_positions
  use MPIconst, only: MPI_UNDEFINED, MPI_COMM_NULL, MPI_COMM_SELF
  use MPItools, only: MPIbarrier, operator(.MPImax.)
  use unitsystem, only: Ang, Ang_
  use configuration, only: WARNING, ERROR
#ifdef USE_ATOM_ORDER
  use communicators, only: atom_order
#endif
  use atomicomm, only: atomic_comm_lists
    type(atom), intent(inout)             :: a(:)   !! all atoms
    type(atom), allocatable, intent(out)  :: atm(:) !! parallelized atom list
    type(grid), intent(in)                :: gc     !! coarse grid
    type(grid), intent(in)                :: gd     !! dense grid
    integer, intent(in)                   :: nspins !! number of spins
    integer, intent(in), optional         :: interpolationO !! half order of double grid interpolator
    integer, intent(in), optional         :: meshrefinement !! as many times denser double grid
    logical, intent(in), optional         :: checkmode !! Check Mode will not perform actions
    ! local vars
    integer                               :: itp, msh
    integer                               :: ia, iatm
    integer                               :: natoms, natm, natm_max
    integer                               :: rnk(3), owner_rank
    integer                               :: i, ni, ii, offprj, ik
    real                                  :: origins(3,27), rcut
    logical                               :: cm
    logical, allocatable                  :: part(:)
    status_t                              :: ist, ierr
    integer                               :: n_warn
#ifdef USE_ATOM_ORDER
    integer, allocatable                  :: order(:)
#endif
cDBG  integer :: nown=0
cDBG  character(len=*), parameter           :: fun = ' prepare_atoms: '

    cm = .false. ; if( present( checkmode ) ) cm = checkmode

    itp = ITP_DEF ; if( present( interpolationO ) ) itp = max( 0, interpolationO )
    msh = MSH_DEF ; if( present( meshrefinement ) ) msh = max( 1, meshrefinement )

    if( .not. any( nspins == (/1,2/) ) ) stop 'prepare_atoms: NSPINS must be one of {1,2}.'


    natoms = size(a)
    if( natoms < 1 ) return

    allocate( part(natoms), stat=ist )

    n_warn = 0
    !---------------------------------
    do ia = 1, natoms
      !-------------------
      Rcut = maxval( a(ia)%s%fprj(:)%rcut ) ! a(ia)%s%rcut
      if( any( ceiling( Rcut/gc%h ) + itp > gc%ng_all(1:3) ) ) then
        ! this is crucial for small cells, because usually,
        ! only one neighboring periodic image is taken into account
        !     (1 image ==> np_max=3**3=27, 2 images ==> np_max=5**3=125),
        ! therefore, any localized function should not extend over more than
        ! one supercell (in radius)
        !
        !          prev. cell       supercell        next cell
        !     -1.5             -.5      x       .5             1.5   (x=cell origin)
        !    ===|===============|===============|===============|==
        !           |<<<<<<<<<<<<<<<A>>>>>>>>>>>>>>>|             (A=a_{0}=atomic pos.)
        !  <<<<<<<<<a>>>>>>>>>>>>>>>|                                  (a_{-1}) left pI
        !                           |<<<<<<<<<<<<<<<a>>>>>>>>>>>>>>>|  (a_{1}) right pI
        !                                                        (pI = periodic images)
        ! ==> 3 |lfun>s will be set up:
        !                           supercell
        !                       |===============|
        !                       <<<<A>>>>>>>>>>>>     |lfun_{ 0}> center
        !                       >>>>|                 |lfun_{-1}>   left
        !                           |<<<<<<<<<<<<     |lfun_{+1}>  right
        !
        ! if the effective rcut (including interpolation) is now larger
        ! than the supercell is wide, |lfun_{-2}> or |lfun_{2}> is truncated.
        n_warn = n_warn+1 ! count similar situations

            ! Double grid method enlarges the cutoff radius on the coarse
      endif ! grid more than one periodic neighbor can cover.
      !-------------------
    enddo ! ia
    !---------------------------------
    if( n_warn > 0 .and. o>0) write(o,'(4A,I0,9A)') sym, fun, WARNING(0), &
      'in ', n_warn, ' situations, rcut > cell, a single periodic image is not sufficient!'

    ! for the logg: show the order and grid factor of the DoubleGrid method
    !if(o>0) write(o,'(3A,/,3A,2(I3,A),/,3A,F0.3,9A)') &
    !  sym, fun, 'double grid method for projectors:', &
    !  sym, fun, '    ', 2*itp, '-th order,', msh, 'x denser grid', &
    !  sym, fun, 'enlarges the projection radius by ', itp*maxval(gc%h)*Ang, Ang_

    !---------------------------------
! !$omp parallel do private(ia,ni,ii,origins,rcut) schedule(dynamic,1)
    do ia = 1, natoms
      !-------------------
cDBG  if(o>0) write(o,'(/3A,I0)') sym, fun, 'atom #', ia
      ! the ell-cutoff for the density in the spheres is chosen 2*s%ml
      ! so twice the ell-cutoff of the partial wave expansion
! cDBG      mlm2 = (2*a(ia)%s%ellmax+1)**2
! cDBG      ! radial multipole moments
! cDBG      if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'number of radial multipoles = ', mlm2, ' in atom #', a(ia)%ja

      ! this function only depends on g%s and g%boundarycondition, so
      ! it will give the same result, no matter if gc or gd is given.
      ni = periodic_positions( gc, a(ia)%relpos, origins=a(ia)%imagepos, shifts=a(ia)%prjphase )

      a(ia)%nimages = ni ! store the number of images

      a(ia)%prj = proj_set( bf=a(ia)%s%fprj(:), origins=a(ia)%imagepos(1:3,1:ni), &
                            offset=gc%off, iphase=a(ia)%prjphase, &
                            hg=gc%h, ng=gc%ng(1:3), &
!                            interpolation=itp, meshrefinement=msh, &
                            interpolation=1, meshrefinement=1, &
!                           derive2i=0, & ! no derivative
!                            jatom=ia, & ! global atom index
                            checkmode=cm )

cDBG  if(o>0) write(o,'(3A,I0,3A,3F10.3)') sym, fun, 'position for atom #', ia, '  ', a(ia)%s%sym, ', pos=', a(ia)%pos
cDBG  if(o>0) write(o,'(3A,3F10.3)') sym, fun, 'position - grid offset=', a(ia)%pos-gc%off
cDBG  if(o>0) write(o,'(5A,I0)') sym, fun, 'Weinert exponent for ', a(ia)%s%sym, ' is ', a(ia)%s%nWexp

      ! prepare the set of imagepositions minus the grid offset of the dense grid gd
      do ii = 1, ni
        ! subtract the offset for the dense grid
        origins(1:3,ii) = a(ia)%imagepos(1:3,ii) - gd%off(1:3)
      enddo ! ii

      rcut = a(ia)%s%rcut ! g .at. a(ia)%s%nr

      a(ia)%cmp = comp_set( ellmax=min( max( 0, 2*a(ia)%s%ellmax ), ELLMAX_CMP ), &
                            rcut=rcut, &
                            s=a(ia)%s, &
                            origins=origins(:,1:ni), &
                            hg=gd%h, ng=gd%ng, &
                            interpolation=1, & ! double grid interpolation order
                            meshrefinement=2, & ! double grid refinement factor
!                           derive2i=0, & ! no derivative
                            checkmode=cm )
!!#if 0
      do ik=1,3
      a(ia)%dcmp(ik) = comp_set( ellmax=min( max( 0, 2*a(ia)%s%ellmax ), ELLMAX_CMP ), &
                            rcut=rcut, &
                            s=a(ia)%s, &
                            origins=origins(:,1:ni), &
                            hg=gd%h, ng=gd%ng, &
                            interpolation=1, & ! double grid interpolation order
                            meshrefinement=2, & ! double grid refinement factor
                            derive2i=ik, & ! no derivative
                            checkmode=cm )
      enddo
!!#endif
      ! the atoms contributes to this process, if either
      !    - the compensation charge distribution has an overlap
      !      with the spatial region of this domain
      ! or - the projectors or their periodic images are in this region
      part(ia) = ( a(ia)%cmp%noe > 0 .or. any( a(ia)%prj%p(:)%noe > 0 ) )
cDBG  if( part(ia) .and. o>0) write(o,'(3A,I0,9A)') sym, fun, 'atom #', ia, ' is included.'

    enddo ! ia
! !$omp end parallel do


    if( .not. cm ) then
! determine the atom-owner process
      do ia = 1, natoms
#ifndef NOMPI
! MPI part
        !-------------------

        ! (SIMPLEST CHOISE)
        ! the domain that contains the atom core is the owner
! cDBG    if(o>0) write(o,'(3A,I6,A,3F6.3)') sym, fun, 'relative position of atom j#', a(ia)%ja, '    ', a(ia)%relpos
        rnk = int((a(ia)%relpos+0.5)*gc%nproc) ! rank coordinates of the owner

        if( any( rnk < 0 .or. rnk >= gc%nproc ) ) then
          if(o>0) write(o,'(4A,9(3(" ",I0),A))') sym, fun, ERROR, &
            'rank coords of the owner process out of bounds, rnk=', rnk, ', but nproc=', gc%nproc, ' ja#', ia
          stop 'prepare_atoms: owner ranks exceed boundaries, check a%relpos to be within [-0.5,0.5)!'
        endif ! rnk out of range

        ! find the rank of the owner ! Caution: C-ordering,  therefore 3,2,1
        call MPI_Cart_rank( gc%comm, (/rnk(3),rnk(2),rnk(1)/), owner_rank, ierr )
cDBG    if( ierr /= 0 .and. o>0) write(o,'(3A,I0)') sym, fun, 'MPI_Cart_rank returned ierr = ', ierr

cDBG    if( a(ia)%owner /= TYPE_ATOM_NOOWNER .and. a(ia)%owner /= owner_rank .and. o>0) write(o,'(3A,9(I0,A))') sym, fun, &
cDBG      'change of ownership of atom #',a(ia)%ja,' from rank #',a(ia)%owner,' to rank #',owner_rank
        a(ia)%owner = owner_rank
! cDBG    if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'process #', owner_rank, ' is owner of atom #', a(ia)%ja

        if( a(ia)%owner == gc%rank ) then
cDBG      nown = nown+1 ! count the number of atoms owned by this domain
          if( .not. part(ia) ) then
            ! error, because the owner also has to contribute to the atom
            write(*,'(4A,9(I0,A))') sym, fun, ERROR, 'process #',gc%rank,' is owner of atom #',a(ia)%ja,', but is not included in the atomic communicator!'
            stop 'prepare_atoms: owner does not contribute!'
          endif ! not part
        endif ! owner

! end of MPI part
#else
        a(ia)%owner = 0 ! all atoms are owned by no one
#endif
      enddo ! ia
    endif ! not Checkmode

    if( cm ) naprj = sum( (/ ( a(ia)%s%mlnm, ia=1,natoms ) /) ) ! in Checkmode, the number of all projectors is returned, no parallelization
    if( cm ) return ! when in CheckMode

    call MPIbarrier( gc%comm )
    !---------------------------------
    do ia = 1, natoms

      ! create a list of processes
      ierr = atomic_comm_lists( a(ia)%ja, contribute=part(ia), comm=gc%comm, owner=a(ia)%owner, list=a(ia)%plist )
cDBG  if(ierr /= 0 .and. o>0) write(o,'(3A,I0)') sym, fun, 'acomm returned ierr = ', ierr
!       a(ia)%comm = gc%comm ! set communicator to grid comm

    enddo ! ia
    !---------------------------------

    natm = count( part ) ! the number of atoms that effectively contribute in this domain
cDBG  if(o>0) write(o,'(/3A,9(I0,A))') sym, fun, 'process #',gc%rank,' owns ',nown,' and contributes to ',natm,' of ',natoms,' atoms.'

    call MPIbarrier( gc%comm ) ! synchronize
    ! find and display the maximum number of locally visible atoms. This number
    ! is needed in OpenMP in order of setting the max. number of treads for the
    ! outer OpenMP loop parallelization over atoms in the prj and add operations
    natm_max = natm .MPImax. gc%comm
    if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'each domain sees a maximum of ',natm_max,' atoms.'

    ! allocate the list of atoms that are relevant to this process domain
    allocate( atm(natm), stat=ist ) ; if( ist /= 0 ) stop 'prepare_atoms: allocation of ATM failed.'

    ! from here, ia is the index for the parallelized atom list
    ! whereas    ja is the index for all atoms

    offprj = 0 ! init offset for the coefficient vectors
    iatm = 0 ! init
#ifdef USE_ATOM_ORDER
    ! find an order such that the communication along the atomic communicators
    ! can be done in the least number of cycles
    allocate( order(natoms), stat=ist ) ; if( ist /= 0 ) stop 'prepare_atom: allocation of order failed.'
    ist = atom_order( part, gc%comm, order )
    if( ist /= 0 ) stop 'prepare_atom: fatal error: atom_order failed to generate a permutation of atoms.'

    do i = 1, natoms ; ia = order(i)
#else
    do ia = 1, natoms
#endif

      if( part(ia) ) then

        iatm = iatm+1 ! next atom index

        atm(iatm) = a(ia) ! copy the atom. All pointers are copied, too.
!         atm(iatm)%plist => a(ia)%plist ! just to make sure

! cDBG    if(o>0) write(o,'(3A,I0,9(A,3F10.6))') sym, fun, 'copy atom #', a(ia)%ja, ' pos', a(ia)%pos, ' ', atm(iatm)%pos

        ! the offset in the coefficient vector
        atm(iatm)%offprj = offprj ! these numbers are process local
        ! the next offset has to be higher by the number of projectors of this atom
        offprj = offprj + atm(iatm)%s%mlnm

cDBG    if(o>0) write(o,'(2A,I0,3A,F0.1,9(A,I0))') sym, fun, atm(iatm)%s%mlnm, ' projectors for ', atm(iatm)%s%sym, ', Z = ', atm(iatm)%s%Z0, &
cDBG       ', indices in c-vector (', atm(iatm)%offprj+1, ':',atm(ia)%offprj+atm(iatm)%s%mlnm ,')'

        ist = allocate_atom_arrays( atm(iatm), nspins=nspins, me=gc%rank )
        if( ist /= 0 ) then
          if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'allocation of atomic arrays failed, atom #', a(ia)%ja,' sum(istatus) = ',ist
          stop 'prepare_atoms: allocation of atomic arrays failed.'
        endif ! ist /= 0
#if 0
!!! line number 547
!!! insert this to display (for each atom) global index ja, number of projectors np
!!! the number of projector parts npi, number of elements real-space grid elements noe
        if(o>0) then
          write(o,'(3A,9(I0,A))') sym, fun, ' ja= ', atm(iatm)%ja, ' np= ', atm(iatm)%s%mlnm
          write(o,'(3A,9(I0,A))') sym, fun, '   npi= ', atm(iatm)%prj%npi
          do ii = 1, atm(iatm)%prj%npi
            if( atm(iatm)%prj%p(ii)%noe > 0 ) write(o,'(3A,9(I0,A))') sym, fun, '   ipi= ', ii, ' noe= ', atm(iatm)%prj%p(ii)%noe
          enddo ! ii
          write(o,'(9A)') sym, fun
        endif ! o>0
!!! until here
#endif
      endif ! part(ia)
    enddo ! loop over all atoms

    naprj = offprj ! number of all projectors in this process
cDBG  if( ia /= natm ) stop 'prepare_atoms: fatal counting error, IA /= NATM after loop'

    deallocate( part, stat=ist )

    ! normalize atomic compensators
    do iatm = 1, size(atm)
      ! the grid spacings are passed only for recomputating the integral again, only needed in DEBUG runs
!       ist = comp_normalize( cmp=atm(iatm)%cmp, comm=atm(iatm)%comm, owner=atm(iatm)%owner, list=atm(iatm)%plist, &
      ist = comp_normalize( cmp=atm(iatm)%cmp, comm=gc%comm, owner=atm(iatm)%owner, list=atm(iatm)%plist, &
                            unity=atm(iatm)%symmetry_factor, hg=gd%h )
    enddo ! iatm

  endfunction ! prepare_atoms


  status_t function allocate_atom_arrays( a, nspins, me ) result( ist )
  use type_species, only: I_TRU, I_SMT, I_PRJ, I_VES
    type(atom), intent(inout)             :: a !! atom, intent(in+out), because some information have already been assigned
    integer, intent(in)                   :: nspins !! number of collinear spins
    integer, intent(in)                   :: me !! my process rank (on gc)

    character(len=*), parameter           :: fun = ' allocate_atom_arrays: '
    integer                               :: mlnm, mlm2, mln, nr
    status_t                              :: i

    ! appreviations for dimensions
    nr   = a%s%nr
    mln  = a%s%mln
    mlm2 = (2*a%s%ellmax+1)**2
    mlnm = a%s%mlnm

    ! all processes (including non-owners) contributing
    ! to this atom allocate the following arrays

    ! compensation charge multipole moments
    allocate( a%qlm( mlm2 ), stat=ist )
    a%qlm = 0. ! init

    if( a%owner == me ) then ! I am the owner
      i = ist
      ! only the atom owner allocates the following arrays
      ! potential shift multipoles
      allocate( a%vlm( mlm2 ), stat=ist ) ;i=i+ist
      a%vlm = 0. ! init

      ! the atomic densities
      allocate( a%rho( 0:nr, mlm2, 0:nspins, I_TRU:I_SMT ), stat=ist ) ;i=i+ist
      a%rho = 0. ! init

      ! potentials: Ves, eXC, vXC(1) [, vXC(2)], in the true, smooth and augmented version
      allocate( a%pot( 0:nr, mlm2, I_VES:nspins, I_TRU:I_SMT ), stat=ist ) ;i=i+ist
      a%pot = 0. ! init

      ! atomic Hamiltonian matrix
      allocate( a%Hm( mlnm, mlnm, 1:nspins ), stat=ist ) ;i=i+ist
      a%Hm = 0. ! init

      allocate( a%Hu( mlnm, mlnm, 1:nspins ), stat=ist ) ;i=i+ist
      a%Hu = 0. ! init

      ! atomic Density matrix, Dm(:,:,0) = sum( Dm(:,:,1:) )
      allocate( a%Dm( mlnm, mlnm, 0:nspins ), stat=ist ) ;i=i+ist
      a%Dm = 0. ! init

     

      ! atomic Density matrix times Gaunt tensor summed over emm
      allocate( a%GntDm( mln, mln, mlm2, 1:nspins ), stat=ist ) ;i=i+ist
      a%GntDm = 0. ! init

      ! atomic population of the density matrix
      allocate( a%atomic_population( mln, 0:nspins ), stat=ist ) ;i=i+ist
      a%atomic_population = 0. ! init

      ! the mixed quantities (density and density matrix) will be allocated on demand
      ist = i
    endif ! a%owner == gc%rank

  endfunction ! allocate_atom_arrays


  status_t function free_atoms( a ) result( ist )
  use type_comp, only: comp_free
  use type_proj, only: proj_free
    type(atom), allocatable, intent(inout)      :: a(:) !! atom list

    character(len=*), parameter                 :: fun = ' free_atoms: '
    integer                                     :: ia

    do ia = 1, size(a)
      a(ia)%offprj =  0
      call comp_free( a(ia)%cmp )
      call proj_free( a(ia)%prj )
      ist = deallocate_atom_arrays( a(ia) )
    enddo ! ia
    deallocate( a, stat=ist )
    if( ist /= 0 .and. o>0) write(o,'(3A,I0)') sym, fun, 'deallocation of parallelized atom list failed, status = ', ist

  endfunction ! free_atoms


  status_t function deallocate_atom_arrays( a ) result( ist )
    type(atom), intent(inout)             :: a !! atom

    character(len=*), parameter           :: fun = ' deallocate_atom_arrays: '

    ! deallocate owner quantities (if allocated)
    deallocate( a%vlm, a%rho, a%pot, a%mix_hist, a%Hm, a%Dm, a%GntDm, a%atomic_population, stat=ist )

    deallocate( a%qlm, stat=ist ) ! deallocate member quantities
    if( ist /= 0 .and.o>0) write(o,'(3A,9(I0,A))') sym, fun, &
      'deallocation of a%qlm failed, atom #', a%ja, ' istatus = ', ist

  endfunction ! deallocate_atom_arrays


  subroutine atomic_population( a, me, o )
  ! determine the atomic_population of
  ! the different enn and ell-channels
  ! integrate over emm-states
  ! i.e. for each ell-channel, the trace over the density
  ! matrix is taken
  use type_species, only: ELLMAX
    type(atom), intent(inout)       :: a !! atom
    integer, intent(in)             :: me !! myrank
    integer, intent(in)             :: o !! output unit

    character(len=*), parameter     :: fun = ' pop: '
    character(len=*), parameter     :: SPDF = 'spdfghi'
    character(len=*), parameter     :: BLNK = '       '
    integer                         :: ell, enn, emm
    integer                         :: i, iln, is, ns
    real                            :: t
    real                            :: show(0:ELLMAX) = 0.

    if( a%owner /= me ) return

    ns = ubound(a%dm,3) ! lbound == 0
#ifdef DEBUG
    if( .not. _allocated_( a%atomic_population ) ) &
      stop 'typeATOM: atomic_population: pointer not allocated.' 
#endif
    a%atomic_population = 0. ! clear
    show = 0.

    do is = 0, ns

      i = 0
      iln = 0
      do ell = 0, a%s%ellmax
        do enn = 1, a%s%nn(ell)
          iln = iln+1
          t = 0. ! temporary
          do emm = -ell, ell
            i = i+1
            t = t + a%dm(i,i,is) ! diagonal elements
          enddo ! emm
          a%atomic_population(iln,is) = t
          if( enn == 1 .and. is == 0 ) show(ell) = show(ell) + t
        enddo ! enn
      enddo ! ell

    enddo ! is


    if(o>0) write(o,'(3A,I6,2A,9(2A,F10.6))') sym, fun, &
      '#', a%ja, '  ', a%s%sym, ( '  ',SPDF(ell+1:ell+1),show(ell), ell=0,a%s%ellmax )

    return ! don''t show the excited state population, because
           ! it is not scaled to have any meaning

#ifdef DEBUG

    ! show also the mix terms
    show = 0.
    is = 0

    i = 0
    iln = 0
    do ell = 0, a%s%ellmax
      do enn = 1, a%s%nn(ell)
        t = 0. ! temporary
        do emm = -ell, ell
          i = i+1
          if( enn == 2 ) t = t + a%dm(i,i-(2*ell+1),0) ! off-diagonal elements
        enddo ! emm
        show(ell) = show(ell) + t
      enddo ! enn
    enddo ! ell

    if(o>0) write(o,'(4A,9(2A,ES10.2))') sym, fun, &
      '# mix    ', a%s%sym, ( '  ',SPDF(ell+1:ell+1),show(ell), ell=0,a%s%ellmax )

    ! show also the first excited projection coefficients trace
    show = 0.
    is = 0

    i = 0
    iln = 0
    do ell = 0, a%s%ellmax
      do enn = 1, a%s%nn(ell)
        iln = iln+1
        t = 0. ! temporary
        do emm = -ell, ell
          i = i+1
          t = t + a%dm(i,i,0) ! diagonal elements
        enddo ! emm
        if( enn == 2 ) show(ell) = show(ell) + t
      enddo ! enn
    enddo ! ell

    if(o>0) write(o,'(4A,9(2A,ES10.2))') sym, fun, &
      '# 2nd    ', a%s%sym, ( '  ',SPDF(ell+1:ell+1),show(ell), ell=0,a%s%ellmax )
#endif
  endsubroutine ! atomic_population


#ifndef DEBUG
  ! in DEBUG mode, the function cannot be elemental, because it contains stops
  elemental &
#endif
  integer function optimal_n_orbitals( a ) result( nopt )
    type(atom), intent(in) :: a

    integer :: ell, enn, iln
#ifdef DEBUG
    integer :: ilnm
    ilnm = 0 ! init counter of orbitals on this atom
    if( .not. _allocated_( a%s )  ) stop 'tATOM optimal_n_orbitals: species pointer not allocated.'
#endif
    nopt = 0 ! init counter of orbitals
    iln  = 0 ! init counter of radial partial waves
    do ell = 0, a%s%ellmax
      do enn = 1, a%s%nn(ell)
        iln = iln+1
        ! a%s%occ_ln: occupation during PAW data generation
#ifdef DEBUG
        if( any( a%s%occ_ln(1:2,iln) > 2*ell+1 ) ) stop 'tATOM error in PAWdata, orbital occupation > 2 ell+1.'
#endif
        if( max( a%s%occ_ln(1,iln), a%s%occ_ln(2,iln) ) > 0. ) then
          ! the best would be a full shell (all emm-states),
          ! because this does not prefer any spatial direction
          nopt = nopt + (2*ell+1)
        endif ! occ > 0.
#ifdef DEBUG
        ilnm = ilnm + (2*ell+1) ! fast forward
#endif
      enddo ! enn
    enddo ! ell
#ifdef DEBUG
    if( iln  /= a%s%mln  ) stop 'tATOM error in species (s%MLN).'
    if( ilnm /= a%s%mlnm ) stop 'tATOM error in species (s%MLNM).'
#endif
  endfunction ! optimal_n_orbitals


  status_t function write_xyz_style( unit, a, comment, iteration ) result( ios )
    integer, intent(in)                    :: unit !! unit number
    type(atom), intent(in)                 :: a(:) !! list of ALL atoms
    character(len=*), intent(in), optional :: comment !!
    integer, intent(in), optional          :: iteration !! will be a comment, too
    ! write to a file or to a unit according the the file format .xyz definitions:
    !
    ! line1:  Number of atoms
    ! line2:  comment
    ! lines:  symb  x     y     z
    character(len=*), parameter     :: fun = ' write_xyz_style: '
    real, parameter                 :: ANG = 0.5291772108618!(18)
    string_t :: cm, it
    integer  :: ia, na

    na = size(a,1) ! number of atoms
    write(unit,'(I0,9A)',iostat=ios) na !, ' atoms'
    if( ios /= 0 ) return ! unable to write to this unit

    cm = '' ; if( present( comment ) ) cm = adjustl(comment)
    it = '' ; if( present( iteration ) ) write(unit=it,fmt='(A,I0)') 'it#', iteration

    ! comment line
    if( cm=='' .and. it=='' ) then
           write(unit,'(9A)',iostat=ios) '# auto generated .xyz file (in Angstrom)'
    else ; write(unit,'(9A)',iostat=ios) '# ', trim(adjustl(it)), ' ', trim(cm), ' (in Angstrom)'
    endif ! present comment

    ! write atoms in Angstrom
    do ia = 1, na
      write(unit,'(A6,3F16.6)',iostat=ios) a(ia)%s%sym, a(ia)%pos*ANG
    enddo ! ia
    write(unit,'(A)',iostat=ios) ! blank line

  endfunction ! write_xyz_style


  ! write a file according the the file format .xyz definitions:
  status_t function write_xyz_file( name, a, comment ) result( ios )
    character(len=*), intent(in)                :: name !! filename without extension .xyz
    type(atom), intent(in)                      :: a(:) !! list of ALL atoms
    character(len=*), intent(in), optional      :: comment

    character(len=*), parameter     :: fun = ' write_xyz_file: '
    iounit_t, parameter             :: iu = 77
    string_t                        :: fname

    ios = 0
    if( size(a) < 1 ) return ! without writing
    write(unit=fname,fmt='(9A)',iostat=ios) trim(name), '.xyz' ! create filename

    open( unit=iu, file=fname, action='write', status='unknown', iostat=ios )
    if( ios /= 0 .and. o>0) write(o,'(5A,I0)') sym, fun, 'opening of file "', trim(fname), '" for writing failed, IOstat = ',ios
    if( ios /= 0 ) return

    ios = write_xyz_style( iu, a, comment )

    close( unit=iu, iostat=ios )
  endfunction ! write_xyz_file


#ifdef EXTENDED
!+ extended

  character(len=256) function atom2string( a ) result( str )
  use constants, only: NUCLEON_MASS
  use unitsystem, only: Ang, Ang_
    type(atom), intent(in) :: a

    status_t :: ios
    character(len=64) :: frc, vel ! forces and velocities
    if( any( abs(a%frc) > 0. ) ) write( unit=frc, fmt='(A,F0.3,2(" ",F0.3),A)', IOstat=ios ) ' force ', a%frc, ' au'
    if( any( abs(a%vel) > 0. ) ) write( unit=vel, fmt='(A,F0.3,2(" ",F0.3),A)', IOstat=ios ) ' velocity ', a%vel, ' au'
    write( unit=str, fmt='(A,F0.1,A,F0.3,2(" ",F0.3),2A,I0,3A,I0,A,F6.1,9A)', IOstat=ios ) &
      'atom Z ', a%s%Z0, ' pos ', a%pos*Ang, Ang_, ' # ', a%ja, ' ', trim(a%label), ' ', &
      a%nimages, ' images, weight ', a%weight/NUCLEON_MASS, ' u', trim(frc), trim(vel)
  endfunction ! atom2string

  character(len=128) function type2string( a ) result( str )
    type(atom), intent(in) :: a !! atom

    write(unit=str,fmt='(A,3F10.6,A,I0,3A)') a%s%sym, a%pos, ' ja=', a%ja, ' ', trim(a%label)
  endfunction ! type2string

  status_t function test( ) result( ios )
    write(*,*,iostat=ios) __FILE__,' no module test implemented!'
  endfunction ! test

!- extended
#endif

endmodule ! type_atom

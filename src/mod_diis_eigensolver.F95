#ifdef DEBUG_ALL
#define DEBUG
#endif

! #define DEBUG
! #define FULL_DEBUG
! #define TIME


#ifdef DEBUG
!!! remove comment from debug line
#define cDBG
#else
!!! comment line
#define cDBG !DBG
#endif

#ifdef NaN_SEARCH
!!! remove comment from NaN search line
#define cNaN
#else
!!! comment line
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


!! @author Paul Baumeister
!! @version 3.0
!!
!! solves the generalized eigenvalue problem using the
!! DirectInversion of the IterativeSubspace method
module diis_eigensolver
  use configuration, only: o ! output unit, 0: no output
implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: sym = 'DIIS' !! module symbol

  integer, parameter, public :: KEY_EIGENSOLVER_DIIS = 0 ! eigensolver collection
  integer, parameter         :: DIIS_HARDLIMIT = 10 ! hard limit of DIIS cycles

  public :: diis_eigen_solve
  interface diis_eigen_solve
    module procedure diis_eig_solve_r, diis_eig_solve_c
  endinterface

#ifdef PARALLELIZE_DIIS_BY_THREADS
  public :: diis_eigen_solve_many
  interface diis_eigen_solve_many
    module procedure diis_eig_solve_many_r, diis_eig_solve_many_c
  endinterface
#endif

#ifdef EXTENDED
!+ extended

  public :: test
  public :: explicit_Hamiltonian
  interface explicit_Hamiltonian
    module procedure explicit_Hamiltonian_r, explicit_Hamiltonian_c
  endinterface

!- extended
#endif

!   public :: subspace_rotation
!   interface subspace_rotation ! not parallelized over bands !
!     module procedure subspace_rotation_r, subspace_rotation_c
!   endinterface

  contains

! end of head part
#endif


#if R1_C2 == 1
#define REAPLEX real
#define conjg(x) (x)
#else
#define REAPLEX complex
#endif

#ifdef PARALLELIZE_DIIS_BY_THREADS

#if R1_C2 == 1
  logical function diis_eig_solve_many_r( &
#else
  logical function diis_eig_solve_many_c( &
  ! same as diis_eig_solv but for some states at the same time to enable an OpenMP parallelization
#endif
      atm, g, kp, &
      vloc, jspin, energy, residual, threshold, mdiis, &
      s, ndiis, jbnd, precon, show_energy ) &
  result( conv )
  use configuration, only: STOPONERROR
  use type_grid, only: grid
  use type_atom, only: atom
  use type_kpoint, only: kpoint
  use operators, only: Hmt
  use operators, only: Precon_Nf1
  use operators, only: scalar_product
  use LAPACK, only: generalized_eig
  use MPItools, only: MPIallsum
  implicit none
    ! parameters
    character(len=*), parameter     :: fun = ' solve: ' ! ' solve_nn: '
    integer, parameter              :: northo = 0
    real, parameter                 :: LAMBDA_MIN = 0.5
    real, parameter                 :: LAMBDA_MAX = 1.0
    real, parameter                 :: LAMBDA_DIIS = 0.3
    real, parameter                 :: EPS_EIG_DIIS = 1.E-14
    real, parameter                 :: RATIO_DIIS = 0.3

    ! all quantities psi, Spsi and Hpsi are treated on the same footing
    integer, parameter              :: I_K=1 ! the \ket{} vector itself
    integer, parameter              :: I_S=2 ! the adjoint vector \hat{S} \ket{}
    integer, parameter              :: I_H=3 ! Hamiltonian times vector \hat H \ket{}
    integer, parameter              :: I_R=4 ! the residual vector

    ! arguments
    type(atom), intent(in)          :: atm(:) ! atoms
    type(grid), intent(in)          :: g ! coarse grid
    type(kpoint), intent(in)        :: kp ! kpoint
    real, intent(in)                :: vloc(:,:,:,:)
    integer, intent(in)             :: jspin
    real, intent(out)               :: energy(:)
    real, intent(out)               :: residual(:)
    real, intent(in)                :: threshold(:)
    integer, intent(in)             :: mdiis

    REAPLEX, intent(inout)          :: s(:,:) ! (nxyzs,nn)
    integer, intent(in), optional   :: ndiis
    integer, intent(in), optional   :: jbnd(:) ! global band index
    logical, intent(in), optional   :: precon
    logical, intent(in), optional   :: show_energy

    ! local vars
!     REAPLEX, allocatable, save      :: a    (:)       !               (0:mdiis)
!     REAPLEX, allocatable, save      :: psi  (:,:,:)   ! (nxyzs,I_K:I_R,0:mdiis)
!     REAPLEX, allocatable, save      :: Pres (:,:)     ! (nxyzs,I_K:I_H)
!     logical, save                   :: temp_arrays_allocated = .false.
    REAPLEX, allocatable            :: psi  (:,:,:,:)   ! (nxyzs,nn,I_K:I_R,0:mdiis)
    REAPLEX, allocatable            :: Pres (:,:,:)     ! (nxyzs,nn,I_K:I_H)
    REAPLEX, allocatable            :: RSmatrix(:,:,:,:)
    REAPLEX, allocatable            :: a      (:)       !               (0:mdiis)
    real, allocatable               :: eigvals(:), energ(:)
!     logical, save                   :: temp_arrays_allocated = .false.
    real, save                      :: array_size = 0.
    real                            :: a_s

    REAPLEX                         :: pHp
    REAPLEX                         :: buff(size(s,2),I_S:I_H)

    logical                         :: precondition
    logical                         :: show_e  ! show energy

    real                            :: lam!, lambda
    real                            :: snorm, ene, f!, fnorm, ray_q, aaa
    logical                         :: run(size(s,2))
    real                            :: resold(size(s,2)), res1st(size(s,2))

    integer                         :: jb(size(s,2)), itt(size(s,2)), nxyzs
    integer                         :: it, jt, i1, i2, ne, ie, nd, nn, ni
    integer                         :: i, ist, k

#define cDBG  

    nn = size(s,2)
! cDBG  if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'treat ', nn, ' eigenstates in a block.'
    conv = ( nn < 1 ) ! init result
    if( conv ) return ! return if less than one eigenstate is passed

cDBG  if( size(energy) /= nn ) stop 'DIIS: diis_eig_solve_many: must pass as many energies as states.'
cDBG  if( size(residual) /= nn ) stop 'DIIS: diis_eig_solve_many: must pass as many residuals as states.'

    precondition = .true. ; if( present( precon ) ) precondition = precon
    
    show_e = .false. ; if( present( show_energy ) ) show_e = show_energy
    nd = mdiis ; if( present( ndiis ) ) nd = min( max( 1, ndiis ), mdiis )
    jb = 0 ; if( present( jbnd ) ) jb = jbnd
!   threshold2 = threshold**2

cDBG  if( lambda_max < lambda_min ) stop 'DIIS: diis_eig_solve: no range: lambda_max < lambda_min.'

    ! arrays have to be allocated

    nxyzs = product( g%ng(1:4) ) ! number of degrees of freedom
cDBG  if( nxyzs /= size(s,1) ) stop 'DIIS solve: nxyzs /= size(s,1)'

    a_s = (nn*(nxyzs*(3+4*(nd+1)))+nd+1)* R1_C2 *0.5**17
! #ifdef DEBUG
    if( a_s > array_size .and. o>0) write(o,'(3A,F0.3,9A)') sym, fun, 'try to allocate DIIS arrays of ', a_s, ' MiByte'
! #endif
    array_size = a_s ! store largest array size

    allocate( a(0:nd), psi(nxyzs,nn,I_K:I_R,0:nd), Pres(nxyzs,nn,I_K:I_H), energ(nn), stat=ist )
    if( ist /= 0 ) stop 'diis_eig_solve_many: allocation failed.'

    psi  = 0. ! init clear
    Pres = 0. ! init clear

    psi(:,:,I_K,0) = s(:,:) ! |psi(0)> = |s> load the wave function

    run = .true.
    it = 0
    do while( any( run ) )
      it = it+1
! cDBG  if(o>0) write(o,'(3A,I3,A,I5)') sym, fun, 'start iteration #', it, ' band#', jbnd

      ! |Hpsi(it-1)> = \hat{H} |psi(it-1)>, |Spsi(it-1)> = \hat{S} |psi(it-1)>
      if( it == 1 ) &
      call Hmt( g, vloc, jspin, atm, kp, ket=psi(:,:,I_K,it-1), Hket=psi(:,:,I_H,it-1), Sket=psi(:,:,I_S,it-1) )

cNaN  if( any(psi(:,:,it-1)/=psi(:,:,it-1)) ) stop 'NaN in  psi(:,it-1)! (1st)'

      ! energy = <psi(it-1)|Hpsi(it-1)>

!$omp parallel
!$omp do private(ni,pHp)
      do ni = 1, nn
        pHp = scalar_product( psi(:,ni,I_K,it-1), psi(:,ni,I_H,it-1), g%hvol ) ! communication collected
        energ(ni) = real( pHp )
      enddo ! ni
!$omp end do
!$omp master
      call MPIallsum( energ, g%comm ) ! communication collected
! cDBG  if(o>0) write(o,'(3A,I6,I2,I6,A,F16.6,A,ES8.1E2,A)') sym, fun, &
! cDBG    'band(', jb(ni), ispn, kp%jk, '): energy =', energ(ni), ' +/-', residual(ni), ' Ha'
cNaN  if( any(energ/=energ) ) stop 'NaN in  energy(ib)! (1st)'
!$omp end master
!$omp do private(ni)
      do ni = 1, nn
        energy(ni) = energ(ni) ! write to output
#ifdef FULL_DEBUG
cDBG  if(o>0) write(o,'(2A,I6,A,F24.16,A,ES8.1E2,A)') sym, fun, jb(ni), ' energy =', energy(ni), ' +/-', residual(ni), ' Ha'
#endif
        ! |res(it-1)> = |Hpsi(it-1)> - E(ib) * |Spsi(it-1)>
        psi(:,ni,I_R,it-1) = psi(:,ni,I_H,it-1) - energy(ni) * psi(:,ni,I_S,it-1)
      enddo ! ni
!$omp end do
!$omp end parallel

      ! preconditioning of the residual vector
      ! |Pres(it-1)> = \hat{P} |res(it-1)>
      if( precondition ) then
!$      stop 'DIIS: line 271 needs OpenMP here!'
        do ni = 1, nn
          call Precon_Nf1( g, kp, v=psi(:,ni,I_R,it-1), Pv=Pres(:,ni,I_K) )
        enddo ! ni
      else  ! precondition
        Pres(:,:,I_K) = psi(:,:,I_R,it-1) ! Preconditioning operator is unity
      endif ! precondition

      ! |SPres(it-1)> = hat{S} |Pres(it-1)>
      call Hmt( g, vloc, jspin, atm, kp, ket=Pres(:,:,I_K), Hket=Pres(:,:,I_H), Sket=Pres(:,:,I_S) )

      lam = LAMBDA_MIN

    ! |psi(it)> = |psi(it-1)> + lambda * |Pres(it-1)> ! with new lambda
    psi(:,:,I_K:I_H,it) = psi(:,:,I_K:I_H,it-1) + lam * Pres(:,:,I_K:I_H)

    allocate( RSmatrix(0:it,0:it,1:2,nn), eigvals(0:it), stat=ist )
cDBG  if( ist /= 0 ) stop 'diis_eig_solve_many: allocation of RSmatrix failed.'
    RSmatrix = 0.

!$omp parallel
!$omp do private(ni)
    do ni = 1, nn
      buff(ni,I_S) = scalar_product( psi(:,ni,I_K,it), psi(:,ni,I_S,it), g%hvol )!, g%comm ) ! cc
!       buff(ni,I_H) = scalar_product( psi(:,ni,I_K,it), psi(:,ni,I_H,it), g%hvol )!, g%comm ) ! cc
    enddo ! ni
!$omp end do
!$omp master
    call MPIallsum( buff, g%comm ) ! collected MPI communication (cc)
!   if( any( buff(:,I_S) < 1E-14 ) ) stop 'DIIS: norm of <psi(it)|S|psi(it)> is less than 10^-14'
!$omp end master
!$omp do private(ni,snorm,f)
    do ni = 1, nn
      ! renormalize psi
      ! f = 1./sqrt( real( <phi(it)|Sphi(it)>  ) )
      snorm = real( buff(ni,I_S) )
      f = 1./sqrt( snorm )

      ! normalize |psi(it)> = f * |psi(it)>
      psi(:,ni,I_K:I_H,it) = f * psi(:,ni,I_K:I_H,it)

    enddo ! ni
!$omp end do
!$omp do private(ni)
    do ni = 1, nn
!       buff(ni,I_S) = scalar_product( psi(:,ni,I_K,it), psi(:,ni,I_S,it), g%hvol )!, g%comm ) ! cc
      buff(ni,I_H) = scalar_product( psi(:,ni,I_K,it), psi(:,ni,I_H,it), g%hvol )!, g%comm ) ! cc
    enddo ! ni
!$omp end do
!$omp master
    call MPIallsum( buff, g%comm ) ! collected MPI communication (cc)
!   if( any( buff(:,I_S) < 1E-14 ) ) stop 'DIIS: norm of <psi(it)|S|psi(it)> is less than 10^-14'
!$omp end master
!$omp do private(ni,snorm,f,ene,i1,i2)
    do ni = 1, nn
!       ! renormalize psi
!       ! f = 1./sqrt( real( <phi(it)|Sphi(it)>  ) )
!       snorm = real( buff(ni,I_S) )
!       f = 1./sqrt( snorm )
! 
!       ! normalize |psi(it)> = f * |psi(it)>
!       psi(:,ni,I_K:I_H,it) = f * psi(:,ni,I_K:I_H,it)
! 
!       ! E = <psi(it)|Hpsi(it)>
!       ene = real( f * buff(ni,I_H) * f ) ! new energy

      energ(ni) = real( buff(ni,I_H) )
#ifdef FULL_DEBUG
cDBG  if(o>0) write(o,'(2A,I6,A,F24.16,A,ES8.1E2,A)') sym, fun, jb(ni), ' ene    =', energ(ni), ' +/-', residual(ni), ' Ha'
#endif

      ! create set of residual vectors
      do jt = 0, it
        ! |res(jt)> = |Hpsi(jt)> - E |Spsi(jt)>
        psi(:,ni,I_R,jt) = psi(:,ni,I_H,jt) - energ(ni) * psi(:,ni,I_S,jt)
      enddo ! jt

cNaN    if( any( psi /= psi ) ) stop 'DIIS: NaN in  psi! (2nd)'

      ! set up the matrices
      do i1 = 0, it
        do i2 = 0, i1 ! triangular loop exploits symmetric/hermitian property
          ! Smatrix(i2,i1) = <phi(i1)|Sphi(i2)>
          RSmatrix(i2,i1,2,ni) = scalar_product( psi(:,ni,I_S,i2), psi(:,ni,I_K,i1), g%hvol )
          ! Rmatrix(i2,i1) = <res(i1)|res(i2)>
          RSmatrix(i2,i1,1,ni) = scalar_product( psi(:,ni,I_R,i2), psi(:,ni,I_R,i1), g%hvol )

          ! symmetric/hermitian matrices
          RSmatrix(i1,i2,2,ni) = complex_conjugate( RSmatrix(i2,i1,2,ni) )
          RSmatrix(i1,i2,1,ni) = complex_conjugate( RSmatrix(i2,i1,1,ni) )

        enddo ! i2
      enddo ! i1

    enddo ! ni
!$omp end do
!$omp master
    call MPIallsum( RSmatrix, g%comm ) ! reduce over real-space grid ! collected MPI communication
cNaN  if( any( RSmatrix /= RSmatrix ) ) stop 'DIIS: NaN in RSmatrix!'
!$omp end master
!$omp do private(ni,ist,eigvals,ne,k,ie,a)
    do ni = 1, nn

      ! Diagonalization
      ist = generalized_eig( RSmatrix(:,:,1,ni), RSmatrix(:,:,2,ni), eigvals ) ! solve by LAPACK
cDBG  if( ist /= 0 .and. o>0) write(o,'(9A)') sym, fun, 'diagonalization failed.'
      if( ist /= 0 ) return ! failed

! !$omp critical
! !$omp end critical

      ! the eigenvectors are contained in RSmatrix(:,:,1,ni) now
cDBG  if(o>0) write(o,'(3A,99ES10.2)') sym, fun, 'eigenvalues of Ax=eBx : ', eigvals(:)

      ! eliminate too small eigenvalues
      ne = 0 ! init the counter for valid eigenvalues
      do k = it, 0, -1
        if( eigvals(k) < EPS_EIG_DIIS ) cycle ! too small
        ne = ne+1 ! count
        ie = k ! store index of the smallest k, where eigvals(k) is not too small
      enddo !  k

      itt(ni) = it

      if( ne > 1 ) then

        if( it >  1 ) resold(ni) = residual(ni) ! pass
        if( it == 2 ) res1st(ni) = residual(ni)/resold(ni)
        residual(ni) = abs( eigvals(ie) )

        if( it > 2 .and. residual(ni) < ratio_diis * resold(ni) * res1st(ni) ) then

          residual(ni) = abs( resold(ni) )
          itt(ni) = it-1
          run(ni) = .false.

        else  ! it > 2 .and. ...

          a(0:it) = RSmatrix(0:it,ie,1,ni) ! get the eigenvector that corresponds to eigenvalue ie

          ! |psi(it)> := \sum_{jt=0}^{it} a(jt) * |psi(jt)>
          ! if(o>0) write(o,'(3A,I3)') sym, fun, 'psi(it) := \sum_j a(j) * psi(j)'

          ! |psi(it)> = a(it) * |psi(it)>
          psi(:,ni,I_K:I_H,it) = a(it) * psi(:,ni,I_K:I_H,it)
          do jt = it-1, 0, -1
            if( a(jt) == 0. ) cycle
            ! |psi(it)> = |psi(it)>  + a(jt) * |psi(jt)>
            psi(:,ni,I_K:I_H,it) = psi(:,ni,I_K:I_H,it) + a(jt) * psi(:,ni,I_K:I_H,jt)
          enddo ! jt

        endif ! it > 2 .and. ...

      elseif( ne == 1 ) then

        ! psi, Spsi and Hpsi are unchanged
        ! if(o>0) write(o,'(3A,I3)') sym, fun, 'psi unchanged, because m==1 in it#', it

      else ; stop 'DIIS: no eigenvalue > Threshold.'
      endif

    enddo ! ni
!$omp end do
!$omp end parallel

      deallocate( RSmatrix, eigvals, stat=ist )

      conv = all( residual < threshold )
      run = run .and. ( ( it < nd ) .and. ( .not. conv ) )
    enddo ! while run

    do ni = 1, nn
      s(:,ni) = psi(:,ni,I_K,itt(ni)) ! store |s> = |psi(it)>
    enddo ! ni

    if( show_e ) then
      do ni = 1, nn
        if(o>0) write(o,'(3A,I6,I2,I6,A,F24.16,A,ES8.1E2,A)') sym, fun, &
          'band(', jb(ni), jspin, kp%jk, '): energy =', energy(ni), ' +/-', residual(ni), ' Ha'
      enddo ! ni
    endif ! show_e

#ifdef DEBUG
#define cDBG
#else
#define cDBG !DBG
#endif

  endfunction ! diis_eig_solve_many
#endif




#if R1_C2 == 1
  logical function diis_eig_solve_r( &
#else
  logical function diis_eig_solve_c( &
#endif
      atm, g, kp, &
      vloc, jspin, energy, residual, threshold, mdiis, &
      s, ndiis, jbnd, precon, show_energy ) &
  result( conv )
  use configuration, only: STOPONERROR
  use type_grid, only: grid
  use type_atom, only: atom
  use type_kpoint, only: kpoint
  use operators, only: Hmt
  use operators, only: Precon_Nf1
  use operators, only: scalar_product
  use LAPACK, only: generalized_eig
  use MPItools, only: MPIallsum
  implicit none
    ! parameters
    character(len=*), parameter     :: fun = ' solve: '
    integer, parameter              :: northo = 0
    real, parameter                 :: LAMBDA_MIN = 0.5
    real, parameter                 :: LAMBDA_MAX = 1.0
    real, parameter                 :: LAMBDA_DIIS = 0.3
    real, parameter                 :: EPS_EIG_DIIS = 1.E-14
    real, parameter                 :: RATIO_DIIS = 0.3

    ! all quantities psi, Spsi and Hpsi are treated on the same footing
    integer, parameter              :: I_K=1 ! the \ket{} vector itself
    integer, parameter              :: I_S=2 ! the adjoint vector \hat{S} \ket{}
    integer, parameter              :: I_H=3 ! Hamiltonian times vector \hat H \ket{}
    integer, parameter              :: I_R=4 ! the residual vector

    ! arguments
    type(atom), intent(in)          :: atm(:) ! atoms
    type(grid), intent(in)          :: g ! coarse grid
    type(kpoint), intent(in)        :: kp            ! kpoint
    real, intent(in)                :: vloc(:,:,:,:)
    integer, intent(in)             :: jspin
    real, intent(out)               :: energy
    real, intent(out)               :: residual
    real, intent(in)                :: threshold
    integer, intent(in)             :: mdiis

    REAPLEX, intent(inout)          :: s(:,1:) ! (nxyzs,1)
    integer, intent(in), optional   :: ndiis
    integer, intent(in), optional   :: jbnd ! global band index
    logical, intent(in), optional   :: precon
    logical, intent(in), optional   :: show_energy

    ! local vars
    REAPLEX, allocatable, save      :: a    (:)       !               (0:mdiis)
    REAPLEX, allocatable, save      :: psi  (:,:,:)   ! (nxyzs,I_K:I_R,0:mdiis)
    REAPLEX, allocatable, save      :: Pres (:,:)     ! (nxyzs,I_K:I_H)

    REAPLEX                         :: pHp!, pSp, rPHp, rPSp, rPr, tmp(I_S:I_R)
    REAPLEX, allocatable            :: RSmatrix(:,:,:)
    real, allocatable               :: eigvals(:)

    logical                         :: precondition
    logical                         :: show_e  ! show energy

    real                            :: lam!, lambda
    real                            :: snorm, ene, f!, fnorm, ray_q, aaa
    logical, save                   :: temp_arrays_allocated = .false.
    logical                         :: run
    real                            :: threshold2
    real                            :: resold, res1st

    integer                         :: jb, nxyzs
    integer                         :: it, jt, i1, i2, ne, ie, nd
    integer                         :: i, ist, k

    if( size(s,2) > 1 ) stop 'DIIS: please pass only one wf with rank2 to diis, i.e. s(:,ib:ib)'
    conv = .false. ! init result

    precondition = .true. ; if( present( precon ) ) precondition = precon
    show_e = .false. ; if( present( show_energy ) ) show_e = show_energy
    nd = mdiis ; if( present( ndiis ) ) nd = min( max( 1, ndiis ), mdiis )
    jb = 1 ; if( present( jbnd ) ) jb = jbnd

    threshold2 = threshold**2

cDBG  if( lambda_max < lambda_min ) stop 'DIIS: diis_eig_solve: no range: lambda_max < lambda_min.'

    if( .not. temp_arrays_allocated ) then
      ! arrays have to be allocated (only the 1st time)
      nxyzs = product( g%ng(1:4) ) ! number of degrees of freedom
! #ifdef DEBUG
      if(o>0) write(o,'(3A,F0.3,9A)') sym, fun, 'try to allocate DIIS arrays of ', &
        (nxyzs*(3+4*(mdiis+1))+mdiis+1)* R1_C2 *0.5**17, ' MiByte'
! #endif
      allocate( a(0:mdiis), psi(nxyzs,I_K:I_R,0:mdiis), Pres(nxyzs,I_K:I_H), stat=ist )
      if( ist /= 0 ) stop 'diis_eig_solve: allocation failed.'
      temp_arrays_allocated = ( ist == 0 )

    endif ! not temp_arrays_allocated


    ! clear
    psi  = 0.
    Pres = 0.

    ! |psi(0)> = |s>
    psi(:,I_K,0) = s(:,1) ! load the wave function

    run = .true.
    it = 0
    do while( run )
      it = it+1
! cDBG  if(o>0) write(o,'(3A,I3,A,I5)') sym, fun, 'start iteration #', it, ' band#', jbnd

      ! |Hpsi(it-1)> = \hat{H} |psi(it-1)>, |Spsi(it-1)> = \hat{S} |psi(it-1)>
      if( it == 1 ) &
      call Hmt( g, vloc, jspin, atm, kp, ket=psi(:,I_K:I_K,it-1), Hket=psi(:,I_H:I_H,it-1), Sket=psi(:,I_S:I_S,it-1) )

cNaN  if( any(psi(:,:,it-1)/=psi(:,:,it-1)) ) stop 'NaN in  psi(:,it-1)! (1st)'

      ! energy = <psi(it-1)|Hpsi(it-1)>
      pHp = scalar_product( psi(:,I_K,it-1), psi(:,I_H,it-1), g%hvol, g%comm )
      energy = real( pHp )
#ifdef FULL_DEBUG
cDBG  if(o>0) write(o,'(2A,I6,A,F24.16,A,ES8.1E2,A)') sym, fun, jb, ' energy =', energy, ' +/-', residual, ' Ha'
#endif
! cDBG  if(o>0) write(o,'(3A,I6,I2,I6,A,F16.6,A,ES8.1E2,A)') sym, fun, &
! cDBG    'band(', ib, ispn, kp%jk, '): energy =', energy, ' +/-', residual, ' Ha'

cNaN  if( energy/=energy ) stop 'NaN in  energy(ib)! (1st)'

      ! |res(it-1)> = |Hpsi(it-1)> - E(ib) * |Spsi(it-1)>
      psi(:,I_R,it-1) = psi(:,I_H,it-1) - energy * psi(:,I_S,it-1)


#ifdef DEBUG
!! output for 1D wave functions: residual vector I_R
  if( all( g%ng_all(2:) == 1 ) ) then
    write(135,'(A)') '', '#'
#if R1_C2 == 1
    write(135,'(F10.3,1ES16.6)') ((i-.5)*g%h(1),psi(i,I_R,it-1),i=1,size(psi,1))
#else
    write(135,'(F10.3,2ES16.6)') ((i-.5)*g%h(1),psi(i,I_R,it-1),i=1,size(psi,1))
#endif
  endif ! g%ng_all = (/ Nx,1,1,1 /)
#endif


      ! preconditioning of the residual vector
      ! |Pres(it-1)> = \hat{P} |res(it-1)>
      if( precondition ) then
! cDBG    if(o>0) write(o,'(3A,I6)') sym, fun, 'Precon!, jbnd=', jb
! cDBG    call Precon_a( g, kp, v=psi(:,I_R,it-1), Pv=Pres(:,I_K) )
        call Precon_Nf1( g, kp, v=psi(:,I_R,it-1), Pv=Pres(:,I_K) )
      else  ! precondition
        Pres(:,I_K) = psi(:,I_R,it-1) ! Preconditioning operator is unity
      endif ! precondition

      ! |SPres(it-1)> = hat{S} |Pres(it-1)>
      call Hmt( g, vloc, jspin, atm, kp, ket=Pres(:,I_K:I_K), Hket=Pres(:,I_H:I_H), Sket=Pres(:,I_S:I_S) )

      lam = LAMBDA_MIN

      ! |psi(it)> = |psi(it-1)> + lambda * |Pres(it-1)> ! with new lambda
      psi(:,I_K:I_H,it) = psi(:,I_K:I_H,it-1) + lam * Pres(:,I_K:I_H)

      ! renormalize psi

      ! f = 1./sqrt( real( <phi(it)|Sphi(it)>  ) )
      snorm = scalar_product( psi(:,I_K,it), psi(:,I_S,it), g%hvol, g%comm )
      if( snorm < 1E-14 ) stop 'DIIS: norm of <psi(it)|S|psi(it)> is less than 10^-14'
      f = 1./sqrt( snorm )

      ! normalize |psi(it)> = f * |psi(it)>
      psi(:,I_K:I_H,it) = f * psi(:,I_K:I_H,it)

      ! E = <psi(it)|Hpsi(it)>
      pHp = scalar_product( psi(:,I_K,it), psi(:,I_H,it), g%hvol, g%comm )
cNaN  if( pHp /= pHp ) stop 'DIIS: NaN in  <psi(it)|H|psi(it)> (2nd)'

      ! new energy
      ene = real( pHp )
#ifdef FULL_DEBUG
cDBG  if(o>0) write(o,'(2A,I6,A,F24.16,A,ES8.1E2,A)') sym, fun, jb, ' ene    =', energy, ' +/-', residual, ' Ha'
#endif

      ! create set of residual vectors
      do jt = 0, it
        ! |res(jt)> = |Hpsi(jt)> - E |Spsi(jt)>
        psi(:,I_R,jt) = psi(:,I_H,jt) - ene * psi(:,I_S,jt)
      enddo ! jt

cNaN  if( any( psi /= psi ) ) stop 'DIIS: NaN in  psi! (2nd)'

      allocate( RSmatrix(0:it,0:it,1:2), eigvals(0:it) )
      RSmatrix = 0.

      ! set up the matrices
      do i1 = 0, it
        do i2 = 0, i1 ! triangular loop exploits symmetric/hermitian property
          ! Smatrix(i2,i1) = <phi(i1)|Sphi(i2)>
          RSmatrix(i2,i1,2) = scalar_product( psi(:,I_S,i2), psi(:,I_K,i1), g%hvol )
          ! Rmatrix(i2,i1) = <res(i1)|res(i2)>
          RSmatrix(i2,i1,1) = scalar_product( psi(:,I_R,i2), psi(:,I_R,i1), g%hvol )

! cNaN    if(RSmatrix(i2,i1,2)/=RSmatrix(i2,i1,2)) stop 'DIIS: NaN in Smatrix'
! cNaN    if(RSmatrix(i2,i1,1)/=RSmatrix(i2,i1,1)) stop 'DIIS: NaN in Rmatrix'

          ! symmetric/hermitian matrices
          RSmatrix(i1,i2,2) = conjg( RSmatrix(i2,i1,2) )
          RSmatrix(i1,i2,1) = conjg( RSmatrix(i2,i1,1) )

        enddo ! i2
      enddo ! i1

      call MPIallsum( RSmatrix, g%comm ) ! reduce over real-space grid ! communication collected
cNaN  if( any( RSmatrix /= RSmatrix ) ) stop 'DIIS: NaN in RSmatrix!'

      ! Diagonalization
      ist = generalized_eig( RSmatrix(:,:,1), RSmatrix(:,:,2), eigvals ) ! solve by LAPACK
      if( ist /= 0 ) then
cDBG    if(o>0) write(o,'(9A)') sym, fun, 'diagonalization failed.'
        return ! failed
      endif ! ist /= 0
      ! the eigenvectors are contained in RSmatrix(:,:,1) now
cDBG  if(o>0) write(o,'(3A,99ES10.2)') sym, fun, 'eigenvalues of Ax=eBx : ', eigvals

      ! eliminate too small eigenvalues
      ne = 0
      do k = it, 0, -1
        if( eigvals(k) < EPS_EIG_DIIS ) cycle
        ne = ne+1
        ie = k
      enddo !  k

      if( ne > 1 ) then

        if( it >  1 ) resold = residual
        if( it == 2 ) res1st = residual/resold
        residual = abs( eigvals(ie) )

        if( it > 2 .and. residual < ratio_diis*resold*res1st ) then
          it = it-1
          residual = abs( resold )
          run = .false.
        else  ! it > 2 .and. ...

          a(0:it) = RSmatrix(0:it,ie,1) ! get the eigenvector that corresponds to eigenvalue ie

          ! |psi(it)> := \sum_{jt=0}^{it} a(jt) * |psi(jt)>
          ! if(o>0) write(o,'(3A,I3)') sym, fun, 'psi(it) := \sum_j a(j) * psi(j)'

          ! |psi(it)> = a(it) * |psi(it)>
          psi(:,I_K:I_H,it) = a(it) * psi(:,I_K:I_H,it)
          do jt = it-1, 0, -1
            if( a(jt) == 0. ) cycle
            ! |psi(it)> = |psi(it)>  + a(jt) * |psi(jt)>
            psi(:,I_K:I_H,it) = psi(:,I_K:I_H,it) + a(jt) * psi(:,I_K:I_H,jt)
          enddo ! jt

        endif ! it > 2 .and. ...

      elseif( ne == 1 ) then

        ! psi, Spsi and Hpsi are unchanged
        ! if(o>0) write(o,'(3A,I3)') sym, fun, 'psi unchanged, because m==1 in it#', it

      else ; stop 'DIIS: no eigenvalue > Threshold.'
      endif


      deallocate( RSmatrix, eigvals )

      conv = ( residual < threshold )
      run = run .and. ( ( it < nd ) .and. ( .not. conv ) )
    enddo ! while run

    ! if(o>0) write(o,'(3A,I3,A,I5)') sym, fun, 'after iteration #', it, ' band#', ib

    ! store |s>  = |psi(it)>
    s(:,1) = psi(:,I_K,it)

    if( show_e ) then
      if(o>0) write(o,'(3A,I6,I2,I6,A,F24.16,A,ES8.1E2,A)') sym, fun, &
        'band(', jb, jspin, kp%jk, '): energy =', energy, ' +/-', residual, ' Ha'
    endif ! show_e

  endfunction ! diis_eig_solve


#ifdef EXTENDED
!+ extended features


#if R1_C2 == 1
  !! diagonalize the full Hamiltonian in real-space
  !! representation using a serial LAPACK routine --- VERY EXPENSIVE
  integer function explicit_Hamiltonian_r( &
#else
  integer function explicit_Hamiltonian_c( &
#endif
      g, vloc, jspin, atm, naprj, kp, spsi, energy ) &
  result( ist )
  use type_grid, only: grid
  use type_atom, only: atom
  use type_kpoint, only: kpoint
  use operators, only: Hmt
  use LAPACK, only: generalized_eig
  use MPIconst, only: Wtime
  use configuration, only: WARNING
cDBG  use toolbox, only: write_bmp_file
cDBG  use LAPACK, only: check_matrix
  implicit none
    ! parameter
    character(len=*), parameter           :: fun = ' explicit_Hamiltonian: '
    real, parameter                       :: MiBYTES = R1_C2 * 8./(1024.**2)
    integer, parameter                    :: BLOCKs = 64
    ! arguments
    type(grid), intent(in)                :: g
    real, intent(in)                      :: vloc(:,:,:,:)
    integer, intent(in)                   :: jspin
    type(atom), intent(in)                :: atm(:)
    integer, intent(in)                   :: naprj
    type(kpoint), intent(in)              :: kp
    REAPLEX, intent(out)                  :: spsi(:,:)
    real, intent(out)                     :: energy(:)

    ! local vars
    REAPLEX, allocatable                  :: s(:,:)
!     REAPLEX                               :: c(naprj)
    REAPLEX, allocatable                  :: Hm(:,:)
    REAPLEX, allocatable                  :: Sm(:,:)
    real, allocatable                     :: Ev(:) ! eigenvalues
    integer                               :: n, i, j, nB, iB, mB, iBo
cDBG  integer                             :: ia, na, nerr
    integer*8, save                       :: memorysize = 0
    real                                  :: t(0:9) ! time
    t=0. ; t(0) = Wtime() ! start time

    ist = -1 ! init as error result for early return

    if( any( g%nproc > 1 ) ) stop 'DIIS explicit_Hamiltonian: only in serial version!'

cDBG  do ia = 1, size(atm)
cDBG    nerr = check_matrix( atm(ia)%Hm(:,:,1), name='a%Hm' )
cDBG  enddo ! ia

    n = product( g%ng(1:4) ) ! number of grid degrees of freedom
cDBG  if(o>0) write(o,'(3A,I8)') sym, fun, 'dimension =', n

    if( memorysize /= 2*n*(n*2+4) ) then
      memorysize = 2*n*(n*2+4)
      if(o>0) write(o,'(4A,F10.3,9A)') sym, fun, WARNING(0), 'extra memory consumption', memorysize*MiBYTES, ' MiByte'
    endif ! memory size has changed

    if( n > 2**12 ) then ! 16**3 = 4096, 2**12 = 4096, more than 800sec
      if(o>0) write(o,'(4A,I8,9A)') sym, fun, WARNING(0), 'dimension very large, N=', n
    elseif( n > 2**13 ) then ! 20**3 = 8000, 2**13 = 8192
      if(o>0) write(o,'(4A,I8,9A)') sym, fun, WARNING(0), 'dimension too large, not diagonalized, N=', n
      return ! very large
    endif ! n

    nB = (n-1)/BLOCKs+1
    mB = min( n, BLOCKs )

    ! aux. vector |s>, Hamiltonian, Overlap matrix and eigenvalues
    allocate( s(n,mB), Hm(n,n), Sm(n,n), Ev(n), stat=ist )
    if( ist /= 0 ) stop 'DIIS explicit_Hamiltonian: allocation failed!'

    ! set up the Hamiltonian \hat{H} and the OverlapMatrix \hat{S}
    t(1) = Wtime() ! start the setup

    do iB = 1, nB ! for each block
      iBo = (iB-1)*BLOCKs ! offset
      mB = min( n-iBo, BLOCKs ) ! size of this block

      ! prepare the ket vectors |s>
      s = 0. ! init
      do i = 1, mB
        s(iBo+i,i) = g%hvol**(-0.5) ! normalized delta peak state
      enddo ! i

      ! apply the Hamiltonian onto those vectors
      call Hmt( g, vloc, jspin, atm, kp, ket=s(:,1:mB), Hket=Hm(:,iBo+1:iBo+mB), Sket=Sm(:,iBo+1:iBo+mB) )

    enddo ! iB

    t(2) = Wtime() ! end the setup

    deallocate( s ) ! auxiliary state vector s is not needed any more

cNaN  if(any(Hm/=Hm)) stop 'DIIS explicit_Hamiltonian: NaN in Hm.'
cNaN  if(any(Sm/=Sm)) stop 'DIIS explicit_Hamiltonian: NaN in Sm.'

#if R1_C2 == 1
cDBG  ist = write_bmp_file( 'dmp/Hmat', Hm, style='minmax' )
cDBG  ist = write_bmp_file( 'dmp/Smat', Sm, style='minmax' )
#endif
cDBG  nerr = check_matrix( Hm, name='Hm' ) ; if( nerr > 0 ) stop 'DIIS explicit_Hamiltonian: Hm is not Hermitian/symmetric!'
cDBG  nerr = check_matrix( Sm, name='Sm' ) ; if( nerr > 0 ) stop 'DIIS explicit_Hamiltonian: Sm is not Hermitian/symmetric!'

    if(o>0) write(o,'(3A,9(I6,A))') sym, fun, 'start explicit diagonalization of', n, ' x', n, ' Hamiltonian'
    ! solve the generalized eigenvalue problem Hm x = Ev Sm x
    t(3) = Wtime() ! end the setup
    ist = generalized_eig( Hm, Sm, Ev )
    t(4) = Wtime() ! end the setup

#if R1_C2 == 1
cDBG  ist = write_bmp_file( 'dmp/Eigv', Hm, style='minmax' )
#endif

    if( ist /= 0 ) then
      if(o>0) write(o,'(9A)') sym, fun, 'explicit diagonalization failed.'
      return ! error
    endif ! no SUCCESS
cDBG  if(o>0) write(o,'(9A)') sym, fun, 'explicit diagonalization done.'

cDBG    do i = 1, min(8,n)
cDBG      if(o>0) write(o,'(3A,I3,F16.10)') sym, fun, 'eigenvalues #', i, Ev(i)
cDBG    enddo ! i

    ! output the lowest eigenvectors to spsi

cDBG  if( size(energy) /= size(spsi,2) ) stop 'DIIS explicit_Hamiltonian: dim#1 of ENERGY is wrong.'
    if( n /= size( spsi, 1 ) ) stop 'DIIS explicit_Hamiltonian: matrix is not fully explicit!'

    ! copy the first nbnd wave functions
    t(5) = Wtime() ! start passing the wave functions
    do i = 1, size(spsi,2)
      spsi(:,i) = Hm(:,i)
      energy(i) = Ev(i) ! and eigenvalues
    enddo ! ibnd
    t(6) = Wtime() ! end passing the wave functions

    deallocate( Hm, Sm, Ev, stat=ist )

    t(7) = Wtime() ! end all
#ifdef TIME
    if(o>0) write(o,'(3A,4F10.3,9A)') sym, fun, 'times [set,sol,lin|tot]', t(2:6:2)-t(1:5:2), t(7)-t(0), ' sec'
#endif
  endfunction ! explicit_Hamiltonian

!- extended features
#endif



#if R1_C2 == 2
! begin of module tail
#undef REAPLEX

  real function dagger_r0( x ) result( xd )
    real, intent(in)    :: x
    xd = x
  endfunction ! dagger

  complex function dagger_c0( x ) result( xd )
    complex, intent(in) :: x
    xd = conjg(x)
  endfunction ! dagger

  function dagger_r2( x ) result( cc )
    real, intent(in)    :: x(:,:)
    real    :: cc(size(x,2),size(x,1))
    cc = transpose(x)
  endfunction ! dagger

  function dagger_c2( x ) result( cc )
    complex, intent(in) :: x(:,:)
    complex :: cc(size(x,2),size(x,1))
    cc = conjg(transpose(x))
  endfunction ! dagger

#ifdef EXTENDED
!+ extended

  integer function test( )
    write(*,*,iostat=test) __FILE__,' no module test implemented!'
  endfunction ! test

!- extended
#endif

endmodule ! diis_eigensolver
#endif
#endif

#include "config.h"

! #define DEBUG
! #define FULL_DEBUG

#ifdef TIME
#define cTIM  
#else
#define cTIM !TIM
#endif

#ifdef DEBUG
#define cDBG  
#else
#define cDBG !DBG
#endif

#ifdef NaN_SEARCH
#define cNaN  
#else
#define cNaN !NaN
#endif

#define SYMMETRIC

#define IHS 2*ib1

! configure the module here for memory consumption vs. number of communications:
! the allreduce operation over the grid communicator can be collected
! the memory consumption goes as nbmax * IND, where IND is also nbmax, the max. local number of bands

#ifdef NOScaLAPACK
! #define IND ib1
#endif



#ifndef IND
! save some memory
#define IND 1
#endif

#ifndef IHS
! save some memory
#define IHS 2
#endif


#ifdef R1_C2
! This file will be preprocessed twice, first
! with R1_C2 == 1, and then with R1_C2 == 2
! then the two parts are concatenated to one source.
! This requires that the module head has to be
! appearing, when R1_C2 == 1 and the line "end module <name>"
! with R1_C2 == 2.

! begin double preprocessing
#if R1_C2 == 1
! begin module head

!! @author Paul Baumeister
!! @version 4.00
!!
!! subspace rotations (important with the
!! diis eigensolver)
!! making use of the ScaLAPACK module
!! works with band parallelization
module subspace
  use configuration, only: o ! output unit, 0: no output
implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: sym = 'SUB' !! module symbol

  public :: subspace_rotation
#ifdef EXTENDED
  public :: Hamiltonian2CRS
  public :: Hamiltonian2file
  public :: Hamiltonian2ASCII
  public :: test
#endif

  interface subspace_rotation ! bands parallel and ScaLAPACK on a subset of the gridcomm
    module procedure SR_usememory_r, SR_usememory_c !, p_SR_r, p_SR_c
    !module procedure p_SR_r, p_SR_c
  endinterface

#ifdef EXTENDED
  interface pp_subspace_rotation ! bands parallel and ScaLAPACK on a subset of the gridcomm
    module procedure pp_SR_r, pp_SR_c
  endinterface

  interface p1_subspace_rotation ! LAPACK and no band parallelization
    module procedure p1_SR_r, p1_SR_c
  endinterface

  interface Hamiltonian2CRS
    module procedure Hamiltonian2CRS_r, Hamiltonian2CRS_c
  endinterface

  interface Hamiltonian2file
    module procedure Hamiltonian2file_r, Hamiltonian2file_c
  endinterface

  interface Hamiltonian2ASCII
    module procedure Hamiltonian2ASCII_r, Hamiltonian2ASCII_c
  endinterface
#endif

  contains

! end of head part
#endif


#if R1_C2 == 1
#define REAPLEX real
#define BLAS_GEMM dgemm
#else
#define REAPLEX complex
#define BLAS_GEMM zgemm
#endif

#if R1_C2 == 1
  !! subspace rotation:
  !!  + band parallelization
  !!  + ScaLAPACK solving the eigenvalue problem
  !!      using a subset of g%comm
  integer function SR_usememory_r( &
#else
  integer function SR_usememory_c( &
#endif
      g, vloc, jspin, atm, kp, energy, s, Ss, band_comm, band_ioff, residual, show_energy ) &
  result( ist )
  use configuration, only: WARNING, ERROR
  use type_grid, only: grid
  use type_atom, only: atom
  use type_kpoint, only: kpoint
  use operators, only: Hmt

  use ScaLAPACK, only: init_matrix
#ifdef SYMMETRIC
  use ScaLAPACK, only: set_entries => set_sym_entries
#else
#error 'no interface for non-symmetric'
#endif
  use ScaLAPACK, only: solve_matrix
  use ScaLAPACK, only: get_matrix

!   use communicators, only: band_comm ! should not be global in the future
!   use communicators, only: band_ioff ! should not be global in the future

  use MPIconst, only: Wtime, PREC, MPI_STATUS_SIZE, MPI_INTEGER
  use MPItools, only: MPInprocs, MPImyrank, MPIparallel ! MPI functions
  use MPItools, only: operator(.MPIsum.), operator(.MPImax.) ! MPI operators
  use MPItools, only: MPIallsum, MPIbarrier, MPIbcast0 ! MPI subroutines
cDBG  use MPItools, only: MPIbarrier ! MPI subroutines
#ifdef EXTENDED
cDBG  use toolbox, only: write_bmp_file
#endif
  implicit none
    ! parameter
    character(len=*), parameter     :: fun = ' SR(mem): '
    real, parameter                 :: MiB = 0.5**20 * 8 * R1_C2 ! 8Byte/real
    character(len=*), parameter     :: MiB_ = ' MiByte'
    integer, parameter              :: I_H=1, I_S=2
    ! arguments
    type(grid), intent(in)          :: g
    real, intent(in)                :: vloc(:,:,:,:)
    integer, intent(in)             :: jspin
    type(atom), intent(in)          :: atm(:)
    type(kpoint), intent(in)        :: kp
    real, intent(out)               :: energy(:)
    REAPLEX, intent(inout)          ::  s(1:,1:) ! (nxyzs,nbsk)   !!           wave functions
    REAPLEX, intent(inout)          :: Ss(1:,1:) ! (nxyzs,nbsk+1) !! temporary wave functions
    MPI_Comm, intent(in)            :: band_comm ! MPI band communicator
    integer, intent(in)             :: band_ioff ! band offset
    real, intent(inout), optional   :: residual(:)
    logical, intent(in), optional   :: show_energy

    ! local vars
    logical                         :: show_e = .false.
    REAPLEX, allocatable            :: HSs(:,:,:) ! aux. vectors for H|psi> and S|psi>

    REAPLEX, allocatable            :: Hm(:,:), Sm(:,:) ! full matrix in local storage for serial LAPACK
    real, allocatable               :: ene(:) ! full eigenvalue array

    REAPLEX, allocatable            :: HSm(:,:) ! temp storage for matrix elements

    integer                         :: Nb ! number of all bands
    integer                         :: io1, nb1 ! offset and number of locally stored bands
    integer                         :: io2, nb2!, mb2 ! offset and number of remotely stored bands
    integer                         :: nbm ! largest number of locally stored bands ! nb1 and nb2 may differ by 1
    integer                         :: nxyzs ! number of grid degrees of freedom
    integer                         :: Np, Ncyc, icyc, me ! for band parallelization
    REAPLEX, allocatable            :: st(:,:)
    integer                         :: isend(4), irecv(4)
#ifndef NOMPI
    integer                         :: istat(MPI_STATUS_SIZE), ierr, ip(-1:+1)
#else
    integer, parameter              :: k1 = 0
#endif
    REAPLEX, parameter              :: ONE = 1., ZERO = 0.
    REAPLEX                         :: dV

!     logical, parameter              :: temp_s_try = .TRUE.
!     integer                         :: temp_s_ist ! status
!     REAPLEX, allocatable            :: temp_s(:,:,:) ! (nxyzs,nbm,Np-1)
!     ! =============== temp_s =====================================
!     if( temp_s_try .and. Ncyc > 0 ) then
!       ! try to allocate as much memory as to fit the remotely stored wave functions to memory
! cDBG  if(o>0) write(o,'(3A,F0.3,9A)') sym, fun, ' try to allocate', nxyzs*nbm*(Np-1)*MiB, MiB_
!       allocate( temp_s(nxyzs,nbm,Np-1), stat=temp_s_ist )
!       if( temp_s_ist == 0 ) then
!         Ncyc =
!         if(o>0) write(o,'(3A,F0.3,9A)') sym, fun,' failed to allocate',nxyzs*(nbm*((Np-1)*MiB)),MiB_,' communicate twice.'
!       endif ! temp_s_ist /= 0
!     else  ! temp_s_try
!       temp_s_ist = -1 ! do not even try
!     endif ! temp_s_try
!     ! =============== temp_s =====================================
! 
!         ! =============== temp_s =====================================
!         ! store the remotetly stored wave functions in memory,
!         ! so we do not need to do this communication again
!         if( temp_s_ist == 0 ) temp_s(:,:,icyc) = Ss ! store in memory
!         ! =============== temp_s =====================================


    integer                         :: ne, ihs
    real                            :: t(0:9) ! times
    logical, save                   :: show_m = .true.
!     integer(kind=8)               :: mem
cDBG  integer                       :: nerr(1:2)
cDBG  integer                       :: nelem
      integer                       :: ib

    t(0) = Wtime()

cDBG  if(o>0) write(o,'(2A,I0,9A)') __FILE__, ':', __LINE__ , ' start subspace rotation'

    show_e = .false. ; if( present( show_energy ) ) show_e = show_energy

    dV = g%hvol * one
    nxyzs = size(s,1) ! number of grid degrees of freedom = #dof
    nb1   = size(s,2) ! local number of bands
    nb2   = 0 ! init  ! remote number of bands
    io1   = band_ioff ! offset of bands
    Nb    = nb1 .MPIsum. band_comm ! global number of all bands
    
    ist = max( 0, Nb ) ! init result
    if( Nb < 1 ) return ! error if Nb<0, success if Nb==0 (no bands)

    nbm   = nb1 .MPImax. band_comm ! max local number of bands

cDBG  if(o>0) write(o,'(3A,2(I0,A),Z8.8)') sym, fun, 'matrix dimension = ',nb,'  nbnd = ',nbm,'  band_comm = 0x',band_comm
cDBG  if( size(Ss,1) /= nxyzs ) stop 'SUB ppSR: dim #1 of SS must match NXYZS!'
cDBG  if( size(Ss,2) < nbm ) stop 'SUB ppSR: dim #2 of SS must be >= NBMax!'

    Np = MPInprocs( band_comm ) ! number of processes in band parallelization
    Ncyc = Np-1 ! 0:Np-1 full number of communication cycles
#ifdef SYMMETRIC
    Ncyc = Np/2 ! integer divide ! reduced number of communication cycles
#endif
    me = MPImyrank( band_comm ) ! rank of this process in band parallelization

#ifdef NaN_SEARCH
    if( any( s/= s) ) stop 'SUB ppSR NaN in  S on entry.'
    if( any(Ss/=Ss) ) stop 'SUB ppSR NaN in SS on entry.'
#endif


#ifdef _SHOW_MEMORY
    if(o>0) write(o,'(3A,I0)') 'print_memusage() line ', __FILE__ ,':', __LINE__
    call print_memusage()
#endif

!     if( show_m ) then ! show memory consumption
!       mem =       2*nxyzs*nbm ! HSs
!       mem = mem + 2*  nbm*nbm ! HSm
!       mem = mem + 2*nxyzs*nbm*min((Np-1),1) ! st
!       if(o>0) write(o,'(3A,F0.6,9A)') sym, fun, 'memory consumption ',mem*MiB,MiB_
!       show_m = .false. ! turn off display
!     endif ! show_m

    t(1) = Wtime()

    allocate( HSs(nxyzs,nb1,I_H:I_S), stat=ist ) ! auxiliary vectors for H*|spsi>, S*|spsi>
    if( ist /= 0 ) then
      if(o>0) write(o,'(4A,F0.6,9A)') sym, fun, ERROR, 'allocation of HSs failed: ',nxyzs*(nbm*2*MiB),MiB_
      ist = -9 ; return ! error
    endif
    HSs = 0. ! init
    
    ! compute the action of the Hamiltonian and overlap matrix onto all |spsi>
    call Hmt( g, vloc, jspin, atm, kp, ket=s(:,:), Hket=HSs(:,:,I_H), Sket=HSs(:,:,I_S) )

    ! allocate distributed arrays for ScaLAPACK
    ist = init_matrix( Nb, g%comm, Hm, Sm )
    if( ist /= 0 ) then
      if(o>0) write(o,'(9A)') sym, fun, ERROR, 'matrix initialization failed!'
      return ! error
    endif

    allocate( HSm(max(nb1,1),nbm), stat=ist )
    if( ist /= 0 ) then 
      if(o>0) write(o,'(4A,F0.6,9A)') sym, fun, ERROR,'allocation of HSm failed: ',nbm*(nbm*MiB),MiB_
      return ! error
    endif

#ifdef _SHOW_MEMORY
    if(o>0) write(o,'(3A,I0)') 'print_memusage() line ', __FILE__ ,':', __LINE__
    call print_memusage()
#endif

! !$omp parallel workshare
    HSm = 0. ! init
! !$omp end parallel workshare

!     if(o>0) write(o,'(9(A,I6))') sym, __LINE__ , ' shape HSm =',size(HSm,1),' |',size(HSm,2)

cDBG  nelem = 0 ! init number of elements that are stored locally
cDBG  if(o>0) write(o,'(3A,I0,A,F0.6,9A)') sym, fun, 'sendrecv  vol. ',nxyzs*nbm,' elements ',nxyzs*(nbm*MiB),MiB_
cDBG  if(o>0) write(o,'(3A,I0,A,F0.6,9A)') sym, fun, 'allreduce vol. ',2*nbm**2,' elements ',nbm*(nbm*2*MiB),MiB_


    t(2) = Wtime()

    isend(1:4) = (/io1,nb1,nxyzs,Nb/) ! offset, number, dof, number of all bands
    do icyc = 0, Ncyc ! full or reduced number of communication cycles
      if( icyc > 0 ) then

#ifndef NOMPI
! MPI part

        ! determine ranks to communicate with
        ip(-1) = modulo(me-icyc,Np) ! receive from process
        ip(+1) = modulo(me+icyc,Np) ! send    to   process
        ! hand shake
        call MPI_sendrecv( isend, 4, MPI_INTEGER, ip( 1), icyc, &
                           irecv, 4, MPI_INTEGER, ip(-1), icyc, band_comm, istat, ierr )
cDBG    if( irecv(3) /= nxyzs ) stop 'SUB band parallelization: received deviating NXYZS!'
cDBG    if( irecv(4) /= Nb    ) stop 'SUB band parallelization: received deviating NB!'
        io2 = irecv(1) ; nb2 = irecv(2)
        ! Ss = 0. ! init receiving array (obsolete)
#ifdef FULL_DEBUG
        if(o>0) write(o,'(A,9(A,I0))') __FILE__,':',__LINE__,'  rank ',my,' is about to receive bands #',io2+1,' through ',io2+nb2,' from rank ',ip(-1)
#endif
        ! communicate
        call MPI_sendrecv(  s(:,1:nb1), nxyzs*nb1, PREC( R1_C2 ), ip( 1), icyc, &
                           Ss(:,1:nb2), nxyzs*nb2, PREC( R1_C2 ), ip(-1), icyc, band_comm, istat, ierr )
#ifdef FULL_DEBUG
        if(o>0) write(o,'(2A,4(I0,A),9(" ",I0))') __FILE__,':',__LINE__,'  rank ',my,'  icyc ',icyc,' ierr ',ierr,' status:',istat
        call MPIbarrier( band_comm )
#endif

        if( ( Np > 1 ) .and. ( 2*Ncyc == Np ) .and. ( icyc == Ncyc ) .and. ( me >= Ncyc ) ) cycle ! if SYMMETRIC some blocks can be skipped

! end MPI part
#endif
        
      else  ! icyc > 0

        ! icyc == 0 ! diagonal w.r.t. band parallelization
        io2 = io1 ; nb2 = nb1 ! offset and number

! !$omp parallel workshare
        Ss(:,1:nb2) = s(:,1:nb1) ! copy
! !$omp end parallel workshare

      endif ! icyc > 0

      do ihs = I_H, I_S, I_S-I_H
        ! compute all scalar products <s|H|s> or <s|S|s>

        ! HSm(ib1,ib2) = sum_k conjg( HSs(k,ib1) ) * Ss(k,ib2) ! replaced with BLAS call
!         HSm(1:nb1,1:nb2) = matmul( cc(transpose( HSs(:,1:nb1,ihs) )), Ss(:,1:nb2) ) * dV
!         call ZGEMM (TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
        call BLAS_GEMM ('c','n',nb1,nb2,nxyzs,dV,HSs(1:,1:,ihs),size(HSs,1),Ss(1:,1:),size(Ss,1),ZERO,HSm(1:,1:),size(HSm,1))

        if( MPIparallel( g%comm ) ) &
          call MPIallsum( HSm, g%comm ) ! reduce matrix elements over all grid processes

        ! store on the distributed matrix grid
        selectcase( ihs )
        case( I_H ) ; ne = set_entries( HSm(1:nb1,1:nb2), (/io1,io2/), Hm )
        case( I_S ) ; ne = set_entries( HSm(1:nb1,1:nb2), (/io1,io2/), Sm )
cDBG    case default ; stop 'SUB: ihs must be either I_H or I_S!'
        endselect ! ihs

cDBG    nelem = nelem+ne ! count up locally stored matrix elements
      enddo ! ihs

    enddo ! icyc ! end loop over Np communication cycles

    deallocate( HSm, stat=ist ) ! auxiliary matrix HSm not needed any more, matrix elements are stored in Hm and Sm
    deallocate( HSs, stat=ist ) ! auxiliary vectors for H*|spsi> and S*|spsi> are not needed any more

cDBG  nelem = nelem .MPIsum. g%comm ! when the matrices are distributed over the space ranks, not every matrix entry gets accepted

    t(3) = Wtime()

    if( MPIparallel( band_comm ) ) then
      ! Allreduce the matrix elements over the band parallelization
      call MPIallsum( Hm, band_comm )
      call MPIallsum( Sm, band_comm )
cDBG  nelem = nelem .MPIsum. band_comm
    endif ! MPIparallel band_comm
cDBG  if( nelem /= 2*nb*nb .and. o>0) write(o,'(4A,9(I0,A))') sym, fun, WARNING(0), 'accepted ', nelem, ' entries, but nb^2 = ', nb*nb

#ifdef EXTENDED
! cDBG  call write_bmp_file( 'subspace', data=real(Hm), style='redblue' )
! cDBG  call write_bmp_file( 'subspace', data=real(Sm), style='redblue' )
#endif

    t(4) = Wtime()

    allocate( Ene(nb), stat=ist )
    if( ist /= 0 ) then
      if(o>0) write(o,'(4A,F0.6,9A)') sym, fun, ERROR, 'allocation of Ene failed: ',nb*MiB,MiB_ ! factor 2 too much for complex
      return ! error
    endif 

! !$omp parallel workshare
    Ene = 0. ! init
! !$omp end parallel workshare

    ist = solve_matrix( nb, g%comm, Hm, Sm, Ene ) ! solve ( Hm - Ene Sm ) Xm = 0 using ScaLAPACK, if possible (store Xm in Hm)
!    write(*,*) 'Im used here (solve_matrix)'
    if( ist /= 0 ) then
      if(o>0) write(o,'(4A,9(I0,A))') sym, fun, ERROR, 'diagonalization failed, status = ', ist
! #ifdef DEBUG
!     if( ist /= 0 ) then
!       do ib1 = 1, nb1 ; write(*,'(16F8.3)') Hm(:,ib1) ; enddo ; write(*,*) ''
!       do ib1 = 1, nb1 ; write(*,'(16F8.3)') Sm(:,ib1) ; enddo ; write(*,*) ''
!       stop 'mod_subspace DEBUG line 451 (failed)'
!     endif
! #endif
      return ! error
    endif 

    deallocate( Sm, stat=ist ) ! distributed overlap matrix is not needed any more, eigenvectors are stored in Hm

    t(5) = Wtime()

!     bs(ROW) = get_block_size(ROW) ; bs(COL) = get_block_size(COL)
!     allocate( zz(bs(ROW),bs(COL)), stat=ist )
!     if( ist /= 0 .and. o>0 ) write(o,'(3A,F0.3,9A)') sym, fun, 'allocation of zz failed, ',bs(ROW)*(bs(COL)*MiB),MiB_ ! factor 2 too much for complex
!     if( ist /= 0 ) return ! error
!     zz = 0. ; jb_memory = -1 ! init as no values in zz

    if( Np > 1 ) then
      allocate( st(nxyzs,nb1), stat=ist )
      if( ist /= 0 ) then
        if(o>0) write(o,'(4A,F0.6,9A)') sym, fun, ERROR, 'allocation of st failed: ',nxyzs*(nbm*MiB),MiB_
        return ! error
      endif
      st = 0.
      st(:,1:nb1) = s(:,1:nb1) ! local copy of the old wave functions (st is never overwritten)
    endif ! Np > 1

! !$omp parallel workshare
    Ss(:,1:nb1) = s(:,1:nb1) ! store a copy of the old wave functions in Ss (temporarily)

    ! create the new wave functions from the eigenvectors
    ! of the rotated subspace (1st time of writing to array s)
    s = ZERO ! init
! !$omp end parallel workshare

    allocate( HSm(nbm,nb1), stat=ist )
    if( ist /= 0 ) then
      if(o>0 ) write(o,'(4A,F0.6,9A)') sym, fun, ERROR, 'allocation of temp. HSm failed: ',nbm*(nbm*MiB),MiB_
      return ! error
    endif

! !$omp parallel workshare
    HSm = 0. ! init
! !$omp end parallel workshare


    isend(1:2) = (/io1,nb1/) ! offset, number
    do icyc = 0, Np-1 ! full number of communication cycles
      if( icyc > 0 ) then
#ifndef NOMPI
!+ MPI part

        ip(-1) = modulo(me-icyc,Np) ! receive from process
        ip(+1) = modulo(me+icyc,Np) ! send    to   process
        ! hand shake
        call MPI_sendrecv( isend, 2, MPI_INTEGER, ip( 1), icyc, &
                           irecv, 2, MPI_INTEGER, ip(-1), icyc, band_comm, istat, ierr )
        io2 = irecv(1) ; nb2 = irecv(2)
#ifdef FULL_DEBUG
        if(o>0) write(o,'(A,9(A,I0))') __FILE__,':',__LINE__,'  rank ',my,' is about to receive bands #',io2+1,' through ',io2+nb2,' from rank ',ip(-1)
#endif
        ! Ss = 0. ! init receiving array (redundant)
        ! communicate
        call MPI_sendrecv( st(:,1:nb1), nxyzs*nb1, PREC( R1_C2 ), ip( 1), icyc, &
                           Ss(:,1:nb2), nxyzs*nb2, PREC( R1_C2 ), ip(-1), icyc, band_comm, istat, ierr )
#ifdef FULL_DEBUG
        if(o>0) write(o,'(2A,4(I0,A),9(" ",I0))') __FILE__,':',__LINE__,'  rank ',my,'  icyc ',icyc,' ierr ',ierr,' status:',istat
        call MPIbarrier( band_comm )
#endif

!- MPI part
#endif
      else  ! icyc > 0
        ! no communication needed
        io2 = io1 ; nb2 = nb1 ! offset and number ! see below
        ! Ss(:,1:nb2) = st(:,1:nb1) ! local copy (not needed, because icyc==0 is the first cycle)
      endif ! icyc > 0

!       allocate( HSm(nb1,nbm), stat=ist )
!       ! get the eigenvector coefficients after diagonalization
!       do ib2 = 1, nb2 ; j(ROW) = ib2+io2
!         do ib1 = 1, nb1 ; j(COL) = ib1+io1
!           HSm(ib1,ib2) = get_matrix_block( Hm, j, g%comm, zz, jb_memory )
!         enddo ! ib1
!       enddo ! ib2
!       call BLAS_GEMM ('n','t',nxyzs,nb1,nb2,ONE,Ss,nxyzs,HSm,nb1,ONE,s,nxyzs)

      ist = get_matrix( Hm, (/io2,io1/), g%comm, HSm(1:nb2,1:nb1) ) ! load eigenvector coefficients from Hm into HSm
      ! new wave functions += eigenvectors * old wave functions
      ! s(ixyzs,ib1) = s(ixyzs,ib1) + sum_ib2 Ss(:,ib2) * HSm(ib2,ib1)
      call MPIbcast0(HSm, g%comm)
      call BLAS_GEMM ('n','n',nxyzs,nb1,nb2,ONE,Ss,size(Ss,1),HSm,size(HSm,1),ONE,s,size(s,1))

    enddo ! icyc

! !$omp parallel workshare
    energy(1:nb1) = Ene(1+io1:nb1+io1) ! new energy eigenvalues (of locally stored bands)
! !$omp end parallel workshare

    t(6) = Wtime()

    if( present( residual ) ) residual = 0.
 !show_e = .true.
 if( show_e .and. o>0 ) then
   do ib = 1, nb1
      write(o,'(3A,I6,I2,I6,A,F24.16,A,ES8.1E2,A)') sym, fun, 'band(', ib, jspin, kp%jk, '): energy =', energy(ib), ' Ha'
    enddo ! ib
  endif ! show_e

    t(9) = Wtime()
cTIM  if(o>0) write(o,'(3A,6(" ",F0.2),9A)') sym, fun, 'times[h+m+c+s+l=T]', t(2:6)-t(1:5), t(9)-t(0), ' sec'
    deallocate( st, HSs, Hm, Sm, ene, HSm, stat=ist )

#ifdef _SHOW_MEMORY
    if(o>0) write(o,'(3A,I0)') 'print_memusage() line ', __FILE__ ,':', __LINE__
    call print_memusage()
#endif

    ist = 0 ! result is success
  endfunction ! SR_usememory




#ifdef EXTENDED
!+ extended


#if R1_C2 == 1
  !! subspace rotation:
  !!  + band parallelization
  !!  + ScaLAPACK solving the eigenvalue problem
  !!      using a subset of g%comm
  integer function p_SR_r( &
#else
  integer function p_SR_c( &
#endif
      g, vloc, jspin, atm, kp, energy, s, Ss, band_comm, band_ioff, residual, show_energy ) &
  result( istatus )
  use configuration, only: WARNING, ERROR
  use type_grid, only: grid
  use type_atom, only: atom
  use type_kpoint, only: kpoint
  use operators, only: scalar_product, Hmt

  use ScaLAPACK, only: ROW, COL
  use ScaLAPACK, only: BLOCK_SIZE
#ifdef SYMMETRIC
  use ScaLAPACK, only: set_sym_matrix_entry
#else
  use ScaLAPACK, only: set_matrix_entry
#endif
  use ScaLAPACK, only: init_matrix
  use ScaLAPACK, only: solve_matrix
  use ScaLAPACK, only: get_matrix_block

!   use communicators, only: band_comm ! should not be global in the future
!   use communicators, only: band_ioff ! should not be global in the future

  use MPIconst, only: Wtime, PREC, MPI_STATUS_SIZE, MPI_INTEGER
  use MPItools, only: MPInprocs, MPImyrank, MPIparallel ! MPI functions
  use MPItools, only: operator(.MPIsum.), operator(.MPImax.) ! MPI operators
  use MPItools, only: MPIbarrier, MPIallsum ! MPI subroutines
#ifdef FULL_DEBUG
  use MPItools, only: operator(.MPIdiff.)
#endif
  implicit none
    ! parameter
    character(len=*), parameter     :: fun = ' SR: '
    real, parameter                 :: MiB = 0.5**20 * 8 * R1_C2 ! 8Byte/real
    character(len=*), parameter     :: MiB_ = ' MiByte'
    ! arguments
    type(grid), intent(in)          :: g
    real, intent(in)                :: vloc(:,:,:,:)
    integer, intent(in)             :: jspin
    type(atom), intent(in)          :: atm(:)
    type(kpoint), intent(in)        :: kp
    real, intent(out)               :: energy(:)
    REAPLEX, intent(inout)          ::  s(1:,1:) ! (nxyzs,nbsk)   !!           wave functions
    REAPLEX, intent(inout)          :: Ss(1:,1:) ! (nxyzs,nbsk+1) !! temporary wave functions
    integer, intent(in)             :: band_comm
    integer, intent(in)             :: band_ioff
    real, intent(inout), optional   :: residual(:)
    logical, intent(in), optional   :: show_energy

    ! local vars
    logical                         :: show_e = .false.
    REAPLEX, allocatable            :: HSs(:,:) ! aux. vectors for H|psi> and S|psi>
    REAPLEX                         :: cc

    REAPLEX, allocatable            :: zz(:,:)
    integer                         :: jb_memory(1:2)
    REAPLEX, allocatable            :: Hm(:,:), Sm(:,:) ! full matrix in local storage for serial LAPACK
    real, allocatable               :: ene(:) ! full eigenvalue array

    REAPLEX, allocatable            :: HSm(:,:,:) ! temp storage for matrix elements

    integer                         :: Nb ! number of all bands
    integer                         :: io1, nb1 ! offset and number of locally stored bands
    integer                         :: io2, nb2, mb2 ! offset and number of remotely stored bands
    integer                         :: nbm ! largest number of locally stored bands ! nb1 and nb2 may differ by 1
    integer                         :: nxyzs ! number of grid degrees of freedom
    integer                         :: ib1, ib2 ! local band indices
    integer                         :: iHb1, iSb1
    integer                         :: j(1:2) ! global band indices
    integer                         :: Np, Nph, icyc, my ! for band parallelization
#ifndef NOMPI
    REAPLEX, allocatable            :: st(:,:)
    integer                         :: isend(4), irecv(4), istat(MPI_STATUS_SIZE), ierr, ip(-1:+1)
#else
    integer, parameter              :: k1 = 0
#endif
    integer                         :: ist, ne ! status for allocations
    real                            :: t(0:9) ! times
cDBG  integer                     :: nerr(1:2)
cDBG  real                        :: time_comm, time_asm1, time_asm0
cDBG  integer                     :: nact_comm, nact_asm1, nact_asm0, nact_Hops, nact_sprd
cDBG  integer                     :: nelem
    logical, save                   :: show_m = .true.
    integer(kind=8)                 :: mem
    t(0) = Wtime()

cDBG  if(o>0) write(o,'(2A,I6,9A)') __FILE__ ,' line#', __LINE__ , ' start subspace rotation'

    show_e = .false. ; if( present( show_energy ) ) show_e = show_energy

    istatus = -1 ! init result as error for early return

    nxyzs = size(s,1) ! number of grid degrees of freedom = #dof
    nb1   = size(s,2) ! local number of bands
    io1   = band_ioff ! offset of bands
    Nb    = nb1 .MPIsum. band_comm ! global number of all bands
    nbm   = nb1 .MPImax. band_comm ! max local number of bands
cDBG  if(o>0) write(o,'(3A,I6,A,I4,A,Z8.8)') sym, fun, 'matrix dimension =', nb, '  nbnd =', nb1, '  band_comm = 0x', band_comm
cDBG  if( size(Ss,1) /= nxyzs ) stop 'SUB ppSR: dim #1 of SS must match NXYZS!'
cDBG  if( size(Ss,2) < nbm ) stop 'SUB ppSR: dim #2 of SS must be >= NBmax!'

    Np = MPInprocs( band_comm ) ; Nph = Np/2 ! number of processes in band parallelization
    my = MPImyrank( band_comm ) ! rank of this process in band parallelization

#ifdef NaN_SEARCH
    if( any( s/= s) ) stop 'SUB ppSR NaN in  S on entry.'
    if( any(Ss/=Ss) ) stop 'SUB ppSR NaN in SS on entry.'
#endif

#ifdef FULL_DEBUG
!     if( reshape( vloc, (/size(vloc)/) ) .MPIdiff. band_comm ) stop 'SUB SR: local potential differs along band_comm'
!     if(o>0) write(o,*) vloc
!     stop
#endif

    ib1 = nb1 ! set index to maximum

#if IHS == 2
    !=================================================================================
    ! H|spsi> and S|spsi> needs to be recomputed every time
    !=================================================================================
#else
    !=================================================================================
    ! all H|spsi> and S|spsi> are stored
    !=================================================================================
#endif

    if( show_m ) then ! show memory consumption
      mem =       nxyzs * IHS ! HSs
      mem = mem + min((Np-1),1)*2*nbm* IND ! HSm
      mem = mem + min((Np-1),1)*2*nbm*nxyzs ! st
      if(o>0) write(o,'(3A,F16.6,9A)') sym, fun, 'memory consumption', mem*MiB, MiB_
      show_m = .false. ! turn off display
    endif ! show_m

    allocate( HSs(nxyzs, IHS ), stat=ist ) ! auxiliary vectors for H*|spsi>, S*|spsi>
    if( ist /= 0 ) stop 'SUB SR: failed to allocate HSs'
    HSs = 0. ! init

    ! allocate distributed arrays in the module ScaLAPACK
    ist = init_matrix( Nb, g%comm, Hm, Sm )


cDBG  nelem = 0 ! init number of elements that are stored locally
cDBG  if(o>0) write(o,'(3A,I12,A,F16.6,9A)') sym, fun, 'sendrecv  vol.', nxyzs*nbm, ' elements', nxyzs*nbm*MiB, MiB_
cDBG  if(o>0) write(o,'(3A,I12,A,F16.6,9A)') sym, fun, 'allreduce vol.', nbm * IND *2,' elements', nbm * IND *2*MiB, MiB_

cDBG  time_comm = 0. ; time_asm0 = 0. ; time_asm1 = 0.
cDBG  nact_comm = 0  ; nact_asm0 = 0  ; nact_asm1 = 0.
cDBG  nact_Hops = 0  ; nact_sprd = 0

    t(1) = Wtime()


#ifndef NOMPI
    isend(1:4) = (/io1,nb1,nxyzs,Nb/) ! offset, number, dof, number of all bands

#ifdef SYMMETRIC
    do icyc = 0, Nph  ! reduced number of communication cycles
#else
    do icyc = 0, Np-1 ! full number of communication cycles
#endif
      if( icyc > 0 ) then

        ip(-1) = modulo(my-icyc,Np) ! receive from process
        ip(+1) = modulo(my+icyc,Np) ! send    to   process
        ! hand shake
        call MPI_sendrecv( isend, 4, MPI_INTEGER, ip(+1), icyc, &
                           irecv, 4, MPI_INTEGER, ip(-1), icyc, band_comm, istat, ierr )
cDBG    if( irecv(3) /= nxyzs ) stop 'SUB band parallelization: received deviating NXYZS!'
cDBG    if( irecv(4) /= Nb    ) stop 'SUB band parallelization: received deviating NB!'
        io2 = irecv(1) ; nb2 = irecv(2)
        Ss = 0. ! init receiving array
cDBG    time_comm = time_comm - Wtime()
#ifdef FULL_DEBUG
        if(o>0) write(o,'(3A,9(I6,A))') sym, fun, 'rank',my,' is about to receive bands#',io2+1,' through',io2+nb2,' from rank',ip(-1)
#endif
        ! communicate
        call MPI_sendrecv(  s(:,1:nb1), nxyzs*nb1, PREC( R1_C2 ), ip(+1), icyc, &
                           Ss(:,1:nb2), nxyzs*nb2, PREC( R1_C2 ), ip(-1), icyc, band_comm, istat, ierr )
#ifdef FULL_DEBUG
        if(o>0) write(o,'(3A,2(I6,A),9I8)') sym, fun, 'rank',my,'  icyc=',icyc,'  status=',istat
        call MPIbarrier( band_comm )
#endif
cDBG    time_comm = time_comm + Wtime()
cDBG    nact_comm = nact_comm + 1


#ifdef SYMMETRIC
        if( ( Np > 1 ) .and. ( 2*Nph == Np ) .and. ( icyc == Nph ) .and. ( my >= Nph ) ) cycle
#endif

        ib1 = nbm
        allocate( HSm(2,nb2, IND ), stat=ist ) ; HSm = 0. ! init

        do ib1 = 1, nb1 ; j(COL) = ib1+io1

#ifdef SAVE_MEMORY
          ! recompute the Hamiltonian
          call Hmt( g, vloc, jspin, atm, kp, ket=s(:,ib1), Hket=HSs(:, IHS : IHS ), Sket=HSs(:, IHS -1: IHS -1) )
cDBG      nact_Hops = nact_Hops + 1
#endif

          do ib2 = 1, nb2 !; j(ROW) = ib2+io2
            ! set up Hamiltonian and Overlap matrix elements
            HSm(1,ib2, IND ) = scalar_product( Ss(:,ib2), HSs(:, IHS   ), h3=g%hvol ) ! here, communication is collected
            HSm(2,ib2, IND ) = scalar_product( Ss(:,ib2), HSs(:, IHS -1), h3=g%hvol ) ! and here, too!
cDBG        nact_sprd = nact_sprd + 2
          enddo ! ib2

#if IND == ib1
        enddo ! ib1 ! stop ib1 loop
#endif

          ! collected communication
cDBG      time_asm1 = time_asm1 - Wtime()
          call MPIallsum( HSm, g%comm ) ! reduces over all grid processes
cDBG      time_asm1 = time_asm1 + Wtime()
cDBG      nact_asm1 = nact_asm1 + 1

#if IND == ib1
        do ib1 = 1, nb1 ; j(COL) = ib1+io1 ! restart ib1 loop
#endif
          do ib2 = 1, nb2 ; j(ROW) = ib2+io2
            ! store on the distributed matrix grid
#ifdef SYMMETRIC
            ne = set_sym_matrix_entry( HSm(1,ib2, IND ), HSm(2,ib2, IND ), j, Hm, Sm )
#else
            ne = set_matrix_entry( HSm(1,ib2, IND ), HSm(2,ib2, IND ), j, Hm, Sm )
#endif
cDBG        nelem = nelem+ne ! count up locally stored matrix elements
          enddo ! ib2
        enddo ! ib1
        deallocate( HSm, stat=ist )

#else
! No MPI
    if( nb1 < Nb ) stop 'SUB no parallelization of bands without MPI.'
    do icyc = 0, 0 ! one time
      if( icyc > 0 ) then
#endif

      else  ! icyc > 0
        ! icyc == 0 ! diagonal w.r.t. band parallelization
        io2 = io1 ; nb2 = nb1 ! offset and number

        allocate( HSm(2,nb1,1), stat=ist ) ; HSm = 0. ! init

        HSm(:,:,:) = 0. ! init
        do ib1 = 1, nb1 ; j(COL) = ib1+io1
          ! apply the two operators, Hamiltonian and Overlap matrix
          call Hmt( g, vloc, jspin, atm, kp, ket=s(:,ib1:ib1), Hket=HSs(:, IHS : IHS ), Sket=HSs(:, IHS -1: IHS -1) )
cDBG      nact_Hops = nact_Hops + 1

#ifdef SYMMETRIC
          mb2 = ib1
#else
          mb2 = nb2
#endif
          do ib2 = 1, mb2 !; j(ROW) = ib2+io2
            ! set up Hamiltonian and Overlap matrix elements
            HSm(1,ib2,1) = scalar_product( s(:,ib2), HSs(:, IHS   ), h3=g%hvol ) ! here, communication is collected
            HSm(2,ib2,1) = scalar_product( s(:,ib2), HSs(:, IHS -1), h3=g%hvol ) ! and here, too!
cDBG        nact_sprd = nact_sprd + 2
          enddo ! ib2

!           ! for this alternative, we need to turn the matrix entries around ! allocate( HSm(nb1,2,1), stat=ist ) ; HSm = 0. ! init
!           HSm(1:mb2,1:2,1) = scalar_product( s(:,1:mb2), HSs(:, IOF1 +1: IOF1 +2), h3=g%hvol ) ! here, communication comes later
! cDBG      nact_sprd = nact_sprd + 2*mb2


          ! collected communication
cDBG      time_asm0 = time_asm0 - Wtime()
          call MPIallsum( HSm(:,1:mb2,1), g%comm ) ! reduces over all grid processes
cDBG      time_asm0 = time_asm0 + Wtime()
cDBG      nact_asm0 = nact_asm0 + 1

          do ib2 = 1, mb2 ; j(ROW) = ib2+io2 ! [triangular]
            ! store on the distributed matrix grid
#ifdef SYMMETRIC
            ne = set_sym_matrix_entry( HSm(1,ib2,1), HSm(2,ib2,1), j, Hm, Sm )
#else
            ne = set_matrix_entry( HSm(1,ib2,1), HSm(2,ib2,1), j, Hm, Sm )
#endif
cDBG        nelem = nelem+ne ! count up locally stored matrix elements
          enddo ! ib2

        enddo ! ib1
        deallocate( HSm, stat=ist )

      endif ! icyc > 0

    enddo ! icyc ! end loop over Np communication cycles

cDBG  if(o>0 .and. nact_comm>0) write(o,'(3A,F10.3,A,I9,9A)') sym, fun, 'sendrecv  took', time_comm, ' sec for', nact_comm, ' ops'
cDBG  if(o>0 .and. nact_asm0>0) write(o,'(3A,F10.3,A,I9,9A)') sym, fun, 'allreduce took', time_asm0, ' sec for', nact_asm0, ' ops'
cDBG  if(o>0 .and. nact_asm1>0) write(o,'(3A,F10.3,A,I9,9A)') sym, fun, 'allreduce took', time_asm1, ' sec for', nact_asm1, ' ops'
cDBG  if(o>0 .and. nact_Hops>0) write(o,'(3A,I9,9A)') sym, fun, 'Hamiltonian   was called', nact_Hops, ' times'
cDBG  if(o>0 .and. nact_sprd>0) write(o,'(3A,I9,9A)') sym, fun, 'inner product was called', nact_sprd, ' times'

cDBG  nelem = nelem .MPIsum. g%comm ! when the matrices are distributed over the space ranks, not every matrix entry gets accepted

    t(2) = Wtime()

    deallocate( HSs, stat=ist ) ! auxiliary vectors for H*|spsi> and S*|spsi> are not needed any more

    if( MPIparallel( band_comm ) ) then
      call MPIallsum( Hm, band_comm )
      call MPIallsum( Sm, band_comm )
cDBG  nelem = nelem .MPIsum. band_comm
    endif ! MPIparallel band_comm
cDBG  if( nelem /= nb*nb .and. o>0) write(o,'(4A,9(I12,A))') sym, fun, WARNING(0), 'accepted', nelem, ' entries, but nb^2=', nb*nb

    t(3) = Wtime()

    allocate( Ene(nb), stat=ist ) ; Ene = 0.

    istatus = solve_matrix( nb, g%comm, Hm, Sm, Ene ) ! solve using ScaLAPACK, if possible

    deallocate( Sm, stat=ist )
    if( istatus /= 0 ) then
      if(o>0) write(o,'(3A,I3,9A)') sym, fun, 'diagonalization failed, status=', istatus
      return
    endif ! no SUCCESS


    t(4) = Wtime()

    allocate( zz(BLOCK_SIZE(ROW),BLOCK_SIZE(COL)), stat=ist) ; zz = 0. ; jb_memory = -1 ! init as no values in zz

#ifndef NOMPI
! MPI part
    if( Np > 1 ) then
      allocate( st(nxyzs,nb1), stat=ist ) ; st = 0.
      if( ist /= 0 ) then
        write(*,'(4A,F12.3,9A)') sym, fun, ERROR, 'allocation of temp. wfs failed,', nxyzs*nb1*0.5**17 * R1_C2 ,' MiByte'
        stop 'SUB allocation of temp array ST failed!'
      endif
      st(:,1:nb1) = s(:,1:nb1) ! local copy (st is never overwritten)
    endif ! Np > 1
#endif

    ! store a copy of the old wave functions in Ss (temporarily)
    Ss(:,1:nb1) = s(:,1:nb1)

    ! create the new wave functions from the eigenvectors
    ! of the rotated subspace (1st time of writing to array s)
    s = 0. ! init

#ifndef NOMPI
! MPI part

    isend(1:2) = (/io1,nb1/) ! offset, number
    do icyc = 0, Np-1 ! communication cycles
      if( icyc > 0 ) then
        ip(-1) = modulo(my-icyc,Np) ! receive from process
        ip(+1) = modulo(my+icyc,Np) ! send    to   process
        ! hand shake
        call MPI_sendrecv( isend, 2, MPI_INTEGER, ip(+1), icyc, &
                           irecv, 2, MPI_INTEGER, ip(-1), icyc, band_comm, istat, ierr )
        io2 = irecv(1) ; nb2 = irecv(2)
! #ifdef FULL_DEBUG
!         if(o>0) write(o,'(3A,9(I6,A))') sym, fun, 'rank',my,' is about to receive bands#',io2+1,' through',io2+nb2,' from rank',ip(-1)
! #endif
        Ss = 0. ! init receiving array (redundant)
        ! communicate
        call MPI_sendrecv( st(:,1:nb1), nxyzs*nb1, PREC( R1_C2 ), ip(+1), icyc, &
                           Ss(:,1:nb2), nxyzs*nb2, PREC( R1_C2 ), ip(-1), icyc, band_comm, istat, ierr )
! #ifdef FULL_DEBUG
!         if(o>0) write(o,'(3A,2(I6,A),9I8)') sym, fun, 'rank',my,'  icyc=',icyc,'  status=',istat
!         call MPIbarrier( band_comm )
! #endif
      else  ! icyc > 0
        ! no communication needed
        io2 = io1 ; nb2 = nb1 ! offset and number ! see below
!         Ss(:,1:nb2) = st(:,1:nb1) ! local copy (not needed, because icyc==0 is the first cycle)
      endif ! icyc > 0
! end MPI part
#else
! No MPI
    if( nb1 < nb ) stop 'SUB no parallelization of bands without MPI.'
    do icyc = 0, 0 ! one time
      io2 = io1 ; nb2 = nb1 ! offset and number
#endif

      do ib1 = 1, nb1 ; j(COL) = ib1+io1
        do ib2 = 1, nb2 ; j(ROW) = ib2+io2
          ! new wave functions = eigenvectors * old wave functions
          ! cc are the eigenvector coefficients after diagonalization
          cc = get_matrix_block( Hm, j, g%comm, zz, jb_memory )

          s(:,ib1) = s(:,ib1) + cc * Ss(:,ib2)

        enddo ! ib2
      enddo ! ib1

    enddo ! icyc
    energy(1:nb1) = Ene(1+io1:nb1+io1) ! new energy eigenvalues (of locally stored bands)

    t(5) = Wtime()

    istatus = 0 ! success

    if( present( residual ) ) residual = 0.

cDBG  if( show_e .and. o>0 ) then
cDBG    do ib1 = 1, nb1
cDBG      write(o,'(3A,I6,I2,I6,A,F24.16,A,ES8.1E2,A)') sym, fun, 'band(', ib1, jspin, kp%jk, '): energy =', energy(ib1), ' Ha'
cDBG    enddo ! ib1
cDBG  endif ! show_e

    t(9) = Wtime()
cTIM  if(o>0) write(o,'(3A,5F10.3,9A)') sym, fun, 'times[set,com,sol,lin,tot]', t(2:5)-t(1:4), t(9)-t(0), ' sec'
  endfunction ! p_SR





#if R1_C2 == 1
  !! subspace rotation:
  !!  + band parallelization
  !!  + ScaLAPACK solving the eigenvalue problem
  !!      using a subset of g%comm
  integer function pp_SR_r( &
#else
  integer function pp_SR_c( &
#endif
      g, vloc, jspin, atm, kp, energy, s, Ss, band_comm, band_ioff, residual, show_energy ) &
  result( istatus )
  use configuration, only: WARNING, ERROR
  use type_grid, only: grid
  use type_atom, only: atom
  use type_kpoint, only: kpoint
  use operators, only: scalar_product, Hmt

  use ScaLAPACK, only: ROW, COL
  use ScaLAPACK, only: get_block_size
#ifdef SYMMETRIC
  use ScaLAPACK, only: set_sym_matrix_entry
#else
  use ScaLAPACK, only: set_matrix_entry
#endif
  use ScaLAPACK, only: init_matrix
  use ScaLAPACK, only: solve_matrix
  use ScaLAPACK, only: get_matrix_block

!   use communicators, only: band_comm ! should not be global in the future
!   use communicators, only: band_ioff ! should not be global in the future

  use MPIconst, only: Wtime, PREC, MPI_STATUS_SIZE, MPI_INTEGER
  use MPItools, only: MPInprocs, MPImyrank, MPIparallel ! MPI functions
  use MPItools, only: operator(.MPIsum.), operator(.MPImax.) ! MPI operators
  use MPItools, only: MPIbarrier, MPIallsum ! MPI subroutines
#ifdef FULL_DEBUG
  use MPItools, only: operator(.MPIdiff.)
#endif
  implicit none
    ! parameter
    character(len=*), parameter     :: fun = ' SR: '
    real, parameter                 :: MiB = 0.5**20
    character(len=*), parameter     :: MiB_ = ' MiByte'
    ! arguments
    type(grid), intent(in)          :: g
    real, intent(in)                :: vloc(:,:,:,:)
    integer, intent(in)             :: jspin
    type(atom), intent(in)          :: atm(:)
    type(kpoint), intent(in)        :: kp
    real, intent(out)               :: energy(:)
    REAPLEX, intent(inout)          ::  s(1:,1:) ! (nxyzs,nbsk)   !!           wave functions
    REAPLEX, intent(inout)          :: Ss(1:,1:) ! (nxyzs,nbsk+1) !! temporary wave functions
    integer, intent(in)             :: band_comm
    integer, intent(in)             :: band_ioff
    real, intent(inout), optional   :: residual(:)
    logical, intent(in), optional   :: show_energy

    ! local vars
    logical                         :: show_e = .false.
    REAPLEX, allocatable            :: HSs(:,:) ! aux. vectors for H|psi> and S|psi>
    REAPLEX                         :: cc

    REAPLEX, allocatable            :: zz(:,:)
    integer                         :: jb_memory(1:2)
    REAPLEX, allocatable            :: Hm(:,:), Sm(:,:) ! full matrix in local storage for serial LAPACK
    real, allocatable               :: ene(:) ! full eigenvalue array

    REAPLEX, allocatable            :: HSm(:,:,:) ! temp storage for matrix elements

    integer                         :: nb ! number of all bands
    integer                         :: io1, nb1 ! offset and number of locally stored bands
    integer                         :: io2, nb2, mb2 ! offset and number of remotely stored bands
    integer                         :: nbm ! largest number of locally stored bands ! nb1 and nb2 may differ by 1
    integer                         :: nxyzs ! number of grid degrees of freedom
    integer                         :: ib1, ib2 ! local band indices
    integer                         :: iHb1, iSb1
    integer                         :: j(1:2) ! global band indices
    integer                         :: Np, Nph, icyc, my ! for band parallelization
#ifndef NOMPI
    REAPLEX, allocatable            :: st(:,:)
    integer                         :: isend(4), irecv(4), istat(MPI_STATUS_SIZE), ierr, ip(-1:+1), k1
#else
    integer, parameter              :: k1 = 0
#endif
    integer                         :: ist, ne ! status for allocations
    real                            :: t(0:9) ! times
cDBG  integer                     :: nerr(1:2)
cDBG  real                        :: time_comm, time_asum
cDBG  integer                     :: nact_comm, nact_asum, nact_Hops, nact_sprd
cDBG  integer                     :: nelem_loc, nelem, nelem_all
    t(0) = Wtime()

    show_e = .false. ; if( present( show_energy ) ) show_e = show_energy

    istatus = -1 ! init result as error for early return

    nxyzs = size(s,1) ! number of grid degrees of freedom = #dof
    nb1   = size(s,2) ! local number of bands
    io1   = band_ioff ! offset of bands
    nb    = nb1 .MPIsum. band_comm ! global number of all bands
    nbm   = nb1 .MPImax. band_comm ! max local number of bands
cDBG  if(o>0) write(o,'(3A,I6,A,I4,A,Z8.8)') sym, fun, 'matrix dimension =', nb, '  nbnd =', nb1, '  band_comm = 0x', band_comm
cDBG  if( size(Ss,1) /= nxyzs ) stop 'SUB ppSR: dim #1 of SS must match NXYZS!'
cDBG  if( size(Ss,2) < nbm ) stop 'SUB ppSR: dim #2 of SS must be >= NBmax!'

    Np = MPInprocs( band_comm ) ; Nph = Np/2 ! number of processes in band parallelization
    my = MPImyrank( band_comm ) ! rank of this process in band parallelization

#ifdef NaN_SEARCH
    if( any( s/= s) ) stop 'SUB ppSR NaN in  S on entry.'
    if( any(Ss/=Ss) ) stop 'SUB ppSR NaN in SS on entry.'
#endif

#ifdef FULL_DEBUG
!     if( reshape( vloc, (/size(vloc)/) ) .MPIdiff. band_comm ) stop 'SUB SR: local potential differs along band_comm'
!     if(o>0) write(o,*) vloc
!     stop
#endif

#ifdef SAVE_MEMORY
    allocate( HSs(nxyzs,2), stat=ist ) ! auxiliary vectors for H*|spsi>, S*|spsi>
#else
    allocate( HSs(nxyzs,2*nb1), stat=ist ) ! vectors for H*|spsi>, S*|spsi> for each band
#endif
    if( ist /= 0 ) stop 'SUB SR: failed to allocate HSs'
    ! these arrays are rank 2, because one could think of storing
    ! the H* and S*local set of wf, instead of computing it again and again
    HSs = 0. ! init

    ! allocate distributed arrays in the module ScaLAPACK
    ist = init_matrix( nb, g%comm, Hm, Sm )


    ib1 = nbm
    allocate( HSm(2,nbm, IND ), stat=ist ) ; HSm = 0. ! init

cDBG  nelem = 0 ! init number of elements that are stored locally
cDBG  if(o>0) write(o,'(3A,I12,A,F16.6,9A)') sym, fun, 'sendrecv  vol.', nxyzs*nbm, ' elements', nxyzs*nbm* R1_C2 *8*MiB, MiB_
cDBG  if(o>0) write(o,'(3A,I12,A,F16.6,9A)') sym, fun, 'allreduce vol.', nbm * IND *2,' elements', nbm * IND * R1_C2 *2*8*MiB, MiB_

cDBG  time_comm = 0.
cDBG  time_asum = 0.
cDBG  nact_comm = 0
cDBG  nact_asum = 0
cDBG  nact_Hops = 0
cDBG  nact_sprd = 0

    t(1) = Wtime()

#ifndef NOMPI
    isend(1:4) = (/io1,nb1,nxyzs,nb/) ! offset, number, dof, number of all bands

#ifdef SYMMETRIC
#define mb2 ib1
    do icyc = 0, Nph  ! reduced number of communication cycles
#else
#define mb2 nb2
    do icyc = 0, Np-1 ! full number of communication cycles
#endif
      ip(-1) = modulo(my-icyc,Np) ! receive from process
      ip(+1) = modulo(my+icyc,Np) ! send    to   process
      k1 = 0 ; if( my < ip(-1) ) k1 = 1
      if( icyc > 0 ) then
        ! hand shake
        call MPI_sendrecv( isend, 4, MPI_INTEGER, ip(+1), icyc, &
                           irecv, 4, MPI_INTEGER, ip(-1), icyc, band_comm, istat, ierr )
cDBG    if( irecv(3) /= nxyzs ) stop 'SUB band parallelization: received deviating NXYZS!'
cDBG    if( irecv(4) /= nb    ) stop 'SUB band parallelization: received deviating NB!'
        io2 = irecv(1) ; nb2 = irecv(2)
        Ss = 0. ! init receiving array
cDBG    time_comm = time_comm - Wtime()
#ifdef FULL_DEBUG
        if(o>0) write(o,'(3A,9(I6,A))') sym, fun, 'rank',my,' is about to receive bands#',io2+1,' through',io2+nb2,' from rank',ip(-1)
#endif
        ! communicate
        call MPI_sendrecv(  s(:,1:nb1), nxyzs*nb1, PREC( R1_C2 ), ip(+1), icyc, &
                           Ss(:,1:nb2), nxyzs*nb2, PREC( R1_C2 ), ip(-1), icyc, band_comm, istat, ierr )
#ifdef FULL_DEBUG
        if(o>0) write(o,'(3A,2(I6,A),9I8)') sym, fun, 'rank',my,'  icyc=',icyc,'  status=',istat
        call MPIbarrier( band_comm )
#endif
cDBG    time_comm = time_comm + Wtime()
cDBG    nact_comm = nact_comm + 1
      else  ! icyc > 0
#else
! No MPI
    if( nb1 < nb ) stop 'SUB no parallelization of bands without MPI.'
    do icyc = 0, 0 ! one time
      if( .TRUE. ) then
#endif
        io2 = io1 ; nb2 = nb1 ! offset and number
        Ss(:,1:nb2) = s(:,1:nb1) ! local copy
      endif ! icyc > 0

    if( icyc == 0 ) then

      HSm(:,:,:) = 0. ! init
      do ib1 = 1, nb1 ; j(COL) = ib1+io1

        ! apply the two operators, Hamiltonian and Overlap matrix
#ifdef SAVE_MEMORY
#define iHb1 1
#define iSb1 2
#else
#define iHb1 2*(ib1-1)+1
#define iSb1 2*(ib1-1)+2
#endif
        call Hmt( g, vloc, jspin, atm, kp, ket=s(:,ib1:ib1), Hket=HSs(:, iHb1 : iHb1 ), Sket=HSs(:, iSb1 : iSb1 ) )
cDBG    nact_Hops = nact_Hops + 1

        do ib2 = 1, mb2 ; j(ROW) = ib2+io2
          ! set up Hamiltonian and Overlap matrix elements
          HSm(1,ib2, IND ) = scalar_product( Ss(:,ib2), HSs(:, iHb1 ), h3=g%hvol ) ! here, communication is collected
          HSm(2,ib2, IND ) = scalar_product( Ss(:,ib2), HSs(:, iSb1 ), h3=g%hvol ) ! and here, too!
cDBG      nact_sprd = nact_sprd + 2

        enddo ! ib2
#if IND == ib1
      enddo ! ib1 ! stop ib1 loop
#endif

      ! collected communication
cDBG  time_asum = time_asum - Wtime()
      call MPIallsum( HSm, g%comm ) ! reduces over all grid processes
cDBG  time_asum = time_asum + Wtime()
cDBG  nact_asum = nact_asum + 1

#if IND == ib1
      do ib1 = 1, nb1 ; j(COL) = ib1+io1 ! restart ib1 loop
#endif
        do ib2 = 1, mb2 ; j(ROW) = ib2+io2 ! [triangular]

          ! store on the distributed matrix grid
#ifdef SYMMETRIC
          ne = set_sym_matrix_entry( HSm(1,ib2, IND ), HSm(2,ib2, IND ), j, Hm, Sm )
#else
          ne = set_matrix_entry( HSm(1,ib2, IND ), HSm(2,ib2, IND ), j, Hm, Sm )
#endif

cDBG      nelem = nelem+ne ! count up locally stored matrix elements
        enddo ! ib2
      enddo ! ib1

    else  ! icyc == 0

#ifdef SYMMETRIC
      if( ( Np > 1 ) .and. ( 2*Nph == Np ) .and. ( icyc == Nph ) .and. ( my >= Nph ) ) cycle
#endif

      HSm(:,:,:) = 0. ! init off diagonal elements
      do ib1 = 1, nb1 ; j(COL) = ib1+io1

#ifdef SAVE_MEMORY
        ! recompute the Hamiltonian
        call Hmt( g, vloc, jspin, atm, kp, ket=s(:,ib1), Hket=HSs(:, iHb1 ), Sket=HSs(:, iSb1 ) )
cDBG    nact_Hops = nact_Hops + 1
#endif
        do ib2 = 1, nb2 !; j(ROW) = ib2+io2
          ! set up Hamiltonian and Overlap matrix elements
          HSm(1,ib2, IND ) = scalar_product( Ss(:,ib2), HSs(:, iHb1 ), h3=g%hvol ) ! here, communication is collected
          HSm(2,ib2, IND ) = scalar_product( Ss(:,ib2), HSs(:, iSb1 ), h3=g%hvol ) ! and here, too!
cDBG      nact_sprd = nact_sprd + 2
        enddo ! ib2
#if IND == ib1
      enddo ! ib1 ! stop ib1 loop
#endif

      ! collected communication
cDBG  time_asum = time_asum - Wtime()
      call MPIallsum( HSm, g%comm ) ! reduces over all grid processes
cDBG  time_asum = time_asum + Wtime()
cDBG  nact_asum = nact_asum + 1

#if IND == ib1
      do ib1 = 1, nb1 ; j(COL) = ib1+io1 ! restart ib1 loop
#endif
        do ib2 = 1, nb2 ; j(ROW) = ib2+io2
          ! store on the distributed matrix grid
#ifdef SYMMETRIC
          ne = set_sym_matrix_entry( HSm(1,ib2, IND ), HSm(2,ib2, IND ), j, Hm, Sm )
#else
          ne = set_matrix_entry( HSm(1,ib2, IND ), HSm(2,ib2, IND ), j, Hm, Sm )
#endif
cDBG      nelem = nelem+ne ! count up locally stored matrix elements
        enddo ! ib2
      enddo ! ib1




    endif ! icyc == 0

    enddo ! icyc ! end loop over Np communication cycles

cDBG  if(o>0 .and. nact_comm>0) write(o,'(3A,F10.3,A,I9,9A)') sym, fun, 'sendrecv  took', time_comm, ' sec for', nact_comm, ' ops'
cDBG  if(o>0 .and. nact_asum>0) write(o,'(3A,F10.3,A,I9,9A)') sym, fun, 'allreduce took', time_asum, ' sec for', nact_asum, ' ops'
cDBG  if(o>0 .and. nact_Hops>0) write(o,'(3A,I9,9A)') sym, fun, 'Hamiltonian   was called', nact_Hops, ' times'
cDBG  if(o>0 .and. nact_sprd>0) write(o,'(3A,I9,9A)') sym, fun, 'inner product was called', nact_sprd, ' times'

cDBG  nelem_loc = nelem
cDBG  nelem = nelem_loc .MPIsum. g%comm

    t(2) = Wtime()

    deallocate( HSm, HSs, stat=ist ) ! auxiliary vectors for H*|spsi> and S*|spsi> are not needed any more

    if( MPIparallel( band_comm ) ) then
      call MPIallsum( Hm, band_comm )
      call MPIallsum( Sm, band_comm )
cDBG  nelem_all = nelem .MPIsum. band_comm
cDBG  else ; nelem_all = nelem
    endif ! MPIparallel band_comm
cDBG  if( nelem_all /= nb*nb .and. o>0) write(o,'(4A,9(I12,A))') sym, fun, WARNING(0), 'accepted', nelem_all, ' entries, but nb^2=', nb*nb


    t(3) = Wtime()

    allocate( Ene(nb), stat=ist ) ; Ene = 0.
    istatus = solve_matrix( nb, g%comm, Hm, Sm, Ene ) ! solve using ScaLAPACK, if possible

    deallocate( Sm, stat=ist )

    if( istatus /= 0 ) then
      if(o>0) write(o,'(3A,I3,9A)') sym, fun, 'diagonalization failed, status=', istatus
      return
    endif ! no SUCCESS

    ! store a copy of the old wave functions in Ss (temporarily)
    Ss(:,1:nb1) = s(:,1:nb1)

    ! create the new wave functions from the eigenvectors
    ! of the rotated subspace (1st time of writing to array s)
    s = 0. ! init

    t(4) = Wtime()

    allocate( zz(get_block_size(ROW),get_block_size(COL)), stat=ist) ; zz = 0. ; jb_memory = -1 ! init as no values in zz

#ifndef NOMPI
! MPI part

    allocate( st(nxyzs,nb1), stat=ist ) ; st = 0.
    if( ist /= 0 ) then
      write(*,'(4A,F12.3,9A)') sym, fun, ERROR, 'allocation of temp. wfs failed,', nxyzs*nb1*0.5**17 * R1_C2 ,' MiByte'
      stop 'SUB allocation of temp array ST failed!'
    endif
    st(:,1:nb1) = Ss(:,1:nb1) ! local copy

    isend(1:2) = (/io1,nb1/) ! offset, number
    do icyc = 0, Np-1 ! communication cycles
      ip(-1) = modulo(my-icyc,Np) ! receive from process
      ip(+1) = modulo(my+icyc,Np) ! send    to   process
      if( icyc > 0 ) then
        ! communicate
        call MPI_sendrecv( isend, 2, MPI_INTEGER, ip(+1), icyc, &
                           irecv, 2, MPI_INTEGER, ip(-1), icyc, band_comm, istat, ierr )
        io2 = irecv(1) ; nb2 = irecv(2)
#ifdef FULL_DEBUG
        if(o>0) write(o,'(3A,9(I6,A))') sym, fun, 'rank',my,' is about to receive bands#',io2+1,' through',io2+nb2,' from rank',ip(-1)
#endif
        Ss = 0. ! init receiving array
        call MPI_sendrecv( st(:,1:nb1), nxyzs*nb1, PREC( R1_C2 ), ip(+1), icyc, &
                           Ss(:,1:nb2), nxyzs*nb2, PREC( R1_C2 ), ip(-1), icyc, band_comm, istat, ierr )
#ifdef FULL_DEBUG
        if(o>0) write(o,'(3A,2(I6,A),9I8)') sym, fun, 'rank',my,'  icyc=',icyc,'  status=',istat
        call MPIbarrier( band_comm )
#endif
      else  ! icyc > 0
        ! no communication needed
        io2 = io1 ; nb2 = nb1 ! offset and number ! see below
        Ss(:,1:nb2) = st(:,1:nb1) ! local copy (not needed, because icyc==0 is the first cycle)
      endif ! icyc > 0

! end MPI part
#else
! No MPI

    if( nb1 < nb ) stop 'SUB no parallelization of bands without MPI.'
    do icyc = 0, 0 ! one time
      io2 = io1 ; nb2 = nb1 ! offset and number
#endif

      do ib1 = 1, nb1 ; j(COL) = ib1+io1
        do ib2 = 1, nb2 ; j(ROW) = ib2+io2
          ! new wave functions = eigenvectors * old wave functions
          ! cc are the eigenvector coefficients after diagonalization
          cc = get_matrix_block( Hm, j, g%comm, zz, jb_memory )

          s(:,ib1) = s(:,ib1) + cc * Ss(:,ib2)

        enddo ! ib2
      enddo ! ib1

    enddo ! icyc
    energy(1:nb1) = Ene(1+io1:nb1+io1) ! new energy eigenvalues (of locally stored bands)

    t(5) = Wtime()

    istatus = 0 ! success

    if( present( residual ) ) residual = 0.

cDBG  if( show_e .and. o>0 ) then
cDBG    do ib1 = 1, nb1
cDBG      write(o,'(3A,I6,I2,I6,A,F24.16,A,ES8.1E2,A)') sym, fun, 'band(', ib1, jspin, kp%jk, '): energy =', energy(ib1), ' Ha'
cDBG    enddo ! ib1
cDBG  endif ! show_e

    t(9) = Wtime()
cTIM  if(o>0) write(o,'(3A,5F10.3,9A)') sym, fun, 'times[set,com,sol,lin,tot]', t(2:5)-t(1:4), t(9)-t(0), ' sec'
  endfunction ! pp_SR






#if R1_C2 == 1
  integer function p1_SR_r( & ! no ScaLAPACK, no band parallelization, but Domain Decomposition
#else
  integer function p1_SR_c( &
#endif
      g, vloc, jspin, atm, kp, energy, s, Ss, band_comm, band_ioff, residual, show_energy ) &
  result( istatus )
  use type_grid, only: grid
  use type_atom, only: atom
  use type_kpoint, only: kpoint
  use operators, only: scalar_product, Hmt
  use MPIconst, only: Wtime
  use MPItools, only: MPIallsum, MPIparallel
  use LAPACK, only: generalized_eig ! serial LAPACK
cDBG  use toolbox, only: write_bmp_file
cDBG  use LAPACK, only: check_matrix
  implicit none
    ! parameter
    character(len=*), parameter     :: fun = ' sr: '
    ! arguments
    type(grid), intent(in)          :: g
    real, intent(in)                :: vloc(:,:,:,:)
    integer, intent(in)             :: jspin
    type(atom), intent(in)          :: atm(:)
    type(kpoint), intent(in)        :: kp
    real, intent(out)               :: energy(:)
    REAPLEX, intent(inout)          ::  s(:,:)
    REAPLEX, intent(inout)          :: Ss(:,:)
    integer, intent(in)             :: band_comm
    integer, intent(in)             :: band_ioff
    real, intent(inout), optional   :: residual(:)
    logical, intent(in), optional   :: show_energy

    ! local vars
    logical                         :: show_e
    real, allocatable               :: Ev(:)
    real, allocatable               :: res(:)
    REAPLEX, allocatable            :: HSs(:,:), Hm(:,:), Sm(:,:)
    integer                     :: nb, nxyzs
    integer                     :: ib, jb
    integer                     :: ist!, istat, ierr
cDBG  logical, save             :: write_Hm = .true.
cDBG  integer                   :: nerr(1:2)
    real                        :: t(0:9)
    t(0) = Wtime() ! start total

    show_e = .false. ; if( present( show_energy ) ) show_e = show_energy

    nb = size( s, 2 ) ! number of bands
cDBG  if(o>0) write(o,'(3A,I6,9A)') sym, fun, 'matrix dimension =', nb
    if( MPIparallel( band_comm ) ) stop 'SUB sr: band parallelization is not implemented here!'

    nxyzs = size( s, 1 ) ! number of grid degrees of freedom

cNaN  if( any( s/= s) ) stop 'DIIS subspace_rotation NaN in  S on entry.'
cNaN  if( any(Ss/=Ss) ) stop 'DIIS subspace_rotation NaN in Ss on entry.'
cDBG  if( size(Ss,1)/=nxyzs ) stop 'DIIS subspace_rotation dim#1 of SS does not match S.'
cDBG  if( size(Ss,2) < nb ) stop 'DIIS subspace_rotation dim#2 of SS too small.' ! may be larger!

    allocate( Hm(nb,nb), Sm(nb,nb), Ev(nb), stat=ist ) ! subspace Hamiltonian and Overlap matrix
    allocate( HSs(nxyzs,1:2), stat=ist ) ! one auxiliary vector for H*|spsi> and S*|spsi>

    t(1) = Wtime() ! start setup
    ! set up the Hamiltonian H and the OverlapMatrix S
    do ib = 1, nb

      call Hmt( g, vloc, jspin, atm, kp, ket=s(:,ib:ib), Hket=HSs(:,1:1), Sket=HSs(:,2:2) )
cNaN  if( any(HSs/=HSs) ) stop 'DIIS subspace_rotation NaN in HSs(:,1:2).'

#ifdef DEBUG
   ! set up the full matrix
#define jb_LIMIT nb
   ! and check if the matrix is symmetric/hermitian
#else
   ! set up only the upper triangular matrix
#define jb_LIMIT ib
   ! and retrieve the other triangular part from H^dagger
#endif

      do jb = 1, jb_LIMIT
        !!!! IMPROVEMENT 1:
        !!!!
        !!!! For a better parallelization, compute the scalar products
        !!!! locally first and do an Allreduce with the two matrices
        !!!! to avoid communication overhead
        Hm(jb,ib) = scalar_product( s(:,jb), HSs(:,1), h3=g%hvol )!, comm=g%comm ) ! communication collected
        Sm(jb,ib) = scalar_product( s(:,jb), HSs(:,2), h3=g%hvol )!, comm=g%comm )
        !!!!
        !!!! further IMPROVEMENT suggestion:
        !!!! Allreduce only the triangular part of the matrix:
        !!!! The diangonal elements of Sm are 1.0, so one could store and allreduce
        !!!!   H11 S21 S31 ...
        !!!!   H12 H22 S32  .
        !!!!   H13 H23 H33  .
        !!!!   ...  .   .  ...
#if jb_LIMIT == ib
        ! exploit symmetric/hermitician
#if R1_C2 == 2
        if( ib == jb ) then ! diagonal elements must be real
          Hm(ib,jb) = real( Hm(ib,jb) )
          Sm(ib,jb) = real( Sm(ib,jb) )
        else  ! diagonal
          Hm(ib,jb) = conjg( Hm(jb,ib) ) ! transpose and conjugate
          Sm(ib,jb) = conjg( Sm(jb,ib) ) ! transpose and conjugate
        endif ! diagonal
#else
        Hm(ib,jb) = Hm(jb,ib) ! transpose
        Sm(ib,jb) = Sm(jb,ib) ! transpose
#endif
        ! end     symmetric/hermitician
#endif

      enddo ! jb
    enddo ! ib

#undef jb_LIMIT

    t(2) = Wtime() ! stop setup

!     do ib = 1, nb
!       write(unit=8,fmt='(999F10.3)') Hm(:,ib)
!       write(unit=9,fmt='(999F10.3)') Sm(:,ib)
!     enddo ! ib

    deallocate( HSs, stat=ist ) ! auxiliary vector for H*|spsi> is not needed any more

cNaN  if(any(Hm/=Hm)) stop 'DIIS subspace_rotation NaN in Hm.'
cNaN  if(any(Sm/=Sm)) stop 'DIIS subspace_rotation NaN in Sm.'

    t(3) = Wtime() ! start communication

    ! Allreduce for the spatial integrals
    if( MPIparallel( g%comm ) ) then
      call MPIallsum( Hm, g%comm )
      call MPIallsum( Sm, g%comm )
    endif ! parallel

    t(4) = Wtime() ! stop communication

#ifdef DEBUG
    if( write_Hm ) then ! display
!       if(o>0) write(o,'(3A,I4)') sym, fun, 'show matrix, nb =', nb
      if( nb < 13 ) then
        if(o>0) then
          write(o,'(3A)') '' ! empty line
          write(o,'(3A,I2,A,I2,A)') sym, fun, 'Hamiltonian (', nb, ' x', nb, ' )'
          do ib = 1, nb
#if R1_C2 == 1
            write(o,'(24F10.6)') Hm(:,ib)
#else
            write(o,'(24F10.6)') real(Hm(:,ib)), aimag(Hm(:,ib))
#endif
          enddo ! ib
          write(o,'(3A)') '' ! empty line
          write(o,'(3A)') sym, fun, 'Overlap'
          do ib = 1, nb
#if R1_C2 == 1
            write(o,'(24F10.6)') Sm(:,ib)
#else
            write(o,'(24F10.6)') real(Sm(:,ib)), aimag(Sm(:,ib))
#endif
          enddo ! ib
          write(o,'(3A)') '' ! empty line
        endif ! o/=0
      else
!         call write_bmp_file( 'dmp/Hmat', abs(Hm), style='inverse'  )
!         call write_bmp_file( 'dmp/Smat', abs(Sm), style='inverse'  )
      endif ! nb < 13
      write_Hm = .false.
    endif ! write_Hm
#endif

cDBG  nerr(1) = check_matrix( Hm, name='Hm' ) ! these checks can only be performed,
cDBG  nerr(2) = check_matrix( Sm, name='Sm' ) ! if the full matrices have been set up.
cDBG  if( any( nerr /= 0 ) ) stop 'DIIS Hamiltonian and/or Smatrix are not symmetric/hermitian.'



! #ifndef DEBUG
!     ! symmetrize Anew = ( A + Adagger )/2
!     ! (this is not needed, if the triangualr matrix has been set up)
!     do ib = 1, nb
!       do jb = 1, ib ! triangular loop
!         ! enforce symmetry/hermiticity
!         Hm(ib,jb) = 0.5*( Hm(ib,jb) + conjg(Hm(jb,ib)) )
!         Hm(jb,ib) = Hm(ib,jb)
!         Sm(ib,jb) = 0.5*( Sm(ib,jb) + conjg(Sm(jb,ib)) )
!         Sm(jb,ib) = Sm(ib,jb)
!       enddo ! jb
!     enddo ! ib
! #endif

    t(5) = Wtime() ! start diagonalization

    ! solve the generalized eigenvalue problem Hm x = Ev Sm x
    istatus = generalized_eig( Hm, Sm, Ev )
    if( istatus /= 0 ) then
      if(o>0) write(o,'(3A,I2,A,I6,9A)') sym, fun, 'spin#', jspin, ' kpoint#', kp%jk, ' diagonalization failed!'
      return ! failure
    endif ! success

    t(6) = Wtime() ! stop diagonalization

    deallocate( Sm, stat=ist ) ! now Hm contains the new eigenvectors and Sm is not needed any more

    ! store a copy of the old wave functions in Ss (temporarily)
    Ss = s

    t(7) = Wtime() ! start rotation
#ifndef USE_MATMUL
    ! create the new wave functions from the eigenvectors
    ! of the rotated subspace
    ! IMPROVEMENT map this to a BLAS3 call
    s = 0.
    do ib = 1, nb
      do jb = 1, nb
        ! new wave functions = eigenvectors * old wave functions
        ! Hm contains the eigenvector coefficients after diagonalization
        s(:,ib) = s(:,ib) + Hm(jb,ib) * Ss(:,jb)
      enddo ! jb
      ! new energy eigenvalue
      energy(ib) = ev(ib)
    enddo ! ib
#else
    s = matmul( Ss, Hm )
    energy(1:nb) = ev(1:nb) ! new energy eigenvalue
#endif
    t(8) = Wtime() ! stop rotation

    ! care for the residuals
    if( present( residual ) ) then
cDBG  if( size(residual) < nb ) stop 'subspace_rotation: residual vector not long enough!'
      allocate( res(1:nb) )
      res = residual**2 ! old residuals squared
      do ib = 1, nb
        residual(ib) = sqrt( dot_product( abs(Hm(:,ib))**2 , res(:) ) )
      enddo ! ib
      deallocate( res )
    endif ! present residual

    deallocate( Hm, Ev, stat=ist )

    if( show_e .and. o/=0 ) then
      if( present( residual ) ) then
        do ib = 1, nb
          write(o,'(3A,I6,I2,I6,A,F24.16,A,ES8.1E2,9A)') sym, fun, &
            'band(', ib, jspin, kp%jk, '): energy =', energy(ib), ' +/-', residual(ib), ' Ha'
        enddo ! ib
      else  ! present residual
        do ib = 1, nb
          write(o,'(3A,I6,I2,I6,A,F24.16,9A)') sym, fun, &
            'band(', ib, jspin, kp%jk, '): energy =', energy(ib), ' Ha'
        enddo ! ib
      endif ! present residual
    endif ! show_e

    t(9) = Wtime() ! stop total
cTIM  if(o>0) write(o,'(3A,5F10.3,9A)') sym, fun, 'times[set,com,dia,rot,tot]', t(2:8:2)-t(1:7:2), t(9)-t(0), ' sec'
  endfunction ! subspace_rotation






#if R1_C2 == 1
  !! CRS stands for CompressedRowStorage
  !! i.e. all non-zero Hamiltonian
  !! and Overlap Matrix
  !! elements are written to a file
  status_t function Hamiltonian2CRS_r( &
#else
  status_t function Hamiltonian2CRS_c( &
#endif
      g, vloc, jspin, atm, kp, filename, rc, assume_filling ) result( ist )
  use type_grid, only: grid
  use type_atom, only: atom
  use type_kpoint, only: kpoint
  use operators, only: Hmt!, Smat
  use configuration, only: o, WARNING
  use MPIconst, only: Wtime
cDBG  use LAPACK, only: check_matrix
  implicit none
    ! parameter
    character(len=*), parameter           :: fun = ' Hamiltonian2CRS: '
    real, parameter                       :: DEF_ASSUME_FILLING = 1.0 ! 1.0:can be full, 0.1 --> 10% nonzeros
    integer, parameter                    :: IH=1
    integer, parameter                    :: IS=2
    ! arguments
    type(grid), intent(in)                :: g !! grid descriptor
    real, intent(in)                      :: vloc(:,:,:,:) !! local potential
    integer, intent(in)                   :: jspin !! spin
    type(atom), intent(in)                :: atm(:) !! parallelized atom list
    type(kpoint), intent(in)              :: kp !! kpoint
    character(len=*), intent(in)          :: filename !! name of the CRS file
    REAPLEX, intent(in)                   :: rc !! use a real number or a complex number
    real, intent(in), optional            :: assume_filling !! make an assumption of sparsity

    ! local vars
    REAPLEX, allocatable                  :: v(:,:), Ov(:,:), Rv(:,:)
    integer                               :: nxyzs, i, j, io
    string_t                              :: files(IH:IS)

    real                                  :: t(0:9) ! time
    real                                  :: af, sparse
    integer(8)                            :: nzeros

    ! CRS / CSR format according to Y. Saad
    character(len=256)                    :: head  !! header
    integer(8)                            :: nnz   !! number of nonzero elements
    REAPLEX, allocatable                  :: a(:)  !! nonzero matrix elements (1:nnz)
    integer(4), allocatable               :: ja(:) !! colIndex list (1:nnz)
    integer(4)                            :: n     !! dimension N
    integer(8), allocatable               :: ia(:) !! rowPointer list (0:n)
    integer(8)                            :: mem
cDBG  integer :: nerr

    t(0) = Wtime() ! start

    af = DEF_ASSUME_FILLING ; if(present(assume_filling)) af = min(max(0.,assume_filling),1.)
cDBG  if(o>0) write(o,'(3A,F7.2,9A)') sym, fun, 'save memory by assumed filling', af*100., ' %'

    ist = -1 ! init result as error for early return

    if( any( g%nproc > 1 ) ) stop 'DIIS Hamiltonian2CRS: only in serial version!'

cDBG      do i = 1, size(atm)
cDBG        nerr = check_matrix( atm(i)%Hm(:,:,1), name='a%Hm' )
cDBG      enddo ! i

    nxyzs = product( g%ng(1:4) ) ! number of grid degrees of freedom

cDBG  if(o>0) write(o,'(3A,I8)') sym, fun, 'dimension =', nxyzs

    write(unit=files(IH),fmt='(9A)') trim(filename), '_H.crs'
    write(unit=files(IS),fmt='(9A)') trim(filename), '_S.crs'

cDBG  if(o>0) write(o,'(9A)') sym, fun, 'open files: "', trim(files(IH)), '" and "', trim(files(IS)), '".'

    ! auxiliary vectors |spsi?>, S*|spsi>, H*|spsi>
    allocate( v(nxyzs,1), Ov(nxyzs,1), Rv(nxyzs,1), stat=ist )

    n = nxyzs
    mem = nint( af * real(n)**2 )
cDBG  if(o>0) write(o,'(3A,F12.3,9A)') sym, fun, 'try to allocate', &
cDBG    ( 4.*dble(mem) + 8.*(n+1) + R1_C2 * 8.*dble(mem) )/2.**20, ' MiByte'
    if( dble(mem) > 0.5 * 2.**32 ) then
      if(o>0) write(o,'(9A)') sym, fun, 'too many (> 2^32) elements requested, return!'
      ist = -1
      return
    endif

    allocate( ia(1:n+1), ja(mem), a(mem), stat=ist )
    if( ist /= 0 ) stop 'Hamiltonian2CRS: cannot allocate memory for matrix.'

    do iO = IH, IS ! Hamiltonian, OverlapMatrix

      t(iO+0) = Wtime() ! t1, t2

      nnz = 0 ! init
      ia = 0
      ja = 0
      a = 0.

      nzeros = 0 ! init counter

      ! set up the Hamiltonian \hat{H} and the OverlapMatrix \hat{S}
      do i = 1, nxyzs
        ! prepare the vector with only zeros and 1 entry 1.0
        v = 0. ; v(i,1) = 1.
        ! get the effect of overlap matrix and Hamiltonian onto this vector
        selectcase( iO )
        case( IS ) ; call Hmt( g, vloc, jspin, atm, kp, ket=v, Hket=Rv, Sket=Ov )
        case( IH ) ; call Hmt( g, vloc, jspin, atm, kp, ket=v, Hket=Ov, Sket=Rv )
        endselect ! iO

        ! start processing elements
        ia(i) = nnz+1 ! store the row-Pointer
        do j = 1, nxyzs
          if( Ov(j,1) /= 0. ) then
            ! found a nonzero element
            nnz = nnz+1 ! count up
            if( nnz > mem ) stop 'Hamiltonian2CRS: assumed too much sparsity.'
            a(nnz) = Ov(j,1) ! copy into value array
            ja(nnz) = j ! store column-index
          else  ! nonzero
            nzeros = nzeros + 1
          endif ! nonzero
        enddo ! j

      enddo ! i
      ! do it once more
      ia(n+1) = nnz+1

      t(iO+2) = Wtime() ! t3, t4

      ! create header
      write(unit=head,fmt='(A48,I12,I24,9A)',iostat=ist) files(iO), &
        n, nnz, ' = N (dimension), Nnz (# of nonzero elements)', &
        ' for CompressedRowStorage. char*256 h, int*4 ia(1:N+1), ja(1:Nnz), real*8 a(1:Nnz)'
      if( ist /= 0 ) stop 'Hamiltonian2CRS: creating header information failed, maybe too short.'
      head(241:256) = 'EndOf256Header!'
      ! write binary file
      open(unit=80,file=files(iO),form='unformatted',action='write',iostat=ist)
      if( ist /= 0 ) stop 'Hamiltonian2CRS: cannot open file for writing.'
      write(80) head, ia(1:n+1), ja(1:nnz), a(1:nnz) ! binary writing
      close(80)

      t(iO+4) = Wtime() ! t5, t6

cDBG  if(o>0) write(o,'(5A,F12.3,9A)') sym, fun, '"', trim(files(iO)), '" file size', &
cDBG    ( 8 + 256 + 4.*nnz + 8.*(n+1) + R1_C2 * 8.*nnz )/2.**20, ' MiByte'

      sparse = real(nzeros)/real(n*n)
      if(o>0) write(o,'(3A,9(F0.2,A))') sym, fun, 'filling ', (1.-sparse)*100.,  &
        ' % ==> sparsity ', sparse*100., ' % > assumed ', (1.-af)*100., ' %'

    enddo ! iO

    deallocate( v, a, ia, ja, stat=ist )

cDBG  if(o>0) write(o,'(9A)') sym, fun, 'files: "', trim(files(IH)), '" and "', trim(files(IS)), '" successfully written.'
cDBG      t(9) = Wtime() ! end all
cTIM  if(o>0) write(o,'(3A,5F10.3,9A)') sym, fun, 'times [Hmt,Smt,hIO,sIO,tot]', t(3:6)-t(1:4), t(9)-t(0), ' sec'
  endfunction ! Hamiltonian2CRS



#if R1_C2 == 1
  !! The full Hmt is written to a file
  status_t function Hamiltonian2file_r( &
#else
  status_t function Hamiltonian2file_c( &
#endif
      g, vloc, jspin, atm, kp, filename, rc ) result( ist )
  use type_grid, only: grid
  use type_atom, only: atom
  use type_kpoint, only: kpoint
  use operators, only: Hmt!, Smat
  use configuration, only: o, ERROR, WARNING
  use MPIconst, only: Wtime
  use MPItools, only: MPImaster, operator(.MPIsum.), operator(.MPIdiff.)
cDBG  use LAPACK, only: check_matrix
  implicit none
    ! parameter
    character(len=*), parameter           :: fun = ' Hamiltonian2file: '
    integer, parameter                    :: IH = 1
    integer, parameter                    :: IS = 2
    character(len=*), parameter           :: SymHerm(1:2) = (/'symmetric','hermitian'/)
    ! arguments
    type(grid), intent(in)                :: g !! grid descriptor
    real, intent(in)                      :: vloc(:,:,:,:) !! local potential
    integer, intent(in)                   :: jspin !! spin
    type(atom), intent(in)                :: atm(:) !! parallelized atom list
    type(kpoint), intent(in)              :: kp !! kpoint
    character(len=*), intent(in)          :: filename !! name of the CRS file
    REAPLEX, intent(in)                   :: rc !! use a real number or a complex number
    ! local vars
    REAPLEX, allocatable                  :: v(:,:), Ov(:,:), Hv(:,:)
    integer                               :: nxyzs, nx, n_all, i, ix,iy,iz, iO, jx,jy,jz, i3(3)
    character(len=48)                     :: files(IH:IS)

    real                                  :: prog, prog10, t(0:9) ! time

    ! CRS / CSR format according to Y. Saad
!     character(len=256)                    :: head  !! header
    REAPLEX, allocatable                  :: xrow(:)  !! nonzero matrix elements (1:nnz)
    integer                               :: n!, nzero, n3(3)     !! dimension N
    integer(kind=8)                       :: nzero_all, nn, mem, jrec, j1, j2
cDBG  integer :: nerr, nzero

    t(0) = Wtime() ! start

    ist = -1 ! init result as error for early return

cDBG  do i = 1, size(atm)
cDBG    nerr = check_matrix( atm(i)%Hm(:,:,1), name='a%Hm' )
cDBG    if( nerr > 0 ) stop 'Hamiltonian2file: one or more a%Hm is not symmetric!'
cDBG  enddo ! i

    if( any( g%ng(1:3) < g%ng_all(1:3) ) ) then
      if(o>0) write(o,'(9A)') sym, fun, ERROR, 'direct access works only in serial!'
      return
    endif ! parallelized

    n_all = product( g%ng_all(1:4) ) ! number of grid degrees of freedom
    nxyzs = product( g%ng(1:4) ) ! number of local degrees of freedom

cDBG  if(o>0) write(o,'(3A,I8)') sym, fun, 'dimension =', n_all

    write(unit=files(IH),fmt='(9A)') trim(filename), '.Hmt'
    write(unit=files(IS),fmt='(9A)') trim(filename), '.Smt'

cDBG  if(o>0) write(o,'(9A)') sym, fun, 'open files: "', trim(files(IH)), '" and "', trim(files(IS)), '".'

    allocate( v(nxyzs,1), Hv(nxyzs,1), Ov(nxyzs,1), stat=ist ) ! auxiliary vectors |spsi?>, S*|spsi>, H*|spsi>

    n = n_all
    nn = n ! convert to integer*8
    mem = nn*nn
cDBG  if(o>0) write(o,'(3A,F16.3,9A)') sym, fun, 'file size', ( R1_C2 * 8.*real(mem) )/2.**20, ' MiByte'
cDBG  if(o>0) write(o,'(3A,F16.6,9A)') sym, fun, 'file size', ( R1_C2 * 8.*real(mem) )/2.**30, ' GiByte'

    nx = g%ng(1)
    if( nx .MPIdiff. g%comm ) stop 'Hamiltonian2file, the length in X-direction must be the same for all domains!'
    allocate( xrow(nx), stat=ist )


    t(1) = Wtime() ! t1

    do iO = IH, IS ! Hamiltonian, OverlapMatrix

      prog10 = 0.01 ! show first time at 1%
      nzero_all = 0 ! count zero elements

      ! open binary file
      open(unit=80+iO,file=files(iO),form='unformatted',action='write',iostat=ist, &
           convert='BIG_ENDIAN',access='direct',recl= nx * 2 * R1_C2 ) ! recl in units of 4 byte, real*8: 2words, complex*16: 4 words
      if( ist /= 0 ) then
        if(o>0) write(o,'(9A)') sym, fun, 'cannot open file "', trim(files(iO)), '" for writing.'
        return
      endif
      if(o>0) write(o,'(5A,I6)') sym, fun, 'file "', trim(files(iO)), '" opened for writing, direct access, RECL=', nx * 2 * R1_C2

    enddo ! iO

    t(2) = Wtime() ! t2

    ! set up the Hamiltonian \hat{H} and the OverlapMatrix \hat{S}
    j1 = 0 ! init total index
    do jz = 1, g%ng_all(3) ; i3(3) = jz - g%ioff(3)
    do jy = 1, g%ng_all(2) ; i3(2) = jy - g%ioff(2)
    do jx = 1, g%ng_all(1) ; i3(1) = jx - g%ioff(1)
      j1 = j1+1 ! count up total index

      ! prepare the vector with only zeros and 1 entry 1.0
      v = 0.0
      if( all( i3 > 0 ) .and. all( i3 <= g%ng(1:3) ) ) then
        i = 1+(i3(1)-1)*1 + (i3(2)-1)*g%ng(1) + (i3(3)-1)*g%ng(1)*g%ng(2) ! local index
        v(i,1) = 1.0
      endif

      ! get the effect of overlap matrix and Hamiltonian onto this vector
      call Hmt( g, vloc, jspin, atm, kp, ket=v, Hket=Hv, Sket=Ov )

cDBG  nzero = 0
      i = 0
      do iz = 1, g%ng(3)
        do iy = 1, g%ng(2)
            ix = 1
            j2 = 1 + (ix+g%ioff(1)-1)*1 + (iy+g%ioff(2)-1)*g%ng_all(1) + (iz+g%ioff(3)-1)*g%ng_all(1)*g%ng_all(2) ! 1st index in global storage
            jrec = (j1-1)*n_all+j2
            jrec = (jrec-1)/nx+1
            ! write elements Ov(i+1:i+nx) at file position jrec
            xrow = Ov(i+1:i+nx,1) ; write(80+iS,rec=jrec) xrow
            xrow = Hv(i+1:i+nx,1) ; write(80+iH,rec=jrec) xrow
cDBG        nzero = nzero + count( xrow == 0. )
            i = i + nx ! forward
        enddo ! iy
      enddo ! iz
cDBG  nzero = nzero .MPIsum. g%comm
cDBG  nzero_all = nzero_all + nzero

      prog = real(j1)/real(n_all)
      if( prog > prog10 ) then
        if(o>0) write(o,'(3A,F6.1,A)') sym, fun, 'progress', prog*100., ' %'
        prog10 = 0.1*ceiling( prog*10. ) ! set the next level, when the progress is reported to 10%, 20%, ...
      endif ! report progress

    enddo ! jx
    enddo ! jy
    enddo ! jz

    t(3) = Wtime() ! t3

    do iO = IH, IS ! Hamiltonian, OverlapMatrix

      if(o>0) write(o,'(9A)') sym, fun, 'close file "', trim(files(iO)), '".'
      close(80+iO,iostat=ist)

cDBG  if(o>0) write(o,'(3A,F7.2,A)') sym, fun, 'sparsity', real(nzero_all)/real(n_all)**2*100., ' %'
    enddo ! iO

    t(4) = Wtime() ! t4

    deallocate( v, xrow, Ov, Hv, stat=ist )

    if(o>0) write(o,'(9A)') sym, fun, 'files: "', trim(files(IH)), '" and "', trim(files(IS)), '" successfully written.'
    t(9) = Wtime() ! end all
cTIM  if(o>0) write(o,'(3A,5F10.3,9A)') sym, fun, 'times [pre,opn,set,cls,tot]', t(1:4)-t(0:3), t(9)-t(0), ' sec'
  endfunction ! Hamiltonian2file




#if R1_C2 == 1
  !! The full Hmt is written to a file
  status_t function Hamiltonian2ASCII_r( &
#else
  status_t function Hamiltonian2ASCII_c( &
#endif
      g, vloc, jspin, atm, kp, basename, rc ) result( ist )
  use type_grid, only: grid
  use type_atom, only: atom
  use type_kpoint, only: kpoint
  use operators, only: Hmt!, Smat
  use configuration, only: o, ERROR, WARNING
cDBG  use LAPACK, only: check_matrix
  use MPIconst, only: Wtime
  use MPItools, only: MPImaster, operator(.MPIsum.), operator(.MPIdiff.)

    type(grid), intent(in)                :: g !! grid descriptor
    real, intent(in)                      :: vloc(:,:,:,:) !! local potential
    integer, intent(in)                   :: jspin !! spin
    type(atom), intent(in)                :: atm(:) !! parallelized atom list
    type(kpoint), intent(in)              :: kp !! kpoint
    character(len=*), intent(in)          :: basename !! name of the file
    REAPLEX, intent(in)                   :: rc !! use a real number or a complex number

    character(len=*), parameter           :: fun = ' Hamiltonian2ASCII: '
    character(len=*), parameter           :: SymHerm(1:2) = (/'symmetric','hermitian'/)
    REAPLEX, allocatable                  :: v(:,:)
    integer                               :: nxyzs, n_all, i1, ix,iy,iz, jx,jy,jz, i3(3)
    string_t                              :: filename

    real                                  :: prog, prog10, t(0:9) ! time

    integer                               :: n !! dimension N
    integer(kind=8)                       :: nzero_all, mem, jrec, j1, j2
cDBG  integer :: i, nerr, nzero

    t(0) = Wtime() ! start

    ist = -1 ! init result as error for early return

cDBG  do i = 1, size(atm)
cDBG    nerr = check_matrix( atm(i)%Hm(:,:,1), name='a%Hm' )
cDBG    if( nerr > 0 ) stop 'Hamiltonian2file: one or more a%Hm is not symmetric!'
cDBG  enddo ! i

    if( any( g%ng(1:3) < g%ng_all(1:3) ) ) then
      if(o>0) write(o,'(9A)') sym, fun, ERROR, 'ASCII writing works only in serial!'
      return
    endif ! parallelized

    n_all = product( g%ng_all(1:4) ) ! number of grid degrees of freedom
    nxyzs = product( g%ng(1:4) ) ! number of local degrees of freedom

cDBG  if(o>0) write(o,'(3A,I0)') sym, fun, 'dimension = ', n_all

    write(unit=filename,fmt='(9A)', iostat=ist) trim(basename), '.AB_', SymHerm( R1_C2 )

cDBG  if(o>0) write(o,'(9A)') sym, fun, 'open files: "', trim(filename), '".'

    allocate( v(nxyzs,0:2), stat=ist ) ! auxiliary vectors |spsi?>, H*|spsi>, S*|spsi>
    if( ist /= 0 ) stop 'Hamiltonian2ASCII: failed to allocated 3 vectors!'

    n = n_all
!     nn = n ! convert to integer*8

    t(1) = Wtime() ! t1

    prog10 = 0.01 ! show first time at 1%
    nzero_all = 0 ! count zero elements

    ! open ASCII file
    open(unit=80,file=filename,form='formatted',action='write',iostat=ist)
    if( ist /= 0 ) then
      if(o>0) write(o,'(9A)') sym, fun, 'cannot open file "', trim(filename), '" for writing.'
      return
    endif
    if(o>0) write(o,'(9A)') sym, fun, 'file "', trim(filename), '" opened for writing.'

    ! write a commented header line
    write(unit=80,fmt='(3A,I0)') '# ', SymHerm( R1_C2 ), ' generalized eigenvalue problem Ax=lBx, dimension ', N
#if R1_C2 == 1
    write(unit=80,fmt='(9A)') '# exported into format: i j A[i,j] B[i,j]'
#else
    write(unit=80,fmt='(9A)') '# exported into format: i j Re{A[i,j]} Im{A[i,j]}  Re{B[i,j]} Im{B[i,j]}'
#endif

    t(2) = Wtime() ! t2

    ! set up the Hamiltonian \hat{H} and the OverlapMatrix \hat{S}
    j1 = 0 ! init total index
    do jz = 1, g%ng_all(3) ; i3(3) = jz - g%ioff(3)
    do jy = 1, g%ng_all(2) ; i3(2) = jy - g%ioff(2)
    do jx = 1, g%ng_all(1) ; i3(1) = jx - g%ioff(1)
      j1 = j1+1 ! count up total index

      ! prepare the vector with only zeros and 1 entry 1.0
      v = 0.0 ! init clear
      if( all( i3 > 0 ) .and. all( i3 <= g%ng(1:3) ) ) then
        i1 = 1+(i3(1)-1)*1 + (i3(2)-1)*g%ng(1) + (i3(3)-1)*g%ng(1)*g%ng(2) ! local index
        v(i1,0) = 1.0 ! set unit vector
      endif ! inside my domain

      ! get the effect of overlap matrix and Hamiltonian onto this vector
      call Hmt( g, vloc, jspin, atm, kp, ket=v(:,0:0), Hket=v(:,1:1), Sket=v(:,2:2) )

cDBG  nzero = 0 ! init

      ! ======================================================================================
      do j2 = 1, j1
        if( abs( v(j2,1) ) < 4E-16 ) then ! Element of the Hamiltonian
          if( abs( v(j2,2) ) < 4E-16 ) cycle ! Element of the Overlap Matrix
        endif
#if R1_C2 == 1
       write(unit=80,fmt='(I0," ",I0,9A)') j1, j2, trim(nn( v(j2,1) )), ' ', trim(nn( v(j2,2) ))
#else
       write(unit=80,fmt='(I0," ",I0,9A)') j1, j2, trim( nn( real( v(j2,1) ) ) ), &
         trim(nn( aimag( v(j2,1) ) )), ' ', trim(nn( real( v(j2,2) ) )), trim(nn( aimag( v(j2,2) ) ))
#endif
cDBG    nzero = nzero + 1
      enddo ! j2
      ! ======================================================================================

! cDBG  nzero = nzero .MPIsum. g%comm
cDBG  nzero_all = nzero_all + nzero

      prog = real(j1)/real(n_all) ! compute progress
      if( prog > prog10 ) then ! report progress
        if(o>0) write(o,'(3A,F6.1,A)') sym, fun, 'progress', prog*100., ' %'
        prog10 = 0.1*ceiling( prog*10. ) ! set the next level, when the progress is reported to 10%, 20%, ...
      endif ! report progress

    enddo ! jx
    enddo ! jy
    enddo ! jz

    t(3) = Wtime() ! t3

    if(o>0) write(o,'(9A)') sym, fun, 'close file "', trim(filename), '".'
    close(80,iostat=ist)

cDBG  if(o>0) write(o,'(3A,F0.3,A)') sym, fun, 'found ', nzero_all*1E-6, ' million non-zero entries.'

    t(4) = Wtime() ! t4

    deallocate( v, stat=ist )

    if(o>0) write(o,'(9A)') sym, fun, 'file "', trim(filename), '" successfully written.'
    t(9) = Wtime() ! end all
cTIM  if(o>0) write(o,'(3A,5(" ",F0.3),9A)') sym, fun, 'times [pre,opn,set,cls,tot]', t(1:4)-t(0:3), t(9)-t(0), ' sec'
  contains

    character(len=24) function nn( val ) result( str ) ! nice_number
      real, intent(in) :: val
      real    :: avl, l10
      integer :: p10, ist, i

      avl = abs( val )
      if( avl < 1E-32 ) then ; str = ' 0' ; return ; endif

      l10 = log10( avl )
      p10 = floor( l10 )
      selectcase( p10 )
      case(    :-25 ) ; str = ' 0'
      case( -24:-10 )
        write(unit=str,fmt='(ES17.9E2)',iostat=ist) val
      case(  -9:-4  )
        write(unit=str,fmt='(ES19.12E1)',iostat=ist) val
      case(  -3:    )
        if( val == 1.0 ) then ; str = ' 1' ; return ; endif
        write(unit=str,fmt='(" ",F0.15)',iostat=ist) val
        i=len(str) ; do while( str(i:i) == '0' ) ; str(i:i) = ' ' ; i=i-1 ; enddo ! clear tailing zer0s
      endselect ! p10

   endfunction ! nn

  endfunction ! Hamiltonian2ASCII



!- extended
#endif

#if R1_C2 == 2
! begin of module tail
#undef REAPLEX

#ifdef EXTENDED
!+ extended

  status_t function test( ) result( ios )
    write(*,*,iostat=ios) __FILE__,' no module test implemented!'
  endfunction ! test

!- extended
#endif

endmodule ! subspace
#endif
#endif

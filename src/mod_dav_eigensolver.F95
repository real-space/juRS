#include "config.h"


!-- You can change the number of Davidson iterations, if you comment out 
!-- the following line and set a number of maximum iterations.
! #define CONTROL_MAXITER 30

!-- You can change the threshold of Davidson iteration, if you comment out 
!-- the following line and set a threshold value.
! #define CONTROL_THRESHOLD 1.0d-9


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

#define SYMMETRIC

#ifdef R1_C2
! This file will be preprocessed twice, first
! with R1_C2 == 1, and then with R1_C2 == 2
! then the two parts are concatenated to one source.
! This requires that the module head has to be
! appearing, when R1_C2 == 1 and the line "end module <name>"
! with R1_C2 == 2.
#if R1_C2 == 1

!> @brief Fortran module for Davidson eigensolver of Kohn-Sham equation 
!! @author Marta Gibertini and Shigeru Tsukamoto (s.tsukamoto@fz-juelich.de)
!! @version 1.10
!!
!! @details solves the generalized eigenvalue problem H|s(:,i)> = e(i) S|s(:,i)> (*)
!!          using the Davidson method
!!          Algorithm: 
!!          -- chose the initial basis V = |psiK(:,i)> = |s(:,i)> where {|s(:,i)>} 
!!             are the initial guess for the solutions of (*).
!!          -- calculate |psiH(:,i)> = H|psiK(:,i)> and |psiS(:,i)> = S|psiK(:,i)>
!!          -- iterate:
!!             -- calculate the matrix MH = V^(+) H V = <psiH(:,i) | psi(:,j)> 
!!             -- calculate the matrix MS = V^(+) S V = <psiS(:,i) | psi(:,j)> 
!!             -- solve the smaller eigenvalue problem MH |y(:,i)> = e(i) MS |y(:,i)>
!!             -- calculate the first nu ritz vectors 
!!                |ritzK(:,i)> = sum_(j<nu) |psiK(:,j)> |y(j,i)> 
!!             -- calculate also 
!!                |ritzH(:,i)> = sum_(j<nu) |psiH(:,j)> |y(j,i)> 
!!                and
!!                |ritzS(:,i)> = sum_(j<nu) |psiS(:,j)> |y(j,i)> 
!!             -- calculate the residuals
!!                |res(:,i)> =  e_i * |ritzS(:,i)> -|ritzH(:,i)>
!!             -- eliminate the converged residuals
!!             -- new V = {|ritzK(:,i)>} + {f(|res(:,i)>)}
!!             -- the dimension of the basis change
!!             -- stop if all residuals converged or the maximum number
!!                of iterations is reached
!!
!!
module dav_eigensolver
  use configuration, only: o ! output unit, 0: no output
implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: sym = 'DAV' !! module symbol
  character(len=*), parameter, private :: fun = ': ' !! for production

  integer, parameter, public :: KEY_EIGENSOLVER_DAV = 0 ! eigensolver collection
  integer, parameter         :: DAV_HARDLIMIT = 10 ! hard limit of DAV cycles

  public :: dav_eigen_solve
  interface dav_eigen_solve
    module procedure dav_eig_solve_r, dav_eig_solve_c
  endinterface


#ifdef EXTENDED
!+ extended

  public :: test

!- extended
#endif

  contains

! end of head part
#endif


#if R1_C2 == 1
#define REAPLEX real
#define conjg(x) (x)
#define BLAS_GEMM dgemm
#define BLAS_COPY dcopy
#else
#define REAPLEX complex
#define BLAS_GEMM zgemm
#define BLAS_COPY zcopy
#endif


#if R1_C2 == 1
  logical function dav_eig_solve_r( &
#else
  logical function dav_eig_solve_c( &
#endif
      atm, g, kp, &
      vloc, jspin, energy, residual, threshold, maxiter, &
      s, band_comm, band_ioff,  precond, show_energy ) &
  result( conv )
  use configuration, only: STOPONERROR
  use type_grid,     only: grid
  use type_atom,     only: atom
  use type_kpoint,   only: kpoint
  use operators,     only: Hmt
  use operators,     only: Precon_Nf1
  use operators,     only: scalar_product
  use MPItools,      only: MPIallsum, MPIparallel

  use ScaLAPACK, only: init_matrix
#ifdef SYMMETRIC
  use ScaLAPACK, only: set_entries => set_sym_entries
#else
#error 'no interface for non-symmetric'
#endif
#ifdef SYMMETRIC
  use ScaLAPACK, only: set_entries => set_sym_entries
#else
#error 'no interface for non-symmetric'
#endif
  use ScaLAPACK, only: solve_matrix
  use ScaLAPACK, only: init_matrix, free_matrix
  use ScaLAPACK, only: get_matrix

!   use communicators, only: band_comm ! should not be global in the future
!   use communicators, only: band_ioff ! should not be global in the future

  use MPIconst, only: Wtime, PREC, MPI_STATUS_SIZE, MPI_INTEGER
  use MPItools, only: MPInprocs, MPImyrank, MPIparallel ! MPI functions
  use MPItools, only: operator(.MPIsum.), operator(.MPImax.) ! MPI operators
  use MPItools, only: MPIallsum, MPIbarrier, MPIbcast0 ! MPI subroutines
  implicit none
    ! parameters
cDBG    character(len=*), parameter :: fun = ' solve: '
    REAPLEX, parameter          :: ONE=1.d0, ZERO=0.d0
    integer, parameter          :: I_K=1 !< Index for Kohn-Sham (K) wave function vectors
    integer, parameter          :: I_H=2 !< Index for Hamiltonian (H) * wave function vectors
    integer, parameter          :: I_S=3 !< Index for overlap matrix (S) * wave function vectors

    ! arguments
    type(atom),   intent(in)           :: atm(:)        !< atoms
    type(grid),   intent(in)           :: g             !< coarse grid
    type(kpoint), intent(in)           :: kp            !< kpoint
    real,         intent(in)           :: vloc(:,:,:,:) !< Local potential
    integer,      intent(in)           :: jspin
    real,         intent(out)          :: energy(:)
    real,         intent(out)          :: residual(:)
    real,         intent(in)           :: threshold     !< Tolerance of convergency
    integer,      intent(in)           :: maxiter       !< Maximum number of Davidson iterations
    REAPLEX,      intent(inout)        :: s(:,:)        !< (nxyzs,nb1) initial basis
    MPI_Comm,     intent(in)           :: band_comm     !< MPI band communicator
    integer,      intent(in)           :: band_ioff     !< band offset
    logical,      intent(in), optional :: precond
    logical,      intent(in), optional :: show_energy

    !local vars
    integer :: max_iter  !< Maxnum number of Davidson iterations actually used in the computation
    real    :: tolerance !< Tolerance of convergency actually ued in the computation
    integer ::  ihs
    !vectors psiK of the used basis, S|psiK> and H|psiK>
    REAPLEX, allocatable, save      :: psiK(:,:), psiS(:,:), psiH(:,:)
    !temporary vectors 
    REAPLEX, allocatable, save      :: Ppsi(:,:) , Pprec(:) 
    !ritz vectors, S|ritz> and H|ritz>
    REAPLEX, allocatable, save      :: ritzK(:,:), ritzS(:,:), ritzH(:,:) 
    REAPLEX, allocatable            :: MS(:,:), MH(:,:), MSHp(:,:) 
    real, allocatable               :: eigvals(:) 

    logical                         :: show_e  ! show energy

    real                            :: snorm, ene, f
    logical, save                   :: temp_arrays_allocated = .false.
    logical                         :: run
    real                            :: resold, res1st
    real                            :: h3  !! product of grid spacings
    integer                         :: isend(5), irecv(5)
    integer                         :: Np, Ncyc, icyc, me ! for band parallelization
    integer                         :: io1, io1B, nb1 ! offset and number of locally stored bands
    integer                         :: io2, io2B, nb2 ! offset and number of remotely stored bands
#ifndef NOMPI
    integer                         :: istat(MPI_STATUS_SIZE), ierr, ip(-1:+1)
#else
    integer, parameter              :: k1 = 0
#endif

    integer                         :: jb, nxyzs,  nbm, nb
    integer                         :: i1, i2, ne, ie
    integer                         :: i, maxdim, nu, ist, k, iter, new, nstates, nstates2,  mxold, maxall, maxall2
    integer                         :: runint , convint , run2, conv2

    !----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0

    !-- Maximum number of Davidson iterations
#ifdef CONTROL_MAXITER
      max_iter = CONTROL_MAXITER
#else
      max_iter = maxiter 
#endif

    !-- Tolerance for convergency
#ifdef CONTROL_THRESHOLD
      tolerance = CONTROL_THRESHOLD
#else
      tolerance = threshold
#endif

    !dV = g%hvol * one
    nxyzs = size(s,1) ! number of grid degrees of freedom = #dof
    !nxyzs = product( g%ng(1:4) ) ! number of degrees of freedom
    nb1   = size(s,2) ! local number of bands
    nu    = nb1
    io1   = band_ioff ! offset of bands

    maxdim  = nb1 !*2 !*1.5)  !NOT USED IN THIS PARALLEL VERSION ! YOU CAN CHANGE THIS NUMBER!  nb1 <= maxdim <= 3*nb1
    maxall  = max(maxdim, nb1*2)    ! maxall = max dimension of the local basis
    nbm     = nb1 .MPImax. band_comm ! max (over all process) local number of bands
    maxall2 = nbm*2 +1   
    Np      = MPInprocs( band_comm )    ! number of processes in band parallelization
    Ncyc    = Np-1 ! 0:Np-1 full number of communication cycles
#ifdef SYMMETRIC
    Ncyc    = Np/2 ! integer divide ! reduced number of communication cycles
#endif
    me      = MPImyrank( band_comm ) ! rank of this process in band parallelization

    ! arrays have to be allocated (only the 1st time)
    if( .not. temp_arrays_allocated ) then
      allocate( psiK(nxyzs,max(maxall,1)), psiS(nxyzs,max(maxall,1)), psiH(nxyzs,max(maxall,1)), Ppsi(nxyzs,max(maxall2,1)), Pprec(nxyzs),  stat=ist )
      if( ist /= 0 ) stop 'dav_eig_solve: allocation failed.'
      temp_arrays_allocated = ( ist == 0 )
    endif ! not temp_arrays_allocated


    ! initial basis V = [ psiK(:,0) , ... psiK(:,nb1) ] 
    ! start initialization
    psiK = 0.
    psiS = 0.
    psiH = 0.
    psiK(:,1:nb1) = s(:,1:nb1)

    do i1 = 1, nb1
      call Hmt( g, vloc, jspin, atm, kp, ket=psiK(:,i1:i1), Hket=psiH(:,i1:i1), Sket=psiS(:,i1:i1) )
    enddo ! i1
    !end of initialization


    show_e = .false. ; if( present( show_energy ) ) show_e = show_energy

    io1B = io1
    nstates = 0
    run = .true.
    runint = 0
    convint = 1
 
    !--
    !-- Davidson iteration cycle
    !--
    iter = 0 !-- Initialize iteration counter
    do while( run )
      call MPIbarrier( band_comm )
      iter = iter + 1
 
      Nb    = 0
      nbm   = 0
      Nb    = nb1 .MPIsum. band_comm ! global number of all bands
      nbm   = nb1 .MPImax. band_comm ! max local number of bands
      nb2   = 0              ! init  ! remote number of bands
 
      allocate( MSHp(max(nb1,1),nbm), stat=ist )
      if( ist /= 0 ) then
        write(*,*) '!!! alloc didnt work!!'
        return ! failed
      endif ! ist /= 0

      !-- Allocate BLACS arrays for Hamiltonian matrix and overlap matrix
      !-- The matrices should be deallocated by the function free_matrix below
      ist = init_matrix( Nb, g%comm, MH, MS )
  
      isend(1:5) = (/io1,nb1,nxyzs,Nb,io1B/) ! offset, number, dof, number of all bands
      do icyc = 0, Ncyc ! full or reduced number of communication cycles
        if( icyc > 0 ) then
#ifndef NOMPI
! MPI part
          ! determine ranks to communicate with
          ip(-1) = modulo(me-icyc,Np) ! receive from process
          ip(+1) = modulo(me+icyc,Np) ! send    to   process
          ! hand shake
          call MPI_sendrecv( isend, 5, MPI_INTEGER, ip( 1), icyc, &
                             irecv, 5, MPI_INTEGER, ip(-1), icyc, band_comm, istat, ierr )
          io2 = irecv(1) ; nb2 = irecv(2) ; io2B = irecv(5)
          ! communicate
          call MPI_sendrecv( psiK(:,1:nb1), nxyzs*nb1, PREC( R1_C2 ), ip( 1), icyc, &
                             Ppsi(:,1:nb2), nxyzs*nb2, PREC( R1_C2 ), ip(-1), icyc, band_comm, istat, ierr )
          if( ( Np > 1 ) .and. ( 2*Ncyc == Np ) .and. ( icyc == Ncyc ) .and. ( me >= Ncyc ) ) cycle ! if SYMMETRIC some blocks can be skipped
! end MPI part
#endif
        
        else  ! icyc > 0
          ! icyc == 0 ! diagonal w.r.t. band parallelization
          io2 = io1 ; nb2 = nb1 ; io2B = io1B ! offset and number
! !$omp parallel workshare
          Ppsi(:,1:nb2) = psiK(:,1:nb1) ! copy
! !$omp end parallel workshare
        endif ! icyc > 0

        do ihs = I_H, I_S, I_S-I_H
 
        ! V = {psi_i} = [ psi(:,1) , psi(:,2), ...] 
        !M_H = V^(+) H V
        !M_S = V^(+) S V
          selectcase( ihs )
            case( I_H ) ; call BLAS_GEMM ('c','n',nb1,nb2,nxyzs,ONE,psiH,size(psiH,1),Ppsi,size(Ppsi,1),ZERO,MSHp,size(MSHp,1))
            case( I_S ) ; call BLAS_GEMM ('c','n',nb1,nb2,nxyzs,ONE,psiS,size(psiS,1),Ppsi,size(Ppsi,1),ZERO,MSHp,size(MSHp,1))
cDBG        case default ; stop 'SUB: ihs must be either I_H or I_S!'
          endselect ! ihs
          !!SHOULD I MULTIPLY BY hvol?????
         
          if( MPIparallel( g%comm ) ) then 
              call MPIallsum( MSHp, g%comm ) ! reduce matrix elements over all grid processes
          endif
         
         
          selectcase( ihs )
            case( I_H ) ; ne = set_entries( MSHp(1:nb1,1:nb2), (/io1B,io2B/), MH )
            case( I_S ) ; ne = set_entries( MSHp(1:nb1,1:nb2), (/io1B,io2B/), MS )
cDBG        case default ; stop 'SUB: ihs must be either I_H or I_S!'
          endselect ! ihs
        enddo ! ihs

      enddo ! icyc ! end loop over Np communication cycles


      deallocate( MSHp, stat=ist ) ! auxiliary matrix HSm not needed any more, matrix elements are stored in Hm and Sm
  
      if( MPIparallel( band_comm ) ) then
        ! Allreduce the matrix elements over the band parallelization
        call MPIallsum( MH, band_comm )
        call MPIallsum( MS, band_comm )
      endif ! MPIparallel band_comm

      allocate( eigvals(Nb) ) ! eigvec_new(bsz, nu), eigvals_new(nu) )
      eigvals =0.

 
      ! solve the smaller eigenv. problem MH |y_i> = e_i MS |y_i>
      ist = solve_matrix( Nb, g%comm, MH, MS, eigvals ) ! solve ( Hm - Ene Sm ) y = 0 using ScaLAPACK, if possible (store y in Hm)

      if( ist /= 0 ) then
        write(*,*) '!!! solve matrix didnt work!!'
        return ! failed
      endif ! ist /= 0
      !eigenvector {y_i} in MH now

      allocate(   ritzK(nxyzs,max(nu,1)), ritzH(nxyzs,max(nu,1)), ritzS(nxyzs,max(nu,1)) )
      ritzK = ZERO
      ritzS = ZERO
      ritzH = ZERO
      allocate( MSHp(nbm,max(nu,1)), stat=ist )
  
      isend(1:3) = (/io1,nb1,io1B/) ! offset, number


      do icyc = 0, Np-1 ! full number of communication cycles
        if( icyc > 0 ) then
#ifndef NOMPI
!+ MPI part

          ip(-1) = modulo(me-icyc,Np) ! receive from process
          ip(+1) = modulo(me+icyc,Np) ! send    to   process
          ! hand shake
          call MPI_sendrecv( isend, 3, MPI_INTEGER, ip( 1), icyc, &
                             irecv, 3, MPI_INTEGER, ip(-1), icyc, band_comm, istat, ierr )
          io2 = irecv(1) ; nb2 = irecv(2) ; io2B = irecv(3)
!- MPI part
#endif
        else  ! icyc > 0
          ! no communication needed
          io2 = io1 ; nb2 = nb1 ; io2B = io1B ! offset and number ! see below
        endif ! icyc > 0

        ist = get_matrix( MH, (/io2B,io1/), g%comm, MSHp(1:nb2,1:nu) ) ! load eigenvector coefficients from Hm into HSm
        call MPIbcast0(MSHp, g%comm)

        do ihs = I_K, I_S, 1
          if( icyc > 0 ) then
#ifndef NOMPI
!+ MPI part
            ! communicate
            selectcase( ihs )
              case( I_K ) ; call MPI_sendrecv( psiK(:,1:nb1), nxyzs*nb1, PREC( R1_C2 ), ip( 1), icyc, &
                                 Ppsi(:,1:nb2), nxyzs*nb2, PREC( R1_C2 ), ip(-1), icyc, band_comm, istat, ierr )
              case( I_S ) ; call MPI_sendrecv( psiS(:,1:nb1), nxyzs*nb1, PREC( R1_C2 ), ip( 1), icyc, &
                                 Ppsi(:,1:nb2), nxyzs*nb2, PREC( R1_C2 ), ip(-1), icyc, band_comm, istat, ierr )
              case( I_H ) ; call MPI_sendrecv( psiH(:,1:nb1), nxyzs*nb1, PREC( R1_C2 ), ip( 1), icyc, &
                                 Ppsi(:,1:nb2), nxyzs*nb2, PREC( R1_C2 ), ip(-1), icyc, band_comm, istat, ierr )
            endselect ! ihs
!- MPI part
#endif
          else  ! icyc > 0
            ! no communication needed
          !  io2 = io1 ; nb2 = nb1 ; io2B = io1B ! offset and number ! see below
            selectcase( ihs )
              case( I_K ) ; Ppsi(:,1:nb2) = psiK(:,1:nb1) ! copy
              case( I_S ) ; Ppsi(:,1:nb2) = psiS(:,1:nb1) ! copy
              case( I_H ) ; Ppsi(:,1:nb2) = psiH(:,1:nb1) ! copy
            endselect ! ihs
          endif ! icyc > 0
   
    
          ! |ritz> = V |y_i>
          selectcase( ihs )
            case( I_K ) ; call BLAS_GEMM ('n','n',nxyzs,nu,nb2,ONE,Ppsi,size(Ppsi,1),MSHp,size(MSHp,1),ONE,ritzK,size(ritzK,1))
            case( I_S ) ; call BLAS_GEMM ('n','n',nxyzs,nu,nb2,ONE,Ppsi,size(Ppsi,1),MSHp,size(MSHp,1),ONE,ritzS,size(ritzS,1))
            case( I_H ) ; call BLAS_GEMM ('n','n',nxyzs,nu,nb2,ONE,Ppsi,size(Ppsi,1),MSHp,size(MSHp,1),ONE,ritzH,size(ritzH,1))
          endselect ! ihs
        enddo !ihs
        ! nu = initial nb1
      enddo ! icyc

      !-- Deallocate BLACS array for Hamiltonian-matrix and overlap-matrix, and invalidate ScaLinfo array
cDBG      if(o>0) write(o,'(9A)') sym, fun, 'Deallocate BLACS arrays: MH and MS'
      ist = free_matrix( g%comm, MH, MS )
      !-- Deallocate BLACS array for workspace
      if( allocated( MSHp ) ) deallocate( MSHp, stat=ist )
      if( ist /= 0 ) stop sym // fun // "ERROR: deallocate BLACS array MSHp failed."
      call MPIbarrier( band_comm )

      !-- Calculate residual vector: Ppsi
cDBG      call MPI_Barrier( g%comm, ist )
cDBG      if(o>0) write(o,'(9A)') sym, fun, 'Calculate residual vetors'
      do i1 = 1, nu
        !residual
        ene = eigvals(i1+io1) 
        !here Ppsi = residual
        Ppsi(:,i1) = ene * ritzS(:,i1) - ritzH(:,i1)
      enddo !i1 

 
      !-- Test the convergence through the norm of the residual vectors
cDBG      call MPI_Barrier( g%comm, ist )
cDBG      if(o>0) write(o,'(9A)') sym, fun, 'Test the convergence through the norm of the residual vectors'
      nstates = 0
      residual(:) = 0.0 
      do i1 = 1, nu

        ! test the convergence: norm of the residual < 1E-14
        snorm = scalar_product( Ppsi(:,i1), Ppsi(:,i1), g%hvol, g%comm )
!        if( snorm < 1E-9 ) then
        if ( snorm < tolerance ) then
          !-- the i1-th vector is considered to be converged.
        else
          nstates = nstates + 1
          residual(:) = residual(:) + snorm
          call Precon_Nf1( g, kp, v=Ppsi(:,i1), Pv=Pprec(:) )
          Ppsi(:,nstates) = Pprec(:)
        endif ! snorm

      enddo ! i1=1, nu

 
      ! if all the residual are converged or
      ! if the max number of iteration is reached then exit 
cDBG      call MPI_Barrier( g%comm, ist )
      if(o>0) write(o,'(2A,2(A,I4),A,ES9.2)') sym, fun, "Iter: ", iter, " subspace dim.: ", Nb, " residual(1): ", residual(1)
      if (nstates == 0) then
        run = .false.
        runint = 1
        conv = .true.
        convint = 0 
        residual(:)=0.0
      elseif ((residual(1)/real(nstates))<= tolerance ) then
        run = .false.
        runint = 1
        conv = .true. 
        convint = 0 
        residual(:) = residual(:)/real(nstates) 
      elseif (iter>= max_iter) then 
        run = .false.
        runint = 1
        conv = .false. 
        convint = 1 
        residual(:) = residual(:)/real(nstates) 
      else
        residual(:) = residual(:)/real(nstates) 
      endif

 
      call MPIbarrier( band_comm )

      io1B = io1
      isend(1:4) = (/io1, nstates, runint, convint/) ! offset, number, dof, number of all bands
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
          io2 = irecv(1) ; nstates2 = irecv(2) ; run2 = irecv(3) ; conv2 = irecv(4)
          if (io2<io1) io1B = io1B + nstates2
          if (run2 == 0) run = .true.
          if (conv2 == 1) conv = .false.
! end MPI part
#endif
        endif ! icyc > 0
      enddo

     
      if (run .EQV. .true.) deallocate(eigvals)

      !choose the new basis (not bigger than maxdim)
      psiK = 0.
      psiS = 0.
      psiH = 0.
      psiK(:,1:nu) = ritzK(:,1:nu)
      psiS(:,1:nu) = ritzS(:,1:nu)
      psiH(:,1:nu) = ritzH(:,1:nu)
      do i1 = 1, nstates
         new = i1 + nu
         psiK(:,new) = Ppsi(:,i1)  
         call Hmt( g, vloc, jspin, atm, kp, ket=psiK(:,new:new), Hket=psiH(:,new:new), Sket=psiS(:,new:new) )
         ! normalize |psi>, scale |Spsi> and |Hpsi> simultaneously
         snorm = scalar_product( psiS(:,new), psiK(:,new), g%hvol, g%comm )
         if( snorm < 1E-14 ) stop 'DAV: norm of <psi(i)|S|psi(i)> is less than 10^-14'
         f = 1./sqrt( snorm )

         ! normalize |psi(i1)> = f * |psi(i1)>
         psiK(:,new) = f * psiK(:,new)
         psiS(:,new) = f * psiS(:,new)
         psiH(:,new) = f * psiH(:,new)
      enddo ! i1
      deallocate( ritzK, ritzH, ritzS )
      nb1 = nu + nstates
    enddo ! while run

    !-- Output a summary of the Davidson iterations
    if(o>0) write(o,'(3A,I3,A,ES9.2,A,L)') sym, fun, "iter = ", iter, " tolerance =", tolerance, ":", conv

    ! return the new wavefunctions and the new energy
    s(:,1:nu) = psiK(:,1:nu)
    energy(1:nu) = eigvals(1+io1:nu+io1) ! new energy eigenvalues (of locally stored bands)

    deallocate( eigvals )
  endfunction ! dav_eig_solve

#if R1_C2 == 2
! begin of module tail
#undef REAPLEX

#ifdef EXTENDED

  integer function test( )
    write(*,*,iostat=test) __FILE__,' no module test implemented!'
  endfunction ! test

!- extended
#endif

endmodule ! dav_eigensolver
#endif
#endif

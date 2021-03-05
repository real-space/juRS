#ifdef DEBUG_ALL
#define DEBUG
#endif

! #define DEBUG

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
!! SteepestDescent method without explicit reorthogonalization
module sd_eigensolver
  use configuration, only: o ! output unit, 0: no output
implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: sym = 'SD' !! module symbol

  public :: sd_update
#ifdef EXTENDED
  public :: test
#endif

  interface sd_update
    module procedure sd_update_r, sd_update_c
  endinterface

  interface submatrix2by2
    module procedure submatrix2by2_r, submatrix2by2_c
  endinterface
  
  real, parameter, private :: TINYRATIO = 2E-16 ! for the 2x2 submatrix diagonalization

  contains

! end of head part
#endif


#if R1_C2 == 1
#define REAPLEX real
#else
#define REAPLEX complex
#endif


#if R1_C2 == 1
  logical function sd_update_r( &
#else
  logical function sd_update_c( &
#endif
      atm, g, kp, &
      vloc, jspin, energy, residual, threshold, maxiter, &
      s, Ss, ibnd, precond, show_energy ) result( converged )
  use configuration, only: STOPONERROR
  use type_grid, only: grid
  use type_atom, only: atom
  use type_kpoint, only: kpoint
  use operators, only: Hmt
!   use operators, only: Precon
!   use operators, only: Precon_Nf1
  use operators, only: scalar_product
!   use MPItools, only: MPIallsum
  use unitsystem, only: eV, eV_
  implicit none
    ! parameters
    character(len=*), parameter     :: fun = ' update: '
    ! arguments
    type(atom), intent(in)          :: atm(:) ! atoms
    type(grid), intent(in)          :: g ! coarse grid
    type(kpoint), intent(in)        :: kp            ! kpoint
    real, intent(in)                :: vloc(:,:,:,:)
    integer, intent(in)             :: jspin
    real, intent(out)               :: energy(:)
    real, intent(out)               :: residual(:)
    real, intent(in), optional      :: threshold ! no use
    integer, intent(in), optional   :: maxiter ! no use
    integer, intent(in), optional   :: ibnd !! CG only for this band, if present
    logical, intent(in), optional   :: precond
    logical, intent(in), optional   :: show_energy

    REAPLEX, intent(inout)          :: s (:,:) !    (nxyzs,nbnd)
    REAPLEX, intent(inout)          :: Ss(:,:) !    (nxyzs,nbnd)

    ! local vars
    REAPLEX     :: Hs(size(s,1),1) ! (nxyzs)
    REAPLEX     :: Rv(size(s,1),1) ! (nxyzs)
!     REAPLEX     :: Pv(size(s,1),1) ! (nxyzs)
    REAPLEX     :: HR(size(s,1),1) ! (nxyzs)
    REAPLEX     :: SR(size(s,1),1) ! (nxyzs)

    REAPLEX                         :: sHr, alpha!, snorm
    real                            :: sSs, sHs, rHr, beta, gamma, ene, res, fs, fr!, snrm
    logical                         :: precondition, show_e  ! show energy
    integer                         :: ib, nbnd, natm, nxyzs, ibs, ibe, jb!, i, ist

    precondition = .true. ; if( present( precond ) ) precondition = precond
    show_e = .false. ; if( present( show_energy ) ) show_e = show_energy

    nxyzs = size(s,1)
    nbnd  = size(s,2)
    natm  = size(atm,1)
#ifdef DEBUG
    if(nxyzs /= product( g%ng(1:4) )) stop 'sd_eig_solve: dim #1 of S does not match the grid.'
    if(size(Ss,2)<nbnd) stop 'sd_eig_solve: dim #2 of SS does not match dim #2 of S.'
    if(size(Ss,1)/=nxyzs) stop 'sd_eig_solve: dim #1 of SS does not match the grid.'
#endif

    if( present( ibnd ) ) then
      ! only for the one wave function #ibnd
      ib = min(max(1,ibnd),nbnd)
      ibs = ib ; ibe = ib
    else  ! present( ibnd )
      ! for all wave functions passed
      ibs = 1 ; ibe = nbnd
    endif ! present( ibnd )

    do ib = ibs, ibe

      ! update |Ss(ib)>, the memory of the adjoint wave functions is shared among the
      ! different sk-states, so always update before using it
      ! |Ss(ib)> = \hat{S} |s(ib)>
      call Hmt( g, vloc, jspin, atm, kp, ket=s(:,ib:ib), Hket=Hs(:,1:1), Sket=Ss(:,ib:ib) )

#ifdef NaN_SEARCH
  if( any( s(:,ib)/= s(:,ib)) ) stop 'NaN in  s! (1st)'
  if( any(Ss(:,ib)/=Ss(:,ib)) ) stop 'NaN in Ss! (1st)'
  if( any(Hs/=Hs) ) stop 'NaN in Hs! (1st)'
#endif

      ! clear arrays

      ! energy(ib) = <s(ib)|Hs(ib)>
      sHs = scalar_product( s(:,ib), Hs(:,1), h3=g%hvol, comm=g%comm ) ! collect
      sSs = scalar_product( s(:,ib), Ss(:,ib), h3=g%hvol, comm=g%comm ) ! communication
      if( sSs < 1E-12 ) stop 'SD: a state cannot be normalized <s|S|s> < 10^-12'
      fs = 1./sqrt(sSs)
      ene = fs * sHs * fs
#ifdef NaN_SEARCH
      if( ene /= ene ) stop 'cg_eig_solve: energy is NaN'
#endif
      energy(ib) = ene


      ! the residual vector Rv that has to be minimized
      ! |Rv> = |Hs(ib)> - energy(ib)*|Ss(ib)>
      Rv(:,1) = fs*Hs(:,1) - (ene*fs) * Ss(:,ib)
!       if(o>0) write(o,'(3A,999ES10.2E3)') sym, fun, '|R> =', Rv

      if( precondition ) then
!         Pv = Rv ! copy
!         call Precon( g, kp=kp, v=Pv, Pv=Rv )
#ifdef NaN_SEARCH
        if( any(Rv/=Rv) ) stop 'NaN in Pgrd! (0th)'
#endif
      endif ! precondition

      call Hmt( g, vloc, jspin, atm, kp, ket=Rv(:,1:1), Hket=HR(:,1:1), Sket=SR(:,1:1) )
#ifdef NaN_SEARCH
      if( any(HR/=HR) ) stop 'NaN in HR! (1st)'
      if( any(SR/=SR) ) stop 'NaN in SR! (1st)'
#endif

      res = real( scalar_product( Rv(:,1), SR(:,1), h3=g%hvol, comm=g%comm ) )

      if( res < 1.E-12 ) then
        converged = .true.

        ! scale with f to be normalized
        s (:,ib) = fs * s (:,ib)
        Ss(:,ib) = fs * Ss(:,ib)

      else ! res < 1.E-12

        fr = 1./sqrt( res )
        ! scale with f

        sHs = ene
        sHr = fs * scalar_product( s(:,ib), HR(:,1), h3=g%hvol, comm=g%comm ) * fr  ! collect
        rHr = fr * real( scalar_product( Rv(:,1), HR(:,1), h3=g%hvol, comm=g%comm ) ) * fr ! communication

        ! diagonalize the 2x2 matrix
        ! ( sHs sHc )
        ! ( cHs cHc ) and find the lower eigenvalue gamma
        ! as well as the components alpha and beta of the
        ! corresponding eigenvector
        gamma = submatrix2by2( sHs, rHr, sHr, alpha, beta )

        ! update |s(ib)>
        s (:,ib) = (alpha*fs) *  s(:,ib) + (beta*fr) * Rv(:,1)
        Ss(:,ib) = (alpha*fs) * Ss(:,ib) + (beta*fr) * SR(:,1)

      endif ! res < threshold

      residual(ib) = res ! store residual information

      if( show_e ) then
        jb = ib ; if( present( ibnd ) ) jb = ibnd
        if(o>0) write(o,'(3A,I6,I2,I6,A,F24.16,9A)') sym, fun, &
          'band(', jb, jspin, kp%jk, '): energy =', energy(ib)*eV, eV_
      endif ! show_e

! #ifdef DEBUG
!       call Hmt( g, vloc, jspin, atm, kp, ket=s(:,ib:ib), Hket=Hs(:,1), Sket=Ss(:,ib:ib) )
!       sSs = real( scalar_product( s(:,ib), Ss(:,ib), h3=g%hvol, comm=g%comm ) )
!       if( abs( sSs - 1.0 ) > 1.E-8 ) then
!         if(o>0) write(o,'(3A,I6,A,F24.16)') sym, fun, 'band(', ib, '): norm deviates =', sSs
!       endif ! not properly normalized
! #endif

    enddo ! ib

    converged = .false.

#ifdef DEBUG
    if(o>0) write(o,'(3A,F24.16,9A)') sym, fun, 'lowest energy =', energy(1)*eV, eV_
#endif
  endfunction ! sd_eig_solve




#if R1_C2 == 1
  real function submatrix2by2_r( &
#else
  real function submatrix2by2_c( &
#endif
                                 sAs, pAp, sAp, alpha, beta  ) result( gamma )
  ! diagonalization of the 2 by 2 matrix
  ! produces the normalized eigenvector [alpha, beta]
  implicit none
    ! parameters
    character(len=*), parameter :: fun = ' submatrix2by2: '
    ! arguments
    real, intent(in)      :: sAs, pAp ! the matrix is assumed hermitian
                                      ! so the diagonal elements must be real
    REAPLEX, intent(in)   :: sAp      ! the off-diagonal element(s)
    REAPLEX, intent(out)  :: alpha
    real, intent(out)     :: beta

    real                  :: tmp, sAppAs

#if R1_C2 == 1
    sAppAs = sAp*sAp ! = |sAp|^2
#else
    sAppAs = real( sAp*conjg(sAp) ) ! = |sAp|^2
#endif

    ! submatrix2by2 =
    !   lowest eigenvalue of the 2 x 2 matrix ( sas , pas )
    !                                         ( sap , pap )

    gamma = 0.5*(sAs+pAp) - sqrt( 0.25*(sAs-pAp)*(sAs-pAp) + sAppAs )

    if( abs((sAs-gamma)/gamma) > TINYRATIO ) then
      tmp = 1.0/sqrt(sAppAs+(sAs-gamma)*(sAs-gamma))
      alpha = sAp*tmp
      beta = -(sAs-gamma)*tmp
    else ! abs...
      alpha = cmplx(1.0,0.0)
      beta  = 0.0
    endif ! abs...

  endfunction ! submatrix2by2

#if R1_C2 == 2
! tail part

#ifdef EXTENDED
!+ extended

  integer function test( )
    write(*,*,iostat=test) __FILE__,' no module test implemented!'
  endfunction ! test

!- extended
#endif

endmodule ! sd_eigensolver
#endif
#endif

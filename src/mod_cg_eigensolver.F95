#ifdef DEBUG_ALL
#define DEBUG
#endif

!!#define DEBUG


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
!! ConjugateGradient method and explicit reorthogonalization
module cg_eigensolver
  use configuration, only: o ! output unit, 0: no output
implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: sym = 'CG' !! module symbol

  public :: cg_eigen_solve
  public :: reorder
#ifdef EXTENDED
  public :: test
#endif

  interface cg_eigen_solve
    module procedure cg_eig_solve_r, cg_eig_solve_c
  endinterface

  interface reorder
    module procedure reorder_r, reorder_c
  endinterface

  interface submatrix2by2
    module procedure submatrix2by2_r, submatrix2by2_c
  endinterface

  interface swap
    module procedure swap_i, swap_r
  endinterface

!   interface power_method
!     module procedure power_method_r, power_method_c
!   endinterface

  ! for the 2x2 submatrix diagonalization
  real, parameter, private    :: TINYRATIO = 2.0E-16
  integer, parameter, private :: CG_MAXITER_HARDLIMIT = 1000

  contains

! end of head part
#endif


#if R1_C2 == 1
#define REAPLEX real
#else
#define REAPLEX complex
#endif


#if R1_C2 == 1
  logical function cg_eig_solve_r( &
#else
  logical function cg_eig_solve_c( &
#endif
      atm, g, kp, &
      vloc, jspin, energy, residual, threshold, maxiter, &
      s, Ss, ibnd, precond, show_energy ) result( converged )
  use configuration, only: STOPONERROR
  use type_grid, only: grid
  use type_atom, only: atom
  use type_kpoint, only: kpoint
  use operators, only: Hmt !, Smat
!   use operators, only: Precon, n_precon_n, c_precon
  use operators, only: Precon_Nf1
  use operators, only: scalar_product
  use MPItools, only: MPIallsum
  use unitsystem, only: eV, eV_

    type(atom), intent(in)          :: atm(:) ! atoms
    type(grid), intent(in)          :: g ! coarse grid
    type(kpoint), intent(in)        :: kp            ! kpoint
    real, intent(in)                :: vloc(:,:,:,:)
    integer, intent(in)             :: jspin
    real, intent(out)               :: energy(:)
    real, intent(out)               :: residual(:)
    real, intent(in)                :: threshold
    integer, intent(in)             :: maxiter
    integer, intent(in), optional   :: ibnd !! CG only for this band, if present
    logical, intent(in), optional   :: precond
    logical, intent(in), optional   :: show_energy

    REAPLEX, intent(inout)          :: s (:,:) !    (nxyzs,nbnd)
    REAPLEX, intent(inout)          :: Ss(:,:) !    (nxyzs,nbnd)

    character(len=*), parameter     :: fun = ' e_solve: '
                                       ! number of orthogonalization steps:
                                       ! northo(1) : the wavefunction before
                                       ! northo(2) : the (preconditioned) gradient
                                       ! northo(3) : the conjugate gradient
    integer, parameter              :: northo(1:3) = (/1,0,1/)
    logical, parameter              :: use_cg = .true.

    REAPLEX, allocatable, save      :: a    (:)  ! (nbnd)
    REAPLEX, allocatable, save      :: Hs   (:,:)  ! (nxyzs)
    REAPLEX, allocatable, save      :: grd  (:,:)  ! (nxyzs)
    REAPLEX, allocatable, save      :: Pgrd (:,:)  ! (nxyzs)
    REAPLEX, allocatable, save      :: SPgrd(:,:)  ! (nxyzs)
    REAPLEX, allocatable, save      :: HPgrd(:,:)  ! (nxyzs)
    REAPLEX, allocatable, save      :: dir  (:,:)  ! (nxyzs)
    REAPLEX, allocatable, save      :: Sdir (:,:)  ! (nxyzs)
    REAPLEX, allocatable, save      :: con  (:,:)  ! (nxyzs)
    REAPLEX, allocatable, save      :: Scon (:,:)  ! (nxyzs)
    REAPLEX, allocatable, save      :: Hcon (:,:)  ! (nxyzs)

    REAPLEX                         :: sHc, alpha, snorm
    real                            :: sHs, cHc, beta, gamma

    logical                         :: precondition
    logical                         :: show_e  ! show energy
    real                            :: res_old, res_new, f
    real                            :: snrm
    logical, save                   :: temp_arrays_allocated = .false.
    logical                         :: run
    real                            :: threshold2


    integer                         :: ib, jb, nbnd, natm, nxyzs!, naprj
    integer                         :: iiter, iortho
    integer                         :: i, ist, ib_start, ib_end


    precondition = .true. ; if( present( precond ) ) precondition = precond
#ifdef DEBUG
    show_e = .true.
#else
    show_e = .false.
#endif
    if( present( show_energy ) ) show_e = show_energy

!     precondition = .false.
!     precondition = .true.
!     if(o>0) write(o,'(3A,L2)') sym, fun, 'gradient preconditioning =', precondition

    nbnd = size( s, 2 )
    natm = size( atm, 1 )
#ifdef DEBUG
    nxyzs = product( g%ng(1:4) )
    if(size(Ss,2)/=nbnd) stop 'cg_eig_solve: dim #2 of SS does not match dim #2 of S.'
    if(size(s ,1)/=nxyzs) stop 'cg_eig_solve: dim #1 of S does not match the grid.'
    if(size(Ss,1)/=nxyzs) stop 'cg_eig_solve: dim #1 of SS does not match the grid.'
    ! if(o>0) write(o,'(3A,I10)') sym, fun, 'number of bands =', nbnd
#endif


    threshold2 = threshold**2 * g%svol
    
    if( temp_arrays_allocated .eqv. .false. ) then
      ! arrays have to be allocated
      ! (only the 1st time)
      nxyzs = product( g%ng(1:4) )
      i = 0
      allocate( a(nbnd), stat=ist ); i=i+ist
      allocate( Hs(nxyzs,1), stat=ist ); i=i+ist
      allocate( grd  (nxyzs,1), stat=ist ); i=i+ist
      allocate( Pgrd (nxyzs,1), stat=ist ); i=i+ist
      allocate( SPgrd(nxyzs,1), stat=ist ); i=i+ist
      allocate( HPgrd(nxyzs,1), stat=ist ); i=i+ist
      allocate( dir  (nxyzs,1), stat=ist ); i=i+ist
      allocate( Sdir (nxyzs,1), stat=ist ); i=i+ist
      allocate( con  (nxyzs,1), stat=ist ); i=i+ist
      allocate( Scon (nxyzs,1), stat=ist ); i=i+ist
      allocate( Hcon (nxyzs,1), stat=ist ); i=i+ist
      
      if( i == 0 ) then
        temp_arrays_allocated = .true.
! #ifdef DEBUG
        if(o>0) write(o,'(3A,F0.3,9A)') sym, fun, 'CG arrays allocated: ', R1_C2 * (nbnd+10*nxyzs) * 8./2.**20, ' MiByte'
! #endif
      else  ! sum of all istatus == 0
        if(o>0) write(o,'(3A,2I10,9A)') sym, fun, 'allocation failed, [nxyzs,naprj,nbnd] =[', nxyzs, nbnd, '].'
        stop 'cg_eig_solve: allocation failed.'
      endif ! sum of all istatus == 0

    endif ! not(temp_arrays_allocated)


    dir = 0.
    Sdir = 0.

    ib_start =  1
    ib_end = nbnd

    if( present( ibnd ) ) then
      !write (*,*) 'we have ibnd', ibnd
      ! only for the one wave function #ibnd
      ib = min(max(1,ibnd),nbnd)
      ib_start = ib
      ib_end   = ib
    endif ! present( ibnd )

    do ib = ib_start, ib_end

      ! update |Ss(ib)>, the memory of the adjoint wave functions is shared among the
      ! different sk-states, so always update before using it
      ! |Ss(ib)> = \hat{S} |s(ib)>
!       call Smat( g, atm, naprj, kp, ket=s(:,ib), Sket=Ss(:,ib) )!, c_out=c(:,ib) )
      call Hmt( g, vloc, jspin, atm, kp, ket=s(:,ib:ib), Hket=Hs(:,1:1), Sket=Ss(:,ib:ib) )!, cket=c(:,ib:ib) )


      do iortho = 1, northo(1)
        ! orthogonalize( ... )

        ! orthogonalize the wave function |s(ib)>
        ! against all lower wave functions
        do jb = 1, ib-1
          ! a(jb) = <Ss(jb)|s(ib)>
          a(jb) = scalar_product( Ss(:,jb), s(:,ib), h3=g%hvol &
                                   , comm=g%comm ) ! ==> collect communication
!                                 )
        enddo ! jb
        !if(ib>1) call MPIallsum( a(1:ib-1), g%comm ) ! communication collected

        do jb = 1, ib-1
          ! |s(ib)>  = |s(ib)>  - a(jb)* |s(jb)>
          ! |Ss(ib)> = |Ss(ib)> - a(jb)*|Ss(jb)>
           s(:,ib) =  s(:,ib) - a(jb)* s(:,jb)
          Ss(:,ib) = Ss(:,ib) - a(jb)*Ss(:,jb)
        enddo ! jb

        ! normalize |s(ib)>, scale |Ss(ib)> simultaneously

        ! snorm = <Ss(ib)|s(ib)>
        snorm = scalar_product( Ss(:,ib), s(:,ib), h3=g%hvol, comm=g%comm )
        snrm = real( snorm )
        if( snrm > 1.E-12 ) then
          f = 1.0/sqrt( snrm )
        else
          if(o>0) write(o,'(3A,I6)') sym, fun, 'norm is close to zero for band i#', ib
          if( STOPONERROR ) stop 'cg_eig_solve: norm close to zero or negative. cannot normalize.'
        endif !
        ! |s(ib)>  =  |s(ib)>/sqrt(snorm)
        s(:,ib)  =  s(:,ib) * f
        ! |Ss(ib)> = |Ss(ib)>/sqrt(snorm)
        Ss(:,ib) = Ss(:,ib) * f

      enddo ! iortho


      ! |Hs(ib)> = \hat{H} |s(ib)>
      call Hmt( g, vloc, jspin, atm, kp, ket=s(:,ib:ib), Hket=Hs(:,1:1), Sket=Ss(:,ib:ib) )

#ifdef NaN_SEARCH
  if( any( s(:,ib)/= s(:,ib)) ) stop 'NaN in  s! (1st)'
  if( any(Ss(:,ib)/=Ss(:,ib)) ) stop 'NaN in Ss! (1st)'
  if( any(Hs/=Hs) ) stop 'NaN in Hs! (1st)'
#endif


      res_new = 1.
      dir  = 0.
      Sdir = 0.
      ! clear arrays

      iiter = 0

      ! CG-loop
      run = ( iiter < maxiter )

      if( .not. run ) then
        ! evaluate only the energy, so the display is correct
        ! energy(ib) = <s(ib)|Hs(ib)>
        energy(ib) = real( scalar_product( s(:,ib), Hs(:,1), h3=g%hvol, comm=g%comm ) )
      endif ! no run

      do while( run )
        iiter = iiter + 1

        ! pass over the residual of the last iteration
        res_old = res_new


        ! energy(ib) = <s(ib)|Hs(ib)>
        energy(ib) = real( scalar_product( s(:,ib), Hs(:,1), h3=g%hvol, comm=g%comm ) )
        if( energy(ib) /= energy(ib) ) stop 'cg_eig_solve: energy is NaN'

        ! the residual vector (gradient) that has to be minimized
        ! |grd> = - [ |Hs(ib)> - energy(ib)*|Ss(ib)> ]
        grd = - Hs + energy(ib) * Ss(:,ib:ib)
!         if(o>0) write(o,'(3A,999ES10.2E3)') sym, fun, '|grd> =', grd

        if( precondition ) then
          ! |Pgrd> = \hat{P} |grd>
          call Precon_Nf1( g, kp=kp, v=grd(:,1), Pv=Pgrd(:,1) )
!           stop 'cg_eigensolver: Precon_Nf1 has not been upgraded to rank2'
#ifdef NaN_SEARCH
  if( any(Pgrd/=Pgrd) ) stop 'NaN in Pgrd! (0th)'
#endif
        else  ! precondition
          ! |Pgrd> = |grd>
          Pgrd = grd
        endif ! precondition

        do iortho = 1, northo(2)

        ! orthogonalize the residual vector
          ! against all lower wave functions and wave function ib
          do jb = 1, ib
            ! a(jb) = <Ss(jb)|Pgrd>
            a(jb) = scalar_product( Ss(:,jb), Pgrd(:,1), h3=g%hvol &
                                             , comm=g%comm )
!                                            )
          enddo ! jb
!          call MPIallsum( a(1:ib), g%comm ) ! communication collected
          ! possible ALLREDUCE
          do jb = 1, ib
            ! |Pgrd> = |Pgrd> - a(jb)*|s(jb)>
            Pgrd = Pgrd - a(jb) * s(:,jb:jb)
          enddo ! jb

        enddo ! iortho

        ! |SPgrd> = \hat{S} |Pgrd>
        call Hmt( g, vloc, jspin, atm, kp, ket=Pgrd, Hket=HPgrd, Sket=SPgrd )
#ifdef NaN_SEARCH
  if( any(Pgrd/=Pgrd) ) stop 'NaN in Pgrd! (1st)'
  if( any(SPgrd/=SPgrd) ) stop 'NaN in SPgrd! (1st)'
#endif

        ! res_new = <grd|SPgrd>
        res_new = real( scalar_product( grd(:,1), SPgrd(:,1), h3=g%hvol, comm=g%comm ) )

        ! store residual information
        residual(ib) = abs(res_new)

        if( use_cg ) then
          ! conjugate gradients method
          if( abs(res_new) > TINY(res_new) ) then
            f = res_old/res_new
          else  ! |res_new| > TINY
            f = 0.
          endif ! |res_new| > TINY
          !!! |dir> and |Sdir> will be used the next iteration

          ! conjugate gradient method

          ! |dir> = |Pgrd> + res_old/res_new* |dir>
          dir   = Pgrd   + f *   dir
          ! |Sdir> = |SPgrd> + res_old/res_new* |Sdir>
          Sdir  = SPgrd  + f *  Sdir

          ! |con> = |dir>
          con   =   dir
          ! |Scon> = |Sdir>
          Scon  =  Sdir

        else ! use_cg

          ! steepest descent method

          ! |con> = |Pgrd>
          con   =   Pgrd
          ! |Scon> = |SPgrd>
          Scon  =  SPgrd

        endif ! use_cg

        do iortho = 1, northo(3)
          ! orthogonalize the conjugate vector
          ! against all lower wave functions
          do jb = 1, ib
            ! a(jb) = <Ss(jb)|con>
            a(jb) = scalar_product( Ss(:,jb), con(:,1), h3=g%hvol &
                                      , comm=g%comm )
!                                     )
          enddo ! jb
!          call MPIallsum( a(1:ib), g%comm ) ! communication collected

          do jb = 1, ib

            ! |con>  = |con>  - a(jb)*|s(jb)>
            con   =  con  - a(jb) *  s(:,jb:jb)
            ! |Scon> = |Scon> - a(jb)*|Ss(jb)>
            Scon  =  Scon - a(jb) * Ss(:,jb:jb)

          enddo ! jb

        enddo ! iortho

        ! snorm = <con|Scon>
        snorm = scalar_product( con(:,1), Scon(:,1), h3=g%hvol, comm=g%comm )
        snrm = real( snorm )

        if( snrm < 1.E-12 ) then
          converged = .true.
        else ! snrm < 1.E-12

          f = 1.0/sqrt( snrm )
          ! |con>  = |con> /sqrt(snorm)
          con   =   con * f
          ! |Scon> = |Scon>/sqrt(snorm)
          Scon  =  Scon * f

#ifdef NaN_SEARCH
  if( any(con/=con) ) stop 'NaN in con! (1st)'
#endif
          ! |Hcon> = \hat{H} |con>
          call Hmt( g, vloc, jspin, atm, kp, ket=con, Hket=Hcon, Sket=Scon )

#ifdef NaN_SEARCH
  if( any(Hcon/=Hcon) ) stop 'NaN in Hcon! (1st)'
#endif

          sHs = energy(ib) ! == <s(ib)|Hs(ib)>
          ! sHc = <s(ib)|Hcon> or <Hs(ib)|con>
          sHc = scalar_product( s(:,ib), Hcon(:,1), h3=g%hvol, comm=g%comm )
          ! cHs = sHc* (not required because matrix is hermitian/symmetric)
          ! cHc = <con|Hcon>
          cHc = real( scalar_product( con(:,1), Hcon(:,1), h3=g%hvol, comm=g%comm ) )

          ! diagonalize the 2x2 matrix
          ! ( sHs sHc )
          ! ( cHs cHc ) and find the lower eigenvalue gamma
          ! as well as the components alpha and beta of the
          ! corresponding eigenvector
          gamma = submatrix2by2( sHs, cHc, sHc, alpha, beta )

          ! update |s(ib)>
          ! |s(ib)> = alpha*|s(ib)> + beta*|con>
          ! |Hs(ib)> = alpha*|Hs(ib)> + beta*|Hcon>
          ! |Ss(ib)> = alpha*|Ss(ib)> + beta*|Scon>
          s(:,ib:ib) = alpha* s(:,ib:ib) + beta* con
          Hs = alpha*Hs + beta*Hcon
          Ss(:,ib:ib) = alpha*Ss(:,ib:ib) + beta*Scon

          converged = ( res_new > threshold2 )

        endif ! snrm < 1.E-12

        run = ( iiter < maxiter ) .and. .not. converged
      enddo ! while run

      !show_e = .true.
      if( show_e ) then
        jb = ib ; if( present( ibnd ) ) jb = ibnd
        if(o>0) write(o,'(3A,I6,I2,I6,A,F16.9,A,ES13.4E2,A)') sym, fun, &
          'band(', jb, jspin, kp%jk, ' ) E=', energy(ib)*eV, ' +/-', residual(ib)*eV, eV_
        
      endif ! show_e

    enddo ! ib

  endfunction ! cg_eig_solve





! Here the double preprocessing is used
! for int/real instead of real/cmplx
#if R1_C2 == 1
  elemental subroutine swap_i( a, b )
    integer, intent(inout) :: a, b
    integer :: c ! temp
#else
  elemental subroutine swap_r( a, b )
    real, intent(inout) :: a, b
    real :: c ! temp
#endif
    c = a ; a = b ; b = c
  endsubroutine ! swap





#if R1_C2 == 1
  subroutine reorder_r( &
#else
  subroutine reorder_c( &
#endif
                        en, s, energytolerance )
    real, intent(inout)             :: en(:) ! energies en(nb)
    REAPLEX, intent(inout)          :: s(:,:) ! wave functions s(nxyzs,nbnd)
    real, intent(in), optional      :: energytolerance

    character(len=*), parameter     :: fun = ' reorder: '
    integer, parameter              :: NOCHANGE = 0
    integer, parameter              :: UNCHANGED = 0
    integer, parameter              :: CHANGED   = 1
    integer, parameter              :: EXITFLAG  = 10
    REAPLEX, allocatable            :: s_store(:) ! wave functions s(nxyzs,nbnd)
!     REAPLEX, allocatable            :: c_store(:) ! wave function coefficients c(naprj,nbnd)
    real                            :: etol
    real                            :: en_store
    real                            :: enc(size(en))

    integer                         :: nb ! number of bands
    integer                         :: ib, ib0, ii, ibstart, ind_store, ifree, iperm!, itmp
    integer                         :: flag
    integer, allocatable            :: ind(:) ! index list
    integer, allocatable            :: inv(:) ! inverted index list
!     integer                         :: indswap
    logical                         :: ordered

    integer                         :: nxyzs

    etol = 0.
    if( present( energytolerance ) ) etol = abs( energytolerance )

    ! number of bands
    nb = size(en,1)

    ! first check if the energies are ordered (within tolerance)
    ordered = .true.
    do ib= 2, nb
      ordered = ordered .and. ( en(ib) > en(ib-1) - etol )
    enddo ! ib

    if( ordered ) then
#ifdef DEBUG
      if(o>0) then
        write(o,'(9A)') sym, fun, 'no re-ordering needed.'
      endif ! o/=0
#endif
      return ! early return
    endif ! ordered


    ! list has to be sorted

    if(o>0) then
      write(o,'(3A,99I4)')   sym, fun, 'position  : ', ( ib, ib=1,nb )
      write(o,'(3A,99F4.1)') sym, fun, 'input     : ', ( en(ib), ib=1,nb )
    endif ! o/=0

    ! create an index list
    allocate( ind(1:nb) )

    do ib= 1, nb
      ! in original (old) ordering
      ind(ib) = ib  ! ind(1:3...) = (/1,2,3,.../)
      enc(ib) = en(ib) ! copy
    enddo ! ib

    ! bubble sort the index list, until en(ind(:)) is in ascending order

    flag = CHANGED ! assume that the en vector is unordered
    do while( flag == CHANGED )
      ib0 = 1
      do while( ib0 < nb )
        ib0 = ib0 + 1
!         if( en(ind(ib0)) < en(ind(ib0-1)) ) then
        if( enc(ib0) < enc(ib0-1) ) then
          call swap( ind(ib0), ind(ib0-1) ) ! exchange entry ib0 and ib0-1 in the index list
          call swap( enc(ib0), enc(ib0-1) ) ! exchange entry ib0 and ib0-1 in the energy list copy
          ib0 = nb + EXITFLAG ! stop the inner while loop
        endif ! en < en
      enddo ! while ib0 < nb
      if( ib0 == nb ) then
        flag = UNCHANGED
#ifdef DEBUG
! unneccessary  ---------------
      elseif( ib0 == nb + EXITFLAG ) then
        flag = CHANGED ! assume that the en vector is unordered
      else ; stop 'REO: fatal error in bubble sort: ib0 may only come out as ib0=nb or ib0=nb+EXITFLAG.'
! -----------------------------
#endif
      endif ! ib0 == nb
    enddo ! while ( flag = CHANGED )

    if(o>0) then
      write(o,'(3A,99I4)') sym, fun, 'index list: ', ( ind(ib), ib=1,nb )
      write(o,'(3A,99F4.1)') sym, fun, 'energies s: ', ( en(ind(ib)), ib=1,nb )
#ifdef DEBUG
      do ib= 2, nb
        if( en(ind(ib)) < en(ind(ib-1)) ) then
          write(o,'(3A,I6,A,I6,A,I6,A)') sym, fun, 'list sort failed. E(',ind(ib-1),') > E(',ind(ib),')'
        endif ! en > en
      enddo ! ib
#endif
    endif ! o/=0


    ! invert the permutation
    allocate( inv(1:nb) )
    inv = inverse_permutation( n=nb, per=ind )

    ind = inv

    if(o>0) write(o,'(3A,99I4)') sym, fun, 'invex list: ', ( inv(ib), ib=1,nb )
    deallocate( inv )

    nxyzs = size( s, 1 )
!     naprj = size( c, 1 )

    allocate( s_store(nxyzs) )!, c_store(naprj) ) ! temporary slot


    flag = CHANGED
    do while( flag == CHANGED )

      ibstart = 0
      ib = 0
      ! search for the first unordered element in inv
      do while( ib < nb )
        ib = ib + 1
        if( ind(ib) /= ib ) then ! ordered indices
          ibstart = ib
          ib = nb ! exit the loop
        endif ! ind(ib) /= ib
      enddo ! ib

      if( ibstart == 0 ) then
        flag = UNCHANGED
        exit ! exit the do while ( flag == CHANGED ) loop
      endif ! ibstart == 0

      ! copy the first unordered entry to the store
      ! en(ibstart) is stored and can thus be overwritten in en(:)
      en_store     = en(ibstart)
      s_store(:)   = s(:,ibstart)
!       c_store(:)   = c(:,ibstart)
      ind_store    = ind(ibstart)
      en(ibstart)  = 0. ! redundant
      ind(ibstart) = 0

      ifree        = ibstart ! en(ifree) may be overwritten without losing data

! #ifdef DEBUG
!       write(*,'(3A,I6)') sym, fun, 'ibstart =', ibstart
!       write(*,'(3A,I4,A,99I4)') sym, fun,'ind store: ', ind_store
!       write(*,'(3A,F4.1,A,99F4.1)') sym, fun,'en  store: ',en_store
!       write(*,'(3A,I6)') sym, fun, ' ifree = ', ifree
! #endif

      iperm = -1
      do while( iperm < 0 )

        ii = find_position_of_e_in_array( e=ifree, n=nb, a=ind )
        if( ii < 1 ) then
          if(o>0) write(o,'(3A,I6)') sym, fun, 'error: ii= ', ii
          stop 'reorder: fatal error.'
        endif ! ii < 1
      
!        write(*,'(3A,I6,A,I6)') sym, fun, ' ind(ii) == ifree , ii=', ii, ' ifree=', ifree

        if( ii /= ind_store ) then
          ! solve one permutation
! #ifdef DEBUG
!           write(*,'(3A,I4,A,99I4)') sym, fun,'ind store: ', ind_store,', index list: ', ( ind(ib),ib=1,nb )
!           write(*,'(3A,F4.1,A,99F4.1)') sym, fun,'en  store: ',en_store,', energies:   ',( en(ib),ib=1,nb )
!           write(*,'(3A,I4,A,I4)') sym, fun,'forwarding operation : ii=', ii, ', ifree=', ifree
! #endif
          en(ifree)    = en(ii)
          s(:,ifree)   = s(:,ii)
!           c(:,ifree)   = c(:,ii)
          ind(ifree)   = ind(ii) ! = ifree
          en(ii)       = 0. ! redundant
          ind(ii)      = 0
! #ifdef DEBUG
!           write(*,'(3A,I4,A,99I4)') sym, fun,'ind store: ', ind_store,', index list: ', ( ind(ib),ib=1,nb )
!           write(*,'(3A,F4.1,A,99F4.1)') sym, fun,'en  store: ',en_store,', energies:   ',( en(ib),ib=1,nb )
! #endif
          ! new free slot
          ifree = ii
          iperm = iperm - 1
        else  ! ii /= ind_store
          ! close the cycle of permutations
! #ifdef DEBUG
!           write(*,'(3A,I4,A,99I4)') sym, fun,'ind store: ', ind_store,', index list: ', ( ind(ib),ib=1,nb )
!           write(*,'(3A,F4.1,A,99F4.1)') sym, fun,'en  store: ',en_store,', energies:   ',( en(ib),ib=1,nb )
!           write(*,'(3A,I4,A,I4)') sym, fun,'closing operation : ii=', ii, ', ifree=', ifree
! #endif
          en(ifree)    = en(ii)
          s(:,ifree)   = s(:,ii)
!           c(:,ifree)   = c(:,ii)
          ind(ifree)   = ind(ii) ! = ifree
          en(ii)       = en_store
          s(:,ii)      = s_store(:)
!           c(:,ii)      = c_store(:)
          ind(ii)      = ind_store
          en_store     = 0. ! redundant
          ind_store    = 0
! #ifdef DEBUG
!           write(*,'(3A,I4,A,99I4)') sym, fun,'ind store: ', ind_store,', index list: ', ( ind(ib),ib=1,nb )
!           write(*,'(3A,F4.1,A,99F4.1)') sym, fun,'en  store: ',en_store,', energies:   ',( en(ib),ib=1,nb )
! #endif
          iperm = -iperm ! stop the inner while loop
          flag = CHANGED
        endif ! ii /= ind_store

! #ifdef DEBUG
!         write(*,'(3A,F4.1,A,99F4.1)') sym, fun,'---------------'
! #endif
          
      enddo ! while( iperm < 0 )

      if(o>0) write(o,'(3A,I0,A)') sym, fun, 'solved a ', iperm,'-fold permutation.'

    enddo ! while flag == CHANGED

! #ifdef DEBUG
!     write(*,'(3A,I6)') sym, fun, ' ibstart =', ibstart
! #endif

! #ifdef DEBUG
!     write(*,'(3A,I4,A,99I4)') sym, fun,'ind store: ', ind_store,', index list: ', ( ind(ib),ib=1,nb )
!     write(*,'(3A,F4.1,A,99F4.1)') sym, fun,'en  store: ',en_store,', energies:   ',( en(ib),ib=1,nb )
! #endif

    if(o>0) then
      do ib = 2, nb
        if( en(ib) < en(ib-1)  ) then
          write(o,'(3A,9(I0,A))') sym, fun, 'bubble sort failed. E(',ib-1,') > E(',ib,')'
        endif ! en > en
      enddo ! ib
    endif ! o>0
    deallocate( ind, s_store )!, c_store )
  endsubroutine ! reorder



#if R1_C2 == 2
! tail part

  integer function find_position_of_e_in_array( e, n, a )
    integer, intent(in) :: e, n, a(n)

    integer :: i

    find_position_of_e_in_array = -1
    do i= 1, n
      if( a(i) == e ) then
        find_position_of_e_in_array = i
        exit
      endif ! a(i) ++ e
    enddo ! i
  endfunction ! find_position_of_e_in_array

  function inverse_permutation( n, per )
    integer, intent(in) :: n
    integer, intent(in) :: per(n)
    integer             :: inverse_permutation(n) !! result

    integer :: i
    if( check_permutation( n, per ) ) then
      do i = 1, n
        inverse_permutation(per(i)) = i ! inversion
      enddo ! i
    else  ! check
      stop 'REO inverse_permutation: not a valid permutation of [1:N].'
    endif ! check
  endfunction ! inverse_permutation

!   subroutine invert_permutation( n, per, inv )
!   implicit none
!     ! arguments
!     integer, intent(in)       :: n
!     integer, intent(in)       :: per(n)
!     integer, intent(out)      :: inv(n)
!     ! local vars
!     integer                   :: i
! 
!     if( check_permutation( n, per ) ) then
!       ! inversion
!       do i = 1, n
!         inv(per(i)) = i
!       enddo ! i
! 
!     else ; stop 'REO invert_permutation: not a valid permutation of [1:N].'
!     endif ! check
! 
!   return
!   endsubroutine ! invert_permutation


  logical function check_permutation( n, per ) result( valid )
    integer, intent(in)       :: n
    integer, intent(in)       :: per(n)

    integer                   :: cnt(n), i
    cnt = 0
    do i = 1, n
      cnt(per(i)) = cnt(per(i)) + 1
    enddo ! i

    ! the permutation is OK,
    ! if each entry appears exactly once
    valid = ( all( cnt(1:n) == 1 ) )
  endfunction ! check_permutation



  !! diagonalization of the 2 by 2 matrix produces the normalized eigenvector [alpha, beta]
  real function submatrix2by2_r( sAs, pAp, sAp, alpha, beta  ) result( gamma )
    real, intent(in)      :: sAs, pAp ! the matrix is assumed symmetric
    real, intent(in)      :: sAp      ! the off-diagonal element(s)
    real, intent(out)     :: alpha, beta

    real :: tmp, sAppAs
    sAppAs = sAp*sAp  ! = |sAp|^2

    ! submatrix2by2_r =
    !   lowest eigenvalue of the 2 x 2 matrix ( sas , pas )
    !                                         ( sap , pap )
    gamma = 0.5*(sAs+pAp) - sqrt( 0.25*(sAs-pAp)*(sAs-pAp) + sAppAs )

    if( abs((sAs-gamma)/gamma) > TINYRATIO ) then
      tmp = 1.0/sqrt(sAppAs+(sAs-gamma)*(sAs-gamma))
      alpha = sAp*tmp
      beta = -(sAs-gamma)*tmp
    else ! abs...
      alpha = 1.0
      beta  = 0.0
    endif ! abs...
  endfunction ! submatrix2by2

  !! diagonalization of the 2 by 2 matrix produces the normalized eigenvector [alpha, beta]
  real function submatrix2by2_c( sAs, pAp, sAp, alpha, beta  ) result( gamma )
    real, intent(in)      :: sAs, pAp ! the matrix is assumed hermitian so the diagonal elements must be real
    complex, intent(in)   :: sAp      ! the off-diagonal element(s)
    complex, intent(out)  :: alpha
    real, intent(out)     :: beta

    real :: tmp, sAppAs
    sAppAs = real( sAp*conjg(sAp) ) ! = |sAp|^2

    ! submatrix2by2_c =
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


#ifdef EXTENDED
!+ extended

  integer function test( )
    write(*,*,iostat=test) __FILE__,' no module test implemented!'
  endfunction ! test

!- extended
#endif

endmodule ! cg_eigensolver
#endif
#endif

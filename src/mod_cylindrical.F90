#include "config.h"

! #define DEBUG
! #define FULL_DEBUG

! #define RADIAL_ROOT_GRID
#define PROGRESS


#define cDBG !DBG
#define cFDBG !FullDBG

#ifdef DEBUG
!!! remove comments from debug lines
#define cDBG

#ifdef FULL_DEBUG
!!! remove comments from full_debug lines
#define cFDBG
#endif

#endif

! documentation

!! @author FLEUR www.flapw.de
!! @author Paul Baumeister
!! @version 3.0
!! @see constants
!! @see type_grid
!! @see type_kpoint
!! @see harmonics
!! @see LAPACK
!! @see type_species
!! @see toolbox
!! @see type_atom
!!
!! this module computes the overlap matrix of atomic orbitals
!! given as radial functions times spherical harmonics for a given
!! atomic geometry. The overlap integrals are solved in a 
!! 2dimensional cylindrical coordinate system and the transformed
!! into the correct orientation using Wigner rotation matrices.
!!
!! *BUGS have been found*
!!
module cylindrical
#ifdef DEBUG
  use configuration, only: o ! output unit, 0: no output
#endif
implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: sym = 'CYL' !! module symbol

  public :: LCAO_coefficients
  public :: test

  interface LCAO_coefficients
    module procedure LCAO_coefficients_c
  endinterface

#ifndef DEBUG
  integer, parameter :: o=0 ! 0:no output
#endif

  !! cutoff for angular momentum quantum number ell
  integer, parameter           :: LMAX = 1 !! 1: only s and p states
  real, parameter              :: GRIDSPACING = 0.2

  interface invert3x3
    module procedure invert3x3_r
  endinterface

  interface determinant3x3
    module procedure determinant3x3_r
  endinterface

  interface Wigner_rotation
    module procedure matrix2Wigner_rotation, &
                     matrices2Wigner_rotation, &
                     Eulerian2Wigner_rotation
  endinterface

  interface Eulerian_angles
    module procedure matrix2Eulerian, vector2Eulerian
  endinterface

  interface show_matrix
    module procedure show_matrix_r, show_matrix_c
  endinterface

!!
!! In order to find a well-suited set of inital
!! wave functions from the given atomic wf''s we
!! preselect a linear combination of atomic
!! orbitals by making an LCAO ansatz
!! therefore, the integral of two functions
!! given as radial part times spherical harmonic
!! must be evaluated at arbitrary displacments
!! of their origins:
!!>           /
!!  I_{f,g} = |d^3r f(|r|)Ylm_f(vector r) * g(|r-R|)Ylm_g(vector (r-R))
!!<           /
!!
!!  these can be reduced to some integrals in cylider
!!  coordinates:
!!>            /   /
!!   I_{f,g} = |dz |rdr f(sqrt(r^2+z^2)) g(sqrt(r^2+(z-|R|)^2)) *
!!             /   /
!!              /2Pi
!!            * |dphi Ylm_f(r,z,phi) * Ylm_f(r,z-|R|,phi)
!!<             /0
!!
!!  the angular part reduces the number of integrals
!!  to be solved numerically on a grid in the z-r-plane
!!  drastically due to symmetries:
!!  the coordinate system is tranformed, such that the
!!  displacement vector R points into the z-direction.
!!  then certain symmetries set a lot of matrix elements to 0.
!!
!!>             ||   1 ||   2 |   3 |   4 ||   5 |   6 |   7 |   8 |   9 ||  10 |  11 |  12 |  13 |  14 |  15 |  16 ||
!!  ==================================================================================================================
!!    1 s       || SS0 ||     | SP0 |     ||     |     | SD0 |     |     ||     |     |     | SF0 |     |     |     ||
!!  ------------------------------------------------------------------------------------------------------------------
!!    2 px      ||     || PP1 |     |     ||     | PD1 |     |     |     ||     |     | PF1 |     |     |     |     ||
!!    3 pz      || SP0 ||     | PP0 |     ||     |     | PD0 |     |     ||     |     |     | PF0 |     |     |     ||
!!    4 py      ||     ||     |     | PP1 ||     |     |     | PD1 |     ||     |     |     |     | PF1 |     |     ||
!!  ------------------------------------------------------------------------------------------------------------------
!!    5 x2-y2   ||     ||     |     |     || DD2 |     |     |     |     ||     | DF2 |     |     |     |     |     ||
!!    6 zx      ||     || PD1 |     |     ||     | DD1 |     |     |     ||     |     | DF1 |     |     |     |     ||
!!    7 3z2-r2  || SD0 ||     | PD0 |     ||     |     | DD0 |     |     ||     |     |     | DF0 |     |     |     ||
!!    8 yz      ||     ||     |     | PD1 ||     |     |     | DD1 |     ||     |     |     |     | DF1 |     |     ||
!!    9 xy      ||     ||     |     |     ||     |     |     |     | DD2 ||     |     |     |     |     | DF2 |     ||
!!  ------------------------------------------------------------------------------------------------------------------
!!   10 xy      ||     ||     |     |     ||     |     |     |     |     || FF3 |     |     |     |     |     |     ||
!!   11 xy      ||     ||     |     |     || DF2 |     |     |     |     ||     | FF2 |     |     |     |     |     ||
!!   12 xy      ||     || PF1 |     |     ||     | DF1 |     |     |     ||     |     | FF1 |     |     |     |     ||
!!   13 xy      || SF0 ||     | PF0 |     ||     |     | DF0 |     |     ||     |     |     | FF0 |     |     |     ||
!!   14 xy      ||     ||     |     | PF1 ||     |     |     | DF1 |     ||     |     |     |     | FF1 |     |     ||
!!   15 xy      ||     ||     |     |     ||     |     |     |     | DF2 ||     |     |     |     |     | FF2 |     ||
!!   16 xy      ||     ||     |     |     ||     |     |     |     |     ||     |     |     |     |     |     | FF3 ||
!!  ==================================================================================================================
!!<
!!  so only 4 non-vanishing elements have to be computed for sp:
!!v   SS0, SP0, PP0, PP1
!!         10 non-vanishing elements have to be computed for spd:
!!v   SS0, SP0, SD0, PP0, PP1, PD0, PD1, DD0, DD1, DD2
!!         20 non-vanishing elements have to be computed for spdf:
!!v   SS0, SP0, SD0, SF0, PP0, PP1, PD0, PD1, PF0, PF1, DD0, DD1, DD2, DF0, DF1, DF2, FF0, FF1, FF2, FF3
!!  .. and 35 for spdfg

contains

  !! computes the coefficients of lowest energy solutions
  !! of the Linear Compbination of Atomic Orbitals for a
  !! given set of kpoints
  status_t function LCAO_coefficients_c( o, a, g, kpt ) result( ist ) !, awfc )
  use type_atom, only: atom
  use type_grid, only: grid, periodic_positions
  use LAPACK, only: diagonalize
  use type_kpoint, only: kpoint
#ifdef DEBUG_ALL
  use LAPACK, only: check_matrix
#endif
  use constants, only: Pi
  implicit none
    ! parameter
    character(len=*), parameter     :: fun = ' LCAO_coefficients: '
!     complex, parameter              :: im = (0.0,1.0)
    complex, parameter              :: im2Pi = (0.0,1.0)*2*Pi
    ! arguments
    integer, intent(in)             :: o !! output unit, 0:no output
    type(atom), intent(in)          :: a(:) !! list of *all* atoms
    type(grid), intent(in)          :: g !! grid descriptor, here only cell information are used
    type(kpoint), intent(in), optional :: kpt(:) !! set of kpoints
!     real, intent(out)               :: awfc(:,:,:) !! atomic wave function coefficients
    ! local vars
    complex, allocatable            :: awfc(:,:,:)
    complex, allocatable            :: Smat(:,:), vec(:,:)
    real, allocatable               :: Eval(:)
    real, allocatable               :: hist(:)
    real, allocatable               :: omat(:,:,:)
    real, allocatable               :: energies(:)
    complex                         :: eikL
    integer                         :: nk, ik ! for kpoints
    integer                         :: no, na, io
    integer                         :: ib, nb
    integer                         :: ni, ii, is(3,27) ! for images
    real                            :: kL(3), ori(3,27)

    na = size( a, 1 ) ! number of atoms 
    if( na < 1 ) return

!     ni = periodic_positions( g, (/0.,0.,0./), is, ori )
    ni = 1


    ist = LCAO_overlap( o, a, g, omat, energies )

    do ii = 1, size(omat,3)
      write(9,'(A,I6,A,3I3)') '# overlap matrix, periodic image #', ii, ' shifts =', is(1:3,ii)
      do io = 1, size(omat,2)
!         write(9,'(1001ES10.2)') omat(:,io,ii)      
        write(9,'(100F10.6)') omat(:,io,ii)
      enddo ! io
    enddo ! ii
    if(o>0) write(o,'(9A)') sym, fun, 'O-matrices written to fort.9'

!     if(o>0) write(o,'(9A)') sym, fun, 'O-matrix (00-image) diagonal:'
!     if(o>0) write(o,'(I6,ES24.16)') ( io, omat(io,io,1), io=1,size(omat,2) )


    nb = 4 ! number of bands
    no = size( omat, 1 ) ! number of orbitals
#ifdef DEBUG
    if( size( omat, 2 ) /= no ) stop 'CYL overlap matrix is not square!'
#endif


! the overlap matrices are not supposed to be symmetric
! #ifdef DEBUG_ALL
!     do ii = 1, size(omat,3)
!       ! check if the matrix is symmetric
!       ist = check_matrix( omat(:,:,ii), name='omat' )
!       if( ist /= 0 ) then
!         if(o>0) then
!           do ib = 1, no
!             write(o,'(1001ES16.6)') omat(:,ib,ii)
!           enddo ! ib
!         endif ! o/=0
!         stop 'LCAO: overlap matrix is not symmetric.'
!       endif ! ist /= 0
!     enddo ! ii
! #endif


    ! result array
!     allocate( awfc(no,nb,nk) ) ; awfc = 0. ! init

    ! matrix to be diagonalized and eigenvector array
    allocate( Smat(no,no), vec(no,no), Eval(no) )
!     allocate( hist(no) )

    if(o>0) write(o,'(2A,9(I0,A))') sym, fun, no, ' orbitals on', na, ' atoms'
    nk = 101
    if( present( kpt ) ) nk = size(kpt)

    if(o>0) write(o,'(2A,9(I0,A))') sym, fun, nk, ' kpoints'

    ni = maxval( a(:)%nimages ) ! number of images
    if(o>0) write(o,'(2A,9(I0,A))') sym, fun, ni, ' periodic images'

    do ik = 1, 1!nk ! go from Gamma to X in nk-1 steps
      if( present( kpt ) ) then
        kL = kpt(ik)%k
      else  ! present kpt
        kL = 0. ; kL(1) = 0.5 * real(ik-1)/real(nk-1) ! Gamma to X point
      endif

      ! ==================================================================
      ! set up the overlap matrix S
      ! ==================================================================
      Smat = 0. ! init
!       if(o>0) write(o,'(3A,I6,A,2F7.3)') sym, fun, 'k#', ik, ', Bloch factor', exp( im*kL_2Pi )
      do ii = 1, ni
        ! Bloch-factor when shifting to the periodic image
        ! x-direction (id=1)
        eikL = exp( im2Pi*sum( is(1:3,ii)*kL ) )
!         if(o>0) write(o,'(3A,I3,A,3I3,A,2F7.3)') sym, fun, 'i#', ii, ' is=', is(:,ii), ', eikL', eikL
        Smat = Smat + eikL * omat(:,:,ii)
      enddo ! ii
      ! ==================================================================

#ifdef DEBUG_ALL
!       if( any( aimag( Smat ) /= 0. ) ) stop 'LCAO: matrix has imaginary components!'
      ! check if the matrix is hermitian
      ist = check_matrix( Smat, name='LCAO' )
      if( ist /= 0 ) then
        if(o>0) then
          write(o,'(9A)') 'real (symmetric) part'
          do ib = 1, no
!             write(o,'(1001ES16.6)') Smat(:,ib)
            write(o,'(1001ES10.2)') real(Smat(:,ib))
          enddo ! ib
          write(o,'(9A)') 'imaginary (antisymmetric) part'
          do ib = 1, no
            write(o,'(1001ES10.2)') aimag(Smat(:,ib))
          enddo ! ib
        endif ! o/=0
        if(o>0) write(o,'(3A,I6,A,3F7.3)') sym, fun, 'error detected at kp#', ik, ' k=', kL
        stop 'LCAO: matrix is not hermitian/symmetric.'
      endif ! ist /= 0
#endif

      ! now diagonalize mat ...
      ist = diagonalize( Smat, vec, Eval )
      ! ... and copy the coefficients
      ! for the lowest nb eigenvalues
      ! into the awfc array
!       if(o>0) write(o,'(9A)') sym, fun, 'write eigenstates.'
!       if(o>0) then
!         do ib = 1, no !min(no,nb)
!           write(o,'(I3,A,F7.4,A,ES16.6,A,9999F8.4)') &
!             ib, '=ib, cos(k)=', real(eikL), ' eigval=', eval(ib), ' coeff=', vec(:,ib)
!         enddo ! ib
!       endif ! o/=0
      ! write to fort.7
!       if(o>0) write(o,'(9A)') sym, fun, 'write spectrum to fort.7'
      write(7,'(F10.6,1001ES16.6)') sqrt( sum(kL*kL)), Eval(:)
#ifdef DEBUG
!       do ib = 1, no
!         write(100+ib,'(1001ES16.6)') kL, vec(:,ib)
!       enddo ! ib
#endif
!     call simple_hist( data=eval, hist=hist )
!     if(o>0) write(o,'(9A)') sym, fun, 'write histogram to fort.8'
!     do ib = 1, no
!       write(8,'(10ES16.6E3)') hist(ib)
!       write(7,'(10ES16.6E3)') eval(ib)
!     enddo ! ib

    enddo ! ik
    stop 'LCAO_coefficients: xmgrace -nxy fort.7 &'

  endfunction ! LCAO_coefficients


  !! computes the overlap matrix of all atomic orbitals
  !! and all periodic images of the atoms
  !! and diagonalizes it using LAPACK
  status_t function LCAO_overlap( o, a, g, omat, ene_list ) result( ist )
  use type_atom, only: atom
  use type_grid, only: grid
  use type_grid, only: periodic_positions
  use toolbox, only: write_bmp_file, operator(+)
!   use type_species, only: radial_fun, radial_fun_max
  use type_species, only: SPIN, I_SMT
  use type_rgrid, only: operator(.at.)
  implicit none
    ! parameter
    character(len=*), parameter     :: fun = ' LCAO_overlap: '
    ! arguments
    integer, intent(in)             :: o !! output unit, 0: no output
    type(atom), intent(in)          :: a(:) !! list of *all* atoms
    type(grid), intent(in)          :: g !! grid descriptor
    real, allocatable, intent(out)  :: omat(:,:,:) !! dims(norb,norb,nimages) <br> overlap matrix
    real, allocatable, intent(out), optional :: ene_list(:) !! output of energies
    ! local vars
    integer                         :: na, ia, ia1, ia2
    integer                         :: iorb, norb, i1, i2, no, no1, no2
    integer                         :: ell1, ell2, iln1, iln2, ell
    integer, allocatable            :: ia_list(:)
    real                            :: pos1(3), pos2(3), r, Rvec(3)
    real                            :: r1max, r2max
    integer                         :: nimages = 1, iimage = 1
#ifdef PROGRESS
    integer                         :: iprog = 0
    real                            :: dprog, prog20th = 0., progress = 0.
#endif


    na = size( a, 1 ) ! number of atoms
    if( na < 1 ) return

    nimages = maxval( a(:)%nimages )
    if( minval( a(:)%nimages ) /= nimages ) &
      stop 'LCAO_overlap: atoms must have the same number of periodic images.'

    if(o>0) write(o,'(3A,I3)') sym, fun, 'number of periodic images', nimages

    norb = (LMAX+1)**2 * na

    ! create a list that says to which atom this orbital belongs
    allocate( ia_list(norb) )
    if( present( ene_list) ) allocate( ene_list(norb) )
    iorb = 0
    do ia = 1, na
      no = ( min(a(ia)%s%ellmax,LMAX) + 1 )**2
      if( present( ene_list) ) then
        do ell = 0, min(a(ia)%s%ellmax,LMAX)
          iln1 = 1
          ene_list(iorb+ell**2+1:iorb+(ell+1)**2) = a(ia)%s%ene_ln(1,iln1)
        enddo ! ell
      endif ! present
      ia_list(iorb+1:iorb+no) = ia
      iorb = iorb + no
    enddo ! ia

    if(o>0) write(o,'(3A,I6)') sym, fun, 'number of atomic orbitals', norb

    if( allocated( omat ) ) deallocate( omat )
    allocate( omat(norb,norb,nimages), stat=ist )
    if( ist /= 0 ) stop 'LCAO_overlap: failed to allocate OMAT.'
    omat = 0. ! init



#ifdef PROGRESS
    dprog = 1./real(norb*norb*nimages)
!     if(o>0) write(o,'(3A)',advance='no') sym, fun, 'progress (5%) ['
    if(o>0) write(o,'(3A)') sym, fun, 'progress (5%) ['
#endif

    do iimage = 1, nimages
      !----------------------------------------------
      i1 = 0
      do ia1 = 1, na

        ! pos1 = a(ia1)%pos ! the real position of atom1
        pos1 = a(ia1)%imagepos(:,iimage) ! the image position of atom1

        iln1 = 1
        do ell1 = 0, min(a(ia1)%s%ellmax,LMAX)
          no1 = 2*ell1+1 ! number of emm''s
          ! find the radial cutoff of the localized function
!           r1max = radial_fun_max( a(ia1)%s, iln1, threshold=0.001 )
!           stop 'CYL import radial function here! line 415'
          r1max = a(ia1)%s%g%rmx * 0.8

          !----------------------------------------------
          i2 = 0
#ifdef DEBUG
          ! no triangular loop
#define ia2_LIMIT na
          ! check if the resulting matrix is symmetric
#else
          ! triangular loop: exploit that omat(i,j)=omat(j,i),
#define ia2_LIMIT ia1
          ! therefore the inner atom index only runs op to ia1
#endif
          do ia2 = 1, ia2_LIMIT
            iln2 = 1

            pos2 = a(ia2)%pos ! the real position of atom2

            ! difference vector between the two radial centers
            Rvec = pos2 - pos1
#ifdef DEBUG
!             if(o/=0 .and. ell1==0) write(o,'(3A,3I3,A,2(3F10.3,A))') sym, fun, &
!               'ia2,ia1,ii', ia2, ia1, iimage, ' diff vector', Rvec, &
!               ' opposite', a(ia1)%pos - a(ia2)%imagepos(:,iimage)
#endif
            do ell2 = 0, min(a(ia2)%s%ellmax,LMAX)
              no2 = 2*ell2+1 ! number of emm''s

              ! find the radial cutoff of the localized function
!               r2max = radial_fun_max( a(ia2)%s, iln2, threshold=0.001 )
!               stop 'CYL radial_fun_max deactivated!'
              r2max = a(ia2)%s%g%rmx * 0.8

              omat( i2+1:i2+no2, i1+1:i1+no1, iimage ) = &
                orbital_overlap( o, Rvec, ell1, r1max, rfun1, ell2, r2max, rfun2 )
#ifdef DEBUG
!               if(o>0) write(o,'(3A,5I3,A,3F10.3,A,999F16.12)') sym, fun, &
!                 'ia2,ell2,ia1,ell1,ii', ia2, ell2, ia1, ell1, iimage, ' R=', Rvec, ' omat=', omat(i2+1,i1+1,iimage)!omat( i2+1:i2+no2, i1+1:i1+no1, iimage )
#endif


#ifdef PROGRESS
              if(o>0) then
                ! -- show progress bar ---------------------------
                iprog = iprog + no2*no1 ! progress for all
#if ia2_LIMIT == ia1
                if( ia1 /= ia2 ) iprog = iprog + no1*no2 ! double progress for off-diagonals
#endif
                progress = dprog*iprog
                do while( progress > prog20th )
                  prog20th = prog20th + 0.05
                  write(o,'(I3,A2)') nint( 100.*progress ) , ' %'
                enddo ! prog
                !----------------------------------------------
              endif ! o/=0
#endif

              i2   = i2   + no2
              iln2 = iln2 + a(ia2)%s%nn(ell2)
            enddo ! ell2
          enddo ! ia2
          !----------------------------------------------

          i1   = i1   + no1
          iln1 = iln1 + a(ia1)%s%nn(ell1)
        enddo ! ell1
      enddo ! ia1
      !----------------------------------------------

#if ia2_LIMIT == ia1
      ! symmetrize the overlap matrix
      do i1 = 1, norb
        do i2 = 1, i1-1
          omat( i2, i1, iimage ) = omat( i1, i2, iimage )
        enddo ! i2
      enddo ! i1
#endif

      ist = write_bmp_file( 'dmp/LCAO.'+iimage, abs(omat(:,:,iimage)), style='invert' )
    enddo ! iimage

#ifdef PROGRESS
    if(o>0) write(o,'(A)',advance='yes') ']'
#endif


  contains

    real function rfun1( r1 )
    implicit none
      real, intent(in)  :: r1
!       integer           :: i1
!       rfun1 = radial_fun( a(ia1)%s, iln1, r1 )
      rfun1 = a(ia1)%s%rwf( a(ia1)%s%g .at. r1 ,iln1,SPIN,I_SMT)
    endfunction

    real function rfun2( r2 )
    implicit none
      real, intent(in)  :: r2
!       integer           :: i2
!       rfun2 = radial_fun( a(ia2)%s, iln2, r2 )
      rfun2 = a(ia2)%s%rwf( a(ia2)%s%g .at. r2 ,iln2,SPIN,I_SMT)
    endfunction

  endfunction LCAO_overlap



  !! computes the overlap of two radially given functions
  !! times spherical harmonics centered at positions that differ by Rvec
  function orbital_overlap( o, Rvec, ell0, r0max, rfun0, ell1, r1max, rfun1 ) result( oovl )
  use harmonics, only: Ylm2Xlm
  use harmonics, only: dagger
  implicit none
    ! parameter
    character(len=*), parameter     :: fun = ' orbital_overlap: '
    ! arguments
    integer, intent(in)             :: o !! output unit, 0: no output
    real, intent(in)                :: Rvec(3) !! difference vector of positions
    integer, intent(in)             :: ell0 , ell1 !! ell quantum numbers
    real, intent(in)                :: r0max, r1max !! radial cutoff
    ! radial functions
    interface                     !! first radial function
      real function rfun0( r0 )
        real, intent(in) :: r0
      endfunction
    endinterface

    interface                    !! second radial function
      real function rfun1( r1 )
        real, intent(in) :: r1
      endfunction
    endinterface

    ! result
    real                            :: oovl(2*ell1+1,2*ell0+1) !! result: overlap of all 2*ell+1 orbitals of the one atom with the others <br> Note, this is not nessecarily a square matrix
    ! local vars
    integer                         :: emm
    real                            :: t, R, Eulerian(3)
    real                            :: ovl(-ell1:ell1,-ell0:ell0)
    complex                         :: tmp(2*ell1+1,2*ell0+1)
                                       ! rotation matrices
    complex                         :: rot0(-ell0:ell0,-ell0:ell0)
    complex                         :: rot1(-ell1:ell1,-ell1:ell1)
                                       ! transform into real Ylm
    complex                         :: c2r(-ell1:ell1,2*ell1+1)
    complex                         :: r2c(2*ell0+1,-ell0:ell0)

    ! length of the distance vector
    R = sqrt( sum( Rvec(1:3)**2 ) )

    oovl = 0.
    if( r0max + r1max < R ) return ! the spheres don''t intersect

    ! Eulerian angles of the distance vector
    Eulerian = Eulerian_angles( Rvec )
    Eulerian(3) = 0. ! the rotation around the new z-axis is not needed.

    ! compute the orbital overlap of two function given as
    ! radial function times real spherical harmonics at two
    ! different centers, difference vector R. Therefore we
    ! transform into the basis of the complex
    ! spherical harmonics |Yc_lm> and rotate such that both
    ! coordinate systems point along the the distance vector
    ! with their z-axis.

    ! Oovl = <Y_l0m|ovl|Y_l1m>
    !      =   <Y_l0m|Yc_l0m> *     ! real2complex
    !         <Yc_l0m|Yc_l0m''> *    ! rotation into the coordinate system
    !      <Yc_l0m''|ovl|Yc_l1m''> *  ! overlap (displacement points along z-axis)
    !        <Yc_l1m''|Yc_l1m> *     ! rotation back
    !         <Yc_l1m|Y_l1m>        ! complex2real
!     if(o>0) write(o,'(3A,I2,A,I2)') sym, fun, 'ell0 =', ell0, '  ell1 =', ell1

    rot0 =       ( Wigner_rotation( Eulerian, ell0 ) )
    rot1 = dagger( Wigner_rotation( Eulerian, ell1 ) )

    c2r  = dagger( Ylm2Xlm( ell1 ) )
    r2c  =       ( Ylm2Xlm( ell0 ) )

    ovl = 0.
    do emm = 0, min( ell0, ell1 )
      t = overlap( o, R, emm, ell0, r0max, rfun0, ell1, r1max, rfun1 )
      ovl( emm, emm) = t
      ovl(-emm,-emm) = t
    enddo ! emm

#ifdef DEBUG
    call show_matrix( o, r2c, 'r2c' )
    call show_matrix( o, rot0, 'rot0' )
    call show_matrix( o, ovl, 'ovl(along z-axis)' )
    call show_matrix( o, rot1, 'rot1' )
    call show_matrix( o, c2r, 'c2r' )
#endif
    ! ======= test ==============================================================

    tmp = matmul( c2r, matmul( rot1, matmul( ovl, matmul( rot0, r2c ) ) ) )

    oovl = real( tmp )
#ifdef DEBUG
    call show_matrix( o, tmp, 'orbital_overlap(complex)' )
    call show_matrix( o, oovl, 'orbital_overlap' )
#endif

  endfunction orbital_overlap




  !! ======= test ==============================================================
  function orbital_overlap_test( o, Rvec, ell0, ell1 ) result( oovl )
!   function orbital_overlap( o, Rvec, ell0, rf0, rg0, ell1, rf1, rg1 )
  use harmonics, only: Ylm2Xlm
  use harmonics, only: dagger
  implicit none
    ! parameter
    character(len=*), parameter     :: fun = ' orbital_overlap: '
    ! arguments
    integer, intent(in)             :: o
    real, intent(in)                :: Rvec(3)
    integer, intent(in)             :: ell0 , ell1
!     real, intent(in), optional      :: rf0(:), rf1(:)
!     real, intent(in), optional      :: rg0(:), rg1(:)
    ! result
    real                            :: oovl(2*ell1+1,2*ell0+1)
    ! local vars
    integer                         :: imx, ir, i
    real                            :: R, Eulerian(3)
    real                            :: ovl(-ell1:ell1,-ell0:ell0)
    complex                         :: tmp(2*ell1+1,2*ell0+1)
                                       ! rotation matrices
    complex                         :: rot0(-ell0:ell0,-ell0:ell0)
    complex                         :: rot1(-ell1:ell1,-ell1:ell1)
                                       ! transform into real Ylm
    complex                         :: c2r(-ell1:ell1,2*ell1+1)
    complex                         :: r2c(2*ell0+1,-ell0:ell0)

    ! ======= test ==============================================================
    real                            :: f0(0:1000), f1(0:444), dr0=0.01, dr1=0.01
#ifdef DEBUG
!     stop 'CYL DEBUG line 83'
#endif
    do ir = 0, 1000
      f0(ir) = exp(-dr0*ir)
    enddo ! ir
    do ir = 0, 444
      f1(ir) = exp(-2.*dr1*ir)
    enddo ! ir
    ! ======= test ==============================================================

    R = sqrt( sum( Rvec(1:3)**2 ) )
    Eulerian = Eulerian_angles( Rvec )
    Eulerian(3) = 0. ! the rotation around the new z-axis is not needed.

    ! compute the orbital overlap of two function given as
    ! radial function times real spherical harmonics at two
    ! different centers, difference vector R. Therefore we
    ! transform into the basis of the complex
    ! spherical harmonics |Yc_lm> and rotate such that both
    ! coordinate systems point along the the distance vector
    ! with their z-axis.

    ! Oovl = <Y_l0m|ovl|Y_l1m>
    !      =   <Y_l0m|Yc_l0m> *     ! real2complex
    !         <Yc_l0m|Yc_l0m''> *    ! rotation into the coordinate system
    !      <Yc_l0m''|ovl|Yc_l1m''> *  ! overlap (displacement along z-axis)
    !        <Yc_l1m''|Yc_l1m> *     ! rotation back
    !         <Yc_l1m|Y_l1m>        ! complex2real
    if(o>0) write(o,'(3A,I2,A,I2)') sym, fun, 'ell0 =', ell0, '  ell1 =', ell1

    rot0 =       ( Wigner_rotation( Eulerian, ell0 ) )
    rot1 = dagger( Wigner_rotation( Eulerian, ell1 ) )

    c2r  = dagger( Ylm2Xlm( ell1 ) )
    r2c  =       ( Ylm2Xlm( ell0 ) )

    ! ======= test ==============================================================
    ovl = overlap_itp( o, R, ell0=ell0, rf0=f0, dr0=dr0, &
                             ell1=ell1, rf1=f1, dr1=dr1 )
#ifdef DEBUG
    call show_matrix( o, r2c, 'r2c' )
    call show_matrix( o, rot0, 'rot0' )
    call show_matrix( o, ovl, 'ovl(along z-axis)' )
    call show_matrix( o, rot1, 'rot1' )
    call show_matrix( o, c2r, 'c2r' )
#endif
    ! ======= test ==============================================================

    tmp = matmul( c2r, matmul( rot1, matmul( ovl, matmul( rot0, r2c ) ) ) )

    oovl = real( tmp )
#ifdef DEBUG
    call show_matrix( o, tmp, 'orbital_overlap(complex)' )
    call show_matrix( o, oovl, 'orbital_overlap' )
#endif

  endfunction orbital_overlap_test




  function uniformradialgrid( irmx, dr, f, r )
  ! converts a radial exponential grid into a uniform grid with dr
  implicit none
    ! parameter
    character(len=*), parameter     :: fun = ' uniformradialgrid: '
    ! arguments
    integer, intent(in)             :: irmx
    real, intent(in)                :: dr, f(:), r(:)
    ! result
    real                            :: uniformradialgrid(0:irmx)
    ! local vars
    integer                         :: imx, ir, i
    real                            :: ri, rp, rm

    ir = 2
    do i = 0, irmx
      ri = i * dr
      do while( ri > r(ir) ) ; ir=ir+1 ; enddo ! while
      rp = ri - r(ir)
      rm = ri - r(ir-1)
      uniformradialgrid(i) = ( rm*f(ir) - rp*f(ir-1) )/( rm-rp )
    enddo ! i

  endfunction uniformradialgrid



  !! computes the overlap of all emm''s of two functions given as radial function
  !! times spherical harmonics centered at different sites, where the displacement
  !! points along the z-direction. Uses interpolation for the radial functions.
  function overlap_itp( o, R, ell0, rf0, dr0, &
                              ell1, rf1, dr1 )
  use constants, only: Pi
  implicit none
    ! parameter
    character(len=*), parameter     :: fun = ' cylindrical_overlap: '
!     real, parameter                 :: DEFAULT_DZ = 0.1
    ! arguments
    integer, intent(in)         :: o !! output unit, 0: no output
    real, intent(in)            :: R !! distance, length of the displacemnet vector, assumed >= 0.
    integer, intent(in)         :: ell0,    ell1 !! angular momentum quantum numbers
    real, intent(in)            :: rf0(0:), rf1(0:) !! radial functions sampled on an equidistant grid
    real, intent(in)            :: dr0,     dr1 !! grid spacings of the equidistant grids, recommended values 0.01 to 0.1
    ! result
    real                        :: overlap_itp(-ell1:ell1,-ell0:ell0) !! result: overlap matrix
    ! local vars
    integer                     :: emm
    integer                     :: ir0max, ir1max
    real                        :: r0max, r1max ! cutoff radius
    real                        :: dr0inv, dr1inv ! cutoff radius
    real                        :: t

    ir0max = size( rf0, 1 )-1    ;  ir1max = size( rf1, 1 )-1
    r0max = ir0max * dr0         ;  r1max = ir1max * dr1
    dr0inv = 1./dr0              ;  dr1inv = 1./dr1
#ifdef DEBUG
    if( ir1max < 1 .or. ir0max < 1  ) stop 'overlap_itp: needs at least 2 function elements.'
#endif

    overlap_itp = 0.
    do emm = 0, min(ell0,ell1)
      t    = overlap( o, R, emm, ell0, r0max, rfun0, &
                                 ell1, r1max, rfun1 )
      ! exploit the symmetry in emm
      overlap_itp( emm, emm) = t
      overlap_itp(-emm,-emm) = t
#ifdef FULL_DEBUG
      if(o>0) write(o,'(3A,3I3,ES16.6)') sym, fun, 'ell0,ell1,emm', ell0,ell1,emm, t
#endif
    enddo ! emm

  contains

    real function rfun0( r0 )
    implicit none
      real, intent(in)       :: r0 ! assumed positive
      if( r0 > r0max ) then
        rfun0 = 0.
      else  ! r > rmax
        rfun0 = rf0( int(r0*dr0inv) ) ! 0-th order interplolation
      endif ! r > rmax
    endfunction

    real function rfun1( r1 )
    implicit none
      real, intent(in)       :: r1 ! assumed positive
      if( r1 > r1max ) then
        rfun1 = 0.
      else  ! r > rmax
        rfun1 = rf1( int(r1*dr1inv) ) ! 0-th order interplolation
      endif ! r > rmax
    endfunction

  endfunction overlap_itp

  !! *core routine of this module*
  !!
  !! computes the overlap integral between two radial functions times
  !! reduced spherical harmonics in cylindrical coordinates exploiting
  !! the orthogonality of the emm-states analytically. 2dimensional
  !! integration in real-space over z (along the displacement) and
  !! rxy (perpendicular to the displacement)
  !!
  !! The overlap integral of two functions given as one single
  !! complex spherical harmonic function Ylm times a radial function rfun(r)
  !! is given by
  !!>  /
  !!   |d^3r rfun0(|r|)*Ylm0(r) * rfun1(|R-r|)*Ylm1(R-r)
  !!<  /V
  !! for simplicity we will rotate both coordinate systems such that
  !! the difference vector R points into positive z-direction. Then the
  !! angular integration over \phi gives a \delta_{emm0,emm1}
  !! and the perpendicular coordinates x and y can be unified to the
  !! distance from the axis rxy. 
  !!>
  !!                   ..-------..
  !!         ..-----../        r1 \
  !!        / r0     /\            \
  !! ------|-----+--|--|----+-------|--------> z-axis
  !!           -R/2        R/2
  !!<
  real function overlap( o, R, emm, ell0, r0max, rfun0, ell1, r1max, rfun1 )
  use constants, only: Pi
  implicit none
    ! parameter
    character(len=*), parameter     :: fun = ' cylindrical_overlap: '
    real, parameter                 :: DEFAULT_DZ = GRIDSPACING
    ! arguments
    integer, intent(in)         :: o ! output
    real, intent(in)            :: R ! distance, assumed positive
    integer, intent(in)         :: emm
    integer, intent(in)         :: ell0,  ell1
    real, intent(in)            :: r0max, r1max ! cutoff radius

    ! radial functions
    interface
      real function rfun0( r )
        real, intent(in) :: r
      endfunction
    endinterface

    interface
      real function rfun1( r )
        real, intent(in) :: r
      endfunction
    endinterface

    ! local vars
    real                        :: rxy, rxy2, rxymax, drxy, det
    real                        :: zmin, zmax, zoff, dz, mz
    real                        :: r0, r1
    real                        :: z, z0, z1, z02, z12
    real                        :: f0, f1
    integer                     :: iz, nzs, nze
    integer                     :: ir, nr, ir0, ir1
    real                        :: t
    integer                     :: ilm1, ilm0

    ! The overlap integral of two functions given as one single
    ! complex spherical harmonic function Ylm times a radial function rfun(r)
    ! is given by
    !  /
    !  |d^3r rfun0(|r|)*Ylm0(r) * rfun1(|R-r|)*Ylm1(R-r)
    !  /V
    ! for simplicity we will rotate both coordinate systems such that
    ! the difference vector R points into positive z-direction. Then the
    ! angular integration over \phi gives a \delta_{emm0,emm1}
    ! and the perpendicular coordinates x and y can be unified to the
    ! distance from the axis rxy. 
    ! z is unchanged.

    if( ell0 < 0 .or. ell1 < 0 ) stop 'CYL overlap: one or both ELL are < 0. unphysical.'

    overlap = 0.

    ! delta_emm0_emm1
    if( abs( emm ) > min( ell0, ell1 ) ) &
#ifdef DEBUG
    then
      if(o>0) write(o,'(3A)') sym, fun, '|emm| > min{ell0,ell1} is will give no overlap.'
      return
    endif ! |emm| > min{ell0,ell1}
#else
    return
#endif
    ! compute the ilm indices out of the emm and ell values
    ilm0 = ell0*(ell0+1) + emm + 1
    ilm1 = ell1*(ell1+1) + emm + 1

    ! now consider the cylindrical integral
    ! over z and rxy

    !                  ..-------..
    !        ..-----../        r1 \
    !       / r0     /\            \
    !------|-----+--|--|----+-------|--------> z-axis
    !          -R/2        R/2

    ! Situation 0: R > r0max+r1max
    !   --> the two spheres are disjoint
    !   ==> return 0.
    if( R > r0max + r1max ) &
#ifdef DEBUG
    then
      if(o>0) write(o,'(3A)') sym, fun, 'spheres are disjoint by', R-(r0max+r1max), ' a.u.'
      return
    endif ! R > r0max + r1max
#else
      return
#endif

    ! Situation 1: R <= |r0max-r1max|
    !   --> the two spheres have maximal overlap
    !   --> the smaller sphere is fully inside the larger sphere
    !  if( r1max > r0max ) then
    !    zmin = 0-r0max ; zmax = 0+r0max
    !  else
    !    zmin = R-r1max ; zmax = R+r1max
    !  endif

    ! ELSE
    ! Situation 2: |r0max-r1max| < R < r0max+r1max
    !   --> the two spheres have an overlap
!     zmin = max( R  - r1max, 0. - r0max )
!     zmax = min( 0. + r0max, R  + r1max )
    zmin = max(  0.5*R - r1max, -0.5*R  - r0max )
    zmax = min( -0.5*R + r0max,  0.5*R  + r1max )

    if( max( r1max, r0max ) > R + min( r1max, r0max ) ) then 
      ! simple solution:
      rxymax = min( r0max, r1max )
    else  !
      ! elliptic equation needed --> TODO
      rxymax = min( r0max, r1max ) ! this can be lower with an elliptic equation
    endif !

#ifdef RADIAL_ROOT_GRID
    ! the square root radial grid takes care of sampling outer points
    ! with a higher density than inner points.
    !
    ! r(ir) = sqrt( Rmax^2 * ir / nr )
    !
    ! and drdi(ir) = 1/2 sqrt( Rmax^2 / ( ir * nr ) )
    !
    ! such that the integral
    !            /Rmax
    !  I =  2 Pi |dr r dr ...  becomes  Pi Rmax^2 / nr sum( ..., ir=1,nr )
    !            /0
#endif

    dz = DEFAULT_DZ
    ! the center distance R should be a multiple of dz,
    ! such that both centers can lie exactly between two grid points


    nzs = nint( zmin/dz - 0.5 )
    nze = nint( zmax/dz + 0.5 )

#ifdef RADIAL_ROOT_GRID
    nr = nint( (rxymax/dz)**2 )
    drxy = rxymax**2/nr
#else
    drxy = dz
    nr = nint( rxymax/drxy )
#endif

!     if(o>0) write(o,'(3A,2F6.2,A,2I6,A)') sym, fun, &
!       '[zmin,zmax] = [', (/1,nz/)*dz+zoff, ' ], [nz,nr] = [', nz, nr, ' ]'

#ifdef DEBUG
    if(o>0) write(o,'(3A,2(I5,A))') sym, fun, &
      'use', nr, ' radial grid points and', nze-nzs+1, ' along the z-axis.'
#endif

    t = 0.
    do iz = nzs, nze
      z = iz * dz
      z0 = z+0.5*R
      z02 = z0*z0
      z1 = z-0.5*R
      z12 = z1*z1
      do ir = 1, nr

#ifdef RADIAL_ROOT_GRID
        rxy2 = drxy * ir
        rxy = sqrt( rxy2 )
!         det = 2. * Pi * rxy * dz * drxy
        ! 2 Pi dz is constant and can be moved out of the loop
        det = 0.5*drxy
#else
        rxy = drxy * (ir-0.5)
        rxy2 = rxy**2
!         det = 2. * Pi * rxy * dz * drxy
        ! 2 Pi dz drxy is constant and can be moved out of the loop
        det = rxy * drxy
#endif

        ! eval rfun0
        r0 = sqrt( rxy2 + z02 )
        f0 = rfun0( r0 ) * cyl_Ylm_rl( ilm0, rxy, z0 ) * r0**(-ell0)
        ! eval rfun1
        r1 = sqrt( rxy2 + z12 )
        f1 = rfun1( r1 ) * cyl_Ylm_rl( ilm1, rxy, z1 ) * r1**(-ell1)

        t = t + f0 * f1 * det
      enddo ! ir
#ifdef RADIAL_ROOT_GRID
      ! subtract half of the outermost (trapezoidal integration)
      t = t - 0.5 * f0 * f1 * det
#endif
    enddo ! iz

    ! constant factors
    det = 2. * Pi * dz

    overlap = t * det
#ifdef FULL_DEBUG
    if(o>0) write(o,'(3A,3I3,ES16.6)') sym, fun, 'ell0,ell1,emm', ell0,ell1,emm, overlap
#endif
  endfunction overlap


  !! complex spherical harmonics Y_{ell,emm}*R^ell in cylindrical coordinates (r,z)
  !! without the phi-dependence exp(i emm phi) --> therefore real
  real function cyl_Ylm_rl( ilm, rxy, z ) result( Ylm )
  use constants, only: Pi, ONESQRTFOURPI
  implicit none
    ! parameter
    character(len=*), parameter     :: fun = ' cyl_Ylm_rl(cylindrical): '
    real, parameter                 :: f = ONESQRTFOURPI ! sqrt(1./(4.*Pi))
    ! arguments
    integer, intent(in)             :: ilm !! ell-emm combindex
    real, intent(in)                :: rxy !! distance from the z-axis 
    real, intent(in)                :: z   !! z-coordinate

!   ! complex spherical harmonic functions
!   xpiy = (x+i*y)
!   xmiy = (x-i*y)

    selectcase( ilm )                                         ! ell emm

!   Ylmax_rl_c( 1) =  f                                       ! s     0
    case( 1) ; Ylm =  f*1.0                                   ! s     0

!   Ylmax_rl_c( 2) =  f*sqrt(1.5)*xmiy                        ! p    -1
!   Ylmax_rl_c( 3) =  f*sqrt(3. )* z                          ! p     0
!   Ylmax_rl_c( 4) = -f*sqrt(1.5)*xpiy                        ! p     1
    case( 2) ; Ylm =  f*sqrt(1.5)*rxy                         ! p    -1
    case( 3) ; Ylm =  f*sqrt(3. )* z                          ! p     0
    case( 4) ; Ylm = -f*sqrt(1.5)*rxy                         ! p     1

!   Ylmax_rl_c( 5) =  f*sqrt(1.875)*xmiy*xmiy                 ! d    -2
!   Ylmax_rl_c( 6) =  f*sqrt(7.5  )*xmiy*z                    ! d    -1
!   Ylmax_rl_c( 7) =  f*sqrt(1.25 )*(3.*z**2-r2)              ! d     0
!   Ylmax_rl_c( 8) = -f*sqrt(7.5  )*xpiy*z                    ! d     1
!   Ylmax_rl_c( 9) =  f*sqrt(1.875)*xpiy*xpiy                 ! d     2
    case( 5) ; Ylm =  f*sqrt(1.875)*rxy**2                    ! d    -2
    case( 6) ; Ylm =  f*sqrt(7.5  )*rxy*z                     ! d    -1
    case( 7) ; Ylm =  f*sqrt(1.25 )*(2*z**2-rxy**2)           ! d     0
    case( 8) ; Ylm = -f*sqrt(7.5  )*rxy*z                     ! d     1
    case( 9) ; Ylm =  f*sqrt(1.875)*rxy**2                    ! d     2

!   Ylmax_rl_c(10) =  f*sqrt( 2.1875)*xmiy2*xmiy              ! f    -3
!   Ylmax_rl_c(11) =  f*sqrt(13.125 )*xmiy2*z                 ! f    -2
!   Ylmax_rl_c(12) =  f*sqrt( 1.3125)*xmiy*(5.*z2-r2)         ! f    -1
!   Ylmax_rl_c(13) =  f*sqrt( 1.75  )*z*(5.*z2-3.*r2)         ! f     0
!   Ylmax_rl_c(14) = -f*sqrt( 1.3125)*xpiy*(5.*z2-r2)         ! f     1
!   Ylmax_rl_c(15) =  f*sqrt(13.125 )*xpiy2*z                 ! f     2
!   Ylmax_rl_c(16) = -f*sqrt( 2.1875)*xpiy2*xpiy              ! f     3
    case(10) ; Ylm =  f*sqrt( 2.1875)*rxy**3                  ! f    -3
    case(11) ; Ylm =  f*sqrt(13.125 )*rxy**2*z                ! f    -2
    case(12) ; Ylm =  f*sqrt( 1.3125)*rxy*(4*z**2-rxy**2)     ! f    -1
    case(13) ; Ylm =  f*sqrt( 1.75  )*z*(2*z**2-3*rxy**2)     ! f     0
    case(14) ; Ylm = -f*sqrt( 1.3125)*rxy*(4*z**2-rxy**2)     ! f     1
    case(15) ; Ylm =  f*sqrt(13.125 )*rxy**2*z                ! f     2
    case(16) ; Ylm = -f*sqrt( 2.1875)*rxy**3                  ! f     3

    case( :0 ) ; stop 'cyl_Ylm_rl: ILM index must be positive'
    case default ; Ylm = 0.
#ifdef DEBUG
     if(o>0) write(o,'(3A,I3)') sym, fun, 'warning: ELLMAX=3 but ilm=', ilm
#endif
    endselect ! ilm

  endfunction cyl_Ylm_rl



  !! determinant of a 3 by 3 matrix
  real function determinant3x3_r( a )
  implicit none
    ! parameter
    character(len=*), parameter     :: fun = ' determinant3x3: '
    ! arguments
    real, intent(in)                :: a(3,3)

    determinant3x3_r = &
          a(1,1)*a(2,2)*a(3,3) + a(1,2)*a(2,3)*a(3,1) &
        + a(2,1)*a(3,2)*a(1,3) - a(1,3)*a(2,2)*a(3,1) &
        - a(2,3)*a(3,2)*a(1,1) - a(2,1)*a(1,2)*a(3,3)

  endfunction determinant3x3_r

  !! inverts a 3 by 3 matrix, if the determinant is non-zero
  real function invert3x3_r( a, inv ) result( det ) !! result: determinant of matrix a
  implicit none
    ! parameter
    character(len=*), parameter     :: fun = ' invert3x3: '
    ! arguments
    real, intent(in)                :: a(3,3) !! matrix
    real, intent(out)               :: inv(3,3) !! inverse
    ! local vars

    det = determinant3x3( a )

    if( det == 0. ) then
#ifdef DEBUG
      if(o>0) write(o,'(9A)') sym, fun, 'determinant = 0.'
#endif
      inv = 0.
      return
    endif ! det == 0

    inv(1,1) = (a(2,2)*a(3,3)-a(2,3)*a(3,2))/det
    inv(1,2) = (a(1,3)*a(3,2)-a(1,2)*a(3,3))/det
    inv(1,3) = (a(1,2)*a(2,3)-a(2,2)*a(1,3))/det
    inv(2,1) = (a(2,3)*a(3,1)-a(2,1)*a(3,3))/det
    inv(2,2) = (a(1,1)*a(3,3)-a(3,1)*a(1,3))/det
    inv(2,3) = (a(1,3)*a(2,1)-a(1,1)*a(2,3))/det
    inv(3,1) = (a(2,1)*a(3,2)-a(2,2)*a(3,1))/det
    inv(3,2) = (a(1,2)*a(3,1)-a(1,1)*a(3,2))/det
    inv(3,3) = (a(1,1)*a(2,2)-a(1,2)*a(2,1))/det

  endfunction invert3x3_r


  !! converts a rotation vector to Eulerian angles
  function vector2Eulerian( v ) result( Eulerian )
  ! determine the Eulerian angles of the vector
  use constants, only: Pi
  implicit none
    ! parameter
    character(len=*), parameter     :: fun = ' vector2Eulerian: '
    real, parameter                 :: THRESHOLD = 1.E-5
    ! arguments
    real,    intent(in)         :: v(3)
    ! result
    real                        :: Eulerian(3)
    ! local vars
    real                      :: det, dmat(3,3)
    real                      :: cosc, sinc, cosb, sinb, cosa, sina
    real                      :: alpha, beta, gamma
    real                      :: rxy, x, y, z, r, f
    character(len=3)          :: deg

    ! find the polar angles of the vector v
    x = v(1) ; y = v(2) ; z = v(3)
    r = sqrt( sum( v(1:3)**2 ) )
    rxy = sqrt( sum( v(1:2)**2 ) )

    if( x /= 0. ) then
      alpha = atan( y/x )
    else  ! x /= 0.
      alpha = 0.5*Pi
      if( y == 0. ) alpha = 0.
    endif ! x /= 0.

    if( rxy /= 0. ) then
      beta = acos( z/r )
    else  ! rxy /= 0.
      beta = 0.
      if( z < 0. ) beta = Pi
    endif ! rxy /= 0.

    gamma = r ! length of the vector

#ifdef DEBUG
!     if( o /= 0 ) then
!       write(o,fmt='(3A)') sym, fun
!       write(o,fmt='(A)') '    Eulerian angles for the vector', &
!                          '    alpha     beta      gamma'
!       f= 180./Pi ; deg = '   ' ; deg(1:1) = achar(176) ! degrees
! !       f= 1./Pi ; deg = 'Pi ' ! Pi
! !       f= 1. ; deg = '   ' ! rad
!       write(o,fmt='(3(F8.3,A3))') alpha*f, deg, beta*f, deg, gamma*f, deg
!     endif ! o/=0
#endif

    Eulerian(1) = alpha
    Eulerian(2) =  beta
    Eulerian(3) = gamma

  endfunction vector2Eulerian

  !! converts a rotation matrix to Eulerian angles
  function matrix2Eulerian( mrot ) result( Eulerian )
  ! determine the Eulerian angles of the rotation matrix
  use constants, only: pi
  implicit none
    ! parameter
    character(len=*), parameter     :: fun = ' matrix2Eulerian: '
    real, parameter                 :: THRESHOLD = 1.E-5
    ! arguments
    real,    intent(in)       :: mrot(3,3)
    ! result
    real                      :: Eulerian(3)
    ! local vars
    logical, save             :: written = .false.

    real                      :: det, dmat(3,3)
    real                      :: cosc, sinc, cosb, sinb, cosa, sina
    real                      :: alpha, beta, gamma

      det = determinant3x3( mrot(:,:) )

      if( abs(1.0-abs(det)) > THRESHOLD ) &
        stop 'Wigner_rotation: determinant not in {+1,-1}'
!
! the eulerian angles are derived from the inverse of
! dmati, because we use the convention that we rotate functions
!
      det = invert3x3( mrot, inv=dmat )
!
! beta follows directly from d33
!
      cosb = dmat(3,3)
      sinb = 1.0 - cosb*cosb
      sinb = max(sinb,0.00)
      sinb = sqrt(sinb)
!
! if beta = 0 or pi , only alpha+gamma or -gamma have a meaning:
!
      if( abs(sinb) < THRESHOLD ) then

        beta = 0.0
        if ( cosb < 0.0 ) beta = pi
        gamma = 0.0
        cosa = dmat(1,1)/cosb
        sina = dmat(1,2)/cosb
        if ( abs(sina) < THRESHOLD ) then
          alpha=0.0
          if ( cosa < 0.0 ) alpha=alpha+pi
        else
          alpha = 0.5*pi - atan(cosa/sina)
          if ( sina < 0.0 ) alpha=alpha+pi
        endif

      else

        beta = 0.5*pi - atan(cosb/sinb)
!
! determine alpha and gamma from d13 d23 d32 d31
!
        cosa = dmat(3,1)/sinb
        sina = dmat(3,2)/sinb
        cosc =-dmat(1,3)/sinb
        sinc = dmat(2,3)/sinb
        if ( abs(sina) < THRESHOLD ) then
          alpha=0.0
          if ( cosa < 0.0 ) alpha=alpha+pi
        else
          alpha = 0.5*pi - atan(cosa/sina)
          if ( sina < 0.0 ) alpha=alpha+pi
        endif
        if ( abs(sinc) < THRESHOLD ) then
          gamma = 0.0
          if ( cosc < 0.0 ) gamma=gamma+pi
        else
          gamma = 0.5*pi - atan(cosc/sinc)
          if ( sinc < 0.0 ) gamma=gamma+pi
        endif

      endif


    if( .not. written .and. o/=0 ) then
      write(o,fmt='(3A)') sym, fun
      write(o,fmt='(A)') '    Eulerian angles for the rotation', &
                         '    alpha     beta      gamma    det'
      write(o,fmt='(4F10.5)') alpha, beta, gamma, det
!       written = .true.
    endif ! not written and o/=0

    Eulerian(1) = alpha
    Eulerian(2) =  beta
    Eulerian(3) = gamma

  endfunction matrix2Eulerian

  !! converts a rotation operations to a Wigner rotation matrices in a ell-subspace
  function matrices2Wigner_rotation( nop, mrot, ellmax, bmat ) result( rot )
  use constants, only: pi
  implicit none
    ! arguments
    integer, intent(in)             :: nop            !! number of operations
    real, intent(in)                :: mrot(3,3,nop)  !! rotation operation matrices
    integer, intent(in)             :: ellmax         !! ell quantum number
    real, intent(in), optional      :: bmat(3,3)      !! *NOT IMPLEMENTED*, residual from FLEUR
    ! result
    complex                 :: rot(-ellmax:ellmax,-ellmax:ellmax,ellmax,nop) !! Wigner rotation matrices
    ! local vars
    integer                 :: ns
    real                    :: Eulerian(3)

    if( present(bmat) ) stop 'matrices2Wigner_rotation: BMAT not implemented.'

    do ns = 1, nop
      Eulerian = matrix2Eulerian( mrot(:,:,ns) )
      rot(:,:,:,ns) = Eulerian2Wigner_rotation_ellmax( Eulerian, ellmax )
    enddo ! ns

  endfunction matrices2Wigner_rotation


  !! converts a rotation operation matrix to a Wigner rotation matrix
  function matrix2Wigner_rotation( mrot, ellmax ) result( rot )
  use constants, only: pi
  implicit none
    ! arguments
    real, intent(in)        :: mrot(3,3) !! rotation matrix
    integer, intent(in)     :: ellmax    !! ell quantum number
    ! result
    complex                 :: rot(-ellmax:ellmax,-ellmax:ellmax,ellmax) !! Wigner rotation matrix
    ! local vars
    real                    :: Eulerian(3)

      Eulerian = matrix2Eulerian( mrot )
      rot(:,:,:) = Eulerian2Wigner_rotation_ellmax( Eulerian, ellmax )

  endfunction matrix2Wigner_rotation

  !! converts Eulerian angles to a Wigner rotation matrix
  function Eulerian2Wigner_rotation( Eulerian, l ) result( rot )
  use constants, only: pi
  implicit none
    ! parameter
    character(len=*), parameter     :: fun = ' Eulerian2Wigner_rotation: '
    real, parameter                 :: THRESHOLD = 1.E-5
    complex, parameter              :: IM = (0.0,1.0)
    ! arguments
    real,    intent(in)             :: Eulerian(3) !! Eulerian angles
    integer, intent(in)             :: l !! ell quantum number
    ! result
    complex                         :: rot(-l:l,-l:l) !! Wigner rotation matrix
    ! local vars
    integer                         :: i,j,k,m,mp,x_lo,x_up,x,e_c,e_s
    real                            :: fac_l_m,fac_l_mp,fac_lmpx,fac_lmx,fac_x,fac_xmpm
    real                            :: co_bh,si_bh,nomin,denom,cp,sp
    real                            :: det
    complex                         :: phase_g,phase_a,bas,d(-l:l,-l:l)
    real                            :: alpha, beta, gamma

    rot = 0.
    rot(0,0) = 1.0 
    if( l == 0 ) return

    alpha = Eulerian(1)
    beta  = Eulerian(2)
    gamma = Eulerian(3)

    co_bh = cos(beta*0.5)
    si_bh = sin(beta*0.5)

    do m = -l,l
      fac_l_m = fac(l+m) * fac(l-m)
      phase_g = exp( - IM * gamma * m )

      do mp = -l,l
        fac_l_mp = fac(l+mp) * fac(l-mp)

        nomin = sqrt( real(fac_l_m * fac_l_mp) )
        phase_a = exp( - IM * alpha * mp )
        x_lo = max(0, m-mp)
        x_up = min(l-mp, l+m)

        bas = nomin * phase_a * phase_g
        d(m,mp) = cmplx(0.0,0.0)
        do x = x_lo,x_up
          fac_lmpx = fac(l-mp-x)
          fac_lmx  = fac(l+m-x)
          fac_x    = fac(x)
          fac_xmpm = fac(x+mp-m)
          denom = fac_lmpx * fac_lmx * fac_x * fac_xmpm
          e_c = 2*l + m - mp - 2*x
          e_s = 2*x + mp - m
          cp = co_bh ** e_c    ;   if( e_c == 0 ) cp = 1.0
          sp = si_bh ** e_s    ;   if( e_s == 0 ) sp = 1.0
          if( denom == 0. ) stop 'Wigner_rotation: DENOMINATOR == 0.'
          d(m,mp) = d(m,mp) + bas * (-1)**x * cp * sp / denom
        enddo

      enddo ! loop over mp
    enddo   ! loop over m

    do m = -l,l
      do mp = -l,l
        d( m,mp ) = d( m,mp ) * (-1)**(m-mp)
      enddo ! mp
    enddo ! m

!     det = 1. ! only rotations, no inversion
! 
!     do m = -l,l
!       do mp = -l,l
!         if( abs(det+1) < THRESHOLD ) then
!             rot(m,mp) = d( m,mp) * (-1)**l ! adds inversion
!         else
!             rot(m,mp) = d( m,mp)
!         endif ! |det+1.0| < THRESHOLD
!       enddo ! mp
!     enddo ! m
    rot = d

#ifdef DEBUG
!     call show_matrix( o, rot, comment='Eulerian2Wigner_rotation: rot' )
#endif

  endfunction Eulerian2Wigner_rotation


  !! converts Eulerian angles to Wigner rotation matrices for all ells from 0 to ellmax
  function Eulerian2Wigner_rotation_ellmax( Eulerian, ellmax ) result( rot )
  implicit none
    ! parameter
    character(len=*), parameter     :: fun = ' Eulerian2Wigner_rotation: '
    ! arguments
    real, intent(in)                :: Eulerian(3)
    integer, intent(in)             :: ellmax
    ! result
    complex                         :: rot(-ellmax:ellmax,-ellmax:ellmax,ellmax)
    ! local vars
    integer                         :: ell

      do ell = 1, ellmax
        rot(-ell:ell,-ell:ell,ell) = Eulerian2Wigner_rotation( Eulerian, ell )
      enddo ! ell

  endfunction Eulerian2Wigner_rotation_ellmax


  subroutine show_matrix_c( unt, mat, comment )
  ! display routine for complex matrix
  implicit none
    ! parameter
    character(len=*), parameter   :: LINE = '----------------------------'
    ! arguments
    integer, intent(in)       :: unt ! output unit
    complex, intent(in)       :: mat(1:,1:)
    character(len=*), optional:: comment
    ! local vars
    integer                   :: n, m, i

    if( unt == 0 ) return
    n = size(mat,1)
    m = size(mat,2)

    write(unt,'(99A)') ( LINE(1:12) , i=1,n)
    if( present( comment ) ) write(unt,'(2A)') ' ', comment
    do i = 1, m
      write(unt,'(99A)') string( mat(1:n,i) )
    enddo ! m
    write(unt,'(99A)') ( LINE(1:12) , i=1,n)
    write(unt,'(A)')   ''

  contains

    elemental character(len=12) function string( c )
    ! makes a nice string out of a complex number
    implicit none
      real, parameter           :: THRES = 1.E-4
      complex, intent(in)     :: c

      if( abs(aimag(c)) < THRES ) then
        write(string,'(A3,F5.2,A3)') '   ', real(c), '   '
      elseif( abs(real(c)) < THRES ) then
        write(string,'(A3,F5.2,A3)') '   ', aimag(c), 'i  '
      elseif( aimag(c) < 0. ) then
        write(string,'(F5.2,A1,F5.2,A1)') real(c), ' ', aimag(c), 'i'
      else
        write(string,'(F5.2,A1,F5.2,A1)') real(c), '+', aimag(c), 'i'
      endif

    endfunction

  endsubroutine show_matrix_c


  subroutine show_matrix_r( unt, mat, comment )
  ! display routine for real matrix
  implicit none
    ! parameter
    character(len=*), parameter   :: LINE = '----------------------------'
    ! arguments
    integer, intent(in)       :: unt ! output unit
    real, intent(in)          :: mat(1:,1:)
    character(len=*), optional:: comment
    ! local vars
    integer                   :: n, m, i

    if( unt == 0 ) return
    n = size(mat,1)
    m = size(mat,2)

    write(unt,'(99A)') ( LINE(1:8) , i=1,n)
    if( present( comment ) ) write(unt,'(2A)') ' ', comment
    do i = 1, m
!         write(unt,'(99F12.8)') mat(1:n,i)
      write(unt,'(99F8.4)') mat(1:n,i)
!         write(unt,'(99F6.2)') mat(1:n,i)
    enddo ! m
    write(unt,'(99A)') ( LINE(1:8) , i=1,n)
    write(unt,'(A)')   ''

  endsubroutine show_matrix_r


  !!>the factorial of n (result as real):
  !!           __n
  !!  fac(n) = ||  i
  !!<          i=1
  elemental real function fac(n)
  ! factorial n
  implicit none
    ! argument
    integer, intent(in) :: n
    ! local var
    integer             :: i
    fac = 0 ; if( n < 0 )  return
    fac = 1 ; if( n == 0 ) return
    do i = 2, n
      fac = fac * i
    enddo
  endfunction fac



#ifdef DEBUG
!+ debug

  status_t function test_rot( ) result( ios )
  ! returns a set of Wigner rotations for real spherical harmonics
  use configuration, only: o
  use harmonics, only: Ylm2Xlm
  use harmonics, only: dagger
  implicit none
    ! parameter
    character(len=*), parameter     :: fun = ' test_cylindrical: '
    integer, parameter  :: nr = 400
    integer, parameter  :: lm = 4
    integer, parameter  :: nop = 1
    ! local vars
    real            :: R
    integer         :: ir, l, m
    real            :: Eulerian(1:3)
    complex         :: d_wgn(-lm:lm,-lm:lm,lm)
    real            :: tmp(-lm:lm,-lm:lm)
    complex         :: rot(-lm:lm,-lm:lm,lm)
    complex         :: c2r(-lm:lm,-lm:lm,lm) ! conversion complex2real Ylm
    complex         :: r2c(-lm:lm,-lm:lm,lm) ! conversion complex2real Ylm

    real                :: rv(3), mrot(3,3)
    integer             :: i

    ios = 0
    do while(ios==0)
!         write(o,'(A)') 'Please enter difference vector of positions'
!         read(5,fmt=*,iostat=ios) rv
!         if( ios == 0 ) then
!           write(o,'(A,3F10.6)') 'Rvec =', rv
!           Eulerian = vector2Eulerian( rv )
        write(o,'(A)') 'Please enter symmetry matrix'
        read(5,fmt=*,iostat=ios) mrot
        if( ios == 0 ) then
          write(o,'(A,3F10.6)') '     ', mrot(:,1)
          write(o,'(A,3F10.6)') 'rot =', mrot(:,2)
          write(o,'(A,3F10.6)') '     ', mrot(:,3)
          Eulerian = matrix2Eulerian( mrot )

          d_wgn = Eulerian2Wigner_rotation_ellmax( Eulerian, lm )
          do l = 1, lm
            r2c(-l:l,-l:l,l)  = Ylm2Xlm( l )
            c2r(-l:l,-l:l,l)  = dagger( r2c(-l:l,-l:l,l) )
            rot(-l:l,-l:l,l) = matmul( c2r(-l:l,-l:l,l) , matmul( d_wgn(-l:l,-l:l,l), r2c(-l:l,-l:l,l) ) )
!             call show_matrix( o, rot(-l:l,-l:l,l), 'rot:' )
            tmp = real( rot(:,:,l) )
!             call show_matrix( o, tmp(-l:l,-l:l), 'rot (real):' )
            write(o,'(A,36F6.2)') '     ', (tmp(i,i), i=-l,l)
          enddo !
        endif ! ios == 0
    enddo ! while ios == 0
  endfunction test_rot



  !! a simple histogram
  subroutine simple_hist( data, hist )
  implicit none
    ! parameters
    character(len=*), parameter :: fun = ' simple_hist: '
    ! arguments
    real, intent(in)    :: data(:) !! data set
    real, intent(out)   :: hist(:) !! hist(nbins) <br> how many values are in the bin of width = [ maxval(data)-minval(data) ]/nbins
    ! local vars
    real                :: mdm(-1:+1)
    integer             :: ib, i, n
    n = size(hist)
    mdm(-1) = minval(data)
    mdm(+1) = maxval(data)
    mdm(0) = ( mdm(1)-mdm(-1) )/real(n-1) 
    hist = 0.
    do i = 1, n
      ib = int( ( data(i)-mdm(-1) )/mdm(0) ) + 1
      hist(ib) = hist(ib) + 1.
    enddo ! i
    if(o>0) write(o,'(2A,I6,5(A,ES12.3E3))') sym, fun, &
      n, ' bins', mdm(-1), ':', mdm(0), ':', mdm(1), &
      ' [min max]=[', minval(hist),' ->', maxval(hist), '].'
  endsubroutine simple_hist


  status_t function test_me( ) result( ios )
  use configuration, only: o
  use constants, only: Pi
!   use debugtools, only: write_vtk_file
!   use harmonics, only: Ylm2Xlm
  implicit none
    ! parameter
    character(len=*), parameter     :: fun = ' test_cylindrical: '
    integer, parameter  :: nr = 400
    integer, parameter  :: lmax = 1
    integer, parameter  :: nop = 1
    ! local vars
    real            :: R
    integer         :: ir, l, m
    real            :: mrot(3,3,nop)
!     real            :: bmat(3,3)
    real            :: Eulerian(1:3)
    complex         :: d_wgn(-lmax:lmax,-lmax:lmax,lmax)
    complex         :: tmp(-lmax:lmax,-lmax:lmax)
    complex         :: rot(-lmax:lmax,-lmax:lmax,lmax)
    complex         :: c2r(-lmax:lmax,-lmax:lmax,lmax) ! conversion complex2real Ylm

!     mrot(:,:,1) = real( reshape( (/ 0, -1,  0,  1,  1,  1, -1,  0,  0/), (/3,3/) ) )
!     mrot(:,:,1) = real( reshape( (/ 0, -1,  0,  0,  0,  1, -1,  0,  0/), (/3,3/) ) )
!     mrot(:,:,1) = real( reshape( (/1,0,0,0,1,0,0,0,1/), (/3,3/) ) )
!     bmat(:,:)   = real( reshape( (/ 1,0,0,0,1,0,0,0,1/), (/3,3/) ) )

!     write(6,*) 'Please enter Eulerian angles in degrees'
!     read(5,*) Eulerian(1:3)
!     Eulerian = Eulerian * Pi/180.

!     call show_matrix( o, mrot(:,:,1) )
! 
!     d_wgn(-lmax:lmax,-lmax:lmax,:) = Wigner_rotation( mrot(:,:,1), lmax )
!     do l = 1, lmax
! !       d_wgn(-l:l,-l:l,l) = Wigner_rotation( Eulerian, l )
! 
!       c2r(-l:l,-l:l,l) = Ylm_c2r( l )
! 
!       rot(-l:l,-l:l,l) = matmul( d_wgn(-l:l,-l:l,l), c2r(-l:l,-l:l,l) )
! 
!       c2r(-l:l,-l:l,l) = Ylm_c2r( l, dagger=.true. )
! 
!       tmp = rot(:,:,l)
!       rot(-l:l,-l:l,l) = matmul( c2r(-l:l,-l:l,l), tmp(-l:l,-l:l) )
! 
!       ! rotation matrix of the real spherical harmonics
!       call show_matrix( o, real(rot(-l:l,-l:l,l)) )
!     enddo ! l


!     do ir = 0, 200
!       R = ir*0.05
!       write(8,'(16F16.10)') R, overlap( o, R, 1, 5., gauss, 1, 5., gauss ), &
!                                overlap( o, R, 1, 5., gauss, 3, 5., gauss ), &
!                                overlap( o, R, 3, 5., gauss, 3, 5., gauss ), &
!                                overlap( o, R, 1, 5., gauss, 7, 5., gauss ), &
!                                overlap( o, R, 3, 5., gauss, 7, 5., gauss ), &
!                                overlap( o, R, 7, 5., gauss, 7, 5., gauss )
!     enddo ! ir

    real                :: rv(3)
    real, allocatable   :: ovl(:,:)
    integer             :: ell0, ell1

    ios = 0
    do while(ios==0)
        write(o,'(A)') 'Please enter difference vector of positions'
        read(5,fmt=*,iostat=ios) rv
        if( ios == 0 ) then
          write(o,'(A,3F10.6)') 'Rvec =', rv
!           ell0 = 0
!           ell1 = 1
          ell0 = 1
          ell1 = 1
          allocate( ovl(2*ell1+1,2*ell0+1) )
          ovl = orbital_overlap_test( o, rv, ell0, ell1 )
!           call show_matrix( o, ovl, 'ovl:' )
          deallocate( ovl )
        endif ! ios == 0
    enddo ! while ios == 0
  endfunction ! test_me

  real function gauss( r )
  use constants, only: sqrt4Pi
    real, intent(in)    :: r
!     rf = rf * Pi**(-0.5)*sqrt(4.*Pi)  ! is the normalized Hydrogen wave function:
    gauss = 2.*exp( -r )
  endfunction

!- debug
#endif

  status_t function test( )
#ifdef DEBUG
    test = test_rot()
#else
    write(*,*,iostat=test) __FILE__,' no module test implemented!'
#endif
  endfunction ! test

endmodule ! cylindrical

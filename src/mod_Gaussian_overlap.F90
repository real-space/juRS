#include "config.h"

#define DEBUG
! #define FULL_DEBUG

! #define PROGRESS

#ifdef DEBUG
!!! remove comments from debug lines
#define cDBG

#ifdef FULL_DEBUG
!!! remove comments from full_debug lines
#define cFDBG
#else
#define cFDBG !FDBG
#endif

#else
#define cDBG !DBG
#define cFDBG !FDBG
#endif

! documentation

!! @author Paul Baumeister
!! @version 4.0
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
!! given as linear combination of Gaussians times spherical harmonics for a given
!! atomic geometry. The overlap integrals are solved in 3dimensional coordinates
!!
module Gaussian_overlap
#ifdef DEBUG
  use configuration, only: o ! output unit, 0: no output
#endif
implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: sym = 'Gov' !! module symbol

  ! public
  public :: LCAO_coefficients
  public :: test

  ! interfaces
  interface LCAO_coefficients
    module procedure LCAO_coefficients_r
  endinterface

#ifndef DEBUG
  iounit_t, parameter :: o = 0 ! no output
#endif


  !! cutoff for angular momentum quantum number ell
!   integer, parameter           :: LMAX = 0 !! 0: only s states
!   integer, parameter           :: LMAX = 1 !! 1: only s and p states
  integer, parameter           :: LMAX = 2 !! 2: s, p and d states
!   integer, parameter           :: LMAX = 3 !! 3: s, p, d and f states

  integer, parameter :: NP_IMAGES = 1 ! number of periodic images of the unit cell

  character, parameter  :: ELLCHAR(-1:7) = (/'?','s','p','d','f','g','h','i','j'/) ! display help

  ! private interfaces
  integer, parameter :: Mx = 2*LMAX!+1 ! odd entries do not contribute anyway, so drop +1!

  ! a polynomial in the cartesian coordinates x, y and z
  type, private :: tripoly
    real    :: f(0:Mx,0:Mx,0:Mx) = 0.
!     logical :: l(0:Mx,0:Mx,0:Mx) = .false.
  endtype tripoly

  interface assignment(=)
                     ! tri = real  ! tri = char
    module procedure rset_tripoly, cset_tripoly
  endinterface

  interface operator(+)
                     ! tri = tri + tri
    module procedure add_tripoly_tripoly
  endinterface

  interface operator(-)
                     ! tri = tri - tri
    module procedure diff_tripoly_tripoly
  endinterface

  interface operator(*)
                     ! tri = tri * tri     ! tri = real * tri
    module procedure mult_tripoly_tripoly, mult_r_tripoly
  endinterface

!!
!! In order to find evaluate the two-center integrals of localized radial functions
!!  /
!!  |d3r R1l1(|r-Rv1|) Yl1m1 * R2l2(|r-R2|) Yl2m2
!!  /
!! we can expand the localized radial function Rl(r) into generalized Gaussions
!!   Rl(r) = r^l sum_k>0 Rl_k exp(-kr^2)
!! in fact for simplicity and an easy-to-optimize implementation of Ylm, we separate
!!   Rl * Ylm into Rl/r^l * r^lYlm
!! such that  Rl(r)/r^l behaves as an even order polynomial at the origin and can thus be expanded as
!!  Rl(r) Ylm(^r) = [ sum_k Rl_k exp(-kr^2) ] * r^lYlm(x,y,z)
!!
!! Now analyzing the overlap expression for one pure Gaussian with one spherical harmonic function
!!  /  /  /
!!  |dx|dy|dz exp(-k1 (r-Rv1)^2 ) * exp(-k2 (r-Rv2)^2 ) r^lYl1m1(x-X1,y-Y1,z-Z1) * r^lYl2m2(x-X2,y-Y2,z-Z2)
!!  /  /  /
!! we see that this expression reduces to independent integrals in x, y or z of the form
!!  / 
!!  |dx exp(-k1(x-X1)^2-k2(x-X2)^2) x^n
!!  /
!! a shift of the origin to a center between the two atom centers kills the terms linear in x in the exponential function
!! this center is found according to the weights
!!    w1 = k2/(k1+k2), w2 = k1/(k1+k2)
!! such the the expressions are suppressed with the exponential factor exp(-(k1+k2)*(Rv1-Rv2)^2)
!!
!! Now the question is, how we decompose the radial function into Gaussians with the coefficients R_k.
!! taking a fixed set of k-values allows to exand
!!   R_k = <G_k'|R> * inverse[ <G_k|G_k'> ]
!! However, it would be good to find the set of k-values such that we can approximate the radial function
!! outside the atomic augmentation sphere in a least square sense.
!!
!! in principle we can find the analytic expressions for the overlap as a function of (k1,k2,X2-X1,Y2-Y1,Z2-Z1,l1m1,l2m2)
!!
!!
    !!    /  /  /
    !! t= |dz|dy|dx  exp( -(k0+k1)*vec r^2 )
    !!    /  /  /      times [r^l Ylm](ell0, vec r + k1/(k0+k1) vec R)
    !!                 times [r^l Ylm](ell1, vec r - k0/(k0+k1) vec R)
!! think of it analytically:
!! s & s ==> rlYlm = 1  ==> O = 1 * exp( -(k1+k2)*R^2 )
!! s & px ==> rlYlm = x ==> O = int exp( -kk*x^2 ) * 1 * (x-dx) int exp(
!!       

contains

  !! computes the coefficients of lowest energy solutions
  !! of the Linear Compbination of Atomic Orbitals for a
  !! given set of kpoints
  status_t function LCAO_coefficients_r( o, a, g, kpt ) result( ist )
  use type_atom, only: atom
  use type_grid, only: grid, periodic_positions
  use LAPACK, only: diagonalize
  use type_kpoint, only: kpoint
#ifdef DEBUG_ALL
  use LAPACK, only: check_matrix
#endif
  use constants, only: Pi
  use toolbox, only: write_bmp_file, operator(+)
  implicit none
    ! parameter
    character(len=*), parameter     :: fun = ' LCAO_coefficients: '
    complex, parameter              :: im2Pi = cmplx(0.,1.)*2.*Pi
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
    integer                         :: ib
    integer                         :: ni, ii, is(3,125) ! for images
    integer                         :: i1, i2
    real                            :: kL(3), ori(3,125)
#ifdef PROGRESS
    integer                         :: iprog = 0
    real                            :: dprog, prog_nxt = 0., progress = 0.
#endif

    na = size( a, 1 ) ! number of atoms 
    if( na < 1 ) return

    ni = periodic_positions( g, (/0.,0.,0./), ori, shifts=is, number_of_images=NP_IMAGES )
    if(o>0) write(o,'(/,9A)') sym, fun, 'start!'


    ist = LCAO_overlap( o, a, g, omat )
!     stop 'Gov: DEBUG line 205'

    do ii = 1, size(omat,3)
      write(9,'(A,I0,A,3I3)') '# overlap matrix, periodic image #', ii, ' shifts =', is(1:3,ii)
      do io = 1, size(omat,2)
        write(9,'(100F10.6)') omat(:,io,ii)
      enddo ! io
    enddo ! ii
    if(o>0) write(o,'(9A)') sym, fun, 'O-matrices written to fort.9'

!     if(o>0) write(o,'(9A)') sym, fun, 'O-matrix (00-image) diagonal:'
!     if(o>0) write(o,'(I6,ES24.16)') ( io, omat(io,io,1), io=1,size(omat,2) )


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

    if(o>0) write(o,'(2A,9(I0,A))') sym, fun, no, ' orbitals on', na, ' atoms'
    nk = 101
    if( present( kpt ) ) nk = size(kpt)

    if(o>0) write(o,'(2A,9(I0,A))') sym, fun, nk, ' kpoints'

    ni = maxval( a(:)%nimages ) ! number of images
    if(o>0) write(o,'(2A,9(I0,A))') sym, fun, ni, ' periodic images'

#ifdef PROGRESS
    iprog = 0 ; progress = 0.
    open(unit=o, carriagecontrol='fortran')
    dprog = 1./real(nk)
    if(o>0) write(o,'(9A)') sym, fun, 'progress'
#endif
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
!       if(o>0) write(o,'(3A,I0,A,2F7.3)') sym, fun, 'k #', ik, ', Bloch factor', exp( im*kL_2Pi )
      do ii = 1, ni ! number of images
        ! Bloch-factor when shifting to the periodic image
        ! x-direction (id=1)
        eikL = exp( im2Pi*sum( is(1:3,ii)*kL ) )
!         if(o>0) write(o,'(3A,I0,A,3I3,A,2F7.3)') sym, fun, 'i #', ii, ' is=', is(:,ii), ', eikL', eikL
        Smat = Smat + eikL * omat(:,:,ii)
      enddo ! ii

      ! check if Smat is Hermitian
      do i1 = 1, no
        do i2 = 1, i1
          if( abs(Smat(i1,i2)-conjg(Smat(i2,i1))) > 1E-8 ) then
            write(*,*) i1, i2, Smat(i1,i2), Smat(i2,i1)

            write(9,'(/,A,I0,A,2F7.3)') '# overlap matrix, kpt #', ik, ' , eikL', eikL
            do io = 1, size(Smat,2)
              write(9,'(999(F12.6,F10.6))') Smat(:,io)
            enddo ! io
            if(o>0) write(o,'(9A)') sym, fun, 'Smatrix written to fort.9'

            stop 'Gov: not Hermitian!'
          endif ! not hermitian
        enddo ! i2
      enddo ! i1
      ! ==================================================================


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

      if( present( kpt ) ) then
        write(8,'(F10.6,1001ES16.6)') kpt(ik)%klen, Eval(:)
      else  ! present kpt
        write(8,'(F10.6,1001ES16.6)') 0.5 * real(ik-1)/real(nk-1), Eval(:)
      endif
      ist = write_bmp_file( 'dmp/LCAO.r_k'+ik,  real(Smat), style='invert' )
      ist = write_bmp_file( 'dmp/LCAO.i_k'+ik, aimag(Smat), style='invert' )

#ifdef PROGRESS
      !----------------------------------------------
      if(o>0) then ! show progress
        iprog = iprog+1 ! progress for all
        progress = dprog*iprog
        do while( progress > prog_nxt )
          prog_nxt = prog_nxt + 0.01              ! char(13) is a carriage return and needs the leading '+'
          write(unit=o,fmt='(2A1,3A,I3,9A)') '+', char(13), sym, fun, 'progress', nint( 100.*progress ) , ' %'
        enddo ! prog
      endif ! o/=0
      !----------------------------------------------
#endif
    enddo ! ik
    stop 'LCAO_coefficients: xmgrace -nxy fort.8 &'

  endfunction ! LCAO_coefficients


  !! computes the overlap matrix of all atomic orbitals
  !! and all periodic images of the atoms
  !! and diagonalizes it using LAPACK
  status_t function LCAO_overlap( o, a, g, omat ) result( ist )
  use type_atom, only: atom
  use type_grid, only: grid
  use type_grid, only: periodic_positions
  use toolbox, only: write_bmp_file, operator(+)
  implicit none
    ! parameter
    character(len=*), parameter     :: fun = ' LCAO_overlap: '
    ! arguments
    integer, intent(in)             :: o !! output unit, 0: no output
    type(atom), intent(in)          :: a(:) !! list of *all* atoms
    type(grid), intent(in)          :: g !! grid descriptor
    real, allocatable, intent(out)  :: omat(:,:,:) !! dims(norb,norb,nimages) <br> overlap matrix
    ! local vars
    integer                         :: na, ia, ia1, ia2
    integer                         :: iorb, norb, i1, i2, no
    integer                         :: ell1, ell2
    integer, allocatable            :: ia_list(:)
    real                            :: pos1(3), pos2(3), r, Rvec(3)
    real                            :: k2=1., k1=1.
    integer                         :: nimages = 1, iimage = 1, ii
#ifdef PROGRESS
    integer                         :: iprog = 0
    real                            :: dprog, prog20th = 0., progress = 0.
#endif
    real                            :: ori(3,(2*NP_IMAGES+1)**3)

!    real, parameter                 :: klist(0:2) = (/0.2,0.3,0.4/)
!     real, parameter                 :: klist(0:2) = (/0.094,0.01,0.475/) ! Cu
!    real, parameter                 :: klist(0:3,1:2) = reshape( (/0.2,0.3,0.4,0.5,  .25,.35,.45,.55 /), (/4,2/) ) ! C, N
    real, parameter                 :: klist(0:3,1:2) = reshape( (/0.2,0.3,0.4,0.5,  0.2,0.3,0.4,0.5 /), (/4,2/) ) ! C, N
    real                            :: f1, f2

    na = size( a, 1 ) ! number of atoms
    if( na < 1 ) return

    na = size( a, 1 ) ! number of atoms
    if( na < 1 ) return

    nimages = periodic_positions( g, (/0.,0.,0./), ori, number_of_images=NP_IMAGES )
!     nimages = maxval( a(:)%nimages )
!     if( minval( a(:)%nimages ) /= nimages ) &
!       stop 'LCAO_overlap: atoms must have the same number of periodic images.'
    if(o>0) write(o,'(2A,9(I0,A))') sym, fun, nimages,' periodic images'

    norb = (LMAX+1)**2 * na

    ! create a list that says to which atom this orbital belongs
    allocate( ia_list(norb) )
    iorb = 0
    do ia = 1, na
      no = ( min(a(ia)%s%ellmax,LMAX) + 1 )**2
      ia_list(iorb+1:iorb+no) = ia
      iorb = iorb + no
    enddo ! ia
    norb = iorb 

    if(o>0) write(o,'(2A,9(I0,A))') sym, fun, norb,' atomic orbitals'

    if( allocated( omat ) ) deallocate( omat )
    allocate( omat(norb,norb,nimages), stat=ist )
    if( ist /= 0 ) stop 'LCAO_overlap: failed to allocate OMAT.'
    omat = 0. ! init



#ifdef PROGRESS
    iprog = 0 ; progress = 0.
    open(unit=o, carriagecontrol='fortran')
    dprog = 1./real(norb*norb*nimages)
    if(o>0) write(o,'(9A)') sym, fun, 'progress'
#endif

#ifdef DEBUG
#define ia2_LIMIT na
 ! regular loop
#else
 ! triangular loop: exploit that omat(i,j)=omat(j,i), therefore the inner atom index only runs op to ia1
#define ia2_LIMIT ia1
#endif

    do iimage = 1, nimages
cFDBG if(o>0) write(o,'(/,3A,I4)') sym, fun, 'iimage', iimage

      !----------------------------------------------
      i1 = 1 ! start index
      do ia1 = 1, na

        pos1 = a(ia1)%pos(:) + ori(:,iimage) ! the image position of atom1

        do ell1 = 0, min(a(ia1)%s%ellmax,LMAX)
          k1 = klist(ell1,min(ia1,2))
          f1 = scale_factor( ell1, k1 )

          !----------------------------------------------
          i2 = 1 ! start index

          do ia2 = 1, ia2_LIMIT

            pos2 = a(ia2)%pos ! the real position of atom2
            Rvec = pos2 - pos1 ! difference vector between the two radial centers

            do ell2 = 0, min(a(ia2)%s%ellmax,LMAX)
              k2 = klist(ell2,min(ia2,2))
              f2 = scale_factor( ell2, k2 )

cFDBG         if(o>0) write(o,'(2A,9(I3,5A))') sym, fun, ia1, '. ', a(ia1)%s%sym, ' ',ELLCHAR(ell1), ' --- ', ia2, '. ', a(ia2)%s%sym, ' ', ELLCHAR(ell2)

              omat( i2:i2+2*ell2, i1:i1+2*ell1, iimage ) = &
                orbital_overlap( o, Rvec, ell1, k1, ell2, k2 ) * f1 * f2
#if ia2_LIMIT == ia1
              omat( i1:i1+2*ell1, i2:i2+2*ell2, iimage ) = transpose( omat( i2:i2+2*ell2, i1:i1+2*ell1, iimage ) )
#endif

cFDBG         if(o>0) then
cFDBG           do ii = i1, i1+2*ell1
cFDBG             write(o,'(99F10.6)') omat( i2:i2+2*ell2, ii, iimage )
cFDBG           enddo ! ii
cFDBG         endif



#ifdef PROGRESS
              !---show progress bar--------------------------
              iprog = iprog + (2*ell2+1)*(2*ell1+1)   ! progress for all
              progress = dprog*iprog
              do while( progress > prog20th )
                prog20th = prog20th + 0.05              ! char(13) is a carriage return and needs the leading '+'
                if(o>0) write(unit=o,fmt='(2A1,3A,I3,9A)') '+', char(13), sym, fun, 'progress', nint( 100.*progress ) , ' %'
              enddo ! prog
              !----------------------------------------------
#endif

              i2 = i2 + (2*ell2+1)
            enddo ! ell2
          enddo ! ia2
          !----------------------------------------------

          i1 = i1 + (2*ell1+1)
        enddo ! ell1
      enddo ! ia1
      !----------------------------------------------

cFDBG if(o>0 .and. norb<33) then
cFDBG   write(o,'(A)') '' ! empty line
cFDBG   do i1 = 1, norb
cFDBG     write(o,'(99F5.2)') omat( 1:norb, i1, iimage )
cFDBG   enddo ! i1
cFDBG endif

      ist = write_bmp_file( 'dmp/LCAO.'+iimage, abs(omat(:,:,iimage)), style='invert' )
    enddo ! iimage

#ifdef PROGRESS
!     if(o>0) write(o,'(A)',advance='yes') ']'
#endif

!     if(o>0) then
!       do i1 = 1, norb
!         write(o,'(9999F7.3)') omat(:,i1,1)
!       enddo ! i1
!     endif ! o>0

  endfunction LCAO_overlap


  real function scale_factor( ell, k ) result( s )
  use constants, only: SQRTPi
  implicit none
    ! parameter
    character(len=*), parameter     :: fun = ' scale_factor: '
    ! arguments
    integer, intent(in)             :: ell
    real, intent(in)                :: k
    ! local vars
    integer                         :: tlf, lf, i

    tlf = 1
    do i = 2, 2*ell+2
      tlf = tlf*i
    enddo ! i
    ! now tlf = factorial(2ell+2)

    lf = 1
    do i = 2, ell+1
      lf = lf*i
    enddo ! i
    ! now lf = factorial(ell+1)

    s = 1./sqrt( SQRTPi * real(tlf)/real(lf) * (8.*k)**(-ell-1.5) )

  endfunction scale_factor

  !! computes the overlap of two generalized Gaussian functions
  !! times spherical harmonics of ell for emm=-ell...ell centered at two positions that differ by Rvec
  function orbital_overlap( o, Rv, ell0, k0, ell1, k1 ) &
  result( oovl )
  implicit none
    ! parameter
    character(len=*), parameter     :: fun = ' orbital_overlap: '
    ! arguments
    integer, intent(in)             :: o !! output unit, 0: no output
    real, intent(in)                :: Rv(3) !! difference vector of positions
    integer, intent(in)             :: ell0 , ell1 !! ell quantum numbers
    real, intent(in)                :: k0, k1 !! Gaussian decay coefficients
    ! result
    real                            :: oovl(-ell1:ell1,-ell0:ell0) !! result: overlap of all 2*ell+1 orbitals of the one atom with the others <br> Note, this is not nessecarily a square matrix
    ! local vars
    integer                         :: ilm0, ilm1, emm0, emm1, i
    real                            :: t, R2, k0pk1, dec, arg, Rv0(3), Rv1(3)
    type(tripoly)                   :: rlYlm0, rlYlm1(-ell1:ell1), prod

    k0pk1 = k0+k1
    oovl = 0. ! init result for early return
    if( k0pk1 <= 0. ) return ! there should not be negative or zero k-values

    R2 = sum( Rv(1:3)**2 ) ! length^2 of the distance vector
!     real :: R
!     R = sqrt( R2 ) ! length of the distance vector

    arg = -k0*k1/k0pk1 * R2
    if( arg < -36. ) return ! prefactor is smaller than 2.32E-16
    dec = exp( arg ) ! decay factor (indep. of the integrations)

    !!  find the integral
    !!    /  /  /
    !! t= |dz|dy|dx  exp( -(k0+k1)*vec r^2 )
    !!    /  /  /      times [r^l Ylm](ell0, vec r + k1/(k0+k1) vec R)
    !!                 times [r^l Ylm](ell1, vec r - k0/(k0+k1) vec R)
    !! the center of this integration has been set to be a point on
    !! the connection line between the two centers, whereas the
    !! distance has been divided in the ratio k1/(k0+k1) : k0/(k0+k1)
    !! to transform the exponents and eliminate the linear component

    Rv0(1:3) = -k1/k0pk1 * Rv(1:3)
    Rv1(1:3) = +k0/k0pk1 * Rv(1:3)
cFDBG if(o>0) write(o,'(A,2(F10.6,2A),3F10.3,9A)') '[k,l]', k0, ELLCHAR(ell0), ' [k,l]', k1, ELLCHAR(ell1), ' R=[', Rv, ' ]'

    ! precompute the spherical harmonic functions for ell1
    do emm1 = -ell1, ell1     ; ilm1 = ell1*(ell1+1)+emm1+1
      rlYlm1(emm1) = rlYlm( ilm1, Rv1 )
! cFDBG   if(o>0) i = write2unit( t=rlYlm1(emm1), u=o )!(, name )

    enddo ! emm1

cFDBG  if(o>0) write(o,'(3A,2(F10.6,2A),3F10.3,9A)') sym, fun, '[k,l]', k0, ELLCHAR(ell0), ' [k,l]', k1, ELLCHAR(ell1), ' R=[', Rv, ' ]'
    do emm0 = -ell0, ell0     ; ilm0 = ell0*(ell0+1)+emm0+1

      rlYlm0 = rlYlm( ilm0, Rv0 )
! cFDBG   if(o>0) i = write2unit( t=rlYlm0, u=o )!(, name )

      do emm1 = -ell1, ell1   ; ilm1 = ell1*(ell1+1)+emm1+1

        prod = rlYlm0 * rlYlm1(emm1) ! self-defined multiplication of two tripolys

! cFDBG   if(o>0) i = write2unit( t=prod, u=o )!(, name )
        t = value( prod, k0pk1 )

        oovl(emm1,emm0) = dec * t
      enddo ! emm1
! cFDBG    if(o>0) write(o,'(9F10.6)') oovl(:,emm0)
    enddo ! emm0
! cFDBG    if(o>0) write(o,'(A)') ! empty line 

  endfunction orbital_overlap



  function rlYlm( ilm, s ) result( Ylm )
  use constants, only: Pi, f => ONESQRTFOURPI
  implicit none
    ! parameters
!     real, parameter       :: f = ONESQRTFOURPI ! sqrt(1./(4.*Pi))
!     real, parameter       :: f = sqrt(1./(4.*Pi))
    ! arguments
    integer, intent(in)   :: ilm
    real, intent(in)      :: s(1:3) !! shift vector s(1:3)
    ! return value
    type(tripoly)         :: Ylm
    ! local vars
    type(tripoly)         :: v0, vX, vY, vZ, X, Y, Z
    type(tripoly)         :: vX2, vY2, vZ2

    v0 = 1.
    X = 'x'
    Y = 'y'
    Z = 'z'
    vX = X - (s(1)*v0)
    vY = Y - (s(2)*v0)
    vZ = Z - (s(3)*v0)

    if( ilm > 4 ) then
      vX2 = vX*vX
      vY2 = vY*vY
      vZ2 = vZ*vZ
    endif ! ilm > 4

    Ylm = 0. ! init
    selectcase( ilm )
    case(:0) ; stop 'rlYlm: ilm < 1 unphysical'
    ! ===  ell = 0  ===============================================================
!     Ylm( 1) =  f ! s-orbital
    case( 1) ; Ylm = (f)*v0 ! s-orbital
    ! ===  ell = 1  ===============================================================
!     Ylm( 2) =  f*sqrt(3.)*v(X) ! px-orbital
!     Ylm( 3) =  f*sqrt(3.)*v(Z) ! pz-orbital
!     Ylm( 4) =  f*sqrt(3.)*v(Y) ! py-orbital
    case( 2) ; Ylm = (f*sqrt(3.))*vX ! px-orbital
    case( 3) ; Ylm = (f*sqrt(3.))*vZ ! pz-orbital
    case( 4) ; Ylm = (f*sqrt(3.))*vY ! py-orbital
    ! ===  ell = 2  ===============================================================
!     Ylm( 5) =  f*sqrt( 3.75)*(v(X)*v(X)-v(Y)*v(Y))    ! d-orbital !  eg
!     Ylm( 6) =  f*sqrt(15.  ) * v(Z)*v(X)              ! d-orbital ! t2g
!     Ylm( 7) =  f*sqrt( 1.25)*(3.*v(Z)*v(Z)-r2)        ! d-orbital !  eg
!     Ylm( 8) =  f*sqrt(15.  ) * v(Y)*v(Z)              ! d-orbital ! t2g
!     Ylm( 9) =  f*sqrt(15.  ) * v(X)*v(Y)              ! d-orbital ! t2g
    case( 5) ; Ylm = (f*sqrt( 3.75))*(vX2-vY2)      ! d-orbital !  eg
    case( 6) ; Ylm = (f*sqrt(15.  )) * (vZ*vX)              ! d-orbital ! t2g
    case( 7) ; Ylm = (f*sqrt( 1.25))*(2.*vZ2-vX2-vY2)        ! d-orbital !  eg
    case( 8) ; Ylm = (f*sqrt(15.  )) * (vY*vZ)              ! d-orbital ! t2g
    case( 9) ; Ylm = (f*sqrt(15.  )) * (vX*vY)              ! d-orbital ! t2g
    ! ===  ell = 3  ===============================================================
!     Ylm(10) =  f*sqrt(  4.375)*v(X)*(v(X)*v(X)-3.*v(Y)*v(Y))  ! f-orbital
!     Ylm(11) =  f*sqrt( 26.25 )*v(Z)*(v(X)*v(X)-v(Y)*v(Y))     ! f-orbital
!     Ylm(12) =  f*sqrt(  2.625)*v(X)*(5.*v(Z)*v(Z)-r2)         ! f-orbital
!     Ylm(13) =  f*sqrt(  1.75 )*v(Z)*(5.*v(Z)*v(Z)-3.*r2)      ! f-orbital
!     Ylm(14) =  f*sqrt(  2.625)*v(Y)*(5.*v(Z)*v(Z)-r2)         ! f-orbital
!     Ylm(15) =  f*sqrt(105.   )*v(X)* v(Y)*v(Z)                ! f-orbital
!     Ylm(16) =  f*sqrt(  4.375)*v(Y)*(3.*v(X)*v(X)-v(Y)*v(Y))  ! f-orbital
    case(10) ; Ylm = (f*sqrt(  4.375))*vX*(vX2-3.*vY2)  ! f-orbital
    case(11) ; Ylm = (f*sqrt( 26.25 ))*vZ*(vX2-vY2)     ! f-orbital
    case(12) ; Ylm = (f*sqrt(  2.625))*vX*(4.*vZ2-vX2-vY2)         ! f-orbital
    case(13) ; Ylm = (f*sqrt(  1.75 ))*vZ*(2.*vZ2-3.*vX2-3.*vY2)      ! f-orbital
    case(14) ; Ylm = (f*sqrt(  2.625))*vY*(4.*vZ2-vX2-vY2)         ! f-orbital
    case(15) ; Ylm = (f*sqrt(105.   ))*vX* (vY*vZ)                ! f-orbital
    case(16) ; Ylm = (f*sqrt(  4.375))*vY*(3.*vX2-vY2)  ! f-orbital
    ! ===  ell = 4  ===============================================================
    case default
      if(o>0) write(o,'(3A,I0)') sym, ' rlYlm: ', 'ilm > 16 not implemented, requested ilm = ',ilm
    endselect ! ilm

  endfunction rlYlm


  real function value( t, k ) result( v )
  implicit none
    type(tripoly), intent(in) :: t
    real, intent(in)          :: k
    integer :: ix, iy, iz
    v = 0. ! init
    ! needs to check only even powers, since odd powers bring a zero contribution
    do iz = 0, Mx, 2
     do iy = 0, Mx, 2
      do ix = 0, Mx, 2
        if( t%f(ix,iy,iz) /= 0. ) v = v + t%f(ix,iy,iz) * I_n( ix, k ) * I_n( iy, k ) * I_n( iz, k )
      enddo ! ix
     enddo ! iy
    enddo ! iz
  endfunction

!   character(len=24) function to_string( t ) result( str )
!   implicit none
!     type(tripoly), intent(in) :: t
!     integer :: ios, id
!     character(len=5) :: xyz(1:3)
!     do id = 1, 3
!       xyz(id) = ''
!       if( tp%n(id) > 0 ) then
!         if( tp%n(id) > 1 ) then
!           write(unit=xyz(id),fmt='(3A1,I0)',iostat=ios) '*',achar(119+id),'^',tp%n(id)
!         else
!           write(unit=xyz(id),fmt='(3A1,I0)',iostat=ios) '*',achar(119+id)
!         endif
!       endif
!     enddo ! id
!     write(unit=str,fmt='(F0.6,9A)',iostat=ios) tp%f, trim(xyz(1)), trim(xyz(2)), trim(xyz(3))
!   endfunction

  integer function write2unit( t, u, name ) result( ios )
  implicit none
    type(tripoly), intent(in) :: t
    integer, intent(in) :: u ! file unit or 6:stdout
    character(len=*), intent(in), optional :: name
    integer :: ix, iy, iz
    if( present( name ) ) write(unit=u,fmt='(A)',iostat=ios) name
    do iz = 0, Mx
     do iy = 0, Mx
      do ix = 0, Mx
        if( t%f(ix,iy,iz) /= 0. ) call printline( t%f(ix,iy,iz), (/ix,iy,iz/) )
      enddo ! ix
     enddo ! iy
    enddo ! iz
    write(unit=u,fmt='(A)',iostat=ios) '' ! empty line
  contains
    subroutine printline( f, n )
    implicit none
      real, intent(in) :: f
      integer, intent(in) :: n(3)
      character(len=5) :: xyz(1:3)
      integer :: id
      do id = 1, 3
        selectcase( n(id) )
        case( 0 )     ; xyz(id) = ''
        case( 1 )     ; write(unit=xyz(id),fmt='(3A1,I0)',iostat=ios) '*',achar(119+id)
        case default  ; write(unit=xyz(id),fmt='(3A1,I0)',iostat=ios) '*',achar(119+id),'^',n(id)
        endselect ! n(id)
      enddo ! id
      write(unit=u,fmt='(F0.6,9A)',iostat=ios) f, trim(xyz(1)), trim(xyz(2)), trim(xyz(3))
    endsubroutine printline
  endfunction write2unit


  function mult_tripoly_tripoly( a, b ) result( p )
  implicit none
    ! arguments
    type(tripoly), intent(in) :: a, b
    ! result
    type(tripoly)             :: p
    ! local vars
    integer                   :: ax, ay, az
    integer                   :: bx, by, bz
    integer                   :: px, py, pz
    real                      :: axb
cDBG  integer                 :: ndrop
    ndrop = 0 ! init drop counter
    p%f = 0. ! init
    do az = 0, Mx
    do ay = 0, Mx
    do ax = 0, Mx ; if( a%f(ax,ay,az) == 0. ) cycle
      do bz = 0, Mx ; pz = az+bz
      do by = 0, Mx ; py = ay+by
      do bx = 0, Mx ; px = ax+bx
                    if( b%f(bx,by,bz) == 0. ) cycle
        axb = a%f(ax,ay,az) * b%f(bx,by,bz)
        if( px > Mx .or. py > Mx .or. pz > Mx ) stop 'mult_tripoly_tripoly: entries dropped!'

cDBG    if( px <= Mx .and. py <= Mx .and. pz <= Mx ) then
        p%f(px,py,pz) = p%f(px,py,pz) + axb
cDBG    else
cDBG      ndrop = ndrop+1
cDBG    endif
      enddo ! bx
      enddo ! by
      enddo ! bz
    enddo ! ax
    enddo ! ay
    enddo ! az
!     p%l = ( p%f /= 0. )
cDBG  if( ndrop > 0 ) stop 'mult_tripoly_tripoly: entries dropped!'
  endfunction


  function diff_tripoly_tripoly( a, b ) result( d )
  implicit none
    ! arguments
    type(tripoly), intent(in) :: a, b
    ! result
    type(tripoly)             :: d ! difference a-b

    d%f = a%f - b%f
!     d%l = ( d%f /= 0. )
  endfunction

  function add_tripoly_tripoly( a, b ) result( s )
  implicit none
    ! arguments
    type(tripoly), intent(in) :: a, b
    ! result
    type(tripoly)             :: s ! sum a+b
    s%f = a%f + b%f
!     s%l = ( s%f /= 0. )
  endfunction

  elemental subroutine rset_tripoly( t, r )
  implicit none
    ! arguments
    type(tripoly), intent(out) :: t
    real, intent(in)           :: r
    t%f = 0. ! init factors
    t%f(0,0,0) = r
  endsubroutine ! assignment(=)

  elemental subroutine cset_tripoly( t, c )
  implicit none
    ! arguments
    type(tripoly), intent(out) :: t
    character, intent(in)      :: c
    t%f = 0. ! init factors
    selectcase( c )
    case( 'x', 'X' ) ; t%f(1,0,0) = 1.
    case( 'y', 'Y' ) ; t%f(0,1,0) = 1.
    case( 'z', 'Z' ) ; t%f(0,0,1) = 1.
    case default ! ; stop 'cset_tripoly: only "X", "Y" or "Z" allowed !'
    endselect ! c
  endsubroutine ! assignment(=)

  elemental function mult_r_tripoly( r, a ) result( rxa )
  implicit none
    ! arguments
    real, intent(in)          :: r
    type(tripoly), intent(in) :: a
    ! result
    type(tripoly)             :: rxa
    rxa%f = r * a%f ! multiply
!     rxa%l = ( rxa%f /= 0. )
  endfunction


  real recursive function I_n( n, k )
  !!       / infty
  !! I_n = | dx     exp(-k x^2) x^n
  !!       /-infty
  use constants, only: Pi
  implicit none
    integer, intent(in) :: n
    real, intent(in)    :: k
    I_n = 0. ! init
    if( n < 0 ) return ! 0.
    if( modulo(n,2) == 1 ) return ! 0. ! all odd n result in zero
!     if( k <= 0. ) return ! 0. ! stop not possible because recursive functions must be pure
    if( k <= 0. ) stop 'Gov: I_n: k must be positive!'
    if( n == 0 ) then
      I_n = sqrt( Pi/k )
    else  ! n == 0
      I_n = (n-1.)/(2.*k)*I_n( n-2, k ) ! recursive invocation
    endif ! n == 0
  endfunction


  status_t function test( )
    write(*,*,iostat=test) __FILE__,' tests only in DEBUG mode!'
  endfunction ! test

endmodule ! Gaussian_overlap

#include "config.h"

! #define DEBUG
! #define FULL_DEBUG

!! @author Paul Baumeister
!! @version 4.05
!!
!! generates the electrostatic potential in the spheres
!! and aligns it with the given shifts a%vlm
module radial_Hartree
  use configuration, only: o ! output unit, 0: no output
implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: sym = 'rHT' !! module symbol

  public :: es_in_the_spheres
  public :: Hartree_potential_ell
#ifdef EXTENDED
  public :: test
#endif

  contains

  status_t function es_in_the_spheres( a ) result( ist )
  use type_atom, only: atom
  use type_species, only: I_TRU, I_SMT, I_VES
  ! a%pot(:,:,I_VES,:) are the electrostatic potentials
  ! a%rho(:,:,0,:) are the spin integrated densities
  use constants, only: sqrt4pi
  use unitsystem, only: eV, eV_
    type(atom), intent(inout)       :: a

    character(len=*), parameter     :: fun = ' es_in_the_spheres: '
    integer                         :: iq, ell, emm, ilm, nr, lmax
    real                            :: EnZ, Z, EvZ
    real, allocatable               :: s_vlm(:), Ees_lm(:,:) ! (mlm,I_TRU:I_SMT)
#ifdef DEBUG
    integer                         :: ios, ir
#endif

#ifdef FULL_DEBUG
    do iq = I_TRU, I_SMT
      if(o>0) write(o,'(3A,I6,I3,A,F20.16,9A)') sym, fun, 'atom j#', a%ja, iq, ' rho    ', sum( a%rho(1:,1,0,iq)*a%s%g%r2dr(1:) )*sqrt4pi, ' e'
    enddo ! iq
#endif

    lmax = 2 * a%s%ellmax
    allocate( s_vlm(1:(lmax+1)**2), Ees_lm(1:(lmax+1)**2,I_TRU:I_SMT), stat=ist )
    Ees_lm = 0. ! init electrostatic energy contributions

!     ! solve the electrostatic potential of the smooth augmented and the tru charge
!     ! distribution in the spheres with the preliminary 0 boundary condition
!     do iq = I_TRU, I_SMT
!       call Hartree_potential( rho=a%rho(0:,:,0,iq), ves=a%pot(0:,:,I_VES,iq), r=a%s%g%r, drdi=a%s%g%dr )
!     enddo ! iq

    ! align the electrostatic potentials, such that
    ! the smooth potential coincides with the smooth potential
    ! on the grid (information stored in a%vlm)
    ! and the true potential matches with the smooth potential
    ! at the sphere boundary
    nr = a%s%nr

#ifdef FULL_DEBUG
    if(o>0) write(o,'(3A,9(F10.6,A))') sym, fun, 'norm =', sum( a%s%rcmp(1:nr,0) * a%s%g%r2dr(1:nr) )
#endif

!$omp parallel do private(ilm,ell,iq,ist)
    do ilm = 1, (lmax+1)**2
      ell = -1 ; do while( (ell+1)**2 < ilm ) ; ell = ell+1 ; enddo ! find the correct ell

      ! solve the electrostatic potential of the smooth augmented and the tru charge
      ! distribution in the spheres with the preliminary 0 boundary condition
      do iq = I_TRU, I_SMT
        ist = Hartree_potential_ell( ell, rho=a%rho(0:,ilm,0,iq), ves=a%pot(0:,ilm,I_VES,iq), r=a%s%g%r, drdi=a%s%g%dr )
        Ees_lm(ilm,iq) = 0.5 * sum( a%pot(1:nr,ilm,I_VES,iq) * a%rho(1:nr,ilm,0,iq) * a%s%g%r2dr(1:nr) )
      enddo ! iq

      ! a%vlm = integral over the grid ( es-potential * compensation charge )
      ! shift    = on the grid - in the sphere
      s_vlm(ilm) = a%vlm(ilm) - sum( a%pot(1:nr,ilm,I_VES,I_SMT) * a%s%rcmp(1:nr,ell) * a%s%g%r2dr(1:nr)  )
#ifdef FULL_DEBUG
      if(o>0.and.ilm==1) write(o,'(3A,9(F10.6,A))') sym, fun, 'shift =', s_vlm(ilm), ' = ', a%vlm(ilm) ,' - ', sum( a%pot(1:nr,ilm,I_VES,I_SMT) * a%s%rcmp(1:nr,ell) * a%s%g%r2dr(1:nr) )
!       if(o>0) write(o,'(3A,I3,9(A,F10.6))') sym, fun, 'shift(ilm=',ilm,') =', s_vlm(ilm), ' = ', a%vlm(ilm) ,' - ', sum( a%pot(1:nr,ilm,I_VES,I_SMT) * a%s%rcmp(1:nr,ell) * a%s%g%r2dr(1:nr) )
#endif

      ! shift both potentials, tru and smt
      do iq = I_TRU, I_SMT
        a%pot(0:nr,ilm,I_VES,iq) = a%pot(0:nr,ilm,I_VES,iq) + s_vlm(ilm)  * a%s%g%r(0:nr)**ell
!         Ees(ilm,iq) = 0.5 * sum( a%pot(1:nr,ilm,I_VES,iq) * a%rho(1:nr,ilm,0,iq) * a%s%g%r2dr(1:nr) )
      enddo ! iq

    enddo ! ilm
!$omp end parallel do

    a%Ees = sum( Ees_lm, dim=1 ) ! ordered summation over the ilm

#ifdef DEBUG
!     if(o>0) write(o,'(3A,99F16.9)') sym, fun, 'Enn(:,tru:smt) =', Ees*eV
    if(o>0) write(o,'(3A,2F16.9,9A)') sym, fun, 'Ees(tru:smt) =', a%Ees*eV, eV_
#endif

#ifdef FULL_DEBUG
    if(o>0) write(o,'(3A,F10.6,9A)') sym, fun, 'shift(ilm=1) =', s_vlm(1)*eV, eV_
!     write(7,'(2A,I6,A,F16.9,9A)') sym, fun, a%ja, ' = ja, shift =', s_vlm(1)*eV, eV_
#endif

#ifdef FULL_DEBUG
    ! show shifted potential at the sphere boundary
    if(o>0) then
      Z = a%s%Z0 * sqrt4Pi
      iq = I_TRU ; write(o,'(3A,9(3F16.9,A))') sym, fun, 'VHt(:R)=', a%pot(nr-2:nr,1,I_VES,iq)/sqrt4Pi*eV, eV_
      iq = I_TRU ; write(o,'(3A,9(3F16.9,A))') sym, fun, 'Ves(:R)=', (a%pot(nr-2:nr,1,I_VES,iq)-Z/a%s%g%r(nr-2:nr))/sqrt4Pi*eV, eV_
      iq = I_SMT ; write(o,'(3A,9(3F16.9,A))') sym, fun, 'Ves(:R)=', a%pot(nr-2:nr,1,I_VES,iq)/sqrt4Pi*eV, eV_
    endif ! o/=0
#endif


#ifdef FULL_DEBUG
    ! show results for the potentials: watch out, the true Coulomb -Z/r potential is still missing
    open(9,file='dmp/ves',iostat=ios)
    if(ios==0) then
      do ir = 1, size(a%rho,1)
        write(9,'(9ES16.6)') a%s%g%r(ir), a%pot(ir,1,I_VES,I_TRU:I_SMT)/sqrt4pi, a%rho(ir,1,0,I_TRU:I_SMT)/sqrt4pi
      enddo ! ir
      close(9)
    endif ! ios
#endif


!     ! coupling of the core to the external electrostatics (other cores etc.)
!     Z = a%s%Z0 / sqrt4Pi
!     EvZ = -Z * s_vlm(1)

    ! now compute the energy of the density in the (external) potential of the nucleus
    Z = a%s%Z0 * sqrt4Pi
    EnZ = -Z * sum( a%rho(1:nr,1,0,I_TRU) * a%s%g%r(1:nr) * a%s%g%dr(1:nr) ) ! integral over density with Z/r

!     a%Ees(I_TRU) = a%Ees(I_TRU) + EnZ + 0.5*EvZ
    a%Ees(I_TRU) = a%Ees(I_TRU) + EnZ
#ifdef DEBUG
!     if(o>0) write(o,'(3A,9(F16.9,A))') sym, fun, 'EvZ =', EvZ*eV, eV_
    if(o>0) write(o,'(3A,9(F16.9,A))') sym, fun, 'EnZ =', EnZ*eV, eV_
    if(o>0) write(o,'(3A,9(F16.9,A))') sym, fun, 'Ees(tru) =', a%Ees(I_TRU)*eV, ' Ees(smt) =', a%Ees(I_SMT)*eV, eV_
#endif

    ! add the true Coulomb -Z/r potential to the electrostatic potential
    Z = a%s%Z0 * sqrt4Pi
    a%pot(1:nr,1,I_VES,I_TRU) = a%pot(1:nr,1,I_VES,I_TRU) - Z/a%s%g%r(1:nr)
    ! the potential value at r=0 is not important since all Hamiltonian
    ! matrix are integrated with r^2dr and r^2dr(r=0) vanishes.
#ifdef FULL_DEBUG
    if(o>0) then ! to check, whether smt and tru potential assume the same values at Rcut for ilm==1
      iq = I_TRU ; write(o,'(3A,9(3F16.9,A))') sym, fun, 'Ves(:R)=', a%pot(nr-2:nr,1,I_VES,iq)/sqrt4Pi*eV, eV_
      iq = I_SMT ; write(o,'(3A,9(3F16.9,A))') sym, fun, 'Ves(:R)=', a%pot(nr-2:nr,1,I_VES,iq)/sqrt4Pi*eV, eV_
    endif ! o>0
#endif

#ifdef FULL_DEBUG
    ! show results for the potentials: here, the true Coulomb -Z/r potential is included
    open(9,file='dmp/ves_Z',iostat=ios)
    if(ios==0) then
      do ir = 1, size(a%rho,1)
        write(9,'(9ES16.6)') a%s%g%r(ir), a%pot(ir,1,I_VES,I_TRU:I_SMT)/sqrt4pi, a%rho(ir,1,0,I_TRU:I_SMT)/sqrt4pi
      enddo ! ir
      close(9)
    endif ! ios
#endif
  endfunction ! es_in_the_spheres

  !! compute the radial electrostatic potentials
  !! without external boundary conditions
  status_t function Hartree_potential_ell( ell, rho, ves, r, drdi ) result( ist )
  use constants, only: Pi
    integer, intent(in)           :: ell
    real, intent(in)              :: rho(0:) ! (nr) ! multipole
    real, intent(out)             :: ves(0:) ! (nr) ! hartree potential
    real, intent(in)              :: r(0:), drdi(0:) ! (nr) ! radial grid

    character(len=*), parameter   :: fun = ' Hartree_potential: '
    real                          :: fourpi_2ellp1, vh10, vh20
    integer                       :: ir, nr

    nr = ubound(rho,1)
#ifdef DEBUG
    if( size( rho, 1 ) /= nr+1 ) stop 'Hartree_potential: dim #1 of RHO wrong.'
    if( size( ves, 1 ) /= nr+1 ) stop 'Hartree_potential: dim #1 of ves wrong.'
    if( size(   r, 1 ) < nr+1 ) stop 'Hartree_potential: dim of R wrong.'
    if( size(drdi, 1 ) < nr+1 ) stop 'Hartree_potential: dim of DRDI wrong.'
#endif
    ist = min( 0, ell ) ! status is 0 if ell >= 0, non-zero else

    fourpi_2ellp1 = (4.*Pi)/(2.*ell+1.)

    !         /R
    ! vH_20 = |dr \rho_{\ell m}^{\sigma}(r) * r^{1-\ell}
    !         /0
    vh20 = 0.
    do ir = 1, nr
      vh20 = vh20 + rho(ir)*r(ir)**(1-ell)*drdi(ir)
    enddo ! ir

    vh10 = 0.
    do ir = 1, nr
      vh10 = vh10 + rho(ir)*r(ir)**(2+ell)*drdi(ir)
      vh20 = vh20 - rho(ir)*r(ir)**(1-ell)*drdi(ir)
      Ves(ir) = fourpi_2ellp1 * ( vh10*r(ir)**(-ell-1) + r(ir)**ell*vh20 )
    enddo ! ir

  endfunction ! Hartree_potential_ell



#ifdef EXTENDED
!+ extended

  status_t function test( )
    write(*,*,iostat=test) __FILE__,' no module test implemented!'
  endfunction ! test

!- extended
#endif

endmodule ! radial_Hartree

#include "config.h"

! #define DEBUG
#ifdef DEBUG
!!! remove comment from debug line
#define cDBG
#else
!!! comment line
#define cDBG !DBG
#endif
!! @author Paul Baumeister, Andrea Nobile
!! 
!! Andrea Nobile 2014 moved stuff from mod_pawdata and implemented stuff to handle gpaw pseudopotentials 
!! PAW element specific data (independent of the atomic position)
module type_species
  use configuration, only: o ! output unit, 0: no output
  use configuration, only: WARNING, ERROR
  use constants, only: NUCLEON_MASS, pse
  use constants, only: Pi, sqrt4Pi
  use type_rgrid, only: rgrid ! radial grid descriptor
  use type_bfun, only: bfun ! Bessel-transformed localized radial function
  use type_comp_desc, only: comp_desc ! compensator shape descriptor 
  use type_spline, only: spline
  use unitsystem, only: eV, eV_, Ang, Ang_
implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: sym = 'tSPECIES' !! module symbol
  character(len=*), parameter, private :: fun = ': '
  public :: species
  public :: species_set
  public :: prepare_species
  public :: matrix_blow_up

#ifdef EXTENDED
  public :: to_string
  public :: test
!   public :: radial_fun ! this function is needed for CYL
!   public :: radial_fun_max ! this function is needed for CYL
!   public :: read_table_of_elements

  interface to_string
    module procedure species2string
  endinterface
#endif

  
  integer, parameter, public :: ELLMAX   = 5 !! s, p, d, f, g, h
  integer, parameter, public :: ENNMAX   = 3 !! '', '*', '**'
  integer, parameter, public :: SPIN     = 1 !! 1: spin degenerate PAW data

  ! important: I_TRU must be smaller than I_SMT, because a lot of
  ! expressions include I_TRU:I_SMT, which is an empty loop otherwise
  integer, parameter, public :: I_TRU  = 1 ! index of the all_electron wave in rwf
  integer, parameter, public :: I_SMT  = 2 ! index of the smooth partial wave in rwf
  integer, parameter, public :: I_PRJ  = 3 ! index of the projector function in rwf

  integer, parameter, public :: I_RPRJ = 4 ! index of the projector function * r in rwf
  integer, parameter, public :: I_FPRJ = 4 ! index of the filtered projector function
  !====================================================================================
  integer, parameter, public :: RWF_DIMENSION = 4 !! allocation limit

  integer, parameter, public :: I_TOT =  0 ! index of the total density
  integer, parameter, public :: I_EXC =  0 ! index of the exchange correlation energy
  integer, parameter, public :: I_VES = -1 ! index of electrostatic potential

  integer, parameter, public :: I_RHOC     = 3 ! + I_TRU or + I_SMT
!   integer, parameter          :: I_TRURHOC  = 4 = I_RHOC + I_TRU ! index of the true core density
!   integer, parameter          :: I_SMTRHOC  = 5 = I_RHOC + I_SMT ! index of the smooth copre density
  integer, parameter, public :: I_VBAR     = 6 ! index of the correction potential
  !====================================================================================
  integer, parameter, public :: RF_DIMENSION = 6 !! allocation limit

  character, parameter, public :: ELLCHAR(-1:ELLMAX+1) = (/'?','s','p','d','f','g','h','+'/)

  integer, parameter, public :: ELLMAX_CMP = 6 ! limiting cutoff for the compensation charges

  character(len=2), parameter, public :: ENNCHAR(0:4) = (/'? ','  ','* ','**','##'/)

  type :: species !! contains all data for the PAW environment
    ! general stuff
    integer             :: iZ = 0          !! atomic number = #of protons
    real                :: Z0 = 0.         !! atomic number = #of protons
    character(len=2)    :: sym = '<>'      !! chemical symbol
    character(len=16)   :: name = ''       !! full name
    real                :: weight = 1.*NUCLEON_MASS !! core weight

    ! PAW method data
    real                :: nve = 0.        !! number of valence electrons
    real                :: nce = 0.        !! number of core electrons
    real                :: ene_ln(2,ENNMAX*(ELLMAX+1)) = 0. !! valence orbital energy
    real                :: occ_ln(2,ENNMAX*(ELLMAX+1)) = 0. !! valence orbital occupation

    real                :: Eatom = 0.     !! total atomic energy
    real                :: e_core_kin = 0. !! kinetic energy of the core electrons
    real                :: es_energy = 0. !! es eneergy


    real                :: coulomb_M             !! same as M in GPAW 
    real                :: zcore  !! electrostatic interaction energy nucleus-core charge
    real                :: delta_zcore

    integer             :: nn( 0:ELLMAX ) = 0    !! number of projectors in each ELL-channel
    integer             :: ellmax = 0            !! highest l_index (lmax)
    integer             :: mln = 0               !! highest ln_index  (ndyd) !! total number of projectors
    integer             :: mlm = 1               !! highest lm_index = (ml+1)**2
    integer             :: mlnm = 0              !! highest lnm_index (nprj)

    integer             :: nr = 0         !! largest cutoff index
    real                :: rcut = 0.      !! universal cutoff radius, max projector rcut before filtering (see pawdata)
    real                :: rcut_chk = 0.     !!rcut for overlap check

    integer             :: ircccut = 0    !! cutoff index for core correction n_c(r)
    real                :: rcccut = 0.    !! cutoff radius for core correction n_c(r)
    integer             :: irvccut = 0    !! cutoff index for potential correction vBAR(r)
    real                :: rvccut = 0.    !! cutoff radius for potential correction vBAR(r)
    real                :: prcut !! real cutoff radius of projector as loaded from file
    !integer             :: nWexp = 3       !! Weinert exponent
    type(comp_desc)     :: cmp_desc 
     ! compensation charge functions on the radial grid
    real, pointer       :: rcmp(:,:) !! radial compensation charge density(nr,0:ELLMAX_RHO)
    type(spline)        :: rcmp_spline(0:ELLMAX_CMP) !! spline containing the compensator shapes
    integer             :: ellmaxcmp !lmax of compensators

    type(rgrid)         :: g            !! radial grid descriptor
    real, pointer       :: rf(:,:)      !! rf(0:g%imx,RF_DIMENSION) radial functions
    real, pointer       :: rwf(:,:,:,:) !! radialwavefunctions(0:g%imx,mln,SPIN,RWF_DIMENSION)

    type(bfun), pointer :: fprj(:) !! filtered projector functions
    type(bfun)          :: rhoc !! filtered smooth core density rhoc
    type(bfun)          :: vbar !! filtered localied potential VBAR
    
  
    real, pointer       :: chdm(:,:,:) !! charge deficit matrix(mln,mln,0:ellmax_rho)
    real                :: q00 = 0. !! monopole moment of the core
    real, pointer       :: overlap_matrix(:,:) !! (mlnm,mlnm)
    real, allocatable   :: delta_n_matrix_full(:,:) !!(mlnm,mlnm) used to compute the atom magnetization


    integer, pointer    :: ind_ell(:) !! (mlnm)
    integer, pointer    :: ind_enn(:) !! (mlnm)
    integer, pointer    :: ind_emm(:) !! (mlnm)
    integer, pointer    :: ind_iln(:) !! (mlnm) combindex ell#enn
    integer, pointer    :: ind_ilm(:) !! (mlnm) combindex ell#emm

   
    real, pointer       :: dkin2(:,:) !! (s%mlnm,s%mlnm) kinetic energy deficit
    real, pointer       :: dkin(:,:) !! (s%mln,s%mln) ! kinetic energy deficit in iln

    real, pointer       :: chdt2(:,:) !! (s%mln*s%mln,s%mln*s%mln) ! charge deficit tensor in iln

    real, allocatable   :: M_p_full(:,:)
    real, allocatable   :: M_ijkl(:, :, :, :)

    real                :: MB !! compensation energy due to smooth core density * vbar     
    real, allocatable   :: MB_p_full(:,:) !!compensation energy due to smooth valence density * vbar     
    
    character(len=128)  :: generator_string !! needed to reproduce these paw data set
  endtype ! species

  


  contains

  type(species) function species_set( Z, iZ, symbol, name, weight, silent ) result( s )
  use configuration, only: o, WARNING
  use type_element, only: atomicnumber_by_symbol, Z_INVALID, Z_MINIMUM, Z_MAXIMUM
  use type_element, only: NUCLEON_MASS, symbol_by_atomicnumber => symbol

    real, intent(in), optional              :: Z
    integer, intent(in), optional           :: iZ
    character(len=*), intent(in), optional  :: symbol
    character(len=*), intent(in), optional  :: name
    real, intent(in), optional              :: weight ! core weight in units of the nucleon mass
    logical, intent(in), optional           :: silent

    logical                                 :: warn
    character(len=*), parameter             :: fun = ' species_set: '

    s%iZ = Z_INVALID

    if( present( symbol ) ) s%iZ = atomicnumber_by_symbol( symbol )
    if( present( Z ) ) s%iZ = min( max( Z_MINIMUM, nint( Z ) ), Z_MAXIMUM )
    if( present( iZ ) ) s%iZ = min( max( Z_MINIMUM, iZ ), Z_MAXIMUM )

    s%Z0 = real( s%iZ ) ; if( present( Z ) ) s%Z0 = Z
    s%sym = symbol_by_atomicnumber( s%iZ )

    if( present( name ) ) s%name = adjustl( name )
    if( s%name == ''  ) then
#ifdef DEBUG
      if(o>0) write(o,'(4A)') sym, fun, WARNING(0), 'empty name, use chemical symbol as name.'
#endif
      s%name = s%sym
    endif ! name == ''

    if( present( weight ) ) s%weight = NUCLEON_MASS*weight

    if( s%iZ == Z_INVALID ) then
      if(o>0) write(o,'(4A,I0,A,F0.3,A)') sym, fun, WARNING(0), 'species has been set, but Z = ', s%iZ, ' (',s%Z0,')'
    endif ! Z is still invalid

    warn = .true. ; if( present( silent ) ) warn = .not. silent
    if( warn ) then
      selectcase( s%iZ ) ! Warnings for special atomic numbers
      case(  -1 ) ; if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'Z = -1 is anti-Hydrogen.'
      case(   0 ) ; if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'Z = 0 is a vacuum-species.'
      case( 121 ) ; if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'Z = 121 is a customized species.'
      case( 100:120 ) ; if(o>0) write(o,'(9A)') sym, fun, 'Z in [100,120] may contain a modified atomic number!'
      endselect ! s%iZ
    endif ! warn
  endfunction ! species_set


  subroutine filter_paw_setup(s, gridspacing, doublegrid, ndkin)
  use type_bfun, only: operator( .at. )
  use type_bfun, only: bfun_filtered, bfun_spline
  
  use type_rgrid, only: operator(.at.)
  use LAPACK, only: LU_decompose, invert
  use LAPACK, only: LU_decomposition_3x3
  use LAPACK, only: invert3x3
  use input, only: eval
  implicit none
    integer, parameter                    :: I_COARSE=1, I_DENSE=2, I_ESTATIX=3
    type(species), intent(inout)          :: s
    real, intent(in)                      :: gridspacing(I_COARSE:I_ESTATIX) !! (/coarse,dense,electrostatix/)
    integer, intent(in)                   :: doublegrid(1:2) !! (/itp,nmesh/)
   
    integer                               :: i, iln, ilm, j, jln, jlm, ij
    integer                               :: ell, enn, emm, ir
    integer                               :: nn, n1, n2, nrc
    real                                  :: GmaxRmax, r, rcr
    real, allocatable                     :: f(:,:), pw(:,:,:)
    real                                  :: prjspw(3,3), left(3,3), upper(3,3), q
    real                                  :: Linv(3,3), Uinv(3,3)
    real                                  :: dkin(3,3,0:5)
    real, intent(out)                     :: ndkin(3,3,0:5)
    integer                               :: ist
    character                             :: l
    !!========================================================================
      !!== Filter the smooth core density ======================================
      !!========================================================================
      s%rhoc = bfun_filtered( s%rf(:s%ircccut,I_RHOC+I_SMT), s%g%r, s%g%dr, ell=0, &
                  h=gridspacing(I_ESTATIX), radiusratio=1.01, ell_in=0, normalize=1, positive=.true. )
      !!========================================================================
      !!== Filter the localized potential correction ===========================
      !!========================================================================
      s%vbar = bfun_filtered( s%rf(:s%irvccut,I_VBAR), s%g%r, s%g%dr, ell=0, &
                  h=gridspacing(I_ESTATIX), radiusratio=1.10, ell_in=0, normalize=0 )

     
      !!========================================================================
      !!== Filter the projectors ===============================================
      !!========================================================================
      rcr = max( 1.0, eval( '$rcr', def=1.2 ) ) ! default is to allow +20%


      if(o>0 .and. rcr /= 1.2 ) write(o,'(3A,F0.3,9A)') sym, fun, 'filtering: new rcut / old rcut = ', rcr
    
!! Rule of Thumb but certainly good for a recommendation
!!
!! Minimum number of grid points for a given configuration of projectors:
!! number of grid points entirely inside a sphere diameter 2*Rcut/h
!!  s:  1..2
!!  s*: 3..4
!!  p:  2..3
!!  p*: 4..5
!!  d:  3..4
!!  d*: 5..6
!!  f:  4..5
!!  f*: 6..7
!!
!!  1...2...3...4...5...6...7...8...9
!!    1...2...3...4...5...6...7...8...9 ! atom is placed exactly ON a grid point (odd)
!!    2...3...4...5...6...7...8...9..10 ! atom is placed BETWEEN two grid points (even)
!!    s       s*      s**
!!        p       p*      p**
!!            d       d*      d**
!!                f       f*      f**
!!
!! ==> better use the second line (even)
!!
!! means that e.g. Cu, rc=2.1 with h=0.6 can never represent a d**-projector
!!  or e.g. O, rc=1.1 with h=0.5 can at most represent a p*-projector

      R = s%rcut / gridspacing(I_COARSE)
      do ell = 0, s%ellmax
        do enn = s%nn(ell), 1, -1
          ! write a warning, if the above rule of thumb has been violated
          if( R < ell+enn ) then
            if(o>0) write(o,'(7A,F0.3,9A)') sym, fun, WARNING(0), &
             'grid very coarse for a ', ELLCHAR(ell), ENNCHAR(enn), '-projector! 2R/h = ', 2.*R
          endif ! R < ...
        enddo ! enn
      enddo ! ell

      allocate( s%fprj( s%mln ), stat=ist )
      if( ist /= 0 ) stop 'pawdata: allocation of filtered projectors (BFUNs) failed.'


      iln = 0
      do ell = 0, s%ellmax
        do enn = 1, s%nn(ell)
          iln = iln + 1
          s%rwf(1:,iln,1,I_PRJ) = s%rwf(1:,iln,1,I_PRJ)*s%g%r(1:)**(-ell)
          s%rwf(0,iln,1,I_PRJ) = s%rwf(1,iln,1,I_PRJ)
          ! filter the projectors and store them on a equidistant radial grid
          s%fprj(iln) = bfun_filtered( s%rwf(:s%nr,iln,1,I_PRJ), s%g%r, s%g%dr, ell=ell, &
                             h=gridspacing(I_COARSE), radiusratio=rcr, ell_in=0, normalize=0 )
!                            h=gridspacing(I_COARSE), radiusratio=rcr, ell_in=0, normalize=2  )
        enddo ! enn
      enddo ! ell

      r = maxval( s%fprj(:)%rcut )
      nrc = s%g .at. r

      if(o>0) write(o,'(3A,F0.3,9A)') sym, fun, 'new rcut = ', r*Ang, Ang_


      iln = 0 ! offset
      do ell = 0, s%ellmax
        nn = s%nn(ell) ! abbr.
        l = ELLCHAR(ell)

        dkin(:,:,ell) = 0.
        dkin(1:nn,1:nn,ell) = s%dkin(iln+1:iln+nn,iln+1:iln+nn)
        dkin(:,:,ell) = 0.5 * ( dkin(:,:,ell) + transpose( dkin(:,:,ell) ) ) ! symmetrize
       
        prjspw = identity( 3 )

        do n1 = 1, nn ! loop over projectors
          s%rwf(:,iln+n1,SPIN,I_FPRJ) = s%fprj(iln+n1) .at. s%g%r ! eval in real space 
          do n2 = 1, nn ! loop over smooth partial waves
            prjspw(n2,n1) = radial_scalar_product( &
                  ! smooth partial wave x filtered projector function (givens as prj*r^-l)
                  s%rwf(:,iln+n2,SPIN,I_SMT), s%rwf(:,iln+n1,SPIN,I_FPRJ), s%g%r, s%g%r2dr, rpower=ell )
          enddo ! n2
          ! show matrix
cDBG      if(u>0) write(u,'(4A,I0,A,9F10.6)') sym, fun, '<',l,n1, '|...>   ', prjspw(:,n1)
        enddo ! n1

        call LU_decomposition_3x3( prjspw, left, upper )
        
        if( invert3x3(  left, Linv ) == 0. ) stop 'PAWdata: result L of LU decomposition cannot be inverted!'
        if( invert3x3( upper, Uinv ) == 0. ) stop 'PAWdata: result U of LU decomposition cannot be inverted!'

        allocate( pw(size(s%rwf,1),3,2) ) ! temp. partial waves
        pw = 0.
        pw(:,1:nn,:) = s%rwf(:,iln+1:iln+nn,SPIN,I_TRU:I_SMT) ! copy

        do n1 = 1, nn
          s%rwf(:,iln+n1,SPIN,I_TRU:I_SMT) = 1.0 * pw(:,n1,:) ! Linv(n1,n1) is always 1.0
          do n2 = 1, n1-1 ! loop over other smooth and true partial waves
            s%rwf(:,iln+n1,SPIN,I_TRU:I_SMT) = s%rwf(:,iln+n1,SPIN,I_TRU:I_SMT) + Linv(n1,n2) * pw(:,n2,:)
cDBG        if(u>0) write(u,'(6A,F10.6,9A)') sym, fun, 'modify partial wave ', l, ennchar(n1), ' with', Linv(n1,n2), ' ', l, ennchar(n2)
          enddo ! n2
        enddo ! n1
        deallocate( pw )

        ! modify the kinetic energy deficit according to the changes in partial waves
        ndkin(:,:,ell) = matmul( Linv, matmul( dkin(:,:,ell), transpose(Linv) ) )

        ! modify the projectors with U
        allocate( f(size(s%fprj(1)%f),1:3) )
        f = 0.
        do n1 = 1, nn
          f(:,n1) = s%fprj(iln+n1)%f
          s%fprj(iln+n1)%f = 0.
        enddo ! n1

        do n1 = 1, nn
          do n2 = n1, nn 
            s%fprj(iln+n2)%f = s%fprj(iln+n2)%f + Uinv(n1,n2) * f(:,n1)
          enddo ! n2
        enddo ! n1
        deallocate( f )
      
         iln = iln + nn ! forward index
       enddo ! ell

        iln = 0

        do ell = 0, s%ellmax
          nn = s%nn(ell) ! abbr.
          do n1 = 1, nn ! loop over projectors
            s%rwf(:,iln+n1,SPIN,I_PRJ) = s%fprj(iln+n1) .at. s%g%r ! eval in real space 
          enddo
          do n1 = 1, nn ! loop over projectors
            s%rwf(1:,iln+n1,SPIN,I_PRJ) = s%rwf(1:,iln+n1,SPIN,I_PRJ)*s%g%r(1:)**(ell) 
          enddo
          s%rwf(0,iln+n1,SPIN,I_PRJ) = s%rwf(1,iln+n1,SPIN,I_PRJ)
          iln = iln + nn ! forward index
        enddo

    

  endsubroutine


  status_t function prepare_species( s, gridspacing, doublegrid, ndkin ) result( ist )
  use type_bfun, only: operator( .at. )
  use type_bfun, only: bfun_filtered, bfun_spline
  use type_rgrid, only: operator(.at.)
  use LAPACK, only: LU_decompose, invert
  use LAPACK, only: LU_decomposition_3x3
  use LAPACK, only: invert3x3
  ! use all_electron, only: write2file
  use input, only: eval
  implicit none

    character(len=*), parameter           :: BL = '                 '
    integer, parameter                    :: I_COARSE=1, I_DENSE=2, I_ESTATIX=3
  
    type(species), intent(inout)          :: s
    real, intent(in)                      :: gridspacing(I_COARSE:I_ESTATIX) !! (/coarse,dense,electrostatix/)
    integer, intent(in)                   :: doublegrid(1:2) !! (/itp,nmesh/)
   
    integer                               :: i, iln, ilm, j, jln, jlm, ij, iatm3
    integer                               :: ell, enn, emm, ir
    integer                               :: nn, n1, n2, nrc
    real                                  :: GmaxRmax, r, rcr, q
    real, allocatable                     :: f(:,:), pw(:,:,:)
    
    real                                  :: dkin(3,3,0:5)
    real, intent(out)                     :: ndkin(3,3,0:5)
    logical                               :: filter
    character                             :: l
    

    ! find cutoff index of the smooth potential correction
    ir = s%g%imx
    do while( s%rf(ir,I_VBAR) == 0. .and. ir > 1 )
      ir = ir-1
    enddo ! while
    !s%irvccut = s%g .at. s%rcut !ir
    s%irvccut = ir
    s%rvccut = s%g .at. ir

    ! find cutoff index of the smooth core density
    q = 0. ! charge outside r
    ir = s%g%imx
    do while( q < 1.E-9 .and. ir > 1 )
      q = q + s%rf(ir,I_RHOC+I_SMT)*s%g%r2dr(ir)*sqrt4pi
      ir = ir-1
    enddo ! while
    s%ircccut = ir
    s%rcccut = s%g .at. ir

    if(o>0) write(o,'(3A,9(F0.3,A))') sym, fun, 'Rcut = ', ( s%g .at. s%irvccut )*Ang, '  Rcore = ', ( s%g .at. s%ircccut )*Ang, Ang_

    if( gridspacing(I_COARSE) < gridspacing(I_DENSE) ) stop 'prepare_species: gridspacings are interchanged!'

    ! count the number of projectors
    i = 0
    do ell = 0, s%ellmax
      i = i + s%nn(ell)*(2*ell+1)
    enddo ! ell
    s%mlnm = i

    ! if(o>0) write(o,'(A,I3,A,I3)') 'PAWdata: Z =', s%Z, ' mprj =', s%mlnm

    ! create the index lists ind_...
    allocate( s%ind_ell(s%mlnm), s%ind_enn(s%mlnm), s%ind_emm(s%mlnm) )
    allocate( s%ind_iln(s%mlnm), s%ind_ilm(s%mlnm) ) ! combindices

    i = 0
    iln = 0
    do ell = 0, s%ellmax
      do enn = 1, s%nn(ell)
        iln = iln+1
        ilm = ell**2 ! offset
        do emm = -ell, ell
          ilm = ilm+1
          i = i+1

          s%ind_ell(i) = ell
          s%ind_enn(i) = enn
          s%ind_emm(i) = emm
          s%ind_iln(i) = iln
          s%ind_ilm(i) = ilm

        enddo ! emm
      enddo ! enn
    enddo ! ell
    
    filter = .false.
    if (filter) then 

      call filter_paw_setup(s, gridspacing, doublegrid, ndkin)

    else ! no filter

      s%rhoc = bfun_spline( s%rf(:,I_RHOC+I_SMT), s%g, ell=0, rc=s%rcccut)
      
      s%vbar = bfun_spline( s%rf(:,I_VBAR), s%g, ell=0, rc=s%rvccut)

      iln = 0 ! offset
      do ell = 0, s%ellmax
        nn = s%nn(ell) ! abbr.
        dkin(:,:,ell) = 0.
        dkin(1:nn,1:nn,ell) = s%dkin(iln+1:iln+nn,iln+1:iln+nn)
        dkin(:,:,ell) = 0.5 * ( dkin(:,:,ell) + transpose( dkin(:,:,ell) ) ) ! symmetrize
        ndkin(:,:,ell) = dkin(:,:,ell)
        do n1 = 1, nn ! loop over projectors
          s%rwf(:,iln+n1,SPIN,I_FPRJ) = s%rwf(:,iln+n1,SPIN,I_PRJ)
        enddo 
        iln = iln + nn ! forward index
      enddo

      allocate( s%fprj( s%mln ), stat=ist )
      if( ist /= 0 ) stop 'pawdata: allocation of filtered projectors (BFUNs) failed.'


      iatm3 = s%g .at. 0.001
      !write (*,*) 'iatm3', iatm3
      iln = 0
      do ell = 0, s%ellmax
        do enn = 1, s%nn(ell)
          iln = iln + 1
          s%rwf(1:,iln,SPIN,I_PRJ) = s%rwf(1:,iln,SPIN,I_PRJ)*s%g%r(1:)**(-ell)    
          !s%rwf(0,iln,SPIN,I_PRJ) = s%rwf(1,iln,SPIN,I_PRJ) 
          if(ell > 0) then 
            s%rwf(0:iatm3,iln,SPIN,I_PRJ) = s%rwf(iatm3,iln,SPIN,I_PRJ)
          endif

          s%fprj(iln) = bfun_spline( s%rwf(:,iln,SPIN,I_PRJ), s%g, ell=ell, rc=s%prcut)
          !CHECK THE SPLINE ON FILE AND THE OLD WAY
          s%rwf(1:,iln,SPIN,I_PRJ) = s%rwf(1:,iln,SPIN,I_PRJ)*s%g%r(1:)**(ell)   
          s%rwf(0,iln,SPIN,I_PRJ) = s%rwf(1,iln,SPIN,I_PRJ)
        enddo ! enn
      enddo ! ell
      !ist = write2file('projectors_sss', s%rwf(0:,:,SPIN,I_PRJ), grid=s%g%r)
    !stop
    end if !filter / nofilter


    !
    !                     /R
    ! chdm(iln,jln,ell) = |dr r^2 r^ell [\phi_iln(r)\phi_jln(r)
    !                     /0        - \tilde\phi_iln(r)\tilde\phi_jln(r)]
    !
    ! set up the charge deficit matrix in the radial basis (emm-degenerate)
    allocate( s%chdm(s%mln,s%mln,0:2*s%ellmax), stat=ist ) ; s%chdm = 0.
    if( ist /= 0 ) stop 'PAWdata: allocation of s%chdm failed!'

    do ell = 0, 2*s%ellmax
      do iln = 1, s%mln
        do jln = 1, iln ! triangular loop (symmetric matrix)
          s%chdm(jln,iln,ell) = charge_deficit_matrix( s, ell, iln, jln )
          s%chdm(iln,jln,ell) = s%chdm(jln,iln,ell) ! symmetric
        enddo ! jln
      enddo ! iln
    enddo ! ell



    ! prepare the overlap matrix for the \hat{S} operator
    if( associated( s%overlap_matrix ) ) deallocate( s%overlap_matrix, stat=ist )
    allocate( s%overlap_matrix(s%mlnm,s%mlnm), stat=ist )
    if( ist /= 0 ) stop 'PAWdata: allocation of s%overlap_matrix failed!'
  
    allocate(s%delta_n_matrix_full(s%mlnm,s%mlnm))

    
    ! the rank2 contributions for kinetic energy deficit
    allocate( s%dkin2(s%mlnm,s%mlnm), stat=ist ) ; s%dkin2 = 0.
    if( ist /= 0 ) stop 'set_tensors: allocation of s%DKIN2 failed.'

    !
    ! \hat{S} = unity + \sum_{ij} |p_i> ovlm_{ij} <p_j|
    !

    ell  = 0
    s%overlap_matrix = 0.
    s%delta_n_matrix_full = 0.
    do i = 1, s%mlnm
      iln = s%ind_iln(i)
      ilm = s%ind_ilm(i)
      do j = 1, i ! triangular loop (symmetric matrix)
        !---------------------------------------------------------
        jln = s%ind_iln(j)
        jlm = s%ind_ilm(j)

        if( ilm == jlm ) then ! delta(ilm,jlm)
          s%overlap_matrix(i,j) = s%chdm(iln,jln,ell)
          s%overlap_matrix(j,i) = s%overlap_matrix(i,j) ! symmetric

          s%delta_n_matrix_full(i, j) = delta_n_matrix(s, ell, iln, jln)
          s%delta_n_matrix_full(j, i) = s%delta_n_matrix_full(i, j)

          ! use rotated dkin = ndkin
          s%dkin2(j,i) = ndkin(s%ind_enn(j),s%ind_enn(i),s%ind_ell(i))
          s%dkin2(i,j) = s%dkin2(j,i) ! symmetric

        endif ! delta(ilm,jlm)

      enddo ! j
    enddo ! i

    !write(*,*) 'overlap = ', s%overlap_matrix
    ! set the constant monopole moment of the compensation charges
    ! which contains the negative ion charge and compensation for
    ! the smooth core charge density correction
    s%q00 = q00_monopole( s )
    
  
    
    ist = generate_radial_compensators(s)
    ist = prepare_electrostatic_corrections(s)
    ist = prepare_chdt( s )

  endfunction ! prepare_species

  integer(kind=8) recursive function fact( n )
    integer, intent(in) :: n
    if (n < 2) then 
      fact = 1
    else
      fact = n * fact( n-1 )
    endif
  endfunction ! fact

  status_t function generate_radial_compensators( s ) result( ist )
  use type_comp_desc, only: SHAPE_GAUSS
  use type_spline, only: spline, spline_create, spline_eval
  use constants, only: Pi, sqrt4pi
    type(species), intent(inout)          :: s
    integer                               :: lmaxcmp
    integer                               :: i, ell
    real, allocatable                     :: r(:) 
    real, allocatable                     :: cmp(:)
    integer                               :: ngr
    real                                  :: rcut, alpha, nrm, dr
    real                                  :: coeff, rr
    type(spline)                          :: sp

    ngr = 50 !MUST BE AN EVEN NUMBER TO USE SIMPSON RULE FOR INTEGRAL
    

    lmaxcmp = min(2*s%ellmax, ELLMAX_CMP)
    s%ellmaxcmp = lmaxcmp

    allocate( r(0:ngr), cmp(0:ngr), stat=ist )
    if( ist /= 0 ) stop 'generate_radial_compensator: allocation of arrays failed!'
    
    
    rcut = 1.0*s%rcut
    dr = rcut/real(ngr)
    do i=0, ngr
      r(i) = dr*real(i)
    enddo
    

    selectcase( s%cmp_desc%comp_type )
    case( SHAPE_GAUSS ) ; cmp(:) = exp(-(r(:)/s%cmp_desc%comp_rc)**2) 
    case default        ; stop 'unknown handle for shape function for charge compensator'
    endselect ! comp_type

    !assume we have the gauss compensator function
    alpha = s%cmp_desc%comp_rc**(-2.0)
    
    !allocate old rcmp stuff
    allocate( s%rcmp(0:s%nr,0:2*s%ellmax) )
    s%rcmp = 0.0

    do ell=0, lmaxcmp

      do i=0,ngr
        cmp(i) = (1./sqrt4pi) * (real(fact(ell))/real(fact(2*ell+1))) * &
          ((4.*alpha)**(real(ell)+1.5))*exp(-alpha * r(i)**2) 
      enddo
      cmp(ngr) = 0.0
      
      sp = spline_create(ell, dr, ngr, cmp)
      s%rcmp_spline(ell) = sp
     
      !setup the old array
      do i = 0, s%nr
        rr = s%g%r(i)
        s%rcmp(i,ell) = spline_eval(sp, rr) * (rr**ell) 
      enddo ! i
      
    enddo ! ell
    
    ist = 0

  endfunction ! generate_radial_compensators

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

  !compute dC , dC_ij, dC_ijkl
  !in gpaw setup.py
  !``M``         Constant correction to Coulomb energy
  !``M_p``       Linear correction to Coulomb energy
  !``M_pp``      2nd order correction to Coulomb energy and Exx energy
  status_t function prepare_electrostatic_corrections( s ) result( ist )
  use spherical, only: Gnt => gaunt_tensor
  use type_spline, only: spline_eval
  use constants, only: Pi, sqrt4pi
  implicit none
    type(species), intent(inout)    :: s
    real, allocatable               :: v_hartree_smooth(:)
    real, allocatable               :: v_hartree_true(:)
    real, allocatable               :: v_hartree_g_l0(:)
    real, allocatable               :: g_l0(:)

    real, allocatable               :: wg_l(:,:)
    real, allocatable               :: g_l(:,:)

    real, allocatable               :: wmct(:)
    real, allocatable               :: mct(:)
    

    real, allocatable               :: n_qg(:,:), nt_qg(:,:)
    real, allocatable               :: A_q(:)
    real, allocatable               :: A_ij(:, :)
    real, allocatable               :: MB_ij(:, :)
    real, allocatable               :: MB_q(:)
    real, allocatable               :: A_qq(:, :, :)
    real, allocatable               :: A_ijkl(:, :, :, :, :)
    !real, allocatable               :: M_ijkl(:, :, :, :)
    real, allocatable               :: wn_lq(:, :, :)  !grid, q, ell
    real, allocatable               :: wnt_lq(:, :, :)  !grid, q, ell
    real, allocatable               :: Delta_lq(:, :) !q, ell
    real                            :: mb, ect, ecs, zcore, tt
    integer                         :: i, j, k, l, m, q, nq, ell, emm
    integer                         :: ilm1, ilm2, ilm3, ilm4, iln1, iln2, iln3, iln4, q1, q2
    integer                         :: i1, i2, i3, i4
  
    allocate( v_hartree_true(0:s%g%imx), v_hartree_smooth(0:s%g%imx) )
    allocate( v_hartree_g_l0(0:s%g%imx), g_l0(0:s%g%imx) )
    ! get the potential of rho_core (smooth and true)
    !Hartree_potential_ell( ell, rho, ves, r, drdi ) s%rf(0, I_RHOC+I_SMT), s%rf(0, I_RHOC+I_TRU)
    ist = Hartree_potential_ell(0, s%rf(0:, I_RHOC+I_SMT), v_hartree_smooth(0:s%g%imx), s%g%r(0:), s%g%dr(0:))
    ist = Hartree_potential_ell(0, s%rf(0:, I_RHOC+I_TRU), v_hartree_true(0:s%g%imx), s%g%r(0:), s%g%dr(0:))
  
    ecs = 0.5*sum(v_hartree_smooth(:)*s%rf(:, I_RHOC+I_SMT)*s%g%r2dr(:))
    ect = 0.5*sum(v_hartree_true(:)*s%rf(:, I_RHOC+I_TRU)*s%g%r2dr(:))

    s%MB = -sum(s%rf(:, I_VBAR)*s%rf(:, I_RHOC+I_SMT)*s%g%r2dr(:))
    !write(*,*) 'MB=' , s%MB

    zcore = -sqrt4pi * s%Z0 * sum(s%rf(:, I_RHOC+I_TRU)*s%g%r(:)*s%g%dr(:))

    !write(*,*) 'zcore', zcore, 'gpaw zcore', s%zcore
    s%delta_zcore = zcore - s%zcore
    if (o>0) write(o,*) 'atom type ',s%sym,' delta zcore eV ',s%delta_zcore*27.2113,' eV'
    if (o>0) write(o,*) 'atom type ',s%sym,'       zcore eV ',  zcore*27.2113,' eV'
    if (o>0) write(o,*) 'atom type ',s%sym,'     s%zcore eV ',s%zcore*27.2113,' eV'
    !write(*,*) 'rhocore =', sqrt4pi*sum(s%rf(:, I_RHOC+I_TRU)*s%g%r2dr(:))
    !stop
  
        !mct_g = nct_g + Delta0 * g_lg[0]
        !wmct_g = wnct_g + Delta0 * wg_lg[0]
        !A -= 0.5 * np.dot(mct_g, wmct_g)
        !self.M = A
    do i=0, s%g%imx
      g_l0(i) = spline_eval(s%rcmp_spline(0), s%g%r(i))
    enddo

    ist = Hartree_potential_ell(0, g_l0(0:), v_hartree_g_l0(0:s%g%imx), s%g%r(0:), s%g%dr(0:))

    allocate( wmct(0:s%g%imx), mct(0:s%g%imx) )

    wmct(:) = v_hartree_smooth(:) + v_hartree_g_l0(:)*s%q00
    mct(:) = s%q00*g_l0(:) + s%rf(:, I_RHOC+I_SMT)

    s%coulomb_M = (ect) + s%zcore -0.5*sum(mct(0:)*wmct(0:)*s%g%r2dr(0:)) -s%es_energy
    
    !write(*,*) 'combo = ', 0.5*sum(g_l0(0:)*v_hartree_smooth(0:)*s%g%r2dr(0:))
    !write (*,*) 'coulomb_M ' , s%coulomb_M
    !write (*,*) 'ect ' , ect, 'ecs', ecs, 'zcore', zcore, 'q00', s%q00

    !write (*,*) 'MB ' , s%MB, 'M', s%coulomb_M
    !s%mln == nj
    nq = s%mln * (s%mln + 1) / 2 ! # of unique entries in a symmetric matrix 
    
    allocate(n_qg(0:s%g%imx, nq), nt_qg(0:s%g%imx, nq))
    allocate(A_q(nq))

    q=1
    do i=1, s%mln
      do j=i, s%mln
        n_qg(:,q) = s%rwf(:, i, 1, I_TRU)*s%rwf(:, j, 1, I_TRU) 
        nt_qg(:,q) = s%rwf(:, i, 1, I_SMT)*s%rwf(:, j, 1, I_SMT)  
        q = q+1
      enddo
    enddo


    allocate( wn_lq(0:s%g%imx, nq, 0:2*s%ellmax) )
    allocate( wnt_lq(0:s%g%imx, nq, 0:2*s%ellmax) )

    allocate( Delta_lq(nq, 0:s%ellmax ) )
    do ell = 0, s%ellmax
      do q=1, nq
        Delta_lq(q, ell) = sum((n_qg(:,q) - nt_qg(:,q))*s%g%r(:)**(2+ell)*s%g%dr(:))!!np.dot(n_qg - nt_qg, r_g**(2 + l) * dr_g)
      enddo
    enddo

    !write(*,*) 'delta_lq', Delta_lq(:,1)

    do ell=0, 2*s%ellmax !+ 1
      do q=1,nq
        ist = Hartree_potential_ell(ell, n_qg(0:,q), wn_lq(0:, q, ell), s%g%r(0:), s%g%dr(0:))
        
        ist = Hartree_potential_ell(ell, nt_qg(0:,q), wnt_lq(0:, q, ell), s%g%r(0:), s%g%dr(0:))
      enddo    
    enddo


    allocate (wg_l(0:s%g%imx, 0:ELLMAX_CMP), g_l(0:s%g%imx, 0:ELLMAX_CMP))

    do ell=0, ELLMAX_CMP
      do i=0, s%g%imx
        g_l(i, ell) = spline_eval(s%rcmp_spline(ell), s%g%r(i))*(s%g%r(i)**ell)
      enddo
      ist = Hartree_potential_ell(ell, g_l(0:, ell), wg_l(0:s%g%imx, ell), s%g%r(0:), s%g%dr(0:))
    enddo


    !s%nr = s%g%imx
    do q=1,nq
      A_q(q) = sum(v_hartree_true(:s%nr)*n_qg(:s%nr,q)*s%g%r2dr(:s%nr))!*0.5
      !write(*,*), 'A_q' , A_q(q)
      !write(*,*) 'A_q' , A_q(q), sum(s%rf(:s%nr, I_RHOC+I_TRU)*wn_lq(:s%nr,q,0)*s%g%r2dr(:s%nr))
      !A_q(q) = A_q(q) + 0.5*sum(s%rf(:s%nr, I_RHOC+I_TRU)*wn_lq(:s%nr,q,0)*s%g%r2dr(:s%nr))
      !-= sqrt(4 * pi) * self.Z * np.dot(n_qg, rdr_g)
      !write(*,*), 'A_q' ,  0.5*sum(s%rf(1:s%nr, I_RHOC+I_TRU)*wn_lq(1:s%nr,q,0)*s%g%r2dr(1:s%nr))
      A_q(q) = A_q(q) - sqrt4pi*s%Z0*sum(n_qg(:s%nr,q)*s%g%r(:s%nr)*s%g%dr(:s%nr))
      
      !A_q -= 0.5 * (np.dot(wnt_lqg[0], mct_g) + np.dot(nt_qg, wmct_g))
      !A_q(q) = A_q(q) - 0.5*(sum(wnt_lq(:s%nr, q, 0)*mct(:s%nr)*s%g%r2dr(:s%nr)) + sum(nt_qg(:s%nr, q)*wmct(:s%nr)*s%g%r2dr(:s%nr)))
      A_q(q) = A_q(q) - (sum(nt_qg(:s%nr, q)*wmct(:s%nr)*s%g%r2dr(:s%nr)))
      
      !A_q -= 0.5 * (np.dot(mct_g, wg_lg[0])
      !                + np.dot(g_lg[0], wmct_g)) * Delta_lq[0]
      A_q(q) = A_q(q) - 0.5*( sum(mct(:s%nr)*v_hartree_g_l0(:s%nr)*s%g%r2dr(:s%nr)) + sum(g_l0(:s%nr)*wmct(:s%nr)*s%g%r2dr(:s%nr)) )*Delta_lq(q, 0)
      !write(*,*) 'A_q' , A_q(q)
    enddo


    
    allocate(A_ij(s%mln, s%mln))

    ! create a symmetric mln matrix
    q=1
    do i=1, s%mln
      do j=i, s%mln
        A_ij(i, j) = A_q(q)
        A_ij(j, i) = A_q(q)
        q = q+1
      enddo
    enddo

    allocate(s%M_p_full(s%mlnm, s%mlnm))
    s%M_p_full = 0.
    
    call matrix_blow_up(s, m_ln=A_ij, m_lnm=s%M_p_full)


    do i=1, s%mlnm
      do j=1, s%mlnm
        ilm1 = s%ind_ilm(i)
        ilm2 = s%ind_ilm(j)
        s%M_p_full(i, j) = s%M_p_full(i, j)*Gnt(ilm1, ilm2, 1)
      enddo
    enddo

    !write(*,*) 'M_p ala jurs', s%M_p_full

    allocate(MB_q(nq))

    !fill MB_q
    do q=1,nq
      MB_q(q) = -sum(s%rf(:, I_VBAR)*nt_qg(0:,q)*s%g%r2dr(:))
    enddo
    !write(*,*) 'MB_q', MB_q
    !stop

     ! create a symmetric mln matrix
    allocate(MB_ij(s%mln, s%mln))

    q=1
    do i=1, s%mln
      do j=i, s%mln
        MB_ij(i, j) = MB_q(q)
        MB_ij(j, i) = MB_q(q)
        q = q+1
      enddo
    enddo

    



    allocate(s%MB_p_full(s%mlnm, s%mlnm))
    s%MB_p_full = 0.

    call matrix_blow_up(s, m_ln=MB_ij, m_lnm=s%MB_p_full)

    do i=1, s%mlnm
      do j=1, s%mlnm
        ilm1 = s%ind_ilm(i)
        ilm2 = s%ind_ilm(j)
        s%MB_p_full(i, j) = s%MB_p_full(i, j)*Gnt(ilm1, ilm2, 1)
      enddo
    enddo


    ! now get the C_iiii M_pp
    allocate(A_qq(nq, nq, 0:2*s%ellmax))

   
    do ell = 0, 2*s%ellmax 
      do q1 = 1, nq
        do q2 = 1, nq
          A_qq(q1, q2, ell) = 0.5*sum(n_qg(:,q1)*wn_lq(:, q2, ell)*s%g%r2dr(:))
          A_qq(q1, q2, ell) = A_qq(q1, q2, ell) - 0.5*sum(nt_qg(:,q1)*wnt_lq(:, q2, ell)*s%g%r2dr(:))
          if(ell <= s%ellmax) then
            A_qq(q1, q2, ell) = A_qq(q1, q2, ell) - 0.5*Delta_lq(q1,ell)*sum(wnt_lq(1:,q2, ell)*g_l(1:, ell)*s%g%r2dr(1:))
            A_qq(q1, q2, ell) = A_qq(q1, q2, ell) - 0.5*Delta_lq(q2,ell)*sum(nt_qg(1:,q1)*wg_l(1:, ell)*s%g%r2dr(1:))

            A_qq(q1, q2, ell) = A_qq(q1, q2, ell) -0.5*sum(wg_l(1:, ell)*g_l(1:, ell)*s%g%r2dr(1:))* &
              Delta_lq(q1,ell)*Delta_lq(q2,ell)
          endif

        enddo 
      enddo
      !write(*,*) 'A_qq = ', A_qq(:,1, 0) 
      !write(*,*) 'A_qq = ', A_qq(1,:, 0) 
      !write(*,*) 'last = ', 0.5*Delta_lq(1,0)*sum(wnt_lq(1:,1, 0)*g_l(1:s%nr, 0)*s%g%r2dr(1:s%nr))
      
    enddo

    !write(*,*) 'A_lqq1 = ', A_qq(:,1, 1)
    !write(*,*) 'A_lqq1 = ', A_qq(1,:, 1)
    !stop

    
    allocate(A_ijkl(s%mln, s%mln, s%mln, s%mln, 0:2*s%ellmax ))
    
    do ell = 0, 2*s%ellmax
      q1 = 1
      q2 = 1
      do i=1, s%mln
        do j=i, s%mln
          q2 = 1
          do k=1, s%mln
            do l=k, s%mln
              A_ijkl(i, j, k, l, ell) = A_qq(q1, q2, ell)
              A_ijkl(i, j, l, k, ell) = A_qq(q1, q2, ell)
              A_ijkl(j, i, k, l, ell) = A_qq(q1, q2, ell)
              A_ijkl(j, i, l, k, ell) = A_qq(q1, q2, ell)
              q2 = q2+1
            enddo
          enddo

          q1 = q1+1
        enddo
      enddo
      
    enddo !! ell

    

    ! now create the full 4 index matrix , we have tu use gaunt coeff
    !write(*,*) 'going to allocate '
    allocate(s%M_ijkl(s%mlnm, s%mlnm, s%mlnm, s%mlnm))
    !write (*,*) 'matricione size = ', s%mlnm, 8*(s%mlnm**4)
    s%M_ijkl = 0.

    do i1 = 1, s%mlnm   
      iln1 = s%ind_iln(i1) 
      ilm1 = s%ind_ilm(i1)
      do i2 = i1, s%mlnm    
        iln2 = s%ind_iln(i2) 
        ilm2 = s%ind_ilm(i2)
        do i3 = 1, s%mlnm 
          iln3 = s%ind_iln(i3) 
          ilm3 = s%ind_ilm(i3)
          do i4 = i3, s%mlnm     
            iln4 = s%ind_iln(i4) 
            ilm4 = s%ind_ilm(i4)
            m=1
            tt = 0.0
            do ell = 0, min(2*s%ellmax, 6)
              do emm = -ell, ell
                tt = tt + A_ijkl(iln1, iln2, iln3, iln4, ell)*Gnt(ilm1, ilm2, m)*Gnt(ilm3, ilm4, m)
                m = m+1
              enddo
            enddo

            s%M_ijkl(i1, i2, i3, i4) = tt
            s%M_ijkl(i2, i1, i3, i4) = tt

            s%M_ijkl(i1, i2, i4, i3) = tt
            s%M_ijkl(i2, i1, i4, i3) = tt

            s%M_ijkl(i3, i4, i1, i2) = tt
            s%M_ijkl(i3, i4, i2, i1) = tt

            s%M_ijkl(i4, i3, i1, i2) = tt
            s%M_ijkl(i4, i3, i2, i1) = tt
       
          enddo !i4
        enddo !i3
      enddo !i2
    enddo !i1
      
   !write(*,*) 'megamatrice ', s%M_ijkl(:,:, :, :)
   !stop


    
    
   ist = 0
  endfunction

  ! prepare the charge deficit tensor
  status_t function prepare_chdt( s ) result( ist )
  use spherical, only: Gnt => Gaunt_tensor
  implicit none
    
    character(len=*), parameter     :: fun = ' prepare_chdt: '
    
    type(species), intent(inout)    :: s
   
    integer :: mlm, ilm, i1, i2, i3, i4, ilm1, ilm2, ilm3, ilm4, iln1, iln2, iln3, iln4
    real                :: t, time
    real, allocatable   :: chdt(:,:,:,:)

    mlm = ubound(Gnt,3)
    
    allocate( chdt(s%mlnm,s%mlnm,s%mlnm,s%mlnm), stat=ist )
    if( ist /= 0 ) stop 'prepare_chdt: allocation of CHDT failed!'
    chdt = 0. ! init

 
    do i1 = 1, s%mlnm     ; iln1 = s%ind_iln(i1) ; ilm1 = s%ind_ilm(i1)
      do i2 = 1, i1       ; iln2 = s%ind_iln(i2) ; ilm2 = s%ind_ilm(i2)
        do i3 = 1, s%mlnm ; iln3 = s%ind_iln(i3) ; ilm3 = s%ind_ilm(i3)
          do i4 = 1, i3   ; iln4 = s%ind_iln(i4) ; ilm4 = s%ind_ilm(i4)
            !---------------------------------------------------------
            t = 0. ! init
            do ilm = 1, mlm
              t = t + Gnt(ilm1,ilm2,ilm)*Gnt(ilm3,ilm4,ilm)
            enddo ! ilm
            if( abs(t) < 1E-12 ) cycle
            t = t * charge_deficit_tensor( s, iln1, iln2, iln3, iln4 )
            ! 24 permutations of the indicies {i1,i2,i3,i4}
            chdt(i4,i3,i2,i1) = t
            chdt(i3,i4,i2,i1) = t
            chdt(i4,i2,i3,i1) = t
            chdt(i2,i4,i3,i1) = t
            chdt(i3,i2,i4,i1) = t
            chdt(i2,i3,i4,i1) = t
            chdt(i4,i3,i1,i2) = t
            chdt(i3,i4,i1,i2) = t
            chdt(i4,i1,i3,i2) = t
            chdt(i1,i4,i3,i2) = t
            chdt(i3,i1,i4,i2) = t
            chdt(i1,i3,i4,i2) = t
            chdt(i4,i2,i1,i3) = t
            chdt(i2,i4,i1,i3) = t
            chdt(i4,i1,i2,i3) = t
            chdt(i1,i4,i2,i3) = t
            chdt(i2,i1,i4,i3) = t
            chdt(i1,i2,i4,i3) = t
            chdt(i3,i2,i1,i4) = t
            chdt(i2,i3,i1,i4) = t
            chdt(i3,i1,i2,i4) = t
            chdt(i1,i3,i2,i4) = t
            chdt(i2,i1,i3,i4) = t
            chdt(i1,i2,i3,i4) = t
            !---------------------------------------------------------
          enddo ! i4
        enddo ! i3
      enddo ! i2
    enddo ! i1
  
    if( associated( s%chdt2 ) ) deallocate( s%chdt2, stat=ist )
    allocate( s%chdt2(s%mlnm**2,s%mlnm**2), stat=ist )
    if( ist /= 0 ) stop 'PAWdata: allocation of s%chdt2 failed!'
    s%chdt2 = reshape( chdt, (/s%mlnm**2,s%mlnm**2/) )

  endfunction ! prepare_chdt





  real function q00_monopole( s ) result( q00 )
  use constants, only: sqrt4pi
  use configuration, only: WARNING
  use type_bfun, only: bfun_norm
  implicit none
    ! parameters
    character(len=*), parameter           :: fun = ' q00_monopole: '
    real, parameter                       :: THRESHOLD_WARNING = 1.E-6
    ! arguments
    type(species), intent(in)             :: s
    ! local vars
    real                                  :: q(I_TRU:I_SMT)!, r2dr
!     integer                               :: ir
    real                                  :: qdiff, temp

    ! compute the monopole compensation charge Q_{00}
    !                /R
    !  Q_{00} = 4\pi |dr r^2 Y_{00}(\hat{\vec{r}}) [ n_c(r) - \tilde{n}_c(r) - Z*\delta(r) ]
    !                /0

    q(I_TRU) = sum( s%rf(:,I_RHOC+I_TRU) * s%g%r2dr(:) )

    !renormalize qcore true
    !temp = q(I_TRU)*sqrt4pi
    !temp = real(nint(temp))/sqrt4pi

    !s%rf(:,I_RHOC+I_TRU) = s%rf(:,I_RHOC+I_TRU)*(temp/q(I_TRU))
    !q(I_TRU) = sum( s%rf(:,I_RHOC+I_TRU) * s%g%r2dr(:) )
    

    q(I_SMT) = sum( s%rf(:,I_RHOC+I_SMT) * s%g%r2dr(:) )


    ! the two core densities are normalized according to
    !
    !      /infty
    ! 4\pi |dr  r^2        Y_00 rf(r) = Q_true_core (integer) or Q_smooth_core
    !      /0
    ! [   ...     ]==> int_dV              Y_00 = 1/sqrt(4Pi)

    ! the true core electron charge qtruc is supposed to be this value
    qdiff = q(I_TRU)*sqrt4pi + s%Nve - real(s%Z0)
    if(o>0) write(o, *) sym, fun, "qdiff, q(I_TRU), s%Nve, s%Z0 = ", qdiff, q(I_TRU), s%Nve, s%Z0
      
    if( abs(qdiff) > THRESHOLD_WARNING ) then
      ! show the deviation of the true core density from the analytical value
      if(o>0) write(o,'(5A,F0.3,A,F0.9,9A)') sym, fun, WARNING(0), s%sym, ' (Z=', s%Z0, ') has been generated while charged with ', qdiff, ' e!'
    endif ! deviation


    ! q00 is negative, because a proton''s charge is -1.
    q00 = - ( real(s%Z0)/sqrt4pi - ( q(I_TRU) - q(I_SMT) ) )


  endfunction ! q00_monopole





  !                     /R
  ! chdm(ell,iln,jln) = |dr r^2 r^ell [\phi_iln(r)\phi_jln(r)
  !                     /0             - \tilde\phi_iln(r)\tilde\phi_jln]
  !
  real function charge_deficit_matrix( s, ell, iln, jln ) result( cd )
  implicit none
    type(species), intent(in)             :: s
    integer, intent(in)                   :: ell, iln, jln
    integer                               :: ir

    cd = 0. 
    !do ir = 1, s%nr-1
    do ir = 0, s%g%imx
      cd = cd + (   s%rwf(ir,iln,SPIN,I_TRU)*s%rwf(ir,jln,SPIN,I_TRU) &
                  - s%rwf(ir,iln,SPIN,I_SMT)*s%rwf(ir,jln,SPIN,I_SMT) &
                ) * s%g%r(ir)**ell * s%g%r2dr(ir)
    enddo ! ir
  endfunction ! charge_deficit_matrix

  real function delta_n_matrix( s, ell, iln, jln ) result( cd )
  implicit none
    type(species), intent(in)             :: s
    integer, intent(in)                   :: ell, iln, jln
    integer                               :: ir

    cd = 0. 
    !do ir = 1, s%nr-1
    do ir = 0, s%nr
      cd = cd + (   s%rwf(ir,iln,SPIN,I_TRU)*s%rwf(ir,jln,SPIN,I_TRU) &
                ) * s%g%r(ir)**ell * s%g%r2dr(ir)
    enddo ! ir
  endfunction ! charge_deficit_matrix



  !
  !                           /R
  ! ch(iln1,iln2,iln3,iln4) = |dr r^2 [\phi_1(r)\phi_2(r)\phi_3(r)\phi_4(r)
  !                           /0             - \tilde\phi...]
  !
  real function charge_deficit_tensor( s, iln1, iln2, iln3, iln4 ) result( cd )
  implicit none
    type(species), intent(in)             :: s
    integer, intent(in)                   :: iln1, iln2, iln3, iln4
    integer                               :: ir

    cd = 0. 
    do ir = 1, s%nr-1
      cd = cd + (   s%rwf(ir,iln1,SPIN,I_TRU)*s%rwf(ir,iln2,SPIN,I_TRU) * &
                    s%rwf(ir,iln3,SPIN,I_TRU)*s%rwf(ir,iln4,SPIN,I_TRU) &
                  - s%rwf(ir,iln1,SPIN,I_SMT)*s%rwf(ir,iln2,SPIN,I_SMT) * &
                    s%rwf(ir,iln3,SPIN,I_SMT)*s%rwf(ir,iln4,SPIN,I_SMT) &
                ) * s%g%r2dr(ir)
    enddo ! ir
  endfunction ! charge_deficit_tensor





  ! makes a full mlnm x mlnm matrix m_lnm
  !       out of a mln x mln matrix m_ln
  !    or out of a mlm x mlm matrix m_lm
  subroutine matrix_blow_up( s, m_ln, m_lm, m_lnm )
  implicit none
    type(species), intent(in)             :: s
    real, intent(in), optional            :: m_ln(:,:)  ! (mln,mln)
    real, intent(in), optional            :: m_lm(:,:)  ! (mlm,mlm)
    real, intent(out)                     :: m_lnm(:,:) ! (mlnm,mlnm)
    integer                               :: i, j, iln, jln, ilm, jlm

    m_lnm = 0. ! init

    if( present( m_ln ) ) then
      if( present( m_lm ) ) stop 'matrix_blow_up: either M_LN or M_LM should be present as input, never both.'

      do i = 1, s%mlnm
        iln = s%ind_iln(i)
        do j = 1, s%mlnm
          jln = s%ind_iln(j)
          m_lnm( j, i ) = m_ln( jln, iln )
        enddo ! j
      enddo ! i

      return
    endif ! present( m_ln )

    if( present( m_lm ) ) then
      if( present( m_ln ) ) stop 'matrix_blow_up: either M_LM or M_LN should be present as input, never both.'

      do i = 1, s%mlnm
        ilm = s%ind_ilm(i)
        do j = 1, s%mlnm
          jlm = s%ind_ilm(j)
          m_lnm( j, i ) = m_lm( jlm, ilm )
        enddo ! j
      enddo ! i

      return
    endif ! present( m_lm )

  endsubroutine ! matrix_blow_up

  real function radial_scalar_product( rf1, rf2, r, drdi, rpower ) result( s )
  implicit none   
    real, intent(in)                      :: rf1(0:), rf2(0:), r(0:), drdi(0:)
    integer, intent(in), optional         :: rpower
    integer                               :: nr, ir, p

    p = 0 ; if( present( rpower ) ) p = rpower
    nr = min( size(rf1), size(rf2) )-1

    s = 0.
    do ir = 1, nr
      s = s + rf1(ir) * rf2(ir) * r(ir)**p * drdi(ir)
    enddo ! ir

  endfunction ! radial_scalar_product

  function identity( n ) result( m )
    integer, intent(in) :: n ! matrix dimension
    real                :: m(n,n) ! result

    integer             :: i
    m = 0. ! init
    do i = 1, n
      m(i,i) = 1. ! set diagonal elements to unity
    enddo ! i
  endfunction ! identity



#ifdef EXTENDED
!+ extended

  string_t function species2string( s ) result( str )
    type(species), intent(in) :: s
    status_t :: ios
    write( unit=str, fmt='(I3,3A,F0.2,2A)', IOstat=ios ) s%iZ, ' ', s%sym, ' ', s%weight, ' ', s%name
  endfunction ! species2string

  status_t function test( )
    write(*,*,iostat=test) __FILE__,' no module test implemented!'
  endfunction ! test

!- extended
#endif

endmodule ! type_species

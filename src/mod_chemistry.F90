#include "config.h"

! #define DEBUG
! #define FULL_DEBUG

#define CHECK_RCUT_OVERLAP
! additional to the bond distance, check if the
! PAW cutoff radii overlap stronger than a certain threshold

! #define GeSbTe


! #define BOND_STATISTIX

#ifdef  BOND_STATISTIX
#define DISCRETE_HIST 64
#endif


#ifdef FULL_DEBUG
#define cDBG
#else
#define cDBG !DBG
#endif


!! @author Paul Baumeister
!! @version 3.0
!!
!! find bonds in the system
module chemistry
implicit none
  private ! default for this module namespace

  public :: find_bonds
#if EXTENDED
  public :: test
#endif

  !! the coordination number is truncated to 16,
  !! ==> the 16 smallest bonds length are accounted for
  integer, parameter, public :: MAX_COORDINATION_NUMBER = 16
  integer, parameter :: MXB = MAX_COORDINATION_NUMBER !! abbrev.
  integer, parameter :: MXA = ((MXB-1)*MXB)/2 !! max. number of bond angles

#ifdef BOND_STATISTIX
  real, parameter, private :: LBL = 2*9.44863 ! longest Bond length = 10 Ang
#else
  real, parameter, private :: LBL = 9.44863 ! longest Bond length = 5 Ang
#endif

  character(len=*), parameter, private :: fun = ': ', sym = 'CHM' !! module symbol

  contains

  !! loop over all pairs of atoms and their periodic images (if periodic)
  !! specify if the atom pair forms a bond by looking at a table of bond length
  !! warn for too small distances
  status_t function find_bonds( o, a ) result( ist )
  use type_atom, only: atom
  use configuration, only: WARNING, ERROR
  use constants, only: Pi
  use unitsystem, only: Ang, Ang_
    iounit_t, intent(in)                  :: o ! output
    type(atom), intent(inout)             :: a(:)

#ifdef DEBUG
    character(len=*), parameter :: fun = ' find_bonds: '
#endif
    real, parameter    :: SHORTEST_BOND_LENGTH = 1.4173 ! .75 Ang
    real, parameter    :: LONGEST_BOND_LENGTH = LBL ! defined above
#ifndef KEI
    real, parameter    :: SMALLEST_BOND_ANGLE = 2.*asin( 0.5*SHORTEST_BOND_LENGTH/LBL )
#else
    real, parameter    :: SMALLEST_BOND_ANGLE = 2.*( 0.5*SHORTEST_BOND_LENGTH/LBL )
#endif
    real, parameter    :: LONGEST_BOND_LENGTH_SQRT3 = LONGEST_BOND_LENGTH * 1.7321
    real, parameter    :: LONGEST_BOND_LENGTH_SQ = LONGEST_BOND_LENGTH**2
    logical, parameter :: SHOW_HISTOGRAMM = .true.
    real, parameter    :: DEGREE = 180./Pi! = 360./(2*Pi) ! for bond angles
    real, parameter    :: BL_WINDOW(-1:+1) = (/0.8,1.0,1.25/) ! bond length window
#ifdef CHECK_RCUT_OVERLAP
    ! launch a warning of dist < ( rcut1+rcut2 )*WARN_SPHERE_OVERLAP
    real, parameter    :: WARN_SPHERE_OVERLAP = 0.9 ! 1.0: exact, 0.7: forgiving
#endif
    ! indices
    integer, parameter :: ILENGTH =  0
    integer, parameter :: OTHER_Z = -1
    integer, parameter :: OTHER_I = -2
    integer, parameter :: P_IMAGE = -3

    integer              :: natm, ia, ja
    integer              :: nbs, ib
    integer(kind=8)      :: nb8, na8, mem
    integer              :: ip, np
    real                 :: mbl(-1:+1) = (/1.8,2.,4./) ! bond length window
    real, parameter      :: SHORT_LONG = 1.E6 ! will be ******* in F7.3 format
    real                 :: short =  SHORT_LONG
    real                 :: long  = -SHORT_LONG
    real                 :: dv(3), dist = 0., dist2
    real, allocatable    :: daa(:) ! distance
    integer, allocatable :: baa(:,:) ! bond
    integer, allocatable :: nba(:) ! number of bonds of an atom
    real, allocatable    :: dba(:,:,:) ! direction of bond
    integer              :: nsm = 0 ! number of too small bonds
    integer              :: ipm, i1, ia1, ia2
#ifdef CHECK_RCUT_OVERLAP
    real                 :: Rc1, Rc2 ! PAW cutoff radii
    real                 :: r_overlap, min_r_overlap = 9.
    integer              :: ia_overlap(2) = 0, n_overlap = 0
#endif
    integer              :: n_warn = 0, ia_warn(2) = 0
    real                 :: min_dist = LONGEST_BOND_LENGTH
    ! histogram of different bond lengths
    integer, allocatable   :: histogram(:)
    character, allocatable :: hst(:,:)
    real                   :: bin(-1:1) = (/1.8,0.2,4.0/) ! [start step end]
    integer                :: nbin, ibin, ndigit, mval, ios, n_too_many_bonds=0
    integer                :: na, nb, ic_mxl(1), angle_count(2,MXA)
    integer                :: bond_count(2,MXB), nac, nbc, ic, ibo ! index in coordination
    real                   :: bd(MXB), ba(MXA)!, bv(3,MXB)
    character(len=128)     :: string

cDBG  character(len=6)                      :: how(-1:+1)
cDBG  if(o>0) write(o,'(9A)') sym, fun, 'start'

    ist = 0
    ! number of atoms
    natm = size( a, 1 ) ; if( natm < 1 ) return ! no atoms ==> no bonds

    np = maxval( a(:)%nimages ) ! number of periodic images
cDBG  if( np < 1 ) stop 'CHM find_bonds: a%nimages should be at least 1'
cDBG  if( any( a(:)%nimages < np ) ) stop 'CHM find_bonds: a%nimages should be the same for all atoms'

cDBG  if(o>0 .and. np>1) write(o,'(3A,I0,9A)') sym, fun, 'check ',np,' periodic images.'

cDBG  if(o>0) write(o,'(3A,I0)') sym, fun, 'limit max coordination number to ', MXB

    ! number of all possible bonds
    na8 = natm
    nb8 = na8*(na8+1)/2*np-na8 ! no atom can bond with its own original image ==> -natm
    if( nb8 > MXB*natm*8 ) then
cDBG  if(o>0) write(o,'(3A,I0)') sym, fun, 'theoretical max. number of bonds is ', nb8
#ifdef BOND_STATISTIX
      nb8 = MXB*na8*64 ! upper limit
#else
      nb8 = MXB*na8*8 ! upper limit
#endif
cDBG  if(o>0) write(o,'(3A,I0)') sym, fun, 'limit max. number of bonds to ', nb8
    endif ! nb too large
    nbs = nb8 ! conversion to regular integer

    mem = 8*nbs+4*3*nbs+4*natm+8*7*MXB*natm
    if(mem>2**24 .and. o>0) write(o,'(3A,F0.3,9A)') sym, fun, 'try to allocate ', mem/2.**20, ' MiByte'
    allocate( daa(nbs), baa(0:2,nbs), nba( natm ), dba(P_IMAGE:3,MXB,natm), stat=ist )
    if( ist /= 0 .and. o>0) write(o,'(3A,F0.3,9A)') sym, fun, 'allocation of ', mem/2.**20, ' MiByte failed!'
    if( ist /= 0 ) return
    ! initialize arrays
    daa = 0. ! compute the atom-atom distances
    baa = 0  ! atom index of the bond
    nba = 0  ! number of bonds of this atom
    dba = 0. ! direction of bond, components(-3:0) keep extra information


cDBG  if(o>0) write(o,'(3A,2(F6.1,A))') sym, fun, 'between', mbl(-1)*Ang, ' and', mbl(+1)*Ang, Ang_

    ! decide if there is a bond
    ibo = 0 ! init counter for all bonds
    ! loop over all atoms

!$omp parallel do private(Rc1,Rc2,r_overlap,ja,mbl,ip,dv,dist2,dist,ib,i1,ia1,ia2,ipm,ic,ic_mxl) schedule(dynamic,1)
    do ia = 1, natm
cDBG  if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'ia = ', ia
#ifdef CHECK_RCUT_OVERLAP
      Rc1 = a(ia)%s%Rcut_chk
#endif
      ! and all other atoms including the atom itself,
      do ja = 1, ia ! for the periodic images (e.g. only one atom, but periodic can have bonds...)
cDBG    if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'ja = ',ja
#ifdef CHECK_RCUT_OVERLAP
        Rc2 = a(ja)%s%Rcut_chk
#endif
        !
        ! mbl(-1) and mbl(+1) are the criteria for a bond
        ! these are a function of the species Zi and Zj
        !     [0.9 1.2]
        mbl = BL_WINDOW * def_bond_length( a(ia)%s%iZ, a(ja)%s%iZ )
        ! loop over all periodic images
        do ip = 1, a(ja)%nimages
          ! position difference vector
          dv(1:3) = a(ja)%imagepos(1:3,ip) - a(ia)%pos(1:3)

!           ! in order to scale better, we leave out any distance that is unreasonably long
!           ! therefore, we could compare vlength( dv ) > LONGEST_BOND_LENGTH ). However,
!           ! vlength involves a sqrt operation. The abs-norm is much faster.
!           ! The abs-norm of dv will set up a octahedron |x|+|y|+|z| = LBL * sqrt(3)
!           ! any vector that lies outside of this octahedron is definitely outside
!           ! of the sphere with radius LONGEST_BOND_LENGTH (LBL)
!           if( abs(dv(1))+abs(dv(2))+abs(dv(3)) > LONGEST_BOND_LENGTH_SQRT3 ) cycle
!           dist = vlength( dv(1:3) ) ! compute exact distance

          ! in order to scale better, we leave out any distance that is unreasonably long
          ! therefore, we could compare vlength( dv ) > LONGEST_BOND_LENGTH ). However,
          ! vlength involves a sqrt operation. Comparing the length^2 is much faster:
          dist2 = dv(1)*dv(1)+dv(2)*dv(2)+dv(3)*dv(3)
          if( dist2 > LONGEST_BOND_LENGTH_SQ ) cycle
          dist = sqrt( dist2 ) ! compute exact distance

          ! check for small distances
          if( dist < SHORTEST_BOND_LENGTH ) then
            if( ia == ja ) cycle ! no bonding with itself
            ! the bond is smaller than 1.5 bohr
!$omp atomic
            n_warn = n_warn + 1

            if( dist < min_dist ) then
!$omp critical
              min_dist = dist
              ia_warn = (/ia,ja/)
!$omp end critical
            endif ! new smallest distance
cDBG        if(o>0) write(o,'(4A,2(I0,A),F0.3,A)') sym, fun, 'Caution! ', &
cDBG          'atom #', a(ja)%ja, ' and #', a(ia)%ja, ' are very close, distance = ', dist*Ang, Ang
          endif ! dist < SHORTEST_BOND_LENGTH

#ifdef CHECK_RCUT_OVERLAP
          ! check for disjoint PAW augmentation spheres
          if( dist < ( Rc1 + Rc2 )*WARN_SPHERE_OVERLAP ) then
!$omp atomic
            n_overlap = n_overlap+1 ! count up
            r_overlap = dist / ( Rc1 + Rc2 ) ! ratio
            if( r_overlap < min_r_overlap ) then
!$omp critical
              min_r_overlap = r_overlap ! set new minimum
              ia_overlap = (/ia,ja/)
!$omp end critical
            endif ! new minimum
cDBG        if(o>0) write(o,'(3A,2(I0,A))') sym, fun, 'augmentation spheres of atom #', a(ja)%ja, ' and #', a(ia)%ja, ' are not disjoint.'
          endif ! dist < allowed
#endif

          if( dist < LONGEST_BOND_LENGTH ) then ! this restriction is for scaling of the algorithm
#ifdef BOND_STATISTIX
            write(8,'(2(A3,I4),F12.6)') a(ia)%s%sym, a(ia)%s%Z, a(ja)%s%sym, a(ja)%s%Z, dist*Ang
#endif
          !================================================
!$omp critical
            ib = ibo+1 ! create new bond index
            ibo = ib ! update bond index
            ! find extrema
            short = min( short, dist )
            long  = max( long,  dist )
!$omp end critical

cDBG        if( ib > size(daa) ) then ; write(*,*) 'CHM find_bonds: fatal error. NB too small!' ; return ; endif
!             if( ib > size(daa) ) stop 'CHM find_bonds: number of bonds is larger than array.'
            if( ib > size(daa) ) then
              ib = size(daa) ! results will be wrong
!$omp atomic
              n_too_many_bonds = n_too_many_bonds+1
            endif ! ib > size(daa)

            daa(ib) = dist ! store distance between the two atoms

            if( dist <= mbl(+1) ) then
              if( dist >= mbl(-1) ) then
                ! store general information
                baa(1,ib) = ia ! the atom
                baa(2,ib) = ja ! other atom
                baa(0,ib) = ip ! periodic image

        !------------------------------------------------
cDBG    how = '' ! init
        !------------------------------------------------
        ! store bond in both atom''s bond lists.
        ! However, if one of the partners is overcoordinated,
        ! the bond is not stored in its coordintaion list.
        !------------------------------------------------
        ! do twice, because a bond is a symmetric relation
        ! if ia is not the same as ja, because that would lead to double counting of bonds
        i1 = +1 ; if( ia == ja ) i1 = -1
          ia1 = ja ; ia2 = ia ! 1st time interchanged
        do ipm = -1, i1, 2

!$omp critical
          nba(ia1) = nba(ia1)+1 ! count bond at atom i

          if( nba(ia1) <= MXB ) then
            ic = nba(ia1) ! index of new coordination is the number of bonds
! cDBG        if(o>0) write(o,'(3A,3I4,A,2I4,A,F6.1,2A,I3)') sym, fun, &
! cDBG          'new coordination  [ia,ja,ip]', ia1,ia2,ip, &
! cDBG          ' Z,Z', a(ia1)%s%Z, a(ia2)%s%Z, ' d=', dist*Ang, Ang_,', ic=', ic
cDBG        how(ipm) = ' new  '
          else  ! nba(ia) < MXC
            ! if all places have been taken,
            ! replace the longest bond, ...
            ic_mxl = maxloc(dba(ILENGTH,1:MXB,ia1)) ; ic = ic_mxl(1) ! ic = index of the longest bond

            if( dist > dba(ILENGTH,ic,ia1) ) then
              ! if the new bond is longer than the longest bond stored in the coordination list
              ic = 0 ! 0: don''t replace
cDBG          how(ipm) = ' dump '
            else  ! new dist > stored dist(ic)
! cDBG          if(o>0) write(o,'(3A,3I4,A,2I4,A,F6.1,2A,I3)') sym, fun, &
! cDBG            'replace coord. by [ia,ja,ip]', ia1,ia2,ip, &
! cDBG            ' Z,Z', a(ia1)%s%Z, a(ia2)%s%Z, ' d=', dist*Ang, Ang_, ', ic=', ic
cDBG          how(ipm) = ' repl '
            endif ! new dist > stored dist(ic)

          endif ! nba(ia1) < MXC
!$omp end critical

          ! store:
          if( ic > 0 ) then
            dba(  1:3  ,ic,ia1) = dv*ipm      ! direction of bond
            dba(ILENGTH,ic,ia1) = dist        ! bond length information
            dba(OTHER_Z,ic,ia1) = a(ia2)%s%Z0 ! information of the other core
            dba(OTHER_I,ic,ia1) = real( ia2 ) ! index of the other atom
            dba(P_IMAGE,ic,ia1) = real( ip )  ! periodic image index
! cDBG        if(o>0) write(o,'(3A,I4,A,I3,A,2I4,A,F6.1,2A,3F6.1)') sym, fun, &
! cDBG          'coordination ia=', ia1, ' ic=', ic, &
! cDBG          ' Z,Z', a(ia1)%s%Z, a(ia2)%s%Z, ' d=', dist*Ang, Ang_, ' dv=', dv*ipm
          endif ! ic > 0

          ia1 = ia ; ia2 = ja ! 2nd time straight
        enddo ! ipm = -1, +1, 2
cDBG    if(o>0) write(o,'(3A,I4,3A,I4,3A,F6.2,A,3F6.2,9A)') sym, fun, &
cDBG      'coordination ia=', ia, how(-1), a(ia)%s%sym, ' --- ja=', ja, how(+1), a(ja)%s%sym, &
cDBG      '  d=', dist, ' au, dv=', dv
        !------------------------------------------------



              else  ! dist >= mbl(-1)
!$omp atomic
                nsm = nsm + 1 ! count the number of too small bonds

cDBG            if(o>0) write(o,'(3A,3I4,2(A,F6.1))') sym, fun, 'short bond [ia,ja,ip]', ia,ja,ip, ' d=', dist*Ang, Ang_
                baa(:,ib) = 0
              endif ! dist >= mbl(-1)
            else  ! dist <= mbl(+1)
              ! long bond
              baa(:,ib) = 0
            endif ! dist <= mbl(+1)
          !================================================
          endif ! LONGEST_BOND_LENGTH

        enddo ! ip
      enddo ! ja
    enddo ! ia
!$omp end parallel do
    nbs = ibo ! the new upper limit is the number of bonds < LONGEST_BOND_LENGTH


    if( n_too_many_bonds > 0 ) then
      if(o>0) write(o,'(3A,I0,9A)') sym, fun, WARNING(0), &
        n_too_many_bonds, ' bonds could not be counted, result may be wrong!'
    endif ! n_too_many_bonds

    if( n_warn > 0 ) then
      if( any( ia_warn < 0 .or. ia_warn > natm ) ) stop 'CHM find_bonds: IA_WARN out of bounds!'
      if(o>0) write(o,'(4A,I0,A,F0.3,6A,F0.3,9A)') sym, fun, WARNING(0), &
        'in ',n_warn,' situations, distances < ', SHORTEST_BOND_LENGTH*Ang, Ang_, '. Especially for ', &
          trim(a(ia_warn(1))%s%sym), '--', trim(a(ia_warn(2))%s%sym), ' (', min_dist*Ang, Ang_, ')'
    endif ! launch warnings for small distances

#ifdef CHECK_RCUT_OVERLAP
    ! warn when non-disjoint PAW augmentation spheres have been detected
    if( n_overlap > 0 ) then
      if( any( ia_overlap < 0 .or. ia_overlap > natm ) ) stop 'CHM find_bonds: IA_OVERLAP out of bounds!'
      if(o>0) write(o,'(4A,I0,A,I0,5A,I0,9A)') sym, fun, WARNING(0), &
        'in ',n_overlap,' situations, radii overlap > ', &
          nint(100*(1.-WARN_SPHERE_OVERLAP)), '%. Especially for ', &
          trim(a(ia_overlap(1))%s%sym), '--', trim(a(ia_overlap(2))%s%sym), ' (', nint(100*(1.-min_r_overlap)), '%)'
      ! recompute the bond distance between the two most critical atoms
      dist = ( a(ia_overlap(1))%s%Rcut + a(ia_overlap(2))%s%Rcut ) * min_r_overlap
      if(o>0) write(o,'(4A,2(I0,A),F0.3,9A)') sym, fun, ' ... ', &
        'between atom #',a(ia_overlap(1))%ja,' and #',a(ia_overlap(2))%ja,' the distance is ',dist*Ang, Ang_
    endif ! dist < allowed
#endif


cDBG  if(o>0) then
cDBG    do ia = 1, natm
cDBG      write(o,'(/3A,I6)') sym, fun, 'ia=', ia
cDBG      do ic = 1, min(nba(ia),MXB)
cDBG        write(o,'(3A,3(I4,A),2I4,A,F12.6,2A,3F6.1)') sym, fun, &
cDBG         'coordination ia=', ia, ' ja=', nint(dba(OTHER_I,ic,ia)), ' ip=', nint(dba(P_IMAGE,ic,ia)), &
cDBG         ' Z,Z', a(ia)%s%iZ, nint(dba(OTHER_Z,ic,ia)), ' d=', dba(ILENGTH,ic,ia)*Ang, Ang_, ', dv=', dba(  1:3  ,ic,ia)
cDBG      enddo ! ic
cDBG    enddo ! ia
cDBG  endif ! o


    if( o>0 .and. natm > 0 ) then

      if( nsm > 0 ) write(o,'(3A,9(I0,A))') sym, fun, &
        'found ',nsm,' bonds shorter than ',nint(BL_WINDOW(-1)*100),'% of the usual bond length.'

      if( long > 0. ) write(o,'(3A,F0.3,9A)') sym, fun, 'longest atom-atom distance = ', long*Ang, Ang_

      if( short < SHORT_LONG ) write(o,'(3A,F0.3,9A)') sym, fun, 'shortest atom-atom distance = ', short*Ang, Ang_

      if( short < SHORTEST_BOND_LENGTH ) &
        write(o,'(4A,F0.3,9A)') sym, fun, WARNING(0), 'one or more atom-atom distances are as short as ', short*Ang, Ang_

      if( nbs > 1 ) write(o,'(3A,9(I0,A))') sym, fun, &
        'found ',sum(nba)/2,' atom-atom distances within [ ', nint(100.*bl_window(-1)), &
        ', ', nint(100.*bl_window(1)), ' ] % of the default bond length.'

      ! show coordination
      write(o,'(9A)') ! blank line
      write(o,'(9A)') sym, '             Coordination'
      write(o,'(9A)') sym, ' Atom#  Sym  #  bs: bonds in', Ang_,'   as: angles in Degrees'
      write(o,'(9A)') sym, ' -----------------------------------------------------------------'
      do ia = 1, natm ! loop over all atoms

        nb = min(nba(ia),MXB) ! number of bonds
        if( nb > 0 ) then
          bd(1:nb) = dba(ILENGTH,1:nb,ia) ! copy bond lengths

          ist = bubble_sort( bd(1:nb) ) ! sort the first nb entries by magnitude, smallest first
          ! analyze bond lengths, create histograms with bin width 0.001 Ang
          bond_count = counted( nb, nint( bd(1:nb)*Ang*1E3 ), nbc )
          write(unit=string,fmt='(99(2A,F0.3))',iostat=ios) ( ' ',trim(nx(bond_count(1,ib))), 1E-3*bond_count(2,ib), ib=1,nbc )

          if( nb > 1 ) then ! write an extra line for the bond angles
            na = (nb*(nb-1))/2 ! number of angles
            ba = bond_angles( nb, dba(1:3,1:nb,ia) ) ! compute bond angles

cDBG        if( any( ba(1:na) < SMALLEST_BOND_ANGLE ) .and. o>0) write(o,'(9A)') sym, fun, ERROR, 'Very small bond angle. A bond has been counted twice!'

            ist = bubble_sort( ba(1:na) ) ! sort the first na entries by magnitude, smallest first
            ! analyze bond angles, create histograms with bin width 1.0 DEGREE
            angle_count = counted( na, nint(ba(1:na)*DEGREE), nac )

            write(o,'(A,I6,2A,I4,3A,999(2A,I0))') &
              sym, ia, '  ', a(ia)%s%sym, nb, '  bs:', trim(string), '  as:', ( ' ',trim(nx(angle_count(1,ib))),angle_count(2,ib), ib=1,nac )
          else  ! nb > 1
            write(o,'(A,I6,2A,I4,3A)') &
              sym, ia, '  ', a(ia)%s%sym, nb, '  bs:', trim(string)
          endif ! nb > 1
        else  ! nb > 0
            write(o,'(A,I6,2A,I4,3A)') &
              sym, ia, '  ', a(ia)%s%sym, nb, '  bs: ', 'none'
        endif ! nb > 0

! #ifndef GeSbTe
! #else
!             write(o,'(A,I6,2A,I4,A,99F0.3)') &
!               sym,  ia, '  ', a(ia)%s%sym, nb, '  bs:', bd(1:nb)*Ang
! !           write(o,'(A,I6,2A,I4,A,99F6.3)') sym,  ia, '  ', a(ia)%s%sym, nb, '  avg_bs: ', avg_rms( bd(1:nb) )*Ang
! !           write(o,'(A,I6,2A,I4,A,99F10.6)') sym,  ia, '  ', a(ia)%s%sym, nb, '  inv_bs: ', nb*avg_rms( 5.707/bd(1:nb) )!*Ang
! #endif

      enddo ! ia
      write(o,'(9A)') sym, ' -----------------------------------------------------------------'
      write(o,'(9A)') ! blank line
    endif ! o /= 0


!     if( .not. SHOW_HISTOGRAMM ) return
! 
!     ! make the histogramm
!     nbin = nint( ( bin(+1) - bin(-1) ) / bin(0) )
!     nbin = max( 2, nbin )
!     bin(0) = max( 0.01 , ( bin(+1) - bin(-1) ) / real( nbin ) )
!     bin(+1) = bin(-1) + nbin * bin(0) ! corrected
! 
!     allocate( histogram( nbin ), stat=ist )
!     if( ist /= 0 ) return
!     ! sort the bonds into the corresponding bins
!     ! according to their bond lengths
!     histogram = 0
!     do ib = 1, nbs
!       ibin = ( daa(ib) - ( bin(-1)-0.5*bin(0) ) ) / bin(0)
!       if( ibin <= nbin .and. ibin > 0 ) histogram(ibin) = histogram(ibin) + 1
!     enddo ! ib
! 
!     ! correct for double counting
!     histogram = histogram/2
! 
!     mval = maxval( histogram )
!     if( mval > 0 ) then
! 
!       ndigit = int( log10( real(mval) ) ) + 1 + 1 ! one more blank
!       ! create format string
!       write(unit=string,fmt='(9(A,I0))',iostat=ios) '(2A,F0.3,A,',nbin,'I',ndigit,',A,F0.3,A)'
!       ! headline
!       if(o>0) write(o,'(3A,F0.3,9A)') sym, fun, 'atom-atom distance histogram, bin width ', bin(0)*Ang, Ang_
!       ! write data
!       if(o>0) write(o,fmt=string) sym, fun, bin(-1)*Ang, ' [', histogram(1:nbin), ' ] ', bin(+1)*Ang, Ang_
!     endif ! mval > 0
!     if(o>0) write(o,'(9A)') ! blank line
!     deallocate( histogram, stat=ist )

    deallocate( baa, nba, dba, stat=ist )

    if( .not. SHOW_HISTOGRAMM ) return

    ! make a new histogramm
    bin(0) = 0.2 ! 0.1058 Ang
    bin(0) = floor( bin(0) * 10. * Ang ) / ( 10. * Ang ) ! --> 0.1 Ang if Ang, 0.2 aB if Bohr
    bin(-1) = 0.
    nbin = 48
    bin(+1) = bin(-1) + (nbin+1)*bin(0) ! --> 4.9 Ang if Ang, 9.8 aB if Bohr

    allocate( histogram(0:nbin), stat=ist )
    if( ist /= 0 ) return
    ! sort the bonds into the corresponding bins
    ! according to their bond lengths
    histogram = 0 ! init
    do ib = 1, nbs
      ibin = nint( daa(ib) / bin(0) )
! cDBG       write(7,*) 'daa', daa(ib), daa(ib)*Ang, ' ibin', ibin
      if( ibin <= nbin .and. ibin >= 0 ) histogram(ibin) = histogram(ibin)+1
    enddo ! ib

    mval = maxval( histogram )
    if( mval > 0 ) then

      ndigit = int( log10( mval+.5 ) )
      allocate( hst(0:nbin,-2:ndigit), stat=ist )
      do ibin = 0, nbin
        write(unit=string,fmt='(I16)',iostat=ios) histogram(ibin)
        if( histogram(ibin) < 1 ) string = ' ' ! empty string instead of 0
        do ic = 0, ndigit
          hst(ibin,ic) = string(16-ic:16-ic)
        enddo ! ic
        hst(ibin,-1) = '-'
        ic = nint( ( bin(-1)+ibin*bin(0) )*Ang*10. )
        if( modulo( ic,10 ) == 0 ) hst(ibin,-1) = achar(48+ic/10)
        hst(ibin,-2) = achar(48+ modulo( nint( ( bin(-1)+ibin*bin(0) )*Ang*10. ),10 ) )
      enddo ! ibin
      write(unit=string,fmt='(F6.1)',iostat=ios) bin(+1)*Ang

      ! headline
      if(o>0) write(o,'(3A,F5.2,9A)') sym, fun, 'atom-atom distance histogram in bins of', bin(0)*Ang, Ang_, ' (counts vertical)'
      ! write data
      do ib = ndigit, 0, -1
        if(o>0) write(o,fmt='(99A)',iostat=ios) sym, fun, '  |', hst(4:,ib), '|'
      enddo ! ib
        if(o>0) write(o,fmt='(99A)',iostat=ios) sym, fun, '  |', hst(4:,-1), '|'
        if(o>0) write(o,fmt='(99A)',iostat=ios) sym, fun, ' 0.', hst(4:,-2), '<',string(4:6), Ang_

      deallocate( hst, stat=ist )
    endif ! mval > 0
    if(o>0) write(o,'(9A)') ! blank line
    deallocate( daa, histogram, stat=ist )

  contains

    character(len=4) elemental function nx( n )
      integer, intent(in) :: n
      status_t :: ios
      nx = '' ; if( n < 2 ) return ! ''
      write(unit=nx,fmt='(I0,A)',iostat=ios) n,'x'
      nx = adjustl(nx)
    endfunction ! nx

    real function vlength( v ) result( r )
      real, intent(in) :: v(1:3)
      r = sqrt( v(1)*v(1) + v(2)*v(2) + v(3)*v(3) )
    endfunction ! vlength

    real function bond_angle( v1, v2 ) result( a ) ! analyze bond angles
      real, intent(in) :: v1(1:3), v2(1:3)

      real :: r1r2cosa, r1r2
      a = 0.
      r1r2 = sqrt( ( v1(1)*v1(1)+v1(2)*v1(2)+v1(3)*v1(3) ) &
                 * ( v2(1)*v2(1)+v2(2)*v2(2)+v2(3)*v2(3) ) )
      if( r1r2 <= 0. ) return ! error
      r1r2cosa = v1(1)*v2(1)+v1(2)*v2(2)+v1(3)*v2(3)
      if( abs(r1r2cosa) > r1r2 ) r1r2 = abs(r1r2cosa)
      a = acos( r1r2cosa/r1r2 )
    endfunction ! bond_angle
 
    function bond_angles( n, v ) result( alist ) ! analyze bond angles
      integer, intent(in) :: n
      real, intent(in)    :: v(1:3,n)
      real                :: alist((n*(n-1))/2) ! result

      integer             :: i, j, k
      k = 0 ! init counter
      do i = 1, n
        do j = 1, i-1 ! self-avoiding triangular loop
          k = k+1 ! count up
          alist(k) = bond_angle( v(1:3,i), v(1:3,j) )
!           if(o>0) write(o,'(2A,2I3,I6,A,I4)') sym, ' bond_angles: i,j,k', i,j,k, ' a=', nint(alist(k)*DEGREE)
        enddo ! j
      enddo ! i
      ! check counter value
      if( k /= (n*(n-1))/2 ) stop 'bond_angles: counting error.'
    endfunction ! bond_angles

  endfunction ! find_bonds


  function avg_rms( val ) result( ar )
    real, intent(in) :: val(:)
    real             :: ar(0:1) ! result

    integer :: n
    real :: a, a2, var
    n = size(val)
    ar = 0. ; if( n < 1 ) return ! [0.,0.]
    a = sum(val)/real(n)
    a2 = sum(val**2)/real(n)
    var = a2-a*a
    ar(0:1) = (/ a, sqrt(var) /)
  endfunction ! avg_rms


  function counted( n, vals, ndiff ) result( cnt ) ! make a histogram out of a list of integer values
    integer, intent(in)  :: n, vals(n)
    integer, intent(out) :: ndiff
    integer, parameter   :: ICNT=1, IVAL=2
    integer              :: cnt(ICNT:IVAL,n) ! result

    integer :: i, j, jf
! #ifdef FULL_DEBUG
!     character(len=*), parameter :: fun = ' counted: '
!     write(*,'(3A,99I5)') sym, fun, 'start: ', vals
! #endif
    cnt = 0
    ndiff = 0
    do i = 1, n

      ! find the index in the already existing list
      jf = 0 ! 0 means, not in the list yet
      do j = 1, ndiff ! loop through list
        if( cnt(IVAL,j) == vals(i) ) jf = j
      enddo ! j

      if( jf > 0 ) then ! slot exists
        cnt(ICNT,jf) = cnt(ICNT,jf)+1 ! count up
      else ! slot does not exist, create
        ndiff = ndiff+1
        cnt(IVAL,ndiff) = vals(i) ! set value
        cnt(ICNT,ndiff) = 1 ! init count to one
! #ifdef FULL_DEBUG
!         write(*,'(3A,I0,A,I0)') sym, fun, 'i = ', i, ' new entry, value = ', cnt(IVAL,ndiff)
! #endif
      endif ! jf == 0
! #ifdef FULL_DEBUG
!       write(*,'(2A,9(A,I0))') sym, fun, 'i = ', i, ' cnt = ', cnt(ICNT,jf)
! #endif
    enddo ! i
! #ifdef FULL_DEBUG
!     write(*,'(3A,I0)') sym, fun, 'ndiff = ', ndiff
!     write(*,'(9A)') sym, fun, 'done'
! #endif
  endfunction ! counted


  real function def_bond_length( Z1, Z2 ) result( bl )
  use constants, only: ANGSTROM ! = 0.52917724924
    integer, intent(in) :: Z1, Z2 ! atomic numbers

    integer, parameter  :: bpm(0:3,1:118) = reshape( (/ &
    ! data from http://chemwiki.ucdavis.edu/Theoretical_Chemistry/Chemical_Bonding/Bond_Order_and_Lengths
     31   ,32   ,  0  ,  0  & !  H     1
    ,28   ,46   ,  0  ,  0  & !  He    2
    ,128  ,133  ,124  ,  0  & !  Li    3
    ,96   ,102  ,90   ,85   & !  Be    4
    ,84   ,85   ,78   ,73   & !  B     5
    ,76   ,75   ,67   ,60   & !  C     6
    ,73   ,71   ,60   ,54   & !  N     7
    ,69   ,63   ,57   ,53   & !  O     8
    ,71   ,64   ,59   ,53   & !  F     9
    ,66   ,67   ,96   ,  0  & !  Ne    10
    ,57   ,155  ,160  ,  0  & !  Na    11
    ,141  ,139  ,132  ,127  & !  Mg    12
    ,121  ,126  ,113  ,111  & !  Al    13
    ,111  ,116  ,107  ,102  & !  Si    14
    ,107  ,111  ,102  ,94   & !  P     15
    ,105  ,103  ,94   ,95   & !  S     16
    ,102  ,99   ,95   ,93   & !  Cl    17
    ,106  ,96   ,107  ,96   & !  Ar    18
    ,203  ,196  ,193  ,  0  & !  K     19
    ,176  ,171  ,147  ,133  & !  Ca    20
    ,170  ,148  ,116  ,114  & !  Sc    21
    ,160  ,136  ,117  ,108  & !  Ti    22
    ,153  ,134  ,112  ,106  & !  V     23
    ,139  ,122  ,111  ,103  & !  Cr    24
!     ,161  ,119  ,105  ,103  & !  Mn    25
    ,132  ,119  ,105  ,103  & !  Mn    25
!     ,142  ,116  ,109  ,102  & !  Fe    26
    ,110  ,116  ,109  ,102  & !  Fe    26  110pm is good for bcc bulk
    ,139  ,111  ,103  ,961  & !  Co    27
    ,124  ,110  ,101  ,101  & !  Ni    28
    ,132  ,112  ,115  ,120  & !  Cu    29
    ,122  ,118  ,120  ,  0  & !  Zn    30
    ,122  ,124  ,117  ,121  & !  Ga    31
#ifndef GeSbTe
    ,120  ,121  ,111  ,114  & !  Ge    32
#else
    ,200  ,121  ,111  ,114  & !  Ge    32
#endif
    ,119  ,121  ,114  ,106  & !  As    33
    ,120  ,116  ,107  ,107  & !  Se    34
    ,120  ,114  ,109  ,110  & !  Br    35
    ,116  ,117  ,121  ,108  & !  Kr    36
    ,220  ,210  ,202  ,  0  & !  Rb    37
    ,195  ,185  ,157  ,139  & !  Sr    38
    ,190  ,163  ,130  ,124  & !  Y     39
    ,175  ,154  ,127  ,121  & !  Zr    40
    ,164  ,147  ,125  ,116  & !  Nb    41
    ,154  ,138  ,121  ,113  & !  Mo    42
    ,147  ,128  ,120  ,110  & !  Tc    43
    ,146  ,125  ,114  ,103  & !  Ru    44
    ,142  ,125  ,110  ,106  & !  Rh    45
    ,139  ,120  ,117  ,112  & !  Pd    46
    ,145  ,128  ,139  ,137  & !  Ag    47
    ,144  ,136  ,144  ,  0  & !  Cd    48
    ,142  ,142  ,136  ,146  & !  In    49
    ,139  ,140  ,130  ,132  & !  Sn    50
#ifndef GeSbTe
    ,139  ,140  ,133  ,127  & !  Sb    51
    ,138  ,136  ,128  ,121  & !  Te    52
#else
    ,209  ,140  ,133  ,127  & !  Sb    51
    ,100  ,136  ,128  ,121  & !  Te    52
#endif
    ,139  ,133  ,129  ,125  & !  I     53
    ,140  ,131  ,135  ,122  & !  Xe    54
    ,244  ,232  ,209  ,  0  & !  Cs    55
    ,215  ,196  ,161  ,149  & !  Ba    56
    ,207  ,180  ,139  ,139  & !  La    57
    ,204  ,163  ,137  ,131  & !  Ce    58
    ,203  ,176  ,138  ,128  & !  Pr    59
    ,201  ,172  ,137  ,  0  & !  Nd    60
    ,199  ,173  ,135  ,  0  & !  Pm    61
    ,198  ,172  ,134  ,  0  & !  Sm    62
    ,198  ,168  ,134  ,  0  & !  Eu    63
    ,196  ,169  ,135  ,132  & !  Gd    64
    ,194  ,168  ,135  ,  0  & !  Tb    65
    ,192  ,167  ,133  ,  0  & !  Dy    66
    ,192  ,166  ,133  ,  0  & !  Ho    67
    ,189  ,165  ,133  ,  0  & !  Er    68
    ,190  ,164  ,131  ,  0  & !  Tm    69
    ,187  ,170  ,129  ,  0  & !  Yb    70
    ,187  ,162  ,131  ,131  & !  Lu    71
    ,175  ,152  ,128  ,122  & !  Hf    72
    ,170  ,148  ,126  ,119  & !  Ta    73
    ,162  ,137  ,120  ,115  & !  W     74
    ,151  ,131  ,119  ,110  & !  Re    75
    ,144  ,129  ,116  ,109  & !  Os    76
    ,141  ,122  ,115  ,107  & !  Ir    77
    ,136  ,123  ,112  ,110  & !  Pt    78
    ,136  ,124  ,121  ,123  & !  Au    79
    ,132  ,134  ,142  ,  0  & !  Hg    80
    ,145  ,144  ,142  ,150  & !  Tl    81
    ,146  ,144  ,135  ,137  & !  Pb    82
    ,148  ,151  ,141  ,135  & !  Bi    83
    ,140  ,145  ,135  ,129  & !  Po    84
    ,150  ,147  ,138  ,128  & !  At    85
    ,150  ,142  ,145  ,133  & !  Rn    86
    ,260  ,223  ,218  ,  0  & !  Fr    87
    ,221  ,201  ,173  ,159  & !  Ra    88
    ,215  ,186  ,153  ,140  & !  Ac    89
    ,206  ,175  ,143  ,136  & !  Th    90
    ,200  ,169  ,138  ,129  & !  Pa    91
    ,196  ,170  ,134  ,118  & !  U     92
    ,190  ,171  ,136  ,116  & !  Np    93
    ,187  ,172  ,135  ,  0  & !  Pu    94
    ,180  ,166  ,135  ,  0  & !  Am    95
    ,169  ,166  ,136  ,  0  & !  Cm    96
    ,166  ,166  ,139  ,  0  & !  Bk    97
    ,168  ,168  ,140  ,  0  & !  Cf    98
    ,165  ,165  ,140  ,  0  & !  Es    99
    ,167  ,167  ,  0  ,  0  & !  Fm    100
    ,173  ,173  ,139  ,  0  & !  Md    101
    ,176  ,176  ,159  ,  0  & !  No    102
    ,161  ,161  ,141  ,  0  & !  Lr    103
    ,157  ,157  ,140  ,131  & !  Rf    104
    ,149  ,149  ,136  ,126  & !  Dr    105
    ,143  ,143  ,128  ,121  & !  Sg    106
    ,141  ,141  ,128  ,119  & !  Bh    107
    ,134  ,134  ,125  ,118  & !  Hs    108
    ,129  ,129  ,125  ,113  & !  Mt    109
    ,128  ,128  ,116  ,112  & !  Ds    110
    ,121  ,121  ,116  ,118  & !  Rg    111
    ,122  ,122  ,137  ,130  & !  Cn    112
    ,136  ,136  ,  0  ,  0  & !  ut    113
    ,143  ,143  ,  0  ,  0  & !  uq    114
    ,162  ,162  ,  0  ,  0  & !  up    115
    ,175  ,175  ,  0  ,  0  & !  uh    116
    ,165  ,165  ,  0  ,  0  & !  us    117
    ,157  ,157  ,  0  ,  0  & !  uo    118
       /) ,(/4,118/))
    ! data from http://chemwiki.ucdavis.edu/
    ! Theoretical_Chemistry/Chemical_Bonding/Bond_Order_and_Lengths
    real, parameter :: PICOMETER = 0.01/ANGSTROM

    bl = 0. ! result for early return
    if( Z1 <   1 .or. Z2 <   1 ) return ! 0. ! stop 'CHM default_bond_length: Z < 1.'
    if( Z1 > 118 .or. Z2 > 118 ) return ! 0. ! stop 'CHM default_bond_length: Z > 118.'

    bl = ( bpm(0,Z1) + bpm(0,Z2) ) * PICOMETER
  endfunction ! def_bond_length


  status_t function bubble_sort( r ) result( ist )
  ! bring the values in r into ascending order
#undef FULL_DEBUG
    real, intent(inout) :: r(:) 

    integer :: i, n ! n=MXB (usually 12) ! or n=66 (=12*(12-1)/2)
    real    :: s ! swap
    logical :: changed
#ifdef FULL_DEBUG
    character(len=*), parameter :: fun = ' bubble_sort: '
#endif
    n = size( r )
    ist = 0 ; if( n < 2 ) return ! 0

#ifdef FULL_DEBUG
    write(*,'(3A,99F10.3)') sym, fun, 'unsorted', r
#endif
    changed = .true. ! assume that the en vector is unordered
    do while( changed )
      i = 1
      do while( i < n )
        i = i+1
        if( r(i) < r(i-1) ) then
          s=r(i) ; r(i)=r(i-1) ; r(i-1)=s ! swap entry i and i-1
          i = n+1 ! stop the inner while loop
        endif ! r(i) < r(i-1)
      enddo ! while i < MXC
      if( i == n ) changed = .false. ! is sorted, stop the outer while loop
    enddo ! while ( changed )
#ifdef FULL_DEBUG
    write(*,'(3A,99F10.3)') sym, fun, '  sorted', r
#endif
  endfunction ! bubble_sort


#if EXTENDED
!+ extended

!   status_t function test2( ) result( ist )
!   implicit none
!     ! parameters
!     character(len=*), parameter :: fun = ': '
! !     integer, parameter :: nw = 12
! !     real, parameter    :: wd = 0.0625
!     integer, parameter :: nw = 36
!     real, parameter    :: wd = 0.0078125
!     ! local vars
!     integer :: ios, u,  Z1, Z2, id
!     character(len=2) :: s1, s2
!     real :: dist ! in Angstrom
! !     real :: dat(0-nw:8191-nw,0:6) ! spacing 10^-3 Ang
!     real :: dat(0-nw:8191-nw,0:6) ! spacing 10^-3 Ang
!     real :: w(-nw:nw)
! 
!     call test2() ; return
! 
!     do id = -nw, nw
!       w(id) = exp(-wd*id*id)
!     enddo ! id
!     w = w*1000./sum(w) ! normalize to unity
! 
!     dat = 0. ! init
! 
!     write(*,'(9A)') sym, fun, 'read fort.8 --> write fort.9'
!     u = 8 ! read from fort.8
!     open(unit=u,file='fort.8',status='old',IOstat=ios)
!     do while( ios == 0 )
!       Z1 = 0 ; Z2 = 0 ; dist = 0.
!       read(u,fmt=*,IOstat=ios) s1, Z1, s2, Z2, dist
!       id = nint( 1000. * abs(dist) )
! !       write(*,*) dist, id
!       selectcase( Z1*Z2 )
! !       case( 1*1 ) ; dat(id-nw:id+nw,1) = dat(id-nw:id+nw,1)+w
! !       case( 1*8 ) ; dat(id-nw:id+nw,2) = dat(id-nw:id+nw,2)+w
! !       case( 8*8 ) ; dat(id-nw:id+nw,3) = dat(id-nw:id+nw,3)+w
!       case( 52*52 ) ; dat(id-nw:id+nw,1) = dat(id-nw:id+nw,1)+w
!       case( 51*51 ) ; dat(id-nw:id+nw,2) = dat(id-nw:id+nw,2)+w
!       case( 32*32 ) ; dat(id-nw:id+nw,3) = dat(id-nw:id+nw,3)+w
!       case( 52*51 ) ; dat(id-nw:id+nw,4) = dat(id-nw:id+nw,4)+w
!       case( 52*32 ) ; dat(id-nw:id+nw,5) = dat(id-nw:id+nw,5)+w
!       case( 51*32 ) ; dat(id-nw:id+nw,6) = dat(id-nw:id+nw,6)+w
!       case default ! no action
!       endselect ! Z1*Z2
!     enddo ! while ios == 0
!     dat(:,0) = dat(:,1)+dat(:,2)+dat(:,3)
! 
! !       write(9,'(A10,9A16)') '# d', 'all', 'H--H', 'O--H', 'O--O'
!       write(9,'(A10,9A16)') '# d', 'all', 'Te--Te', 'Sb--Sb', 'Ge--Ge', 'Te--Sb', 'Te--Ge', 'Sb--Ge'
!     do id = lbound(dat,1), ubound(dat,1)
!       write(9,'(F10.3,9F16.4)') id*0.001, dat(id,0:)
!     enddo ! id
! 
!   return
!   endsubroutine test2




  status_t function test( ) result( ios )
  use configuration, only: WARNING, ERROR
  use configuration, only: o
  use type_element, only: symbol
  use sorting, only: permutation_of
    character(len=*), parameter :: fun = ': '
!     integer, parameter :: nw = 12
!     real, parameter    :: wd = 0.0625
!     integer, parameter :: nw = 36
!     real, parameter    :: wd = 0.0078125
    integer, parameter :: nw = 72
    real, parameter    :: wd = 0.0078125*0.125
    integer :: Z(1:2), id
    iounit_t :: u
    character(len=2) :: s(1:2)
    real :: dist ! in Angstrom
!     real :: dat(0-nw:8191-nw,0:6) ! spacing 10^-3 Ang
    real, allocatable :: dat(:,:) ! dat(0-nw:8191-nw,0:6) ! spacing 10^-3 Ang
    real              :: w(-nw:nw)
    real              :: b
!     integer :: nvals
    integer           :: ns = 0 ! number of different species
    integer           :: np = 0 ! number of species pairs
    integer           :: isl(0:121) = 0
    integer           :: nsl(0:121) = 0
    integer           :: iZl(0:121) = 0
    integer, allocatable :: ipl(:,:)
    integer           :: i1, i2, ii, nv, is, iZ, ip
#ifdef DISCRETE_HIST
    integer, parameter        :: MKN = DISCRETE_HIST
    integer, allocatable      :: h(:,:)
    integer                   :: jkn, ikn, nkn=0, iperm(1:MKN)=0
    real                      :: bkn(0:MKN)=0.
    real, parameter           :: TOLERANCE = 1E-3 ! tolerance for perfect lattice positions
#endif

    nv = 0 ; is = 0 ; nsl = 0 ; isl = 0 ! init
    if(o>0) write(o,'(9A)') sym, fun, 'read  fort.8'
    u = 8 ! read from fort.8
    open(unit=u,file='fort.8',status='old',IOstat=ios)
      Z = 0
      read(u,fmt=*,IOstat=ios) s(1), Z(1), s(2), Z(2), dist ! read 1st line
    do while( ios == 0 )
      nv = nv+1 ! count up number of (ios==0)-valid lines
      do ii = 1, 2 ! for both species in the pair
        iZ = Z(ii) ! abbrev.
        if( iZ >= 0 .and. iZ < 120 ) then
          ! Z is valid
          if( nsl(iZ) < 1 ) then
            ! ==> new entry
            is = is+1 ! count up number of different species
            isl(iZ) = is ! assign index to is(Z)
            iZl(is) = iZ ! Z-list
          endif ! species has  not been found earlier
          nsl(iZ) = nsl(iZ)+1 ! count number of times Z has been mentioned
        else ; if(o>0) write(o,'(4A,I0,9A)') sym, fun, WARNING(0), 'Z = ', iZ, ' with symbol ', s(ii), ' found!'
        endif ! Z valid
      enddo ! ii
      Z = 0
      read(unit=u,fmt=*,IOstat=ios) s(1), Z(1), s(2), Z(2), dist ! read nxt line
    enddo ! while

    ns = is
    if( ns /= count( isl /= 0 ) ) stop 'CHM: fatal error counting (isl)!' ! number of different species
    if( ns /= count( nsl  > 0 ) ) stop 'CHM: fatal error counting (nsl)!' ! number of different species
    np = ns*(ns+1)/2 ! number of species pairs



    ! set up pair index table
    allocate( ipl(0:ns,0:ns) ) ; ipl = 0 ! init
    ip = 0
    do i1 = 1, ns
      do i2 = 1, i1 ! triangular loop
        ip = ip+1 ! count up pair index
        ipl(i1,i2) = ip
        ipl(i2,i1) = ip ! symmetric matrix
      enddo ! i2
    enddo ! i1
    if( ip /= np ) stop 'CHM: fatal error counting (ip/=np)!' ! number of different pairs
    ! show
    if(o>0) then
      write(o,'(9A)') sym, fun, 'pair index table'
      do i1 = 1, ns ; write(o,'(I6,99I3)') ipl(1:,i1) ; enddo ! i1
      write(o,'(9A)') '' ! empty line
    endif ! o>0

#ifdef DISCRETE_HIST
    allocate( h(0:MKN,0:np) ) ; h = 0 ! lbound(h,1) == 0 for safety
#endif

    ! prepare broadening
    do id = -nw, nw
      w(id) = exp(-wd*id*id)
    enddo ! id
    w = w/sum(w) ! normalize such that the sum is unity
!     w = w/w(0) ! normalize such that the peak has value 1.0
    if(o>0) write(o,'(3A,9F16.10)') sym, fun, 'w:', w(0), w(nw)

    allocate( dat(0-nw:ceiling(1000.*LBL)-nw,0:np) ) ; dat = 0. ! init

    rewind(unit=u,IOstat=ios) ! reset file
    if(o>0 .and. ios/=0) write(o,'(4A,9(I0,A))') sym, fun, ERROR, 'rewinding unit ',u,' failed, ios = ',ios
      Z = 0 ; dist = 0.
      read(unit=u,fmt=*,IOstat=ios) s(1), Z(1), s(2), Z(2), dist ! read 1st line
    do while( ios == 0 )
      b = abs(dist)
      !====================================================================
      id = nint( 1000. * b )
!       if(o>0) write(o,*) dist, id, Z
      i1 = isl(Z(1))
      i2 = isl(Z(2))
      ip = ipl(i1,i2)
      if( id+nw <= ubound(dat,1) ) &
        dat(id-nw:id+nw,ip) = dat(id-nw:id+nw,ip)+w
#ifdef DISCRETE_HIST
      !====================================================================
      ikn = 0 ! 0:not found
      jkn = 0 ! init
      do while( jkn < nkn ) ! search in known values
        jkn = jkn+1 ! count up
        if( abs( b - bkn(jkn) ) < TOLERANCE ) then
          ikn = jkn ! found
          jkn = nkn+999 ! stop the loop
        endif
      enddo ! while
      if( ikn == 0 ) then ! not found
        ! try to add new
        ikn = nkn+1 ! next higher index than the known ones
        if( ikn > MKN ) then ! exceeds FIX limits
          ikn = 0 ! write to the zero entry
        else  ! ikn > MKN
          nkn = ikn ! increase number of known values
          bkn(ikn) = b ! store value
          h(ikn,:) = 0 ! init
        endif ! ikn > MKN
      endif ! not found
      h(ikn,ip) = h(ikn,ip)+1 ! count up
#endif
      !====================================================================
      Z = 0 ; dist = 0.
      read(unit=u,fmt=*,IOstat=ios) s(1), Z(1), s(2), Z(2), dist ! read nxt line
    enddo ! while ios == 0
    close(unit=u,IOstat=ios)

#ifdef DISCRETE_HIST
    ! some entries have been written to h(0,ip) ==> WARNING
    if( any( h(0,:) > 0 ) .and. o>0 ) write(o,'(9A)') sym, fun, WARNING(0), &
     'too many different bond lengths, increase DISCRETE_HIST or TOLERANCE!'

    iperm(1:nkn) = permutation_of( nkn, bkn(1:nkn) )

    if(o>0) write(o,'(/,9A)') sym, fun, 'integer Histogram'
    if(o>0) write(o,'(1A8,999F6.2)') 'Sy--Sy  ', bkn(iperm(1:nkn))
    if(o>0) write(o,'(1A8,999A6)')   '======  ', ('-----',ikn=1,nkn)
    ip = 0
    do i1 = 1, ns
      do i2 = 1, i1 ! triangular loop
        ip = ip+1
        if(o>0) write(o,'(4A2,999I6)') symbol(iZl(i1)),'--',symbol(iZl(i2)), '  ', h(iperm(1:nkn),ip)
      enddo ! i2
    enddo ! i1
    if(o>0) write(o,'(1A8,999A6)')   '------  ', ('-----',ikn=1,nkn)
    if(o>0) write(o,'(1A8,999I6)') '   Sum  ', ( sum( h(iperm(ikn),:) ) , ikn=1,nkn)

    if(o>0) write(o,'(A)') ! empty line
#endif

    ! backfold tail (if there are bonds with dist 0.)
    do id = 0-nw, 0
      dat(abs(id),:) = dat(abs(id),:) + dat(id,:) ! add to here
      dat(id,:) = 0.                              ! delete here
    enddo ! id

    ! collect all
    dat(:,0) = 0. ! init data set for all
    do ip = 1, np
      dat(0:,0) = dat(0:,0) + dat(0:,ip)
    enddo ! ip


    if(o>0) write(o,'(9A)') sym, fun, 'write fort.9'
    u = 9 ! write to fort.9
    write(u,'(A10,A16,99(A10,3A2))') '# dist', 'all', ( ( ' ',symbol(iZl(i1)),'--',symbol(iZl(i2)), i2=1,i1 ), i1=1,ns )
    do id = 0, ubound(dat,1)
      if( any( dat(id,0:np) > 0.00001 ) ) &
      write(unit=u,fmt='(F10.3,99F16.3)') id*0.001, dat(id,0:np)
    enddo ! id

  endfunction ! test

!- extended
#endif

endmodule ! chemistry

! #
! # some element in bulk hcp
! #
!
! # 2 hexagonal layers (AB stacking)
!
! #
! #    |    |          |    |
! #--  / \   / \       \ /   \ /  --
! #   / B \ / b \     --B2----b--
! #   -----A2----      / \ A / \
! #   \  x/ \   /     /  x\ /   \
! #    \ / B \ /      -----B1----
! #   --A1----a--     \ A / \ a /
! #--  / \   / \       \ /   \ /  --
! #    |    |          |    |
! # origin at x
!
! atoms_fractional
! C  -1:4  -2:6  -1:4   #A1
! C   1:4   1:6  -1:4   #A2
! C   1:4  -1:6   1:4   #B1
! C  -1:4   2:6   1:4   #B2
! atoms_fractional

! # a bond length
!
! # height of a tetrahedron: 2/3 * c *sqrt(3) , where c is the side length
! # of the cube, that takes the tetrahedron. the edge length of the T is
! # c*sqrt(2)  = bond length a
! # ==> h = 2/3 * sqrt(3/2) a = sqrt(2/3) a
!
! #     a                    sqrt(3)*a              sqrt(8/3)*a
! cell  2.6741238626870429   4.6317183959062982     4.3668259817224291
! ngps  8                    12                     10
!
! boundary  1 1 1

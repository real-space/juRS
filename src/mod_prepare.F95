#ifdef DEBUG_ALL
#define DEBUG
#endif

! #define DEBUG
! #define FULL_DEBUG

! #define RAD_DEBUG


#ifdef DEBUG
#define cDBG
#else
#define cDBG !DBG
#endif

#define cNaN !NaN
#ifdef EXTENDED
#ifdef NaN_SEARCH
#define cNaN
#endif
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

!! @author Paul Baumeister
!! @version 4.04
!!
!! generate start wave functions from atomic orbitals
module prepare
  use configuration, only: o
implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: fun = ': ', sym = 'PRE' !! module symbol

  public :: prepare_wavefunctions
  public :: prepare_wf_planewaves
  public :: otherkpoint_copy
#ifdef EXTENDED
  public :: test
#endif

  interface prepare_wavefunctions
    module procedure prepare_wavefunctions_r, prepare_wavefunctions_c
  endinterface

  interface generate_atomic_orbital ! generation of an atomic orbital
    module procedure generate_atomic_orbital_r, generate_atomic_orbital_c
  endinterface

  interface prepare_wf_planewaves ! only plane waves
    module procedure prepare_wf_planewaves_r, prepare_wf_planewaves_c
  endinterface

  interface otherkpoint_copy ! copy a wave function Psi_k2(r) = exp{i(k1-k1)*r} * Psi_k2(r)
    module procedure otherkpoint_copy_r, otherkpoint_copy_c
  endinterface

  interface generate_plane_wave
    module procedure generate_plane_wave_r, generate_plane_wave_c
  endinterface

  interface antisymmetrize
    module procedure antisymmetrize_r, antisymmetrize_c
  endinterface

  !! atomic orbital will be cut off with mask=(1-(r/R)^(2*MASK_POW))^MASK_EXP
  integer, parameter, private :: MASK_EXP = 4 !! 0: no mask, 1: mask goes to zero linearly, 2: mask goes to zero quadratically
  integer, parameter, private :: MASK_POW = 4 !! the higher MASK_POW, the larger the region around r=0 where mask is approx 1.0

  contains

#endif
! end of head part



#if R1_C2 == 1
#define REAPLEX real
#else
#define REAPLEX complex
#endif



#if R1_C2 == 1
  !! prepare initial guess wave functions
  !! by selecting from a list of atomic orbitals and plane waves
  subroutine prepare_wavefunctions_r( &
#else
  subroutine prepare_wavefunctions_c( &
#endif
          spsi, psi, atm, g, kpt, global, addcharge, checkmode )
  use type_atom, only: atom
  use type_grid, only: grid
  use type_info, only: info
  use type_state, only: state
  use type_kpoint, only: kpoint!, kpoint_Gamma
  use type_grid, only: BC_PERIODIC
  use constants, only: Pi, PSE
  use configuration, only: WARNING, ERROR
  use MPIconst, only: Wtime
  use MPItools, only: MPIbarrier
  use unitsystem, only: eV, eV_, Ang, Ang_
#ifdef FULL_DEBUG
  use toolbox, only: operator(+)
#endif
#ifdef RAD_DEBUG
  use toolbox, only: operator(+)
  use toolbox, only: radial_hist
  use constants, only: Y00 => ONESQRTFOURPI
#endif
cNaN  use debugtools, only: NaN_search
  implicit none
    ! parameters
cDBG  character(len=*), parameter     :: fun = ' prepare_wavefunctions: '
    integer, parameter                :: K_ELL = -1 !! ell == -1  key for plane waves
    integer, parameter                :: K_IAT =  0 !! iat ==  0  key for plane waves

    character, parameter              :: ELLCHAR(K_ELL:5) = (/'K','s','p','d','f','g','h'/) !! ell-coding
    character(len=*), parameter       :: ENNCHAR(0:4) = (/'_?','  ','* ','**','*?'/) ! only 1 and 2 used
    character(len=*), parameter       :: HLINE = ' ---------------------------------'
cDBG  character(len=2), parameter     :: PERICHAR(-2:2) = (/'--',' -',' 0',' +','++'/)
    ! ---- indices ----
    integer, parameter                :: I_ELL = 0 ! angular momentum quantum number
    integer, parameter                :: I_ENN = 1 ! valence energy quantum number
    integer, parameter                :: I_EMM = 2 ! angular momentum z-component
    integer, parameter                :: I_ILN = 3 ! combindex ell-enn ! for radial fun.
    integer, parameter                :: I_ILM = 4 ! combindex ell-emm ! for sph. harmonic
    integer, parameter                :: I_IAT = 5 ! atom index

    integer, parameter                :: I_PWX = 1 ! plane wave in x-direction
    integer, parameter                :: I_PWY = 2 ! plane wave in y-direction
    integer, parameter                :: I_PWZ = 3 ! plane wave in z-direction

    ! arguments
    REAPLEX, intent(out)              :: spsi(:,:)
    type(state), intent(inout)        :: psi(:)
    type(atom), intent(in)            :: atm(:) ! list of all atoms
    type(grid), intent(in)            :: g
    type(kpoint), intent(in)          :: kpt(:)
    type(info), intent(in)            :: global
    real, intent(in)                  :: addcharge
    logical, intent(in)               :: checkmode
    ! result
    integer                           :: ist
    ! local vars
    integer                           :: nbnd, nspn, nkpt
    integer                           :: nbands, nspins
    integer                           :: nbsk, natm
    integer                           :: ib, ibnd, jbnd, ispn, jspn, ikpt, jkpt, ibsk
    integer                           :: iat, ell, enn, emm
    integer                           :: ilm0, ilm, mlnm, iln
    integer                           :: nxyzs, id

    real                              :: kv(3), spinw, flip

    integer                           :: norb
    integer, allocatable              :: orb(:,:)
    real, allocatable                 :: orb_energy(:)
    integer                           :: natorb, natorb_max, natorb_opt, natorb_min, natorb_lim
    real                              :: rn_min, rn_lim
    real                              :: occ(1:2), oc, addq
    integer                           :: i10, noccupied, i, n
    logical                           :: occup
    logical                           :: fullshells ! is true, if there are enough bands
    real                              :: maxcell
    real                              :: time, maxmom

    integer, allocatable              :: which_orb(:,:,:)
    integer, allocatable              :: which_iZ(:)
    integer                           :: iZ
    character(len=4)                  :: equals = '  = '

#ifdef TIME
    time = Wtime( )
#endif

    nspins = global%nspins
    spinw = 2./real(nspins)

    nbnd = global%nbnd
    nspn = global%nspn
    nkpt = global%nkpt

    nbands = global%nbands

    nbsk = size( spsi, 2 ) ! number of states in parallelized list
    if( .not. checkmode ) then
      if( nbsk < nbnd*nspn*nkpt ) stop 'PRE prepare_wavefunctions: dim #2 of SPSI /= NBND*NSPN*NKPT.'
    endif ! not checkmode

    natm = size( atm, 1 ) ! number of atoms in the parallelized list
    nxyzs = size( spsi, 1 ) ! number of degrees of freedom in a wf.

!     ! export the localized projector functions
!     do ia = 1, natm
!       do ii = 1, atm(ia)%nprj
!         if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'atom#',atm(ia)%ja,' write ',PERICHAR( atm(ia)%prjphase(:,ii) ),' to fort.9'
!         call lfun2file( 9, atm(ia)%prj(ii), comment='prj'+ii )
!       enddo ! ii
!     enddo ! ia

    natorb_max = 0 ! the highest number of atomic orbitals that can be generated
    natorb_opt = 0 ! the best number
    natorb_min = 0 ! the smallest number of atomic orbitals to host the charge
    natorb_lim = 0 ! the smallest number of atomic orbitals to host the atomic occupations
    rn_min = 0.
    rn_lim = 0.

    ! count the number of required bands, the optimal number of bands
    ! and the number of unoccupied atomic orbitals that can be generated
    do iat = 1, natm
      mlnm = 0 ! init couter of max. orbitals on this atom
      iln  = 0
      do ell = 0, atm(iat)%s%ellmax
        do enn = 1, atm(iat)%s%nn(ell)
          iln = iln+1

          ! occupation during pseudopotential calculation
          occ(1:2) = atm(iat)%s%occ_ln(1:2,iln)
cDBG      if( any( occ > 2*ell+1 ) ) stop 'PRE error in PAWdata, orbital occupation > 2 ell+1.'
          oc = max(occ(1),occ(2))
          if( oc > 0. ) then
            rn_min = rn_min + oc
            rn_lim = rn_lim + occ(1) + occ(2)
            ! the best would be a full shell (all emm-states),
            ! because this does not prefer any spatial direction
            natorb_opt = natorb_opt + (2*ell+1)
          endif ! occ > 0.

          mlnm = mlnm + (2*ell+1)
        enddo ! enn
      enddo ! ell
cDBG  if( iln  /= atm(iat)%s%mln  ) stop 'PRE error in species (s%MLN).'
cDBG  if( mlnm /= atm(iat)%s%mlnm ) stop 'PRE error in species (s%MLNM).'
      natorb_max = natorb_max + mlnm
    enddo ! iat

    natorb_min = nint( rn_min )

    if( nbands < natorb_min ) then
      if(o>0) write(o,'(3A,9(I0,A))') sym, fun, WARNING(0), nbands, ' bands < ', natorb_min, ' orbitals (minimal)!'
    endif ! nbnd < natorb_opt

    natorb_lim = nint( 0.5 * rn_lim )

    if( nbands < natorb_lim ) then
      if(o>0) write(o,'(3A,9(I0,A))') sym, fun, ERROR, nbands, ' bands < ', natorb_lim, ' orbitals (minimal)!'
      stop 'PRE prepare_wavefunctions: not enough bands for occupied atomic states.'
    endif ! nbnd < natorb_opt

    fullshells = ( nbands >= natorb_opt )

    if( .not. fullshells ) then
      if(o>0) write(o,'(3A,9(I0,A))') sym, fun, WARNING(0), nbands, ' bands < ', natorb_opt, ' orbitals (optimal)!'
    endif ! fullshells

cDBG  if( nbands > natorb_min .and. o>0) write(o,'(3A,I0,9A)') sym, fun, 'suggest ', nbands-natorb_min, ' plane-wave states.'

    ! suggest in addition to the occupied atomic states
    !   - unoccupied excited atomic orbitals
    !   - plane waves
    norb = max( nbands, natorb_max + max(nbands-natorb_min,0) )

cDBG  if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'test ', norb, ' orbitals'

    allocate( orb(I_ELL:I_IAT,1:norb), orb_energy(1:norb), stat=ist )
    orb = 0 ; orb_energy = 0. ! init

    ib = 0
    ! occupied states first and unoccupied states later
    do i10 = 1, 0, -1 ! 1 == occupied, 0 == unoccupied

!! The disadvantage of this procedure
!! is that if there are few states only,
!! atom #1 will get more than enough whereas
!! the others have to suffer. This leads to a
!! start configuration that breaks the symmetry.

      ! occupied states and unoccupied states
      do iat = 1, natm
        iln = 0
        ilm0 = 0
        do ell = 0, atm(iat)%s%ellmax ! or ELLMAX
          do enn = 1, atm(iat)%s%nn(ell)
            iln = iln+1

            occup = any( atm(iat)%s%occ_ln(:,iln) > 0. )
            if( occup .eqv. ( i10 == 1 ) ) then
              ! this is only executed, if
              !   1)   occupied and i10 == 1     or
              !   2) unoccupied and i10 == 0
              ilm = ilm0
              do emm = -ell, ell
                ilm = ilm+1
                if( ib < natorb_max ) then
                  ib = ib+1 ! count up ib
                  orb( (/I_ELL,I_ENN,I_EMM,I_ILN,I_ILM,I_IAT/) ,ib) = (/ell,enn,emm,iln,ilm,iat/)
                  ! energy of the atomic orbital
                  orb_energy(ib) = atm(iat)%s%ene_ln(1,iln)
                endif ! ib < nbands
              enddo ! emm
            endif ! occ > 0.

          enddo ! enn
          ilm0 = ilm0 + (2*ell+1)
        enddo ! ell
      enddo ! iat

      if( i10 == 1 ) noccupied = ib

    enddo ! i10

    natorb = ib ! number of all atomic orbitals
    ! if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'natorb = ', natorb, ' norb = ', norb, ' noccupied = ', noccupied

    ! still not enough wave functions?
    if( ib < norb ) then ! create some plane wave functions
      orb(I_ELL,ib+1:) = K_ELL ! ell
      orb(I_IAT,ib+1:) = K_IAT ! iat
      call plane_wave_modes( s=g%s, bc=g%bc(:,1), & ! Warning: only the lower BC is passed to this function
                             modes=orb(I_PWX:I_PWZ,ib+1:norb), &
                             energies=orb_energy(ib+1:norb) )
    endif ! ib < nbands

    ! sort the unoccupied orbitals by energy
    call orb_order( io=orb(:,noccupied+1:norb), eo=orb_energy(noccupied+1:norb) )

    ! display the list
    if( o > 0 ) then
cDBG  write(o,'(3A,9(I0,A))') sym, fun, 'nbands ', nbands, ', atomic orbitals: max ', natorb_max, ' occ ', natorb_min

#ifdef FULL_DEBUG
!! begin FULL_DEBUG

      write(o,'(9(/,3A))') sym, fun, 'initial wave functions:', HLINE
#ifdef FULL_DEBUG
      ! show all proposed atomic states and plane waves
      do ib = 1, max( norb, nbands )
#else
      ! show the atomic states and plane waves that will be prepared
      do ib = 1, nbands
#endif

        iat = orb(I_IAT,ib)
        if( iat > K_IAT ) then ! atomic wave function
          iln  = orb(I_ILN,ib)
          write(o,'(5A,I2,A,F10.3,2A,I0)') ' ', atm( iat )%s%sym, &
            ' [lnm]=[', ELLCHAR( orb(I_ELL,ib) ), ENNCHAR( orb(I_ENN,ib) ), orb(I_EMM,ib), '] E=', orb_energy(ib)*eV, eV_, ', atom #', iat
        elseif( orb(I_ELL,ib) == K_ELL ) then ! plane wave
          write(o,'(A,3I3,A,F10.3,A)') ' pw k=[', orb(I_PWX:I_PWZ,ib) , '] E=', orb_energy(ib)*eV, eV_
        else ; stop 'PRE undetermined inital wave function, neither atomic nor plane wave.'
        endif ! iat
        ! this line indicates, up to which proposed state
        ! preliminary wave functions are generated
        if( ib == nbands ) write(o,'(9A)') HLINE

      enddo ! ib

!! end FULL_DEBUG
#endif

      if( checkmode ) then

        ! show a summary of what kind of atomic orbitals have been selected
        if(o>0) write(o,'(2A,9(I0,A))') sym, fun, natorb, ' atomic orbitals:'

        allocate( which_orb(1:maxval(orb(I_ENN,:)),0:max(0,maxval(orb(I_ELL,:))),lbound(PSE,1):ubound(PSE,1)) )
        which_orb = 0 ! init
        do ib = 1, min(natorb,nbands)
          iat = orb(I_IAT,ib)
          if( iat < 1 ) cycle
          ell = orb(I_ELL,ib)
          enn = orb(I_ENN,ib)
          iZ  = atm(iat)%s%iZ
          which_orb(enn,ell,iZ) = which_orb(enn,ell,iZ)+1
        enddo ! ib

        allocate( which_iZ(lbound(PSE,1):ubound(PSE,1)) )
        which_iZ = 0
        do iat = 1, natm
          iZ  = atm(iat)%s%iZ
          which_iZ(iZ) = which_iZ(iZ)+1
        enddo ! iat

        do iZ = lbound( which_orb, 3 ), ubound( which_orb, 3 )
          do ell = 0, ubound( which_orb, 2 )
            do enn = 1, ubound( which_orb, 1 )
              n = which_orb(enn,ell,iZ)
              if( n > 0 ) then
                if( which_iZ(iZ) * (2*ell+1) /= n ) then
                  equals = ' !='
                  write(o,'(9A)') sym, fun, WARNING(0), 'not enough states to generate a full shell of ', PSE(iZ),' ',ELLCHAR(ell),'***'(1:enn-1), '-orbitals!'
                else ; equals = ' ='
                endif
                write(o,'(6A,I6,A,I0,2A,I0)') sym, fun, PSE(iZ),' ',ELLCHAR(ell),'***   '(5-enn:7-enn), which_iZ(iZ),' x ',2*ell+1,trim(equals),' ',n
              endif ! n > 0
            enddo ! enn
          enddo ! ell
        enddo ! iZ
        deallocate( which_orb, which_iZ )

      endif ! checkmode


      ! show resumee of the energy selection
      !-------------------------------------------------------------------------------------------
      i = count( orb(I_ELL,1:nbands) == K_ELL )
      n = count( orb(I_ELL,1:norb) == K_ELL )
      if( i > 0 ) write(o,'(3A,9(I0,A))') sym, fun, 'selected ', i, ' out of ', n, ' plane waves.'
      i = count( orb(I_IAT,1:nbands) > K_IAT )
!       n = count( orb(I_IAT,1:norb) > K_IAT )
!       if( i > 0 ) write(o,'(3A,9(I0,A))') sym, fun, 'selected ', i, ' out of ', n, ' atomic orbitals.'
      if( i > 0 ) write(o,'(3A,9(I0,A))') sym, fun, 'selected ', i, ' out of ', natorb_max, ' atomic orbitals.'

    endif ! o > 0


    if( addcharge < 0. ) then
      if(o>0) write(o,'(4A,F0.6,9A)') sym, fun, WARNING(0), 'negative additional charge may cause rho < 0, q = ', addcharge, ' e'
    endif ! addcharge negative

    !======================================================================================
    !== END of CHECKMODE ==================================================================
    !======================================================================================
    if( checkmode ) return
    !======================================================================================

    spsi = 0. ! clear wave functions
    maxcell = maxval(g%s)
    do id = 1, 3
      if( any( g%bc(id,:) == BC_PERIODIC ) ) maxcell = min( maxcell, g%s(id) )
    enddo ! id

cDBG  if( any( orb(I_IAT,:) > K_IAT ) ) then
    if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'atomic orbitals will be cut off with [1-(r/R)^',2*MASK_POW,']^',MASK_EXP

    if(o>0) write(o,'(3A,F0.3,9A)') sym, fun, ' ... at R = ', abs(maxcell)*Ang, Ang_, ' due to limited periodic images.'

cDBG  endif ! any atomic orbital

#ifdef TIME
    time = Wtime( ) - time
    if(o>0) write(o,'(3A,F10.3,9A)') sym, fun, 'time for wave function  selection', time, ' sec'
    time = Wtime( ) ! start time measurement again
#endif

cDBG    if(o>0) write(o,'(9A)') sym, fun, 'begin generation of start wave functions.'


    spsi = 0.

!!omp-comment: the dynamic scheduling is important because different bands may differ in their execution time
!$omp parallel do collapse(3) private(ikpt,ispn,ibnd,ibsk,jkpt,jspn,jbnd,ell,iln,ilm,iat,kv) schedule(dynamic)
    do ikpt = 1, nkpt
      do ispn = 1, nspn
        do ibnd = 1, nbnd
          ibsk = ibnd + nbnd*( ispn-1 + nspn*(ikpt-1) ) ! total index for band,spin,kpoint

          jkpt = ikpt + global%iokpt ! absolute k-point index
          jspn = ispn + global%iospn ! absolute collinear spin index
          jbnd = ibnd + global%iobnd ! absolute band index
#ifdef FULL_DEBUG
          if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'generate wave function #', jbnd, ' local index #', ibnd
#endif

          if( orb(I_IAT,jbnd) > K_IAT ) then
            ! create an atomic orbital

            ! load orbital identification numbers
            ell = orb(I_ELL,jbnd)
            iln = orb(I_ILN,jbnd)
            ilm = orb(I_ILM,jbnd)
            iat = orb(I_IAT,jbnd)

            ! occupation for spins according to PAW-data
            psi(ibsk)%ene = 0.5 * ( atm(iat)%s%ene_ln(1,iln) + atm(iat)%s%ene_ln(2,iln) ) ! spin averaged
            psi(ibsk)%occ = 0.5 * ( atm(iat)%s%occ_ln(1,iln) + atm(iat)%s%occ_ln(2,iln) ) * spinw/(2*ell+1.)

            !spinw = 2 if spin paired, 1 if polarized

            !!determine if the shell is open or closed!!!
            !!deg is (2*ell+1.)
            if(psi(ibsk)%occ > 0.0) then

              if( abs((atm(iat)%s%occ_ln(1,iln) + atm(iat)%s%occ_ln(2,iln)) - 2*(2*ell+1.)) < 1e-6  ) then
              !closed shell, not magnetic
              else
                psi(ibsk)%occ = 0.5 * ( atm(iat)%s%occ_ln(1,iln) + atm(iat)%s%occ_ln(2,iln) ) * spinw/(2*ell+1.)

              !write(*,*) 'opened!!'
              !check if we want magnetization
                  if((nspins .eq. 2) .and.  (atm(iat)%magmom /= 0.0)) then
                !!open shell, spin polarized calculation and we want magmom
                !!psi(ibsk)%occ is the spin up or spin down occupation that creates an unpolarized partially filled shell

                    maxmom = min(atm(iat)%s%occ_ln(1,iln) + atm(iat)%s%occ_ln(2,iln), 2*(2*ell+1.) - atm(iat)%s%occ_ln(1,iln) - atm(iat)%s%occ_ln(2,iln))
                    if(abs(atm(iat)%magmom) > maxmom) then
                      if (o>0) write(*,*) 'magmom for atom ', iat, 'is bigger than the maximum possible value'
                      stop
                    endif

                  endif

                  if(jspn .eq. 1) then
                    psi(ibsk)%occ = 0.5*(2.0*psi(ibsk)%occ - (atm(iat)%magmom/(2*ell+1.)))
                  else
                    psi(ibsk)%occ = 0.5*(2.0*psi(ibsk)%occ + (atm(iat)%magmom/(2*ell+1.)))
                  endif

              endif


cDBG              write(*,'(3a,3i4,a,f8.4,a,i4,a,f8.4)') sym, fun, '(jbnd,jspn,jkpt) = (', jbnd, jspn, jkpt, ') occ = ' , psi(ibsk)%occ, ' iat =', iat, ' magmom =', atm(iat)%magmom
            endif


            !if( nspins > 1 ) then
            !  if(o>0) write(o,'(3A,I0,A,99(F6.1,F4.1))') sym, fun, ' atom #', iat, ' ScaleInitialMagneticMoment=', atm(iat)%ScaleInitialMagneticMoment
            !  flip = min(max(-1., atm(iat)%ScaleInitialMagneticMoment ),1.)
            !  psi(ibsk)%ene = psi(ibsk)%ene + flip * (jspn-1.5) * ( atm(iat)%s%ene_ln(2,iln) - atm(iat)%s%ene_ln(1,iln) )
            !  if(o>0) write(o,'(3A,I0,A,99(F6.1,F4.1))') sym, fun, ' atom #', iat, ' atomic occ=', atm(iat)%s%occ_ln(:,iln)
            !  psi(ibsk)%occ = psi(ibsk)%occ + flip * (jspn-1.5) * ( atm(iat)%s%occ_ln(2,iln) - atm(iat)%s%occ_ln(1,iln) ) * spinw/(2*ell+1.)
            !  if(o>0) write(o,'(3A,F10.6,2A,F10.6,9(A,I0))') sym, fun, 'atomic orbital ene=',psi(ibsk)%ene*eV, eV_,' occ=',psi(ibsk)%occ,', atom#',iat,' spin=',jspn
            !endif ! ScaleInitialMagneticMoment

!           if(o>0) write(o,'(3A,I0,A,99(F6.1,F4.1))') sym, fun, ' atom #', iat, ' atomic occ=', atm(iat)%s%occ_ln
!           if(o>0) write(o,'(3A,F10.6,2A,F10.6,9(A,I0))') sym, fun, 'atomic orbital ene=',psi(ibsk)%ene*eV, eV_,' occ=',psi(ibsk)%occ,', atom#',iat,' spin=',jspn
#ifdef FULL_DEBUG
            if(o>0) write(o,'(3A,F0.6,2A,F0.6,A,I0)') sym, fun, 'atomic orbital ene=', psi(ibsk)%ene*eV, eV_, ' occ=', psi(ibsk)%occ, ', atom #', iat
#endif
            call generate_atomic_orbital( atm(iat), ell, iln, ilm, kpt(ikpt), maxcell, g, spsi(:,ibsk) )
!!!! write(o,'(11I4,9F12.6)') ikpt, ispn, ibnd, jkpt, jspn, jbnd, ell, iln, ilm, iat, ibsk, psi(ibsk)%ene, psi(ibsk)%occ

          elseif( orb(I_ELL,jbnd) == K_ELL ) then
            ! prepare plane wave
            psi(ibsk)%ene = orb_energy(jbnd)
            psi(ibsk)%occ = 0. ! cannot be set inside an omp-parallelized loop
#ifdef FULL_DEBUG
            if(o>0) write(o,'(3A,2(F0.6,2A))') sym, fun, 'atomic orbital ene=', psi(ibsk)%ene*eV, eV_
#endif
            kv(1:3) = 2.*Pi/g%s(1:3) ! plane wave k-vector

            call generate_plane_wave( g, m=orb(I_PWX:I_PWZ,jbnd), kv=kv(1:3), spsi=spsi(:,ibsk) ) ! create a PW

          else ; stop 'PRE undetermined inital wave function (2nd).'
          endif ! ...

cNaN      if( any( spsi(:,ibsk) /= spsi(:,ibsk) ) ) stop 'PRE prepare_wavefunctions NaN in SPSI'

        enddo ! ibnd
      enddo ! ispn
    enddo ! ikpt
!$omp end parallel do





    ! set the occupation numbers of the plane waves (all spin-kpoint sets will get the same occupation)
    if( any( orb(I_ELL,:) == K_ELL ) ) then
! !$omp parallel do collapse(2) private(ibsk,ikpt,ispn,ibnd,jbnd,addq,oc) schedule(dynamic)
      do ikpt = 1, nkpt
        do ispn = 1, nspn

          addq = max(0.,addcharge) ! copy
          do ibnd = 1, nbnd
            jbnd = ibnd + global%iobnd ! absolute band index
            if( orb(I_ELL,jbnd) /= K_ELL ) cycle
            ibsk = ibnd + nbnd*( (ispn-1) + nspn*(ikpt-1) ) ! total index for bands,col.spins,kpoint

            oc = min( addq, spinw )
            addq = addq - oc
            psi(ibsk)%occ = oc
#ifdef FULL_DEBUG
            if(o>0) write(o,'(3A,2(F0.6,2A))') sym, fun, 'plane wave ene=', psi(ibsk)%ene*eV, eV_, ' occ=', psi(ibsk)%occ
#endif
          enddo ! ibnd

        enddo ! ispn
      enddo ! ikpt
! !$omp end parallel do
    endif ! some plane waves

#ifdef TIME
    time = Wtime( ) - time
    if(o>0) write(o,'(3A,F10.3,9A)') sym, fun, 'time for wave function generation', time, ' sec'
#endif


!   do ibsk = 1, nkpt*nspn*nbnd
!     ikpt = 1+modulo((ibsk-1)/(nspn*nbnd),nkpt)
!      ispn = 1+modulo((ibsk-1)/(nbnd),nspn)
!       ibnd = 1+modulo((ibsk-1),nbnd)
!     write(o,'(3I4,9F12.3)') ikpt, ispn, ibnd, psi(ibsk)%ene, psi(ibsk)%occ
!   enddo

!   do ibsk = 1, nkpt*nspn*nbnd
!     write(99,'(F16.6)') 2.**20 * spsi(:,ibsk)
!     write(99,'(A)')
!   enddo ! ibsk


#ifdef RAD_DEBUG
    do iln = 1, 2
      do ir = 9, atm(1)%s%g%imx
        rs = atm(1)%s%g%r(ir)
        r2s = rs*rs
        mask = (1.0-(r2s*Rmax2inv)**MASK_POW)**MASK_EXP
  !                     xlm = Xlm_rl( ilm, v=rv ) ! one single spherical harmonic * r^ell
  !                       Xlm = xlm*rs**(-ell)
        write(100+iln,'(F10.6,ES16.6)') rs, atm(iat)%s%rwf(ir,iln,1,I_SMT) * mask * Y00
      enddo ! ir
    enddo ! iiln
    do ibnd = 1, nbnd ! data, hg, origin, ellmax, unit, file, comm
      call radial_hist( data=reshape( real(spsi(:,ibnd)), g%ng(1:3) ), hg=g%h, origin=atm(1)%pos-g%off, &
                        ellmax=2, file='dmp/start_wf.'+ibnd, comm=g%comm )
    enddo ! ibnd
    stop 'PRE RAD_DEBUG ... fort.10x ... dmp/start_wf.00000x ...'
#endif
    ! the two sets of 3 loops, even though running over the same
    ! range of states, have been separated, because the normalization
    ! requires synchronization, whereas the setup is independent.

cNaN  call NaN_search( spsi, fun, 'spsi after setting up atomic orbitals' )

    ! WARNING, wave functions are neither normalized nor orthogonal
  endsubroutine ! prepare_wavefunctions





#if R1_C2 == 1
  subroutine generate_atomic_orbital_r( &
#else
  subroutine generate_atomic_orbital_c( &
#endif
                       a, ell, iln, ilm, kp, maskrad, g, spsi )
  use type_atom, only: atom
  use type_grid, only: grid
  use type_kpoint, only: kpoint
  use operators, only: Bloch_phase
  use type_species, only: I_SMT
  use type_rgrid, only: operator(.at.)
  use harmonics, only: Xlm_rl
  implicit none
    ! parameters
cDBG  character(len=*), parameter     :: fun = ' gen_ao: '
    ! arguments
    type(atom), intent(in)            :: a ! only atm(iat)
    integer, intent(in)               :: ell, iln, ilm
    type(kpoint), intent(in)          :: kp
    real, intent(in)                  :: maskrad
    type(grid), intent(in)            :: g
    REAPLEX, intent(out)              :: spsi(:)
    ! local vars
    REAPLEX       :: bloch
    integer       :: irmx, ni, ii, ixyz, i1, i2, i3, ir
    real          :: rmx2, Rmax2inv, mask, rv(3), dv(3), r2s, rs, xlm

    irmx = a%s%g%imx
    ! the extend of the wave function may maximally be
    ! over the extend of one unit cell, otherwise the
    ! sampling of periodic positions {-1,0,1} must be
    ! enlarged to {-2,-1,0,1,2} or even higher
    Rmax2inv = 1./max(1.,maskrad**2)
    rmx2 = min( maskrad**2, a%s%g%rmx**2 ) ! Rmax^2
    ni = a%nimages

    spsi = 0. ! init

    ixyz = 0 ! init
    do i3 = 1, g%ng(3)
      do i2 = 1, g%ng(2)
        do i1 = 1, g%ng(1)
          ixyz = ixyz+1  ! count up

          dv(1:3) = (/i1,i2,i3/)*g%h + g%off(1:3)

          do ii = 1, ni
            rv(1:3) = dv(1:3) - a%imagepos(1:3,ii)

            r2s = rv(1)*rv(1) + rv(2)*rv(2) + rv(3)*rv(3) + 1E-6 ! safety
            if( r2s > rmx2 ) cycle

            ! prepare the real space cut-off mask that ensures the
            ! symmetry of the start wave functions even with only
            ! one periodic image in each direction
            mask = ( 1.0 - ( r2s * Rmax2inv ) ** MASK_POW ) ** MASK_EXP

            rs = sqrt( r2s )
            xlm = Xlm_rl( ilm, v=rv ) ! one single spherical harmonic * r^ell

            ir = a%s%g .at. rs ! eval radial grid index
            if( ir >= irmx ) cycle
            Xlm = xlm * rs ** (-ell)

            bloch = bloch_phase( kp%eik, a%prjphase(:,ii) ) ! conversion to real for if ( R1_C2 == 1 )
            ! use the SMT partial wave start wave function
            spsi(ixyz) = spsi(ixyz) + bloch * Xlm * a%s%rwf(ir,iln,1,I_SMT) * mask
          enddo ! ii

        enddo ! i1
      enddo ! i2
    enddo ! i3

  endsubroutine ! generate_atomic_orbital



#if R1_C2 == 1
  subroutine prepare_wf_planewaves_r( &
#else
  subroutine prepare_wf_planewaves_c( &
#endif
          spsi, psi, g, kpt, global, checkmode )
  use type_grid, only: grid
#ifdef FULL_DEBUG
!   use type_lfun, only: lfun2file
  use toolbox, only: print_polar
  use toolbox, only: cat
#endif
  use type_atom, only: atom
  use type_info, only: info
  use type_state, only: state
  use type_kpoint, only: kpoint, kpoint_Gamma
  use operators, only: scalar_product
  use constants, only: Pi
  use configuration, only: WARNING, ERROR
cNaN  use debugtools, only: NaN_search
cNaN  use toolbox, only: operator(+)
  implicit none
    ! parameters
    character(len=*), parameter       :: fun = ' prepare_wf_planewaves: '
    ! arguments
    REAPLEX, intent(out)              :: spsi(:,:)
!     REAPLEX, intent(out)              :: cpsi(:,:)
    type(state), intent(inout)        :: psi(:)
!     type(atom), intent(in)            :: atm(:) ! parallelized list, needed for Smat
    type(grid), intent(in)            :: g
    type(kpoint), intent(in)          :: kpt(:)
    type(info), intent(in)            :: global
    logical, intent(in)               :: checkmode
    ! local vars
!     REAPLEX, allocatable              :: Sspsi(:)
    integer                           :: nbnd, nspn, nkpt
    integer                           :: nbands!, nspins, nkpoints
    type(kpoint)                      :: kp
    integer                           :: nbsk, nxyzs!, naprj
    integer                           :: ib, ispn, ik, ibs, ibsk!, jspn
    integer, allocatable              :: orb(:,:)
    real, allocatable                 :: orb_energy(:)
    real                              :: kv(3), k2

    nbnd = global%nbnd
    nspn = global%nspn
    nkpt = global%nkpt

    nbands = global%nbands

    nbsk = size( spsi, 2 )
    if( .not. checkmode .and. nbsk /= nbnd*nspn*nkpt ) &
      stop 'PRE prepare_wavefunctions: dim #5 of SPSI /= NBND*NSPN*NKPT.'

!     naprj = size( cpsi, 1 )
! cDBG  if(o>0) write(o,'(2A,I6,A,2I6)') sym, fun, naprj, '=naprj, shape(cpsi)=', shape(cpsi)
    nxyzs = size( spsi, 1 )

    ! suggest plane waves modes
cDBG  if(o>0) write(o,'(2A,I0,9A)') sym, fun, nbands, ' plane waves'

    allocate( orb(1:3, nbands ), orb_energy( nbands ) )

    ! create some plane wave functions
    ! Warning: only the lower BC is passed to this function
    call plane_wave_modes( s=g%s, bc=g%bc(:,1), &
                            modes=orb(1:3,:), &
                            energies=orb_energy(:) )

! sorting unneccessary since the lowest energy modes are already found
! by sorting from a rectangular box of modes.
!     ! sort the orbitals by energy
!     call orb_order( io=orb(:,:), eo=orb_energy(:) )

    ! display the list
    if( o /= 0 ) then
      write(o,'(9A)') sym, fun, 'initial wave functions:'
      write(o,'(9A)') '------------------------'
      do ib = 1, nbands
        ! plane waves
        write(o,'(A,3I3,A,F10.3,A)') ' e^ikx,  k=[', orb(1:3,ib) , ' ], E =', orb_energy(ib), '  Ha'
      enddo ! ib
      write(o,'(9A)') '------------------------'
    endif ! o/=0

    if( checkmode ) return

    !-------------------------------
!     cpsi  = 0. ! clear coefficients
    spsi  = 0. ! clear wave functions
!     allocate( Sspsi(nxyzs) ) ! one temp. adjoint wave function
!     Sspsi = 0. ! clear adjoint wave function

    ibs = 0 ! index of band, spin and 1st k-point

    kp = kpoint_Gamma( )
      do ib = 1, nbnd
        ibs = ibs + 1

        ! prepare plane wave
        psi(ibs)%ene = orb_energy(ib)
#ifdef FULL_DEBUG
        if(o>0) write(o,'(3A,2(F10.6,A))') sym, fun, 'plane wave ene=', psi(ibs)%ene
#endif
        ! k-vector
        kv(1:3) = 2.*Pi/g%s(1:3)

        ! create a PW
        call generate_plane_wave( g, m=orb(1:3,ib), kv=kv(1:3), spsi=spsi(:,ibs) )
cNaN    call NaN_search( spsi(:,ibs) , fun, text='spsi(:,ibs),ibs='+ibs )
      enddo ! ib

cDBG  if( nkpt > 1 .and. o>0) write(o,'(9A)') sym, fun, 'create other k-point sets from Gamma-point copy'

    ! copy for the other k-points
    do ik = nkpt, 1, -1 ! count backwards because the initial
                        ! wave function set is generated at the Gamma point
                        ! and so the difference in k-vectors
                        ! is always the k-vector itself
      do ispn = 1, nspn
        do ib = 1, nbnd
          ibs  = ib                         ! source
          ibsk = (ik-1)*nspn*nbnd + ibs     ! destination
          spsi(:,ibsk) = spsi(:,ibs)        ! copy

!           if(o>0) write(o,'(3A,I6,I2,I6)') sym, fun, 'call otherkpoint_copy for [b,s,k]', ib, ispn, ik

          ! correct the phase with \exp{ i \vec \delta k * \vec r}
          call otherkpoint_copy( s=spsi(:,ibsk), dk=kpt(ik)%k, g=g )
          k2 = sum( ( 2.*Pi*kpt(ik)%k(1:3)/g%s )**2 )
          psi(ibsk)%ene = psi(ibs)%ene + 0.5 * k2 ! positive dispersion assumed

        enddo ! ib
      enddo ! ispn

    enddo ! ik

cNaN  ik = count( spsi /= spsi )
cNaN  if( ik > 0 .and. o>0) write(o,'(3A,9(I0,A))') sym, fun, 'spsi has ', ik , ' / ', size(spsi), ' NaN entries.'
  endsubroutine ! prepare_wf_planewaves





#if R1_C2 == 1
  subroutine otherkpoint_copy_r( &
#else
  subroutine otherkpoint_copy_c( &
#endif
                                 s, dk, g )
  use constants, only: Pi
  use configuration, only: WARNING
  use type_state, only: state
  use type_grid, only: BC_FINITE, BC_PERIODIC, BC_MIRROR
#ifdef GENERAL_CELL
  use type_grid, only: BC_SHIFTED
#endif
  use type_grid, only: grid
  implicit none
    ! parameter
    character(len=*), parameter     :: fun = ' otherkpoint_copy: '
    complex, parameter              :: m1  = (-1.0,0.0) ! -1
    ! arguments
    REAPLEX, intent(inout)          :: s(:) ! wave function to be multiplied with e(ikx)
    real, intent(in)                :: dk(3)
    type(grid), intent(in)          :: g
    ! local vars
    complex                         :: ph
    integer                         :: i1, i2, i3, i4, id, ie
    real                            :: k(3), rv(3), arg
cDBG  ie = product(g%ng(1:4)) ! number of elements
cDBG  if(size(s,1)/=ie) stop 'PRP: otherkpoint_copy: dim #1 of S does not match grid'

    k = dk
    if( all( k(1:3) == 0. ) ) then
      return
    endif ! all k == 0

#ifdef FULL_DEBUG
    if(o>0) write(o,'(3A,3F7.3,9A)') sym, fun, 'apply e^{ikx}, where k=[', dk, ' ].'
#endif

    !=========================================================================
    do id = 1, 3
      selectcase( g%bc(id,1) ) ! check only the lower boundary
      case( BC_FINITE )
        ! no action
cDBG    if( k(id) /= 0.0 .and. o>0) write(0,'(4A,F0.3,9A)') sym, fun, WARNING(0), 'k-point differs from 0., k = ',k(id),' in ',achar(119+id),'-direction (isolated BC).'
      case( BC_MIRROR )

        if( nint( 2.*abs(k(id)) ) == 1 ) then
          ! antisymmetric solutions required
          call antisymmetrize( s, id, g )
          k(id) = 0. ! switch off the application of e^{ikx} (later) in this direction

        elseif( nint( 2.*k(id) ) == 0 ) then
          ! symmetric solutions
          ! no action
cDBG    else ! k(id) is not half integer
cDBG      if(o>0) write(0,'(4A,F0.3,9A)') sym, fun, WARNING(0), 'k-point coordinate not half integer, k = ',k(id),' in ',achar(119+id),'-direction (mirror BC).'
        endif ! |k| == 0.5 or 0.0


      case( BC_PERIODIC )
        ! will be treated afterwards
#ifdef GENERAL_CELL
      case( BC_SHIFTED )
        ! works as periodic -> later
#endif

cDBG  case default
cDBG    if(o>0) write(o,'(4A,I0)') sym, fun, WARNING(0), 'other BC not implemented, BC-key = ', g%bc(id,1)
      endselect ! bc
    enddo ! id

    if( all( k(1:3) == 0. ) ) then
      ! all needs satisfied.
! cDBG  if(o>0) write(0,'(4A,F6.3,9A)') sym, fun, 'e^{ikx} is not needed.'
      return
    endif ! all k == 0.

    ie = 0
    do i4 = 1, g%ng(4)
      do i3 = 1, g%ng(3)                  ; rv(3) = g%off(3) + i3 * g%h(3)
        do i2 = 1, g%ng(2)                ; rv(2) = g%off(2) + i2 * g%h(2)
          do i1 = 1, g%ng(1)              ; rv(1) = g%off(1) + i1 * g%h(1)
            ie = ie+1
            arg = dot_product( 2.*k, rv/g%s )
            ph = m1**( arg ) ! = e^{ikx}
!             arg = 2*Pi*dot_product( k, rv/g%s )
!             ph = cmplx( cos(arg), sin(arg) )
#ifdef FULL_DEBUG
!   if(o>0) write(o,'(3A,3I4,A,3F7.3,A,2F10.6)') sym, fun, 'ind[',i1,i2,i3,'] k[', k, ' ] ph=', ph
#endif
            s(ie) = s(ie) * ph
          enddo ! i1
        enddo ! i2
      enddo ! i3
    enddo ! i4

  endsubroutine ! otherkpoint_copy




#if R1_C2 == 1
  subroutine antisymmetrize_r( &
#else
  subroutine antisymmetrize_c( &
#endif
                               s, id, g )
  ! on entry, s should be a symmetric wave function with
  ! the lower boundary plane as a mirror.
  ! en exit, s contains the antisymmetric wave function
  ! that has a nodal plane in the mirror plane
  ! and is almost the same in the rest of space
  use constants, only: Pi
  use type_grid, only: grid
  implicit none
    ! parameter
    character(len=*), parameter     :: fun = ' antisymmetrize: '
    real, parameter                 :: FAC = 2./Pi
    ! arguments
    REAPLEX, intent(inout)          :: s(:)
    integer, intent(in)             :: id
    type(grid), intent(in)          :: g
    ! local vars
    integer                         :: i1, i2, i3, i4, ie
    real                            :: rv(3), scal

cDBG  integer                       :: ne

cDBG  ne = product(g%ng(1:4)) ! number of elements
cDBG  if(size(s,1)/=ne) stop 'PRP: antisymmetrize: dim #1 of  S does not match grid'
cDBG  if( id < 1 .or. id > 3 ) stop 'PRP: antisymmetrize: ID out of bounds [1,3]'
cDBG  ! output
! cDBG  if(o>0) write(o,'(9A)') sym, fun, 'in ', achar(119+id), '-direction.'
! cDBG  if(o>0) write(o,'(3A,3I5,I2,9A)') sym, fun, 'grid numbers [', g%ng(1:4), ']'

    !=========================================================================

    !      ^
    !      |
    !     1+   _,..---------------
    !      |  /  arcustangens(x) * 2/Pi
    !      | /
    !    -0+-------------------> x
    !      M (= Mirror Plane)

    ie = 0
    do i4 = 1, g%ng(4)
      !------------------
      do i3 = 1, g%ng(3)                  ; rv(3) = g%off(3) + i3 * g%h(3)
        do i2 = 1, g%ng(2)                ; rv(2) = g%off(2) + i2 * g%h(2)
          do i1 = 1, g%ng(1)              ; rv(1) = g%off(1) + i1 * g%h(1)
            !------------------
            ie = ie+1
            scal = atan( rv(id) ) * FAC ! FAC = 2/Pi
            s(ie) = s(ie) * scal
            !------------------
          enddo ! i1
        enddo ! i2
      enddo ! i3
      !------------------
    enddo ! i4
! cDBG  if(o>0) write(o,'(3A,F10.6)') sym, fun, 'scale by', scal
cDBG  if( ie /= ne ) stop 'PRP: antisymmetrize: fatal counting error.'
    !=========================================================================
  endsubroutine ! antisymmetrize






#if R1_C2 == 1
  subroutine generate_plane_wave_r( &
#else
  subroutine generate_plane_wave_c( &
#endif
                                    g, m, kv, spsi )
  use configuration, only: WARNING
  use type_grid, only: grid
  implicit none
    ! parameters
    character(len=*), parameter       :: fun = ' generate_plane_wave: '
    ! arguments
    type(grid), intent(in)            :: g
    integer, intent(in)               :: m(1:3)
    real, intent(in)                  :: kv(1:3)
    REAPLEX, intent(out)              :: spsi(:)
    REAPLEX                           :: w3, w23 !
    ! local vars
    integer                           :: i3, i2, i1, ixyz
    integer                           :: bc(1:3)
    real                              :: rv(1:3)

    bc = g%bc(1:3,1) ! lower BC

    ixyz = 0 ! init overall index
    do i3 = 1, g%ng(3) ;     rv(3)  = i3*g%h(3) + g%off(3)
                       w3  = planewave( m(3), kv(3), rv(3), bc(3) )
      do i2 = 1, g%ng(2) ;   rv(2)  = i2*g%h(2) + g%off(2)
                  w23 = w3 * planewave( m(2), kv(2), rv(2), bc(2) )
        do i1 = 1, g%ng(1) ; rv(1)  = i1*g%h(1) + g%off(1)
          ixyz = ixyz+1 ! count up
          ! conversion to real if R1_C2 == 1 happens earlier ! is that correct?
          spsi(ixyz) = w23 * planewave( m(1), kv(1), rv(1), bc(1) )

        enddo ! i1
      enddo ! i2
    enddo ! i3

cDBG  if( ixyz /= size( spsi ) .and. o>0) write(o,'(9A)') sym, fun, WARNING(0), 'number of elements written does not match array size.'
  endsubroutine ! generate_plane_wave




! start of tail part
#if R1_C2 == 2
#undef REAPLEX


  complex function planewave( m, k, r, bc ) result( p )
  use type_grid, only: BC_PERIODIC, BC_FINITE, BC_MIRROR
#ifdef GENERAL_CELL
  use type_grid, only: BC_SHIFTED
#endif
  implicit none
    ! arguments
    integer, intent(in)       :: m ! m_integer
    real, intent(in)          :: k ! kv(id)
    real, intent(in)          :: r ! rv(id)
    integer, intent(in)       :: bc
    ! local vars
    real                      :: arg

    selectcase( bc )
#ifdef GENERAL_CELL
    case( BC_PERIODIC, BC_SHIFTED )
#else
    case( BC_PERIODIC )
#endif
      arg = m * k * r
      p = cmplx( cos(arg), sin(arg) )
    case( BC_FINITE )
      arg = (m-0.5) * k * r
      p = cmplx( cos(arg), 0.0 )
    case( BC_MIRROR )
      arg = 0.5 * (m-0.5) * k * r
      p = cmplx( cos(arg), 0.0 )
    case default
      stop 'PRE planewave: this key for BC is not implmented.'
    endselect ! bc

  endfunction planewave


  subroutine plane_wave_modes( s, bc, modes, energies )
  use constants, only: Pi
  use type_grid, only: BC_FINITE, BC_PERIODIC, BC_MIRROR
  use unitsystem, only: eV, eV_
  use sorting, only: permutation_of
  implicit none
    ! parameter
    character(len=*), parameter           :: fun = ' plane_wave_modes: '
    logical, parameter                    :: DSP_UNSRT_MODES = .false.
    logical, parameter                    :: DSP_SRTED_MODES = .false.
    ! arguments
    real, intent(in)                      :: s(3) ! size of the cell
    integer, intent(in)                   :: bc(3) ! boundary condition
    integer, intent(out)                  :: modes(:,:)
    real, intent(out)                     :: energies(:)
    ! local vars
    integer                               :: Nk
    integer                               :: id, mmin(3), ik, ii, io, mx, my, mz, mi
    integer                               :: mmax(3)
    integer, allocatable                  :: m(:,:), ind(:)
    real, allocatable                     :: e(:)
    real                                  :: sm2(3) ! (Pi/s)^2
    real                                  :: ss(1:3), p_s, Vol, v13 ! product of sizes

    ss = s

    Nk = size( modes, 2 )
cDBG  if( size( modes, 1 ) /= 3 ) stop 'PRE plane_wave_modes: dim #1 of MODES is not 3.'
cDBG  if( size( energies, 1 ) /= Nk ) stop 'PRE plane_wave_modes: dim of ENERGIES is not match dim #2 of MODES.'
cDBG  if(o>0) write(o,'(3A,I6,9A)') sym, fun, 'number of states', Nk

    do id = 1, 3
      selectcase( bc(id) )
      case( BC_PERIODIC ) ; mmin(id) = 0 ! for periodic wave functions, e^(0*ikx) = 1 is a valid mode
      case( BC_MIRROR ) ;   mmin(id) = 1 ! for mirror symmetry, cos(1*kx) would be a good wave function
                            ss(id) = 2. * ss(id)
      case default ;        mmin(id) = 1 ! for isolated wave functions, sin(1*kx) is at least needed
      endselect ! bc
    enddo ! id

    sm2 = ( Pi/ss )**2 ! reciprocal space vector
    p_s = product( ss(1:3) ) ! Volume of the real space cell
    Vol = 6.*real(Nk)/(Pi*p_s) ! Volume of the Fermi sphere
    v13 = Vol**(1./3.)

    mmax(1:3) = nint( s(1:3) * v13 ) + mmin(1:3)
cDBG  if(o>0) write(o,'(3A,F0.3,9A)') sym, fun, 'average # of states in each direction ', real(product( mmax(1:3) ))**(1./3.)

    ! now make a list of the mi lowest energy modes
    mi = product( (mmax+1) ) ! because some might run from 0

    allocate( m(3,mi), e(mi) )

    ii = 0 ! counter
    do mz = mmin(3), mmax(3)
      do my = mmin(2), mmax(2)
        do mx = mmin(1), mmax(1)
          ii = ii+1 ! counter
          ! mode
          m(1:3,ii) = (/mx,my,mz/)
          ! energy
          e(ii) = 0.5 * sum( sm2(1:3) * real( m(1:3,ii)**2 ) )
        enddo ! mx
      enddo ! my
    enddo ! mz

#ifdef FULL_DEBUG
    if(o>0) write(o,'(3A,2(I0,A),F4.1,9A)') sym, fun, 'find ', Nk, ' lowest energies in ', ii, ' modes (=', 100.*real(Nk)/real(ii), '%).'
#endif
    if( ii < Nk ) stop 'PRE fatal error, Volume of the Fermi sphere wrong!'

#ifdef FULL_DEBUG
    if( DSP_UNSRT_MODES .and. o/=0 ) write(o,'(A,3I3,A,F10.3,A)') ( ' [', m(1:3,ik), ' ], e =', e(ik)*eV, eV_, ik=1,ii )
#endif

    allocate( ind(1:ii) )
    ! sort list by energies
    ind = permutation_of( ii, e(1:ii) )

    ! copy the Nk plane waves with the lowest energy
    do ik = 1, Nk
      io = ind(ik)
      modes(1:3,ik) = m(1:3,io) ! copy the mode
      energies(ik)  = e(io)     ! copy the energy
    enddo ! ik

#ifdef FULL_DEBUG
!     if( DSP_UNSRT_MODES .and. o/=0 ) then
!       write(o,'(9A)') sym, fun, 'plane wave mode energies:'
!       write(o,'(A,3I3,A,F10.3,A)') ( ' [', m(1:3,ind(ik)), ' ], e =', e(ind(ik)), '  Ha', ik=1,Nk )
!       ! display the following higher energy modes
!       write(o,'(9A)') ' --------------------------------' ! line
!       write(o,'(A,3I3,A,F10.3,A)') ( ' [', m(1:3,ind(ik)), ' ], e =', e(ind(ik)), '  Ha', ik=Nk+1,ii )
!     endif ! DSP_UNSRT_MODES .and. o/=0
#endif

cDBG  if(o>0) write(o,'(3A,2(I0,A),F6.2,9A)') sym, fun, 'found ', Nk, ' lowest energies in ', ii, &
cDBG    ' modes (=', 100.*real(Nk)/real(ii), '%, expected 52.36%)'
cDBG  if(o>0) write(o,'(3A,F10.3,9A)') sym, fun, 'highest energy =', e(ind(Nk))*eV, eV_

#ifdef FULL_DEBUG
    ! display only the modes
    if(o>0) write(o,'(A,3I3,A)') ( ' [', modes(1:3,ik), ' ]', ik=1,ii )
#endif
  endsubroutine ! plane_wave_modes

  subroutine orb_order( io, eo )
  ! orders the arrays according to their energy
  use sorting, only: permutation_of
  implicit none
    ! parameter
    character(len=*), parameter     :: fun = ' orb_order: '
    ! arguments
    integer, intent(inout)          :: io(:,:)
    real, intent(inout)             :: eo  (:)
    ! local vars
    integer                         :: n, i, new_i(size(io,1),size(io,2)), perm(size(io,2))
    real                            :: new_e(size(io,2))
    n = size( io, 2 )

    ! get the right permutation according to the energies
    perm = permutation_of( n, eo )

    do i = 1, n
      new_e(  i) = eo(  perm(i))
      new_i(:,i) = io(:,perm(i))
    enddo ! i

    ! overwrite old
    io(:,:) = new_i(:,:)
    eo  (:) = new_e  (:)
  endsubroutine ! orb_order

#ifdef EXTENDED
!+ extended

  integer function test( )
    write(*,*,iostat=test) __FILE__,' no module test implemented!'
  endfunction ! test

!- extended
#endif

endmodule ! prepare
! end of double preprocessing
#endif
#endif


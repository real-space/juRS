#ifdef DEBUG_ALL
#define DEBUG
#endif

! #define DEBUG


! #define NEW_SYMMETRIZE

#ifdef DEBUG
#define cDBG  
#else
#define cDBG ! 
#endif

#ifdef TIME
#define cTIM  
#else
#define cTIM ! 
#endif

!! @author Paul Baumeister
!! @version 4.00
!!
!! symmetrization of cartesian quantities
!! only for special cases of symmetries
module symmetry
  use configuration, only: o ! output unit, 0: no output
  use type_item, only: item
  use type_species, only: ELLMAX
implicit none
  private ! default for the module namespace
  character(len=*), parameter, private :: sym = 'SYM' !! module symbol
  character(len=*), parameter, private :: fun = ': ' !! function symbol

  public :: generate_kpoint_set
  public :: symmetrize_radial_coefficients
  public :: symmetrize_density_matrix
  public :: symmetrize_cartesian_quantity
  public :: check_symmetry
  public :: kgen
#ifdef EXTENDED
  public :: test
#endif

#ifdef NEW_SYMMETRIZE
  public :: symmetrize
#endif


  interface symmetrize_cartesian_quantity
    module procedure symmetrize_cartesian_quantity_r3, & ! rank-3 arrays
                     symmetrize_cartesian_quantity_r4    ! rank-4 arrays -> wrapper for rank-3
  endinterface

#ifdef NEW_SYMMETRIZE
  interface symmetrize
    module procedure symmetrize_fcc
  endinterface
#endif

  integer, parameter :: KSYM_REV = -1 ! time reversal
  integer, parameter :: KSYM_NONE=  0 ! default
  integer, parameter :: KSYM_SC  =  1
  integer, parameter :: KSYM_BCC =  2
  integer, parameter :: KSYM_NEW =  3
  integer, parameter :: KSYM_FCC =  4
  integer, parameter :: KSYM_DIA =  5
  integer, parameter :: KSYM_HEX =  6
  integer, parameter :: KSYM_HCP =  7
  integer, parameter :: KSYM_XROT=  8 ! chain in x-direction
  integer, parameter :: KSYM_INV =  9 ! chain in x-direction
  !---------------------------------------
  integer, parameter :: KSYM_MIN = -1
  integer, parameter :: KSYM_MAX =  9

  integer, parameter :: KSYM_DEFAULT = KSYM_REV

  type(item), parameter, public :: Dictionary(28) = (/ &
    item('default',KSYM_DEFAULT), item('',KSYM_DEFAULT), &
    item('no',KSYM_NONE), item('non',KSYM_NONE), item('0',KSYM_NONE), &
    item('Inversion',KSYM_INV), item('inv',KSYM_INV), &
    item('TimeReversal',KSYM_REV), item('rev',KSYM_REV), item('trev',KSYM_REV), &
    item('SimpleCubic',KSYM_SC), item('sc',KSYM_SC), &
    item('BodyCenteredCubic',KSYM_BCC), item('bcc',KSYM_BCC), &
    item('NewSymmetry',KSYM_NEW), item('new',KSYM_NEW), &
    item('FaceCenteredCubic',KSYM_FCC), item('fcc',KSYM_FCC), &
    item('Diamond',KSYM_DIA), item('DiamondStructure',KSYM_DIA), item('dia',KSYM_DIA), &
    item('Hexagonal',KSYM_HEX), item('hex',KSYM_HEX), &
    item('HexagonalClosedPacked',KSYM_HCP), item('hcp',KSYM_HCP), &
    item('RotationAroundXAxis',KSYM_XROT), item('xrot',KSYM_XROT), &
    item('none',KSYM_NONE) /)

!   character(len=*), parameter :: SYMMETRY_NAME(KSYM_MIN:KSYM_MAX) = &
!     (/  'TimeReversal         ', & ! -1 (default)
!         'NoSymmetry           ', & !  0
!         'SimpleCubic          ', & !  1
!         'BodyCenteredCubic    ', & !  2
!         'NewSymmetry          ', & !  3
!         'FaceCenteredCubic    ', & !  4
!         'DiamondStructure     ', & !  5
!         'Hexagonal            ', & !  6
!         'HexagonalClosedPacked', & !  7
!         'RotationAroundXAxis  ', & !  8
!         'Inversion            ' /) !  9


  ! cubic symmetry operations, apply to SC, BCC, FCC
  integer, parameter :: MROT48(3,3,48) = reshape( (/ &
     1,  0,  0,  0,  1,  0,  0,  0,  1, & !
     0,  0,  1,  1,  0,  0,  0,  1,  0, & !
     0,  1,  0,  0,  0,  1,  1,  0,  0, & !
     0,  0,  1,  0,  1,  0,  1,  0,  0, & !
     0,  1,  0,  1,  0,  0,  0,  0,  1, & !
     1,  0,  0,  0,  0,  1,  0,  1,  0, & !
    -1,  0,  0,  0,  1,  0,  0,  0,  1, & !
     0,  0, -1,  1,  0,  0,  0,  1,  0, & !
     0, -1,  0,  0,  0,  1,  1,  0,  0, & !
     0,  0, -1,  0,  1,  0,  1,  0,  0, & !
     0, -1,  0,  1,  0,  0,  0,  0,  1, & !
    -1,  0,  0,  0,  0,  1,  0,  1,  0, & !
     1,  0,  0,  0, -1,  0,  0,  0,  1, & !
     0,  0,  1, -1,  0,  0,  0,  1,  0, & !
     0,  1,  0,  0,  0, -1,  1,  0,  0, & !
     0,  0,  1,  0, -1,  0,  1,  0,  0, & !
     0,  1,  0, -1,  0,  0,  0,  0,  1, & !
     1,  0,  0,  0,  0, -1,  0,  1,  0, & !
    -1,  0,  0,  0, -1,  0,  0,  0,  1, & !
     0,  0, -1, -1,  0,  0,  0,  1,  0, & !
     0, -1,  0,  0,  0, -1,  1,  0,  0, & !
     0,  0, -1,  0, -1,  0,  1,  0,  0, & !
     0, -1,  0, -1,  0,  0,  0,  0,  1, & !
    -1,  0,  0,  0,  0, -1,  0,  1,  0, & !
     1,  0,  0,  0,  1,  0,  0,  0, -1, & !
     0,  0,  1,  1,  0,  0,  0, -1,  0, & !
     0,  1,  0,  0,  0,  1, -1,  0,  0, & !
     0,  0,  1,  0,  1,  0, -1,  0,  0, & !
     0,  1,  0,  1,  0,  0,  0,  0, -1, & !
     1,  0,  0,  0,  0,  1,  0, -1,  0, & !
    -1,  0,  0,  0,  1,  0,  0,  0, -1, & !
     0,  0, -1,  1,  0,  0,  0, -1,  0, & !
     0, -1,  0,  0,  0,  1, -1,  0,  0, & !
     0,  0, -1,  0,  1,  0, -1,  0,  0, & !
     0, -1,  0,  1,  0,  0,  0,  0, -1, & !
    -1,  0,  0,  0,  0,  1,  0, -1,  0, & !
     1,  0,  0,  0, -1,  0,  0,  0, -1, & !
     0,  0,  1, -1,  0,  0,  0, -1,  0, & !
     0,  1,  0,  0,  0, -1, -1,  0,  0, & !
     0,  0,  1,  0, -1,  0, -1,  0,  0, & !
     0,  1,  0, -1,  0,  0,  0,  0, -1, & !
     1,  0,  0,  0,  0, -1,  0, -1,  0, & !
    -1,  0,  0,  0, -1,  0,  0,  0, -1, & !
     0,  0, -1, -1,  0,  0,  0, -1,  0, & !
     0, -1,  0,  0,  0, -1, -1,  0,  0, & !
     0,  0, -1,  0, -1,  0, -1,  0,  0, & !
     0, -1,  0, -1,  0,  0,  0,  0, -1, & !
    -1,  0,  0,  0,  0, -1,  0, -1,  0  & !
          /), (/3,3,48/) ) ! reshape to 48 3x3 matrices

#ifdef USE_SYMMETRY
  integer, parameter :: MROT6(3,3,6) = reshape( (/ &
                  1, 0, 0,   0, 1, 0,   0, 0, 1  ,&
                  0, 0, 1,   1, 0, 0,   0, 1, 0  ,&
                  0, 1, 0,   0, 0, 1,   1, 0, 0  ,&
                  0, 0, 1,   0, 1, 0,   1, 0, 0  ,&
                  0, 1, 0,   1, 0, 0,   0, 0, 1  ,&
                  1, 0, 0,   0, 0, 1,   0, 1, 0   &
          /), (/3,3,6/) ) ! reshape to 6 3x3 matrices

  integer, parameter :: MMIR8(3,3,8) = reshape( (/ &
                  1, 0, 0,   0, 1, 0,   0, 0, 1  ,&
                 -1, 0, 0,   0, 1, 0,   0, 0, 1  ,&
                  1, 0, 0,   0,-1, 0,   0, 0, 1  ,&
                 -1, 0, 0,   0,-1, 0,   0, 0, 1  ,&
                  1, 0, 0,   0, 1, 0,   0, 0,-1  ,&
                 -1, 0, 0,   0, 1, 0,   0, 0,-1  ,&
                  1, 0, 0,   0,-1, 0,   0, 0,-1  ,&
                 -1, 0, 0,   0,-1, 0,   0, 0,-1   &
          /), (/3,3,8/) ) ! reshape to 8 3x3 matrices

  integer, parameter :: MIRR8(3,8) = reshape( (/ & ! only diagonal terms of MMIR8
                  1,  1,  1  ,&
                 -1,  1,  1  ,&
                  1, -1,  1  ,&
                 -1, -1,  1  ,&
                  1,  1, -1  ,&
                 -1,  1, -1  ,&
                  1, -1, -1  ,&
                 -1, -1, -1   &
          /), (/3,8/) ) ! reshape to 8 3 vectors

  integer, parameter :: PROT6(3,6) = reshape( (/ & ! only permutations of MROT6
                  1, 2, 3 ,&
                  3, 1, 2 ,&
                  2, 3, 1 ,&
                  3, 2, 1 ,&
                  2, 1, 3 ,&
                  1, 3, 2  &
          /), (/3,6/) ) ! reshape to 8 3 vectors


!   ! Tables
!   integer, parameter :: ILM_NONZERO_BCC_FCC(3) = (/  1,17,21 /)
!   integer, parameter :: ILM_NONZERO_DIA(4) = (/  1,15,17,21 /)
!   integer, parameter :: ILM_NONZERO_HCP(9) = (/  1,4,5,7,14,16,17,19,21 /)


  ! index list for fcc symmetry
  integer, allocatable :: ifcc(:,:,:)
#endif

  contains


!   logical function check_key( key, name )
!     ! parameters
! cDBG  character(len=*), parameter     :: fun = ' check_key: '
!     ! args
!     integer, intent(in)                      :: key
!     character(len=*), intent(out), optional  :: name
! 
!     selectcase( key )
!     case( KSYM_MIN:KSYM_MAX ) ; check_key = .true.
!       if( present( name ) ) name = SYMMETRY_NAME( key )
!     case default            ; check_key = .false.
!       if( present( name ) ) name = '<unknown key>'
!     endselect ! key
! 
!   endfunction check_key



!   integer function string2key( str ) result( k )
!   use configuration, only: WARNING
!   use configuration, only: KEYWORD, I_KEYWORD_SYMM
!     ! parameters
! cDBG  character(len=*), parameter     :: fun = ' string2key: '
!     ! args
!     character(len=*), intent(in)    :: str
!     ! local vars
!     character(len=24)               :: name
!     logical                         :: tru, show
!     show = .true.
! 
!     ! TODO: convert str to lowercase
!     selectcase( str )
!     case( 'inv' ) ; k = KSYM_INV
!     case( 'sc' )  ; k = KSYM_SC
!     case( 'bcc' ) ; k = KSYM_BCC
!     case( 'new' ) ; k = KSYM_NEW
!     case( 'fcc' ) ; k = KSYM_FCC
!     case( 'dia' ) ; k = KSYM_DIA
!     case( 'hcp' ) ; k = KSYM_HCP
!     case( 'xrot' ); k = KSYM_XROT
!     case( 'rev' ) ; k = KSYM_REV
!     case( 'no', 'non', 'none', '0' )
!                     k = KSYM_NONE
!     case( 'default' )
!       show=.false.; k = KSYM_DEFAULT ! no output
!     case default ;  k = KSYM_DEFAULT ! warning
!       if(o>0) write(o,'(9A)') sym, fun, WARNING(0), '"', trim(str), '" is no valid parameter for ', trim(KEYWORD(I_KEYWORD_SYMM)), ', use default'
!     endselect ! str
!     tru = check_key( k, name=name )
!     if( .not. tru ) stop 'SYM k: fatal error, no valid key assigned.'
!     ! show
!     if(o>0 .and. show ) write(o,'(9A)') sym, fun, 'use ', trim(name), ' as symmetry option.'
! 
!   endfunction string2key



  subroutine symmetrize_radial_coefficients( a, me, ksym )
  use type_atom, only: atom
    ! parameters
cDBG  character(len=*), parameter     :: fun = ' symmetrize_radial_coefficients: '
    ! args
    type(atom), intent(inout)       :: a(:)
    integer, intent(in)             :: me
    integer, intent(in)             :: ksym ! key
#ifdef USE_SYMMETRY
    ! local vars
    integer                         :: ia, m

    do ia = 1, size(a)
      if( a(ia)%owner /= me ) cycle
      m = size( a(ia)%GntDm, 3 )

      !     ==========  symmetry operations  ==========
      selectcase( ksym )
      case( KSYM_NONE )
      case( KSYM_INV )
      case( KSYM_SC ) ; a(ia)%GntDm(:,:,2:,:) = 0.
      case( KSYM_BCC )
      case( KSYM_NEW ) ! not implemented
      case( KSYM_FCC ) ; a(ia)%GntDm(:,:,2:min(16,m),:) = 0.
      case( KSYM_DIA ) ; a(ia)%GntDm(:,:,2:min(9,m),:) = 0.
      case( KSYM_HCP )
      case( KSYM_XROT ) ; a(ia)%GntDm(:,:,3:4,:) = 0.
      case( KSYM_REV )
      case default ! key not implemented
!         if(o>0) &
        write(o,'(3A,I0,9A)') sym, fun, 'unknown symmetry key = ', ksym
        stop 'symmetry: unknown key.'
      endselect ! ksym

    enddo ! ia
#endif
  endsubroutine symmetrize_radial_coefficients



!   subroutine symmetrize_radial_densities( a, me, ksym, ns )
!   use type_species, only: species, ELLMAX, I_SMT, I_TRU
!   use type_atom, only: atom
!     ! parameters
!     character(len=*), parameter     :: fun = ' symmetrize_radial_densities: '
!     ! args
!     type(atom), intent(inout)       :: a(:)
!     integer, intent(in)             :: me
!     integer, intent(in)             :: ksym ! key
!     integer, intent(in)             :: ns
! #ifdef USE_SYMMETRY
!     ! local vars
!     integer                         :: ia
! 
!     !     ==========  symmetry operations  ==========
!     do ia = 1, size(a) ; if( a(ia)%owner /= me ) cycle
! 
!     selectcase( ksym )
!     case( KSYM_NONE ) ! do nothing
!     case( KSYM_SC, KSYM_BCC, KSYM_FCC )
!       call symmetrize_radial_multipoles( a(ia), ns, &
!                                          ilm_nonzero=ILM_NONZERO_BCC_FCC )
!     case( KSYM_DIA )
!       call symmetrize_radial_multipoles( a(ia), ns, &
!                                          ilm_nonzero=ILM_NONZERO_DIA )
!     case( KSYM_HCP )
!       call symmetrize_radial_multipoles( a(ia), ns, &
!                                          ilm_nonzero=ILM_NONZERO_HCP )
!     case( KSYM_XROT ) ; a(ia)%rho(:,3:4,:,:) = 0.
!     case( KSYM_REV )
!     case default !
!       ! key not implemented
!       if(o>0) write(o,'(3A,I0)') sym, fun, 'unknown key ksym = ', ksym
! !       stop 'symmetry: unknown key.'
!     endselect ! ksym
!     enddo ! ia
! #endif
!   endsubroutine symmetrize_radial_densities




  subroutine symmetrize_density_matrix( a, ksym )
  use configuration, only: WARNING
  use type_atom, only: atom
#ifdef USE_SYMMETRY
  use spherical, only: fcc_rotation_matrix
#endif
  use MPIconst, only: Wtime
cDBG  use type_atom, only: atomic_population
    ! parameters
cDBG  character(len=*), parameter     :: fun = ' symmetrize_density_matrix: '
    ! args
    type(atom), intent(inout)       :: a(:)
    integer, intent(in)             :: ksym ! key
#ifdef USE_SYMMETRY
    ! local vars
    integer                   :: ia, n, i48, i1, i2, i3, i4
    integer                   :: ist
    real, allocatable         :: Dm(:,:), SDm(:,:), R1(:,:), R2(:,:)
    real                      :: rot(16,16,48) ! rotations
    real                      :: t(0:9) ! timing


    selectcase( ksym )
    ! ==========  symmetry operations  ==========
    case( KSYM_REV )

    case( KSYM_SC, KSYM_BCC, KSYM_FCC )
    ! ==========  fcc  ==========
cDBG  if(o>0) write(o,'(9A)') sym, fun, 'cubic'

      t(0) = Wtime()

      if( ksym == KSYM_FCC .and. size(a) /= 4 ) then
        if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'FCC only implemented with 4 atoms'
        return
      endif ! 4 atoms

      ist = fcc_rotation_matrix( rot )

   do ia = 1, size(a)
      n = size(a(ia)%Dm,1)
      allocate( Dm(n,n), SDm(n,n), R1(n,n), R2(n,n) )
!       if(o>0) write(o,'(5A,99I3)') sym, fun, 'species ', a(ia)%s%sym, ' ilm''s:', a(ia)%s%ind_ilm

#ifdef DEBUG
      ! write density matricies to fort.7
      if(o>0) then
          write(7,'(I2)') ia
          i3 = 1
        do i2 = 1, size(a(ia)%Dm,2)
          write(7,'(2F15.6,2F12.6,F15.6,4F12.6,F15.6,6F12.6,F15.6,8F12.6,F15.6,10F12.6)') a(ia)%Dm(1:i2,i2,1)
          if( any( (/1,4,9,16,25,36,49/) == i2 ) ) write(7,'(A)') ! blank line
          do i1 = i2, 1, -1
            if( abs( a(ia)%Dm(i1,i2,i3) ) > 1.E-9 ) then
              write( 9,'(A,2(I4,2I3),ES26.16,F16.9)') 'i1,l,m, i2,l,m = ', &
                 i1, a(ia)%s%ind_ell(i1), a(ia)%s%ind_emm(i1), &
                 i2, a(ia)%s%ind_ell(i2), a(ia)%s%ind_emm(i2), a(ia)%Dm(i1,i2,i3)
            endif
          enddo ! i1
        enddo ! i2
      endif ! o>0
#endif

      do i3 = lbound(a(ia)%Dm,3), ubound(a(ia)%Dm,3)
        SDm = 0. ! init
        Dm = a(ia)%Dm(:,:,i3) ! load
        do i48 = 1, 48
          R1 = 0.
          R2 = 0.
          do i2 = 1, n
            do i1 = 1, n
              if( a(ia)%s%ind_enn(i1) == a(ia)%s%ind_enn(i2) ) then
                R1(i1,i2) = rot( a(ia)%s%ind_ilm(i1), a(ia)%s%ind_ilm(i2), i48 )
                R2(i2,i1) = R1(i1,i2) ! transpose of R1
              endif
            enddo ! i1
          enddo ! i2
          SDm = SDm + matmul( matmul( R1, Dm ), R2 )
        enddo ! i48
        a(ia)%Dm(:,:,i3) = SDm/48. ! store

#ifdef DEBUG
        if( i3 == 1 ) then
          ! write density matricies to fort.13
          if(o>0) then
            do i2 = 1, size(a(ia)%Dm,2)
              do i1 = i2, 1, -1
                if( abs( a(ia)%Dm(i1,i2,i3) ) > 1.E-9 ) then
                  write(19,'(A,2(I4,2I3),ES26.16,F16.9)') 'i1,l,m, i2,l,m = ', &
                    i1, a(ia)%s%ind_ell(i1), a(ia)%s%ind_emm(i1), &
                    i2, a(ia)%s%ind_ell(i2), a(ia)%s%ind_emm(i2), SDm(i1,i2)/48., a(ia)%Dm(i1,i2,i3)
                endif
              enddo ! i1
            enddo ! i2
          endif ! o>0
        endif ! i3 == 1
#endif
      enddo ! i3


      deallocate( Dm, SDm, R1, R2 )
cDBG  if( ksym == KSYM_FCC ) call atomic_population( a(ia), 0, o )
    enddo ! ia

      t(1) = Wtime()
cDBG  if(o>0) write(o,'(3A,F0.3,9A)') sym, fun, 'needed ', t(1)-t(0), ' sec'
    ! ==========  end fcc  ==========
    case default ! nothing
    endselect ! ksym

#endif
  endsubroutine symmetrize_density_matrix







  subroutine symmetrize_radial_multipoles( a, ns, ilm_nonzero )
  use type_species, only: species, ELLMAX, I_SMT, I_TRU
  use type_atom, only: atom
    ! parameters
cDBG  character(len=*), parameter     :: fun = ' symmetrize_radial_multipoles: '
    ! args
    type(atom), intent(inout)       :: a
    integer, intent(in)             :: ns
                                                      ! indices of the non-vanishing
    integer, intent(in)             :: ilm_nonzero(:) ! lm-coefficients
#ifdef USE_SYMMETRY
    ! local vars
    integer                         :: iq, is, ilm, ip, ir, im
    integer                         :: nnonzero = 0


      nnonzero = size(ilm_nonzero,1)

      ! for the two quantities I_TRU and I_SMT
      do iq = I_TRU, I_SMT, I_SMT-I_TRU
        do is = 1, ns ! and for each spin

          im = 1
          ilm = 0
          do while( ilm < a%s%mlm .and. im <= nnonzero )
            ilm = ilm + 1

            if( ilm == ilm_nonzero(im) ) then
              im = im + 1
            else ! ilm == ilm_nonzero
              a%rho(:,ilm,is,iq) = 0.
            endif ! ilm == ilm_nonzero

          enddo ! while ... ilm and im

        enddo  ! is
      enddo  ! iq

#endif
  endsubroutine symmetrize_radial_multipoles





  subroutine symmetrize_cartesian_quantity_r3( ksym, d, name )
  use configuration, only: WARNING
  use MPIconst, only: Wtime
    ! parameters
cDBG  character(len=*), parameter     :: fun = ' symmetrize_cartesian_quantity: '
    real, parameter                 :: oneeighth  = 1./8.
    real, parameter                 :: onesixth   = 1./6.
    real, parameter                 :: onefourth  = 1./4.
    real, parameter                 :: onethird   = 1./3.
    real, parameter                 :: onehalf    = 1./2.
    ! args
    integer, intent(in)             :: ksym ! key
    real, intent(inout)             :: d(:,:,:) ! rank3 array
    character(len=*), intent(in), optional :: name
#ifdef USE_SYMMETRY
    ! local vars
    real, allocatable               :: w(:,:,:) ! work array
    integer                         :: ix, iy, iz
    integer                         :: nx, ny, nz
    integer                         :: mx, my, mz
    character(len=24)               :: qname
cDBG  real                            :: diff(0:2), temp, dif, t(0:3)


    qname = '.'
    if( present(name)) write(unit=qname,fmt='(3A)') ' to "', trim(name), '".'

    nx=size(d,1) ; ny=size(d,2) ; nz=size(d,3)
    mx=nx+1; my=ny+1; mz=nz+1

    ! work array
    allocate( w(nx,ny,nz) )

    !==========  symmetric operations  ==========
    selectcase( ksym )
    case( KSYM_REV )

    case( KSYM_NONE )
      ! do nothing
    case( KSYM_INV )
      ! Inversion symmetry
cDBG  if(o>0) write(o,'(9A)') sym, fun, 'apply inversion symmetry operation', trim(qname)
      do iz= 1, nz
        do iy= 1, ny
          do ix= 1, nx
            w(ix,iy,iz) = onehalf * (  d(ix,iy,iz) + d(mx-ix,my-iy,mz-iz) )
          enddo ! ix
        enddo ! iy
      enddo ! iz

      d = w

    case( KSYM_SC, KSYM_BCC, KSYM_FCC )
      ! sc, bcc, fc

      if( nx /= ny .or. ny /= nz .or. nz /= nx ) &
        stop 'SYM symmetrize_cartesian_quantity: fatal error: SC/BCC/FCC, but grid dimensions differ.'

cDBG  if(o>0) write(o,'(9A)') sym, fun, 'apply 48 symmetry operations', trim(qname)
cDBG  t(0) = Wtime() ! start

      do iz = 1, nz
        do iy = 1, ny
          do ix = 1, nx
            ! mirror operations
            w(ix,iy,iz) = oneeighth * ( &
              d(ix,   iy,   iz   ) + &      ! unity      (  1, 0, 0,   0, 1, 0,   0, 0, 1 )
              d(mx-ix,iy,   iz   ) + &      ! Mx         ( -1, 0, 0,   0, 1, 0,   0, 0, 1 )
              d(ix,   my-iy,iz   ) + &      !    My      (  1, 0, 0,   0,-1, 0,   0, 0, 1 )
              d(ix,   iy,   mz-iz) + &      !       Mz   (  1, 0, 0,   0, 1, 0,   0, 0,-1 )
              d(mx-ix,my-iy,iz   ) + &      ! Mx My      ( -1, 0, 0,   0,-1, 0,   0, 0, 1 )
              d(mx-ix,iy,   mz-iz) + &      ! Mx    Mz   ( -1, 0, 0,   0, 1, 0,   0, 0,-1 )
              d(ix,   my-iy,mz-iz) + &      !    My Mz   (  1, 0, 0,   0,-1, 0,   0, 0,-1 )
              d(mx-ix,my-iy,mz-iz) )        ! Mx My Mz   ( -1, 0, 0,   0,-1, 0,   0, 0,-1 )
          enddo ! ix
        enddo ! iy
      enddo ! iz

cDBG  diff = 0. ! init difference measurement
      do iz = 1, nz
        do iy = 1, ny
          do ix = 1, nx
            ! rotations
cDBG        temp = d(ix,iy,iz) ! store old value
            d(ix,iy,iz) = onesixth * ( & !!
                w(ix,iy,iz) & !! (  1, 0, 0,   0, 1, 0,   0, 0, 1 )
              + w(iy,iz,ix) & !! (  0, 0, 1,   1, 0, 0,   0, 1, 0 )
              + w(iz,ix,iy) & !! (  0, 1, 0,   0, 0, 1,   1, 0, 0 )
              + w(iz,iy,ix) & !! (  0, 0, 1,   0, 1, 0,   1, 0, 0 )
              + w(iy,ix,iz) & !! (  0, 1, 0,   1, 0, 0,   0, 0, 1 )
              + w(ix,iz,iy) ) !! (  1, 0, 0,   0, 0, 1,   0, 1, 0 )
cDBG        dif = abs( temp - d(ix,iy,iz) )
cDBG        diff(0) = max( diff(0), dif ) ! inf-norm
cDBG        diff(1) = diff(1) + dif       !   1-norm
cDBG        diff(2) = diff(2) + dif*dif   !   2-norm
          enddo ! ix
        enddo ! iy
      enddo ! iz
cDBG  diff(1:2) = diff(1:2)/real(nx*ny*nz)
cDBG  diff(2) = sqrt(diff(2))
cDBG  if(o>0) write(o,'(3A,3ES10.2,9A)') sym, fun, 'change-norm [max,||^1,^2] =', diff(0:2), ' for', trim(qname(4:))

cDBG  t(1) = Wtime() ! stop
cDBG  if(o>0) write(o,'(3A,F10.3,9A)') sym, fun, 'needed', t(1)-t(0), ' sec'

    case( KSYM_DIA )
      ! diamond structure

      if( nx/=ny .or. ny/=nz .or. nz/=nx ) &
        stop 'symmetrize_cartesian_quantity: fatal error: DIAMOND, but grid dimensions differ.'

cDBG  if(o>0) write(o,'(9A)') sym, fun, 'apply diamond symmetry operations', trim(qname)

      do iz = 1, nz
        do iy = 1, ny
          do ix = 1, nx
            w(ix,iy,iz) = onefourth * ( & !!
                d(   ix,   iy,   iz) & !! (  1, 0, 0,   0, 1, 0,   0, 0, 1 )
              + d(mx-ix,my-iy,   iz) & !! ( -1, 0, 0,   0,-1, 0,   0, 0, 1 )
              + d(   ix,my-iy,mz-iz) & !! (  1, 0, 0,   0,-1, 0,   0, 0,-1 )
              + d(mx-ix,   iy,mz-iz) ) !! ( -1, 0, 0,   0, 1, 0,   0, 0, 1 )
          enddo ! ix
        enddo ! iy
      enddo ! iz

      do iz = 1, nz
        do iy = 1, ny
          do ix = 1, nx
            d(ix,iy,iz) = onethird * ( &
                w(ix,iy,iz) & !! (  1, 0, 0,   0, 1, 0,   0, 0, 1 )
              + w(iy,iz,ix) & !! (  0, 0, 1,   1, 0, 0,   0, 1, 0 )
              + w(iz,ix,iy) ) !! (  1, 0, 0,   0, 1, 0,   0, 0, 1 )
          enddo ! ix
        enddo ! iy
      enddo ! iz

    case( KSYM_HCP )
      ! hexagonal closed package
cDBG  if(o>0) write(o,'(9A)') sym, fun, 'apply hcp symmetry operations', trim(qname)
      do iz= 1, nz
        do iy= 1, ny
          do ix= 1, nx
            w(ix,iy,iz) = onefourth *                     ( &
              d(   ix,   iy,   iz) + d(   ix,   iy,   iz) + &
              d(   ix,   iy,mz-iz) + d(mx-ix,   iy,mz-iz) )
          enddo ! ix
        enddo ! iy
      enddo ! iz

      d = w

    case( KSYM_HEX )
      ! hexagonal closed package
cDBG  if(o>0) write(o,'(9A)') sym, fun, 'apply hex symmetry operations', trim(qname)
      do iz= 1, nz
        do iy= 1, ny
          do ix= 1, nx
            w(ix,iy,iz) = onefourth *                     ( &
              d(   ix,   iy,   iz) + d(   ix,   iy,   iz) + &
              d(   ix,   iy,mz-iz) + d(mx-ix,   iy,mz-iz) )
          enddo ! ix
        enddo ! iy
      enddo ! iz

      d = w

    case( KSYM_XROT )
      ! rotational symmetry around x-axis

      do iz = 1, nz
        do iy = 1, ny
          ! mirror operations
          w(:,iy,iz) = onefourth *              ( &
            d(:,   iy,   iz) + d(:,my-iy,   iz) + &
            d(:,   iy,mz-iz) + d(:,my-iy,mz-iz) )
        enddo ! iy
      enddo ! iz

      do iz = 1, nz
        do iy = 1, ny
          ! rotation about pi/2
          d(:,iy,iz) = onehalf * ( w(:,iy,iz) + w(:,iz,my-iy) )
        enddo ! iy
      enddo ! iz

cDBG  if(o>0) write(o,'(9A)') sym, fun, 'apply 8 xrot symmetry operations', trim(qname)

    case( KSYM_NEW ) ; if(o>0) write(o,'(3A,I0)') sym, fun, 'not implemented: ksym = ', ksym
    case default ; if(o>0) write(o,'(3A,I0)') sym, fun, 'unknown key ksym = ', ksym
                   stop 'SYM symmetrize_cartesian_quantity: unknown key.' ! key not implemented
    endselect ! ksym

    deallocate( w ) ! free the work array memory
#endif
  endsubroutine symmetrize_cartesian_quantity_r3


  !! wrapper routine for the rank-3-routine
  subroutine symmetrize_cartesian_quantity_r4( ksym, d, name )
    ! parameters
cDBG  character(len=*), parameter     :: fun = ' symmetrize_cartesian_quantity_r4: '
    ! arguments
    integer, intent(in)             :: ksym ! key
    real, intent(inout)             :: d(:,:,:,:)
    character(len=*), intent(in), optional :: name
#ifdef USE_SYMMETRY
    ! local vars
    integer                         :: i4

    do i4 = 1, size( d, 4 )
      call symmetrize_cartesian_quantity_r3( ksym, d(:,:,:,i4), name )
    enddo ! i4
#endif
  endsubroutine symmetrize_cartesian_quantity_r4



  !! checks the symmetry of the setup
  logical function check_symmetry( g, a, ksym ) result( ok )
  use configuration, only: WARNING, ERROR
  use type_grid, only: grid, BC_PERIODIC
  use type_atom, only: atom
    ! parameter
cDBG  character(len=*), parameter           :: fun = ' check_symmetry: '
    integer, parameter                    :: M = 2**18
    ! arguments
    type(grid), intent(in)                :: g !! grid descriptor
    type(atom), intent(in)                :: a(1:) !! list of all atoms
    integer, intent(in)                   :: ksym !! assumed symmetry
#ifndef USE_SYMMETRY
    if( ksym > 0 .and. o>0) write(o,'(9A)') sym, fun, WARNING(0), 'cannot check symmetry, please compile with USE_SYMMETRY!'
    ok = .true.
#else
    integer, parameter                    :: pind(1-2:3+2) = (/2,3,1,2,3,1,2/) ! periodically continued index
    real, parameter                       :: EPS = 1E-9
    ! local vars
    logical                               :: allowed(48)
    integer                               :: ia, ja, na, iop, i6, i8, n, ii
    real                                  :: pos(3)
    integer                               :: ipos(3,4), dipos(3)!, jpos(3,4), ias(0:4), jas(0:4), dipos(3,4,4)
    logical                               :: center, same_cell(3), same_ngps(3)

    same_cell(1:3) = ( abs( g%s(pind(2:4)) - g%s(pind(3:5)) ) < EPS )
    same_ngps(1:3) = ( g%ng_all(pind(2:4)) == g%ng_all(pind(3:5)) )

    if( ksym /= KSYM_FCC ) then
      if( ksym > 0 .and. o>0) write(o,'(9A)') sym, fun, 'symmetry check only implemented for fcc!'
      ok = .true.
      return ! true
    endif

    ok = .false. ! result for early return


      if( any( g%bc /= BC_PERIODIC ) ) then
        if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'fcc-symmetry requires periodic boundary conditions!'
        return ! false
      endif

      if( .not. all( same_cell ) ) then
        if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'fcc-symmetry requires that the cell is cubic!'
        return ! false
      endif

      if( .not. all( same_ngps ) ) then
        if(o>0) write(o,'(4A,3I4)') sym, fun, WARNING(0), 'fcc-symmetry requires cubic grid point numbers, found', g%ng_all(1:3)
        return ! false
      endif

      if( any( mod( g%ng_all(1:3), 2 ) == 1 ) ) then
        if(o>0) write(o,'(4A,3I4)') sym, fun, WARNING(0), 'fcc-symmetry requires even grid point numbers, found', g%ng_all(1:3)
        return ! false
      endif


    na = size(a)
    if( na /= 4 ) then
      if(o>0) write(o,'(4A,I0,9A)') sym, fun, WARNING(0), 'fcc-symmetry requires 4 atoms, but found ', size(a), ' atoms!'
      return ! false
    endif

    ipos = 0 ! init
    do ia = 1, na
      if( a(ia)%s%iZ /= a(1)%s%iZ ) then
        if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'no fcc-symmetry, found different species ', a(ia)%s%sym, ' and ', a(1)%s%sym
        return ! false
      endif
      ipos(:,ia) = mod( nint( a(ia)%relpos(1:3) * 2*M ) + M, 2*M ) - M ! backfolding, if needed
      if( any( mod( ipos(:,ia), M ) /= 0 ) ) then
        if(o>0) write(o,'(4A,3I4)') sym, fun, WARNING(0), 'fcc-symmetry implementation allows fractions of 1:2 only!'
        return ! false
      endif
      do ja = 1, ia-1
        dipos = ipos(:,ja) - ipos(:,ia) ! translation vector
        dipos = mod( dipos + M, 2*M ) - M ! backfolding, if needed
        if( all( dipos == 0 ) ) then
          write(0,'(4A,9(I0,A))') sym, fun, ERROR, 'atom #',ia, ' is at the same position as atom #',ja
          stop 'SYM check_symmetry (fcc) two atoms are in the same position!'
          if(o>0) write(o,'(4A,9(I0,A))') sym, fun, WARNING(0), 'atom #',ia, ' is at the same position as atom #',ja
          return ! false
        endif
        if( count( abs( dipos ) == M ) /= 2 .or. count( dipos == 0 ) /= 1 ) then
          if(o>0) write(o,'(4A,3F7.3)') sym, fun, WARNING(0), &
            'fcc-symmetry requires position differences in the 6 permutations of ( 0 +-1/2 +-1/2 ), but found ', dipos/(2.*M)
          return ! false
        endif
#ifdef DEBUG
        if(o>0) write(o,'(2I2,3F10.6)') ia,ja, dipos/(2.*M)
#endif
      enddo ! ja
    enddo ! ia

    if(o>0) write(o,'(9A)') sym, fun, 'found fcc-symmetry according to the implementation.'
    ok = .true. ! fcc-symmetric
#endif
  endfunction check_symmetry


! 
! 
!   function check_atomic_fractional_positions( a, bc, at, exploit )
!   use type_grid, only: BC_FINITE, BC_PERIODIC
!   use type_atom, only: atom
!     ! parameter
!     character(len=*), parameter           :: fun = ' check_atomic_fractional_positions: '
!     integer, parameter                    :: NOPARTNER = 0
!     integer, parameter                    :: IAMMYPARTNER = -1
!     ! arguments
!     type(atom), intent(in)                :: a(:)
!     integer, intent(in)                   :: bc(1:3) ! boundary condition
!     type(atom), intent(out), allocatable  :: at(:)
!     logical, intent(in)                   :: exploit(1:3)
!     ! return value
!     logical                               :: check_atomic_fractional_positions(1:3)
!     ! local vars
!     integer                               :: ja1, natoms, ja2
!     real                                  :: origin(1:3)
!     logical                               :: s(1:3), sandr(1:3)
!     integer, allocatable                  :: apartner(:,:)
!     integer                               :: niammyp(1:3)
!     integer                               :: id
!     real                                  :: r1(1:3), r2(1:3)
!     integer                               :: matoms, Z
! 
!     check_atomic_fractional_positions = .false.
!     s = .false.
! 
!     natoms = size( a, 1 )
!     allocate( apartner(natoms,3) )
!     apartner = NOPARTNER
! 
!     do id = 1, 3 ! for all spatial directions
!       origin(id) = 0.5
!       if( bc(id) == BC_FINITE ) then
! 
!         niammyp(id) = 0
!         do ja1 = 1, natoms
!           Z = a(ja1)%s%Z
!           r1 = a(ja1)%relpos - origin
!           if( r1(id) == 0.0 ) then
!             apartner(ja1,id) = IAMMYPARTNER
!             niammyp(id) = niammyp(id) + 1
!           else ! r1(id) == 0
!             ! look for other atoms to be the partner
!             do ja2 = 1, ja1-1
!   !               if(o>0) write(o,'(3A,2(I4,I3,3F7.5))') sym, fun, 'ja1,Z1,r1 =', &
!   !                 ja1,Z,r1
!   !               if(o>0) write(o,'(3A,2(I4,I3,3F7.5))') sym, fun, 'ja2,Z2,r2 =', &
!   !                 ja2,a(ja2)%s%Z, r2
!               r2 = a(ja2)%relpos - origin
!               ! the opposite sign
!               r2(id) = - r2(id)
!               if( all( r1 == r2 ) .and. a(ja2)%s%Z == Z ) then
!                 apartner(ja1,id) = ja2
!                 ! partnership is a symmetric relation
!                 apartner(ja2,id) = ja1
!               endif ! r1 == r2
!             enddo ! ja2
!           endif ! r1(id) == 0
!         enddo ! ja1
!         if(o>0) write(o,'(3A,100I4)') sym, fun, 'partnerlist =', apartner(:,id)
!       elseif( bc(id) == BC_PERIODIC ) then
!         stop 'symmetry: only finite BC implemented.'
!       else ! bc == ...
!         stop 'symmetry: only finite and periodic BC implemented.'
!       endif ! bc == ...
! 
!       ! if no atom is left without a partner, the symmetry is true
!       check_atomic_fractional_positions(id) = all( apartner(:,id) /= NOPARTNER )
! 
!     enddo ! id
! 
!     ! do it if it is possible and required
!     s = exploit .and. check_atomic_fractional_positions
! 
!     if(o>0) write(o,'(A,3(A,3L2),9A)') sym, &
!       ' symmetry given [', check_atomic_fractional_positions, &
!       ' ] required, [', exploit, ' ] exploit [', s, ' ].'
! 
!     if( any(s) ) then
!       matoms = 0
!       do ja1 = 1, natoms
!         r1 = a(ja1)%relpos - origin
!         sandr = s .and. ( r1 <= 0.0 )
!         if( any( sandr ) ) matoms = matoms + 1
!       enddo ! ja1
!       if( matoms < 1 ) stop 'symmetry: fatal error counting'
!       if( allocated( at ) ) deallocate( at )
!       allocate( at(matoms) )
!       ja2 = 0
!       do ja1 = 1, natoms
!         r1 = a(ja1)%relpos - origin
!         sandr = s .and. ( r1 <= 0.0 )
!         if( any( sandr ) ) then
!           ja2 = ja2 + 1
!           ! copy the atoms wich are relpos(id) <=  0.5
!           at(ja2) = a(ja1)
!         endif ! all
!         if(o>0) write(o,'(3A,I5,A,3F7.5)') sym, fun, 'atom j#', ja1, ' pos =', r1
!       enddo ! ja1
!     endif ! any exploit
! 
!   return
!   endfunction check_atomic_fractional_positions
! 
! 
! 

  integer function generate_kpoint_set( n, kpt, shift, ksymm ) result( ist )
    ! parameter
cDBG  character(len=*), parameter           :: fun = ' generate_kpoint_set: '
    ! arguments
    integer, intent(in)                   :: n(3)
    real, allocatable, intent(out)        :: kpt(:,:)
    real, intent(in), optional            :: shift(3)
    integer, intent(in), optional         :: ksymm
    ! local vars
    integer, allocatable                  :: mmrot(:,:,:)
    integer, allocatable                  :: ikp(:,:)
    integer                               :: key, nop, nk(1:3), ik, nkp
    real                                  :: d(0:3), s(3)

    if( allocated( kpt ) ) deallocate( kpt, stat=ist )

    s = 0. ; if( present( shift ) ) s = mod( shift - 0.5, 1.0 ) + 0.5
    if( any( abs( s ) > 1E-9 ) ) then
      ! generate mesh without symmetry reduction
      ist = kmesh_generation( n, s, kpt )
      return
    endif ! shifts nonzero

    nk(1:3) = max( 1, abs(n) )

    key = KSYM_SC ; if( present( ksymm ) ) key = ksymm

    selectcase( key )
    case( KSYM_NONE )
      nop = 1
      allocate( mmrot(3,3,nop) )
      mmrot = reshape( (/ &
            1,  0,  0,  0,  1,  0,  0,  0,  1 & !   E
          /), (/3,3,nop/) ) ! reshape to a 3x3 matrix
    case( KSYM_INV )
      nop = 2
      allocate( mmrot(3,3,nop) )
      mmrot = reshape( (/ &
            1,  0,  0,  0,  1,  0,  0,  0,  1, & !   E
           -1,  0,  0,  0, -1,  0,  0,  0, -1  & !   I
          /), (/3,3,nop/) ) ! reshape to 2 3x3 matrices
    case( KSYM_DIA )
      nop = 12
      allocate( mmrot(3,3,nop) )
      mmrot = reshape( (/ &
            1,  0,  0,  0,  1,  0,  0,  0,  1 ,& !   E
            0,  1,  0,  1,  0,  0,  0,  0,  1 ,& !
           -1,  0,  0,  0, -1,  0,  0,  0, -1 ,& !   I
            0, -1,  0, -1,  0,  0,  0,  0, -1 ,& !
           -1,  0,  0,  0,  0, -1,  0, -1,  0 ,& !
            0, -1,  0,  0,  0, -1, -1,  0,  0 ,& !
            0,  0, -1, -1,  0,  0,  0, -1,  0 ,& !
            0,  0, -1,  0, -1,  0, -1,  0,  0 ,& !
            0,  0,  1,  0,  1,  0,  1,  0,  0 ,& !
            0,  0,  1,  1,  0,  0,  0,  1,  0 ,& !
            0,  1,  0,  0,  0,  1,  1,  0,  0 ,& !
            1,  0,  0,  0,  0,  1,  0,  1,  0  &
          /), (/3,3,nop/) ) ! reshape to 12 3x3 matrices
     case( KSYM_REV )
      nop = 2
      allocate( mmrot(3,3,nop) )
      mmrot = reshape( (/ &
            1,  0,  0,  0,  1,  0,  0,  0,  1 ,& !   E
           -1,  0,  0,  0, -1,  0,  0,  0, -1  & !   I
          /), (/3,3,nop/) ) ! reshape to 2 3x3 matrices
   case( KSYM_SC, KSYM_BCC, KSYM_FCC )
      nop = 48
      allocate( mmrot(3,3,nop) )
      mmrot = MROT48
!     character(len=3), parameter :: sname(NOP) = &
!         (/ &
!           'E  ', 'm  ', 'c_2', 'm  ', 'c_2', 'm  ',  &
!           'I  ', 'c_2', 'm  ', 'c_3', 'c_2', 'm  ',  &
!           'c_2', 'c_3', 'm  ', 'm  ', 'c_2', 'm  ',  &
!           'm  ', 'c_3', 'm  ', 'c_2', 'c_4', 'm  ',  &
!           'm  ', 'c_2', 'c_3', 'm  ', 'm  ', 'c_4',  &
!           'm  ', 'c_3', 'm  ', 'c_4', 'm  ', 'c_2',  &
!           'm  ', 'c_3', 'c_4', 'm  ', 'm  ', 'c_3',  &
!           'c_4', 'm  ', 'c_3', 'm  ', 'm  ', 'c_4' & /)
    case( KSYM_XROT )
    case default ; stop 'generate_kpoints_set: symmetry key unknown'
    endselect ! key

    nkp = 0
    ! will allocate ikp
    ist = kpoint_symmetry_reduction( nk, mmrot, ikp )
    if( ist /= 0 ) return

    nkp = ubound( ikp, 2 )
    allocate( kpt(0:3,1:nkp), stat=ist )
    if( ist /= 0 ) return

    d = real( ikp(0:3,0) )
    do ik = 1, nkp
      kpt(1:3,ik) = ikp(1:3,ik) / d(1:3) ! in the interval (-0.5,0.5]
      kpt(0,ik) = real( ikp(0,ik) )  ! weights are integer
    enddo ! ik

    deallocate( ikp )

  endfunction generate_kpoint_set



  integer function kmesh_generation( nk, sh, kp ) result( ist )
    ! parameter
cDBG  character(len=*), parameter     :: fun = ' kmesh_generation: '
    ! arguments
    integer, intent(in)             :: nk(3)
    real, intent(in)                :: sh(3) ! shifts
    real, allocatable, intent(out)  :: kp(:,:) ! (0:3,nkpoints)
    ! local vars
    integer :: n(3), nkp, ikp, i1, i2, i3
    real    :: s(3)

    s = mod( sh - 0.5, 1.0 ) + 0.5 ! restrict to the range (-0.5,+0.5]
    n = max( 1, nk )
    where( nk < 1 ) s = 0.
    nkp = product( n )
    allocate( kp(0:3,nkp), stat=ist ) ; if( ist /= 0 ) return
    ikp = 0
    do i3 = 0, n(3)-1
      do i2 = 0, n(2)-1
        do i1 = 0, n(1)-1
          ikp = ikp+1
          kp(0,ikp) = 1./nkp ! weight
          kp(1:3,ikp) = ( (/ i1, i2, i3 /) + s )/n
          where( kp(1:3,ikp) > 0.5 ) kp(1:3,ikp) = kp(1:3,ikp) - 1.0 ! restrict to range [-0.5,0.5)
cDBG      if(o>0) write(o,'(3A,F0.9)') sym, fun, 'kp-length  ', sqrt( sum( kp(1:3,ikp)**2 ) )
        enddo ! i1
      enddo ! i2
    enddo ! i3

  endfunction kmesh_generation



  integer function kpoint_symmetry_reduction( nk, mmrot, kpred, reductionratio ) result( ist )
    ! parameter
cDBG  character(len=*), parameter         :: fun = ' kpoint_symmetry_reduction: '
    ! arguments
    integer, intent(in)                   :: nk(3)
    integer, intent(in)                   :: mmrot(:,:,:) ! (3,3,nsym)
    integer, allocatable, intent(out)     :: kpred(:,:) ! (0:3,nirreducible)
    real, intent(out), optional           :: reductionratio
    ! local vars
    integer                         :: nk2(1:3), im(1:3)
    integer                         :: nkall = 1
    integer, allocatable            :: kpall(:,:) ! (0:3, product(nk(1:3)) )
    integer                         :: i1, i2, i3, ik, ig, is
    integer                         :: kp(1:3)
    integer                         :: kps(1:3)
    logical                         :: irred
    integer                         :: nirred, wsum, nsym
    real                            :: rratio ! reductionratio

    nsym = size( mmrot, 3 )
cDBG  if( size(mmrot,1) /= 3 ) stop 'kpoint_symmetry_reduction: needs 3x3 integer symmetry operations.'
cDBG  if( size(mmrot,2) /= 3 ) stop 'kpoint_symmetry_reduction: needs 3x3 integer symmetry operations.'

    nk2(1:3) = nk/2
    im(1:3) = modulo(nk,2)+1
    nkall = product( nk(1:3) )
    allocate( kpall(0:3,nkall), stat=ist ) ; if( ist /= 0 ) return
    kpall = 0
    nirred = 0

cDBG  if(o>0) write(o,'(2A,3(I3,A))') sym, fun, nk(1), ' x', nk(2), ' x', nk(3), ' mesh'
cDBG  if(o>0) write(o,'(3A,I6,A)')    sym, fun, 'use', nsym, ' symmetry operations'

    ik = 0
    do i3 = nk(3), 1, -1
      do i2 = nk(2), 1, -1
        do i1 = nk(1), 1, -1
        !-------------------------------------
          ik = ik+1
          kp(1:3) = (/i1,i2,i3/) * 2 - 2*nk2 - im

          kpall(1:3,ik) = kp(1:3)
#ifdef FULL_DEBUG
!   write(o,'(A,I0,A,3I4,I8)') 'kp #', ik, '   ', kp !kpall(1:3,ik), kpall(0,ik)
#endif
          irred = .true. ! by default irreducible is assumed
          ! compare to all previous
          ig = ik-1
          do while( irred .and. ig > 0 )
            if( kpall( 0 ,ig) > 0 ) then
              is = 1
              do while( is <= nsym )
                kps = matmul( mmrot(:,:,is), kpall(1:3,ig) )
                if( all( kps == kp ) ) then
#ifdef FULL_DEBUG
  write(o,'(2(A,I0),2(" ",I0),9(A,I0))') 'kp #', ik, ' is  reducible  [', kpall(1:3,ik), ' ] ==> kp #', ig, ' by symmetry #', is
#endif
                  ! tranfer weight to ig
                  kpall(0,ig) = kpall(0,ig) + 1
                  kpall(0,ik) = 0
                  irred = .false.
                  is = nsym+1 ! exit the loop
                endif ! R * k = k''
                is = is+1
              enddo ! while is <= 48
            endif ! weight(ig) > 0
            ig = ig-1
          enddo ! while irred

          if( irred ) then
            nirred = nirred+1
            kpall( 0 ,ik) = 1
#ifdef FULL_DEBUG
  write(o,'(A,I0,A,3I4,A)') 'kp #', ik, ' is irreducible [', kpall(1:3,ik), ' ]'
#endif
          endif ! irred
        !-------------------------------------
        enddo ! i1
      enddo ! i2
    enddo ! i3

cDBG  if( ik /= nkall ) stop 'kpoint_symmetry_reduction: fatal counting error'
cDBG  wsum = sum(kpall(0,:))
cDBG  if( wsum /= nkall ) stop 'kpoint_symmetry_reduction: fatal error: weights are not conserved. (1)'
cDBG  if(o>0) write(o,'(3A,I6)') sym, fun, 'number of irreducible kpoints', nirred

    deallocate( kpred, stat=ist )
    allocate( kpred(0:3,0:nirred), stat=ist ) ; if( ist /= 0 ) return ! kpred(:,0) are the denominators
    kpred = 0
    ! denominators
    kpred( 0 ,0) = nkall
    kpred(1:3,0) = 2*nk(1:3)

    ig = 0
    wsum = 0
    do ik = nkall, 1, -1
      if( kpall(0,ik) > 0 ) then
        ig = ig+1
        kpred(0:3,ig) = kpall(0:3,ik) ! copy
        wsum = wsum + kpred(0,ig) ! sum up weights
      endif ! weight > 0
    enddo ! ik
cDBG  if( ig /= nirred ) stop 'kpoint_symmetry_reduction: fatal counting error (irreducible)'
cDBG  if( wsum /= nkall ) stop 'kpoint_symmetry_reduction: fatal error: weights are not conserved.'

#ifdef DEBUG
    if(o>0) then ! display
      write(o,'(A)') '----------------------------------------'
      write(o,'(A)') ' reduced k-point set'
      write(o,'(A)') '----------------------------------------'
      do ig = 1, nirred
        write(o,'(3I6,I12)') kpred(1:3,ig), kpred(0,ig)
      enddo ! ig
      write(o,'(A)') '----------------------------------------'
        write(o,'(3I6,I12)') kpred(1:3, 0), kpred(0, 0)
      write(o,'(A)') '----------------------------------------'
    endif ! o>0
#endif
cDBG  if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'found ', nirred, ' irreducible kpoints.'

    rratio = real(nirred)/real(nkall) ! reductionratio
    if( present( reductionratio ) ) then
      reductionratio = rratio ! export the value instead of displaying it
    elseif( nirred < nkall ) then  ! present reductionratio
      if(o>0) write(o,'(3A,4(I0,A),F0.1)') sym, fun, 'k-point mesh ',nk(1),' x ',nk(2),' x ',nk(3),' with ', nsym,' symmetry ops ==> reduction by factor ',1./rratio
    else  ! nirred < nkall
      if(o>0) write(o,'(3A,4(I0,A),F0.1)') sym, fun, 'k-point mesh ',nk(1),' x ',nk(2),' x ',nk(3),' with ', nsym,' symmetry ops ==> no reduction'
    endif ! present reductionratio

  endfunction kpoint_symmetry_reduction




!   subroutine test( )
!     ! parameter
!     character(len=*), parameter           :: fun = ' test: '
!     ! arguments
! 
!     ! local vars
! !     integer                               :: ui(3,3,2) = 0
! !     integer                               :: rot(3,3,3) = 0
! !     integer                               :: mir(3,3,3) = 0
! 
! !     call generate_symmetries( )
! 
!   endsubroutine test



!   subroutine generate_symmetries(  )
!     ! parameter
!     character(len=*), parameter           :: fun = ' generate_symmetries: '
!     integer, parameter                    :: N33m1 = 3**3-1
! !     integer, parameter                    :: No(3) = (/0,-1,1/)
!     integer, parameter                    :: No(3,3) = &
!       reshape( (/  0,-1,1,   0,-1,1,   0,-1,1  /), (/3,3/) )
!     ! arguments
! 
!     ! local vars
!     integer                               :: iop, i3, i2, i1, ii, i03, nrm, ia
!     integer                               :: op(3,3), v(3), vlist(3,0:N33m1), d, t, k
! !     integer                               :: all_op(3,3,0:N33m1,0:N33m1,0:N33m1)
!     integer                               :: icnt(-6:+6) = 0
! 
!     ia = 0
!     do i03 = 0, 3
! 
!       ii = 0
!       do i1 = 1, 3
!         v(1) = No(i1,1)
!         do i2 = 1, 3
!           v(2) = No(i2,2)
!           do i3 = 1, 3
!             v(3) = No(i3,3)
!             v = (/i1,i2,i3/)-2
!             nrm = sum(v*v)
!             if( nrm == i03 ) then
!               vlist(:,ia) = v
!               write(*,'(2I3,A,3I3,A,I3)') ia, ii, '  ', v, '       |v|=', nrm
!               ii = ii+1
!               ia = ia+1
!             endif ! nrm == i03
!           enddo ! i3
!         enddo ! i2
!       enddo ! i1
! 
!     enddo ! io3
! 
!     do i1 = 1, 6 !0, N33m1
!       op(:,1) = vlist(:,i1)
!       do i2 = 1, 6 !0, N33m1
!         op(:,2) = vlist(:,i2)
!         do i3 = 1, 6 !0, N33m1
!           op(:,3) = vlist(:,i3)
! 
!           d = det( op )
!           if( d == 1 ) then
!             t = tr( op )
!             k = key( d, t )
! 
!             if( k == 2 .and. sum( op*op ) == 3 ) then
!             write(10+k,'(A)') ''
!             write(10+k,'(3I3)') op(:,:)
!             endif !
! 
!             if( k == 3 .and. sum( op*op ) == 3 ) then
!             write(10+k,'(A)') ''
!             write(10+k,'(3I3)') op(:,:)
!             endif !
! 
!             if( k == 4 .and. sum( op*op ) == 3 ) then
!             write(10+k,'(A)') ''
!             write(10+k,'(3I3)') op(:,:)
!             endif !
! 
! 
!             icnt(k) = icnt(k) + 1
! 
!           endif ! |d| == 1
!         enddo
!       enddo
!     enddo !
! 
!     do k = -6, 6
!       write(*,'(I3,A,I8)') k, ' =det, count=', icnt(k)
!     enddo ! d
! 
! !     integer                               :: ui(3,3,2) = 0
! !     integer                               :: rot(3,3,3) = 0
! !     integer                               :: mir(3,3,3) = 0
! 
! !       write(*,'(I3,A,I8)') det( reshape( (/1,0,0,0,1,0,0,0,1/), (/3,3/) ) ) '== 1 ?'
! 
!   return
!   endsubroutine generate_symmetries


  integer function det( m )
    integer, intent(in)       :: m(3,3)
    det =     m(1,1)*m(2,2)*m(3,3)  &
            + m(1,2)*m(2,3)*m(3,1)  &
            + m(1,3)*m(2,1)*m(3,2)  &
            - m(1,1)*m(2,3)*m(3,2)  &
            - m(1,2)*m(2,1)*m(3,3)  &
            - m(1,3)*m(2,2)*m(3,1)
  endfunction

  integer function tr( m )
    integer, intent(in)       :: m(3,3)
    tr = m(1,1)+m(2,2)+m(3,3)
  endfunction

  integer function key( d, t )
    integer, parameter        :: cops(-1:3)=(/ 2, 3, 4, 6, 1 /)
    integer, intent(in)       :: d, t
    selectcase( d*t )
    case( -1:3 )
      key = d * cops( d*t )
    case default
      key = 0 ! no valid symmetry operation
    endselect ! dt
  endfunction

#ifdef NEW_SYMMETRIZE
!+ new_symmetrize

  !! the advantage of this parallelization is that
  !! it can be parrallelized in domain decomposition (to be done)
  subroutine symmetrize_fcc( g, ksym, d, a )
  use configuration, only: WARNING, ERROR
  use type_grid, only: grid
  use type_atom, only: atom
  use type_atom, only: atomic_population
  use MPIconst, only: Wtime
  use spherical, only: fcc_rotation_matrix
    ! parameters
    character(len=*), parameter     :: fun = ' symmetrize_fcc: '
    ! arguments
    type(grid), intent(in)    :: g !! grid descriptor
    integer, intent(in)       :: ksym
    real, intent(inout)       :: d(:,:,:,:)
    type(atom), intent(inout), optional :: a(:)
    ! module vars
    ! integer, allocatable    :: ifcc(:,:,:)
#ifdef USE_SYMMETRY
    ! local vars
    integer                   :: n, ni, ii, i1, i2, i3, i4
    real, allocatable         :: dirred(:), wirred(:)
    integer                   :: ia, i48
    real, allocatable         :: Dm(:,:), SDm(:,:), R1(:,:), R2(:,:)
    real                      :: rot(16,16,48) ! rotations
    real                      :: t(0:9) ! timing
cDBG  real                    :: diff

cDBG  if(o>0) write(o,'(4A,9(I8,A))') sym, fun, 'start!'
    if( ksym /= KSYM_FCC ) return

    ! check requirements for FCC symmetry
    if( g%ng_all(1) /= g%ng_all(2) .or. g%ng_all(2) /= g%ng_all(3) ) then
      if(o>0) write(o,'(4A,9(I8,A))') sym, fun, WARNING(0), 'all directions must be same for FCC.'
      return
    endif ! #grid points must be the same in each direction

    n = g%ng_all(1)/2
    if( any( g%ng_all(1:3) /= 2*n ) ) then
      if(o>0) write(o,'(4A,9(I8,A))') sym, fun, WARNING(0), 'even number of grid points required for FCC'
      return
    endif ! #grid points is not even

    if( any( g%ng_all(1:3) /= g%ng(1:3) ) ) then
      if(o>0) write(o,'(4A,9(I8,A))') sym, fun, WARNING(0), 'not parallelized in domain decomposition!'
      return
    endif ! domain decomposition


    t(0) = Wtime()


    call prepare_fcc_irreducible( n )

    ni = n*(n+1)*(n+2)/6 ! is always integer divisible
    allocate( dirred(ni), wirred(ni) )

    t(1) = Wtime()

    do i4 = 1, size(d,4)

      dirred = 0. ! init
      do i3 = 1, g%ng(3)
        do i2 = 1, g%ng(2)
          do i1 = 1, g%ng(1)
            ii = ifcc(i1,i2,i3)
cDBG        if( ii < 1 .or. ii > ni ) stop 'SYM symmetrize_fcc: IFCC contains wrong indices.'
            dirred( ii ) = dirred( ii ) + d(i1,i2,i3,i4) ! sum
            wirred( ii ) = wirred( ii ) + 1. ! weight
          enddo ! i1
        enddo ! i2
      enddo ! i3
      ! in parallel, perform an Allreduce of dirred and wirred here
      ! however, wirred can be known from the generation of ifcc
cDBG  diff = 0.
      do i3 = 1, g%ng(3)
        do i2 = 1, g%ng(2)
          do i1 = 1, g%ng(1)
            ii = ifcc(i1,i2,i3)
cDBG        if( wirred( ii ) <= 0. ) stop 'SYM symmetrize_fcc: IFCC weights wrong.'
cDBG        diff = diff + abs( d(i1,i2,i3,i4) - dirred( ii )/wirred( ii ) )
cDBG        if( ksym == KSYM_FCC ) &
            d(i1,i2,i3,i4) = dirred( ii )/wirred( ii )
          enddo ! i1
        enddo ! i2
      enddo ! i3
      ! now d(:,:,:,i4) is symmetrized
      ! allreduce diff
cDBG  if(o>0) write(o,'(3A,I2,F24.16)') sym, fun, 'difference for i4=', i4, diff/product(g%ng_all(1:3))
    enddo ! i4

    t(2) = Wtime()

    if( .not. present(a) ) return

    if( size(a) /= 4 ) then
      if(o>0) write(o,'(4A,9(I8,A))') sym, fun, WARNING(0), 'FCC only implemented with 4 atoms'
      return
    endif ! 4 atoms

    call fcc_rotation_matrix( rot )

    do ia = 1, size(a)
cDBG  if( ubound(a(ia)%Dm,3) /= size(d,4) ) stop 'SYM symmetrize_fcc: Spin index of a%Dm should match dim#4 of D.'
      n = size(a(ia)%Dm,1)
      allocate( Dm(n,n), SDm(n,n), R1(n,n), R2(n,n) )

#ifdef DEBUG
      ! write density matricies to fort.7
      if(o>0) then
          write(7,'(I2)') ia
          i3 = 1
        do i2 = 1, size(a(ia)%Dm,2)
          write(7,'(2F15.6,2F12.6,F15.6,4F12.6,F15.6,6F12.6,F15.6,8F12.6,F15.6,10F12.6)') a(ia)%Dm(1:i2,i2,1)
          if( any( (/1,4,9,16,25,36,49/) == i2 ) ) write(7,'(A)') ! blank line
          do i1 = i2, 1, -1
            if( abs( a(ia)%Dm(i1,i2,i3) ) > 1.E-9 ) then
              write( 9,'(A,2(I4,2I3),2F16.9)') 'i1,l,m, i2,l,m = ', &
                 i1, a(ia)%s%ind_ell(i1), a(ia)%s%ind_emm(i1), &
                 i2, a(ia)%s%ind_ell(i2), a(ia)%s%ind_emm(i2), a(ia)%Dm(i1,i2,i3)
            endif
          enddo ! i1
        enddo ! i2
      endif ! o>0
#endif

      do i3 = lbound(a(ia)%Dm,3), ubound(a(ia)%Dm,3)
        SDm = 0. ! init
        Dm = a(ia)%Dm(:,:,i3) ! load
        do i48 = 1, 48
          do i2 = 1, n
!             do i1 = 1, n
!               R1(i1,i2) = rot( a(ia)%s%ind_ilm(i1), a(ia)%s%ind_ilm(i2), i48 )
!               R2(i2,i1) = rot( a(ia)%s%ind_ilm(i1), a(ia)%s%ind_ilm(i2), i48 )
!             enddo ! i1
            R1(:,i2) = rot( a(ia)%s%ind_ilm, a(ia)%s%ind_ilm(i2), i48 )
            R2(i2,:) = R1(:,i2) ! transpose
          enddo ! i2
          SDm = SDm + matmul( matmul( R1, Dm ), R2 )
        enddo ! i48
! cDBG    if( ksym == KSYM_FCC ) &
        a(ia)%Dm(:,:,i3) = SDm/48. ! store


#ifdef DEBUG
    if( i3 == 1 ) then
      ! write density matricies to fort.13
      if(o>0) then
        do i2 = 1, size(a(ia)%Dm,2)
          do i1 = i2, 1, -1
            if( abs( a(ia)%Dm(i1,i2,i3) ) > 1.E-9 ) then
              write(19,'(A,2(I4,2I3),2F16.9)') 'i1,l,m, i2,l,m = ', &
                 i1, a(ia)%s%ind_ell(i1), a(ia)%s%ind_emm(i1), &
                 i2, a(ia)%s%ind_ell(i2), a(ia)%s%ind_emm(i2), SDm(i1,i2)/48., a(ia)%Dm(i1,i2,i3)
            endif
          enddo ! i1
        enddo ! i2
      endif ! o>0
    endif ! i3 == 1
#endif

      enddo ! i3


      deallocate( Dm, SDm, R1, R2 )
    enddo ! ia = 1, size(a)

cDBG  if( ksym /= KSYM_FCC ) return
! cDBG    call atomic_population( a(1), 0, o )

    if( ksym == KSYM_FCC ) then

      do ia = 2, 4
        a(1)%Dm = a(1)%Dm + a(ia)%Dm ! copy
      enddo ! ia
      do ia = 4, 1, -1
        a(ia)%Dm = a(1)%Dm/4.
      enddo ! ia

    endif ! fcc

    t(3) = Wtime()
    if(o>0) write(o,'(3A,4F10.3,9A)') sym, fun, 'needed (...,tot)', t(1:3)-t(0:2), t(3)-t(0), ' sec'

#endif
  endsubroutine ! symmetrize_fcc


  !! the advantage of this parallelization is that
  !! it can be parrallelized in domain decomposition (to be done)
  subroutine prepare_fcc_irreducible( n )
  use configuration, only: WARNING, ERROR
  use MPIconst, only: Wtime
    ! parameters
    character(len=*), parameter :: fun = ' ifcc: '
    ! arguments
    integer, intent(in) :: n !! half the number of all grid points in one direction
    ! module vars
    ! integer, allocatable :: ifcc(:,:,:)

#ifdef USE_SYMMETRY
!+ use_symmetry

    ! local vars
    integer             :: i1, i2, i3
    integer             :: ix, iy, iz
    logical, parameter  :: CREATE_INDEX_LIST = .true.
    integer             :: f1, f2, ws = 0
    integer             :: w(2*n,2*n,2*n)
    integer             :: ni, m, i, t
    real                :: tm(0:9)
#ifdef DEBUG
    integer :: i01
    do i01 = 0, 1
      ! loop over the routine twice:
      ! i01 == 0: only in DEBUG mode, check if the weights are normalized correctly
#else
    integer, parameter :: i01 = 1
#endif
      ! i01 == 1:
      ! write index of the irreducible element into ifcc and create the
      ! symmetrization index array such that for the symmetrization of a(2*n,2*n,2*n)
      ! create an temp array real :: airred(1:ni)
      ! airred = 0. ! init
      ! airred( ifcc ) = airred( ifcc ) + a !! Caution! beware race conditions in OpenMP
      ! a = airred( ifcc )
      ! ==> now a is symmetrized

      if( n > 72 ) stop 'SYM ifcc: n > 72 (144 grid points) will cause a segmentation fault.'

      tm(0) = Wtime() ! start time

!       if( allocated( ifcc ) ) deallocate( ifcc, stat=i )
!       allocate( ifcc(2*n,2*n,2*n) )

      if( all( shape( ifcc ) == 2*n ) ) return ! already generated

      if( any( shape( ifcc ) /= 2*n ) ) then
        deallocate( ifcc, stat=i )
! cDBG    if(o>0) write(o,'(3A,F16.6,9A)') sym, fun, 'try to allocate', (2*n)**3*4*2.**(-20), ' MiByte'
        allocate( ifcc(2*n,2*n,2*n), stat=i )
        if( i/=0 ) stop 'SYM ifcc: allocation failed!'
      endif ! shape

      ifcc = 0 ! init index array
      ws = 0 ! init weight sum
      ni = 0 ! init counter for irreducible elements

      tm(1) = Wtime() ! start wedge

      ! prepare the irreducible wedge
      do i3 = 1, n
        do i2 = i3, n
          do i1 = i2, n

            if( i1 == i2 ) then
              f1 = 4 ! face weight
              f2 = 6 ! volume weight
              if( i2 == i3 ) f2 = 2 ! edge weight on space diagonal
            else  ! i1 == i2
              f1 = 8 ! volume weight
              f2 = 6 ! volume weight
              if( i2 == i3 ) f2 = 3 ! face weight
            endif ! i1 == i2
            ! f1 * f2 can assume values 48,24,16,12,8

            ni = ni+1 ! count irreducible elements
            ws = ws + f1 * f2 ! collect weights

            ! create index list
            ifcc(i1,i2,i3) = f1 * f2 * ni
            ! check weight normalization in 1st iteration (i01==0)
cDBG        if( i01 == 0 ) ifcc(i1,i2,i3) = f1 * f2 * 1
          enddo ! i1
        enddo ! i2
      enddo ! i3


      ! check weights
      if( ws /= (2*n)**3 ) then
        if(o>0) write(o,'(4A,9(I8,A))') sym, fun, ERROR, 'weight of irred. entries =', ws, ' does not match volume', (2*N)**3
      endif

cDBG  if(o>0) write(o,'(3A,I4,A,I9)') sym, fun, 'for N =', N, ' number of irreducible elements is', ni
      ! ==> ni = n*(n+1)*(n+2)/6
      if( ni /= n*(n+1)*(n+2)/6 ) then
        if(o>0) write(o,'(4A,9(I8,A))') sym, fun, ERROR, 'number of irred. entries =', ni, ' does not match prediction', n*(n+1)*(n+2)/6
      endif

      if( i01 == 1 ) then
        ! the number of irreducible elements converges towards 1/48 for large n
        if(o>0) write(o,'(3A,I4,F8.5,9A)') sym, fun, 'for N =', 2*n, (1./48.) / (ni/real(2*n)**3) * 100., ' % efficiency'
      endif ! i01

      ! number of gridpoints = 2*n
      m = 2*n+1 ! for inversion: R --> -R whereas i --> m-i

      tm(2) = Wtime() ! start mirror ops

      do i3 = 1, 2*n
        do i2 = 1, 2*n
          do i1 = 1, 2*n
            ! mirror operations
            t = ( ifcc(i1,  i2,  i3  ) &      ! unity      (  1, 0, 0,   0, 1, 0,   0, 0, 1 )
                + ifcc(m-i1,i2,  i3  ) &      ! Mx         ( -1, 0, 0,   0, 1, 0,   0, 0, 1 )
                + ifcc(i1,  m-i2,i3  ) &      !    My      (  1, 0, 0,   0,-1, 0,   0, 0, 1 )
                + ifcc(i1,  i2,  m-i3) &      !       Mz   (  1, 0, 0,   0, 1, 0,   0, 0,-1 )
                + ifcc(m-i1,m-i2,i3  ) &      ! Mx My      ( -1, 0, 0,   0,-1, 0,   0, 0, 1 )
                + ifcc(m-i1,i2,  m-i3) &      ! Mx    Mz   ( -1, 0, 0,   0, 1, 0,   0, 0,-1 )
                + ifcc(i1,  m-i2,m-i3) &      !    My Mz   (  1, 0, 0,   0,-1, 0,   0, 0,-1 )
                + ifcc(m-i1,m-i2,m-i3) )        ! Mx My Mz   ( -1, 0, 0,   0,-1, 0,   0, 0,-1 )
cDBG        if( mod( t, 8 ) /= 0 ) stop 'SYM ifcc: failed, number not divisible (8)'
            w(i1,i2,i3) = t/8
          enddo ! i1
        enddo ! i2
      enddo ! i3

      tm(3) = Wtime() ! end mirror ops, start rotations

      do i3 = 1, 2*n
        do i2 = 1, 2*n
          do i1 = 1, 2*n
            ! rotations
            t = ( w(i1,i2,i3) & !! (  1, 0, 0,   0, 1, 0,   0, 0, 1 )
                + w(i2,i3,i1) & !! (  0, 0, 1,   1, 0, 0,   0, 1, 0 )
                + w(i3,i1,i2) & !! (  0, 1, 0,   0, 0, 1,   1, 0, 0 )
                + w(i3,i2,i1) & !! (  0, 0, 1,   0, 1, 0,   1, 0, 0 )
                + w(i2,i1,i3) & !! (  0, 1, 0,   1, 0, 0,   0, 0, 1 )
                + w(i1,i3,i2) ) !! (  1, 0, 0,   0, 0, 1,   0, 1, 0 )
cDBG        if( mod( t, 6 ) /= 0 ) stop 'SYM ifcc: failed, number not divisible (6)'
            ifcc(i1,i2,i3) = t/6
          enddo ! i1
        enddo ! i2
      enddo ! i3

      tm(4) = Wtime() ! end rotations

#ifdef DEBUG
!+ debug
      if( i01 == 0 ) then
        ! check weight normalization: the irreducible wedge has been set to the value 48,
        ! edges and faces to 24,16,12, or 8, others to 0. After the symmetry operations,
        ! all entries of ifcc must be 1 exactly!
        if( any( ifcc /= 1 ) ) then
          if(o>0) write(o,'(4A,I4,9A)') sym, fun, WARNING(0), 'for n =', n, ' not all D are 1!'
        endif
      endif

    enddo ! i01
!- debug
#endif

cDBG  if(o>0) write(o,'(3A,5F10.5,9A)') sym, fun, 'time[a,w,m,r,tot]', tm(1:4)-tm(0:3), tm(4)-tm(0), ' sec'

#ifdef FULL_DEBUG
!+ full_debug
    if( n < 4 ) then ! show all list
      write(*,*)
      do i3 = 1, 2*n
        write(*,'(9(A,I4))') 'z =', i3, ' /', 2*n
        do i2 = 1, 2*n
          write(*,'(999I3)') ifcc(:,i2,i3)
        enddo ! i2
      enddo ! i3
      write(*,*)
    endif ! n < 4
!- full_debug
#endif

!- use_symmetry
#endif
  endsubroutine ! prepare_fcc_irreducible

!- new_symmetrize
#endif


  integer function kgen( ) result( ios )
  ! use symmetry, only: generate_kpoint_set, string2key
  use configuration, only: BlockKeyWord_Kpnts
  use type_item, only: operator(.in.)
    integer                   :: ik, n(3), ksym, nk
    character(len=80)         :: ln
    character(len=3)          :: w
    integer                   :: o = 6
    real, allocatable         :: kp(:,:) ! (0:3,1:nirreducible)
    real                      :: s(3)

    n = 8 ! default
    write(6,'(9(A,I0))') 'kpoint generator: please insert n ( default is ',n(1),' x ',n(2),' x ',n(3),' )'
    read(5,'(A)',iostat=ios) ln
    read(ln,*,iostat=ios) n(1:3)
    if( ios /= 0 ) then
      read(ln,*,iostat=ios) n(1)
      if( ios == 0 ) n(2:3) = n(1)
    endif ! ios /= 0
    write(6,'(9(A,I0))') 'kpoint generator: use ',n(1),' x ',n(2),' x ',n(3)
    write(6,'(A)') 'kpoint generator: please insert symmetry {non,inv,sc,bcc,fcc,dia,hcp}'
    read(5,*,iostat=ios) w
    ksym = w .in. Dictionary
    write(6,*) 'kpoint generator: use ',trim(ksym .in. Dictionary)
    ! shift of the origin
    s = 0. ! default
    write(6,'(A)') 'kpoint generator: please insert shift (default is 0.0)'
    read(5,'(A)',iostat=ios) ln
    read(ln,*,iostat=ios) s(1:3)
    if( ios /= 0 ) then
      read(ln,*,iostat=ios) s(1)
      if( ios == 0 ) s(2:3) = s(1)
    endif ! ios /= 0
    write(6,'(A,3F10.6)') 'kpoint generator: use shift', s

    ios = generate_kpoint_set( n, kp, shift=s, ksymm=ksym )
    nk = size( kp, 2 )

    write(o,'(A)') BlockKeyWord_Kpnts
    write(o,'(3(A,I0),9A)') '# ', n(1), ' x ', n(2), ' x ', n(3), '   ',trim(ksym .in. Dictionary)
    write(o,'(3F12.8,F10.6)') ( kp(1:3,ik), kp(0,ik), ik=1,nk )
    write(o,'(A,I0)') '# number of kpoints is ', nk
    write(o,'(A)') BlockKeyWord_Kpnts
  endfunction ! kgen

#ifdef EXTENDED
!+ extended

  integer function test( )
    write(*,*,iostat=test) __FILE__,' no test implemented for this module!'
  endfunction ! test

!- extended
#endif

endmodule ! symmetry

!    1 : E
!        1  0  0    0.0
!        0  1  0    0.0
!        0  0  1    0.0
! 
!    2 : m
!        0  1  0    0.0
!        1  0  0    0.0
!        0  0  1    0.0
! 
!    3 : c_2
!       -1  0  0    0.0
!        0 -1  0    0.0
!        1  1  1    0.0
! 
!    4 : m
!        0 -1  0    0.0
!       -1  0  0    0.0
!        1  1  1    0.0
! 
!    5 : c_2
!        0  1  0    0.0
!        1  0  0    0.0
!       -1 -1 -1    0.0
! 
!    6 : m
!        1  0  0    0.0
!        0  1  0    0.0
!       -1 -1 -1    0.0
! 
!    7 : I
!       -1  0  0    0.0
!        0 -1  0    0.0
!        0  0 -1    0.0
! 
!    8 : c_2
!        0 -1  0    0.0
!       -1  0  0    0.0
!        0  0 -1    0.0
! 
!    9 : m
!        0  0  1    0.0
!        1  0  0    0.0
!       -1 -1 -1    0.0
! 
!   10 : c_3
!        0  0  1    0.0
!        0  1  0    0.0
!       -1 -1 -1    0.0
! 
!   11 : c_2
!        1  1  1    0.0
!        0 -1  0    0.0
!        0  0 -1    0.0
! 
!   12 : m
!        1  1  1    0.0
!       -1  0  0    0.0
!        0  0 -1    0.0
! 
!   13 : c_2
!       -1  0  0    0.0
!        1  1  1    0.0
!        0  0 -1    0.0
! 
!   14 : c_3
!        1  0  0    0.0
!        0  0  1    0.0
!       -1 -1 -1    0.0
! 
!   15 : m
!        0  1  0    0.0
!        0  0  1    0.0
!       -1 -1 -1    0.0
! 
!   16 : m
!        0 -1  0    0.0
!        1  1  1    0.0
!        0  0 -1    0.0
! 
!   17 : c_2
!       -1  0  0    0.0
!        0  0 -1    0.0
!        0 -1  0    0.0
! 
!   18 : m
!        0  1  0    0.0
!       -1 -1 -1    0.0
!        1  0  0    0.0
! 
!   19 : m
!        0 -1  0    0.0
!        0  0 -1    0.0
!       -1  0  0    0.0
! 
!   20 : c_3
!        1  0  0    0.0
!       -1 -1 -1    0.0
!        0  1  0    0.0
! 
!   21 : m
!        1  1  1    0.0
!        0  0 -1    0.0
!        0 -1  0    0.0
! 
!   22 : c_2
!        0  0  1    0.0
!       -1 -1 -1    0.0
!        1  0  0    0.0
! 
!   23 : c_4
!        1  1  1    0.0
!        0  0 -1    0.0
!       -1  0  0    0.0
! 
!   24 : m
!        0  0  1    0.0
!       -1 -1 -1    0.0
!        0  1  0    0.0
! 
!   25 : m
!        0  0 -1    0.0
!       -1  0  0    0.0
!        0 -1  0    0.0
! 
!   26 : c_2
!        0  0 -1    0.0
!        0 -1  0    0.0
!       -1  0  0    0.0
! 
!   27 : c_3
!       -1 -1 -1    0.0
!        0  1  0    0.0
!        1  0  0    0.0
! 
!   28 : m
!       -1 -1 -1    0.0
!        1  0  0    0.0
!        0  1  0    0.0
! 
!   29 : m
!        0  0  1    0.0
!        0  1  0    0.0
!        1  0  0    0.0
! 
!   30 : c_4
!        1  1  1    0.0
!       -1  0  0    0.0
!        0 -1  0    0.0
! 
!   31 : m
!        1  1  1    0.0
!        0 -1  0    0.0
!       -1  0  0    0.0
! 
!   32 : c_3
!        0  0  1    0.0
!        1  0  0    0.0
!        0  1  0    0.0
! 
!   33 : m
!        0  0 -1    0.0
!        1  1  1    0.0
!       -1  0  0    0.0
! 
!   34 : c_4
!        0  0 -1    0.0
!        1  1  1    0.0
!        0 -1  0    0.0
! 
!   35 : m
!       -1 -1 -1    0.0
!        0  0  1    0.0
!        1  0  0    0.0
! 
!   36 : c_2
!       -1 -1 -1    0.0
!        0  0  1    0.0
!        0  1  0    0.0
! 
!   37 : m
!       -1  0  0    0.0
!        1  1  1    0.0
!        0 -1  0    0.0
! 
!   38 : c_3
!        0  1  0    0.0
!        0  0  1    0.0
!        1  0  0    0.0
! 
!   39 : c_4
!        0 -1  0    0.0
!        1  1  1    0.0
!       -1  0  0    0.0
! 
!   40 : m
!        1  0  0    0.0
!        0  0  1    0.0
!        0  1  0    0.0
! 
!   41 : m
!        1  0  0    0.0
!       -1 -1 -1    0.0
!        0  0  1    0.0
! 
!   42 : c_3
!        0  1  0    0.0
!       -1 -1 -1    0.0
!        0  0  1    0.0
! 
!   43 : c_4
!        0 -1  0    0.0
!        0  0 -1    0.0
!        1  1  1    0.0
! 
!   44 : m
!       -1  0  0    0.0
!        0  0 -1    0.0
!        1  1  1    0.0
! 
!   45 : c_3
!       -1 -1 -1    0.0
!        1  0  0    0.0
!        0  0  1    0.0
! 
!   46 : m
!       -1 -1 -1    0.0
!        0  1  0    0.0
!        0  0  1    0.0
! 
!   47 : m
!        0  0 -1    0.0
!        0 -1  0    0.0
!        1  1  1    0.0
! 
!   48 : c_4
!        0  0 -1    0.0
!       -1  0  0    0.0
!        1  1  1    0.0

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
!! @version 4.03
!!
!! generates weigts for each KS-state according to a
!! projection of the state to the atomic orbitals
!! given that the PAW enn=1 partial waves is normalized
!! to the atomic eigenstate. These weights can forther
!! be used in a ProjectedDensityOfStates
module analysis
  use configuration, only: o ! output unit, 0: no output
implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: sym = 'ANA' !! module symbol

  public :: PDoS_weights
#ifdef EXTENDED
  public :: test
#endif


  interface PDoS_weights
    module procedure PDoS_weights_r, PDoS_weights_c
  endinterface

  contains

! end of the head part
#endif



#if R1_C2 == 1
#define REAPLEX real
  ! doc
  !! compute projection of the KS-states onto atomic
  !! orbitals exploiting that the enn=1 PAW partial wave
  !! is usually chosen as the atomic eigenstate,
  !! so all we have to do is square the projection
  !! coefficient
  subroutine PDoS_weights_r( &
#else
#define REAPLEX complex
  !! see real version
  subroutine PDoS_weights_c( &
#endif
                            a, &
                            jatm, &
                            cpsi, &
                            psi, &
                            me, &
                            mode, &
                            weights, &
                            labels )
  use configuration, only: WARNING, ERROR
  use type_state, only: state
  use type_atom, only: atom
  use MPItools, only: MPIparallel
  use MPItools, only: MPIallsum
  use input, only: eval
#ifdef DEBUG
    character(len=*), parameter           :: fun = ' characterize_states: '
#else
    character(len=*), parameter           :: fun = ': '
#endif
    integer, parameter                    :: ELLMAX = 3
    character(len=*), parameter           :: ILMCHAR(0:(ELLMAX+1)**2) = (/ 'all', &
    's  ','px ','pz ','py ','dr2','dzx','dz2','dyz','dxy','f1 ','f2 ','f3 ','f4 ','f5 ','f6 ','f7 ' /)
    character, parameter                  :: ELLCHAR(-1:ELLMAX) = (/'a','s','p','d','f'/) ! 'a'=all
    integer, parameter                    :: ELL_LIST((ELLMAX+1)**2) = (/0, 1,1,1, 2,2,2,2,2, 3,3,3,3,3,3,3/)
    ! arguments
    type(atom), intent(in)                :: a(1:)          !! parallelized list of atoms
    integer, intent(in)                   :: jatm           !! number of the atom to analyze
    REAPLEX, intent(in)                   :: cpsi(1:,1:)    !! vector of projection coefficients
    type(state), intent(in)               :: psi(1:)        !! parallelized list of state objects
    integer, intent(in)                   :: me             !! MPI grid rank ! gc%rank
    character(len=2), intent(in)          :: mode           !! 3 allowed modes: '  ', 'l ', 'lm'
    real, allocatable, intent(out)        :: weights(:,:)   !! results
    character(len=*), allocatable, intent(out) :: labels(:) !! labels
    ! local vars
    integer                               :: nbsk, nlm, lmax, ia, ibsk, ja, i1, i2, is
    integer                               :: ind, u=0
    real                                  :: wnew
    logical                               :: emmi, elli

    elli = ( mode(1:1) /= 'l' )
    emmi = ( mode(2:2) /= 'm' ) .or. elli
#ifdef DEBUG
    u = o
#else
    ! add line $pdosunit = 33 to your input file to write the weights to fort.33
    u = nint( eval( '$pdosunit', 0.0 ) ) ! 0:no output
    if(o>0 .and. u>0) write(o,'(3A,I6,A,I3)') sym, fun, 'write weights for PDoS of atom#', jatm, ' to fort.', u
#endif

    nbsk = size(cpsi,2) ; if( nbsk < 1 ) return ! number of states

    ia = 0
    do ja = 1, size(a)
      if( a(ja)%ja == jatm .and. a(ja)%owner == me ) then
        ia = ja
      endif ! global index is jatm
    enddo ! ja
    deallocate( weights, stat=is ) ! no matter if allocated or not
    if( ia == 0 ) return ! atom is not in the list or process is not owner

    deallocate( labels, stat=is ) ! no matter if allocated or not

    lmax = min(a(ia)%s%ellmax,ELLMAX)

    ! determine number of weights additional to weight(0): 'all'
    if( elli ) then     ; nlm = 0             ! ell- and emm-integrated
    elseif( emmi ) then ; nlm = lmax+1        ! ell-resolved, emm-integrated
    else                ; nlm = ( lmax+1 )**2 ! ell- and emm-resolved
    endif ! ell- or emm-integrated

    allocate( weights(0:nlm,nbsk), labels(0:nlm), stat=is )
    if( is /= 0 ) stop 'ANA: allocation of WEIGHTS and LABELS failed!'

    ! set labels
    labels(0) = 'all'
    if( elli ) then     ! nothing more
    elseif( emmi ) then ; labels(1:nlm) = ELLCHAR(0:nlm-1)
    else                ; labels(1:nlm) = ILMCHAR(1:nlm)
    endif ! ell- or emm-integrated

    weights = 0. ! init weights

    if(u>0) write(u,'(A)') '' ! empty line
    if(u>0) write(u,'(A,I0)') 'atom #', jatm
    if(u>0) write(u,'(A6,A2,2A6,A2,99A10)') 'band', ' s', 'kpt', 'atom', '', labels(0:nlm)

!$omp parallel do private(ibsk,i1,i2,wnew,ind)
    do ibsk = 1, nbsk ! for all KS-states
      !----------------------------------------------------------------------------
      do i1 = 1, a(ia)%s%mlnm ! all projectors
        if( a(ia)%s%ind_enn(i1) /= 1 ) cycle ! only the 1st projector
        i2 = a(ia)%offprj + i1 ! index in dim#1 of cpsi
#ifdef DEBUG
        if( i2 > size(cpsi,1) ) stop 'ANA: projector index exceeds dim#1 of CPSI'
#endif
        ! find the weight
#if R1_C2 == 1
        wnew = cpsi(i2,ibsk)**2
#else
        wnew = real(cpsi(i2,ibsk))**2 + aimag(cpsi(i2,ibsk))**2
#endif

        weights(0,ibsk) = weights(0,ibsk) + wnew ! add to total weight
        if( elli ) then
          ! nothing more
        elseif( emmi ) then
          ind = a(ia)%s%ind_ell(i1)+1
          weights(ind,ibsk) = weights(ind,ibsk) + wnew ! add to ell-weight
        else
          ind = a(ia)%s%ind_ilm(i1)
          weights(ind,ibsk) = weights(ind,ibsk) + wnew ! add to ell-emm-weight
        endif ! ell- or emm-integrated
      enddo ! i1
      !----------------------------------------------------------------------------
    enddo ! ibsk
!omp end parallel do

#ifdef DEBUG
    do ibsk = 1, nbsk ! for all KS-states
      write(*,'(4(I0,A),999F24.16)') psi(ibsk)%jbnd,' ',psi(ibsk)%jspn,' ',psi(ibsk)%jkpt,' ',jatm, 'a ', weights(0:,ibsk)
    enddo ! ibsk
#endif
  endsubroutine ! PDOS_weights

#if R1_C2 == 2

#ifdef EXTENDED
!+ extended

  integer function test( )
    write(*,*,iostat=test) __FILE__,' no module test implemented!'
  endfunction ! test

!- extended
#endif

endmodule ! analysis
#endif
#endif

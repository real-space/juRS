#ifdef DEBUG_ALL
#define DEBUG
#endif

! #define DEBUG
! #define FULL_DEBUG


#ifdef R1_C2
! This file will be preprocessed twice, first
! with R1_C2 == 1, and then with R1_C2 == 2
! then the two parts are concatenated to one source.
! This requires that the module head has to be
! appearing, when R1_C2 == 1 and the line "end module <name>"
! with R1_C2 == 2.
#if R1_C2 == 1

! documentation
!! @author Paul Baumeister
!! @version 3.0
!!
!! the module boundary takes care of the communication
!! between the different MPI processes for the halo-data
!! exchange. When a wave function is communicated across
!! the periodic boundary, the Bloch-factor has to be
!! applied, because a Bloch state is not periodic, but
!! rather quasi-periodic.
!!
!! The alternative approach is to split of the Bloch
!! factor and only treat the fully periodic part of
!! the wavefunction. This on the other hand brings
!! in k-point dependence in all non-local operators,
!! such as the kinetic energy and the non-local
!! pseudopotential. Compared to the full volume
!! of the computational domain, an operation
!! restricted to the surface scales better.
module boundary
#ifdef DEBUG
  use configuration, only: o ! output unit, 0: no output
#endif
implicit none
  private ! default for the module namespace
  character(len=*), parameter, private :: sym = 'BND' !! module symbol

  public :: dataexchange
  public :: test_dataexchange
#ifdef EXTENDED
  public :: test
#endif

  interface dataexchange ! send and receive data for the halos
    module procedure dataexchange_r, dataexchange_c
  endinterface

  interface copy
    module procedure copy_r, copy_c
  endinterface

  integer, parameter          :: I_SEND = 1, I_RECV = 2 !! index for send and receive temp array
  character(len=*), parameter :: DIR(1:3) = (/'x','y','z'/) !! cartesian direction
  character(len=*), parameter :: UPDN(1:2) = (/'dn','up'/) !! send to lower or higher rank

  logical, save :: displayboundaryconditions = .true. !! show the boundary conditions at start (only once)

#ifndef DEBUG
  integer, parameter :: o=0 !! output unit 0: no output
#endif

  contains

#endif
! end of module head


#if R1_C2 == 1
#define REAPLEX real
#else
#define REAPLEX complex
#endif


#if R1_C2 == 1
  !! this routine is the main routine for real space domain decomposition
  !!
  !! a halo enlarged array is given, where the central region contains data.
  !! MPI communication will send the corresponding data pieces to the 
  !! neighboring processes and copy them into the halos of the array.
  !!
  !! dataexchange is needed for all non-local, but locallized operations
  !! such as finite difference derivatives or high order interpolation.
  subroutine dataexchange_r( &
#else
  !! for documentation, see dataexchange_r above
  !!
  !! *PHASE CONVENTION*
  !!
  !! for the Bloch wave functions with a constant
  !! lattice periodic part, a plane wave running into positive
  !! direction is assumed i.e.
  !!
  !!v Psi(x) = exp(ikx)
  !!
  !! there are 5 regions of the halo-enlarged vector:
  !!
  !!>  |lw|lr|center|ur|uw| l:lower, upper, w:write, r:read
  !!      |            |
  !!< exp(-ikL/2)  exp(ikL/2)
  !!
  !! let''s consider the lower boundary (ise==1)
  !! uw = lr * phase(id,1) = exp(-ikL/2) * exp(ikL)
  !!
  subroutine dataexchange_c( &
#endif
                             g, v, kp, nf )
  use type_grid, only: grid
  use type_grid, only: BC_INNER
  use type_grid, only: BC_FINITE
  use type_grid, only: BC_PERIODIC
  use type_grid, only: BC_MIRROR
#ifdef GENERAL_CELL
  use type_grid, only: BC_SHIFTED
#endif
  use type_grid, only: BCD => BC_DESCRIPTION
  use configuration, only: WARNING, ERROR, STOPONERROR
  use type_kpoint, only: kpoint, kpoint_Gamma
  use MPIconst, only: PREC, MPI_STATUS_SIZE
  implicit none

    ! parameters
    character(len=*), parameter     :: fun = ' dataexchange: '
    integer, parameter              :: DN = 1, UP = 2
    ! arguments
    type(grid), intent(in)          :: g !! the grid descriptor contains the boundary conditions g%bc(3,2), the number of grid points g%ng(1:4) and the halo thickness g%nh(1:3)
    REAPLEX, intent(inout)          :: v(1:,1:,1:) !! dims(1:g%ng+2*g%nh)^3 <br> halo enlarged array
    type(kpoint), intent(in), optional :: kp    !! kpoint for Bloch phase factor, default is the Gamma point
    integer, intent(in), optional      :: nf(1:3) !! halo thickness that is needed, if it deviates from the halos of the array, this can save communication volume and time
    ! local vars
    integer                         :: mx, id, ise, iput!, ist, iget
!     integer                         :: npnh(3) ! n+nh

    ! save variables
    integer, save                   :: bc(1:3,DN:UP) ! technical boundary condition
    integer, save                   :: h(1:3) = 0 ! not initialized
    integer, save                   :: n(1:3) = 0 ! not initialized
    integer, save                   :: f(1:3) = 0 ! not initialized
    type(kpoint), save              :: akp ! actual kpoint
!     logical                         :: changed
    REAPLEX, save                   :: phase(1:3,DN:UP)
    REAPLEX, allocatable, save      :: t(:,:,:)
    integer, save                   :: nsend(1:3,DN:UP) = 0 ! how many elements to send
    integer, save                   :: nrecv(1:3,DN:UP) = 0 ! how many elements to send
    logical, save                   :: warning_realnumbersandkpoint = .true.

#ifndef NOMPI
    integer                         :: ifl, istat(MPI_STATUS_SIZE), ierr!, ireq(DN:UP)
#endif

#ifdef DEBUG
    integer                         :: u
#endif

#ifdef DEBUG
    ! check array dimensions
    do id = 1, 3
      if( size( v, id ) /= g%ng(id) + 2*g%nh(id) ) &
        stop 'BND dataexchange: some dim of array V does not match the grid G.'
    enddo ! id
#endif


    if( present(kp) ) then ; akp = kp
    else                   ; akp = kpoint_Gamma( )
    endif ! present(kp)


    h(1:3)      = g%nh(1:3) ! halo thickness (upper == lower)
    n(1:3)      = g%ng(1:3) ! grid numbers
    f(1:3)      = h(1:3) ; if( present( nf ) ) f = min( max( 0, nf ), h )

    bc(1:3,DN:UP) = g%tbc(1:3,1:2) ! use the technical boundary condition

    ! number of elements to send:
    ! for each direction, the number of elements is the area times the
    ! halo thickness h. The formula for the area varies depending on
    ! the direction, because like that, also the edges
    ! (halo-halo overlap regions)_are transferred.
    nsend(1,DN:UP) =   h(1)          * ( n(2)+0*h(2) ) * ( n(3)+0*h(3) )
    nsend(2,DN:UP) = ( n(1)+2*h(1) ) *   h(2)          * ( n(3)+0*h(3) )
    nsend(3,DN:UP) = ( n(1)+2*h(1) ) * ( n(2)+2*h(2) ) *   h(3)

    mx = maxval(nsend)
    if( size( t, 1 ) < mx ) then
      if(o>0) write(o,'(4A,9(I0,A))') sym, fun, 'enlarge temporary ', &
#if R1_C2 == 1
        'real array from ', size(t,1), ' to ', mx
#else
        'complex array from ', size(t,1), ' to ', mx
#endif
      if( allocated( t ) ) deallocate( t )
      allocate( t( mx, 1:2, I_SEND:I_RECV ) )
    endif ! size t < mx

    ! for all directions ( where the halo has a positive extend )
    do id = 1, 3
      if( h(id) < 1 ) cycle

      do ise = DN, UP
        selectcase( bc(id,ise) )
        case( BC_INNER )
          phase(id,ise) = 1. ! between 2 domains, the information
                             ! is just passed as it is
        case( BC_FINITE )
          phase(id,ise) = 0. ! wave functions and Hartree/Coulomb
                             ! potential have to vanish in the vacuum
          nsend(id,ise) = 0 ! no need for communication
          nrecv(id,ise) = 0 ! no need for communication

        case( BC_MIRROR )
          phase(id,ise) = 1. ! only 1.0 and -1.0 are allowed
          nsend(id,ise) = 0 ! no need for communication
          nrecv(id,ise) = 0 ! no need for communication

          if( present( kp ) ) then
            akp = kp
#ifdef DEBUG
            if( abs(abs(real(kp%eik(id)))-1.)>1.E-7 .or. abs(aimag(kp%eik(id)))>1.E-7 ) then
              if(o>0) write(o,'(3A,6F7.3,9A)') sym, fun, 'kpoint phase = [', kp%eik(1:3), ' ].'
              stop 'BND k-point is not commensurate with mirror BC.'
            endif ! k(id) == 0. or 0.5
#endif
            if( abs( real( kp%eik(id) ) ) == 1. ) then
              phase(id,ise) = real( kp%eik(id) ) ! even or uneven mirror symmetry
            else ; stop 'BND k-point phase factor is not commensurate with mirror BC.'
            endif !

          endif ! present( kp )


#ifdef GENERAL_CELL
        case( BC_PERIODIC, BC_SHIFTED )
          ! periodic and shifted boundary conitions
#ifndef DEBUG
          if( bc(id,ise) == BC_SHIFTED ) then
            ! as long as the the shift boundary conditions are not fully debugged,
            ! it will not run without -D DEBUG at least for this module.
            if(o>0) write(o,'(9A)') sym, fun, WARNING(0), &
              'shift boundary condition is maybe not bug-free.'
          endif ! bc == BC_SHIFTED
#endif

#else
        case( BC_PERIODIC )
#endif
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !! PHASE CONVENTION
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !! for the Bloch wave functions with a constant
  !! lattice periodic part, a plane wave running into positive
  !! direction is assumed i.e.
  !!
  !! Psi(x) = exp(ikx)
  !!
  !! there are 5 regions of the halo-enlarged vector:
  !!
  !!  |lw|lr|center|ur|uw| l:lower, upper, w:write, r:read
  !!     |            |
  !! exp(-ikL/2)  exp(ikL/2)
  !!
  !! let''s consider the lower boundary (ise==1)
  !! uw = lr * phase(id,1) = exp(-ikL/2) * exp(ikL)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          phase(id,ise) = 1. ! any phase with |phase|^2 = 1. is allowed
          if( present( kp ) ) then
            akp = kp
            ! prepare the phase factor for the k-point
            if( ise == DN ) then
              phase(id,DN) = kp%eik(id)
            else ! ise == 1
              phase(id,UP) = 1./kp%eik(id)
            endif ! ise == 1 or 2
#ifdef DEBUG
#if R1_C2 == 1
! REAL only
            if( aimag( kp%eik(id) ) /= 0. ) then
              if( warning_realnumbersandkpoint ) then
                if(o>0) write(o,'(9A)') sym, fun, WARNING(0), &
                  'kpoint present with real numbers and periodic boundary conditions.'
                warning_realnumbersandkpoint = .false. ! disable further warnings
              endif ! warning_realnumbersandkpoint
            endif ! aimag(eik) /= 0.
#endif
#endif
          endif ! present(kp)

        case default
#ifdef DEBUG
          if(o>0) write(o,'(9A)') sym, fun, ERROR, &
            'boundary condition switch in ', DIR(id), '-direction is NOT DEFINED.'
          if( STOPONERROR ) &
#endif
          stop 'boundary: BC not implemented.'
        endselect ! bc

      enddo ! ise
    enddo ! id

#ifdef DEBUG
    ! display the boundary conditions
    if( displayboundaryconditions .and. o>0 ) then
      write(o,'(A)') '', ' Boundary conditions: ' ! empty line + head line
      if( any( g%nproc > 1 ) ) &
      write(o,'(A)')     '             physical  technical'
      write(o,'(A)')     ' -------------------------------' ! line
      do id = 1, 3 ! for all directions
        do ise = DN, UP ! for both boundaries
          if( g%nproc(id) > 1 ) then                              ! physical BC              ! technical BC
            write(o,'(9A)') '    ', DIR(id), ': ', UPDN(ise), '  ', BCD( g%bc(id,ise) ), '  ', BCD( bc(id,ise) )
          else  ! nproc > 1
            write(o,'(9A)') '    ', DIR(id), ': ', UPDN(ise), '  ', BCD( g%bc(id,ise) )
          endif ! nproc > 1
        enddo ! ise
      enddo ! id
      write(o,'(A)')    ' -------------------------------', '' ! line + empty line
      displayboundaryconditions = .false. ! this will be displayed only once.
    endif ! displayboundaryconditions
#endif


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  do id = 1, 3 ! X, Y and Z-direction
    if( h(id) < 1 ) cycle ! halo size must be larger than 0

    iput = 0 ! init iput=0 for the lower boundary (ise==1)
    do ise = DN, UP

      selectcase( bc(id,ise) )
      case( BC_FINITE )
        ! set the halos to zero, so no data has to be copied
#ifdef FULL_DEBUG
!         if(o>0) write(o,'(9A)') sym, fun, 'set halo to zero in ', DIR(id), '-direction.'
#endif
        selectcase( id )
        case( 1 ) ; v( iput+1:iput+h(1), :, : ) = 0.
        case( 2 ) ; v( :, iput+1:iput+h(2), : ) = 0.
        case( 3 ) ; v( :, :, iput+1:iput+h(3) ) = 0.
        endselect

      case( BC_INNER, BC_PERIODIC )
        ! send without phase factors (in between domains) or
        ! multiply the halos with the phase 
        ! factor eikL of the k-Point (if present)
        ! else with 1.0
                      ! copy  v --> t
        nsend(id,ise) = copy( v, t(:,ise,I_SEND), id, ise, v2t=.true., n=n, h=h, f=f )
        nrecv(id,ise) = nsend(id,ise)

        ! apply the phase factor to the part that will be sent
!         if( phase(id,ise) /= (1.0,0.0) ) &
        t(:,ise,I_SEND) = t(:,ise,I_SEND) * phase(id,ise)

!         if( phase(id,ise) /= 1. ) then
!           if(o>0) write(o,'(3A,I1,A,I1,A,2F7.3)') sym, fun, &
!             'multiply phase(id=', id, ',ise=', ise, ') =', phase(id,ise)
!             t(:,ise,I_SEND) = t(:,ise,I_SEND) * phase(id,ise)
!         endif ! phase /= 1.
#ifdef GENERAL_CELL
      case( BC_SHIFTED )
        ! periodic and shifted boundary conitions
        selectcase( id )
        case( 2 )
          call shifted_y_boundary( v, phase(:,ise), ise, n, h, shift=g%shift(1) )
        case( 3 ) ! ; call shifted_z_boundary( v, phase(id,ise), id, ise, n, h, shift=g%shift(2:3) )
          stop 'dataexchange: Shifted BC for Z-direction not yet implemented.'
        case default ; stop 'dataexchange: Shifted BC only possible for Y- and Z-direction.'
        endselect ! id
#endif
      case( BC_MIRROR ) ; call mirror_boundary( v, real(phase(id,ise)), id, ise, n, h ) ! copy without using halos
      endselect ! bc

    iput = n(id) + h(id) ! set iput to n+h for the upper boundary (ise==2)
    enddo ! ise




    ! if any if the ksend-switches has been turned on,
    ! there will be communication right now
    do ise = DN, UP
      if( nsend(id,ise) > 0 .or. nrecv(id,3-ise) > 0 ) then

#ifndef NOMPI
! MPI part

#ifdef FULL_DEBUG
        if(o>0) write(o,'(2A,5(A,I6),A,I2,9A)') sym, fun, &
          'SendRecv me#', g%rank, &
          ' send', nsend(id,ise), ' to#', g%neighbor(id,ise), &
          ' get', nrecv(id,3-ise), ' from#', g%neighbor(id,3-ise), &
          ' ise=', ise, ' direction=', DIR(id)
#endif

        if( g%rank /= g%neighbor(id,ise) ) then
          ifl = ise ! flag
          call MPI_Sendrecv( t(:,ise,I_SEND), nsend(id,ise), &
                             PREC( R1_C2 ), g%neighbor(id,ise), ifl,  &
                             t(:,3-ise,I_RECV), nrecv(id,3-ise), &
                             PREC( R1_C2 ), g%neighbor(id,3-ise), ifl,  &
                             g%comm, istat, ierr )
#ifdef FULL_DEBUG
          if( ierr /= 0 ) then
            if(o>0) write(o,'(3A,I3)') sym, fun, 'after SendRecv ierr =', ierr
          endif
#endif
        else  ! me == my neighbor
! end MPI part
#else
        if( .true. ) then ! always
#endif
          t(:,3-ise,I_RECV) = t(:,ise,I_SEND) ! ise==1: 2 --> 1, ise==2: 1 --> 2
        endif ! ...

      endif ! ksend(id,ise) == 1
    enddo ! ise



    do ise = DN, UP
      selectcase( bc(id,ise) )
      case( BC_INNER, BC_PERIODIC )
!         if(o>0) write(o,'(9A)') sym, fun, 'set halo to recv buffer in ', DIR(id), '-direction.'

        ! write the data of the receive buffer into halos of v
                      ! copy  v <-- t
        nsend(id,ise) = copy( v, t(:,ise,I_RECV), id, ise, v2t=.false., n=n, h=h, f=f )

      endselect ! bc
    enddo ! ise

  enddo ! id = 1, 3
  ! end of XYZ-direction
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  return

  contains


  subroutine mirror_boundary( vec, phase, id, ise, n, h )
  implicit none
    ! arguments
    REAPLEX, intent(inout)                :: vec(1:,1:,1:) !! halo enlarged vector
    real, intent(in)                      :: phase ! only 1.0 and -1.0 are physical, therefore real
    integer, intent(in)                   :: id, ise
    integer, intent(in)                   :: n(1:3)
    integer, intent(in)                   :: h(1:3)

#ifdef DEBUG
    if( id < 1 .or. id > 3 ) stop 'mirror_boundary: ID stands for the direction ==> bounds [1 3].'
    if( ise < 1 .or. ise > 2 ) stop 'mirror_boundary: ISE stands for the start/end ==> bounds [1 2].'
#endif
    if( ise /= 1 ) stop 'BND mirror_boundary: MIRROR PLANE may only be at the lower boundary!'


    !
    !     [1]   [2]   [3]   [4]   [5]   [6]   [7]
    !   |-----|-----|-----M-----|-----|-----|--...
    !   1) <-----------------------------<
    !   2)       <-----------------<
    !   3)             <-----<
    !                             example: h=3, lower boundary, M= mirror BC

    selectcase( id )
    case( 1 ) ! X-direction
      vec(      1:  h(1):+1, 1+1*h(2):n(2)+1*h(2), 1+1*h(3):n(3)+1*h(3) ) = phase * &
      vec( 2*h(1):1+h(1):-1, 1+1*h(2):n(2)+1*h(2), 1+1*h(3):n(3)+1*h(3) )
    case( 2 ) ! Y-direction
      vec( 1+0*h(1):n(1)+2*h(1),      1:  h(2):+1, 1+1*h(3):n(3)+1*h(3) ) = phase * &
      vec( 1+0*h(1):n(1)+2*h(1), 2*h(2):1+h(2):-1, 1+1*h(3):n(3)+1*h(3) )
    case( 3 ) ! Z-direction
      vec( 1+0*h(1):n(1)+2*h(1), 1+0*h(2):n(2)+2*h(2),      1:  h(3):+1 ) = phase * &
      vec( 1+0*h(1):n(1)+2*h(1), 1+0*h(2):n(2)+2*h(2), 2*h(3):1+h(3):-1 )
    endselect ! id

  endsubroutine mirror_boundary

#ifdef GENERAL_CELL
! begin GENERAL_CELL
  subroutine shifted_y_boundary( vec, phase, ise, n, h, shift )
  implicit none
    ! parameters
    character(len=*), parameter           :: fun = ' shifted_y_boundary: '
    integer, parameter                    :: ID = 2
    ! arguments
    REAPLEX, intent(inout)                :: vec(1:,1:,1:)
    REAPLEX, intent(in)                   :: phase(3) ! all 3 phase factors
    integer, intent(in)                   :: ise
    integer, intent(in)                   :: n(1:3)
    integer, intent(in)                   :: h(1:3)
    real, intent(in)                      :: shift(1)
    ! local vars
    REAPLEX                               :: ph ! phase factor
    integer                               :: ih, iput, iget
    integer                               :: i1, i2, j1, j2
    integer                               :: isft(1)
    real                                  :: rsft(1)
    logical                               :: matched
#ifdef DEBUG
    integer                               :: ilim(1:4,2,1)
#endif

#ifdef DEBUG
    if( id /= 2 ) &
      stop 'shifted_y_boundary: ID must be 2 (Y-direction) for this routine.'
    if( ise < 1 .or. ise > 2 ) &
      stop 'shifted_y_boundary: ISE stands for the start/end ==> bounds [1 2].'
    if( abs(shift(1)) > 0.5 ) &
      stop 'shifted_y_boundary: |shift| > 1/2.'
#endif

    isft(1) = nint( shift(1)*real(n(1)) )
    rsft(1) = shift(1)*real(n(1)) - isft(1)
    matched = ( abs(rsft(1)) < 1.E-6 )
#ifdef FULL_DEBUG
    if(o>0) write(o,'(3A,F6.3,A,I5,A,F6.3)') sym, fun, &
      'XY shift =', shift(1), ' ==>', isft(1), ' +', rsft(1)
#endif

    !  lower halo  lower copy  bulk region  upper copy     upper halo
    ! ||| 1:1*h ||| h+1:2*h ;;; 2*h+1:n ;;; n+1:n+1*h ||| n+1*h+1:n+2*h |||

    if( matched ) then

      if( ise == 1 ) then
        iput = 0
        iget = h(id)
        isft(1) = -isft(1)
      elseif( ise == 2 ) then
        iput = n(id)+1*h(id)
        iget = n(id)
      else  ! ise == 1 or 2
        stop 'dataexchange: ISE out of bounds [1,2].'
      endif ! ise == 1 or 2

      selectcase( isft(1) )
      case( :-1 )
        ! negative shift
        i1 = 1+0*h(1) ; i2 = n(1)+1*h(1)+isft(1)
        j2 = n(1)+2*h(1) ; j1 = j2-i2+i1
        ! translation iy+1
        ph = phase(2)
        vec( i1:i2, iput+1:iput+h(2), 1+1*h(3):n(3)+1*h(3) ) = ph * &
        vec( j1:j2, iget+1:iget+h(2), 1+1*h(3):n(3)+1*h(3) )
      case( 0 )
        ! no shift
        i1 = 1 ; i2 = n(1)+2*h(1)
        j1 = 1 ; j2 = n(1)+2*h(1)
        ! translation iy+1
        ph = phase(2)
        vec( i1:i2, iput+1:iput+h(2), 1+1*h(3):n(3)+1*h(3) ) = ph * &
        vec( j1:j2, iget+1:iget+h(2), 1+1*h(3):n(3)+1*h(3) )
      case( 1: )
        ! positive shift
        i1 = h(1)+1+isft(1) ; i2 = n(1)+2*h(1)
        j1 = 1+0*h(1) ; j2 = j1+i2-i1
        ! translation iy+1, ix-1
        ph = phase(2)/phase(1)
        vec( i1:i2, iput+1:iput+h(2), 1+1*h(3):n(3)+1*h(3) ) = ph * &
        vec( j1:j2, iget+1:iget+h(2), 1+1*h(3):n(3)+1*h(3) )
      endselect ! ishft
#ifdef FULL_DEBUG
      if(o>0) write(o,'(3A,2I5,A,2I5)') sym, fun, &
        'XY shift index range', (/i1, i2/)-h(1), ' -->', (/j1, j2/)-h(1)
        ! display with -h(1) makes it clearer
#endif

      selectcase( isft(1) )
      case( :-1 )
        ! negative shift
        i1 = n(1)+1*h(1)+isft(1)+1 ; i2 = n(1)+2*h(1)
        j1 = 1+0*h(1) ; j2 = j1+i2-i1
        ! translation iy+1, ix+1
        ph = phase(2)*phase(1)
        vec( i1:i2, iput+1:iput+h(2), 1+1*h(3):n(3)+1*h(3) ) = ph * &
        vec( j1:j2, iget+1:iget+h(2), 1+1*h(3):n(3)+1*h(3) )
      case( 0 )
        ! nothing to do
      case( 1: )
        ! positive shift
        i1 = 1+0*h(1) ; i2 = h(1)+isft(1)
        j2 = n(1)+2*h(1) ; j1 = j2-i2+i1
        ! translation iy+1
        ph = phase(2)
        vec( i1:i2, iput+1:iput+h(2), 1+1*h(3):n(3)+1*h(3) ) = ph * &
        vec( j1:j2, iget+1:iget+h(2), 1+1*h(3):n(3)+1*h(3) )
      endselect ! ishft
#ifdef FULL_DEBUG
      if( isft(1) /= 0 ) then
        if(o>0) write(o,'(3A,2I5,A,2I5)') sym, fun, &
          'XY shift index range', (/i1, i2/)-h(1), ' -->', (/j1, j2/)-h(1)
      endif ! isft <> 0
#endif

    else  ! matched
      ! arbitrary shifts are only possible with interpolation, not implemented
      stop 'shifted_y_boundary: shift does not match the grid, interpolation required.'
    endif ! matched

  endsubroutine shifted_y_boundary
! end of GENERAL_CELL
#endif


  endsubroutine ! dataexchange





#if R1_C2 == 1
  integer function copy_r( &
#else
  integer function copy_c( &
#endif
                           v, t, id, ise, v2t, n, h, f ) result( i0 )
  ! returns the number of elements copied
  implicit none
    ! parameter
    character(len=*), parameter           :: fun = ' copy: '
    integer, parameter                    :: IS = 1, IE = 2
    ! arguments
    REAPLEX, intent(inout)                :: v(1:,1:,1:), t(1:)
    integer, intent(in)                   :: id  ! direction
    integer, intent(in)                   :: ise ! start or end
    logical, intent(in)                   :: v2t
    integer, intent(in)                   :: n(3), h(3), f(3)
    ! local vars
    integer                               :: i1, i2, i3
    integer                               :: l(1:3,IS:IE) ! limits

#ifdef DEBUG
    if( ise /= IS .and. ise /= IE ) stop 'BND copy: ise must be 1 or 2.'
    if( id < 1 .or. id > 3 ) stop 'BND copy: direction id must be in [1,3].'
    if( any( h > n ) ) stop 'BND copy: halo must not be larger than central region.'
    if( any( f > h ) ) stop 'BND copy: # of slices to copy cannot be larger than halo width.'
    if( any( shape(v) /= n+2*h ) ) stop 'BND copy: shape of V muste be N+2*H'
#endif

    ! set the limits for the two other directions
    selectcase( id )
    case( 1 ) ! x direction
      l(2,IS) =    1+h(2)
      l(2,IE) = n(2)+h(2)
      l(3,IS) =    1+h(3)
      l(3,IE) = n(3)+h(3)
      ! (does not include the y and z halos)^*
    case( 2 ) ! y direction
      l(1,IS) =    1+h(1)-f(1)
      l(1,IE) = n(1)+h(1)+f(1)
      l(3,IS) =    1+h(3)
      l(3,IE) = n(3)+h(3)
      ! (includes the x halos, but not the z halos)^*
    case( 3 ) ! z direction
      l(1,IS) =    1+h(1)-f(1)
      l(1,IE) = n(1)+h(1)+f(1)
      l(2,IS) =    1+h(2)-f(2)
      l(2,IE) = n(2)+h(2)+f(2)
      ! (includes the x and y halos)^*
    case default ; stop 'BND copy: ID out of range [1,3].'
    endselect ! id
! ^*
! this way provides the full halo information
! including the spatial diagonal parts, because
! these are transferred via the cartesian neighbors:
! example in 2 spatial directions,
!    (c) stands for central region data,
!    (ab) is a highlighted data set
!
! 1st step: ( exchange in x direction )
!   ---------    ---------
!  |         |  |         |
!  |  ccccc  |  |  ccccc  |
!  |  ccccc  |  |  ccccc  |
!  |  cccccab<-----abccc  |
!  |         |  |         |
!   ---------    ---------
!   ---------
!  |         |
!  |  ccccc  |
!  |  ccccc  |
!  |  ccccc  |
!  |         |
!   ---------
!
! 2nd step: ( exchange in y direction )
!   ---------    ---------
!  |         |  |         |
!  |  ccccc  |  |  ccccc  |
!  |  ccccc  |  |  ccccc  |
!  |  cccccab|  |  abccc  |
!  |       | |  |         |
!   -------|-    ---------
!   -------v-
!  |       ab|
!  |  ccccc  |
!  |  ccccc  |
!  |  ccccc  |
!  |         |
!   ---------
!
! this means that the total volume of communication
! is given by the volume of the halo-enlarged array
! subtracting the volume of the central region.
!
!   V_comm = product( h+n+h ) - product( n )
!
! in units of data elements





! the beginning and the end of the halo-enlarged array looks like this
! (one dimensional pictogramms)
!        _______ ______________ ______________ _____________
!       |_______|______________|______________|_____________...
!       0      h-f             h             h+f
!    ___________ ______________ ______________ _______
! ...___________|______________|______________|_______|
!             h+n-f           h+n           h+n+f   h+n+h
!
! where h is the extend of the full halo,
!       f is the number of needed elements,
!       n is the length of the central region.
! thus the central data are stored between h and h+n
!
! in the following 1D pictogramms, R stands for read
!                                  W    for write

    if( v2t ) then 
      ! copy the content of the central region of the halo-enlarged
      ! array v into the temporary rank-1-array t


      selectcase( ise )
      case( IS )
!        _______ ______________ ______________ _____________
!       |_______|______________|______________|_____________
!       0      h-f             h             h+f
!                              |RRRRRRRRRRRRRR|
!
        l(id,IS) = h(id)+1
        l(id,IE) = h(id)+f(id)
      case( IE )
!    ___________ ______________ ______________ _______
!    ___________|______________|______________|_______|
!             h+n-f           h+n           h+n+f   h+n+h
!               |RRRRRRRRRRRRRR|
!
        l(id,IS) = n(id)+h(id)-f(id)+1
        l(id,IE) = n(id)+h(id)
#ifdef DEBUG
      case default ; stop 'BND copy: ISE should be either IS or IE! (v2t=true)'
#endif
      endselect ! ise

#ifdef FULL_DEBUG
!   if(o>0) write(o,'(3A,2I6,9A)') sym, fun, ' xlimits [', l(1,IS:IE), ' ]'
#endif

      i0 = 0 ! counter for elements and function result
      do i3 = l(3,IS), l(3,IE)
        do i2 = l(2,IS), l(2,IE)
          do i1 = l(1,IS), l(1,IE)
            i0 = i0+1
            t(i0) = v(i1,i2,i3)
          enddo ! i1
        enddo ! i2
      enddo ! i3

    else ! v2t
      ! copy the content of the temporary rank-1-array t
      ! into the halos of the halo-enlarged array v

      selectcase( ise )
      case( IS )
        l(id,IS) = h(id)-f(id)+1
        l(id,IE) = h(id)
!        _______ ______________ ______________ _____________
!       |_______|______________|______________|_____________
!       0      h-f             h             h+f
!               |WWWWWWWWWWWWWW|
!
      case( IE )
        l(id,IS) = n(id)+h(id)+1
        l(id,IE) = n(id)+h(id)+f(id)
!    ___________ ______________ ______________ _______
!    ___________|______________|______________|_______|
!             h+n-f           h+n           h+n+f   h+n+h
!                              |WWWWWWWWWWWWWW|
!
#ifdef DEBUG
      case default ; stop 'BND copy: ISE should be either IS or IE! (v2t=false)'
#endif
      endselect ! ise

#ifdef FULL_DEBUG
!   if(o>0) write(o,'(3A,2I6,9A)') sym, fun, ' xlimits [', l(1,IS:IE), ' ]'
#endif

      i0 = 0 ! counter for elements and function result
      do i3 = l(3,IS), l(3,IE)
        do i2 = l(2,IS), l(2,IE)
          do i1 = l(1,IS), l(1,IE)
            i0 = i0+1
            v(i1,i2,i3) = t(i0)
          enddo ! i1
        enddo ! i2
      enddo ! i3

    endif ! v2t

#ifdef FULL_DEBUG
    if(o>0) write(o,'(3A,3(2I4,A),I8,L2)') sym, fun, &
      'limits [', l(1,:)-h(1), '],[', l(2,:)-h(2), '],[', l(3,:)-h(3), '], noe =', i0, v2t
#endif
  endfunction ! copy















! 
! #if R1_C2 == 1
!   subroutine Xchange_r &
! #else
!   subroutine Xchange_c &
! #endif
!                            ( g, v, kp, nf )
!   use type_grid, only: grid
!   use type_grid, only: BC_INNER
!   use type_grid, only: BC_FINITE
!   use type_grid, only: BC_PERIODIC
!   use type_grid, only: BC_MIRROR
! #ifdef GENERAL_CELL
!   use type_grid, only: BC_SHIFTED
! #endif
!   use type_grid, only: BCD => BC_DESCRIPTION
!   use configuration, only: WARNING, ERROR, STOPONERROR
!   use type_kpoint, only: kpoint, kpoint_Gamma
!   use MPIconst, only: PREC, MPI_STATUS_SIZE
!   implicit none
! 
!     ! parameters
!     character(len=*), parameter     :: fun = ' Xchange: '
!     integer, parameter              :: DN = 1, UP = 2
!     ! arguments
!     type(grid), intent(in)          :: g !! the grid descriptor contains the boundary conditions g%bc(3,2), the number of grid points g%ng(1:4) and the halo thickness g%nh(1:3)
!     REAPLEX, intent(inout)          :: v(1:,1:,1:) !! dims(1:g%ng+2*g%nh)^3 <br> halo enlarged array
!     type(kpoint), intent(in), optional :: kp    !! kpoint for Bloch phase factor, default is the Gamma point
!     integer, intent(in), optional      :: nf(1:3) !! halo thickness that is needed, if it deviates from the halos of the array, this can save communication volume and time
!     ! local vars
!     integer                         :: mx, id, ist, ise, iget, iput
!     integer                         :: npnh(3) ! n+nh
! 
!     ! save variables
!     integer, save                   :: bc(1:3,DN:UP) ! technical boundary condition
!     integer, save                   :: h(1:3) = 0 ! not initialized
!     integer, save                   :: n(1:3) = 0 ! not initialized
!     integer, save                   :: f(1:3) = 0 ! not initialized
!     type(kpoint), save              :: akp ! actual kpoint
!     logical                         :: changed
!     REAPLEX, save                   :: phase(1:3,DN:UP)
!     REAPLEX, allocatable, save      :: t(:,:,:)
!     integer, save                   :: nsend(1:3,DN:UP) = 0 ! how many elements to send
!     integer, save                   :: nrecv(1:3,DN:UP) = 0 ! how many elements to send
!     logical, save                   :: warning_realnumbersandkpoint = .true.
! 
! #ifndef NOMPI
!     integer                         :: ireq(DN:UP), ifl, istat(MPI_STATUS_SIZE), ierr
! #endif
! 
! #ifdef DEBUG
!     integer                         :: u
! #endif
! 
! #ifdef DEBUG
!     ! check array dimensions
!     do id = 1, 3
!       if( size( v, id ) /= g%ng(id) + 2*g%nh(id) ) &
!         stop 'BND dataexchange: some dim of array V does not match the grid G.'
!     enddo ! id
! #endif
! 
! 
!     if( present(kp) ) then ; akp = kp
!     else                   ; akp = kpoint_Gamma( )
!     endif ! present(kp)
! 
! 
!     h(1:3)      = g%nh(1:3) ! halo thickness (upper == lower)
!     n(1:3)      = g%ng(1:3) ! grid numbers
!     f(1:3)      = h(1:3) ; if( present( nf ) ) f = min( max( 0, nf ), h )
! 
!     bc(1:3,DN:UP) = g%tbc(1:3,1:2) ! use the technical boundary condition
! 
!     ! number of elements to send:
!     ! for each direction, the number of elements is the area times the
!     ! halo thickness h. The formula for the area varies depending on
!     ! the direction, because like that, also the edges
!     ! (halo-halo overlap regions)_are transferred.
!     nsend(1,DN:UP) =   h(1)          * ( n(2)+0*h(2) ) * ( n(3)+0*h(3) )
!     nsend(2,DN:UP) = ( n(1)+2*h(1) ) *   h(2)          * ( n(3)+0*h(3) )
!     nsend(3,DN:UP) = ( n(1)+2*h(1) ) * ( n(2)+2*h(2) ) *   h(3)
! 
!     mx = maxval(nsend)
!     if( size( t, 1 ) < mx ) then
!       if(o>0) write(o,'(4A,9(I8,A))') sym, fun, 'enlarge temporary ', &
! #if R1_C2 == 1
!         'real array from', size(t,1), ' to', mx
! #else
!         'complex array from', size(t,1), ' to', mx
! #endif
!       if( allocated( t ) ) deallocate( t )
!       allocate( t( mx, 1:2, I_SEND:I_RECV ) )
!     endif ! size t < mx
! 
!     ! for all directions ( where the halo has a positive extend )
!     do id = 1, 3
!       if( h(id) < 1 ) cycle
! 
!       do ise = DN, UP
!         selectcase( bc(id,ise) )
!         case( BC_INNER )
!           phase(id,ise) = 1. ! between 2 domains, the information
!                              ! is just passed as it is
!         case( BC_FINITE )
!           phase(id,ise) = 0. ! wave functions and Hartree/Coulomb
!                              ! potential have to vanish in the vacuum
!           nsend(id,ise) = 0 ! no need for communication
!           nrecv(id,ise) = 0 ! no need for communication
! 
!         case( BC_MIRROR )
!           phase(id,ise) = 1. ! only 1.0 and -1.0 are allowed
!           nsend(id,ise) = 0 ! no need for communication
!           nrecv(id,ise) = 0 ! no need for communication
! 
!           if( present( kp ) ) then
!             akp = kp
! #ifdef DEBUG
!             if( abs(abs(real(kp%eik(id)))-1.)>1.E-7 .or. abs(aimag(kp%eik(id)))>1.E-7 ) then
!               if(o>0) write(o,'(3A,6F7.3,9A)') sym, fun, 'kpoint phase = [', kp%eik(1:3), ' ].'
!               stop 'BND k-point is not commensurate with mirror BC.'
!             endif ! k(id) == 0. or 0.5
! #endif
!             if( abs( real( kp%eik(id) ) ) == 1. ) then
!               phase(id,ise) = real( kp%eik(id) ) ! even or uneven mirror symmetry
!             else ; stop 'BND k-point phase factor is not commensurate with mirror BC.'
!             endif !
! 
!           endif ! present( kp )
! 
! 
! #ifdef GENERAL_CELL
!         case( BC_PERIODIC, BC_SHIFTED )
!           ! periodic and shifted boundary conitions
! #ifndef DEBUG
!           if( bc(id,ise) == BC_SHIFTED ) then
!             ! as long as the the shift boundary conditions are not fully debugged,
!             ! it will not run without -D DEBUG at least for this module.
!             if(o>0) write(o,'(9A)') sym, fun, WARNING(0), &
!               'shift boundary condition is maybe not bug-free.'
!           endif ! bc == BC_SHIFTED
! #endif
! 
! #else
!         case( BC_PERIODIC )
! #endif
!   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   !! PHASE CONVENTION
!   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   !! for the Bloch wave functions with a constant
!   !! lattice periodic part, a plane wave running into positive
!   !! direction is assumed i.e.
!   !!
!   !! Psi(x) = exp(ikx)
!   !!
!   !! there are 5 regions of the halo-enlarged vector:
!   !!
!   !!  |lw|lr|center|ur|uw| l:lower, upper, w:write, r:read
!   !!     |            |
!   !! exp(-ikL/2)  exp(ikL/2)
!   !!
!   !! let''s consider the lower boundary (ise==1)
!   !! uw = lr * phase(id,1) = exp(-ikL/2) * exp(ikL)
!   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!           phase(id,ise) = 1. ! any phase with |phase|^2 = 1. is allowed
!           if( present( kp ) ) then
!             akp = kp
!             ! prepare the phase factor for the k-point
!             if( ise == DN ) then
!               phase(id,DN) = kp%eik(id)
!             else ! ise == 1
!               phase(id,UP) = 1./kp%eik(id)
!             endif ! ise == 1 or 2
! #ifdef DEBUG
! #if R1_C2 == 1
! ! REAL only
!             if( aimag( kp%eik(id) ) /= 0. ) then
!               if( warning_realnumbersandkpoint ) then
!                 if(o>0) write(o,'(9A)') sym, fun, WARNING(0), &
!                   'kpoint present with real numbers and periodic boundary conditions.'
!                 warning_realnumbersandkpoint = .false. ! disable further warnings
!               endif ! warning_realnumbersandkpoint
!             endif ! aimag(eik) /= 0.
! #endif
! #endif
!           endif ! present(kp)
! 
!         case default
! #ifdef DEBUG
!           if(o>0) write(o,'(9A)') sym, fun, ERROR, &
!             'boundary condition switch in ', DIR(id), '-direction is NOT DEFINED.'
!           if( STOPONERROR ) &
! #endif
!           stop 'boundary: BC not implemented.'
!         endselect ! bc
! 
!       enddo ! ise
!     enddo ! id
! 
! #ifdef DEBUG
!     ! display the boundary conditions
!     if( displayboundaryconditions .and. o>0 ) then
!       write(o,'(A)') '', ' Boundary conditions: ' ! empty line + head line
!       if( any( g%nproc > 1 ) ) &
!       write(o,'(A)')     '             physical  technical'
!       write(o,'(A)')     ' -------------------------------' ! line
!       do id = 1, 3 ! for all directions
!         do ise = DN, UP ! for both boundaries
!           if( g%nproc(id) > 1 ) then                              ! physical BC              ! technical BC
!             write(o,'(9A)') '    ', DIR(id), ': ', UPDN(ise), '  ', BCD( g%bc(id,ise) ), '  ', BCD( bc(id,ise) )
!           else  ! nproc > 1
!             write(o,'(9A)') '    ', DIR(id), ': ', UPDN(ise), '  ', BCD( g%bc(id,ise) )
!           endif ! nproc > 1
!         enddo ! ise
!       enddo ! id
!       write(o,'(A)')    ' -------------------------------', '' ! line + empty line
!       displayboundaryconditions = .false. ! this will be displayed only once.
!     endif ! displayboundaryconditions
! #endif
! 
! 
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   do id = 1, 3 ! X, Y and Z-direction
!     if( h(id) < 1 ) cycle ! halo size must be larger than 0
! 
!     iput = 0 ! init iput=0 for the lower boundary (ise==1)
!     do ise = DN, UP
! 
!       selectcase( bc(id,ise) )
!       case( BC_FINITE )
!         ! set the halos to zero, so no data has to be copied
! #ifdef FULL_DEBUG
! !         if(o>0) write(o,'(9A)') sym, fun, 'set halo to zero in ', DIR(id), '-direction.'
! #endif
!         selectcase( id )
!         case( 1 ) ; v( iput+1:iput+h(1), :, : ) = 0.
!         case( 2 ) ; v( :, iput+1:iput+h(2), : ) = 0.
!         case( 3 ) ; v( :, :, iput+1:iput+h(3) ) = 0.
!         endselect
! 
!       case( BC_INNER, BC_PERIODIC )
!         ! send without phase factors (in between domains) or
!         ! multiply the halos with the phase 
!         ! factor eikL of the k-Point (if present)
!         ! else with 1.0
!                       ! copy  v --> t
!         nsend(id,ise) = copy( v, t(:,ise,I_SEND), id, ise, v2t=.true., n=n, h=h, f=f )
!         nrecv(id,ise) = nsend(id,ise)
! 
!         ! apply the phase factor to the part that will be sent
! !         if( phase(id,ise) /= (1.0,0.0) ) &
!         t(:,ise,I_SEND) = t(:,ise,I_SEND) * phase(id,ise)
! 
! !         if( phase(id,ise) /= 1. ) then
! !           if(o>0) write(o,'(3A,I1,A,I1,A,2F7.3)') sym, fun, &
! !             'multiply phase(id=', id, ',ise=', ise, ') =', phase(id,ise)
! !             t(:,ise,I_SEND) = t(:,ise,I_SEND) * phase(id,ise)
! !         endif ! phase /= 1.
! !       case( BC_MIRROR ) ; call mirror_boundary( v, real(phase(id,ise)), id, ise, n, h ) ! copy without using halos
!       case( BC_MIRROR ) ; stop 'mirror not implemented again'
!       endselect ! bc
! 
!     iput = n(id) + h(id) ! set iput to n+h for the upper boundary (ise==2)
!     enddo ! ise
! 
! 
! 
! 
!     ! if any if the ksend-switches has been turned on,
!     ! there will be communication right now
!     do ise = DN, UP
!       if( nsend(id,ise) > 0 .or. nrecv(id,3-ise) > 0 ) then
! 
! #ifndef NOMPI
! ! MPI part
! 
! #ifdef FULL_DEBUG
!         if(o>0) write(o,'(2A,5(A,I6),A,I2,9A)') sym, fun, &
!           'SendRecv me#', g%rank, &
!           ' send', nsend(id,ise), ' to#', g%neighbor(id,ise), &
!           ' get', nrecv(id,3-ise), ' from#', g%neighbor(id,3-ise), &
!           ' ise=', ise, ' direction=', DIR(id)
! #endif
! 
!         if( g%rank /= g%neighbor(id,ise) ) then
!           ifl = ise ! flag
!           call MPI_Sendrecv( t(:,ise,I_SEND), nsend(id,ise), &
!                              PREC( R1_C2 ), g%neighbor(id,ise), ifl,  &
!                              t(:,3-ise,I_RECV), nrecv(id,3-ise), &
!                              PREC( R1_C2 ), g%neighbor(id,3-ise), ifl,  &
!                              g%comm, istat, ierr )
! #ifdef FULL_DEBUG
!           if( ierr /= 0 ) then
!             if(o>0) write(o,'(3A,I3)') sym, fun, 'after SendRecv ierr =', ierr
!           endif
! #endif
!         else  ! me == my neighbor
! ! end MPI part
! #else
!         if( .true. ) then ! always
! #endif
!           t(:,3-ise,I_RECV) = t(:,ise,I_SEND) ! ise==1: 2 --> 1, ise==2: 1 --> 2
!         endif ! ...
! 
!       endif ! ksend(id,ise) == 1
!     enddo ! ise
! 
! 
! 
!     do ise = DN, UP
!       selectcase( bc(id,ise) )
!       case( BC_INNER, BC_PERIODIC )
! !         if(o>0) write(o,'(9A)') sym, fun, 'set halo to recv buffer in ', DIR(id), '-direction.'
! 
!         ! write the data of the receive buffer into halos of v
!                       ! copy  v <-- t
!         nsend(id,ise) = copy( v, t(:,ise,I_RECV), id, ise, v2t=.false., n=n, h=h, f=f )
! 
!       endselect ! bc
!     enddo ! ise
! 
!   enddo ! id = 1, 3
!   ! end of XYZ-direction
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
! 
!   return
!   endsubroutine ! Xchange













! tail of the module
#if R1_C2 == 2



  logical function test_dataexchange( g ) result( ok )
  ! most important self-test for the domain decomposition
  use configuration, only: o
  use type_grid, only: grid, set, BC_PERIODIC, BC_FINITE, BC_INNER, BC_MIRROR
  implicit none
    ! parameter
    character(len=*), parameter     :: fun = ' test_dataexchange: '
    integer, parameter              :: ACT=1, CMP=2 ! act and compare
    real, parameter                 :: ERROR_VALUE = -.666
    ! arguments
    type(grid), intent(in)          :: g
    ! local vars
    real, allocatable   :: v(:,:,:,:)
    integer             :: ise, id, i1, i2, i3, nall, nerr, ierr
    integer*8           :: itotal, i0, i00, i000

#ifdef FULL_DEBUG
    character(len=*), parameter :: FRMT = '(8I4,I6,7I4)'
    if(o>0) write(o,'(3A,6I3,9(I12,2I6))') sym, fun, 'tbc,ng_all,nh,ng,ioff', g%tbc, g%ng_all(1:3), g%nh, g%ng(1:3), g%ioff
#endif
    ok = .false. ! default on early return

    do ise = 1, 2
      do id = 1, 3
        selectcase( g%tbc(id,ise) )
        case( BC_PERIODIC, BC_INNER ) ! nothing
        case( BC_FINITE )
        case( BC_MIRROR ) ; if( ise == 2 ) stop 'BND test_dataexchange: Mirror BC only at lower boundary!'
        case default ; if(o>0) write(o,'(9A)') sym, fun, 'only periodic, finite and mirror BC implemented' ; return
!           stop 'BND test_dataexchange: only periodic, finite and mirror BC implemented.'
        endselect ! g%bc
      enddo ! id
    enddo ! ise

    ! create 2 arrays with halos to each side
    allocate( v(1-g%nh(1):g%ng(1)+g%nh(1), &
                1-g%nh(2):g%ng(2)+g%nh(2), &
                1-g%nh(3):g%ng(3)+g%nh(3),ACT:CMP) )

    ! set up the index data with periodic/inner BC (the modulo function takes care of the periodicity)
    do i3 = 1-g%nh(3), g%ng(3)+g%nh(3) ; i000 = modulo( i3-1 + g%ioff(3), g%ng_all(3) )*g%ng_all(1)*g%ng_all(2)*1
      do i2 = 1-g%nh(2), g%ng(2)+g%nh(2) ; i00 = modulo( i2-1 + g%ioff(2), g%ng_all(2) )*g%ng_all(1)*1
        do i1 = 1-g%nh(1), g%ng(1)+g%nh(1) ; i0 = modulo( i1-1 + g%ioff(1), g%ng_all(1) )*1
          itotal = 1 + i0 + i00 + i000 ! compute the total index in the entire grid
          ! the conversion to real is needed, because the interface dataexchange is only real and complex, no integer
          v(i1,i2,i3,CMP) = real( itotal )
        enddo ! i1
      enddo ! i2
    enddo ! i3

#ifdef FULL_DEBUG
    if(o>0) write(o,'(9A)') sym, fun, 'show v(:,:,:,CMP)'
    if(o>0) write(o,fmt=FRMT) nint(v(:,:,:,CMP))
#endif

    ! modify other mirror boundary conditions, mirror only at the lower boundary
    if( g%tbc(1,1) == BC_MIRROR ) v(0:1-g%nh(1):-1,:,:,CMP) = v(1:g%nh(1),:,:,CMP)
    if( g%tbc(2,1) == BC_MIRROR ) v(:,0:1-g%nh(2):-1,:,CMP) = v(:,1:g%nh(2),:,CMP)
    if( g%tbc(3,1) == BC_MIRROR ) v(:,:,0:1-g%nh(3):-1,CMP) = v(:,:,1:g%nh(3),CMP)

    ! modify other finite boundary conditions
    if( g%tbc(1,1) == BC_FINITE ) v(:0,:,:,CMP) = 0.
    if( g%tbc(1,2) == BC_FINITE ) v(g%ng(1)+1:,:,:,CMP) = 0.
    if( g%tbc(2,1) == BC_FINITE ) v(:,:0,:,CMP) = 0.
    if( g%tbc(2,2) == BC_FINITE ) v(:,g%ng(2)+1:,:,CMP) = 0.
    if( g%tbc(3,1) == BC_FINITE ) v(:,:,:0,CMP) = 0.
    if( g%tbc(3,2) == BC_FINITE ) v(:,:,g%ng(3)+1:,CMP) = 0.

#ifdef FULL_DEBUG
    if(o>0) write(o,'(9A)') sym, fun, 'show v(:,:,:,CMP) adjusted to the BCs'
    if(o>0) write(o,fmt=FRMT) nint(v(:,:,:,CMP))
#endif

    v(:,:,:,ACT) = ERROR_VALUE ! initialize the halos with a non-zero, but non-integer value,
                               ! so at the end, errors can be counted
    ! set values only in the center region
    v(1:g%ng(1),1:g%ng(2),1:g%ng(3),ACT) = v(1:g%ng(1),1:g%ng(2),1:g%ng(3),CMP)
#ifdef FULL_DEBUG
    if(o>0) write(o,'(9A)') sym, fun, 'show v(:,:,:,ACT) before data_exchange:'
    if(o>0) write(o,fmt=FRMT) nint(v(:,:,:,ACT))
#endif
    call dataexchange( g, v(:,:,:,ACT) )
#ifdef FULL_DEBUG
    if(o>0) write(o,'(9A)') sym, fun, 'show v(:,:,:,ACT) after  data_exchange:'
    if(o>0) write(o,fmt=FRMT) nint(v(:,:,:,ACT))
#endif

    ! number of halo points = number of all points - central region
    nall = product( g%ng(1:3)+2*g%nh(1:3) ) - product( g%ng(1:3) )

    ! percentages are not parallelized, but process local

    nerr = count( v(:,:,:,ACT) == ERROR_VALUE )
    if( nerr > 0 ) then
      if(o>0) write(o,'(3A,F7.3,9A)') sym, fun, &
       'failed,', 100.*real(nerr)/real(nall), ' % values where not assigned.'
    endif ! nerr

    ! compare, if the inner region has stayed unchanged
    nerr = count( v(1:g%ng(1),1:g%ng(2),1:g%ng(3),ACT) /= v(1:g%ng(1),1:g%ng(2),1:g%ng(3),CMP) )
    if( nerr > 0 ) stop 'BND test_dataexchange: FATAL ERROR, routine data_exchange modified central region!'

    nerr = count( v(:,:,:,ACT) /= v(:,:,:,CMP) )
    if( nerr > 0 ) then
      if(o>0) write(o,'(3A,F6.1,9(A,I0))') sym, fun, 'failed,', 100.*real(nerr)/real(nall), &
        ' % indices were transferred incorrectly ( ', nerr, ' of ', nall, ' ).'
      return
    else
      ok = .true.
    endif ! nerr > o
#ifndef NOMPI
    call MPI_Barrier( g%comm, ierr )
#endif
    deallocate( v, stat=ierr )
  endfunction test_dataexchange


  integer function test() result( ist )
    ist = 0
#ifndef EXTENDED
!+ not extended 
  endfunction ! test
!- not extended
#else
!+ extended
    call test_visual( )
  endfunction ! test

  subroutine test_visual( )
  use type_grid, only: grid, set, BC_PERIODIC
    character(len=*), parameter :: fun = ' test_visual: '
    integer, parameter :: o=6 ! output to std
    integer             :: bc(3,2) = BC_PERIODIC
    type(grid)          :: g
    real, allocatable   :: v(:,:,:)
    character(len=80)   :: fmrt
    integer             :: i1, i2, i3, ii, ist

    g = set( (/1.,1.,1./), (/2,3,4/), 1, bc, name='test_grid' )
    g%nf = 1
    g%nh = g%nf

    allocate( v(1-g%nh(1):g%ng(1)+g%nh(1), &
                1-g%nh(2):g%ng(2)+g%nh(2), &
                1-g%nh(3):g%ng(3)+g%nh(3)), stat=ist )
    v = 0.
    ii = 0
    do i3 = 1, g%ng(3)
      do i2 = 1, g%ng(2)
        do i1 = 1, g%ng(1)
          ii = ii+1
          v(i1,i2,i3) = real( ii )
    enddo ; enddo ; enddo ! i123


    ! prepare format for display
    write(fmrt,fmt='(3(A,I3))') &
      '(A,', g%ng(2)+2*g%nh(2),'(', g%ng(1)+2*g%nh(1),'I3,A))'

    write(o,fmt='(A)') '', 'before', ''
    do i3 = 1-g%nf(3), g%ng(3)+g%nf(3)
      write(o,fmt=fmrt) '  |', ( nint(v(:,i2,i3)), '  |', i2=1-g%nh(2),g%ng(2)+g%nh(2) )
    enddo ! i3
    call dataexchange( g, v )
    write(o,fmt='(A)') '', 'after', ''
    do i3 = 1-g%nf(3), g%ng(3)+g%nf(3)
      write(o,fmt=fmrt) '  |', ( nint(v(:,i2,i3)), '  |', i2=1-g%nh(2),g%ng(2)+g%nh(2) )
    enddo ! i3
    deallocate( v, stat=ist )

    call test_pw( )
  endsubroutine ! test_visual

  subroutine test_pw( )
  use type_grid, only: grid, set, BC_PERIODIC
  use type_kpoint, only: kpoint, kpoint_set
    character(len=*), parameter     :: fun = ' test_pw: '
    complex, parameter  :: im = (0.0,1.0)
    integer, parameter  :: N = 8
    integer, parameter  :: o=6 ! output to std
    integer             :: bc(3,2) = 0 
    type(grid)          :: g
    type(kpoint)        :: kp
    complex             :: v(-1:10,1:1,1:1)
    real                :: x, pi
    integer             :: i
    pi = acos(-1.0)
    kp = kpoint_set( (/0.5,0.,0./) )
    bc(1,:) = BC_PERIODIC
    g = set( (/1.,1.,1./), (/8,1,1/), 1, bc, name='grid_pwx' )
    g%nf(:) = 0 ; g%nf(1) = 2
    g%nh = g%nf
    v = 0.
    do i = 1, g%ng(1)
      x = (i-0.5)/real(N)-0.5
      v(i,1,1) = kp%eik(1)**x
    enddo ! i

    write(o,fmt='(A)') '', 'before', ''
    write(o,fmt='(24F6.2)') v
    write(o,fmt='(12F12.6)') aimag( log( v ) )/pi
    call dataexchange( g, v, kp )

    write(o,fmt='(A)') '', 'after', ''
    write(o,fmt='(24F6.2)') v
    write(o,fmt='(12F12.6)') aimag( log( v ) )/pi
  endsubroutine ! test_pw

  subroutine test2( )
    character(len=*), parameter :: fun = ' test_copy_function: '
    character(len=*), parameter :: V2T_LABEL(1:2) = (/ 'v -> t', 't -> v' /)
    integer, parameter  :: o=6 ! output to std
    integer             :: n(3) = 6
    integer             :: h(3) = 3
    integer             :: f(3) = 2
    real                :: v(12,12,12), t(432)
    integer             :: nsent, id, ise, i12, nsent_all=0

    write(o,'(A)') '', ''
    write(o,'(A,3I3,A,3I2,A,3I2)') 'TEST copy(): ng =', n, ' halo =', h, ' used =', f
    write(o,'(A)') '----------------------------------------------------- '

    do i12 = 1, 2
      write(o,'(A,I2)') V2T_LABEL( i12 )
      do id = 1, 3
        write(o,'(9A)') 'direction id =', DIR(id)
        do ise = 1, 2

          nsent = copy( v, t, id=id, ise=ise, v2t=(i12==1), n=n, h=h, f=f )
          write(o,'(A,I2,A,I6)') '   ise =', ise, '  nsent =', nsent
          nsent_all = nsent_all + nsent

        enddo ! ise
      enddo ! id
    enddo ! i12

    write(o,'(A)') '----------------------------------------------------- '
    write(o,'(A,I12)') ' total ammount of communication =', nsent_all/2
    write(o,'(A,I12)') '  volume of (used)   halos       ', product( n+2*f ) - product( n )
  endsubroutine ! test2

!- extended
#endif
 
endmodule ! boundary
#endif
#endif
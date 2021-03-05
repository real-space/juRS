#include "config.h"

! #define DEBUG
! #define FULL_DEBUG

#ifdef DEBUG
#define cDBG
#else
#define cDBG !DBG
#endif


! #define EXTRA_COMMUNICATORS

!! @author Paul Baumeister
!! @version 3.0
!!
!! this module takes care of the MPI environment.
!! It set the different levels of communicators
!! and parallelizes the number grid into domains
module communicators
implicit none
  private ! default for this module namespace

  ! public
  public :: comm_init
  public :: dimensions
  public :: atom_order
  public :: parallelize_tasks
  public :: comm_finalize
#ifdef EXTENDED
  public :: test
#endif
  
  real, save, private :: start_time !! will be set when comm_init is called, so the total time can be displayed by comm_finalize

  character(len=*), parameter, private :: sym = 'COM' !! module symbol
  character(len=*), parameter, private :: fun = ': ' !! function symbol

  contains

  MPI_Comm function comm_init( ) result( comm )
!   use configuration, only: CARTESIAN_MAPPING, STOPONERROR
!   use configuration, only: o !! output unit 0: no output, 6:stdout
!   use configuration, only: ERROR
  use MPIconst, only: MPI_COMM_WORLD, Wtime
  ! use, only vars: real, save :: start_time
    logical, save :: comm_initialized = .false.
#ifndef NOMPI
    status_t :: ierr
    if( .not. comm_initialized ) then
      call MPI_Init( ierr )
      if( ierr /= 0 ) stop 'COM error calling MPI_Init!'
      comm_initialized = ( ierr == 0 )
    endif ! not initialized
#endif
    ! set the module variable to the time, when MPI_Init is called. 
    ! Will be used, when comm_finalize is called to display the total time
    start_time = Wtime( ) ! start the stopwatch
    comm = MPI_COMM_WORLD ! result
  endfunction ! comm_init

  integer function div( nom, den )
    integer, intent(in) :: nom, den
    if( den < 1 ) stop 'COM div: denominator < 1'
    div = (nom+den-1)/den
  endfunction ! div

  !! main routine of this module:
  !!
  !! distributes the parallelization using a 7dimensional
  !! cartesian communicator structure:
  !!
  !! 0: INDEPENDENT
  !! 1: KPOINTS
  !! 2: SPINS (collinear)
  !! 3: BANDS (not active)
  !! 4: Z-DIRECTION
  !! 7: Y-DIRECTION
  !! 6: X-DIRECTION
  !! 
  status_t function dimensions( comm, bc, g, global, nxyz_proc, checkmode, &
                         indep_nproc, indep_comm ) result( ierr )
  use configuration, only: o !! output unit 0: no output, 6:stdout
  use configuration, only: WARNING, ERROR, STOPonERROR
  use configuration, only: ParallelizeSpace
  use configuration, only: CartesianMapping
  use configuration, only: ParallelizeBands
  use configuration, only: ParallelizeSpins
  use configuration, only: ParallelizeKpnts
  use type_grid, only: grid
  use type_grid, only: is_periodic
  use type_grid, only: parallelize_grid
  use type_info, only: info
  use MPIconst, only: MPI_COMM_SELF
  use MPItools, only: operator(.MPImax.)
    MPI_Comm, intent(in)      :: comm !! world communicator from comm_init, see above
    integer, intent(in)       :: bc(1:3) !! boundary conditions, keys are defined in the <a href="type_grid.html">grid module</a>
    type(grid), intent(inout) :: g !! grid descriptor for the coarsest grid
    type(info), intent(inout) :: global !! global information container
    integer, intent(in)       :: nxyz_proc(3) !! spatial parallelisation (number of domains in x,y,z)
    integer, intent(in)       :: checkmode !! 0:run, no CheckMode, >0: pretend to run with _checkmode_ processes

    integer, intent(inout), optional :: indep_nproc
    MPI_Comm, intent(out), optional  :: indep_comm

    integer, parameter :: I_INDEP=0, I_KPNTS=1, I_SPINS=2, I_BANDS=3, I_CELLZ=4, I_CELLY=5, I_CELLX=6
    character, parameter :: P_LEVEL(0:6) = (/'I','K','S','B','Z','Y','X'/)
cDBG  character(len=*), parameter     :: fun = ' dimensions: '

    logical  :: reorder(0:6)
    logical  :: periodic(0:6)
    logical  :: remain(0:6)
    integer  :: nxyz(3), nxyz_all ! spatial domain decompositon
    integer  :: ngrid_procs(0:6)=1, nprocs_left=1, nall_procs=1, id, ios
    MPI_Comm :: cart_all_comm
    MPI_Comm :: cart_indp_comm
    MPI_Comm :: cart_equi_comm
    integer  :: equi_rnk(3)=0, me_equi=0, rnk_ksb(3)=0
    MPI_Comm :: cart_cell_comm
    integer  :: cell_rnk(3)=0, me_cell=0, rnk_zyx(3)=0, neigh_rnk(3)=0
    integer  :: grid_proc_size(3), grid_my_coords(3)
    logical  :: periods(3)
#ifdef EXTRA_COMMUNICATORS
    MPI_Comm :: cart_kpt_comm
    integer  :: kpt_rank
    MPI_Comm :: cart_spn_comm
    integer  :: spn_rank
    MPI_Comm :: cart_bnd_comm
    integer  :: bnd_rank
    logical  :: remain3(3)
#endif
    integer  :: neighbor(3,2)=0
    real     :: loadbalance

      ! filter input: number of processes in the domain decompositon
      nxyz = 1 ; if( ParallelizeSpace ) nxyz = max( 1, nxyz_proc )

      reorder  = .true. ! allow the MPI Library to reorder the processes

      periodic = .false. ! init as non-periodic
      ! set those dimension to periodic that have physical periodic boundary conditions
      do id = 1, 3
        ! for each spatial direction
        ! id      7-id
        !========================
        ! id=1 ==> 6 == I_CELLX
        ! id=2 ==> 5 == I_CELLY
        ! id=3 ==> 4 == I_CELLZ
        periodic(7-id) = is_periodic( bc(id) )
      enddo ! id

cDBG  if(o>0) write(o,'(3A,9(I0,A))') sym, fun, '[nk,ns,nb] =[ ',global%nkpoints,', ',global%nspins,', ',global%nbands,' ]'

      if( present( indep_comm ) ) indep_comm = MPI_COMM_SELF ! init
      ierr = 0 ! init result

#ifndef NOMPI
! MPI part

      ! nprocs_left is the number of all processes running
      call MPI_Comm_size( comm, nall_procs, ierr )
      if( ierr /= 0 ) stop 'COM error calling MPI_Comm_size!'

      if( checkmode > 0 ) then
        ! replace the number by a fictitious number
        nall_procs = checkmode
        if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'Checkmode: pretend to distribute ',checkmode,' process elements.'
      endif ! checkmode > 0

      nprocs_left = nall_procs

      ngrid_procs(:) = 1 ! init all as 1


      if( present( indep_nproc ) ) then
        if( indep_nproc > nprocs_left ) indep_nproc = 1 ! disable INDEP parallelization
      ! ===============================================================================
      ! === independent tasks =========================================================
      ! ===============================================================================
        ngrid_procs(I_INDEP) = indep_nproc
        ! reduce the number of processes that are left to be distributed
        nprocs_left = nprocs_left/ngrid_procs(I_INDEP) ! integer divide
      endif ! present indep_nproc

      ! 3 dimensional spatial domain decomposition
      nxyz_all = product( nxyz(1:3) )
      if( nxyz_all > 1 ) then
cDBG    if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'a domain decomposition group consists of ',nxyz_all,' process elements.'
        if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'domain decomposition ',nxyz(1),' x ',nxyz(2),' x ',nxyz(3)
      else  ! nxyz_all > 1
        if(o>0) write(o,'(9A)') sym, fun, 'no domain decomposition.'
      endif ! nxyz_all > 1

      if( modulo( nprocs_left, nxyz_all ) /= 0 ) then
        if(o>0) write(o,'(3A,I0,A)') &
          sym, fun, 'unable to divide an independent set of PEs with ', nprocs_left, ' processes', &
          sym, fun, ' ...   into groups of ', nxyz_all, ' domains.'
        if( checkmode > 0 ) then
          if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'DomainDecomposition failed!'
        else  ! checkmode
          stop 'COM dimensions: number not divisible'
        endif ! checkmode
      endif ! cannot be divided

      ngrid_procs(I_CELLZ:I_CELLX) = nxyz(3:1:-1)
      ! reduce the number of processes that are left to be distributed
      nprocs_left = nprocs_left/nxyz_all


      ! ===============================================================================
      ! === kpoints ===================================================================
      ! ===============================================================================
      if( ParallelizeKpnts .and. global%nkpoints > 1 ) then
        if( modulo(nprocs_left,global%nkpoints) == 0 ) then  ! n > np
          ! best case: np >= nk and np is a multiple of nk
          ngrid_procs(I_KPNTS) = global%nkpoints ! possible further (band-)parallelization
          if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'Parallelize k-points in ',ngrid_procs(I_KPNTS),' sets (maximal).'
        elseif( nprocs_left < global%nkpoints ) then ! the case == cannot occur !
          ! each process gets some kpoints
          ngrid_procs(I_KPNTS) = nprocs_left
          if( o>0 .and. ngrid_procs(I_KPNTS) > 1 ) then
            if( modulo(global%nkpoints,nprocs_left) == 0 ) then
              write(o,'(3A,9(I0,A))') sym, fun, 'Parallelize k-points in ',ngrid_procs(I_KPNTS),' sets (optimal).'
              ! load balance should be close to 100%
            else  ! number divisible
              write(o,'(3A,9(I0,A))') sym, fun, 'Parallelize k-points in ',ngrid_procs(I_KPNTS),' sets.'
              ! load balance is defined by n-/n+ and load imbalance should be defined by (n+/n-)-1
              loadbalance = load_balance( global%nkpoints, ngrid_procs(I_KPNTS) )
              if( loadbalance < 0.7501 ) then ! loadbalance of 3/4 = 0.75
                write(o,'(4A,9(F0.1,A))') sym, fun, WARNING(0), 'expected k-point load balance is ', loadbalance*100., '%'
              else  ! loadbalance
                write(o,'(3A,9(F0.1,A))') sym, fun, 'Expected k-point load balance ', loadbalance*100., '%'
              endif ! loadbalance low
            endif ! number divisible
          endif ! np > 1
        else  ! ...
          ngrid_procs(I_KPNTS) = 1
          if(o>0) write(o,'(9A)') sym, fun, 'no k-points parallelized.'
        endif ! ...
        ! reduce the number of processes that are left to be distributed
        nprocs_left = nprocs_left/ngrid_procs(I_KPNTS)
      else ; ngrid_procs(I_KPNTS) = 1
      endif ! ParallelizeKpnts and nkpoints > 1


      ! ===============================================================================
      ! === collinear spins ===========================================================
      ! ===============================================================================
      if( ParallelizeSpins .and. global%nspins == 2 ) then
        if( modulo(nprocs_left,2) == 0 ) then
          ngrid_procs(I_SPINS) = 2
          if(o>0) write(o,'(9A)') sym, fun, 'Parallelize collinear spins.'
          ! reduce the number of processes that are left to be distributed
          nprocs_left = nprocs_left/ngrid_procs(I_SPINS)
        else ; ngrid_procs(I_SPINS) = 1
        endif ! n > np
      else ; ngrid_procs(I_SPINS) = 1
      endif ! ParallelizeSpins and nspins == 2


      ! ===============================================================================
      ! === bands =====================================================================
      ! ===============================================================================
      if( ParallelizeBands .and. nprocs_left > 1 ) then
        if( global%nbands >= nprocs_left ) then
          ngrid_procs(I_BANDS) = nprocs_left
          if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'Parallelize bands in ',ngrid_procs(I_BANDS),' sets.'
          ! reduce the number of processes that are left to be distributed
          nprocs_left = nprocs_left/ngrid_procs(I_BANDS) ! integer divide
        elseif( global%nbands < 0 ) then
          ! automatic number of bands, will be determined later
          ngrid_procs(I_BANDS) = nprocs_left ! take all
          nprocs_left = 1
          if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'Parallelize bands in ',ngrid_procs(I_BANDS),' sets (auto).'
        else ; ngrid_procs(I_BANDS) = 1
        endif ! n > np
      else ; ngrid_procs(I_BANDS) = 1
      endif ! ParallelizeBands and nbands > 1

      ! === show ======================================================================
      if(o>0) write(o,'(2A,9A5)') sym, fun,     P_LEVEL(6:0:-1)
      if(o>0) write(o,'(2A,9I5)') sym, fun, ngrid_procs(6:0:-1)
      ! ===============================================================================

      if( checkmode > 0 ) then

        ! === set up FAKE communicators in checkmode ======================================
        ierr = parallelize_grid( g, nxyz_proc, cart_cell_comm, me_cell, cell_rnk, neighbor )

        ! === warn ======================================================================
        if( nprocs_left > 1 ) then
          if(o>0) write(o,'(4A,I0,9A)') sym, fun, WARNING(0), &
            'after distribution, blocks of ',nprocs_left,' process elements have the same task.'
        elseif( nprocs_left < 1 ) then
          if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'not enough processes for domain decomposition.'
        endif !

        if( nall_procs < product(ngrid_procs) ) then
          if(o>0) write(o,'(4A,9(I0,A))') sym, fun, WARNING(0), &
         & 'not enough processes, please use at least ',nxyz_all,' or some multiple of that.'
        elseif( nall_procs > product(ngrid_procs) ) then
          if(o>0) write(o,'(4A,I0,9A)') sym, fun, WARNING(0), 'too many processes, please use ',product(ngrid_procs),' processes!'
        endif !
        global%band_comm = MPI_COMM_SELF

        ! in CheckMode, just set the parallelized numbers nbnd, nspn and nkpt
        global%nbnd = div( global%nbands,   ngrid_procs(I_BANDS) )
        global%nspn = div( global%nspins,   ngrid_procs(I_SPINS) )
        global%nkpt = div( global%nkpoints, ngrid_procs(I_KPNTS) )
cDBG    if(o>0) write(o,'(9A)') sym, fun, 'CheckMode ==> return'
        return
      endif ! checkmode


      if( nall_procs /= product(ngrid_procs) ) then
        if(o>0) write(o,'(3A,9(I0,A))') sym, fun, ERROR, &
          nall_procs,' processes running, but expected ',product(ngrid_procs),' processes!'
        stop 'COM dimensions: distribution failed.'
      endif ! nprocs_left /= 1

      if( nprocs_left /= 1 ) then
        if(o>0) write(o,'(4A,I0,9A)') sym, fun, ERROR, &
          'after distribution, blocks of ',nprocs_left,' process elements have the same task.'
        stop 'COM dimensions: factorization failed.'
      endif ! nprocs_left /= 1


      ! ==================================================
      ! === cartesian all communicator ===================
      ! === 7 dim ========================================
      ! ==================================================

      ! create a cartesian communicator for all processes
      call MPI_Cart_create( comm, 7, ngrid_procs, periodic, reorder, cart_all_comm, ierr )
      if( ierr /= 0 ) stop 'COM error calling MPI_Cart_create!'
      call display_communicator( o, name='cart_all_comm', cartcomm=cart_all_comm, ndim=7, level=P_LEVEL(0:6) )

!       call MPI_Comm_rank( cart_all_comm, me_all, ierr )
!       call MPI_Cart_coords( cart_all_comm, me_all, 7, all_coords, ierr )
! cDBG  if(o>0) write(o,'(3A,I8,A,9I4)') sym, fun, ' all: rank', me_all, ' coords', all_coords(6:0:-1)

    if( present( indep_nproc ) ) then

      ! ==================================================
      ! === independent sets communicator ================
      ! === 1 dim ========================================
      ! ==================================================

      remain = .false. ; remain(I_INDEP) = .true. ! mask
      call MPI_Cart_sub( cart_all_comm, remain, cart_indp_comm, ierr ) ! sub communicator
      if( ierr /= 0 ) stop 'COM error calling MPI_Cart_sub! (indep)'
      call display_communicator( o, name='cart_indp_comm', cartcomm=cart_indp_comm, ndim=1, level=P_LEVEL(0:0) )

      ! independent sets
      if( present( indep_comm ) ) indep_comm = cart_indp_comm
    endif

      ! ==================================================
      ! === independent set communicator =================
      ! === 6 dim ========================================
      ! ==================================================

      remain = .true. ; remain(I_INDEP) = .false. ! mask
      call MPI_Cart_sub( cart_all_comm, remain, global%cart_calc_comm, ierr ) ! sub communicator
      if( ierr /= 0 ) stop 'COM error calling MPI_Cart_sub! (calc)'
      call display_communicator( o, name='cart_calc_comm', cartcomm=global%cart_calc_comm, ndim=6, level=P_LEVEL(1:6) )


!       ! ==================================================
!       ! === space communicator ===========================
!       ! === 3 dim ========================================
!       ! ==================================================
! 
!       remain = .false. ; remain(I_KPNTS:I_CELLX) = .true. ! mask
!       call MPI_Cart_sub( cart_all_comm, remain, cart_space_comm, ierr ) ! sub communicator
!       if( ierr /= 0 ) stop 'COM error calling MPI_Cart_sub!'
! 
! 
!       call space( cart_space_comm, bc, gd, nxyz_proc, checkmode )
! 


      ! ==================================================
      ! === equivalent space communicators ===============
      ! === 3 dim ========================================
      ! ==================================================

      remain = .false. ; remain(I_KPNTS:I_BANDS) = .true. ! mask
      call MPI_Cart_sub( cart_all_comm, remain, cart_equi_comm, ierr ) ! sub communicator
      if( ierr /= 0 ) stop 'COM error calling MPI_Cart_sub! (equi)'
      call display_communicator( o, name='cart_equi_comm', cartcomm=cart_equi_comm, ndim=3, level=P_LEVEL(1:3) )

      call MPI_Comm_rank( cart_equi_comm, me_equi, ierr )
      call MPI_Cart_coords( cart_equi_comm, me_equi, 3, rnk_ksb, ierr )

      equi_rnk(1:3) = rnk_ksb(3:1:-1) ! inversion to (b,s,k) order
! cDBG  if(o>0) write(o,'(3A,I8,A,9I4)') sym, fun, 'equi: rank', me_equi, ' coords', equi_rnk


      ! ==================================================
      ! === band communicator ============================
      ! === 1 dim ========================================
      ! ==================================================

      ! bands
      remain = .false. ; remain(I_BANDS) = .true. ! mask
      call MPI_Cart_sub( cart_all_comm, remain, global%band_comm, ierr ) ! sub communicator
      if( ierr /= 0 ) stop 'COM error calling MPI_Cart_sub! (bands)'
      call display_communicator( o, name='band_comm', cartcomm=global%band_comm, ndim=1, level=P_LEVEL(3:3) )
      ! band_comm is a 1dim communicator



!       ! ==================================================
!       ! === solv communicator ============================
!       ! === 4 dim, but used as 2dim ======================
!       ! ==================================================
! 
!       ! ScaLAPACK solver
!       remain = .false. ; remain(I_BANDS:I_CELLX) = .true. ! mask
!       call MPI_Cart_sub( cart_all_comm, remain, solv_comm, ierr ) ! sub communicator
!       call display_communicator( o, name='solv_comm', cartcomm=solv_comm, ndim=4, level=P_LEVEL(3:6) )


#ifdef EXTRA_COMMUNICATORS
      ! kpoints
      remain = .false. ; remain(I_KPNTS) = .true. ! mask
      call MPI_Cart_sub( cart_all_comm, remain, cart_kpt_comm, ierr ) ! sub communicator
      if( ierr /= 0 ) stop 'COM error calling MPI_Cart_sub!'
      call display_communicator( o, name='cart_kpt_comm', cartcomm=cart_kpt_comm, ndim=1, level=P_LEVEL(1:1) )
      ! cart_kpt_comm is a 1dim communicator

      ! collinear spins
      remain = .false. ; remain(I_SPINS) = .true. ! mask
      call MPI_Cart_sub( cart_all_comm, remain, cart_spn_comm, ierr ) ! sub communicator
      if( ierr /= 0 ) stop 'COM error calling MPI_Cart_sub!'
      call display_communicator( o, name='cart_spn_comm', cartcomm=cart_spn_comm, ndim=1, level=P_LEVEL(2:2) )
      ! cart_spn_comm is a 1dim communicator

      ! bands
      remain = .false. ; remain(I_BANDS) = .true. ! mask
      call MPI_Cart_sub( cart_all_comm, remain, cart_bnd_comm, ierr ) ! sub communicator
      if( ierr /= 0 ) stop 'COM error calling MPI_Cart_sub!'
      call display_communicator( o, name='cart_bnd_comm', cartcomm=cart_bnd_comm, ndim=1, level=P_LEVEL(3:3) )
      ! cart_bnd_comm is a 1dim communicator
#endif


      ! ==================================================
      ! === spatial domain decomposition =================
      ! === 3 dim ========================================
      ! ==================================================

      ! cell
      remain = .false. ; remain(I_CELLZ:I_CELLX) = .true.
      call MPI_Cart_sub( cart_all_comm, remain, cart_cell_comm, ierr )
      if( ierr /= 0 ) stop 'COM error calling MPI_Cart_sub! (cell)'
      call display_communicator( o, name='cart_cell_comm', cartcomm=cart_cell_comm, ndim=3, level=P_LEVEL(4:6) )

      call MPI_Comm_rank( cart_cell_comm, me_cell, ierr )
      call MPI_Cart_coords( cart_cell_comm, me_cell, 3, rnk_zyx, ierr )
      if( ierr /= 0 ) stop 'COM error calling MPI_Cart_coords!'

      cell_rnk(1:3) = rnk_zyx(3:1:-1) ! inversion to (x,y,z) order
cDBG  if(o>0) write(o,'(3A,I8,A,9I4)') sym, fun, 'cell: rank', me_cell, ' coords', cell_rnk


      call MPI_Cart_get(cart_cell_comm, 3, grid_proc_size, periods, grid_my_coords, ierr)
cDBG        write (o, '(A, 3I4)') 'proc size is', grid_proc_size(1), grid_proc_size(2), grid_proc_size(3)
      do id = 1, 3
cDBG    if(o>0) write(o,'(3A,I1,9A)') sym, fun, 'call MPI_Cart_shift(cart_cell_comm,', 3-id, ',+/-1,n-,n+,ierr)'
        !    MPI_Cart_shift     comm         dir  1:nearest   lower        upper

        if(grid_proc_size(id) >1 ) then

            neigh_rnk = grid_my_coords
            neigh_rnk(id) = modulo(grid_my_coords(id) - 1, grid_proc_size(id))
            call MPI_Cart_rank(cart_cell_comm, neigh_rnk, neighbor(4-id,1), ierr)
cDBG           write (o, '(A, 2I4)') 'got + neigh = ', neighbor(4-id,1), me_cell

            neigh_rnk = grid_my_coords
            neigh_rnk(id) = modulo(grid_my_coords(id) + 1, grid_proc_size(id))
            call MPI_Cart_rank(cart_cell_comm, neigh_rnk, neighbor(4-id,2), ierr)
cDBG            write (o, '(A, 2I4)') 'got - neigh = ', neighbor(4-id,2), me_cell
!            call MPI_Cart_shift( cart_cell_comm, 3-id, 1, neighbor(id,1), neighbor(id,2), ierr )
            if( ierr /= 0 ) stop 'COM error calling MPI_Cart_shift!'
        else
             neighbor(4-id,:) = me_cell
        endif
cDBG        if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'neighbor processes in direction #', id, ' are ', neighbor(id,1), ' and ', neighbor(id,2)

#ifdef EXTRA_COMMUNICATORS
        ! ==================================================
        ! === planes and rows  =============================
        ! ==================================================

        ! set the subcommunicators for planes in the cell
        remain3 = .true. ; remain3(4-id) = .false. ! reduce along dimension id
        call MPI_Cart_sub( cart_cell_comm, remain3, cart_cell_plane_comm(id), ierr )
        if( ierr /= 0 ) stop 'COM error calling MPI_Cart_sub!'
        call MPI_Comm_rank( cart_cell_plane_comm(id), tmp_rank, ierr )
        call MPI_Cart_coords( cart_cell_plane_comm(id), tmp_rank, 2, plane_coords(:,id), ierr )
        ! warning: plane coords are interchanged

        ! set the subcommunicators for rows in the cell
        remain3 = .false. ; remain3(4-id) = .true. ! reduce along all dimensions but id
        call MPI_Cart_sub( cart_cell_comm, remain3, cart_cell_row_comm(id), ierr )
        if( ierr /= 0 ) stop 'COM error calling MPI_Cart_sub!'
        call MPI_Comm_rank( cart_cell_row_comm(id), tmp_rank, ierr )
        call MPI_Cart_coords( cart_cell_row_comm(id), tmp_rank, 1, row_coord(id), ierr )
#endif

      enddo ! id

      ! apply values to the grid
      ierr = parallelize_grid( g, nxyz_proc, cart_cell_comm, me_cell, cell_rnk, neighbor, &
                                           cart_equi_comm, me_equi, equi_rnk )

      ! loadbalance =   ( product( g%ng(1:3) ) ) .MPImin. g%comm ) / real( product( g%ng(1:3) ) .MPImax. g%comm )
      loadbalance = -( ( -product( g%ng(1:3) ) ) .MPImax. g%comm ) / real( product( g%ng(1:3) ) .MPImax. g%comm )

      ! loadbalance of 10^3/11^3 = 0.7513
      !!!! do n = 1, 19 ; write(*,'(2I4,9F9.4)') n,n+1, (n/(n+1.))**(/1,2,3/) ; enddo !!! execute this line to find a warning threshold
      if( loadbalance < 0.7501 .and. o>0) write(o,'(4A,F0.1,9A)') sym, fun, WARNING(0), &
          'grid load balance ', loadbalance*100., ' % (min/max ratio)!'

! #ifdef EXTRA_COMMUNICATORS
!       global%bnd_comm   = cart_bnd_comm
!       global%spn_comm   = cart_spn_comm
!       global%kpt_comm   = cart_kpt_comm
! #endif
      global%nspn  = global%nspins
      if( ngrid_procs(I_SPINS) == 2 ) global%nspn = 1
      global%iospn = global%nspn * equi_rnk(2)

cDBG  if( global%iospn >= global%nspins ) then
cDBG    if(o>0) write(o,'(3A,I3,I2,I5,A,I8)') sym, fun, 'equi_rnk(B,S,K) = [', equi_rnk, ' ],  me_equi =', me_equi
cDBG    stop 'COM dimensions: spins: offset larger than number.'
cDBG  endif ! iospn >= nspins

      ! distribute the task with the highest possible load balance
      global%nkpt  = parallelize_tasks( ntasks=global%nkpoints, & ! in
                                        nprocs=ngrid_procs(I_KPNTS), & ! in
                                        irank=equi_rnk(3), & ! in
                                        ioffset=global%iokpt ) ! out
cDBG  if( global%iokpt >= global%nkpoints ) stop 'COM dimensions: kpoints: offset larger than number.'


      if( global%nbands < 0 ) then
        ! automatic number of bands
        global%nbnd  = -1
        global%iobnd =  0
      else  ! bands
        global%nbnd  = parallelize_tasks( ntasks=global%nbands, & ! in
                                          nprocs=ngrid_procs(I_BANDS), & ! in
                                          irank=equi_rnk(1), & ! in
                                          ioffset=global%iobnd ) ! out
      endif ! bands

! end of the MPI part
#endif
  endfunction ! dimensions

  real function load_balance( ntasks, nsets ) result( lb )
    integer, intent(in) :: ntasks, nsets
    real :: q
    lb = 0. ; if( nsets < 1 ) return ! 0.
    q = real(ntasks)/real(nsets)
    lb = floor( q )/real(ceiling( q ))
  endfunction ! load_balance

  ! this function distributes NTASKS onto nprocs such that
  ! the load imbalance is smallest. example: ntasks=27, nproc=8
  !
  !>" rank 0    1    2    3    4    5    6    7                  "
  ! "------------------------------------------------            "
  ! "    T25 T26 T27                                             "
  ! "                                                            "
  ! "    T17 T18 T19 T20 T21 T22 T23 T24                         "
  ! "                                                            "
  ! "    T09 T10 T11 T12 T13 T14 T15 T16                         "
  ! "                                                            "
  ! "    T01 T02 T03 T04 T05 T06 T07 T08                         "
  ! "                                                            "
  !<"  ntsk = parallelize_tasks( ntasks, nprocs, irank, ioffset )"
  !
  integer function parallelize_tasks( ntasks, nprocs, irank, ioffset ) result( nptasks )
  use configuration, only: o, WARNING, ERROR
    integer, intent(in)             :: ntasks !! number of all tasks
    integer, intent(in)             :: nprocs !! number of processes
    integer, intent(in)             :: irank !! rank of this process
    integer, intent(out)            :: ioffset !! offset in the full list

cDBG  character(len=*), parameter     :: fun = ' parallelize_tasks: '
cDBG  integer                         :: nloop
    integer                         :: nt(-1:+1), np(-1:+1) ! Parallel distribution

    ioffset = 0
    ! result, if an error occurs
    nptasks = -1 ! init as error for early return

#ifdef DEBUG
    if( ntasks < 1 ) then
      if(o>0) write(o,'(9A)') sym, fun, ERROR, 'ntasks < 1, return.'
      return ! ERROR
    elseif( nprocs < 1 ) then
      if(o>0) write(o,'(9A)') sym, fun, ERROR, 'nprocs < 1, return.'
      return ! ERROR
    elseif( irank < 0 .or. irank >= nprocs ) then
      if(o>0) write(o,'(9A)') sym, fun, ERROR, 'irank out of bounds [0,nprocs-1], return.'
      return ! ERROR
    endif ! irank out of [0,nprocs-1]
#endif

    if( nprocs <= 1 ) then
      ! unparallelized
      ioffset = 0
      nptasks = ntasks ! all
      return
    endif ! unparallelized

    ! parallelize the tasks among the processes
    nt( 0) = ntasks                 ! # of all tasks
    np( 0) = nprocs                 ! # of all procs
    np(+1) = modulo( nt(0), np(0) ) ! # of procs with the larger # of tasks
    np(-1) = np(0) - np(+1)         ! # of procs with the smaller # of tasks

cDBG  if( modulo( (nt(0)-np(+1)), np(0) ) /= 0 ) stop 'parallelize_tasks: fatal error in task distribution.'

    nt(-1) = ( nt(0) - np(+1) )/ np(0) ! smaller # of tasks

cDBG  if( nt(-1) < 0 ) stop 'parallelize_tasks: fatal error: smaller number less than 0'

    nt(+1) = nt(-1)+1                ! larger # of tasks

    if( np(+1) > 0 ) then
      ! load balance = nt(-1)/nt(+1), if there are processes that have more
      ! ==> load imbalance = 1-load balance = 1-nt(-1)/nt(+1) = (nt(+1)-nt(-1))/nt(+1) = 1/nt(+1)
      if(o>0) write(o,'(3A,F0.1,9A)') sym, fun, 'estimated load imbalance ', 100./nt(+1), '%'
    else  ! np+ > 0
      if(o>0) write(o,'(3A,F0.1,9A)') sym, fun, 'estimated load imbalance ', 0., '%'
    endif ! np+ > 0

    ! check conservation rule
cDBG  if( np(-1)*nt(-1) + np(+1)*nt(+1) /= nt(0) ) stop 'parallelize_tasks: fatal error: # of tasks is not conserved.'
    ! up to here, everything is the same for all procs

    ! now, the number of parallelized tasks and offsets are assigned individually
    if( irank < np(+1) ) then
      ! this process has to work more
      ioffset = irank * nt(+1)
      nptasks = nt(+1)
    else  ! irank
      ! this process has to work less
      ioffset = np(+1)*nt(+1) + (irank-np(+1)) * nt(-1)
      nptasks = nt(-1)
    endif ! irank

    ! Warning for idle processes
    if( nt(-1) == 0 ) then
      if(o>0) write(o,'(3A,I0,9A)') sym, fun, WARNING(0), np(-1),' processes will be idle!'
    endif ! nt(-1) == 0

#ifdef DEBUG
    ! checks
    if( nptasks < 0 ) stop 'parallelize_tasks: fatal error: negative # of parallelized tasks.'
    if( np(-1) < 1 ) stop 'parallelize_tasks: fatal error: # of PEs with smaller # of tasks is < 1'
    if( nt(-1) <= 0 .and. nt(+1) /= 1 ) stop 'parallelize_tasks: fatal error: nt(+) must be 1, if other PEs are idle.'
    if( np(+1) <= 0 .and. nt(-1) < 1 ) stop 'parallelize_tasks: fatal error: total # of tasks is < 1'
    ! output
    if(o>0) then
      ! 3 possible cases
      if( np(+1) > 0 ) then
        if( nt(-1) > 0 ) then
          ! the # of tasks is divided into some PEs with less and some with more tasks
          write(o,'(2A,9(I0,A))') sym, fun, np(0), ' processes share ',nt(0),' tasks as ',np(-1),'p x ',nt(-1),'t + ',np(+1),'p x ',nt(+1),'t'
        else ! nt(-1) > 0
          ! the # of tasks is divided into some PEs with tasks and some idle
          ! ==> nt(-1) == 0 ; nt(+1) == 1
          write(o,'(2A,9(I0,A))') sym, fun, np(0), ' processes share ',nt(0),' tasks as ',np(-1),'p x 0t + ',np(+1),'p x 1t'
        endif ! nt(-1) > 0
      else  ! np(+1) > 0
        ! the # of tasks is divided into equally between all PEs
        ! ==> np(+1) == 0
          write(o,'(2A,9(I0,A))') sym, fun, np(0), ' processes share ',nt(0),' tasks as ',np(-1),'p x ',nt(-1),'t'
      endif ! np(+1) > 0
    endif ! o>0
#endif
  endfunction ! parallelize_tasks


  !! this method uses the Minesweeper algorithm
  !! to find the table of blocking atomic communications
  !! and generates an order of treating the parallelized
  !! list of atoms such that the atomic communications
  !! will not serialize. Therefore coloring is used.
  !!
  !! two ideas to save memory:
  !!   - using a shift-insert list instead of a full matrix for IACYC
  !!   - factor 8 for BLOCK when using bitwise logicals
  integer function atom_order( p, comm, ord ) result( ist )
  use configuration, only: WARNING, ERROR
  use configuration, only: o
  use MPIconst, only: MPI_LOR, MPI_LOGICAL1 => MPI_CHARACTER
  use MPIconst, only: Wtime
#ifdef DEBUG
  use configuration, only: u => o
#else
    iounit_t, parameter :: u=0 ! no output
#endif

    logical, intent(in)  :: p(:) !! p(number of all atoms) true, if the non-local projector of the atom has an overlap with this domain
    MPI_Comm, intent(in) :: comm !! grid communicator
    integer, intent(out) :: ord(:) !! ord(number of all atoms) <br> indices of all atoms in new order

cDBG  character(len=*), parameter           :: fun = ' atomorder: '
    integer :: ia1, ia2, ija, nblock, ii, na, ja, nprocs=1, me=0, ncol
    logical(1), allocatable :: block(:), blk(:)
    real :: t(0:6)

    ist = 0 ! SUCCESS
    na = size( p ) ! number of all atoms
    if( na < 1 ) return ! success

    ist = 1 ! ERROR
cDBG  if( size(ord) /= na ) stop 'COM: atom_order: array ORD should match the number of atoms'
    if( size(ord) /= na ) return
    ord = (/ ( ja, ja=1,na ) /) ! serial part

    ist = 0 ! SUCCESS
    if( na < 2 ) return

#ifdef NOMPI
! without MPI, multi-coloring is not needed !
    return
#else
! start of MPI part

    call MPI_Comm_size( comm, nprocs, ist ) ! get comm size
    if( nprocs <= 1 ) return
    call MPI_Comm_rank( comm, me, ist ) ! get my rank

cDBG  if(u>0) write(u,'(2A,9(I0,A))') sym, fun, me, '#: ',count( p ),' of ',na,' atoms participate'

    ! an atom pair cannot be reduced along the atomic communicator,
    ! if any of the processes contributes to both atoms of that pair.
    ! set up a table of blocked atom pairs:
    nblock = (na*(na-1))/2

    t(0) = Wtime()

    ! memory scaling quadratically, time scaling quadratically
cDBG  if(u>0) write(u,'(3A,I0,9A)') sym, fun, 'allocate ',nblock,' 1-byte-logicals for atom pairs.'
    allocate( blk(nblock), stat=ist )
    if( ist /= 0 ) stop 'atom_order: allocation of BLOCK (blk) failed!'
    ija = 0 ! init combindex
    do ia1 = 1, na
      do ia2 = 1, ia1-1 ! triangular self-avoiding loop
        ija = ija+1 ! count up combindex
        blk(ija) = p(ia1) .and. p(ia2)
!         ! this tests, if the formula ia2+((ia1-1)*(ia1-2))/2 is correct (formula is used later)
! cDBG  if( ija /= ia2+((ia1-1)*(ia1-2))/2 .and. o>0) write(o,'(3A,9(I0,A))') sym, fun, 'ija > nblock, ia2 ',ia2,' ia1 ',ia1,' ija ',ija,' nblock ',nblock,' jia''', ia2+(ia1*(ia1-1))/2
cDBG    if( ija /= ia2+((ia1-1)*(ia1-2))/2 ) stop 'atom_order: ia2+((ia1-1)*(ia1-2))/2 wrong function for ija'
      enddo ! ia2
    enddo ! ia1
cDBG  if(u>0) write(u,'(3A,9I9)') sym, fun, 'part(me/T/F/n):', me, count(p), count(.not. p), size(p)
cDBG  if(u>0 .and. me==0) write(u,'(3A,9I9)') sym, fun, 'block(T/F/n):', count(blk), count(.not. blk), size(blk)
cDBG  if( ija /= nblock ) stop 'atom_order: fatal counting error.'

    t(1) = Wtime()

    allocate( block(nblock), stat=ist )
    if( ist /= 0 ) stop 'atom_order: allocation of BLOCK failed!'

    call MPI_Allreduce( blk, block, nblock, MPI_LOGICAL1, MPI_LOR, comm, ist )
    if( ist /= 0 ) stop 'atom_order: allreduce of BLOCK failed!'

    t(2) = Wtime()

    deallocate( blk, stat=ii )
    ! now block contains all blocked atom pairs
cDBG  if(u>0) write(u,'(3A,9I9)' ) sym, fun, 'allblock (T/F/n):', count(block), count(.not. block), nblock ! show statistics

#ifdef DEBUG
    ! write the information about the graph to a file
    if( me == 0 ) then
      open( unit=15, file='atomcolor.graph', status='old', action='write', iostat=ii )
      if( ii /= 0 ) then ! file must exist
        if(o>0) write(o,'(9A)') sym, fun, 'create file "atomcolor.graph" to export the graph adjacency matrix elements!'
      else
        ija = 0 ! init combindex
        do ia1 = 1, na
          do ia2 = 1, ia1-1 ! triangular self-avoiding loop
            ija = ija+1 ! count up combindex
            if( block(ija) ) write( unit=15, fmt='(I0," ",I0)', iostat=ii ) ia1, ia2
          enddo ! ia2
        enddo ! ia1
        close( unit=15, iostat=ii )
        if(o>0) write(o,'(9A)') sym, fun, 'graph adjacency matrix elements exported to file "atomcolor.graph".'
      endif ! ios == 0
    endif ! master
#endif

! end of MPI part
#endif
    ncol = atom_color( na, block, ord )
    if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'multi-coloring of ',na,' atoms results in ',ncol,' colors.'
    deallocate( block, stat=ii )
  endfunction ! atom_order

  integer function atom_color( na, block, iai ) result( nc ) !! uses temp. memory ~ na
cDBG  use configuration, only: o 
    integer, intent(in)    :: na ! number of atoms
    logical(1), intent(in) :: block(:)
    integer, intent(out)   :: iai(:)

cDBG  character(len=*), parameter     :: fun = ' atom_color: '
cDBG  iounit_t, parameter :: u=6
cDBG  integer :: ica(na)
    logical :: allowed
    integer :: ia, ja, ija, ic, ii, io, nblock, ioc(na), nac(na)

    nblock = (na*(na-1))/2
cDBG  if( size(block) /= nblock ) stop 'atom_color: dim of BLOCK should match (NA*(NA-1))/2!'
cDBG  if( size(iai) /= na ) stop 'atom_color: dim of IAI should match NA!'

    nc = 0 ! return value is the number of colors

cDBG  ica = 0 ! init color index of atom index

    iai = 0 ! atom order
    nac = 0 ! number of atoms in color
    ioc = 0 ! integer offset of color
    nc = 0 ! init number of colors

    do ia = 1, na
cDBG  if( ica(ia) /= 0 ) stop 'atom_order: fatal: ICA has not been initialized!'
cDBG  ica(ia) = 0 ! initialize as 0:undecided, in which color this atom is (redundant)
      allowed = .false. ! init as blocked

      ic = 0 ! init
      do while( ic < nc ) ! loop over colors
        ic = ic+1 ! count up
        ! suggest color ic
        io = ioc(ic) ! offset

        allowed = .true. ! init as not blocked
        ii = 0
        do while( ii < nac(ic) .and. allowed ) ! loop over other atoms in ic
          ii = ii+1
          ! load the atom index of the atoms that are already in this color
          ja = iai(io+ii) ! assuming that ja < ia, because ja has been added earlier
cDBG      if( ja <= 0 ) stop 'atom_order: index JA < 0'
cDBG      if( ja >= ia ) stop 'atom_order: JA >= IA should not happen.'
cDBG      if( ica(ja) /= ic ) stop 'atom_order: fatal ICA(JA) /= IC'
          ija = ja+((ia-1)*(ia-2))/2 ! compute combindex for ija
cDBG      if( ija > nblock ) then
cDBG        if(o>0) write(o,'(3A,9(I6,A))') sym, fun, 'ija > nblock, ja', ja, ' ia', ia, ' ija', ija, ' nblock', nblock
cDBG        stop 'atom_order: ija > nblock'
cDBG      endif ! ija > nblock
! cDBG      if(u>0) write(u,'(2A,3(I3,A),L2)') sym, fun, ia, '=ia, compare ja=', ja, ' in color', ic, ' b=', block(ija)
          allowed = allowed .and. ( .not. block(ija) )
        enddo ! while ii ! loop over the atoms in this color

        if( allowed ) then
! cDBG      if(u>0) write(u,'(2A,9(I3,A))') sym, fun, ia, '=ia, add to color ic =', ic
          ! add atom ia to color ic
          nac(ic) = nac(ic)+1 ! increase the number of atoms in this color
          ! compute new position in ord list
          ii = io + nac(ic) ! at the end of the color ic
          ! shift the iai list, such that position ii becomes free
          iai(ii+1:ia) = iai(ii:ia-1)
          ! set ia entry in iai list
          iai(ii) = ia
cDBG      ica(ia) = ic ! set the color index of atom ia
          ic = nc ! exit the while(ic<nc)-loop
        endif ! allowed

      enddo ! while ic < nc

      if( .not. allowed ) then ! in existing colors
        ! the ica index is still 0 (== undecided), this
        ! means, that no color has been found, where ia1 is permitted
        ! therefore ia1 needs to

        ! create a new color
        nc = nc+1 ! increase the number of colors
        ic = nc   ! the index of the new color is the new number of colors

! cDBG    if(u>0) write(u,'(2A,9(I3,A))') sym, fun, ia, '=ia, add to color ic =', ic, ' (new)'
        ! add atom ia to color ic
cDBG    if( nac(ic) /= 0 ) stop 'atom_order: fatal: NAC has not been initialized!'
        nac(ic) = 1 ! increase the number of atoms in this color (must have been zero before)
        ! set ia entry in iai list (diagonal)
        iai(ia) = ia
cDBG    ica(ia) = ic ! set the color index of atom ia

      endif ! ia not allowed in existing colors

      io = 0 ! init
      do ic = 1, nc
        ioc(ic) = io ! update offsets
        io = io + nac(ic)
      enddo ! ic
cDBG  if( io /= ia ) stop 'atom_order: fatal counting !'

#ifdef FULL_DEBUG
cDBG  if(u>0) write(u,'(2A,I3,A,99I3)') sym, fun, ia, '=ia, ioc: ', ioc(1:nc)
cDBG  if(u>0) write(u,'(2A,I3,A,99I3)') sym, fun, ia, '=ia, nac: ', nac(1:nc)
cDBG  if(u>0) write(u,'(2A,I3,A,99I3)') sym, fun, ia, '=ia, ica: ', ica(1:ia)
cDBG  if(u>0) write(u,'(2A,I3,A,99I3)') sym, fun, ia, '=ia, iai: ', iai(1:ia)
cDBG  if(u>0) write(u,'(2A,I3,A,99I3)') sym, fun, ia, '=ia, ord: ', ica(iai(1:ia))
cDBG  if(u>0) write(u,'(2A,I3,A,99I3)') ! empty line
#endif
    enddo ! ia
#ifdef FULL_DEBUG
cDBG  if(u>0) write(u,'(3A,999I3)') sym, fun, 'ica:', ica
cDBG  if(u>0) write(u,'(3A,999A3)') sym, fun, '   :|', ((' ',ii=2,nac(ic)),'|',ic=1,nc)
cDBG  if(u>0) write(u,'(3A,999I3)') sym, fun, '   :', ica(iai)
cDBG  if(u>0) write(u,'(3A,I3,9A)') sym
#endif
cDBG  if(u>0) write(u,'(3A,999(I3,A))') sym, fun, 'found', nc, ' colors: |', ( nac(ic), ' |',ic=1,nc)
cDBG  if(u>0) write(u,'(3A,999I3)')      sym, fun, 'order:', iai
  endfunction ! atom_color



  !! write a nice overview about the information contained in a
  !! multi dimensional cartesian communicator to unit u
  subroutine display_communicator( u, name, cartcomm, ndim, level )
    iounit_t, intent(in)            :: u        !! unit to write to
    character(len=*), intent(in)    :: name     !! name of the communicator
    MPI_Comm, intent(in)            :: cartcomm !! communicator
    integer, intent(in)             :: ndim     !! number of dimensions
    character, intent(in), optional :: level(:) !! parallelization level ??? not sure
#ifdef FULL_DEBUG
    integer :: dims(ndim), coords(ndim), i
    logical :: peri(ndim)
    status_t :: ierr
      if( u == 0 ) return
#ifndef NOMPI
      call MPI_Cart_get( cartcomm, ndim, dims, peri, coords, ierr )
#endif
      write(u,'(4A,Z8)') sym, ' comm  "', name, '": 0x', cartcomm
      write(u,'(99A)')   sym, ' ', ('----', i=-1,ndim)
      if( present( level ) ) &
      write(u,'(2A,9A4)') sym, ' level ',level(1:ndim)
      write(u,'(2A,9I4)') sym, ' dims  ', dims(1:ndim)
      write(u,'(2A,9L4)') sym, ' peri  ', peri(1:ndim)
      write(u,'(99A)')    sym, ' ', ('----', i=-1,ndim)
      write(u,'(99A)') '' ! empty line
#endif
  endsubroutine ! display_communicator


  !! finalizes the MPI environment
  status_t function comm_finalize( message ) result( ierr )
  use configuration, only: o
  use MPIconst, only: Wtime
! use, only: start_time
    character(len=*), intent(in)          :: message

cDBG  character(len=*), parameter           :: fun = ' finalize: '
    real :: time

    time = Wtime( ) - start_time
    if(o>0) write(o,'(5A,F0.1,9A)') sym, fun, '"', trim(message), '"  total time ', time, ' sec'
#ifndef NOMPI
    call MPI_Finalize( ierr )
    if( ierr /= 0 ) stop 'MPI_Finalize returned an IERR /= 0'
#else
    ierr = 0
#endif
  endfunction ! comm_finalize


  !! find the best factorization nproc, such that
  !! product(nproc) == nprocs 
  !! and the internal surface are minimal
  !! the function does not care, if the number of grid
  !! points is divisible by the resulting nproc, but
  !! it just gives the best usage of the given number of
  !! processes. on exit, ng is adjusted to that suggestion.
  !!                                        pbaum Aug 2009
  logical function factorize( nprocs, ng, nproc ) result( ok )
  use configuration, only: o, WARNING
    integer, intent(in)         :: nprocs  ! number of processes
    integer, intent(inout)      :: ng(1:3) ! number of grid points
    integer, intent(out)        :: nproc(1:3)

    integer, parameter :: NPRIME = 25, PRIME_LIST(NPRIME+1) = &
    (/  2, 3, 5, 7,11,13,17,19,23,29,31,37,41, &
          43,47,53,59,61,67,71,73,79,83,89,97,101 /)
cDBG  character(len=*), parameter :: fun = ' factorize: '
cDBG  integer :: ii
    character(len=128)          :: m
    integer                     :: ip, ia, prime, ft(NPRIME), mp1mp2h(NPRIME)
    integer                     :: id, istat, ic, i1, i2, i3, i3n, iloc(1)
    integer                     :: nfa, ifa, np(3), nall
    integer, allocatable        :: conf(:,:), mcomb(:,:,:)
    real, allocatable           :: sconf(:) ! surface
    logical                     :: adjusted, run
    real                        :: s

    ok = .false. ; m = 'undocumented error'

    nproc(1:3) = 1
    if( nprocs < 1 ) stop 'COM factorize: NPROCS < 1 impossible.'
    if( any( ng < 1 ) ) stop 'COM factorize: NG < 1 impossible.'

    ! this case does not need to be treated specially, but
    ! but it avoids errors, since this one is very frequent.
!     if( nprocs == 1 ) then
!       ok = .TRUE. ; m= 'ok' ; return ! simplest case factorization is very fast!
!     endif ! nprocs == 1

    if( nprocs >= PRIME_LIST(NPRIME+1) ) then
      if(o>0) write(o,'(4A,9(I0,A))') sym, fun, WARNING(0), &
        'factorization of ',nprocs,' might fail, largest prime is ', PRIME_LIST(NPRIME)
    endif ! out of the range of the prime list

    ft(:) = 0 ! init
    ia  = nprocs
    ip  = 0 ! index of the prime number ! init
    run = ( ip < NPRIME ) ! = .TRUE.
    do while( run )
      ip = ip+1
      prime = PRIME_LIST(ip)
      do while( modulo( ia, prime ) == 0 )
        ft(ip) = ft(ip)+1 ! count up prime factor
        ia = ia/prime ! division without loss of information
      enddo ! while divisible by prime number ip
      run = ( ia >= PRIME_LIST(ip+1) ) & ! stop, if ia < next prime factor
              .and.   ( ia /= 1 )      & ! stop, if ia == 1
              .and.   ( ip < NPRIME )    ! stop, if NPRIME is reached 
    enddo ! while

    if( ia /= 1 ) then
      if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'factorization failed, ', &
          nprocs,' contains prime factors larger than ', PRIME_LIST(NPRIME)
      ok = .FALSE. ; m = 'failed, prime list too short.' ; return
    endif ! ia /= 1

#ifdef FULL_DEBUG
    if(o>0) write(o,'(A)') ''
    if(o>0) write(o,'(2A,I0,9A)') sym, fun, nprocs,' = ',trim(write_factorization( ft ))
#endif
    ! Warn about high prime factors
    if( any(ft(5:)>0 ) ) m='Warning: better avoid high prim factors.'

    ! now, nprocs is factorized.
    ! how many possibilities are there to generate a
    ! distribution of of these prime factors?
    ! an easy solution is, to take each prime factor
    ! as many time as it occurs and put it to each
    ! of the three places once, but this would mean,
    ! that multiple prime factors generate the same
    ! configuration more than once.

    ! prime factor p
    ! multiplicity m

    ! for a multiple prime factor,
    ! the number of configs is (m+1)*(m+2)/2,
    ! this includes the case m=1 and m=0, so

    mp1mp2h = ((ft+1)*(ft+2))/2 ! division ok, one of the factors is even
    nfa = count( ft > 0 )    

    ! generate lists of all subspaces
    allocate( mcomb(1:3,maxval(mp1mp2h),nfa), stat=istat )
    mcomb = 0 ! init
    ifa = 0
    do ip = 1, NPRIME
      if( ft(ip) > 0 ) then
        ifa = ifa+1
        !=========================
        ic = 0
        do i3 = 0, ft(ip) ; do i2 = 0, ft(ip) ; do i1 = 0, ft(ip)
          if( i1 + i2 + i3 == ft(ip) ) then
            ic = ic+1
            mcomb(1:3,ic,ifa) = (/i1,i2,i3/)
          endif ! i1+i2+i3 == ft
        enddo ; enddo ; enddo ! i123
        !=========================
      endif ! ft(ip) > 0
    enddo ! ip
    if( ifa /= nfa ) stop 'COM factorize: fatal counting error'

#ifdef FULL_DEBUG
    if(o>0) then ! show mcomb
      ifa = 0
      do ip = 1, NPRIME
        if( ft(ip) > 0 ) then
          ifa = ifa+1
          write(o,'(2A,I6,A,99(3I1,A))') sym, fun, &
           PRIME_LIST(ip), ':  ', (mcomb(1:3,ic,ifa), ' ', ic=1,mp1mp2h(ip) ) 
        endif ! ft(ip) > 0
      enddo ! ip
    endif ! o/=0
#endif

    ! number of all possibilities:
    !     NPRIME
    !     ---
    ! N = | |    (ft(ip)+1)*(ft(ip)+2)/2
    !     ip=1
    nall = product( mp1mp2h )

#ifdef FULL_DEBUG
    if(o>0) write(o,'(2A,I0,9A)') sym, fun, nall,' combinations:'
#endif

    ! generate all configurations
    allocate( conf(0:3,nall), sconf(nall), stat=istat )
    do ic = 1, nall
      i3n = 1
      np(1:3) = 1
      ifa = 0
      do ip = 1, NPRIME
        if( ft(ip) > 0 ) then
          ifa = ifa+1
          id = mod( ic / i3n , mp1mp2h(ip) ) + 1
          if( id < 1 .or. id > mp1mp2h(ip) ) stop 'COM factorize: fatal error 2.'
          np(1:3) = np(1:3) * PRIME_LIST(ip) ** mcomb(1:3,id,ifa)
          i3n = i3n * mp1mp2h(ip)
        endif ! ft > 0
      enddo ! ip
      conf(1:3,ic) = np(1:3)
      conf( 0 ,ic) = ic
      s = comm_surface_ratio( np, ng )
      sconf(ic) = s
#ifdef FULL_DEBUG
      ! output all configurations
      if(o>0) write(o,'(3A,3I4,F16.10)') sym, fun, '    ', np, s
#endif
    enddo ! ic

#ifdef DEBUG
    ! check, if any combination appears twice (triangual loop)
    ! Warning: scales as N^2
    ic = 0 ! counter
    id = 0 ! counter
    do i1 = 1, nall
      if( product(conf(1:3,i1)) /= nprocs ) ic = ic+1 
      do i2 = i1+1, nall
        if( all( conf(1:3,i1) == conf(1:3,i2) ) ) id = id+1
      enddo ! i2
    enddo ! i1
    if( ic > 0 ) stop 'COM factorize: at least one combination is incorrect.'
    if( id > 0 ) stop 'COM factorize: at least one combination appears twice'
#endif

    iloc = minloc( sconf(:) )
    nproc(1:3) = conf(1:3,iloc(1)) ! best choice with the lowest S/V ratio
#ifdef FULL_DEBUG
    np = nproc
    s = sconf(iloc(1))
    if(o>0) write(o,'(9A)') sym, fun, ('--------', ii=1,4)
    if(o>0) write(o,'(3A,3I4,F16.10)') sym, fun, 'min:', np, s
#endif

    ! adjust ng to nproc
    adjusted = .false.
    do id = 1, 3
      if( modulo( ng(id), nproc(id) ) /= 0 ) then
        ng(id) = ( ng(id)/nproc(id) + 1 )*nproc(id)
        adjusted = .true.
      endif ! ng % nproc /= 0
    enddo ! id
#ifdef FULL_DEBUG
    if(o/=0 .and. adjusted) write(o,'(3A,3I7)') sym, fun, 'adjust ng =', ng
#endif


    ok = .TRUE. ; m = 'ok'
#ifdef FULL_DEBUG
    if(o>0) write(o,'(A)') ''
#endif

    deallocate( sconf, conf, mcomb, stat=istat )

  return
  contains

    real function comm_surface_ratio( np, ng ) result( s )
      integer, intent(in) :: np(1:3) ! number of processes
      integer, intent(in) :: ng(1:3) ! number of grid points
    !! the surface/volume ratio is to be minimized, because
    !! it is proportional to the amount of communication
      !!! these weights enfavour the z-direction
      !!! to be parallelized in the case of degeneracy
      !!! because then the inner loops (x and y) are longer
      real, parameter :: WEIGHT(1:3) = (/1.001,1.000,0.999/)
      !
      ! surface = 2*( ny*nz + nz*nx + nx*ny )
      !           ----------------------------
      ! volume  =         nx * ny * nz
      !
      s = 2.* sum( np/real(ng) * WEIGHT )
    endfunction ! comm_surface_ratio

    character(len=128) function write_factorization( f ) result( str )
      integer, intent(in)           :: f(1:) ! factorization

      character(len=*), parameter   :: LINK = ' * ', POW = '^' ! '**'
      integer :: ip
      status_t :: ios
      str = '1' ! init
      do ip = 1, min( NPRIME, ubound(f,1) )
        selectcase( f(ip) )
        case( -1 ) ; stop 'write_factorization: impossible'
        case(  0 ) ! nothing
        case(  1 )   ; write(str,fmt='(2A,I0)',IOstat=ios) trim(str), LINK, PRIME_LIST(ip) ! does not need the power
        case default ; write(str,fmt='(A,9(A,I0))',IOstat=ios) trim(str), LINK, PRIME_LIST(ip), POW, f(ip)
        endselect ! f(ip)
      enddo ! ip
    endfunction ! write_factorization

  endfunction ! factorize

#ifdef EXTENDED
!+ extended

  status_t function test_topo( ) result( ist )
  ! create a new 3 dimensional topolopy from the Scalasca result files, 
  ! even though the actual measurement has been performed with a 9 dim cart. communicator
    integer :: N(3)
    logical :: done, copy
    status_t :: lios, ios
    character(len=128) :: line

!     integer :: in, id
!     character(len=8) :: arg

!    do id = 1, 3
!      in = 0
!      call get_command_argument( id, arg )
!      read(unit=arg,fmt=*,iostat=ios) in
!      if( ios == 0 .and. in > 0 ) N(id) = in
!    enddo ! id

    N(1:3) = (/8,8,8/) ! default

    write(*,'(A,3I6)') 'COM Please input new Dimensions! default=',N
    read(5,fmt=*,iostat=ios) N
    if( ios /= 0 ) stop 'COM change_topology: please input 3 positive integers!'
    write(*,'(A,3I6)') 'COM Change topology to', N

    open(unit=7,file='epitome.cube',action='read',status='old',iostat=ios) 
    if( ios /= 0 ) stop 'COM cannot find file "epitome.cube".'
    open(unit=8,file='epitome.cube.bak',action='write',iostat=ios)
    if( ios /= 0 ) stop 'COM opening file "epitome.cube.bak" failed.'
    open(unit=9,file='epitome.cube.new',action='write',iostat=ios) 
    if( ios /= 0 ) stop 'COM opening file "epitome.cube.new" failed.'

    copy = .true. ! status normal
    done = .false. ! not done

    ! copy file
      read(unit=7,fmt='(A)',iostat=lios) line ! read 1st line
    do while( lios == 0 )
      write(unit=8,fmt='(A)') trim(line) ! writes a backup file
      if( done ) then
        write(unit=9,fmt='(A)') trim(line) ! writes the same line to the output file
      else  ! done
        if( copy ) then
          write(unit=9,fmt='(A)') trim(line) ! writes the same line to the output file
          if( line == '    <topologies>'  ) then
            copy = .false. ! switch off copying
            ist = create_new_topology( 9, N ) ! create new topology
          endif !
        elseif( line == '    </topologies>' ) then
          write(unit=9,fmt='(A)') trim(line) ! writes the same line to the output file
          done = .true.
        endif ! is == 0 
      endif ! done

      read(unit=7,fmt='(A)',iostat=lios) line ! read nxt line
    enddo ! while

    close(7,iostat=ios)
    close(8,iostat=ios)
    close(9,iostat=ios)

    call system( "mv epitome.cube.new epitome.cube" )
    stop
  !   stop 'mv epitome.cube.new epitome.cube' ! command line to override to old file with the new

  contains

    status_t function create_new_topology( u, N ) result( ios )
      iounit_t, intent(in) :: u    !! unit number to write to
      integer, intent(in)  :: N(3) !! dimensions

      integer              :: i1, i2, i3, id
      write(u,'(9(A,I0))',iostat=ios)   '      <cart ndims="3">' ! create new topology
      do id = 1, 3
        write(u,'(9(A,I0))',iostat=ios) '        <dim size="',N(id), '" periodic="true"/>'
      enddo ! id
      id = 0
      do i3 = 0, N(3)-1 ; do i2 = 0, N(2)-1 ; do i1 = 0, N(1)-1
        write(u,'(9(A,I0))',iostat=ios) '        <coord thrdId="',id,'"> ',i1,' ',i2,' ',i3,' </coord>'
        id = id+1 ! cout up
      enddo ; enddo ; enddo ! i1 i2 i3
      write(u,'(9(A,I0))',iostat=ios)   '      </cart>'
    endfunction ! create_new_topology

  endfunction ! test_topo

  status_t function test( )
!     integer :: na
!     do na = 1, 38
!       call test_na( na*na )
!     enddo ! na
    integer :: nprocs, nproc(3), ng(3)
    do nprocs = 115, 1, -1
       ng = 256 ! (/2**4*3**2,2**3*5**2,2*3*5/)
       write(*,'(2(A,I0),2(" x ",I0),A,3(" ",I0),A,L2)') __FILE__, &
         nprocs, ' processes = ', nproc(1:3), '      ng=', ng, '  ok=', factorize( nprocs, ng, nproc )
    enddo ! nprocs
    test = 0
  endfunction ! test

!- extended
#endif
endmodule ! communicators

#include "config.h"

! #define DEBUG

#define BANDSTRUCTURE_USE_CG
! ToDo remove dependence to PRE e_order, not needed with subspace_rotation

! documentation
!! @author Paul Baumeister
!! @version 3.0
!!
!! compute a bandstructure along a given path in the Brillouin zone
module bandstructure
  use configuration, only: o ! output unit, 0: no output
implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: sym = 'BST' !! module symbol

  public :: generate_path
  public :: bandpath
#ifdef EXTENDED
  public :: test
#endif

  interface bandpath
    module procedure bandpath_c
  endinterface

  !! step length in reciprocal space
  real, parameter, private :: DEFAULT_GSPACING = 0.05 !! 0.05 ==> 10 steps from Gamma to X, if the cell extend is 6.28
  !! minimal step length in reciprocal space
  real, parameter, private :: MINIMUM_GSPACING = 1.E-6 ! very dense

  contains

  !! creates the densely sampled path of k-points
  !! connecting the k-point edges given in kpath.
  status_t function generate_path( g, kpath, k_list, gspacing, outputunit, allkpoints ) result( ist )
  use constants, only: Pi
  use configuration, only: CommentChars
  use type_grid, only: grid
  use type_kpoint, only: kpoint, kpoint_set
    type(grid), intent(in)                            :: g           !! the coarse grid
    type(kpoint), intent(in)                          :: kpath(:)    !! kpoint path, list of edges to be connected
    real, allocatable, intent(out), optional          :: k_list(:,:) !! on demand, here all k-verctors along the sampled path are returned in a list
    real, intent(in), optional                        :: gspacing    !! step length in reciprocal space (<a href="#DEFAULT_GSPACING">default</a>)
    iounit_t, intent(in), optional                    :: outputunit  !! unit to write energy eigenvalues to, 0: do not write
    type(kpoint), allocatable, intent(out), optional  :: allkpoints(:) !! on demand, here all kpoints along the sampled path are returned in a list

#ifdef DEBUG
    character(len=*), parameter :: fun = ' generate_path: '
#else
    character(len=*), parameter :: fun = ': '
#endif
    character, parameter        :: CommentChar = CommentChars(1)
    real                        :: kvec(3), dkvec(3)!, dk, kdiff(3), lkvec
    real                        :: dg, lgvec, dgvec(3), dghere
    real                        :: gbasis(3) ! diagonal only
    real                        :: lgall, gvec(3)
    integer                     :: nk, ne, ik, ik0, i01, ie, id, nkall, ikall
    iounit_t                    :: u

    u = 0 ; if( present( outputunit ) ) u = outputunit
    ist = 0

    dg = DEFAULT_GSPACING ; if( present( gspacing ) ) dg = max( abs(gspacing), MINIMUM_GSPACING )

    ne = size( kpath, 1 )  ! number of edges of the kpath

    if( ne < 2 ) return   ! no path with less than 2 edges

    do id = 1, 3
      ! if non-rectangular basis vectors were permitted, this
      ! 3x3-matrix has to be found by inversion
      ! gbasis(1:3,id) = 0.
      ! gbasis( id,id) = (2.*Pi)/g%s(id) ! reciprocal lattice vectors

      ! diagonal
      gbasis(id) = (2.*Pi)/g%s(id) ! reciprocal lattice vectors
    enddo ! id

    nkall = 0


    ! calculate the path in advance
    if(o>0) then
      write(o,'(/,3A,I0,9A)') sym, fun, 'connect ', ne, ' k-path edge points:'
      write(o,'(9A)') sym, fun, '-----------------------------------------------------'
    endif ! o>0

    ! write a headline for unit u
    if(u>0) &       ! aligned with fmt='(F12.6,2(3F12.6))'
      write(u,'(9A)') CommentChar, &
                     '    length,', '    k-points (internal coordinates),', &
                                    '    g-points (external coordinates)'

    do i01 = 0, 1

      if( i01 == 1 ) then
        if( nkall < 1 ) return
        if( present( k_list ) ) allocate( k_list(0:3,0:nkall), stat=ist )
        if( present( allkpoints ) ) allocate( allkpoints(0:nkall), stat=ist )
      endif

      ! do for all connections between two edges of the kpath
      do ie = 1, ne-1

        ! compute the path to go
        dkvec(1:3) = kpath(ie+1)%k(1:3) - kpath(ie)%k(1:3) ! internal coords
        ! dgvec(1:3) = matmul( gbasis, dkvec(1:3) )
        dgvec(1:3) = gbasis(1:3) * dkvec(1:3) ! convert to global coords
        lgvec      = sqrt( sum( dgvec*dgvec ) ) ! length in global coords
        nk         = nint( lgvec/dg )
        ! if the two edges points differ, at least one step is needed.

        if( lgvec > 0.1*dg ) then
          nk = max( 1, nk )
          dghere = lgvec/real(nk)
        else  ! lgvec > TINY
          nk = 0
          dghere = lgvec
        endif ! lgvec > TINY

        if( i01 == 0 ) then
          if(o>0) write(o,'(2A,2(I3,A),F10.6,A,I5,A,F9.6)') sym, fun, &
            ie, ' ->', ie+1, ' l =', lgvec, ' in', nk, ' steps, dg =', dghere
        endif ! i01 == 0

        ik0 = 1
        if( ie == 1 ) then
          ! only for the first edge point
          ik0   =  0 ! start the counter at 0
          lgall = -dghere
          ikall = -1 ! counter for k-points
        endif ! ie == 1

        do ik = ik0, nk

          if( nk > 0 ) then ; kvec(1:3) = kpath(ie)%k(1:3) + dkvec(1:3) * real(ik)/real(nk) ! internal coords
          else              ; kvec(1:3) = kpath(ie)%k(1:3) ! for nk == 0
          endif ! nk > 0

          lgall = lgall + dghere
          ikall = ikall + 1

          if( i01 == 0 ) then
            ! gvec  = matmul( gbasis, kvec(1:3) )
            gvec  = gbasis(1:3) * kvec(1:3) ! convert to global coords
            ! write the k-vectors to the result unit u
            if(u/=0) write(u,'(F12.6,2(3F12.6))') lgall, kvec(1:3), gvec(1:3)
          elseif( i01 == 1 ) then ! i01
            !
            if( present( k_list ) ) then
              k_list(1:3,ikall) = kvec(1:3) ; k_list( 0 ,ikall) = lgall
            endif ! present ! k_list
            if( present( allkpoints ) ) then
              allkpoints(ikall) = kpoint_set( kvec, weight=0. )
              allkpoints(ikall)%klen = lgall ! store the path progress in the kpoint-length variable
            endif ! present( allkpoints )

          endif ! i01

        enddo ! ik, loop for kpoints on a straight line

          ! write a blank line (only comment-char)
        if( i01 == 0 ) then ; if(u/=0) write(u,'(A)') CommentChar ; endif ! i01

      enddo ! ie, loop for edges

      nkall = ikall

    enddo ! i01

    if(o>0) then
      write(o,'(9A)') sym, fun, '-----------------------------------------------------'
      if( nkall > 0 ) &
        write(o,'(3A,F10.6,A,I0,A,F9.6)') sym, fun, &
          ' length all =', lgall, ', ', nkall, ' steps avg =', lgall/real(nkall)
      ! inform about the writing to unit u
      if( u>0 ) then
        write(o,'(3A,I0)') sym, fun, ' list of g- and k-points written to unit ', u
      endif ! u>0
      write(o,'(9A)') ''
    endif ! o>0

  endfunction ! generate_path


  function parallelize_tasks( ntasks, nprocs, irank ) result( stst ) ! start and stop index
  use configuration, only: WARNING, ERROR, StopOnError
    integer, intent(in) :: ntasks !! number of independent tasks
    integer, intent(in) :: nprocs !! number of processes
    integer, intent(in) :: irank  !! rank of this process
    integer             :: stst(1:2) !! result: start and stop index in the original list
    ! indicies
    ! 0: all, +1: more tasks, -1: less tasks, 1: start, 2:stop

    character(len=*), parameter :: fun = ' parallelize_tasks: '
    integer :: nt(-1:+1), np(-1:+1), i ! Parallel distribution


    ! result, if an erro occurs
    stst(1) =  1  ! start index
    stst(2) = -1  ! stop  index  ! stop index == -1 means error


    if( nprocs < 2 ) then
      ! unparallelized
      stst(1) = 1      ! start index
      stst(2) = ntasks ! stop  index
      return
    endif ! unparallelized

    ! parallelize the tasks among the processes
    nt( 0) = ntasks                 ! # of all tasks ( task numbers from 0:nkpath)
    np( 0) = nprocs                 ! # of all procs
    np(+1) = modulo( nt(0), np(0) ) ! # of procs with the larger # of tasks
    np(-1) = np(0) - np(+1)         ! # of procs with the smaller # of tasks



    nt(-1) = ( nt(0) - np(+1) )/ np(0) ! smaller # of tasks



    nt(+1) = nt(-1) + 1                ! larger # of tasks



    if( irank < np(-1) ) then
      ! this process has to work less
      i = irank*nt(-1)
      stst(1) = i + 1
      stst(2) = i + nt(-1)
    else  ! irank
      ! this process has to work more
      i = np(-1)*nt(-1) + (irank-np(-1))*nt(+1)
      stst(1) = i + 1
      stst(2) = i + nt(+1)
    endif ! irank

    ! Warning for idle processes
    if( nt(-1) == 0 ) then
      if(o>0) write(o,'(3A,I6,9A)') sym, fun, WARNING(0), np(-1),' processes will be idle!'
    endif ! nt(-1) == 0



  endfunction ! parallelize_tasks


  !! computes the eigenvalue spectrum along a given kpoint path for a fixed Hamiltonian.
  !! Since the diagonalization is iterative, a small step length
  !! in the path is advantageous for convergence of the eigenstates
  status_t function bandpath_c( &
              name, global, &
              g, nband, &
              nxyzs, &
              a, &
              vloc, jspn, & ! coarse grid local potential
              kpath, comm, checkmode, &
              gspacing, fermilevel, wf_start, wf_start_kp, outputunit ) result( ist )
  ! solves the self-consistent Hamiltonian for eigenvalues
  ! on a given path of k-points
  use configuration, only: BST_FileNameExtension, WARNING
  use type_info, only: info
  use type_atom, only: atom
  use type_grid, only: grid
  use type_kpoint, only: kpoint, kpoint_set
  use cg_eigensolver, only: cg_eigen_solve
  use diis_eigensolver, only: diis_eigen_solve
  use subspace, only: subspace_rotation
  use sorting, only: permutation_of
  use prepare, only: otherkpoint_copy ! multiplies states with e^{i \Delta k \vec r}
  use toolbox, only: operator(+)
  use MPItools, only: MPIallsum, MPInprocs, MPImyrank, MPImaster, MPIparallel
  use MPIconst, only: Wtime
  use unitsystem, only: eV, eV_
#ifdef USE_GRACE
  use grace, only: grace_plot
#endif

    character(len=*), intent(in)          :: name           !! project name
    type(info), intent(in)                :: global         !! global information container
    type(grid), intent(in)                :: g              !! the coarse grid
    type(atom), intent(inout)             :: a(:)           !! parallelized list of atoms
    integer, intent(in)                   :: nband          !! number of bands
    integer, intent(in)                   :: nxyzs          !! degrees of freedom of each wave function
    real, intent(inout)                   :: vloc(:,:,:,:)  !! total local potential on the coarse grid
    integer, intent(in)                   :: jspn           !! absolute spin index
    type(kpoint), intent(in)              :: kpath(:)       !! list of kpoints in the path
    MPI_Comm, intent(in)                   :: comm           !! MPI communicator for kpoint parallelization
    logical, intent(in)                   :: checkmode      !! CheckMode 0:run >0:generate and show path
    real, intent(in), optional            :: gspacing       !! step length in reciprocal space, see above
    real, intent(in), optional            :: fermilevel     !! level of the Fermi energy (nice for the plots)
    complex, intent(in), optional         :: wf_start(:,:)  !! dims(nxyzs,nbnd) set of starting guess wave functions, converged wave functions would be best
    type(kpoint), intent(in), optional    :: wf_start_kp    !! kpoint belonging to the start guess wave function set
    iounit_t, intent(in), optional        :: outputunit     !! unit to write results to, 0:no write

    character(len=*), parameter           :: fun = ' bandpath: '
    logical, parameter                    :: USE_K_COPY = .true.
    complex, allocatable                  :: s(:,:), ss(:,:)
    real, allocatable                     :: ene(:) ! energies
    real, allocatable                     :: res(:) ! residual
    real, allocatable                     :: k_list(:,:)
    real, allocatable                     :: bst(:,:)
    type(kpoint)                          :: kp
    real                                  :: kdiff(1:3), k_old(1:3)
    iounit_t                              :: u
    integer                               :: natm, nwf_start, nkpath, ik, ib, ii, sr, jj
    logical                               :: diis_conv
    logical                               :: cg_conv
    real                                  :: dg, ef ! fermilevel
    integer, allocatable                  :: perm(:)
    real                                  :: work_progress, time_start, time_now, time_est
    integer                               :: progress_i10 = 1 ! show 1st time around 1%

    integer                               :: ikst(1:2) ! start and stop index
    integer                               :: nerr, nit
    logical                               :: start = .false.

    ist = 0
    u = 0 


    dg = DEFAULT_GSPACING ; if( present( gspacing ) ) dg = max( abs(gspacing), MINIMUM_GSPACING )

    ef = 0. ; if( present( fermilevel ) ) ef = fermilevel

    if( global%nbnd < global%nbands ) then
      if(o>0) write(o,'(9A)') sym, fun, 'not implemented with band parallelization. return!'
      return
    endif ! bands parallelized

    ist = generate_path( g, kpath, k_list, gspacing )

    nkpath = ubound( k_list, 2 ) ! the list contains 0 as lowest index
    if( nkpath < 0 ) then
      if(o>0) write(o,'(9A)') sym, fun, 'no kpoints on the path, return!'
      return
    endif ! nkpath < 1

    if( checkmode ) return

    ikst(1:2) = parallelize_tasks( nkpath+1, nprocs=MPInprocs(comm), irank=MPImyrank(comm) )
    ikst(1:2) = ikst(1:2) - 1 ! because the list indices start at 0

    natm = size( a, 1 )     ! the number of atoms in this process

   
    ! wave function preparation
    ! set up an extra set of nband wave function
    allocate( s(nxyzs,nband), ss(nxyzs,nband), stat=ist )
    if( ist /= 0 ) stop 'BST bandpath: allocation of wave functions failed!'

    if( present( wf_start ) ) then

      if( size( wf_start, 1 ) /= nxyzs ) stop 'BST dim #1 of start wave functions wrong.'

      nwf_start = size( wf_start , 2 )
      if( nwf_start < nband ) then
        if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'only ', nwf_start, ' inital wave functions passed, use orthogonal solutions.'
      endif ! nwf_start < nband

      do ib = 1, min( nband, nwf_start )
        ! plain copy
        s(:,ib) = cmplx( wf_start(:,ib) )
      enddo ! ib

      do ib = nwf_start+1, nband
        ! fill the higher bands with new states, that will
        ! be orthogonalized against the lower ones in the
        ! first CG steps.
        s(:,ib) = 1.0 ! just a constant (k=[0,0,0] plane wave)
        ! different plane wave modes according to wf_start_k
        ! would be a better guess but wf_start_k is often zero.
      enddo ! ib

      ! the vector to start with
      if( present( wf_start_kp ) ) then
        k_old = wf_start_kp%k
        start = .true.
      else  ! given a kp of the initial wave function
        k_old = 0. ! Gamma point assumed
      endif ! given a kp of the initial wave function


    else ; stop 'BST needs a set of starting wave functions!'
    endif ! present wf_start

    ! prepared

    ! set up arrays to recollect the energy eigenvalues
    allocate( ene(nband), res(nband), bst(0:nkpath,-2:nband), perm(nband) )
    ene = 0. ; res = 0. ; bst = 0. ; perm = 0

    time_start = Wtime()

    open (unit=10, file='bands.out', status='unknown')
    ! do for all k-points in the path
    do ik = ikst(1), ikst(2) ! start to stop

      kp = kpoint_set( k=k_list(1:3,ik), jk=ik )

      ! prepare the wave function set for the next iteration
      if( USE_K_COPY ) then

        kdiff = k_list(1:3,ik) - k_old
        if( start .and. o>0 ) write(o,'(3A,3F7.3,9A)') sym, fun, 'copy from start wf, Delta k =', kdiff
        do ib = 1, nband
          call otherkpoint_copy( s(:,ib), dk=kdiff, g=g )
        enddo ! ib
        k_old = k_list(1:3,ik) ! for next iteration

      endif ! USE_K_COPY

        ! before everything else: subspace_rotation
        sr = subspace_rotation( g=g, & ! coarse grid
                                vloc=vloc, & ! local potential
                                jspin=jspn, & ! spin index
                                atm=a, & ! atoms
                                kp=kp, & ! kpoint
                                energy=ene, &
                                s=  s(:,1:nband), &
                                Ss=ss(:,1:nband), &
                                band_comm=global%band_comm, &
                                band_ioff=global%iobnd, &
                                show_energy=.false. &
                              )

      if( start ) then
        nit = 10 ; start = .false.
      else
        nit = 4
      endif ! start

      do ii = 1, nit

#ifdef BANDSTRUCTURE_USE_CG
       
        cg_conv = cg_eigen_solve( &
                                atm=a, & ! atoms
                                g=g, & ! coarse grid
                                kp=kp, & ! kpoint
                                vloc=vloc, & ! local potential
                                jspin=jspn, & ! spin index
                                energy=ene, &
                                residual=res, &
                                threshold=1.0E-8, & ! in energy units (Ha)
                                maxiter=2, & ! maximum number of CG iterations
                                s=  s(:,1:nband), &
                                Ss=ss(:,1:nband), &
                                show_energy=.false. &
                              )
#endif
        do ib = 1, nband

            diis_conv = diis_eigen_solve( &
                              atm=a, & ! atoms
                              g=g, & ! coarse grid
                              kp=kp, & ! kpoint
                              vloc=vloc, & ! local potential
                              jspin=jspn, & ! spin index
                              s=s(:,ib:ib), & ! wave function
                              energy=ene(ib), &
                              residual=res(ib), &
                              threshold=1E-8, & ! in energy units (Ha)
                              jbnd=ib, & ! global band index
                              mdiis=4,  & ! max.# of DIIS iterations
                              show_energy=.false. &
                            )
            
        enddo ! ib

        sr = subspace_rotation( g=g, & ! coarse grid
                                vloc=vloc, & ! local potential
                                jspin=jspn, & ! spin index
                                atm=a, & ! atoms
                                kp=kp, & ! kpoint
                                energy=ene, &
                                s=  s(:,1:nband), &
                                Ss=ss(:,1:nband), &
                                band_comm=global%band_comm, &
                                band_ioff=global%iobnd, &
                                show_energy=(ii==4) & ! show in last iteration
                              )
      enddo ! ii




      ! results
      bst(ik,-2) = 1.0            ! 1.0 == this ik task has been done
      bst(ik,-1) = k_list(0,ik)   ! progress on the k-path, x-axis for plot
      bst(ik, 0) = ef             ! Fermi level

      perm = permutation_of( nband, ene )

      bst(ik,1:nband) = ene( perm ) ! ordered eigenenergies

      

      !if( 10 > 0 ) write(10,'(F12.6,A,999ES16.8E2)') k_list(0,ik), ' ', (ene(perm(ib)), ib=1,nband), ef

      

    enddo ! ik, parallelized loop for all kpoints on the path

    
    deallocate( s, ss, ene, res, stat=ist )


    ! gather the bandstructure information from the distributed tasks
    if(o>0 .and. MPIparallel(comm) ) write(o,'(3A,9(I0,A))',advance='no') sym, fun, 'gather energies...'
    call MPIallsum( bst, comm )
    if(o>0 .and. MPIparallel(comm) ) write(o,'(9A)') 'done!'


    ! very important self-check:
    ! bst(:,-2) has been set to 1.0 for each completed task
    nerr = count( bst(:,-2) /= 1.0 )
    if( nerr > 0 ) then ! that''s pretty bad
      if(o>0) write(o,'(3A,9(I0,A))') sym, fun, WARNING(0), nerr, ' tasks have not been completed correctly.'
    endif ! nerr > 0

#ifdef USE_GRACE
    if( MPImaster(comm) ) then ! xmGRACE external plotting tool
      ist = grace_plot( file=name+BST_FileNameExtension, ydata=bst(0:,0:)*eV, & ! required args
                        xdata=bst(0:,-1), labels=(/'k-path','E in'+eV_/), &
                        title='Band Structure', subtitle=name, scheme='bandstructure' ) ! scheme is an optional argument
    endif ! Master
#endif

    if( MPImaster(comm) ) then
      !if( 10 > 0 ) write(10,'(F12.6,A,999ES16.8E2)') k_list(0,ik), ' ', (ene(perm(ib)), ib=1,nband), ef
      do ii=0, ubound(bst, 1)
        write(10, '(999ES16.8E2)') bst(ii, -1:)
      enddo
      !call print_bandstructure(nkpath+1, nband, bst(:,:))
    endif 

    deallocate( bst, perm, stat=ist ) ! drop the band structure energies and wave functions
  endfunction ! bandpath


  status_t function test( )
    write(*,*,iostat=test) __FILE__,' no module test implemented!'
  endfunction 



endmodule ! bandstructure

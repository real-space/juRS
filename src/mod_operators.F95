#ifdef DEBUG_ALL
#define DEBUG
#endif

! #define DEBUG
! #define FULL_DEBUG

#ifdef RHO_ON_GD
  ! this version generates the smooth density from the smooth wave functions by
  !   rho(r)_dense = sum_bsk | interpolate( psi_bsk(r)_coarse ) |^2
#else
  ! this PreProcessor flag deactivates the non-locality of the overlap matrix
  ! and generates the smooth density from the smooth wave functions on the coarse grid by
  !   rho(r)_dense = interpolate( sum_bsk |psi_bsk(r)_coarse|^2 )
#endif

#define USE_KINETIC_MERGED_LOOPS

#ifdef DEBUG
#define cDBG   
#else
#define cDBG !
#endif


#ifdef R1_C2
! This file will be preprocessed twice, first
! with R1_C2 == 1, and then with R1_C2 == 2
! then the two parts are concatenated to one source.
! This requires that the module head has to be
! appearing, when R1_C2 == 1 and the line "end module <name>"
! with R1_C2 == 2.
#if R1_C2 == 1


!! @author Paul Baumeister
!! @version 4.04
!!
!! Hamiltonian (+overlap matrix), preconditioner
module operators
  use configuration, only: o ! output unit, 0: no output
implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: sym = 'OPS' !! module symbol

  public :: Bloch_phase
  public :: Hmt
  public :: scalar_product
  !public :: gradient
  public :: precon_a ! needed by CG and DIIS eigensolver
  public :: precon_Nf1 ! needed by CG and DIIS eigensolver
#ifdef EXTENDED
  public :: test
#endif

  interface Hmt ! the Hamiltonian and overlap matrix
    module procedure Hmt_r, Hmt_c
  endinterface

  interface scalar_product
    ! the scalar product between two rank1 arrays
    module procedure scalar_product_r, scalar_product_c, &
    ! the scalar product between two rank2 arrays
                   scalar_products_r, scalar_products_c
  endinterface

  interface complex_conjugate ! this function returns conjg(x) if x is complex, x if x is real (scalars only)
    module procedure complex_conjugate_r, complex_conjugate_c
  endinterface

  interface kinetic ! the kinetic energy (or -1/2*Laplacian)
    module procedure kinetic_r, kinetic_c
  endinterface

  

 ! this module provides the nonlocal operator |p> <p|,
 ! by the three module procedures
 !       - project:
 !             Find the coefficient vector c = <p|f>  (local)
 !       - reduce:
 !             Transport the information via the atomic communicator
 !              --> all members to the atom owner ("reduce")
 !              --> all members to all members ("allreduce")
 !              --> the atom owner to all members ("bcast")
 !       - add:
 !             |f> := |f> + c * |p> (local)

  interface scalar_norm2 ! the 2-norm of a rank4 array
    module procedure scalar_norm2_r, scalar_norm2_c
  endinterface



  interface precon_Nf1 ! the preconditioner_rank4,Nf=1
    module procedure precon_Nf1_r, precon_Nf1_c
  endinterface

  interface precon_a ! the preconditioner_rank4,Nf=4
    module procedure precon_a_r, precon_a_c
  endinterface

  interface projection_operation
    module procedure projection_operation_r, projection_operation_c
  endinterface

  interface addition_operations
    module procedure addition_operations_r, addition_operations_c
  endinterface

  contains

! end of the module head
#endif


#if R1_C2 == 1
#define REAPLEX real
#else
#define REAPLEX complex
#endif



#if R1_C2 == 1
  !! the action of the Hamiltonian and the overlap matrix
  !!
  !!  H|ket> = [ T + V_loc + |p> a%Hm <p| ] |ket>
  !!  S|ket> = [ I^2 + |p> a%dq <p| ] |ket>
  !!
  subroutine Hmt_r( &
#else
  subroutine Hmt_c( &
#endif
                    g, vloc, jspin, a, kp, ket, Hket, Sket, cket )
  use type_atom, only: atom
  use type_kpoint, only: kpoint
  use type_grid, only: grid
!   use MPItools, only: MPIreduce0 ! send to the atom owner
!   use MPItools, only: MPIbcast0 ! get from the atom owner
  use MPItools, only: MPIbarrier
  use atomicomm, only: AtomBcast, AtomReduce
  implicit none
    ! parameters
    character(len=*), parameter     :: fun = ' Hmt: '
    ! arguments
    type(grid), intent(in)          :: g
    real, intent(in)                :: vloc(:,:,:,:) ! local potential(:,:,:,ns)
    integer, intent(in)             :: jspin         ! index of the spin
    type(atom), intent(in)          :: a(:)          ! local ist of atoms
    type(kpoint), intent(in)        :: kp            ! kpoint
    REAPLEX, intent(in)             :: ket(:,1:)        ! |ket> vectors
    REAPLEX, intent(out)            :: Hket(:,1:)       ! Hamiltonian times |ket>s
    REAPLEX, intent(out)            :: Sket(:,1:)       ! overlap matrix times |ket>s
    REAPLEX, intent(out), optional  :: cket(:,1:)       ! cket(g%naprj) c=<p|ket> coefficients
    ! local vars
    REAPLEX                         :: cpr(g%naprj,size(ket,2))
    REAPLEX                         :: cad(2*g%naprj,size(ket,2))
    integer                         :: n, ia, np, io, nn, ii
#ifdef DEBUG
    integer                         :: nerr, i1, i2
#endif

    nn = size(ket,2) ! number of kets to be processed simultaneously



    ! convention: the projection coefficients in cpr are stored
    ! io(1)+1 : io(1)+np(1), ..., io(na)+1 : io(na)+np(na)
    !      a(1)%c                        a(na)%c
    cpr = 0. ! init
    ! project it onto the atoms (projection operation is non-local)

    call projection_operation( g, kp, a, ket, cpr )

    

    n = g%ng(1)*g%ng(2)*g%ng(3) ! *g%ng(4) for non-collinear spin

    ! calculate the kinetic energy -1/2 Laplacian of ket (Hartree units)
    ! and the overlap matrix, if present
    do ii = 1, nn
      ! TODO move the ii-loop inside kinetic
      ! apply the kinetic energy and the local potential
      call kinetic( g, kp, v=ket(:,ii), Tv=Hket(:,ii), Sv=Sket(:,ii), Vloc=Vloc(:,:,:,jspin) )

    enddo ! ii
    

    ! for the atomic communicaton an reduce must be used
    ! as 1st step and a bcast as 2nd after the Hamiltonian
    ! matrix has applied by the owner process.
    do ia = 1, size(a)
      np = a(ia)%s%mlnm ! number of projectors
      io = a(ia)%offprj ! offset
      call AtomReduce( cpr(io+1:io+np,1:nn), g%comm, a(ia)%owner, a(ia)%plist )
    enddo ! ia

    
    ! double convention: the projection coefficients in cad are stored
    ! 2*io(1)+1 : 2*io(1)+np(1), 2*io(1)+np(1)+1 : io(1)+2*np(1), 2*io(2)+1 :....
    !      a(1)%Hc                             a(1)%Sc                       a(2)%Hc
    cad = 0.
    ! for all owned atoms in the process
    do ia = 1, size(a)
      if( g%rank /= a(ia)%owner ) cycle

      np = a(ia)%s%mlnm ! number of projectors
      io = a(ia)%offprj ! offset

      do ii = 1, nn
        ! apply the Hamiltonian_matrix  ! simple matrix multiplication
        cad(2*io+1:2*io+np,ii) = matmul( a(ia)%Hm(:,:,jspin), cpr(io+1:io+np,ii) )
        ! apply the Norm deficit_matrix ! simple matrix multiplication
        cad(2*io+np+1:2*io+2*np,ii) = matmul( a(ia)%s%overlap_matrix, cpr(io+1:io+np,ii) )
      enddo ! ii

    enddo ! ia

    ! this operation distributes the coeffcients
    ! multiplied by the Hamiltonian matrix of the owner
    ! process to the other processes that contribute to
    ! this atom
    ! for simultaneous communication of both vectors avoiding overhead

    do ia = 1, size(a)
      np = a(ia)%s%mlnm ! number of projectors
      io = a(ia)%offprj ! offset
      call AtomBcast( cad(2*io+1:2*io+2*np,1:nn), g%comm, a(ia)%owner, a(ia)%plist )
    enddo ! ia
    

    ! |HPsi> := (T+Vloc)|Psi> + |p>*dH*c where c=<p|Psi> has been calculated before (see cpr)
    call addition_operations( g, kp, a, cad, Hket, Sket )

    !! put as before
     if( present( cket ) ) cket = cpr ! get a copy for further use

    

  endsubroutine ! Hmt


#if R1_C2 == 1
  subroutine projection_operation_r( &
#else
  subroutine projection_operation_c( &
#endif
                                     g, kp, a, ket, cpr )
  use type_proj, only: project
  use type_grid, only: grid
  use type_atom, only: atom
  use type_kpoint, only: kpoint
  implicit none
    ! parameter
    character(len=*), parameter     :: fun = ' prj_ops: '
    ! arguments
    type(grid), intent(in)          :: g
    type(kpoint), intent(in)        :: kp
    type(atom), intent(in)          :: a(:)
    REAPLEX, intent(in)             :: ket(:,1:)
    REAPLEX, intent(out)            :: cpr(:,1:)
    ! local variables
    integer                         :: ia, np, io, ii, nn

    nn = size(ket,2) ! number of kets that are processed simultaneously
#ifdef DEBUG
    if( size(cpr,2) < nn ) stop 'projection_operation: needs more cpr''s'
!     if( size(cpr,2) /= nn ) stop 'projection_operation: needs more cpr''s' ! stricter
#endif

!!!!!$omp parallel if( nn > 1 )
!!!!$omp do private(ii,ia,io,np) schedule(static)
      do ii = 1, nn
        do ia = 1, size(a)
          np = a(ia)%s%mlnm
          io = a(ia)%offprj ! offset
          call project( a(ia)%prj, ket(:,ii), cpr(io+1:io+np,ii), dV=g%hvol, eikL=kp%eik )
        enddo ! ia
      enddo ! ii
!!!!$omp end do
!!!!$omp end parallel
  endsubroutine ! projection_operation


#if R1_C2 == 1
  subroutine addition_operations_r( &
#else
  subroutine addition_operations_c( &
#endif
                                    g, kp, a, cad, Hket, Sket )
  use type_proj, only: add
  use type_grid, only: grid
  use type_atom, only: atom
  use type_kpoint, only: kpoint
  implicit none
    ! parameter
    character(len=*), parameter   :: fun = ' add_ops: '
    ! arguments
    type(grid), intent(in)      :: g
    type(kpoint), intent(in)    :: kp
    type(atom), intent(in)      :: a(:)
    REAPLEX, intent(in)         :: cad(:,1:)
    REAPLEX, intent(inout)      :: Hket(:,1:)
    REAPLEX, intent(inout)      :: Sket(:,1:)
    ! local variables
    integer :: ia, np, io, ioh, ios
    integer :: nn, ii

    nn = size( cad,2) ! number of kets that are processed simultaneously
#ifdef DEBUG
    if( size(Hket,2) < nn ) stop 'add_ops: not enough Hkets'
    if( size(Sket,2) < nn ) stop 'add_ops: not enough Skets'
#endif

!!!!$omp parallel if( nn > 1 )
!!!!$omp do private(ii,ia,np,io,ioh,ios) schedule(static)
    do ii = 1, nn
      do ia = 1, size(a)
        np = a(ia)%s%mlnm
        io = a(ia)%offprj ! offset
        ioh = 2*io
        ios = ioh+np
        call add( a(ia)%prj, Hket(:,ii), cad(ioh+1:ioh+np,ii), eikL=kp%eik )
        call add( a(ia)%prj, Sket(:,ii), cad(ios+1:ios+np,ii), eikL=kp%eik )
      enddo ! ia
    enddo ! ii
!!!!$omp end do
!!!!$omp end parallel
  endsubroutine ! addition_operations


#if R1_C2 == 1
  !! kinetic energy and overlap operator
  subroutine kinetic_r( &
#else
  subroutine kinetic_c( &
#endif
                        g, kp, v, Tv, Sv, Vloc )
  use boundary, only: dataexchange
  use type_grid, only: grid
  use type_kpoint, only: kpoint
#ifdef DEBUG
  use configuration, only: WARNING
#endif
  implicit none
    ! parameter
    character(len=*), parameter   :: fun = ' kinetic: '

    ! arguments
    type(grid), intent(in)      :: g
    type(kpoint), intent(in)    :: kp
    REAPLEX, intent(in)   :: v(:)
    REAPLEX, intent(out)  :: Tv(:)
    REAPLEX, intent(out)  :: Sv(:) ! localized overlap operator
    real, intent(in), optional :: Vloc(:,:,:) ! local potential

    ! local variables
    real                  :: tc(-15:16,1:3) ! (-nfmax:nfmax,3)
    integer               :: ix, iy, iz, ii, ixyz, ixy
    integer               :: id, nf!, nvv, nvvv
    real                  :: v0
    REAPLEX               :: tx, ty, tz, t0
    
    !REAPLEX               :: vec(1-g%nf(1):g%ng(1)+g%nf(1), &
    !                             1-g%nf(2):g%ng(2)+g%nf(2), &
    !                             1-g%nf(3):g%ng(3)+g%nf(3))

    REAPLEX,allocatable               :: vec(:,:,:)

  
    allocate (vec(1-g%nf(1):g%ng(1)+g%nf(1), &
                                 1-g%nf(2):g%ng(2)+g%nf(2), &
                                 1-g%nf(3):g%ng(3)+g%nf(3)))

    tc = 0.
    do id = 1, 3
      nf = get_F2ndD_coefficients( g%nf(id), h=g%h(id), c=tc(0:,id) )
      tc(-nf:-1,id) = tc(nf:1:-1,id)
    enddo ! id
#ifdef DEBUG
    do id = 1, 3 ! extra check
      if( abs( sum( tc(:,id) ) ) > 1E-12 ) stop 'kinetic: FDcoeffs do not sum up to 0.0'
    enddo ! id
#endif
    tc = - tc / 2. ! due to Hartree unit system


  
    Sv = v ! overlap matrix of grid points is unity


    vec = 0.0 ! init and fill the central region of the vector with halos
    vec( 1:g%ng(1), 1:g%ng(2), 1:g%ng(3) ) = reshape( v, g%ng(1:3) )
    ! use MPI communication to exchange data and fill the halos
    !write (*,*) 'kinetic g%nf', g%nf
    call dataexchange( g, vec, kp )
    !write(*,*) 'using nf fofr kinetic ', g%nf
! !$omp end master


!!!!$omp parallel
!!!!$omp do private(ixyz,ix,iy,iz,t1,ii,tx,ty,tz,v0,t0)
     do ixyz = 1, g%ng(3)*g%ng(2)*g%ng(1)

          ix = mod( ixyz-1, g%ng(1) ) + 1;
          iy = mod( (ixyz-1)/g%ng(1), g%ng(2) ) + 1;
          iz = (ixyz-1)/(g%ng(1)*g%ng(2)) + 1;

!           t1 = 0.0
!           do ii = -g%nf(1), g%nf(1)
!             t1 = t1 + vec(ix+ii,iy,iz) * tc(ii,1)
!           enddo ! ii
!           do ii = -g%nf(2), g%nf(2)
!             t1 = t1 + vec(ix,iy+ii,iz) * tc(ii,2)
!           enddo ! ii
!           do ii = -g%nf(3), g%nf(3)
!             t1 = t1 + vec(ix,iy,iz+ii) * tc(ii,3)
!           enddo ! ii
!           Tv(ixyz) = t1


          t0 = vec(ix,iy,iz)

          v0 = 0. ; if( present( Vloc ) ) v0 = Vloc(ix,iy,iz)

          tx = t0 * tc(0,1)
          do ii = 1, g%nf(1)
            tx = tx + ( vec(ix-ii,iy,iz) + vec(ix+ii,iy,iz) ) * tc(ii,1)
          enddo ! ii

          ty = t0 * tc(0,2)
          do ii = 1, g%nf(2)
            ty = ty + ( vec(ix,iy-ii,iz) + vec(ix,iy+ii,iz) ) * tc(ii,2)
          enddo ! ii

          tz = t0 * tc(0,3)
          do ii = 1, g%nf(3)
            tz = tz + ( vec(ix,iy,iz-ii) + vec(ix,iy,iz+ii) ) * tc(ii,3)
          enddo ! ii
          !Tv(ixyz) = v0 * t0
          Tv(ixyz) = tx + ty + tz + v0 * t0


    enddo ! ixyz
!!!$omp end do
!!!!$omp end parallel

    deallocate(vec)
  endsubroutine ! kinetic

!!#define USE_KINETIC_MERGED_LOOPS

#if R1_C2 == 1
  !! kinetic energy and overlap broadener
  subroutine kinetic_rold( &
#else
  subroutine kinetic_cold( &
#endif
                        g, kp, v, Tv, Sv, Vloc )
  use boundary, only: dataexchange
  use type_grid, only: grid
  use type_kpoint, only: kpoint
#ifdef DEBUG
  use configuration, only: WARNING
#endif
  implicit none
    ! parameter
    character(len=*), parameter   :: fun = ' kinetic: '
#ifdef RHO_ON_GD
     ! coefficients for the range-1 overlap operator
     real, parameter             :: d =   27./2.**12  ! vertices
     real, parameter             :: c =   90./2.**12  ! edges
     real, parameter             :: b =  300./2.**12  ! faces
     real, parameter             :: a = 1000./2.**12  ! volume
     real, parameter :: sc(-1:1,-1:1,-1:1) = reshape( (/ d,c,d, c,b,c, d,c,d, &
                                                         c,b,c, b,a,b, c,b,c, &
                                                         d,c,d, c,b,c, d,c,d /), (/3,3,3/) )
#endif
    ! arguments
    type(grid), intent(in)      :: g
    type(kpoint), intent(in)    :: kp
    REAPLEX, intent(in)   :: v(:)
    REAPLEX, intent(out)  :: Tv(:)
    REAPLEX, intent(out)  :: Sv(:) ! localized overlap operator
    real, intent(in), optional :: Vloc(:,:,:) ! local potential

    ! local variables
    real                  :: tc(-15:16,1:3) ! (-nfmax:nfmax,3)
    integer               :: ix, iy, iz, ii, ixyz, ixy
    integer               :: id, nf!, nvv, nvvv
    real                  :: v0
    REAPLEX               :: tx, ty, tz, t0
    REAPLEX               :: t1
    REAPLEX               :: t2(g%ng(1))
    REAPLEX               :: t3(g%ng(1),g%ng(2))
    REAPLEX               :: vec(1-g%nf(1):g%ng(1)+g%nf(1), &
                                 1-g%nf(2):g%ng(2)+g%nf(2), &
                                 1-g%nf(3):g%ng(3)+g%nf(3))
! !$omp master

    tc = 0.
    do id = 1, 3
      nf = get_F2ndD_coefficients( g%nf(id), h=g%h(id), c=tc(0:,id) )
      tc(-nf:-1,id) = tc(nf:1:-1,id)
    enddo ! id
#ifdef DEBUG
    do id = 1, 3 ! extra check
      if( abs( sum( tc(:,id) ) ) > 1E-12 ) stop 'kinetic: FDcoeffs do not sum up to 0.0'
    enddo ! id
#endif
    tc = - tc / 2. ! due to Hartree unit system

#ifndef RHO_ON_GD
  ! this PreProcessor flag deactivates the non-locality of the overlap matrix
  ! and generates the smooth density from the smooth wave functions on the coarse grid by
  !   rho(r)_dense = interpolate( sum_bsk |psi_bsk(r)_coarse|^2 )
    Sv = v ! overlap matrix of grid points is unity
#endif

    vec = 0.0 ! init and fill the central region of the vector with halos
    vec( 1:g%ng(1), 1:g%ng(2), 1:g%ng(3) ) = reshape( v, g%ng(1:3) )
    ! use MPI communication to exchange data and fill the halos
    call dataexchange( g, vec, kp )

! !$omp end master

#ifndef USE_KINETIC_MERGED_LOOPS

    ixyz = 0
    do iz = 1, g%ng(3)
      do iy = 1, g%ng(2)
        do ix = 1, g%ng(1)
          ixyz = ixyz+1
          t1 = 0.0
          do ii = -g%nf(1), g%nf(1)
            t1 = t1 + vec(ix+ii,iy,iz) * tc(ii,1)
          enddo ! ii
!           t1 = vec(ix,iy,iz) * tc(0,1)
!           do ii = 1, g%nf(1)
!             t1 = t1 + ( vec(ix+ii,iy,iz) + vec(ix-ii,iy,iz) ) * tc(ii,1) ! even stencil
!           enddo ! ii
          Tv(ixyz) = t1
#ifdef RHO_ON_GD
  ! this version generates the smooth density from the smooth wave functions by
  !   rho(r)_dense = sum_bsk | interpolate( psi_bsk(r)_coarse ) |^2
          ! range-1 overlap operator
          Sv(ixyz) = sum( vec(ix-1:ix+1,iy-1:iy+1,iz-1:iz+1) * sc )
#endif
        enddo ! ix
      enddo ! iy
    enddo ! iz

    ixyz = 0
    do iz = 1, g%ng(3)
      do iy = 1, g%ng(2)
        t2(:) = 0.0
        do ii = -g%nf(2), g%nf(2)
          t2(:) = t2(:) + vec(1:g%ng(1),iy+ii,iz) * tc(ii,2)
        enddo ! ii
!         t2 = vec(1:g%ng(1),iy,iz) * tc(0,2)
!         do ii = 1, g%nf(2)
!           t2 = t2 + ( vec(1:g%ng(1),iy-ii,iz) + vec(1:g%ng(1),iy+ii,iz) ) * tc(ii,2) ! even stencil
!         enddo ! ii

        Tv(ixyz+1:ixyz+g%ng(1)) = Tv(ixyz+1:ixyz+g%ng(1)) + t2(:)
        ixyz = ixyz+g%ng(1)
      enddo ! iy
    enddo ! iz

    ixyz = 0
    do iz = 1, g%ng(3)
      t3(:,:) = 0.0
      do ii = -g%nf(3), g%nf(3)
        t3(:,:) = t3(:,:) + vec(1:g%ng(1),1:g%ng(2),iz+ii) * tc(ii,3)
      enddo ! ii
!       t3 = vec(1:g%ng(1),1:g%ng(2),iz) * tc(0,3)
!       do ii = 1, g%nf(3)
!         t3 = t3 + ( vec(1:g%ng(1),1:g%ng(2),iz-ii) + vec(1:g%ng(1),1:g%ng(2),iz+ii) ) * tc(ii,3) ! even stencil
!       enddo ! ii

      ixy = 0
      do iy = 1, g%ng(2)
        Tv(ixyz+ixy+1:ixyz+ixy+g%ng(1)) = Tv(ixyz+ixy+1:ixyz+ixy+g%ng(1)) + t3(:,iy)
        ixy = ixy + g%ng(1)
      enddo ! iy
      ixyz = ixyz+g%ng(1)*g%ng(2) ! forward
    enddo ! iz

#else

!$omp parallel
!$omp do private(ixyz,ix,iy,iz,t1,ii,tx,ty,tz,v0,t0)
     do ixyz = 1, g%ng(3)*g%ng(2)*g%ng(1)

          ix = mod( ixyz-1, g%ng(1) ) + 1;
          iy = mod( (ixyz-1)/g%ng(1), g%ng(2) ) + 1;
          iz = (ixyz-1)/(g%ng(1)*g%ng(2)) + 1;

!           t1 = 0.0
!           do ii = -g%nf(1), g%nf(1)
!             t1 = t1 + vec(ix+ii,iy,iz) * tc(ii,1)
!           enddo ! ii
!           do ii = -g%nf(2), g%nf(2)
!             t1 = t1 + vec(ix,iy+ii,iz) * tc(ii,2)
!           enddo ! ii
!           do ii = -g%nf(3), g%nf(3)
!             t1 = t1 + vec(ix,iy,iz+ii) * tc(ii,3)
!           enddo ! ii
!           Tv(ixyz) = t1


          t0 = vec(ix,iy,iz)

          v0 = 0. ; if( present( Vloc ) ) v0 = Vloc(ix,iy,iz)

          tx = t0 * tc(0,1)
          do ii = 1, g%nf(1)
            tx = tx + ( vec(ix-ii,iy,iz) + vec(ix+ii,iy,iz) ) * tc(ii,1)
          enddo ! ii

          ty = t0 * tc(0,2)
          do ii = 1, g%nf(2)
            ty = ty + ( vec(ix,iy-ii,iz) + vec(ix,iy+ii,iz) ) * tc(ii,2)
          enddo ! ii

          tz = t0 * tc(0,3)
          do ii = 1, g%nf(3)
            tz = tz + ( vec(ix,iy,iz-ii) + vec(ix,iy,iz+ii) ) * tc(ii,3)
          enddo ! ii

          Tv(ixyz) = tx + ty + tz + v0 * t0

#ifdef RHO_ON_GD
  ! this version generates the smooth density from the smooth wave functions by
  !   rho(r)_dense = sum_bsk | interpolate( psi_bsk(r)_coarse ) |^2
          ! range-1 overlap operator
          Sv(ixyz) = sum( vec(ix-1:ix+1,iy-1:iy+1,iz-1:iz+1) * sc )
#endif
    enddo ! ixyz
!$omp end do
!$omp end parallel

#endif
  endsubroutine ! kinetic


#if R1_C2 == 1
  subroutine precon_Nf1_r( &
#else
  subroutine precon_Nf1_c( &
#endif
                           g, kp, v, Pv ) ! Preconditioner with Nf=1, rank4
  use boundary, only: dataexchange
  use type_grid, only: grid
  use type_kpoint, only: kpoint
  implicit none
  ! parameter
  character(len=*), parameter :: fun = ' precon_Nf1: '
  real, parameter             :: c000 = 1./2.
  real, parameter             :: c100 = 1./12. ! c0 + c1 * 6 neighbors = 1.0
  ! arguments
  type(grid), intent(in)      :: g
  type(kpoint), intent(in)    :: kp
  REAPLEX, intent(in)         :: v (:)
  REAPLEX, intent(out)        :: Pv(:)
  ! local variables
  integer                     :: ix, iy, iz, is, ixyzs, ixyzs_start, nxyz
  REAPLEX                     :: vec(1-1:g%ng(1)+1,1-1:g%ng(2)+1,1-1:g%ng(3)+1)
  type(grid)                  :: myg

  myg = g ; myg%nh = 1 ! make a copy of the grid descriptor and set nh=1

  nxyz = g%ng(1)*g%ng(2)*g%ng(3)
  do is = 1, g%ng(4)
    vec = 0.0

    ixyzs_start = (is-1)*nxyz

!     ixyzs = ixyzs_start
!     do iz = 1, g%ng(3)
!       do iy = 1, g%ng(2)
!         do ix = 1, g%ng(1)
!           ixyzs = ixyzs+1
!           vec(ix,iy,iz) = v(ixyzs)
!         enddo ! ix
!       enddo ! iy
!     enddo ! iz
    vec(1:g%ng(1),1:g%ng(2),1:g%ng(3)) = reshape( v(1+(is-1)*nxyz:is*nxyz), g%ng(1:3) )

!         write(8,'(2A,16ES16.6E2)') fun, '(before) ', vec(0:7,1,1)
    call dataexchange( myg, vec, kp )
!         write(8,'(2A,16ES16.6E2)') fun, '(after)  ', vec(0:7,1,1)

    ixyzs = ixyzs_start
    do iz = 1, g%ng(3)
      do iy = 1, g%ng(2)
        do ix = 1, g%ng(1)
          ixyzs = ixyzs+1
          Pv(ixyzs) = c000* vec(ix,iy,iz) + c100* ( &
            vec(ix-1,iy,iz) + vec(ix+1,iy,iz) + &
            vec(ix,iy-1,iz) + vec(ix,iy+1,iz) + &
            vec(ix,iy,iz-1) + vec(ix,iy,iz+1) )
        enddo ! ix
      enddo ! iy
    enddo ! iz

  enddo ! is

  endsubroutine ! precon_Nf1






#if R1_C2 == 1
  subroutine precon_a_r( &
#else
  subroutine precon_a_c( &
#endif
                         g, kp, v, Pv ) ! Preconditioner with Nf=4, rank4
  use boundary, only: dataexchange
  use type_grid, only: grid
  use type_kpoint, only: kpoint
  implicit none
  ! parameter
  character(len=*), parameter   :: fun = ' precon_a: '
  ! arguments
  type(grid), intent(in)      :: g
  type(kpoint), intent(in)    :: kp
  REAPLEX, intent(in)         :: v (:)
  REAPLEX, intent(out)        :: Pv(:)
  ! local variables
  integer  :: ix, iy, iz, is, ixyzs, nxyz!, ixyzs_start
  integer  :: jy, jz, nh1!, jx
  REAPLEX  :: vec(1-g%nh(1):g%ng(1)+g%nh(1),1-g%nh(2):g%ng(2)+g%nh(2),1-g%nh(3):g%ng(3)+g%nh(3))!, t
  real     :: c(-g%nh(1):+g%nh(1),-g%nh(2):+g%nh(2),-g%nh(3):+g%nh(3)), ct, arg

    ! prepare coefficients c
    c = 0.
    if( any( g%nh < 0 ) ) stop 'OPS precon_a: g%NH < 0 impossible'
    do iz = 0, g%nh(3)
      do iy = 0, g%nh(2)
        do ix = 0, g%nh(1)
          arg = 0.75 * ( ix*ix + iy*iy + iz*iz )
          ct  = exp(-arg)
          c( ix, iy, iz) = ct
          c(-ix, iy, iz) = ct
          c( ix,-iy, iz) = ct
          c(-ix,-iy, iz) = ct
          c( ix, iy,-iz) = ct
          c(-ix, iy,-iz) = ct
          c( ix,-iy,-iz) = ct
          c(-ix,-iy,-iz) = ct
        enddo ! ix
      enddo ! iy
    enddo ! iz
    ct = sum( c )
    c = c/ct ! normalize


    nxyz = g%ng(1)*g%ng(2)*g%ng(3)
    Pv(1:nxyz*g%ng(4)) = 0. ! init

    do is = 1, g%ng(4)
      vec = 0. ! init as zero and fill the center region with data
      vec(1:g%ng(1),1:g%ng(2),1:g%ng(3)) = reshape( v(1+(is-1)*nxyz:is*nxyz), g%ng(1:3) )

!         write(8,'(2A,16ES16.6E2)') fun, '(before) ', vec(0:7,1,1)
      call dataexchange( g, vec, kp )
!         write(8,'(2A,16ES16.6E2)') fun, '(after)  ', vec(0:7,1,1)

#define OPTIMIZE
#ifndef OPTIMIZE
      !!! unoptimized version (very slow)
      ixyzs = (is-1)*nxyz ! index offset
      do iz = 1, g%ng(3)
        do iy = 1, g%ng(2)
          do ix = 1, g%ng(1)
            ixyzs = ixyzs+1
            Pv(ixyzs) = sum( vec(ix-g%nh(1):ix+g%nh(1),iy-g%nh(2):iy+g%nh(2),iz-g%nh(3):iz+g%nh(3)) * c )
          enddo ! ix
        enddo ! iy
      enddo ! iz
#else
!       !!! optimized version
!       nh1 = g%nh(1)
!       ixyzs = (is-1)*nxyz ! index offset
!       do iz = 1, g%ng(3)
!         do iy = 1, g%ng(2)
!           do ix = 1, g%ng(1)
!             ixyzs = ixyzs+1
!             t = 0. ! init
!             do jz = -g%nh(3), g%nh(3)
!               do jy = -g%nh(2), g%nh(2)
!                 t = t + dot_product( vec(ix-nh1:ix+nh1,iy+jy,iz+jz), c(-nh1:nh1,jy,jz) )
!               enddo ! jy
!             enddo ! jz
!             Pv(ixyzs+ix) = t
!           enddo ! ix
!         enddo ! iy
!       enddo ! iz

      !!! optimized version2
      nh1 = g%nh(1)
      ixyzs = (is-1)*nxyz ! index offset
      do iz = 1, g%ng(3)
        do iy = 1, g%ng(2)
          do jz = -g%nh(3), g%nh(3)
            do jy = -g%nh(2), g%nh(2)
              do ix = 1, g%ng(1)
                Pv(ixyzs+ix) = Pv(ixyzs+ix) + dot_product( vec(ix-nh1:ix+nh1,iy+jy,iz+jz), c(-nh1:nh1,jy,jz) )
              enddo ! ix
            enddo ! jy
          enddo ! jz
          ixyzs = ixyzs+g%ng(1) ! forward offset
        enddo ! iy
      enddo ! iz
#endif
    enddo ! is

  endsubroutine ! precon_a




#if R1_C2 == 1
  REAPLEX function scalar_product_r( &
#else
  REAPLEX function scalar_product_c( &
#endif
                                     bra, ket, h3, comm ) result( s )
  ! <bra|ket> ( the vector bra will be complex conjugated )
  use MPItools, only: operator(.MPIsum.)
  implicit none
    ! arguments
    REAPLEX, intent(in)               :: bra(:), ket(:)
    real, intent(in)                  :: h3             !! product of grid spacings
    integer, intent(in), optional     :: comm           !! MPIcommunicator for global reduction
    ! local vars
    integer                           :: ixyzs

#ifdef DEBUG
    if( size(bra,1) /= size(ket,1) ) stop 'OPS: scalar_product: dimensions of A and B do not match.'
#endif

!     s = 0.
!     do ixyzs = 1, size(bra,1)
! #if R1_C2 == 1
!       s = s + bra(ixyzs)*ket(ixyzs)         ! real
! #else
!       s = s + conjg(bra(ixyzs))*ket(ixyzs)  ! complex
! #endif
!     enddo ! ixyzs
    s = dot_product( bra, ket )

    if( present( comm ) ) s = s .MPIsum. comm

#ifdef NaN_SEARCH
    if(s/=s) write(*,*) sym, ' scalar_product: NaN in result.'
#endif
    s = s*h3
  endfunction ! scalar_product



#if R1_C2 == 1
  function scalar_products_r( &
#else
  function scalar_products_c( &
#endif
                              bras, kets, h3, comm ) result( s )
  ! <bra|ket> ( the vectors bra will be complex conjugated )
  use MPItools, only: MPIallsum
  implicit none
    ! arguments
    REAPLEX, intent(in)               :: bras(:,:), kets(:,:)
    real, intent(in)                  :: h3             !! product of grid spacings
    integer, intent(in), optional     :: comm           !! MPIcommunicator for global reduction
    ! result
    REAPLEX                           :: s(size(bras,2),size(kets,2))
    ! local vars
    integer                           :: ixyzs, ib, ik

#ifdef DEBUG
    if( size(bras,1) /= size(kets,1) ) stop 'OPS: scalar_product: dimensions of A and B do not match.'
#endif

    ! todo: insert a BLAS call here
!     s = 0.
    do ik = 1, size(kets,2)
      do ib = 1, size(bras,2)
!     do ixyzs = 1, size(bras,1)
! #if R1_C2 == 1
!       s(ib,ik) = s(ib,ik) + bras(ixyzs,ib)*kets(ixyzs,ik)         ! real
! #else
!       s(ib,ik) = s(ib,ik) + conjg(bras(ixyzs,ib))*kets(ixyzs,ik)  ! complex
! #endif
!     enddo ! ixyzs
        s(ib,ik) = dot_product( bras(:,ib), kets(:,ik) )
      enddo ! ib
    enddo ! ik

    if( present( comm ) ) call MPIallsum( s, comm )

#ifdef NaN_SEARCH
    if(any(s/=s)) write(*,*) sym, ' scalar_products: NaN in result.'
#endif
    s = s * h3
  endfunction ! scalar_products





#if R1_C2 == 1
  real function scalar_norm2_r( &
#else
  real function scalar_norm2_c( &
#endif
                                a, h3, comm ) result( s2 )
  use MPItools, only: operator(.MPIsum.)
  implicit none
    ! arguments
    REAPLEX, intent(in)               :: a(:)
    real, intent(in)                  :: h3 ! product of grid spacing
    integer, intent(in), optional     :: comm
    ! local vars
    integer                           :: ixyzs

    s2 = 0.
    do ixyzs = 1, size(a,1)
#if R1_C2 == 1
      s2 = s2 + a(ixyzs)*a(ixyzs)
#else
      s2 = s2 + real(a(ixyzs))**2 + aimag(a(ixyzs))**2
#endif
    enddo ! ixyzs

    if( present( comm ) ) s2 = s2 .MPIsum. comm

    s2 = s2 * h3
  endfunction ! scalar_norm2





      ! END of MODULE
#if R1_C2 == 2
! module tail


  real elemental function complex_conjugate_r( r ) result( cc )
    real, intent(in)    :: r
    cc = r
  endfunction ! complex_conjugate_r

  complex elemental function complex_conjugate_c( c ) result( cc )
    complex, intent(in)    :: c
    cc = conjg( c )
  endfunction ! complex_conjugate_c


  integer function get_F2ndD_coefficients( nin, h, c ) result( n )
    real, intent(in)       :: h ! grid spacing
    integer, intent(in)    :: nin ! nin --> n can be corrected
    real, intent(out)      :: c(0:) ! coefficients

    if( nin > 13 ) then
      ! Warn
      n = 13
    else ; n = nin
    endif ! order larger than implemented

    selectcase( n )
    case(:-1 ) ; stop 'get_F2ndD_coefficients: N < 0 not defined'
    case(  0 ) ! Warn
      c = 0.

    case(  1 ) ! use  3 points
      c( 1) = 1./(1.*h*h)
      c( 0) = -2./(1.*h*h)

    case(  2 ) ! use  5 points
      c( 2) = -1./(12.*h*h)
      c( 1) = 4./(3.*h*h)
      c( 0) = -5./(2.*h*h)

    case(  3 ) ! use  7 points
      c( 3) = 1./(90.*h*h)
      c( 2) = -3./(20.*h*h)
      c( 1) = 3./(2.*h*h)
      c( 0) = -49./(18.*h*h)

    case(  4 ) ! use  9 points
      c( 4) = -1./(560.*h*h)
      c( 3) = 8./(315.*h*h)
      c( 2) = -1./(5.*h*h)
      c( 1) = 8./(5.*h*h)
      c( 0) = -205./(72.*h*h)

    case(  5 ) ! use 11 points
      c( 5) = 1./(3150.*h*h)
      c( 4) = -5./(1008.*h*h)
      c( 3) = 5./(126.*h*h)
      c( 2) = -5./(21.*h*h)
      c( 1) = 5./(3.*h*h)
      c( 0) = -5269./(1800.*h*h)

    case(  6 ) ! use 13 points
      c( 6) = -1./(16632.*h*h)
      c( 5) = 2./(1925.*h*h)
      c( 4) = -1./(112.*h*h)
      c( 3) = 10./(189.*h*h)
      c( 2) = -15./(56.*h*h)
      c( 1) = 12./(7.*h*h)
      c( 0) = -5369./(1800.*h*h)

    case(  7 ) ! use 15 points
      c( 7) = 1./(84084.*h*h)
      c( 6) = -7./(30888.*h*h)
      c( 5) = 7./(3300.*h*h)
      c( 4) = -7./(528.*h*h)
      c( 3) = 7./(108.*h*h)
      c( 2) = -7./(24.*h*h)
      c( 1) = 7./(4.*h*h)
      c( 0) = -266681./(88200.*h*h)

    case(  8 ) ! use 17 points
      c( 8) = -1./(411840.*h*h)
      c( 7) = 16./(315315.*h*h)
      c( 6) = -2./(3861.*h*h)
      c( 5) = 112./(32175.*h*h)
      c( 4) = -7./(396.*h*h)
      c( 3) = 112./(1485.*h*h)
      c( 2) = -14./(45.*h*h)
      c( 1) = 16./(9.*h*h)
      c( 0) = -1077749./(352800.*h*h)

    case(  9 ) ! use 19 points
      c( 9) = 1./(1969110.*h*h)
      c( 8) = -9./(777920.*h*h)
      c( 7) = 9./(70070.*h*h)
      c( 6) = -2./(2145.*h*h)
      c( 5) = 18./(3575.*h*h)
      c( 4) = -63./(2860.*h*h)
      c( 3) = 14./(165.*h*h)
      c( 2) = -18./(55.*h*h)
      c( 1) = 9./(5.*h*h)
      c( 0) = -9778141./(3175200.*h*h)

    case( 10 ) ! use 21 points
      c(10) = -1./(9237800.*h*h)
      c( 9) = 10./(3741309.*h*h)
      c( 8) = -5./(155584.*h*h)
      c( 7) = 30./(119119.*h*h)
      c( 6) = -5./(3432.*h*h)
      c( 5) = 24./(3575.*h*h)
      c( 4) = -15./(572.*h*h)
      c( 3) = 40./(429.*h*h)
      c( 2) = -15./(44.*h*h)
      c( 1) = 20./(11.*h*h)
      c( 0) = -1968329./(635040.*h*h)

    case( 11 ) ! use 23 points
      c(11) = 1./(42678636.*h*h)
      c(10) = -11./(17635800.*h*h)
      c( 9) = 11./(1360476.*h*h)
      c( 8) = -55./(806208.*h*h)
      c( 7) = 55./(129948.*h*h)
      c( 6) = -11./(5304.*h*h)
      c( 5) = 11./(1300.*h*h)
      c( 4) = -11./(364.*h*h)
      c( 3) = 55./(546.*h*h)
      c( 2) = -55./(156.*h*h)
      c( 1) = 11./(6.*h*h)
      c( 0) = -239437889./(76839840.*h*h)

    case( 12 ) ! use 25 points
      c(12) = -1./(194699232.*h*h)
      c(11) = 12./(81800719.*h*h)
      c(10) = -3./(1469650.*h*h)
      c( 9) = 44./(2380833.*h*h)
      c( 8) = -33./(268736.*h*h)
      c( 7) = 132./(205751.*h*h)
      c( 6) = -11./(3978.*h*h)
      c( 5) = 396./(38675.*h*h)
      c( 4) = -99./(2912.*h*h)
      c( 3) = 88./(819.*h*h)
      c( 2) = -33./(91.*h*h)
      c( 1) = 24./(13.*h*h)
      c( 0) = -240505109./(76839840.*h*h)

    case( 13 ) ! use 27 points
      c = 0.
      c(13) = 1./(878850700.*h*h)
      c(12) = -13./(374421600.*h*h)
      c(11) = 13./(25169452.*h*h)
      c(10) = -13./(2600150.*h*h)
      c( 9) = 13./(366282.*h*h)
      c( 8) = -143./(723520.*h*h)
      c( 7) = 143./(158270.*h*h)
      c( 6) = -143./(40698.*h*h)
      c( 5) = 143./(11900.*h*h)
      c( 4) = -143./(3808.*h*h)
      c( 3) = 143./(1260.*h*h)
      c( 2) = -13./(35.*h*h)
      c( 1) = 13./(7.*h*h)
      c( 0) = -40799043101./(12985932960.*h*h)
    ! code was generated in Analytical/LagrangePolynomials
    case default ; stop 'get_F2ndD_coefficients: max implemented is N=13'
    endselect ! n
#ifdef DEBUG
    if( abs( 2.*sum( c(1:n) )+c(0) ) > 1E-12 ) stop 'get_F2ndD_coefficients: inaccurate!'
#endif
  endfunction ! get_F2ndD_coefficients



  complex function Bloch_phase( eik, ishift ) result( ph )
  !!!
  !!! computes the complex Bloch phase factor
  !!! depending on the shift of the atoms.
  !!! |eik| == 1 is NOT assumed.
  !!!
    complex, intent(in)                   :: eik(3)
    integer, intent(in)                   :: ishift(3)

    integer                               :: id

    ph = (1.0,0.0) ! unity

    do id = 1, 3
      selectcase( ishift(id) ) ! special treatment for ishift in {-2,0,2}
      case(  2 ) ; ph = ph * ( eik(id)*eik(id) )
      case(  1 ) ; ph = ph * eik(id)
      case(  0 ) ! do nothing
      case( -1 ) ; ph = ph / eik(id)
      case( -2 ) ; ph = ph / ( eik(id)*eik(id) )
      case default ; ph = ph * eik(id)**ishift(id)
      endselect
    enddo ! id

  endfunction ! Bloch_phase

#ifdef EXTENDED
!+ extended

  integer function test( )
    write(*,*,iostat=test) __FILE__,' no module test implemented!'
  endfunction ! test

!- extended
#endif

endmodule ! operators
! end of doble preprocessing
#endif
#endif

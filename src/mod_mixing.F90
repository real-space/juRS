#include "config.h"

! #define DEBUG
! #define FULL_DEBUG

#ifdef DEBUG
!!! remove comment from debug line
#define cDBG
#else
!!! comment line
#define cDBG !DBG
#endif

#ifndef EXTENDED
#define cNaN !NaN
#else
#ifdef NaN_SEARCH
#define cNaN  
#else
#define cNaN !NaN
#endif
#endif


#define RECOMPUTE
!! re-calculate the Rij every iteration



!! @author Paul Baumeister
!! @version 3.0
!!
!! mixing of the density on the grid
!! and the atomic density matrix in the spheres
module mixing
  use configuration, only: o ! output unit, 0: no output
  use type_item, only: item, operator(.in.)
implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: sym = 'MIX' !! module symbol

  public :: mix
#ifdef EXTENDED
  public :: test
#endif

  ! keys
  integer, parameter :: KEY_POTENTIAL_MIXING = 2 ! potential and a%Hm
  integer, parameter :: KEY_DENSITY_MIXING   = 1 ! density and a%rho
  integer, parameter :: KEY_DMATRIX_MIXING   = 0 ! density and a%Dm
  !=======================================================================
  integer, parameter :: KEY_STRAIGHT         =  0 ! straight mixing method
  integer, parameter :: KEY_BROYDEN_1st      =  4 ! Broyden''s first method
  integer, parameter :: KEY_BROYDEN_2nd      =  8 ! Broyden''s second method
  integer, parameter :: KEY_ANDERSON         = 12 ! generalized Anderson method

  ! configuration
  integer, parameter :: KEY_MIXING_DEFAULT   = 0

  ! mapping words <--> keys for the input and output
  type(item), parameter, public :: Dictionary(12) = (/ &
    item('density',KEY_DENSITY_MIXING), &
    item('adm',KEY_DMATRIX_MIXING), &
    item('potential',KEY_POTENTIAL_MIXING), &
    item('straight',KEY_STRAIGHT), item('simple',KEY_STRAIGHT), &
    item('Broyden1',KEY_BROYDEN_1st), &
    item('Broyden',KEY_BROYDEN_1st), item('Broyden2',KEY_BROYDEN_2nd), &
    item('Anderson',KEY_ANDERSON), &
    item('default',KEY_MIXING_DEFAULT), item('',KEY_MIXING_DEFAULT), &
    item('none',KEY_MIXING_DEFAULT) /)

  ! interfaces
  interface mix
    module procedure mix_Broyden
  endinterface

  interface integral
    module procedure integral_r3, integral_r4
  endinterface

  interface scalar_product
    module procedure scalar_product_r4
  endinterface

  

 contains

  real function mix_Broyden( g, a, qnt, key, beta, nhistory ) result( dp )
  use type_grid, only: grid
  use type_atom, only: atom
  use configuration, only: WARNING
cNaN  use debugtools, only: NaN_search
  implicit none
    ! parameters
    character(len=*), parameter           :: fun = ' mix: '
    real, parameter                       :: DEFAULT_BETA = 0.25
!     integer, parameter                    :: SIMPLE_MIX_BEFORE = 0
    ! arguments
    type(grid), intent(in)                :: g ! grid
    type(atom), intent(inout)             :: a(:) ! atoms
    real, intent(inout)                   :: qnt(:,:,:,:) ! (x,y,z,s)
    integer, intent(in), optional         :: key !! mixing scheme
    real, intent(in), optional            :: beta
    integer, intent(in), optional         :: nhistory
    ! local vars
    integer, save                         :: nhist=1 ! history length
    integer, save                         :: mit=0 ! iteration index

    integer                               :: ikey, nqnt, idof, ndof, ist, ia, np, ns, is ! atom index
    integer                               :: ia_list(size(a)), na, ja
    real                                  :: dp2, bt    ! beta
    real, allocatable                     :: dof(:)


    if( present( nhistory ) ) then
      nhist = max( 1, nhistory )
      !nhist = 4
      mit =  0 ! indicate Broyden routine to allocate history
      if( nhistory < 0 ) &
      mit = -1 ! indicate Broyden routine to deallocate history
    endif ! present

    bt = DEFAULT_BETA ; if( present( beta ) ) bt = beta
    ! if beta is not in the reasonable region [0. 1.], write a warning
    if( bt < 0. .or. bt > 1. ) then
      if(o>0) write(o,'(4A,F0.3,9A)') sym, fun, WARNING(0), 'mixing parameter strange: mixing with ',bt*100.,' %, adjust!'
      bt = min(max(0.,bt),1.)
    endif ! beta not [0, 1.]

    ikey = 0 ; if( present( key ) ) ikey = key
    if( ikey == KEY_STRAIGHT ) then
      dp = straight_mixing( g, a, qnt, key, beta, nhistory )
      
      return
    endif ! straight mixing



    ! transform degrees of freedom into a vector
    nqnt = size(qnt) ! defo in grid vector
    ns = size(qnt,4)

    na = 0
    ndof = nqnt
    do ia = 1, size(a)
      if( a(ia)%owner /= g%rank ) cycle ! atom owner task
#ifdef DEBUG
      if( .not. _allocated_( a(ia)%Dm ) ) stop 'MIX mix_Boyden: an atomic Density matrix a%Dm is not allocated!'
      if( ubound( a(ia)%Dm, 3 ) /= ns ) stop 'MIX mix_Boyden: an atomic Density matrix a%Dm should be NP x NP x NS!'
#endif
      np = size( a(ia)%Dm, 1 )
      ns = ubound( a(ia)%Dm, 3 )

      na = na+1
      ia_list(na) = ia
      do is = 1, ns
        ndof = ndof + np*np
      enddo ! is
    enddo ! ia

    allocate( dof(ndof), stat=ist ) ; dof = 0.

    dof(1:nqnt) = reshape( qnt, (/size(qnt)/) )

    idof = nqnt ! offset

    do ja = 1, na ; ia = ia_list(ja)
      np = size( a(ia)%Dm, 1 )
      ns = ubound( a(ia)%Dm, 3 )
      do is = 1, ns

        dof(idof+1:idof+np*np) = reshape( a(ia)%Dm(:,:,is), (/np*np/) )
        idof = idof + np*np ! forward
      enddo ! is
    enddo ! ia


    dp2 = Broyden_a( g, ndof, dof, mit, nhist, a, ia_list(1:na), nqnt, dV=g%hvol, comm=g%comm, alpha=beta )
    dp = sqrt(dp2/g%svol) ! now dp has the unit of a density


    qnt = reshape( dof(1:nqnt), shape(qnt) )

    idof = nqnt ! offset

    do ja = 1, na ; ia = ia_list(ja)
      np = size( a(ia)%Dm, 1 )
      ns = ubound( a(ia)%Dm, 3 )
      do is = 1, ns
        a(ia)%Dm(:,:,is) = reshape( dof(idof+1:idof+np*np), (/np,np/) )
        idof = idof + np*np ! forward offset

      enddo ! is

      ! a%Dm(:,:,0) = sum_both_spins a%Dm(:,:,s)
      a(ia)%Dm(:,:,0) = a(ia)%Dm(:,:,1)
      if( ns > 1 ) a(ia)%Dm(:,:,0) = a(ia)%Dm(:,:,0) + a(ia)%Dm(:,:,2)
    enddo ! ia


    mit = mit+1 ! count up

    ! return value dp is the change norm of qnt (the quantity on the cartesian grid)
  endfunction mix_Broyden



  real function Broyden_a( g, ndof, sn, mit, maxiter, a, ia_list, ngrid, dV, comm, alpha ) result( dp2 ) ! residual
  !! ndof         number of degrees of freedom
  !! sn           on 0th entry: start input density
  !!              on entry: output density
  !!              on exit: new mixed density
  !! mit          iteration number
  !! maxiter      max. number of history steps
  !! alpha        straight mixing ratio
  ! <needed for the inner product>
  !! a            list of atoms
  !! ia_list      list of atom indices
  !! ngrid        number of DoF on the grid
  !! dV           grid volume elements
  !! comm         MPI grid communicator
  use type_atom, only: atom
  use type_grid, only: grid
  use MPItools, only: operator(.MPIsum.), MPIallsum
  use broyden_second_mod
  implicit none
    ! parameters
    character(len=*), parameter     :: fun = ' Broyden: '
    integer, parameter              :: I_Fp = -0
    integer, parameter              :: I_Sp = -1
    integer, parameter              :: I_Pr = -2
    

    real, parameter                 :: alpha_DEFAULT = 0.25
    integer, parameter              :: STRAIGHT_MIXING_BEFORE = 3

    type(grid), intent(in)                :: g ! grid
    integer, intent(in)             :: ndof ! number of degrees of freedom
    real, intent(inout)             :: Sn(:) ! degrees of freedom
    integer, intent(inout)          :: mit ! iteration counter (changed when restarted)
    integer, intent(in)             :: maxiter
    type(atom), intent(inout)       :: a(:)
    integer, intent(in)             :: ia_list(1:)
    integer, intent(in)             :: ngrid ! number of grid values
    real, intent(in)                :: dV
    MPI_Comm, intent(in), optional  :: comm
    real, intent(in), optional      :: alpha
    !
    real, allocatable, save         :: hist(:,:) ! (0:idof,ih)
    real                            :: Sm(ndof), Fm(ndof), dF(ndof), um(ndof), vm(ndof)
    real                            :: am(0:maxiter), vmnorm, vmscal, fmvm, alfa
    integer                         :: i

    real, allocatable, save         :: Sm1(:), Fm1(:)
    real, allocatable, save         :: ui2(:, :), vi2(:, :)
    real                            :: g_metric(ndof)

    integer, save                   :: ii = 1;
    real, save                      :: dp2old = 130000.0
    real, save                      :: alfacor = 10.0
    integer                         :: DIM_HIST 
!     integer*8                       :: mem

    alfa = alpha_DEFAULT ; if( present( alpha ) ) alfa = min( max( 0., alpha ), 1. )


    DIM_HIST = maxiter
    g_metric(:) = 1.0

    selectcase( mit )
    case( :-1 ) ! reset
      deallocate( hist, stat=i )
      if(o>0) write(o,'(9A)') sym, fun, 'free mixing history!'
      dp2 = 9E209 ; return
    case(  0 ) ! init
      if(o>0) write(o,'(3A,I0,A,F10.3,9A)') sym, fun, 'mixing history for ',maxiter, &
        ' steps,', (ndof+1)*(2*maxiter-I_Pr+1)*2.**(3-20), ' MiByte'
      if(.not. allocated(hist)) then 
        allocate( hist(0:ndof,I_Pr:0), stat=i )
      endif
      if( i /= 0 ) stop 'Broyden: allocation of internal mixing history failed!'
      hist = 0. ! init
      hist(1:,I_Pr) = Sn ! store input for the first iteration
      allocate(Sm1(ndof))
      allocate(Fm1(ndof))
      allocate(ui2(ndof, 2:DIM_HIST)) 
      allocate(vi2(ndof, 2:DIM_HIST))
      dp2 = 9E209 ; return 
    endselect ! mit

    Sm = hist(1:,I_Pr) ! load input
    Fm = Sn - Sm ! response = (out) - (in)
    dF = adjoint( Fm, a, ia_list, ngrid, dV, g ) ! use dF as temporary for <F|
    dp2 = dot_product( dF, Fm) .MPIsum. comm ! communication

    dp2old = dp2

    if( mit <= max(1,STRAIGHT_MIXING_BEFORE) ) then

      hist(1:,I_Sp) = Sm ! store for the next iteration
      hist(1:,I_Fp) = Fm ! store for the next iteration

      
      Sn = (1. - alfa) * Sm + alfa * Sn
      
      !!write(*,*) 'done this simple mix!!', alfa
    else
     

      
      hist(1:,I_Sp) = Sm ! store for the next iteration

      dF = Fm - hist(1:,I_Fp)

      hist(1:,I_Fp) = Fm ! store for the next iteration
      !Sn = Sm + alfa * Fm ! straight mixing first
      Sn = (1. - alfa) * Sm + alfa * Sn
      !Sm = (1. - alfa) * Sm 
      !call broyden_second(Sm, Sn, sm1, fm1, ui2,vi2, g_metric, alfa, &
      !                  comm, DIM_HIST, ndof, ii)

      
      call broyden_second_xxxx(Sm, Sn, sm1, fm1, ui2,vi2, alfa, &
                        comm, DIM_HIST, ndof, ii, a, ia_list, ngrid, dV, g)

      Sn = Sm
      !write(*,*) 'done broyd it', ii, 'alfa ', alfa
      ii = ii + 1
      


    endif !end broyden!

    hist(1:,I_Pr) = Sn ! store input for the next iteration

  

  endfunction ! Broyden_a

  

  function adjoint( v, a, ial, ng, dV, g ) result( va )
    use type_atom, only: atom
    use type_grid, only: grid
    use laplacian, only: laplace_smoother
    implicit none
      ! parameters
      character(len=*), parameter     :: fun = ' adjoint: '
      ! arguments
      real, intent(in)                :: v(1:) ! input vector
      type(atom), intent(in)          :: a(1:)
      integer, intent(in)             :: ial(1:), ng
      real, intent(in)                :: dV
      type(grid), intent(in)          :: g ! grid
      ! result
      real                            :: va(1:size(v))
      ! local vars
      integer           :: io, ia, na, np, np2, ns, is, ist
#define USE_SMOOTHING
#ifdef USE_SMOOTHING
      !! destroy the stack!!
      real tmpgrd(g%ng(1), g%ng(2), g%ng(3), g%ng(4))
      real tmpgrd1(g%ng(1), g%ng(2), g%ng(3), g%ng(4))

      ! grid part, diagonal, constant metric dV
      tmpgrd = reshape(v(1:ng), (/g%ng(1), g%ng(2), g%ng(3), g%ng(4)/))
      ist = laplace_smoother(tmpgrd(:,:,:,1), tmpgrd1(:,:,:,1), g, 0.0)
      if(g%ng(4) == 2) then 
         ist = laplace_smoother(tmpgrd(:,:,:,2), tmpgrd1(:,:,:,2), g, 0.0)
      endif 
      
      va(1:ng) = reshape(tmpgrd1(:,:,:,:), (/ng/))
      !va(1:ng) = v(1:ng) * 3.0*dV
      va(1:ng) = va(1:ng)*dV
#else
      va(1:ng) = v(1:ng)*dV
#endif

      io = ng ! init

      ! atomic parts
      do na = 1, size(ial) ; ia = ial(na)
        np = size(a(ia)%Dm,1)
        np2 = np*np
        ns = ubound(a(ia)%Dm,3)
        do is = 1, ns
          va(io+1:io+np2) = matmul( a(ia)%s%chdt2, v(io+1:io+np2) )
          io = io+np2
        enddo ! is
      enddo ! na

    endfunction ! adjoint




  real function straight_mixing( g, a, qnt, key, beta, nhistory ) result( dp )
  use type_atom, only: atom
  use type_grid, only: grid
  use configuration, only: WARNING
  
  implicit none
    ! parameters
    character(len=*), parameter           :: fun = ' straight_mixing: '
    real, parameter                       :: DEFAULT_ALPHA = 0.25
    integer, parameter                    :: iRtmp = 0
    integer, parameter                    :: iPvec = iRtmp ! take Rtemp without preconditioning
    integer, parameter                    :: nhist = 1 ! history length
    integer, parameter                    :: ihist = 1 ! history index
    type(grid), intent(in)                :: g ! grid
    type(atom), intent(inout)             :: a(:)
    real, intent(inout)                   :: qnt(:,:,:,:)
    integer, intent(in), optional         :: key
    real, intent(in), optional            :: beta
    integer, intent(in), optional         :: nhistory
    real, allocatable, save               :: qnt_hist(:,:,:,:,:)
    
    integer                               :: ia, natm
    integer, save                         :: ikey=KEY_MIXING_DEFAULT
    integer                               :: ist
    real                                  :: d2p, alpha
    real :: gamma
    logical                               :: first
    integer :: nd(4)!=1 ! sizes


    if( present( key ) ) ikey = key

    alpha = DEFAULT_ALPHA ; if( present( beta ) ) alpha = beta
    ! if eta is not in the reasonable region [0. 1.], write a warning
    if( alpha < 0. .or. alpha > 1. ) then
      if(o>0) write(o,'(4A,F6.1,9A)') sym, fun, WARNING(0), &
        'mixing parameter strange: mixing with ', alpha*100. ,' %, adjust!'
      alpha = min( max( 0.0, alpha ), 1.0 )
    endif ! alpha not [0, 1.]
    
    first = present( nhistory )
    gamma = 0.2

    if( first ) then
      nd(1:4) = shape( qnt ) ! dimensions
      deallocate( qnt_hist, stat=ist )
      allocate( qnt_hist(nd(1),nd(2),nd(3),nd(4),iPvec:nhist), stat=ist )
      if( ist /= 0 ) stop 'mixing: allocation of QNT_HIST array failed.'
      qnt_hist = 0.
      alpha = 1.
      gamma = 0.

    endif ! not allocated
    
    ! number of atoms
    natm = size( a ,1 )

    ! compute the norm of the change
    qnt_hist(:,:,:,:,iRtmp) = qnt - qnt_hist(:,:,:,:,ihist) ! change vector
    d2p = scalar_product( qnt_hist(:,:,:,:,iRtmp), qnt_hist(:,:,:,:,iRtmp), g )

 
    dp = sqrt( d2p/g%svol ) ! normalized by the cell volume
    ! if(o>0) write(o,'(3A,F10.6A,F10.6)') sym, fun, 'density residual d2p=', d2p, ' dp =', dp

    ! mix the smooth density on the coarse grid
    
    qnt =    alpha    * qnt +  &
           (1.-alpha) * qnt_hist(:,:,:,:,ihist)


    ! write to the history for the next iteration
    qnt_hist(:,:,:,:,ihist) = qnt


    do ia = 1, natm

      ! atom owner task
      if( a(ia)%owner /= g%rank ) cycle

  
      if( first ) then



        selectcase( ikey )
        case( KEY_DENSITY_MIXING )   ; nd = shape(a(ia)%rho) ! mix the radial smooth and true charge in multipole representation
        case( KEY_POTENTIAL_MIXING ) ; nd = shape(a(ia)%pot) ! mix the radial smooth and true potential in multipole representation
        case( KEY_DMATRIX_MIXING )   ; nd(1:3) = shape(a(ia)%Dm) ; nd(4) = 1 ! mix the density matrix in the spheres
        case default ; stop 'mix: only ranks 3 or 4 objects implemented for mixing.'
        endselect ! ndim


        deallocate( a(ia)%mix_hist, stat=ist )
        allocate( a(ia)%mix_hist(nd(1),nd(2),nd(3),nd(4),nhist) )
        a(ia)%mix_hist = 0. ! init

      endif ! first iteration, history not allocated

      selectcase( ikey )
      case( KEY_DENSITY_MIXING )
#define q4 a(ia)%rho
        q4 = alpha * q4 + (1.-alpha) * a(ia)%mix_hist(:,:,:,:,ihist)
        a(ia)%mix_hist(:,:,:,:,ihist) = q4
#undef  q4
      case( KEY_POTENTIAL_MIXING )
#define q4 a(ia)%pot
        q4 = alpha * q4 + (1.-alpha) * a(ia)%mix_hist(:,:,:,:,ihist)
        a(ia)%mix_hist(:,:,:,:,ihist) = q4
#undef  q4
      case( KEY_DMATRIX_MIXING )   
#define q3 a(ia)%Dm
        q3 = alpha * q3 + (1.-alpha) * a(ia)%mix_hist(:,:,:,1,ihist)
        a(ia)%mix_hist(:,:,:,1,ihist) = q3
#undef  q3
        a(ia)%Dm = alpha * a(ia)%Dm + (1.-alpha) * a(ia)%mix_hist(:,:,:,1,ihist)
        a(ia)%mix_hist(:,:,:,1,ihist) = a(ia)%Dm


      case default ; stop 'mix: only ranks 3 or 4 objects implemented for mixing.'
      endselect ! ikey

    enddo ! ia

    ! the return value is the density change norm dp
  endfunction ! straight_mixing



  real function scalar_product_r4( q1, q2, g ) result( s )
  use type_grid, only: grid
  use MPItools, only: operator(.MPIsum.)
  implicit none
    ! parameter
    character(len=*), parameter           :: fun = ' scalar_product: '
    ! arguments
    real, intent(in)                      :: q1(:,:,:,:), q2(:,:,:,:)
    type(grid), intent(in)                :: g

    s = ( sum( q1 * q2 ) .MPIsum. g%comm ) * g%hvol

  endfunction ! scalar_product_r4



  real function integral_r4( density, g ) result( s )
  use type_grid, only: grid
  use MPItools, only: operator(.MPIsum.)
  implicit none
    ! parameter
    character(len=*), parameter           :: fun = ' integral: '
    ! arguments
    real, intent(in)                      :: density(:,:,:,:)
    type(grid), intent(in)                :: g

    s = ( sum( density ) .MPIsum. g%comm )*g%hvol

  endfunction ! integral_r4

  real function integral_r3( density, g ) result( s )
  use type_grid, only: grid
  use MPItools, only: operator(.MPIsum.)
  implicit none
    ! parameter
    character(len=*), parameter           :: fun = ' integral: '
    ! arguments
    real, intent(in)                      :: density(:,:,:)
    type(grid), intent(in)                :: g

    s = ( sum( density ) .MPIsum. g%comm )*g%hvol

  endfunction ! integral_r3


  subroutine broyden_second_xxxx(sm, fm, sm1, fm1, ui2,vi2, alpha, &
                          communicator, itdbryd, imap, iter, &
                          a, ial, ng, dv, g)
  use type_atom, only: atom
  use type_grid, only: grid
  use laplacian, only: laplace_smoother
!   use MPIconst, only: MPI_DOUBLE_PRECISION, MPI_SUM
  use MPItools, only: MPIallsum, operator(.MPIsum.)
  implicit none

  ! Parameters
  real, parameter :: ONE =1.0

  ! Arguments
  real, intent(inout), dimension(imap) :: sm
  real, intent(inout), dimension(imap) :: fm
  real, intent(inout), dimension(imap) :: sm1
  real, intent(inout), dimension(imap) :: fm1
  real, intent(inout), dimension(imap, 2:itdbryd) :: ui2
  real, intent(inout), dimension(imap, 2:itdbryd) :: vi2

  real, intent(in) :: alpha
  !real, intent(in), dimension(imap) :: g_metric
  integer, intent(in) :: communicator
  integer, intent(in) :: itdbryd
  integer, intent(in) :: imap
  integer, intent(in) :: iter
  
  
  type(atom), intent(in)          :: a(1:)
  integer, intent(in)             :: ial(1:), ng
  real, intent(in)                :: dV
  type(grid), intent(in)                :: g ! grid

  ! Local variables of broyden_second
  real :: rmixiv
  real :: vmnorm
  real :: ddot_local
  real :: ddot_global
  real :: cmm_local
  real :: cmm_global
  integer :: ij
  integer :: mit
  integer :: it
  integer :: ierr
  real, dimension(2:itdbryd) :: am
  real, dimension(2:itdbryd) :: am_local
  real, dimension(imap) :: work
  real, dimension(imap) :: vi3
  real, dimension(imap) :: ui3
  real :: EPS

   !    .. External Functions ..
   real, external :: ddot
   external MPI_Allreduce

   EPS = epsilon(1.0)

   mit = mod(iter-1, itdbryd) + 1
   rmixiv = one/alpha
   !
   !---->  the following block is activated only one iteration before
   !        broyden iteration scheme is used
   !---->  set up of : sm1 = rho(1) ; fm1=fm[1]=f(rho(1)) - rho(1) ;
   !                   metric  g := r*r*drdi

   ! from simple mixed V_out -> reconstruct unmixed V_out ! OMG! WTF!
   ! fm = 1/alpha * (V_out[V_in] - V_in)
   
   if (mit == 1) then
     work = fm
   end if

   do ij = 1,imap
     fm(ij) = rmixiv* (fm(ij)-sm(ij))
   end do

   !=====  For MIT GT 1 activ  ==============================================

   if (mit.gt.1) then

     !----> calculate  sm = rho(m) - rho(m-1)
     !----> calculate dfm = f[m] - f[m-1]
     !
     do ij = 1,imap
       sm1(ij) = sm(ij) - sm1(ij)
       fm1(ij) = fm(ij) - fm1(ij)
     end do

     ! sm1 = \Delta V_in
     ! fm1 = \Delta V_out

     !----> loop to generate u[m] = u(ij,mit)
     !
     ! See Srivastava (16).
     ! = alpha*\Delta V_out + \Delta V_in
     do ij = 1,imap
       ui3(ij) = alpha*fm1(ij) + sm1(ij)
     end do

     do it = 2,mit - 1
       do ij = 1,imap
         work(ij)=vi2(ij,it)
       enddo
       am_local(it) = ddot(imap,fm1,1,work,1)
     enddo

!      call MPI_Allreduce(am_local,am,(itdbryd-1), MPI_DOUBLE_PRECISION,MPI_SUM,communicator,ierr)
     am = am_local; call MPIallsum(am, communicator)

     ! active only from 3rd iteration on
     ! ui3 = \sum_i^j -am_i (alpha*\Delta V_out + \Delta V_in)_i
     ! with
     ! am_i = (\Delta V_out)_i^T . G . (\Delta V_out)_j ???
     do it = 2,mit - 1
       do ij = 1,imap
         work(ij)=ui2(ij,it)
       enddo
       call daxpy(imap,-am(it),work,1,ui3,1)
     enddo

     !-------->     b r o y d e n  s   s e c o n d    m e t h o d
     !----> calculate v[m]  convoluted with the metric g

     ! vi3 = G . \Delta V_out
     !do ij = 1,imap
     !  vi3(ij) = g_metric(ij)*fm1(ij)
     !end do
     !!function adjoint( v, a, ial, ng, dV ) result( va )
     vi3 = adjoint(fm1, a, ial, ng, dV, g)
      
     !----> calculate #vm# and normalize v[m]

     ! = (\Delta V_out)^T . G . (\Delta V_out) ! using diagonal metric matrix G
     ddot_local = ddot(imap,vi3,1,fm1,1)

!      call MPI_Allreduce(ddot_local,ddot_global,1, MPI_DOUBLE_PRECISION,MPI_SUM,communicator,ierr)
     ddot_global = ddot_local .MPIsum. communicator

     vmnorm = ddot_global

     ! normalize (\Delta V_out)
     ! v^T(i) in Srivastava paper, equ. (16)
     if (vmnorm > EPS) then ! vmnorm must not be zero
       call dscal(imap,one/vmnorm,vi3,1)
     else
       vi3 = 0.0d0
     end if

     !============ END MIXING, NOW OUTPUT =====================================

     !----> store u3(ij) and v3(ij) for following iterations
     do ij = 1,imap
       ui2(ij,mit)=ui3(ij)
       vi2(ij,mit)=vi3(ij)
     enddo

     !----> update f[m-1] = f[m]  ; rho(m) = rho(m-1)
     do ij = 1,imap
       fm1(ij) = fm(ij)
       sm1(ij) = sm(ij)
     end do

     !----> calculate cmm
     !

     ! See coefficients c_mi in Srivastava - why only c_mm???
     ! c_mi should change from iteration to iteration???
     ! is only 1 term of sum used???
     ! Is there a relation between cmm and am(i) ???
     ! cmm = v^T(i) . G . F(m)   i=m ???
     ! cmm = 1/alpha * (V_out[V_in] - V_in] . (\Delta V_out / ||\Delta V_out||)
     cmm_local = ddot(imap,fm,1,vi3,1)

!      call MPI_Allreduce(cmm_local,cmm_global,1, MPI_DOUBLE_PRECISION,MPI_SUM,communicator,ierr)
     cmm_global = cmm_local .MPIsum. communicator

     !----> update rho(m+1)
     !
     ! V_in_new = (1 - cmm) * (alpha*\Delta V_out + \Delta V_in) + V_in
     call daxpy(imap,one-cmm_global,ui3,1,sm,1)

   else if (mit == 1) then !1st iteration

     !----> update f[m-1] = f[m]  ; rho(m) = rho(m-1)
     do ij = 1,imap
       fm1(ij) = fm(ij)
       sm1(ij) = sm(ij)
     end do
     sm = work

   else
     write(*,*) "Iteration index has to be >= 1."
     STOP
   end if

   !      MIT = MIT + 1
 end subroutine

  status_t function test( )
    write(*,*,iostat=test) 'useless'
  endfunction ! test

endmodule ! mixing

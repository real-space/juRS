#include "config.h"
!!#define TEST_BROYDEN_SECOND_MOD__
!> Extracted Broyden s 2nd method from classic code.

module broyden_second_mod
implicit none

public :: test

  contains

    !*********************************************************************
    !> Broyden mixing. Second method only
    !>
    !> \verbatim
    !>    imix :
    !>      3      broyden s            f i r s t  m e t h o d
    !>      4      broyden s          s e c o n d  m e t h o d
    !>      5      anderson s     g e n e r a l i z e d   m e t h o d
    !>
    !>    implemented here according to notes of s.b.
    !>    broyden s iteration scheme following the papers of :
    !>    srivastava, j. phys. , 17 (1984) , pp l317
    !>    c.g. broyden in math.comput., 19 , pp 577, 1965
    !>    c.g. broyden in ibid, 21 ,pp 368 ,1967
    !>    the method has been generalized to include a metric.the
    !>    definition of the necessary inner products are similar to the
    !>    discription given in the notes of m.weinert.the algorithm
    !>    discribed in the paper srivastava  has been simplified
    !>    ( see notes of s.b.)
    !>    the files ui,vi are stored on high speed ssd memory.
    !>    broyden s update treats charge and spin on the same footing
    !>                 s. bluegel , kfa , may 1987
    !>    the anderson method (d.g. anderson, j. acm 12, 547 (1964)) has
    !>    been generalized and reformulated as an improvement of broyden s
    !>    second method. successive linesearch is replaced by successive
    !>    search on hyperplanes. ( see notes of s.b. )
    !>                 s. bluegel , issp , july 1989
    !>
    !>    modified for non spherical potential
    !>                 b. drittler , aug. 1988
    !>
    !>    parallelized over the number of atoms
    !>                 a. thiess , jun. 2008
    !>
    !>    made independent of atoms e.r.
    !>
    !> \endverbatim
    !*********************************************************************

 !*********************************************************************
 ! PARAMETERS STILL UNCLEAR!!!
 ! arrays destroyed on output, result in sm
 ! sm = input: V_in from current iteration, new V_in on output (=RESULT)
 ! fm = input: simple mixed pot. from current iteration
 !      output: changed on output
 ! sm1 = work array
 ! fm1 = work array
 ! ui2, vi2 ... work arrays
 ! g_metric ... diagonal of metric matrix
 ! imap ... length of arrays
subroutine broyden_second(sm, fm, sm1, fm1, ui2,vi2, g_metric, alpha, &
                          communicator, itdbryd, imap, iter)
  use MPItools, only: operator(.MPIsum.), MPIallsum
  implicit none

  ! Parameters
  real, parameter :: ONE =1.0d0

  ! Arguments
  real, intent(inout), dimension(imap) :: sm
  real, intent(inout), dimension(imap) :: fm
  real, intent(inout), dimension(imap) :: sm1
  real, intent(inout), dimension(imap) :: fm1
  real, intent(inout), dimension(imap, 2:itdbryd) :: ui2
  real, intent(inout), dimension(imap, 2:itdbryd) :: vi2

  real, intent(in) :: alpha
  real, intent(in), dimension(imap) :: g_metric
  integer, intent(in) :: communicator
  integer, intent(in) :: itdbryd
  integer, intent(in) :: imap
  integer, intent(in) :: iter

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

   EPS = epsilon(1.0d0)

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

   if (mit > 1) then

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

     am = am_local; call MPIallsum(am(:(itdbryd-1)), communicator)

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

     ! -------->     broyden s   
     ! second    method
     ! ----> calculate v[m]  convoluted with the metric g

     ! vi3 = G . \Delta V_out
     do ij = 1,imap
       vi3(ij) = g_metric(ij)*fm1(ij)
     end do

     !----> calculate #vm# and normalize v[m]

     ! = (\Delta V_out)^T . G . (\Delta V_out) ! using diagonal metric matrix G
     ddot_local = ddot(imap,vi3,1,fm1,1)

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

!> Test-case for Broydens second method
!> @author Elias Rabel
! A test for the Broyden routine. Run with 1 MPI-process.
 status_t function test( ) result(ist)
  use MPIconst, only: MPI_COMM_WORLD
  implicit none

  integer, parameter :: NUM_IT = 30
  integer, parameter :: DIM_HIST = 20
  real :: alpha
  real :: g_metric(2)
  real :: x(2)
  integer :: ii, ierr
  real, dimension(2) :: fm, sm1, fm1
  real, dimension(2, 2:DIM_HIST) :: ui2, vi2

  x = [10.0d0, -1.8d0]
  g_metric = [1.0d0, 1.0d0]

  alpha = 0.01d0
#ifndef NOMPI
  call MPI_Init(ierr)
#endif
  do ii = 1, NUM_IT

    call himmelblau(x, fm)
    ! simple mix first
    fm = (1. - alpha) * x + alpha * fm

    call broyden_second(x, fm, sm1, fm1, ui2,vi2, g_metric, alpha, &
                        MPI_COMM_WORLD, DIM_HIST, 2, ii)

    write(*,*) ii, x
  end do
  
  ist = 0
#ifndef NOMPI
  call MPI_Finalize(ierr)
#endif
  contains
    subroutine himmelblau(x, f)
      ! test to find fixed point of f which is (3,2)
      implicit none
      real, intent(in) :: x(2)
      real, intent(out) :: f(2)

      f(1) = 2*x(1)**3 + 2*x(1)*x(2) + x(2)**2 - 21*x(1) - 7 + 3
      f(2) =   x(1)**2 + 2*x(1)*x(2) + 2*x(2)**3-13*x(2) - 11 + 2
    end subroutine
 end function

end module

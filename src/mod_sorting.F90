#include "config.h"

! #define DEBUG


!! @author Paul Baumeister
!! @version 3.0
!!
module sorting
implicit none
  private ! default for this module namespace

  ! public
  public :: permutation_of
#ifdef EXTENDED
  public :: ipermutation_of ! inverse permutation of
  public :: identity_permutation
  public :: gen_permutation
  public :: factorial

  public :: test
#endif

  ! interfaces
  interface permutation_of
    module procedure perm
  endinterface

  !! output unit for warnings and error messages
#ifdef DEBUG
  integer, parameter, private :: o = 6 !! 6:stdout (default) 0: no output
#else
  integer, parameter, private :: o = 0 !! 6:stdout (default) 0: no output
#endif

  type, private :: ord
    integer :: i ! global index
    real    :: r ! ordering quantity
  endtype ! ord

  contains

  function perm( n, v ) result( j )
    integer, intent(in)                   :: n !! number of elements
    real, intent(in)                      :: v(n) !! values to be ordered
    integer                               :: j(n) !! result: permutation

    type(ord)                             :: d(n), s(n)
    integer                               :: i
#ifdef DEBUG
    integer                               :: m(n) ! permutation
#endif

    do i = 1, n
      d(i)%i = i    ! set up 0-permutation
      d(i)%r = v(i) ! copy values
    enddo ! i

    s = sort( n, d )

    do i = 1, n
      j(i) = s(i)%i ! copy result permutation
    enddo ! i

#ifdef DEBUG
    ! check if j is a valid permutation
    m = 0 ! init
    do i = 1, n
      m(j(i)) = m(j(i))+1
    enddo ! i
    if( any( m /= 1 ) ) then ! each element has to appear exactly once
      if(o>0) write(o,'(9A)') __FILE__,' is not a valid permutation'
      stop 'SRT permutation of: not a valid permutation.'
    endif ! one of the counts is not 1

    ! check ordering
    do i = 2, n
      if( v(j(i)) < v(j(i-1)) ) then
        if(o>0) write(o,'(A,9(A,I0))') __FILE__,' list sort failed. V(',j(i-1),') > V(',j(i),')'
        stop 'SRT permutation of: sort failed.'
      endif ! v(i) < v(i+1)
    enddo ! i
#endif
  endfunction ! perm

  recursive function sort( na, lst ) result( srt )
    integer  , intent(in) :: na !! number of elements
    type(ord), intent(in) :: lst(na) !! values to be ordered
    type(ord)             :: srt(na) !! result

    type(ord)   :: l1(na/2+1), l2(na/2)
    integer     :: i, n1, n2, i1, i2
    logical     :: t1f2

    selectcase( na )
    case( 1 ) ! a list of a single element is always ordered
      srt(1) = lst(1) ! copy
      return
    case( 2: ) ! split list
      n2 = na/2 ! the "smaller half"
      n1 = na - n2 ! the other part
      ! recursive invocation of this function
      l1(1:n1) = sort( n1, lst(   1:n1) ) ! 1st "half"
      l2(1:n2) = sort( n2, lst(n1+1:na) ) ! 2nd "half"
      ! merge sorted partial lists
      i1 = 1 ; i2 = 1 ! init
      do i = 1, na
#ifdef DEBUG
        if( i1+i2 /= i+1 ) stop 'DSP sort: fatal error: number of used indices is wrong!'
#endif
        if( i1 <= n1 ) then ! can take from list l1
          if( i2 <= n2 ) then ! can take from list l2

            ! decide from which list to take by comparing the two
            if( l1(i1)%r <= l2(i2)%r ) then
              t1f2 = .true.  ! take from list l1
            else
              t1f2 = .false. ! take from list l2
            endif

          else ! cannot take from list l2
            t1f2 = .true.    ! take from list l1
          endif ! i2 <= n2
        else ! cannot take from list l1
#ifdef DEBUG
          if( i2 > n2 ) stop 'DSP sort: fatal error: both lists exhausted at a time!'
#endif
          t1f2 = .false.     ! take from list l2
        endif ! i1 <= n1

        if( t1f2 ) then
          srt(i) = l1(i1) ! take from list l1
          i1 = i1+1 ! count up list1 counter
        else  ! t1f2
          srt(i) = l2(i2) ! take from list l2
          i2 = i2+1 ! count up list2 counter
        endif ! t1f2
      enddo ! i
#ifdef DEBUG
      ! additional check
      if( i1 /= n1+1 ) stop 'DSP sort: i1 did not count up to n1!'
      if( i2 /= n2+1 ) stop 'DSP sort: i2 did not count up to n2!'
    case default !; stop 'DSP sort: case 0 or less should never happen!'
#endif
    endselect ! na

  endfunction ! sort



#ifdef EXTENDED
!+ extended

  function identity_permutation( n ) result( id )
    integer, intent(in) :: n !! number of elements
    integer             :: i, id(n) !! result: identity_permutation
    id(1:n) = (/ (i, i=1,n) /)
  endfunction ! identity_permutation

  function ipermutation_of( n, v ) result( inv )
    integer, intent(in)                   :: n !! number of elements
    real, intent(in)                      :: v(n) !! values to be ordered
    integer                               :: inv(n) !! result: inverse permutation

    integer :: i, j(n) ! permutation

    j = permutation_of( n, v )

#ifdef DEBUG
    inv = 0
#endif
    do i = 1, n
      inv(j(i)) = i
    enddo ! i
#ifdef DEBUG
    if( any( inv == 0 ) ) stop 'invert permutation failed!  J(:) is not a valid permutation!'
#endif
  endfunction ! ipermutation_of

  recursive function gen_permutation( nn, ii ) result( iperm )
    integer, intent(in)             :: nn
    integer(kind=8), intent(in)     :: ii
    integer                         :: iperm(nn) !! result

    integer             :: ipnm1(nn-1), ipos
    integer(kind=8)     :: nf, n8

    selectcase( nn )
    case( :0 ) ! do nothing
    case( 1  ) ; iperm(1) = 1 ! trivial
    case( 2  ) ! even or odd permutation of {1,2}
      if( mod( ii, 2_8 ) == 0 ) then
        iperm(1) = 1
        iperm(2) = 2
      else  ! even
        iperm(1) = 2
        iperm(2) = 1
      endif ! odd
    case( 3: )
      nf = factorial( nn-1 )
      n8 = nn ! conversion to integer*8
      ipnm1 = gen_permutation( nn-1, ii )
      ipos = nn-mod( ii/nf, n8 )
      iperm( :ipos-1) = ipnm1(:ipos-1)
      iperm(  ipos  ) = nn
      iperm(1+ipos: ) = ipnm1(ipos:)
    endselect

  endfunction ! gen_permutation

  integer(kind=8) elemental function factorial( nn )
    integer, intent(in) :: nn

    integer :: ii
    factorial = 1
    do ii = 2, nn
      factorial = factorial * ii
    enddo ! ii
  endfunction ! factorial

  status_t function test( ) result( n )
    integer, parameter  :: M = 5
    integer             :: iperm(M), jperm(M), cnt(M)
    integer(kind=8)     :: i0, j0

    do n = M, 2, -1
      write(*,'(/,9(A,I0))') ' test ',factorial(n),' permutations for n = ',n
      do i0 = 0, factorial(n)-1
        iperm(1:n) = gen_permutation( n, i0 )
        write(*,'(I12,A,99(" ",I0))') i0, '  --> ', iperm(1:n)

        ! test if iperm(1:n) is a valid permutation, i.e. all number in the range [1,n] appear exactly once
        cnt = 0 ! init
        cnt(iperm(1:n)) = cnt(iperm(1:n)) + 1
        if( any( cnt(1:n) /= 1 ) ) then
          write(*,'(A,9(A,I0))') __FILE__,' ERROR! Generated permutation #',i0,' is invalid for N=',n
          return ! stop 'SRT: failed to generate valid permutations from an integer!'
        endif ! failed

        do j0 = 0, i0-1
          ! test if the mapping from the integer to the permutation is unique
          jperm(1:n) = gen_permutation( n, j0 )
          if( all( jperm(1:n) == iperm(1:n) ) ) then
            write(*,'(A,9(A,I0))') __FILE__,' ERROR! Generated permutation #',i0,' is not unique and matches with #',j0,' for N=',n
            return ! stop 'SRT: Failed to generate unique permutations from an integer!'
          endif ! Failed
        enddo ! j0
      enddo ! i0
    enddo ! n
    write(*,'(/,9(A,I0))') ' all permutation tests for N in [2,',M,'] successfull.'
    n = 0 ! all tests successfull
  endfunction ! test

!- extended
#endif
endmodule ! sorting

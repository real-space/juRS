#include "config.h"

! #define DEBUG

#ifdef DEBUG
! #define INTERNAL_COUNTERS
#define INTERNAL_INDEX
#endif

module type_llist
!! linked list for insertion sort algorithm
!! of non-negative real values as a histogram
implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: sym = 'tLLIST' !! module symbol

  public :: llist
  public :: add_value
  public :: get_histogram
  public :: free
#ifdef EXTENDED
  public :: test
#endif

  type :: llist ! the type declaration is public
    private             ! but the components are private
    real    :: v=0.     ! value
    integer :: n=0      ! occurrence
    type(llist), pointer :: next
#ifdef INTERNAL_INDEX
    integer :: i=1      ! index
    !! all list elements have an index that can
    !! be used to debug
#endif
  endtype ! llist

#ifdef INTERNAL_COUNTERS
  !! add_value counts the occurrence of the three cases:
  !!              {1:append_new, 2:match, 3:insert_new}
  integer, public :: n_cases(1:3) = 0
#endif

  interface free
    module procedure free_llist
  endinterface

#ifdef DEBUG
  iounit_t, parameter :: o = 6 ! 6:std out
#else
  iounit_t, parameter :: o = 0 ! 0:no output
#endif

  contains

  type(llist) function new( v, next &
#ifdef INTERNAL_INDEX
                          , i &
#endif
                          )
  implicit none
    ! arguments
    real, intent(in)                             :: v
    type(llist), intent(inout), target, optional :: next
#ifdef INTERNAL_INDEX
    integer, intent(in)                          :: i
    new%i = i
#endif
    new%v = v
    new%n = 1
    if( present( next ) ) new%next => next
  endfunction new

  recursive subroutine free_llist( ll )
  implicit none
    ! arguments
    type(llist), intent(inout) :: ll
    ! local vars
    integer                    :: ist
    ll%v = 0.
    ll%n = 0
#ifdef INTERNAL_INDEX
    ll%i = 1
#endif
    if( associated( ll%next ) ) then
      call free_llist( ll%next )
      deallocate( ll%next, stat=ist )
    endif ! associated
  endsubroutine free_llist


  integer function get_histogram( first, values, occurrence ) result( n )
  implicit none
    ! arguments
    type(llist), intent(in), target    :: first ! 1st element of the LinkedList
    real, allocatable, intent(out)     :: values(:)
    integer, allocatable, intent(out)  :: occurrence(:)
    ! local vars
    status_t :: ist
    integer :: i
    type(llist), pointer :: now

    n = 0 ! init
    now => first ! init
    do while( associated( now ) )
       n = n+1 ! increase
       now => now%next ! go to next element
    enddo ! while

    deallocate( values, occurrence, stat=ist )
    allocate( values(1:n), occurrence(1:n), stat=ist )

    i = 0 ! init
    now => first ! init
    do while( associated( now ) )
       i = i+1 ! increase
       values(i)      = now%v
       occurrence(i)  = now%n
#ifdef INTERNAL_INDEX
       if(o>0) write(o,'(A,I0)') 'index ', now%i
#endif
       now => now%next ! go to next element
    enddo ! while
#ifdef DEBUG
    if( i /= n ) stop 'get_histogram: fatal counting error!' 
#endif
  endfunction get_histogram


  logical function add_value( value, first, tolerance ) result( added_element )
  implicit none
    ! arguments
    real, intent(in)                      :: value
    type(llist), intent(inout), target    :: first
    real, intent(in), optional            :: tolerance
    ! local vars
    real                                  :: tol
    type(llist), pointer                  :: lst, now, nxt

    tol = 0. ; if(present(tolerance)) tol = abs(tolerance)
    added_element = .false. ! init

    if( value < 0. ) stop 'add_value: requires non-negative values!'

    now => first ! init
    do while( value > now%v + tol )
      if( associated( now%next ) ) then
        lst => now
        now => now%next ! go to next element
      else  ! not last
        ! case 1: no element with a value this high
#ifdef INTERNAL_COUNTERS
        n_cases(1) = n_cases(1)+1
#endif
        if(o>0) write(o,'(A,3F7.3)') 'case add new', value
        ! add a new element
        allocate( now%next )
        now%next = new( value &
#ifdef INTERNAL_INDEX
                      , i=now%i+1 &
#endif
                      )
        added_element = .true.
        return ! true
      endif ! not last
    enddo ! while

    if( abs( value - now%v ) <= tol ) then
      ! case 2: matching value
#ifdef INTERNAL_COUNTERS
      n_cases(2) = n_cases(2)+1
#endif
      if(o>0) write(o,'(A,3F7.3)') 'case match', value, now%v
      now%n = now%n+1 ! increase histogram counter
      return ! false
    endif

    ! case 3: lst%v + tol < value < now%v - tol
#ifdef INTERNAL_COUNTERS
    n_cases(3) = n_cases(3)+1
#endif
    if(o>0) write(o,'(A,3F7.3)') 'case between', lst%v, value, now%v
    nxt => now ! will be next after the new element has been inserted
    nullify( now )
    allocate( now )
    now = new( value , next=nxt &
#ifdef INTERNAL_INDEX
             , i=lst%i+1 &
#endif
             )
#ifdef INTERNAL_INDEX
    call increase_index( nxt ) ! start to increase all following indices by one
#endif
    lst%next => now ! repair the link
    added_element = .true.
    return ! true
  endfunction add_value

#ifdef INTERNAL_INDEX
  recursive subroutine increase_index( ll )
  !! increase index ll%i by one and do it for all following elements
  implicit none
    ! arguments
    type(llist), intent(inout) :: ll
    ll%i = ll%i+1 !! increase index by one
    if( associated( ll%next ) ) &
      call increase_index( ll%next )
  endsubroutine increase_index
#endif


#ifdef EXTENDED
!+ extended

  status_t function test()
    iounit_t, parameter :: o=6
    real    :: val(21) = (/3.0,3.5,5.2,0.0,3.5,4.1,6.4,0.0,0.3,9.0,8.3,4.1,4.5,3.2,4.1,6.4,2.3,5.2,0.0,3.0,8.3/)
    integer :: iv, na=0
    type(llist), target :: fst
    real, allocatable :: xh(:)
    integer, allocatable :: nh(:)
    integer :: mh

    do iv = 1, 21
      if( add_value( val(iv), fst ) ) na = na+1
    enddo ! iv
    if(o>0) write(o,'(9(A,I0))') 'added ',na,' new elements'

    mh = get_histogram( fst, xh, nh )
    if(o>0) write(o,'(99F7.3)') xh(1:mh)
    if(o>0) write(o,'(99I7  )') nh(1:mh)
    if(o>0) write(o,'(A,I0)') 'checksum ', sum(nh)
    test = 0
  endfunction

!- extended
#endif

endmodule ! type_llist

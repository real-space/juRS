#include "config.h"

! #define DEBUG

module type_item
implicit none
  private ! default for this module namespace

  public :: item, operator(.in.), operator(.alt.), min_key, max_key
#ifdef EXTENDED 
  public :: test
#endif

  type :: item
    string_t  :: word=''
    integer   :: key=0
  endtype ! item

  interface operator(.in.)
    module procedure map_string2int, map_int2string
  endinterface

  interface operator(.alt.)
    module procedure map_int2string_2nd
  endinterface

  string_t, parameter, public :: TypeItemUnknown = '?'

#ifdef DEBUG
  iounit_t, parameter, private :: o = 6 ! stdout
#else
  iounit_t, parameter, private :: o = 0 ! suppress output
#endif

  contains

  integer function map_string2int( word, items ) result( key )
    character(len=*), intent(in) :: word
    type(item)      , intent(in) :: items(:)

    integer, parameter :: MIN_KEY_INIT = 999999
    integer :: ii

    key = MIN_KEY_INIT ! init minimum key
    do ii = 1, size( items )
      if( word == items(ii)%word ) then
        key = items(ii)%key
#ifdef DEBUG
        if(o>0) write(o,'(9(4A,I0))') __FILE__,' found "',trim(word),'" in list with key=',key
#endif
        return
      endif ! match
      key = min( key, items(ii)%key-1 )
    enddo ! ii
    ! not found ==> return (the smallest key in items) - 1
#ifdef DEBUG
    if(o>0) write(o,'(9(4A,I0))') __FILE__,' "',trim(word),'" not found in list use default key=',key
#endif
  endfunction ! .in.

  integer function max_key( items )
    type(item), intent(in) :: items(:)
    max_key = maxval( items(:)%key )
  endfunction ! max_key

  integer function min_key( items )
    type(item), intent(in) :: items(:)
    min_key = minval( items(:)%key )
  endfunction ! min_key

  string_t function map_int2string( key, items ) result( word )
    integer   , intent(in) :: key
    type(item), intent(in) :: items(:)

    integer :: ii

    word = TypeItemUnknown
    do ii = 1, size( items )
      if( key == items(ii)%key ) then
        word = items(ii)%word
#ifdef DEBUG
        if(o>0) write(o,'(2A,I0,9A)') __FILE__,' entry for key=',key,' found "',trim(word),'"'
#endif
        return
      endif ! match
    enddo ! ii
#ifdef DEBUG
    if(o>0) write(o,'(2A,I0,9A)') __FILE__,' no entry for key=',key,' found, use default "',trim(word),'"'
#endif
  endfunction ! .in.

  string_t function map_int2string_2nd( key, items ) result( word )
    integer   , intent(in) :: key
    type(item), intent(in) :: items(:)

    logical, parameter :: FirstHitIfNoAltFound = .false.
    logical :: found1before
    integer :: ii

    word = ' ' ! init clear
    found1before = .false.
    do ii = 1, size( items )
      if( key == items(ii)%key ) then
        if( found1before ) then
          word = items(ii)%word
          return
        endif ! found1before
        found1before = .true.
        if( FirstHitIfNoAltFound ) word = items(ii)%word
      endif ! match
    enddo ! ii
#ifdef DEBUG
    if( FirstHitIfNoAltFound ) then
      if(o>0) write(o,'(2A,I0,9A)') __FILE__,' no alternative entry for key=',key,' found, use default "',trim(word),'"'
    else
      if(o>0) write(o,'(2A,I0,9A)') __FILE__,' no alternative entry for key=',key,' found, return " ".'
    endif
#endif
  endfunction ! .alt.

#ifdef EXTENDED 
!+ extended

  status_t function test( )
    type(item), parameter :: dict(13)= (/ &
      item('integrated',1), item('paired',1), item('no',1), item('0',1), item('1',1), &
      item('polarized',2), item('2',2), item('',2), item('yes',2), &
      item('noco',4), item('non-collinear',4), item('3',4), item('4',4) /)

    write(*,'(A,I0)') 'key=', ( 'no' .in. dict )
    write(*,'(A,I0)') 'key=', ( 'yes' .in. dict )
    write(*,'(A,I0)') 'key=', ( 'noco' .in. dict )
    write(*,'(A,I0)') 'key=', ( ' ' .in. dict )
    write(*,  '(9A)') 'str=', ( 1 .in. dict )
    write(*,  '(9A)') 'str=', ( 2 .in. dict )
    write(*,  '(9A)') 'str=', ( -2 .in. dict )
    write(*,  '(9A)') 'str=', ( -2 .in. dict )
    write(*,  '(9A)') 'str=', ( 3 .in. dict )
    write(*,  '(9A)') 'str=', ( 4 .in. dict )
!     write(*,  '(9A)') 'alt_str=', alternative( 4, dict )
    write(*,  '(9A)') 'alt_str=', ( 4 .alt. dict )

    test = 0
  endfunction ! test

!- extended
#endif

endmodule ! type_item

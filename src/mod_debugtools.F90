#include "config.h"

! #define DEBUG

!! @author Paul Baumeister
!! @version 3.0
!!
!! toolbox
module debugtools
implicit none
  public ! default for this module namespace
  character(len=*), parameter, private :: sym = 'DEBUGTOOLS' !! module symbol

  interface NaN_search ! ( array, symfun, text )
    module procedure search_for_NaN_r0, &
                     search_for_NaN_r1, &
                     search_for_NaN_r2, &
                     search_for_NaN_r3, &
                     search_for_NaN_r4, &
                     search_for_NaN_c0, &
                     search_for_NaN_c1, &
                     search_for_NaN_c2, &
                     search_for_NaN_c3, &
                     search_for_NaN_c4
  endinterface

  iounit_t, parameter, private :: o=6 ! output to stdout

  contains

  subroutine translate_matrix( n, tau, m, tm )
    integer, intent(in)  :: n, tau
    complex, intent(in)  :: m(n,n)
    complex, intent(out) :: tm(n,n)

    character(len=*), parameter :: fun = ' translate_matrix: '
    complex, parameter :: phase = (0.0,1.0) ! i
    integer :: t, i1, i2, j1, j2, iph
    if( n < 1 ) return
    t = modulo( tau-1, n ) +1

    if( t == n .or. t == 0 ) then
      tm = m
      return
    endif ! t == n

    do i1 = 1, n
      j1 = i1 + t
      if( j1 > n ) then
        j1 = j1 - n
        iph = 1
      endif

      do i2 = 1, n
        j2 = i2 + t
        if( j2 > n ) then
          j2 = j2 - n
          tm(j2,j1) = m(i2,i1) * phase ** (1-iph)
        else
          tm(j2,j1) = m(i2,i1) * phase ** (-iph)
        endif
      enddo ! i2
    enddo ! i1

  endsubroutine ! translate_matrix


!   !! sample subroutine
!   status_t function check_( ) result( ist )
!   implicit none
!     ! parameter
!     character(len=*), parameter     :: fun = ' check_: '
!     ! arguments
!     ...
!     ! local vars
!     ist = 0
!     ...
!   endfunction check_


  !! todo: turn to character(len=256) function explain_iostat( ios )
  !! the explanations for different iostat values are taken from
  !! http://www.ncsa.uiuc.edu/UserInfo/Resources/Hardware/IBMp690/IBM/usr/share/man/info/en_US/xlf/html/lr88.HTM
  subroutine explain_iostat( ios, unt )
    status_t, intent(in)            :: ios !!
    iounit_t, intent(in), optional  :: unt !! unit to write to

    character(len=*), parameter     :: fun = ' explain_iostat: '
    iounit_t                        :: u
    logical                         :: cnverr=.false.

    u = o ; if( present( unt ) ) u = unt
    if( u == 0 ) return

    if( ios == 0 ) return ! without explanation

    write(u,'(A,I3,A)',advance='no') 'IOstatus =', ios, ' : '
    selectcase( ios )
#ifndef KEI 
    !=================================================================
    case(  -4 ) ; write(u,'(A)') "End of record encountered on a nonadvancing, format-directed READ of an external file."
    !=================================================================
    case(  -2 ) ; write(u,'(A)') "End of file encountered on READ of an internal file."
    case(  -1 ) ; write(u,'(A)') "End of file encountered on sequential READ of an external file, or END= is specified ",&
"on a direct access read and the record is nonexistent."
    !=================================================================
    case(   0 ) ; stop "DEBUGtools: explain_iostat: ios==0 should return without explanation."
    !=================================================================
    case(   1 ) ; write(u,'(A)') "END= is not specified on a direct access READ and the record is nonexistent."
    case(   2 ) ; write(u,'(A)') "End of file encountered on WRITE of an internal file."
    case(   6 ) ; write(u,'(A)') "File cannot be found and STATUS='OLD' is specified on an OPEN statement."
    case(  10 ) ; write(u,'(A)') "Read error on direct file."
    case(  11 ) ; write(u,'(A)') "Write error on direct file."
    case(  12 ) ; write(u,'(A)') "Read error on sequential file."
    case(  13 ) ; write(u,'(A)') "Write error on sequential file."
    case(  14 ) ; write(u,'(A)') "Error opening file."
    case(  15 ) ; write(u,'(A)') "Permanent I/O error encountered on file."
    case(  37 ) ; write(u,'(A)') "Dynamic memory allocation failure - out of memory."
    case(  38 ) ; write(u,'(A)') "REWIND error."
    case(  39 ) ; write(u,'(A)') "ENDFILE error."
    case(  40 ) ; write(u,'(A)') "BACKSPACE error."
    case( 107 ) ; write(u,'(A)') "File exists and STATUS='NEW' was specified on an OPEN statement."
    case( 119 ) ; write(u,'(A)') "BACKSPACE statement attempted on unit connected to a tape device."
    case( 122 ) ; write(u,'(A)') "Incomplete record encountered during direct access READ."
    case( 130 ) ; write(u,'(A)') "ACTION='READWRITE' specified on an OPEN statement to connect a pipe."
    case( 135 ) ; write(u,'(A)') "The user program is making calls to an unsupported version of the XL Fortran Run-time ",&
"Environment."
    case( 139 ) ; write(u,'(A)') "I/O operation not permitted on the unit because the file was not opened with an ",&
"appropriate value for the ACTION= specifier."
    case( 142 ) ; write(u,'(A)') "CLOSE error."
    case( 144 ) ; write(u,'(A)') "INQUIRE error."
    case( 152 ) ; write(u,'(A)') "ACCESS='DIRECT' is specified on an OPEN statement for an AIX file that can only ",&
"be accessed sequentially."
    case( 153 ) ; write(u,'(A)') "POSITION='REWIND' or POSITION='APPEND' is specified on an OPEN statement and the ",&
"file is an AIX pipe."
    case( 156 ) ; write(u,'(A)') "Invalid value for RECL= specifier on an OPEN statement."
    case( 159 ) ; write(u,'(A)') "External file input could not be flushed because the associated device is not seekable."
    case( 165 ) ; write(u,'(A)') "The record number of the next record that can be read or written is out of the range ",&
"of the variable specified with the NEXTREC= specifier of the INQUIRE statement."
    case( 169 ) ; write(u,'(A)') "The asynchronous I/O statement cannot be completed because the unit is connected for ",&
"synchronous I/O only."
    case( 172 ) ; write(u,'(A)') "The connection failed because the file does not allow asynchronous I/O."
    case( 173 ) ; write(u,'(A)') "An asynchronous READ statement was executed while asynchronous WRITE statements were ",&
"pending for the same unit, or an asynchronous WRITE statement was executed while asynchronous READ statements were ",&
"pending for the same unit."
    case( 174 ) ; write(u,'(A)') "The synchronous I/O statement cannot be completed because an earlier asynchronous I/O ",&
"statement has not been completed."
    case( 175 ) ; write(u,'(A)') "The WAIT statement cannot be completed because the value of the ID= specifier is invalid."
    case( 176 ) ; write(u,'(A)') "The WAIT statement cannot be completed because the corresponding asynchronous I/O statement ",&
"is in a different scoping unit."
    case( 178 ) ; write(u,'(A)') "The asynchronous direct WRITE statement for a record is not permitted because an earlier ",&
"asynchronous direct WRITE statement for the same record has not been completed."
    case( 179 ) ; write(u,'(A)') "The I/O operation cannot be performed on the unit because there are still incomplete ",&
"asynchronous I/O operations on the unit."
    case( 181 ) ; write(u,'(A)') "A file cannot be connected to a unit because multiple connections are allowed for ",&
"synchronous I/O only."
    case( 182 ) ; write(u,'(A)') "Invalid value for UWIDTH= option. It must be set to either 32 or 64."
    case( 183 ) ; write(u,'(A)') "The maximum record length for the unit is out of the range of the scalar variable ",&
"specified with the RECL= specifier in the INQUIRE statement."
    case( 184 ) ; write(u,'(A)') "The number of bytes of data transmitted is out of the range of the scalar variable ",&
"specified with the SIZE= or NUM= specifier in the I/O statement."
    case( 185 ) ; write(u,'(A)') "A file cannot be connected to two units with different UWIDTH values."
    case( 186 ) ; write(u,'(A)') "Unit numbers must be between 0 and 2,147,483,647."
    !=================================================================
    case(  16 ) ; write(u,'(A)') "Value of REC= specifier invalid on direct I/O."
    case(  17 ) ; write(u,'(A)') "I/O statement not allowed on direct file."
    case(  18 ) ; write(u,'(A)') "Direct I/O statement on an unconnected unit."
    case(  19 ) ; write(u,'(A)') "Unformatted I/O attempted on formatted file."
    case(  20 ) ; write(u,'(A)') "Formatted I/O attempted on unformatted file."
    case(  21 ) ; write(u,'(A)') "Sequential I/O attempted on direct file."
    case(  22 ) ; write(u,'(A)') "Direct I/O attempted on sequential file."
    case(  23 ) ; write(u,'(A)') "Attempt to connect a file that is already connected to another unit."
    case(  24 ) ; write(u,'(A)') "OPEN specifiers do not match the connected file's attributes."
    case(  25 ) ; write(u,'(A)') "RECL= specifier omitted on an OPEN statement for a direct file."
    case(  26 ) ; write(u,'(A)') "RECL= specifier on an OPEN statement is negative."
    case(  27 ) ; write(u,'(A)') "ACCESS= specifier on an OPEN statement is invalid."
    case(  28 ) ; write(u,'(A)') "FORM= specifier on an OPEN statement is invalid."
    case(  29 ) ; write(u,'(A)') "STATUS= specifier on an OPEN statement is invalid."
    case(  30 ) ; write(u,'(A)') "BLANK= specifier on an OPEN statement is invalid."
    case(  31 ) ; write(u,'(A)') "FILE= specifier on an OPEN or INQUIRE statement is invalid."
    case(  32 ) ; write(u,'(A)') "STATUS='SCRATCH' and FILE= specifier specified on same OPEN statement."
    case(  33 ) ; write(u,'(A)') "STATUS='KEEP' specified on CLOSE statement when file was opened with STATUS='SCRATCH'."
    case(  34 ) ; write(u,'(A)') "Value of STATUS= specifier on CLOSE statement is invalid."
    case(  36 ) ; write(u,'(A)') "Invalid unit number specified in an I/O statement."
    case(  47 ) ; write(u,'(A)') "A namelist input item was specified with one or more components of nonzero rank."
    case(  48 ) ; write(u,'(A)') "A namelist input item specified a zero-sized array."
    case(  58 ) ; write(u,'(A)') "Format specification error."
    case(  93 ) ; write(u,'(A)') "I/O statement not allowed on error unit (unit 0)."
    case( 110 ) ; write(u,'(A)') "Illegal edit descriptor used with a data item in formatted I/O."
    case( 120 ) ; write(u,'(A)') "The NLWIDTH setting exceeds the length of a record."
    case( 125 ) ; write(u,'(A)') "BLANK= specifier given on an OPEN statement for an unformatted file."
    case( 127 ) ; write(u,'(A)') "POSITION= specifier given on an OPEN statement for a direct file."
    case( 128 ) ; write(u,'(A)') "POSITION= specifier value on an OPEN statement is invalid."
    case( 129 ) ; write(u,'(A)') "ACTION= specifier value on an OPEN statement is invalid."
    case( 131 ) ; write(u,'(A)') "DELIM= specifier given on an OPEN statement for an unformatted file."
    case( 132 ) ; write(u,'(A)') "DELIM= specifier value on an OPEN statement is invalid."
    case( 133 ) ; write(u,'(A)') "PAD= specifier given on an OPEN statement for an unformatted file."
    case( 134 ) ; write(u,'(A)') "PAD= specifier value on an OPEN statement is invalid."
    case( 136 ) ; write(u,'(A)') "ADVANCE= specifier value on a READ statement is invalid."
    case( 137 ) ; write(u,'(A)') "ADVANCE='NO' is not specified when SIZE= is specified on a READ statement."
    case( 138 ) ; write(u,'(A)') "ADVANCE='NO' is not specified when EOR= is specified on a READ statement."
    case( 145 ) ; write(u,'(A)') "READ or WRITE attempted when file is positioned after the endfile record."
    case( 163 ) ; write(u,'(A)') "Multiple connections to a file located on a non-random access device are not allowed."
    case( 164 ) ; write(u,'(A)') "Multiple connections with ACTION='WRITE' or ACTION='READWRITE' are not allowed."
    case( 170 ) ; write(u,'(A)') "ASYNCH= specifier value on an OPEN statement is invalid."
    case( 171 ) ; write(u,'(A)') "ASYNCH= specifier given on an OPEN statement is invalid because the FORM= specifier ",&
"is set to FORMATTED."
    case( 177 ) ; write(u,'(A)') "The unit was closed while there were still incomplete asynchronous I/O operations."
    !=================================================================
    case(   3 ) ; write(u,'(A)') "End of record encountered on an unformatted file."
    case(   4 ) ; write(u,'(A)') "End of record encountered on a formatted external file using advancing I/O."
    case(   5 ) ; write(u,'(A)') "End of record encountered on an internal file."
    case(   7 ) ; write(u,'(A)') "Incorrect format of list-directed input found in an external file." ; cnverr=.true.
    case(   8 ) ; write(u,'(A)') "Incorrect format of list-directed input found in an internal file." ; cnverr=.true.
    case(   9 ) ; write(u,'(A)') "List-directed or NAMELIST data item too long for the internal file." ; cnverr=.true.
    case(  41 ) ; write(u,'(A)') "Valid logical input not found in external file."
    case(  42 ) ; write(u,'(A)') "Valid logical input not found in internal file."
    case(  43 ) ; write(u,'(A)') "Complex value expected using list-directed or NAMELIST input in external file but not found."
    case(  44 ) ; write(u,'(A)') "Complex value expected using list-directed or NAMELIST input in internal file but not found."
    case(  45 ) ; write(u,'(A)') "NAMELIST item name specified with unknown or invalid derived-type component name in ",&
"NAMELIST input."
    case(  46 ) ; write(u,'(A)') "NAMELIST item name specified with an invalid substring range in NAMELIST input."
    case(  49 ) ; write(u,'(A)') "List-directed or namelist input contained an invalid delimited character string."
    case(  56 ) ; write(u,'(A)') "Invalid digit found in input for B, O or Z format edit descriptors."
    case(  84 ) ; write(u,'(A)') "NAMELIST group header not found in external file." ; cnverr=.true.
    case(  85 ) ; write(u,'(A)') "NAMELIST group header not found in internal file." ; cnverr=.true.
    case(  86 ) ; write(u,'(A)') "Invalid NAMELIST input value found in external file."
    case(  87 ) ; write(u,'(A)') "Invalid NAMELIST input value found in internal file."
    case(  88 ) ; write(u,'(A)') "Invalid name found in NAMELIST input."
    case(  90 ) ; write(u,'(A)') "Invalid character in NAMELIST group or item name in input."
    case(  91 ) ; write(u,'(A)') "Invalid NAMELIST input syntax."
    case(  92 ) ; write(u,'(A)') "Invalid subscript list for NAMELIST item in input."
    case(  94 ) ; write(u,'(A)') "Invalid repeat specifier for list-directed or NAMELIST input in external file."
    case(  95 ) ; write(u,'(A)') "Invalid repeat specifier for list-directed or NAMELIST input in internal file."
    case(  96 ) ; write(u,'(A)') "Integer overflow in input."
    case(  97 ) ; write(u,'(A)') "Invalid decimal digit found in input."
    case(  98 ) ; write(u,'(A)') "Input too long for B, O or Z format edit descriptors."
    case( 121 ) ; write(u,'(A)') "Output length of NAMELIST item name or NAMELIST group name is longer than the maximum ",&
"record length or the output width specified by the NLWIDTH option." ; cnverr=.true.
#endif
    !=================================================================
    case default ; write(u,'(A)') "<Explanation not implemented>."
    endselect ! ios
  endsubroutine ! explain_iostat

  subroutine check_spin( nspins, nspn )
    integer, intent(in)             :: nspins
    integer, intent(in), optional   :: nspn

    character(len=*), parameter     :: fun = ' check_spin: '

    if( nspins < 1 .or. nspins > 2 ) then
      write(*,'(3A,I0)') sym, fun, 'wrong value for nspins = ', nspins
      stop 'NSPINS out of {1,2}'
    endif ! nspins not in {1,2}

    if( .not. present( nspn ) ) return

    if( nspn < 1 .or. nspn > nspins ) then
      write(*,'(3A,I0)') sym, fun, 'wrong value for nspn = ', nspn
      stop 'NSPN out of {1,nspins}'
    endif ! nspn not in {1,nspins}
  endsubroutine ! check_spin


  ! NaN-search subroutines
  !-----------------------------------------------------------------
  status_t function search_for_NaN_r0( a, symfun, text ) result( inan )
    real, intent(in)                :: a
    character(len=*), intent(in)    :: symfun
    character(len=*), intent(in), optional :: text
#ifdef NaN_SEARCH
    inan = count( a /= a )
    if( inan > 0 ) then
      isze = size( a )
      call show( inan, size(a), symfun, text )
      stop 'NaN found (rank0 object)'
    endif ! a /= a
#else
    inan = 0
#endif
  endfunction ! search_for_NaN

  status_t function search_for_NaN_r1( a, symfun, text ) result( inan )
    real, intent(in)                :: a(:)
    character(len=*), intent(in)    :: symfun
    character(len=*), intent(in), optional :: text
#ifdef NaN_SEARCH
    inan = count( a /= a )
    if( inan > 0 ) then
      isze = size( a )
      call show( inan, size(a), symfun, text )
      stop 'NaN found (rank1 object)'
    endif ! a /= a
#else
    inan = 0
#endif
  endfunction ! search_for_NaN

  status_t function search_for_NaN_r2( a, symfun, text ) result( inan )
    real, intent(in)                :: a(:,:)
    character(len=*), intent(in)    :: symfun
    character(len=*), intent(in), optional :: text
#ifdef NaN_SEARCH
    inan = count( a /= a )
    if( inan > 0 ) then
      isze = size( a )
      call show( inan, size(a), symfun, text )
      stop 'NaN found (rank2 object)'
    endif ! a /= a
#else
    inan = 0
#endif
  endfunction ! search_for_NaN

  status_t function search_for_NaN_r3( a, symfun, text ) result( inan )
    real, intent(in)                :: a(:,:,:)
    character(len=*), intent(in)    :: symfun
    character(len=*), intent(in), optional :: text
#ifdef NaN_SEARCH
    inan = count( a /= a )
    if( inan > 0 ) then
      isze = size( a )
      call show( inan, size(a), symfun, text )
      stop 'NaN found (rank3 object)'
    endif ! a /= a
#else
    inan = 0
#endif
  endfunction ! search_for_NaN

  status_t function search_for_NaN_r4( a, symfun, text ) result( inan )
    real, intent(in)                :: a(:,:,:,:)
    character(len=*), intent(in)    :: symfun
    character(len=*), intent(in), optional :: text
#ifdef NaN_SEARCH
    inan = count( a /= a )
    if( inan > 0 ) then
      isze = size( a )
      call show( inan, size(a), symfun, text )
      stop 'NaN found (rank4 object)'
    endif ! a /= a
#else
    inan = 0
#endif
  endfunction ! search_for_NaN

  status_t function search_for_NaN_c0( a, symfun, text ) result( inan )
    complex, intent(in)             :: a
    character(len=*), intent(in)    :: symfun
    character(len=*), intent(in), optional :: text
#ifdef NaN_SEARCH
    inan = count( a /= a )
    if( inan > 0 ) then
      isze = size( a )
      call show( inan, size(a), symfun, text )
      stop 'NaN found (rank0 object)'
    endif ! a /= a
#else
    inan = 0
#endif
  endfunction ! search_for_NaN

  status_t function search_for_NaN_c1( a, symfun, text ) result( inan )
    complex, intent(in)             :: a(:)
    character(len=*), intent(in)    :: symfun
    character(len=*), intent(in), optional :: text
#ifdef NaN_SEARCH
    inan = count( a /= a )
    if( inan > 0 ) then
      isze = size( a )
      call show( inan, size(a), symfun, text )
      stop 'NaN found (rank1 object)'
    endif ! a /= a
#else
    inan = 0
#endif
  endfunction ! search_for_NaN

  status_t function search_for_NaN_c2( a, symfun, text ) result( inan )
    complex, intent(in)             :: a(:,:)
    character(len=*), intent(in)    :: symfun
    character(len=*), intent(in), optional :: text
#ifdef NaN_SEARCH
    inan = count( a /= a )
    if( inan > 0 ) then
      isze = size( a )
      call show( inan, size(a), symfun, text )
      stop 'NaN found (rank2 object)'
    endif ! a /= a
#else
    inan = 0
#endif
  endfunction ! search_for_NaN

  status_t function search_for_NaN_c3( a, symfun, text ) result( inan )
    complex, intent(in)             :: a(:,:,:)
    character(len=*), intent(in)    :: symfun
    character(len=*), intent(in), optional :: text
#ifdef NaN_SEARCH
    inan = count( a /= a )
    if( inan > 0 ) then
      isze = size( a )
      call show( inan, size(a), symfun, text )
      stop 'NaN found (rank3 object)'
    endif ! a /= a
#else
    inan = 0
#endif
  endfunction ! search_for_NaN

  status_t function search_for_NaN_c4( a, symfun, text ) result( inan )
    complex, intent(in)             :: a(:,:,:,:)
    character(len=*), intent(in)    :: symfun
    character(len=*), intent(in), optional :: text
#ifdef NaN_SEARCH
    inan = count( a /= a )
    if( inan > 0 ) then
      isze = size( a )
      call show( inan, size(a), symfun, text )
      stop 'NaN found (rank4 object)'
    endif ! a /= a
#else
    inan = 0
#endif
  endfunction ! search_for_NaN

  subroutine show( inan, isze, symfun, text )
  use configuration, only: o
    integer, intent(in) :: inan, isze
    character(len=*), intent(in) :: symfun
    character(len=*), optional, intent(in) :: text
    if( inan > 0 ) then
      if( present( text ) ) then
        write(o,'(A,2(A,I0),9A)') symfun, ': ',inan,' of ',isze,' NaN found in "', trim(text), '".'
      else  ! present text
        write(o,'(A,2(A,I0),9A)') symfun, ': ',inan,' of ',isze,' NaN found.'
      endif ! present text
    endif ! inan > 0
  endsubroutine ! show

#ifdef EXTENDED
!+ extended

  status_t function test( )
    write(*,*,iostat=test) __FILE__,' no module test implemented!'
  endfunction ! test

!- extended
#endif

endmodule ! debugtools

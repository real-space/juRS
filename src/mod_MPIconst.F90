!! @author Paul Baumeister
!! @version 3.0
!!
!! replacement for the MPI header file
module MPIconst
implicit none
  public ! default for this module namespace
#ifndef NOMPI
  include 'mpif.h'
#else
  integer, parameter :: MPI_PACKED = 1275068687
  integer, parameter :: MPI_UB = 1275068433
  integer, parameter :: MPI_LB = 1275068432
  integer, parameter :: MPI_REAL = 1275069468
  integer, parameter :: MPI_INTEGER = 1275069467
  integer, parameter :: MPI_LOGICAL = 1275069469
  integer, parameter :: MPI_DOUBLE_PRECISION = 1275070495
  integer, parameter :: MPI_COMPLEX = 1275070494
  integer, parameter :: MPI_DOUBLE_COMPLEX = 1275072546
  integer, parameter :: MPI_CHARACTER = 1275068717
  integer, parameter :: MPI_INTEGER1 = 1275068717
  integer, parameter :: MPI_INTEGER2 = 1275068975
  integer, parameter :: MPI_INTEGER4 = 1275069488
  integer, parameter :: MPI_REAL4 = 1275069479
  integer, parameter :: MPI_REAL8 = 1275070505
  integer, parameter :: MPI_COMPLEX16 = 1275072554 
  integer, parameter :: MPI_2INTEGER = 1275070496
  integer, parameter :: MPI_2REAL = 1275070497
  integer, parameter :: MPI_2DOUBLE_PRECISION = 1275072547
  integer, parameter :: MPI_2COMPLEX = 1275072548
  integer, parameter :: MPI_2DOUBLE_COMPLEX = 1275076645
  integer, parameter :: MPI_COMM_WORLD = 1140850688
  integer, parameter :: MPI_COMM_SELF = 1140850689
  integer, parameter :: MPI_GROUP_EMPTY = 1207959552
  integer, parameter :: MPI_IDENT = 0
  integer, parameter :: MPI_CONGRUENT = 1
  integer, parameter :: MPI_SIMILAR = 2
  integer, parameter :: MPI_UNEQUAL = 3

  integer, parameter :: MPI_MAX = 1476395009
  integer, parameter :: MPI_MIN = 1476395010
  integer, parameter :: MPI_SUM = 1476395011
  integer, parameter :: MPI_PROD = 1476395012
  integer, parameter :: MPI_LAND = 1476395013
  integer, parameter :: MPI_BAND = 1476395014
  integer, parameter :: MPI_LOR = 1476395015
  integer, parameter :: MPI_BOR = 1476395016
  integer, parameter :: MPI_LXOR = 1476395017
  integer, parameter :: MPI_BXOR = 1476395018
  integer, parameter :: MPI_MINLOC = 1476395019
  integer, parameter :: MPI_MAXLOC = 1476395020

  integer, parameter :: MPI_TAG_UB = 1681915906
  integer, parameter :: MPI_HOST = 1681915908
  integer, parameter :: MPI_IO = 1681915910
  integer, parameter :: MPI_WTIME_IS_GLOBAL = 1681915912
  integer, parameter :: MPI_COMM_NULL = 67108864
  integer, parameter :: MPI_OP_NULL = 402653184
  integer, parameter :: MPI_GROUP_NULL = 134217728
  integer, parameter :: MPI_DATATYPE_NULL = 201326592
  integer, parameter :: MPI_REQUEST_NULL = 738197504
  integer, parameter :: MPI_ERRHANDLER_NULL = 335544320
  integer, parameter :: MPI_MAX_PROCESSOR_NAME = 127
  integer, parameter :: MPI_MAX_ERROR_STRING = 1023
  integer, parameter :: MPI_UNDEFINED = -32766
  integer, parameter :: MPI_KEYVAL_INVALID = 603979776
  integer, parameter :: MPI_BSEND_OVERHEAD = 56
  integer, parameter :: MPI_PROC_NULL = -1
  integer, parameter :: MPI_ANY_SOURCE = -2
  integer, parameter :: MPI_ANY_TAG = -1
  integer, parameter :: MPI_BOTTOM = 0

  integer, parameter :: MPI_GRAPH = 1
  integer, parameter :: MPI_CART = 2
  integer, parameter :: MPI_SOURCE = 3
  integer, parameter :: MPI_TAG = 4
  integer, parameter :: MPI_ERROR = 5
  integer, parameter :: MPI_ERRORS_ARE_FATAL = 1409286144
  integer, parameter :: MPI_ERRORS_RETURN = 1409286145
  integer, parameter :: MPI_SUCCESS = 0

  integer, parameter :: MPI_ERR_BUFFER = 1
  integer, parameter :: MPI_ERR_COUNT = 2
  integer, parameter :: MPI_ERR_TYPE = 3
  integer, parameter :: MPI_ERR_TAG = 4
  integer, parameter :: MPI_ERR_COMM = 5
  integer, parameter :: MPI_ERR_RANK = 6
  integer, parameter :: MPI_ERR_ROOT = 7
  integer, parameter :: MPI_ERR_GROUP = 8
  integer, parameter :: MPI_ERR_OP = 9
  integer, parameter :: MPI_ERR_TOPOLOGY = 10
  integer, parameter :: MPI_ERR_DIMS = 11
  integer, parameter :: MPI_ERR_ARG = 12
  integer, parameter :: MPI_ERR_UNKNOWN = 13
  integer, parameter :: MPI_ERR_TRUNCATE = 14
  integer, parameter :: MPI_ERR_OTHER = 15
  integer, parameter :: MPI_ERR_INTERN = 16
  integer, parameter :: MPI_ERR_IN_STATUS = 17
  integer, parameter :: MPI_ERR_PENDING = 18
  integer, parameter :: MPI_ERR_REQUEST = 19
  integer, parameter :: MPI_ERR_LASTCODE = 1073741823

  integer, parameter :: MPI_STATUS_SIZE = 5
  integer, parameter :: MPI_OFFSET_KIND = 8
#endif

  ! self defined variables
#ifdef SINGLE_PRECISION
  integer, parameter :: PREC(1:2) = (/ MPI_REAL, MPI_COMPLEX /) !! precision for [int,real,complex]
#else
  integer, parameter :: PREC(1:2) = (/ MPI_DOUBLE_PRECISION, MPI_DOUBLE_COMPLEX /) !! precision for [int,real,complex]
#endif

  contains

  real function Wtime()
#ifndef NOMPI
    Wtime = real( MPI_Wtime() )
#else
!   call cpu_time( Wtime )
    integer :: v(8)
    call date_and_time(values=v)
    Wtime = ( v(5)*60. + v(6) )*60. + v(7) + v(8)*.001 ! hours minutes seconds and miliseconds
#endif
  endfunction ! Wtime

  integer function test( )
    write(*,*,iostat=test) __FILE__,' no module test implemented!'
  endfunction ! test

endmodule ! MPIconst

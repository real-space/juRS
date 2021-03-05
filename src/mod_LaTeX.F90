#include "config.h"

#define DEBUG

!! @author Paul Baumeister
module LaTeX
! use type_item, only: item, operator(.in.)
implicit none
  public ! all public
  character(len=*), parameter, private :: sym = 'TeX'

  interface operator(+)
    module procedure cat_si, cat_is, cat_ss
  endinterface
  private :: cat_si, cat_is, cat_ss

  interface opt
    module procedure optional_logical!, optional_string
  endinterface
  private :: optional_logical!, optional_string

#ifdef DEBUG
  iounit_t, parameter, private :: o = 6 ! stdout
#else
  iounit_t, parameter, private :: o = 0 ! no output
#endif


contains

  string_t function TeX_begin( sec ) result( str )
    character(len=*), intent(in) :: sec
    str='\begin{'+trim(sec)+'}'
  endfunction

  string_t function TeX_end( sec ) result( str )
    character(len=*), intent(in) :: sec
    str='\end{'+trim(sec)+'}'
  endfunction


  status_t function TeX_table( tu, data, label, caption, placement, centering, legends, alignments ) result( ios )
    iounit_t, intent(in) :: tu
    character(len=*), intent(in) :: data(:,:)
    character(len=*), intent(in), optional :: label, caption
    character(len=*), intent(in), optional :: placement
    logical, intent(in), optional :: centering
    character, intent(in), optional :: alignments(:)
    character(len=*), intent(in), optional :: legends(:)
    !================================================================================
    character(len=*), parameter :: elem='table'
    integer :: i1, i2
    if( tu <= 0 ) return ! do not write to silenced tex-units
    if( present( placement ) ) then
      write(tu,'(99A)',iostat=ios) TeX_begin( elem ), trim( placement )
    else
      write(tu,'(99A)',iostat=ios) TeX_begin( elem )
    endif
    if( opt( centering ) ) &
    write(tu,'(99A)',iostat=ios) '\centering % used for centering table'

    ios = TeX_tabular( tu, data, legends, alignments )

    if( present( caption ) ) &
    write(tu,'(99A)',iostat=ios) '\caption{', trim( caption ), '}'
    if( present( label ) ) &
    write(tu,'(99A)',iostat=ios) '\label{', trim( label ), '}'
    write(tu,'(99A)',iostat=ios) TeX_end( elem )
    write(tu,'(99A)',iostat=ios) '%'
  endfunction

  status_t function TeX_tabular( tu, data, alignments, legends ) result( ios )
    iounit_t, intent(in) :: tu
    character(len=*), intent(in) :: data(1:,1:)
    character, intent(in), optional :: alignments(:)
    character(len=*), intent(in), optional :: legends(:)
    !================================================================================
    character(len=*), parameter :: elem='tabular'
    character :: align(size(data,1))
    integer :: i, j, n
    if( tu <= 0 ) return ! do not write to silenced tex-units
    align = 'c' ; if( present( alignments ) ) align = alignments
      write(tu,'(999A)',iostat=ios) trim( TeX_begin( elem ) ), '{',align(:),'}'
    n = size( data, 1 )
      write(tu,'(999A)',iostat=ios) '\hline'
    if( present( legends ) ) then
      write(tu,'(999A)',iostat=ios) ( trim( legends(i) ), TeX_ampersands( last=(i==n) ), i=1,n )
      write(tu,'(999A)',iostat=ios) '\hline'
    endif
    do j = 1, size( data, 2 )
      write(tu,'(999A)',iostat=ios) ( trim( data(i,j) ), TeX_ampersands( last=(i==n) ), i=1,n )
    enddo ! j
      write(tu,'(999A)',iostat=ios) '\hline'
      write(tu,'(999A)',iostat=ios) TeX_end( elem )
      write(tu,'(99A)',iostat=ios) '%'
  endfunction ! table

  character(len=3) function TeX_ampersands( last ) result( str )
    logical, intent(in) :: last
    str = ' & ' ; if( last ) str = ' \\'
  endfunction


  string_t function cat_ss( s1, s2 ) result( str )
    character(len=*), parameter           :: fun = ' cat: '
    character(len=*), intent(in)          :: s1, s2
    integer                               :: ios, ln
#ifdef DEBUG
    ln = len_trim(s1) + len_trim(s2)
    if(o>0) then
      if( ln > len(str) ) write(o,'(3A,9(I0,A))') sym, fun, 'sum of string lengths is ', ln, ' but result has length', len(str)
      if( ios /= 0 ) write(o,'(3A,9(I0,A))') sym, fun, 'writing to string returned IOstatus=', ios
    endif ! o>0
#endif
    write(unit=str,fmt='(9A)',iostat=ios) trim(s1), trim(s2)
  endfunction

  string_t function cat_is( i1, s2 ) result( str )
    integer, intent(in)                   :: i1
    character(len=*), intent(in)          :: s2
    integer                               :: ios
    write(unit=str,fmt='(I0,A)',iostat=ios) i1, trim(s2)
  endfunction

  string_t function cat_si( s1, i2 ) result( str )
    character(len=*), intent(in)          :: s1
    integer, intent(in)                   :: i2
    integer                               :: ios
    write(unit=str,fmt='(A,I0)',iostat=ios) trim(s1), i2
  endfunction

  logical function optional_logical( l ) result( f )
    logical, intent(in), optional :: l
    f = .false. ; if( present( l ) ) f = l
  endfunction

!   string_t function optional_string( s ) result( str )
!     character(len=*), intent(in), optional :: s
!     str = '' ; if( present( s ) ) str = s
!   endfunction

! #ifdef XLM_NAMES
! !+ Xlm_names
! 
!   string_t function Xlm_rl_name( ilm ) result( n )
!   ! writes the expressions for spherical harmonics (real combinations) 
!   implicit none
!     ! parameters
!     character(len=*), parameter     :: fun = ' Xlm_rl_name: '
!     ! arguments
!     integer, intent(in)   :: ilm
! 
!     selectcase( ilm )
! #ifdef DEBUG
!     case( :0 ) ; stop 'Xlm_rl_name: ILM should at least be 1'
! #endif
!     case( 1) ; n = 'S_1         ' !  =  f ! s-orbital
! 
!     case( 2) ; n = 'P_x         ' !  =  f*sqrt(3.)*v(X) ! px-orbital
!     case( 3) ; n = 'P_z         ' !  =  f*sqrt(3.)*v(Z) ! pz-orbital
!     case( 4) ; n = 'P_y         ' !  =  f*sqrt(3.)*v(Y) ! py-orbital
! 
!     case( 5) ; n = 'D_x2-y2     ' !  =  f*sqrt( 3.75)*(v(X)*v(X)-v(Y)*v(Y))    ! d-orbital !  eg
!     case( 6) ; n = 'D_zx        ' !  =  f*sqrt(15.  ) * v(Z)*v(X)              ! d-orbital ! t2g
!     case( 7) ; n = 'D_3z2-r2    ' !  =  f*sqrt( 1.25)*(3.*v(Z)*v(Z)-r2)        ! d-orbital !  eg
!     case( 8) ; n = 'D_yz        ' !  =  f*sqrt(15.  ) * v(Y)*v(Z)              ! d-orbital ! t2g
!     case( 9) ; n = 'D_xy        ' !  =  f*sqrt(15.  ) * v(X)*v(Y)              ! d-orbital ! t2g
! 
!     case(10) ; n = 'F_x(x2-3y2) ' !  =  f*sqrt(  4.375)*v(X)*(v(X)*v(X)-3.*v(Y)*v(Y))  ! f-orbital
!     case(11) ; n = 'F_z(x2-y2)  ' !  =  f*sqrt( 26.25 )*v(Z)*(v(X)*v(X)-v(Y)*v(Y))     ! f-orbital
!     case(12) ; n = 'F_x(5z2-r2) ' !  =  f*sqrt(  2.625)*v(X)*(5.*v(Z)*v(Z)-r2)         ! f-orbital
!     case(13) ; n = 'F_z(5z2-3r2)' !  =  f*sqrt(  1.75 )*v(Z)*(5.*v(Z)*v(Z)-3.*r2)      ! f-orbital
!     case(14) ; n = 'F_y(5z2-r2) ' !  = -f*sqrt(  2.625)*v(Y)*(5.*v(Z)*v(Z)-r2)         ! f-orbital
!     case(15) ; n = 'F_xyz       ' !  = -f*sqrt(105.   )*v(X)* v(Y)*v(Z)                ! f-orbital
!     case(16) ; n = 'F_y(3x2-y2) ' !  = -f*sqrt(  4.375)*v(Y)*(3.*v(X)*v(X)-v(Y)*v(Y))  ! f-orbital
! #ifdef DEBUG
!     case( 17: ) ; write(*,'(3A)') sym, fun, 'ilm > 16 not implemented'
! #endif
!     case default ; n = ' '
!     endselect ! ilm
! 
!   endfunction ! Xlm_rl_name
! 
!   string_t function Xlm_name( ilm ) result( n )
!   ! writes the expressions for spherical harmonics (real combinations) 
!   implicit none
!     ! parameters
!     character(len=*), parameter     :: fun = ' Xlm_name: '
!     ! arguments
!     integer, intent(in)   :: ilm
! 
!     selectcase( ilm )
! #ifdef DEBUG
!     case( :0 ) ; stop 'Xlm_name: ILM should at least be 1'
! #else
!     case( :0 ) ; n = 'ilm<1'
! #endif
!     case( 1) ; n = 's    ' !  =  f ! s-orbital
! 
!     case( 2) ; n = 'px   ' !  =  f*sqrt(3.)*v(X) ! px-orbital
!     case( 3) ; n = 'pz   ' !  =  f*sqrt(3.)*v(Z) ! pz-orbital
!     case( 4) ; n = 'py   ' !  =  f*sqrt(3.)*v(Y) ! py-orbital
! 
!     case( 5) ; n = 'dx2y2' !  =  f*sqrt( 3.75)*(v(X)*v(X)-v(Y)*v(Y))    ! d-orbital !  eg
!     case( 6) ; n = 'dzx  ' !  =  f*sqrt(15.  ) * v(Z)*v(X)              ! d-orbital ! t2g
!     case( 7) ; n = 'd3z2 ' !  =  f*sqrt( 1.25)*(3.*v(Z)*v(Z)-r2)        ! d-orbital !  eg
!     case( 8) ; n = 'dyz  ' !  =  f*sqrt(15.  ) * v(Y)*v(Z)              ! d-orbital ! t2g
!     case( 9) ; n = 'dxy  ' !  =  f*sqrt(15.  ) * v(X)*v(Y)              ! d-orbital ! t2g
!     case( 17: ) ; write(n,'(I4)') ilm
!     case default ; n = 'error'
!     endselect ! ilm
! 
!   endfunction ! Xlm_name
! 
! !- Xlm_names
! #endif

  status_t function test( ) result( ist )
    character(len=*), parameter :: fun = ' test: '
    ist = 0
  endfunction ! test

endmodule ! LaTeX

#ifdef __MAIN__
program test_LaTeX
use LaTeX
call test()
endprogram
#endif

! end of file

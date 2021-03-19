#include "config.h"

! #define DEBUG
! #define FULL_DEBUG

#define cFDB !FDB

#ifdef DEBUG
!! removes comment from debug line
#define cDBG

#ifdef FULL_DEBUG
!! removes comments from full-debug lines
#define cFDB
#endif
#else

#define cDBG !DBG
#endif

!!! suppress the usage of the variable environment
!#define NO_ENV


!! @author Paul Baumeister
!! @version 3.0
!!
!! tools for reading the input file
!! general subroutines for reading the input file,
!! espacially lists of atoms
!! variable environment
module input
cDBG  use configuration, only: o
  use type_item, only: item
implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: fun = ': ', sym = 'INP' !! module symbol

  public :: readblock
  public :: open_input_file
  public :: read3or1
  public :: setenv
  public :: showenv
  public :: eval
  public :: possible_misstypes
  public :: atomicnumber_by_symbol
#ifdef EXTENDED
  public :: test
#endif

  interface read3or1
    module procedure read3or1_r, read3or1_i
  endinterface

  interface setenv ! change to set_env, because getenv is an intrinsic of fortran 2003
    module procedure setenv_r, setenv_i
  endinterface

  ! ios: iostatus convention
  ! == 0 everything is alright
  !  > 0 can be ignored
  !  < 0 Warnings should be launched
  interface eval
    module procedure eval_default, eval_iostatus, eval_variables, eval_expression
  endinterface


  ! variable environment
  character, parameter        :: env_VariableIdentifier = '$'
  character, parameter        :: env_AltVariableIdentifier = '@' ! alternative
  integer, parameter          :: env_MAX_NUM_VARS = 99 !! hard limit, set to 0 to disable variable usage
  logical, save, protected    :: env_used = .false.    !! init as false
  integer, save, protected    :: env_n_known_vars = 0  !! counter
  string_t, save, protected   :: env_var(env_MAX_NUM_VARS) = '' !! init names as empty
  real, save, protected       :: env_val(env_MAX_NUM_VARS) = 0. !! init values as zero


#define intKEY_t integer
  ! keys for evaluation
  intKEY_t, parameter :: K_OPERATOR = -9 !! word matches an implemented operator
  intKEY_t, parameter :: K_FUNCTION = -6 !! word matches an implemented function
  intKEY_t, parameter :: K_UNDEF    = -3 !! word cannot be processed
  intKEY_t, parameter :: K_NOTHING  = -2 !! word is empty
  intKEY_t, parameter :: K_CONST    = -1 !! word matches an implemented constant
  intKEY_t, parameter :: K_VALUE    =  0 !! direct reading as real or fractional number possible

  contains
 
  !! show a list of the defined environment variables
  status_t function showenv( u ) result( iv )
    iounit_t, intent(in) :: u ! output unit number
    iv = 0
    return
#ifdef NO_ENV
    return
#endif
!     write(u,'(1(A,L2),2(A,I3))') 'env: used', env_used, ' unit', dsp, ' nvars', env_n_known_vars
    if( u <= 0 ) return ! dont write to unit 0
    if( env_n_known_vars < 1 ) return
    if( .not. env_used ) return

    write(u,'(/,9A)') ' ========================='
    write(u,  '(9A)') ' = environment variables ='
    write(u,  '(9A)') ' ========================='
    do iv = 1, env_n_known_vars
      write(u, '(3A,F0.9)') ' ', trim(env_var(iv)), ' = ', env_val(iv)
    enddo ! iv
    write(u,  '(9A)',iostat=iv) ! empty line
  endfunction ! showenv


  !! find possible definitions of variables in a line e.g. read from an input file
  status_t function eval_variables( line ) result( ios )
  ! use, only: env_VARLEN, env_MAX_NUM_VARS, env_n_known_vars
  use configuration, only: o
    character(len=*), intent(in) :: line

cDBG  character(len=*), parameter :: fun = ' eval_variables: '
    string_t :: w(-1:3) = '' ! init as ''
    real     :: val

    ios = 0 ; if( line == '' ) return ! empty lines are OK

    ios = -1
#ifdef NO_ENV
    return
#endif
    if( env_MAX_NUM_VARS < 0 ) return ! variables not enabled

cFDB   if(o>0) write(o,'(5A,F10.6,9A)') sym, fun, 'got line ="', trim(line), '"'

    w = '' ! init
    read( unit=line, fmt=*, IOstat=ios ) w ! read 5 words
    ! ignore ios, since line is not empty, at least w(-1) will be not empty
    if( .not. is_var( w(-1) ) ) then
      ios = 1
      return
    endif ! the first word in the line is not a variable name, so
          ! this line does not contain any environment directive

    ! w(-1) is a variable (i.e. it starts from $)
    selectcase( w(0) )
    case( '' ) ! only the variable name is given
      ! ==> output the variable
cDBG    if(o>0) write(o,'(9A)') sym, fun, 'try to eval ="', trim(w(1)), '"'
      val = eval( w(-1), ios ) ! no default value
      if( ios == 0 ) then
        if(o>0) write(o,'(/2A,F10.6)') trim(w(-1)), ' =', val
      else  ! ios
cDBG      if(o>0) write(o,'(9A)') sym, fun, 'could not eval ="', trim(w(1)), '"'
      endif ! ios
      return

    case( '=', ':=', '=>' ) ! set a new variable
cDBG    if(o>0) write(o,'(9A)') sym, fun, 'try to eval expression "', trim(w(1)),' ',trim(w(2)),' ',trim(w(3)), '"'
      val = eval_expression( w(1:3), ios )
      if( ios == 0 ) then
        ios = setenv( name=w(-1), value=val )
        return
      endif ! ios == 0
cDBG    if(o>0) write(o,'(9A)') sym, fun, 'failed on  expression "', trim(w(1)),' ',trim(w(2)),' ',trim(w(3)), '"'
    case default ! w(0)
cDBG    if(o>0) write(o,'(9A)') sym, fun, 'variable context can "$var" or "$var = expr", '
cDBG    if(o>0) write(o,'(9A)') sym, fun, '... where expr should follow the simplified grammar.'
    endselect ! w(0)
  endfunction ! eval_variables



  logical function is_var( str )
  !! true, if the string contains a $ character and a name after $
  !! here REGEX (regular expressions) would be good to check, if there
  !! are characters in front of $
cDBG  use configuration, only: o
    character(len=*), intent(in) :: str !! assume that str is a continuous word, e.i. only tailing white spaces

    integer :: ind
    ind = scan( str, env_VariableIdentifier ) ; if( ind < 1 ) ind = scan( str, env_AltVariableIdentifier )
! cDBG    if(o>0) write(o,'(7A,I0)') sym, ' is_var: ', '"', trim(str), '" index of ',env_VariableIdentifier,' is ',ind
    is_var = ( ind > 0 ) ! contains a $
    is_var = is_var .and. ( str(ind+1:ind+1) /= '' ) ! $ is directly followed by something
!     if( is_var .and. ind > 1 ) is_var = ( str(:ind-1) == '' ) ! there is nothing before $
    is_var = is_var .and. ( str(:ind-1) == '' ) ! there is nothing before $
! cDBG    if(o>0) write(o,'(5A,L1)') sym, ' is_var: ', '"', trim(str), '" => ', is_var
  endfunction ! is_var


  !! evaluate the string taking defined environment variables into account
  real function eval_default( str, def ) result( val )
cDBG  use configuration, only: o
    character(len=*), intent(in) :: str
    real, intent(in)             :: def !! default value

cDBG  character(len=*), parameter :: fun = ' eval_default: '
#ifndef NO_ENV
    intKEY_t :: key

    key = eval_key( str, val )
    selectcase( key )
    case( K_CONST, K_VALUE, 1:env_MAX_NUM_VARS ) ; return
    case default
#endif
      val = def ! set default
#ifdef NO_ENV
cDBG    if(o>0) write(o,'(5A,F10.6,9A)') sym, fun, 'NO_ENV "', trim(str), '", set to DEFAULT =', val
#else
cDBG    if(o>0) write(o,'(5A,F10.6,9A)') sym, fun, 'undefined string "', trim(str), '", set', val, '   DEFAULT'
    endselect ! key
cDBG  if(o>0) write(o,'(5A,F10.6)') sym, fun, '"', trim(str), '" to', val
#endif
  endfunction ! eval


  !! define a value
  status_t function setenv_r( name, value ) result( ios )
  use configuration, only: o
  use configuration, only: WARNING
    character(len=*), intent(in) :: name
    real, intent(in)             :: value

cDBG  character(len=*), parameter :: fun = ' setenv: '
    integer :: iv
#ifndef NO_ENV
    ios = 0
    ! define a new variable
#ifdef CHECK_FOR_OVERWRITE
    ! check if name already exists and warn, otherwise
#endif
    iv = env_n_known_vars+1 ! simply take the next
!! cDBG  if(o>0) write(o,'(5A,I0)') sym, fun, 'define variable "', trim(name), '" @ iv = ',iv
    ! check, if the limit is exceeded
    if( iv > env_MAX_NUM_VARS ) then
      if(o>0) write(o,'(4A,9(I0,A))') sym, fun, WARNING(0), &
        'exceeded hard limit of ',env_MAX_NUM_VARS,' variables when trying to define "', trim(name), '".'
      ios = env_MAX_NUM_VARS+1 ; return
    endif ! iv > MAX_NUM_VARS

    env_n_known_vars = iv ! set up counter
    env_val(iv) = value
    env_var(iv) = name
cDBG  if(o>0) write(o,'(5A,F10.6)') sym, fun, 'define variable "', trim(env_var(iv)), '" =', env_val(iv)
#else
    ios = -1
#endif
  endfunction ! setenv

  status_t function setenv_i( name, value ) result( ios )
  !! wrapper for sentenv_r
  use configuration, only: o
    character(len=*), intent(in) :: name
    integer, intent(in)          :: value

cDBG  character(len=*), parameter :: fun = ' setenv: '
cDBG  if(o>0) write(o,'(5A,I0)') sym, fun, 'define variable "', trim(name), '" = ',value
    ios = setenv_r( name, value=real(value) )
  endfunction ! setenv


  real function eval_iostatus( str, ios, dictionary ) result( val )
  !! if str matches a defined variable or
  !!               an implemented constant or
  !!               can be read as a direct value
  !!   ios=0
  !! else, ios=-1, val=0.
  use type_item, only: item, operator(.in.), min_key
    character(len=*), intent(in)     :: str
    status_t, intent(out)            :: ios
    type(item), intent(in), optional :: dictionary(:)

cDBG  character(len=*), parameter :: fun = ' eval_iostatus: '
    integer :: key
#ifndef NO_ENV
    ios = 0 ! success
    key = eval_key( str, val )
    selectcase( key )
    case( K_CONST, K_VALUE, 1:env_MAX_NUM_VARS ) ! success
    case( K_NOTHING ) ; val = 0.
    case( K_UNDEF   ) ; val = 0. ; ios = -1
    case default      ; val = 0. ; ios = -1
cDBG  if(o>0) write(o,'(5A,9(I0,A))') sym, fun, 'undefined string "', trim(str), '", set ios = ',ios,' key = ',key
    endselect ! key
cDBG  if(o>0) write(o,'(5A,F10.6)') sym, fun, '"', trim(str), '" to', val
#else
    val = 0.
    ios = -1
cDBG    if(o>0) write(o,'(5A,I0)') sym, fun, 'NO_ENV "', trim(str), '", set ios =',ios
#endif
    if( present( dictionary ) ) then
      key = str .in. dictionary
      if( key >= min_key( dictionary ) ) then
        val = real( key )
        ios = 0 ! success
      endif
    endif ! present dictionary
  endfunction ! eval


  !! classify the content of str
  intKEY_t function eval_key( str, val ) result( key )
! use, only: env_MAX_NUM_VARS, env_n_known_vars, env_var, env_val, env_used
  use constants, only: Pi
  use constants, only: EVOLT
  use constants, only: KELVIN
  use constants, only: ANGSTROM
!   use constants, only: OERSTED
    character(len=*), intent(in) :: str
    real, intent(out)            :: val

cDBG  character(len=*), parameter :: fun = ' eval_key: '
    integer  :: iv
    status_t :: ios

    key = K_NOTHING ; if( str == '' ) return ! K_NOTHING

    ! try to read a value
#ifndef NO_FRACTIONAL
    ios = read_frac_number( str, val )
#else
    read( unit=str, fmt=*, IOstat=ios ) val
#endif
    key = K_VALUE ; if( ios == 0 ) return ! K_VALUE

    ! try to find a defined function or operator ! or constant
    selectcase( str )
    !=================================================================================
    case( 'Pi',  'PI',  'pi'  ) ; val=Pi ; key = K_CONST ; return
    case( 'Deg', 'DEG', 'deg' ) ; val=2.*Pi/360. ; key = K_CONST ; return ! Pi = 180 Deg
    case( 'Ang', 'ANG', 'ang' ) ; val=1.0/ANGSTROM ; key = K_CONST ; return ! Angstrom length unit
    case( 'nm' ) ; val=10./ANGSTROM ; key = K_CONST ; return ! Bohr = 0.052917721086(18) nm ==> 1 nm = 18.897261247728981
    case( 'pm' ) ; val=.01/ANGSTROM ; key = K_CONST ; return ! Bohr = 52.917721086(18) nm ==> 1 pm = 0.018897261247728981
    case( 'aB', 'a0', 'Bohr', 'bohr', 'BOHR' ) ; val=1.0 ; key = K_CONST ; return ! Bohr length unit
    case( 'Kel', 'KEL', 'kel' ) ; val=1.0/KELVIN ; key = K_CONST ; return ! Kelvin temperature unit
    case( 'eV', 'EV', 'ev' ) ; val=1.0/EVOLT ; key = K_CONST ; return ! eVolt energy unit
    case( 'Ha', 'HA', 'ha', 'htr', 'Htr', 'HTR' ) ; val=1.0 ; key = K_CONST ; return ! Hartree energy unit
    case( 'Ry', 'RY', 'ry', 'Ryd', 'RYD', 'ryd' ) ; val=0.5 ; key = K_CONST ; return ! Rydberg energy unit
!   case( 'Oe', 'OE', 'oe' ) ; val=1.0/OERSTED ; key = K_CONST ; return !
    case( 'meV' ) ; val=1E-3/EVOLT ; key = K_CONST ; return !
    case( 'percent' ) ; val=0.01 ; key = K_CONST ; return !

    case( 'sqrth' ) ; val=sqrt(.5) ; key = K_CONST ; return ! square root
    case( 'sqrt2' ) ; val=sqrt(2.) ; key = K_CONST ; return ! square root
    case( 'sqrt3' ) ; val=sqrt(3.) ; key = K_CONST ; return ! square root
    case( 'sqrt5' ) ; val=sqrt(5.) ; key = K_CONST ; return ! square root
    case( 'sqrt6' ) ; val=sqrt(6.) ; key = K_CONST ; return ! square root
    case( 'sqrt7' ) ; val=sqrt(7.) ; key = K_CONST ; return ! square root
    case( 'sqrt8' ) ; val=sqrt(8.) ; key = K_CONST ; return ! square root
    !=================================================================================
    case( 'exp', 'cos', 'sin', 'abs', 'sqrt', 'log', 'sign', 'step' ) ; key = K_FUNCTION ; return
    !=================================================================================
    case( '-', '+', '*', ':', '%', '^','**' ) ; key = K_OPERATOR ; return
    !=================================================================================
    endselect ! str

    ! try to find it in the list of variables
    if( is_var( str ) ) then
      ! try to eval via the known variables
      iv = env_n_known_vars ! start with the highest defined varaible
      do while( iv > 0 )
        env_used = .true. ! search in the list of environment variables
! cDBG    if(o>0) write(o,'(9A)') sym, fun, 'compare with defined variable "', trim(env_var(iv)), '"'
        if(  str == env_var(iv) ) then
cDBG    if(o>0) write(o,'(5A,F10.6)') sym, fun, 'found defined variable "', trim(env_var(iv)), '" with value', env_val(iv)
          ! match a defined variable
          val = env_val(iv)
          key = iv ! return the index of the define variable
          return
        endif ! var
        iv = iv-1 ! count negative, so variable names can be reused
      enddo ! ivr
      ! the variable is not defined
cDBG    if(o>0) write(o,'(9A)') sym, fun, 'undefined variable "', trim(str), '"'
    else  ! str is a valid variable string with leading $
      ! string contains no "$"
cDBG    if(o>0) write(o,'(9A)') sym, fun, 'undefined string "', trim(str), '"'
    endif ! str is a valid variable string with leading $

    key = K_UNDEF ! undefined
    val = 0.
  endfunction ! eval_key


  !! classifies the keys into "V":value, "F":function and "O":operator as a character
  character function what( key ) result( c )
    integer, intent(in) :: key

    selectcase( key )
    case( K_UNDEF ) ; c = '?' ! UNDEF, cannot be processed
    case( K_NOTHING ) ; c = ' ' ! NOTHING
    case( K_OPERATOR ) ; c = 'O' ! OPERATOR
    case( K_FUNCTION ) ; c = 'F' ! FUNCTION
    case( K_CONST, K_VALUE ) ; c = 'V' ! VALUE implemented constant or direct readable value
    case( 1:env_MAX_NUM_VARS ) ; c = 'V' ! VALUE from table of defined vars
    case( env_MAX_NUM_VARS+1: ) ; c = '?' ! UNDEF index exceeds table bounds, cannot be processed
    case default ; c = 'E' ! ERROR, not implemented
cDBG  if(o>0) write(o,'(3A,I0)') sym, ' what: ', 'key = ',key
cDBG  stop 'INP what: ERROR: undefined key'
    endselect ! key
  endfunction ! what


  !! eval expressions up to 3 words, no parenthesis !!
  real function eval_expression( s, ios ) result( value )
    character(len=*), intent(in)    :: s(:)
    status_t, intent(out)           :: ios

cDBG  character(len=*), parameter :: fun = ' eval_expression: '
    integer          :: i
    intKEY_t         :: key(3)
    character(len=3) :: c3
    real             :: v(3)

    if( size(s) > 3 ) stop 'INP eval_expression: implemented maximum of 3 words in an expression.'

  !==================
  ! --> ?
  ! --> exp ?
  ! --> cos ?
  ! --> sin ?
  ! --> abs ?
  ! --> ? + ?
  ! --> ? - ?
  ! --> ? * ?
  ! --> ? ** ?
  ! --> ? / ? !! input not possible, / is interpreted as line break

    ! init
    value = 0.
    key = K_NOTHING
    c3 = '   '
    v = 0.

    do i = 1, min(3,size(s))
      key(i)  = eval_key( s(i), v(i) )
      c3(i:i) = what( key(i) )
    enddo ! i
cDBG if(o>0) write(o,'(3A,3I3,99A)') sym, fun, 'keys found ', key, ' what = [',c3,'] inp =', ( ', "', trim(s(i)), '"', i=1,size(s) )

    ios = 0 ! SUCCESS

    selectcase( c3 )
    case( 'V  ' )

      value = v(1) ! directly evaluated

    case( 'FV ' )

      selectcase( s(1) ) ! function
      case( 'exp' ) ;    value = exp( v(2) )
      case( 'sin' ) ;    value = sin( v(2) )
      case( 'cos' ) ;    value = cos( v(2) )
      case( 'abs' ) ;    value = abs( v(2) )
      case( 'sign' ) ;   value = sign( 1.0, v(2) )
      case( 'step' ) ;   value = 0. ; if( v(2) > 0. ) value = 1. ! Heaviside step function
      case( 'sqrt' ) ;   if( v(2) < 0. ) stop 'INP math error: sqrt of negative'
                         value = sqrt( v(2) )
      case( 'log' )  ;   if( v(2) <= 0. ) stop 'INP math error: log only of positive'
                         value = log( v(2) )
      case( 'atan' ) ;   value = atan( v(2) )
      case default   ;   ios = -6 ! failed
cDBG    if(o>0) write(o,'(9A)') sym, fun, 'undefined function "',trim(s(1)),'".'
      endselect ! s(1)
cDBG    if(o>0) write(o,'(5A,F0.6,9A)') sym, fun, 'defined function ',trim(s(1)),'(', v(2), ' )'

    case( 'VFV' )

      selectcase( s(2) ) ! function
      case( 'exp' ) ;      value = v(1) * exp( v(3) )
      case( 'sin' ) ;      value = v(1) * sin( v(3) )
      case( 'cos' ) ;      value = v(1) * cos( v(3) )
      case( 'abs' ) ;      value = v(1) * abs( v(3) )
      case( 'sqrt' ) ;  if( v(3) < 0. ) stop 'INP math error: sqrt of negative'
                           value = v(1) * sqrt( v(3) )
      case( 'log' ) ;   if( v(3) <= 0. ) stop 'INP math error: log only of positive'
                           value = v(1) * log( v(3) )
      case( 'atan' ) ;     value = v(1) * atan( v(3) )
      case default ; ios = -6 ! failed
cDBG    if(o>0) write(o,'(9A)') sym, fun, 'undefined function "',trim(s(2)),'".'
      endselect ! s(2)
cDBG    if(o>0) write(o,'(5A,F0.6,9A)') sym, fun, 'defined function ',trim(s(2)),'(', v(3), ' )'

    case( 'VV ' ) ; value = v(1) * v(2) ! product of 2
cDBG    if(o>0) write(o,'(9A)') sym, fun, 'product of ',trim(s(1)),' and ',trim(s(2))
    case( 'VVV' ) ; value = v(1) * v(2) * v(3) ! product of 3
cDBG    if(o>0) write(o,'(9A)') sym, fun, 'product of ',trim(s(1)),', ',trim(s(2)),' and ',trim(s(3))
    case( 'VOV' )

      selectcase( s(2) ) ! operator
      case( '-' ) ;      value = v(1)  - v(3)
      case( '+' ) ;      value = v(1)  + v(3)
      case( '*' ) ;      value = v(1)  * v(3)
      case( ':' ) ; if( v(3) == 0. ) stop 'INP math error: division by zero.'
                         value = v(1)  / v(3)
      case( '%' ) ; if( v(3) == 0. ) stop 'INP math error: modulo with zero.'
                         value = modulo( v(1), v(3) )
      case( '^','**' ) ; value = v(1) ** v(3)
        if( v(1) == 0. .and. v(3) < 0. ) stop 'INP math error: negative power of zero.'
        if( aimag( cmplx(v(1),0.) ** v(3) ) /= 0. ) stop 'INP math error: complex result.'
      case default ; ios = -9
cDBG    if(o>0) write(o,'(9A)') sym, fun, 'undefined operator "',trim(s(2)),'".'
      endselect ! s(2)
cDBG  if(o>0) write(o,'(5A,9(F0.6,2A))') sym, fun, 'defined operator (',trim(s(2)),')  in ', v(1), ' ', trim(s(2)), v(3)
    case default ; ios = -3
cDBG    if(o>0) write(o,'(9A)') sym, fun, 'cannot evaluate expression "',trim(s(1)),' ',trim(s(2)),' ',trim(s(3)),'"!'
cDBG    if(o>0) write(o,'(9A)') sym, fun, 'no case "', c3, '" implemented.'
cDBG    if(o>0) write(o,'(9A)') sym, fun, 'variable context should be "$var" or "$var = expr",'
cDBG    if(o>0) write(o,'(9A)') sym, fun, ' ... where expr can be "V", "VV", "FV", "VFV", "VVV" or "VOV"'
cDBG    if(o>0) write(o,'(9A)') sym, fun, ' ... with "V"=value out of {Pi,Ang,nm} or a defined $variable'
cDBG    if(o>0) write(o,'(9A)') sym, fun, ' ...      "F"=function out of {sin,cos,exp,abs,log,sqrt}'
cDBG    if(o>0) write(o,'(9A)') sym, fun, ' ...      "O"=operator out of {+,-,*,:,^}'
    endselect ! c3

    if( value /= value ) stop 'INP eval_expression: NaN occured.' ! check for NaN
  endfunction ! eval_expression


  !! read numbers formated as a:b where a and b are integer
  status_t function read_frac_number( str, r ) result( ios )
  use configuration, only: STOPonERROR
    character(len=*), intent(in)    :: str
    real, intent(out)               :: r !! the value of the fractional number, if ios==0

cDBG  character(len=*), parameter :: fun = ' read_frac_number: '
    character, parameter :: FRAC = ':'
    integer :: inom, iden, ip, ios_den, ios_nom

    r = 0. ! init as default

    ip = scan( str, FRAC ) ! scan the string to see if it contains a ':'
    if( ip > 0 ) then ! string contains ':'
      ! try to read integers to the left and to the right of ':'
cFDB    if(o>0) write(o,'(7A,I0,A)') sym, fun, 'string "', trim(str), '" has a "',FRAC,'" at pos ',ip
      read(unit=str(1:ip-1),fmt=*,iostat=ios_nom) inom
      read(unit=str(ip+1:) ,fmt=*,iostat=ios_den) iden
cFDB    if(o>0) write(o,'(9A)') sym, fun, &
cFDB      'string"', trim(str), '" has denominator part "', trim(str(ip+1:)), '".'
      if( ios_nom == 0 .and. ios_den == 0 ) then

        if( iden /= 0 ) then
          ios = 0 ! result OK
          r = real( inom )/real( iden )
        else  ! iden /= 0
cFDB      if(o>0) write(o,'(9A)') sym, fun, 'string"', trim(str), '" has 0 as denominator.'
cFDB      if( STOPonERROR ) stop 'INP math: denominator of integer fraction is 0'
cFDB      ios = -6 ! not OK
        endif ! iden /= 0
      else  ! ios
cFDB    if(o>0) write(o,'(9A)') sym, fun, 'string"', trim(str), '" contains "',FRAC,'" but reading integers failed.'
        ios = -1 ! not OK
      endif ! ios == 0 and denominator /= 0
    else  ! string does not contain ':'
cFDB  if(o>0) write(o,'(9A)') sym, fun, 'string "', trim(str), '" has no "',FRAC,'" ==> regular reading.'
      ! regular reading
      read(unit=str,fmt=*,iostat=ios) r
    endif ! string contains ':'

  endfunction ! read_frac_number



  status_t function open_input_file( inunit, infile, MPIcommunicator ) result( ios )
  !! simple function to open a file and explain why it failed, if so
  use configuration, only: o
  use MPIconst, only: MPI_COMM_SELF
  use MPItools, only: MPImaster, MPIbcast0
    iounit_t, intent(in)            :: inunit
    character(len=*), intent(in)    :: infile
    MPI_Comm, intent(in), optional  :: MPIcommunicator

cDBG  character(len=*), parameter :: fun = ' open_input_file: '
    MPI_Comm :: comm

    ! if(o>0) write(o,'(9A)') sym, fun, 'received "', trim(infile), '".'
    ios = 1 ! init as error

    if( infile == '' ) then
      if(o>0) write(o,'(9A)') sym, fun, 'received empty string for the input file. return!'
      return ! ERROR
    endif ! inputfile == ''

    comm = MPI_COMM_SELF ; if( present( MPIcommunicator ) ) comm = MPIcommunicator
    if( MPImaster( comm ) ) then

      ! open existing(status='old') file called infile for reading(action='read')
      open( unit=inunit, file=infile, action='read', status='old', iostat=ios )

    endif ! master
    call MPIbcast0( ios, comm ) ! tell the other processes

    if( ios /= 0 .and. o>0) write(o,'(5A,I0,9A)') sym, fun, 'cannot find input file "', trim(infile), '", IOstatus = ',ios,', return!'
  endfunction ! open_input_file


  integer function readblock( unt, keyword, array, MPIcomm ) result( n )
  !! read blocks of formatted input lines from unit unt.
  !! a block is wrapped by lines that start with keyword
  !!
  !! new feature: if the keyword is followed by a filename of an
  !! existing file, the file is openend and block entries are
  !! read from that file 
  use configuration, only: o
  use configuration, only: WARNING, ERROR
  use configuration, only: CommentChars ! usually '#'
  use configuration, only: STOPonERROR
  use configuration, only: MaxInputFileLineLen
  use configuration, only: MaxKeyWordLen
  use configuration, only: MaxInputFileNameLen
  use configuration, only: BlockKeyWord_Atoms, BlockKeyWord_Afrac, BlockKeyWord_Kpnts, BlockKeyWord_Kpath
  use MPIconst, only: MPI_COMM_SELF
  use MPItools, only: MPImaster, MPIparallel, MPIbcast0
    integer, intent(in)                   :: unt         !! original input file unit
    character(len=*), intent(in)          :: keyword
    real, intent(out), allocatable        :: array(:,:)
    integer, intent(in), optional         :: MPIcomm

cDBG  character(len=*), parameter :: fun = ' readblock: '
    integer, parameter :: NCW_CHECK = 0 ! key for the 1st run
    integer, parameter :: NCW_WRITE = 1 ! key for the 2nd run
    integer, parameter :: uFILE = 19 ! external file unit
    integer, parameter :: KEY_ATOMS   = 0
    integer, parameter :: KEY_ATOMS_F = 1
    integer, parameter :: KEY_KPOINTS = 2
    integer, parameter :: KEY_KPATH   = 3

    integer                               :: ninvalid, itmp, i3, ist, ncw, u, iwhat, nd
    integer                               :: rios, lios, xfios, rdios, ios, wios(0:3)
    real                                  :: tmp(0:9)
    character(len=3)                      :: ctmp
    character(len=MaxKeyWordLen)          :: key
    character(len=MaxInputFileLineLen)    :: line, word(0:3), wMag, hub, hub_p
    character(len=MaxInputFileNameLen)    :: xfile !! external file name
    logical                               :: readmode, move(3)
    integer                               :: comm

    selectcase( keyword )
    case( BlockKeyWord_Atoms ) ; iwhat = KEY_ATOMS    ; nd = 9
    case( BlockKeyWord_Afrac ) ; iwhat = KEY_ATOMS_F  ; nd = 9
    case( BlockKeyWord_Kpnts ) ; iwhat = KEY_KPOINTS  ; nd = 3
    case( BlockKeyWord_Kpath ) ; iwhat = KEY_KPATH    ; nd = 3
    case default
      if(o>0) write(o,'(9A)') sym, fun, ERROR, 'unknown keyword "', trim(keyword), '"!'
      stop 'readblock: keyword not implemented.'
    endselect ! what

    comm = MPI_COMM_SELF ; if( present( MPIcomm ) ) comm = MPIcomm

cFDB  write(*,'(5A,I0,A,Z8.8)') sym, fun, ' keyword "', trim(keyword), '" nd=', nd, ' comm=0x',comm


    if( .not. MPImaster( comm ) ) then
      ! MPImembers other than the master of comm will execute these 3 operations
      call MPIbcast0( n, comm ) ! receive the number of valid entries
cFDB  write(*,'(5A,9(I0,A))') sym, fun, ' keyword "', trim(keyword), '" ', nd,'x',n
      if( n < 1 ) return
      allocate( array(0:nd,n), stat=ist ) ! allocate the array
      if( ist /= 0 ) stop 'INP readblock: allocation of ARRAY failed! (MPImember)'
      call MPIbcast0( array, comm ) ! receive the data
      return ! and then return early
    endif ! .not. MPImaster

    n = 0 ! init the result
    ! make sure that array is not allocated
    if( allocated(array) ) deallocate( array, stat=ist )

    if( unt == uFILE ) stop 'readblock: UNT may not match uFILE!'
    u = unt ! set to original input file unit

    do ncw = NCW_CHECK, NCW_WRITE, NCW_WRITE-NCW_CHECK ! 2 times

      rewind( unit=unt, iostat=ios ) ! try to rewind the file for reading
cDBG  if(ios /= 0 .and. o>0) write(o,'(4A,9(I0,A))') sym, fun, WARNING(0), 'unable to rewind unit number ',unt

      n = 0 ! init
      ninvalid = 0 ! init
      readmode = .false. ! off
      xfile = '' ! init

        line = '' ; read( unit=u, fmt='(A)', iostat=rios ) line ! read the 1st line
cFDB    if(rios/=0 .and. o/=0) write(o,'(3A,I0)') sym, fun, 'ios = ',rios
! cFDB  if(o>0) write(o,'(9A)') sym, fun, 'line = "', trim(line), '".'
      do while( rios == 0 )
        ! read the first word in string line
        read( unit=line, fmt=*, iostat=rdios ) key
        if( rdios == 0 ) then
          if( all( key(1:1) /= CommentChars ) ) then
            if( key == keyword ) then

              read( unit=line, fmt=*, iostat=rdios ) key, xfile ! try to read a filename
              if( rdios == 0 ) then

                open( unit=uFILE, file=xfile, status='old', iostat=xfios, action='read' )
                if( xfios == 0 ) then
                  if(o>0 .and. ncw == NCW_CHECK ) write(o,'(9A)') sym, fun, 'open external file "', trim(xfile), '".'
                  u = uFILE ! set unit to external file unit
                  readmode = .true.
                else  ! xfios
                  if(o>0 .and. ncw == NCW_CHECK ) write(o,'(9A)') sym, fun, WARNING(0), &
                    'keyword "', trim(keyword), '" is followed by "', trim(xfile), '" which is not an external file name.'
                  readmode = .not. readmode ! 0 -> 1 , 1 -> 0
cFDB              if(o>0) write(o,'(3A,L2)') sym, fun, 'switch readmode to', readmode
                endif ! xfios

              else  ! keyword is followed by something
                readmode = .not. readmode ! 0 -> 1 , 1 -> 0
cFDB            if(o>0) write(o,'(3A,L2)') sym, fun, 'switch readmode to', readmode
              endif ! keyword is followed by something

            else ! key == keyword
              word = '' ! init
              tmp = 0. ! init
              move = .true.
              if( readmode ) then

                selectcase( iwhat )
                case( KEY_ATOMS, KEY_ATOMS_F )
                  ! gives atomic numbers or symbols followed by coordinates
                  ! try to read the format <Sym_or_Z> <x> <y> <z> mx my mz iMag
                  read( unit=line, fmt=*, iostat=lios ) ctmp, word(1:3), move(1:3), wMag, hub, hub_p
                  ! try to read at least the components <Sym_or_Z> <x> <y> <z>
                  read( unit=line, fmt=*, iostat=lios ) ctmp, word(1:3)
#ifdef NO_ENV
                  do i3 = 1, 3 ! read numbers directly or fractionals "a:b" with a,b integer
                    wios(i3) = read_frac_number( word(i3), tmp(i3) )
                  enddo ! i3
                  if( any( wios(1:3) /= 0 ) ) lios = 1 ! global ios
#else
                  do i3 = 1, 3 ! evaluate with help of the variable-environment
                    wios(i3) = eval_key( word(i3), tmp(i3) )
                  enddo ! i3
                  if( any( wios(1:3) < K_VALUE ) ) lios = 1 ! global ios
#endif
                  itmp = atomicnumber_by_symbol( ctmp ) ! find the atomic number
                  tmp(0) = real(itmp) ! pass the atomic number
                  if( itmp < -1 .or. itmp > 121 ) lios = 1 ! only [-1:121] valid

                  where( move ) tmp(4:6) = 1. ! set the displacement multiplyer (fix)

                  ! set tmp(7) to default value 1.0 if wMag cannot be evaluated
#ifdef NO_ENV
                  if( read_frac_number( wMag,  tmp(7) ) /= 0 ) tmp(7) = 0.
                  if( read_frac_number( hub,   tmp(8) ) /= 0 ) tmp(8) = 0.
                  if( read_frac_number( hub_p, tmp(9) ) /= 0 ) tmp(9) = 0.
#else
                  if( eval_key( wMag,  tmp(7) ) < K_VALUE )    tmp(7) = 0.
                  if( eval_key( hub,   tmp(8) ) < K_VALUE )    tmp(8) = 0.
                  if( eval_key( hub_p, tmp(9) ) < K_VALUE )    tmp(9) = 0.
#endif

                case( KEY_KPOINTS ) ! read kpoints in the format  <x> <y> <z> <w8>
                  read( unit=line, fmt=*, iostat=lios ) word(1:3), word(0)
                  do i3 = 0, 3
                    wios(i3) = read_frac_number( word(i3), tmp(i3) )
                  enddo ! i3
                  if( any( wios(0:3) /= 0 ) ) lios = 1

                case( KEY_KPATH ) ! read edges of the k-path <x> <y> <z>, no weight required
                  read( unit=line, fmt=*, iostat=lios ) word(1:3)
                  do i3 = 1, 3
                    wios(i3) = read_frac_number( word(i3), tmp(i3) )
                  enddo ! i3
                  if( any( wios(1:3) /= 0 ) ) lios = 1

cDBG            case default ; stop 'readblock: key iWHAT not implemented.'
                endselect ! iwhat

                if( lios == 0 ) then

                  n = n + 1
                  if( ncw == NCW_WRITE ) array(0:nd,n) = tmp(0:nd) ! write to the array
cFDB              if( ncw == NCW_CHECK .and. o>0 ) write(o, '(3A,I6,9ES8.1E1)' ) sym, fun, 'CHECK values =', itmp, tmp(0:nd)
cFDB              if( ncw == NCW_WRITE .and. o>0 ) write(o, '(3A,I6,9ES8.1E1)' ) sym, fun, 'WRITE values =', itmp, tmp(0:nd)

                else  ! lios == 0
                  ninvalid = ninvalid+1 ! count, warn if count > 1 at the end
cDBG              if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'line "', trim(line), '" has invalid content'
                endif ! lios == 0

              else  ! readmode is off, ignore lines
! cFDB          if(o>0) write(o,'(9A)') sym, fun, 'readmode = OFF'
              endif ! readmode

            endif ! key == keyword
          endif ! key(1:1) /= CommentChars

        endif ! rdios == 0

        line = '' ; read( unit=u, fmt='(A)', iostat=rios ) line ! read the nxt line
cFDB    if(rios/=0 .and. o>0) write(o,'(3A,I0)') sym, fun, 'ios = ',rios
! cFDB  if(o>0) write(o,'(9A)') sym, fun, 'line = "', trim(line), '".'

        if( rios /= 0 .and. u == uFILE ) then
          ! get out of the external file and 
          ! continue reading the original input file
          if(o>0 .and. ncw == NCW_CHECK ) write(o,'(9A)') sym, fun, 'leave external file "', trim(xfile), '".'
          close( unit=u, iostat=xfios )
          u = unt ! set unit back to unit original input file
          readmode = .false. ! the end of an external file switches the readmode OFF

          ! read the nxt line from the original input file
          line = '' ; read( unit=u, fmt='(A)', iostat=rios ) line ! read the nxt line

cFDB      if(rios/=0 .and. o>0) write(o,'(3A,I0)') sym, fun, 'ios = ',rios
! cFDB    if(o>0) write(o,'(9A)') sym, fun, 'line = "', trim(line), '".'
        endif ! rios

      enddo ! while( rios == 0 )



      selectcase( ncw )
      case( NCW_CHECK )

        if( readmode ) then ! readmode is still 'ON'
          if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'input for "', trim(keyword), '" ended before block was completed.'
          readmode = .false.
        endif ! readmode

        if( ninvalid > 0 ) then
          if(o>0) write(o,'(4A,I0,9A)') sym, fun, WARNING(0), 'detected ',ninvalid,' invalid input lines for "', trim(keyword), '".'
        endif ! ninvalid > 0

        if( n < 1 ) then
cDBG      if(o>0) write(o,'(9A)') sym, fun, 'no elements found for "', trim(keyword),'".'
          n = 0
          call MPIbcast0( n, comm ) ! master tells the number of found valid entries
          return ! early return
        else  ! n < 1
cFDB      if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'allocate(0:',nd,',',n,')'
          call MPIbcast0( n, comm ) ! master tells the number of found valid entries
          allocate( array(0:nd,n), stat=ist ) ! allocate the array
          if( ist /= 0 ) stop 'INP readblock: allocation of ARRAY failed! (MPImaster)'
        endif ! n < 1

      case( NCW_WRITE ) ! ncw
        if( n /= size(array,2) ) then
          if(o>0) write(o,'(4A,9(I0,A))') sym, fun, ERROR, '1st and 2nd time reading produced different results: ',size(array,2),' and ',n,' (2nd)'
          if( STOPonERROR ) stop 'readblock: different results reading the same.'
        endif ! howmany /= readblock
      case default ; stop 'readblock: fatal: NCW out of {0,1,3}'
      endselect ! ncw

    enddo ! ncw = check, write

    call MPIbcast0( array, comm ) ! master broadcasts the data

  endfunction ! readblock


  status_t function read3or1_r( line, v3, silent ) result( ios )
  !!>
  !!  these functions replace this constructions:
  !!
  !!             read( unit=line, fmt=*, iostat=ios ) k_word, cellsize(1:3)
  !!             if( ios /= 0 ) then
  !!               ! if only one number is given, use it for all 3 directions
  !!               read( unit=line, fmt=*, iostat=ios ) k_word, cellsize(1)
  !!               if( ios == 0 ) then
  !!                 cellsize(2:3) = cellsize(1)
  !!                 if(o>0) write(o,'(4A,F10.3,9A)') sym, fun, WARNING(0), &
  !!                   'use', cellsize(1), ' as cell size for all 3 dimensions.'
  !!               endif ! ios == 0
  !!             endif ! ios /= 0
  !!<
  use configuration, only: o
  use configuration, only: MaxKeyWordLen
    character(len=*), intent(in)      :: line    !! line of the input file
    real, intent(out)                 :: v3(1:3) !! values
    logical, intent(in), optional     :: silent

cDBG  character(len=*), parameter     :: fun = ' read3or1: '
    character(len=*), parameter       :: WARN = 'Caution! '
#ifndef NO_ENV
    string_t                          :: w3(1:3) ! words, *important* is the initialization
    integer                           :: i
    character(len=3)                  :: c3 ! classification
#endif
    string_t                          :: kw
    logical                           :: wrn

    v3 = 0. ; kw = '' ; wrn = .false. ! init
#ifdef NO_ENV
    ! read a keyword and 3 numbers
    read( unit=line, fmt=*, IOstat=ios ) kw, v3(1:3)
    if( ios /= 0 ) then
      read( unit=line, fmt=*, IOstat=ios ) kw, v3(1)
      if( ios == 0 ) then ! reading 1 was succesful
        v3(2:3) = v3(1) ! but warn
        wrn = .true.
      endif ! ios == 0
    endif ! ios /= 0
#else
    w3 = '' ! init
    ! read a keyword and 3 strings
    read( unit=line, fmt=*, IOstat=ios ) kw, w3(1:3)
    ! evaluate the strings via the environment variables
    do i = 1, 3
      c3(i:i) = what( eval_key( w3(i), v3(i) ) ) ! real
    enddo ! i

    selectcase( c3 )
    case( 'VVV' ) ; ios = 0 ! 3 values are OK
    case( 'VV ' ) ; ios = 0 ! interpret 2nd value as a factor
      v3(3) = v3(1)*v3(2) ! multiply
      v3(1:2) = v3(3) ! but warn
      wrn = .true.
    case( 'V  ' ) ; ios = 0 ! use 1st value for all
      v3(2:3) = v3(1) ! but warn
      wrn = .true.
    case default ; ios = -1 ! failed
    endselect
#endif

    if( present( silent ) ) wrn = wrn .and. ( .not. silent )
#ifdef NO_ENV
    if( wrn .and. o>0 ) then
      if( nint(v3(1))==v3(1) ) then
        write(o,'(4A,I0,9A)') sym, fun, WARN, 'use ',nint(v3(1)),' as ',trim(kw),' for all 3 dimensions.'
      else ! value == int
        write(o,'(4A,F0.3,9A)') sym, fun, WARN, 'use ',v3(1),' as ',trim(kw),' for all 3 dimensions.'
      endif ! value == int
    endif ! warn o>0
#else
    if( wrn .and. o>0) write(o,'(9A)') sym, fun, WARN, 'use ',trim(w3(1)),' as ',trim(kw),' for all 3 dimensions.'
#endif
  endfunction ! read3or1

  status_t function read3or1_i( line, v3, dictionary, silent ) result( ios )
  !! wrapper to read3or1_r
  use configuration, only: o
  use configuration, only: MaxKeyWordLen
  use type_item, only: item, min_key, operator(.in.)
    character(len=*), intent(in)      :: line    !! line of the input file
    integer, intent(out)              :: v3(1:3) !! values
    type(item), intent(in), optional  :: dictionary(:)
    logical, intent(in), optional     :: silent

cDBG  character(len=*), parameter       :: fun = ' read3or1: '
    character(len=*), parameter       :: WARN = 'Caution! '
    string_t                          :: kw, wrd(1:3)
    real                              :: v3r(1:3)
    integer                           :: ii
    if( present( dictionary ) ) then
      wrd = '' ! init
      read(unit=line,fmt=*,iostat=ios) kw, wrd(1:3)
      if( ios /= 0 ) then
        v3(1) = wrd(1) .in. dictionary
        if(o>0) write(o,'(9A)') sym, fun, WARN, 'use ',trim(wrd(1)),' as ',trim(kw),' for all 3 dimensions.'
        v3 = v3(1)
        ios = min( 0, v3(1)-min_key( dictionary ) )
      else  ! ios
        do ii = 1, 3
          v3(ii) = wrd(ii) .in. dictionary
        enddo ! ii
        ios = min( 0, minval(v3)-min_key( dictionary ) )
      endif ! ios
    else  ! dictionary
      ios = read3or1_r( line, v3r, silent )
      v3 = nint(v3r)
    endif ! dictionary
  endfunction ! read3or1



  character(len=2) function element_symbol( iZ )
  !! returns the element_symbol from PSE with buffered input
  use constants, only: PSE
  use configuration, only: o
    integer, intent(in)             :: iZ ! atomic number

cDBG  character(len=*), parameter     :: fun = ' element_symbol: '
    selectcase( iZ )
    case( :lbound(PSE,1)-1 ) ; element_symbol = '<0'
      if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'atomic number < ',lbound(PSE,1),', no such element with Z = ', iZ
    case( ubound(PSE,1)+1: ) ; element_symbol = '>M'
      if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'atomic number > ',ubound(PSE,1),', symbols not implemented for Z = ', iZ
    case default             ; element_symbol = PSE(iZ)
    endselect ! iZ
  endfunction ! element_symbol

  integer function atomicnumber_by_symbol( symbol_or_number ) result( iZ )
  !! retrieves the atomic number from the element symbol
  !! regular results: iZ in [-1,121]
  !! errors:          iZ == -7 ! empty string
  !! errors:          iZ == -5 ! not found
  use constants, only: PSE
  use configuration, only: o
    character(len=3), intent(in) :: symbol_or_number

cDBG  character(len=*), parameter :: fun = ' atomicnumber_by_symbol: '
    character(len=3) :: s
    integer :: ios

    iZ = -6 ! not found
    s = adjustl(symbol_or_number)
    if( s  == '   ' ) then
cFDB  if(o>0) write(o,'(9A)') sym, fun, 'received empty string.'
      iZ = -7 ; return
    endif ! s == ''

    read( unit=s, fmt=*, iostat=ios ) iZ
    if( ios == 0 ) then
      if( iZ >= lbound(PSE,1) .and. iZ <= ubound(PSE,1) ) then
cFDB    if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'found element Z = ',iZ,' in string "', trim(s), '".'
        return
      endif ! iZ >= 0 .and. iZ <= ubound(pse)
    endif ! ios == 0

    iZ = lbound(PSE,1)
    do while( iZ <= ubound(PSE,1) )
      if( s(1:2) == PSE( iZ ) ) then
cFDB    if(o>0) write(o,'(5A,I0,9A)') sym, fun, "found element '", trim(s(1:2)), "' at Z = ",iZ
        if( iZ == 0 .and. o>0) write(o,'(3A)') sym, fun, 'Warning: Vacuum atom with Z=  0 detected.'
        if( s(3:3) /= ' ' ) iZ = -8 ! third place is not a space
        return
      endif ! s == PSE( Z )
      iZ = iZ+1 ! check next
    enddo ! iZ <= 121

cDBG  if(o>0) write(o,'(9A)') sym, fun, "symbol '", trim(s), "' not found in PSE."
    iZ = -5 ! not found
  endfunction ! atomicnumber_by_symbol


  integer function atomic_number_by_symbol( Sy ) result( Z )
  !! retrieves the atomic number from an element symbol of length 3
  !! valid input are all chemical symbols of the periodic table
  !! in correct case, i.e. first letter capitalized
  !!  second letter (if any) lower case
  !! also accepts (integer) numbers in the range [0:121]
  !! errors: Z=-6
  use configuration, only: o
    character(len=3), intent(in)    :: Sy

cDBG  character(len=*), parameter     :: fun = ' atomic_number_by_symbol: '
    integer, parameter              :: Z_ERROR = -6
    integer                         :: ios
    character                       :: S, y ! 1st and 2nd character of Sy

cDBG  if(o>0) write(o,'(9A)') sym, fun, 'search for symbol "', trim(Sy), '" in PSE.'
   
    read( unit=Sy, fmt=*, IOstat=ios ) Z ! try integer reading
    if( ios == 0 ) then
      ! an integer number could be read
      if( Z < -1 .or. Z > 121 ) Z = Z_ERROR
      return
    endif ! ios == 0

    Z = Z_ERROR

    if( Sy(3:3) /= ' ' ) return ! error

    S = Sy(1:1) ! abbrev.
    y = Sy(2:2) ! abbrev.

    selectcase( S )
    !-------------------------------
    case('A')
      selectcase( y )
      case('c') ; Z = 89 ! Ac
      case('g') ; Z = 47 ! Ag
      case('l') ; Z = 13 ! Al
      case('m') ; Z = 95 ! Am
      case('r') ; Z = 18 ! Ar
      case('s') ; Z = 33 ! As
      case('t') ; Z = 85 ! At
      case('u') ; Z = 79 ! Au
      endselect ! y
    case('B')
      selectcase( y )
      case(' ') ; Z =  5 ! B
      case('a') ; Z = 56 ! Ba
      case('e') ; Z =  4 ! Be
      case('h') ; Z =107 ! Bh
      case('i') ; Z = 83 ! Bi
      case('k') ; Z = 97 ! Bk
      case('r') ; Z = 35 ! Br
      endselect ! y
    case('C')
      selectcase( y )
      case(' ') ; Z =  6 ! C
      case('a') ; Z = 20 ! Ca
      case('d') ; Z = 48 ! Cd
      case('e') ; Z = 58 ! Ce
      case('f') ; Z = 98 ! Cf
      case('l') ; Z = 17 ! Cl
      case('m') ; Z = 96 ! Cm
      case('n') ; Z =112 ! Cn
      case('o') ; Z = 27 ! Co
      case('r') ; Z = 24 ! Cr
      case('s') ; Z = 55 ! Cs
      case('u') ; Z = 29 ! Cu
      endselect ! y
    case('D')
      selectcase( y )
      case('b') ; Z =105 ! Db
      case('s') ; Z =110 ! Ds
      case('y') ; Z = 66 ! Dy
      endselect ! y
    case('E')
      selectcase( y )
      case('r') ; Z = 68 ! Er
      case('s') ; Z = 99 ! Es
      case('u') ; Z = 63 ! Eu
      endselect ! y
    case('F')
      selectcase( y )
      case(' ') ; Z =  9 ! F
      case('e') ; Z = 26 ! Fe
      case('m') ; Z =100 ! Fm
      case('r') ; Z = 87 ! Fr
      endselect ! y
    case('G')
      selectcase( y )
      case('a') ; Z = 31 ! Ga
      case('d') ; Z = 64 ! Gd
      case('e') ; Z = 32 ! Ge
      endselect ! y
    case('H')
      selectcase( y )
      case(' ') ; Z =  1 ! H
      case('e') ; Z =  2 ! He
      case('f') ; Z = 72 ! Hf
      case('g') ; Z = 80 ! Hg
      case('o') ; Z = 67 ! Ho
      case('s') ; Z =108 ! Hs
      endselect ! y
    case('I')
      selectcase( y )
      case(' ') ; Z = 53 ! I
      case('n') ; Z = 49 ! In
      case('r') ; Z = 77 ! Ir
      endselect ! y
    case('K')
      selectcase( y )
      case(' ') ; Z = 19 ! K
      case('r') ; Z = 36 ! Kr
      endselect ! y
    case('L')
      selectcase( y )
      case('a') ; Z = 57 ! La
      case('i') ; Z =  3 ! Li
      case('r') ; Z =103 ! Lr
      case('u') ; Z = 71 ! Lu
      endselect ! y
    case('M')
      selectcase( y )
      case('d') ; Z =101 ! Md
      case('g') ; Z = 12 ! Mg
      case('n') ; Z = 25 ! Mn
      case('o') ; Z = 42 ! Mo
      case('t') ; Z =109 ! Mt
      endselect ! y
    case('N')
      selectcase( y )
      case(' ') ; Z =  7 ! N
      case('a') ; Z = 11 ! Na
      case('b') ; Z = 41 ! Nb
      case('d') ; Z = 60 ! Nd
      case('e') ; Z = 10 ! Ne
      case('i') ; Z = 28 ! Ni
      case('o') ; Z =102 ! No
      case('p') ; Z = 93 ! Np
      endselect ! y
    case('O')
      selectcase( y )
      case(' ') ; Z =  8 ! O
      case('s') ; Z = 76 ! Os
      endselect ! y
    case('P')
      selectcase( y )
      case(' ') ; Z = 15 ! P
      case('a') ; Z = 91 ! Pa
      case('b') ; Z = 82 ! Pb
      case('d') ; Z = 46 ! Pd
      case('m') ; Z = 61 ! Pm
      case('o') ; Z = 84 ! Po
      case('r') ; Z = 59 ! Pr
      case('t') ; Z = 78 ! Pt
      case('u') ; Z = 94 ! Pu
      endselect ! y
    case('R')
      selectcase( y )
      case('a') ; Z = 88 ! Ra
      case('b') ; Z = 37 ! Rb
      case('e') ; Z = 75 ! Re
      case('f') ; Z =104 ! Rf
      case('g') ; Z =111 ! Rg
      case('h') ; Z = 45 ! Rh
      case('n') ; Z = 86 ! Rn
      case('u') ; Z = 44 ! Ru
      endselect ! y
    case('S')
      selectcase( y )
      case(' ') ; Z = 16 ! S
      case('b') ; Z = 51 ! Sb
      case('c') ; Z = 21 ! Sc
      case('e') ; Z = 34 ! Se
      case('g') ; Z =106 ! Sg
      case('i') ; Z = 14 ! Si
      case('m') ; Z = 62 ! Sm
      case('n') ; Z = 50 ! Sn
      case('r') ; Z = 38 ! Sr
      endselect ! y
    case('T')
      selectcase( y )
      case('a') ; Z = 73 ! Ta
      case('b') ; Z = 65 ! Tb
      case('c') ; Z = 43 ! Tc
      case('e') ; Z = 52 ! Te
      case('h') ; Z = 90 ! Th
      case('i') ; Z = 22 ! Ti
      case('l') ; Z = 81 ! Tl
      case('m') ; Z = 69 ! Tm
      endselect ! y
    case('U')
      selectcase( y )
      case(' ') ; Z = 92 ! U
      endselect ! y
    case('V')
      selectcase( y )
      case(' ') ; Z = 23 ! V
      endselect ! y
    case('W')
      selectcase( y )
      case(' ') ; Z = 74 ! W
      endselect ! y
    case('X')
      selectcase( y )
      case('e') ; Z = 54 ! Xe
      case(' ') ; Z =121 ! custom (jmol style)
      endselect ! y
    case('Y')
      selectcase( y )
      case(' ') ; Z = 39 ! Y
      case('b') ; Z = 70 ! Yb
      endselect ! y
    case('Z')
      selectcase( y )
      case('n') ; Z = 30 ! Zn
      case('r') ; Z = 40 ! Zr
      endselect ! y

    ! ==== specialties ====
    case('_')
      selectcase( y )
      case('_') ; Z =  0 ! __
      endselect ! y
    case('e')
      selectcase( y )
      case(' ') ; Z = -1 ! electron
      endselect ! y
    case('+')
      selectcase( y )
      case('+') ; Z =121 ! custom
      endselect ! y
    ! =====================

    !-------------------------------
    endselect ! S

!! code generator: input alphabetically ordered list of chem. symbols
! !       character(len=2) :: sy
! !       character        :: s, y, s_prev=' '
! !       integer          :: Z, ios
! !       write(*,'(9A)') "    selectcase( s )"
! !       read(5,*,IOstat=ios) sy, Z ! read 1st
! !       do while( ios == 0 )
! !         s = sy(1:1)
! !         y = sy(2:2)
! !         if( s /= s_prev ) then
! !           if( s_prev /= ' ' ) &
! !           write(*,'(9A)') "      endselect ! y"
! !           write(*,'(9A)') "    case('",s,"')"
! !           write(*,'(9A)') "      selectcase( y )"
! !         endif
! !           write(*,'(3A,I0,9A)') "      case('",y,"') ; Z = ", Z, " ! ", sy
! !         s_prev = s
! !         read(5,*,IOstat=ios) sy, Z ! read nxt
! !       enddo ! while
! !       write(*,'(9A)') "      endselect ! y"
! !       write(*,'(9A)') "    endselect ! s"
! !       end
  endfunction ! atomic_number_by_symbol


  !! checks for misstyped keywords
  logical function possible_misstypes( word, poss ) result( match )
  use configuration, only: o, WARNING
    character(len=*), intent(in)    :: word, poss(:) !! word to be compared, possibilities 

cDBG  character(len=*), parameter   :: fun = ' possible_misstypes: '
    real, parameter                 :: SIMILARITY_THRESHOLD = 0.51
    integer                         :: i, imaxpos
    real                            :: sim(size(poss)), simmax

    simmax = 0. ! init
    imaxpos = 0 ! init as none
    do i = 1, size(poss)
      sim(i) = string_compare( word, poss(i) )
      if( sim(i) > simmax ) then
        simmax = sim(i)
        imaxpos = i
      endif ! sim > simmax
    enddo ! i

    if( imaxpos > 0 .and. simmax > SIMILARITY_THRESHOLD ) then
      match = .true.
      if(o>0) write(o,'(6A,I0,9A)') sym, fun, WARNING(0), '"', trim(word), '" is ',int(simmax*100.), '% similar to "', trim(poss(imaxpos)), '".'
    else
      match = .false.
cDBG  if(o>0) write(o,'(9A)') sym, fun, 'no similarity found for "', trim(word), '".'
    endif ! match
  endfunction ! possible_misstypes


  !! compares two strings
  real function string_compare( s1, s2 )
    character(len=*), intent(in) :: s1, s2
    integer :: k, j, ident, nd, l1, l2 ! lengths

    l1 = len_trim( s1 )
    l2 = len_trim( s2 )

    ident = 0
    if( l1 >= l2 ) then
      nd = l1 - l2
      do j = 0, nd
        do k = 1, l2
          if( s1(k+j:k+j) == s2(k:k) ) ident = ident + 1
        enddo ! k
      enddo ! j
    elseif( l1 < l2 ) then ! l1 >= l2
      nd = l2 - l1
      do j = 0, nd
        do k = 1, l1
          if( s2(k+j:k+j) == s1(k:k) ) ident = ident + 1
        enddo ! k
      enddo ! j
#ifdef DEBUG
    else ; stop 'INP: string_compare: fatal error!'
#endif
    endif ! l1 >= l2

    string_compare = min(max( 0.0, real(ident)/(max(l1,l2,1)*1.)/(1.+nd) ), 1.0 )
#ifdef FULL_DEBUG
!!    write(*,'(6A,9F10.6)') sym,' string_compare: "',trim(s1),'" to "',trim(s2),'" ==>',string_compare,real(ident)/real(l1+nd)
#endif
  endfunction ! string_compare


#ifdef EXTENDED
  !! test the fractional number input function
  status_t function test( ) result( ios )
    iounit_t, parameter :: o = 6 ! output to std
cDBG  character(len=*), parameter :: fun = ' test: '
    integer  :: i
    string_t :: c, input_line = "17:19"
    real     :: r

    if(o>0) write(o,'(9A)') sym, fun, 'reading fractional numbers  in format  n:d where n and d are integer. exit by "stop".'
!     i = 0 ; do while( i == 0 )
!       read(unit=5,fmt=*,iostat=i) c
    read(unit=input_line,fmt=*,iostat=i) c
!       if( c == 'stop' ) i = -1
    ios = read_frac_number( c, r )
    if(o>0) write(o,'(5A,I0,A,F10.6)') sym, fun, 'read_frac_number( "', trim(c), '", r ) returns ',ios,' and r=', r
!     enddo ! while
  endfunction ! test
#endif

endmodule ! input

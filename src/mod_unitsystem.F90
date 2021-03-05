#ifdef DEBUG_ALL
#define DEBUG
#endif

! #define DEBUG
! #define FULL_DEBUG
! #define ATOMIC_UNITS_ALWAYS
! #define ATOMIC_UNITS_DEFAULT

!! @author Paul Baumeister
!! @version 4.00
!!
!! change your output unit system to "eV Ang", if required
module unitsystem
  use constants, only: ANGSTROM, EVOLT
implicit none
  private ! default visibility for this module namespace
  character(len=*), parameter, private :: sym = 'USYS' !! module symbol

  public :: set, operator(/)
#ifdef EXTENDED
  public :: test
#endif

  interface set !! change the unitsystem with a string
    module procedure set_unitsystem
  endinterface

  interface operator(/) !! use the division operator for the unit strings
    module procedure string_div
  endinterface

#ifdef ATOMIC_UNITS_ALWAYS
!+ atomic_units_always

  real, parameter, public :: eV  = 1.0          ; character(len=3), parameter, public :: eV_  = ' Ha'
  real, parameter, public :: Ang = 1.0          ; character(len=4), parameter, public :: Ang_ = ' aB '

!- atomic_units_always
#else
!+ variable unit system

#ifdef ATOMIC_UNITS_DEFAULT
  real, protected, public :: eV  = 1.0          ; character(len=3), protected, public :: eV_  = ' Ha'
  real, protected, public :: Ang = 1.0          ; character(len=4), protected, public :: Ang_ = ' aB '
#else
  real, protected, public :: eV  = EVOLT        ; character(len=3), protected, public :: eV_  = ' eV'
  real, protected, public :: Ang = ANGSTROM     ; character(len=4), protected, public :: Ang_ = ' Ang'
#endif

!- variable unit system
#endif

  contains

  !! control functionality
  integer function set_unitsystem( str ) result( ios )
#ifndef DEBUG
  use configuration, only: o
#else
    integer, parameter :: o=6 ! output to stdout
#endif
    character(len=*), intent(in) :: str !! string input, e.g. "Angstrom"

#ifdef ATOMIC_UNITS_ALWAYS
!+ atomic_units_always

    ios = -1 ! no modification possible !
    ! show, that no system can be set than atomic units
    if(o>0) write(o,'(9A)') sym, ': cannot modify unit system with "', trim(str), '", use atomic units.'

!- atomic_units_always
#else
!+ variable unit system

    type :: my_unit !! internal type
      character(len=4)    :: abbr !! abbreviation
      character(len=12)   :: name !! full name
      real                :: fac  !! factor
    endtype ! my_unit
    ! ============================================================
    ! predefined unit systems
    ! ============================================================
    ! = length ===================================================
    ! ============================================================
    type(my_unit), parameter :: L_UNIT(0:3) = (/ &
      my_unit( 'aB',  'Bohr',                1.0 ), &
      my_unit( 'Ang', 'Angstrom',       ANGSTROM ), & ! 0.52917724924
      my_unit( 'pm',  'picometer', 100.*ANGSTROM ), &
      my_unit( 'nm',  'nanometer',  0.1*ANGSTROM ) /)
    ! ============================================================
    ! = energy ===================================================
    ! ============================================================
    type(my_unit), parameter :: E_UNIT(0:7) = (/ &
      my_unit( 'Ha',   'Hartree', 1.0 ), &
      my_unit( 'eV',   'eVolt', EVOLT ), & ! EVOLT=27.210282768626
      my_unit( 'meV',  'milieVolt', 1E3*EVOLT ), &
      my_unit( 'Ry',   'Rydberg', 2.0 ), &
      my_unit( 'mHa',  'miliHa', 1E3 ), &
      my_unit( 'kJ',   'kJ/mol', 2625.50 ), &
      my_unit( 'kcal', 'kcal/mol', 627.51 ), &
      my_unit( 'cm',   'cm-1', 219470. ) /)
    ! ============================================================
#ifdef ATOMIC_UNITS_DEFAULT
    integer, save :: index_le(0:1) = 0 ! defaults: Bohr, Ha
#else
    integer, save :: index_le(0:1) = 1 ! defaults: Ang, eV
#endif
    character(len=*), parameter :: fun=': set '
    integer :: ie, il

    ios = 0
    selectcase( str )
    case( '' ) ; return ! does nothing
    case( L_UNIT(0)%abbr, L_UNIT(0)%name ) ; il = 0
    case( L_UNIT(1)%abbr, L_UNIT(1)%name ) ; il = 1
    case( L_UNIT(2)%abbr, L_UNIT(2)%name ) ; il = 2
    case( L_UNIT(3)%abbr, L_UNIT(3)%name ) ; il = 3
    case default ; ios = 1                 ; il = 0
    endselect ! str

    if( ios == 0 ) then
      ! set the units for length
      Ang = L_UNIT(il)%fac       ! factor
      Ang_(1:1) = ' '            ! blank
      Ang_(2:) = L_UNIT(il)%abbr ! abbreviation

      if( index_le(0) /= il ) then
        if(o>0) write(o,'(9A)') sym, fun, 'length unit to "', trim(L_UNIT(il)%name),'".'
        index_le(0) = il ! set
      endif ! changed

    else  ! ok1
      ios = 0
      selectcase( str )
      case( E_UNIT(0)%abbr, E_UNIT(0)%name ) ; ie = 0
      case( E_UNIT(1)%abbr, E_UNIT(1)%name ) ; ie = 1
      case( E_UNIT(2)%abbr, E_UNIT(2)%name ) ; ie = 2
      case( E_UNIT(3)%abbr, E_UNIT(3)%name ) ; ie = 3
      case( E_UNIT(4)%abbr, E_UNIT(4)%name ) ; ie = 4
      case( E_UNIT(5)%abbr, E_UNIT(5)%name ) ; ie = 5
      case( E_UNIT(6)%abbr, E_UNIT(6)%name ) ; ie = 6
      case( E_UNIT(7)%abbr, E_UNIT(7)%name ) ; ie = 7
      case default ; ios = 2                 ; ie = 0
      endselect ! str

      if( ios == 0 ) then
        ! set the units for energy
        eV = E_UNIT(ie)%fac       ! factor
        eV_(1:1) = ' '            ! blank
        eV_(2:) = E_UNIT(ie)%abbr ! abbreviation

        if( index_le(1) /= ie ) then
          if(o>0) write(o,'(9A)') sym, fun, 'energy unit to "', trim(E_UNIT(ie)%name),'".'
          index_le(1) = ie ! set
        endif ! changed

#ifdef DEBUG
      else ; if(o>0) write(o,'(9A)') sym, fun, 'unrekognized keyword "', trim(str), '".'
#endif
      endif ! ok2
    endif ! ok1

#ifdef DEBUG
    if(o>0) write(o,'(2A,9(3A,F0.6))') sym, fun, 'factors', trim(eV_), ' = ', eV, ' and', trim(Ang_), ' = ', Ang
#endif

!- variable unit system
#endif
  endfunction ! set_unitsystem

  character(len=8) function string_div( nom, den ) result( s )
    character(len=*), intent(in) :: nom, den
    integer :: ios
    write(unit=s,fmt='(9A)',IOstat=ios) trim(nom),'/',adjustl(den)
  endfunction ! string_div

#ifdef EXTENDED
  integer function test( )
    write(*,*,iostat=test) __FILE__,' no module test implemented!'
  endfunction ! test
#endif

endmodule ! unitsystem

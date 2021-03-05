#include "config.h"

!! @author Paul Baumeister
!! @version 3.0
!!
!! only constant expressions allowed in this module
module constants
implicit none
  public ! default for this module namespace

  integer, parameter :: SOLVER_SUBSROT          =  b'00000001'
  integer, parameter :: SOLVER_DESCENT          =  b'00000010'
  integer, parameter :: SOLVER_CONGRAD          =  b'00000100'
  integer, parameter :: SOLVER_RMMDIIS          =  b'00001000'
  integer, parameter :: SOLVER_XPLICIT          =  b'00010000'
  integer, parameter :: SOLVER_NONE             =  b'00000000'
  integer, parameter :: SOLVER_SCHEME_STABLE    =  b'00001101' ! sr & cg & di
  integer, parameter :: SOLVER_SCHEME_SPEED     =  b'00001001' ! sr & di
  integer, parameter :: SOLVER_DAVIDSO          =  b'00100000' ! 

  ! physical constants
  real, parameter    :: SPEED_OF_LIGHT = 137.035989561 ! 137.0359895(61) in Hartree atomic units

  ! mathematical constants
  real, parameter    :: PI = 3.1415926535897932384626433832795028841971
  real, parameter    :: ONESQRTFOURPI = 0.282094791773878143474039725780386 !! 1./sqrt(4*Pi)
  real, parameter    :: SQRTPI = 1.77245385090551602729816748334115 !! sqrt(Pi)
  real, parameter    :: SQRT4PI = 2.*SQRTPI !! sqrt(4*Pi)
  real, parameter    :: SQRTHALF = 0.707106781186547524400844362104849 !! sqrt(0.5)
  real, parameter    :: SQRT2OVERPI = 0.797884560802865355879892119868764 ! = sqrt(2./Pi)

  ! unit conversion factors
  real, parameter    :: NUCLEON_MASS = 1822.888479031408 !! one Dalton, approx. mass of a nucleon in Hartree atomic units
  real, parameter    :: ANGSTROM = 0.52917724924
  real, parameter    :: EVOLT    = 27.211385 !27.210282768626
  ! real, parameter  :: K_BOLTZMANN = 3.1668294205414475E-6 !! Boltzmanns constant in Hartree atomic units
  real, parameter    :: KELVIN   = 315773.244215 ! = 1./K_BOLTZMANN
!   real, parameter    :: OERSTED  = ???

  ! element symbols
  character(len=2), parameter :: PSE(-1:121) = (/ & !! element symbol from the periodic tabel of elements
  'e ', '__', &                                                    ! vacuum
  'H ',                              'He', &                       ! 1s
  'Li','Be','B ','C ','N ','O ','F ','Ne', &                       ! 2s, 2p
  'Na','Mg','Al','Si','P ','S ','Cl','Ar',&                        ! 3s, 3p
  'K ','Ca', &                                                     ! 4s
        'Sc','Ti','V ','Cr','Mn','Fe','Co','Ni','Cu','Zn', &       ! 3d
            'Ga','Ge','As','Se','Br','Kr', &                       ! 4p
  'Rb','Sr', &                                                     ! 5s
        'Y ','Zr','Nb','Mo','Tc','Ru','Rh','Pd','Ag','Cd', &       ! 4d
            'In','Sn','Sb','Te','I ','Xe', &                       ! 5p
  'Cs','Ba', &                                                     ! 6s
        'La', &                                                    ! 5d
                            'Ce','Pr','Nd','Pm','Sm','Eu','Gd', &  ! 4f Lanthanides
                            'Tb','Dy','Ho','Er','Tm','Yb','Lu', &  ! 4f Lanthanides
            'Hf','Ta','W ','Re','Os','Ir','Pt','Au','Hg', &        ! 5d
            'Tl','Pb','Bi','Po','At','Rn', &                       ! 6p
  'Fr','Ra', &                                                     ! 7s
        'Ac', &                                                    ! 7p
                            'Th','Pa','U ','Np','Pu','Am','Cm', &  ! 5f Actinides
                            'Bk','Cf','Es','Fm','Md','No','Lr', &  ! 5f Actinides
            'Rf','Db','Sg','Bh','Hs','Mt','Ds','Rg','Cn', &        ! 6d
            'ut','uq','up','uh','us','uo', &                       ! 7p
  'un','u2','++' /)                                                ! custom
!
! ! use constants, only: PSE
! !   write(*,'(92(A1,I2.2,A,A2))') ('"', i, trim(PSE(i)), '" ', i=1,92 )
!

  interface opt
    module procedure opt_bool, opt_int, opt_float, opt_complex, opt_string
  endinterface
  private ::         opt_bool, opt_int, opt_float, opt_complex, opt_string

  contains

  logical function opt_bool( b, def ) result( zero )
    logical, intent(in), optional :: b
    logical, intent(in)           :: def ! needs to called with argument name ,def=... !
    if( present( b ) ) then ; zero = b ; else ; zero = def ; endif
  endfunction ! opt

  integer function opt_int( i, def ) result( zero )
    integer, intent(in), optional :: i
    integer, intent(in)           :: def ! needs to called with argument name ,def=... !
    if( present( i ) ) then ; zero = i ; else ; zero = def ; endif
  endfunction ! opt

  real function opt_float( f, def ) result( zero )
    real, intent(in), optional :: f
    real, intent(in)           :: def ! needs to called with argument name ,def=... !
    if( present( f ) ) then ; zero = f ; else ; zero = def ; endif
  endfunction ! opt

  real function opt_complex( c, def ) result( zero )
    complex, intent(in), optional :: c
    complex, intent(in)           :: def ! needs to called with argument name ,def=... !
    if( present( c ) ) then ; zero = c ; else ; zero = def ; endif
  endfunction ! opt

  string_t function opt_string( s, def ) result( zero )
    character(len=*), intent(in), optional :: s
    character(len=*), intent(in)           :: def ! needs to called with argument name ,def=... !
    if( present( s ) ) then ; zero = s ; else ; zero = def ; endif
  endfunction ! opt

#ifdef EXTENDED
!+ extended

  status_t function test( ) result( ios )
    ! calculate machine accuracy
    real :: machine_precision, machine_zero, machine_infinity, a1, a2, a3
    machine_precision = 0
    a1 = 4.0/3.0
    do while (machine_precision == 0.0)
       a2 = a1 - 1.0
       a3 = a2 + a2 + a2
       machine_precision = abs(a3 - 1.0)
    enddo
    machine_zero = machine_precision**4
    machine_infinity = 1.0/machine_zero
    write(*,*,iostat=ios) __FILE__,' [machine_precision, machine_zero, machine_infinity] = ', &
                                      machine_precision, machine_zero, machine_infinity
  endfunction ! test

!- extended
#endif

endmodule ! constants

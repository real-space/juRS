#include "config.h"

! #define DEBUG


!! @author Paul Baumeister
!! @version 3.0
!! @see type_kpoint
!! information container for Kohn-Sham states
module type_state

implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: sym = 'tSTATE' !! module symbol

  public :: set, state
#ifdef EXTENDED
  public :: test
#endif


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!    type(state)      !!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#ifdef CONSTR
  integer, parameter, public :: MAX_N_CONSTRAINTS = max(1, CONSTR )
#endif

  type :: state !! Kohn-Sham state descriptor
    real    :: ene       = 0. !! energy
    real    :: occ       = 0. !! occupation
    real    :: wgt       = 1. !! weight
    real    :: res       = 1. !! residual in Ha
    real    :: threshold = 1E-8 !! in Ha
    integer :: jbnd      = 0 !! global band index
    integer :: jspn      = 0 !! global spin index
    integer :: jkpt      = 0 !! global k-point index
#ifdef CONSTR
    real :: cwgt(MAX_N_CONSTRAINTS) !! constraint weight
    real :: cocc(MAX_N_CONSTRAINTS) !! constraint occupation
#endif
  endtype ! state

  interface set
    module procedure state_set
  endinterface

  contains

  type(state) function state_set( j, kw8 ) result( s )
#ifdef DEBUG
  use configuration, only: o, WARNING
#endif
    integer, intent(in)         :: j(3) ! global indices for bands, spins and kpoints
    real, intent(in), optional  :: kw8 ! weight of the k-point

#ifdef DEBUG
    if( any( j < 1 ) .and. o>0 ) write(o,'(4A,9(" ",I0))') sym,' state_set: ', WARNING(0), 'a global index is < 1, j=', j
#endif
    s%jbnd     = j(1)
    s%jspn     = j(2)
    s%jkpt     = j(3)
    s%wgt = 1. ; if( present( kw8 ) ) s%wgt = min( max( 0., kw8 ), 1.0 )
  endfunction ! state_set

#ifdef EXTENDED
!+ extended

  status_t function test( ) result( ios )
    write(*,*,iostat=ios) __FILE__,' no module test implemented!'
  endfunction ! test

!- extended
#endif

endmodule ! type_state

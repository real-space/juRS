#include "config.h"

!! @author Paul Baumeister
!! @version 3.0
!!
!! a simple information container
module type_info
implicit none
  public ! default for this module namespace


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!    type(info)      !!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  type :: info
    MPI_Comm :: cart_calc_comm ! Cartesian calculation MPI communicator

    integer :: band_comm !! band MPI  communicator
    integer :: nbands   = 1 !! number of all bands
    integer :: nbnd     = 1 !! number of bands here
    integer :: iobnd    = 0 !! offset of bands

    integer :: nspins   = 1 !! number of all spins
    integer :: nspn     = 1 !! number of spins here
    integer :: iospn    = 0 !! offset of spins

    integer :: nkpoints = 1 !! number of all k-points
    integer :: nkpt     = 1 !! number of k-points here
    integer :: iokpt    = 0 !! offset of k-points

    real    :: temperature = 1E-2 ! start temperature
    integer :: nhistory    = 1
    real    :: charge      = 0.
    real    :: mixing      = 0.5 ! start mixing
    integer :: kmixing     = 0
    integer :: ksymmetry   = 0
    integer :: ksolver     = 0
    integer :: kpoisson    = 0
    integer :: kfunctional = 0
  endtype ! info

  interface set
    module procedure info_set
  endinterface
  private ::         info_set

  contains

  type(info) function info_set( nbands, nspins, nkpoints, nbnd, nspn, nkpt ) result( i )
    integer, intent(in)             :: nbands, nspins, nkpoints
    integer, intent(in), optional   :: nbnd, nspn, nkpt

    i%nbands   = nbands
    i%nspins   = nspins
    i%nkpoints = nkpoints

    i%nbnd = nbands   ; if( present( nbnd ) ) i%nbnd = nbnd
    i%nspn = nspins   ; if( present( nspn ) ) i%nspn = nspn
    i%nkpt = nkpoints ; if( present( nkpt ) ) i%nkpt = nkpt

    i%iobnd = 0
    i%iospn = 0
    i%iokpt = 0
  endfunction ! info_set

#ifdef EXTENDED
!+ extended

  status_t function test( ) result( ios )
    write(*,*,iostat=ios) __FILE__,' no module test implemented!'
  endfunction ! test

!- extended
#endif

endmodule ! type_info

!! @author Andrea Nobile
#include "pawxmlreader.h"
    

module mod_pawxmlreader
  use iso_c_binding
  implicit none

  ! must match exactly first part of struct atom in .c file
  type, bind(C) :: paw_atom
    real(c_double) :: zeta
    real(c_double) :: n_core
    real(c_double) :: n_valence
    real(c_double) :: ae_energy_kin
    real(c_double) :: ae_energy_xc
    real(c_double) :: ae_energy_es
    real(c_double) :: ae_energy_tot
    real(c_double) :: core_energy_kin
    real(c_double) :: shape_comp_rc
    real(c_double) :: shape_comp_lamb
    integer(c_int) :: shape_comp_type
    integer(c_int) :: num_states
    integer(c_int) :: num_grids
  endtype ! paw_atom

  
  ! must match defines in .c file 
  integer, parameter :: SHAPE_GAUSS  = PAWXMLREADER_SHAPE_GAUSS  
  integer, parameter :: SHAPE_BESSEL = PAWXMLREADER_SHAPE_BESSEL
  integer, parameter :: SHAPE_SINC   = PAWXMLREADER_SHAPE_SINC
  integer, parameter :: SHAPE_EXP    = PAWXMLREADER_SHAPE_EXP


  character(len=32) :: xc_type
  character(len=32) :: xc_name

  interface 
!   C function to parse xml file with expat       
    subroutine pawxml_reader_read(filename) bind(C)      
      import
      character(kind=c_char), intent(in) :: filename(*)
    endsubroutine pawxml_reader_read
  endinterface

  interface
    subroutine pawxml_fill_paw_atom(pawatom) bind(C)
      import 
      type(paw_atom), intent(out) :: pawatom
    endsubroutine
  endinterface
  
  interface
    subroutine pawxml_reader_cleanup() bind(C)
      import 
    endsubroutine
  endinterface

  interface
    integer(c_int) function pawxml_get_number_of_projectors(ell) bind(C)
      import 
      integer(c_int), value :: ell
    endfunction 
  endinterface

  interface
    subroutine pawxml_get_generator_string(gen_string) bind(C)
      import
      character(kind=c_char), intent(out) :: gen_string(*)
    endsubroutine
  endinterface

  interface
    real(c_double) function pawxml_get_projector_rcut(ell) bind(C)
      import 
      integer(c_int), value :: ell
    endfunction 
  endinterface

  interface
     real(c_double) function pawxml_get_state_ae_energy(enn, ell) bind(C)
      import 
      integer(c_int), value :: enn
      integer(c_int), value :: ell
    endfunction 
  endinterface

  interface
     real(c_double) function pawxml_get_state_occupation(enn, ell) bind(C)
      import 
      integer(c_int), value :: enn
      integer(c_int), value :: ell
    endfunction 
  endinterface 
  

  !!void pawxml_get_grid(double *a, int *istart, int *iend, int *n)
  interface
    subroutine pawxml_get_grid(a, istart, iend, n) bind(C)
      import
      real(c_double) :: a
      integer(c_int) :: istart
      integer(c_int) :: iend
      integer(c_int) :: n
    endsubroutine
  endinterface

    !double pawxml_get_kin_diff(int enn, int ell, int en2, int el2 )
  interface
    real(c_double) function pawxml_get_kin_diff(enn, ell, en2, el2) bind(C)
      import 
      integer(c_int), value :: enn
      integer(c_int), value :: ell
      integer(c_int), value :: en2
      integer(c_int), value :: el2
    endfunction 
  endinterface 

  !!void pawxml_get_rho_core(double *rho_smoot, double *rho_true, double *grid, int n)
  interface
    subroutine pawxml_get_rho_core(rho_smoot, rho_true, grid, n, cm) bind(C)
      import 
      real(c_double) :: rho_smoot
      real(c_double) :: rho_true
      real(c_double) :: grid
      integer(c_int), intent(in), value :: n
      real(c_double) :: cm
    endsubroutine 
  endinterface 

  !// key is a number that tells if the requested wf is all electron, smooth or projector
  !// wave is output, 
  !void pawxml_get_state_wave(double *wave, double *grid, int n, int enn, int ell, int key)
  interface
    subroutine pawxml_get_state_wave(wave, grid, n, enn, ell, key) bind(C)
      import 
      real(c_double) :: wave
      real(c_double) :: grid
      integer(c_int), intent(in), value :: n
      integer(c_int), intent(in), value :: enn
      integer(c_int), intent(in), value :: ell
      integer(c_int), intent(in), value :: key
    endsubroutine 
  endinterface 
  
  interface
    subroutine pawxml_get_v_zero(v_zero, grid, n) bind(C)
      import 
      real(c_double) :: v_zero
      real(c_double) :: grid
      integer(c_int), intent(in), value :: n
    endsubroutine 
  endinterface 
  
  interface      
    subroutine pawxml_get_xc_type(xctype) bind(C)      
      import
      character(kind=c_char), intent(out) :: xctype(*)
    endsubroutine 
  endinterface

  interface      
    subroutine pawxml_get_xc_name(xcname) bind(C)      
      import
      character(kind=c_char), intent(out) :: xcname(*)
    endsubroutine 
  endinterface

  public :: print_from_fortran

  contains 

  subroutine pawxml_read(filename)
    character(len=*) :: filename
    call pawxml_reader_read(filename)
  endsubroutine ! pawxml_read

  subroutine print_from_fortran(to_print_c, n) bind(C)
    use configuration, only: o
    character(kind=c_char), intent(in) :: to_print_c(128)
    integer(kind=c_int), intent(in) :: n
    integer :: i
    if(o>0) write(o,'(128A)',advance='no') (to_print_c(i), i=1,n)
  end subroutine
  
endmodule ! mod_pawxmlreader

      
      



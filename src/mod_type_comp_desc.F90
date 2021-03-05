#include "pawxmlreader.h"

!!! @Author Andrea Nobile

module type_comp_desc
implicit none
  
  type :: comp_desc !! compesator description, used to contruct the compensator
    integer          :: comp_type
    real             :: comp_rc
    real             :: comp_lamb
  endtype ! comp_desc

! must match defines in pawxmlreader.c file and mod_pawxmlreader
  integer, parameter  :: SHAPE_GAUSS  = PAWXMLREADER_SHAPE_GAUSS  
  integer, parameter  :: SHAPE_BESSEL = PAWXMLREADER_SHAPE_BESSEL
  integer, parameter  :: SHAPE_SINC   = PAWXMLREADER_SHAPE_SINC
  integer, parameter  :: SHAPE_EXP    = PAWXMLREADER_SHAPE_EXP

endmodule ! type_comp_desc

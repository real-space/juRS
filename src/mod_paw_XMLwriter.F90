#include "config.h"

module paw_XMLwriter
use type_element, only: symbol, element_name
implicit none
  private ! default for this module namespace

  public :: valence_state, write_paw_xml
#ifdef EXTENDED 
  public :: test
#endif

  iounit_t, parameter :: o=6

  type :: valence_state
    integer :: n =  0 ! principal quantum number
    integer :: l = -1 ! angular momentum character
    real :: f  = 0 ! occupation
    real :: rc = 0 ! cutoff radius
    real :: e  = 0 ! energy
    character(len=8) :: id = "" ! indentifier
    integer :: ae ! key for the ae_partial_wave
    integer :: ps ! key for the pseudo_partial_wave
    integer :: pr ! key for the projector_function
  endtype ! valence_state

  contains

  status_t function write_paw_xml(iZ, qcv, config, xc, energies, s, gauss_rc, &
                                  radial_data, kinetic_energy_differences, grid) result(ios)
  use type_rgrid, only: rgrid, rgrideq2string
    integer, intent(in) :: iZ
    real, intent(in)    :: qcv(2) ! [qcore,qvalence]
    character(len=*), intent(in) :: config
    character(len=*), intent(in) :: xc(2) ! [type,name]
    real, intent(in) :: energies(:) ! (5)
    type(valence_state), intent(in) :: s(:) ! (nstates)
    real, intent(in) :: gauss_rc
    real, intent(in) :: radial_data(:,:) ! (n_grid,keys)
    real, intent(in) :: kinetic_energy_differences(:,:) ! (nstates,nstates)
    type(rgrid), intent(in) :: grid

    iounit_t, parameter :: fu = 321
    integer :: i, n_grid, nstates
    character(len=32) :: filename, occupation
    character(len=4) :: chemsym
    
    write(unit=filename, fmt="(a,i3.3,a)") "pawdata.",iZ,".xml"
    open(unit=fu, file=filename, action="write", iostat=ios) 
    if (ios /= 0) return
    n_grid = size(radial_data, 1)
    write(fu, "(a)") '<?xml version="1.0"?>'
    write(fu, "(a)") '<paw_setup version="0.6">'
    write(fu, "(9a)")'  <!-- ',trim(element_name(iZ)),' setup for the Projector Augmented Wave method. -->'
    write(fu, "(a)") '  <!-- Units: Hartree and Bohr radii.                      -->'
    
    chemsym = Symbol(iZ)
    write(fu, "(3a,9(i0,a,f0.1,a))") '  <atom symbol="',trim(chemsym),'" Z="',iZ,'" core="',qcv(1),'" valence="',nint(qcv(2)),'"/>'
    write(fu, "(9a)") '  <xc_functional type="',trim(xc(1)),'" name="',trim(xc(2)),'"/>'
    write(fu, "(a)") '  <generator type="scalar-relativistic" name="juRS">'
    write(fu, "(9a)") '    ',trim(config)
    write(fu, "(a)") '  </generator>'
    write(fu, "(9(a,f0.6))") '  <ae_energy kinetic="',energies(1),'" xc="',energies(3), & 
                                '" electrostatic="',energies(2),'" total="',energies(4),'"/>'
    write(fu, "(9(a,f0.6))") '  <core_energy kinetic="',energies(5),'"/>'
    write(fu, "(a)") '  <valence_states>'
    nstates = size(s)
    do i = 1, nstates
      occupation = "" ; if (s(i)%f > 0) write(occupation, "(9(a,f0.3))") ' f="',s(i)%f,'"'
      write(fu, "(2(a,i0),a,f0.3,a,f0.6,9a)") '    <state n="',s(i)%n,'" l="',s(i)%l, &
          '" rc="',s(i)%rc,'" e="',s(i)%e,'" id="',trim(chemsym),'-',trim(s(i)%id),'"',trim(occupation),'/>'
    enddo ! i states
    write(fu, "(a)") '  </valence_states>'
    
    write(fu, "(2a,2(a,es13.6e2),9(a,i0))") '  <radial_grid eq="',trim(rgrideq2string(grid)),'" a="',grid%a,'" d="',grid%d,'" n="',n_grid,'" istart="0" iend="',n_grid-1,'" id="g1"/>'
    write(fu, "(9(a,es20.12e2))") '  <shape_function type="gauss" rc="',gauss_rc,'"/>'
    
    write(fu, "(a)") '  <ae_core_density grid="g1">'
    write(fu, "(a,9999(es20.12e3,' '))") '    ',radial_data(:,1)
    write(fu, "(a)") '  </ae_core_density>'
    write(fu, "(a)") '  <pseudo_core_density grid="g1">'
    write(fu, "(a,9999(es20.12e3,' '))") '    ',radial_data(:,2)
    write(fu, "(a)") '  </pseudo_core_density>'
    write(fu, "(a)") '  <pseudo_valence_density grid="g1">'
    write(fu, "(a,9999(es20.12e3,' '))") '    ',radial_data(:,3)
    write(fu, "(a)") '  </pseudo_valence_density>'
    write(fu, "(a)") '  <zero_potential grid="g1">'
    write(fu, "(a,9999(es20.12e3,' '))") '    ',radial_data(:,4)
    write(fu, "(a)") '  </zero_potential>'
    write(fu, "(a)") '  <ae_core_kinetic_energy_density grid="g1">'
    write(fu, "(a,9999(es20.12e3,' '))") '    ',radial_data(:,5)
    write(fu, "(a)") '  </ae_core_kinetic_energy_density>'
    write(fu, "(a)") '  <pseudo_core_kinetic_energy_density grid="g1">'
    write(fu, "(a,9999(es20.12e3,' '))") '    ',radial_data(:,6)
    write(fu, "(a)") '  </pseudo_core_kinetic_energy_density>'

    do i = 1, nstates
      write(fu, "(9a)") '  <ae_partial_wave state="',trim(chemsym),'-',trim(s(i)%id),'" grid="g1">'
      write(fu, "(a,9999(es20.12e3,' '))") '    ',radial_data(:,s(i)%ae)
      write(fu, "(9a)") '  </ae_partial_wave>'
      write(fu, "(9a)") '  <pseudo_partial_wave state="',trim(chemsym),'-',trim(s(i)%id),'" grid="g1">'
      write(fu, "(a,9999(es20.12e3,' '))") '    ',radial_data(:,s(i)%ps)
      write(fu, "(9a)") '  </pseudo_partial_wave>'
      write(fu, "(9a)") '  <projector_function state="',trim(chemsym),'-',trim(s(i)%id),'" grid="g1">'
      write(fu, "(a,9999(es20.12e3,' '))") '    ',radial_data(:,s(i)%pr)
      write(fu, "(9a)") '  </projector_function>'
    enddo ! i states
  
    write(fu, "(a)") '  <kinetic_energy_differences>'
    do i = 1, nstates
      write(fu, "(a,999es21.12e3)") '    ',kinetic_energy_differences(:,i)
    enddo ! i states
    write(fu, "(a)") '  </kinetic_energy_differences>'
    write(fu, "(a)") '</paw_setup>'
    close(unit=fu, iostat=ios)
  endfunction ! write_paw_xml

#ifdef EXTENDED 
  status_t function test()
  use type_rgrid, only: rgrid_create_exp, rgrid
    type(valence_state) :: val(6)
    type(rgrid) :: g
    real :: ked(6,6)=0, rad(600,24)=0, energies(5)=0

    g = rgrid_create_exp( nint( 250.*sqrt(4+9.) )) ! set the radial grid with default values
    test = write_paw_xml(26, [18., 8.], "custom", ["LDA", "PW"], energies, val, .632455532, rad, ked, g)
  endfunction ! test
#endif

endmodule ! paw_XMLwriter

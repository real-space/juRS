#include "config.h"

#define DEBUG
#define FULL_DEBUG

#ifdef DEBUG

#define cDBG  

#ifdef FULL_DEBUG
#define cFDB   
#else
#define cFDB  !FDBG
#endif

#else

#define cDBG !DBG
#define cFDB  !FDBG

#endif


!! @author Paul Baumeister
!! @version 4.04
!!
!! reads PAW setups from an XML formatted file
module paw_XMLfile
  use configuration, only: o
implicit none
  private !! default for this namespace
  character(len=*), parameter, private :: sym = 'XML' !! module symbol

  public :: read_paw_XML_file
#ifdef EXTENDED
  public :: test
#endif

  interface operator( .get. )
    module procedure get_attribute, get_value_r, get_value_i
  endinterface

  ! private types
  type, private :: valence_state
    integer           :: n  =  0 ! principal quantum number
    integer           :: l  = -1 ! angular momentum qnumber
    real              :: f  = .0 ! occupation number
    real              :: rc = .0 ! cutoff radius
    real              :: e  = .0 ! energy parameter
    character(len=8)  :: id = '' ! identity tag
  endtype ! valence_state

  type, private :: radial_grid
    character(len=16) :: eq     = '' ! equation
    real              :: a      = .0 ! param1
    real              :: b      = .0 ! param2
    real              :: d      = .0 ! param3
    integer           :: n      =  0 ! number of grid points
    integer           :: istart =  0 ! start index
    integer           :: iend   = -1 ! end index
    character(len=8)  :: id     = '' ! identity tag
  endtype ! radial_grid

  contains

  !! read a file in the paw.xml format defined
  !! https://wiki.fysik.dtu.dk/gpaw/setups/pawxml.html#pawxml
  status_t function read_paw_XML_file( filename, s ) result( ios )
  use configuration, only: WARNING, ERROR
  use constants, only: sqrt4Pi
  use type_species, only: species
  use type_species, only: ELLMAX, ENNMAX, RWF_DIMENSION, RF_DIMENSION
  use type_species, only: I_VBAR, I_RHOC, I_SMT, I_TRU, I_PRJ
  use type_rgrid, only: rgrid_eqn
  use type_rgrid, only: operator(.at.)
cDBG  use unitsystem, only: eV, eV_, Ang, Ang_
  implicit none
    ! parameter
    character(len=*), parameter           :: fun = ' read: '
    iounit_t, parameter                   :: U = 8
    integer, parameter                    :: MAX_nLN = ENNMAX*(1+ELLMAX)
    integer, parameter                    :: MAX_NRG = 3 ! max. # of radial grids
    ! arguments
    character(len=*), intent(in)          :: filename
    type(species), intent(inout)          :: s ! new species

    ! local vars
    status_t                              :: lios
    integer                               :: nline, nignore
    character(len=128)                    :: line

    character(len=64)                     :: xml_object
    real                                  :: xml_version
    real                                  :: paw_setup_version

    character(len=2)                      :: atom_symbol
    integer                               :: atom_Z, atom_core, atom_valence

    string_t                              :: xc_type, xc_name

    string_t                              :: generator_type, generator_name, generator_content
    logical                               :: generator

    real                                  :: ae_energy_kinetic, ae_energy_xc
    real                                  :: ae_energy_electrostatic, ae_energy_total
    real                                  :: core_energy_kinetic

    integer                               :: jln, nln ! number of valence states
    type(valence_state)                   :: val(MAX_nLN) ! valence state descriptors (local type)
    real, allocatable                     :: dkin(:,:) ! kinetic energy deficit
    logical                               :: valence_states

    integer                               :: irg, nrg ! number of radial grids
    type(radial_grid)                     :: g(MAX_NRG) ! radial grid descriptors (local type)
    integer                               :: gn, max_gn = 0

    string_t                              :: grid_id = ''
    string_t                              :: state_id = ''


    string_t                              :: shape_function_type = '' ! '':numerical, grid must be given
    real                                  :: shape_function_rc = .0

    integer                               :: i, j, i_qnt ! quantity
    integer                               :: ell, enn, iln, nn(0:ELLMAX)
    integer                               :: iln_list(MAX_nLN) = 0

    integer, parameter :: &
      I_SHP = 7, & ! shape function
      I_AEC = 4, & ! ae_core_density
      I_PSC = 5, & ! pseudo_core_density
      I_PSV = 8, & ! pseudo_valence_density
      I_ZRO = 6, & ! zero_potential
      I_KJP = 9, & ! Kresse_Joubert_local_ionic_potential
      I_AEK = 10,& ! ae_core_kinetic_energy_density
      I_PSK = 11,& ! pseudo_core_kinetic_energy_density
      I_MAX = I_PSK+1,& ! max index will be ignored
      I_IGNORE_LINE = -1

    integer                               :: g1_index(4:I_MAX) = 0 ! radial grid index
    real, allocatable                     :: a1(:,:) ! (max_gn,4:M_IND)

    integer, parameter                    :: I_AEP = 1 ! ae_partial_wave
    integer, parameter                    :: I_PSP = 2 ! pseudo_partial_wave
    integer, parameter                    :: I_PRF = 3 ! projector_function
    integer                               :: g2_index(MAX_nLN,1:3) = 0 ! radial grid index
    real, allocatable                     :: a2(:,:,:) ! (max_gn,nln,1:3)
cDBG  real                                :: r
cDBG  integer                             :: ir

cDBG  if(o>0) write(o,'(/5A/)') sym, fun, 'try to open "', trim(filename), '".'
    open( unit=U, file=filename, action='read', status='old', iostat=ios )
    if( ios /= 0 ) then
      if(o>0) write(o,'(9A)') sym, fun, ERROR, 'failed to open "', trim(filename), '".'
      return ! error
    endif ! opening ist

    nline = 0 ! init line counter
    nignore = 0 ! init counter for ignored lines

    generator_content = ""
    generator = .false.
    valence_states = .false. ! switch 0:off 1:on else:error
    nln = 0 ! init number of valence states
    nrg = 0 ! init number of radial grids

    ! begin read loop
      line = '' ! init
      read( unit=U, fmt='(A)', iostat=lios ) line !! read 1st line
    do while( lios == 0 )
      nline = nline+1 ! count up number of lines
      i_qnt = 0 ! init as no quantity
      grid_id = ''
      state_id = ''
      xml_object = '' ! init
      read( unit=line, fmt=*, iostat=ios ) xml_object
      selectcase( xml_object )
      case( '' ) ! nothing
cDBG    if(o>0) write(o,'(3A,I0)') sym, fun, 'ignore blank line #', nline
      case( '<?xml' )
        xml_version = ( line .get. ' version=' ) .get. 0.0
cDBG    if(o>0) write(o,'(A,F0.3,9A)') '<?xml version="', xml_version, '"?>'
      case( '<paw_setup' )
        paw_setup_version = ( line .get. ' version=' ) .get. 0.0
cDBG    if(o>0) write(o,'(A,F0.3,9A)') '<paw_setup version="', paw_setup_version, '">'
      case( '<atom' )
        atom_symbol = line .get. ' symbol=' ! chemical symbol
        atom_Z = ( line .get. ' Z=' ) .get. 0 ! atomic number
        atom_core = ( line .get. ' core=' ) .get. 0 ! number of core electrons
        atom_valence = ( line .get. ' valence=' ) .get. 0 ! number of valence electrons
cDBG    if(o>0) write(o,'(3A,9(I0,A))') '<atom symbol="', trim(atom_symbol), '" Z="', atom_Z, '" core="', atom_core, '" valence="', atom_valence, '"/>'
      case( '<xc_functional' )
        xc_type = line .get. ' type='
        xc_name = line .get. ' name='
cDBG    if(o>0) write(o,'(9A)') '<xc_functional type="', trim(xc_type), '" name="', trim(xc_name), '"/>'
      case( '<generator' ) ; generator = .true.
        generator_type = line .get. ' type='
        generator_name = line .get. ' name='
      case( '<ae_energy' )
        ae_energy_kinetic = ( line .get. ' kinetic=' ) .get. .0
        ae_energy_xc = ( line .get. ' xc=' ) .get. .0

!!         ! line break
!!         line = '' ! init
!!         read( unit=U, fmt='(A)', iostat=lios ) line !! read nxt line
!!         ae_energy_electrostatic = ( line .get. ' electrostatic=' ) .get. .0
!!         ae_energy_total = ( line .get. ' total=' ) .get. .0

        if( scan(line,'/>') < 1 ) read( unit=U, fmt='(A)', iostat=lios ) line !! read nxt line
        ae_energy_total = ( line .get. ' total=' ) .get. .0

        ae_energy_electrostatic = ( line .get. ' electrostatic=' ) .get. .0
        if( abs( ae_energy_total - ( ae_energy_kinetic+ae_energy_xc+ae_energy_electrostatic ) ) > 3E-6 .and. o>0) &
          write(o,'(4A,9(F0.8,A))') sym, fun, WARNING(0), 'found <ae_energy kinetic="', ae_energy_kinetic, '" xc="', ae_energy_xc, &
           '" electrostatic="', ae_energy_electrostatic, '" total="', ae_energy_total, '"> but calculated total=',ae_energy_kinetic+ae_energy_xc+ae_energy_electrostatic
        ae_energy_total = ae_energy_kinetic+ae_energy_xc+ae_energy_electrostatic
cDBG    if(o>0) write(o,'(9(A,F0.12))') '<ae_energy kinetic="', ae_energy_kinetic, '" xc="', ae_energy_xc, '" electrostatic="', ae_energy_electrostatic, '" total="', ae_energy_total, '">'
      case( '<core_energy' )
        core_energy_kinetic = ( line .get. ' kinetic=' ) .get. .0
cDBG    if(o>0) write(o,'(9(A,F0.12))') '<core_energy kinetic="', core_energy_kinetic, '">'
      case( '<valence_states>' ) ; valence_states = .true. ! switch on
      case( '<state' )

        if( valence_states ) then
          nln = nln+1 ! count up number of valence states
          if( nln > MAX_nLN ) stop 'pawXML: too many valence states!'
          jln = nln ! index of this valence state

          val(jln)%n  = ( line .get.  ' n=' ) .get.  0 ! principal quantum number
          val(jln)%l  = ( line .get.  ' l=' ) .get. -1 ! angular momentum qnumber
          val(jln)%f  = ( line .get.  ' f=' ) .get. .0 ! occupation number
          val(jln)%rc = ( line .get. ' rc=' ) .get. .0 ! cutoff radius
          val(jln)%e  = ( line .get.  ' e=' ) .get. .0 ! energy parameter
          val(jln)%id = ( line .get. ' id=' )          ! identity tag
cFDB       if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'new state object #', jln, ': energy = ', ( line .get.  'e=' )
cFDB       if(o>0) write(o,*) val(jln)
cFDB       if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'new state object #', jln, ':'

cDBG      if( val(jln)%n > 0 .and. val(jln)%n < 9 ) then
cDBG        if( val(jln)%f > 0. ) then
cDBG          if(o>0) write(o,'(2(A,I0),2(A,F0.3),A,F0.6,9A)') '  <state n="',val(jln)%n,'" l="',val(jln)%l,'" f="',val(jln)%f,'" rc="',val(jln)%rc,'" e="',val(jln)%e,'" id="',trim(val(jln)%id),'"/>'
cDBG        else  ! occupied state
cDBG          if(o>0) write(o,'(2(A,I0),A,F0.3,A,F0.6,9A)') '  <state n="',val(jln)%n,'" l="',val(jln)%l,'" rc="',val(jln)%rc,'" e="',val(jln)%e,'" id="',trim(val(jln)%id),'"/>'
cDBG        endif ! occupied state
cDBG      else  ! n in range [1,8]
cDBG        if(o>0) write(o,'(A,I0,A,F0.3,A,F0.6,9A)') '  <state l="',val(jln)%l,'" rc="',val(jln)%rc,'" e="',val(jln)%e,'" id="',trim(val(jln)%id),'"/>'
cDBG      endif ! n in range [1,8]
        else ; nignore = nignore+1
cDBG      if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'ignore state object in line #', nline, ': "', trim(line), '"'
        endif ! valence_states

      ! data part
      case( '<radial_grid' )
        nrg = nrg+1 ! count up number of radial grids
        if( nrg > MAX_NRG ) stop 'pawXML: too many radial grids!'
        irg = nrg ! index of this radial grid

        g(irg)%eq = ( line .get. ' eq=' )          ! equation
        g(irg)%a  = ( line .get. ' a=' ) .get. 0.  ! param1
        g(irg)%b  = ( line .get. ' b=' ) .get. 0.  ! param2
        g(irg)%d  = ( line .get. ' d=' ) .get. 0.  ! param3
        g(irg)%n  = ( line .get. ' n=' ) .get. -1  ! number of grid points
        g(irg)%istart = ( line .get. ' istart=' ) .get.  0 ! start index
        g(irg)%iend = ( line .get. ' iend=' ) .get. -1 ! end index
        g(irg)%id = ( line .get. ' id=' )          ! identity tag

        if( g(irg)%n < 1 ) g(irg)%n = g(irg)%iend - g(irg)%istart + 1 ! g%n has probably not been specified

cFDB    if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'new radial grid object #', irg, ':'
cDBG    if(o>0) write(o,'(3A,3(ES10.3,A),3(I0,A),9A)') '<radial_grid eq="',trim(g(irg)%eq),'" a="',g(irg)%a,'" b="',g(irg)%b,'" d="',g(irg)%d,'" n="',g(irg)%n,'" istart="',g(irg)%istart,'" iend="',g(irg)%iend,'" id="',trim(g(irg)%id),'"/>'
        max_gn = max( max_gn, g(irg)%n ) ! search global maximum
cFDB    if(o>0) write(o,'(3A,I0)') sym, fun, 'max. number of grid points is ', max_gn

      ! a1-quantities
      case( '<ae_core_density' ) ; i_qnt = I_AEC
      case( '<pseudo_core_density' ) ; i_qnt = I_PSC
      case( '<pseudo_valence_density' ) ; i_qnt = I_PSV
      case( '<zero_potential' ) ; i_qnt = I_ZRO
      case( '<kresse_joubert_local_ionic_potential' ) ; i_qnt = I_KJP
      case( '<ae_core_kinetic_energy_density' ) ; i_qnt = I_AEK
      case( '<pseudo_core_kinetic_energy_density' ) ; i_qnt = I_PSK
      case( '<shape_function' )
        shape_function_type = ( line .get. ' type=' )
        shape_function_rc = ( line .get. ' rc=' ) .get. .0 ! cutoff radius
cDBG    if(o>0) write(o,'(3A,F0.3,9A)') '<shape_function type="',trim(shape_function_type),'" rc="',shape_function_rc,'"/>'
        grid_id = ( line .get. ' grid=' ) ! grid, only if given numerically
        if( grid_id /= '' ) i_qnt = I_SHP ! shape function given numerically

      ! a2-quantities
      case( '<ae_partial_wave' ) ; i_qnt = I_AEP
      case( '<pseudo_partial_wave' ) ; i_qnt = I_PSP
      case( '<projector_function' ) ; i_qnt = I_PRF

      case( '<kinetic_energy_differences>' )
        if( nln > 0 ) then
cDBG      if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'expect ',nln,' x ',nln,' elements for dKin!'
          allocate( dkin(nln,nln), stat=ios )
          if( ios /= 0 ) then
            if(o>0) write(o,'(9A)') sym, fun, 'allocation of dKin failed!'
            return
          endif ! ios /= 0
          dkin = 0.
          read( unit=U, fmt=*, iostat=ios ) dkin
          if( ios /= 0 ) then
            if(o>0) write(o,'(4A,I3)') sym, fun, ERROR, 'failed to read <kinetic_energy_differences> ios =', ios
            return ! error
          endif ! ios /= 0
cFDB      if(o>0) write(o,'(9A)') sym, fun, 'dKin:' ! show values
cFDB      do jln = 1, nln
cFDB        if(o>0) write(o,'(99F10.6)') dkin(:,jln)
cFDB      enddo ! jln
cDBG    else  ! nln > 0
cDBG      if(o>0) write(o,'(9A)') sym, fun, 'expect no elements for dKin!'
        endif ! nln > 0
      case( '<exact_exchange_X_matrix>' ) ; i_qnt = I_IGNORE_LINE ! ignore the next line
      case( '<exact_exchange' ) ! ignore
      case( '<' ) ! close an xml tag with </tag>, but Fortran read will understand new record from '/'
        if( valence_states ) then 
          valence_states = .false. ! switch off
        elseif( generator ) then
          generator = .false.
cDBG      if(o>0) write(o,'(9A)') '<generator type="', trim(generator_type), '" name="', trim(generator_name), '"> ', trim(generator_content), ' </generator>'
        else
          ! some other xml-quantity ends here
        endif
      case( '<!--' ) ! xml comment
      case default 
        if( generator ) then
          generator_content = adjustl(line)
cFDB      if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'expect ',nln,' valence states.'
        else
          nignore = nignore+1
cFDB      if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'ignore line #', nline, ' line reads "', trim(line(1:64)), '..."'
        endif ! generator 
      endselect ! xml_object



      if( i_qnt > 0 ) then

        grid_id = ( line .get. ' grid=' ) ! grid
        if( grid_id == '' ) then
          if(o>0) write(o,'(9A)') sym, fun, ERROR, trim(xml_object), '> specifies no grid.'
          return ! error
        endif ! no grid

        ios = my_allocate( ) ! allocate arrays, if still not allocated
        if( ios /= 0 ) return ! error

        ! find the grid index
        irg = 0 ! init result as 0: not found
        do i = 1, nrg
          if( g(i)%id == grid_id ) irg = i
        enddo ! i
        if( irg < 1 ) then
          if(o>0) write(o,'(9A)') sym, fun, ERROR, 'for ', trim(xml_object), ' grid="', trim(grid_id), '"> no radial grid was found.'
          return ! error
        endif ! irg < 1

      endif ! i_qnt > 0

      selectcase( i_qnt )
      case( 1:3 ) ! partial waves and projectors

        state_id = ( line .get. 'state=' )
        ! find jln index
        if( state_id == '' ) then
          if(o>0) write(o,'(9A)') sym, fun, ERROR, trim(xml_object), '> specifies no state.'
          return ! error
        endif ! no grid

        jln = 0
        do i = 1, nln
          if( val(i)%id == state_id ) jln = i
        enddo ! i
        if( jln < 1 ) then
          if(o>0) write(o,'(9A)') sym, fun, ERROR, 'for ', trim(xml_object), ' state="', trim(state_id), '"> no valence state was found.'
          return ! error
        endif ! jln < 1

        ! store grid index
        g2_index( jln, i_qnt ) = irg
        a2(:,jln,i_qnt) = 0. ! init as zero
        gn = g(irg)%n ! number of radial grid points
        read( unit=U, fmt=*, iostat=ios ) a2(1:gn,jln,i_qnt)
        if( ios /= 0 ) then
          if(o>0) write(o,'(10A,I0)') sym, fun, ERROR, &
            'occured reading data for ', trim(xml_object), ' state="', trim(state_id), '" grid="', trim(grid_id), '">, ios = ', ios
          return ! error
        endif ! reading failed

      case( 4:I_MAX ) ! other functions
        ! store grid index
        g1_index( i_qnt ) = irg
        a1(:,i_qnt) = 0. ! init as zero
        gn = g(irg)%n ! number of radial grid points
        read( unit=U, fmt=*, iostat=ios ) a1(1:gn,i_qnt)
        if( ios /= 0 ) then
          if(o>0) write(o,'(8A,I0)') sym, fun, ERROR, &
            'occured reading data for ', trim(xml_object), ' grid="', trim(grid_id), '">, ios = ', ios
          return ! error
        endif ! reading failed

      case( 0 ) ! no quantity
      case( I_IGNORE_LINE )
        read( unit=U, fmt='(A)', iostat=lios ) line !! read a line
cFDB    if(o>0) write(o,'(9A)') sym, fun, 'ignore line "', trim(line(1:64)), '..."'

cDBG  case default ; stop 'XML: ERROR: i_qnt not in [-1,I_MAX]!'
      endselect ! i_qnt

      line = '' ! init
      read( unit=U, fmt='(A)', iostat=lios ) line !! read nxt line
    enddo ! while lios == 0
    ! end read loop

    close( unit=U, iostat=ios )

    ! warn for ignored lines
    if( nignore > 0 .and. o>0) write(o,'(3A,I0,9A)') sym, fun, WARNING(0), nignore, ' lines in "', trim(filename), '" have been ignored.'

    if( nrg < 1 ) then
      if(o>0) write(o,'(9A)') sym, fun, ERROR, 'no radial grid found in "',trim(filename),'".'
      ios = nrg-1 ; return ! error
    endif ! no radial grid

    if( any( g2_index(1:nln,:) < 1 .or. g2_index(1:nln,:) > nrg ) ) then
      if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'some partial wave or projectors did not define the grid_id properly.'
    endif ! wrong indices of radial grids

    ! find the index of the radial grid with the largest n
    irg = 0
    do i = 1, nrg
      if( g(i)%n == max_gn ) irg = i
    enddo ! i

cDBG  if(o>0) write(o,'(9A)') sym, fun, 'reading of "', trim(filename), '" successfull.'

    !! radial grids
    !! eq                 parameters
    !!-------------------------------
    !! r=a*exp(d*i)       a and d
    !! r=a*i/(1-b*i)      a and b
    !! r=a*i/(n-i)        a and n
    !! r=a*(exp(d*i)-1)   a and d
    !! r=d*i              d
    !! r=(i/n+a)^5/a-a^4  a and n
    s%g = rgrid_eqn( g(irg)%eq, n=g(irg)%n, a=g(irg)%a, d=g(irg)%d, b=g(irg)%b )

      ! this plot works only, if all quantities are on a similar grid (number g%n may differ)
cDBG  write(77,'(99A)') '# r, ae_core_density, pseudo_core_density' &
cDBG  ,', zero_potential, shape_function, pseudo_valence_density' &
cDBG  ,', Kresse_Joubert_local_ionic_potential', ( ( ', ', trim(val(jln)%id), jln=1,nln ), i=1,3 )
cDBG  do ir = 1, max_gn
cDBG    write(77,'(99ES16.6E3)') s%g .at. ir, a1(ir,4:I_MAX-1), a2(ir,1:nln,1:3)
cDBG  enddo ! ir

    !!
    !! start processing the quantities
    !!

    s%iZ = atom_Z
    s%nve = real(atom_valence)
    s%nce = atom_core
    s%Eatom = ae_energy_total
    s%e_core_kin = core_energy_kinetic
    s%ellmax = -1 ! init
    nn = 0 ! init
    do i = 1, nln
      ell = val(i)%l
      if( ell < 0 ) stop 'XML: ell < 0 unphysical!'
      if( ell > ELLMAX ) stop 'XML: ell higher than implemented!'
      nn(ell) = nn(ell)+1
      if( nn(ell) > ENNMAX ) stop 'XML: enn higher than implemented!'
      s%ellmax = max( s%ellmax, ell )
    enddo ! mln
cDBG  if(o>0) write(o,'(3A,9I2)') sym, fun, 'nn =', nn(0:s%ellmax)
    s%nn = nn

    ! count the number of partial waves and projectors
    s%mln = 0 ! init
    do ell = 0, s%ellmax
      s%mln = s%mln+s%nn(ell)
    enddo ! ell
    s%mlm = (s%ellmax+1)**2

    ! find the ordering for iln-states
    nn = 0 ! init again
    do i = 1, nln
      ell = val(i)%l
      enn = nn(ell)+1
      iln_list(i) = sum( s%nn(0:ell-1) ) + enn
      nn(ell) = enn ! increased
    enddo ! mln
cDBG  if(o>0) write(o,'(3A,99(" ",I0))') sym, fun, 'iln indices =', iln_list(1:nln)


    s%g%imx = max_gn-1
    allocate( s%rwf(0:s%g%imx,s%mln,1,RWF_DIMENSION), &
              s%rf(0:s%g%imx,RF_DIMENSION), &
              s%dkin(s%mln,s%mln), &
              stat=ios )
    if( ios /= 0 ) then
      if(o>0) write(o,'(9A)') sym, fun, ERROR, 'allocation of species pointers s%RWF, s%RF and s%DKIN failed!'
      return
    endif ! ios /= 0
    s%rf   = 0.
    s%rwf  = 0.
    s%dkin = 0.

    s%ene_ln = 0. ! init
    s%occ_ln = 0. ! init
    do i = 1, nln
      iln = iln_list(i)
      if( iln > 0 ) then
        s%ene_ln(1:2,iln) = val(i)%e ! energy
        s%occ_ln(1:2,iln) = val(i)%f/2. ! occupation, distribute equally to both spins
        ell = val(i)%l
        ! transfer data
        s%rwf(:,iln,1,I_TRU) = a2(:,i,I_AEP) ! ae_partial_wave
        s%rwf(:,iln,1,I_SMT) = a2(:,i,I_PSP) ! pseudo_partial_wave
        s%rwf(:,iln,1,I_PRJ) = a2(:,i,I_PRF)/s%g%r**ell ! projector function
        s%rwf(0,iln,1,I_PRJ) = s%rwf(1,iln,1,I_PRJ) ! extrapolate 0-th order
        ! kinetic energy deficit
        do j = 1, nln
          jln = iln_list(j)
          if( jln < 1 ) cycle
          s%dkin(jln,iln) = dKin(j,i)
        enddo ! j
cDBG  else  ! iln > 0
cDBG    if(o>0) write(o,'(9A)') sym, fun, '<state id="', trim(val(i)%id), '"> has been ignored!'
      endif ! iln > 0
    enddo ! i

    ! this plot works only, if all quantities are on a similar grid (number g%n may differ)
cDBG  write(88,'(99A)') '# r, projectors'
cDBG  do ir = 0, s%g .at. 5.0
cDBG    write(88,'(99F16.6)') s%g .at. ir, s%rwf(ir,:,1,I_PRJ)
cDBG    write(86,'(99F16.6)') s%g .at. ir, s%rwf(ir,:,1,I_SMT)
cDBG    write(84,'(99F16.6)') s%g .at. ir, s%rwf(ir,:,1,I_TRU)
cDBG  enddo ! ir

    s%rf(:,I_RHOC+I_TRU) = a1(:,I_AEC) ! ae_core_density
    s%rf(:,I_RHOC+I_SMT) = a1(:,I_PSC) ! pseudo_core_density
    s%rf(:,I_VBAR)       = a1(:,I_ZRO) ! zero_potential

    ! manipulate
    !s%nWexp = 7
    !if(o>0) write(o,'(3A,I0)') sym, fun, 'set s%nWexp to ', s%nWexp

    s%rcut = maxval( val(:)%rc )
    s%nr = s%g .at. s%rcut
cDBG  if(o>0) write(o,'(3A,I0,A,F0.3,9A)') sym, fun, 'set Nr = ', s%Nr, '  Rcut = ', s%rcut*Ang, Ang_

    ios = 0
cDBG  if(o>0) write(o,'(3A,I0)') sym, fun, 'status = ', ios

  contains

    integer function my_allocate( ) result( ist )
      if( .not. allocated( a1 ) ) then
cDBG    if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'expect a max. of ', max_gn, ' radial grid points!'
        allocate( a1(max_gn,1:I_MAX), stat=ist )
cDBG    if( ist /= 0 .and. o>0) write(o,'(9A)') sym, fun, ERROR, 'allocation of a1 failed!'
        a1 = 0. ! init
      endif ! allocated
      if( .not. allocated( a2 ) ) then
cDBG    if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'expect a max. of ', max_gn, ' radial grid points!'
cDBG    if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'expect ', nln, ' valence states!'
        allocate( a2(max_gn,nln,1:3), stat=ist )
cDBG    if( ist /= 0 .and. o>0) write(o,'(9A)') sym, fun, ERROR, 'allocation of a2 failed!'
        a2 = 0. ! init
      endif ! allocated
    endfunction ! my_allocate

  endfunction ! read_paw_XML_file

  !! read an attribute given in double quotes, so line contains ' keyw="word" '
  string_t function get_attribute( line, key ) result( word )
    ! parameter
    character(len=*), parameter           :: fun = ' get_attribute: '
    character(len=*), parameter           :: ATTR_CHAR = '"'
    ! arguments
    character(len=*), intent(in)          :: key
    character(len=*), intent(in)          :: line
    ! local vars
    integer                               :: i0, i1, i2
cFDB   iounit_t, parameter :: u = 9 ! write to fort.9

    word = '' ! for early return
    i0 = index( line, key ) ! returns the starting position for key within line
    if( i0 <  1 ) return ! not found
cFDB   if(u>0) write(u,'(3A,I0,9A)') sym, fun, 'position of key    is ',i0,' --> ', line(i0:i0+len(key)-1)
    i1 = index( line(i0+1:), ATTR_CHAR ) + i0 ! return the position of the 1st double quotation mark
    if( i1 < i0 ) return ! no 1st double quotation mark in line
cFDB   if(u>0) write(u,'(3A,I0,9A)') sym, fun, 'position of 1st qm is ',i1,' --> ', line(i1:i1)
    i2 = index( line(i1+1:), ATTR_CHAR ) + i1
    if( i2 < i1 ) return ! no 2nd double quotation mark in line
cFDB   if(u>0) write(u,'(3A,I0,9A)') sym, fun, 'position of 2nd qm is ',i2,' --> ', line(i2:i2)
    word = adjustl(line(i1+1:i2-1)) ! only the part in double quotation marks

  endfunction ! get

  !! read an integer value from a string, if it failes, use the default value
  integer function get_value_i( str, def ) result( val )
    ! arguments
    character(len=*), intent(in)          :: str
    integer, intent(in)                   :: def ! default value
    ! local vars
    status_t                              :: ios
    val = 0
    read( unit=str, fmt=*, iostat=ios ) val
    if( ios /= 0 ) val = def ! set default
  endfunction ! get

  !! read a real value from a string, if it failes, use the default value
  real function get_value_r( str, def ) result( val )
    ! arguments
    character(len=*), intent(in)          :: str
    real, intent(in)                      :: def ! default value
    ! local vars
    status_t                              :: ios
    val = 0.
    read( unit=str, fmt=*, iostat=ios ) val
    if( ios /= 0 ) val = def ! set default
  endfunction ! get

#ifdef EXTENDED
!+ extended

  status_t function test()
    use type_species, only: species
    type(species) :: s
    test = read_paw_XML_file( "Ar.paw", s )
  endfunction ! test

!- extended
#endif

endmodule ! paw_XMLfile

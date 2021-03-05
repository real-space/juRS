#include "config.h"

! #define DEBUG
! #define FULL_DEBUG

#ifdef DEBUG
#define cDBG
#ifdef FULL_DEBUG
#define cFDB
#else
#define cFDB !FDB
#endif
#else
#define cDBG !DBG
#define cFDB !FDB
#endif


!! @author Paul Baumeister
!! @version 4.04
!!
!! reads the input file and checks consistency of various params
module init
  use type_item, only: item
  use constants, only: SOLVER_NONE, SOLVER_SUBSROT, SOLVER_CONGRAD, SOLVER_DAVIDSO, SOLVER_DESCENT, SOLVER_RMMDIIS, SOLVER_XPLICIT
  use constants, only: SOLVER_SCHEME_SPEED, SOLVER_SCHEME_STABLE ! default is speed
  use type_grid, only: BC_MIRROR, BC_FINITE, BC_PERIODIC, BC_DEFAULT
  use mixing, only: MixrDict => Dictionary
  use symmetry, only: SymmDict => Dictionary
  use Poissonsolver, only: PoisDict => Dictionary
  use density_functionals, only: XCfnDict => Dictionary
implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: fun = ': ', sym = 'INI' !! module symbol

  public :: initialize
  public :: example_input_file
#ifdef EXTENDED
  public :: test ! self-test of the derived spherical harmonics
#endif  

  type(item), parameter :: SpinDict(15)= (/ &
    item('auto',0), &
    item('integrated',1), item('paired',1), item('no',1), item('0',1), item('1',1), & ! spin 1 is integrated for downwards compatibility
    item('polarized',2), item('2',2), item('up&dn',2), item('',2), item('yes',2), &
    item('non-collinear',4), item('noco',4), item('3',4), item('4',4) /)

  type(item), parameter :: BcndDict(17)= (/ &
    item('isolated',BC_FINITE), item('iso',BC_FINITE), item('finite',BC_FINITE), &
    item('0',BC_FINITE), item('off',BC_FINITE), item('cluster',BC_FINITE), item('molecule',BC_FINITE), &
    item('periodic',BC_PERIODIC), item('peri',BC_PERIODIC), &
    item('1',BC_PERIODIC), item('on',BC_PERIODIC), item('bulk',BC_PERIODIC), item('crystal',BC_PERIODIC), &
    item('mirror',BC_MIRROR), item('|',BC_MIRROR), &
    item('default',BC_DEFAULT), item('',BC_DEFAULT) /)

  type(item), parameter :: BoolDict(13)= (/ &
    item('true',1), item('T',1), item('t',1), item('1',1), item('yes',1), item('on',1), &
    item('false',0), item('F',0), item('f',0), item('0',0), item('no',0), item('off',0), item('none',0) /)

  type(item), parameter :: SolvDict(17)= (/ &
    item('diis',SOLVER_RMMDIIS), item('rmm_diis',SOLVER_RMMDIIS), &
    item('sr',SOLVER_SUBSROT), item('subspace_rotation',SOLVER_SUBSROT), &
    item('sd',SOLVER_DESCENT), item('steepest_descent',SOLVER_DESCENT), &
    item('cg',SOLVER_CONGRAD), item('conjugate_gradients',SOLVER_CONGRAD),&
    item('explicit',SOLVER_XPLICIT), &
    item('speed',SOLVER_SCHEME_SPEED), item('scheme_speed',SOLVER_SCHEME_SPEED), &
    item('stable',SOLVER_SCHEME_STABLE), item('scheme_stable',SOLVER_SCHEME_STABLE), &
    item('default',SOLVER_RMMDIIS), item('',SOLVER_RMMDIIS), &
    item('none',SOLVER_NONE) , &
    item('dav',SOLVER_DAVIDSO) /)

  integer, parameter :: KEY_recipe_DEFAULT = 2 ! 'light'

  type(item), parameter :: RcpiDict(7)= (/ &
    item('really_light',1), &
    item('light',2), &
    item('right',3), &
    item('tight',4), &
    item('really_tight',5), &
    item('default',KEY_recipe_DEFAULT), &
    item('',KEY_recipe_DEFAULT) /)

  contains

  !! move this routine to module configuration
  status_t function example_input_file( filename ) result( ios )
  use type_item, only: operator(.in.)
  use configuration, only: o, &
    CommentChars, &
    KeyWordDict, &
    I_KeyWord_CELL, &
    I_KeyWord_SCAL, &
    I_KeyWord_RCPI, &
    I_KeyWord_BCND, &
    I_KeyWord_DOMN, &
    I_KeyWord_MIXR, &
    I_KeyWord_CMNT, &
    I_KeyWord_KMSH, &
    I_KeyWord_APOS, &
    BlockKeyWord_Atoms, &
    BlockKeyWord_Afrac, &
    CodeVersion, &
    CodeName
    character(len=*), intent(in)            :: filename

    character(len=*), parameter             :: fun = ' example_input_file: '
    iounit_t, parameter                     :: u = 14
    character, parameter                    :: cc = CommentChars(1)

    open( unit=u, file=filename, action='write', status='unknown', iostat=ios )
    if( ios /= 0 ) then
      if(o>0) write(o,'(9A)') sym, fun, 'cannot open file "', trim(filename), '".'
      return
    endif ! ios /= 0
    write(u,'(2A/5A/99(2A/))') &
      cc, ' --------------------------------------', &
      cc, ' ', CodeName, ' v', CodeVersion(), &
      trim(I_KeyWord_CMNT.in.KeyWordDict), ' example input file', &
      cc, ' --------------------------------------', &
      cc, ' all coordinates are given xyz-order', &
      cc, ' keywords may be in arbitrary order', &
      cc, ' --------------------------------------'
    write(u,'(/2A/9A)') cc, ' extend of the rectangular cell (non-optional)', trim(I_KeyWord_CELL.in.KeyWordDict), ' 12. 12. 12.'
    write(u,'(/2A/9A)') cc, ' scale the cell by a factor', trim(I_KeyWord_SCAL.in.KeyWordDict), ' Ang'
    write(u,'(/2A/9A)') cc, ' specify the convergence parameter set', trim(I_KeyWord_RCPI.in.KeyWordDict), ' light'
    write(u,'(/2A/9A)') cc, ' boundary condition for each direction', trim(I_KeyWord_BCND.in.KeyWordDict), ' peri peri iso'
    write(u,'(/2A/9A)') cc, ' MPI tasks in domain decomposition', trim(I_KeyWord_DOMN.in.KeyWordDict), ' 1 1 1'
    write(u,'(/2A/9A)') cc, ' mixing strategy', trim(I_KeyWord_MIXR.in.KeyWordDict), ' 0.3 straight'
    write(u,'(/2A/9A)') cc, ' use an automatically generated k-point mesh', trim(I_KeyWord_KMSH.in.KeyWordDict), ' 1 1 1'
    write(u,'(/2A/A/A/A/)') cc, ' list of atoms in fractional coordinates', BlockKeyWord_AFRAC, 'H  0 -1:8 -0.16', BlockKeyWord_AFRAC
    write(u,'(/2A/A/2A/A/)') cc, ' list of atoms in absolute coordinates', BlockKeyWord_ATOMS, cc, 'H  0  0. 0:5', BlockKeyWord_ATOMS
    write(u,'(/2A/9A)') cc, ' scale the abolute atomic coordinates by a factor', trim(I_KeyWord_APOS.in.KeyWordDict), ' Ang'
    write(u,'(/2A/A/A/A/)') cc, ' self-defined variables with $ or @', '$x = 3.44'
    write(u,'(/2A/2A)') cc, ' list of atoms with more specification', cc, ' Manganese, T F T:no relaxation in y-direction, -1:flip initial magnetic moment'
    write(u,'(A/2A/A/)') BlockKeyWord_ATOMS, cc, 'Mn  $x 0. 0.  T F T  -1', BlockKeyWord_ATOMS
    write(u,'(/2A/)') cc, ' run with --CheckMode first'
    close( unit=u )

    if(o>0) write(o,'(9A)') 'example_input_file written to "', trim(filename), '".'
  endfunction ! example_input_file


  !! check if a real value equals its integer part
  logical function is_integer( r )
    real, intent(in) :: r
    is_integer = ( real(nint(r)) == r )
  endfunction ! is_integer


  !! main input routine:
  !! --- opens the input file
  !! --- reads the lines in the input file searching for keywords
  !! --- searches the lines passed via tha command line for keywords
  !! --- read block structures (atomic positions, kpoints...)
  !! --- checks consistency of input
  !!     --- check if kpoints are commensurate with BoundaryConditions
  !!     --- check if atomic position are okay with cell and boundary
  !! --- gives warnings
  status_t function initialize( CheckMode, infile, override, comm, &
                               bc, cellsize, atoms, speci, &
                               nbands, nspins, addcharge, kpoints, kpath, ngps, nscale, nxyzproc, &
                               mixingratio, key_mixing, nhistory, temperature, key_Poisson, &
                               key_symmetry, key_functional, key_solver, efield, hfield, &
#ifdef GENERAL_CELL
                               shifts, &
#endif
                               scf, wfs, pss, mdi, frc, doublegrid, finitediff, pdos, pawdatapath ) &
                               result( ist )
  use configuration, only: o
  use configuration, only: MaxInputFileNameLen, MaxInputFileLineLen, MaxInputBufferLines
  use configuration, only: WARNING, ERROR, STOPonERROR
  use input, only: open_input_file
  use type_item, only: operator(.in.), operator(.alt.)
  use type_item, only: min_key, max_key
  use configuration, only: KeyWordDict ! dictionary (item-array) containing keywords in long and short versions
  use configuration, only:   I_KeyWord_CELL, &
                             I_KeyWord_HGRD, &
                             I_KeyWord_NGPS, &
                             I_KeyWord_DOMN, &
                             I_KeyWord_SPIN, &
                             I_KeyWord_APOS, &
                             I_KeyWord_SOLV, &
                             I_KeyWord_PDOS, &
                             I_KeyWord_BCND, &
                             I_KeyWord_XCFN, &
                             I_KeyWord_UNKNOWN
  use configuration, only: ParallelizeBands
  use input, only: readblock ! reads block lists
  use configuration, only: BlockKeyWord_Atoms, & ! block key words
                           BlockKeyWord_Afrac, &
                           BlockKeyWord_Kpnts, &
                           BlockKeyWord_Kpath
  use input, only: showenv ! show all environment variables
  use type_species, only: species, species_set
  use type_criteria, only: criteria
  use unitsystem, only: Ang, Ang_, eV, eV_
  use type_element, only: Z_MAXIMUM, Z_MINIMUM
  use type_atom, only: TYPE_ATOM_POS_ABSOLUTE, TYPE_ATOM_POS_FRACTIONAL
  use type_atom, only: atom, atom_set
  use type_kpoint, only: kpoint, kpoint_set
  use symmetry, only: generate_kpoint_set
#ifdef GENERAL_CELL
  use type_grid, only: BC_SHIFTED
#endif
  ! use symmetry, only: check_atomic_fractional_positions
  use MPItools, only: MPIparallel, MPImaster, MPIbcast0
    integer, intent(in)                     :: CheckMode    !! if CheckMode > 0, more checks are performed
    character(len=*), intent(in)            :: infile       !! input file name
    character(len=*), intent(in)            :: override(1:,1:) !! lines passed with --override or --preset to the command line
    MPI_Comm, intent(in)                    :: comm         !! MPIcommunicator
    integer, intent(out)                    :: bc(3,2)      !! physical boundary condition
    real, intent(out)                       :: cellsize(3)  !! rectangular size of the cell
    type(atom), allocatable, intent(out)    :: atoms(:)     !! *all* atoms
    type(species), allocatable, target, intent(out) :: speci(:) !! species, that the atoms point to
    integer, intent(out)                    :: nbands       !! number of bands
    integer, intent(out)                    :: nspins       !! number of spins
    real, intent(out)                       :: addcharge    !! additional charge in the cell
    type(kpoint), allocatable, intent(out)  :: kpoints(:)   !! list of kpoints
    type(kpoint), allocatable, intent(out)  :: kpath(:)     !! path edges of a kpoint-path
    integer, intent(out)                    :: ngps(3)      !! number of grid points xyz
    integer, intent(out)                    :: nscale       !! refinement of the dense grid over coarse grid
    integer, intent(out)                    :: nxyzproc(3)  !! number of processes xyz (DomainDecomposition)
    real, intent(out)                       :: mixingratio  !! for straight mixing
    integer, intent(out)                    :: key_mixing   !! mixing method
    integer, intent(out)                    :: nhistory     !! mixing history max. number of steps
    real, intent(out)                       :: temperature  !! smearing temperature
    integer, intent(out)                    :: key_Poisson  !! Poisson solver method
    integer, intent(out)                    :: key_symmetry !!
    integer, intent(out)                    :: key_functional !! Density Functional
    integer, intent(out)                    :: key_solver   !! eigenvalue problem solver
    real, intent(out)                       :: efield(1:3)  !! electrical field
    real, intent(out)                       :: hfield(1:3)  !! magnetic field
#ifdef GENERAL_CELL
    real, intent(out)                       :: shifts(3)    !!
#endif
    type(criteria), intent(out)             :: scf          !! convergence criteria for the self-consistency
    type(criteria), intent(out)             :: wfs          !! convergence criteria for the wave functions
    type(criteria), intent(out)             :: pss          !! convergence criteria for the Poisson equation
    type(criteria), intent(out)             :: mdi          !! convergence criteria for molecular dynamics
    logical, intent(out)                    :: frc(3)       !! switch for forces xyz
    integer, intent(out)                    :: doublegrid(:) !! DoubleGrid parameters (itp,msh,spare)
    integer, intent(out)                    :: finitediff(:) !! FiniteDifference parameters (nf_kinetic,nf_gradients,nf_poisson)
    character(len=*), intent(out)           :: pdos(:)      !! switch for projected DoS plots
    character(len=*), intent(out)           :: pawdatapath  !! path where to find the pawdata files

cDBG  character(len=*), parameter             :: fun = ' initialize: '
    iounit_t, parameter                     :: unt = 12
    logical, parameter                      :: AT_LEAST_ONE_ATOM = .FALSE.
    character(len=1), parameter             :: DIR(1:3) = (/'x','y','z'/)
    real, parameter                         :: MIN_VACUUM = 2.0 ! a.u.  ! to the vacuum
    real, parameter                         :: MIN_DISTANCE = 1.0 ! a.u. ! of two atoms
    character(len=*), parameter             :: WARN = 'Warning! '
    integer, parameter                      :: SET_DEFAULTS = -999
    integer, parameter                      :: SET_RECIPES = -777

    integer                                 :: key_recipe  !! automatic parameters
    !================================================================================
    integer                                 :: element_count(Z_MINIMUM:Z_MAXIMUM)
    integer                                 :: speci_index  (Z_MINIMUM:Z_MAXIMUM)

    ! boundary conditions
    integer                                 :: general_bc(1:3)
    integer                                 :: nmirror

    ! blocks
    real, allocatable                       :: atmpos_a(:,:) ! absolute coordinates
    real, allocatable                       :: atmpos_f(:,:) ! fractional coordinates
    real, allocatable                       :: atmpos(:,:) ! all atoms
    real, allocatable                       :: kpathpos(:,:) ! all edges of the kpath
    integer, allocatable                    :: atminfo(:,:)
    real                                    :: apos(1:3), fpos(1:3)
    real, allocatable                       :: kptpos(:,:) !
    real, allocatable                       :: newkpt(:,:) !
    real                                    :: kpt(0:3), delta_k2, weightsum
    real                                    :: masscenter(0:3)
    ! origin of atomic coordinates in fractions of the cell
    real                                    :: origin(1:3)

    ! counters
    integer                                 :: iat, ja
    integer                                 :: ik, nk, mk, ik1, ik2
    integer                                 :: iZ, isp, nspecies, iov
    ! integer quantities
    integer                                 :: matoms   = 0, ma
    integer                                 :: matoms_a = 0
    integer                                 :: matoms_f = 0
    integer                                 :: mkpnts   = 0
    integer                                 :: mkpath   = 0
    string_t                                :: atom_label
    integer                                 :: ipdos = 0

    ! line processing
    character(len=MaxInputFileLineLen)      :: c_line ! current line
    character(len=MaxInputFileLineLen)      :: b_line(MaxInputBufferLines) ! line buffer
    integer                                 :: n_line(MaxInputBufferLines) ! line number
    integer                                 :: nbl, ibl, irun
    real                                    :: rbuf

    integer                                 :: nline
    status_t                                :: ios, lios

    integer, allocatable                    :: kc(:) ! count how many times a keyword is mentioned

    integer                                 :: kmesh(3) = 0   ! automatic k-mesh
    real                                    :: kshift(3) = 0. ! shift of k-mesh
    real                                    :: cell_scale = 1.0 ! scale factor for cell extends
    real                                    :: apos_scale = 1.0 ! scale factor for absolute positions
    integer                                 :: nsym
    integer                                 :: id ! spatial directions

    ! forces
    integer                                 :: ifrc(3) = 1 ! default is on

    integer :: nwarn_close2isoboundary(1:3), nwarn_mirrorbutnegative(1:3)
    integer :: nwarn_close2mirrorboundary(1:3), ninfo_backfold4periodic(1:3)

    integer                                 :: nempty, ncommented, nignored
    character(len=MaxInputFileLineLen)      :: last_ignored_line
    integer                                 :: last_ignored_linenumber

    real                                    :: gridspacing, h(3), ani(3) ! grid spacing and anisotropy
    logical                                 :: MPIp, MPIm ! MPIparallel, MPImaster

cDBG  logical                               :: is_int, k_adj
!     if( CheckMode > 0 ) o = output


    deallocate( atmpos_a, atmpos_f, atmpos, kpathpos, atminfo, kptpos, newkpt, kc, stat=ist )
    deallocate( atoms, speci, kpoints, kpath, stat=ist )


    ! master process opens the input file
    ist = open_input_file( inunit=unt, infile=infile, MPIcommunicator=comm )
    if( ist /= 0 .and. o>0) write(o,'(9A)') sym, fun, ERROR, 'unable to open input file "', trim(infile), '".'
    if( ist /= 0 .and. STOPonERROR ) stop 'INIT: input file could not be opened successfully.'

    ! set default values and configurations
    ist = read_keywords( line='', linenumber=SET_DEFAULTS )

    allocate( kc(I_KeyWord_UNKNOWN:max_key(KeyWordDict)), stat=ist )
    kc = 0 ! init counter how many times a keyword is mentioned: 0: use defaults, 1: no Warning, >1: launch WARNINGS

cDBG  if(o>0 .and. size(override)>0) write(o,'(3A,9(I0,A))') sym, fun, 'read ', size(override), ' input lines from the command line.'
    do iov = 1, size(override,1)
      if( override(iov,1) == '' ) cycle
cDBG  if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'read preset line #',iov
      if(o>0) write(o,'(9A)') sym, fun, 'read preset line "', trim(override(iov,1)), '".'
      !====================================================================
      !=== read from the command line ( --preset "..." ) ================
      !====================================================================
      ist = read_keywords( line=override(iov,1), linenumber=-iov )
      ! the negative line number indicates command line input
      !====================================================================
      if( ist /= 0 .and. STOPonERROR ) stop 'INIT: keyword in wrong context (command line with predefs).'
    enddo ! iov


    MPIm = MPImaster( comm ) ! master process
    MPIp = MPIparallel( comm ) ! more than one process running

    nline = 0 ! init the global line number counter
    ! read the entire line (therefore fmt='(A)')
    rbuf = 0. ! init number of times the buffer is used
    nempty = 0 ; ncommented = 0 ; nignored = 0 ! number of empty lines
cDBG  if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'buffered reading with ', MaxInputBufferLines, ' lines.'
    ! buffered reading mode:
    nbl = MaxInputBufferLines ! while loop runs at least once
    do while( nbl == MaxInputBufferLines )

cDBG  if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'start buffer!'
      ! master fills the buffer from the input file
      nbl = 0 ! init buffer line counter
      if( MPIm ) then ! master task

        ibl = 0 ! init buffer line index
        b_line = '' ! init buffer
        lios = 0
        do while( nbl < MaxInputBufferLines .and. lios == 0 )
          ibl = ibl+1
          c_line = '' ! init
          !====================================================================
          !=== read from the input file =======================================
          !====================================================================
          nline = nline+1 ! count up the global line number counter
          read( unit=unt, fmt='(A)', iostat=lios ) c_line !! read line from the input file
          !====================================================================
          if( lios == 0 ) then
            if( c_line == '' ) then
cFDB          if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'line #',nline,' is empty and will be ignored.'
              nempty = nempty+1 ! count up the number of ignored empty lines
            else  ! line empty
              nbl = nbl+1 ! count the number of lines in this buffer where reading was successful
              b_line(nbl) = c_line ! store the line itself
              n_line(nbl) = nline ! store the number of the line in the input file
! cFDB        if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'now ',nbl,' lines in the buffer.'
            endif ! line empty
cDBG      else ; if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'io failed for line #',nline, ', ',nbl,' lines are in the buffer.'
          endif ! line io successful
        enddo ! while

cDBG    if(o>0 .and. nbl == MaxInputBufferLines) write(o,'(3A,9(I0,A))') sym, fun, 'found ',nbl,' lines in the buffer (full).'
cDBG    if(o>0 .and. nbl  < MaxInputBufferLines) write(o,'(3A,9(I0,A))') sym, fun, 'found ',nbl,' lines in the buffer.'
      endif ! MPIm(aster)

      ! communication
      if( MPIp ) then
        call MPIbcast0( nbl, comm ) ! send one integer containing the size
cDBG    if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'broadcast ',nbl,' buffer lines.'
        if( nbl > 0 ) call MPIbcast0( b_line(1:nbl), comm ) ! send buffered lines
        if( nbl > 0 ) call MPIbcast0( n_line(1:nbl), comm ) ! send buffered line numbers
      endif ! MPIp(arallel)

      do ibl = 1, nbl
! cFDB  if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'process line #',n_line(ibl),' of the buffer.'
        !====================================================================
        !=== processing of the lines ========================================
        !====================================================================
        ist = read_keywords( line=b_line(ibl), linenumber=n_line(ibl) )
        !====================================================================
        if( ist /= 0 .and. STOPonERROR ) stop 'INIT: keyword in wrong context (input file).'
      enddo ! ibl

      ! the buffer was completely filled last time, so
      ! it is likely that more lines will follow, therefore restart the loop
cDBG  if( nbl == MaxInputBufferLines .and. o>0) write(o,'(3A,9(I0,A))') sym, fun, 'buffer was completely full, restart.'
      rbuf = rbuf + real(nbl)/real(MaxInputBufferLines) ! measure buffer usage
    enddo ! while( nbl == MaxInputBufferLines )
    if(MPIm .and. o>0) write(o,'(2A,I0,9A)') sym, fun, nempty,' empty lines were ignored.'
    if(o>0) write(o,'(2A,F0.2,A,I0,9A)') sym, fun, rbuf,' x ',MaxInputBufferLines,' non-empty lines of the input file were read.'


cDBG  if( size(override,2) < 2 ) stop 'INIT: array override should have a dim#2 of 2.'
    do iov = 1, size(override,1)
      if( override(iov,2) == '' ) cycle
cDBG  if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'read override line #',iov
      !====================================================================
      !=== read from the command line ( --override "..." ) ================
      !====================================================================
      if(o>0) write(o,'(9A)') sym, fun, 'read override line "', trim(override(iov,2)), '".'
      ist = read_keywords( line=override(iov,2), linenumber=-iov )
      ! the negative line number indicates command line input
      !====================================================================
      !====================================================================
      if( ist /= 0 .and. STOPonERROR ) stop 'INIT: keyword in wrong context (command line with overrides).'
    enddo ! iov

    ist = read_keywords( line='', linenumber=SET_RECIPES )

cDBG  if( ncommented > 0 .and. o>0) write(o,'(2A,9(I0,A))') sym, fun, ncommented,' lines were commented out.'
    if( nignored > 0 .and. o>0) write(o,'(3A,2(I0,A),9A)') sym, fun, WARNING(0), &
      nignored,' lines were ignored, last ignored line(#',last_ignored_linenumber,') says "', trim(last_ignored_line), '"!'

    nwarn_close2isoboundary = 0 ; nwarn_mirrorbutnegative = 0 ; nwarn_close2mirrorboundary = 0 ; ninfo_backfold4periodic = 0


    cellsize = cellsize * cell_scale
    if( cell_scale /= 1.0 ) then
      if(o>0) write(o,'(3A,F0.9,9A)') sym, fun, 'scale cell by factor ', cell_scale
    endif ! scale /= 1.0


    ! do some checks for required parameters
    !---------------------------------------------------------------
    !---------------------------------------------------------------

    !---------------------------------------------------------------
    if( apos_scale /= 1.0 ) then
      if( apos_scale == 0. ) then
        if(o>0) write(o,'(9A)') sym, fun, ERROR, 'scale factor for absolute positions ("',trim(I_KeyWord_APOS.in.KeyWordDict),'") is zero!'
        ist = -1 ; return
      endif !
      if(o>0) write(o,'(3A,F0.9,9A)') sym, fun, 'scale absolute positions by factor ', apos_scale
    endif ! scale /= 1.0
    !---------------------------------------------------------------

    if( kc(I_KeyWord_CELL) < 1 ) then
      if(o>0) write(o,'(9A)') sym, fun, ERROR, 'cell extend must be defined, keyword is "',trim(I_KeyWord_CELL.in.KeyWordDict),'".'
      ist = -1 ; return
    endif ! not determined

    if( any( cellsize <= 0.0 ) ) then
      if(o>0) write(o,'(5A,3(" ",F0.3),9A)') sym, fun, ERROR, 'all cell dimensions have to be positive, ', &
        'but found [', cellsize*Ang, ' ]', Ang_, ' for "',trim(I_KeyWord_CELL.in.KeyWordDict),'".'
      ist = -1 ; return
    endif ! any cellsize < 1



    if( kc(I_KeyWord_NGPS) > 0 ) then
      ! grid_points has been specified
      if( any( ngps < 1 ) ) then
        if(o>0) write(o,'(4A,3(" ",I0),9A)') sym, fun, ERROR, &
          'all numbers of grid points have to be positive, but found [', ngps, ' ] for "',trim(I_KeyWord_NGPS.in.KeyWordDict),'".'
        ist = -1 ; return
      endif ! ngps < 1
      if( kc(I_KeyWord_HGRD) > 0 .and. o>0) write(o,'(9A)') sym, fun, WARNING(0), & ! warn if both are specified
         '"',trim(I_KeyWord_NGPS.in.KeyWordDict),'" overwrites the effects of "',trim(I_KeyWord_HGRD.in.KeyWordDict),'".'

      ! ==> calculate grid spacing h
    elseif( kc(I_KeyWord_HGRD) > 0 ) then
      ! grid_spacing has been specified
      if( gridspacing <= 0.0 ) then
        if(o>0) write(o,'(4A,F0.6,9A)') sym, fun, ERROR, &
          'the grid spacing needs to be positive, but found ', gridspacing*Ang, Ang_,' for "',trim(I_KeyWord_HGRD.in.KeyWordDict),'".'
        ist = -1 ; return
      endif ! hin <= 0.0
      ! gridspacing has been specified, so calculate ngps
      ngps = ceiling( cellsize / gridspacing - 1E-6 ) ! 1E-6 is subtracted
      ! a small safety is subtracted for the cases of cellsize = m * gridspacing
      ! then floating inaccuracy would give ngps = m+1          (m is integer)
    else
      if(o>0) write(o,'(3A,F0.6,9A)') sym, fun, 'use ', gridspacing*Ang, Ang_, ' as default for "',trim(I_KeyWord_HGRD.in.KeyWordDict),'".'
      ! neither ngps nor gridspacing has been specified, use default
      ngps = ceiling( cellsize / gridspacing - 1E-6 ) ! see above
    endif ! ngps specified

    ! define the exact grid spacing
    h = cellsize / real( ngps ) ! an h for each direction, h <= gridspacing

    !---------------------------------------------------------------
    if(o>0 .and. any(h > 1.)) write(o,'(4A,3(" ",F0.6),9A)') sym, fun, WARNING(0), 'grid spacing very large:', h*Ang, Ang_
    if(o>0 .and. any(h < .1)) write(o,'(4A,3(" ",F0.6),9A)') sym, fun, WARNING(0), 'grid spacing very small:', h*Ang, Ang_
    !---------------------------------------------------------------

    !---------------------------------------------------------------
    ! Warn, if the grid spacings are very anisotropic
    ani = h * ( h(1)*h(2)*h(3) )**(-1./3.) - 1.0
    if( any( abs(ani) > .10 ) ) then ! 10%
      if(o>o) write(o,'(4A,3(" ",F0.1),9A)') sym, fun, WARNING(0), 'large grid anisotropy [', ani*100., ' ] %'
    elseif( any( abs(ani) > .01 ) ) then ! 1%
      if(o>o) write(o,'(3A,3(" ",F0.1),9A)') sym, fun, 'grid anisotropy [', ani*100., ' ] %'
    endif ! warn at large grid anisotropy
    !---------------------------------------------------------------

    !---------------------------------------------------------------
    ! check if the spin input was ok
    selectcase( nspins )
    case(    0 ) ; nspins = 1 ; if(o>0) write(o,'(9A)') sym, fun, 'spin undetermined, use default.'
    case(  1:2 ) ! spin ok
    case(    4 ) ; nspins = 2 ; if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'spin ',trim(4.in.SpinDict),' not implemented!'
    case default ; nspins = 1 ; if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'spin must be in {1 or 2[,4 is planned]}!'
    endselect ! nspins
    if(o>0) write(o,'(9A)') sym, fun, 'spin ',trim(nspins.in.SpinDict),'.'
    !---------------------------------------------------------------


    !---------------------------------------------------------------
    ! check if the spin input was ok
    if( hfield(3) /= 0. ) then
      selectcase( nspins )
      case( 1 ) ; hfield = 0. ; if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'external magnetic field only with spin!'
      case( 2 ) ; if(o>0) write(o,'(3A,F0.3,9A)') sym, fun, 'spin polarized with external magnetic field ', hfield(3)*eV, eV_
      endselect ! nspins
    endif ! hfield
    !---------------------------------------------------------------


    !---------------------------------------------------------------
    ! check for manual process numbers of the domain decomposition
    if( any( nxyzproc < 1 ) ) then
      if(o>0) write(o,'(9A)') sym, fun, WARNING(0), &
        'keyword "', trim(I_KeyWord_DOMN.in.KeyWordDict), '" must appear with 1 or 3 positive integers!'
      nxyzproc = max( 1, nxyzproc ) ! adjust it to at least 1
    endif ! any nxyzproc < 1
    !---------------------------------------------------------------

    !---------------------------------------------------------------
    ! finite difference order can be 0 (off) but not negative
    if(o>0 .and. any(finitediff<1)) write(o,'(4A,9(" ",I0))') sym, fun, WARNING(0), 'check finite-difference orders: [KS,gr,es]', finitediff
    finitediff = max(0,finitediff)
    !---------------------------------------------------------------

    !---------------------------------------------------------------
    ! eigenstate solver method
    if(o>0) write(o,'(9A)') sym, fun, 'apply "',trim(I_KeyWord_SOLV.in.KeyWordDict),' ',trim(key_solver.in.SolvDict),'".'
    selectcase( key_solver )
    case( SOLVER_CONGRAD )
      if( ParallelizeBands .and.o>0) write(o,'(9A)') sym, fun, WARNING(0), 'solver "',trim(key_solver.in.SolvDict),'" does not work with band parallelization!'
    case( SOLVER_SUBSROT ) ; if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'solver "',trim(key_solver.in.SolvDict),'" alone ==> orbitals will not relax!'
    case( SOLVER_XPLICIT ) ; if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'solver "',trim(key_solver.in.SolvDict),'" is extremely expensive!'
    case( SOLVER_NONE )    ; if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'solver "',trim(key_solver.in.SolvDict),'" switches off any solver.'
    case( SOLVER_DESCENT, SOLVER_RMMDIIS, SOLVER_SCHEME_SPEED, SOLVER_SCHEME_STABLE, SOLVER_DAVIDSO ) ! ok, no warning
    case default ; key_solver = SOLVER_RMMDIIS
      if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'unknown ',trim(I_KeyWord_SOLV.in.KeyWordDict),', use default!'
      if(o>0) write(o,'(9A)') sym, fun, 'use "',trim(I_KeyWord_SOLV.in.KeyWordDict),' ',trim(key_solver.in.SolvDict),'".'
    endselect ! str
    !---------------------------------------------------------------


    !---------------------------------------------------------------
    ! show the full name of the XC-functional
    if(o>0) write(o,'(99A)') sym, fun, 'apply "',trim(I_KeyWord_XCFN.in.KeyWordDict),' ',&
      trim(key_functional .in. XCfnDict),'", i.e. ', trim(key_functional .alt. XCfnDict), '.'
    !---------------------------------------------------------------


    !---------------------------------------------------------------
    ! show, which variables have been define in the input environemnt
    ist = showenv( o )
    !---------------------------------------------------------------


    ! launch warnings, if a keywords has been mentioned more than once
    do ik = lbound(kc,1), ubound(kc,1)
      if( kc(ik) < 2 ) cycle
      if(o>0) write(o,'(8A,I0,9A)') sym, fun, WARNING(0), &
       'keyword "',trim(ik .in. KeyWordDict),'" or "',trim(ik .alt. KeyWordDict),'" has been mentioned ',kc(ik),' times.'
    enddo ! ik

    ! check boundary conditions
    do id = 1, 3

      selectcase( general_bc(id) )
      case( BC_FINITE )
        ! finite boundary conditions:
        ! lower and upper boundary condition
        bc(id,1:2) = general_bc(id)
        if( efield(id) /= 0. ) then
          if(o>0) write(o,'(5A,ES10.2E1,9A)') sym, fun, 'external electric field in ',achar(119+id),'-direction ', efield(id), ' Ha/Bohr'
        endif
        kmesh(id) = 0

      case( BC_PERIODIC )
        ! periodic boundary conditions:
        ! lower and upper boundary condition
        bc(id,1:2) = general_bc(id)
        if( efield(id) /= 0. ) then
          stop 'INI: electric field in periodic direction impossible!'
        endif
        kmesh(id) = max( 1, kmesh(id) )

      case( BC_MIRROR )
        ! mirror boundary conditions
        bc(id,1) = BC_MIRROR ! the lower BC is mirrored
        bc(id,2) = BC_FINITE ! the upper BC is vacuum (finite)
        kmesh(id) = 2
        kshift(id) = 0.5

#ifdef GENERAL_CELL
      case( BC_SHIFTED )

        if( id == 1 ) stop 'initialize: SHIFT boundary conditions only for y(1) and z(2)'
        bc(1,1:2) = BC_PERIODIC ! the BC in x-direction must be periodic
        if( id == 3 ) bc(2,1:2) = BC_SHIFTED ! the BC in y-direction must be shifted, too
#endif
      case default ! stop 'INIT initialize: only FINITE, MIRROR, PERIODIC and SHIFTED boundary implemented. (kpoints)'
        if(o>0) write(o,'(99A)') sym, fun, WARNING(0), 'unknown ',trim(I_KeyWord_BCND.in.KeyWordDict),' ',trim(general_bc(id).in.BcndDict),'" in ',achar(119+id),'-direction!'
        general_bc(id) = BC_FINITE ! set default
        if(o>0) write(o,'(9A)') sym, fun, 'use "',trim(general_bc(id).in.BcndDict),'" in ',achar(119+id),'-direction!'

      endselect ! general_bc(id)
    enddo ! id

cDBG  if(o>0) write(o,'(9A)') sym, fun, 'Boundary condition set.'


    !---------------------------------------------------------------
    !-- block data -----------------------
    !---------------------------------------------------------------

    ! read a block of explicitly given k-points
    mkpnts = readblock( unt=unt, keyword=BlockKeyWord_Kpnts, array=kptpos, MPIcomm=comm )

    if( mkpnts < 1 ) then
      ! the explicit list of k-points has priority over the automatically generated list
      if( any( kmesh > 0 ) ) then
        if(o>0) write(o,'(3A,9(I0,A))') sym, fun, &
          'use automatic generation of a ',max(kmesh(1),1),' x ',max(kmesh(2),1),' x ',max(kmesh(3),1),' k-point set.'
      endif
      ! generate automatic k-point mesh
      ist = generate_kpoint_set( kmesh, kptpos, kshift, key_symmetry )
      mkpnts = size( kptpos, 2 )
cDBG
      if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'automatically generated ',mkpnts,' k-points.'
    endif ! mkpnts < 1


! cDBG   if(o>0) write(o,'(9A)') sym, fun, 'DEBUG line 1921'

cDBG  is_int = .true.
! cDBG   if(o>0) write(o,'(3A,2I3)') sym, fun, 'shape(kptpos)=', shape(kptpos)
! cDBG    if(o>0) write(o,*) sym, fun, 'shape(kptpos)=', shape(kptpos)
cDBG  do ik = 1, size( kptpos, 2 )
cDBG    is_int = is_int .and. is_integer( kptpos(0,ik) )
!cDBG     if(o>0) write(o,'(2A,I3,A,F10.3)') sym, fun, ik, '=ik, w=', kptpos(0,ik)
cDBG  enddo ! ik

    if( mkpnts < 1 ) then
      deallocate( kptpos, stat=ist )
      if(o>0) write(o,'(9A)') sym, fun, 'no k-points in input file, create Gamma-point.' ! no WARNING(0) here
      ! create the gamma point
      allocate( kptpos(0:3,1:1), stat=ist )
      if( ist /= 0 ) stop 'INIT: allocation of KPTPOS failed!'
      kptpos(1:3,1) = 0.0 ! Gamma points
      kptpos( 0 ,1) = 1.0 ! weight = 1
      mkpnts        = 1
      nk            = 1
cDBG  if(o>0) write(o,'(9A)') sym, fun, 'Gamma-point created.'
    endif ! mkpnts < 1
    !---------------------------------------------------------------



    ! check if the k-points are consistent with the boundary condition
    !
cDBG  k_adj = .false. ! init as false: no kpoint has been adjusted
    do id = 1, 3
      nk = size( kptpos, 2 )
      selectcase( bc(id,1) )
      case( BC_FINITE, BC_MIRROR )
        ! set all k-point coordinates to the gamma point
cDBG    k_adj = k_adj .or. any( kptpos(id,:) /= 0. )
        kptpos(id,:) = 0.

#ifdef GENERAL_CELL
      case( BC_PERIODIC, BC_SHIFTED )
#else
      case( BC_PERIODIC )
#endif
        ! fold k-points back into [ -0.5, 0.5 ]
        do ik = 1, nk
cDBG      k_adj = k_adj .or. ( nint( kptpos(id,ik) ) /= 0 )
          kptpos(id,ik) = kptpos(id,ik) - real(nint( kptpos(id,ik) ))
        enddo ! ik
      endselect ! bc

    enddo ! id
cDBG  if(k_adj.and.o>0) write(o,'(9A)') sym, fun, 'k-points adjusted to boundary conditions.'


    if( any( bc(1:3,1) == BC_MIRROR ) ) stop 'INIT: Mirror boundary condition disabled!'
    nmirror = 0
!!    !! activate this for Mirror boundary conditions
!     nmirror = count( bc(1:3,1) == BC_MIRROR )
!
! #ifdef DEBUG
!     if( nmirror > 1 ) then
!       if(o>0) write(o,'(9A)') sym, fun, WARNING(0), &
!         'Mirror boundary: Take care of half-integer k-points!'
!     endif ! bc = MIRROR
! #endif
! 
!     ! set the kpoints from the given values in the array kptpos
!     nk = size( kptpos ,2 )
!     mk = nk * ( 2**nmirror )
! 
!     ! create an equally sized array
!     allocate( newkpt(0:3,mk), stat=ist )
!     if( ist /= 0 ) stop 'INIT: allocation of NEWKPT failed!'
!     where( bc(:,1) == BC_MIRROR ) ; kmirror = 1
!     elsewhere                     ; kmirror = 0
!     endwhere
! 
!     ik = 0
!     do kz = 0, kmirror(3)
!       do ky = 0, kmirror(2)
!         do kx = 0, kmirror(1)
!           newkpt( 0 ,ik+1:ik+nk) = abs( kptpos( 0 ,1:nk) ) ! copy the weights
!           newkpt(1:3,ik+1:ik+nk) =      kptpos(1:3,1:nk)   ! copy coordinates
!           if( kx == 1 ) newkpt(1,ik+1:ik+nk) = 0.5
!           if( ky == 1 ) newkpt(2,ik+1:ik+nk) = 0.5
!           if( kz == 1 ) newkpt(3,ik+1:ik+nk) = 0.5
!           ik = ik + nk
!         enddo ! kx
!       enddo ! ky
!     enddo ! kz

    ! copy the kpoint array
    mk = nk ! create an equally sized array
    allocate( newkpt(0:3,mk), stat=ist )
    if( ist /= 0 ) stop 'INIT: allocation of NEWKPT failed!'
    newkpt = kptpos ! copy


    ! check for double-counted k-points
    if(o>0.and.mk>1) write(o,'(3A,I0,9A)') sym, fun, 'check ',mk,' k-points for double counting.'
    if( .true. ) then

      do ik1 = 1, mk
        ! run over all others
        do ik2 = ik1+1, mk
          if( newkpt(0,ik2) > 0. ) then
            kpt(1:3) = newkpt(1:3,ik1) - newkpt(1:3,ik2) ! distance vector
            delta_k2 = kpt(1)**2 + kpt(2)**2 + kpt(3)**2 ! distance^2
            if( delta_k2 <= 1.E-12 ) then
              newkpt(0,ik1) = newkpt(0,ik1) + newkpt(0,ik2) ! transfer the weight of ik2 to ik1
              ! clear the weight of ik2, so the sum of all weights should be preserved
              newkpt(0,ik2) = 0.
cFDB          if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'k-point #',ik2,' falls to k-point #',ik1
            endif ! delta_k2 <= THRESHOLD
          endif !  weight(ik2) > 0.
        enddo ! ik2
      enddo ! ik1

    else ; if(o>0.and.mk>1) write(o,'(9A)') sym, fun, 'check for k-points for double counting SWITCHED OFF'
    endif ! check for double_counting

    ! number of k-points that are left with a non-zero weight
    nk = count( newkpt(0,:) > 0. )
    if(o>0.and.mk/=nk) write(o,'(3A,9(I0,A))') sym, fun, 'reduce from ',mk,' to ',nk,' k-points.'
    if( nk < 1 ) stop 'INIT less than one kpoint impossible!'

    ! re-allocate k-points
    deallocate( kptpos, stat=ist ) ; allocate( kptpos(0:3,nk), stat=ist )
    if( ist /= 0 ) stop 'INIT: 2nd allocation of KPTPOS failed!'
    ! initialize
    kptpos = 0.

    ik = 0
    weightsum = 0.
    do ik1 = 1, mk
      if( newkpt(0,ik1) > 0. ) then
        ik = ik+1
        kptpos(0:3,ik) = newkpt(0:3,ik1) ! copy weight and position
        weightsum = weightsum + kptpos(0,ik)
      endif !
    enddo ! ik1

    deallocate( newkpt, stat=ist ) ! free the auxiliary k-point array

cDBG  if(o>0) then ! show in the form, how it should appear in the input file
cDBG    write(o,'(A)') '', BlockKeyWord_Kpnts
cDBG    do ik = 1, nk                                               ! weight
cDBG      write(o,'(3F8.3,A,I6)') kptpos(1:3,ik), '  ', nint( kptpos(0,ik) )
cDBG    enddo ! ik
cDBG    write(o,'(A)') BlockKeyWord_Kpnts, ''
cDBG  endif ! o/=0


    ! renormalize the weights
    kptpos(0,:) = kptpos(0,:) * 2**nmirror /weightsum


    ! check the numerical quality of new weights
    weightsum = sum( kptpos(0,:) )
    if( abs(weightsum-2**nmirror) > 1E-13 ) then
      if(o>0) write(o,'(4A,ES10.2E2)') sym, fun, WARNING(0), &
        'the sum of the renomalized k-point weight differs from 1.0 by', weightsum-2**nmirror
    endif ! sum of all weights is not 1.0

    ! set the kpoints
    nk = size( kptpos, 2 )
    allocate( kpoints( nk ), stat=ist )
    if( ist /= 0 ) stop 'INIT: allocation of kpoints(:) failed!'
    do ik = 1, nk
      kpoints(ik) = kpoint_set( k=kptpos(1:3,ik), weight=kptpos(0,ik), jk=ik )
    enddo ! ik
! cDBG      if(o>0) write(o,'(3A,I0)') sym, fun, 'nkpoints = ',nk
! cDBG      if(o>0) write(o,'(3A,I0)') sym, fun, 'nkpoints = ',size(kpoints,1)


    ! k-point-path
    mkpath = readblock( unt=unt, keyword=BlockKeyWord_Kpath, array=kpathpos, MPIcomm=comm )

    if( any( general_bc == BC_PERIODIC ) ) then

      if( mkpath == 1 ) then
        if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'a k-path needs at least 2 edge points.'
      endif ! mkpath == 1

    else  ! any periodic BC
      ! no periodic BC
      if( mkpath > 1 ) then
        if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'a k-path is only meaningful for periodic boundary conditions.'
      endif ! mkpath > 1

      mkpath = 0
      if( allocated( kpathpos ) ) deallocate( kpathpos, stat=ist )
    endif ! any periodic BC

    if( mkpath > 1 ) then

      do id = 1, 3
        selectcase( general_bc(id) )
        case( BC_FINITE )
          ! finite boundary conditions:
          ! set all k-point coordinates to the gamma point
          kpathpos(id,1:mkpath) = 0.

        case( BC_PERIODIC )
          ! periodic boundary conditions:
          ! fold k-points back into [ -0.5, 0.5 ]
          do ik = 1, mkpath
            kpathpos(id,ik) = mod( kpathpos(id,ik) - 0.5, 1.0 ) + 0.5
          enddo ! ik

        case( BC_MIRROR )
          ! mirror boundary conditions
          do ik = 1, mkpath
            ! check if all k-points are either  -0.5 or 0.0 or 0.5
            if( is_integer( 2.*kpathpos(id,ik) ) ) then
              ! Ok
            else ! is_half_integer
              if(o>0) write(o,'(4A,I0,9A)') sym, fun, WARNING(0), &
                'mirror boundary needs to adjust k-path-edge #',ik,' to a half-integer.'
              if( modulo( nint( 2.*kpathpos(id,ik)), 2 ) == 1 ) then
                ! mirror symmetry is odd
                kpathpos(id,ik) = 0.5
              else
                ! mirror symmetry is even
                kpathpos(id,ik) = 0.0
              endif
            endif ! is_half_integer
          enddo ! ik

        case default; stop 'INIT initialize: only FINITE, MIRROR and PERIODIC boundary implemented. (kpath)'
        endselect ! general_bc(id)
      enddo ! id

      ! show in the form, how it should appear in the input file
      if(o>0) then
        write(o,'(A)') ' ', BlockKeyWord_Kpath
        do ik = 1, mkpath
          write(o,'(3F8.3)') kpathpos(1:3,ik)
        enddo ! ik
        write(o,'(A)') BlockKeyWord_Kpath, ' '
      endif ! o/=0

      allocate( kpath(mkpath), stat=ist )
      if( ist /= 0 ) stop 'INIT: allocation of kpath(:) failed!'
      do ik = 1, mkpath
        kpath(ik) = kpoint_set( k=kpathpos(1:3,ik), weight=1.0, jk=-ik )
      enddo ! ik

    endif ! mkpath > 1







    ! now read atoms
    matoms_a = readblock( unt=unt, keyword=BlockKeyWord_Atoms, array=atmpos_a, MPIcomm=comm )

    if( matoms_a > 0 ) atmpos_a(1:3,:) = atmpos_a(1:3,:) * apos_scale

    matoms_f = readblock( unt=unt, keyword=BlockKeyWord_Afrac, array=atmpos_f, MPIcomm=comm )

    ! close the input file
    if(MPIm) close( unit=unt, iostat=ios )
    if(MPIp) call MPIbcast0( ios, comm )

cFDB  if(o>0) write(o,'(3A,999I3)') sym, fun, 'found ', nint(atmpos_a(0,:))
cFDB  if(o>0) write(o,'(3A,999I3)') sym, fun, 'found ', nint(atmpos_f(0,:))
cFDB  if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'found ',size(atmpos_a,2),' atoms (absolute).'
cFDB  if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'found ',size(atmpos_f,2),' atoms (fractional).'

cDBG  ! check if the routine readblock works properly
cDBG  if( size(atmpos_a,2) /= matoms_a ) stop 'initialize: readblock does not work right (abs).'
cDBG  if( size(atmpos_f,2) /= matoms_f ) stop 'initialize: readblock does not work right (frac).'

    if( AT_LEAST_ONE_ATOM ) then
      ! condition of at least one atom in the sample
      if( matoms_a < 1 .and. matoms_f < 1 ) then
        if(o>0) write(o,'(9A)') sym, fun, ERROR, 'no atoms in input file, please use at least one atom (Z=0 also possible).'
        ist = -1 ; return
      endif ! matoms_a + matoms_f < 1
    endif ! AT_LEAST_ONE_ATOM


    matoms = matoms_a + matoms_f ! maximum number of atoms
    allocate( atmpos(1:9,matoms), atminfo(0:5,matoms), stat=ist )
    if( ist /= 0 ) stop 'INIT: allocation of ATMPOS failed!'

cDBG  if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'found ', matoms, ' atoms to be checked.'

    ! set the coordinates origin to the center of the cell
    if( any( origin /= 0. ) ) then
      if(o>0) write(o,'(3A,3(" ",F0.1),A,3(" ",F0.3),9A)') sym, fun, &
        'coordinate origin is',100.*origin,' % from the cell center, i.e.',origin*cellsize*Ang, Ang_
    else  ! origin /= 0
      if(o>0) write(o,'(9A)') sym, fun, 'default coordinate origin is the cell center.'
    endif ! origin /= 0

    ! count the number of valid atoms

    ma = 0 ! all


    do iat = 1, matoms_a ! number of atoms in atmpos_a
      iZ = nint(atmpos_a(0,iat))
      if( iZ >= -1 .and. iZ <= Z_MAXIMUM ) then
        ! valid
        ma = ma + 1
        atmpos(1:3,ma) = atmpos_a(1:3,iat) - origin(1:3) * cellsize(1:3)
        atmpos(4:9,ma) = atmpos_a(4:9,iat)
        atminfo(0,ma) = iZ ! species
!         atminfo(1:3,ma) = 1 ! symmetry
        atminfo(4,ma) = TYPE_ATOM_POS_ABSOLUTE
        atminfo(5,ma) = iat
      endif ! Z in range
    enddo ! iat


    do iat = 1, matoms_f ! number of atoms in atmpos_f
      iZ = nint(atmpos_f(0,iat))
      if( iZ >= -1 .and. iZ <= Z_MAXIMUM ) then
        ! valid
        ma = ma + 1
        atmpos(1:3,ma) = atmpos_f(1:3,iat) - origin(1:3)
        atmpos(4:9,ma) = atmpos_f(4:9,iat)
        atminfo(0,ma) = iZ ! species
!         atminfo(1:3,ma) = 1 ! symmetry
        atminfo(4,ma) = TYPE_ATOM_POS_FRACTIONAL
        atminfo(5,ma) = iat
      endif ! Z in range
    enddo ! iat

    matoms = ma
cDBG  if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'found ', matoms, ' valid atoms.'


    ! do some checks according to the boundary conditions
    ! ------------------------------------------------------


    do iat = 1, matoms

      ! prepare pos and fpos for checks
      selectcase( atminfo(4,iat) ) ! pos fractional or absolute
      case( TYPE_ATOM_POS_FRACTIONAL )
        fpos(1:3) = atmpos(1:3,iat)
        apos(1:3) = fpos(1:3) * cellsize(1:3)
      case( TYPE_ATOM_POS_ABSOLUTE )
        apos(1:3) = atmpos(1:3,iat)
        fpos(1:3) = apos(1:3) / cellsize(1:3)
      case default
        if(o>0) write(o,'(4A,I0,9A)') sym, fun, ERROR, 'undetermined key found at the ',iat,'-th valid atom.'
        stop 'INIT initialize: atomic positions can only be absolute or fractional.'
      endselect ! pos fractional or absolute

      ! now pos is the absolute poition

      ! check each direction separately
      do id = 1, 3
cFDB   if(o>0) write(o,'(9A)') sym, fun, 'check ', DIR(id), '-direction.'

        atminfo(id,iat) = 1 ! default

        selectcase( bc(id,1) ) ! check lower BC

        !==========finite=============
        case( BC_FINITE )
          ! finite boundary conditions:

          if( abs( apos(id) ) > 0.5 * cellsize(id) ) then
            ! atom is outside the isolated boundary

            if(o>0) write(o,'(4A,I0,9A)') sym, fun, ERROR, 'the position of atom #', &
              iat,' exceeds the isolated boundary in ',DIR(id),'-direction.'

            if( STOPonERROR ) stop 'INIT initialize: atomic position exceeds cell borders.'
            atminfo(id,iat) = 0

          ! check if atoms come close to the boundaries
          elseif( abs( apos(id) ) > 0.5 * cellsize(id) - MIN_VACUUM ) then
            ! atom is very close to the isolated boundary
cDBG        if(o>0) write(o,'(4A,I6,9A)') sym, fun, WARN, 'the position of atom #', &
cDBG          iat, ' is very close to the isolated boundary in ', DIR(id), '-direction.'
            nwarn_close2isoboundary(id) = nwarn_close2isoboundary(id) + 1
          endif ! |apos| > cell/2
        !==========finite=============


        !==========periodic=============
#ifdef GENERAL_CELL
        case( BC_PERIODIC, BC_SHIFTED )
#else
        case( BC_PERIODIC )
#endif
          ! periodic boundary conditions:
          if( abs( fpos(id) ) >= 0.5 ) then
            ! atom exceeds the periodic boundary ==> backfold
cFDB        if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'the position of atom #', &
cFDB          iat,' is folded back into the 1st unit cell (periodic in ',DIR(id),'-direction).'
            fpos(id) = modulo( fpos(id) + 0.5, 1.0 ) - 0.5
            ! modification of apos
            apos(id) = fpos(id) * cellsize(id)

cFDB        if(o>0) write(o,'(3A,I0,A,F0.3,9A)') sym, fun, 'the position of atom #', &
cFDB          iat,' is ',apos(id),' after folding back in ',DIR(id),'-direction.'
            if( abs( fpos(id) ) > 0.5 ) & ! do not mention those atoms on the boundary
              ninfo_backfold4periodic(id) = ninfo_backfold4periodic(id) + 1
          endif ! |fpos| >= 0.5
        !==========periodic=============

        !==========mirror=============
        case( BC_MIRROR )
          ! mirror boundary conditions
cDBG      if( bc(id,2) /= BC_FINITE ) & ! check to opposite BC
cDBG        stop 'INIT initialize: MIRROR BC is requires FINITE BC on the opposite side.'

          ! atomic coordinates have to be
          !      --> zero      (symmetry factor 1)
          ! or   --> positive  (symmetry factor 2)

          if( apos(id) < 0. ) then

cDBG        if(o>0) write(o,'(9A)') sym ,fun, WARN, 'the position of atom #', &
cDBG          iat, ' is negative though mirror boundary conditions in ', &
cDBG          DIR(id), '-direction.'
            nwarn_mirrorbutnegative(id) = nwarn_mirrorbutnegative(id) + 1
          elseif( apos(id) == 0. ) then
            ! atom lies in the mirror plane
            atminfo(id,iat) = 2
          elseif( apos(id) < cellsize(id) ) then
            ! position is positive
            atminfo(id,iat) = 1

            if( apos(id) < 0.5 * MIN_DISTANCE ) then
              ! very close to the mirror plane
cDBG          if(o>0) write(o,'(4A,I6,9A)') sym, fun, WARN, 'the distance of atom #', &
cDBG            iat, ' to the mirror boundary is small, but not 0.'
              nwarn_close2mirrorboundary(id) = nwarn_close2mirrorboundary(id) + 1
            endif ! pos < 0.1

            if( apos(id) > cellsize(id) - MIN_VACUUM ) then
              ! atom is very close to the isolated boundary
cDBG          if(o>0) write(o,'(9A)') sym, fun, WARN, &
cDBG            'an atomic position is very close to the isolated boundary.'
              nwarn_close2isoboundary(id) = nwarn_close2isoboundary(id) + 1
            endif ! pos close to iso boundary

          else ! apos ...
            ! the position is outside the cell
            ! atom is very outside the isolated boundary
cDBG        if(o>0) write(o,'(4A,I6,9A)') sym, fun, ERROR, 'the position of atom #', &
cDBG          iat, ' exceeds the isolated boundary in ', &
cDBG         DIR(id), '-direction (opposite to mirror plane).'
            if( STOPonERROR ) stop 'INIT initialize: atomic position exceeds cell borders.'
            atminfo(id,iat) = 0

          endif ! apos negative
        !==========mirror=============

cDBG    case default ; stop 'INIT: only FINITE, MIRROR and PERIODIC boundary implemented. (atoms)'
        endselect ! bc(id,ise)

      enddo ! id

      ! now all atoms are in absolute coordinates, but
      ! the information, if they were given in absolute
      ! or fractional coordinates will be kept in atminfo(4,:)
      atmpos(1:3,iat) = apos(1:3)

    enddo ! iat

    ! give warnings
    if( o > 0 ) then
      do id = 1, 3

        if( ninfo_backfold4periodic(id) > 0 ) write(o,'(3A,I0,9A)') sym, fun, '', & ! no warning here
            ninfo_backfold4periodic(id),' atoms are folded back into the 1st unit cell (periodic) in ',DIR(id),'-direction.'

        if( nwarn_close2isoboundary(id) > 0 ) write(o,'(3A,I0,9A)') sym, fun, WARNING(0), &
            nwarn_close2isoboundary(id),' atoms are close to the isolated boundary in ',DIR(id),'-direction.'

        if( nwarn_mirrorbutnegative(id) > 0 ) write(o,'(3A,I0,9A)') sym, fun, WARNING(0), &
            nwarn_mirrorbutnegative(id),' atoms have negative coordinates (beyond the mirror plane) in ',DIR(id),'-direction.'

        if( nwarn_close2mirrorboundary(id) > 0 ) write(o,'(3A,I0,9A)') sym, fun, WARNING(0), &
            nwarn_close2mirrorboundary(id),' atoms are close to but not at the mirror boundary in ',DIR(id),'-direction.'

      enddo ! id
    endif ! o > 0


    ! count how many real atoms are there and of which species
    matoms = 0
    do iat = 1, ma
      nsym = atminfo(1,iat)*atminfo(2,iat)*atminfo(3,iat)
      if( nsym > 0 ) matoms = matoms + 1
    enddo ! iat
    if(o>0) write(o,'(2A,I0,9A)') sym, fun, matoms,' atoms in the system.'

    if( AT_LEAST_ONE_ATOM .and. matoms < 1 ) then
      if(o>0) write(o,'(9A)') sym, fun, ERROR, 'no atom with valid atomic number.'
      ist = -1 ; return
    endif ! AT_LEAST_ONE_ATOM

    allocate( atoms(matoms), stat=ist ) ; if( ist /= 0 ) stop 'INIT: allocation of atoms(:) failed!'

    element_count = 0 ! init
    do iat = 1, matoms
      iZ = atminfo(0,iat) ! abbreviate
      element_count(iZ) = element_count(iZ)+1 ! count up
    enddo ! iat

    nspecies = count( element_count > 0 )
    if( element_count( -1) > 0 .and. o>0) write(o,'(3A,I0,9A)') sym, fun, WARNING(0), element_count( -1),' negatrons detected.'
    if( element_count(  0) > 0 .and. o>0) write(o,'(3A,I0,9A)') sym, fun, WARNING(0), element_count(  0),' vacuum atoms detected.'
    if( element_count(121) > 0 .and. o>0) write(o,'(3A,I0,9A)') sym, fun, WARNING(0), element_count(121),' custom atoms detected.'

cDBG  if(o>0) write(o,'(3A,I0)') sym, fun, 'number of different species = ', nspecies
cDBG  if( nspecies < 0 ) stop 'INIT initialize: FATAL ERROR, nspecies < 0'

    allocate( speci(nspecies), stat=ist ) ; if( ist /= 0 ) stop 'INIT: allocation of speci(:) failed!'
    isp = 0 ! init
    do iZ = lbound(element_count,1), ubound(element_count,1)
      if( element_count(iZ) > 0 ) then
        isp = isp+1 ! count up
        speci(isp) = species_set( iZ=iZ ) ! copy default species information
      endif ! element_count(Z) > 0
    enddo ! Z

cDBG  if( isp /= nspecies ) stop 'INIT initialize: FATAL COUNTING ERROR for nspecies.'
cDBG  if(o>0) write(o,'(3A,123A3)') sym, fun, 'system contains ', speci(:)%sym
!     if(o>0) write(o,'(4A,99(9(I0,3A),/))') sym, fun, 'system contains', ( '  ',element_count(speci(isp)%Z),'x ', speci(isp)%sym, isp=1,nspecies )
    if(o>0) write(o,'(4A,99(9(I0,3A),/))') sym, fun, 'system contains', ( '  ',element_count(speci(isp)%iZ),'x ', speci(isp)%sym, isp=1,nspecies )

    ! create look-up table
    speci_index = 0 ! init
    do isp = 1, nspecies
      speci_index(speci(isp)%iZ) = isp
    enddo ! isp
    ! now speci(speci_index(iZ))%iZ == iZ for all iZ''s in this system
cDBG  if(o>0) write(o,'(3A,999(" ",I0))') sym, fun, 'speci_index:', speci_index

    masscenter = 0. ! init
    element_count = 0 ! init again
    ja = 0 ! init global atom index
    do iat = 1, matoms
      iZ = atminfo(0,iat) ! abbreviate

      ! create the atoms
      ! ======================================================
      nsym = atminfo(1,iat)*atminfo(2,iat)*atminfo(3,iat)
      if( nsym > 0 ) then

        isp = speci_index(iZ)
cDBG    if( isp <= 0 ) stop 'INIT initialize: FATAL ERROR: species index is 0!'

        ! generate a label for the atom
        element_count(iZ) = element_count(iZ)+1 ! count up
        write( unit=atom_label, fmt='(I0,2A)', IOstat=ios ) element_count(iZ),'.',speci(isp)%sym
        ! write( unit=atom_label, fmt='(I0,2A)', IOstat=ios ) iat,'.',speci(isp)%sym ! number

        ja = ja+1 ! new global atom index
        apos = atmpos(1:3,iat) ! absolute position
        atoms(ja) = atom_set( s=speci(isp), & ! atomic species
                              label=atom_label, &
                              ja=ja, pos=apos, &
                              key_pos=atminfo(4,iat), & ! how the coordinates were input (abs or frac)
                              symmetry_factor=1./real(nsym) )
        atoms(ja)%relpos = apos/cellsize ! relative position
        atoms(ja)%fix = atmpos(4:6,iat) ! fixed positions
        atoms(ja)%magmom = atmpos(7,iat) ! ! spin_of_initial_occupation 
        atoms(ja)%hubbard_U_J = atmpos(8,iat) ! ! spin_of_initial_occupation 
        atoms(ja)%hubbard_U_J_p = atmpos(9,iat) ! ! spin_of_initial_occupation 
        !write(*,*) " atmpos(9,iat) = ",  atmpos(9,iat)
        ! compute center of mass
        masscenter(1:3) = masscenter(1:3) + speci(isp)%weight * apos(1:3)
        masscenter( 0 ) = masscenter( 0 ) + speci(isp)%weight ! denominator
      endif ! nsym > 0
      ! ======================================================
    enddo ! iat

    if( masscenter(0) > 0. ) then
      apos = masscenter(1:3)/masscenter( 0 )
      if(o>0) write(o,'(3A,3F9.5,9A)') sym, fun, 'center of mass', apos/cellsize, ' rel'
      if(o>0) write(o,'(3A,3F9.3,9A)') sym, fun, '            ',   apos*Ang,  '  ', Ang_
    endif ! denominator is non-vanishing, positive

!     if( CheckMode > 0 ) & ! measure all possible atom-atom distances
!       call check_atom_distances( atminfo(0,1:matoms), atmpos(1:3,1:matoms) )

    !---------------------------------------------------------------

  contains

    !! read a line and evaluate parameters that appear after keywords
    status_t function read_keywords( line, linenumber ) result( istatus )
    use configuration, only: o
    use constants, only: KELVIN, ANGSTROM
    use configuration, only: MaxKeyWordLen, CommentChars, &
      I_KeyWord_CELL, &
      I_KeyWord_HGRD, &
      I_KeyWord_NGPS, &
      I_KeyWord_SHFT, &
      I_KeyWord_BCND, &
      I_KeyWord_DOMN, &
      I_KeyWord_BNDS, &
      I_KeyWord_SPIN, &
      I_KeyWord_MIXR, &
      I_KeyWord_ORIG, &
      I_KeyWord_CHRG, &
      I_KeyWord_SCFI, &
      I_KeyWord_WFSI, &
      I_KeyWord_SCAL, &
      I_KeyWord_FRCS, &
      I_KeyWord_TEMP, &
      I_KeyWord_DGRD, &
      I_KeyWord_APOS, &
      I_KeyWord_NFDO, &
      I_KeyWord_SOLV, &
      I_KeyWord_ELEM, &
      I_KeyWord_NSCA, &
      I_KeyWord_KMSH, &
      I_KeyWord_KSHF, &
      I_KeyWord_POIS, &
      I_KeyWord_SYMM, &
      I_KeyWord_XCFN, &
      I_KeyWord_UNTS, &
      I_KeyWord_MDIT, &
      I_KeyWord_EFLD, &
      I_KeyWord_HFLD, &
      I_KeyWord_FPTH, &
      I_KeyWord_PDOS, &
      I_KeyWord_CMNT, &
      I_KeyWord_WARN, &
      I_KeyWord_RCPI, &
      I_KeyWord_UNKNOWN ! unknown --> leads to errors
    use input, only: atomicnumber_by_symbol
    use input, only: possible_misstypes ! check for typos
    use input, only: read3or1
    use input, only: eval
    use type_criteria, only: assignment(=)
    use type_element, only: customize_element
    use unitsystem, only: set_units => set
    use toolbox, only: operator(+)
    implicit none
      ! parameters
cDBG  character(len=*), parameter   :: fun = ' read_keywords: '
      real, parameter               :: GridSpacings(5) = (/ .63, .4724315, .378, .28346, .1889726 /)
      !integer, parameter            :: DoubleGridNs(5) = (/ 1, 2, 4, 4, 6 /)
      !integer, parameter            :: DoubleGridMs(5) = (/ 3, 5, 5, 5, 7 /)
      integer, parameter            :: DoubleGridNs(5) = (/ 1, 1, 1, 1, 1 /)
      integer, parameter            :: DoubleGridMs(5) = (/ 1, 1, 1, 1, 1 /)
      integer, parameter            :: FiniteDiffNs(5) = (/ 4, 4, 4, 4, 4 /)
      integer, parameter            :: NscaleValues(5) = (/ 1, 1, 1, 1, 1 /)
      real, parameter               :: Kmesh_x_Cell(5) = (/ 16., 32., 48., 64., 128. /)
      integer, parameter            :: SCF_criteria(5) = (/ -4, -5, -6, -7, -8 /)
      integer, parameter            :: WFS_criteria(5) = (/ -3, -4, -5, -7, -9 /)
      integer, parameter            :: PSS_criteria(5) = (/ -5, -11, -11, -11, -11 /)

      ! arguments
      character(len=*), intent(in)  :: line
      integer, intent(in)           :: linenumber
      ! local vars
      character(len=MaxKeyWordLen)  :: kword, wrd(3), chem
      integer                       :: kios, iZ
      integer                       :: ikey
      real                          :: rnum

      if( linenumber == SET_DEFAULTS ) then
        !---------------------------------------------------------------
        !- set defaults ------------------------------------------------
        !---------------------------------------------------------------

        cellsize(1:3) = 0. ! impossible, needs to be specified
        ngps(1:3) = 0 ! impossible, will be adjusted
        nxyzproc(1:3) = 1 ! serial

        cell_scale = 1. ! 1:do not scale
#ifdef GENERAL_CELL
        shifts(1:3) = 0.
#endif
        nbands = -1 ! -1:auto
        nspins = 'auto' .in. SpinDict ! 0:auto
        addcharge = 0.
        efield(1:3) = 0.
        hfield(1:3) = 0.
        bc          = 'isolated' .in. BcndDict
        general_bc  = 'isolated' .in. BcndDict
        origin(1:3) = 0.
        temperature = 315.773244/KELVIN ! 1 mHa for (/smearing,mixing,plotting/)
        kshift(1:3) = 0. ! symmetric around the Gamma point
        pdos = ' ' ! none
        ipdos = 0 
        pawdatapath = '.' ! ==> .:here, slash will be added

        mixingratio = 0.25
        nhistory       = 1

        key_mixing = 'default' .in. MixrDict

        key_symmetry = 'default' .in. SymmDict ! no symmetry by default
        key_Poisson =  'default' .in. PoisDict ! default Poisson solver
        key_functional = 'PBE' .in. XCfnDict ! default density functional
        key_solver = 'default' .in. SolvDict ! default solving scheme

        ! convergence or stop criteria for scf and poisson
        frc = .true. ! turn on force calculation

        key_recipe = 'default' .in. RcpiDict ! default
        nscale = 1 ! ge (for potential generation) is nscale times denser than gd
        gridspacing = 0.5
        mdi = 'silent max 0 min 0 < 1E-4 default'
        scf = 'silent max 33 min 3 < 1E-7 default'
        wfs = 'silent max 1 min 1 < 1E-2 default'
        pss = 'silent max 1000 min 3 < 1E-10 default' !!! setting defaults here does not work!!!!!
        !---------------------------------------------------------------
        !doublegrid(1:2) = (/4,5/) ! order/2, meshrefinement for DoubleGrid method
        doublegrid(1:2) = (/1,1/) ! order/2, meshrefinement for DoubleGrid method
        !finitediff(1:3) = (/4,4,4/) ! nf for (/kinetic,gradients,poisson/)
        finitediff(1:3) = (/4,3,4/) ! nf for (/kinetic,gradients,poisson/)
        kmesh(1:3) = -1 ! -1: auto

        istatus = 0 ; return

      elseif( linenumber == SET_RECIPES ) then
        !---------------------------------------------------------------
        !- set recipes ------------------------------------------------
        !---------------------------------------------------------------
        if(o>0) write(o,'(9A)') sym, fun, 'apply "',trim(I_KeyWord_RCPI.in.KeyWordDict),' ',trim(key_recipe.in.RcpiDict),'".'

        selectcase( key_recipe )
        case(  1  )  ; if(o>0) write(o,'(9A)') sym, fun, WARNING(0), ' "',trim(key_recipe.in.RcpiDict),'" may lead to inaccurate results.'
        case( 2:4 )  ! 'light' (default), 'right', 'tight'
        case(  5  )  ; if(o>0) write(o,'(9A)') sym, fun, WARNING(0), ' "',trim(key_recipe.in.RcpiDict),'" may lead to expensive calculations.'
        case default ; key_recipe = KEY_recipe_DEFAULT ! correct
          if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'input unclear for "',trim(I_KeyWord_RCPI.in.KeyWordDict),'", set default!'
          if(o>0) write(o,'(9A)') sym, fun, 'use "',trim(I_KeyWord_RCPI.in.KeyWordDict),' ',trim(key_recipe.in.RcpiDict),'".'
        endselect ! recipe

        ! was this mentioned? otherwise set some values according to the recipies
        if( kc(I_KeyWord_HGRD) < 1 ) gridspacing   = GridSpacings(key_recipe)
        if( kc(I_KeyWord_DGRD) < 1 ) doublegrid(1) = DoubleGridNs(key_recipe)
        if( kc(I_KeyWord_DGRD) < 1 ) doublegrid(2) = DoubleGridMs(key_recipe)
        if( kc(I_KeyWord_NFDO) < 1 ) finitediff(1) = FiniteDiffNs(key_recipe)
        if( kc(I_KeyWord_KMSH) < 1 ) kmesh = nint( Kmesh_x_Cell(key_recipe)/cellsize )
        if( kc(I_KeyWord_WFSI) < 1 ) wfs = 'silent max 1 min 1 < 1E'+WFS_criteria(key_recipe)
        if( kc(I_KeyWord_SCFI) < 1 ) scf = 'silent max 33 min 3 < 1E'+SCF_criteria(key_recipe)
        if( kc(I_KeyWord_POIS) < 1 ) pss = 'silent max 1000 min 3 < 1E'+PSS_criteria(key_recipe)

      endif ! linenumber == SET_RECIPES

      istatus = -1 ! for early return

      ! start parsing this line from the input file (or passed via the the command line --> negative linenumbers)

      kword = '' ; read( unit=line, fmt=*, iostat=kios ) kword ! read the first word of the line
cFDB  if(o>0) write(o,'(5A,9(I0,A))') sym, fun, 'echo: line = "', trim(line), '", kios = ',kios,' in line #',linenumber
      if( kios == 0 ) then
        if( all( kword(1:1) /= CommentChars ) ) then
          wrd = '' ! init array of words

          selectcase( kword )             ! increase the keyword counter !
          !=========================
          ! block keywords: just make them a case (each), so they are not considered as unknown
          case( BlockKeyWord_Atoms ) ! opens/closes a block
          case( BlockKeyWord_Afrac ) ! opens/closes a block
          case( BlockKeyWord_Kpnts ) ! opens/closes a block
          case( BlockKeyWord_Kpath ) ! opens/closes a block
          !=========================
          case( '' ) ; ios = 0 ! ignore empty line
cFDB        if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'empty line will be ignored, line #',linenumber
          ! until here the select works on the string kword
          case default ! kword

            !======================================================================================================================
            ikey = kword .in. KeyWordDict ! search word in dictionary, returns I_KeyWord_UNKNOWN if unknown
            selectcase( ikey )
            !======================================================================================================================
            ! now the select works on the integer ikey

#define inc kc(ikey)=kc(ikey)+1

            ! required keyword
            case( I_KeyWord_CELL ) ; inc ; ios = read3or1( line, cellsize )
            ! optional keywords
            case( I_KeyWord_NGPS ) ; inc ; ios = read3or1( line, ngps )
            case( I_KeyWord_HGRD ) ; inc ; read( unit=line, fmt=*, iostat=ios ) kword, wrd(1:3)
                                              gridspacing = eval( wrd(1:3), ios )
            case( I_KeyWord_NSCA ) ; inc ; read( unit=line, fmt=*, iostat=ios ) kword, nscale 
            case( I_KeyWord_SCAL ) ; inc ; read( unit=line, fmt=*, iostat=ios ) kword, wrd
                                              cell_scale = eval( wrd(1:3), ios )
            case( I_KeyWord_BCND ) ; inc ; ios = read3or1( line, general_bc, dictionary=BcndDict )
            case( I_KeyWord_DOMN ) ; inc ; ios = read3or1( line, nxyzproc )
            case( I_KeyWord_BNDS ) ; inc ; read( unit=line, fmt=*, iostat=ios ) kword, wrd(1:3)
                                              nbands = nint( eval( wrd(1:3), ios ) )
            case( I_KeyWord_SPIN ) ; inc ; read( unit=line, fmt=*, iostat=ios ) kword, wrd(1)
                                              nspins = nint( eval( wrd(1), ios, dictionary=SpinDict ) )
            case( I_KeyWord_ORIG ) ; inc ; ios = read3or1( line, origin )
            case( I_KeyWord_CHRG ) ; inc ; read( unit=line, fmt=*, iostat=ios ) kword, wrd(1:3)
                                              addcharge = eval( wrd(1:3), ios )
            case( I_KeyWord_FRCS ) ; inc ; ios = read3or1( line, ifrc, dictionary=BoolDict ) ; frc = ( ifrc /= 0 )
            case( I_KeyWord_TEMP ) ; inc ; read( unit=line, fmt=*, iostat=ios ) kword, wrd(1:3)
                                              temperature = eval( wrd(1:3), ios )
            case( I_KeyWord_DGRD ) ; inc ; read( unit=line, fmt=*, iostat=ios ) kword, doublegrid(1:2) ! 2 int
            case( I_KeyWord_NFDO ) ; inc ; ios = read3or1( line, finitediff )
            case( I_KeyWord_SCFI ) ; inc ; scf = line ; ios = 0 ! set convergence criteria
            case( I_KeyWord_WFSI ) ; inc ; wfs = line ; ios = 0 ! set convergence criteria
            case( I_KeyWord_EFLD ) ; inc ; ios = read3or1( line, efield(1:3) )
            case( I_KeyWord_HFLD ) ; inc ; read( unit=line, fmt=*, iostat=ios ) kword, wrd(1:3)
                                              hfield(3) = eval( wrd(1:3), ios )
            case( I_KeyWord_SOLV ) ; inc ; read( unit=line, fmt=*, iostat=ios ) kword, wrd(1)
                                              key_solver = wrd(1) .in. SolvDict
            case( I_KeyWord_APOS ) ; inc ; read( unit=line, fmt=*, iostat=ios ) kword, wrd(1:3)
                                              apos_scale = eval( wrd(1:3), ios )
            case( I_KeyWord_ELEM ) ; ios = customize_element( line, keyword=(ikey.in.KeyWordDict) )
!             if(o>0) write(o,'(9A)') sym, fun, 'line for ',trim(ikey.in.KeyWordDict),' reads "',trim(line),'"'
            case( I_KeyWord_KMSH ) ; inc ; ios = read3or1( line, kmesh )
            case( I_KeyWord_KSHF ) ; inc ; ios = read3or1( line, kshift )
            case( I_KeyWord_FPTH ) ; inc ; read( unit=line, fmt=*, iostat=ios ) kword, pawdatapath
#ifdef GENERAL_CELL
            case( I_KeyWord_SHFT ) ; inc ; read( unit=line, fmt=*, iostat=ios ) kword, shifts(1:3)
#else
            case( I_KeyWord_SHFT ) ; inc ; if(o>0) write(o,'(9A)') sym, fun, 'keyword "',trim(ikey.in.KeyWordDict),'" ignored!'
#endif

            !=====================================================
            ! modules, that do their own input processing
            !=====================================================
            case( I_KeyWord_MIXR ) ; inc ; read( unit=line, fmt=*, iostat=ios ) kword, wrd(1:3)
              nhistory    = max(1,nint(eval( wrd(3), def=1. ))) ! #of history steps
              key_mixing  = wrd(2) .in. MixrDict ! mixing method
              mixingratio = eval( wrd(1:1), ios ) ! mixing ratio
cDBG          if(o>0) write(o,'(3A,F10.3)') sym, fun, 'mixingratio =', mixingratio
            case( I_KeyWord_SYMM ) ; inc ; read( unit=line, fmt=*, iostat=ios ) kword, wrd(1)
              key_symmetry = wrd(1) .in. SymmDict
            case( I_KeyWord_XCFN ) ; inc ; read( unit=line, fmt=*, iostat=ios ) kword, wrd(1)
              key_functional = wrd(1) .in. XCfnDict
            case( I_KeyWord_POIS ) ; inc ; read( unit=line, fmt=*, iostat=ios ) kword, wrd(1)
              key_Poisson = wrd(1) .in. PoisDict ! change method for solving the Poisson equation
              pss = line ; ios = 0 ! set convergence criteria
            case( I_KeyWord_MDIT ) ; inc ; read( unit=line, fmt=*, iostat=ios ) kword, wrd(1)
!             key_md = string2mdmethod( wrd(1) ) ! change method of molecular dynamics
              mdi = line ; ios = 0 ! set convergence criteria
            case( I_KeyWord_UNTS ) ; inc ; read( unit=line, fmt=*, iostat=ios ) kword, wrd(1:2)
cDBG          if(o>0) write(o,'(9A)') sym, fun, 'line for ', trim(ikey.in.KeyWordDict), ' reads "', trim(line), '"'
cDBG          if(o>0) write(o,'(9A)') sym, fun, ' ... "', trim(wrd(1)), '" and "', trim(wrd(2)), '"'
              ios = abs( set_units( wrd(1) ) ) + abs( set_units( wrd(2) ) )
            case( I_KeyWord_PDOS ) 

! ! write(*,'(3(A,I0),9A)') 'kc(ikey)=',kc(ikey),' size(pdos)=',size(pdos),' ipdos=',ipdos,' line="',trim(line),'"'
              if( ipdos < 1 .or. ipdos >= size(pdos) ) inc 
! ! write(*,'(3(A,I0),9A)') 'kc(ikey)=',kc(ikey)
              ipdos = min( size(pdos), ipdos+1 )
! ! write(*,'(3(A,I0),9A)') 'ipdos=',ipdos
              read( unit=line, fmt=*, iostat=ios ) kword, pdos(ipdos)
! ! write(*,'(2(A,I0),9A)') 'ios=',ios,' pdos(ipdos)=pdos(',ipdos,')="',pdos(ipdos),'" kword="',trim(kword),'"'
! ! write(*,'(2(A,I0),9A)') ''

            case( I_KeyWord_RCPI ) ; inc ; read( unit=line, fmt=*, iostat=ios ) kword, wrd(1)
              key_recipe = wrd(1) .in. RcpiDict
#undef inc
            case( I_KeyWord_CMNT ) ; ios = 0 !; inc ! do not increase the kc-counter with inc so we can use comment several times without warning!
              if(o>0) write(o,'(/,2A,I0,2A,/)') trim(infile),':',linenumber,'  ',trim(line)
            case( I_KeyWord_WARN ) ; ios = 0 !; inc ! do not increase the kc-counter with inc so we can use comment several times without warning!
              if(o>0) write(o,'(/,2A,I0,2A,/)') trim(infile),':',linenumber,'  ',WARNING(0),trim(line) ! show the warning
            !======================================================================================================================


            ! until here the select works on the integer ikey
            !======================================================================================================================
            case( I_KeyWord_UNKNOWN )
              ! error handling here
              ios = eval( line ) ! if the line contains some valid variable definitions, ios will be 0
              if( ios > 0 ) then

cFDB            if(o>0) write(o,'(5A,I0)') sym, fun, 'try to read a real number from "',trim(kword),'", line #',linenumber
                read( unit=line, fmt=*, iostat=ios ) rnum
                if( ios /= 0 ) then
cFDB              if(o>0) write(o,'(5A,I0)') sym, fun, 'try to read a chemical symbol from "',trim(kword),'", line #',linenumber
                  if( atomicnumber_by_symbol( kword(1:3) ) > -4 ) &
cFDB              then
cFDB                if(o>0) write(o,'(5A,I0)') sym, fun, 'found a chemical symbol in "',trim(kword),'", Z = ',atomicnumber_by_symbol( kword(1:3) )
                    ios = 0 ! found a valid element
cFDB              else ; if(o>0) write(o,'(5A,I0)') sym, fun, 'failed to read a chemical symbol from "',trim(kword),'", line #',linenumber
cFDB              endif
cFDB            else ; if(o>0) write(o,'(5A,I0)') sym, fun, 'found a real number from "',trim(kword),'", ignore line #',linenumber
                endif ! failed to read a real number

                ! variable environment was not able to process the line
                if( ios /= 0 ) then
                  if( possible_misstypes( kword, KeyWordDict(:)%word ) ) then
                    if(o>0) write(o,'(4A,9(I0,A))') sym, fun, WARNING(0),'line #',linenumber,' may contain a misstyped keyword!'
                    ios = 0 ! ignore the line
                  else  ! misstyped keyword
                    if(o>0) write(o,'(4A,I0,9A)') sym, fun, ERROR,'line #',linenumber,' line = "',trim(line),'" cannot be parsed!'
                  endif ! misstyped keyword
                endif ! ios /= 0

              elseif( ios < 0 ) then  ! ios > 0
                if(o>0) write(o,'(/,3A,I0,9A)') sym, fun, 'Warning! Cannot eval line #',linenumber,', ignore line "', trim(line), '".'
                ! ignore line with WARNING which will be launched at the end when nignored > 0
                last_ignored_line = adjustl(line)
                last_ignored_linenumber = linenumber
                nignored = nignored+1
                ios = 0
              endif ! line could also not be processed successfully by the variable environment

            case default ; stop 'INIT: fatal: this case may never happen, Dictonary does not work right!'
            endselect ! ikey
            ! now the select continues on the string kword again
          endselect ! kword

          if( ios /= 0 ) then
            if( ( kword .in. KeyWordDict ) > I_KeyWord_UNKNOWN ) then
              if(o>0) then
                write(o,'(/,6A,I0)') sym, fun, WARNING(0), &
                  'keyword "',trim(kword),'" does not appear in the right context, line #',linenumber
                write(o,'(9A)') sym, fun, '     ... number or type of arguments wrong or variables undefined.'
                write(o,'(9A)') sym, fun, '     ... input line reads "', trim(line), '".'
              endif ! o>0
            else  ! is a valid keyword
              if(o>0) write(o,'(/,4A,I0,9A)') sym, fun, WARNING(0), 'line #',linenumber,', "',trim(kword),' ..." cannot be processed!'
            endif ! is a valid keyword
            ! if( STOPonERROR ) stop 'INIT: keyword in wrong context.'
            istatus = -1 ; return ! ERROR
          endif ! ios /= 0
        else  ! kword(1:1) /= CommentChar
          ncommented = ncommented+1
cFDB      if(o>0) write(o,'(9A)') sym, fun, 'commented line "', trim(kword), '" ...'
        endif ! kword(1:1) /= CommentChar

      else  ! kios == 0
cFDB    if(o>0) write(o,'(3A,I0)') sym, fun, 'unable to read a keyword in line #',linenumber
      endif ! kios == 0
      istatus = 0 ! successful

    endfunction ! read_keywords

  endfunction ! initialize

#ifdef EXTENDED
  status_t function test( )
    write(*,*,iostat=test) __FILE__,' no module test implemented!'
  endfunction ! test
#endif

endmodule ! init

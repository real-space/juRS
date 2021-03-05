#include "config.h"

#define COUNT_WARNINGS

!! @author Paul Baumeister
!! @version 3.0
!!
!! contains most important configurations for this code.
!! the only runtime variable is the output unit o,
!! because that will be 0(silent) in parallel runs
!! except for the master process
module configuration
  use type_item, only: item, TypeItemUnknown ! TypeItemWordLen
implicit none
  public ! default for this module namespace

  ! logging output unit o
  iounit_t, save, protected    :: o   = 6  !! default output, 0: no output
! iounit_t, save, protected    :: TeX = 0  !! default output, 0: no output

  ! the string WARNING(0) is globally known to make sure that the spelling is
  ! always the same. This helps to find all warnings when grepping the output file.
#ifdef COUNT_WARNINGS
  ! WARNING(i) is a character(len=9) function, that returns the
  ! string ' WARNING! ' for i==0 and the count of warnings for i==1
  integer, private, save       :: nWarnings = 0 !! counter for warnings
  ! Important: the warning counter is only correct, if the output unit is on (o/=0).
#else
  character(len=9), parameter  :: WARNING(0:1) = (/'WARNING! ', ' <nocount> '/) !! the constant string WARNING(0)
#endif
  character(len=*), parameter  :: ERROR = 'ERROR! ' !! the constant string ERROR


  ! from here, only parameters allowed, all quantities are public
  character(len=*), parameter  :: CodeName = 'juRS' !! the code name
  character(len=*), parameter  :: FullCodeName = 'Juelich Real-Space project' !! the code name in full length
  character(len=*), parameter  :: BugReportMailAddress = 'juRS_bug_report@FZ-Juelich.De' !! contact address
  integer, parameter           :: VersionNumber    = 14 !! last 2 digits of the year
  integer, parameter           :: SubVersionNumber = 03 !! 2 digits, subversion, changes roughly every month

  logical, parameter           :: StopOnError = .TRUE. !! some parts of the code ask this before stopping

  logical, parameter           :: ParallelizeKpnts = .TRUE.  !! implemented and tested
  logical, parameter           :: ParallelizeSpins = .TRUE.  !! implemented and tested
  logical, parameter           :: ParallelizeBands = .TRUE.  !! implemented and tested

  logical, parameter           :: ParallelizeSpace = .TRUE. !! use domain decomposition
! logical, parameter           :: EqualDomainSizes = .TRUE. !! distribute the grid points with a max diff of 1
  logical, parameter           :: EqualDomainSizes = .FALSE. !! distribute the grid points with a max diff of 1
  logical, parameter           :: CartesianMapping = .TRUE. !! exploit cartesian communicators

  ! string lengths
  integer, parameter           :: MaxInputFileNameLen =  32 !! max length for the input file name
  integer, parameter           :: MaxInputFileLineLen = 128 !! max length for lines in the input file
  integer, parameter           :: MaxInputBufferLines = 100 !! number of lines read from the input file at once

  integer, parameter           :: MaxKeyWordLen = len(TypeItemUnknown) ! TypeItemWordLen !! max length for keywords

  integer, parameter           :: NonCollinearSpins = 1 !! non-collinear spin configuration 1: collinear 2:non-collinear

  character, parameter         :: CommentChars(3) = (/'#','!','%'/) !! characters to comment out lines in the input file,

  character(len=*), parameter  :: ExampleFileName = 'example' !! name of the example file that is written when called with --example
  character(len=*), parameter  :: ControlFileNamePrefix = 'running.'!+ProjectName

  integer, parameter :: I_KeyWord_UNKNOWN = 0 !! unknown --> leads to input error

  integer, parameter :: I_KeyWord_APOS =  1 !! scale factor for absolute atomic positions
  integer, parameter :: I_KeyWord_BNDS =  2 !! number of bands
  integer, parameter :: I_KeyWord_BCND =  3 !! boundary conditions
  integer, parameter :: I_KeyWord_CELL =  4 !! cell extends
  integer, parameter :: I_KeyWord_CHRG =  5 !! additional charge, default = 0.
  integer, parameter :: I_KeyWord_CMNT =  6 !! show a long comment about the calculation in the output file
  integer, parameter :: I_KeyWord_SCAL =  7 !! scale the cell size by this factor, default 1.0
  integer, parameter :: I_KeyWord_DGRD =  8 !! double grid parameters, default = (/4,3/)
  integer, parameter :: I_KeyWord_DOMN =  9 !! domain decomposition
  integer, parameter :: I_KeyWord_EFLD = 10 !! electric field (in directions of isolated BCs)
  integer, parameter :: I_KeyWord_ELEM = 11 !! change defaults for an elemente/species
  integer, parameter :: I_KeyWord_FRCS = 12 !! compute the forces [for the given directions]
  integer, parameter :: I_KeyWord_NGPS = 13 !! number of grid points
  integer, parameter :: I_KeyWord_HFLD = 14 !! magnetic field (collinear spin)
  integer, parameter :: I_KeyWord_KMSH = 15 !! automatically generated k-mesh
  integer, parameter :: I_KeyWord_KSHF = 16 !! shift the regular k-mesh
  integer, parameter :: I_KeyWord_MDIT = 17 !! molecular dynamics convergence criteria
  integer, parameter :: I_KeyWord_MIXR = 18 !! simple mixing ratio default = 0.2
  integer, parameter :: I_KeyWord_NFDO = 19 !! finite difference orders for (kinetic,poisson)
  integer, parameter :: I_KeyWord_NSCA = 20 !! change defaults for nscale, the grid ratio, usually 2
  integer, parameter :: I_KeyWord_ORIG = 21 !! coordinate offset shift, default = (/0.,0.,0./)
  integer, parameter :: I_KeyWord_RCPI = 22 !! recipes
  integer, parameter :: I_KeyWord_FPTH = 23 !! PAW data file path
  integer, parameter :: I_KeyWord_PDOS = 24 !! projected DoS
  integer, parameter :: I_KeyWord_POIS = 25 !! poisson equation, method and convergence criteria
  integer, parameter :: I_KeyWord_SCFI = 26 !! criteria for scf iterations
  integer, parameter :: I_KeyWord_HGRD = 27 !! grid spacing
  integer, parameter :: I_KeyWord_SPIN = 28 !! spin polarized {1,2} default = 1
  integer, parameter :: I_KeyWord_SOLV = 29 !! solver recepie for the KS equation
  integer, parameter :: I_KeyWord_SYMM = 30 !! symmetry option
  integer, parameter :: I_KeyWord_TEMP = 31 !! smearing temperature, default = 1mHa
  integer, parameter :: I_KeyWord_UNTS = 32 !! output unitsystem
  integer, parameter :: I_KeyWord_WARN = 33 !! launch a warning
  integer, parameter :: I_KeyWord_WFSI = 34 !! wave function convergence criteria
  integer, parameter :: I_KeyWord_XCFN = 35 !! XC-density functional
! integer, parameter :: I_KeyWord_free = 36 !! to be added --> keywords after this are not shown with -kw
  integer, parameter :: I_KeyWord_SHFT = 37 !! shifted cell boundary (unsecure)


  ! this item-array is used as a dictionary for the input syntax
  ! make sure that any two keywords which mean the same map to the same constant integer key
  ! the order of items with the same key in this list is relevant for the
  ! display of keywords, i.e. when you query the string of a keyword from this dictionary,
  ! the string of the item defined first will be returned
  type(item), parameter :: KeyWordDict(85) = (/ &
    item('atom_scale',          I_KeyWord_APOS),item('ascale',  I_KeyWord_APOS), & !
    item('number_of_bands',     I_KeyWord_BNDS),item('bands',   I_KeyWord_BNDS), & !
    item('boundary_condition',  I_KeyWord_BCND),item('bc',      I_KeyWord_BCND), item('boundary',I_KeyWord_BCND), & !
    item('cell_size',           I_KeyWord_CELL),item('cell',    I_KeyWord_CELL), & !
    item('additional_charge',   I_KeyWord_CHRG),item('charged', I_KeyWord_CHRG), item('addcharge',I_KeyWord_CHRG), & !
    item('input_comment',       I_KeyWord_CMNT),item('comment', I_KeyWord_CMNT), & !
    item('cell_scale',          I_KeyWord_SCAL),item('cscale',  I_KeyWord_SCAL), item('scale',I_KeyWord_SCAL), & !
    item('double_grid',         I_KeyWord_DGRD),item('dg',      I_KeyWord_DGRD), & !
    item('domain_decomposition',I_KeyWord_DOMN),item('domains', I_KeyWord_DOMN), & !
    item('electric_field',      I_KeyWord_EFLD),item('efield',  I_KeyWord_EFLD), & !
    item('element',             I_KeyWord_ELEM),item('element', I_KeyWord_ELEM), & ! (no alternative)
    item('get_forces',          I_KeyWord_FRCS),item('forces',  I_KeyWord_FRCS), & !
    item('grid_points',         I_KeyWord_NGPS),item('grid',    I_KeyWord_NGPS), item('ngps',I_KeyWord_NGPS),  & !
    item('magnetic_field',      I_KeyWord_HFLD),item('hfield',  I_KeyWord_HFLD), & !
    item('kpoint_mesh',         I_KeyWord_KMSH),item('kmesh',   I_KeyWord_KMSH), & !
    item('kpoint_shift',        I_KeyWord_KSHF),item('kshift',  I_KeyWord_KSHF), & !
    item('molecular_dynamic',   I_KeyWord_MDIT),item('md',      I_KeyWord_MDIT), item('relaxation',I_KeyWord_MDIT), & !
    item('density_mixing',      I_KeyWord_MIXR),item('mixing',  I_KeyWord_MIXR), item('mix',I_KeyWord_MIXR), & !
    item('finite_difference',   I_KeyWord_NFDO),item('nfd',     I_KeyWord_NFDO), item('fd',I_KeyWord_NFDO), item('nf',I_KeyWord_NFDO), & !
    item('grid_multiplier',     I_KeyWord_NSCA),item('nscale',  I_KeyWord_NSCA), & !
    item('shift_origin',        I_KeyWord_ORIG),item('origin',  I_KeyWord_ORIG), & !
    item('parameter_set',       I_KeyWord_RCPI),item('params',  I_KeyWord_RCPI), &
    item('paw_directory',       I_KeyWord_FPTH),item('path',    I_KeyWord_FPTH), item('pawpath',I_KeyWord_FPTH), & !
    item('projected_dos',       I_KeyWord_PDOS),item('pdos',    I_KeyWord_PDOS), & !
    item('poisson_solver',      I_KeyWord_POIS),item('poisson', I_KeyWord_POIS), & !
    item('self_consistency',    I_KeyWord_SCFI),item('scf',     I_KeyWord_SCFI), item('scfiter',I_KeyWord_SCFI), & !
    item('grid_spacing',        I_KeyWord_HGRD),item('spacing', I_KeyWord_HGRD), & !
    item('spin_polarization',   I_KeyWord_SPIN),item('spin',    I_KeyWord_SPIN), & !
    item('eigenstate_solver',   I_KeyWord_SOLV),item('solver',  I_KeyWord_SOLV), item('eigensolver',I_KeyWord_SOLV),& !
    item('fix_symmetry',        I_KeyWord_SYMM),item('symmetry',I_KeyWord_SYMM), & !
    item('fermi_temperature',   I_KeyWord_TEMP),item('temp',    I_KeyWord_TEMP), item('temperature',I_KeyWord_TEMP), & !
    item('unit_system',         I_KeyWord_UNTS),item('units',   I_KeyWord_UNTS), & !
    item('input_warning',       I_KeyWord_WARN),item('warning', I_KeyWord_WARN), & !
    item('wave_functions',      I_KeyWord_WFSI),item('wfs',     I_KeyWord_WFSI), & !
    item('exchange_correlation',I_KeyWord_XCFN),item('xc',      I_KeyWord_XCFN), item('functional',I_KeyWord_XCFN ), & ! xc functional
    item('cell_shift',          I_KeyWord_SHFT),item('shift',   I_KeyWord_SHFT) /) !
    ! long keyword version                           short key                   deprecated keywords


  ! keywords for lists (blocks)
  character(len=*), parameter :: BlockKeyWord_Atoms = 'atoms' !! block of atoms in absolute coordinates
  character(len=*), parameter :: BlockKeyWord_Afrac = 'atoms_fractional' !! block of atoms in fractional coordinates, i.e. [-0.5,0.5], fractional number reading supported
  character(len=*), parameter :: BlockKeyWord_Kpnts = 'kpoints' !! block of k-points
  character(len=*), parameter :: BlockKeyWord_Kpath = 'kpath' !! block of k-point path edges


  ! for file IO
  integer, parameter :: FileNameExtensionLen = 4 !! includes the dot, as e.g. '.ext'
  integer, parameter :: FileNameLen = MaxInputFileNameLen + FileNameExtensionLen !! max name length of files

  ! file extension for the density of states
  character(len=*), parameter :: WFS_FileNameExtension = '.wfs' !! wave function file extension
  character(len=*), parameter :: DoS_FileNameExtension = '.dos' !! Density of States file extension
  character(len=*), parameter :: VTK_FileNameExtension = '.vtp' !! Visualization Toolkit file extension for ParaView
  character(len=*), parameter :: RHO_FileNameExtension = '.rho' !! Density file extension
  character(len=*), parameter :: ADM_FileNameExtension = '.adm' !! atomic Density matrices file extension
  character(len=*), parameter :: MAG_FileNameExtension = '.mag' !! Magnetization file extension
  character(len=*), parameter :: BST_FileNameExtension = '.bst' !! Bandstructure file extension (xmGrace formated)
  character(len=*), parameter :: FRC_FileNameExtension = '.frc' !! forces file extension
  character(len=*), parameter :: OUT_FileNameExtension = '.out' !! output file extension
  character(len=*), parameter :: TeX_FileNameExtension = '.tex' !! LaTeX file extension
  character(len=*), parameter :: EiG_FileNameExtension = '.eig' !! Kohn-Sham eigenvalues in ASCII file extension

  ! configure where to write the output by default
  character(len=*), parameter :: OutputFileName_STDOUT = '<stdout>' ! write to unit 6

  interface failed
    module procedure failed_i, failed_l
  endinterface
  private ::         failed_i, failed_l

  contains

  !! return a string of the actual version and subversion
  character(len=8) function CodeVersion( )
    status_t :: ios
    write(unit=CodeVersion,fmt='(I0,A,I2.2)',iostat=ios) VersionNumber, '.', SubVersionNumber
  endfunction ! CodeVersion

#ifdef COUNT_WARNINGS
  !! WARNING is string function that counts using a modul variable
  !! how many times WARNING has been called.
  character(len=9) function WARNING( i ) result( s )
    integer, intent(in)   :: i !! 0:"WARNING! ", 1:number of warnings launched

    if( i == 0 ) then
      s = 'WARNING! '
      nWarnings = nWarnings + 1 ! count up
    elseif( i == 1 ) then
      if( nWarnings == 0 ) then
        s = '      NO '  ! no warnings
      else  ! nWarnings == 0
        write(unit=s,fmt='(I8,A)') nWarnings, ' ' !  # warnings
      endif ! nWARNINGs == 0
    else ! i is neither 0 nor 1
      ! this stop sentence is important, because no other inputs than 0 or 1 are allowed!
      stop 'configuration: WARNING(i), argument i must be 0 (or 1 to get the count)!'
    endif ! i /= 0

  endfunction ! WARNING
#endif

  !! handle for the protected variable o (output unit)
  status_t function set_output_unit( unt ) result( ist )
    iounit_t, intent(in) :: unt
    o = max( 0, unt )
    ist = o - unt 
  endfunction ! set_output_unit


  logical function failed_i( status, message, source, line ) result( f ) ! returns ( status /= 0 )
    status_t, intent(in)                   :: status
    character(len=*), intent(in), optional :: message ! show message
    character(len=*), intent(in), optional :: source ! sourcefile
    integer, intent(in), optional          :: line ! line number

    integer            :: iln
    status_t           :: ios
    character(len=64)  :: msg, src

    f = ( status /= 0 )
    if( .not. f ) return

    msg = '!' ; if( present( message ) ) msg = adjustl( message )
    iln =  0  ; if( present( line ) ) iln = line
    src = '?' ; if( present( source ) ) src = adjustl( source )

    if(o>0) write( unit=o, fmt='(/,5A,9(I0,A))', iostat=ios ) ' FAILED! "',trim(msg),'" in ',trim(src),':',iln, ' status=',status

  endfunction ! failed

  !! wrapper function for failed_i requesting an integer status
  logical function failed_l( check, message, source, line ) result( f ) ! returns( check )
    logical, intent(in)                    :: check
    character(len=*), intent(in), optional :: message ! show message
    character(len=*), intent(in), optional :: source ! sourcefile
    integer, intent(in), optional          :: line ! line number

    integer            :: iln
    status_t           :: ios
    character(len=64)  :: msg, src

    f = check
    if( .not. f ) return

    msg = '!' ; if( present( message ) ) msg = adjustl( message )
    iln =  0  ; if( present( line ) ) iln = line
    src = '?' ; if( present( source ) ) src = adjustl( source )

    if(o>0) write( unit=o, fmt='(/,5A,9(I0,A))', iostat=ios ) ' FAILED! "',trim(msg),'" in ',trim(src),':',iln

  endfunction ! failed

  status_t function die( message, line, file ) result( ist ) ! all args optional
    character(len=*), intent(in), optional :: message ! show message
    integer, intent(in), optional          :: line ! line number
    character(len=*), intent(in), optional :: file ! sourcefile

    character(len=*), parameter :: ERROR = 'ERROR! '
    integer            :: iln, ios
    character(len=64)  :: msg, src

    msg = '!' ; if( present( message ) ) msg = adjustl( message )
    iln =  0  ; if( present( line ) ) iln = line
    src = '?' ; if( present( file ) ) src = adjustl( file )

    if(o>0) &
    write( unit=o, fmt='(/,4A,I0,9A)', iostat=ios ) ERROR,'in ',trim(src),':',iln, ' "', trim(msg), '"'
    write( unit=0, fmt='(/,4A,I0,9A)', iostat=ios ) ERROR,'in ',trim(src),':',iln, ' "', trim(msg), '"'

    ist = -9 ! ERROR
    if( .not. StopOnError ) return

    if(o>0) stop 'killed with an error!' ; stop ! silent
  endfunction ! die

  string_t function KeyWord( i_KeyWord )
  use type_item, only: operator(.in.)
    integer, intent(in) :: i_KeyWord
    KeyWord = i_KeyWord .in. KeyWordDict
  endfunction ! KeyWord

  status_t function show_KeyWords( outunit ) result( ios )
  use type_item, only: operator(.in.), max_key, TypeItemUnknown, operator(.alt.)
    iounit_t, intent(in) :: outunit

    integer  :: ikw
    string_t :: kword, altkw
    if( outunit < 1 ) return
    write(unit=outunit,fmt='(/,A,/)',iostat=ios) '  keywords for the input file [long keys]'
    do ikw = I_KeyWord_UNKNOWN+1, max_key( KeyWordDict )
      kword = ikw .in. KeyWordDict ! the temp. kword is neccesary to avoid a recoursive IO operation when module type_item is in DEBUG
      if( kword == TypeItemUnknown ) exit ! leave the loop
      altkw = ikw .alt. KeyWordDict ! the temp. kword is neccesary to avoid a recoursive IO operation when module type_item is in DEBUG
      write(unit=outunit,fmt='(A,A12,9A)',iostat=ios) '  ', altkw, '[',trim( kword ), ']' !, achar(9) ! 9:tab
    enddo ! ikw
    write(unit=outunit,fmt='(A)',iostat=ios) ! empty line
  endfunction ! show_KeyWords

#ifdef EXTENDED
!+ extended

  status_t function test( )
    write(*,*,iostat=test) __FILE__,' no module test implemented!'
  endfunction ! test

!- extended
#endif

endmodule ! configuration

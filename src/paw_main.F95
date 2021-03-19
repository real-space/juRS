#include "config.h"

! #define DEBUG
! #define OUTPUT_CENTRAL_POTENTIAL

! #define LCAO
! #define PLOT_BASIS
#define INDEPENDENT_TASKS
! #define JELLIUM

#ifdef DEBUG
!!! remove comment from debug line
!!! ==> all debug lines will be included
#define cDBG
#else
!!! comment lines with !DBG
!!! ==> no debug lines will be included
#define cDBG !DBG
#endif


! part of this source file is prepared for automatic
! documentation using the f90doc tool,
! available at http://erikdemaine.org/software/f90doc/

!!>" %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                      "
!! " %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                      "
!! " %%%                        %%%                      "
!! " %%%    ReSpawN             %%%                      "
!! " %%%                        %%%                      "
!! " %%%    Real                %%%                      "
!! " %%%    Space               %%%                      "
!! " %%%    PAW                 %%%                      "
!! " %%%    eNvironment         %%%                      "
!! " %%%                        %%%                      "
!! " %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                      "
!! " PAW = Projector Augmented Wave                      "
!! "                                                     "
!! " %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% "
!! " %%%                                             %%% "
!! " %%%  PAW-method on uniform                      %%% "
!! " %%%  real-space grids, using double grid        %%% "
!! " %%%  technnique, localized functions and        %%% "
!! " %%%  iterative solvers.                         %%% "
!! " %%%                                             %%% "
!! " %%%  This code is beeing developed at the       %%% "
!! " %%%                                             %%% "
!! " %%%            _/   Peter Gru:nberg Institut    %%% "
!! " %%%           _/    and                (PGI)    %%% "
!! " %%%          _/     Institute for               %%% "
!! " %%%         _/      Advanced Simulation(IAS)    %%% "
!! " %%%  _/   _/                                    %%% "
!! " %%%   _/_/          Forschungszentrum Ju:lich   %%% "
!! " %%%                 52425 Ju:lich, Germany      %%% "
!! " %%%                                             %%% "
!! " %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% "
!! "                                                     "
!! "  SSSS     CCCC                                      "
!! " S    S   C                                          "
!! " S       C                                           "
!! "  SS     C                                           "
!! "    SS   C        ooo  pppp   eeee                   "
!! "      S  C       o   o p   p e eeee                  "
!! " S    S   C      o   o p   p e                       "
!! "  SSSS     CCCC   ooo  pppp   eeee                   "
!! "                       p                             "
!! " Scientific Computing  p                             "
!! "            at optimal parallel efficiency           "
!! "                                                     "
!!<"                                                     "
!!
!! The structure concept of this code is meant to yield
!! easy bookkeeping while developing it with a group
!! of programmers, not necessarily sharing one office.
!! The goal is to have readable and understandable code
!! at the top level, so that one can do physics out of a
!! toolbox, not caring too much about computer issues.
!! Therefore the module-concept of FORTRAN90 seems to
!! fit perfectly to gather subroutines, functions, derived
!! data types and operators in topic-related packages.
!! The derived data types of FORTRAN90 provide an excellent
!! way to combine data of different basic data types to
!! some object-like structure, making the argument lists
!! shorter and thus the code more readable.
!!
!! The concept is based on a strict hierarchy of modules.
!!
!! Notice:    The subroutine TBX build_dep_tree() is able
!!            to construct a full dependency-tree structure
!!            from the source code. Therefore, it is important
!!            that all use-statements are written as follows:
!!v             use module, only: *only-namelist*
!!            note that the routine creating the dependency
!!            list is case sensitive.
!!
!! [Level 0]   modules which contain ONLY constants, i.e.
!!             variables declared with the attribute 
!!             'parameter' and always 'public'.
!!             default 'public' and 'save'. To see the
!!             differences immediately looking at the code,
!!             it is convenient to write the names of
!!             CONSTANTS (in the sense of parameter) 
!!             in UPPER CASE letters. Level 0 modules
!!             may be included everywhere, but also here, the 
!!v            use
!!             statement has to be followed by the 
!!v            only
!!             list to show up, what is being
!!             included from that module.
!!
!! Example: module of constants
!!
!!>      module numbers
!!       implicit none
!!         real, parameter, public     :: ONE = 1.0
!!<      endmodule numbers
!!
!!
!! Exception in module configuration
!!             During development, several modules have to access the
!!             default output unit for DEBUG statements, Warning!s or
!!             convergence control by the user. Therefore the module
!!             configuration supports the integer variable "o" which
!!             stands for output. If the output is on (o>0), write()
!!             statements may use this unit number write(unit=o,...).
!!             Another non-constant feature of module configuration
!!             is the function WARNING(0) which returns the string
!!             "Warning! " and at the same time, increases an internal
!!             counter of how many warnings have been launched.
!!
!! [Level 1]   type declarations of basic derived data types.
!!             Basic means that these only contain elementary data
!!             types and arrays of them. Each derived data type
!!             module should have meaningful initialization values
!!             and provide a creation function ( usually <typename>_set() )
!!             returning the new structure.
!!             Only level 0 modules may be referred with the use statement.
!!
!! Example Level 1 type declaration (ficticious, not contained in the code)
!!
!!>      module type_spin
!!       implicit none
!!
!!         integer, parameter, public  :: TYPE_SPIN_PARA = 0
!!         integer, parameter, public  :: TYPE_SPIN_UP   = 1
!!         integer, parameter, public  :: TYPE_SPIN_DOWN = 2
!!
!!         type, public :: spin
!!           integer :: i = TYPE_SPIN_PARA ! default value
!!         endtype spin
!!
!!       contains
!!
!!         type(spin) function spin_set( up )
!!         implicit none
!!           ! arguments
!!           logical, intent(in) :: up
!!           if( up ) then
!!             spin_set%i = TYPE_SPIN_UP
!!           else  ! up
!!             spin_set%i = TYPE_SPIN_DOWN
!!           endif ! up
!!         return
!!         endfunction spin_set
!!
!!<      endmodule type_spin
!!
!! Comment   A useful feature for this derived data type
!!           would be to overload the operator(==) with
!!           the type(spin) comparing two spins or a spin
!!           and a string as ( s == 'up' ) and so on.
!!
!!
!! [Level 2]  type declarations of derived data types
!!             which include derived data types of the lower level.
!!             Basically, the same rules apply.
!!
!!> RULES
!!             --> NO IMPLICIT DATA TYPES
!!             --> NO BLOCK DATA STATEMENTS
!!             --> NO COMMON BLOCKS
!!             --> NO USE WITHOUT ONLY
!!             --> NO STATEMENT LABELS
!!                 --> NO FORMAT STATEMENTS
!!                 --> NO GOTO STATEMENTS
!!                 --> NO CONTINUE STATEMENTS
!!             --> NO ARGUMENTS WITHOUT EXPLICIT INTENT
!!             --> NO BINARY FILES WITHOUT HEADER
!!             --> NO READ WITHOUT IOSTAT
!!             --> NO ALLOCATE WITHOUT STAT
!!             --> ALWAYS COMPILE WITH -CheckBoundaries WHEN DEVELOPING
!!             --> A SEPARATE SOURCE FILE FOR EACH PUBLIC DERIVED DATA TYPE
!!<            --> (further rules to be added here)
!!
!! FUTURE PROJECT
!!             Every module has to support a subroutine named "test()"
!!             that conducts self-tests of its subroutines with a set of
!!             different input conditions when compiled in DEBUG mode.
!!
!!
!! FEATURES
!!             Besides the modular approach, conditional compiling
!!             is applied using a preprocessor. Therefore most source
!!             files have the .F90 extension rather than the usual .f90.
!!             In addition, the suffix .F95 indicates DoublePreprocessing
!!             (see below)
!!             ! Example of preprocessing
!!>
!!#ifdef DEBUG
!!             if(o>0) write(o,*) 'Debug' ! this code line will be included, if
!!             ! the CPPFLAGS in the makefile contain -D DEBUG
!!#else
!!             ! this line is included when DEBUG is not defined
!!#endif
!!<
!!             The standard preprocessor directives are
!!               --> DEBUG ! additional write statements, additional checks,
!!                   define it at the top of a source file rather than in the Makefile!
!!               --> DEBUG_ALL ! more additional write statements,
!!                   all module source files will define DEBUG, if not
!!                   deactivated at the top.
!!
!!                !!! IMPORTANT: the code lines between #ifdef DEBUG and #endif
!!                !!! must not produce any latency effects, so variables may only
!!                !!! be used in a read-only context
!!                !!! if you want to change a variable inside the
!!                !!! DEBUG region, e.g. for test purpose, please
!!                !!!   -- indicate the manipulation with a "! manipulate" comment
!!                !!!   -- if possible, write a warning to stdout or some error unit
!!                !!!   -- try to include a stop after the effects have been seen
!!
!!               --> NOMPI ! no usage of MPI-subroutines and CONSTANTS "use mpi"
!!               --> NaN_SEARCH ! checks if some quantity is NaN (Not a Number)
!!               --> NOLAPACK ! several subroutines will not work properly without
!!                              the LAPACK library but at least the program can be
!!                              linked and run for test purposes.
!!               --> NOScaLAPACK ! the parallel eigensolver library ScaLAPACK
!!                               is replaced by serial LAPACK (might be slow)
!!
!!             Preprocessor DIRECTIVEs used should appear at the beginning of
!!             the main routine writing a stamp to the output unit "o" as e.g.:
!!>
!!#ifdef NOMPI
!!  if(o>0) write(o,*) 'This version has been compiled without usage of the MPI Library'
!!#endif
!!<
!!
!! DoublePreprocessing: the *.F95-source files
!!              These files simplify the way to provide source code for subroutines
!!              for real and complex numbers at the same time. Therefore,
!!              the .F95-source file is preprocessed twice: the 1st time with
!!              the preprocessor directive -D R1_C2=1,
!!              the 2nd time               -D R1_C2=2.
!!              Please use the case R1_C2 == 1 for real
!!                              and R1_C2 == 2 for complex, respectively.
!!              The head of the module should stand between #if R1_C2 == 1 and an #endif.
!!              Analogously, the tail of the module containing the 'endmodule'
!!              statement must stand between #if R1_C2 == 2 and an #endif.
!!              The head part as well as the tail part may contain functions and
!!              subroutines which are not sensitive to real/complex arguments.
!!              The middle part will be compiled twice and thus provides space
!!              for R/C-hybrid procedures. However, interface declarations must
!!              be made in the module head.
!!
!! Example: a module that computes the density of a wave function
!!              This example is very short, so the necessarity of DoublePreprocessing
!!              is not given to simplify the maintenance of the code, but in general,
!!              a lot of subroutines only differ in a few code lines and so enfavour
!!              this technique. Note: in reality, the PreProcessor directive r1_c2 is
!!              written in CAPITALS. This cannot be done here, because then these macros
!!              would be replaced by the PreProcessor acting on this very source file.
!!
!!>'#ifdef r1_c2                                        '!!!%%% r1_c2 must be defined
!! '#if r1_c2                                           '!!!%%% start head part
!! '  module density                                    '!!!%%%
!! '    interface wf_density                            '!!!%%% interfaces make life easy
!! '      module procedure wf_density_r, &              '!!!%%%
!! '                       wf_density_c                 '!!!%%%
!! '    endinterface wf_density                         '!!!%%%
!! '  contains                                          '!!!%%% contains must be inside the head
!! '#endif                                              '!!!%%% end head
!! '                                                    '!!!%%%
!! '#if r1_c2 == 1                                      '!!!%%%
!! '#define REAPLEX real                                '!!!%%%
!! '    subroutine wf_density_r &                       '!!!%%% case:  real
!! '#else                                               '!!!%%%
!! '#define REAPLEX complex                             '!!!%%%
!! '    subroutine wf_density_c &                       '!!!%%% case:  complex
!! '#endif                                              '!!!%%%
!! '                           ( wf, rho )              '!!!%%%
!! '    implicit none                                   '!!!%%%
!! '      ! arguments                                   '!!!%%%
!! '      REAPLEX, intent(in) :: wf(:)  ! wave function '!!!%%% both cases: real and complex
!! '      real, intent(out)   :: rho(:) ! density       '!!!%%% this line is the same
!! '#if r1_c2 == 1                                      '!!!%%%
!! '      rho(:) = wf(:) * wf(:)                        '!!!%%% case:  real
!! '#else                                               '!!!%%%
!! '      rho(:) = real( conjg( wf(:) ) * wf(:) )       '!!!%%% case:  complex
!! '#endif                                              '!!!%%%
!! '    return                                          '!!!%%%
!! '    endsubroutine ! wf_density                      '!!!%%% works for both without the name
!! '                                                    '!!!%%%
!! '#if r1_c2 == 2                                      '!!!%%% begin tail
!! '  endmodule density                                 '!!!%%%
!! '#endif                                              '!!!%%% end tail
!!<'#endif                                              '!!!%%% end of #ifdef r1_c2
!!
!!
!!
!! STYLE Recommendations
!!             --> exploit the FORTRAN90-specific features
!!                 wherever possible because it makes
!!                 the code much more readable.
!!                 This means, code like this
!!>                 cnt = 0
!!                   do ind = 1, Nnd
!!                     if( array(ind) > 0 ) cnt = cnt + 1
!!<                  enddo ! ind
!!                 can rather be written like this
!!                   cnt = count( arr(1:Nnd) > 0 )
!!                 There are many more, such as all(), any(),...
!!
!!             --> in the output and DEBUG output statements,
!!                 use all-CAPITALS only for ERROR and WARNING, so
!!                 it still is an eye-catcher.
!!
!!             --> echo the block structure name
!!                 by mention the name of a "do"-loop counter
!!                 as a comment after enddo:
!!>                  do id = 1, Nd
!!                     ...
!!<                  enddo ! id
!!                 and for "if", mention the uppermost condition
!!>                  if( a > 0 ) then
!!                     ...
!!                  [else  ! a > 0]
!!                     ...
!!<                  endif ! a > 0
!!                 as for "do while" or "selectcase", (in addition to
!!                 indentation) it simplifies to copy parts of the code.
!!
!!             --> use at least 2 spaces indentation (I prefer 2 spaces)
!!                 when opening a block structure as for example
!!                 (do, do while, if, selectcase, where) and at a line- &
!!                   continuation.
!!             --> check indentation to be aligned!
!!
!!             --> use meaningful names! If you want to abbreviate,
!!                 it is very intuitive to throw away the wovels of
!!                 a word (except leading wovels), e.g.
!!                   smooth --> smth or smt
!!                   atomicpopulation --> atmpop
!!
!! #ifdef SUPERSTRICT
!!             --> when using counters other than i, use two letters and explain
!!                 what they count in their declaration line or
!!                 right before the loop.
!! #endif
!!
!!             --> when optional arguments are used, meaningful,
!!                 names in full length are very important,
!!                 since these should be passed using the
!!                 explicit interface and called with
!!                 the dummy arguments names, such as
!!
!!> subroutine sr( a, spinpolarized )
!!  implicit none
!!    ! arguments
!!    real, intent(in)                     :: a(:)
!!    logical, intent(in), optional        :: spinpolarized
!!    ! local vars
!!    logical                              :: spin
!!
!!    spin = .false.
!!    if( present( spinpolarized ) ) &
!!      spin = spinpolarized
!!    ...
!!  return
!!< endsubroutine sr
!!
!!  ...
!!v  call sr( array, spinpolarized=(nspins==2) )
!!
!!
!!             --> you might want to exploit the Fortran language feature,
!!                 that optional arguments are passed on as optionals.
!! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! Parallelization concept
!!
!! --------------------------------
!! There are 9 levels to parallelize: from top to bottom
!!
!! Here is a short overview:
!!    0:   Independent Tasks (activate via preprocessor)
!!    1:   Kpoints
!!    2:   Collinear spin
!!    3:   Bands
!!  456:   z-y-x-Real-Space Domain decomposition
!!    E:   Equivalent space communicators
!!    B:   band comm
!!
!! Now in detail:
!!
!!   ----------------------------------------------
!!   --0-- Independent Tasks
!!   ----------------------------------------------
!!
!!           equivalent setups with slightly
!!           different parameters.
!!           there is no communication
!!           among the different worlds
!!           Example: 2 sets with different cellsizes
!!
!!>'           _________________   _________________  '
!! '          /                 \ /                 \ '
!! '          |set# 1           | |set# 2           | '
!! '          |cell 3.9 3.9 3.9 | |cell 4.1 4.1 4.1 | '
!! '          \_________________/ \_________________/ '
!!<'                                                  '
!!
!!           communicator: independetworldcomm
!!
!!           because of their independence, the
!!           existing predefined MPI_Comm_WORLD must NOT be used!
!!
!!   ----------------------------------------------
!!   --1-- Kpoint parallelization
!!   ----------------------------------------------
!!
!!           equivalent spatial cells with
!!           different part of the kpoint set.
!!           communication is required
!!           for the sums over occupation numbers
!!           that finds the atomic density matrix
!!           and the creation of the smooth charge
!!           density from the wave functions
!!           on the coarse grid
!!           Example: 2 sets with 9 kpoints
!! 
!!>'           _________________   _________________  '
!! '          /                 \ /                 \ '
!! '          |kpoint# 1:4      | |kpoint# 5:9      | '
!! '          \_________________/ \_________________/ '
!!<'                                                  '
!!
!!   ----------------------------------------------
!!   --2-- Spin parallelization
!!   ----------------------------------------------
!!
!!           equivalent spatial cells but some
!!           quantities are calculated only
!!           for spin up or only for spin down.
!!           the smooth density on the dense grid
!!           and the atomic densities on the radial
!!           grid have to contain both spin
!!           components to evaluale the LSDA functional
!!           classical example:
!!
!!>'           _________________   _________________  '
!! '          /                 \ /                 \ '
!! '          |spin up          | |spin down        | '
!! '          \_________________/ \_________________/ '
!!<'                                                  '
!!            communicator: spincomm
!!
!!   ----------------------------------------------
!!   --3-- Band parallelization
!!   ----------------------------------------------
!!
!!           equivalent spatial cells with
!!           a certain part of the set of bands only.
!!           the orthogonalization of bands
!!           requires high communication volumes
!!           and large temporary memory usage.
!!           this works well with the DIIS method
!!           but performs badly with the explicit
!!           orthogonalization applied in CG.
!!           Example: 2 sets with 11 bands
!!
!!>'           _________________   _________________  '
!! '          /                 \ /                 \ '
!! '          |band# 1:6        | |band# 7:11       | '
!!<'          \_________________/ \_________________/ '
!!
!!
!!           communicator: band_comm
!!
!!   ----------------------------------------------
!!   -456- Domain decomposition
!!           in 3 spatial directions
!!   ----------------------------------------------
!!
!!           the communication pattern is a cartesian
!!           communicator for the grid quantities
!!           and atomic communicators for the reduction
!!           operation of the projection coefficients
!!           and broadcasting operations of the
!!           compensation charge multipole moments of
!!           the atom-owner process
!!           Example: domain decomposition 3x4x2 (=xyz)
!!>'                                                   '
!! '                      ------- ------- -------      '
!! '                    /       /       /       /|     '
!! '                    ------- ------- -------  |     '
!! '                  /       /       /       /| |     '
!! '                  ------- ------- -------  |       '
!! '                /       /       /       /| |/|     '
!! '                ------- ------- -------  |   |     '
!! '              /       /       /       /| |/| |     '
!! '              ------- ------- -------  |   |       '
!! '             |       |       |       | |/| |/      '
!! '             |       |       |       |   |         '
!! '             |       |       |       |/| |/        '
!! '              ------- ------- -------  |           '
!! '             |       |       |       | |/          '
!! '             |       |       |       |             '
!! '             |       |       |       |/            '
!! '              ------- ------- -------              '
!!<'                                                   '
!!            this sketch was auto-generated by
!!v              DSP display_decomp_ascii
!!
!!           cartesian communicator:  g%comm (g is the grid)
!!           localized communicators: a%comm (a are atoms)
!!           orthogonal communicator: equicomm (for equivalent
!!             space domains of other band-, spin- and kpoint-cells)
!!
!!           because of copying processes and
!!           strided memory access, it is probably
!!           advantageous to parallelize with the
!!           largest number of processes along the
!!           z-direction.
!!
!!   ----------------------------------------------
!!   --E-- Equivalent spaces
!!   ----------------------------------------------
!!
!!           equivalent spaces is a communicator that is
!!           orthogonal to the domain decomposition and
!!           unifies kpoint, spin and band parallelization.
!!           The Kohn-Sham potentials is the same in each
!!           processes of equi_comm.
!!
!!
!!           communicator: equi_comm
!!
!!   ----------------------------------------------
!!   --B-- Band parallelization
!!   ----------------------------------------------
!!
!!           communicator: band_comm
!!
!!           one rank of the band_comm stores
!!           only a part of all KS-states for a
!!           given spin and k-point. For the
!!           setup of the subspace matrices
!!           and the new linear combination of
!!           eigenvectors, communication is needed
!!
!!    -------------------------------------------
!!    --- Parallelization errors ----------------
!!    -------------------------------------------
!!
!!    Besides numerical differences in the results
!!    of the parallel version compared to serial,
!!    which are due to an interchanged summation
!!    ordering, there are the following points
!!    that have to be checked:
!!       -- does the module boundary (BND)
!!          operate properly
!!          ==> self-test via overall index implemented
!!       -- does the communication via the atomic
!!          communicators work properly
!!       -- correct communicator in global or partial
!!          reduction operations
!!       -- is the atom ownership unique
!!
!! =====================================================
!!
!! Command Line Arguments:
!!
!!       There are certain tools and features,
!!       that are integrated into the main code.
!!       These can be called from the command line,
!!       when run in serial:
!!
!!>' usage: ./paw  <inputfile>  [options]                        '
!! '   options:                                                  '
!! '   -c   --complex          complex wave functions (default)  '
!! '   -r   --real             real wave functions               '
!! '   -cm  --check  [#]       check the input file only         '
!! '   -h   --help             this usage help                   '
!! '   -l   --load             load existing wave functions      '
!! '   -L   --Load             load existing density file        '
!! '   -ex  --example          writes an example input file      '
!! '   -ov  --override         additional input line             '
!! '   -i   --ignore <arg>     the next arg will not be parsed   '
!!<'   -t   --test <module>    debug function testing modules    '
!!
!! =====================================================


! DoublePreprocessing
! This file will be preprocessed twice, first
! with R1_C2 == 1, and then with R1_C2 == 2
! then the two parts are concatenated to one source.
! This requires that the module head has to be
! appearing when R1_C2 == 1 and the line "end module <name>"
! with R1_C2 == 2.
#ifdef R1_C2
! begin double preprocessing
#if R1_C2 == 1
! head of the module
!
!
!! @author Paul Baumeister
!!
module rs_paw
implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: sym = 'paw' !! module symbol

  public :: usage_help
  public :: start_real
  public :: start_complex
  public :: test_module
  public :: command_line_arguments
  public :: analyze_cube_file

  interface relax_density
    module procedure relax_density_r, relax_density_c
  endinterface

  interface projected_density_of_states
    module procedure projected_density_of_states_r, projected_density_of_states_c
  endinterface

  ! There is no interface for the functions start_real and start_complex,
  ! because the top level program will decide, which one to call.

  integer, parameter :: STARTMODE_GENERATE_PAWDATA = 512

#ifdef INDEPENDENT_TASKS
  !! special parallelization over independent DFT tasks
  !! as e.g. for different images of a nudged elastic band calculations
  MPI_Comm, save :: indep_comm = 0 !! MPI communicator
#endif

#ifdef EXTENDED
  interface visualize_all_wfs
    module procedure visualize_all_wfs_r, visualize_all_wfs_c
  endinterface

  interface check_PAW_assumption
    module procedure check_PAW_assumption_r, check_PAW_assumption_c
  endinterface
#endif



  contains


    subroutine analyze_cube_file(infile)
      use inout, only: read_wfs_cube 
      use toolbox, only: radial_hist, operator(+)
      use communicators, only: comm_init, comm_finalize
      
      string_t, intent(in) :: infile
      
      real, allocatable    :: wf(:,:,:)
      integer, allocatable :: atomic_numbers(:)
      real, allocatable    :: atomic_positions(:,:)
      real                 :: gspacing(3)
      status_t             :: ios
      integer              :: comm
      integer :: i1,i2,i3
      comm = comm_init()
      
      allocate(wf(0,0,0), atomic_numbers(0), atomic_positions(3,0))

      ios = read_wfs_cube(wf, atomic_numbers, atomic_positions, gspacing, infile)

      !do i1=1,68
      !write(i1+20,'(68ES20.8E3)') wf(:,:,i1)
      !enddo

      !do i1=2,69
      !do i2=2,69
      !do i3=2,69
      !if(wf(i1,i2,i3)>10*wf(i1+1,i2,i3) .or. wf(i1,i2,i3)>10*wf(i1,i2+1,i3) .or. wf(i1,i2,i3)>10*wf(i1,i2,i3+1)) write(*,*) i1,i2,i3
      !enddo
      !enddo
      !enddo



      ios = radial_hist(wf, gspacing, atomic_positions(:,1), 3, file=trim(infile)+'.rhist')

      ios = comm_finalize("analyze_cube_file done")
    endsubroutine

! module head ends here
#endif
! from here DoublePreprocessing

#if R1_C2 == 1
#define REAPLEX real
  !! this is the main routine of the code with real wave functions
  real function start_real &
#else
#define REAPLEX complex
  !! this is the main routine of the code with complex wave functions
  real function start_complex &
#endif
        ( name, override, checkmode, startmode, outfilename, realcmplx, xml_format ) &
        result( totalenergy )
#ifdef _OPENMP
      use omp_lib 
#endif        
      use configuration, only: o ! output unit 0: no output, 6: stdout
      use configuration, only: set_output_unit ! only function that may change the output unit o
      use configuration, only: WARNING, ERROR ! Warning string (count usage), Error string
      use configuration, only: CodeName, CodeVersion ! for the header: Code name and version number
      use configuration, only: OUT_FileNameExtension, OutputFileName_STDOUT

      use configuration, only: BlockKeyWord_Atoms ! only for repeating the atomic input
      use configuration, only: KeyWord, I_KeyWord_APOS ! only for showing the relaxed positions
      use configuration, only: I_KeyWord_FPTH ! show the keyword for the pawpath

      ! derived data types and their constructors
      use type_grid, only: grid, set ! grid descriptor
#ifdef INDEPENDENT_TASKS
      use type_grid, only: change_cell
#endif
      use type_info, only: info, set ! global information
      use type_state, only: state, set ! KS-state descriptor
      use type_kpoint, only: kpoint, half_integer ! k-point descriptor
      use type_species, only: species ! PAW data
      use type_criteria, only: criteria ! convergence criteria
      use type_atom, only: atom ! atom descriptor
      use type_atom, only: prepare_atoms, free_atoms
      use type_atom, only: optimal_n_orbitals
      use type_atom, only: write_xyz_file

      use pawdata, only: read_pawdata ! loading of PAW data
      use type_item, only: operator(.in.)
      use density_functionals, only: XCfnDict => Dictionary

      use FermiDirac, only: Fermidistribution ! occupation numbers
      use FermiDirac, only: density_of_states ! creates '.dos' file

      use display, only: display_title ! header
      use display, only: display_cell_ascii ! shows cell with atoms
      use display, only: display_sample ! some input details
      use display, only: display_forces ! show forces
      use display, only: display_decomp_ascii ! domain decomposition

      use input, only: eval ! scan the variable environment

      use spherical, only: init_Gaunt ! initialization of the Gaunt tensor
      use spherical, only: init_Xlm ! initialization of spherical harmonics

      use symmetry, only: check_symmetry ! check if positions fulfill symmetry

      use init, only: initialize ! read the input file
      use prepare, only: prepare_wavefunctions ! start guess wave functions from atomic orbitals

      use selfcon, only: integral ! spacial integration of rank 3 or 4
      use selfcon, only: rhov_from_psi, allocate_rhov_c

      use forces, only: force_calculation ! eval forces acting on atoms
      use forces, only: write_force_file ! creates '.frc' file


      use communicators, only: comm_init ! MPI initialization
      use communicators, only: dimensions ! setup of MPI communicators
      use communicators, only: comm_finalize ! MPI finalization
      use communicators, only: parallelize_tasks

      use inout, only: read_wfs ! load '.wfs' file
      use inout, only: write_wfs ! store in '.wfs'
      use inout, only: write_wfs_cube ! store in '.cube'
      use configuration, only: WFS_FileNameExtension ! '.wfs'
      use inout, only: read_density ! load '.rho' file
      use inout, only: write_density ! store in '.rho'
      use configuration, only: RHO_FileNameExtension ! '.rho'
      use configuration, only: MAG_FileNameExtension ! '.mag'
      use inout, only: read_aDm ! load '.adm' file
      use inout, only: write_aDm ! store in '.adm'
      use configuration, only: ADM_FileNameExtension ! '.adm'

      use chemistry, only: find_bonds ! analyze the local coordination of each atom
      use unitsystem, only: eV , Ang  ! unit conversion factors
      use unitsystem, only: eV_, Ang_, operator(/) ! unit labels

      ! utilities
      use boundary, only: test_dataexchange ! self test of the domain decomposition

      use MPIconst, only: Wtime ! wall clock time
      use MPItools, only: operator(.MPIsum.)  ! global reduction of a single number
      use MPItools, only: operator(.MPIdiff.) ! global comparison of a single number
      use MPItools, only: operator(.MPImax.)  ! global maximum of a single number
      use MPItools, only: MPIallsum ! all elements are summed over the processes
      use MPItools, only: MPInprocs ! number of processes in a communicator
      use MPItools, only: MPImyrank ! rank of this process in a communicator
      use MPItools, only: MPImaster ! true for rank 0 in a communicator
      use MPItools, only: MPIbarrier ! all processes wait for the last one
      use MPItools, only: MPIbcast0 ! master tells all processes
      use MPItools, only: MPIparallel ! true if nprocs > 1

      use toolbox, only: operator(+) ! tool for string concatenation
#ifdef EXTENDED
!+ extended
      use display, only: display_VTK_file ! export density
      use display, only: display_decomp_xfig ! DD as '.fig' file
      use display, only: display_atoms_povray ! positions as '.pov' file
      use inout, only: density2xsf

      use bandstructure, only: bandpath, generate_path ! sample bandstructure if path edges are given
      use all_electron, only: generate ! generate PAW data
      use relaxedcore, only: grid2radial ! project rho and ves at the atomic site onto a radial grid
      use toolbox, only: visualize_density ! output to '.bmp' format
      
cDBG  use toolbox, only: radial_hist ! transforms grid quantities to radial representation files
cDBG  use toolbox, only: write_vtk_file ! interface towards VTK
cDBG  use debugtools, only: NaN_search ! finds NaN in arrays
!- extended
#endif

#ifdef CONSTR
      use constraints, only: init_mask, n_constraints
#endif

#ifdef LCAO
!       use cylindrical, only: LCAO_coefficients ! lcao method for start orbitals
      use Gaussian_overlap, only: LCAO_coefficients ! lcao method for start orbitals
#endif
      implicit none
        ! arguments
        character(len=*), intent(in)      :: name !! project name and simultaneously name of the input file
        character(len=*), intent(in)      :: override(:,:) !! arguments passed via the command line with --override (or -ov) that will be interpreted as additional lines to the input file
        integer, intent(in)               :: checkmode !! 0:real run, >0: CheckMode --> input file(s) is read in and checked for syntax, calculation parameters are shown in nice display.
        !! The value of checkmode is the number of processes you want to use for running the caluclation. Then the factorization is checked (specify "--CheckMode 8" to test for example a 2x2x2 domain decomposition)
        integer, intent(in)               :: startmode !! contains information about loading existing files
        character(len=*), intent(inout)   :: outfilename
        integer, intent(inout)            :: realcmplx ! {-2,-1,0,1,2} 0:auto, {-1,1}:real, {-2,2}:complex, negativ:silent initialization
        integer, intent(in)               :: xml_format  ! if /= 0 load xml pseudopotentials


        character(len=*), parameter       :: fun = ': '
        integer, parameter                :: NOCOSPINS = 1

        !------------------------------------------------------------------------
        ! keys for TASKs
        !------------------------------------------------------------------------
        integer, parameter    :: KEY_TASK_NOTHING               =  0 !
        ! initial tasks
        integer, parameter    :: KEY_TASK_WFS_GENERATE          =  1 !
        integer, parameter    :: KEY_TASK_WFS_LOADOLD           =  2 !
        integer, parameter    :: KEY_TASK_RHO_FROM_WFS          =  3 !
        integer, parameter    :: KEY_TASK_RHO_LOADOLD           =  4
        ! main tasks
        integer, parameter    :: KEY_TASK_RHO_RELAX             = 10 !
        integer, parameter    :: KEY_TASK_FORCES                = 18 !
        integer, parameter    :: KEY_TASK_POS_RELAX             = 25 !
        integer, parameter    :: KEY_TASK_BANDSTRUCTURE         = 21 !
        ! analysis tasks
        integer, parameter    :: KEY_TASK_POPULATION            = 32 !
        integer, parameter    :: KEY_TASK_DENSITY_OF_STATES     = 31 !
        integer, parameter    :: KEY_TASK_ATOMIC_ANALYSIS       = 34 !
        integer, parameter    :: KEY_TASK_POTENTIAL2RADIAL      = 33 !
        ! final tasks
        integer, parameter    :: KEY_TASK_WFS_STORE             = 43 !
        integer, parameter    :: KEY_TASK_RHO_STORE             = 45 !
        !------------------------------------------------------------------------


        ! basic data of the MPI parallelization
        MPI_Comm                          :: world_comm = 0 ! MPI world communicator

        integer                           :: mdit = 0 ! steps of ion movement
        real                              :: maxfrc, maxdsp
        integer                           :: itask_relax_again = 0 ! jump mark for md
        real                              :: mditerationtime = 0.

        integer                           :: ntasks = 1 ! number of tasks
        integer                           :: itask ! counter
        integer                           :: ktask ! key
        integer                           :: task_list(20) = KEY_TASK_NOTHING ! list of what to do

        integer                           :: nspecies = 0 ! number of different species
        integer                           :: ispc ! species counter
        integer, allocatable              :: ispecies(:) ! species index list for atoms

        integer                           :: natoms = 0 ! number of all atoms
        integer                           :: iatm ! atom counter

        integer                           :: nbands = 0 ! number of all bands
        integer                           :: nbnd = 0 ! local number of bands
        integer                           :: ibnd ! band counter

        integer                           :: nspins = 0 ! number of all collinear spins
        integer                           :: nspn = 0 ! local number of collinear spins
        integer                           :: ispn ! spin counter

        integer                           :: nkpoints = 0 ! number of all k-points
        integer                           :: nkpt = 0   ! local number of k-points
        integer                           :: ikpt ! k-point counter

        integer                           :: ibsk ! combindex for bands,spins,k-points
        integer                           :: nbsk = 0 ! upper limit for the combindex ibsk

        ! the cell and grid parameters
        integer                           :: nxyzproc(3) = 1 ! 3 dimensional spatial domain decomposition
        integer                           :: ngps(3) = 1 ! number of grid points in x,y,z-direction
        integer                           :: nscale = 2 ! refinement ratio dense/coarse

        integer                           :: nxyzs = 1 ! number of local degrees of a wave function
        ! cell
        real                              :: cellsize(3) = 1. ! extend of the cell in x,y,z-direction
#ifdef GENERAL_CELL
        real                              :: shifts(3) = 0. ! xy, yz, xz
#endif

        integer                           :: bc(3,2) = 0 ! boundary conditions 0: isolated, 1:periodic, ...

        integer                           :: nhistory = 1 ! 1: simple mixing
        integer                           :: key_mixing = 0 ! mixing quantity
        integer                           :: key_Poisson = 0 ! Poisson solver method
        integer                           :: key_symmetry = 0 ! symmmetry option
        integer                           :: key_solver = 0 ! eigensolver scheme 0: stable, 1: fast
        integer                           :: key_functional = 0 ! exchange-correlation functional

        real                              :: temperature = .001 ! temperature for occupation numbers .001=315 K

        integer                           :: doublegrid(1:3) = (/4,5,0/) ! double grid parameters (itp,nmesh,spare)
        integer                           :: finitediff(1:3) = 4 ! finite difference order (kinetic,gradients,Laplace)

        integer                           :: ellmax = 2 ! ell-cutoff for the radial expansion and compensation charges

        real                              :: addcharge = 0. ! additional charge in the cell (not from atoms)

        ! grids
        type(grid)                        :: gc ! coarse grid (wave function grid)
        type(grid)                        :: gd ! dense grid (gc x 2)
        type(grid)                        :: ge ! electrostatix grid (gd x nscale)

        type(info)                        :: global ! global information structure

        real, allocatable  :: vTOT_c(:,:,:,:) ! (nx,ny,nz,ns) ! Total (local) potential on gc
        real, allocatable  :: vTOT_d(:,:,:,:) ! (nx,ny,nz,ns) ! Total effective potential on gd
        real, allocatable  :: vTOT_e(:,:,:,:) ! (nx,ny,nz,ns) ! Total effective potential on ge
        real, allocatable  :: vES(:,:,:) ! (nx,ny,nz) ! Electrostatic potential on ge
        real, allocatable  :: rhov(:,:,:,:) ! (nx,ny,nz,ns) ! Smooth valence density on gd
        real, allocatable  :: rhoc(:,:,:,:) ! (nx,ny,nz,ns) ! Smooth core densities on ge
        real, allocatable  :: vBAR(:,:,:,:) ! (nx,ny,nz,ns) ! Smooth potential correction on ge
        real, allocatable  :: rho_jellium(:,:,:) ! (nx,ny,nz) ! fixed jellium density (usually negative) ge
        
        real, allocatable  :: tmp2(:,:),  tmp3(:,:,:) ! temporaries

        ! real or complex-valued wave smooth wave functions
        REAPLEX, allocatable              ::  spsi(:,:) ! (nxyzs,nbsk) ! smooth wave functions
        REAPLEX, allocatable              ::  cpsi(:,:) ! (naprj,nbsk) ! projection coefficients
        ! occupation numbers, energies, etc. for all states
        type(state), allocatable          :: psi_state(:) ! (nbsk) ! KS-state descriptors

        real                              :: FermiLevel ! level of the Fermi energy
        real                              :: totalcharge ! total number of electrons in the system
        real                              :: mixingratio = 0.2 ! default=0.2 ! simple-mixing ratio

        ! list of all kpoints
        type(kpoint), allocatable         :: kpoints_all(:) ! all_kpoints(nkpoints)
        ! parallelized list of kpoints
        type(kpoint), allocatable         :: kpt(:)         ! kpt(nkpt)

        ! kpoint-path quantities
        type(kpoint), allocatable         :: kpath(:)        ! kpath(nkpath) edges of the path for bandstructure
        type(kpoint), allocatable         :: kpathpoints(:)  ! expanded path points
        real                              :: kpath_dk = 0.01 !.002 ! default k-path sampling

        ! list of all atoms
        type(atom), allocatable           :: atoms(:)   ! all_atoms(natoms)
        ! parallelized list of atoms
        type(atom), allocatable           :: atm(:)         ! atm(natm)

        ! list of all species
        type(species), allocatable,target :: speci(:)           ! (1:nspc)

        ! convergence criteria
        type(criteria)                    :: scf ! for SCF
        type(criteria)                    :: wfs ! for wave functions
        type(criteria)                    :: pss ! for Poisson
        type(criteria)                    :: mdi ! for iterations of molecular dynamics or structural relaxations

        real                              :: writewfs, writewfscube
        logical                           :: showpos = .false.
        logical                           :: calc_force(3)
        real                              :: frcall(3)
        real                              :: efield(1:3), hfield(1:3)

        integer                           :: o_silent=0

        ! help vars
        integer                           :: ia, i, n!, i3(3), j3(3)
        status_t                          :: ist, ios
        integer                           :: nx, ny, nz ! abbrev. for allocation
        character(len=128)                :: pawdatapath
        character(len=128)                :: pawdatafile
        character(len=4)                  :: pdos(4)
        real                              :: time
        real                              :: sint ! spatial integral
cDBG    integer                           :: species_count(0:121), iZ


  !!!!!!!!!!!!!!!!!!!!!!!!
  !! executable section !!
  !!!!!!!!!!!!!!!!!!!!!!!!


  totalenergy = 0. ! init the return value of this function

  world_comm = comm_init( ) ! initialize the MPI environment

  ! switch on outputs for the root process only
  if( MPImaster(world_comm) ) then
    if( realcmplx >= 0 ) then
      ! usual
      ist = set_output_unit( 6 ) ! outputs 6:stdout on
      if( outfilename == name .or. outfilename == '' ) &
        outfilename = name+OUT_FileNameExtension ! modify the outfilename not to overwrite the input file
      if( outfilename /= OutputFileName_STDOUT ) then ! '<stdout>'
        open( unit=4, file=outfilename, action='write', status='unknown', iostat=ios )
        if( ios == 0 ) then
           ist = set_output_unit( 4 ) ! write to the file unit
        else  ! ios == 0
          if(o>0) write(o,'(/9A)') sym, fun, WARNING(0), ' failed to open "',trim(outfilename),'", use stdout.'
        endif ! ios == 0
      endif ! outfilename /= '<stdout>'
    else
      ! silent initialization, output unit o is still active and maybe even connected to the outfile
      o_silent = o ! store the output unit number
      ist = set_output_unit( 0 ) ! turn all output off
    endif ! usual or silent initialization
  else  ! MPImaster of all MPI processes
    ist = set_output_unit( 0 ) ! turn all output off
  endif ! MPImaster of all MPI processes

cDBG  if( MPIparallel(world_comm) ) then
cDBG    i = MPImyrank( world_comm )
! cDBG  if( i > 0 ) open( unit=o, file='dmp/parallel_rank'+i+'.out', action='write', iostat=ist )
cDBG    if( i > 0 ) ist = set_output_unit( 1000+i ) ! on
cDBG  endif ! parallel


  if(o>0) then
    if( checkmode > 0 ) then
      ist = display_title( modul='   ', unt=o ) ! large title from module display
    else ! checkmode: small title
      write(o,'(/9A)')      sym, fun,     '********************************'
      write(o,'(3A,A8,9A)') sym, fun, '******      ',    CodeName, '      ******'
      write(o,'(3A,A8,9A)') sym, fun, '******      v  ',CodeVersion(),'   ******'
      write(o,'(9A/)')      sym, fun,     '********************************'
    endif ! checkmode
    ! show the project name (which is the name of the input file at the same time)
    write(o,'(/5A/)') sym, fun, 'Project "', trim(name), '"'
    n = MPInprocs( world_comm )
#ifdef _OPENMP
    if( n > 1 ) write(o,'(2A,9(I0,A))') sym, fun, n,' processes with ', omp_get_max_threads(), ' threads per process running.'
#else
    if( n > 1 ) write(o,'(2A,I0,9A)') sym, fun, n,' processes running (No OpenMP threadings).'
#endif
  endif ! o>0


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!! indicate the set of PreProcessor options, that generated this code version !!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if(o>0) write(o,'(9A)') sym, fun, 'this version has been compiled on ', __DATE__ cDBG ,' at ', __TIME__

#ifdef DEBUG_ALL
  if(o>0) write(o,'(9A)') sym, fun, 'this version has been compiled with the DEBUG_ALL flag.'
#endif

#ifdef EXTENDED
  if(o>0) write(o,'(9A)') sym, fun, 'this version is compiled with EXTENDED features.'
#endif

#ifdef SINGLE_PRECISION
  if(o>0) write(o,'(9A)') sym, fun, 'this 32-bit-version uses real*4 and complex*8.'
#endif

#ifdef CONSTR
  if(o>0) write(o,'(9A)') sym, fun, 'constrained DFT enabled.'
#endif

#ifdef JELLIUM
  if(o>0) write(o,'(9A)') sym, fun, 'jellium enabled.'
#endif

#ifdef NaN_SEARCH
  if(o>0) write(o,'(9A)') sym, fun, 'NaN_SEARCH, search for NaN (Not a Number) enabled.'
#endif

#ifdef NOMPI
  if(o>0) write(o,'(9A)') sym, fun, 'MessagePassingInterface, MPI disabled!'
#endif

#ifdef NOScaLAPACK
  if(o>0) write(o,'(9A)') sym, fun, 'ScaLAPACK library usage disabled.'
#endif

#ifdef GENERAL_CELL
  if(o>0) write(o,'(9A)') sym, fun, 'non-rectangular real space basis is enabled (in serial).'
#endif

#ifdef NOHartreePotential
  if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'HartreePotential disabled!'
#endif

#ifdef USE_SYMMETRY
  if(o>0) write(o,'(9A)') sym, fun, 'symmetry for selected structures enabled.'
#endif

#ifdef RHO_ON_GD
  ! this version generates the smooth density from the smooth wave functions by
  !   rho(r)_dense = sum_bsk | interpolate( psi_bsk(r)_coarse ) |^2
  if(o>0) write(o,'(9A)') sym, fun, 'wave functions are squared on the dense grid!'
#else
  ! this PreProcessor flag deactivates the non-locality of the overlap matrix
  ! and generates the smooth density from the smooth wave functions on the coarse grid by
  !   rho(r)_dense = interpolate( sum_bsk |psi_bsk(r)_coarse|^2 )
!   if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'wave functions are squared on the coarse grid!'
  if(o>0) write(o,'(9A)') sym, fun, 'wave functions are squared on the coarse grid.'
#endif



  if( startmode == 1 ) then
    if(o>0) write(o,'(9A)') sym, fun, 'try to load wave functions.'
  else  ! startmode
#ifdef PREPARE_PLANE_WAVES
    if(o>0) write(o,'(9A)') sym, fun, 'use plane waves as start wave functions.'
#else
    if(o>0) write(o,'(9A)') sym, fun, 'use atomic orbitals as start wave functions.'
#endif
  endif ! startmode

  if( checkmode > 0 ) then
    if(o>0) write(o,'(/3A/)') sym, fun, 'CheckMode: input files are read in and checked.'
  endif ! checkmode

  ! initialize all main values, system sizes ...
  ! reads the input file "infile"
  time = Wtime() ! start timing
  ist = initialize( checkmode, name, override, world_comm, & ! only these 4 are intent(in) arguments
          bc, cellsize, &
          atoms, &
          speci, &
          nbands, nspins, &
          addcharge, &
          kpoints_all, kpath, ngps, nscale, nxyzproc, &
          mixingratio, key_mixing, nhistory, &
          temperature, &
          key_Poisson, &
          key_symmetry, &
          key_functional, &
          key_solver, &
          efield, &
          hfield, &
#ifdef GENERAL_CELL
          shifts=shifts, &
#endif
          scf=scf, &
          wfs=wfs, &
          pss=pss, &
          mdi=mdi, &
          doublegrid=doublegrid, &
          finitediff=finitediff, &
          frc=calc_force, &
          pdos=pdos, &
          pawdatapath=pawdatapath )
  if( ist /= 0 ) then
    if(o>0) write(o,'(4A,I0)') sym, fun, ERROR, 'initialize returned status = ',ist
    stop 'paw: error found parsing the input file.'
  endif ! initialize

  !=============================================================================
  !== Check, if there are kpoints, that require complex wave functions
  !== or if complex wave functions are used but not necessary
  !=============================================================================
cDBG  if(o>0) write(o,'(3A,L2)') sym, fun, 'all k-points half integer? ', all( half_integer( kpoints_all(:) ) )
cDBG  if(o>0) write(o,'(3A,I0)') sym, fun, 'R1_C2 = ', R1_C2
  if( all( half_integer( kpoints_all(:) ) ) .eqv. ( R1_C2 == 2 ) ) then
#if R1_C2 == 1
cDBG  if(o>0) write(o,'(3A)') sym, fun, 'complex wave functions required but real version was called.'
#else
cDBG  if(o>0) write(o,'(3A)') sym, fun, 'real wave functions can be used but the complex version was called.'
#endif
    selectcase( realcmplx )
    case( 0 ) ! 0:automatic decision about real or complex
      realcmplx = R1_C2 - 3 ! modify: 1 => -2, 2 => -1 ( realcmplx < 0 means silent initialization )
      if(o>0) write(o,'(3A,I0)') sym, fun, 'automatic decision about 1:real or 2:complex gives ', -realcmplx
      if(o>0) write(o,'(3A)') sym, fun, 're-start application with silent initialization!', '  ...'
      return
    case( 1 ) ; if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'k-point set requires complex wave functions!'
    case( 2 ) ; if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'system can be treated with real wave functions!'
cDBG  case( -1,-2) ; stop 'error: realcmplx negative!' ! is not allowed to have this discrepancy
cDBG  case default ; stop 'error: realcmplx not in {0,1,2}!!'
    endselect ! realcmplx

  elseif( realcmplx < 0 ) then
    ! for {-2,-1}, the program has been started with realcmplx==0 (auto) and found that the other type
    ! needs to be used, so the program has been restarted and the input file is read again but the output
    ! is silenced until this point
    if( o_silent > 0 ) ist = set_output_unit( o_silent ) ! recover the output unit

  endif ! the used wave function type does not match to this k-point set

#if R1_C2 == 1
  if(o>0) write(o,'(9A)') sym, fun, 'wave functions are REAL-valued.'
#else
  if(o>0) write(o,'(9A)') sym, fun, 'wave functions are COMPLEX-valued.'
#endif
  !=============================================================================

! #ifdef TIME
  time = Wtime() - time ! stop timing
  if(o>0) write(o,'(3A,F0.3,9A)') sym, fun, 'initialization needed ', time, ' sec'
! #endif


#ifndef USE_SYMMETRY
  if( key_symmetry > 0 .and. o>0) write(o,'(9A)') sym, fun, WARNING(0), 'activate the USE_SYMMETRY flag to use symmetry!'
#endif

  ! domain decomposition (DD)
  if( MPInprocs(world_comm) <= 1 .and. any( nxyzproc > 1 ) ) then
    ! running in serial, but the input file specifies a DD-pattern.
    if( checkmode > 0 ) then
      ! show space parallelization by domain decomposition
      i = display_decomp_ascii( modul=sym, unt=o, nproc=nxyzproc )
#ifdef EXTENDED
      i = display_decomp_xfig( name, nxyzproc )!, cell=gc%s )
#endif
    else  ! checkmode > 0
      ! real run (no checkmode):
      if(o>0) write(o,'(4A,3(" ",I0),9A)') sym, fun, WARNING(0), 'domain decomposition << domains', nxyzproc,' >> ignored.'
      nxyzproc = 1 ! ignore what the input file says about DD.
    endif ! checkmode > 0
  endif ! nprocesses > 1 and any nxyzproc > 1


  ! number of all atoms
  natoms = size( atoms, 1 )
cDBG  if(o>0) write(o,'(3A,I0)') sym, fun, 'natoms = ', natoms
  ! number of all kpoints
  nkpoints = size( kpoints_all, 1 )
  global%nkpoints = nkpoints
cDBG  if(o>0) write(o,'(3A,I0)') sym, fun, 'nkpoints = ', nkpoints

  ! global information container of type(info)
  global = set( nbands, nspins, nkpoints )

  global%temperature    = temperature
  global%ksymmetry      = key_symmetry
  global%kPoisson       = key_Poisson
  global%ksolver        = key_solver
  global%kfunctional    = key_functional
  global%kmixing        = key_mixing
  global%mixing         = mixingratio
  global%nhistory       = nhistory

#ifdef GENERELL_CELL
  ! shifts for non-rectangular unit cell (GENERAL_CELL)
  if(o>0) write(o,'(3A,3F6.3)') sym, fun, 'shifts =', shifts(1:3)
#endif

  ! set parameters of the real-space (coarse) grid
  gc = set( cellsize=cellsize, &
            ngridpoints=ngps, &
            nspins=NOCOSPINS, &  ! this means the 4th array dimension is 1 or 2
            bc=bc, & ! boundary conditions
#ifdef GENERELL_CELL
            shifts=shifts, &
#endif
            name='coarse grid' )

#ifndef INDEPENDENT_TASKS
  ist = dimensions( world_comm, bc, gc, global, nxyzproc, checkmode )
#else
  if(o>0) write(o,'(9A)') sym, fun, 'preprocessor option INDEPENDENT_TASKS set.'
  n = max( 0, nint( eval( '$indep', def=1. ) ) )
  ist = dimensions( world_comm, bc, gc, global, nxyzproc, checkmode, &
                    indep_nproc=n, indep_comm=indep_comm )
  i = MPImyrank( indep_comm )
  if( n > 1 ) then
    ia = mod( i + (n-1)/2, n ) - (n-1)/2 ! sample ia from 0, ..., +n/2, -(n-1)/2, ..., -1
    if( i > 0 .and. MPImaster( gc%comm ) .and. MPImaster( gc%equi_comm ) ) ist = set_output_unit( 100+ia )
    if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'start ', n, ' INDEPENDENT_TASKS'
    sint = min( max( 1E-4, eval( '$bulk', def=0.01 ) ), 0.05 ) ! default is 1%
    if(o>0) write(o,'(3A,9(A,F0.2))') sym, fun, 'variation of the lattice constant', &
      ' from ',sint*((1-n)/2)*100,'% to ',sint*(n/2)*100,'% in steps of ',sint*100,'%'
    cellsize = gc%s * ( 1. + sint * ia )
    if(o>0) write(o,'(A,2(I0,A),3(" ",F0.6),9A)') 'INDEPENDENT_rank= ',i,' of ',n,'  cell=', cellsize * Ang, Ang_
    gc = change_cell( gc, cellsize )
    do ia = 1, size(atoms) ; atoms(ia)%pos = atoms(ia)%relpos * cellsize ; enddo ! ia ! adjust absolute atomic positions
  else  ! n > 1
    if(o>0) write(o,'(9A)') sym, fun, 'compute $indep lattice constants in parallel. Specify the interval in $bulk.'
  endif ! n > 1
#endif
  if( ist /= 0 .and. o>0) write(o,'(4A,9(I0,A))') sym, fun, WARNING(0), 'distribution of MPI tasks returned status = ', ist

  ! create the real-space (dense) grid as 2 x the coarse grid
  gd = set( gc, Multiplier=2, nspins=NOCOSPINS*nspins, name='dense grid' )

  ! the electrostatix grid has Nscale times more grid points along each
  ! dimension corresponding to a spatial direction (x,y and z) than the dense grid
  ! usually nscale = 2, but 1, 3 and higher are in principle possible
  if( Nscale /= 1 .and. o>0) write(o,'(3A,9(I0,A))') sym, fun, 'es-grid = ', Nscale, ' x dense grid'

  ! create the real-space (electrostatix) grid as Nscale x the dense grid
  ge = set( gd, Multiplier=Nscale, nspins=1, name='electrostatix grid' )

  ! show all grid numbers
  if(o>0) write(o,'(9A)')           sym, fun, 'grids      x     y     z  s name'
  if(o>0) write(o,'(3A,3I6,I3,9A)') sym, fun, 'grid: ', gc%ng_all, ' ', trim(gc%name)
  if(o>0) write(o,'(3A,3I6,I3,9A)') sym, fun, 'grid: ', gd%ng_all, ' ', trim(gd%name)
  if(o>0) write(o,'(3A,3I6,I3,9A)') sym, fun, 'grid: ', ge%ng_all, ' ', trim(ge%name)
  if(o>0) write(o,'(3A,3I6,9A)')    sym, fun, 'BCs:  ', bc(:,1) ,'  0:isolated, 1:periodic boundary'


  ! set number of finite difference neighbors (both grids)
  gc%nf = min( finitediff(1), gc%ng_all(1:3)/gc%nproc(1:3) ) ! for the Kohn-Sham equation
  if( any( gc%nf < finitediff(1) ) ) then
    if(o>0) write(o,'(4A,3(" ",I0))') sym, fun, WARNING(0), 'reduce the finit-difference range to', gc%nf
!     stop 'paw: not enough grid points per domain for the requested range of finite-difference stencil.'
  endif ! not the same FD-order in all 3-directions
  gd%nf = 1 ! linear !! = finitediff(2) for the interpolation to ge
  ge%nf = min( finitediff(3), gd%ng_all(1:3)/gd%nproc(1:3) ) ! for the Poisson equation
  ! set size of the halos and order of interpolation (both grids)
  gc%nh = gc%nf
  gd%nh = gd%nf
  ge%nh = ge%nf

  ! if not in checkmode, show the set of calculation parameters
  !if( checkmode == 0 ) i = display_sample( modul=sym, unt=o, g=gc )


  ! display the atomic coordinates
  if( o > 0 ) then
    if( natoms > 0 ) then
      if( natoms < 99 ) then
        write(o,'(/,A)') BlockKeyWord_Atoms
        do ia = 1, natoms
          write(o,'(2A,3F10.3,A,3L2,9A)') atoms(ia)%s%sym,'  ',atoms(ia)%pos*Ang,'  ',( atoms(ia)%fix > 0. ),'  ',trim(atoms(ia)%label)
        enddo ! ia
        write(o,'(A,/)') BlockKeyWord_Atoms
      endif ! natoms not too many
    else  ! natoms > 0
      write(o,'(/,A,/)') 'no atoms!'
      write(o,'(9A)') sym, fun, WARNING(0), 'the sample does not contain any atoms!'
    endif ! natoms > 0
  endif ! o > 0


#ifdef DEBUG
  species_count(0:) = 0
  do ia = 1, natoms
    iZ = atoms(ia)%s%iZ
    species_count( iZ ) = species_count( iZ )+1 ! count up
  enddo ! ia
  if( species_count(0) > 0 ) then
    if(o>0) write(o,'(3A,I0,9A)') sym, fun, WARNING(0), species_count(0), ' vacuum-atoms detected!'
  endif ! pseudo atom
  nspecies = count( species_count(0:) > 0 ) ! also the vacuum atom Z=0 counts
  if(o>0) write(o,'(3A,I0)') sym, fun, 'number of different species: ',nspecies
  if( nspecies /= size(speci(:)) ) stop 'paw: different NSPECIES.'
  if(o>0) write(o,'(3A,120A3)') sym, fun, 'elements in this sample: ', speci(1:)%sym
#else
  nspecies = size( speci ) ! ; if(o>0) write(o,'(3A,I3)') sym, fun, 'nspecies: ', nspecies
#endif

cDBG  if(o>0) write(o,'(9A)') sym, fun, 'use simple mixing.'

  !=============================================================================
  !== Preparation of the Gaunt coefficients
  !=============================================================================
  ! initialize the numerically determined Gaunt-coefficients
  ! that are used for setting up the Delta tensor, which
  ! is species-dependent and later converts the atom-centered
  ! density matrix to the compensation charge multipoles qlm
  ! as well as the radial wave functions to radial densities
  ist = init_Gaunt( nsph=35, ellmax_prj=6, ellmax_rho=6 )
  !=============================================================================



  !=============================================================================
  !== Create an index list that says which atom will point to which species
  !=============================================================================
  allocate( ispecies(natoms) ) ; ispecies = 0
  do iatm = 1, natoms
cDBG  if( .not. associated(atoms(iatm)%s) ) then ! check if each atom has found its species
cDBG    if(o>0) write(o,'(4A,I0,A)') sym, fun, ERROR, 'the species pointer of atom #',atoms(iatm)%ja,' not associated!'
cDBG    stop 'paw ERROR: an atomic species pointer is not associated!'
cDBG  endif ! not associated(atoms(iatm)%s)
cDBG  if( atoms(iatm)%s%iZ == 0 .and. o>0) write(o,'(4A,I0,9A)') sym, fun, WARNING(0), 'atom #',atoms(iatm)%ja,' is a vacuum-atom!'
    do ispc = 1, nspecies
      if( atoms(iatm)%s%iZ == speci(ispc)%iZ ) ispecies(iatm) = ispc !! does not understand replaced species, e.g. core hole setups
    enddo ! ispc
    if( ispecies(iatm) == 0 ) stop 'paw ERROR: an atom%s%Z cannot be found in the list of species!'
  enddo ! iatm
  !=============================================================================




  !=============================================================================
  !== Loading of the precomputed PAWdata from files
  !=============================================================================
  do i = 1, len(pawdatapath)
    if( pawdatapath(i:i) == char(92) ) pawdatapath(i:i) = '/' ! replace backslashes with slashes
  enddo ! i
  if(o>0) write(o,'(9A)') sym, fun, 'look for existing PAWdata files in ', &
              trim(KeyWord(I_KeyWord_FPTH)), '="', trim(pawdatapath), '/".'
  time = Wtime() ! start timing
  do ispc = 1, nspecies

    ! create PAW data file name "pawdata.00Z" (e.g. "pawdata.029" for Cu)
    write(unit=pawdatafile,fmt='(2A,I3.3)',IOstat=ios) trim(adjustl(pawdatapath)), '/pawdata.', speci(ispc)%iZ
   
    if(o>0) write(o,'(9A)') sym, fun, 'look for existing file "', trim(pawdatafile), '"'

    ! read in the PAWdata from file pawdata.00Z
    ist = read_pawdata( filename=pawdatafile, &
            s=speci(ispc), &
            gridspacing=(/ maxval(gc%h),maxval(gd%h),maxval(ge%h) /), &
            doublegrid=doublegrid, &
            xc_functional=(key_functional .in. XCfnDict), &
            checkmode=checkmode, &
            xml_format=xml_format, &
            MPIcomm=world_comm ) ! if no communicator is passed, each process will read the file on its own

    if( ist /= 0 ) then
#ifdef EXTENDED
      ! failed to read pawdata.00Z file
      if( startmode/STARTMODE_GENERATE_PAWDATA > 0 ) then
        ! generate data
        if( MPImaster( world_comm ) ) &
          ist = generate( speci(ispc)%iZ, xc=global%kfunctional, checks=(checkmode>1), spin_polarized=(nspins>1) ) ! checkmode must be >1 for logarithmic derivatives
        call MPIbarrier( world_comm )
        if(o>0) write(o,'(9A)') sym, fun, 'PAWdata for ', trim(speci(ispc)%name), ' generated, please restart!'
        if(o>0) write(o,'(9A)') sym, fun, WARNING(1), trim(WARNING(0)), 's have been launched!'
        call MPIbarrier( world_comm )
        ist = comm_finalize( message='PAWdata generated' )
        stop ! PAWdata generated
      endif
      if(o>0) write(o,'(9A)') sym, fun, 'please run this application with --gen'
#endif
      if(o>0) write(o,'(9A)') sym, fun, ERROR, 'reading "', trim(pawdatafile), '" for ', trim(speci(ispc)%name)
      stop 'paw: something wrong while loading PAW data!'
    endif ! error

  enddo ! ispc

  do ispc =1,nspecies ! print all the generation strings
      if(o>0) write(o,'(9A)') sym, fun, "generation string for ", speci(ispc)%sym, ":", speci(ispc)%generator_string
  enddo ! ispc

! #ifdef TIME
  time = Wtime() - time ! stop timing
  if(o>0) write(o,'(3A,F0.3,9A)') sym, fun, 'loading of PAW data needed ', time, ' sec'
! #endif

  if( startmode/STARTMODE_GENERATE_PAWDATA > 0 ) then
    if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'PAWdata have been found for all species, run without --gen'
    call MPIbarrier( world_comm )
    ist = comm_finalize( message='PAWdata already present' )
    if( checkmode > 0 ) stop 'paw: generate option specified although PAWdata are available.'
    stop
  endif
  ! all requested PAW data loaded
  !=============================================================================



  !=============================================================================
  !== Set the atomic species pointers to the species containing the PAW data
  !=============================================================================
  do iatm = 1, natoms
    ! set the species pointer to the species in the list
    ! use the index list, because speci%Z may have changed when loading
    ispc = ispecies(iatm)
cDBG  if( ispc < 1 .or. ispc > nspecies ) then
cDBG    if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'ispc = ',ispc,' is out of bounds [1,',nspecies,']!'
cDBG    stop 'paw: fatal: ispc out of bounds!'
cDBG  endif ! out of bounds
    ! set the species pointer of the atoms to the species stored in speci
    ! speci only contains the species of atoms appearing in this project
    atoms(iatm)%s => speci(ispc)
  enddo ! iatm
  !=============================================================================


  !=============================================================================
  !== Determine the highest ell-cutoff of all species
  !=============================================================================
  ellmax = 2 * maxval( speci(:)%ellmax )
cDBG  if(o>0) write(o,'(3A,I0,9A)') sym, fun, &
cDBG     'largest ellmax of all species = ', ellmax, ' (potentials and densities in the spheres)'
  !=============================================================================

  !=============================================================================
  !== Preparation for the sphere transformation
  !=============================================================================
  ! create the points in the spherical shell and their transformation
  ! matrix to the representation in sperical harmonics.
  ! number of points in the spherical shell: 2*nsph*nsph
  ist = init_Xlm( nsph=8, ellmax=2*ellmax )
  ! the upper limit 2*ellmax covers the expansion of densities and
  ! potentials in the spheres that reaches 2*s%ellmax
  !=============================================================================


  !=============================================================================
  !== Check the number of electrons in the system
  !=============================================================================
  ! count the number of all valence electrons = sum( a(:)%s%Nve )
  totalcharge = addcharge ! init with the extra charge in the system
  do iatm = 1, natoms
    totalcharge = totalcharge + atoms(iatm)%s%Nve * atoms(iatm)%symmetry_factor
  enddo ! iatm
  global%charge = totalcharge
  if(o>0) write(o,'(3A,F0.6,A)') sym, fun, 'total valence charge is ', totalcharge, ' electrons.'
  if( totalcharge < 1.0 .and. o>0 ) write(o,'(9A)') sym, fun, WARNING(0), 'less than 1 valence electron in the cell!'
  if( addcharge /= 0. .and. o>0 ) write(o,'(4A,F0.3,9A)') sym, fun, WARNING(0), 'additional ', addcharge, ' electrons in the cell!'
  !=============================================================================


  !=============================================================================
  !== Check if there are enough bands to accommodate the electrons
  !=============================================================================
  nbands = global%nbands
  if( abs( global%charge*NOCOSPINS - 2*global%nbands ) < .001 ) then

    ! the Fermi surface may be squeezed.
    if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'minimal number of bands may result in a wrong Fermi surface!'
    ! this warning also applies to isolated systems, if the HOMO level is degenerate

  elseif( 2*global%nbands < global%charge*NOCOSPINS ) then

    if( global%nbands < 0 ) then
      ! automatic number of bands
      nbands = 0
      do ia = 1, size(atoms)
        nbands = nbands + optimal_n_orbitals( atoms(ia) )
      enddo ! ia
!       nbands = sum( optimal_n_orbitals( atoms ) ) ! this way the function opt.. must be elemental
      if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'automatic choice of ',nbands,' bands.'

    else  ! atomatic number of bands

      ! this is independent of the number of collinear spins, because
      ! for non-spin calculations, each band can accommodate 2.0 e
      nbands = ceiling( 0.5*global%charge*NOCOSPINS ) ! adjust the number of bands
      if(o>0) write(o,'(4A,F0.3,9(A,I0))') sym, fun, WARNING(0), &
        'cannot accomodate ', global%charge, ' electrons in ',global%nbands,' bands, adjust to ',nbands,' bands!'

    endif ! automatic number of bands

    ! redistribute the bands in band parallelization
    global%nbands = nbands ! new number of bands
    global%nbnd = parallelize_tasks( ntasks=global%nbands, & ! in
                                     nprocs=MPInprocs( global%band_comm ), & ! in
                                     irank=MPImyrank( global%band_comm ), & ! in
                                     ioffset=global%iobnd ) ! out
    if( MPInprocs( global%band_comm ) > max( 1, global%nbands ) ) then
      if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'more processes than bands, some procs will run idle!'
    endif ! more procs than bands

  endif ! not enough bands
  !=============================================================================



  !=========================================================================
  !== Parallelization of dependent tasks
  !=========================================================================
  nkpt = global%nkpt ! number of kpoints in this process
  allocate( kpt(nkpt), stat=ist )
cDBG  if( ist /= 0 .and. o>0) write(o,'(3A,I0)') sym, fun, 'kpt: allocation status = ',ist
  ! copy the kpoints that are processed in this PE
  kpt(1:nkpt) = kpoints_all(global%iokpt+1:global%iokpt+nkpt)
cDBG  if(nkpt < nkpoints .and. o>0) write(o,'(3A,9(I0,A))') sym, fun, 'k-points parallelized: ',nkpt,' of ',nkpoints,', offset ',global%iokpt
  if( nkpt < nkpoints .and. o>0) write(o,'(9A)') sym, fun, 'k-points parallelized!'

  nspn = global%nspn ! number of spins in this process
  if( nspn < nspins .and. o>0) write(o,'(9A)') sym, fun, 'collinear spins parallelized!'

  nbnd = global%nbnd ! number of bands in this process
  if( nbnd < global%nbands .and. o>0) write(o,'(9A)') sym, fun, 'bands parallelized!'
  !=========================================================================



  !=============================================================================
  !== Check if atomic positions fulfill the symmetry
  !=============================================================================
  if( .not. check_symmetry( gc, atoms, global%ksymmetry ) ) then
    if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'atoms not in symmetric positions!'
  endif ! symmetry fullfilled ?
  !=============================================================================





  mditerationtime = Wtime( ) ! start timing for md-steps

  !=============================================================================
  !== Prepare atoms
  !=============================================================================
  time = Wtime() ! start timing
  ! select for each atom:
  !   if an atom is centered in this domain,
  !     this domain will be the owner of the atom and the atom will appear again
  !     in the "atm" list
  !   if this domain contains parts of the compensation charge distribution or
  !     of the projectors, the atom will be in the "atm" list
  !   if this domain is too far away from the atom to contribute
  !     (no overlap between domain region and outermost cutoff radius),
  !     the atom will not appear in the list
  ! prepare atoms, allocate inner arrays, set values and offsets
  ! return the sum of numbers of all atomic projectors in this domain
  gc%naprj = prepare_atoms( atoms, atm, gc, ge, nspins=nspins, &
               interpolationO=doublegrid(1), meshrefinement=doublegrid(2), checkmode=(checkmode>0) )

! #ifdef TIME
  time = Wtime() - time ! stop timing
  if(o>0) write(o,'(3A,F0.3,9A)') sym, fun, 'preparation of atoms needed ', time, ' sec'
! #endif

  ! atm contains the atoms, that actively contribute in this domain
  if(o>0) write(o,'(9A)') sym, fun, 'atoms prepared.'
  !=============================================================================

#ifdef CONSTR
  !=============================================================================
  !== constrained DFT
  !=============================================================================
  ist = init_mask( name, totalcharge, gc, atoms, spin=(nspins==2) )
  if( ist /= 0 ) stop 'paw: initialization of cDFT masks failed!'
  !=============================================================================
#endif





  !=============================================================================
  !== Check Mode: Display some quantities, perform some checks, than exit
  !=============================================================================
  if( checkmode > 0 ) then

    ! write the atomic coordinates (in Angstrom) to a file called <projectname>.xyz
    if(o>0) i = write_xyz_file( name, atoms )

    i = find_bonds( o, atoms ) ! find possible bonds and bond lengths in the system

    call prepare_wavefunctions( spsi, psi_state, & ! Caution: some arrays are not yet allocated
                                atoms, gc, kpt, global, &
                                addcharge, checkmode=.true. )

    if( size(kpath,1) > 1 ) then
#if R1_C2 == 1
      if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'bandstructure calculation only implemented for complex wave functions!' ! in CheckMode only
#endif

#ifdef EXTENDED
      ! generate a densely sampled path of k-points connecting the edges
      i = generate_path( gc, kpath=kpath, gspacing=kpath_dk, allkpoints=kpathpoints )
#ifdef LCAO
           ! set up a basis of atomic orbitals and calculate their
           ! overlap to find a good set of start guess wave functions
           call LCAO_coefficients( o, a=atoms, g=gc, kpt=kpathpoints )
    else ; call LCAO_coefficients( o, a=atoms, g=gc )
#endif
#endif
    endif ! kpath has more than 1 edge kpoint


    ! show a set of calculation parameters and atomic positions
    i = display_sample( modul=sym, unt=o, g=gc, a=atoms, totalcharge=totalcharge )

    ! atoms are OK, display positions in ASCII graphics
    i = display_cell_ascii( modul=sym, unt=o, atm=atoms, s=gc%s )

    ! write a povray file for high-quality visualization
    allocate( tmp2(0:4,size(atoms)), stat=ist )
    do iatm = 1, natoms ; tmp2(0,iatm) = atoms(iatm)%s%iZ*1. ; tmp2(1:3,iatm) = atoms(iatm)%pos ; tmp2(4,iatm) = atoms(iatm)%s%rcut ; enddo ! iatm
#ifdef EXTENDED
    ist = display_atoms_povray( name, gc%s, tmp2, AtomRadiusScale=eval( '$povray_ars', def=1. ) )
#endif

    ! show a set of calculation parameters
    i = display_sample( modul=sym, unt=o, g=gc, totalcharge=totalcharge )

    if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'about to allocate ',nbnd,' x ',nspn,' x ',nkpt,' wave functions.'

#ifdef EXTENDED
    ! estimate memory !! the DoublePreprocessing variable R1_C2 is used here !
    i = estimate_memory( global, gc, nscale, wf_complex=( R1_C2 ==2), natoms=natoms, nhist=nhistory )
#endif

    if(o>0) write(o,'(9A)') sym, fun, 'CheckMode ends before main quantity arrays are allocated.'
    if(o>0) write(o,'(9A)') sym, ' ! ---------------- ', 'End --- of --- CheckMode', ' ------------------- !'

    ! WARNING(1) will return a string with the number of WARNING(0)s that have been called.
    if(o>0) write(o,'(9A)') sym, fun, WARNING(1), trim(WARNING(0)), 's have been launched!'

    call MPIbarrier( world_comm )
    ist = comm_finalize( message='EndOfCheckMode' )
    stop ! END OF CHECKMODE
 
  !=============================================================================
  else ! checkmode
  !=============================================================================

      !=============================================================================
      ! check if the domain distribution is working correctly using integers
      !=============================================================================

      ! coarse grid
      if( .not. test_dataexchange( gc ) ) stop 'test_dataexchange FAILED for gc, domain decomposition incorrect!'
cDBG  if(o>0) write(o,'(9A)') sym, fun, 'test_dataexchange gc: OK'

      ! dense grid
      if( .not. test_dataexchange( gd ) ) stop 'test_dataexchange FAILED for gd, domain decomposition incorrect!'
cDBG  if(o>0) write(o,'(9A)') sym, fun, 'test_dataexchange gd: OK'

      ! electrostatics grid
      if( .not. test_dataexchange( ge ) ) stop 'test_dataexchange FAILED for ge, domain decomposition incorrect!'
cDBG  if(o>0) write(o,'(9A)') sym, fun, 'test_dataexchange ge: OK'
      !=============================================================================

  endif ! checkmode
  !=============================================================================

  if( eval( '$xsf_density', def=0. ) > .1 ) then
#ifdef EXTENDED
    i = density2xsf( name, gd, atoms )
    stop 'xsf density file written.'
#else
    if(o>0) write(o,'(9A)') sym, fun, WARNING(0), ' $xsf_density export function only with -D EXTENDED'
#endif
  endif


#ifdef DEBUG
#ifdef EXTENDED
!   do ispc = 1, size(speci)
!     ist = test_species( speci(ispc), global )
!   enddo ! ispc
!   ist = test_all_projectors( atm, gc )

  do iatm = 1, size(atm)
    ist = test_projectors( atm(iatm), gc, 2*ellmax )
  enddo ! iatm

  do iatm = 1, size(atm)
    ist = test_compensation( atm(iatm), ge )
  enddo ! iatm
#endif
#endif



  !=============================================================================
  !== Allocation of grid quantities
  !=============================================================================
  i = 0 ! init sum of all allocation status flags

  ! on the COARSE GRID gc
  nx = gc%ng(1) ; ny = gc%ng(2) ; nz = gc%ng(3)

  ! 'Total potential on the coarse grid'
  allocate( vTOT_c(nx,ny,nz,nspins), stat=ist ) ; i=i+ist ; vTOT_c = 0.

  ! on the DENSE GRID gd
  nx = gd%ng(1) ; ny = gd%ng(2) ; nz = gd%ng(3)

  ! 'Total potential on the dense grid'
  allocate( vTOT_d(nx,ny,nz,nspins), stat=ist ) ; i=i+ist ; vTOT_d = 0.

  ! 'Smooth valence density on the dense grid'
  allocate( rhov(nx,ny,nz,nspins), stat=ist ) ; i=i+ist ; rhov = 0.

  ! on the ELECTROSTATIX GRID ge
  nx = ge%ng(1) ; ny = ge%ng(2) ; nz = ge%ng(3)

  ! 'Total potential on ge'
  allocate( vTOT_e(nx,ny,nz,nspins), stat=ist ) ; i=i+ist ; vTOT_e = 0.

  ! 'Electrostatic potential on ge'
  allocate( ves(nx,ny,nz), stat=ist ) ; i=i+ist ; ves = 0.

  ! 'smooth core densities on ge'
  allocate( rhoc(nx,ny,nz,nspins), stat=ist ) ; i=i+ist ; rhoc = 0.

  ! 'smooth repulsive potential on ge'
  allocate( vBAR(nx,ny,nz,nspins), stat=ist ) ; i=i+ist ; vBAR = 0.

#ifdef JELLIUM
    ! Jellium switched on
    if(o>0) write(o,'(9A)') sym, fun, 'set up jellium.'
    ! 'fixed jellium density (often negative) on ge'
    allocate( rho_jellium(nx,ny,nz), stat=ist ) ; i=i+ist ; rho_jellium = 0.
#endif

  if( i /= 0 ) then
    if(o>0) write(o,'(4A,I0)') sym, fun, ERROR, 'failed to allocate grid quantities, status = ',i
    stop 'paw: ERROR failed to allocate grid quantities!'
  endif ! ist /= 0


  !=============================================================================
  !== Preparation of
  !==      -- the smooth core charge density on the dense grid
  !==      -- the correction potential on the dense grid
  !=============================================================================
cDBG  if(o>0) write(o,'(9A)') sym, fun, 'prepare smooth correction.'

  ist = smooth_correction( atoms, ge, rhoc, vBAR )
  ! check for negative values
  if( any( rhoc < 0. ) ) then
    if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'smooth core density has negative entries!'
  endif ! any rhoc < 0

cDBG  if(o>0) write(o,'(9A)') sym, fun, 'smooth correction prepared.'

#ifdef DEBUG
   if( size(atm,1) > 0 ) then
     ist = test_smooth_correction( atoms(1), ge, rhoc(:,:,:,1), name=name+'.rhoc' )
     ist = test_smooth_correction( atoms(1), ge, vBAR(:,:,:,1), name=name+'.vbar' )
     if(o>0) write(o,'(A)') '', & ! suggest plotting
       ' xmgrace dmp/rhoc_dense_grid -nxy dmp/rhoc_radial_grid &', &
       '', ' xmgrace dmp/vbar_dense_grid dmp/vbar_radial_grid &', ''
   endif ! natm > 1
#endif

  ! allocate coarse valence density
  call allocate_rhov_c(gc, global)

  !=============================================================================
  !== Allocation of wave functions
  !=============================================================================
  ! total number of wave functions
  nbsk = nbnd * nspn * nkpt
  nxyzs = product( gc%ng(1:4) )

cDBG  if(o>0) write(o,'(3A,9F16.6)') sym, fun, 'sum of kpoint-weights:', sum(kpoints_all(:)%w), sum(kpt(:)%w)

  ! set up Kohn-Sham state descriptors
  allocate( psi_state(nbsk), stat=ist )
  if( ist /= 0 ) then
    if(o>0) write(o,'(4A,I0)') sym, fun, ERROR, 'failed to state descriptors and projection coefficients, status = ', ist
    stop 'paw: ERROR failed to allocate wave functions.'
  endif ! ist /= 0
  
  ibsk = 0
  do ikpt = 1, nkpt
    do ispn = 1, nspn
      do ibnd = 1, nbnd
        ibsk = ibsk + 1
        psi_state(ibsk) = set( (/ ibnd+global%iobnd, ispn+global%iospn, ikpt+global%iokpt /), kpt(ikpt)%w )
      enddo ! ibnd
    enddo ! ispn
  enddo ! ikpt

cDBG  sint = sum(psi_state(:)%wgt) .MPIsum. gc%equi_comm
cDBG  if(o>0) write(o,'(3A,F16.6)') sym, fun, 'sum of all state weights:', sint

  !!! from here on, the versions differ depending on REAPLEX and R1_C2

  if(o>0) write(o,'(3A,F0.3,9A)') sym, fun, 'local size of each wf is ', R1_C2 *nxyzs*.5**7,' kiByte'

  allocate( cpsi(gc%naprj,nbsk), stat=ist ) ; cpsi = 0.
  if( ist /= 0 ) then
    if(o>0) write(o,'(4A,F0.3,A,I0)') sym, fun, ERROR, &
      'failed to allocate ', R1_C2 *real(gc%naprj)*real(nbsk)*.5**27,' GiByte for projection coefficients, status = ', ist
    stop 'paw: ERROR failed to allocate projection coefficients.'
  endif ! ist /= 0

cDBG  if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'allocate ',nbnd,' x ',nspn,' x ',nkpt,' = ',nbsk,' wave functions and coefficient vectors.'
  allocate( spsi(nxyzs,nbsk), stat=ist )
  if( ist /= 0 ) then
    if(o>0) write(o,'(4A,F0.3,A,I0)') sym, fun, ERROR, &
      'failed to allocate ', R1_C2 *real(nxyzs)*real(nbsk)*.5**27,' GiByte for wave functions, status = ', ist
    stop 'paw: ERROR failed to allocate wave functions.'
  endif ! ist /= 0
  spsi = 0.

  !=============================================================================
  !== Assembly of the Task list
  !=============================================================================
  task_list = KEY_TASK_NOTHING ! init
  task_list(1:14) =(/ KEY_TASK_WFS_GENERATE, & ! #1
                      KEY_TASK_RHO_FROM_WFS, & ! #2
                      KEY_TASK_NOTHING, &      ! empty
                      KEY_TASK_RHO_RELAX, &    ! #4 relax electronic degrees of freedom until self-consistency
                      KEY_TASK_NOTHING, &
!                       KEY_TASK_POTENTIAL2RADIAL, & ! inactive
                      KEY_TASK_DENSITY_OF_STATES, & ! total density of states
                      KEY_TASK_RHO_STORE, & ! in this position, checkpointing of the density between the MD-iterations is done
                      KEY_TASK_WFS_STORE, & ! in this position, checkpointing of the wave functions between the MD-iterations is done
                      KEY_TASK_FORCES, & ! compute forces
                      KEY_TASK_POS_RELAX, & ! possible jump to task_list entry #4
                      KEY_TASK_ATOMIC_ANALYSIS, & ! contains also projected density of states analysis
!                       KEY_TASK_RHO_STORE, & ! store density only after structure has been relaxed
!                       KEY_TASK_WFS_STORE, & ! store the wave functions only at the end
                      KEY_TASK_BANDSTRUCTURE, &
                      KEY_TASK_NOTHING, &
                      KEY_TASK_NOTHING /)

#ifdef INDEPENDENT_TASKS
  if( .not. MPImaster( indep_comm ) ) task_list(5:10) = KEY_TASK_NOTHING
#endif

  ! start mode modifies the task list
  i = modulo( startmode, STARTMODE_GENERATE_PAWDATA )
  if( i == 1 .or. i == 3 ) task_list(1) = KEY_TASK_WFS_LOADOLD ! start mode load wfs = 1
  if( i == 2 .or. i == 3 ) task_list(2) = KEY_TASK_RHO_LOADOLD ! start mode load rho = 2

  ! throw away the tailing number of tasks
  ntasks = size( task_list )
  do while( task_list( ntasks ) == KEY_TASK_NOTHING .and. ntasks > 1 )
    ntasks = ntasks-1
  enddo ! while
  !=============================================================================

  mdit = 0 ! init the MD-iteration counter


!=============================================================================
!== Start the TASK loop
!=============================================================================


itask = 0
do while( itask < ntasks )
  itask = itask + 1
  ktask = task_list( itask )
  if(o>0) write(o,'(A)') ! empty line per task


  if( ktask == KEY_TASK_WFS_GENERATE ) then
    if(o>0) write(o,'(9A)') sym, fun, 'generate start wave functions.'

#ifdef PREPARE_PLANE_WAVES
    if(o>0) write(o,'(9A)') sym, fun, 'use only plane waves as start guess.'
    ! creates preliminary only plane waves as! inital
    ! wave functions, sets analytical plane wave energies
    call prepare_wf_planewaves( spsi, psi_state, &
                                gc, kpt, global, checkmode=.false. )
    call Fermidistribution( psi_state, nele=global%charge, &
              temperature=global%temperature, FermiLevel=FermiLevel, &
              comm=gc%equi_comm, dos_ef=dos_eF )
#else

    ! create preliminary atomic orbitals or plane waves as
    ! inital wave functions, sets preliminary energies and
    ! atomic occupation numbers
    call prepare_wavefunctions( spsi, psi_state, &
                                atoms, gc, kpt, global, &
                                addcharge, checkmode=.false. )

#ifdef CONSTR
    do i = 1, n_constraints
      psi_state(:)%cocc(i) = psi_state(:)%occ
    enddo ! i
#endif

#endif
    call MPIbarrier( gc%comm ) ! wait for all
    if(o>0) write(o,'(9A)') sym, fun, 'wave functions prepared.'


  endif ! ktask == KEY_TASK_WFS_GENERATE



  if( ktask == KEY_TASK_RHO_FROM_WFS ) then
    if(o>0) write(o,'(9A)') sym, fun, 'generate smooth density from wave functions.'
    ist = rhov_from_psi( global, kpt, &
                         gc, gd, &
                         global%ksymmetry, &
                         atm, &
                         cpsi, &
                         spsi, & ! on gc
                         psi_state, &
                         rhov ) ! on gd
  endif ! ktask == KEY_TASK_RHO_FROM_WFS



  if( ktask == KEY_TASK_RHO_RELAX ) then
    if(o>0) write(o,'(9A)') sym, fun, 'relax density self-consistently.'
    itask_relax_again = itask ! set jump mark for MD-loop

    totalenergy = relax_density( name, global, gc, gd, ge, nscale &
              , atm & ! parallelized list of atoms
              , kpt & ! parallelized list of k-points
              , vBAR & ! correction potential on ge
              , rhoc & ! smooth core density on ge
              , rhov & ! smooth valence charge density on gd
              , spsi & ! smooth wave functions on gc
              , cpsi & ! projection coefficients
              , psi_state & ! KS-state descriptors
              , efield, hfield & ! electric and magnetic fields
              , ves &  ! electrostatic potential on ge
              , vTOT_e, vTOT_d, vTOT_c & ! total potential on ge, on gd, on gc
              , FermiLevel=FermiLevel & ! Fermi energy
              , scf=scf, wfs=wfs, pss=pss & ! convergence criteria for SCF, wave functions and Poisson
#ifdef JELLIUM
              , rho_jellium=rho_jellium & ! optional: Jellium density on ge
#endif
               )

#ifdef DEBUG
#ifdef EXTENDED
    ist = visualize_density( 'dmp/'+name, rho=rhov, comm=gd%comm, nscale=1 ) ! each grid point will be nscale x nscale pixels

    ibsk = 0 ; do ikpt = 1, nkpt ; do ispn = 1, nspn ; do ibnd = 1, nbnd ; ibsk = ibsk+1
!     pawdatafile,'(3A,9(I0,A))') 'dmp/'+name+'.'+(ikpt+global%iokpt)+'.'+(ispn+global%iospn)+'.'+(ibnd+global%iobnd)
      ist = visualize_density( 'dmp/'+name+'.'+(ikpt+global%iokpt)+'.'+(ispn+global%iospn)+'.'+(ibnd+global%iobnd), &
             rho=reshape( abs(spsi(:,ibsk))**2, gc%ng(1:4) ), nscale=1, comm=gc%comm )
    enddo ; enddo ; enddo ! ibnd, ispn, ikpt
#endif
#endif

  endif ! ktask == KEY_TASK_RHO_RELAX


  if( ktask == KEY_TASK_POTENTIAL2RADIAL ) then
#ifdef EXTENDED
#ifdef DEBUG
    if( MPImaster(gc%equi_comm) ) then
      if(o>0) write(o,'(9A)') sym, fun, 'writes spherical potential files.'
      ! spherically averaged potentials for core relaxation
      do ia = 1, size(atoms)
        if( ge%ng(1) > gd%ng(1) ) & ! write only if grid e is even denser than grid d
        ist = grid2radial( ge, vTOT_e(:,:,:,1), atoms(ia)%imagepos(:,1:atoms(ia)%nimages), &
                          atoms(ia)%s%g%r, file='vtot_e.a'+atoms(ia)%ja, comm=ge%comm )
        ist = grid2radial( gd, vTOT_d(:,:,:,1), atoms(ia)%imagepos(:,1:atoms(ia)%nimages), &
                          atoms(ia)%s%g%r, file='vtot_d.a'+atoms(ia)%ja, comm=gd%comm )
        ist = grid2radial( gc, vTOT_c(:,:,:,1), atoms(ia)%imagepos(:,1:atoms(ia)%nimages), &
                          atoms(ia)%s%g%r, file='vtot_c.a'+atoms(ia)%ja, comm=gc%comm )
      enddo ! ia
      if(o>0) write(o,'(9A)') sym, fun, 'writes spherical density files.'
      do ia = 1, size(atoms)
        ist = grid2radial( gd, rhov(:,:,:,1), atoms(ia)%imagepos(:,1:atoms(ia)%nimages), &
                          atoms(ia)%s%g%r, file='rhov_d.a'+atoms(ia)%ja, comm=gd%comm )
        ist = grid2radial( ge, rhoc(:,:,:,1), atoms(ia)%imagepos(:,1:atoms(ia)%nimages), &
                          atoms(ia)%s%g%r, file='rhoc_e.a'+atoms(ia)%ja, comm=ge%comm )
      enddo ! ia
    endif ! MPImaster(gc%equi_comm)
#endif
#endif
  endif ! ktask == KEY_TASK_POTENTIAL2RADIAL




  if( ktask == KEY_TASK_DENSITY_OF_STATES ) then
    if(o>0) write(o,'(9A)') sym, fun, 'create total density of states.'

    ! total Density of States
    if( MPImaster( gc%comm ) ) then
      ! this is a task only for the cell masters of each space region
      ist = density_of_states( name=name, global=global, psi=psi_state, &
              FermiLevel=FermiLevel, temperature=temperature, comm=gc%equi_comm, comment='total DoS' )
    endif ! gc master

  endif ! ktask == KEY_TASK_DENSITY_OF_STATES





  if( ktask == KEY_TASK_FORCES .and. any( calc_force ) ) then
    if(o>0) write(o,'(9A)') sym, fun, 'find forces.'

    frcall = force_calculation( & ! returns average force
                            global, & ! global information container
                            gc, gd, ge, & ! coarse grid, dense grid, potential grid
                            atm, & ! parallelized list of atoms
                            atoms, & ! list of all atoms
                            spsi, & ! smooth wave functions [on gc]
                            psi_state, & ! KS-state descriptors
                            vTOT_e, & ! local potential [on ge]
                            vBAR, & ! local potential correction [on ge]
                            rhov, & ! smooth valence density [on gd]
                            ves, & ! electrostatic potential [on ge]
                            kpt, & ! parallelized kpoint list
                            interpolationO=doublegrid(1), &
                            meshrefinement=doublegrid(2), &
                            directions=calc_force ) ! compute force in these directions

    ist = display_forces( sym, o, atoms, frcall )

    ! write a file <projectname>.frc that contains positions and forces
    if( MPImaster( gc%equi_comm ) .and. MPImaster( gc%comm ) ) &
      ist = write_force_file( name, atoms, gc, totalenergy )

  endif ! ktask == KEY_TASK_FORCES



  if( ktask == KEY_TASK_POS_RELAX ) then
    ! decide wheter the positions are updated and the density is relaxed again

    call MPIbarrier( gc%comm )      ! synchronization of grid processes
    call MPIbarrier( gc%equi_comm ) ! synchronization of state parallelizations
    if(o>0) write(o,'(3A,I0,9(A,F0.1))') sym, fun, 'MD iteration #',mdit,' needed ', Wtime( )-mditerationtime, ' sec'
    mditerationtime = Wtime( )      ! start new iteration

    ! find the largest force of the movable(fix>0) atoms
    maxfrc = 0.
    do ia = 1, size(atoms)
      maxfrc = max( maxfrc, maxval( abs( atoms(ia)%frc * atoms(ia)%fix ) ) )
    enddo ! ia

    ! stop criterion?
    if( ( maxfrc > mdi%thres .or. mdit < mdi%minit ) .and. ( mdit < mdi%maxit ) ) then

      if(o>0) write(o,'(3A,F0.6,9A)') sym, fun, 'relax atomic positions, largest |force| is ', maxfrc*eV/Ang, eV_/Ang_

      ! write a file each md-iteration <projectname>.md13.frc that contains positions and forces of iteration #13
      if( MPImaster( gc%equi_comm ) .and. MPImaster( gc%comm ) ) &
        ist = write_force_file( name+'.md'+mdit, atoms, gc, totalenergy )

#ifdef EXTENDED
cDBG  ist = visualize_density( 'dmp/'+name+'.md'+mdit, rho=rhov, comm=gd%comm, nscale=2 ) ! each grid point will be nscale x nscale pixels
#endif
      ! store the atomic density matrices
      ist = write_aDm( name+ADM_FileNameExtension, atm, owners=atoms(:)%owner, nspins=nspins, gridcomm=gc%comm )

      call MPIbarrier( gc%comm )
      call MPIbarrier( gc%equi_comm )

      ist = free_atoms( atm ) ! deallocate all arrays of atoms in the parallelized list of atoms and the list itself

      allocate( tmp3(1:3,size(atoms),3), stat=ist )

      do ia = 1, size(atoms)
        tmp3(1:3,ia,1) = atoms(ia)%pos ! store old position
        tmp3(1:3,ia,2) = atoms(ia)%frc * atoms(ia)%fix ! store old forces
      enddo ! ia

      ! simple position update (steepest descent)
      maxdsp = 0.3
      if(o>0) write(o,'(3A,F0.3,9A)') sym, fun, 'update atomic positions with atan(F) * ',maxdsp*Ang, Ang_
      do ia = 1, size(atoms)
        tmp3(1:3,ia,3) = tmp3(1:3,ia,1) + maxdsp * atan( tmp3(1:3,ia,2) ) ! arctangent as saturating function --> Pi/2
      enddo ! ia

      call MPIbcast0( tmp3(:,:,3), global%cart_calc_comm ) ! master process tells all other processes

      ! adopt new positions
      do ia = 1, size(atoms)
! #ifdef DEBUG
        if(o>0) write(o,'(3A,3F12.6,9(A,I0))') sym, fun, 'position at ', tmp3(1:3,ia,1)*Ang, ' (atom #',atoms(ia)%ja,')'
        if(o>0) write(o,'(3A,3F12.6,9(A,I0))') sym, fun, ' ..moves to ', tmp3(1:3,ia,3)*Ang, Ang_
! #endif
        atoms(ia)%pos = tmp3(1:3,ia,3) ! new positions
        atoms(ia)%relpos = atoms(ia)%pos / gc%s ! new relative positions
      enddo ! ia
      showpos = any( tmp3(:,:,3) /= tmp3(:,:,1) ) ! show new positions at the end of relaxation

      maxdsp = maxval( abs( tmp3(:,:,3)-tmp3(:,:,1) ) )
      if(o>0) write(o,'(3A,F0.9,9A)') sym, fun, 'largest |displacement| is ', maxdsp*Ang, Ang_


      deallocate( tmp3, stat=ist )



      mdit = mdit+1 ! count up MD-iterations
      if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'start MD iteration #',mdit

      ! prepare atoms (again)
      gc%naprj = prepare_atoms( atoms, atm, gc, ge, nspins=nspins, &
                   interpolationO=doublegrid(1), meshrefinement=doublegrid(2), checkmode=.false. )
cDBG  if(o>0) write(o,'(9A)') sym, fun, 'atoms prepared again.'
      ist = smooth_correction( atoms, ge, rhoc, vBAR )
cDBG  if(o>0) write(o,'(9A)') sym, fun, 'smooth correction prepared again.'

      ! load the atomic density matrices. The atom ownership may have changed due to changed positions
      ist = read_aDm( name+ADM_FileNameExtension, atm, owners=atoms(:)%owner, nspins=nspins, gridcomm=gc%comm )
      if( ist /= 0 .and. o>0) write(o,'(9A)') sym, fun, 'atomic density matrix loading failed!'

cDBG  if(o>0) write(o,'(3A,I0)') sym, fun, 'jump to TASK_RHO_RELAX = task #',itask_relax_again
      itask = itask_relax_again-1 ! manipulate the task-loop: jump to TASK_RHO_RELAX

    else  ! go on
      if(o>0 .and. any( calc_force ) ) write(o,'(3A,F0.6,9A)') sym, fun, 'largest |force| is ', maxfrc*eV/Ang, eV_/Ang_
    endif ! go on
    if(o>0 .and. any( calc_force ) ) write(o,'(3A,2(I0,A),F0.6,9A)') sym, fun, &
      'criteria for md max ', mdi%maxit, ' min ', mdi%minit, ' < ', mdi%thres*eV/Ang, eV_/Ang_

  endif ! ktask == KEY_TASK_POS_RELAX



  if( ktask == KEY_TASK_BANDSTRUCTURE .and. allocated(kpath) ) then
#ifdef EXTENDED
    if(o>0) write(o,'(9A)') sym, fun, 'compute bandstructure.'

    ! Bandstructure calculation
    selectcase( size(kpath) )
    case( 1 )    ; if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'no bandstructure with 1 path edge.'
    case( 2: )   ! start bandstructure calculation
      if(o>0 .and. nspins > 1 ) write(o,'(9A)') sym, fun, WARNING(0), 'bandstructure only for spin #1.'
#if R1_C2 == 1
      ! real wave functions, need to convert the start wave functions to complex (could be memory demanding)
      if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'needs to convert the start wave functions to complex.'
#endif
      i = bandpath( name, global, gc, nbnd, nxyzs, &
            atm, vTOT_c, jspn=1, & ! coarse grid local potential on gc
            kpath=kpath, checkmode=.false., &
#if R1_C2 == 1
            wf_start=cmplx( spsi(:,1:nbnd) ), & ! SC wf of 1st SC kpoint as initial guess wf
#else
            wf_start=spsi(:,1:nbnd), & ! SC wf of 1st SC kpoint as initial guess wf
#endif
            wf_start_kp=kpt(1), &               ! 1st SC kpoint
            FermiLevel=FermiLevel, &
            gspacing=kpath_dk, &
            outputunit=8, & ! serial output to unit 8
            comm=gc%equi_comm ) ! kpoint_communicator should be used here
            ! the same communicator that is used for
            ! the distribution of the SC kpoints
    case default ; if(o>0) write(o,'(9A)') sym, fun, 'no bandstructure requested.'
    endselect ! #of points in kpath
#else
    if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'please activate -D EXTENDED features for band structure!'
#endif
  endif ! ktask == KEY_TASK_BANDSTRUCTURE






  if( ktask == KEY_TASK_ATOMIC_ANALYSIS ) then
    do i = 1, size(pdos,1)     
      ist = projected_density_of_states( name, pdos(i), gc, speci, atoms, atm, global, spsi, cpsi, psi_state, temperature, FermiLevel )
    enddo ! i
#ifdef EXTENDED
!+ extended

#ifdef DEBUG
!+ debug
    if( eval( '$check_PAW_assumption', def=0. ) > .1 ) then
      call check_PAW_assumption( name, gc, atm, spsi, cpsi, psi_state, FermiLevel )
    endif
!- debug
#endif

    if(     eval( '$visualize_all_wfs',   def=0. ) > .1 &
       .or. eval( '$visualize_all_wfs_z', def=0. ) > .1 ) then
      ist = visualize_all_wfs( name, global, gc, spsi )
    elseif( eval( '$visualize_all_wfs_y', def=0. ) > .1 ) then
      ist = visualize_all_wfs( name, global, gc, spsi, direction='y' )
    elseif( eval( '$visualize_all_wfs_x', def=0. ) > .1 ) then
      ist = visualize_all_wfs( name, global, gc, spsi, direction='x' )
    endif ! $visualize_all_wfs

!- extended
#endif
  endif ! ktask == KEY_TASK_ATOMIC_ANALYSIS





  if( ktask == KEY_TASK_WFS_LOADOLD ) then

 
    if(o>0) write(o,'(9A)') sym, fun, 'load wave functions from file.'


    ist = read_wfs( spsi, gc, name+WFS_FileNameExtension, global, &
                    psi_state(:)%ene, psi_state(:)%occ, global%cart_calc_comm )

    if( ist == 0 ) then
      if(o>0) write(o,'(9A)') sym, fun, 'wave functions loaded.'
    else  ! success
!       if(o>0) write(o,'(4A)') sym, fun, ERROR, 'load option was passed', &
!         sym, fun, ' ...  but wave function file not found or content does not match!'
!       stop 'paw: loading of wave functions failed!'
      ! alternative:
      if(o>0) write(o,'(9A)') sym, fun, 'loading of wave functions failed --> generate.'
      task_list( itask ) = KEY_TASK_WFS_GENERATE
      itask = itask-1 ! manipulate task list, rewind one step
    endif ! success

#ifdef CONSTR
    do i = 1, n_constraints
      psi_state(:)%cocc(i) = psi_state(:)%occ
    enddo ! i
#endif
  endif ! ktask == KEY_TASK_WFS_LOADOLD



  if( ktask == KEY_TASK_WFS_STORE ) then
    if(o>0) write(o,'(9A)') sym, fun, 'store wave functions.'

    writewfs = eval( '$write_wfs', def=1. ) ! SoftSwitch
    if( writewfs > 0. ) then
      ist = write_wfs( spsi, gc, name+WFS_FileNameExtension, global, &
                        psi_state(:)%ene, psi_state(:)%occ, global%cart_calc_comm )
      if( ist /= 0 .and. o>0) write(o,'(9A)') sym, fun, WARNING(0), 'writing of smooth wave functions failed!'
#ifdef FULL_DEBUG
      !===================================================
      !== confirm wave function read-write works 2-ways ==
      !===================================================
      if(o>0) write(o,'(9A)') sym, fun, 'try to confirm wave function writing.'
      allocate( tmp2(size(spsi,1),size(spsi,2)) )
      ! read wave functions again ...
      ist = read_wfs( tmp2, gc, name+WFS_FileNameExtension, global, &
                        psi_state(:)%ene, psi_state(:)%occ, global%cart_calc_comm )
      ! ... and confirm that they have been stored properly!
      if( ist /= 0 ) then
        if(o>0) write(o,'(9A)') sym, fun, 'confirm-reading wave functions failed!'
      elseif( any( tmp2 /= spsi ) ) then
        if(o>0) write(o,'(9A)') sym, fun, ERROR, 'wave functions differ after storing!'
      else
        if(o>0) write(o,'(9A)') sym, fun, 'confirm-reading wave functions successful.'
      endif ! ist
      deallocate( tmp2 )
      !==================================================
#endif
    else
      if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'wave function disabled by $write_wfs < 1'
    endif ! writewfs
#if R1_C2 == 1
    writewfscube = eval( '$write_wfs_cube', def=0. ) ! SoftSwitch
    if( writewfscube > 0. ) then
      ist = write_wfs_cube( spsi, gc, atoms, name, global )
      if( ist /= 0 .and. o>0) write(o,'(9A)') sym, fun, WARNING(0), 'writing of smooth wave functions failed!'
    else
      if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'wave function disabled by $write_wfs_cube < 1'
    endif
#endif

  endif ! ktask == KEY_TASK_WFS_STORE



  if( ktask == KEY_TASK_RHO_LOADOLD ) then
    if(o>0) write(o,'(9A)') sym, fun, 'load smooth density from file.'

    ! try to load an existing charge density
    ist = read_density( rhov, gd, name+RHO_FileNameExtension, global )
    if( ist /= 0 ) then
      if(o>0) write(o,'(4A)') sym, fun, ERROR, 'load option was passed', &
        sym, fun, ' ...  but density file not found or content does not match!'
      stop 'paw: ERROR loading of density failed!'
!         ! alternative:
!         if(o>0) write(o,'(9A)') sym, fun, 'loading of  density failed --> use wave functions'
!         task_list( itask ) = KEY_TASK_RHO_FROM_WFS
!         itask = itask-1 ! manipulate task list, rewind one step
    else  ! failed
!       loaded = .TRUE.
      sint = integral( rhov, gd )
      if(o>0) write(o,'(3A,F0.3,9A)') sym, fun, 'smooth density loading successful, total charge ', sint, ' e.'
    endif ! failed

    ! try to load an existing ASCII atomic density matrix file
    ist = read_aDm( name+ADM_FileNameExtension, atm, owners=atoms(:)%owner, nspins=nspins, gridcomm=gc%comm )
    if( ist /= 0 ) then
      if(o>0) write(o,'(9A)') sym, fun, ERROR, 'load option was passed but reading the atomic density matrix file failed.'
      stop 'paw: ERROR loading of the atomic density matrices failed!'
!         ! alternative:
!         if(o>0) write(o,'(9A)') sym, fun, 'loading of  density failed --> use wave functions'
!         task_list( itask ) = KEY_TASK_RHO_FROM_WFS
!         itask = itask-1 ! manipulate task list, rewind one step
    else  ! failed
!       loaded = .TRUE.
      if(o>0) write(o,'(9A)') sym, fun, 'atomic density matrix loading successful.'
#ifdef FULL_DEBUG
      ! new trial from module INOUT: write in ASCII file
      ist = write_aDm( name+'.adm2', atm, owners=atoms(:)%owner, nspins=nspins, gridcomm=gc%comm )
      stop 'DEBUG main line 2244: compare *.adm and *.adm2 now!'
#endif
    endif ! failed

  endif ! ktask == KEY_TASK_RHO_LOADOLD



  if( ktask == KEY_TASK_RHO_STORE ) then
    if(o>0) write(o,'(9A)') sym, fun, 'store density.'

    if( MPImaster( gc%equi_comm ) ) then

      ist = write_aDm( name+ADM_FileNameExtension, atm, owners=atoms(:)%owner, nspins=nspins, gridcomm=gc%comm )
      if( ist /= 0 .and. o>0) write(o,'(9A)') sym, fun, WARNING(0), 'writing of atomic density matrices failed!'

      ist = write_density( rhov, gd, name+RHO_FileNameExtension, global )
      if( ist /= 0 .and. o>0) write(o,'(9A)') sym, fun, WARNING(0), 'writing of smooth grid density failed!'

#ifdef DEBUG
      if( .not. MPIparallel( gd%comm ) ) then
        ist = display_VTK_file( name, gd, rhov(:,:,:,1) )
        if( ist /= 0 .and. o>0) write(o,'(4A,I0,9A)') sym, fun, WARNING(0), 'writing of VTK file failed! ', ist, 'k'
      endif !
#endif

    endif ! MPImaster of gc%equi_comm
#ifdef FULL_DEBUG
    call MPIbarrier( gc%equi_comm ) ! synchronize
    !===============================================
    !== confirm densities read-write works 2-ways ==
    !===============================================
    if(o>0) write(o,'(9A)') sym, fun, 'try to confirm density writing.'
    allocate( tmp4(gd%ng(1),gd%ng(2),gd%ng(3),size(rhov,4)) )
    ! read wave functions again ...
    ist = read_density( tmp4, gd, name+RHO_FileNameExtension, global )
    ! ... and confirm that they have been stored properly!
    if( ist /= 0 ) then ;                if(o>0) write(o,'(9A)') sym, fun, 'confirm-reading densities failed!'
    elseif( any( tmp4 /= rhov ) ) then ; if(o>0) write(o,'(9A)') sym, fun, ERROR, 'densities functions differ after storing!'
    else ;                               if(o>0) write(o,'(9A)') sym, fun, 'confirm-reading densities successful.'
    endif ! ist
    deallocate( tmp4 )
    !==================================================
#endif

  endif ! ktask == KEY_TASK_RHO_STORE




enddo ! itask
!=============================================================================
!== End of the TASK loop                                                ======
!=============================================================================

#ifdef  OUTPUT_CENTRAL_POTENTIAL
    call output_potential( gc, vTOT_c(:,:,:,1), 543 )
    call output_potential( ge, vBAR(:,:,:,1), 500 )
#endif

    deallocate( spsi, cpsi, stat=ist ) ! deallocate REAPLEX type quantities

    deallocate( vTOT_e, vTOT_d, vTOT_c, ves, rhov, rhoc, vBAR, &
      rho_jellium, psi_state, kpt, kpoints_all, kpath, speci, stat=ist )

    ! display the final atomic coordinates
    if( o > 0 .and. showpos .and. natoms > 0 ) then
      write(o,'(A)') ''
      if( Ang /= 1.0 ) write(o,'(2A)') trim(KeyWord(I_KeyWord_APOS)), Ang_
      write(o,'(A)') BlockKeyWord_Atoms
      do ia = 1, natoms
        write(o,'(2A,3F16.6,9A)') atoms(ia)%s%sym,'  ', atoms(ia)%pos*Ang !, '      ', trim(atoms(ia)%label)
      enddo ! ia
      write(o,'(A)') BlockKeyWord_Atoms, ''
    endif ! showpos

    ! show the calculation parameters again
    i = display_sample( modul=sym, unt=o, g=gc, totalcharge=totalcharge ) ! without atomic positions

    ! show how many warnings have been launched
    if(o>0) write(o,'(9A)') sym, fun, WARNING(1), trim(WARNING(0)), 's have been launched.'
    if(o>0) write(o,'(A,F16.6,A)') 'TOTAL ENERGY', totalenergy*eV, eV_

    call MPIbarrier( world_comm ) ! synchronize all tasks

#ifdef INDEPENDENT_TASKS
    n = MPInprocs( indep_comm )
    if( n > 1 ) then
      i = MPImyrank( indep_comm )
      if(o>0) write(o,'(/,A,2(I0,A),9(F0.9,A))') 'INDEPENDENT_rank= ',i,' of ',n,'  totalenergy= ', totalenergy * eV, eV_
      allocate( tmp2(0:n-1,0:2), stat=ist )
      tmp2 = 0.0 ; tmp2(i,0:2) = (/ 1.0, cellsize(1), totalenergy /) ! set checklist(1.0), my parameter and my result
      call MPIallsum( tmp2, indep_comm ) ! gather results
      if( any( tmp2(:,0) /= 1.0 ) .and. o>0) write(o,'(/,9A)') '# INDEPENDENT results corrupted!' ! checklist
      if(o>0) write(o,'(/,9A)') '# INDEPENDENT results in', Ang_, ' and', eV_
      do ia = -(n-1)/2, n/2 ; i = mod( ia + n, n ) ! order the lattice constants smallest to largest
        if(o>0) write(o,'(F10.6,9F18.12)') tmp2(i,1)*Ang, tmp2(i,2)*eV
      enddo ! i
      if(o>0) write(o,'(/)') ! empty line
      ist = indep_fit_lattice_param( o, avals=tmp2(:,1)*Ang, Evals=tmp2(:,2)*eV )
    endif ! n > 1
#endif

    ist = comm_finalize( name ) ! finalize the MPI environment
    if(o>0) close( unit=o, iostat=ios ) ! try to close the output file
  endfunction ! start



#ifdef INDEPENDENT_TASKS
!+ independent_tasks
#if R1_C2 == 1
!+ r1_c2 == 1

  status_t function indep_fit_lattice_param( o, avals, Evals ) result( info )
  use configuration, only: WARNING, ERROR
  implicit none
    ! parameters
    integer, parameter              :: mdof = 4
    character(len=*), parameter     :: fun = 'indep_fit_lattice_param: '
    ! arguments
    iounit_t, intent(in)            :: o
    real, intent(in)                :: avals(1:), Evals(1:)
    ! local vars
    integer                         :: nvals, ndof, np1
    real :: A(size(avals,1),mdof), b(size(avals,1)), work(2*size(avals,1)*mdof), c(0:mdof-1), x0

    nvals = size(avals,1)
    if( size(Evals,1) < nvals ) then
      if(o>0) write(o,'(9A)') sym, fun, ERROR, 'fitting needs ', nvals, ' energy values!'
      info = -1
      return
    endif ! not enough Energy values

    do ndof = mdof-1, mdof ! loop runs over 3 and 4

      if( ndof > nvals ) then
        if(o>0) write(o,'(9A)') sym, fun, ERROR, 'fitting ', ndof, ' params to ', nvals, ' values is overdetermined!'
        info = 16
        return
      endif ! overdetermined, impossible

      if( ndof == nvals ) then
        if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'fit completely determined!'
      endif ! determined, no fitting

      do np1 = 1, mdof
        A(:,np1) = avals(:)**(np1-1) ! generate powers of the x-axis
      enddo
      b = Evals(1:nvals) ! copy the vector to be fitted

      call DGELS( 'N', nvals, ndof, 1, A, size(A,1), B, size(B,1), work, size(work), info )

      if( info /= 0 ) then
        if(o>0) write(o,'(4A,I0)') sym, fun, WARNING(0), 'Lapack DGELS returned info = ', info
        return
      endif ! info

      c = 0. ; c(0:ndof-1) = b(1:ndof) ! copy the resulting coefficients

      selectcase( ndof-1 )
      case( 2 ) ! parabola
        if(o>0) write(o,'(3A,9(F0.6,A))') sym, fun, 'parabolic minimum@ ', -c(1)/(2.*c(2)), ' Emin= ', c(0)-c(1)**2/(4.*c(2)), ' curvature= ', 2.*c(2)
      case( 3 ) ! plus cubic part
        x0 = ( sqrt( c(2)**2 - 3.*c(1)*c(3) ) - c(2) )/( 3.*c(3) ) ! location of the minimum
        if(o>0) write(o,'(3A,9(F0.6,A))') sym, fun, 'cubic     minimum@ ', x0, ' Emin= ', c(0)+x0*(c(1)+x0*(c(2)+x0*c(3))), ' curvature= ', 2.*c(2)+6.*c(3)*x0
      case default
        if(o>0) write(o,'(3A,9(" ",F0.6))') sym, fun, 'coefficients=', c(0:ndof-1)
      endselect ! ndof-1

    enddo ! ndof

  endfunction ! indep_fit_lattice_param

!- r1_c2 == 1
#endif
!- independent_tasks
#endif


#ifdef EXTENDED
!+ extended features

#if R1_C2 == 1
  subroutine check_PAW_assumption_r &
#else
  subroutine check_PAW_assumption_c &
#endif
       (name, g, a, spsi, cpsi, psi_state, FermiLevel )
  use configuration, only: o, WARNING
  use type_grid, only: grid
  use type_atom, only: atom
  use type_state, only: state
  use type_info, only: info
#ifdef DEBUG
  use MPItools, only: MPIallsum, operator(.MPImax.), MPIbarrier
!   use toolbox, only: cat
#endif
  use type_rgrid, only: operator(.at.)
  use harmonics, only: Xlmax_rl
  use constants, only: Pi
  use type_species, only: I_SMT, SPIN
  use unitsystem, only: eV, eV_
  implicit none
    ! parameters
    character(len=*), parameter           :: fun = ' PAW_assumption: '
    ! arguments
    character(len=*), intent(in)          :: name
    type(grid), intent(in)                :: g !! coarse grid
    type(atom), intent(in)                :: a(1:) !! parallelized list of atoms
    REAPLEX, intent(in)                   :: spsi(1:,1:) ! (nxyzs,nbsk) smooth wave functions
    REAPLEX, intent(in)                   :: cpsi(1:,1:) ! (naprj,nbsk) projection coefficients
    type(state), intent(in)               :: psi_state(1:) !! parallelized list of state descriptors
    real, intent(in)                      :: FermiLevel
    ! local vars
    integer                   :: v=13, u=11, nbsk, ia, ii, i1, i2, i3, ibsk, ixyz, ir, ilm, iln, ell, emm, enn
    real                      :: rv(3), r2s, rs, rcut2, mask, maskw, rmax, rmx2
    REAPLEX                   :: s!, slm(25)
    real                      :: diff(0:5), Xlm(25), rmell
    integer, parameter        :: ELLs(25) = (/ 0, 1,1,1, 2,2,2,2,2, 3,3,3,3,3,3,3, 4,4,4,4,4,4,4,4,4 /)
    integer                   :: ios
    character(len=32)         :: fn

    nbsk = size(spsi,2)
#ifdef DEBUG
    if(o>0) write(o,'(9A)') sym, fun, 'start'
    if( size(cpsi,1) /= g%naprj ) stop ': dim#1 of CPSI does not match g%NAPRJ!'
    if( size(cpsi,2) /= nbsk ) stop ': dim#2 of CPSI does not match NBSK!'
    if( size(psi_state,1) /= nbsk ) stop 'pdos: dim of PSI_STATE does not match NBSK!'
#endif

    if(o>0) write(o,'(9A)') sym, fun, 'run'
    do ia = 1, size(a,1)

      rcut2 = maxval( a(ia)%s%fprj(:)%rcut )**2
      if(o>0) write(o,'(3A,I0,A,F10.6)') sym, fun, 'ia= ', ia, ' Rcut=', sqrt(rcut2)
      if(o>0) write(o,'(3A,I0,A,I0)') sym, fun, 'ia= ', ia, ' offprj= ', a(ia)%offprj
      rmax = sqrt(2.*rcut2)
      rmx2 = rmax*rmax

      write(unit=fn,fmt='(3A,9(I0,A))') 'dmp/', trim(name),'.a', ia
      v = 13 ; open(unit=v,file=fn,action='write',iostat=ios)
      if( ios /= 0 ) v = 0 ! no writing
      if(v>0) write(v,'(A,I0,9A)') 'atom #',ia,' energy in', eV_

      do ibsk = 1, nbsk
!         write(unit=fn,fmt='(3A,9(I0,A))') 'dmp/', trim(name),'.a', ia, '.bsk', ibsk
!         u = 11 ; open(unit=u,file=fn,action='write',iostat=ios)
!         if( ios /= 0 ) u = 0 ! no writing
        u = 0 ! no writing
        !===============================================================================
          maskw = 0.
          diff = 0. ! init
          ixyz = 0
          do i3 =     1, g%ng(3) ; rv(3) = i3*g%h(3) + g%off(3) - a(ia)%pos(3)
            do i2 =   1, g%ng(2) ; rv(2) = i2*g%h(2) + g%off(2) - a(ia)%pos(2)
              do i1 = 1, g%ng(1) ; rv(1) = i1*g%h(1) + g%off(1) - a(ia)%pos(1)
                ixyz = ixyz+1  ! count up

                  r2s = rv(1)*rv(1)+rv(2)*rv(2)+rv(3)*rv(3)
                  if( r2s < rmx2 ) then
                  !===============================================================================
                    mask = ( 1. - (r2s/rmx2)**8 )**2
                    maskw = maskw + mask * g%hvol

                    rs = sqrt(r2s)
                    Xlm = Xlmax_rl( 4, rv ) ! get 25 spherical harmonics

                    if( rs > 1E-3 ) then
                      ir = a(ia)%s%g .at. rs
                      s = 0.
                      iln = 0
                      ii = a(ia)%offprj ! offset of the projection coefficients of this atom
                      rmell = 1. ! rs^-ell
                      do ell = 0, a(ia)%s%ellmax
                        do enn = 1, a(ia)%s%nn(ell)
                          iln = iln+1
                          ilm = ell**2 ! offset
                          do emm = -ell, ell
                            ilm = ilm+1
                            ii = ii+1
                            s = s + cpsi(ii,ibsk) * a(ia)%s%rwf(ir,iln,SPIN,I_SMT) * Xlm(ilm) * rmell
                          enddo ! emm
                        enddo ! enn
                        rmell = rmell/rs
                      enddo ! ell
                      if(u>0) write(u,'(9F16.6)') rs, spsi(ixyz,ibsk), s

#if R1_C2 == 1
#define cpp_ABS
#else
#define cpp_ABS abs
#endif
                      diff(0) = diff(0) + cpp_ABS ( spsi(ixyz,ibsk) ) **2 * mask
                      diff(1) = diff(1) + cpp_ABS ( s ) **2 * mask
                      diff(2) = diff(2) + cpp_ABS ( ( s - spsi(ixyz,ibsk) ) ) **2 * mask
                      diff(3) = diff(3) + cpp_ABS ( ( s + spsi(ixyz,ibsk) ) ) **2 * mask
#undef cpp_ABS
                    endif ! rs > 1E-3
                endif ! |r| < rcut
              enddo ! ix
            enddo ! iy
          enddo ! iz
          if(u>0) close(u)
          if(o>0) write(o,'(3A,I0,A,I0,9F16.6)') sym, fun, 'ia= ', ia, ' ibsk= ', ibsk, diff(0:3), maskw
          if(v>0) write(v,'(9F16.6)') (psi_state(ibsk)%ene-FermiLevel)*eV, diff(2)/maskw, diff(2)/diff(0)
        !===============================================================================
      enddo ! ibsk
      if(v>0) close(v)
      if(o>0) write(o,'(3A,I0,A,F10.6)') sym, fun, 'ia= ', ia, ' Mask=', maskw
    enddo ! ia
cDBG  if(o>0) write(o,'(9A)') sym, fun, 'end'
  endsubroutine ! check_PAW_assumption



#endif


#if R1_C2 == 1
  !! documentation for real and complex version of relax_density
  !!
  !! relaxes the density self-consitently for a given atomic geometry
  !!
  !! The PAW environment, i.e. the projectors on the real-space grid have to be set already.
  real function relax_density_r( &
#else
  !! please see documentation of <a href="#relax_density_r">relax_density_r<a/> above
  real function relax_density_c( &
#endif
                        name, &
                        global, gc, gd, ge, nscale, &
                        atm, &
                        kpt, &
                        vBAR, & ! local correction potential on ge
                        rhoc, & ! smooth core charge density on ge
                        rhov, & ! smooth valence charge density on gd
                        spsi, cpsi, psi_state, & ! wave functions on gc, projection coefficients, state information
                        efield, hfield, &
                        ves, & ! electrstatic potential on ge
                        vTOT_e, vTOT_d, vTOT_c, & ! total potential on ge, on gd, on gc
                        FermiLevel, &
                        scf, wfs, pss, &
                        rho_jellium ) & ! optional
  result( etotal ) ! total energy
  use configuration, only: o ! output unit
  use configuration, only: WARNING, ERROR
  use type_grid, only: grid
  use type_info, only: info
  use type_state, only: state
  use type_kpoint, only: kpoint
  use type_atom, only: atom
  use type_criteria, only: criteria, go_on
  use FermiDirac, only: Fermidistribution
  use toolbox, only: operator(+)
  use unitsystem, only: eV, eV_, Ang, Ang_
#ifdef CONSTR
  use constraints, only: constrain ! on or off?
  use constraints, only: nc => n_constraints ! number of constraints
  use constraints, only: qc => q_constraints ! charge in each mask
  use constraints, only: ec => e_constraints ! Fermi-level in mask
  use constraints, only: wc => w_constraints ! weight of each mask
#endif
  use display, only: display_states
  use selfcon, only: normalize_states
  use selfcon, only: rhov_from_psi
  use selfcon, only: pot_from_rho
  use selfcon, only: relax_eigenstates
  use selfcon, only: sum_of_eigenvalues
!   use toolbox, only: cat
#ifdef EXTENDED
  use toolbox, only: radial_hist
  use toolbox, only: visualize_density
  use subspace, only: Hamiltonian2CRS
  use subspace, only: Hamiltonian2file
  use subspace, only: Hamiltonian2ASCII
#endif
  use MPItools, only: operator(.MPIsum.)
  use MPItools, only: operator(.MPImax.)
  use MPItools, only: operator(.MPIdiff.)
  use MPItools, only: operator(.MPIaverage.)
  use MPItools, only: MPIbarrier
  use MPItools, only: MPImaster
  use MPIconst, only: MPI_Comm_WORLD
  use MPIconst, only: Wtime
  use control, only: scf_run_control
  use input, only: eval
  use mixing, only: mix
#ifdef FULL_DEBUG
  use FermiDirac, only: density_of_states
  use toolbox, only: radial_hist
  use toolbox, only: operator(+)
#endif
    character(len=*), intent(in)      :: name !! project name
    type(info), intent(in)            :: global !! global information container
    type(grid), intent(in)            :: gc, gd, ge !! coarse and dense and electrostatix grid descriptor
    integer, intent(in)               :: nscale
    type(atom), intent(inout)         :: atm(:) !! parallelized list of atoms i.e. only those, which have an nonvanishing overlap of the non-local projectors with the volume of this domain
    type(kpoint), intent(in)          :: kpt(:) !! parallelized self-consistent kpoints, i.e. only those which will be computed by this set of processes
    real, intent(in)                  :: vBAR(:,:,:,:) !! dims(gd%ng(1),gd%ng(2),gd%ng(3),nspin) <br> local correction potential on the <a href="#ge">e-grid</a>
    real, intent(in)                  :: rhoc(:,:,:,:) !! dims(gd%ng(1),gd%ng(2),gd%ng(3),nspin) <br> smooth core density on the <a href="#ge">e-grid</a>
    real, intent(inout)               :: rhov(:,:,:,:) !! dims(gd%ng(1),gd%ng(2),gd%ng(3),nspin) <br> smooth valence density on the <a href="#gd">dense grid</a>
    REAPLEX, intent(inout)            :: spsi(:,:) !! dims(nxyzs,nbsk) <br> smooth Kohn-Sham wave functions on gc
    REAPLEX, intent(inout)            :: cpsi(:,:) !! dims(gc%naprj,nbsk) <br> projection coefficients
    type(state), intent(inout)        :: psi_state(:) !! dims(nbsk) <br> Kohn-Sham state information container, hold energy, occupation, weight, etc.
    real, intent(in)                  :: efield(1:3), hfield(1:3) !!
    real, intent(inout)               :: ves(:,:,:) !! dims(gd%ng(1),gd%ng(2),gd%ng(3)) <br> electrostatic (Coulomb) potential on the <a href="#ge">e-grid</a>
    real, intent(out)                 :: vTOT_e(:,:,:,:) !! dims(ge%ng(1),ge%ng(2),ge%ng(3),nspin) <br> total potential on ge
    real, intent(out)                 :: vTOT_d(:,:,:,:) !! dims(gd%ng(1),gd%ng(2),gd%ng(3),nspin) <br> total potential on the <a href="#gd">dense grid</a>
    real, intent(out)                 :: vTOT_c(:,:,:,:) !! dims(gc%ng(1),gc%ng(2),gc%ng(3),nspin) <br> total potential on the <a href="#gc">coarse grid</a>
    real, intent(out)                 :: FermiLevel !! the Kohn-Sham states have been occupied up to this energy (if no temperature smearing ...)
    type(criteria), intent(in)        :: scf !! convergence criteria for the self-consistency iterations
    type(criteria), intent(in)        :: wfs !! convergence criteria for the wave functions
    type(criteria), intent(inout)     :: pss !! convergence criteria for the iterative solution of the Poisson equation
    real, intent(in), optional        :: rho_jellium(:,:,:) !! jellium density on ge <br> external constant charge distribution, default = 0.

#ifdef DEBUG
    character(len=*), parameter       :: fun = ' relax_density: '
#else
    character(len=*), parameter       :: fun = ': '
#endif
    character(len=*), parameter       :: th_(0:4) = (/'th','st','nd','rd','th'/)
    integer, parameter                :: I_RUN  = 1
    integer, parameter                :: I_STOP = 0

    real    :: mixratio
    real    :: residual
    real    :: mam_vloc(-1:+1)
    real    :: temperature ! Fermi Dirac smearing temperature
    real    :: dos_ef ! density of states at the Fermi level
    real    :: holu_gap ! energy gap at the Fermi level

    integer :: nbnd ! number of bands
!   integer :: nspins ! number of collinear spins
    integer :: nspn ! number of collinear spins
    integer :: nkpt ! number of k-points
    integer :: nbsk ! # of (bands x spins x kpoints)
    integer :: nxyzs ! # of degrees of freedom for a wf

    real, allocatable  :: vXC(:,:,:,:) ! (nx,ny,nz,0:ns) ! exchange correlation potential (and energy:is=0) on ge
    real, allocatable  :: rhot(:,:,:,:) ! (nx,ny,nz,ns) ! smooth density plus smooth core densities on ge
    real, allocatable  :: rho_aug(:,:,:) ! (nx,ny,nz) ! smooth density plus augmentation charges on ge

    ! smooth wave functions
    REAPLEX, allocatable :: Sspsi(:,:) ! adjoint wave functions for one sk-set of bands
    integer  :: iscf, i, iit_wfs, nx, ny, nz, ns, js
    status_t :: ist, n0
    logical  :: run, run_wfs
    real     :: res_wfs, time, skp_time !, skp_fraction

#ifdef CONSTR
    integer                           :: ic
#endif

#ifdef FULL_DEBUG
    integer                           :: ibnd
#endif

#ifdef DEBUG
    integer                           :: id
  ! checks
  do id = 1, 3
    if( size( vBAR, id ) /= ge%ng(id) ) stop 'relax: a dim of VBAR is wrong.'
    if( size( rhoc, id ) /= ge%ng(id) ) stop 'relax: a dim of RHOC is wrong.'
    if( size( rhov, id ) /= gd%ng(id) ) stop 'relax: a dim of RHOV is wrong.'
    if( size( ves, id ) /= ge%ng(id) ) stop 'relax: a dim of VES is wrong.'
    if( size( vTOT_e, id ) /= ge%ng(id) ) stop 'relax: a dim of vTOT_e is wrong.'
    if( size( vTOT_d, id ) /= gd%ng(id) ) stop 'relax: a dim of vTOT_d is wrong.'
    if( size( vTOT_c, id ) /= gc%ng(id) ) stop 'relax: a dim of vTOT_c is wrong.'
  enddo ! id
#endif

  etotal = 0. ! init

  !=============================================================================
  !== Allocation of grid quantities                                       ======
  !=============================================================================
  i = 0 ! init sum of all allocation status flags
  nx = ge%ng(1) ; ny = ge%ng(2) ; nz = ge%ng(3) ; ns = global%nspins
  ! exchange correlation potential ( and eXC=vXC(:,:,:,0) ) on ge
  allocate( vXC(nx,ny,nz,0:ns), stat=ist ) ; i=i+ist ; vXC = 0.
  ! smooth density plus smooth core densities on ge
  allocate( rhot(nx,ny,nz,ns), stat=ist ) ; i=i+ist ; rhot = 0.
  ! smooth density plus augmentation charges on ge
  allocate( rho_aug(nx,ny,nz), stat=ist ) ; i=i+ist ; rho_aug = 0.

  if( i /= 0 ) then
    if(o>0) write(o,'(4A,I0)') sym, fun, ERROR, 'failed to allocate grid quantities, status = ',i
    stop 'paw: ERROR failed to allocate grid quantities.'
  endif ! i /= 0

  !=============================================================================
  !== Allocation of adjoint wave functions                                ======
  !=============================================================================

  nbnd = global%nbnd
  nspn = global%nspn
  nkpt = global%nkpt

  nbsk  = nbnd * nspn * nkpt ! total number of wave functions
  nxyzs = product( gc%ng(1:4) ) ! total number of grid points

  ! allocate only one set of bands to save memory
  ! use nbnd+1, because if bands are parallelized, nbnd may differ by 1 among the processes.
  allocate( Sspsi(nxyzs,nbnd+1), stat=ist )
#ifdef DEBUG
  if(o>0) write(o,'(3A,I0,A,F0.3,9A)') sym, fun, 'allocated ',nbnd+1,' adjoint wave functions, ', nxyzs*(nbnd+1)*0.5**17, ' MiByte'
#endif
  if( ist /= 0 ) then
    write(*,'(4A,F0.3,A,I0)') sym, fun, ERROR, 'failed to allocate ', nxyzs*(nbnd+1)*0.5**17, ' MiByte adjoint wave functions, status = ',ist
    stop 'paw: ERROR failed to allocate adjoint wave functions.'
  endif ! ist /= 0
  Sspsi = 0. ! init

  residual = mix( gd, atm, rhov, global%kmixing, mixratio, nhistory=global%nhistory ) ! init history

  iscf = 0 ! init SCF loop counter
  run = go_on( scf )
  do while( run )
    iscf = iscf+1

    if(o>0) write(o,'(/99A)')  sym, ' ', ('=',i=1,64) ! line
    if(o>0) write(o,'(3A,I0)') sym, fun, 'iteration #', iscf
    if(o>0) write(o,'(99A/)')  sym, ' ', ('=',i=1,64) ! line

    mixratio = max( 0.0, min( global%mixing, 1.0 ) )
    temperature = max( 0.0, global%temperature ) ! Fermi smearing temperature
cDBG  if(o>0) write(o,'(3A,ES10.2E2,A,F6.3,9A)') sym, fun, 'Temperature =', temperature, '  mixing =', mixratio

    time = Wtime() ! start timing

    ! generate new potential and calculate the total energy
    etotal = pot_from_rho( &
                global, &
                gc, gd, ge, nscale, &
                key_symmetry=global%ksymmetry, &
                key_Poisson=global%kPoisson, &
                Poisson=pss, &
                a=atm, &
                sumeigvals=sum_of_eigenvalues( gc, psi_state ), &
                efield=efield, hfield=hfield, &
                rhov_d=rhov, & ! on gd
                rhoc=rhoc, & ! on ge
                rhot=rhot, & ! on ge
                rho_aug=rho_aug, & ! on ge
                vBAR=vBAR, & ! on ge
                vXC=vXC, & ! on ge
                ves=ves, & ! on ge
                vTOT=vTOT_e, & ! on ge
                vTOT_d=vTOT_d, & ! on gd
                vTOT_c=vTOT_c, & ! on gc
                rho_jellium=rho_jellium ) ! on ge

    skp_time = Wtime() ! start timing for then sk-parallel part

    ! show the total energy and scf-residual
    if(o>0) write(o,'(3A,I4,A,EN22.12E1,2A,F7.2,A,ES9.2E2)') sym, ': ', &
      'it#', iscf, ' total energy', etotal*eV, eV_, '  log', log10( max( residual, 0.1**99.99 ) ), '  res', residual

#ifdef FULL_DEBUG
    ! check if the local potential on the coarse grid is the same along equi_comm
    ! Warning: very expensive operation!
    if( reshape(vTOT_c,(/size(vTOT_c)/)) .MPIdiff. gc%equi_comm ) then
      if(o>0) write(o,'(9A)') sym, fun, 'vTOT_c differs between equivalent space processes!'
      stop 'paw: fatal: vTOT_c differs along the eqivalent space communicator!'
    elseif(o>0) then ; write(o,'(9A)') sym, fun, 'vTOT_c .MPIdiff. gc%equi_comm is .FALSE.'
    endif ! diff
#endif

    iit_wfs = 0 ; run_wfs = .true. ! init
    do while( run_wfs )
      iit_wfs = iit_wfs+1

      ! Solve for the eigenstates of the new Kohn-Sham Hamiltonian using
      ! the KS wave functions of the previous iteration as a start guess
      res_wfs = relax_eigenstates( &
                  iscf, &             ! SCF iteration counter
                  gc, &               ! coarse grid
                  atm, &              ! parallelized atoms list
                  global, &
                  kpt, &              ! list of kpoints
                  vTOT_c, &           ! local potential [on gc]
                  spsi, &             ! smooth wave functions on gc
                  Sspsi, &            ! adjoint wave functions ( Overlap x wf )
                  psi_state, &        ! state information objects
                  key_solver=global%ksolver, & ! solver or solver mode (stable,fast,...)
                  precond=.true. )    ! use the preconditioner

      n0 = normalize_states( &
            iscf, &             ! SCF iteration counter
            gc, &               ! coarse grid descriptor
            atm, &              ! local list of atoms
            global%nbnd, &      ! local # of bands
            global%nspn, &      ! local # of spins
            global%nkpt, &      ! local # of kpoints
            kpt, &              ! local list of kpoints
            spsi, &             ! local smooth wave functions
            psi_state, &        ! local state descriptors
            cpsi )              ! local projection coefficient vector (out)

      if( iscf > 1 ) then
        run_wfs = go_on( wfs, iit_wfs, res_wfs )
        if( run_wfs ) then
          if(o>0) write(o,'(2A,ES9.2E2,A,I0,9A)') sym, ': relax states to', res_wfs, &
                              ' in the ', iit_wfs, th_(min(iit_wfs,4)),' solver step.'
        else  ! iit_wfs
          if(o>0) write(o,'(2A,ES9.2E2,A,I0,9A)') sym, ': relax states to', res_wfs, ' in scf-iteration #', iscf
        endif ! iit_wfs
      else  ! iscf > 1
        run_wfs = .false. ! do not solve more than once in SCF iteration#1 (only SR)
      endif ! iscf > 1

    enddo ! while


#if R1_C2 == 1
#ifdef FULL_DEBUG
  ! project KS states onto bessel functions in the region around (0,0,0) each iteration
  do ibnd = 1, global%nbnd
    ist = radial_hist( data=reshape(spsi(:,ibnd),gc%ng(1:3)), &
      hg=gc%h, origin=-gc%off, ellmax=2, file='dmp/wf_r.'+ibnd+'.scf'+iscf, comm=gc%comm )
  enddo ! ibnd
#endif
#endif


#ifdef CONSTR
    if( constrain ) then ! the module can be switched off (2nd line in the .cDFT input file)
                         ! so only the regular occupation numbers are computed and then copied.
      do ic = 1, nc ! for all constraints
        ist = Fermidistribution( global%nspins, psi_state, nele=qc(ic,1), &
                temperature=temperature, FermiLevel=ec(ic,1), &
                comm=gc%equi_comm, weights=psi_state(:)%cwgt(ic), occ=psi_state(:)%cocc(ic) )
        if(o>0) write(o,'(3A,I0,A,EN16.6,2A,9(F10.6,A))') sym, fun, 'constraint #',ic,' Fermi-level', ec(ic,1)*eV, eV_,', weight', wc(ic), ' charge', qc(ic,1), ' e'
      enddo ! ic
      FermiLevel = sum( wc*ec(:,1) ) ! weighted average for Fermi level
      if(o>0) write(o,'(3A,EN16.6,A)') sym, fun, 'average Fermi-level', FermiLevel*eV, eV_
    else  ! constrains
#endif

    ! compute new occupation numbers
    ist = Fermidistribution( global%nspins, psi_state, nele=global%charge, &
              temperature=temperature,  FermiLevel=FermiLevel, &
              comm=gc%equi_comm, dos_ef=dos_ef, holu_gap=holu_gap )

#ifdef CONSTR
      if(o>0) write(o,'(3A,EN16.6,2A,9(F10.6,A))') sym, fun, 'constraints OFF, Fermi-level', FermiLevel*eV, eV_,', charge ', global%charge, ' e'
      ! copy psi_state%occ into special numbers
      do i = 1, nc
        psi_state(:)%cocc(i) = psi_state(:)%occ
      enddo ! i
    endif ! constrains
#endif

#ifdef FULL_DEBUG
    ! Density of States every SCF iteration
    if( gc%rank == 0 ) then
      ! this is a task only for the cell masters of each space region
      ist = density_of_states( name=name+'.'+iscf, global=global, psi=psi_state, &
              FermiLevel=FermiLevel, temperature=temperature, comm=gc%equi_comm )
    endif ! gc%equi_rank
#endif

    ! create new density
    ist = rhov_from_psi( &
            global, kpt, &
            gc, gd, &
            global%ksymmetry, &
            atm, & ! and new atomic density matrices atm(:)%Dm(:,:,is)
            cpsi, &
            spsi, & ! on gc
            psi_state, &
            rhov ) ! on gd

    skp_time = Wtime() - skp_time ! stop timing for then sk-parallel part

    ! mix the density on the dense grid and the density matrices in the atomic spheres
    residual = mix( gd, atm, rhov, global%kmixing, mixratio )

! cDBG  call visualize_density( 'dmp/'+name+'.'+iscf, rho=rhov, comm=gd%comm )

    run = scf_run_control( name, comm=global%cart_calc_comm, eta=mixratio, temp=temperature )
    run = run .and. go_on( scf, iscf, residual )

    time = Wtime() - time ! stop timing
! #ifdef TIME
    if(o>0) write(o,'(3A,F0.1,9A)') sym, fun, 'sk-parallel part took ', skp_time, ' sec' ! now we can compute Amdahl fraction
    if(o>0) write(o,'(3A,I0,9(A,F0.1))') sym, fun, 'iteration #',iscf, ' took ', time/60, ' min = ', time, ' sec'
! #endif
  enddo ! iscf

#ifdef INDEPENDENT_TASKS
  if(o>0) write(o,'(9A)') sym, fun, 'synchronize INDEPENDENT_TASKS'
  call MPIbarrier( MPI_Comm_WORLD )
#endif
  run = scf_run_control( name, comm=global%cart_calc_comm, delete=.true. ) ! delete the control file

  if( residual > scf%thres .and. o > 0 ) &
    write(o,'(9A)') sym, fun, WARNING(0), 'density is NOT self-consistent!'

  ! show all state energies in the output if they are not more than 999 (by default)
  n0 = nint( eval( '$display_states', def=1E3 ) )
#ifdef INDEPENDENT_TASKS
  if( MPImaster( indep_comm ) ) then
#else
  if( .true. ) then
#endif

#ifdef CONSTR
    if( constrain ) then ! update psi(ibsk)%occ = sum( psi(ibsk)%cocc(1:nc) )/max(1E-9,sum( psi(ibsk)%w(1:nc) )) for display
      do i = 1, global%nbnd * global%nspn * global%nkpt
        psi_state(i)%occ = sum( psi_state(i)%cocc(1:nc) )/max(1E-9,sum( psi_state(i)%cwgt(1:nc) ))
      enddo ! ibsk
    endif ! constrain
#endif
    i = display_states( '', o, global%nbands, global%nspins, global%nkpoints, kpt, psi_state, &
          iteration=iscf, dp=residual, comm=gc%equi_comm, write_eig_file=(n0<1), basename=name, max_states=abs(n0), FermiLevel=FermiLevel )
  endif ! MPImaster of indep_comm

#if R1_C2 == 1
#ifdef FULL_DEBUG
  ! project KS states onto bessel functions in the region around (0,0,0)
  do ibnd = 1, global%nbnd
    ist = radial_hist( file='dmp/wf_r.'+ibnd, data=reshape(spsi(:,ibnd),gc%ng(1:3)), &
                      hg=gc%h, origin=-gc%off, ellmax=1, comm=gc%comm )
  enddo ! ibnd
#endif
#endif


  ! find the minimum values of the total effective potential
  if(o>0) write(o,'(3A,F0.6,9A)') sym, fun, 'Fermi level  ', FermiLevel*eV, eV_
  do js = 1, size(vTOT_c,4)
    mam_vloc(-1) = -( -minval(vTOT_c(:,:,:,js)) .MPImax. gc%comm )
    mam_vloc( 0) =  (  sum(vTOT_c(:,:,:,js)) .MPIsum. gc%comm ) / real(product(gc%ng_all(1:3)))
    mam_vloc(+1) =  (  maxval(vTOT_c(:,:,:,js)) .MPImax. gc%comm )
    if(o>0) write(o,'(3A,3(" ",F0.6),9A)') sym, fun, 'Vloc min. avg. max.', mam_vloc*eV, eV_
  enddo ! js
  if(o>0) write(o,'(3A,F0.6,9A)') sym, fun, 'total energy ', etotal*eV, eV_


  if( eval( '$compressed_row_storage', def=0. ) > .1 .or. &
      eval( '$Hamiltonian2ASCII', def=0. ) > .1 ) then ! export the Hamiltonian
    ! write the Hamiltonian in CompressedRowStorage
#ifdef EXTENDED
!     ist = Hamiltonian2CRS( gc, vloc, 1, atm, size(cpsi,1), kp=kpt(1), filename='new', rc=spsi(1,1), assume_filling=0.2 )
!     if(o>0) write(o,'(3A,I0)') sym, fun, 'write the Hamiltonian in CompressedRowStorage, status = ', ist
!     ist = Hamiltonian2CRS( gc, vloc=vloc, jspin=1, atm=atm, naprj=size(cpsi,1), kp=kpt(1), filename=name, rc=spsi(1,1) )
!     if(o>0) write(o,'(3A,I0)') sym, fun, 'CompressedRowStorage, status = ', ist
    if(o>0) write(o,'(9A)') sym, fun, 'write the Hamiltonian of spin#1, kpt#1 to a sparse ASCII file.'
    ist = Hamiltonian2ASCII( gc, vloc=vTOT_c, jspin=1, atm=atm, kp=kpt(1), basename=name, rc=spsi(1,1) )
    if(o>0) write(o,'(3A,I0)') sym, fun, 'Hamiltonian2ASCII, status = ', ist
!     if(o>0) write(o,'(9A)') sym, fun, 'write the Hamiltonian of spin#1, kpt#1 in FullStorage'
!     ist = Hamiltonian2file( gc, vloc=vTOT_c, jspin=1, atm=atm, kp=kpt(1), filename=name, rc=spsi(1,1) )
!     if(o>0) write(o,'(3A,I0)') sym, fun, 'FullStorage, status = ', ist
#else
    if(o>0) write(o,'(9A)') sym, fun, 'Hamiltonian to FullStorage not installed, activate extended features!'
#endif
  endif ! crs


cDBG  if(o>0) write(o,'(/,9A)') sym, fun, 'atomic populations in iln-representation:'
cDBG  do i = 1, size(atm)
cDBG    if( atm(i)%owner /= gc%rank ) cycle
cDBG    if(o>0) write(o,'(3A,I0,2A,99F10.6)') sym, fun, 'atom #',atm(i)%ja,' ', atm(i)%s%sym, atm(i)%atomic_population(:,0)
cDBG  enddo ! ia

    deallocate( vXC, rhot, rho_aug, Sspsi, stat=ist ) ! free the memory
    residual = mix( gd, atm, rhov, global%kmixing, mixratio, nhistory=-1 ) ! deallocation of the mixing history

  endfunction ! relax_density




#if R1_C2 == 1
  integer function projected_density_of_states_r( &
#else
  integer function projected_density_of_states_c( &
#endif
       name, pdos, gc, speci, atoms, atm, global, &
       spsi, cpsi, psi_state, temperature, FermiLevel ) &
  result( ist )
  use configuration, only: o, WARNING
  use type_grid, only: grid
  use type_atom, only: atom
  use type_species, only: species
  use type_state, only: state
  use type_info, only: info
  use MPItools, only: MPIallsum, operator(.MPImax.), MPIbarrier
  use analysis, only: PDoS_weights ! weights for the projected density of states
  use FermiDirac, only: density_of_states
  use unitsystem, only: Ang, Ang_
  use toolbox, only: operator(+)
    character(len=*), intent(in)          :: name
    character(len=3), intent(in)          :: pdos
    type(grid), intent(in)                :: gc !! coarse grid
    type(species), target, intent(in)     :: speci(:)
    type(atom), intent(in)                :: atoms(1:) !! list of all atoms
    type(atom), intent(in)                :: atm(1:) !! parallelized list of atoms
    type(info), intent(in)                :: global
    REAPLEX, intent(in)                   :: spsi(1:,1:) ! (nxyzs,nbsk) wave functions
    REAPLEX, intent(in)                   :: cpsi(1:,1:) ! (naprj,nbsk) projection coefficients
    type(state), intent(in)               :: psi_state(1:) !! parallelized list of state descriptors
    real, intent(in)                      :: temperature, FermiLevel

    character(len=*), parameter     :: fun = ' pdos: '
    integer                         :: jatm, ispc, nlm, nbsk
    real, allocatable               :: w8(:,:) ! atom    weights(nlm,nbsk)
    real, allocatable               :: ws(:,:) ! species weights(nlm,nbsk)
    character(len=8), allocatable   :: lab(:)  ! labels (nlm)
    integer                         :: iz, iza, ibsk, izs, ize

    ist = 0 ! init result
    nbsk = size(spsi,2)
cDBG  if( size(cpsi,2) /= nbsk ) stop 'pdos: dim#2 of CPSI does not match NBSK!'
cDBG  if( size(cpsi,1) /= gc%naprj ) stop 'pdos: dim#1 of CPSI does not match gc%NAPRJ!'
cDBG  if( size(psi_state,1) /= nbsk ) stop 'pdos: dim of PSI_STATE does not match NBSK!'

    selectcase( pdos(1:3) )
    case( 'z' )

      if(o>0) write(o,'(9A)') sym, fun, 'create z-projected density of states.'
      allocate( w8(1,nbsk), stat=ist )
      do iza = 1, gc%ng_all(3)
        w8 = 0. ! init
        iz = iza - gc%ioff(3)
        if(o>0) write(o,'(3A,I4,A,F12.6,9A)') sym, fun, 'z-plane #', iza, ' at z =', ( gc%off(3) + iz*gc%h(3) )*Ang, Ang_
        if( iz > 0 .and. iz <= gc%ng(3) ) then
          izs = 1+(iz-1)*gc%ng(1)*gc%ng(2) ! start index
          ize = iz*gc%ng(1)*gc%ng(2) ! end index
          ! find weights for each KS state
          do ibsk = 1, nbsk
            w8(1,ibsk) = dot_product( spsi(izs:ize,ibsk), spsi(izs:ize,ibsk) )
          enddo ! ibsk
        endif ! iz in my range
        call MPIallsum( w8, gc%comm ) ! allreduce over all grid ranks
        w8 = w8 * gc%hvol ! multiply the volume element
        if( gc%rank == 0 ) & ! grid master creates a .dos-file for each z-plane
        ist = density_of_states( name=name+'.z'+iza, global=global, psi=psi_state, &
                FermiLevel=FermiLevel, temperature=temperature, comm=gc%equi_comm, weights=w8, &
                comment='projected onto z-plane # '+iza )
      enddo ! iza

    case( 'alm', 'al', 'a' )

      if(o>0) write(o,'(9A)') sym, fun, 'create atom-projected density of states.' ! for each atom
      do jatm = 1, size(atoms)
        ! find weights for each KS state
        call PDoS_weights( atm, jatm, cpsi, psi_state, me=gc%rank, mode=pdos(2:3), weights=w8, labels=lab )
        ! Projected Density of States
        if( gc%rank == atoms(jatm)%owner ) & ! creates a .dos-file for each atom
        ist = density_of_states( name=name+'.a'+jatm, global=global, psi=psi_state, &
                FermiLevel=FermiLevel, temperature=temperature, comm=gc%equi_comm, weights=w8, labels=lab, &
                comment='projected onto atom#'+jatm+'_'+atoms(jatm)%s%sym )
      enddo ! jatm

    case( 'slm', 'sl', 's' )

      do ispc = 1, size(speci)
        if(o>0) write(o,'(9A)') sym, fun, 'create species-projected density of states for ',trim(speci(ispc)%name)
        nlm = 0 ! init
        do jatm = 1, size(atoms)
          if( associated( atoms(jatm)%s, speci(ispc) ) ) then
cDBG        if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'atom(', atoms(jatm)%ja, ')%s => ',trim(speci(ispc)%name)
            call PDoS_weights( atm, jatm, cpsi, psi_state, me=gc%rank, mode=pdos(2:3), weights=w8, labels=lab )

            if( allocated(w8) ) then ! only atom owner will allocate array wa
              nlm = size(w8,1)
              if( .not. allocated(ws) ) then
                allocate( ws(nlm,nbsk), stat=ist )
                ws = 0. ! init
              endif ! not allocated ws
cDBG          if( any( shape(w8) /= shape(ws) ) ) stop 'pdos: ERROR: shapes of W8 and WS differ!'
              ws = ws + w8 ! add
            endif ! allocated w8

          endif ! associated
        enddo ! jatm

        nlm = nlm .MPImax. gc%comm
        if( .not. allocated(ws) ) then
cDBG      if(o>0) write(o,'(5A,I0)') sym, fun, 'post-allocate(ws) for ',trim(speci(ispc)%name),' nlm = ',nlm
          allocate( ws(nlm,nbsk), stat=ist ) ; ws = 0. ! init
        endif
        if( .not. allocated(lab) ) allocate( lab(nlm), stat=ist ) ; lab = '' ! init

        ! collect from all atoms of this species in the entire cell
        call MPIallsum( ws, gc%comm ) ! ; if(o>0) write(o,'(3A,Z8.8)') sym, fun, 'MPIallsum over 0x',gc%comm
        ! labels are neglected, labels may not be allocated if computed with domain decomposition

        ! Projected Density of States
        if( gc%rank == 0 ) & ! speci(ispc)%owner ! creates a dosfile for each species
        ist = density_of_states( name=name+'.'+speci(ispc)%sym, global=global, psi=psi_state, &
                FermiLevel=FermiLevel, temperature=temperature, comm=gc%equi_comm, weights=ws, labels=lab, &
                comment='projected onto_'+speci(ispc)%name+'_'+pdos(2:3) )
                ! _pdos(2:3) indicates the mode, even if labels are missing
        deallocate( ws, w8, stat=ist )
        call MPIbarrier( gc%comm )
cDBG    if(o>0) write(o,'(9A)') sym, fun, 'species-projected density of states for ',trim(speci(ispc)%name),' created!'
      enddo ! ispc

    case( '', '0', 'no', 'non', 'off' ) ! Nothing to be done, no warning
    case default
      if(o>0) write(o,'(9A)') sym, fun, WARNING(0), '"',trim(pdos),'" is not a valid control sequence for pdos!'
    endselect ! pdos

  endfunction ! projected_density_of_states

#ifdef EXTENDED
!+ extended

#if R1_C2 == 1
  integer function visualize_all_wfs_r( &
#else
  integer function visualize_all_wfs_c( &
#endif
                                        name, global, g, spsi, direction ) result( ist )
  use configuration, only: o, WARNING
  use type_grid, only: grid
  use type_info, only: info
!   use toolbox, only: cat
  use toolbox, only: write_bmp_file
  use MPItools, only: MPIallsum, MPIparallel, MPImyrank, MPInprocs, MPIbarrier
  implicit none
    ! parameters
    character(len=*), parameter         :: fun = ' visualize_all_wfs: '
    character(len=*), parameter         :: SPIN_CHAR(1:2) = (/'.dn','.up'/)
    character(len=*), parameter         :: DIR = 'bmp/'
    ! arguments
    character(len=*), intent(in)        :: name ! project name
    type(info), intent(in)              :: global
    type(grid), intent(in)              :: g ! wave function grid gc
    REAPLEX, intent(in)                 :: spsi(:,1:)
    character, intent(in), optional     :: direction ! default is 'z', possible also 'x' or 'y'
    ! local vars
    complex, allocatable                :: w(:,:,:) ! always of complex type!
    integer                             :: ik, jk, is, js, ib, jb, ibsk, ndk, ndb, ndz, istart(3), iend(3), jg, idir
    character(len=64)                   :: filename, fnformat, kpformat
    character(len=8)                    :: KpointChar, SpinChar
    integer                             :: myrank, nprocs
    logical                             :: parallel
    character                           :: dc ! DirectionChar

    idir = 3 ! default direction is 'z'
    if( present( direction ) ) then
      selectcase( direction )
      case( 'x', 'X', '1' ) ; idir = 1
      case( 'y', 'Y', '2' ) ; idir = 2
      case( 'z', 'Z', '3' )
      case default ; if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'unable to resolve direction "',direction,'", use "z" by default!'
      endselect ! direction
    endif ! present direction
    dc = achar( 119+idir )

    ndz = min(max( 1, 1+int( log10( g%ng_all(idir)+.1 ) ) ), 6 ) ! number of grid points in the selected direction
    ndb = min(max( 1, 1+int( log10( global%nbands+.1 ) ) ), 6 ) ! bands
    ndk = min(max( 1, 1+int( log10( global%nkpoints+.1 ) ) ), 6 ) ! kpoints

    write( unit=kpformat, fmt='(9(A,I0))', iostat=ist ) '(A,I',ndk,'.',ndk,')' ! e.g. '(A,I6.6)'
    if(o>0) write(o,'(9A)') sym, fun, 'create k-point tags with format "',trim(kpformat),'".'

    write( unit=fnformat, fmt='(9(A,I0))', iostat=ist ) '(3A,I',ndb,'.',ndb,',4A,I',ndz,'.',ndz,',9A)'
    ! create filename like this: trim(dir),trim(name),'_',jb,trim(SpinChar),trim(KpointChar),'.',achar(119+idir),jg

!     write( unit=fnformat, fmt='(A,9(2A,2(A,I0)))', iostat=ist ) '("',trim(dir),trim(name),'_",I',ndb,'.',ndb,',2A,".',dc,'",I',ndz,'.',ndz,',9A)'
!     ! --> create filename like this: write( unit=filename, fmt=fnformat, iostat=ist ) jb,trim(SpinChar),trim(KpointChar),jg

    if(o>0) write(o,'(9A)') sym, fun, 'create filenames with format "',trim(fnformat),'".'

    if(o>0) write(o,'(3A,F0.3,9A)') sym, fun, 'try to allocate ',product(g%ng_all(1:3))*.5**16,' MByte.'
    allocate( w(g%ng_all(1),g%ng_all(2),g%ng_all(3)), stat=ist )
    if( ist /= 0 .and. o>0) write(o,'(3A,F0.3,9A)') sym, fun, 'failed to allocate ',product(g%ng_all(1:3))*.5**16,' MByte!'
    if( ist /= 0 ) return

    istart =       1 + g%ioff ! start indices
    iend = g%ng(1:3) + g%ioff ! end indices

    parallel = MPIparallel( g%comm )
    myrank = MPImyrank( g%comm )
    nprocs = MPInprocs( g%comm )

    ibsk = 0 ! init collective index
    do ik = 1, global%nkpt ; jk = ik + global%iokpt
      KpointChar = '' ; if( global%nkpoints > 1 ) write( unit=KpointChar, fmt=kpformat, iostat=ist ) '.k',jk
      do is = 1, global%nspn ; js = is + global%iospn
        SpinChar = '' ; if( global%nspins == 2 ) SpinChar = SPIN_CHAR(js)
        do ib = 1, global%nbnd ; jb = ib + global%iobnd
          ibsk = ibsk+1
          w = 0. ! init
          w(istart(1):iend(1),istart(2):iend(2),istart(3):iend(3)) = reshape( spsi(:,ibsk), g%ng(1:3) ) ! assign data

          if( parallel ) call MPIallsum( w, g%comm ) ! reduce over the grid communicator to collect wave function data

          do jg = 1, g%ng_all(idir)
            if( modulo( jg-1, nprocs ) /= myrank ) cycle ! cyclic distribution
            ! create filename from global indices
            write( unit=filename, fmt=fnformat, iostat=ist ) trim(dir),trim(name),'_',jb,trim(SpinChar),trim(KpointChar),'.',dc,jg
            ! write( unit=filename, fmt=fnformat, iostat=ist ) jb,trim(SpinChar),trim(KpointChar),jg
            if(o>0) write(o,'(9A)') sym, fun, 'try to create file "',trim(filename),'.bmp".'

            selectcase( idir )
            case( 1 ) ; ist = write_bmp_file( filename, w(jg,:,:) ) ! show y-z-planes ! Warning ! needs temp. memory
            case( 2 ) ; ist = write_bmp_file( filename, w(:,jg,:) ) ! show x-z-planes ! Warning ! needs temp. memory
            case( 3 ) ; ist = write_bmp_file( filename, w(:,:,jg) ) ! show x-y-planes
            case default ; stop 'visualize_all_wfs: fatal, IDIR should be in {1,2,3}!'
            endselect ! idir

          enddo ! jz

          if( parallel ) call MPIbarrier( g%comm ) ! sync

        enddo ! ib
      enddo ! is
    enddo ! ik

  endfunction ! visualize_all_wfs
!- extended
#endif





#if R1_C2 == 2
! begin of tail part of double preprocessing
#undef REAPLEX


  !! setup of the local correction potential vBAR
  !! and the smooth core correction rhoc using bfuns
  status_t function smooth_correction( a, g, rhoc, vBAR ) result( ist )
  !=============================================================================
  !== Preparation of                                                      ======
  !==      -- the smooth core charge density on the dense grid            ======
  !==      -- the correction potential on the dense grid                  ======
  !=============================================================================
  use configuration, only: WARNING, o ! output unit
  use type_grid, only: grid, periodic_positions
  use type_atom, only: atom
  use type_bfun, only: add_s_projector cDBG ,bfun_norm
  use unitsystem, only: Ang, Ang_
  use constants, only: sqrt4pi
  use selfcon, only: integral
    type(atom), intent(in)                :: a(:) !! list of *all* atoms <br> (all, because the rhoc is not strictly localized within the spheres)
    type(grid), intent(in)                :: g !! grid descriptor, usually the <a href="#gd">dense grid</a>
    real, intent(out)                     :: rhoc(:,:,:,1:) !! smooth core density, not strictly localized within the atomic spheres
    real, intent(out)                     :: vBAR(:,:,:,1:) !! local correction potential, localized within the spheres

    character(len=*), parameter           :: fun = ' smooth correction: '
    integer                               :: ia, is, ns, ni, nn, nxi(9)=0
    real                                  :: spinfactor
    real                                  :: rmx
    real, allocatable                     :: imagepos(:,:)
    real                                  :: sint, smq00


    ist = 0
    ns = size( rhoc, 4 )

    spinfactor = 1.0 ; if( ns > 1 ) spinfactor = 0.5 ! each spin channel gets half

    rmx = minval(g%s) ! the smallest extend of the supercell
    nxi = 0

    rhoc = 0. ! init

    do ia = 1, size(a)
      if( a(ia)%s%rhoc%rcut > rmx ) then
        ! how many periodic image positions are required?
        nn = ceiling( a(ia)%s%rhoc%rcut/rmx )
        nxi(nn) = nxi(nn)+1 ! count up

        allocate( imagepos(3,(nn+1+nn)**3), stat=ist )


        ! if the core density is extended further than rmx,
        ! generate up to 125 image positions for 3 periodic boundary conditions
        ni = periodic_positions( g, a(ia)%relpos, imagepos, number_of_images=nn )

        do is = 1, ns
          call add_s_projector( a(ia)%s%rhoc, & ! bfun
                                qnt=rhoc(:,:,:,is), &
                                hg=g%h, &
                                origins=imagepos(:,1:ni), &
                                offset=g%off, &
                                factor=spinfactor )
        enddo ! is
        deallocate( imagepos, stat=ist )
      else  ! rcut > rmx
        ! the usual imagepositions (<= 27) are sufficient
        ni = a(ia)%nimages

        do is = 1, ns
          call add_s_projector( a(ia)%s%rhoc, & ! bfun
                                qnt=rhoc(:,:,:,is), &
                                hg=g%h, &
                                origins=a(ia)%imagepos(1:3,1:ni), &
                                offset=g%off, &
                                factor=spinfactor )
        enddo ! is
      endif ! rcut > rmx

    enddo ! ia


    if( any( nxi > 0 ) ) then
!      if(o>0) write(o,'(3A,9I3,9A)') sym, fun, 'in [', nxi, ' ] situations of the core density'
!      if(o>0) write(o,'(3A,9I3,9A)') sym, fun, '... ', (nn, nn=1,9), '   periodic images were required.'
      if(o>0) write(o,'(3A,9I3,9A)') sym, fun, 'in [',  nxi,         ' ] situations'
      if(o>0) write(o,'(3A,9I3,9A)') sym, fun, '... ', (nn, nn=1,9), '   periodic images'
      if(o>0) write(o,'(3A,9I3,9A)') sym, fun, '...   of the smooth core density were required.'
    endif ! #of situations where more than 1 image is required


 
    where( rhoc < 0. ) ; rhoc = 0. ; endwhere


    sint = integral( rhoc, g ) !! all core charge densities

    smq00 = 0.
    do ia = 1, size(a) ! sum up all static compensation monopoles
      smq00 = smq00 - ( a(ia)%s%q00 * sqrt4pi + a(ia)%s%Nve )
    enddo ! ia

    if( abs(sint-smq00) > .4E-12 ) then
      if(o>0) write(o,'(4A,ES12.3,9A)') sym, fun, WARNING(0), 'charge error of core densities', sint-smq00, ' e'
    endif ! |error| > threshold


    ! the radius of the correction potential vBAR is usually the smallest
    ! projector radius, so a warning will be lanched at projector setup,
    ! or during geometry checks in CheckMode in case that radius is too large.
    vBAR = 0. ! init
    do ia = 1, size(a)
      call add_s_projector( a(ia)%s%vBAR, &
                            qnt=vBAR(:,:,:,1), &
                            hg=g%h, &
                            origins=a(ia)%imagepos(:,1:a(ia)%nimages), &
                            offset=g%off )
    enddo ! ia
    ! second spin channel --- same correction potential
    if( size(vBAR,4) > 1 ) vBAR(:,:,:,2) = vBAR(:,:,:,1) ! copy

  endfunction ! smooth_correction



#ifdef EXTENDED
!+ extended

  ! =====================================================================
  ! === Test function for various quantities                  ===========
  ! =====================================================================

#ifdef  OUTPUT_CENTRAL_POTENTIAL
!+ output_central_potential

  subroutine output_potential( g, v, unt )
  use type_grid, only: grid
    type(grid), intent(in) :: g
    real, intent(in) :: V(:,:,:)
    integer, intent(in) :: unt

    real :: rv(3), r
    integer :: i1, i2, i3

    do i3 = 1, g%ng(3) ; do i2 = 1, g%ng(2) ; do i1 = 1, g%ng(1)
      rv = g%h*(/i1,i2,i3/)+g%off
      r = sqrt( sum( rv**2 ) )
      write(unt,'(9F16.6)') r, V(i1,i2,i3)
    enddo ; enddo ; enddo
    write(*,'(/,A,I0,/)') '  OUTPUT_CENTRAL_POTENTIAL to fort.',unt
  endsubroutine ! output_potential

!- output_central_potential
#endif


#ifdef DEBUG
!+ debug

  status_t function test_smooth_correction( a, g, corr, name ) result( ios )
  use type_grid, only: grid
  use type_atom, only: atom
  use constants, only: Y00 => ONESQRTFOURPI
  use type_species, only: I_RHOC, I_TRU, I_SMT, I_VBAR
  use toolbox, only: operator(+)
  use toolbox, only: radial_hist
  use toolbox, only: write_vtk_file

    type(atom), intent(in) :: a
    type(grid), intent(in) :: g
    real, intent(in)       :: corr(:,:,:)
    character(len=*), intent(in), optional :: name

    character(len=*), parameter :: fun = ' test_smooth_correction: '
    character(len=32)           :: nm, fl
    integer                     :: ir
    real                        :: mxrhoc

    nm = 'corr' ; if( present( name ) ) nm = name
    fl = 'dmp/'+nm+'_dense_grid'

    ! decompose the function on the 3dim grid into 1dim radial functions
    ! up to ellmax(give the spd... character) or for a single Ylm(ilm=...)
    ios = radial_hist( data=corr, hg=g%h, origin=a%pos-g%off, ellmax=0, file=fl, comm=g%comm )

    mxrhoc = maxval( a%s%rf(:,I_RHOC+I_SMT)*Y00 ) ! largest value of the smooth contribution
    open(12,file='dmp/rhoc_radial_grid',iostat=ios)
    if( ios == 0 ) then
      do ir = 0, a%s%ircccut
        write(12,'(9ES16.6)') a%s%g%r(ir), a%s%rf(ir,I_RHOC+I_SMT)*Y00, min( a%s%rf(ir,I_RHOC+I_TRU)*Y00, mxrhoc ) ! the tru is cut off at the max.val
      enddo ! ir
      close(12)
    endif ! ios == 0

    open(12,file='dmp/vbar_radial_grid',iostat=ios)
    if( ios == 0 ) then
      do ir = 0, a%s%irvccut
        write(12,'(9ES16.6)') a%s%g%r(ir), a%s%rf(ir,I_VBAR)*Y00
      enddo ! ir
      close(12)
    endif ! ios == 0
#ifdef EXTENDED
    ios = write_vtk_file( 'dmp/'+nm, corr )
#endif
  endfunction ! test_smooth_correction


  status_t function test_compensation( a, g ) result( ist )
  ! test compensation charges for the correct multipole moments
  use constants, only: sqrt4pi
  use configuration, only: o
  use MPIconst, only: PREC
  use type_grid, only: grid
  use type_atom, only: atom
  use type_comp, only: project, add
  use selfcon, only: integral
  implicit none
    ! parameters
    character(len=*), parameter     :: fun = ' test_compensation: '
    ! arguments
    type(atom), intent(in)          :: a
    type(grid), intent(in)          :: g
    ! local vars
    integer                         :: mlm, ilm
    real, allocatable               :: f(:,:,:), mm(:,:), c(:), q(:)
    character(len=16)               :: filename

    if( a%owner /= 0 ) return

    mlm = (a%cmp%ellmax+1)**2

    allocate( f(g%ng(1),g%ng(2),g%ng(3)), c(mlm), mm(mlm,mlm), q(mlm), stat=ist )
    mm = 0.

    ! compensation charges
    do ilm = 1, mlm
      f = 0.
      c = 0.
      c(ilm) = 1.
      call add( cmp=a%cmp, rho=f, qlm=c )
      q(ilm) = integral( f, g )/sqrt4pi
      call project( cmp=a%cmp, Ves=f, c=mm(:,ilm), dV=g%hvol  )
      if(o>0) write(8,'(I3,A,F20.17,A,49ES10.2E2)') ilm, ' = ilm, q =', q(ilm), ' <ilm,ilm''> =', mm(:,ilm)
    enddo ! i1
    if(o>0) write(o,'(/3A,I3,9A)') sym, fun, 'overlap of a%cmp''s for atom#', a%ja, ' written to fort.8'

  endfunction ! test_compensation


  status_t function test_projectors( a, g, ellmax ) result( ist )
  ! test for orthongonality
  use configuration, only: o
  use type_proj, only: project, add
  use type_proj, only: proj_rescale
  use type_grid, only: grid
  use type_atom, only: atom
  use type_species, only: species, I_PRJ, I_SMT, I_RPRJ
  use type_species, only: I_FPRJ
  use type_bfun, only: operator(.at.) ! operator(.filteredat.)
  use MPItools, only: MPIallsum
    type(atom), intent(inout)       :: a
    type(grid), intent(in)          :: g
    integer, intent(in)             :: ellmax

    character(len=*), parameter     :: fun = ' test_projectors: '
    complex, parameter              :: unity(1:3) = 1.
    real, allocatable               :: rwf(:,:), f(:), c(:)
    real, allocatable               :: pp(:,:), f3(:,:,:), cilm(:), ppr(:,:,:)
    integer                         :: ir, ell, enn, iln, i1, i2, ilm, mlm, iln1, iln2, enn2
    real                            :: r, dr, sm, sm2, s(9)
    character(len=16)               :: filename

    if( a%owner /= g%rank ) return

    allocate( f(product(g%ng(1:3)) ), c(a%s%mlnm), pp(a%s%mlnm,a%s%mlnm), ppr(a%s%mln,a%s%mln,3), stat=ist ) ; pp = 0. ; ppr = 0.

    do i1 = 1, a%s%mlnm
      f = 0. ; c = 0. ; c(i1) = 1.
      call add( p=a%prj, f=f, c=c, eikL=unity )
      call project( p=a%prj, f=f, c=pp(:,i1), dV=g%hvol, eikL=unity  )
    enddo ! i1
    if(o>0) write(o,*) 'a%comm removed' ! call MPIallsum( pp, a%comm )

    if(o>0) then
      write(o,'(/A,I3)') 'test <p|p>, see fort.8 for more digits, atom#', a%ja
      write(8,'(/A,I3,9A)') 'test <p|p> for atom#', a%ja, ' on the ', trim(g%name)
      do i1 = 1, a%s%mlnm
        write(o,'(36F7.2)') pp(:,i1)
        write(8,'(36F24.16)') pp(:,i1)
      enddo ! i1

      ! plot the expected value of <p|p> (diagonals only)
      write(8,'(9A)') 'unfiltered <p|p> for ', a%s%sym, ' on the radial grid'
      iln = 0
      do ell = 0, a%s%ellmax
        do enn = 1, a%s%nn(ell)
          iln1 = iln+enn
          s = 0.
          do enn2 = 1, a%s%nn(ell)
            iln2 = iln+enn2
            sm = 0.
            do ir = 1, a%s%g%imx
              sm = sm + ( a%s%g%r(ir)**ell * a%s%rwf(ir,iln1,1,I_PRJ) )* &
                        ( a%s%g%r(ir)**ell * a%s%rwf(ir,iln2,1,I_PRJ) ) * a%s%g%r2dr(ir)
            enddo ! ir
            s(enn2) = sm
            ppr(iln2,iln1,1) = sm
          enddo ! enn2
          write(8,'(2(A,I3),9F24.16)') 'ell=', ell, ' enn=', enn, s(1:a%s%nn(ell))
        enddo ! enn
        iln = iln+a%s%nn(ell) ! forward
      enddo ! ell

      ! plot the expected value of <p|p> (diagonals only)
      write(8,'(9A)') 'filtered <p|p> for ', a%s%sym, ' on the radial grid (given as BFUN)'
      iln = 0
      do ell = 0, a%s%ellmax
        do enn = 1, a%s%nn(ell)
          iln1 = iln+enn
          s = 0.
          do enn2 = 1, a%s%nn(ell)
            iln2 = iln+enn2
            sm = 0.
            do ir = 1, a%s%g%imx
              r = a%s%g%r(ir)
              sm = sm + ( r**ell * (a%s%fprj(iln1) .at. r) )* &
                        ( r**ell * (a%s%fprj(iln2) .at. r) ) * a%s%g%r2dr(ir)
            enddo ! ir
            s(enn2) = sm
            ppr(iln2,iln1,2) = sm
          enddo ! enn2
          write(8,'(2(A,I3),9F24.16)') 'ell=', ell, ' enn=', enn, s(1:a%s%nn(ell))
        enddo ! enn
        iln = iln+a%s%nn(ell) ! forward
      enddo ! ell


    endif ! o>0


    if(o>0) then
      ! plot diagonal elements
      write(9,'(9A)') '# unfiltered, filtered, +dg-filtered, on the grid (and emm-resolved)'
      write(9,'(99F18.12)') ( ( ppr(iln1,iln1,ir), iln1=1,a%s%mln ),  ir=1,3 ),   ( pp(i1,i1), i1=1,a%s%mlnm )
    endif ! o>0

!     stop 'main line 3556 --> see fort.9'

    return
    ! renormalize projectors on the grid
    do i1 = 1, a%s%mlnm
      iln1  = a%s%ind_iln(i1)
      c(i1) = sqrt( ppr(iln1,iln1,2)/pp(i1,i1) )
    enddo ! i1
#ifdef RHO_ON_GD
#ifdef DEBUG
     if( g%h(1) == 1./2. ) c = 1. / (/ 0.8490467089628156, 0.6454926836650585, 0.6454926836650585, 0.6454926836650585 /)
     if( g%h(1) == 1./3. ) c = 1. / (/ 0.9306649325233712, 0.8151885069908865, 0.8151885069908865, 0.8151885069908865 /)
#endif
#else
#ifdef DEBUG
     if( g%h(1) == 1./2. ) c = 1. / (/ 0.9089692207863025, 0.8127519113603675, 0.8127519113603675, 0.8127519113603675 /)
     if( g%h(1) == 1./3. ) c = 1. / (/ 0.9614049931455843, 0.9167549622220552, 0.9167549622220552, 0.9167549622220552 /)
#endif
#endif
    if(o>0) write(o,'(3A,99F10.6)') sym, fun, 'renormalize projector with', c
    call proj_rescale( a%prj, c )
    if(o>0) write(o,'(9A)') sym, fun, 'projector has been renormalized!'


    return

    ! check again
    pp = 0.
    do i1 = 1, a%s%mlnm
      f = 0. ; c = 0. ; c(i1) = 1.
      call add( p=a%prj, f=f, c=c, eikL=unity )
      call project( p=a%prj, f=f, c=pp(:,i1), dV=g%hvol, eikL=unity  )
    enddo ! i1
    stop 'a%comm removed' ! call MPIallsum( pp, a%comm )

    if(o>0) then
      write(o,'(/A,I3,9A)') 'test <p|p>, atom#', a%ja, ' on the ', trim(g%name)
      do i1 = 1, a%s%mlnm
        write(o,'(36F24.16)') pp(:,i1)
      enddo ! i1
    endif ! o>0

  endfunction ! test_projectors

!- debug
#endif

#ifdef PLOT_BASIS
!+ plot_basis

  subroutine plot_PAW_basis_function( a, g, mesh )
  use type_grid, only: grid
  use type_atom, only: atom
  use type_species, only: I_R, I_RHOC, I_TRU, I_SMT
  use toolbox, only: cat
  use toolbox, only: write_bmp_file
  use projection, only: project
  use harmonics, only: Ylmax_rl
    type(atom), intent(in)              :: a
    type(grid), intent(in)              :: g
    integer, intent(in)                 :: mesh !! mesh refinement factor

    character(len=*), parameter         :: fun = ' plot_PAW_basis_function: '
    character(len=9)                    :: gridpoint
    real                                :: c(a%s%mlnm), Ylm(a%s%mlm)
    real                                :: coa(g%ng(1),g%ng(2),g%ng(3))
    real                                :: psi(product(g%ng(1:3)))
    real, allocatable                   :: bas(:,:,:)
    integer                             :: i1, i2, i3, ii, ng(1:3), ist
    integer                             :: ix, iy, iz, ir, nr, ilnm, is
    real                                :: off(1:3), h(1:3), rv(1:3), r2v(1:3), rr, rs, rc2, am1, bm1, t, tmax, h3

    ng = g%ng(1:3)*mesh
    if(o>0) write(o,'(3A,3I6,9A)') sym, fun, 'ng =', ng, ' grid points'
    if(o>0) write(o,'(3A,F12.3,9A)') sym, fun, 'try to allocate', product(ng)*8./2.**20, ' MiByte'
    allocate( bas(ng(1),ng(2),0:0), stat=ist )
    if( ist /= 0 ) stop 'plot_PAW_basis_function: allocation failed.'

    h  = g%h/real(mesh)
    h3 = product(h)
    off = a%pos - ( g%off + 0.5 * g%h - 0.5 * h )
    if(o>0) write(o,'(3A,9(3F10.3,A))') sym, fun, 'off =', off, ' aB, off/h =', off/h
    nr = a%s%nr
    rc2 = a%s%rf(nr,I_R)**2 ! cutoff radius squared
    if(o>0) write(o,'(3A,F10.3,9A)') sym, fun, 'rcut =', sqrt(rc2), ' aB'
    am1 = 1.0/a%s%rmax_a_b(2)
    bm1 = 1.0/a%s%rmax_a_b(3)
    if(o>0) write(o,'(3A,9(I3,A))') sym, fun, 'mlnm =', a%s%mlnm, ' mlm =', a%s%mlm, ' ml =', a%s%ellmax
    tmax = 0.

    ii = 0
    do i3 = 1, g%ng(3)
      do i2 = 1, g%ng(2)
        do i1 = 1, g%ng(1)
          ii = ii+1

          if( i3 == g%ng(3)/2 ) then ! middle
          if( i2 == g%ng(2)/2 ) then ! middle
            write(unit=gridpoint,fmt='(3I3.3)') i1, i2, i3

            psi = 0. ; psi(ii) = 1.
            call project( a%prj(1), psi, c, dV=1.0 ) !g%hvol )
            if(o>0) write(o,'(3A,3I4,A,99ES10.2E2)') sym, fun, 'i=[',i1,i2,i3, ' ], c=', c
            bas(:,:,:) = 0.
            ! now add the grid point to bas
!             bas( 1+(i1-1)*mesh:i1*mesh , 1+(i2-1)*mesh:i2*mesh, 1+(i3-1)*mesh:i3*mesh ) = 1.
            bas( 1+(i1-1)*mesh:i1*mesh , 1+(i2-1)*mesh:i2*mesh, 0 ) = 10.0

            ! now add the wave function correction
            if( any( c /= 0. ) ) then
              is = 0
              do iz = 1, ng(3)     ; rv(3) = iz*h(3) - off(3) ; r2v(3) = rv(3)*rv(3)
                do iy = 1, ng(2)   ; rv(2) = iy*h(2) - off(2) ; r2v(2) = rv(2)*rv(2)
                  do ix = 1, ng(1) ; rv(1) = ix*h(1) - off(1) ; r2v(1) = rv(1)*rv(1)
                    rr = r2v(1) + r2v(2) + r2v(3)

                    if( rr < rc2 ) then
                      is = is+1 ! count elements in the sphere
                      t = 0.
                      rs = sqrt( rr ) ; rs = max(rs,1.E-6)
                      ! find the radial index on the exponential grid
                      ir = min( max( 1, nint(log(rs*bm1+1.)*am1+1.) ), nr-1)
                      Ylm = Ylmax_rl( a%s%ellmax, rv )
                      do ilnm = 1, a%s%mlnm
                        t = t + c(ilnm) * Ylm( a%s%ind_ilm(ilnm) ) * rs **( -a%s%ind_ell(ilnm) ) * &
                                ( a%s%rwf(ir,a%s%ind_iln(ilnm),1,I_TRU) - &
                                  a%s%rwf(ir,a%s%ind_iln(ilnm),1,I_SMT) )
!     if(o>0) write(7,*) 'ilnm=',ilnm,' c=',c(ilnm),' ilm=',a%s%ind_ilm(ilnm),' Ylm=',Ylm(a%s%ind_ilm(ilnm))
!     if(o>0) write(7,*) ' ir=',ir,' rs=',rs,' ell=',a%s%ind_ell(ilnm),' iln=',a%s%ind_iln(ilnm),' tru-smt=', &
!                          ( a%s%rwf(ir,a%s%ind_iln(ilnm),1,I_TRU) - a%s%rwf(ir,a%s%ind_iln(ilnm),1,I_SMT) )
                      enddo ! ilnm
!     if( is == 99 ) stop 'plot_PAW_basis_function: test for values.'
!                       bas(ix,iy,iz) = bas(ix,iy,iz) + t
                      tmax = max(abs(t),tmax)
                      bas(ix,iy,0) = bas(ix,iy,0) + t
!     if(o>0) write(7,'(A,I6,A,F10.3,A,EN26.12E3)') 'ir =',ir,' rs =',rs,' t =',t
                    endif ! r*r < rcut^2

                  enddo ! ix
                enddo ! iy
              enddo ! iz
            endif ! any c /= 0

!             bas(:,:,0) = 0.
!             do iz = 1, ng(3)
!               bas(:,:,0) = bas(:,:,0) + bas(:,:,iz)
!             enddo ! iz
            call write_bmp_file( filename=gridpoint, data=bas(:,:,0) ) !, style=... )
          endif ! middle in y
          endif ! middle in z

        enddo ! i1
      enddo ! i2
    enddo ! i3
    if(o>0) write(o,'(2A,2(I0,A),F0.1,A)') sym, fun, &
        is, ' /', product(ng), ' elements in the sphere, ', is/(product(ng)*.01), ' %'
    if(o>0) write(o,'(3A,ES16.6E2)') sym, fun, 'max{|t|} =', tmax
    stop 'plot_PAW_basis_function: Bitmap files written.'
  endsubroutine ! plot_PAW_basis_function

!- plot_basis
#endif

  !! estimates roughly the memory requirement for a set of computation parameters
  status_t function estimate_memory( global, gc, nscale, wf_complex, natoms, nhist ) result( ios )
  use configuration, only: o
  use type_info, only: info
  use type_grid, only: grid
    type(info), intent(in)        :: global
    type(grid), intent(in)        :: gc ! coarse grid
    integer, intent(in)           :: nscale !! grid refinement factor (ge/gd)
    logical, intent(in)           :: wf_complex !! are the Kohn-Sham wave functions complex-valued?
    integer, intent(in)           :: natoms !! number of all atoms
    integer, intent(in), optional :: nhist !! number of history steps for mixing methods, default=1

    character(len=*), parameter   :: SERIAL(0:1) = (/ ' total ', ' / proc' /)
    ! choose this for MiByte, GiByte ..., integer powers of 2^10
    character(len=*), parameter   :: ABBREV(0:7) = (/ '  ','ki','Mi','Gi','Ti','Pi','Ei','Hi'/)
    real, parameter               :: POWER(0:7) = 2.**(/0,10,20,30,40,50,60,70/)
!!     ! choose this for MByte, GByte ..., integer powers of 10^3
!!     character(len=*), parameter   :: ABBREV(0:7) = (/ ' ','k','M','G','T','P','E','H'/)
!!     real, parameter               :: POWER(0:7) = 10.**(/0,3,6,9,12,15,18,21/)
    real, parameter :: ASSUMED_NMAX=2., ASSUMED_LMAX=3., ASSUMED_NRMX=2000., ASSUMED_NH=4.
    integer(kind=8) :: i, nh, pow3, nbsk, ns, nk, nb, ng(1:3), in, na
    real            :: est, n3c, n3d, n3e, rc, scaLP

    ios = 1 ; if( o == 0 ) return ! dont write to unit 0
    nh = 1  ; if( present(nhist) ) nh = nhist
    rc = 1. ; if( wf_complex ) rc = 2. ! decides if real or complex data type is used
    do in = 0, 1 ! 0: total, 1: per process
      if( in == 0 ) then ! per process
        ng = gc%ng_all(1:3)
        nb = global%nbands ! number of all bands
        nk = global%nkpoints ! number of all kpoints
        ns = global%nspins ! number of collinear spins {1,2}
        na = natoms ! number of all atoms
      else  ! per process
        ng = gc%ng(1:3)
        nb = global%nbnd ! number of parallized bands
        nk = global%nkpt ! number of parallized kpoints
        ns = global%nspn ! number of collinear spins {1,2,1:parallelized}
        na = ceiling( na/real(gc%nprocs) ) ! estimate
      endif ! per process
      nbsk = nb * ns * nk
      ns = global%nspins ! number of spins for potential {1,2,4}
      n3c = real( product(ng(1:3)) ) ; n3d = n3c * 8 ; n3e = n3d * nscale*nscale*nscale ! number of grid points
      est = na * ( ( ASSUMED_LMAX + 1 )**2 * ASSUMED_NRMX * 6 + ASSUMED_NRMX * ASSUMED_NMAX ) ! atoms
      ! main and scf
      est = est + ( ( nbsk + nb+1 )*n3c  +  1*n3d )*rc ! wave functions spsi and sspsi, neglecting gc%naprj
      est = est + ( 1*n3c + (2*ns+1+2*(nh+1))*n3d + (4*ns+3)*n3e ) ! potentials and history
      est = est + 23*rc*n3c ! DIIS
      est = est + product( ng(1:3) * 2 * nscale + 2*ASSUMED_NH ) ! LPL
      est = est + 5 * n3e ! + (5+2) * n3d + (5+2) * n3c + 5 * n3b ! !! multigrid ! PSS
      scaLP = 1. ! depends on ScaLAPACK ! subspace
#ifndef NOScaLAPACK
      scaLP = 1./real(gc%nprocs) ! depends on ScaLAPACK
#endif
      nb = global%nbands
      est = est + (4*nb+2)*nb*rc * scaLP + 2*n3c*rc
      if( global%nbnd < global%nbands ) est = est + (global%nbnd+1) * n3c * rc ! temp for band parallelization
      est = est * 8.
#ifdef SINGLE_PRECISION
      est = 0.5 * est
#endif
      pow3 = 0 ; do i = 1, 7 ; if( est > POWER(i) ) pow3 = i ; enddo ! i
      write(o,'(2A,F9.3,9A)',iostat=ios) sym,' estimate memory: ',est/power(pow3),' ',ABBREV(pow3),'Byte',SERIAL(in)
    enddo ! in
  endfunction ! estimate_memory

!- extended
#endif


  status_t function t_pawxmlreader()
#ifdef USE_PAWXMLREADER
    use mod_pawxmlreader, only: test
    t_pawxmlreader = test()
#else
    stop 'module pawxmlreader has not been built!'
#endif  
  endfunction ! t_pawxmlreader

  !! controls the invocation of test functions of various modules
  status_t function test_module( name ) result( s )
  use configuration, only: o ! output unit
#ifdef EXTENDED

#if 0
!===================================================================
  #!/bin/bash
  for m in *.mod
  do
    y=${m%.mod}
    z=${y##*/}
    echo "  use $z, only: t_$z => test"       >> use_file
    echo "    case( \""$m"\" ) ; s = t_$z()"  >> cases_file
    echo "      t, \""$m"\", &"               >> show_file
  done
  cat use_file cases_file show_file
!===================================================================
!=== this bash script generates the following code =================
!===================================================================
#endif
!
  use all_electron, only: t_all_electron => test
  use analysis, only: t_analysis => test
  use atomicomm, only: t_atomicomm => test
  use bandstructure, only: t_bandstructure => test
  use boundary, only: t_boundary => test
  use broyden_second_mod, only: t_broyden_second => test
  use cg_eigensolver, only: t_cg_eigensolver => test
  use chemistry, only: t_chemistry => test
  use communicators, only: t_communicators => test
  use configuration, only: t_configuration => test
  use constants, only: t_constants => test
  use constraints, only: t_constraints => test
  use control, only: t_control => test
  use cylindrical, only: t_cylindrical => test
  use debugtools, only: t_debugtools => test
  use density_functionals, only: t_density_functionals => test
  use diis_eigensolver, only: t_diis_eigensolver => test
  use dav_eigensolver, only: t_dav_eigensolver => test
  use display, only: t_display => test
  use dynamics, only: t_dynamics => test
  use fermidirac, only: t_fermidirac => test
  use fft_tools, only: t_fft_tools => test
  use forces, only: t_forces => test
  use gaussian_overlap, only: t_gaussian_overlap => test
  use gga_tools, only: t_gga_tools => test
  use grace, only: t_grace => test
  use harmonics, only: t_harmonics => test
  use init, only: t_init => test
  use inout, only: t_inout => test
  use input, only: t_input => test
  use interpolation, only: t_interpolation => test
  use lapack, only: t_lapack => test
  use laplacian, only: t_laplacian => test
  use lebedevlaikov, only: t_lebedevlaikov => test
  use mixing, only: t_mixing => test
  use mpiconst, only: t_mpiconst => test
  use mpitools, only: t_mpitools => test
  use operators, only: t_operators => test
  use paw_xmlfile, only: t_paw_xmlfile => test
  use paw_xmlwriter, only: t_paw_xmlwriter => test
  use pawdata, only: t_pawdata => test
  use pawdatafile, only: t_pawdatafile => test
  use poissonsolver, only: t_poissonsolver => test
  use prepare, only: t_prepare => test
  use radial_hartree, only: t_radial_hartree => test
  use radial_integrator, only: t_radial_integrator => test
  use radial_interpolation, only: t_radial_interpolation => test
  use radial_potential, only: t_radial_potential => test
  use relaxedcore, only: t_relaxedcore => test
  use scalapack, only: t_scalapack => test
  use sd_eigensolver, only: t_sd_eigensolver => test
  use selfcon, only: t_selfcon => test
  use sorting, only: t_sorting => test
  use spherical, only: t_spherical => test
  use subspace, only: t_subspace => test
  use symmetry, only: t_symmetry => test
  use toolbox, only: t_toolbox => test
  use type_atom, only: t_type_atom => test
  use type_bfun, only: t_type_bfun => test
  use type_comp, only: t_type_comp => test
  use type_criteria, only: t_type_criteria => test
  use type_element, only: t_type_element => test
  use type_grid, only: t_type_grid => test
  use type_info, only: t_type_info => test
  use type_item, only: t_type_item => test
  use type_kpoint, only: t_type_kpoint => test
  use type_llist, only: t_type_llist => test
  use type_proj, only: t_type_proj => test
  use type_rgrid, only: t_type_rgrid => test
  use type_species, only: t_type_species => test
  use type_state, only: t_type_state => test
  use unitsystem, only: t_unitsystem => test
!
  use toolbox, only: build_dep_tree, mail_notify
  use configuration, only: BugReportMailAddress
#endif
  use symmetry, only: kgen
    character(len=*), intent(in) :: name !! name of the module to be tested

    character(len=*), parameter  :: fun=' test module: ', t='   '
    integer :: show ! show a table of availabe modules 0: show no table

    s = 0 ; if( o == 0 ) return
    show = 0 ! 0: show no table
    write(o,'(4A,/)') sym, ': entering TestMode for "', trim(name), '"'

    selectcase( name ) ! name is "none", if no input file was found ...
    case( 'none' )                ; return ! ... and testmode was not specified
    case( '' )                    ; show = 1 ! 1: show a table
    ! ==========================================================
#ifdef EXTENDED
!
    case( "all_electron.mod" ) ; s = t_all_electron()
    case( "analysis.mod" ) ; s = t_analysis()
    case( "atomicomm.mod" ) ; s = t_atomicomm()
    case( "bandstructure.mod" ) ; s = t_bandstructure()
    case( "boundary.mod" ) ; s = t_boundary()
    case( "broyden_second.mod" ) ; s = t_broyden_second()
    case( "cg_eigensolver.mod" ) ; s = t_cg_eigensolver()
    case( "chemistry.mod" ) ; s = t_chemistry()
    case( "communicators.mod" ) ; s = t_communicators()
    case( "configuration.mod" ) ; s = t_configuration()
    case( "constants.mod" ) ; s = t_constants()
    case( "constraints.mod" ) ; s = t_constraints()
    case( "control.mod" ) ; s = t_control()
    case( "cylindrical.mod" ) ; s = t_cylindrical()
    case( "debugtools.mod" ) ; s = t_debugtools()
    case( "density_functionals.mod" ) ; s = t_density_functionals()
    case( "diis_eigensolver.mod" ) ; s = t_diis_eigensolver()
    case( "dav_eigensolver.mod" ) ; s = t_dav_eigensolver()
    case( "display.mod" ) ; s = t_display()
    case( "dynamics.mod" ) ; s = t_dynamics()
    case( "fermidirac.mod" ) ; s = t_fermidirac()
    case( "fft_tools.mod" ) ; s = t_fft_tools()
    case( "forces.mod" ) ; s = t_forces()
    case( "gaussian_overlap.mod" ) ; s = t_gaussian_overlap()
    case( "gga_tools.mod" ) ; s = t_gga_tools()
    case( "grace.mod" ) ; s = t_grace()
    case( "harmonics.mod" ) ; s = t_harmonics()
    case( "init.mod" ) ; s = t_init()
    case( "inout.mod" ) ; s = t_inout()
    case( "input.mod" ) ; s = t_input()
    case( "interpolation.mod" ) ; s = t_interpolation()
    case( "lapack.mod" ) ; s = t_lapack()
    case( "laplacian.mod" ) ; s = t_laplacian()
    case( "lebedevlaikov.mod" ) ; s = t_lebedevlaikov()
    case( "mixing.mod" ) ; s = t_mixing()
    case( "mpiconst.mod" ) ; s = t_mpiconst()
    case( "mpitools.mod" ) ; s = t_mpitools()
    case( "operators.mod" ) ; s = t_operators()
    case( "paw_xmlfile.mod" ) ; s = t_paw_xmlfile()
    case( "paw_xmlwriter.mod" ) ; s = t_paw_xmlwriter()
    case( "pawxmlreader.mod" ) ; s = t_pawxmlreader()
    case( "pawdata.mod" ) ; s = t_pawdata()
    case( "pawdatafile.mod" ) ; s = t_pawdatafile()
    case( "poissonsolver.mod" ) ; s = t_poissonsolver()
    case( "prepare.mod" ) ; s = t_prepare()
    case( "radial_hartree.mod" ) ; s = t_radial_hartree()
    case( "radial_integrator.mod" ) ; s = t_radial_integrator()
    case( "radial_interpolation.mod" ) ; s = t_radial_interpolation()
    case( "radial_potential.mod" ) ; s = t_radial_potential()
    case( "relaxedcore.mod" ) ; s = t_relaxedcore()
    case( "scalapack.mod" ) ; s = t_scalapack()
    case( "sd_eigensolver.mod" ) ; s = t_sd_eigensolver()
    case( "selfcon.mod" ) ; s = t_selfcon()
    case( "sorting.mod" ) ; s = t_sorting()
    case( "spherical.mod" ) ; s = t_spherical()
    case( "subspace.mod" ) ; s = t_subspace()
    case( "symmetry.mod" ) ; s = t_symmetry()
    case( "toolbox.mod" ) ; s = t_toolbox()
    case( "type_atom.mod" ) ; s = t_type_atom()
    case( "type_bfun.mod" ) ; s = t_type_bfun()
    case( "type_comp.mod" ) ; s = t_type_comp()
    case( "type_criteria.mod" ) ; s = t_type_criteria()
    case( "type_element.mod" ) ; s = t_type_element()
    case( "type_grid.mod" ) ; s = t_type_grid()
    case( "type_info.mod" ) ; s = t_type_info()
    case( "type_item.mod" ) ; s = t_type_item()
    case( "type_kpoint.mod" ) ; s = t_type_kpoint()
    case( "type_llist.mod" ) ; s = t_type_llist()
    case( "type_proj.mod" ) ; s = t_type_proj()
    case( "type_rgrid.mod" ) ; s = t_type_rgrid()
    case( "type_species.mod" ) ; s = t_type_species()
    case( "type_state.mod" ) ; s = t_type_state()
    case( "unitsystem.mod" ) ; s = t_unitsystem()
!
    ! ==========================================================
    case( 'dep' ) ; s = build_dep_tree( )
    case( 'mail' ) ; s = mail_notify( BugReportMailAddress, &
      'test message', message='test the mail function' )
#endif
    case( 'kgen' ) ; s = kgen() ! k-point mesh generator
    ! ==========================================================
    case( 'all' )             ; show = 3 ! 3: show how to test all
    case( 'show','list','?' ) ; show = 1 ! 1: show a table
    case default              ; show = 2 ! 2: name not found
    endselect ! name

    if( show == 0 ) stop

    write(o,'(4A)') '' ! empty line
    if( show == 2 ) &
    write(o,'(4A)') fun, 'no TestMode for "', trim(name), '".', ''
    write(o,'(2A)') fun, 'available modules:', &
      '', '', &
#ifdef EXTENDED
!
      t, "all_electron.mod", &
      t, "analysis.mod", &
      t, "atomicomm.mod", &
      t, "bandstructure.mod", &
      t, "boundary.mod", &
      t, "cg_eigensolver.mod", &
      t, "chemistry.mod", &
      t, "communicators.mod", &
      t, "configuration.mod", &
      t, "constants.mod", &
      t, "constraints.mod", &
      t, "control.mod", &
      t, "cylindrical.mod", &
      t, "debugtools.mod", &
      t, "density_functionals.mod", &
      t, "diis_eigensolver.mod", &
      t, "dav_eigensolver.mod", &
      t, "display.mod", &
      t, "dynamics.mod", &
      t, "fermidirac.mod", &
      t, "fft_tools.mod", &
      t, "forces.mod", &
      t, "gaussian_overlap.mod", &
      t, "gga_tools.mod", &
      t, "grace.mod", &
      t, "harmonics.mod", &
      t, "init.mod", &
      t, "inout.mod", &
      t, "input.mod", &
      t, "interpolation.mod", &
      t, "lapack.mod", &
      t, "laplacian.mod", &
      t, "lebedevlaikov.mod", &
      t, "mixing.mod", &
      t, "mpiconst.mod", &
      t, "mpitools.mod", &
      t, "operators.mod", &
      t, "paw_xmlfile.mod", &
      t, "paw_xmlwriter.mod", &
      t, "pawxmlreader.mod", &
      t, "pawdata.mod", &
      t, "pawdatafile.mod", &
      t, "poissonsolver.mod", &
      t, "prepare.mod", &
      t, "radial_hartree.mod", &
      t, "radial_integrator.mod", &
      t, "radial_interpolation.mod", &
      t, "radial_potential.mod", &
      t, "relaxedcore.mod", &
      t, "scalapack.mod", &
      t, "sd_eigensolver.mod", &
      t, "selfcon.mod", &
      t, "sorting.mod", &
      t, "spherical.mod", &
      t, "subspace.mod", &
      t, "symmetry.mod", &
      t, "toolbox.mod", &
      t, "type_atom.mod", &
      t, "type_bfun.mod", &
      t, "type_comp.mod", &
      t, "type_criteria.mod", &
      t, "type_element.mod", &
      t, "type_grid.mod", &
      t, "type_info.mod", &
      t, "type_item.mod", &
      t, "type_kpoint.mod", &
      t, "type_llist.mod", &
      t, "type_proj.mod", &
      t, "type_rgrid.mod", &
      t, "type_species.mod", &
      t, "type_state.mod", &
      t, "unitsystem.mod", &
!
      t, '====================', &
      t, 'dep', &
      t, 'accuracy', &
      t, 'mail', &
      t, 'kgen', &
      '', '', fun, 'please choose from the list above!', ''
    if( show == 3 ) &
    write(o,'(2A)') fun, 'test all modules with the following command', ''
    if( show == 3 ) stop 'for m in *.mod ; do ./paw -t $m ; done'
#else
      t, 'none', '', '', fun, 'please compile with -D EXTENDED!', ''
    s = 0
#endif
  endfunction ! test_module




  logical function command_line_arguments( infile, outfile, realcmplx, &
     checkmode, startmode, testmode, override, exec, xml_format, &
     tool_analyze ) result( infileset )
  !! reads and processes the command line arguments passed to the executable
  !!
  !! rekognizes options (starting with --keyword or -kw for abbreviations)
  !!        and names, the first name will be interpreted as input file name
  !! returns true if an input file has been specified
  use configuration, only: o ! output unit
  use configuration, only: CodeName, CodeVersion
  use configuration, only: STOPonERROR
  use configuration, only: WARNING
  use configuration, only: ExampleFileName
  use configuration, only: show_KeyWords, KeyWord, I_KeyWord_ELEM
  use configuration, only: OutputFileName_STDOUT
  use init, only: example_input_file
  use type_element, only: show_electronic_configuration
  use toolbox, only: convert2lowercase
  use configuration, only: set_output_unit
    character(len=*), intent(out)         :: infile      !! name of the input file
    character(len=*), intent(out)         :: outfile     !! name of the output file
    integer, intent(out)                  :: realcmplx   !! 1: real (specify "--real" or "-r"), 2:complex (specify "--complex" or "-c")
    integer, intent(out)                  :: checkmode   !! 0: no CheckMode, 1: CheckMode (specify "--CheckMode" or "-cm")
    integer, intent(out)                  :: startmode   !! if the lowest bit is set, load wave functions (specify "-l") <br> the 2nd to lowest bit: smooth density (specify "-L") 
    integer, intent(out)                  :: xml_format  !! if different from 0 use xml paw potentials
    character(len=*), intent(out)         :: testmode    !! name of the module to be tested (specify "--test <modulename>.mod")
    character(len=*), intent(out)         :: override(:,:) !! additional lines that will be interpreted as appended to the input file <br> specify "--overrride" or "-ov" followed by the line content.
    integer, intent(out)                  :: tool_analyze !! 0: no Tool for analyzation, >0 respective tool used for analysis
    !! In most cases, quotes are needed for the line content, because the line consists of a keyword followed by values, but the blank in between would make two or more command line arguments from it,
    !! the content may override those keywords specified in the input file, block structure are excepted.
    !!  *Example* call <br> ./paw -r NH3 -ov "spin 2"
    !! to compute ammonia spin polarized, even though this has not been specified in the input file "NH3".
    character(len=*), intent(out), optional :: exec      !! name of the binary executable of this program

    character(len=*), parameter :: fun = ' command_line_arguments: '
    integer, parameter          :: MAXNARGS = 14 ! only MAXNARGS are read in
#ifdef FULL_DEBUG
    integer             :: u = 6 ! std output
#else
    integer, parameter  :: u = 0 ! no output
#endif
    string_t :: a(0:MAXNARGS+1), a_lowercase
    status_t :: ios, ist
    integer  :: nargs, iarg, i, ilen, nprocs_checkmode, n_pl(2) ! number (and index) of preset and override lines
    n_pl = 0 ! init counters for preset and override
    tool_analyze = 0 ! disable anylazation mode by default

#ifdef NOMPI
cDBG    ist = set_output_unit( 6 )
#else
cDBG    ist = set_output_unit( 0 ) ! do NOT write ! (because this routine runs before the communicators and ranks are determined)
#endif
    nargs = command_argument_count()
    ! warning for cutoff of the argument list
    if( nargs > MAXNARGS ) write(*,'(4A,9(I0,A))') sym, fun, WARNING(0), &
      'list of command line arguments was cut off after arg #', MAXNARGS
    nargs = min( nargs, MAXNARGS )

    a = '' ! init arguments
    do iarg = 0, nargs
      ! read out the string parts of the command line arguments
      ! the program has been called with, a(0) is the name of the executable
      call get_command_argument( iarg, a(iarg), ilen, ios )
cDBG  if( ios /= 0 .and. u>0) write(u,'(4A,9(I0,A))') sym, fun, WARNING(0), &
cDBG    'argument #',iarg,', input string length is ',ilen,' > ',len(a(1)),' (max), iostatus = ',ios
    enddo ! iarg

    if( present(exec) ) exec = a(0) ! name of the executable

    ! init output vars
    infile    = '' ! init clear
    outfile   = OutputFileName_STDOUT ! init as writing to stdout by default
    infileset = .false. ! init as not yet set
    realcmplx = 0 ! 0:auto, 1:real, 2:complex
    checkmode = 0 ! 0:run, >0:run in checkmode pretending to run on # processes
    startmode = 0 ! 0:generate, 1:load_wf, 2:load_density, 3:load_both
    testmode  = 'none' ! init
    override  = '' ! init clear
    xml_format = 0

    iarg = 0 ! init counter
    do while( iarg < nargs )
      iarg = iarg+1 ! count up

      i = convert2lowercase( u=a(iarg), l=a_lowercase ) ! convert the argument to all lower case letters

      selectcase( a_lowercase )
      !-----------------------------------------------------------------------------------
      case( '-a', '--analyze-cube')
        if( infileset ) then
cDBG    if(u>0) write(u,'(3A,I0,9A)') sym, fun, 'argument #', iarg, ' enables the analyze mode.'
cDBG    if(u>0) write(u,'(3A,I0,9A)') sym, fun, 'argument #', iarg, ' is followed by "', trim(a(iarg+1)), '".'
          read(unit=a(iarg+1),fmt=*,iostat=ios) tool_analyze
          if( ios == 0 .and. tool_analyze > 0 ) then
            iarg = iarg+1 ! tool number valid, forward the arg counter
          else
            tool_analyze = 1
          endif !ios
        else
          write(*,'(a)') "-a or --analyze-cube was specified but the input file was not, mind the order of arguments!"
        endif ! infileset
      !-----------------------------------------------------------------------------------
      case( '-r', '--real' )
        if( realcmplx == 2 ) then
cDBG      if(u>0) write(u,'(3A,I0,9A)') sym, fun, 'argument #', iarg, ' (real wave functions) is ignored!'
        else  ! ignore
          realcmplx = 1
cDBG      if(u>0) write(u,'(3A,I0,9A)') sym, fun, 'argument #', iarg, ' enables real-valued wave functions.'
        endif ! ignore
      !-----------------------------------------------------------------------------------
      case( '-c', '--complex' )
        if( realcmplx == 1 ) then
cDBG      if(u>0) write(u,'(3A,I0,9A)') sym, fun, 'argument #', iarg, ': complex wins over real wave functions!'
        endif
        realcmplx = 2
cDBG    if(u>0) write(u,'(3A,I0,9A)') sym, fun, 'argument #', iarg, ' enables complex-valued wave functions.'
      !-----------------------------------------------------------------------------------
      case( '-l', '--load' )
        selectcase( a(iarg)(1:3) ) ! case sensitive
        case( '-l', '--l' ) ; startmode = startmode + 1 ! wave functions
cDBG      if(u>0) write(u,'(3A,I0,9A)') sym, fun, 'argument #', iarg, ' tries to load an existing wave function files.'
        case( '-L', '--L' ) ; startmode = startmode + 2 ! density
cDBG      if(u>0) write(u,'(3A,I0,9A)') sym, fun, 'argument #', iarg, ' tries to load an existing density file.'
        endselect ! a
      !-----------------------------------------------------------------------------------
      case('-xml')
cDBG    if(u>0) write(u,'(3A,I0,9A)') sym, fun, 'argument #', iarg, ' activates the usage of .xml-formatted PAW data sets.'
        xml_format = 1
      !-----------------------------------------------------------------------------------
      case( '-i', '--ignore' )
cDBG    if(u>0) write(u,'(3A,I0,9A)') sym, fun, 'argument #', iarg+1, ' will be ignored on purpose.'
        iarg = iarg+1 ! forward the counter to the next arg, because the following is ignored.
      !-----------------------------------------------------------------------------------
      case( '-ps', '--preset' ) !! define e.g. variables before the input file is read in
        if( n_pl(1) >= ubound(override,1) ) stop 'command_line_arguments: array override(:,1) has not enough entries.'
        n_pl(1) = n_pl(1)+1 ! count up
        iarg = iarg+1 ! forward the counter for the next arg, because --present requires one
        override(n_pl(1),1) = a(iarg) ! the argument after --preset
      !-----------------------------------------------------------------------------------
      case( '-ov', '--override' ) !! append lines to the input file
        if( n_pl(2) >= ubound(override,1) ) stop 'command_line_arguments: array override(:,2) has not enough entries.'
        n_pl(2) = n_pl(2)+1 ! count up
        iarg = iarg+1 ! forward the counter for the next arg, because --override requires one
        override(n_pl(2),2) = a(iarg) ! the argument after --override
      case( '-o', '--out', '--output' ) !! append lines to the input file
        outfile = a(iarg+1) ! assume that the next argument specifies the name of the output file
        if( outfile(1:1) == '-' ) then ! the next argument is another control sequence
          outfile = '' ! empty string ==> writing to <infile>.out
        else ; iarg = iarg+1 ! forward the counter for the next arg, because --output required one
        endif ! next agr is a valid name for the outfile
cDBG    if(u>0) write(u,'(3A,I0,9A)') sym, fun, 'argument #', iarg, ' sets the output file to "', trim(outfile), '".'
      !-----------------------------------------------------------------------------------
      case( '-cm', '--check', '--checkmode' )
        checkmode = 1
cDBG    if(u>0) write(u,'(3A,I0,9A)') sym, fun, 'argument #', iarg, ' enables the CheckMode.'
cDBG    if(u>0) write(u,'(3A,I0,9A)') sym, fun, 'argument #', iarg, ' is followed by "', trim(a(iarg+1)), '".'
        read(unit=a(iarg+1),fmt=*,iostat=ios) nprocs_checkmode
        if( ios == 0 .and. nprocs_checkmode > 0 ) then
          checkmode = nprocs_checkmode
cDBG      if(u>0) write(u,'(3A,9(I0,A))') sym, fun, 'argument #', iarg+1, ' pretends to run ', nprocs_checkmode, ' processes (CheckMode).'
          iarg = iarg+1 ! arg valid, forward the counter for the next arg.
        endif ! ios ...
#ifdef EXTENDED
      !-----------------------------------------------------------------------------------
      case( '-g', '--gen', '--generate' )
        if( infileset ) then
          startmode = startmode + STARTMODE_GENERATE_PAWDATA ! if it returns from generate
        else
          write(*,*) "-g --gen or --generate was specified but the input file was not, mind the order of arguments!"
        endif
#endif
      !-----------------------------------------------------------------------------------
      case( '-t', '--test', '--testmode' )
        ist = set_output_unit( 6 ) ! Caution: output unit of module configuration set to stdout
        testmode = a(iarg+1)
cDBG    if(u>0) write(u,'(3A,I0,9A)') sym, fun, 'argument #', iarg, ' enables the TestMode with parameter "', trim(testmode), '".'
        iarg = iarg+1 ! forward the counter for the next arg, because --test requires one
      !-----------------------------------------------------------------------------------
      case( '-h', '--help', '--h', '-help' )
        ios = usage_help( executablename=a(0) ) ; stop
      !-----------------------------------------------------------------------------------
      case( '-kw', '--keywords' )
cDBG    if(u>0) write(u,'(3A,I0,9A)') sym, fun, 'argument #', iarg, ' lists a keywords usable in the input file .'
        ios = show_KeyWords( outunit=6 ) ; stop
!         write(*,'(3A,/,999A)') sym, ': ', 'keywords to be used in the input file:', &
!           ( " '",trim(KeyWord(i)),"'", i=1,size(KeyWord) ) ; stop
      !-----------------------------------------------------------------------------------
      case( '-ex', '--example' )
cDBG    if(u>0) write(u,'(3A,I0,9A)') sym, fun, 'argument #', iarg, ' writes an example input file.'
        ios = example_input_file( filename=ExampleFileName ) ; stop
      !-----------------------------------------------------------------------------------
      case( '-e', '--elem', '--element' )
        write(*,'(A)') '', show_electronic_configuration( a(iarg+1) ), '' ; stop
      !-----------------------------------------------------------------------------------
      case( '-v', '--version' )
        write(*,'(9A)') CodeName, ' ', CodeVersion() ; stop
      !-----------------------------------------------------------------------------------
      case default ! the command line argument is not marked as an option, so the input file is expected.

        if( infileset ) then ! the input file has already been found, so the arguments is missplaced or misstyped
cDBG      if(u>0) write(u,'(3A,I0,9A)') sym, fun, 'unrekognized argument #', iarg, ' ("', trim(a(iarg)),'").'
          if( STOPonERROR ) stop 'command_line_arguments: unrekognized argument.'
cDBG      if(u>0) write(u,'(9A)') sym, fun, '"', trim(a(iarg)),'") was ignored.'
        elseif( a_lowercase(1:1) == '-' ) then
          ! looks like a flag, but did not match any of the defined flags
cDBG      if(u>0) write(u,'(4A,I0,9A)') sym, fun, WARNING(0), 'argument #', iarg, ' ("', trim(a(iarg)),'") is not a defined flag.'
        else  ! infileset
          infile = adjustl(a(iarg)) ! this string must NOT be converted to lower case!
          infileset = .true.
cDBG      if(u>0) write(u,'(3A,I0,9A)') sym, fun, 'argument #', iarg, ' was rekognized as input file "', trim(a(iarg)),'".'
        endif ! infileset

      endselect ! a_lowercase

    enddo ! while

  endfunction ! command_line_arguments

  !! shows the possible command line options and the way, the input file(s) are specified.
  !! The usage help is shown, if no input file is given or the program dies because incorrect
  !! command line options were encountered.
  status_t function usage_help( executablename ) result( ios )
    character(len=*), intent(in) :: executablename !! name of the executable binary

    write(*,'(3A,9(/,A))',iostat=ios) &
      ' usage: ',trim(executablename),'  <inputfile>  [options]', &
      '   options:', &
      '   -o   --output [<file>]                     redirect output', &
      '   -l   --load                                load existing wave functions', &
      '   -L   --Load                                load existing density files', &
      '   -ps  --preset <line>                       lines read before input file', &
      '   -ov  --override <line>                     append line to input file', &
      '        --real                                enforce real wave functions', &
      '        --complex                             enforce complex wave functions', &
#ifdef EXTENDED
      '   -i   --ignore <arg>                        the next arg will not be parsed', &
      '   -g   --generate                            generate PAW data file', &
      '   -e   --element <Z>                         show default element configuration', &
      '   -t   --test <module>                       debug function testing modules', &
#endif
      '   -cm  --check [<np>]                        only check the input file', &
      '        --example                             writes an example input file', &
      '   -kw  --keywords                            lists all input file keywords', &
      '   -v   --version                             shows the version number', &
      '   -h   --help                                this usage help', &
      '   -a   --analyze-cube [<mode>]               analyze cube file', &
      '' ! empty line
  endfunction ! usage_help

endmodule ! rs_paw


!! @author Paul Baumeister
!!
!! this program
!!  -- reads the command_line_arguments
!!  -- calls start_real or start_complex from module rs_paw
!!
program paw
  use rs_paw, only: command_line_arguments
  use rs_paw, only: start_real, start_complex
  use rs_paw, only: test_module, usage_help
  use rs_paw, only: analyze_cube_file
implicit none

  string_t :: projectname !! title of the project and input file name
  string_t :: outputname !! name of the output file
  integer  :: realcmplx !! use real/complex numbers for the Kohn-Sham wave functions, 1:real, 2:complex
  integer  :: checkmode !! 0:run >0: CheckMode, read in input file(s) and perform more checks and produce nicer output than in a real run
  integer  :: startmode !! start options (such as load...)
  integer  :: xml_format !! if different from 0 use xml paw potentials
  string_t :: testmode  !! name of a module to run the self-test
  string_t :: executable !! name of the binary executable
  string_t :: override(5,1:2) !! preset(1) and override(2) input lines passed via the command line
  real     :: totalenergy !! result: the total energy
  status_t :: is
  integer  :: tool_analyze !! :0 no analyzation tool, >0 use the specified analyzation tool on the input file

  if( command_line_arguments( projectname, outputname, realcmplx, checkmode, &
        startmode, testmode, override, executable, xml_format, tool_analyze ) ) then
    ! an input file name was specified

    if( tool_analyze >0 ) then
       selectcase (tool_analyze)
       case(1) ; call analyze_cube_file(projectname)
       endselect !tool_analyze
    else
  
      if( realcmplx == 0 .or. realcmplx ==  1 ) & ! 0:auto, 1:real
        totalenergy = start_real   ( projectname, override, checkmode, startmode, outputname, realcmplx, xml_format )
      ! if realcmplx was set to auto and the system requires complex wave functions, realcmplx == -2 now
      if( realcmplx == 2 .or. realcmplx == -2 ) & ! 2:complex, -2:complex with silent initialization
        totalenergy = start_complex( projectname, override, checkmode, startmode, outputname, realcmplx, xml_format )
    endif ! tool_analyze

  else  ! command_line_arguments
    write(*,'(/9A)') trim(executable), ': no input file given.'
    is = test_module( testmode ) ! maybe the test mode has been specified
    is = usage_help( executable ) ! show how to use the code
  endif ! command_line_arguments

  stop ! done
endprogram ! paw

#endif
#endif

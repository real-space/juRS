#include "config.h"

! #define DEBUG

!! @author Paul Baumeister
!! @version 4.04
!!
!! writes the PAW data into pawdata.00Z formated files
module pawdatafile
  use configuration, only: o
implicit none
  private !! default for this namespace

  character, parameter, public        :: CommentChar = '#'
  character(len=*), parameter, public :: PawFileName = 'pawdata.' ! "pawdata.00Z"
  ! keywords
  character(len=*), parameter, public :: KEY_Zcor = 'AtomicNumber=' !! Z
  character(len=*), parameter, public :: KEY_Conf = 'element' !! e%config
  character(len=*), parameter, public :: KEY_XCFn = 'ExchangeCorrelation=' !! kxc
  character(len=*), parameter, public :: KEY_Eunt = 'EnergyUnit=' !! ENERGYUNIT
  character(len=*), parameter, public :: KEY_Etot = 'TotalEnergy=' !! etot
  character(len=*), parameter, public :: KEY_nVEs = 'ValenceElectrons=' !! nve
  character(len=*), parameter, public :: KEY_nWxp = 'WeinertExponent=' !! nWexp
  character(len=*), parameter, public :: KEY_Grid = 'RadialGrid=' !! irmx ...
  character(len=*), parameter, public :: KEY_nPrj = 'Projectors=' !! nn(0:ellmax)
  character(len=*), parameter, public :: KEY_RhoC = 'CoreDensities' !! ...
  character(len=*), parameter, public :: KEY_Ecor = 'KineticCoreEnergy=' !! ecor
  character(len=*), parameter, public :: KEY_Vbar = 'ZeroPotential' !! ...
  character(len=*), parameter, public :: KEY_rPhi = 'PartialWaves' !! tru,smt,prj
  character(len=*), parameter, public :: KEY_Rcut = 'CutoffRadius=' !! Z
  character(len=*), parameter, public :: KEY_Vers = 'PawDataFileVersion=' !! Nversion
  character(len=*), parameter, public :: KEY_dKin = 'KineticEnergyDeficit=' !! dkin
  character(len=*), parameter, public :: KEY_Warn = 'Warning=' !! message appearing in the output
  character(len=*), parameter, public :: KEY_Cmnt = 'Comment=' !! some comment that appears in the output

#ifdef EXTENDED
!+ extended

  public :: write_pawdata_file
  public :: test

  character, parameter, private :: CC = CommentChar 
  integer, parameter, private :: PawFileVersion = 9
  character(len=*), parameter, private :: EnergyUnit = 'Hartree'

  character(len=*), parameter, private :: sym = 'pawFile' !! module symbol

  contains

  status_t function write_pawdata_file( iZ, Z, Nve, r, drdi, nn, occ, xc_name, nWexp, rhocor, &
                       ecor, etot, eval, vBAR, rphi, rcut, dkin, config, comment ) result( ios )
  use type_element, only: symbol, show_electronic_configuration
  use configuration, only: WARNING, ERROR
  implicit none
    ! parameter
    character(len=*), parameter           :: fun = ' write_pawdata_file: '
    integer, parameter                    :: I_TRU=1, I_SMT=2, I_PRJ=3
    integer, parameter                    :: u = 8
    character(len=*), parameter           :: HorizontalLine = ' ---------------------------------------------'
    character(len=*), parameter           :: FMT1 = '(9999ES24.16E2)'
    character(len=*), parameter           :: FMT2 = '(9999ES24.15E3)'
    ! arguments
    integer, intent(in)                   :: iZ
    real, intent(in)                      :: Z, Nve
    real, intent(in)                      :: r(:), drdi(:)
    integer, intent(in)                   :: nn(0:)
    real, intent(in)                      :: occ(-1:,1:,0:) !! valence occupation(-1:+1,enn,ell)
    character(len=*), intent(in)          :: xc_name
    integer, intent(in)                   :: nWexp !! Weinert exponent
    real, intent(in)                      :: rhocor(:,:,:) !! (irmx,1,I_TRU:I_SMT)
    real, intent(in)                      :: ecor, etot, eval(:,:), vBAR(:)
    real, intent(in)                      :: rphi(:,:,:,:) !! (irmx,mln,1,I_TRU:I_PRJ)
    real, intent(in)                      :: rcut(0:) !! cutoff radii
    real, intent(in)                      :: dkin(:,:,0:) !! (ENNMAX,ENNMAX,0:ELLMAX)
    character(len=*), intent(in)          :: config
    character(len=*), intent(in), optional:: comment

    ! local vars
    character(len=16)                     :: filename
    integer                               :: nspin, irmx, ellmax, ell, enn, iln, inl
    real                                  :: oc(1:2)

#ifdef DEBUG
    if(o>0) write(o,'(3A,99F5.1)') sym, fun, 'occupation', occ
#endif

    ! find the number of elements in the radial grid
    irmx = size( r, 1 )
    if( irmx > 99999 ) stop 'write_pawdatafile: only radial grid < 100k implemented, change format otherwise.'

    ellmax = ubound(nn,1)
    nspin = size( rphi, 3 )
    if( nspin == 1 ) then
      !
    elseif( nspin == 2 ) then
      if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'only is==1 will be written.'
    else
      if(o>0) write(o,'(9A)') sym, fun, ERROR, 'nspin not in {1,2}.'
      return
    endif ! nspin in {1,2}

    write( unit=filename, fmt='(A,I3.3)' ) PawFileName, iZ ! create the filename "pawdata.00Z"

    open( unit=u, file=filename, action='write', iostat=ios )
    if( ios /= 0 ) then
      if(o>0) write(o,'(9A)') sym, fun, 'unable to open "', trim(filename), '" for writing.'
      return
    endif ! ios /= 0

    write(u,'(9A)')   CC, HorizontalLine
    write(u,'(9A)')   CC, '  Projector Augmented Wave method data for ', symbol(iZ)
    write(u,'(9A)')   CC, HorizontalLine
    if( present( comment ) ) &
    write(u,'(9A)')   CC, ' ', trim(comment)
    write(u,'(9A)')   CC
    write(u,'(9A)')   CC, ' ', KEY_Cmnt, ' "if activated, this appears as a comment in the output"'
    write(u,'(9A)')   CC, ' ', KEY_Warn, ' "if activated, this appears as a warning in the output"'
    write(u,'(9A)')   CC

    write(u,'(9A)')   trim(show_electronic_configuration( symbol(iZ) ))
    write(u,'(9A)')   CC
    write(u,'(2A,I0)') KEY_Vers, ' ', PawFileVersion
    write(u,'(9A)')   CC
    write(u,'(9A)')   KEY_Eunt, '  ', EnergyUnit
    write(u,'(9A)')   CC
    write(u,'(9A)')   KEY_XCFn, '  ', trim(xc_name)
    write(u,'(9A)')   CC

    write(u,'(2A,F0.3,9A)') KEY_Zcor, '  ', Z
    write(u,'(2A,F0.9,9A)') KEY_Etot, '  ', etot, '  ', EnergyUnit
    write(u,'(2A,F0.3,9A)') KEY_nVEs, '  ', nve
    write(u,'(9A)')     CC
    write(u,'(2A,I0)')  KEY_nWxp, '  ', nWexp

    ! number of projectors in each ell-channel
    write(u,'(9A)')     CC
    write(u,'(9A)')     CC, '          ', '      s      p      d      f      g'
    write(u,'(A,9I7)')  KEY_nPrj, nn(0:ellmax)
    ! cutoff radii
    write(u,'(A,9F7.3)') KEY_Rcut, rcut(0:ellmax)
    ! radial grid
    write(u,'(9A)')     CC
    write(u,'(2A,I0,A)') KEY_Grid, '  ',irmx,'  points'
    write(u,FMT1) r
    write(u,FMT1) drdi

    if( Z > 1 ) then
    ! kinetic core state energy
    write(u,'(9A)')     CC
    write(u,'(2A,F0.6,9A)') KEY_Ecor, '  ', ecor, '  ', EnergyUnit
    ! core densities
    write(u,'(9A)')     CC
    write(u,'(9A)')     KEY_RhoC, '   TRUE and SMOOTH (4*Pi*rho)'
#ifdef DEBUG
    if(o>0) write(o,'(3A,F10.3,9A)') sym, fun, 'norm of the TRU core charge', sum( rhocor(:,1,I_TRU)*r(:)**2*drdi(:) ), ' e'
#endif
    write(u,FMT2) rhocor(:,1,I_TRU)
    write(u,FMT2) rhocor(:,1,I_SMT)
    endif ! Z > 1

    ! potential
    write(u,'(9A)')     CC
    write(u,'(9A)')     KEY_Vbar
    write(u,FMT1) vBAR(:)

    ! partial waves and projector
    write(u,'(9A)')     CC
    write(u,'(3A,2A3,2A6,A16,9A)') &
                        CC, ' ', KEY_rPhi, 'l', 'n', 'occup', 'occdn', 'e.      ', '  ', EnergyUnit
    write(u,'(9A)')     CC, '     0.000E+00 ... TRUE   partial wave (r*twf)'
    write(u,'(9A)')     CC, '     0.000E+00 ... SMOOTH partial wave (r*swf)'
    write(u,'(9A)')     CC, '     1.000E+00 ... PROJECTOR function  (r^-l*prj)'
    write(u,'(9A)')     CC

    iln = 0
    do ell = 0, ellmax
      do enn = 1, nn(ell)
        iln = iln+1
        write(u,'(A,2I3,2F6.2,F16.6,9A)') KEY_rPhi, ell, enn, occ(+1,enn,ell), occ(-1,enn,ell), eval(iln,1), '  ', EnergyUnit
        write(u,FMT1) rphi(:,iln,1,I_TRU)
        write(u,FMT1) rphi(:,iln,1,I_SMT)
        write(u,FMT1) rphi(:,iln,1,I_PRJ)
      enddo ! enn
    enddo ! ell

    write(u,'(9A)')  CC
    write(u,'(9A)')  KEY_dKin, '  ', EnergyUnit
    do ell = 0, ellmax
      write(u,'(2A,I0)') CC, 'l= ', ell
      do enn = 1, nn(ell)
        write(u,'(9ES24.16E2)') dkin(1:nn(ell),enn,ell)
      enddo ! enn
    enddo ! ell
    write(u,'(9A)')  CC

    ! end
    write(u,'(9A)')  CC, HorizontalLine
    close( unit=u )

    if(o>0) write(o,'(/5A/)') sym, fun, '"', trim(filename), '" written.'
  endfunction ! write_pawdata_file


  status_t function test( )
    write(*,*,iostat=test) __FILE__,' no module test implemented!'
  endfunction ! test

!- extended
#endif
endmodule ! pawdatafile

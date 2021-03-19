#include "config.h"

! #define DEBUG


#ifdef DEBUG
#define cDBG
#else
#define cDBG !DBG
#endif


!! @author Paul Baumeister
!! @version 3.0
!!
!! nice numbers and some ASCII graphics
module display
implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: sym = 'DSP' !! module symbol

  public :: display_title
  public :: display_sample
  public :: display_states
  public :: display_forces
  public :: display_cell_ascii
  public :: display_decomp_ascii
#ifdef EXTENDED
  public :: display_atoms_povray
  public :: display_decomp_xfig
  public :: display_atoms_xfig
  public :: display_stopwatch
  public :: display_VTK_file
  public :: error_digits
  public :: test
#endif

  !! output unit for warnings and error messages
#ifdef DEBUG
  iounit_t, parameter :: o=6 !! 6:stdout (default) 0: no output
#else
  iounit_t, parameter :: o=0 !! 6:stdout (default) 0: no output
#endif

  ! abbreviations for the 10^(3*n), achar(181) = greek \mu (micro)
  ! character(len=1), parameter             :: POWER3(-5:5) = &  
  !  (/'f','p','n',achar(181),'m',' ','k','M','G','T','P'/)

  contains


  !! shows the name and version number of this code
  status_t function display_title( modul, unt ) result( ios )
  use configuration, only: CodeName, FullCodeName, v => CodeVersion, BugReportMailAddress
    ! arguments
    character(len=*), intent(in)    :: modul !! symbol of calling module
    iounit_t, intent(in)            :: unt   !! write to this unit
    ! local vars
    character(len=4)                :: M
    if( unt <= 0 ) return

    M = '' ; M(1:3) = modul

    write( unt,'(A4,A)' )   M, '+---------------------------------------------------------------+'
    write( unt,'(A4,A,A8,A,A32,A)',iostat=ios) &
                            M, '|---  ', CodeName, '    ', FullCodeName,         '    ----------|'
    write( unt, '(A4,A)'  ) M, '|---------------------------------------------------------------|'
    write( unt,'(A4,A,A8,A)',iostat=ios) &
                            M, '|-----------------------------------  version ', v(),'----------|'
!   write( unt, '(A4,A)'  ) M, '|----- Real-Space Density-Functional Calculator ----------------|'
    write( unt, '(A4,A)'  ) M, '|---   Density-Functional Calculator  --------------------------|'
    write( unt, '(A4,A)'  ) M, '|---------------------------------------------------------------|'
    write( unt, '(A4,A)'  ) M, '|---   Paul F. Baumeister, Institute for Advanced Simulation  --|'
    write( unt, '(A4,A)'  ) M, '|-----------------------  Forschungszentrum Juelich, Germany  --|'
    write( unt, '(A4,A)'  ) M, '|---------------------------------------------------------------|'
    write( unt, '(A4,A)'  ) M, '+---------------------------------------------------------------+'
    write( unt, '(A4,9A)' ) M, '       Please report bugs to   ', BugReportMailAddress
    write( unt, '(A4,A)'  ) ! empty line
  endfunction ! display_title



  !! shows the basic input parameters of the simulation
  status_t function display_sample( modul, unt, g, a, totalcharge ) result( ios )
  use type_atom, only: atom
  use type_grid, only: grid
  use constants, only: Pi
  use unitsystem, only: eV, eV_, Ang, Ang_
    ! args
    character(len=*), intent(in)    :: modul !! symbol of calling module
    iounit_t, intent(in)            :: unt   !! write to this unit
    type(grid), intent(in)          :: g
    type(atom), intent(in), optional :: a(:)
    real, intent(in), optional :: totalcharge
    ! local vars
    integer                     :: na, ia
    iounit_t                    :: u
    character(len=4)            :: M
    real                        :: E, h, ng(3) ! average
    u = unt
    if( u <= 0 ) return
    M = '' ; M(1:3) = modul

    h = product( g%h(1:3) )**(1./3.) ! geometric average grid spacing
    E = 0.5*(Pi/h)**2 ! geometric average of cutoff energies

      write(u,'(9A)',iostat=ios)          M, '+---------------------------------------------------------------+'
      write(u,'(9A)')                     M, '|---InputQuantity ---------------------------------------- unit |'
      write(u,'(9A)')                     M, '|---------------------------------------------------------------|'
      write(u,'(2A,3F10.4,9A)')           M, '|---SupercellSize ---',            g%s*Ang, ' -------', Ang_, ' |'
      write(u,'(2A,3I10,A,I1,9A)')        M, '|---GridPoints ------', g%ng_all(1:3), ' - [x', g%ng(4),'] --   |'
    if( any( g%nproc(1:3) > 1 ) ) then
      write(u,'(2A,3I10,9A)')             M, '|---Domains ---------',                  g%nproc, ' ---------   |'
      ng(1:3) = g%ng_all(1:3)/real( g%nproc(1:3) )
      write(u,'(2A,3F10.1,A,I1,9A)')      M, '|---DomainGrid ------  ', ng(1:3),       ' [x', g%ng(4),'] --   |'
    endif ! nproc > 1
      write(u,'(2A,3F10.4,3A)')           M, '|---GridSpacing -----',            g%h*Ang, ' -------', Ang_, ' |'
      write(u,'(2A,F10.2,2A,F10.4,9A)')   M, '|---AverageCutoff ---',E*eV,eV_,' ----- ',h*Ang,' -------',Ang_,' |' ! show in output energy units
    if( present( totalcharge ) ) &
      write(u,'(2A,F16.4,9A)')            M, '|---TotalCharge -----              ',totalcharge, ' -------- e  |' ! electrons

  if( present( a ) ) then
    na = size(a,1)
    if( na > 0 ) then
      write(u,'(9A)') M, '|---------------------------------------------------------------|'
      write(u,'(9A)') M, '|---AtomicPositions----------------------------------------',Ang_,' |'
      write(u,'(9A)') M, '|--- #   Z  sym              x               y               z -|'
      write(u,'(9A)') M, '|---------------------------------------------------------------|'
      do ia = 1, na
      if( abs( a(ia)%s%iZ - a(ia)%s%Z0 ) < 1E-6 ) then
             write(u,'(2A,I6,I4,2A2,3F16.6,9A)',iostat=ios) &
                      M, '|', ia, a(ia)%s%iZ, '  ', a(ia)%s%sym, a(ia)%pos * Ang, ' |'
      else ; write(u,'(2A,I6,F8.3,3F16.6,9A)',iostat=ios) &
                      M, '|', ia, a(ia)%s%Z0                   , a(ia)%pos * Ang, ' |'
      endif ! iZ == Z
      enddo ! na
    else  ! na > 0
      write(u,'(9A)') M, '|---NoAtoms-----------------------------------------------------|'
    endif ! na > 0
  endif ! present a
      write(u,'(9A)') M, '+---------------------------------------------------------------+'
  endfunction ! display_sample



  status_t function display_forces( modul, unt, a, frcall ) result( ios )
  use type_atom, only: atom
  use unitsystem, only: eV, eV_, Ang, Ang_, operator(/)
!   use toolbox, only: operator(+)
  implicit none
    ! arguments
    character(len=*), intent(in)    :: modul !! symbol of calling module
    iounit_t, intent(in)            :: unt   !! write to this unit
    type(atom), intent(in)          :: a(:)  !! list of all atoms
    real, intent(in)                :: frcall(3) !! average forces
    ! local vars
    integer                         :: ia
    real                            :: eV_Ang
    character(len=4)                :: M

    if( unt <= 0 ) return

    M = '' ; M(1:3) = modul

    eV_Ang = eV/Ang ! get the scale factor ff and unit string fs for forces
!       eV_Ang_ = eV_+'/'+adjustl(Ang_) ! and the unit label ! cat is from module toolbox
!     write(unit=eV_Ang_,fmt='(9A)') trim(eV_),'/',trim(adjustl(Ang_)) ! the unit label
    write(unt,'(9A)',iostat=ios)
    write(unt,'(9A)') M, ' +-----------------------------------------------------+'
    write(unt,'(9A)') M, ' | atom#    |     F_x         F_y         F_z  ', eV_ / Ang_ , '|'
    write(unt,'(9A)') M, ' |-----------------------------------------------------|'
    do ia = 1, size(a)
      write(unt,'(2A,I6,A1,A2,A,3F12.6,A)',iostat=ios) M, ' |', a(ia)%ja, &
                  ' ', a(ia)%s%sym, ' | ',    (a(ia)%frc         )*eV_Ang, '     |'
    enddo ! ia
    write(unt,'(9A)') M, ' +-----------------------------------------------------+'
      write(unt,'(2A,I6,A,3F12.6,A)') M, & ! show average force
                         ' |', 0,'    | ',                  frcall*eV_Ang, '     |'
    write(unt,'(2A)') M, ' +-----------------------------------------------------+'
    write(unt,'(9A)')
  endfunction display_forces





  !! writes the energies and occupation numbers of all states, 
  !! i.e. for each kpoint, spin, band
  !!
  !! occupation numbers are not
  !! weighted with the <a href="http://www.real-space.de/wiki/doku.php?id=kpoints#weight">kpoint weight</a>
  status_t function display_states( modul, unt, nbands, nspins, nkpoints, kpoints, psi, iteration, &
                     dp, comm, write_eig_file, basename, max_states, FermiLevel ) result( ist )
  use type_state, only: state
  use type_kpoint, only: kpoint
  use MPItools, only: MPIallsum
  use unitsystem, only: eV, eV_!, Ang, Ang_
  use configuration, only: EiG_FileNameExtension, CodeName
  use constants, only: opt
    ! parameter
    character(len=*), parameter       :: fun = ' display_states: '
    integer, parameter                :: ROOT = 0
    integer, parameter                :: I_ENE = 1
    integer, parameter                :: I_OCC = 2
    iounit_t, parameter               :: eig = 89
    ! arguments
    character(len=*), intent(in)      :: modul !! symbol of calling module, not used
    iounit_t, intent(in)              :: unt   !! write to this unit
    integer, intent(in)               :: nbands, nspins, nkpoints
    type(kpoint), intent(in)          :: kpoints(:) ! k-point set (parallelized list)
    type(state), intent(in)           :: psi(:)     ! state information
    integer, intent(in)               :: iteration  ! scf iteration counter
    real, intent(in)                  :: dp         ! scf density residual
    MPI_Comm, intent(in)              :: comm       ! parallelization
!     logical, intent(in), optional     :: framed ! removed from interface
    logical, intent(in), optional          :: write_eig_file
    character(len=*), intent(in), optional :: basename
    integer, intent(in), optional          :: max_states
    real, intent(in), optional        :: FermiLevel
    ! local vars
    character(len=1)                  :: f, l
    character(len=2)                  :: ff
    character(len=4)                  :: M
    integer                           :: ib, is, ik, ibsk
    real, allocatable                 :: eo_all(:,:,:,:)
    real, allocatable                 :: kp_all(:,:)
    real                              :: t(6), eF ! Fermi level
    string_t                          :: filename, spinformat

    allocate( eo_all(nbands,nspins,nkpoints,I_ENE:I_OCC), kp_all(-1:3,nkpoints), stat=ist )
    eo_all = 0. ; kp_all = 0. ! init
    do ibsk = 1, size(psi)
      eo_all(psi(ibsk)%jbnd,psi(ibsk)%jspn,psi(ibsk)%jkpt,I_ENE:I_OCC) = (/ psi(ibsk)%ene, psi(ibsk)%occ /)
      ik = (ibsk-1)/(nbands*nspins) +1
      if( kp_all( -1,psi(ibsk)%jkpt) == 0. ) then
        kp_all( 0 ,psi(ibsk)%jkpt) = kpoints(ik)%w
        kp_all(1:3,psi(ibsk)%jkpt) = kpoints(ik)%k(1:3)
        kp_all( -1,psi(ibsk)%jkpt) = 1. ! checksum
      endif
    enddo ! ibsk
    call MPIallsum( eo_all, comm )
    call MPIallsum( kp_all, comm )

    if( unt <= 0 ) then
      ! processes for which unt is not positive will return here
      deallocate( eo_all, kp_all, stat=ist )
      return
    endif ! unt <= 0

!   if( size(psi) > 0 .and. any( kp_all(-1,:) /= 1. ) ) stop 'kp_gathering failed!'
    ! after reduction all kp_all(-1,:) will be equal to the number of task that parallelize bands (>= 1, integer)

    eF = opt( FermiLevel, def=0. ) ! get the Fermi level

    if( opt( write_eig_file, def=.false. ) ) then
      write(unit=filename,fmt='(9A)',iostat=ist) trim( opt( basename, def=CodeName ) ), EiG_FileNameExtension
      open(unit=eig,file=filename,action='write',iostat=ist)
      if( ist == 0 ) then
        write(eig,'(9A)',iostat=ist) '# ',trim(CodeName),' Kohn-Sham eigenvalues of project "',trim(basename),'"'
        write(eig,'(9A)',iostat=ist) '# energies given in', eV_
        write(eig,'(9(A,F0.9))',iostat=ist) '# energies are aligned to the internal Fermi level at ',eF*eV, eV_
        write(eig,'(9(A,I0))',iostat=ist) '# ',nkpoints,' k-points, ',nbands,' bands and ',nspins,' spins'
        write(eig,'(9(A,I0))',iostat=ist) '# line format:'
        if( nspins > 1 ) then
          write(eig,'(9(A,I0))',iostat=ist) '#       E_dn-E_F        E_up-E_F  occ_dn  occ_up  band_index'
          spinformat = '(2F16.9,2F8.3,A,I0)'
        else  ! spin
          write(eig,'(9(A,I0))',iostat=ist) '#          E-E_F     occ  band_index'
          spinformat = '(1F16.9,1F8.3,A,I0)'
        endif ! spin
        do ik = 1, nkpoints
          write(eig,'(/,A,3(" ",F0.6),A,F0.6,A,I0)',iostat=ist) '# k-point=', kp_all(1:3,ik), '  k-weight= ', kp_all(0,ik), '  k-index= ',ik
          do ib = 1, nbands
            write(unit=eig,fmt=spinformat,iostat=ist) ( eo_all(ib,:,ik,I_ENE)-eF )*eV, eo_all(ib,:,ik,I_OCC), '  ', ib
          enddo ! ib
        enddo ! ik
        write(eig,'(A)',iostat=ist) '', '#' ! blank line and tailing comment line
        close(unit=eig,iostat=ist)
      else  ! opening
        write(unt,'(99A)') sym, fun, 'failed to open file "', trim(filename), '" for writing, skip.'
      endif ! opening
    endif ! write_eig_file

    if( nbands*nkpoints > opt(max_states, def=999) ) return ! do not show if more than max_states

    M = '' ; M(1:3) = modul

    write(unt,'(99A)') M
    write(unt,'(99A)') M,'   Kohn-Sham orbitals:'
    write(unt,'(99A)') M,'  -------------------------------------------------------'
    write(unt,'(99A)') M,'     #k  #s     #b   KS-eigenvalue       ',eV_,'      occup'
    write(unt,'(99A)') M,'  ====== === ====== ========================= ==========='
    do ik = 1, nkpoints
      do is = 1, nspins
        do ib = 1, nbands
          write(unt,'(A,I7,I4,I7,F26.16,F12.6)') M, ik,is,ib, eo_all(ib,is,ik,I_ENE)*eV, eo_all(ib,is,ik,I_OCC)
        enddo ! ib
        if( is < nspins ) write ( unt, '(99A)') M,'  ---------------------------------------------------'
      enddo ! is
      if( ik < nkpoints ) write ( unt, '(99A)') M,'  ---------------------------------------------------'
    enddo ! ik
    write(unt,'(99A)') M
    write(unt,'(2A,I5,9(A,ES14.6E2))') M,'   SC it#', iteration, '  density change:', dp, ' au'
    write(unt,'(99A)') M
    deallocate( eo_all, kp_all, stat=ist )

  endfunction ! display_states



  !! shows an ASCII image of the atomic geometry in the rectangular cell
  !!>                                                         z     
  !!                 -----------------------------------------       
  !!               /|                                        /|      
  !!              / |                                       / |      
  !!             /  |                                      /  |      
  !!            /   |                   /                 /   |      
  !!           /    |              (H) / (H)             /    |      
  !!          /      --------------(C)/(C)--------------/----- y     
  !!         /     /        (H)C)(C) / (C)(C(H)        /     /       
  !!        /     /   (H)   (C)    (N)    (C)   (H)   /     /        
  !!       /   --------(C)---(N)--(Fe)-(N)---(C)-----/--   /         
  !!      /     /   (H)   (C)    (N)    (C)   (H)   /     /          
  !!     /     /        (H(C)(C) / (C)(C)H)        /     /           
  !!     --------------------(C)-(C)--------------      /            
  !!    |    /             (H) / (H)              |    /             
  !!    |   /                 /                   |   /              
  !!    |  /                                      |  /               
  !!    | /                                       | /                
  !!    |/                                        |/                 
  !!     -----------------------------------------                   
  !!<                                              x                 
  !! like this setup of <a href="http://www.real-space.de/wiki/doku.php?id=porphirine" >Porhporine<a/> (37 atoms)
  status_t function display_cell_ascii( modul, unt, atm, s, noframe ) result( ios )
  use constants, only: PSE
  use type_atom, only: atom
  use sorting, only: permutation_of
  implicit none
    ! parameters
    real, parameter  :: YXRATIO = 0.3 !! perspective aspect ratio of ASCII characters (width/height)
    integer, parameter :: WIDTH = 65 !! width of the screen for ASCII display
    ! arguments
    character(len=*), intent(in)      :: modul !! symbol of calling module
    iounit_t, intent(in)              :: unt   !! write to this unit
    type(atom), intent(in)            :: atm(:) !! list of all atoms (only atm%relpos is used)
    real, intent(in)                  :: s(1:3) !! rectangular size 
    logical, intent(in), optional     :: noframe !! default is .FALSE.

    ! local vars
    character(len=4)                  :: M
    character, allocatable            :: c(:,:)
    ! the full with is used (no borders) if useratio = 1.0
    real, parameter                   :: useratio = 0.85
    ! the aspect ration of an ascii char
    real, parameter                   :: width_height = 0.48 ! 0.6146
    integer                           :: nx, ny, nz, nwbord, nhbord, nh
    real                              :: xrat

    real                              :: mvals(3,2)

    real                              :: dn
    integer                           :: i, ia, na, is, i1, i2, ii
    integer                           :: iorder(size(atm,1))
    real                              :: dist(size(atm,1))
    integer                           :: iorig(2)
    character(len=24)                 :: frmt
    character(len=2)                  :: ESym
    ! coordinate system
    character, parameter              :: xLine = '-'
    character, parameter              :: yLine = '/'
    character, parameter              :: zLine = '|'
    character, parameter              :: Origin = 'x' ! ' '
    ! frame characters
    character, parameter              :: lvframe = ' '!'<' ! ' '
    character, parameter              :: rvframe = ' ' !'>' ! ' '
    character, parameter              :: hframe  = ' ' ! ' '
    character, parameter              :: cframe  = ' ' ! corner
!     character, parameter              :: lvframe = '['!'<' ! ' '
!     character, parameter              :: rvframe = ']' !'>' ! ' '
!     character, parameter              :: hframe  = '=' ! ' '
!     character, parameter              :: cframe  = '@' ! corner

    real                              :: camera(3)
    logical, parameter                :: centercross = .true.
    logical                           :: framed = .true.
    logical, parameter                :: capitallabel = .false.
    logical, parameter                :: parenthesis = .true.
    character(len=1)                  :: label(3)

    ios = 1
    if( unt <= 0 ) return ! do not write to unit 0

    if( present( noframe ) ) framed = .not. noframe

    M = '' ; M(1:3) = modul

    xrat = useratio*0.5*( s(1) )/( s(1) + s(2)*YXRATIO )
    nx = nint( xrat*WIDTH )
    ny = nint( nx*yxratio*s(2)/s(1) )
    nz = nint( nx*width_height*s(3)/s(1) )

    nwbord = nint( 0.5*( WIDTH - ( 2*nx+1 + 2*ny ) ) )
    nhbord = nint( nwbord*width_height )
    nh = ( 2*nz+1 + 2*ny ) + 2*nhbord

#ifdef DEBUG
    if(o>0) write(o,'(9(A,I0))') 'DSP display_cell_ascii: horizontal border ',nwbord,', vertical border ',nhbord,', ',nh,' lines'
    if(o>0) write(o,'(9(A,I0))') 'DSP display_cell_ascii: nx = ',nx,', ny = ',ny,', nz =',nz
#endif

    camera = (/ 1., (nx/s(1))/(ny/s(2)), -1. /)
    dn = dot_product( camera, camera )
    camera = camera/sqrt(dn) ! normalized

    na = size(atm,1)

    ! order atoms by their distance towards the camera
    do ia= 1, na

      if( any( abs(atm(ia)%relpos) > 0.5 ) ) then
        if(o>0) write(o,'(A,I4,9(A,3F10.3))') 'DSP display_cell_ascii: ia =', ia, ' pos=', atm(ia)%pos, ' box=', s
        do i = 1, 3
          mvals(i,1) = minval( atm(:)%pos(i) )
          mvals(i,2) = maxval( atm(:)%pos(i) )
        enddo ! i
        if(o>0) write(o,'(A,9(3F7.1,A))') 'DSP display_cell_ascii: minvals: ', &
          mvals(:,1), ' maxvals: ', mvals(:,2), ' minbox: ', mvals(:,2) - mvals(:,1)
!         stop 'display_cell_ascii: atomic positions out of bounding box.'
        return
      endif ! any

      dist(ia) = dot_product( atm(ia)%pos(1:3), camera )

    enddo ! ia2

    iorder = permutation_of( na, dist ) ! sort the atoms by distance (ascending)

    iorig(1) = nwbord + nx + ny
    iorig(2) = nhbord + nz + ny + 1

    allocate( c(WIDTH,nh), stat=ios ) ! get memory for the canvas
    c(:,:) = ' ' ! clear the canvas

    ! plot back frame
    do i=-(nx-1),(nx-1)
      c(iorig(1)+i+ny,iorig(2)-nz-ny) = xLine ! upper back
      c(iorig(1)+i+ny,iorig(2)+nz-ny) = xLine ! lower back
      ! c(iorig(1)+i-ny,iorig(2)-nz+ny) = xLine ! upper front
      c(iorig(1)+i-ny,iorig(2)+nz+ny) = xLine ! lower front
    enddo ! i

    if(centercross) c(iorig(1)+1:iorig(1)+(nx-1),iorig(2)) = xLine ! centercross

    do i=-(ny-1),(ny-1)
      c(iorig(1)+nx-i,iorig(2)+nz+i) = yLine ! lower right
      c(iorig(1)-nx-i,iorig(2)+nz+i) = yLine ! lower left
      c(iorig(1)+nx-i,iorig(2)-nz+i) = yLine ! upper right
      c(iorig(1)-nx-i,iorig(2)-nz+i) = yLine ! upper left
    enddo ! i

    if(centercross) then
      do i=-(ny-1),(ny-1)
        c(iorig(1)-i,iorig(2)+i) = yLine ! center
      enddo ! i
    endif ! centercross

    do i=-(nz-1),(nz-1)
      c(iorig(1)-nx+ny,iorig(2)+i-ny) = zLine ! back left
      c(iorig(1)+nx+ny,iorig(2)+i-ny) = zLine ! back right
      c(iorig(1)-nx-ny,iorig(2)+i+ny) = zLine ! front left
      c(iorig(1)+nx-ny,iorig(2)+i+ny) = zLine ! front right
    enddo ! i

    if(centercross) then
      do i=-(nz-1),(nz-1)
        c(iorig(1),iorig(2)+i) = zLine ! center
      enddo ! i
    endif ! centercross

    do i=-(nx-1),(nx-1)
      c(iorig(1)+i-ny,iorig(2)-nz+ny) = xLine ! upper front
    enddo ! i

    if(centercross) c(iorig(1)-(nx-1):iorig(1)-1,iorig(2)) = xLine ! centercross
    c(iorig(1),iorig(2)) = Origin ! centercross

    label = achar(119+(/1,2,3/))
    if( capitallabel ) label = achar( 87+(/1,2,3/))

    c(iorig(1)+nx-ny+1,iorig(2)+nz+ny+1) = label(1)
    c(iorig(1)+nx+ny+1,iorig(2)+nz-ny+0) = label(2)
    c(iorig(1)+nx+ny+1,iorig(2)-nz-ny-1) = label(3)

    do ii = na, 1, -1 ! reversed
      ia = iorder(ii) ! get the atoms ordered such that the largest distance from the camera is drawn first

      ! projection onto 2dim coordinates
      i1 = iorig(1) + nint( (atm(ia)%relpos(1)+0.5)*2*nx ) - nx &
                    + nint( (atm(ia)%relpos(2)+0.5)*2*ny ) - ny
      i2 = iorig(2) - nint( (atm(ia)%relpos(3)+0.5)*2*nz ) + nz &
                    - nint( (atm(ia)%relpos(2)+0.5)*2*ny ) + ny
      ESym = atm(ia)%s%sym
      is = 0
      c( i1, i2 ) = ESym(1:1)
      if( Esym(2:2) /= ' ' ) then
        c( i1+1, i2 ) = ESym(2:2)
        is = 1
      endif

      if( parenthesis ) then
        c( i1-1,    i2 ) = '('
        c( i1+is+1, i2 ) = ')'
      endif ! parenthesis

    enddo ! ii

    if( framed ) then
      c(2:WIDTH-1, 1) = hframe ! upper horizontal frame
      c(2:WIDTH-1,nh) = hframe ! lower horizontal frame
      c(    1, 1) = cframe ; c(WIDTH, 1) = cframe ! corners
      c(WIDTH,nh) = cframe ; c(    1,nh) = cframe ! corners
      c(    1,2:nh-1) = lvframe !  left vertical frame
      c(WIDTH,2:nh-1) = rvframe ! right vertical frame
    endif ! framed

    ! create the format string
    write(unit=frmt,fmt='(9(A,I0))') '(A4,',WIDTH,'A1)' ! e.g. '(A4,65A1)'
    ! write drawing to unit unt
    do i=1, nh
      write(unit=unt,fmt=frmt,iostat=ios) M, c(:,i)
    enddo
    deallocate( c, stat=ios )
  endfunction ! display_cell_ascii



!! displays a nice overview of how many processes are used 
!! (in real runs or <a href="http://www.real-space.de/wiki/doku.php?id=checkmode">CheckMode</a>)
!!>                      [       ----- -----     ]
!!                       [     /     /     /|    ]
!!                       [     ----- -----  |    ]
!!                       [    |     |     |      ]
!!                       [    |     |     |/     ]
!!<                      [     ----- -----       ]
  status_t function display_decomp_ascii( modul, unt, nproc ) result( ios )
  implicit none
    ! parameters
    integer, parameter :: WIDTH = 65 !! width of the screen for ASCII display
    character, parameter              :: xLine = '-'
    character, parameter              :: yLine = '/'
    character, parameter              :: zLine = '|'
    character, parameter              :: lvframe = '['!'<' ! ' '   ! left edge
    character, parameter              :: rvframe = ']' !'>' ! ' '  ! right edge
    character, parameter              :: hframe = '=' ! ' '        ! horizontal line
    character, parameter              :: cframe = '@'              ! corner
    integer, parameter                :: wdh_large(3) = (/5,1,2/)
!     integer, parameter                :: wdh_large(3) = (/7,1,3/) ! a cube is:
! !                       [        -------     ]          ! 8 in width
! !                       [      /       /|    ]          ! 2 in depth
! !                       [      -------  |    ]          ! 4 in height
! !                       [     |       | |    ]
! !                       [     |       |      ]
! !                       [     |       |/     ]
! !                       [      -------       ]
    ! arguments
    character(len=*), intent(in)      :: modul !! symbol of calling module
    iounit_t, intent(in)              :: unt   !! write to this unit
    integer, intent(in)               :: nproc(1:3)
    ! local vars
    character(len=4)                  :: M
    character, allocatable            :: c(:,:)
    string_t                          :: frmt
    integer                           :: toolarge, mult, nall(3), n(3), nh, nw, nwspace(2)
    integer                           :: iorig(2), i, j, k, ist, wdh(3)

    ios = 1
    M = '' ; M(1:3) = modul

    mult = 1

    wdh = wdh_large
    toolarge = 1

    k = 0
    do while( toolarge /= 0 )

      n = mult*wdh+1
      nall = nproc*n

      nh = 4 + 1  & ! 2 upper and 2 lower frame + 1 line
        + nall(3) &  ! z
        + nall(2)    ! y
      nw = 4 + 1  & ! 2 left and 2 right frame + 1 line
        + nall(1) &  ! x
        + nall(2)    ! y

      if( nw < Width - 4 ) then
        toolarge = 0 ! exit the loop
      else
        toolarge = 1
        wdh = max( wdh-1, 1)
      endif

      k = k+1
      if( k >= WIDTH ) then
        ! cannot display
        if(unt/=0) write(unt,'(2A,I0,A,3I6)') 'DSP display_decomp_ascii: ', &
          'too many procs to display within ', WIDTH, ' chars, nproc =', nproc
        return
      endif !

    enddo ! while

    nwspace(1) = int( 0.5*(WIDTH-nw) ) ; nwspace(2) = WIDTH - nw - nwspace(1)

    allocate( c(WIDTH,nh), stat=ios )

    c(:,:) = ' ' ! set clear

    iorig(1:2) = (/ 2+nwspace(1)+nall(1)+1, 2+nall(2)+1 /)

    do i=0, nproc(2) ; do j=0, nproc(1)-1 ;  do k=1, n(1)-1
      c(iorig(1)-nall(1)+j*n(1)+k+n(2)*i,iorig(2)-n(2)*i) = xLine !
    enddo ; enddo ; enddo ! kji

    do i=1, nproc(3) ; do j=0, nproc(1)-1 ; do k=1, n(1)-1
      c(iorig(1)-nall(1)+j*n(1)+k,iorig(2)+n(3)*i) = xLine !
    enddo ; enddo ; enddo ! kji

    do i=0, nproc(1) ; do j=0, nproc(3)-1 ; do k=1, n(3)-1
      c(iorig(1)-nall(1)+n(1)*i,iorig(2)+j*n(3)+k) = zLine !
    enddo ; enddo ; enddo ! kji

    do i=1, nproc(2) ; do j=0, nproc(3)-1 ; do k=1, n(3)-1
      c(iorig(1)+n(2)*i,iorig(2)+j*n(3)+k-i*n(2)) = zLine !
    enddo ; enddo ; enddo ! kji

    do i=0, nproc(1) ; do j=0, nproc(2)-1 ; do k=1, n(2)-1
      c(iorig(1)-nall(1)+i*n(1)+j*n(2)+k,iorig(2)-j*n(2)-k) = yLine !
    enddo ; enddo ; enddo ! kji

    do i=1, nproc(3) ; do j=0, nproc(2)-1 ; do k=1, n(2)-1
      c(iorig(1)+j*n(2)+k,iorig(2)+i*n(3)-j*n(2)-k) = yLine !
    enddo ; enddo ; enddo ! kji

    ! MAKE THE FRAME AROUND IT

  !   if( framed ) then
      c(2:WIDTH-1,1) = hframe
      c(1,1) = cframe ; c(WIDTH,1) = cframe
      c(WIDTH,nh) = cframe ; c(1,nh) = cframe
      c(2:WIDTH-1,nh) = hframe
      c(1,2:nh-1) = lvframe
      c(WIDTH,2:nh-1) = rvframe
  !   endif ! framed

    ! generate format descriptor
    write(unit=frmt,fmt='(9(A,I0))',iostat=ios) '(A4,',WIDTH,'A1)' ! e.g. '(A4,65A1)'
    do i=1, nh
      write(unit=unt,fmt=frmt,iostat=ios) M, c(:,i)
    enddo
    deallocate( c, stat=ios )
  endfunction ! display_decomp_ascii



#ifdef EXTENDED
!+ extended

  !! write a nice file "DD.fig" for xfig viewing
  status_t function display_decomp_xfig( name, n, cell ) result( ios )
  implicit none
    ! parameters
    character, parameter      :: SEP(3) = (/'.','x','x'/)
    iounit_t, parameter       :: u = 98
    integer, parameter        :: X=1, Y=2, Z=3
    integer, parameter        :: CC = 32 ! 32: custom colour
    real, parameter           :: BX = 1200. ! each box length
    ! arguments
    character(len=*), intent(in)    :: name ! project name
    integer, intent(in)             :: n(1:3) ! number of process
    real, intent(in), optional      :: cell(1:3)
    ! local vars
    real      :: v3(3), v2s(2), v2e(2), s(3) ! scale factor
    real      :: m(3,X:Y) ! projection matrix
    integer   :: i, i12, f, d1, d2, lw=12 ! line width
    string_t  :: fname

    ! generate filename
    write(unit=fname,fmt='(A,9(A,I0))',iostat=ios) trim(adjustl(name)), ( SEP(i), n(i), i=1,3 ), '.fig' ! append '_N1xN2xN3.fig' to the project name

    open(unit=u,file=fname,action='write',status='unknown',iostat=ios)
    if( ios /= 0 ) then
      write(*,'(9A)') 'DSP display_decomp_xfig: cannot open file "',trim(fname),'". return!'
      return ! cannot open file
    endif ! failed

    s = 1. ; if( present( cell ) ) s = max( 1E-3, abs(cell) )
    ! pseudo perspective view
    m(:,X) = (/1.,0.,-.25/)
    m(:,Y) = (/0.,1.,-.25/)

!     write(u,'(9(A,I3))') '#DomainDecomposition for', n(1), ' x', n(2), ' x', n(3)
    write(unit=u,fmt='(A)',iostat=ios) '#FIG 3.2','Landscape','Center','Inches','Letter  ','100.00','Single','-2','1200 2','0 32 #232311'

    ! for the 3 visible faces of the cube
    do f = 1, 3
      selectcase( f )
      case( 1 ) ; d1 = 3 ; d2 = 2 ; lw =  6 ! left
      case( 2 ) ; d1 = 1 ; d2 = 3 ; lw =  4 ! up ! less line width appears brighter
      case( 3 ) ; d1 = 2 ; d2 = 1 ; lw = 12 ! front
      endselect ! f

      do i12 = 1, 2
        do i = 0, n(d1)
          v3 = 0. ! init 3dim vector
          v3(d1) =     i * BX * s(d1)
          v3(d2) =     0 * BX * s(d2)
          v2s = matmul( v3, m ) ! 2dim start position
          v3 = 0. ! init 3dim vector
          v3(d1) =     i * BX * s(d1)
          v3(d2) = n(d2) * BX * s(d2)
          v2e = matmul( v3, m ) ! 2dim end position
          !! draw an XFIG line
          ! 2 1 0 LW CL 7 50 -1 -1 0.000 0 1 -1 0 0 2
          !        xstart ystart, xend yend
          ! 2 1 0 3 32 7 50 -1 -1 0.000 0 1 -1 0 0 2
          !        5550 1650 5550 4050
          write(unit=u,fmt='(9(A,I0))',iostat=ios) '2 1 0 ',LW,' ',CC,' 7 50 -1 -1 0.000 0 1 -1 0 0 2'
          write(unit=u,fmt='(A,4I8)',iostat=ios)  '        ', nint(v2s(X:Y)), nint(v2e(X:Y))
        enddo ! i
        ! swap d1 and d2
        i = d1 ; d1 = d2 ; d2 = i
      enddo ! i12

    enddo ! faces
    close(unit=u,iostat=ios)
  endfunction ! display_decomp_xfig


  status_t function display_stopwatch( modul, unt, nsec_in ) result( ios )
  implicit none
    ! args
    character(len=*), intent(in)    :: modul !! symbol of calling module
    iounit_t, intent(in)            :: unt   !! write to this unit
    integer, intent(in)             :: nsec_in
    ! local vars
    integer                         :: nsec, nmin, nhrs, ndys
    real                            :: rsec, rmin, rhrs, rdys
    character(len=4)                :: M

    ios = 1 ; if( unt < 1 ) return

    nsec = 0
    nmin = 0
    nhrs = 0
    ndys = 0

    nsec = nsec_in
    rsec = real(nsec)

    if( rsec >= 86400. ) then
      rdys = rsec/86400.
      ndys = int(rdys)
      rsec = rsec - ndys*86400.
    endif

    if( rsec >= 3600. ) then
      rhrs = rsec/3600.
      nhrs = int(rhrs)
      rsec = rsec - nhrs*3600.
    endif

    rmin = rsec/60.
    nmin = int(rmin)
    rsec = rsec - nmin*60.

    nsec = int(rsec)

    M = '' ; M(1:3) = modul

    write( unt, '(A4,A)'  )     M, '+---------------------------------------------------------------+'
    write( unt, '(A4,A,I12,A)') M, '|---------- Total time ------         ',nsec_in, ' sec        --|'
  if( nsec < 0 ) then
    write( unt, '(A4,A)')     sym, '|---------- Error in stopwatch( ), needs positive time! --------|'
  elseif( ndys > 0 ) then
    write( unt, '(A4,A,I4,A,I3,A,I3,A,I3,A)'  )  M, '|---------------------------- =', &
             ndys, ' d', nhrs, ' h', nmin,  ' m', nsec, ' sec        --|'
  elseif( nhrs > 0 ) then
    write( unt, '(A4,A,I3,A,I3,A,I3,A)'  ) M, '|---------------------------- =      ', &
                         nhrs, ' h', nmin,  ' m', nsec, ' sec        --|'
  elseif( nmin > 0 ) then
    write( unt, '(A4,A,I3,A,I3,A)'  ) M, '|---------------------------- =           ', &
                                     nmin,  ' m', nsec, ' sec        --|'
  else
    if( .false. ) &
    write( unt, '(A4,A,I3,A)'  ) M, '|---------------------------- =                ', &
                                                  nsec, ' sec        --|'
  endif
    write( unt, '(A4,A)'  ) M, '+---------------------------------------------------------------+'

  endfunction display_stopwatch

    !! PSE .---------------------------------------------------------------.
    !! PSE :                                                               :
    !! PSE :    |H |                                               |He|    :
    !! PSE :    |Li|Be|                             |B |C |N |O |F |Ne|    :
    !! PSE :    |Na|Mg|                             |Al|Si|P |S |Cl|Ar|    :
    !! PSE :    |K |Ca|Sc|Ti|V |Cr|Mn|Fe|Co|Ni|Cu|Zn|Ga|Ge|As|Se|Br|Kr|    :
    !! PSE :    |Rb|Sr|Y |Zr|Nb|Mo|Tc|Ru|Rh|Pd|Ag|Cd|In|Sn|Sb|Te|I |Xe|    :
    !! PSE :    |Cs|Ba|La|Hf|Ta|W |Re|Os|Ir|Pt|Au|Hg|Tl|Pb|Bi|Po|At|Rn|    :
    !! PSE :    |Fr|Ra|Ac|Rf|Db|Sg|Bh|Hs|Mt|++|                            :
    !! PSE :        (4f) |Ce|Pr|Nd|Pm|Sm|Eu|Gd|Tb|Dy|Ho|Er|Tm|Yb|Lu|       :
    !! PSE :        (5f) |Th|Pa|U |Np|Pu|Am|Cm|Bk|Cf|Es|Fm|Md|No|Lr|       :
    !! PSE :                                                               :
    !! PSE '---------------------------------------------------------------'


  status_t function display_atoms_xfig( filename, atom, bond, aw8s, bw8s, AtomRadiusScale ) result( ios )
  use constants, only: Pi
  implicit none
    ! parameters
    character(len=*), parameter     :: fun = ' a2x: '
    iounit_t, parameter             :: u = 17
    ! arguments
    character(len=*), intent(in)    :: filename ! write to this file
    real, intent(in)                :: atom(0:,1:) ! (0:3,1:na), [0:3]=[Z,x,y,z] (Z should be integer)
    integer, intent(in)             :: bond(1:,1:) ! (1:2,1:nb) ! from atom#bond(1,:) to atom#bond(2,:)
    real, intent(in), optional      :: aw8s(0:) ! (1:na), atomic weights
    real, intent(in), optional      :: bw8s(0:) ! (1:nb), bond weights
    real, intent(in), optional      :: AtomRadiusScale

    ! configuration
    integer, parameter :: MSCALE = 120000 ! 100 inch
    integer, parameter :: AtomLineThickness = 3
    integer, parameter :: AtomTypeTable(0:6) = (/0,32,51,52,6,1,16/)
    real, parameter    :: AtomRadiusTable(0:6) = (/0.5,1.8,2.5,2.2,1.1,0.9,1.5/)
    integer, parameter :: AtomColorTable(0:6) = (/32,33,34,35,36,37,38/)
    real, parameter    :: BondRadiusTable(0:0) = (/0.05/)
    integer, parameter :: ColorTable(3,0:7) = reshape( (/ &
    127,127,127, & ! 32 ! grey
  !!! nice colors orange, grass-green, navy-blue
    255,127, 26, & ! 33 ! Ge
    178,255, 26, & ! 34 ! Sb
    102,102,255, & ! 35 ! Te
  ! 0.4,0.4,0.92
  ! 0.08,0.08,0.63
  ! 0.9,0.83,0.83
  !!! nice colors but not very much contrast
  !    102,102,235, & ! 33 ! Ge
  !     20, 20,161, & ! 34 ! Sb
  !    230,212,212, & ! 35 ! Te
      255,255,  0, & ! 36 C yellow
      0,255,255, & ! 37 H aqua/cyan
      128,128,  0, & ! 38 S olive
      0,  0,  0  & ! black
      /), (/3,8/) )

    integer :: EightColorTable(0:6,0:8)
    integer :: BondColorTable(0:16)
    real    :: AtomRadiusMult = 1.0

!     integer :: icoords(0:2,0:3)
!     integer :: iedge(3), i8e(0:3), i, id
!     real :: perspective(3,2), cube_pos(3), cube_size(3), cube_distance(3), rot(3,3), phi_theta_psi(3), dsth, dstv
    real :: pos(3), center_of_mass(0:3), projection(3,0:2)
    real :: camera_pos(3), camera_lookat(3), dhv(0:2), hv(1:2)
    real :: dst, fac, bnd(3), s
    real :: min_depth, max_depth, depth, mass
    real :: aw8_min=1., aw8_max=1., bw8_min=1., bw8_max=1.
    integer :: ipen_color, ifill_color, iw8, idepth, irad
    integer :: ia, na, ib, nb, ii, ic, k, iab(2)
    integer, allocatable :: iZa(:), itypa(:), iposa(:,:), iposb(:,:,:)
    real, allocatable :: deptha(:), depthb(:,:), aw8(:), bw8(:)

    open(unit=u,file=filename,action='write',iostat=ios) ! open the file
    if( ios /= 0 ) stop 'xfig: cannot open file' ! warn

    ! start writing the fig file
    write(u,'(A)') '#FIG 3.2', 'Landscape', 'Center', 'Inches', 'Letter', '100.00', 'Single', '-2', '1200 2'
    ic = 32 ! 32 defined colors
    ! now user defined colors
    do ii = lbound(ColorTable,2), ubound(ColorTable,2)
      write(u,'(A,I0,A,3Z2.2)') '0 ', ic, ' #', int( ColorTable(:,ii) )
      ic = ic+1
    enddo ! ii

    do k = 1, 6 ! for the atom type Ge, Sb, Te, C
      do ii = 1, 8
        write(u,'(A,I0,A,3Z2.2)') '0 ', ic, ' #', int( ColorTable(:,k) * ii/8. )
        EightColorTable(k,ii) = ic
        ic = ic+1
      enddo ! ii
    enddo ! k

    do ii = lbound(BondColorTable,1), ubound(BondColorTable,1)
      write(u,'(A,I0,A,3Z2.2)') '0 ', ic, ' #', ii*(/15,15,15/)
      BondColorTable(ii) = ic
      ic = ic+1
    enddo ! ii


    na = size(atom,2)
    if( na < 1 ) return
    nb = size(bond,2)

    if( any(bond < 1 .or. bond > na) ) stop 'xfig: bonds illegal, bond indices may not exceed Natoms!'

    allocate( iZa(na), itypa(na), deptha(na), iposa(0:2,na), aw8(na), &
              iposb(0:2,1:2,nb), depthb(1:2,nb), bw8(nb), stat=ios )
    if( ios /= 0 ) return ! error

    aw8 = 1.
    if( present( aw8s ) ) then
      if( size(aw8s) < na ) stop 'not enough atomic weights passed!'
      aw8 = max(0.,aw8s)
      aw8_min = minval( aw8 )
      aw8_max = maxval( aw8 )
      if(o>0) write(o,'(3A,2F10.6,9A)') sym, fun, 'map atomic weights [', aw8_min, aw8_max, ' ] --> [0,8]'
    endif
    bw8 = 1.
    if( present( bw8s ) ) then
      if( size(bw8s) < nb ) stop 'not enough bond weights passed!'
      bw8 = max(0.,bw8s)
      bw8_min = minval( bw8 )
      bw8_max = maxval( bw8 )
      if(o>0) write(o,'(3A,2F10.6,9A)') sym, fun, 'map bond weights [', bw8_min, bw8_max, ' ] --> [0,16]'
    endif

    itypa = 0 ! init internal atom type numbers
    center_of_mass = 0. ! init

    AtomRadiusMult = 1.0 ; if( present( AtomRadiusScale ) ) AtomRadiusMult = max( 0., AtomRadiusScale ) 

    do ia = 1, na ! parse atoms
      iZa(ia) = nint( atom(0,ia) ) ! atomic number

      do k = lbound(AtomTypeTable,1), ubound(AtomTypeTable,1)
        if( iZa(ia) == AtomTypeTable(k) ) itypa(ia) = k
      enddo ! k
!       ! special case for GeSbTe
!       selectcase( iZa(ia) )
!       case( 32 ) ; itypa(ia) = 1 ! Ge
!       case( 51 ) ; itypa(ia) = 2 ! Sb
!       case( 52 ) ; itypa(ia) = 3 ! Te
!       endselect ! iZa

      pos = atom(1:3,ia)
      mass = iZa(ia) * 1822.888479031408 * 2 ! approximate mass, wrong for H and for Z >> 20
      center_of_mass(1:3) = center_of_mass(1:3) + mass * pos
      center_of_mass( 0 ) = center_of_mass( 0 ) + mass
    enddo ! ia
    if( center_of_mass(0) > 0. ) center_of_mass(1:3) = center_of_mass(1:3)/center_of_mass(0)
    center_of_mass(0) = center_of_mass(0)/na ! average mass (or average Z)

    camera_lookat = center_of_mass(1:3) ! center of mass of all atoms

    ! specifiy the camera position
!     camera_pos = center_of_mass(1:3) - 250.*(/1,0,0/) ! look straight into the (100)-direction
!     camera_pos = center_of_mass(1:3) - 250.*(/1,1,1/)/sqrt(3.) ! look straight into the (111)-direction
!     camera_pos = center_of_mass(1:3) - 250.*(/5,4,3/)/sqrt(50.) ! look into the (543)-direction
    camera_pos = center_of_mass(1:3) - 250.*(/0.0001,0.0001,1./) ! look straight into the (100)-direction

    pos = camera_lookat - camera_pos ! view vector
    projection(:,0) = pos/sqrt(sum(pos**2)) ! normalized depth vector

    ! now find two vectors that are orthogonal to projection(:,0) and mutually
    pos = cross_product( projection(:,0), (/0.,0.,1./) ) ! z-direction
    projection(:,1) = pos/sqrt(sum(pos**2)) ! normalized horizontal-vector

    pos = cross_product( projection(:,0), projection(:,1) )
    projection(:,2) = pos/sqrt(sum(pos**2)) ! normalized vertical-vector

!     ! check if the projection matrix is normalized properly
!     write(*,'(3F10.6)') matmul(projection,transpose(projection)) ! should be unity
!     write(*,'(3F10.6)') matmul(transpose(projection),projection) ! should also be unity

!     ! check if pos rotated is the (0,0,1)-vector
!     pos = camera_lookat - camera_pos
!     dhv = matmul( pos, projection )
!     write(*,'(3A,9F10.6)') sym, fun, 'dhv=', dhv

    do ia = 1, na ! parse atoms
      pos = atom(1:3,ia) - camera_pos

      dhv = matmul( pos, projection ) ! rotation
      deptha(ia) = dhv(0) ! store depth
      hv(1:2) = dhv(1:2)/dhv(0) ! perspective projection
      ! integer coordinates
      iposa(1:2,ia) = nint( MSCALE * hv )*(/1,-1/) ! in xfig, the vertical coordinate is pointing downwards
    enddo ! ia


    do ib = 1, nb ; iab = bond(:,ib)

      bnd(1:) = atom(1:3,iab(2))-atom(1:3,iab(1)) ! ia1 --> ia2
      dst = sqrt( sum(bnd**2) ) ! bond length
      if( dst < 0.001 ) stop 'xfig: too short bond!'
      bnd = bnd/max(0.01,dst) ! normalized bond direction vector

      do k = 1, 2
!         s = 2.*k-3. ! maps [1,2] --> [-1.,+1.] ! ia2 --> ia1
        s = 3.-2.*k ! maps [1,2] --> [+1.,-1.] ! ia1 --> ia2
        pos = atom(1:3,iab(k)) + s * bnd(1:3) * AtomRadiusTable( itypa(iab(k)) ) - camera_pos(1:3)

        dhv = matmul( pos, projection ) ! rotation
        depthb(k,ib) = dhv(0) ! store depth
        hv(1:2) = dhv(1:2)/dhv(0) ! perspective projection
        ! integer coordinates
        iposb(1:2,k,ib) = nint( MSCALE * hv )*(/1,-1/) ! in xfig, the vertical coordinate is pointing downwards
      enddo ! k

    enddo ! ib

    min_depth = min( minval( deptha ), minval( depthb ) )
    max_depth = max( maxval( deptha ), maxval( depthb ) )
    if(o>0) write(o,'(3A,9F10.3)') sym, fun, '[min,max] depth', min_depth, max_depth
    ! now map depths between [min_depth,max_depth] to integer depth between [10,990]

    fac = 980./(max_depth-min_depth)
    iposa(0,:) = 10 + nint( (deptha(:)-min_depth)*fac )
    iposb(0,:,:) = 10 + nint( (depthb(:,:)-min_depth)*fac ) ! same conversion for bond depths


    do ia = 1, na
      ! draw atoms
      irad = nint( AtomRadiusTable( itypa(ia) ) * MSCALE * AtomRadiusMult / deptha(ia) ) ! radius
      ipen_color = EightColorTable( itypa(ia) , 6 ) ! border color
      iw8 = 8
      if( present( aw8s ) ) iw8 = nint( min( (aw8(ia)-aw8_min)/max((aw8_max-aw8_min),0.001) , 1.0 )*8 )
      ifill_color = EightColorTable( itypa(ia) , iw8 ) ! fill color

      !! from http://epb.lbl.gov/xfig/fig-format.html ! ELLIPSE
      write(u,'(A,4(I0,A),9I8)') '1 3 0 ', AtomLineThickness, ' ', ipen_color, ' ', ifill_color, ' ', iposa(0,ia), &
        ' 0 20 0.0 1 0.0 ', iposa(1:2,ia), irad, irad, iposa(1:2,ia), iposa(1:2,ia)+(/irad,0/) ! circle
    enddo ! ia

    if( any(bond < 1 .or. bond > na) ) stop 'bonds illegal!'

    do ib = 1, nb
      ! draw bonds
      ifill_color = 0 ! BondColorTable( 0 ) ! no fill used
      iw8 = 8
      if( present( bw8s ) ) iw8 = nint( min( (bw8(ib)-bw8_min)/max((bw8_max-bw8_min),0.001), 1.0 )*ubound(BondColorTable,1) )
      ipen_color = BondColorTable( iw8 ) ! bond color

      depth = 0.5*( depthb(1,ib)+depthb(2,ib) ) ! average depth between start and end point
      irad = nint( BondRadiusTable( 0 ) * MSCALE / depth  ) ! bond radius
      idepth = 10 + nint( (depth-min_depth)*fac ) ! integer depth value

      !! from http://epb.lbl.gov/xfig/fig-format.html ! POLYLINE
      write(u,'(A,3(I0,A),/,9I8)') '2 1 0 ', irad, ' ', ipen_color, ' 0 ', idepth, &
        ' -1 -1 0.0 0 1 -1 0 0 2', iposb(1:2,1:2,ib)
    enddo ! ib

    close(unit=u,iostat=ios)

  contains
    function cross_product( v, w ) result( u )
      real, intent(in) :: v(3), w(3)
      real :: u(3)
      u(1) = v(2)*w(3)-v(3)*w(2)
      u(2) = v(3)*w(1)-v(1)*w(3)
      u(3) = v(1)*w(2)-v(2)*w(1)
    endfunction ! cross_product
  endfunction ! display_atoms_xfig


  status_t function display_atoms_povray( name, cell, apos, aw8s, AtomRadiusScale ) result( ios )
  use constants, only: Pi, ANGSTROM
  use configuration, only: o, MaxInputFileNameLen
    character(len=*), intent(in)    :: name ! write to the file "<name>.pov"
    real, intent(in)                :: cell(3) ! extend of the rectanglar cell
    real, intent(in)                :: apos(0:,:) ! (0:3,1:na), (0:3)=(/Z,x,y,z[,r]/) (Z should be integer[, radius r])
    real, intent(in), optional      :: aw8s(:) ! (1:na), atomic weights
    real, intent(in), optional      :: AtomRadiusScale
!     integer, intent(in)             :: bond(:,:) ! (1:2,1:nb) ! from atom#bond(1,:) to atom#bond(2,:)
!     real, intent(in), optional      :: bw8s(:) ! (1:nb), bond weights

    character(len=*), parameter     :: fun = ' povray: '
    iounit_t, parameter             :: u = 17

#ifdef __GFORTRAN__
    integer :: JMOL_COLORS(1:109) ! BOZ literals do not work in array constructor
#else
    integer, parameter :: JMOL_COLORS(1:109) = (/ &
    z'FFFFFF', z'D9FFFF', z'CC80FF', z'C2FF00', z'FFB5B5', &
    z'909090', z'3050F8', z'FF0D0D', z'90E050', z'B3E3F5', &
    z'AB5CF2', z'8AFF00', z'BFA6A6', z'F0C8A0', z'FF8000', &
    z'FFFF30', z'1FF01F', z'80D1E3', z'8F40D4', z'3DFF00', &
    z'E6E6E6', z'BFC2C7', z'A6A6AB', z'8A99C7', z'9C7AC7', &
    z'E06633', z'F090A0', z'50D050', z'C88033', z'7D80B0', &
    z'C28F8F', z'668F8F', z'BD80E3', z'FFA100', z'A62929', &
    z'5CB8D1', z'702EB0', z'00FF00', z'94FFFF', z'94E0E0', &
    z'73C2C9', z'54B5B5', z'3B9E9E', z'248F8F', z'0A7D8C', &
    z'006985', z'C0C0C0', z'FFD98F', z'A67573', z'668080', &
    z'9E63B5', z'D47A00', z'940094', z'429EB0', z'57178F', &
    z'00C900', z'70D4FF', z'FFFFC7', z'D9FFC7', z'C7FFC7', &
    z'A3FFC7', z'8FFFC7', z'61FFC7', z'45FFC7', z'30FFC7', &
    z'1FFFC7', z'00FF9C', z'00E675', z'00D452', z'00BF38', &
    z'00AB24', z'4DC2FF', z'4DA6FF', z'2194D6', z'267DAB', &
    z'266696', z'175487', z'D0D0E0', z'FFD123', z'B8B8D0', &
    z'A6544D', z'575961', z'9E4FB5', z'AB5C00', z'754F45', &
    z'428296', z'420066', z'007D00', z'70ABFA', z'00BAFF', &
    z'00A1FF', z'008FFF', z'0080FF', z'006BFF', z'545CF2', &
    z'785CE3', z'8A4FE3', z'A136D4', z'B31FD4', z'B31FBA', &
    z'B30DA6', z'BD0D87', z'C70066', z'CC0059', z'D1004F', &
    z'D90045', z'E00038', z'E6002E', z'EB0026' /)
    ! from http://jmol.sourceforge.net/jscolors/
#endif

    ! local vars
    real :: hc, cl(3), la(3), center_of_pos(0:3), p(3), c(3), r, rs
    integer :: na, ia, id, i1, i2, ic, nc, iZ
    real :: rrgb(0:3,-1:120) = .5 ! (radius,red,green,blue) init as 0.5,grey
    character(len=MaxInputFileNameLen+4) :: filename, f

    write(unit=filename,fmt='(9A)',iostat=ios) trim(name),'.pov' ! create the file name
    if( ios /= 0 ) then
      if(o>0) write(o,'(9A)') sym, fun, 'cannot create file name from "',trim(name),'" + ".pov"!' ! warn
      return
    endif ! ios /= 0
    open(unit=u,file=filename,action='write',iostat=ios) ! open the file
    if( ios /= 0 ) then
      if(o>0) write(o,'(9A)') sym, fun, 'cannot open file "',trim(filename),'"!' ! warn
      return
    endif ! ios /= 0

    ! start writing the pov file
    write(u,'(A)',iostat=ios) '#version 3.6;', '#include "colors.inc"', '#include "textures.inc"', '#include "shapes.inc"', '#include "glass.inc"', '#include "metals.inc"'
    write(u,'(A)',iostat=ios) '', 'background {color White}', ''

    rs = ANGSTROM ; if( present( AtomRadiusScale ) ) rs = abs( AtomRadiusScale )

    ! put a plane underneath the cell with a bit of mirror properties
    write(u,'(A,F0.3,A)',iostat=ios) 'plane { <0,1,0>, ',-.5*cell(3)-.1,' pigment{color <.6,.6,.6>} finish{ reflection {.2} ambient .5 diffuse .5 metallic }}', ''

    do iZ = 1, 109
      rrgb(1:3,iZ) = rgb_color( JMOL_COLORS(iZ) ) ! convert color
    enddo ! iZ

    rrgb(0,:) = 1. ! default radii

    ! special radii and colors
    ! Tab: iZ      radius red green blue
    rrgb(:, 0) = (/ 1.5,  .9, .9, .9 /) ! __
!     rrgb(:,32) = (/ 1.8,  .90, .83, .83 /) ! Ge
!     rrgb(:,51) = (/ 2.5,  .40, .40, .92 /) ! Sb
!     rrgb(:,52) = (/ 2.2,  .08, .08, .63 /) ! Te
    rrgb(:,32) = (/ 1.8,  1.0, .50, .10 /) ! Ge    255,127, 26
    rrgb(:,51) = (/ 2.5,  .70, 1.0, .10 /) ! Sb    178,255, 26
    rrgb(:,52) = (/ 2.2,  .40, .40, 1.0 /) ! Te    102,102,255

    ! adjust some radii
    rrgb(0, 1) =  .9 ! H
    rrgb(0, 6) = 1.3 ! C
    rrgb(0, 8) = 1.3 ! O
    rrgb(0,16) = 1.4 ! S
    rrgb(0,29) = 2.3 ! Cu
    rrgb(0,79) = 2.3 ! Au

    na = size(apos,2)
    center_of_pos = 0.
    do ia = 1, na ! for all atoms
      center_of_pos(1:3) = center_of_pos(1:3) + apos(1:3,ia)
      center_of_pos( 0 ) = center_of_pos( 0 ) + 1.0 ! each atom has weight 1
    enddo ! ia
    if( center_of_pos(0) > 0. ) center_of_pos = center_of_pos/center_of_pos(0)

    cl = (/3,2,1/) * cell ! camera location
    la = center_of_pos(1:3) ! look at the center of mass of all atomic positions
    write(u,'(2(3(A,F0.2),A))',iostat=ios) 'camera { location <',cl(1),',',cl(3),',',cl(2),'> ', &
      'look_at <',la(1),',',la(3),',',la(2), '> right x*image_width/image_height/2 up y/2 }'

    write(u,'(A)',iostat=ios) '', '// add lights', &
      'light_source { <0,200,400> color White area_light <200,0,0>, <0,0,200>, 99, 99 adaptive 1 jitter }', &
      'light_source { <400,200,0> color White area_light <200,0,0>, <0,0,200>, 99, 99 adaptive 1 jitter }', ''

    write(u,'(A)',iostat=ios) '', '// add the atoms'
    do ia = 1, na
      iZ = nint( apos(0,ia) ) ! atomic number
      p = apos(1:3,ia) ! position
      r = rrgb( 0 ,iZ) * rs ! radius
      if( ubound( apos, 2 ) > 3 ) r = max( .5, abs( apos(4,ia) ) ) * rs
      c = rrgb(1:3,iZ) ! color
      f = '' ; if( iZ < 1 ) f = ' filter .5'
      write(u,'(7(A,F0.2),9A)',iostat=ios) 'sphere{ <',p(1),',',p(3),',',p(2),'>, ',r, &
        ' pigment { color <',c(1),',',c(2),',',c(3),'>',trim(f),' } finish { phong .5 } }'
    enddo ! ia

    write(u,'(A)',iostat=ios) '', '// add the cell borders'
    do id = 1, 3
      nc = floor( 0.5*cell(id)*ANGSTROM )
      hc = cell(id)/( 2.*nc+1. )
      do i2 = -1, 1, 2
        do i1 = -1, 1, 2
          selectcase( id )
          case( 1 ) ; p = 0.5*cell*(/0,i1,i2/)
          case( 2 ) ; p = 0.5*cell*(/i2,0,i1/)
          case( 3 ) ; p = 0.5*cell*(/i1,i2,0/)
          endselect ! id
          c = p
          do ic = -nc, nc
            p(id) = hc*(ic-0.4) ; c(id) = hc*(ic+0.4) ! start and end
            write(u,'(9(A,F0.3))',iostat=ios) &
             'cylinder { <',p(1),',',p(3),',',p(2),'> <',c(1),',',c(3),',',c(2),'>, .1 pigment { color <.9,.9,.9> } }'
          enddo ! ic
        enddo ! i1
      enddo ! i2
    enddo ! id

    close(unit=u,iostat=ios)
    if(o>0) write(o,'(9A)') sym, fun, 'generated file "', trim(filename), '".'

  contains

    function rgb_color( i24 ) result( rgb )
      integer, intent(in) :: i24
      real :: rgb(3)
      integer :: i, j, ic
      i = i24 ! copy
      do ic = 3, 1, -1
        j = mod( i, 256 )
        rgb(ic) = j / 255.
        i = i / 256
      enddo ! ic
    endfunction ! rgb_color

  endfunction ! display_atoms_povray


  status_t function display_VTK_file( name, g0, rho ) result( ios )
  use type_grid, only: grid, BC_PERIODIC
  use configuration, only: MaxInputFileNameLen, VTK_FileNameExtension
    character(len=*), intent(in)    :: name
    type(grid), intent(in)          :: g0
    real, intent(in)                :: rho(:,:,:) ! (ng(1),ng(2),ng(3))

    character(len=*), parameter     :: fun = ': '
    iounit_t, parameter             :: u = 77
    integer, parameter              :: IMPOSSIBLE = -9 ! impossible index value
    real, parameter                 :: rIMPOSSIBLE = -999. ! impossible real value
    integer, parameter              :: iv3IDX(1:6) = (/ 3,6,2,5,1,4 /) ! 3rd vertex
    integer, parameter              :: i6_opposed(1:6) = (/ 5,4,6,2,1,3 /) ! non-adjacent edge
    integer, parameter              :: Niso = 4
    real, parameter                 :: isopercent(1:Niso) = (/50.,25.,12.5,6.25/)
    character(len=MaxInputFileNameLen+16) :: filename
    type(grid)                            :: g
    logical :: peri(3)

    !! tetrahedron method
    integer :: ng(3) ! number of grid points in each direction

!     real, allocatable :: rpoints(:,:,:,:)
    real, allocatable :: val(:,:,:)
    real :: val07(0:7)
    real :: rpo(3), rpe(3)
    integer :: ix, iy, iz, ii, id, ik, ie, ip, iiso, it, iv, is(3), iv4(4), i4, icnt, i7, j7, i6, j6
    integer :: ntriangles, npoints, ipoint, itri
    integer, allocatable  :: ipedge(:,:,:,:) ! (7,0:nkp(1:3))
    integer, allocatable  :: id3angle(:,:) ! (3,12*product(ng+1))
    real, allocatable     :: redge(:,:,:,:,:) ! (3,7,0:ng(1:3)))
    logical, allocatable  :: ledge(:,:,:,:) !   (7,0:ng(1:3))
    real, allocatable     :: rpos(:,:) ! (3,:)
    real                  :: val_iso
    integer               :: i26(2,6), i6_3or4(4), ip_3or4(4)
cDBG  integer :: hist012(0:2)

cDBG  if(o>0) write(o,'(9A)') sym, fun, 'start!'

  g = g0 ! copy grid descriptor
  g%nh = 1 ! set the halo thickness to the minimum
  peri(1:3) = ( g%bc(:,1) == BC_PERIODIC )

  ng(1:3) = shape( rho )
  if( any( ng /= g%ng_all(1:3) ) ) stop 'tetra: number of grid points does not match!'
  allocate( val(0:ng(1)+2,0:ng(2)+2,0:ng(3)+2), stat=ios )
  if( ios /= 0 ) return
  val = 0. ! init
  val(1:ng(1),1:ng(2),1:ng(3)) = rho ! copy data to the central region

  ! continue data periodically, if the boundary condition is periodic
  if( peri(1) ) val(ng(1)+1,:,:) = val(1,:,:) ! in x-direction
  if( peri(1) ) val(ng(1)+2,:,:) = val(2,:,:) ! in x-direction
  if( peri(1) ) val(  0,:,:) = val(ng(1),:,:) ! in x-direction
  if( peri(2) ) val(:,ng(2)+1,:) = val(:,1,:) ! in y-direction
  if( peri(2) ) val(:,ng(2)+2,:) = val(:,2,:) ! in y-direction
  if( peri(2) ) val(:,  0,:) = val(:,ng(2),:) ! in y-direction
  if( peri(3) ) val(:,:,ng(3)+1) = val(:,:,1) ! in z-direction
  if( peri(3) ) val(:,:,ng(3)+2) = val(:,:,2) ! in z-direction
  if( peri(3) ) val(:,:,  0) = val(:,:,ng(3)) ! in z-direction

!   ! create an array of grid point position vectors which is enlarged into the halo-regions
!   allocate( rpoints(3,0:ng(1)+2,0:ng(2)+2,0:ng(3)+2), stat=ios )
!   if( ios /= 0 ) return
!   rpoints = 0.
!   do iz = 0, ng(3)+2
!    do iy = 0, ng(2)+2
!     do ix = 0, ng(1)+2
!       rpoints(1:3,ix,iy,iz) = (/ix,iy,iz/) * g%h(1:3) + g%off(1:3)
!     enddo ! ix
!    enddo ! iy
!   enddo ! iz

  !! cut each cube (in internal coordinates) into 6 tetrahedra,
  !! where all 6 tetrahedra have a common edge being the space-diagonal of that cube

  !! if the basis vectors of the space were non-orthognal, one sould rotate space
  !! such that the space diagonal is shortest

  !!
  !!     6-------7
  !!    /|      /|
  !!   4-------5 |    z
  !!   | 2-----|-3    |  y
  !!   |/      |/     | /
  !!   0-------1      o ----x
  !! the cube vertices are indexed #0,#1,#2,#3,#4,#5,#6 and #7
  !! according to the binary representation 000 ... 111

  !!
  !! now lets assume that we choose the spatial diagonal #0-#7, then we get a top view
  !! onto the cube with the space diagonal pointing
  !! out of the plane          and into the plane
  !!
  !!     2-----3                   4-----5
  !!    /     / \                 / \     \
  !!   /     /   \               /   \     \
  !!  6 --- 7     1             6     0 --- 1
  !!   \     \   /               \   /     /
  !!    \     \ /                 \ /     /
  !!     4-----5                   2-----3
  !!
  !! so we can start to identify the 6 tetrahedra formed by their non-shared regular edge
  !!
  !!       2-----3              1  2  3  4
  !!      / \ c / \          a #0-#1-#3-#7
  !!     / b \ / a \         b #0-#2-#6-#7
  !!    6 --- 7 --- 1        c #0-#3-#2-#7
  !!     \ f / \ e /         d #0-#4-#5-#7
  !!      \ / d \ /          e #0-#5-#1-#7
  !!       4-----5           f #0-#6-#4-#7
  !!
  !! for each cube, there are 19 edges, i.e.
  !!       12 regular edges
  !!        6 face diagonals (here, #1-#7, #2-#7 and #4-#7 are visible)
  !!    and 1 space diagonal (#0-#7, out of plane)
  !!
  !! however, adjacent cubes share faces and thus edges (and we might be working on a periodic grid)
  !! so we can save resources by trying to avoid double evaluations
  !! with this, each cube owns only 3 regular edges, only 3 face diagonals, 1 space diagonal, so 7 of 19 (wow, only 37%)
  !!
  !! so we only care about the 3 regular edges #0-#1, #0-#2 and #0-#4
  !!                      the 3 face diagonals #0-#3, #0-#5 and #0-#6
  !!                  and the 1 space diagonal #0-#7
  !! i.e. all the edges that connect to the original vertex point  #0 (000)

  allocate( redge(3,7,0:ng(1)+1,0:ng(2)+1,0:ng(3)+1), &
              ledge(7,0:ng(1)+1,0:ng(2)+1,0:ng(3)+1), stat=ios )
  if( ios /= 0 ) return

  !!
  !!     6-------7
  !!    /|      /|
  !!   4-------5 |    z
  !!   | 2-----|-3    |  y
  !!   |/      |/     | /
  !!   0-------1      o ----x
  !!

  do iiso = 1, Niso

    val_iso = isopercent(iiso)/100. * maxval( rho )
cDBG  if(o>0) write(o,'(3A,F0.3,9A)') sym, fun, 'create isosurface at ', isopercent(iiso), ' %.'

    ledge = .false. ! init
    redge = rIMPOSSIBLE ! init impossible value in internal coordinates if the Brillin zone is sampled from -0.5 to 0.5

    do iz = 0, ng(3)+1
     do iy = 0, ng(2)+1
      do ix = 0, ng(1)+1

        !! process the 7 edges

        ! copy values on the 8 vertices of the cube ! index ip==0 means the vertices themselfs, no additional points
        val07 = reshape( val(ix:ix+1,iy:iy+1,iz:iz+1), (/8/) ) - val_iso

        rpo = (/ix,iy,iz/) * g%h(1:3) + g%off(1:3) ! rpoints(:,ix,iy,iz) ! origin grid point
        do ie = 1, 7 ! for all 7 edges from #0 to #ie

          ledge(ie,ix,iy,iz) = ( val07(0) * val07(ie) < 0. ) ! does the sign change?
          if( .not. ledge(ie,ix,iy,iz) ) cycle ! nothing to do if the sign does not change

          is = ishift( ie )
          rpe = ( (/ix,iy,iz/)+is ) * g%h(1:3) + g%off(1:3) ! rpoints(:,ix+is(1),iy+is(2),iz+is(3)) ! target grid point

          ! use simple linear interpolation between the vertices of the cube
          redge(:,ie,ix,iy,iz) = rpo - ( rpe - rpo ) * val07(0) / ( val07(ie) - val07(0) )

        enddo ! ie

      enddo ! ix
     enddo ! iy
    enddo ! iz

    npoints = count( ledge ) ! get the total number of points
    allocate( rpos(3,0:npoints), ipedge(7,0:ng(1)+1,0:ng(2)+1,0:ng(3)+1), id3angle(3,99+2*npoints), stat=ios )
    if( ios /= 0 ) return
    rpos = rIMPOSSIBLE ! init
    ipedge = IMPOSSIBLE ! init
    id3angle = IMPOSSIBLE ! init
    ntriangles = 0 ! total counter for the number of triangles

cDBG    hist012 = 0 ! init histogram
    ii = 0
    ipoint = 0 ! init, start with point ID 0 
    do iz = 0, ng(3)
     do iy = 0, ng(2)
      do ix = 0, ng(1)

        !! process the tetrahedra

        iv4(1) = 0 ; iv4(4) = 7 ! 1st and last point of the tetrahedron
        do it = 1, 6 ! for each of the 6 tetrahedra, a...f

          !! i4: 1  2  3  4      2-----3
          !!  a #0-#1-#3-#7     / \ c / \
          !!  b #0-#2-#6-#7    / b \ / a \
          !!  c #0-#3-#2-#7   6 --- 7 --- 1
          !!  d #0-#4-#5-#7    \ f / \ e /
          !!  e #0-#5-#1-#7     \ / d \ /
          !!  f #0-#6-#4-#7      4-----5
          !!     0 it iv 7  with iv == iv3IDX(1:6) = (/ 3,6,2,5,1,4 /)
          iv4(2) = it
          iv4(3) = iv3idx(it)
          !! iv4(1)==0, iv4(2)==it, iv4(3)==iv3idx(it), iv4(4)==7

          ! set up the table of the 6 edges of this tetrahedron
          !             adjacency matrix  1  2  3  4  5  6 (edges i and j have common vertices)
          i26(:,1) = (/iv4(1),iv4(4)/) !  T  T  T  T  F  T the 1st one is the space diagonal (the only constant)
          i26(:,2) = (/iv4(1),iv4(3)/) !  T  T  T  F  T  T
          i26(:,3) = (/iv4(1),iv4(2)/) !  T  T  T  T  T  F
          i26(:,4) = (/iv4(2),iv4(4)/) !  T  F  T  T  T  T
          i26(:,5) = (/iv4(2),iv4(3)/) !  F  T  T  T  T  T
          i26(:,6) = (/iv4(3),iv4(4)/) !  T  T  F  T  T  T
          ! instead of the logical adjacency matrix, we store the index of the
          ! non-adjacent edge i6_opposed(1:6) = (/ 5,4,6,2,1,3 /)

cDBG      i6_3or4 = IMPOSSIBLE ! init index array
cDBG      ip_3or4 = IMPOSSIBLE ! init index array
          icnt = 0 ! init counter of intersecting edge of this tetrahedron
          do i6 = 1, 6 ! for each of the 6 edges of this tetrahedron
            ! find out which edge (j6) connects the cube vertices i26(1,i6) and i26(2,i6)
            ! and in which data set it the edge is stored, this cube(is==000) or a neighbor cube
            j6 = jedge( i26(1,i6), i26(2,i6), is )

            if( .not. ledge(j6,ix+is(1),iy+is(2),iz+is(3)) ) cycle
            ! there is an intersection on this edge
            if( ipedge(j6,ix+is(1),iy+is(2),iz+is(3)) < 0 ) then
              ! ID has not been assigned
              ! assign new point ID
              ip = ipoint ! and use the new point ID after this as ip
              ipoint = ipoint+1 ! create new point ID for the next point

              ipedge(j6,ix+is(1),iy+is(2),iz+is(3)) = ip
              ! store the point position to the list of triangle vertex points
              rpos(:,ip) = redge(:,j6,ix+is(1),iy+is(2),iz+is(3))
            else  ! the ID has not been set
              ! the ID has been assigned before
              ip = ipedge(j6,ix+is(1),iy+is(2),iz+is(3)) ! load old point ID
            endif ! the ID has not been set
            icnt = icnt+1! count how many edges of this tetrahedron intersect
            i6_3or4(icnt) = i6 ! store the edge which intersects
            ip_3or4(icnt) = ip ! store the ID of the intersection point
          enddo ! i6

          !=================================================================================
          selectcase( icnt )
          !=================================================================================
          case( 0 ) ! none
            !-------------------------------------------------------------------------------
cDBG        hist012(0) = hist012(0)+1
            !-------------------------------------------------------------------------------

          !=================================================================================
          case( 3 ) ! one vertex has a different sign, intersection is one triangle
            !-------------------------------------------------------------------------------
cDBG        hist012(1) = hist012(1)+1

            ntriangles = ntriangles+1 ! new triangle
            id3angle(:,ntriangles) = ip_3or4(1:3) ! go 1-2-3
            ! the 3 edges which intersect are given in i6_3or4(1:3)
            !-------------------------------------------------------------------------------

          !=================================================================================
          case( 4 ) ! intersection is a tetragon, so create two triangles
            !-------------------------------------------------------------------------------
cDBG        hist012(2) = hist012(2)+1

            ntriangles = ntriangles+1 ! new triangle
            id3angle(:,ntriangles) = ip_3or4(1:3) ! go 1-2-3

            ntriangles = ntriangles+1 ! new triangle
            ! for the second triangle we need to see, which is the edge opposing the 1st edge
            if( i6_opposed( i6_3or4(1) ) == i6_3or4(2) ) then
              id3angle(:,ntriangles) = ip_3or4( (/1,2,4/) ) ! go 1-2-4
            elseif( i6_opposed( i6_3or4(1) ) == i6_3or4(3) ) then
              id3angle(:,ntriangles) = ip_3or4( (/1,3,4/) ) ! go 1-3-4
            else cDBG if( i6_opposed( i6_3or4(1) ) == i6_3or4(4) ) then
              id3angle(:,ntriangles) = ip_3or4( (/2,3,4/) ) ! go 2-3-4
cDBG        else ; stop 'tetra: one of the 3 cases must match!'
            endif
            ! the 4 edges which intersect are given in i6_3or4(1:4)
            !-------------------------------------------------------------------------------

cDBG      !=================================================================================
cDBG      case default
cDBG        !-------------------------------------------------------------------------------
cDBG        stop 'tetra: cases {0,3,4} only!'
cDBG        !-------------------------------------------------------------------------------

          !=================================================================================
          endselect ! icnt
          !=================================================================================

        enddo ! it
      enddo ! ix
     enddo ! iy
    enddo ! iz
cDBG  if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'create ', ntriangles, ' triangles'
cDBG  if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'histogram: ', hist012(0), ' x case(0), ', hist012(1), ' x case(3) and ', hist012(2), ' x case(4)'
! cDBG  if( sum(hist012) /= 6*product(nkp) ) stop 'tetra: some counting error occured! histogram is messed up'

    if( ipoint /= npoints .and. o>0) write(o,'(3A,9(I0,A))') sym, fun, 'not every intersection point has been found! Only ', ipoint, ' of ', npoints
    npoints = ipoint
cDBG  if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'create ', npoints, ' points'

    write(unit=filename,fmt='(2A,I0,9A)',iostat=ios) trim(name),'_',iiso,VTK_FileNameExtension
    if( ios /= 0 ) return

cDBG  if(o>0) write(o,'(9A)') sym, fun, 'write to file "', trim(filename), '".'
    open(unit=u,file=filename,iostat=ios)
    if( ios /= 0 ) return
    write(u,'(9(A,I0))') '<VTKFile type="PolyData" version="0.1" byte_order="LittleEndian">'
    write(u,'(9(A,I0))') '  <PolyData>'
    write(u,'(9(A,I0))') '    <Piece NumberOfPoints="',npoints,'" NumberOfVerts="0" NumberOfLines="0" NumberOfStrips="0" NumberOfPolys="',ntriangles,'">'
    write(u,'(9(A,I0))') '      <Points>'
    write(u,'(9(A,I0))') '        <DataArray NumberOfComponents="3" type="Float32" format="ascii">'
    do ipoint = 0, npoints-1
      write(u,'(9x,3(" ",F12.7))') rpos(1:3,ipoint)
    enddo !
    write(u,'(9(A,I0))') '        </DataArray>'
    write(u,'(9(A,I0))') '      </Points>'
    write(u,'(9(A,I0))') '      <Polys>'
    write(u,'(9(A,I0))') '        <DataArray type="Int32" Name="connectivity" format="ascii">'
    do itri = 1, ntriangles
      write(u,'(9x,3(" ",I0))') id3angle(1:3,itri)
    enddo ! itri
    write(u,'(9(A,I0))') '        </DataArray>'
    write(u,'(9(A,I0))') '        <DataArray type="Int32" Name="offsets" format="ascii">'
      write(u,'(9x,9999(" ",I0))') ( 3*itri, itri=1,ntriangles )
    write(u,'(9(A,I0))') '        </DataArray>'
    write(u,'(9(A,I0))') '      </Polys>'
!     write(u,'(9(A,I0))') '      <PointData Scalars="RealPart">'
!     write(u,'(9(A,I0))') '        <DataArray type="Float32" Name="RealPart" NumberOfComponents="1" format="ascii">'
!     write(u,'(9(A,I0))') '        </DataArray>'
!     write(u,'(9(A,I0))') '      </PointData>'
    write(u,'(9(A,I0))') '    </Piece>'
    write(u,'(9(A,I0))') '  </PolyData>'
    write(u,'(9(A,I0))') '</VTKFile>'
    close(unit=u,iostat=ios)
    deallocate( rpos, ipedge, id3angle, stat=ios )

  enddo ! iiso

  contains

    function ishift( iedge ) result( is )
      integer, intent(in) :: iedge ! argument
      integer             :: is(3) ! result

      selectcase( iedge ) ! the edge is identified via the vertex point they points to
      !==============================================
      case( 1 ) ; is = (/1,0,0/) ! regular edge   100
      case( 2 ) ; is = (/0,1,0/) ! regular edge   010
      case( 4 ) ; is = (/0,0,1/) ! regular edge   001
      !==============================================
      case( 3 ) ; is = (/1,1,0/) ! face-diagonal  110
      case( 5 ) ; is = (/1,0,1/) ! face-diagonal  101
      case( 6 ) ; is = (/0,1,1/) ! face-diagonal  011
      !==============================================
      case( 7 ) ; is = (/1,1,1/) ! space-diagonal 111
      !==============================================
!     case( 0 ) ; is = (/0,0,0/) ! vertex point   000
cDBG  case default ; stop 'tetra: cases 1..7 only!'
      endselect ! iedge
    endfunction ! ishift

    integer function jedge( i0, ie, is ) result( j6 )
      integer, intent(in)  :: i0, ie ! start and end vertex index
      integer, intent(out) :: is(3)
!      !  (the indices in this cube are in parenthesis)
!      !                                       cartesian edges           shared face diagonals
      !!
      !!     6-------7
      !!    /|      /|
      !!   4-------5 |    z
      !!   | 2-----|-3    |  y
      !!   |/      |/     | /
      !!   0-------1      o ----x
      !!
      ! cube in 100 direction shares 3 edges: #2 (#1-#3) #4 (#1-#5)          #6 (#1-#7)
      ! cube in 010 direction shares 3 edges: #1 (#2-#3) #4 (#2-#6)          #5 (#2-#7)
      ! cube in 001 direction shares 3 edges: #1 (#4-#5) #2 (#4-#6)          #3 (#4-#7)
      ! cube in 110 direction shares 1 edge:  #4 (#3-#7)
      ! cube in 101 direction shares 1 edge:  #2 (#5-#7)
      ! cube in 011 direction shares 1 edge:  #1 (#6-#7)
      selectcase( i0 +8* ie )
      !--------------------------------------------------
      ! edges of this cube: all shifts are 0
      case( 0 +8* 1 ) ; j6 = 1 ; is = (/0,0,0/)
      case( 0 +8* 2 ) ; j6 = 2 ; is = (/0,0,0/)
      case( 0 +8* 3 ) ; j6 = 3 ; is = (/0,0,0/)
      case( 0 +8* 4 ) ; j6 = 4 ; is = (/0,0,0/)
      case( 0 +8* 5 ) ; j6 = 5 ; is = (/0,0,0/)
      case( 0 +8* 6 ) ; j6 = 6 ; is = (/0,0,0/)
      case( 0 +8* 7 ) ; j6 = 7 ; is = (/0,0,0/)

      ! cube in 100 direction shares 3 edges: #2 (#1-#3) #4 (#1-#5)          #6 (#1-#7)
      case( 1 +8* 3 ) ; j6 = 2 ; is = (/1,0,0/)
      case( 1 +8* 7 ) ; j6 = 6 ; is = (/1,0,0/)
      case( 5 +8* 1 ) ; j6 = 4 ; is = (/1,0,0/)

      ! cube in 010 direction shares 3 edges: #1 (#2-#3) #4 (#2-#6)          #5 (#2-#7)
      case( 2 +8* 6 ) ; j6 = 4 ; is = (/0,1,0/)
      case( 2 +8* 7 ) ; j6 = 5 ; is = (/0,1,0/)
      case( 3 +8* 2 ) ; j6 = 1 ; is = (/0,1,0/)

      ! cube in 001 direction shares 3 edges: #1 (#4-#5) #2 (#4-#6)          #3 (#4-#7)
      case( 4 +8* 5 ) ; j6 = 1 ; is = (/0,0,1/)
      case( 4 +8* 7 ) ; j6 = 3 ; is = (/0,0,1/)
      case( 6 +8* 4 ) ; j6 = 2 ; is = (/0,0,1/)

      ! cube in 110 direction shares 1 edge:  #4 (#3-#7)
      case( 3 +8* 7 ) ; j6 = 4 ; is = (/1,1,0/)
      ! cube in 101 direction shares 1 edge:  #2 (#5-#7)
      case( 5 +8* 7 ) ; j6 = 2 ; is = (/1,0,1/)
      ! cube in 011 direction shares 1 edge:  #1 (#6-#7)
      case( 6 +8* 7 ) ; j6 = 1 ; is = (/0,1,1/)
      !--------------------------------------------------
      case default ; stop 'tetra: implementation wrong!'
      endselect ! ie + 8*i0
    endfunction ! jedge

  endfunction ! display_VTK_file

  !! writes a real number with the F24.16 format, but the erroneous digits are cut off.
  elemental string_t function error_digits( value, error ) result( str ) ! result is a string
    real, intent(in)  :: value, error

    string_t          :: f !! new format
    integer           :: n !! number of digits after the dot
    status_t          :: s
    n = max( 1, min(   nint( log10( abs(error) ) )  , 16 ))
    write( unit=f, fmt='(9(A,I0))', iostat=s ) '(F', 8+n ,'.', n, ')'
    write( unit=str, fmt=f, iostat=s ) value
  endfunction ! error_digits

  !! self tests for this module
  status_t function test( )
    test = display_decomp_ascii( modul='   ', unt=6, nproc=(/3,4,2/) )
  endfunction ! test

!- extended
#endif
endmodule ! display

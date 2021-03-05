#include "config.h"

#define DEBUG

!! @author Paul Baumeister
!! @version 4.0
!!
!! tools for constrained DFT, see special documentation
module constraints
  use configuration, only: o
  use type_grid, only: grid
  use unitsystem, only: Ang, Ang_
  use type_atom, only: atom
#ifdef CONSTR
  use type_state, only: MAX_N_CONSTRAINTS
#endif
implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: sym = 'cDFT' !! module symbol

  public :: init_mask
#ifdef EXTENDED
  public :: test
#endif

  logical,              protected, public :: constrain = .false. ! On Off
  integer,              protected, public :: n_constraints = 0
  real, allocatable,    protected, public :: mask(:,:) ! (nxyzs,n_constraints)
  real, allocatable,    protected, public :: q_constraints(:,:) ! (n_constraints,nspin)
  real, allocatable,               public :: e_constraints(:,:) ! (n_constraints,nspin)
  real, allocatable,    protected, public :: w_constraints(:) ! (n_constraints)
  integer, allocatable, protected, public :: i_constraints(:) ! (natoms)

  real,         parameter :: GaussianSpreadMinimum = 0.01 ! in Bohr ! use this if given value is smaller
  real,         parameter :: GaussianSpreadDefault = 1.0  ! in Bohr ! use this if no value is given
  real,         parameter :: GaussianSpreadMaximum = 100. ! in Bohr ! warn if value is larger
  real, protected, public :: GaussianSpread = GaussianSpreadDefault ! w8 = exp( -r^2/GaussianSpread^2 )

  contains

  status_t function init_mask( project, qtot, g, a, spin ) result( ist )
  use MPItools, only: MPIallsum
  use configuration, only: WARNING
  implicit none
    ! parameters
    character(len=*), parameter     :: fun = ' init_mask: '
    character(len=*), parameter     :: fileextension = '.cDFT'
    iounit_t, parameter             :: u = 77 ! file unit
#ifndef CONSTR
    integer, parameter              :: MAX_N_CONSTRAINTS = 1
#endif
    ! arguments
    character(len=*), intent(in)    :: project
    real, intent(in)                :: qtot !! total electron charge
    type(grid), intent(in)          :: g !! grid descriptor (coarse grid)
    type(atom), intent(in)          :: a(:) !! make sure, these are all atoms
    logical, intent(in)             :: spin
    ! local vars
    string_t                        :: word, filename
    integer                         :: ka(size( a )) ! which mask this atom belongs to
    integer                         :: k, ios, lios, ia, na, i1, i2, i3, ii, ixyzs, nxyzs, ic, nc, is, ns
    real                            :: rv(3), rr, wgt, qm(2)
    integer                         :: aindex(2)
    real, allocatable               :: m(:), cow(:,:)
    real                            :: q(2,MAX_N_CONSTRAINTS)
    real                            :: dec ! = 1.0/GaussianSpread^2 !! mask decay factor

    q = 0. ! init charges

    ns = 1 ; if( spin ) ns = 2
    ! generate filename <projectname>.cDFT
    write(unit=filename,fmt='(9A)',iostat=ios) trim(project),fileextension
#ifdef DEBUG
    if(o>0) write(o,'(9A)') sym, fun, 'try to read from "', trim( filename ), '".'
#endif
    open(unit=u,file=filename,status='old',action='read',iostat=ios)
    if( ios /= 0 ) then
      if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'file "', trim( filename ) , '" not found!'
      if(o>0) write(o,'(9A)') sym, fun, 'create a single mask and switch constraints OFF!'
      nxyzs = product( g%ng ) ! number of degrees of freedom of a wave function
      nc = 1
      allocate( q_constraints(nc,ns), w_constraints(nc), mask(nxyzs,1:nc),  &
                e_constraints(nc,ns), i_constraints(size(a)), stat=ist )
      mask = 1. ! unity
      if(o>0) write(o,'(4A,I0,9A)') sym, fun, WARNING(0), 'constraint mask is unity. Compile without -D CONSTR=',MAX_N_CONSTRAINTS,' to speed up.'
      constrain = .false. ! OFF
      n_constraints = nc
      e_constraints = 0.
      w_constraints = 1.
      q_constraints = 0. ! does not matter since constraints are OFF
      i_constraints = 1  ! all atoms belong to mask #1
      return ! allocation status
    endif ! ios /= 0
    ! now file is open

    nc = 0 ! init number of valid constraints
    ka = 0 ! init (by default, all atoms belong to mask 0)

    ! read the cDFT file <projectname>.cDFT
    ! 1st line contains the spread parameter followed by comments
    ! 2nd line says "on", as first word, if the constraints should be applied,
    !   otherwise only the mask weights are computed
    ! from the 3rd line, follow the pattern
    !   atom_start_index atom_end_index charge_in_mask magnetic_moment_of_mask


    word = '' ; read(unit=u,fmt=*,iostat=lios) word ! first contains the GaussianSpread parameter in Bohr units
    if(o>0) write(o,'(9A)') sym, fun, 'constrained DFT file, 1st line says="',trim(word),'", mask spread parameter in Bohr expected!'
    read(unit=word,fmt=*,iostat=ios) GaussianSpread ! first line contains the spread in Bohr units

    if( ios /= 0 ) then ! could not be read from the .cDFT file
      GaussianSpread = GaussianSpreadDefault
      if(o>0) write(o,'(4A,F0.6,9A)') sym, fun, WARNING(0),'cannot read mask spread parameter, use default ',GaussianSpread*Ang, Ang_
    endif ! default

    if( GaussianSpread <= GaussianSpreadMinimum ) then
      GaussianSpread = GaussianSpreadMinimum
      if(o>0) write(o,'(4A,F0.6,9A)') sym, fun, WARNING(0),'mask spread parameter is too small, set to minimum ',GaussianSpread*Ang, Ang_
    endif ! minimum
    
    if( GaussianSpread > GaussianSpreadMaximum ) then
      if(o>0) write(o,'(4A,F0.3,9A)') sym, fun, WARNING(0),'mask spread parameter is very large ',GaussianSpread*Ang, Ang_
      ii = minval( a(:)%nimages )
      if(ii>1 .and. o>0) write(o,'(3A,I0,A,F0.3,9A)') sym, fun, 'make sure that ',ii, &
       ' periodic images are sufficient for a mask spread of ',GaussianSpread*Ang, Ang_
    endif ! maximum

    if(o>0) write(o,'(3A,F0.6,9A)') sym, fun, 'spread parameter of the masks is ',GaussianSpread*Ang, Ang_

    word = '' ; read(unit=u,fmt=*,iostat=lios) word ! here ist the on/off switch
    selectcase( word )
    case( 'on', 'ON', 'On' ) ; constrain = .true.
      if(o>0) write(o,'(9A)') sym, fun, 'constrained DFT switched ON'
    case default             ; constrain = .false.
      if(o>0) write(o,'(9A)') sym, fun, 'constrained DFT switched OFF, line says="',trim(word),'", use "on" to activate!'
    endselect ! word

      read(unit=u,fmt=*,iostat=lios) aindex(1:2), qm(1:2) ! read 1st line
    do while( lios == 0 )
      ! process line
      if( any( aindex < 1 ) ) stop 'cDFT an atom index < 1'
      if( any( aindex > size(a) ) ) stop 'cDFT an atom index exceeds limits!'
      ! if( aindex(2) < aindex(1) ) stop 'cDFT atom index #2 < atom index #1'
      if( qm(1) < 0. ) stop 'cDFT negative electronic charge for constraint.'
      if( abs(qm(2)) > qm(1) ) stop 'cDFT |M| > Q, too much magnetization!'
      nc = nc+1 ! make a new mask
      if( nc > MAX_N_CONSTRAINTS ) then
        write(*,'(9(3A,I0))') sym, fun, 'too many masks requested, use preprocessor directive -D CONSTR=',nc,' (or larger)'
        stop 'cDFT: too many masks requested, increase hard limit via the preprocessor directive -D CONSTR=<n>'
      endif

      na = 0
#ifdef DEBUG
      if(o>0) write(o,'(3A,I3,2(A,I0),A,F10.3)') sym, fun, 'new mask #',nc, &
        ' from atom #',aindex(1),' through #',aindex(2),' q=',qm(1)
#endif
      do ia = aindex(1), aindex(2)
        if( ka(ia) == 0 ) then
          ka(ia) = nc ! set
          na = na+1
        else
          if(o>0) write(o,'(4A,9(I0,A))') sym, fun, WARNING(0), &
            'atom #', ia, ' has been assigned to mask #',ka(ia),' before!'
        endif ! ka(ia) is unset
      enddo ! ia

      if( na < 1 ) then
        if(o>0) write(o,'(4A,9(I0,A))') sym, fun, WARNING(0), &
          'no new mask has been created for the range atom #',aindex(1),' through #',aindex(2)
        nc = nc-1
      else ; q(1:2,nc) = qm(1:2)
      endif ! na < 1

      read(unit=u,fmt=*,iostat=lios) aindex(1:2), qm(1:2) ! read next line
    enddo ! while
    close(unit=u,iostat=ios) ! reading finished

    if( any( ka == 0 ) ) then
      nc = nc+1
      where( ka == 0 ) ka = nc
      q(1,nc) = qtot - sum( q(1,1:nc-1) )
      q(2,nc) = 0.   - sum( q(2,1:nc-1) )
      if(o>0) write(o,'(3A,I0,A,2F10.3)') sym, fun, &
        'not all constraints have been set, other atoms in mask #', nc, ' q =', q(:,nc)
    else  ! any ka == 0
      ! all ka have been set, check, if the system is charged in total
      if( abs( sum( q(1,1:nc) ) - qtot ) > 1E-6 ) then
        if(o>0) write(o,'(4A,9(F10.3,A))') sym, fun, WARNING(0), &
          'the sum of all constraint charges deviates from the total charge, sum=', &
           sum( q(1,1:nc) ), ' qtot=', qtot
      endif ! 
      if(o>0) write(o,'(3A,9(F10.3,A))') sym, fun, &
          'the sum of all constraint magnetic moments deviates from 0, sum=', sum( q(2,1:nc) )
    endif ! any ka == 0


#ifdef DEBUG
    do ia = 1, size(a)
      if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'atom #', ia, ' mask #', ka(ia)!, ' #images', a(ia)%nimages
    enddo ! ia
#endif

    nxyzs = product( g%ng ) ! number of degrees of freedom of a wave function
    n_constraints = nc
    allocate( q_constraints(nc,ns), w_constraints(nc), mask( nxyzs, 1:nc ),  &
              e_constraints(nc,ns), i_constraints(size(a)) )
    allocate( m(0:nc), cow(0:3,nc) )
    q_constraints = transpose( q(1:ns,1:nc) )
    i_constraints = ka

    dec = 1./GaussianSpread**2
    if(o>0) write(o,'(3A,F0.6,9A)') sym, fun, 'spread parameter of the masks is ',sqrt(1./dec)*Ang, Ang_

    mask = 0. ! init
#ifdef DEBUG
    if(o>0) write(o,'(9A)') sym, fun, 'start'
#endif

    cow = 0 ! center of weight

    do i3 = 1, g%ng(3)     ; rv(3) = i3*g%h(3) + g%off(3)
      do i2 = 1, g%ng(2)   ; rv(2) = i2*g%h(2) + g%off(2)
        do i1 = 1, g%ng(1) ; rv(1) = i1*g%h(1) + g%off(1)
          ixyzs = i1 + g%ng(1)*( (i2-1) + g%ng(2)*(i3-1) )
          ! at each grid point
          m = 0.
          do ia = 1, size(a)
            k = ka(ia) ! mask index
            do ii = 1, a(ia)%nimages
              rr = sum( (rv(1:3)-a(ia)%imagepos(1:3,ii))**2 )
              wgt = exp(-dec*rr)
              m(k) = m(k) + wgt ! sum weight for mask number k
              cow(0,k) = cow(0,k) + wgt
              cow(1:3,k) = cow(1:3,k) + wgt*rv(1:3)
            enddo ! ii
          enddo ! ia
          m(0) = sum(m(1:nc))
          if( m(0) > 0. ) then
              mask(ixyzs,1:nc) = m(1:nc) / m(0) ! normalize all weights
          else
              mask(ixyzs,1:nc) = 0
              write(*,'(3A,9(i0,a))') sym, fun, 'cannot normalize mask at grid point ',i1,' ',i2,' ',i3
          endif

        enddo ! i1
      enddo ! i2
    enddo ! i3

    do ic = 1, nc
      m(ic) = sum( mask(:,ic) ) / real( nxyzs )
    enddo ! ic
    m(0) = sum( m(1:nc) )
    call MPIallsum( m(0:), g%comm ) ! reduce over the cartesian grid communicator
    w_constraints(1:nc) = m(1:nc) ! volume of each mask
#ifdef DEBUG
    if(o>0) write(o,'(3A,ES10.2,A,99(" ",F0.6))') sym, fun, 'space  total 1+', (m(0)-1.), ' each', m(1:)
    do is = 1, ns
      if(o>0) write(o,'(3A,F10.6,A,99(" ",F0.6))') sym, fun, 'charge total   ', sum(q_constraints), &
                                                                    ' each', q_constraints(1:nc,is)
    enddo ! is
    do ic = 1, nc
      cow(1:3,ic) = cow(1:3,ic)/cow(0,ic)
      if(o>0) write(o,'(3a,i0,a,f0.6,a,9(" ",f0.6))') sym, fun, 'mask #',ic,' has volume ', &
          w_constraints(ic),' centered around',cow(1:3,ic)*Ang
    enddo ! ic
    if(o>0) write(o,'(9A)') sym, fun, 'created'
#endif
    ist = 0
  endfunction ! init_mask

#ifdef EXTENDED
!+ extended

  status_t function test( )
    write(*,fmt=*,iostat=test) __FILE__,': create an example of the constrained DFT file:'
    write(*,'(A)',iostat=test) '','filename: <project>.cDFT','','',' 1.0   ## spread of the masks in Bohr', &
     ' off  ## on --> apply constraints, off --> show weights only', &
     ' 1  3  12.0  0.0     ## mask number 1: atoms #1, #2 and #3 have 12 electrons',&
     ' 4  5   8.0 -1.5     ## mask number 2: atoms #4 and #5 host 8 electrons and magnetization -1.5','',''
  endfunction ! test

!- extended
#endif

endmodule ! constraints


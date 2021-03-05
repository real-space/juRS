#include "config.h"

!#define DEBUG
! #define FULL_DEBUG


!! @author Paul Baumeister, Andrea Nobile

!! Andrea Nobile 5.2014 removed unused code for weinert shape compensators
!! localized charge distribution for the compensation of missing multipole moments
module type_comp
#ifdef DEBUG
  use configuration, only: o ! output unit, 0: no output
#endif
implicit none
  private ! default visibility for the module namespace
  character(len=*), parameter, private :: sym = 'tCOMP' !! module symbol

  public :: comp
  public :: comp_set
  public :: comp_normalize
  public :: comp_free
  public :: project
  public :: add

  !public :: comp_set_old
  !public :: generate_cmp_vH

#ifdef EXTENDED
  public :: test
#endif

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! TYPE COMP  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  type :: comp !! localized compensation charge densities for (ellmax+1)^2 multipole moments
    integer          :: ellmax = -1 !! highest angular momentum quantum number
    integer          :: npi = 0  !! number of periodic images
    integer          :: noe = 0  !! number of elements (grid points)
    integer          :: ise(3,2,27) = 0 !! start and end indices (internal) of each image
    integer          :: ng(3) = 0 !! numbers of grid points in the cartesian grid (internal)
    real, _allocatable_    :: val(:,:) !! val(mlm,noe) matrix of values ! todo : do we need pointers here?
    real, _allocatable_    :: nrm(:) !! nrm(mlm) normalisation          ! todo : do we need pointers here?
  endtype ! comp

  interface add
    module procedure add_comp
  endinterface

  interface project
    module procedure prj_comp
  endinterface

#ifndef DEBUG
  iounit_t, parameter, private :: o = 0
  character(len=*), parameter, private :: fun = ': '
#endif

  contains

  !Andrea Nobile 4.2014 modified to use spline and removed DG
  ! creates a localized set of functions for the compensation charges
  type(comp) function comp_set( ellmax, rcut, s, origins, hg, ng, &
    interpolation, meshrefinement, derive2i, checkmode ) result( cmp )
  
  use harmonics, only: Xlmax_rl, d_Xlmax_rl_dri, ELLMAX_IMPLEMENTED
  use constants, only: Y00 => ONESQRTFOURPI
  use configuration, only: WARNING
  use type_species, only: species
  use type_spline, only: spline_eval_and_derive
    integer, intent(in)           :: ellmax !! ell-cutoff
    real, intent(in)              :: rcut   !! radial cutoff
    type(species), intent(in)     :: s  !! atom specie
    real, intent(in)              :: origins(1:,1:) !! (3,nperiimages) origins
    real, intent(in)              :: hg(3)        !! grid spacing of the cartesian grid
    integer, intent(in)           :: ng(3)        !! number of grid points in the cartesian grid
    integer, intent(in), optional :: interpolation, meshrefinement !! double grid setup params
    integer, intent(in), optional :: derive2i !! compensation charge derived w.r.t. its center position
    logical, intent(in), optional :: checkmode 

    integer           :: i1, i2, i3, id, ioe, noe, iof, nn(3)
    integer           :: ipi, ider
    real              :: r2s, rv(3), r2v(3)!, rs
    real              :: Xlm(1:(ellmax+1)**2) ! real spherical harmonics 
    real              :: dXlm(1:(ellmax+1)**2) ! derived real spherical harmonics
    integer           :: ilm, ell, emm
    status_t          :: ist    
    real              :: rcut2, radialpart, dradialpart

    character(len=*), parameter :: fun = ' comp_set: '
    
    ider = 0 ; if( present( derive2i ) ) ider = derive2i

    if( ellmax < 0 ) stop 'tCOMP comp_set: ELLMAX < 0 unphysical.'
    if( ellmax > ELLMAX_IMPLEMENTED ) stop 'tCOMP comp_set: ELLMAX > IMPLEMENTED.'

    cmp%ellmax = ellmax ! init

    ! set the number of periodic images
    cmp%npi = size(origins,2)
    if( cmp%npi > size(cmp%ise,3) ) then
      if(o>0) write(o,'(9A)') sym, fun, WARNING(0), 'number of periodic images truncated to 27!'
    endif ! truncated

    cmp%ng = ng ! for later checks, if the array has the correct size

    rcut2 = rcut*rcut 

    if( _allocated_(cmp%val) ) deallocate(cmp%val, stat=ist)

    cmp%noe = 0 ! init
    do ipi = 1, cmp%npi
      do id = 1, 3
        cmp%ise(id,1,ipi) = max( nint( ( origins(id,ipi)-rcut )/hg(id)-0.5 ),      1 )
        cmp%ise(id,2,ipi) = min( nint( ( origins(id,ipi)+rcut )/hg(id)+0.5 ), ng(id) )
      enddo ! id
      noe = product( max(0,cmp%ise(1:3,2,ipi)-cmp%ise(1:3,1,ipi)+1) )
      cmp%noe = cmp%noe + noe
    enddo ! ipi

    
    if( present(checkmode) ) then ; if( checkmode ) return ; endif ! Checkmode

    allocate( cmp%val( (cmp%ellmax+1)**2 ,cmp%noe), stat=ist )
    if( ist /= 0 ) stop 'tCOMP comp_set: allocation of cmp%VAL failed.'

    cmp%val = 0.

    allocate( cmp%nrm( (cmp%ellmax+1)**2), stat=ist )
    if( ist /= 0 ) stop 'tCOMP comp_set: allocation of cmp%NRM failed.'
    cmp%nrm = 0.

    iof = 0 ! init counter offset for elements


    do ipi = 1, cmp%npi ! for each periodic image

      nn = cmp%ise(:,2,ipi)-cmp%ise(:,1,ipi)+1

      do i3 =     cmp%ise(3,1,ipi), cmp%ise(3,2,ipi)
        do i2 =   cmp%ise(2,1,ipi), cmp%ise(2,2,ipi)
          do i1 = cmp%ise(1,1,ipi), cmp%ise(1,2,ipi)

            ioe = iof + 1 + (i1-cmp%ise(1,1,ipi)) + nn(1) * (i2-cmp%ise(2,1,ipi)) + nn(1) * nn(2) * (i3-cmp%ise(3,1,ipi))
            cmp%val(:,ioe) = 0. ! init

            rv(3) = i3*hg(3) - origins(3,ipi) 
            r2v(3) = rv(3)*rv(3)
          
            rv(2) = i2*hg(2) - origins(2,ipi)
            r2v(2) = rv(2)*rv(2)

            rv(1) = i1*hg(1) - origins(1,ipi)
            r2v(1) = rv(1)*rv(1)

            r2s = r2v(3) + r2v(2) + r2v(1)
            
            ! inside the sphere of radius rcut
            if( r2s < rcut2 ) then 
            
              Xlm = Xlmax_rl( ellmax, v=rv )
              ilm = 0
              do ell = 0, ellmax         
                !radialpart = spline_eval(s%rcmp_spline(ell), sqrt(r2s))          
                call spline_eval_and_derive( s%rcmp_spline(ell), sqrt(r2s), radialpart, dradialpart )
              
                selectcase( ider )
                case( 0 )
                  ! underived form
                  do emm = -ell, ell
                    ilm = ilm+1
                    cmp%val(ilm,ioe) = cmp%val(ilm,ioe) + Xlm(ilm)*radialpart
                  enddo ! emm

                case( 1, 2, 3 )
                  ! get the derived Ylm*r^l coefficients for all ell-channels
                    
                  dXlm = 0
                  if (ellmax < 5) &
                  dXlm = d_Xlmax_rl_dri( ellmax, rv, derive2i=ider )
               
                  do emm = -ell, ell
                    ilm = ilm+1
                    cmp%val(ilm,ioe) = cmp%val(ilm,ioe) + Xlm(ilm)*dradialpart*rv(ider)/max( sqrt(r2s), 1E-12 ) + dXlm(ilm)*radialpart
                  enddo ! emm
 
                !dddg(1:,k1,k2,k3) = dXlm(ind_ilm(:)) *  rfval(ind_iln(:)) + &
                !                     Xlm(ind_ilm(:)) * drfval(ind_iln(:)) * rv(ider)/max( rs, 1E-12 )
                endselect ! ider
              enddo
            endif ! r^2 < rcut^2

            cmp%nrm = cmp%nrm + cmp%val(:,ioe)

          enddo ! i1
        enddo ! i2
      enddo ! i3


      noe = product( max(0,cmp%ise(1:3,2,ipi)-cmp%ise(1:3,1,ipi)+1) )
      iof = iof + noe ! forward offset

    enddo ! ipi

    ! multiply the dV for a grid point element
    cmp%nrm = cmp%nrm * product(hg(1:3)) * Y00

  endfunction ! comp_set


   !! deallocate the memory of this localized function
  elemental subroutine comp_free( cmp )
    type(comp), intent(inout) :: cmp
    status_t :: ist
    deallocate( cmp%val, cmp%nrm, stat=ist )
    cmp%noe = 0 ; cmp%ise = 0 ; cmp%ng = 0 ; cmp%ellmax = -1 ; cmp%npi = 0
  endsubroutine ! comp_free



 
  !! orthogonalize the higher lm compensators q_lm(r) to q_00(r)
  !! such that these don''t show any monopole moment
  !! and renormalize q_00(r) to unity
  status_t function comp_normalize( cmp, comm, owner, list, unity, hg ) result( ist )
  use configuration, only: WARNING, ERROR
  use constants, only: Y00 => ONESQRTFOURPI
  use atomicomm, only: AtomReduce, AtomBcast
    type(comp), intent(inout)             :: cmp !! compensator
    integer, intent(in)                   :: comm !! MPI communicator
    integer, intent(in)                   :: owner !! atom owner rank
    integer, intent(in)                   :: list(:) !! rank contribution list
    real, intent(in), optional            :: unity !! atomic symmetryfactor, default=1
    real, intent(in), optional            :: hg(3) !! grid spacings, if present, the norm will be recomputed

    character(len=*), parameter           :: fun = ' comp_normalize: '
    real, parameter                       :: THRES_WARN = 1E-9
    integer                               :: ioe
    real                                  :: nrm(1:(cmp%ellmax+1)**2), one, q00inv

    ist = 0
    one = 1.0 ; if( present( unity ) ) one = unity

    if( _allocated_( cmp%nrm ) ) then
      ! make sure, the part of the compensator in this domain is non-zero
      nrm = cmp%nrm
      deallocate( cmp%nrm, stat=ist ) ! not needed any more
    else  ! allocated
      nrm = 0.
    endif ! allocated

    call AtomReduce( nrm, comm, owner, list )
    call AtomBcast ( nrm, comm, owner, list )


#ifdef DEBUG
    if(o>0) write(o,'(3A,99ES16.6)') sym, fun, 'integral q_lm =', nrm
#endif
    ! Warnings
    if( abs( nrm(1)-one )/one > THRES_WARN .and. o>0 ) &
      write(o,'(4A,F6.3,A,ES8.1)') sym, fun, WARNING(0), 'deviation from unity, q_00 =', one, ' +', nrm(1)-one
    if( any( abs( nrm(2:) )/one > THRES_WARN ) .and. o>0 ) &
      write(o,'(4A,99ES10.2)') sym, fun, WARNING(0), 'deviation of q_lm (lm>00) =', nrm(2:)
    if( nrm(1) < one*0.9 .or. nrm(1) > one*1.1 ) then
      write(*,'(4A,F0.9,99ES10.2)') sym, fun, ERROR, 'deviation of q_lm = ', nrm(1:)
      stop 'tCOMP comp_normalize: ERROR! the 1-norm of q_00(r) deviates more than 10%.'
    endif ! norm deviates to strongly



    q00inv = one/nrm(1)

#ifdef DEBUG
    if(o>0) write(o,'(3A,F20.16,9A)') sym, fun, 'renormalize q_00 with', q00inv, ' and adjust q_lm by q_00 for lm > 00.'
#endif

    ! renormalize q_00 and orthogonalize the q_lm (lm>00) to q_00
    do ioe = 1, cmp%noe ! grid elements
      cmp%val(1 ,ioe) = cmp%val(1 ,ioe) * q00inv
      cmp%val(2:,ioe) = cmp%val(2:,ioe) - nrm(2:)*cmp%val(1,ioe)
    enddo ! ioe

#ifdef DEBUG
    if( present(hg) ) then ! compute norm again
      nrm = 0.
      do ioe = 1, cmp%noe ! grid elements
        nrm = nrm + cmp%val(:,ioe)
      enddo ! ioe
      nrm = nrm*product(hg(1:3))*Y00
      nrm(1) = nrm(1)-one
      if(o>0) write(o,'(3A,F6.3,A,ES8.1,99ES10.2)') sym, fun, 'new integral q_lm =', one, '+', nrm(1:)
    endif ! present(hg)
#endif
  endfunction ! comp_normalize




  subroutine add_comp( cmp, qlm, rho )
    type(comp), intent(in) :: cmp            ! compensator
    real, intent(in)       :: qlm(1:)        ! coeffs       q_lm
    real, intent(inout)    :: rho(1:,1:,1:)  ! function     rho_aug

    character(len=*), parameter :: fun = ' add_comp: '
    integer :: ix, iy, iz, ioe, ii, mlm
    mlm = (cmp%ellmax+1)**2
#ifdef DEBUG
    if( size(qlm) < mlm ) stop 'tCOMP add_comp: dim of QLM is not correct!'
    if( any( shape(rho) /= cmp%ng(1:3) ) ) stop 'tCOMP add_comp: a dim of RHO is not correct!'
#endif

    ioe = 0
    do ii = 1, cmp%npi ! number of periodic images
      do iz =     cmp%ise(3,1,ii), cmp%ise(3,2,ii)
        do iy =   cmp%ise(2,1,ii), cmp%ise(2,2,ii)
          do ix = cmp%ise(1,1,ii), cmp%ise(1,2,ii)
            ioe = ioe+1
            rho(ix,iy,iz) = rho(ix,iy,iz) + dot_product( qlm(1:mlm), cmp%val(1:mlm,ioe) )
          enddo ! ix
        enddo ! iy
      enddo ! iz
    enddo ! ii
  endsubroutine ! add_comp

  subroutine prj_comp( cmp, Ves, c, dV )
    type(comp), intent(in)     :: cmp             ! comp         
    real, intent(in)           :: Ves(1:,1:,1:)   ! function     electrostatic potential
    real, intent(out)          :: c(1:)           ! coeffs
    real, intent(in), optional :: dV              ! volume element

    integer :: ix, iy, iz, ioe, ii, mlm
    real :: dVol

    dVol = 1.0 ; if(present(dV)) dVol = dV
    mlm = (cmp%ellmax+1)**2
! #ifdef DEBUG
    if( size(c) < mlm ) stop 'tCOMP prj_comp: dim of UHLM is not correct!'
    if( any( shape(Ves) /= cmp%ng(1:3) ) ) stop 'tCOMP prj_comp: a dim of VES is not correct!'
! #endif

    c = 0. ! init
    ioe = 0
    do ii = 1, cmp%npi ! number of periodic images

      do iz =     cmp%ise(3,1,ii), cmp%ise(3,2,ii)
        do iy =   cmp%ise(2,1,ii), cmp%ise(2,2,ii)
          do ix = cmp%ise(1,1,ii), cmp%ise(1,2,ii)
            ioe = ioe+1
            c(1:mlm) = c(1:mlm) + Ves(ix,iy,iz) * cmp%val(1:,ioe)
          enddo ! ix
        enddo ! iy
      enddo ! iz

    enddo ! ii
    c = c * dVol
#ifdef FULL_DEBUG
    if(o>0) write(o,'(2A,99F10.6)') sym, ' prj_comp: vlm =', c(1:mlm)
#endif
  endsubroutine ! prj_comp




#ifdef EXTENDED
  status_t function test( ) result( ios )
    write(*,*,iostat=ios) sym, ' no module test implemented!'
  endfunction ! test
#endif


endmodule ! type_comp

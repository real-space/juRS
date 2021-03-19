#include "config.h"


#ifdef R1_C2

#if R1_C2 == 1
!+ r1_c2 == 1

!! @author Paul Baumeister
!! @version 4.0
!!
!! non-local projector function on a set of real-space grid points
module type_proj


#ifdef DEBUG
  use configuration, only: o ! output unit, 0: no output
#endif

implicit none

  private ! default for this module namespace
#ifndef DEBUG
  iounit_t, parameter, private :: o = 0 ! 0:no output
#endif

  character(len=*), parameter, private :: sym = 'tPROJ' !! module symbol


  public :: proj ! public type name
  public :: proj_set
  public :: proj_rescale
  public :: proj_free
  public :: project
  public :: add
  public :: Bloch_phase
#ifdef EXTENDED
  public :: test
#endif

  integer, parameter, public :: DEFAULT_MESHREFINEMENT = 3
  integer, parameter, public :: DEFAULT_INTERPOLATION  = 2

  interface project
    module procedure project_1_r, project_1_c, project_n_r, project_n_c
  endinterface

  interface add
    module procedure add_1_r, add_1_c, add_n_r, add_n_c
  endinterface



  integer, parameter, private :: MAXNPI=27 ! max number of projector images
  integer, parameter, private :: IS=1, IE=2 ! start and end index


  type, private :: prjp !! projector part
    integer          :: noe = 0  ! number of real-space elements
    real,    allocatable :: val(:,:) ! val(nop,noe) ! values
    integer, allocatable :: ind(:)   ! ind(noe) ! total indices of grid points
    integer          :: iphase(1:3)=0 ! projector part phase shift eikL^iphase
    integer          :: iimage   ! index in the lists of periodic positions
  endtype ! prjp


  type :: proj !! a set of projectors
    integer          :: nop = 0  ! number of projector functions
    integer          :: nxyz = 0  ! max. total index of gridpoints
    integer          :: npi = 0  ! number of periodic images
    integer          :: ider = 0 ! ider=0: regular, ider in {1,2,3} derived
    type(prjp)       :: p(MAXNPI)    ! projector parts
  endtype ! proj


 ! this module provides the nonlocal operator |p> <p|,
 ! by the three module procedures
 !       - project:
 !             Find the coefficient vector c = <p|f>  (local)
 !       - reduce:
 !             Transport the information via the atomic communicator
 !              --> all members to the atom owner ("reduce")
 !              --> all members to all members ("allreduce")
 !              --> the atom owner to all members ("bcast")
 !       - add:
 !             |f> := |f> + c * |p> (local)


  contains



  !! computes the complex Bloch phase factor
  !! depending on the shift of the atoms.
  !! |eik| == 1 is NOT assumed.
  !!
  complex function Bloch_phase( eik, ishift ) result( ph )
    complex, intent(in) :: eik(3)
    integer, intent(in) :: ishift(3)
    integer             :: id

    ph = (1.0,0.0) ! unity

    do id = 1, 3
      selectcase( ishift(id) ) ! special treatment for ishift in [-2,2]
      case(  0 ) ! do nothing
      case(  1 ) ; ph = ph * eik(id)
      case( -1 ) ; ph = ph / eik(id)
      case(  2 ) ; ph = ph * ( eik(id)*eik(id) )
      case( -2 ) ; ph = ph / ( eik(id)*eik(id) )
      case default ; ph = ph * eik(id)**ishift(id)
      endselect ! ishift(id)
    enddo ! id
    !do id = 1, 3
    !  ph = ph*eik(id)**(real(ishift(id)))
    !enddo ! id

  endfunction Bloch_phase


#ifdef DEBUG
!!! remove comment from debug line
!!! ==> all debug lines will be included
#define cDBG
#else
!!! comment lines with !DBG
!!! ==> no debug lines will be included
#define cDBG !DBG
#endif

  ! computes Lagrange interpolants h(j) is l_j(xi) h(j)[x_i] = delta_{ij}
  subroutine lagrange_eval(xi,n,xpts,h)
  implicit none
  integer, intent(in) :: n
  real, intent(in)   ::  xi,xpts(n)
  real, intent(out)  ::  h(n)

  integer dgr,i,j
  real prod1,prod2

  do dgr=1,n

    prod1 = 1.0d0
    prod2 = 1.0d0
    do i=1,n
      if(i /= dgr) then
        prod1 = prod1*(xi-xpts(i))
        prod2 = prod2*(xpts(dgr)-xpts(i))
      endif
    enddo
    h(dgr)=prod1/prod2

  enddo

  end subroutine lagrange_eval

  subroutine lagrange_quadratic_centered(inarr, outarr)
    real, intent(in)          :: inarr(3, 3, 3)
    real, intent(out)         :: outarr(3, 3, 3)

    integer :: ix, iy, iz, kx, ky, kz
    real    :: lx, ly, lz
    real    :: hx(3), hy(3), hz(3)
    real    :: xpts(3)

    xpts(1) = -1.
    xpts(2) = 0.
    xpts(3) = 1.

     do ix = 1,3
      lx = real(ix-2)*(1./3.)
      call lagrange_eval(lx, 3, xpts, hx)
      do iy = 1,3
        ly = real(iy-2)*(1./3.)
        call lagrange_eval(ly, 3, xpts, hy)
        do iz = 1,3
          lz = real(iz-2)*(1./3.)
          call lagrange_eval(lz, 3, xpts, hz)
          outarr(ix, iy, iz) = 0.
          do kx=1, 3
            do ky=1, 3
              do kz=1, 3
                outarr(ix, iy, iz) = outarr(ix, iy, iz) + &
                  hx(kx)*hy(ky)*hz(kz)*inarr(kx, ky, kz)
              enddo !kz
            enddo !ky
          enddo !kx
      
        enddo ! iz
      enddo  !iy
    enddo !ix

 
  endsubroutine

  subroutine lagrange_quartic_centered(inarr, outarr)
    real, intent(in)          :: inarr(5, 5, 5)
    real, intent(out)         :: outarr(3, 3, 3)

    integer :: ix, iy, iz, kx, ky, kz
    real    :: lx, ly, lz
    real    :: hx(5), hy(5), hz(5)
    real    :: xpts(5)

    xpts(1) = -2.
    xpts(2) = -1.
    xpts(3) = 0.
    xpts(4) = 1.
    xpts(5) = 2.

     do ix = 1,3
      lx = real(ix-2)*(1./3.)
      call lagrange_eval(lx, 5, xpts, hx)
      do iy = 1,3
        ly = real(iy-2)*(1./3.)
        call lagrange_eval(ly, 5, xpts, hy)
        do iz = 1,3
          lz = real(iz-2)*(1./3.)
          call lagrange_eval(lz, 5, xpts, hz)
          outarr(ix, iy, iz) = 0.
          do kx=1, 5
            do ky=1, 5
              do kz=1, 5
                outarr(ix, iy, iz) = outarr(ix, iy, iz) + &
                  hx(kx)*hy(ky)*hz(kz)*inarr(kx, ky, kz)
              enddo !kz
            enddo !ky
          enddo !kx
      
        enddo ! iz
      enddo  !iy
    enddo !ix

 
  endsubroutine

!!#define USE_DOUBLE_GRID  
#ifdef USE_DOUBLE_GRID
  type(proj) function proj_set( bf, origins, offset, iphase, hg, ng, &
                                interpolation, meshrefinement, &
                                derive2i, checkmode ) & ! optionals
  result( p )
  ! This routine makes use of the double grid technique
  use harmonics, only: Xlmax_rl, d_Xlmax_rl_dri

  use configuration, only: ERROR
  use type_bfun, only: bfun, operator(.at.), operator(.derat.)
   
    character(len=*), parameter       :: fun = ' proj_set: '
    
    type(bfun), intent(in)            :: bf(:) ! j0-transformed localized projector fuctions
    real, intent(in)                  :: origins(1:,:) ! origin w.r.t the origin(i=0) of the coarse grid
    real, intent(in)                  :: offset(1:3) ! origin of the coarse grid
    integer, intent(in)               :: iphase(1:,:) !! integer phase shifts for this image
    real, intent(in)                  :: hg(1:3) ! grid spacing
    integer, intent(in)               :: ng(1:3) ! number of grid points
    
    integer, intent(in), optional     :: interpolation ! ==> itp
    integer, intent(in), optional     :: meshrefinement ! ==> nmesh
    integer, intent(in), optional     :: derive2i ! ==> ider
    logical, intent(in), optional     :: checkmode

    real                              :: rcut, rcut2 ! cutoff radius
    integer                           :: mem
    status_t                          :: ist ! status flag for allocations
    integer                           :: id  ! counter for spatial directions 1:x, 2:y, 3:z
    real                              :: rv(3), rv2(3), r2s, rs ! vector and distances
    integer                           :: ellmax, ell, emm!, enn ! quantum numbers
    integer                           :: iln, ilnm, ilm, mln, mlnm ! combindices
    integer                           :: kse(1:3,IS:IE) ! start and end index of the accessed dense grid
    integer                           :: gse(1:3,IS:IE) ! start and end index of the non-zero dense grid
    integer                           :: ise(1:3,IS:IE) ! start and end index of the coarse grid
    integer                           :: i1, i2, i3 ! coarse grid loop indices
    integer                           :: k1, k2, k3 ! dense grid loop indices
    real(4), allocatable              :: dddg(:,:,:,:) ! (0:mlnm,dense,dense,dense) grid array dddg(0:mlnm,gse)
    logical(1), allocatable           :: dddg_nonzero(:,:,:) ! dddg_nonzero(kse)
    logical(1)                        :: contr
    integer                           :: j1, j2, j3 ! interpolation loop indices
    integer                           :: itp, nmesh, itpnmo ! = itp*nmesh-1
    real, allocatable                 :: w8s(:,:) ! weights for reversed inverpolation w8s(-itpnmo:itpnmo,1:3)
    real                              :: w8xyz ! product of 3 weights
    integer                           :: ider = 0 ! derivative? 0:none, 1:d/dx, 2:d/dy, 3:d/dz
    real, allocatable                 :: Xlm(:), dXlm(:) ! (mlm) ! spherical harmonics times r^ell
    real, allocatable                 :: rfval(:), drfval(:) ! (mln) ! value of bfuns at rs
    real                              :: hdg(3) ! grid spacing of the double grid
    real                              :: ori(3) ! origin on the double grid
    integer, allocatable              :: ind_iln(:), ind_ilm(:) ! index lists
    integer                           :: ii, ipi!, npi ! indicies of periodic images
    integer                           :: ioe, noe
    integer, allocatable              :: ind_tot(:) ! list for total indices of coarse grid points
    integer, allocatable              :: ind_123(:,:) ! list for cartesian 3 indices of coarse grid points

    real                              :: tarr(5, 5, 5) 
    !real                              :: interp_values(3, 3, 3)
    real                              :: interp_values_multi(3, 3, 3, 5, 5, 5)

    integer                           :: kx, ky, kz , jx, jy, jz
    p%nop = 0 ! result for early return

    ! cutoff radius    
    rcut = maxval( bf(:)%rcut )
    rcut2 = rcut*rcut

    ! derived localized function? 0:no or derive to 1:x, 2:y, 3:z
    ider = 0 ; if( present( derive2i ) ) ider = derive2i
    
    nmesh = 3
    itp = 3

    !nmesh = 3
    !itp = 3
    itpnmo = itp*nmesh-1 

    ! cartesian grid and grid spacings
    hdg = hg/real(nmesh) ! grid spacing of the denser DoubleGrid

    ! determine total number of functions and total number of projectors
    mln = size(bf)
    ellmax = -1 ! init
    mlnm = 0 ! counter for number of projectors
    do iln = 1, mln
      ell = bf(iln)%ell
      ellmax = max( ellmax, ell )
      mlnm = mlnm + (2*ell+1)
    enddo ! iln
    p%nop = mlnm ! total number of projectors

    allocate(  rfval(mln),  Xlm(1:(ellmax+1)**2), stat=ist ) ; if( ist /= 0 ) stop 'tPROJ proj_set: failed to allocate YLM'
    if( ider > 0 ) &
    allocate( drfval(mln), dXlm(1:(ellmax+1)**2), stat=ist ) ; if( ist /= 0 ) stop 'tPROJ proj_set: failed to allocate dYLM'
    ! lists of ilm and iln indices
    allocate( ind_ilm(mlnm), ind_iln(mlnm), stat=ist ) ; if( ist /= 0 ) stop 'tPROJ proj_set: failed to allocate IND_ILM and IND_ILN'
   

    ! prepare index tables
    
    ilnm = 0
    do iln = 1, size(bf)
      ell = bf(iln)%ell
      ilm = ell*ell ! offset
      do emm = -ell, ell
        ilnm = ilnm+1
        ind_iln(ilnm) = iln
        ilm = ilm+1
        ind_ilm(ilnm) = ilm
      enddo ! emm
    enddo ! iln

    
    ! loop over periodic images of the atom (origins)

    ii = 1 ! init as 1 !!! the counter ii is incremented at the end if 
                       !!! the part got nonzero elements, i.e. p%noe > 0
    write(*,*) 'number of periodic images = ', size(origins,2)
    do ipi = 1, size(origins,2) ! for each periodic image

      if( ii > MAXNPI ) stop 'tPROJ proj_set: number of periodic images has been truncated to MAXNPI!'

      ! ori is the position of the radial center relative to the double grid points
      ori(1:3) = origins(1:3,ipi)-offset(1:3) ! no shift
      p%p(ii)%iphase = iphase(1:3,ipi) ! set integer phase information
      p%p(ii)%iimage = ii ! set number of periodic image


      do id = 1, 3 ! dimension index (x, y, z)
        ! example: meshrefinment = 2
        !    0   1   2   3   4   5   6   7   8    index k
        !  --x---x---x---x---x---x---x---x---x--- dense  grid
        !    |       |       |       |       |
        !  --origin--o-------o-------o-------o-- coarse grid
        !    0       1       2       3       4   index i
        !    |  hg   | grid spacing


        ! xc(i) = i*hg - origin
        ! xd(k) = (k)*hg/nmesh - origin = k*hg/nmesh - ori
        !

        ! now find k, such that xd(k) == origin + rcut
        ! <==> k = (origin+rcut)/hg*nmesh 

        ! start and end index of the non-zero localized function on the dense grid
        gse(id,IS) = floor  ( ( ori(id)-rcut )/hdg(id) )
        gse(id,IE) = ceiling( ( ori(id)+rcut )/hdg(id) )

        ! start and end index of the non-zero localized function on the coarse grid:
        ! the interpolation scheme goes from a coarse grid point
        ! (itp*nmesh-1) dense grid steps to the right (negative)
        ! and itp*nmesh dense grid steps to the left (positive)
        ! ==> i*nmesh - (itp*nmesh-1) = k
        ! ==> i = floor( ( k + (itp*nmesh-1))/nmesh )
        ise(id,IS) = floor  ( (gse(id,IS)-itpnmo-1)/real(nmesh) )
        ise(id,IE) = ceiling( (gse(id,IE)+itpnmo+1)/real(nmesh) )
       
        ! start and end index of the double grid array. these are larger than the gse
        kse(id,IS) = nmesh*ise(id,IS)-itpnmo
        kse(id,IE) = nmesh*ise(id,IE)+itpnmo
      
         ! now cut off everything that exceeds the grid limits (of this domain)
        ise(id,IS) = max( ise(id,IS),      1 )
        ise(id,IE) = min( ise(id,IE), ng(id) )

        ! example nmesh = 2
  !        situation 1: no cell borders intersecting the spheres, itp=1 ==> 4 nonzero weights
  !
  !        Atomic position               rcut         rcut+2*itp*h_coarse
  !        |>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>|0000000000000000000|
  !        ---x---x---x---x---x---x---x---x---x---x---x---x---x---x double grid
  !                                       g\                k /    k = g + 2*itp*nmesh
  !                                         \                /
  !                                          \              /
  !                                           \            /
  !                                            \          /
  !                                             \        /
  !                                              \      /
  !                                               \i* i/        i = min( ceiling(i*), ng ) = ceiling(i*)
  !        ---O-------O-------O-------O-------O-------O-------O coarse grid
  !
        !
  !        situation 2: cell borders intersecting the spheres, itp=1 ==> 4 nonzero weights
  !
  !        Atomic position               rcut         rcut+2*itp*h_coarse
  !        |>>>>>>>>>>>>>>>>>>>>>>>>>>>>>|0000000000000000000|
  !        -x---x---x---x---x---x---x---x---x---x---x---x---x---x double grid
  !                       | k /         g\                      k = i + itp*nmesh
  !                     ce|ll/            \
  !                    bor|der             \
  !                       |/                \
  !                       |                  \
  !                      /|                   \
  !                     / |                    \
  !                   i/  |                     \i*             i = min( ceiling(i*), ng ) = ng
  !        ---O-------O---|---O-------O-------O-------O-------O coarse grid
  !                  ng   |
  !
  !         ! the cutoff radius (after double grid) may not be larger then the smallest
  !         ! extend of the supercell.
  !         ! this is crucial for small cells, because usually,
  !         ! only one neighboring periodic image is taken into account
  !         !     (1 image ==> np_max=3**3=27, 2 images ==> np_max=5**3=125)
  !         !
  !         !          prev. cell       supercell        next cell
  !         !     -1.5             -.5      x       .5             1.5   (x=cell origin)
  !         !    ===|===============|===============|===============|==
  !         !           |<<<<<<<<<<<<<<<A>>>>>>>>>>>>>>>|             (A=a_{0}=atomic pos.)
  !         !  <<<<<<<<<a>>>>>>>>>>>>>>>|                                  (a_{-1}) left pI
  !         !                           |<<<<<<<<<<<<<<<a>>>>>>>>>>>>>>>|  (a_{1}) right pI
  !         !                                                        (pI = periodic images)
  !         ! ==> 3 |lfun>s will be set up:
  !         !                           supercell
  !         !                       |===============|
  !         !                       <<<<A>>>>>>>>>>>>     |lfun_{ 0}> center
  !         !                       >>>>|                 |lfun_{-1}>   left
  !         !                           |<<<<<<<<<<<<     |lfun_{+1}>  right
  !         !
  !         ! if the effective rcut (including interpolation) were larger
  !         ! than the supercell is wide, |lfun_{-2}> or |lfun_{2}> would be truncated.



        ! gse == kse would let the computations run over the entire dg array
        ! cut off the index range for the double grid, where a boundary has cut away the i parts
        gse(id,IS) = max( gse(id,IS), kse(id,IS) )
        gse(id,IE) = min( gse(id,IE), kse(id,IE) )

        gse(id, : ) = kse(id, :)

        
      enddo ! id
    
      noe = product( max( 0, ise(1:3,IE)-ise(1:3,IS)+1 ) )
      if( noe < 1 ) then
        cycle ! this image does not contribute
      endif ! noe < 1

      p%p(ii)%noe = noe


      if( present( checkmode ) ) then ; if( checkmode ) return ; endif ! Checkmode


      ! i*nmesh = k
      ! Example: nmesh = 2

      ! --x---x---x---x---x---x---x---x-- dense  grid
      !   1   2   3   4   5   6   7   8   index k
      ! ------o-------o-------o-------o-- coarse grid
      !       1       2       3       4   index i

      ! stores in the ranges kse if there are non-zero projector values,
      ! especially in the range where kse is larger than gse, dg_nonzero is false
      allocate( dddg_nonzero(kse(1,IS):kse(1,IE),kse(2,IS):kse(2,IE),kse(3,IS):kse(3,IE)), stat=ist )
      ! allocate the double grid   [[ dddg(0,:,:,:)-index 0 is the radius ]]
      allocate( dddg(0:mlnm,gse(1,IS):gse(1,IE),gse(2,IS):gse(2,IE),gse(3,IS):gse(3,IE)), stat=ist )
      if( ist /= 0 ) then
        mem = 4*(1+mlnm)*product(gse(1:3,IE)-gse(1:3,IS)+1) + product(kse(1:3,IE)-kse(1:3,IS)+1)
        write(*,'(4A,I12,9A)') sym, fun, ERROR, 'allocation of DoubleGrid (', mem, ' Bytes) failed!'
        stop 'tPROJ proj_set: allocation of double grid array DoubleGrid failed!'
      endif ! ist /= 0
      
      dddg = 0. ! init
      dddg_nonzero = .false. ! init mask as false


      ! prepare the localized function on the denser grid dddg 
      do     k3 = gse(3,IS), gse(3,IE) ; rv(3) = k3*hdg(3)-ori(3) ; rv2(3) = rv(3)*rv(3)
        do   k2 = gse(2,IS), gse(2,IE) ; rv(2) = k2*hdg(2)-ori(2) ; rv2(2) = rv(2)*rv(2)
          do k1 = gse(1,IS), gse(1,IE) ; rv(1) = k1*hdg(1)-ori(1) ; rv2(1) = rv(1)*rv(1)

            r2s = rv2(1) + rv2(2) + rv2(3)
            if( r2s < rcut2 ) then
              dddg_nonzero(k1,k2,k3) = .true. !! mask: nonzero !!

              rs = sqrt( r2s ) ! radius of the grid point

              rfval = bf .at. rs ! eval functions at rs
              !write(*,*) 'got rfval', rfval
              Xlm = Xlmax_rl( ellmax, rv ) ! get the Ylm*r^l coefficients for all ell-channels

              selectcase( ider )
              case( 0 ) 

                ! set the values to radial function * spherical harmonics*r^ell
                dddg(1:,k1,k2,k3) = Xlm(ind_ilm(:)) * rfval(ind_iln(:))

              case( 1, 2, 3 ) ! set the derived values

                drfval = bf .derat. rs ! evaluate the derivative w.r.t. r at rs

                ! get the derived Ylm*r^l coefficients for all ell-channels
                dXlm = d_Xlmax_rl_dri( ellmax, rv, derive2i=ider )

                dddg(1:,k1,k2,k3) = dXlm(ind_ilm(:)) *  rfval(ind_iln(:)) + &
                                     Xlm(ind_ilm(:)) * drfval(ind_iln(:)) * rv(ider)/max( rs, 1E-12 )


              endselect ! ider

            endif ! r^2 < rcut^2

          enddo ! k1
        enddo ! k2
      enddo ! k3


      !============================================================================
      ! Check: run over the coarse grid points and see,
      !        which ones will get a non-zero contribution.
      !============================================================================
      allocate( ind_tot(1:noe), stat=ist ) ; if( ist /= 0 ) stop 'tPROJ proj_set: allocation of IND_TOT failed!'
      allocate( ind_123(1:3,1:noe), stat=ist ) ; if( ist /= 0 ) stop 'tPROJ proj_set: allocation of IND_123 failed!'


      ioe = 0 ! index of element
      do i3 =     ise(3,IS), ise(3,IE)
        do i2 =   ise(2,IS), ise(2,IE)
          do i1 = ise(1,IS), ise(1,IE)


            contr = .false. ! does not contribute ! loops stop, once contr is true
            do k3 =     i3*nmesh-itpnmo, i3*nmesh+itpnmo ; if( contr ) exit
              do k2 =   i2*nmesh-itpnmo, i2*nmesh+itpnmo ; if( contr ) exit
                do k1 = i1*nmesh-itpnmo, i1*nmesh+itpnmo ; if( contr ) exit

                  if( dddg_nonzero(k1,k2,k3) ) contr = .true.

                enddo ! j1
              enddo ! j2
            enddo ! j3

            if( contr ) then ! coarse point i1, i2, i3 does contribute
              ioe = ioe + 1 ! count op
              ind_tot(ioe) = 1 + 1*(i1-1) + ng(1)*(i2-1) + ng(1)*ng(2)*(i3-1) ! total index formula
              ind_123(1:3,ioe) = (/i1,i2,i3/)
            endif 

          enddo ! i1
        enddo ! i2
      enddo ! i3

      noe = ioe ! reduce noe to the number of non-zero coarse grid entries
      p%p(ii)%noe = noe 

      if (allocated(p%p(ii)%ind)) then 
        deallocate( p%p(ii)%ind, stat=ist )
      endif
      allocate( p%p(ii)%ind(1:noe), stat=ist ) ; if( ist /= 0 ) stop 'tPROJ proj_set: allocation of l%IND failed!'
      p%p(ii)%ind(:) = ind_tot(1:noe) ! copy
      deallocate( ind_tot, stat=ist ) ; if( ist /= 0 ) stop 'tPROJ proj_set: deallocation of IND_TOT failed!'
      if (allocated(p%p(ii)%val)) then 
        deallocate( p%p(ii)%val, stat=ist )
      endif
      ! allocate the projector grid values
      allocate( p%p(ii)%val(1:mlnm,1:noe), stat=ist ) ; if( ist /= 0 ) stop 'tPROJ proj_set: allocation of l%VAL failed!'

      
     
      tarr = 0.
      do kz = 1, 5
        do ky = 1, 5
          do kx = 1, 5
            tarr(6-kx, 6-ky, 6-kz) = 1. 
            call lagrange_quartic_centered(tarr, interp_values_multi(1,1,1, kx, ky, kz))
            tarr(6-kx, 6-ky, 6-kz) = 0. 
          enddo
        enddo
      enddo

      do ioe = 1, noe
        p%p(ii)%val(:,ioe) = 0. ! init
        i1 = ind_123(1,ioe) !
        i2 = ind_123(2,ioe) ! load coarse grid indices
        i3 = ind_123(3,ioe) !

        !p%p(ii)%val(:,ioe) = p%p(ii)%val(:,ioe) + &
        !                (1./1.)*dddg(1:, 3*i1, 3*i2, 3*i3) 

          do kz = 1, 5
            do ky = 1, 5
              do kx = 1, 5
                
                !tarr(6-kx, 6-ky, 6-kz) = 1. 
                !call lagrange_quartic_centered(tarr, interp_values)

                do jz = 0, 2
                  do jy = 0, 2
                    do jx = 0, 2

                      p%p(ii)%val(:,ioe) = p%p(ii)%val(:,ioe) + &
                        (1./27.)*dddg(1:, 3*i1-1 + (kx-3)*3+jx, 3*i2-1 + (ky-3)*3 +jy, 3*i3-1 + (kz-3)*3 + jz) * &
                        interp_values_multi(jx+1, jy+1, jz+1, kx, ky, kz)

                    enddo ! jx
                  enddo ! jy
                enddo ! jz
                !tarr(6-kx, 6-ky, 6-kz) = 0. !restore the 0 
                
              enddo !kx  
            enddo  !ky
          enddo  !kz
         !if(any(p%p(ii)%val(:,ioe) /= p%p(ii)%val(:,ioe))) then 
         !   write(*,*) 'NaN '
         !endif 
      enddo ! ioe


      deallocate( dddg, dddg_nonzero, IND_123, stat=ist ) ; if( ist /= 0 ) stop 'tPROJ proj_set: deallocation of DG failed!'

      if( all( p%p(ii)%val == 0. ) ) then
        ! no element differs from zero, so the projector part does not need to be stored.
        deallocate( p%p(ii)%val, stat=ist ) ; if( ist /= 0 ) stop 'tPROJ proj_set: deallocation of empty l%VAL failed.'
        p%p(ii)%noe = 0
      else  
        ii = ii+1 
      endif 


    enddo ! ipi
    p%npi = ii-1 ! ii always points to the projector part to be set next,
                 ! therefore the number of projector parts is 1 less

    if( all( p%p(:)%noe <= 0 ) ) then
      p%npi = 0
    endif ! all projector parts zero

  endfunction !!proj_set

#endif

#if 0
  type(proj) function proj_set_fft( bf, origins, offset, iphase, hg, ng, &
         interpolation, meshrefinement, derive2i, checkmode, jatom ) & ! optionals
  result( p )
  use configuration, only: ERROR
  use harmonics, only: Xlmax_rl, d_Xlmax_rl_dri
  use type_bfun, only: bfun, operator(.at.), operator(.derat.)
    type(bfun), intent(in)            :: bf(:) ! j0-transformed localized projector fuctions
    real, intent(in)                  :: origins(1:,:) ! origin w.r.t the origin(i=0) of the coarse grid
    real, intent(in)                  :: offset(1:3) ! origin of the coarse grid (if index was 0 but actual index starts form 1)
    integer, intent(in)               :: iphase(1:,:) !! integer phase shifts for this image
    real, intent(in)                  :: hg(1:3) ! grid spacing
    integer, intent(in)               :: ng(1:3) ! number of grid points
    integer, intent(in), optional     :: interpolation ! ==> itp
    integer, intent(in), optional     :: meshrefinement ! ==> nmesh
    integer, intent(in), optional     :: derive2i ! ==> ider
    logical, intent(in), optional     :: checkmode
    integer, intent(in), optional     :: jatom ! global atom index

    character(len=*), parameter       :: fun = ' proj_set: '
    real                              :: rcut, rcut2 ! cutoff radius
    integer(kind=8)                   :: mem
    status_t                          :: ist ! status flag for allocations

    real                              :: rv(3), rv2(3), r2s, rs ! vector and distances
    integer                           :: ellmax, ell, emm!, enn ! quantum numbers
    integer                           :: iln, ilnm, ilm, mln, mlnm ! combindices and upper limits
    integer, allocatable              :: ind_iln(:), ind_ilm(:) ! index lists
 
    integer                           :: ise(1:3,IS:IE) ! start and end index of the coarse grid
    integer                           :: i1, i2, i3 ! coarse grid loop indices
  
    integer                           :: ider = 0 ! derivative? 0:none, 1:d/dx, 2:d/dy, 3:d/dz
    real, allocatable                 :: Xlm(:),   dXlm(:)   ! (mlm) ! spherical harmonics times r^ell
    real, allocatable                 :: rfval(:), drfval(:) ! (mln) ! value of bfuns at rs

    real                              :: hdg(3) ! grid spacing of the double grid
    real                              :: ori(3) ! origin on the double grid
    integer                           :: ii, ipi ! indicies of periodic images
    integer                           :: ioe, noe ! index and number of coarse grid element
    integer                           :: boxsize

    complex, allocatable              :: dgbox(:,:,:)

    p%nop = 0 ! result for early return

    rcut = maxval( bf(:)%rcut )
    rcut2 = rcut*rcut

    ! derived localized function? 0:no or derive to 1:x, 2:y, 3:z 
    ider = 0 ; if( present( derive2i ) ) ider = derive2i

    p%nxyz = ng(1)*ng(2)*ng(3) ! this is for a check, if the input array is on the same grid.

  
    ! determine total number of functions and total number of projectors
    mln = size(bf)
    ellmax = -1 ! init
    mlnm = 0 ! counter for number of projectors
    do iln = 1, mln
      ell = bf(iln)%ell
      ellmax = max( ellmax, ell )
      mlnm = mlnm + (2*ell+1)
    enddo ! iln
    p%nop = mlnm ! total number of projectors

   
    ! lists of ilm and iln indices
    allocate( ind_ilm(mlnm), ind_iln(mlnm), stat=ist ) ; if( ist /= 0 ) stop 'tPROJ proj_set: failed to allocate IND_ILM and IND_ILN'

    ! prepare index tables

    ilnm = 0
    do iln = 1, size(bf)
      ell = bf(iln)%ell
      ilm = ell*ell ! offset
      do emm = -ell, ell
        ilnm = ilnm+1
        ind_iln(ilnm) = iln
        ilm = ilm+1
        ind_ilm(ilnm) = ilm
      enddo ! emm
    enddo ! iln



    !!create a box to hold a projector on a 2 x fine grid
    hdg = hg*0.5
    
    boxsize = ceiling(2.0*rcut/hdg) + 1 + 2
    allocate(dgbox(1:boxsize, 1:boxsize, 1:boxsize), stat = ist)
    if( ist /= 0 ) stop 'failed to allcate double grid box'

    ! loop over periodic images of the atom (origins)
   
    ii = 1 ! init as 1 !!! the counter ii is incremented at the end if and only
                       !!! if the part got nonzero elements, i.e. p%noe > 0
    do ipi = 1, size(origins,2) ! for each periodic image

      if( ii > MAXNPI ) stop 'tPROJ proj_set: number of periodic images has been truncated to MAXNPI!'

      ! ori is the position of the radial center relative to the double grid points
      ori(1:3) = origins(1:3,ipi)-offset(1:3) ! no shift
      p%p(ii)%iphase = iphase(1:3,ipi) ! set integer phase information
      p%p(ii)%iimage = ii ! set number of periodic image

      ise(1:3,IS) = floor  ( ( ori(1:3)-rcut )/hg(1:3) )
      ise(1:3,IE) = ceiling( ( ori(1:3)+rcut )/hg(1:3) )
      
      ! now cut off everything that exceeds the grid limits (of this domain)
      ise(1:3,IS) = max( ise(1:3,IS),       1 )
      ise(1:3,IE) = min( ise(1:3,IE), ng(1:3) )

      noe = product( max( 0, ise(1:3,IE)-ise(1:3,IS)+1 ) )
      if( noe < 1 ) then
        cycle ! this image does not contribute
      endif ! noe < 1

      p%p(ii)%noe = noe

      if( present( checkmode ) ) then ; if( checkmode ) return ; endif 

      if( ider > 0 ) &
      allocate( drfval(mln), dXlm(1:(ellmax+1)**2), stat=ist ) ; if( ist /= 0 ) stop 'tPROJ proj_set: failed to allocate dYLM'
      allocate(  rfval(mln),  Xlm(1:(ellmax+1)**2), stat=ist ) ; if( ist /= 0 ) stop 'tPROJ proj_set: failed to allocate YLM'


      deallocate( p%p(ii)%ind, p%p(ii)%val, stat=ist )

      allocate( p%p(ii)%ind(1:noe), p%p(ii)%val(1:mlnm,1:noe), stat=ist ) ! allocate the projector grid values

      if( ist /= 0 ) stop 'tPROJ proj_set: allocation of l%VAL or l%IND failed!'
      p%p(ii)%val(:,:) = 0.
      ioe = 0 ! index of element
      do i3 =     ise(3,IS), ise(3,IE)
        do i2 =   ise(2,IS), ise(2,IE)
          do i1 = ise(1,IS), ise(1,IE)
            !if( gccc_nonZ(i1,i2,i3) ) then
              ioe = ioe + 1
              p%p(ii)%ind(ioe) = i1 + ng(1)*((i2-1) + ng(2)*(i3-1)) ! total index formula
              !p%p(ii)%val(:,ioe) = gccc(1:mlnm,i1,i2,i3)
              rv(3) = i3*hg(3)-ori(3) ; rv2(3) = rv(3)*rv(3)
              rv(2) = i2*hg(2)-ori(2) ; rv2(2) = rv(2)*rv(2)
              rv(1) = i1*hg(1)-ori(1) ; rv2(1) = rv(1)*rv(1)
              r2s = rv2(1) + rv2(2) + rv2(3) 
              
              if( r2s <= rcut2 ) then
                rs = sqrt( r2s ) ! radius of the grid point
                
                
                rfval = bf .at. rs ! eval functions at rs
                Xlm = Xlmax_rl( ellmax, rv ) ! get the Ylm*r^l coefficients for all ell-channels
                selectcase( ider )
                case( 0 ) 
                p%p(ii)%val(:,ioe) = Xlm(ind_ilm(:)) * rfval(ind_iln(:))
                 
                case( 1, 2, 3 ) ! set the derived values

                drfval = bf .derat. rs ! evaluate the derivative w.r.t. r at rs
                ! get the derived Ylm*r^l coefficients for all ell-channels
                dXlm = d_Xlmax_rl_dri( ellmax, rv, derive2i=ider )
                p%p(ii)%val(:,ioe) = dXlm(ind_ilm(:)) *  rfval(ind_iln(:)) + &
                                   Xlm(ind_ilm(:)) * drfval(ind_iln(:)) * rv(ider)/max( rs, 1E-12 )
                endselect
              endif
            
              
            !endif ! gccc_nonZ     
          enddo ! i1
        enddo ! i2
      enddo ! i3


      if( all( p%p(ii)%val == 0. ) ) then
        ! no element differs from zero, so the projector part does not need to be stored.
        deallocate( p%p(ii)%val, stat=ist ) ; if( ist /= 0 ) stop 'tPROJ proj_set: deallocation of empty l%VAL failed.'
        p%p(ii)%noe = 0
      else  ! all values zero
        ! some values are nonzero:
        ii = ii+1 !! increment the ii counter for periodic images
      endif ! all values zero

      if( ider > 0 ) &
      deallocate( drfval, dXlm, stat=ist )
      deallocate(  rfval,  Xlm, stat=ist )

    enddo ! ipi


    p%npi = ii-1 ! ii always points to the projector part to be set next,
                 ! therefore the number of projector parts is 1 less
   
    if( all( p%p(:)%noe <= 0 ) ) then
      p%npi = 0
    endif ! all projector parts zero

  endfunction ! proj_set

#endif

#ifndef USE_DOUBLE_GRID

  type(proj) function proj_set_orig( bf, origins, offset, iphase, hg, ng, &
         interpolation, meshrefinement, derive2i, checkmode, jatom ) & ! optionals
  result( p ) ! This routine makes use of the double grid technique
  use configuration, only: ERROR
  use harmonics, only: Xlmax_rl, d_Xlmax_rl_dri
#ifdef NaN_SEARCH
  use debugtools, only: NaN_search
#endif
cDBG  use configuration, only: o, WARNING
cDBG  use MPIconst, only: Wtime
  use type_bfun, only: bfun, operator(.at.), operator(.derat.)
    type(bfun), intent(in)            :: bf(:) ! j0-transformed localized projector fuctions
    real, intent(in)                  :: origins(1:,1:) ! origin w.r.t the origin(i=0) of the coarse grid
    real, intent(in)                  :: offset(1:3) ! origin of the coarse grid
    integer, intent(in)               :: iphase(1:,:) !! integer phase shifts for this image
    real, intent(in)                  :: hg(1:3) ! grid spacing
    integer, intent(in)               :: ng(1:3) ! number of grid points
    integer, intent(in), optional     :: interpolation ! ==> itp
    integer, intent(in), optional     :: meshrefinement ! ==> nmesh
    integer, intent(in), optional     :: derive2i ! ==> ider
    logical, intent(in), optional     :: checkmode
    integer, intent(in), optional     :: jatom ! global atom index

    character(len=*), parameter       :: fun = ' proj_set: '
    real                              :: rcut, rcut2 ! cutoff radius
    integer(kind=8)                   :: mem
    status_t                          :: ist ! status flag for allocations

    real                              :: rv(3), rv2(3), r2s, rs ! vector and distances
    integer                           :: ellmax, ell, emm!, enn ! quantum numbers
    integer                           :: iln, ilnm, ilm, mln, mlnm ! combindices and upper limits
    integer, allocatable              :: ind_iln(:), ind_ilm(:) ! index lists

    integer                           :: kse(1:3,IS:IE) ! start and end index of the accessed dense grid
    integer                           :: gse(1:3,IS:IE) ! start and end index of the non-zero dense grid
    integer                           :: ise(1:3,IS:IE) ! start and end index of the coarse grid
    integer                           :: i1, i2, i3 ! coarse grid loop indices
    integer                           :: k1, k2, k3 ! dense grid loop indices
    integer                           :: j1, j2, j3 ! interpolation loop indices

    real, allocatable                 :: gddd(:,:) ! (mlnm,dense)[dense,dense] grid array
    logical(kind=1), allocatable      :: gddd_nonZ(:)
    real, allocatable                 :: gcdd(:,:,:,:) ! (mlnm,coarse,dense,dense) grid array 
    logical(kind=1), allocatable      :: gcdd_nonZ(:,:,:) ! gddd_nonZ(i,k,k)
    real, allocatable                 :: gccd(:,:,:,:) ! (mlnm,coarse,coarse,dense) grid array
    logical(kind=1), allocatable      :: gccd_nonZ(:,:,:) ! gddd_nonZ(i,i,k)
    real, allocatable                 :: gccc(:,:,:,:) ! (mlnm,coarse,coarse,coarse) grid array
    logical(kind=1), allocatable      :: gccc_nonZ(:,:,:) ! gddd_nonZ(i,i,i)

    integer                           :: itp, nmesh, itpnmo ! itpnmo=itp*nmesh-1
    real, allocatable                 :: w8s(:,:) ! weights for reversed inverpolation w8s(-itpnmo:itpnmo,1:3)

    integer                           :: ider = 0 ! derivative? 0:none, 1:d/dx, 2:d/dy, 3:d/dz
    real, allocatable                 :: Xlm(:),   dXlm(:)   ! (mlm) ! spherical harmonics times r^ell
    real, allocatable                 :: rfval(:), drfval(:) ! (mln) ! value of bfuns at rs

    real                              :: hdg(3) ! grid spacing of the double grid
    real                              :: ori(3) ! origin on the double grid
    integer                           :: ii, ipi ! indicies of periodic images
    integer                           :: ioe, noe ! index and number of coarse grid element

#ifdef FULL_DEBUG
    integer :: id  ! counter for spatial directions 1:x, 2:y, 3:z
#endif

#ifdef DEBUG_BESSEL
    real                              :: q, dq, hq, fq
    integer                           :: ngs, nqs, iq, ir
    real, allocatable                 :: j0c(:,:)
    integer, allocatable              :: ell_list(:), ilm_list(:)
#endif
cDBG  real :: tm(0:9) ! times
cDBG  tm(0) = Wtime() ! start time

    p%nop = 0 ! result for early return

    ! ============================================================================
    ! general checks
    ! ============================================================================
cDBG  if( size(origins,1) /= 3 ) stop 'tPROJ proj_set: dim#1 of ORIGINS must be 3!'
cDBG  if( size(iphase,1) /= 3 ) stop 'tPROJ proj_set: dim#1 of IPHASE must be 3!'
cDBG  if( size(iphase,2) < size(origins,2) ) stop 'tPROJ set: dim#2 of IPHASE must be (at least) dim#2 of ORIGINS!'
    ! ============================================================================

cDBG  if(o>0) write(o,'(/,9A,2I3)') sym, fun

    ! ============================================================================
    ! cutoff radius
    ! ============================================================================
    rcut = maxval( bf(:)%rcut )
cDBG ! if(o>0) write(o,'(3A,F10.3,A,9I2)') sym, fun, 'rcut =', rcut, ' bf%ell =', bf%ell
    rcut2 = rcut*rcut

    if( rcut2 <= 0.0 ) then ! rcut2 is rcut^2
cDBG  if(o>0) write(o,'(9A)') sym, fun, 'Warning, rcut is 0. ==> return.'
      return
    endif ! rcut == 0.
    rcut = sqrt( rcut2 )
    ! ============================================================================



    ! ============================================================================
    ! derived localized function? 0:no or derive to 1:x, 2:y, 3:z
    ! ============================================================================
    ider = 0 ; if( present( derive2i ) ) ider = derive2i
cDBG  if( ider < 0 .or. ider > 3 ) stop 'tPROJ proj_set: derivative index IDER out of bounds [0, 3].'
    ! ============================================================================



    ! ============================================================================
    ! prepare for inverse interpolation
    ! ============================================================================
    itp   = DEFAULT_INTERPOLATION  ; if( present( interpolation  ) ) itp   = max( 1, abs( interpolation  ) )
    nmesh = DEFAULT_MESHREFINEMENT ; if( present( meshrefinement ) ) nmesh = max( 1, abs( meshrefinement ) )

    itp = 1
    nmesh = 1

cDBG  if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'itp = ',itp,' nmesh = ',nmesh
    itpnmo = itp*nmesh-1

    allocate( w8s(-itpnmo:itpnmo,1:3), stat=ist ) ; if( ist /= 0 ) stop 'tPROJ proj_set: failed to allocate WEIGHTS'
    w8s = 0.
    ! prepare weights for reversed interpolation
    call interpolationweights( o, nmesh, itp, w8s(:,1) )
    w8s(:,2) = w8s(:,1) ; w8s(:,3) = w8s(:,1) ! copy x-->y and x-->z
    ! ============================================================================
    w8s = w8s/real(nmesh)


    ! ============================================================================
    ! cartesian grid and grid spacings
    ! ============================================================================
    hdg = hg/real(nmesh) ! grid spacing of the denser DoubleGrid
cDBG  if( any(hg <= 0.) ) stop 'tPROJ proj_set: grid spacing must be positive!'
    p%nxyz = ng(1)*ng(2)*ng(3) ! this is for a check, if the input array is on the same grid.
cDBG  if( any(ng < 1) ) stop 'tPROJ proj_set: numbers of grid points should be positive!'
    ! ============================================================================


cDBG  if(o>0) write(o,'(3A,2(F10.6,A))') sym, fun, 'Rcut =', rcut, ' +', itpnmo*maxval(hdg), ' aB [rcut + interpolation]'


    ! ============================================================================
    ! determine total number of functions and total number of projectors
    ! ============================================================================
    mln = size(bf)
    ellmax = -1 ! init
    mlnm = 0 ! counter for number of projectors
    do iln = 1, mln
      ell = bf(iln)%ell
      ellmax = max( ellmax, ell )
      mlnm = mlnm + (2*ell+1)
    enddo ! iln
    p%nop = mlnm ! total number of projectors
#ifdef FULL_DEBUG
    if(o>0) write(o,'(3A,I0)') sym, fun, 'Number of projectors is ',mlnm
#endif
    ! ============================================================================



    ! ============================================================================
    ! prepare lists
    ! ============================================================================
#if 0
    ! with OpenMP, these arrays are private so they have to be allocated inside the parallel region
    if( ider > 0 ) &
    allocate( drfval(mln), dXlm(1:(ellmax+1)**2), stat=ist ) ; if( ist /= 0 ) stop 'tPROJ proj_set: failed to allocate dYLM'
    allocate(  rfval(mln),  Xlm(1:(ellmax+1)**2), stat=ist ) ; if( ist /= 0 ) stop 'tPROJ proj_set: failed to allocate YLM'
#endif
    ! lists of ilm and iln indices
    allocate( ind_ilm(mlnm), ind_iln(mlnm), stat=ist ) ; if( ist /= 0 ) stop 'tPROJ proj_set: failed to allocate IND_ILM and IND_ILN'
    ! ============================================================================

    
    ind_ilm = 0
    ind_iln = 0
    ! ============================================================================
    ! prepare index tables
    ! ============================================================================
    ilnm = 0
    do iln = 1, size(bf)
      ell = bf(iln)%ell
      ilm = ell*ell ! offset
      do emm = -ell, ell
        ilnm = ilnm+1
        ind_iln(ilnm) = iln
        ilm = ilm+1
        ind_ilm(ilnm) = ilm
      enddo ! emm
    enddo ! iln
cDBG  if( ilnm /= mlnm ) stop 'tPROJ proj_set: fatal error, number of projectors wrong.'
#ifdef FULL_DEBUG
    if(o>0) write(o,'(3A,99I3)') sym, fun, 'iln list:', ind_iln
    if(o>0) write(o,'(3A,99I3)') sym, fun, 'ilm list:', ind_ilm
#endif
    ! ============================================================================


    ! ============================================================================
    ! loop over periodic images of the atom (origins)
    ! ============================================================================
    ii = 1 ! init as 1 !!! the counter ii is incremented at the end if and only
                       !!! if the part got nonzero elements, i.e. p%noe > 0
    do ipi = 1, size(origins,2) ! for each periodic image
cDBG  tm(1) = Wtime() ! start time for this projector part

      if( ii > MAXNPI ) stop 'tPROJ proj_set: number of periodic images has been truncated to MAXNPI!'

      ! ori is the position of the radial center relative to the double grid points
      ori(1:3) = origins(1:3,ipi)-offset(1:3) ! no shift
      p%p(ii)%iphase = iphase(1:3,ipi) ! set integer phase information
      p%p(ii)%iimage = ii ! set number of periodic image


      ! example: meshrefinement = 2
      !    0   1   2   3   4   5   6   7   8    index k
      !  --x---x---x---x---x---x---x---x---x--- dense  grid
      !    |       |       |       |       |
      !  --origin--o-------o-------o-------o-- coarse grid
      !    0       1       2       3       4   index i
      !    |  hg   | grid spacing


      ! xc(i) = i*hg - origin
      ! xd(k) = k*hg/nmesh - origin = k*hg/nmesh - ori
      !

      ! now find k, such that xd(k) == origin + rcut
      ! <==> k = (origin+rcut)/hg*nmesh 

      ! start and end index of the non-zero localized function on the dense grid
      gse(1:3,IS) = floor  ( ( ori(1:3)-rcut )/hdg(1:3) )
      gse(1:3,IE) = ceiling( ( ori(1:3)+rcut )/hdg(1:3) )

      ! start and end index of the non-zero localized function on the coarse grid:
      ! the interpolation scheme goes from a coarse grid point
      ! (itp*nmesh-1) dense grid steps to the right (negative)
      ! and itp*nmesh dense grid steps to the left (positive)
      ! ==> i*nmesh - (itp*nmesh-1) = k
      ! ==> i = floor( ( k + (itp*nmesh-1))/nmesh )
      ise(1:3,IS) = floor  ( (gse(1:3,IS)-itpnmo-1)/real(nmesh) )
      ise(1:3,IE) = ceiling( (gse(1:3,IE)+itpnmo+1)/real(nmesh) )
#ifdef FULL_DEBUG
      if(o>0) write(o,'(3A,3(A,2I5,A),A,I8)') sym, fun, 'coarse grid range before: ', ( '[',gse(id,IS:IE),'] ', id=1,3)
#endif
      ! now cut off everything that exceeds the grid limits (of this domain)
      ise(1:3,IS) = max( ise(1:3,IS),       1 )
      ise(1:3,IE) = min( ise(1:3,IE), ng(1:3) )
      ! start and end index of the double grid array. these are larger than the gse
      kse(1:3,IS) = nmesh*ise(1:3,IS)-itpnmo
      kse(1:3,IE) = nmesh*ise(1:3,IE)+itpnmo
      !
      ! example nmesh = 2
!        situation 1: no cell borders intersecting the spheres, itp=1 ==> 4 nonzero weights
!
!        Atomic position               rcut         rcut+2*itp*h_coarse
!        |>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>|0000000000000000000|
!        ---x---x---x---x---x---x---x---x---x---x---x---x---x---x double grid
!                                       g\                k /    k = g + 2*itp*nmesh
!                                         \                /
!                                          \              /
!                                           \            /
!                                            \          /
!                                             \        /
!                                              \      /
!                                               \i* i/        i = min( ceiling(i*), ng ) = ceiling(i*)
!        ---O-------O-------O-------O-------O-------O-------O coarse grid
!
      !
!        situation 2: cell borders intersecting the spheres, itp=1 ==> 4 nonzero weights
!
!        Atomic position               rcut         rcut+2*itp*h_coarse
!        |>>>>>>>>>>>>>>>>>>>>>>>>>>>>>|0000000000000000000|
!        -x---x---x---x---x---x---x---x---x---x---x---x---x---x double grid
!                       | k /         g\                      k = i + itp*nmesh
!                     ce|ll/            \
!                    bor|der             \
!                       |/                \
!                       |                  \
!                      /|                   \
!                     / |                    \
!                   i/  |                     \i*             i = min( ceiling(i*), ng ) = ng
!        ---O-------O---|---O-------O-------O-------O-------O coarse grid
!                  ng   |
!
!         ! the cutoff radius (after double grid) may not be larger then the smallest
!         ! extend of the supercell.
!         ! this is crucial for small cells, because usually,
!         ! only one neighboring periodic image is taken into account
!         !     (1 image ==> np_max=3**3=27, 2 images ==> np_max=5**3=125)
!         !
!         !          prev. cell       supercell        next cell
!         !     -1.5             -.5      x       .5             1.5   (x=cell origin)
!         !    ===|===============|===============|===============|==
!         !           |<<<<<<<<<<<<<<<A>>>>>>>>>>>>>>>|             (A=a_{0}=atomic pos.)
!         !  <<<<<<<<<a>>>>>>>>>>>>>>>|                                  (a_{-1}) left pI
!         !                           |<<<<<<<<<<<<<<<a>>>>>>>>>>>>>>>|  (a_{1}) right pI
!         !                                                        (pI = periodic images)
!         ! ==> 3 |lfun>s will be set up:
!         !                           supercell
!         !                       |===============|
!         !                       <<<<A>>>>>>>>>>>>     |lfun_{ 0}> center
!         !                       >>>>|                 |lfun_{-1}>   left
!         !                           |<<<<<<<<<<<<     |lfun_{+1}>  right
!         !
!         ! if the effective rcut (including interpolation) were larger
!         ! than the supercell is wide, |lfun_{-2}> or |lfun_{2}> would be truncated.

#ifdef FULL_DEBUG
      if(o>0) write(o,'(3A,3(A,2I5,A),A,I8)') sym, fun, 'localization ind. before: ', ( '[',gse(id,IS:IE),'] ', id=1,3)
#endif

      ! gse == kse would let the computations run over the entire dg array
      ! cut off the index range for the double grid, where a boundary has cut away the i parts
      gse(1:3,IS) = max( gse(1:3,IS), kse(1:3,IS) )
      gse(1:3,IE) = min( gse(1:3,IE), kse(1:3,IE) )

      noe = product( max( 0, ise(1:3,IE)-ise(1:3,IS)+1 ) )
      if( noe < 1 ) then
#ifdef FULL_DEBUG
        if(o>0) write(o,'(3A,3I2,9A)') sym, fun, 'image ', iphase(:,ipi), ' does not contribute.'
#endif
        cycle ! this image does not contribute
      endif ! noe < 1

      p%p(ii)%noe = noe

#ifdef FULL_DEBUG
      if(o>0) then
        write(o,'(3A,3(A,2I5,A),A,I8)') sym, fun, 'localization index range: ', ( '[',gse(id,IS:IE),'] ', id=1,3)
        write(o,'(3A,3(A,2I5,A),A,I8)') sym, fun, ' coarse grid index range: ', ( '[',ise(id,IS:IE),'] ', id=1,3), 'noe =', noe
        write(o,'(3A,3(A,2I5,A),A,I8)') sym, fun, ' double grid index range: ', ( '[',kse(id,IS:IE),'] ', id=1,3)

        write(o,'(3A,3F9.3,A)') sym, fun, 'origin on coarse grid  : [', (origins(:,ipi)-offset)/hg, ' ]'
        write(o,'(3A,3F9.3,A)') sym, fun, 'requested o on d grid  : [', nmesh*((origins(:,ipi)-offset)/hg), ' ]'
        write(o,'(3A,3F9.3,A)') sym, fun, 'origin on double grid  : [', ori/hdg, ' ]'
cDBG    call show_position_relative_to_grid( o, ori,  hg, 'coarse grid' ) ! only in DEBUG mode
cDBG    call show_position_relative_to_grid( o, ori, hdg, 'double grid' ) ! only in DEBUG mode
      endif ! o>0
#endif
      if( present( checkmode ) ) then ; if( checkmode ) return ; endif ! Checkmode


      ! i*nmesh = k
      ! Example: nmesh = 2

      ! shft = 0.00 (matched)
      ! --x---x---x---x---x---x---x---x-- dense  grid
      !   1   2   3   4   5   6   7   8   index k
      ! ------o-------o-------o-------o-- coarse grid
      !       1       2       3       4   index i

      ! The Lagrangian  interpolation weights are
      !
      !              __N  x_k - x_m
      ! w_i( x_k ) = ||  -----------
      !              m=0  x_i - x_m
      !             m/=i
      !
      ! where the set of data points at x_j, j=0:N should be
      ! distributed as symmetric as possible around x_k.
      !
      ! ==> the grid points x_k with k in [(i-1)*nmesh+1:i*nmesh]
      !     are all between the coarse grid points i-1 and i.
      !     in the limit shift=0, k=i*nmesh and i match!


      mem = (1+8*mlnm)*max(0,(ise(1,IE)-ise(1,IS)+1))*max(0,(gse(2,IE)-gse(2,IS)+1))*max(0,(gse(3,IE)-gse(3,IS)+1))
      if( mem > 2**30 .and. o>0) write(o,'(3A,F0.3,9A)') sym, fun, 'try to allocate DoubleGrid with ',mem*.5**30,' GiByte.'
cDBG  if( mem > 2**20 .and. o>0) write(o,'(3A,F0.3,9A)') sym, fun, 'try to allocate DoubleGrid with ',mem*.5**20,' MiByte.'
 
      allocate( gcdd_nonZ(ise(1,IS):ise(1,IE),gse(2,IS):gse(2,IE),gse(3,IS):gse(3,IE)), stat=ist )
      allocate( gcdd(mlnm,ise(1,IS):ise(1,IE),gse(2,IS):gse(2,IE),gse(3,IS):gse(3,IE)), stat=ist )
#ifdef FULL_DEBUG
      if( .true. ) then ! show memory consumption always
#else
      if( ist /= 0 ) then
#endif
        if(o>0) write(o,'(3A,F0.3,9A)') sym, fun, 'allocation of DoubleGrid (',mem*.5**20,' MiByte)!'
        if( ist /= 0 ) stop 'tPROJ proj_set: allocation of double grid array gcdd failed!'
      endif ! ist /= 0

      gcdd_nonZ = .false.
      gcdd = 0

cDBG  tm(2) = Wtime() ! start double grid

      ! prepare the localized function on the denser grid gddd but only in the regions gse <= kse

! !$omp parallel private(gddd_nonZ,gddd,Xlm,dXlm,rfval,drfval)

      ! stores in the ranges kse if there are non-zero projector values,
      ! especially in the range where kse is larger than gse, dg_nonZ is false
      ! allocate the double grid   [[ gddd(0,:,:,:)-index 0 is the radius ]]
      allocate( gddd_nonZ(gse(1,IS):gse(1,IE)), gddd(mlnm,gse(1,IS):gse(1,IE)), stat=ist )
      if( ist /= 0 ) stop 'failed to allocate gddd arrays!'
      gddd_nonZ = .false. ; ! init mask as false
      gddd = 0. 

      if( ider > 0 ) &
      allocate( drfval(mln), dXlm(1:(ellmax+1)**2), stat=ist ) ; if( ist /= 0 ) stop 'tPROJ proj_set: failed to allocate dYLM'
      allocate(  rfval(mln),  Xlm(1:(ellmax+1)**2), stat=ist ) ; if( ist /= 0 ) stop 'tPROJ proj_set: failed to allocate YLM'

      drfval = 0.
      rfval = 0.

! !$omp do collapse(2) private(k3,k2,k1,rv,r2s,rs,i1,j1) schedule(static)
      do     k3 = gse(3,IS), gse(3,IE) 
        do   k2 = gse(2,IS), gse(2,IE)

          rv(3) = k3*hdg(3)-ori(3) ; rv2(3) = rv(3)*rv(3)
          rv(2) = k2*hdg(2)-ori(2) ; rv2(2) = rv(2)*rv(2)
          do k1 = gse(1,IS), gse(1,IE) 
            rv(1) = k1*hdg(1)-ori(1) ; rv2(1) = rv(1)*rv(1)

            r2s = rv2(1) + rv2(2) + rv2(3)
            if( r2s >= rcut2 ) then
              gddd_nonZ(k1) = .false.
              gddd(:,k1) = 0. ! necessary because dg will be accessed
            else  ! r^2 >= rcut^2
              gddd_nonZ(k1) = .true. !! mask: nonzero !!

              rs = sqrt( r2s ) ! radius of the grid point

              rfval = bf .at. rs ! eval functions at rs
              
              Xlm = Xlmax_rl( ellmax, rv ) ! get the Ylm*r^l coefficients for all ell-channels

              selectcase( ider )
              case( 0 ) 

                ! set the values to radial function * spherical harmonics*r^ell
                gddd(1:mlnm,k1) = Xlm(ind_ilm(:)) * rfval(ind_iln(:))

              case( 1, 2, 3 ) ! set the derived values

                drfval = bf .derat. rs ! evaluate the derivative w.r.t. r at rs

                ! get the derived Ylm*r^l coefficients for all ell-channels
                dXlm = d_Xlmax_rl_dri( ellmax, rv, derive2i=ider )

                gddd(1:mlnm,k1) = dXlm(ind_ilm(:)) *  rfval(ind_iln(:)) + &
                                   Xlm(ind_ilm(:)) * drfval(ind_iln(:)) * rv(ider)/max( rs, 1E-12 )

cDBG          case default ; stop 'tPROJ proj_set: IDER out of [0,3]'
              endselect ! ider

            endif ! r^2 >= rcut^2

          enddo ! k1

          ! filter from gddd to gcdd (on-the-fly: the gddd-level does exhibit a k2,k3 index)
          !==================================================================================
          do i1 = ise(1,IS), ise(1,IE)
            gcdd_nonZ(i1,k2,k3) = .false. ! init mask as false
            gcdd(:,i1,k2,k3) = 0.
            do j1 = max(-itpnmo,gse(1,IS)-i1*nmesh), min(gse(1,IE)-i1*nmesh,itpnmo) 
              k1 = i1 * nmesh + j1
              if( gddd_nonZ(k1) .and. w8s(j1,1) /= 0. ) then
                gcdd(:,i1,k2,k3) = gcdd(:,i1,k2,k3) + w8s(j1,1) * gddd(1:,k1)
                gcdd_nonZ(i1,k2,k3) = .true.
              endif
            enddo ! j1
          enddo ! i1
          !==================================================================================

        enddo ! k2
      enddo ! k3
! !$omp end do

#ifdef NaN_SEARCH
      ist = NaN_search( gddd, fun, 'dense-dense-dense grid for DG technique' )
#endif
      deallocate( gddd, gddd_nonZ, stat=ist )
      if( ider > 0 ) &
      deallocate( drfval, dXlm, stat=ist )
      deallocate(  rfval,  Xlm, stat=ist )

! !$omp barrier
! !$omp master

cDBG  tm(3) = Wtime() ! end double grid

      allocate( gccd_nonZ(ise(1,IS):ise(1,IE),ise(2,IS):ise(2,IE),gse(3,IS):gse(3,IE)), stat=ist )
      allocate( gccd(mlnm,ise(1,IS):ise(1,IE),ise(2,IS):ise(2,IE),gse(3,IS):gse(3,IE)), stat=ist )
      if( ist /= 0 ) stop 'failed to allocate gccd array'


      gccd_nonZ = .false.
      gccd = 0.
! !$omp end master
! !$omp barrier

      ! filter from gcdd to gccd


! !$omp do collapse(2) private(k3,i2,k2,i1,j2) schedule(static)
      do k3 = gse(3,IS), gse(3,IE)
        !==================================================================================
        do i2 = ise(2,IS), ise(2,IE)

          gccd(:,:,i2,k3) = 0.
          gccd_nonZ(:,i2,k3) = .false.
          do j2 = max(-itpnmo,gse(2,IS)-i2*nmesh), min(gse(2,IE)-i2*nmesh,itpnmo) 
            if( w8s(j2,2) == 0. ) cycle
            k2 = i2*nmesh + j2
            do i1 = ise(1,IS), ise(1,IE)
              if( gcdd_nonZ(i1,k2,k3) ) then
                gccd(:,i1,i2,k3) = gccd(:,i1,i2,k3) + w8s(j2,2) * gcdd(:,i1,k2,k3)
                gccd_nonZ(i1,i2,k3) = .true.
              endif ! non-zero
            enddo ! i1
          enddo ! j2
#ifndef ManuallyCollapseLoops
        enddo ! i2
#endif
        !==================================================================================
      enddo ! k3
! !$omp end do
! !$omp barrier
! !$omp master

#ifdef NaN_SEARCH
      ist = NaN_search( gcdd, fun, 'coarse-dense-dense grid for DG technique' )
#endif
      deallocate( gcdd, gcdd_nonZ, stat=ist )

      allocate( gccc_nonZ(ise(1,IS):ise(1,IE),ise(2,IS):ise(2,IE),ise(3,IS):ise(3,IE)), stat=ist )
      allocate( gccc(mlnm,ise(1,IS):ise(1,IE),ise(2,IS):ise(2,IE),ise(3,IS):ise(3,IE)), stat=ist )
      if( ist /= 0 ) stop 'failed to allocate gccc array'

      gccc_nonZ = .false.
      gccc = 0.

cDBG  tm(4) = Wtime() ! start reversed interpolation time
      
! !$omp end master
! !$omp barrier

      ! filter from gccd to gccc
      !==================================================================================
! !$omp do private(k3,i3,i2,i1,j3) schedule(static)
      do i3 = ise(3,IS), ise(3,IE)
        gccc_nonZ(:,:,i3) = .false.
        gccc(:,:,:,i3) = 0.
        do j3 = max(-itpnmo,gse(3,IS)-i3*nmesh), min(gse(3,IE)-i3*nmesh,itpnmo) 
          if( w8s(j3,3) == 0. ) cycle
          k3 = i3*nmesh + j3
          do i2 = ise(2,IS), ise(2,IE)
            do i1 = ise(1,IS), ise(1,IE)
              if( gccd_nonZ(i1,i2,k3) ) then
                gccc(:,i1,i2,i3) = gccc(:,i1,i2,i3) + w8s(j3,3) * gccd(:,i1,i2,k3)
                gccc_nonZ(i1,i2,i3) = .true.
              endif ! non-zero
            enddo ! i1
          enddo ! i2
        enddo ! j3
      enddo ! i3
! !$omp end do
      !==================================================================================

! !$omp end parallel

cDBG  tm(5) = Wtime() ! stop reversed interpolation time
 
#ifdef NaN_SEARCH
      ist = NaN_search( gccd, fun, 'coarse-coarse-dense grid for DG technique' )
      ist = NaN_search( gccc, fun, 'coarse-coarse-coarse grid for DG technique' )
#endif
      deallocate( gccd, gccd_nonZ, stat=ist )

      noe = count( gccc_nonZ ) 

      p%p(ii)%noe = noe ! set

      deallocate( p%p(ii)%ind, p%p(ii)%val, stat=ist )
! !$omp critical
      allocate( p%p(ii)%ind(1:noe), p%p(ii)%val(1:mlnm,1:noe), stat=ist ) ! allocate the projector grid values
! !$omp end critical
      if( ist /= 0 ) stop 'tPROJ proj_set: allocation of l%VAL or l%IND failed!'

      ioe = 0 ! index of element
      do i3 =     ise(3,IS), ise(3,IE)
        do i2 =   ise(2,IS), ise(2,IE)
          do i1 = ise(1,IS), ise(1,IE)
            if( gccc_nonZ(i1,i2,i3) ) then
              ioe = ioe + 1
              p%p(ii)%ind(ioe) = i1 + ng(1)*((i2-1) + ng(2)*(i3-1)) ! total index formula
              p%p(ii)%val(:,ioe) = gccc(1:mlnm,i1,i2,i3)

! write(99,'(I0," ",I0," ",I0)') i1,i2,i3 ! output for load imbalance estimation

            endif ! gccc_nonZ     
          enddo ! i1
        enddo ! i2
      enddo ! i3

      deallocate( gccc, gccc_nonZ, stat=ist )

#ifdef FULL_DEBUG
      if(o>0) write(o,'(3A,9I4)') sym, fun, 'pointers set2: ', shape(p%p(ii)%val), shape(p%p(ii)%ind)
#endif
cDBG  ! if(o>0) write(o,'(3A,9(I4,2I8))') sym, fun, 'arrays allocated: ', shape(p%p(ii)%val), shape(p%p(ii)%ind), mlnm, noe, noe

#ifdef DEBUG_POINTS
      write(88,'(A,3F12.6)') '#offset ', offset
      do ioe = 1, noe
        rv(1:3) = hg*ind_123(1:3,ioe)+offset
        rs = sqrt( sum(rv**2) )
        write(88,'(99F12.6)') rs, p%p(1)%val(1:2,ioe)
      enddo ! ioe
#endif

#ifdef DEBUG_BESSEL
      ! transform to Bessel functions for the visualization
      hq = product(hg(1:3))**(1./3.)
      ngs = nint( rcut/hq + itp + 0.5 )
      nqs = nint( 0.9 * ngs ) ; dq = acos(-1.)/(nqs*hq) ! = Pi/h
      allocate( ilm_list(mlnm), ell_list(mlnm), j0c(mlnm,-1:nqs) ) ; j0c = 0.
      ! ilm_list(:) e.g. = (/1,1,2,3,4,2,3,4/) for s,s*,p,p*
      ilnm = 0 ! init
      do iln = 1, mln ; ell = bf(iln)%ell
        ilm = ell**2 ! compute the index of the first ylm of this emm-subspace directly
        do emm = -ell, ell
          ilnm = ilnm + 1
          ilm = ilm+1
          ilm_list(ilnm) = ilm
          ell_list(ilnm) = ell
        enddo ! emm
      enddo ! iln

      if(o>0) then
        write(o,'(A,99I3)') 'ilm_list', ilm_list
        write(o,'(A,99I3)') 'ell_list', ell_list
      endif ! o/=0

      do ioe = 1, noe
        rv(1:3) = ind_123(1:3,ioe)*hg(1:3)-ori(1:3)
        r2s = sum( rv*rv )
        Xlm = Xlmax_rl( ellmax, rv ) ! get spherical harmonics
        if( r2s > 1.E-12 ) then
          rs = sqrt( r2s )
          do iq = 0, nqs ; q = dq*iq
            j0c(:,iq) = j0c(:,iq) + val(:,ioe) * Xlm( ilm_list(:) ) * bessel_j0( q*rs )
          enddo ! iq
        endif ! r2s
      enddo ! ioe

      if( noe > 0 ) then
        fq = 2./acos(-1.)*product(hg(1:3))*dq
        ! write on an equidistant radial grid, origin might deviate a lot!
        do ir = 8, 16*ngs ; rs = ir*hq/16.
          j0c(:,-1) = 0.
          do iq = 0, nqs ; q = dq*iq
            j0c(:,-1) = j0c(:,-1) + q*q * j0c(:,iq) * bessel_j0( q*rs )
          enddo ! iq
          write(33,'(99ES16.6E2)') rs, j0c(:,-1) * fq
        enddo ! ir

        deallocate( j0c, ilm_list, ell_list )
        ! write r^ell*prj(r) to file fort.32
        do ir = 0, 127 ; rs = ir*0.01*rcut
          write(32,'(99ES16.6E2)') rs, ( bf .at. rs )*rs**(2*bf(:)%ell)
        enddo ! ir
        stop 'tPROJ Bessel tranformation written to fort.33'
      endif ! noe > 0
      ! transformed to Bessel functions for visualization
#endif
!       deallocate( ind_123, stat=ist ) ; if( ist /= 0 ) stop 'tPROJ proj_set: deallocation of IND_123 failed.'



#ifdef FULL_DEBUG
      if(o>0) write(o,'(3A,9(I4,2I8))') sym, fun, 'dimensions:     ', mlnm, noe, noe
      if(o>0) write(o,'(3A,9(I4,2I8))') sym, fun, 'check arrays:   ', shape(p%p(ii)%val), shape(p%p(ii)%ind)
      if(o>0) write(o,'(3A,99I10)')      sym, fun,   'indices(1:20):  ', p%p(ii)%ind(1:min(20,noe))
      if(o>0) write(o,'(3A,99ES10.2E2)') sym, fun,   'values(1,1:20): ', p%p(ii)%val(1,1:min(20,noe))
#endif

#ifdef NaN_SEARCH
      if( any( p%p(ii)%val /= p%p(ii)%val ) ) stop 'tPROJ proj_set: NaN in VAL.'
#endif

      if( all( p%p(ii)%val == 0. ) ) then
        ! no element differs from zero, so the projector part does not need to be stored.
        deallocate( p%p(ii)%val, stat=ist ) ; if( ist /= 0 ) stop 'tPROJ proj_set: deallocation of empty l%VAL failed.'
        p%p(ii)%noe = 0
      else  ! all values zero
#if 0
        ! export values to a simple file fort.991
        write(991,'(A,9I8)') '# ii, noe = ', ii, p%p(ii)%noe
        do ioe = 1, p%p(ii)%noe
          write(991,'(I8,99F20.15)') p%p(ii)%ind(ioe), p%p(ii)%val(:,ioe)
        enddo ! ioe
#endif
        ! some values are nonzero:
        ii = ii+1 !! increment the ii counter for periodic images
      endif ! all values zero

cDBG  tm(6) = Wtime() ! stop all
#ifdef TIME
cDBG  if(o>0) write(o,'(3A,4(" ",F0.3),9A)') sym, fun, 'time [Setup+x,y-Itp,z-Itp,Copy,Total]', tm(3:5)-tm(2:4), tm(6)-tm(1), ' sec'
#endif

    enddo ! ipi
    p%npi = ii-1 ! ii always points to the projector part to be set next,
                 ! therefore the number of projector parts is 1 less
    ! ============================================================================

cDBG  if(o>0) write(o,'(2A,9(I0,A))') sym, fun, p%npi,' of ', size(origins,2),' possible projector parts are non-zero.'
cDBG  if( p%npi > size(origins,2) ) stop 'tPROJ proj_set: fatal: more non-zero projectors than possible.'

    if( all( p%p(:)%noe <= 0 ) ) then
      p%npi = 0
cDBG  if(o>0) write(o,'(9A)') sym, fun, 'projector fully zero!'
    endif ! all projector parts zero



cDBG  tm(9) = Wtime() ! final stop
#ifdef TIME
cDBG  if(o>0) write(o,'(3A,F0.3,9A)') sym, fun, 'time [Total] ', tm(9)-tm(0), ' sec'
#endif
  endfunction ! proj_set

#endif



#ifndef USE_DOUBLE_GRID

type(proj) function proj_set( bf, origins, offset, iphase, hg, ng, &
         interpolation, meshrefinement, derive2i, checkmode, jatom ) & ! optionals
  result( p )
  use configuration, only: ERROR
  use harmonics, only: Xlmax_rl, d_Xlmax_rl_dri
  use type_bfun, only: bfun, operator(.at.), operator(.derat.)
    type(bfun), intent(in)            :: bf(:) ! j0-transformed localized projector fuctions
    real, intent(in)                  :: origins(1:,:) ! origin w.r.t the origin(i=0) of the coarse grid
    real, intent(in)                  :: offset(1:3) ! origin of the coarse grid (if index was 0 but actual index starts form 1)
    integer, intent(in)               :: iphase(1:,:) !! integer phase shifts for this image
    real, intent(in)                  :: hg(1:3) ! grid spacing
    integer, intent(in)               :: ng(1:3) ! number of grid points
    integer, intent(in), optional     :: interpolation ! ==> itp
    integer, intent(in), optional     :: meshrefinement ! ==> nmesh
    integer, intent(in), optional     :: derive2i ! ==> ider
    logical, intent(in), optional     :: checkmode
    integer, intent(in), optional     :: jatom ! global atom index

    character(len=*), parameter       :: fun = ' proj_set: '
    real                              :: rcut, rcut2 ! cutoff radius
    integer(kind=8)                   :: mem
    status_t                          :: ist ! status flag for allocations

    real                              :: rv(3), rv2(3), r2s, rs ! vector and distances
    integer                           :: ellmax, ell, emm!, enn ! quantum numbers
    integer                           :: iln, ilnm, ilm, mln, mlnm ! combindices and upper limits
    integer, allocatable              :: ind_iln(:), ind_ilm(:) ! index lists
 
    integer                           :: ise(1:3,IS:IE) ! start and end index of the coarse grid
    integer                           :: i1, i2, i3 ! coarse grid loop indices
  
    integer                           :: ider = 0 ! derivative? 0:none, 1:d/dx, 2:d/dy, 3:d/dz
    real, allocatable                 :: Xlm(:),   dXlm(:)   ! (mlm) ! spherical harmonics times r^ell
    real, allocatable                 :: rfval(:), drfval(:) ! (mln) ! value of bfuns at rs

    real                              :: hdg(3) ! grid spacing of the double grid
    real                              :: ori(3) ! origin on the double grid
    integer                           :: ii, ipi ! indicies of periodic images
    integer                           :: ioe, noe, true_noe ! index and number of coarse grid element

    p%nop = 0 ! result for early return

    rcut = maxval( bf(:)%rcut )
    rcut2 = rcut*rcut
    
    ! derived localized function? 0:no or derive to 1:x, 2:y, 3:z 
    ider = 0 ; if( present( derive2i ) ) ider = derive2i

    p%nxyz = ng(1)*ng(2)*ng(3) ! this is for a check, if the input array is on the same grid.

  
    ! determine total number of functions and total number of projectors
    mln = size(bf)
    ellmax = -1 ! init
    mlnm = 0 ! counter for number of projectors
    do iln = 1, mln
      ell = bf(iln)%ell
      ellmax = max( ellmax, ell )
      mlnm = mlnm + (2*ell+1)
    enddo ! iln
    p%nop = mlnm ! total number of projectors

   
    ! lists of ilm and iln indices
    allocate( ind_ilm(mlnm), ind_iln(mlnm), stat=ist ) ; if( ist /= 0 ) stop 'tPROJ proj_set: failed to allocate IND_ILM and IND_ILN'

    ! prepare index tables

    ilnm = 0
    do iln = 1, size(bf)
      ell = bf(iln)%ell
      ilm = ell*ell ! offset
      do emm = -ell, ell
        ilnm = ilnm+1
        ind_iln(ilnm) = iln
        ilm = ilm+1
        ind_ilm(ilnm) = ilm
      enddo ! emm
    enddo ! iln

    ! loop over periodic images of the atom (origins)
   
    ii = 1 ! init as 1 !!! the counter ii is incremented at the end if and only
                       !!! if the part got nonzero elements, i.e. p%noe > 0
    do ipi = 1, size(origins,2) ! for each periodic image

      if( ii > MAXNPI ) stop 'tPROJ proj_set: number of periodic images has been truncated to MAXNPI!'

      ! ori is the position of the radial center relative to the double grid points
      ori(1:3) = origins(1:3,ipi)-offset(1:3) ! no shift
      p%p(ii)%iphase = iphase(1:3,ipi) ! set integer phase information
      p%p(ii)%iimage = ii ! set number of periodic image

      ise(1:3,IS) = floor  ( ( ori(1:3)-rcut )/hg(1:3) )
      ise(1:3,IE) = ceiling( ( ori(1:3)+rcut )/hg(1:3) )
      
      !write (*,*) 'ise is', ise(1:3, IS)
      !write (*,*) 'ise ie', ise(1:3, IE)

      ! now cut off everything that exceeds the grid limits (of this domain)
      ise(1:3,IS) = max( ise(1:3,IS),       1 )
      ise(1:3,IE) = min( ise(1:3,IE), ng(1:3) )


      !write (*,*) 'origins', origins(1:3, ipi)
      !write (*,*) 'offset', offset
      !write (*,*) 'ori', ori
      !write (*,*) 'ng', ng(1:3)
      !write (*,*) 'ise is', ise(1:3, IS)
      !write (*,*) 'ise ie', ise(1:3, IE)
      !write (*,*) 'rcut ', rcut
      noe = product( max( 0, ise(1:3,IE)-ise(1:3,IS)+1 ) )
      

      true_noe = 0
      do i3 = 1, ng(3)
        do i2 = 1, ng(2)
          do i1 = 1, ng(1)
            ! ori is the relative coordinate of the atom origin wrt the 0, 0, 0 of the grid of this domain
            ! now compute the vector that goes from the ori to the curent point of the grid
            rv(3) = i3*hg(3)-ori(3)
            rv(2) = i2*hg(2)-ori(2) 
            rv(1) = i1*hg(1)-ori(1)
            !if the norm of this vector is less than rcut , this point contributes to this projector part
            r2s = rv(1)*rv(1) + rv(2)*rv(2) + rv(3)*rv(3)
          
            if (rcut*rcut >= r2s) then 
              true_noe = true_noe + 1
            endif


          enddo
        enddo
      enddo
      !write (*,*) 'true noe ', true_noe
      !write (*,*) 'noe ', noe
      if( true_noe < 1 ) then
        cycle ! this image does not contribute
      endif 
      p%p(ii)%noe = true_noe

      if( present( checkmode ) ) then ; if( checkmode ) return ; endif 

      if( ider > 0 ) &
      allocate( drfval(mln), dXlm(1:(ellmax+1)**2), stat=ist ) ; if( ist /= 0 ) stop 'tPROJ proj_set: failed to allocate dYLM'
      allocate(  rfval(mln),  Xlm(1:(ellmax+1)**2), stat=ist ) ; if( ist /= 0 ) stop 'tPROJ proj_set: failed to allocate YLM'


      deallocate( p%p(ii)%ind, p%p(ii)%val, stat=ist )

      allocate( p%p(ii)%ind(1:true_noe), p%p(ii)%val(1:mlnm,1:true_noe), stat=ist ) ! allocate the projector grid values
      
      if( ist /= 0 ) stop 'tPROJ proj_set: allocation of l%VAL or l%IND failed!'
      p%p(ii)%val(:,:) = 0.
      ioe = 0 ! index of element
      do i3 = 1, ng(3)
        do i2 = 1, ng(2)
          do i1 = 1, ng(1)
            rv(3) = i3*hg(3)-ori(3)
            rv(2) = i2*hg(2)-ori(2) 
            rv(1) = i1*hg(1)-ori(1)
            !if the norm of this vector is less than rcut , this point contributes to this projector part
            r2s = rv(1)*rv(1) + rv(2)*rv(2) + rv(3)*rv(3)
          
            if (rcut*rcut >= r2s) then 
              ioe = ioe + 1
              p%p(ii)%ind(ioe) = i1 + ng(1)*((i2-1) + ng(2)*(i3-1)) ! total index formula
              
              rs = sqrt( r2s ) ! radius of the grid point
                  
              rfval = bf .at. rs ! eval functions at rs
              Xlm = Xlmax_rl( ellmax, rv ) ! get the Ylm*r^l coefficients for all ell-channels
              selectcase( ider )
              case( 0 ) 
              p%p(ii)%val(:,ioe) = Xlm(ind_ilm(:)) * rfval(ind_iln(:))
                 
              case( 1, 2, 3 ) ! set the derived values

              drfval = bf .derat. rs ! evaluate the derivative w.r.t. r at rs
                ! get the derived Ylm*r^l coefficients for all ell-channels
              dXlm = d_Xlmax_rl_dri( ellmax, rv, derive2i=ider )
              p%p(ii)%val(:,ioe) = dXlm(ind_ilm(:)) *  rfval(ind_iln(:)) + &
                                   Xlm(ind_ilm(:)) * drfval(ind_iln(:)) * rv(ider)/max( rs, 1E-12 )
              endselect
            endif
            
              
            !endif ! gccc_nonZ     
          enddo ! i1
        enddo ! i2
      enddo ! i3


      if( all( p%p(ii)%val == 0. ) ) then
        !stop 'this should not happen'
        ! no element differs from zero, so the projector part does not need to be stored.
        deallocate( p%p(ii)%val, stat=ist ) ; if( ist /= 0 ) stop 'tPROJ proj_set: deallocation of empty l%VAL failed.'
        p%p(ii)%noe = 0
      else  ! all values zero
        ! some values are nonzero:
        ii = ii+1 !! increment the ii counter for periodic images
      endif ! all values zero

      if( ider > 0 ) &
      deallocate( drfval, dXlm, stat=ist )
      deallocate(  rfval,  Xlm, stat=ist )

    enddo ! ipi


    p%npi = ii-1 ! ii always points to the projector part to be set next,
                 ! therefore the number of projector parts is 1 less
   
    if( all( p%p(:)%noe <= 0 ) ) then
      p%npi = 0
    endif ! all projector parts zero

  endfunction ! proj_set
#endif 

  subroutine proj_rescale( p, fac ) ! renormalize the values of this projector
    type(proj), intent(inout)       :: p
    real, intent(in)                :: fac(:)

    integer :: ii, ie
    do ii = 1, p%npi
      if( size(fac) == size(p%p(ii)%val,1) ) then
        do ie = 1, size(p%p(ii)%val,2)
          p%p(ii)%val(:,ie) = p%p(ii)%val(:,ie) * fac(:)
        enddo ! ie
      else  ! sizes match
        stop 'tPROJ: rescale: sizes do not match'
      endif ! sizes match
    enddo ! ii

  endsubroutine ! proj_rescale


  elemental subroutine proj_free( p ) ! deallocate the memory of this localized function
    type(proj), intent(inout)       :: p

    integer :: ii, ist
    do ii = 1, p%npi
      deallocate( p%p(ii)%val, p%p(ii)%ind, stat=ist )
      p%p(ii)%noe = 0
      p%p(ii)%iphase = 0
      p%p(ii)%iimage = 0
    enddo ! ii
    p%nop = 0 ; p%nxyz = 0 ; p%npi = 0 ; p%ider = 0

  endsubroutine ! proj_free


  subroutine interpolationweights( o, Nmesh, itp, weights )
    ! weights for inverse interpolation used in the matched double grid technique
    integer, intent(in)                   :: Nmesh, itp
    iounit_t, intent(in)                  :: o
    real, intent(out)                     :: weights(1-itp*nmesh:itp*nmesh-1)

    character(len=*), parameter           :: fun = ' interpolationweights: '
    integer                               :: k, j, m, i
    integer(kind=8)                       :: inom, iden
    double precision                      :: nom_over_den, f

    weights = 0. ; weights(0) = 1.0 ! default weights for n=1, i<=1

    do k = 0, nmesh-1
      do j = -(itp-1), itp
        inom = 1
        !-------
        iden = 1
        do i = -(itp-1), itp ; if( i == j ) cycle
          inom = inom *  (k-i*nmesh)
                        !------------
          iden = iden * ((j-i)*nmesh)
        enddo ! i
        nom_over_den = dble( inom ) / dble( iden )
        m = j*nmesh - k
        if( abs( m ) < itp*nmesh ) weights(m) = real( nom_over_den )
      enddo ! j
    enddo ! k

#ifdef DEBUG
    if(6>0) write(6,'(/,A,/,A,99F10.6)') ' Weights for reversed interpolation ', ' w =', weights(0:)*nmesh ; if(o>0) write(o,'(A)') ''
    do m = 0, itp*nmesh-1
      if( weights(m) /= weights(-m) ) stop 'tPROJ: interpolationweights: numerical issues.'
    enddo ! m
#endif
  endsubroutine ! interpolationweights

!- r1_c2 == 1
#endif


#ifndef BENCHMARK
!+ no benchmark

#if R1_C2 == 1
#define REAPLEX real
#define conjg(x) (x)
#else
#define REAPLEX complex
#endif




#if R1_C2 == 1
  subroutine add_1_r( &
#else
  subroutine add_1_c( &
#endif
                      p, f, c, eikL )
  ! |f> = |f> + c*|p>
    ! parameter
    character(len=*), parameter     :: fun = ' add: '
    ! arguments
    type(proj), intent(in)          :: p     ! projectors  |p_i>
    REAPLEX, intent(inout)          :: f(1:)  ! function    |f>
    REAPLEX, intent(in)             :: c(1:)  ! coeffs      c_i
    complex, intent(in)             :: eikL(3)  ! Blochphases c_i
    ! local variables
    complex                         :: phase
    REAPLEX                         :: cph(size(c))
    integer                         :: ioe, ii, ixyz

    ! projector contains any elements?
    if( p%nop < 1 ) return

    do ii = 1, p%npi ! for each projector part due to periodic images
      phase = Bloch_phase( eikL, p%p(ii)%iphase )
      cph = conjg( phase * c ) ! warning: conversion to real, if ( REAPLEX == real )

      do ioe = 1, p%p(ii)%noe
        ixyz = p%p(ii)%ind(ioe)
        f(ixyz) = f(ixyz) + dot_product( cph, p%p(ii)%val(:,ioe) ) ! |f> = |f> + c*|p>
      enddo ! ioe

    enddo ! ii

  endsubroutine ! add_1




#if R1_C2 == 1
  subroutine add_n_r( &
#else
  subroutine add_n_c( &
#endif
                      p, f, c, eikL ) ! |f> = |f> + c*|p>
    type(proj), intent(in)          :: p        ! projectors  |p_i>
    REAPLEX, intent(inout)          :: f(:,1:)  ! function    |f>
    REAPLEX, intent(in)             :: c(:,1:)  ! coeffs      c_i
    complex, intent(in)             :: eikL(3)  ! Blochphases c_i

    character(len=*), parameter     :: fun = ' add: '
    complex                         :: phase
    REAPLEX                         :: cph(size(c,1),size(c,2))
    integer                         :: ioe, ii, ixyz
    integer                         :: nc, ic
    
    if( p%nop < 1 ) return ! projector contains any elements?

    nc = p%nop ! = size(c,2)

    do ii = 1, p%npi ! for each projector part due to periodic images
      phase = Bloch_phase( eikL, p%p(ii)%iphase )
      cph = conjg( phase * c ) ! warning: conversion to real, if ( REAPLEX == real )
      do ioe = 1, p%p(ii)%noe
        ixyz = p%p(ii)%ind(ioe)
        do ic = 1, nc
          f(:,ixyz) = f(:,ixyz) + cph(:,ic) * p%p(ii)%val(ic,ioe) ! |f> = |f> + c*|p>
        enddo ! ic
      enddo ! ioe
    enddo ! ii

  endsubroutine ! add_n


!- no benchmark
#endif

#if R1_C2 == 1
  subroutine project_1_r( &
#else
  subroutine project_1_c( &
#endif
                          p, f, c, dV, eikL )
    type(proj), intent(in)          :: p       !! projectors  |p_i>
    REAPLEX, intent(in)             :: f(1:)   !! function    |f>
    REAPLEX, intent(out)            :: c(1:)   !! coeffs      c_i
    real, intent(in)                :: dV      !! volume element
    complex, intent(in)             :: eikL(3) !! Bloch phases

    !! performs the inner products c_i = <p_i|f>,
    !! whereas the p_i are only non-zero at a subset of
    !! the entries of f which is defined in an index list
    character(len=*), parameter     :: fun = ' prj: ' !! function name
    complex                         :: phase !! Bloch phase
    REAPLEX                         :: cph(size(c)) !! temporary coefficients
    integer                         :: ixyz !! index of the grid point
    integer                         :: ioe !! index of the projector element
    integer                         :: ii !! index of the projector part


    if( p%nop < 1 ) return ! projector contains any elements?

    c(1:p%nop) = 0. ! init result coefficients

    !write(*,*) 'projector parts = ', p%npi, p%p(1)%iphase
    do ii = 1, p%npi !! for each projector part
      !! (there may be several due to periodic images)
      phase = Bloch_phase( eikL, -p%p(ii)%iphase ) !! phase factor
      !write(*,*) 'phase ', phase
      cph = 0. ! init temporary coefficients for this projector part

      do ioe = 1, p%p(ii)%noe ! for each element of the projector functions
        ixyz = p%p(ii)%ind(ioe) ! get the index of the grid point

        cph = cph + f(ixyz) * p%p(ii)%val(:,ioe) ! c = <p|f>

      enddo ! ioe
      c = c + phase * cph ! warning: conversion to real, if ( REAPLEX == real )
    enddo ! ii
    c = c * dV ! multiply the volume element of each grid point


#if R1_C2 == 1

#endif
  endsubroutine ! project_1


#if R1_C2 == 1
  subroutine project_n_r( &
#else
  subroutine project_n_c( &
#endif
                          p, f, c, dV, eikL )
    !! performs the inner products c_i = <p_i|f>,
    !! whereas the p_i are only non-zero at a subset of
    !! the entries of f which is defined in an index list
    type(proj), intent(in)          :: p       !! projectors  |p_i>
    REAPLEX, intent(in)             :: f(:,1:) !! function    |f>
    REAPLEX, intent(out)            :: c(:,1:) !! coeffs      c_i
    real, intent(in)                :: dV      !! volume element
    complex, intent(in)             :: eikL(3) !! Bloch phases

    character(len=*), parameter     :: fun = ' prj: ' !! function name
    complex                         :: phase !! Bloch phase
    REAPLEX                         :: cph(size(c,1),size(c,2)) !! temporary coefficients
    integer                         :: ixyz !! index of the grid point
    integer                         :: ioe !! index of the projector element
    integer                         :: ii !! index of the projector part
    integer                         :: nc, ic

    if( p%nop < 1 ) return ! projector contains any elements?

    nc = p%nop ! = size(c,2)

    c(:,1:) = 0. ! init result coefficients

    do ii = 1, p%npi !! for each projector part (there may be several due to periodic images)
      phase = Bloch_phase( eikL, -p%p(ii)%iphase ) !! phase factor

      cph = 0. ! init temporary coefficients for this projector part
      do ioe = 1, p%p(ii)%noe ! for each element of the projector functions
        ixyz = p%p(ii)%ind(ioe) ! get the index of the grid point
        do ic = 1, nc
          cph(:,ic) = cph(:,ic) + f(:,ixyz) * p%p(ii)%val(ic,ioe) ! c = <p|f>
        enddo ! ic
      enddo ! ioe
      c = c + phase * cph ! warning: conversion to real, if ( REAPLEX == real )
    enddo ! ii
    c = c * dV ! multiply the volume element of each grid point

  endsubroutine ! project_n





#ifdef  BENCHMARK
#undef  R1_C2
#define R1_C2 2
#endif



#if R1_C2 == 2
!+ r1_c2 == 2

#ifdef EXTENDED
!+ extended

  status_t function test( )
#ifdef SNAPSHOT
    test = benchmark_projection()
#else
    write(*,'(2A)',iostat=test) __FILE__, ' no module test implemented!', &
      __FILE__, ' for BENCHMARKing with -t type_proj.mod, active #define SNAPSHOT in src/mod_type_proj.F95'
#endif
  endfunction ! test

!- extended
#endif

endmodule ! type_proj

!- r1_c2 == 2
#endif

!- DoublePreprocessing
#endif

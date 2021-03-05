!!
!! @author R.C. Singleton
!! @author M. Olesen
!! @author J. Beale
!! @author D. Wortmann
!! @author Paul Baumeister
!! @version 0.0
!!
!!>This code is a replacement of the old FFT code
!! It has been modified to
!! 1. Not to use any kind-parameter
!! 2. to be fixed-format compatible
!! 3. Some further changes are marked by !DW
!!          D. Wortmann
!!<4. Fixed-format was removed again ! PB  P.Baumeister
!!
!!-----------------------------------------------------------------------------
!! Multivariate Fast Fourier Transform
!!
!! Fortran 90 (ELF90) Implementation of Singleton's mixed-radix algorithm,
!! RC Singleton, Stanford Research Institute, Sept. 1968.
!!
!! Adapted from fftn.c, translated from Fortran 66 to C by Mark Olesen and
!! John Beale.
!!
!! Fourier transforms can be computed either in place, using assumed size
!! arguments, or by generic function, using assumed shape arguments.
!!
!!
!! Public:
!!
!!   fftkind                              kind parameter of complex :: arguments
!!                                        and function results.
!!
!!   fft(array, dims, inv)                 generic transform function
!!    COMPLEX(fftkind), DIMENSION(:,...,:), INTENT(IN) :: array
!!    INTEGER,          DIMENSION(:),       INTENT(IN),  OPTIONAL :: dims
!!    LOGICAL,                              INTENT(IN),  OPTIONAL :: inv
!!
!!   fftn(array, shape, dim, inv, stat)   in place transform subroutine
!!    COMPLEX(fftkind), DIMENSION(*), INTENT(INOUT)        :: array
!!    INTEGER,          DIMENSION(:), INTENT(IN)               :: shape
!!    INTEGER,          DIMENSION(:), INTENT(IN),  OPTIONAL :: dims
!!    LOGICAL,                        INTENT(IN),  OPTIONAL :: inv
!!    INTEGER,                        INTENT(OUT), OPTIONAL     :: stat
!!
!!
!! Formal Parameters:
!!
!!   array    The complex :: array to be transformed. array can be of arbitrary
!!            rank (i.e. up to seven).
!!
!!   shape    With subroutine fftn, the shape of the array to be transformed
!!            has to be passed separately, since fftradix - the internal trans-
!!            formation routine - will treat array always as one dimensional.
!!            The product of elements in shape must be the number of
!!            elements in array.
!!            Although passing array with assumed shape would have been nicer,
!!            I prefered assumed size in order to prevent the compiler from
!!            using a copy-in-copy-out mechanism. That would generally be
!!            necessary with fftn passing array to fftradix and with fftn
!!            being prepared for accepting non consecutive array sections.
!!            Using assumed size, it's up to the user to pass an array argu-
!!            ment, that can be addressed as continous one dimensional array
!!            without copying. Otherwise, transformation will not really be
!!            performed in place.
!!            On the other hand, since the rank of array and the size of
!!            shape needn't match, fftn is appropriate for handling more than
!!            seven dimensions.
!!            As far as function fft is concerned all this doesn't matter,
!!            because the argument will be copied anyway. Thus no extra
!!            shape argument is needed for fft.
!!
!! Optional Parameters:
!!
!!   dims     One dimensional integer :: array, containing the dimensions to be
!!            transformed. Default is (/1,...,N/) with N being the rank of
!!            array, i.e. complete transform. dims can restrict transformation
!!            to a subset of available dimensions. Its size must not exceed the
!!            rank of array or the size of shape respectivly.
!!
!!   inv      If .true., inverse transformation will be performed. Default is
!!            .false., i.e. forward transformation.
!!
!!   stat     If present, a system dependent nonzero status value will be
!!            returned in stat, if allocation of temporary storage failed.
!!            For functions, the integer :: variable status is used.
!!
!! Scaling:
!!
!!   Transformation results will always be scaled by the square root of the
!!   product of sizes of each dimension in dims. (See examples below)
!!
!!
!! Examples:
!!
!!   Let A be a L*M*N three dimensional complex :: array. Then
!!
!!     result = fft(A)
!!
!!   will produce a three dimensional transform, scaled by sqrt(L*M*N), while
!!
!!     call fftn(A, SHAPE(A))
!!
!!   will do the same in place.
!!
!!     result = fft(A, dims = (/1,3/))
!!
!!   will transform with respect to the first and the third dimension, scaled
!!   by sqrt(L*N).
!!
!!     result = fft(fft(A), inv = .true.)
!!
!!   should (approximately) reproduce A.
!!   With B having the same shape as A
!!
!!     result = fft(fft(A) * CONJG(fft(B)), inv = .true.)
!!
!!   will correlate A and B.
!!
!!
!! Remarks:
!!
!!   Following changes have been introduced with respect to fftn.c:
!!   - complex arguments and results are of type complex, rather than
!!     real an imaginary part separately.
!!   - increment parameter (magnitude of isign) has been dropped,
!!     inc is always one, direction of transform is given by inv.
!!   - maxf and maxp have been dropped. The amount of temporary storage
!!     needed is determined by the fftradix routine. Both fftn and fft
!!     can handle any size of array. (Maybe they take a lot of time and
!!     memory, but they will do it)
!!
!!   Redesigning fftradix in a way, that it handles assumed shape arrays
!!   would have been desirable. However, I found it rather hard to do this
!!   in an efficient way. Problems were:
!!   - to prevent stride multiplications when indexing arrays. At least our
!!     compiler was not clever enough to discover that in fact additions
!!     would do the job as well. On the other hand, I haven't been clever
!!     enough to find an implementation using array operations.
!!   - fftradix is rather large and different versions would be necessaray
!!     for each possible rank of array.
!!   Consequently, in place transformation still needs the argument stored
!!   in a consecutive bunch of memory and can't be performed on array
!!   sections like A(100:199:-3, 50:1020). Calling fftn with such sections
!!   will most probably imply copy-in-copy-out. However, the function fft
!!   works with everything it gets and should be convenient to use.
!!
!!   To enable this module to be used with ELF90 it appears to be necessary
!!   to allocate a 1-D work array into which the multi-dimensional array is
!!   copied, and then to copy the results back from the 1-D array to the
!!   multi-dimensional array ft.
!!
!!   Unfortunately, ELF90 will not allow a function to return more than one
!!   output variable.   The variable `stat' has been dropped from the function
!!   arguments.   Users should examine the value of the variable `status'
!!   instead.   This is a PUBLIC variable declared in this module.
!!
!! Michael Steffens, 09.12.96, <Michael.Steffens@mbox.muk.uni-hannover.de>
!! ELF90-compatible version by Alan Miller, 29 April 1997 & 6 June 1997
!! amiller @ bigpond.net.au
!! Restructured fftradix for better optimization by M. Steffens, 4 June 1997
!!-----------------------------------------------------------------------------
module FFT_tools
  implicit none
  private !! default visibility for this module namespace

  public :: fftn
  public :: fft
  public :: test

  interface fft
    module procedure fft1d, fft2d, fft3d, fft4d, fft5d, fft6d, fft7d
  endinterface

  character(len=*), parameter, private :: sym = 'fft' !! module symbol

  real, parameter :: sin60 = 0.86602540378443865
  real, parameter :: cos72 = 0.30901699437494742
  real, parameter :: sin72 = 0.95105651629515357
  real, parameter :: Pi    = 3.14159265358979323

  integer, save :: status = 0 !--- shifted to here as elf90 does not allow arguments to be intent(out)

  contains

  function fft1d( a, dims, inv ) result( ft )
    !--- formal parameters
    complex, intent(in) :: a(:)
    integer, intent(in),  optional :: dims(:)
    logical, intent(in),  optional :: inv
    !--- function result
    complex :: ft(size(a))

    ft = a
    call fftn( ft, shape(a), dims, inv=inv, stat=status )
  endfunction ! fft1d


  function fft2d( a, dims, inv ) result( ft )
    !--- formal parameters
    complex, intent(in) :: a(:,:)
    integer, intent(in), optional :: dims(:)
    logical, intent(in), optional :: inv
    !--- function result
    complex :: ft(size(a,1),size(a,2))
    complex :: work(size(a))

    work = reshape( a, (/ size(a) /) )
    call fftn( work, shape(a), dims, inv, stat=status )
    ft = reshape( work, shape(ft) )
  endfunction ! fft2d


  function fft3d( a, dims, inv ) result( ft )
    !--- formal parameters
    complex, intent(in) :: a(:,:,:)
    integer, intent(in),  optional :: dims(:)
    logical, intent(in),  optional :: inv
    !--- function result
    complex :: ft(size(a,1),size(a,2),size(a,3))
    complex :: work(size(a))

    work = reshape( a, (/ size(a) /) )
    call fftn( work, shape(a), dims, inv, stat=status )
    ft = reshape( work, shape(ft) )
  endfunction ! fft3d


  function fft4d( a, dims, inv ) result(ft)
    !--- formal parameters
    complex, intent(in) :: a(:,:,:,:)
    integer, intent(in), optional :: dims(:)
    logical, intent(in), optional :: inv
    !--- function result
    complex :: ft(size(a,1),size(a,2),size(a,3),size(a,4))
    complex :: work(size(a))

    work = reshape( a, (/ size(a) /) )
    call fftn( work, shape(a), dims, inv, stat=status )
    ft = reshape( work, shape(ft) )
  endfunction ! fft4d


  function fft5d( a, dims, inv ) result(ft)
    !--- formal parameters
    complex, intent(in) :: a(:,:,:,:,:)
    integer, intent(in), optional :: dims(:)
    logical, intent(in), optional :: inv
    !--- function result
    complex :: ft(size(a,1),size(a,2),size(a,3),size(a,4),size(a,5))
    complex :: work(size(a))

    work = reshape( a, (/ size(a) /) )
    call fftn( work, shape(a), dims, inv, stat=status )
    ft = reshape( work, shape(ft) )
  endfunction ! fft5d


  function fft6d( a, dims, inv ) result(ft)
    !--- formal parameters
    complex, intent(in) :: a(:,:,:,:,:,:)
    integer, intent(in), optional :: dims(:)
    logical, intent(in), optional :: inv
    !--- function result
    complex :: ft(size(a,1),size(a,2),size(a,3),size(a,4),size(a,5),size(a,6))
    complex :: work(size(a))

    work = reshape( a, (/ size(a) /) )
    call fftn( work, shape(a), dims, inv, stat=status )
    ft = reshape( work, shape(ft) )
  endfunction ! fft6d


  function fft7d( a, dims, inv ) result(ft)
    !--- formal parameters
    complex, intent(in) :: a(:,:,:,:,:,:,:)
    integer, intent(in), optional :: dims(:)
    logical, intent(in), optional :: inv
    !--- function result
    complex :: ft(size(a,1),size(a,2),size(a,3),size(a,4),size(a,5),size(a,6),size(a,7))
    complex :: work(size(a))

    work = reshape( a, (/ size(a) /) )
    call fftn( work, shape(a), dims, inv, stat=status )
    ft = reshape( work, shape(ft) )
  endfunction ! fft7d


  subroutine fftn( array, shape, dims, inv, stat )
    !--- formal parameters
    complex, intent(inout)            :: array(:)
    integer, intent(in)               :: shape(:)
    integer, intent(in), optional     :: dims(:)
    logical, intent(in), optional     :: inv
    integer, intent(out), optional    :: stat
    !--- local vars
    integer         :: d(size(shape))
    logical         :: inverse
    integer         :: id, ndims, ntotal
!    real            :: scal

    !--- optional parameter settings
    inverse = .false. ; if( present(inv) ) inverse = inv

    if( present(dims) ) then
      ndims = min( size(dims), size(d) )
      d(1:ndims) = dims(1:ndims)
    else
      ndims = size(d)
      d = (/ ( id, id=1,size(d) ) /) ! default is all dimensions 1,...,N
    endif

    ntotal = product(shape)

    !dw!
    !dw: no scaling!
    ! scal = sqrt( 1. / product(shape(d(1:ndims))) )
    ! array = scal * array

    do id = 1, ndims
      call fftradix( array, ntotal, shape(d(id)), product(shape(1:d(id))), inverse )
    enddo ! id

    if( present(stat) ) stat = status
  endsubroutine ! fftn

  subroutine fftradix( array, ntotal, npass, nspan, inv )
    !--- formal parameters
    complex, intent(inout)      :: array(:)
    integer, intent(in)         :: ntotal, npass, nspan
    logical, intent(in)         :: inv
    !--- local arrays
    integer :: factor(bit_size(0))
    !--- local scalars
    integer :: maxfactor, nfactor, nsquare
    !--- intrinsics used

    if ( npass <= 1 ) return

    maxfactor = factorize( npass, factor, nfactor, nsquare )

    call transform( array, ntotal, npass, nspan, factor, nfactor, maxfactor, inv )
    call permute( array, ntotal, npass, nspan, factor, nfactor, nsquare, maxfactor )
  endsubroutine ! fftradix

  integer function factorize( npass, factor, nfactor, nsquare ) result( maxfactor )
    !--- formal parameters
    integer, intent(in)  :: npass
    integer, intent(out) :: factor(:)
    integer, intent(out) :: nfactor, nsquare
    !--- local scalars
    integer :: j, jj, k

    nfactor = 0 ! init the number of factors
    k = npass

    ! try with 4
    do while ( mod( k, 16 ) == 0 ) ! npass can be divided by 16
      nfactor = nfactor + 1 ! increase the number of factors
      factor(nfactor) = 4 ! store the factor 4
      k = k / 16 ! proceed with the reduced number
    enddo
    ! now k does not contain any powers of 16 any more

    j = 3 ! try odd numbers starting from 3
    jj = j * j
    do
      do while ( mod( k, jj ) == 0 ) ! npass can be divided by j^2
        nfactor = nfactor + 1 ! increase the number of factors
        factor(nfactor) = j ! store the factor j
        k = k / jj ! proceed with the reduced number
      enddo
      j = j + 2 ! try higher odd numbers
      jj = j * j
      if ( jj > k ) exit
    enddo

    if ( k <= 4 ) then
      nsquare = nfactor
      factor(nfactor+1) = k
      if ( k /= 1 ) nfactor = nfactor + 1
    else   ! k <= 4
      if ( k - ishft( k / 4, 2 ) == 0 ) then
        nfactor = nfactor + 1
        factor(nfactor) = 2
        k = k / 4
      endif
      nsquare = nfactor
      j = 2
      do
        if ( mod( k, j ) == 0 ) then
          nfactor = nfactor + 1
          factor(nfactor) = j
          k = k / j
        endif
        j = ishft( (j + 1) / 2, 1 ) + 1
        if ( j > k ) exit
      enddo
    endif ! k <= 4
    if ( nsquare > 0 ) then
      j = nsquare
      do
        nfactor = nfactor + 1
        factor(nfactor) = factor(j)
        j = j - 1
        if ( j == 0 ) exit
      enddo
    endif
    maxfactor = maxval( factor(:nfactor) )
  endfunction ! factorize

  subroutine transform( array, ntotal, npass, nspan, factor, nfactor, maxfactor, inv )
    !--- formal parameters
    complex, intent(inout)      :: array(:)
    integer, intent(in)         :: ntotal, npass, nspan
    integer, intent(in)         :: factor(:)
    integer, intent(in)         :: nfactor
    integer, intent(in)         :: maxfactor
    logical, intent(in)         :: inv
    !--- local scalars
    integer         :: ii, ispan
    integer         :: j, jc, jf, jj
    integer         :: k, kk, kspan, k1, k2, k3, k4
    integer         :: nn, nt
    real            :: s60, c72, s72, pi2, radf
    real            :: c1, s1, c2, s2, c3, s3, cd, sd, ak
    complex         :: cc, cj, ck, cjp, cjm, ckp, ckm
    !--- local arrays
  !   complex, allocatable  :: ctmp(:)
  !   real, allocatable     :: sine(:), cosine(:)
  ! 
  !   maxfactor = maxval(factor(:nfactor))
  !   allocate( ctmp(maxfactor), sine(maxfactor), cosine(maxfactor), stat=status )
  !   if (status /= 0) return
    complex :: ctmp(maxfactor)
    real :: sine(maxfactor), cosine(maxfactor)

    c72 = cos72
    if (inv) then
        s72 = sin72
        s60 = sin60
        pi2 = pi
    else
        s72 = -sin72
        s60 = -sin60
        pi2 = -pi
    endif

    nt = ntotal
    nn = nt - 1
    kspan = nspan
    jc = nspan / npass
    radf = pi2 * jc
    pi2 = pi2 * 2.0 !-- use 2 pi from here on

    ii = 0
    jf = 0
    do
      sd = radf / kspan
      cd = sin(sd)
      cd = 2.0 * cd * cd
      sd = sin(sd + sd)
      kk = 1
      ii = ii + 1

      select case (factor(ii))
      case (2)
        !-- transform for factor of 2 (including rotation factor)
        kspan = kspan / 2
        k1 = kspan + 2
        do
            do
              k2 = kk + kspan
              ck = array(k2)
              array(k2) = array(kk)-ck
              array(kk) = array(kk) + ck
              kk = k2 + kspan
              if (kk > nn) exit
            enddo
            kk = kk - nn
            if (kk > jc) exit
        enddo
        if (kk > kspan) then
!             deallocate( ctmp, sine, cosine, stat=status )
            return
        endif
        do
            c1 = 1.0 - cd
            s1 = sd
            do
              do
                  do
                    k2 = kk + kspan
                    ck = array(kk) - array(k2)
                    array(kk) = array(kk) + array(k2)
                    array(k2) = ck * cmplx(c1, s1)
                    kk = k2 + kspan
                    if (kk >= nt) exit
                  enddo
                  k2 = kk - nt
                  c1 = -c1
                  kk = k1 - k2
                  if (kk <= k2) exit
              enddo
              ak = c1 - (cd * c1 + sd * s1)
              s1 = sd * c1 - cd * s1 + s1
              c1 = 2.0 - (ak * ak + s1 * s1)
              s1 = s1 * c1
              c1 = c1 * ak
              kk = kk + jc
              if (kk >= k2) exit
            enddo
            k1 = k1 + 1 + 1
            kk = (k1 - kspan) / 2 + jc
            if (kk > jc + jc) exit
        enddo

      case (4) !-- transform for factor of 4
        ispan = kspan
        kspan = kspan / 4

        do
            c1 = 1.0
            s1 = 0.0
            do
              do
                  k1 = kk + kspan
                  k2 = k1 + kspan
                  k3 = k2 + kspan
                  ckp = array(kk) + array(k2)
                  ckm = array(kk) - array(k2)
                  cjp = array(k1) + array(k3)
                  cjm = array(k1) - array(k3)
                  array(kk) = ckp + cjp
                  cjp = ckp - cjp
                  if (inv) then
                    ckp = ckm + cmplx(-aimag(cjm), real(cjm))
                    ckm = ckm + cmplx(aimag(cjm), -real(cjm))
                  else
                    ckp = ckm + cmplx(aimag(cjm), -real(cjm))
                    ckm = ckm + cmplx(-aimag(cjm), real(cjm))
                  endif
                  !-- avoid useless multiplies
                  if (s1 == 0.0) then
                    array(k1) = ckp
                    array(k2) = cjp
                    array(k3) = ckm
                  else
                    array(k1) = ckp * cmplx(c1, s1)
                    array(k2) = cjp * cmplx(c2, s2)
                    array(k3) = ckm * cmplx(c3, s3)
                  endif
                  kk = k3 + kspan
                  if (kk > nt) exit
              enddo

              c2 = c1 - (cd * c1 + sd * s1)
              s1 = sd * c1 - cd * s1 + s1
              c1 = 2.0 - (c2 * c2 + s1 * s1)
              s1 = s1 * c1
              c1 = c1 * c2
              !-- values of c2, c3, s2, s3 that will get used next time
              c2 = c1 * c1 - s1 * s1
              s2 = 2.0 * c1 * s1
              c3 = c2 * c1 - s2 * s1
              s3 = c2 * s1 + s2 * c1
              kk = kk - nt + jc
              if (kk > kspan) exit
            enddo
            kk = kk - kspan + 1
            if (kk > jc) exit
        enddo
        if (kspan == jc) then
!             deallocate( ctmp, sine, cosine, stat=status )
            return
        endif

      case default
        !-- transform for odd factors
        k = factor(ii)
        ispan = kspan
        kspan = kspan / k

        select case (k)
        case (3) !-- transform for factor of 3 (optional code)
            do
              do
                  k1 = kk + kspan
                  k2 = k1 + kspan
                  ck = array(kk)
                  cj = array(k1) + array(k2)
                  array(kk) = ck + cj
                  ck        = ck - cmplx( 0.5 * real (cj), 0.5 * aimag(cj))
                  cj = cmplx( (real(array(k1)) - real (array(k2))) * s60,(aimag(array(k1)) - aimag(array(k2))) * s60)
                  array(k1) = ck + cmplx(-aimag(cj), real(cj))
                  array(k2) = ck + cmplx(aimag(cj), -real(cj))
                  kk = k2 + kspan
                  if (kk >= nn) exit
              enddo
              kk = kk - nn
              if (kk > kspan) exit
            enddo

        case (5) !-- transform for factor of 5 (optional code)
            c2 = c72 * c72 - s72 * s72
            s2 = 2.0 * c72 * s72
            do
              do
                  k1 = kk + kspan
                  k2 = k1 + kspan
                  k3 = k2 + kspan
                  k4 = k3 + kspan
                  ckp = array(k1) + array(k4)
                  ckm = array(k1) - array(k4)
                  cjp = array(k2) + array(k3)
                  cjm = array(k2) - array(k3)
                  cc = array(kk)
                  array(kk) = cc + ckp + cjp
                  ck = cmplx(real(ckp) * c72, aimag(ckp) * c72 ) + &
                      cmplx(real(cjp) * c2,  aimag(cjp) * c2) + cc
                  cj = cmplx(real(ckm) * s72, aimag(ckm) * s72) + &
                      cmplx(real(cjm) * s2,  aimag(cjm) * s2)
                  array(k1) = ck + cmplx(-aimag(cj), real(cj))
                  array(k4) = ck + cmplx(aimag(cj), -real(cj))
                  ck  = cmplx(real(ckp) * c2,  aimag(ckp) * c2) &
                      + cmplx(real(cjp) * c72, aimag(cjp) * c72) + cc
                  cj   = cmplx(real(ckm) * s2,  aimag(ckm) * s2) - &
                        cmplx(real(cjm) * s72, aimag(cjm) * s72)
                  array(k2) = ck + cmplx(-aimag(cj), real(cj))
                  array(k3) = ck + cmplx(aimag(cj), -real(cj))
                  kk = k4 + kspan
                  if (kk >= nn) exit
              enddo
              kk = kk - nn
              if (kk > kspan) exit
            enddo

        case default
            if (k /= jf) then
              jf = k
              s1 = pi2 / k
              c1 = cos(s1)
              s1 = sin(s1)
              cosine (jf) = 1.0
              sine (jf) = 0.0
              j = 1
              do
                  cosine (j) = cosine (k) * c1 + sine (k) * s1
                  sine (j) = cosine (k) * s1 - sine (k) * c1
                  k = k-1
                  cosine (k) = cosine (j)
                  sine (k) = -sine (j)
                  j = j + 1
                  if (j >= k) exit
              enddo
            endif
            do
              do
                  k1 = kk
                  k2 = kk + ispan
                  cc = array(kk)
                  ck = cc
                  j = 1
                  k1 = k1 + kspan
                  do
                    k2 = k2 - kspan
                    j = j + 1
                    ctmp(j) = array(k1) + array(k2)
                    ck = ck + ctmp(j)
                    j = j + 1
                    ctmp(j) = array(k1) - array(k2)
                    k1 = k1 + kspan
                    if (k1 >= k2) exit
                  enddo
                  array(kk) = ck
                  k1 = kk
                  k2 = kk + ispan
                  j = 1
                  do
                    k1 = k1 + kspan
                    k2 = k2 - kspan
                    jj = j
                    ck = cc
                    cj = (0.0, 0.0)
                    k = 1
                    do
                        k = k + 1
                        ck = ck + cmplx(real (ctmp(k)) * cosine(jj),aimag(ctmp(k)) * cosine(jj))
                        k = k + 1
                        cj = cj + cmplx( real (ctmp(k)) * sine(jj),aimag(ctmp(k)) * sine(jj))
                        jj = jj + j
                        if (jj > jf) jj = jj - jf
                        if (k >= jf) exit
                    enddo
                    k = jf - j
                    array(k1) = ck + cmplx(-aimag(cj), real(cj))
                    array(k2) = ck + cmplx(aimag(cj), -real(cj))
                    j = j + 1
                    if (j >= k) exit
                  enddo
                  kk = kk + ispan
                  if (kk > nn) exit
              enddo
              kk = kk - nn
              if (kk > kspan) exit
            enddo

        endselect
        !--  multiply by rotation factor (except for factors of 2 and 4)
        if (ii == nfactor) then
!             deallocate( ctmp, sine, cosine, stat=status )
            return
        endif

        kk = jc + 1
        do
            c2 = 1.0 - cd
            s1 = sd
            do
              c1 = c2
              s2 = s1
              kk = kk + kspan
              do
                  do
                    array(kk) = cmplx(c2, s2)* array(kk)
                    kk = kk + ispan
                    if (kk > nt) exit
                  enddo
                  ak = s1 * s2
                  s2 = s1 * c2 + c1 * s2
                  c2 = c1 * c2 - ak
                  kk = kk - nt + kspan
                  if (kk > ispan) exit
              enddo
              c2 = c1 - (cd * c1 + sd * s1)
              s1 = s1 + sd * c1 - cd * s1
              c1 = 2.0 - (c2 * c2 + s1 * s1)
              s1 = s1 * c1
              c2 = c2 * c1
              kk = kk - ispan + jc
              if (kk > kspan) exit
            enddo
            kk = kk - kspan + jc + 1
            if (kk > jc + jc) exit
        enddo

      endselect
    enddo

    ! deallocate( ctmp, sine, cosine, stat=status )
  endsubroutine ! transform

  subroutine permute( array, ntotal, npass, nspan, factor, nfactor, nsquare, maxfactor )
    !--- formal parameters
    complex, intent(inout)      :: array(:)
    integer, intent(in)         :: ntotal, npass, nspan
    integer, intent(inout)      :: factor(:)
    integer, intent(in)         :: nfactor, nsquare
    integer, intent(in)         :: maxfactor
    !--- local scalars
    integer         :: ii, ispan
    integer         :: j, jc, jj
    integer         :: k, kk, kspan, kt, k1, k2, k3
    integer         :: nn, nperm, nt
    complex         :: ck
    !--- local arrays
    complex, allocatable  :: ctmp(:)
    integer, allocatable  :: perm(:)

    allocate( ctmp(maxfactor), stat=status )
    if (status /= 0) return

    if (nfactor - ishft(nsquare, 1) > 0) then
        nperm = max(nfactor + 1,product(factor(nsquare+1: nfactor-nsquare)) - 1)
    else
        nperm = nfactor + 1
    endif
    allocate( perm(nperm), stat=status )
    if ( status /= 0 ) return

    !--  permute the results to normal order---done in two stages
    !--  permutation for square factors of n

    nt = ntotal
    nn = nt - 1
    kt = nsquare
    kspan = nspan
    jc = nspan / npass

    perm (1) = nspan
    if (kt > 0) then
      k = kt + kt + 1
      if (nfactor < k) k = k - 1
      j = 1
      perm (k + 1) = jc
      do
        perm (j + 1) = perm (j) / factor(j)
        perm (k) = perm (k + 1) * factor(j)
        j = j + 1
        k = k - 1
        if (j >= k) exit
      enddo
      k3 = perm (k + 1)
      kspan = perm (2)
      kk = jc + 1
      k2 = kspan + 1
      j = 1

      if (npass /= ntotal) then
        permute_multi: do
            do
              do
                  k = kk + jc
                  do
                    !-- swap array(kk) <> array(k2)
                    ck = array(kk)
                    array(kk) = array(k2)
                    array(k2) = ck
                    kk = kk + 1
                    k2 = k2 + 1
                    if (kk >= k) exit
                  enddo
                  kk = kk + nspan - jc
                  k2 = k2 + nspan - jc
                  if (kk >= nt) exit
              enddo
              kk = kk - nt + jc
              k2 = k2 - nt + kspan
              if (k2 >= nspan) exit
            enddo
            do
              do
                  k2 = k2 - perm (j)
                  j = j + 1
                  k2 = perm (j + 1) + k2
                  if (k2 <= perm (j)) exit
              enddo
              j = 1
              do
                  if (kk < k2) cycle permute_multi
                  kk = kk + jc
                  k2 = k2 + kspan
                  if (k2 >= nspan) exit
              enddo
              if (kk >= nspan) exit
            enddo
            exit
        enddo permute_multi
      else
        permute_single: do
            do
              !-- swap array(kk) <> array(k2)
              ck = array(kk)
              array(kk) = array(k2)
              array(k2) = ck
              kk = kk + 1
              k2 = k2 + kspan
              if (k2 >= nspan) exit
            enddo
            do
              do
                  k2 = k2 - perm (j)
                  j = j + 1
                  k2 = perm (j + 1) + k2
                  if (k2 <= perm (j)) exit
              enddo
              j = 1
              do
                  if (kk < k2) cycle permute_single
                  kk = kk + 1
                  k2 = k2 + kspan
                  if (k2 >= nspan) exit
              enddo
              if (kk >= nspan) exit
            enddo
            exit
        enddo permute_single
      endif
      jc = k3
    endif

    if (ishft(kt, 1) + 1 >= nfactor) then
      deallocate( perm, ctmp )
      return
    endif

    ispan = perm (kt + 1)
    !-- permutation for square-free factors of n
    j = nfactor - kt
    factor(j + 1) = 1
    do
        factor(j) = factor(j) * factor(j+1)
        j = j - 1
        if (j == kt) exit
    enddo
    kt = kt + 1
    nn = factor(kt) - 1
    j = 0
    jj = 0
    do
        k = kt + 1
        k2 = factor(kt)
        kk = factor(k)
        j = j + 1
        if (j > nn) exit !-- exit infinite loop
        jj = jj + kk
        do while (jj >= k2)
          jj = jj - k2
          k2 = kk
          k = k + 1
          kk = factor(k)
          jj = jj + kk
        enddo
        perm (j) = jj
    enddo
    !--  determine the permutation cycles of length greater than 1
    j = 0
    do
      do
        j = j + 1
        kk = perm(j)
        if (kk >= 0) exit
      enddo
      if (kk /= j) then
        do
            k = kk
            kk = perm (k)
            perm (k) = -kk
            if (kk == j) exit
        enddo
        k3 = kk
      else
        perm (j) = -j
        if (j == nn) exit !-- exit infinite loop
      endif
    enddo
    !--  reorder a and b, following the permutation cycles
    do
      j = k3 + 1
      nt = nt - ispan
      ii = nt - 1 + 1
      if (nt < 0) exit !-- exit infinite loop
      do
        do
            j = j-1
            if (perm(j) >= 0) exit
        enddo
        jj = jc
        do
            kspan = jj
            if (jj > maxfactor) kspan = maxfactor
            jj = jj - kspan
            k = perm(j)
            kk = jc * k + ii + jj
            k1 = kk + kspan
            k2 = 0
            do
              k2 = k2 + 1
              ctmp(k2) = array(k1)
              k1 = k1 - 1
              if (k1 == kk) exit
            enddo
            do
              k1 = kk + kspan
              k2 = k1 - jc * (k + perm(k))
              k = -perm(k)
              do
                  array(k1) = array(k2)
                  k1 = k1 - 1
                  k2 = k2 - 1
                  if (k1 == kk) exit
              enddo
              kk = k2
              if (k == j) exit
            enddo
            k1 = kk + kspan
            k2 = 0
            do
              k2 = k2 + 1
              array(k1) = ctmp(k2)
              k1 = k1 - 1
              if (k1 == kk) exit
            enddo
            if (jj == 0) exit
        enddo
        if (j == 1) exit
      enddo
    enddo

    deallocate( perm, ctmp )
  endsubroutine ! permute

  integer function test( )
    write(*,*,iostat=test) 'FFT: no module test implemented!'
  endfunction ! test

endmodule ! FFT_tools

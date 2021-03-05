#ifdef DEBUG_ALL
#define DEBUG
#endif

! #define DEBUG
! #define FULL_DEBUG

#ifdef SINGLE_PRECISION
!!! replace the Lapack routine names
#define DSYEV ssyev
#define ZHEEV cheev
#define DSYGV ssygv
#define ZHEGV chegv
#define DGETRF sgetrf

#endif


#ifdef R1_C2
! This file will be preprocessed twice, first
! with R1_C2 == 1, and then with R1_C2 == 2
! then the two parts are concatenated to one source.
! This requires that the module head has to be
! appearing, when R1_C2 == 1 and the line "end module <name>"
! with R1_C2 == 2.
#if R1_C2 == 1
! module head

!! @author Paul Baumeister
!! @version 3.0
!!
!! wrapper module for calls to LAPACK functions
module LAPACK
!!! nice wrappers to avoid LAPACK calls
#ifdef DEBUG
  use configuration, only: o ! output unit
#endif
implicit none
  private ! default for this module namespace
  character(len=*), parameter, private :: sym = 'LAPACK' !! module symbol

  public :: diagonalize
  public :: generalized_eig
  public :: LU_decompose
  public :: solve_Ax_b
  public :: invert
  public :: invert3x3
  public :: determinant3x3
  public :: LU_decomposition_3x3
  public :: LU_doolittle
#ifdef EXTENDED
  public :: check_matrix
  public :: test
#endif

  interface diagonalize ! symmetric/hermitian
    module procedure diagonalize_r, diagonalize_c
  endinterface

  interface generalized_eig ! symmetric/hermitian
    module procedure generalized_eig_r, generalized_eig_c
  endinterface

  interface LU_decompose
    module procedure LU_decompose_r
  endinterface

  interface LU_doolittle
    module procedure doolittle
  endinterface

  interface solve_Ax_b
    module procedure solve_Ax_b_r
  endinterface


  interface invert
    module procedure invert_newarray, invert_replace
  endinterface

  interface invert3x3
    module procedure invert3x3_r
  endinterface

  interface determinant3x3
    module procedure determinant3x3_r
  endinterface

#ifndef DEBUG
  integer, parameter          :: o = 6 !! output unit 0: no output 6:stdout
#endif

#ifdef EXTENDED
  interface check_matrix
    module procedure check_symmetric_matrix, check_hermitian_matrix
  endinterface
#endif

  contains

  subroutine LU_decomposition_3x3( A, L, U )
    real, intent(in)  :: A(3,3)
    real, intent(out) :: L(3,3), U(3,3)
    U = 0.
    L = 0.

    U(1,1) = A(1,1)                                   ! I
    U(1,2) = A(1,2)                                   ! II
    U(1,3) = A(1,3)                                   ! III
    L(2,1) = A(2,1)/A(1,1)                            ! IV
    U(2,2) = A(2,2)-L(2,1)*U(1,2)                     ! V
    U(2,3) = A(2,3)-L(2,1)*U(1,3)                     ! VI
    L(3,1) = A(3,1)/U(1,1)                            ! VII
    L(3,2) = ( A(3,2) - L(3,1)*U(1,2) )/U(2,2)        ! VIII
    U(3,3) = A(3,3) - L(3,1)*U(1,3) - L(3,2)*U(2,3)   ! IX
    L(1,1) = 1.
    L(2,2) = 1.
    L(3,3) = 1.
  endsubroutine ! LU_decomposition_3x3


  subroutine doolittle( Ain, L, U, n )
    real, intent(in)    :: Ain(:,:)
    real, intent(out)   :: L(:,:), U(:,:)
    integer, intent(in) :: n ! dimension

    integer             :: i,j,k
    real                :: A(n,n)
    real                :: coeff
  
    U = 0.
    L = 0.

    A = Ain ! copy A in into local A
    !! magic starts now
    do k=1, n-1
      do i=k+1,n
	coeff=A(i,k)/A(k,k)
	L(i,k) = coeff
	do j=k+1,n
         A(i,j) = A(i,j)-coeff*A(k,j)
	end do
      end do
    end do
  
    do i=1,n
      L(i,i) = 1.0
    end do

    do j=1,n
      do i=1,j
	U(i,j) = a(i,j)
      end do
    end do
    
   A = matmul(L, U)
   !!write (*,*) A-Ain

  endsubroutine ! doolittle


  integer function LU_decompose_r( a, l, u ) result( info )
    real, intent(in)     :: a(:,:) !! A(M,N)
    real, intent(out)    :: l(:,:) !! L(:,:)
    real, intent(out)    :: u(:,:) !! U(:,:)

    integer              :: m, n ! dimensions
    integer              :: i !, j ! indices
    real, allocatable    :: p(:,:) ! pivoting matrix
    integer, allocatable :: ipiv(:) ! pivoting list
    logical              :: piv
#ifdef DEBUG
    character(len=*), parameter :: fun = ' LU_decompose: '
    iounit_t, parameter  :: o=6
    real, allocatable    :: plu(:,:) ! test matrix
#endif

    m = size( a, 1 ) ; n = size( a, 2 )
    if( m /= n ) stop 'LAPACK LU_decompose: only square matrices implemented!'
#ifdef DEBUG
    if( size(u,1) /= n ) stop 'LAPACK LU_decompose: dim#1 of U does not match dim#1 of A.'
    if( size(u,2) /= n ) stop 'LAPACK LU_decompose: dim#2 of U does not match dim#2 of A.'
    if( size(l,1) /= n ) stop 'LAPACK LU_decompose: dim#1 of L does not match dim#1 of A.'
    if( size(l,1) /= n ) stop 'LAPACK LU_decompose: dim#2 of L does not match dim#2 of A.'
    if(o>0) write(o,'(A)') ''
#endif
    allocate( ipiv(min(m,n)) )
    ! copy A, because A is read-only (intent(in))
    l = a
    !    DGETRF ( M, N, A, LDA, IPIV, INFO )
    ! http://www.netlib.org/lapack/double/dgetrf.f
    call DGETRF ( m, n, l, m, ipiv, info )
!! *  M       (input) INTEGER
!! *          The number of rows of the matrix A.  M >= 0.
!! *
!! *  N       (input) INTEGER
!! *          The number of columns of the matrix A.  N >= 0.
!! *
!! *  A       (input/output) REAL array, dimension (LDA,N)
!! *          On entry, the M-by-N matrix to be factored.
!! *          On exit, the factors L and U from the factorization
!! *          A = P*L*U; the unit diagonal elements of L are not stored.
!! *
!! *  LDA     (input) INTEGER
!! *          The leading dimension of the array A.  LDA >= max(1,M).
!! *
!! *  IPIV    (output) INTEGER array, dimension (min(M,N))
!! *          The pivot indices; for 1 <= i <= min(M,N), row i of the
!! *          matrix was interchanged with row IPIV(i).
!! *
!! *  INFO    (output) INTEGER
!! *          = 0:  successful exit
!! *          < 0:  if INFO = -i, the i-th argument had an illegal value
!! *          > 0:  if INFO = i, U(i,i) is exactly zero. The factorization
!! *                has been completed, but the factor U is exactly
!! *                singular, and division by zero will occur if it is used
!! *                to solve a system of equations.

    u = 0. ! init
    if( info /= 0 ) then ; l = 0. ; return ; endif

    allocate( p(n,n) ) ; p = 0. ! init

    piv = .false.
    do i = 1, n
      piv = piv .or. ( ipiv(i) /= i )
      u(1:i,i) = l(1:i,i)
      l(1:i,i) = 0.
      l(  i,i) = 1. ! create the unit diagonal elements of L
      p(i,ipiv(i)) = 1.
#ifdef DEBUG
      if(o>0) write(o,'(3A,I2,A,99F10.6)') sym, fun, '  U(:,',i,') =', u(:,i)
    enddo ! i
    if( piv ) then
      if(o>0) write(o,'(A)') ''
      do i = 1, n
        if(o>0) write(o,'(3A,I2,A,99F10.6)') sym, fun, '  P(:,',i,') =', p(:,i)
      enddo ! i
    endif ! piv
    if(o>0) write(o,'(A)') ''
    do i = 1, n
      if(o>0) write(o,'(3A,I2,A,99F10.6)') sym, fun, '  L(:,',i,') =', l(:,i)
#endif
    enddo ! i

#ifdef DEBUG
    allocate( plu(n,n) )
    if(o>0) write(o,'(A)') ''
    plu = matmul( p, matmul( l, u ) )
    do i = 1, n
      if(o>0) write(o,'(3A,I2,A,99F10.6)') sym, fun, 'PLU(:,',i,') =', plu(:,i)
    enddo ! i
    if(o>0) write(o,'(A)') ''
#endif
    ! Pivoting has not been tested !!!
    if( piv ) stop 'LAPACK LU_decompose: Pivoting matrix might be incorrect (transposed)'
  endfunction ! LU_decompose



! end of module head
#endif
! from here, everything will be compiled twice

#if R1_C2 == 1
#define REAPLEX real
#else
#define REAPLEX complex
#endif


#if R1_C2 == 1
  !! diagonalization of a symmetric matrix
  integer function diagonalize_r( &
#else
  !! diagonalization of a hermitian matrix
  integer function diagonalize_c( &
#endif
                                   mat, eigvec, eigval ) result( info )
    REAPLEX, intent(inout)    :: mat(:,:)
    REAPLEX, intent(out)      :: eigvec(:,:)
    real, intent(out)         :: eigval(:)

    character, parameter      :: JOBZ = 'V', UPLO = 'U'
    integer, save             :: lwork = 0
    REAPLEX, allocatable, save:: work(:)
#if R1_C2 == 2
    real, allocatable, save   :: rwork(:) ! is only allocated, if R1_C2 == 2
#endif
    integer                   :: n

    n = size( mat, 1 )
#ifdef DEBUG
    if( size( mat, 2 ) /= n )    stop 'LAPACK: diagonalize: matrix MAT is not square.'
    if( size( eigval, 1 ) /= n ) stop 'LAPACK: diagonalize: dim of EIGVAL does not match matrix dim.'
    if( size( eigvec, 1 ) /= n ) stop 'LAPACK: diagonalize: dim #1 of EIGVEC does not match matrix dim.'
    if( size( eigvec, 2 ) /= n ) stop 'LAPACK: diagonalize: dim #2 of EIGVEC does not match matrix dim.'
#endif

    if( lwork < n*n ) then
      if( allocated(work) ) deallocate( work ) ; lwork = n*n ; allocate( work(lwork) )
#if R1_C2 == 2
      if( allocated(rwork) ) deallocate( rwork ) ; allocate( rwork(3*n) )
#endif
    endif ! lwork < n*n


#if R1_C2 == 1
! R1_C2 == 1

         ! double precision symmetric eigenvalues and eigenvectors
#ifdef NOLAPACK
    stop 'LAPACK required for solving the symmetric eigenvalue problem. http://www.netlib.org/lapack/double/dsyev.f'
#else
    ! http://www.netlib.org/lapack/double/dsyev.f
    call DSYEV ( JOBZ, UPLO, n, mat, n, eigval, work, lwork, info )
#endif


! R1_C2 == 1
#else
! R1_C2 == 2

         ! double complex hermitian eigenvalues and eigenvectors
#ifdef NOLAPACK
    stop 'LAPACK required for solving the hermitian eigenvalue problem. http://www.netlib.org/lapack/complex16/zheev.f'
#else
    ! http://www.netlib.org/lapack/complex16/zheev.f
    call ZHEEV ( JOBZ, UPLO, n, mat, n, eigval, work, lwork, rwork, info )
#endif

! R1_C2 == 2
#endif
    if( info == 0 ) eigvec(:,:) = mat(:,:)
  endfunction ! diagonalize



#if R1_C2 == 1
  integer function generalized_eig_r( &
#else
  integer function generalized_eig_c( &
#endif
                                       a, b, e ) result( info )
  implicit none
    REAPLEX, intent(inout)          :: a(1:,1:)
    REAPLEX, intent(inout)          :: b(1:,1:)
    real, intent(out)               :: e(1:)

    character(len=*), parameter     :: fun = ' generalized_eig: '
    integer, parameter              :: itype = 1 ! A*x = lambda*B*x
    character, parameter            :: JOBZ = 'V', UPLO = 'U'
#if R1_C2 == 1
    character(len=*), parameter     :: RC = 'real'
#else
    character(len=*), parameter     :: RC = 'complex'
    real, allocatable, save         :: rwork(:) ! is only allocated if R1_C2 == 2
#endif
    integer, save                   :: lwork = 0
    REAPLEX, allocatable, save      :: work(:)

#ifdef FULL_DEBUG
! #if R1_C2 == 1
!     real, allocatable         :: b_copy(:,:)
! #else
!     complex, allocatable      :: b_copy(:,:)
! #endif
#endif

    integer                         :: i, n

    n = size( a, 1 ) ; if( n < 1 ) return
!     if(o>0) write(o,'(4A,I6,9A)') sym, fun, RC, ' matrix dimension =', n

#ifdef DEBUG
    if( size( a, 2 ) /= n ) stop 'LAPACK: generalized_eig: matrix A is not square.'
    if( size( b, 1 ) /= n ) stop 'LAPACK: generalized_eig: dim #1 of matrix B does not match matrix size.'
    if( size( b, 2 ) /= n ) stop 'LAPACK: generalized_eig: dim #2 of matrix B does not match matrix size.'
    if( size( e, 1 ) /= n ) stop 'LAPACK: generalized_eig: dim of eigenvalue vector E does not match matrix dim.'
#endif

#ifdef FULL_DEBUG
  if( n > 7 ) then
    if(o>0) then
      write(o,'(3A)') sym, fun, 'write A to fort.17 and B to fort.18'
      do i = 1, n
        write(17,'(99F16.12)') a(:,i)
      enddo ! i
      write(17,'(99F16.12)')
!       stop 'LAPACK line 375'
      do i = 1, n
        write(18,'(99F16.12)') b(:,i)
      enddo ! i
      write(18,'(99F16.12)')
    endif ! o>0
  endif ! n > 7
!   allocate( b_copy(n,n) ) ; b_copy = b
#endif

    i = max(n*n,3*n-1)
    if( lwork < i ) then
      if( allocated(work) ) deallocate( work ) ; lwork = i ; allocate( work(lwork) )
    endif ! lwork < n*n


#if R1_C2 == 1
! R1_C2 == 1

         ! double precision symmetric generalized eigenvalues and eigenvectors
#ifdef NOLAPACK
    stop 'LAPACK required for solving the symmetric generalized eigenvalue problem. http://www.netlib.org/lapack/double/dsygv.f'
#else
    ! http://www.netlib.org/lapack/double/dsygv.f
    call DSYGV ( ITYPE, JOBZ, UPLO, n, a, n, b, n, e, work, lwork, info )
#endif


! R1_C2 == 1
#else
! R1_C2 == 2
    if( allocated(rwork) .and. size(rwork) < 3*n ) deallocate( rwork )
    if( .not. allocated(rwork) ) allocate( rwork(3*n) )

         ! double complex hermitian generalized eigenvalues and eigenvectors
#ifdef NOLAPACK
    stop 'LAPACK required for solving the hermitian generalized eigenvalue problem. http://www.netlib.org/lapack/complex16/zhegv.f'
#else
    ! http://www.netlib.org/lapack/complex16/zhegv.f
    call ZHEGV ( ITYPE, JOBZ, UPLO, n, a, n, b, n, e, work, lwork, rwork, info )
#endif

! R1_C2 == 2
#endif

    if( info == 0 ) return

    if( info < 0 ) then
      if(o>0) write(o,'(3A,I0,9A)') sym, fun, 'illegal value in argument #',-info
    elseif( info <= N ) then
      if(o>0) write(o,'(2A,I0,9A)') sym, fun, info, &
        ' off-diagonal elements of an intermediate tridiagonal form did not converge to zero.'
! #ifdef FULL_DEBUG
!       else ! info
!         if(o>0) write(o,'(3A,I3,9A)') sym, fun, 'the leading minor of order',  info - n, &
!           ' of B is not positive definite, B='
!         ! display a copy of b made on entry to this driver subroutine
!         if(o>0) then
!           do i = 1, n
!             write(o,'(999ES10.2E2)') b_copy(1:n,i)
!           enddo ! i
!         endif ! o/=0
! #else
! !         if(o>0) write(o,'(3A,I5,9A)') sym, fun, 'the leading minor of order',  info - n, &
! !           ' of B is not positive definite.'
! #endif
    endif ! info

#ifdef FULL_DEBUG
  if( n > 7 ) then
    if(o>0) then
      write(o,'(3A)') sym, fun, 'write Z to fort.19'
      do i = 1, n
        write(19,'(99F16.12)') a(:,i)
      enddo ! i
      write(19,'(99F16.12)')
    endif ! o>0
  endif ! n > 7
#endif
  endfunction ! generalized_eig


#ifdef EXTENDED
!+ extended



#if R1_C2 == 1
#define conjg(x) (x)
  integer function check_symmetric_matrix( &
#else
  integer function check_hermitian_matrix( &
#endif
                                            m, name ) result( nerr )
    REAPLEX, intent(in)             :: m(:,:)
    character(len=*), intent(in), optional      :: name

    character(len=*), parameter     :: fun = ' check_matrix: '
#ifdef RELATIVE_DEVIATION
    real, parameter                 :: THRESHOLD = 1.E-7 ! relative
    character(len=*), parameter     :: DEVCHAR = '  max.rel.dev='
#else
#ifdef SINGLE_PRECISION
    real, parameter                 :: THRESHOLD = 1.E-6 ! absolute
#else
    real, parameter                 :: THRESHOLD = 1.E-12 ! absolute
#endif
    character(len=*), parameter     :: DEVCHAR = '  max.dev='
#endif
    real, parameter                 :: THRES2 = THRESHOLD**2
    integer                         :: n, i, j, d, e, ix, jx
    real                            :: dev2, mx

    d = 0
    e = 0
    mx = 0. ; ix = 0 ; jx = 0

    n = size( m, 1 )
    if( size( m, 2 ) /= n ) then
      nerr = -666 ! matrix is not square
      if(o>0) write(o,'(9A)') sym, fun, 'matrix is not square'
      return
    endif

    do i = 1, n
      do j = 1, i-1 ! triangular loop, only off-diagonal elements
#ifdef RELATIVE_DEVIATION
        dev2 = abs2( m(i,j)-conjg(m(j,i)) )/abs2(m(i,j))
#else
        dev2 = abs2( m(i,j)-conjg(m(j,i)) )
#endif
        if( dev2 > THRES2 ) e = e+1
        if( dev2 > mx ) then
          mx = dev2 ; ix = i ; jx = j
        endif ! dev2 > mx
!         mx = max( mx, dev2 )

!         if( abs2( m(i,j)-complex_conjugate(m(j,i)) ) > THRES2 * abs2(m(i,j)) ) e = e+1
      enddo ! j
      ! check if the diagonal elements are real
#ifdef RELATIVE_DEVIATION
      if( abs2( m(i,i) - real(m(i,i)) ) > THRES2 * abs2(m(i,i)) ) d = d+1
#else
      if( abs2( m(i,i) - real(m(i,i)) ) > THRES2 ) d = d+1
#endif
    enddo ! i

    mx = sqrt( mx )

    nerr = 2 * e + d
    if( o/=0 .and. nerr > 0 ) then
      if( present( name ) ) then
        write(o,'(2A,2(A,I4),2(A,I8),4A,E10.3,9A)') sym, fun, 'dim:', n, &
          ' diag:', d, ' offdiag:', e, ' errors:', nerr, ' for "', trim(name), '",', DEVCHAR, mx
      else  ! present name
        write(o,'(2A,2(A,I4),2(A,I8),A,E10.3,9A)') sym, fun, 'dim:', n, &
          ' diag:', d, ' offdiag:', e, ' errors:', nerr, DEVCHAR, mx
      endif ! present name
!       ! output value and position of the largest deviation
!       if(ix/=0) write(o,'(3A,E10.3,A,2I4,9A)') sym, fun, &
!         DEVCHAR, mx, ' at [', ix, jx, ']' 
    endif ! o/=0 and nerr > 0

  contains

    real function abs2( rc ) ! abs square
      REAPLEX, intent(in) :: rc
#if R1_C2 == 1
      abs2 = rc*rc
#undef conjg
#else
      abs2 = real(rc)**2 + aimag(rc)**2
#endif
    endfunction ! abs2

  endfunction ! check_symmetric/hermitian_matrix

!- extended
#endif


! until here, everything will be compiled twice
#if R1_C2 == 2
! module tail


  integer function invert_newarray( a, inv ) result( ist )
    real, intent(in)                :: a(:,:) !! matrix
    real, intent(out)               :: inv(:,:) !! inverse

    character(len=*), parameter     :: fun = ' invert3x3: '
    real                            :: det = 0.
    integer                         :: n
    n = size(a,1) ; if( size(a,2) /= n ) stop 'invert: matrix A must be square!'
    if( size(inv,1) /= n ) stop 'invert: dim#1 of matrix INV does not match dim of A'
    if( size(inv,2) /= n ) stop 'invert: dim#2 of matrix INV does not match dim of A'

    selectcase( n )
    case( 0 ) ; return
    case( 1 ) ; det = a(1,1) ; inv(1,1) = 1./a(1,1)
    case( 2 ) ; det = invert2x2_r( a, inv )
    case( 3 ) ; det = invert3x3_r( a, inv )
    case( 4:) ; stop 'invert: LAPACK needs to be implemented here!' ! do a real matrix inversion
    case default ; stop 'invert: size of A is < 0, impossible!'
    endselect ! n
    ist = -1 ; if( det*det > 1E-16 ) ist = 0
  endfunction ! invert_newarray

  integer function invert_replace( a ) result( ist )
    real, intent(inout)             :: a(:,:) !! matrix -> inverse

    character(len=*), parameter     :: fun = ' invert3x3: '
    real                            :: inv(size(a,1),size(a,2))
    ist = invert_newarray( a, inv )
    if( ist == 0 ) then ; a = inv ; else ; a = 0. ; endif
  endfunction ! invert_replace

  !! determinant of a 2 by 2 matrix
  real function determinant2x2_r( a ) result( det )
    real, intent(in) :: a(2,2)
    det = a(1,1)*a(2,2) - a(2,1)*a(1,2)
  endfunction ! determinant2x2

  !! inverts a 2 by 2 matrix, if the determinant is non-zero
  real function invert2x2_r( a, inv ) result( det ) !! result: determinant of matrix a
    real, intent(in)                :: a(2,2) !! matrix
    real, intent(out)               :: inv(2,2) !! inverse

    det = determinant2x2_r( a )
    if( det == 0. ) then
#ifdef DEBUG
      if(o>0) write(o,'(9A)') sym, ' invert2x2: determinant = 0.'
#endif
      inv = 0.
      return
    endif ! det == 0
    inv(1,1) =  a(2,2)/det
    inv(1,2) = -a(1,2)/det
    inv(2,1) = -a(2,1)/det
    inv(2,2) =  a(1,1)/det
  endfunction ! invert2x2

  !! determinant of a 3 by 3 matrix
  real function determinant3x3_r( a ) result( det )
    real, intent(in) :: a(3,3)
    det = a(1,1)*a(2,2)*a(3,3) + a(1,2)*a(2,3)*a(3,1) &
        + a(2,1)*a(3,2)*a(1,3) - a(1,3)*a(2,2)*a(3,1) &
        - a(2,3)*a(3,2)*a(1,1) - a(2,1)*a(1,2)*a(3,3)
  endfunction ! determinant3x3

  !! inverts a 3 by 3 matrix, if the determinant is non-zero
  real function invert3x3_r( a, inv ) result( det ) !! result: determinant of matrix a
    real, intent(in)                :: a(3,3) !! matrix
    real, intent(out)               :: inv(3,3) !! inverse

    det = determinant3x3_r( a )
    if( det == 0. ) then
#ifdef DEBUG
      if(o>0) write(o,'(9A)') sym,' invert3x3: determinant = 0.'
#endif
      inv = 0.
      return
    endif ! det == 0
    inv(1,1) = (a(2,2)*a(3,3)-a(2,3)*a(3,2))/det
    inv(1,2) = (a(1,3)*a(3,2)-a(1,2)*a(3,3))/det
    inv(1,3) = (a(1,2)*a(2,3)-a(2,2)*a(1,3))/det
    inv(2,1) = (a(2,3)*a(3,1)-a(2,1)*a(3,3))/det
    inv(2,2) = (a(1,1)*a(3,3)-a(3,1)*a(1,3))/det
    inv(2,3) = (a(1,3)*a(2,1)-a(1,1)*a(2,3))/det
    inv(3,1) = (a(2,1)*a(3,2)-a(2,2)*a(3,1))/det
    inv(3,2) = (a(1,2)*a(3,1)-a(1,1)*a(3,2))/det
    inv(3,3) = (a(1,1)*a(2,2)-a(1,2)*a(2,1))/det
  endfunction ! invert3x3


  integer function solve_Ax_b_r( A, b, x ) result( info )
    real, intent(inout), target  :: A(:,1:), b(:)
    real, intent(out), target :: x(:)


    real, pointer :: A_p(:,:),x_p(:)



    integer              :: n
    integer, allocatable :: ipivot(:)

#ifdef NOLAPACK
    stop 'solve_Ax_b: this version has been compiled without the LAPACK library.'
#else
    n = size( A, 1 )
    if( size( A, 2 ) /= n ) stop 'solve_Ax_b: dim #2 of matrix A is wrong. A not square.'
    if( size( b, 1 ) /= n ) stop 'solve_Ax_b: dim #1 of vector B is wrong.'
    if( size( x, 1 ) /= n ) stop 'solve_Ax_b: dim #1 of vector X is wrong.'

    allocate( ipivot(n) )

    if(is_contiguous(A)) then
      A_p=>A
    else
      allocate(A_p,source=A)
    endif

    if(is_contiguous(x)) then
      x_p=>x
    else
      allocate(x_p,mold=x)
    endif

    x_p = b ! copy b into x, so that the solver gets b stored in x

    ! usage of LAPACK DGESV
    call DGESV( n, 1, A_p(:,1), n, ipivot, x_p, n, info )

    if( .not. is_contiguous(A) ) then
      A=A_p
      deallocate(A_p)
    endif

    if( .not. is_contiguous(x) ) then
      x=x_p
      deallocate(x_p)
    endif

    if( info /= 0 .and. o/=0 ) write(o,'(2A,I0)') sym,' solve Ax=b: DGESV returned info = ',info
    deallocate( ipivot )
#endif
  endfunction ! solve_Ax_b


#ifdef EXTENDED
!+ extended

  integer function test( ) result( ios )
    write(*,*,iostat=ios) __FILE__,' no module test implemented!'
  endfunction ! test

!- extended
#endif

endmodule ! LAPACK
! end of module tail
#endif
! end of DoublePreprocessing
#endif

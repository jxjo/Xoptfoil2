! MIT License
! Copyright (c) 2025 Jochen Guenzel

module spline

  ! 1D nd 2D cubic spline 

  use os_util

  implicit none
  private

  integer, parameter, public  :: NOT_A_KNOT = -999    ! boundary condition, third deriv = 0 
  integer, parameter, public  :: NATURAL    = 999     ! boundary condition, second deriv = 0 

  interface eval_1D
    module procedure eval_1D_array        ! eval array
    module procedure eval_1D_scalar       ! eval scalar
  end interface 

  interface eval_spline 
    module procedure eval_2D_array        ! eval array
    module procedure eval_2D_scalar       ! eval scalar
  end interface 

  type spline_1D_type
    double precision, allocatable   :: x(:)                    ! initial x of spline
    double precision, allocatable   :: a(:), b(:), c(:), d(:)  ! the polynomal parameters 
    logical                         :: arccos                  ! arccos transformation of x (for high curv at 0.0) 
  end type spline_1D_type

  type spline_2D_type
    type(spline_1D_type)            :: splx, sply               ! x and y 1D splines 
    double precision, allocatable   :: s(:)                     ! arc length array 
  end type spline_2D_type


  public :: spline_1D_type
  public :: spline_1D
  public :: eval_1D

  public :: spline_2D_type
  public :: spline_2D
  public :: eval_spline
  public :: eval_spline_curvature

contains


  function spline_1D (x, y, boundary, arccos) result (spl)

    !----------------------------------------------------------------------------
    !! Build cubic spline based on x,y. x must be strongly ascending.
    !!
    !! x,y : array_like
    !! boundary : Type of boundary condition  - either 
    !!     'notaknot'   - at the first and last interior break, 
    !!                    even the third derivative is continuous - default 
    !!     'natural'    - the second derivate at start and end is zero 
    !!
    !! arccos : .true. an arccos transformation of x is done to avoid oscillation at le 
    !! Returns
    !! spline : spline_type 
    !----------------------------------------------------------------------------

    ! based on https://en.wikiversity.org/wiki/Cubic_Spline_Interpolation
    ! and      https://blog.scottlogic.com/2020/05/18/cubic-spline-in-python-and-alteryx.html

    ! Info     https://sepwww.stanford.edu/sep/sergey/128A/answers6.pdf for boundary conditions
    !          https://documents.uow.edu.au/~/greg/math321/Lec3.pdf 

    use math_util,    only : diff_1D, transformed_arccos

    double precision, intent(in)    :: x(:), y(:)
    integer, intent(in), optional   :: boundary
    logical, optional               :: arccos

    type (spline_1D_type)           :: spl   
    integer                         :: n, nA, nB, nC, nD, nM, nh 
    integer                         :: i, bnd
    double precision, allocatable   :: h(:), A(:), B(:), C(:), D(:), M(:) 
    
    n = size (x) 

    if (present (boundary)) then 
      bnd = boundary
    else
      bnd = NOT_A_KNOT
    end if 

    if (bnd == NOT_A_KNOT .and. n < 4) then 
      call my_stop ("Spline: NOT_A_KNOT must have at least 4 points")
    else if (n < 3) then 
      call my_stop ("Spline: ust have at least 3 points")
    else if (n /= size(y)) then 
      call my_stop ("Spline: Length of x,y is different")
    end if  

    ! optional arccos distribution to avoid oscillation at LE   
    
    spl%x = x 
    spl%arccos = .false.

    if (present(arccos)) then 
      if (arccos) then 
        spl%x = transformed_arccos (x)
        spl%arccos = .true.
      end if 
    end if 

    ! get delta h of x

    h = diff_1D (spl%x)                 ! the differences hi = xi+1 - xi  (nh = n-1)
    if (minval (h) <= 0d0) & 
      call my_stop ('Spline: x must be strictly ascending')

    ! build the tridiagonal matrix with simple, natural boundary condition
    call build_tridiagonalArrays (n, h, A, B, C)

    ! build the right hand side 
    D = build_targetArray (n, h, y)

    ! boundary conditions - overwrite boundaries of A, B, C, D

    nA = size(A) 
    nB = size(B) 
    nC = nB
    nD = size(D)
    nh = size(h)

    if (bnd == NATURAL) then 

      ! 1. der2(x0) and der2(xn) is known 
      !    special case: 'Natural' or 'Simple'   der2(x0) = der2(xn) = 0 
      !    Di = 2 * der2i
      C(1)  = 0d0
      A(nA) = 0d0  
      D(1)  = 0d0                 ! 2 * 5.0     # 2nd derivative test
      D(nD) = 0d0                 ! 2 * 3.0     # 2nd derivative test

      M = solve_tridiagonalsystem (A, B, C, D, reduced = .false.)

    else if (bnd == NOT_A_KNOT) then 

      !  2. not a knot  ( knot (x1) and (xn-1) is not a knot)
      !     der3(x0) = der3(x1)  and der3[xn-1] = der[xn]
      !     According to
      !     https://documents.uow.edu.au/~/greg/math321/Lec3.pdf
          
      !   in this case only a (n-2) x (n-2) matrix has be solved
      B(2)    = (2*h(2)     + h(1))     / h(2)                          ! diagonal - upper-left corner 
      B(nB-1) = (2*h(nh-1)  + h(nh))    / h(nh-1)                       ! diagonal - lower-right corner 
      C(2)    = (h(2)**2    - h(1)**2)  / (h(2) * (h(1)  + h(2)))       ! super diagonal 
      A(nA-1) = (h(nh-1)**2 - h(nh)**2) / (h(nh-1) * (h(nh-1) + h(nh))) ! sub diagonal 

      M = solve_tridiagonalsystem (A, B, C, D, reduced=.true.)

      ! evaluate the missing M0 and M-1 (eqauls derivate2 at x0 and xn) 
      nM = size(M)
      M(1)  = ((h(1)    + h(2))  * M(2)    - h(1)  * M(3))    / h(2)
      M(nM) = ((h(nh-1) + h(nh)) * M(nM-1) - h(nh) * M(nM-2)) / h(nh-1)

    end if 

    ! extract coefficients of polynoms
    allocate (spl%a(nA))
    allocate (spl%b(nB))
    allocate (spl%c(nC))
    allocate (spl%d(nD))
    spl%a = 0d0
    spl%b = 0d0
    spl%c = 0d0
    spl%d = 0d0

    do i = 1,nA
        spl%a(i) = y(i) 
        spl%b(i) = (y(i+1) - y(i)) / h(i) - h(i) * (3d0 * M(i) + (M(i+1) - M(i))) / 6d0
        spl%c(i) = M(i) / 2d0 
        spl%d(i) = (M(i+1) - M(i)) / (6d0 * h(i))
    end do 

    continue

  end function 



  subroutine build_tridiagonalArrays (n, h, A, B, C)

    ! returns the tridiagonal arrays A, B, C 
    !    with B[i] = 2
    ! 
    !    b0   c0    0    0             B - diagonal elements length n       
    !    a0   b1   c1    0             A - below length n-1
    !     0   a1   b2   c2             C - above length n-1 
    !     0    0   a2   b3


    integer, intent(in)                        :: n
    double precision, intent(in)               :: h(:) 
    double precision, allocatable, intent(out) :: A(:), B(:), C(:)
    integer :: i 
    
    allocate(A(n-1))
    A = 0d0
    do i = 1, size(A) - 1 
      A(i) = h(i) / (h(i) + h(i+1))
    end do 

    allocate(B(n))
    B = 2d0

    allocate(C(n-1))
    C= 0d0
    do i = 2, size(C)
      C(i) = h(i) / (h(i-1) + h(i))
    end do 

  end subroutine 



  function build_targetArray(n, h, y) result (D)

    ! returns the right hand side (rhs) array D 
    !   which is the "divided difference" f[xi-1, xi, xi+1]
    !   https://en.wikipedia.org/wiki/Divided_differences
    
    !   d0                            D - rhs array length n
    !   d1
    !   d2 

    integer, intent(in)                        :: n
    double precision, intent(in)               :: h(:), y(:) 
    double precision, allocatable  :: D(:) 
    integer                        :: i

    allocate (D(n))
    D = 0d0 

    do i = 2, n-1 
      D(i) = 6d0 * ((y(i+1) - y(i))/ h(i) - (y(i) - y(i-1)) / h(i-1)) / &
                   (h(i) + h(i-1))
    end do 
  end function 


  function solve_tridiagonalsystem (A, B, C, D, reduced) result (M)

    ! solves the tridiagonal system ABC * M = D  
    !
    ! when reduced the inner (n-2) x (n-2) matrix is solved (need for not a knot)
    ! https://gist.github.com/cbellei/8ab3ab8551b8dfc8b081c518ccd9ada9


    double precision, intent(in) :: A(:), B(:), C(:), D(:) 
    logical, intent(in)          :: reduced

    double precision, allocatable :: M(:) 
    double precision, allocatable :: ac(:), bc(:), cc(:), dc(:)
    double precision              :: mc
    integer :: di, iEnd, il, it, nbc, ndc 

    if (reduced) then 
      di = 1
    else
      di = 0 
    end if 

    iEnd = size(D) - di         ! number of equations 

    ac = A
    bc = B 
    cc = C 
    dc = D
  
    nbc = size(bc)
    ndc = size(dc)

    do it = (2+di), iEnd 
      mc = ac (it-1) / bc (it-1)
      bc(it) = bc(it) - mc * cc(it-1)
      dc(it) = dc(it) - mc * dc(it-1)
    end do 

    M = bc 
    M(nbc-di) = dc(ndc-di)/bc(nbc-di)

    do il = (iEnd-1), 1+di, - 1
      M(il) = (dc(il) - cc(il) * M(il+1)) / bc(il)
    end do 

  end function 



  function eval_1D_scalar (spl, xin, derivative) result (y) 

    !----------------------------------------------------------------------------
    !! evaluate spline or its derivatives.
    !!
    !! spline:  spline_type
    !! x:       Scalar at which to return the value of the spline or its derivatives. 
    !! der:     int, optional - The order of derivative of the spline to compute 
    !!
    !! Returns
    !! y  Scalar representing the spline function evaluated at x
    !----------------------------------------------------------------------------

    type (spline_1D_type), intent(in)  :: spl 
    double precision, intent(in)    :: xin
    integer, intent(in), optional   :: derivative 

    double precision        :: x, y, xj 
    integer                 :: j, nx, der 

    if (present(derivative)) then 
      der = derivative
    else 
      der = 0 
    end if 

    nx = size(spl%x)
    x  = xin 
    x  = max (x, spl%x(1)) 
    x  = min (x, spl%x(nx)) 

    ! get relative coordinate of x within interval j  

    do j = 1, nx - 1                            ! find interval 
      if (x <= spl%x(j+1)) exit
    end do 
    xj = x - spl%x(j)

    ! eval polynom 

    if (der == 0) then 
      y = spl%a(j) +  spl%b(j) * xj + spl%c(j) * xj**2 + spl%d(j) * xj**3
    else if (der == 1) then
      y =  spl%b(j) + 2d0 * spl%c(j) * xj + 3d0 * spl%d(j) * xj**2
    else if (der == 2) then
      y =  2d0 * spl%c(j) + 6d0 * spl%d(j) * xj
    else 
      y = 0d0 
    end if 

  end function 




  function eval_1D_array (spl, xin, derivative) result (y) 

    !----------------------------------------------------------------------------
    !! evaluate spline or its derivatives for array of x 
    !!
    !! spline:  spline_type
    !! x:       array at which to return the value of the spline or its derivatives. 
    !! der:     int, optional - The order of derivative of the spline to compute 
    !!
    !! Returns
    !! y  array representing the spline function evaluated at x
    !----------------------------------------------------------------------------

    use math_util,          only : transformed_arccos

    type (spline_1D_type), intent(in)  :: spl 
    double precision, intent(in)    :: xin (:) 
    integer, intent(in), optional   :: derivative 

    double precision, allocatable   :: x(:), y(:) 
    integer       :: nxin, i

    nxin = size (xin) 
    allocate (y(nxin))

    ! arccos transform x if spline was build with arccos 

    if (spl%arccos) then 
      x = transformed_arccos (xin)
    else 
      x = xin
    end if 

    ! eval y for each x 

    do i = 1, nxin 
      y(i) = eval_1D_scalar (spl, x(i), derivative)
    end do 

  end function 
  


  ! --- Spline_2D ---------------------------------------------------------------


  function spline_2D (x, y, boundary) result (spl)

    !----------------------------------------------------------------------------
    !! Build cubic "D spline based on x,y. x must be strongly ascending.
    !!
    !! x,y : array_like
    !! boundary : Type of boundary condition  - either 
    !!     'notaknot'   - at the first and last interior break, 
    !!                    even the third derivative is continuous - default 
    !!     'natural'    - the second derivate at start and end is zero 
    !!
    !! Returns
    !! spline : spline_2D_type 
    !----------------------------------------------------------------------------

    ! based on https://en.wikiversity.org/wiki/Cubic_Spline_Interpolation
    ! and      https://blog.scottlogic.com/2020/05/18/cubic-spline-in-python-and-alteryx.html

    ! Info     https://sepwww.stanford.edu/sep/sergey/128A/answers6.pdf for boundary conditions
    !          https://documents.uow.edu.au/~/greg/math321/Lec3.pdf 


    double precision, intent(in)    :: x(:), y(:)
    integer, intent(in), optional   :: boundary

    type (spline_2D_type)           :: spl

    ! calc and normalize arc length 
    spl%s = calc_arc_length (x,y) 

    ! build x and y 1D splines 
    spl%splx = spline_1D ( spl%s, x, boundary)
    spl%sply = spline_1D ( spl%s, y, boundary)

  end function 



  function calc_arc_length (x, y) result (s)

    !! calc arc length array s of spline

    use math_util,    only : diff_1D
    double precision, intent(in)    :: x(:), y(:)
    double precision, allocatable   :: dx(:), dy(:), ds (:), s(:)
    integer     :: i

    dx = diff_1D (x)
    dy = diff_1d (y)

    ds = [ 0d0, (dx**2 + dy**2)** 0.5d0 ]           ! add 0d0 at the beginning...

    allocate (s (size(ds)))
    s = 0d0 

    do i = 2, size(s)
      s(i) = s(i-1) + ds(i)
    end do 

  end function 


  subroutine eval_2D_array (spl, s, x, y, derivative)  

    !----------------------------------------------------------------------------
    !! evaluate spline or its derivatives 
    !!
    !! spline:      2D spline_type
    !! s:           array of arc length at which to return the value 
    !!              of the spline or its derivatives. 
    !! derivative:  int, optional - The order of derivative of the spline to compute 
    !!
    !! Returns
    !! x, y:        array representing the spline function evaluated at u 
    !!              which is x,y or dx,dy or ddx, ddy 
    !----------------------------------------------------------------------------

    type (spline_2D_type), intent(in) :: spl 
    double precision, intent(in)      :: s (:) 
    double precision, allocatable, intent(out) :: x(:) , y(:) 
    integer, intent(in), optional     :: derivative 

    ! eval 1D x,y (or derivatives) 
    x = eval_1D (spl%splx, s, derivative)
    y = eval_1D (spl%sply, s, derivative)

  end subroutine




  subroutine eval_2D_scalar (spl, s, x, y, derivative)  

    !----------------------------------------------------------------------------
    !! evaluate spline or its derivatives 
    !!
    !! spline:      2D spline_type
    !! s:           arc length at which to return the value 
    !!              of the spline or its derivatives. 
    !! derivative:  int, optional - The order of derivative of the spline to compute 
    !!
    !! Returns
    !! x, y:        ascalar values representing the spline function evaluated at u 
    !!              which is x,y or dx,dy or ddx, ddy 
    !----------------------------------------------------------------------------

    type (spline_2D_type), intent(in) :: spl 
    double precision, intent(in)      :: s 
    double precision, intent(out)     :: x , y 
    integer, intent(in), optional     :: derivative 

    ! eval 1D x,y (or derivatives) 
    x = eval_1D (spl%splx, s, derivative)
    y = eval_1D (spl%sply, s, derivative)

  end subroutine


  function eval_spline_curvature (spl, s) result(curv) 

    !----------------------------------------------------------------------------
    !! evaluate curvature of spl at its knots  
    !! spl:         2D spline_type
    !! s:           array of arc length at which to return the value 
    !!              of the spline or its derivatives. 
    !! Returns
    !! curv:        array representing curvature evaluated at u 
    !----------------------------------------------------------------------------

    type (spline_2D_type), intent(in) :: spl 
    double precision, intent(in)      :: s(:) 

    double precision, allocatable     :: curv (:) 
    double precision, allocatable     :: dx(:), dy(:), ddx(:), ddy(:)

    call eval_2D_array (spl, s,  dx,  dy, derivative=1)
    call eval_2D_array (spl, s, ddx, ddy, derivative=2)

    curv = (ddy * dx - ddx *dy) / (dx**2 + dy**2) ** 1.5d0

  end function



  end module 
  
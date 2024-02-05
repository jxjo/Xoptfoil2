! MIT License
! Copyright (c) 2024 Jochen Guenzel

module spline

  ! 1D nd 2D cubic spline 

  implicit none
  private

  integer, parameter, public  :: NOT_A_KNOT = -999    ! boundary condition, third deriv = 0 
  integer, parameter, public  :: NATURAL    = 999     ! boundary condition, second deriv = 0 

  public:: spline_type

  type spline_type
    double precision, allocatable   :: x(:), y(:) 
    double precision, allocatable   :: a(:), b(:), c(:), d(:)  
  end type spline_type


contains


  function spline1D (x, y, boundary) result (spline)

    !----------------------------------------------------------------------------
    !! Build cubic spline based on x,y. x must be strongly ascending.
    !!
    !! x,y : array_like
    !! boundary : Type of boundary condition  - either 
    !!     'notaknot'   - at the first and last interior break, 
    !!                    even the third derivative is continuous - default 
    !!     'natural'    - the second derivate at start and end is zero 
    !!
    !! Returns
    !! spline : spline_type 
    !----------------------------------------------------------------------------

    use math_deps,    only : diff_1D

    double precision, intent(in)    :: x(:), y(:)
    integer, intent(in), optional   :: boundary

    type (spline_type)            :: spline   
    integer                       :: n, nA, nB, nC, nD, nM, nh 
    integer                       :: i, bnd
    double precision, allocatable :: h(:), A(:), B(:), C(:), D(:), M(:) 
    
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

    ! keep for later use 
    spline%x = x
    spline%y = y

    h = diff_1D (x)                 ! the differences hi = xi+1 - xi  (length n-1)
    if (minval (h) <= 0d0) & 
      call my_top ('Spline: x must be strictly ascending')

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
    nM = size(M)

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
      M(1)  = ((h(1)    + h(2))  * M(2)    - h(1)  * M(3))    / h(2)
      M(nM) = ((h(nh-1) + h(nh)) * M(nM-1) - h(nh) * M(nM-2)) / h(nh-1)

    end if 

    ! extract coefficients of polynoms
    allocate (spline%a(nA))
    allocate (spline%b(nB))
    allocate (spline%c(nC))
    allocate (spline%c(nD))
    spline%a = 0d0
    spline%b = 0d0
    spline%c = 0d0
    spline%d = 0d0

    do i = 1,nA
        spline%a(i) = y(i) 
        spline%b(i) = (y(i+1) - y(i)) / h(i) - h(i) * (3d0 * M(i) + (M(i+1) - M(i))) / 6d0
        spline%c(i) = M(i) / 2d0 
        spline%d(i) = (M(i+1) - M(i)) / (6d0 * h(i))
    end do 

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
    integer :: i, di, iEnd, il, it, nbc, ndc 

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

    do il = (iEnd-2), 1+di, - 1
      M(il) = (dc(il) - c(il) * M(il+1)) / bc(il)
    end do 

  end function 

end module 
  
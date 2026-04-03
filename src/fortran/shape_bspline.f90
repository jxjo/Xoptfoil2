! MIT License
! Copyright (c) 2026 jxjo

module shape_bspline
   
  !-------------------------------------------------------------------------
  ! B-spline based airfoil generation, modification, ...
  !-------------------------------------------------------------------------
 
  use os_util 
  use print_util
  use math_util,         only : diff_1D, cumsum, clip, find_closest_index, linspace, interp_vector, cosine_distribution
  use math_util,         only : curve_spec_type
  use string_util, only : read_file_to_string, json_get_string, json_get_section, &
                          json_get_array, json_get_integer, &
                          json_write_array, json_write_string, json_write_integer, stri
  
  implicit none
  private
 
  ! --- B-spline constants ---------------------------------------------------
  
  integer, parameter :: BSPLINE_DEGREE = 4              ! Quintic B-splines (degree 4)
  logical, parameter :: UNIFORM_KNOTS = .true.          ! Use uniform clamped knot vectors
  double precision, parameter :: LE_BUNCH = 0.84d0      ! Paneling bunching parameter at LE
  double precision, parameter :: TE_BUNCH = 0.70d0      ! Paneling bunching parameter at TE

  ! --- bspline types --------------------------------------------------------- 

  type shape_bspline_type
    integer                       :: ndv
    integer                       :: ncp_top, ncp_bot
    double precision              :: initial_perturb
  end type

  type, extends(curve_spec_type) :: bspline_spec_type
    !! B-spline curve specification - extends curve_spec_type
    !! Inherits px(:), py(:) from parent
  end type bspline_spec_type

  public :: bspline_spec_type, shape_bspline_type
  public :: BSPLINE_DEGREE, UNIFORM_KNOTS


  ! --- bspline functions ----------------------------------------------------- 


  public :: bspline_eval 
  public :: bspline_eval_side
  public :: bspline_round_decimals
  interface bspline_eval_1D
     module procedure bspline_eval_1D_array
     module procedure bspline_eval_1D_scalar
  end interface 
  public :: bspline_eval_1D
  public :: bspline_curvature
  public :: bspline_le_curvature
  public :: bspline_eval_y_on_x
  public :: bspline_violates_constraints
 
  ! bspline from/to design variables   

  public :: map_dv_to_bspline
  public :: bspline_get_dv0, get_initial_bspline
  public :: bspline_get_dv_initial_perturb
  public :: ncp_to_ndv

  public :: u_distribution_bspline
  public :: u_of_arc_fractions_bspline

  ! file function 

  public :: is_bspline_file, read_bspline_file, write_bspline_file

  public :: print_bspline_spec

  ! knot vector functions
  
  public :: generate_uniform_knots


  ! --- private --------------------------------------------------------------
  
  type bound_type 
    double precision              :: min, max
  end type bound_type

contains

  !-----------------------------------------------------------------------------
  ! Knot vector generation
  !-----------------------------------------------------------------------------

  function generate_uniform_knots(ncp, degree) result(knots)
    !! Generate a uniform clamped knot vector
    !! ncp: number of control points
    !! degree: spline degree
    integer, intent(in) :: ncp, degree
    double precision, allocatable :: knots(:)
    
    integer :: num_knots, i, num_interior
    
    ! Total number of knots: ncp + degree + 1
    num_knots = ncp + degree + 1
    allocate(knots(num_knots))
    knots = 0d0
    
    ! Clamp at start (degree+1 zeros)
    knots(1:degree+1) = 0d0
    
    ! Interior knots (if any) linearly spaced  
    if (num_knots > 2 * (degree + 1)) then
      num_interior = num_knots - 2 * (degree + 1)
      do i = 1, num_interior
        knots(degree + 1 + i) = dble(i) / dble(num_interior + 1)
      end do
    end if
    
    ! Clamp at end (degree+1 ones)
    knots(num_knots - degree : num_knots) = 1d0
    
  end function



  !-----------------------------------------------------------------------------
  ! B-spline basis function (Cox-de Boor recursion)
  !-----------------------------------------------------------------------------

  recursive function basis_function(i, k, u, knots, ncp) result(N)
    !! Evaluate B-spline basis function N_{i,k}(u) using Cox-de Boor recursion
    !! i: basis function index (1-based in Fortran)
    !! k: degree
    !! u: parameter value
    !! knots: knot vector
    !! ncp: number of control points
    integer, intent(in) :: i, k, ncp
    double precision, intent(in) :: u
    double precision, intent(in) :: knots(:)
    double precision :: N
    
    double precision :: denom1, denom2, term1, term2
    integer :: n_knots
    
    N = 0d0
    n_knots = size(knots)
    
    ! Check if index is valid
    if (i < 1 .or. i + k + 1 > n_knots) then
      N = 0d0
      return
    end if
    
    ! Base case: k = 0
    if (k == 0) then
      ! Semi-open interval [knots(i), knots(i+1)), except closed at the end
      ! Python: if u == knots[-1] and i == n-1: return 1.0
      ! Closed interval at the end: include u == last knot for last basis
      if (u == knots(n_knots) .and. i == ncp) then
        N = 1d0
      else if (knots(i) <= u .and. u < knots(i+1)) then
        N = 1d0
      else
        N = 0d0
      end if
      return
    end if
    
    ! Recursive case
    denom1 = knots(i+k) - knots(i)
    term1 = 0d0
    if (abs(denom1) > 1d-14) then
      term1 = ((u - knots(i)) / denom1) * basis_function(i, k-1, u, knots, ncp)
    end if
    
    denom2 = knots(i+k+1) - knots(i+1)
    term2 = 0d0
    if (abs(denom2) > 1d-14) then
      term2 = ((knots(i+k+1) - u) / denom2) * basis_function(i+1, k-1, u, knots, ncp)
    end if
    
    N = term1 + term2
    
  end function


  recursive function basis_function_deriv(i, k, u, knots, ncp, der) result(N)
    !! Evaluate derivative of B-spline basis function
    !! der: derivative order (1 or 2)
    integer, intent(in) :: i, k, ncp, der
    double precision, intent(in) :: u
    double precision, intent(in) :: knots(:)
    double precision :: N
    
    double precision :: denom1, denom2, term1, term2
    
    N = 0d0
    
    if (der == 0) then
      N = basis_function(i, k, u, knots, ncp)
      return
    end if
    
    if (der == 1) then
      ! First derivative
      denom1 = knots(i+k) - knots(i)
      term1 = 0d0
      if (abs(denom1) > 1d-14 .and. k > 0) then
        term1 = dble(k) * basis_function(i, k-1, u, knots, ncp) / denom1
      end if
      
      denom2 = knots(i+k+1) - knots(i+1)
      term2 = 0d0
      if (abs(denom2) > 1d-14 .and. k > 0) then
        term2 = dble(k) * basis_function(i+1, k-1, u, knots, ncp) / denom2
      end if
      
      N = term1 - term2
      
    else if (der == 2) then
      ! Second derivative
      if (k < 2) then
        N = 0d0
        return
      end if
      
      denom1 = knots(i+k) - knots(i)
      term1 = 0d0
      if (abs(denom1) > 1d-14) then
        term1 = dble(k) * basis_function_deriv(i, k-1, u, knots, ncp, 1) / denom1
      end if
      
      denom2 = knots(i+k+1) - knots(i+1)
      term2 = 0d0
      if (abs(denom2) > 1d-14) then
        term2 = dble(k) * basis_function_deriv(i+1, k-1, u, knots, ncp, 1) / denom2
      end if
      
      N = term1 - term2
    end if
    
  end function


  !-----------------------------------------------------------------------------
  ! Core B-spline evaluation
  !-----------------------------------------------------------------------------

  subroutine bspline_eval(bspline, u, x, y, der)
    !! Evaluate B-spline curve at parameter values u (0..1)
    !! der: optional derivative order (0,1,2)
    !! Knots are computed on-the-fly from control point count
    type(bspline_spec_type), intent(in) :: bspline
    double precision, allocatable, intent(in) :: u(:) 
    double precision, allocatable, intent(out) :: x(:), y(:) 
    integer, intent(in), optional :: der 
    
    double precision, allocatable :: knots(:)
    integer :: ncp, degree
    
    ! Compute knots and degree on-the-fly
    ncp = size(bspline%px)
    degree = BSPLINE_DEGREE
    if (degree >= ncp) degree = max(1, ncp - 1)
    knots = generate_uniform_knots(ncp, degree)
    
    x = bspline_eval_1D_array(bspline%px, u, knots, degree, der)   
    y = bspline_eval_1D_array(bspline%py, u, knots, degree, der)

  end subroutine 


  subroutine bspline_eval_side(bspline, npoint_side, x_side, y_side, curvature_side, use_arc_length)
    !! Evaluate B-spline curve to generate airfoil side coordinates
    !! curvature_side: optional - returns curvature at each point
    !! use_arc_length: optional - if .false., use fast parameter-space distribution (default .true.)
    !! Note: bspline must be initialized (knots allocated) before calling
    use math_util, only : cosine_distribution
    type(bspline_spec_type), intent(in) :: bspline
    integer, intent(in) :: npoint_side
    double precision, allocatable, intent(out) :: x_side(:), y_side(:)
    double precision, allocatable, intent(out), optional :: curvature_side(:)
    logical, intent(in), optional :: use_arc_length

    double precision, allocatable :: u(:)
    logical :: use_arc
    integer :: i

    ! Default to arc-length distribution for quality
    use_arc = .true.
    if (present(use_arc_length)) use_arc = use_arc_length

    if (use_arc) then
      ! Full arc-length based distribution (high quality, slower)
      u = u_distribution_bspline(bspline, npoint_side)
    else
      ! Fast cosine distribution in parameter space (for optimization)
      ! Concentrates points at LE/TE without expensive arc-length computation
    !   u = cosine_distribution(npoint_side, LE_BUNCH, TE_BUNCH)
      u = cosine_distribution(npoint_side, le_bunch=0.5d0, te_bunch=0.4d0)  ! More moderate bunching for bsplines to avoid extreme clustering at LE/TE
    end if
    
    call bspline_eval(bspline, u, x_side, y_side)

    ! Calculate curvature if requested
    if (present(curvature_side)) then
      allocate(curvature_side(npoint_side))
      do i = 1, npoint_side
        curvature_side(i) = bspline_curvature(bspline, u(i))
      end do
    end if

  end subroutine


  function bspline_eval_1D_scalar(px, u, knots, degree, der) result(val)
    !! Evaluate 1D B-spline at scalar u (wrapper for array version)
    !! knots and degree are optional - auto-generated if not provided
    double precision, intent(in) :: px(:)
    double precision, intent(in) :: u
    double precision, intent(in), optional :: knots(:)
    integer, intent(in), optional :: degree
    integer, intent(in), optional :: der

    double precision :: val
    double precision :: uarray(1), result(1) 

    uarray(1) = u
    result = bspline_eval_1D_array(px, uarray, knots, degree, der)
    val = result(1)

  end function 


  function bspline_eval_1D_array(px, u, knots, degree, der) result(bspline)
    !! Core 1D B-spline evaluation using Cox-de Boor recursion
    !! px: control points, u: parameters 0..1, der: derivative order
    !! knots and degree are optional - auto-generated if not provided
    double precision, intent(in) :: px(:), u(:)
    double precision, intent(in), optional :: knots(:)
    integer, intent(in), optional :: degree
    integer, intent(in), optional :: der 

    double precision :: bspline(size(u))
    double precision, allocatable :: knots_local(:)
    integer :: i, j, ncp, derivative, degree_local
    double precision :: N_val
    
    ncp = size(px)
    bspline = 0d0

    ! Auto-generate knots and degree if not provided
    if (present(degree)) then
      degree_local = degree
    else
      degree_local = BSPLINE_DEGREE
      if (degree_local >= ncp) degree_local = max(1, ncp - 1)
    end if

    if (present(knots)) then
      knots_local = knots
    else
      knots_local = generate_uniform_knots(ncp, degree_local)
    end if

    if (.not. present(der)) then 
      derivative = 0 
    else 
      derivative = der 
    end if 

    ! Sum over all control points
    do j = 1, size(u)
      do i = 1, ncp
        if (derivative == 0) then
          N_val = basis_function(i, degree_local, u(j), knots_local, ncp)
        else if (derivative == 1) then
          N_val = basis_function_deriv(i, degree_local, u(j), knots_local, ncp, 1)
        else if (derivative == 2) then
          N_val = basis_function_deriv(i, degree_local, u(j), knots_local, ncp, 2)
        else
          N_val = 0d0
        end if
        
        bspline(j) = bspline(j) + N_val * px(i)
      end do
    end do

  end function 


  function bspline_eval_y_on_x(bspline, x, epsilon) result(y)
    !! Evaluate y value for given x using Newton iteration
    !! epsilon: precision (default 1e-10) 
    !! Knots are computed on-the-fly from control point count

    type(bspline_spec_type), intent(in) :: bspline 
    double precision, intent(in) :: x
    double precision, intent(in), optional :: epsilon
    double precision :: y

    double precision, allocatable :: px(:), py(:), knots(:)
    double precision :: eps, u0, xn, dxn, u 
    integer :: i, ncp, degree
    
    px = bspline%px
    py = bspline%py
    
    ! Compute knots and degree on-the-fly
    ncp = size(px)
    degree = BSPLINE_DEGREE
    if (degree >= ncp) degree = max(1, ncp - 1)
    knots = generate_uniform_knots(ncp, degree)

    ! Handle boundary cases
    if (x == px(1)) then
      y = py(1)
      return
    else if (x == px(size(px))) then 
      y = py(size(py))
      return
    end if 

    if (.not. present(epsilon)) then 
      eps = 10d-10
    else 
      eps = epsilon 
    end if 

    ! Initial guess for Newton iteration 
    if (x < 0.05d0) then                      
      u0 = 0.05d0
    else if (x > 0.95d0) then
      u0 = 0.95d0
    else 
      u0 = x
    end if  

    ! Newton iteration to find u where bspline_x(u) = x
    u = u0
    do i = 1, 50
      u = max(1d-10, min(1d0, u))                 ! Keep within bounds

      xn = bspline_eval_1D_scalar(px, u, knots, degree) - x
      if (abs(xn) < eps) exit
      
      dxn = bspline_eval_1D_scalar(px, u, knots, degree, 1) 

      if (abs(dxn) < 1d-14) then 
        if (abs(xn) > eps) write (*,*) "Error: B-spline Newton iteration - zero derivative"
        exit
      end if  

      u = u - xn / dxn 
    end do 

    if (abs(xn) < eps) then 
      y = bspline_eval_1D_scalar(py, u, knots, degree)
    else
      y = 0d0
    end if  

  end function 


  function bspline_curvature(bspline, u) result(curv)
    !! Evaluate curvature at parameter u (0..1) 
    !! Generates knots once for performance (this function often called in loops)

    type(bspline_spec_type), intent(in) :: bspline
    double precision, intent(in) :: u
    double precision :: curv
    double precision :: dx, dy, ddx, ddy
    double precision, allocatable :: knots(:)
    integer :: ncp, degree
    
    ! Generate knots once, reuse for all 4 evaluations
    ncp = size(bspline%px)
    degree = BSPLINE_DEGREE
    if (degree >= ncp) degree = max(1, ncp - 1)
    knots = generate_uniform_knots(ncp, degree)

    dx = bspline_eval_1D_scalar(bspline%px, u, knots, degree, der=1)
    dy = bspline_eval_1D_scalar(bspline%py, u, knots, degree, der=1)
    ddx = bspline_eval_1D_scalar(bspline%px, u, knots, degree, der=2)
    ddy = bspline_eval_1D_scalar(bspline%py, u, knots, degree, der=2)

    curv = (ddy * dx - ddx * dy) / (dx ** 2 + dy ** 2) ** 1.5

    ! Make curvature positive for both top and bottom sides
    if (sum(bspline%py) > 0d0) curv = -curv  

  end function 


  function bspline_le_curvature(bspline) result(curv)
    !! LE curvature using closed-form when px(1)=px(2)=0, else numerical
    !! Formula: |kappa(0)| = (degree-1)/(2*degree) * |x2| / y1^2

    type(bspline_spec_type), intent(in) :: bspline
    double precision :: curv
    integer :: ncp, degree
    
    ! Determine degree from control point count
    ncp = size(bspline%px)
    degree = BSPLINE_DEGREE
    if (degree >= ncp) degree = max(1, ncp - 1)

    if (bspline%px(1) == 0d0 .and. bspline%px(2) == 0d0) then
      ! Closed-form for B-spline LE curvature
      curv = (dble(degree - 1) / (2d0 * dble(degree))) * bspline%px(3) / (bspline%py(2)**2)
    else
      ! Numerical evaluation for general case
      curv = bspline_curvature(bspline, 0d0)
    end if

  end function


  !-----------------------------------------------------------------------------
  ! File I/O functions
  !-----------------------------------------------------------------------------

  function is_bspline_file(filename)
    !! Check if filename has .bsp extension
    character(*), intent(in) :: filename
    logical :: is_bspline_file 
    character(:), allocatable :: suffix 
    
    suffix = filename_suffix(filename)
    is_bspline_file = suffix == '.bsp' .or. suffix =='.BSP'

  end function  


  subroutine read_bspline_file(filename, side, name, bspline)
    !! Read a B-spline definition from file in JSON-like format
    !
    ! Format:
    ! {
    !     "name": "airfoil_name",
    !     "upper": {
    !         "px": [0.0, 0.0, ...],
    !         "py": [0.0, 0.0, ...],
    !         "knots": [0.0, 0.0, ...],
    !         "degree": 4
    !     },
    !     "lower": { ... }
    ! }

    character(*), intent(in) :: filename
    character(3), intent(in) :: side
    character(:), allocatable, intent(out) :: name
    type(bspline_spec_type), intent(out) :: bspline 

    character(:), allocatable :: json_content, section_name, section_str
    
    ! Read entire file into one string
    json_content = read_file_to_string(filename)
    
    ! Extract top-level name
    name = json_get_string(json_content, 'name')
    
    ! Determine section name
    if (side == 'Top') then
      section_name = 'upper'
    else
      section_name = 'lower'
    end if
    
    ! Extract the section content
    section_str = json_get_section(json_content, section_name)
    
    ! Parse control point arrays into bspline structure
    ! Note: knots and degree in file are ignored - computed on-the-fly from px/py
    bspline%px = json_get_array(section_str, 'px')
    bspline%py = json_get_array(section_str, 'py')
    
    ! Validate we got data
    if (size(bspline%px) == 0) then
      call my_stop('Cannot read B-spline definition file ' // trim(filename) // ' - Syntax error')
    end if
    
    call bspline_round_decimals(bspline)

  end subroutine


  subroutine write_bspline_file (filename, name, top_bspline, bot_bspline)
    !! Write a B-spline definition of an airfoil to file in JSON-like format
    !! Note: bsplines must be initialized (knots allocated) before calling

    ! {
    !     "name": "airfoil_name",
    !     "upper": {
    !         "px": [0.0, 0.0, ...],
    !         "py": [0.0, 0.0072329252, ...],
    !         "knots": [0.0, 0.0, 0.0, ...],
    !         "degree": 4
    !     },
    !     "lower": {
    !         "px": [0.0, 0.0, ...],
    !         "py": [0.0, -0.0060258674, ...],
    !         "knots": [0.0, 0.0, 0.0, ...],
    !         "degree": 4
    !     }
    ! }    

    character(*), intent(in) :: filename, name
    type(bspline_spec_type), intent(in) :: top_bspline, bot_bspline

    integer :: iunit, iside, ncp, degree
    type(bspline_spec_type) :: bspline
    double precision, allocatable :: knots(:)
    character(5) :: side_label

    iunit = 13
    open(unit=iunit, file=filename, status='replace')

    ! Write JSON-like format
    write(iunit, '(A)') '{'
    call json_write_string(iunit, 'name', name, indent=4)
    
    ! Loop over top and bottom sides
    do iside = 1, 2
      if (iside == 1) then
        bspline = top_bspline
        side_label = 'upper'
      else
        bspline = bot_bspline
        side_label = 'lower'
      end if
      
      ! Compute knots and degree for this bspline
      ncp = size(bspline%px)
      degree = BSPLINE_DEGREE
      if (degree >= ncp) degree = max(1, ncp - 1)
      knots = generate_uniform_knots(ncp, degree)
      
      write(iunit, '(A)', advance='no') '    "' // trim(side_label) // '": {'
      write(iunit, *)
      
      call json_write_array(iunit, 'px', bspline%px, indent=8)
      call json_write_array(iunit, 'py', bspline%py, indent=8)
      call json_write_array(iunit, 'knots', knots, indent=8)
      call json_write_integer(iunit, 'degree', degree, indent=8, last=.true.)
      
      if (iside == 1) then
        write(iunit, '(A)') '    },'
      else
        write(iunit, '(A)') '    }'
      end if
    end do
    
    write(iunit, '(A)') '}'

    close(iunit)

  end subroutine


  !-----------------------------------------------------------------------------
  ! Design variables and B-spline mapping
  !-----------------------------------------------------------------------------

  function ncp_to_ndv(ncp, le_c2_coupled)
    !! Convert number of control points to design variables
    integer, intent(in) :: ncp
    logical, intent(in), optional :: le_c2_coupled
    integer :: ncp_to_ndv

    if (present(le_c2_coupled) .and. le_c2_coupled) then
      ncp_to_ndv = (ncp - 3) * 2                    ! C2: py(2) is derived
    else
      ncp_to_ndv = (ncp - 3) * 2 + 1                ! Normal: all coordinates as DVs
    end if

  end function


  subroutine map_dv_to_bspline(is_bot, dv_in, te_gap, bspline, le_curv)
    !! Map design variables to B-spline control points
    !! le_curv present: C2-coupled mode, py(2) derived from curvature

    logical, intent(in)             :: is_bot
    double precision, intent(in)    :: dv_in(:)
    double precision, intent(in)    :: te_gap
    type(bspline_spec_type), intent(out) :: bspline
    double precision, intent(in), optional :: le_curv

    type(bound_type), allocatable :: bounds_x(:), bounds_y(:)
    double precision :: delta
    double precision, allocatable :: dv(:)
    integer :: ndv, ncp, ip, idv
    logical :: c2_coupled

    dv = dv_in
    ndv = size(dv)
    c2_coupled = present(le_curv)

    if (c2_coupled) then
      ncp = ndv / 2 + 3
      if (ndv < 2) call my_stop('B-spline C2: ndv < 2')
    else
      ncp = (ndv - 1) / 2 + 3
      if (ndv < 3) call my_stop('B-spline: ndv < 3')
    end if

    ! Clamp dv to [0,1]
    do idv = 1, ndv
      dv(idv) = max(0d0, min(1d0, dv(idv)))
    end do
    
    call bspline_cp_bounds(is_bot, ncp, te_gap, bounds_x, bounds_y)

    bspline%px = bounds_x%min
    bspline%py = bounds_y%min

    idv = 1
    do ip = 2, ncp-1
      if (ip == 2) then
        if (.not. c2_coupled) then
          delta = abs(bounds_y(2)%max - bounds_y(2)%min)
          if (is_bot) then
            bspline%py(2) = bounds_y(2)%max - dv(idv) * delta
          else
            bspline%py(2) = bounds_y(2)%min + dv(idv) * delta
          end if
          idv = idv + 1
        end if
      else
        ! ip >= 3: always map px and py from dv
        delta = abs(bounds_x(ip)%max - bounds_x(ip)%min)
        bspline%px(ip) = bounds_x(ip)%min + dv(idv) * delta
        idv = idv + 1

        delta = abs(bounds_y(ip)%max - bounds_y(ip)%min)
        if (is_bot) then
          bspline%py(ip) = bounds_y(ip)%max - dv(idv) * delta
        else
          bspline%py(ip) = bounds_y(ip)%min + dv(idv) * delta
        end if
        idv = idv + 1
      end if
    end do

    if (c2_coupled) then
      ! Derive py(2) from LE curvature: |py(2)| = sqrt((degree-1)*px(3) / (2*degree*le_curv))
      if (bspline%px(3) > 0d0 .and. le_curv > 0.1d0) then
        bspline%py(2) = sqrt((dble(BSPLINE_DEGREE - 1) * bspline%px(3)) / &
                             (2d0 * dble(BSPLINE_DEGREE) * le_curv))
        if (is_bot) bspline%py(2) = -bspline%py(2)
        bspline%py(2) = clip(bspline%py(2), bounds_y(2)%min, bounds_y(2)%max)
      else
        bspline%py(2) = bounds_y(2)%min
      end if
    end if
    
    call bspline_round_decimals(bspline)

  end subroutine


  subroutine bspline_round_decimals(bspline)
    !! Round B-spline coordinates to file precision (10 decimals) 
    type(bspline_spec_type), intent(inout) :: bspline

    integer :: ip 
    character(30) :: val_buffer

    do ip = 2, size(bspline%px) - 1
      write(val_buffer, '(2F14.10)') bspline%px(ip), bspline%py(ip)
      read(val_buffer, *) bspline%px(ip), bspline%py(ip)
    end do 

  end subroutine 


  function bspline_get_dv0(is_bot, bspline, c2_coupled) result(dv) 
    !! Extract initial design variables from B-spline control points
    !! C2 mode: py(2) is not a DV

    logical, intent(in)                 :: is_bot
    type(bspline_spec_type), intent(in) :: bspline
    logical, intent(in)                 :: c2_coupled

    double precision, allocatable :: dv(:)
    type(bound_type), allocatable :: bounds_x(:), bounds_y(:)
    double precision :: min_val, max_val 
    integer :: ndv, ncp, icp, idv

    ncp = size(bspline%px)

    ndv = ncp_to_ndv(ncp, c2_coupled)
    if (c2_coupled) then
      if (ndv < 2) call my_stop('B-spline C2: ndv < 2')
    else
      if (ndv < 3) call my_stop('B-spline: ndv < 3')
    end if

    allocate(dv(ndv))
    dv = 0d0

    call bspline_cp_bounds(is_bot, ncp, 0d0, bounds_x, bounds_y)

    ! Check bounds
    do icp = 1, ncp - 1   
      if (bspline%px(icp) < bounds_x(icp)%min .or. bspline%px(icp) > bounds_x(icp)%max) then 
        call print_error("B-spline control point "//stri(icp)//" outside x-bounds")
      end if 
      if (bspline%py(icp) < bounds_y(icp)%min .or. bspline%py(icp) > bounds_y(icp)%max) then 
        call print_error("B-spline control point "//stri(icp)//" outside y-bounds")
      end if 
    end do

    idv = 1
    do icp = 2, ncp-1
      if (icp == 2) then
        if (.not. c2_coupled) then
          min_val = bounds_y(2)%min
          max_val = bounds_y(2)%max
          if (is_bot) then
            dv(idv) = (max_val - bspline%py(2)) / abs(max_val - min_val)
          else
            dv(idv) = (bspline%py(2) - min_val) / abs(max_val - min_val)
          end if
          idv = idv + 1
        end if
      else
        min_val = bounds_x(icp)%min
        max_val = bounds_x(icp)%max
        dv(idv) = (bspline%px(icp) - min_val) / abs(max_val - min_val)
        idv = idv + 1

        min_val = bounds_y(icp)%min
        max_val = bounds_y(icp)%max
        if (is_bot) then
          dv(idv) = (max_val - bspline%py(icp)) / abs(max_val - min_val)
        else
          dv(idv) = (bspline%py(icp) - min_val) / abs(max_val - min_val)
        end if
        idv = idv + 1
      end if
    end do

  end function


  function bspline_get_dv_initial_perturb(initial, bspline, c2_coupled) result(dv_perturb) 
    !! Get initial perturbation for design variables
    !! C2 mode: perturb starts at px(3)
    double precision, intent(in) :: initial 
    type(bspline_spec_type), intent(in) :: bspline
    logical, intent(in) :: c2_coupled
    double precision, allocatable :: dv_perturb(:)

    integer :: ndv, ncp, icp, idv

    ncp = size(bspline%px)
    ndv = ncp_to_ndv(ncp, c2_coupled)

    allocate(dv_perturb(ndv))
    dv_perturb = 0d0

    if (c2_coupled) then
      ! C2: no perturb for py(2) — start from px(3), py(3), ...
      idv = 0
      do icp = 3, ncp-1
        idv = idv + 1
        dv_perturb(idv) = min(0.5d0, initial * 0.7d0)   ! x may move more
        idv = idv + 1
        dv_perturb(idv) = min(0.1d0, initial * 0.1d0)   ! y constrained
      end do
    else
      ! Normal mode: tangent - only y - not too volatile 
      dv_perturb(1) = min(0.05d0, initial * 0.1d0)

      ! Control points 3..n-1 take x + y 
      idv = 1
      do icp = 3, ncp-1   
        idv = idv + 1 
        dv_perturb(idv) = min(0.5d0, initial * 1.0d0)   ! x may move more 
        idv = idv + 1 
        dv_perturb(idv) = min(0.1d0, initial * 0.1d0)   ! y constrained
      end do  
    end if

  end function


  subroutine bspline_cp_bounds(is_bot, ncp, te_gap, bounds_x, bounds_y)
    !! Get bounds for control points (for DV mapping) 

    logical, intent(in)             :: is_bot
    integer, intent(in)             :: ncp
    double precision, intent(in)    :: te_gap 

    type(bound_type), allocatable, intent(out) :: bounds_x(:), bounds_y(:)
    integer :: ndv, ip
    double precision :: min_val, max_val 

    ndv = ncp_to_ndv(ncp)

    allocate(bounds_x(ncp))
    allocate(bounds_y(ncp))

    bounds_x%min = 0d0 
    bounds_x%max = 1d0 
    bounds_y%min = 0d0 
    bounds_y%max = 1d0 
    
    ! Fixed B-spline control points at LE and TE 

    bounds_x(1)%max = 0d0
    bounds_y(1)%max = 0d0 

    bounds_x(ncp)%min = 1d0
    bounds_y(ncp)%min = abs(te_gap) 
    bounds_y(ncp)%max = abs(te_gap)

    bounds_x(2)%max = 0d0
    bounds_y(2)%min = 0.001d0 
    bounds_y(2)%max = 0.2d0 

    do ip = 3, ncp-1                                 
      bounds_x(ip)%min = 0.005d0                         ! x not too close to LE
      bounds_x(ip)%max = 0.95d0                          ! x not too close to TE
      bounds_y(ip)%min = -0.2d0                          ! y rough bounds
      bounds_y(ip)%max = 0.9d0
    end do 

    ! Mirror y-bounds for bottom side 

    if (is_bot) then 
      do ip = 2, ncp
        min_val = bounds_y(ip)%min            
        max_val = bounds_y(ip)%max  
        bounds_y(ip)%min = -max_val
        bounds_y(ip)%max = -min_val   
      end do 
    end if 

  end subroutine


  function get_initial_bspline(x, y, le_curv, y_te, ncp) result(bspline)
    !! Create initial B-spline from airfoil side coordinates  

    double precision, intent(in) :: x(:), y(:)
    double precision, intent(in) :: le_curv, y_te
    integer, intent(in)          :: ncp

    type(bspline_spec_type) :: bspline
    integer                 :: i, ib, icp 
    double precision        :: xi, dx, y_fac

    ! sanity ncp >= 4
    if (ncp < 4) then
      call my_stop('B-spline: ncp must be >= 4')
    end if

    allocate(bspline%px(ncp))
    allocate(bspline%py(ncp))

    ! Initialize with simple distribution
    bspline%px(1) = 0d0
    bspline%py(1) = 0d0
    bspline%px(ncp) = 1d0
    bspline%py(ncp) = y_te

    ! Distribute control points based on x-coordinates
    do icp = 2, ncp - 1
      xi = dble(icp - 1) / dble(ncp - 1)
      bspline%px(icp) = xi ** 1.5d0  ! Clustering toward LE
      
      ! Find y value at this x location
      ib = 1
      do i = 1, size(x) - 1
        if (x(i) <= bspline%px(icp) .and. x(i+1) >= bspline%px(icp)) then
          ib = i
          exit
        end if
      end do
      
      ! Linear interpolation
      if (ib < size(x)) then
        dx = x(ib+1) - x(ib)
        if (abs(dx) > 1d-10) then
          y_fac = (bspline%px(icp) - x(ib)) / dx
          bspline%py(icp) = y(ib) + y_fac * (y(ib+1) - y(ib))
        else
          bspline%py(icp) = y(ib)
        end if
      else
        bspline%py(icp) = y(ib)
      end if
    end do

    ! Special handling for second control point (LE tangent)

    bspline%px(2) = 0d0
    if (le_curv > 0d0) then
      ! used closed form curvature model for B-spline to derive py(2) from le_curv
        bspline%py(2) = sqrt((dble(BSPLINE_DEGREE - 1) * bspline%px(3)) / &
                             (2d0 * dble(BSPLINE_DEGREE) * le_curv))
        if (y(2) < 0d0) bspline%py(2) = -bspline%py(2)  ! Bottom side
    else
      ! No curvature info - just set py(2) to a fraction of py(3)
      bspline%py(2) = bspline%py(3) * 0.4d0
    end if

    call bspline_round_decimals(bspline)

  end function


  function bspline_violates_constraints(bspline) result(violates) 
    !! Check if B-spline violates basic geometric constraints
    type(bspline_spec_type), intent(in) :: bspline
    logical :: violates
    
    integer :: i, ncp
    
    violates = .false.
    ncp = size(bspline%px)
    
    ! Check for monotonic x (except first two points)
    do i = 3, ncp - 1
      if (bspline%px(i) <= bspline%px(i-1)) then
        violates = .true.
        return
      end if
    end do
    
    ! Check TE point
    if (bspline%px(ncp) /= 1d0) then
      violates = .true.
      return
    end if
    
  end function


  !-----------------------------------------------------------------------------
  ! Utility functions
  !-----------------------------------------------------------------------------

  function u_distribution_bspline(bspline, nPoints) result(u)
    !! Parameter distribution with arc-length based cosine distribution
    !! for airfoil paneling. Uses cosine distribution in arc-length space,
    !! then maps to curve parameter u via arc-length inversion.
    !!
    !! Returns numpy array of u having arc-length based cosine distribution
    !! for one curve side running from 0..1 with nPoints points
    
    type(bspline_spec_type), intent(in) :: bspline
    integer, intent(in) :: nPoints
    double precision, allocatable :: u(:), u_cos(:)

    ! Get cosine distribution in arc-length space
    u_cos = cosine_distribution(nPoints, LE_BUNCH, TE_BUNCH)
    
    ! Map arc-length fractions to curve parameter u
    u = u_of_arc_fractions_bspline(bspline, u_cos)
    
  end function


  function u_of_arc_fractions_bspline(bspline, arc_fractions) result(u)
    !! Maps target arc-length fractions [0,1] back to curve parameter u [0,1]
    !!
    !! Samples the curve densely with uniform u, computes cumulative arc length,
    !! then uses linear interpolation to invert the arc-length → u mapping.
    !! This allows any desired distribution in arc-length space to be expressed
    !! as the corresponding curve parameter values.
    
    type(bspline_spec_type), intent(in) :: bspline
    double precision, intent(in) :: arc_fractions(:)
    double precision, allocatable :: u(:)
    
    double precision, allocatable :: u_dense(:), x_d(:), y_d(:), dx(:), dy(:), ds(:), s(:)
    integer :: n_dense, n_fractions
    
    n_dense = 1000
    n_fractions = size(arc_fractions)
    
    ! Sample curve densely with uniform parameter
    u_dense = linspace(0.0d0, 1.0d0, n_dense)
    call bspline_eval(bspline, u_dense, x_d, y_d)
    
    ! Compute differential arc lengths using numpy-style utilities
    dx = diff_1D(x_d)
    dy = diff_1D(y_d)
    ds = sqrt(dx**2 + dy**2)
    
    ! Build cumulative arc length: prepend 0, then cumsum of ds
    allocate(s(n_dense))
    s(1) = 0.0d0
    s(2:n_dense) = cumsum(ds)
    
    ! Normalize to [0,1]
    if (s(n_dense) > 0.0d0) then
      s = s / s(n_dense)
    end if
    
    ! Interpolate arc_fractions -> u
    allocate(u(n_fractions))
    call interp_vector(s, u_dense, arc_fractions, u)
    
    ! Ensure exact endpoints
    u(1) = 0.0d0
    u(n_fractions) = 1.0d0
    
  end function u_of_arc_fractions_bspline


  subroutine print_bspline_spec(bspline)
    !! Print B-spline specification for debugging
    !! Note: bspline must be initialized (knots allocated) before calling
    type(bspline_spec_type), intent(in) :: bspline
    
    integer :: i, ncp, degree
    character(:), allocatable :: side_str
    
    if (bspline%py(2) < 0d0) then
      side_str = "Bot"
    else
      side_str = "Top"
    end if
    
    ncp = size(bspline%px)
    degree = BSPLINE_DEGREE
    if (degree >= ncp) degree = max(1, ncp - 1)
    call print_text("B-spline "//side_str//" - ncp: "//stri(ncp)// &
                    " degree: "//stri(degree))
    
    do i = 1, min(ncp, 10)
      write(*,'(I3,A,F10.6,A,F10.6)')  i, ": ", bspline%px(i), ", ", bspline%py(i)
    end do
    
    if (ncp > 10) then
      write(*,'(A)') "  ..."
    end if
    
  end subroutine

end module shape_bspline

! MIT License
! Copyright (c) 2026 jxjo

module shape_bezier
   
  !-------------------------------------------------------------------------
  ! Bezier based airfoil generation, modification, ...
  !-------------------------------------------------------------------------
 
  use os_util 
  use print_util
  use math_util,  only : diff_1D, cumsum, clip, find_closest_index, linspace, interp_vector, cosine_distribution
  use math_util,  only : curve_spec_type, round, binary_search_u
  use string_util, only : stri
  
  implicit none
  private
  
  ! --- bezier types --------------------------------------------------------- 

  type shape_bezier_type
    integer                       :: ncp_top, ncp_bot
    double precision              :: initial_perturb
  end type

  type, extends(curve_spec_type) :: bezier_spec_type
    !! Bezier curve specification - extends curve_spec_type
    !! Inherits px(:), py(:) from parent
  end type bezier_spec_type

  public :: bezier_spec_type, shape_bezier_type

  double precision, parameter :: LE_BUNCH_DEFAULT = 0.84d0     ! Paneling bunching paramter at LE
  double precision, parameter :: TE_BUNCH_DEFAULT = 0.70d0     ! Paneling bunching parameter at TE

  ! --- bezier functions ----------------------------------------------------- 
 

  public :: bezier_eval 
  public :: bezier_eval_side
  public :: bezier_round_decimals
  interface bezier_eval_1D
     module procedure bezier_eval_1D_array
     module procedure bezier_eval_1D_scalar
  end interface 
  public :: bezier_eval_1D
  interface bezier_curvature
     module procedure bezier_curvature_array
     module procedure bezier_curvature_scalar
  end interface
  public :: bezier_curvature
  public :: bezier_le_curvature
  public :: bezier_te_angle
  public :: bezier_eval_y_on_x
  public :: bezier_violates_constraints
 
  ! Bezier from/to design variables   

  public :: map_dv_to_bezier
  public :: bezier_get_dv0, get_initial_bezier
  public :: bezier_get_dv_inital_perturb
  public :: ncp_to_ndv
  public :: shape_bezier_ndv

  public :: u_distribution_bezier
  public :: u_of_arc_fractions_bezier

  ! file function 

  public :: is_bezier_file, read_bezier_file, write_bezier_file

  public :: print_bezier_spec


  ! --- private --------------------------------------------------------------
  
  type bound_type 
    double precision              :: min, max
  end type bound_type

contains

  subroutine bezier_eval (bezier, u, x, y, der)
    !! Evaluate bezier curve at parameter values u (0..1)
    !! der: optional derivative order (0,1,2)
    type (bezier_spec_type), intent(in)      :: bezier
    double precision, allocatable,intent(in)  :: u(:) 
    double precision, allocatable,intent(out) :: x(:), y(:) 
    integer, intent(in), optional :: der 

    x =  bezier_eval_1D_array (bezier%px, u, der)   
    y =  bezier_eval_1D_array (bezier%py, u, der )

  end subroutine 



  subroutine bezier_eval_side (bezier, npoint_side, x_side, y_side, curvature_side, use_arc_length)
    !! Evaluate bezier curve to generate airfoil side coordinates
    !! curvature_side: optional - returns curvature at each point
    !! use_arc_length: optional - if .false., use fast parameter-space distribution (default .true.)
    type (bezier_spec_type), intent(in)      :: bezier
    integer, intent(in)                       :: npoint_side
    double precision, allocatable,intent(out) :: x_side(:), y_side(:)
    double precision, allocatable,intent(out), optional :: curvature_side(:)
    logical, intent(in), optional             :: use_arc_length

    double precision, allocatable :: u(:)
    logical :: use_arc

    ! Default to arc-length distribution for quality
    use_arc = .true.
    if (present(use_arc_length)) use_arc = use_arc_length

    if (use_arc) then
      ! Full arc-length based distribution (high quality, slower)
      u = u_distribution_bezier (bezier, npoint_side)
    else
      ! Fast cosine distribution in parameter space (for optimization)
      ! Concentrates points at LE/TE without expensive arc-length computation
      u = cosine_distribution(npoint_side, LE_BUNCH_DEFAULT, TE_BUNCH_DEFAULT)
    end if
    
    call bezier_eval (bezier, u, x_side, y_side)

    ! Calculate curvature if requested
    if (present(curvature_side)) then
      curvature_side = bezier_curvature(bezier, u)

      ! sanity curvature must be positive at le 
      if (curvature_side(1) < 0d0) then
        curvature_side = -curvature_side
      end if

    end if

  end subroutine



  function bezier_eval_1D_scalar (px, u, der ) 
    !! Evaluate 1D bezier at scalar u (wrapper for array version) 
    double precision, intent(in)    :: px(:)
    double precision, intent(in)    :: u 
    integer, intent(in), optional   :: der

    double precision                :: bezier_eval_1D_scalar
    double precision                :: uarray(1), bezier(1) 

    uarray = u
    bezier = bezier_eval_1D_array (px, uarray, der )

    bezier_eval_1D_scalar = bezier(1)

  end function 



  function bezier_eval_1D_array (px, u, der ) 
    !! Core 1D bezier evaluation - px: control points, u: parameters 0..1, der: derivative order 

    double precision, intent(in)  :: px(:), u (:)
    integer, intent(in), optional :: der 

    double precision              :: bezier(size(u)), bezier_eval_1D_array (size(u))
    double precision, allocatable :: weights(:)
    integer     :: i, n, derivative

    bezier = 0d0

    if (.not. present(der)) then 
      derivative = 0 
    else 
      derivative = der 
    end if 

    n = size(px) - 1                               ! degree of Bezier curve
    weights = px
    bezier  = 0d0

    ! adjust weights for derivatives
    if (derivative > 0) then 
      weights = diff_1D (weights) * n
      n = n - 1
    end if 
    if (derivative > 1) then 
      weights = diff_1D (weights) * n 
      n = n - 1
    end if 

    ! evaluate bernstein polynomials 
          
    do i = 1, size(weights)
      bezier = bezier + basisFunction (n, i, u) * weights(i)
    end do 

    bezier_eval_1D_array = bezier 

  end function 

  function bezier_eval_y_on_x (bezier, x, epsilon) 
    !! Evaluate y value for given x using Newton iteration
    !! epsilon: precision (default 1e-10) 

    use string_util, only : strf

    type (bezier_spec_type), intent(in) :: bezier 
    double precision, intent(in)            :: x
    double precision, intent(in), optional  :: epsilon
    double precision    :: bezier_eval_y_on_x

    double precision, allocatable    :: px(:), py(:)
    double precision                 :: eps, u0, xn, dxn, u 
    integer   :: i 

    px = bezier%px
    py = bezier%py 

    ! handle boundary cases
    if (x == px(1)) then
      bezier_eval_y_on_x = py(1)
      return
    else if (x == px(size(px))) then 
      bezier_eval_y_on_x = py(size(py))
      return
    end if 

    if (.not. present(epsilon)) then 
      eps = 10d-10
    else 
      eps = epsilon 
    end if 

    ! Initial guess via binary search (5 bisections → bracket ≤ 0.03)
    u0 = binary_search_u(eval_bezier_x, x, 1d-4, 1d0 - 1d-4)

    ! Newton iteration to find u where bezier_x(u) = x
    u = u0
    do i = 1, 50
      u = max(1d-10, min(1d0, u))                 ! keep within bounds

      xn  = bezier_eval_1D (px, u) - x
      if ((abs(xn) < eps)) exit
      dxn = bezier_eval_1D (px, u, 1) 

      if (dxn == 0d0) then 
        if (xn /= 0d0) write (*,*) "Error: Bezier Newton iteration - zero derivative"
        exit
      end if  

      u = u - xn / dxn 
    end do 

    if ((abs(xn) < eps)) then 
      bezier_eval_y_on_x = bezier_eval_1D (py, u)
    else 
      call print_error ("Bezier Newton iteration did not converge for x = "//strf('f6.4',x))
      bezier_eval_y_on_x = 0d0
    end if  

    return 

  contains

    function eval_bezier_x(u_in) result(xval)
      double precision, intent(in) :: u_in
      double precision             :: xval
      xval = bezier_eval_1D_scalar(px, u_in)
    end function eval_bezier_x

  end function 



  function bezier_curvature_scalar (bezier, u) result (curv)
    !! Evaluate curvature at scalar parameter u (0..1) 

    type (bezier_spec_type), intent(in) :: bezier
    double precision, intent(in)        :: u
    double precision                    :: curv
    double precision                    :: u_array(1), curv_array(1)

    u_array = u
    curv_array = bezier_curvature_array(bezier, u_array)
    curv = curv_array(1)

  end function



  function bezier_curvature_array (bezier, u) result (curv)
    !! Evaluate curvature at parameter array u(:) (0..1) 

    type (bezier_spec_type), intent(in)      :: bezier
    double precision, intent(in)             :: u(:)
    double precision, allocatable            :: curv(:)
    double precision, allocatable            :: dx(:), dy(:), ddx(:), ddy(:)

    dx  = bezier_eval_1D_array (bezier%px, u, der=1)
    dy  = bezier_eval_1D_array (bezier%py, u, der=1)
    ddx = bezier_eval_1D_array (bezier%px, u, der=2)
    ddy = bezier_eval_1D_array (bezier%py, u, der=2)

    curv = (ddy * dx - ddx * dy) / (dx ** 2 + dy ** 2) ** 1.5

    ! make curvature positive for both top and bottom sides
    if (sum(bezier%py) > 0d0) curv = -curv
    
    ! round to 10 decimals to avoid numerical noise
    curv = round(curv, 10)

  end function 


  function bezier_le_curvature (bezier) result (curv)
    !! LE curvature using closed-form when px(1)=px(2)=0, else numerical

    type (bezier_spec_type), intent(in) :: bezier
    double precision                    :: curv
    integer                             :: ncp

    ncp = size(bezier%px)
    if (bezier%px(1) == 0d0 .and. bezier%px(2) == 0d0) then
      curv = dble(ncp-2) * bezier%px(3) / (dble(ncp-1) * bezier%py(2)**2)
    else
      curv = bezier_curvature (bezier, 0d0)
    end if

  end function


  function bezier_te_angle (bezier) result (angle_deg)
    !! TE tangent angle in degrees for Bezier side
    !! positive means tangent points downward toward TE

    type (bezier_spec_type), intent(in) :: bezier
    double precision                    :: angle_deg
    integer                             :: ncp
    double precision                    :: dxdu, dydu

    ncp = size(bezier%px)
    if (ncp < 2) then
      angle_deg = 0d0
      return
    end if

    dxdu = dble(ncp - 1) * (bezier%px(ncp) - bezier%px(ncp-1))
    dydu = dble(ncp - 1) * (bezier%py(ncp) - bezier%py(ncp-1))

    angle_deg = -atan2(dydu, dxdu) * 180d0 / acos(-1d0)

  end function bezier_te_angle


  ! --- file functions -------------------------------------------------------


  function is_bezier_file (filename)
    !! Check if filename has .bez extension
    character(*),  intent(in) :: filename
    logical                   :: is_bezier_file 
    character(:), allocatable :: suffix 
    
    suffix = filename_extension (filename)
    is_bezier_file = suffix == '.bez' .or. suffix =='.BEZ'

  end function  


  
  subroutine read_bezier_file (filename, side, name, bezier)
    !! read a bezier definition from file - returns control points for top and bot 
    !
    ! # 'airfoil name'
    ! # Top Start
    ! # 0.0000000000000000 0.0000000000000000
    ! # ...
    ! # 1.0000000000000000 0.0000000000000000
    ! # Top End
    ! # Bottom Start
    ! # ...
    ! # Bottom End

    character(*),  intent(in)                   :: filename
    character (3), intent(in)                   :: side
    character (:), allocatable, intent(out)     :: name
    type (bezier_spec_type), intent(out)        :: bezier 

    double precision, dimension (100) :: px_tmp, py_tmp 
    integer :: iunit, ioerr, np, i
    logical :: do_read 
    character (255) :: in_buffer 
    character(:), allocatable :: in_line
  
    iunit = 12
    open(unit=iunit, file=filename, status='old', position='rewind', iostat=ioerr)
    if (ioerr /= 0) then
      call my_stop ('Cannot find bezier definition file '//trim(filename))
    end if

    do_read = .false. 
    np = 0
    name = ''

    do i = 1, size(px_tmp) 
      read(iunit, '(A)',iostat=ioerr) in_buffer
      in_line = trim(in_buffer) 
      if (i == 1) then 
        name = in_line
      else if (in_line == 'Top Start'    .and. side == 'Top') then
        do_read = .true. 
      else if (in_line == 'Bottom Start' .and. side == 'Bot') then
        do_read = .true.
      else if (do_read .and. (in_line == 'Top End' .or. in_line == 'Bottom End')) then 
        exit  
      else if  (in_line == 'Bottom Start' .or. in_line == 'Bottom Start') then
        continue
      else if (do_read) then 
        np = np + 1
        read (in_line,*) px_tmp(np), py_tmp(np) 
      end if
    end do
  
    if (np> 0) then 
      bezier%px = px_tmp(1:np)
      bezier%py = py_tmp(1:np)

      call bezier_round_decimals (bezier) 
      
    else
      call my_stop ('Cannot read bezier definition file ' // trim(filename) // ' - Syntax error')
    end if 
  
    close(iunit)

  end subroutine


  subroutine write_bezier_file (pathfilename, name, top_bezier, bot_bezier)
    !! write a bezier definition of an airfoil to file
    !
    ! # 'airfoil name'
    ! # Top Start
    ! # 0.0000000000000000 0.0000000000000000
    ! # ...
    ! # 1.0000000000000000 0.0000000000000000
    ! # Top End
    ! # Bottom Start
    ! # ...trim(outname)//'.bez'
    ! # Bottom End

    character(*),  intent(in)              :: pathfilename, name
    type(bezier_spec_type), intent(in)     :: top_bezier, bot_bezier

    integer :: iunit, i, ncp_top, ncp_bot

    ncp_top = size(top_bezier%px)
    ncp_bot = size(bot_bezier%px)

    iunit = 13
    open  (unit=iunit, file=pathfilename, status='replace')

    write (iunit, '(A)') trim(name) 

    write (iunit, '(A)') "Top Start"
    do i = 1, ncp_top
      write (iunit, '(2F14.10)') top_bezier%px(i), top_bezier%py(i)
    end do 
    write (iunit, '(A)') "Top End"

    write (iunit, '(A)') "Bottom Start"
    do i = 1, ncp_bot
      write (iunit, '(2F14.10)') bot_bezier%px(i), bot_bezier%py(i)
    end do 
    write (iunit, '(A)') "Bottom End"

    close (iunit)

  end subroutine


  ! --- design variables and bezier ------------------------------------------


  function ncp_to_ndv (ncp, le_c2_coupled)
    !! Convert number of control points to design variables
    integer, intent(in) :: ncp
    logical, intent(in), optional :: le_c2_coupled
    integer :: ncp_to_ndv
    logical :: c2_mode

    c2_mode = .false.
    if (present(le_c2_coupled)) c2_mode = le_c2_coupled

    if (c2_mode) then
      ncp_to_ndv = (ncp - 3) * 2                    ! C2: py(2) is derived
    else
      ncp_to_ndv = (ncp - 3) * 2 + 1                ! normal: all coordinates as DVs
    end if

  end function


  function shape_bezier_ndv(shape_bezier)
    !! Get number of design variables from shape_bezier_type
    type(shape_bezier_type), intent(in) :: shape_bezier
    integer :: shape_bezier_ndv

    shape_bezier_ndv = ncp_to_ndv(shape_bezier%ncp_top) + &
                       ncp_to_ndv(shape_bezier%ncp_bot, le_c2_coupled=.true.)

  end function



  function map_dv_to_bezier (is_bot, dv_in, te_gap, le_curv) result(bezier)

    !! Map design variables to bezier control points
    !! le_curv present: C2-coupled mode, py(2) derived from curvature

    logical, intent(in)                        :: is_bot
    double precision, intent(in)               :: dv_in(:)
    double precision, intent(in)               :: te_gap
    double precision, intent(in), optional     :: le_curv   ! if present: C2-coupled mode

    type (bezier_spec_type)         :: bezier

    type(bound_type), allocatable   :: bounds_x(:), bounds_y(:)
    double precision                :: delta
    double precision, allocatable   :: dv(:)
    integer                         :: ndv, ncp, ip, idv
    logical                         :: c2_coupled

    dv = dv_in
    ndv = size(dv)
    c2_coupled = present(le_curv)

    if (c2_coupled) then
      ncp = ndv / 2 + 3
      if (ndv < 2) call my_stop('Bezier C2: ndv < 2')
    else
      ncp = (ndv - 1) / 2 + 3
      if (ndv < 3) call my_stop('Bezier: ndv < 3')
    end if

    ! clamp dv to [0,1]
    do idv = 1, ndv
      dv(idv) = max(0d0, min(1d0, dv(idv)))
    end do
    call bezier_cp_bounds(is_bot, ncp, te_gap, bounds_x, bounds_y)

    bezier%px = bounds_x%min
    bezier%py = bounds_y%min

    idv = 1
    do ip = 2, ncp-1
      if (ip == 2) then
        if (.not. c2_coupled) then
          delta = abs(bounds_y(2)%max - bounds_y(2)%min)
          if (.not.is_bot) then
            bezier%py(2) = bounds_y(2)%min + dv(idv) * delta
          else
            bezier%py(2) = bounds_y(2)%max - dv(idv) * delta
          end if
          idv = idv + 1
        end if
        ! If c2_coupled, skip py(2) here; will compute after loop
      else
        ! ip >= 3: always map px and py from dv
        delta = abs(bounds_x(ip)%max - bounds_x(ip)%min)
        bezier%px(ip) = bounds_x(ip)%min + dv(idv) * delta
        idv = idv + 1

        delta = abs(bounds_y(ip)%max - bounds_y(ip)%min)
        if (.not.is_bot) then
          bezier%py(ip) = bounds_y(ip)%min + dv(idv) * delta
        else
          bezier%py(ip) = bounds_y(ip)%max - dv(idv) * delta
        end if
        idv = idv + 1
      end if
    end do

    if (c2_coupled) then
      ! derive py(2) from LE curvature: |py(2)| = sqrt((ncp-2)*px(3) / ((ncp-1)*le_curv))
      if (bezier%px(3) > 0d0 .and. le_curv > 0.1d0) then
        bezier%py(2) = sqrt((dble(ncp-2) * bezier%px(3)) / (dble(ncp-1) * le_curv))
        if (is_bot) bezier%py(2) = -bezier%py(2)
        bezier%py(2) = clip(bezier%py(2), bounds_y(2)%min, bounds_y(2)%max)
      else
        bezier%py(2) = bounds_y(2)%min
      end if
    end if

  end function



  subroutine bezier_round_decimals (bezier)
    !! Round bezier coordinates to file precision (10 decimals) 
    type (bezier_spec_type), intent(inout) :: bezier

    bezier%px = round(bezier%px, 10)
    bezier%py = round(bezier%py, 10)

  end subroutine 



function bezier_get_dv0 (is_bot, bezier, c2_coupled) result (dv) 
    !! Extract initial design variables from bezier control points
    !! C2 mode: py(2) is not a DV
    logical, intent(in)                         :: is_bot
    type(bezier_spec_type), intent(in)          :: bezier
    logical, intent(in)                         :: c2_coupled
    double precision, allocatable               :: dv(:)

    type(bound_type), allocatable   :: bounds_x(:), bounds_y(:)
    double precision                :: min_val, max_val 
    integer                         :: ndv, ncp, icp, idv

    ncp = size(bezier%px)

    ndv = ncp_to_ndv(ncp, c2_coupled)
    if (c2_coupled) then
      if (ndv < 2) call my_stop ('Bezier C2: ndv < 2')
    else
      if (ndv < 3) call my_stop ('Bezier: ndv < 3')
    end if

    allocate (dv (ndv))
    dv = 0d0

    call bezier_cp_bounds (is_bot, ncp, 0d0, bounds_x, bounds_y)

    ! check bounds
    do icp = 1, ncp - 1   
      if (bezier%px(icp) < bounds_x(icp)%min .or. bezier%px(icp) > bounds_x(icp)%max) then 
        call print_error ("Bezier control point "//stri(icp)//" outside x-bounds")
      end if 
      if (bezier%py(icp) < bounds_y(icp)%min .or. bezier%py(icp) > bounds_y(icp)%max) then 
        call print_error ("Bezier control point "//stri(icp)//" outside y-bounds")
      end if 
    end do

    idv = 1
    do icp = 2, ncp-1
      if (icp == 2) then
        if (.not. c2_coupled) then
          min_val = bounds_y(2)%min                 ! LE tangent - only y
          max_val = bounds_y(2)%max
          if (.not. is_bot) then
            dv(idv) = (bezier%py(2) - min_val) / abs (max_val - min_val)
          else
            dv(idv) = (max_val - bezier%py(2)) / abs (max_val - min_val)
          end if
          idv = idv + 1
        end if
      else
        min_val = bounds_x(icp)%min
        max_val = bounds_x(icp)%max
        dv(idv) = (bezier%px(icp) - min_val) / abs (max_val - min_val)
        idv = idv + 1

        min_val = bounds_y(icp)%min
        max_val = bounds_y(icp)%max
        if (.not. is_bot) then
          dv(idv) = (bezier%py(icp) - min_val) / abs (max_val - min_val)
        else
          dv(idv) = (max_val - bezier%py(icp)) / abs (max_val - min_val)
        end if
        idv = idv + 1
      end if
    end do

  end function



  function bezier_get_dv_inital_perturb (initial, bezier, c2_coupled) result (dv_perturb) 
    !! Get initial perturbation for design variables
    !! C2 mode: perturb starts at px(3)
    double precision, intent(in)                :: initial 
    type(bezier_spec_type), intent(in)          :: bezier
    logical, intent(in)                         :: c2_coupled
    double precision, allocatable               :: dv_perturb(:)

    integer                         :: ndv, ncp, icp, idv

    ncp = size(bezier%px)

    ndv = ncp_to_ndv(ncp, c2_coupled)

    allocate (dv_perturb (ndv))
    dv_perturb = 0d0

    if (c2_coupled) then

      ! C2 bot: no perturb for py(2) — start directly from px(3), py(3), ...
      idv = 0
      do icp = 3, ncp-1
        idv = idv + 1
        dv_perturb(idv) = min (0.5d0, initial * 0.7d0)   ! x value may move around more
        idv = idv + 1
        dv_perturb(idv) = min (0.1d0, initial * 0.1d0)  ! y value may move not too much
      end do

    else

      ! Normal mode: start tangent - only y - not too volatile 
      dv_perturb(1) = min (0.05d0, initial * 0.1d0)

      ! normal control points 3..n-1 take x + y 
      idv = 1
      do icp = 3, ncp-1   
        idv = idv + 1 
        dv_perturb(idv) = min (0.5d0, initial * 1.0d0)   ! x value may move around more 
        idv = idv + 1 
        dv_perturb(idv) = min (0.1d0, initial * 0.1d0)  ! y value may move not too much
      end do  

    end if

  end function


  subroutine bezier_cp_bounds (is_bot, ncp,te_gap, bounds_x, bounds_y)
    !! Get bounds for control points (for DV mapping) 

    logical, intent(in)           :: is_bot
    integer, intent(in)           :: ncp
    double precision, intent(in)  :: te_gap 
    type(bound_type), allocatable, intent(out) :: bounds_x(:), bounds_y(:)
    integer             :: ndv, ip
    double precision    :: min_val, max_val 

    ndv = ncp_to_ndv(ncp)

    allocate (bounds_x(ncp))
    allocate (bounds_y(ncp))

    bounds_x%min  = 0d0 
    bounds_x%max  = 1d0 
    bounds_y%min  = 0d0 
    bounds_y%max  = 1d0 
    
    ! fixed bezier control points at LE and TE 

    bounds_x(1)%max   = 0d0
    bounds_y(1)%max   = 0d0 

    bounds_x(ncp)%min = 1d0
    bounds_y(ncp)%min = abs(te_gap) 
    bounds_y(ncp)%max = abs(te_gap)

    bounds_x(2)%max = 0d0
    bounds_y(2)%min = 0.001d0 
    bounds_y(2)%max = 0.2d0 

    do ip = 3, ncp-1                                 
      bounds_x(ip)%min = 0.001d0                         ! x not too close to LE
      bounds_x(ip)%max = 0.98d0                          ! x not too close to TE
      bounds_y(ip)%min = -0.2d0                          ! y rough bounds - this is the opposite side (patch: prior -0.01)
      bounds_y(ip)%max =  0.9d0                          ! better higher value for smaller y movements
    end do 

    ! mirror y-bounds for bottom side 

    if (is_bot) then 
      do ip = 2, ncp
        min_val = bounds_y(ip)%min            
        max_val = bounds_y(ip)%max  
        bounds_y(ip)%min = -max_val
        bounds_y(ip)%max = -min_val   
      end do 
    end if 

  end subroutine


  function  get_initial_bezier (x, y, le_curv, y_te, ncp) result(bezier)
    !! Create initial bezier from airfoil side coordinates  

    double precision, intent(in)  :: x(:), y(:)
    double precision, intent(in)  :: le_curv, y_te
    integer, intent(in)           :: ncp
    type(bezier_spec_type)        :: bezier

    integer                   :: i, ib, icp, np_between 
    double precision          :: xi, dx, y_fac
    logical                   :: is_bot

    if (ncp < 4)        call my_stop ('Initial Bezier: ncp < 4')
    if (le_curv < 10d0) call my_stop ('Initial Bezier: le_curv < 10')

    is_bot = (y(2) < 0d0)

    allocate (bezier%px (ncp))
    allocate (bezier%py (ncp))
    bezier%px = 0d0
    bezier%py = 0d0

    ! fix control points for te

    bezier%px(ncp) = 1d0                                    ! te 
    bezier%py(ncp) = y_te                                   ! set te gap       
    
    ! build x values and first estimate of y 

    np_between = ncp - 3
    if (np_between ==1) then 
      dx = 0.35d0 
    else 
      dx = 1d0 / (np_between + 1) 
    end if 

    xi = 0d0 
    do ib = 1, np_between
      icp = 2 + ib 
      xi = xi + dx 
      i  = find_closest_index (x, xi)
      bezier%px(icp) = x(i) 
      bezier%py(icp) = y(i) 
    end do 

    ! y of start tangent - derive from LE curvature using closed-form formula

    ! Use closed-form: |py(2)| = sqrt((ncp-2)*px(3) / ((ncp-1)*le_curv))
    bezier%py(2) = sqrt((dble(ncp-2) * bezier%px(3)) / (dble(ncp-1) * le_curv))
    if (is_bot) bezier%py(2) = -bezier%py(2)
    bezier%py(2) = clip(bezier%py(2), -0.2d0, 0.2d0)

    !adjust y values between le and te for best fit 

    do icp = 3, ncp - 1
      if (ncp == 6) then 
        y_fac = 1.2d0
      else if (ncp == 5) then 
        y_fac = 1.3d0       
      else if (ncp == 4) then 
        y_fac = 1.6d0       
      else  
        y_fac = 1.15d0 
      end if       

      if (icp == 3) y_fac = y_fac * 1.2d0

      bezier%py(icp) = bezier%py(icp) * y_fac
    end do

  end function get_initial_bezier


  function bezier_violates_constraints (bezier) result (violates) 
    !! Check if bezier violates bounds or control point ordering 

    type(bezier_spec_type), intent(in)    :: bezier

    logical       :: violates
    integer       :: i

    violates = .false. 

    do i = 1, size(bezier%px) 
      if (bezier%px(i) < 0d0 .or. bezier%px(i) > 1d0 ) then 
        violates = .true.
        return 
      end if 
    end do 

    do i = 3, size(bezier%px)
      if ((bezier%px(i-1) - bezier%px(i)) > -0.05d0 ) then
        violates = .true.
        return
      end if  
    end do

  end function bezier_violates_constraints


  ! --- helper functions -----------------------------------------------------


  function Ni (n, i ) 
    !! Binomial coefficient: n! / (i! * (n-i)!)
    integer, intent(in)     :: n, i
    double precision  :: Ni

    Ni = gamma (real(n+1)) / (gamma (real(i+1)) * gamma(real(n-i+1)))

  end function 

  function basisFunction(n, i1, u) 
    !! Bernstein basis polynomial: Ni(n,i) * u^i * (1-u)^(n-i)
    integer, intent(in)     :: n, i1 
    double precision, dimension(:), intent(in) :: u 
    double precision, dimension(size(u)) :: basisFunction 
    integer           :: i 

    i = i1 - 1
    basisFunction = Ni (n,i) * ( u**i) * (1-u) ** (n-i)

  end function 


  function u_distribution_bezier (bezier, nPoints) result(u)
    !! Parameter distribution with arc-length based cosine distribution
    !! for airfoil paneling. Uses cosine distribution in arc-length space,
    !! then maps to curve parameter u via arc-length inversion.
    !!
    !! Returns numpy array of u having arc-length based cosine distribution
    !! for one curve side running from 0..1 with nPoints points
    
    type(bezier_spec_type), intent(in) :: bezier
    integer, intent(in) :: nPoints 
    double precision, allocatable :: u(:), u_cos(:)

    ! Get cosine distribution in arc-length space
    u_cos = cosine_distribution(nPoints, LE_BUNCH_DEFAULT, TE_BUNCH_DEFAULT)
    
    ! Map arc-length fractions to curve parameter u
    u = u_of_arc_fractions_bezier(bezier, u_cos)

  end function u_distribution_bezier



  function u_of_arc_fractions_bezier(bezier, arc_fractions) result(u)
    !! Maps target arc-length fractions [0,1] back to curve parameter u [0,1]
    !!
    !! Samples the curve densely with uniform u, computes cumulative arc length,
    !! then uses linear interpolation to invert the arc-length → u mapping.
    !! This allows any desired distribution in arc-length space to be expressed
    !! as the corresponding curve parameter values.
    
    type(bezier_spec_type), intent(in) :: bezier
    double precision, intent(in) :: arc_fractions(:)
    double precision, allocatable :: u(:)
    
    double precision, allocatable :: u_dense(:), x_d(:), y_d(:), dx(:), dy(:), ds(:), s(:)
    integer :: n_dense, n_fractions
    
    n_dense = 1000
    n_fractions = size(arc_fractions)
    
    ! Sample curve densely with uniform parameter
    u_dense = linspace(0.0d0, 1.0d0, n_dense)
    call bezier_eval(bezier, u_dense, x_d, y_d)
    
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
    
  end function u_of_arc_fractions_bezier



  subroutine print_bezier_spec (ip, side, bezier)
    !! Print bezier control points for debugging

    integer, intent(in)                  :: ip
    character(*),  intent(in)            :: side 
    type (bezier_spec_type), intent(in)  :: bezier

    integer             :: i, ncp

    ncp = size(bezier%px)

    write (*,'(I4,A)', advance='no') ip, " "//side // ":  "
    do i = 1, ncp
      write (*,'(2F7.4,"   ")', advance='no') bezier%px(i), bezier%py(i)
    end do 
    write (*,*) 

  end subroutine


end module


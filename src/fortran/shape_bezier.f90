! MIT License
! Copyright (c) 2023 jxjo

module shape_bezier
   
  !-------------------------------------------------------------------------
  ! Bezier based airfoil generation, modification, ...
  !-------------------------------------------------------------------------
 
  use os_util 
  use print_util
  
  implicit none
  private
 
  ! --- bezier types --------------------------------------------------------- 

  type shape_bezier_type                              ! describe shaping of an airfoil 
    integer                       :: ndv              ! number of design variables 
    integer                       :: ncp_top          ! no of control points  
    integer                       :: ncp_bot          ! no of control points  
    double precision              :: initial_perturb  ! common max. initial perturb
  end type

  type bezier_spec_type                               ! bezier curve definition of one side
    double precision, allocatable :: px(:), py(:)     ! control point coordinates 
  end type bezier_spec_type

  public :: bezier_spec_type, shape_bezier_type


  ! --- bezier functions --------------------------------------------------------- 


  public :: bezier_eval 
  public :: bezier_create_airfoil
  interface bezier_eval_1D
     module procedure bezier_eval_1D_array        ! eval array
     module procedure bezier_eval_1D_scalar       ! eval scalar
  end interface 
  public :: bezier_eval_1D
  public :: bezier_curvature
  public :: bezier_eval_y_on_x
  public :: bezier_violates_constraints
 
  ! Bezier from/to design variables   

  public :: map_dv_to_bezier
  public :: bezier_get_dv0, get_initial_bezier
  public :: bezier_get_dv_inital_perturb
  public :: ndv_to_ncp, ncp_to_ndv, ncp_to_ndv_side

  public :: u_distribution_bezier

  ! file function 

  public :: is_bezier_file, read_bezier_file, load_bezier_airfoil, write_bezier_file
  public :: create_bezier_example_airfoil, create_bezier_MH30


  ! --- private ---------------------------------------------------
  
  type bound_type 
    double precision              :: min              ! lower boundary  
    double precision              :: max              ! upper boundary 
  end type bound_type

contains

  subroutine bezier_eval (bezier, u, x, y, der)
    !! Bezier main function - evaluates u with control point coordinates px and py
    !!
    !!    bezier:  bezier control points
    !!    u:    an array of bezier parameters 0..1 at which to return bezier value
    !!    der:  optional derivative - either 0,1 or 2 
    !!    x,y:  returns x,y coordinates at u
    type (bezier_spec_type), intent(in)      :: bezier
    double precision, allocatable,intent(in)  :: u(:) 
    double precision, allocatable,intent(out) :: x(:), y(:) 
    integer, intent(in), optional :: der 

    x =  bezier_eval_1D_array (bezier%px, u, der)   
    y =  bezier_eval_1D_array (bezier%py, u, der )

  end subroutine 



  subroutine bezier_create_airfoil (top_bezier, bot_bezier, npoint, x, y)
    !! evaluates airfoil coordinates x,y with control point coordinates px and py for top and bot 
    !
    !    top_bezier, bot:   bezier definition top and bot  
    !    npoint:       number of coordinates of airfoil x,y 
    !    x,y:          returns x,y coordinates at u
    type (bezier_spec_type), intent(in)      :: top_bezier, bot_bezier
    integer, intent(in)                       :: npoint 
    double precision, allocatable,intent(out) :: x(:), y(:) 

    double precision, allocatable :: x_top(:), y_top(:), x_bot(:), y_bot(:), u(:)
    integer                       :: npoint_top, npoint_bot, i

    npoint_bot = (npoint + 1) / 2
    npoint_top = (npoint + 1) - npoint_bot 

    ! generate top side 
    
    u = u_distribution_bezier (npoint_top)
    call bezier_eval (top_bezier, u, x_top, y_top) 
    
    ! generate bot side 
    
    u = u_distribution_bezier (npoint_bot)
    call bezier_eval (bot_bezier, u, x_bot, y_bot) 

    ! build x,y from top and bot coordinates 

    allocate (x (npoint))
    allocate (y (npoint))
  
    do i = 1, npoint_top
      x(i) = x_top (npoint_top - i + 1)
      y(i) = y_top (npoint_top - i + 1)
    end do
    do i = 1, npoint_bot-1
      x (i + npoint_top) = x_bot (i + 1)
      y (i + npoint_top) = y_bot (i + 1)
    end do

  end subroutine 



  function bezier_eval_1D_scalar (px, u, der ) 
    !! Bezier core function - evaluates u with control point coordinates x or y
    !!
    !!    pxy:  either x or y coordinates of the bezier control points
    !!    u:    scalar  0..1 at which to return bezier value
    !!    der:  optional derivative - either 0,1 or 2 
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
    !! Bezier core function - evaluates u with control point coordinates x or y
    !!
    !!    pxy:  either x or y coordinates of the bezier control points
    !!    u:    an array of normed arc length 0..1 at which to return bezier value
    !!    der:  optional derivative - either 0,1 or 2 

    use math_util,        only : diff_1D
    
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

    ! # http://math.aalto.fi/~ahniemi/hss2012/Notes06.pdf 

    n = size(px) - 1                               ! n - degree of Bezier 
    weights = px
    bezier  = 0d0

    ! adjust point weights for derivatives 

    if (derivative > 0) then 
      weights = diff_1D (weights) * n               ! new weight = difference * n
      n = n - 1                                     ! lower 1 degree
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
    !! Evaluate the y value based on x 
    !!
    !!    px, py:  x or y coordinates of the bezier control points
    !!    x:    x value to evaluate y 
    !!    epsilon:  precision of y value - default 10d-10 

    type (bezier_spec_type), intent(in) :: bezier 
    double precision, intent(in)            :: x
    double precision, intent(in), optional  :: epsilon
    double precision    :: bezier_eval_y_on_x

    double precision, allocatable    :: px(:), py(:)
    double precision                 :: eps, u0, xn, dxn, u 
    integer   :: i 

    px = bezier%px
    py = bezier%py 

    ! optimize first / last x
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

    ! define a good start value for newton iteration 
    if (x < 0.05d0) then                      
      u0 = 0.05d0
    else if (x > 0.95d0) then
      u0 = 0.95d0
    else 
      u0 = x
    end if  

    ! newton iteration to get bezier u value from x
    u = u0
    do i = 1, 50

      if (u > 1d0) u = 1d0                        ! ensure to stay within boundaries 
      if (u < 0d0) u = 1d-10 !0d0 

      xn  = bezier_eval_1D (px, u) - x            ! find root f(u) - x
      if ((abs(xn) < eps)) exit                   ! succeeded
      dxn = bezier_eval_1D (px, u, 1) 

      if (dxn == 0d0) then 
        if (xn /= 0d0) write (*,*) "Error: Bezier newton iteration with zero derivative"
        exit
      end if  

      u = u - xn / dxn 
    end do 

    ! finally get y from iterated u-value 
    if ((abs(xn) < eps)) then 
      bezier_eval_y_on_x = bezier_eval_1D (py, u)
    else
      ! print *, "Newton failed - i: ", 50, u, xn
    end if  

    return 

  end function 



  function bezier_curvature (bezier, u) result (curv)
    !! Evaluate the curvature of self at u 0..1
    !!    u :   Scalar of arc length at which to return 

    type (bezier_spec_type), intent(in)      :: bezier
    double precision, intent(in)  :: u
    double precision    :: dx, dy, ddx, ddy, curv

    dx  = bezier_eval_1D_scalar (bezier%px, u, der=1)
    dy  = bezier_eval_1D_scalar (bezier%py, u, der=1)
    ddx = bezier_eval_1D_scalar (bezier%px, u, der=2)
    ddy = bezier_eval_1D_scalar (bezier%py, u, der=2)

    curv = (ddy * dx - ddx * dy) / (dx ** 2 + dy ** 2) ** 1.5

    ! #hack to have positive curvature at le for top and bot 
    if (sum(bezier%py) > 0d0) then                    ! upper side --> inverse
      curv = -curv 
    end if  

  end function 


  ! ------------- file functions ----------------------------


  function is_bezier_file (filename)
    !! .true. if filename has ending '.bez'
    character(*),  intent(in) :: filename
    logical       :: is_bezier_file 
    character (4) :: extension 
    integer       :: length
    
    is_bezier_file = .false.
    length = len (trim(filename))
    if (length > 4) then
      extension = '.bez' !filename (length-3, length)
      if (extension == '.bez' .or. extension =='.BEZ') then
        is_bezier_file = .true.
      end if 
    end if  
  end function  


  subroutine load_bezier_airfoil (filename, npoint, name, x, y, top_bezier, bot_bezier)
    !! read airfoil bezier file and eval x, y coordinates of airfoil with npoint  
    character(*),  intent(in) :: filename
    integer, intent(in)  :: npoint
    character (len=:), allocatable, intent(out) :: name 
    double precision, allocatable, intent(out)  :: x(:), y(:) 
    type(bezier_spec_type), intent(out)         :: top_bezier, bot_bezier 

    ! read control points top and bot side 
    
    call read_bezier_file (filename, 'Top', name, top_bezier )
    call read_bezier_file (filename, 'Bot', name, bot_bezier )
    
    ! build airfoil x,y from top and bot 

    call bezier_create_airfoil (top_bezier, bot_bezier, npoint, x, y)

  end subroutine


  
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
    type (bezier_spec_type), intent(out)       :: bezier 

    double precision, dimension (100) :: px_tmp, py_tmp 
    integer :: iunit, ioerr, np, i
    logical :: do_read 
    character (255) :: in_buffer 
    character(:), allocatable :: in_line
  
    ! Open bezier definition file
  
    iunit = 12
    open(unit=iunit, file=filename, status='old', position='rewind', iostat=ioerr)
    if (ioerr /= 0) then
      write (*,*) 'Cannot find bezier definition file '//trim(filename)
      stop 1
    end if
   
    ! Read first line; determine if it is a title or not
  
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
        ! skip
      else if (do_read) then 
        np = np + 1
        read (in_line,*) px_tmp(np), py_tmp(np) 
      else 
        ! skip
      end if
    end do
  
    if (np> 0) then 
      bezier%px = px_tmp(1:np)
      bezier%py = py_tmp(1:np)
    else
      allocate( bezier%px( 0 ) ) 
      allocate( bezier%py( 0 ) )  
      write (*,*) 'Cannot read bezier definition file ' // trim(filename) // ' - Syntax error'
      stop 1
    end if 
  
    close(iunit)

    return 
  end subroutine



  subroutine create_bezier_example_airfoil (npoint, name, x, y, top_bezier, bot_bezier)
    !! create airfoil bezier example and eval x, y coordinates of airfoil with npoint  
    integer, intent(in)  :: npoint
    character (len=:), allocatable, intent(out) :: name 
    double precision, allocatable, intent(out)  :: x(:), y(:) 
    type(bezier_spec_type), intent(out)         :: top_bezier, bot_bezier 

    top_bezier%px = [   0d0,    0d0, 0.33d0,  1d0]
    top_bezier%py = [   0d0, 0.06d0, 0.12d0,  0d0]

    bot_bezier%px = [   0d0,    0d0, 0.25d0,  1d0]
    bot_bezier%py = [   0d0,-0.04d0,-0.07d0,  0d0]  

    name = "Bezier_Exmaple"
   
    ! build airfoil x,y from top and bot 

    call bezier_create_airfoil (top_bezier, bot_bezier, npoint, x, y)

  end subroutine


  subroutine create_bezier_MH30 (npoint, name, x, y, top_bezier, bot_bezier)
    !! create MH30-norm-bezier - data from Airfoil Editor match bezier 

      ! .\test-cases\reference-airfoils\MH 30-norm-bezier-7-5.bez


      ! MH 30-norm-bezier-7-5
      ! Top Start
      !  0.0000000000  0.0000000000
      !  0.0000000000  0.0143951798
      !  0.1032405700  0.1029764415
      !  0.4901254031  0.0382743125
      !  0.6314525508  0.0661920564
      !  0.8552401081  0.0130084982
      !  1.0000000000  0.0000000000
      ! Top End
      ! Bottom Start
      !  0.0000000000  0.0000000000
      !  0.0000000000 -0.0048568117
      !  0.0130325103 -0.0587325332
      !  0.5995297333 -0.0043740872
      !  1.0000000000  0.0000000000
      ! Bottom End
    

    integer, intent(in)  :: npoint
    character (len=:), allocatable, intent(out) :: name 
    double precision, allocatable, intent(out)  :: x(:), y(:) 
    type(bezier_spec_type), intent(out)         :: top_bezier, bot_bezier 

    top_bezier%px = [0.0000000000d0, 0.0000000000d0, 0.1032405700d0, 0.4901254031d0, 0.6314525508d0, 0.8552401081d0, 1.0000000000d0]
    top_bezier%py = [0.0000000000d0, 0.0143951798d0, 0.1029764415d0, 0.0382743125d0, 0.0661920564d0, 0.0130084982d0, 0.0000000000d0]

    bot_bezier%px = [0.0000000000d0, 0.0000000000d0, 0.0130325103d0, 0.5995297333d0, 1.0000000000d0]
    bot_bezier%py = [0.0000000000d0,-0.0048568117d0,-0.0587325332d0,-0.0043740872d0, 0.0000000000d0]  

    name = "MH30-norm-bezier"
   
    ! build airfoil x,y from top and bot 

    call bezier_create_airfoil (top_bezier, bot_bezier, npoint, x, y)

  end subroutine



  subroutine write_bezier_file (filename, name, top_bezier, bot_bezier)
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

    character(*),  intent(in)               :: filename, name
    type(bezier_spec_type), intent(in)     :: top_bezier, bot_bezier

    integer :: iunit, i, ncp_top, ncp_bot

    ncp_top = size(top_bezier%px)
    ncp_bot = size(bot_bezier%px)

    iunit = 13
    open  (unit=iunit, file=filename, status='replace')

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


  ! ------------- design variables and bezier---------------------------

  
  function ndv_to_ncp (ndv)
    !! get number of control points from number of design variables 
    integer, intent(in) :: ndv
    integer             :: ndv_to_ncp
    ndv_to_ncp = 1 + 1 + (ndv - 1) / 2 + 1    ! add LE, TE and x of LE tangent 
  end function

  function ncp_to_ndv_side (ncp)
    !! get number of design variables from number of control points  
    integer, intent(in) :: ncp
    integer             :: ncp_to_ndv_side
    ncp_to_ndv_side = (ncp - 3) * 2  + 1           ! subtract LE, TE and x of LE tangent 
  end function


  
  function ncp_to_ndv (ncp)
    !! get number of design variables from number of control points of top and bot  
    integer, intent(in) :: ncp
    integer             :: ncp_to_ndv

     ! subtract LE, TE and x of LE tangent to get design variables 
    ncp_to_ndv = (ncp - 3) * 2  + 1   

  end function



  subroutine map_dv_to_bezier (side, dv_in, te_gap, bezier)
    !! build bezier control points from design vars with a 'te_gap' 
    !
    !  p1   = 0     , 0
    !  p2   = 0     , dv(1)
    !  p3   = dv(2) , dv(3)
    !  p4   = dv(4) , dv(5)
    !  ...
    !  pn   = 1     , te_gap 
    !
    character(3), intent(in)     :: side 
    double precision, intent(in) :: dv_in(:)
    double precision, intent(in) :: te_gap
    type (bezier_spec_type), intent(out) :: bezier

    type(bound_type), allocatable   :: bounds_x(:), bounds_y(:)
    double precision                :: min_val, max_val 
    double precision, allocatable   :: dv(:) 
    integer                         :: ndv, ncp, ip, idv

    dv = dv_in

    ndv = size (dv)
    ncp  = ndv_to_ncp (ndv)                  ! calc number of control points 

    if (ndv < 3) call my_stop ('Bezier: Number of design variables less than 3')

    ! sanity check for bounaries 

    do idv = 1, ndv 
      dv(idv) = max (0d0, dv(idv))
      dv(idv) = min (1d0, dv(idv))
    end do 

    ! init new bezier control points with fixed boundaries 

    call bezier_cp_bounds (side, ncp, te_gap, bounds_x, bounds_y)

    bezier%px = bounds_x%min                        ! will set fixed control points like LE 
    bezier%py = bounds_y%min 

    ! map design vars to control point coordinates (scale within boundaries)

    min_val = bounds_y(2)%min                       ! LE tangent 
    max_val = bounds_y(2)%max
    if (side == "Top") then
      bezier%py(2) = min_val + dv(1) * abs (max_val - min_val) 
    else                                            ! invert for lower side 
      bezier%py(2) = max_val - dv(1) * abs (max_val - min_val) 
    end if

    do ip = 3, ncp-1  

      idv = (ip - 2) * 2                            ! x value 
      min_val = bounds_x(ip)%min
      max_val = bounds_x(ip)%max
      bezier%px(ip) = min_val + dv(idv) * abs (max_val - min_val)  

      idv = idv + 1                                 ! y value 
      min_val = bounds_y(ip)%min
      max_val = bounds_y(ip)%max
      if (side == "Top") then
        bezier%py(ip) = min_val + dv(idv) * abs (max_val - min_val)  
      else                                          ! invert for lower side 
        bezier%py(ip) = max_val - dv(idv) * abs (max_val - min_val)  
      end if 
    end do  
  
  end subroutine


  function bezier_get_dv0 (side, bezier) result (dv) 
    !! get inital design vars dv0 from bezier control points
    !!     (initial equals bezier)  
    !
    !  p1   = 0     , 0
    !  p2   = 0     , dv(1)
    !  p3   = dv(2) , dv(3)
    !  p4   = dv(4) , dv(5)
    !  ...
    !  pn   = 1     , te_gap 
    !
    character(3), intent(in)                    :: side 
    type(bezier_spec_type), intent(in)          :: bezier
    double precision, allocatable               :: dv(:)

    type(bound_type), allocatable   :: bounds_x(:), bounds_y(:)
    double precision                :: min_val, max_val 


    integer :: ndv, ncp, icp, idv

    ncp  = size(bezier%px)
    ndv = ncp_to_ndv_side (ncp) 

    if (ndv < 3) then 
      call my_stop ('Bezier: Number of design variables less than 3.')
    end if 

    allocate (dv (ndv))
    dv = 0d0

    ! get bounds auf control points (te gap is not known in this context) 

    call bezier_cp_bounds (side, ncp, 0d0, bounds_x,  bounds_y)

    ! sanity check - does bezier already violate bounds? 

    do icp = 1, ncp - 1                       ! do not check te   
                                    
      if (bezier%px(icp) < bounds_x(icp)%min .or. bezier%px(icp) > bounds_x(icp)%max) then 
        call print_error ("Bezier "//side//" control point "//stri(icp)//" outside x-bounds")
      end if 

      if (bezier%py(icp) < bounds_y(icp)%min .or. bezier%py(icp) > bounds_y(icp)%max) then 
        call print_error ("Bezier "//side//" control point "//stri(icp)//" outside y-bounds")
      end if 
    end do 


    ! map control point coordinates to design vars

    ! start tangent  - only y 
    min_val = bounds_y(2)%min                       ! LE tangent 
    max_val = bounds_y(2)%max
    if (side == "Top") then
      dv(1) = (bezier%py(2) - min_val) / abs (max_val - min_val)                                   
    else                                              ! invert for lower side 
      dv(1) = (max_val - bezier%py(2)) / abs (max_val - min_val)  
    end if                                  

    ! normal control points 3..n-1 take x + y 
    idv = 1
    do icp = 3, ncp-1   

      idv = idv + 1 
      min_val = bounds_x(icp)%min                       ! x value 
      max_val = bounds_x(icp)%max
      dv(idv) = (bezier%px(icp) - min_val) / abs (max_val - min_val)                                   

      idv = idv + 1 
      min_val = bounds_y(icp)%min                       ! y value 
      max_val = bounds_y(icp)%max
      if (side == "Top") then
        dv(idv) = (bezier%py(icp) - min_val) / abs (max_val - min_val)  
      else                                              ! invert for lower side 
        dv(idv) = (max_val - bezier%py(icp)) / abs (max_val - min_val)  
      end if                                  
    end do  
  
    ! print *, side 
    ! print *,"px    ", bezier%px
    ! print *,"py    ", bezier%py
    ! print *,"y min ", bounds_y%min 
    ! print *,"y max ", bounds_y%max 
    ! print *,"dv    ", dv 

  end function



  function bezier_get_dv_inital_perturb (initial, side, bezier) result (dv_perturb) 

    !! get inital perturb of design vars depending on px and py 
    !!     (the common initial value is defined in inputs)  
    !
    !  p1   = 0     , 0
    !  p2   = 0     , dv(1)
    !  p3   = dv(2) , dv(3)
    !  p4   = dv(4) , dv(5)
    !  ...
    !  pn   = 1     , te_gap 
    !
    double precision, intent(in)                :: initial 
    character(3), intent(in)                    :: side 
    type(bezier_spec_type), intent(in)          :: bezier
    double precision, allocatable               :: dv_perturb(:)

    type(bound_type), allocatable   :: bounds_x(:), bounds_y(:)
    double precision                :: extent 
    integer                         :: ndv, ncp, icp, idv

    ncp = size(bezier%px)
    ndv = ncp_to_ndv_side (ncp) 
    allocate (dv_perturb (ndv))
    dv_perturb = 0d0

    ! get bounds auf control points 

    call bezier_cp_bounds (side, ncp, 0d0, bounds_x,  bounds_y)

    ! map bounds extent to dv_perturb scaled by initial perturb 

    ! start tangent  - only y - not too volatile 
    extent = abs (bounds_y(2)%max - bounds_y(2)%min)
    dv_perturb(1) = extent * initial * 0.1   

    ! normal control points 3..n-1 take x + y 
    idv = 1
    do icp = 3, ncp-1   

      idv = idv + 1 
      extent = abs (bounds_x(icp)%max - bounds_x(icp)%min)
      dv_perturb(idv) = extent * initial * 2d0          ! x value may move around more 

      idv = idv + 1 
      extent = abs (bounds_y(icp)%max - bounds_y(icp)%min)
      dv_perturb(idv) = extent * initial * 0.5d0          ! y value may move not too much
    end do  
  
    ! print *, side 
    ! print '(A,2F8.4)',"initial        ", initial
    ! print '(A,20F8.4)',"bounds x min  ", bounds_x%min
    ! print '(A,20F8.4)',"bounds x max  ", bounds_x%max
    ! print '(A,20F8.4)',"bounds y min  ", bounds_y%min
    ! print '(A,20F8.4)',"bounds y max  ", bounds_y%max
    ! print '(A,20F8.4)',"dv_perturb    ", dv_perturb

  end function


  subroutine bezier_cp_bounds (side, ncp, te_gap, bounds_x, bounds_y)

    !! get the bounds of control points  
    !  - for re-mapping of designvars to control points 

    character(3), intent(in)      :: side 
    integer, intent(in)           :: ncp
    double precision, intent(in)  :: te_gap 
    type(bound_type), allocatable, intent(out) :: bounds_x(:), bounds_y(:)
    integer             :: ndv, ip
    double precision    :: min_val, max_val 

    ndv = ncp_to_ndv_side (ncp) 

    ! init new bounds with default values 

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

    ! start tangent  - limited y-range - LE not too sharp or too blund  

    bounds_x(2)%max = 0d0 
    bounds_y(2)%min = 0.002d0 
    bounds_y(2)%max = 0.5d0  

    ! control points between LE and TE - quite free moving around   

    do ip = 3, ncp-1                                 
      bounds_x(ip)%min = 0.01d0                          ! x not too close to LE
      bounds_x(ip)%max = 0.95d0                          ! x not too close to TE
      bounds_y(ip)%min = -0.05d0                         ! y rough bounds 
      bounds_y(ip)%max = 0.70d0                          !  
    end do 

    ! for bot side invert y values 

    if (side == 'Bot') then 

      do ip = 1, ncp                                      ! mirror all y-bounds
        min_val = bounds_y(ip)%min            
        max_val = bounds_y(ip)%max  
        bounds_y(ip)%min = -max_val                      
        bounds_y(ip)%max = -min_val   
      end do 
    end if 

  end subroutine


  subroutine get_initial_bezier (x, y, ncp, bezier)
    !! get initial bezier control points x, y of an airfoil side 
    !!    x, y:    coordinates of an airfoil side
    !!    ncp:     number of bezier control points 
    !!    bezier:  coordinates of bezier control points  

    use math_util,          only : interp_vector, linspace

    double precision, intent(in)  :: x(:), y(:)
    integer, intent(in)           :: ncp
    type(bezier_spec_type), intent(out) :: bezier

    integer :: i, ip, ncoord
    double precision :: px2_dummy (1), py2 (1)
    double precision, allocatable :: px_coord (:), py_coord (:)


    if (ncp < 3) then 
      call my_stop ('Bezier: Number of control points less than 3')
    end if 

    ! init new bezier control points 
    allocate (bezier%px (ncp))
    allocate (bezier%py (ncp))
    bezier%px = 0d0
    bezier%py = 0d0 
    ncoord = size(x)

    ! fix control points for le and te
    bezier%px(1)   = 0d0                                    ! le
    bezier%py(1)   = 0d0                                   
    bezier%px(ncp) = 1d0                                    ! te 
    bezier%py(ncp) = y(ncoord)                              ! set te gap       
    
    ! start tangent (point 2)  
    bezier%px(2) = 0d0                                      ! le tangent 
    px2_dummy = 0.1d0                                       ! ... will retrieve y value at x=0.1
    call interp_vector(x, y, px2_dummy, py2)
    bezier%py(2) = py2(1)                                   ! le tangent 

    ! equally spaced control points in between le and te, py interpolated from y
    px_coord = linspace (0d0, 1d0, int(ncp-1))
    allocate (py_coord(size(px_coord)))
    call interp_vector(x, y, px_coord, py_coord)

    ip = 3                                          ! from 3rd point to np-1
    do i = 2, size(px_coord) - 1
      bezier%px(ip) = px_coord (i) 
      bezier%py(ip) = py_coord (i) * 1.2d0          ! add 20% for better bezier fit 
      ip = ip + 1
    end do 
  
  end subroutine

  function bezier_violates_constraints (bezier) result (violates) 
  
    !! checks if bezier px, py violate constraints like boundary or 
    !! too much overtake of control points 

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

    do i = 2, size(bezier%px)
      if ((bezier%px(i-1) - bezier%px(i)) > 0.1d0 ) then 
        ! print *,"#### overtake ", i, bezier%px(i-1),  bezier%px(i), bezier%px(i-1) - bezier%px(i)
        violates = .true.
        return
      end if  
    end do 


  end function 


  ! ------------- helper functions ----------------------------


  function Ni (n, i ) 
    !! Binomial Coefficients
    !   n:    number of points 
    !   i:    index 0..n-1  (fortran) 
    integer, intent(in)     :: n, i
    double precision  :: Ni
    ! Gamma(n+1) == fact(n) 
    Ni = gamma (real(n+1)) / (gamma (real(i+1)) * gamma(real(n-i+1)))   ! N = n! / (i! * (n-i)!)

  end function 

  function basisFunction(n, i1, u) 
    !! bernstein basis polynomial   
    !   def basisFunction (n, i, u):
    !       J = np.array (Ni(n, i) * (u ** i) * (1 - u) ** (n - i))
    !       return J 
    integer, intent(in)     :: n, i1 
    double precision, dimension(:), intent(in) :: u 
    double precision, dimension(size(u)) :: basisFunction 
    integer           :: i 

    i = i1 - 1                                    ! i = 0.. (n-1) 
    basisFunction = Ni (n,i) * ( u**i) * (1-u) ** (n-i)

  end function 


  function cosinus_distribution (nPoints)

    !! returns a special cosinus distibuted array of u between 0..1
    !
    !   Bezier needs a special u cosinus distribution as the ponts are bunched

    use math_util,        only : linspace

    integer, intent(in)     :: nPoints 
    double precision, dimension(nPoints) :: cosinus_distribution

    double precision, dimension(nPoints) :: beta, u 
    double precision      :: pi, umin, umax

    pi = acos(-1.d0)

    ! special cosinus distribution with strong bunch at start and light bunch at end  

    beta = linspace (0.25d0, 0.7d0, nPoints) * pi 
    u    = (1d0 - cos(beta)) * 0.5

    ! normalize 
    umin = minval(u)
    umax = maxval(u)
    u = (u - umin) / (umax - umin)

    !ensure 0.0 and 1.0 
    u (1) = 0d0 
    u (nPoints) = 1d0

    cosinus_distribution = u

  end function 



  function u_distribution_bezier (nPoints)
    !! a special distribution for Bezier curve to achieve a similar bunching to splined airfoils
    integer, intent(in)     :: nPoints 

    double precision, dimension(nPoints)    :: u, u_distribution_bezier
    double precision, dimension(nPoints-1)  :: du
    double precision      :: te_du_end, te_du_growth, le_du_start, le_du_growth, du_ip
    integer               :: ip, nPanels

    ! for a constant du the resulting arc length of a curve section (panel) is proportional the 
    ! reverse of the curvature, so it fits naturally the need of airfoil paneling especially
    ! at LE. For LE and TE a little extra bunching is done ...

    te_du_end    = 0.5d0                          ! size of last du compared to linear du
    te_du_growth = 1.4d0                          ! how fast panel size will grow 

    le_du_start  = 0.8d0                          ! size of first du compared to linear du
    le_du_growth = 1.1d0                          ! how fast panel size will grow 

    nPanels = nPoints - 1
    u  = 0d0
    du = 1d0

    ! start from LE backward - increasing du 
    du_ip = le_du_start 
    ip = 1
    do while (du_ip < 1.0)
      du(ip) = du_ip
      ip = ip + 1
      du_ip = du_ip * le_du_growth
    end do

    ! run from TE forward - increasing du 
    du_ip = te_du_end
    ip = size(du) 
    do while (du_ip < 1.0)
      du(ip) = du_ip
      ip = ip - 1
      du_ip = du_ip * te_du_growth
    end do

    ! build u array and normalized to 0..1
    do ip = 1, nPanels
      u(ip+1) = u(ip) + du(ip) 
    end do 

    u_distribution_bezier = u / u(nPoints)  


  end function u_distribution_bezier

end module


! MIT License
! Copyright (c) 2025 Jochen Guenzel

module airfoil_geometry

  ! Airfoil geometry routines 

  use os_util 
  use commons
  use commons,            only : TOP, BOT
  use print_util
  use string_util,        only : stri, strf

  use airfoil_base,       only : airfoil_type, side_airfoil_type, panel_options_type, EPSILON
  use spline,             only : spline_2D_type
  use shape_bezier,       only : bezier_spec_type  
  use shape_hicks_henne,  only : hh_spec_type

  implicit none
  private

  ! --- public functions ------------------------------------------------------------

  public :: te_angle
  public :: get_geometry
  public :: set_geometry
  public :: set_geometry_by_scale
  public :: set_te_gap
  public :: eval_thickness_camber_lines
  public :: eval_y_on_x
  public :: print_coordinate_data
  public :: max_curvature_at_te
  public :: deviation_of_side


contains


  function te_angle (foil)

    !! trailing edge angle of foil in degrees 

    use math_util,        only : tangent_angle

    type(airfoil_type), intent(in)  :: foil
    double precision :: te_angle, upper_angle, lower_angle
  
    upper_angle = tangent_angle (foil%top%x, foil%top%y, 0.95d0, 1.0d0)
    lower_angle = tangent_angle (foil%bot%x, foil%bot%y, 0.95d0, 1.0d0)
    te_angle = abs(upper_angle - lower_angle)

  end function


  subroutine build_from_thickness_camber (thickness, camber, foil)

    !-----------------------------------------------------------------------------
    !! rebuild foil from a thickness and a camber line 
    !! - recalc curvature of top and bot 
    !-----------------------------------------------------------------------------

    use airfoil_base,     only: build_from_sides

    type(side_airfoil_type), intent(in)   :: thickness, camber
    type(airfoil_type), intent(inout)     :: foil

    double precision, allocatable   :: bot_x_old (:) 

    ! sanity check - thickness and camber must have the same x-base 

    if (sum(thickness%x) /= sum(camber%x)) then 
      call my_stop ("rebuild_from_thicknees: thickness and camber must have same x values" )
    end if 
    if (.not. allocated (foil%bot%x)) then 
      call my_stop ("rebuild_from_thicknees: foil%bot%x isn't available" )
    end if 
    
    ! save bot side paneling 

    bot_x_old = foil%bot%x

    ! easy rebuild of top and bot side 

    foil%top%x    =  thickness%x
    foil%top%y    =  thickness%y / 2d0 + camber%y

    foil%bot%x    =  thickness%x
    foil%bot%y    = -thickness%y / 2d0 + camber%y

    ! rebuild x and y out of sides - new spline is build 

    call build_from_sides (foil)

    ! restore old panel on bot side 

    foil%bot%x = bot_x_old
    foil%bot%y = eval_bot_side_with_xnew (foil, bot_x_old)

    ! final rebuild x and y out of sides 

    call build_from_sides (foil)

  end subroutine 


  
  subroutine get_geometry (foil, maxt, xmaxt, maxc, xmaxc) 

    !-----------------------------------------------------------------------------
    !! evaluates max thickness and camber values 
    !-----------------------------------------------------------------------------

    use airfoil_base,   only : is_normalized_coord, split_foil_into_sides, normalize

    type (airfoil_type), intent(in)       :: foil 
    double precision, intent(out)         :: maxt, xmaxt, maxc, xmaxc

    type (airfoil_type)                   :: tmp_foil 
    type (side_airfoil_type)              :: thickness, camber

    ! sanity check - get_geometry may be called with a 'raw' airfoil 

    if (.not. is_normalized_coord (foil)) then 
      tmp_foil = foil
      call normalize (tmp_foil)
    else
      tmp_foil = foil 
      if (.not. allocated(tmp_foil%top%x)) then 
        call split_foil_into_sides (tmp_foil)
      end if 
    end if 

    ! evaluate thickness and camber line of airfoil 

    call eval_thickness_camber_lines (tmp_foil, thickness, camber) 

    call eval_highpoint_of_line (thickness, xmaxt, maxt)
    call eval_highpoint_of_line (camber,    xmaxc, maxc)

  end subroutine 



  subroutine set_geometry (foil, maxt, xmaxt, maxc, xmaxc) 

    !-----------------------------------------------------------------------------
    !! set geometry values like  max thickness and camber values 
    !-----------------------------------------------------------------------------

    use airfoil_base,   only : is_normalized_coord, split_foil_into_sides, is_dat_based, normalize

    type (airfoil_type), intent(inout)      :: foil 
    double precision, intent(in),optional   :: maxt, xmaxt, maxc, xmaxc

    type (airfoil_type)                   :: tmp_foil 
    type (side_airfoil_type)              :: thickness, camber
    double precision                      :: fac, maxt_cur, xmaxt_cur, maxc_cur, xmaxc_cur

    ! sanity check 

    if (.not. is_dat_based (foil)) then 
      call my_stop("set_geometry: can only be set for dat-based airfoils. ")
    end if

    ! sanity check - set_geometry may be called with a 'raw' airfoil 

    if (.not. is_normalized_coord (foil)) then 
      tmp_foil = foil
      call normalize (tmp_foil)
    else
      tmp_foil = foil 
      if (.not. allocated(tmp_foil%top%x)) then 
        call split_foil_into_sides (tmp_foil)
      end if 
    end if 

    ! evaluate thickness and camber line of airfoil 

    call eval_thickness_camber_lines (tmp_foil, thickness, camber) 


    ! set new max thickness and its position 
    
    if (present(maxt) .or. present(xmaxt)) then 
      call eval_highpoint_of_line (thickness, xmaxt_cur, maxt_cur)

      if (present(maxt)) then 
        fac = maxt / maxt_cur 
        thickness%y = thickness%y * fac                                 ! just multiply thickness - done
      end if 

      if (present(xmaxt)) then 
        thickness%y = move_xmax_of_line (thickness, xmaxt_cur, xmaxt)   ! a little bit more complicated
      end if 

    end if 


    ! set new max camber and its position 
    
    if (present(maxc) .or. present(xmaxc)) then 
      call eval_highpoint_of_line (camber, xmaxc_cur, maxc_cur)

      if (present(maxc)) then 
        fac = maxc / maxc_cur 
        camber%y = camber%y * fac                                       ! just multiply camber - done
      end if 
      if (present(xmaxc)) then  
        camber%y = move_xmax_of_line (camber, xmaxc_cur, xmaxc)         ! a little bit more complicated
      end if 

    end if 

    ! finally rebuild foil out of thickness and camber line 

    foil = tmp_foil                                         ! bot%x needed for build ...
    
    call build_from_thickness_camber (thickness, camber, foil)

  end subroutine 



  subroutine set_geometry_by_scale (foil, fmaxt, fxmaxt, fmaxc, fxmaxc, fle_radius, le_blend) 

    !-----------------------------------------------------------------------------
    !! set geometry values like max thickness and camber values by a scale factor 
    !! - all values must be provided. '1.0' means - do nothing  
    !!   fmaxt:       factor max thick          0.01 .. 1 .. 
    !!   fxmaxt:      factor max thick pos       0.1 .. 1 .. 1.9 
    !!   fmaxc:       factor max camber         0.01 .. 1 ..
    !!   fxmaxc:      factor max camber pos      0.1 .. 1 .. 1.9 
    !!   fle_radius:  factor le radius           0.1 .. 1 .. 10.0
    !!   le_blend:    bleding distance          0.01 .. 1  
    !-----------------------------------------------------------------------------

    use airfoil_base,   only : is_normalized_coord, is_dat_based
    use math_util,      only : clip

    type (airfoil_type), intent(inout)    :: foil 
    double precision, intent(in)          :: fmaxt, fxmaxt, fmaxc, fxmaxc, fle_radius, le_blend

    type (side_airfoil_type)              :: thickness, camber
    double precision                      :: ft, fxt, fc, fxc, fr, blend
    double precision                      :: xmaxt, xmaxc
    double precision                      :: maxt_cur, xmaxt_cur, maxc_cur, xmaxc_cur

    ! sanity check 

    if (.not. is_dat_based (foil)) then 
      call my_stop("set_geometry: can only be set for dat-based airfoils. ")
    end if

    if (.not. is_normalized_coord (foil)) & 
      call my_stop ("set_geometry_by_scale: airfoil isn't normalized")

    ft    = max (fmaxt, 0.01d0)
    fxt   = clip (fxmaxt, 0.1d0, 1.9d0) 
    fc    = max (fmaxc, 0.01d0)
    fxc   = clip (fxmaxc, 0.1d0, 1.9d0) 
    fr    = clip (fle_radius, 0.1d0, 10d0) 
    blend = clip (le_blend, 0.01d0, 1d0) 

    ! do nothing if all factors are 1.0 

    if (ft == 1d0 .and. fxt == 1d0 .and. fc == 1d0 .and. fxc == 1d0 .and. fr == 1d0) &
      return    

    ! evaluate thickness and camber line of airfoil and theier high points 

    call eval_thickness_camber_lines (foil, thickness, camber) 


    ! set new max thickness and its position 
    
    if (ft /= 1d0 .or. fxt /= 1d0) then 

      call eval_highpoint_of_line (thickness, xmaxt_cur, maxt_cur)

      thickness%y = thickness%y * ft                        ! just multiply thickness - done

      if (fxt < 1d0) then                                   ! calc new pos from factor and shift 
        xmaxt   =  xmaxt_cur * fxt
      else  
        xmaxt   =  xmaxt_cur  + (fxt -1d0) * (1d0 - xmaxt_cur)
      end if 
      thickness%y = move_xmax_of_line (thickness, xmaxt_cur, xmaxt)  

    end if 

    ! set new max camber and its position 
    
    if (fc /= 1d0 .or. fxc /= 1d0) then 

      call eval_highpoint_of_line (camber, xmaxc_cur, maxc_cur)

      camber%y = camber%y * fc                              ! just multiply thickness - done

      if (fxc < 1d0) then                                   ! calc new pos from factor and shift 
        xmaxc   =  xmaxc_cur * fxc
      else  
        xmaxc   =  xmaxc_cur  + (fxc -1d0) * (1d0 - xmaxc_cur)
      end if 
      camber%y = move_xmax_of_line (camber, xmaxc_cur, xmaxc)  

    end if 

    ! set le radius with its blending distanc 

    if (fr /= 1d0) then 
      thickness%y = set_le_radius (thickness, fr, blend)
    end if 

    ! finally rebuild foil out of thickness and camber line 

    call build_from_thickness_camber (thickness, camber, foil)

  end subroutine 




  subroutine set_te_gap (foil, gap_new, xBlend_in) 

    !-----------------------------------------------------------------------------
    !! set trailing edge gap 
    !-----------------------------------------------------------------------------

    use airfoil_base,       only : is_normalized_coord, build_from_sides, split_foil_into_sides
    use airfoil_base,       only : is_dat_based, normalize
    use math_util,          only : clip

    type (airfoil_type), intent(inout)      :: foil 
    double precision, intent(in)            :: gap_new 
    double precision, intent(in), optional  :: xBlend_in 

    double precision    :: gap, dgap, xblend, arg, tfac
    integer             :: i, npt, npb
    
    ! sanity check 

    if (.not. is_dat_based (foil)) then 
      call my_stop("set_geometry: can only be set for dat-based airfoils. ")
    end if

    if (.not. is_normalized_coord (foil)) then 
      call normalize (foil)
    else
      if (.not. allocated(foil%top%x)) then 
        call split_foil_into_sides (foil)
      end if 
    end if 

    ! optional blending distance 

    if (present (xBlend_in)) then 
      xBlend = xBlend_in
    else 
      xBlend = 0.8d0 
    end if 
    xBlend = clip (xBlend, 0.1d0, 1.0d0)

    npt = size(foil%top%x) 
    npb = size(foil%bot%x) 
    
    gap  = foil%top%y(npt) - foil%bot%y(npb)
    dgap = gap_new - gap 

    if (dgap == 0d0) return 

    ! top side - go over each point, changing the y-thickness appropriately 

    do i = 1, npt

      ! thickness factor tails off exponentially away from trailing edge
      arg = min ((1d0 - foil%top%x(i)) * ( 1d0/xBlend - 1d0), 15d0)
      tfac = exp(-arg)
      foil%top%y(i) = foil%top%y(i) + 0.5d0 * dgap * foil%top%x(i) * tfac 

    end do 

    ! bot side  

    do i = 1, npb

      ! thickness factor tails off exponentially away from trailing edge
      arg = min ((1d0 - foil%bot%x(i)) * ( 1d0/xBlend - 1d0), 15d0)
      tfac = exp(-arg)
      foil%bot%y(i) = foil%bot%y(i) - 0.5d0 * dgap * foil%bot%x(i) * tfac 

    end do 

    ! rebuild airfoil out of top and bot side 

    call build_from_sides (foil)


  end subroutine 


  
  function set_le_radius (thickness, factor, xBlend) result (new_y)

    !-----------------------------------------------------------------------------
    !! set leading edge radius bei 'factor' - blending new y from 0..xBlend 
    !! on the thickness distribution line   
    !  The procedere is based on xfoil  
    !-----------------------------------------------------------------------------

    use math_util,                only : clip

    type (side_airfoil_type), intent(in)  :: thickness 
    double precision, intent(in)          :: factor, xBlend 

    double precision, allocatable   :: new_y (:) 
    double precision                :: blend, fac, arg, srfac, tfac
    integer                         :: i, np

    blend = clip (xBlend, 0.001d0, 1d0)
    fac   = clip (factor, 0.01d0, 10d0)

    ! go over each thickness point, changing the thickness appropriately

    np = size(thickness%y)
    new_y = thickness%y                        

    do i = 2, np - 1                                  ! exclude le, te - avoid numerical issues 

      ! thickness factor tails off exponentially towards trailing edge

      arg   = min (thickness%x(i) / blend, 15d0)
      srfac = (abs (fac)) ** 0.5d0
      tfac  = 1d0 - (1d0 - srfac) * exp(-arg)
      new_y(i) = thickness%y(i) * tfac

    end do 

  end function



  subroutine eval_thickness_camber_lines (foil, thickness, camber) 

    !-----------------------------------------------------------------------------
    !! evaluates thickness and camber lines as side_airfoil_type
    !-----------------------------------------------------------------------------

    type (airfoil_type), intent(in)       :: foil 
    type (side_airfoil_type), intent(out) :: thickness, camber
    double precision, allocatable         :: bot_y_new (:)

    ! get bot y coordinates based on upper x coordinates 

    bot_y_new = eval_bot_side_with_xnew (foil, foil%top%x)

    ! thickness and camber can now be easily calculated 

    thickness%name  = 'thickness'
    thickness%x     = foil%top%x
    thickness%y     = foil%top%y - bot_y_new

    camber%name     = 'camber'
    camber%x        = foil%top%x
    camber%y        = (foil%top%y + bot_y_new) / 2d0

  end subroutine 



  subroutine eval_highpoint_of_line (line, max_pos, max_val)

    !-----------------------------------------------------------------------------
    !! evaluates max value and max pos of a line (side_airfoil_type)
    !-----------------------------------------------------------------------------

    use spline,       only : spline_1D, spline_1D_type, eval_1D

    type (side_airfoil_type), intent(in) :: line
    double precision, intent(out)        :: max_val, max_pos 
    integer                       :: imax, istart, iend, i
    double precision              :: x, dy, ddy
    double precision, allocatable :: x_helper(:), y_helper(:)
    type(spline_1D_type)          :: spl

    ! first guess of highpoint 

    imax = maxloc(line%y,1)

    istart = max (imax - 5, 1)
    iend   = min (imax + 5, size(line%y))

    ! build a litlle helper spline around first highpoint guess 

    x_helper = line%x (istart: iend)
    y_helper = line%y (istart: iend)
    
    spl = spline_1D (x_helper, y_helper)

    ! newton iteration to get x where dy=0 (tangent is horizontal)

    x =line%x(imax)                              ! approx. start value for newton iteration
    do i = 1, 50

      ! ensure to stay within boundaries 
      if (x > x_helper(size(x_helper))) x = x_helper(size(x_helper))                       
      if (x < x_helper(1))              x = x + 1d-10  

      dy = eval_1D (spl, x, 1)                    ! eval spline to get actual dy (=f(x))

      if (abs(dy) < EPSILON) exit                 ! succeeded

      ddy = eval_1D (spl, x, 2)                   ! eval second derivative (=f'(x) ) 
    
      if (ddy == 0d0) & 
        call my_stop ( "eval_max_of_line: zero derivative in Newton iteration")

      x = x - dy / ddy                            ! Newton delta 
    end do 

    if (abs(dy) >= EPSILON) then 
      call print_warning ("eval_max_of_line:  Newton failed after "//stri(i)// &
                          " iterations (x="//strf('F6.4',x)//')')
    end if  

   ! finally get y from iterated x-value 

    max_pos  = x
    max_val  = eval_1D (spl, x, 0) 

  end subroutine 



  function eval_bot_side_with_xnew (foil, xnew) result (y)

    !-----------------------------------------------------------------------------
    !! returns y-values of bot side evaluated with x-values of top side 
    !-----------------------------------------------------------------------------

    use spline,         only : eval_1D
    use airfoil_base,   only : is_normalized_coord

    type (airfoil_type), intent(in)           :: foil 
    double precision, allocatable, intent(in) :: xnew (:)
    double precision, allocatable     :: y (:)
    integer               :: i, n 

    ! sanity foil must be normalized
    
    if (.not. is_normalized_coord (foil)) &
      call my_stop ( "eval_bot_side: airfoil not normalized")

    n = size(xnew)
    allocate (y(n))

    do i = 1, n
      y(i) = eval_y_on_x (foil%bot, xnew(i), foil%spl)
    end do 

  end function 


  function deviation_of_side (side, target_side, spl) result (devi)

    !-----------------------------------------------------------------------------
    !! returns deviation as array of abs values of target points to foil side
    !! spl is needed if side is based on cubic spline 
    !-----------------------------------------------------------------------------

    type (side_airfoil_type), intent(in) :: side, target_side
    type (spline_2D_type), intent(in), optional   :: spl
    double precision, allocatable      :: devi(:)
    integer                            :: i, n

    n = size(target_side%x)
    allocate (devi(n))

    do i = 1, n 
      if (present(spl)) then
        devi(i) = abs (eval_y_on_x(side, target_side%x(i), spl) - target_side%y(i))
      else
        devi(i) = abs (eval_y_on_x(side, target_side%x(i)) - target_side%y(i))
      end if
    end do 
    
  end function


  function spline_eval_y_on_x (is_top_side, spl, xn) result (y)

    !-----------------------------------------------------------------------------
    !! Private helper: returns y-value at x evaluated with spline
    !! Uses spl%le_index for leading edge detection (stored during spline creation)
    !-----------------------------------------------------------------------------

    use spline,         only : eval_1D
    use math_util,      only : round

    logical, intent(in)                       :: is_top_side
    type (spline_2D_type), intent(in)         :: spl
    double precision, intent(in)              :: xn
    double precision                          :: y 
    double precision                          :: s, s_start, s_end, x, dx, delta
    integer                                   :: i, le_index

    if (xn < 0d0 .or. xn > 1d0) &
      call my_stop ( "spline_eval_y_on_x: x value not in range from 0 to 1")

    if (.not. allocated(spl%s)) &
      call my_stop ( "spline_eval_y_on_x: spline isn't allocated")

    ! Use leading edge index stored in spline
    le_index = spl%le_index

    ! Determine s boundaries based on is_top
    if (is_top_side) then
      s_start = spl%s(1) 
      s_end   = spl%s(le_index)
    else
      s_start = spl%s(le_index) 
      s_end   = spl%s(size(spl%s))
    end if  

    ! Handle edge cases by evaluating spline at boundaries
    if (xn < EPSILON) then
      y = 0d0
    else if (xn > 1d0 - EPSILON) then
      ! Evaluate spline at TE - round to avoid floating-point noise
      if (is_top_side) then
        y = eval_1D(spl%sply, s_start)            ! TE for top (first s value)
      else
        y = eval_1D(spl%sply, s_end)              ! TE for bot (last s value)
      end if
    else

      ! Define approximate start value for Newton iteration
      if (.not. is_top_side) then
        if (xn < 0.05) then
          s = s_start + 0.05d0
        else if (xn > 0.95) then
          s = s_end - 0.05d0
        else 
          s = s_start + xn
        end if  
      else
        if (xn < 0.05) then
          s = s_end - 0.05d0
        else if (xn > 0.95) then
          s = s_start + 0.05d0
        else 
          s = s_end - xn
        end if  
      end if  

      ! Newton iteration to get spline arc s value from x
      do i = 1, 50
  
        if (s > s_end) s = s_end  ! ensure to stay within boundaries

        x = eval_1D(spl%splx, s, 0)  ! eval spline to get actual x
        delta = x - xn
        if (abs(delta) < EPSILON) exit  ! succeeded

        dx = eval_1D(spl%splx, s, 1)  ! eval first derivative for Newton
      
        if (dx == 0d0 .and. (x /= 0d0)) & 
          call my_stop("spline_eval_y_on_x: zero derivative in Newton iteration")
  
        s = s - delta / dx  ! Newton delta

      end do 

      if (abs(delta) >= EPSILON) then 
        !$omp critical 
        print *, xn, x, dx        
        call print_warning("spline_eval_y_on_x: Newton failed after "//stri(i)// &
                            " iterations (x="//strf('F6.4',xn)//')')
        !$omp end critical
      end if 

      ! Finally get y from iterated s-value
      y = eval_1D(spl%sply, s, 0) 

    end if

    y = round (y,10)      ! round to avoid floating-point noise 

  end function 



  function eval_y_on_x (side, xn, spl) result (y)

    !-----------------------------------------------------------------------------
    !! Returns y-value at x of side evaluated with bezier, bspline, or spline
    !! Side-centric API: works with side_airfoil_type directly
    !! Optional spl parameter for spline evaluation
    !-----------------------------------------------------------------------------

    use shape_bezier,       only : bezier_eval_y_on_x
    use shape_bspline,      only : bspline_eval_y_on_x
    use airfoil_base,       only : is_top

    type (side_airfoil_type), intent(in)      :: side
    double precision, intent(in)              :: xn
    type (spline_2D_type), intent(in), optional :: spl
    double precision                          :: y

    if (allocated(side%bezier%px)) then

      y = bezier_eval_y_on_x(side%bezier, xn)

    else if (allocated(side%bspline%px)) then

      y = bspline_eval_y_on_x(side%bspline, xn)

    else if (present(spl)) then

      y = spline_eval_y_on_x(is_top (side), spl, xn)

    else
      call my_stop("eval_y_on_x: neither curves nor spline provided")
    end if

  end function 



  function move_xmax_of_line (line, cur_xmax, new_xmax) result (new_y)

    !-----------------------------------------------------------------------------
    !! moves the maximum (highpoint) of line from cur_xmax to new_xmax
    !!  returns new y for line
    !  (quite similar to xfoils implmentation HIPNT)
    !-----------------------------------------------------------------------------

    use spline,       only : spline_1D, spline_1D_type, eval_1D, spline_2D, eval_spline, NATURAL
    use math_util,     only : linspace, clip

    type (side_airfoil_type), intent(in)  :: line
    double precision, intent(in)          :: cur_xmax, new_xmax
    double precision, allocatable         :: new_y(:)    
    
    double precision, allocatable     :: x(:), y(:), snew(:), xmap(:), ymap(:), new_x(:)
    double precision                  :: new_max  
    type(spline_1D_type)              :: mapSpl_1D, tmpSpl_1D
    type(spline_2D_type)              :: mapSpl_2D
    integer                           :: i, np

    ! sanity check - only a certain range of move is possible 

    if (cur_xmax == new_xmax) then 
      new_y = line%y
      return
    end if  

    new_max = clip (new_xmax, 0.1d0, 0.9d0)

    !  from xfoil: 
    !     the assumption is that a smooth function (cubic, given by the old and 
    !     new highpoint locations) maps the range 0-1 for x/c
    !     into the range 0-1 for altered x/c distribution for the same y/c
    !     thickness or camber (ie. slide the points smoothly along the x axis)

    np = size(line%x)
    x = [line%x(1), cur_xmax, line%x(np)]    
    y = [line%x(1), new_max,  line%x(np)]    
    mapSpl_2D = spline_2D (x,y, NATURAL)

    snew  = linspace (0d0, mapSPl_2D%s(size(x)), 50)
    call eval_spline (mapSpl_2D, snew, xmap, ymap)

    mapSpl_1D = spline_1D (xmap,ymap, NATURAL)

    ! finally re-map x-values to move high point 

    allocate (new_x(nP))

    do i = 1, np 
      new_x(i) = eval_1D (mapSpl_1d, line%x(i))
    end do 

    new_x(1)  = line%x(1)             ! ensure LE and TE not to change due to numeric issues
    new_x(np) = line%x(np)

    ! build a temp spline with the new x and the current y values 
    ! 1D spline with arccos is needed to avoid oscillations at LE for thickness distribution with high curvature

    tmpSpl_1D = spline_1D (new_x, line%y, arccos=.true.) 
    new_y = eval_1D (tmpSpl_1D, line%x)

    ! ensure start and end is really, really the same (numerical issues) 

    new_y(1)  = line%y(1) 
    new_y(np) = line%y(np) 

  end function 
  


  subroutine print_coordinate_data (foil1, foil2, foil3, indent)

    !-----------------------------------------------------------------------------
    !! prints geometry data like le position, te, etc of up to 3 airfoils 
    !-----------------------------------------------------------------------------

    use math_util,          only : point_type
    use airfoil_base,       only : le_of_spline, te_point

    type (airfoil_type), intent(in)           :: foil1
    type (airfoil_type), intent(in), optional :: foil2, foil3
    integer, intent(in), optional             :: indent
    
    integer                           :: nfoils, ile, i, ind, np
    type (airfoil_type)               :: foils (3) 
    type (point_type)                 :: le_s 

    nfoils = 1
    foils(1) = foil1
    if (present (foil2)) then
      nfoils = 2
      foils(2) = foil2
    end if 
    if (present (foil3)) then 
      nfoils = 3
      foils(3) = foil3
    end if 

    ind = 5
    if (present (indent)) then 
      if (indent >= 0 .and. indent < 80) ind = indent
    end if

    ! print header 
    
    call print_fixed     (""       ,ind, .false.)   
    call print_fixed     ("Name"    ,15, .false.)   
    call print_fixed     ("np"      , 5, .true.)   
    call print_fixed     ("ilE"     , 5, .true.)   

    call print_fixed     ("xLE"     ,13, .true.)   
    call print_fixed     ("yLE"     ,11, .true.)   
    call print_fixed     ("spl xLE" ,11, .true.)   
    call print_fixed     ("spl yLE" ,11, .true.)   

    call print_fixed     ("top xTE" ,13, .true.)   
    call print_fixed     ("top yTE" ,11, .true.)   
    call print_fixed     ("bot xTE" ,11, .true.)   
    call print_fixed     ("bot yTE" ,11, .true.)   
    print *

    ! print data 
    do i = 1, nfoils 

      np  = size (foils(i)%x)
      ile = minloc (foils(i)%x,1)
      le_s = le_of_spline (foils(i))

      if (abs(le_s%x) < 0.0000001d0) le_s%x = 0d0
      if (abs(le_s%y) < 0.0000001d0) le_s%y = 0d0

      call print_fixed     (""        ,ind, .false.)   
      call print_fixed     (foils(i)%filename, 15, .false.)   
      call print_colored_i (5, Q_NO, np)
      call print_colored_i (5, Q_NO, ile) 

      call print_colored_f (13, '(F10.7)', Q_NO, foils(i)%x(ile))
      call print_colored_f (11, '(F10.7)', Q_NO, foils(i)%y(ile))
      call print_colored_f (11, '(F10.7)', Q_NO, le_s%x)
      call print_colored_f (11, '(F10.7)', Q_NO, le_s%y)

      call print_colored_f (13, '(F10.7)', Q_NO, foils(i)%x(1))
      call print_colored_f (11, '(F10.7)', Q_NO, foils(i)%y(1))
      call print_colored_f (11, '(F10.7)', Q_NO, foils(i)%x(np))
      call print_colored_f (11, '(F10.7)', Q_NO, foils(i)%y(np))
      print * 

    end do 

  end subroutine


  function max_curvature_at_te (curvature)

    !! get max. curvature at the end of polyline (= TE)

    double precision              :: max_curvature_at_te
    double precision, intent(in)  :: curvature (:)
    integer                       :: npt

    npt = size(curvature)
    max_curvature_at_te =abs(curvature(npt))

  end function max_curvature_at_te


  function is_curvature_at_le_monoton (curvature)

    !! .true. if curvature at LE is monotonic from LE to TE, otherwise return .false.

    logical                       :: is_curvature_at_le_monoton
    double precision, intent(in)  :: curvature (:)
    integer                       :: npt, i, iend

    npt = size(curvature)
    iend = min (10, npt)             ! check only first 10 points from LE 
    is_curvature_at_le_monoton = .true.

    do i = 2, iend
      if (curvature(i) * curvature(i-1) < 0d0) then 
        is_curvature_at_le_monoton = .false.
        exit
      end if 
    end do 

  end function is_curvature_at_le_monoton


end module
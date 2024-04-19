! MIT License
! Copyright (c) 2024 Jochen Guenzel

module airfoil_geometry

  ! Airfoil geometry routines 

  use os_util 
  use commons
  use print_util

  use airfoil_base,       only : airfoil_type, side_airfoil_type, panel_options_type
  use spline,             only : spline_2D_type
  use shape_bezier,       only : bezier_spec_type  
  use shape_hicks_henne,  only : hh_spec_type

  implicit none
  private

  ! --- public functions ------------------------------------------------------------

  public :: normalize
  public :: repanel 
  public :: repanel_and_normalize
  public :: repanel_bezier
  public :: te_gap
  public :: le_find
  public :: is_normalized
  public :: get_geometry
  public :: set_geometry
  public :: set_geometry_by_scale
  public :: set_te_gap
  public :: eval_thickness_camber_lines
  public :: eval_deviation_at_side
  public :: eval_y_on_x_at_side_spline, eval_y_on_x_at_side
  public :: print_coordinate_data
  public :: EPSILON


  double precision, parameter    :: EPSILON = 1.d-10          ! numerical accuracy of geo 
  double precision, parameter    :: LE_PANEL_FACTOR = 0.4     ! lenght LE panel / length prev panel

contains


  function te_gap (foil)

    !! trailing edge gap of foil 

    type(airfoil_type), intent(in)  :: foil
    double precision :: te_gap
  
    te_gap = sqrt ((foil%x(1) - foil%x(size(foil%x)))**2 + &
                   (foil%y(1) - foil%y(size(foil%y)))**2)
  end function 
  


  subroutine le_check (foil, ile_close, is_le)

    !! find the point index which is closest to the real splined le  
    !! If this point is EPSILON to le, is_le is .true. 

    use math_util,      only : norm_2

    type (airfoil_type), intent(in) :: foil
    integer, intent(out)  :: ile_close
    logical, intent(out)  :: is_le

    integer :: i, npt
    double precision, allocatable :: x(:), y(:) 
    double precision, dimension(2) :: r1, r2
    double precision :: dist1, dist2, dot
    double precision :: xle, yle

    ile_close = 0
    is_le = .false.

    x = foil%X
    y = foil%y 
    npt = size(x)

    ! Get leading edge location from spline

    call le_find (foil, xle, yle)

    ! Determine leading edge index and where to add a point

    npt = size(x,1)
    do i = 1, npt-1
      r1(1) = xle - x(i)
      r1(2) = yle - y(i)
      dist1 = norm_2(r1)
      if (dist1 /= 0.d0) r1 = r1/dist1

      r2(1) = xle - x(i+1)
      r2(2) = yle - y(i+1)
      dist2 = norm_2(r2)
      if (dist2 /= 0.d0) r2 = r2/dist2

      dot = dot_product(r1, r2)
      if (dist1 < EPSILON) then                               ! point is defacto at 0,0 
        ile_close = i
        is_le = .true.
        exit
      else if (dist2 < EPSILON) then                          ! point is defacto at 0,0 
        ile_close = i+1
        is_le = .true.
        exit
      else if (dot < 0.d0) then
        if (dist1 < dist2) then
          ile_close = i
        else
          ile_close = i+1
        end if
        exit
      end if
    end do

  end subroutine 


  subroutine le_find (foil, xle, yle) 

    !----------------------------------------------------------------------------
    !! find real leading edge based on scalar product tangent and te vector = 0
    !! returns coordinates and arc length of this leading edge
    !----------------------------------------------------------------------------

    use spline,           only : eval_spline, spline_2D

    type (airfoil_type), intent(in)   :: foil 
    double precision, intent(out)     :: xle, yle

    double precision  :: sle

    sle = le_eval_spline (foil)
    call eval_spline (foil%spl, sLe,  xle,  yle, 0) 

  end subroutine  



  function le_eval_spline (foil) result (sle)

    !----------------------------------------------------------------------------
    !! find real leading edge based on scalar product tangent and te vector = 0
    !! returns arc length of this leading edge
    !----------------------------------------------------------------------------

    use spline,           only : eval_spline

    type (airfoil_type), intent(in)   :: foil 
    double precision                  :: sLe

    double precision                  :: x, y, dx, dy, ddx, ddy
    double precision                  :: dot, ddot
    double precision                  :: xTe, yTe, dxTe, dyTe, ds
    integer                           :: iter, iLeGuess

    double precision, parameter       :: EPS = 1d-10     ! Newton epsilon 

    ! sanity - is foil splined? 
    if (.not. allocated(foil%spl%s)) then 
      call my_stop ("Le_find: spline is not initialized")
    end if 

    ! first guess for uLe
    iLeGuess = minloc (foil%x, 1) 
    sLe      = foil%spl%s(iLeGuess)   
    
    ! te point 
    xTe = (foil%x(1) + foil%x(size(foil%x))) / 2d0 
    yTe = (foil%y(1) + foil%y(size(foil%y))) / 2d0 

    ! Newton iteration to get exact uLe

    do iter = 1, 50 

      sLe = min (sLe, 1.8d0)                            ! ensure to stay within boundaries 
      sLe = max (sLe, 0.2d0)
      
      call eval_spline (foil%spl, sLe,  x,  y, 0)       ! eval le coordinate and derivatives 
      call eval_spline (foil%spl, sLe, dx, dy, 1)       ! vector 1 tangent at le 
    
      dxTe = x - xTe                                    ! vector 2 from te to le 
      dyTe = y - yTe

      ! dot product of the two vectors                  ! f(u) --> 0.0  
      dot = dx * dxTe + dy * dyTe

      if ((abs(dot) < EPS)) exit                        ! succeeded

      ! df(u) for Newton 
      call eval_spline (foil%spl, sLe, ddx, ddy, 2)     ! get 2nd derivative 
      ddot = dx**2 + dy**2 + dxTe * ddx + dyTe * ddy    ! derivative of dot product 

      ds   = - dot / ddot                               ! Newton delta 
      sLe  = sLe + ds  

      ! print '(A,I5, 8F13.7)', "Newton", iter, dot, ddot, ds, sLe

    end do 


    if (((abs(dot) >= EPS))) then 

      call print_warning ("Le_find: Newton iteration not successful. Taking best guess" )
      sLe = foil%spl%s(iLeGuess) 

    end if 

  end function  




  function is_normalized (foil) result(is_norm)

    !! Checks if foil is normalized 
    !!  - Leading edge real and virtual at 0,0 
    !!  - Trailing edge at 1,0 (upper and lower side may have a gap) 

    use spline,         only : spline_2D
    use airfoil_base,   only : is_normalized_coord

    type(airfoil_type), intent(in)  :: foil

    type(airfoil_type)    :: foil_splined
    logical               :: is_norm, is_le
    integer               :: le

    is_norm = is_normalized_coord (foil)
    if (.not. is_norm) return 

    ! sanity check - spline is needed for find the real, splined LE

    foil_splined = foil                                     ! foil is just input

    if (.not. allocated(foil%spl%s)) then
      foil_splined%spl = spline_2d (foil%x, foil%y)
    end if 

    call le_check (foil_splined, le, is_le)
    if (.not. is_le) is_norm = .false.

  end function 



  subroutine repanel_and_normalize (in_foil, foil, panel_options_in)

    !-----------------------------------------------------------------------------
    !! Repanel an airfoil with npoint and normalize it to get LE at 0,0 and
    !!    TE at 1.0 (upper and lower side may have a gap)  
    !-----------------------------------------------------------------------------

    use math_util,    only : norm_2, norm2p
    use spline,       only : eval_spline, spline_2D
    use airfoil_base, only : split_foil_into_sides

    type(airfoil_type), intent(in)          :: in_foil
    type(airfoil_type), intent(out)         :: foil
    type(panel_options_type), intent(in), optional :: panel_options_in

    type(panel_options_type)                 :: panel_options
    type(airfoil_type)  :: tmp_foil
    integer             :: i, ile_close
    logical             :: le_fixed, inserted, is_le
    double precision    :: xle, yle
    character (:), allocatable     :: text

    ! use default panel options if not provided 

    if (present (panel_options_in)) then 
      panel_options = panel_options_in
    else 
      panel_options%npoint   = 161
      panel_options%le_bunch = 0.86d0
      panel_options%te_bunch = 0.6d0
    end if

    ! For normalization le_find is used to calculate the (virtual) LE of
    !    the airfoil - then it's shifted, rotated, scaled to be normalized.
    !
    ! Bad thing: a subsequent le_find won't deliver LE at 0,0 but still with a little 
    !    offset. SO this is iterated until the offset is smaller than epsilon

    tmp_foil = in_foil

    ! sanity - is foil splined? 
    if (.not. allocated(tmp_foil%spl%s)) then 
      tmp_foil%spl = spline_2D (tmp_foil%x, tmp_foil%y)
    end if 

    ! initial paneling to npoint_new
    call repanel (tmp_foil, panel_options, foil)

    le_fixed = .false. 
    inserted = .false.
  
    do i = 1,20

      call normalize (foil)

      ! repanel again to see if there is now a natural fir of splined LE

      tmp_foil = foil
      call repanel (tmp_foil, panel_options, foil)

      call le_find (foil, xle, yle)
      ! print '(A,2F12.8)', "le nach repan", xle, yle

      if (norm2p (xle, yle)  < EPSILON) then
        call normalize (foil)                   ! final normalize
        le_fixed = .true. 
        exit 
      end if
      
    end do

    ! reached a virtual LE which is closer to 0,0 than epsilon, set it to 0,0

    if (le_fixed) then 

      call le_check (foil, ile_close, is_le)

      if (.not. is_le) then 

        call print_warning ("Leading couldn't be iterated excactly to 0,0")
 
      else

        ! point is already EPSILON at 0,0 - ensure 0,0 
        foil%x(ile_close) = 0d0                         
        foil%y(ile_close) = 0d0
      end if 
    else
      call print_warning ("Leading edge couln't be moved close to 0,0. Continuing ...",3)
      write (*,*)
    end if 

    ! now split airfoil to get upper and lower sides for future needs  

    call split_foil_into_sides (foil)

    foil%name = in_foil%name // '-norm'

    text = 'Repaneling and normalizing. Airfoil will have '
    call print_action (text, stri(panel_options%npoint) //' Points') 

  end subroutine repanel_and_normalize



  subroutine normalize (foil, basic)

    !-----------------------------------------------------------------------------
    !! Translates and scales an airfoil 
    !! If 'basic' then LE of coordinates is taken - otherwise LE of spline 
    !! - length of 1 
    !! - leading edge at 0,0 and trailing edge is symmetric at 1,x
    !-----------------------------------------------------------------------------

    use spline,       only : spline_2D

    type(airfoil_type), intent(inout) :: foil
    logical, intent(in), optional     :: basic 

    double precision :: foilscale_upper, foilscale_lower
    double precision :: angle, cosa, sina
    double precision :: xle, yle, xi, yi, te_gap_old

    integer :: npoints, i, ile
    logical :: just_basic

    npoints = size(foil%x)
    te_gap_old = te_gap (foil)

    ! basic normalize or based on spline? 

    if (present(basic)) then 
      just_basic = basic 
    else 
      just_basic = .false.
    end if 

    if (just_basic) then 

      ile = minloc (foil%x, 1)
      xle = foil%x(ile) 
      yle = foil%y(ile) 

    else

      if (.not. allocated(foil%spl%s)) foil%spl = spline_2D (foil%x, foil%y) 
      call le_find (foil, xle, yle)     ! get the 'real' leading edge of spline 

    end if 

    ! Translate so that the leading edge is at the origin

    do i = 1, npoints
      foil%x(i) = foil%x(i) - xle
      foil%y(i) = foil%y(i) - yle
    end do

    ! Rotate the airfoil so chord is on x-axis 

    angle = atan2 ((foil%y(1)+foil%y(npoints))/2.d0,(foil%x(1)+foil%x(npoints))/2.d0)
    cosa  = cos (-angle) 
    sina  = sin (-angle) 
    do i = 1, npoints
      xi = foil%x(i) 
      yi = foil%y(i)
      foil%x(i) = xi * cosa - yi * sina
      foil%y(i) = xi * sina + yi * cosa
    end do

    ! Ensure TE is at x=1

    If (foil%x(1) /= 1d0) then 

      ! Scale airfoil so that it has a length of 1 
      ! - there are mal formed airfoils with different TE on upper and lower
      ! - also from rotation there is a mini diff  

      ile = minloc (foil%x, 1)
      foilscale_upper = 1.d0 / foil%x(1)
      do i = 1, ile  ! - 1
        foil%x(i) = foil%x(i)*foilscale_upper
        foil%y(i) = foil%y(i)*foilscale_upper
      end do

    end if 

    If (foil%x(npoints) /= 1d0) then 
      ile = minloc (foil%x, 1)
      foilscale_lower = 1.d0 / foil%x(npoints)
      do i = ile + 1, npoints
          foil%x(i) = foil%x(i)*foilscale_lower
          foil%y(i) = foil%y(i)*foilscale_lower
      end do
    end if 

    foil%x(1)       = 1d0                                   ! ensure now really, really
    foil%x(npoints) = 1d0

    ! Force TE to old TE gap if delta < epsilon 

    if (abs(foil%y(1)) < EPSILON) then 
      foil%y(1)       = 0d0                     ! make te gap to 0.0
      foil%y(npoints) = 0d0 
    else if (abs(foil%y(1) - (te_gap_old/2d0)) < EPSILON) then 
      foil%y(1)       =  te_gap_old/2d0 
      foil%y(npoints) = -te_gap_old/2d0 
    end if 

    ! rebuild spline 

    foil%spl = spline_2D (foil%x, foil%y)      

  end subroutine normalize



  subroutine repanel (foil_in, panel_options, foil)

    !-----------------------------------------------------------------------------
    !! repanels airfoil to npoint
    !-----------------------------------------------------------------------------

    use spline,   only : eval_spline, spline_2D

    type(airfoil_type), intent(in)        :: foil_in
    type(panel_options_type), intent(in)  :: panel_options
    type(airfoil_type), intent(out)       :: foil

    integer                         :: nPanels, nPan_top, nPan_bot 
    double precision                :: s_start, s_end, s_le
    double precision, allocatable   :: u_cos_top (:), u_cos_bot(:), s(:), s_top(:), s_bot(:)
    double precision                :: le_bunch, te_bunch

    nPanels  = panel_options%npoint - 1
    le_bunch = panel_options%le_bunch
    te_bunch = panel_options%te_bunch

    ! in case of odd number of panels, top side will have +1 panels 
    if (mod(nPanels,2) == 0) then
        nPan_top = int (nPanels / 2)
        nPan_bot = nPan_top
    else 
        nPan_bot = int(nPanels / 2)
        nPan_top = nPan_bot + 1 
    end if 

    foil = foil_in

    ! major points on arc 

    s_start = foil%spl%s(1) 
    s_le    = le_eval_spline (foil) 
    s_end   = foil%spl%s(size(foil%spl%s))

    ! normalized point distribution u 

    u_cos_top = get_panel_distribution (nPan_top+1, le_bunch, te_bunch)
    u_cos_top = u_cos_top (size(u_cos_top) : 1 : -1)        ! flip
    s_top = s_start + abs (u_cos_top - 1d0) * s_le

    u_cos_bot = get_panel_distribution (nPan_bot+1, le_bunch, te_bunch)
    s_bot = s_le + u_cos_bot * (s_end - s_le) 

    ! add new top and bot distributions 

    s = [s_top, s_bot(2:)]  

    ! new calculated x,y coordinates  

    call eval_spline (foil%spl, s, foil%x, foil%y) 

    ! Finally re-spline with new coordinates 

    foil%spl    = spline_2D (foil%x, foil%y) 

  end subroutine 



  function get_panel_distribution (nPoints, le_bunch, te_bunch) result (u) 

    !-----------------------------------------------------------------------------
    !! returns an array with cosinus similar distributed values 0..1
    !    
    ! Args: 
    ! nPoints : new number of coordinate points
    ! le_bunch : 0..1  where 1 is the full cosinus bunch at leading edge - 0 no bunch 
    ! te_bunch : 0..1  where 1 is the full cosinus bunch at trailing edge - 0 no bunch 
    !-----------------------------------------------------------------------------

    use math_util,        only : linspace, diff_1D

    integer, intent(in)           :: npoints
    double precision, intent(in)  :: le_bunch, te_bunch

    double precision, allocatable :: u(:), beta(:), du(:)

    double precision      :: ufacStart, ufacEnd, pi, du_ip
    double precision      :: te_du_end, te_du_growth
    integer               :: ip

    pi = acos(-1.d0)

    ufacStart = 0.1d0 - le_bunch * 0.1d0
    ufacStart = max(0.0d0, ufacStart)
    ufacStart = min(0.5d0, ufacStart)
    ufacEnd   = 0.65d0  ! slightly more bunch      ! 0.25 = constant size towards te 

    beta = linspace (ufacStart, ufacEnd , nPoints) * pi
    u    = (1.0d0 - cos(beta)) * 0.5d0

    ! trailing edge area 

    te_du_end = 1d0 - te_bunch * 0.9d0              ! relative size of the last panel - smallest 0.1
    te_du_growth = 1.2d0                            ! growth rate going towars le 

    du = diff_1D(u)                                 ! the differences 
    
    ip = size(du)  
    du_ip = te_du_end * du(ip)                      ! size of the last panel  
    do while (du_ip < du(ip))                       ! run forward until size reaches normal size
        du(ip) = du_ip
        ip = ip - 1
        du_ip = du_ip * te_du_growth
    end do 

    ! rebuild u array and normalize to 0..1
    u  = 0d0
    do ip = 1, size(du) 
        u(ip+1) = u(ip) + du(ip) 
    end do 

    u = u / u (size(u))

    ! ensure 0.0 and 1.0 
    u(1)       = 0d0 
    u(size(u)) = 1d0 

  end function 



  subroutine repanel_bezier (foil_in, foil, panel_options)

    !-----------------------------------------------------------------------------
    !! repanels a bezier based airfoil to npoint
    !-----------------------------------------------------------------------------

    use airfoil_base,   only : split_foil_into_sides
    use shape_bezier,   only : bezier_create_airfoil

    type(airfoil_type), intent(in)        :: foil_in
    type(panel_options_type), intent(in)  :: panel_options
    type(airfoil_type), intent(out)       :: foil

    foil = foil_in

    call print_action ('Repaneling - airfoil will have ', stri(panel_options%npoint) //' Points') 

    call bezier_create_airfoil (foil%top_bezier, foil%bot_bezier, &
                              panel_options%npoint, foil%x, foil%y)
    call split_foil_into_sides (foil)

    foil%name = foil%name // '-repan'

  end subroutine 



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

    use airfoil_base,   only : is_normalized_coord, split_foil_into_sides

    type (airfoil_type), intent(in)       :: foil 
    double precision, intent(out)         :: maxt, xmaxt, maxc, xmaxc

    type (airfoil_type)                   :: tmp_foil 
    type (side_airfoil_type)              :: thickness, camber

    ! sanity check - get_geometry may be called with a 'raw' airfoil 

    if (.not. is_normalized_coord (foil)) then 
      call repanel_and_normalize (foil, tmp_foil)
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

    use airfoil_base,   only : is_normalized_coord, split_foil_into_sides

    type (airfoil_type), intent(inout)      :: foil 
    double precision, intent(in),optional   :: maxt, xmaxt, maxc, xmaxc

    type (airfoil_type)                   :: tmp_foil 
    type (side_airfoil_type)              :: thickness, camber
    double precision                      :: fac, maxt_cur, xmaxt_cur, maxc_cur, xmaxc_cur

    ! sanity check - set_geometry may be called with a 'raw' airfoil 

    if (.not. is_normalized_coord (foil)) then 
      call repanel_and_normalize (foil, tmp_foil)
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

    use airfoil_base,   only : is_normalized_coord

    type (airfoil_type), intent(inout)    :: foil 
    double precision, intent(in)          :: fmaxt, fxmaxt, fmaxc, fxmaxc, fle_radius, le_blend

    type (side_airfoil_type)              :: thickness, camber
    double precision                      :: ft, fxt, fc, fxc, fr, blend
    double precision                      :: xmaxt, xmaxc
    double precision                      :: maxt_cur, xmaxt_cur, maxc_cur, xmaxc_cur

    ! sanity check 

    if (.not. is_normalized_coord (foil)) & 
      call my_stop ("set_geometry_by_scale: airfoil isn't normalized")

    ft    = max (fmaxt, 0.01d0)
    fxt   = min (max (fxmaxt, 0.1d0), 1.9d0) 
    fc    = max (fmaxc, 0.01d0)
    fxc   = min (max (fxmaxc, 0.1d0), 1.9d0) 
    fr    = min (max (fle_radius, 0.1d0), 10d0) 
    blend = min (max (le_blend, 0.01d0), 1d0) 

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

    type (airfoil_type), intent(inout)      :: foil 
    double precision, intent(in)            :: gap_new 
    double precision, intent(in), optional  :: xBlend_in 

    type (airfoil_type) :: tmp_foil 
    double precision    :: gap, dgap, xblend, arg, tfac
    integer             :: i, npt, npb
    
    ! sanity check - set_geometry may be called with a 'raw' airfoil 

    if (.not. is_normalized_coord (foil)) then 
      call repanel_and_normalize (foil, tmp_foil)
      foil = tmp_foil 
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
    xBlend = min( max( xBlend , 0.1d0 ) , 1.0d0 )

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

    type (side_airfoil_type), intent(in)  :: thickness 
    double precision, intent(in)          :: factor, xBlend 

    double precision, allocatable   :: new_y (:) 
    double precision                :: blend, fac, arg, srfac, tfac
    integer                         :: i, np

    blend = min (max (xBlend , 0.001d0) ,  1d0)
    fac   = min (max (factor ,  0.01d0) , 10d0)

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
                          " iterations (x="//strf('(F6.4)',x)//')')
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
      y(i) = eval_y_on_x_at_side (foil, 'Bot', xnew(i))
    end do 

  end function 


  function eval_deviation_at_side (foil, side, target_x, target_y) result (devi_norm2)

    !-----------------------------------------------------------------------------
    !! returns  norm2 deviation of target points to foil side 
    !-----------------------------------------------------------------------------

    type (airfoil_type), intent(in)           :: foil 
    character(3), intent(in)                  :: side 
    double precision, intent(in)              :: target_x(:), target_y (:)
    double precision              :: devi_norm2, y
    double precision, allocatable :: devi(:)
    integer                       :: i, nTarg

    nTarg = size(target_x)
    allocate (devi(nTarg))

    do i = 1, nTarg 
      y = eval_y_on_x_at_side (foil, side, target_x(i))
      devi(i) = abs (y - target_y(i))
    end do 
    
    devi_norm2 = norm2 (devi)

  end function 



  function eval_y_on_x_at_side (foil, side, xn) result (y)

    !-----------------------------------------------------------------------------
    !! returns y-value at x of side 'Top' or 'Bot' evaluated 
    !! either with bezier or spline 
    !-----------------------------------------------------------------------------

    use shape_bezier,       only : bezier_eval_y_on_x
    type (airfoil_type), intent(in)           :: foil 
    character(3), intent(in)                  :: side 
    double precision, intent(in)              :: xn
    double precision                          :: y 

    if (foil%is_bezier_based) then 
      if (side == 'Top') then 
        y = bezier_eval_y_on_x (foil%top_bezier, xn)
      else
        y = bezier_eval_y_on_x (foil%bot_bezier, xn)
      end if
    else 
      y = eval_y_on_x_at_side_spline (foil, side, xn)
    end if 

  end function 


  function eval_y_on_x_at_side_spline (foil, side, xn) result (y)

    !-----------------------------------------------------------------------------
    !! returns y-value at x of side 'Top' or 'Bot' evaluated spline 
    !-----------------------------------------------------------------------------

    use spline,         only : eval_1D
    use airfoil_base,   only : is_normalized_coord

    type (airfoil_type), intent(in)           :: foil 
    character(3), intent(in)                  :: side 
    double precision, intent(in)              :: xn
    double precision      :: y 
    double precision      :: s, s_start, s_end, x, dx, delta, s_sav
    integer               :: i

    ! sanity 
    
    if (.not. is_normalized_coord (foil)) &
      call my_stop ( "eval_y_on_x: airfoil not normalized")

    if (xn < 0d0 .or. xn > 1d0) &
      call my_stop ( "eval_y_on_x: x value not in range from 0 to 1")

    if (.not. allocated(foil%spl%s)) &
      call my_stop ( "eval_y_on_x: spline isn't allocated up to now")

    y = 0d0 
    ! get s start and end 

    if (side == 'Top') then
      s_start = foil%spl%s (1) 
      s_end   = foil%spl%s (minloc(foil%x,1))
    else
      s_start = foil%spl%s (minloc(foil%x,1)) 
      s_end   = foil%spl%s (size(foil%spl%s))
    end if  

    if (xn == 0d0) then                          ! avoid numerical issues at 0 and 1    
      y = 0d0
    else if (xn == 1d0) then 
      if (side == 'Top') then
        y = foil%top%y(size(foil%top%y))
      else
        y = foil%bot%y(size(foil%bot%y))
      end if        
    else

      ! define a approx. start value for newton iteration 

      if (side == 'Bot') then
        if (xn < 0.05) then                      
          s = s_start + 0.05d0                       ! little dist from start
        else if (xn > 0.95) then
          s = s_end - 0.05d0                         ! little dist from end
        else 
          s = s_start + xn                           ! approx x = s 
        end if  
      else
        if (xn < 0.05) then                      
          s = s_end - 0.05d0                         ! little dist from end
        else if (xn > 0.95) then
          s = s_start + 0.05d0                       ! little dist from start
        else 
          s = s_end - xn                           ! approx x = s 
        end if  
      end if 

      ! newton iteration to get spline arc s value from x

      do i = 1, 50
  
        if (s > s_end)   s = s_end                  ! ensure to stay within boundaries 
        ! if (s < s_start) s = s_start + 1d-10  

        x  = eval_1D (foil%spl%splx, s, 0)          ! eval spline to get actuual x
        delta = x-xn
        if (abs(delta) < EPSILON) exit              ! succeeded

        dx = eval_1D (foil%spl%splx, s, 1)          ! eval first derivative for Newton 
      
        if (dx == 0d0 .and. (x /= 0d0)) & 
          call my_stop ( "eval_y_on_x: zero derivative in Newton iteration")
  
        s_sav = s 
        s = s - delta / dx                              ! Newton delta 

      end do 

      if (abs(delta) >= EPSILON) then 
        !$omp critical 
        print *, xn, x, dx        
        call print_warning ("eval_y_on_x "//side//": Newton failed after "//stri(i)// &
                            " iterations (x="//strf('(F6.4)',xn)//')')
        !$omp end critical
      end if 
      ! finally get y from iterated s-value 

      y = eval_1D (foil%spl%sply, s, 0) 

    end if 

  end function 




  function move_xmax_of_line (line, cur_xmax, new_xmax) result (new_y)

    !-----------------------------------------------------------------------------
    !! moves the maximum (highpoint) of line from cur_xmax to new_xmax
    !!  returns new y for line
    !  (quite similar to xfoils implmentation HIPNT)
    !-----------------------------------------------------------------------------

    use spline,       only : spline_1D, spline_1D_type, eval_1D, spline_2D, eval_spline, NATURAL
    use math_util,     only : linspace

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

    new_max = max (0.1d0, new_xmax)
    new_max = min (0.9d0, new_max)

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

    type (airfoil_type), intent(in)           :: foil1
    type (airfoil_type), intent(in), optional :: foil2, foil3
    integer, intent(in), optional             :: indent
    
    integer                           :: nfoils, ile, i, ind, np
    type (airfoil_type)               :: foils (3) 
    character (20)                    :: name
    double precision                  :: xle_s, yle_s

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
    call print_fixed     ("top yLE" ,11, .true.)   
    call print_fixed     ("bot xLE" ,11, .true.)   
    call print_fixed     ("bot yLE" ,11, .true.)   
    print *

    ! print data 
    do i = 1, nfoils 

      np  = size (foils(i)%x)
      ile = minloc (foils(i)%x,1)
      name = foils(i)%name 
      call le_find (foils(i), xle_s, yle_s)

      if (abs(xle_s) < 0.0000001d0) xle_s = 0d0
      if (abs(yle_s) < 0.0000001d0) yle_s = 0d0

      call print_fixed     (""        ,ind, .false.)   
      call print_fixed     (foils(i)%name, 15, .false.)   
      call print_colored_i (5, Q_NO, np)
      call print_colored_i (5, Q_NO, ile) 

      call print_colored_r (13, '(F10.7)', Q_NO, foils(i)%x(ile))
      call print_colored_r (11, '(F10.7)', Q_NO, foils(i)%y(ile))
      call print_colored_r (11, '(F10.7)', Q_NO, xle_s)
      call print_colored_r (11, '(F10.7)', Q_NO, yle_s)

      call print_colored_r (13, '(F10.7)', Q_NO, foils(i)%x(1))
      call print_colored_r (11, '(F10.7)', Q_NO, foils(i)%y(1))
      call print_colored_r (11, '(F10.7)', Q_NO, foils(i)%x(np))
      call print_colored_r (11, '(F10.7)', Q_NO, foils(i)%y(np))
      print * 

    end do 

  end subroutine




end module
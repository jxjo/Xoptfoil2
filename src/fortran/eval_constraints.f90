! MIT License
! Copyright (c) 2024 Jochen Guenzel 

module eval_constraints

  !-------------------------------------------------------------------------
  ! evaluations of airfoil geometry constraints
  !-------------------------------------------------------------------------
   
  use os_util
  use airfoil_base,           only: airfoil_type, side_airfoil_type
  use eval_commons

  implicit none
  private 

  public :: geo_constraints_type
  public :: curv_constraints_type
  public :: curv_side_constraints_type

  public :: eval_geometry_violations
  public :: eval_curvature_violations
  public :: eval_side_curvature_violations
  
  public :: assess_surface
  public :: violation_stats_print, violation_stats_reset
  public :: max_panels_angle, max_curvature_at_te


  ! return codes of eval_geometry_violations

  integer, parameter, public  :: VIOL_LE_BLUNT        = 1
  integer, parameter, public  :: VIOL_LE_SHARP        = 2
  integer, parameter, public  :: VIOL_MIN_TE_ANGLE    = 3
  integer, parameter, public  :: VIOL_MAX_PANEL_ANGLE = 4

  integer, parameter, public  :: VIOL_MAX_CAMBER      = 5
  integer, parameter, public  :: VIOL_MIN_CAMBER      = 6
  integer, parameter, public  :: VIOL_MAX_THICKNESS   = 7
  integer, parameter, public  :: VIOL_MIN_THICKNESS   = 8

  integer, parameter, public  :: VIOL_MAX_REVERSALS   = 9
  integer, parameter, public  :: VIOL_MAX_SPIKES      = 10 
  integer, parameter, public  :: VIOL_TE_CURVATURE    = 11 

  integer, parameter, public  :: VIOL_MAX_LE_DIFF     = 12

  integer, parameter, public  :: MAX_VIOLATION_ID     = 12    ! ! update for new IDs 

  
  ! --- public, types ---------------------------------------------------

  
  ! --- private, static ---------------------------------------------------

  integer, allocatable        :: violation_stats (:)      ! statistics of number of violations 
  character (25), allocatable :: violation_short_text (:) ! info text on violation type 

  contains


  !--------------------------------------------------------------------------------------


  subroutine eval_geometry_violations (foil, geometry_constraints, has_violation, info)

    !-----------------------------------------------------------------------------
    !! check foil against 'geometry_constraints'
    !! return first violation with 'has_violation' and info text  
    !-----------------------------------------------------------------------------

    use airfoil_geometry,           only : eval_thickness_camber_lines, get_geometry, te_gap

    type (airfoil_type), intent(in)         :: foil
    type (geo_constraints_type), intent(in) :: geometry_constraints
    logical, intent(out)                    :: has_violation
    character (100), intent(out)            :: info

    type (side_airfoil_type)                :: thickness, camber 
    type (geo_constraints_type)             :: c
    double precision :: min_angle
    double precision :: maxt, xmaxt, maxc, xmaxc

    has_violation = .true.
    info = ""
    c = geometry_constraints

    ! too blunt leading edge

    if (max_le_panel_angle (foil) > 89.99d0) then 
      call add_to_stats (VIOL_LE_BLUNT)
      info = "Panel angle "//strf('(F6.2)', max_le_panel_angle (foil) )// &
                " at leading edge is too blunt."
      return 
    end if 

    ! too sharp leading edge 

    if (le_panels_angle (foil) > 20d0) then 
      call add_to_stats (VIOL_LE_SHARP)
      info = "Panel angle "//strf('(F6.2)', le_panels_angle(foil) )// &
                " at leading edge is too sharp."
      ! write (*,*) info
      return 
    end if 

    ! Too small te thickness angle 

    call eval_thickness_camber_lines (foil, thickness, camber)

    min_angle = min_te_angle (thickness)
    if (min_angle < c%min_te_angle) then 
      call add_to_stats (VIOL_MIN_TE_ANGLE)
      info = "Min TE angle = "//strf('(F4.1)', min_angle)//" is smaller than min_te_angle = "//&
             strf('(F4.1)', c%min_te_angle) 
      return 
    end if 


    ! Too small panel angle
    !     Due to numerical issues (?) it happens, that the final maxpanang ist greater 30.

    if (max_panels_angle(foil) > 30d0) then
      call add_to_stats (VIOL_MAX_PANEL_ANGLE)
      info = "Panel angles ("//strf('(F6.2)', max_panels_angle(foil))//") are too large."
      return 
    end if


    ! thickness, camber 

    if (c%max_camber    /= NOT_DEF_D .or. c%min_camber    /= NOT_DEF_D .or. & 
        c%max_thickness /= NOT_DEF_D .or. c%min_thickness /= NOT_DEF_D) then 

      call get_geometry (foil, maxt, xmaxt, maxc, xmaxc)


      if (c%max_camber /= NOT_DEF_D .and. maxc > c%max_camber) then 
        call add_to_stats (VIOL_MAX_CAMBER)
        info = "Camber is higher than max value: "//strf('(F6.4)', c%max_camber)
        return 
      end if 
      if (c%min_camber /= NOT_DEF_D .and. maxc < c%min_camber) then 
        call add_to_stats (VIOL_MIN_CAMBER)
        info = "Camber is less than min value: "//strf('(F6.4)', c%min_camber)
        return 
      end if

      if (c%max_thickness /= NOT_DEF_D .and. maxt > c%max_thickness) then 
        call add_to_stats (VIOL_MAX_THICKNESS)
        info = "Thickness is more than max value: "//strf('(F6.4)', c%max_thickness)
        return 
      end if 
      if (c%min_thickness /= NOT_DEF_D .and. maxc < c%min_thickness) then 
        call add_to_stats (VIOL_MIN_THICKNESS)
        info = "Thickness is less than min value: "//strf('(F6.4)', c%min_thickness)
        return 
      end if

    end if 

    has_violation = .false. 

  end subroutine



  subroutine eval_curvature_violations (foil, curv_constraints, has_violation, info)

    !! check foil against curvature constraints 
    !! return at first violation with 'has_violation' and info text  

    use shape_bezier,      only : bezier_curvature

    type(airfoil_type), intent(in)           :: foil
    type(curv_constraints_type), intent(in)  :: curv_constraints
    logical, intent(out)                     :: has_violation
    character (100), intent(out)             :: info
    double precision      :: top_curv_le, bot_curv_le, le_diff, max_diff

    has_violation = .false.
    info = ""

    ! check top and bot separately 

    call eval_side_curvature_violations (foil%top, curv_constraints%top, has_violation, info)
    if (has_violation) return
 
    if (.not. foil%symmetrical) then 

      call eval_side_curvature_violations (foil%bot, curv_constraints%bot, has_violation, info)
      if (has_violation) return

    end if 

    ! Bezier - difference of curvature at LE is to big 

    if (foil%is_bezier_based) then 

      top_curv_le = bezier_curvature(foil%top_bezier, 0d0)
      bot_curv_le = bezier_curvature(foil%bot_bezier, 0d0)

      le_diff   = abs (top_curv_le - bot_curv_le)
      max_diff  = curv_constraints%max_le_curvature_diff

      if (le_diff > max_diff) then 
        call add_to_stats (VIOL_MAX_LE_DIFF)
        info = "Bezier: Curvature difference at LE ("//strf('(F5.1)',le_diff)//&
               ") exceeds 'max_le_curvature_diff' ("//strf('(F5.1)',max_diff)//")"
        has_violation = .true.
        return 
      end if 

    end if 


  end subroutine




  subroutine eval_side_curvature_violations (side, side_curv_constraints, has_violation, info)

    !! check side of foil against curvature constraints
    !! return at first violation with 'has_violation' and info text  

    use math_util,      only : count_reversals, derivative1

    type(side_airfoil_type), intent(in)           :: side
    type(curv_side_constraints_type), intent(in)  :: side_curv_constraints
    logical, intent(out)                          :: has_violation
    character (100), intent(out)                  :: info

    type(curv_side_constraints_type)  :: c
    integer             :: is, ie, nreverse, nspikes
    double precision    :: max_curv

    has_violation = .true.
    info = ""

    ! Curvature reversals  

    c = side_curv_constraints

    is = c%nskip_LE
    ie = size(side%x)
    nreverse = count_reversals (is, ie, side%curvature, c%curv_threshold)  
    ! write (*,*) is, ie, nreverse, c%max_curv_reverse

    if (nreverse > c%max_curv_reverse) then 
      call add_to_stats (VIOL_MAX_REVERSALS)
      info = side%name//": Number of reversals ("//stri(nreverse)//")exceeds max reversals"
      return 
    end if 


    ! spikes = Reversals of derivative of curvature = Bumps of curvature

    if (c%check_curvature_bumps) then 

      nspikes = count_reversals (is, ie, derivative1 (side%x, side%curvature), c%spike_threshold)
      if (nspikes > c%max_spikes) then 
        call add_to_stats (VIOL_MAX_SPIKES)
        info = side%name//": Number of spikes exceeds max spikes"
        return 
      end if 

    end if 

    ! TE curvature? 
    !    In the current Hicks Henne shape functions implementation, the last panel is
    !    forced to become TE which can lead to a thick TE area with steep last panel(s)
    !       (see create_shape ... do j = 2, npt-1 ...)
    !    so the curvature (2nd derivative) at the last 10 panels is checked


    max_curv = max_curvature_at_te (side%curvature)
    if (max_curv  > c%max_te_curvature) then 
      call add_to_stats (VIOL_TE_CURVATURE)
      info = side%name//": Curvature at TE exceeds max val: "//strf('(f5.1)', c%max_te_curvature)
      return 
    end if 

    has_violation = .false.

  end subroutine



  subroutine assess_surface (side, show_details, &
                            is, ie, iend_spikes, &
                            curv_threshold, spike_threshold, overall_quality)

    !! Assess polyline (x,y) on surface quality (curvature and derivative of curvature)
    !! - will return surface quality e.g. Q_GOOD
    !! - print an info string like this '-----R---H--sss--' (show_details)

    use math_util,        only : find_reversals, derivative1

    type (side_airfoil_type), intent(in)  :: side 
    logical, intent(in)                   :: show_details
    integer, intent(in)                   :: is, ie, iend_spikes
    double precision, intent(in)          :: curv_threshold, spike_threshold
    integer, intent(out)                  :: overall_quality

    integer             :: nspikes, nreversals, npt
    double precision    :: cur_te_curvature
    integer             :: quality_spikes, quality_reversals
    integer             :: quality_te
    character (size(side%x))     :: result_info
    character (100)           :: result_out
    character(:), allocatable :: info

    info = side%name // " side"

    nreversals = 0
    nspikes    = 0
    npt        = size(side%x)

    result_info           = repeat ('-', npt ) 

    ! have a look at derivative of curvature ... 

    call find_reversals (is, iend_spikes, spike_threshold, derivative1 (side%x, side%curvature), &
                        's', nspikes, result_info)


    ! have a look at curvature 

    call find_reversals(is, ie, curv_threshold, side%curvature, 'R', &
                        nreversals, result_info)
            
    quality_spikes    = i_quality (nspikes, 2, 6, 40)
    quality_reversals = i_quality (nreversals, 2, 3, 10)
    overall_quality   = ior(quality_spikes, quality_reversals)

    ! check te curvature 
    
    cur_te_curvature = max_curvature_at_te (side%curvature )

    ! in case of a reversal, allow a little more curvature e.g. reflexed airfoil
    if (nreversals == 1) then 
      quality_te      = r_quality (cur_te_curvature, 0.4d0, 2d0, 10d0)
    else
      quality_te      = r_quality (cur_te_curvature, 0.2d0, 1d0, 10d0)
    end if 
    ! te quality counts only half as too often it is bad ... 
    overall_quality = ior(overall_quality, (quality_te / 2))

  
    ! all the output ...

    if(show_details) then

      if (len(result_info) > len(result_out)) then
        result_out = '... ' // result_info ((len(result_info) - len(result_out) + 1 + 4):)
      else
        result_out = result_info
      end if 

      call print_colored (COLOR_NOTE, repeat(' ',5) //info//' '//result_out)
      print *

      call print_colored (COLOR_NOTE, repeat(' ',18) // 'Spikes')
      call print_colored_i (3, quality_spikes, nspikes)
      call print_colored (COLOR_NOTE, '     Reversals')
      call print_colored_i (3, quality_reversals, nreversals)
      call print_colored (COLOR_NOTE, '     Curvature at TE')
      call print_colored_r (5,'(F5.2)', quality_te, cur_te_curvature) 
      if (quality_te > Q_BAD) then
        call print_colored (COLOR_NOTE, '  (geometric spoiler at TE?)')
      end if 
      print *
    end if
                              
  end subroutine assess_surface
 


  !--------------------------------------------------------------------------------------



  function max_curvature_at_te (curvature) 

    !! get max. curvature at the end of polyline (= TE)

    double precision   :: max_curvature_at_te 
    double precision, intent(in) :: curvature (:)
    integer            :: npt, ite
    
    npt    = size(curvature)
    ite    = npt - 3                !  "te" begins at index
    max_curvature_at_te = maxval (abs(curvature(ite: npt)))

  end function max_curvature_at_te



  function max_le_panel_angle (foil) 

    !! max. panel angle at leading edge 

    double precision   :: max_le_panel_angle, panang1, panang2
    type (airfoil_type), intent(in) :: foil

    panang1 = atan((foil%top%y(2)-foil%top%y(1))/(foil%top%x(2)-foil%top%x(1))) *                &
              180.d0/acos(-1.d0)
    panang2 = atan((foil%bot%y(1)-foil%bot%y(2))/(foil%bot%x(2)-foil%bot%x(1))) *                &
              180.d0/acos(-1.d0)

    max_le_panel_angle = max(panang2,panang1)

  end function  



  function le_panels_angle (foil) 

    !!angle between the two panels at le  

    double precision   :: le_panels_angle, panang1, panang2
    type (airfoil_type), intent(in) :: foil

    panang1 = atan((foil%top%y(2)-foil%top%y(1))/(foil%top%x(2)-foil%top%x(1))) *                &
              180.d0/acos(-1.d0)
    panang2 = atan((foil%bot%y(1)-foil%bot%y(2))/(foil%bot%x(2)-foil%bot%x(1))) *                &
              180.d0/acos(-1.d0)

    le_panels_angle = abs (panang2 - panang1)

  end function  



  function max_panels_angle (foil) result (max_angle) 

    !! return the maximmum angle between two panels - typical between 0° - 20° 

    type (airfoil_type), intent(in) :: foil
    double precision    :: max_angle
    double precision    :: dx1, dx2, dy1, dy2, angle, crossp
    integer             :: i 

    max_angle = 0d0
    do i = 2, size(foil%x) - 1 
      
      dx1 = foil%x(i)-foil%x(i-1)
      dy1 = foil%y(i)-foil%y(i-1)
      dx2 = foil%x(i)-foil%x(i+1)
      dy2 = foil%y(i)-foil%y(i+1)

      if (dx1 /= 0d0 .and. dx2 /= 0d0) then 
        crossp = (dx2*dy1 - dy2*dx1)  / SQRT((dx1**2 + dy1**2) * (dx2**2 + dy2**2))
        angle = abs (ASIN(crossp)*(180d0/3.1415926d0))
        max_angle = max (max_angle, angle) 
      ! write (*,*) i, foil%x(i), foil%y(i), angle , max_angle
      end if 

    end do 

  end function  


  function min_te_angle (thickness) 

    !-----------------------------------------------------------------------------
    !! minimum thickness angle looking from te 
    !! (thickness line as input to allow avoiding several thickness evaluations)
    !-----------------------------------------------------------------------------

    type (side_airfoil_type), intent(in) :: thickness
    double precision   :: min_te_angle, min_te_angle_x
    double precision   :: max_slope, min_slope, min_slope_x, slope, te_gap
    double precision   :: pi, dx, dy
    integer            :: i, np, imax_slope

    pi = acos(-1.d0)
    np = size(thickness%x)
    te_gap = thickness%y(np)

    ! first find point of max. angle (slope for speed) - looking from te 

    max_slope = 0d0

    do i = 2, np-1
      dx = thickness%x(np) - thickness%x(i)
      dy = thickness%y(i) - te_gap 
      slope = dy / dx 
      if (slope > max_slope) then 
        max_slope = slope 
      else 
        exit
      end if 
    end do 

   ! then find minimum between max point and te (slope for speed) - looking from te 

    imax_slope = i-1
    min_slope = 1d99

    do i = imax_slope, np-1
      dx = thickness%x(np) - thickness%x(i)
      dy = thickness%y(i) - te_gap 
      slope = dy / dx 
      if (slope < min_slope) then 
        min_slope   = slope 
        min_slope_x = thickness%x(i) 
      end if 
    end do 

    min_te_angle   = atan (slope) * 180d0 / pi 
    min_te_angle_x = min_slope_x

    ! print '(A,F5.2,A,F5.2)', "Min angle: ", min_te_angle, "  at: ", min_te_angle_x

  end function  




  !--  private  ------------------------------------------------------------------------------

  subroutine add_to_stats (violation_id) 

    !! update violation statistics with new violation 

    integer, intent(in)     :: violation_id

    !$omp critical

    if (.not. allocated (violation_stats)) then 
      allocate (violation_stats(MAX_VIOLATION_ID))
      violation_stats = 0 

      ! initialize short text array 
      allocate (violation_short_text(MAX_VIOLATION_ID))
      violation_short_text = "" 
      violation_short_text (VIOL_LE_BLUNT)        = "LE blunt"
      violation_short_text (VIOL_LE_SHARP)        = "LE sharp"
      violation_short_text (VIOL_MIN_TE_ANGLE)    = "min_te_angle"
      violation_short_text (VIOL_MAX_PANEL_ANGLE) = "Panel angle"

      violation_short_text (VIOL_MAX_CAMBER)      = "max_camber"
      violation_short_text (VIOL_MIN_CAMBER)      = "min_camber"
      violation_short_text (VIOL_MAX_THICKNESS)   = "max_thickness"
      violation_short_text (VIOL_MIN_THICKNESS)   = "min_thickness"

      violation_short_text (VIOL_MAX_REVERSALS)   = "max_curv_reverse"
      violation_short_text (VIOL_MAX_SPIKES)      = "max_spikes"
      violation_short_text (VIOL_TE_CURVATURE)    = "max_te_curvature"

      violation_short_text (VIOL_MAX_LE_DIFF)     = "max_le_curvature_diff"

    end if 

    if (violation_id <= MAX_VIOLATION_ID) then
      ! write (*,*) "viol id", violation_id
      violation_stats(violation_id) = violation_stats(violation_id) + 1 
    end if 

  !$omp end critical

  end subroutine 


  subroutine violation_stats_print (intent)
   
    !! print current geometry violation sstatistics

    integer, intent(in), optional   :: intent
    integer      :: i, viol_leader
    logical      :: first

    if (.not. allocated (violation_stats)) return 

    if (present (intent)) then 
      i = intent
    else
      i = 10 
    end if 

    call print_colored (COLOR_PALE, repeat(' ',i)//"Geometry violations: ") 
    
    viol_leader = maxloc (violation_stats,1)
    first = .true. 
    
    do i = 1, size(violation_stats)

      if (violation_stats(i) > 0 ) then 

        if (first) then                             ! just for the ','
          first = .false. 
        else 
          call print_colored (COLOR_PALE, ", ")
        end if   
        
        call print_colored (COLOR_NOTE, stri(violation_stats(i))//" ")
        if (i == viol_leader) then
          call print_colored (COLOR_NOTE, trim(violation_short_text(i)))
        else 
          call print_colored (COLOR_NOTE, trim(violation_short_text(i)))
        end if 
      end if 

    end do 
    print *

  end subroutine 


  subroutine violation_stats_reset ()
   
    !! reset current geometry violation statistics

    if (.not. allocated (violation_stats)) return 

    !$omp critical 
    violation_stats = 0d0
    !$omp end critical 

  end subroutine 


end module 
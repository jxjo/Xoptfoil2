! MIT License
! Copyright (c) 2024 Jochen Guenzel 

module eval_constraints

  !-------------------------------------------------------------------------
  ! evaluations of airfoil geometry constraints
  !-------------------------------------------------------------------------
   
  use os_util
  use commons,     only: airfoil_type, side_airfoil_type
  use commons,     only: NOT_DEF_D
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
  character (20), allocatable :: violation_short_text (:) ! info text on violation type 

  contains


  !--------------------------------------------------------------------------------------


  subroutine eval_geometry_violations (foil, geometry_constraints, has_violation, info)

    !! check foil against 'geometry_constraints'
    !! return at first violation with 'has_violation' and info text  

    use xfoil_driver,         only : xfoil_set_airfoil, xfoil_get_geometry_info

    type(airfoil_type), intent(in)          :: foil
    type(geo_constraints_type), intent(in)  :: geometry_constraints
    logical, intent(out)                    :: has_violation
    character (:), allocatable, intent(out) :: info

    type(geo_constraints_type)              :: c
    double precision, allocatable           :: x_thick(:), thick(:)
    double precision :: tegap, heightfactor, gapallow
    double precision :: maxt, xmaxt, maxc, xmaxc
    integer          :: i, nptint

    has_violation = .false.

    if (.not. geometry_constraints%check_geometry) return       ! early exit 

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

    ! Interpolate thickness 

    call thickness_array (foil, x_thick, thick)

    nptint = size(x_thick) 
    tegap = foil%y(1) - foil%y(size(foil%y))
    heightfactor = tan (c%min_te_angle * acos(-1.d0)/180.d0/2.d0)


    ! Check if thinner than specified wedge angle on back half of airfoil

    do i = 2, nptint - 1

      if (x_thick(i) > 0.5d0) then
        gapallow = tegap + 2.d0 * heightfactor * (x_thick(nptint) - x_thick(i))
        if (thick(i) < gapallow) then 
          call add_to_stats (VIOL_MIN_TE_ANGLE)
          info = "Airfoil is thinner than min_te_angle at x = "//strf('(F6.2)', x_thick(i))
          return 
        end if 
      end if

    end do

    ! deactivated ... Additional thickness constraints
    ! if (naddthickconst > 0) then
    !   call interp_vector(x_thick, thickness, addthick_x(1:naddthickconst), add_thickvec)

    !   do i = 1, naddthickconst
    !     if (max(0.d0,add_thickvec(i)-addthick_max(i))/0.1d0 > 0.d0) penalty_info = trim(penalty_info) // ' addThickMax'
    !     if (max(0.d0,addthick_min(i)-add_thickvec(i))/0.1d0 > 0.d0) penalty_info = trim(penalty_info) // ' addThickMin'
    !   end do
    ! end if


    ! Too small panel angle
    !     Due to numerical issues (?) it happens, that the final maxpanang ist greater 30.

    if (max_panels_angle(foil) > 30d0) then
      call add_to_stats (VIOL_MAX_PANEL_ANGLE)
      info = "Panel angles ("//strf('(F6.2)', max_panels_angle(foil))//") are too large."
      return 
    end if


    ! next checks need xfoil geo routines...

    if (c%max_camber    /= NOT_DEF_D .or. c%min_camber    /= NOT_DEF_D .or. & 
        c%max_thickness /= NOT_DEF_D .or. c%min_thickness /= NOT_DEF_D) then 

      call xfoil_set_airfoil (foil)       
      call xfoil_get_geometry_info (maxt, xmaxt, maxc, xmaxc)


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
    character (:), allocatable, intent(out)  :: info
    double precision      :: top_curv_le, bot_curv_le, le_diff

    has_violation = .true.
    info = ""

    ! Bezier - difference of curvature at LE is to big 

    if (foil%is_bezier_based .and. curv_constraints%le_curvature_equal) then 

      top_curv_le = bezier_curvature(foil%top_bezier, 0d0)
      bot_curv_le = bezier_curvature(foil%bot_bezier, 0d0)

      le_diff = abs (top_curv_le - bot_curv_le)

      if (le_diff > curv_constraints%le_curvature_max_diff) then 
        call add_to_stats (VIOL_MAX_LE_DIFF)
        print *, le_diff
        info = "Bezier: Curvature difference at LE ("//strf('(F5.1)',le_diff)//") exceeds max_diff"
        return 
      end if 

    end if 

    has_violation = .false.

  end subroutine




  subroutine eval_side_curvature_violations (side, side_curv_constraints, has_violation, info)

    !! check side of foil against curvature constraints
    !! return at first violation with 'has_violation' and info text  

    use math_deps,      only : count_reversals, derivative1

    type(side_airfoil_type), intent(in)           :: side
    type(curv_side_constraints_type), intent(in)  :: side_curv_constraints
    logical, intent(out)                          :: has_violation
    character (:), allocatable, intent(out)       :: info

    type(curv_side_constraints_type)  :: c
    integer     :: is, ie, nreverse, nspikes

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

    if (max_curvature_at_te (side%curvature)  > c%max_te_curvature) then 
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

    use math_deps,        only : find_reversals, derivative1

    type (side_airfoil_type), intent(in)  :: side 
    logical, intent(in)                   :: show_details
    integer, intent(in)                   :: is, ie, iend_spikes
    double precision, intent(in)          :: curv_threshold, spike_threshold
    integer, intent(out)                  :: overall_quality

    integer             :: nspikes, nreversals, npt
    double precision    :: cur_te_curvature
    integer             :: quality_spikes, quality_reversals
    integer             :: quality_te
    character (size(side%x)) :: result_info
    character (90)      :: result_out
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



  subroutine thickness_array (foil, x_thick, thick)

    !! interpolates thickness at x-stations from top or bot side
    !! depending which side has more x-stations 

    use math_deps,      only : interp_vector

    type (airfoil_type), intent(in) :: foil
    double precision, allocatable,intent(out) :: x_thick (:), thick(:)

    integer   :: nptt, nptb
    double precision, allocatable   :: y_inter(:) 

    nptt = size(foil%top%x)
    nptb = size(foil%bot%x)
  
    if (nptt >= nptb) then 

      x_thick = foil%top%x
      y_inter = foil%top%x                          ! dummy for alloc 
      call interp_vector(foil%bot%x, foil%bot%y, x_thick, y_inter)

      thick = foil%top%y - y_inter

    else

      x_thick = foil%bot%x
      y_inter = foil%bot%x                          ! dummy for alloc 
      call interp_vector(foil%top%x, foil%top%y, x_thick, y_inter)

      thick =  y_inter - foil%bot%y 
    end if

  end subroutine 



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
      violation_short_text (VIOL_LE_BLUNT) = "LE blunt"
      violation_short_text (VIOL_LE_SHARP) = "LE sharp"
      violation_short_text (VIOL_MIN_TE_ANGLE) = "TE min angle"
      violation_short_text (VIOL_MAX_PANEL_ANGLE) = "Panel angle"

      violation_short_text (VIOL_MAX_CAMBER) = "max camber"
      violation_short_text (VIOL_MIN_CAMBER) = "min camber"
      violation_short_text (VIOL_MAX_THICKNESS) = "max thick"
      violation_short_text (VIOL_MIN_THICKNESS) = "min thick"

      violation_short_text (VIOL_MAX_REVERSALS) = "max reversals"
      violation_short_text (VIOL_MAX_SPIKES) = "max spikes"
      violation_short_text (VIOL_TE_CURVATURE) = "TE curvature"

      violation_short_text (VIOL_MAX_LE_DIFF) = "LE curv difference"

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
    integer      :: i

    if (.not. allocated (violation_stats)) return 

    if (present (intent)) then 
      i = intent
    else
      i = 10 
    end if 

    call print_colored (COLOR_PALE, repeat(' ',i)//"Geometry violations: ")    
    
    do i = 1, size(violation_stats)

      if (violation_stats(i) > 0 ) then 
        call print_colored (COLOR_PALE, stri(violation_stats(i))//" "//trim(violation_short_text(i)))
        if (i < size(violation_stats)) call print_colored (COLOR_PALE, ", ")
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
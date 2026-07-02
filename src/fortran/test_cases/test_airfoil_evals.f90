! MIT License

module test_airfoil_evals
  
  !-------------------------------------------------------------------------
  ! Airfoil evaluations with geometry, curvature constraints 
  !-------------------------------------------------------------------------

  use os_util
  use test_util 
  use airfoil_base,       only : airfoil_type, create_bezier_MH30
  use airfoil_base,       only : split_foil_into_sides, build_from_sides
  use shape_bezier,       only : bezier_spec_type
  use eval_commons,       only : geo_constraints_type
  use eval_constraints,   only : penalty_bumpiness

  implicit none

  contains

  subroutine test_geo_constraints  ()

    !! test of geometry constraints 

    use airfoil_geometry, only : te_angle, te_angle_top, te_angle_bot, eval_y_on_x, get_geometry
    use eval_constraints

    type(airfoil_type)              :: airfoil
    type (geo_constraints_type)     :: geo 
    double precision                :: penalty, x_pos, maxt, xmaxt, maxc, xmaxc

    call test_header ("Airfoil constraints")

    airfoil = create_bezier_MH30 (201)

    ! constraints 

    geo%check_geometry = .true.
    geo%min_thickness = NOT_DEF_D
    geo%max_thickness = NOT_DEF_D
    geo%min_te_angle  = NOT_DEF_D
    geo%min_te_top_angle = NOT_DEF_D
    geo%max_te_bot_angle = NOT_DEF_D
    geo%min_camber    = NOT_DEF_D
    geo%max_camber    = NOT_DEF_D   
    geo%min_thickness_at_x%x = NOT_DEF_D
    geo%min_thickness_at_x%y = NOT_DEF_D

    call get_geometry (airfoil, maxt, xmaxt, maxc, xmaxc)

    ! test minimum thickness

    geo%min_thickness = maxt * 1.1d0

    call penalty_stats_init ()

    penalty = penalty_geo (airfoil, geo)

    call assert (has_penalty(PEN_MIN_THICKNESS), "Detect minimum thickness")

    call assertf (penalty, penalty_min_thickness (maxt, geo%min_thickness, scale=PEN_GEO_SCALE), &
                  "Geometry penalty uses PEN_GEO_SCALE for thickness", 6)

    ! test maximum thickness

    geo%min_thickness = NOT_DEF_D
    geo%max_thickness = maxt * 0.9d0

    call penalty_stats_init ()

    penalty = penalty_geo (airfoil, geo)

    call assert (has_penalty(PEN_MAX_THICKNESS), "Detect maximum thickness")

    penalty = penalty_max_thickness (1.001d0, 1.0d0, scale=PEN_GEO_SCALE)
    call assertf (penalty, 0.01d0, "Small max thickness raw penalty", 6)

    penalty = penalty_max_thickness (1.05d0, 1.0d0, scale=PEN_GEO_SCALE)
    call assertf (penalty, 0.5d0, "Large max thickness raw penalty", 6)

    ! test minimum camber

    geo%max_thickness = NOT_DEF_D
    geo%min_camber = maxc * 1.1d0

    call penalty_stats_init ()

    penalty = penalty_geo (airfoil, geo)

    call assert (has_penalty(PEN_MIN_CAMBER), "Detect minimum camber")

    call assertf (penalty, penalty_min_camber (maxc, geo%min_camber, scale=PEN_GEO_SCALE), &
                  "Geometry penalty uses PEN_GEO_SCALE for camber", 6)

    ! test maximum camber

    geo%min_camber = NOT_DEF_D
    geo%max_camber = maxc * 0.9d0

    call penalty_stats_init ()

    penalty = penalty_geo (airfoil, geo)

    call assert (has_penalty(PEN_MAX_CAMBER), "Detect maximum camber")

    ! test check_geometry

    ! set min TE angle above actual to trigger violation

    geo%max_camber = NOT_DEF_D
    geo%min_te_angle = te_angle (airfoil) + 5d0  

    call penalty_stats_init ()   

    penalty = penalty_geo (airfoil, geo)  ! get stats for geometry penalty

    call assert (has_penalty(PEN_MIN_TE_ANGLE), "Detect min TE angle")

    ! test minimum top TE tangent angle

    geo%min_te_angle = NOT_DEF_D
    geo%min_te_top_angle = te_angle_top (airfoil) + 3d0

    call penalty_stats_init ()

    penalty = penalty_geo (airfoil, geo)

    call assert (has_penalty(PEN_MIN_TE_TOP_ANGLE), "Detect min TE top tangent angle")

    ! test maximum bot TE tangent angle (positive = downward)

    geo%min_te_top_angle = NOT_DEF_D
    geo%max_te_bot_angle = te_angle_bot (airfoil) - 3d0

    call penalty_stats_init ()

    penalty = penalty_geo (airfoil, geo)

    call assert (has_penalty(PEN_MAX_TE_BOT_ANGLE), "Detect max TE bot tangent angle")

    ! test minimum thickness at x

    geo%max_te_bot_angle = NOT_DEF_D
    x_pos = 0.5d0
    geo%min_thickness_at_x%x = x_pos
    geo%min_thickness_at_x%y = eval_y_on_x (airfoil%top, x_pos) - eval_y_on_x (airfoil%bot, x_pos) + 0.01d0

    call penalty_stats_init ()

    penalty = penalty_geo (airfoil, geo)

    call assert (has_penalty(PEN_MIN_THICKNESS_AT_X), "Detect minimum thickness at x")

    penalty = penalty_min_thickness_at_x (airfoil, x_pos, &
                  eval_y_on_x (airfoil%top, x_pos) - eval_y_on_x (airfoil%bot, x_pos) + &
                  2.3d-12, scale=100d0)

    call assertf (penalty, 0d0, "Tiny thickness-at-x penalty rounds to zero", 6)

    ! test check_geometry

    geo%check_geometry = .false.
    penalty = penalty_geo (airfoil, geo)  ! get stats for geometry penalty

    call assertf (penalty, 0.0d0, "Check geometry switch off", 5)

  end subroutine



  subroutine test_penalty_curv_deriv ()

    !! test curvature bump penalty function (new sign-change approach)

    use eval_constraints, only: penalty_bumpiness

    type(airfoil_type)     :: airfoil
    double precision       :: penalty
    double precision       :: scale
    integer                :: max_reversals

    airfoil = create_bezier_MH30 (201)
    call split_foil_into_sides (airfoil)

    ! test parameters
    max_reversals = 0
    scale         = 0.01d0

    ! test with smooth curve (should have low penalty, few sign changes)
    penalty = penalty_bumpiness (airfoil%top%x, airfoil%top%curvature, max_reversals)
    if (penalty >= 0d0 .and. penalty < 0.1d0) then
      call print_action ("Top - low penalty - Ok")
      nok = nok + 1
    else
      nfails = nfails + 1
      call print_action ("Top - low penalty - Failed")
      call print_text ("      penalty: "//strf('(F10.5)', penalty))
    end if

    penalty = penalty_bumpiness (airfoil%bot%x, airfoil%bot%curvature, max_reversals)
    if (penalty >= 0d0 .and. penalty < 0.1d0) then
      call print_action ("Bot - low penalty - Ok")
      nok = nok + 1
    else
      nfails = nfails + 1
      call print_action ("Bot - low penalty - Failed")
      call print_text ("      penalty: "//strf('(F10.5)', penalty))
    end if

    ! test with one allowed reversal (should allow more sign changes in 2nd derivative)
    max_reversals = 1
    penalty = penalty_bumpiness (airfoil%top%x, airfoil%top%curvature, max_reversals)
    ! just check it's non-negative and finite
    if (penalty < 0d0 .or. penalty /= penalty) then
      call print_text ("Error: penalty is negative or NaN")
    end if

  end subroutine

end module 


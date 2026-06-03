! MIT License
! Copyright (c) 2025 Jochen Guenzel

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

    use airfoil_geometry, only : te_angle
    use eval_constraints, only : penalty_geo, penalty_stats_init, penalty_stats_print, has_penalty, PEN_MIN_TE_ANGLE

    type(airfoil_type)              :: airfoil
    type (geo_constraints_type)     :: geo 
    double precision                :: penalty

    call test_header ("Airfoil constraints")

    airfoil = create_bezier_MH30 (201)

    ! constraints 

    geo%check_geometry = .true.
    geo%min_thickness = NOT_DEF_D
    geo%max_thickness = NOT_DEF_D
    geo%min_te_angle  = NOT_DEF_D
    geo%min_camber    = NOT_DEF_D
    geo%max_camber    = NOT_DEF_D   

    ! test check_geometry

    ! set min TE angle above actual to trigger violation

    geo%min_te_angle = te_angle (airfoil) + 5d0  

    call penalty_stats_init ()   

    penalty = penalty_geo (airfoil, geo)  ! get stats for geometry penalty

    call assert (has_penalty(PEN_MIN_TE_ANGLE), "Detect min TE angle")

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


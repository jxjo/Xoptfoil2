! MIT License

module eval_setup

  ! One-time preparation of evaluation specifications before optimization starts.

  use commons,            only : show_details
  use os_util
  use print_util
  use string_util

  use airfoil_base,       only : airfoil_type, is_bezier_based, is_bspline_based
  use op_point
  use geo_target,         only : geo_target_type, geo_result_type, geo_target_eval_type
  use geo_target,         only : init_geo_target_seed_ref, geo_target_eval
  use eval_commons,       only : eval_spec_type
  use eval_constraints,   only : penalty_geo, penalty_curv
  use xfoil_driver,       only : xfoil_options_type, run_op_points

  implicit none
  private

  public :: adjust_weightings
  public :: eval_seed_scale_objectives

  double precision, parameter :: EPSILON = 1d-10

contains

  subroutine adjust_weightings (eval_spec)

    !-----------------------------------------------------------------------------
    !! normalize weighting of op points, geo targets and goal attainment to a sum of 1.0
    !-----------------------------------------------------------------------------

    type (eval_spec_type), intent(inout) :: eval_spec

    integer             :: i, noppoint, n_goal_targets
    double precision    :: sum_weightings, attainment_weighting_user_eff
    double precision    :: sum_goal_target_weightings_user

    double precision, parameter :: GOAL_TARGET_WEIGHTING_REF = 6d0
    double precision, parameter :: GOAL_ATTAINMENT_SCALE = 0.1d0

    noppoint = size(eval_spec%op_point_specs)
    eval_spec%goal_attainment%weighting = 0d0
    n_goal_targets = 0
    sum_goal_target_weightings_user = 0d0

    do i = 1, noppoint
      if (is_target(eval_spec%op_point_specs(i)) .and. eval_spec%op_point_specs(i)%weighting_user > 0d0) then
        n_goal_targets = n_goal_targets + 1
        sum_goal_target_weightings_user = sum_goal_target_weightings_user + &
                                          eval_spec%op_point_specs(i)%weighting_user
      end if
    end do

    do i = 1, size(eval_spec%geo_targets)
      n_goal_targets = n_goal_targets + 1
      sum_goal_target_weightings_user = sum_goal_target_weightings_user + &
                                          eval_spec%geo_targets(i)%weighting_user
    end do

    ! calculate an effective user weighting for the single goal-attainment term,
    ! scaled in proportion to the total weighting of all eligible goal targets.
    ! GOAL_ATTAINMENT_SCALE calibrates the overall contribution level, while
    ! GOAL_TARGET_WEIGHTING_REF keeps the relation to the other target weightings.

    if (n_goal_targets > 1) then
      attainment_weighting_user_eff = GOAL_ATTAINMENT_SCALE * &
                                      max(eval_spec%goal_attainment%weighting_user, 0d0) * &
                                      sum_goal_target_weightings_user / GOAL_TARGET_WEIGHTING_REF
    else
      attainment_weighting_user_eff = 0d0
    end if

    sum_weightings = sum(eval_spec%op_point_specs%weighting_user) + &
                     sum(eval_spec%geo_targets%weighting_user) + &
                     attainment_weighting_user_eff

    if (sum_weightings > 0d0) then
      eval_spec%op_point_specs%weighting           = eval_spec%op_point_specs%weighting_user / sum_weightings
      eval_spec%geo_targets%weighting              = eval_spec%geo_targets%weighting_user / sum_weightings
      eval_spec%goal_attainment%weighting          = attainment_weighting_user_eff / sum_weightings
      eval_spec%goal_attainment%weighting_user_eff = attainment_weighting_user_eff
    else
      eval_spec%op_point_specs%weighting           = 0d0
      eval_spec%geo_targets%weighting              = 0d0
      eval_spec%goal_attainment%weighting          = 0d0
      eval_spec%goal_attainment%weighting_user_eff = 0d0
    end if

    if (eval_spec%goal_attainment%weighting_user_eff > 0d0) then
      call print_note("Target attainment optimization enabled with user weighting "// &
                      strf('F4.1', eval_spec%goal_attainment%weighting_user), &
                      indent = 3)
    end if

  end subroutine adjust_weightings



  subroutine eval_seed_scale_objectives (seed_foil, eval_spec)

    !----------------------------------------------------------------------------------
    !! evaluates seed airfoil to scale objectives to achieve objective function = 1.0
    !----------------------------------------------------------------------------------

    use shape_airfoil,        only : shape_spec

    type (airfoil_type), intent(in)    :: seed_foil
    type (eval_spec_type), intent(inout) :: eval_spec

    type(op_point_spec_type)                  :: op_spec
    type(op_point_result_type)                :: op
    type(op_point_result_type), allocatable   :: op_point_results (:)
    type(geo_result_type)                     :: geo_result
    type(xfoil_options_type)                  :: local_xfoil_options
    double precision, allocatable             :: flap_angles (:)

    integer                         :: i
    character(:), allocatable       :: opt_type


    call print_action ('Evaluate seed airfoils objectives to scale objective function to 1.0')

    local_xfoil_options = eval_spec%xfoil_options
    local_xfoil_options%show_details = show_details

    flap_angles = eval_spec%op_point_specs(:)%flap_angle

    call run_op_points (seed_foil, local_xfoil_options, shape_spec%flap_spec, flap_angles, &
                        eval_spec%op_point_specs, op_point_results)

    geo_result = geo_objective_results_setup(seed_foil)

    do i = 1, size(eval_spec%geo_targets)
      call init_geo_target_seed_ref(eval_spec%geo_targets(i), geo_result)
    end do

    do i = 1, size(eval_spec%op_point_specs)

      op_spec  = eval_spec%op_point_specs(i)
      op       = op_point_results(i)
      opt_type = opt_type_name(op_spec%opt_type)

      if (.not. op%converged) then

        print *
        call print_note("If an operating of point of the seed airfoil isn't converged,")
        call print_text("it can lead to a non-representative objective function value.", 7)
        call print_text("Try to change the specification of the operating point a little.", 7)

        call my_stop("Xfoil did not converge for operating point: "//stri(i))

      end if

      if (op%cl <= 0.d0 .and. ((op_spec%opt_type == OPT_MIN_SINK) .or. &
          (op_spec%opt_type == OPT_MAX_GLIDE))) then
        call my_stop("Operating point "//stri(i)//" has Cl <= 0. "// &
                     "Cannot use "//opt_type//" optimization in this case.")
      end if

      call init_op_point_seed_ref(op_spec, op)
      eval_spec%op_point_specs(i) = op_spec

    end do

    eval_spec%goal_attainment%seed_value = goal_gap_raw_setup(eval_spec%op_point_specs, &
                                                              eval_spec%geo_targets, &
                                                              op_point_results, geo_result)
    eval_spec%curv_constraints%seed_penalty = penalty_curv(seed_foil, eval_spec%curv_constraints)
    eval_spec%geo_constraints%seed_penalty  = penalty_geo(seed_foil, eval_spec%geo_constraints)

  end subroutine eval_seed_scale_objectives



  function geo_objective_results_setup (foil) result (geo_result)

    !----------------------------------------------------------------------------
    !! eval geometry results for setup without relying on eval module state
    !----------------------------------------------------------------------------

    use math_util,            only : rms
    use airfoil_geometry,     only : get_geometry, deviation_of_side
    use shape_bezier,         only : bezier_curvature, bezier_le_curvature
    use shape_bspline,        only : bspline_curvature, bspline_le_curvature

    type(airfoil_type), intent(in)           :: foil
    type(geo_result_type)                    :: geo_result


    call get_geometry(foil, geo_result%maxt, geo_result%xmaxt, geo_result%maxc, geo_result%xmaxc)

    if (is_bezier_based(foil)) then
      geo_result%top_curv_le = bezier_le_curvature(foil%top%bezier)
      geo_result%top_curv_te = bezier_curvature(foil%top%bezier, 1d0)
      geo_result%bot_curv_le = bezier_le_curvature(foil%bot%bezier)
      geo_result%bot_curv_te = bezier_curvature(foil%bot%bezier, 1d0)
    else if (is_bspline_based(foil)) then
      geo_result%top_curv_le = bspline_le_curvature(foil%top%bspline)
      geo_result%top_curv_te = bspline_curvature(foil%top%bspline, 1d0)
      geo_result%bot_curv_le = bspline_le_curvature(foil%bot%bspline)
      geo_result%bot_curv_te = bspline_curvature(foil%bot%bspline, 1d0)
    else
      geo_result%top_curv_le = foil%top%curvature(1)
      geo_result%top_curv_te = foil%top%curvature(size(foil%top%curvature))
      geo_result%bot_curv_le = foil%bot%curvature(1)
      geo_result%bot_curv_te = foil%bot%curvature(size(foil%bot%curvature))
    end if

  end function geo_objective_results_setup



  function goal_gap_raw_setup (op_point_specs, geo_targets, op_point_results, geo_result) result (quality)

    !-----------------------------------------------------------------------------
    !! Raw quality metric for aero and geo targets during setup.
    !----------------------------------------------------------------------------- 

    type(op_point_spec_type), intent(in)    :: op_point_specs (:)
    type(geo_target_type), intent(in)       :: geo_targets (:)
    type(op_point_result_type), intent(in)  :: op_point_results (:)
    type(geo_result_type), intent(in)       :: geo_result

    double precision                  :: quality
    type(op_point_eval_type)          :: op_eval
    type(geo_target_eval_type)        :: geo_eval
    integer                           :: i
    double precision                  :: total_target_weight, weighted_gap_sq_sum
    double precision                  :: weight_user, worst_gap, rms_gap

    double precision, parameter       :: GOAL_GAP_WORST_ALPHA = 0.7d0

    quality = 0d0
    total_target_weight = 0d0
    weighted_gap_sq_sum = 0d0
    worst_gap = 0d0

    do i = 1, size(op_point_specs)

      if (.not. is_target(op_point_specs(i))) cycle

      weight_user = max(op_point_specs(i)%weighting_user, 0d0)
      if (weight_user <= EPSILON) cycle

      total_target_weight = total_target_weight + weight_user

      op_eval = op_point_eval(op_point_specs(i), op_point_results(i))
      weighted_gap_sq_sum = weighted_gap_sq_sum + weight_user * op_eval%target_deviation**2
      worst_gap = max(worst_gap, op_eval%target_deviation)
    end do

    do i = 1, size(geo_targets)

      weight_user = max(geo_targets(i)%weighting_user, 0d0)
      if (weight_user <= EPSILON) cycle

      geo_eval = geo_target_eval(geo_targets(i), geo_result)
      if (geo_eval%quality == Q_NO) cycle

      total_target_weight = total_target_weight + weight_user
      weighted_gap_sq_sum = weighted_gap_sq_sum + weight_user * geo_eval%target_deviation**2
      worst_gap = max(worst_gap, geo_eval%target_deviation)
    end do

    if (total_target_weight <= EPSILON) return

    rms_gap = sqrt(weighted_gap_sq_sum / total_target_weight)
    quality = rms_gap + GOAL_GAP_WORST_ALPHA * (worst_gap - rms_gap)

  end function goal_gap_raw_setup

end module eval_setup
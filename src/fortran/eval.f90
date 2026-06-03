! MIT License
! Copyright (C) 2017-2019 Daniel Prosser
! Copyright (c) 2022-2025 Jochen Guenzel

module eval

  ! Core module: Sets up and evaluates the objective function for an airfoil design

  use commons       
  use os_util
  use print_util
  use string_util,        only : str_duration

  use airfoil_base,       only : airfoil_type, panel_options_type
  use airfoil_base,       only : is_bezier_based, is_bspline_based
  use shape_airfoil,      only : shape_spec_type
  use shape_hicks_henne,  only : penalty_hh_x_order

  use xfoil_driver,       only : xfoil_options_type
  use xfoil_driver,       only : flap_spec_type
  use geo_target,         only : geo_target_type, geo_result_type, geo_target_eval_type
  use geo_target,         only : geo_target_objective, geo_target_eval
  use geo_target,         only : init_geo_target_seed_ref
  use op_point,           only : op_point_spec_type, re_type, op_point_result_type
  use op_point,           only : OPT_MIN_CD, OPT_MAX_CL, OPT_MAX_GLIDE, OPT_MIN_SINK, OPT_MAX_XTR
  use op_point,           only : OPT_TARGET_CD, OPT_TARGET_CL, OPT_TARGET_GLIDE, OPT_TARGET_CM
  use op_point,           only : opt_type_name

  use eval_commons
  use eval_out 
  use eval_constraints

  implicit none 
  private
 
  ! --- Public functions -----------------------------------

  public :: eval_design_is_valid                        ! to find valid inital designs
  public :: objective_function                          ! the one and only 

  public :: eval_seed_scale_objectives                  
  public :: write_progress
  public :: write_final_results
  public :: set_eval_spec

  double precision, parameter, public  :: OBJ_XFOIL_FAIL  = 5d0
  double precision, parameter, public  :: OBJ_DESIGN_FAIL = 4d0

  double precision, parameter, public  :: MAX_PENALTY_DESIGN_VALID = 0.0001d0
  double precision, parameter, public  :: MAX_PENALTY_DESIGN_FAIL  = 0.01d0


  ! ---- static, private ---------------------------------

  ! these evaluation specifications are loaded at the beginnning of optimization 
  ! and are static, private to this module to ensure encapsulation 

  type (op_point_spec_type),  allocatable :: op_point_specs (:)
  type (geo_target_type), allocatable     :: geo_targets (:) 

  type (geo_constraints_type)             :: geo_constraints 
  type (curv_constraints_type)            :: curv_constraints
  type (panel_options_type)               :: panel_options
  type (xfoil_options_type), private      :: xfoil_options
  type (match_foil_spec_type)             :: match_foil_spec

  ! Save the best result of evaluation for write_design 
  !    so no extra run_xfoil is needed

  double precision                        :: best_objective = 2d0   ! dummy higher than 1.0
  type(airfoil_type)                      :: best_foil
  type(op_point_result_type), allocatable :: best_op_point_results (:) 
  type(goal_attainment_type)              :: goal_attainment

  ! Numerical accuracy 

  double precision, parameter    :: EPSILON = 1.d-10          

contains


  function objective_function (dv)

    !-----------------------------------------------------------------------------
    !! The Objective Function 
    !-----------------------------------------------------------------------------

    use op_point,             only : op_point_result_type
    use shape_bezier,         only : write_bezier_file
    use airfoil_geometry,     only : max_curvature_at_te

    double precision, intent(in)    :: dv (:) 

    double precision                :: objective_function
    double precision                :: penalty, aero, geo, goal_attain
    type(airfoil_type)              :: foil
    double precision, allocatable   :: flap_angles (:) 
    ! character(:), allocatable       :: debug_file

    type(op_point_result_type), allocatable :: op_point_results (:)
    type(geo_result_type)           :: geo_result
    integer                         :: i
    objective_function = 0d0
    penalty            = 0d0
    aero               = 0d0
    geo                = 0d0
    goal_attain        = 0d0

    ! get new airfoil out of design variables 

    foil =  create_airfoil (dv)

    ! Geometry and Curvature constraints violations? 

    penalty = penalty + penalty_curv (foil, curv_constraints)

    penalty = penalty + penalty_geo  (foil, geo_constraints)

    if (penalty > MAX_PENALTY_DESIGN_FAIL) then 
      objective_function = OBJ_DESIGN_FAIL  
      return 
    end if 

    ! get actual flap angles and results

    flap_angles = get_flap_angles (dv)

    ! evaluate the foil with xfoil ...

    op_point_results = aero_objective_results (foil, flap_angles) 

    ! early exit if one op point fails - objective function wouldn't make sense 

    do i = 1, size(op_point_results)
      if (.not. op_point_results(i)%converged) then 
        objective_function = OBJ_XFOIL_FAIL
        return 
      end if
    end do

    ! calc aero objective function from xfoil results 

    aero = aero_objective_function (op_point_results)

    ! calc geometry objective function from  geo targets 

    geo_result = geo_objective_results (foil)      
    geo        = geo_objective_function (geo_result )

    ! calc goal-attainment objective over aero and geo targets

    goal_attain = goal_attainment_objective (op_point_results, geo_result)

    ! final objective_function

    objective_function = aero + geo + goal_attain + penalty

    ! Save the best result to be written later at write_design ...

    !$omp critical
    ! print *, "Objective function: ", strf('f8.6', objective_function), " (aero: ", strf('f8.6', aero),&
    !           " geo: ", strf('f8.6', geo), " penalty: ", strf('f8.6', penalty), ")"
    if (objective_function < best_objective) then
      best_objective        = objective_function 
      best_foil             = foil 
      best_op_point_results = op_point_results
    end if 
    !$omp end critical

  end function objective_function



  subroutine set_eval_spec (eval_spec)

    !-----------------------------------------------------------------------------
    !! sets eval specification into static structures for later evaluation  
    !-----------------------------------------------------------------------------

    type (eval_spec_type), intent(in)  :: eval_spec 

    op_point_specs          = eval_spec%op_point_specs
    geo_targets             = eval_spec%geo_targets

    geo_constraints         = eval_spec%geo_constraints
    curv_constraints        = eval_spec%curv_constraints

    xfoil_options           = eval_spec%xfoil_options
    panel_options           = eval_spec%panel_options

    match_foil_spec         = eval_spec%match_foil_spec
    
    goal_attainment         = eval_spec%goal_attainment
    goal_attainment%seed_value = 0d0

  
  end subroutine set_eval_spec
  


  subroutine eval_seed_scale_objectives (seed_foil)

    !----------------------------------------------------------------------------------
    !! evaluates seed airfoil to scale objectives to achieve objective function = 1.0
    !----------------------------------------------------------------------------------

    use eval_commons 
    use shape_airfoil,        only : shape_spec
    use xfoil_driver,         only : run_op_points, xfoil_defaults
    use op_point,             only : op_point_result_type, init_op_point_seed_ref

    type (airfoil_type), intent(in)           :: seed_foil

    type(op_point_spec_type)                  :: op_spec
    type(op_point_result_type)                :: op
    type(op_point_result_type), allocatable   :: op_point_results (:)
    type(geo_result_type)                     :: geo_result
    type(xfoil_options_type)                  :: local_xfoil_options
    double precision, allocatable             :: flap_angles (:) 

    integer          :: i
    character(:), allocatable  :: opt_type


    ! Analyze airfoil at requested operating conditions with Xfoil

    call print_action ('Evaluate seed airfoils objectives to scale objective function to 1.0')

    ! allow local xfoil options for seed airfoil 
    local_xfoil_options = xfoil_options
    local_xfoil_options%show_details = show_details   ! for seed we are single threaded, show deails posssible 

    ! set flaps of seed to predefined angle from input

    flap_angles = op_point_specs(:)%flap_angle 
 
    ! now run xfoil ...
   
    call run_op_points (seed_foil, local_xfoil_options, shape_spec%flap_spec, flap_angles, &
              op_point_specs, op_point_results)

    ! Evaluate seed values of geometry targets and initialize their references.

    geo_result = geo_objective_results (seed_foil)
      
    do i = 1, size(geo_targets)
      call init_geo_target_seed_ref(geo_targets(i), geo_result)

    end do 


    ! Evaluate objectives to establish scale factors for each point

    do i = 1, size(op_point_specs)

      op_spec  = op_point_specs(i)
      op       = op_point_results(i) 
      opt_type = opt_type_name(op_spec%opt_type)

      ! Check for unconverged points

      if (.not. op%converged) then

        print *
        call print_note("If an operating of point of the seed airfoil isn't converged,")
        call print_text("it can lead to a non-representative objective function value.", 7)
        call print_text("Try to change the specification of the operating point a little.", 7)

        call my_stop("Xfoil did not converge for operating point: "//stri(i))

      end if

      if (op%cl <= 0.d0 .and. ((op_spec%opt_type == OPT_MIN_SINK) .or.   &
         (op_spec%opt_type == OPT_MAX_GLIDE)) ) then
        call my_stop( "Operating point "//stri(i)//" has Cl <= 0. "//     &
                  "Cannot use "//opt_type//" optimization in this case.")
      end if

      call init_op_point_seed_ref (op_spec, op)   ! sets seed_ref for improvement calculation 

      op_point_specs(i) = op_spec                 ! write back seed reference

    end do

    goal_attainment%seed_value = goal_gap_raw(op_point_results, geo_result)

    
  end subroutine 



  subroutine eval_design_is_valid (dv, is_valid, penalty)

    !-----------------------------------------------------------------------------
    !! Returns is_valid = .true. if the design variables result in a geometric valid design,
    !! and the penalty value if not.
    !-----------------------------------------------------------------------------

    double precision, intent(in)            :: dv(:)
    logical, intent(out)                    :: is_valid
    double precision, intent(out), optional :: penalty

    type (airfoil_type)     :: foil
    double precision        :: total_penalty

    foil = create_airfoil (dv)

    total_penalty = penalty_geo  (foil, geo_constraints)
    total_penalty = total_penalty + penalty_curv (foil, curv_constraints)

    if (present(penalty)) penalty = total_penalty

    is_valid =  (total_penalty < MAX_PENALTY_DESIGN_VALID) 

  end subroutine 




  function geo_objective_results (foil) result (geo_result) 

    !----------------------------------------------------------------------------
    !! eval geometry results  
    !----------------------------------------------------------------------------

    use math_util,            only : rms
    use airfoil_geometry,     only : get_geometry,  deviation_of_side
    use shape_bezier,         only : bezier_curvature, bezier_le_curvature
    use shape_bspline,        only : bspline_curvature, bspline_le_curvature

    type(airfoil_type), intent(in)  :: foil
    type(geo_result_type)           :: geo_result 

    ! match foil - only evaluate deviation - the other results are not needed 

    if (match_foil_spec%active) then

      geo_result%match_top_deviation = rms (deviation_of_side (foil%top, match_foil_spec%foil%top, foil%spl))
      geo_result%match_bot_deviation = rms (deviation_of_side (foil%bot, match_foil_spec%foil%bot, foil%spl))

      return 
    end if 

    ! get thickness, camber ...

    call get_geometry (foil, geo_result%maxt, geo_result%xmaxt, geo_result%maxc, geo_result%xmaxc)

    ! bezier/bspline: get the curvature from the control points
        
    if (is_bezier_based (foil)) then 
      geo_result%top_curv_le = bezier_le_curvature(foil%top%bezier)
      geo_result%top_curv_te = bezier_curvature(foil%top%bezier, 1d0)
      geo_result%bot_curv_le = bezier_le_curvature(foil%bot%bezier)
      geo_result%bot_curv_te = bezier_curvature(foil%bot%bezier, 1d0)
    else if (is_bspline_based (foil)) then 
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

  end function 



  function geo_objective_function (geo_result) result (geo)

    !----------------------------------------------------------------------------
    !!  Geo objective function as result of geometry evaluation
    !----------------------------------------------------------------------------

    type(geo_result_type), intent(in) :: geo_result

    integer          :: i
    double precision :: geo

    geo  = 0.d0 
    
    ! Evaluate objective of single target and sum up - weighted and with strength

    do i = 1, size(geo_targets)

      geo = geo + geo_target_objective (geo_result, geo_targets(i))

    end do

  end function 



  function aero_objective_results (foil, flap_angles) result (op_point_results)

    !----------------------------------------------------------------------------
    !!  evalute aero objectives with xfoil 
    !----------------------------------------------------------------------------

    use xfoil_driver,       only : run_op_points
    use op_point,           only : op_point_result_type
    use shape_airfoil,      only : shape_spec

    type(airfoil_type), intent(in)  :: foil
    double precision, intent(in)    :: flap_angles (:)

    type(op_point_result_type), allocatable :: op_point_results (:) 
    type(xfoil_options_type)                :: local_xfoil_options

    ! Analyze airfoil at requested operating conditions with Xfoil

    local_xfoil_options = xfoil_options
    local_xfoil_options%show_details        = .false.  ! switch off because of multi-threading
    local_xfoil_options%exit_if_unconverged = .true.   ! speed up if an op point uncoverges

    call run_op_points (foil, local_xfoil_options, shape_spec%flap_spec, flap_angles, &
                        op_point_specs, op_point_results)

  end function



  function aero_objective_function (op_point_results) result (aero)

    !-----------------------------------------------------------------------------
    !!  Objective function as result of aerodynamic evaluation
    !!
    !!  Input:   op points results from xfoil calculation
    !!  Output:  objective function value based on airfoil performance
    !-----------------------------------------------------------------------------

    use op_point,           only : op_point_result_type
    use op_point,           only : op_point_spec_type
    use op_point,           only : op_point_objective

    type(op_point_result_type), intent(in) :: op_point_results (:)

    double precision                  :: aero
    integer                           :: i

    aero = 0d0

    ! Get objective function of each op point and sum up 
    ! - weighted and with strength 

    do i = 1, size(op_point_specs)

      aero = aero + op_point_objective (op_point_specs(i), op_point_results(i))

    end do

  end function 



  function goal_attainment_objective (op_point_results, geo_result) result (quality)

    !-----------------------------------------------------------------------------
    !! Seed-normalized objective contribution for goal attainment.
    !! Raw goal gap is mapped to a seed-relative objective with seed value 1.0:
    !!   quality_obj = weighting * (1 + raw_gap) / (1 + raw_gap_seed)
    !-----------------------------------------------------------------------------

    use op_point,           only : op_point_result_type
    use geo_target,         only : geo_result_type

    type(op_point_result_type), intent(in) :: op_point_results (:)
    type(geo_result_type), intent(in)      :: geo_result

    double precision                  :: quality
    double precision                  :: raw_goal_gap

    quality = 0d0

    if (goal_attainment%weighting <= EPSILON) return

    raw_goal_gap = goal_gap_raw(op_point_results, geo_result)
    quality = (1d0 + raw_goal_gap) / (1d0 + goal_attainment%seed_value)
    quality = quality * goal_attainment%weighting

  end function goal_attainment_objective



  function goal_gap_raw (op_point_results, geo_result) result (quality)

    !-----------------------------------------------------------------------------
    !! Raw quality metric for aero and geo targets.
    !!  - weighted RMS of remaining target_deviation
    !!  - reached targets contribute zero
    !!  - user priorities are respected via weighting_user
    !-----------------------------------------------------------------------------

    use op_point,           only : op_point_result_type, op_point_spec_type
    use op_point,           only : op_point_eval_type, op_point_eval, is_target
    use geo_target,         only : geo_result_type, geo_target_type, geo_target_eval_type
    use geo_target,         only : geo_target_eval

    type(op_point_result_type), intent(in) :: op_point_results (:)
    type(geo_result_type), intent(in)      :: geo_result

    double precision                  :: quality
    type(op_point_spec_type)          :: op_spec
    type(op_point_eval_type)          :: op_eval
    type(geo_target_type)             :: geo_spec
    type(geo_target_eval_type)        :: geo_eval
    integer                           :: i
    double precision                  :: total_target_weight, weighted_gap_sq_sum
    double precision                  :: weight_user

    quality = 0d0
    total_target_weight = 0d0
    weighted_gap_sq_sum = 0d0


    do i = 1, size(op_point_specs)

      op_spec = op_point_specs(i)

      if (.not. is_target(op_spec)) cycle

      weight_user = max(op_spec%weighting_user, 0d0)
      if (weight_user <= EPSILON) cycle

      total_target_weight = total_target_weight + weight_user

      op_eval = op_point_eval(op_spec, op_point_results(i))
      weighted_gap_sq_sum = weighted_gap_sq_sum + weight_user * op_eval%target_deviation**2
    end do

    do i = 1, size(geo_targets)

      geo_spec = geo_targets(i)

      weight_user = max(geo_spec%weighting_user, 0d0)
      if (weight_user <= EPSILON) cycle

      geo_eval = geo_target_eval(geo_spec, geo_result)

      if (geo_eval%quality == Q_NO) cycle

      total_target_weight = total_target_weight + weight_user
      weighted_gap_sq_sum = weighted_gap_sq_sum + weight_user * geo_eval%target_deviation**2
    end do

    if (total_target_weight <= EPSILON) return

    quality = sqrt(weighted_gap_sq_sum / total_target_weight)

  end function goal_gap_raw



  subroutine write_progress (dv, designcounter, all_targets_achieved)

    !-----------------------------------------------------------------------------
    !! Writes airfoil coordinates and op Points results to files during optimization
    !!  designcounter = 0 will start new files 
    !!  all_targets_achieved signals if optimization is done and all targets are achieved 
    !-----------------------------------------------------------------------------

    use xfoil_driver,         only : run_op_points, xfoil_stats_print
    use op_point,             only : op_point_result_type
    use shape_airfoil,        only : shape_spec, BEZIER, HICKS_HENNE, get_seed_foil

    double precision, intent(in)    :: dv (:)
    integer, intent(in)             :: designcounter
    logical, intent(out), optional  :: all_targets_achieved

    type(op_point_result_type), allocatable :: op_point_results (:)
    type(airfoil_type)              :: foil
    type(xfoil_options_type)        :: local_xfoil_options
    type(geo_result_type)           :: geo_result
    double precision, allocatable   :: flap_angles (:)

    if (present(all_targets_achieved)) all_targets_achieved = .false.

    if (designcounter == 0) then
      call print_action ("Writing design #0 being seed airfoil to "//design_subdir)
    else
      if (show_details) print *  
    end if
  
    ! Design 0 is seed airfoil to output - take the original values 

    if (designcounter == 0) then 
      foil = get_seed_foil ()

    ! Design > 0 - Build current foil from design variables 
    else 
      foil = create_airfoil (dv)
      foil%filename = output_prefix
      foil%name     = output_prefix
    end if

    ! Get actual flap angles based on design variables

    flap_angles = get_flap_angles (dv)

    ! Performance Cache: Try to get xfoil result from "save best" in objective function

    if (allocated(best_op_point_results)) then 
      ! Sanity check - Is the "best" really our current foil
      if (abs(sum(foil%y) - sum(best_foil%y)) < EPSILON ) then  ! num issues with symmetrical) 
        op_point_results = best_op_point_results
      else
        call print_warning ("eval write: no best design available") 
        best_objective = 2d0                                  ! reset best store - something wrong...?
      end if 
    end if 

    ! There is no stored result - so re-calc for this foil

    if (.not. allocated(op_point_results)) then 

      ! Analyze airfoil at requested operating conditions with Xfoil

      local_xfoil_options = xfoil_options
      local_xfoil_options%show_details        = .false.  
      local_xfoil_options%exit_if_unconverged = .false.       ! we need all op points
      if (designcounter == 0) then
        local_xfoil_options%reinitialize = .false.            ! strange: reinit leeds sometimes to not converged
      end if 

      call run_op_points (foil, local_xfoil_options, shape_spec%flap_spec, flap_angles, &
              op_point_specs, op_point_results)
    end if 

    geo_result = geo_objective_results (foil)


    ! All out to files - coordinates, op points, geo targets...

    call write_progress_to_files (designcounter, foil, op_point_results, flap_angles, geo_result)


    ! Print improvement details for current design

    if (show_details .and. (designcounter > 0)) then 
      call print_improvement  (op_point_specs, geo_targets, op_point_results, geo_result, &
                              shape_spec%flap_spec%use_flap, flap_angles) 

      call print_objective_contributions (foil, op_point_results, geo_result)

      ! call penalty_stats_print_table ()
      call xfoil_stats_print
      print * 
    end if

    if (present(all_targets_achieved)) then
      all_targets_achieved = eval_all_targets_achieved (op_point_results, geo_result)
    end if

    return

  end subroutine



  subroutine write_progress_to_files (designcounter, foil, op_point_results, flap_angles, geo_result)

    use op_point,             only : op_point_result_type
    use shape_airfoil,        only : shape_spec, BEZIER, HICKS_HENNE

    integer, intent(in)                       :: designcounter
    type(airfoil_type), intent(in)            :: foil
    type(op_point_result_type), intent(in)    :: op_point_results (:)
    double precision, intent(in)              :: flap_angles (:)
    type(geo_result_type), intent(in)         :: geo_result

    character(:), allocatable                 :: foil_file, bezier_file, op_points_file
    character(:), allocatable                 :: geo_targets_file, hicks_file
    integer                                   :: foil_unit, bezier_unit, hicks_unit, op_points_unit, geo_unit

    foil_file        = design_subdir//'Design_Coordinates.csv'
    op_points_file   = design_subdir//'Design_OpPoints.csv'
    bezier_file      = design_subdir//'Design_Beziers.csv'
    hicks_file       = design_subdir//'Design_Hicks.csv'
    geo_targets_file = design_subdir//'Design_GeoTargets.csv'

    if (designcounter == 0) then

      open(newunit=op_points_unit, file=op_points_file, status='replace', err=901)
      call write_design_op_points_header (op_points_unit)

      if (shape_spec%type == BEZIER) then
        open(newunit=bezier_unit, file=bezier_file, status='replace', err=902)
        call write_design_bezier_header (bezier_unit, foil)
      else if (shape_spec%type == HICKS_HENNE) then
        open(newunit=hicks_unit, file=hicks_file, status='replace', err=902)
        call write_design_hh_header (hicks_unit, foil, shape_spec%hh)
      else
        open(newunit=foil_unit, file=foil_file, status='replace', err=900)
        call write_design_coord_header (foil_unit, foil)
      end if

      if (size(geo_targets) > 0) then
        open(newunit=geo_unit, file=geo_targets_file, status='replace', err=903)
        call write_design_geo_targets_header (geo_unit)
      end if

    else

      open(newunit=op_points_unit, file=op_points_file, status='old', position='append', &
           action='readwrite', err=901)

      if (shape_spec%type == BEZIER) then
        open(newunit=bezier_unit, file=bezier_file, status='old', position='append', &
             action='readwrite', err=902)
      else if (shape_spec%type == HICKS_HENNE) then
        open(newunit=hicks_unit, file=hicks_file, status='old', position='append', &
             action='readwrite', err=902)
      else
        open(newunit=foil_unit, file=foil_file, status='old', position='append', &
             action='readwrite', err=900)
      end if

      if (size(geo_targets) > 0) then
        open(newunit=geo_unit, file=geo_targets_file, status='old', position='append', &
             action='readwrite', err=903)
      end if

    end if

    call write_design_op_points_data (op_points_unit, designcounter, op_point_specs, &
                                      op_point_results, flap_angles)

    if (shape_spec%type == BEZIER) then
      call write_design_bezier_data (bezier_unit, designcounter, foil)
    else if (shape_spec%type == HICKS_HENNE) then
      call write_design_hh_data (hicks_unit, designcounter, foil)
    else
      call write_design_coord_data (foil_unit, designcounter, foil)
    end if

    if (size(geo_targets) > 0) then
      call write_design_geo_targets_data (geo_unit, designcounter, geo_targets, geo_result)
    end if

    close (op_points_unit)
    if (shape_spec%type == BEZIER) then
      close (bezier_unit)
    else if (shape_spec%type == HICKS_HENNE) then
      close (hicks_unit)
    else
      close (foil_unit)
    end if
    if (size(geo_targets) > 0) close (geo_unit)

    return

    900 call print_warning ("Warning: unable to open "//foil_file//". Skipping ...")
      return
    901 call print_warning ("Warning: unable to open "//op_points_file//". Skipping ...")
      return
    902 call print_warning ("Warning: unable to open "//bezier_file//". Skipping ...")
      return
    903 call print_warning ("Warning: unable to open "//geo_targets_file//". Skipping ...")
      return

  end subroutine write_progress_to_files



  subroutine print_objective_contributions (foil, op_point_results, geo_result)

    use math_util,            only : sort_vector_descend
    use op_point,             only : op_point_result_type
    use shape_airfoil,        only : get_seed_foil

    type(airfoil_type), intent(in)              :: foil
    type(op_point_result_type), intent(in)      :: op_point_results (:)
    type(geo_result_type), intent(in)           :: geo_result

    double precision                            :: obj_aero, obj_geo, obj_targ_attain
    double precision                            :: obj_penalty_curv, obj_penalty_geo
    double precision                            :: obj_total, improv_total_pp
    double precision                            :: imp_aero, imp_geo, imp_targ_attain
    double precision                            :: imp_pen_curv, imp_pen_geo
    double precision                            :: seed_aero, seed_geo, seed_quality
    double precision                            :: seed_penalty_curv, seed_penalty_geo
    double precision, allocatable               :: improv_sorted (:)
    type(airfoil_type)                          :: seed_foil

    obj_aero          = aero_objective_function(op_point_results)
    obj_geo           = geo_objective_function(geo_result)
    obj_targ_attain   = goal_attainment_objective(op_point_results, geo_result)
    obj_penalty_curv  = penalty_curv(foil, curv_constraints)
    obj_penalty_geo   = penalty_geo(foil, geo_constraints)

    seed_aero     = sum(op_point_specs%weighting)
    seed_geo      = sum(geo_targets%weighting)
    seed_quality  = goal_attainment%weighting
    seed_foil     = get_seed_foil()
    seed_penalty_curv = penalty_curv(seed_foil, curv_constraints)
    seed_penalty_geo  = penalty_geo(seed_foil, geo_constraints)

    imp_aero        = (seed_aero         - obj_aero)         * 100d0
    imp_geo         = (seed_geo          - obj_geo)          * 100d0
    imp_targ_attain = (seed_quality      - obj_targ_attain)       * 100d0
    imp_pen_curv    = (seed_penalty_curv - obj_penalty_curv) * 100d0
    imp_pen_geo     = (seed_penalty_geo  - obj_penalty_geo)  * 100d0

    obj_total = obj_aero + obj_geo + obj_targ_attain + obj_penalty_curv + obj_penalty_geo

    improv_total_pp = imp_aero + imp_geo + imp_targ_attain + imp_pen_curv + imp_pen_geo
    improv_sorted = [imp_aero, imp_geo, imp_targ_attain, imp_pen_curv, imp_pen_geo]
    call sort_vector_descend (improv_sorted)

    call print_text('Objective contributions', 10)
    call print_single_obj_contrib('aero objective',    obj_aero,         imp_aero,        improv_sorted,12)
    call print_single_obj_contrib('geometry targets',  obj_geo,          imp_geo,         improv_sorted,12)
    call print_single_obj_contrib('target attainment', obj_targ_attain,  imp_targ_attain, improv_sorted,12)
    call print_single_obj_contrib('curvature penalty', obj_penalty_curv, imp_pen_curv,    improv_sorted,12)
    call print_single_obj_contrib('geometry penalty ', obj_penalty_geo,  imp_pen_geo,     improv_sorted,12)

    print *

  end subroutine print_objective_contributions


  
  subroutine write_final_results (dv, fmin, elapsed_seconds, final_foil, flap_angles )

    !-----------------------------------------------------------------------------
    !! Writes final airfoil design 
    !!    Returns final airfoil 
    !-----------------------------------------------------------------------------

    use shape_airfoil,          only : shape_spec
    use xfoil_driver,           only : run_op_points
    use op_point,               only : op_point_spec_type, op_point_result_type

    double precision, allocatable, intent(in)   :: dv (:) 
    double precision, intent(in)                :: fmin
    double precision, intent(in)                :: elapsed_seconds
    type(airfoil_type), intent(out)             :: final_foil
    double precision, allocatable, intent(out)  :: flap_angles (:) 

    type(op_point_result_type), allocatable     :: op_point_results (:)
    type(geo_result_type)                       :: geo_result

    print *
    call print_header ('Optimization completed in '//str_duration(elapsed_seconds)//'. ', no_crlf=.true.)
    call print_colored (COLOR_NORMAL,'Objective function improvement over seed:')
    call print_colored_f (8, '(F7.4,"%")', Q_GOOD, ((1d0 - fmin) * 100.d0))
    print *

    ! create final airfoil and flap angles from designvars 

    final_foil  = create_airfoil  (dv)
    flap_angles = get_flap_angles (dv)

    ! analyze final design - Run xfoil for requested operating points

    call run_op_points (final_foil, xfoil_options, shape_spec%flap_spec, flap_angles,  &
              op_point_specs, op_point_results)

    ! get geo results 

    geo_result = geo_objective_results (final_foil)      

    print *
    call print_improvement  (op_point_specs, geo_targets, op_point_results, geo_result, &
                              shape_spec%flap_spec%use_flap, flap_angles) 
    print *

  end subroutine 



  function create_airfoil (dv) result (foil) 

    !-------------------------------------------------------------------------------
    !! Create an airfoil out of a seed airfoil and designvars 
    !-------------------------------------------------------------------------------

    use shape_airfoil,      only : shape_spec, BEZIER, BSPLINE, HICKS_HENNE 
    use shape_airfoil,      only : create_airfoil_hicks_henne
    use shape_airfoil,      only : create_airfoil_bezier
    use shape_airfoil,      only : create_airfoil_bspline
    use shape_airfoil,      only : get_ndv_of_flaps
    
    double precision, intent(in)    :: dv(:)
    type(airfoil_type)              :: foil

    double precision, allocatable   :: dv_shape (:)
    integer                         :: ndv, ndv_flap, ndv_shape 

    ndv = size(dv) 
    ndv_flap  = get_ndv_of_flaps ()                 
    ndv_shape = ndv - ndv_flap 

    dv_shape = dv (1 : ndv_shape)

    ! extract designvars for shape_spec (without flap designvars)

    select case (shape_spec%type)

      case (BEZIER)

        call create_airfoil_bezier (dv_shape, foil)

      case (BSPLINE)

        call create_airfoil_bspline (dv_shape, foil)

      case (HICKS_HENNE)

        call create_airfoil_hicks_henne (dv_shape, foil)

      case default

        call my_stop ("create_airfoil: Unknown shape type")

    end select 

  end function



    
  function get_flap_angles (dv) result (flap_angles)
    
    !----------------------------------------------------------------------------
    !! Get actual flap angles from design vars (if there are...) 
    !! If the flap of an op point is fixed, return the fixed value (normally = 0) 
    !! dv:    all design variables! - of flaps are at the end  
    !----------------------------------------------------------------------------

    use shape_airfoil,            only : get_flap_angles_optimized

    double precision, intent(in)    :: dv (:)
    double precision, allocatable   :: flap_angles (:)

    double precision, allocatable   :: flap_angles_optimized (:)
    integer               :: i, iopt, noppoint

    noppoint = size(op_point_specs)  

    ! init with predefined flap angle 
    
    flap_angles = op_point_specs(:)%flap_angle

    ! retrieve all optimized flap angles from dv 

    flap_angles_optimized = get_flap_angles_optimized (dv)
    iopt = 0  

    ! assign these to op points which have flap to be optimized 

    do i = 1, noppoint

      if (op_point_specs(i)%flap_optimize) then               ! this op angle is optimized

        iopt = iopt + 1
        flap_angles (i) = flap_angles_optimized (iopt)

      else                                                    ! this op angle is fixed

        flap_angles (i) = op_point_specs(i)%flap_angle

      end if 
    end do 

  end function  



function eval_all_targets_achieved (op_point_results, geo_result) result (achieved)

  !! Check if all targets are achieved - meaning deviation to target value is below defined threshold

  use op_point, only : op_point_eval_type, op_point_eval

  type(op_point_result_type), intent(in)  :: op_point_results (:)
  type(geo_result_type), intent(in)       :: geo_result

  type(op_point_eval_type)  :: op_eval
  logical                   :: achieved
  type(geo_target_eval_type) :: geo_eval
  integer                   :: i

  achieved = .false.
  
  do i= 1, size(op_point_specs)
    op_eval = op_point_eval(op_point_specs(i), op_point_results(i))
    if (.not. op_eval%target_reached) return
  end do 

  do i= 1, size(geo_targets)
    geo_eval = geo_target_eval(geo_targets(i), geo_result)
    if (.not. geo_eval%target_reached) return
  end do

  achieved = .true. 

end function eval_all_targets_achieved

end module


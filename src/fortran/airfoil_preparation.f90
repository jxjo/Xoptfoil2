! MIT License
! Copyright (c) 2025 Jochen Guenzel

!
! Preparing seed airfoil prior to optimization 
!

module airfoil_preparation
  
  use commons
  use os_util
  use print_util
  use string_util,      only : stri, strf
  use eval_commons,     only : curv_side_constraints_type

  use airfoil_base,     only : airfoil_type, side_airfoil_type, panel_options_type
  use airfoil_base,     only : is_hh_based, is_bezier_based, is_bspline_based
  use airfoil_base,     only : is_top, is_bot
  use airfoil_base,     only : airfoil_load

  implicit none
  private

  public :: prepare_seed_foil
  public :: prepare_match_foil
  public :: as_bezier_based, as_bspline_based
  public :: match_bezier, match_bspline
  public :: repanel_match_foil

  public :: determine_auto_curvature

  ! --------- private --------------------------------------------------------

  ! static vars for match objective function 

  type (side_airfoil_type)      :: target_side                  ! target side airfoil to match
  double precision              :: target_le_curv               ! target le curvature 
  double precision              :: target_te_gap                ! target TE gap 
  type (curv_side_constraints_type) :: target_curv_constraints  ! curvature constraints for matching
  integer                       :: nevals = 0                   ! number of evaluations
  logical                       :: is_match_bezier              ! is matching bezier or bspline (for dispatching in objective function)

contains

  subroutine prepare_seed_foil (org_foil, eval_spec, shape_spec, seed_foil)

    !-----------------------------------------------------------------------------
    !! Read and prepare seed airfoil to be ready for optimization 
    !-----------------------------------------------------------------------------

    use airfoil_base,         only : airfoil_write_with_shapes, make_symmetrical, is_bezier_le_c2
    use eval_commons,         only : eval_spec_type
    use shape_airfoil,        only : shape_spec_type
    use shape_airfoil,        only : BEZIER, BSPLINE, HICKS_HENNE
  
    type (airfoil_type), intent(in)       :: org_foil
    type (eval_spec_type), intent(inout)  :: eval_spec
    type (shape_spec_type), intent(inout) :: shape_spec
    type (airfoil_type), intent(out)      :: seed_foil

    type (airfoil_type) :: foil

    foil = org_foil

    ! symmetrical? 

    if (eval_spec%geo_constraints%symmetrical)  call make_symmetrical (foil)

    ! Auto curvature constraints based on seed

    if (eval_spec%curv_constraints%auto_curvature) &
      call determine_auto_curvature (foil, eval_spec%curv_constraints)

    ! Prepare Seed Airfoil based on optimization shape type  
  
    if (shape_spec%type == BEZIER) then 
  
      if (is_bezier_based (foil) .and. is_bezier_le_c2 (foil)) then          ! take seed bezier definition  
      
        shape_spec%bezier%ncp_top = size(foil%top%bezier%px)
        shape_spec%bezier%ncp_bot = size(foil%bot%bezier%px)

        seed_foil = foil

        call print_note ("Using Bezier control points from Bezier seed airfoil. "// &
                        "Values in 'bezier_options' will be ignored", 3)
      else                                      ! a new bezier "match foil" is generated  
    
        seed_foil = as_bezier_based (foil, shape_spec%bezier, eval_spec%panel_options, & 
                                        eval_spec%curv_constraints)
      end if


    else if (shape_spec%type == BSPLINE) then

      if (is_bspline_based (foil)) then         ! take seed bspline definition
          
        shape_spec%bspline%ncp_top = size(foil%top%bspline%px)
        shape_spec%bspline%ncp_bot = size(foil%bot%bspline%px)

        seed_foil = foil

        call print_note ("Using B-spline control points from seed airfoil. "// &
                        "Values in 'bspline_options' will be ignored", 3)
      else                                      ! a new bspline "match foil" is generated to be new seed

        seed_foil = as_bspline_based (foil, shape_spec%bspline, eval_spec%panel_options, &
                                         eval_spec%curv_constraints)
      end if


    else if (shape_spec%type == HICKS_HENNE) then

      if (is_hh_based (foil)) then              ! take seed hh definition
  
        shape_spec%hh%nfunctions_top = size(foil%top%hh%hhs)
        shape_spec%hh%nfunctions_bot = size(foil%bot%hh%hhs)

        call print_note ("Using Hicks-Henne functions from seed airfoil. "// &
                         "Values in 'hicks_henne_options' will be ignored")

        seed_foil = foil

      else                                      ! smooth (match bezier) of seed prior ot optimization

        seed_foil = as_bezier_based (foil, shape_spec%bezier, eval_spec%panel_options, & 
                                        eval_spec%curv_constraints)
      end if 

    end if  
  
  
    ! Make sure seed airfoil passes constraints - final checks
    call check_foil (seed_foil, shape_spec, eval_spec%curv_constraints, eval_spec%geo_constraints, &
                     eval_spec%xfoil_options)

                     
    ! write final seed airfoil as reference 
  
    call airfoil_write_with_shapes (seed_foil, design_subdir, highlight=.false.)             

  end subroutine 


  function repanel_match_foil (foil) result(foil_repanelled)

    !-----------------------------------------------------------------------------
    !! Repanel match foil to have a constant number of points  
    !-----------------------------------------------------------------------------

    use airfoil_base, only : repanel

    type (airfoil_type), intent(in)          :: foil
    type (airfoil_type)                      :: foil_repanelled

    type (panel_options_type)                :: panel_options

    panel_options%npoint   = 101
    panel_options%le_bunch = 0.98d0
    panel_options%te_bunch = 0.7d0

    foil_repanelled = repanel (foil, panel_options, silent=.true.)

  end function



  subroutine prepare_match_foil (seed_foil, match_foil_spec) 

    !-----------------------------------------------------------------------------
    !! Read and prepare match airfoil to be ready for optimization 
    !-----------------------------------------------------------------------------

    use airfoil_base,         only : normalize, te_gap, EPSILON, repanel
    use eval_commons,         only : match_foil_spec_type

    use airfoil_base   

    type (airfoil_type), intent(in)             :: seed_foil
    type (match_foil_spec_type), intent(inout)  :: match_foil_spec
    type (airfoil_type)             :: match_foil

    ! read / create airfoil 
    
    match_foil = airfoil_load (match_foil_spec%filename)

    match_foil = repanel_match_foil (match_foil)  ! ensure same number of points as for match foil


    match_foil_spec%foil = match_foil

    ! check if seed and match foil have same te gap 

    if (abs(te_gap (seed_foil) - te_gap(match_foil)) > EPSILON) then 
      call print_warning("Seed and match airfoil have different TE gaps. Match won't be good.",5)
    end if 

  end subroutine



  subroutine check_foil (foil, shape_spec, curv_constraints, geo_constraints, xfoil_options)

    !-----------------------------------------------------------------------------
    !! Checks seed airfoil passes all geometric constraints.
    !-----------------------------------------------------------------------------

    use eval_commons 
    use xfoil_driver,         only : xfoil_options_type
    use xfoil_driver,         only : xfoil_defaults
    use eval_constraints,     only : penalty_geo, penalty_curv
    use eval_constraints,     only : penalty_stats_init, penalty_stats_print, penalty_stats_print_table

    use shape_airfoil,        only : shape_spec_type, HICKS_HENNE, BEZIER, BSPLINE


    type (airfoil_type), intent(inout)          :: foil
    type (shape_spec_type), intent(inout)       :: shape_spec
    type (curv_constraints_type), intent(inout) :: curv_constraints
    type (geo_constraints_type), intent(in)     :: geo_constraints
    type (xfoil_options_type), intent(in)       :: xfoil_options
    
    double precision :: penalty
    logical          :: ok_top, ok_bot

    ! Validate that shape-specific airfoil data exists -------------------------

    if (shape_spec%type == BEZIER .and. .not. is_bezier_based(foil)) then
      call my_stop ('Seed airfoil missing Bezier data - preparation failed')
    else if (shape_spec%type == BSPLINE .and. .not. is_bspline_based(foil)) then
      call my_stop ('Seed airfoil missing B-spline data - preparation failed')
    else if (shape_spec%type == HICKS_HENNE .and. .not. (is_hh_based(foil) .or. is_bezier_based(foil))) then
      call my_stop ('Seed airfoil missing Bezier or Hicks-Henne data - preparation failed')
    end if


    ! Check curvature constraints like reversals -------------------------

    if(curv_constraints%check_curvature) then

      call print_action ('Checking seed airfoil passes all curvature constraints ... ', no_crlf = .true.)


      ! --  overall curvature check - early exit if ok 

      call penalty_stats_init ()

      penalty = penalty_curv (foil, curv_constraints)

      if (penalty == 0d0) then 
        if (show_details) then 
          call print_colored_rating (Q_GOOD)
          print * 
        end if 

      else

        ! -- check and handle violations on top and bot 
        print *

        call check_side_curvature_violations (foil%top, curv_constraints%top, ok_top)

        if (.not. foil%symmetrical) then
          call check_side_curvature_violations (foil%bot, curv_constraints%bot, ok_bot)
        else
          ok_bot = ok_top
        end if

        print *
        if (.not. (ok_top .and. ok_bot)) then 
          call print_note ('Optimization will proceed despite this initial curvature constraint violation.')   
          print *     
        end if
      end if

    end if

    ! Check geometry ------------------------------------------------------------------

    ! init xfoil - will be used also for geometry

    call xfoil_defaults (xfoil_options)


    if (geo_constraints%check_geometry) then

      call print_action ('Checking seed airfoil passes all geometry constraints ... ', no_crlf = .true.)


      penalty = penalty_geo (foil, geo_constraints)  ! get stats for geometry penalty

      if (penalty > 0d0) then 
        print * 
        print *
        call penalty_stats_print (5)
        call print_note ("Please adapt this geometry constraint to seed airfoil", 5)
        call my_stop ("Seed airfoil doesn't meet a geometry constraint") 
      else
        if (show_details) then 
          call print_colored_rating (Q_GOOD)
          print * 
        end if 
      end if 

    end if 


  end subroutine 


!-----------------------------------------------------------------------------

  subroutine match_assess_results (result_side, steps, max_iterations, result_ok)

    !! Assess and print matching result quality (deviation from target, LE curvature accuracy)

    use math_util,            only : rms, count_reversals
    use eval_commons,         only : curv_constraints_type
    use eval_constraints,     only : penalty_bumpiness

    use airfoil_geometry,     only : deviation_of_side

    type (side_airfoil_type), intent(in)    :: result_side
    integer, intent(in)                     :: steps            ! optimization iterations performed
    integer, intent(in)                     :: max_iterations   ! max allowed iterations
    logical, intent(out)                    :: result_ok        ! .true. if result is acceptable

    double precision              :: dev_rms, dev_max_at, pen_bumps, te_curv
    double precision              :: max_curv_te, min_te_allowed, max_te_allowed, delta_te
    integer                       :: how_good_revers, how_good_dev, how_good_bumps, how_good_te
    integer                       :: n_reversals
    double precision, allocatable :: devi (:), x(:), curv(:)
    type (curv_side_constraints_type)  :: con

    x    = result_side%x
    curv = result_side%curvature
    con  = target_curv_constraints

    ! assess rms 

    devi            = deviation_of_side (result_side, target_side)  ! get deviation at each target point as array
    dev_rms         = rms (devi) 
    dev_max_at      = target_side%x(maxloc (devi,1))
    how_good_dev    = r_quality (dev_rms, 0.0001d0, 0.0005d0, 0.002d0) 

    ! assess TE curvature - should be below max_te_curvature constraint

    te_curv         = curv(size(curv))
    max_curv_te     = abs(con%max_te_curvature) * (-1d0)**con%max_curv_reverse
    min_te_allowed  = min(0d0, max_curv_te)
    max_te_allowed  = max(0d0, max_curv_te)
    delta_te        = 0d0
    if (te_curv < min_te_allowed) then
      delta_te = abs(te_curv - min_te_allowed)
    else if (te_curv > max_te_allowed) then
      delta_te = abs(te_curv - max_te_allowed)
    end if
    how_good_te     = r_quality (delta_te, 0.1d0, 2.0d0, 1d6)

    ! assess curvature reversals - should be below max_curv_reverse constraint

    n_reversals     = count_reversals (x, curv, con%curv_threshold) 
    how_good_revers = i_quality (n_reversals - con%max_curv_reverse, 1, 1, 2)

    ! assess bumpiness - should be below threshold (computed value, not hard constraint)
    
    pen_bumps       = penalty_bumpiness (x, curv, max_reversals=con%max_curv_reverse) * 1000d0
    how_good_bumps  = r_quality (pen_bumps, 0.01d0, 0.3d0, 1d0)

    result_ok = (how_good_dev <= Q_OK) .and. (how_good_te == Q_GOOD) .and. &
                (how_good_bumps <= Q_OK) .and. (how_good_revers == Q_GOOD)

    if (show_details) then 

      if (result_ok .and. .false.) then
          call print_colored_rating (Q_GOOD)
          print * 
      else

        call print_text        ('Iterations: '//stri(steps), 7, no_crlf = .true.)
        call print_highlighted (', Deviation rms: ', how_good_dev, strf('f6.4', dev_rms*100d0), '%')
        call print_colored     (COLOR_NOTE, ' max at x='//strf('F4.2', dev_max_at))
        call print_highlighted (', TE curv: ', how_good_te, strf('F5.2', te_curv), '')
        call print_highlighted (', Reversals: ', how_good_revers, stri(n_reversals), '')
        call print_highlighted (', Bumps: ', how_good_bumps, strf('F5.2', pen_bumps), '')

        if (steps == max_iterations) then
          call print_colored (COLOR_WARNING, ' Max iter reached')
        end if
        print *
      end if

    end if

    if (.not. result_ok) then
      call print_warning ('Matching result not good enough - consider adjusting curvature constraints', 5)
    end if

  end subroutine


!-----------------------------------------------------------------------------


  function as_bezier_based (foil, shape_bezier, panel_options, curv_constraints) result(foil_bezier)

    !-----------------------------------------------------------------------------
    !! Transform foil to bezier based using simplex optimization for best fit 
    !! of bezier curves on top and bot side.
    !! Returns the transformed airfoil
    !-----------------------------------------------------------------------------

    use airfoil_base,         only : split_foil_into_sides, is_normalized_coord, add_suffix_to_name
    use airfoil_base,         only : airfoil_from_bezier
    use airfoil_base,         only : te_gap, repanel, is_normalized

    use eval_commons,         only : curv_constraints_type
    use shape_bezier,         only : shape_bezier_type
    use shape_bezier,         only : bezier_eval_side, write_bezier_file
    use shape_bezier,         only : bezier_spec_type, bezier_le_curvature

    type (airfoil_type), intent (in)          :: foil
    type (shape_bezier_type), intent (inout)  :: shape_bezier
    type (panel_options_type), intent (in)    :: panel_options
    type (curv_constraints_type), intent (in) :: curv_constraints
    type (airfoil_type)                       :: foil_bezier

    type (bezier_spec_type)   :: top_bezier, bot_bezier
    type (airfoil_type)       :: match_foil
    logical                   :: top_result_ok, bot_result_ok
    double precision          :: le_curv

    ! Sanity check 
  
    call print_action ("Creating Bezier based seed airfoil by matching "//foil%filename//"...")

    if (show_details .and. .not. is_normalized (foil)) then 
       call print_note ("Airfoil will be normalized before matching", 5) 
    end if

    ! Create reduced-point airfoil (81 points) for faster matching

    match_foil = repanel_match_foil (foil)
    le_curv = maxval (match_foil%top%curvature)  ! use max curvature at LE as target for matching 

    ! Top side  

    call match_bezier  (match_foil%top, le_curv, curv_constraints%top, shape_bezier%ncp_top, &
                        top_bezier, top_result_ok)

    ! Bot side 

    call match_bezier  (match_foil%bot, le_curv, curv_constraints%bot, shape_bezier%ncp_bot, &
                        bot_bezier, bot_result_ok)

    ! build airfoil out of Bezier curves 

    foil_bezier = airfoil_from_bezier (top_bezier, bot_bezier, panel_options%npoint)

    foil_bezier%filename = filename_stem (foil%filename) 
    foil_bezier%name     = foil%name

    call add_suffix_to_name (foil_bezier, '-bezier'//stri(shape_bezier%ncp_top) //stri(shape_bezier%ncp_bot))

  end function



  function as_bspline_based (foil, shape_bspline, panel_options, curv_constraints) result(foil_bspline)

    !-----------------------------------------------------------------------------
    !! Transform foil to B-spline based using simplex optimization for best fit
    !! of B-spline curves on top and bot side.
    !! Returns the transformed airfoil
    !-----------------------------------------------------------------------------

    use airfoil_base,         only : split_foil_into_sides, is_normalized_coord, add_suffix_to_name
    use airfoil_base,         only : airfoil_from_bspline, repanel, is_normalized

    use eval_commons,         only : curv_constraints_type
    use shape_bspline,        only : shape_bspline_type
    use shape_bspline,        only : bspline_eval_side, write_bspline_file
    use shape_bspline,        only : bspline_spec_type, bspline_le_curvature

    type (airfoil_type), intent (in)           :: foil
    type (shape_bspline_type), intent (inout)  :: shape_bspline
    type (panel_options_type), intent (in)     :: panel_options
    type (curv_constraints_type), intent (in)  :: curv_constraints
    type (airfoil_type)                        :: foil_bspline

    type (bspline_spec_type)  :: top_bspline, bot_bspline
    type (airfoil_type)       :: match_foil
    logical                   :: top_result_ok, bot_result_ok
    double precision          :: le_curv

    call print_action ("Creating B-Spline based seed airfoil by matching "//foil%filename//"...")

    if (show_details .and. .not. is_normalized (foil)) then 
       call print_note ("Airfoil will be normalized before matching", 5) 
    end if

    ! Create reduced-point airfoil (81 points) for faster matching
    match_foil = repanel_match_foil (foil)
    le_curv = maxval (match_foil%top%curvature)  ! use max curvature at LE as target for matching 

    ! Top side

    call match_bspline (match_foil%top, le_curv, curv_constraints%top, shape_bspline%ncp_top, &
                        top_bspline, top_result_ok)

    ! Bot side

    call match_bspline (match_foil%bot, le_curv, curv_constraints%bot, shape_bspline%ncp_bot, &
                        bot_bspline, bot_result_ok)

    ! build airfoil out of B-spline curves

    foil_bspline = airfoil_from_bspline (top_bspline, bot_bspline, panel_options%npoint)

    foil_bspline%filename = filename_stem (foil%filename) 
    foil_bspline%name     = foil%name

    call add_suffix_to_name (foil_bspline, '-bspline'//stri(shape_bspline%ncp_top) //stri(shape_bspline%ncp_bot))

  end function



  function match_objective_function (dv) result (obj) 

    !! Objective function to match Bezier or B-spline curve to 'target_side'
    !! Target parameters are set by match_bezier via module variables
    
    use math_util,            only : rms
    use shape_bezier,         only : bezier_eval_side, map_dv_to_bezier
    use shape_bspline,        only : bspline_eval_side, map_dv_to_bspline
    use eval_constraints,     only : penalty_curv_of_side, penalty_stats_print_vals, penalty_stats_init
    use airfoil_geometry,     only : deviation_of_side

    double precision, intent(in)  :: dv(:)
    double precision              :: obj

    double precision              :: obj_rms, pen_curv
    type(side_airfoil_type)       :: side

    nevals = nevals + 1

    if (mod(nevals, 100) == 0) call penalty_stats_init ()

    ! Map DV to bezier control points and eval bezier

    if (is_match_bezier) then
      side%bezier = map_dv_to_bezier (is_bot(target_side), dv, target_te_gap, target_le_curv)
      call bezier_eval_side (side%bezier, 81, side%x, side%y, side%curvature, use_arc_length=.false.)
      side%name = target_side%name  ! Ensure side has the same name as target
    else
      side%bspline = map_dv_to_bspline (is_bot(target_side), dv, target_te_gap, target_le_curv)
      call bspline_eval_side (side%bspline, 81, side%x, side%y, side%curvature, use_arc_length=.false.)
      side%name    = target_side%name  ! Ensure side has the same name as target
    end if

    ! Calculate deviation from target points and curvature penalties for this side

    obj_rms  = rms (deviation_of_side (side, target_side)) 

    ! Curvature penalty - scaled up to ensure good curvature match 
    ! - otherwise optimizer might find a good fit in terms of point deviation but with bad curvature (e.g. bumps) 
    !   which is not desired for a good seed airfoil satisfying curvature constraints.

    pen_curv = penalty_curv_of_side (side, target_curv_constraints)

    obj      = obj_rms + pen_curv

    ! Debug output every 100 evaluations
    if (mod(nevals, 100) == 0 .and. show_details) then
      if (.false.) then
        if (nevals == 100) print *
        call print_text(stri(nevals,4)//':', 10, no_crlf=.true.)
        call print_text('obj: '//strf('F9.6', obj,.true.), 3, no_crlf=.true.)
        call print_text('rms: '//strf('F9.6', obj_rms,.true.), 3, no_crlf=.true.)
        if (pen_curv > 0d0) then
          call penalty_stats_print_vals (6)
        else
          print *
        end if
      else
        call print_colored (COLOR_PALE, ".")
      end if
    end if

  end function match_objective_function



  subroutine match_bezier  (side, le_curv, curv_side_constraints, ncp, bezier, result_ok)
   
    !-----------------------------------------------------------------------------
    !! match a bezier curve to one side of an airfoil 
    !!    side:                  either 'top' or 'bot' side of an airfoil
    !!    le_curv:               leading edge curvature for this side
    !!    curv_side_constraints: curvature constraints for this side
    !!    ncp:                   number of control points the bezier curve should have 
    !!    bezier:                returns evaluated bezier definition 
    !!    result_ok:             returns .true. if result is acceptable 
    !-----------------------------------------------------------------------------

    use math_util,            only : interp1
    use eval_commons,         only : curv_side_constraints_type
    use shape_bezier,         only : bezier_spec_type, get_initial_bezier, bezier_get_dv0, bezier_eval_side, map_dv_to_bezier
    use simplex_search,       only : simplexsearch, simplex_options_type 
    use eval_constraints,     only : penalty_stats_init

    type (side_airfoil_type), intent(in)            :: side  
    double precision, intent(in)                    :: le_curv
    type (curv_side_constraints_type), intent(in)   :: curv_side_constraints
    integer, intent(in)                             :: ncp
    type (bezier_spec_type), intent(out)            :: bezier 
    logical, intent(out)                            :: result_ok

    type (side_airfoil_type)        :: result_side  
    type (simplex_options_type)     :: sx_options
    double precision, allocatable   :: xopt(:), dv0(:)
    double precision                :: fmin
    integer                         :: steps, fevals

    ! Setup targets in module variable for objective function 
    target_te_gap           = side%y(size(side%y))
    target_le_curv          = le_curv
    target_curv_constraints = curv_side_constraints
    target_side             = side

    is_match_bezier = .true.  ! flag for objective function to know which type of curve is being matched

    ! Initial estimate for bezier control points based on 'side to match'   
    bezier = get_initial_bezier(target_side%x, target_side%y, target_le_curv, target_te_gap, ncp)

    ! Nelder mead (simplex) optimization
    sx_options%no_improv_break = 100
    sx_options%no_improv_thr   = 1d-8
    sx_options%min_iterations  = 200                   ! ensure not to leave optimization too early 
    sx_options%max_iterations  = 4000
    sx_options%initial_step    = interp1 ( 5d0, 8d0, dble(ncp), 0.2d0, 0.1d0) 

    ! Start vector of design variables dv0 
    dv0 = bezier_get_dv0(is_bot(side), bezier, c2_coupled=.true.)

    if (show_details) then
      call penalty_stats_init()
      call print_text(side%name//' side targets: '//stri(size(target_side%x))//' points'// &
                      ', '//stri(ncp)//' control points, LE curvature '//stri(nint(target_le_curv))//'. Matching ',&
                      indent = 5, no_crlf = .true.)
    end if 

    nevals = 0
    xopt = dv0
    call simplexsearch (xopt, fmin, steps, fevals, match_objective_function, &
                        dv0, sx_options)

    ! Finished - build bezier, calc deviation at target points and assess results

    result_side%bezier = map_dv_to_bezier (is_bot(side), xopt, target_te_gap, target_le_curv)
    call bezier_eval_side (result_side%bezier, 81, result_side%x, result_side%y, result_side%curvature, use_arc_length=.true.)

    if (show_details) print *
    
    call match_assess_results (result_side, steps, sx_options%max_iterations, result_ok)
 
    ! Return the optimized bezier
    bezier = result_side%bezier

  end subroutine match_bezier 



  subroutine match_bspline  (side, le_curv, curv_side_constraints, ncp, bspline, result_ok)
   
    !-----------------------------------------------------------------------------
    !! match a bspline curve to one side of an airfoil 
    !!    side:                  either 'top' or 'bot' side of an airfoil
    !!    le_curv:               leading edge curvature for this side
    !!    curv_side_constraints: curvature constraints for this side
    !!    ncp:                   number of control points the bspline curve should have 
    !!    bspline:               returns evaluated bspline definition 
    !!    result_ok:             returns .true. if result is acceptable 
    !-----------------------------------------------------------------------------

    use math_util,            only : interp1
    use eval_commons,         only : curv_side_constraints_type
    use shape_bspline,        only : bspline_spec_type, get_initial_bspline, bspline_get_dv0, bspline_eval_side, map_dv_to_bspline
    use simplex_search,       only : simplexsearch, simplex_options_type 
    use eval_constraints,     only : penalty_stats_init, penalty_stats_print_table

    type (side_airfoil_type), intent(in)            :: side  
    double precision, intent(in)                    :: le_curv
    type (curv_side_constraints_type), intent(in)   :: curv_side_constraints
    integer, intent(in)                             :: ncp
    type (bspline_spec_type), intent(out)           :: bspline 
    logical, intent(out)                            :: result_ok

    type (simplex_options_type)     :: sx_options
    double precision, allocatable   :: xopt(:), dv0(:)
    double precision                :: fmin
    integer                         :: steps, fevals
    type (side_airfoil_type)        :: result_side

    ! Setup targets in module variable for objective function 
    target_te_gap           = side%y(size(side%y))
    target_le_curv          = le_curv
    target_curv_constraints = curv_side_constraints
    target_side             = side
    is_match_bezier         = .false.  ! flag for objective function to know which type of curve is being matched


    ! Initial estimate for bspline control points based on 'side to match'   
    bspline = get_initial_bspline(target_side%x, target_side%y, target_le_curv, target_te_gap, ncp)

    ! Nelder mead (simplex) optimization
    sx_options%no_improv_thr   = 1d-7
    sx_options%min_iterations  = 200                   ! ensure not to leave optimization too early 
    sx_options%max_iterations  = 4000
    sx_options%no_improv_break = 100
    sx_options%initial_step    = interp1 ( 5d0, 10d0, dble(ncp), 0.2d0, 0.2d0) 

    ! Start vector of design variables dv0 
    dv0 = bspline_get_dv0(is_bot(side), bspline, c2_coupled=.true.)

    if (show_details) then
      call penalty_stats_init()
      call print_text(side%name//' side targets: '//stri(size(target_side%x))//' points'// &
                      ', '//stri(ncp)//' control points, LE curvature '//stri(nint(target_le_curv))//'. Matching ',&
                      indent = 5, no_crlf = .true.)
    end if 

    nevals = 0
    xopt = dv0
    call simplexsearch (xopt, fmin, steps, fevals, match_objective_function, dv0, sx_options)

    ! Finished - build bspline, calc deviation at target points  

    result_side%bspline = map_dv_to_bspline (is_bot(side), xopt, target_te_gap, target_le_curv)
    call bspline_eval_side (result_side%bspline, 81, result_side%x, result_side%y, result_side%curvature, use_arc_length=.true.)

    call match_assess_results (result_side, steps, sx_options%max_iterations, result_ok)
 
    ! Return the optimized bspline
    bspline = result_side%bspline

  end subroutine match_bspline 

  
  
  subroutine determine_curvature_constraints_side (side, curv_threshold, c_spec, has_spikes)
  
    !! Evaluates and sets the best values for surface thresholds and constraints
  
    use math_util,            only: count_reversals, clip, derivative1, round
    use eval_commons,         only: curv_side_constraints_type
    use airfoil_base,         only: side_airfoil_type
    use airfoil_geometry,     only: max_curvature_at_te
    
    type (side_airfoil_type), intent(in)  :: side 
    double precision, intent(in) :: curv_threshold
    type (curv_side_constraints_type), intent (inout)  :: c_spec
    logical, intent(out) :: has_spikes

    double precision      :: max_te_curv, threshold_spikes
    integer               :: nrevers, nspikes

    ! sanity

    if (.not. allocated(side%curvature)) then
      call my_stop ("Curvature array not allocated for "//side%name//" side")
    end if

    ! check for spikes (derivative of curvature) due to few decimals in .dat file

    threshold_spikes = 0.1d0
    nspikes    = count_reversals (side%x, derivative1 (side%x, side%curvature), threshold_spikes, x_start = 0.1d0)
    has_spikes = (nspikes > 10)

    ! evaluate curvature reversals 

    ! if (has_spikes) then 
    !   threshold = max (threshold, 0.5d0)  ! if we have spikes, we need a higher threshold to get a reasonable number of reversals
    ! end if

    ! nrevers    = count_reversals (side%x, side%curvature, threshold, x_start = 0.1d0, x_end = 0.9d0, smooth=has_spikes) 
    nrevers    = count_reversals (side%x, side%curvature, curv_threshold, x_start = 0.1d0, x_end = 1.0d0, smooth=has_spikes) 

    c_spec%max_curv_reverse = nrevers

    ! evaluate max trailing edge curvature 

    max_te_curv = max_curvature_at_te (side%curvature)
    max_te_curv = round (max_te_curv, 1)            ! equals AE approach, penalty has threshold of 0.05
    max_te_curv = clip (max_te_curv, 0.0d0, 2.0d0)  ! sanity limit for max curvature at te

    c_spec%max_te_curvature = max_te_curv

    if (show_details) then
      call print_text (side%name//' side: ',5, no_crlf=.true.)
      call print_highlighted ('', Q_NEW, stri(nrevers,2),' reversal(s)')
      call print_highlighted (', max_te_curvature: ', Q_NEW, strf('f4.2',max_te_curv),'')
      print *
    end if 

     
  end subroutine determine_curvature_constraints_side
  

  subroutine determine_auto_curvature (foil, c_spec)
  
    !! Evaluates and sets the best values for surface thresholds and constraints for both sides of the airfoil
  
    use math_util,            only: count_reversals, derivative1 
    use eval_commons,         only: curv_constraints_type
    use airfoil_base,         only: airfoil_type, is_dat_based
  
    type (airfoil_type), intent(in)  :: foil 
    type (curv_constraints_type), intent (inout)  :: c_spec

    double precision        :: curv_threshold
    logical                 :: has_spikes_top, has_spikes_bot 
  
    ! adapt thresholds based on foil type

    if (is_dat_based(foil)) then 
      curv_threshold = 0.1d0
    else 
      curv_threshold = c_spec%top%curv_threshold 
    end if

    call print_action ("Determine auto curvature constraints ...")

    if (show_details) then 
      call print_highlighted ('Using threshold = ', Q_OK, strf('F5.2', curv_threshold), ' for detection of reversals.', &
                              indent = 5, no_crlf = .false.)
    end if

    ! Evaluate constraints for sides and set in c_spec

    call determine_curvature_constraints_side (foil%top, curv_threshold, c_spec%top, has_spikes_top)
  
    if (.not. foil%symmetrical) then 
      call determine_curvature_constraints_side (foil%bot, curv_threshold, c_spec%bot, has_spikes_bot)
    else
      has_spikes_bot = has_spikes_top
      c_spec%bot = c_spec%top
    end if 

    if (has_spikes_top .or. has_spikes_bot) then 
      call print_warning ("Due to few decimals in .dat file, the result is uncertain", 5)
    end if

  end subroutine determine_auto_curvature

 
  
  subroutine auto_te_curvature_side (side, c_spec)
  
    !! Evaluates the best value for curvature at TE of polyline
  
    use airfoil_geometry,     only : max_curvature_at_te 
    use eval_commons,         only : curv_side_constraints_type
  
    type (side_airfoil_type), intent(in)  :: side 
    type (curv_side_constraints_type), intent (inout)  :: c_spec
  
    double precision          :: max_curv 
    integer                   :: quality_te
    character(:), allocatable :: label
    logical                   :: auto  
    
    auto = .true.

    max_curv = max_curvature_at_te (side%curvature)  
  ! give a little more to breath during opt.
    max_curv = max ((max_curv * 1.1d0), (max_curv + 0.05d0))
    c_spec%max_te_curvature = max_curv
  
  
    ! Print it all 
  
    if (show_details) then 

      call print_text ("",16, no_crlf=.true.)

      quality_te      = r_quality (c_spec%max_te_curvature, 0.2d0, 1d0, 10d0)
      label = 'max_te_curvature'
      call print_colored (COLOR_PALE, label//' =') 
      call print_colored_f (5,'(F5.2)', quality_te, c_spec%max_te_curvature) 
      if (auto) then
        if (quality_te > Q_BAD) then
          call print_text ('Like seed there will be a geometric spoiler at TE', 3)
        else
          call print_text ('Smallest value based on seed',3)
        end if 
      else 
        call print_text ('User defined',3)
      end if 
    end if 
  
  end subroutine auto_te_curvature_side
  
    

  subroutine check_side_curvature_violations (side, c, ok)
  
    !! Checks surface x,y for violations of curvature contraints 
    !!     reversals > max_curv_reverse and handles user response  
  
    use math_util,            only : derivative1, count_reversals, round
    use eval_commons,         only : curv_side_constraints_type
    use airfoil_geometry,     only : max_curvature_at_te
    use eval_constraints,     only : penalty_stats_init, penalty_curv_of_side, has_penalty, penalty_bumpiness
    use eval_constraints,     only : penalty_stats_print_table, penalty_d4_bspline
    use eval_constraints,     only : PEN_CURV_REVERSALS, PEN_TE_CURV, PEN_LE_CURV_MONOTON, PEN_CURV_BUMPS, PEN_D4
  
    type (side_airfoil_type), intent(in)  :: side 
    type (curv_side_constraints_type), intent (inout)  :: c
    logical, intent(out) :: ok
  
    integer                     :: nreverse, how_good_bumps
    double precision            :: curv_te, penalty, penalty_bumps
    character(:), allocatable   :: info
  
    info = side%name // " side"
    ok = .true.

    call penalty_stats_init()                              
    
    penalty   = penalty_curv_of_side (side, c)   ! get total curvature penalty for this side to report in case of violation

    if (penalty == 0d0) then 
      return                                              ! no violation, exit early
    end if

    ok = .false.

    print *
    call print_warning ("Curvature violations on " // trim(info), indent = 5)
    print *
    
    ! curvature reversals ... 

    if (has_penalty (PEN_CURV_REVERSALS)) then
      nreverse = count_reversals (side%x, side%curvature, c%curv_threshold, x_start = 0.2d0)  
      call print_highlighted ('Found ', Q_PROBLEM, stri(nreverse), &
                              ' reversal(s) where max_curv_reverse is set to '// stri(c%max_curv_reverse),&
                              indent = 7, no_crlf = .false.)
    end if

    ! curvature at te

    if (has_penalty (PEN_TE_CURV)) then
      curv_te = max_curvature_at_te (side%curvature)
      call print_highlighted ('Curvature at TE ', Q_PROBLEM, strf('F6.2', curv_te), &
                              ' where max_te_curvature is set to '// strf('F6.2',c%max_te_curvature), &
                              indent = 7, no_crlf = .false.)
    end if

    ! curvature at le - monotonicity

    if (has_penalty (PEN_LE_CURV_MONOTON)) then
      call print_highlighted ('Leading edge curvature is ', Q_PROBLEM, 'not monotonically', ' decreasing ',&
                              indent = 7, no_crlf = .false.)
    end if

    ! bumps 

    if (has_penalty (PEN_CURV_BUMPS)) then
      penalty_bumps   = penalty_bumpiness (side%x, side%curvature, max_reversals=c%max_curv_reverse) * 100d0
      how_good_bumps  = r_quality (penalty_bumps, 0.001d0, 0.03d0, 0.1d0)
      call print_highlighted ('Curvature bumps ', how_good_bumps, strf('F6.4', penalty_bumps), '', &
                              indent = 7, no_crlf = .false.)
    end if

    call print_note ('Setting initial penalty to '// strf('F8.6', penalty), 5)
    c%initial_penalty = penalty
  
  end subroutine check_side_curvature_violations
  
  
end module
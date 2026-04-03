! MIT License
! Copyright (c) 2025 Jochen Guenzel

!
! Preparing seed airfoil prior to optimization 
!

module airfoil_preparation
  
  use os_util
  use print_util
  use string_util,            only : stri, strf
  use commons

  use airfoil_base,     only : airfoil_type, side_airfoil_type, panel_options_type
  use airfoil_base,     only : is_hh_based, is_bezier_based, is_bspline_based

  implicit none
  private

  public :: get_airfoil
  public :: prepare_seed_foil
  public :: prepare_match_foil
  public :: as_bezier_based, as_bspline_based
  public :: match_bezier, match_bspline, match_get_best_le_curvature

  public :: check_airfoil_curvature, auto_curvature_constraints

  ! --------- private --------------------------------------------------------

  ! static vars for match objective function 

  double precision, allocatable :: target_x(:)        ! target coordinates of side_to_match
  double precision, allocatable :: target_y(:) 
  double precision              :: target_le_curv           ! target le curvature 
  double precision              :: target_max_te_curv       ! max te curvature 
  double precision              :: te_gap                   ! ... needed for bezier rebuild 
  integer                       :: target_max_reversals     ! max reversals allowed in match
  integer                       :: nevals = 0               ! number of evaluations 
  
  ! Curve type and parameters for generic objective function
  logical                       :: match_use_bezier         ! .true. for bezier, .false. for bspline
  integer                       :: match_n_eval             ! 81 for bezier, 101 for bspline
  double precision              :: match_scale_te           ! 10.0 for bezier, 5.0 for bspline 

contains

  subroutine prepare_seed_foil (filename, eval_spec, shape_spec, seed_foil)

    !-----------------------------------------------------------------------------
    !! Read and prepare seed airfoil to be ready for optimization 
    !-----------------------------------------------------------------------------

    use airfoil_base,         only : normalize, is_normalized_coord
    use airfoil_base,         only : airfoil_write_with_shapes, split_foil_into_sides, make_symmetrical
    use airfoil_geometry,     only : repanel, repanel_and_normalize
    use eval_commons,         only : eval_spec_type
    use shape_airfoil,        only : shape_spec_type
    use shape_airfoil,        only : BEZIER, BSPLINE, HICKS_HENNE
    use shape_bezier,         only : bezier_ncp_to_ndv => ncp_to_ndv
    use shape_bspline,        only : bspline_ncp_to_ndv => ncp_to_ndv
    use shape_hicks_henne,    only : nfunctions_to_ndv
  
    character (*), intent(in)             :: filename
    type (eval_spec_type), intent(inout)  :: eval_spec
    type (shape_spec_type), intent(inout) :: shape_spec
    type (airfoil_type), intent(out)      :: seed_foil

    type (airfoil_type)       :: original_foil

    ! read / create seed airfoil 

    call get_airfoil (filename, original_foil)

    ! is LE at 0,0? Do basic normalize to split, create spline ...

    if (.not. is_normalized_coord(original_foil)) & 
      call normalize (original_foil, basic=.true.)

    call split_foil_into_sides (original_foil)     ! upper and lower will be needed for input sanity


    ! repanel seed to panel_options

    if (is_bezier_based (original_foil) .or. is_bspline_based (original_foil)) then
      ! Bezier and B-spline are already normalized - just repanel
      seed_foil = repanel(original_foil, eval_spec%panel_options)
    else if (is_hh_based (original_foil)) then       ! Hicks-Henne foils is already normalized
      seed_foil = original_foil                    ! keep paneling 
    else
      call repanel_and_normalize (original_foil, seed_foil, eval_spec%panel_options) 
    end if


    ! symmetrical? 

    if (eval_spec%geo_constraints%symmetrical)  call make_symmetrical (seed_foil)
   

    ! Prepare Airfoil based on optimization shape type  
  
    if (shape_spec%type == BEZIER) then 
  
      if (is_bezier_based (seed_foil)) then 
  
          ! ignore 'bezier_options' - take seed bezier definition  
          shape_spec%bezier%ncp_top = size(seed_foil%top_bezier%px)
          shape_spec%bezier%ncp_bot = size(seed_foil%bot_bezier%px)
          shape_spec%bezier%ndv     = bezier_ncp_to_ndv (shape_spec%bezier%ncp_top) + &
                                      bezier_ncp_to_ndv (shape_spec%bezier%ncp_bot, le_c2_coupled=.true.)
  
          call print_note ("Using Bezier control points from seed airfoil. "// &
                          "Values in 'bezier_options' will be ignored", 3)
      else
  
        ! a new bezier "match foil" is generated to be new seed 
        seed_foil%name = seed_foil%name // '-bezier' //stri(shape_spec%bezier%ncp_top) //&
                                                       stri(shape_spec%bezier%ncp_bot)
  
        seed_foil = as_bezier_based (seed_foil, shape_spec%bezier, eval_spec%panel_options, & 
                                        eval_spec%curv_constraints)
  
      end if

    else if (shape_spec%type == BSPLINE) then

      if (is_bspline_based (seed_foil)) then

          ! ignore 'bspline_options' - take seed bspline definition
          shape_spec%bspline%ncp_top = size(seed_foil%top_bspline%px)
          shape_spec%bspline%ncp_bot = size(seed_foil%bot_bspline%px)
          shape_spec%bspline%ndv     = bspline_ncp_to_ndv (shape_spec%bspline%ncp_top) + &
                                       bspline_ncp_to_ndv (shape_spec%bspline%ncp_bot, le_c2_coupled=.true.)

          call print_note ("Using B-spline control points from seed airfoil. "// &
                          "Values in 'bspline_options' will be ignored", 3)
      else

        ! a new bspline "match foil" is generated to be new seed
        seed_foil%name = seed_foil%name // '-bspline' //stri(shape_spec%bspline%ncp_top) //&
                                                         stri(shape_spec%bspline%ncp_bot)

        seed_foil = as_bspline_based (seed_foil, shape_spec%bspline, eval_spec%panel_options, &
                                         eval_spec%curv_constraints)

      end if

    else if (shape_spec%type == HICKS_HENNE) then

      if (is_hh_based (seed_foil)) then 
  
        ! ignore 'hciks henne _options' - take seed hh  definition  
        shape_spec%hh%nfunctions_top = size(seed_foil%top_hh%hhs)
        shape_spec%hh%nfunctions_bot = size(seed_foil%bot_hh%hhs)
        shape_spec%hh%ndv  = nfunctions_to_ndv (shape_spec%hh%nfunctions_top) + & 
                             nfunctions_to_ndv (shape_spec%hh%nfunctions_bot)

        call print_note ("Using Hicks-Henne functions from seed airfoil. "// &
                         "Values in ", 3, no_crlf=.true.)
        call print_colored (COLOR_WARNING, "'hicks_henne_options' will be ignored")
        print *

      ! smooth (match bezier) of seed prior ot optimization 

      else if (shape_spec%hh%smooth_seed) then 
        seed_foil = as_bezier_based (seed_foil, shape_spec%bezier, eval_spec%panel_options, & 
                                        eval_spec%curv_constraints)
        seed_foil%name = seed_foil%name // '-smoothed'
      end if 

    end if  
  
  
    ! Make sure seed airfoil passes constraints - final checks

    call check_seed (seed_foil, shape_spec, eval_spec%curv_constraints, eval_spec%geo_constraints, &
                     eval_spec%xfoil_options)

                     
    ! write final seed airfoil as reference 
  
    call airfoil_write_with_shapes (seed_foil, design_subdir, highlight=.false.)             

  end subroutine 



  subroutine prepare_match_foil (seed_foil, match_foil_spec) 

    !-----------------------------------------------------------------------------
    !! Read and prepare match airfoil to be ready for optimization 
    !-----------------------------------------------------------------------------

    use airfoil_base,         only : normalize, te_gap, EPSILON
    use airfoil_geometry,     only : repanel, repanel_and_normalize
    use eval_commons,         only : match_foil_spec_type

    use airfoil_base   

    type (airfoil_type), intent(in)             :: seed_foil
    type (match_foil_spec_type), intent(inout)  :: match_foil_spec
    type (airfoil_type)             :: match_foil

    ! read / create airfoil 
    call get_airfoil (match_foil_spec%filename, match_foil)

    ! is LE at 0,0? Do basic normalize to split, create spline ...
    if (.not. is_normalized_coord(match_foil)) & 
      call normalize (match_foil, basic=.true.)

    call split_foil_into_sides (match_foil)     ! upper and lower will be needed for input sanity

    match_foil_spec%foil = match_foil

    ! check if seed and match foil have same te gap 

    if (abs(te_gap (seed_foil) - te_gap(match_foil)) > EPSILON) then 
      call print_warning("Seed and match airfoil have different TE gaps. Match won't be good.",5)
    end if 

    ! get the target coordinates to match 
    ! #todo
    ! call match_get_targets (match_foil%top, match_foil_spec%target_top_x, &
    !                                         match_foil_spec%target_top_y)

    ! call match_get_targets (match_foil%bot, match_foil_spec%target_bot_x, &
    !                                         match_foil_spec%target_bot_y)
  end subroutine



  subroutine get_airfoil (filename, foil, silent_mode )

    !-----------------------------------------------------------------------------------
    !! loads either .dat or .bez file into 'foil' 
    !-----------------------------------------------------------------------------------

    use airfoil_base,       only : is_dat_file
    use shape_bezier,       only : read_bezier_file, is_bezier_file, bezier_spec_type
    use shape_hicks_henne,  only : load_hh_airfoil, is_hh_file
    use shape_airfoil,      only : build_from_hh_seed
    use airfoil_base,       only : airfoil_load, split_foil_into_sides, airfoil_from_bezier

    character(*), intent(in)        :: filename
    type(airfoil_type), intent(out) :: foil
    logical,intent(in), optional    :: silent_mode 

    integer           :: np
    logical           :: silent

    if (present(silent_mode)) then 
      silent = silent_mode 
    else 
      silent = .false. 
    end if 


    if (is_dat_file (filename)) then 

      ! Read seed airfoil from .dat file

      if (.not. silent) call print_action ('Reading airfoil file', filename)

      call airfoil_load (filename, foil)


    else if (is_bezier_file (filename)) then

      ! Read seed bezier control points from .bez file and generate airfoil
      if (.not. silent) call print_action ('Reading Bezier file', filename)

      np = 161                              ! 201 points as default - will be repaneled anyway
      call read_bezier_file (filename, 'Top', foil%name, foil%top_bezier)
      call read_bezier_file (filename, 'Bot', foil%name, foil%bot_bezier)
      foil = airfoil_from_bezier (foil%top_bezier, foil%bot_bezier, np, foil%name) 

      
    else if (is_hh_file (filename)) then

      ! Read seed hh functions and coordinates from .hicks file and generate airfoil 
      if (.not. silent) call print_action ('Reading Hicks-Henne file', filename)
   
      call load_hh_airfoil (filename, foil%name, foil%top_hh, foil%bot_hh, foil%hh_seed_name, &
                            foil%hh_seed_x, foil%hh_seed_y) 
      call build_from_hh_seed (foil)
    
    else

      call my_stop ("Unknown file type: "//quoted (filename))
    
    end if


  end subroutine get_airfoil



  subroutine check_seed(seed_foil, shape_spec, curv_constraints, geo_constraints, xfoil_options)

    !-----------------------------------------------------------------------------
    !! Checks seed airfoil passes all geometric constraints.
    !-----------------------------------------------------------------------------

    use eval_commons 
    use xfoil_driver,         only : xfoil_options_type
    use xfoil_driver,         only : xfoil_defaults
    use eval_constraints,     only : assess_surface 
    use eval_constraints,     only : penalty_geo, penalty_curv, penalty_curv_of_side 
    use eval_constraints,     only : penalty_stats_init, penalty_stats_print
    use math_util,            only : norm_2

    use shape_airfoil,        only : shape_spec_type, HICKS_HENNE, BEZIER, BSPLINE


    type (airfoil_type), intent(inout)          :: seed_foil
    type (shape_spec_type), intent(inout)       :: shape_spec
    type (curv_constraints_type), intent(inout) :: curv_constraints
    type (geo_constraints_type), intent(in)     :: geo_constraints
    type (xfoil_options_type), intent(in)       :: xfoil_options
    
    double precision :: penalty
    integer          :: nptt, nptb, overall_quality

    ! Validate that shape-specific airfoil data exists -------------------------

    if (shape_spec%type == BEZIER) then
      if (.not. allocated(seed_foil%top_bezier%px) .or. .not. allocated(seed_foil%bot_bezier%px)) then
        call my_stop('Seed airfoil missing Bezier data - preparation failed')
      end if
    else if (shape_spec%type == BSPLINE) then
      if (.not. allocated(seed_foil%top_bspline%px) .or. .not. allocated(seed_foil%bot_bspline%px)) then
        call my_stop('Seed airfoil missing B-spline data - preparation failed')
      end if
    else if (shape_spec%type == HICKS_HENNE) then
      if (is_hh_based (seed_foil)) then
        if (.not. allocated(seed_foil%top_hh%hhs) .or. .not. allocated(seed_foil%bot_hh%hhs)) then
          call my_stop('Seed airfoil missing Hicks-Henne data - preparation failed')
        end if
      end if
    end if

    ! Check curvature constraints like reversals -------------------------

    if(curv_constraints%check_curvature) then

      call print_action ('Checking seed airfoil passes all curvature constraints ... ')

      ! call eval_curvature_violations (seed_foil, curv_constraints, has_violation, viol_id, violation_text)

      ! if seed has already a non monotous LE curvature like NACA 23112 or Eppler 334
      !    - switch further checks off  


      ! if (viol_id == VIOL_LE_CURV_MONOTON) then
      !   print * 
      !   call print_warning (violation_text, 5)
      !   call print_note ('Switching off curvature monotonous checks at LE' )
      !   curv_constraints%top%check_le_curvature = .false. 
      !   curv_constraints%bot%check_le_curvature = .false. 

      call check_airfoil_curvature (curv_constraints, seed_foil, overall_quality)

      if (overall_quality > Q_OK) then 
        print * 
        print *
        call print_note ("Please adapt curvature constraints to seed airfoil", 5)
        call my_stop ("Seed airfoil doesn't meet a curvature constraint") 
      end if

      ! Final check for curvature reversals

      ! call check_handle_curve_violations (seed_foil%top, curv_constraints%top)
      ! if (.not. seed_foil%symmetrical) & 
      !   call check_handle_curve_violations (seed_foil%bot, curv_constraints%bot)

      ! ! Final check trailing edge 

      ! call check_te_curvature_violations (seed_foil%top, curv_constraints%top)
      ! if (.not. seed_foil%symmetrical) & 
      !   call check_te_curvature_violations (seed_foil%bot, curv_constraints%bot)

    end if

    ! Check geometry ------------------------------------------------------------------

    nptt = size(seed_foil%top%x)
    nptb = size(seed_foil%bot%x)

    ! init xfoil - will be used also for geometry

    call xfoil_defaults (xfoil_options)


    if (geo_constraints%check_geometry) then

      call print_action ('Checking seed airfoil passes all geometry constraints ... ', no_crlf = .true.)

      call penalty_stats_init ()

      penalty = penalty_geo (seed_foil, geo_constraints)  ! get stats for geometry penalty

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


  subroutine match_curve_calc_results (curve, deviation, le_curv_result, te_curv_result, x, curv)

    !! Calculate deviation and curvature results for a curve match (Bezier or B-spline)
    !! Uses polymorphism with dispatcher routines for clean code

    use math_util,            only : curve_spec_type
    use shape_curve,          only : curve_eval_y_on_x, curve_eval_side
    use shape_curve,          only : curve_le_curvature, curve_curvature

    class(curve_spec_type), intent(in)       :: curve  ! Polymorphic: bezier or bspline
    double precision, allocatable, intent(out)   :: deviation(:), x(:), curv(:)
    double precision, intent(out)                :: le_curv_result, te_curv_result

    double precision, allocatable :: y(:)
    integer :: nTarg, i, n

    nTarg = size(target_x)
    allocate (deviation(nTarg))

    ! Dispatchers handle type selection internally
    do i = 1, nTarg 
      deviation(i) = abs(curve_eval_y_on_x(curve, target_x(i), epsilon=1d-8) - target_y(i))
    end do 

    le_curv_result = curve_le_curvature(curve)
    te_curv_result = curve_curvature(curve, 1d0)

    ! Evaluate curve for penalty calculation (same as in objective function)
    n = 101
    call curve_eval_side(curve, n, x, y, curv, use_arc_length=.false.)

  end subroutine


!-----------------------------------------------------------------------------


  subroutine match_assess_results (deviation, le_curv_target, le_curv_result, &
                                             te_curv_result, steps, max_iterations, &
                                             x, curv, result_ok)

    !! Assess and print matching result quality (deviation from target, LE curvature accuracy)

    use eval_constraints,       only : penalty_curv_deriv
    use math_util,              only : count_reversals

    double precision, intent(in)  :: deviation(:)     ! deviation at each target point
    double precision, intent(in)  :: le_curv_target   ! target LE curvature
    double precision, intent(in)  :: le_curv_result   ! achieved LE curvature
    double precision, intent(in)  :: te_curv_result   ! achieved TE curvature
    integer, intent(in)           :: steps            ! optimization iterations performed
    integer, intent(in)           :: max_iterations   ! max allowed iterations
    double precision, intent(in)  :: x(:), curv(:)    ! evaluated curve for penalty calculation
    logical, intent(out)          :: result_ok        ! .true. if result is acceptable

    double precision :: dev_rms, dev_max, dev_max_at, pen_curv_deriv, te_delta
    integer          :: how_good_revers, how_good_dev, how_good_le, how_good_bump, how_good_te
    integer          :: n_reversals

    ! assess results 

    dev_rms         = 100d0 * norm2 (deviation) / sqrt(dble(size(deviation)))
    dev_max_at      = target_x(maxloc (deviation,1))
    how_good_dev    = r_quality (dev_rms, 0.001d0, 0.005d0, 0.020d0) 

    te_delta        = abs (te_curv_result) - target_max_te_curv - 0.001d0
    how_good_te     = r_quality (te_delta, 0d0, 0.01d0, 0.1d0)

    n_reversals     = count_reversals (1, -1, curv, 0d0) 
    how_good_revers = i_quality (n_reversals - target_max_reversals, 1, 1, 2)

    pen_curv_deriv  = penalty_curv_deriv (x, curv, threshold_body=1d0, threshold_te=1d0)
    how_good_bump   = r_quality (pen_curv_deriv, 0.001d0, 0.01d0, 0.5d0)

    if (show_details) then 

      call print_text        ('Iterations: '//stri(steps), 7, no_crlf = .true.)
      call print_highlighted (', Deviation rms: ', how_good_dev, strf('(f5.3)', dev_rms), '%')
      call print_colored     (COLOR_NOTE, ' max at x='//strf('(F4.2)', dev_max_at))
      call print_highlighted (', TE curv: ', how_good_te, strf('(F5.2)', te_curv_result), '')
      call print_highlighted (', Reversals: ', how_good_revers, stri(n_reversals), '')
      call print_highlighted (', Bump: ', how_good_bump, strf('(F6.3)', pen_curv_deriv), '')

      if (steps == max_iterations) then
        call print_colored (COLOR_WARNING, ' Max iter reached')
      end if
      print *

    end if

    result_ok = (how_good_dev <= Q_OK) .and. (how_good_te == Q_GOOD) .and. &
                (how_good_bump <= Q_OK) .and. (how_good_revers == Q_GOOD)
    if (.not. result_ok) then
      call print_warning ('Matching result not good enough - consider adjusting curvature constraints', 5)
    end if

  end subroutine


!-----------------------------------------------------------------------------


  function as_bezier_based (foil, shape_bezier, panel_options, curv_constraints) result(airfoil_bezier)

    !-----------------------------------------------------------------------------
    !! Transform foil to bezier based using simplex optimization for best fit 
    !! of bezier curves on top and bot side.
    !! Returns the transformed airfoil
    !-----------------------------------------------------------------------------

    use airfoil_base,         only : split_foil_into_sides, is_normalized_coord
    use airfoil_base,         only : airfoil_from_bezier
    use airfoil_base,         only : te_gap

    use eval_commons,         only : curv_constraints_type
    use eval_constraints,     only : assess_side
    use shape_bezier,         only : shape_bezier_type
    use shape_bezier,         only : bezier_eval_side, write_bezier_file
    use shape_bezier,         only : bezier_spec_type, bezier_le_curvature

    type (airfoil_type), intent (in)          :: foil
    type (shape_bezier_type), intent (inout)  :: shape_bezier
    type (panel_options_type), intent (in)    :: panel_options
    type (curv_constraints_type), intent (in) :: curv_constraints
    type (airfoil_type)                       :: airfoil_bezier

    type (bezier_spec_type)   :: top_bezier, bot_bezier
    integer                   :: max_reversals
    double precision          :: best_le_curv, max_te_curv
    logical                   :: top_result_ok, bot_result_ok

    ! Sanity check 
  
    if (.not. is_normalized_coord(foil)) then 
      call my_stop ('Airfoil is not normalized prior to Bezier transform')
    end if  

    call print_action ("Create Bezier based airfoil")

    if (show_details) print *

    ! Le curvature is constrained in optimization
    best_le_curv = match_get_best_le_curvature (foil)

    ! Top side  

    max_reversals = curv_constraints%top%max_curv_reverse
    max_te_curv   = curv_constraints%top%max_te_curvature

    call match_bezier  (foil%top, best_le_curv, max_te_curv, max_reversals, shape_bezier%ncp_top, &
                        top_bezier, top_result_ok)

    ! Bot side 

    max_reversals = curv_constraints%bot%max_curv_reverse
    max_te_curv   = curv_constraints%bot%max_te_curvature

    call match_bezier  (foil%bot, bezier_le_curvature (top_bezier), max_te_curv, max_reversals, shape_bezier%ncp_bot, &
                        bot_bezier, bot_result_ok)

    if (show_details) print *

    ! build airfoil out of Bezier curves 
    airfoil_bezier = airfoil_from_bezier (top_bezier, bot_bezier, panel_options%npoint, foil%name)

  end function



  function as_bspline_based (foil, shape_bspline, panel_options, curv_constraints) result(airfoil_bspline)

    !-----------------------------------------------------------------------------
    !! Transform foil to B-spline based using simplex optimization for best fit
    !! of B-spline curves on top and bot side.
    !! Returns the transformed airfoil
    !-----------------------------------------------------------------------------

    use airfoil_base,         only : split_foil_into_sides, is_normalized_coord, te_gap
    use airfoil_base,         only : airfoil_from_bspline

    use eval_commons,         only : curv_constraints_type
    use eval_constraints,     only : assess_side
    use shape_bspline,        only : shape_bspline_type
    use shape_bspline,        only : bspline_eval_side, write_bspline_file
    use shape_bspline,        only : bspline_spec_type, bspline_le_curvature

    type (airfoil_type), intent (in)           :: foil
    type (shape_bspline_type), intent (inout)  :: shape_bspline
    type (panel_options_type), intent (in)     :: panel_options
    type (curv_constraints_type), intent (in)  :: curv_constraints
    type (airfoil_type)                        :: airfoil_bspline

    type (bspline_spec_type)  :: top_bspline, bot_bspline
    integer                   :: max_reversals
    double precision          :: best_le_curv, max_te_curv
    logical                   :: top_result_ok, bot_result_ok

    ! Sanity check

    if (.not. is_normalized_coord(foil)) then
      call my_stop ('Airfoil is not normalized prior to B-spline transform')
    end if

    call print_action ("Create B-spline based airfoil")

    if (show_details) print *

    ! LE curvature is constrained in optimization
    best_le_curv = match_get_best_le_curvature (foil)

    ! Top side

    max_reversals = curv_constraints%top%max_curv_reverse
    max_te_curv   = curv_constraints%top%max_te_curvature

    call match_bspline (foil%top, best_le_curv, max_te_curv, max_reversals, shape_bspline%ncp_top, &
                        top_bspline, top_result_ok)

    ! Bot side

    max_reversals = curv_constraints%bot%max_curv_reverse
    max_te_curv   = curv_constraints%bot%max_te_curvature

    call match_bspline (foil%bot, bspline_le_curvature (top_bspline), max_te_curv, max_reversals, shape_bspline%ncp_bot, &
                        bot_bspline, bot_result_ok)

    if (show_details) print *

    ! build airfoil out of B-spline curves

    airfoil_bspline = airfoil_from_bspline (top_bspline, bot_bspline, panel_options%npoint, foil%name)

  end function



  function match_get_best_le_curvature (foil) result (best_curv)

    !! determine the target curvature at leading edge 
    !!    if there are bumps it's not, a good value has to be estimated

    type (airfoil_type), intent(in)    :: foil 
    
    double precision  :: at_le, max_around_le, best_curv
    double precision, allocatable   :: top_curv (:), bot_curv (:)
    logical                         :: bump_at_upper_le, bump_at_lower_le

    top_curv = abs(foil%top%curvature)
    bot_curv = abs(foil%bot%curvature)

    at_le = top_curv (1) 
    max_around_le = max (maxval(top_curv (1:5)), maxval(bot_curv (1:5)))

    bump_at_upper_le = (top_curv (2) < top_curv (3))
    bump_at_lower_le = (bot_curv (2) < bot_curv (3))

    if (max_around_le > at_le) then                       ! mean value of max and curv at le  
      best_curv = (max_around_le + at_le) / 2d0
    else if (bump_at_upper_le) then                       ! mean value without bump 
      best_curv = (at_le + top_curv (3)) / 2d0 
    else if (bump_at_lower_le) then 
      best_curv = (at_le + bot_curv (3)) / 2d0 
    else                                                   
      best_curv = at_le
    end if 

    ! print '(3(A,F5.0),2(A,L))', "   Best le curv: ", best_curv, "  le: ", at_le, &
    !         "  max_around_le: ", max_around_le, &
    !         "  bump_upper: ", bump_at_upper_le, "  lower: ", bump_at_lower_le
           
  end function


  function calculate_match_penalties(devi, x, curv, scale_te) result(obj)

    !! Calculate penalties for curve matching - common for bezier and bspline
    !! Inputs:
    !!   devi(:)   - deviation from target at each point
    !!   x(:)      - x coordinates for curvature evaluation
    !!   curv(:)   - curvature at evaluation points
    !!   scale_te  - scale factor for TE penalty (10.0)
    !! Returns:
    !!   obj       - total objective value (sum of all penalties)

    use math_util,            only : norm2p
    use eval_constraints,     only : penalty_reversals, penalty_te_curv, penalty_curv_deriv
    use eval_constraints,     only : penalty_le_curv_monoton

    double precision, intent(in) :: devi(:), x(:), curv(:), scale_te
    double precision :: obj
    
    double precision :: obj_rms, pen_le_monoton, pen_te, pen_curv_deriv, pen_revers
    integer :: nTarg

    nTarg = size(devi)
    
    ! RMS of deviations to target - 0.001 is ok, 0.0002 is good 
    obj_rms = norm2(devi) / sqrt(dble(nTarg)) * 10000d0

    ! LE: if curvature is not monotonous at LE 
    pen_le_monoton = penalty_le_curv_monoton(curv, scale=1d0)

    ! TE: curvature at the very end should be between 0.0 and target_max_te_curv
    pen_te = penalty_te_curv(curv, target_max_te_curv, threshold=0.0d0, scale=scale_te)
        
    ! Penalty for curvature derivative to avoid bumps
    pen_curv_deriv = penalty_curv_deriv(x, curv, threshold_body=1d0, threshold_te=1d0)

    ! Reversals in curvature - avoid bumps
    pen_revers = penalty_reversals(x, curv, 0.2d0, 1.0d0, target_max_reversals, 1d0)

    ! Objective function is sum of single objectives 
    obj = obj_rms + pen_le_monoton + pen_te + pen_curv_deriv + pen_revers

  end function calculate_match_penalties


  function match_curve_objective_function(dv) result (obj) 

    !! Generic objective function to match curve (bezier or bspline) to 'side_to_match'
    !! Curve type and parameters are set by match_bezier/match_bspline via module variables
    !! Uses polymorphism with dispatcher routines for clean code
    
    use math_util,            only : curve_spec_type
    use shape_curve,          only : curve_eval_y_on_x, curve_eval_side
    use shape_curve,          only : map_dv_to_control_points
    use shape_bezier,         only : bezier_spec_type
    use shape_bspline,        only : bspline_spec_type

    double precision, intent(in) :: dv(:)
    double precision :: obj

    class(curve_spec_type), allocatable :: curve  ! Polymorphic: bezier or bspline
    double precision, allocatable :: devi(:), y(:), curv(:), x(:)
    integer                       :: i, nTarg
    logical                       :: is_bot
    
    ! Determine which side we're matching
    is_bot = (target_y(2) < 0d0)
    nTarg = size(target_x)

    ! Allocate specific curve type based on module flag
    if (match_use_bezier) then
      allocate(bezier_spec_type :: curve)
    else
      allocate(bspline_spec_type :: curve)
    end if

    ! Map DV to control points - dispatcher handles type selection
    call map_dv_to_control_points(is_bot, dv, te_gap, curve, target_le_curv)

    ! Calculate deviation from target points - dispatcher handles type selection
    allocate(devi(nTarg))
    do i = 1, nTarg 
      devi(i) = abs(curve_eval_y_on_x(curve, target_x(i), epsilon=1d-8) - target_y(i))
    end do 

    ! Evaluate curve for curvature-based penalties - dispatcher handles type selection
    call curve_eval_side(curve, match_n_eval, x, y, curv, use_arc_length=.false.)

    ! Calculate all penalties using common helper
    obj = calculate_match_penalties(devi, x, curv, match_scale_te)

    ! Eval counter (for debugging) 
    nevals = nevals + 1

  end function match_curve_objective_function


  subroutine match_bezier  (side, le_curv, max_te_curv, max_reversals, ncp, &
                            bezier, result_ok)
   
    !-----------------------------------------------------------------------------
    !! match a bezier curve to one side of an airfoil 
    !!    side:         either 'top' or 'bot' side of an airfoil
    !!    le_curv:      target LE curvature
    !!    max_te_curv:  maximum TE curvature allowed
    !!    max_reversals: maximum number of curvature reversals allowed
    !!    ncp:          number of control points the bezier curve should have 
    !!    bezier:       returns evaluated bezier definition 
    !!    result_ok:    returns .true. if result is acceptable 
    !-----------------------------------------------------------------------------

    use shape_bezier,         only : bezier_spec_type, get_initial_bezier, bezier_get_dv0
    use shape_curve,          only : map_dv_to_control_points
    use simplex_search,       only : simplexsearch, simplex_options_type 
    use eval_constraints,     only : penalty_stats_init

    type (side_airfoil_type), intent(in)    :: side  
    double precision, intent(in)            :: le_curv, max_te_curv
    integer, intent(in)                     :: max_reversals, ncp
    type (bezier_spec_type), intent(out)    :: bezier 
    logical, intent(out)                    :: result_ok

    type (simplex_options_type)     :: sx_options
    double precision, allocatable   :: xopt(:), dv0(:), deviation(:), x(:), curv(:)
    double precision                :: fmin, f0_ref, le_curv_result, te_curv_result
    integer                         :: steps, fevals
    logical                         :: is_bot

    ! Setup targets in module variable for objective function 
    te_gap                = side%y(size(side%y))
    target_le_curv        = le_curv 
    target_max_reversals  = max_reversals
    target_max_te_curv    = max_te_curv
    target_x              = side%x
    target_y              = side%y
    is_bot                = (side%y(2) < 0d0)

    ! Initial estimate for bezier control points based on 'side to match'   
    bezier = get_initial_bezier(target_x, target_y, le_curv, te_gap, ncp)

    ! Nelder mead (simplex) optimization
    sx_options%min_radius     = 1d-5
    sx_options%max_iterations = 4000
    sx_options%initial_step   = 0.16d0

    ! Start vector of design variables dv0 
    dv0 = bezier_get_dv0(is_bot, bezier, c2_coupled=.true.)

    if (show_details) then
      call penalty_stats_init()
      call print_text('Matching '//side%name//' side ('//stri(size(target_x))//'p) with ' &
                      //stri(ncp)//' control points ...', 5)
    end if 

    ! Set curve type and parameters for generic objective function
    match_use_bezier = .true.
    match_n_eval     = 81
    match_scale_te   = 10.0d0

    nevals = 0
    xopt = dv0
    xopt = 0d0
    call simplexsearch(xopt, fmin, steps, fevals, match_curve_objective_function, &
                      dv0, .false., f0_ref, sx_options)

    ! Finished - build bezier, calc deviation at target points  
    call map_dv_to_control_points(is_bot, xopt, te_gap, bezier, le_curv)

    if (show_details) then
      call match_curve_calc_results(bezier, deviation, le_curv_result, te_curv_result, x, curv)
      call match_assess_results(deviation, le_curv, le_curv_result, te_curv_result, &
                               steps, sx_options%max_iterations, x, curv, result_ok)
    end if
 
  end subroutine match_bezier 



  subroutine match_bspline  (side, le_curv, max_te_curv,max_reversals, ncp, &
                             bspline, result_ok)
   
    !-----------------------------------------------------------------------------
    !! match a bspline curve to one side of an airfoil 
    !!    side:         either 'top' or 'bot' side of an airfoil
    !!    le_curv:      target LE curvature
    !!    max_te_curv:  maximum TE curvature allowed
    !!    max_reversals: maximum number of curvature reversals allowed
    !!    ncp:          number of control points the bspline curve should have 
    !!    bspline:      returns evaluated bspline definition 
    !!    result_ok:    returns .true. if result is acceptable 
    !-----------------------------------------------------------------------------

    use shape_bspline,        only : bspline_spec_type, get_initial_bspline, bspline_get_dv0
    use shape_curve,          only : map_dv_to_control_points
    use simplex_search,       only : simplexsearch, simplex_options_type 
    use eval_constraints,     only : penalty_stats_init

    type (side_airfoil_type), intent(in)    :: side  
    double precision, intent(in)            :: le_curv, max_te_curv
    integer, intent(in)                     :: max_reversals, ncp
    type (bspline_spec_type), intent(out)   :: bspline 
    logical, intent(out)                    :: result_ok

    type (simplex_options_type)     :: sx_options
    double precision, allocatable   :: xopt(:), dv0(:), deviation(:), x(:), curv(:)
    double precision                :: fmin, f0_ref, le_curv_result, te_curv_result
    integer                         :: steps, fevals
    logical                         :: is_bot

    ! Setup targets in module variable for objective function 
    te_gap                = side%y(size(side%y))
    target_le_curv        = le_curv 
    target_max_reversals  = max_reversals
    target_max_te_curv    = max_te_curv
    target_x              = side%x
    target_y              = side%y
    is_bot                = (side%y(2) < 0d0)

    ! Initial estimate for bspline control points based on 'side to match'   
    bspline = get_initial_bspline(target_x, target_y, le_curv, te_gap, ncp)

    ! Nelder mead (simplex) optimization
    sx_options%min_radius     = 1d-5
    sx_options%max_iterations = 4000
    sx_options%initial_step   = 0.16d0

    ! Start vector of design variables dv0 
    dv0 = bspline_get_dv0(is_bot, bspline, c2_coupled=.true.)

    if (show_details) then
      call penalty_stats_init()
      call print_text('Matching '//side%name//' side ('//stri(size(target_x))//'p) with ' &
                      //stri(ncp)//' control points ...', 5)
    end if 

    ! Set curve type and parameters for generic objective function
    match_use_bezier = .false.
    match_n_eval     = 81
    match_scale_te   = 10.0d0

    nevals = 0
    xopt = dv0
    xopt = 0d0
    call simplexsearch(xopt, fmin, steps, fevals, match_curve_objective_function, &
                      dv0, .false., f0_ref, sx_options)

    ! Finished - build bspline, calc deviation at target points  
    call map_dv_to_control_points(is_bot, xopt, te_gap, bspline, le_curv)

    if (show_details) then
      call match_curve_calc_results(bspline, deviation, le_curv_result, te_curv_result, x, curv)
      call match_assess_results(deviation, le_curv, le_curv_result, te_curv_result, &
                               steps, sx_options%max_iterations, x, curv, result_ok)
    end if
 
  end subroutine match_bspline 



  subroutine check_airfoil_curvature (curv_constraints, foil, overall_quality)

    !-------------------------------------------------------------------------
    !! Checks curvature quality of foil
    !!   when smooting is active and the quality is not good, smooting will be done
    !!   prints summary of the quality 
    !-------------------------------------------------------------------------
  
    use eval_commons,         only : curv_constraints_type
    use eval_constraints,     only : assess_surface, assess_side
    use airfoil_base,         only : build_from_sides
  
    type (curv_constraints_type), intent (in) :: curv_constraints
    type (airfoil_type), intent (inout)       :: foil
    integer, intent(out)                      :: overall_quality
  
    integer             :: top_quality, bot_quality, istart, iend, iend_spikes
    doubleprecision     :: curv_threshold, spike_threshold
  
    !  ------------ analyze top -----
  
    curv_threshold  = curv_constraints%top%curv_threshold
    spike_threshold = curv_constraints%top%spike_threshold
    istart          = 1
    iend            = size(foil%top%x)
    iend_spikes     = size(foil%top%x)
  
    if (show_details) then 
      call print_colored (COLOR_NOTE, '     Using curv_threshold =')
      call print_colored_r (5,'(F5.2)', Q_OK, curv_threshold) 
      call print_colored (COLOR_NOTE, ', spike_threshold =')
      call print_colored_r (5,'(F5.2)', Q_OK, spike_threshold) 
      call print_colored (COLOR_NOTE, ' for detection')
      write (*,*)
    end if
  
    top_quality = 0
    bot_quality = 0
   
    call assess_side (foil%top, x_start=0.0d0, x_end=1.0d0, curv_constraints=curv_constraints%top, quality=top_quality)
      ! call assess_surface (foil%top, show_details, istart, iend, iend_spikes, &
      !                      curv_threshold, spike_threshold, top_quality)
  
    if (top_quality >= Q_BAD ) then 

      continue          ! #todo - implement match_foil? 
  
    end if
  
    !  ------------ analyze bot -----
  
    if (.not. foil%symmetrical) then 
  
      curv_threshold  = curv_constraints%bot%curv_threshold
      spike_threshold = curv_constraints%bot%spike_threshold
      istart          = 1
      iend            = size(foil%bot%x)
      iend_spikes     = size(foil%bot%x)
  
      if (show_details .and. &
         (curv_threshold  /= curv_constraints%top%curv_threshold  .or. &
          spike_threshold /= curv_constraints%top%spike_threshold)) then 
        write (*,*)
        write (*,'(3x)', advance = 'no') 
        call print_colored (COLOR_NOTE, 'Using curv_threshold =')
        call print_colored_r (5,'(F5.2)', Q_OK, curv_threshold) 
        call print_colored (COLOR_NOTE, ', spike_threshold =')
        call print_colored_r (5,'(F5.2)', Q_OK, spike_threshold) 
        call print_colored (COLOR_NOTE, ' for detection')
        write (*,*)
      end if
  
      call assess_side (foil%bot, x_start=0.0d0, x_end=1.0d0, curv_constraints=curv_constraints%bot, quality=bot_quality)
      ! call assess_surface (foil%bot, show_details, istart, iend, iend_spikes, &
      !                     curv_threshold, spike_threshold, bot_quality)

      if (bot_quality >= Q_BAD ) then 

        continue          ! #todo - implement match foil? 
    
      end if
                          
    else
      bot_quality = top_quality
    end if 
  
    overall_quality = int((top_quality + bot_quality)/2)
    
    ! ... printing stuff 
  
    if (show_details) then
      call print_colored (COLOR_NOTE, repeat(' ',5))
    else
      call print_colored (COLOR_NOTE, '   Airfoil curvature assessment: ')
    end if
  
  
    if (overall_quality < Q_OK) then
      call print_colored (COLOR_NOTE,'The curvature has a ')
      call print_colored (COLOR_GOOD,'perfect')
      call print_colored (COLOR_NOTE,' quality')
    elseif (overall_quality < Q_BAD) then
      call print_colored (COLOR_NOTE,'The curvature quality is ')
      call print_colored (COLOR_NORMAL,'Ok')
    elseif (overall_quality < Q_PROBLEM) then
      call print_colored (COLOR_NOTE,'The curvature quality of the airfoil is not good. ')
      call print_colored (COLOR_WARNING,'Better choose another seed foil ...')
    else
      call print_colored (COLOR_ERROR,' The curvature is not really suitable for optimization')
    end if  
    print *
  
  end subroutine
  
  
  
  subroutine auto_curvature_constraints (side, c_spec)
  
    !! Evaluates and sets the best values for surface thresholds and constraints
  
    use eval_commons,         only: curv_side_constraints_type
    
    type (side_airfoil_type), intent(in)  :: side 
    type (curv_side_constraints_type), intent (inout)  :: c_spec
  
    ! evaluate curvature threshold 
  
    call auto_curvature_threshold_side (side, c_spec)

    ! evaluate spike threshold 
  
    if (c_spec%check_curvature_bumps) then 

      call auto_spike_threshold_side (side, c_spec)

    end if 
    
    ! evaluate max trailing edge curvature 

    call auto_te_curvature_side (side, c_spec)
     
  end subroutine auto_curvature_constraints
  
  
  
  subroutine auto_curvature_threshold_side (side, c_spec)
  
    !! Evaluates the best value for curvature thresholds of polyline
    !!    depending on max_curv_reverse defined by user 
  
    use math_util,            only : min_threshold_for_reversals, count_reversals
    use eval_commons,         only : curv_side_constraints_type
  
    type (side_airfoil_type), intent(in)  :: side 
    type (curv_side_constraints_type), intent (inout)  :: c_spec
  
    double precision    :: min_threshold, max_threshold, curv_threshold
    integer             :: istart, iend, nreversals, quality_threshold, max_curv_reverse
    character(:), allocatable :: info, label
   
    min_threshold = 0.01d0
    max_threshold = 4.0d0
  
    max_curv_reverse = c_spec%max_curv_reverse
    curv_threshold   = c_spec%curv_threshold
  
    ! How many reversals do we have now? more than user specified as a constraint?
  
    istart = 1
    iend   = size(side%x)
    nreversals = count_reversals (istart, iend, side%curvature, curv_threshold)
  
    if (nreversals > max_curv_reverse) then
      print * 
      call print_warning (stri(nreversals) // ' reversal(s) on '//side%name//' side'//&
                          ' - but max_curv_reverse is set to '//stri(max_curv_reverse), 5)
      call print_note ('This will lead to a high curvature threshold value to fulfil this constraint.', 5)
      call print_note ('Increase curv_threshold or choose another seed airfoil which fits to the reversal constraints.', 5)
      print *  
    end if 

    ! now get smallest threshold for max_reversals defined by user 
  
    curv_threshold = min_threshold_for_reversals (istart, iend, side%curvature , &
                               min_threshold, max_threshold, max_curv_reverse)
    
    ! ... and give a little more threshold to breeze
    curv_threshold = curv_threshold * 1.1d0     
  
    c_spec%curv_threshold = curv_threshold
  
    ! Print it all 
  
    if (show_details) then 

      call print_text (side%name// " side   ",5, no_crlf=.true.)

      quality_threshold  = r_quality (curv_threshold, 0.02d0, 0.10d0, 0.3d0)
      label = 'curv_threshold'
      call print_colored (COLOR_PALE, label//'   =') 
      call print_colored_r (5,'(F5.2)', quality_threshold, curv_threshold) 
      if (quality_threshold > Q_BAD) then
        call print_text ('The contour will have some reversals within this high treshold', 3)
      else
        if (max_curv_reverse == 0) then 
          info = "no"
        else
          info = stri(max_curv_reverse)
        end if 
        call print_text ('Lowest value for '//info//' reversals', 3)
      end if 
    end if 
  
  end subroutine auto_curvature_threshold_side
  
  
  
  subroutine auto_spike_threshold_side (side, c_spec)
  
    !! Evaluates the best value for curvature thresholds of polyline
    !!    depending on spike_threshold defined by user 
  
    use math_util,              only : count_reversals, derivative1, min_threshold_for_reversals
    use eval_commons,           only : curv_side_constraints_type
  
    type (side_airfoil_type), intent(in)  :: side 
    type (curv_side_constraints_type), intent (inout)  :: c_spec
  
    double precision          :: spike_threshold
    integer                   :: istart, iend, nspikes, quality_threshold
    character(:), allocatable :: label, text_who
  
    double precision, parameter    :: OK_THRESHOLD  = 0.5d0
    double precision, parameter    :: MIN_THRESHOLD = 0.2d0
    double precision, parameter    :: MAX_THRESHOLD = 1.0d0
  
    spike_threshold = c_spec%spike_threshold
  
    ! How many Spikes do we have with current threshold defined by user / default?
  
    istart = 1
    iend   = size(side%x)
    nspikes = count_reversals (istart, iend, derivative1 (side%x, side%curvature), spike_threshold)
    ! print *, '------ ', istart, iend , spike_threshold, nspikes
  
    ! now get smallest threshold to achieve this number of spikes
  
    spike_threshold = min_threshold_for_reversals (istart, iend, derivative1 (side%x, side%curvature), &
                      min_threshold, max_threshold, nspikes)
  
    ! ... and give a little more threshold to breeze
  
    c_spec%spike_threshold = max (spike_threshold * 1.1d0, spike_threshold + 0.03)
  
    ! Max Spikes - allow at least max_curv_reverse or seed spikes as max number of spikes 
  
    if (c_spec%max_spikes == 0 )  then 
  
    ! Do not take no of reversals as minimum number of spikes
    !    c_spec%max_spikes = max (nspikes, c_spec%max_curv_reverse)
      c_spec%max_spikes = nspikes
      text_who = 'Auto:'
    else
    ! ... overwrite from input file by user? 
      text_who = 'User:'
    end if 
  
    ! Print it all 
  
    if (show_details) then 
      
      call print_text ("",16, no_crlf=.true.)

      quality_threshold  = r_quality (c_spec%spike_threshold, (MIN_THRESHOLD + 0.035d0), OK_THRESHOLD, 0.8d0)
      label = 'spike_threshold'
      call print_colored (COLOR_PALE, label//'  =') 
      call print_colored_r (5,'(F5.2)', quality_threshold, c_spec%spike_threshold) 
  
      if (c_spec%max_spikes == 0) then 
        call print_colored (COLOR_NOTE, '   There will be no spikes or bumps.')
      else
        call print_colored (COLOR_NOTE, '   '//text_who//' There will be max '//stri(c_spec%max_spikes) //' spike(s) or bump(s).')
      end if
      write (*,*)
    end if 
  
  end subroutine 
  
  
  
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
  
    ! recognize if user changed value  to allow a defined te curvature 
    ! although auto_curvature is active 
    if (c_spec%max_te_curvature == 4.9999d0) then 
    
      auto = .true.
  
      max_curv = max_curvature_at_te (side%curvature)  
    ! give a little more to breath during opt.
      max_curv = max ((max_curv * 1.1d0), (max_curv + 0.05d0))
      c_spec%max_te_curvature = max_curv
  
    else 
  
      auto = .false.
    
    end if
  
    ! Print it all 
  
    if (show_details) then 

      call print_text ("",16, no_crlf=.true.)

      quality_te      = r_quality (c_spec%max_te_curvature, 0.2d0, 1d0, 10d0)
      label = 'max_te_curvature'
      call print_colored (COLOR_PALE, label//' =') 
      call print_colored_r (5,'(F5.2)', quality_te, c_spec%max_te_curvature) 
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
  
  
  
  subroutine  auto_check_curvature_bumps_side (side, c_spec)
  
    !! Print info about check_curvature  
    !!     - activate bump_detetction if possible 
  
    use eval_commons,         only : curv_side_constraints_type
    use math_util,            only : count_reversals, derivative1
  
    type (side_airfoil_type), intent(in)  :: side 
    type (curv_side_constraints_type), intent (inout)  :: c_spec
  
    double precision, parameter    :: OK_THRESHOLD  = 0.5d0
    integer, parameter             :: OK_NSPIKES    = 5
  
    integer :: istart, iend 
    integer :: nspikes
    character(:), allocatable :: info
  
    info = side%name // " side"
    istart = 1
    iend   = size(side%x)
  

    ! How many spikes = Rversals of 3rd derivation = Bumps of curvature
    nspikes = count_reversals (istart, iend, derivative1 (side%x, side%curvature), c_spec%spike_threshold)
  
    ! activate bump detection (reversal of 3rd derivative) if values are ok
  
    if (c_spec%spike_threshold <= OK_THRESHOLD .and. &
        c_spec%max_spikes <= OK_NSPIKES .and. &
        nspikes <= c_spec%max_spikes) then 
  
      c_spec%check_curvature_bumps = .true.
  
    else
      c_spec%check_curvature_bumps = .false.
    end if
     
    if (show_details) then 
      call print_text (side%name// " side   ",5, no_crlf=.true.)

      if (c_spec%check_curvature_bumps) then 
        call print_note ("Bump detection for max "//stri(c_spec%max_spikes)//' bump(s).',0)
      else
        call print_note ("No Bump detection - spike values not good enough for detetction",0)
      end if
    end if 
  
  end subroutine 
  
    

  subroutine  check_handle_curve_violations (side, c)
  
    !! Checks surface x,y for violations of curvature contraints 
    !!     reversals > max_curv_reverse and handles user response  
  
    use math_util,            only : derivative1, count_reversals
    use eval_commons,         only : curv_side_constraints_type
  
    type (side_airfoil_type), intent(in)  :: side 
    type (curv_side_constraints_type), intent (inout)  :: c
  
    integer :: n, max_rev, nreverse_violations, istart, iend, nreverse 
    integer :: max_spikes, nspikes, nspike_violations
    character(:), allocatable :: info
  
    info = side%name // " side"
  
    istart = 1
    iend   = size(side%x)
  
    ! How many reversals?  ... 
    nreverse = count_reversals (istart, iend, side%curvature, c%curv_threshold)  
    nreverse_violations  = max(0,(nreverse - c%max_curv_reverse))
  
    ! How many spikes = Rversals of 3rd derivation = Bumps of curvature
    if (c%check_curvature_bumps) then 
      nspikes = count_reversals (istart, iend, derivative1 (side%x, side%curvature), c%spike_threshold)
      nspike_violations  = max(0,(nspikes - c%max_spikes))
    else
      nspike_violations  = 0
    end if
  
  
    ! Exit if everything is ok 
    if (nreverse_violations > 0 .or. nspike_violations > 0) then 
  
      write (*,*)
      call print_warning ("Curvature violations on " // trim(info))
      write (*,*)
  
      if (nreverse_violations > 0) then 
        n       = nreverse_violations + c%max_curv_reverse
        max_rev = c%max_curv_reverse
        write (*,'(10x,A,I2,A,I2)')"Found ",n, " Reversal(s) where max_curv_reverse is set to ", max_rev
      end if 
  
      if (nspike_violations > 0) then 
        n          = nspike_violations + c%max_spikes
        max_spikes = c%max_spikes
        write (*,'(10x,A,I2,A,I2)')"Found ",n, " Spikes(s) or bumps where max_spikes is set to ", max_spikes
      end if 
  
      write (*,*)
      write (*,'(10x,A)') 'Either increase max_... or ..._threshold (not recommended) or'
      write (*,'(10x,A)') 'choose another seed airfoil. Have a look at the curvature with the AirfoilEditor.'
      call my_stop ('The Optimizer may not find a solution with this inital curvature violation.')
    end if
  
  end subroutine check_handle_curve_violations
  
  
  
  subroutine  check_te_curvature_violations (side, c)
  
    !! Checks trailing edga curvature x,y for violations max_te_crvature
    !! and handles user response  
  
    use airfoil_geometry,     only : max_curvature_at_te
    use eval_commons,         only : curv_side_constraints_type
    use math_util,            only : count_reversals
  
    type (side_airfoil_type), intent(in)  :: side 
    type (curv_side_constraints_type), intent (inout)  :: c 
  
    double precision  :: cur_te_curvature
    character(:), allocatable :: info
  
    info = side%name // " side"
  
    cur_te_curvature = max_curvature_at_te (side%curvature)
    if (cur_te_curvature  > c%max_te_curvature) then 
      call my_stop("Curvature of "//strf('(F6.2)', cur_te_curvature)// &
                    " on "//trim(info)//" at trailing edge violates max_te_curvature constraint.")
    end if 
  
  end subroutine check_te_curvature_violations
  
  
end module 
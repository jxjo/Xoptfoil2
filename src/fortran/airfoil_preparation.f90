! MIT License
! Copyright (c) 2024 Jochen Guenzel

!
! Preparing seed airfoil prior to optimization 
!

module airfoil_preparation
  
  use os_util
  use print_util
  use commons

  use airfoil_base,  only : airfoil_type, side_airfoil_type, panel_options_type

  implicit none
  private

  public :: get_airfoil
  public :: prepare_seed_foil
  public :: prepare_match_foil
  public :: transform_to_bezier_based
  public :: match_bezier, match_get_best_le_curvature

  public :: check_airfoil_curvature, auto_curvature_constraints

  ! --------- private --------------------------------------------------------

  ! static vars for match bezier objective function 

  type(side_airfoil_type)       :: side_to_match
  double precision, allocatable :: match_target_x(:)        ! target coordinates of side_to_match
  double precision, allocatable :: match_target_y(:) 
  double precision              :: target_le_curv           ! target le curvature 
  double precision              :: target_le_curv_weighting ! target le curvature weighting within objectiveFn
  double precision              :: target_te_curv_max       ! max te curvature 
  double precision              :: te_gap                   ! ... needed for bezier rebuild 
  integer                       :: nevals = 0               ! number of evaluations 

contains

  subroutine prepare_seed_foil (filename, eval_spec, shape_spec, seed_foil)

    !-----------------------------------------------------------------------------
    !! Read and prepare seed airfoil to be ready for optimization 
    !-----------------------------------------------------------------------------

    use airfoil_geometry,     only : normalize, repanel_bezier, repanel_and_normalize
    use eval_commons,         only : eval_spec_type
    use shape_airfoil,        only : shape_spec_type
    use shape_airfoil,        only : BEZIER, HICKS_HENNE
    use shape_bezier,         only : ncp_to_ndv
    use shape_hicks_henne,    only : nfunctions_to_ndv

    use airfoil_base    
  
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

    if (original_foil%is_bezier_based) then        ! Bezier is already normalized
      call repanel_bezier (original_foil, seed_foil, eval_spec%panel_options)
    else if (original_foil%is_hh_based) then       ! Hicks-Henne foils is already normalized
      seed_foil = original_foil                    ! keep paneling 
    else
      call repanel_and_normalize (original_foil, seed_foil, eval_spec%panel_options) 
    end if


    ! symmetrical? 

    if (eval_spec%geo_constraints%symmetrical)  call make_symmetrical (seed_foil)
  

    ! preset airfoil to geo targets 
    !   - currently not possible if seed airfoil is .bez
    !   - not made if seed airfoil is hicks-henne based to retain original seed 

    if (.not. (shape_spec%type == BEZIER      .and. seed_foil%is_bezier_based) .and. &
        .not. (shape_spec%type == HICKS_HENNE .and. seed_foil%is_hh_based)) then                         ! currently only HH - #todo

      call preset_airfoil_to_targets (eval_spec%geo_targets, seed_foil)

    end if 
  

    ! Prepare Airfoil based on optimization shape type  
  
    if (shape_spec%type == BEZIER) then 
  
      if (seed_foil%is_bezier_based) then 
  
          ! ignore 'bezier_options' - take seed bezier definition  
          shape_spec%bezier%ncp_top = size(seed_foil%top_bezier%px)
          shape_spec%bezier%ncp_bot = size(seed_foil%bot_bezier%px)
          shape_spec%bezier%ndv     = ncp_to_ndv (shape_spec%bezier%ncp_top) + & 
                                      ncp_to_ndv (shape_spec%bezier%ncp_bot)
  
          call print_note ("Using Bezier control points from seed airfoil. "// &
                          "Values in 'bezier_options' will be ignored", 3)
      else
  
        ! a new bezier "match foil" is generated to be new seed 
        seed_foil%name = seed_foil%name // '-bezier' //stri(shape_spec%bezier%ncp_top) //&
                                                       stri(shape_spec%bezier%ncp_bot)
  
        call transform_to_bezier_based (shape_spec%bezier, eval_spec%panel_options, seed_foil)
  
      end if

    else if (shape_spec%type == HICKS_HENNE) then

      if (seed_foil%is_hh_based) then 
  
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
        call transform_to_bezier_based (shape_spec%bezier, eval_spec%panel_options, seed_foil)
        seed_foil%name = seed_foil%name // '-smoothed'
        seed_foil%is_bezier_based = .true. 
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

    use airfoil_geometry,     only : normalize, repanel_bezier, repanel_and_normalize, te_gap
    use airfoil_geometry,     only : EPSILON
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

    call match_get_targets (match_foil%top, match_foil_spec%target_top_x, &
                                            match_foil_spec%target_top_y)

    call match_get_targets (match_foil%bot, match_foil_spec%target_bot_x, &
                                            match_foil_spec%target_bot_y)
  end subroutine



  subroutine get_airfoil (filename, foil, silent_mode )

    !-----------------------------------------------------------------------------------
    !! loads either .dat or .bez file into 'foil' 
    !-----------------------------------------------------------------------------------

    use airfoil_base,       only : is_dat_file
    use shape_bezier,       only : load_bezier_airfoil, is_bezier_file
    use shape_hicks_henne,  only : load_hh_airfoil, is_hh_file
    use shape_airfoil,      only : build_from_hh_seed
    use airfoil_base,       only : airfoil_load, split_foil_into_sides

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

      foil%is_bezier_based = .true.
      np = 201                              ! 201 points as default - will be repaneled anyway

      call load_bezier_airfoil (filename, np, foil%name, foil%x, foil%y, foil%top_bezier, foil%bot_bezier) 

      
    else if (is_hh_file (filename)) then

      ! Read seed hh functions and coordinates from .hicks file and generate airfoil 
      if (.not. silent) call print_action ('Reading Hicks-Henne file', filename)
  
      foil%is_hh_based = .true.
  
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
    use eval_constraints,     only : eval_geometry_violations, eval_curvature_violations
    use math_util,            only : norm_2

    use shape_airfoil,        only : shape_spec_type, HICKS_HENNE, BEZIER


    type (airfoil_type), intent(inout)          :: seed_foil
    type (shape_spec_type), intent(inout)       :: shape_spec
    type (curv_constraints_type), intent(inout) :: curv_constraints
    type (geo_constraints_type), intent(in)     :: geo_constraints
    type (xfoil_options_type), intent(in)       :: xfoil_options
    
    double precision :: penaltyval
    double precision :: pi
    integer          :: nptt, nptb, overall_quality
    logical          :: has_violation 
    character (100)  :: violation_text

    penaltyval = 0.d0
    pi = acos(-1.d0)

    ! Check curvature constraints like reversals -------------------------

    if(curv_constraints%check_curvature) then

      if (shape_spec%type == HICKS_HENNE) then

        call print_action ("Analyzing curvature if it's suitable for Hicks-Henne shape type")

        call check_airfoil_curvature (curv_constraints, seed_foil, overall_quality)

      end if 

      ! Get best values fur surface constraints 

      if (curv_constraints%auto_curvature) then 

        call print_action ("Auto_curvature: Evaluate best values of curvature constraints based on seed")

        call auto_curvature_constraints (seed_foil%top, curv_constraints%top)
        if (.not. seed_foil%symmetrical) & 
          call auto_curvature_constraints (seed_foil%bot, curv_constraints%bot)

        ! le curvature difference check only Bezier 

        if (shape_spec%type == BEZIER) then
            call auto_le_curvature_diff (seed_foil, curv_constraints%max_le_curvature_diff)
        end if 

      end if

      ! Bump detection only for Hicks Henne 

      if (shape_spec%type == HICKS_HENNE ) then
        call print_action ("Check_curvature_bumps: Activate bump detection if possible")

        call auto_check_curvature_bumps_side (seed_foil%top, curv_constraints%top)
        if (.not. seed_foil%symmetrical) & 
          call auto_check_curvature_bumps_side (seed_foil%bot, curv_constraints%bot)
      else
        curv_constraints%top%check_curvature_bumps = .false.
        curv_constraints%bot%check_curvature_bumps = .false.
      end if 

      ! Final check for curvature reversals

      call check_handle_curve_violations (seed_foil%top, curv_constraints%top)
      if (.not. seed_foil%symmetrical) & 
        call check_handle_curve_violations (seed_foil%bot, curv_constraints%bot)

      ! Final check trailing edge 

      call check_te_curvature_violations (seed_foil%top, curv_constraints%top)
      if (.not. seed_foil%symmetrical) & 
        call check_te_curvature_violations (seed_foil%bot, curv_constraints%bot)

    end if

    ! Check geometry ------------------------------------------------------------------

    nptt = size(seed_foil%top%x)
    nptb = size(seed_foil%bot%x)

    ! init xfoil - will be used also for geometry

    call xfoil_defaults (xfoil_options)


    if (geo_constraints%check_geometry) then

      call print_action ('Checking seed airfoil passes all geometry constraints ... ', no_crlf = .true.)

      call eval_geometry_violations (seed_foil, geo_constraints, has_violation, violation_text)

      if (has_violation) then 
        print * 
        print *
        call print_error (trim(violation_text), 5)
        call print_note ("Please adapt this geometry constraint to seed airfoil", 5)
        call my_stop ("Seed airfoil doesn't meet a geometry constraint") 
      else
        if (show_details) then 
          call print_colored(COLOR_GOOD, "Ok")
          print * 
        end if 
      end if 

    end if 

    if (curv_constraints%check_curvature) then

      call print_action ('Checking seed airfoil passes all curvature constraints ... ', no_crlf = .true.)

      call eval_curvature_violations (seed_foil, curv_constraints, has_violation, violation_text)

      if (has_violation) then 
        print * 
        call my_stop (trim(violation_text)) 
      else
        if (show_details) then 
          call print_colored(COLOR_GOOD, "Ok")
          print * 
        end if 
      end if 

    end if 

  end subroutine 



  subroutine preset_airfoil_to_targets (geo_targets, foil) 

    !-----------------------------------------------------------------------------
    !! Set airfoil thickness and camber according to defined geo targets 
    !!   and/or thickness/camber constraints (in airfoil evaluation commons)
    !-----------------------------------------------------------------------------

    use airfoil_geometry,         only: set_geometry, set_te_gap, te_gap
    use eval_commons,             only: geo_target_type

    type (geo_target_type), intent (in)   :: geo_targets (:)
    type (airfoil_type), intent (inout)   :: foil

    doubleprecision     :: new_camber, new_thick, cur_te_gap
    integer             :: i, ngeo_targets

    ngeo_targets = size(geo_targets)

    new_thick  = -1d0
    new_camber = -1d0

    if (ngeo_targets > 0) then 

      do i= 1, ngeo_targets

        if (geo_targets(i)%preset_to_target) then 
          select case (geo_targets(i)%type)

            case ('thickness')                   

              new_thick  = geo_targets(i)%target_value
              cur_te_gap = te_gap (foil)
              call print_action ('Preset thickness to target value ', strf('(F6.4)', new_thick))
              call set_geometry (foil, maxt=new_thick)

              ! retain te gap (set thickness changed it)

              if (cur_te_gap > 0d0) then 
                call print_action ('Retain trailing edge gap at ', strf('(F5.2)', cur_te_gap*1d2)//'%')
                call set_te_gap (foil, cur_te_gap)
              end if 

            case ('camber')                      

              new_camber = geo_targets(i)%target_value
              call print_action ('Preset camber to target value ', strf('(F6.4)', new_camber))
              call set_geometry (foil, maxc=new_camber)

          end select
        end if 

      end do

    end if

    if (new_thick /= -1d0 .or. new_camber /= -1d0) then
      foil%name = foil%name // "-preset" 
    end if 

  end subroutine 


!-----------------------------------------------------------------------------


  subroutine transform_to_bezier_based (shape_bezier, panel_options, foil)

    !-----------------------------------------------------------------------------
    !! Transform foil to bezier based using simplex optimization for best fit 
    !! of bezier curves on top and bot side.
    !! - write .dat and .bez file 
    !-----------------------------------------------------------------------------

    use airfoil_base,         only : split_foil_into_sides, is_normalized_coord
    use airfoil_geometry,     only : te_gap 

    use shape_bezier,         only : shape_bezier_type
    use shape_bezier,         only : bezier_create_airfoil, write_bezier_file
    use shape_bezier,         only : bezier_spec_type

    type (shape_bezier_type), intent (inout) :: shape_bezier
    type (panel_options_type), intent (in)   :: panel_options
    type (airfoil_type), intent (inout)      :: foil

    type (bezier_spec_type)   :: top_bezier, bot_bezier
    double precision          :: best_le_curv, weighting
    logical                   :: result_ok
    integer                   :: i

    ! Sanity check 
  
    if (.not. is_normalized_coord(foil)) then 
      call my_stop ('Airfoil is not normalized prior to Bezier transform')
    else
      ! ensure top and bot side do exist in foil   
      call split_foil_into_sides (foil) 
    end if  

    call print_action ("Create Bezier based airfoil")

    ! Simplex optimization (nelder mead) for both sides  - increase weight for le curvature diff 

    best_le_curv = match_get_best_le_curvature (foil)
    weighting = 0.5d0 

    do i = 1, 4
      call match_bezier  (foil%top, best_le_curv, weighting * i, shape_bezier%ncp_top, top_bezier, result_ok)
      if (result_ok) exit
    end do 

    do i = 1, 4
      call match_bezier  (foil%bot, best_le_curv, weighting* i , shape_bezier%ncp_bot, bot_bezier, result_ok)
      if (result_ok) exit
    end do 

    ! build airfoil out of Bezier curves 

    call bezier_create_airfoil (top_bezier, bot_bezier, panel_options%npoint, foil%x, foil%y)

    call split_foil_into_sides (foil)                 ! prepare for further need 
    foil%top_bezier = top_bezier
    foil%bot_bezier = bot_bezier
    foil%is_bezier_based = .true.

  end subroutine



  subroutine match_set_targets (side)

    !! set the target coordinates and te curvature in static module variables 
    !! for objective function evaluation

    type (side_airfoil_type), intent(in)    :: side  

    ! get the coordinates of the match checks points   

    call match_get_targets (side, match_target_x, match_target_y)

    ! define target le curvature based on target side 

    if (allocated(side%curvature)) then 

      target_te_curv_max = side%curvature (size(side%curvature))
      ! print *, side%name, "ntarg ", ntarg, "  curv te", target_te_curv_max
    else 
      target_te_curv_max = 0.5d0
    end if 

  end subroutine



  subroutine match_get_targets (side, target_x, target_y)

    !-----------------------------------------------------------------------------
    !! get the target coordinates of a side for match foil 
    !-----------------------------------------------------------------------------

    use math_util,            only : find_closest_index
    type (side_airfoil_type), intent(in)       :: side  
    double precision, allocatable, intent(out) :: target_x(:), target_y(:)

    integer           :: i, nTarg, iTarg
    double precision  :: x1, x2, x3, dx1, dx2, dx3, x

    ! based on delta x
    ! we do not take every coordinate point - define different areas of point intensity 

    x1  = 0.02d0 ! 0.03d0                                     ! a le le curvature is master 
    dx1 = 0.04d0 ! 0.025d0                                    ! higher density at nose area

    x2  = 0.25d0                                              ! low density in the middle of chord 
    dx2 = 0.04d0

    x3  = 0.8d0                                               ! no higher density at te
    dx3 = 0.03d0 !0.03d0                                      ! to handle reflexed or rear loading

    ! evaluate number of target coordinates 

    x = x1
    nTarg  = 0 
    do while (x < 1.0d0) 
      nTarg = nTarg + 1
      if (x > x3) then                             
          x = x + dx3
      else if (x > x2) then                             
        x = x + dx2
      else 
        x = x + dx1
      end if
    end do 

    ! build target coordinate arrays 

    allocate (target_x(nTarg))
    allocate (target_y(nTarg))

    iTarg  = 0 
    x = x1
    do while (x < 1.0d0) 
      
      i = find_closest_index (side%x, x)

      iTarg = iTarg + 1
      target_x(iTarg) = side%x(i)
      target_y(iTarg) = side%y(i)

      if (x > x3) then                              
        x = x + dx3
      else if (x > x2) then                             
        x = x + dx2
      else 
        x = x + dx1
      end if
  end do 

  ! print *, size(target_x), size(side%x)
  ! print '(100F7.5)', target_x
  ! print '(100F7.5)', target_y 

  end subroutine




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


  function match_bezier_objective_function(dv) result (obj) 

    !! objective function to match bezier to 'side_to_match'
    
    use math_util,            only : linspace, derivative1, count_reversals, norm2p
    use shape_bezier,         only : bezier_spec_type, bezier_eval_1D, print_bezier_spec
    use shape_bezier,         only : bezier_violates_constraints, bezier_eval_y_on_x
    use shape_bezier,         only : bezier_curvature, map_dv_to_bezier

    double precision, intent(in) :: dv(:)
    double precision :: obj

    type (bezier_spec_type)       :: bezier
    double precision, allocatable :: devi (:), u(:), curv(:), x(:), deriv1 (:)
    double precision              :: te_curv, delta, diff
    double precision              :: obj_norm2, obj_le, obj_te, obj_te_deriv, obj_revers
    double precision              :: max_curv_deriv_te, lim_curv_deriv_te
    integer                       :: i, nTarg, n
    
    obj          = 0d0
    obj_norm2    = 0d0
    obj_le       = 0d0
    obj_te       = 0d0
    obj_te_deriv = 0d0
    obj_revers   = 0d0

    ! remap bezier control points out of design variables 

    call map_dv_to_bezier (side_to_match%name, dv, te_gap, bezier)

    ! print '(I5,100F8.4)', nevals, dv
    ! call print_bezier_spec (nevals,'', bezier)

    ! sanity check - nelder mead could have crossed to bounds or changed to order of control points 


    ! calc deviation bezier to target points 

    nTarg = size(match_target_x)
    allocate (devi(nTarg))

    do i = 1, nTarg 
      devi(i) = abs (bezier_eval_y_on_x (bezier, match_target_x(i), epsilon=1d-8) - match_target_y(i))
    end do 
    
    ! norm2 of deviations to target - 0.001 is ok, 0.0002 is good 
    
    obj_norm2 = norm2 (devi) * 1000d0                 ! -> 1.0 is ok, 0.2 is good 

    ! add curvature at LE and TE to objective 

    if (allocated(side_to_match%curvature)) then 

      ! LE: deviation to target curvature (see set targets) 
      !     It should be highly weighted to have a good quality at le 
      !     the seed airfoil

      diff = abs (target_le_curv - abs(bezier_curvature(bezier, 0d0)))
      obj_le = (diff / 40d0) * target_le_curv_weighting      ! empirical value to norm2-devi 


      ! TE: take curvature at the very end which should be between target and 0.0 
      !     Only outlier should influence objective - do not try to force curvature to a value
      !     as resulting Bezier will be not nice ...

      ! curvature on bezier side_upper is negative !
      if (bezier_curvature(bezier, 0d0) < 0d0) then 
        te_curv    = - bezier_curvature(bezier, 1d0)
      else
        te_curv    = bezier_curvature(bezier, 1d0)
      end if 

      !current should be between 0.0 and target te curvature
      if (target_te_curv_max >= 0.0) then 
        if (te_curv >= 0.0) then 
          delta = te_curv - target_te_curv_max
        else
          delta = - te_curv * 3d0              ! te curvature shouldn't result in reversal 
        end if 
      else 
        if (te_curv < 0.0) then  
          delta = - (te_curv - target_te_curv_max)
        else
          delta = te_curv * 3d0                ! te curvature shouldn't result in reversal 
        end if 
      end if 
       
      if (delta > 0.1d0) then
        obj_te = delta - 0.1d0                ! add empirical  delta     
      end if 
        
    end if 

    ! calculate derivative of curvature for detection of curvature artefacts 

    u = linspace (0.2d0, 1d0, 100)
    n = size(u)
    allocate (x(n))
    allocate (curv(n))
    do i = 1, n 
      x(i)    = bezier_eval_1D (bezier%px, u(i))   
      curv(i) = bezier_curvature (bezier, u(i))   
    end do 
    deriv1 = derivative1 (x,curv)

    ! derivative of curvature at te 
    !	  try to avoid that curvature slips away at TE when control point 
    !   is getting closer to TE 


    max_curv_deriv_te = maxval(abs(deriv1(n-10:n)),1)           ! take absolute max of derivative
    lim_curv_deriv_te = abs(target_te_curv_max) * 10d0          ! deriv is approx 10 * curvature
    lim_curv_deriv_te = max (lim_curv_deriv_te, 1d0)            ! allow some derivation

    if (max_curv_deriv_te > lim_curv_deriv_te) then 
      obj_te_deriv = (max_curv_deriv_te - lim_curv_deriv_te) / 20d0   ! only small weight 
      ! print *, nevals, max_curv_deriv_te , lim_curv_deriv_te, obj_te_deriv
    end if 

    ! reversals in derivative of curvature - avod bumps

    obj_revers = 0.4d0 * (count_reversals (1, n, deriv1, 0.02d0)) ** 2


    ! objective function is sum of single objectives 
    ! take norm2 of deviation an le curvature to get balanced result 

    obj = norm2p (obj_norm2, obj_le) + obj_te + obj_te_deriv + obj_revers
    ! obj = obj_norm2 + obj_le + obj_te + obj_te_deriv + obj_revers

    ! testing 
    ! if (mod(nevals, 100) == 0) then 
    !   if (nevals == 0) print *
    !   print '(I4, 6(2x, A,F5.2))', nevals, " obj:", obj, " norm2:", obj_norm2, " le:",obj_le, &
    !                           " te:",obj_te, " te_der:",obj_te_deriv, " rev:", obj_revers
    ! end if 

    ! eval counter (for debugging) 

    nevals = nevals + 1

  end function match_bezier_objective_function



  subroutine match_bezier  (side, le_curv, le_curv_weighting, ncp, bezier, result_ok)
   
    !-----------------------------------------------------------------------------
    !! match a bezier curve to one side of an airfoil 
    !!    side:  either 'top' or 'bot' of an airfoil
    !!    le_curv:  le curvature which should be achieved
    !!    le_curv_weighting:  weighting of le_curv within objectiveFn
    !!    ncp:   number of control points the bezier curve should have 
    !!    bezier:  returns evaluated bezier definition 
    !!    result_ok: returns .true. if result is acceptable 
    !-----------------------------------------------------------------------------

    use shape_bezier
    use simplex_search,       only : simplexsearch, simplex_options_type 

    type (side_airfoil_type), intent(in)    :: side  
    double precision, intent(in)            :: le_curv, le_curv_weighting
    integer, intent(in)                     :: ncp
    type (bezier_spec_type), intent(out)    :: bezier 
    logical, intent(out)                    :: result_ok

    type (simplex_options_type)     :: sx_options
    double precision, allocatable   :: xopt(:), dv0(:), deviation(:)
    double precision                :: fmin, f0_ref, dev_max, dev_norm2, dev_max_at, te_curv_result
    integer                         :: steps, fevals, ndv, i, nTarg, le_curv_result
    integer                         :: how_good_dev, how_good_le, le_curv_diff
    

    ! setup targets in module variable for objective function 

    nevals = 0 
    side_to_match = side
    te_gap = side%y(size(side%y))
    target_le_curv = le_curv 
    target_le_curv_weighting = le_curv_weighting 

    call match_set_targets (side)

    ! initial estimate for bezier control points based on 'side to match'   

    call get_initial_bezier (side%name, match_target_x, match_target_y, te_gap, ncp, bezier)
    ! call get_initial_bezier (side%x, side%y, ncp, bezier)

    ! nelder mead (simplex) optimization

    sx_options%min_radius     = 1d-5
    sx_options%max_iterations = 4000
    sx_options%initial_step   = 0.16d0 ! 0.16d0        ! seems to be best value - strong relation to bezier bounds

    ! --- start vector of design variables dv0 

    dv0 = bezier_get_dv0 (side%name, bezier)

    ndv = size(dv0)

    if (show_details) then
      call print_text ('Matching '//side%name//' side ('//stri(ncp)//' points): ', 5, no_crlf=.true.)
    end if 

    nevals = 0                                      ! counter in objective function 
    xopt = dv0                                      ! just for allocation 
    xopt = 0d0                                      ! result array 
    call simplexsearch(xopt, fmin, steps, fevals, match_bezier_objective_function, &
                      dv0, .false. , f0_ref, sx_options)


    ! --- finished - build bezier, calc deviation at target points  

    call map_dv_to_bezier (side%name, xopt, te_gap, bezier)

    if (show_details) then

      nTarg = size(match_target_x)
      allocate (deviation(nTarg))

      do i = 1, nTarg 
        deviation(i) = abs (bezier_eval_y_on_x (bezier, match_target_x(i), epsilon=1d-8) - match_target_y(i))
      end do 

      dev_norm2       = norm2 (deviation)
      dev_max         = maxval(deviation)
      dev_max_at      = match_target_x(maxloc (deviation,1))
      le_curv_result  = int(bezier_curvature (bezier, 0d0))
      le_curv_diff    = abs(int(le_curv)-le_curv_result)
      te_curv_result  = bezier_curvature (bezier, 1d0)

      how_good_dev = r_quality (dev_norm2, 0.001d0, 0.002d0, 0.005d0) 
      how_good_le  = i_quality (le_curv_diff, 4, 20, 50) 

      call  print_colored   (COLOR_NOTE, stri(steps,4)//' iter')
      call  print_colored   (COLOR_NOTE, ', deviation: ')
      call  print_colored_r (8, '(f8.5)', how_good_dev, dev_norm2) 
      call  print_colored   (COLOR_NOTE, ', le curvature diff:')
      call  print_colored_i (3, how_good_le, le_curv_diff) 
      call  print_colored   (COLOR_NOTE, '  (weight '//strf('(f3.1)', le_curv_weighting)//')')

      ! call  print_colored   (COLOR_NOTE, strf('(f8.2)',target_te_curv_max,.true.))
      ! call  print_colored   (COLOR_NOTE, strf('(f8.2)',te_curv_result,.true.))

      if (steps == sx_options%max_iterations) then
        call print_colored (COLOR_WARNING, ' Max iter reached')
      end if
      print *
    end if 

    result_ok = (how_good_dev <= Q_OK) .and. (how_good_le == Q_GOOD)
 
  end subroutine 



  subroutine check_airfoil_curvature (curv_constraints, foil, overall_quality)

    !-------------------------------------------------------------------------
    !! Checks curvature quality of foil
    !!   when smooting is active and the quality is not good, smooting will be done
    !!   prints summary of the quality 
    !-------------------------------------------------------------------------
  
    use eval_commons,         only : curv_constraints_type
    use eval_constraints,     only : assess_surface
    use airfoil_base,         only : build_from_sides
  
    type (curv_constraints_type), intent (in) :: curv_constraints
    type (airfoil_type), intent (inout)       :: foil
    integer, intent(out)                      :: overall_quality
  
    integer             :: top_quality, bot_quality, istart, iend, iend_spikes
    doubleprecision     :: curv_threshold, spike_threshold
  
    !  ------------ analyze top -----
  
    curv_threshold  = curv_constraints%top%curv_threshold
    spike_threshold = curv_constraints%top%spike_threshold
    istart          = curv_constraints%top%nskip_LE
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
   
    call assess_surface (foil%top, show_details, istart, iend, iend_spikes, &
                         curv_threshold, spike_threshold, top_quality)
  
    if (top_quality >= Q_BAD ) then 

      continue          ! #todo - implement match_foil? 
  
    end if
  
    !  ------------ analyze bot -----
  
    if (.not. foil%symmetrical) then 
  
      curv_threshold  = curv_constraints%bot%curv_threshold
      spike_threshold = curv_constraints%bot%spike_threshold
      istart          = curv_constraints%bot%nskip_LE
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
  
      call assess_surface (foil%bot, show_details, istart, iend, iend_spikes, &
                          curv_threshold, spike_threshold, bot_quality)

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
  
    istart = c_spec%nskip_LE
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
  
    istart = c_spec%nskip_LE
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
  
    use eval_constraints,     only : max_curvature_at_te 
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
    istart = c_spec%nskip_LE
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
  
  

  subroutine auto_le_curvature_diff (foil, le_diff)
  
    !! Evaluates the best value for curvature difference at le (Bezier)
  
    use eval_constraints,     only : max_curvature_at_te 
    use shape_bezier,         only : bezier_curvature
  
    type (airfoil_type), intent(in)  :: foil
    double precision, intent(inout)  :: le_diff 
  
    double precision          :: top_curv_le, bot_curv_le
    integer                   :: quality
    character(:), allocatable :: label
    logical                   :: auto
  
    if (le_diff == 5d0 .and. foil%is_bezier_based) then     ! 5.0 is default value from inputs
    
      auto = .true.
      top_curv_le = bezier_curvature(foil%top_bezier, 0d0)
      bot_curv_le = bezier_curvature(foil%bot_bezier, 0d0)

      le_diff = abs (top_curv_le - bot_curv_le)

    ! give a little more to breath during opt.

      le_diff = max ((le_diff * 1.1d0), (le_diff + 2d0))
  
    else 
  
      auto = .false.
    
    end if
  
    ! Print it all 
  
    if (show_details) then 

      call print_text ("Bezier     ",5, no_crlf=.true.)

      quality = r_quality (le_diff, 5d0, 20d0, 200d0)
      label   = 'max_le_curv_diff '
      call print_colored (COLOR_PALE, label//'=') 
      call print_colored_r (5,'(F5.0)', quality, le_diff) 
      if (auto) then
        if (quality > Q_BAD) then
          call print_text ('Like seed there will be high curvature difference at LE', 3)
        else
          call print_text ('Smallest value based on seed',3)
        end if 
      else 
        call print_text ('User defined',3)
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
  
    istart = c%nskip_LE
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
      write (*,'(10x,A)') 'choose another seed airfoil. Find details in geometry plot of the viszualizer.'
      call my_stop ('The Optimizer may not found a solution with this inital violation.')
    end if
  
  end subroutine check_handle_curve_violations
  
  
  
  subroutine  check_te_curvature_violations (side, c)
  
    !! Checks trailing edga curvature x,y for violations max_te_crvature
    !! and handles user response  
  
    use eval_constraints,     only : max_curvature_at_te 
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
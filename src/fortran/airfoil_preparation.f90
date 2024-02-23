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

  public :: prepare_seed
  public :: transform_to_bezier_based
  public :: match_bezier, match_get_target_le_curvature

  public :: check_airfoil_curvature, auto_curvature_constraints

  ! --------- private --------------------------------------------------------

  ! static vars for match objective function 

  type(side_airfoil_type)       :: side_to_match
  double precision, allocatable :: target_x(:), target_y(:) ! target coordinates of side_to_match
  double precision              :: target_le_curv           ! target le curvature 
  double precision              :: max_te_curv              ! max te curvature 
  double precision              :: te_gap                   ! ... needed for bezier rebuild 
  integer                       :: nevals = 0               ! number of evaluations 

contains

  subroutine prepare_seed (airfoil_filename, eval_spec, shape_spec, seed_foil)

    !-----------------------------------------------------------------------------
    !! Read and prepare seed airfoil to be ready for optimization 
    !-----------------------------------------------------------------------------

    use airfoil_geometry,     only : normalize, repanel_bezier, repanel_and_normalize
    use eval_commons,         only : eval_spec_type
    use shape_airfoil,        only : shape_spec_type
    use shape_airfoil,        only : BEZIER, HICKS_HENNE
    use shape_bezier,         only : ncp_to_ndv

    use airfoil_base    
  
    character (*), intent(in)             :: airfoil_filename
    type (eval_spec_type), intent(inout)  :: eval_spec
    type (shape_spec_type), intent(inout) :: shape_spec
    type (airfoil_type), intent(out)      :: seed_foil

    type (airfoil_type)       :: original_foil

    ! read / create seed airfoil 

    call get_seed_airfoil (airfoil_filename, original_foil)

    ! is LE at 0,0? Do basic normalize to split, create spline ...

    if (.not. is_normalized_coord(original_foil)) & 
      call normalize (original_foil, basic=.true.)

    call split_foil_into_sides (original_foil)     ! upper and lower will be needed for input sanity


    ! repanel to seed to panel_options

    if (original_foil%is_bezier_based) then 
      call repanel_bezier        (original_foil, seed_foil, eval_spec%panel_options)
    else
      call repanel_and_normalize (original_foil, seed_foil, eval_spec%panel_options) 
    end if

    ! symmetrical? 

    if (eval_spec%geo_constraints%symmetrical)  call make_symmetrical (seed_foil)
  

    ! preset airfoil to geo targets - currently not possible if seed airfoil is .bez

    if (.not. (shape_spec%type == BEZIER .and. seed_foil%is_bezier_based)) then                         ! currently only HH - #todo

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
  
          call print_note ("Using no of Bezier control points from seed airfoil. "// &
                          "Values in 'bezier_options' will be ignored!", 3)
  
      else
  
        ! a new bezier "match foil" is generated to be new seed 
        seed_foil%name = seed_foil%name // '-bezier' //stri(shape_spec%bezier%ncp_top) //&
                                                       stri(shape_spec%bezier%ncp_bot)
  
        call transform_to_bezier_based (shape_spec%bezier, eval_spec%panel_options, seed_foil)
  
      end if 
    end if  
  
  
    ! Make sure seed airfoil passes constraints - final checks, prepare objective function 
    !  - get scaling factors for operating points with xfoil, 
  
    call check_seed (seed_foil, shape_spec, eval_spec%curv_constraints, eval_spec%geo_constraints, &
                     eval_spec%xfoil_options)

                     
    ! write final seed airfoil as reference 
  
    call airfoil_write_with_shapes (seed_foil, design_subdir, highlight=.false.)             

  end subroutine 



  subroutine get_seed_airfoil (airfoil_filename, foil )

    !-----------------------------------------------------------------------------------
    !! loads either .dat or .bez file into 'foil' 
    !-----------------------------------------------------------------------------------

    use shape_bezier,       only : load_bezier_airfoil
    use airfoil_base,       only : airfoil_load, split_foil_into_sides

    character(*), intent(in)        :: airfoil_filename
    type(airfoil_type), intent(out) :: foil

    character (:), allocatable  :: extension
    integer                     :: istart, np

    ! evaluate filetype from filename extension 

    istart = len(airfoil_filename) - 3
    extension = airfoil_filename (istart : )

    if (extension == '.dat' .or. extension == '.DAT') then 

    ! Read seed airfoil from .dat file

      call print_action ('Reading airfoil file', airfoil_filename)

      call airfoil_load (airfoil_filename, foil)

    else if (extension == '.bez' .or. extension == '.BEZ') then

    ! Read seed bezier control points from .bez file and generate airfoil

      call print_action ('Reading Bezier file', airfoil_filename)

      foil%is_bezier_based = .true.
      np = 201                              ! 201 points as default - will be repaneled anyway

      call load_bezier_airfoil (airfoil_filename, np, foil%name, foil%x, foil%y, foil%top_bezier, foil%bot_bezier) 
  
    else

      call my_stop ("Unknown file extension: "//extension)
    
    end if


  end subroutine get_seed_airfoil



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

      call print_action ('Checking seed airfoil passes all geometry constraints ...  ', no_crlf = .true.)

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

    use airfoil_geometry,         only: set_geometry
    use eval_commons,             only: geo_target_type

    type (geo_target_type), intent (in)   :: geo_targets (:)
    type (airfoil_type), intent (inout)   :: foil

    doubleprecision     :: new_camber, new_thick
    integer             :: i, ngeo_targets

    ngeo_targets = size(geo_targets)

    new_thick  = -1d0
    new_camber = -1d0

    if (ngeo_targets > 0) then 

      do i= 1, ngeo_targets

        if (geo_targets(i)%preset_to_target) then 
          select case (geo_targets(i)%type)

            case ('Thickness')                   

              new_thick = geo_targets(i)%target_value
              call print_action ('Scaling thickness to target value '// strf('(F6.4)', new_thick))
              call set_geometry (foil, maxt=new_thick)

            case ('Camber')                      

              new_camber = geo_targets(i)%target_value
              call print_action ('Scaling thickness to target value '// strf('(F6.4)', new_camber))
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
    double precision          :: best_le_curv

    ! Sanity check 
  
    if (.not. is_normalized_coord(foil)) then 
      call my_stop ('Airfoil is not normalized prior to Bezier transform')
    else
      ! ensure top and bot side do exist in foil   
      call split_foil_into_sides (foil) 
    end if  

    call print_action ("Create Bezier based airfoil")

    ! Simplex optimization (nelder mead) for both sides  

    best_le_curv = match_get_target_le_curvature (foil)

    call match_bezier  (foil%top, best_le_curv, shape_bezier%ncp_top, top_bezier)
    call match_bezier  (foil%bot, best_le_curv, shape_bezier%ncp_bot, bot_bezier)

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

    integer     :: i, imax, step, nTarg, iTarg

    ! we do not take every coordinate point - nelder mead would take much to 
    ! long to evaluate x,y on Bezier  

    imax = size(side%x) - 2                 ! not the last two points as target 
    step   = int (size(side%x)/21)  !21

    i = 3
    nTarg  = 0 
    do while (i <= imax)
      nTarg = nTarg + 1
      if (i <= 9) then 
        i = i + 3
      else
        i = i + step
      end if     
    end do

    if (allocated (target_x)) deallocate (target_x)
    if (allocated (target_y)) deallocate (target_y)
    allocate (target_x(nTarg))
    allocate (target_y(nTarg))

    i = 3
    iTarg  = 0 
    do while (i <= imax)
      iTarg = iTarg + 1
      target_x(iTarg) = side%x(i)
      target_y(iTarg) = side%y(i)
      if (i <= 9) then 
        i = i + 3
      else
        i = i + step
      end if  
    end do 


    ! define target le curvature based on target side 

    if (allocated(side_to_match%curvature)) then 

    ! define max te curvature based on target side 

      max_te_curv = side_to_match%curvature (size(side_to_match%curvature))

      ! print *, side%name, "ntarg ", ntarg, "  curv te", max_te_curv
    end if 


  end subroutine


  function match_get_target_le_curvature (foil) result (target_curv)

    ! determine the target curvature at leading edge

    type (airfoil_type), intent(in)    :: foil 
    
    double precision  :: target_curv, top_max, bot_max, le_curv, top_mean, bot_mean
    double precision, allocatable   :: top_curv (:), bot_curv (:)

    top_curv = foil%top%curvature
    bot_curv = foil%bot%curvature

    top_mean = 0d0
    bot_mean = 0d0
    top_max  = 0d0
    bot_max  = 0d0 
    le_curv  = top_curv (1)

    target_le_curv = 0d0 

    ! is there a curvature bump close to LE -> mean value with outbump 

    if (top_curv (2) < top_curv (3)) & 
      top_mean = (le_curv + top_curv (3)) / 2d0 
    
    if (bot_curv (2) < bot_curv (3)) & 
      bot_mean = (le_curv + bot_curv (3)) / 2d0 

    ! is there another max value at le 

    top_max = maxval (top_curv(2:)) 
    bot_max = maxval (bot_curv(2:))

    ! try to find the best value ...

    if (max(top_max, bot_max) > le_curv) then 
      target_curv = (le_curv + max(top_max, bot_max)) / 2d0
  
    else if (top_mean > 0d0) then
      target_curv = top_mean
    else if (bot_mean > 0d0) then
      target_curv = bot_mean
    else 
      target_curv = le_curv
    end if 

    ! print '(6(A,F5.0))',"   Target le curv: ",target_curv, "  le: ", le_curv, &
    !         "  top max: ", top_max, "  bot max: ", bot_max,&
    !         "  top mean: ", top_mean, "  bot mean: ", bot_mean

  end function


  function match_bezier_objective_function(dv) result (obj) 

    !! objective function to match bezier to 'side_to_match'
    
    use shape_bezier,         only : bezier_spec_type
    use shape_bezier,         only : bezier_violates_constraints, bezier_eval_y_on_x
    use shape_bezier,         only : bezier_curvature, map_dv_to_bezier

    double precision, intent(in) :: dv(:)
    double precision :: obj

    type (bezier_spec_type)       :: bezier
    double precision, allocatable :: devi (:), base(:)
    double precision              :: shift, te_curv, delta, deviation
    integer                       :: i, nTarg
    
    ! eval counter (for debugging) 

    nevals = nevals + 1

    ! remap bezier control points out of design variables 

    call map_dv_to_bezier (side_to_match%name, dv, te_gap, bezier)

    ! sanity check - nelder mead could have crossed to bounds or changed to order of control points 

    if (bezier_violates_constraints (bezier)) then 
      obj = 9999d0              ! penalty crossed bounds
      return
    end if 

    ! calc deviation bezier to target points 

    nTarg = size(target_x)
    allocate (devi(nTarg))
    allocate (base(nTarg))

    do i = 1, nTarg 
      devi(i) = abs (bezier_eval_y_on_x (bezier, target_x(i), epsilon=1d-8) - target_y(i))

      ! debug 
      if (mod(nevals,1000) == 0 .and. devi(i) > 0.01) then
        print *, "deviation error at ", i, nevals, target_y(i), bezier_eval_y_on_x (bezier, target_x(i), epsilon=1d-8) 
      end if 
    end do 
    
    ! calculate norm2 of the *relative* deviations 
    
    base = abs(target_y)

    ! #test normalize thickness

    base = (base / maxval(base) ) * 0.02d0 ! 0.025d0 

    ! move base so targets with a small base (at TE) don't become overweighted 

    shift =  maxVal (base) * 0.3d0 ! 0.7d0 

    ! print *, shift
    ! print '(3F9.5,2I6)', maxVal (base), shift, maxval(devi), maxloc(devi) , maxloc(devi / (base+shift))
    ! print '(100F7.5)', devi / (base+shift)

    obj = norm2 (devi / (base+shift))

    ! testing 
    if (nevals == 500) then 
      i = 0 
    end if 

    ! add curvature at LE and TE to objective 

    if (allocated(side_to_match%curvature)) then 

      ! LE: deviation to target curvature (see set targets) 

      deviation = abs (target_le_curv - bezier_curvature(bezier, 0d0)) / target_le_curv

      if (deviation > 0.001d0) then                              ! equals 0.1% deviation - allow blur 
        obj = obj + deviation / 15d0 ! 10d0                      ! empirical to reduce influence       
        ! print *, deviation, target_le_curv, curr_curv
      end if 

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
      if (max_te_curv >= 0.0) then 
        if (te_curv >= 0.0) then 
          delta = te_curv - max_te_curv
        else
          delta = - te_curv
        end if 
      else 
        if (te_curv < 0.0) then  
          delta = - (te_curv - max_te_curv)
        else
          delta = te_curv
        end if 
      end if 

      if (delta > 0.1) then
        ! print *, target_curv_te, cur_curv_te, delta
        ! only apply a soft penalty for real outliers
        obj = obj + delta / 500d0                ! add empirical  delta     
      end if 
        
    end if 

  end function match_bezier_objective_function



  subroutine match_bezier  (side, le_curv, ncp, bezier)
   
    !-----------------------------------------------------------------------------
    !! match a bezier curve to one side of an airfoil 
    !!    side:  either 'top' or 'bot' of an airfoil
    !!    le_curv:  le curvature which should be achieved
    !!    ncp:   number of control points the bezier curve should have 
    !!    bezier:  returns evaluated bezier definition 
    !-----------------------------------------------------------------------------

    use shape_bezier
    use simplex_search,       only : simplexsearch, simplex_options_type 

    type (side_airfoil_type), intent(in)    :: side  
    double precision, intent(in)            :: le_curv
    integer, intent(in)                     :: ncp
    type (bezier_spec_type), intent(out)    :: bezier 

    type (simplex_options_type)          :: sx_options
    double precision, allocatable   :: xopt(:), dv0(:), deviation(:)
    double precision                :: fmin, f0_ref, dev_max, dev_norm2, dev_max_at 
    integer                         :: steps, fevals, ndv, i, nTarg, le_curv_result
    

    ! setup targets in module variable for objective function 

    nevals = 0 
    side_to_match = side
    te_gap = side%y(size(side%y))
    target_le_curv = le_curv 

    call match_set_targets (side)

    ! initial estimate for bezier control points based on 'side to match'   

    call get_initial_bezier (side%x, side%y, ncp, bezier)

    ! nelder mead (simplex) optimization

    sx_options%min_radius     = 1d-5
    sx_options%max_iterations = 4000
    sx_options%initial_step = 0.1d0                    ! seems to be best value

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

    nTarg = size(target_x)
    allocate (deviation(nTarg))

    do i = 1, ntarg 
      deviation(i) = abs (bezier_eval_y_on_x (bezier, target_x(i), epsilon=1d-8) - target_y(i))
    end do 

    dev_norm2       = norm2 (deviation)
    dev_max         = maxval(deviation)
    dev_max_at      = target_x(maxloc (deviation,1))
    le_curv_result  = int(bezier_curvature (bezier, 0d0))

    if (show_details) then
      call  print_colored (COLOR_NOTE, stri(steps,4)//' iter'// &
                                       ', deviation: '//strf('(f8.5)',dev_norm2)// &
                                       ', max at: '//strf('(f5.3)',dev_max_at)// &
                                       ', le curv:'//stri(le_curv_result,4)//" - ")
      if (steps == sx_options%max_iterations) then
        call print_colored (COLOR_WARNING, 'Max iter reached')
      else 
        if (dev_norm2 < 0.0005d0) then 
          call print_colored(COLOR_GOOD, "Good")
        elseif (dev_norm2 < 0.001d0) then 
          call print_colored(COLOR_NORMAL, "OK")
        elseif (dev_norm2 < 0.005d0) then 
          call print_colored(COLOR_WARNING, "Bad")
        else
          call print_colored(COLOR_ERROR, "Failed")
        end if 
    end if
    print *

    end if 

  end subroutine match_bezier



  subroutine check_airfoil_curvature (curv_constraints, foil, overall_quality)

    !-------------------------------------------------------------------------
    !! Checks curvature quality of foil
    !!   when smooting is active and the quality is not good, smooting will be done
    !!   prints summary of the quality 
    !-------------------------------------------------------------------------
  
    use eval_commons,         only : curv_constraints_type
    use eval_constraints,     only : assess_surface
    use airfoil_base,         only : rebuild_from_sides
  
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

      quality_threshold  = r_quality (curv_threshold, 0.015d0, 0.03d0, 0.2d0)
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
  
    double precision, parameter    :: OK_THRESHOLD  = 0.4d0
    double precision, parameter    :: MIN_THRESHOLD = 0.1d0
    double precision, parameter    :: MAX_THRESHOLD = 1.0d0
  

    spike_threshold = c_spec%spike_threshold
  
    ! How many Spikes do we have with current threshold defined by user / default?
  
    istart = c_spec%nskip_LE
    iend   = size(side%x)
    nspikes = count_reversals (istart, iend, derivative1 (side%x, side%curvature), spike_threshold)
    ! write (*,*) '------ ', istart, iend , spike_threshold, nspikes
  
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
  
    if (c_spec%max_te_curvature == 10d0) then 
    
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
  
    double precision, parameter    :: OK_THRESHOLD  = 0.4d0
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
  
    !! Evaluates the best value for curvature at TE of polyline
  
    use eval_constraints,     only : max_curvature_at_te 
    use shape_bezier,         only : bezier_curvature
  
    type (airfoil_type), intent(in)  :: foil
    double precision, intent(inout)  :: le_diff 
  
    double precision          :: top_curv_le, bot_curv_le
    integer                   :: quality
    character(:), allocatable :: label
    logical                   :: auto
  
    if (le_diff == 5d0 .and. foil%is_bezier_based) then     ! 1.0 is default value from inputs
    
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
! MIT License
! Copyright (C) 2017-2019 Daniel Prosser
! Copyright (c) 2022-2025 Jochen Guenzel

module eval

  ! Core module: Sets up and evaluates the objective function for an airfoil design

  use commons       
  use os_util
  use print_util

  use airfoil_base,       only : airfoil_type, panel_options_type

  use xfoil_driver,       only : xfoil_options_type
  use xfoil_driver,       only : op_point_spec_type, re_type
  use xfoil_driver,       only : op_point_result_type
  use xfoil_driver,       only : flap_spec_type

  use eval_commons
  use eval_out
  use eval_constraints

  implicit none 
  private
 
  ! --- Public functions -----------------------------------

  public :: is_design_valid                             ! to find valid inital designs
  public :: objective_function                          ! the one and only 

  public :: eval_seed_scale_objectives                  
  public :: write_progress
  public :: write_final_results
  public :: set_eval_spec

  double precision, parameter, public  :: OBJ_XFOIL_FAIL = 55.55d0
  double precision, parameter, public  :: OBJ_GEO_FAIL   = 1000d0


  ! ---- static, private ---------------------------------

  ! these evaluation specifications are loaded at the beginnning of optimization 
  ! and are static, private to this module to ensure encapsulation 

  type (op_point_spec_type),  allocatable :: op_points_spec (:)
  type (geo_target_type), allocatable     :: geo_targets (:) 

  type (geo_constraints_type)             :: geo_constraints 
  type (curv_constraints_type)            :: curv_constraints
  type (panel_options_type)               :: panel_options

  type (xfoil_options_type), private      :: xfoil_options

  type (dynamic_weighting_spec_type)      :: dynamic_weighting_spec 

  type (match_foil_spec_type)             :: match_foil_spec


  ! Save the best result of evaluation for write_design (+ dynamic weighting)
  !    so no extra run_xfoil is needed

  double precision                        :: best_objective = 1d0
  type(airfoil_type)                      :: best_foil
  type(op_point_result_type), allocatable :: best_op_points_result (:) 

  ! dynamic weighting helper
 
  type dynamic_variable_type
    double precision  :: dev  
    double precision  :: weighting, new_weighting 
  end type 

  ! Numerical accuracy 

  double precision, parameter    :: EPSILON = 1.d-10          

contains


  function objective_function (dv)

    !-----------------------------------------------------------------------------
    !! The Objective Function 
    !-----------------------------------------------------------------------------

    use xfoil_driver,       only : op_point_result_type

    double precision, intent(in)    :: dv (:) 

    double precision                :: objective_function
    double precision                :: geo_penalty, aero, geo
    type(airfoil_type)              :: foil
    double precision, allocatable   :: flap_angles (:) 

    type(op_point_result_type), allocatable :: op_points_result (:)
    type(geo_result_type)           :: geo_result
    integer     :: i

    objective_function = 0d0
    geo_penalty        = 0d0
    aero               = 0d0
    geo                = 0d0

    ! get new airfoil out of design variables 

    foil =  create_airfoil (dv)

    ! Geometry constraints violations? - early exit

    geo_penalty = geo_penalty_function (foil)

    if(geo_penalty >= OBJ_GEO_FAIL) then          
      objective_function = geo_penalty 
      return 
    end if 

    ! get actual flap angles (some could be optimized)

    flap_angles = get_flap_angles (dv)

    ! evaluate the foil with xfoil ...

    op_points_result = aero_objective_results (foil, flap_angles) 

    ! early exit if one op point fails - objective function wouldn't make sense 

    do i = 1, size(op_points_result)
      if (.not. op_points_result(i)%converged) then 
        objective_function = OBJ_XFOIL_FAIL
        return 
      end if
    end do

    ! calc aero objective function from xfoil results 

    aero = aero_objective_function (op_points_result)

    ! calc geometry objective function from  geo targets 

    geo_result = geo_objective_results (foil)      
    geo  = geo_objective_function (geo_result )


    ! final objective_function

    objective_function = aero + geo + geo_penalty

    ! Save the best result to be written later at write_design ...

    !$omp critical
    if (objective_function < best_objective) then
      best_objective = objective_function 
      best_foil = foil 
      best_op_points_result = op_points_result
    end if 
    !$omp end critical

  end function 



  subroutine set_eval_spec (eval_spec)

    !-----------------------------------------------------------------------------
    !! sets eval specification into static structures for later evaluation  
    !-----------------------------------------------------------------------------

    type (eval_spec_type), intent(in)  :: eval_spec 

    op_points_spec          = eval_spec%op_points_spec
    geo_targets             = eval_spec%geo_targets

    geo_constraints         = eval_spec%geo_constraints
    curv_constraints        = eval_spec%curv_constraints

    xfoil_options           = eval_spec%xfoil_options
    panel_options           = eval_spec%panel_options

    match_foil_spec         = eval_spec%match_foil_spec
    
    dynamic_weighting_spec  = eval_spec%dynamic_weighting_spec

  
  end subroutine 
  


  subroutine eval_seed_scale_objectives (seed_foil)

    !----------------------------------------------------------------------------------
    !! evaluates seed airfoil to scale objectives to achieve objective function = 1.0
    !----------------------------------------------------------------------------------

    use eval_commons 
    use shape_airfoil,        only : shape_spec
    use math_util,            only : norm_2
    use xfoil_driver,         only : run_op_points, op_point_result_type, xfoil_defaults

    type (airfoil_type), intent(in)           :: seed_foil

    type(op_point_spec_type)                  :: op_spec
    type(op_point_result_type)                :: op
    type(op_point_result_type), allocatable   :: op_points_result (:)
    type(geo_result_type)                     :: geo_result
    type(xfoil_options_type)                  :: local_xfoil_options
    double precision, allocatable             :: flap_angles (:) 

    integer          :: i
    double precision :: correction, checkval
    ! double precision :: aero, geo, obj 
    double precision :: ref_value, seed_value, tar_value, cur_value
    double precision :: dist = 0d0
    character(:), allocatable  :: opt_type


    ! Analyze airfoil at requested operating conditions with Xfoil

    call print_action ('Evaluate seed airfoils objectives to scale objective function to 1.0')

    ! allow local xfoil options for seed airfoil 
    local_xfoil_options = xfoil_options
    ! Re-Init boundary layer at each op point to ensure convergence (slower)
    ! local_xfoil_options%reinitialize = .false.      ! strange: reinit leeds sometimes to not converged
    local_xfoil_options%show_details = show_details   ! for seed we are single threaded, show deails posssible 
    ! set flaps of seed to predefined angle from input

    flap_angles = op_points_spec(:)%flap_angle 
 
    ! now run xfoil ...
   
    call run_op_points (seed_foil, local_xfoil_options, shape_spec%flap_spec, flap_angles, &
                        op_points_spec, op_points_result)

    ! Evaluate seed value of geomtry targets and scale factor 

    geo_result = geo_objective_results (seed_foil)
      
    do i = 1, size(geo_targets)

      select case (geo_targets(i)%type)

        case ('thickness')                            ! take foil thickness calculated above
          seed_value = geo_result%maxt
          ref_value  = seed_value
          correction = 1.2d0                          ! thickness is less sensible to changes

        case ('camber')                               ! take xfoil camber from  above
          seed_value = geo_result%maxc
          ref_value  = seed_value
          correction = 0.7d0                          ! camber is quite sensible to changes

        case ('match-foil')
          seed_value = geo_result%match_top_deviation + geo_result%match_bot_deviation
          ref_value  = 0d0 ! seed_value
          correction = 1d0

        case default
          call my_stop("Unknown geo target_type '"//geo_targets(i)%type)
      end select

      geo_targets(i)%seed_value      = seed_value
      geo_targets(i)%reference_value = ref_value

      ! target value negative?  --> take current seed value * |target_value| 
      if (geo_targets(i)%target_value < 0.d0)      &
          geo_targets(i)%target_value = seed_value * abs(geo_targets(i)%target_value)

      tar_value = geo_targets(i)%target_value

      ! will scale objective to 1 ( = no improvement) 
      geo_targets(i)%scale_factor = 1 / ( ref_value + abs(tar_value - seed_value) * correction)

    end do 


    ! Evaluate objectives to establish scale factors for each point

    do i = 1, size(op_points_spec)

      op_spec  = op_points_spec(i)
      op       = op_points_result(i) 
      opt_type = op_spec%optimization_type

      ! Check for unconverged points

      if (.not. op%converged) then
        call print_not_converged_reason (op_spec, op)
        call my_stop("Xfoil calculations did not converge for operating point: "//stri(i))
      end if

      if (op%cl <= 0.d0 .and. (opt_type == 'min-sink' .or.   &
          opt_type == 'max-glide') ) then
        call my_stop( "Operating point "//stri(i)//" has Cl <= 0. "//     &
                  "Cannot use "//opt_type//" optimization in this case.")
      end if

      if (opt_type == 'min-sink') then

        checkval   = op%cd / op%cl**1.5d0

      elseif (opt_type == 'max-glide') then

        checkval   = op%cd / op%cl

      elseif (opt_type == 'min-drag') then

        checkval   = op%cd

      elseif (opt_type == 'target-drag') then

        cur_value = op%cd

        if (op_spec%target_value < 0.d0) &          ! negative? - value is factor to seed
            op_spec%target_value = cur_value * abs(op_spec%target_value)

        if (op_spec%allow_improved_target) then
          dist = max (0d0, (cur_value - op_spec%target_value))
        else 
          dist = abs(cur_value - op_spec%target_value)
          if (dist < 0.000004d0) dist = 0d0         ! little threshold to achieve target
        end if 

        checkval   = op_spec%target_value + dist

      elseif (opt_type == 'target-glide') then

        cur_value = op%cl / op%cd

        if (op_spec%target_value < 0.d0) &          ! negative? - value is factor to seed
            op_spec%target_value = cur_value * abs(op_spec%target_value)

        if (op_spec%allow_improved_target) then
          dist = max (0d0, (op_spec%target_value - cur_value))
        else 
          dist = abs(cur_value - op_spec%target_value)
          if (dist < 0.01d0) dist = 0d0             ! little threshold to achieve target
        end if 
        
        correction = 0.7d0                          ! glide ration is quite sensible to changes
        checkval   = op_spec%target_value + dist * correction

      elseif (opt_type == 'target-lift') then

        cur_value = op%cl

        if (op_spec%target_value < 0.d0) &          ! negative? - value is factor to seed
            op_spec%target_value = cur_value * abs(op_spec%target_value)

        if (op_spec%allow_improved_target) then
          dist = max (0d0, (op_spec%target_value - cur_value))
        else 
          dist = abs(cur_value - op_spec%target_value)
          if (dist < 0.001d0) dist = 0d0            ! little threshold to achieve target
        end if 

        ! add a constant base value to the lift difference so the relative change won't be to high
        correction = 0.8d0                          ! lift is quite sensible to changes
        checkval   = 1.d0 + dist * correction

      elseif (opt_type == 'target-moment') then

        cur_value = op%cm

        ! note - the "trick" with negative target is not possible with moments
        !        as the cm-target-value may be negative... 
        ! if (op_spec%target_value < 0.d0) &          ! negative? - value is factor to seed
        !     op_spec%target_value = cur_value * abs(op_spec%target_value) 

        if (op_spec%allow_improved_target) then
          dist = max (0d0, (op_spec%target_value - cur_value))
        else 
          dist = abs(cur_value - op_spec%target_value)
          if (dist < 0.001d0) dist = 0d0            ! little threshold to achieve target
        end if 
    
        ! add a base value (Clark y or so ;-) to the moment difference so the relative change won't be to high
        checkval   = dist + 0.05d0

      elseif (opt_type == 'max-lift') then

        checkval   = 1.d0/op%cl

      elseif (opt_type == 'max-xtr') then

        checkval   = 1.d0/(0.5d0*(op%xtrt + op%xtrb) + 0.1d0)  ! Ensure no division by 0
     
      else
        call my_stop ("Unknown optimization_type for operating point "// stri(i))
      end if

      op_spec%scale_factor = 1.d0/checkval

      op_points_spec(i) = op_spec             ! write back target, scale, ...

    end do


    ! Final sanity check - this is also done in 'optimization' 

    ! aero = aero_objective_function (op_points_result)
    ! geo  = geo_objective_function (geo_result )
    ! obj  = aero + geo
    ! if (abs(1d0 - obj) > EPSILON) then 
    !   call print_warning ("eval_seed: objective of seed isn't 1.0 ("//strf('(F12.8)',obj)//")", 5)
    ! end if 
    
  end subroutine 



  function is_design_valid (dv)

    !-----------------------------------------------------------------------------
    !! returns .true. if the design variables result in a geometric valid design  
    !-----------------------------------------------------------------------------

    double precision, intent(in)  :: dv(:) 

    type (airfoil_type)     :: foil
    logical                 :: is_design_valid

    foil = create_airfoil (dv)

    is_design_valid = (geo_penalty_function (foil) == 0d0)

  end function 



  function geo_penalty_function(foil) result (penalty)

    !-----------------------------------------------------------------------------
    !!  Geometric Penalty function
    !!
    !!  Asses geometry to find violations of geometric 
    !!  or curvature constraints.
    !!  The first violation will abort further checks 
    !!
    !!  Returns penalty OBJ_GEO_FAIL if a violation was detected
    !-----------------------------------------------------------------------------

    type(airfoil_type), intent(in)    :: foil
    double precision                  :: penalty

    logical                     :: has_violation 
    integer                     :: viol_id
    character (100)             :: info

    penalty = OBJ_GEO_FAIL

    ! Check for curvature constraints  

    if (curv_constraints%check_curvature) then

      call eval_curvature_violations (foil, curv_constraints, has_violation, viol_id, info)
      if (has_violation) return

    end if 

    ! Check geometry contraints   

    if (geo_constraints%check_geometry) then

      call eval_geometry_violations (foil, geo_constraints, has_violation, viol_id)
      if (has_violation) return

    end if 

    ! no violations detected 

    penalty = 0d0

  end function 



  function geo_objective_results (foil) result (geo_result) 

    !----------------------------------------------------------------------------
    !! eval geometry results  
    !----------------------------------------------------------------------------

    use airfoil_geometry,     only : get_geometry, eval_deviation_at_side
    use shape_bezier,         only : bezier_curvature

    type(airfoil_type), intent(in)  :: foil
    type(geo_result_type)           :: geo_result 
    double precision, allocatable   :: target_x(:), target_y(:) 

    ! match foil - only evaluate deviation - the other results are not needed 

    if (match_foil_spec%active) then

      target_x = match_foil_spec%target_top_x
      target_y = match_foil_spec%target_top_y
      geo_result%match_top_deviation = eval_deviation_at_side (foil, 'Top', target_x, target_y)

      target_x = match_foil_spec%target_bot_x
      target_y = match_foil_spec%target_bot_y
      geo_result%match_bot_deviation = eval_deviation_at_side (foil, 'Bot', target_x, target_y)

      return 
    end if 

    ! get thickness, camber ...

    call get_geometry (foil, geo_result%maxt, geo_result%xmaxt, geo_result%maxc, geo_result%xmaxc)

    ! bezier: get the difference of top and bot curvature at le 
        
    if (foil%is_bezier_based) then 
      geo_result%top_curv_le = bezier_curvature(foil%top_bezier, 0d0)
      geo_result%top_curv_te = bezier_curvature(foil%top_bezier, 1d0)
      geo_result%bot_curv_le = bezier_curvature(foil%bot_bezier, 0d0)
      geo_result%bot_curv_te = bezier_curvature(foil%bot_bezier, 1d0)
    else
      geo_result%top_curv_le = foil%top%curvature(1) 
      geo_result%top_curv_te = foil%top%curvature(size(foil%top%curvature)) 
      geo_result%bot_curv_le = foil%bot%curvature(1) 
      geo_result%bot_curv_te = foil%bot%curvature(size(foil%bot%curvature)) 
    end if 

  end function 



  function geo_objective_function (geo_result, eval_only_dynamic_ops )

    !----------------------------------------------------------------------------
    !!  Geo objective function as result of geometry evaluation
    !!
    !!  Input: geo_result  the actual geometry values 
    !!         eval_only_dynamic_ops - used for dynamic weighing 
    !----------------------------------------------------------------------------

    double precision :: geo_objective_function

    type(geo_result_type), intent(in) :: geo_result
    logical,  intent(in), optional :: eval_only_dynamic_ops

    integer          :: i
    double precision :: ref_value, tar_value, cur_value, increment, geo, correction
    logical          :: eval_all

    geo  = 0.d0 
    
    if (present(eval_only_dynamic_ops)) then
      eval_all = .not. eval_only_dynamic_ops
    else
      eval_all = .true.
    end if

    ! Evaluate current value of geomtry targets 
    do i = 1, size(geo_targets)

      if (eval_all .or. geo_targets(i)%dynamic_weighting ) then

        ref_value = geo_targets(i)%reference_value
        tar_value = geo_targets(i)%target_value

        select case (geo_targets(i)%type)

          case ('thickness')                            
            cur_value  = geo_result%maxt
            correction = 1.2d0                          ! thickness is less sensible to changes

          case ('camber')                               
            cur_value  = geo_result%maxc
            correction = 0.7d0                          ! camber is quite sensible to changes

          case ('match-foil')  
            cur_value  = geo_result%match_top_deviation + geo_result%match_bot_deviation                             
            correction = 1d0                          

          case default
            call my_stop("Unknown target_type "//quoted(geo_targets(i)%type))
        end select

        ! scale objective to 1 ( = no improvement) 
        increment = (ref_value + abs(tar_value - cur_value) * correction) * geo_targets(i)%scale_factor 

        geo = geo +  geo_targets(i)%weighting * increment

      end if 

    end do

    geo_objective_function = geo

  end function 



  function aero_objective_results (foil, flap_angles) result (op_points_result)

    !----------------------------------------------------------------------------
    !!  evalute aero objectives with xfoil 
    !----------------------------------------------------------------------------

    use xfoil_driver,       only : run_op_points, op_point_result_type
    use shape_airfoil,      only : shape_spec

    type(airfoil_type), intent(in)  :: foil
    double precision, intent(in)    :: flap_angles (:)

    type(op_point_result_type), allocatable :: op_points_result (:) 
    type(xfoil_options_type)                :: local_xfoil_options

    ! Analyze airfoil at requested operating conditions with Xfoil

    local_xfoil_options = xfoil_options
    local_xfoil_options%show_details        = .false.  ! switch off because of multi-threading
    local_xfoil_options%exit_if_unconverged = .true.   ! speed up if an op point uncoverges

    call run_op_points (foil, local_xfoil_options, shape_spec%flap_spec, flap_angles, &
                        op_points_spec, op_points_result)

  end function



  function aero_objective_function (op_points_result, eval_only_dynamic_ops)

    !-----------------------------------------------------------------------------
    !!  Objective function as result of aerodynamic evaluation
    !!
    !!  Input:   op points results from xfoil calculation
    !!  Output:  objective function value based on airfoil performance
    !-----------------------------------------------------------------------------

    use xfoil_driver,       only : op_point_result_type

    type(op_point_result_type), intent(in) :: op_points_result (:)
    logical,  intent(in), optional         :: eval_only_dynamic_ops

    double precision                  :: aero_objective_function
    type(op_point_spec_type)          :: op_spec
    type(op_point_result_type)        :: op
    integer          :: i, noppoint
    double precision :: pi
    double precision :: cur_value, increment, dist, correction
    character(15)    :: opt_type
    logical          :: eval_all

    pi = acos(-1.d0)
    noppoint = size(op_points_spec)  

    if (present(eval_only_dynamic_ops)) then
      eval_all = .not. eval_only_dynamic_ops
    else
      eval_all = .true.
    end if

    ! Get objective function contribution from aerodynamics 
    !    (aero performance times normalized weight)

    aero_objective_function = 0.d0

    do i = 1, noppoint


      op_spec  = op_points_spec(i)
      op       = op_points_result(i) 
      opt_type = op_spec%optimization_type

      if (eval_all .or. (op_spec%dynamic_weighting .and. (.not. eval_all))) then
  
      ! Objective function evaluation

        if (opt_type == 'min-sink') then

        ! Maximize Cl^1.5/Cd

          if (op%cl > 0.d0) then
            increment = (op%cd / op%cl**1.5d0) * op_spec%scale_factor
          else
            increment = 1.D9   ! Big penalty for lift <= 0
          end if
          cur_value  = op%cl**1.5d0 / op%cd

        elseif (opt_type == 'max-glide') then

        ! Maximize Cl/Cd

          if (op%cl > 0.d0) then
            increment = op%cd / op%cl * op_spec%scale_factor
          else
            increment = 1.D9   ! Big penalty for lift <= 0
          end if
          cur_value  = op%cl / op%cd 

        elseif (opt_type == 'min-drag') then

        ! Minimize Cd

          increment = op%cd * op_spec%scale_factor
          cur_value = op%cd 

        elseif (opt_type == 'target-drag') then

        ! Minimize difference between target cd value and current value

          cur_value = op%cd

          if (op_spec%allow_improved_target) then
            dist = max (0d0, (cur_value - op_spec%target_value))
          else 
            dist = abs(cur_value - op_spec%target_value)
            if (dist < 0.000004d0) dist = 0d0         ! little threshold to achieve target
          end if 

          increment = (op_spec%target_value + dist) * op_spec%scale_factor 

        elseif (opt_type == 'target-glide') then

        ! minimize difference between target glide ratio and current glide ratio 
        
          cur_value = op%cl / op%cd

          if (op_spec%allow_improved_target) then
            dist = max (0d0, (op_spec%target_value - cur_value))
          else 
            dist = abs(cur_value - op_spec%target_value)
            if (dist < 0.01d0) dist = 0d0         ! little threshold to achieve target
          end if 

          correction = 0.7d0               ! glide ration is quite sensible to changes
          increment = (op_spec%target_value + dist * correction) * op_spec%scale_factor 

        elseif (opt_type == 'target-lift') then

        ! Minimize difference between target cl value and current value 
        !    Add a base value to the lift difference
        
          cur_value = op%cl

          if (op_spec%allow_improved_target) then
            dist = max (0d0, (op_spec%target_value - cur_value))
          else 
            dist = abs(cur_value - op_spec%target_value)
            if (dist < 0.001d0) dist = 0d0         ! little threshold to achieve target
          end if 

          correction = 0.8d0               ! lift is quite sensible to changes
          increment = (1.d0 + dist * correction)  * op_spec%scale_factor 

        elseif (opt_type == 'target-moment') then

        ! Minimize difference between target moment value and current value 
        !        Add a base value (Clark y or so ;-) to the moment difference
        !        so the relative change won't be to high
          cur_value = op%cm

          if (op_spec%allow_improved_target) then
            dist = max (0d0, (op_spec%target_value - cur_value))
          else 
            dist = abs(cur_value - op_spec%target_value)
            if (dist < 0.001d0) dist = 0d0         ! little threshold to achieve target
          end if 

          increment = (dist + 0.05d0) * op_spec%scale_factor

        elseif (opt_type == 'max-lift') then

        ! Maximize Cl (at given angle of attack)

          if (op%cl > 0.d0) then
            increment = op_spec%scale_factor / op%cl
          else
            increment = 1.D9   ! Big penalty for lift <= 0
          end if
          cur_value = op%cl

        elseif (opt_type == 'max-xtr') then

        ! Maximize laminar flow on top and bottom (0.1 factor to ensure no
        !   division by 0)

          increment = op_spec%scale_factor/(0.5d0*(op%xtrt + op%xtrb)+0.1d0)
          cur_value = 0.5d0*(op%xtrt + op%xtrb)

        else

          call my_stop ("Requested optimization_type not recognized.")

        end if

        aero_objective_function = aero_objective_function &
                                  + op_spec%weighting * increment

      end if
    end do

  end function 



subroutine write_progress (dv, designcounter)

  !-----------------------------------------------------------------------------
  !! Writes airfoil coordinates and op Points results to files during optimization
  !!   designcounter = 0 will start new files 
  !-----------------------------------------------------------------------------

  use math_util,            only : min_threshold_for_reversals
  use xfoil_driver,         only : run_op_points, op_point_result_type, xfoil_stats_print
  use shape_airfoil,        only : shape_spec, BEZIER, HICKS_HENNE, get_seed_foil

  double precision, intent(in)  :: dv (:)
  integer, intent(in)           :: designcounter

  type(op_point_result_type), allocatable :: op_points_result (:)
  type(airfoil_type)              :: foil
  type(xfoil_options_type)        :: local_xfoil_options
  type(geo_result_type)           :: geo_result
  double precision, allocatable   :: flap_angles (:)
  character(:), allocatable       :: foil_file, bezier_file, op_points_file, geo_targets_file, hicks_file
  integer                         :: foil_unit, bezier_unit, hicks_unit, op_points_unit, geo_unit
  integer                         :: dstart, dfreq
  logical                         :: dynamic_done
 

  if (designcounter == 0) then
    call print_action ("Writing design #0 being seed airfoil to "//design_subdir)
  else
    if (show_details) print *  
  end if
 

  ! Design 0 is seed airfoil to output - take the original values 

  if (designcounter == 0) then 
    call get_seed_foil (foil)

  ! Design > 0 - Build current foil from design variables 
  else 
    foil = create_airfoil (dv)
    foil%name = output_prefix
  end if

  ! Get actual flap angles based on design variables

  flap_angles = get_flap_angles (dv)


  ! Performance Cache: Try to get xfoil result from "save best" in objective function

  if (allocated(best_op_points_result)) then 
    ! Sanity check - Is the "best" really our current foil
    if (abs(sum(foil%y) - sum(best_foil%y)) < EPSILON ) then  ! num issues with symmetrical) 
      op_points_result = best_op_points_result
    else
      call print_warning ("eval write: no best design available") 
      best_objective = 1d0                                  ! reset best store - something wrong...?
    end if 
  end if 

  ! There is no stored result - so re-calc for this foil

  if (.not. allocated(op_points_result)) then 

    ! Analyze airfoil at requested operating conditions with Xfoil

    local_xfoil_options = xfoil_options
    local_xfoil_options%show_details        = .false.  
    local_xfoil_options%exit_if_unconverged = .false.       ! we need all op points
    if (designcounter == 0) then
      local_xfoil_options%reinitialize = .false.            ! strange: reinit leeds sometimes to not converged
    end if 

    call run_op_points (foil, local_xfoil_options, shape_spec%flap_spec, flap_angles, &
                        op_points_spec, op_points_result)
  end if 
 
  
  ! Set output file names and identifiers

  foil_file       = design_subdir//'Design_Coordinates.csv'
  op_points_file  = design_subdir//'Design_OpPoints.csv'
  bezier_file     = design_subdir//'Design_Beziers.csv'
  hicks_file      = design_subdir//'Design_Hicks.csv'
  geo_targets_file= design_subdir//'Design_GeoTargets.csv'

  foil_unit       = 13
  op_points_unit  = 14
  bezier_unit     = 15
  hicks_unit      = 16
  geo_unit        = 17

  ! Open files of design data ...

  if (designcounter == 0) then

    ! ... design=0 (seed airfoil), write header and opPoint specifications

    open(unit=op_points_unit, file=op_points_file, status='replace', err=901)
    call write_design_op_points_header (op_points_unit)

    if (shape_spec%type == BEZIER) then 
      open(unit=bezier_unit, file= bezier_file, status='replace', err=902)
      call write_design_bezier_header (bezier_unit, foil)
    else if (shape_spec%type == HICKS_HENNE) then 
      open(unit=hicks_unit, file= hicks_file, status='replace', err=902)
      call write_design_hh_header (hicks_unit, foil, shape_spec%hh)
    else 
      open(unit=foil_unit,   file=foil_file,    status='replace', err=900)
      call write_design_coord_header (foil_unit, foil)
    end if 

    if (size(geo_targets) > 0 ) then 
      open(unit=geo_unit,   file=geo_targets_file,    status='replace', err=903)
      call write_design_geo_targets_header (geo_unit)
    end if 

  else

    ! ... design > 0 

    open (unit=op_points_unit, file=op_points_file, status='old', position='append', & 
          action = 'readwrite', err=901)

    if (shape_spec%type == BEZIER) then 
      open (unit=bezier_unit,  file= bezier_file,  status='old', position='append', &
            action = 'readwrite', err=902)
    else if (shape_spec%type == HICKS_HENNE) then 
      open (unit=hicks_unit,  file= hicks_file,  status='old', position='append', &
            action = 'readwrite', err=902)
    else
      open (unit=foil_unit,    file=foil_file,     status='old', position='append', &
            action = 'readwrite', err=900)
    end if

    if (size(geo_targets) > 0 ) then 
      open (unit=geo_unit, file=geo_targets_file, status='old', position='append', &
            action = 'readwrite', err=903)
    end if 


  end if

  ! Write design data 

  call write_design_op_points_data (op_points_unit, designcounter, op_points_spec, &
                                    op_points_result, flap_angles)

  if (shape_spec%type == BEZIER) then 
    call write_design_bezier_data   (bezier_unit, designcounter, foil)
  else if (shape_spec%type == HICKS_HENNE) then 
    call write_design_hh_data       (hicks_unit, designcounter, foil)
  else 
    call write_design_coord_data    (foil_unit, designcounter, foil)
  end if 

  if (size(geo_targets) > 0 ) then 

    ! Evaluate geometry results of current design foil

    geo_result = geo_objective_results (foil) 

    call write_design_geo_targets_data (geo_unit, designcounter, geo_targets, geo_result)
  end if 

  ! done 

  close (foil_unit)
  close (op_points_unit)
  close (bezier_unit)
  close (hicks_unit)
  close (geo_unit)


  ! ----- Actions when new design was found --------------------------------------


  ! Dynamic Weighting of op points and geo targets

  if (dynamic_weighting_spec%active) then 

    if (designcounter == 0) then
      dstart = dynamic_weighting_spec%start_with_design
      dfreq  = dynamic_weighting_spec%frequency
      if (show_details) then
        print *
        call print_note ("Dynamic weighting starting with design #"// stri(dstart) // & 
                            " repeating every " //stri(dfreq)//" designs.", 3)
      end if 
    end if  

    call do_dynamic_weighting (designcounter, dynamic_weighting_spec, & 
                               op_points_result, geo_result, dynamic_done)
  else
    dynamic_done = .false.
  end if

  if (show_details .and. (designcounter > 0)) then 
    call print_improvement  (op_points_spec, geo_targets, op_points_result, geo_result, &
                             shape_spec%flap_spec%use_flap, flap_angles, dynamic_done) 

    call violation_stats_print ()
    call xfoil_stats_print
    print * 
  end if

  ! Testing:  Write op points deviation to file
  !  call write_op_results (designcounter, op_points_result, geo_result) 
  !  call write_designvars (designcounter, dv) 
  !  call print_designvars (designcounter, dv) 


  return

  ! File I/O Warnings 

  900 call print_warning ("Warning: unable to open "//foil_file//". Skipping ...")
    return
  901 call print_warning ("Warning: unable to open "//op_points_file//". Skipping ...")
    return
  902 call print_warning ("Warning: unable to open "//bezier_file//". Skipping ...")
    return
  903 call print_warning ("Warning: unable to open "//geo_targets_file//". Skipping ...")
    return

end subroutine



subroutine write_final_results (dv, fmin, final_foil, flap_angles )

  !-----------------------------------------------------------------------------
  !! Writes final airfoil design 
  !!    Returns final airfoil 
  !-----------------------------------------------------------------------------

  use shape_airfoil,          only : shape_spec
  use xfoil_driver,           only : run_op_points, op_point_result_type
  use xfoil_driver,           only : op_point_spec_type

  double precision, allocatable, intent(in)   :: dv (:) 
  double precision, intent(in)                :: fmin
  type(airfoil_type), intent(out)             :: final_foil
  double precision, allocatable, intent(out)  :: flap_angles (:) 

  type(op_point_result_type), allocatable     :: op_points_result (:)
  type(geo_result_type)                       :: geo_result

  print *
  call print_header ('Optimization completed. Objective function improvement over seed: ', no_crlf=.true.)
  call print_colored_r (8, '(F7.4,"%")', Q_GOOD, ((1d0 - fmin) * 100.d0))
  print *

  ! create final airfoil and flap angles from designvars 

  final_foil  = create_airfoil  (dv)
  flap_angles = get_flap_angles (dv)

  ! analyze final design - Run xfoil for requested operating points

  call run_op_points (final_foil, xfoil_options, shape_spec%flap_spec, flap_angles,  &
                      op_points_spec, op_points_result)

  ! get geo results 

  geo_result = geo_objective_results (final_foil)      

  print *
  call print_improvement  (op_points_spec, geo_targets, op_points_result, geo_result, &
                            shape_spec%flap_spec%use_flap, flap_angles, .false.) 
  print *

end subroutine 



function create_airfoil (dv) result (foil) 

  !-------------------------------------------------------------------------------
  !! Create an airfoil out of a seed airfoil and designvars 
  !-------------------------------------------------------------------------------

  use shape_airfoil,      only : shape_spec, CAMB_THICK, BEZIER, HICKS_HENNE 
  use shape_airfoil,      only : create_airfoil_camb_thick
  use shape_airfoil,      only : create_airfoil_hicks_henne
  use shape_airfoil,      only : create_airfoil_bezier
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

    case (CAMB_THICK)

      call create_airfoil_camb_thick (dv_shape, foil)

    case (BEZIER)

      call create_airfoil_bezier (dv_shape, foil)

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

  noppoint = size(op_points_spec)  

  ! init with predefined flap angle 
  
  flap_angles = op_points_spec(:)%flap_angle

  ! retrieve all optimized flap angles from dv 

  flap_angles_optimized = get_flap_angles_optimized (dv)
  iopt = 0  

  ! assign these to op points which have flap to be optimized 

  do i = 1, noppoint

    if (op_points_spec(i)%flap_optimize) then               ! this op angle is optimized

      iopt = iopt + 1
      flap_angles (i) = flap_angles_optimized (iopt)

    else                                                    ! this op angle is fixed

      flap_angles (i) = op_points_spec(i)%flap_angle

    end if 
  end do 

end function  



  subroutine print_not_converged_reason (op_spec, op)

  !------------------------------------------------------------------------------
  !! Print possible reason for op point not converged 
  !------------------------------------------------------------------------------

    type(op_point_spec_type), intent(in)      :: op_spec
    type(op_point_result_type), intent(in)    :: op

    if (op%converged) return 

    if (op_spec%ma%number > 0d0 .and. .not. op_spec%spec_cl) then 

      print *
      call print_note("If mach number > 0.0 and there is a single op point with a high alpha")
      call print_text("xfoil may have difficulties to initalize boundary layer.", 7)
      call print_text("It could help to add an additional op opoint with a lower alpha", 7)
      call print_text("to get an initialized boundary layer.", 7)

    end if 

  end subroutine



subroutine do_dynamic_weighting (designcounter, dyn_weight_spec, &
                                 op_points_result, geo_result, &
                                 dynamic_done) 

  !------------------------------------------------------------------------------
  !! Dynamic weighting
  !!  - recalc weighting of each op-point depending on its deviation to target value
  !!  - Returns new weighting in op_points_spec
  !------------------------------------------------------------------------------

  use xfoil_driver,       only : op_point_result_type, op_point_spec_type
  use math_util,          only : median

  integer, intent(in) :: designcounter
  type(dynamic_weighting_spec_type), intent(in) :: dyn_weight_spec
  type(op_point_result_type), intent(in)        :: op_points_result (:)
  type(geo_result_type),  intent(in)            :: geo_result
  logical, intent(out)                          :: dynamic_done

  type(dynamic_variable_type), dimension(:), allocatable :: dyn_ops, dyn_geos
  doubleprecision, dimension(:), allocatable             :: dyn_devs

  integer                           :: i, ndyn, j, nop, ngeo_targets
  doubleprecision                   :: avg_dev, sum_weighting_user, median_dev, weighting_diff
  doubleprecision                   :: new_dyn_obj_fun, cur_dyn_obj_fun, scale_dyn_obj_fun
  doubleprecision                   :: min_new_weighting,max_new_weighting, min_weighting, max_weighting
  logical                           :: show_dev

  doubleprecision, parameter        :: EXTRA_PUNCH_THRESHOLD = 1.5d0
  doubleprecision, parameter        :: SUPER_PUNCH_THRESHOLD = 3.0d0
  doubleprecision, parameter        :: REDUCTION = 1.3d0

  nop    = size(op_points_spec)
  ngeo_targets = size(geo_targets)

  allocate (dyn_ops(nop))
  allocate (dyn_geos(ngeo_targets))

  dynamic_done = .false. 
  show_dev     = .false.
  
! dyn weighting only if design counter matches frequency
  
  if ((designcounter < dyn_weight_spec%start_with_design) .or. & 
      (mod(designcounter- dyn_weight_spec%start_with_design, dyn_weight_spec%frequency) /= 0)) then
    return
  end if

! get all the data from op points xfoil result and geo targets which are relevant

  ndyn = 0 
  call collect_dyn_ops_data (op_points_result, dyn_ops, ndyn)
  call collect_dyn_geo_data (geo_result,       dyn_geos,ndyn)
  if(ndyn == 0 ) return

! average and median of all deviations 

  allocate (dyn_devs(ndyn))

  j = 0
  do i= 1, nop
    if (op_points_spec(i)%dynamic_weighting) then
      j = j + 1
      dyn_devs(j) = abs( dyn_ops(i)%dev)
    end if
  end do 
  do i= 1, ngeo_targets
    if (geo_targets(i)%dynamic_weighting) then
      j = j + 1
      dyn_devs(j) = abs( dyn_geos(i)%dev)
    end if
  end do 
  avg_dev    = sum    ( dyn_devs (1:j)) / j
  median_dev = median ( dyn_devs (1:j)) 

  write (*,*) 
  call print_colored (COLOR_FEATURE,' - Dynamic Weighting')
  call print_colored (COLOR_PALE,' of '//stri(ndyn)//' targets' //&
                                 ' having an average deviation of '//strf('(F4.1)', avg_dev)//'%' //&
                                 ' and a median of '//strf('(F4.1)', median_dev)//'%')
                            
  write (*,*) 
  write (*,*) 

! Dynamic weighting ----------------------------------------------------------

  min_new_weighting = 9999d0
  max_new_weighting = 0d0

  if (designcounter == 0) then 
    ! first initial design: start with user defined / default weighting
    dyn_ops%new_weighting = dyn_ops%weighting
  else

  ! 1. first guess of new weighting of relevant op_points and new objective function
  !    weighting is proportional to the deviation to target compared to average deviation

    do i= 1, nop
      if (op_points_spec(i)%dynamic_weighting) then

        dyn_ops(i)%new_weighting = abs(dyn_ops(i)%dev) / (avg_dev) 

        min_new_weighting = min (dyn_ops(i)%new_weighting, min_new_weighting)
        max_new_weighting = max (dyn_ops(i)%new_weighting, max_new_weighting)
      end if
    end do 
  
    do i= 1, ngeo_targets
      if (geo_targets(i)%dynamic_weighting) then

        dyn_geos(i)%new_weighting = abs(dyn_geos(i)%dev) / avg_dev 

        min_new_weighting = min (dyn_geos(i)%new_weighting, min_new_weighting)
        max_new_weighting = max (dyn_geos(i)%new_weighting, max_new_weighting)
      end if
    end do 

  ! 1b. Reduce weighting range when average deviation getting close to 0 
  !         to avoid oscillation  

    min_weighting     = dyn_weight_spec%min_weighting  
    max_weighting     = dyn_weight_spec%max_weighting

    weighting_diff = max_weighting - min_weighting

    if (avg_dev < 0.1d0) then 
      weighting_diff = weighting_diff / REDUCTION ** 2 
    elseif (avg_dev < 0.5d0) then 
      weighting_diff = weighting_diff / REDUCTION 
    end if 
    max_weighting     = min_weighting + weighting_diff
    if (show_dev) write (*,'(8x,A,2F5.2)') '- Min / Max     weighting  ',min_weighting, max_weighting 
    if (show_dev) write (*,'(8x,A,2F5.2)') '- Min / Max new weighting  ',min_new_weighting, max_new_weighting 

  
  ! 2. Scale weighting of each op point to defined weighting range 

    do i= 1, nop
      if (op_points_spec(i)%dynamic_weighting) then

        dyn_ops(i)%new_weighting = min_weighting + &
                      (dyn_ops(i)%new_weighting - min_new_weighting) * &
                      ((max_weighting-min_weighting) / (max_new_weighting-min_new_weighting)) 

        if (show_dev) write (*,'(8x, A,I2,3x, A,F6.2,A,F5.1)', advance='no') '- Op ', i, &
                 ' dev:',dyn_ops(i)%dev,'  weight:', dyn_ops(i)%new_weighting

    ! 2a. Outlier - give a super extra punch if deviation is too far away from average deviation

        if ((abs(dyn_ops(i)%dev) > (avg_dev * SUPER_PUNCH_THRESHOLD )) .and. &
                 (avg_dev > 0.3d0)) then

          dyn_ops(i)%new_weighting = dyn_ops(i)%new_weighting * dyn_weight_spec%extra_punch **2
          op_points_spec(i)%extra_punch = .true.
          if (show_dev) write (*,'(A, F5.1)') ' +punch:', dyn_ops(i)%new_weighting

    ! 2b. Bad Op - give a extra punch if deviation is quite far away from average deviation

          elseif ((abs(dyn_ops(i)%dev) > (avg_dev * EXTRA_PUNCH_THRESHOLD )) .and. &
                (avg_dev > 0.1d0)) then

          dyn_ops(i)%new_weighting = dyn_ops(i)%new_weighting * dyn_weight_spec%extra_punch
          op_points_spec(i)%extra_punch = .true.
          if (show_dev) write (*,'(A, F5.1)') '  punch:', dyn_ops(i)%new_weighting

    ! 2c. Former bad Op - also give a good, at least medium weighting if op had a punch before
    !                     to avoid oscillation of results 

        elseif (op_points_spec(i)%extra_punch) then 
          dyn_ops(i)%new_weighting = & 
              max (dyn_ops(i)%new_weighting, (max_weighting * 0.9d0))
          op_points_spec(i)%extra_punch = .false.
          if (show_dev) write (*,'(A, F5.1)') '   post:', dyn_ops(i)%new_weighting

        else
          op_points_spec(i)%extra_punch = .false.
          if (show_dev) write (*,*)
        end if
      end if
    end do 

    do i= 1, ngeo_targets
      if (geo_targets(i)%dynamic_weighting) then

      ! 2b. Scale current deviation range to defined weighting range and set weighting for op point
        dyn_geos(i)%new_weighting = min_weighting + &
                      (dyn_geos(i)%new_weighting - min_new_weighting) * &
                      ((max_weighting-min_weighting) / (max_new_weighting-min_new_weighting)) 
                      
        if (show_dev) write (*,'(8x, A,I2,3x, A,F6.2,A,F5.1)', advance='no') '- Geo', i, &
                 ' dev:',dyn_geos(i)%dev,'  weight:', dyn_geos(i)%new_weighting

      ! 3b. give an extra punch if deviation is too far away from average deviation

        if ((abs(dyn_geos(i)%dev) > (median_dev * EXTRA_PUNCH_THRESHOLD )) .and. &
                (avg_dev  > 0.2d0)) then
          dyn_geos(i)%new_weighting = dyn_geos(i)%new_weighting * dyn_weight_spec%extra_punch
          geo_targets(i)%extra_punch = .true.
          if (show_dev) write (*,'(A, F5.1)') '  punch:', dyn_geos(i)%new_weighting

        elseif (geo_targets(i)%extra_punch) then 
        ! ... also give a good, at least medium weighting if op had a punch before
        !     to avoid oscillation of results 
          dyn_geos(i)%new_weighting = & 
              max (dyn_geos(i)%new_weighting, (max_weighting + min_weighting) / 2d0)
          geo_targets(i)%extra_punch = .false.
          if (show_dev) write (*,'(A, F5.1)') '   post:', dyn_geos(i)%new_weighting
        else
          geo_targets(i)%extra_punch = .false.
          if (show_dev) write (*,*)
        end if
      end if
    end do 

  ! 4. Multiply weighting by user defined weighting - default = 1 but user may overwrite  

    do i= 1, nop
      if (op_points_spec(i)%dynamic_weighting) &
        dyn_ops(i)%new_weighting = dyn_ops(i)%new_weighting * op_points_spec(i)%weighting_user 
    end do 

    do i= 1, ngeo_targets
      if (geo_targets(i)%dynamic_weighting) &
        dyn_geos(i)%new_weighting = dyn_geos(i)%new_weighting * geo_targets(i)%weighting_user
    end do 

  ! 5. cur and new objective function with new (raw) weightings)

    cur_dyn_obj_fun = aero_objective_function (op_points_result, .true.) + &
                      geo_objective_function (geo_result, .true. )

    op_points_spec%weighting = dyn_ops%new_weighting
    geo_targets%weighting    = dyn_geos%new_weighting

    new_dyn_obj_fun = aero_objective_function (op_points_result, .true.) + &
                      geo_objective_function  (geo_result, .true. )


  ! 6. Done - scale and assign new weightings to current optimization weightings


    scale_dyn_obj_fun = cur_dyn_obj_fun / new_dyn_obj_fun

    do i= 1, nop
      if (op_points_spec(i)%dynamic_weighting) &
        op_points_spec(i)%weighting = dyn_ops(i)%new_weighting * scale_dyn_obj_fun
    end do
    do i= 1, ngeo_targets
      if (geo_targets(i)%dynamic_weighting) &
        geo_targets(i)%weighting    = dyn_geos(i)%new_weighting * scale_dyn_obj_fun
    end do

  ! 7. Store data for user information 
                         
    sum_weighting_user = sum(op_points_spec%weighting_user) + &
                         sum(geo_targets%weighting_user)
 
    op_points_spec%weighting_user_prv = dyn_ops%weighting        * sum_weighting_user
    op_points_spec%weighting_user_cur = op_points_spec%weighting * sum_weighting_user

    geo_targets%weighting_user_prv    = dyn_geos%weighting       * sum_weighting_user
    geo_targets%weighting_user_cur    = geo_targets%weighting    * sum_weighting_user

    dynamic_done = .true. 

  end if

end subroutine



subroutine collect_dyn_ops_data (op_points_result, dyn_ops, ndyn)

  !!  get the data for dynamic op points from op points spec and xfoil results

  use xfoil_driver,       only : op_point_result_type, op_point_spec_type

  type(op_point_result_type), dimension(:), intent(in)         :: op_points_result
  type(dynamic_variable_type), dimension(:), intent(inout)     :: dyn_ops
  integer, intent(inout)           :: ndyn

  type(op_point_spec_type) :: op_spec
  type(op_point_result_type)        :: op
  character(15)                     :: opt_type
  integer                           :: i
  doubleprecision                   :: dist, cur_value
  
  dyn_ops%weighting     = op_points_spec%weighting
  dyn_ops%new_weighting = op_points_spec%weighting
  dyn_ops%dev = 0d0

  do i= 1, size(op_points_spec)

    opt_type = op_points_spec(i)%optimization_type
    op_spec  = op_points_spec(i)
    op       = op_points_result(i) 

    if (op%converged) then

      if (op_spec%dynamic_weighting) ndyn = ndyn + 1

      select case  (opt_type)

        case ('target-drag')

          cur_value = op%cd

          if (op_spec%allow_improved_target) then
            dist = max (0d0, (cur_value - op_spec%target_value))
          else 
            dist = abs(cur_value - op_spec%target_value)
            if (dist < 0.000004d0) dist = 0d0         ! little threshold to achieve target
          end if 
    
          dyn_ops(i)%dev = dist / op_spec%target_value * 100d0

        case ('target-glide')

          cur_value = op%cl / op%cd

          if (op_spec%allow_improved_target) then
            dist = max (0d0, (op_spec%target_value - cur_value))
          else 
            dist = abs(cur_value - op_spec%target_value)
            if (dist < 0.01d0) dist = 0d0         ! little threshold to achieve target
          end if 
  
          dyn_ops(i)%dev = dist / op_spec%target_value * 100d0

        case ('target-lift')

          cur_value = op%cl

          if (op_spec%allow_improved_target) then
            dist = max (0d0, (op_spec%target_value - cur_value))
          else 
            dist = abs(cur_value - op_spec%target_value)
            if (dist < 0.001d0) dist = 0d0         ! little threshold to achieve target
          end if 
    
          dyn_ops(i)%dev = dist / (1d0 + op_spec%target_value) * 100d0

        case ('target-moment')

          cur_value = op%cm

          if (op_spec%allow_improved_target) then
            dist = max (0d0, (op_spec%target_value - cur_value))
          else 
            dist = abs(cur_value - op_spec%target_value)
            if (dist < 0.001d0) dist = 0d0         ! little threshold to achieve target
          end if 
  
          dyn_ops(i)%dev  = dist / (0.05d0 + op_spec%target_value) * 100d0

      end select
    end if
  end do

end subroutine collect_dyn_ops_data



subroutine collect_dyn_geo_data (geo_result, dyn_geos, ndyn)

  !! get the data for dynamic op points from geo targets spec and geo results

  type(geo_result_type), intent(in)                        :: geo_result
  type(dynamic_variable_type), dimension(:), intent(inout) :: dyn_geos
  integer, intent(inout)            :: ndyn
  integer                           :: i
  doubleprecision                   :: dist
  
  dyn_geos%weighting     = geo_targets%weighting
  dyn_geos%new_weighting = geo_targets%weighting
  dyn_geos%dev = 0d0

  do i= 1, size(geo_targets)

      if (geo_targets(i)%dynamic_weighting) ndyn = ndyn + 1

      select case  (geo_targets(i)%type)

        case ('thickness')

          dist = geo_result%maxt - geo_targets(i)%target_value       ! positive is worse
          dyn_geos(i)%dev = dist / geo_targets(i)%target_value * 100d0
          
        case ('camber')

          dist = geo_result%maxc - geo_targets(i)%target_value       ! positive is worse
          dyn_geos(i)%dev = dist / geo_targets(i)%target_value * 100d0

        case default
          call my_stop("Unknown geo target_type '"//geo_targets(i)%type)
        end select

      end do

end subroutine collect_dyn_geo_data

end module


! MIT License
! Copyright (C) 2017-2019 Daniel Prosser
! Copyright (c) 2022-2024 Jochen Guenzel

module eval

  ! Core module: Sets up and evaluates the objective function for an airfoil design

  use commons       
  use os_util
  use xfoil_driver, only : xfoil_options_type, xfoil_geom_options_type
  use xfoil_driver, only : op_point_spec_type, re_type
  use xfoil_driver, only : op_point_result_type

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

  public :: get_ndv_of_flaps
  public :: get_dv0_of_flaps


  double precision, parameter, public  :: OBJ_XFOIL_FAIL = 55.55d0
  double precision, parameter, public  :: OBJ_GEO_FAIL   = 1000d0


  ! ---- static, private ---------------------------------

  ! these evaluation specifications are loaded at the beginnning of optimization 
  ! and are static, private to this module to ensure encapsulation 

  integer                                 :: noppoint
  type (op_point_spec_type),  allocatable :: op_points_spec (:)
  type (geo_target_type), allocatable     :: geo_targets (:) 

  type (geo_constraints_type)             :: geo_constraints 
  type (curv_constraints_type)            :: curv_constraints

  type (flap_spec_type)                   :: flap_spec

  type (xfoil_options_type), private      :: xfoil_options
  type (xfoil_geom_options_type), private :: xfoil_geom_options

  type (dynamic_weighting_spec_type)      :: dynamic_weighting_spec 

  logical                                 :: match_foils
  type (airfoil_type)                     :: foil_to_match
  double precision                        :: match_foils_scale_factor
 

  ! Save the best result of evaluation for write_design (+ dynamic weighting)
  !    so no extra run_xfoil is needed

  double precision                        :: best_objective = 1d0
  type(airfoil_type)                      :: best_foil
  type(op_point_result_type), allocatable :: best_op_points_result (:) 


contains


  function objective_function (dv)

    !-----------------------------------------------------------------------------
    !! The Objective Function 
    !
    ! Evaluate a new design defined by designvars dv
    ! First perfroms geometry evaluation and 
    ! then Xfoil aero calculation at operating points    
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

    call create_airfoil_from_designvars (dv, foil)

    ! Geometry constraints violations? 

    geo_penalty = geo_penalty_function (foil)

    
    if(geo_penalty >= OBJ_GEO_FAIL) then          ! if yes - further evaluation is not needed

      objective_function = geo_penalty

    else

      ! Objective function - special treatment for match_foil mode

      if (.false.) then      !match_foils

        geo   = matchfoil_objective_function(foil) 

      ! Objective function - the master 

      else

        ! if flaps activated, the flap angle at an op will be part of the design space
        allocate (flap_angles(noppoint))
        call get_flap_angles_from_dv (dv, flap_angles)

        ! evaluate the foil with xfoil ...
        op_points_result = aero_objective_results (foil, flap_angles) 

        do i = 1, noppoint
          if (.not. op_points_result(i)%converged) then 
            aero = OBJ_XFOIL_FAIL
            exit 
          end if
        end do
      
        if(aero == OBJ_XFOIL_FAIL) then
          objective_function = aero              ! return just fail value for further detection
        else

          aero = aero_objective_function (op_points_result)

          ! evalute geo targets 
          geo_result = geo_objective_results (foil)      
          geo  = geo_objective_function (geo_result )

          objective_function = aero + geo + geo_penalty

          ! Save the best result to be written later at write_design ...
          !$omp critical
          if (objective_function < best_objective) then
            best_objective = objective_function 
            best_foil = foil 
            best_op_points_result = op_points_result
          end if 
          !$omp end critical
        end if

      end if 
      
    end if

  end function objective_function



  subroutine set_eval_spec (eval_spec)

    !-----------------------------------------------------------------------------
    !! sets eval specification into static structures for later evaluation  
    !-----------------------------------------------------------------------------

    type (eval_spec_type), intent(in)  :: eval_spec 

    op_points_spec          = eval_spec%op_points_spec
    noppoint                = size(op_points_spec)
    geo_targets             = eval_spec%geo_targets

    geo_constraints         = eval_spec%geo_constraints
    curv_constraints        = eval_spec%curv_constraints

    flap_spec               = eval_spec%flap_spec

    xfoil_options           = eval_spec%xfoil_options
    xfoil_geom_options      = eval_spec%xfoil_geom_options
    
    dynamic_weighting_spec  = eval_spec%dynamic_weighting_spec

    match_foils             = eval_spec%match_foils
    foil_to_match           = eval_spec%foil_to_match
    match_foils_scale_factor = eval_spec%match_foils_scale_factor
  
  end subroutine 
  


  subroutine eval_seed_scale_objectives (seed_foil)

    !----------------------------------------------------------------------------------
    !! evaluates seed airfoil to scale objectives to achieve objective function = 1.0
    !----------------------------------------------------------------------------------

    use commons

    use eval_commons 
    use math_deps,            only : interp_point, derivation_at_point, smooth_it, norm_2
    use xfoil_driver,         only : run_op_points, op_point_result_type, xfoil_defaults

    type (airfoil_type), intent(in)           :: seed_foil

    type(op_point_spec_type)                  :: op_spec
    type(op_point_result_type)                :: op
    type(op_point_result_type), allocatable   :: op_points_result (:)
    type(geo_result_type)                     :: geo_result
    type(xfoil_options_type)                  :: local_xfoil_options
    double precision, allocatable             :: flap_degrees (:) 

    double precision :: correction
    double precision :: slope
    double precision :: checkval
    double precision :: pi
    integer :: i
    character(:), allocatable  :: opt_type
    ! logical :: addthick_violation
    double precision :: ref_value, seed_value, tar_value, cur_value
    double precision :: dist = 0d0

    ! Analyze airfoil at requested operating conditions with Xfoil

    write (*,'(" - ",A)') 'Analyze seed airfoil at requested operating points'

    ! Re-Init boundary layer at each op point to ensure convergence (slower)
    local_xfoil_options = xfoil_options
    ! local_xfoil_options%reinitialize = .false.    ! strange: reinit leeds sometimes to not converged
    local_xfoil_options%show_details = .false.

    ! #todo get flap degrees x0 
    allocate (flap_degrees(size(op_points_spec)))
    flap_degrees = 0d0 
    
    call run_op_points (seed_foil, xfoil_geom_options, local_xfoil_options,        &
                        flap_spec, flap_degrees, &
                        op_points_spec, op_points_result)


    ! Evaluate seed value of geomtry targets and scale factor 

    geo_result = geo_objective_results (seed_foil)
      
    do i = 1, size(geo_targets)

      select case (geo_targets(i)%type)

        case ('Thickness')                            ! take foil thickness calculated above
          seed_value = geo_result%maxt
          ref_value  = geo_result%maxt
          correction = 1.2d0                          ! thickness is less sensible to changes

        case ('Camber')                               ! take xfoil camber from  above
          seed_value = geo_result%maxc
          ref_value  = geo_result%maxc
          correction = 0.7d0                          ! camber is quite sensible to changes

        case ('bezier-le-curvature')                  ! curvature top and bot at le is equal 
          seed_value = abs (abs(geo_result%top_curv_le - geo_result%bot_curv_le))        ! difference
          ref_value  = abs (abs(geo_result%top_curv_le + geo_result%bot_curv_le)) / 2d0  ! mean value
          correction = 0.05d0                         ! curv diff changes a lot -> attenuate

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

    do i = 1, noppoint

      op_spec  = op_points_spec(i)
      op       = op_points_result(i) 
      opt_type = op_spec%optimization_type

      ! Check for unconverged points

      if (.not. op%converged) then
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

      ! Op point type 'target-....'
      !      - minimize the difference between current value and target value
      !      - target_value negative?  --> take current seed value * |target_value| 

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
        checkval   = ABS (op_spec%target_value - op%cm) + 0.05d0

      elseif (opt_type == 'max-lift') then

        checkval   = 1.d0/op%cl

      elseif (opt_type == 'max-xtr') then

        checkval   = 1.d0/(0.5d0*(op%xtrt + op%xtrb) + 0.1d0)  ! Ensure no division by 0

      ! jx-mod Following optimization based on slope of the curve of op_point
      !         convert alpha in rad to get more realistic slope values
      !         convert slope in rad to get a linear target 
      !         factor 4.d0*pi to adjust range of objective function (not negative)

      elseif (opt_type == 'max-lift-slope') then

      ! Maximize dCl/dalpha 
        slope = derivation_at_point (i, (op_points_result%alpha * pi/180.d0) , &
                                        (op_points_result%cl))
        checkval   = 1.d0 / (atan(abs(slope))  + 2.d0*pi)

      elseif (opt_type == 'min-lift-slope') then

      ! Minimize dCl/dalpha e.g. to reach clmax at alpha(i) 
        slope = derivation_at_point (i, (op_points_result%alpha * pi/180.d0) , &
                                        (op_points_result%cl))
        checkval   = atan(abs(slope)) + 2.d0*pi

      elseif (opt_type == 'min-glide-slope') then

      ! Minimize d(cl/cd)/dcl e.g. to reach best glide at alpha(i) 
        slope = derivation_at_point (i, (op_points_result%cl * 20d0), &
                                        (op_points_result%cl/op_points_result%cd))
        checkval   = atan(abs(slope)) + 2.d0*pi
      
      else
        call my_stop ("Unknown optimization_type for operating point "// stri(i))
      end if

      op_spec%scale_factor = 1.d0/checkval

      op_points_spec(i) = op_spec             ! write back target, scale, ...

    end do
    
  end subroutine eval_seed_scale_objectives



  function is_design_valid (dv)

    !-----------------------------------------------------------------------------
    !! returns .true. if the design variables result in a geometric valid design  
    !-----------------------------------------------------------------------------

    double precision, intent(in)  :: dv(:) 

    type (airfoil_type)     :: foil
    logical                 :: is_design_valid

    call create_airfoil_from_designvars (dv, foil)

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
    character (:), allocatable  :: info

    penalty = OBJ_GEO_FAIL

    ! Check for curvature constraints (when using Hicks-Henne)

    if (curv_constraints%check_curvature) then

      ! top side 

      call eval_side_curvature_violations (foil%top, curv_constraints%top, has_violation, info)
      if (has_violation) return

      ! bot side 
      
      if (.not. foil%symmetrical) then 

        call eval_side_curvature_violations (foil%bot, curv_constraints%bot, has_violation, info)
        if (has_violation) return

      end if 
    end if 


    ! Check geometry contraints  - resulting in penalties added  

    call eval_geometry_violations (foil, geo_constraints, has_violation, info)
    if (has_violation) return


    ! no violations detected 

    penalty = 0d0

  end function geo_penalty_function



  function geo_objective_results (foil) result (geo_result) 

    !----------------------------------------------------------------------------
    !! eval geometry results  
    !----------------------------------------------------------------------------

    use xfoil_driver,   only : xfoil_set_airfoil, xfoil_get_geometry_info
    use shape_bezier,   only : bezier_curvature

    type(airfoil_type), intent(in)  :: foil
    type(geo_result_type)           :: geo_result 

    
    call xfoil_set_airfoil (foil)        
    call xfoil_get_geometry_info (geo_result%maxt, geo_result%xmaxt, &
                                  geo_result%maxc, geo_result%xmaxc)

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

    use math_deps,          only : derivation_at_point

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

        select case (geo_targets(i)%type)

          case ('Thickness')                            
            cur_value  = geo_result%maxt
            correction = 1.2d0                          ! thickness is less sensible to changes

          case ('Camber')                               
            cur_value  = geo_result%maxc
            correction = 0.7d0                          ! camber is quite sensible to changes

          case ('bezier-le-curvature')                  ! curvature top and bot at le is equal 
            cur_value  = abs(geo_result%top_curv_le - geo_result%bot_curv_le)
            correction = 0.05d0                         ! curv diff changes a lot -> attenuate
    
          case default
            call my_stop("Unknown target_type '"//geo_targets(i)%type)
        end select

        ref_value = geo_targets(i)%reference_value
        tar_value = geo_targets(i)%target_value

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

    use xfoil_driver,       only : run_op_points, xfoil_set_airfoil, op_point_result_type

    type(airfoil_type), intent(in)  :: foil
    double precision, intent(in)    :: flap_angles (:)

    type(op_point_result_type), allocatable :: op_points_result (:) 
    type(xfoil_options_type)                :: local_xfoil_options

    ! Analyze airfoil at requested operating conditions with Xfoil

    call xfoil_set_airfoil (foil) 
    
    local_xfoil_options = xfoil_options
    local_xfoil_options%show_details        = .false.  ! switch off because of multi-threading
    local_xfoil_options%exit_if_unconverged = .true.   ! speed up if an op point uncoverges

    call run_op_points (foil, xfoil_geom_options, local_xfoil_options,        &
                        flap_spec, flap_angles, &
                        op_points_spec, op_points_result)

  end function



  function aero_objective_function (op_points_result, eval_only_dynamic_ops)

    !-----------------------------------------------------------------------------
    !!  Objective function as result of aerodynamic evaluation
    !!
    !!  Input:   op points results from xfoil calculation
    !!  Output:  objective function value based on airfoil performance
    !-----------------------------------------------------------------------------

    use math_deps,          only : derivation_at_point
    use xfoil_driver,       only : op_point_result_type

    type(op_point_result_type), intent(in) :: op_points_result (:)
    logical,  intent(in), optional         :: eval_only_dynamic_ops

    double precision                  :: aero_objective_function
    type(op_point_spec_type)          :: op_spec
    type(op_point_result_type)        :: op
    integer          :: i
    double precision :: pi
    double precision :: cur_value, slope, increment, dist, correction
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

        ! Following optimization based on slope of the curve of op_point
        !         convert alpha in rad to get more realistic slope values
        !         convert slope in rad to get a linear target 
        !         factor eg 4.d0*pi to adjust range of objective function (not negative)

        elseif (opt_type == 'max-lift-slope') then

        ! Maximize dCl/dalpha (0.1 factor to ensure no division by 0)

          slope = derivation_at_point (i, (op_points_result%alpha * pi/180.d0) , &
                                          (op_points_result%cl))
          increment = op_spec%scale_factor / (atan(abs(slope))  + 2.d0*pi)
          cur_value = atan(abs(slope))

        elseif (opt_type == 'min-lift-slope') then

        ! Minimize dCl/dalpha e.g. to reach clmax at alpha(i) 
          slope = derivation_at_point (i, (op_points_result%alpha * pi/180.d0) , &
                                          (op_points_result%cl))

          increment = op_spec%scale_factor * (atan(abs(slope)) + 2.d0*pi)
          cur_value = atan(abs(slope))

        elseif (opt_type == 'min-glide-slope') then

        ! Minimize d(cl/cd)/dcl e.g. to reach best glide at alpha(i) 
          slope = derivation_at_point (i, (op_points_result%cl * 20d0), &
                                          (op_points_result%cl/op_points_result%cd))

          increment = op_spec%scale_factor * (atan(abs(slope))  + 2.d0*pi)
          cur_value = atan(abs(slope))  

        else

          call my_stop ("Requested optimization_type not recognized.")

        end if

        aero_objective_function = aero_objective_function &
                                  + op_spec%weighting * increment
      end if
    end do

  end function 



!=============================================================================80
!
! Objective function for matching one airfoil to another (for testing shape
! functions, optimization algorithms, etc.).  Assumes x-values of points line
! up; this should be handled before optimizing.
!
!=============================================================================80
function matchfoil_objective_function(foil)

  use math_deps,       only : norm_2

  type(airfoil_type), intent(in)    :: foil
  double precision :: matchfoil_objective_function
  double precision :: match_delta
  integer          :: nptt, nptb

  nptt = size(foil%top%x)
  nptb = size(foil%bot%x)

! Evaluate the new airfoil, (not-> changed)  counting fixed LE and TE points

  match_delta = norm_2(foil%top%y(2:nptt-1) - foil_to_match%top%y(2:nptt-1)) + &
                norm_2(foil%bot%y(2:nptb-1) - foil_to_match%bot%y(2:nptb-1))
  !if (match_delta < 1d-10)  match_delta = 1d-1 

  ! Scale result to initial value 1.
  matchfoil_objective_function = match_delta * match_foils_scale_factor

end function matchfoil_objective_function



subroutine write_progress (dv, designcounter)

  !! Generic function to write designs. Selects either match_foils or 'normal'

  double precision, dimension(:), intent(in) :: dv
  integer, intent(in) :: designcounter
  integer :: write_stat                     ! currently not used 

  if (match_foils) then
    write_stat = write_progress_matchfoil(dv, designcounter)
  else
    write_stat = write_progress_airfoil_optimization(dv, designcounter)
  end if

end subroutine write_progress



function write_progress_airfoil_optimization(dv, designcounter)

  !-----------------------------------------------------------------------------
  !! Writes airfoil coordinates and op Points results to files during optimization
  !!   designcounter = 0 will start new files 
  !-----------------------------------------------------------------------------

  use math_deps,            only : interp_vector, min_threshold_for_reversals
  use airfoil_operations,   only : airfoil_write_to_unit
  use xfoil_driver,         only : run_op_points, op_point_result_type

  use shape_airfoil,        only : shape_spec, BEZIER, get_seed_foil

  double precision, intent(in)  :: dv (:)
  integer, intent(in)           :: designcounter
  integer :: write_progress_airfoil_optimization

  type(airfoil_type)                :: foil
  type(xfoil_options_type)          :: local_xfoil_options
  type(geo_result_type)             :: geo_result
  type(op_point_result_type), allocatable :: op_points_result (:)

  double precision, dimension(noppoint) :: flap_angles
 
  character(:), allocatable :: foil_file, bezier_file, op_points_file, geo_targets_file
  integer        :: foil_unit, bezier_unit, op_points_unit, geo_unit
  logical        :: dynamic_done
 

  if (designcounter == 0) then
    call print_colored (COLOR_NORMAL,' - Writing design #0 being seed airfoil')
  else
    call print_colored (COLOR_NORMAL,' -> Writing design #'//stri(designcounter))
  end if
  write (*,*)
  if (show_details .and. (designcounter > 0)) write (*,*) 

  ! Design 0 is seed airfoil to output - take the original values 
  if (designcounter == 0) then 
    call get_seed_foil (foil)
  ! Design > 0 - Build current foil out seed foil and current design 
  else 
    call create_airfoil_from_designvars (dv, foil)
    foil%name = output_prefix
  end if

  ! Get actual flap angles based on design variables

  call get_flap_angles_from_dv (dv, flap_angles)

  ! Try to get xfoil result for foil from "save best" in objective function

  if (allocated(best_op_points_result)) then 
    ! Sanity check - Is the "best" really our current foil
    if (abs(sum(foil%y) - sum(best_foil%y)) < 1d-10 ) then  ! use epsilon (num issues with symmetrical) 
      op_points_result = best_op_points_result
    else
      best_objective = 1d0                 ! reset best store - something wrong...?
    end if 
  end if 

  ! There is no stored result - so re-calc for this foil
  if (.not. allocated(op_points_result)) then 

    ! Analyze airfoil at requested operating conditions with Xfoil

    local_xfoil_options = xfoil_options
    local_xfoil_options%show_details        = .false.  
    local_xfoil_options%exit_if_unconverged = .false.  ! we need all op points
    if (designcounter == 0) then
      local_xfoil_options%reinitialize = .false.       ! strange: reinit leeds sometimes to not converged
      local_xfoil_options%show_details = .false.
    end if 
    call run_op_points (foil, xfoil_geom_options, local_xfoil_options,  &
                        flap_spec, flap_angles, &
                        op_points_spec, op_points_result)
  end if 
     
  ! Set output file names and identifiers

  foil_file       = design_subdir//'Design_Coordinates.csv'
  op_points_file  = design_subdir//'Design_OpPoints.csv'
  bezier_file     = design_subdir//'Design_Beziers.csv'
  geo_targets_file= design_subdir//'Design_GeoTargets.csv'

  foil_unit       = 13
  op_points_unit  = 14
  bezier_unit     = 15
  geo_unit        = 16

  ! Open files of design data ...

  if (designcounter == 0) then

    ! ... design=0 (seed airfoil), write header and opPoint specifications

    open(unit=op_points_unit, file=op_points_file, status='replace', err=901)
    call write_design_op_points_header (op_points_unit)

    if (shape_spec%type == BEZIER) then 
      open(unit=bezier_unit, file= bezier_file, status='replace', err=902)
      call write_design_bezier_header (bezier_unit, foil)
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
  close (geo_unit)


  ! ----- Actions when new design was found --------------------------------------


  ! Dynamic Weighting of op points and geo targets

  if (dynamic_weighting_spec%active) then 
    call do_dynamic_weighting (designcounter, dynamic_weighting_spec, & 
                               op_points_result, geo_result, dynamic_done)
  else
    dynamic_done = .false.
  end if

  if (show_details .and. (designcounter > 0)) then 
    call print_improvement  (op_points_spec, geo_targets, &
                            op_points_result, geo_result, dynamic_done) 

    call violation_stats_print ()
  end if

  ! Testing:  Write op points deviation to file
  !  call write_op_results (designcounter, op_points_result, geo_result) 
  !  call write_designvars (designcounter, dv) 
  !  call print_designvars (designcounter, dv) 

  write_progress_airfoil_optimization = 0

  return

  ! File I/O Warnings 

  900 call print_warning ("Warning: unable to open "//foil_file//". Skipping ...")
    write_progress_airfoil_optimization = 1
    return
  901 call print_warning ("Warning: unable to open "//op_points_file//". Skipping ...")
    write_progress_airfoil_optimization = 2
    return
  902 call print_warning ("Warning: unable to open "//bezier_file//". Skipping ...")
    write_progress_airfoil_optimization = 3
    return
  903 call print_warning ("Warning: unable to open "//geo_targets_file//". Skipping ...")
    write_progress_airfoil_optimization = 4
    return

end function write_progress_airfoil_optimization



function write_progress_matchfoil(dv, designcounter)

  !-----------------------------------------------------------------------------
  !! Writes airfoil coordinates during matchfoil optimization 
  !-----------------------------------------------------------------------------

  use xfoil_driver,       only : xfoil_set_airfoil, xfoil_get_geometry_info
  use airfoil_operations, only : airfoil_write_to_unit

  use shape_airfoil,      only: get_seed_foil


  double precision, dimension(:), intent(in) :: dv
  integer, intent(in) :: designcounter
  integer :: write_progress_matchfoil

  type(airfoil_type)       :: foil
  double precision :: maxt, xmaxt, maxc, xmaxc
  character(8) :: maxtchar, xmaxtchar, maxcchar, xmaxcchar


  character(:), allocatable :: foil_file, title
  integer :: foil_unit


  ! Set output file names and identifiers

  foil_file = design_subdir//'Design_Coordinates.dat'
  foil_unit = 13

  if (designcounter == 0) then
    call print_colored (COLOR_NORMAL,' - Writing seed airfoil')
  else
    write (*,'(2x,A)', advance ='no') '-> Writing design '
    call  print_colored (COLOR_NORMAL,'#'//stri(designcounter))
  end if
  write (*,*)


  ! Design 0 is seed airfoil to output - take the original values 
  !     Smoothing - Restore the original, not smoothed seed airfoil to
  !                 ...design_coordinates.dat to show it in visualizer
  if (designcounter == 0) then
    call get_seed_foil (foil)
  ! Design > 0 - Build current foil out seed foil and current design 
  else 
    call create_airfoil_from_designvars (dv, foil)
    foil%name = output_prefix
  end if

  call xfoil_set_airfoil (foil)
  call xfoil_get_geometry_info(maxt, xmaxt, maxc, xmaxc)
  write(maxtchar,'(F8.5)') maxt
  maxtchar = adjustl(maxtchar)
  write(xmaxtchar,'(F8.5)') xmaxt
  xmaxtchar = adjustl(xmaxtchar)
  write(maxcchar,'(F8.5)') maxc
  maxcchar = adjustl(maxcchar)
  write(xmaxcchar,'(F8.5)') xmaxc
  xmaxcchar = adjustl(xmaxcchar)

  ! Open file and write header, if necessary

  if (designcounter == 0) then

    ! New File: Header for coordinate file & Seed foil 
    open(unit=foil_unit, file=foil_file, status='replace')
    write(foil_unit,'(A)') 'title="Airfoil coordinates"'
    write(foil_unit,'(A)') 'variables="x" "z"'
    title =  'zone t="Seed airfoil, '//'name='//foil%name//', maxt='//trim(maxtchar)//&
             ', xmaxt='//trim(xmaxtchar)//', maxc='//&
              trim(maxcchar)//', xmaxc='//trim(xmaxcchar)//'"'
    
  else

    ! Append to file: Header for design foil coordinates
    open(unit=foil_unit, file=foil_file, status='old', position='append', err=910)
    title =  'zone t="Airfoil, '//'name='//foil%name//', maxt='//trim(maxtchar)//&
             ', xmaxt='//trim(xmaxtchar)//', maxc='//&
              trim(maxcchar)//', xmaxc='//trim(xmaxcchar)//'", '//&
             'SOLUTIONTIME='//stri(designcounter)
  end if

  call  airfoil_write_to_unit (foil_unit, title, foil)

  close(foil_unit)


  ! Append the coordinates of the match foil when seed foil is written
  if (designcounter == 0) then
    call print_colored (COLOR_NORMAL,' - Writing airfoil to match')
    write (*,*)
    title = 'zone t="Match airfoil, '//'name='//foil_to_match%name//'"'
    open(unit=foil_unit, file=foil_file, status='old', position='append', err=910)
    call  airfoil_write_to_unit (foil_unit, title, foil_to_match)
    close(foil_unit)  
  end if 

  write_progress_matchfoil = 0

  return

  ! Warning if there was an error opening design_coordinates file

  910 write(*,*) "Warning: unable to open "//trim(foil_file)//". Skipping ..."
  write_progress_matchfoil = 1
  return

end function 



subroutine write_final_results (dv, steps, fevals, f0, fmin, final_foil)

  !-----------------------------------------------------------------------------
  !! Writes final airfoil design 
  !!    Returns final airfoil 
  !-----------------------------------------------------------------------------

  use commons
  use airfoil_operations,     only : airfoil_write
  use xfoil_driver,           only : run_op_points, op_point_result_type
  use xfoil_driver,           only : op_point_spec_type

  use shape_bezier,           only : write_bezier_file
  use shape_hicks_henne,      only : write_hh_file

  double precision, allocatable, intent(in) :: dv (:) 
  integer, intent(in)                       :: steps, fevals
  double precision, intent(in)              :: f0, fmin
  type(airfoil_type), intent(out)           :: final_foil

  type(op_point_spec_type)                :: op_spec
  type(op_point_result_type)              :: op
  type(op_point_result_type), allocatable :: op_points_result (:)
  double precision, allocatable           :: flap_degrees (:) 
  integer                   :: i, iunit
  character(:), allocatable :: aero_file
  character(20)             :: flapnote
  double precision          :: ncrit

  print *
  print *,'Optimization complete. Totals: '
  print *,'  Steps: '//stri(steps)//'   Objective function evaluations: '//stri(fevals)

  ! create final airfoil and flap angles from designvars 

  call create_airfoil_from_designvars (dv, final_foil)
  call get_flap_angles_from_dv   (dv, flap_degrees)

  ! analyze final design

  if (.not. match_foils) then

    ! Run xfoil for requested operating points

    call run_op_points (final_foil, xfoil_geom_options, xfoil_options,        &
                        flap_spec, flap_degrees,  &
                        op_points_spec, op_points_result)

    ! Write summary to screen and file

    aero_file  = design_subdir//'Performance_Summary.dat'

    iunit = 13
    open(unit=iunit, file=aero_file, status='replace')

    write(*,*)
    write(*    ,'(A)') " Optimal airfoil performance summary"
    write(iunit,'(A)') " Optimal airfoil performance summary"
    write(*    ,'(A)') ""
    write(iunit,'(A)') ""

    ! i    alpha     CL        CD       Cm    Top Xtr Bot Xtr   Re      Mach     ncrit     flap 
    ! --  ------- -------- --------- -------- ------- ------- -------- -------- ------- -----------
    !  7  -1.400   0.0042   0.00513  -0.0285  0.7057  0.2705  6.00E+04   0.000     9.1    5.23 spec
    ! I2    F8.3    F9.4     F10.5     F9.4    F8.4    F8.4     ES9.2     F8.3     F7.1    F6.2  

    write (iunit,'(A)') " i   alpha     CL        CD       Cm    Top Xtr Bot Xtr   Re      Mach    ncrit     flap"
    write (iunit,'(A)') " -- ------- -------- --------- -------- ------- ------- ------- -------- ------- -----------"
    write (*    ,'(A)') " i   alpha     CL        CD       Cm    Top Xtr Bot Xtr   Re      Mach    ncrit     flap"
    write (*    ,'(A)') " -- ------- -------- --------- -------- ------- ------- ------- -------- ------- -----------"

    do i = 1, noppoint

      op_spec  = op_points_spec(i)
      op       = op_points_result(i) 

      if (flap_degrees(i) /= 0d0) then
        write (flapnote, '(F6.2)') flap_degrees(i)
        if (op_spec%flap_angle == NOT_DEF_D) then
          flapnote = trim(flapnote) //" opt"
        else
          flapnote = trim(flapnote) //" spec"
        end if 
      else
        flapnote = "   -"
      end if 

      if (op_spec%ncrit == -1d0) then 
        ncrit = xfoil_options%ncrit
      else
        ncrit = op_spec%ncrit
      end if 

      write (iunit,  "(I2,   F8.3,   F9.4,    F10.5, F9.4,   F8.4,   F8.4, ES9.2     F8.3     F7.1, 3X, A)") &
        i, op%alpha, op%cl, op%cd, op%cm, op%xtrt, op%xtrb, &
        op_spec%re%number, op_spec%ma%number, ncrit, trim(flapnote)
      write (*    ,  "(I2,   F8.3,   F9.4,    F10.5, F9.4,   F8.4,   F8.4, ES9.2     F8.3     F7.1, 3X, A)") &
        i, op%alpha, op%cl, op%cd, op%cm, op%xtrt, op%xtrb, &
        op_spec%re%number, op_spec%ma%number, ncrit, trim(flapnote)

    end do

    print *
    call print_colored (COLOR_NORMAL, " Objective function improvement over seed: ")
    call print_colored_r (9, '(F8.4,"%")', Q_GOOD, ((f0 - fmin)/f0*100.d0))

    write(iunit,*)
    write(iunit,'(A43,F8.4,A1)') " Objective function improvement over seed: ",  &
                          (f0 - fmin)/f0*100.d0, "%" 

    close(iunit)

    print *
    call print_text ("- Writing summary to "//trim(aero_file))


  else
    call write_matchfoil_summary (final_foil)
  end if


end subroutine 



subroutine write_matchfoil_summary (final_foil)

  !-----------------------------------------------------------------------------
  !! Write some data of the final match foil 
  !-----------------------------------------------------------------------------

  use commons,            only : airfoil_type
  use shape_airfoil,      only : get_seed_foil
  use math_deps,          only : median
  use xfoil_driver,       only : xfoil_set_airfoil, xfoil_get_geometry_info

  type (airfoil_type), intent(in)  :: final_foil


  type (airfoil_type)   :: seed_foil
  double precision :: max_dzt, max_dzb, avg_dzt, avg_dzb, max_dzt_rel, max_dzb_rel
  double precision :: xmax_dzt, xmax_dzb, median_dzt, median_dzb
  double precision :: maxt, xmaxt, maxc, xmaxc
  integer          :: imax_dzt, imax_dzb

  ! Max Delta and position on top and bot 

  max_dzt  = maxval(abs (final_foil%top%y - foil_to_match%top%y))
  max_dzb  = maxval(abs (final_foil%bot%y - foil_to_match%bot%y))
  imax_dzt = maxloc(abs (final_foil%top%y - foil_to_match%top%y),1)
  imax_dzb = maxloc(abs (final_foil%bot%y - foil_to_match%bot%y),1)


  call get_seed_foil (seed_foil) 

  xmax_dzt = seed_foil%top%x(imax_dzt)
  xmax_dzb = seed_foil%bot%x(imax_dzb)

  ! rel. deviation  

  call xfoil_set_airfoil (foil_to_match)        
  call xfoil_get_geometry_info (maxt, xmaxt, maxc, xmaxc)

  max_dzt_rel = (max_dzt / maxt) * 100.d0
  max_dzb_rel = (max_dzb / maxt) * 100.d0

  ! absolute average and median of deltas  

  avg_dzt  = sum (abs(final_foil%top%y - foil_to_match%top%y)) / size(seed_foil%top%x,1)
  avg_dzb  = sum (abs(final_foil%bot%y - foil_to_match%bot%y)) / size(seed_foil%bot%x,1)

  median_dzt  = median (final_foil%top%y - foil_to_match%top%y)
  median_dzb  = median (final_foil%bot%y - foil_to_match%bot%y)

  write(*,*)
  write(*,'(A)') " Match airfoil deviation summary"
  write(*,*)
  write(*,'(A)') "      Delta of y-coordinate between best design and match airfoil surface"
  write(*,*)
  write(*,'(A)') "               average      median   max delta      at  of thickness"

  write (*,'(A10)', advance = 'no') "   top:"
  write (*,'(F12.7,F12.7,F12.7,F8.4,F10.3,A1)') avg_dzt, median_dzt, max_dzt, xmax_dzt, max_dzt_rel,'%'
  write (*,'(A10)', advance = 'no') "   bot:"
  write (*,'(F12.7,F12.7,F12.7,F8.4,F10.3,A1)') avg_dzb, median_dzb, max_dzb, xmax_dzb, max_dzb_rel,'%'
  write(*,*)

end subroutine write_matchfoil_summary



subroutine create_airfoil_from_designvars (dv, foil)

  !-------------------------------------------------------------------------------
  !!
  !! Create an airfoil out of a seed airfoil and designvars 
  !!
  !-------------------------------------------------------------------------------

  use commons,             only: airfoil_type

  use shape_airfoil,      only : shape_spec, CAMB_THICK, BEZIER, HICKS_HENNE 
  use shape_airfoil,      only : create_airfoil_camb_thick
  use shape_airfoil,      only : create_airfoil_hicks_henne
  use shape_airfoil,      only : create_airfoil_bezier
  
  type(airfoil_type), intent(out) :: foil
  double precision, intent(in)    :: dv(:)

  double precision, allocatable   :: dv_shape_spec (:)

  ! extract designvars for shape_spec (without flap designvars)



  if (shape_spec%type == CAMB_THICK) then

    dv_shape_spec = dv (1 : shape_spec%camb_thick%ndv)
    call create_airfoil_camb_thick (dv_shape_spec, foil)


  else if (shape_spec%type == BEZIER) then
    
    dv_shape_spec = dv (1 : shape_spec%bezier%ndv)
    call create_airfoil_bezier (dv_shape_spec, foil)

  else if (shape_spec%type == HICKS_HENNE) then

    dv_shape_spec = dv (1 : shape_spec%hh%ndv)
    call create_airfoil_hicks_henne(dv_shape_spec, foil)

  else 

    call my_stop ("Unknown shape type")

  end if

end subroutine create_airfoil_from_designvars


  
function get_ndv_of_flaps () result (ndv)

  !----------------------------------------------------------------------------
  !! no of designvars of flap optimzation (defined in local 'flap_spec') 
  !----------------------------------------------------------------------------

  integer :: ndv

  ndv = flap_spec%ndv

end function 



function get_dv0_of_flaps () result (dv0)

  !----------------------------------------------------------------------------
  !! start values of designvariables (0..1) for flaps to be optimized 
  !!    we'll start in the middle between min and max angle defined in flap_spec 
  !----------------------------------------------------------------------------

  double precision, allocatable :: dv0 (:) 
  double precision              :: min_angle, max_angle, start_angle

  allocate (dv0 (flap_spec%ndv))

  if (flap_spec%ndv == 0) return                ! no flaps to optimize 

  min_angle   = flap_spec%min_flap_degrees
  max_angle   = flap_spec%max_flap_degrees
  start_angle = (max_angle - min_angle) / 2d0 

  ! map angle array to range 0..1
  dv0 = (start_angle - min_angle / (max_angle - min_angle))
    
end function 



subroutine get_flap_angles_from_dv (dv, flap_degrees)

  !----------------------------------------------------------------------------
  !! Get actual flap anglesfrom design vars (if there are...) 
  !! If the flap of an op point is fixed, return the fixed value (normally = 0) 
  !! dv:    all design variables! - of flaps are at the end  
  !----------------------------------------------------------------------------


  use shape_airfoil,      only : get_ndv_of_shape

  double precision, dimension(:), intent(in) :: dv
  double precision, intent(inout) :: flap_degrees (:)

  double precision      :: min_angle, max_angle 
  integer               :: ndv_shape, i, idv

  min_angle   = flap_spec%min_flap_degrees
  max_angle   = flap_spec%max_flap_degrees

  ndv_shape = get_ndv_of_shape ()                 ! design variables of aero optimization
  idv = ndv_shape + 1 

  do i = 1, noppoint

    if (op_points_spec(i)%flap_angle == NOT_DEF_D) then     ! this op angle is optimized

      flap_degrees (i) = min_angle + dv(i) * (max_angle - min_angle)
      idv = idv + 1

    else                                                    ! this op angle is fixed

      flap_degrees (i) = op_points_spec(i)%flap_angle

    end if 
  end do 

end subroutine get_flap_angles_from_dv 




subroutine do_dynamic_weighting (designcounter, dyn_weight_spec, &
                                 op_points_result, geo_result, &
                                 dynamic_done) 

  !------------------------------------------------------------------------------
  !! Dynamic weighting
  !!  - recalc weighting of each op-point depending on its deviation to target value
  !!  - Returns new weighting in op_points_spec
  !------------------------------------------------------------------------------

  use xfoil_driver,       only : op_point_result_type, op_point_spec_type
  use math_deps,          only : median

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

        case ('Thickness')

          dist = geo_result%maxt - geo_targets(i)%target_value       ! positive is worse
          dyn_geos(i)%dev = dist / geo_targets(i)%target_value * 100d0
          
        case ('Camber')

          dist = geo_result%maxc - geo_targets(i)%target_value       ! positive is worse
          dyn_geos(i)%dev = dist / geo_targets(i)%target_value * 100d0

        case ('bezier-le-curvature')                  ! curvature top and bot at le is equal 

          dist  = abs(geo_result%top_curv_le - geo_result%bot_curv_le)
          dyn_geos(i)%dev = dist / geo_targets(i)%reference_value * 100d0

        case default
          call my_stop("Unknown geo target_type '"//geo_targets(i)%type)
        end select

      end do

end subroutine collect_dyn_geo_data

end module


! MIT License

module input_sanity

  use os_util
  use commons
  use print_util
  use string_util,          only : stri, strf

  use geo_target,           only : geo_target_type, GEO_TARGET_MATCH_FOIL
  use eval_commons
  use xfoil_driver,         only : xfoil_options_type
  use op_point,             only : op_point_spec_type, is_target, OPT_MAX_XTR
  use shape_airfoil,        only : shape_spec_type
  use optimization,         only : optimize_spec_type

  implicit none
  private

  public :: check_and_process_inputs


  contains

  subroutine check_and_process_inputs (eval_spec, shape_spec )

    !----------------------------------------------------------------------------
    !! Checks and adapts various inputs to be consistent and valid 
    !----------------------------------------------------------------------------

    use optimization,         only : PSO
    use shape_airfoil,        only : BEZIER, HICKS_HENNE

    type(eval_spec_type), intent(inout)     :: eval_spec
    type(shape_spec_type), intent(inout)    :: shape_spec

    call print_action ("Adjusting input parameters")

    ! --- geometry targets ------------------------------------------

    if (is_match_foil_mode(eval_spec%geo_targets)) then 
      call adapt_to_match_foil (eval_spec) 
    end if 

    ! --- Airfoil aero evaluation -----------------------------------------

    call adjust_weightings (eval_spec%geo_targets, &
                            eval_spec%op_point_specs, &
                            eval_spec%goal_attainment)
    
    call adapt_re_type (eval_spec%op_point_specs)

    call check_xtrip (eval_spec%op_point_specs, eval_spec%xfoil_options)

    ! --- geometry constraints --------------------------------------

    call check_flap (shape_spec%flap_spec, eval_spec%op_point_specs)
    
    ! --- Curvature constraints and shape functions --------------------------------------

    call adapt_curv_constraints (shape_spec, eval_spec%curv_constraints)

    call check_curv_reversals (shape_spec, eval_spec%curv_constraints)

    ! --- goal attainment --------------------------------------

    if (eval_spec%goal_attainment%weighting_user_eff > 0d0) then 
      call print_note ("Goal attainment active with user weighting: "// &
                        strf('F5.2', eval_spec%goal_attainment%weighting_user_eff),&
                        indent = 3)
    end if

    ! Xfoil options --------------------------------------------------

    ! Check for a good value of xfoil vaccel to ensure convergence at higher cl

    if (eval_spec%xfoil_options%vaccel > 0.01d0) then
      call print_note ("The xfoil convergence paramter vaccel: "// &
                       strf('F8.4', eval_spec%xfoil_options%vaccel)// &
                      " should be less then 0.01 to avoid convergence problems.")
    end if

  end subroutine 



  subroutine adjust_weightings (geo_targets, op_point_specs, goal_attainment)

    !-----------------------------------------------------------------------------
    !! normalize weighting of op points, geo targets and goal attainment to a sum of 1.0 
    !-----------------------------------------------------------------------------

    type (geo_target_type), allocatable, intent(inout)     :: geo_targets (:) 
    type (op_point_spec_type), allocatable, intent(inout)  :: op_point_specs (:)
    type (goal_attainment_type), intent(inout)             :: goal_attainment

    integer             :: i, noppoint, n_goal_targets
    double precision    :: sum_weightings, attainment_weighting_user_eff
    double precision    :: sum_goal_target_weightings_user

    noppoint = size (op_point_specs)
    goal_attainment%weighting = 0d0
    n_goal_targets = 0
    sum_goal_target_weightings_user = 0d0


    do i= 1, noppoint
      if (is_target(op_point_specs(i)) .and. op_point_specs(i)%weighting_user > 0d0) then
        n_goal_targets = n_goal_targets + 1
        sum_goal_target_weightings_user = sum_goal_target_weightings_user + op_point_specs(i)%weighting_user
      end if
    end do

    do i= 1, size(geo_targets)
      if ((geo_targets(i)%type /= GEO_TARGET_MATCH_FOIL) .and. &
          (geo_targets(i)%weighting_user > 0d0)) then
        n_goal_targets = n_goal_targets + 1
        sum_goal_target_weightings_user = sum_goal_target_weightings_user + geo_targets(i)%weighting_user
      end if
    end do

    ! calculate an effective user weighting for goal attainment, 
    ! to avoid that it dominates the optimization when many goal targets are defined

    if (n_goal_targets > 1) then
      attainment_weighting_user_eff = max(goal_attainment%weighting_user, 0d0) * &
                                      sum_goal_target_weightings_user / 6d0 / 10d0
    else
      attainment_weighting_user_eff = 0d0
    end if

    ! Normalize weightings for operating points and geo targets  

    sum_weightings = sum(op_point_specs%weighting_user) +&
                     sum(geo_targets%weighting_user) + &
                     attainment_weighting_user_eff

    if (sum_weightings > 0d0) then 
      op_point_specs%weighting            = op_point_specs%weighting_user / sum_weightings
      geo_targets%weighting               = geo_targets%weighting_user    / sum_weightings
      goal_attainment%weighting           = attainment_weighting_user_eff / sum_weightings
      goal_attainment%weighting_user_eff  = attainment_weighting_user_eff
    else
      op_point_specs%weighting            = 0d0
      geo_targets%weighting               = 0d0
      goal_attainment%weighting           = 0d0
      goal_attainment%weighting_user_eff  = 0d0
    end if
    
  end subroutine 



  subroutine adapt_re_type (op_point_specs)

    !-----------------------------------------------------------------------------
    !! adapt re for polar type 2
    !-----------------------------------------------------------------------------

    type (op_point_spec_type), allocatable, intent(inout)  :: op_point_specs (:)

    integer             :: i, noppoint

    noppoint = size (op_point_specs)

    ! May the king of xfoil polars be lenient ...
    !        ... when patching to support negative cl for Type 2 based op_points
    do i = 1, noppoint
      if ((op_point_specs(i)%re%type == 2) .and. (op_point_specs(i)%spec_cl) & 
                                          .and. (op_point_specs(i)%value < 0d0)) then
        op_point_specs(i)%re%type    = 1
        op_point_specs(i)%re%number  = op_point_specs(i)%re%number / & 
                                      (abs(op_point_specs(i)%value) ** 0.5d0)
      end if
    end do 

  end subroutine



  subroutine adapt_curv_constraints (shape_spec, curv_constraints)

    !-----------------------------------------------------------------------------
    !! adapt curvature constraints depending on shape type 
    !-----------------------------------------------------------------------------

    use shape_airfoil,        only : shape_spec_type, BEZIER, HICKS_HENNE

    type (shape_spec_type), intent(inout)       :: shape_spec
    type (curv_constraints_type), intent(inout) :: curv_constraints

    ! Shape functions and geomtry / curvature checks

    if (shape_spec%type == HICKS_HENNE ) then

      if (.not. curv_constraints%check_curvature) then 
        call print_warning ("When using shape function 'hicks-henne', curvature ckecking "// &
                            "should be switched on to avoid bumps.", 5)
      end if 

    end if 


  end subroutine



  subroutine check_curv_reversals (shape_spec, curv_constraints)

    !-----------------------------------------------------------------------------
    !! check curvature reversals inputs, identify relexed and rear laoding  
    !-----------------------------------------------------------------------------

    use shape_airfoil,        only : shape_spec_type, BEZIER, HICKS_HENNE

    type (shape_spec_type), intent(inout)        :: shape_spec
    type (curv_constraints_type), intent(inout)  :: curv_constraints
    integer               :: reverse_top, reverse_bot

    if (.not. curv_constraints%check_curvature) return

    reverse_top =  curv_constraints%top%max_curv_reverse
    reverse_bot =  curv_constraints%bot%max_curv_reverse

    if (reverse_top > 1) then 
      call print_warning ("Max "//stri (reverse_top)//" reversals defined for Top side. "//&
                          "Does it make sense?", 5)
    end if 

    if (reverse_bot > 1) then 
      call print_warning ("Max "//stri (reverse_bot)//" reversals defined for Bot side. "//&
                          "Does it make sense?", 5)
    end if 

    ! detect camber type 'reflexed' or 'rear-loading' 

    shape_spec%camber_type = '' 

    if (reverse_top > 0 .and. reverse_bot > 0) then 
      call print_note ("Reversals defined both for Top and Bot side (...?)")
    else
      if (reverse_top == 1) then 

        shape_spec%camber_type = "reflexed"

      else if (reverse_bot == 1) then 

        shape_spec%camber_type = "rear-loading"

      end if 

    end if 


  end subroutine



  subroutine check_xtrip (op_point_specs, xfoil_options)

    !-----------------------------------------------------------------------------
    !! check xfoil xtrip fits to max-xtr
    !-----------------------------------------------------------------------------

    type (op_point_spec_type), allocatable, intent(in)  :: op_point_specs (:)
    type (xfoil_options_type), intent(in)               :: xfoil_options

    integer             :: i, nxtr_opt, noppoint

    nxtr_opt = 0
    noppoint = size (op_point_specs)

    if ( (xfoil_options%xtript < 1.d0) .or. (xfoil_options%xtripb < 1.d0) ) then
      do i = 1, noppoint
        if (op_point_specs(i)%opt_type == OPT_MAX_XTR) nxtr_opt = nxtr_opt + 1
      end do
    
      if (nxtr_opt > 0) then 
        call my_stop ('Using max-xtr optimization but xtript or xtripb is less than 1')
      end if 
    end if
    
  end subroutine 




  subroutine check_flap (flap_spec, op_point_specs) 

    !-----------------------------------------------------------------------------
    !! check flap constraints 
    !-----------------------------------------------------------------------------

    use xfoil_driver,         only : flap_spec_type

    type (flap_spec_type), intent(inout)                  :: flap_spec
    type (op_point_spec_type), allocatable, intent(inout) :: op_point_specs (:)

    integer                     :: i, noppoint, iopt
    character (:), allocatable  :: op 

    if (.not. flap_spec%use_flap) return 

    noppoint = size (op_point_specs)

    ! start flap angle of optimized op points flap
    allocate (flap_spec%start_flap_angle (flap_spec%ndv))
    iopt = 0 

    do i = 1, noppoint

      op = "op point "//stri(i)

      if (op_point_specs(i)%flap_angle < flap_spec%min_flap_angle) then
        call print_warning ("Flap angle of "//op//" less than min_flap_angle of constraints."//&
                            " Adjusting angle ...", 5)
        op_point_specs(i)%flap_angle = flap_spec%min_flap_angle
      end if

      if (op_point_specs(i)%flap_angle > flap_spec%max_flap_angle) then
        call print_warning ("Flap angle of "//op//" greater than max_flap_angle of constraints."//&
                            " Adjusting angle ...", 5)
        op_point_specs(i)%flap_angle = flap_spec%max_flap_angle
      end if 

      ! assign start flap angle of flap optimized op points 

      if (op_point_specs(i)%flap_optimize) then 
        iopt = iopt + 1
        flap_spec%start_flap_angle(iopt) = op_point_specs(i)%flap_angle
      end if 

    end do
        
  end subroutine 




  subroutine adapt_to_match_foil (eval_spec) 

    !-----------------------------------------------------------------------------
    !! adapt all options to match_foil 
    !-----------------------------------------------------------------------------

    type(eval_spec_type), intent(inout)     :: eval_spec

    integer         :: nop

    ! sanity check 

    if (.not. is_match_foil_mode (eval_spec%geo_targets)) return 


    ! remove all op_points 

    nop = size(eval_spec%op_point_specs)
    if (nop > 0) then 
      call print_note ("Adapting options to 'match-foil': Removing operating points")
      deallocate (eval_spec%op_point_specs)
      allocate (eval_spec%op_point_specs(0))
    end if 

    ! switch off constraint checks  

    ! eval_spec%curv_constraints%check_curvature  = .false.
    ! eval_spec%curv_constraints%auto_curvature   = .false.
    ! eval_spec%geo_constraints%check_geometry    = .false.

    ! switch on match-foil mode 

    eval_spec%match_foil_spec%active = .true. 
    eval_spec%match_foil_spec%filename = eval_spec%geo_targets(1)%target_string 

  end subroutine 



  function is_match_foil_mode (geo_targets)

    !! is match-foil defined? 

    type (geo_target_type), allocatable, intent(in)     :: geo_targets (:) 

    integer             :: i
    logical             :: is_match_foil_mode

    is_match_foil_mode = .false. 

    do i = 1, size(geo_targets)
      if (geo_targets(i)%type == GEO_TARGET_MATCH_FOIL) then 
        is_match_foil_mode = .true.
        return 
      end if 
    end do         

  end function

end module input_sanity

! MIT License
! Copyright (C) 2017-2019 Daniel Prosser
! Copyright (c) 2022-2024 Jochen Guenzel


module input_sanity

  use os_util
  use commons
  use print_util

  use eval_commons
  use xfoil_driver,         only : xfoil_options_type
  use xfoil_driver,         only : op_point_spec_type

  use shape_airfoil,        only : shape_spec_type

  use optimization,         only : optimize_spec_type


  implicit none
  private

  public :: check_and_process_inputs


  contains

  subroutine check_and_process_inputs (eval_spec, shape_spec, optimize_options )

    !----------------------------------------------------------------------------
    !! Checks and adapts various inputs to be consistent and valid 
    !----------------------------------------------------------------------------

    use optimization,         only : PSO
    use shape_airfoil,        only : BEZIER, HICKS_HENNE, CAMB_THICK

    type(eval_spec_type), intent(inout)     :: eval_spec
    type(optimize_spec_type), intent(inout) :: optimize_options
    type(shape_spec_type), intent(inout)    :: shape_spec

    call print_action ("Adjusting input parameters")

    ! --- Airfoil aero evaluation -----------------------------------------

    call adjust_weightings (eval_spec%geo_targets, eval_spec%op_points_spec, eval_spec%dynamic_weighting_spec)
    
    call adapt_re_type (eval_spec%op_points_spec)

    call check_xtrip (eval_spec%op_points_spec, eval_spec%xfoil_options)

    ! --- geometry targets ------------------------------------------

    if (is_match_foil_mode(eval_spec%geo_targets)) then 
      call adapt_to_match_foil (eval_spec) 
    end if 

    ! --- geometry constraints --------------------------------------

    call check_flap (shape_spec%flap_spec, eval_spec%op_points_spec)
    
    ! --- Curvature constraints and shape functions --------------------------------------

    call adapt_curv_constraints (shape_spec, eval_spec%curv_constraints)

    call check_curv_reversals (shape_spec, eval_spec%curv_constraints)


    ! --- Optimization options ------------------------------------

    if (shape_spec%type == CAMB_THICK) then
      optimize_options%pso_options%convergence_profile = 'quick_camb_thick'
      optimize_options%pso_options%max_retries         = 0                  ! no retry - no geo checks
      call print_note ("Adapting PSO options for shape function "//&
                          quoted (shape_spec%type_as_text))
    end if


    ! Xfoil options --------------------------------------------------

    ! Check for a good value of xfoil vaccel to ensure convergence at higher cl

    if (eval_spec%xfoil_options%vaccel > 0.01d0) then
      call print_note ("The xfoil convergence paramter vaccel: "// &
                       strf('(F8.4)', eval_spec%xfoil_options%vaccel)// &
                      " should be less then 0.01 to avoid convergence problems.")
    end if

  end subroutine 



  subroutine adjust_weightings (geo_targets, op_points_spec, dynamic_weighting_spec)

    !-----------------------------------------------------------------------------
    !! normalize weighting of op points and geo targets to a sum of 1.0 
    !-----------------------------------------------------------------------------

    type (geo_target_type), allocatable, intent(inout)     :: geo_targets (:) 
    type (op_point_spec_type), allocatable, intent(inout)  :: op_points_spec (:)
    type (dynamic_weighting_spec_type), intent(inout)      :: dynamic_weighting_spec 

    integer             :: i, noppoint, ndyn, nscaled
    double precision    :: sum_weightings

    noppoint = size (op_points_spec)

    ! Set op points to dynamic if weighting is positive

    do i= 1, noppoint
      if (op_points_spec(i)%optimization_type (1:6) == 'target') then
        if (op_points_spec(i)%weighting_user < 0d0) then
          ! switch off dynamic if user defined explizit weighting
          op_points_spec(i)%dynamic_weighting = .false.
          op_points_spec(i)%weighting_user = - op_points_spec(i)%weighting_user
        else
          if (dynamic_weighting_spec%active) &
            op_points_spec(i)%dynamic_weighting = .true.
        end if 
      end if
    end do

    if (.not. any(op_points_spec%dynamic_weighting)) dynamic_weighting_spec%active = .false.

    ! adjust geo target weightings  

    do i = 1, size(geo_targets)
      if (dynamic_weighting_spec%active) then          
      ! Set geo targets to dynamic if weighting is positive
        if (geo_targets(i)%weighting_user < 0d0) then
        ! no dynamic if user defined explizit weighting
          geo_targets(i)%dynamic_weighting = .false.
          geo_targets(i)%weighting_user = - geo_targets(i)%weighting_user
        else
          if (dynamic_weighting_spec%active) &
            geo_targets(i)%dynamic_weighting = .true.
        end if 
      end if 
    end do   
    
    ! Normalize weightings for operating points and geo targets  

    sum_weightings = sum(op_points_spec%weighting_user) + sum(geo_targets%weighting_user)

    if (sum_weightings > 0d0) then 
      op_points_spec%weighting = op_points_spec%weighting_user / sum_weightings
      geo_targets%weighting    = geo_targets%weighting_user    / sum_weightings
    else
      op_points_spec%weighting = 0d0
      geo_targets%weighting    = 0d0
    end if
    
    ! Dynamic Weighting  

    if (dynamic_weighting_spec%active) then 

      ! - We need enough targets to be dynamic    
      ! - Not too much dynamic weightings should be scaled by user     
    
        ndyn = 0
        nscaled = 0 
    
        do i= 1, size(geo_targets)
          if (geo_targets(i)%dynamic_weighting) then
            ndyn = ndyn + 1
            if (geo_targets(i)%weighting_user /= 1d0) nscaled = nscaled + 1
          end if 
        end do
        do i = 1, size(op_points_spec)
          if (op_points_spec(i)%dynamic_weighting) then 
            ndyn = ndyn + 1
            if (op_points_spec(i)%weighting_user /= 1d0) nscaled = nscaled + 1
          end if
        end do
    
        if (ndyn < 3) then

          call print_note ("Dynamic weighting switched off (needs at least 3 op points with targets)")
          dynamic_weighting_spec%active = .false.

        else if ((ndyn - nscaled) < 3) then

          call my_stop("For Dynamic weighting only a few targets should have a scaled weighting <> 1.0."//&
                      " Set weighting to 1.0 (or just remove it)")
        end if 
      end if


  end subroutine 



  subroutine adapt_re_type (op_points_spec)

    !-----------------------------------------------------------------------------
    !! adapt re for polar type 2
    !-----------------------------------------------------------------------------

    type (op_point_spec_type), allocatable, intent(inout)  :: op_points_spec (:)

    integer             :: i, noppoint

    noppoint = size (op_points_spec)

    ! May the king of xfoil polars be lenient ...
    !        ... when patching to support negative cl for Type 2 based op_points
    do i = 1, noppoint
      if ((op_points_spec(i)%re%type == 2) .and. (op_points_spec(i)%spec_cl) & 
                                          .and. (op_points_spec(i)%value < 0d0)) then
        op_points_spec(i)%re%type    = 1
        op_points_spec(i)%re%number  = op_points_spec(i)%re%number / & 
                                      (abs(op_points_spec(i)%value) ** 0.5d0)
      end if
    end do 

  end subroutine



  subroutine adapt_curv_constraints (shape_spec, curv_constraints)

    !-----------------------------------------------------------------------------
    !! adapt curvature constraints depending on shape type 
    !-----------------------------------------------------------------------------

    use shape_airfoil,        only : shape_spec_type, BEZIER, HICKS_HENNE, CAMB_THICK

    type (shape_spec_type), intent(inout)       :: shape_spec
    type (curv_constraints_type), intent(inout) :: curv_constraints

    ! Shape functions and geomtry / curvature checks

    if (shape_spec%type == CAMB_THICK) then

      ! in case of camb_thick checking of curvature makes no sense
      if (curv_constraints%check_curvature) then 
        call print_note ("Curvature ckecking switched off for shape function "//&
                          quoted (shape_spec%type_as_text))
        curv_constraints%check_curvature = .false. 
        curv_constraints%auto_curvature  = .false. 
      end if 
    
    elseif (shape_spec%type == BEZIER ) then

      if (curv_constraints%top%check_curvature_bumps .or. curv_constraints%bot%check_curvature_bumps) then 
        curv_constraints%top%check_curvature_bumps = .false.
        curv_constraints%bot%check_curvature_bumps = .false.
        ! call print_note ("'check_curvature_bumps' switched off for 'bezier' shape type")
      end if

    elseif (shape_spec%type == HICKS_HENNE ) then

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

    use shape_airfoil,        only : shape_spec_type, BEZIER, HICKS_HENNE, CAMB_THICK

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



  subroutine check_xtrip (op_points_spec, xfoil_options)

    !-----------------------------------------------------------------------------
    !! check xfoil xtrip fits to max-xtr
    !-----------------------------------------------------------------------------

    type (op_point_spec_type), allocatable, intent(in)  :: op_points_spec (:)
    type (xfoil_options_type), intent(in)               :: xfoil_options

    integer             :: i, nxtr_opt, noppoint

    nxtr_opt = 0
    noppoint = size (op_points_spec)

    if ( (xfoil_options%xtript < 1.d0) .or. (xfoil_options%xtripb < 1.d0) ) then
      do i = 1, noppoint
        if (op_points_spec(i)%optimization_type == "max-xtr") nxtr_opt = nxtr_opt + 1
      end do
    
      if (nxtr_opt > 0) then 
        call my_stop ('Using max-xtr optimization but xtript or xtripb is less than 1')
      end if 
    end if
    
  end subroutine 




  subroutine check_flap (flap_spec, op_points_spec) 

    !-----------------------------------------------------------------------------
    !! check flap constraints 
    !-----------------------------------------------------------------------------

    use xfoil_driver,         only : flap_spec_type

    type (flap_spec_type), intent(inout)                  :: flap_spec
    type (op_point_spec_type), allocatable, intent(inout) :: op_points_spec (:)

    integer                     :: i, noppoint, iopt
    character (:), allocatable  :: op 

    if (.not. flap_spec%use_flap) return 

    noppoint = size (op_points_spec)

    ! start flap angle of optimized op points flap
    allocate (flap_spec%start_flap_angle (flap_spec%ndv))
    iopt = 0 

    do i = 1, noppoint

      op = "op point "//stri(i)

      if (op_points_spec(i)%flap_angle < flap_spec%min_flap_angle) then
        call print_warning ("Flap angle of "//op//" less than min_flap_angle of constraints."//&
                            " Adjusting angle ...", 5)
        op_points_spec(i)%flap_angle = flap_spec%min_flap_angle
      end if

      if (op_points_spec(i)%flap_angle > flap_spec%max_flap_angle) then
        call print_warning ("Flap angle of "//op//" greater than max_flap_angle of constraints."//&
                            " Adjusting angle ...", 5)
        op_points_spec(i)%flap_angle = flap_spec%max_flap_angle
      end if 

      ! assign start flap angle of flap optimized op points 

      if (op_points_spec(i)%flap_optimize) then 
        iopt = iopt + 1
        flap_spec%start_flap_angle(iopt) = op_points_spec(i)%flap_angle
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

    nop = size(eval_spec%op_points_spec)
    if (nop > 0) then 
      call print_note ("Adapting options to 'match-foil': Removing operating points")
      deallocate (eval_spec%op_points_spec)
      allocate (eval_spec%op_points_spec(0))
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
      if (geo_targets(i)%type == 'match-foil') then 
        is_match_foil_mode = .true.
        return 
      end if 
    end do         

  end function

end module input_sanity

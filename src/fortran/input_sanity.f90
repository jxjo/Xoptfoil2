! MIT License

module input_sanity

  use os_util
  use commons
  use print_util
  use string_util,          only : stri, strf

  use geo_target,           only : geo_target_type
  use eval_commons
  use xfoil_driver,         only : xfoil_options_type
  use op_point,             only : op_point_spec_type, OPT_MAX_XTR
  use shape_airfoil,        only : shape_spec_type

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

    call check_re_type_2 (eval_spec%op_point_specs)

    call check_xtrip (eval_spec%op_point_specs, eval_spec%xfoil_options)

    ! --- geometry constraints --------------------------------------

    call check_flap (shape_spec%flap_spec, eval_spec%op_point_specs)
    call check_flap_hh_curvature (shape_spec, eval_spec%curv_constraints)
    
    ! --- Curvature constraints and shape functions --------------------------------------

    call check_curv_reversals (shape_spec, eval_spec%curv_constraints)

    ! Xfoil options --------------------------------------------------

    ! Check for a good value of xfoil vaccel to ensure convergence at higher cl

    if (eval_spec%xfoil_options%vaccel > 0.01d0) then
      call print_note ("The xfoil convergence paramter vaccel: "// &
                       strf('F8.4', eval_spec%xfoil_options%vaccel)// &
                      " should be less then 0.01 to avoid convergence problems.")
    end if

  end subroutine 


  subroutine check_flap_hh_curvature (shape_spec, curv_constraints)

    !! Disable curvature checks when only flap is optimized with Hicks-Henne

    use shape_airfoil,        only : HICKS_HENNE

    type(shape_spec_type), intent(in)            :: shape_spec
    type(curv_constraints_type), intent(inout)   :: curv_constraints

    logical :: only_flap_optimized

    only_flap_optimized = shape_spec%flap_spec%use_flap .and. &
                          (shape_spec%flap_spec%ndv > 0) .and. &
                          (shape_spec%type == HICKS_HENNE) .and. &
                          (shape_spec%hh%nfunctions_top == 0) .and. &
                          (shape_spec%hh%nfunctions_bot == 0)

    if (.not. only_flap_optimized) return

    if (curv_constraints%check_curvature) then
      call print_note ("Switching off check_curvature for flap-only Hicks-Henne optimization.")
    end if

    curv_constraints%check_curvature = .false.

  end subroutine check_flap_hh_curvature



  subroutine check_re_type_2 (op_point_specs)

    !-----------------------------------------------------------------------------
    !! check that no negative cl is specified for re type 2 polars, 
    !! which are not supported by xfoil and would cause convergence problems
    !-----------------------------------------------------------------------------

    type (op_point_spec_type), allocatable, intent(inout)  :: op_point_specs (:)

    integer             :: i, noppoint

    noppoint = size (op_point_specs)

    do i = 1, noppoint
      if ((op_point_specs(i)%re%type == 2) .and. (op_point_specs(i)%spec_cl) & 
                                          .and. (op_point_specs(i)%value < 0d0)) then
        call my_stop ("Negative cl specified for polar type 2 in op point "//stri(i)//". ")
      end if
    end do 

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


end module input_sanity

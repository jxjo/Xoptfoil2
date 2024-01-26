! MIT License
! Copyright (C) 2017-2019 Daniel Prosser
! Copyright (c) 2022-2024 Jochen Guenzel


module input_sanity

  use os_util

  implicit none
  private

  public :: check_and_process_inputs
  public :: eval_seed


  contains

  subroutine check_and_process_inputs (optimize_options, shaping)

    !----------------------------------------------------------------------------
    !! Checks various inputs to be consistent and valid 
    !----------------------------------------------------------------------------

    use commons

    use optimization_driver,  only : optimize_spec_type, PSO, GENETIC
    use shape_airfoil,        only : shape_spec_type, BEZIER, HICKS_HENNE, CAMB_THICK

    use eval_commons,         only : curv_constraints, match_foils 
    use eval_commons,         only : xfoil_options, xfoil_geom_options
    use eval_commons,         only : noppoint, op_points_spec
    use eval_commons,         only : dynamic_weighting_spec
    use eval_commons,         only : geo_targets, geo_constraints


    use xfoil_driver,         only : op_point_specification_type


    type(optimize_spec_type), intent(inout) :: optimize_options
    type(shape_spec_type), intent(inout)    :: shaping

    integer             :: i, nxtr_opt, ndyn, nscaled
    double precision    :: sum_weightings
    type(op_point_specification_type) :: op_spec
    character(:), allocatable         :: opt_type


    ! -- Airfoil evaluation -----------------------------------------

    ! adjust geo target weightings  

    do i = 1, size(geo_targets)
      if (dynamic_weighting_spec%active) then           ! #todo op_specs must be before geo in input file
      ! Set geo targets to dynamic if weighting is positive
        if (geo_targets(i)%weighting_user < 0d0) then
        ! no dynamic if user defined explizit weighting
          geo_targets(i)%weighting_user = - geo_targets(i)%weighting_user
        else
          geo_targets(i)%dynamic_weighting = .true.
        end if 
      end if 
    end do   
    
    ! Normalize weightings for operating points and geo targets  

    if (noppoint > 0) then

        sum_weightings = sum(op_points_spec%weighting_user)               &
                      + sum(geo_targets%weighting_user)

      if (sum_weightings > 0d0) then 
        op_points_spec%weighting = op_points_spec%weighting_user / sum_weightings
        geo_targets%weighting    = geo_targets%weighting         / sum_weightings
      else
        op_points_spec%weighting = 0d0
        geo_targets%weighting    = 0d0
      end if
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
    
        if (ndyn < 3) &
          call my_stop("Dynamic weighting needs at least 3 op points with a target based"//  &
                      " optimization_type")
        if ((ndyn - nscaled) < 3) &
          call my_stop("For Dynamic weighting only a few targets should have a scaled weighting <> 1.0."//&
                      " Set weighting to 1.0 (or just remove it)")
        call print_note ("Dynamic weighting starting with design #"// & 
                          stri(dynamic_weighting_spec%start_with_design) //" repeating every " //&
                          stri(dynamic_weighting_spec%frequency) //" designs.")
      end if
    
    ! Check for bad combinations of operating conditions and optimization types

    do i = 1, noppoint

      op_spec  = op_points_spec(i) 
      opt_type = op_spec%optimization_type

      if ((op_spec%value <= 0.d0) .and. (op_spec%spec_cl)) then
        if ( (opt_type /= 'min-drag') .and.                                &
            (opt_type /= 'max-xtr') .and.                                 &
              ! jx-mod - allow target and min-lift-slope, min-glide-slope
            (opt_type /= 'target-drag') .and.                             &
            (opt_type /= 'min-lift-slope') .and.                          &
            (opt_type /= 'min-glide-slope') .and.                         &
            (opt_type /= 'max-lift-slope') ) then
          call my_stop ("Operating point "//stri(i)//" is at Cl = 0. "//  &
                        "Cannot use '"//opt_type//"' optimization in this case.")
        end if

      elseif (op_spec%spec_cl .and. (opt_type == 'max-lift')) then
        call my_stop ("Cl is specified for operating point "//stri(i)//   &
                      ". Cannot use 'max-lift' optimization type in this case.")

      elseif (op_spec%spec_cl .and. (opt_type == 'target-lift')) then              
        call my_stop ("op_mode = 'spec_cl' doesn't make sense "//                &
                    "for optimization_type 'target-lift'")
      end if

    end do

    ! Ask about removing turbulent trips for max-xtr optimization

    nxtr_opt = 0
    if ( (xfoil_options%xtript < 1.d0) .or. (xfoil_options%xtripb < 1.d0) ) then
      do i = 1, noppoint
        if (op_points_spec(i)%optimization_type == "max-xtr") nxtr_opt = nxtr_opt + 1
      end do
    
      if (nxtr_opt > 0) then 
        call my_stop ('Using max-xtr optimization but xtript or xtripb is less than 1')
      end if 
    end if
    
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

    
    ! -- Shape functions -----------------------------------------


    ! Shape functions and geomtry / curvature checks

    if (shaping%type == CAMB_THICK) then

      ! in case of camb_thick checking of curvature makes no sense
      if (curv_constraints%check_curvature) then 
        call print_note ("Because of shape function 'camb-thick' curvature ckecking "// &
                        "will be switched off during optimization")
        curv_constraints%check_curvature = .false. 
        curv_constraints%auto_curvature  = .false. 
      end if 
      if ((.not. curv_constraints%do_smoothing) .and. (.not. match_foils)) then 
        call print_note ("Smoothing switched on for shape function 'camb-thick' "// &
                        "to ensure good results.")
        curv_constraints%do_smoothing = .true. 
      end if 
    
    elseif (shaping%type == BEZIER ) then

      if (curv_constraints%do_smoothing) then 
        curv_constraints%do_smoothing = .false. 
        call print_note ("Smoothing switched off for 'bezier' shape type")
      end if 
      curv_constraints%same_le_curvature = .true. 
      call print_note ("Added geo target 'same_le_curvature' for 'bezier' shape type")



    elseif (shaping%type == HICKS_HENNE ) then

      if (.not. curv_constraints%check_curvature .and. (.not. match_foils)) then 
        call print_warning ("When using shape function 'hicks-henne', curvature ckecking "// &
                        "should be switched on to avoid bumps.")
      end if 
    end if 


    ! PSO auto_retry  --------------------------------------------------

    if ((shaping%type == CAMB_THICK)  .and. (optimize_options%type == PSO)) then 
      if (optimize_options%pso_options%max_retries >= 0) then 
        call print_note ('Particle retry switched off (only for Hicks-Henne or Bezier shape_type)')
        optimize_options%pso_options%max_retries = 0
        optimize_options%pso_options%auto_retry = .false.
      end if 
    end if 


    ! Match foil  --------------------------------------------------

    ! Switch off geometric checks 
    if (match_foils) then 
      geo_constraints%check_geometry = .false.
      ! curv_constraints%check_curvature = .true. 
      ! curv_constraints%auto_curvature  = .true. 
      curv_constraints%do_smoothing = .false. 
      call print_note ("Smoothing and geometry checks switched off for match foil mode.")
    endif 


    ! Xfoil options --------------------------------------------------

    ! Repanel option depending on shape type 

    if (shaping%type == CAMB_THICK) then
      ! re-paneling is not needed and not good for high cl
      xfoil_geom_options%repanel = .false. 

    elseif (shaping%type == BEZIER) then
      ! paneling master is bezier curve
      xfoil_geom_options%repanel = .false. 
        
    end if 

    ! Check for a good value of xfoil vaccel to ensure convergence at higher cl

    if (xfoil_options%vaccel > 0.01d0) then
      call print_note ("The xfoil convergence paramter vaccel: "//strf('(F8.4)', xfoil_options%vaccel)// &
                      " should be less then 0.01 to avoid convergence problems.")
    end if


  end subroutine 


  subroutine eval_seed (seed_foil)

    !-----------------------------------------------------------------------------
    !! Checks that the seed airfoil passes all constraints, sets scale factors for
    !! objective functions at each operating point.
    !-----------------------------------------------------------------------------

    use commons

    use eval_commons 
    use eval,                 only : geo_objective_results
    use airfoil_constraints,  only : eval_geometry_violations, assess_surface
    use math_deps,            only : interp_point, derivation_at_point, smooth_it, norm_2
    use xfoil_driver,         only : run_op_points, op_point_result_type, xfoil_defaults

    type (airfoil_type), intent(in)   :: seed_foil 

    type(op_point_specification_type) :: op_spec
    type(op_point_result_type)        :: op
    type(op_point_result_type), dimension(:), allocatable :: op_points_result
    type(xfoil_options_type)          :: local_xfoil_options

    type(geo_result_type)             :: geo_result

    double precision :: correction
    double precision :: slope
    double precision :: checkval
    double precision :: pi
    integer :: i
    character(100) :: text
    character(15) :: opt_type
    ! logical :: addthick_violation
    double precision :: ref_value, seed_value, tar_value, cur_value
    double precision :: dist = 0d0

    ! Analyze airfoil at requested operating conditions with Xfoil

    write (*,'(" - ",A)') 'Analyze seed airfoil at requested operating points'

    ! Re-Init boundary layer at each op point to ensure convergence (slower)
    local_xfoil_options = xfoil_options
    local_xfoil_options%reinitialize = .false.    ! strange: reinit leeds sometimes to not converged
    local_xfoil_options%show_details = .false.

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

      write(text,*) i
      text = adjustl(text)

      op_spec  = op_points_spec(i)
      op       = op_points_result(i) 
      opt_type = op_spec%optimization_type

      ! Check for unconverged points

      if (.not. op%converged) then
        call my_stop("Xfoil calculations did not converge for operating point: "//stri(i))
      end if

      if (op%cl <= 0.d0 .and. (opt_type == 'min-sink' .or.   &
          opt_type == 'max-glide') ) then
        call my_stop( "Operating point "//trim(text)//" has Cl <= 0. "//     &
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
        checkval   = 1.d0 + ABS (op_spec%target_value - op%cl) * correction

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
        call my_stop ("Requested optimization_type for operating point "//   &
                      trim(text)//" not recognized.")
      end if

      op_spec%scale_factor = 1.d0/checkval

      op_points_spec(i) = op_spec             ! write back target, scale, ...

    end do

  end subroutine eval_seed



end module input_sanity

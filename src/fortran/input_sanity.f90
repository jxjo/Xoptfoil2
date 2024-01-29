! MIT License
! Copyright (C) 2017-2019 Daniel Prosser
! Copyright (c) 2022-2024 Jochen Guenzel


module input_sanity

  use os_util

  implicit none
  private

  public :: check_and_process_inputs


  contains

  subroutine check_and_process_inputs (eval_spec, shape_spec, optimize_options )

    !----------------------------------------------------------------------------
    !! Checks various inputs to be consistent and valid 
    !----------------------------------------------------------------------------

    use commons

    use optimization_driver,  only : optimize_spec_type, PSO, GENETIC
    use shape_airfoil,        only : shape_spec_type, BEZIER, HICKS_HENNE, CAMB_THICK
    use eval_commons                                          ! all types needed here 
    use xfoil_driver,         only : op_point_spec_type
    use xfoil_driver,         only : xfoil_options_type
    use xfoil_driver,         only : xfoil_geom_options_type


    type(eval_spec_type), intent(inout)     :: eval_spec
    type(optimize_spec_type), intent(inout) :: optimize_options
    type(shape_spec_type), intent(inout)    :: shape_spec

    integer             :: i, nxtr_opt, ndyn, nscaled
    double precision    :: sum_weightings
    type (op_point_spec_type)               :: op_spec
    type (geo_target_type), allocatable     :: geo_targets (:) 
    type (geo_constraints_type)             :: geo_constraints 
    type (curv_constraints_type)            :: curv_constraints 
    type (op_point_spec_type), allocatable  :: op_points_spec (:)
    type (dynamic_weighting_spec_type)      :: dynamic_weighting_spec 
    type (xfoil_options_type)               :: xfoil_options
    type (xfoil_geom_options_type)          :: xfoil_geom_options
  
  
    character(:), allocatable   :: opt_type
    integer                     :: noppoint

    op_points_spec    = eval_spec%op_points_spec
    geo_targets       = eval_spec%geo_targets
    geo_constraints   = eval_spec%geo_constraints
    curv_constraints  = eval_spec%curv_constraints
    dynamic_weighting_spec = eval_spec%dynamic_weighting_spec

    noppoint = size (op_points_spec)

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

    if (shape_spec%type == CAMB_THICK) then

      ! in case of camb_thick checking of curvature makes no sense
      if (curv_constraints%check_curvature) then 
        call print_note ("Because of shape function 'camb-thick' curvature ckecking "// &
                        "will be switched off during optimization")
        curv_constraints%check_curvature = .false. 
        curv_constraints%auto_curvature  = .false. 
      end if 
      if ((.not. curv_constraints%do_smoothing) .and. (.not. eval_spec%match_foils)) then 
        call print_note ("Smoothing switched on for shape function 'camb-thick' "// &
                        "to ensure good results.")
        curv_constraints%do_smoothing = .true. 
      end if 
    
    elseif (shape_spec%type == BEZIER ) then

      if (curv_constraints%do_smoothing) then 
        curv_constraints%do_smoothing = .false. 
        call print_note ("Smoothing switched off for 'bezier' shape type")
      end if 
      curv_constraints%same_le_curvature = .true. 
      call print_note ("Added geo target 'same_le_curvature' for 'bezier' shape type")



    elseif (shape_spec%type == HICKS_HENNE ) then

      if (.not. curv_constraints%check_curvature .and. (.not. eval_spec%match_foils)) then 
        call print_warning ("When using shape function 'hicks-henne', curvature ckecking "// &
                        "should be switched on to avoid bumps.")
      end if 
    end if 


    ! PSO auto_retry  --------------------------------------------------

    if ((shape_spec%type == CAMB_THICK)  .and. (optimize_options%type == PSO)) then 
      if (optimize_options%pso_options%max_retries >= 0) then 
        call print_note ('Particle retry switched off (only for Hicks-Henne or Bezier shape_type)')
        optimize_options%pso_options%max_retries = 0
        optimize_options%pso_options%auto_retry = .false.
      end if 
    end if 


    ! Match foil  --------------------------------------------------

    ! Switch off geometric checks 
    if (eval_spec%match_foils) then 
      geo_constraints%check_geometry = .false.
      ! curv_constraints%check_curvature = .true. 
      ! curv_constraints%auto_curvature  = .true. 
      curv_constraints%do_smoothing = .false. 
      call print_note ("Smoothing and geometry checks switched off for match foil mode.")
    endif 


    ! Xfoil options --------------------------------------------------

    ! Repanel option depending on shape type 

    if (shape_spec%type == CAMB_THICK) then
      ! re-paneling is not needed and not good for high cl
      xfoil_geom_options%repanel = .false. 

    elseif (shape_spec%type == BEZIER) then
      ! paneling master is bezier curve
      xfoil_geom_options%repanel = .false. 
        
    end if 

    ! Check for a good value of xfoil vaccel to ensure convergence at higher cl

    if (xfoil_options%vaccel > 0.01d0) then
      call print_note ("The xfoil convergence paramter vaccel: "//strf('(F8.4)', xfoil_options%vaccel)// &
                      " should be less then 0.01 to avoid convergence problems.")
    end if


  end subroutine 


end module input_sanity

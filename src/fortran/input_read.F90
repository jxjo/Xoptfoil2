! MIT License
! Copyright (C) 2017-2019 Daniel Prosser
! Copyright (c) 2022-2024 Jochen Guenzel

module input_read

  !------------------------------------------------------------------------------------
  ! Read command line and input file, build main parameter data structures  ... 
  !------------------------------------------------------------------------------------

  use os_util

  implicit none
  private

  public :: read_inputs

  public :: namelist_check
  public :: open_input_file, close_input_file

  public :: read_xfoil_options_inputs, read_op_points_spec, read_xfoil_paneling_inputs
  public :: read_bezier_inputs, read_flap_worker_inputs, read_curvature_inputs


  contains


  subroutine read_inputs (input_file_in, airfoil_filename, seed_airfoil_type, &
                          eval_spec, shape_spec, optimize_options)

    !------------------------------------------------------------------------------------
    !! read command line and the input file  
    !!  input_file_in : defaults to '' - needed for Worker 'check' 
    !------------------------------------------------------------------------------------

    use commons

    use eval_commons,         only : eval_spec_type
    use shape_airfoil,        only : shape_spec_type
    use optimization,  only : optimize_spec_type


    character(*), intent(in)                :: input_file_in
    character(:), allocatable, intent(out)  :: airfoil_filename, seed_airfoil_type

    type(eval_spec_type), intent(out)       :: eval_spec
    type(shape_spec_type), intent(inout)    :: shape_spec
    type(optimize_spec_type), intent(out)   :: optimize_options

    character(:), allocatable   :: input_file
    double precision            :: re_default_cl
    integer                     :: iunit

    ! Set default names, read command line arguments
  
    output_prefix = 'optfoil'                     ! default name of result airfoil 
    airfoil_filename = ''                         ! either from command line or input file 
    re_default_cl = 0d0                           ! either from command line or input file 
  
    if (input_file_in == '') then 
      input_file = 'inputs.inp'                    ! defualt name of input file 
      call read_clo (input_file, output_prefix, airfoil_filename, re_default_cl, 'Xoptfoil2')
    else
      input_file = input_file_in                   ! for Worker check  
    end if 
      
    ! open input file to read all the single namelists

    call open_input_file (input_file, iunit)

    ! main namelist 

    call read_optimization_options_inputs (iunit, seed_airfoil_type, airfoil_filename, &
                                          shape_spec, optimize_options)
  
    ! shape functions
      
    call read_bezier_inputs (iunit, shape_spec%bezier)
    call read_hh_inputs     (iunit, shape_spec%hh)


    ! option to match seed airfoil to another instead of aerodynamic optimization

    call read_match_foils_inputs (iunit, eval_spec) 


    ! operating conditions

    if (.not. eval_spec%match_foils) then

      call read_op_points_spec (iunit, re_default_cl, eval_spec) 

    end if


    ! geo targets - start read and weight options

    call read_geometry_targets_inputs  (iunit, eval_spec%geo_targets)


    ! geometry and curvature constraints

    call read_curvature_inputs   (iunit, eval_spec%curv_constraints)
    call read_constraints_inputs (iunit, eval_spec%geo_constraints, eval_spec%flap_spec)


    ! optimizer options 

    call read_pso_options_inputs      (iunit, shape_spec%type, initial_perturb, &
                                          optimize_options%pso_options)
    call read_genetic_options_inputs  (iunit, optimize_options%ga_options)
    call read_simplex_options_inputs  (iunit, optimize_options%sx_options)


    ! xfoil options 

    call read_xfoil_options_inputs  (iunit, eval_spec%xfoil_options)
    call read_xfoil_paneling_inputs (iunit, eval_spec%xfoil_geom_options)


    call close_input_file (iunit)


    if (.not. eval_spec%match_foils .and. show_details) then 
      call echo_op_points_spec  (eval_spec%op_points_spec, eval_spec%xfoil_options) 
    end  if


    
  end subroutine read_inputs


  ! ------------------------------------------------------------------------------


  subroutine read_optimization_options_inputs  (iunit, seed_airfoil_type, airfoil_filename, &
                                                shape_spec, optimize_options)

    !! Read main namelist 'optimization_options'

    use commons,              only : show_details, initial_perturb

    use shape_airfoil,        only : shape_spec_type, HICKS_HENNE, BEZIER, CAMB_THICK
    use optimization,  only : optimize_spec_type, PSO, GENETIC

    integer, intent(in)                     :: iunit
    character (:), allocatable, intent(out) :: seed_airfoil_type
    character (:), allocatable, intent(inout) :: airfoil_filename
    type(optimize_spec_type), intent(out)   :: optimize_options
    type(shape_spec_type), intent(out)      :: shape_spec

    character (250)        :: global_search, seed_airfoil, airfoil_file, shape_functions

    integer :: iostat1

    namelist /optimization_options/ global_search,                               &
              seed_airfoil, airfoil_file, shape_functions, show_details

    ! defaults for main namelist options

    global_search = 'particle_swarm'
    seed_airfoil = 'from_file'
    airfoil_file = ''
    shape_functions = 'hicks-henne'
    initial_perturb = 0.003d0
    show_details = .false.                              ! Show more infos  / supress echo

    ! Open input file and read namelist from file

    if (iunit > 0) then
      rewind (iunit)
      read (iunit, iostat=iostat1, nml=optimization_options)
      call namelist_check('optimization_options', iostat1, 'warn')
    end if

    seed_airfoil_type = trim(seed_airfoil)          ! keep var name compatible
    if (airfoil_filename == '') then                ! no airfoil_filename from command line ...
      airfoil_filename = airfoil_file               ! take from input file
    end if 

    ! search 

    if (trim(global_search) == 'particle_swarm') then 
      optimize_options%type = PSO
    else if (trim(global_search) == 'genetic_algorithm') then 
      optimize_options%type = GENETIC
    else
      call my_stop("Search type '"//trim(global_search)//"' not known'")
    end if 

    ! shape functions options 

    if      (trim(shape_functions) == 'bezier') then
      shape_spec%type = BEZIER
    else if (trim(shape_functions) == 'hicks_henne') then
      shape_spec%type = HICKS_HENNE
    else if (trim(shape_functions) == 'camb_thick') then
      shape_spec%type = CAMB_THICK
    else 
      call my_stop("Shape_functions '"//trim(shape_functions)//"' not known'")
    end if 

    ! Optimization settings

    if (trim(seed_airfoil) /= 'from_file' .and.                                  &
        trim(seed_airfoil) /= 'from_bezier' )                                    &
      call my_stop("seed_airfoil must be 'from_file' or 'from_bezier'.")
    if (initial_perturb <= 0.d0)                                                 &
      call my_stop("initial_perturb must be > 0.")

  end subroutine 




  subroutine read_op_points_spec  (iunit, re_default_cl, eval_spec)

    !! Read operating points specification from input file 

    use commons,              only : MAX_NOP, NOT_DEF_D

    use xfoil_driver,         only : op_point_spec_type, re_type
    use xfoil_driver,         only : flap_spec_type

    use eval_commons,         only : eval_spec_type
    use eval_commons,         only : dynamic_weighting_spec_type

    integer, intent(in)                  :: iunit
    double precision, intent(in)         :: re_default_cl            ! re default from command line 
    type(eval_spec_type), intent(out)    :: eval_spec


    integer                               :: noppoint
    type(op_point_spec_type), allocatable :: op_points_spec (:)
    type(dynamic_weighting_spec_type)     :: dynamic_weighting_spec
    type(flap_spec_type)                  :: flap_spec


    ! Op_point specification 
    character(7),     dimension(MAX_NOP)  :: op_mode
    character(15),    dimension(MAX_NOP)  :: optimization_type, flap_selection
    double precision, dimension(MAX_NOP)  :: op_point, weighting
    double precision, dimension(MAX_NOP)  :: ncrit_pt, target_value, reynolds, mach
    double precision, dimension(MAX_NOP)  :: flap_degrees

    double precision      :: re_default
    logical               :: re_default_as_resqrtcl, dynamic_weighting
    logical               :: allow_improved_target
    type(op_point_spec_type) :: op

    integer               :: i, iostat1, nflap_opt
    double precision      :: x_flap, y_flap
    character(3)          :: y_flap_spec
    logical               :: use_flap

    type (re_type)        :: re_def

    namelist /operating_conditions/ noppoint, op_mode, op_point, reynolds, mach,   &
              target_value, weighting, optimization_type, ncrit_pt,                & 
              re_default_as_resqrtcl, re_default, dynamic_weighting, dynamic_weighting_spec, &
              use_flap, x_flap, y_flap, y_flap_spec, flap_degrees, flap_selection, &
              allow_improved_target

    ! Set defaults for operating conditions and constraints

    noppoint  = 0
    nflap_opt = 0 

    re_default = 100000d0
    re_default_as_resqrtcl = .false.
    reynolds(:) = -1.d0                         ! value in input file

    op_mode(:) = 'spec-cl'
    op_point(:) = 0.d0
    optimization_type(:) = 'min-drag'
    mach(:) = 0.d0
    weighting(:) = 1.d0
    ncrit_pt(:) = -1.d0
    target_value(:) = 0 

    use_flap     = .false.                
    x_flap       = 0.75d0
    y_flap       = 0.d0
    y_flap_spec  = 'y/c'
    flap_degrees      = 0d0
    flap_selection    = 'specify'

    allow_improved_target = .false.

    ! Default values controlling dynamic weighting 
    dynamic_weighting = .false. 
    dynamic_weighting_spec%min_weighting = 0.6d0 
    dynamic_weighting_spec%max_weighting = 1.4d0 
    dynamic_weighting_spec%extra_punch   = 1.2d0 
    dynamic_weighting_spec%start_with_design = 10
    dynamic_weighting_spec%frequency = 20


    ! (Open input file) , read options

    if (iunit > 0) then
      rewind (iunit)
      read(iunit, iostat=iostat1, nml=operating_conditions)
      call namelist_check('operating_conditions', iostat1, 'warn')
    end if

    ! overwrite re_default if specified in command line 

    if (re_default_cl > 0d0) then                       
      re_def%number = re_default_cl 
    else 
      re_def%number = re_default
    end if 
    if (re_default_as_resqrtcl) then
      re_def%type = 2
    else
      re_def%type = 1
    end if

    ! store op_point specification in data structure 

    allocate (op_points_spec(noppoint)) 

    do i = 1, noppoint

      op_points_spec(i)%spec_cl = (op_mode(i) == 'spec-cl')
      op_points_spec(i)%value   = op_point(i)
      
      op_points_spec(i)%ncrit = ncrit_pt(i)    
      op_points_spec(i)%optimization_type = trim(optimization_type (i))
      op_points_spec(i)%target_value = target_value (i)
      op_points_spec(i)%allow_improved_target = allow_improved_target
      
      if (reynolds(i) /= -1.d0) then
        op_points_spec(i)%re%number  = reynolds(i)
        op_points_spec(i)%re%type    = 1
      else                                    ! take default Re number
        op_points_spec(i)%re = re_def 
      end if
      op_points_spec(i)%ma%number  = mach(i)                ! mach number only Type 1
      op_points_spec(i)%ma%type    = 1

      op_points_spec(i)%weighting_user  = weighting (i)
      op_points_spec(i)%scale_factor    = 1d0

      op_points_spec(i)%weighting_user_cur = 0d0
      op_points_spec(i)%weighting_user_prv = 0d0

      ! map flap inputs to op point spec 
      if (use_flap) then 
        if (flap_selection(i) == "optimize") then 
          op_points_spec(i)%flap_angle = NOT_DEF_D          ! indicate: will be otomized
          nflap_opt = nflap_opt + 1
        else
          op_points_spec(i)%flap_angle = flap_degrees(i)    ! set to fix value 
        end if 
      else 
        op_points_spec(i)%flap_angle = 0d0                  ! 0 equals no flap setting 
      end if 
    end do 

    ! Dynamic weighting - if activated all op_points with 'target' will be dynamic 

    dynamic_weighting_spec%active = dynamic_weighting
    op_points_spec%extra_punch       = .false. 
    op_points_spec%dynamic_weighting = .false.      

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

    ! Flap settings to data structure

    flap_spec%use_flap    = use_flap
    flap_spec%ndv         = nflap_opt                         ! no of design variables in opti 
    flap_spec%x_flap      = x_flap
    flap_spec%y_flap      = y_flap
    flap_spec%y_flap_spec = y_flap_spec

    ! put sub types into main data structure 

    eval_spec%op_points_spec = op_points_spec
    eval_spec%dynamic_weighting_spec = dynamic_weighting_spec
    eval_spec%flap_spec = flap_spec


    ! Check input data  ------------------------

    if (noppoint < 1) call my_stop("noppoint must be > 0")
    if (noppoint > MAX_NOP) then
      call my_stop("noppoints must be <= "//stri(MAX_NOP)//".")
    end if

    if ((use_flap) .and. (x_flap <= 0.0)) call my_stop("x_flap must be > 0.")
    if ((use_flap) .and. (x_flap >= 1.0)) call my_stop("x_flap must be < 1.")
    if ((use_flap) .and. (y_flap_spec /= 'y/c') .and. (y_flap_spec /= 'y/t'))    &
      call my_stop("y_flap_spec must be 'y/c' or 'y/t'.")

    if ((y_flap_spec  /= 'y/c') .and. (y_flap_spec  /= 'y/t')) &
      call my_stop ("Vertical hinge definition must be 'y/c' or 'y/t'")

    do i = 1, noppoint

      op  = op_points_spec(i) 

      if (op%re%number <= 0.d0) &
        call my_op_stop (i,op_points_spec, "reynolds must be > 0. Default value (re_default) could not be set")
      if (op%ma%number < 0.d0) &
        call my_op_stop (i,op_points_spec, "mach must be >= 0.")
      if (op%weighting_user <= 0.d0) &
        call my_op_stop (i,op_points_spec, "weighting must be > 0.")
      if (op%optimization_type /= 'min-drag' .and.                         &
          op%optimization_type /= 'max-glide' .and.                          &
          op%optimization_type /= 'min-sink' .and.                           &
          op%optimization_type /= 'max-lift' .and.                           &
          op%optimization_type /= 'target-moment' .and.                      &
          op%optimization_type /= 'target-drag' .and.                        &
          op%optimization_type /= 'target-lift' .and.                        &
          op%optimization_type /= 'target-glide' .and.                        &
          op%optimization_type /= 'max-xtr' .and.                            &
          op%optimization_type /= 'min-lift-slope' .and.                     &
          op%optimization_type /= 'min-glide-slope' .and.                    &
          op%optimization_type /= 'max-lift-slope')                          &
        call my_op_stop (i,op_points_spec, "optimization_type must be 'min-drag', 'max-glide', "//     &
                    "'min-sink', 'max-lift', 'max-xtr', 'target-moment', "//    &
                    "'target-drag', 'target-glide', 'min-lift-slope', 'min-glide-slope'"//      &
                    " or 'max-lift-slope'.")
      if ((op%optimization_type == 'max-lift-slope') .and. (noppoint == 1))&
        call my_op_stop (i,op_points_spec, "at least two operating points are required for to "//      &
                    "maximize lift curve slope.")
      if ((op%optimization_type == 'min-lift-slope') .and. (noppoint == 1))&
        call my_op_stop (i,op_points_spec, "at least two, better three operating points are required"//&
                    " for to minimize lift curve slope.")
      if ((op%optimization_type == 'min-glide-slope') .and. (noppoint == 1))&
        call my_op_stop (i,op_points_spec, "at least two, better three operating points are required"//&
                    " for to minimize lift curve slope.")
      if ((op%ncrit <= 0.d0) .and. (op%ncrit /= -1d0)) &
        call my_op_stop (i,op_points_spec, "ncrit_pt must be > 0 or -1.")

      if (((op%optimization_type == 'target-moment') .and.                &
          (target_value(i)) == -1.d3) )                                         &
        call my_op_stop (i,op_points_spec, "No 'target-value' defined for "//  &
                  "for optimization_type 'target-moment'")
      if (((op%optimization_type == 'target-drag') .and.                  &
          (target_value(i)) == -1.d3) )                                         &
        call my_op_stop (i,op_points_spec, "No 'target-value' defined for "//  &
                      "for optimization_type 'target-drag'")
      if (((op%optimization_type == 'target-glide') .and.                 &
          (target_value(i)) == -1.d3) )                                         &
        call my_op_stop (i,op_points_spec, "No 'target-value' defined for "//  &
                      "for optimization_type 'target-glide'")
      if (((op%optimization_type == 'target-lift') .and.                  &
          (target_value(i)) == -1.d3) )                                         &
        call my_op_stop (i,op_points_spec, "No 'target-value' defined for "//  &
                      "for optimization_type 'target-lift'")

      if (abs(flap_degrees(i)) > 70d0) &
        call my_stop ('Flap angle must be less than 70 degrees')
      if ((flap_selection(i) /= 'specify') .and. (flap_selection(i) /= 'optimize')) then
        call my_stop ("Flap selection must be 'spcify' or 'optimize')")
      end if 

    end do

  end subroutine read_op_points_spec



  subroutine read_geometry_targets_inputs  (iunit, geo_targets)

    !! Read 'constraints' inputs into derived types

    use commons,                only : MAX_NOP, NOT_DEF_D, NOT_DEF_I
    use eval_commons,           only : geo_target_type

    integer, intent(in)                :: iunit
    type (geo_target_type), allocatable, intent(inout) :: geo_targets (:)

    integer :: iostat1

    double precision, dimension(MAX_NOP) :: target_geo
    double precision, dimension(MAX_NOP) :: weighting_geo
    character(30), dimension(MAX_NOP)    :: target_type
    integer :: ngeo_targets, i

    namelist /geometry_targets/ ngeo_targets, target_type, target_geo, weighting_geo 

    ! Default values for curvature parameters

    ngeo_targets = 0
    target_type (:) = ''
    target_geo(:) = 0.d0 
    weighting_geo(:) = 1d0 

    ! Open input file and read namelist from file

    if (iunit > 0) then
      rewind (iunit)
      read (iunit, iostat=iostat1, nml=geometry_targets)
      call namelist_check('geometry_targets', iostat1, 'no-warn')
    end if

    allocate (geo_targets(ngeo_targets)) 
    geo_targets%reference_value   = 0.d0
    geo_targets%dynamic_weighting = .false.

    do i = 1, ngeo_targets
      geo_targets(i)%type           = trim(target_type(i))
      geo_targets(i)%target_value   = target_geo(i)
      geo_targets(i)%weighting      = weighting_geo(i)  ! will be normalized        
      geo_targets(i)%weighting_user = weighting_geo(i)
    end do   

    ! Geo targets - check options

    do i = 1, ngeo_targets
      if ((geo_targets(i)%type /= 'Camber') .and.                          &
          (geo_targets(i)%type /= 'Thickness') .and.                       &
          (geo_targets(i)%type /= 'bezier-le-curvature'))                  &
        call my_stop("Target_type must be 'Camber','Thickness' or 'bezier-le-curvature'.")
    end do   

  end subroutine 



  subroutine read_hh_inputs  (iunit, hh)

    !! read input file for hicks henne shape options 

    use shape_airfoil,        only : shape_hh_type
    use shape_hicks_henne,    only : nfunctions_to_ndv

    integer, intent(in)                   :: iunit
    type(shape_hh_type), intent(out)      :: hh

    double precision    :: min_bump_width                 
    integer             :: nfunctions_top, nfunctions_bot
    integer             :: iostat1

    ! #todo hicks henne options are still in namelist optimization_options
    namelist /optimization_options/ nfunctions_top, nfunctions_bot, min_bump_width

    ! Init default values 

    nfunctions_top = 4
    nfunctions_bot = 4
    min_bump_width = 0.1d0                    ! #todo - has to be implemented

    if (iunit > 0) then
      rewind (iunit)
      read (iunit, iostat=iostat1, nml=optimization_options)
      ! call namelist_check('optimization_options', iostat1, 'no-warn')
    end if

    ! Put options into derived types

    hh%nfunctions_top = nfunctions_top
    hh%nfunctions_bot = nfunctions_bot
    hh%ndv = nfunctions_to_ndv (nfunctions_top, nfunctions_bot)

    if (nfunctions_top < 0) &
      call my_stop("nfunctions_top must be >= 0.")
    if (nfunctions_bot < 0) &
      call my_stop("nfunctions_bot must be >= 0.")
    
    if (min_bump_width <= 0.01d0 .or. min_bump_width > 1.0d0) then 
      call my_stop("min_bump_width must be > 0.01 and < 1.0.")
    end if

  end subroutine read_hh_inputs



  subroutine read_bezier_inputs  (iunit, bezier)

    !! read input file for bezier shape options 

    use shape_airfoil,        only : shape_bezier_type
    use shape_bezier,         only : ncp_to_ndv

    integer, intent(in)                   :: iunit
    type(shape_bezier_type), intent(out)  :: bezier

    integer     :: ncp_top, ncp_bot
    integer     :: iostat1

    namelist /bezier_options/ ncp_top, ncp_bot

    ! Init default values 

    ncp_top = 6    
    ncp_bot = 6    
    
    if (iunit > 0) then
      rewind (iunit)
      read (iunit, iostat=iostat1, nml=bezier_options)
      call namelist_check('bezier_options', iostat1, 'no-warn')
    end if
    
    ! Put options into derived types

    bezier%ncp_top = ncp_top
    bezier%ncp_bot = ncp_bot
    bezier%ndv     = ncp_to_ndv (ncp_top, ncp_bot)

    if (ncp_top < 3 .or. ncp_top > 10) &
      call my_stop("Number of Bezier control points must be >= 3 and <= 10.")
    if (ncp_bot < 3 .or. ncp_bot > 10) &
      call my_stop("Number of Bezier control points must be >= 3 and <= 10.")
    
  end subroutine read_bezier_inputs



  subroutine read_match_foils_inputs  (iunit, eval_spec)

    !! read input file for bezier shape options 

    use eval_commons,        only : eval_spec_type

    integer, intent(in)                  :: iunit
    type(eval_spec_type), intent(inout)  :: eval_spec

    integer         :: iostat1
    logical         :: match_foils
    character(255)  :: matchfoil_file

    namelist /matchfoil_options/ match_foils, matchfoil_file

    ! Init default values 

    match_foils = .false.
    matchfoil_file = 'none'
    
    if (iunit > 0) then
      rewind (iunit)
      read (iunit, iostat=iostat1, nml=matchfoil_options)
      call namelist_check('bezier_options', iostat1, 'no-warn')
    end if
    
    ! Put options into derived types

    eval_spec%match_foils         = match_foils
    eval_spec%foil_to_match_name  = trim(matchfoil_file)
    
  end subroutine read_match_foils_inputs



  subroutine read_flap_worker_inputs  (iunit, flap_spec, degrees) 

    !! Read flap setting options

    use commons,                  only : MAX_NOP
    use xfoil_driver,             only : flap_spec_type    

    integer, intent(in)           :: iunit
    type(flap_spec_type), intent(out) :: flap_spec
    double precision, allocatable, intent(out) :: degrees (:)  

    double precision, dimension(MAX_NOP) :: flap_degrees
    double precision               :: x_flap, y_flap
    character(3)                   :: y_flap_spec
    integer                        :: iostat1, i, ndegrees
    logical                        :: use_flap

    namelist /operating_conditions/ x_flap, y_flap, y_flap_spec, &
                                    flap_degrees

    ! Init default values 

    x_flap       = 0.75d0
    y_flap       = 0.d0
    y_flap_spec  = 'y/c'

    flap_degrees      = 0d0

    ! Open input file and read namelist from file

    if (iunit > 0) then
      rewind (iunit)
      read (iunit, iostat=iostat1, nml=operating_conditions)
      call namelist_check('operating_conditions', iostat1, 'no-warn')
    end if
    

    ! All parms optional - so no warning call namelist_check ...

    ! Check Input 

    if ((use_flap) .and. (x_flap <= 0.0)) call my_stop("x_flap must be > 0.")
    if ((use_flap) .and. (x_flap >= 1.0)) call my_stop("x_flap must be < 1.")
    if ((use_flap) .and. (y_flap_spec /= 'y/c') .and. (y_flap_spec /= 'y/t'))    &
      call my_stop("y_flap_spec must be 'y/c' or 'y/t'.")

    if ((y_flap_spec  /= 'y/c') .and. (y_flap_spec  /= 'y/t')) &
      call my_stop ("Vertical hinge definition must be 'y/c' or 'y/t'")
    do i = 1, size(flap_degrees)
      if (abs(flap_degrees(i)) > 70d0) &
        call my_stop ('Flap angle must be less than 70 degrees')
    end do

    ndegrees = 0

    do i = size(flap_degrees), 1, -1
      if (flap_degrees(i) /= 0d0) then
        ndegrees = i
        exit
      end if
    end do

    flap_spec%x_flap      = x_flap
    flap_spec%y_flap      = y_flap
    flap_spec%y_flap_spec = y_flap_spec

    if (ndegrees == 0) then 
      allocate (degrees(0))
    else
      degrees = flap_degrees (1:ndegrees)
    end if 


  end subroutine read_flap_worker_inputs



  subroutine my_op_stop (iop, op_points_spec, message)

    !! Stops execution when there is an invalid op_point parameter

    use xfoil_driver,       only : op_point_spec_type

    type(op_point_spec_type), allocatable, intent(in)  :: op_points_spec (:)
    integer, intent (in)      :: iop
    character(*), intent(in)  :: message

    call echo_op_points_spec  (op_points_spec)

    call my_stop ('Op_point '// stri(iop) //': '// message)

  end subroutine my_op_stop



  subroutine echo_op_points_spec  (op_points_spec, xfoil_options)

    !! Echo input parms of operating points entered by user

    use xfoil_driver,       only : op_point_spec_type
    use xfoil_driver,       only : xfoil_options_type

    type(op_point_spec_type), dimension(:), intent(in)  :: op_points_spec
    type(xfoil_options_type), intent(in), optional  :: xfoil_options

    integer               :: i, re_int
    character(10)         :: spec_char, target_value_char, ncrit_char, dynamic_char
    type(op_point_spec_type) :: op


    !write (*,'(" - ",A)') 'Echo operating point definitions'
    write (*,*) 
    write (*,'(" - ",A2,":",A7,A6,A15,A9,A10,A5,A7,A15)') &
                  'No', 'Spec', 'Point', 'Opt. Type', ' Target', & 
                  'Re', 'Type', 'ncrit', 'Weighting'
    
    do i = 1, size (op_points_spec)

      op = op_points_spec(i)

      if (op%spec_cl) then
        spec_char = 'cl'
      else
        spec_char = 'alpha'
      end if

      if (op%target_value == -1.d3 ) then 
        target_value_char = '-'
      elseif (op%target_value >= 10d0 ) then 
        write (target_value_char,'(F9.2)') op%target_value
      else
        write (target_value_char,'(F9.5)') op%target_value
      end if

      if (op%ncrit == -1.d0 ) then 
        if ( present(xfoil_options)) then
          write (ncrit_char,'(F7.1)') xfoil_options%ncrit
        else
          ncrit_char = '-'
        end if 
      else
        write (ncrit_char,'(F7.1)') op%ncrit
      end if

      if (op%dynamic_weighting) then 
        dynamic_char = 'dynamic'
      else
        dynamic_char = ''
      end if

      re_int = int (op%re%number)

      write (*,'(3x,I2,":",A7,F6.2,A15,A9,I10,I5,A7,F11.2, 1x,A7)') &
            i, trim(spec_char), op%value, op%optimization_type, trim(target_value_char), &
            re_int, op%re%type, trim(ncrit_char), op%weighting_user, trim(dynamic_char)
    end do 

    write (*,*)

  end subroutine echo_op_points_spec



  subroutine read_curvature_inputs  (iunit, curv_constraints)

    !! Read 'curvature' inputs into derived types

    use commons,                only : NOT_DEF_D, NOT_DEF_I
    use eval_commons,           only : curv_constraints_type, curv_side_constraints_type

    integer, intent(in)                :: iunit
    type (curv_constraints_type),  intent(inout) :: curv_constraints

    type (curv_side_constraints_type) :: curv_top_spec
    type (curv_side_constraints_type) :: curv_bot_spec
    type (curv_side_constraints_type) :: spec

    integer :: iostat1
    integer :: max_curv_reverse_top, max_curv_reverse_bot
    double precision  :: max_te_curvature
    double precision  :: curv_threshold, spike_threshold
    logical :: check_curvature, auto_curvature, do_smoothing

    ! #depricated
    integer :: max_spikes_top, max_spikes_bot

    namelist /curvature/   check_curvature, auto_curvature, do_smoothing, &
                          spike_threshold, curv_threshold, &
                          max_te_curvature, &
                          max_curv_reverse_top, max_curv_reverse_bot,  &
                          max_spikes_top, max_spikes_bot, &
                          curv_top_spec, curv_bot_spec


    ! Default values for curvature parameters

    do_smoothing         = .false.              ! now default - smoothing will be forced if 
                                                !               quality of surface is bad
    check_curvature      = .true.
    auto_curvature       = .true.
    max_te_curvature     = 10.d0                ! more or less inactive by default
    max_curv_reverse_top = 0
    max_curv_reverse_bot = 0
    max_spikes_top       = 0
    max_spikes_bot       = 0
    curv_threshold       = 0.1d0
    spike_threshold      = 0.4d0


    ! Set final top and bot data structure to "undefined" 
    ! - to detect user overwrite in input file (Expert mode) 

    spec%check_curvature_bumps = .false.
    spec%max_te_curvature = NOT_DEF_D
    spec%max_curv_reverse = NOT_DEF_I
    spec%max_spikes       = NOT_DEF_I
    spec%curv_threshold   = NOT_DEF_D
    spec%spike_threshold  = NOT_DEF_D

    curv_top_spec = spec
    curv_bot_spec = spec

    ! Open input file and read namelist from file

    if (iunit > 0) then
      rewind (iunit)
      read (iunit, iostat=iostat1, nml=curvature)
      call namelist_check('curvature', iostat1, 'no-warn')
    end if

    curv_constraints%check_curvature      = check_curvature
    curv_constraints%auto_curvature       = auto_curvature
    curv_constraints%do_smoothing         = do_smoothing

    ! Allow user input of detailed internal structures  

    spec = curv_top_spec

    if (spec%max_te_curvature == NOT_DEF_D) spec%max_te_curvature = max_te_curvature
    if (spec%max_curv_reverse == NOT_DEF_I) spec%max_curv_reverse = max_curv_reverse_top
    if (spec%max_spikes       == NOT_DEF_I) spec%max_spikes       = max_spikes_top
    if (spec%curv_threshold   == NOT_DEF_D) spec%curv_threshold   = curv_threshold
    if (spec%spike_threshold  == NOT_DEF_D) spec%spike_threshold  = spike_threshold

    curv_top_spec = spec 

    spec = curv_bot_spec

    if (spec%max_te_curvature == NOT_DEF_D) spec%max_te_curvature = max_te_curvature
    if (spec%max_curv_reverse == NOT_DEF_I) spec%max_curv_reverse = max_curv_reverse_bot
    if (spec%max_spikes       == NOT_DEF_I) spec%max_spikes       = max_spikes_bot
    if (spec%curv_threshold   == NOT_DEF_D) spec%curv_threshold   = curv_threshold
    if (spec%spike_threshold  == NOT_DEF_D) spec%spike_threshold  = spike_threshold

    curv_bot_spec = spec 

    if (curv_constraints%check_curvature ) then 
      if (curv_top_spec%curv_threshold   < 0.01d0)  call my_stop("curv_threshold must be >= 0.01")
      if (curv_top_spec%spike_threshold  < 0.01d0 ) call my_stop ("spike_threshold must be >= 0.01")
      if (curv_top_spec%max_curv_reverse < 0)       call my_stop("max_curv_reverse_top must be >= 0")
      if (curv_bot_spec%max_curv_reverse < 0)       call my_stop("max_curv_reverse_bot must be >= 0")
      if (curv_top_spec%max_te_curvature < 0.d0)    call my_stop("max_te_curvature must be >= 0")
    end if 

    curv_constraints%top = curv_top_spec
    curv_constraints%bot = curv_bot_spec

  end subroutine read_curvature_inputs



  subroutine read_constraints_inputs  (iunit, geo_constraints, flap_spec)

    !! Read 'constraints' inputs into derived types

    use commons,                only : NOT_DEF_D, NOT_DEF_I
    use xfoil_driver,           only : flap_spec_type
    use eval_commons,           only : geo_constraints_type

    integer, intent(in)                         :: iunit
    type (geo_constraints_type), intent(inout)  :: geo_constraints
    type (flap_spec_type), intent(inout)        :: flap_spec

    integer             :: iostat1
    logical             :: check_geometry, symmetrical
    double precision    :: min_thickness, max_thickness, min_te_angle, min_camber, max_camber
    double precision    :: min_flap_degrees, max_flap_degrees

    namelist /constraints/  min_thickness, max_thickness, min_camber, max_camber, &
                            min_te_angle,  &
                            check_geometry, symmetrical, &
                            min_flap_degrees, max_flap_degrees
                            ! naddthickconst, addthick_x, addthick_min, addthick_max

    ! Default values for curvature parameters

    check_geometry = .true.
    min_thickness = NOT_DEF_D
    max_thickness = NOT_DEF_D
    min_camber    = NOT_DEF_D
    max_camber    = NOT_DEF_D
    min_te_angle  = 2.d0

    symmetrical = .false.
    min_flap_degrees = -5.d0
    max_flap_degrees = 15.d0

    ! naddthickconst = 0
    ! addthick_x(:) = 0.01d0
    ! addthick_min(:) = -1000.d0
    ! addthick_max(:) = 1000.d0
    
    ! Open input file and read namelist from file

    if (iunit > 0) then
      rewind (iunit)
      read (iunit, iostat=iostat1, nml=constraints)
      call namelist_check('constraints', iostat1, 'no-warn')
    end if

    geo_constraints%check_geometry = check_geometry
    geo_constraints%symmetrical = symmetrical
    geo_constraints%min_thickness = min_thickness
    geo_constraints%max_thickness = max_thickness
    geo_constraints%min_camber = min_camber
    geo_constraints%max_camber = max_camber   
    geo_constraints%min_te_angle = min_te_angle

    flap_spec%min_flap_degrees = min_flap_degrees
    flap_spec%max_flap_degrees = max_flap_degrees

    ! #todo flap min, max 

    ! Sort thickness constraints in ascending x/c order
    ! if (naddthickconst > 0) then
    !   call sort_vector(addthick_x(1:naddthickconst), sort_idxs(1:naddthickconst))
    !   temp_thickmin = addthick_min
    !   temp_thickmax = addthick_max
    !   do i = 1, naddthickconst
    !     addthick_min(i) = temp_thickmin(sort_idxs(i))
    !     addthick_max(i) = temp_thickmax(sort_idxs(i))
    !   end do
    ! end if

    if (min_thickness /= NOT_DEF_D .and. min_thickness <= 0.d0) &
        call my_stop("min_thickness must be > 0.")
    if (max_thickness /= NOT_DEF_D .and. max_thickness <= 0.d0) &
        call my_stop("max_thickness must be > 0.")
    if (min_thickness /= NOT_DEF_D .and. max_thickness /= NOT_DEF_D .and. min_thickness >= max_thickness) & 
        call my_stop("min_thickness must be < max_thickness.")

    if (min_camber /= NOT_DEF_D .and. min_camber <= 0.d0) &
        call my_stop("min_camber must be > 0.")
    if (max_camber /= NOT_DEF_D .and. max_camber <= 0.d0) &
        call my_stop("max_camber must be > 0.")
    if (min_camber /= NOT_DEF_D .and. max_camber /= NOT_DEF_D .and. min_camber >= max_camber) & 
        call my_stop("min_camber must be < max_camber.")

    if (min_te_angle < 0.d0) &
        call my_stop("min_te_angle must be >= 0.")
    if (symmetrical)                                                             &
        call print_note ("Mirroring top half of seed airfoil for symmetrical constraint.")
    if (min_flap_degrees >= max_flap_degrees)                                    &
        call my_stop("min_flap_degrees must be < max_flap_degrees.")
    if (min_flap_degrees <= -30.d0)                                              &
        call my_stop("min_flap_degrees must be > -30.")
    if (max_flap_degrees >= 30.d0)                                               &
        call my_stop("max_flap_degrees must be < 30.")
    
    ! if (naddthickconst > max_addthickconst) then
    !    write(text,*) max_addthickconst
    !    text = adjustl(text)
    !    call my_stop("naddthickconst must be <= "//trim(text)//".")
    ! end if
    ! do i = 1, naddthickconst
    !   if (addthick_x(i) <= 0.d0) call my_stop("addthick_x must be > 0.")
    !   if (addthick_x(i) >= 1.d0) call my_stop("addthick_x must be < 1.")
    !   if (addthick_min(i) >= addthick_max(i))                                    &
    !     call my_stop("addthick_min must be < addthick_max.")
    ! end do

  end subroutine read_constraints_inputs




  subroutine read_xfoil_paneling_inputs  (iunit, geom_options)

    !! Read xoptfoil input file to xfoil_paneling_options

    use xfoil_driver,       only : xfoil_geom_options_type

    integer, intent(in)      :: iunit
    type(xfoil_geom_options_type), intent(out) :: geom_options
    double precision :: cvpar, cterat, ctrrat, xsref1, xsref2, xpref1, xpref2

    integer :: npan
    integer :: iostat1
    logical :: repanel

    namelist /xfoil_paneling_options/ npan, cvpar, cterat, ctrrat, xsref1,       &
              xsref2, xpref1, xpref2

    ! Init default values for xfoil options

    npan   = 200            ! a real default

    cvpar  = 2d0            ! increase bunching based on curvature to get fine le 
    cterat = 0.15d0            
    ctrrat = 0.2d0
    xsref1 = 1.d0
    xsref2 = 1.d0
    xpref1 = 1.d0
    xpref2 = 1.d0

    repanel = .false.          ! repanel for each design before running xfoil

    ! Open input file and read namelist from file

    if (iunit > 0) then
      rewind (iunit)
      read (iunit, iostat=iostat1, nml=xfoil_paneling_options)
      call namelist_check('xfoil_paneling_options', iostat1, 'no-warn')
    end if
    
    ! Put xfoil options into derived types

    if (npan < 80) call my_stop("npan must be >= 80.")
    if (cvpar <= 0.d0) call my_stop("cvpar must be > 0.")
    if (cterat < 0.d0) call my_stop("cterat must be >= 0.")
    if (ctrrat <= 0.d0) call my_stop("ctrrat must be > 0.")
    if (xsref1 < 0.d0) call my_stop("xsref1 must be >= 0.")
    if (xsref2 < xsref1) call my_stop("xsref2 must be >= xsref1")
    if (xsref2 > 1.d0) call my_stop("xsref2 must be <= 1.")
    if (xpref1 < 0.d0) call my_stop("xpref1 must be >= 0.")
    if (xpref2 < xpref1) call my_stop("xpref2 must be >= xpref1")
    if (xpref2 > 1.d0) call my_stop("xpref2 must be <= 1.")


    geom_options%repanel = repanel
    geom_options%npan   = npan
    geom_options%cvpar  = cvpar
    geom_options%cterat = cterat
    geom_options%ctrrat = ctrrat
    geom_options%xsref1 = xsref1
    geom_options%xsref2 = xsref2
    geom_options%xpref1 = xpref1
    geom_options%xpref2 = xpref2

  end subroutine read_xfoil_paneling_inputs



  subroutine read_pso_options_inputs  (iunit, shape_spec_type, initial_perturb,  pso_options)

    !! Read 'particle_swarm_options' and 'initialization' input options 
    !! into pso_options 

    use particle_swarm, only : pso_options_type
    use shape_airfoil,  only : CAMB_THICK

    integer, intent(in)           :: iunit
    integer, intent(in)           :: shape_spec_type
    double precision, intent(in)  :: initial_perturb
    type(pso_options_type), intent(out) :: pso_options

    integer           :: pso_pop, pso_maxit, feasible_init_attempts
    double precision  :: pso_tol
    logical           :: feasible_init
    integer           :: iostat1
    character(20)     :: pso_convergence_profile


    namelist /initialization/ feasible_init,                    &
                              feasible_init_attempts
    
    namelist /particle_swarm_options/ pso_pop, pso_tol, pso_maxit,               &
                                      pso_convergence_profile,                   &
                                      pso_options       ! allow direct manipulation

    ! initialization default options

    feasible_init = .true. 
    feasible_init_attempts = 1000

    ! PSO default options
    
    pso_pop = 30
    pso_tol = 0.005d0
    pso_maxit = 500

    ! #todo remove
    if (shape_spec_type == CAMB_THICK) then
      pso_convergence_profile = 'quick_camb_thick'
    else
      pso_convergence_profile = 'exhaustive'
    end if
                            
    ! Rewind (open) unit 

    if (iunit > 0) then
      rewind (iunit)
      read (iunit, iostat=iostat1, nml=particle_swarm_options)
      call namelist_check('particle_swarm_options', iostat1, 'no-warn')
      rewind (iunit)
      read (iunit, iostat=iostat1, nml=initialization)
      call namelist_check('initialization', iostat1, 'no-warn')
    end if
    
    pso_options%pop = pso_pop
    pso_options%tol = pso_tol
    pso_options%maxspeed = initial_perturb
    pso_options%maxit = pso_maxit
    pso_options%convergence_profile = pso_convergence_profile

    pso_options%feasible_init = feasible_init
    pso_options%feasible_init_attempts = feasible_init_attempts

    ! Input checks 
    ! #todo move init to pso 
      
    if ((feasible_init_attempts < 1) .and. feasible_init)                        &
      call my_stop("feasible_init_attempts must be > 0.")

      if (pso_pop < 1) call my_stop("pso_pop must be > 0.")
    if (pso_tol <= 0.d0) call my_stop("pso_tol must be > 0.")
    if (pso_maxit < 1) call my_stop("pso_maxit must be > 0.")  
    if ( (trim(pso_convergence_profile) /= "quick") .and.                    &
          (trim(pso_convergence_profile) /= "exhaustive") .and.               &
          (trim(pso_convergence_profile) /= "quick_camb_thick"))                       &
      call my_stop("pso_convergence_profile must be 'exhaustive' "//&
                    "or 'quick' or 'quick_camb_thick'.")

  end subroutine 



  subroutine read_genetic_options_inputs  (iunit, ga_options)

    !! Read 'genetic_algorithm_options' and 'initialization' input options 
    !! into ga_options 

    use genetic_algorithm, only : ga_options_type

    integer, intent(in)           :: iunit
    type(ga_options_type), intent(out) :: ga_options

    integer           :: feasible_init_attempts
    logical           :: feasible_init
    integer           :: iostat1

    double precision  :: ga_tol, parent_fraction, roulette_selection_pressure,    &
                        tournament_fraction, crossover_range_factor,             &
                        mutant_probability, chromosome_mutation_rate,            &
                        mutation_range_factor
    integer           :: ga_pop, ga_maxit
    character(10)     :: parents_selection_method

    namelist /initialization/ feasible_init,    &
                              feasible_init_attempts
    
    namelist /genetic_algorithm_options/ ga_pop, ga_tol, ga_maxit,               &
              parents_selection_method, parent_fraction,                         &
              roulette_selection_pressure, tournament_fraction,                  &
              crossover_range_factor, mutant_probability,                        &
              chromosome_mutation_rate, mutation_range_factor

    ! initialization default options

    feasible_init = .true. 
    feasible_init_attempts = 1000

    ! genetic algorithm default options

    ga_pop = 80
    ga_tol = 1.D-04
    ga_maxit = 700
    parents_selection_method = 'tournament'
    parent_fraction = 0.5d0
    roulette_selection_pressure = 8.d0
    tournament_fraction = 0.025d0
    crossover_range_factor = 0.5d0
    mutant_probability = 0.4d0
    chromosome_mutation_rate = 0.01d0
    mutation_range_factor = 0.2d0
                            
    ! Rewind (open) unit 

    if (iunit > 0) then
      rewind (iunit)
      read (iunit, iostat=iostat1, nml=genetic_algorithm_options)
      call namelist_check('genetic_algorithm_options', iostat1, 'no-warn')
      rewind (iunit)
      read (iunit, iostat=iostat1, nml=initialization)
      call namelist_check('initialization', iostat1, 'no-warn')
    end if

    ga_options%pop = ga_pop
    ga_options%tol = ga_tol
    ga_options%maxit = ga_maxit
    ga_options%parents_selection_method = parents_selection_method
    ga_options%parent_fraction = parent_fraction
    ga_options%roulette_selection_pressure = roulette_selection_pressure
    ga_options%tournament_fraction = tournament_fraction
    ga_options%crossover_range_factor = crossover_range_factor
    ga_options%mutant_probability = mutant_probability
    ga_options%chromosome_mutation_rate = chromosome_mutation_rate
    ga_options%mutation_range_factor = mutation_range_factor

    ga_options%feasible_init = feasible_init
    ga_options%feasible_init_attempts = feasible_init_attempts

    ! Input checks 
      
    if ((feasible_init_attempts < 1) .and. feasible_init)                        &
      call my_stop("feasible_init_attempts must be > 0.")

    if (ga_pop < 1) call my_stop("ga_pop must be > 0.")
    if (ga_tol <= 0.d0) call my_stop("ga_tol must be > 0.")
    if (ga_maxit < 1) call my_stop("ga_maxit must be > 0.")
    if ( (trim(parents_selection_method) /= "roulette") .and.                &
          (trim(parents_selection_method) /= "tournament") .and.              &
          (trim(parents_selection_method) /= "random") )                      &
      call my_stop("parents_selection_method must be 'roulette', "//&
                    "'tournament', or 'random'.")
    if ( (parent_fraction <= 0.d0) .or. (parent_fraction > 1.d0) )           &
      call my_stop("parent_fraction must be > 0 and <= 1.")
    if (roulette_selection_pressure <= 0.d0)                                 &
      call my_stop("roulette_selection_pressure must be > 0.")
    if ( (tournament_fraction <= 0.d0) .or. (tournament_fraction > 1.d0) )   &
      call my_stop("tournament_fraction must be > 0 and <= 1.")
    if (crossover_range_factor < 0.d0)                                       &
      call my_stop("crossover_range_factor must be >= 0.")
    if ( (mutant_probability < 0.d0) .or. (mutant_probability > 1.d0) )      &
      call my_stop("mutant_probability must be >= 0 and <= 1.") 
    if (chromosome_mutation_rate < 0.d0)                                     &
      call my_stop("chromosome_mutation_rate must be >= 0.")
    if (mutation_range_factor < 0.d0)                                        &
      call my_stop("mutation_range_factor must be >= 0.")

  end subroutine 



  subroutine read_simplex_options_inputs  (iunit, sx_options)

    !! Read 'simplex_options' input options into sx_options 

    use simplex_search, only : simplex_options_type

    integer, intent(in)           :: iunit
    type(simplex_options_type), intent(out) :: sx_options

    integer           :: iostat1

    integer           :: simplex_maxit
    double precision  :: simplex_tol

    namelist /simplex_options/ simplex_tol, simplex_maxit

    ! simplex default options

    simplex_tol = 1.0D-05
    simplex_maxit = 1000
                                
    ! Rewind (open) unit 

    if (iunit > 0) then
      rewind (iunit)
      read (iunit, iostat=iostat1, nml=simplex_options)
      call namelist_check('simplex_options', iostat1, 'no-warn')
    end if

    sx_options%tol = simplex_tol
    sx_options%maxit = simplex_maxit

    ! Input checks 

    if (simplex_tol <= 0.d0) call my_stop("simplex_tol must be > 0.")
    if (simplex_maxit < 1)   call my_stop("simplex_maxit must be > 0.")

  end subroutine 



  subroutine read_xfoil_options_inputs  (iunit, xfoil_options)

    !! Read xfoil_run_options input

    use xfoil_driver, only : xfoil_options_type

    integer, intent(in)      :: iunit
    type(xfoil_options_type), intent(out)    :: xfoil_options

    logical :: viscous_mode, silent_mode, fix_unconverged, reinitialize, show_details
    integer :: bl_maxit
    double precision :: ncrit, xtript, xtripb, vaccel
    integer :: iostat1

    namelist /xfoil_run_options/ ncrit, xtript, xtripb, viscous_mode,            &
    silent_mode, bl_maxit, vaccel, fix_unconverged, reinitialize, show_details


    ! Set default xfoil aerodynamics

    ncrit = 9.d0
    xtript = 1.d0
    xtripb = 1.d0
    viscous_mode = .true.
    silent_mode = .true.
    bl_maxit = 50             ! reduced to 50 as above the potential result is rarely usable..
    vaccel = 0.005d0          ! the original value of 0.01 leads to too many non convergences at 
                              !   higher lift --> reduced 
    fix_unconverged = .true.
    reinitialize = .false.    ! as run_xfoil is improved, this will speed up the xfoil calcs
    show_details = .false.    ! show success info during op point calculation

    ! Read xfoil options

    if (iunit > 0) then
      rewind (iunit)
      read (iunit, iostat=iostat1, nml=xfoil_run_options)
      call namelist_check('xfoil_run_options', iostat1, 'no-warn')
    end if

    ! XFoil run options

    if (ncrit < 0.d0) call my_stop("ncrit must be >= 0.")
    if (xtript < 0.d0 .or. xtript > 1.d0)                                        &
      call my_stop("xtript must be >= 0. and <= 1.")
    if (xtripb < 0.d0 .or. xtripb > 1.d0)                                        &
      call my_stop("xtripb must be >= 0. and <= 1.")
    if (bl_maxit < 1) call my_stop("bl_maxit must be > 0.")
    if (vaccel < 0.d0) call my_stop("vaccel must be >= 0.")


    ! Put xfoil options into derived types

    xfoil_options%ncrit = ncrit
    xfoil_options%xtript = xtript
    xfoil_options%xtripb = xtripb
    xfoil_options%viscous_mode = viscous_mode
    xfoil_options%silent_mode = silent_mode
    xfoil_options%maxit = bl_maxit
    xfoil_options%vaccel = vaccel
    xfoil_options%fix_unconverged = fix_unconverged
    xfoil_options%exit_if_unconverged = .false.
    xfoil_options%detect_outlier = .true.
    xfoil_options%reinitialize = reinitialize
    xfoil_options%show_details = show_details

  end subroutine read_xfoil_options_inputs



  ! ------------------------------------------------------------------------------


  subroutine open_input_file (input_file, iunit, optionally)

    !----------------------------------------------------------------------------
    !! open input file, returns iunit 
    !!  iunit > 0: file was openend
    !!  iunit = 0: file couldn't be openend (if optionally = .true.)  
    !----------------------------------------------------------------------------

    character(*), intent(in)      :: input_file 
    integer, intent(out)          :: iunit
    logical, intent(in), optional :: optionally

    integer :: ioerr
    logical :: is_optional
    character (255) :: msg 

    iunit = 12
    is_optional = present (optionally) .and. optionally

    open(unit=iunit, file=input_file, status='old', iostat=ioerr, iomsg=msg)

    if (ioerr /= 0) then 
      if (is_optional ) then
        iunit = 0
      else 
        call my_stop('Could not open input file '//trim(input_file)//' - '//trim(msg))
      end if 
    end if 

  end subroutine

  subroutine close_input_file (iunit)

    !! closes iunit (just for encapsulation of iunit=0)
    integer, intent(in)          :: iunit
    if (iunit > 0) close (iunit) 

  end subroutine


    
  subroutine namelist_check(nmlname, errcode, action_missing_nml)

    !! Prints error and stops or warns for bad namelist read

    character(*), intent(in) :: nmlname
    integer, intent(in) :: errcode
    character(*), intent(in) :: action_missing_nml

    if (errcode < 0) then
      if (trim(action_missing_nml) == 'warn') then
        call print_note ("Namelist '"//trim(nmlname)//& 
                        "' not found in input file. Using default values.")
      elseif (trim(action_missing_nml) == 'no-warn') then
        ! do nothing
      else
        call my_stop ('Namelist '//trim(nmlname)//' is required and was not found in input file.')
      end if

    else if (errcode == 2) then
      if (trim(action_missing_nml) == 'warn') then
        call print_note ('No input file. Using default values for namelist '// trim(nmlname))
      elseif (trim(action_missing_nml) == 'no-warn') then
        ! do nothing
      else
        call my_stop ('No input file. Namelist '//trim(nmlname)//' is required for operation.')
      end if

    else if (errcode > 0) then
      call my_stop ('Unrecognized variable in namelist '//trim(nmlname)//' (err='//stri(errcode)//')')
    else
      continue
    end if

  end subroutine namelist_check



  subroutine read_clo(input_file, output_prefix, airfoil_file, re_default, exename)

    !! Reads command line arguments 

    character(:), allocatable, intent(inout) :: input_file, output_prefix, airfoil_file
    double precision, intent(inout)          :: re_default
    character(*), intent(in)                 :: exename

    character(100) :: arg
    integer i, nargs
    logical getting_args

    nargs = iargc()
    if (nargs > 0) then
      getting_args = .true.
    else
      getting_args = .false.
    end if

    i = 1
    do while (getting_args)
      call getarg(i, arg) 

      if (trim(arg) == "-i") then
        if (i == nargs) then
          call my_stop("Must specify an input file with -i option.")
        else
          call getarg(i+1, arg)
          input_file = trim(arg) 
          i = i+2
        end if

      else if (trim(arg) == "-o") then
        if (i == nargs) then
          call my_stop("Must specify an output prefix with -o option.")
        else
          call getarg(i+1, arg)
          output_prefix = trim(arg) 
          i = i+2
        end if

      else if (trim(arg) == "-r") then
        if (i == nargs) then
          call my_stop("Must specify a re value for -r option.")
        else
          call getarg(i+1, arg)
          read (arg,*) re_default
          if (re_default == 0d0)     &
            call my_stop("-r option has no valid reynolds numer")
          i = i+2
        end if

      else if (trim(arg) == "-a") then
        if (i == nargs) then
          call my_stop("Must specify filename of seed airfoil for -a option.")
        else
          call getarg(i+1, arg)
          airfoil_file = trim(arg) 
          i = i+2
        end if

      else if ( (trim(arg) == "-h") .or. (trim(arg) == "--help") ) then
        call print_usage(exename)
        stop

      else
        call print_error ("Unrecognized option "//trim(arg)//".")
        call print_usage (exename)
        stop 1
      end if

      if (i > nargs) getting_args = .false.
    end do

  end subroutine read_clo



  subroutine print_usage(exeprint)

    !! Prints usage information

    character(*), intent(in) :: exeprint

    write(*,'(A)') 
    write(*,'(A)') '         (c) 2017-2019 Daniel Prosser (original Xoptfoil)'
    write(*,'(A)') '         (c) 2019-2024 Jochen Guenzel'
    write(*,'(A)')
    write(*,'(A)') "Usage: "//trim(exeprint)//" [OPTION]"
    write(*,'(A)')
    write(*,'(A)') "Options:"
    write(*,'(A)') "  -i input_file     Specify a non-default input file"
    write(*,'(A)') "  -o output_prefix  Specify a non-default output prefix"
    write(*,'(A)') "  -r xxxxxx         Specify a default reynolds number (re_default)"
    write(*,'(A)') "  -a airfoil_file   Specify filename of seed airfoil"
    write(*,'(A)') "  -h, --help        Display usage information and exit"
    write(*,'(A)')
    write(*,'(A)')
    write(*,'(A)') "Development page: https://github.com/jxjo/Xoptfoil2"
    write(*,'(A)') "Report bugs using the issue reporting system "
    write(*,'(A)')

  end subroutine print_usage


end module 

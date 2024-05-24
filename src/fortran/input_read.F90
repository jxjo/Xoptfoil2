! MIT License
! Copyright (C) 2017-2019 Daniel Prosser
! Copyright (c) 2022-2024 Jochen Guenzel
 
module input_read

  !------------------------------------------------------------------------------------
  ! Read command line and input file, build main parameter data structures  ... 
  !------------------------------------------------------------------------------------

  use os_util
  use print_util


  implicit none
  private

  public :: read_inputs

  public :: namelist_check
  public :: open_input_file, close_input_file

  public :: read_xfoil_options_inputs, read_operating_conditions_inputs
  public :: read_panel_options_inputs
  public :: read_bezier_inputs, read_flap_worker_inputs, read_curvature_inputs
  public :: read_polar_inputs

  integer, parameter    :: MAX_NOP = 30               ! max number of operating points
  integer, parameter    :: MAXPOLARS = 30             ! max number of polars


  contains


  subroutine read_inputs (input_file_in, &
                          airfoil_filename, output_prefix, show_details, wait_at_end, &
                          eval_spec, shape_spec, optimize_options)

    !------------------------------------------------------------------------------------
    !! read command line and the input file  
    !!  input_file_in : defaults to '' - needed for Worker 'check' 
    !------------------------------------------------------------------------------------

    use eval_commons,         only : eval_spec_type
    use shape_airfoil,        only : shape_spec_type
    use optimization,         only : optimize_spec_type


    character(*), intent(in)                :: input_file_in
    character(:), allocatable, intent(out)  :: output_prefix, airfoil_filename
    logical, intent(out)                    :: show_details 
    logical, intent(out)                    :: wait_at_end 

    type(eval_spec_type), intent(out)       :: eval_spec
    type(shape_spec_type), intent(out)      :: shape_spec
    type(optimize_spec_type), intent(out)   :: optimize_options

    character(:), allocatable   :: input_file
    double precision            :: re_default_cl
    integer                     :: iunit

    ! Set default names, read command line arguments
  
    output_prefix = ''                            ! default name of result airfoil 
    airfoil_filename = ''                         ! either from command line or input file 
    re_default_cl = 0d0                           ! either from command line or input file 
  
    if (input_file_in == '') then 
      input_file = ''                             ! default name of input file 
      call get_command_line (input_file, output_prefix, airfoil_filename, re_default_cl)
    else
      input_file = input_file_in                  ! for Worker check  
    end if 

    if (output_prefix == '') then                 ! take stem of filename as output prefix 
      output_prefix = filename_stem (input_file)
    end if 

      
    ! open input file to read all the single namelists

    call open_input_file (input_file, iunit)

    ! main namelist 

    call read_optimization_options_inputs (iunit, airfoil_filename, &
                                          shape_spec, optimize_options, show_details, wait_at_end)


    Call set_show_details (show_details)            ! will control print output of details 

    if (show_details) then 
      call print_header ("Processing input")
      call print_action ("Reading input", input_file)
      call print_action ("Output prefix", output_prefix)
    else 
      call print_header ("Processing   ", input_file)
      call print_header ("Output prefix", output_prefix)
    end if 

    ! shape functions
      
    call read_bezier_inputs           (iunit, shape_spec%bezier)
    call read_hicks_henne_inputs      (iunit, shape_spec%hh)
    call read_camb_thick_inputs       (iunit, shape_spec%camb_thick)


    ! operating conditions

    call read_operating_conditions_inputs (iunit, re_default_cl, eval_spec, shape_spec%flap_spec) 


    ! geo targets - start read and weight options

    call read_geometry_targets_inputs (iunit, eval_spec%geo_targets)


    ! geometry and curvature constraints

    call read_curvature_inputs        (iunit, eval_spec%curv_constraints)
    call read_constraints_inputs      (iunit, eval_spec%geo_constraints, shape_spec%flap_spec)


    ! optimizer options 

    call read_particle_swarm_options_inputs (iunit, optimize_options%pso_options)
    call read_simplex_options_inputs  (iunit, optimize_options%sx_options)


    ! Geometry options 

    call read_panel_options_inputs (iunit, eval_spec%panel_options)

    ! xfoil options 

    call read_xfoil_options_inputs    (iunit, eval_spec%xfoil_options)


    call close_input_file (iunit)


  end subroutine read_inputs


  ! ------------------------------------------------------------------------------


  subroutine read_optimization_options_inputs  (iunit, airfoil_filename, &
                                                shape_spec, optimize_options, show_details, wait_at_end)

    !! Read main namelist 'optimization_options'

    use shape_airfoil,        only : shape_spec_type, HICKS_HENNE, BEZIER, CAMB_THICK
    use shape_bezier,         only : is_bezier_file
    use shape_hicks_henne,    only : is_hh_file
    use airfoil_base,         only : is_dat_file
    use optimization,         only : optimize_spec_type, PSO

    integer, intent(in)                     :: iunit
    character (:), allocatable, intent(inout) :: airfoil_filename
    type(optimize_spec_type), intent(out)   :: optimize_options
    type(shape_spec_type), intent(inout)    :: shape_spec
    logical, intent(out)                    :: show_details, wait_at_end

    character (250)               :: airfoil_file, shape_functions
    integer                       :: iostat1
    integer                       :: cpu_threads

    namelist /optimization_options/ airfoil_file, shape_functions, show_details, &
                                    cpu_threads, wait_at_end

    ! defaults for main namelist options

    airfoil_file = ''
    shape_functions = 'hicks-henne'
    show_details = .true.                              ! Show more infos  / supress echo
    wait_at_end  = .false.                             ! wait at end for user to press 'Enter'
    cpu_threads  = -1                                  ! either absolut or relativ (max threads - cpu_threads)

    ! Open input file and read namelist from file

    if (iunit > 0) then
      rewind (iunit)
      read (iunit, iostat=iostat1, nml=optimization_options)
      call namelist_check('optimization_options', iostat1, 'warn')
    end if

    if (airfoil_filename == '') then                ! no airfoil_filename from command line ...
      airfoil_filename = trim(airfoil_file)         ! take from input file
    end if 

    ! no of cpu threads to use during optimzation 
    
    optimize_options%cpu_threads = cpu_threads

    ! currently only PSO supported
    
    optimize_options%type = PSO

    ! shape functions options 

    if      (trim(shape_functions) == 'bezier') then
      shape_spec%type = BEZIER
    else if (trim(shape_functions) == 'hicks-henne') then
      shape_spec%type = HICKS_HENNE
    else if (trim(shape_functions) == 'camb-thick') then
      shape_spec%type = CAMB_THICK
    else 
      call my_stop("Shape_functions "//quoted(shape_functions)//" not known")
    end if 

    shape_spec%type_as_text = trim(shape_functions)

    ! first check of airfoil filename 

    if (len(airfoil_filename) < 5) &
      call my_stop("Seed airfoil filename "//quoted(airfoil_filename)//" is not valid")

    if (.not. is_bezier_file (airfoil_filename) .and. &
        .not. is_hh_file     (airfoil_filename) .and. & 
        .not. is_dat_file    (airfoil_file)) &
      call my_stop("Seed airfoil "//quoted(airfoil_filename)//" extension must be "// &
                   "either '.dat', '.bez' or 'hicks'")

  end subroutine 



  subroutine read_operating_conditions_inputs  (iunit, re_default_cl, eval_spec, flap_spec)

    !! Read operating points specification from input file 

    use xfoil_driver,         only : op_point_spec_type, re_type
    use xfoil_driver,         only : flap_spec_type

    use eval_commons,         only : eval_spec_type
    use eval_commons,         only : dynamic_weighting_spec_type

    integer, intent(in)                   :: iunit
    double precision, intent(in)          :: re_default_cl            ! re default from command line 
    type(eval_spec_type), intent(out)     :: eval_spec
    type (flap_spec_type), intent(inout)  :: flap_spec

    integer                               :: noppoint
    type(op_point_spec_type), allocatable :: op_points_spec (:)
    type(dynamic_weighting_spec_type)     :: dynamic_weighting_spec


    ! Op_point specification 
    character(7),     dimension(MAX_NOP)  :: op_mode
    character(15),    dimension(MAX_NOP)  :: optimization_type
    double precision, dimension(MAX_NOP)  :: op_point, weighting
    double precision, dimension(MAX_NOP)  :: ncrit_pt, target_value, reynolds, mach
    double precision, dimension(MAX_NOP)  :: flap_angle
    logical, dimension(MAX_NOP)           :: flap_optimize 

    double precision            :: re_default, flap_angle_default, mach_default 
    logical                     :: re_default_as_resqrtcl, dynamic_weighting
    logical                     :: allow_improved_target
    type(op_point_spec_type)    :: op
    character (:), allocatable  :: opt_type

    integer               :: i, iostat1, nflap_opt
    double precision      :: x_flap, y_flap
    character(3)          :: y_flap_spec
    logical               :: use_flap

    type (re_type)        :: re_def

    namelist /operating_conditions/ noppoint, op_mode, op_point, reynolds, mach,   &
              target_value, weighting, optimization_type, ncrit_pt,                & 
              re_default_as_resqrtcl, re_default, mach_default, dynamic_weighting, dynamic_weighting_spec, &
              use_flap, x_flap, y_flap, y_flap_spec, flap_angle, flap_optimize, flap_angle_default, &
              allow_improved_target

    ! Set defaults for operating conditions and constraints

    noppoint            = 0

    re_default          = 400000d0
    mach_default        = 0d0 
    re_default_as_resqrtcl = .false.

    op_mode(:)          = 'spec-cl'
    op_point(:)         = 0.d0
    optimization_type(:) = 'target-drag'
    weighting(:)        = 1.d0
    reynolds(:)         = NOT_DEF_D                        
    mach(:)             = NOT_DEF_D
    ncrit_pt(:)         = -1d0
    target_value(:)     = NOT_DEF_D

    use_flap            = .false.                
    x_flap              = 0.75d0
    y_flap              = 0.d0
    y_flap_spec         = 'y/c'
    flap_angle_default  = 0d0 
    flap_angle (:)      = NOT_DEF_D 
    flap_optimize (:)   = .false. 

    allow_improved_target = .true.

    ! Default values controlling dynamic weighting 
    dynamic_weighting = .true. 
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

    nflap_opt = 0 
    allocate (op_points_spec(noppoint)) 

    do i = 1, noppoint

      op_points_spec(i)%spec_cl = (op_mode(i) == 'spec-cl')
      op_points_spec(i)%value   = op_point(i)
      
      op_points_spec(i)%ncrit = ncrit_pt(i)    
      op_points_spec(i)%optimization_type = trim(optimization_type (i))
      op_points_spec(i)%target_value = target_value (i)
      op_points_spec(i)%allow_improved_target = allow_improved_target
      
      if (reynolds(i) /= NOT_DEF_D) then
        op_points_spec(i)%re%number  = reynolds(i)
        op_points_spec(i)%re%type    = 1
      else                                    ! take default Re number
        op_points_spec(i)%re = re_def 
      end if

      if (mach(i) /= NOT_DEF_D) then 
        op_points_spec(i)%ma%number = mach(i)                ! mach number only Type 1
      else 
        op_points_spec(i)%ma%number = mach_default
      end if 
      op_points_spec(i)%ma%type    = 1

      op_points_spec(i)%weighting_user  = weighting (i)
      op_points_spec(i)%scale_factor    = 1d0

      op_points_spec(i)%weighting_user_cur = 0d0
      op_points_spec(i)%weighting_user_prv = 0d0

      ! map flap inputs to op point spec 

      if (use_flap) then 
        op_points_spec(i)%flap_optimize = flap_optimize(i) 
        if (flap_optimize(i)) nflap_opt = nflap_opt + 1     ! count no of op points with optimize 
        if (flap_angle(i) == NOT_DEF_D) then
          op_points_spec(i)%flap_angle = flap_angle_default ! take default value if nothing specified 
        else
          op_points_spec(i)%flap_angle = flap_angle(i)      ! set to defined value 
        end if 
      else 
        op_points_spec(i)%flap_optimize = .false. 
        op_points_spec(i)%flap_angle = 0d0                  ! 0 degrees is default 
      end if 
    end do 

    ! Dynamic weighting - if activated all op_points with 'target' will be dynamic 

    dynamic_weighting_spec%active = dynamic_weighting
    op_points_spec%extra_punch       = .false. 
    op_points_spec%dynamic_weighting = .false.      


    ! Flap settings to data structure

    flap_spec%use_flap    = use_flap
    flap_spec%ndv         = nflap_opt                         ! no of design variables in opti 
    flap_spec%x_flap      = x_flap
    flap_spec%y_flap      = y_flap
    flap_spec%y_flap_spec = y_flap_spec

    ! put sub types into main data structure 

    eval_spec%op_points_spec = op_points_spec
    eval_spec%dynamic_weighting_spec = dynamic_weighting_spec


    ! Check input data  ------------------------

    ! if (noppoint < 1) call my_stop("noppoint must be > 0")
    if (noppoint > MAX_NOP) then
      call my_stop("noppoints must be <= "//stri(MAX_NOP)//".")
    end if

    if (use_flap) then 
      if (x_flap <= 0.0) call my_stop("x_flap must be > 0.0")
      if (x_flap >= 1.0) call my_stop("x_flap must be < 1.0")
      if ((y_flap_spec /= 'y/c') .and. (y_flap_spec /= 'y/t'))    &
        call my_stop("y_flap_spec must be 'y/c' or 'y/t'.")
      if ((y_flap_spec  /= 'y/c') .and. (y_flap_spec  /= 'y/t')) &
        call my_stop ("Vertical hinge definition must be 'y/c' or 'y/t'")
    end if 

    do i = 1, noppoint

      op  = op_points_spec(i)
      opt_type = op%optimization_type
 

      if (op%re%number <= 0.d0) &
        call my_op_stop (i,op_points_spec, "reynolds must be > 0. Default value (re_default) could not be set")
        if (op%ma%number < 0.d0) &
        call my_op_stop (i,op_points_spec, "mach must be >= 0.")
      if (op%ma%number >= 1.d0) &
        call my_op_stop (i,op_points_spec, "mach must be < 1.")
      if (opt_type /= 'min-drag' .and. &
          opt_type /= 'max-glide' .and. &
          opt_type /= 'min-sink' .and. &
          opt_type /= 'max-lift' .and. &
          opt_type /= 'target-moment' .and. &
          opt_type /= 'target-drag' .and. &
          opt_type /= 'target-lift' .and. &
          opt_type /= 'target-glide' .and. &
          opt_type /= 'max-xtr') & 
        call my_op_stop (i,op_points_spec, "optimization_type must be 'min-drag', 'max-glide', "//     &
                    "'min-sink', 'max-lift', 'max-xtr', 'target-moment', "//    &
                    "'target-drag' or 'target-glide'")
      if ((op%ncrit <= 0.d0) .and. (op%ncrit /= -1d0)) &
        call my_op_stop (i,op_points_spec, "ncrit_pt must be > 0 or -1.")

      if (opt_type == 'target-moment' .and. target_value(i) == NOT_DEF_D) &
        call my_op_stop (i,op_points_spec, "No 'target-value' defined for "//  &
                  "for optimization_type 'target-moment'")
      if (opt_type == 'target-drag' .and. target_value(i) == NOT_DEF_D) &
        call my_op_stop (i,op_points_spec, "No 'target-value' defined for "//  &
                      "for optimization_type 'target-drag'")
      if (opt_type == 'target-glide' .and. target_value(i) == NOT_DEF_D) &
        call my_op_stop (i,op_points_spec, "No 'target-value' defined for "//  &
                      "for optimization_type 'target-glide'")
      if (opt_type == 'target-lift' .and. target_value(i) == NOT_DEF_D) &
        call my_op_stop (i,op_points_spec, "No 'target-value' defined for "//  &
                      "for optimization_type 'target-lift'")

      if (use_flap) then 

        if (flap_angle(i) /= NOT_DEF_D .and. abs(flap_angle(i)) > 70d0) &
          call my_op_stop (i,op_points_spec, "Flap angle must be less than 70 degrees")

      end if 


      if (op%value <= 0.d0 .and. op%spec_cl) then
        if ((opt_type /= 'min-drag') .and. &
            (opt_type /= 'max-xtr') .and. &
            (opt_type /= 'target-drag')) then
          call my_stop ("Operating point "//stri(i)//" is at Cl = 0. "// &
                        "Cannot use '"//opt_type//"' optimization in this case.")
        end if

      elseif (op%spec_cl .and. opt_type == 'max-lift') then
        call my_stop ("Cl is specified for operating point "//stri(i)// &
                      ". Cannot use 'max-lift' optimization type in this case.")

      elseif (op%spec_cl .and. opt_type == 'target-lift') then              
        call my_stop ("op_mode = 'spec_cl' doesn't make sense "// &
                      "for optimization_type 'target-lift'")
      end if

    end do

  end subroutine read_operating_conditions_inputs



  subroutine read_geometry_targets_inputs  (iunit, geo_targets)

    !! Read 'constraints' inputs into derived types

    use eval_commons,           only : geo_target_type

    integer, intent(in)                :: iunit
    type (geo_target_type), allocatable, intent(inout) :: geo_targets (:)

    integer :: iostat1

    double precision, dimension(MAX_NOP) :: target_value, weighting
    character(30), dimension(MAX_NOP)    :: target_type, target_string
    logical, dimension(MAX_NOP)          :: preset_to_target 
    integer :: ngeo_targets, i
    logical :: match_foil

    ! deprecated
    double precision, dimension(MAX_NOP) :: target_geo
    double precision, dimension(MAX_NOP) :: weighting_geo

    namelist /geometry_targets/ ngeo_targets, target_type, target_geo, weighting_geo, &
                                target_value, weighting, target_string, &
                                preset_to_target

    ! Default values for curvature parameters

    ngeo_targets = 0
    target_type (:) = ''
    target_value(:) = 0.d0 
    target_string(:) = '' 
    weighting(:) = 1d0 
    preset_to_target (:) = .false.

    ! deprecated
    weighting_geo(:) = 0d0 
    target_geo(:) = 0.d0 


    ! Open input file and read namelist from file

    if (iunit > 0) then
      rewind (iunit)
      read (iunit, iostat=iostat1, nml=geometry_targets)
      call namelist_check('geometry_targets', iostat1, 'no-warn')
    end if


    ! Handle deprecated option names 

    if (sum(weighting_geo) /= 0d0) then 
      weighting = weighting_geo 
      call print_warning ("'weighting_geo' is deprecated - please use 'weighting'",5 )
    end if
    if (sum(target_geo) /= 0d0) then 
      target_value = target_geo
      call print_warning ("'target_geo' is deprecated - please use 'target_value'",5 )
    end if

    allocate (geo_targets(ngeo_targets)) 
    geo_targets%reference_value   = 0.d0
    geo_targets%dynamic_weighting = .false.

    do i = 1, ngeo_targets
      geo_targets(i)%type           = to_lower (trim(target_type(i)))
      geo_targets(i)%preset_to_target = preset_to_target(i)
      geo_targets(i)%target_value   = target_value(i)
      geo_targets(i)%target_string  = target_string(i)
      geo_targets(i)%weighting      = weighting(i)              ! will be normalized        
      geo_targets(i)%weighting_user = weighting(i)
    end do   

    ! Geo targets - check options

    match_foil = .false. 

    do i = 1, ngeo_targets
      if ((geo_targets(i)%type /= 'camber') .and. &
          (geo_targets(i)%type /= 'match-foil') .and. &
          (geo_targets(i)%type /= 'thickness')) &
      call my_stop("Target_type must be 'camber' or 'thickness'.")

      if (geo_targets(i)%type == 'match-foil') match_foil = .true.
    end do   

    if (match_foil .and. ngeo_targets > 1) &
      call my_stop("Beside 'match-foil' no other geometry targets can be defined.")

  end subroutine 



  subroutine read_hicks_henne_inputs  (iunit, hh)

    !! read input file for hicks henne shape options 

    use shape_hicks_henne,    only : shape_hh_type
    use shape_hicks_henne,    only : nfunctions_to_ndv

    integer, intent(in)                   :: iunit
    type(shape_hh_type), intent(out)      :: hh

    double precision    :: initial_perturb                
    integer             :: nfunctions_top, nfunctions_bot
    logical             :: smooth_seed
    integer             :: iostat1

    namelist /hicks_henne_options/ nfunctions_top, nfunctions_bot, &
                                  initial_perturb, smooth_seed   

    ! Init default values 

    smooth_seed     = .false.
    nfunctions_top  = 3
    nfunctions_bot  = 3
    initial_perturb = 0.1d0                  ! good value - about 10% of dv solution space

    if (iunit > 0) then
      rewind (iunit)
      read (iunit, iostat=iostat1, nml=hicks_henne_options)
      call namelist_check('hicks_henne_options', iostat1, 'no-warn')
    end if

    ! Put options into derived types

    hh%nfunctions_top   = nfunctions_top
    hh%nfunctions_bot   = nfunctions_bot
    hh%ndv              = nfunctions_to_ndv (nfunctions_top, nfunctions_bot)
    hh%initial_perturb  = initial_perturb
    hh%smooth_seed      = smooth_seed

    if (nfunctions_top < 0) &
      call my_stop("nfunctions_top must be >= 0.")
    if (nfunctions_bot < 0) &
      call my_stop("nfunctions_bot must be >= 0.")
    if (initial_perturb < 0.01d0 .or. initial_perturb > 0.5d0) &
      call my_stop("Bezier: initial_perturb must be >= 0.01 and <= 0.5")

  end subroutine read_hicks_henne_inputs



  subroutine read_bezier_inputs  (iunit, bezier)

    !! read input file for bezier shape options 
 
    use shape_bezier,         only : shape_bezier_type
    use shape_bezier,         only : ncp_to_ndv

    integer, intent(in)                   :: iunit
    type(shape_bezier_type), intent(out)  :: bezier

    double precision    :: initial_perturb                
    integer     :: ncp_top, ncp_bot
    integer     :: iostat1

    namelist /bezier_options/ ncp_top, ncp_bot, initial_perturb

    ! Init default values 

    ncp_top = 5                              ! number of control points - top 
    ncp_bot = 5                              ! number of control points - bot 
    initial_perturb = 0.1d0                  ! good value - about 10% of dv solution space
    
    if (iunit > 0) then
      rewind (iunit)
      read (iunit, iostat=iostat1, nml=bezier_options)
      call namelist_check('bezier_options', iostat1, 'no-warn')
    end if
    

    if (ncp_top < 3 .or. ncp_top > 10) &
      call my_stop("Number of Bezier control points must be >= 3 and <= 10")
    if (ncp_bot < 3 .or. ncp_bot > 10) &
      call my_stop("Number of Bezier control points must be >= 3 and <= 10")
    if (initial_perturb < 0.01d0 .or. initial_perturb > 0.5d0) &
      call my_stop("Bezier: initial_perturb must be >= 0.01 and <= 0.5")


    bezier%ncp_top = ncp_top
    bezier%ncp_bot = ncp_bot
    bezier%ndv     = ncp_to_ndv (ncp_top) + ncp_to_ndv (ncp_bot)
    bezier%initial_perturb = initial_perturb
  
  end subroutine read_bezier_inputs



  subroutine read_camb_thick_inputs  (iunit, camb_thick)

    !! read input file for bezier shape options 

    use shape_camb_thick,        only : shape_camb_thick_type

    integer, intent(in)                      :: iunit
    type(shape_camb_thick_type), intent(out) :: camb_thick

    integer                       :: ndv                  ! number of design variables 
    logical                       :: thickness            ! optimize max thickness
    logical                       :: thickness_pos        ! optimize max thickness position 
    logical                       :: camber               ! optimize max camber
    logical                       :: camber_pos           ! optimize max camber position 
    logical                       :: le_radius            ! optimize le radius 
    logical                       :: le_radius_blend      ! optimize le radius blending distance
    double precision              :: initial_perturb      ! common max. initial perturb 
    integer     :: iostat1

    namelist /camb_thick_options/ thickness, thickness_pos, camber, camber_pos, le_radius, &
                                  le_radius_blend, initial_perturb

    ! Init default values 

    ndv               = 0    
    thickness         = .true.  
    thickness_pos     = .true. 
    camber            = .true.     
    camber_pos        = .true.   
    le_radius         = .true.     
    le_radius_blend   = .true. 
    initial_perturb   = 0.1d0
    
    if (iunit > 0) then
      rewind (iunit)
      read (iunit, iostat=iostat1, nml=camb_thick_options)
      call namelist_check('camb_thick_options', iostat1, 'no-warn')
    end if

    ! eval no of design vars

    ndv = 0 
    if (thickness)        ndv = ndv + 1
    if (thickness_pos)    ndv = ndv + 1
    if (camber)           ndv = ndv + 1
    if (camber_pos)       ndv = ndv + 1
    if (le_radius)        ndv = ndv + 1
    if (le_radius_blend)  ndv = ndv + 1

    ! Put options into derived types
             
    camb_thick%ndv = ndv       
    camb_thick%thickness       = thickness       
    camb_thick%thickness_pos   = thickness_pos  
    camb_thick%camber          = camber         
    camb_thick%camber_pos      = camber_pos     
    camb_thick%le_radius       = le_radius      
    camb_thick%le_radius_blend = le_radius_blend
    camb_thick%initial_perturb = initial_perturb

    if (ndv == 0) &
      call my_stop("All camb_thick options are switched off. There is nothing to optimize.")
    
  end subroutine 




  subroutine read_flap_worker_inputs  (iunit, flap_spec, angles) 

    !! Read flap setting options - returns a list of angles defined - minimum size=1 with 0 degrees

    use xfoil_driver,             only : flap_spec_type    

    integer, intent(in)           :: iunit
    type(flap_spec_type), intent(out)          :: flap_spec
    double precision, allocatable, intent(out) :: angles (:)  

    double precision, dimension(MAX_NOP) :: flap_angle
    double precision               :: x_flap, y_flap
    character(3)                   :: y_flap_spec
    integer                        :: iostat1, i, nangles
    logical                        :: use_flap

    namelist /operating_conditions/ x_flap, y_flap, y_flap_spec, flap_angle

    ! Init default values 

    nangles      = 0 
    use_flap     = .true. 
    x_flap       = 0.75d0
    y_flap       = 0.d0
    y_flap_spec  = 'y/c'

    flap_angle (:)  = NOT_DEF_D

    ! Open input file and read namelist from file

    if (iunit > 0) then
      rewind (iunit)
      read (iunit, iostat=iostat1, nml=operating_conditions)
      call namelist_check('operating_conditions', iostat1, 'no-warn')
    end if
    
    ! Check Input 

    if (use_flap) then 

      if (x_flap <= 0.0) call my_stop("x_flap must be > 0.0")
      if (x_flap >= 1.0) call my_stop("x_flap must be < 1.0")
      if ((y_flap_spec /= 'y/c') .and. (y_flap_spec /= 'y/t'))    &
        call my_stop("y_flap_spec must be 'y/c' or 'y/t'.")
      if ((y_flap_spec  /= 'y/c') .and. (y_flap_spec  /= 'y/t')) &
        call my_stop ("Vertical hinge definition must be 'y/c' or 'y/t'")

      do i = 1, size(flap_angle)

        if (flap_angle(i) /= NOT_DEF_D) then 
          if (abs(flap_angle(i)) > 70d0) call my_stop ('Flap angle must be less than 70 degrees')
        end if 

        if (flap_angle(i) /= NOT_DEF_D) then
          nangles = i
        end if

      end do

    end if 

    flap_spec%x_flap      = x_flap
    flap_spec%y_flap      = y_flap
    flap_spec%y_flap_spec = y_flap_spec

    if (nangles == 0) then 
      allocate (angles(1))
      angles = 0d0
    else
      angles = flap_angle (1:nangles)
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

      if (op%target_value == NOT_DEF_D ) then 
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

    use eval_commons,           only : curv_constraints_type, curv_side_constraints_type

    integer, intent(in)                :: iunit
    type (curv_constraints_type),  intent(inout) :: curv_constraints

    type (curv_side_constraints_type) :: curv_top_spec
    type (curv_side_constraints_type) :: curv_bot_spec
    type (curv_side_constraints_type) :: spec

    integer :: iostat1
    integer :: max_curv_reverse_top, max_curv_reverse_bot
    logical :: check_curvature, auto_curvature
    double precision  :: max_te_curvature
    double precision  :: curv_threshold, spike_threshold
    double precision  :: max_le_curvature_diff

    ! #depricated
    integer :: max_spikes_top, max_spikes_bot

    namelist /curvature/  check_curvature, auto_curvature, &
                          spike_threshold, curv_threshold, &
                          max_te_curvature, &
                          max_curv_reverse_top, max_curv_reverse_bot,  &
                          max_spikes_top, max_spikes_bot, &
                          curv_top_spec, curv_bot_spec, &
                          max_le_curvature_diff

    ! Default values for curvature parameters
                                                
    check_curvature      = .true.
    auto_curvature       = .true.
    max_te_curvature     = 4.9999d0                 ! strange value to recognize user input 
                                                    ! even with auto_curvature user may define te curvature 
    max_le_curvature_diff= 5d0                      ! Bezier: allowed diff of le curvature on top and bot 
    max_curv_reverse_top = 0
    max_curv_reverse_bot = 0
    max_spikes_top       = 0
    max_spikes_bot       = 0
    curv_threshold       = 0.1d0
    spike_threshold      = 0.5d0

    ! Set final top and bot data structure to "undefined" 
    ! - to detect user overwrite in input file (Expert mode) 

    spec%check_curvature_bumps = .true.
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

    curv_constraints%check_curvature       = check_curvature
    curv_constraints%auto_curvature        = auto_curvature
    curv_constraints%max_le_curvature_diff = max_le_curvature_diff

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

    if (curv_constraints%max_le_curvature_diff < 1d0) &
      call my_stop("max_le_curvature_diff must be >= 1.0")

  end subroutine read_curvature_inputs



  subroutine read_constraints_inputs  (iunit, geo_constraints, flap_spec)

    !! Read 'constraints' inputs into derived types

    use xfoil_driver,           only : flap_spec_type
    use eval_commons,           only : geo_constraints_type

    integer, intent(in)                         :: iunit
    type (geo_constraints_type), intent(inout)  :: geo_constraints
    type (flap_spec_type), intent(inout)        :: flap_spec

    integer             :: iostat1
    logical             :: check_geometry, symmetrical
    double precision    :: min_thickness, max_thickness, min_te_angle, min_camber, max_camber
    double precision    :: min_flap_angle, max_flap_angle

    namelist /constraints/  min_thickness, max_thickness, min_camber, max_camber, &
                            min_te_angle,  &
                            check_geometry, symmetrical, &
                            min_flap_angle, max_flap_angle
                            ! naddthickconst, addthick_x, addthick_min, addthick_max

    ! Default values for curvature parameters

    check_geometry = .true.
    min_thickness = NOT_DEF_D
    max_thickness = NOT_DEF_D
    min_camber    = NOT_DEF_D
    max_camber    = NOT_DEF_D
    min_te_angle  = 2.d0

    symmetrical = .false.
    min_flap_angle = -5.d0
    max_flap_angle = 15.d0
    
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

    flap_spec%min_flap_angle = min_flap_angle
    flap_spec%max_flap_angle = max_flap_angle


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

    if (min_flap_angle >= max_flap_angle)                                    &
        call my_stop("min_flap_angle must be < max_flap_angle.")
    if (min_flap_angle <= -30.d0)                                              &
        call my_stop("min_flap_angle must be > -30.")
    if (max_flap_angle >= 30.d0)                                               &
        call my_stop("max_flap_angle must be < 30.")
    
  end subroutine read_constraints_inputs



  subroutine read_panel_options_inputs  (iunit, panel_options)

    !! Read xoptfoil input file to geometry_options

    use airfoil_base,    only : panel_options_type

    integer, intent(in)                 :: iunit
    type(panel_options_type), intent(out) :: panel_options

    double precision  :: te_bunch, le_bunch
    integer           :: npoint, npan
    integer           :: iostat1

    namelist /panelling_options/ npoint, le_bunch, te_bunch, npan

    ! Init default values for xfoil options

    npoint   = 161          ! a real default
    npan     = 0            ! alternate input instead npoint 
    le_bunch = 0.86d0       ! for 161 a little higher than 0.82
    te_bunch = 0.6d0

    ! Open input file and read namelist from file

    if (iunit > 0) then
      rewind (iunit)
      read (iunit, iostat=iostat1, nml=panelling_options)
      call namelist_check('panelling_options', iostat1, 'no-warn')
    end if
    
    ! user choosed npan 

    if (npan > 0) then
      npoint = npan + 1
    end if

    ! Put xfoil options into derived types

    if (npoint < 80) call my_stop("npoint must be >= 80.")
    if (le_bunch < 0.d0 .or. le_bunch > 1d0) &
        call my_stop("le_bunch must be => 0 and <= 1.0.")
    if (te_bunch < 0.d0 .or. te_bunch > 1d0) &
        call my_stop("te_bunch must be => 0 and <= 1.0.")

    panel_options%npoint   = npoint
    panel_options%le_bunch = le_bunch
    panel_options%te_bunch = te_bunch

  end subroutine 


  subroutine read_particle_swarm_options_inputs  (iunit, pso_options)

    !! Read 'particle_swarm_options' and 'initialization' input options 
    !! into pso_options 

    use particle_swarm, only : pso_options_type
    use shape_airfoil,  only : CAMB_THICK

    integer, intent(in)           :: iunit
    type(pso_options_type), intent(out) :: pso_options

    integer           :: pop, max_iterations, init_attempts, max_retries
    double precision  :: min_radius, max_speed
    logical           :: rescue_particle
    integer           :: iostat1
    character(20)     :: convergence_profile
   
    namelist /particle_swarm_options/ pop, min_radius, max_iterations, max_speed,    &
                                      max_retries, convergence_profile, init_attempts, rescue_particle

    ! PSO default options
    
    convergence_profile = "exhaustive"
    pop = 30
    min_radius = 0.001d0
    max_iterations = 500
    max_speed = 0.1                       ! good value - about 10% of dv solution space
    max_retries = 2
                           ! max. retries of particle 
    init_attempts = 1000
    rescue_particle = .true.
                            
    ! Rewind (open) unit 

    if (iunit > 0) then
      rewind (iunit)
      read (iunit, iostat=iostat1, nml=particle_swarm_options)
      call namelist_check('particle_swarm_options', iostat1, 'no-warn')
    end if
    
    pso_options%pop             = pop
    pso_options%min_radius      = min_radius
    pso_options%max_speed       = max_speed
    pso_options%max_iterations  = max_iterations
    pso_options%max_retries     = max_retries
    pso_options%convergence_profile = trim(convergence_profile)
    pso_options%init_attempts   = init_attempts
    pso_options%rescue_particle = rescue_particle

    ! Input checks 
    
    if (max_speed > 0.7 .or. max_speed < 0.01) &
      call my_stop ("max_speed should be between 0.01 and 0.7")
    if (init_attempts < 1) &
      call my_stop("PSO: init_attempts must be > 0.")
    if (pop < 1) call my_stop("pop must be > 0.")
    if (min_radius <= 0.d0) call my_stop("min_radius must be > 0.")
    if (max_iterations < 1) call my_stop("max_iterations must be > 0.")  
    if ( (trim(convergence_profile) /= "quick") .and.                    &
         (trim(convergence_profile) /= "exhaustive") .and.               &
         (trim(convergence_profile) /= "quick_camb_thick")) &
      call my_stop("convergence_profile must be 'exhaustive' "//&
                    "or 'quick' or 'quick_camb_thick'.")

  end subroutine 



  subroutine read_simplex_options_inputs  (iunit, sx_options)

    !! Read 'simplex_options' input options into sx_options 

    use simplex_search, only : simplex_options_type

    integer, intent(in)           :: iunit
    type(simplex_options_type), intent(out) :: sx_options

    integer           :: iostat1

    integer           :: max_iterations
    double precision  :: min_radius

    namelist /simplex_options/ min_radius, max_iterations

    ! simplex default options

    min_radius = 1.0D-05
    max_iterations = 1000
                                
    ! Rewind (open) unit 

    if (iunit > 0) then
      rewind (iunit)
      read (iunit, iostat=iostat1, nml=simplex_options)
      call namelist_check('simplex_options', iostat1, 'no-warn')
    end if

    sx_options%min_radius = min_radius
    sx_options%max_iterations = max_iterations

    ! Input checks 

    if (min_radius <= 0.d0) call my_stop("simplex min_radius must be > 0.")
    if (max_iterations < 1)   call my_stop("simplex max_iterations must be > 0.")

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
    xfoil_options%exit_if_clmax = .false.
    xfoil_options%detect_outlier = .true.
    xfoil_options%reinitialize = reinitialize 
    xfoil_options%show_details = show_details
  end subroutine read_xfoil_options_inputs



  subroutine read_polar_inputs  (iunit, re_default, generate_polar, &
                                 auto_range, spec_cl, op_point_range, type_of_polar, &
                                 polar_reynolds, polar_mach)

    !----------------------------------------------------------------------------
    !! Read input file to get polar definition 
    !----------------------------------------------------------------------------

    use xfoil_driver,       only : xfoil_options_type, re_type

    integer,          intent(in)  :: iunit
    type (re_type),   intent(in)  :: re_default

    logical,          intent(out) :: generate_polar 
    logical,          intent(out) :: spec_cl, auto_range 
    integer,          intent(out) :: type_of_polar                      ! 1 or 2 
    double precision, intent(out) :: op_point_range (3)                 ! -1.0, 10.0, 0.5
    double precision, allocatable, intent(out) :: polar_reynolds (:)    ! 40000, 70000, 100000
    double precision, allocatable, intent(out) :: polar_mach (:)        ! 0.0, 0.2, 0.5

    character (7)   :: op_mode                                          ! 'spec-al' 'spec_cl'
    integer         :: iostat1, i, npolars

    namelist /polar_generation/ generate_polar, type_of_polar, polar_reynolds, polar_mach,  &
                                op_mode, op_point_range, auto_range

    ! Init default values for polars

    generate_polar  = .true.
    auto_range      = .false. 
    type_of_polar   = -1
    op_mode         = 'spec-al'
    op_point_range  = (/ -2d0, 10d0 , 1.0d0 /)
    allocate (polar_reynolds(MAXPOLARS))
    allocate (polar_mach    (MAXPOLARS))
    polar_reynolds  = 0d0
    polar_mach      = 0d0

    ! Open input file and read namelist from file

    if (iunit > 0) then
      rewind (iunit)
      read (iunit, iostat=iostat1, nml=polar_generation)
      call namelist_check('polar_generation', iostat1, 'warn')
    end if

    ! if there are no re numbers in input file take default

    if (polar_reynolds(1) == 0d0) then 
      polar_reynolds(1) = re_default%number 
    end if
    if (type_of_polar == -1) then 
      type_of_polar     = re_default%type
    end if 


    ! Input sanity

    if (type_of_polar /= 1 .and. type_of_polar /= 2) & 
      call my_stop ("polar_generation: Type of polars must be either '1' or '2'")

    if (.not. auto_range) then 
      if (op_mode /= 'spec-al' .and. op_mode /= 'spec-cl') &
        call my_stop ("polar_generation: op_mode must be 'spec-cl' or 'spec-al'")
      if ((op_point_range(2) - op_point_range(1)) <= 0d0 ) & 
        call my_stop ("polar_generation: End of polar op_point_range must be higher than the start.")
      if (( op_point_range(1) + op_point_range(3)) >= op_point_range(2) ) & 
        call my_stop ("polar_generation: Start of polar op_point_range + increment should be end of op_point_range.")
    end if 

    npolars = 0
    do i = 1, size(polar_reynolds)
      if (polar_reynolds(i) > 0d0) then   
        if (polar_reynolds(i) < 1000d0) then   
          call my_stop ("polar_generation: reynolds number must be >= 1000")
        else if (polar_mach(i) < 0d0) then 
          call my_stop ("polar_generation: mach number must be >= 0.0")
        else if (polar_mach(i) >= 1d0) then 
          call my_stop ("polar_generation: mach number must be <= 1.0")
        else 
          npolars = npolars + 1
        end if
      end if
    end do 

    if (npolars == 0) &
      call my_stop ("polar_generation: No Reynolds number found - either in input file nor as command line parameter.")

    spec_cl = (op_mode == 'spec-cl')
    polar_reynolds = polar_reynolds (1:npolars)
    polar_mach     = polar_mach     (1:npolars)

  end subroutine 


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
    if (present (optionally)) then 
      is_optional = optionally
    else 
      is_optional = .false. 
    end if 

    open(unit=iunit, file=input_file, status='old', iostat=ioerr, iomsg=msg)

    if (ioerr /= 0) then 
      if (is_optional ) then
        iunit = 0
      else 
        call my_stop(trim(msg))
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



  subroutine get_command_line (input_file, output_prefix, airfoil_file, re_default)

    !! Reads command line arguments 

    character(:), allocatable, intent(inout) :: input_file, output_prefix, airfoil_file
    double precision, intent(inout)          :: re_default

    character(250)            :: arg
    character (:),allocatable :: args
    integer                   :: i, nargs
    logical                   :: getting_args

    nargs = iargc()
    if (nargs > 0) then
      getting_args = .true.
    else
      call print_error ("Missing input file")
      call print_usage ()
      stop 1
      getting_args = .false.
    end if

    i = 1
    do while (getting_args)

      call get_command_argument (i, arg) 
      args = trim(arg)

      if (args == "-i") then
        if (i == nargs) then
          call my_stop("Must specify an input file with -i option.")
        else
          call getarg(i+1, arg)
          input_file = trim(arg) 
          i = i+2
        end if

      else if (args == "-o") then
        if (i == nargs) then
          call my_stop("Must specify an output prefix with -o option.")
        else
          call getarg(i+1, arg)
          output_prefix = trim(arg) 
          i = i+2
        end if

      else if (args == "-r") then
        if (i == nargs) then
          call my_stop("Must specify a re value for -r option.")
        else
          call getarg(i+1, arg)
          read (arg,*) re_default
          if (re_default == 0d0)     &
            call my_stop("-r option has no valid reynolds numer")
          i = i+2
        end if

      else if (args == "-a") then
        if (i == nargs) then
          call my_stop("Must specify filename of seed airfoil for -a option.")
        else
          call getarg(i+1, arg)
          airfoil_file = trim(arg) 
          i = i+2
        end if

      else if ( args == "-h" .or. args == "--help") then
        call print_usage()
        stop

      else if ( args(1:1) /= "-") then
        input_file = trim(arg) 
        i = i+1

      else
        call print_error ("Unrecognized option "//args//".")
        call print_usage ()
        stop 1
      end if

      if (i > nargs) getting_args = .false.
    end do

  end subroutine get_command_line



  subroutine print_usage()

    !! Prints usage information

    use commons,            only : PGM_NAME

    print * 
    print *, "Usage:  "//PGM_NAME//"  input_file  [OPTIONS]"
    print *
    print *, "Options:"
    print *, "     -i input_file     Name of input file, '-i' can be omitted"
    print *, "     -o output_prefix  Output prefix of optimized airfoil - default: name of input file"
    print *, "     -r xxxxxx         Default reynolds number used for operating points"
    print *, "     -a airfoil_file   Filename of seed airfoil"
    print *, "     -h                Show this text"
    print *
    print *, "Home: https://github.com/jxjo/"//PGM_NAME
    print *, "      (c) 2019-2024 Jochen Guenzel"
    print *

  end subroutine print_usage


end module 

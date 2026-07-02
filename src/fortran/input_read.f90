! MIT License
 
module input_read

  ! Read command line and input file, build main parameter data structures  ... 

  use os_util
  use print_util
  use string_util,            only : stri, strf, to_lower


  implicit none
  private

  public :: read_inputs
  public :: run_mode_from_command_line

  public :: namelist_check
  public :: open_input_file, close_input_file

  public :: read_xfoil_options_inputs, read_operating_conditions_inputs
  public :: read_panel_options_inputs, panel_options_exist
  public :: read_bezier_inputs, read_bspline_inputs, read_flap_worker_inputs, read_curvature_inputs
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
    call read_bspline_inputs          (iunit, shape_spec%bspline)
    call read_hicks_henne_inputs      (iunit, shape_spec%hh)

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



  subroutine read_optimization_options_inputs  (iunit, airfoil_filename, &
                                                shape_spec, optimize_options, show_details, wait_at_end)

    !! Read main namelist 'optimization_options'

    use shape_airfoil,        only : shape_spec_type, HICKS_HENNE, BEZIER, BSPLINE
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
    character(:), allocatable     :: shape
    integer                       :: iostat1
    integer                       :: cpu_threads

    namelist /optimization_options/ airfoil_file, shape_functions, show_details, &
                                    cpu_threads, wait_at_end

    ! defaults for main namelist options

    airfoil_file = ''
    shape_functions = 'bezier'                         ! default shape functions
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

    shape = to_lower (trim(shape_functions))
    
    if      (shape == 'bezier') then
      shape_spec%type = BEZIER
    else if (shape == 'bspline') then
      shape_spec%type = BSPLINE
    else if (shape == 'hicks-henne') then
      shape_spec%type = HICKS_HENNE
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

    use op_point
    use xfoil_driver,         only : flap_spec_type

    use eval_commons,         only : eval_spec_type

    integer, intent(in)                   :: iunit
    double precision, intent(in)          :: re_default_cl            ! re default from command line 
    type(eval_spec_type), intent(out)     :: eval_spec
    type (flap_spec_type), intent(inout)  :: flap_spec

    integer                               :: noppoint
    type(op_point_spec_type), allocatable :: op_point_specs (:)


    ! Op_point specification 
    character(7),     dimension(MAX_NOP)  :: op_mode
    character(15),    dimension(MAX_NOP)  :: optimization_type
    double precision, dimension(MAX_NOP)  :: op_point, weighting
    double precision, dimension(MAX_NOP)  :: ncrit_pt, target_value, reynolds, mach
    double precision, dimension(MAX_NOP)  :: flap_angle
    logical, dimension(MAX_NOP)           :: flap_optimize 

    double precision            :: re_default, flap_angle_default, mach_default 
    logical                     :: re_default_as_resqrtcl
    logical                     :: allow_improved_target
    type(op_point_spec_type)    :: op

    integer               :: i, iostat1, nflap_opt
    double precision      :: x_flap, y_flap
    character(3)          :: y_flap_spec
    logical               :: use_flap

    type (re_type)        :: re_def

    namelist /operating_conditions/ noppoint, op_mode, op_point, reynolds, mach,   &
              target_value, weighting, optimization_type, ncrit_pt,                & 
              re_default_as_resqrtcl, re_default, mach_default, &
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
    y_flap_spec         = 'y/t'
    flap_angle_default  = 0d0 
    flap_angle (:)      = NOT_DEF_D 
    flap_optimize (:)   = .false. 

    allow_improved_target = .true.

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
    allocate (op_point_specs(noppoint)) 

    do i = 1, noppoint

      op_point_specs(i)%spec_cl = (op_mode(i) == 'spec-cl')
      op_point_specs(i)%value   = op_point(i)
      
      op_point_specs(i)%ncrit = ncrit_pt(i)    
      op_point_specs(i)%opt_type = opt_type_enum(optimization_type(i))
      op_point_specs(i)%target_value = target_value (i)
      op_point_specs(i)%allow_improved_target = allow_improved_target
      
      if (reynolds(i) /= NOT_DEF_D) then
        op_point_specs(i)%re%number  = reynolds(i)
        op_point_specs(i)%re%type    = 1
      else                                    ! take default Re number
        op_point_specs(i)%re = re_def 
      end if

      if (mach(i) /= NOT_DEF_D) then 
        op_point_specs(i)%ma%number = mach(i)                ! mach number only Type 1
      else 
        op_point_specs(i)%ma%number = mach_default
      end if 
      op_point_specs(i)%ma%type    = 1

      op_point_specs(i)%weighting_user  = abs(weighting (i)) ! abs - compatibility with old input files, negative values ...

      ! map flap inputs to op point spec 

      if (use_flap) then 
        op_point_specs(i)%flap_optimize = flap_optimize(i) 
        if (flap_optimize(i)) nflap_opt = nflap_opt + 1     ! count no of op points with optimize 
        if (flap_angle(i) == NOT_DEF_D) then
          op_point_specs(i)%flap_angle = flap_angle_default ! take default value if nothing specified 
        else
          op_point_specs(i)%flap_angle = flap_angle(i)      ! set to defined value 
        end if 
      else 
        op_point_specs(i)%flap_optimize = .false. 
        op_point_specs(i)%flap_angle = 0d0                  ! 0 degrees is default 
      end if 
    end do 

    ! Flap settings to data structure

    flap_spec%use_flap    = use_flap
    flap_spec%ndv         = nflap_opt                         ! no of design variables in opti 
    flap_spec%x_flap      = x_flap
    flap_spec%y_flap      = y_flap
    flap_spec%y_flap_spec = y_flap_spec

    ! put sub types into main data structure 

    eval_spec%op_point_specs = op_point_specs


    ! Check input data  ------------------------

    ! if (noppoint < 1) call my_stop("noppoint must be > 0")
    if (noppoint > MAX_NOP) then
      call my_stop("noppoints must be <= "//stri(MAX_NOP)//".")
    end if

    if (use_flap) then 
      if ((x_flap <= 0.0) .or. (x_flap >= 1.0)) &
        call my_stop("x_flap must be > 0.0 and < 1.0")
      if ((y_flap < 0.0) .or. (y_flap > 1.0)) &
        call my_stop("y_flap must be => 0.0 and <= 1.0")
      if ((y_flap_spec  /= 'y/c') .and. (y_flap_spec  /= 'y/t')) &
        call my_stop ("Vertical hinge definition must be 'y/c' or 'y/t'")
    end if 

    do i = 1, noppoint

      op  = op_point_specs(i)

      if (op%re%number <= 0.d0) then
        call my_op_stop (i, "reynolds must be > 0. Default value (re_default) could not be set")
        if (op%ma%number < 0.d0) &
        call my_op_stop (i, "mach must be >= 0.")
      else if (op%re%number >= 1.d8) then 
        call my_stop ("reynolds number must be < 1e8")
      end if 
      
      if (op%ma%number >= 1.d0) &
        call my_op_stop (i, "mach must be < 1.")
      if (op%opt_type == 0) & 
        call my_op_stop (i, "optimization_type must be 'min-drag', 'max-glide', "//     &
                    "'min-sink', 'max-lift', 'max-xtr', 'target-moment', "//    &
                    "'target-drag', 'target-glide' or 'target-cp-min'")
      if ((op%ncrit <= 0.d0) .and. (op%ncrit /= -1d0)) &
        call my_op_stop (i, "ncrit_pt must be > 0 or -1.")

      select case (op%opt_type)
        case (OPT_TARGET_CM)
          if (target_value(i) == NOT_DEF_D) &
            call my_op_stop (i, "No 'target-value' defined for "//  &
                      "for optimization_type 'target-moment'")
        case (OPT_TARGET_CD)
          if (target_value(i) == NOT_DEF_D) &
            call my_op_stop (i, "No 'target-value' defined for "//  &
                        "for optimization_type 'target-drag'")
          if (target_value(i) < 0.d0) &
            call my_op_stop (i, "target-value for 'target-drag' must be >= 0.")
        case (OPT_TARGET_GLIDE)
          if (target_value(i) == NOT_DEF_D) &
            call my_op_stop (i, "No 'target-value' defined for "//  &
                        "for optimization_type 'target-glide'")
            if (target_value(i) < 0.d0) &
              call my_op_stop (i, "target-value for 'target-glide' must be >= 0.")
        case (OPT_TARGET_CL)
          if (target_value(i) == NOT_DEF_D) &
            call my_op_stop (i, "No 'target-value' defined for "//  &
                        "for optimization_type 'target-lift'")
        case (OPT_TARGET_CP_MIN)
          if (target_value(i) == NOT_DEF_D) &
            call my_op_stop (i, "No 'target-value' defined for "//  &
                        "for optimization_type 'target-cp-min'")
          if (target_value(i) >= 0.d0) &
            call my_op_stop (i, "target-value for 'target-cp-min' must be < 0 (suction coefficient)")
      end select

      if (use_flap) then 

        if (flap_angle(i) /= NOT_DEF_D .and. abs(flap_angle(i)) > 70d0) &
          call my_op_stop (i, "Flap angle must be less than 70 degrees")

      end if 


      if (op%value <= 0.d0 .and. op%spec_cl) then
        if ((op%opt_type /= OPT_MIN_CD) .and. &
            (op%opt_type /= OPT_MAX_XTR) .and. &
            (op%opt_type /= OPT_TARGET_CD) .and. &
            (op%opt_type /= OPT_TARGET_CP_MIN)) then
          call my_stop ("Operating point "//stri(i)//" is at Cl = 0. "// &
                        "Cannot use this optimization type in this case.")
        end if

      elseif (op%spec_cl .and. op%opt_type == OPT_MAX_CL) then
        call my_stop ("Cl is specified for operating point "//stri(i)// &
                      ". Cannot use 'max-lift' optimization type in this case.")

      elseif (op%spec_cl .and. op%opt_type == OPT_TARGET_CL) then              
        call my_stop ("op_mode = 'spec_cl' doesn't make sense "// &
                      "for optimization_type 'target-lift'")
      end if

    end do

  end subroutine read_operating_conditions_inputs
  subroutine read_geometry_targets_inputs  (iunit, geo_targets)

    !! Read 'constraints' inputs into derived types

    use geo_target,             only : geo_target_type
    use geo_target,             only : geo_target_type_enum

    integer, intent(in)                :: iunit
    type (geo_target_type), allocatable, intent(inout) :: geo_targets (:)

    integer :: iostat1

    double precision, dimension(MAX_NOP) :: target_value, weighting
    character(30), dimension(MAX_NOP)    :: target_type
    integer :: ngeo_targets, i

    ! deprecated
    logical, dimension(MAX_NOP)          :: preset_to_target 

    namelist /geometry_targets/ ngeo_targets, target_type, &
                                target_value, weighting, preset_to_target

    ! Default values for curvature parameters

    ngeo_targets = 0
    target_type (:) = ''
    target_value(:) = 0.d0 
    weighting(:) = 1d0 

    ! deprecated

    preset_to_target (:) = .false.

    ! Open input file and read namelist from file

    if (iunit > 0) then
      rewind (iunit)
      read (iunit, iostat=iostat1, nml=geometry_targets)
      call namelist_check('geometry_targets', iostat1, 'no-warn')
    end if

    ! Handle deprecated option names 

    if (any(preset_to_target)) then 
      call print_warning ("'preset_to_target' is deprecated and will be ignored",5 )
    end if

    allocate (geo_targets(ngeo_targets)) 
    geo_targets%reference_value   = 0.d0

    geo_targets%weighting = 0d0                         ! will be normalized later

    do i = 1, ngeo_targets
      geo_targets(i)%type           = geo_target_type_enum(target_type(i))
      geo_targets(i)%target_value   = target_value(i)
      geo_targets(i)%weighting_user = abs(weighting(i)) ! abs - compatibility with old input files, negative values ...
    end do   

    ! Geo targets - check options

    do i = 1, ngeo_targets
      if (geo_targets(i)%type == 0) &
      call my_stop("Target_type must be 'camber' or 'thickness'.")
    end do   

  end subroutine 



  subroutine read_hicks_henne_inputs  (iunit, hh)

    !! read input file for hicks henne shape options 

    use shape_hicks_henne,    only : shape_hh_type
    use shape_hicks_henne,    only : nfunctions_to_ndv

    integer, intent(in)                   :: iunit
    type(shape_hh_type), intent(out)      :: hh

    double precision    :: initial_perturb                
    integer             :: nfunctions_top, nfunctions_bot
    integer             :: iostat1


    namelist /hicks_henne_options/ nfunctions_top, nfunctions_bot, &
                                  initial_perturb  

    ! Init default values 

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
    hh%initial_perturb  = initial_perturb

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
    ncp_top = 6                              ! number of control points - top 
    ncp_bot = 6                              ! number of control points - bot 
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
    bezier%initial_perturb = initial_perturb

  end subroutine read_bezier_inputs



  subroutine read_bspline_inputs  (iunit, bspline)

    !! read input file for bspline shape options 
 
    use shape_bspline,        only : shape_bspline_type
    use shape_bspline,        only : ncp_to_ndv

    integer, intent(in)                   :: iunit
    type(shape_bspline_type), intent(out) :: bspline

    double precision    :: initial_perturb                
    integer     :: ncp_top, ncp_bot
    integer     :: iostat1

    namelist /bspline_options/ ncp_top, ncp_bot, initial_perturb

    ! Init default values 
    ncp_top = 7                              ! number of control points - top 
    ncp_bot = 7                              ! number of control points - bot 
    initial_perturb = 0.1d0                  ! good value - about 10% of dv solution space

    if (iunit > 0) then
      rewind (iunit)
      read (iunit, iostat=iostat1, nml=bspline_options)
      call namelist_check('bspline_options', iostat1, 'no-warn')
    end if

    if (ncp_top < 5 .or. ncp_top > 12) &
      call my_stop("Number of B-spline control points must be >= 5 and <= 12")
    if (ncp_bot < 5 .or. ncp_bot > 12) &
      call my_stop("Number of B-spline control points must be >= 5 and <= 12")
    if (initial_perturb < 0.01d0 .or. initial_perturb > 0.5d0) &
      call my_stop("B-spline: initial_perturb must be >= 0.01 and <= 0.5")

    bspline%ncp_top = ncp_top
    bspline%ncp_bot = ncp_bot
    bspline%initial_perturb = initial_perturb

  end subroutine read_bspline_inputs



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



  subroutine my_op_stop (iop,  message)

    !! Stops execution when there is an invalid op_point parameter

    use op_point,           only : op_point_spec_type

    integer, intent (in)      :: iop
    character(*), intent(in)  :: message

    call my_stop ('Op_point '// stri(iop) //': '// message)

  end subroutine my_op_stop



  subroutine read_curvature_inputs  (iunit, curv_constraints)

    !! Read 'curvature' inputs into derived types

    use eval_commons,           only : curv_constraints_type, curv_side_constraints_type

    integer, intent(in)                :: iunit
    type (curv_constraints_type),  intent(inout) :: curv_constraints

    type (curv_side_constraints_type) :: spec

    integer :: iostat1
    integer :: max_curv_reverse_top, max_curv_reverse_bot
    logical :: check_curvature, auto_curvature
    logical :: check_curvature_bumps, check_le_curvature
    double precision  :: max_te_curvature
    double precision  :: curv_threshold, bump_threshold

    ! #depricated
    ! integer :: max_spikes_top, max_spikes_bot

    namelist /curvature/  check_curvature, auto_curvature, &
                          curv_threshold, bump_threshold, &
                          max_te_curvature, &
                          max_curv_reverse_top, max_curv_reverse_bot, &
                          check_curvature_bumps, check_le_curvature

    ! Default values for curvature parameters
                                                
    check_curvature       = .true.
    auto_curvature        = .true.
    check_curvature_bumps = check_curvature
    check_le_curvature    = check_curvature
    max_curv_reverse_top  = 0
    max_curv_reverse_bot  = 0
    max_te_curvature      = 0.1d0          
    curv_threshold        = 0.01d0
    bump_threshold        = 0.01d0

    ! Open input file and read namelist from file

    if (iunit > 0) then
      rewind (iunit)
      read (iunit, iostat=iostat1, nml=curvature)
      call namelist_check('curvature', iostat1, 'no-warn')
    end if

    curv_constraints%check_curvature       = check_curvature
    curv_constraints%auto_curvature        = auto_curvature .and. check_curvature

    spec%check_curvature_bumps  = check_curvature_bumps .and. check_curvature
    spec%check_le_curvature     = check_le_curvature    .and. check_curvature
    spec%max_te_curvature       = max_te_curvature
    spec%curv_threshold         = curv_threshold

    curv_constraints%top = spec
    curv_constraints%top%max_curv_reverse = max_curv_reverse_top

    curv_constraints%bot = spec
    curv_constraints%bot%max_curv_reverse = max_curv_reverse_bot

    if (max_curv_reverse_top < 0 .or. max_curv_reverse_top > 2) &
        call my_stop("max_curv_reverse_top must be 0,1 or 2.")
    if (max_curv_reverse_bot < 0 .or. max_curv_reverse_bot > 2) &
        call my_stop("max_curv_reverse_bot must be 0,1 or 2.")
    if (max_te_curvature < 0.d0 .or. max_te_curvature > 10.d0) &
        call my_stop("max_te_curvature must be between 0 and 10.")
    if (curv_threshold < 0.01d0 .or. curv_threshold > 1.d0) &
        call my_stop("curv_threshold must be between 0.01 and 1.")

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
    double precision    :: min_thickness, max_thickness, min_te_angle, min_te_top_angle, max_te_bot_angle
    double precision    :: min_camber, max_camber
    double precision    :: min_flap_angle, max_flap_angle
    double precision    :: min_thickness_at_x(2)

    namelist /constraints/  min_thickness, max_thickness, min_camber, max_camber, &
                            min_te_angle, min_te_top_angle, max_te_bot_angle, &
                            check_geometry, symmetrical, &
                            min_flap_angle, max_flap_angle, &
                            min_thickness_at_x

    ! Default values for curvature parameters

    check_geometry = .true.

    min_thickness       = NOT_DEF_D
    min_thickness_at_x  = NOT_DEF_D
    max_thickness       = NOT_DEF_D
    min_camber          = NOT_DEF_D
    max_camber          = NOT_DEF_D

    min_te_angle        = NOT_DEF_D
    min_te_top_angle    = NOT_DEF_D
    max_te_bot_angle    = NOT_DEF_D

    symmetrical         = .false.
    min_flap_angle      = -5.d0
    max_flap_angle      = 15.d0
    
    ! Open input file and read namelist from file

    if (iunit > 0) then
      rewind (iunit)
      read (iunit, iostat=iostat1, nml=constraints)
      call namelist_check('constraints', iostat1, 'no-warn')
    end if

    geo_constraints%check_geometry        = check_geometry
    geo_constraints%symmetrical           = symmetrical
    geo_constraints%min_thickness         = min_thickness
    geo_constraints%max_thickness         = max_thickness
    geo_constraints%min_camber            = min_camber
    geo_constraints%max_camber            = max_camber
    geo_constraints%min_te_angle          = min_te_angle
    geo_constraints%min_te_top_angle      = min_te_top_angle
    geo_constraints%max_te_bot_angle      = max_te_bot_angle
    geo_constraints%min_thickness_at_x%x  = min_thickness_at_x(1)
    geo_constraints%min_thickness_at_x%y  = min_thickness_at_x(2)

    flap_spec%min_flap_angle              = min_flap_angle
    flap_spec%max_flap_angle              = max_flap_angle


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

    if (min_te_angle /= NOT_DEF_D .and. min_te_angle < 0.d0) &
        call my_stop("min_te_angle must be >= 0.")
    if (min_te_top_angle /= NOT_DEF_D .and. min_te_top_angle < 0.d0) &
      call my_stop("min_te_top_angle must be >= 0.")
    if (max_te_bot_angle /= NOT_DEF_D .and. (max_te_bot_angle <= -90.d0 .or. max_te_bot_angle >= 90.d0)) &
      call my_stop("max_te_bot_angle must be > -90 and < 90.")

    if ((min_thickness_at_x(1) == NOT_DEF_D) .neqv. (min_thickness_at_x(2) == NOT_DEF_D)) &
      call my_stop("min_thickness_at_x must be specified as x, thickness.")

    if (min_thickness_at_x(1) /= NOT_DEF_D .and. (min_thickness_at_x(1) < 0.d0 .or. min_thickness_at_x(1) > 1.d0)) &
      call my_stop("min_thickness_at_x: x must be >= 0 and <= 1.")
    if (min_thickness_at_x(2) /= NOT_DEF_D .and. min_thickness_at_x(2) <= 0.d0) &
      call my_stop("min_thickness_at_x: thickness must be > 0.")

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

    namelist /paneling_options/ npoint, le_bunch, te_bunch, npan

    ! Init default values for xfoil options

    npoint   = 161          ! a real default
    npan     = 0            ! alternate input instead npoint 
    le_bunch = 0.86d0       ! for 161 a little higher than 0.82
    te_bunch = 0.6d0

    ! Open input file and read namelist from file

    if (iunit > 0) then
      rewind (iunit)
      read (iunit, iostat=iostat1, nml=paneling_options)
      call namelist_check('paneling_options', iostat1, 'no-warn')
    end if
    
    ! user choosed npan 

    if (npan > 0) then
      npoint = npan + 1
    end if

    ! Put xfoil options into derived types

    if (npoint < 80) call my_stop("npoint must be >= 80.")
    if (npoint > 350) call my_stop("npoint must be <= 350.")
    if (le_bunch < 0.d0 .or. le_bunch > 1d0) &
        call my_stop("le_bunch must be => 0 and <= 1.0.")
    if (te_bunch < 0.d0 .or. te_bunch > 1d0) &
        call my_stop("te_bunch must be => 0 and <= 1.0.")

    panel_options%npoint   = npoint
    panel_options%le_bunch = le_bunch
    panel_options%te_bunch = te_bunch

  end subroutine 



  function panel_options_exist  (iunit) result (exist)

    !! returns .true. if  namelist panel_option are in file 

    integer, intent(in)       :: iunit
    double precision  :: te_bunch, le_bunch
    integer           :: npoint, npan
    integer           :: iostat1
    logical           :: exist 

    namelist /paneling_options/ npoint, le_bunch, te_bunch, npan

    ! Open input file and read namelist from file

    if (iunit > 0) then
      rewind (iunit)
      read (iunit, iostat=iostat1, nml=paneling_options)
    end if
    if (iostat1 == 0) then
      exist = .true. 
    else 
      exist = .false. 
    end if 
    rewind (iunit)

  end function 




  subroutine read_particle_swarm_options_inputs  (iunit, pso_options)

    !! Read 'particle_swarm_options' and 'initialization' input options 
    !! into pso_options 

    use particle_swarm, only : pso_options_type

    integer, intent(in)           :: iunit
    type(pso_options_type), intent(out) :: pso_options

    integer           :: pop, max_iterations, max_retries
    double precision  :: min_radius, max_speed
    integer           :: iostat1
    character(20)     :: convergence_profile
   
    namelist /particle_swarm_options/ pop, min_radius, max_iterations, max_speed,    &
                                      max_retries, convergence_profile

    ! PSO default options
    
    convergence_profile = "exhaustive"
    pop = 30
    min_radius = 0.001d0
    max_iterations = 500
    max_speed = 0.1                       ! good value - about 10% of dv solution space
    max_retries = 3                       ! max. retries of particle 
                           
                            
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

    ! Input checks 
    
    if (max_speed > 0.7 .or. max_speed < 0.01) &
      call my_stop ("max_speed should be between 0.01 and 0.7")
    if (pop < 1) call my_stop("pop must be > 0.")
    if (min_radius <= 0.d0) call my_stop("min_radius must be > 0.")
    if (max_iterations < 1) call my_stop("max_iterations must be > 0.")  
    if ( (trim(convergence_profile) /= "quick") .and.                    &
         (trim(convergence_profile) /= "exhaustive")) &
      call my_stop("convergence_profile must be 'exhaustive' "//&
                    "or 'quick'.")

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

    logical :: viscous_mode, silent_mode, fix_unconverged, reinitialize, show_details, detect_bubble
    integer :: bl_maxit
    double precision :: ncrit, xtript, xtripb, vaccel
    integer :: iostat1

    namelist /xfoil_run_options/ ncrit, xtript, xtripb, viscous_mode,            &
                                 silent_mode, bl_maxit, vaccel, fix_unconverged, reinitialize, & 
                                 show_details, detect_bubble

    ! Set default xfoil aerodynamics

    ncrit = 9.d0
    xtript = 1.d0 
    xtripb = 1.d0
    viscous_mode = .true.
    silent_mode = .true.
    bl_maxit = 40                   ! reduced to 40 as above the potential result is rarely usable..
    vaccel = 0.005d0                ! the original value of 0.01 leads to too many non convergences at 
                                    !   higher lift --> reduced 
    fix_unconverged = .true.
    reinitialize = .false.          ! as run_xfoil is improved, this will speed up the xfoil calcs
    show_details = .false.          ! show success info during op point calculation
    detect_bubble = .false.         ! detect bubbles in the boundary layer

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
    xfoil_options%repair_polar_outlier = .false.
    xfoil_options%reinitialize = reinitialize 
    xfoil_options%show_details = show_details
    xfoil_options%detect_bubble = detect_bubble

  end subroutine read_xfoil_options_inputs



  subroutine read_polar_inputs  (iunit, re_default, generate_polar, &
                                 auto_range, spec_cl, op_point_range, type_of_polar, &
                                 polar_reynolds, polar_mach)

    !----------------------------------------------------------------------------
    !! Read input file to get polar definition 
    !----------------------------------------------------------------------------

    use op_point,           only : re_type

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
    type_of_polar   = NOT_DEF_I
    op_mode         = 'spec-al'
    op_point_range  = NOT_DEF_D
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

    ! default type T1 

    if (type_of_polar == NOT_DEF_I) then 
      type_of_polar     = re_default%type
    end if 

    ! default values for range 

    if (type_of_polar == 1 .and. op_mode == 'spec-al') then  
      if (op_point_range(1) == NOT_DEF_D) op_point_range(1) =  3d0
      if (op_point_range(2) == NOT_DEF_D) op_point_range(2) = 12d0
      if (op_point_range(3) == NOT_DEF_D) op_point_range(2) = 0.2d0
    else if (type_of_polar == 2 .and. op_mode == 'spec-cl') then  
      if (op_point_range(1) == NOT_DEF_D) op_point_range(1) = 0.05d0
      if (op_point_range(2) == NOT_DEF_D) op_point_range(2) = 1.2d0
      if (op_point_range(3) == NOT_DEF_D) op_point_range(2) = 0.02d0
    end if 

    if (auto_range .and. type_of_polar == 1) then 
      if (op_point_range(3) == NOT_DEF_D) op_point_range(3) = 0.2d0
    else
      if (op_point_range(3) == NOT_DEF_D) op_point_range(3) = 0.02d0
    end if 

    ! if there are no re numbers in input file take default

    if (polar_reynolds(1) == 0d0) then 
      polar_reynolds(1) = re_default%number 
    end if

    ! Input sanity

    if (type_of_polar /= 1 .and. type_of_polar /= 2) & 
      call my_stop ("polar_generation: Type of polars must be either '1' or '2'")

    if (.not. auto_range) then 
      if (op_mode /= 'spec-al' .and. op_mode /= 'spec-cl') &
        call my_stop ("polar_generation: op_mode must be 'spec-cl' or 'spec-al'")
      if (op_point_range(1) == NOT_DEF_D) &
        call my_stop ("polar_generation: Range 'from' not defined")
      if (op_point_range(2) == NOT_DEF_D) &
        call my_stop ("polar_generation: Range 'to' not defined")
      if ((op_point_range(2) - op_point_range(1)) <= 0d0 ) & 
        call my_stop ("polar_generation: End of polar op_point_range must be higher than the start.")
      if (( op_point_range(1) + op_point_range(3)) >= op_point_range(2) ) & 
        call my_stop ("polar_generation: Start of polar op_point_range + increment should be end of op_point_range.")
    end if 

    if (op_point_range(3) == NOT_DEF_D) &
      call my_stop ("polar_generation: 'increment' within range not defined")

    npolars = 0
    do i = 1, size(polar_reynolds)
      if (polar_reynolds(i) > 0d0) then   
        if (polar_reynolds(i) < 1000d0) then   
          call my_stop ("polar_generation: reynolds number must be >= 1000")
        else if (polar_mach(i) < 0d0) then 
          call my_stop ("polar_generation: mach number must be >= 0.0")
        else if (polar_mach(i) >= 1d0) then 
          call my_stop ("polar_generation: mach number must be <= 1.0")
        else if (polar_reynolds(i) >= 1.d8) then 
          call my_stop ("polar_generation: reynolds number must be < 1e8")
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

      else if (args == "-m") then
        i = i+2                                     ! skip -m (mode) here ( -> separate function) 

      else if ( args == "-h" .or. args == "--help") then
        call print_usage()
        stop

      else if ( args(1:1) /= "-") then
        input_file = trim(arg) 
        i = i+1

      else
        call print_usage ()
        call my_stop ("Unrecognized option "//args//".")
      end if

      if (i > nargs) getting_args = .false.
    end do

  end subroutine


  
  function run_mode_from_command_line () result (run_mode)

    !! returns  rund_mode from command line  

    use commons,      only : MODE_CHILD_PROCESS, MODE_NORMAL

    character(250)            :: arg
    character (:),allocatable :: args
    integer                   :: i, nargs, run_mode
    logical                   :: getting_args

    run_mode = MODE_NORMAL

    nargs = iargc()
    if (nargs > 0) then
      getting_args = .true.
    else
      return
    end if

    i = 1
    do while (getting_args)

      call get_command_argument (i, arg) 
      args = trim(arg)

      if (args == "-m") then
        if (i == nargs) then
          call print_error ("Missing argument for -m paramter")
        else
          call getarg(i+1, arg)
          arg = trim(arg) 

          if (arg == "child") then 
            run_mode = MODE_CHILD_PROCESS
          end if 
        end if
        exit
      else
        i = i + 1
      end if  

      if (i > nargs) getting_args = .false.
    end do

  end function



  subroutine print_usage()

    !! Prints usage information

    use commons,            only : PGM_NAME

    print * 
    print *, "Usage:  xoptfoil2  input_file  [OPTIONS]"
    print *
    print *, "Options:"
    print *, "     -i input_file     Name of input file, '-i' can be omitted"
    print *, "     -o output_prefix  Output prefix of optimized airfoil - default: name of input file"
    print *, "     -r xxxxxx         Default reynolds number used for operating points"
    print *, "     -a airfoil_file   Filename of seed airfoil"
    print *, "     -h                Show this text"
    print *
    print *, "Home: https://github.com/jxjo/"//PGM_NAME
    print *, "      (c) 2019-2026 Jochen Guenzel"
    print *

  end subroutine print_usage


end module 

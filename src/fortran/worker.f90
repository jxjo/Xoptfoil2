! MIT License

module worker_functions

  !   Utility Functions based on xfoil to complement Xoptfoil2
  !   
  !   The Worker uses an Xoptfoil input-file to get the paramters.
  !     only a few sections are needed
  !   


  use os_util
  use print_util
  use xoptfoil_version,       only : XOPTFOIL_VERSION_TEXT
  use string_util,            only : stri, strf
  use commons,                only : show_details
  use airfoil_base,           only : airfoil_type, panel_options_type
  use airfoil_base,           only : is_bezier_based, is_bspline_based, is_hh_based

  implicit none

  character (*), parameter    :: PGM_NAME = 'Worker'
  

contains

  subroutine match_bezier (input_file, outname_auto, output_prefix, seed_foil)

    !-------------------------------------------------------------------------
    !! adapt bezier curves to top and bot side and generate new airfoil 
    !-------------------------------------------------------------------------

    use airfoil_base,         only : airfoil_write_with_shapes, is_normalized_coord, repanel
    use airfoil_preparation,  only : as_bezier_based, determine_auto_curvature

    use input_read,           only : read_bezier_inputs, read_panel_options_inputs
    use input_read,           only : open_input_file, close_input_file, read_curvature_inputs

    use shape_bezier,         only : shape_bezier_type
    use eval_commons,         only : curv_constraints_type

    character(*), intent(in)        :: input_file, output_prefix
    type (airfoil_type), intent (inout)  :: seed_foil
    logical, intent(in)             :: outname_auto

    type (airfoil_type)             :: foil
    type (shape_bezier_type)        :: shape_bezier
    type (panel_options_type)       :: panel_options
    type (curv_constraints_type)    :: curv_constraints
    integer                         :: iunit

    write (*,*) 'Match bezier curves for the top and bot side'

    ! Read inputs file to get options needed 

    call open_input_file           (input_file, iunit, optionally=.true.)
    call read_panel_options_inputs (iunit, panel_options)
    call read_bezier_inputs        (iunit, shape_bezier)
    call read_curvature_inputs     (iunit, curv_constraints)
    call close_input_file          (iunit)

    write (*,*) 
    show_details = .true.

    ! Auto curvature constraints based on seed

    if (curv_constraints%auto_curvature) &
      call determine_auto_curvature (seed_foil, curv_constraints)

    ! simplex optimization for both side  

    foil = as_bezier_based (seed_foil, shape_bezier, panel_options, curv_constraints)

    if (.not. outname_auto) then 
      foil%name     = output_prefix
      foil%filename = ensure_filename_extension (output_prefix, '.dat')
    end if

    call airfoil_write_with_shapes (foil, "")             

  end subroutine match_bezier



  subroutine match_bspline (input_file, outname_auto, output_prefix, seed_foil)

    !-------------------------------------------------------------------------
    !! adapt bspline curves to top and bot side and generate new airfoil 
    !-------------------------------------------------------------------------

    use airfoil_base,         only : airfoil_write_with_shapes, is_normalized_coord, repanel
    use airfoil_preparation,  only : as_bspline_based, determine_auto_curvature

    use input_read,           only : read_bspline_inputs, read_panel_options_inputs
    use input_read,           only : open_input_file, close_input_file, read_curvature_inputs

    use shape_bspline,        only : shape_bspline_type
    use eval_commons,         only : curv_constraints_type

    character(*), intent(in)        :: input_file, output_prefix
    type (airfoil_type), intent (inout)  :: seed_foil
    logical, intent(in)             :: outname_auto

    type (airfoil_type)             :: foil
    type (shape_bspline_type)       :: shape_bspline
    type (panel_options_type)       :: panel_options
    type (curv_constraints_type)    :: curv_constraints
    integer                         :: iunit

    write (*,*) 'Match bspline curves for the top and bot side'

    ! Read inputs file to get options needed 

    call open_input_file           (input_file, iunit, optionally=.true.)
    call read_panel_options_inputs (iunit, panel_options)
    call read_bspline_inputs       (iunit, shape_bspline)
    call read_curvature_inputs     (iunit, curv_constraints)
    call close_input_file          (iunit)

    write (*,*) 
    show_details = .true.

    ! Auto curvature constraints based on seed

    if (curv_constraints%auto_curvature) &
      call determine_auto_curvature (seed_foil, curv_constraints)

    ! simplex optimization for both side  

    foil = as_bspline_based (seed_foil, shape_bspline, panel_options, curv_constraints)

    if (.not. outname_auto) then 
      foil%name     = output_prefix
      foil%filename = ensure_filename_extension (output_prefix, '.dat')
    end if

    call airfoil_write_with_shapes (foil, "")             

  end subroutine match_bspline


  subroutine generate_polars (input_file, output_prefix, re_default_cl, foil)

    !-------------------------------------------------------------------------
    !! Generate flapped polars - same as generate_polars with all polar files 
    !!  in one directory also the flapped ones ... 
    ! - read input file for namelist &polar_generation
    ! - get polar definitions
    ! - get flap definitions - optional 
    ! - calculate polars for each flap setting 
    ! - write each polar to a file 
    !-------------------------------------------------------------------------

    use xfoil_driver,       only : xfoil_options_type
    use xfoil_driver,       only : flap_spec_type
    use op_point,           only : re_type

    use input_read,         only : open_input_file, close_input_file
    use input_read,         only : read_xfoil_options_inputs
    use input_read,         only : read_panel_options_inputs, read_polar_inputs
    use input_read,         only : panel_options_exist
    use input_read,         only : read_flap_worker_inputs

    use airfoil_base,       only : repanel

    use polar_operations,   only : initialize_polars, generate_polar_set
    use polar_operations,   only : polar_type
  
    character(*), intent(in)        :: input_file
    double precision, intent(in)    :: re_default_cl 
    type (airfoil_type), intent (in)  :: foil
    character(*), intent(in)        :: output_prefix

    type (airfoil_type)            :: seed_foil
    type (panel_options_type)      :: panel_options
    type (xfoil_options_type)      :: xfoil_options
    type (flap_spec_type)          :: flap_spec
    type (re_type)                 :: re_default
    type (polar_type), allocatable :: polars (:) 
    double precision, allocatable  :: flap_angle (:), polar_reynolds (:), polar_mach(:)
    integer                        :: iunit, type_of_polar
    logical                        :: spec_cl, generate_polar, auto_range, splitted, do_repanel
    double precision               :: op_point_range (3)

    ! read optional ncrit 

    call open_input_file (input_file, iunit, optionally=.true.)
    call read_xfoil_options_inputs  (iunit, xfoil_options)
    call close_input_file (iunit)

    ! read optional panel options 

    call open_input_file (input_file, iunit, optionally=.true.)
    do_repanel = panel_options_exist (iunit)
    if (do_repanel) call read_panel_options_inputs  (iunit, panel_options)
    call close_input_file (iunit)

    ! Read and for polar generation 

    re_default%number = re_default_cl
    re_default%type   = 1

    call open_input_file   (input_file, iunit)
    call read_polar_inputs (iunit, re_default, generate_polar, &
                            auto_range, spec_cl, op_point_range, type_of_polar, &
                            polar_reynolds, polar_mach)

    ! read optional flap settings and paneling info 

    call read_flap_worker_inputs (iunit, flap_spec, flap_angle)        ! csv supports flaps
    call close_input_file (iunit)

    if (.not. generate_polar) &
      call my_stop ("Polar generation is switched off")

    ! initialize polar definition structure 

    call initialize_polars (auto_range, spec_cl, op_point_range, type_of_polar, xfoil_options, &
                            polar_reynolds, polar_mach, flap_spec, flap_angle, polars, splitted)

    if (size(polars) > 0) then

      ! handle repaneling 
      if (do_repanel) then 
        seed_foil = repanel(foil, panel_options)
      else 
        seed_foil = foil 
      end if 

      print *
      print *

      ! Generate polars  

      call generate_polar_set (auto_range, output_prefix, seed_foil, xfoil_options, polars, splitted)

    end if

  end subroutine 


  subroutine set_geometry_value (input_file, outname_auto, output_prefix, seed_foil, &
                                value_argument)

    !-------------------------------------------------------------------------
    !! Setting thickness of foil
    !-------------------------------------------------------------------------

    use airfoil_base,       only : airfoil_write_with_shapes, is_normalized_coord
    use airfoil_base,       only : normalize
    use airfoil_geometry,   only : set_geometry, set_te_gap
    use input_read,         only : read_panel_options_inputs
    use input_read,         only : open_input_file, close_input_file
    
    character(*), intent(in)     :: output_prefix, value_argument, input_file
    type (airfoil_type), intent (inout) :: seed_foil
    logical, intent(in)          :: outname_auto

    type (airfoil_type)         :: foil
    type (panel_options_type)   :: panel_options
    character (:), allocatable  :: value_str, value_type
    double precision            :: value_number, value_percent 
    integer                     :: iunit, ierr

    print *,'Max thickness, camber or trailing edge gap ' 
    print *

    call open_input_file (input_file, iunit, optionally=.true. )
    call read_panel_options_inputs (iunit, panel_options)
    call close_input_file (iunit)

    value_type = trim(value_argument (1:(index (value_argument,'=') - 1)))
    value_str  = trim(value_argument ((index (value_argument,'=') + 1):))

    read (value_str ,*, iostat = ierr) value_percent
    value_number = value_percent / 100d0 

    if(ierr /= 0) & 
      call my_stop ("Wrong argument format '"//trim(value_argument)//"' in set command") 

    foil = seed_foil

    ! normalize if foil isn't 

    if (.not. is_normalized_coord(foil)) then
      call normalize (foil) 
      call print_action ("Normalizing airfoil")
    end if 

    select case (value_type)

      case ('t') 
        call print_action ('Setting thickness to', value_str//'%')
        call set_geometry (foil, maxt=value_number)

      case ('xt') 
        call print_action ('Setting max thickness position to', value_str//'%')
        call set_geometry (foil,xmaxt=value_number)

      case ('c') 
        call print_action ('Setting camber to', value_str//'%')
        call set_geometry (foil,maxc=value_number)

      case ('xc') 
        call print_action ('Setting max camber position to', value_str//'%')
        call set_geometry (foil,xmaxc=value_number)

      case ('te') 
        call print_action ('Setting trailing edge gap to', value_str//'%')
        call set_te_gap (foil, value_number)

      case default
        call my_stop ('Unknown type for setting geometry')

    end select

    if (outname_auto) then 
      foil%filename = ensure_filename_extension (output_prefix // '_' // value_type // "=" //value_str, '.dat')
      foil%name     = foil%filename
    else
      foil%name     = output_prefix
    end if

    call airfoil_write_with_shapes (foil, "")

  end subroutine set_geometry_value



  subroutine check_input_file (input_file)

   !-------------------------------------------------------------------------
   !! Checks xoptfoil2 input file for errors
   !-------------------------------------------------------------------------

    use commons,              only : output_prefix
    use input_read,           only : read_inputs
    use optimization,         only : optimize_spec_type
    use input_sanity,         only : check_and_process_inputs
    use shape_airfoil,        only : shape_spec
    use eval_commons,         only : eval_spec_type

    character(*), intent(in)    :: input_file

    character (:), allocatable  :: airfoil_filename
    double precision            :: re_default_cl 

    type(optimize_spec_type)    :: optimize_options
    type(eval_spec_type)        :: eval_spec
    logical                     :: wait_at_end

    write (*,*) 
    airfoil_filename = ''
    re_default_cl = 0d0
 
    call read_inputs (input_file, airfoil_filename, output_prefix, show_details, wait_at_end, &
                      eval_spec, shape_spec, optimize_options) 
    write (*,*) 
 
    call check_and_process_inputs (eval_spec, shape_spec)

    write(*,*)
    call print_colored (COLOR_GOOD, '- Input file seems OK')
    write(*,*)

  end subroutine



  subroutine repanel_normalize (input_file, outname_auto, output_prefix, seed_foil)

    !-------------------------------------------------------------------------
    !! Repanels and foil based on settings in 'input file'
    !-------------------------------------------------------------------------

    use eval_commons,         only : curv_constraints_type
    use airfoil_base,         only : airfoil_write_with_shapes, repanel

    use input_read,           only : read_curvature_inputs
    use input_read,           only : read_panel_options_inputs
    use input_read,           only : open_input_file, close_input_file

    character(*), intent(in)            :: input_file, output_prefix
    type (airfoil_type), intent (inout) :: seed_foil
    logical, intent(in)                 :: outname_auto

    type (airfoil_type)             :: foil
    type (panel_options_type)       :: panel_options
    type (curv_constraints_type)    :: curv_constraints
    integer                         :: iunit

    print *, 'Repanel and normalize the airfoil'

    ! Read inputs file to get options needed 

    call open_input_file (input_file, iunit, optionally=.true.)

    call read_panel_options_inputs (iunit, panel_options)
    call read_curvature_inputs (iunit, curv_constraints)

    call close_input_file (iunit)

    ! Prepare airfoil  - Repanel and split 

    print *
    foil = repanel (seed_foil, panel_options)


    if (.not. outname_auto) then 
      foil%filename = ensure_filename_extension (output_prefix, '.dat')
      foil%name     = output_prefix
    end if

    call airfoil_write_with_shapes (foil, "")

  end subroutine repanel_normalize



  subroutine blend_foils (input_file, outname_auto, output_prefix, seed_foil_in, blend_foil_in, value_argument)

    !-------------------------------------------------------------------------
    !! Blend to seed_foil a blend_foil by (value) % 
    !-------------------------------------------------------------------------

    use math_util,          only : interp_vector
    use airfoil_base,       only : build_from_sides, airfoil_write_with_shapes, split_foil_into_sides, repanel
    use airfoil_base,       only : is_normalized
    use input_read,         only : read_panel_options_inputs
    use input_read,         only : open_input_file, close_input_file

    character(*), intent(in)          :: input_file, output_prefix, value_argument
    type (airfoil_type), intent (inout)  :: seed_foil_in, blend_foil_in
    logical, intent(in)               :: outname_auto

    double precision, dimension(:), allocatable :: xt, xb, yt, yb, bxt, bxb, byt, byb
    double precision, dimension(:), allocatable :: yttmp, ybtmp, yt_blended, yb_blended
    type (airfoil_type)     :: blended_foil, in_foil, blend_foil
    type (panel_options_type) :: panel_options
    integer                 :: pointst, pointsb, ierr, iunit
    double precision        :: blend_factor

    write (*,*) 'The coordinates of two airfoils'

    read (value_argument ,*, iostat = ierr) blend_factor  

    if(ierr /= 0) & 
      call my_stop ("Wrong blend value format '"//trim(value_argument)//"' in blend command") 

    ! Argument could be in % or as a fraction 
    
    if (blend_factor > 1) blend_factor = blend_factor / 100d0
    if (blend_factor <0 .or. blend_factor > 1) & 
      call my_stop ("Blend value must be between 0 and 1.0 ( or 0 and 100)") 


    ! Read inputs file to get paneling options  

    call open_input_file (input_file, iunit, optionally=.true.)
    call read_panel_options_inputs (iunit, panel_options)
    call close_input_file (iunit)


    ! Prepare - Repanel both airfoils? 

    write (*,*) 

    if (is_normalized (seed_foil_in)) then 
      in_foil = seed_foil_in
      call split_foil_into_sides (in_foil)
      call print_text ('- Airfoil '//in_foil%filename //' is already normalized with '//& 
                            stri(size(in_foil%x)) //' points')
    else
      in_foil = repanel (seed_foil_in, panel_options)
    end if 

    if (is_normalized (blend_foil_in)) then 
      blend_foil = blend_foil_in
      call split_foil_into_sides (blend_foil)
      call print_text ('- Airfoil '//blend_foil%filename //' is already normalized with '//& 
                            stri(size(blend_foil%x)) //' points')
    else
      blend_foil = repanel (blend_foil_in, panel_options)
    end if 

    ! Now split  in upper & lower side 

    xt = in_foil%top%x
    xb = in_foil%bot%x
    yt = in_foil%top%y
    yb = in_foil%bot%y

    bxt = blend_foil%top%x
    bxb = blend_foil%bot%x
    byt = blend_foil%top%y
    byb = blend_foil%bot%y

    ! Interpolate x-vals of blend_foil to match to seed airfoil points to x-vals 
    !    - so the z-values can later be blended

    pointst = size(xt,1)
    pointsb = size(xb,1)
    allocate(yttmp(pointst))
    allocate(ybtmp(pointsb))
    call interp_vector(bxt, byt, xt, yttmp)
    call interp_vector(bxb, byb, xb, ybtmp)

    ! now blend the z-values of the two poylines to become the new one

    call print_action ('Blending '//seed_foil_in%filename//' and '//&
            blend_foil_in%filename//' with '//stri(int(blend_factor * 100))//'%')
  
    yt_blended = (1d0 - blend_factor) * yt + blend_factor * yttmp
    yb_blended = (1d0 - blend_factor) * yb + blend_factor * ybtmp

    ! and build new foil 

    blended_foil%top%x = xt
    blended_foil%top%y = yt_blended
    blended_foil%bot%x = xb
    blended_foil%bot%y = yb_blended
    
    call build_from_sides (blended_foil)

    ! Write airfoil 

    if (outname_auto) then 
      blended_foil%name     = output_prefix // '-blend'
      blended_foil%filename = ensure_filename_extension (output_prefix // '-blend', '.dat')
    else
      blended_foil%name     = output_prefix
      blended_foil%filename = ensure_filename_extension (output_prefix, '.dat')
    end if

    call airfoil_write_with_shapes (blended_foil, "")

  end subroutine blend_foils



  subroutine set_flap (input_file, outname_auto, output_prefix, seed_foil)

    !-------------------------------------------------------------------------
    !! Repanels and set flaps of foil based on settings in 'input file'
    !-------------------------------------------------------------------------

    use airfoil_base,       only : is_normalized_coord, normalize
    use input_read,         only : read_flap_worker_inputs
    use input_read,         only : read_panel_options_inputs
    use input_read,         only : open_input_file, close_input_file
    use xfoil_driver,       only : flap_spec_type
    use eval_out,           only : write_airfoil_flapped

    character(*), intent(in)          :: input_file, output_prefix
    type (airfoil_type), intent (inout)  :: seed_foil
    logical, intent(in)               :: outname_auto

    type (airfoil_type)       :: foil
    type (flap_spec_type)     :: flap_spec
    type (panel_options_type) :: panel_options

    double precision, allocatable :: flap_angles (:)
    integer             :: iunit

    ! Read inputs file to get xfoil paneling options  

    call open_input_file (input_file, iunit, optionally=.true.)
    call read_panel_options_inputs (iunit, panel_options)
    call close_input_file (iunit)

    call open_input_file (input_file, iunit)
    call read_flap_worker_inputs (iunit, flap_spec, flap_angles)
    call close_input_file (iunit)

    ! flap set? 

    if (size(flap_angles) == 1 .and. flap_angles(1) == 0d0) then 
      call my_stop ('No flap angles defined in input file')
    elseif (size(flap_angles) == 1) then 
      call print_colored (COLOR_NORMAL, 'Setting one flap position')
    else
      call print_colored (COLOR_NORMAL, 'Setting '//stri(size(flap_angles))//' flap positions')
    end if
    print '(1x,A,I2,A,F4.1,A)', 'at ', int (flap_spec%x_flap*1d2), &
                  '% ('//flap_spec%y_flap_spec//'=', flap_spec%y_flap,')'
    print *

    ! normalize if foil isn't 

    foil = seed_foil
    
    if (.not. is_normalized_coord(foil)) then
      call normalize (foil) 
      call print_action ("Normalizing airfoil")
    end if 

    ! Now set flap to all requested angles and write airfoils 

    if (.not. outname_auto) then 
      foil%filename = ensure_filename_extension (output_prefix, '.dat')
      foil%name     = output_prefix
    end if

    call write_airfoil_flapped (foil, flap_spec, flap_angles, outname_auto)

  end subroutine set_flap



  subroutine get_command_line (input_file, output_prefix, airfoil_name, action, &
                               second_airfoil_filename, value_argument, re_default)

    !-------------------------------------------------------------------------
    !! Reads command line arguments for input file name and output file prefix
    !-------------------------------------------------------------------------

    character(:), allocatable, intent(inout) :: input_file, output_prefix, action, value_argument
    character(:), allocatable, intent(inout) :: airfoil_name, second_airfoil_filename
    double precision, intent(inout)          :: re_default

    character(250) :: arg
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
          airfoil_name = trim(arg)
          i = i+2
        end if
      else if (trim(arg) == "-a2") then
        if (i == nargs) then
          call my_stop("Must specify filename of second airfoil for -a2 option.")
        else
          call getarg(i+1, arg)
          second_airfoil_filename = trim(arg)
          i = i+2
        end if
      else if (trim(arg) == "-w") then
        if (i == nargs) then
          call my_stop("Must specify an action for the worker e.g. polar")
        else
          call getarg(i+1, arg)
          action = trim(arg)
          i = i+2
          if (trim(action) == 'set') then
            call getarg(i, arg)
            value_argument = trim(arg)
            i = i+1
          elseif (trim(action) == 'blend') then
            call getarg(i, arg)
            value_argument = trim(arg)
            i = i+1
          end if 
        end if
      else if ( (trim(arg) == "-h") .or. (trim(arg) == "--help") ) then
        call print_worker_usage
        stop

      else if (trim(arg) == "-m") then
        i = i+2                                     ! skip -m (mode) here ( -> separate function) 

      else
        call print_worker_usage
        call my_stop ("Unrecognized option: "//trim(arg))
      end if

      if (i > nargs) getting_args = .false.
    end do

  end subroutine 



  subroutine print_worker_usage()

    !-------------------------------------------------------------------------
    !! Print usage information of worker
    !-------------------------------------------------------------------------

    print *
    call print_colored (COLOR_FEATURE,' '//PGM_NAME)
    print *,'                                '//trim(XOPTFOIL_VERSION_TEXT)
    print *
    print *,"Usage: worker -w worker_action [Options]"
    print *
    print *,"  -w polar          Generate polars of 'airfoil_file' in xfoil format"
    print *,"  -w polar-flapped  Generate polars of 'airfoil_file' with flapped polars in a single directory"
    print *,"  -w norm           Repanel, normalize 'airfoil_file'"
    print *,"  -w flap           Set flap of 'airfoil_file'"
    print *,"  -w bezier         Create a Bezier based airfoil matching 'airfoil_file'"
    print *,"  -w set [arg]      Set geometry parameters where [arg]:"
    print *,"                       't=zz'  max. thickness in % chord"
    print *,"                       'xt=zz' max. thickness location in % chord"
    print *,"                       'c=zz'  max. camber in % chord"
    print *,"                       'xt=zz' max. camber location in % chord"
    print *,"                       'te=y'  trailing edge gap in % chord (80% blending)"
    print *,"  -w blend xx       Blend 'airfoil_file' with 'second_airfoil_file' by xx%"
    print *,"  -w check-input    Check a Xoptfoil2 input file for errors"
    print *
    print *,"Options:"
    print *,"  -i input_file     Specify an input file"
    print *,"  -o output_prefix  Specify an output prefix (default: 'foil')"
    print *,"  -r xxxxxx         Specify a default reynolds number (re_default)"
    print *,"  -a airfoil_file   Specify filename of seed airfoil"
    print *,"  -a2 airfoil_file  Specify filename of a second airfoil (for blending)"
    print *,"  -h, --help        Display usage information and exit"
    print *
    print *,"Refer to the Worker reference guide for complete input help."
    print *

  end subroutine print_worker_usage

end module



program worker

  !-------------------------------------------------------------------------
  !!   main  
  !-------------------------------------------------------------------------

  use commons,            only : show_details, MODE_CHILD_PROCESS
  use os_util 

  use input_read,         only : run_mode_from_command_line

  use airfoil_base,       only : airfoil_type, split_foil_into_sides, airfoil_load
  use xfoil_driver,       only : xfoil_init, xfoil_cleanup, xfoil_options_type
  use xfoil_driver,       only : xfoil_set_airfoil, xfoil_reload_airfoil, xfoil_defaults
  use worker_functions

  implicit none
  type(airfoil_type) :: foil, blend_foil
  type (xfoil_options_type) :: xfoil_options

  character(:), allocatable  :: input_file, output_prefix, airfoil_filename, second_airfoil_filename
  character(:), allocatable  :: action, value_argument
  logical                    :: outname_auto, file_exists
  double precision           :: re_default_cl

  print *

  ! get run_mode from command 

  if (run_mode_from_command_line () == MODE_CHILD_PROCESS) then 
    call set_my_stop_to_stderr (.true.) 
  end if 

  ! Set default names and read command line arguments

  show_details      = .true. 
  call set_show_details (show_details)  

  input_file        = ''
  output_prefix     = ''
  action            = ''
  airfoil_filename  = ''
  second_airfoil_filename = ''
  re_default_cl     = 0d0 

  call get_command_line(input_file, output_prefix, airfoil_filename, action, & 
                       second_airfoil_filename, value_argument, re_default_cl)

  if (action == "") then
    call my_stop("Must specify an action for the Worker with -w option.")

  else if (action == "check-input") then 
    if (input_file == "") call my_stop("Must specify an input file with -i option.")

  else if (airfoil_filename == "") then
    call my_stop("Must specify an airfoil file with the -a option.")
  end if 

  ! input file exists if specified? 
 
  if (input_file /= '') then 
    inquire (FILE=input_file, EXIST=file_exists)  
    if (.not. file_exists) &
       call my_stop("Input file "// quoted(input_file) // " not found")
  end if 

  ! Let's start

  write (*,'(1x)', advance = 'no') 
  if (action == "check-input") then 
    call print_colored (COLOR_FEATURE,'Worker')
    write (*,'(3x,A,A,1x,A,3x)', advance = 'no') '-', action, input_file 

  else
    call print_colored (COLOR_FEATURE,'Worker on '//airfoil_filename)
    write (*,'(3x,A,A,3x)', advance = 'no') '-',action  

    ! Load airfoil defined in command line 

    foil = airfoil_load (airfoil_filename)

  end if 

  ! Allocate xfoil variables
  call xfoil_init()
  xfoil_options%silent_mode = .true.
  call xfoil_defaults(xfoil_options)

  ! Set name of output file - from command line or auto?  
  if (output_prefix == '') then
    output_prefix = airfoil_filename (1:(index (airfoil_filename,'.', back = .true.) - 1))
    outname_auto = .true.
  else
    outname_auto = .false.
  end if

  ! Do actions according command line option

  select case (action) 

    case ('bezier')       ! Generate bezier airfoil "<output_prefix>_bezier.dat" and "...bez"

      call match_bezier (input_file, outname_auto, output_prefix, foil)

    case ('bspline')       ! Generate bspline airfoil "<output_prefix>_bspline.dat" and "...bsp"

      call match_bspline (input_file, outname_auto, output_prefix, foil)

    case ('polar','polar-flapped')  ! Generate polars in subdirectory ".\<output_prefix>_polars\*.*"

      call generate_polars (input_file, output_prefix, re_default_cl, foil)

    case ('norm')         ! Repanel, Normalize into "<output_prefix>.dat"

      call repanel_normalize (input_file, outname_auto, output_prefix, foil)
  
    case ('flap')         ! Repaneland set flap into "<output_prefix>.dat"

      call set_flap (input_file, outname_auto, output_prefix, foil)

    case ('check-input')  ! check the input file for erros

      call check_input_file (input_file)

    case ('set')          ! set geometry value like thickness etc...
      
      call set_geometry_value (input_file, outname_auto, output_prefix, foil, value_argument)

    case ('blend')         ! blend two airfoils...
      
      if (trim(second_airfoil_filename) == "") &
        call my_stop("Must specify a second airfoil file with the -a2 option.")

      blend_foil = airfoil_load (second_airfoil_filename)
      call blend_foils (input_file, outname_auto, output_prefix, foil, blend_foil, value_argument)

    case default

      print * 
      call print_worker_usage()
      call my_stop ("Unknown action '"//trim(action)//"' defined for paramter '-w'")
  end select 
 
  write (*,*) 

  call xfoil_cleanup()

end program worker


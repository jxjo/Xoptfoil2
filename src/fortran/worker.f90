! MIT License
! Copyright (c) 2022-2024 Jochen Guenzel

!
!   Worker  
!
!   Utility Functions based on xfoil to complement Xoptfoil2
!   
!   The Worker uses an Xoptfoil input-file to get the paramters.
!     only a few sections are needed
!   


module worker_functions

  use os_util
  use print_util
  use commons,        only : show_details
  use airfoil_base,   only : airfoil_type, panel_options_type

  implicit none

#ifndef PACKAGE_VERSION
#define PACKAGE_VERSION ""
#endif
  

contains

  subroutine match_bezier (input_file, outname_auto, output_prefix, seed_foil)

    !-------------------------------------------------------------------------
    !! adapt bezier curves to top and bot side and generate new airfoil 
    !-------------------------------------------------------------------------

    use airfoil_geometry,     only : repanel_and_normalize
    use airfoil_base,         only : airfoil_write_with_shapes, is_normalized_coord 
    use airfoil_preparation,  only : transform_to_bezier_based

    use input_read,           only : read_bezier_inputs, read_panel_options_inputs
    use input_read,           only : open_input_file, close_input_file

    use shape_bezier,         only : shape_bezier_type

    implicit none


    character(*), intent(in)        :: input_file, output_prefix
    type (airfoil_type), intent (inout)  :: seed_foil
    logical, intent(in)             :: outname_auto

    type (airfoil_type)             :: foil
    type (shape_bezier_type)        :: shape_bezier
    type (panel_options_type)         :: panel_options
    integer                         :: iunit

    write (*,*) 'Match bezier curves for the top and bot side'

    ! Read inputs file to get options needed 

    call open_input_file (input_file, iunit, optionally=.true.)
    call read_panel_options_inputs (iunit, panel_options)
    call read_bezier_inputs (iunit, shape_bezier)
    call close_input_file (iunit)

    ! Prepare airfoil  - Repanel and split if not LE at 0,0 
  
    write (*,*) 
    if (.not. is_normalized_coord(seed_foil)) then 
        call repanel_and_normalize (seed_foil, foil, panel_options)
    else
      foil = seed_foil
    end if  

    if (outname_auto) then 
      foil%name = output_prefix // '-bezier'
    else
      foil%name = output_prefix
    end if

    ! simplex optimization for both side  

    show_details = .true.
    call transform_to_bezier_based (shape_bezier, panel_options, foil)

    call airfoil_write_with_shapes (foil, "")             


  end subroutine match_bezier



  subroutine check_and_do_polar_generation (input_file, csv_format, output_prefix, re_default_cl, foil)

    !-------------------------------------------------------------------------
    !! Generate polars
    ! - read input file for namelist &polar_generation
    ! - get polar definitions
    ! - calculate polars for foil with foilname
    ! - write each polar to a file 
    !-------------------------------------------------------------------------

    use xfoil_driver,       only : xfoil_options_type, re_type
    use xfoil_driver,       only : flap_spec_type

    use input_read,         only : open_input_file, close_input_file
    use input_read,         only : read_xfoil_options_inputs
    use input_read,         only : read_panel_options_inputs
    use input_read,         only : read_flap_worker_inputs

    use polar_operations,   only : read_init_polar_inputs, generate_polar_set, set_polar_info
  
    character(*), intent(in)        :: input_file, output_prefix
    logical, intent(in)             :: csv_format
    double precision, intent(in)    :: re_default_cl 
    type (airfoil_type), intent (in)  :: foil

    type (panel_options_type)        :: panel_options
    type (xfoil_options_type)      :: xfoil_options
    type (flap_spec_type)          :: flap_spec
    type (re_type)                 :: re_default
    double precision, allocatable  :: degrees (:)
    logical                        :: generate_polar
    character (255)                :: polars_subdirectory
    integer                        :: iunit

    call open_input_file (input_file, iunit, optionally=.true.)
    call read_xfoil_options_inputs  (iunit, xfoil_options)
    call close_input_file (iunit)

    ! Read and set options for polar generation for each new design (generate_polar = .true.) 

    re_default%number = re_default_cl
    re_default%type   = 1

    call open_input_file (input_file, iunit)
    call read_init_polar_inputs (iunit, re_default, xfoil_options%ncrit, &
                                foil%name, csv_format, generate_polar)

    if (generate_polar) then

      call read_flap_worker_inputs (iunit, flap_spec, degrees)        ! csv supports flaps
      call read_panel_options_inputs (iunit, panel_options)

      xfoil_options%show_details = .true.

      write (*,*)
      write (*,*)

      if (csv_format) then

        polars_subdirectory = ''
        ! if output prefix specified take this a filename of polar file.csv
        if (output_prefix /= '') call set_polar_info ("", output_prefix//".csv", "")

      else

        flap_spec%use_flap = .false.                      ! xfoil file doesn't support flap

        ! Create subdir for polar files if not exist
        polars_subdirectory = output_prefix//'_polars'
        call make_directory (trim(polars_subdirectory), .true.) ! preserve existing
      end if
      ! Generate polars in this subdir 
      call generate_polar_set (.true., csv_format, trim(polars_subdirectory), foil, &
                              flap_spec, degrees, xfoil_options)

      call close_input_file (iunit)

    end if

  end subroutine check_and_do_polar_generation



  subroutine set_geometry_value (input_file, outname_auto, output_prefix, seed_foil, &
                                value_argument)

    !-------------------------------------------------------------------------
    !! Setting thickness of foil
    !-------------------------------------------------------------------------

    use airfoil_base,       only : airfoil_write, is_normalized_coord
    use airfoil_geometry,   only : normalize   
    use airfoil_geometry,   only : set_geometry, set_te_gap
    use input_read,         only : read_panel_options_inputs
    use input_read,         only : open_input_file, close_input_file
    
    character(*), intent(in)     :: output_prefix, value_argument, input_file
    type (airfoil_type), intent (inout) :: seed_foil
    logical, intent(in)          :: outname_auto

    type (airfoil_type)         :: foil
    type (panel_options_type)   :: panel_options
    character (:), allocatable  :: value_str
    character (2)               :: value_type
    double precision            :: value_number, value_percent 
    integer                     :: iunit, ierr

    print *,'Max thickness, camber or trailing edge gap ' 
    print *

    call open_input_file (input_file, iunit, optionally=.true. )
    call read_panel_options_inputs (iunit, panel_options)
    call close_input_file (iunit)

    value_type = value_argument (1:(index (value_argument,'=') - 1))
    value_str  = trim(value_argument ((index (value_argument,'=') + 1):))

    read (value_str ,*, iostat = ierr) value_percent
    value_number = value_percent / 100d0 


    if(ierr /= 0) & 
      call my_stop ("Wrong argument format '"//trim(value_argument)//"' in set command") 

    foil = seed_foil

    ! normalize if foil isn't 
    if (is_normalized_coord(foil)) then
      call normalize (foil) 
    end if 

    select case (trim(value_type))

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
      foil%name = output_prefix // '_' // (trim(value_type)) // "=" //value_str
    else
      foil%name = output_prefix
    end if

    call airfoil_write   (foil%name//'.dat', foil)

  end subroutine set_geometry_value


  subroutine check_input_file (input_file)

    !! Checks xoptfoil-jx input file for errors

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

    write (*,*) 
    airfoil_filename = ''
    re_default_cl = 0d0

    call read_inputs (input_file, airfoil_filename, output_prefix, show_details,&
                      eval_spec, shape_spec, optimize_options) 
    write (*,*) 
    call check_and_process_inputs (eval_spec, shape_spec, optimize_options)

    write(*,*)
    call print_colored (COLOR_GOOD, '- Input file seems OK')
    write(*,*)

  end subroutine


    
  subroutine check_foil_curvature (input_file, seed_foil)

    !-------------------------------------------------------------------------
    !! Checks curvature quality of foil 
    !-------------------------------------------------------------------------

    use eval_commons,         only : curv_constraints_type
    use airfoil_geometry,     only : repanel_and_normalize, get_geometry, te_gap
    use airfoil_geometry,     only : print_coordinate_data
    use airfoil_preparation,  only : check_airfoil_curvature, auto_curvature_constraints
    use input_read,           only : read_panel_options_inputs, read_curvature_inputs
    use input_read,           only : open_input_file, close_input_file
    use math_util,            only : count_reversals
    use spline,               only : spline_2D

    character(*), intent(in)     :: input_file
    type (airfoil_type), intent (in)  :: seed_foil

    type (panel_options_type)      :: panel_options
    type (airfoil_type)          :: tmp_foil, norm_foil
    type (curv_constraints_type) :: curv_constraints
    integer                      :: overall_quality, is, ie, nreversals, iunit
    double precision             :: curv_threshold, maxt, xmaxt, maxc, xmaxc

    print *,'geometry, curvature with reversals and spikes'

    call open_input_file (input_file, iunit, optionally=.true.)

    call read_panel_options_inputs  (iunit, panel_options)
    call read_curvature_inputs (iunit, curv_constraints)

    call close_input_file (iunit)

    tmp_foil = seed_foil


    !  ------------ seed airfoil data -----

    show_details = .false.
    print *
    call get_geometry (tmp_foil, maxt, xmaxt, maxc, xmaxc)

    print *
    call print_colored (COLOR_NOTE,'     ')
    call print_colored (COLOR_NOTE,&
        "Thickness "//strf('(F5.2)',maxt*100)//"% at "//strf('(F5.2)',xmaxt*100)//'%   |   ')
    call print_colored (COLOR_NOTE, &
          "Camber "//strf('(F5.2)',maxc*100)//"% at "//strf('(F5.2)',xmaxc*100)//'%   |   ')
    call print_colored (COLOR_NOTE, &
          "TE gap "//strf('(F5.2)', te_gap (seed_foil)*100)//"%")
    write (*,*)

    !  ------------ repanl, normalize  -----

    print * 
    show_details = .true.
    call repanel_and_normalize (tmp_foil, norm_foil, panel_options)

    tmp_foil%spl  = spline_2d (tmp_foil%x, tmp_foil%y)
    norm_foil%spl = spline_2d (norm_foil%x, norm_foil%y)

    print *
    tmp_foil%name  = "original"
    norm_foil%name = "normalized"
    call print_coordinate_data (tmp_foil, norm_foil, indent=5)
    print *

    !  ------------ analyze  -----

    call print_action ("Check_curvature.")
    print * 
    call check_airfoil_curvature (curv_constraints, norm_foil, overall_quality)
    print * 

    !  ------------ Find best values  -----

    call print_action ("Auto_curvature contraints for normalized airfoil")
    print * 

    ! supress reversal warning in auto_curvature_constraints

    is = curv_constraints%top%nskip_LE
    ie = size(norm_foil%top%x) 
    curv_threshold = curv_constraints%top%curv_threshold
    nreversals = count_reversals (is, ie, norm_foil%top%curvature, curv_threshold) 

    curv_constraints%top%max_curv_reverse = nreversals     
    call auto_curvature_constraints (norm_foil%top, curv_constraints%top)

    is = curv_constraints%bot%nskip_LE
    ie = size(norm_foil%bot%x) 
    curv_threshold = curv_constraints%bot%curv_threshold
    nreversals = count_reversals (is, ie, norm_foil%bot%curvature, curv_threshold) 

    curv_constraints%bot%max_curv_reverse = nreversals 
    call auto_curvature_constraints (norm_foil%bot, curv_constraints%bot)

  end subroutine check_foil_curvature



  subroutine repanel (input_file, outname_auto, output_prefix, seed_foil)

    !-------------------------------------------------------------------------
    !! Repanels and foil based on settings in 'input file'
    !-------------------------------------------------------------------------

    use eval_commons,         only : curv_constraints_type
    use airfoil_base,         only : airfoil_write

    use airfoil_geometry,     only : repanel_and_normalize
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
    call repanel_and_normalize (seed_foil, foil, panel_options)


    if (.not. outname_auto) then 
      foil%name = output_prefix
    end if

    call airfoil_write   (foil%name//'.dat', foil)

  end subroutine repanel



  subroutine blend_foils (input_file, outname_auto, output_prefix, seed_foil_in, blend_foil_in, value_argument)

    !-------------------------------------------------------------------------
    !! Blend to seed_foil a blend_foil by (value) % 
    !-------------------------------------------------------------------------

    use math_util,          only : interp_vector
    use airfoil_base,       only : rebuild_from_sides, airfoil_write, split_foil_into_sides
    use airfoil_geometry,   only : repanel_and_normalize, is_normalized
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


    ! Read inputs file to get panelling options  

    call open_input_file (input_file, iunit, optionally=.true.)
    call read_panel_options_inputs (iunit, panel_options)
    call close_input_file (iunit)


    ! Prepare - Repanel both airfoils? 

    write (*,*) 

    if (is_normalized (seed_foil_in)) then 
      in_foil = seed_foil_in
      call split_foil_into_sides (in_foil)
      call print_text ('- Airfoil '//in_foil%name //' is already normalized with '//& 
                            stri(size(in_foil%x)) //' points')
    else
      call repanel_and_normalize (seed_foil_in, in_foil, panel_options)
    end if 

    if (is_normalized (blend_foil_in)) then 
      blend_foil = blend_foil_in
      call split_foil_into_sides (blend_foil)
      call print_text ('- Airfoil '//blend_foil%name //' is already normalized with '//& 
                            stri(size(blend_foil%x)) //' points')
    else
      call repanel_and_normalize (blend_foil_in, blend_foil, panel_options)
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

    call print_action ('Blending '//seed_foil_in%name//' and '//&
            blend_foil_in%name//' with '//stri(int(blend_factor * 100))//'%')
  
    yt_blended = (1d0 - blend_factor) * yt + blend_factor * yttmp
    yb_blended = (1d0 - blend_factor) * yb + blend_factor * ybtmp

    ! and build new foil 

    blended_foil%top%x = xt
    blended_foil%top%y = yt_blended
    blended_foil%bot%x = xb
    blended_foil%bot%y = yb_blended
    
    call rebuild_from_sides (blended_foil)

    ! Write airfoil 

    if (outname_auto) then 
      blended_foil%name = output_prefix // '-blend'
    else
      blended_foil%name = output_prefix
    end if

    call airfoil_write (blended_foil%name//'.dat', blended_foil)

  end subroutine blend_foils



  subroutine set_flap (input_file, outname_auto, output_prefix, seed_foil)

    !-------------------------------------------------------------------------
    !! Repanels and set flaps of foil based on settings in 'input file'
    !-------------------------------------------------------------------------

    use xfoil_driver,       only : xfoil_apply_flap_deflection, xfoil_reload_airfoil
    use xfoil_driver,       only : xfoil_set_airfoil
    use xfoil_driver,       only : flap_spec_type
    use airfoil_base,       only : airfoil_write
    use airfoil_geometry,   only : repanel_and_normalize
    use input_read,         only : read_flap_worker_inputs
    use input_read,         only : read_panel_options_inputs
    use input_read,         only : open_input_file, close_input_file

    character(*), intent(in)          :: input_file, output_prefix
    type (airfoil_type), intent (inout)  :: seed_foil
    logical, intent(in)               :: outname_auto

    type (airfoil_type)       :: foil, foil_flapped
    type (flap_spec_type)     :: flap_spec
    type (panel_options_type) :: panel_options

    character(20)       :: text_degrees
    double precision    :: flap_degree
    double precision, allocatable :: degrees (:)
    character (255)     :: outname
    integer             :: i, iunit

    ! Read inputs file to get xfoil paneling options  

    call open_input_file (input_file, iunit, optionally=.true.)
    call read_panel_options_inputs (iunit, panel_options)
    call close_input_file (iunit)

    call open_input_file (input_file, iunit)
    call read_flap_worker_inputs (iunit, flap_spec, degrees)
    call close_input_file (iunit)

    ! flap set? 

    if (size(degrees) == 0) then 
      call my_stop ('No flap angles defined in input file')
    elseif (size(degrees) == 1) then 
      write (*,'(A)', advance = 'no') 'Setting one flap position'
    else
      write (*,'(A,I2,A)', advance = 'no') 'Setting',size(degrees),' flap positions'
    end if
    write (*,'(1x,A,I2,A,F4.1,A)') 'at ', int (flap_spec%x_flap*1d2), &
                  '% ('//flap_spec%y_flap_spec//'=', flap_spec%y_flap,')'

    ! Repanel seed airfoil with xfoil PANGEN 

    write (*,*) 
    call repanel_and_normalize (seed_foil, foil, panel_options)

    ! Write airfoil 

    if (outname_auto) then 
      outname = output_prefix // '-f' 
    else
      outname = output_prefix
    end if

    ! Now set flap to all requested angles

    do i = 1, size(degrees)

      flap_degree = degrees(i)

      if (int(flap_degree)*10  == int(flap_degree*10d0)) then  !degree having decimal?
        write (text_degrees,'(SP,I3)') int (flap_degree)
      else
        write (text_degrees,'(SP,F6.1)') flap_degree
      end if

      call print_action ('Setting flaps to', trim(adjustl(text_degrees))//' deg')

      call xfoil_set_airfoil(foil)
      call xfoil_apply_flap_deflection(flap_spec, flap_degree)
      call xfoil_reload_airfoil(foil_flapped)

      if (outname_auto) then 
        foil_flapped%name = output_prefix // '-f' // trim(adjustl(text_degrees))
      else
        foil_flapped%name = output_prefix
      end if
    
      call airfoil_write   (foil_flapped%name//'.dat', foil_flapped)

    end do

  end subroutine set_flap



  subroutine read_worker_clo(input_file, output_prefix, airfoil_name, action, &
                            second_airfoil_filename, value_argument, re_default)

    !-------------------------------------------------------------------------
    !! Reads command line arguments for input file name and output file prefix
    !-------------------------------------------------------------------------

    character(:), allocatable, intent(inout) :: input_file, output_prefix, action, value_argument
    character(:), allocatable, intent(inout) :: airfoil_name, second_airfoil_filename
    double precision, intent(inout)          :: re_default

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
      else
        call print_error ("Unrecognized option: "//trim(arg))
        write (*,*)
        call print_worker_usage
        stop 1
      end if

      if (i > nargs) getting_args = .false.
    end do

  end subroutine read_worker_clo


  subroutine print_worker_usage()

    !-------------------------------------------------------------------------
    !! Print usage information of worker
    !-------------------------------------------------------------------------

      
    write(*,'(A)') 'Worker      v'//trim(PACKAGE_VERSION)//  &
                   '            (c) 2024 Jochen Guenzel'
    write(*,'(A)')
    write(*,'(A)') "Usage: Worker -w worker_action [Options]"
    write(*,'(A)')
    write(*,'(A)') "  -w polar          Generate polars of 'airfoil_file' in xfoil format"
    write(*,'(A)') "  -w polar-csv      Generate polars of 'airfoil_file' in csv format"
    write(*,'(A)') "  -w norm           Repanel, normalize 'airfoil_file'"
    write(*,'(A)') "  -w flap           Set flap of 'airfoil_file'"
    write(*,'(A)') "  -w bezier         Create a Bezier based airfoil matching 'airfoil_file'"
    write(*,'(A)') "  -w check          Check the quality of surface curvature"
    write(*,'(A)') "  -w set [arg]      Set geometry parameters where [arg]:"
    write(*,'(A)') "                       't=zz'  max. thickness in % chord"
    write(*,'(A)') "                       'xt=zz' max. thickness location in % chord"
    write(*,'(A)') "                       'c=zz'  max. camber in % chord"
    write(*,'(A)') "                       'xt=zz' max. camber location in % chord"
    write(*,'(A)') "                       'te=y'  trailing edge gap in % chord (80% blending)"
    write(*,'(A)') "  -w blend xx       Blend 'airfoil_file' with 'second_airfoil_file' by xx%"
    write(*,'(A)') "  -w check-input    Check a Xoptfoil-JX input file for errors"
    write(*,'(A)')
    write(*,'(A)') "Options:"
    write(*,'(A)') "  -i input_file     Specify an input file"
    write(*,'(A)') "  -o output_prefix  Specify an output prefix (default: 'foil')"
    write(*,'(A)') "  -r xxxxxx         Specify a default reynolds number (re_default)"
    write(*,'(A)') "  -a airfoil_file   Specify filename of seed airfoil"
    write(*,'(A)') "  -a2 airfoil_file  Specify filename of a second airfoil (for blending)"
    write(*,'(A)') "  -h, --help        Display usage information and exit"
    write(*,'(A)')
    write(*,'(A)') "Refer to the worker reference guide for complete input help."
    write(*,'(A)')

  end subroutine print_worker_usage


end module


!-------------------------------------------------------------------------
!   main  
!-------------------------------------------------------------------------

program worker

  use commons,            only : show_details 
  use os_util 
  use airfoil_base,       only : airfoil_type, airfoil_load, split_foil_into_sides
  use xfoil_driver,       only : xfoil_init, xfoil_cleanup, xfoil_options_type
  use xfoil_driver,       only : xfoil_set_airfoil, xfoil_reload_airfoil, xfoil_defaults
  use worker_functions

  implicit none
  type(airfoil_type) :: foil, blend_foil
  type (xfoil_options_type) :: xfoil_options

  character(:), allocatable  :: input_file, output_prefix, airfoil_filename, second_airfoil_filename
  character(:), allocatable  :: action, value_argument
  logical                    :: visualizer, outname_auto
  double precision           :: re_default_cl

  write(*,'(A)') 

  ! Set default names and read command line arguments

  show_details      = .true. 
  call set_show_details (show_details)  

  input_file        = ''
  output_prefix     = ''
  action            = ''
  airfoil_filename  = ''
  second_airfoil_filename = ''
  visualizer        = .false.
  re_default_cl     = 0d0 

  call read_worker_clo(input_file, output_prefix, airfoil_filename, action, & 
                       second_airfoil_filename, value_argument, re_default_cl)

  if (trim(action) == "") then
    call my_stop("Must specify an action for the worker with -w option.")

  else if (trim(action) == "check-input") then 
    if (trim(input_file) == "") & 
      call my_stop("Must specify an input file with -i option.")

  else if (trim(airfoil_filename) == "") then
    call my_stop("Must specify an airfoil file with the -a option.")
  end if 

  ! Let's start

  write (*,'(1x)', advance = 'no') 
  if (trim(action) == "check-input") then 
    call print_colored (COLOR_FEATURE,'Worker')
    write (*,'(3x,A,A,1x,A,3x)', advance = 'no') '-',trim(action), trim(input_file) 

  else
    call print_colored (COLOR_FEATURE,'Worker on '//trim(airfoil_filename))
    write (*,'(3x,A,A,3x)', advance = 'no') '-',trim(action)  

    ! Load airfoil defined in command line 

    call airfoil_load(airfoil_filename, foil)

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

  select case (trim(action)) 

    case ('bezier')       ! Generate bezier airfoil "<output_prefix>_bezier.dat" and "...bez"

      call match_bezier (input_file, outname_auto, output_prefix, foil)

    case ('polar')        ! Generate polars in subdirectory ".\<output_prefix>_polars\*.*"

      call check_and_do_polar_generation (input_file, .false., output_prefix, re_default_cl, foil)

    case ('polar-csv')    ! Generate polars in csv format "<output_prefix>.csv"

      call check_and_do_polar_generation (input_file, .true., output_prefix, re_default_cl, foil)

    case ('norm')         ! Repanel, Normalize into "<output_prefix>.dat"

      call repanel (input_file, outname_auto, output_prefix, foil)
  
    case ('flap')         ! Repaneland set flap into "<output_prefix>.dat"

      call set_flap (input_file, outname_auto, output_prefix, foil)

    case ('check')        ! Check the curvature quality of airfoil surface

      call check_foil_curvature (input_file, foil)

    case ('check-input')  ! check the input file for erros

      call check_input_file (input_file)

    case ('set')          ! set geometry value like thickness etc...
      
      call set_geometry_value (input_file, outname_auto, output_prefix, foil, value_argument)

    case ('blend')         ! blend two airfoils...
      
      if (trim(second_airfoil_filename) == "") &
        call my_stop("Must specify a second airfoil file with the -a2 option.")

      call airfoil_load(second_airfoil_filename, blend_foil)
      call blend_foils (input_file, outname_auto, output_prefix, foil, blend_foil, value_argument)

    case default

      write (*,*)
      call print_error ("Unknown action '"//trim(action)//"' defined for paramter '-w'")
      call print_worker_usage()

  end select 
 
  write (*,*) 

  call xfoil_cleanup()

end program worker


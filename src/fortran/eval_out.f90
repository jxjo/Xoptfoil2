! MIT License
! Copyright (c) 2025 Jochen Guenzel

!
! print abd write improvement info during optimization 
!

module eval_out 

  use os_util
  use print_util
  use string_util
  use commons

  use op_point         
  use geo_target
  use airfoil_base,     only : airfoil_type
  use eval_commons

  use xfoil_driver,     only : xfoil_options_type

  use shape_hicks_henne,only : shape_hh_type

  ! use eval_commons

  implicit none
  public

  private :: airfoil_type
  private :: op_point_spec_type, re_type 
  private :: op_point_result_type
  private :: xfoil_options_type

contains


  subroutine write_design_op_points_header (iunit)
    !! write csv header of op points data 
    integer, intent(in) :: iunit
    write (iunit, '(A5,";",A4,8(";",A11), ";",A9, ";",A9)') "  No", "iOp", "alpha", "cl", "cd", &
                                                  "cm", "xtrt", "xtrb", "cp_min","dist", "dev", "flap"
  end subroutine



  subroutine write_design_op_points_data (iunit, design, op_point_specs, op_point_results, flap_angle)

    !! write csv op points result 
    
    use op_point,             only : op_point_eval_type, op_point_eval, is_target

    integer, intent(in)                     :: iunit, design
    type(op_point_spec_type), intent(in)    :: op_point_specs (:)
    type(op_point_result_type), intent(in)  :: op_point_results (:)
    double precision, intent(in)            :: flap_angle (:)

    type(op_point_result_type)      :: op
    type(op_point_spec_type)        :: op_spec 
    type (op_point_eval_type)       :: op_eval

    integer                     :: i
    double precision            :: dist, dev
   

    do i = 1, size (op_point_specs)

      op      = op_point_results(i) 
      op_spec = op_point_specs(i) 
      op_eval = op_point_eval(op_spec, op)

      ! compatibility with old output 
      if (is_target(op_spec)) then
        dist = op_eval%target_deviation_abs
        dev  = op_eval%target_deviation
      else
        dist = op_eval%improvement_abs
        dev  = op_eval%improvement
      end if

      if (.not. op%converged) then 
        call print_error ('  Op '//stri(i)//' not converged in final calculation (this should not happen...)')
      end if 

      write(iunit,'(I5,";",I4, 9(";",F11.6), ";",F9.4)') &
                            design, i, op%alpha, op%cl, op%cd, op%cm, op%xtrt, op%xtrb, op%cp_min, &
                            dist, dev, flap_angle (i)
    end do
  end subroutine



  subroutine write_design_geo_targets_header (iunit)
    !! write csv header of op points data 
    integer, intent(in) :: iunit
    write (iunit, '(A5,";",A4,";",A12, 4(";",A11))') "  No", "iGeo", "type", "val",  &
                                                    "dist", "dev"
  end subroutine



  subroutine write_design_geo_targets_data (iunit, design, geo_target_def, geo_result)

    !! write csv op points result  

    integer, intent(in)                :: iunit, design
    type(geo_target_type), intent(in)  :: geo_target_def (:)
    type(geo_result_type), intent(in)  :: geo_result

    type(geo_target_type)       :: geo
    type(geo_target_eval_type)  :: geo_eval
    integer                     :: i
    double precision            :: dist, dev, val 

    do i = 1, size(geo_target_def)
      geo = geo_target_def(i) 

      val = geo_target_value(geo, geo_result)

      geo_eval = geo_target_eval(geo, geo_result)

      dist = geo_eval%target_deviation_abs
      dev  = geo_eval%target_deviation

      write(iunit,'(I5,";",I4, ";", A12, 3(";",F11.6))') &
                    design, i, geo_target_type_name(geo%type), val, dist, dev
    end do
  end subroutine



  subroutine write_design_coord_header (iunit, foil)
    !! write csv header of design airfoil coordinates 

    integer, intent(in) :: iunit
    type(airfoil_type), intent(in)  :: foil
    integer     :: i
    write (iunit, '(A5,";",A15, ";",A6)', advance='no') '  No', 'Name', 'Coord'
    do i = 1,size(foil%x)
      write (iunit, '(";",I10)', advance='no') i
    end do 
    write (iunit,*)

  end subroutine



  subroutine write_design_coord_data (iunit, design, foil)
    !! write csv design data - coordinates of airfoil

    integer, intent(in) :: iunit, design
    type(airfoil_type), intent(in)  :: foil
    character(:), allocatable :: name 
    integer     :: i

    name = design_foil_name (design, foil)
    write (iunit, '(I5,";",A,";",A5)', advance='no') design, name, 'x'
    do i = 1,size (foil%x)
      write (iunit, '(";",F10.7)', advance='no') foil%x(i)
    end do 
    write (iunit,*)

    write (iunit, '(I5,";",A,";",A5)', advance='no') design, name, 'y'
    do i = 1,size (foil%x)
      write (iunit, '(";",F10.7)', advance='no') foil%y(i)
    end do 
    write (iunit,*)

  end subroutine



  subroutine write_design_bezier_header (iunit, foil)
    !! write csv header of bezier design data 

    integer, intent(in) :: iunit
    type(airfoil_type), intent(in)  :: foil
    integer     :: i, ncp_top, ncp_bot

    ncp_top = size(foil%top%bezier%px)
    ncp_bot = size(foil%bot%bezier%px)

    write (iunit, '(A5,";",A15, ";",A5)', advance='no') '  No', 'Name', 'Side'
    do i = 1,max(ncp_top, ncp_bot)
      write (iunit, '(2(";",A12))', advance='no') 'p'//stri(i)//'x', 'p'//stri(i)//'y'
    end do 
    write (iunit,*)

  end subroutine



  subroutine write_design_bezier_data (iunit, design, foil)
    !! write csv design data - coordinates of control points

    integer, intent(in) :: iunit, design
    type(airfoil_type), intent(in)  :: foil
    character(:), allocatable :: name 
    integer     :: i, ncp_top, ncp_bot

    ncp_top = size(foil%top%bezier%px)
    ncp_bot = size(foil%bot%bezier%px)

    name = design_foil_name (design, foil)

    write (iunit, '(I5,";",A,";",A5)', advance='no') design, name, TOP
    do i = 1,ncp_top
      write (iunit, '(2(";",F12.8))', advance='no') foil%top%bezier%px(i), foil%top%bezier%py(i)
    end do 
    write (iunit,*)

    write (iunit, '(I5,";",A,";",A5)', advance='no') design, name, BOT
    do i = 1,ncp_bot
      write (iunit, '(2(";",F12.8))', advance='no') foil%bot%bezier%px(i), foil%bot%bezier%py(i)
    end do 
    write (iunit,*)

  end subroutine



  subroutine write_design_hh_header (iunit, foil, shape_hh)
    !! write csv header of hicks henne design data 

    integer, intent(in) :: iunit
    type(airfoil_type), intent(in)  :: foil
    type(shape_hh_type), intent(in) :: shape_hh
    integer     :: i

    ! first write header and coordinates of seed 
    
    call write_design_coord_header (iunit, foil)
    call write_design_coord_data (iunit, 0, foil)

    ! then header for hicks henne designs 

    write (iunit, '(A5,";",A15, ";",A5)', advance='no') '  No', 'Name', 'Side'
    do i = 1,max(shape_hh%nfunctions_top, shape_hh%nfunctions_bot)
      write (iunit, '(3(";",A12))', advance='no') 'hh'//stri(i)//'_str', 'hh'//stri(i)//'_loc', 'hh'//stri(i)//'_wid'
    end do 
    write (iunit,*)

  end subroutine



  subroutine write_design_hh_data (iunit, design, foil)
    !! write csv design data - coordinates of hicks henne fucktions

    integer, intent(in) :: iunit, design
    type(airfoil_type), intent(in)  :: foil
    character(:), allocatable :: name 
    integer     :: i, nhh_top, nhh_bot

    if (design > 0) then                      ! design #0 was already written in header

      nhh_top = size(foil%top%hh%hhs)
      nhh_bot = size(foil%bot%hh%hhs)

      name = design_foil_name (design, foil)

      write (iunit, '(I5,";",A,";",A5)', advance='no') design, name, TOP
      do i = 1,nhh_top
        write (iunit, '(3(";",F12.8))', advance='no') foil%top%hh%hhs(i)%strength, &
                                                      foil%top%hh%hhs(i)%location, &
                                                      foil%top%hh%hhs(i)%width
      end do 
      write (iunit,*)

      write (iunit, '(I5,";",A,";",A5)', advance='no') design, name, BOT
      do i = 1,nhh_bot
        write (iunit, '(3(";",F12.8))', advance='no') foil%bot%hh%hhs(i)%strength, &
                                                      foil%bot%hh%hhs(i)%location, &
                                                      foil%bot%hh%hhs(i)%width
      end do 
      write (iunit,*)
    end if 

  end subroutine


  function design_foil_name (design, foil)
    !! name of airfoil design when writing design to file 
    integer, intent(in) :: design
    type(airfoil_type), intent(in)  :: foil
    character(:), allocatable       :: design_foil_name
    character(:), allocatable       :: name

    name = filename_stem(ensure_filename_extension(foil%filename, '.dat'))

    if (design == 0) then                             ! seed foil 
      design_foil_name = name    
    else                                              ! design foils
      if (len (name) > 15) then
        name = name (1:15) // '__'
      end if 
      design_foil_name = name // '~'//stri(design)
    end if 

  end function


  subroutine write_airfoil_flapped (foil, flap_spec, flap_angles, auto_name) 

    !------------------------------------------------------------------------------
    !! Write airfoil files for all flap angles 
    !! Duplicate flap angles and flap =0.0 are not written  
    !! if auto_name, then something like '-f5.4' will be appended to foil name 
    !------------------------------------------------------------------------------

    use xfoil_driver,       only : xfoil_apply_flap_deflection, xfoil_reload_airfoil
    use xfoil_driver,       only : xfoil_set_airfoil
    use xfoil_driver,       only : flap_spec_type
    use airfoil_base,       only : airfoil_write_dat, print_airfoil_write, name_flapped_suffix
    use airfoil_base,       only : add_suffix_to_name
    use os_util,            only : filename_replace_extension
    
    type (airfoil_type), intent(in)           :: foil
    type (flap_spec_type), intent(in)         :: flap_spec
    double precision, intent(in)              :: flap_angles (:) 
    logical, intent(in)                       :: auto_name

    type (airfoil_type)           :: foil_flapped
    integer                       :: i, n 
    double precision              :: angle, min_val, max_val
    double precision, allocatable :: unique_angles (:) 

    ! remove duplicates angles 

    allocate (unique_angles(size(flap_angles)))

    min_val = minval(flap_angles) - 1d0
    max_val = maxval(flap_angles)
    n = 0 
    do while (min_val < max_val)
        n = n + 1
        min_val = minval(flap_angles, mask=flap_angles>min_val)
        unique_angles(n) = min_val
    enddo
    
    ! set flap with xfoil and write  

    do i = 1, n

      angle = unique_angles(i)

      if (angle /= 0d0) then 

        call xfoil_set_airfoil(foil)
        call xfoil_apply_flap_deflection(flap_spec, angle)
        call xfoil_reload_airfoil(foil_flapped)

        foil_flapped%filename = foil%filename
        foil_flapped%name     = foil%name

        if (auto_name) then 
          call add_suffix_to_name (foil_flapped, name_flapped_suffix (angle))
        end if

        call print_airfoil_write ("", filename_replace_extension (foil_flapped%filename, '.dat'), highlight=.true.)
        call airfoil_write_dat   (foil_flapped%filename, foil_flapped%name, foil_flapped%x, foil_flapped%y)
      end if 

    end do 

  end subroutine 


  subroutine print_improvement (op_point_specs, geo_targets, op_point_results, geo_result, &
                                use_flap, flap_angles) 

    !------------------------------------------------------------------------------
    !! Prints op results during optimization (show_details) 
    !------------------------------------------------------------------------------

    use op_point,           only : op_point_result_type, op_point_eval_type, op_point_eval
    use geo_target,         only : geo_target_type, geo_target_eval_type, geo_target_eval

    type(op_point_spec_type), intent(in)      :: op_point_specs (:)
    type(geo_target_type), intent(in)         :: geo_targets (:)
    type(op_point_result_type), intent(in)    :: op_point_results (:)
    type(geo_result_type),  intent(in)        :: geo_result
    double precision, allocatable, intent(in) :: flap_angles (:) 
    logical, intent(in)                       :: use_flap  

    type(op_point_result_type)        :: op
    type(op_point_spec_type)          :: op_spec
    type(geo_target_type)             :: geo_spec
    type(geo_target_eval_type)        :: geo_eval
    integer             :: i, intent, noppoint
    character(10)        :: geo_type 
    double precision     :: glide
    type(op_point_eval_type) :: op_eval

    intent = 10
    noppoint = size(op_point_results)

    if (noppoint > 0 ) then 

      call print_colored (COLOR_PALE, repeat(' ',intent))

      call print_colored (COLOR_PALE, 'Op'//'   ')
      call print_colored (COLOR_PALE, 'spec')
      call print_colored (COLOR_PALE, ' cl  '// '  ')
      call print_colored (COLOR_PALE, ' al  '// '  ')
      call print_colored (COLOR_PALE, ' cd   '// '  ')
      call print_colored (COLOR_PALE, 'glide'// ' ')
      if (use_flap) then
        call print_colored (COLOR_PALE, ' flap'//'    ')
      else
        call print_colored (COLOR_PALE, '  ')
      end if 
      call print_improvement_op (0, 'Optimize')
      if (bubble_detected (op_point_results)) then 
        call print_bubble_info (3, 'Ttr bubble   Btr bubble ')
      end if 
      write (*,*)
    
    end if 

    ! All op points - one per line -------------------------

    do i = 1, noppoint

      op      = op_point_results(i)
      op_spec = op_point_specs(i)
      op_eval = op_point_eval(op_spec, op)

      call print_colored (COLOR_PALE, repeat(' ',intent))

      call print_colored (COLOR_PALE, stri(i,2)//'   ')
      if (op_spec%spec_cl) then 
        call print_colored (COLOR_PALE, 'cl'//'  ')
      else
        call print_colored (COLOR_PALE, 'al'//'  ')
      end if 
      call print_colored (COLOR_PALE, strf('F5.2', op%cl,    .true.)//'  ')
      call print_colored (COLOR_PALE, strf('F5.2', op%alpha, .true.)//'  ')
      call print_colored (COLOR_PALE, strf('F6.5', op%cd   , .true.)//'  ')

      if (op%cl > 0.05d0) then 
        glide = op%cl / op%cd
        if (glide > 99.9d0) then 
          call print_colored (COLOR_PALE, strf('F5.1', glide  , .true.)//' ')
        else
          call print_colored (COLOR_PALE, strf('F5.2', glide  , .true.)//' ')
        end if 
      else 
        call print_colored (COLOR_PALE, '   - '//' ')
      end if
      if (use_flap) then
        call print_colored (COLOR_PALE, strf('F5.1', flap_angles(i)  , .true.)//'    ')
      else 
        call print_colored (COLOR_PALE, '  ')
      end if 
  
      ! -- improvement 

      call print_improvement_op (0, '', op_spec, op)

      ! -- weighting 

      ! call print_weighting (5, '', op_spec%weighting, op_spec%weighting_user, &
      !                              op_eval%objective)

      if (bubble_detected (op_point_results)) then 
        call print_bubble_info (3, '', op)
      end if 

      print *
    end do


    ! All Geo targets - one per line -------------------------

    ! if (size(geo_targets) > 0) print * 

    do i = 1, size(geo_targets)

      geo_spec = geo_targets(i)
      geo_type = geo_target_type_name(geo_spec%type)//'  '  ! for formatted print 
      geo_eval = geo_target_eval(geo_spec, geo_result)

      call print_colored (COLOR_PALE, repeat(' ',intent))

      call print_colored (COLOR_PALE, stri(i,2)//'   ')
      call print_colored (COLOR_PALE, geo_type)
      call print_colored (COLOR_PALE, '   ')
      call print_colored (COLOR_PALE, repeat(' ', 21))
      ! --
      call print_improvement_geos (0, '', geo_spec, geo_result)

      ! -- weighting 

      ! call print_weighting (5, '', geo_spec%weighting, geo_spec%weighting_user, &
      !                              geo_eval%objective)

      print *
    end do

   print *

  end subroutine print_improvement


  subroutine print_weighting (intent, header, weighting, weighting_user, objective)

    !! print weighting info of a single op 

    use math_util, only : clip
    
    integer, intent(in)                   :: intent
    character (*), intent(in)             :: header
    doubleprecision, intent(in), optional :: weighting
    doubleprecision, intent(in), optional :: weighting_user
    doubleprecision, intent(in), optional :: objective

    double precision    :: objective_raw

    call print_colored (COLOR_PALE, repeat(' ',intent))

    if (present(weighting)) then 

      objective_raw = objective / weighting

       call print_colored (COLOR_PALE, strf('F3.1', weighting_user) //' ')
      call print_colored (COLOR_NOTE, strf_dec (8, 5, objective_raw, no_sign=.true.))
      call print_colored ( COLOR_PALE, '  ') 
    else
      call print_fixed (header, 25)
    end if
  
  end subroutine 



  subroutine print_improvement_op (intent, header, op_spec, op)

    !! print improvement of a single opPoint

    use op_point,           only : op_point_result_type, op_point_eval, op_point_eval_type, op_point_eval_quality
    use op_point,           only : is_target, opt_type_name

    character (*), intent(in)                         :: header
    type(op_point_spec_type), intent(in), optional    :: op_spec
    type(op_point_result_type), intent(in), optional  :: op
    integer, intent(in)                               :: intent

    integer                       :: how_good, decimals
    character (:), allocatable    :: opt_name
    type(op_point_eval_type)      :: op_eval

    call print_colored (COLOR_PALE, repeat(' ',intent))

    if (.not. present(op_spec)) then 
      ! print header text 
      call print_fixed (header, 25)
      return
    end if

    ! calculate distance and improvement   

    op_eval = op_point_eval(op_spec, op)

    opt_name = opt_type_name(op_spec%opt_type)

    select case  (op_spec%opt_type)
      case (OPT_TARGET_CD)
        decimals = 5
      case (OPT_TARGET_GLIDE)
        decimals = 2
      case (OPT_TARGET_CL)
        decimals = 2
      case (OPT_TARGET_CM)
        decimals = 3
      case (OPT_TARGET_CP_MIN)
        decimals = 4
      case (OPT_MIN_SINK)
        decimals = 2
      case (OPT_MAX_GLIDE)
        decimals = 2
      case (OPT_MIN_CD)
        decimals = 5
      case (OPT_MAX_CL)
        decimals = 2
      case (OPT_MAX_XTR)
        decimals = 3
      case default
        decimals = 2
    end select

    how_good = op_point_eval_quality(op_spec, op_eval)

    ! output all the stuff

    call print_fixed (opt_name, 14)

    call print_colored (COLOR_PALE, ' ')

    if (is_target(op_spec)) then
      call print_colored (COLOR_PALE, 'gap: ')
      call print_colored (COLOR_NOTE, strf_dec (7, decimals, op_eval%target_deviation_abs, no_sign=.true.))
      call print_colored (COLOR_PALE, '  ')
      if (op_eval%target_reached) then 
          call print_colored_s (how_good, 'hit  ')
      else
        call print_colored_s (how_good, strf_auto(4, op_eval%target_deviation, no_sign=.true.)//"%")
      end if
    else
      call print_colored (COLOR_PALE, 'imp: ')
      call print_colored (COLOR_NOTE, strf_dec (7, decimals, op_eval%improvement_abs))
      call print_colored (COLOR_PALE, '  ')
      call print_colored_s (how_good, strf_auto(4, op_eval%improvement)//"%")
    end if


  end subroutine print_improvement_op 



  subroutine print_improvement_geos (intent, header, geo_spec, geo_result)

    !! print improvement of geo targets

    use op_point,           only : op_point_result_type

    character (*), intent(in)                   :: header
    type(geo_target_type), intent(in), optional :: geo_spec
    type(geo_result_type), intent(in), optional :: geo_result
    integer, intent(in) :: intent

    type(geo_target_eval_type)  :: geo_eval

    call print_colored (COLOR_PALE, repeat(' ',intent))

    if (present(geo_spec)) then 

      if (geo_spec%type == GEO_TARGET_THICKNESS .or. geo_spec%type == GEO_TARGET_CAMBER) then 

        geo_eval = geo_target_eval(geo_spec, geo_result)

        call print_fixed   ('target...', 14)
        call print_colored (COLOR_PALE, 'gap: ')
        call print_colored (COLOR_NOTE, strf_dec (7, 5, geo_eval%target_deviation_abs, no_sign=.true.))
        call print_colored (COLOR_PALE, '  ')

        if (geo_eval%target_reached) then 
          call print_colored_s (geo_eval%quality, 'hit  ')
        else
          call print_colored_s (geo_eval%quality, strf_auto(4, geo_eval%target_deviation, &
                                no_sign=.true.)//"%")
        end if

      end if 

    else 
      call print_fixed (header, 25)
    end if 

  end subroutine print_improvement_geos 



  subroutine print_bubble_info (intent, header, op)

    !! print xfoil bubble info of a single op 

    use op_point,           only : op_point_result_type

    character (*), intent(in) :: header
    type(op_point_result_type),        intent(in), optional :: op
    integer, intent(in) :: intent
    character (3)       :: from, to, xtr

    call print_colored (COLOR_PALE, repeat(' ',intent))

    !  >Ttr bubble   Btr bubble < 
    !  >.99 .88-.96  .72 .88-.95< 
    if (present(op)) then 

      if ((.not. op%bubblet%found) .and. (.not. op%bubbleb%found)) return
      
      ! call  print_colored (COLOR_WARNING,'Bubbles')
      if (op%bubblet%found) then 
        if (op%xtrt < 1d0) then 
          write (xtr ,'(F3.2)') min (op%xtrt, 0.99d0) 
        else
          xtr = ' - ' 
        end if 
        write (from,'(F3.2)') op%bubblet%xstart   
        if (op%bubblet%xend < 1d0) then 
          write (to  ,'(F3.2)') min (op%bubblet%xend, 0.99d0)   
        else
          write (to  ,'(F3.1)') op%bubblet%xend   
        end if  
        call  print_colored (COLOR_PALE,xtr // ' ' // from //'-'//to//'  ')
      else 
        call  print_colored (COLOR_PALE,repeat (' ', 13))
      end if 
      if (op%bubbleb%found) then 
        if (op%xtrb < 1d0) then 
          write (xtr ,'(F3.2)') min (op%xtrb, 0.99d0) 
        else
          xtr = ' - ' 
        end if 
        write (from,'(F3.2)') op%bubbleb%xstart    
        if (op%bubbleb%xend < 1d0) then 
          write (to  ,'(F3.2)') min (op%bubbleb%xend, 0.99d0)   
        else
          write (to  ,'(F3.1)') op%bubbleb%xend   
        end if  
        call  print_colored (COLOR_PALE,xtr // ' ' // from //'-'//to)
      else
        call  print_colored (COLOR_PALE,repeat (' ', 11))
      end if 
    else
      call print_fixed (header, 24)
    end if
  
  end subroutine print_bubble_info



  function bubble_detected (op_point_results)

    use op_point,           only : op_point_result_type

    logical :: bubble_detected
    type(op_point_result_type), dimension(:),  intent(in) :: op_point_results
    integer :: i

    bubble_detected = .false.
    do i = 1, size(op_point_results)
      if (op_point_results(i)%bubblet%found .or. op_point_results(i)%bubbleb%found) then
        bubble_detected = .true.
        return
      end if
    end do

  end function



  subroutine print_single_obj_contrib (label, value, improv_pp, improv_sorted, indent)

    character(*), intent(in)      :: label
    double precision, intent(in)  :: value, improv_pp
    double precision, intent(in)  :: improv_sorted (:)
    integer, intent(in)           :: indent

    double precision              :: first, second
    integer                       :: how_good

    if (improv_pp == 0d0) return

    first = improv_sorted(1)
    if (size(improv_sorted) > 1) then 
      second = improv_sorted(2)
    else
      second = 0d0
    end if

    if (improv_pp < 0d0) then 
      how_good = Q_BAD
    else if (improv_pp == first) then
      how_good = Q_GOOD
    else if (improv_pp == second) then
      how_good = Q_OK
    else
      how_good = Q_NO
    end if     

    call print_text  ('', indent, no_crlf=.true.)
    call print_fixed (label, 17)
    call print_text  (' '//strf_dec(10, 6, value, no_sign=.true.), 0, no_crlf=.true.)
    call print_highlighted (' ',how_good, strf_dec(7, 3, improv_pp, no_sign=.false.)//'%',&
                            '', no_crlf=.false.)

  end subroutine print_single_obj_contrib



  subroutine print_not_converged_reason (op_spec, op)

    !! Print possible reason for op point not converged 

    type(op_point_spec_type), intent(in)      :: op_spec
    type(op_point_result_type), intent(in)    :: op

    if (op%converged) return 

    if (op_spec%ma%number > 0d0 .and. .not. op_spec%spec_cl) then 

      print *
      call print_note("If mach number > 0.0 and there is a single op point with a high alpha")
      call print_text("xfoil may have difficulties to initalize boundary layer.", 7)
      call print_text("It could help to add an additional op opoint with a lower alpha", 7)
      call print_text("to get an initialized boundary layer.", 7)

    end if 

  end subroutine



  subroutine write_performance_summary (op_point_specs, xfoil_options, op_point_results, flap_angles)

    !! write a short performance summary to 'Performance_Summary.dat' 

    use commons,              only : design_subdir
    type (op_point_spec_type), intent(in)   :: op_point_specs (:)
    type (xfoil_options_type), intent(in)   :: xfoil_options
    type (op_point_result_type), intent(in) :: op_point_results (:)
    double precision, intent(in)            :: flap_angles (:)

    type (op_point_result_type)             :: op
    type (op_point_spec_type)               :: op_spec 
    integer                                 :: i, iunit
    double precision                        :: ncrit
    character(:), allocatable               :: aero_file
    character(20)                           :: flapnote

    aero_file  = design_subdir//'Performance_Summary.dat'

    iunit = 13
    open(unit=iunit, file=aero_file, status='replace')

    write(iunit,'(A)') " Optimal airfoil performance summary"
    write(iunit,'(A)') ""

    ! i    alpha     CL        CD       Cm    Top Xtr Bot Xtr   Re      Mach     ncrit     flap 
    ! --  ------- -------- --------- -------- ------- ------- -------- -------- ------- -----------
    !  7  -1.400   0.0042   0.00513  -0.0285  0.7057  0.2705  6.00E+04   0.000     9.1    5.23 spec
    ! I2    F8.3    F9.4     F10.5     F9.4    F8.4    F8.4     ES9.2     F8.3     F7.1    F6.2  

    write (iunit,'(A)') " i   alpha     CL        CD       Cm    Top Xtr Bot Xtr   Re      Mach    ncrit     flap"
    write (iunit,'(A)') " -- ------- -------- --------- -------- ------- ------- ------- -------- ------- -----------"

    do i = 1, size(op_point_specs)

      op_spec  = op_point_specs(i)
      op       = op_point_results(i) 

      if (flap_angles(i) /= 0d0) then
        write (flapnote, '(F6.2)') flap_angles(i)
        if (op_spec%flap_optimize) then
          flapnote = trim(flapnote) //" opt"
        else
          flapnote = trim(flapnote) //" fix"
        end if 
      else
        flapnote = "   -"
      end if 

      if (op_spec%ncrit == -1d0) then 
        ncrit = xfoil_options%ncrit
      else
        ncrit = op_spec%ncrit
      end if 

      write (iunit,  "(I2, F8.3, F9.4, F10.5, F9.4, F8.4, F8.4, ES9.2, F8.3, F7.1, 3X, A)") &
        i, op%alpha, op%cl, op%cd, op%cm, op%xtrt, op%xtrb, &
        op_spec%re%number, op_spec%ma%number, ncrit, trim(flapnote)
    end do
    close(iunit)
    call print_text ("- Writing summary to "//trim(aero_file))

  end subroutine 

end module 

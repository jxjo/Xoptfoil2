! MIT License
! Copyright (c) 2025 Jochen Guenzel

!
! print abd write improvement info during optimization 
!

module eval_out 

  use os_util
  use print_util

  use airfoil_base,     only : airfoil_type 
  use eval_commons

  use xfoil_driver,     only : op_point_spec_type, re_type
  use xfoil_driver,     only : op_point_result_type
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
                                                  "cm", "xtrt", "xtrb", "dist", "dev", "flap", "weight"
  end subroutine



  subroutine write_design_op_points_data (iunit, design, op_points_specification, op_points_result, flap_angle)
    !! write csv op points result  
    integer, intent(in)                     :: iunit, design
    type(op_point_spec_type), intent(in)    :: op_points_specification (:)
    type(op_point_result_type), intent(in)  :: op_points_result (:)
    double precision, intent(in)            :: flap_angle (:)

    type(op_point_result_type)              :: op
    type(op_point_spec_type)       :: op_spec 
    integer                     :: i,how_good
    double precision            :: dist, dev, weighting

    do i = 1, size (op_points_specification)
      op = op_points_result(i) 
      op_spec = op_points_specification(i) 
      call get_op_improvement(op_spec, op, dist, dev, how_good)

      if (.not. op%converged) then 
        call print_error ('  Op '//stri(i)//' not converged in final calculation (this should not happen...)')
      end if 

      if (op_spec%weighting_user_cur == 0d0) then           ! in beginning there is no dynamic weight
        weighting = op_spec%weighting_user
      else
        weighting = op_spec%weighting_user_cur
      end if 
      write(iunit,'(I5,";",I4, 8(";",F11.6), ";",F9.4, ";",F5.2)') &
                            design, i, op%alpha, op%cl, op%cd, op%cm, op%xtrt, op%xtrb, &
                            dist, dev, flap_angle (i), weighting
    end do
  end subroutine



  subroutine write_design_geo_targets_header (iunit)
    !! write csv header of op points data 
    integer, intent(in) :: iunit
    write (iunit, '(A5,";",A4,";",A12, 4(";",A11))') "  No", "iGeo", "type", "val",  &
                                                    "dist", "dev", "weight"
  end subroutine



  subroutine write_design_geo_targets_data (iunit, design, geo_target_def, geo_result)
    !! write csv op points result  
    integer, intent(in)                :: iunit, design
    type(geo_target_type), intent(in)  :: geo_target_def (:)
    type(geo_result_type), intent(in)  :: geo_result

    type(geo_target_type)              :: geo
    integer                     :: i,how_good
    double precision            :: dist, dev, weighting, val 

    do i = 1, size(geo_target_def)
      geo = geo_target_def(i) 

      if (geo%type == 'thickness') then 
        val = geo_result%maxt
      else if (geo%type == 'camber') then 
        val = geo_result%maxc
      else 
        val = 0d0 
      end if 

      call get_geo_improvement_info (geo, geo_result,  dist, dev, how_good)

      if (geo%weighting_user_cur == 0d0) then           ! in beginning there is no dynamic weight
        weighting = geo%weighting_user
      else
        weighting = geo%weighting_user_cur
      end if 
      write(iunit,'(I5,";",I4, ";", A12, 4(";",F11.6))') &
                    design, i, geo%type, val, dist, dev, weighting
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

    ncp_top = size(foil%top_bezier%px)
    ncp_bot = size(foil%bot_bezier%px)

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

    ncp_top = size(foil%top_bezier%px)
    ncp_bot = size(foil%bot_bezier%px)

    name = design_foil_name (design, foil)

    write (iunit, '(I5,";",A,";",A5)', advance='no') design, name, 'Top'
    do i = 1,ncp_top
      write (iunit, '(2(";",F12.8))', advance='no') foil%top_bezier%px(i), foil%top_bezier%py(i)
    end do 
    write (iunit,*)

    write (iunit, '(I5,";",A,";",A5)', advance='no') design, name, 'Bot'
    do i = 1,ncp_bot
      write (iunit, '(2(";",F12.8))', advance='no') foil%bot_bezier%px(i), foil%bot_bezier%py(i)
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

      nhh_top = size(foil%top_hh%hhs)
      nhh_bot = size(foil%bot_hh%hhs)

      name = design_foil_name (design, foil)

      write (iunit, '(I5,";",A,";",A5)', advance='no') design, name, 'Top'
      do i = 1,nhh_top
        write (iunit, '(3(";",F12.8))', advance='no') foil%top_hh%hhs(i)%strength, &
                                                      foil%top_hh%hhs(i)%location, &
                                                      foil%top_hh%hhs(i)%width
      end do 
      write (iunit,*)

      write (iunit, '(I5,";",A,";",A5)', advance='no') design, name, 'Bot'
      do i = 1,nhh_bot
        write (iunit, '(3(";",F12.8))', advance='no') foil%bot_hh%hhs(i)%strength, &
                                                      foil%bot_hh%hhs(i)%location, &
                                                      foil%bot_hh%hhs(i)%width
      end do 
      write (iunit,*)
    end if 

  end subroutine


  function design_foil_name (design, foil)
    !! name of airfoil design when writing design to file 
    integer, intent(in) :: design
    type(airfoil_type), intent(in)  :: foil
    character(:), allocatable       :: design_foil_name

    if (design == 0) then                             ! seed foil 
      design_foil_name = foil%name    
    else                                              ! design foils
      if (len (foil%name) > 15) then
        design_foil_name = foil%name (1:15) // '__'
      else
        design_foil_name = foil%name
      end if 
      design_foil_name = design_foil_name // '~'//stri(design)
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
    use airfoil_base,       only : airfoil_write, print_airfoil_write, airfoil_name_flapped
    
    type (airfoil_type), intent(in)           :: foil
    type (flap_spec_type), intent(in)         :: flap_spec
    double precision, intent(in)              :: flap_angles (:) 
    logical, intent(in)                       :: auto_name

    type (airfoil_type)           :: foil_flapped
    integer                       :: i, n 
    double precision              :: angle, min_val, max_val
    double precision, allocatable :: unique_angles (:) 
    character (:), allocatable    :: filename 

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

        if (auto_name) then 
          foil_flapped%name = airfoil_name_flapped (foil, angle)
        else
          foil_flapped%name = foil%name
        end if

        filename = foil_flapped%name//'.dat'
        call print_airfoil_write ("", fileName, 'dat', .true.)
        call airfoil_write       (filename, foil_flapped)
      end if 

    end do 

  end subroutine 


  subroutine print_improvement (op_points_spec, geo_targets, op_points_result, geo_result, &
                                use_flap, flap_angles, dynamic_done) 

    !------------------------------------------------------------------------------
    !! Prints op results during optimization (show_details) 
    !------------------------------------------------------------------------------

    use xfoil_driver,       only : op_point_result_type

    type(op_point_spec_type), intent(in)      :: op_points_spec (:)
    type(geo_target_type), intent(in)         :: geo_targets (:)
    type(op_point_result_type), intent(in)    :: op_points_result (:)
    type(geo_result_type),  intent(in)        :: geo_result
    double precision, allocatable, intent(in) :: flap_angles (:) 
    logical, intent(in)                       :: use_flap, dynamic_done  

    type(op_point_result_type)        :: op
    type(op_point_spec_type)          :: op_spec
    type(geo_target_type)             :: geo_spec
    integer             :: i, intent, noppoint
    character(10)        :: geo_type 
    doubleprecision     :: glide

    intent = 10
    noppoint = size(op_points_result)

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
      call print_improvement_op (0, 'Type Base  deviat/improv')
      if (dynamic_done) then
        call print_dynamic_weighting_op (5, 'Dynamic Weighting')
      elseif (bubble_detected (op_points_result)) then 
        call print_bubble_info (3, 'Ttr bubble   Btr bubble ')
      end if 
      write (*,*)
    
    end if 

    ! All op points - one per line -------------------------

    do i = 1, noppoint

      op      = op_points_result(i)
      op_spec = op_points_spec(i)

      call print_colored (COLOR_PALE, repeat(' ',intent))

      call print_colored (COLOR_PALE, stri(i,2)//'   ')
      if (op_spec%spec_cl) then 
        call print_colored (COLOR_PALE, 'cl'//'  ')
      else
        call print_colored (COLOR_PALE, 'al'//'  ')
      end if 
      call print_colored (COLOR_PALE, strf('(F5.2)', op%cl,    .true.)//'  ')
      call print_colored (COLOR_PALE, strf('(F5.2)', op%alpha, .true.)//'  ')
      call print_colored (COLOR_PALE, strf('(F6.5)', op%cd   , .true.)//'  ')

      if (op%cl > 0.05d0) then 
        glide = op%cl / op%cd
        if (glide > 99.9d0) then 
          call print_colored (COLOR_PALE, strf('(F5.1)', glide  , .true.)//' ')
        else
          call print_colored (COLOR_PALE, strf('(F5.2)', glide  , .true.)//' ')
        end if 
      else 
        call print_colored (COLOR_PALE, '   - '//' ')
      end if
      if (use_flap) then
        call print_colored (COLOR_PALE, strf('(F5.1)', flap_angles(i)  , .true.)//'    ')
      else 
        call print_colored (COLOR_PALE, '  ')
      end if 
  
      ! --
      call print_improvement_op (0, '', op_spec, op)

      ! --
      if (dynamic_done) then 
        call print_dynamic_weighting_op (5,'', op_spec)
      elseif (bubble_detected (op_points_result)) then 
        call print_bubble_info (3, '', op)
      end if 

      print *
    end do


    ! All Geo targets - one per line -------------------------

    ! if (size(geo_targets) > 0) print * 

    do i = 1, size(geo_targets)

      geo_spec = geo_targets(i)
      geo_type = geo_spec%type //'  '                        ! for formatted print 
      call print_colored (COLOR_PALE, repeat(' ',intent))

      call print_colored (COLOR_PALE, stri(i,2)//'   ')
      call print_colored (COLOR_PALE, geo_type)
      call print_colored (COLOR_PALE, '   ')

      if (geo_spec%type == 'thickness') then 

        call print_colored (COLOR_PALE, strf('(F7.5)', geo_result%maxt, .true.))

      elseif (geo_spec%type == 'camber') then 

          call print_colored (COLOR_PALE, strf('(F7.5)', geo_result%maxc, .true.))

      elseif (geo_spec%type == 'match-foil') then 
        ! call print_colored (COLOR_PALE, strf('(F7.5)', 0d0, .true.))
      end if 

      call print_colored (COLOR_PALE, '              ')
    ! --
      call print_improvement_geos (0, '', geo_spec, geo_result)
    ! --
      if (dynamic_done) call print_dynamic_weighting_geo (5, '', geo_spec)

      print *
    end do

   print *

  end subroutine print_improvement



  subroutine print_dynamic_weighting_op (intent, header, op_spec)

    !! print dynamic weighting ifo of a single op 

    character (*), intent(in) :: header
    type(op_point_spec_type), intent(in), optional :: op_spec
    integer, intent(in) :: intent
    doubleprecision     :: old_val, val
    character (30)      :: s

    call print_colored (COLOR_PALE, repeat(' ',intent))

    if (present(op_spec)) then 

      val     = op_spec%weighting_user_cur
      old_val = op_spec%weighting_user_prv

      if (op_spec%dynamic_weighting) then

        call print_colored (COLOR_PALE, strf('(F3.1)', old_val))
        call print_colored (COLOR_PALE, ' -> ')
        if (abs(old_val - val) / old_val > 0.1d0 ) then 
          call print_colored (COLOR_FEATURE, strf('(F3.1)', val))
        else
          call print_colored (COLOR_PALE, strf('(F3.1)', val))
        end if
        if (op_spec%extra_punch) then
          call print_colored (COLOR_FEATURE, '*') 
        else
          call print_colored (COLOR_HIGH, ' ') 
        end if

      else
        call print_colored (COLOR_PALE, strf('(F3.1)', val))
        call print_colored (COLOR_PALE, '    ')
        call print_colored (COLOR_PALE, 'fix ') 
      end if
      call print_colored ( COLOR_PALE, '    ') 
    else
      s = header
      call print_colored (COLOR_PALE, s (1:17))
    end if
  
  end subroutine 



  subroutine print_improvement_op (intent, header, op_spec, op)

    !! print improvement of a single opPoint

    use xfoil_driver,       only : op_point_result_type
    character (*), intent(in) :: header
    type(op_point_spec_type), intent(in), optional :: op_spec
    type(op_point_result_type),        intent(in), optional :: op
    integer, intent(in) :: intent
    doubleprecision     :: dist, dev, value_base
    integer             :: how_good
    character (5)       :: base
    character (4)       :: opt_type
    character (30)      :: s

    value_base = 1d0

    call print_colored (COLOR_PALE, repeat(' ',intent))

    if (present(op_spec)) then 

    ! calculate distance and improvement   

      call get_op_improvement (op_spec, op, dist, dev, how_good)

    ! nice text for output

      if (op_spec%optimization_type (1:6) == 'target') then
        opt_type = 'targ'

        select case  (op_spec%optimization_type)
          case ('target-drag')
            value_base = 0.01d0
            base  = 'cd'
          case ('target-glide')
            value_base = 10d0
            base  = 'glide'
          case ('target-lift')
            value_base = 1d0
            base = 'cl'
          case ('target-moment')
            value_base = 0.1d0
            base = 'cm'
        end select

      else
        select case  (op_spec%optimization_type)
          case ('min-sink')
            opt_type = 'max'
            base = 'climb'                                       ! scale_factor = seed value
            value_base = 10d0
          case ('max-glide')
            opt_type = 'max'
            base = 'cl/cd'                                       ! scale_factor = seed value
            value_base = 10d0
          case ('min-drag')
            opt_type = 'min'
            base = 'cd'                                          ! scale_factor = seed value
            value_base = 0.01d0
          case ('max-lift')
            opt_type = 'max'
            base = 'cl'                                          ! scale_factor = seed value
            value_base = 1d0
          case ('max-xtr')
            opt_type = 'max'
            base = 'xtr'                                         ! scale_factor = seed value
            value_base = 1d0
          case default
            opt_type = 'n.a.'
            base  = ' '
            value_base = 1d0
        end select
      end if

    ! output all the stuff

      call print_colored (COLOR_PALE, opt_type//' ')
      call print_colored (COLOR_PALE, base//' ')

      if (trim (opt_type) /= 'n.a.') then 
        if (value_base == 10d0) then 
          call print_colored_r (7,'(SP,F7.2)', -1, dist) 
        elseif (value_base == 1d0) then 
          call print_colored_r (7,'(SP,F7.3)', -1, dist) 
        elseif (value_base == 0.1d0) then 
          call print_colored_r (7,'(SP,F7.4)', -1, dist) 
        elseif (value_base == 0.01d0) then 
          call print_colored_r (7,'(SP,F7.5)', -1, dist) 
        else 
          call print_colored_r (7,'(SP,F7.3)', -1, dist) 
        end if 

        call print_colored (COLOR_PALE, '  ')
          if (abs(dev) < 9.95d0) then 
            call print_colored_r (4,'(SP,F4.1)', how_good, dev) 
          else
            call print_colored_i (4, how_good, nint(dev)) 
          end if
          call print_colored_s (               how_good, '%') 
      else
        call print_colored (COLOR_PALE, repeat(' ',14))
      end if

    else 

    ! print header text 

      write (s,'(A)') header
      call print_colored (COLOR_PALE, s (1:25))
    end if 

  end subroutine print_improvement_op 



  subroutine get_op_improvement (op_spec, op, dist, dev, how_good)
    !! calculate improvement of a single opPoint
    !! returns distance in base type units and
    !!         deviation in % 

    use xfoil_driver,       only : op_point_result_type
    type(op_point_spec_type), intent(in) :: op_spec
    type(op_point_result_type),        intent(in) :: op
    doubleprecision, intent(out ) :: dist, dev
    integer, intent(out )         :: how_good

    character (:),allocatable   :: opt_type 
    double precision            :: improv

    opt_type = op_spec%optimization_type

    if (opt_type (1:6) == 'target') then

      select case  (opt_type)
        case ('target-drag')
          dist  = op%cd - op_spec%target_value                ! positive is worse
          dev   = dist / op_spec%target_value * 100d0
        case ('target-glide')
          dist  = op%cl /op%cd - op_spec%target_value         ! negative is worse
          dev   = dist / op_spec%target_value * 100d0
        case ('target-lift')  
          dist = op%cl - op_spec%target_value                 ! negative is worse
          dev  = dist / (1d0 + op_spec%target_value) * 100d0
        case ('target-moment')
          dist = op%cm - op_spec%target_value                  ! negative is worse
          dev  = dist / (op_spec%target_value + 0.1d0) * 100d0 ! cm could be 0
      end select

      if (op_spec%allow_improved_target .and. opt_type == 'target-drag' .and. dev < 0d0) then 
        how_good = Q_GOOD
      else if (op_spec%allow_improved_target .and. opt_type /= 'target-drag' .and. dev > 0d0) then 
        how_good = Q_GOOD
      else
        how_good = r_quality (abs(dev), 0.1d0, 2d0, 10d0)      ! in percent
      end if

    else
      select case  (opt_type)
        case ('min-sink')
          dist = op%cl**1.5d0 / op%cd - op_spec%scale_factor   
          dev  = dist / op_spec%scale_factor * 100d0
          improv = dev                                         ! positive is good
        case ('max-glide')
          dist = op%cl / op%cd - op_spec%scale_factor          ! positive is good
          dev  = dist / op_spec%scale_factor * 100d0
          improv = dev                                         ! positive is good
        case ('min-drag')
          dist = op%cd - 1d0 / op_spec%scale_factor                  
          dev  = -dist * op_spec%scale_factor * 100d0
          improv = dev                                         ! negative is good
        case ('max-lift')
          dist = op%cl - op_spec%scale_factor                  
          dev  = dist / op_spec%scale_factor * 100d0
          improv = dev                                         ! positive is good
        case ('max-xtr')
          dist = 0.5d0*(op%xtrt + op%xtrb) + 0.1d0 -  op_spec%scale_factor  
          dev  = dist / op_spec%scale_factor * 100d0
          improv = dev                                         ! positive is good
        case default
          dist  = 0d0           
          dev   = 0d0
          improv = 0d0                                          
      end select
      if (improv <= 0d0) then 
        how_good = Q_BAD
      elseif (improv < 2d0) then 
        how_good = Q_OK
      else 
        how_good = Q_GOOD
      end if 
    end if

  end subroutine get_op_improvement 



  subroutine get_geo_improvement_info (geo_spec, geo_result,  dist, dev, how_good)
    !!returns improvment of a geo target
    type(geo_target_type), intent(in), optional :: geo_spec
    type(geo_result_type),        intent(in), optional :: geo_result
    doubleprecision, intent(out ) :: dist, dev
    integer, intent(out )         :: how_good

    if (present(geo_spec)) then 
      select case  (trim(geo_spec%type))

        case ('thickness')
          dist  = geo_result%maxt - geo_spec%target_value
          dev   = dist / geo_spec%target_value * 100d0
          how_good = r_quality (abs(dev), 0.07d0, 2d0, 10d0)   ! in percent

        case ('camber')  
          dist  = geo_result%maxc  - geo_spec%target_value   
          dev   = dist / geo_spec%target_value * 100d0
          how_good = r_quality (abs(dev), 0.07d0, 2d0, 10d0)   ! in percent

        case default
          dist = 0d0
          dev  = 0d0
          how_good = 0d0
        end select

    else 
      dev = 0d0 
      how_good = 0d0
      dist = 0d0
    end if 

  end subroutine get_geo_improvement_info



  subroutine print_improvement_geos (intent, header, geo_spec, geo_result)

    !! print improvement of geo targets

    use xfoil_driver,       only : op_point_result_type
    character (*), intent(in) :: header
    type(geo_target_type), intent(in), optional :: geo_spec
    type(geo_result_type), intent(in), optional :: geo_result
    integer, intent(in) :: intent
    doubleprecision     :: dist, dev, dev_top, dev_bot
    integer             :: how_good
    character (30)      :: s

    call print_colored (COLOR_PALE, repeat(' ',intent))

    if (present(geo_spec)) then 

      if (geo_spec%type == 'thickness' .or. geo_spec%type == 'camber' ) then 

        call get_geo_improvement_info (geo_spec, geo_result,  dist, dev, how_good)

        call print_colored (COLOR_PALE, 'targ'//' ')
        call print_colored (COLOR_PALE, 'y     ')
        call print_colored_r (7,'(SP,F7.5)', -1, dist) 
        call print_colored (COLOR_PALE, '  ')
        if (how_good == Q_Good) then 
          call print_colored_s (how_good, '  hit') 
        else
          if (abs(dev) < 10.0d0) then 
            call print_colored_r (4,'(SP,F4.1)', how_good, dev) 
          else
            call print_colored_r (4,'(SP,F4.0)', how_good, dev) 
          end if
          call print_colored_s (               how_good, '%') 
        end if

      else if (geo_spec%type == 'match-foil') then 

        dev_top = geo_result%match_top_deviation
        dev_bot = geo_result%match_bot_deviation

        call print_colored (COLOR_PALE, 'deviation top: '//strf('(F8.6)', dev_top))
        call print_colored (COLOR_PALE, '  bot: '//strf('(F8.6)', dev_bot))
  
      end if 

    else 
      write (s,'(A)') header
      call print_colored (COLOR_PALE, s (1:28))
    end if 

  end subroutine print_improvement_geos 



  subroutine print_dynamic_weighting_geo (intent, header, geo_spec)

    !! print dynamic weighting info of a single geo target

    character (*), intent(in) :: header
    type(geo_target_type), intent(in), optional :: geo_spec
    integer, intent(in) :: intent
    doubleprecision     :: old_val, val
    character (30)      :: s

    call print_colored (COLOR_PALE, repeat(' ',intent))
    if (present(geo_spec)) then 
      val     = geo_spec%weighting_user_cur
      old_val = geo_spec%weighting_user_prv
      write (s,'(F3.1)') old_val
      call print_colored (COLOR_PALE, trim(s))
      if (geo_spec%dynamic_weighting) then
        call print_colored (COLOR_PALE, ' -> ')
        write (s,'(F3.1)') val
        if (abs(old_val - val) / old_val > 0.1d0 ) then 
          call print_colored (COLOR_FEATURE, trim(s))
        else
          call print_colored (COLOR_PALE, trim(s))
        end if
        if (geo_spec%extra_punch) then
          call print_colored (COLOR_FEATURE, '*') 
        else
          call print_colored (COLOR_HIGH, ' ') 
        end if
      else
        call print_colored (COLOR_PALE, '    '//'fix ') 
      end if
      call print_colored ( COLOR_PALE, '      ') 
    else
      write (s,'(A)') header
      call print_colored (COLOR_PALE, s (1:17))
    end if
  
  end subroutine print_dynamic_weighting_geo



  subroutine print_bubble_info (intent, header, op)

    !! print xfoil bubble info of a single op 

    use xfoil_driver,       only : op_point_result_type

    character (*), intent(in) :: header
    type(op_point_result_type),        intent(in), optional :: op
    integer, intent(in) :: intent
    character (30)      :: s
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
      write (s,'(A)') header
      call print_colored (COLOR_PALE, s (1:24))
    end if
  
  end subroutine print_bubble_info



  function bubble_detected (op_points_result)

    use xfoil_driver,       only : op_point_result_type

    logical :: bubble_detected
    type(op_point_result_type), dimension(:),  intent(in) :: op_points_result
    integer :: i

    bubble_detected = .false.
    do i = 1, size(op_points_result)
      if (op_points_result(i)%bubblet%found .or. op_points_result(i)%bubbleb%found) then
        bubble_detected = .true.
        return
      end if
    end do

  end function




  subroutine write_performance_summary (op_points_spec, xfoil_options, op_points_result, flap_angles)

    !! write a short performance summary to 'Performance_Summary.dat' 

    use commons,              only : design_subdir
    type (op_point_spec_type), intent(in)   :: op_points_spec (:)
    type (xfoil_options_type), intent(in)   :: xfoil_options
    type (op_point_result_type), intent(in) :: op_points_result (:)
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

    do i = 1, size(op_points_spec)

      op_spec  = op_points_spec(i)
      op       = op_points_result(i) 

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

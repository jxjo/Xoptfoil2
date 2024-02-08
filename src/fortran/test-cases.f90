! MIT License
! Copyright (c) 2023 jxjo
!
! Driver to run all implemented testcases - exit with errorcode if error occured 
!

module test_util

  !-------------------------------------------------------------------------
  ! Utility functions for test cases
  !-------------------------------------------------------------------------

  use os_util
  use print_util
  
  implicit none

  integer :: nfails = 0 
  integer :: nok = 0 
  integer :: itime_started = 0

  contains
  
    subroutine test_header (message)  
      !! print header for a test section  
      character (*), intent(in)   :: message
      character (:), allocatable  :: line, text
      integer   :: ib, ie

      line = repeat('-',80)
      text = " " // message // " "
      ib = 3
      ie = ib + len(text) - 1
      line (ib:ie) = text
  
      write (*,*) 
      call print_text(line)
      write (*,*) 
  
    end subroutine 
  
    module subroutine test_footer (message)   
      !! print header for a test section with number of fails   

      
      use, intrinsic:: iso_fortran_env, only: stdin=>input_unit

      character (*), intent(in) :: message
  
      write (*,*) 
  
      if (nfails == 0) then 
        call print_colored (COLOR_NOTE, " -- Finished "//message//" ------ ")
        call print_colored (COLOR_GOOD, stri(nok)//" tests passed successfully")
      else
        call print_colored (COLOR_NOTE, " -- Finished "//message//" ------ ")
        call print_colored (COLOR_ERROR, stri(nfails)//" errors")
      end if 
      write (*,*) 
      write (*,*) 


      ! print *, 'Enter to continue ...'
      ! read(stdin,*)
    
    end subroutine 
  
    
    module subroutine assertf (val1, val2, message, decimals)  
      !! compare two float numbers, print message, return 1 if not equal 
  
      double precision, intent(in)  :: val1, val2
      character (*), intent(in)     :: message
      integer, intent (in), optional :: decimals
      integer   :: dec
      character (:), allocatable ::format_string, val1s, val2s

      if (.not. present (decimals)) then 
        dec = 7
      else 
        dec = decimals         
      end if
      
      format_string = "(f10."//stri(dec)//")"

      val1s = strf (format_string, val1)
      val2s = strf (format_string, val2)

      if (val1s == val2s) then
        call print_text (" - "//message//" - Ok")
        nok = nok + 1
      else
        nfails = nfails + 1
        call print_error (" - "//message//" - Failed")
        call print_text  ("     val1: "//val1s//"  <->  val2: "//val2s)
      end if 
  
    end subroutine 
  
  
    module subroutine asserti (val1, val2, message) 
      !! compare two float numbers, print message, return 1 if not equal 
      !!
      integer, intent(in)           :: val1, val2
      character (*), intent(in)     :: message
  
      if (val1 == val2) then
        call print_text (" - "//message//" - Ok")
        nok = nok + 1
      else
        nfails = nfails + 1
        call print_error (" - "//message//" - Failed")
        call print_text  ("     val1: "//stri(val1))
        call print_text  ("     val2: "//stri(val2))

      end if 
  
    end subroutine
  

    subroutine timing_start ()
      !! start the timer for timing measurements
   
      call system_clock(count=itime_started)

    end subroutine

    subroutine timing_result (message)
      !! start the timer for timing measurements
      character (*), intent(in) :: message

      integer :: itime_finish, rate
      double precision :: time_diff

      call system_clock(count_rate=rate)
      call system_clock(count=itime_finish)
      time_diff = real (itime_finish-itime_started)/real(rate)
      call print_colored (COLOR_FEATURE, "  - Time: "// message //"  "//strf('(f5.4)', time_diff)//"s")
      write (*,*)

    end subroutine

end module
    


module spline_test
  
  !-------------------------------------------------------------------------
  ! spline_1D and 2d tests
  !-------------------------------------------------------------------------

  use os_util
  use test_util

  implicit none

  contains

  subroutine test_spline_1d  ()

    !! tests for 1D spline  

    use math_deps,    only : linspace
    use spline,       only : spline_1D, eval_1D, spline_1D_type, NATURAL, NOT_A_KNOT

    double precision              :: x(7), y(7)
    type (spline_1D_type)            :: spl 
    double precision, allocatable :: xnew (:), ynew (:) 

    call test_header ("Spline_1D")

    data x  / 0d0, .5d0, 2d0,  3d0,  4d0,  5d0,  7d0 /  
    data y  / 0d0,  3d0, 0d0,  2d0,  0d0,  2d0,  0d0 /   

    spl = spline_1D (x,y, boundary=NATURAL)
    call assertf (eval_1D (spl, 1d0), 2.9546299523643555d0, "natural: ", 10)


    xnew = linspace (x(1), x(size(x)), 10)

    ynew = eval_1D (spl, xnew)
    !print *, "not a knot der=0", sum(ynew)
    call assertf (sum(ynew), 15.463452566096425d0, "not_a_knot: ", 10)

    ynew = eval_1D (spl, xnew, derivative=1)
    !print *, "not a knot der=1", sum(ynew)
    call assertf (sum(ynew), 7.3958873336792408d-2, "not_a_knot: derivative 1", 10)

    ynew = eval_1D (spl, xnew, derivative=2)
    ! print *, "not a knot der=2", sum(ynew)
    call assertf (sum(ynew), -49.181959564541224d0, "not_a_knot: derivative 2", 10)

  end subroutine



  subroutine test_spline_2d  ()

    !! tests for 2D spline  

    use math_deps,    only : linspace
    use spline,       only : spline_2D, spline_2D_type, NATURAL, NOT_A_KNOT
    use spline,       only : eval_spline, eval_spline_curvature

    double precision              :: x(7), y(7)
    type (spline_2D_type)         :: spl 
    double precision, allocatable :: xnew (:), ynew (:), s(:), curv (:)
    double precision              :: send

    call test_header ("Spline_1D")

    data x  / 0d0, .5d0, 2d0,  3d0,  4d0,  5d0,  7d0 /  
    data y  / 0d0,  3d0, 0d0,  2d0,  0d0,  2d0,  0d0 /   

    spl = spline_2D (x,y, boundary=NOT_A_KNOT)

    send = spl%s(size(spl%s))
    s = linspace (0d0, send, 10)

    call eval_spline (spl, s, xnew, ynew)
    ! print *, "not a knot der=0", sum(xnew), sum(ynew)
    call assertf (sum(xnew), 28.86562563555187d0,  "2D not_a_knot: xsum", 10)
    call assertf (sum(ynew), 12.849386740364190d0, "2D not_a_knot: ysum", 10)

    curv = eval_spline_curvature (spl, s)
    ! print *, "2D not a knot curvature", sum(curv), curv 
    call assertf (sum(curv), -10.268879474628816d0, "not_a_knot: curvature", 10)

    ! ynew = eval_1D (spl, xnew, derivative=2)
    ! ! print *, "not a knot der=2", sum(ynew)
    ! call assertf (sum(ynew), -49.181959564541224d0, "not_a_knot: derivative 2", 10)

  end subroutine

end module 



module airfoil_basics_test
  
  !-------------------------------------------------------------------------
  ! Airfoil basic functions like split ...
  !-------------------------------------------------------------------------

  use os_util
  use test_util
  use airfoil_operations,   only : airfoil_type 
  use airfoil_operations,   only : split_foil_into_sides, rebuild_from_sides
  use shape_bezier,         only : bezier_spec_type, create_bezier_example_airfoil      

  implicit none

  contains

  subroutine test_airfoil_split  ()

    !! test of split airfoil into top and bot 

    character (:), allocatable      :: name 
    double precision, allocatable   :: x(:), y(:)
    type(airfoil_type)              :: airfoil, new_airfoil 
    type(bezier_spec_type)          :: bezier, bot_bezier 
    ! integer :: i

    call test_header ("Airfoil split")

    call create_bezier_example_airfoil (201, name, x, y, bezier, bot_bezier)

    airfoil%x = x
    airfoil%y = y
    airfoil%name = name
    airfoil%symmetrical = .false. 

    call split_foil_into_sides (airfoil) 

    call asserti (size(airfoil%bot%x), 101, "Number of x points after split")
    call asserti (size(airfoil%bot%y), 101, "Number of y points after split")

    call assertf (sum(airfoil%top%y), 4.448841d0, "Checksum top y coordinates", 6)

    ! print *, "maxval ", maxval(airfoil%top%curvature), maxval(airfoil%bot%curvature)
    ! print *, "minval ", minval(airfoil%top%curvature), minval(airfoil%bot%curvature)

    call assertf (minval(airfoil%top%curvature), 0.17d0, "Min top curvature", 2)
    call assertf (minval(airfoil%bot%curvature), 0.06d0, "Min bot curvature", 2)
    call assertf (maxval(airfoil%top%curvature), 78.5d0, "Max top curvature", 1)
    call assertf (maxval(airfoil%bot%curvature),111.7d0, "Max bot curvature", 1)

    ! write(*,"('top curv LE: ',200f7.1)") ( airfoil%top%curvature(i), i=1,10 )
    ! write(*,"('bot curv LE: ',200f7.1)") ( airfoil%bot%curvature(i), i=1,10 )

    ! write(*,"('top curv TE: ',200f6.2)") ( airfoil%top%curvature(i), i=92,101 )
    ! write(*,"('bot curv TE: ',200f6.2)") ( airfoil%bot%curvature(i), i=92,101)

    ! --- now rebuild airfoil again 

    call rebuild_from_sides (airfoil%top, airfoil%bot, new_airfoil, "Rebuild example airfoil")
    call assertf (sum(airfoil%x), sum(new_airfoil%x), "Rebuilded airfoil x coordinates ")
    call assertf (sum(airfoil%y), sum(new_airfoil%y), "Rebuilded airfoil z coordinates ")


  end subroutine


  
  subroutine test_airfoil_normalize  ()

    !! test of split airfoil into top and bot 

    use airfoil_operations,   only : repanel_and_normalize

    character (:), allocatable      :: name 
    double precision, allocatable   :: x(:), y(:)
    type(airfoil_type)              :: airfoil, new_airfoil 
    type(bezier_spec_type)          :: bezier, bot_bezier 
    ! integer :: i

    call test_header ("Airfoil normalize")

    call create_bezier_example_airfoil (201, name, x, y, bezier, bot_bezier)

    airfoil%x = x
    airfoil%y = y
    airfoil%name = name
    airfoil%symmetrical = .false. 
    call split_foil_into_sides (airfoil) 

    call assertf (airfoil%top%curvature(1), 78.5d0, "le top curvature before ", 1)

    call repanel_and_normalize (airfoil, 181, new_airfoil) 

    call assertf (new_airfoil%top%curvature(1), 78.5d0, "le top curvature after  ", 1)

  end subroutine


end module 



module airfoil_evals_test
  
  !-------------------------------------------------------------------------
  ! Airfoil evaluations with geometry, curvature constraints 
  !-------------------------------------------------------------------------

  use os_util
  use test_util
  use commons,            only : NOT_DEF_D
  use airfoil_operations, only : airfoil_type
  use shape_bezier,       only : bezier_spec_type, create_bezier_MH30
  use airfoil_operations, only : split_foil_into_sides, rebuild_from_sides
  use eval_commons,       only : geo_constraints_type
  use eval_constraints,   only : eval_geometry_violations, max_panels_angle

  implicit none

  contains

  subroutine test_eval_constraints  ()

    !! test of geometry constraints 

    character (:), allocatable      :: name 
    double precision, allocatable   :: x(:), y(:)
    type(airfoil_type)              :: airfoil
    type(bezier_spec_type)          :: top_bezier, bot_bezier 
    type (geo_constraints_type)     :: geo 
    logical                         :: has_violation 
    character (:), allocatable      :: info

    call test_header ("Airfoil constraints")

    call create_bezier_MH30 (201, name, x, y, top_bezier, bot_bezier)

    airfoil%x = x
    airfoil%y = y
    airfoil%name = name
    airfoil%symmetrical = .false. 
    airfoil%npoint = size(x)

    call split_foil_into_sides (airfoil) 

    ! airfoil data 

    call assertf (max_panels_angle(airfoil), 12.2d0, "Max panels angle", 1)
    ! constraints 

    geo%check_geometry = .true.
    geo%min_thickness = NOT_DEF_D
    geo%max_thickness = NOT_DEF_D
    geo%min_te_angle = 2d0
    geo%min_camber = NOT_DEF_D
    geo%max_camber = NOT_DEF_D   

    call eval_geometry_violations (airfoil, geo, has_violation, info)

    call asserti (len(info), 0, "No violations - no text: "//info)

    geo%max_thickness = 0.07d0
    call eval_geometry_violations (airfoil, geo, has_violation, info)
    call asserti (len(info), 40, "Violated max thickness - text: "//info)

    geo%max_thickness = NOT_DEF_D
    geo%min_camber = 0.02d0
    call eval_geometry_violations (airfoil, geo, has_violation, info)
    call asserti (len(info), 37, "Violated min camber - text: "//info)

  end subroutine

end module 



module bezier_test
  
  !-------------------------------------------------------------------------
  ! Bezier functions 
  !-------------------------------------------------------------------------

  use os_util
  use test_util
  use shape_bezier

  implicit none

  contains

  subroutine test_bezier ()

    !! test of bezier implementation comparing to python bezier results 

    character (:), allocatable    :: name 
    double precision, allocatable :: x(:), y(:), u(:) 
    type(bezier_spec_type)        :: bezier, bot_bezier 
    integer :: i, j


    call test_header ("Bezier curves")

    call create_bezier_example_airfoil (201, name, x, y, bezier, bot_bezier)

    u = [0.0d0, 0.25d0, 0.5d0, 0.75d0, 1.0d0]

    ! test bezier eval 2D 

    call bezier_eval (bezier, u, x, y)

    call assertf (sum(x) + sum(y), 2.040625d0, "Eval x,y at u" ,6)
                                
    ! n = size(x)
    ! write(*,"('u: ',100f8.4)") ( u(i), i=1,n )
    ! write(*,"('x: ',100f8.4)") ( x(i), i=1,n )
    ! write(*,"('y: ',100f8.4)") ( y(i), i=1,n )

    ! test bezier eval 2D - 1st derivative

    call bezier_eval (bezier, u, x, y, 1)

    call assertf (sum(x) + sum(y), 4.893750d0, "Eval 1. derivative at u", 7)
                               
    ! n = size(x)
    ! write(*,"('dx: ',100f8.4)") ( x(i), i=1,n )
    ! write(*,"('dy: ',100f8.4)") ( y(i), i=1,n )

    ! test cosinus distribution 

    u = u_distribution_bezier (101)
    call asserti (size(u), 101, "Number of points")
    call assertf (sum(u), 50.73283d0, "Cosinus distribution", 5)
     
    ! write(*,"('u distribution_bezier: ',100f8.4)") ( u(i), i=1,size(u) )

    ! eval y on u 

    u = linspace (0d0, 1d0, 10)
    y = u
    do i = 1, size(u) 
      y(i) = bezier_eval_1D (bezier%py, u(i), 0 ) 
    end do 

    call assertf (sum(y), 0.4d0, "Eval y on u", 6 )
    ! write (*,*) checksumi
    ! write(*,"('u distribution: ',100f9.6)") ( u(i), i=1,size(u) )
    ! write(*,"('y evaluated:    ',100f9.6)") ( y(i), i=1,size(u) )

    ! eval y on x 

    x = linspace (0d0, 1d0, 10)
    y = x
    do i = 1, size(x) 
      y(i) = bezier_eval_y_on_x (bezier, x(i))
    end do 

    call assertf (sum(y), 0.417716d0, "Eval y on x", 6)
    ! write (*,*) checksumi
    ! write(*,"('x distribution: ',100f9.6)") ( x(i), i=1,size(x) )
    ! write(*,"('y evaluated:    ',100f9.6)") ( y(i), i=1,size(x) )

    ! eval curvature 

    call assertf (bezier_curvature(bezier, 0d0), 61.1d0, "Curvature top at le",1)
    call assertf (bezier_curvature(bezier, 1d0), 0.17d0, "Curvature top at te",2)
    
    call assertf (bezier_curvature(bot_bezier, 0d0),104.2d0, "Curvature bot at le",1)
    call assertf (bezier_curvature(bot_bezier, 1d0), 0.06d0, "Curvature bot at te",2)
    
    call timing_start ()
        x = linspace (0d0, 1d0, 100)
        y = x
        do j = 1,1000
          do i = 1, size(x) 
            y(i) = bezier_eval_y_on_x (bezier, x(i))
          end do 
        end do 
    call timing_result ("100000 evals y on x")

  end subroutine



  subroutine test_bezier_match ()

    !! test of bezier match foil with MH30-norm-bezier from Airfoil Editor 

    use commons,              only : show_details
    use airfoil_operations,   only : airfoil_type
    use airfoil_operations,   only : split_foil_into_sides, airfoil_write
    use airfoil_preparation,  only : match_bezier, match_bezier_target_le_curvature

    character (:), allocatable    :: name 
    double precision, allocatable :: delta(:)
    type(airfoil_type)            :: airfoil
    type(bezier_spec_type)        :: top_bezier, bot_bezier 
    integer :: i
    double precision              :: best_le_curv


    call test_header ("Bezier match airfoil MH30-norm-bezier")

    call create_bezier_MH30 (201, name, airfoil%x, airfoil%y, &
                                  airfoil%top_bezier, airfoil%bot_bezier)

    airfoil%name = name
    airfoil%symmetrical = .false. 
    airfoil%npoint = size(airfoil%x)

    call split_foil_into_sides (airfoil) 

    ! write seed to dat file 

    call print_text ("Writing Match_seed.dat",3)
    call airfoil_write("Match_seed.dat", "Match_seed", airfoil)

    ! simplex optimization 

    show_details = .true.
    call timing_start ()

    ! call match_bezier  (airfoil%top, size (airfoil%top_bezier%px), top_bezier)
    ! call match_bezier  (airfoil%bot, size (airfoil%bot_bezier%px), bot_bezier)
    best_le_curv = match_bezier_target_le_curvature (airfoil)

    call match_bezier  (airfoil%top, best_le_curv, 7, top_bezier)
    call match_bezier  (airfoil%bot, best_le_curv, 5, bot_bezier)

    call timing_result ("Matching top and bot")

    ! write result to bezier file 

    call print_text  ("Writing Match_bezier.bez", 3)
    call write_bezier_file ("Match_bezier.bez", "Match_bezier", top_bezier, bot_bezier)

    ! check delta between original and result bezier

    delta = top_bezier%px
    do i = 1, size(top_bezier%px)
      delta(i) = abs(top_bezier%px(i) - airfoil%top_bezier%px(i))
    end do 
    call assertf (sum(delta), 0.01d0, "Delta px of Bezier control points", 0)

    do i = 1, size(top_bezier%py)
      delta(i) = abs(top_bezier%py(i) - airfoil%top_bezier%py(i))
    end do 
    call assertf (sum(delta), 0.00d0, "Delta py of Bezier control points", 0)

    call assertf (bezier_curvature(top_bezier, 0d0), 407d0, "Curvature at LE",0)
    ! call assertf (bezier_curvature(top_bezier, 0d0), airfoil%top%curvature(1), "Curvature at LE",1)
    ! time check 



  end subroutine


  
  subroutine test_bezier_create_shape ()

    !! test of bezier create shape for optimization 

    use airfoil_operations,   only : airfoil_type
    use airfoil_operations,   only : split_foil_into_sides, te_gap

    character (:), allocatable    :: name 
    double precision, allocatable :: dv_top(:), dv_bot(:)

    type(airfoil_type)            :: seed_foil, foil
    type(bezier_spec_type)        :: top_bezier, bot_bezier 
    integer                       :: ndv_top, ndv_bot
    double precision              :: diff, side_te_gap, diff_seed

    call test_header ("Bezier create shape based on MH30-norm-bezier")

    ! create seed airfoil 

    call create_bezier_MH30 (201, name, seed_foil%x, seed_foil%y, &
                            seed_foil%top_bezier, seed_foil%bot_bezier)
    seed_foil%name = name
    seed_foil%symmetrical = .false. 
    seed_foil%npoint = size (seed_foil%x)

    call split_foil_into_sides (seed_foil) 

    ndv_top = ncp_to_ndv_side (size(seed_foil%top_bezier%px))
    ndv_bot = ncp_to_ndv_side (size(seed_foil%bot_bezier%px))

    ! make design vars 

    dv_top  = bezier_get_dv0 ("Top", seed_foil%top_bezier)
    dv_bot  = bezier_get_dv0 ("Bot", seed_foil%bot_bezier)

    ! build new bezier from design vars 

    side_te_gap = te_gap (seed_foil) / 2

    call map_dv_to_bezier ('Top', dv_top, side_te_gap, top_bezier)
    call map_dv_to_bezier ('Bot', dv_bot, side_te_gap, bot_bezier)

    call assertf (bot_bezier%px(5), seed_foil%bot_bezier%px(5), "Created bezier equal seed x", 8)

    ! build new airfoil 

    call bezier_eval_airfoil (top_bezier, bot_bezier, seed_foil%npoint, foil%x, foil%y) 

    call assertf (sum(seed_foil%x), sum(foil%x), "Created airfoil equal seed x", 2)
    call assertf (sum(seed_foil%y), sum(foil%y), "Created airfoil equal seed y", 2)

    diff_seed = abs (bezier_curvature(seed_foil%top_bezier, 0d0)- bezier_curvature(seed_foil%bot_bezier, 0d0))
    diff = abs (bezier_curvature(top_bezier, 0d0)- bezier_curvature(bot_bezier, 0d0))
    call assertf (diff, diff_seed, "Curvature diff at le",2)

  end subroutine

end module 


!==========================================================================
  

module simplex_test
 
  !-------------------------------------------------------------------------
  ! simplex (nelder mead) optimization
  !-------------------------------------------------------------------------
 
  use os_util
  use test_util

  implicit none

  contains

  function my_objective_function (dv)
    double precision, intent(in) :: dv(:)
    double precision :: my_objective_function 

    my_objective_function = dv(1) ** 2 + dv(2) ** 2 

  end function 

  subroutine test_simplex () 

    use simplex_search, only : simplexsearch, simplex_options_type 

    double precision      :: xmin(2), x0(2)
    double precision      :: fmin, f0_ref
    integer               :: steps, fevals, f
    type(simplex_options_type) :: sx_options

    f = 0 
    call test_header ("Simplex optimization")

    sx_options%tol   = 1d-7
    sx_options%maxit = 100
    x0 = [0.2d0,0.8d0]

    call simplexsearch(xmin, fmin, steps, fevals, my_objective_function, &
                       x0, .false. , f0_ref, sx_options)

    call assertf (xmin(1), 0d0, "Found min at x=0.0", 6)
    call asserti (steps, 88, "88 steps needed" )

  end subroutine 

end module

!----------------------------------------------------------------------------------------------


program test_cases

  !-------------------------------------------------------------------------
  ! main - Test Driver 
  !-------------------------------------------------------------------------
 
  use spline_test
  use bezier_test
  use simplex_test
  use airfoil_basics_test
  use airfoil_evals_test

  call test_spline_1d ()
  call test_spline_2d () 

  call test_bezier ()
  call test_bezier_create_shape ()

  ! call test_simplex ()
  call test_airfoil_split () 
  call test_airfoil_normalize ()

  call test_eval_constraints ()

  call test_bezier_match ()

  call test_footer ("") 

  if (nfails > 0) then 
    stop 1
  else
    stop 0 
  end if 

end program 


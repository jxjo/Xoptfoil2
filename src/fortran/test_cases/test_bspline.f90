! MIT License
! Copyright (c) 2025 Jochen Guenzel

module test_bspline
  
  !-------------------------------------------------------------------------
  ! bspline functions 
  !-------------------------------------------------------------------------

  use os_util
  use print_util
  use test_util
  use airfoil_base,       only : airfoil_type
  use airfoil_base,       only : create_bezier_JX_GS3_100
  use shape_bspline,      only : bspline_spec_type
  use shape_bspline,      only : bspline_eval, bspline_curvature, bspline_eval_y_on_x, bspline_eval_1D
  use shape_bspline,      only : map_dv_to_bspline, u_distribution_bspline, bspline_get_dv0
  use shape_bspline,      only : ncp_to_ndv
  use shape_bspline,      only : write_bspline_file


  implicit none

  private

  public :: test_bspline_all


contains

  subroutine test_bspline_all ()

    call test_header ("BSpline curves")

    call test_get_initial_bspline ()
    call test_bspline_eval ()
    call test_bspline_match ()
  end subroutine


  subroutine test_get_initial_bspline ()

    !! test of bspline initial bspline from airfoil 

    use os_util,               only : delete_file
    use math_util,             only : linspace
    use shape_bspline,         only : get_initial_bspline, print_bspline_spec, write_bspline_file
    use shape_bspline,         only : read_bspline_file, bspline_le_curvature
    use airfoil_base,          only : te_gap

    type(bspline_spec_type)       :: top_bspline, bot_bspline, top_bspline2, bot_bspline2
    type(airfoil_type)            :: foil
    double precision              :: y_te
    character (:), allocatable    :: name, name2
    integer                       :: ncp

    foil = create_bezier_JX_GS3_100 (161)
    y_te = te_gap (foil) / 2

    ncp = 8
    top_bspline = get_initial_bspline (foil%top%x, foil%top%y, le_curv=300.0d0, y_te= y_te, ncp=ncp)
    bot_bspline = get_initial_bspline (foil%bot%x, foil%bot%y, le_curv=300.0d0, y_te=-y_te, ncp=ncp)
    
    call assertf (bspline_le_curvature (top_bspline), 300.0d0, "LE curvature of initial top bspline", 1)
    call assertf (top_bspline%py(ncp), y_te, "TE gap of initial top bspline", 1)


    name = "Initial BSpline"
    call write_bspline_file ("Initial.bsp", name, top_bspline, bot_bspline)

    call read_bspline_file ("Initial.bsp", "Top", name2, top_bspline2)
    call read_bspline_file ("Initial.bsp", "Bot", name2, bot_bspline2)

    call delete_file ("Initial.bsp")

    call assert (name == name2, "BSpline name read/write")

    call assert (all (top_bspline%px == top_bspline2%px), "BSpline top px read/write")
    call assert (all (top_bspline%py == top_bspline2%py), "BSpline top py read/write")
    call assert (all (bot_bspline%px == bot_bspline2%px), "BSpline bot px read/write")
    call assert (all (bot_bspline%py == bot_bspline2%py), "BSpline bot py read/write")

  end subroutine



  subroutine test_bspline_eval ()

    !! test of bspline implementation comparing to python bspline results 

    use math_util,          only : linspace
    use shape_bspline,      only : bspline_eval, bspline_curvature, bspline_eval_y_on_x, bspline_eval_1D
    use shape_bspline,      only : get_initial_bspline
    use airfoil_base,       only : airfoil_type, airfoil_from_bspline, airfoil_write_with_shapes
    use airfoil_base,       only : te_gap

    character (:), allocatable    :: name 
    double precision, allocatable :: x(:), y(:), u(:) 
    type(bspline_spec_type)       :: bspline, bot_bspline, top_bspline
    type(airfoil_type)            :: foil
    double precision              :: y_te
    integer                       :: i, j, ncp


    foil = create_bezier_JX_GS3_100 (161)
    y_te = te_gap (foil) / 2

    ncp = 8
    top_bspline = get_initial_bspline (foil%top%x, foil%top%y, le_curv=300.0d0, y_te= y_te, ncp=ncp)
    bot_bspline = get_initial_bspline (foil%bot%x, foil%bot%y, le_curv=300.0d0, y_te=-y_te, ncp=ncp)

    foil = airfoil_from_bspline (top_bspline, bot_bspline, 161, "Bspline eval test")

    ! call airfoil_write_with_shapes (foil, "")

    ! Use the created bsplines for testing
    bspline = top_bspline
    name = foil%name

    u = [0.0d0, 0.25d0, 0.5d0, 0.75d0, 1.0d0]

    ! test bspline eval 2D 

    call bspline_eval (bspline, u, x, y)
    call assertf (sum(x) + sum(y), 2.24d0, "Eval x,y at u", 2)
                                
    ! test bspline eval 2D - 1st derivative

    call bspline_eval (bspline, u, x, y, 1)
    call assertf (sum(x) + sum(y), 5.6d0, "Eval 1. derivative at u", 1)

    ! test arc-length based distribution 

    u = u_distribution_bspline (bspline, 101)
    call asserti (size(u), 101, "Number of points")
    call assertf (sum(u), 51.5d0, "Arc-length distribution", 1)
     
    ! eval y on u 

    u = linspace (0d0, 1d0, 10)
    y = u
    do i = 1, size(u) 
      y(i) = bspline_eval_1D (bspline%py, u(i), der=0 ) 
    end do 

    call assertf (sum(y), 0.33d0, "Eval y on u", 2 )

    x = linspace (0d0, 1d0, 10)
    y = x
    do i = 1, size(x) 
      y(i) = bspline_eval_y_on_x (bspline, x(i))
    end do 

    call assertf (sum(y), 0.30d0, "Eval y on x", 2)

    ! eval curvature 

    call assertf (bspline_curvature(bspline, 0d0), 300.0d0, "Curvature top at le",1)
    call assertf (bspline_curvature(bspline, 1d0), 0.0d0, "Curvature top at te",1)
    
    call assertf (bspline_curvature(bot_bspline, 0d0), 300.0d0, "Curvature bot at le",1)
    call assertf (bspline_curvature(bot_bspline, 1d0), 0.0d0, "Curvature bot at te",1)
    
    call timing_start ()
        x = linspace (0d0, 1d0, 100)
        y = x
        do j = 1,1000
          do i = 1, size(x) 
            y(i) = bspline_eval_y_on_x (bspline, x(i))
          end do 
        end do 
    call timing_result ("100000 evals y on x")

  end subroutine

 

!   subroutine test_bspline_match ()

!     !! Test bspline C2-coupled matching: both sides 
!     !! LE curvature of top and bot must be equal by construction.

!     use commons,              only : show_details
!     use airfoil_preparation,  only : match_bspline, match_get_best_le_curvature
 

  subroutine test_bspline_match ()

    !! Test bspline C2-coupled matching: both sides 
    !! LE curvature of top and bot must be equal by construction.

    use commons,              only : show_details
    use airfoil_base,         only : airfoil_type, airfoil_from_bspline, airfoil_write_with_shapes
    use airfoil_preparation,  only : match_bspline, match_get_best_le_curvature
    use eval_constraints,     only : penalty_stats_print_table, penalty_stats_init, penalty_stats_print
    use shape_bspline,        only : bspline_le_curvature
    use math_util,            only : count_reversals

    type (airfoil_type)           :: airfoil, airfoil_matched
    type(bspline_spec_type)       :: top_bspline, bot_bspline
    double precision              :: best_le_curv, le_curv_top, le_curv_bot, le_curv_diff
    double precision              :: top_te_curv, bot_te_curv
    integer                       :: top_nrevers, bot_nrevers
    logical                       :: result_ok

    call test_header ("BSpline match airfoil JX-GS3-100")

    airfoil = create_bezier_JX_GS3_100 (81)

    show_details = .true.
    call timing_start ()

    best_le_curv = match_get_best_le_curvature (airfoil)
    top_nrevers = count_reversals (0, -1, airfoil%top%curvature, 0d0) 
    bot_nrevers = count_reversals (0, -1, airfoil%bot%curvature, 0d0)
    top_te_curv = airfoil%top%curvature (size(airfoil%top%curvature))
    bot_te_curv = airfoil%bot%curvature (size(airfoil%bot%curvature))

    call print_action ("Best LE curvature for matching: "//strf('(F7.2)', best_le_curv) // " with " // &
                        stri(top_nrevers) // " top reversals and " // &
                        stri(bot_nrevers) // " bot reversals in curvature")

    ! C2-coupled mode: py(2) derived from le_curv by construction - no retry loop needed

    call penalty_stats_init ()                ! reset penalty stats before matching
    call match_bspline (airfoil%top, best_le_curv, top_te_curv, top_nrevers, 6, top_bspline, result_ok)
    ! call penalty_stats_print (7)

    call penalty_stats_init ()                ! reset penalty stats before matching
    call match_bspline (airfoil%bot, best_le_curv, bot_te_curv, bot_nrevers, 6,bot_bspline, result_ok)
    ! call penalty_stats_print_table (7)

    call timing_result ("C2-coupled matching top and bot")

    ! write result to bspline file for visual comparison

    airfoil_matched = airfoil_from_bspline (top_bspline, bot_bspline, 161, "Match_JX-GS3-100_bspline")
    call airfoil_write_with_shapes (airfoil_matched, "")

    ! LE curvature must be equal for top and bot by construction
    le_curv_top  = bspline_le_curvature (top_bspline)
    le_curv_bot  = bspline_le_curvature (bot_bspline)
    le_curv_diff = abs (le_curv_top - le_curv_bot)

    ! In C2-coupled mode py(2) is derived to produce exactly best_le_curv 
    call assertf (le_curv_diff, 0.0d0, "C2-coupled LE curvature diff top/bot", 1)

  end subroutine


end module 


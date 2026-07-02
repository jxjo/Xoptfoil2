! MIT License

module test_bezier
  
  ! Bezier functions 

  use os_util
  use print_util
  use test_util
  use airfoil_base,       only : airfoil_type, airfoil_from_bezier
  use airfoil_base,       only : create_bezier_example_airfoil, create_bezier_MH30, create_bezier_JX_GS3_100
  use shape_bezier,       only : bezier_spec_type
  use shape_bezier,       only : bezier_eval, bezier_curvature, bezier_eval_y_on_x, bezier_eval_1d
  use shape_bezier,       only : map_dv_to_bezier, u_distribution_bezier, bezier_get_dv0
  use shape_bezier,       only : ncp_to_ndv
  use shape_bezier,       only : write_bezier_file


  implicit none

  private 

  public :: test_bezier_all

contains

  subroutine test_bezier_all ()
    call test_bezier_create_shape ()
    call test_bezier_eval ()
    call test_bezier_match ()
  end subroutine


  subroutine test_bezier_eval ()

    !! test of bezier implementation comparing to python bezier results 

    use math_util,          only : linspace

    double precision, allocatable :: x(:), y(:), u(:) 
    type(bezier_spec_type)        :: bezier, bot_bezier 
    type(airfoil_type)            :: foil
    integer :: i, j


    call test_header ("Bezier curves")

    foil = create_bezier_example_airfoil (161)
    x = foil%x
    y = foil%y
    bezier = foil%top%bezier
    bot_bezier = foil%bot%bezier

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

    u = u_distribution_bezier (bezier, 101)
    call asserti (size(u), 101, "Number of points")
    call assertf (sum(u), 56.90317d0, "Cosinus distribution", 5)
     
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
        do j = 1,100
          do i = 1, size(x) 
            y(i) = bezier_eval_y_on_x (bezier, x(i))
          end do 
        end do 
    call timing_result ("10000 evals y on x")

  end subroutine

 

  subroutine test_bezier_match ()

    !! Test Bezier C2-coupled matching: both sides 
    !! LE curvature of top and bot must be equal by construction.

    use commons,              only : show_details
    use airfoil_base,         only : airfoil_type, airfoil_from_bezier, airfoil_write_with_shapes
    use airfoil_preparation,  only : match_bezier, determine_auto_curvature, repanel_match_foil
    use airfoil_geometry,     only : max_curvature_at_te
    use eval_commons,         only : curv_side_constraints_type, curv_constraints_type
    use eval_constraints,     only : print_penalty_stats_table, penalty_stats_init, print_penalty_stats
    use shape_bezier,         only : bezier_le_curvature

    double precision, allocatable :: delta(:)
    type (airfoil_type)           :: airfoil, airfoil_matched
    type (bezier_spec_type)       :: top_bezier, bot_bezier
    type (curv_constraints_type)  :: curv_constraints
    double precision              :: le_curv_top, le_curv_bot, le_curv_diff, le_curv
    integer                       :: i
    logical                       :: result_ok

    call test_header ("Bezier match airfoil JX-GS3-100")

    show_details = .true.

    airfoil = create_bezier_JX_GS3_100 (81)
    airfoil = repanel_match_foil (airfoil)

    call determine_auto_curvature (airfoil, curv_constraints)

    ! C2-coupled mode: py(2) derived from le_curv by construction - no retry loop needed

    call print_action ("Matching ...")
    call timing_start ()

    call penalty_stats_init ()                ! reset penalty stats before matching

    le_curv = maxval (airfoil%top%curvature)  ! use max curvature at LE as target for matching

    call match_bezier (airfoil%top, le_curv, curv_constraints%top, 6, top_bezier, result_ok)

    call penalty_stats_init ()                ! reset penalty stats before matching

    call match_bezier (airfoil%bot, le_curv, curv_constraints%bot, 6, bot_bezier, result_ok)

    call timing_result ("C2-coupled matching top and bot")


    ! write result to bezier file for visual comparison
    airfoil%filename = "Target_JX-GS3-100_bezier.bez"
    call airfoil_write_with_shapes (airfoil, "")

    airfoil_matched = airfoil_from_bezier (top_bezier, bot_bezier, 161)
    airfoil_matched%filename = "Match_JX-GS3-100_bezier.bez" 
    call airfoil_write_with_shapes (airfoil_matched, "")

    ! LE curvature must be equal for top and bot by construction
    le_curv_top  = bezier_le_curvature (top_bezier)
    le_curv_bot  = bezier_le_curvature (bot_bezier)
    le_curv_diff = abs (le_curv_top - le_curv_bot)

    ! In C2-coupled mode py(2) is derived to produce exactly best_le_curv, 
    call assertf (le_curv_diff, 0.0d0, "C2-coupled LE curvature diff top/bot", 1)

    ! check delta between original and result bezier
    delta = top_bezier%px
    do i = 1, size(top_bezier%px)
      delta(i) = abs(top_bezier%px(i) - airfoil%top%bezier%px(i))
    end do 
    call assertf (sum(delta), 0.01d0, "Delta px of Bezier control points", 2)

  end subroutine


  
  subroutine test_bezier_create_shape ()

    !! test of bezier create shape for optimization 

    use airfoil_base,         only : te_gap
    use shape_bezier,         only : bezier_eval_side, bezier_le_curvature

    double precision, allocatable :: dv_top(:), dv_bot(:)

    type(airfoil_type)            :: seed_foil, foil
    type(bezier_spec_type)        :: top_bezier, bot_bezier 
    integer                       :: ndv_top, ndv_bot, npoint
    double precision              :: diff, side_te_gap, le_curv, diff_seed

    call test_header ("Bezier create shape based on MH30-norm-bezier")

    ! create seed airfoil 

    seed_foil = create_bezier_MH30 (161)

    ndv_top = ncp_to_ndv(size(seed_foil%top%bezier%px))
    ndv_bot = ncp_to_ndv(size(seed_foil%bot%bezier%px))

    ! make design vars 

    dv_top  = bezier_get_dv0 (.false., seed_foil%top%bezier, c2_coupled=.true.)
    dv_bot  = bezier_get_dv0 (.true.,  seed_foil%bot%bezier, c2_coupled=.true.)

    ! build new bezier from design vars 

    side_te_gap = te_gap (seed_foil) / 2

    le_curv = bezier_le_curvature (seed_foil%top%bezier)  
    top_bezier = map_dv_to_bezier (.false., dv_top, side_te_gap, le_curv)

    le_curv = bezier_le_curvature (seed_foil%bot%bezier)  
    bot_bezier = map_dv_to_bezier (.true., dv_bot, side_te_gap, le_curv)

    call assertf (bot_bezier%px(5), seed_foil%bot%bezier%px(5), "Created bezier equal seed x", 8)

    ! build new airfoil from the recalculated bezier curves

    npoint = size(seed_foil%x)
    foil = airfoil_from_bezier (top_bezier, bot_bezier, npoint)

    call assertf (sum(seed_foil%x), sum(foil%x), "Created airfoil equal seed x", 2)
    call assertf (sum(seed_foil%y), sum(foil%y), "Created airfoil equal seed y", 2)

    diff_seed = abs (bezier_curvature(seed_foil%top%bezier, 0d0)- bezier_curvature(seed_foil%bot%bezier, 0d0))
    diff = abs (bezier_curvature(top_bezier, 0d0)- bezier_curvature(bot_bezier, 0d0))
    call assertf (diff, diff_seed, "Curvature diff at le",2)

  end subroutine

end module 


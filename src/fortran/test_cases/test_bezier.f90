! MIT License
! Copyright (c) 2024 Jochen Guenzel

module test_bezier
  
  !-------------------------------------------------------------------------
  ! Bezier functions 
  !-------------------------------------------------------------------------

  use os_util
  use print_util
  use test_util
  use airfoil_base,       only : airfoil_type
  use shape_bezier,       only : bezier_spec_type
  use shape_bezier,       only : create_bezier_example_airfoil, create_bezier_MH30
  use shape_bezier,       only : bezier_eval, bezier_curvature, bezier_eval_y_on_x, bezier_eval_1d
  use shape_bezier,       only : map_dv_to_bezier, u_distribution_bezier, bezier_get_dv0
  use shape_bezier,       only : ncp_to_ndv_side
  use shape_bezier,       only : bezier_create_airfoil, write_bezier_file


  implicit none

  contains

  subroutine test_bezier_eval ()

    !! test of bezier implementation comparing to python bezier results 

    use math_util,          only : linspace

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
    use airfoil_base,         only : split_foil_into_sides, airfoil_write
    use airfoil_preparation,  only : match_bezier, match_get_best_le_curvature

    character (:), allocatable    :: name 
    double precision, allocatable :: delta(:)
    type (airfoil_type)           :: airfoil
    type(bezier_spec_type)        :: top_bezier, bot_bezier 
    integer                       :: i
    double precision              :: best_le_curv, weighting
    logical                       :: result_ok


    call test_header ("Bezier match airfoil MH30-norm-bezier")

    call create_bezier_MH30 (201, name, airfoil%x, airfoil%y, &
                                  airfoil%top_bezier, airfoil%bot_bezier)

    airfoil%name = name
    airfoil%symmetrical = .false. 

    call split_foil_into_sides (airfoil) 

    ! write seed to dat file 

    call print_action ("Writing Match_seed.dat and .bez")
    call airfoil_write     ("Match_seed.dat", airfoil)
    call write_bezier_file ("Match_seed.bez", "Match_seed", airfoil%top_bezier, airfoil%bot_bezier)

    ! simplex optimization 

    show_details = .true.
    call timing_start ()

    best_le_curv = match_get_best_le_curvature (airfoil)
    weighting = 1.0d0

    call match_bezier  (airfoil%top, best_le_curv, weighting, 7, top_bezier, result_ok)
    call match_bezier  (airfoil%bot, best_le_curv, weighting, 5, bot_bezier, result_ok)

    call timing_result ("Matching top and bot")

    ! write result to bezier file 

    call print_action  ("Writing Match_bezier.bez")
    call write_bezier_file ("Match_bezier.bez", "Match_bezier", top_bezier, bot_bezier)

    ! check delta between original and result bezier

    delta = top_bezier%px
    do i = 1, size(top_bezier%px)
      delta(i) = abs(top_bezier%px(i) - airfoil%top_bezier%px(i))
    end do 
    call assertf (sum(delta), 0.15d0, "Delta px of Bezier control points", 2)

    do i = 1, size(top_bezier%py)
      delta(i) = abs(top_bezier%py(i) - airfoil%top_bezier%py(i))
    end do 
    call assertf (sum(delta), 0.05d0, "Delta py of Bezier control points", 2)

    call assertf (bezier_curvature(top_bezier, 0d0), 407d0, "Curvature at LE",0)

  end subroutine


  
  subroutine test_bezier_create_shape ()

    !! test of bezier create shape for optimization 

    use airfoil_base,         only : split_foil_into_sides
    use airfoil_geometry,     only : te_gap

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

    call bezier_create_airfoil (top_bezier, bot_bezier, size(seed_foil%x), foil%x, foil%y) 

    call assertf (sum(seed_foil%x), sum(foil%x), "Created airfoil equal seed x", 2)
    call assertf (sum(seed_foil%y), sum(foil%y), "Created airfoil equal seed y", 2)

    diff_seed = abs (bezier_curvature(seed_foil%top_bezier, 0d0)- bezier_curvature(seed_foil%bot_bezier, 0d0))
    diff = abs (bezier_curvature(top_bezier, 0d0)- bezier_curvature(bot_bezier, 0d0))
    call assertf (diff, diff_seed, "Curvature diff at le",2)

  end subroutine

end module 


! MIT License
! Copyright (c) 2025 Jochen Guenzel

module test_spline
  
  !-------------------------------------------------------------------------
  ! spline_1D and 2d tests
  !-------------------------------------------------------------------------

  use os_util
  use test_util

  implicit none

  contains

  subroutine test_spline_1d  ()

    !! tests for 1D spline  

    use math_util,    only : linspace
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

    use math_util,    only : linspace
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
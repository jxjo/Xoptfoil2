! MIT License
! Copyright (c) 2024 Jochen Guenzel

  

module test_simplex
 
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

  subroutine test_simplex_search () 

    use simplex_search, only : simplexsearch, simplex_options_type 

    double precision      :: xmin(2), x0(2)
    double precision      :: fmin, f0_ref
    integer               :: steps, fevals, f
    type(simplex_options_type) :: sx_options

    f = 0 
    call test_header ("Simplex optimization")

    sx_options%min_radius     = 1d-7
    sx_options%max_iterations = 100
    x0 = [0.2d0,0.8d0]

    call simplexsearch(xmin, fmin, steps, fevals, my_objective_function, &
                       x0, .false. , f0_ref, sx_options)

    call assertf (xmin(1), 0d0, "Found min at x=0.0", 6)
    call asserti (steps, 88, "88 steps needed" )

  end subroutine 

end module

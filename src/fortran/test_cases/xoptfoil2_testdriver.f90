! MIT License
! Copyright (c) 2025 Jochen Guenzel


program testdriver

  !-------------------------------------------------------------------------
  ! testdriver to run all automated tests 
  !-------------------------------------------------------------------------
 
  use print_util
  use test_util
  use test_bezier
  use test_airfoil_evals
  use test_spline
  use test_simplex
  use test_airfoil_basics

  implicit none 

  call set_show_details (.true.) 

  call test_spline_1d ()
  call test_spline_2d () 

  call test_bezier_eval ()
  call test_bezier_create_shape ()

  call test_airfoil_split () 
  call test_airfoil_normalize ()
  call test_airfoil_geometry ()

  call test_eval_constraints ()

  call test_simplex_search ()
  call test_bezier_match ()

  call test_footer ("") 

  if (nfails > 0) then 
    stop 1
  else
    stop 0 
  end if 

end program  


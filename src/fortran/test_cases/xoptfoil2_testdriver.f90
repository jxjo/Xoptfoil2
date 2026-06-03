! MIT License
! Copyright (c) 2025 Jochen Guenzel


program testdriver

  !-------------------------------------------------------------------------
  ! testdriver to run all automated tests 
  !-------------------------------------------------------------------------
 
  use print_util
  use test_util
  use test_bezier
  use test_bspline
  use test_airfoil_evals
  use test_geo_target
  use test_op_point
  use test_spline
  use test_simplex
  use test_airfoil_basics

  implicit none 

  call set_show_details (.true.) 

  call test_spline_1d ()
  call test_spline_2d () 

  call test_airfoil_split () 
  call test_airfoil_normalize ()
  call test_airfoil_geometry ()
  call test_airfoil_load_dat ()

  call test_geo_constraints ()
  call test_penalty_curv_deriv ()

  call test_simplex_search ()

  call test_bezier_all ()

  ! call test_bspline_all ()

  call test_geo_target_all ()

  call test_op_point_all ()
  
  call test_footer ("") 

  if (nfails > 0) then 
    stop 1
  else
    stop 0 
  end if 

end program  

